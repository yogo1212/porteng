unit EngineWorld;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineShader, Math, EngineTypes, EngineFileUtils,
	Convenience, EngineMath, EngineMemory, EngineDebug;

//{$UNDEF ENGINEDEBUG}
{$UNDEF USEDEPTHACCU}
type

	TVoxelInfo = record
		position: TVec3;
		colour: TCol4b;
		size: GLfloat;
	end;

	PVoxelInfo = ^TVoxelInfo;

	{ TOcEntry }

	TOcType = (OcEmpty = 0, OcFull, OcRec, OcData);

	TOcEntryPlain = record
		case boolean of
			False:		// 3-bit-index, 1st bit is x, 2nd y and 3rd z
			// two bits are addressed
			(map,
				// actual childs are this far away [TOcEntryPlain]
				offset: word);
			// if this OcData
			True: (id: cardinal);
	end;

	POcEntryPlain = ^TOcEntryPlain;

	TOcEntry = object
		o: POcEntryPlain;
		function Get(index: byte): TOcType;
		function Recurse(index: byte): TOcEntry;
		function SubColour: TCol4b;
		function TranslateColour(index: byte): TCol4b;
		procedure GetVoxelData(relrenderPos: TVec3; var destpnt: PVoxelInfo;
			pos: TVec3; size: GLfloat; LoDper1: word; curCol: TCol4b);
	end;

	{ TWorldChunkPlain }

	TWorldChunkPlain = object
	private
		octree: TOcEntry;
		octreecount: cardinal;

		usecnt: byte;

		function BuildFileName: string;
	public
		position: TVec3i;

		constructor Create(const nposition: TVec3i);
		destructor Destroy;

		function Use: TOcEntry;
		procedure UnUnse;

		function RelToAbsPos(input: TVec3): TVec3;
		function AbsToRelPos(input: TVec3): TVec3s;

		function CountVoxels: cardinal;
	end;

	{ TWorldChunk }

	TWorldChunk = ^TWorldChunkPlain;

	{ TFillThread }

	TFillThread = class(TThread)
		Data: PVoxelInfo;
		curvoxel, voxelcount: cardinal;
		chunk: TWorldChunk;
		finished: PBoolean;
		AbsViewPos: TVec3;
		LoDper1: word;
		constructor Create(nchunk: TWorldChunk; finishedflag: PBoolean;
			nAbsViewPos: TVec3; nLoDper1: word);
		destructor Destroy; override;
		procedure Execute; override;
		procedure CollectResult(vertexbuffer: GLuint; pvoxelcount: PCardinal);
	end;

	{ TLoadedChunk }

	TUpdateRenderDataProc = procedure(const nAbsViewPos: TVec3;
		const nLoDper1: word) of object;

	TLoadedChunk = object
	private
		fillthread: TFillThread;

		procedure FillRenderData;
		procedure DontUpdateRenderData(const nAbsViewPos: TVec3; const nLoDper1: word);
		procedure DoDraw;
	public
		chunk: TWorldChunk;
		AbsRenderPos: TVec3i;
		AbsViewPos: TVec3;
		LoDper1: word;

		vertexarray, vertexbuffer: GLuint;
		voxelcount: word;

		fillthreadfinished: boolean;
		UpdateRenderData: TUpdateRenderDataProc;
		draw: TObjProc;

		constructor Create(const nAbsRenderPos: TVec3i);
		destructor Destroy;

		procedure DoUpdateRenderData(const nAbsViewPos: TVec3; const nLoDper1: word);
	end;

	PLoadedChunk = ^TLoadedChunk;

procedure GameWorldInit;
function CheckWorldCollision(var worldPos: TRelWorldPosition;
	var pos: TGamePosition; movement: TVec3): boolean;

{$IFDEF ENGINEDEBUG}
function CastRayEmpty(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean;
function CastRayFull(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean;
function CastRayRec(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean;
{$ENDIF}

implementation

type
	TLCAlterProc = procedure(chunk: PLoadedChunk);

	{ TChunkComparer }

	TChunkComparer = class(_TPointerComparer)
		function compare(o1, o2: Pointer): shortint; override;
		function greater(o1, o2: Pointer): boolean; override;
		function equal(o1, o2: Pointer): boolean; override;
		function less(o1, o2: Pointer): boolean; override;
	end;

var
	mapDataPath: string;
	initialised: boolean = False;
	activechunks: TSortedMemoryManager;
	chunkcomparer: TChunkComparer;
	cubebuffer: GLuint;

procedure GameWorldDestroy;
begin
	if initialised then
	begin
		initialised := False;

		FreeAndNil(activechunks);
		FreeAndNil(chunkcomparer);
	end;
end;

var
	fobj: array[0..13] of TVec3;

procedure GameWorldInit;
const
	size: GLfloat = 1;
begin
	if not initialised then
	begin
		initialised := True;
		GameMapShaderInit;

		fobj[0] := Vec3(-Size, Size, Size);
		fobj[1] := Vec3(-Size, -Size, Size);
		fobj[2] := Vec3(Size, Size, Size);
		fobj[3] := Vec3(Size, -Size, Size);
		fobj[4] := Vec3(Size, -Size, -Size);
		fobj[5] := Vec3(-Size, -Size, Size);
		fobj[6] := Vec3(-Size, -Size, -Size);
		fobj[7] := Vec3(-Size, Size, Size);
		fobj[8] := Vec3(-Size, Size, -Size);
		fobj[9] := Vec3(Size, Size, Size);
		fobj[10] := Vec3(Size, Size, -Size);
		fobj[11] := Vec3(Size, -Size, -Size);
		fobj[12] := Vec3(-Size, Size, -Size);
		fobj[13] := Vec3(-Size, -Size, -Size);

		glGenBuffers(1, @cubebuffer);
		glBindBuffer(GL_ARRAY_BUFFER, cubebuffer);
		glBufferData(GL_ARRAY_BUFFER, SizeOf(TVec3) * Length(fobj), @fobj[0],
			GL_STATIC_DRAW);

		chunkcomparer := TChunkComparer.Create;
		activechunks := TSortedMemoryManager.Create(chunkcomparer,
			TFragmentedMemoryManager.Create(SizeOf(TWorldChunkPlain), 10));
	end;
end;

{$DEFINE DEBUGNORMALISER}
procedure normaliseWorldPos(var worldPos: glint; var pos: GLfloat);
begin
	// TODO rename parameters
	if pos > 256 then
	begin
		pos -= 512;
		Inc(worldPos);
	end
	else
{$IFDEF DEBUGNORMALISER}
	if pos < 256 then
{$ENDIF}
	begin
		pos += 512;
		Dec(worldPos);
	end
{$IFDEF DEBUGNORMALISER}
	else
		raise Exception.Create('normaliseWorldPos ran into else!!');
{$ENDIF}
	;
end;

{$IFNDEF ENGINEDEBUG}
function CastRayEmpty(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean; forward;
function CastRayFull(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean; forward;
function CastRayRec(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean; forward;
{$ENDIF}

type													 //OcEmpty = 0, OcFull, OcRec, OcData
	TRayCastFunc = function(const ocentry: TOcEntry; var pos: TVec3;
		var direction: TVec3): boolean;

const
	CastRayOctree: array[TOcType] of TRayCastFunc =
		(@CastRayEmpty, @CastRayFull, @CastRayRec, @CastRayEmpty);

procedure DontProgressVectorComponent(const pos, dir: GLfloat; var Result: GLfloat);
begin

end;

procedure DoprogressVectorComponent(const pos, dir: GLfloat; var Result: GLfloat);
var
	offset: TIntFloat;
begin
	//This gives a float with same sign of dir
	offset.floatval := 1;
	offset.intval := offset.intval or (Pglfloatsizedint(@dir)^ and glfloatsignbit);
	// and adds it to pos
	offset.floatval := pos - offset.floatval;
	offset.intval := (offset.intval and not glfloatsignbit)
    or (Pglfloatsizedint(@dir)^ and glfloatsignbit);
  // TODO there was a sigfpe
	Result := Min(offset.floatval / dir, Result);
end;

type
	TProgressVectorProc = procedure(const pos, dir: GLfloat; var Result: GLfloat);

const
	progressVectorComponent: array[boolean] of TProgressVectorProc =
		(@DontProgressVectorComponent, @DoprogressVectorComponent);

function CastRayEmpty(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean;
var
	m: GLfloat;
begin
	// TODO improve this:

	{
  the ray being s + m * d = r; where s, d and r are vectors
  the plane being n * (x - t) = 0 - all of them vectors (except for zero).
  in our case all vectors are translated so that the current voxel is a cube with a
  length of 2 located at the origin.
  that way t and n are identical and unit.

  // substituting r for x yields: -n*(n - t) / n*d = m

  but: n will only be a vector with two components being 0 and one being either 1 or -1
  => n*n is always one.
  Otherwise, the dot-product is equivalent to a projection and a potential sign-toggle.
  }

	m := 1;

	//This gives a float with the same sign as dir.X
	progressVectorComponent[direction.X <> 0](pos.X, direction.X, m);
	progressVectorComponent[direction.Y <> 0](pos.Y, direction.Y, m);
	progressVectorComponent[direction.Z <> 0](pos.Z, direction.Z, m);

	// two possible branches so far (min)

	// if the pos is not inside the unit-sized cube at origin, then m could be zero(or less)
	// and there would have to be checks here.
	pos += direction * m;
	direction *= (1 - m);
	//Result := (m < 1);
	Result := False;
	// no further checks necessary
end;

function CastRayFull(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean;
begin
	// TODO is it wise to remove checks here?
	// if for some reason you are inside the geometry, you are stuck there ... forever! :-D
	// it is common practice to only test for collision on surfaces
	Result := True;
end;

procedure DontCorrectSigns(pos: Pglfloat; const direction: glfloat);
begin

end;

procedure DoCorrectSigns(pos: Pglfloat; const direction: glfloat);
begin
	// let the sign of pos be the same of that of direction
	// pos should be zero and changing the sign should only be relevant for our tweaks
	// if in doubt, this can be solved with branching, because the case of one component of
	// a vector being zero should not occur to often
	Pglfloatsizedint(pos)^ := Pglfloatsizedint(pos)^ and not glfloatsignbit;
	Pglfloatsizedint(pos)^ := Pglfloatsizedint(pos)^ xor
		(Pglfloatsizedint(@direction)^ and glfloatsignbit);
end;

type
	TSignProc = procedure(pos: Pglfloat; const direction: glfloat);

const
	fixSignProblems: array[boolean] of TSignProc = (@DontCorrectSigns, @DoCorrectSigns);

function CastRayRec(const ocentry: TOcEntry; var pos: TVec3;
	var direction: TVec3): boolean;

	procedure prepareSigns(var pos: TVec3; const direction: TVec3);
	begin
		// the index of the next node in the octree
		// if one component of pos is zero, prepare the sign so that collision is not tested
		// against a voxel that direction is facing away from
		fixSignProblems[pos.X = 0](@pos.X, direction.X);
		fixSignProblems[pos.Y = 0](@pos.Y, direction.Y);
		fixSignProblems[pos.Z = 0](@pos.Z, direction.Z);
	end;

var
	index: byte;
begin
	prepareSigns(pos, direction);

	index := (((Pglfloatsizedint(@pos.X)^ and glfloatsignbit) shr glfloatsignbitindex) or
		((Pglfloatsizedint(@pos.Y)^ and glfloatsignbit) shr (glfloatsignbitindex - 1)) or
		((Pglfloatsizedint(@pos.Z)^ and glfloatsignbit) shr (glfloatsignbitindex - 2)))
    xor $7;
  {writeln(
    chr(((Pglfloatsizedint(@pos.X)^ and glfloatsignbit) shr glfloatsignbitindex)
      + Ord('0')),
	  chr(((Pglfloatsizedint(@pos.Y)^ and glfloatsignbit) shr (glfloatsignbitindex - 1))
      + Ord('0')),
		chr(((Pglfloatsizedint(@pos.Z)^ and glfloatsignbit) shr (glfloatsignbitindex - 2))
      + Ord('0')));}

	direction *= 2;
	pos := (pos - Vec3((index and 1) - 0.5, ((index shr 1) and 1) - 0.5,
		((index shr 2) and 1) - 0.5)) * 2;

	Result := CastRayOctree[ocentry.Get(index)](ocentry.Recurse(index), pos, direction);

	pos := (pos / 2) + Vec3((index and 1) - 0.5, ((index shr 1) and 1) -
		0.5, ((index shr 2) and 1) - 0.5);
	direction /= 2;

	if ((pos.X = 0) and (direction.X <> 0)) or ((pos.Y = 0) and (direction.Y <> 0))
    or ((pos.Z = 0) and (direction.Z <> 0)) and (not Result) then
	begin
		prepareSigns(pos, direction);

		index := (((Pglfloatsizedint(@pos.X)^ and glfloatsignbit) shr glfloatsignbitindex) or
			((Pglfloatsizedint(@pos.Y)^ and glfloatsignbit) shr (glfloatsignbitindex - 1)) or
			((Pglfloatsizedint(@pos.Z)^ and glfloatsignbit) shr (glfloatsignbitindex - 2))) xor $7;

		direction *= 2;
		pos := (pos - Vec3((index and 1) - 0.5, ((index shr 1) and 1) - 0.5,
			((index shr 2) and 1) - 0.5)) * 2;

		Result := CastRayOctree[ocentry.Get(index)](ocentry.Recurse(index), pos, direction);

		pos := (pos / 2) + Vec3((index and 1) - 0.5, ((index shr 1) and 1) -
			0.5, ((index shr 2) and 1) - 0.5);
		direction /= 2;
	end;
end;

procedure RelToMiau(var pos, dir: TVec3; size: GLfloat); inline;
begin
	pos := pos / size;
	dir := dir / size;
end;

procedure MiauToRel(var pos, dir: TVec3; size: GLfloat); inline;
begin
	pos := pos * size;
	dir := dir * size;
end;

type
  TCorrectComponentProc = procedure (var worldPos: GLint; var pos: GLfloat;
  const dir: GLfloat; var flag: Boolean); register;

procedure DontTryCorrectComponent(var worldPos: GLint; var pos: GLfloat;
  const dir: GLfloat; var flag: Boolean);
begin

end;

procedure DoTryCorrectComponent(var worldPos: GLint; var pos: GLfloat; const dir: GLfloat;
  var flag: Boolean);
begin
  if (pos > 256) or
    ((pos = 256) and ((Pglfloatsizedint(@dir)^ and glfloatsignbit) = 0)) then
  begin
    pos -= 512;
    worldPos += 1;
    flag := True;
  end
  else if (pos < -256) or
    ((pos = -256) and ((Pglfloatsizedint(@dir)^ and glfloatsignbit) <> 0)) then
  begin
    pos += 512;
    worldPos -= 1;
    flag := True;
  end;
end;

const TryCorrectComponent: array[Boolean] of TCorrectComponentProc =
  (@DontTryCorrectComponent, @DoTryCorrectComponent);

function CheckWorldCollision(var worldPos: TRelWorldPosition;
	var pos: TGamePosition; movement: TVec3): boolean;
var
	tmpchunk: TWorldChunkPlain; flag: Boolean;
begin
  repeat
    flag := False;
	  tmpchunk.position := worldPos;

	  RelToMiau(pos, movement, 256);

 	  // TODO inline if  or not? hopefully it will be predicted.
	  if activechunks.Fetch(@tmpchunk) then
		  Result := CastRayOctree[OcRec](tmpchunk.octree, pos, movement)
    else
      Result := CastRayOctree[OcEmpty](tmpchunk.octree, pos, movement);

	  MiauToRel(pos, movement, 256);

	  if Abs(pos.X) > 256 then
    begin
		  normaliseWorldPos(worldPos.X, pos.X);
      flag := True;
    end;
	  if Abs(pos.Y) > 256 then
    begin
		  normaliseWorldPos(worldPos.Y, pos.Y);
      flag := True;
    end;
	  if Abs(pos.Z) > 256 then
    begin
		  normaliseWorldPos(worldPos.Z, pos.Z);
      flag := True;
    end;
  until not flag;
end;

function finaliseColour(const input: TCol4b): TCol4b;
begin
	// elements of input-colour are 0;64;128 or 192
	PCol3b(@Result)^ := (PCol3b(@input)^ div $30) * 85;
	// r g and b are now 0;85;170;255
{$IFDEF USEDEPTHACCU}
	Result.w := 63 + input.w;
	// a is now 63;127;191;255
{$ELSE}
	// Sadly, before order-independent-transparency-rendering is feasible:
	Result.w := 255;
{$ENDIF}
end;

{$ifdef ENGINEDEBUG}
procedure printVoxelArray(pnt: PVoxelInfo; cnt: cardinal);
begin
	while cnt > 0 do
	begin
		Dec(cnt);
		logTrace(VecToStr(pnt^.position) + ': ' + FloatToStr(pnt^.size) +
			' ' + BinToHexStr(@pnt^.colour, SizeOf(TVoxelInfo.colour)));
		Inc(pnt);
	end;
end;
{$endif}

function RequestChunk(const position: TVec3i): TWorldChunk;
var
	tmpchunk: TWorldChunkPlain;
begin
	Result := nil;
	tmpchunk.position := position;
	if activechunks.Fetch(@tmpchunk) then
	begin
		Result := @tmpchunk;
	end
	else
	begin
		tmpchunk.Create(position);
		if tmpchunk.octree.o <> nil then
			Result := activechunks.Store(@tmpchunk);
	end;
end;

function TChunkComparer.compare(o1, o2: Pointer): shortint;
begin
	Result := 0;
	if TWorldChunk(o1)^.position.X < TWorldChunk(o2)^.position.X then
		Result := -1
	else if TWorldChunk(o1)^.position.X > TWorldChunk(o2)^.position.X then
		Result := 1
	else
	if TWorldChunk(o1)^.position.Y < TWorldChunk(o2)^.position.Y then
		Result := -1
	else if TWorldChunk(o1)^.position.Y > TWorldChunk(o2)^.position.Y then
		Result := 1
	else
	if TWorldChunk(o1)^.position.Z < TWorldChunk(o2)^.position.Z then
		Result := -1
	else if TWorldChunk(o1)^.position.Z > TWorldChunk(o2)^.position.Z then
		Result := 1;
end;

function TChunkComparer.greater(o1, o2: Pointer): boolean;
begin
	Result := True;
	if not (TWorldChunk(o1)^.position.X > TWorldChunk(o2)^.position.X) then
	begin
		if TWorldChunk(o1)^.position.X = TWorldChunk(o2)^.position.X then
		begin
			if not (TWorldChunk(o1)^.position.Y > TWorldChunk(o2)^.position.Y) then
			begin
				if TWorldChunk(o1)^.position.Y = TWorldChunk(o2)^.position.Y then
					Result := (TWorldChunk(o1)^.position.Z > TWorldChunk(o2)^.position.Z)
				else
					Result := False;
			end;
		end
		else
			Result := False;
	end;
end;

function TChunkComparer.equal(o1, o2: Pointer): boolean;
begin
	// component-wise comparison
	Result := (TWorldChunk(o1)^.position.X = TWorldChunk(o2)^.position.X) and
		(TWorldChunk(o1)^.position.Y = TWorldChunk(o2)^.position.Y) and
		(TWorldChunk(o1)^.position.Z = TWorldChunk(o2)^.position.Z);
end;

function TChunkComparer.less(o1, o2: Pointer): boolean;
begin
	Result := True;
	if not (TWorldChunk(o1)^.position.X < TWorldChunk(o2)^.position.X) then
	begin
		if TWorldChunk(o1)^.position.X = TWorldChunk(o2)^.position.X then
		begin
			if not (TWorldChunk(o1)^.position.Y < TWorldChunk(o2)^.position.Y) then
			begin
				if TWorldChunk(o1)^.position.Y = TWorldChunk(o2)^.position.Y then
					Result := (TWorldChunk(o1)^.position.Z < TWorldChunk(o2)^.position.Z)
				else
					Result := False;
			end;
		end
		else
			Result := False;
	end;
end;

{ TOcEntry }

function TOcEntry.Get(index: byte): TOcType;
begin
	Result := TOcType((o^.map shr ((index and $7) shl 1)) and $3);
end;

function TOcEntry.Recurse(index: byte): TOcEntry;
var
	buffer: word;
begin
	Result.o := (o + 1 + o^.offset);
	buffer := o^.map;
	while index > 0 do
	begin
		Dec(index);
		if (buffer and $2) <> 0 then
			Inc(Result.o);
		buffer := buffer shr 2;
	end;
end;

function TOcEntry.TranslateColour(index: byte): TCol4b;
begin
	Result := Vec4ub(Ord(Get((0 + index) and $7)) + Ord(Get((4 + index) and $7)),
		Ord(Get((1 + index) and $7)) + Ord(Get((5 + index) and $7)),
		Ord(Get((2 + index) and $7)) + Ord(Get((6 + index) and $7)),
		Ord(Get((3 + index) and $7)) + Ord(Get((7 + index) and $7))) * $30;
end;

function TOcEntry.SubColour: TCol4b;
var
	cnt: byte;
begin
	Result := Vec4ub(0, 0, 0, 0);
	for cnt := 0 to 7 do
	begin
		if Get(cnt) = OcRec then
			Result := Result + Recurse(cnt).SubColour
		else if Get(cnt) = OcFull then
			Result := Result + TranslateColour(cnt);
	end;
end;

procedure TOcEntry.GetVoxelData(relrenderPos: TVec3; var destpnt: PVoxelInfo;
	pos: TVec3; size: GLfloat; LoDper1: word; curCol: TCol4b);

	function GetSubOffsVec(index: byte): TVec3;
	begin
		Result.X := (index and $1);
		Result.Y := ((index shr $1) and $1);
		Result.Z := (index shr $2);
	end;

var
	cnt: byte;
	subpos: TVec3;
	subsize: GLfloat;
	subcol: TCol4b;
begin
	subsize := size / 2;
	if LengthSquare(relrenderPos - pos) - sqr(size) < sqr(LoDper1 * size) then
	begin
		for cnt := 0 to 7 do
		begin
			subpos := pos - Vec3(subsize, subsize, subsize) + GetSubOffsVec(cnt) * size;
			subcol := curCol + TranslateColour(cnt);
			if Get(cnt) = OcRec then
				Recurse(cnt).GetVoxelData(relrenderPos, destpnt, subpos, subsize, LoDper1,
					SubCol)
			else if Get(cnt) = OcFull then
			begin
				destpnt^.colour := finaliseColour(subcol);
				destpnt^.position := subpos;
				destpnt^.size := subsize;
				Inc(destpnt);
			end;
		end;
	end
	else
	begin
		destpnt^.colour := finaliseColour(curcol + SubColour);
		destpnt^.position := pos;
		destpnt^.size := size;
		Inc(destpnt);
	end;
end;

{ TFillThread }

constructor TFillThread.Create(nchunk: TWorldChunk; finishedflag: PBoolean;
	nAbsViewPos: TVec3; nLoDper1: word);
begin
	inherited Create(True);
	chunk := nchunk;
	curvoxel := 0;
	LoDper1 := nLoDper1;
	voxelcount := nchunk^.CountVoxels;
	finished := finishedflag;
	AbsViewPos := nAbsViewPos;
	Getmem(Data, voxelcount * SizeOf(TVoxelInfo));
end;

destructor TFillThread.Destroy;
begin
	Getmem(Data, voxelcount * SizeOf(TVoxelInfo));
	inherited Destroy;
end;

procedure TFillThread.Execute;
var
	tmppnt: PVoxelInfo;
begin
	tmppnt := Data;
	chunk^.octree.GetVoxelData(Vec3(chunk^.AbsToRelPos(AbsViewPos)), tmppnt, Vec3(0, 0, 0),
		256, LoDper1, Vec4ub(0, 0, 0, 0));

	curvoxel := (tmppnt - Data);
{$ifdef ENGINEDEBUG}
	printVoxelArray(Data, curvoxel);
	logTrace('Built new chunk with ' + IntToStr(curvoxel) + ' voxels');
	logTrace('Chunk Data ' + BinToHexStr(Data, curvoxel * SizeOf(TVoxelInfo)));
{$endif}
	if curvoxel > voxelcount then
		raise Exception.Create('weird stuff goin on here' + PtrToHex(Data) +
			' ' + IntToStr(voxelcount) + ' ' + PtrToHex(tmppnt) + ' ' +
			IntToStr(SizeOf(TVoxelInfo)));
	if not terminated then
		finished^ := True;
	Terminate;
end;

procedure TFillThread.CollectResult(vertexbuffer: GLuint; pvoxelcount: PCardinal);
begin
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, curvoxel * SizeOf(TVoxelInfo), Data, GL_STATIC_DRAW);
	pvoxelcount^ := curvoxel;
end;

{ TLoadedChunk }

constructor TLoadedChunk.Create(const nAbsRenderPos: TVec3i);
begin
	chunk := RequestChunk(nAbsRenderPos);
	if chunk <> nil then
	begin
		fillthread := nil;
		fillthreadfinished := False;

		glGenVertexArrays(1, @vertexarray);
		glGenBuffers(1, @vertexbuffer);

		voxelcount := 0;

		AbsRenderPos := nAbsRenderPos;
		draw := @DoDraw;
		UpdateRenderData := @DoUpdateRenderData;
		chunk^.Use;
	end
	else
	begin
		PPointer(@draw)^ := Pointer(@DoNothing);
		UpdateRenderData := @DontUpdateRenderData;
	end;
end;

destructor TLoadedChunk.Destroy;
begin
	if (chunk <> nil) then
	begin
		chunk^.UnUnse;
		glDeleteBuffers(1, @vertexbuffer);
		glDeleteVertexArrays(1, @vertexarray);
	end;
end;

procedure TLoadedChunk.DoUpdateRenderData(const nAbsViewPos: TVec3;
	const nLoDper1: word);
begin
	AbsViewPos := nAbsViewPos;
	LoDper1 := nLoDper1;
	FillRenderData;
end;

procedure TLoadedChunk.FillRenderData;
begin
	if fillthread <> nil then
	begin
		fillthread.Terminate;
		FreeAndNil(fillThread);
	end;
	fillthreadfinished := False;
	fillthread := TFillThread.Create(chunk, @fillthreadfinished, AbsViewPos, LoDper1);
	fillthread.Start;
end;

procedure TLoadedChunk.DontUpdateRenderData(const nAbsViewPos: TVec3;
	const nLoDper1: word);
begin

end;

procedure DoReload(chunk: PLoadedChunk);
begin
	chunk^.fillthread.CollectResult(chunk^.vertexbuffer, @chunk^.voxelcount);
	chunk^.fillthreadfinished := False;
	FreeAndNil(chunk^.fillthread);

	// this needs to be done here (and not only once in the constructor) because
	// otherwise it won't render anything :-(
	glBindVertexArray(chunk^.vertexarray);
	glBindBuffer(GL_ARRAY_BUFFER, cubebuffer);

	glEnableVertexAttribArray(0);
	glVertexAttribDivisor(0, 0);
	// NOOOOOO HOLY FUKING SHIET!! HAD SIZEOF MULTIPLIED BY #!! EMBARRASSING
	glVertexAttribPointer(0, 3, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVec3), nil);

	glBindBuffer(GL_ARRAY_BUFFER, chunk^.vertexbuffer);

	glEnableVertexAttribArray(1);
	glVertexAttribDivisor(1, 1);
	glVertexAttribPointer(1, 3, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVoxelInfo),
		@PVoxelInfo(nil)^.position);

	glEnableVertexAttribArray(2);
	glVertexAttribDivisor(2, 1);
	glVertexAttribPointer(2, 4, GL_UNSIGNED_BYTE, bytebool(GL_TRUE),
		SizeOf(TVoxelInfo), @PVoxelInfo(nil)^.colour);

	glEnableVertexAttribArray(3);
	glVertexAttribDivisor(3, 1);
	glVertexAttribPointer(3, 1, GL_FLOAT, bytebool(GL_FALSE), SizeOf(TVoxelInfo),
		@PVoxelInfo(nil)^.size);
end;

procedure DontReload(chunk: PLoadedChunk);
begin
end;

var
	reloadLookup: array[boolean] of TLCAlterProc = (@DontReload, @DoReload);

procedure TLoadedChunk.DoDraw;
begin
	reloadLookup[fillthreadfinished](@Self);
	EngineShader.SetRenderOffset(AbsRenderPos);
	glBindVertexArray(vertexarray);
	glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 14, voxelcount);
end;

{ TWorldChunk }

constructor TWorldChunkPlain.Create(const nposition: TVec3i);
var
	filestream: TFileStream;
	filename: string;
begin
	position := nposition;

	filename := mapDataPath + BuildFileName;
	if FileExists(filename) then
	begin
		filestream := TFileStream.Create(filename, fmOpenRead);

		filestream.ReadBuffer(octreecount, SizeOf(octreecount));
		GetMem(octree.o, octreecount * SizeOf(TOcEntryPlain));
		filestream.ReadBuffer(octree.o^, octreecount * SizeOf(TOcEntryPlain));

		FreeAndNil(filestream);
	end
	else
		octree.o := nil;

	usecnt := 0;
end;

function TWorldChunkPlain.BuildFileName: string;
begin
	Result := BinToHexStr(@position, SizeOf(position));
end;

destructor TWorldChunkPlain.Destroy;
begin
	FreeMem(octree.o, octreecount * SizeOf(TOcEntryPlain));
	activechunks.Delete(@Self);
end;

function TWorldChunkPlain.Use: TOcEntry;
begin
	Result := octree;
	Inc(usecnt);
end;

procedure TWorldChunkPlain.UnUnse;
begin
	Dec(usecnt);
	if usecnt = 0 then
		Destroy;
end;

function TWorldChunkPlain.RelToAbsPos(input: TVec3): TVec3;
begin
	Result.X := position.X + input.X / 128;
	Result.Y := position.Y + input.Y / 128;
	Result.Z := position.Z + input.Z / 128;
	//Result := position + (input / 128);
end;

function TWorldChunkPlain.AbsToRelPos(input: TVec3): TVec3s;
begin
	Result.X := round((input.X - position.X) * 128);
	Result.Y := round((input.Y - position.Y) * 128);
	Result.Z := round((input.Z - position.Z) * 128);
end;


procedure DoAccu(input: word; var accu: cardinal);
begin
	accu += input and $1;
end;

procedure DontAccu({%H-}input: word; var {%H-}accu: cardinal);
begin

end;

type
	TAccumulateProc = procedure(input: word; var accu: cardinal);

var
	acculookup: array[boolean] of TAccumulateProc = (@DontAccu, @DoAccu);

procedure Accumulate01BitPairs(input: word; var accu: cardinal);
begin
	acculookup[(input and $2) = 0](input, accu);
	input := input shr 2;
	acculookup[(input and $2) = 0](input, accu);
	input := input shr 2;
	acculookup[(input and $2) = 0](input, accu);
	input := input shr 2;
	acculookup[(input and $2) = 0](input, accu);
	input := input shr 2;
	acculookup[(input and $2) = 0](input, accu);
	input := input shr 2;
	acculookup[(input and $2) = 0](input, accu);
	input := input shr 2;
	acculookup[(input and $2) = 0](input, accu);
	input := input shr 2;
	acculookup[(input and $2) = 0](input, accu);
end;

function TWorldChunkPlain.CountVoxels: cardinal;
var
	it: cardinal;
begin
	it := 0;
	Result := 0;
	while it < octreecount do
	begin
		Accumulate01BitPairs((octree.o + it)^.map, Result);
		Inc(it);
	end;
end;

initialization
	mapDataPath := EnforceDir('map');
end.
