unit EngineWorld;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineShader, Math, EngineTypes, EngineFileUtils,
	Convenience, EngineMath, EngineMemory, EngineDebug, EngineOctree;

{$UNDEF ENGINEDEBUG}
{$UNDEF USEDEPTHACCU}
type

	{ TWorldChunkPlain }

	TWorldChunkPlain = object
	private
		octree: TOcPart;

		usecnt: byte;

		function BuildFileName: string;
	public
		position: TChunkPosition;

		constructor Create(const nposition: TChunkPosition);
		destructor Destroy;

		function Use: TOcPart;
		procedure UnUnse;

		function AbsToRelPos(input: TGamePosition): TVec3;
	end;

	{ TWorldChunk }

	TWorldChunk = ^TWorldChunkPlain;

	{ TFillThread }

	TFillThread = class(TThread)
		Data: TContinuousMemoryManager;
		chunk: TWorldChunk;
		finished: PBoolean;
		AbsViewPos: TGamePosition;
		LoDper1: word;
		constructor Create(nchunk: TWorldChunk; finishedflag: PBoolean;
			nAbsViewPos: TGamePosition; nLoDper1: word);
		destructor Destroy; override;
		procedure Execute; override;
		procedure CollectResult(vertexbuffer: GLuint; pvoxelcount: PCardinal);
	end;

	{ TLoadedChunk }

	TUpdateRenderDataProc = procedure(const nAbsViewPos: TGamePosition;
		const nLoDper1: word) of object;

	TLoadedChunk = object
	private
		fillthread: TFillThread;

		procedure FillRenderData;
		procedure DontUpdateRenderData(const {%H-}nAbsViewPos: TGamePosition;
			const {%H-}nLoDper1: word);
		procedure DoDraw;
	public
		chunk: TWorldChunk;
		AbsRenderPos: TChunkPosition;
		AbsViewPos: TGamePosition;
		LoDper1: word;

		vertexarray, vertexbuffer: GLuint;
		voxelcount: word;

		fillthreadfinished: boolean;
		UpdateRenderData: TUpdateRenderDataProc;
		draw: TObjProc;

		constructor Create(const nAbsRenderPos: TChunkPosition);
		destructor Destroy;

		procedure DoUpdateRenderData(const nAbsViewPos: TGamePosition; const nLoDper1: word);
	end;

	PLoadedChunk = ^TLoadedChunk;

procedure GameWorldInit;
function CheckWorldCollision(var pos: TGamePosition; movement: TVec3): boolean;
function TryNormalise(var pos: TGamePosition): boolean;

function CastRayEmpty(const ocentry: TOcPart; var pos: TVec3;
	var direction: TVec3): boolean;
function CastRayFull(const ocentry: TOcPart; var pos: TVec3;
	var direction: TVec3): boolean;
function CastRayRec(const ocentry: TOcPart; var pos: TVec3;
	var direction: TVec3): boolean;

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

procedure GameWorldInit;
const
	size: GLfloat = 1;
var
	fobj: array[0..13] of TVec3;
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
	if pos > worldChunkSize / 2 then
	begin
		pos -= worldChunkSize;
		Inc(worldPos);
	end
	else
{$IFDEF DEBUGNORMALISER}
	if pos < -worldChunkSize / 2 then
{$ENDIF}
	begin
		pos += worldChunkSize;
		Dec(worldPos);
	end
{$IFDEF DEBUGNORMALISER}
	else
		raise Exception.Create('normaliseWorldPos ran into else!!');
{$ENDIF}
	;
end;


type
	TRayCastFunc = function(const ocentry: TOcPart; var pos: TVec3;
		var direction: TVec3): boolean;

const
	CastRayOctree: array[TOcType] of TRayCastFunc =
		(@CastRayEmpty, @CastRayFull, @CastRayRec, @CastRayEmpty);

procedure DontProgressVectorComponent(const {%H-}pos, {%H-}dir: GLfloat;
	var {%H-}Result: GLfloat);
begin

end;

procedure DoProgressVectorComponent(const pos, dir: GLfloat; var Result: GLfloat);
var
	offset: TIntFloat;
	tmp: GLdouble;
{
  what is this function for?
  it deals with the problem that we want know how far ray cast in the unit-cube can travel
  before it hits a wall (boundary of the cube).
  pos is in [-1;1]
  instead of branching and doing two divisions..
  pos is translated to a space in [-2;0] or [0;2] (depending on the sign of dir) and
  stored in offset.floatval
  this translated value now get's the same sign as dir
  and can then be divided by dir to yield a positive result
}
begin
	//This gives a unit float with same sign of dir
	offset.floatval := 1;
	offset.intval := offset.intval or (Pglfloatsizeduint(@dir)^ and glfloatsignbit);
	// and adds it to pos
	offset.floatval := pos - offset.floatval;
	// now the result needs to have the same size as dir because of the sign of the offset
	offset.intval := offset.intval xor glfloatsignbit;

	// don't run into sigfpe
	tmp := GLdouble(offset.floatval) / dir;
	// TODO sse?
	Result := Min(tmp, Result);
	// no else necessary because result is expected to be 1 at most.
end;

type
	TProgressVectorProc = procedure(const pos, dir: GLfloat; var Result: GLfloat);

const
	progressVectorComponent: array[boolean] of TProgressVectorProc =
		(@DontProgressVectorComponent, @DoProgressVectorComponent);

function CastRayEmpty(const ocentry: TOcPart; var pos: TVec3;
	var direction: TVec3): boolean;
var
	m: GLfloat;
begin
	{
  intersection of ray with unit cube (3 planes)
  the ray being s + m * d = r; where s, d and r are vectors
  the planes being n * (x - t) = 0 - all of them vectors (except for zero).
  we only need to check againt the 3 planes dir is pointing at (pointing at from within
  the cube) e.g. for dir.x > 0 that can only be (1,s,t) (normal and point are identical).
  in our case all vectors are already translated in a way that the current voxel is a
  cube with a length of 2 located at origin.
  that way t and n are identical and unit.

  substituting r for x yields: -n*(n - t) / n*d = m

  but: n (the plane in which one side of the unit cube lies) will only be a vector with
  two components being 0 and one being either 1 or -1
  => n*n is always one.
  so for each plane this is the equation: (1 - t_c) / (n_c * d_c) = m
  Otherwise, the dot-product is equivalent to a projection and a potential sign-toggle.
  }

	m := 1;

	progressVectorComponent[direction.X <> 0](pos.X, direction.X, m);
	progressVectorComponent[direction.Y <> 0](pos.Y, direction.Y, m);
	progressVectorComponent[direction.Z <> 0](pos.Z, direction.Z, m);

	// 3 possible branches so far (min)  (0 with sse)

	// if pos is not inside the unit-sized cube at origin, then m could be zero(or less)
	// and there would have to be checks here.
	pos += direction * m;
	direction *= (1 - m);
	//Result := (m < 1);
	Result := False;
	// no further checks necessary
end;

function CastRayFull(const ocentry: TOcPart; var pos: TVec3;
	var direction: TVec3): boolean;
begin
	// TODO is it wise to remove checks here?
	// if for some reason you are inside the geometry, you are stuck there ... forever! :-D
	// it is common practice to only test for collision on surfaces
	Result := True;
end;

procedure DontCorrectSigns({%H-}pos: Pglfloat; const {%H-}direction: glfloat);
begin

end;

procedure DoCorrectSigns(pos: Pglfloat; const direction: glfloat);
begin
	// let the sign of pos be the same of that of direction
	// pos should be zero and changing the sign should only be relevant for our tweaks
	// if in doubt, this can be solved with branching, because the case of one component of
	// a vector being zero should not occur to often
	Pglfloatsizeduint(pos)^ := Pglfloatsizeduint(pos)^ and not glfloatsignbit;
	Pglfloatsizeduint(pos)^ := Pglfloatsizeduint(pos)^ xor
		(Pglfloatsizeduint(@direction)^ and glfloatsignbit);
end;

type
	TSignProc = procedure(pos: Pglfloat; const direction: glfloat);

const
	fixSignProblems: array[boolean] of TSignProc = (@DontCorrectSigns, @DoCorrectSigns);

function CastRayRec(const ocentry: TOcPart; var pos: TVec3;
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

	index := (((Pglfloatsizeduint(@pos.X)^ and glfloatsignbit) shr glfloatsignbitindex) or
		((Pglfloatsizeduint(@pos.Y)^ and glfloatsignbit) shr (glfloatsignbitindex - 1)) or
		((Pglfloatsizeduint(@pos.Z)^ and glfloatsignbit) shr
		(glfloatsignbitindex - 2))) xor $7;
	{writeln(
    chr(((Pglfloatsizeduint(@pos.X)^ and glfloatsignbit) shr glfloatsignbitindex)
      + Ord('0')),
    chr(((Pglfloatsizeduint(@pos.Y)^ and glfloatsignbit) shr (glfloatsignbitindex - 1))
      + Ord('0')),
    chr(((Pglfloatsizeduint(@pos.Z)^ and glfloatsignbit) shr (glfloatsignbitindex - 2))
      + Ord('0')));}

	direction *= 2;
	pos := (pos - Vec3((index and 1) - 0.5, ((index shr 1) and 1) - 0.5,
		((index shr 2) and 1) - 0.5)) * 2;

	Result := CastRayOctree[ocentry.Recurse(TOcPos(index)).o^._type](
		ocentry.Recurse(TOcPos(index)), pos, direction);

	pos := (pos / 2) + Vec3((index and 1) - 0.5, ((index shr 1) and 1) -
		0.5, ((index shr 2) and 1) - 0.5);
	direction /= 2;

	if ((pos.X = 0) and (direction.X <> 0)) or ((pos.Y = 0) and (direction.Y <> 0)) or
		((pos.Z = 0) and (direction.Z <> 0)) and (not Result) then
	begin
		prepareSigns(pos, direction);

		index := (((Pglfloatsizeduint(@pos.X)^ and glfloatsignbit) shr
			glfloatsignbitindex) or ((Pglfloatsizeduint(@pos.Y)^ and glfloatsignbit) shr
			(glfloatsignbitindex - 1)) or ((Pglfloatsizeduint(@pos.Z)^ and glfloatsignbit) shr
			(glfloatsignbitindex - 2))) xor $7;

		direction *= 2;
		pos := (pos - Vec3((index and 1) - 0.5, ((index shr 1) and 1) -
			0.5, ((index shr 2) and 1) - 0.5)) * 2;

		Result := CastRayOctree[ocentry.Recurse(TOcPos(index)).o^._type](
			ocentry.Recurse(TOcPos(index)), pos, direction);

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
	TCorrectComponentProc = procedure(var worldPos: GLint; var pos: GLfloat;
		const dir: GLfloat; var flag: boolean); register;

procedure DontTryCorrectComponent(var {%H-}worldPos: GLint;
	var {%H-}pos: GLfloat; const {%H-}dir: GLfloat; var {%H-}flag: boolean);
begin

end;

procedure DoTryCorrectComponent(var worldPos: GLint; var pos: GLfloat;
	const dir: GLfloat; var flag: boolean);
begin
	if (pos > worldChunkSize / 2) or ((pos = worldChunkSize / 2) and
		((Pglfloatsizeduint(@dir)^ and glfloatsignbit) = 0)) then
	begin
		pos -= worldChunkSize;
		worldPos += 1;
		flag := True;
	end
	else if (pos < -worldChunkSize / 2) or ((pos = -worldChunkSize / 2) and
		((Pglfloatsizeduint(@dir)^ and glfloatsignbit) <> 0)) then
	begin
		pos += worldChunkSize;
		worldPos -= 1;
		flag := True;
	end;
end;

const
	TryCorrectComponent: array[boolean] of TCorrectComponentProc =
		(@DontTryCorrectComponent, @DoTryCorrectComponent);

function TryNormalise(var pos: TGamePosition): boolean;
begin
	Result := False;
	if Abs(pos.offset.X) > worldChunkSize / 2 then
	begin
		normaliseWorldPos(pos.worldPos.X, pos.offset.X);
		Result := True;
	end;
	if Abs(pos.offset.Y) > worldChunkSize / 2 then
	begin
		normaliseWorldPos(pos.worldPos.Y, pos.offset.Y);
		Result := True;
	end;
	if Abs(pos.offset.Z) > worldChunkSize / 2 then
	begin
		normaliseWorldPos(pos.worldPos.Z, pos.offset.Z);
		Result := True;
	end;
end;

function CheckWorldCollision(var pos: TGamePosition; movement: TVec3): boolean;
var
	tmpchunk: TWorldChunkPlain;
	flag: boolean;
begin
	repeat
		tmpchunk.position := pos.worldPos;

		RelToMiau(pos.offset, movement, worldChunkSize / 2);

		// TODO inline if  or not? hopefully it will be predicted.
		if activechunks.Fetch(@tmpchunk) then
			Result := CastRayOctree[OcRec](tmpchunk.octree, pos.offset, movement)
		else
			// TODO remove this branch? doesn't seem to work
			Result := CastRayOctree[OcEmpty](tmpchunk.octree, pos.offset, movement);

		MiauToRel(pos.offset, movement, worldChunkSize / 2);

		flag := TryNormalise(pos);
	until not flag or Result;
end;

{$ifdef ENGINEDEBUG}
procedure printVoxelArray(pnt: PVoxelInfo; cnt: cardinal);
begin
	while cnt > 0 do
	begin
		Dec(cnt);
		logTrace(VecToStr(pnt^.position) + ': ' + FloatToStr(pnt^.hsize) +
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

{ TFillThread }

constructor TFillThread.Create(nchunk: TWorldChunk; finishedflag: PBoolean;
	nAbsViewPos: TGamePosition; nLoDper1: word);
begin
	inherited Create(True);
	chunk := nchunk;
	LoDper1 := nLoDper1;
	finished := finishedflag;
	AbsViewPos := nAbsViewPos;
	Data := TContinuousMemoryManager.Create(Sizeof(TVoxelInfo), 100);
end;

destructor TFillThread.Destroy;
begin
	FreeAndNil(Data);
	inherited Destroy;
end;

procedure TFillThread.Execute;
begin
	chunk^.octree.GetVoxelData(chunk^.AbsToRelPos(AbsViewPos) / 2, Data, LoDper1);

{$ifdef ENGINEDEBUG}
	printVoxelArray(Data.Get(0), Data.Count);
	logTrace('Built new chunk with ' + IntToStr(Data.Count) + ' voxels');
	logTrace('Chunk Data ' + BinToHexStr(Data, Data.Count * SizeOf(TVoxelInfo)));
{$endif}
	if not terminated then
		finished^ := True;
	Terminate;
end;

procedure TFillThread.CollectResult(vertexbuffer: GLuint; pvoxelcount: PCardinal);
begin
	glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
	glBufferData(GL_ARRAY_BUFFER, Data.Count * SizeOf(TVoxelInfo),
		Data.Get(0), GL_STATIC_DRAW);
	pvoxelcount^ := Data.Count;
end;

{ TLoadedChunk }

constructor TLoadedChunk.Create(const nAbsRenderPos: TChunkPosition);
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

procedure TLoadedChunk.DoUpdateRenderData(const nAbsViewPos: TGamePosition;
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

procedure TLoadedChunk.DontUpdateRenderData(const nAbsViewPos: TGamePosition;
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
		@PVoxelInfo(nil)^.hsize);
end;

procedure DontReload({%H-}chunk: PLoadedChunk);
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

constructor TWorldChunkPlain.Create(const nposition: TChunkPosition);
var
	filename: string;
begin
	position := nposition;

	filename := mapDataPath + BuildFileName;
	if FileExists(filename) then
		octree.LoadFromFile(filename, worldChunkSize);

	usecnt := 0;
end;

function TWorldChunkPlain.BuildFileName: string;
begin
	Result := BinToHexStr(@position, SizeOf(position));
end;

destructor TWorldChunkPlain.Destroy;
begin
	octree.CleanUp;
	activechunks.Delete(@Self);
end;

function TWorldChunkPlain.Use: TOcPart;
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

function TWorldChunkPlain.AbsToRelPos(input: TGamePosition): TVec3;
begin
	Result.X := ((input.worldPos.X - position.X) * 512 + input.offset.X);
	Result.Y := ((input.worldPos.Y - position.Y) * 512 + input.offset.Y);
	Result.Z := ((input.worldPos.Z - position.Z) * 512 + input.offset.Z);
	//Result.X := round((input.X - position.X) * 128);
	//Result.Y := round((input.Y - position.Y) * 128);
	//Result.Z := round((input.Z - position.Z) * 128);
end;


initialization
	mapDataPath := EnforceDir('map');
end.
