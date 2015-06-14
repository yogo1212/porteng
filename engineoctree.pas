unit EngineOctree;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dglOpenGL, EngineDataTypes, EngineTypes, EngineMath, EngineMemory;

type

	{ TOcStoreEntry }

	TOcType = (OcEmpty = 0, OcFull, OcRec, OcData);

	TOcStoreEntryPlain = record
		case boolean of
			False:		// 3-bit-index, 1st bit is x, 2nd y and 3rd z
			// two bits are addressed
			(map,
				// childs are this far away [TOcStoreEntryPlain]
				offset: word);
			// if this OcData
			True: (id: cardinal);
	end;

	// LeftBottomFront
	TOcPos = (opLBF = 0, opRBF, opLTF, opRTF, opLBR, opRBR, opLTR, opRTR);

	POcPart = ^_TOcPart;

	{ TOcPar }

	{ TOcPart }

	TOcPart = object
		o: POcPart;

		// this works with half-sizes. as does the shader.
		procedure GetVoxelData(const relrenderPos: TVec3; Data: TContinuousMemoryManager;
			LoDper1: word);

		function Recurse(pos: TOcPos): TOcPart;

		procedure LoadFromFile(const filepath: string; const size: GLfloat);
		procedure SaveToFile(const filepath: string);

		procedure CleanUp;
	end;

	{ TOcPart }

	_TOcPart = record
		position: TVec3;
		hsize: GLfloat;
		colour: TCol4b;

		case _type: TOcType of
			OcEmpty: ();
			OcFull: (
			);
			OcRec: (
				children: array[TOcPos] of POcPart;
			);
			OcData: ();
	end;

function OcPosToRelOffset(const pos: TOcPos): TVec3; inline;

implementation

function finaliseColour(const input: TCol4b): TCol4b; inline;
begin
	// elements of input-colour are 0;64;128 or 192
	PCol3b(@Result)^ := (PCol3b(@input)^ div $30) * 85;
	// r g and b are now 0;85;170;255
{$IFDEF USEDEPTHACCU}
	Result.w := 63 + input.w;
	// a is now 63;127;191;255
{$ELSE}
	// Sadly, before order-independent-transparency-rendering is feasible:
	Result.A := 255;
{$ENDIF}
end;

{ TOcPart }

procedure TOcPart.GetVoxelData(const relrenderPos: TVec3;
	Data: TContinuousMemoryManager; LoDper1: word);
var
	vi: TVoxelInfo;
	tmp: POcPart;
	queue: TSimpleQueue;
	cnt: byte;
begin
	queue := TSimpleQueue.Create(o);
	repeat
		tmp := queue.Value;

		if LengthSquare(relrenderPos - tmp^.position) - sqr(tmp^.hsize) <
			sqr(LoDper1 * tmp^.hsize) then
		begin

			if tmp^._type = OcFull then
			begin
				vi.colour := finaliseColour(tmp^.colour);
				vi.hsize := tmp^.hsize;
				vi.position := tmp^.position;
				Data.Append(@vi);
			end;

			if tmp^._type = ocRec then
			begin
				for cnt := 0 to 7 do
				begin
					queue.Enqueue(tmp^.children[TOcPos(cnt)]);
				end;
			end;

		end
		else
		begin
			// TODO might want to a bit recursive here
			vi.colour := finaliseColour(tmp^.colour);
			vi.hsize := tmp^.hsize;
			vi.position := tmp^.position;
			Data.Append(@vi);
		end;
	until not queue.Dequeue;
	FreeAndNil(queue);
end;

function TOcPart.Recurse(pos: TOcPos): TOcPart;
begin
	Result.o := nil;
	if o^._type = OcRec then
	begin
		Result.o := o^.children[pos];
	end;
	// TODO raise here?
end;

// :-D
// now my naming is biting me in the a*
function BitAtPos(index: TOcPos; pos: byte): byte; inline;
begin
	Result := (Ord(index) shr pos) and 1;
end;

function FunnyColourFromIndex(index: TOcPos): TCol4b; inline;
begin
	Result := Col4b(-64 * BitAtPos(index, 2) + 64 * BitAtPos(index, 0),
		-64 * BitAtPos(index, 1) + 64 * BitAtPos(index, 5), -64 *
		BitAtPos(index, 6) + 64 * BitAtPos(index, 4), -64 * BitAtPos(index, 3) +
		64 * BitAtPos(index, 7)) * $30;
end;

procedure TOcPart.LoadFromFile(const filepath: string; const size: GLfloat);
var
	filestream: TFileStream;

	procedure LoadFromStream;
	var
		queue: TSimpleQueue;
		ot: TOcStoreEntryPlain;
		cnt: byte;
		parent, tmp: POcPart;
	begin
		o^.colour := Col4b(128, 128, 128, 128);

		if filestream.Size = 0 then
		begin
			o^._type := OcFull;
			exit;
		end;

		o^._type := OcRec;

		queue := TSimpleQueue.Create(o);
		repeat
			filestream.Read(ot, sizeof(ot));

			parent := queue.Value;

			for cnt := 0 to 7 do
			begin
				new(tmp);
				tmp^._type := TOcType(ot.map and $3);
				tmp^.hsize := parent^.hsize / 2;
				tmp^.position := parent^.position + OcPosToRelOffset(TOcPos(cnt)) *
					parent^.hsize;

				tmp^.colour := parent^.colour + FunnyColourFromIndex(TOcPos(cnt));
				if tmp^._type = OcRec then
				begin
					queue.Enqueue(tmp);
				end;

				ot.map := ot.map shr 2;
				parent^.children[TOcPos(cnt)] := tmp;
			end;
		until not queue.Dequeue;
		FreeAndNil(queue);
	end;

begin
	New(o);
	o^.position := Vec3(0, 0, 0);
	o^.hsize := size / 2;

	if FileExists(filepath) then
	begin
		filestream := TFileStream.Create(filepath, fmOpenRead);
		LoadFromStream;
		FreeAndNil(filestream);
	end
	else
	begin
		o^._type := OcEmpty;
	end;
end;

procedure TOcPart.SaveToFile(const filepath: string);
var
	filestream: TFileStream;
	octreecount: cardinal;
begin
	filestream := TFileStream.Create(filepath, fmOpenReadWrite);

	// start a thread, let that write the octrees to a pipe
	// read from pipe here and write to file, if that has changed

	// alternative: write diff
	{
  filestream.ReadBuffer(octreecount, SizeOf(octreecount));

  GetMem(storeTree.o, octreecount * SizeOf(TOcStoreEntryPlain));
  filestream.ReadBuffer(storeTree.o, octreecount * SizeOf(TOcStoreEntryPlain));

  max_voxel_count := countVoxels(storeTree.o, octreecount);

  FreeAndNil(filestream);
  }
end;

procedure TOcPart.CleanUp;
var
	queue: TSimpleQueue;
	tmp: POcPart;
	cnt: byte;
begin
	queue := TSimpleQueue.Create(o);
	repeat
		tmp := queue.Value;

		if tmp^._type = ocRec then
		begin
			for cnt := 0 to 7 do
			begin
				queue.Enqueue(tmp^.children[TOcPos(cnt)]);
			end;
		end;
		Dispose(tmp);
	until not queue.Dequeue;
	FreeAndNil(queue);
	o := nil;
end;

function OcPosToRelOffset(const pos: TOcPos): TVec3;
begin
	Result := Vec3(-0.5, -0.5, -0.5);
	if (Ord(pos) and 1) <> 0 then
		Result.X += 1;
	if ((Ord(pos) shr 1) and 1) <> 0 then
		Result.Y += 1;
	if ((Ord(pos) shr 2) and 1) <> 0 then
		Result.Y += 1;
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

end.
