unit TestTypes;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, syncobjs, dynstructs, NetThread;

type

	{ TNetNode }

	TNetNode = class(TInterfacedObject, IBinTreeElement)
		thr: TNetThread;
		function GetKey: longword;
	end;

	TStuffType = (stSend, stRcvd, stGone);

	TStuff = record
		src: word;
		what: TStuffType;
		milli: longword;
		packet: longword;
	end;

	{ TStuffQueue }

	TStuffQueue = object
		rPos, wPos: byte;
		writSect: TCriticalSection;
		stufflist: array[byte] of TStuff;
		function dequeue: TStuff;
		procedure enqueue(newstuff: TStuff);
		constructor Create;
	end;

function Stuff(src: word; what: TStuffType; milli: longword; packet: longword): TStuff;

implementation

function Stuff(src: word; what: TStuffType; milli: longword; packet: longword): TStuff;
begin
	Result.src := src;
	Result.what := what;
	Result.milli := milli;
	Result.packet := packet;
end;

{ TNetNode }

function TNetNode.GetKey: longword;
begin
	Result := thr.id;
end;

{ TStuffQueue }

function TStuffQueue.dequeue: TStuff;
begin
	while wPos = rPos do
	begin
		Sleep(10);
		// Pseudo-Blocking
	end;
	Result := stufflist[rPos];
	Inc(rPos);
	// This is only called by one reading thread
end;

procedure TStuffQueue.enqueue(newstuff: TStuff);
begin
	writSect.Acquire;
	stufflist[wPos] := newstuff;

	writSect.Release;
end;

constructor TStuffQueue.Create;
begin
	writSect := TCriticalSection.Create;
	rPos := 0;
	wPos := 0;
end;

end.


