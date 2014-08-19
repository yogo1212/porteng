unit NetTypes;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, syncobjs, SortedList;

type

	TStuffType = (stSend, stRcvd, stGone);

	{ TNetNode }

	TNetNode = class(TSortedListEntry)
		thr: TThread;
		procedure NodeFunc;
		constructor Create;
		function isGreater(r: TSortedListEntry): boolean; override;
	end;

	{ TStuff }

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
		destructor Destroy;
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

procedure TNetNode.NodeFunc;
begin
	while True do
	begin

	end;
end;

constructor TNetNode.Create;
type

	{ TNodeThread }

	TNodeThread = class(TThread)
		node: TNetNode;
		procedure Execute; override;
		constructor Create(bla: TNetNode);
	end;

	{ TNodeThread }

	procedure TNodeThread.Execute;
	begin
		node.NodeFunc;
	end;

	constructor TNodeThread.Create(bla: TNetNode);
	begin
		inherited Create(True);
		node := bla;
	end;

begin
	thr := TNodeThread.Create(Self);

	thr.Start;
end;

function TNetNode.isGreater(r: TSortedListEntry): boolean;
begin

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
	Inc(wPos);

	writSect.Release;
end;

constructor TStuffQueue.Create;
begin
	writSect := TCriticalSection.Create;
	rPos := 0;
	wPos := 0;
end;

destructor TStuffQueue.Destroy;
begin
	FreeAndNil(writSect);
end;

end.



