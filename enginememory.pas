unit EngineMemory;

{$mode objfpc}{$H+}
{$CALLING REGISTER}

interface

uses
	Classes, SysUtils, Math, EngineDataTypes;

type

	//TComparer = function(a, b: Pointer): smallint;

	_TPointerComparer = specialize TGameComparer<Pointer>;

	{ TPCardinalComparer }
	// This better be somewhere else

	TPCardinalComparer = class(_TPointerComparer)
		function compare(o1, o2: Pointer): shortint; override;
		function greater(o1, o2: Pointer): boolean; override;
		function equal(o1, o2: Pointer): boolean; override;
		function less(o1, o2: Pointer): boolean; override;
	end;

	{ TIndexedMemoryManager }

	TIndexedMemoryManager = class
		abstract
	protected
		esize, used: cardinal;
		constructor Create(nelementsize: cardinal);
	public
		// get the pointer to the i-th element
		function Get(i: cardinal): Pointer; virtual; abstract;
		// copy the data from p to the i-th element
		procedure Put(i: cardinal; p: Pointer); virtual; abstract;
		// make space for another element at i
		procedure Insert(i: cardinal); virtual; abstract;
		procedure DeleteAt(index: cardinal); virtual; abstract;
		procedure Clear; virtual; abstract;
		procedure Append(p: Pointer);
		property Count: cardinal read used;
		property Elementsize: cardinal read esize;
		property Items[i: cardinal]: Pointer read Get write Put;
	end;

	{ TContinuousMemoryManager }

	TContinuousMemoryManager = class(TIndexedMemoryManager)
	protected
		bsize, capacity: cardinal;
		Data: PByte;
	public
		procedure DeleteAt(index: cardinal); override;
		//procedure Delete(p: Pointer);
		function Get(i: cardinal): Pointer; override;
		procedure Clear; override;
		procedure Put(i: cardinal; p: Pointer); override;
		procedure Insert(i: cardinal); override;
		constructor Create(nelementsize, blocksize: cardinal);
		destructor Destroy; override;
	end;

	{ TFragmentedMemoryManager }

	TFragmentedMemoryManager = class(TIndexedMemoryManager)
		mem: TIndexedMemoryManager;
		function Get(i: cardinal): Pointer; override;
		procedure Put(i: cardinal; p: Pointer); override;
		procedure DeleteAt(index: cardinal); override;
		procedure Insert(i: cardinal); override;
		procedure Clear; override;
		property Items[i: cardinal]: Pointer read Get write Put;
		constructor Create(nelementsize, blocksize: cardinal);
		destructor Destroy; override;
	end;

	{ TKeyBasedMemoryManager }

	TKeyBasedMemoryManager = class
		abstract
	private
		function GetCount: cardinal;
		function GetPointerAt(i: cardinal): Pointer;
	protected
		mem: TIndexedMemoryManager;
		compare: _TPointerComparer;
		function GetIndex(p: Pointer; out index: cardinal): boolean; virtual; abstract;
	public
		function Fetch(p: Pointer): boolean;
		function Store(p: Pointer): Pointer;
		function Delete(p: Pointer): boolean; virtual; overload;
		destructor Destroy; override;
		procedure Clear;
		constructor Create(comparer: _TPointerComparer; memory: TIndexedMemoryManager);
		property Traverse[i: cardinal]: Pointer read GetPointerAt;
		property Count: cardinal read GetCount;
	end;

	{ TSortedMemoryManager }

	TSortedMemoryManager = class(TKeyBasedMemoryManager)
	protected
		function GetIndex(p: Pointer; out index: cardinal): boolean; override;
	end;

implementation

{ TPCardinalComparer }

function TPCardinalComparer.compare(o1, o2: Pointer): shortint;
begin
	Result := 0;
	if PCardinal(o1)^ < PCardinal(o2)^ then
		Result := -1
	else if PCardinal(o1)^ > PCardinal(o2)^ then
		Result := 1;
end;

function TPCardinalComparer.greater(o1, o2: Pointer): boolean;
begin
	Result := PCardinal(o1)^ > PCardinal(o2)^;
end;

function TPCardinalComparer.equal(o1, o2: Pointer): boolean;
begin
	Result := PCardinal(o1)^ = PCardinal(o2)^;
end;

function TPCardinalComparer.less(o1, o2: Pointer): boolean;
begin
	Result := PCardinal(o1)^ < PCardinal(o2)^;
end;

{ TFragmentedMemoryManager }

function TFragmentedMemoryManager.Get(i: cardinal): Pointer;
begin
	Result := PPointer(mem.Get(i))^;
end;

procedure TFragmentedMemoryManager.Put(i: cardinal; p: Pointer);
begin
	Move(p^, Get(i)^, esize);
end;

procedure TFragmentedMemoryManager.DeleteAt(index: cardinal);
begin
	Freemem(Get(index), esize);
	mem.DeleteAt(index);
	used := mem.used;
end;

procedure TFragmentedMemoryManager.Insert(i: cardinal);
begin
	mem.Insert(i);
	GetMem(PPointer(mem.Get(i))^, esize);
	used := mem.used;
end;

procedure TFragmentedMemoryManager.Clear;
var
	cnt: cardinal;
begin
	cnt := mem.used;
	while cnt > 0 do
	begin
		Dec(cnt);
		DeleteAt(cnt);
	end;
	used := 0;
end;

constructor TFragmentedMemoryManager.Create(nelementsize, blocksize: cardinal);
begin
	inherited Create(nelementsize);
	used := 0;
	mem := TContinuousMemoryManager.Create(SizeOf(Pointer), blocksize);
end;

destructor TFragmentedMemoryManager.Destroy;
var
	cnt: cardinal;
begin
	cnt := 0;
	while cnt < mem.used do
	begin
		Freemem(Get(cnt), esize);
		Inc(cnt);
	end;
	FreeAndNil(mem);
	inherited Destroy;
end;

{ TSortedMemoryManager }

function TSortedMemoryManager.GetIndex(p: Pointer; out index: cardinal): boolean;
var
	tmp: smallint;
begin
	// TODO hier binary-search
	index := 0;
	tmp := 1;
	while (index < Mem.Count) and (tmp > 0) do
	begin
		tmp := compare.compare(p, mem.Get(index));
		Inc(index);
	end;
	Result := tmp = 0;
	if tmp < 1 then
		Dec(index);
end;

{ TKeyBasedMemoryManager }

constructor TKeyBasedMemoryManager.Create(comparer: _TPointerComparer;
	memory: TIndexedMemoryManager);
begin
	inherited Create;
	mem := memory;
	compare := comparer;
end;

destructor TKeyBasedMemoryManager.Destroy;
begin
	FreeAndNil(mem);
	inherited Destroy;
end;

procedure TKeyBasedMemoryManager.Clear;
begin
	mem.Clear;
end;

function TKeyBasedMemoryManager.GetCount: cardinal;
begin
	Result := mem.Count;
end;

function TKeyBasedMemoryManager.GetPointerAt(i: cardinal): Pointer;
begin
	Result := mem.Get(i);
end;

function TKeyBasedMemoryManager.Fetch(p: Pointer): boolean;
var
	index: cardinal;
begin
	Result := GetIndex(p, index);
	if Result then
		Move(mem.Get(index)^, p^, mem.esize);
end;

function TKeyBasedMemoryManager.Store(p: Pointer): Pointer;
var
	index: cardinal;
begin
	if not GetIndex(p, index) then
		mem.Insert(index);
	Result := mem.Get(index);
	Move(p^, Result^, mem.esize);
end;

function TKeyBasedMemoryManager.Delete(p: Pointer): boolean;
var
	index: cardinal;
begin
	Result := GetIndex(p, index);
	if Result then
	begin
		Move(mem.get(index)^, p^, mem.Elementsize);
		mem.DeleteAt(index);
	end;
end;

{ TIndexedMemoryManager }

constructor TIndexedMemoryManager.Create(nelementsize: cardinal);
begin
	esize := nelementsize;
end;

procedure TIndexedMemoryManager.Append(p: Pointer);
begin
	Insert(Count);
	Put(Count - 1, p);
end;

{ TContinuousMemoryManager }

constructor TContinuousMemoryManager.Create(nelementsize, blocksize: cardinal);
begin
	bsize := blocksize;
	esize := nelementsize;
	used := 0;
	capacity := blocksize;
	Getmem(Data, capacity * esize);
end;

destructor TContinuousMemoryManager.Destroy;
begin
	Freemem(Data, capacity * esize);
	inherited Destroy;
end;

procedure TContinuousMemoryManager.DeleteAt(index: cardinal);
var
	tmpbytes: PByte;
	len: cardinal;
begin
	Dec(used);
	len := (used - index) * esize;
	if used < (capacity - floor(1.5 * bsize)) then
	begin
		Dec(capacity, bsize);
		tmpbytes := Data;
		GetMem(Data, capacity * esize);
		Move(tmpbytes[0], Data[0], index * esize);
		Move(tmpbytes[(index + 1) * esize], Data[index * esize], len);
		Freemem(tmpbytes);
	end
	else if len <> 0 then
	begin
		Getmem(tmpbytes, len);
		Move(Data[(index + 1) * esize], tmpbytes^, len);
		Move(tmpbytes^, Data[index * esize], len);
		Freemem(tmpbytes);
	end;
end;

{
procedure TContinuousMemoryManager.Delete(p: Pointer);
function IndexOfPointer(p: Pointer): Cardinal;
begin
  Result := (p - Data) div Elementsize;
  end;
begin
  DeleteAt(IndexOfPointer(p));
end;
}

procedure TContinuousMemoryManager.Clear;
begin
	capacity := bsize;
	FreeMem(Data);
	GetMem(Data, capacity * esize);
	used := 0;
end;

function TContinuousMemoryManager.Get(i: cardinal): Pointer;
begin
	Result := @Data[i * esize];
end;

procedure TContinuousMemoryManager.Put(i: cardinal; p: Pointer);
begin
	Move(p^, Data[i * esize], esize);
end;

procedure TContinuousMemoryManager.Insert(i: cardinal);
var
	tmpbytes: PByte;
	len: cardinal;
begin
	len := (used - i) * esize;
	if used = capacity then
	begin
		Inc(capacity, bsize);
		tmpbytes := Data;
		GetMem(Data, capacity * esize);
		Move(tmpbytes[0], Data[0], i * esize);
		Move(tmpbytes[i * esize], Data[(i + 1) * esize], len);
		Freemem(tmpbytes);
	end
	else if len <> 0 then
	begin
		GetMem(tmpbytes, len);
		Move(Data[i * esize], tmpbytes[0], len);
		Move(tmpbytes[0], Data[(i + 1) * esize], len);
		Freemem(tmpbytes, len);
	end;
	Inc(used);
end;

end.
