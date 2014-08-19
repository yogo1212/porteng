unit EngineStrings;

{$mode objfpc}{$H+}

{$undef ENGINEDEBUG}
{$T+}
interface

uses
	Classes, SysUtils, Convenience
{$ifdef ENGINEDEBUG}
  , EngineMemory

{$endif}
  ;

type

	{ TEngineString }

	TEngineString = record
		xHash: TXorHash; dHash: TDeczHash;
	end;

	PEngineString = ^TEngineString;

{$ifdef ENGINEDEBUG}
  { TEngineStringComparer }

  TEngineStringComparer = class(_TPointerComparer)
    function compare(o1, o2: Pointer): shortint; override;
    function greater(o1, o2: Pointer): Boolean; override;
    function equal(o1, o2: Pointer): Boolean; override;
    function less(o1, o2: Pointer): Boolean; override;
  end;

var enginestringcomparer: TEngineStringComparer;
{$endif}

operator = (a, b: TEngineString): boolean;
operator < (a, b: TEngineString): boolean;
operator > (a, b: TEngineString): boolean;

function CompareEngineStrings(what, withwhat: TEngineString): smallint;
function CompareEngineStringsByRef(what, withwhat: PEngineString): smallint;

function GreaterEngineStrings(what, withwhat: TEngineString): Boolean;
function LessEngineStrings(what, withwhat: TEngineString): Boolean;
function EqualEngineStrings(what, withwhat: TEngineString): Boolean;

function EngineString(input: string): TEngineString; overload;
function EngineString(input: string; xSalt: TXorHash; dSalt: TDeczHash): TEngineString;

function GetRealValue(id: TEngineString): String;

implementation

{$IFDEF ENGINEDEBUG}
type
	TStringTableEntry = record
		id: TEngineString;
		Value: string;
	end;

	TStringTable = class
	private
		Data: TSortedMemoryManager;
		function GetString(i: TEngineString): string;
		procedure SetString(i: TEngineString; s: string);
		constructor Create;
	public
		property Items[i: TEngineString]: string read GetString write SetString;
	end;

var
	stringtable: TStringTable;

{ TEngineStringComparer }

function TEngineStringComparer.compare(o1, o2: Pointer): shortint;
begin
  Result := CompareEngineStringsByRef(o1, o2);
end;

function TEngineStringComparer.greater(o1, o2: Pointer): Boolean;
begin
  Result := GreaterEngineStrings(PEngineString(o1)^, PEngineString(o2)^);
end;

function TEngineStringComparer.equal(o1, o2: Pointer): Boolean;
begin
  Result := EqualEngineStrings(PEngineString(o1)^, PEngineString(o2)^);
end;

function TEngineStringComparer.less(o1, o2: Pointer): Boolean;
begin
  Result := LessEngineStrings(PEngineString(o1)^, PEngineString(o2)^);
end;

constructor TStringTable.Create;
begin
  Data := TSortedMemoryManager.Create(enginestringcomparer,
    TContinuousMemoryManager.Create(SizeOf(TStringTableEntry), 10));
end;

function TStringTable.GetString(i: TEngineString): string;
var entry: TStringTableEntry;
begin
  entry.id := i;
  entry.Value := '!NoString!';
  Data.Fetch(@entry);
  Result := entry.Value;
end;

procedure TStringTable.SetString(i: TEngineString; s: string);
var entry: TStringTableEntry;
begin
  entry.id := i;
  entry.Value:=s;
  Data.Store(@entry);
end;

function GetRealValue(id: TEngineString): String;
begin
  Result := stringtable.Items[id];
end;
{$ELSE}

function GetRealValue(id: TEngineString): String;
begin
  Result := BinToHexStr(@id, SizeOf(id));
  // Or exception?
end;

{$ENDIF}
// TODO maßschneidern

operator = (a, b: TEngineString): boolean;
begin
	Result := CompareEngineStrings(a, b) = 0;
end;

operator < (a, b: TEngineString): boolean;
begin
	Result := CompareEngineStrings(a, b) < 0;
end;

operator > (a, b: TEngineString): boolean;
begin
	Result := CompareEngineStrings(a, b) > 0;
end;

{$if sizeof(TEngineString) = sizeof(nativeuint)}
function CompareEngineStrings(what, withwhat: TEngineString): smallint;
var tmp: NativeUint;
begin
  tmp := PNativeUint(@what)^ - PNativeUint(@withwhat)^;
  if tmp = 0 then
    Result := 0
	else if tmp > PNativeUint(@what)^ then
		Result := -1
	else // if tmp < PNativeUint(@what)^ then
		Result := 1;
end;

function GreaterEngineStrings(what, withwhat: TEngineString): Boolean;
begin
  Result := PNativeUint(@what)^ > PNativeUint(@withwhat)^;
end;

function LessEngineStrings(what, withwhat: TEngineString): Boolean;
begin
  Result := PNativeUint(@what)^ < PNativeUint(@withwhat)^;
end;

function EqualEngineStrings(what, withwhat: TEngineString): Boolean;
begin
  Result := PNativeUint(@what)^ = PNativeUint(@withwhat)^;
end;
{$else}
function CompareEngineStrings(what, withwhat: TEngineString): smallint;
var
	tmp1: TXorHash;
  tmp2: TDeczHash;
begin
	tmp1 := what.xHash - withwhat.xHash;
	if tmp1 = 0 then
	begin
		Result := 0;
		tmp2 := what.dHash - withwhat.dHash;
		if tmp2 = 0 then
			exit
		else if tmp2 > what.dHash then
			Result := -1
		else // if tmp2 < what.dHash then
			Result := 1;
	end	else if tmp1 > what.xHash then
		Result := -1
	else // if tmp1 < what.xHash then
		Result := 1;
end;

function GreaterEngineStrings(what, withwhat: TEngineString): Boolean;
begin
  Result := what.xHash > withwhat.xHash;
end;

function LessEngineStrings(what, withwhat: TEngineString): Boolean;
begin
  Result := what.xHash < withwhat.xHash;
end;

function EqualEngineStrings(what, withwhat: TEngineString): Boolean;
begin
  Result := what.xHash = withwhat.xHash;
end;
{$ifend}

function EngineString(input: string): TEngineString;
begin
	Result.xHash := HashXor(input);
	Result.dHash := HashDecz(input);
{$IFDEF ENGINEDEBUG}
	stringtable.Items[Result] := input;
{$ENDIF}
end;

function EngineString(input: string; xSalt: TXorHash; dSalt: TDeczHash): TEngineString;
begin
	Result.xHash := HashXor(input, xSalt);
	Result.dHash := HashDecz(input, dSalt);
{$IFDEF ENGINEDEBUG}
	stringtable.Items[Result] := input;
{$ENDIF}
end;

function CompareEngineStringsByRef(what, withwhat: PEngineString): smallint;
begin
  Result := CompareEngineStrings(what^, withwhat^);
end;

{$IFDEF ENGINEDEBUG}
initialization
  enginestringcomparer := TEngineStringComparer.Create;
  stringtable := TStringTable.Create;
{$ENDIF}
end.
