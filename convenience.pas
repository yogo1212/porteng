unit Convenience;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

type

	TObjProc = procedure() of object; register;

	//Method in Object-Pointer
	TMinOPtr = record
		method, obj: Pointer;
	end;

	TXorHash = Cardinal;
	TDeczHash = Cardinal;

	PXorHash = ^TXorHash;
	PDeczHash = ^TDeczHash;

	PNativeUint = ^NativeUint;

	PMinOPtr = ^TMinOPtr;

function BinToHexStr(stuff: Pointer; Count: word): string;
function BinToHexStrRev(stuff: Pointer; Count: word): string;

function PtrToHex(pntr: Pointer): string;
function PtrToHex(input: TMinOPtr): string; overload;
function ObjPtrToHex(var input): string;

function HashXor(input: Pointer; len: cardinal; Salt: TXorHash): TXorHash;
function HashDecz(input: Pointer; len: cardinal; Salt: TDeczHash): TDeczHash;
function HashXor(input: Pointer; len: cardinal): TXorHash;
  overload; inline;
function HashDecz(input: Pointer; len: cardinal): TDeczHash;
  overload; inline;
function HashXor(input: string): TXorHash; overload; inline;
function HashDecz(input: string): TDeczHash; overload; inline;
function HashXor(input: string; Salt: TXorHash): TXorHash; overload; inline;
function HashDecz(input: string; Salt: TDeczHash): TDeczHash; overload; inline;

function NextPowerOfTwo(dim: Integer): Integer; inline;
function PowerOfTwo(exp: Byte): Integer; inline;

function PreInc(var i: Integer; b: Integer): Integer;
function PostInc(var i: Integer; b: Integer): Integer;

procedure DoNothing; register;

// ok... trying to make sure the inlined code is being optimised...
// not sure how inlining is implemented
{$OPTIMIZATION ON}
procedure genericLookup(second: Boolean; a,b: TObjProc); inline;
procedure genericLookup(really: Boolean; a: TObjProc); inline; overload;
{$OPTIMIZATION DEFAULT}

implementation

{$OPTIMIZATION ON}
// Is this crap?
procedure genericLookup(second: Boolean; a, b: TObjProc);
// cant make this a global variable because not thread-safe. :-(
var genericBinaryLookupTable: Array[Boolean] of TObjProc;
begin
  genericBinaryLookupTable[False] := a;
  genericBinaryLookupTable[True] := b;
  genericBinaryLookupTable[second]();
end;

procedure genericLookup(really: Boolean; a: TObjProc);
var genericSingularLookupTable: Array[Boolean] of TObjProc;
begin
  PPointer(@genericSingularLookupTable[False])^ := Pointer(@DoNothing);
  genericSingularLookupTable[True] := a;
  genericSingularLookupTable[really]();
end;
{$OPTIMIZATION DEFAULT}

function NextPowerOfTwo(dim: Integer): Integer;
begin
  Result := 1;
  while (Result < dim) do
    Result := Result shl 1;
end;

function PowerOfTwo(exp: Byte): Integer;
begin
  Result := 1;
  Result := Result shl exp;
end;

function PreInc(var i: Integer; b: Integer): Integer;
begin
  Inc(i, b);
  Result := i;
end;

function PostInc(var i: Integer; b: Integer): Integer;
begin
  Result := i;
  Inc(i, b);
end;

procedure DoNothing;
begin

end;

function PtrToHex(pntr: Pointer): string;
var
	ptr: Pointer;
begin
	ptr := @pntr;
{$IFDEF ENDIAN_BIG}
	Result := BinToHexStr(@ptr, SizeOf((@ptr)));
{$ELSE}
	Result := BinToHexStrRev(Pointer(ptr + SizeOf(@ptr) - 1), SizeOf((@ptr)));
{$ENDIF}
end;

function ObjPtrToHex(var input): string;
begin
	Result := PtrToHex(@input);
end;

function HashXor(input: Pointer; len: cardinal; Salt: TXorHash): TXorHash;
var
	cnt, remainder: cardinal;
	tmp: TXorHash;
begin
	Result := Salt;
	remainder := len mod SizeOf(TXorHash);
	tmp := 0;
	cnt := len - remainder;
	while cnt <> 0 do
	begin
		Result := Result xor PXorHash(input)^;
		Inc(input, SizeOf(TXorHash));
		Dec(cnt, SizeOf(TXorHash));
	end;
	Move(input^, tmp, remainder);
	Result := Result xor tmp;
end;

function HashDecz(input: Pointer; len: cardinal; Salt: TDeczHash): TDeczHash;
var
	cnt, remainder: cardinal;
	tmp: TDeczHash;
begin
	Result := Salt;
	remainder := len mod SizeOf(TDeczHash);
	tmp := 0;
	cnt := len - remainder;
	while cnt <> 0 do
	begin
		Result := Result - PDeczHash(input)^;
		Inc(input, SizeOf(TDeczHash));
		Dec(cnt, SizeOf(TXorHash));
	end;
	Move(input^, tmp, remainder);
	Result := Result - tmp;
end;

function HashXor(input: Pointer; len: cardinal): TXorHash;
begin
  Result := HashXor(input, len, 0);
end;

function HashDecz(input: Pointer; len: cardinal): TDeczHash;
begin
  Result := HashDecz(input, len, 0);
end;

function HashXor(input: string): TXorHash;
begin
	Result := HashXor(@input[1], Length(input), 0);
end;

function HashDecz(input: string): TDeczHash;
begin
	Result := HashDecz(@input[1], Length(input), 0);
end;

function HashXor(input: string; Salt: TXorHash): TXorHash;
begin
	Result := HashXor(@input[1], Length(input), Salt);
end;

function HashDecz(input: string; Salt: TDeczHash): TDeczHash;
begin
	Result := HashDecz(@input[1], Length(input), Salt);
end;

function PtrToHex(input: TMinOPtr): string;
begin
	Result := PtrToHex(input.method) + ' on ' + PtrToHex(input.obj);
end;

const
	Values: array[0..15] of char =
		('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A',
		'B', 'C', 'D', 'E', 'F');

function BinToHexStr(stuff: Pointer; Count: word): string;
var
	ptr: PChar;
begin
	SetLength(Result, Count shl 1);
	ptr := @Result[1];
	while Count <> 0 do
	begin
		ptr^ := values[byte(stuff^) shr 4];
		Inc(ptr);
		ptr^ := values[byte(stuff^) and $F];
		Inc(ptr);
		Inc(stuff);
		Dec(Count);
	end;
end;

function BinToHexStrRev(stuff: Pointer; Count: word): string;
var
	ptr: PChar;
begin
	SetLength(Result, Count shl 1);
	ptr := @Result[1];
	while Count <> 0 do
	begin
		ptr^ := values[byte(stuff^) shr 4];
		Inc(ptr);
		ptr^ := values[byte(stuff^) and $F];
		Inc(ptr);
		Dec(stuff);
		Dec(Count);
	end;
end;

end.
