unit EngineStringComparer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EngineDataTypes, EngineStrings, EngineMemory;

type
  _TEngineStringComparer = specialize TGameComparer<TEngineString>;

  { TPEngineStringComparer }

  TPEngineStringComparer = class(_TPointerComparer)
    function compare(o1, o2: Pointer): shortint; override;
    function greater(o1, o2: Pointer): Boolean; override;
    function equal(o1, o2: Pointer): Boolean; override;
    function less(o1, o2: Pointer): Boolean; override;
  end;

  { TEngineStringComparer }

  TEngineStringComparer = class(_TEngineStringComparer)
    function compare(o1, o2: TEngineString): shortint; override;
    function greater(o1, o2: TEngineString): Boolean; override;
    function equal(o1, o2: TEngineString): Boolean; override;
    function less(o1, o2: TEngineString): Boolean; override;
  end;

var
  Comparer: TEngineStringComparer;
  RefComparer: TPEngineStringComparer;

implementation

{ TPEngineStringComparer }

function TPEngineStringComparer.compare(o1, o2: Pointer): shortint;
begin
  Result := CompareEngineStrings(PEngineString(o1)^, PEngineString(o2)^);
end;

function TPEngineStringComparer.greater(o1, o2: Pointer): Boolean;
begin
  Result := GreaterEngineStrings(PEngineString(o1)^, PEngineString(o2)^);
end;

function TPEngineStringComparer.equal(o1, o2: Pointer): Boolean;
begin
  Result := EqualEngineStrings(PEngineString(o1)^, PEngineString(o2)^);
end;

function TPEngineStringComparer.less(o1, o2: Pointer): Boolean;
begin
  Result := LessEngineStrings(PEngineString(o1)^, PEngineString(o2)^);
end;

{ TEngineStringComparer }

function TEngineStringComparer.compare(o1, o2: TEngineString): shortint;
begin
  Result:= CompareEngineStrings(o1, o2);
end;

function TEngineStringComparer.greater(o1, o2: TEngineString): Boolean;
begin
  Result:= GreaterEngineStrings(o1, o2);
end;

function TEngineStringComparer.equal(o1, o2: TEngineString): Boolean;
begin
  Result:= EqualEngineStrings(o1, o2);
end;

function TEngineStringComparer.less(o1, o2: TEngineString): Boolean;
begin
  Result:= LessEngineStrings(o1, o2);
end;


initialization
  Comparer := TEngineStringComparer.Create;
  RefComparer := TPEngineStringComparer.Create;
finalization
  FreeAndNil(Comparer);
  FreeAndNil(RefComparer);
end.

