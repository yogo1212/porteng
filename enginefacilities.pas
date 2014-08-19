unit EngineFacilities;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

procedure AddFreeRoutine(ProcPnt: TProcedure);
procedure GameClose;

implementation

type

	{ TGameFacilitiesStack }

	TGameFacilitiesStack = class
		Next: TGameFacilitiesStack;
		Method: TProcedure;
		constructor Create(meth: TProcedure);
		destructor Destroy; override;
	end;

var
	stack: TGameFacilitiesStack = nil;

procedure AddFreeRoutine(ProcPnt: TProcedure);
var
	tmp: TGameFacilitiesStack;
begin
	if stack = nil then
		stack := TGameFacilitiesStack.Create(ProcPnt)
	else
	begin
		tmp := stack;
		while tmp.Next <> nil do
			tmp := tmp.Next;
		tmp.Next := TGameFacilitiesStack.Create(ProcPnt);
	end;
end;

procedure GameClose;
begin
	FreeAndNil(stack);
end;

{ TGameFacilitiesStack }

constructor TGameFacilitiesStack.Create(meth: TProcedure);
begin
	inherited Create;
	Method := meth;
	Next := nil;
end;

destructor TGameFacilitiesStack.Destroy;
begin
	if Next <> nil then
		FreeAndNil(Next);
	Method();
	inherited Destroy;
end;


end.
