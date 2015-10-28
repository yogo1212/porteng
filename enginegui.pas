unit EngineGUI;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineTypes, EngineResource, dglOpenGL;

type

	// Every element draws itself from 0,0 (Left, Top) to 1,1 (Right, Bottom)
	// The shader will do the scaling and stuff

	{ TEngineUIElement }

	TEngineUIElement = class(TEngineResourceUser)
	protected
		procedure SetBoundry(AValue: TVec4);
	public
		boundry: TVec4;
		matrix: TGLMatrixf4;
		constructor Create(nboundry: TVec4);
		procedure Draw;
		procedure DrawSelf; virtual; abstract;
		property bounds: TVec4 read boundry write SetBoundry;
	end;

implementation

{ TEngineUIElement }

procedure TEngineUIElement.SetBoundry(AValue: TVec4);
begin
	boundry := AValue;
end;

constructor TEngineUIElement.Create(nboundry: TVec4);
begin
	inherited Create;
	boundry := nboundry;
end;

procedure TEngineUIElement.Draw;
begin

end;

end.
