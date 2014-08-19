unit GameContext;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineShader, EngineFacilities, EngineTypes, dglOpenGL,
	EngineObject, EngineResourceColor, EngineResourceTexture, EngineResource,
	EngineText, EngineTextureLoader, EngineResourceLoader, EngineStrings, EngineWorld;

procedure RenderWorldObjects; inline;
procedure GameMapInit;

implementation

var
	initialised: boolean = False;
	testobj, textobj, testbmpobj: TGameGraphicsObj;
	testname, textname, testbmpname: TEngineString;

procedure SetupTextObj;
var
	tmpmodel, tmptex: TEngineString;
begin
	tmptex := GetTextureForString('MIAAAAAUUUU');

	tmpmodel := EngineString('Oh Hi');
	textname := EngineString('Ohio');

	Create2DRect(tmpmodel, gbBottomMiddle, 10, 2);
	GameResourceAdd(GameTexModLoader(textname, tmpmodel, tmptex), textname);

	textobj := GetTextObj(textname);
end;

procedure SetupBMPTestObj;
var
	tmptex, tmpmod: TEngineString;
begin
	tmptex := EngineString('bmptexture');
	tmpmod := EngineString('cube');
	testbmpname := EngineString('testbmp');

	GameResourceAdd(LoadTexFromBmp('../../pictures/miau.bmp', tmptex, GL_RGBA), tmptex);
	CreateCube(tmpmod, gbMiddle, 2);
	GameResourceAdd(GameTexModLoader(testbmpname, tmpmod, tmptex), testbmpname);

	testbmpobj.Create(testbmpname, XYZRotation(0, 0), Position(0, 4, 0), Vec3i(0, 0, 0));
end;

procedure SetupTestObj;
begin
	testname := EngineString('colPyramid');
	GameResourceAdd(ColouredPyramidModelLoader(1, testname), testname);

	testobj.Create(testname, XYZRotation(0, 0), Position(0, 0, -256), Vec3i(0, 0, 0));
end;

procedure RenderWorldObjects;
begin
	textobj.Draw;
	testobj.Draw;
	testbmpobj.Draw;
end;

procedure GameMapCleanup;
begin
	if initialised then
	begin
		initialised := False;

		GameResourceUnUnse(testname);
		GameResourceUnUnse(textname);
		GameResourceUnUnse(testbmpname);
	end;
end;

procedure GameMapInit;
begin
	if not initialised then
	begin
		initialised := True;

		GameResourceInit;

		GameWorldInit;

		GameTextureShaderInit;
		GameColorShaderInit;

		SetupTestObj;
		SetupTextObj;
		SetupBMPTestObj;

		AddFreeRoutine(@GameMapCleanup);
	end;
end;

end.
