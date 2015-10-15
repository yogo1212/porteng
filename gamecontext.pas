unit GameContext;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, PortEngProj, EngineShader, EngineFacilities, EngineTypes, dglOpenGL,
	EngineObject, EngineResourceColour, EngineResourceTexture, EngineResource,
	EngineText, EngineTextureLoader, EngineResourceLoader, EngineStrings, EngineWorld,
	EnginePort, EngineUnit, GameUnit, GamePort, EngineInput;

procedure GameLogicInit;

implementation

var
	testobj, textobj, testbmpobj: TGameGraphicsObj;
	testname, textname, testbmpname: TEngineString;
	ballname: TEngineString;

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
	textobj.pos.offset += Vec3(0, 0, 1);
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

	testbmpobj.Create(testbmpname, XYZRotation(0, 0), GamePosition(0, 4, 0, 0, 0, 0));
end;

procedure SetupTestObj;
begin
	testname := EngineString('colPyramid');
	GameResourceAdd(ColouredPyramidModelLoader(1, testname), testname);

	testobj.Create(testname, XYZRotation(0, 0), GamePosition(0, 0, 256, 0, 0, -1));
end;

procedure SetupTestBall;
begin
	ballname := EngineString('colBall');
	GameResourceAdd(ColouredBallModelLoader(1, 3, Vec3(0, 1, 0), ballname), ballname);
end;

procedure DoRenderWorldObjects;
begin
	textobj.Draw;
	testobj.Draw;
	testbmpobj.Draw;
end;

procedure FreeMapResources;
begin
	GameResourceUnUnse(testname);
	GameResourceUnUnse(textname);
	GameResourceUnUnse(testbmpname);
	GameResourceDelete(testname);
	GameResourceDelete(textname);
	GameResourceDelete(testbmpname);
	// TODO do units have longer life-time than this?
	//GameResourceDelete(ballname);
end;

procedure InitMapResources;
begin
	EngineResourceInit;

	GameWorldInit;

	GameTextureShaderInit;
	GameColorShaderInit;

	SetupTestObj;
	SetupTextObj;
	SetupBMPTestObj;
	SetupTestBall;
end;

var
	initialised: boolean = False;

procedure GameLogicFree;
begin
	if initialised then
	begin
		initialised := False;

		FreeMapResources;
	end;
end;

procedure GameLogicInit;
begin
	if not initialised then
	begin
		initialised := True;
		GameInputInit(portengproject.mousekeyboard, portengproject.gamepad);

		GamePortInit;

		EngineUnitInit;

		SetPortListener;
		InitMapResources;
		SetWorldRenderCallback(@DoRenderWorldObjects);


		AddFreeRoutine(@GameLogicFree);
	end;
end;

end.
