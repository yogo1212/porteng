unit GameContext;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, PortEngProj, Convenience, dglOpenGL,
	EngineShader, EngineFacilities, EngineTypes, EngineObject, EngineResourceColour,
	EngineResourceTexture, EngineResource, EngineText, EngineTextureLoader,
	EngineResourceLoader, EngineWorld, EnginePort, EngineUnit, GameUnit,
	GamePort, EngineInput;

procedure GameLogicInit;

implementation

type

	{ TSomeStuff }

	TSomeStuff = class(TEngineResourceUser)
		testobj, textobj, testbmpobj: TDrawableObject;
		constructor Create;
		procedure Draw;
	end;

var
	some_stuff: TSomeStuff = nil;

procedure DoRenderWorldObjects;
begin
	some_stuff.Draw;
end;

procedure GameLogicFree;
begin
	if Assigned(some_stuff) then
	begin
		FreeAndNil(some_stuff);
		SetWorldRenderCallback(@DoNothing);
	end;
end;

procedure GameLogicInit;
begin
	if not Assigned(some_stuff) then
	begin
		SetPortListener;
		SetWorldRenderCallback(@DoRenderWorldObjects);

		some_stuff := TSomeStuff.Create;

		AddFreeRoutine(@GameLogicFree);
	end;
end;

{ TSomeStuff }

constructor TSomeStuff.Create;

	procedure SetupTextObj;
	var
		tmpmodel, tmptex, textname, fontname: TResourceName;
		font: TGameFontEngine;
		textrepr: TGameModelRepr;
	begin
		fontname := EngineText.FontName('pornoFont');
		GameFontLoader(fontname, 'cmunss', 72);

		font := TGameFontEngine(GetResource(fontname, grtFont));
		tmptex := font.GetString('MIAAAAAUUUU');

		tmpmodel := ModelName('Oh Hi');
		textname := TextureName('Ohio');

		Create2DRect(tmpmodel, gbBottomMiddle, 10, 2);
		GameTexModLoader(textname, tmpmodel, tmptex);

		textrepr := TGameModel(GetResource(textname, grtModel)).GetRepresentation;
		textobj.Create(textrepr, XYZRotation(0, 0), GamePosition(0, 0, 0, 0, 0, 0));
		;
		textobj.pos.offset += Vec3(0, 0, 1);
	end;

	procedure SetupBMPTestObj;
	var
		tmptex, tmpmod, testbmpname: TResourceName;
		bmprepr: TGameModelRepr;
	begin
		tmptex := TextureName('bmptexture');
		tmpmod := ModelName('cube');
		testbmpname := ModelName('testbmp');

		LoadTexFromBmp('../../pictures/miau.bmp', tmptex, GL_RGBA);
		CreateCube(tmpmod, gbMiddle, 2);
		GameTexModLoader(testbmpname, tmpmod, tmptex);

		bmprepr := TGameModel(GetResource(testbmpname, grtModel)).GetRepresentation;
		testbmpobj.Create(bmprepr, XYZRotation(0, 0), GamePosition(0, 4, 0, 0, 0, 0));
	end;

	procedure SetupTestObj;
	var
		testname: TResourceName;
		testrepr: TGameModelRepr;
	begin
		testname := ModelName('colPyramid');
		ColouredPyramidModelLoader(1, testname);

		testrepr := TGameModel(GetResource(testname, grtModel)).GetRepresentation;
		testobj.Create(testrepr, XYZRotation(0, 0), GamePosition(0, 0, 256, 0, 0, -1));
	end;

	procedure CreateTestBall;
	var
		ballname: TResourceName;
	begin
		ballname := ModelName('colBall');
		ColouredBallModelLoader(1, 3, Vec3(0, 1, 0), ballname);
	end;

begin
	inherited Create;

	SetupTestObj;
	SetupTextObj;
	SetupBMPTestObj;
	CreateTestBall;
end;

procedure TSomeStuff.Draw;
begin
	textobj.Draw;
	testobj.Draw;
	testbmpobj.Draw;
end;

end.
