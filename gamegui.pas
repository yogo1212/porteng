unit GameGUI;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineGUI, EngineResourceTexture, EngineStrings, Convenience,
	EngineTextureLoader, dglOpenGL, EngineResource, EngineResourceLoader, EngineFacilities,
	EngineObject, EngineShader, EngineTypes;

type

	{ TGameAbilityBar }

	TGameAbilityBar = class(TEngineUIElement)
		asdfff: TGameModelRepr;
		procedure DrawSelf; override;
		constructor Create(nboundry: TVec4);
		destructor Destroy; override;
	end;

	{ TGameChatBox }

	TGameChatBox = class(TEngineUIElement)
		procedure DrawSelf; override;
		constructor Create(nboundry: TVec4);
	end;

	{ TGameInterfaceMaster }

	TGameInterfaceMaster = class(TEngineUIElement)
		chatbox: TGameChatBox;
		abilitybar: TGameAbilityBar;
		constructor Create;
		procedure DrawSelf; override;
	end;


procedure InitGuiTextures;

implementation

const
	abilitybarfile = '../../pictures/patheticAbilityBar.bmp';
	GUIxSalt: TXorHash = Low(TXorHash) + 1;
	GUIdSalt: TDeczHash = High(TDeczHash) div 4 + 1;

var
	initialised: boolean = False;
	interfacemaster: TGameInterfaceMaster;
	texmodname: TEngineString;

procedure DestroyGui;
begin
	if initialised then
	begin
		initialised := False;
	end;
end;

procedure InitGuiTextures;
var
	texname, modname: TEngineString;
begin
	if not initialised then
	begin
		initialised := True;

		EngineResourceInit;

		GameTextureGUIShaderInit;
		GameColourGUIShaderInit;

		modname := EngineString('abilitybarmod', GUIxSalt, GUIdSalt);
		texname := EngineString('abilitybartex', GUIxSalt, GUIdSalt);
		texmodname := EngineString('abilitybartexmod', GUIxSalt, GUIdSalt);

		GameResourceAdd(LoadTexFromBmp(abilitybarfile, texname, GL_RGBA), texname);

		Create2DGUIRect(modname, gbBottomMiddle, 0, -1, 1, 0.2);

		GameResourceAdd(GameTexModLoader(texmodname, modname, texname), texmodname);

		AddFreeRoutine(@DestroyGui);
	end;
end;

{ TGameChatBox }

procedure TGameChatBox.DrawSelf;
begin

end;

constructor TGameChatBox.Create(nboundry: TVec4);
begin
	inherited Create(nboundry);
end;

{ TGameAbilityBar }

procedure TGameAbilityBar.DrawSelf;
begin
	//getprogram(spTextureGUI).SetSize(boundry.X, boundry.Y);
	PrepareShader(@asdfff);
	DrawModel(@asdfff);
end;

constructor TGameAbilityBar.Create(nboundry: TVec4);
begin
	inherited Create(nboundry);
	asdfff := TGameTexModPair(GameResourceUse(texmodname, grtModel)).GetRepresentation;
	asdfff.programtype := spTextureGUI;
end;

destructor TGameAbilityBar.Destroy;
begin
	GameResourceUnUnse(texmodname);
	inherited Destroy;
end;

{ TGameInterfaceMaster }

constructor TGameInterfaceMaster.Create;
begin
	inherited Create(Vec4(0, 0, 1, 1));
	abilitybar := TGameAbilityBar.Create(Vec4(0.7, 0.25, 0.2, 0.5));
	chatbox := TGameChatBox.Create(Vec4(0.6, 0.05, 0.3, 0.4));
end;

procedure TGameInterfaceMaster.DrawSelf;
begin
	abilitybar.DrawSelf;
end;

end.
