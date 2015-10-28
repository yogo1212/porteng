unit GameGUI;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineGUI, EngineResourceTexture, EngineStrings, Convenience,
	EngineTextureLoader, dglOpenGL, EngineResource, EngineResourceLoader, EngineFacilities,
	EngineObject, EngineShader, EngineTypes, GameUnit;

type

	{ TGameHealthBox }

	TGameHealthBox = class(TEngineUIElement)
		u: TGameUnit;
		box: TGameModelRepr;
		procedure DrawSelf; override;
		constructor Create(nu: TGameUnit; nboundry: TVec4);
	end;

	{ TGameAbilityBar }

	TGameAbilityBar = class(TEngineUIElement)
		asdfff: TGameModelRepr;
		texmodname: TEngineString;
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
		healthbox: TGameHealthBox;
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

procedure DestroyGui;
begin
	if initialised then
	begin
		initialised := False;
	end;
end;

procedure InitGuiTextures;
begin
	if not initialised then
	begin
		initialised := True;

		EngineResourceInit;

		GameTextureGUIShaderInit;
		GameColourGUIShaderInit;

		AddFreeRoutine(@DestroyGui);
	end;
end;

{ TGameHealthBox }

procedure TGameHealthBox.DrawSelf;
begin
	//getprogram(spTextureGUI).SetSize(boundry.X, boundry.Y);
	PrepareShader(@box);
	DrawModel(@box);
end;

constructor TGameHealthBox.Create(nu: TGameUnit; nboundry: TVec4);
begin
	inherited Create(nboundry);
	// TODO wrong texture
	box := TGameTexModPair(GetResource(ResourceName('abilitybartexmod'), grtModel)).GetRepresentation;
	box.programtype := spTextureGUI;
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
	PrepareShader(@asdfff);
	DrawModel(@asdfff);
end;

constructor TGameAbilityBar.Create(nboundry: TVec4);
var
	texname, modname: TEngineString;
begin
	inherited Create(nboundry);

	modname := ModelName('abilitybarmod');
	texname := TextureName('abilitybartex');
	texmodname := ModelName('abilitybartexmod');

	LoadTexFromBmp(abilitybarfile, texname, GL_RGBA);

	Create2DGUIRect(modname, gbBottomMiddle, 0, -1, 1, 0.2);

	GameTexModLoader(texmodname, modname, texname);

	asdfff := TGameModel(GetResource(texmodname, grtModel)).GetRepresentation;
	asdfff.programtype := spTextureGUI;
end;

destructor TGameAbilityBar.Destroy;
begin
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
