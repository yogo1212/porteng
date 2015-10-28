unit EngineText;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Convenience, dglOpenGL,
	SDL2_ttf, SDL2, EngineTypes, EngineResourceTexture, EngineObject, EngineResourceLoader,
	EngineResource, EngineFileUtils, EngineMemory, EngineFacilities, EngineStrings;

type

	{ TGameFontEngine }

	TGameFontEngine = class(TEngineResourceUser)
		t_font: PTTF_Font;
		constructor Create(const fontname: string; const fontsize: byte);
		destructor Destroy; override;
		function GetString(const thing: string): TEngineString;
	end;

function GameFontLoader(const Name: TResourceName; const fontname: string;
	const fontsize: byte): TResourceBuilderType;

procedure GameTextInit;
{

function GetTextObj(texmodname: TResourceName): TDrawableObject;
begin
  Result.Create(texmodname, XYZRotation(0, 0), GamePosition(0, 0, 0, 0, 0, 0));
end;     }

function FontName(const s: string): TResourceName;

implementation

const
	foregroundColour: TSDL_Color = (r: 0; g: 0; b: 0; a: 255);


var
	initialised: boolean = False;

procedure GameTextFinish;
begin
	TTF_Quit;
end;

procedure GameTextInit;
begin
	if not initialised then
	begin
		initialised := True;
		TTF_Init;

		AddFreeRoutine(@GameTextFinish);
	end;
end;

function FontName(const s: string): TResourceName;
begin
	Result := EngineString('font.' + s);
end;

function GameFontLoader(const Name: TResourceName; const fontname: string;
	const fontsize: byte): TResourceBuilderType;
var
	fontnamelen, bytesize: cardinal;
	Data: PByte;
	iterator: Pointer;
begin
	fontnamelen := Length(fontname);
	bytesize := Sizeof(fontnamelen) + fontnamelen + Sizeof(fontsize);
	GetMem(Data, bytesize);
	iterator := Data;
	StoreVar(iterator, fontnamelen, SizeOf(fontnamelen));
	StoreVar(iterator, fontname[1], fontnamelen);
	StoreVar(iterator, fontsize, SizeOf(fontsize));
	GameResourceAdd(Name, grbFont, Data, bytesize);
	Freemem(Data);
end;

{ TGameFont }

constructor TGameFontEngine.Create(const fontname: string; const fontsize: byte);
var
	filename: string;
begin
	inherited Create;
	prefix := 'font.' + fontname + '.string.';
	filename := EnforceDir('fonts') + fontname + '.otf';

	t_font := TTF_OpenFont(PChar(filename), fontsize);
	if t_font = nil then
		raise Exception.Create('Couldn'' load font (nil): ' + filename);
	try
		TTF_SetFontKerning(t_font, 1);
	except
		TTF_CloseFont(t_font);
		raise Exception.Create('Couldn'' set kerning for ' + filename);
	end;
end;

destructor TGameFontEngine.Destroy;
begin
	TTF_CloseFont(PTTF_Font(t_font));
	inherited Destroy;
end;

function TGameFontEngine.GetString(const thing: string): TEngineString;
var
	surf: PSDL_Surface;
begin
	Result := ResourceName(thing);

	if not TestResourceExistence(Result, grtTexture) then
	begin
		// So, is this method still a catastrophy?

		surf := TTF_RenderUTF8_Blended(t_font, PChar(thing), foregroundColour);

{$IFDEF ENGINEDEBUG}
		if surf^.format^.BitsPerPixel = 32 then
{$ENDIF}
			GameTextureLoader(Result, surf^.w, surf^.h, GL_RGBA, GL_UNSIGNED_BYTE,
				GL_RGBA, surf^.pixels)
{$IFDEF ENGINEDEBUG}
		else
			raise Exception.Create('invalid pixelformat for font-rendering. ouch.');
{$ENDIF}
		;

		SDL_FreeSurface(surf);
	end;
end;

type

	{ TgrbFontHandler }

	TgrbFontHandler = class(TEngineResourceHandler)
		function LoadFromCache(const cache: Pointer; const cachesize: cardinal;
			out conttype: TGameResourceType): TObject; override;
		procedure FreeLoaded(loaded: Pointer; const rtype: TGameResourceType); override;
	end;

{ TgrbFontHandler }

function TgrbFontHandler.LoadFromCache(const cache: Pointer;
	const cachesize: cardinal; out conttype: TGameResourceType): TObject;
var
	fontnamelen: cardinal;
	fontname: string;
	fontsize: byte;
	iterator: Pointer;
begin
	iterator := cache;
	ExtractVar(iterator, fontnamelen, SizeOf(fontnamelen));
  SetLength(fontname, fontnamelen);
	ExtractVar(iterator, fontname[1], fontnamelen);
	ExtractVar(iterator, fontsize, SizeOf(fontsize));
	conttype := grtFont;
	Result := TGameFontEngine.Create(fontname, fontsize);
end;

procedure TgrbFontHandler.FreeLoaded(loaded: Pointer; const rtype: TGameResourceType);
begin
	FreeAndNil(TGameFontEngine(loaded));
end;

initialization
	EnforceDir('fonts');

	SetHandler(grbFont, TgrbFontHandler.Create);

	//TODO font := LoadFont(sansfile, 72);
end.
