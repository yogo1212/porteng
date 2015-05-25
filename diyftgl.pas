unit diyftgl;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils,
	EngineFacilities, EngineResourceTexture, dglOpenGL,
	Convenience, EngineStrings, EngineResource, EngineResourceLoader,
	SDL2_ttf, SDL2;

// Load a font by file name
function FontInit(filename: string; Size: byte): byte;
// Get bitmap for a string
function GetString(fontid: byte; thing: string): TEngineString;
// Init the whole font thingy
procedure GameFontInit;

implementation

const
	dispStringXSalt: TXorHash = Pred(High(TXorHash));
	dispStringDSalt: TDeczHash = Pred(High(TDeczHash));
	maxfontindex = 3;
	foregroundColour: TSDL_Color = (r: 0; g: 0; b: 0; a: 255);

var
	initialised: boolean = False;
	fonts: array[0..maxfontindex] of PTTF_Font;

procedure GameFontFree;
var
	zhlr: byte;
begin
	for zhlr := 0 to maxfontindex do
		if fonts[zhlr] <> nil then
			TTF_CloseFont(fonts[zhlr]);
	TTF_Quit;
end;

procedure GameFontInit;
var
	zhlr: byte;
begin
	TTF_Init;
	if not initialised then
	begin
		for zhlr := 0 to maxfontindex do
		begin
			fonts[zhlr] := nil;
		end;

		AddFreeRoutine(@GameFontFree);
	end;
end;

function FontInit(filename: string; Size: byte): byte;
var
	zhlr: byte;
begin
	//look for free slot in fonts
	zhlr := 0;
	while zhlr <= maxfontindex do
	begin
		if fonts[zhlr] = nil then
			break;
		Inc(zhlr);
	end;
	if zhlr <= maxfontindex then
	begin
		fonts[zhlr] := TTF_OpenFont(PChar(filename), Size);
		if fonts[zhlr] = nil then
			raise Exception.Create('Couldn'' load font: ' + filename);
		try
			TTF_SetFontKerning(fonts[zhlr], 1);
			Result := zhlr;
		except
			FreeAndNil(fonts[zhlr]);
			raise Exception.Create('Couldn'' set kerning for ' + filename);
		end;
	end
	else
		raise Exception.Create('Cant hold more fonts..');
end;

function GetString(fontid: byte; thing: string): TEngineString;
var
	surf: PSDL_Surface;
begin
	Result := EngineString(thing, dispStringXSalt, dispStringDSalt);

	if not GameResourceHas(Result, grtTexture) then
	begin
		// So, is this method still a catastrophy?

		surf := TTF_RenderUTF8_Blended(fonts[fontid], PChar(thing), foregroundColour);

{$IFDEF ENGINEDEBUG}
		if surf^.format^.BitsPerPixel = 32 then
{$ENDIF}
			GameResourceAdd(GameTextureLoader(Result, surf^.w, surf^.h,
				GL_RGBA, GL_UNSIGNED_BYTE, GL_RGBA, surf^.pixels), Result)
{$IFDEF ENGINEDEBUG}
		else
			raise Exception.Create('invalid pixelformat for font-rendering. ouch.');
{$ENDIF}
		;

		SDL_FreeSurface(surf);
	end;
end;

end.
