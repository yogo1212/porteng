unit FTGLheader;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dynlibs;

const
	FTGL_RENDER_FRONT = $0001;
	FTGL_RENDER_BACK = $0002;
	FTGL_RENDER_SIDE = $0004;
	FTGL_RENDER_ALL = $ffff;

	FTGL_ALIGN_LEFT = 0;
	FTGL_ALIGN_CENTER = 1;
	FTGL_ALIGN_RIGHT = 2;
	FTGL_ALIGN_JUSTIFY = 3;

var
	ftglCreatePixmapFont: function(filename: PChar): Pointer; cdecl;
	ftglCreateTextureFont: function(filename: PChar): Pointer; cdecl;
	ftglCreateBitmapFont: function(filename: PChar): Pointer; cdecl;
	ftglCreateBufferFont: function(filename: PChar): Pointer; cdecl;
	ftglCreateOutlineFont: function(filename: PChar): Pointer; cdecl;
	ftglSetFontFaceSize: function(font: Pointer; size: longword; res: longword): integer;
	cdecl;
	ftglRenderFont: procedure(font: Pointer; Value: PChar; mode: integer); cdecl;
	ftglDestroyFont: procedure(font: Pointer); cdecl;

implementation

const
{$IFDEF Linux}
	textlibpath = '/usr/local/lib/libftgl.so';
{$ELSE}
  {$IFDEF Windows}
  textlibpath = '';
  {$ENDIF}
{$ENDIF}

var
	textlib: TLibHandle;
   {
initialization
	textlib := LoadLibrary(textlibpath);
	if textlib = 0 then
		raise Exception.Create('no textlib "' + textlibpath + '"');
	// shoot yourself in the foot
	Pointer(ftglCreatePixmapFont) := GetProcedureAddress(textlib, 'ftglCreatePixmapFont');
	Pointer(ftglCreateTextureFont) := GetProcedureAddress(textlib, 'ftglCreateTextureFont');
	Pointer(ftglCreateBufferFont) := GetProcedureAddress(textlib, 'ftglCreateBufferFont');
	Pointer(ftglCreateBitmapFont) := GetProcedureAddress(textlib, 'ftglCreateBitmapFont');
	Pointer(ftglCreateOutlineFont) := GetProcedureAddress(textlib, 'ftglCreateOutlineFont');
	Pointer(ftglSetFontFaceSize) := GetProcedureAddress(textlib, 'ftglSetFontFaceSize');
	Pointer(ftglRenderFont) := GetProcedureAddress(textlib, 'ftglRenderFont');
	Pointer(ftglDestroyFont) := GetProcedureAddress(textlib, 'ftglDestroyFont');

finalization;
	FreeLibrary(textlib);}
end.
