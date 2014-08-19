program octreerecursiontest;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
	cthreads, {$ENDIF} {$ENDIF}
	Classes,
	SysUtils,
	engineshader,
	engineresourcecolor,
	enginecamera,
	enginekeyboard,
	enginetext,
	engineresourceloader,
	enginetypes,
	convenience,
	engineobject,
	enginetimer,
	gamegui,
	gamecontext,
	enginediskcache,
	enginemath,
	enginegui,
	enginedebug,
	enginetextureloader,
	engineshaderbuilder,
	engineunit,
	enginestringcomparer,
	engineresourcetexture,
	diyftgl,
	engineworld,
	enginefacilities,
	enginefileutils,
	enginemodelloader,
	enginethread,
	enginedatatypes,
	assimpscene,
	enginestrings,
	enginememory,
	engineport,
	engineresource,
	enginedevice,
	portengproj,
	engineform,
	engineinput,
	engineglcontext,
	dglOpenGL,
	SDL2,
	SDL2_ttf;

const
	world: array[0..4] of TOcEntryPlain = (
		(map: Ord(OcFull) or (Ord(OcFull) shl 2) or (Ord(OcRec) shl 4) or
		(Ord(OcRec) shl 6) or (Ord(OcFull) shl 8) or (Ord(OcFull) shl 10) or
		(Ord(OcRec) shl 12) or (Ord(OcRec) shl 14); offset: 0),
		(map: Ord(OcEmpty) or (Ord(OcEmpty) shl 2) or (Ord(OcFull) shl 4) or
		(Ord(OcFull) shl 6) or (Ord(OcEmpty) shl 8) or (Ord(OcEmpty) shl 10) or
		(Ord(OcFull) shl 12) or (Ord(OcFull) shl 14); offset: 0),
		(map: Ord(OcEmpty) or (Ord(OcEmpty) shl 2) or (Ord(OcFull) shl 4) or
		(Ord(OcFull) shl 6) or (Ord(OcEmpty) shl 8) or (Ord(OcEmpty) shl 10) or
		(Ord(OcFull) shl 12) or (Ord(OcFull) shl 14); offset: 0),
		(map: Ord(OcEmpty) or (Ord(OcEmpty) shl 2) or (Ord(OcFull) shl 4) or
		(Ord(OcFull) shl 6) or (Ord(OcEmpty) shl 8) or (Ord(OcEmpty) shl 10) or
		(Ord(OcFull) shl 12) or (Ord(OcFull) shl 14); offset: 0),
		(map: Ord(OcEmpty) or (Ord(OcEmpty) shl 2) or (Ord(OcFull) shl 4) or
		(Ord(OcFull) shl 6) or (Ord(OcEmpty) shl 8) or (Ord(OcEmpty) shl 10) or
		(Ord(OcFull) shl 12) or (Ord(OcFull) shl 14); offset: 0));

var
  ocentry: TOcEntry = (o: @world[0]);
  position, direction: TVec3;

begin
  position := Vec3(0, 0.4, 0);
  direction := Vec3(0, -1, 0);
  engineworld.CastRayRec(ocentry, position, direction);
end.
