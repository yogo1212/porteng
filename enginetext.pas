unit EngineText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, diyftgl, EngineTypes, EngineResourceTexture, EngineObject,
  EngineResource, EngineFileUtils;

procedure GameTextInit;
function GetTextureForString(dung: string): TResourceName;
function GetTextObj(texmodname: TResourceName): TGameGraphicsObj;

implementation

var
  sansfile: String;
  initialised: boolean = False;
  font: integer;

procedure GameTextInit;
begin
  if not initialised then
  begin
    initialised := True;
    // Initialise the font-subsystem
    GameFontInit;
    // Initialise this particular font
    font := FontInit(sansfile, 72);
  end;
end;

function GetTextureForString(dung: string): TResourceName;
begin
  Result := GetString(font, dung);
end;

function GetTextObj(texmodname: TResourceName): TGameGraphicsObj;
begin
  Result.Create(texmodname, XYZRotation(0, 0), GamePosition(0, 0, 0, 0, 0, 0));
end;

initialization
  sansfile := EnforceDir('fonts') + 'cmunss.otf';
end.
