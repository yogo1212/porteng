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
    try
      font := FontInit(sansfile, 72, 72);
    except
      on E: Exception do raise Exception.Create('Couldn''t load font-file "' + sansfile
        + '": ' + E.Message);
    end;
  end;
end;

function GetTextureForString(dung: string): TResourceName;
begin
  Result := GetString(font, dung);
end;

function GetTextObj(texmodname: TResourceName): TGameGraphicsObj;
begin
  Result.Create(texmodname, XYZRotation(0, 0), Position(0, 0, 0), Vec3i(0,0,0));
end;

initialization
  sansfile := EnforceDir('fonts') + 'cmunss.otf';
end.
