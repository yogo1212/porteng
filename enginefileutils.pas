unit EngineFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function EnforceDir(input: String): String;
//function GetGameConfigDir: String;

implementation

var gameconfigdir: String;

function GetGameConfigDir: String;
begin
  Result := gameconfigdir;
end;

//Prepends configdir
function EnforceDir(input: String): String;
begin
  if input[Length(input)] <> DirectorySeparator then
    input := input + DirectorySeparator;
  if input[1] <> DirectorySeparator then
    input := DirectorySeparator + input;
  input := gameconfigdir + input;
  if not DirectoryExists(input) then
    if not CreateDir(input) then
      raise Exception.Create('Couldn''t create ' + input);
  Result := input;
end;

initialization
  gameconfigdir := GetAppConfigDir(false);
  if gameconfigdir[Length(gameconfigdir)] = DirectorySeparator then
    Delete(gameconfigdir, Length(gameconfigdir), 1);
  if not DirectoryExists(gameconfigdir) then
    if not CreateDir(gameconfigdir) then
      raise Exception.Create('Couldn''t create ' + gameconfigdir);

end.

