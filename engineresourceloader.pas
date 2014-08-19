unit EngineResourceLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EngineStrings, EngineDiskCache, EngineFileUtils;

type
  TResourceBuilderType = (grbTexture, grbTextureFile, grbColModel, grbTexModel2,
    grbTexModel3, grbTexModPair);

  TGameResourceType = (grtTexture, grtModel);

  TResourceLoader = function (cache: Pointer; cachesize: Cardinal;
    out conttype: TGameResourceType): TObject;

procedure LoadResource(bType: TResourceBuilderType; name: TEngineString;
  out Content: TObject; out conttype: TGameResourceType);
procedure SetLoader(bType: TResourceBuilderType; loader: TResourceLoader);
procedure Store(name: TEngineString; pnt: Pointer; size: Cardinal);

implementation

var
  loaders: array[TResourceBuilderType] of TResourceLoader;
  diskcache: TEngineDiskCache;

procedure LoadResource(bType: TResourceBuilderType; name: TEngineString;
  out Content: TObject; out conttype: TGameResourceType);
var tmppnt: Pointer; tmpsize: Cardinal;
begin
  tmpsize := diskcache.Load(name, tmppnt);

  Content := loaders[bType](tmppnt, tmpsize, conttype);

  Freemem(tmppnt, tmpsize);
end;

procedure Store(name: TEngineString; pnt: Pointer; size: Cardinal);
begin
  diskcache.Store(EngineDiskCacheEntry(name), pnt, size);
end;

procedure SetLoader(bType: TResourceBuilderType; loader: TResourceLoader);
begin
  loaders[bType] := loader;
end;

initialization
  diskcache := TEngineDiskCache.Create(EnforceDir('cache'));
finalization

end.

