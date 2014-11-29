unit EngineResourceLoader;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineStrings, EngineDiskCache, EngineFileUtils;

type
	TResourceBuilderType = (grbTexture, grbTextureFile, grbColModel, grbIndexedColModel,
		grbTexModel2, grbTexModel3, grbTexModPair);

	TGameResourceType = (grtTexture, grtModel);

	TResourceLoader = function(const cache: Pointer; const cachesize: cardinal;
		out conttype: TGameResourceType): TObject;

procedure LoadResource(bType: TResourceBuilderType; Name: TEngineString;
	out Content: TObject; out conttype: TGameResourceType);
procedure SetLoader(bType: TResourceBuilderType; loader: TResourceLoader);
procedure Store(Name: TEngineString; pnt: Pointer; size: cardinal);

procedure ExtractVar(var src: Pointer; out dest; const amount: longword);
procedure StoreVar(var dest: Pointer; const src; const amount: longword);

implementation

var
	loaders: array[TResourceBuilderType] of TResourceLoader;
	diskcache: TEngineDiskCache;

procedure LoadResource(bType: TResourceBuilderType; Name: TEngineString;
	out Content: TObject; out conttype: TGameResourceType);
var
	tmppnt: Pointer;
	tmpsize: cardinal;
begin
	tmpsize := diskcache.Load(Name, tmppnt);

	Content := loaders[bType](tmppnt, tmpsize, conttype);

	Freemem(tmppnt, tmpsize);
end;

procedure Store(Name: TEngineString; pnt: Pointer; size: cardinal);
begin
	diskcache.Store(EngineDiskCacheEntry(Name), pnt, size);
end;

procedure ExtractVar(var src: Pointer; out dest; const amount: longword);
begin
	Move(src^, dest, amount);
	src += amount;
end;

procedure StoreVar(var dest: Pointer; const src; const amount: longword);
begin
	Move(src, dest^, amount);
	dest += amount;
end;

procedure SetLoader(bType: TResourceBuilderType; loader: TResourceLoader);
begin
	loaders[bType] := loader;
end;

initialization
	diskcache := TEngineDiskCache.Create(EnforceDir('cache'));

finalization

end.
