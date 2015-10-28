unit EngineResourceLoader;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineStrings, EngineDiskCache, EngineFileUtils;

type
	TResourceBuilderType = (grbFont, grbTexture, grbTextureFile, grbColModel,
		grbIndexedColModel, grbTexModel2, grbTexModel3, grbTexModPair);

	TGameResourceType = (grtTexture, grtModel, grtFont);

	TEngineResourceHandler = class
		function LoadFromCache(const cache: Pointer; const cachesize: cardinal;
			out conttype: TGameResourceType): TObject; virtual; abstract;
		procedure FreeLoaded(loaded: Pointer; const rtype: TGameResourceType);
			virtual; abstract;
	end;

procedure LoadResource(const bType: TResourceBuilderType;
	const Name: TEngineString; out Content: TObject; out conttype: TGameResourceType);
procedure FreeResource(const bType: TResourceBuilderType; Content: TObject;
	const conttype: TGameResourceType);
procedure Store(Name: TEngineString; pnt: Pointer; size: cardinal);
procedure SetHandler(bType: TResourceBuilderType; handler: TEngineResourceHandler);

procedure ExtractVar(var src: Pointer; out dest; const amount: longword);
procedure StoreVar(var dest: Pointer; const src; const amount: longword);

implementation

var
	handlers: array[TResourceBuilderType] of TEngineResourceHandler;
	diskcache: TEngineDiskCache;

procedure LoadResource(const bType: TResourceBuilderType;
	const Name: TEngineString; out Content: TObject; out conttype: TGameResourceType);
var
	tmppnt: Pointer;
	tmpsize: cardinal;
begin
	tmpsize := diskcache.Load(Name, tmppnt);

	Content := handlers[bType].LoadFromCache(tmppnt, tmpsize, conttype);

	Freemem(tmppnt, tmpsize);
end;

procedure FreeResource(const bType: TResourceBuilderType; Content: TObject;
	const conttype: TGameResourceType);
begin
	handlers[bType].FreeLoaded(Content, conttype);

	FreeAndNil(Content);
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

procedure SetHandler(bType: TResourceBuilderType; handler: TEngineResourceHandler);
begin
	handlers[bType] := handler;
end;

initialization
	diskcache := TEngineDiskCache.Create(EnforceDir('cache'));

finalization

end.
