unit EngineResource;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Convenience, EngineDataTypes, EngineFacilities, EngineStrings,
	EngineMemory, EngineResourceLoader;

type

	EDuplicateResource = Exception;
	EInvalidResourceName = Exception;
	ENoSuchResource = Exception;

	{ TGameResource }

	TGameResource = record
		Name: TEngineString;
		resourceType: TGameResourceType;
		builderType: TResourceBuilderType;
		Content: TObject;
		usecnt: longword;
	end;

	TResourceName = TEngineString;

	{ TEngineResourceUser }

	TEngineResourceUser = class
	protected
		_used_resources: TContinuousMemoryManager;
		prefix: string;
		function ResourceName(const s: string): TResourceName;
		function GetResource(const rpath: TResourceName;
			const rtype: TGameResourceType): Pointer;
		function TestResourceExistence(const rpath: TResourceName;
			const rtype: TGameResourceType): boolean;
		procedure ReleaseResource(const rpath: TResourceName);
		constructor Create;
	public
		destructor Destroy; override;
	end;

	PGameResource = ^TGameResource;

	_TGameResourceComparer = specialize TGameComparer<TGameResource>;

	{ TGameResourceComparer }

	TGameResourceComparer = class(_TGameResourceComparer)
		function compare(o1, o2: TGameResource): shortint; override;
		function greater(o1, o2: TGameResource): boolean; override;
		function equal(o1, o2: TGameResource): boolean; override;
		function less(o1, o2: TGameResource): boolean; override;
	end;

	{ TPGameResourceComparer }

	TPGameResourceComparer = class(_TPointerComparer)
		function compare(o1, o2: Pointer): shortint; override;
		function greater(o1, o2: Pointer): boolean; override;
		function equal(o1, o2: Pointer): boolean; override;
		function less(o1, o2: Pointer): boolean; override;
	end;

	DoesntExistException = class(Exception);


procedure EngineResourceInit;

procedure GameResourceAdd(rPath: TEngineString; btype: TResourceBuilderType;
	Data: Pointer; datalen: cardinal);
procedure GameResourceFreeUnused;
procedure GameResourceDelete(rPath: TEngineString);

implementation

function GameResourceUse(rPath: TEngineString;
	rtype: TGameResourceType): Pointer; forward;
function GameResourceHas(rPath: TEngineString; rtype: TGameResourceType): boolean;
	forward;
procedure GameResourceUnUnse(const rPath: TEngineString); forward;

// This will be passed as the comparer for the TGameTree in TGameResource.Content
var
	resources: TKeyBasedMemoryManager;
	initialised: boolean = False;

procedure EngineResourceDestroy;
begin
	if initialised then
	begin
		initialised := False;

		GameResourceFreeUnused;
		// TODO Free all remaining resources
	end;
end;

procedure EngineResourceInit;
begin
	if not initialised then
	begin
		initialised := True;
		resources := TSortedMemoryManager.Create(TPGameResourceComparer.Create,
			TContinuousMemoryManager.Create(SizeOf(TGameResource), 50));
		AddFreeRoutine(@EngineResourceDestroy);
	end;
end;

function GameResourceUse(rPath: TEngineString; rtype: TGameResourceType): Pointer;
var
	tmpres: TGameResource;
begin
	tmpres.Name := rPath;
	if resources.Fetch(@tmpres) then
	begin
		if tmpres.usecnt = 0 then
		begin
			LoadResource(tmpres.builderType, tmpres.Name, tmpres.Content,
				tmpres.resourceType);
		end;
		Inc(tmpres.usecnt);
		resources.Store(@tmpres);
		if not (rtype = tmpres.resourceType) then
			raise Exception.Create(GetRealValue(rPath) + ' is not the right type of resource!');
	end
	else
		raise DoesntExistException.Create(GetRealValue(rPath) + ' doesn''t exist');
	Result := tmpres.Content;
end;

procedure GameResourceAdd(rPath: TEngineString; btype: TResourceBuilderType;
	Data: Pointer; datalen: cardinal);
var
	tmpres: TGameResource;
begin
	tmpres.usecnt := 0;
	tmpres.builderType := btype;
	tmpres.Name := rPath;
	tmpres.Content := nil;

	resources.Store(@tmpres);

	Store(rPath, Data, datalen);
end;

procedure GameResourceFreeUnused;
begin
	//TODO
end;

function GameResourceHas(rPath: TEngineString; rtype: TGameResourceType): boolean;
var
	tmpres: TGameResource;
begin
	tmpres.Name := rPath;
	Result := (resources.Fetch(@tmpres)) and (rtype = tmpres.resourceType);
end;

procedure GameResourceUnUnse(const rPath: TEngineString);
var
	tmpres: TGameResource;
begin
	tmpres.Name := rPath;
	if resources.Fetch(@tmpres) then
	begin
		if tmpres.usecnt > 0 then
		begin
			Dec(tmpres.usecnt);
			if tmpres.usecnt = 0 then
				FreeResource(tmpres.builderType, tmpres.Content,
					tmpres.resourceType);
			resources.Store(@tmpres);
		end
		else
			// TODO don't crash game
			raise Exception.Create(GetRealValue(rPath) + ' is already unused');
	end;
end;

procedure GameResourceDelete(rPath: TEngineString);
var
	tmpres: TGameResource;
begin
	tmpres.Name := rPath;
	if resources.Fetch(@tmpres) then
	begin
		if tmpres.usecnt <> 0 then
			raise Exception.Create(GetRealValue(rPath) +
				' can''t be freed because it is still in use');
		FreeAndNil(tmpres.Content);
		resources.Delete(@tmpres);
	end;
end;

{ TPGameResourceComparer }

function TPGameResourceComparer.compare(o1, o2: Pointer): shortint;
begin
	Result := CompareEngineStrings(PGameResource(o1)^.Name, PGameResource(o2)^.Name);
end;

function TPGameResourceComparer.greater(o1, o2: Pointer): boolean;
begin
	Result := GreaterEngineStrings(PGameResource(o1)^.Name, PGameResource(o2)^.Name);
end;

function TPGameResourceComparer.equal(o1, o2: Pointer): boolean;
begin
	Result := EqualEngineStrings(PGameResource(o1)^.Name, PGameResource(o2)^.Name);
end;

function TPGameResourceComparer.less(o1, o2: Pointer): boolean;
begin
	Result := LessEngineStrings(PGameResource(o1)^.Name, PGameResource(o2)^.Name);
end;

{ TGameResourceComparer }

function TGameResourceComparer.compare(o1, o2: TGameResource): shortint;
begin
	Result := CompareEngineStrings(o1.Name, o2.Name);
end;

function TGameResourceComparer.greater(o1, o2: TGameResource): boolean;
begin
	Result := GreaterEngineStrings(o1.Name, o2.Name);
end;

function TGameResourceComparer.equal(o1, o2: TGameResource): boolean;
begin
	Result := EqualEngineStrings(o1.Name, o2.Name);
end;

function TGameResourceComparer.less(o1, o2: TGameResource): boolean;
begin
	Result := LessEngineStrings(o1.Name, o2.Name);
end;

{ TEngineResourceUser }

function TEngineResourceUser.ResourceName(const s: string): TResourceName;
begin
	Result := EngineString(prefix + s);
end;

function TEngineResourceUser.GetResource(const rpath: TResourceName;
	const rtype: TGameResourceType): Pointer;
begin
	Result := GameResourceUse(rpath, rtype);
	_used_resources.Append(@rpath);
end;

function TEngineResourceUser.TestResourceExistence(const rpath: TResourceName;
	const rtype: TGameResourceType): boolean;
begin
	Result := GameResourceHas(rpath, rtype);
end;

procedure TEngineResourceUser.ReleaseResource(const rpath: TResourceName);
var
	zhlr: cardinal;
begin
	//TODO enumerator for operations that don't need to be that fast?
	zhlr := _used_resources.Count;
	while zhlr > 0 do
	begin
		zhlr -= 1;
		if EqualEngineStrings(rpath, PEngineString(_used_resources.Get(zhlr))^) then
		begin
			GameResourceUnUnse(rpath);
			_used_resources.DeleteAt(zhlr);
			exit;
		end;
	end;
	raise Exception.Create('resource + »' + GetRealValue(rpath) +
		'« can''t be released because it isn''t loaded');
end;

constructor TEngineResourceUser.Create;
begin
	_used_resources := TContinuousMemoryManager.Create(SizeOf(TResourceName), 10);
	prefix := '';
end;

destructor TEngineResourceUser.Destroy;
var
	zhlr: cardinal;
begin
	//TODO enumerator for operations that don't need to be that fast?
	zhlr := _used_resources.Count;
	while zhlr > 0 do
	begin
		GameResourceUnUnse(PEngineString(_used_resources.Get(zhlr))^);
		_used_resources.DeleteAt(zhlr);
	end;
	FreeAndNil(_used_resources);
	inherited Destroy;
end;

end.
