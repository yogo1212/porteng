unit EngineResource;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineDataTypes, EngineFacilities, EngineStrings,
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

  PGameResource = ^TGameResource;

	_TGameResourceComparer = specialize TGameComparer<TGameResource>;

  { TGameResourceComparer }

  TGameResourceComparer = class(_TGameResourceComparer)
    function compare(o1, o2: TGameResource): shortint; override;
    function greater(o1, o2: TGameResource): Boolean; override;
    function equal(o1, o2: TGameResource): Boolean; override;
    function less(o1, o2: TGameResource): Boolean; override;
  end;

  { TPGameResourceComparer }

  TPGameResourceComparer = class(_TPointerComparer)
    function compare(o1, o2: Pointer): shortint; override;
    function greater(o1, o2: Pointer): Boolean; override;
    function equal(o1, o2: Pointer): Boolean; override;
    function less(o1, o2: Pointer): Boolean; override;
  end;


procedure EngineResourceInit;

procedure GameResourceAdd(btype : TResourceBuilderType;
  rPath: TEngineString);
procedure GameResourceFreeUnused;
function GameResourceUse(rPath: TEngineString; rtype: TGameResourceType): Pointer;
function GameResourceHas(rPath: TEngineString; rtype: TGameResourceType): Boolean;
procedure GameResourceUnUnse(const rPath: TEngineString);
procedure GameResourceDelete(rPath: TEngineString);

implementation

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
var tmpres: TGameResource;
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
      raise Exception.Create(GetRealValue(rPath)+ ' is not the right type of resource!');
  end
  else
    raise Exception.Create(GetRealValue(rPath) + ' doesn''t exist');
	Result := tmpres.Content;
end;

procedure GameResourceAdd(btype : TResourceBuilderType; rPath: TEngineString);
var tmpres: TGameResource;
begin
	tmpres.usecnt := 0;
  tmpres.builderType := btype;
  tmpres.Name := rPath;
  tmpres.Content := nil;

  resources.Store(@tmpres);
end;

procedure GameResourceFreeUnused;
begin
  //TODO
end;

function GameResourceHas(rPath: TEngineString; rtype: TGameResourceType): Boolean;
var tmpres: TGameResource;
begin
  tmpres.Name := rPath;
  Result := (resources.Fetch(@tmpres)) and (rtype = tmpres.resourceType);
end;

procedure GameResourceUnUnse(const rPath: TEngineString);
var tmpres: TGameResource;
begin
  tmpres.Name := rPath;
  if resources.Fetch(@tmpres) then
  begin
    if tmpres.usecnt > 0 then
    begin
   	  Dec(tmpres.usecnt);
      if tmpres.usecnt = 0 then
        FreeAndNil(tmpres.Content);
      resources.Store(@tmpres);
    end
    else
      // TODO don't crash game
      raise Exception.Create(GetRealValue(rPath) + ' is already unused');
  end;
end;

procedure GameResourceDelete(rPath: TEngineString);
var tmpres: TGameResource;
begin
  tmpres.Name := rPath;
  if resources.Fetch(@tmpres) then
  begin
    if tmpres.usecnt <> 0 then
      raise Exception.Create(GetRealValue(rPath)
        + ' can''t be freed because it is still in use');
    FreeAndNil(tmpres.Content);
    resources.Delete(@tmpres);
  end;
end;

{ TPGameResourceComparer }

function TPGameResourceComparer.compare(o1, o2: Pointer): shortint;
begin
  Result:= CompareEngineStrings(PGameResource(o1)^.Name, PGameResource(o2)^.Name);
end;

function TPGameResourceComparer.greater(o1, o2: Pointer): Boolean;
begin
  Result:= GreaterEngineStrings(PGameResource(o1)^.Name, PGameResource(o2)^.Name);
end;

function TPGameResourceComparer.equal(o1, o2: Pointer): Boolean;
begin
  Result:= EqualEngineStrings(PGameResource(o1)^.Name, PGameResource(o2)^.Name);
end;

function TPGameResourceComparer.less(o1, o2: Pointer): Boolean;
begin
  Result:= LessEngineStrings(PGameResource(o1)^.Name, PGameResource(o2)^.Name);
end;

{ TGameResourceComparer }

function TGameResourceComparer.compare(o1, o2: TGameResource): shortint;
begin
  Result := CompareEngineStrings(o1.Name, o2.Name);
end;

function TGameResourceComparer.greater(o1, o2: TGameResource): Boolean;
begin
  Result := GreaterEngineStrings(o1.Name, o2.Name);
end;

function TGameResourceComparer.equal(o1, o2: TGameResource): Boolean;
begin
  Result := EqualEngineStrings(o1.Name, o2.Name);
end;

function TGameResourceComparer.less(o1, o2: TGameResource): Boolean;
begin
  Result := LessEngineStrings(o1.Name, o2.Name);
end;

end.
