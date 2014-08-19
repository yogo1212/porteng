unit EngineDiskCache;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineStrings, Convenience, EngineMemory,
  EngineStringComparer;

{$undef ENGINEDEBUG}
type

	{ TEngineDiskCacheEntry }

	TEngineDiskCacheEntry = record
		// for now, this must be the first element for direct string-comparisons to work
		id: TEngineString;
		// path: array[0..4*SizeOf(NativeUint)] of Char;
	end;

	PEngineDiskCacheEntry = ^TEngineDiskCacheEntry;

	{ TEngineDiskCache }

	TEngineDiskCache = class
	private
		dirpath: string;
		entries: TSortedMemoryManager;
		procedure FreeDataOf(entry: TEngineDiskCacheEntry);
		function BuildCacheEntryFilePath(entry: TEngineDiskCacheEntry): string;
		procedure SaveDataFor(entry: TEngineDiskCacheEntry; Data: Pointer; Size: cardinal);
	public
		constructor Create(directorypath: string);
		procedure Store(new: TEngineDiskCacheEntry; Data: Pointer; Size: cardinal);
		procedure Delete(rPath: TEngineString);
		function Load(rPath: TEngineString; out dest: Pointer): cardinal;
{$ifdef ENGINEDEBUG}
		function printentries: string;
		function GetEntries: TSortedMemoryManager;
{$endif}
	end;

function EngineDiskCacheEntry(id: TEngineString): TEngineDiskCacheEntry;

implementation

function EngineDiskCacheEntry(id: TEngineString): TEngineDiskCacheEntry;
begin
	Result.id := id;
end;

{ TEngineDiskCache }

{$ifdef ENGINEDEBUG}

function TEngineDiskCache.printentries: string;
begin
	Result := BinToHexStr(entries.Items[0], entries.Count * SizeOf(TEngineDiskCacheEntry));
end;

function TEngineDiskCache.GetEntries: TSortedMemoryManager;
begin
	Result := entries;
end;

{$endif}

function TEngineDiskCache.BuildCacheEntryFilePath(entry: TEngineDiskCacheEntry): string;
begin
	Result := dirpath + BinToHexStr(@entry.id, SizeOf(entry.id));
end;

constructor TEngineDiskCache.Create(directorypath: string);
begin
	dirpath := directorypath;
	entries := TSortedMemoryManager.Create(EngineStringComparer.RefComparer,
    TContinuousMemoryManager.Create(SizeOf(TEngineDiskCacheEntry), 50));
end;

procedure TEngineDiskCache.FreeDataOf(entry: TEngineDiskCacheEntry);
begin
	DeleteFile(BuildCacheEntryFilePath(entry));
end;

procedure TEngineDiskCache.SaveDataFor(entry: TEngineDiskCacheEntry;
	Data: Pointer; Size: cardinal);
var
	tmpfile: TFileStream;
begin
	tmpfile := TFileStream.Create(BuildCacheEntryFilePath(entry),
		fmOpenReadWrite or fmCreate);
	tmpfile.WriteBuffer(Data^, Size);
 	tmpfile.Free;
end;

procedure TEngineDiskCache.Store(new: TEngineDiskCacheEntry; Data: Pointer;
	Size: cardinal);
begin
	if entries.Fetch(@new) then
		FreeDataOf(new);
	entries.Store(@new);
	SaveDataFor(new, Data, Size);
end;

procedure TEngineDiskCache.Delete(rPath: TEngineString);
var
	new: TEngineDiskCacheEntry;
  pnt: PEngineDiskCacheEntry;
begin
	new := EngineDiskCacheEntry(rPath);
  pnt := @new;
  entries.Delete(pnt);
	if pnt <> nil then
		FreeDataOf(new);
end;

function TEngineDiskCache.Load(rPath: TEngineString; out dest: Pointer): cardinal;
var
	new: TEngineDiskCacheEntry;
	tmpfile: TFileStream;
begin
	new := EngineDiskCacheEntry(rPath);
	if entries.Fetch(@new) then
	begin
		tmpfile := TFileStream.Create(BuildCacheEntryFilePath(new), fmOpenRead);

		// In the future, load the resource here; instead of returning the file-content
		Result := tmpfile.Size;
		GetMem(dest, Result);
		tmpfile.ReadBuffer(dest^, Result);

		FreeAndNil(tmpfile);
	end
	else
	begin
		dest := nil;
		Result := 0;
	end;
end;

end.
