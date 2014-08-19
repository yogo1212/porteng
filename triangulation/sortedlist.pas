unit SortedList;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

type

	TSortedListEntry = class
		Next: TSortedListEntry;
		function isGreater(r: TSortedListEntry): boolean; virtual; abstract;
	end;

	{ TSortedList }

	TSortedList = class
		ding: TSortedListEntry;
		procedure Insert(v: TSortedListEntry);
		procedure Remove(v: TSortedListEntry);
		constructor Create;
	end;


implementation

{ TSortedList }

procedure TSortedList.Insert(v: TSortedListEntry);
var
	last, it: TSortedListEntry;
begin
	last := nil;
	it := ding;
	while it <> nil do
	begin
		if not v.isGreater(it) then
			break
		else
			last := it;
		it := it.Next;
	end;
	v.Next := it;
	if last <> nil then
		last.Next := v;
end;

procedure TSortedList.Remove(v: TSortedListEntry);
var
	last, it: TSortedListEntry;
begin
	last := nil;
	it := ding;
	while it <> nil do
	begin
		if v = it then
			break
		else
			last := it;
		it := it.Next;
	end;
	last.Next := it.Next;
end;

constructor TSortedList.Create;
begin
	inherited Create;
	ding := nil;
end;

end.
