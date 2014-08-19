unit dynstructs;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

type

	{ IBinTreeElement }

	IBinTreeElement =  interface
		function GetKey: longword;
	end;

	TBinTree = class
		Value: IBinTreeElement;
		left, right: TBinTree;
		constructor Create(val: IBinTreeElement);
		constructor Create(val: IBinTreeElement; l, r: TBinTree);
		function Search(val: longword): IBinTreeElement;
		procedure Insert(val: IBinTreeElement);
		function Remove(val: longword): IBinTreeElement;
	private
		function BRemove(val: longword; res: IBinTreeElement): boolean;
		function RemoveRightMost: IBinTreeElement;
		function RemoveLeftMost: IBinTreeElement;
	end;

implementation

{ TBinTree }

constructor TBinTree.Create(val: IBinTreeElement);
begin
	Create(val, nil, nil);
end;

constructor TBinTree.Create(val: IBinTreeElement; l, r: TBinTree);
begin
	inherited Create;
	Value := val;
	left := l;
	right := r;
end;

function TBinTree.Search(val: longword): IBinTreeElement;
var
	tmpval: longword;
begin
	tmpval := Value.GetKey;
	if tmpval = val then
		Result := Value
	else if tmpval < val then
		Result := right.Search(val)
	else if tmpval > val then
		Result := left.Search(val)
	else
		Result := nil;
end;

procedure TBinTree.Insert(val: IBinTreeElement);
var
	tmpval: longword;
begin
	if Value <> nil then
	begin
		tmpval := Value.GetKey;
		if tmpval = val.GetKey then
			Value := val
		else if tmpval < val.GetKey then
			right.Insert(val)
		else if tmpval > val.GetKey then
			left.Insert(val);
	end
	else
		Value := val;
end;

function TBinTree.Remove(val: longword): IBinTreeElement;
begin
	BRemove(val, Result);
end;

function TBinTree.BRemove(val: longword; res: IBinTreeElement): boolean;
var
	tmpval: longword;
	tmptree: TBinTree;
begin
	tmpval := Value.GetKey;
	Result := False;
	if tmpval = val then
	begin
		res := Value;
		if left <> nil then
			Value := left.RemoveRightMost
		else if right <> nil then
			Value := right.RemoveLeftMost
		else
		begin
			Value := nil;
			Result := True;
		end;
	end
	else if tmpval < val then
	begin
		if right.BRemove(val, res) then
			FreeAndNil(right);
	end
	else if tmpval > val then
	begin
		if left.BRemove(val, res) then
			FreeAndNil(left);
	end;
end;

function TBinTree.RemoveRightMost: IBinTreeElement;
var
	tmptree: TBinTree;
begin
	if right <> nil then
		Result := right.RemoveRightMost
	else
	begin
		Result := Value;
		if left <> nil then
		begin
			tmptree := left;
			Value := tmptree.Value;
			right := tmptree.right;
			left := tmptree.left;
			FreeAndNil(tmptree);
		end
		else
			Value := nil;
	end;
end;

function TBinTree.RemoveLeftMost: IBinTreeElement;
var
	tmptree: TBinTree;
begin
	if left <> nil then
		Result := left.RemoveRightMost
	else
	begin
		Result := Value;
		if right <> nil then
		begin
			tmptree := right;
			Value := tmptree.Value;
			right := tmptree.right;
			left := tmptree.left;
			FreeAndNil(tmptree);
		end
		else
			Value := nil;
	end;
end;

end.
