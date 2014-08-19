unit EngineDataTypes;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

type

  { TGameComparer }
  // with 2.7.1 the first 4 can be abstract

  generic TGameComparer<T> = class
    function compare(o1, o2: T): shortint; virtual;
    function greater(o1, o2: T): Boolean; virtual;
    function equal(o1, o2: T): Boolean; virtual;
    function less(o1, o2: T): Boolean; virtual;
    function greaterOrEqual(o1,o2: T): Boolean; inline;
    function lessOrEqual(o1,o2: T): Boolean; inline;
  end;

	{ TGameListElement }

	TGameListElement = class
		Data: Pointer;
		Left, Right: TGameListElement;
		constructor Create(d: Pointer; l: TGameListElement; r: TGameListElement = nil);
		destructor Destroy; override;
	end;

  TGameTreeFlaggedPointer = record
    data: Pointer;
    flag: Boolean;
  end;

	{ TGameTreeElement }

	TGameTreeElement = class(TGameListElement)
  	type
  	  TEleComparer = specialize TGameComparer<Pointer>;
    // TCompareFunc = function(o1, o2: Pointer): shortint of object;

		EGameTreeDuplicate = Exception;
	var
		Count: word;

		constructor Create(d: Pointer; l: TGameTreeElement; r: TGameTreeElement);
		procedure insert(thing: Pointer; comparer: TEleComparer); register;
    function RemoveRightMost: Pointer;
    function RemoveLeftMost: Pointer;
		function search(thing: Pointer; comparer: TEleComparer): Pointer; register;
		function Remove(thing: Pointer; comparer: TEleComparer): TGameTreeFlaggedPointer; register;
		destructor Destroy; override;
	end;

	{ TSimpleBase }

  TSimpleBase = class
  public
	  value: Pointer;
	  next: TSimpleBase;
    constructor Create(val: Pointer; nex: TSimpleBase = nil);
  end;

	{ TSimpleList }

  TSimpleList = class(TSimpleBase)
		procedure Append(val: Pointer);
	end;

	{ TSimpleStack }

 TSimpleStack = class(TSimpleBase)
		procedure Push(val: Pointer);
    function Pop: Boolean;
	end;

	{ TSimpleQueue }

  TSimpleQueue = class(TSimpleBase)
    procedure Enqueue(val: Pointer);
    function Dequeue: Boolean;
  end;

	{ TGameEnumerator }

  generic TGameEnumerator<T> = class(TObject)
  protected
    FList: TSimpleList;
    function GetCurrent: T;
  public
    constructor Create(AList: TSimpleList);
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

	{ TGameList }

	generic	TGameList<T> = class
	private
		procedure append(thing: T); register;
	public
  type
		TCompareFunc = function(o1, o2: T): shortint;
	var
		First: TGameListElement;
		comparer: TCompareFunc;
		procedure insert(thing: T);
		constructor Create(compare: TCompareFunc = nil);
		destructor Destroy; override;
	end;

	{ TGameTree }

	generic TGameTree<T> = class
	type
		//TCompareFunc = function(o1, o2: T): shortint;
    TTreeComparer = specialize TGameComparer<T>;
    TGameTreeEnum = specialize TGameEnumerator<T>;
	var
  private
		Root: TGameTreeElement;
		comparer: TTreeComparer;
  public
		procedure insert(thing: T);
    function IsEmpty: Boolean;
		function search(var thing: T): boolean;
    function Remove(thing: T): T;
    function InOrderTraversal: TSimpleList;
    function GetEnumerator: TGameTreeEnum;
		constructor Create(compare: TTreeComparer);
		destructor Destroy; override;
	end;

implementation

{ TComparer }

function TGameComparer.compare(o1, o2: T): shortint;
begin
  Result := 0;
  if @o1 < @o2 then
    Result := -1
  else if @o1 > @o2 then
    Result := 1;
end;

function TGameComparer.greater(o1, o2: T): Boolean;
begin
  Result := @o1 > @o2;
end;

function TGameComparer.equal(o1, o2: T): Boolean;
begin
  Result := @o1 = @o2;
end;

function TGameComparer.less(o1, o2: T): Boolean;
begin
  Result := @o1 < @o2;
end;

function TGameComparer.greaterOrEqual(o1, o2: T): Boolean;
begin
  Result := not less(o1,o2);
end;

function TGameComparer.lessOrEqual(o1, o2: T): Boolean;
begin
  Result := not greater(o1,o2);
end;

{ TSimpleBase }

constructor TSimpleBase.Create(val: Pointer; nex: TSimpleBase);
begin
  inherited Create;
  value := val;
  next := nex;
end;

{ TSimpleList }

procedure TSimpleList.Append(val: Pointer);
var iterator: TSimpleBase;
begin
  iterator := Self;
  while iterator.next <> nil do
    iterator := iterator.next;
  iterator.next := TSimpleList.Create(val);
end;

{ TSimpleQueue }

procedure TSimpleQueue.Enqueue(val: Pointer);
var iterator: TSimpleBase;
begin
  iterator := Self;
  while iterator.next <> nil do
	  iterator := iterator.next;
	iterator.next := TSimpleQueue.Create(val);
end;

function TSimpleQueue.Dequeue: Boolean;
var tmpbase: TSimpleBase;
begin
  Result := next <> nil;
  if Result then
  begin
    tmpbase := next;
    value := next.value;
    next := next.next;
    FreeAndNil(tmpbase);
	end;
end;

{ TSimpleStack }

procedure TSimpleStack.Push(val: Pointer);
var tmpstack: TSimpleBase;
begin
  tmpstack := next;
  next := TSimpleStack.Create(value, tmpstack);
  value := val;
end;

function TSimpleStack.Pop: Boolean;
var tmpstack: TSimpleBase;
begin
  Result := next <> nil;
  if Result then
  begin
    value := next.value;
    tmpstack := next;
    next := next.next;
  	FreeAndNil(tmpstack);
	end;
end;

{ TGameEnumerator }

function TGameEnumerator.GetCurrent: T;
begin
  Result := T(FList.value);
end;

constructor TGameEnumerator.Create(AList: TSimpleList);
begin
  FList := AList;
end;

function TGameEnumerator.MoveNext: Boolean;
var tmplist: TSimpleList;
begin
  tmplist := FList;
  Result := FList.next <> nil;
  FList:= FList.next as TSimpleList;
  if Result then
  begin
    FreeAndNil(tmplist);
	end;
end;

{ TGameTree }

procedure TGameTree.insert(thing: T);
begin
	if Root = nil then
		Root := TGameTreeElement.Create(thing, nil, nil)
	else
		Root.insert(thing, Comparer);
end;

function TGameTree.search(var thing: T): boolean;
var
	Resp: Pointer;
begin
	Result := True;
	if Root = nil then
		Result := False
	else
	begin
		// The thing will be freed
		Resp := Root.search(thing, Comparer);
		if Resp = nil then
			Result := False
		else
			thing := T(Resp);
	end;
end;

function TGameTree.Remove(thing: T): T;
var
	Resp: TGameTreeFlaggedPointer;
begin
	Result := nil;
	if Root <> nil then
	begin
		Resp := Root.remove(thing, comparer);
    if Resp.flag then
      FreeAndNil(Root);
		if Resp.data <> nil then
			Result := T(Resp.data);
	end;
end;

function TGameTree.InOrderTraversal: TSimpleList;
var current: TGameListElement; stack: TSimpleStack;
begin
  Result := TSimpleList.Create(nil);
  stack := TSimpleStack.Create(nil);
  current := Root;

  while current <> nil do
  begin
    while current.Left <> nil do
    begin
		  stack.Push(current);
      current := current.Left;
		end;
		Result.Append(TGameTreeElement(current).Data);
    if current.Right <> nil then
      current := current.Right
    else
    begin
      if stack.Pop then
			  current := TGameListElement(stack.value)
      else
        break;
		end;
	end;
end;

function TGameTree.GetEnumerator: TGameTreeEnum;
begin
  Result := TGameTreeEnum.Create(InOrderTraversal);
end;

function TGameTree.IsEmpty: Boolean;
begin
  Result := Root = nil;
end;

constructor TGameTree.Create(compare: TTreeComparer);
begin
	inherited Create;
	comparer := compare;
	Root := nil;
end;

destructor TGameTree.Destroy;
begin
	FreeAndNil(Root);
	inherited Destroy;
end;

{ TGameTreeElement }

constructor TGameTreeElement.Create(d: Pointer; l: TGameTreeElement;
	r: TGameTreeElement);
begin
	inherited Create(d, l, r);
	Count := 1;
	if l <> nil then
		Count += l.Count;
	if r <> nil then
		Count += r.Count;
end;

procedure TGameTreeElement.insert(thing: Pointer; comparer: TEleComparer);
	register;
var
	cmp: shortint;
begin
	cmp := comparer.compare(thing, Data);
	Count += 1;
	if cmp < 0 then
	begin
		if Left <> nil then
			TGameTreeElement(Left).Insert(thing, comparer)
		else
			Left := TGameTreeElement.Create(thing, nil, nil);
	end
	else if cmp > 0 then
	begin
		if Right <> nil then
			TGameTreeElement(Right).Insert(thing, comparer)
		else
			Right := TGameTreeElement.Create(thing, nil, nil);
	end
	else if cmp = 0 then
	begin
		raise EGameTreeDuplicate.Create('');
		Count -= 1;
	end;
end;

function TGameTreeElement.search(thing: Pointer; comparer: TEleComparer): Pointer;
	register;
var
	cmp: shortint;
begin
	// Compare the thing to own Value:
	cmp := comparer.compare(thing, Data);
	if cmp < 0 then
	begin
		if Left <> nil then
			Result := TGameTreeElement(Left).Search(thing, comparer)
		else
			Result := nil;
	end
	else if cmp > 0 then
	begin
		if Right <> nil then
			Result := TGameTreeElement(Right).Search(thing, comparer)
		else
			Result := nil;
	end
	// Match!
	else if cmp = 0 then
	begin
		// If the search thing is not us, free it
		if thing <> Data then
			FreeAndNil(thing);
		Result := Data;
	end;
end;

function TGameTreeElement.RemoveRightMost: Pointer;
var tmp: TGameListElement;
begin
  if Right.Right <> nil then
    Result := TGameTreeElement(Right).RemoveRightMost
  else
  begin
    tmp := Right;
    Result := tmp.Data;
    if Right.Left <> nil then
    begin
      Result := Right.Data;
      Right := Right.Left;
      tmp.Left := nil;
    end
    else
      Right := nil;
    FreeAndNil(tmp);
  end;
end;

function TGameTreeElement.RemoveLeftMost: Pointer;
var tmp: TGameListElement;
begin
  if Left.Left <> nil then
    Result := TGameTreeElement(Left).RemoveLeftMost
  else
  begin
    tmp := Left;
    Result := tmp.Data;
    if Left.Right <> nil then
    begin
      Result := Left.Data;
      Left := Left.Right;
      tmp.Right := nil;
    end
    else
      Left := nil;
    FreeAndNil(tmp);
  end;
end;

function TGameTreeElement.Remove(thing: Pointer; comparer: TEleComparer):
  TGameTreeFlaggedPointer; register;
var
	cmp: shortint;
  tmp: TGameListElement;
begin
	// Compare the thing to own Value:
	cmp := comparer.compare(thing, Data);
	Result.data := nil;
  Result.flag := false;
	if cmp < 0 then
	begin
		if Left <> nil then
    begin
      Result := TGameTreeElement(Left).Remove(thing, comparer);
      if Result.flag then
      begin
        Result.flag := False;
        FreeAndNil(left);
      end;
    end;
	end
	else if cmp > 0 then
	begin
		if Right <> nil then
    begin
			Result := TGameTreeElement(Right).Remove(thing, comparer);
      if Result.flag then
      begin
        Result.flag := False;
        FreeAndNil(Right);
      end;
    end;
	end
	// Match!
	else if cmp = 0 then
	begin
		// If the search thing is not us, free it
		if thing <> Data then
			FreeAndNil(thing);
		Result.data := Data;
    if Left <> nil then
    begin
      if Left.Right <> nil then
      	Data := TGameTreeElement(Left).RemoveRightMost
      else
      begin
        tmp := Left;
        Data := tmp.Data;
        Left := tmp.left;
        tmp.Left := nil;
        FreeAndNil(tmp);
      end;
    end
    else if Right <> nil then
    begin
      if Right.Left <> nil then
        Data := TGameTreeElement(Right).RemoveLeftMost
      else
      begin
        tmp := Right;
        Data := tmp.Data;
        Right := tmp.Right;
        tmp.Right := nil;
        FreeAndNil(tmp);
      end;
    end
    // Tell the previous caller on the stack, he should remove our reference
    else
      Result.flag := True;
	end;
end;

destructor TGameTreeElement.Destroy;
begin
	if Left <> nil then
		FreeAndNil(Left);
	if Right <> nil then
		FreeAndNil(Right);
	inherited Destroy;
end;

{ TGameListElement }

constructor TGameListElement.Create(d: Pointer; l: TGameListElement;
	r: TGameListElement = nil);
begin
	inherited Create;
	Data := d;
	Left := l;
	Right := r;
end;

destructor TGameListElement.Destroy;
begin
	FreeAndNil(Data);
	inherited Destroy;
end;

{ TGameList }

procedure TGameList.append(thing: T); register;
var
	iterator: TGameListElement;
begin
	if First = nil then
		First := TGameListElement.Create(@thing, nil)
	else
	begin
		iterator := First;
		while iterator.Right <> nil do
			iterator := iterator.Right;
		iterator.Right := TGameListElement.Create(@thing, iterator);
	end;
end;

procedure TGameList.insert(thing: T);
var
	iterator: TGameListElement;
begin
	if comparer <> nil then
	begin
		if First = nil then
			First := TGameListElement.Create(@thing, nil)
		else if comparer(thing, T(First.Data^)) > 0 then
		begin
			First := TGameListElement.Create(@thing, nil, First);
			First.Right.Left := First;
		end
		else
		begin
			iterator := First;
			while iterator.Right <> nil do
			begin
				if comparer(T(iterator.Right.Data^), thing) > 0 then
					break;
				iterator := iterator.Right;
			end;
			if iterator.Right <> nil then
			begin
				iterator.Right.Left := TGameListElement.Create(@thing, iterator, iterator.Right);
				iterator.Right := iterator.Right.Left;
			end
			else
				iterator.Right := TGameListElement.Create(@thing, iterator);
		end;
	end
	else
		append(thing);
end;

constructor TGameList.Create(compare: TCompareFunc = nil);
begin
	First := nil;
	comparer := compare;
end;

destructor TGameList.Destroy;
var
	tmp: TGameListElement;
begin
	while First <> nil do
	begin
		tmp := First.Right;
		FreeAndNil(First);
		First := tmp;
	end;
	inherited Destroy;
end;

end.
