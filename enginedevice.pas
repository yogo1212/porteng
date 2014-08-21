unit EngineDevice;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Convenience, EngineMouseKeyboard, EngineDebug;

type
	TInputNotify = procedure(const id: word; const Value: shortint) of object;

	{ TGameDevice }

	TGameDevice = class(TThread)
	protected
		dbg: boolean;
		Name: string;
		runs: boolean;
		notify: TInputNotify;
		procedure DoTerminate; override;
		procedure Prepare; virtual;
		// Hide those warnings
		procedure DummyCB(const {%H-}x: word; const {%H-}y: shortint);
		procedure Fetch; virtual; abstract;
	public
		procedure Execute; override; final;

		procedure Plug(notif: TInputNotify);
		procedure Unplug;

		function GetName: string;
		function GetNameOfDeviceEvent(id: word): string; virtual; abstract;

    procedure MakeCursorAbs; virtual; abstract;
    procedure MakeCursorRel; virtual; abstract;

		constructor Create(inputname: string);
		destructor Destroy; override;
	end;

	TJoystickEvent = record
		time: longword;
		Value: smallint;
		art, id: byte;
	end;

	{ TGamepadDevice }

	TGamepadDevice = class(TGameDevice)
		// This is for da loop 2 on linux
		tmpevent: TJoystickEvent;

		f: file of TJoystickEvent;
		path: string;
		ff_path: string;
		procedure Prepare; override;
		procedure DoTerminate; override;
		procedure Fetch; override;
		function GetNameOfDeviceEvent(id: word): string; override;

		constructor Create(inputname: string; device: String; ff_device: string = '');
		destructor Destroy; override;
	end;

	{ TMouseKeyboardDevice }

	TMouseKeyboardDevice = class(TGameDevice)
		//       For da loop
		tmpevt: TGameMouseKeyEvent;

		procedure Fetch; override;
		function GetNameOfDeviceEvent(id: word): string; override;

		constructor Create(inputname: string);
		destructor Destroy; override;
	end;

	TUnotProc = procedure(dev: TGameDevice);

procedure SetUnboundNotify(proc: TUNotProc);

implementation

var
	UnboundNotify: TUNotProc = nil;

procedure SetUnboundNotify(proc: TUNotProc);
begin
	UnboundNotify := proc;
end;

{ TMouseKeyboardDevice }

procedure TMouseKeyboardDevice.Fetch;
begin
	tmpevt := Dequeue;
  notify(tmpevt.key, tmpevt.value);
end;

constructor TMouseKeyboardDevice.Create(inputname: string);
begin
	inherited Create(inputname);
	logTrace('Keyboard-input created');
end;

destructor TMouseKeyboardDevice.Destroy;
begin
	inherited Destroy;
end;

function TMouseKeyboardDevice.GetNameOfDeviceEvent(id: word): string;
begin
	Result := 'Keyboard';
end;

{ TGamepadDevice }

procedure TGamepadDevice.Prepare;
begin
	if FileExists(path) then
	begin
		AssignFile(f, path);
		Reset(f);
		logTrace('Started gamepad thread ' + Name);
	end
	else
		logError('"' + path + '" does not exist');
end;

procedure TGamepadDevice.DoTerminate;
begin
	inherited DoTerminate;
	if runs then
	begin
		CloseFile(f);
		runs := False;
		logTrace(Name + ' terminated');
	end;
end;

procedure TGamepadDevice.Fetch;
begin
	Read(f, tmpevent);

	// -2000 < val < 2000 ignorieren?!
	// Google : Deadzone implentieren

	// see page 351

	if (tmpevent.Value < 2000) and (-2000 < tmpevent.Value) then
		tmpevent.Value := 0
	;//	else
		// Das hier macht shit mit negativen Zahlen :
		//  states[evt] := ((val and $8000) shr 8) or ((val and $7F00) shr 8);

    // das nicht, braucht aber länger:
		//tmpevent.Value := tmpevent.Value div 256;

    // so ist es richtig
	Notify(PWord(@tmpevent.Art)^, SarSmallint(tmpevent.Value, 8));
  //  Notify(PWord(@tmpevent.Art)^, tmpevent.Value div 256);
	//writeln(tmpevent.time, ': Art: ', IntToHex(tmpevent.art, 2), ' id: ',
	//   tmpevent.id, ' val: ', IntToHex(Word(tmpevent.Value), 2), ' nId: ',
	//   IntToHex(PWord(@tmpevent.Art)^, 4));
end;

function TGamepadDevice.GetNameOfDeviceEvent(id: word): string;
begin
	case id of
		0: Result := 'asd';
		else
			Result := 'Unknown';
	end;
end;

constructor TGamepadDevice.Create(inputname: string; device: string; ff_device: string);
begin
	inherited Create(inputname);
	path := device;
	ff_path := ff_device;
end;

destructor TGamepadDevice.Destroy;
begin
	runs := False;
	DoTerminate;
	inherited Destroy;
end;

{ TGameDevice }

procedure TGameDevice.DoTerminate;
begin
	inherited DoTerminate;
	if FatalException <> nil then
		logError('Input "' + Name + '" terminated with Excpetion "' +
			FatalException.ToString + '"')
	else if dbg then
		logTrace('Input "' + Name + '" terminated');
end;

procedure TGameDevice.Prepare;
begin
	runs := True;
	FreeOnTerminate := True;
  Sleep(100);
end;

procedure TGameDevice.DummyCB(const x: word; const y: shortint);
begin
	if UnboundNotify <> nil then
		UnboundNotify(Self)
	else
		logTrace('No method for notifying unused devices on input ' + getName + ' !');
end;

procedure TGameDevice.Execute;
begin
  Prepare;
	logTrace('Entered loop of ' + GetName);
	try
		while not Terminated do
			Fetch;
	except
		on E: Exception do
			logError(Name + ' failed with ' + E.Message);
	end;
	EndThread;
end;

procedure TGameDevice.Plug(notif: TInputNotify);
begin
	if notif <> nil then
	begin
		notify := notif;
		logTrace('Set Notifier of ' + Name + ' to ' + PtrToHex(TMinOPtr(notif)));
	end
	else
		logTrace('Tried setting notifier to nil on ' + GetName);
end;

procedure TGameDevice.Unplug;
begin
	notify := @DummyCB;
end;

function TGameDevice.GetName: string;
begin
	Result := Name;
end;

constructor TGameDevice.Create(inputname: string);
begin
	inherited Create(True);
	notify := @DummyCB;
	Name := inputname;
end;

destructor TGameDevice.Destroy;
begin
	if not Terminated then
	begin
		Terminate;
		WaitForThreadTerminate(ThreadId, 0);
	end;
	runs := False;
	inherited Destroy;
end;

end.
