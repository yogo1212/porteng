unit EngineInput;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, EngineDevice, EngineDebug, EngineMouseKeyboard, EngineFacilities,
	Convenience, SDL2;

type
	TEngineInputElement = (ieNone = 0,
		// these are states:
		ieMoveRelX, ieMoveRelY, ieMoveRelZ,
		ieTurnRelXZ, ieTurnRelY, ieLookRelXZ, ieLookRelY,
		ieCursorX, ieCursorY, ieZoom,
		// these are events (special because not necessarily tied to in-game states):
		ieActionLeft, ieActionMiddle, ieActionRight, ieActionX1, ieActionX2,
		ieJump, ieAlignCam,
		ieTurnRelTickXZ, ieTurnRelTickY, ieLookRelTickXZ, ieLookRelTickY,
		ieCursorTickX, ieCursorTickY,
		ieZoomTickX, ieZoomTickY,
		// The following can be used to express atomar GIEs (states);
		ieMoveRelXp, ieMoveRelXn, ieMoveRelYp, ieMoveRelYn, ieMoveRelZp, ieMoveRelZn,
		ieCursorXp, ieCursorXn, ieCursorYp, ieCursorYn, ieLookRelXZp, ieLookRelXZn,
		ieLookRelYp, ieLookRelYn, ieTurnRelXZp, ieTurnRelXZn, ieTurnRelYp,
		ieTurnRelYn, ieZoomp, ieZoomn,
		// these are modifiers
		ieLockTurnLookXYZ, ieMakeCursorMotionLook, ieMakeCursorMotionTurnAndLock
		);

	TEngineLogicRelatedElement = Low(TEngineInputElement)..Pred(ieMoveRelXp);
	TEngineInputRelatedElement = ieMoveRelXp..High(TEngineInputElement);

	TGameEventCallback = procedure(const evt: TEngineInputElement;
		const Value: shortint) of object;

	TEngineInputElementProc = procedure(const newtype: TEngineInputElement);

	TInputType = (itKeyboard, itMouse, itGamepad);

	{ TInput }

	TInput = class
	protected
		bindings: array[word] of TEngineInputElement;
		states: array[TEngineInputRelatedElement] of shortint;
		dbg: boolean;
		dev: TGameDevice;
		handleLogicRelatedLookup, handleInputRelatedLookup, handleEIElookup: array[boolean] of
		TGameEventCallback;
		itype: TInputType;

		UnlockRotaProc, LockRotaProc: TObjProc;

		procedure NotifyPortBinding(const evt: TEngineInputElement; const Value: shortint);
		procedure HandleComponent(const evt: TEngineInputElement; const Value: shortint);
		procedure HandleModifier(const evt: TEngineInputElement; const Value: shortint);
		procedure HandleInputRelatedElement(const evt: TEngineInputElement;
			const Value: shortint);
		procedure ProcessInput(const id: word; const Value: shortint);
	public
		property InputType: TInputType read itype;
		function addBinding(const event: word;
			const binding: TEngineInputElement): TEngineInputElement;

		procedure SetStatesCallback(const newcb: TGameEventCallback);
		procedure SetEventsCallback(const newcb: TGameEventCallback);
		procedure SetLockRotaProc(const newproc: TObjProc);
		procedure SetUnlockRotaProc(const newproc: TObjProc);

		function GetDeviceName: string;
		constructor Create(const device: TGameDevice);
		destructor Destroy; override;
	end;

procedure GameInputInit(mousekeyboard, gamepad: boolean);
procedure GamepadInit;
procedure MouseKeyboardInit;

implementation

var
	initialised: boolean = False;
	MouseKeyboard: TMouseKeyboardDevice = nil;
	gamepad: array[0..7] of TGamepadDevice = (nil, nil, nil, nil, nil, nil, nil, nil);
	gamepadcnt: byte = 0;

	devices: array[0..8] of TGameDevice = (nil, nil, nil, nil, nil, nil, nil, nil, nil);
	devicecnt: byte = 0;

function AddDevice(dev: TGameDevice): boolean;
begin
	Result := devicecnt < Length(devices);
	if Result then
	begin
		devices[devicecnt] := dev;
		Inc(devicecnt);
		dev.Start;
	end;
end;

procedure MouseKeyboardInit;
begin
	MouseKeyboard := TMouseKeyboardDevice.Create('mouse and keyboard');
	AddDevice(MouseKeyboard);
end;

procedure GameInputCleanup;
var
	zhlr: integer;
begin
	if initialised then
	begin
		initialised := False;
		if MouseKeyboard <> nil then
			MouseKeyboard.Terminate;
		for zhlr := 0 to gamepadcnt - 1 do
			gamepad[zhlr].Terminate;
	end;
end;

procedure GameInputInit(mousekeyboard, gamepad: boolean);
begin
	if not initialised then
	begin
		initialised := True;
		if gamepad then
			GamepadInit;
		if MouseKeyboard then
			MouseKeyboardInit;

		AddFreeRoutine(@GameInputCleanup);
	end;
end;

// TODO make this use SDL2
procedure GamepadInit;
var
	findrec: TSearchRec;
	joysticks, feedbacks: TStringList;
	tmpstr: string;
	path: string = '/dev/input/by-id/';
begin
	joysticks := TStringList.Create;
	feedbacks := TStringList.Create;
	if FindFirst(path + '*-joystick', faSysFile, findrec) = 0 then
	begin
		repeat
			if Pos('-event-joystick', findrec.Name) <> 0 then
			begin
				dbgTrace('Found Force-Feedback: ' + findrec.Name);
				feedbacks.Add(findrec.Name);
			end
			else
			begin
				dbgTrace('Found Joystick: ' + findrec.Name);
				joysticks.Add(findrec.Name);
			end;
		until FindNext(findrec) <> 0;
		FindClose(findrec);
	end
	else
		dbgTrace('Found no joystick events (/dev/input/by-id/*-joystick)');

	for tmpstr in joysticks do
	begin
		gamepad[gamepadcnt] :=
			TGamepadDevice.Create(Copy(tmpstr, 0, Pos('-joystick', tmpstr) - 1),
			path + tmpstr, '');
		addDevice(gamepad[gamepadcnt]);

		Inc(gamepadcnt);

		dbgTrace('Added gamepad: ' + tmpstr);
		//Feddes TODO
		if gamepadcnt > Length(gamepad) then
		begin
			dbgError('Cant take more than ' + IntToStr(Length(gamepad)) + ' joysticks');
			break;
		end;
	end;
end;

{ TInput }

procedure TInput.NotifyPortBinding(const evt: TEngineInputElement;
	const Value: shortint);
begin
	handleLogicRelatedLookup[evt < ieActionLeft](evt, Value);
end;

procedure TInput.HandleComponent(const evt: TEngineInputElement; const Value: shortint);
begin
	case evt of
		ieMoveRelXp, ieMoveRelXn:
			NotifyPortBinding(ieMoveRelX, states[ieMoveRelXp] - states[ieMoveRelXn]);
		ieMoveRelYp, ieMoveRelYn:
			NotifyPortBinding(ieMoveRelY, states[ieMoveRelYp] - states[ieMoveRelYn]);
		ieMoveRelZp, ieMoveRelZn:
			NotifyPortBinding(ieMoveRelZ, states[ieMoveRelZp] - states[ieMoveRelZn]);
		ieCursorXp, ieCursorXn:
			NotifyPortBinding(ieCursorX, states[ieCursorXp] - states[ieCursorXn]);
		ieCursorYp, ieCursorYn:
			NotifyPortBinding(ieCursorY, states[ieCursorYp] - states[ieCursorYn]);
		ieLookRelXZp, ieLookRelXZn:
			NotifyPortBinding(ieLookRelXZ, states[ieLookRelXZp] - states[ieLookRelXZn]);
		ieLookRelYp, ieLookRelYn:
			NotifyPortBinding(ieLookRelY, states[ieLookRelYp] - states[ieLookRelYn]);
		ieTurnRelXZp, ieTurnRelXZn:
			NotifyPortBinding(ieTurnRelXZ, states[ieTurnRelXZp] - states[ieTurnRelXZn]);
		ieTurnRelYp, ieTurnRelYn:
			NotifyPortBinding(ieTurnRelY, states[ieTurnRelYp] - states[ieTurnRelYn]);
		ieZoomp, ieZoomn:
			NotifyPortBinding(ieZoom, states[ieZoomp] - states[ieZoomn]);
	end;
end;

procedure TInput.HandleModifier(const evt: TEngineInputElement; const Value: shortint);
begin
	case evt of
		ieLockTurnLookXYZ:
		begin
		  if states[evt] = 0 then
      begin
        if states[ieMakeCursorMotionTurnAndLock] = 0 then
		 	  	UnlockRotaProc();
      end
      else
      begin
        if states[ieMakeCursorMotionTurnAndLock] = 0 then
			  	LockRotaProc();
      end;
		end;
		ieMakeCursorMotionLook:
		begin
			if states[evt] = 0 then
			begin
        if states[ieMakeCursorMotionTurnAndLock] = 0 then
        begin
  				SDL_SetRelativeMouseMode(False);
  				addBinding(Ord(mtkMotionX), TEngineInputElement.ieCursorX);
  				addBinding(Ord(mtkMotionY), TEngineInputElement.ieCursorY);
        end;
			end
			else
			begin
				// TODO make this work independently of input-types
        if states[ieMakeCursorMotionTurnAndLock] = 0 then
        begin
				  addBinding(Ord(mtkMotionX), TEngineInputElement.ieLookRelTickXZ);
				  addBinding(Ord(mtkMotionY), TEngineInputElement.ieLookRelTickY);
				  SDL_SetRelativeMouseMode(True);
			  end;
      end;
		end;
    ieMakeCursorMotionTurnAndLock:
		begin
			if states[evt] = 0 then
			begin
        if states[ieLockTurnLookXYZ] = 0 then
			  	UnLockRotaProc();
        if states[ieMakeCursorMotionLook] = 0 then
        begin
  				SDL_SetRelativeMouseMode(False);
  				addBinding(Ord(mtkMotionX), TEngineInputElement.ieCursorX);
  				addBinding(Ord(mtkMotionY), TEngineInputElement.ieCursorY);
        end;
			end
			else
			begin
				// TODO make this work independently of input-types
        if states[ieMakeCursorMotionLook] = 0 then
        begin
				  addBinding(Ord(mtkMotionX), TEngineInputElement.ieLookRelTickXZ);
				  addBinding(Ord(mtkMotionY), TEngineInputElement.ieLookRelTickY);
				  SDL_SetRelativeMouseMode(True);
			  end;
        if states[ieLockTurnLookXYZ] = 0 then
			  	LockRotaProc();
      end;
		end;
	end;
end;

procedure TInput.HandleInputRelatedElement(const evt: TEngineInputElement;
	const Value: shortint);
begin
	states[evt] := Value;
	handleInputRelatedLookup[evt < ieLockTurnLookXYZ](evt, Value);
end;

procedure TInput.ProcessInput(const id: word; const Value: shortint);
begin
	handleEIElookup[bindings[id] < Low(TEngineInputRelatedElement)](bindings[id], Value);
end;

function TInput.addBinding(const event: word;
	const binding: TEngineInputElement): TEngineInputElement;
begin
	Result := bindings[event];
	bindings[event] := binding;
end;

procedure TInput.SetStatesCallback(const newcb: TGameEventCallback);
begin
	handleLogicRelatedLookup[True] := newcb;
end;

procedure TInput.SetEventsCallback(const newcb: TGameEventCallback);
begin
	handleLogicRelatedLookup[False] := newcb;
end;

procedure TInput.SetLockRotaProc(const newproc: TObjProc);
begin
	LockRotaProc := newproc;
end;

procedure TInput.SetUnlockRotaProc(const newproc: TObjProc);
begin
	UnlockRotaProc := newproc;
end;

function TInput.GetDeviceName: string;
begin
	Result := dev.GetName;
end;

constructor TInput.Create(const device: TGameDevice);
begin
	inherited Create;
	//mach doc auf, filldword zb, und benchmarke
	FillByte(bindings, Length(bindings) * SizeOf(TEngineInputElement), 0);
	FillByte(states, Length(states) * SizeOf(states[Low(TEngineInputRelatedElement)]), 0);
	dev := device;
	dev.Plug(@ProcessInput);

	UnlockRotaProc := nil;
	LockRotaProc := nil;

	handleLogicRelatedLookup[False] := nil;
	handleLogicRelatedLookup[True] := nil;
	handleEIElookup[False] := @HandleInputRelatedElement;
	handleEIElookup[True] := @NotifyPortBinding;
	handleInputRelatedLookup[False] := @HandleModifier;
	handleInputRelatedLookup[True] := @HandleComponent;

	if dev is TGamepadDevice then
	begin
		itype := TInputType.itGamepad;

		addBinding(258, TEngineInputElement.ieLookRelY);
		addBinding(2, TEngineInputElement.ieLookRelXZ);
	end
	else if dev is TMouseKeyboardDevice then
	begin
		itype := TInputType.itKeyboard;
		// Cheat us sum bindings here until there is a way to read and write
		addBinding(SDL_SCANCODE_W, TEngineInputElement.ieMoveRelZn);
		addBinding(SDL_SCANCODE_S, TEngineInputElement.ieMoveRelZp);
		addBinding(SDL_SCANCODE_A, TEngineInputElement.ieMoveRelXn);
		addBinding(SDL_SCANCODE_D, TEngineInputElement.ieMoveRelXp);
		addBinding(SDL_SCANCODE_I, TEngineInputElement.ieLookRelYp);
		addBinding(SDL_SCANCODE_K, TEngineInputElement.ieLookRelYn);
		addBinding(SDL_SCANCODE_J, TEngineInputElement.ieLookRelXZn);
		addBinding(SDL_SCANCODE_L, TEngineInputElement.ieLookRelXZp);
		addBinding(SDL_SCANCODE_T, TEngineInputElement.ieTurnRelYp);
		addBinding(SDL_SCANCODE_G, TEngineInputElement.ieTurnRelYn);
		addBinding(SDL_SCANCODE_F, TEngineInputElement.ieTurnRelXZn);
		addBinding(SDL_SCANCODE_H, TEngineInputElement.ieTurnRelXZp);
		addBinding(SDL_SCANCODE_B, TEngineInputElement.ieZoomn);
		addBinding(SDL_SCANCODE_V, TEngineInputElement.ieZoomp);
		addBinding(SDL_SCANCODE_X, TEngineInputElement.ieAlignCam);
		addBinding(SDL_SCANCODE_SPACE, TEngineInputElement.ieJump);
		addBinding(Ord(mtkMotionX), TEngineInputElement.ieCursorX);
		addBinding(Ord(mtkMotionY), TEngineInputElement.ieCursorY);
		addBinding(Ord(mtkWheelY), TEngineInputElement.ieZoomTickY);
		addBinding(Ord(mtkLeft), TEngineInputElement.ieMakeCursorMotionLook);
		addBinding(Ord(mtkRight), TEngineInputElement.ieMakeCursorMotionTurnAndLock);
	end
	else
		raise Exception.Create('no default input available for ' + device.ClassName);
	dbgTrace('Created input of ' + dev.GetName + ' at ' + PtrToHex(@Self));
end;

destructor TInput.Destroy;
begin
	dbgTrace('Input of ' + dev.GetName + ' was destroyed');
	dev.Unplug;
	inherited Destroy;
end;

end.
