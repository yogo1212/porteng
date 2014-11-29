unit EngineMouseKeyboard;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, SDL2;

type
	TGameMouseKeyEvent = record
		key: word;
		Value: shortint;
	end;

	TMouseToKeyMapping = (mtkLeft = SDL_NUM_SCANCODES, mtkMiddle, mtkRight, mtkX1, mtkX2,
		// positive -> right; negative -> left
		mtkWheelX,
		// positive -> upward motion; negative -> downward motion
		mtkWheelY,
		mtkMotionX, mtkMotionY);

	PGameMouseKeyEvent = ^TGameMouseKeyEvent;

	PSDL_KeyboardEvent = ^TSDL_KeyboardEvent;
	PSDL_MouseButtonEvent = ^TSDL_MouseButtonEvent;
	PSDL_MouseWheelEvent = ^TSDL_MouseWheelEvent;
	PSDL_MouseMotionEvent = ^TSDL_MouseMotionEvent;

procedure KeyDown(really: boolean; evt: PSDL_KeyboardEvent); inline;
procedure KeyUp(really: boolean; evt: PSDL_KeyboardEvent); inline;
procedure MouseBtnDown(really: boolean; evt: PSDL_MouseButtonEvent); inline;
procedure MouseBtnUp(really: boolean; evt: PSDL_MouseButtonEvent); inline;
procedure MouseWheel(really: boolean; evt: PSDL_MouseWheelEvent); inline;
procedure MouseMotion(really: boolean; evt: PSDL_MouseMotionEvent); inline;
function Dequeue: TGameMouseKeyEvent;

//procedure EnableRelativeMode;
//procedure DisableRelativeMode;

implementation

type
	TGameRingArray = record
		{There are only two threads, one reading and one writing
    Not more than 255 chars can be inserted at a time}
		rPos, wPos: byte;
		Data: array[byte] of TGameMouseKeyEvent;
	end;

var
	EvtRingArray: TGameRingArray;

type
	TKeyFunc = procedure(evt: PSDL_KeyboardEvent; Value: SInt32);
	TMouseBtnFunc = procedure(evt: PSDL_MouseButtonEvent; Value: Sint32);
	TMouseWheelFunc = procedure(evt: PSDL_MouseWheelEvent);
	TMouseMotionFunc = procedure(evt: PSDL_MouseMotionEvent);

procedure TextFunc(evt: PSDL_KeyboardEvent; Value: Sint32);
begin
	raise Exception.Create('Not yet implemented');
end;

procedure QueueKeyFunc(evt: PSDL_KeyboardEvent; Value: SInt32); inline;
begin
	// It is important to first put the event in the buffer and THEN increase the count.
	// Otherwise, it would not be thread-safe (to read and write simultaneously)
	EvtRingArray.Data[EvtRingArray.wPos].key := evt^.keysym.scancode;
	EvtRingArray.Data[EvtRingArray.wPos].Value := Value;
	Inc(EvtRingArray.wPos);
end;

procedure QueueMouseBtnFunc(evt: PSDL_MouseButtonEvent; Value: Sint32); inline;
begin
	// It is important to first put the event in the buffer and THEN increase the count.
	// Otherwise, it would not be thread-safe (to read and write simultaneously)
	EvtRingArray.Data[EvtRingArray.wPos].key := Ord(mtkLeft) + (evt^.button - 1);
	EvtRingArray.Data[EvtRingArray.wPos].Value := Value;
	Inc(EvtRingArray.wPos);
end;

procedure QueueMouseWheelFunc(evt: PSDL_MouseWheelEvent); inline;
begin
	// It is important to first put the event in the buffer and THEN increase the count.
	// Otherwise, it would not be thread-safe (to read and write simultaneously)
	EvtRingArray.Data[EvtRingArray.wPos].key := Ord(mtkWheelX);
	EvtRingArray.Data[EvtRingArray.wPos].Value := evt^.x;
	Inc(EvtRingArray.wPos);
	EvtRingArray.Data[EvtRingArray.wPos].key := Ord(mtkWheelY);
	EvtRingArray.Data[EvtRingArray.wPos].Value := evt^.y;
	Inc(EvtRingArray.wPos);
end;

var
	RelativeMode: boolean = True;
{
procedure EnableRelativeMode;
begin
  if not RelativeMode then
  begin
    RelativeMode := True;
    // dont have to save the position of the cursor. It doesn't change in relativemode
    SDL_SetRelativeMouseMode(true);
  end;
end;

procedure DisableRelativeMode;
begin
  if RelativeMode then
  begin
    RelativeMode := False;
    SDL_SetRelativeMouseMode(false);
  end;
end;
}

procedure QueueMouseRelativeMotionFunc(evt: PSDL_MouseMotionEvent); inline;
begin
	// It is important to first put the event in the buffer and THEN increase the count.
	// Otherwise, it would not be thread-safe (to read and write simultaneously)
	EvtRingArray.Data[EvtRingArray.wPos].key := Ord(mtkMotionX);
	EvtRingArray.Data[EvtRingArray.wPos].Value := evt^.xrel;
	Inc(EvtRingArray.wPos);
	EvtRingArray.Data[EvtRingArray.wPos].key := Ord(mtkMotionY);
	EvtRingArray.Data[EvtRingArray.wPos].Value := evt^.yrel;
	Inc(EvtRingArray.wPos);
end;

procedure QueueMouseAbsoluteMotionFunc(evt: PSDL_MouseMotionEvent); inline;
begin
	// It is important to first put the event in the buffer and THEN increase the count.
	// Otherwise, it would not be thread-safe (to read and write simultaneously)
	EvtRingArray.Data[EvtRingArray.wPos].key := Ord(mtkMotionX);
	EvtRingArray.Data[EvtRingArray.wPos].Value := evt^.x;
	Inc(EvtRingArray.wPos);
	EvtRingArray.Data[EvtRingArray.wPos].key := Ord(mtkMotionY);
	EvtRingArray.Data[EvtRingArray.wPos].Value := evt^.y;
	Inc(EvtRingArray.wPos);
end;

var
	run: array[boolean] of TKeyFunc = (@QueueKeyFunc, @TextFunc);

const
	Textmode: boolean = False;

// :-p                                   keybard
procedure DontKey({%H-}Key: PSDL_KeyboardEvent; {%H-}Value: Sint32);
begin

end;

procedure DoKey(evt: PSDL_KeyboardEvent; Value: Sint32);
begin
	run[TextMode](evt, Value);
end;

var
	keyLookup: array[boolean] of TKeyFunc = (@DontKey, @DoKey);

procedure KeyUp(really: boolean; evt: PSDL_KeyboardEvent);
begin
	keyLookup[really](evt, 0);
end;

procedure KeyDown(really: boolean; evt: PSDL_KeyboardEvent);
begin
	keyLookup[really](evt, 127);
end;

//                                         mousebtn
procedure DontMouseBtn({%H-}Key: PSDL_MouseButtonEvent; {%H-}Value: Sint32);
begin

end;

procedure DoMouseBtn(evt: PSDL_MouseButtonEvent; Value: Sint32);
begin
	QueueMouseBtnFunc(evt, Value);
end;

var
	mouseBtnLookup: array[boolean] of TMouseBtnFunc = (@DontMouseBtn, @DoMouseBtn);

procedure MouseBtnDown(really: boolean; evt: PSDL_MouseButtonEvent);
begin
	mouseBtnLookup[really](evt, 127);
end;

procedure MouseBtnUp(really: boolean; evt: PSDL_MouseButtonEvent);
begin
	mouseBtnLookup[really](evt, 0);
end;

//                                         mousemotion
procedure DontMouseMotion({%H-}Key: PSDL_MouseMotionEvent);
begin

end;

procedure DoMouseMotion(evt: PSDL_MouseMotionEvent);
const
	motionLookup: array[boolean] of TMouseMotionFunc =
		(@QueueMouseAbsoluteMotionFunc, @QueueMouseRelativeMotionFunc);
begin
	motionLookup[RelativeMode](evt);
end;

const
	mouseMotionLookup: array[boolean] of TMouseMotionFunc =
		(@DontMouseMotion, @DoMouseMotion);

procedure MouseMotion(really: boolean; evt: PSDL_MouseMotionEvent);
begin
	mouseMotionLookup[really](evt);
end;

//                                         mousewheel
procedure DontMouseWheel({%H-}Key: PSDL_MouseWheelEvent);
begin

end;

procedure DoMouseWheel(evt: PSDL_MouseWheelEvent);
begin
	QueueMouseWheelFunc(evt);
end;

var
	mouseWheelLookup: array[boolean] of TMouseWheelFunc = (@DontMouseWheel, @DoMouseWheel);

procedure MouseWheel(really: boolean; evt: PSDL_MouseWheelEvent);
begin
	mouseWheelLookup[really](evt);
end;

function Dequeue: TGameMouseKeyEvent;
begin
	// Thiz gievs s problmz when more than 255 are pending
	while EvtRingArray.wPos = EvtRingArray.rPos do
	begin
		Sleep(10);
		// Pseudo-Blocking
	end;
	Result := EvtRingArray.Data[EvtRingArray.rPos];
	Inc(EvtRingArray.rPos);
	// This is only called by one reading thread (1 TKeyboardDevice)
end;

initialization
	TextMode := False;
	EvtRingArray.rPos := 0;
	EvtRingArray.wPos := 0;
end.
