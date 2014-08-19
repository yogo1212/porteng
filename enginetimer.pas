unit EngineTimer;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, SDL2, EngineFacilities;

type

	{ TGameTimer }

	TGameTimer = object
	private
		last, new, delta, milliSecInterval: UInt32;
	public
		function waitFor: UInt32; inline;
		function GetDelta: UInt32; inline;
		constructor Create(nmilliSecInterval: UInt32);
	end;

procedure GameTimeInit;

implementation

var initialised: Boolean = False;

procedure GameTimeFree;
begin
  if initialised then
  begin
     SDL_QuitSubSystem(SDL_INIT_TIMER);
  end;
end;

procedure GameTimeInit;
begin
  if not initialised then
  begin
     SDL_InitSubSystem(SDL_INIT_TIMER);
     AddFreeRoutine(@GameTimeFree);
  end;
end;

type
	PUInt32 = ^UInt32;

	TWaitProc = procedure(millisecs: UInt32); register;
	TOverflowProc = procedure(correct: PUInt32); register;

procedure DoWait(millisecs: UInt32); register;
begin
	Sleep(millisecs);
end;

procedure DoNoWait({%H-}millisecs: UInt32); register;
begin
	// I don't even know wether this makes sense to look-up
end;

procedure DoNoOverflow({%H-}correct: PUInt32); register;
begin

end;

procedure CorrectOverflow(amount: PUInt32); register;
begin
  // amount is a number that can be expressed as (2^32 - a) - b
  // that equals 2^32 - (a + b)

	amount^ += -(amount^);
end;

var
	waitLookup: array[boolean] of TWaitProc = (@DoNoWait, @DoWait);
	overflowLookup: array[boolean] of TOverflowProc = (@DoNoOverflow, @CorrectOverflow);

function GetTimeOfDayMilliSeconds: UInt32;
begin
	Result := SDL_GetTicks;
end;

{
function GetTimeOfDayMicroSeconds: UInt64;
begin
  fpgettimeofday(@tp, nil);
  // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := (tp.tv_sec * 1000000) + tp.tv_usec;
end;
}

{ TGameTimer }

function TGameTimer.waitFor: UInt32;
begin
	delta := GetDelta;

	waitLookup[delta < milliSecInterval]((milliSecInterval - delta));

	Result := delta + GetDelta;
end;

function TGameTimer.GetDelta: UInt32;
begin
	new := GetTimeOfDayMilliSeconds;
	Result := new - last;
  //overflow happens every 50 days
	overflowLookup[last > new](@Result);
	last := new;
end;

constructor TGameTimer.Create(nmilliSecInterval: UInt32);
begin
	milliSecInterval := nmilliSecInterval;
	last := GetTimeOfDayMilliSeconds;
end;

end.
