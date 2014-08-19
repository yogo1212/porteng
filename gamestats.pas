unit GameStats;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineMemory;

type
	TGameStats = record
		speed: single;
	end;

	TGameStatModifierProc = procedure(var stats: TGameStats); register;

	PGameStatModifier = ^TGameStatModifier;

	TGameStatModifier = record
		proc: TGameStatModifierProc;
		priority: byte;
	end;

	{ TGameStatModifyState }

	TGameStatModifyState = object
	private
		mods: TContinuousMemoryManager;
	public
		constructor Create;
		procedure Add(priority: byte; proc: TGameStatModifierProc);
		procedure Apply(var stats: TGameStats); register;
		procedure Delete(priority: byte; proc: TGameStatModifierProc);
	end;

implementation

{ TGameStatModifyState }

constructor TGameStatModifyState.Create;
begin
	mods := TContinuousMemoryManager.Create(SizeOf(TGameStatModifier), 50);
end;

procedure TGameStatModifyState.Add(priority: byte; proc: TGameStatModifierProc);
var
	tmpmod: TGameStatModifier;
	tmpindex: cardinal;
begin
	tmpmod.proc := proc;
	tmpmod.priority := priority;

	tmpindex := 0;
	while (tmpindex < mods.Count) and
		(priority <= PGameStatModifier(mods.Get(tmpindex))^.priority) do
		Inc(tmpindex);

  mods.Insert(tmpindex);
  mods.Items[tmpindex] := @tmpmod;
	{ // binary-search
  mods.Get((left + right) / 2);
  }
end;

procedure TGameStatModifyState.Apply(var stats: TGameStats); register;
var
	cnt: cardinal;
begin
	cnt := 0;
	while cnt < mods.Count do
	begin
		PGameStatModifier(mods.Get(cnt))^.proc(stats);
		Inc(cnt);
	end;
end;

procedure TGameStatModifyState.Delete(priority: byte; proc: TGameStatModifierProc);
var
	tmpmod: TGameStatModifier; tmpindex: Cardinal;
begin
	tmpmod.proc := proc;
	tmpmod.priority := priority;


	tmpindex := 0;
	while (tmpindex < mods.Count) and
		not((priority = PGameStatModifier(mods.Get(tmpindex))^.priority)
    and (proc = PGameStatModifier(mods.Get(tmpindex))^.proc)) do
		Inc(tmpindex);
	mods.Delete(tmpindex);
end;

end.
