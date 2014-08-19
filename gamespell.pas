unit GameSpell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GameUnit, GameStats;

function CreateSampleAbility: TAbility;

implementation

procedure DoubleSpeed(var stats: TGameStats);
begin
  stats.speed *= 2;
end;

function SampleAbilityAction(const ability: PAbility; const caster, target: PGameUnit)
  : Boolean;
begin
  caster^.statmodifiers.add(10, @DoubleSpeed);
end;

function CreateSampleAbility: TAbility;
begin
  Result.Init(1, 3, 10, 10, @DefaultConditionChecks, @SampleAbilityAction);
end;

end.

