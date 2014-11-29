unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, EngineUnit, EngineTypes, EngineMath, GameStats;

type
	PGameUnit = ^TGameUnit;

	PAbility = ^TAbility;

	TAbilityFunc = function(const ability: PAbility;
		const caster, target: PGameUnit): boolean;

	{ TAbility }

	TAbility = object
	private
		CheckConditions, TriggerAction: TAbilityFunc;
	public
		range, casttime, cooldown, remainingCooldown: single;
		resourcecost: longword;
    unrest: byte;
		procedure Init(nrange, ncasttime, ncooldown: single; nresourcecost: longword;
      nunrest: Byte; nCheckConditions, nTriggerAction: TAbilityFunc);
		function StartCast(const caster, target: PGameUnit): boolean;
		function FinaliseCast(const caster, target: PGameUnit): boolean;
		procedure passtime(seconds: single);
	end;

	{ TCast }

	TCast = object
		ability: PAbility;
		target: PGameUnit;
		progress: single;
		constructor Init(nability: PAbility; ntarget: PGameUnit);
		destructor Clear;
	end;

	{ TGameUnit }

	TGameUnit = object
		abilities: array[0..5] of TAbility;
		physunit: TEngineUnit;
		currentCast: TCast;
		resource: longword;
    unrest: Byte;

    baseStats, stats: TGameStats;
	  statmodifiers: TGameStatModifyState;

    procedure UpdateStats;
		procedure passtime(seconds: single);
		// TRY casting the ability at index ai
		procedure Cast(ai: word; target: PGameUnit);
	end;

function CheckRange(const ability: PAbility; const caster, target: PGameUnit): boolean;
	inline;
function CheckMana(const ability: PAbility; const caster, target: PGameUnit): boolean;
	inline;
function DefaultConditionChecks(const ability: PAbility;
	const caster, target: PGameUnit): boolean; inline;

implementation

function DummyAbilityFunc(const ability: PAbility;
	const caster, target: PGameUnit): boolean;
begin
	Result := True;
end;

const
	emptyAbility: TAbility = (CheckConditions: @DummyAbilityFunc;
		TriggerAction: @DummyAbilityFunc; range: 1; casttime: 0; cooldown: 0;
		remainingCooldown: 0; resourcecost: 0);

function CheckRange(const ability: PAbility; const caster, target: PGameUnit): boolean;
	inline;
begin
	Result := LengthSquare(caster^.physunit.pos - target^.physunit.pos) <=
		(ability^.range * ability^.range);
end;

function CheckMana(const ability: PAbility; const caster, target: PGameUnit): boolean;
	inline;
begin
	Result := caster^.resource >= ability^.resourcecost;
end;

function CheckRest(const ability: PAbility; const caster, target: PGameUnit): boolean;
	inline;
begin
	Result := caster^.unrest + ability^.unrest <= 255;
end;

function CheckResources(const ability: PAbility; const caster, target: PGameUnit): boolean;
	inline;
begin
  Result := CheckRest(ability, caster, target) and CheckMana(ability, caster, target);
end;

function DefaultConditionChecks(const ability: PAbility;
	const caster, target: PGameUnit): boolean; inline;
begin
	Result := CheckRange(ability, caster, target) and CheckResources(ability, caster, target);
end;

{ TCast }

constructor TCast.Init(nability: PAbility; ntarget: PGameUnit);
begin
	ability := nability;
	target := ntarget;
	progress := 0;
end;

destructor TCast.Clear;
begin
	ability := @emptyAbility;
end;

{ TGameUnit }

procedure TGameUnit.UpdateStats;
begin
  stats := baseStats;
  statmodifiers.Apply(stats);

  physunit.speed := stats.speed;
end;

procedure TGameUnit.passtime(seconds: single);
begin
	currentcast.progress += seconds;
	// TODO ugli-brunches
	if currentCast.progress > currentCast.ability^.casttime then
	begin
		if currentcast.ability^.FinaliseCast(@Self, currentCast.target) then
			currentCast.ability^.TriggerAction(currentCast.ability, @Self, currentCast.target);
	end;

	// hand-rolled-sushi:
	abilities[0].passtime(seconds);
	abilities[1].passtime(seconds);
	abilities[2].passtime(seconds);
	abilities[3].passtime(seconds);
	abilities[4].passtime(seconds);
	abilities[5].passtime(seconds);
end;

procedure TGameUnit.Cast(ai: word; target: PGameUnit);
begin
	if (currentCast.ability = @emptyAbility) and
		abilities[ai].StartCast(@Self, target) then
		currentCast.Init(@abilities[ai], target);
end;

{ TAbility }

procedure TAbility.Init(nrange, ncasttime, ncooldown: single; nresourcecost: longword;
	nunrest: Byte; nCheckConditions, nTriggerAction: TAbilityFunc);
begin
	range := nrange;
	casttime := ncasttime;
	cooldown := ncooldown;
	remainingCooldown := 0;
	resourcecost := nresourcecost;
  unrest := nunrest;
	CheckConditions := nCheckConditions;
	TriggerAction := nTriggerAction;
end;

function TAbility.StartCast(const caster, target: PGameUnit): boolean;
begin
	Result := CheckConditions(@Self, caster, target);
	// TODO show cast-bar?
end;

function TAbility.FinaliseCast(const caster, target: PGameUnit): boolean;
begin
	Result := CheckConditions(@Self, caster, target);
	if Result then
		TriggerAction(@Self, caster, target);
end;

procedure TAbility.passtime(seconds: single);
begin
	// TODO lookup (if possible)
	if remainingCooldown > 0 then
	begin
		remainingCooldown -= seconds;
		if remainingCooldown < 0 then
			remainingCooldown := 0;
	end;
end;

end.

