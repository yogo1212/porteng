unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Math, EngineUnit, EngineTypes, EngineMath, GameStats;

type
	PGameUnit = ^TGameUnit;

	PAbility = ^TAbility;

	TCastResult = (CR_OK = 0, CR_OUT_OF_RANGE, CR_OUT_OF_MANA, CR_UNREST);

	TAbilityFunc = function(const ability: PAbility;
		const caster, target: PGameUnit): TCastResult;

	{ TAbility }

	TAbility = object
	private
		CheckConditions, TriggerAction: TAbilityFunc;
	public
		range, casttime, cooldown, remainingCooldown: single;
		resourcecost: longword;
		unrest: byte;
		procedure Init(nrange, ncasttime, ncooldown: single; nresourcecost: longword;
			nunrest: byte; nCheckConditions, nTriggerAction: TAbilityFunc);
		function StartCast(const caster, target: PGameUnit): TCastResult;
		function FinaliseCast(const caster, target: PGameUnit): TCastResult;
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
		unrest: byte;

		baseStats, stats: TGameStats;
		statmodifiers: TGameStatModifyState;

		procedure UpdateStats;
		procedure passtime(seconds: single);
		// TRY casting the ability at index ai
		procedure Cast(ai: word; target: PGameUnit);
	end;

function CheckRange(const ability: PAbility;
	const caster, target: PGameUnit): TCastResult; inline;
function CheckMana(const ability: PAbility;
	const caster, target: PGameUnit): TCastResult;
	inline;
function DefaultConditionChecks(const ability: PAbility;
	const caster, target: PGameUnit): TCastResult; inline;

implementation

function mergeCastResults(const a, b: TCastResult): TCastResult; inline;
begin
	Result := TCastResult(Max(Ord(a), Ord(b)));
end;

function CheckRange(const ability: PAbility;
	const caster, target: PGameUnit): TCastResult;
	inline;
begin
	if LengthSquare(caster^.physunit.pos - target^.physunit.pos) <= ability^.range then
		Result := CR_OK
	else
		Result := CR_OUT_OF_RANGE;
end;

function CheckMana(const ability: PAbility;
	const caster, target: PGameUnit): TCastResult;
	inline;
begin
	if caster^.resource >= ability^.resourcecost then
		Result := CR_OK
	else
		Result := CR_OUT_OF_MANA;
end;

function CheckRest(const ability: PAbility;
	const caster, target: PGameUnit): TCastResult;
	inline;
begin
	if caster^.unrest + ability^.unrest <= High(caster^.unrest) then
		Result := CR_OK
	else
		Result := CR_UNREST;
end;

function CheckResources(const ability: PAbility;
	const caster, target: PGameUnit): TCastResult;
	inline;
begin
	Result := mergeCastResults(CheckRest(ability, caster, target),
		CheckMana(ability, caster, target));
end;

function DefaultConditionChecks(const ability: PAbility;
	const caster, target: PGameUnit): TCastResult; inline;
begin
	Result := mergeCastResults(CheckRange(ability, caster, target),
		CheckResources(ability, caster, target));
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
		if currentcast.ability^.FinaliseCast(@Self, currentCast.target) = CR_OK then
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
	if abilities[ai].StartCast(@Self, target) = CR_OK then
		currentCast.Init(@abilities[ai], target);
end;

{ TAbility }

procedure TAbility.Init(nrange, ncasttime, ncooldown: single;
	nresourcecost: longword; nunrest: byte; nCheckConditions, nTriggerAction: TAbilityFunc);
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

function TAbility.StartCast(const caster, target: PGameUnit): TCastResult;
begin
	Result := CheckConditions(@Self, caster, target);
	// TODO show cast-bar?
end;

function TAbility.FinaliseCast(const caster, target: PGameUnit): TCastResult;
begin
	Result := CheckConditions(@Self, caster, target);
	if Result = CR_OK then
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
