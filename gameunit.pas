unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Math, EngineUnit, EngineTypes, EngineMath, GameStats;

type
	PGameUnit = ^TGameUnit;

	PAbility = ^TAbility;

	TAbiltyID = 0..5;

	TCastResult = (CR_OK = 0, CR_OUT_OF_RANGE, CR_OUT_OF_MANA, CR_UNREST);

	TAbilityFunc = function(const ability: PAbility;
		const caster, target: PGameUnit): TCastResult;

	{ TAbility }

	// non-critical game-logic-stuff can probably be a slow TObject anyway
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
		caster, target: PGameUnit;
		progress: single;
		constructor Init(nability: PAbility; ncaster, ntarget: PGameUnit);
		procedure Inpterrupt;
		function advanceTime(seconds: single): boolean;
		procedure Clear;
	end;

	{ TGameUnit }

	TGameUnit = object
		physunit: PEngineUnit;
		currentCast: TCast;
		resource: longword;
		unrest: byte;

		baseStats, stats: TGameStats;
		statmodifiers: TGameStatModifyState;

		procedure UpdateStats;
		procedure passtime(seconds: single);
		// TRY casting the ability
		procedure Cast(ai: PAbility; target: PGameUnit);
		constructor Init(u: PEngineUnit; speed: single);
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
	if LengthSquare(caster^.physunit^.pos - target^.physunit^.pos) <= ability^.range then
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

constructor TCast.Init(nability: PAbility; ncaster, ntarget: PGameUnit);
begin
	ability := nability;
	target := ntarget;
	caster := ncaster;
	progress := 0;
end;

procedure TCast.Inpterrupt;
begin

end;

function TCast.advanceTime(seconds: single): boolean;
begin
	Result := False;
	progress += seconds;

	// TODO oh god, this would be so much nicer with events
	if progress > ability^.casttime then
	begin
		if ability^.FinaliseCast(caster, target) = CR_OK then
		begin
			Result := True;
			ability^.TriggerAction(ability, caster, target);
		end;
	end;
end;

procedure TCast.Clear;
begin

end;

{ TGameUnit }

procedure TGameUnit.UpdateStats;
begin
	stats := baseStats;
	statmodifiers.Apply(stats);

	physunit^.speed := stats.speed;
end;

procedure TGameUnit.passtime(seconds: single);
begin
	if currentCast.advanceTime(seconds) then
		currentCast.Clear;

end;

procedure TGameUnit.Cast(ai: PAbility; target: PGameUnit);
begin
	if ai^.StartCast(@Self, target) = CR_OK then
		currentCast.Init(ai, @Self, target);
end;

constructor TGameUnit.Init(u: PEngineUnit; speed: single);
begin
	physunit := u;

	currentCast.Clear;
	resource := 0;
	unrest := 0;

	FillChar(baseStats, SizeOf(baseStats), 0);
	baseStats.speed := speed;
	statmodifiers.Init;
	UpdateStats;
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
	begin
		caster^.resource -= resourcecost;
		caster^.unrest += unrest;
		remainingCooldown := cooldown;
		TriggerAction(@Self, caster, target);
	end;
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
