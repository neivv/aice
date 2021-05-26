# Aice

Aice is a iscript plugin which extends iscript with a few commands.

Aice doesn't use `iscript.bin`, but instead requires a complete iscript in the file
`scripts\iscript.txt` which will be compiled on startup. A starting `iscript.txt` can be generated
from decompiling entire `iscript.bin` with IceCC.

PyICE can be used to decompile the script too, but it has some differences in output that need
to be corrected before it is accepted. Trying to load the script with Aice should give line
numbers for following issues:
1. Some labels have backslashes that need to be removed (`Longbolt\Halo\GeminiMissilesTrailInit`)
2. Some twilight and desert tileset doodads get duplicate names, leading to conflicting labels.
They have to be renamed to be unique. For example, there should be 2 different `DesertDoodad`
images with same labels, other of them should be changed to use labels `DesertDoodad2Init` and
`DesertDoodad2Local00` instead.

## Miscellaneous features

### Quick reload

During game, the script will be compiled on initial startup, but also every time a map starts,
allowing `iscript.txt` to be modified without having to restart Starcraft every time. Note that this
obviously requires using a setting that sources mod files directly from disk without compiling them
to a exe.

### iscript.bin size limits

Aice does not directly raise the limit of 65535 bytes of compiled iscript.bin, but extended
commands are mostly compiled to a separate memory buffer which makes it easier to keep the
bw-visible .bin under 65kB. If anyone ever ends up getting close to the limit with Aice, it is
relatively simple to update it to take over some regular commands, keeping the bw-visible
filesize low.

### Compatibility with other plugins

Aice has not been tested with any other iscript-touching plugin, though it may
work with plugins that hook parts of Starcraft's command handling. Compatibility depends on the
other plugin's hooking methods; replacing switch jump destinations or hooking at switch cases
is expected to work. iscript.bin format extensions have to be added to Aice's compiling code,
though as far as I'm aware, no plugin extends the format of iscript.bin.

## Commands

### if

```
if <condition> goto <label>
if <condition> call <label>
```

Jumps to or calls `label` if the `condition` evaluates to `true`. The condition can be an arbitrary
expression, see [Expressions][expr] for the syntax on expressions.

### set

```
set <bw_place> = <expression>
set spritelocal <var> = <expression>
set global <var> = <expression>
```

Evaluates [`expression`][expr] and assigns it to a place.

The place to be assigned can be either [Bw-visible memory][bw-place] or a variable internal
to Aice. Assigning to a bw place will truncate the variable if it is larger than the memory
location being assigned to.

When assigning to a variable the variable's type must be declared; using different types
with a single name will be a compile error. The variable types are

- `spritelocal` A variable that is shared between images of a single sprite.
- `global` A global variable, shared between iscript of all images, regardless of player.
`global` can be useful when wanting to move data to a child image that a following command
creates, as the first frame of the child's iscript will execute immediately before execution
continues from parent's iscript.

Anything that was assigned with `set` can be evaluated later in any [expression][expr], as
expressions can use variables and Bw-visible memory.

### fireweapon

```
fireweapon <weapon_id>
```

Causes the unit to attack with a weapon. Weapon can be any [expression][expr], though
most of the time you likely just use a constant Weapon ID that you need.

`fireweapon` behaves exactly like `castspell` except it allows specifying any weapon to be used
instead of using what was specified in `orders.dat`.
There may be some minor difference between `fireweapon` and `attack` or `attackwith`, though that
has not been explicitly confirmed. The very least they have very similar behaviour. While
`useweapon` also allows specifying any weapon to used, `useweapon` spawns the bullet on top of
target regardless of its `weapons.dat` configuration, making it less useful than `fireweapon` with
certain weapons.

### playfram

```
playfram <frame>
```

Aice allows specifying arbitrary [expression][expr] for `playfram` command.

Otherwise the functionality is same as in vanilla BW: If the image ID has `Turning Graphic` is
set in `images.dat`, the actual displayed frame will be chosen from range `frame` to `frame + 16`
depending on image's rotation.

### gotorepeatattk

```
gotorepeatattk
```

Aice overrides the original behaviour of `gotorepeatattk`, arguably fixing a bug:
If an unit's cooldown had expired before `gotorepeatattk` was used to reset its attack sequence,
the unit would only check for next attack cycle every 8th (9th?) frame, causing cooldowns be
surprisingly off from what was expected.

Aice changes the behaviour so the unit immediately attacks again if its cooldown had expired
before `gotorepeatattk`. This does end up slightly reducing some cooldowns compared to unmodded
game, bringing them to what `weapons.dat` specified.


### create\_unit

```
create_unit <unit_id> <x> <y> <player>
```

Creates an unit for player. The unit will be spawned at the specified position, even if a ground
unit would not fit there or the area is unwalkable. All four arguments are [expressions][expr].

#### Example

```
create_unit 40 (flingy.position_x + 3) (flingy.position_y + 3) player
create_unit 40 (flingy.position_x - 3) (flingy.position_y - 3) player
```

Creates 2 broodlings at the current unit exists for the current unit's player, slightly on top
of each other. Parentheses around x/y expressions would be optional, but the are likely nicer
for readability

### call

```
call <label>
```

Calls a subroutine. Use `return` inside subroutine to continue from a point after `call`.

Unlike `call` in vanilla BW, which only allows a single call that must be returned from before
using `call` again, Aice allows nested calls up to 256 calls.

## Expressions

There are two types of expressions: Integer (32-bit signed) and boolean expressions
(`true` and `false`).  Integer expressions can use basic arithmetic and comparision operators to
combine them, while booleans can only be compared with `==` and `!=`.

Arithmetic operators are `+` `-` `*` `/` and `%` (modulo), addition and subtraction will saturate
at largest/smallest possible 32-bit integer values, dividing by a expression evaluating to zero
*does not fail, but returns maxmimum 32-bit integer* (This may be changed in the future, please
don't rely on this), dividing by a constant zero is a compile-time error.

Comparision operators are `==` `!=` `<` `<=` `>` and `>=`, a result of a integer comparision will
be a boolean.

Booleans can be chained with `&&` and and `||` or, mixing them always requires parentheses.

There are also several builtin expressions referring to current unit or bullet of the iscript.
If an expressions refers to an object that the image doesn't have, an error will be printed and
it evaluates to something - exact behaviour is not specified.

The following are builtin boolean expressions (Available also in Mtl):
- `parasited` true if the unit is parasited
- `blind` true if the unit is blind
- `under_storm` true if the unit has been recently damaged by psionic storm. This variable is
set every few frames, and only set again when the storm damages the unit again, so it is not
reliable to a frame.
- `lifted_off` true if the unit is not a landed building
- `building_unit` true if the unit is building another unit. Applies to buildings which train
their units, and to SCVs constructing a building.
- `in_transport` true if the unit is currently in a transport
- `in_bunker` true if the unit is currently in a bunker
- `carrying_powerup` true if the worker is currently carrying a powerup unit
- `carrying_minerals` true if the worker is holding minerals (not a mineral powerup unit)
- `carrying_gas` true if the worker is holding gas (not a gas powerup unit)
- `burrowed` true if the unit is burrowed
- `disabled` true if the unit is disabled, either by a trigger, from being an unpowered Protoss
building, or being hit by a lockdown/stasis/maelstrom
- `completed` true if the unit is completed
- `self_cloaked` true if the unit is using a cloak order. False for units using permament cloak
in units.dat
- `arbiter_cloaked` true for unburrowed units under an Arbiter.
- `cloaked` true for any unburrowed unit that is cloaked
- `under_dweb` true if the unit is under a Disruption Web
- `hallucination` true if the unit is an hallucination
- `tech(int player, int tech_id)` true if `player` has reseached `tech_id`

The following are Aice-specific boolean expressions:
- `sprite.has_flingy` true if the current image's parent sprite is linked to an unit or a
bullet.
- `sprite.has_bullet` true if the current image's parent sprite is linked to a bullet.
- `sprite.has_unit` true if the current image's parent sprite is linked to an unit.

The following are builtin integer expressions (Available also in Mtl):
- `matrix_hitpoints` Current hitpoints for defensive matrix (256 times displayed value), or 0
if the unit is not matrixed
- `acid_spore_count` How many stacks of Devourer's acid spores the unit has on it
- `fighters` How many Scarabs/Interceptors the unit has in its hangar
- `mines` How many Spider Mines a Vulture has
- `hitpoints` Unit's current hitpoints (256 times displayed value)
- `hitpoints_percent` How many percent hitpoints the unit has left
- `shields` Unit's current shields (256 times displayed value)
- `shields_percent` How many percent shields the unit has left
- `energy` Unit's current energy (256 times displayed value)
- `kills` How many kills the unit has
- `carried_resource_amount` How many resources a worker is carrying (That is, usually 8, 2, or 0)
- `frame_count` How many frames (Game logic steps) the game has lasted
- `tileset` Integer ID of the current map's tileset
- `minerals` Mineral count of the current unit's player (Does not work with bullets)
- `gas` Mineral count of the current unit's player (Does not work with bullets)
- `ground_cooldown` How many frames of cooldown are remaining before the unit can attack again
with its ground weapon
- `air_cooldown` How many frames of cooldown are remaining before the unit can attack again
with its air weapon
- `spell_cooldown` How many frames of cooldown are remaining before the unit can cast another
spell
- `speed` The unit's current speed, in flingy.dat units but also applicable to units using iscript
movement
- `sigorder` Iscript sigorder value for the current unit (Not too useful, can use spritelocal
variables instead)
- `player` Unit's or bullet's player
- `unit_id` Unit's units.dat ID
- `order` Unit's current order ID
- `deaths(int player, int unit_id)` Death count for `player` and `unit_id`
- `upgrade(int player, int upgrade_id)` Upgrade level or `upgrade_id` for `player`
- `unit_count_any(int player, int unit_id)` Unit count of completed and incomplete units of
`unit_id` for `player`
- `unit_count_completed(int player, int unit_id)` Unit count of only completed units of `unit_id`
for `player`
- `sin(int angle_degrees`) and `cos(int angle_degrees)` Calculates sin/cos for an angle of degrees,
returning a value between -256 and 256. Note that the angle is a mathematical angle,
e.g angle 0 is towards right and angle 180 is towards left.

The following integer expressions work, but are (currently) incompatible with the Mtl plugin's
timer customization functionality.
- `stim_timer` frames of stim remaining, or 0 if the unit is not stimmed
- `ensnare_timer` frames of ensnare remaining, or 0 if the unit is not ensnared
- `maelstrom_timer` frames of maelstrom remaining, or 0 if the unit is not maelstromed
- `lockdown_timer` frames of lockdown remaining, or 0 if the unit is not lockdowned
- `stasis_timer` frames of stasis remaining, or 0 if the unit is not stasised
- `irradiate_timer` frames of irradiate remaining, or 0 if the unit is not irradiated
- `matrix_timer` frames of defensive matrix remaining, or 0 if the unit is not matrixed
- `death_timer` frames until the unit dies. Set for broodlings and hallucinations. 0 otherwise.

Additonally any variable name is an integer expression (Variables need to be have been used with
`set` at least once), and any [bw variable assignable with `set`][bw-place] is also an integer
expression.

### Bw-visible variables

These variables have a slightly different syntax from other integer expressions, and most of them
can be assigned with `set`. The unassignable ones aren't really different from the "builtin"
expressions, other than them being only in Aice an not Mtl.
Variables under `game` take 1 or 2 parameters for specifying affected player and/or relevant
ids. Those parameters can be arbitrary expressions that are evaluated every time to determine
which variable will be accessed.

- `flingy.position_x` and `flingy.position_y` NOTE: Not assignable. x/y coordinates (in pixels) of
  the flingy
- `flingy.move_target_x` and `flingy.move_target_y` The point to which a flingy is moving towards
(Equivalent to position when not moving)
- `flingy.facing_direction` Flingy's facing direction in degrees (compatible with sin/cos without
extra conversions)
- `flingy.movement_direction` Direction towards which the flingy is moving. Not equivalent to
facing direction if the a flingy is turning while having flingy.dat momentum.
- `flingy.target_direction` Direction towards which the flingy is turning, more or less the
direction in which the move target point is.
- `flingy.turn_speed` How many *BW direction units* the flingy turns in a frame. These are not
converted to degrees, but are in a 256-unit circle. The value for units is specified in flingy.dat,
bullets accelerate one unit per frame.
- `flingy.acceleration` Flingy's acceleration, starts equivalent to the flingy.dat value
- `flingy.top_speed` Flingy's top speed, starts equivalent to the flingy.dat value
- `flingy.speed` Flingy's current speed, valid also for iscript movement flingies
- `bullet.weapon_id` Bullet's weapon id
- `bullet.death_timer` Death timer for a bullet (frames). If BW decrements it to 0 the bullet
automatically dies unless it a bouncing bullet.
- `bullet.state` Bullet's state. Setting this can be used to script bullet's behaviour, but
looking at BW's code to understand the details is recommended.
    * `0` Initializes the bullet
    * `1` Bullet moves towards a point in ground
    * `2` Bullet moves towards an unit
    * `3` Bullet moves towards an unit, and bounces on hit
    * `4` Bullet targets ground and deals damage every few frames (Psi storm)
    * `5` Bullet is dying. Bullet must have state 5 at start of the current frame if `end` is used.
    * `6` Bullet is moving near an unit (Valkyrie attack)
- `bullet.bounces_remaining` Amount of bounces remaining. Only used if `bullet.state` is 3
- `bullet.order_target_x` and `bullet.order_target_y` The point which bullet is targeting
- `unit.hitpoints` Current hitpoints of the unit (256 times displayed value).
    * Setting hitpoints above maximum value makes the game freeze when damaging a building with
    damage overlays.
    * Setting hitpoints to 0 does not kill the unit, but makes the unit unkillable instead.
- `unit.shields` Current shields of the unit (256 times displayed value).
- `unit.energy` Current energy of the unit (256 times displayed value).
- `unit.max_hitpoints` Units.dat hitpoints for unit (256 times displayed value).
    * This value cannot be modified.
- `unit.max_shields` Units.dat shields for unit (256 times displayed value).
    * This value cannot be modified.
- `unit.max_energy` Max energy for unit (256 times displayed value).
    * This value cannot be modified.
- `image.frame` The latest frameset base that was specified with `playfram`, or copied from
    the primary overlay by using `followmaingraphic`, `engset`, or `engframe`.
    * This value cannot be modified with `set`, use `playfram <value>` instead.
- `image.displayed_frame` Current frame that the image is displaying (Taking direction into account).
    * This is not the frameset base which is used in `playfram` if the image has `Turning
    Graphic` set in `images.dat`.
    * This value cannot be modified with `set`. You can use `playfram` to modify the frameset base,
    which will also change the current frame depending on rotation.
- `image.drawfunc` Current images.dat draw function for image.
- `image.drawfunc_param` Current images.dat draw function parameter for image.
- `game.deaths(p, u)` Deaths of unit `u` for player `p`
- `game.kills(p, u)` Kills of unit `u` for player `p`
- `game.upgrade_level(p, u)` Upgrade level of upgrade `u` for player `p`
- `game.upgrade_limit(p, u)` Max upgrade level of upgrade `u` for player `p`
- `game.tech_level(p, t)` Tech level (Either 0 or 1) of tech `t` for player `p`
- `game.tech_availability(p, r)` Whether player `p` can research tech `t` (`1`) or not (`0`)
- `game.unit_availability(p, u)` Whether player `p` can build unit `u` (`1`) or not (`0`)
- `game.alliance(p1, p2)` Whether player `p1` is allied to `p2` (`1`) or not (`0`)
    The reverse (`p2` being allied to `p1`) is not considered.
    It is possible to make a player unally themselves, though the game may not be stable
    if that is done (Research needed).
- `game.shared_vision(p1, p2)` Whether player `p1` is sharing vision to `p2` (`1`) or not (`0`)
    The reverse (`p2` sharing vision to `p1`) is not considered.
- `game.minerals(p)` Minerals of player `p`
- `game.gas(p)` Gas of player `p`
- `game.zerg_supply_max(p)` Max zerg supply of player `p`.
- `game.zerg_supply_used(p)` Currently used zerg supply of player `p`.
- `game.zerg_supply_provided(p)` Zerg supply provided by buildings of player `p`.
- `game.terran_supply_max(p)` Max terran supply of player `p`.
- `game.terran_supply_used(p)` Currently used terran supply of player `p`.
- `game.terran_supply_provided(p)` Terran supply provided by buildings of player `p`.
- `game.protoss_supply_max(p)` Max protoss supply of player `p`.
- `game.protoss_supply_used(p)` Currently used protoss supply of player `p`.
- `game.protoss_supply_provided(p)` Protoss supply provided by buildings of player `p`.
    * **NOTE** All supply values are 2 times the displayed value as there are units that cost 0.5
    supply. (i.e. the default max supply is 400)
    * It is possible to modify supply used / provided, but it is not recommended to reduce them
    below the original values (Increasing them is fine). If the values are reduced and end up
    going below zero due to units dying, they will roll over to excessively high numbers.
- `game.location(id).left`
- `game.location(id).top`
- `game.location(id).right`
- `game.location(id).bottom`
    * Coordinates of a location `id`
- `game.units_total(p)` Total units of player `p`.
- `game.units_produced(p)` Amount of units produced by player `p`.
- `game.units_owned(p)` Amount of units owned by player `p`.
- `game.units_lost(p)` Amount of units player `p` has lost.
- `game.units_killed(p)` Amount of units player `p` has killed.
- `game.units_score(p)` Unit production score of player `p`.
- `game.units_killed_score(p)` Unit killing score of player `p`.
- `game.buildings_total(p)` Total buildings of player `p`.
- `game.buildings_constructed(p)` Amount of buildings constructed by player `p`.
- `game.buildings_owned(p)` Amount of buildings owned by player `p`.
- `game.buildings_lost(p)` Amount of buildings lost by player `p`.
- `game.buildings_razed(p)` Amount of buildings razed by player `p`.
- `game.buildings_score(p)` Building construction score of player `p`.
- `game.buildings_razed_score(p)` Building razing score of player `p`.
- `game.factories_constructed(p)` Amount of factories (Unit-producing buildings) constructed by
player `p`.
- `game.factories_owned(p)` Amount of factories owned by player `p`.
- `game.factories_lost(p)` Amount of factories lost by player `p`.
- `game.factories_razed(p)` Amount of factories razed by player `p`.
- `game.custom_score(p)` Custom score (Which can be accessed through triggers) of player `p`.
    * I don't know what's the difference between "total" and "owned" values. Maybe "owned" only
    includes units that are currently alive? Making "total" similar to "produced?
- `game.player_color_choice(p)` Color selected for player `p` in SC:R lobby. Not sure if the
    values are sensible when the map isn't configured to allow player select their color in lobby.
    Returns always 23 for single player games.
    * This value cannot be modified.
    * Values:
        * `0` Red
        * `1` Blue
        * `2` Teal
        * `3` Purple
        * `4` Orange
        * `5` Brown
        * `6` White
        * `7` Yellow
        * `8` Green
        * `9` Pale Yellow
        * `10` Tan
        * `11` Dark Aqua (Cannot be selected in lobby)
        * `12` Pale Green
        * `13` Bluish Grey
        * `14` Cyan (Cannot be selected in lobby)
        * `15` Pink
        * `16` Olive
        * `17` Lime
        * `18` Navy
        * `19` Magenta
        * `20` Grey
        * `21` Black
        * `22` Default / Random
        * `23` Map specified

Setting the following unit variables can be used to manipulate time a buff lasts for.
The time unit "step" for these variables is 8 frames (Maybe 9? Needs confirming)
It is recommended to set the timer to 1 if you want to disable the buff, as that lets
BW run its cleanup code to remove debuff overlays.
Similarly enabling the buff by setting the value to will not cause the debuff overlays to appear.
(You can imgol them manually though)
The effects are expected to behave normally even if enabled with iscript,
but nothing has been confirmed.
- `unit.stim_timer` steps of stim remaining.
- `unit.ensnare_timer` steps of ensnare remaining.
- `unit.plague_timer` steps of plague remaining.
- `unit.maelstrom_timer` steps of maelstrom remaining.
- `unit.lockdown_timer` steps of lockdown remaining.
- `unit.stasis_timer` steps of stasis remaining.
- `unit.irradiate_timer` steps of irradiate remaining.
- `unit.matrix_timer` steps of defensive matrix remaining.
- `unit.death_timer` steps until the unit dies. 0 to not make unit die.
- `unit.matrix_hitpoints` Current hitpoints for defensive matrix (256 times displayed value)

[expr]: #expressions
[bw-place]: #bw-visible-variables
