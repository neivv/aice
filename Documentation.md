# Aice

Aice is a iscript plugin which extends iscript with a few commands.

Aice doesn't use `iscript.bin`, but instead requires a complete iscript in the file
`scripts\iscript.txt` which will be compiled on startup. A starting `iscript.txt` can be generated
from decompiling entire `iscript.bin` with IceCC. PyICE has not been verified to work, and it
may have minor differences in its decompilation that makes Aice reject the text.

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
```

Jumps to `label` if the `condition` evaluates to `true`. The condition can be an arbitrary
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
returning a value between -256 and 256. Not that the angle is a mathematical angle,
e.g angle 0 is towards right and angle 180 is towards left.

Additonally any variable name is an integer expression (Variables need to be have been used with
`set` at least once), and any [bw variable assignable with `set`][bw-place] is also an integer
expression.

### Bw-visible variables

These variables have a slightly different syntax from other integer expressions, and most of them
can be assigned with `set`. The unassignable ones aren't really different from the "builtin"
expressions, other than them being only in Aice an not Mtl.

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

[expr]: #expressions
[bw-place]: #bw-visible-variables
