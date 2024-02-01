# Aice

Aice is a iscript plugin which extends iscript with new commands.

Aice doesn't use `scripts\iscript.bin`, but instead requires a complete iscript in the file
`scripts\iscript.txt` which will be compiled on startup. A starting `iscript.txt` can be generated
by decompiling the entire `iscript.bin`, preferably with IceCC.

PyICE can be used to decompile the script too, but it has some differences in output that need to be
corrected before it is accepted. Trying to load the freshly decompiled script with Aice should give
line numbers for following issues:
1. Some labels have backslashes that need to be removed (`Longbolt\Halo\GeminiMissilesTrailInit`);
2. Some twilight and desert tileset doodads get duplicate names, leading to conflicting labels. They
have to be renamed to be unique. For example, there should be 2 different `DesertDoodad`
images with same labels, other of them should be changed to use labels `DesertDoodad2Init` and
`DesertDoodad2Local00` instead.

## Miscellaneous features

### Quick reload

During the game, the script will be compiled on initial startup, but also every time a map starts,
allowing `iscript.txt` to be modified without having to restart Starcraft every time. Note that this
obviously requires using a setting that sources mod files directly from disk without compiling them
to an exe.

### iscript.bin size limits

Aice does not directly raise the limit of 65535 bytes of compiled iscript.bin, but extended commands
are mostly compiled to a separate memory buffer which makes it easier to keep the BW-visible .bin
under 65kB. If anyone ever ends up getting close to the limit with Aice, it is relatively simple to
update it to take over some regular commands, keeping the BW-visible filesize low.

### Compatibility with other plugins

Aice has not been tested with any other iscript-touching plugin, though it may work with plugins
that hook parts of Starcraft's command handling. Compatibility depends on the other plugin's hooking
methods; replacing switch jump destinations or hooking at switch cases is expected to work.
iscript.bin format extensions have to be added to Aice's compiling code, though as far as I'm aware,
no plugin extends the format of iscript.bin.

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
set spritelocal [type] <var> = <expression>
set if_uninit spritelocal [type] <var> = <expression>
set global [type] <var> = <expression>
```

Evaluates [`expression`][expr] and assigns it to a place.

The place to be assigned can be either [Bw-visible memory][bw-place] or a variable internal to Aice.
Assigning to a bw place will truncate the variable if it is larger than the memory location being
assigned to.

If the place cannot be assigned to due to it being from [another unit][other-units] that does not
exist, nothing happens.

When assigning to a variable, the variable's storage type must be declared; using different types
with a single name will be a compile error. The variable types are:

- `spritelocal` A variable that is shared between images of a single sprite;
- `if_uninit spritelocal` Only sets the spritelocal variable if it has not already been set
  somewhere else. This can be useful to set a default value if a variable may not have been
  initialized yet;
- `global` A global variable, shared between iscript of all images, regardless of player. `global`
  can be useful when wanting to move data to a child image that a following command creates, as the
  first frame of the child's iscript will execute immediately before execution continues from parent's
  iscript.

Additionally specifying `unit` after `spritelocal` or `global` makes the variable store a
reference to an unit instead of an integer, in which case `expression` must be refering to an
[another unit][other-units]. Unit variable can be explicitly reset to no unit by assigning `null`
to it.

Integers that was assigned with `set` can be evaluated later in any [expression][expr], as
expressions can use variables and BW-visible memory.

#### Examples

```
set spritelocal integer_var = 100
set spritelocal integer_var = unit.hitpoints / 256 + 600
set spritelocal unit some_unit_var = unit.target
set spritelocal unit some_unit_var = null
```

### print

```
print <text>
```

Prints text. The text can contain [expressions][expr] wrapped in `{}`, in which case they will be
evaluated and inserted in place of the expression. You do not need to add [default
expression][opt-expr] if accessing [other units][other-units]; unevaluable expressions will display
`NONE`.

Unit references itself can be printed too, in which case the unit's unique id (uid), unit id,
position, and owning player are printed. Or `NONE` if the unit reference does not exist.

You can also include color codes with `<hex>` syntax similar to SCMDraft.

If you need to have `{` or `}` in the message, you can refer to them with their hex codes `<7b>` for
`{` and `<7d>` for `}`. `<` and `>` can be escaped with `<3c>` and `<3e>`, though as long as they
aren't part of a hex code they'll be printed normally anyway.

Similarly, regular spaces after `print` and at end of the line are not included in message, but if
you use `<20>` as the first or last space character they will be included.

#### Examples

```
print hello this is a message
print current unit's hitpoints are {hitpoints / 256} while its target has {unit.target.hitpoints / 256} hitpoints
print Current unit's target is {unit.target}
print <5>Color 1, <1c>color 2
```

### fireweapon

```
fireweapon <weapon_id>
fireweapon <weapon_id> with {
    spritelocal1 = <value>
    [...]
}
```

Causes the unit to attack with a weapon. Weapon can be any [expression][expr], though most of the
time you will likely just use a constant Weapon ID that you need.

`fireweapon` behaves exactly like `castspell` except it allows specifying any weapon to be used
instead of using what was specified in `orders.dat`. There may be some minor difference between
`fireweapon` and `attack` or `attackwith`, though that has not been explicitly confirmed. The very
least they have very similar behaviour. While `useweapon` also allows specifying any weapon to used,
`useweapon` spawns the bullet on top of target regardless of its `weapons.dat` configuration, making
it less useful than `fireweapon` with certain weapons.

You can use `with {}` block to set spritelocals in the created weapon.
See [create\_unit examples][with-example] for more information.

Note that since `fireweapon` behaves as `castspell`, it will respect the rarely used weapons.dat
targeting flags, and do nothing if the targeting flags prevent attacking the current target unit.

### playfram

```
playfram <frame>
```

Aice allows specifying arbitrary [expression][expr] for `playfram` command.

Otherwise the functionality is same as in standard BW: If the image ID has `Turning Graphic` is set
in `images.dat`, the actual displayed frame will be chosen from range `frame` to `frame + 16`
depending on image's rotation.

### gotorepeatattk

```
gotorepeatattk
```

Aice overrides the original behaviour of `gotorepeatattk`, arguably fixing a bug: if a unit's
cooldown had expired before `gotorepeatattk` was used to reset its attack sequence, the unit would
only check for next attack cycle every 9th frame, causing cooldowns be surprisingly off from
what was expected.

Aice changes the behaviour so the unit immediately attacks again if its cooldown had expired before
`gotorepeatattk`. This does end up slightly reducing some cooldowns compared to unmodded game,
bringing them to what `weapons.dat` specified.

### create\_unit

```
create_unit <unit_id> <x> <y> <player>
create_unit <unit_id> <x> <y> <player> with {
    spritelocal1 = <value>
    [...]
}
```

Creates a unit for player. The unit will be spawned at the specified position, even if a ground unit
would not fit there or the area is unwalkable. All four arguments are [expressions][expr].

Spritelocal variables can be set to the newly created unit `with { }` block containing spritelocal
names and expressions to set them to.

#### create\_unit examples

```
create_unit 40 (flingy.position_x + 3) (flingy.position_y + 3) player
create_unit 40 (flingy.position_x - 3) (flingy.position_y - 3) player
```

Creates 2 broodlings where the current unit exists for the current unit's player, slightly on top of
each other. Parentheses around x/y expressions would be optional, but the are likely nicer for
readability.

```
create_unit 1 flingy.position_x flingy.position_y player with {
    something1 = 400
    # Creating unit's hitpoints
    something2 = hitpoints
    # Current player's ghost deaths divided by 4
    third_variable = game.deaths(player, 1) / 4
}
```

Creates ghost for current player, setting 3 variables to be accessed by the ghost's iscript (as if
`set spritelocal something1 = 400` had been run immediately on ghost's iscript start). Note that the
expressions are evaluated in current image's context: Value of `something2` is going to be set to
hitpoints of whatever the unit creating the ghost is, not the ghost.

Since spritelocal variables need to be written before they are accessed by any expressions, if
ghosts can be created through other ways which don't init these three variables, it is recommended
to use `set if_uninit spritelocal something = 0` to initialize the variables in ghost's init
animation.

### give\_unit

```
give_unit <unit> <player>
```

Gives a [unit][other-units] for a player. This functions like 'Give Unit' trigger action,
with same behaviour related to loaded units, interceptors, addons, etc. `player` is an
[expression][expr].

### transform

```
transform <unit> <unit_id>
```

Transforms a [unit][other-units] to new unit type. `unit_id` is an [expression][expr].


### issue\_order

```
issue_order <order_id> <x> <y>
```

Issues order for the current unit. If the unit is currently dying, you cannot issue any new orders.
`order_id`, `x` and `y` are [expressions][expr].

### call

```
call <label>
```

Calls a subroutine. Use `return` inside subroutine to continue from a point after `call`.

Unlike `call` in vanilla BW, which only allows a single call that must be returned from before using
`call` again, Aice allows nested calls up to 256 calls.

### imgul\_on / imgol\_on

```
imgul_on <unit> <image_id> <x> <y>
imgol_on <unit> <image_id> <x> <y>
```

Versions of `imgul` and `imgol` which spawn the overlay on [another unit][other-units]. Additionally
the image ID, x and y can be any [expression][expr].

Note that as a minor difference from regular `imgul` and `imgol`, which insert the overlay directly
below or above the current image executing the command, `imgul_on` and `imgol_on`
will spawn the overlay as the top/bottom overlay of the entire sprite.

If the unit does not exist, this command will silently do nothing.

## Expressions

There are two types of expressions: Integer (32-bit signed) and boolean expressions (`true` or
`false`).  Integer expressions can use basic arithmetic and comparision operators to
combine them, while booleans can only be compared with `==` and `!=`.

Arithmetic operators are `+` `-` `*` `/` and `%` (modulo), addition and subtraction will saturate at
largest/smallest possible 32-bit integer values, dividing by a expression evaluating to zero
*does not fail, but returns maximum 32-bit integer* (This may be changed in the future, please don't
rely on this), dividing by a constant zero is a compile-time error.

There are also bitwise operators `&` `|` `^` `<<` `>>` that can be used with integers. Right shift
is logical, not arithmetic (Shifting negative values right clears the sign bit).

Comparison operators are `==` `!=` `<` `<=` `>` and `>=`, a result of a integer comparison will be a boolean.

Booleans can be chained with `&&` (and) and `||` (or), mixing them always requires parentheses.

Additionally there is `default` operator, which is used for evaluating expressions that access
potentially nonexistent unit. See [Default Expressions][opt-expr] for details.

There are also several built-in expressions referring to current unit or bullet of the iscript. If
an expression refers to an object that the image doesn't have, an error will be printed and it
evaluates to something - exact behaviour is not specified.

The following are built-in boolean expressions (Available also in MTL):
- `parasited` true if the unit is parasited.
- `blind` true if the unit is blind.
- `under_storm` true if the unit has been recently damaged by psionic storm. This variable is set
  every few frames, and only set again when the storm damages the unit again, so it is not reliable
  to a frame.
- `lifted_off` true if the unit is not a landed building.
- `building_unit` true if the unit is building another unit. Applies to buildings which train their
  units, and to SCVs constructing a building.
- `in_transport` true if the unit is currently in a transport.
- `in_bunker` true if the unit is currently in a bunker.
- `carrying_powerup` true if the worker is currently carrying a powerup unit.
- `carrying_minerals` true if the worker is holding minerals (not a mineral powerup unit).
- `carrying_gas` true if the worker is holding gas (not a gas powerup unit).
- `burrowed` true if the unit is burrowed.
- `disabled` true if the unit is disabled, either by a trigger, from being an unpowered Protoss
  building, or being hit by a lockdown / stasis / maelstrom.
- `completed` true if the unit is completed.
- `self_cloaked` true if the unit is using a cloak order. False for units using permanent cloak in units.dat.
- `arbiter_cloaked` true for unburrowed units under an Arbiter.
- `cloaked` true for any unburrowed unit that is cloaked.
- `under_dweb` true if the unit is under a Disruption Web.
- `hallucination` true if the unit is an hallucination.
- `tech(int player, int tech_id)` true if `player` has reseached `tech_id`.
- `on_creep` true if the unit's center is currently on a creep tile.
- `on_unbuildable` true if the unit's center is currently on a unbuildable tile.
- `terrain_protection` true if the unit's center is on a tile giving protection (such as behind a tree doodad).

The following are Aice-specific boolean expressions:
- `sprite.has_flingy` true if the current image's parent sprite is linked to a unit or a bullet.
- `sprite.has_bullet` true if the current image's parent sprite is linked to a bullet.
- `sprite.has_unit` Equivalent to `has(unit)`.
- `has(unit_name)`
    * The expression `has(unit_name)` where `unit_name` is an [unit][other-units] or simply `unit`
      evaluates to `true` if the specified unit can be accessed.

The following are built-in integer expressions (Available also in MTL):
- `matrix_hitpoints` Current hitpoints for defensive matrix (256 times displayed value), or 0 if the
  unit is not matrixed.
- `acid_spore_count` How many stacks of Devourer's acid spores the unit has on it.
- `fighters` How many Scarabs/Interceptors the unit has in its hangar.
- `mines` How many Spider Mines a Vulture has.
- `hitpoints` Unit's current hitpoints (256 times displayed value).
- `hitpoints_percent` How many percent hitpoints the unit has left.
- `shields` Unit's current shields (256 times displayed value).
- `shields_percent` How many percent shields the unit has left.
- `energy` Unit's current energy (256 times displayed value).
- `kills` How many kills the unit has.
- `carried_resource_amount` How many resources a worker is carrying (That is, usually 8, 2, or 0).
- `frame_count` How many frames (game logic steps) the game has lasted.
- `tileset` Integer ID of the current map's tileset.
- `minerals` Mineral count of the current unit's player (does not work with bullets).
- `gas` Vespene gas count of the current unit's player (does not work with bullets).
- `ground_cooldown` How many frames of cooldown are remaining before the unit can attack again with
  its ground weapon.
- `air_cooldown` How many frames of cooldown are remaining before the unit can attack again with its air weapon.
- `spell_cooldown` How many frames of cooldown are remaining before the unit can cast another spell.
- `speed` The unit's current speed, in flingy.dat units but also applicable to units using iscript movement.
- `sigorder` Iscript sigorder value for the current unit (not too useful, can use spritelocal variables instead).
- `player` Unit's or bullet's player.
- `unit_id` Unit's units.dat ID.
- `order` Unit's current order ID.
- `deaths(int player, int unit_id)` Death count for `player` and `unit_id`.
- `upgrade(int player, int upgrade_id)` Upgrade level for `upgrade_id` for `player`.
- `unit_count_any(int player, int unit_id)` Unit count of completed and incomplete units of `unit_id` for `player`.
- `unit_count_completed(int player, int unit_id)` Unit count of only completed units of `unit_id` for `player`.
- `sin(int angle_degrees`), `cos(int angle_degrees)`, `tan(int angle_degrees)`:
    * Calculates sin/cos/tan for an angle of degrees. Since the value returned for sin/cos is
      between -1 and 1, and these expressions cannot represent that non-integers, the returned value
      is multiplied by 256, giving a value between -256 and 256. The returned value of `tan(x)` is
      similarly multiplied by 256, though it can obviously go excessively high as the angle approaches 90.
      Note that the angle is a mathematical angle, e.g angle 0 is towards right and angle 180 is towards
      left.
- `asin(int x)`, `acos(int x)`, `atan(int x)`:
    * Inverse functions of `sin(x)`, `cos(x)`, `tan(x)`. Input has to be again multiplied by 256
      since decimal numbers aren't supported. `asin(x)` returns degrees between -90 to 90, `acos(x)`
      and `atan(x)` return degrees between 0 to 180.
- `min(int a, int b)` Returns smaller of `a` and `b`.
- `max(int a, int b)` Returns greater of `a` and `b`.
- `clamp(int min, int x, int max)` Returns `x` if `min < x < max`, otherwise if returns `min` if `x`
  is less than `min`, and returns `max` if `x` is greater than `max`.
- `tile_height` The terrain height of tile the unit is on (0 = low, 1 = middle, 2 = high).
    * This is slightly inaccurate for ramp tiles which have mixed height.
- `dat(int table, int field, int entry)` Reads a value from .dat files. If it refers to a value that
  is customizable in the editor (e.g. unit hitpoints), it will read from the map's actual value.
    * The first argument `table` selects the dat table:
        - `0` `units.dat`
        - `1` `weapons.dat`
        - `2` `flingy.dat`
        - `3` `sprites.dat`
        - `4` `images.dat`
        - `5` `orders.dat`
        - `6` `upgrades.dat`
        - `7` `techdata.dat`
        - `8` `sfxdata.dat`
        - `9` `portdata.dat`
        - `10` `buttons.dat`
    * The second argument `field` selects the stat, see [Dat table fields][dat-fields] for the
      listing of most values.
    * The third argument `entry` selects the unit / weapon / sprite / etc. index to be read.
    * For example, to check if the current unit is mechanical, use:
        `dat(0, 0x16, unit_id) & 0x40000000 != 0`.

The following integer expressions work, but are (currently) incompatible with the MTL plugin's timer
customization functionality.
- `stim_timer` frames of stim remaining, or 0 if the unit is not stimmed.
- `ensnare_timer` frames of ensnare remaining, or 0 if the unit is not ensnared.
- `maelstrom_timer` frames of maelstrom remaining, or 0 if the unit is not maelstrommed.
- `lockdown_timer` frames of lockdown remaining, or 0 if the unit is not locked down.
- `stasis_timer` frames of stasis remaining, or 0 if the unit is not in stasis.
- `irradiate_timer` frames of irradiate remaining, or 0 if the unit is not irradiated.
- `matrix_timer` frames of defensive matrix remaining, or 0 if the unit is not matrixed.
- `death_timer` frames until the unit dies. Set for broodlings and hallucinations. 0 otherwise.

Additionally any variable name is an integer expression (Variables need to be have been used with
`set` at least once), and any [BW variable assignable with `set`][bw-place] is also an integer
expression.

### BW-visible variables

These variables have a slightly different syntax from other integer expressions, and most of them
can be assigned with `set`. The unassignable ones aren't really different from the "built-in"
expressions, other than them being only in Aice and not MTL. Variables under `game` take 1 or 2
parameters for specifying affected player and / or relevant IDs. Those parameters can be arbitrary
expressions that are evaluated every time to determine which variable will be accessed.

- `flingy.position_x` and `flingy.position_y` NOTE: Not assignable. x / y coordinates (in pixels) of the flingy.
- `flingy.move_target_x` and `flingy.move_target_y` The point towards which a flingy is moving
  (equivalent to position when not moving).
- `flingy.facing_direction` Flingy's facing direction in degrees (compatible with sin / cos without
  extra conversions).
- `flingy.movement_direction` Direction towards which the flingy is moving. Not equivalent to facing
  direction if the a flingy is turning while having flingy.dat momentum.
- `flingy.target_direction` Direction towards which the flingy is turning, more or less the
  direction in which the move target point is.
- `flingy.turn_speed` How many *BW direction units* the flingy turns in a frame. These are not
  converted to degrees, but are in a 256-unit circle. The value for units is specified in
  flingy.dat,
bullets accelerate one unit per frame.
- `flingy.acceleration` Flingy's acceleration, starts equivalent to the flingy.dat value.
- `flingy.top_speed` Flingy's top speed, starts equivalent to the flingy.dat value.
- `flingy.speed` Flingy's current speed, valid also for iscript movement flingies.
- `flingy.player` Flingy's player.
- `flingy.flingy_id` Flingy's flingy.dat ID:
    * This value can be modified, though exact effects are not known.
- `flingy.movement_flags` Flags related to flingy's state.
    * 0x1 Turning
    * 0x2 Moving
    * 0x4 Braking (?)
    * 0x8 Attacking
    * 0x10 Has path? Has complex path?
    * 0x20 Don't slow down at move target (When move waypoints are shift-queued)
    * 0x40 Short path?
    * 0x80 ?
- `bullet.weapon_id` Bullet's weapons.dat ID.
- `bullet.death_timer` Death timer for a bullet (frames). If BW decrements it to 0 the bullet
  automatically dies unless it is a bouncing bullet.
- `bullet.state` Bullet's state. Setting this can be used to script bullet's behaviour, but looking
  at BW's code to understand the details is recommended.
    * `0` Initializes the bullet;
    * `1` Bullet moves towards a point in ground;
    * `2` Bullet moves towards a unit;
    * `3` Bullet moves towards a unit, and bounces on hit;
    * `4` Bullet targets ground and deals damage every few frames (Psi storm);
    * `5` Bullet is dying. Bullet must have state 5 at start of the current frame if `end` is used;
    * `6` Bullet is moving near a unit (Valkyrie attack).
- `bullet.bounces_remaining` Amount of bounces remaining. Only used if `bullet.state` is 3.
- `bullet.order_target_x` and `bullet.order_target_y` The point which bullet is targeting.
- `bullet.flags` Flags controlling bullet's behaviour
    * 0x1 Will miss (No damage to target with non-splash behaviours).
        - Note that when the bullet is set to miss, `order_target_x` and `order_target_y` are
        offset as well to make the bullet appear at a different point.
    * 0x2 Hallucination.
- `unit.kills` How many kills the unit has.
- `unit.carried_resource_amount` How many resources a worker is carrying (That is, usually 8, 2, or 0).
- `unit.ground_cooldown` How many frames of cooldown are remaining before the unit can attack again
  with its ground weapon.
- `unit.air_cooldown` How many frames of cooldown are remaining before the unit can attack again
  with its air weapon.
- `unit.spell_cooldown` How many frames of cooldown are remaining before the unit can cast another spell.
- `unit.speed` The unit's current speed, in flingy.dat units but also applicable to units using
  iscript movement variables instead).
- `unit.unit_id` Unit's units.dat ID.
    * This value cannot be modified.
- `unit.hitpoints` Current hitpoints of the unit (256 times displayed value).
    * Setting hitpoints above maximum value makes the game freeze when damaging a building with damage overlays.
    * Setting hitpoints to 0 does not kill the unit, but makes the unit unkillable instead.
- `unit.shields` Current shields of the unit (256 times displayed value).
- `unit.energy` Current energy of the unit (256 times displayed value).
- `unit.max_hitpoints` Units.dat hitpoints for unit (256 times displayed value).
    * This value cannot be modified.
- `unit.max_shields` Units.dat shields for unit (256 times displayed value).
    * This value cannot be modified.
- `unit.max_energy` Max energy for unit (256 times displayed value).
    * This value cannot be modified.
- `unit.mineral_cost` Mineral cost of the unit.
    * This value cannot be modified.
- `unit.gas_cost` Gas cost of the unit.
    * This value cannot be modified.
- `unit.supply_cost` Supply cost of the unit (2 times displayed value):
    * This value cannot be modified.
- `unit.overlay_size` Overlay size specified in units.dat flags. 0 = Small, 1 = Medium, 2 = Large.
    * This value cannot be modified.
- `unit.resources` Amount of resources in a resource container unit, 0 if the unit isn't a resource.
    * Modifying this value does not update the displayed frame of a mineral field; if that is
      desired, you'll have to add in the logic yourself.
    * Modifying this value for a unit which is not a resource does nothing. (Trying to read it back
      will still evaluate it to 0)
- `unit.hangar_count_inside` Amount of fighters (Scarabs / Interceptors) *hidden inside* this unit.
    * This value cannot be modified.
- `unit.hangar_count_outside` Amount of Interceptors attached to this Carrier outside the Carrier.
    * This value cannot be modified.
    * Scarabs become detached from a reaver when launched, and aren't counted by this. (Or do they? Should verify.)
- `unit.loaded_count` Amount of units the transport (or bunker) is carrying.
    * This value cannot be modified.
- `unit.current_upgrade` Upgrade ID of the upgrade that is being researched by a building, 61 if an
  upgrade isn't being researched.
    * This value cannot be modified.
- `unit.current_tech` Tech ID of the tech that is being researched by a building, 44 if a tech isn't
  being researched.
    * This value cannot be modified.
- `unit.build_queue(n)` Unit ID of nth unit that is currently being built by this unit. 228 when the
  queue slot is not used.
    * The index is 0-based, so `unit.build_queue(0)` returns the unit ID of the unit being currently
      built (Or being queued once supply is available), `unit.build_queue(1)` is for the second
      unit, up to `unit.build_queue(4)`.
    * If the unit is currently morphing, the first slot of queue has the ID of the unit type being morphed into.
    * If the unit is building an addon, the first slot of the queue has addon's unit ID.
    * A worker moving towards to construct a building has building's ID in the first slot of queue.
        - However, the building ID will stay in queue if the worker is given another order, so it
          alone should not be relied on to check that the unit is building something.
    * This value can be modified, though it may cause instability if the first slot is changed when
      a unit is being built.
- `unit.remaining_build_time` Frames until *this unit* is complete.
    * For flags, frames until the flag gets returned to flag beacon.
    * This value can be modified, though it can cause unit to finish without full HP.
- `unit.remaining_research_time` Frames until the upgrade / tech research is complete. 0 if no
  research is in progress.
- `unit.order` Unit's current order ID
    * This value cannot be modified. `issue_order` can be used instead.
- `unit.order_state` Value that may change depending on what part of the order unit is. For example,
  some spell orders use 0 for first frame, 1 for "moving to range" part, and 2 for casting the
spell.
- `unit.order_timer` Timer value that current order may use. Meaning depends on current order.
- `unit.rank_increase` Allows increasing (Maybe also decreasing?) unit's rank from units.dat value.
  This seems to be unused functionality that may still work.
- `unit.mine_amount` Spider mines remaining for vulture units. Note that if the tech isn't usable,
  reading this value returns 0, but writing this value sets mine amount once tech becomes usable.
- `unit.rally_x`, `unit.rally_y` Rally point coordinates.
    * If both are 0 then there is no rally point (Can't rally to topleftmost pixel of the map).
    * If the rally was set to a unit, these coordinates stay at the value the unit's position was at
      when the rally was set if unit moves, and will take effect only after unit dies / goes out of
      player's vision.
    * Pylons and non-buildings cannot have a rally point.
- `unit.flags` Flags related to individual unit's state. To check if e.g. a flag 0x4 is set for a
  unit, use `if unit.flags & 0x4 != 0` (== 1 won't work).
  Modifying flags is allowed, but in most situations it does nothing, or freezes units in weird ways.
  Note that many flags are just "raw" values for states that can be evaluated through built-in boolean
  expressions mentioned above.
    * 0x1 Completed. Unsetting it freezes units and gives buildings the visually filled progress
      bar, but they never go out of this state.
    * 0x2 Landed building.
    * 0x4 Air. Can be set on units, and makes them start being targeted with air weapons (so immune
      to attack by melee units and to spells like spawn broodling), but not actually flying.
    * 0x8 Disabled
    * 0x10 Burrowed
    * 0x20 In building
    * 0x40 In transport
    * 0x80 Unknown, AI targeting-related?
    * 0x100 Begin invisibility
    * 0x200 Invisibility done
        - These two flags, when set on units, make them cloak in a hacky way; initially, they remain
          visible but cannot be selected / right-clicked anymore until you "detect" them (by
          scanning / bringing a detector unit nearby, at which point they will become properly
          cloaked (translucent). If the scan runs out / the detector leaves, they vanish into
          invisibility, but the shadow remains for units that had it (and follows around).
    * 0x400 Disabled. When set, disables unit without playing the relevant animation (if any),
      making them stuck on whatever they were doing. Protoss buildings that are disabled this way
      become enabled again when a new pylon is built nearby, even if these were buildings that did not
      require power (e.g. a nexus or a pylon).
    * 0x800 Energy-free invisibility (Arbiter cloak or burrowing)
    * 0x1000 Uninterruptable order
    * 0x2000 nobrkcodestart active
    * 0x4000 Has creep that should disappear (Only set on dead units when lot of creep is disappearing)
    * 0x8000 Under disruption web (BW recalculates this each frame; writing from iscript may end up
      being useless if the recalculation code is ran between write and unit attacking)
    * 0x00010000 Can turn?
    * 0x00020000 Reacts? "Intelligent"?
    * 0x00040000 Unstacking
    * 0x00080000 Movement target updated?
    * 0x00100000 Unit collision
    * 0x00200000 No collision. This flag can be set and causes other units to become able to move
      through this one, but it cannot move through other units.
    * 0x00400000 Unknown collision flag
    * 0x00800000 Harvesting collision.
      This flag can be set and causes the unit to able to move through others, like a
      worker when harvesting.
    * 0x01000000 Subunit turret moving?
    * 0x02000000 Subunit turret following parent?
    * 0x04000000 Invincible. This flag can be set, having the same effect as "Set invincibility"
      trigger action. It will also be set as part of Stasis Field's effect.
    * 0x08000000 Attack move
    * 0x10000000 Has movement speed upgrade. This flag can be set and have the desired effect.
    * 0x20000000 Has attack speed upgrade. This flag can be set and have the desired effect.
    * 0x40000000 Hallucination. Without additional manipulations, works only part of the expected
      way. The unit will not expire unless `unit.death_timer` is set to a nonzero value as well,
      and the unit doesn't become instakillable with spells, but starts dealing 0 damage and
      vanishes when killed.
    * 0x80000000 Suiciding

To set them, use e.g. `set unit.flags = unit.flags | 0x00200000` (to disable collision in this case).
There are three ways to unset a flag:
    * Use e.g. `set unit.flags = unit.flags & 0xffdfffff` (a hexadecimal value with the flag's own
      value, 0x200000, subtracted);
    * Use a set followed by a toggle, e.g. `set unit.flags = (unit.flags | 0x00200000) ^ 0x00200000`;
    * Or, use e.g. `set unit.flags = unit.flags & !0x00200000`, which appears to be the most
      straightforward way.

- `unit.detection_status` Flags signifying which players are able to detect this unit. Can be
  written but BW will most likely quickly write the normal value again, making writing pretty
  useless.
    0x1 = Player 1, 0x2 = Player 2, 0x4 = Player 3, etc. to 0x80 = Player 8. 0x80000000 means that
    the detection status is out of date and needs updating.
- `unit.pathing_flags` Flags related to pathing. Not sure if these are useful at all.
    * 0x1 Collides?
    * 0x2 Unstacking?
    * 0x4 Is stacked?
- `unit.movement_state` An integer specifying in which of various different states that the pathing
    system uses the unit is in.
- `image.frame` The latest frameset base that was specified with `playfram`, or copied from the
  primary overlay by using `followmaingraphic`, `engset`, or `engframe`.
    * This value cannot be modified with `set`, use `playfram <value>` instead.
- `image.displayed_frame` Current frame that the image is displaying (Taking direction into account).
    * This is not the frameset base which is used in `playfram` if the image has `Turning Graphic`
      set in `images.dat`.
    * This value cannot be modified with `set`. You can use `playfram` to modify the frameset base,
      which will also change the current frame depending on rotation.
- `image.drawfunc` Current images.dat draw function for image.
- `image.drawfunc_param` Current images.dat draw function parameter for image.
- `game.deaths(p, u)` Deaths of unit `u` for player `p`.
- `game.kills(p, u)` Kills of unit `u` for player `p`.
- `game.upgrade_level(p, u)` Upgrade level of upgrade `u` for player `p`.
- `game.upgrade_limit(p, u)` Max upgrade level of upgrade `u` for player `p`.
- `game.tech_level(p, t)` Tech level (Either 0 or 1) of tech `t` for player `p`.
- `game.tech_availability(p, r)` Whether player `p` can research tech `t` (`1`) or not (`0`).
- `game.unit_availability(p, u)` Whether player `p` can build unit `u` (`1`) or not (`0`).
- `game.alliance(p1, p2)` Whether player `p1` is allied to `p2` (`1`) or not (`0`).
    * The reverse (`p2` being allied to `p1`) is not considered.
    * It is possible to make a player unally themselves, which will make the affected player's units
      attack each other and in some cases themselves, down to suiciding (with whichever weapon they
      have).
- `game.shared_vision(p1, p2)` Whether player `p1` is sharing vision to `p2` (`1`) or not (`0`).
    * The reverse (`p2` sharing vision to `p1`) is not considered.
- `game.minerals(p)` Minerals of player `p`.
- `game.gas(p)` Gas of player `p`.
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
      supply. (i.e. the default max supply is 400).
    * It is possible to modify supply used / provided, but it is not recommended to reduce them
      below the original values (Increasing them is fine). If the values are reduced and end up
      going below zero due to units dying, they will roll over to excessively high numbers.
- `game.location(id).left`
- `game.location(id).top`
- `game.location(id).right`
- `game.location(id).bottom`:
    * Coordinates of a location `id`.
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
- `game.factories_constructed(p)` Amount of factories (Unit-producing buildings) constructed by player `p`.
- `game.factories_owned(p)` Amount of factories owned by player `p`.
- `game.factories_lost(p)` Amount of factories lost by player `p`.
- `game.factories_razed(p)` Amount of factories razed by player `p`.
- `game.custom_score(p)` Custom score (Which can be accessed through triggers) of player `p`.
    * I don't know what's the difference between "total" and "owned" values. Maybe "owned" only
      includes units that are currently alive? Making "total" similar to "produced?
- `game.leaderboard_type` Leaderboard type. These values are trigger action IDs.
    * `0` No leaderboard
    * `17` Control
    * `18` Control at location
    * `19` Resources
    * `20` Kills
    * `21` Points
    * `33` Control with goal
    * `34` Control at location with goal
    * `35` Resources with goal
    * `36` Kills with goal
    * `37` Points with goal
    * `40` Greed
- `game.leaderboard_location` Location ID for Control at location leaderboard
- `game.leaderboard_parameter` Leaderboard parameter, meaning depends on `game.leaderboard_type`
    * For Control / Kill leaderboards, this is Unit ID
    * For Resource leaderboards:
        * `0` = Minerals
        * `1` = Gas
        * `2` = Both
    * For Points leaderboards:
        * `0` = Total
        * `1` = Units
        * `2` = Buildings
        * `3` = Units and Buildings
        * `4` = Kills
        * `5` = Razings
        * `6` = Kills and Razings
        * `7` = Custom
- `game.leaderboard_goal` Leaderboard goal, if using a leaderboard type which has a goal
- `game.leaderboard_computers` `0` to disable computers in leaderboard, `1` to enable.
- `game.player_color_choice(p)` Color selected for player `p` in SC:R lobby. Not sure if the values
  are sensible when the map isn't configured to allow player select their color in lobby. Always
  returns 23 for single player games.
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
        * `23` Map specified.

Setting the following unit variables can be used to manipulate time a buff lasts for. The time unit
"step" for these variables is 8 frames (Maybe 9? Needs confirming). It is recommended to set the
timer to 1 if you want to disable the buff, as that lets BW run its cleanup code to remove debuff
overlays. Similarly enabling the buff by setting the value to will not cause the debuff overlays to
appear (you can `imgol` them manually though). The effects are expected to behave normally even if
enabled with iscript, but nothing has been confirmed.
- `unit.stim_timer` steps of stim remaining.
- `unit.ensnare_timer` steps of ensnare remaining.
- `unit.plague_timer` steps of plague remaining.
- `unit.maelstrom_timer` steps of maelstrom remaining.
- `unit.lockdown_timer` steps of lockdown remaining.
- `unit.stasis_timer` steps of stasis remaining.
- `unit.irradiate_timer` steps of irradiate remaining.
- `unit.matrix_timer` steps of defensive matrix remaining.
- `unit.death_timer` steps until the unit dies. 0 to not make unit die.
- `unit.matrix_hitpoints` Current hitpoints for defensive matrix (256 times displayed value).
- `unit.spell_update_timer` Frames until next step of decrementing all spell-specific timers by 1.
- `unit.parasite_players` Is nonzero if the unit has parasite.
    * Bit 0x1 = First player, 0x2 = Second, 0x4 = Third, etc.
- `unit.is_blind` Is nonzero if the unit has been blinded.
    * Does track blinding player like parasite does, even if the game doesn't use that
      functionality.
- `unit.is_being_healed` Used to prevent multiple medics from healing a single unit.
    * This is set to 0 on every frame during unit timer handling (before the iscript animation of
        the unit is ran), and to 1 once a medic with heal order heals the unit.
- `unit.is_under_storm` Gives temporary psi storm immunity
    * Game sets this to 1 after being damaged by psi storm.
    * Game zeroes this after few frames. (At next `spell_update_timer` check?)

### Other units

In addition to just prefixing `unit.` or `flingy.` to access the current unit's variables, the
following names can be used to access other unit's, such as the current target as `unit.target`. Any
objects accessible from `unit.` can be chained if wished, such as`bullet.target.target.addon`.

Additionally an variables that were assigned with [`unit` type][set-opcode] can be used in any point
of chain, e.g. `spritelocal_variable1.target.addon.spritelocal_variable2` as an extreme example.

Note that any expression using these other units [needs to provide a default value][opt-expr] when
the other unit is missing.

Additionally any `spritelocal` variable names can be used to resolve the value of the `spritelocal`
for the referenced unit's sprite. For example `unit.target.some_variable` can be used to read
`some_variable` from unit's target. Note that when accessing `spritelocal`s from other units, an
uninitialized `spritelocal` will also cause the default value to be executed.

That is, `6 + unit.target.some_variable default -1` will evaluate to -1 if either `unit.target` does
not exist, or if target has not set `some_variable` to any value. This is different from plain
`spritelocal` access (`6 + some_variable`) which will print an error if the `some_variable` has not
bee initialized.

If you want to check if a unit exists, for example to jump depending on that, `has(unit_name)`
boolean expression can be used.

The following units can be accesed:

- `unit.target`
    * Target unit for the current order being executed.
- `unit.parent`
    * For a Larva, the Hatchery.
    * For a Nuke not being nuked, the Nuclear Silo it is in.
        * Nuke being nuked is not linked to a Ghost, even if the Ghost is to the Nuke.
    * For an addon, the main building.
    * For a subunit turret, the base unit.
    * For an Interceptor/Scarab, the parent Carrier/Reaver.
    * For a powerup, the worker that is carrying it.
- `unit.nuke`
    * For a nuking Ghost, the Nuke.
        * The nuke stays attached to ghost while it is flying out of the silo and being hidden,
        but is no longer attached once it appears above the nuke dot.
    * For a Nuclear Silo, the stored nuke once it's been completed.
- `unit.currently_building`
    * or a SCV, the building it is currently constructing.
    * For any production building, the hidden unit that is being trained.
    * For a Terran building constructing addon, the addon.
    * For a nuke silo, the nuke that is being constructed.
    * For a nydus canal building exit, the exit unit.
- `unit.transport`
    * For units inside a transport or Bunker, the said transport/Bunker.
- `unit.addon`
    * For Terran buildings, a completed addon that is attached to self.
- `unit.subunit`
    * The turret subunit for Tank/Goliath bases.
- `unit.linked_nydus`
    * The other side of a nydus canal link.
- `unit.powerup`
    * A carried powerup for a worker.
- `unit.rally_target`
    * The unit set as a rally point target.
- `unit.irradiated_by`
    * The Science Vessel that has used irradiate on self.
- `bullet.parent`
    * The unit which spawned this bullet.
- `bullet.target`
    * Target unit of a bullet.
- `bullet.previous_bounce_target`
    * The previous target that a bouncing bullet hit.

### Default expressions

When using other units in an expression, such as `unit.target.hitpoints + 256`, the expression is
not possible to be evaluated if the unit currently has no target. Any such expressions need to
provide a default value as a fallback to use when the units do not exist, using the special
`default` operator.

The syntax for specifying the default expression is `<expression> default <expression>`, where the
expression on the left is tried to be evaluated, and the expression on the right will be evaluated
if the one on the left fails. The fallback expression may access potentially unavailable units, in
which case an additional `default` has to be used to handle the case where the fallback expression
cannot be evaluated either.

Examples:
* Read unit's target's hitpoints, or evaluate to 0 if target does not exist
```
unit.target.hitpoints default 0
```
* If parentheses are not used, default takes entire expression on the left and right;
    The following are equivalent.
```
5 + unit.target.hitpoints + unit.addon.hitpoints default 5 + 6 + 7
(5 + unit.target.hitpoints + unit.addon.hitpoints) default (5 + 6 + 7)
```
* Check if unit's target has more than 100 HP, defaulting to false when there is no target.
    Two different but equivalent ways.
```
(unit.target.hitpoints default 0) > 256 * 100
unit.target.hitpoints > 256 * 100 default false
```
* Evaluates to `256 + hitpoints` for first of the following units that exists
    1) Unit's target's addon
    2) Unit's target
    3) Unit itself
```
(unit.target.addon.hitpoints default unit.target.hitpoints default unit.hitpoints) + 256
```
* Similar to above, but only adds 256 for the first two cases and subtracts 256 in the third case
```
(unit.target.addon.hitpoints default unit.target.hitpoints) + 256 default unit.hitpoints - 256
```

### Dat table fields

```
Units.dat:
- 0x00 Flingy
- 0x01 Subunit
- 0x02 Subunit 2
- 0x03 Infestation
- 0x04 Construction image
- 0x05 Direction
- 0x06 Has shields
- 0x07 Shields
- 0x08 Hitpoints
- 0x09 Elevation level
- 0x0a Floating
- 0x0b Rank
- 0x0c Ai idle order
- 0x0d Human idle order
- 0x0e Return to idle order
- 0x0f Attack unit order
- 0x10 Attack move order
- 0x11 Ground weapon
- 0x12 Ground weapon hits
- 0x13 Air weapon
- 0x14 Air weapon hits
- 0x15 AI flags
- 0x16 Flags
    0x1 Building
    0x2 Addon
    0x4 Flyer
    0x8 Worker
    0x10 Subunit
    0x20 Flying Building
    0x40 Hero
    0x80 Regenerate
    0x100 Clickable Overlays
    0x200 Cloakable
    0x400 2 Units in 1 Egg
    0x800 Powerup
    0x1000 Resource Depot
    0x2000 Resource Container
    0x4000 Robotic
    0x8000 Detector
    0x00010000 Organic
    0x00020000 Requires Creep
    0x00040000 Unknown 18
    0x00080000 Requires Psi
    0x00100000 Burrowable
    0x00200000 Spellcaster
    0x00400000 Permanent Cloak
    0x00800000 Unknown 23
    0x01000000 Ignore Supply Check
    0x02000000 Use Medium Overlays
    0x04000000 Use Large Overlays
    0x08000000 Intelligent
    0x10000000 Full Auto-Attack
    0x20000000 Invincible
    0x40000000 Mechanical
    0x80000000 Unknown 31
- 0x17 Target acquisition range
- 0x18 Sight range
- 0x19 Armor upgrade
- 0x1a Armor type
- 0x1b Armor
- 0x1c Rclick action
- 0x1d Ready sound
- 0x1e First what sound
- 0x1f Last what sound
- 0x20 First annoyed sound
- 0x21 Last annoyed sound
- 0x22 First yes sound
- 0x23 Last yes sound
- 0x24 Placement box
- 0x25 Addon position
- 0x26 Dimension box
- 0x27 Portrait
- 0x28 Mineral cost
- 0x29 Gas cost
- 0x2a Build time
- 0x2b Datreq offset
- 0x2c Group flags
    0x1 Zerg
    0x2 Terran
    0x4 Protoss
    0x8 Men
    0x10 Building
    0x20 Factory
    0x40 Independent
    0x80 Neutral
- 0x2d Supply provided
- 0x2e Supply cost
- 0x2f Space required
- 0x30 Space provided
- 0x31 Build score
- 0x32 Kill score
- 0x33 Map label
- 0x34 ???
- 0x35 Misc flags

Weapons.dat:
- 0x00 Label
- 0x01 Flingy
- 0x02 ???
- 0x03 Flags
- 0x04 Min range
- 0x05 Max range
- 0x06 Upgrade
- 0x07 Damage type
- 0x08 Behaviour
- 0x09 Death time
- 0x0a Effect
- 0x0b Inner splash
- 0x0c Middle splash
- 0x0d Outer splash
- 0x0e Damage
- 0x0f Upgrade bonus
- 0x10 Cooldown
- 0x11 Factor
- 0x12 Attack angle
- 0x13 Launch spin
- 0x14 X offset
- 0x15 Y offset
- 0x16 Error msg
- 0x17 Icon

Flingy.dat:
- 0x00 Sprite ID
- 0x01 Top speed
- 0x02 Acceleration
- 0x03 Halt distance
- 0x04 Turn speed
- 0x05 Unused
- 0x06 Movement type

Sprites.dat:
- 0x00 Image
- 0x01 Healthbar
- 0x02 Unknown2
- 0x03 Start as visible
- 0x04 Selection circle
- 0x05 Image pos

Images.dat:
- 0x00 Grp
- 0x01 Can turn
- 0x02 Clickable
- 0x03 Full iscript
- 0x04 Draw if cloaked
- 0x05 Drawfunc
- 0x06 Remapping
- 0x07 Iscript header
- 0x08 Overlay
- 0x09 Overlay
- 0x0a Damage Overlay
- 0x0b Special Overlay
- 0x0c Landing Overlay
- 0x0d Liftoff Overlay

Upgrades.dat:
- 0x00 Mineral cost
- 0x01 Mineral factor
- 0x02 Gas cost
- 0x03 Gas factor
- 0x04 Time cost
- 0x05 Time factor
- 0x06 Dat req offset
- 0x07 Icon
- 0x08 Label
- 0x09 Race
- 0x0a Repeat count
- 0x0b Brood war

Techdata.dat:
- 0x00 Mineral cost
- 0x01 Gas cost
- 0x02 Time cost
- 0x03 Energy cost
- 0x04 Dat req research offset
- 0x05 Dat req use offset
- 0x06 Icon
- 0x07 Label
- 0x08 Unk?
- 0x09 Misc?
- 0x0a Brood War

Portdata.dat:
- 0x00 Idle path
- 0x01 Talking path
- 0x02 Idle SMK change
- 0x03 Talking SMK change
- 0x04 Idle unknown
- 0x05 Talking unknown

Orders.dat:
- 0x00 Label
- 0x01 Use weapon targeting
- 0x02 Secondary order (unused)
- 0x03 Non-subunit (unused)
- 0x04 Subunit inherits
- 0x05 Subunit can use (unused)
- 0x06 Interruptable
- 0x07 Stop moving before next queued
- 0x08 Can be queued
- 0x09 Keep target while disabled
- 0x0a Clip to walkable terrain
- 0x0b Fleeable
- 0x0c Requires moving (unused)
- 0x0d Order weapon
- 0x0e Order tech
- 0x0f Animation
- 0x10 Icon
- 0x11 Requirement offset
- 0x12 Obscured order
```

[expr]: #expressions
[bw-place]: #bw-visible-variables
[other-units]: #other-units
[opt-expr]: #default-expressions
[dat-fields]: #dat-table-fields
[with-example]: #create_unit-examples
[set-opcode]: #set
