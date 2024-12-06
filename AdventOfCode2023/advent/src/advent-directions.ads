package Advent.Directions is

   pragma Pure;

   type Direction is
     (North,
      East,
      South,
      West,
      North_East,
      South_East,
      South_West,
      North_West);

   subtype Cardinal_Direction is Direction range North .. West;

   subtype Ordinal_Direction is Direction range North_East .. North_West;

   --  Returns the opposite Direction from Dir. The conversions are:
   --
   --  * North      => South
   --  * North_East => South_West
   --  * East       => West
   --  * South_East => North_West
   --  * South      => North
   --  * South_West => North_East
   --  * West       => East
   --  * North_West => South_East
   function Opposite (Dir : Direction) return Direction;

   --  Rotates a Direction 90 degrees counter-clockwise. The conversion is as follows:
   --
   --  * North      => West
   --  * North_East => North_West
   --  * East       => North
   --  * South_East => North_East
   --  * South      => East
   --  * South_West => South_East
   --  * West       => South
   --  * North_West => South_West
   function Rotate_CCW_90 (Dir : Direction) return Direction;

   --  Rotates a Direction 90 degrees clockwise. The conversion is as follows:
   --
   --  * North      => East
   --  * North_East => South_East
   --  * East       => South
   --  * South_East => South_West
   --  * South      => West
   --  * South_West => North_West
   --  * West       => North
   --  * North_West => North_East
   function Rotate_CW_90 (Dir : Direction) return Direction;

   --  Returns the Direction perpendicular to Dir. This is an alias to one of
   --  the 90 degree rotation functions, but is provided because the name can
   --  express intent more clearly.
   function Perpendicular (Dir : Direction) return Direction renames Rotate_CW_90;

   type Direction_Flags is array (Direction) of Boolean;
   pragma Pack (Direction_Flags);

   function Rotate_Left (Dir : Cardinal_Direction) return Cardinal_Direction renames Rotate_CCW_90;
   pragma Obsolescent (Rotate_Left, "Use Rotate_CCW_90");

   function Rotate_Right (Dir : Cardinal_Direction) return Cardinal_Direction renames Rotate_CW_90;
   pragma Obsolescent (Rotate_Right, "Use Rotate_CW_90");

   Invalid_Character : exception;

   --  Parses a single character as a direction using typical Advent of Code
   --  conventions. The accepted characters for each direction are:
   --
   --  North: N, n, U, u, ^
   --  South: S, s, D, d, v
   --  West:  W, w, L, l, <
   --  East:  E, e, R, r, >
   --
   --  Raises Invalid_Character if C is not one of the characters above. Note
   --  that this function only parses Cardinal_Directions.
   function Parse_Direction (C : Character) return Cardinal_Direction;

end Advent.Directions;
