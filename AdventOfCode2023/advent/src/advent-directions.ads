package Advent.Directions is

   pragma Pure;

   type Direction is
     (North,
      North_East,
      East,
      South_East,
      South,
      South_West,
      West,
      North_West);

   subtype Cardinal_Direction is Direction
   with Static_Predicate => Cardinal_Direction in North | East | South | West;

   subtype Ordinal_Direction is Direction
   with
     Static_Predicate =>
       Ordinal_Direction in North_East | South_East | South_West | North_West;

   --  Returns the opposite Direction from Dir. The conversions are:
   --
   --  * North => South
   --  * South => North
   --  * West => East
   --  * East => West
   function Opposite (Dir : Direction) return Direction;

   --  Returns the Direction perpendicular to Dir. The value returned will
   --  always be the perpendicular formed rotating the direction clockwise by
   --  90 degrees. The conversion table is as follows:
   --
   --  * North      => East
   --  * North_East => South_East
   --  * East       => South
   --  * South_East => South_West
   --  * South      => West
   --  * South_West => North_West
   --  * West       => North
   --  * North_West => North_East
   function Perpendicular (Dir : Direction) return Direction;

   type Direction_Flags is array (Direction) of Boolean;
   pragma Pack (Direction_Flags);

   function Rotate_Left (Dir : Cardinal_Direction) return Cardinal_Direction;

   function Rotate_Right (Dir : Cardinal_Direction) return Cardinal_Direction;

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
