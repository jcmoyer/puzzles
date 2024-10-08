package Advent.Directions is

   pragma Pure;

   type Direction is (North, South, West, East);

   --  Returns the opposite Direction from Dir. The conversions are:
   --
   --  * North => South
   --  * South => North
   --  * West => East
   --  * East => West
   function Opposite (Dir : Direction) return Direction;

   type Direction_Flags is array (Direction) of Boolean;
   pragma Pack (Direction_Flags);

   function Rotate_Left (Dir : Direction) return Direction;

   function Rotate_Right (Dir : Direction) return Direction;

   Invalid_Character : exception;

   --  Parses a single character as a direction using typical Advent of Code
   --  conventions. The accepted characters for each direction are:
   --
   --  North: N, n, U, u, ^
   --  South: S, s, D, d, v
   --  West:  W, w, L, l, <
   --  East:  E, e, R, r, >
   --
   --  Raises Invalid_Character if C is not one of the characters above.
   function Parse_Direction (C : Character) return Direction;

end Advent.Directions;
