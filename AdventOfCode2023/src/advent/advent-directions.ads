with Advent.Integer_Vector_Math;

package Advent.Directions is

   type Direction is (North, South, West, East);

   --  Returns a vector using conventional <row, col> matrix coordinates.
   function To_Vector (Dir : Direction) return Advent.Integer_Vector_Math.Vec2;

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

end Advent.Directions;
