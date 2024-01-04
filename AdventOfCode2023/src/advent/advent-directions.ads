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

end Advent.Directions;
