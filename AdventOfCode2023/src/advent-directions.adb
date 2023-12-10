package body Advent.Directions is

   function To_Vector (Dir : Direction) return Vec2i is
   begin
      case Dir is
         when North =>
            return (-1, 0);
         when South =>
            return (1, 0);
         when West =>
            return (0, -1);
         when East =>
            return (0, 1);
      end case;
   end To_Vector;

   function Opposite (Dir : Direction) return Direction is
   begin
      case Dir is
         when North =>
            return South;
         when South =>
            return North;
         when West =>
            return East;
         when East =>
            return West;
      end case;
   end Opposite;

end Advent.Directions;
