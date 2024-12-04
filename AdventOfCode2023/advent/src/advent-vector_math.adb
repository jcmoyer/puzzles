package body Advent.Vector_Math is

   function To_Vector (Dir : Direction) return Vec2 is
   begin
      case Dir is
         when North =>
            return (-1, 0);
         when North_East =>
            return (-1, 1);
         when East =>
            return (0, 1);
         when South_East =>
            return (1, 1);
         when South =>
            return (1, 0);
         when South_West =>
            return (1, -1);
         when West =>
            return (0, -1);
         when North_West =>
            return (-1, -1);
      end case;
   end To_Vector;

end Advent.Vector_Math;
