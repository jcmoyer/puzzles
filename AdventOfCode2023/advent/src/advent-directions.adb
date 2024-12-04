package body Advent.Directions is

   function Opposite (Dir : Direction) return Direction is
   begin
      case Dir is
         when North =>
            return South;
         when North_East =>
            return South_West;
         when East =>
            return West;
         when South_East =>
            return North_West;
         when South =>
            return North;
         when South_West =>
            return North_East;
         when West =>
            return East;
         when North_West =>
            return South_East;
      end case;
   end Opposite;

   function Perpendicular (Dir : Direction) return Direction
   is (Direction'Val ((Direction'Pos (Dir) + 2) rem 8));

   function Rotate_Left (Dir : Cardinal_Direction) return Cardinal_Direction is
   begin
      case Dir is
         when North =>
            return West;
         when South =>
            return East;
         when West =>
            return South;
         when East =>
            return North;
      end case;
   end Rotate_Left;

   function Rotate_Right (Dir : Cardinal_Direction) return Cardinal_Direction is
   begin
      case Dir is
         when North =>
            return East;
         when South =>
            return West;
         when West =>
            return North;
         when East =>
            return South;
      end case;
   end Rotate_Right;

   function Parse_Direction (C : Character) return Cardinal_Direction is
   begin
      case C is
         when 'U' | 'u' | 'N' | 'n' | '^' =>
            return North;
         when 'D' | 'd' | 'S' | 's' | 'v' =>
            return South;
         when 'L' | 'l' | 'W' | 'w' | '<' =>
            return West;
         when 'R' | 'r' | 'E' | 'e' | '>' =>
            return East;
         when others =>
            raise Invalid_Character;
      end case;
   end Parse_Direction;

end Advent.Directions;
