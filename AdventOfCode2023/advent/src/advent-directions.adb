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

   function Rotate_CCW_90 (Dir : Direction) return Direction is
   begin
      case Dir is
         when North =>
            return West;
         when North_East =>
            return North_West;
         when East =>
            return North;
         when South_East =>
            return North_East;
         when South =>
            return East;
         when South_West =>
            return South_East;
         when West =>
            return South;
         when North_West =>
            return South_West;
      end case;
   end Rotate_CCW_90;

   function Rotate_CW_90 (Dir : Direction) return Direction is
   begin
      case Dir is
         when North =>
            return East;
         when North_East =>
            return South_East;
         when East =>
            return South;
         when South_East =>
            return South_West;
         when South =>
            return West;
         when South_West =>
            return North_West;
         when West =>
            return North;
         when North_West =>
            return North_East;
      end case;
   end Rotate_CW_90;

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
