package body Advent.Directions is

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

   function Rotate_Left (Dir : Direction) return Direction is
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

   function Rotate_Right (Dir : Direction) return Direction is
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

   function Parse_Direction (C : Character) return Direction is
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
