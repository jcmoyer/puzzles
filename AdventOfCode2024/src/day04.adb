with Advent.IO;                  use Advent.IO;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;

procedure Day04 is
   type Direction_8 is
     (North,
      North_East,
      East,
      South_East,
      South,
      South_West,
      West,
      North_West);

   subtype Diagonal_Direction_8 is Direction_8
   with
     Static_Predicate =>
       Diagonal_Direction_8 in
         North_East
         | South_East
         | South_West
         | North_West;

   function Opposite (D : Direction_8) return Direction_8 is
   begin
      case D is
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

   function Perpendicular (D : Direction_8) return Direction_8
   is (Direction_8'Val ((Direction_8'Pos (D) + 2) rem 8));

   function To_Vector (D : Direction_8) return Vec2 is
   begin
      case D is
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

   function In_Bounds (M : Char_Matrix; Where : Vec2) return Boolean is
   begin
      return Where (X) in M'Range(1) and then Where (Y) in M'Range(2);
   end In_Bounds;

   function Index (M : Char_Matrix; Where : Vec2) return Character
   with Pre => In_Bounds (M, Where)
   is
   begin
      return M (Where (X), Where (Y));
   end Index;

   function Equal_Along
     (Map       : Char_Matrix;
      Start     : Vec2;
      Direction : Direction_8;
      Substring : String) return Boolean
   is
      Pos : Vec2;
   begin
      for Offset in 0 .. Substring'Length - 1 loop
         Pos := Start + To_Vector (Direction) * Offset;

         if not In_Bounds (Map, Pos) then
            return False;
         end if;

         if Substring (Substring'First + Offset) /= Index (Map, Pos) then
            return False;
         end if;
      end loop;

      return True;
   end Equal_Along;

   function Count_XMAS (Map : Char_Matrix; Start : Vec2) return Integer is
      Count : Integer := 0;
   begin
      for D in Direction_8 loop
         if Equal_Along (Map, Start, D, "XMAS") then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_XMAS;

   function Find_X_MAS (Map : Char_Matrix; Start : Vec2) return Boolean is
      Search : constant String := "MAS";
      Pos    : Vec2;

      Search_Start     : Vec2;
      Search_Direction : Direction_8;
      Perp             : Direction_8;
   begin
      for Offset_Dir in Diagonal_Direction_8 loop
         Search_Start := Start + To_Vector (Offset_Dir);
         Search_Direction := Opposite (Offset_Dir);

         if Equal_Along (Map, Search_Start, Search_Direction, "MAS") then
            --  Found MAS, now inspect the perpendicular crossing through `A`
            Perp := Perpendicular (Search_Direction);
            if Equal_Along (Map, Start - To_Vector (Perp), Perp, "MAS") then
               return True;
            end if;
            if Equal_Along (Map, Start - To_Vector (Perp), Perp, "SAM") then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end Find_X_MAS;

   Map : constant Char_Matrix :=
     Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));

   Sum_P1 : Integer := 0;
   Sum_P2 : Integer := 0;

begin
   for I in Map'Range(1) loop
      for J in Map'Range(2) loop
         Sum_P1 := Sum_P1 + Count_XMAS (Map, Vec2'(I, J));

         if Find_X_MAS (Map, Vec2'(I, J)) then
            Sum_P2 := Sum_P2 + 1;
         end if;
      end loop;
   end loop;

   Solution (Sum_P1);
   Solution (Sum_P2);
end Day04;
