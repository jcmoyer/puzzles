with Advent.IO;                  use Advent.IO;
with Advent.Strings;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Directions;          use Advent.Directions;
with Ada.Command_Line;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Containers;

procedure Day06 is

   type Visited_Map is
     array (Integer range <>, Integer range <>, Direction range <>) of Boolean;

   function Simulate (Map : Char_Matrix; Start : Vec2) return Integer is
      Visited : Visited_Map (Map'Range(1), Map'Range(2), Direction) :=
        (others => (others => (others => False)));

      Pos   : Vec2 := Start;
      Dir   : Cardinal_Direction := North;
      Steps : Natural := 0;
   begin
      while In_Bounds (Map, Pos) loop
         --  if we were at this place before facing the same direction, the
         --  path will definitely repeat
         if Visited (Pos (X), Pos (Y), Dir) then
            return -1;
         end if;
         Visited (Pos (X), Pos (Y), Dir) := True;

         declare
            Next_Pos : constant Vec2 := Pos + To_Vector (Dir);
         begin
            if In_Bounds (Map, Next_Pos) and then Element (Map, Next_Pos) = '#'
            then
               Dir := Rotate_CW_90 (Dir);
            else
               Pos := Next_Pos;
            end if;
         end;
      end loop;

      for I in Visited'Range(1) loop
         for J in Visited'Range(2) loop
            for K in Cardinal_Direction loop
               if Visited (I, J, K) then
                  Steps := Steps + 1;
                  exit;
               end if;
            end loop;
         end loop;
      end loop;

      return Steps;
   end Simulate;

   No_Start : exception;

   function Find_Start (Map : Char_Matrix) return Vec2 is
   begin
      for I in Map'Range(1) loop
         for J in Map'Range(2) loop
            if Map (I, J) = '^' then
               return (I, J);
            end if;
         end loop;
      end loop;
      raise No_Start with "no starting position for guard";
   end Find_Start;

   function Simulate_Count_Loops
     (Map : Char_Matrix; Start : Vec2) return Natural
   is
      Loops : Natural := 0;
   begin
      for I in Map'Range(1) loop
         for J in Map'Range(2) loop
            if Map (I, J) = '.' then
               declare
                  Modified_Map : Char_Matrix := Map;
               begin
                  Modified_Map (I, J) := '#';
                  if Simulate (Modified_Map, Start) = -1 then
                     Loops := Loops + 1;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      return Loops;
   end Simulate_Count_Loops;

   Map : constant Char_Matrix :=
     Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));

   Start : constant Vec2 := Find_Start (Map);

begin
   Solution (Simulate (Map, Start));
   Solution (Simulate_Count_Loops (Map, Start));
end Day06;
