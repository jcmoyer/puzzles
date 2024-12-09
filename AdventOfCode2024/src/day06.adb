with Advent.IO;                  use Advent.IO;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Directions;          use Advent.Directions;
with Ada.Containers.Vectors;
with Ada.Command_Line;

procedure Day06 is

   type Visited_Map is array (Integer range <>, Integer range <>) of Boolean;

   type Visited_Directional_Map is
     array (Integer range <>, Integer range <>, Cardinal_Direction range <>) of Boolean;

   package Location_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Vec2);

   subtype Location_Vector is Location_Vectors.Vector;

   --  Returns the number of distinct locations visited and stores those
   --  locations in `Visited_Locations`. Assumes the input terminates.
   function Simulate_To_Termination
     (Map : Char_Matrix; Start : Vec2; Visited_Locations : in out Location_Vector) return Natural
   is
      Visited : Visited_Map (Map'Range (1), Map'Range (2)) := (others => (others => False));

      Pos : Vec2               := Start;
      Dir : Cardinal_Direction := North;
   begin
      Visited_Locations.Clear;

      while In_Bounds (Map, Pos) loop
         if not Visited (Pos (X), Pos (Y)) then
            Visited (Pos (X), Pos (Y)) := True;
            Visited_Locations.Append (Vec2'(Pos (X), Pos (Y)));
         end if;

         declare
            Next_Pos : constant Vec2 := Pos + To_Vector (Dir);
         begin
            if In_Bounds (Map, Next_Pos) and then Element (Map, Next_Pos) = '#' then
               Dir := Rotate_CW_90 (Dir);
            else
               Pos := Next_Pos;
            end if;
         end;
      end loop;

      return Natural (Visited_Locations.Length);
   end Simulate_To_Termination;

   type Loop_Result is (Looping, Terminating);

   --  Simplified version of the above function that only checks for loops -
   --  doesn't save the path.
   function Simulate_For_Loop (Map : Char_Matrix; Start : Vec2) return Loop_Result is
      Visited : Visited_Directional_Map (Map'Range (1), Map'Range (2), Cardinal_Direction) :=
        (others => (others => (others => False)));

      Pos : Vec2               := Start;
      Dir : Cardinal_Direction := North;
   begin
      while In_Bounds (Map, Pos) loop
         --  if we were at this place before facing the same direction, the
         --  path will definitely repeat
         if Visited (Pos (X), Pos (Y), Dir) then
            return Looping;
         end if;
         Visited (Pos (X), Pos (Y), Dir) := True;

         declare
            Next_Pos : constant Vec2 := Pos + To_Vector (Dir);
         begin
            if In_Bounds (Map, Next_Pos) and then Element (Map, Next_Pos) = '#' then
               Dir := Rotate_CW_90 (Dir);
            else
               Pos := Next_Pos;
            end if;
         end;
      end loop;

      return Terminating;
   end Simulate_For_Loop;

   No_Start : exception;

   function Find_Start (Map : Char_Matrix) return Vec2 is
   begin
      for I in Map'Range (1) loop
         for J in Map'Range (2) loop
            if Map (I, J) = '^' then
               return (I, J);
            end if;
         end loop;
      end loop;
      raise No_Start with "no starting position for guard";
   end Find_Start;

   function Simulate_Count_Loops
     (Map : Char_Matrix; Start : Vec2; Obstacles : Location_Vector) return Natural
   is
      Loops : Natural := 0;
   begin
      for Location of Obstacles loop
         declare
            Modified_Map : Char_Matrix := Map;
         begin
            Modified_Map (Location (X), Location (Y)) := '#';
            if Simulate_For_Loop (Modified_Map, Start) = Looping then
               Loops := Loops + 1;
            end if;
         end;
      end loop;
      return Loops;
   end Simulate_Count_Loops;

   Map : constant Char_Matrix := Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));

   Start   : constant Vec2 := Find_Start (Map);
   Visited : Location_Vector;

begin
   Solution (Simulate_To_Termination (Map, Start, Visited));
   Solution (Simulate_Count_Loops (Map, Start, Visited));
end Day06;
