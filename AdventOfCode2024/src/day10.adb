with Advent.IO;                  use Advent.IO;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;

procedure Day10 is

   package Vec2_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Vec2);

   type Visited_Map is array (Integer range <>, Integer range <>) of Boolean;

   procedure Gather_Starts (Map : Char_Matrix; Result : in out Vec2_Vectors.Vector) is
   begin
      for I in Map'Range (1) loop
         for J in Map'Range (2) loop
            if Map (I, J) = '0' then
               Result.Append (Vec2'(I, J));
            end if;
         end loop;
      end loop;
   end Gather_Starts;

   function Count_Reachable (Map : Char_Matrix; Start : Vec2; Distinct : Boolean) return Integer is
      Visited : Visited_Map (Map'Range (1), Map'Range (2)) := (others => (others => False));

      function Visit_Rec (From : Vec2) return Integer is
         Next      : constant Character := Character'Val (Character'Pos (Element (Map, From)) + 1);
         Reachable : Integer            := 0;
      begin
         if Distinct then
            if Visited (From (X), From (Y)) then
               return 0;
            end if;
            Visited (From (X), From (Y)) := True;
         end if;

         if Element (Map, From) = '9' then
            return 1;
         end if;

         for D in Cardinal_Direction loop
            if In_Bounds (Map, From + To_Vector (D))
              and then Element (Map, From + To_Vector (D)) = Next
            then
               Reachable := Reachable + Visit_Rec (From + To_Vector (D));
            end if;
         end loop;

         return Reachable;
      end Visit_Rec;
   begin
      return Visit_Rec (Start);
   end Count_Reachable;

   Map    : constant Char_Matrix := Advent.IO.Read_Tilemap (Ada.Command_Line.Argument (1));
   Starts : Vec2_Vectors.Vector;

   Sum_P1 : Integer := 0;
   Sum_P2 : Integer := 0;

begin

   Gather_Starts (Map, Starts);

   for S of Starts loop
      Sum_P1 := Sum_P1 + Count_Reachable (Map, S, Distinct => True);
   end loop;

   for S of Starts loop
      Sum_P2 := Sum_P2 + Count_Reachable (Map, S, Distinct => False);
   end loop;

   Solution (Sum_P1);
   Solution (Sum_P2);

end Day10;
