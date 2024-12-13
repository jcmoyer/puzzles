with Advent.IO;         use Advent.IO;
with Advent.Strings;    use Advent.Strings;
with Advent.Directions; use Advent.Directions;
with Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Containers;
with Advent.Vector_Math;
with Advent.Parsers.Integers;

--  System:
--
--  a_x i + b_x j = p_x
--  a_y i + b_y j = p_y
--
--  Isolate i:
--
--  a_x i + b_x j = p_x
--  a_x i         = p_x - b_x j
--      i         = (p_x - b_x j)/a_x
--
--  Substitute into second equation:
--
--  a_y ((p_x - b_x j)/a_x) + b_y j = p_y
--
--  Solve j:
--
--  (a_y (p_x - b_x j)/a_x) + b_y j = p_y
--  a_y (p_x - b_x j) + a_x b_y j = p_y a_x
--  a_y p_x - a_y b_x j + a_x b_y j = p_y a_x
--          - a_y b_x j + a_x b_y j = p_y a_x - a_y p_x
--          j(- a_y b_x + a_x b_y)  = p_y a_x - a_y p_x
--          j                       = (p_y a_x - a_y p_x) / (-a_y b_x + a_x b_y)
--
--  Substitute first, solve i:
--
--  a_x i + b_x ((p_y a_x - a_y p_x) / (-a_y b_x + a_x b_y)) = p_x
--  a_x i = p_x - b_x ((p_y a_x - a_y p_x) / (-a_y b_x + a_x b_y))
--  i = (p_x - b_x ((p_y a_x - a_y p_x) / (-a_y b_x + a_x b_y))) / a_x
--
--  Not sure if there is always exactly 1 solution but it seems to be true for
--  my input.

procedure Day13 is

   subtype Int is Long_Long_Integer;

   package Int_Vector_Math is new Advent.Vector_Math (Int);
   use Int_Vector_Math;

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Int);

   package AIP is new Advent.Parsers.Integers
     (Element_Type => Int, Element_Vectors => Int_Vectors);

   Lines : constant String_Array := Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Part_1 : Int := 0;
   Part_2 : Int := 0;

   Ints : Int_Vectors.Vector;
   Line : Positive := Lines.First_Index;

   A, B, P : Vec2;

   --  See above.
   procedure Get_IJ (I, J : out Int) is
   begin
      --!pp off
      I := (P(X) - B(X)*((P(Y)*A(X) - A(Y)*P(X)) / (A(X)*B(Y) - A(Y)*B(X)))) / A(X);
      J := (P(Y)*A(X) - A(Y)*P(X)) / (-A(Y)*B(X) + A(X)*B(Y));
      --!pp on
   end Get_IJ;

begin
   while Line <= Lines.Last_Index loop
      Ints  := AIP.Extract_Integers (Lines (Line));
      A (X) := Ints (1);
      A (Y) := Ints (2);
      Line  := Line + 1;

      Ints  := AIP.Extract_Integers (Lines (Line));
      B (X) := Ints (1);
      B (Y) := Ints (2);
      Line  := Line + 1;

      Ints  := AIP.Extract_Integers (Lines (Line));
      P (X) := Ints (1);
      P (Y) := Ints (2);
      Line  := Line + 2;

      declare
         I, J : Int := 0;
      begin
         Get_IJ (I, J);
         if A * I + B * J = P then
            Part_1 := Part_1 + (3 * I + J);
         end if;
      end;

      P (X) := P (X) + 10_000_000_000_000;
      P (Y) := P (Y) + 10_000_000_000_000;

      declare
         I, J : Int := 0;
      begin
         Get_IJ (I, J);
         if A * I + B * J = P then
            Part_2 := Part_2 + (3 * I + J);
         end if;
      end;
   end loop;

   Solution (Part_1);
   Solution (Part_2);
end Day13;
