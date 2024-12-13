with Advent.IO;      use Advent.IO;
with Advent.Vector_Math;
with Advent.Parsers.Integers;
with Ada.Command_Line;
with Ada.Containers.Vectors;

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

   --  See above.
   procedure Solve_IJ (A, B, P : Vec2; I, J : out Int) is
   begin
      --!pp off
      I := (P(X) - B(X)*((P(Y)*A(X) - A(Y)*P(X)) / (A(X)*B(Y) - A(Y)*B(X)))) / A(X);
      J := (P(Y)*A(X) - A(Y)*P(X)) / ((-A(Y))*B(X) + A(X)*B(Y));
      --!pp on
   end Solve_IJ;

   Text : constant String := Advent.IO.Read_All_Text (Ada.Command_Line.Argument (1));

   Ints : constant Int_Vectors.Vector := AIP.Extract_Integers (Text);

   Index : Positive := Ints.First_Index;

   Part_1 : Int := 0;
   Part_2 : Int := 0;

   A, B, P : Vec2;
   I, J    : Int;

begin
   while Index <= Ints.Last_Index loop
      A := (Ints (Index + 0), Ints (Index + 1));
      B := (Ints (Index + 2), Ints (Index + 3));
      P := (Ints (Index + 4), Ints (Index + 5));

      Index := Index + 6;

      Solve_IJ (A, B, P, I, J);
      if A * I + B * J = P then
         Part_1 := Part_1 + (3 * I + J);
      end if;

      P := P + Fill (10_000_000_000_000);

      Solve_IJ (A, B, P, I, J);
      if A * I + B * J = P then
         Part_2 := Part_2 + (3 * I + J);
      end if;
   end loop;

   Solution (Part_1);
   Solution (Part_2);
end Day13;
