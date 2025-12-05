with Advent.IO;
with Advent.Long_Parsers;
with Advent.Strings;
with Advent.Intervals;
with Ada.Command_Line;

procedure Day05 is
   package Long_Intervals is new
     Advent.Intervals (Element_Type => Long_Long_Integer);
   use Long_Intervals;

   Lines  : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));
   Nums   : Advent.Long_Parsers.Array_Type (1 .. 2);
   N_Ints : Integer;

   Intervals : Long_Intervals.Multi_Interval;

   P1 : Long_Long_Integer := 0;
begin
   for Line of Lines loop
      N_Ints := Advent.Long_Parsers.Extract_Positive_Integers (Line, Nums);
      if N_Ints = 2 then
         Insert (Intervals, Interval'(Min => Nums (1), Max => Nums (2)));
      elsif N_Ints = 1 then
         if Contains (Intervals, Nums (1)) then
            P1 := P1 + 1;
         end if;
      end if;
   end loop;

   Advent.IO.Solution (P1);
   Advent.IO.Solution (Count (Intervals));
end Day05;
