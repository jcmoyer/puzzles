with Advent.Integer_Parsers; use Advent.Integer_Parsers;
with Advent.Testing;         use Advent.Testing;

procedure Test_Integer_Parsers is

   Arr : Advent.Integer_Parsers.Array_Type (1 .. 16);
   N   : Integer := 0;

begin

   N := Extract_Integers ("1-2", Arr);
   Assert (N = 2);
   Assert (Arr (1) = 1);
   Assert (Arr (2) = -2);

   N := Extract_Positive_Integers ("1-2", Arr);
   Assert (N = 2);
   Assert (Arr (1) = 1);
   Assert (Arr (2) = 2);

   N := Extract_Integers ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16", Arr);
   Assert (N = 16);
   Assert (for all I in 1 .. 16 => Arr (I) = I);

end Test_Integer_Parsers;
