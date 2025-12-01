with Advent.IO;
with Advent.Strings;
with Ada.Command_Line;
with Advent.Integer_Parsers; use Advent.Integer_Parsers;

procedure Day01 is
   type Dial_Type is mod 100;

   Lines : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Amount_Arr : Advent.Integer_Parsers.Array_Type (1 .. 1);
   Amount     : Integer;
   N_Ints     : Natural;
   Clicks     : Integer;

   Dial   : Dial_Type := 50;
   Zeroes : Integer := 0;
   P1, P2 : Integer := 0;
begin
   for Line of Lines loop
      N_Ints := Extract_Integers (Line, Amount_Arr);
      if N_Ints /= 1 then
         raise Program_Error with "expected 1 integer per line";
      end if;

      Zeroes := 0;

      Amount := Amount_Arr (Amount_Arr'First);

      if Line (Line'First) = 'L' then
         Clicks := Integer (Dial) - Amount;
         if Clicks <= 0 then
            Zeroes := (if Dial = 0 then 0 else 1) + Clicks / (-100);
         end if;

         Dial := Dial - Dial_Type'Mod (Amount);
      elsif Line (Line'First) = 'R' then
         Clicks := Integer (Dial) + Amount;
         Zeroes := Clicks / 100;

         Dial := Dial + Dial_Type'Mod (Amount);
      end if;

      if Dial = 0 then
         P1 := P1 + 1;
      end if;

      P2 := P2 + Zeroes;
   end loop;

   Advent.IO.Solution (P1);
   Advent.IO.Solution (P2);
end Day01;
