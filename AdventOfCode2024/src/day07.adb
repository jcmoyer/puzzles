with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Advent.Long_Parsers;
with Ada.Command_Line;

procedure Day07 is

   package ALP renames Advent.Long_Parsers;

   --  Oldest trick in the book :)
   --
   --  The input to this function is never more than 3 digits in official
   --  inputs, so we optimize for that case but still provide total coverage up
   --  to 20 digits for other inputs.
   function N_Digits (X : Long_Long_Integer) return Natural is
   begin
      if X >= 100 then
         if X >= 1_000 then
            return 3 + N_Digits (X / 1_000);
         end if;
         return 3;
      elsif X >= 10 then
         return 2;
      end if;
      return 1;
   end N_Digits;

   function Concat (X, Y : Long_Long_Integer) return Long_Long_Integer is
   begin
      return (X * (10**N_Digits (Y))) + Y;
   end Concat;

   function Find_Solution (E : ALP.Array_Type; With_Concat : Boolean) return Boolean is
      function Add_Mul_Concat (Total, Current : Long_Long_Integer; Place : Positive) return Boolean
      is
      begin
         --  Cull impossible solutions early
         if Current > Total then
            return False;
         end if;

         --  Base case
         if Place = 1 + E'Last then
            return Total = Current;
         end if;

         declare
            Head : constant Long_Long_Integer := E (Place);
            Tail : constant Positive          := Place + 1;
         begin
            return
              Add_Mul_Concat (Total, Current + Head, Tail)
              or else Add_Mul_Concat (Total, Current * Head, Tail)
              or else (With_Concat and then Add_Mul_Concat (Total, Concat (Current, Head), Tail));
         end;
      end Add_Mul_Concat;
   begin
      return Add_Mul_Concat (E (1), E (2), 3);
   end Find_Solution;

   Lines : constant String_Array := Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Sum_P1, Sum_P2 : Long_Long_Integer := 0;

begin
   for Line of Lines loop
      declare
         Equation   : ALP.Array_Type (1 .. 13);
         Equation_N : constant Natural := ALP.Extract_Integers (Line, Equation);
      begin
         if Find_Solution (Equation (1 .. Equation_N), False) then
            Sum_P1 := Sum_P1 + Equation (1);
         end if;

         if Find_Solution (Equation (1 .. Equation_N), True) then
            Sum_P2 := Sum_P2 + Equation (1);
         end if;
      end;
   end loop;

   Solution (Sum_P1);
   Solution (Sum_P2);
end Day07;
