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

   function Find_Solution (E : ALP.Vector; With_Concat : Boolean) return Boolean is
      function Add_Mul_Concat
        (Total, Current : Long_Long_Integer; Parts : ALP.Vector) return Boolean
      is
         Children : ALP.Vector;
         Head     : Long_Long_Integer;
      begin
         --  Cull impossible solutions early
         if Current > Total then
            return False;
         end if;

         --  Base case
         if Parts.Is_Empty then
            return Total = Current;
         end if;

         Children := Parts.Copy;
         Head     := Children.First_Element;
         Children.Delete_First;

         return
           Add_Mul_Concat (Total, Current + Head, Children)
           or else Add_Mul_Concat (Total, Current * Head, Children)
           or else (With_Concat and then Add_Mul_Concat (Total, Concat (Current, Head), Children));
      end Add_Mul_Concat;

      Children    : ALP.Vector := E.Copy;
      Start, Goal : Long_Long_Integer;

   begin
      Goal := Children.First_Element;
      Children.Delete_First;
      Start := Children.First_Element;
      Children.Delete_First;
      return Add_Mul_Concat (Goal, Start, Children);
   end Find_Solution;

   Lines : constant String_Array := Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Sum_P1, Sum_P2 : Long_Long_Integer := 0;

begin
   for Line of Lines loop
      declare
         Equation : constant ALP.Vector := ALP.Extract_Integers (Line);
      begin
         if Find_Solution (Equation, False) then
            Sum_P1 := Sum_P1 + Equation.First_Element;
         end if;

         if Find_Solution (Equation, True) then
            Sum_P2 := Sum_P2 + Equation.First_Element;
         end if;
      end;
   end loop;

   Solution (Sum_P1);
   Solution (Sum_P2);
end Day07;
