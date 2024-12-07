with Advent.IO;         use Advent.IO;
with Advent.Strings;    use Advent.Strings;
with Advent.Long_Parsers;
with Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings;       use Ada.Strings;

procedure Day07 is

   package ALP renames Advent.Long_Parsers;

   function Concat (X, Y : Long_Long_Integer) return Long_Long_Integer is
   begin
      return Long_Long_Integer'Value (Trim (X'Image, Left) & Trim (Y'Image, Left));
   end Concat;

   function Find_Solution (E : ALP.Vector; With_Concat : Boolean) return Boolean is
      function Add_Mul_Concat
        (Total, Current : Long_Long_Integer; Parts : ALP.Vector) return Boolean
      is
         Children : ALP.Vector;
         Head     : Long_Long_Integer;
      begin
         if Parts.Is_Empty then
            if Total = Current then
               return True;
            else
               return False;
            end if;
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
