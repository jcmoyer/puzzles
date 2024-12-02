with Advent.IO;
with Advent.Strings;
with Advent.Integer_Parsers;
with Ada.Command_Line;

procedure Day02 is
   package AIP renames Advent.Integer_Parsers;

   function Derivative (A : AIP.Array_Type) return AIP.Array_Type is
      R : AIP.Array_Type (A'First .. A'Last - 1);
   begin
      for I in A'First + 1 .. A'Last loop
         R (I - 1) := A (I) - A (I - 1);
      end loop;
      return R;
   end Derivative;

   function Safe (Deriv : AIP.Array_Type) return Boolean
   is ((for all D of Deriv => (abs D in 1 .. 3))
       and then ((for all D of Deriv => D > 0)
                 or else (for all D of Deriv => D < 0)));

   --  Returns a copy of array `A` without the element at `Index`
   function Without
     (A : AIP.Array_Type; Index : Positive) return AIP.Array_Type
   is
      R           : AIP.Array_Type (A'First .. A'Last - 1);
      Write_Index : Positive := R'First;
   begin
      for I in A'Range loop
         if I /= Index then
            R (Write_Index) := A (I);
            Write_Index := Write_Index + 1;
         end if;
      end loop;
      return R;
   end Without;

   Lines          : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));
   Sum_P1, Sum_P2 : Integer := 0;

begin
   for Line of Lines loop
      declare
         Ints : AIP.Array_Type (1 .. 8);
         Num  : constant Natural := AIP.Extract_Integers (Line, Ints);
      begin
         if Safe (Derivative (Ints (1 .. Num))) then
            Sum_P1 := Sum_P1 + 1;
            Sum_P2 := Sum_P2 + 1;
         else
            for I in 1 .. Num loop
               if Safe (Derivative (Without (Ints (1 .. Num), I))) then
                  Sum_P2 := Sum_P2 + 1;
                  exit;
               end if;
            end loop;
         end if;
      end;
   end loop;

   Advent.IO.Solution (Sum_P1);
   Advent.IO.Solution (Sum_P2);
end Day02;
