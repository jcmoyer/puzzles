with Advent.IO;
with Advent.Strings;
with Ada.Command_Line;

procedure Day03 is
   type Integer_Array is array (Natural range <>) of Integer;

   function Find_Largest (S : String; Size : Integer) return Long_Long_Integer is
      Num    : Integer_Array (0 .. Size) := (others => 0);
      Pos    : Integer_Array (0 .. Size) := (others => 0);
      Cur    : Integer;
      Result : Long_Long_Integer := 0;
   begin
      for I in 1 .. Size loop
         for J in Pos (I - 1) + 1 .. S'Length - (Size - I) loop
            Cur := Integer'Value ((1 => S (J)));
            if Cur > Num (I) then
               Pos (I) := J;
               Num (I) := Cur;
            end if;
         end loop;
      end loop;

      for I in 1 .. Size loop
         Result := Result * 10 + Long_Long_Integer (Num (I));
      end loop;

      return Result;
   end Find_Largest;

   ----------------------------------------------------------------------------

   Lines : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   P1, P2 : Long_Long_Integer := 0;

begin
   for Line of Lines loop
      P1 := P1 + Find_Largest (Line, 2);
      P2 := P2 + Find_Largest (Line, 12);
   end loop;

   Advent.IO.Solution (P1);
   Advent.IO.Solution (P2);
end Day03;
