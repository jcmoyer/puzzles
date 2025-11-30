with Advent.IO;
with Advent.Strings;
with Ada.Command_Line;

procedure Day01 is
   Lines : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));
begin
   for Line of Lines loop
      null;
   end loop;

   Advent.IO.Solution (Integer (0));
end Day01;
