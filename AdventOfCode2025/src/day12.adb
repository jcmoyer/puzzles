with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Advent.IO;         use Advent.IO;
with Ada.Command_Line;
with Advent.Containers.String_Vectors;
with Advent.Integer_Parsers;

procedure Day12 is

   Input_Error : exception;

   package AIP renames Advent.Integer_Parsers;

   Lines : constant Advent.Containers.String_Vectors.Vector :=
     Read_All_Lines (Ada.Command_Line.Argument (1));

   Line_No : Positive := Lines.First_Index;

   Ints   : AIP.Array_Type (1 .. 8);
   N_Ints : Natural;

   Shape_Areas   : array (0 .. 5) of Natural := (others => 0);
   Region_Shapes : array (0 .. 5) of Natural := (others => 0);

   P1 : Long_Long_Integer := 0;

begin
   while Line_No <= Lines.Last_Index loop
      declare
         Line                : constant String := Lines (Line_No);
         Total_Required_Area : Natural := 0;
         Shape_Area          : Natural := 0;
         Region_Area         : Natural := 0;
      begin
         if Index (Line, "x") /= 0 then
            N_Ints := AIP.Extract_Integers (Lines (Line_No), Ints);

            Region_Area := Ints (Ints'First) * Ints (Ints'First + 1);
            for I in Ints'First + 2 .. N_Ints loop
               Region_Shapes (I - Ints'First - 2) := Ints (I);
            end loop;

            for I in 0 .. 5 loop
               Total_Required_Area := Total_Required_Area + Shape_Areas (I) * Region_Shapes (I);
            end loop;

            if Total_Required_Area <= Region_Area then
               P1 := P1 + 1;
            end if;

            Line_No := Line_No + 1;
         elsif Index (Line, ":") /= 0 then
            N_Ints := AIP.Extract_Integers (Lines (Line_No), Ints);
            if N_Ints /= 1 then
               raise Input_Error with "expected integer followed by colon";
            end if;

            Shape_Area := 0;
            for Line_Offset in 1 .. 3 loop
               for Col in 1 .. 3 loop
                  if Lines (Line_No + Line_Offset) (Col) = '#' then
                     Shape_Area := Shape_Area + 1;
                  end if;
               end loop;
            end loop;

            Shape_Areas (Ints (Ints'First)) := Shape_Area;

            Line_No := Line_No + 5;
         else
            raise Input_Error with "unrecognized line " & Line_No'Image;
         end if;
      end;
   end loop;

   Solution (P1);
end Day12;
