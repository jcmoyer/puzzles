with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

procedure Day25 is

   type Bit_Height is range 0 .. 7;

   type Bit_Array is array (1 .. 5) of Bit_Height;

   package Bit_Array_Vectors is new Ada.Containers.Vectors (Positive, Bit_Array);

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   Line  : Positive              := 1;

   Keys, Locks : Bit_Array_Vectors.Vector;

   Part_1 : Integer := 0;

   procedure Read_Key_Lock is
      Current : Bit_Array := (others => 0);
      Is_Key  : Boolean   := False;
   begin
      if Lines (Line) = "#####" then
         Is_Key := True;
      end if;

      for I in 1 .. 7 loop
         for J in 1 .. 5 loop
            Current (J) := Current (J) + (if Lines (Line) (J) = '#' then 1 else 0);
         end loop;
         Line := Line + 1;
      end loop;

      if Is_Key then
         Keys.Append (Current);
      else
         Locks.Append (Current);
      end if;
   end Read_Key_Lock;

   function Fit (Key, Lock : Bit_Array) return Boolean is
   begin
      for I in 1 .. 5 loop
         if Key (I) + Lock (I) > 7 then
            return False;
         end if;
      end loop;
      return True;
   end Fit;

begin
   while Count_Type (Line) <= Lines.Length loop
      if Lines.Element (Line)'Length = 5 then
         Read_Key_Lock;
      else
         Line := Line + 1;
      end if;
   end loop;

   for K of Keys loop
      for L of Locks loop
         if Fit (K, L) then
            Part_1 := Part_1 + 1;
         end if;
      end loop;
   end loop;

   Solution (Part_1);
end Day25;
