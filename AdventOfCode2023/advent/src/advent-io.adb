with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Streams;             use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Advent.IO is

   procedure Solution (Value : Integer) is
   begin
      Ada.Text_IO.Put_Line (Trim (Value'Image, Ada.Strings.Left));
   end Solution;

   procedure Solution (Value : Long_Long_Integer) is
   begin
      Ada.Text_IO.Put_Line (Trim (Value'Image, Ada.Strings.Left));
   end Solution;

   procedure Solution (Value : String) is
   begin
      Ada.Text_IO.Put_Line (Value);
   end Solution;

   package SIO renames Ada.Streams.Stream_IO;
   package Latin_1 renames Ada.Characters.Latin_1;

   function Read_All_Bytes (Filename : String) return Stream_Element_Array_Ptr is
      Input_File : SIO.File_Type;
      Buffer     : Stream_Element_Array_Ptr;
      Last       : Stream_Element_Offset;

      use type SIO.Count;

   begin
      SIO.Open (Input_File, SIO.In_File, Filename);
      Buffer := new Stream_Element_Array (0 .. Stream_Element_Offset (SIO.Size (Input_File) - 1));
      SIO.Read (Input_File, Buffer.all, Last);
      SIO.Close (Input_File);
      return Buffer;
   end Read_All_Bytes;

   function Read_All_Text (Filename : String) return String is
      Buffer : constant Stream_Element_Array_Ptr := Read_All_Bytes (Filename);
      Result : String (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Result (Result'First + Integer (I - Buffer'First)) := Character'Val (Buffer (I));
      end loop;
      return Result;
   end Read_All_Text;

   function Read_All_Lines (Filename : String) return Advent.Strings.String_Array is
      Result : Advent.Strings.String_Array;

      Buffer : constant Stream_Element_Array_Ptr := Read_All_Bytes (Filename);

      Line_Start : Stream_Element_Offset := 0;
      Line_End   : Stream_Element_Offset := 0;
      I          : Stream_Element_Offset := 0;

   begin
      I := Buffer.all'First;

      while I <= Buffer.all'Last loop
         --  scan for newline
         Line_Start := I;
         while I <= Buffer.all'Last
           and then Buffer.all (I) /= Stream_Element (Character'Pos (Latin_1.LF))
         loop
            I := I + 1;
         end loop;
         Line_End := I - 1;

         --  handle \r\n
         if Buffer.all (Line_End) = Stream_Element (Character'Pos (Latin_1.CR)) then
            Line_End := Line_End - 1;
         end if;

         --  skip newline
         I := I + 1;

         --  copy processed line
         declare
            Line : String (1 .. Integer (Line_End - Line_Start + 1));
         begin
            for J in Line_Start .. Line_End loop
               Line (Line'First + Integer (J - Line_Start)) := Character'Val (Buffer.all (J));
            end loop;
            Result.Append (Line);
         end;
      end loop;

      return Result;
   end Read_All_Lines;

   function Read_Tilemap (Filename : String) return Char_Matrix is
      use Advent.Strings.String_Vectors;
      Lines     : constant Advent.Strings.String_Array := Read_All_Lines (Filename);
      Row_Count : constant Integer                     := Integer (Lines.Length);
      Col_Count : constant Integer                     := First_Element (Lines)'Length;
   begin
      return Result : Char_Matrix (1 .. Row_Count, 1 .. Col_Count) do
         for I in 1 .. Row_Count loop
            for J in 1 .. Col_Count loop
               Result (I, J) := Lines (I - 1) (J);
            end loop;
         end loop;
      end return;
   end Read_Tilemap;

   function Rows (M : Char_Matrix) return Integer is
   begin
      return M'Length (1);
   end Rows;

   function Cols (M : Char_Matrix) return Integer is
   begin
      return M'Length (2);
   end Cols;

   function Image (M : Char_Matrix) return String is
      --  Add an additional column for each row to store a newline
      Result_Length : constant Integer            := Rows (M) * Cols (M) + Rows (M);
      Result        : String (1 .. Result_Length) := (others => Ada.Characters.Latin_1.LF);

      Line_Length : constant Integer := Cols (M) + 1;
   begin
      for I in M'First (1) .. M'Last (1) loop
         for J in M'First (2) .. M'Last (2) loop
            Result (1 + (I - M'First (1)) * Line_Length + (J - M'First (2))) := M (I, J);
         end loop;
      end loop;
      return Result;
   end Image;

   function Transpose (M : Char_Matrix) return Char_Matrix is
      Result : Char_Matrix (M'First (2) .. M'Last (2), M'First (1) .. M'Last (1));
   begin
      for I in M'First (1) .. M'Last (1) loop
         for J in M'First (2) .. M'Last (2) loop
            Result (J, I) := M (I, J);
         end loop;
      end loop;
      return Result;
   end Transpose;

end Advent.IO;
