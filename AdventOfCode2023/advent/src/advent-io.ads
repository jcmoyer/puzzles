with Advent.Strings;
with Ada.Streams;

package Advent.IO is

   pragma Elaborate_Body;

   procedure Solution (Value : Integer);
   procedure Solution (Value : Long_Long_Integer);
   procedure Solution (Value : String);

   type Stream_Element_Array_Ptr is access all Ada.Streams.Stream_Element_Array;

   function Read_All_Bytes (Filename : String) return Stream_Element_Array_Ptr;

   function Read_All_Text (Filename : String) return String;

   --  Returns an array of strings where each element corresponds to a single
   --  line in the input file.
   function Read_All_Lines (Filename : String) return Advent.Strings.String_Array;

   type Char_Matrix is array (Integer range <>, Integer range <>) of Character;

   function Read_Tilemap (Filename : String) return Char_Matrix;
   function Rows (M : Char_Matrix) return Integer;
   function Cols (M : Char_Matrix) return Integer;
   function Image (M : Char_Matrix) return String;
   function Transpose (M : Char_Matrix) return Char_Matrix;

end Advent.IO;
