with Advent.Containers.String_Vectors;
with Advent.Integer_Vector_Math;
with Ada.Streams;

package Advent.IO is

   pragma Elaborate_Body;

   procedure Solution (Value : Integer);
   procedure Solution (Value : Long_Long_Integer);
   procedure Solution (Value : String);

   type Stream_Element_Array_Ptr is access all Ada.Streams.Stream_Element_Array;

   function Read_All_Bytes (Filename : String) return Stream_Element_Array_Ptr;

   function Read_All_Text (Filename : String) return String;

   --  Returns a vector of strings where each element corresponds to a single
   --  line in the input file.
   function Read_All_Lines (Filename : String) return Advent.Containers.String_Vectors.Vector;

   type Char_Matrix is array (Integer range <>, Integer range <>) of Character;

   function Read_Tilemap (Filename : String) return Char_Matrix;
   function Rows (M : Char_Matrix) return Integer;
   function Cols (M : Char_Matrix) return Integer;
   function Image (M : Char_Matrix) return String;
   function Transpose (M : Char_Matrix) return Char_Matrix;
   function In_Bounds (M : Char_Matrix; Index : Advent.Integer_Vector_Math.Vec2) return Boolean;
   function Element (M : Char_Matrix; Index : Advent.Integer_Vector_Math.Vec2) return Character with
     Pre => In_Bounds (M, Index);
   function Element_Wrapping (M : Char_Matrix; Index : Advent.Integer_Vector_Math.Vec2) return Character;

end Advent.IO;
