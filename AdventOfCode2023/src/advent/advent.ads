with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;

package Advent is
   procedure Solution (Val : Integer);
   procedure Solution (Val : Long_Long_Integer);
   procedure Solution (Val : String);

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => String);
   subtype String_Array is String_Vectors.Vector;

   function Is_Any_Of (C : Character; Vals : String) return Boolean;

   --  When I <= J, returns S(I..J); otherwise returns an empty string.
   function Slice_Empty (S : String; I, J : Integer) return String;

   function Unbounded_Slice_Empty (S : Unbounded_String; Low, High : Integer) return Unbounded_String;

   function Split (S : String; Delims : String; Keep_Empty : Boolean := True) return String_Array;

   function Split
     (S : Unbounded_String; Delims : String; Keep_Empty : Boolean := True) return String_Array;

   --  Splits S on any delimiter contained in Delims.
   function Split_Any
     (S : String; Delims : String; Keep_Empty : Boolean := True) return String_Array;

   function Starts_With (Source, Substr : String) return Boolean;
   function Ends_With (Source, Substr : String) return Boolean;

   function Delete_Whitespace (Source : String) return String with
     Post => Delete_Whitespace'Result'Length <= Source'Length;

   --
   --  I/O  [TODO: to be extracted into another module]
   --

   type Stream_Element_Array_Ptr is access all Ada.Streams.Stream_Element_Array;

   function Read_All_Bytes (Filename : String) return Stream_Element_Array_Ptr;

   function Read_All_Text (Filename : String) return String;

   --  Returns an array of strings where each element corresponds to a single
   --  line in the input file.
   function Read_All_Lines (Filename : String) return String_Array;

   type Char_Matrix is array (Integer range <>, Integer range <>) of Character;

   function Read_Tilemap (Filename : String) return Char_Matrix;
   function Rows (M : Char_Matrix) return Integer;
   function Cols (M : Char_Matrix) return Integer;
   function Image (M : Char_Matrix) return String;
   function Transpose (M : Char_Matrix) return Char_Matrix;

   type Rectangle is record
      Left, Right, Top, Bottom : Integer;
   end record;

   type Point is record
      X, Y : Integer;
   end record;

   function Contains (R : Rectangle; P : Point) return Boolean;
   procedure Inflate (R : in out Rectangle; Delta_X, Delta_Y : Integer);
end Advent;
