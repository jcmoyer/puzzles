with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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

   --  Returns an array of strings where each element corresponds to a single
   --  line in the input file.
   function Read_All_Lines (Filename : String) return String_Array;

   type Char_Matrix is array (Integer range <>, Integer range <>) of Character;

   function Read_Tilemap (Filename : String) return Char_Matrix;
   function Rows (M : Char_Matrix) return Integer;
   function Cols (M : Char_Matrix) return Integer;

   type Rectangle is record
      Left, Right, Top, Bottom : Integer;
   end record;

   type Point is record
      X, Y : Integer;
   end record;

   function Contains (R : Rectangle; P : Point) return Boolean;
   procedure Inflate (R : in out Rectangle; Delta_X, Delta_Y : Integer);

end Advent;
