with Advent.Containers.String_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Advent.Strings is

   pragma Preelaborate;

   subtype String_Array is Advent.Containers.String_Vectors.Vector;

   function Is_Any_Of (C : Character; Vals : String) return Boolean;

   --  When I <= J, returns S(I..J); otherwise returns an empty string.
   function Slice_Empty (S : String; I, J : Integer) return String;

   function Unbounded_Slice_Empty
     (S : Unbounded_String; Low, High : Integer) return Unbounded_String;

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

end Advent.Strings;
