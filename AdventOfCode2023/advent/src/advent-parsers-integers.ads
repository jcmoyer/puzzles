with Ada.Containers.Vectors;

generic
   type Element_Type is range <>;

   with package Element_Vectors is new Ada.Containers.Vectors (
      Index_Type => <>,
      Element_Type => Element_Type
   );
package Advent.Parsers.Integers is

   pragma Preelaborate;

   subtype Vector is Element_Vectors.Vector;

   function Extract_Integers (S : String) return Vector;

   type Array_Type is array (Positive range <>) of Element_Type;

   function Extract_Integers (S : String; Output : out Array_Type) return Natural;

end Advent.Parsers.Integers;
