with Ada.Containers.Vectors;

generic
   type Element_Type is range <>;
package Advent.Parsers.Integers is

   pragma Preelaborate;

   package Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Element_Type);

   subtype Vector is Vectors.Vector;

   function Extract_Integers (S : String) return Vector;

end Advent.Parsers.Integers;
