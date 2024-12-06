with Ada.Containers.Vectors;

generic
   type Element_Type is range <>;

   with package Element_Vectors is new Ada.Containers.Vectors (
      Index_Type => <>,
      Element_Type => Element_Type
   );
package Advent.Generic_Integer_Math is

   pragma Preelaborate;

   subtype Vector is Element_Vectors.Vector;

   use type Ada.Containers.Count_Type;

   function Gcd (A, B : Element_Type) return Element_Type;

   function Lcm (A, B : Element_Type) return Element_Type;

   function Lcm (Xs : Vector) return Element_Type with
     Pre => Xs.Length > 0;

end Advent.Generic_Integer_Math;
