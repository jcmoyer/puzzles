with Ada.Containers.Vectors;

generic
   type Element_Type is range <>;
package Advent.Integers is

   package Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Element_Type);

   subtype Vector is Vectors.Vector;

   use type Ada.Containers.Count_Type;

   function Gcd (A, B : Element_Type) return Element_Type;

   function Lcm (A, B : Element_Type) return Element_Type;

   function Lcm (Xs : Vector) return Element_Type with
     Pre => Xs.Length > 0;

end Advent.Integers;
