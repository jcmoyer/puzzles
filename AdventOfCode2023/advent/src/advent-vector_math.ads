--  Currently this package only supports integer vectors since AoC problems
--  rarely if ever deal with real numbers. The only counter-example I can think
--  of is 2019 day 10 which *could* be solved with real number trigonometry.

with Advent.Directions; use Advent.Directions;
with Ada.Containers;

generic
   type Element_Type is range <>;
package Advent.Vector_Math is

   pragma Pure;

   --
   --  Generic
   --
   type Axis is (X, Y, Z, W);

   --
   --  2D Vector math
   --
   subtype Axis2 is Axis range X .. Y;

   type Vec2 is array (Axis2) of Element_Type;
   function Image (V : Vec2) return String is ("<" & V (X)'Image & "," & V (Y)'Image & ">");

   function "-" (A : Vec2) return Vec2 is ((-A (X), -A (Y)));

   function "+" (A, B : Vec2) return Vec2 is ((A (X) + B (X), A (Y) + B (Y)));
   function "-" (A, B : Vec2) return Vec2 is ((A (X) - B (X), A (Y) - B (Y)));
   function "*" (A : Vec2; B : Element_Type) return Vec2 is ((A (X) * B, A (Y) * B));
   function "*" (A : Element_Type; B : Vec2) return Vec2 is (B * A);
   function "/" (A : Vec2; B : Element_Type) return Vec2 is ((A (X) / B, A (Y) / B));

   function Absolute (A : Vec2) return Vec2 is ((abs A (X), abs A (Y)));

   function Elementwise_Mul (A, B : Vec2) return Vec2 is ((A (X) * B (X), A (Y) * B (Y)));

   function Manhattan (A, B : Vec2) return Element_Type is
     (abs (A (X) - B (X)) + abs (A (Y) - B (Y)));

   function Fill (X : Element_Type) return Vec2 is ((X, X));

   --  Teschner, Matthias & Heidelberger, Bruno & Müller, Matthias &
   --  Pomeranets, Danat & Gross, Markus. (2003). Optimized Spatial Hashing for
   --  Collision Detection of Deformable Objects. VMV’03: Proceedings of the
   --  Vision, Modeling, Visualization. 3.
   use type Ada.Containers.Hash_Type;
   function Hash (A : Vec2) return Ada.Containers.Hash_Type is
     ((Ada.Containers.Hash_Type'Mod (A (X)) * 73_856_093) xor
      (Ada.Containers.Hash_Type'Mod (A (Y)) * 19_349_663));

   --  Returns a vector using conventional <row, col> matrix coordinates.
   function To_Vector (Dir : Direction) return Vec2;

   --
   --  3D Vector math
   --
   subtype Axis3 is Axis range X .. Z;

   type Vec3 is array (Axis3) of Element_Type;

   function Hash (A : Vec3) return Ada.Containers.Hash_Type is
     ((Ada.Containers.Hash_Type'Mod (A (X)) * 73_856_093) xor
      (Ada.Containers.Hash_Type'Mod (A (Y)) * 19_349_663) xor
      (Ada.Containers.Hash_Type'Mod (A (Z)) * 83_492_791));

end Advent.Vector_Math;
