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
   --  2D Vector math
   --
   type Vec2 is array (0 .. 1) of Element_Type;
   function Image (V : Vec2) return String is ("<" & V (0)'Image & "," & V (1)'Image & ">");

   function "-" (A : Vec2) return Vec2 is ((-A (0), -A (1)));

   function "+" (A, B : Vec2) return Vec2 is ((A (0) + B (0), A (1) + B (1)));
   function "-" (A, B : Vec2) return Vec2 is ((A (0) - B (0), A (1) - B (1)));
   function "*" (A : Vec2; B : Element_Type) return Vec2 is ((A (0) * B, A (1) * B));
   function "/" (A : Vec2; B : Element_Type) return Vec2 is ((A (0) / B, A (1) / B));

   function Absolute (A : Vec2) return Vec2 is ((abs A (0), abs A (1)));

   function Elementwise_Mul (A, B : Vec2) return Vec2 is ((A (0) * B (0), A (1) * B (1)));

   function Manhattan (A, B : Vec2) return Element_Type is
     (abs (A (0) - B (0)) + abs (A (1) - B (1)));

   --  Teschner, Matthias & Heidelberger, Bruno & Müller, Matthias &
   --  Pomeranets, Danat & Gross, Markus. (2003). Optimized Spatial Hashing for
   --  Collision Detection of Deformable Objects. VMV’03: Proceedings of the
   --  Vision, Modeling, Visualization. 3.
   use type Ada.Containers.Hash_Type;
   function Hash (A : Vec2) return Ada.Containers.Hash_Type is
     ((Ada.Containers.Hash_Type'Mod (A (0)) * 73_856_093) xor
      (Ada.Containers.Hash_Type'Mod (A (1)) * 19_349_663));

   --  Returns a vector using conventional <row, col> matrix coordinates.
   function To_Vector (Dir : Direction) return Vec2;

   --
   --  3D Vector math
   --
   type Vec3 is array (0 .. 2) of Element_Type;

end Advent.Vector_Math;
