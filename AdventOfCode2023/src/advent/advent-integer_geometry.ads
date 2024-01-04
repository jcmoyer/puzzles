package Advent.Integer_Geometry is

   pragma Pure;

   type Rectangle is record
      Left, Right, Top, Bottom : Integer;
   end record;

   type Point is record
      X, Y : Integer;
   end record;

   function Contains (R : Rectangle; P : Point) return Boolean;
   procedure Inflate (R : in out Rectangle; Delta_X, Delta_Y : Integer);

end Advent.Integer_Geometry;
