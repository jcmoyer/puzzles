with Advent;         use Advent;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Advent.Vector_Math;
with Advent.Long_Parsers;
with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Numerics.Long_Long_Real_Arrays;

procedure Day24 is

   package Long_Vectors is new Advent.Vector_Math (Element_Type => Long_Long_Integer);

   use Long_Vectors;

   --  Lucky us, Ada provides the linear algebra functions we need for part 2
   --  in the standard library!
   package RA renames Ada.Numerics.Long_Long_Real_Arrays;

   --  Bring operators into scope.
   use type RA.Real_Vector;
   use type RA.Real_Matrix;

   subtype RVec3 is RA.Real_Vector (0 .. 2);
   subtype RMat3 is RA.Real_Matrix (0 .. 2, 0 .. 2);

   type Hailstone is record
      Position, Velocity : RVec3;
   end record;

   package Hailstone_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Hailstone);

   --  Integer to real vector
   function Real (V : Vec3) return RVec3 is
     ((Long_Long_Float (V (0)), Long_Long_Float (V (1)), Long_Long_Float (V (2))));

   --  Determinant of a 2x2 matrix, here treating the rows/columns as vectors
   --  This intentionally takes a 3D vector and ignores the Z component.
   function Det (A, B : RVec3) return Long_Long_Float is
   begin
      return (A (1) * B (0) - A (0) * B (1));
   end Det;

   --  Part 1 asks if any paths cross in the test area, notably it doesn't ask
   --  if they *collide*.
   --
   --  We can scale a vector P moving at V an arbitrary amount t by: P + V t
   --  (this defines a point along a ray chosen by varying t)
   --
   --  Find the intersection of two rays. We know P and V, so we need to find t.
   --  Use a system of equations.
   --
   --  P = pos 1,  Q = pos 2
   --  V = vel 1,  W = vel 2
   --  T = time 1, U = time 2
   --
   --  P_x + V_x T = Q_x + W_x U
   --  P_y + V_y T = Q_y + W_y U
   --
   --  Solve T
   --
   --        V_x T = Q_x + W_x U - P_x
   --            T = (Q_x + W_x U - P_x) / V_x
   --
   --  Substitute T into equation 2
   --
   --           P_y + V_y ((Q_x + W_x U - P_x) / V_x) = Q_y + W_y U
   --
   --  Solve U
   --
   --           P_y + V_y ((Q_x + W_x U - P_x) / V_x) = Q_y + W_y U
   --  (V_y Q_x + V_y W_x U - V_y P_x) / V_x) - W_y U = Q_y - P_y
   --       V_y Q_x + V_y W_x U - V_y P_x - V_x W_y U = V_x (Q_y - P_y)
   --                           V_y W_x U - V_x W_y U = V_x (Q_y - P_y) + V_y P_x - V_y Q_x
   --                           U (V_y W_x - V_x W_y) = V_x (Q_y - P_y) + V_y P_x - V_y Q_x
   --                                               U = (V_x (Q_y - P_y) + V_y P_x - V_y Q_x) / (V_y W_x - V_x W_y)
   --                                               U = (V_x (Q_y - P_y) + V_y (P_x - Q_x)) / (V_y W_x - V_x W_y)
   --
   --

   function Crosses (A, B : Hailstone; Where : out RVec3) return Boolean is
      --  As solved above
      T, U : Long_Long_Float;
      D    : constant Long_Long_Float := Det (A.Velocity, B.Velocity);
   begin
      --  Gee I sure hope eric didn't put any overlapping rays in here because
      --  this isn't strictly correct. U is only defined if this value is != 0,
      --  which may mean the lines don't intersect or they're overlapping.
      if D = 0.0 then
         return False;
      end if;

      U :=
        (A.Velocity (0) * (B.Position (1) - A.Position (1)) +
         A.Velocity (1) * (A.Position (0) - B.Position (0))) /
        D;

      T := (B.Position (0) + B.Velocity (0) * U - A.Position (0)) / A.Velocity (0);

      if U > 0.0 and then T > 0.0 then
         Where := B.Position + B.Velocity * U;
         return True;
      end if;

      return False;
   end Crosses;

   --  Part 2 wants us to find the ray that intersects all the hailstones as
   --  they move.
   --
   --  Input observations:
   --
   --  1. 300 hailstones. The positions are extremely large (1e14) and the
   --     velocities are small (1e2). There is also a large variation in
   --     positions which implies t may be a large number.
   --
   --  2. There may be time gaps between the hailstone collisions (See example,
   --     no collision at t=2). Hailstones will be some multiple of our
   --     projectile's V apart when in the correct position.
   --
   --  3. The collision time of each hailstone must be unique (implied by the
   --     line "it doesn't seem like any hailstones will naturally collide",
   --     therefore hailstones will never overlap and thus the projectile can
   --     never hit more than one at a time)
   --
   --  I originally solved this problem using z3, but I've implemented evouga's
   --  solution in Ada for completeness. The gist is that you build a 6x6
   --  system of linear equations and solve using Gaussian elimination.
   --
   --  Source:
   --  https://old.reddit.com/r/adventofcode/comments/18pnycy/2023_day_24_solutions/kepu26z/

   --  Standard vector cross product
   function Cross (A, B : RVec3) return RVec3 is
   begin
      return
        (A (1) * B (2) - A (2) * B (1),
         A (2) * B (0) - A (0) * B (2),
         A (0) * B (1) - A (1) * B (0));
   end Cross;

   function Cross_Matrix (V : RVec3) return RMat3 is
   begin
      --!pp off
      return (
        (  0.0 , -V (2),  V (1)),
        ( V (2),   0.0 , -V (0)),
        (-V (1),  V (0),   0.0 )
      );
      --!pp on
   end Cross_Matrix;

   --  Copies the entire `Source` matrix into `Destination` starting at `Row`,`Col`.
   procedure Copy_Into
     (Source : RA.Real_Matrix; Destination : in out RA.Real_Matrix; Row, Col : Integer)
   is
   begin
      for I in Row .. Row + Source'Length (1) - 1 loop
         for J in Col .. Col + Source'Length (2) - 1 loop
            Destination (I, J) := Source (Source'First (1) + I - Row, Source'First (2) + J - Col);
         end loop;
      end loop;
   end Copy_Into;

   subtype Test_Range is Long_Long_Float range 200_000_000_000_000.0 .. 400_000_000_000_000.0;

   Lines      : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   Hailstones : Hailstone_Vectors.Vector;
begin

   for Line of Lines loop
      declare
         Ints : constant Long_Parsers.Vector := Long_Parsers.Extract_Integers (Line);
         P    : constant Vec3                := (Ints (1), Ints (2), Ints (3));
         V    : constant Vec3                := (Ints (4), Ints (5), Ints (6));
      begin
         Hailstones.Append ((Real (P), Real (V)));
      end;
   end loop;

   --  Part 1
   declare
      Where : RVec3;
      Sum   : Integer := 0;
   begin
      for I in Hailstones.First_Index .. Hailstones.Last_Index loop
         for J in I + 1 .. Hailstones.Last_Index loop
            if Crosses (Hailstones (I), Hailstones (J), Where) and then Where (0) in Test_Range
              and then Where (1) in Test_Range
            then
               Sum := Sum + 1;
            end if;
         end loop;
      end loop;
      Solution (Sum);
   end;

   --  Part 2
   declare
      A      : RA.Real_Matrix (0 .. 5, 0 .. 5);
      X      : RA.Real_Vector (0 .. 5);
      Result : RA.Real_Vector (0 .. 5);
      Sum    : Long_Long_Integer := 0;
   begin
      X (0 .. 2) :=
        -Cross (Hailstones (1).Position, Hailstones (1).Velocity) +
        Cross (Hailstones (2).Position, Hailstones (2).Velocity);
      X (3 .. 5) :=
        -Cross (Hailstones (1).Position, Hailstones (1).Velocity) +
        Cross (Hailstones (3).Position, Hailstones (3).Velocity);

      Copy_Into
        (Source => Cross_Matrix (Hailstones (1).Velocity) - Cross_Matrix (Hailstones (2).Velocity),
         Destination => A,
         Row         => 0,
         Col         => 0);

      Copy_Into
        (Source => Cross_Matrix (Hailstones (1).Velocity) - Cross_Matrix (Hailstones (3).Velocity),
         Destination => A,
         Row         => 3,
         Col         => 0);

      Copy_Into
        (Source      =>
           -Cross_Matrix (Hailstones (1).Position) + Cross_Matrix (Hailstones (2).Position),
         Destination => A,
         Row         => 0,
         Col         => 3);

      Copy_Into
        (Source      =>
           -Cross_Matrix (Hailstones (1).Position) + Cross_Matrix (Hailstones (3).Position),
         Destination => A,
         Row         => 3,
         Col         => 3);

      Result := RA.Inverse (A) * X;

      for Val of Result (0 .. 2) loop
         Sum := Sum + Long_Long_Integer (Val);
      end loop;

      Solution (Sum);
   end;

end Day24;
