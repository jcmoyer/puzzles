with Advent;         use Advent;
with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with Advent.Parsers.Integers;
with Advent.Vector_Math;

procedure Day24 is

   package Long_Parsers is new Advent.Parsers.Integers (Element_Type => Long_Long_Integer);

   package Long_Vectors is new Advent.Vector_Math (Element_Type => Long_Long_Integer);

   use Long_Vectors;

   type RVec2 is array (0 .. 1) of Long_Long_Float;
   function Image (V : RVec2) return String is ("<" & V (0)'Image & "," & V (1)'Image & ">");

   function "-" (A : RVec2) return RVec2 is ((-A (0), -A (1)));

   function "+" (A, B : RVec2) return RVec2 is ((A (0) + B (0), A (1) + B (1)));
   function "-" (A, B : RVec2) return RVec2 is ((A (0) - B (0), A (1) - B (1)));
   function "*" (A : RVec2; B : Long_Long_Float) return RVec2 is ((A (0) * B, A (1) * B));
   function "/" (A : RVec2; B : Long_Long_Float) return RVec2 is ((A (0) / B, A (1) / B));

   type Hailstone is record
      Position, Velocity : RVec2;
   end record;

   package Hailstone_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Hailstone);

   --  Integer to real vector
   function Real (V : Vec2) return RVec2 is ((Long_Long_Float (V (0)), Long_Long_Float (V (1))));

   --  Determinant of a 2x2 matrix, here treating the rows/columns as vectors
   function Det (A, B : RVec2) return Long_Long_Float is
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

   function Crosses (A, B : Hailstone; Where : out RVec2) return Boolean is
      --  As solved above
      T, U : Long_Long_Float;
      D    : constant Long_Long_Float := Det (A.Velocity, B.Velocity);
   begin
      --  Gee I sure hope eric didn't put any parallel rays in here because
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

   Lines      : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   Hailstones : Hailstone_Vectors.Vector;
begin

   for Line of Lines loop
      declare
         Ints : constant Long_Parsers.Vector := Long_Parsers.Extract_Integers (Line);
         P    : constant Vec2                := (Ints (1), Ints (2));
         V    : constant Vec2                := (Ints (4), Ints (5));
      begin
         Hailstones.Append ((Real (P), Real (V)));
      end;
   end loop;

   declare
      Where : RVec2;
      Sum   : Integer := 0;

      Test_First : constant Long_Long_Float := 200_000_000_000_000.0;
      Test_Last  : constant Long_Long_Float := 400_000_000_000_000.0;
   begin
      for I in Hailstones.First_Index .. Hailstones.Last_Index loop
         for J in I + 1 .. Hailstones.Last_Index loop
            if Crosses (Hailstones (I), Hailstones (J), Where) and then Where (0) >= Test_First
              and then Where (0) <= Test_Last and then Where (1) >= Test_First
              and then Where (1) <= Test_Last
            then
               Sum := Sum + 1;
            end if;
         end loop;
      end loop;
      Solution (Sum);
   end;

end Day24;
