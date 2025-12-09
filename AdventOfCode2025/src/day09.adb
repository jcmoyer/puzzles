with Advent.IO;           use Advent.IO;
with Advent.Long_Parsers; use Advent.Long_Parsers;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Advent.Strings;
with Advent.Vector_Math;

procedure Day09 is
   package Long_Vectors is new Advent.Vector_Math (Element_Type => Long_Long_Integer);
   use Long_Vectors;

   package Vec2_Vectors is new Ada.Containers.Vectors (Positive, Vec2);

   type AABB is record
      Min, Max : Vec2;
   end record
   with Dynamic_Predicate => Min (X) <= Max (X) and Min (Y) <= Max (Y);

   function Left (Box : AABB) return Long_Long_Integer
   is (Box.Min (X));

   function Right (Box : AABB) return Long_Long_Integer
   is (Box.Max (X));

   function Top (Box : AABB) return Long_Long_Integer
   is (Box.Min (Y));

   function Bottom (Box : AABB) return Long_Long_Integer
   is (Box.Max (Y));

   --  P..Q forms a line segment along the perimeter of the polygon. The puzzle
   --  text states these lines are always horizontal or vertical.
   type Edge is record
      P, Q : Vec2;
   end record
   with Dynamic_Predicate => P (X) = Q (X) or P (Y) = Q (Y);

   package Edge_Vectors is new Ada.Containers.Vectors (Positive, Edge);

   function Left (E : Edge) return Long_Long_Integer
   is (Long_Long_Integer'Min (E.P (X), E.Q (X)));

   function Right (E : Edge) return Long_Long_Integer
   is (Long_Long_Integer'Max (E.P (X), E.Q (X)));

   function Top (E : Edge) return Long_Long_Integer
   is (Long_Long_Integer'Min (E.P (Y), E.Q (Y)));

   function Bottom (E : Edge) return Long_Long_Integer
   is (Long_Long_Integer'Max (E.P (Y), E.Q (Y)));

   function Is_Vertical (E : Edge) return Boolean
   is (E.P (X) = E.Q (X));

   function Is_Horizontal (E : Edge) return Boolean
   is (E.P (Y) = E.Q (Y));

   function Inside (Box : AABB; E : Edge) return Boolean is
   begin
      if Is_Vertical (E) then
         return
           Left (E) > Left (Box)
           and then Left (E) < Right (Box)
           and then ((Top (E) < Bottom (Box) and then Bottom (E) > Top (Box)));
      elsif Is_Horizontal (E) then
         return
           Top (E) > Top (Box)
           and then Top (E) < Bottom (Box)
           and then (Left (E) < Right (Box) and then Right (E) > Left (Box));
      end if;
      raise Program_Error with "unreachable";
   end Inside;

   function Any_Inside (Box : AABB; Edges : Edge_Vectors.Vector) return Boolean
   is (for some E of Edges => Inside (Box, E));

   function Min (A, B : Vec2) return Vec2
   is ((Long_Long_Integer'Min (A (X), B (X)), Long_Long_Integer'Min (A (Y), B (Y))));

   function Max (A, B : Vec2) return Vec2
   is ((Long_Long_Integer'Max (A (X), B (X)), Long_Long_Integer'Max (A (Y), B (Y))));

   ----------------------------------------------------------------------------

   Lines : constant Advent.Strings.String_Array :=
     Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

   Points : Vec2_Vectors.Vector;
   Ints   : Array_Type (1 .. 2);
   N_Ints : Integer;

   Box_Area : Long_Long_Integer;

   Area_P1, Area_P2 : Long_Long_Integer := 0;

   Edges : Edge_Vectors.Vector;

   P, Q : Vec2;
   Box  : AABB;

begin
   for Line of Lines loop
      N_Ints := Extract_Integers (Line, Ints);
      if N_Ints /= 2 then
         raise Program_Error with "expected 2 ints per line";
      end if;

      Points.Append (Vec2'(Ints (1), Ints (2)));
   end loop;

   for I in 1 .. Points.Last_Index loop
      P := Points (I);
      if I + 1 in 1 .. Points.Last_Index then
         Q := Points (I + 1);
      else
         Q := Points.First_Element;
      end if;
      Edges.Append (Edge'(P, Q));
   end loop;

   for I in Points.First_Index .. Points.Last_Index loop
      for J in I + 1 .. Points.Last_Index loop
         Box_Area :=
           (abs (Points (I) (X) - Points (J) (X)) + 1)
           * (abs (Points (I) (Y) - Points (J) (Y)) + 1);
         Area_P1 := Long_Long_Integer'Max (Area_P1, Box_Area);

         Box := (Min (Points (I), Points (J)), Max (Points (I), Points (J)));
         if not Any_Inside (Box, Edges) then
            Area_P2 := Long_Long_Integer'Max (Area_P2, Box_Area);
         end if;
      end loop;
   end loop;

   Solution (Area_P1);
   Solution (Area_P2);
end Day09;
