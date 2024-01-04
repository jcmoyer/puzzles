with Advent;         use Advent;
with Advent.IO;      use Advent.IO;
with Advent.Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

procedure Day11 is
   package Long_Vector_Math is new Advent.Vector_Math (Element_Type => Long_Long_Integer);

   use Long_Vector_Math;

   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Integer);

   type Galaxy is record
      Position : Vec2;
   end record;

   package Galaxy_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Galaxy);

   subtype Galaxy_Vector is Galaxy_Vectors.Vector;

   --  Returns an array of galaxies and discards non-# characters.
   function Find_Galaxies (M : Char_Matrix) return Galaxy_Vector is
      Result : Galaxy_Vector;
   begin
      for I in 1 .. Rows (M) loop
         for J in 1 .. Cols (M) loop
            if M (I, J) = '#' then
               Result.Append ((Position => (Long_Long_Integer (I), Long_Long_Integer (J))));
            end if;
         end loop;
      end loop;
      return Result;
   end Find_Galaxies;

   --  Returns the indices of columns that need to be expanded.
   function Find_Expandable_Columns (M : Char_Matrix) return Integer_Vectors.Vector is
      Result : Integer_Vectors.Vector;
   begin
      for J in 1 .. Cols (M) loop
         declare
            Found_Galaxy : Boolean := False;
         begin
            for I in 1 .. Rows (M) loop
               --  Stop looking in this column if we found a galaxy
               if M (I, J) = '#' then
                  Found_Galaxy := True;
                  exit;
               end if;
            end loop;
            if not Found_Galaxy then
               Result.Append (J);
            end if;
         end;
      end loop;
      return Result;
   end Find_Expandable_Columns;

   --  Returns the indices of rows that need to be expanded.
   function Find_Expandable_Rows (M : Char_Matrix) return Integer_Vectors.Vector is
      Result : Integer_Vectors.Vector;
   begin
      for I in 1 .. Rows (M) loop
         declare
            Found_Galaxy : Boolean := False;
         begin
            for J in 1 .. Cols (M) loop
               --  Stop looking in this row if we found a galaxy
               if M (I, J) = '#' then
                  Found_Galaxy := True;
                  exit;
               end if;
            end loop;
            if not Found_Galaxy then
               Result.Append (I);
            end if;
         end;
      end loop;
      return Result;
   end Find_Expandable_Rows;

   --  Expands all galaxies as if we had replaced empty rows/columns with Count
   --  rows/columns.
   --
   --  We only care about the relative position of galaxies, so simply shift
   --  their positions instead of doing expensive reallocation/copying stuff
   --  with the matrix. This is a destructive operation and you cannot Expand a
   --  universe multiple times since shifting galaxies will reposition them
   --  relative to the empty rows/columns in the input.
   procedure Expand
     (M : Char_Matrix; Galaxies : in out Galaxy_Vector; Count : Long_Long_Integer := 1)
   is
      Col_Indices : constant Integer_Vectors.Vector := Find_Expandable_Columns (M);
      Row_Indices : constant Integer_Vectors.Vector := Find_Expandable_Rows (M);

      --  The input data already has empty rows and columns, and this algorithm
      --  behaves as if we're *inserting* new rows/columns not replacing
      --  existing ones, so subtract 1.
      Insert_Count : constant Long_Long_Integer := Count - 1;
   begin
      for I of Col_Indices loop
         for G of Galaxies loop
            if G.Position (1) < Long_Long_Integer (I) then
               G.Position (1) := G.Position (1) - Insert_Count;
            end if;
         end loop;
      end loop;

      for I of Row_Indices loop
         for G of Galaxies loop
            if G.Position (0) < Long_Long_Integer (I) then
               G.Position (0) := G.Position (0) - Insert_Count;
            end if;
         end loop;
      end loop;
   end Expand;

   --  Returns the sum of manhattan distances between all pairs of galaxies.
   function Sum_Distances (Galaxies : Galaxy_Vector) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
   begin
      for I in Galaxies.First_Index .. Galaxies.Last_Index loop
         for J in I + 1 .. Galaxies.Last_Index loop
            Result := Result + Manhattan (Galaxies (I).Position, Galaxies (J).Position);
         end loop;
      end loop;
      return Result;
   end Sum_Distances;

   --  locals
   Map         : constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));
   Galaxies_P1 : Galaxy_Vector        := Find_Galaxies (Map);
   Galaxies_P2 : Galaxy_Vector        := Galaxies_P1.Copy;

begin

   Expand (Map, Galaxies_P1, Count => 2);
   Solution (Sum_Distances (Galaxies_P1));

   Expand (Map, Galaxies_P2, Count => 1_000_000);
   Solution (Sum_Distances (Galaxies_P2));

end Day11;
