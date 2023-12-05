with Advent;         use Advent;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day05 is
   ----------------------------------------------------------------------------
   --  Example 1: remap encloses
   ----------------------------------------------------------------------------
   --
   --             |-----A-----|                             << seeds
   --             .           .
   --        |--------------------|                         << soil
   --             .           .
   --             |-----------|                             << seeds pending
   --                   \                                      remap
   --                    \
   --                     V
   --                |-----A-----|                          << seeds remapped
   --
   --  When A is totally enclosed, the whole range is translated without
   --  splitting. The leftmost seed is always the solution, in this case A.Min.
   --
   ----------------------------------------------------------------------------
   --  Example 2: remap overlaps
   ----------------------------------------------------------------------------
   --
   --  |----A-----|          |--B---|                       << seeds
   --  .    ..    .          .  .   .
   --  .    .|------------------|   .                       << soil
   --  .    ..    .          .  .   .
   --  .    .|----|          |--|   .                       << seeds pending
   --  .    .    \              \   .                          remap
   --  .    .     \              \  .
   --  .    .      V              V .
   --  .    .    |-A2-|          |B1|                       << soil remap dest
   --  |-A1-|                    |B2|                       << seeds passed
   --                                                          without remap
   --
   --  Conceptually, A and B get split into two here. After all the remaps,
   --  A1.Min is the solution.
   --
   ----------------------------------------------------------------------------
   --  Example 3: remap splits
   ----------------------------------------------------------------------------
   --
   --        |-----------A-------------|                   << seeds
   --        .      .        .         .
   --        .      .|------|.         .                   << soil
   --        .      .    \   .         .
   --        |--A1--|     \  |---A3----|                   << seeds passed
   --                      \                                  without remap
   --                       V
   --                    |--A2--|                          << remapped seeds
   --
   --  Here, A1.Min is the solution.

   ----------------------------------------------------------------------------
   --  TODO: Refactor the interval stuff into type-generic package

   subtype I64 is Long_Long_Integer;

   type Interval is record
      Min : I64;
      Max : I64;
   end record;

   Empty_Interval : constant Interval := (Min => 0, Max => -1);

   function Image (R : Interval) return String is
     (R.Min'Image & ".." & R.Max'Image);

   function Singleton (Value : I64) return Interval is
     (Min => Value, Max => Value);

   function First (R : Interval) return I64 is (R.Min);
   function Last (R : Interval) return I64 is (R.Max);
   function Length (R : Interval) return I64 is (R.Max - R.Min + 1);

   function Contains (R : Interval; Val : I64) return Boolean is
     (Val in First (R) .. Last (R));

   function Contains (R, Enclosed : Interval) return Boolean is
     (Contains (R, Enclosed.Min) and then Contains (R, Enclosed.Max));

   function Overlaps (A, B : Interval) return Boolean is
     (Contains (A, First (B)) or else Contains (A, Last (B))
      or else Contains (B, First (A)) or else Contains (B, Last (A)));

   function Merge (A, B : Interval) return Interval is
     (Min => I64'Min (A.Min, B.Min), Max => I64'Max (A.Max, B.Max)) with
     Pre => Overlaps (A, B);

   function Intersect (A, B : Interval) return Interval is
   begin
      if Overlaps (A, B) then
         return (Min => I64'Max (A.Min, B.Min), Max => I64'Min (A.Max, B.Max));
      else
         return Empty_Interval;
      end if;
   end Intersect;

   package Interval_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Interval);
   subtype Interval_Vector is Interval_Vectors.Vector;

   type Multi_Interval is record
      Children : Interval_Vector;
   end record;

   procedure Clear (M : in out Multi_Interval) is
   begin
      M.Children.Clear;
   end Clear;

   function Copy (M : Multi_Interval) return Multi_Interval is
   begin
      return N : Multi_Interval do
         N.Children := M.Children.Copy;
      end return;
   end Copy;

   --  Ported from https://github.com/jcmoyer/puzzles/blob/e3a9ef6ef68fa71fd87942bfd283351cf6497f3b/AdventOfCode2022/src/day15.zig#L15-L36
   --  Can probably be significantly optimized.
   procedure Reduce (M : in out Multi_Interval) is
      Last_Size : Count_Type := 0;
      I, J      : Integer;
   begin
      while M.Children.Length /= Last_Size loop
         Last_Size := M.Children.Length;
         I         := M.Children.First_Index;
         while I <= M.Children.Last_Index loop
            J := I + 1;
            while J <= M.Children.Last_Index loop
               if Overlaps (M.Children (I), M.Children (J)) then
                  M.Children.Replace_Element
                    (I, Merge (M.Children (I), M.Children (J)));
                  M.Children.Swap (J, M.Children.Last_Index);
                  M.Children.Delete_Last;
               else
                  J := J + 1;
               end if;
            end loop;
            I := I + 1;
         end loop;
      end loop;
   end Reduce;

   procedure Insert (M : in out Multi_Interval; R : Interval) is
   begin
      M.Children.Append (R);
      Reduce (M);
   end Insert;

   procedure Insert (M : in out Multi_Interval; R : Multi_Interval) is
   begin
      for C of R.Children loop
         M.Children.Append (C);
      end loop;
      Reduce (M);
   end Insert;

   procedure Delete
     (M : in out Multi_Interval; R : Interval; Deleted : out Multi_Interval)
   is
      I : Integer := M.Children.First_Index;
   begin

      while I <= M.Children.Last_Index loop
         if Contains (R, M.Children (I)) then
            --  Child is entirely contained within R; swap-remove and look for
            --  others since R might have been larger than this interval.
            Insert (Deleted, M.Children (I));
            M.Children.Swap (I, M.Children.Last_Index);
            M.Children.Delete_Last;
         elsif Contains (M.Children (I), R) then
            --  The deletion is entirely contained within this Interval, so we
            --  have to split it in two. One of the new intervals may be zero sized.
            --  Since we perform reduction on insert, there's no way any other
            --  interval could be affected, so we can safely return after performing
            --  the split.
            declare
               Left  : constant Interval :=
                 (Min => M.Children (I).Min, Max => R.Min - 1);
               Right : constant Interval :=
                 (Min => R.Max + 1, Max => M.Children (I).Max);
            begin
               Insert (Deleted, R);
               M.Children.Swap (I, M.Children.Last_Index);
               M.Children.Delete_Last;
               if Length (Left) > 0 then
                  M.Children.Append (Left);
               end if;
               if Length (Right) > 0 then
                  M.Children.Append (Right);
               end if;
            end;
            return;
         elsif Overlaps (R, M.Children (I)) then
            --  R chops off one side of the interval.
            Insert (Deleted, Intersect (R, M.Children (I)));
            if Contains (R, M.Children (I).Min) then
               --  Left side
               M.Children.Replace_Element
                 (I, (Min => R.Max + 1, Max => M.Children (I).Max));
            else
               --  Right side
               M.Children.Replace_Element
                 (I, (Min => M.Children (I).Min, Max => R.Min - 1));
            end if;
            I := I + 1;
         else
            --  These intervals do not touch.
            I := I + 1;
         end if;
      end loop;
   end Delete;

   procedure Translate (M : in out Multi_Interval; Amount : I64) is
   begin
      for C of M.Children loop
         C.Min := C.Min + Amount;
         C.Max := C.Max + Amount;
      end loop;
   end Translate;

   procedure Translate
     (M : in out Multi_Interval; Span : Interval; Amount : I64)
   is
      I : Integer := M.Children.First_Index;

      --  Deleting from M while mutating it is pretty complicated, so we'll
      --  just use a temporary Multi_Interval
      New_Children : Multi_Interval;
      Deleted      : Multi_Interval;
   begin
      while I <= M.Children.Last_Index loop
         if Overlaps (Span, M.Children (I)) then
            declare
               Translate_Part : Interval := Intersect (Span, M.Children (I));
            begin
               Delete (M, Translate_Part, Deleted);
               Translate_Part.Min := Translate_Part.Min + Amount;
               Translate_Part.Max := Translate_Part.Max + Amount;
               Insert (New_Children, Translate_Part);
            end;
         else
            I := I + 1;
         end if;
      end loop;
      Insert (M, New_Children);
   end Translate;

   function First (M : Multi_Interval) return I64 is
      Result : I64 := I64'Last;
   begin
      for Child of M.Children loop
         Result := I64'Min (Result, Child.Min);
      end loop;
      return Result;
   end First;

   type Source_Dest_Range is record
      Source      : Interval;
      Destination : Interval;
   end record;

   function Translation (SDR : Source_Dest_Range) return I64 is
     (SDR.Destination.Min - SDR.Source.Min);

   package SDR_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Source_Dest_Range);
   subtype SDR_Vector is SDR_Vectors.Vector;

   function Parse_Source_Dest_Range (S : String) return Source_Dest_Range is
      Parts : constant String_Array := Split (S, " ");

      Dest_Start   : constant I64 := I64'Value (Parts (0));
      Source_Start : constant I64 := I64'Value (Parts (1));
      Length       : constant I64 := I64'Value (Parts (2));
   begin
      return
        (Source      => (Source_Start, Source_Start + Length - 1),
         Destination => (Dest_Start, Source_Start + Length - 1));
   end Parse_Source_Dest_Range;

   package I64_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => I64);
   subtype I64_Vector is I64_Vectors.Vector;

   Lines : constant String_Array := Read_All_Lines ("test/2023-05-input.txt");
   Seeds : I64_Vector;

   type Map_Kind is
     (Seed_Soil,
      Soil_Fert,
      Fert_Water,
      Water_Light,
      Light_Temp,
      Temp_Humid,
      Humid_Loc);

   Current_Map : Map_Kind := Seed_Soil;

   type Map_Chain is array (Map_Kind) of SDR_Vector;

   Maps : Map_Chain;

   function Map_Range
     (M : Multi_Interval; Map : SDR_Vector) return Multi_Interval
   is
      Input   : Multi_Interval := Copy (M);
      Result  : Multi_Interval;
      Deleted : Multi_Interval;
   begin
      for SDR of Map loop
         Delete (Input, SDR.Source, Deleted => Deleted);
         Translate (Deleted, Translation (SDR));
         Insert (Result, Deleted);
         Clear (Deleted);
      end loop;
      Insert (Result, Input);
      return Result;
   end Map_Range;

   function Map_Range (M : Multi_Interval) return Multi_Interval is
      Input : Multi_Interval := Copy (M);
   begin
      for K in Map_Kind'Range loop
         Input := Map_Range (Input, Maps (K));
      end loop;
      return Input;
   end Map_Range;

   Seeds_P1 : Multi_Interval;
   Seeds_P2 : Multi_Interval;

begin
   for Line of Lines loop
      if Starts_With (Line, "seeds: ") then
         declare
            Seeds_Strs : constant String_Array :=
              Split (Line (8 .. Line'Last), " ");
         begin
            for Str of Seeds_Strs loop
               Seeds.Append (I64'Value (Str));
            end loop;
         end;
      elsif Line = "seed-to-soil map:" then
         Current_Map := Seed_Soil;
      elsif Line = "soil-to-fertilizer map:" then
         Current_Map := Soil_Fert;
      elsif Line = "fertilizer-to-water map:" then
         Current_Map := Fert_Water;
      elsif Line = "water-to-light map:" then
         Current_Map := Water_Light;
      elsif Line = "light-to-temperature map:" then
         Current_Map := Light_Temp;
      elsif Line = "temperature-to-humidity map:" then
         Current_Map := Temp_Humid;
      elsif Line = "humidity-to-location map:" then
         Current_Map := Humid_Loc;
      elsif Line'Length > 0 then
         Maps (Current_Map).Append (Parse_Source_Dest_Range (Line));
      end if;
   end loop;

   --  Load into multi intervals
   declare
      I : Integer := Seeds.First_Index;
   begin
      while I <= Seeds.Last_Index loop
         Insert (Seeds_P1, Singleton (Seeds (I)));
         Insert (Seeds_P1, Singleton (Seeds (I + 1)));
         Insert
           (Seeds_P2,
            Interval'(Min => Seeds (I), Max => Seeds (I) + Seeds (I + 1) - 1));
         I := I + 2;
      end loop;
   end;

   Solution (First (Map_Range (Seeds_P1)));
   Solution (First (Map_Range (Seeds_P2)));
end Day05;
