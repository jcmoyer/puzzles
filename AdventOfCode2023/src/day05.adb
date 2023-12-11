with Advent;         use Advent;
with Advent.Intervals;
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

   subtype I64 is Long_Long_Integer;

   package I64_Intervals is new Advent.Intervals (Element_Type => I64);
   use I64_Intervals;

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

   package I64_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => I64);
   subtype I64_Vector is I64_Vectors.Vector;

   type Map_Kind is
     (Seed_Soil, Soil_Fert, Fert_Water, Water_Light, Light_Temp, Temp_Humid, Humid_Loc);

   type Map_Chain is array (Map_Kind) of SDR_Vector;

   function Map_Range (M : Multi_Interval; Map : SDR_Vector) return Multi_Interval is
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

   function Map_Range (M : Multi_Interval; Maps : Map_Chain) return Multi_Interval is
      Input : Multi_Interval := Copy (M);
   begin
      for K in Map_Kind'Range loop
         Input := Map_Range (Input, Maps (K));
      end loop;
      return Input;
   end Map_Range;

   Lines       : constant String_Array := Read_All_Lines ("test/2023-05-input.txt");
   Seeds       : I64_Vector;
   Current_Map : Map_Kind              := Seed_Soil;
   Maps        : Map_Chain;

   Seeds_P1 : Multi_Interval;
   Seeds_P2 : Multi_Interval;

begin
   for Line of Lines loop
      if Starts_With (Line, "seeds: ") then
         declare
            Seeds_Strs : constant String_Array := Split (Line (8 .. Line'Last), " ");
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
         Insert (Seeds_P2, Interval'(Min => Seeds (I), Max => Seeds (I) + Seeds (I + 1) - 1));
         I := I + 2;
      end loop;
   end;

   Solution (First (Map_Range (Seeds_P1, Maps)));
   Solution (First (Map_Range (Seeds_P2, Maps)));
end Day05;
