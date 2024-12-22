with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers; use Ada.Containers;

procedure Day22 is

   type U64 is mod 2**64;

   type Price_Type is range 0 .. 9;

   type Price_Diff is range -9 .. +9;

   type Diff_Array is array (1 .. 4) of Price_Diff;

   --  Randomly chosen primes. TODO: algorithmically determine these; they can
   --  vary runtime from a couple seconds to over a minute.
   function Hash (T : Diff_Array) return Hash_Type is
     (Hash_Type'Mod (T (1)) * 216_091 xor Hash_Type'Mod (T (2)) * 110_503 xor
      Hash_Type'Mod (T (3)) * 57_885_161 xor Hash_Type'Mod (T (4)) * 30_402_457);

   package Diff_Array_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Diff_Array, Element_Type => U64, Hash => Hash, Equivalent_Keys => "=");

   package Diff_Array_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Diff_Array, Hash => Hash, Equivalent_Elements => "=");

   function Mix (Value, Secret : U64) return U64 is
   begin
      return Value xor Secret;
   end Mix;

   function Prune (Secret : U64) return U64 is
   begin
      return Secret mod 16_777_216;
   end Prune;

   procedure Evolve (Secret : in out U64) is
   begin
      Secret := Prune (Mix (Secret * 64, Secret));
      Secret := Prune (Mix (Secret / 32, Secret));
      Secret := Prune (Mix (Secret * 2_048, Secret));
   end Evolve;

   function Price (Secret : U64) return Price_Type is
   begin
      return Price_Type (Secret rem 10);
   end Price;

   function Diff (Secret_1, Secret_0 : U64) return Price_Diff is
   begin
      return Price_Diff (Price (Secret_1)) - Price_Diff (Price (Secret_0));
   end Diff;

   package U64_Vectors is new Ada.Containers.Vectors (Positive, U64);

   package Price_Diff_Vectors is new Ada.Containers.Vectors (Positive, Price_Diff);

   Secrets     : U64_Vectors.Vector;
   Price_Diffs : Price_Diff_Vectors.Vector;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Total_Prices : Diff_Array_Maps.Map;
   Last         : U64 := U64'Last;
   Secret       : U64;
   Sum          : U64 := 0;
   Seen         : Diff_Array_Sets.Set;
begin
   for Line of Lines loop
      Secret := U64'Value (Line);
      Last   := Secret;

      for I in 1 .. 2_000 loop
         Evolve (Secret);
         Secrets.Append (Secret);
         Price_Diffs.Append (Diff (Secret, Last));
         Last := Secret;
      end loop;

      for I in Price_Diffs.First_Index .. Price_Diffs.Last_Index - 3 loop
         declare
            Diffs : constant Diff_Array :=
              (Price_Diffs (I), Price_Diffs (I + 1), Price_Diffs (I + 2), Price_Diffs (I + 3));

            Sell_Price : constant Price_Type := Price (Secrets (I + 3));
         begin
            if not Seen.Contains (Diffs) then
               Seen.Include (Diffs);
               if Total_Prices.Contains (Diffs) then
                  Total_Prices.Include (Diffs, Total_Prices.Element (Diffs) + U64 (Sell_Price));
               else
                  Total_Prices.Include (Diffs, U64 (Sell_Price));
               end if;
            end if;
         end;
      end loop;

      Secrets.Clear;
      Price_Diffs.Clear;
      Seen.Clear;

      Sum := Sum + Secret;
   end loop;

   Solution (Long_Long_Integer (Sum));

   declare
      Max : U64 := 0;
   begin
      for Price of Total_Prices loop
         Max := U64'Max (Max, Price);
      end loop;

      Solution (Long_Long_Integer (Max));
   end;
end Day22;
