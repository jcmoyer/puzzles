with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;

procedure Day22 is

   type U64 is mod 2**64;

   type Price_Type is range 0 .. 9;

   type Price_Diff is range -9 .. +9;

   type Diff_Array is array (Positive range <>) of Price_Diff;

   subtype Diff_Window is Diff_Array (1 .. 4);

   type Seen_Windows_Array is
     array (Price_Diff, Price_Diff, Price_Diff, Price_Diff) of Boolean with
     Default_Component_Value => False;

   type Window_Score_Array is array (Price_Diff, Price_Diff, Price_Diff, Price_Diff) of U64 with
     Default_Component_Value => 0;

   function Mix (Value, Secret : U64) return U64 is
   begin
      return Value xor Secret;
   end Mix;

   function Prune (Secret : U64) return U64 is
   begin
      return Secret mod 16_777_216;
   end Prune;

   function Evolve (Secret : U64) return U64 is
      A : constant U64 := Prune (Mix (Secret * 64, Secret));
      B : constant U64 := Prune (Mix (A / 32, A));
      C : constant U64 := Prune (Mix (B * 2_048, B));
   begin
      return C;
   end Evolve;

   function Price (Secret : U64) return Price_Type is
   begin
      return Price_Type (Secret rem 10);
   end Price;

   function Diff (Secret_1, Secret_0 : U64) return Price_Diff is
   begin
      return Price_Diff (Price (Secret_1)) - Price_Diff (Price (Secret_0));
   end Diff;

   Prices      : array (1 .. 2_000) of Price_Type;
   Price_Diffs : Diff_Array (1 .. 2_000);

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Total_Prices : Window_Score_Array;
   Last         : U64;
   Secret       : U64;
   Sum          : U64 := 0;
   Seen         : Seen_Windows_Array;
begin
   for Line of Lines loop
      Secret := U64'Value (Line);
      Last   := Secret;

      for I in 1 .. 2_000 loop
         Secret          := Evolve (Secret);
         Prices (I)      := Price (Secret);
         Price_Diffs (I) := Diff (Secret, Last);
         Last            := Secret;
      end loop;

      Sum := Sum + Secret;

      Seen := (others => (others => (others => (others => False))));

      for I in Price_Diffs'First .. Price_Diffs'Last - 3 loop
         declare
            Diffs      : constant Diff_Window := Price_Diffs (I .. I + 3);
            Sell_Price : constant Price_Type  := Prices (I + 3);
         begin
            if not Seen (Diffs (1), Diffs (2), Diffs (3), Diffs (4)) then
               Seen (Diffs (1), Diffs (2), Diffs (3), Diffs (4)) := True;

               Total_Prices (Diffs (1), Diffs (2), Diffs (3), Diffs (4)) :=
                 Total_Prices (Diffs (1), Diffs (2), Diffs (3), Diffs (4)) + U64 (Sell_Price);
            end if;
         end;
      end loop;
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
