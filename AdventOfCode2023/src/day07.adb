with Advent;         use Advent;
with Advent.Parsers.Integers;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day07 is
   --  A hand consists of five cards labeled one of A, K, Q, J, T, 9, 8, 7, 6,
   --  5, 4, 3, or 2.
   type Card is ('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A');

   --  Part 2: J < everything else
   function Joker_Rule_Pos (C : Card) return Integer is
   begin
      if C = 'J' then
         return Card'Pos (Card'First) - 1;
      else
         return Card'Pos (C);
      end if;
   end Joker_Rule_Pos;

   type Hand is array (1 .. 5) of Card;

   --  Comparison function for part 2 hands
   function Less_Than_Joker (Left, Right : Hand) return Boolean is
   begin
      for I in Hand'Range loop
         if Joker_Rule_Pos (Left (I)) < Joker_Rule_Pos (Right (I)) then
            return True;
         end if;
         if Joker_Rule_Pos (Left (I)) > Joker_Rule_Pos (Right (I)) then
            return False;
         end if;
      end loop;
      return False;
   end Less_Than_Joker;

   --  Returns the first index of a card in hand if it exists or 0 otherwise.
   function Index (H : Hand; Elem : Card) return Natural is
   begin
      for I in H'Range loop
         if H (I) = Elem then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   function Image (H : Hand) return String is
     (H (1)'Image & H (2)'Image & H (3)'Image & H (4)'Image & H (5)'Image);

   type Hand_Type is (High_Card, One_Pair, Two_Pair, Three_Kind, Full_House, Four_Kind, Five_Kind);

   function Classify (H : Hand) return Hand_Type is
      Counts : array (Card) of Integer := (others => 0);

      --  Returns the number of distinct cards in the hand that have N copies.
      function Copy_Count (N : Integer) return Integer is
         Result : Integer := 0;
      begin
         for I in Counts'Range loop
            if Counts (I) = N then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Copy_Count;
   begin
      for C of H loop
         Counts (C) := Counts (C) + 1;
      end loop;

      if Copy_Count (5) = 1 then
         return Five_Kind;
      elsif Copy_Count (4) = 1 then
         return Four_Kind;
      elsif Copy_Count (3) = 1 and then Copy_Count (2) = 1 then
         return Full_House;
      elsif Copy_Count (3) = 1 and then Copy_Count (1) = 2 then
         return Three_Kind;
      elsif Copy_Count (2) = 2 then
         return Two_Pair;
      elsif Copy_Count (2) = 1 and then Copy_Count (1) = 3 then
         return One_Pair;
      elsif Copy_Count (1) = 5 then
         return High_Card;
      end if;
   end Classify;

   function Classify_Joker (H : Hand) return Hand_Type is
      package Hand_Vectors is new Ada.Containers.Vectors
        (Index_Type => Positive, Element_Type => Hand);

      Search_States : Hand_Vectors.Vector;
      Best          : Hand_Type := Classify (H);
   begin
      Search_States.Append (H);

      --  Adjacent states are obtained by removing one 'J' and appending one
      --  substituted hand for each other card type.
      --
      --  However we don't need to actually insert one of each other card,
      --  because the best solution will always be to insert cards that already
      --  exist in the hand as this will push e.g. three-of-a-kinds to
      --  four-of-a-kinds. (TODO: this optimization is not yet implemented)
      --
      --  Unlike a normal BFS/DFS we do not need to maintain a list of seen
      --  nodes since it's impossible to revisit a state. Substituting a 'J'
      --  for another card always advances the state towards a leaf node,
      --  because 'J' can never be re-inserted.

      while Search_States.Length > 0 loop
         declare
            Next        : constant Hand := Search_States.Last_Element;
            Adj         : Hand          := Next;
            Joker_Index : Natural       := Index (Next, 'J');
         begin
            Search_States.Delete_Last;

            if Joker_Index /= 0 then
               -- convert one joker to other cards
               for I in Card'Range loop
                  if I /= 'J' then
                     Adj (Joker_Index) := I;
                     Search_States.Append (Adj);
                  end if;
               end loop;
            else
               -- no jokers means this is a leaf state
               Best := Hand_Type'Max (Best, Classify (Next));
            end if;
         end;
      end loop;

      return Best;
   end Classify_Joker;

   type Hand_Bid is record
      Cards                : Hand;
      Bid                  : Integer;
      Classification       : Hand_Type;
      Classification_Joker : Hand_Type;
   end record;

   function Less_Than_Normal (Left, Right : Hand_Bid) return Boolean is
   begin
      if Left.Classification < Right.Classification then
         return True;
      end if;
      if Left.Classification > Right.Classification then
         return False;
      end if;

      return Left.Cards < Right.Cards;
   end Less_Than_Normal;

   function Less_Than_Joker (Left, Right : Hand_Bid) return Boolean is
   begin
      if Left.Classification_Joker < Right.Classification_Joker then
         return True;
      end if;
      if Left.Classification_Joker > Right.Classification_Joker then
         return False;
      end if;

      return Less_Than_Joker (Left.Cards, Right.Cards);
   end Less_Than_Joker;

   package Hand_Bid_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Hand_Bid);

   function Parse_Hand (S : String) return Hand with
     Pre => S'Length = 5
   is
      Result : Hand;
   begin
      for I in S'Range loop
         --  TODO: having to manually surround the character with apostrophes is
         --  kind of gross, does ada have a better way to parse character enumerations?
         Result (Result'First + I - S'First) := Card'Value (''' & S (I .. I) & ''');
      end loop;
      return Result;
   end Parse_Hand;

   package Hand_Bid_Sorting is new Hand_Bid_Vectors.Generic_Sorting ("<" => Less_Than_Normal);

   --  Joker rules change the tiebreak mechanism
   package Hand_Bid_Joker_Sorting is new Hand_Bid_Vectors.Generic_Sorting ("<" => Less_Than_Joker);

   function Winnings (Hands : in out Hand_Bid_Vectors.Vector; Joker_Rules : Boolean) return Integer
   is
      Result : Integer := 0;
   begin
      if Joker_Rules then
         Hand_Bid_Joker_Sorting.Sort (Hands);
      else
         Hand_Bid_Sorting.Sort (Hands);
      end if;

      for Rank in Hands.First_Index .. Hands.Last_Index loop
         Result := Result + Hands (Rank).Bid * Rank;
      end loop;

      return Result;
   end Winnings;

   Lines : constant String_Array := Read_All_Lines ("test/2023-07-input.txt");
   Hands : Hand_Bid_Vectors.Vector;

begin
   for Line of Lines loop
      declare
         H   : constant Hand    := Parse_Hand (Line (1 .. 5));
         Bid : constant Integer := Integer'Value (Line (7 .. Line'Last));
      begin
         Hands.Append
           ((Cards                => H,
             Bid                  => Bid,
             Classification       => Classify (H),
             Classification_Joker => Classify_Joker (H)));
      end;
   end loop;

   Solution (Winnings (Hands, Joker_Rules => False));
   Solution (Winnings (Hands, Joker_Rules => True));
end Day07;
