with Advent;         use Advent;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Command_Line;

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

   --  Zero-based indexing significantly reduces overall parse/classify time by
   --  ~25%.
   type Hand is array (0 .. 4) of Card;

   type Card_Count is range 0 .. 5;

   type Card_Counts is record
      Ones   : Card_Count := 0;
      Twos   : Card_Count := 0;
      Threes : Card_Count := 0;
      Fours  : Card_Count := 0;
      Fives  : Card_Count := 0;
      Mostly : Card       := 'J';
   end record;

   function Count (H : Hand) return Card_Counts is
      Counts    : array (Card) of Integer := (others => 0);
      Result    : Card_Counts;
      Most_Type : Integer                 := 0;
   begin
      for C of H loop
         Counts (C) := Counts (C) + 1;
         if C /= 'J' and then Counts (C) > Most_Type then
            Most_Type     := Counts (C);
            Result.Mostly := C;
         end if;
      end loop;

      for I in Counts'Range loop
         case Counts (I) is
            when 1 =>
               Result.Ones := Result.Ones + 1;
            when 2 =>
               Result.Twos := Result.Twos + 1;
            when 3 =>
               Result.Threes := Result.Threes + 1;
            when 4 =>
               Result.Fours := Result.Fours + 1;
            when 5 =>
               Result.Fives := Result.Fives + 1;
            when others =>
               null;
         end case;
      end loop;

      return Result;
   end Count;

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
   function Index (H : Hand; Elem : Card; Start : Natural := Hand'First) return Natural is
   begin
      for I in Start .. H'Last loop
         if H (I) = Elem then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   function Image (H : Hand) return String is
     (H (0)'Image & H (1)'Image & H (2)'Image & H (3)'Image & H (4)'Image);

   type Hand_Type is (High_Card, One_Pair, Two_Pair, Three_Kind, Full_House, Four_Kind, Five_Kind);

   function Classify (Counts : Card_Counts) return Hand_Type is
   begin
      if Counts.Fives = 1 then
         return Five_Kind;
      elsif Counts.Fours = 1 then
         return Four_Kind;
      elsif Counts.Threes = 1 and then Counts.Twos = 1 then
         return Full_House;
      elsif Counts.Threes = 1 and then Counts.Ones = 2 then
         return Three_Kind;
      elsif Counts.Twos = 2 then
         return Two_Pair;
      elsif Counts.Twos = 1 and then Counts.Ones = 3 then
         return One_Pair;
      elsif Counts.Ones = 5 then
         return High_Card;
      end if;
      return One_Pair;
   end Classify;

   function Classify_Joker (H : Hand; Counts : Card_Counts) return Hand_Type is
      Substituted : Hand := H;
   begin
      for I in Substituted'Range loop
         if Substituted (I) = 'J' then
            Substituted (I) := Counts.Mostly;
         end if;
      end loop;
      return Classify (Count (Substituted));
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

   --  This may look silly but it reduces the parse time by 70ms (originally
   --  out of 600ms total program runtime) on a 2.5M input compared to using
   --  Card'Value.
   function Parse_Card (C : Character) return Card is
   begin
      case C is
         --!pp off
         when '2' => return '2';
         when '3' => return '3';
         when '4' => return '4';
         when '5' => return '5';
         when '6' => return '6';
         when '7' => return '7';
         when '8' => return '8';
         when '9' => return '9';
         when 'T' => return 'T';
         when 'J' => return 'J';
         when 'Q' => return 'Q';
         when 'K' => return 'K';
         when 'A' => return 'A';
         when others =>
            raise Program_Error;
         --!pp on
      end case;
   end Parse_Card;

   function Parse_Hand (S : String) return Hand with
     Pre => S'Length = 5
   is
      Result : Hand;
   begin
      for I in S'Range loop
         Result (Result'First + I - S'First) := Parse_Card (S (I));
      end loop;
      return Result;
   end Parse_Hand;

   package Hand_Bid_Sorting is new Hand_Bid_Vectors.Generic_Sorting ("<" => Less_Than_Normal);

   --  Joker rules change the tiebreak mechanism
   package Hand_Bid_Joker_Sorting is new Hand_Bid_Vectors.Generic_Sorting ("<" => Less_Than_Joker);

   function Winnings
     (Hands : in out Hand_Bid_Vectors.Vector; Joker_Rules : Boolean) return Long_Long_Integer
   is
      Result : Long_Long_Integer := 0;
   begin
      if Joker_Rules then
         Hand_Bid_Joker_Sorting.Sort (Hands);
      else
         Hand_Bid_Sorting.Sort (Hands);
      end if;

      for Rank in Hands.First_Index .. Hands.Last_Index loop
         Result := Result + Long_Long_Integer (Hands (Rank).Bid) * Long_Long_Integer (Rank);
      end loop;

      return Result;
   end Winnings;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   Hands : Hand_Bid_Vectors.Vector;

begin
   for Line of Lines loop
      declare
         H      : constant Hand        := Parse_Hand (Line (1 .. 5));
         Bid    : constant Integer     := Integer'Value (Line (7 .. Line'Last));
         Counts : constant Card_Counts := Count (H);
      begin
         Hands.Append
           ((Cards                => H,
             Bid                  => Bid,
             Classification       => Classify (Counts),
             Classification_Joker => Classify_Joker (H, Counts)));
      end;
   end loop;

   Solution (Winnings (Hands, Joker_Rules => False));
   Solution (Winnings (Hands, Joker_Rules => True));
end Day07;
