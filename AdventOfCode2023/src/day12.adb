with Advent;                use Advent;
with Advent.Parsers.Integers;
with Ada.Command_Line;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with Ada.Strings;
with Ada.Containers.Hashed_Maps;
with Ada.Containers;        use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Day12 is
   package Integer_Parsers is new Advent.Parsers.Integers (Element_Type => Integer);

   subtype Integer_Vector is Integer_Parsers.Vector;

   --  Part 2: repeat pattern 5 times interspersed with '?''
   function Unfold_Pattern (S : String) return String is
   begin
      return S & '?' & S & '?' & S & '?' & S & '?' & S;
   end Unfold_Pattern;

   --  Part 2: repeat spans 5 times
   function Unfold_Spans (S : Integer_Vector) return Integer_Vector is
      Result : Integer_Vector;
   begin
      for I in 1 .. 5 loop
         Result.Append_Vector (S);
      end loop;
      return Result;
   end Unfold_Spans;

   --  Returns a copy of Xs with the first element removed.
   function Tail (Xs : Integer_Vector) return Integer_Vector is
      Result : Integer_Vector := Xs.Copy;
   begin
      Result.Delete_First;
      return Result;
   end Tail;

   --  Memoized state
   type Spring_Record is record
      Springs : Unbounded_String;
      Spans   : Integer_Vector;
   end record;

   function Hash (R : Spring_Record) return Hash_Type is
      H : Hash_Type := 0;
   begin
      H := Ada.Strings.Unbounded.Hash (R.Springs);
      for Span of R.Spans loop
         H := H xor (54_018_521 * Hash_Type (Span));
      end loop;
      return H;
   end Hash;

   package Spring_Record_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Spring_Record,
      Element_Type    => Long_Long_Integer,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   function Dp (R : Spring_Record; Seen : in out Spring_Record_Maps.Map) return Long_Long_Integer
   is
      use type Spring_Record_Maps.Cursor;

      Result : Long_Long_Integer := 0;

      Existing : constant Spring_Record_Maps.Cursor := Seen.Find (R);
   begin
      if Existing /= Spring_Record_Maps.No_Element then
         return Spring_Record_Maps.Element (Existing);
      end if;

      --  We ran out of damaged springs...
      if R.Spans.Length = 0 then
         if Index (R.Springs, To_Set ('#')) /= 0 then
            ---  ...but the template string still has more, so this solution is invalid
            Seen.Insert (R, 0);
            return 0;
         else
            --  ...and we've used up all the damaged springs in the template
            --  string, so this is a solution
            Seen.Insert (R, 1);
            return 1;
         end if;
      end if;

      --  We have unused springs, but we ran out of space in the string. So
      --  this solution is invalid.
      if Length (R.Springs) < R.Spans.First_Element then
         Seen.Insert (R, 0);
         return 0;
      end if;

      declare
         Window : constant Unbounded_String :=
           Unbounded_Slice_Empty (R.Springs, Low => 1, High => R.Spans.First_Element);
      begin
         --  No '.' means there are only damaged or unknown springs in this
         --  window.
         if Index (Window, To_Set ('.'), 1) = 0 then
            --  Can we fit the current span in this window?
            if Length (R.Springs) = R.Spans.First_Element
              or else Element (R.Springs, 1 + R.Spans.First_Element) /= '#'
            then
               --  Yes -> chop off the rest of the string following this window
               --  (+1 for space, +1 for 1-based indices thanks ada)
               declare
                  Rest : constant Unbounded_String :=
                    Unbounded_Slice_Empty
                      (R.Springs,
                       Low  => R.Spans.First_Element + 1 + 1,
                       High => Length (R.Springs));
               begin
                  Result :=
                    Result + Dp (Spring_Record'(Springs => Rest, Spans => Tail (R.Spans)), Seen);
               end;
            end if;
         end if;
      end;

      --  First character is either '.' or '?'. There's nothing to match here
      --  (if we *could* fit a span, we would have done it above)
      if Element (R.Springs, 1) /= '#' then
         --  Chop off the first character and process the rest of the string
         Result :=
           Result +
           Dp (Spring_Record'
                (Springs => Unbounded_Slice (R.Springs, Low => 2, High => Length (R.Springs)),
                 Spans   => R.Spans.Copy),
              Seen);
      end if;

      Seen.Insert (R, Result);
      return Result;
   end Dp;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   P1    : Long_Long_Integer     := 0;
   P2    : Long_Long_Integer     := 0;

begin

   for Line of Lines loop
      declare
         Left_Right : constant String_Array   := Split (Line, " ");
         Pattern    : constant String         := Left_Right (0);
         Spans      : constant Integer_Vector := Integer_Parsers.Extract_Integers (Left_Right (1));

         Unfolded_Pattern : constant String         := Unfold_Pattern (Pattern);
         Unfolded_Spans   : constant Integer_Vector := Unfold_Spans (Spans);

         Seen : Spring_Record_Maps.Map;
      begin

         P1 :=
           P1 +
           Dp (Spring_Record'(Springs => To_Unbounded_String (Pattern), Spans => Spans), Seen);

         P2 :=
           P2 +
           Dp (Spring_Record'
                (Springs => To_Unbounded_String (Unfolded_Pattern), Spans => Unfolded_Spans),
              Seen);

      end;
   end loop;

   Solution (P1);
   Solution (P2);

end Day12;
