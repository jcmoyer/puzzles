with Ada.Containers;

package body Advent.Intervals is
   ----------------------------------------------------------------------------
   --  Interval
   ----------------------------------------------------------------------------

   function "=" (A, B : Interval) return Boolean is
     (A.Min = B.Min and then A.Max = B.Max);

   function Image (R : Interval) return String is
     (R.Min'Image & ".." & R.Max'Image);

   function Singleton (Value : Element_Type) return Interval is
     (Min => Value, Max => Value);

   function First (R : Interval) return Element_Type is (R.Min);
   function Last (R : Interval) return Element_Type is (R.Max);
   function Length (R : Interval) return Element_Type is (R.Max - R.Min + 1);

   function Contains (R : Interval; Val : Element_Type) return Boolean is
     (Val in First (R) .. Last (R));

   function Contains (R, Enclosed : Interval) return Boolean is
     (Contains (R, Enclosed.Min) and then Contains (R, Enclosed.Max));

   function Overlaps (A, B : Interval) return Boolean is
     (Contains (A, First (B)) or else Contains (A, Last (B))
      or else Contains (B, First (A)) or else Contains (B, Last (A)));

   function Merge (A, B : Interval) return Interval is
     (Min => Element_Type'Min (A.Min, B.Min),
      Max => Element_Type'Max (A.Max, B.Max));

   function Intersect (A, B : Interval) return Interval is
   begin
      if Overlaps (A, B) then
         return
           (Min => Element_Type'Max (A.Min, B.Min),
            Max => Element_Type'Min (A.Max, B.Max));
      else
         return Empty_Interval;
      end if;
   end Intersect;

   ----------------------------------------------------------------------------
   --  Multi_Interval
   ----------------------------------------------------------------------------

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
      use type Ada.Containers.Count_Type;

      Last_Size : Ada.Containers.Count_Type := 0;
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
            --  have to split it in two. One of the new intervals may be zero
            --  sized. Since we perform reduction on insert, there's no way any
            --  other interval could be affected, so we can safely return after
            --  performing the split.
            declare
               Left  : constant Interval :=
                 (Min => M.Children (I).Min, Max => First (R) - 1);
               Right : constant Interval :=
                 (Min => Last (R) + 1, Max => M.Children (I).Max);
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

   procedure Translate (M : in out Multi_Interval; Amount : Element_Type) is
   begin
      for C of M.Children loop
         C.Min := C.Min + Amount;
         C.Max := C.Max + Amount;
      end loop;
   end Translate;

   procedure Translate
     (M : in out Multi_Interval; Span : Interval; Amount : Element_Type)
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

   function First (M : Multi_Interval) return Element_Type is
      Result : Element_Type := Element_Type'Last;
   begin
      for Child of M.Children loop
         Result := Element_Type'Min (Result, Child.Min);
      end loop;
      return Result;
   end First;

end Advent.Intervals;
