with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

procedure Day19 is

   package String_Int_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Long_Long_Integer,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Cache    : String_Int_Maps.Map;
   Patterns : String_Array;

   function Ways_To_Make (D : String) return Long_Long_Integer is
      N : Long_Long_Integer := 0;
   begin
      if Cache.Contains (D) then
         return Cache.Element (D);
      end if;

      for P of Patterns loop
         if D = P then
            N := N + 1;
         elsif P'Length <= D'Length and then D (D'First .. D'First + P'Length - 1) = P then
            N := N + Ways_To_Make (D (D'First + P'Length .. D'Last));
         end if;
      end loop;

      Cache.Include (D, N);
      return N;
   end Ways_To_Make;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Line_Nr : Integer           := 1;
   Part_1  : Long_Long_Integer := 0;
   Part_2  : Long_Long_Integer := 0;

   Ways : Long_Long_Integer;

begin
   for Line of Lines loop
      if Line_Nr = 1 then
         Patterns := Split (Line, ", ");
      elsif Line_Nr >= 3 then
         Ways   := Ways_To_Make (Line);
         Part_1 := Part_1 + (if Ways /= 0 then 1 else 0);
         Part_2 := Part_2 + Ways;
      end if;
      Line_Nr := Line_Nr + 1;
   end loop;

   Solution (Part_1);
   Solution (Part_2);
end Day19;
