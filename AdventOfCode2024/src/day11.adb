with Advent.IO;   use Advent.IO;
with Advent.Long_Parsers;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

procedure Day11 is

   package AIP renames Advent.Long_Parsers;

   --  Map of number to quantity. Theory: there are few distinct numbers in
   --  this problem; all devolve into even numbers and are multiples of 2024 or
   --  splits of it. Modeling this with an array is wasteful, so maybe if we
   --  compress the state it will be fast enough to simulate.
   package Number_Maps is new Ada.Containers.Ordered_Maps (Long_Long_Integer, Long_Long_Integer);

   use type Number_Maps.Cursor;

   function N_Digits (X : Long_Long_Integer) return Natural is
   begin
      if X >= 100 then
         if X >= 1_000 then
            return 3 + N_Digits (X / 1_000);
         end if;
         return 3;
      elsif X >= 10 then
         return 2;
      end if;
      return 1;
   end N_Digits;

   procedure Split (X : Long_Long_Integer; Left, Right : out Long_Long_Integer) is
      ND : constant Natural := N_Digits (X);
   begin
      Left  := X / (10**(ND / 2));
      Right := X - (Left * (10**(ND / 2)));
   end Split;

   procedure Add_Number (M : in out Number_Maps.Map; Number, Quantity : Long_Long_Integer) is
      Cursor : constant Number_Maps.Cursor := M.Find (Number);
   begin
      if Cursor = Number_Maps.No_Element then
         M.Insert (Number, Quantity);
      else
         Number_Maps.Replace_Element (M, Cursor, Number_Maps.Element (Cursor) + Quantity);
      end if;
   end Add_Number;

   procedure Step (M : in out Number_Maps.Map) is
      Working : constant Number_Maps.Map := M.Copy;
      Cursor  : Number_Maps.Cursor       := Working.First;
   begin
      M.Clear;

      while Cursor /= Number_Maps.No_Element loop
         declare
            N : constant Long_Long_Integer := Number_Maps.Key (Cursor);
            Q : constant Long_Long_Integer := Number_Maps.Element (Cursor);
         begin
            if N = 0 then
               Add_Number (M, 1, Q);
            elsif N_Digits (N) rem 2 = 0 then
               declare
                  Left  : Long_Long_Integer;
                  Right : Long_Long_Integer;
               begin
                  Split (N, Left, Right);
                  Add_Number (M, Left, Q);
                  Add_Number (M, Right, Q);
               end;
            else
               Add_Number (M, N * 2_024, Q);
            end if;
         end;
         Cursor := Number_Maps.Next (Cursor);
      end loop;
   end Step;

   procedure Print (M : Number_Maps.Map) is
      Cursor : Number_Maps.Cursor := M.First;
   begin
      while Cursor /= Number_Maps.No_Element loop
         Put_Line (Number_Maps.Key (Cursor)'Image & " " & Number_Maps.Element (Cursor)'Image);
         Cursor := Number_Maps.Next (Cursor);
      end loop;
   end Print;

   function Count (M : Number_Maps.Map) return Long_Long_Integer is
      Cursor : Number_Maps.Cursor := M.First;
      Result : Long_Long_Integer  := 0;
   begin
      while Cursor /= Number_Maps.No_Element loop
         Result := Result + Number_Maps.Element (Cursor);
         Cursor := Number_Maps.Next (Cursor);
      end loop;
      return Result;
   end Count;

   Text  : constant String     := Advent.IO.Read_All_Text (Ada.Command_Line.Argument (1));
   Ints  : constant AIP.Vector := AIP.Extract_Integers (Text);
   World : Number_Maps.Map;

begin

   for I of Ints loop
      Add_Number (World, I, 1);
   end loop;

   for I in 1 .. 25 loop
      Step (World);
   end loop;

   Solution (Count (World));

   for I in 1 .. 50 loop
      Step (World);
   end loop;

   Solution (Count (World));

end Day11;
