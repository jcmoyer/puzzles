with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Strings;         use Ada.Strings;
with Advent.IO;
with Advent.Long_Parsers; use Advent.Long_Parsers;
with Ada.Command_Line;
with Advent.Containers.Long_Vectors;

procedure Day02 is
   function Is_Invalid (S : String; First_Chunk_Size : Positive) return Boolean is
      Chunk_Size  : Positive := First_Chunk_Size;
      Chunk_Count : Integer;

      function Is_Valid_Chunk_Size return Boolean
      is (S'Length rem Chunk_Size = 0);

      --!format off
      function Contains_Duplicate_Chunks return Boolean
      --  true if the first chunk in S is equal to every subsequent chunk
      is (for all I in 1 .. Chunk_Count - 1 =>
            S (S'First .. S'First + Chunk_Size - 1)
            =
            S (S'First + I * Chunk_Size .. S'First + I * Chunk_Size + Chunk_Size - 1));
      --!format on
   begin
      while Chunk_Size <= S'Length / 2 loop
         Chunk_Count := S'Length / Chunk_Size;

         if Is_Valid_Chunk_Size and then Contains_Duplicate_Chunks then
            return True;
         end if;

         Chunk_Size := Chunk_Size + 1;
      end loop;

      return False;
   end Is_Invalid;

   ----------------------------------------------------------------------------

   Text : constant String := Advent.IO.Read_All_Text (Ada.Command_Line.Argument (1));

   Ints : constant Advent.Containers.Long_Vectors.Vector := Extract_Positive_Integers (Text);

   I : Integer := Ints.First_Index;

   P1, P2 : Long_Long_Integer := 0;

   First, Last : Long_Long_Integer;
begin
   while I in Ints.First_Index .. Ints.Last_Index loop
      First := Ints (I);
      Last := Ints (I + 1);

      for Int in First .. Last loop
         declare
            S : constant String := Trim (Int'Image, Left);
         begin
            if S'Length rem 2 = 0 and then Is_Invalid (S, First_Chunk_Size => S'Length / 2) then
               P1 := P1 + Int;
            end if;

            if Is_Invalid (S, First_Chunk_Size => 1) then
               P2 := P2 + Int;
            end if;
         end;
      end loop;

      I := I + 2;
   end loop;

   Advent.IO.Solution (P1);
   Advent.IO.Solution (P2);
end Day02;
