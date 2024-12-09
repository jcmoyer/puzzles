with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Advent.Containers.Priority_Queues;
with Ada.Command_Line;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

procedure Day09 is

   --  Represents number of blocks represented by one character of the puzzle
   --  input; free blocks may be 0, but files should always have a non-zero
   --  count.
   type Block_Count is range 0 .. 9;

   --  Size of an actual file in blocks.
   type File_Block_Count is range 1 .. 9;

   type Block_Type is (Undefined, Free, File);

   type Block (Kind : Block_Type := Undefined) is record
      case Kind is
         when File =>
            File_Id   : Natural;
            --  TODO: this should be stored externally, once per file_id
            File_Size : File_Block_Count;
         when Free =>
            null;
         when Undefined =>
            null;
      end case;
   end record;

   subtype Block_Index is Positive;

   type Block_Array is array (Block_Index range <>) of Block;

   --  As an optimization for part 2, we keep min-heaps keyed by the width of
   --  the free space so that we can find the next place to move a file in
   --  ~O(log(N)).
   package Free_Queues is new Advent.Containers.Priority_Queues
     (Element_Type => Block_Index, "<" => "<");

   type Free_Queue_Array is array (File_Block_Count) of Free_Queues.Queue;

   procedure Print_Blocks (A : Block_Array) is
      package ASF renames Ada.Strings.Fixed;
   begin
      for B of A loop
         case B.Kind is
            when File =>
               Put (ASF.Trim (B.File_Id'Image, Ada.Strings.Left));
            when Free =>
               Put ('.');
            when others =>
               raise Program_Error with "invalid state";
         end case;
      end loop;
      New_Line;
   end Print_Blocks;

   function Digit_To_Block_Count (Digit : Character) return Block_Count is
     (Block_Count (Character'Pos (Digit) - Character'Pos ('0')));

   function Measure_Disk_Size (S : String) return Natural is
      Size : Natural := 0;
   begin
      for C of S loop
         Size := Size + Natural (Digit_To_Block_Count (C));
      end loop;
      return Size;
   end Measure_Disk_Size;

   procedure Load_Disk (S : String; Blocks : out Block_Array) is
      Dest : Block_Index := Blocks'First;
   begin
      for I in S'Range loop
         declare
            Length  : constant Block_Count := Digit_To_Block_Count (S (I));
            Kind    : constant Block_Type  := (if I rem 2 = 1 then File else Free);
            File_Id : constant Natural     := I / 2;
         begin
            for J in 1 .. Length loop
               Blocks (Dest) :=
                 (if
                    Kind = File
                  then
                    Block'
                      (Kind => File, File_Id => File_Id, File_Size => File_Block_Count (Length))
                  else Block'(Kind => Free));

               Dest := Dest + 1;
            end loop;
         end;
      end loop;
   end Load_Disk;

   function Find_Next_Free_Space (Blocks : Block_Array; From : in out Block_Index) return Boolean
   is
   begin
      for I in From .. Blocks'Last loop
         if Blocks (I).Kind = Free then
            From := I;
            return True;
         end if;
      end loop;
      return False;
   end Find_Next_Free_Space;

   function Find_Next_File (Blocks : Block_Array; From : in out Block_Index) return Boolean is
   begin
      for I in reverse Blocks'First .. From loop
         if Blocks (I).Kind = File then
            From := I;
            return True;
         end if;
      end loop;
      return False;
   end Find_Next_File;

   procedure Swap_Blocks (Blocks : in out Block_Array; First, Second : Block_Index) is
      A : constant Block := Blocks (First);
   begin
      Blocks (First)  := Blocks (Second);
      Blocks (Second) := A;
   end Swap_Blocks;

   --  Source span is overwritten with free space.
   procedure Move_Blocks (Blocks : in out Block_Array; From, To : Block_Index; Count : Positive) is
   begin
      Blocks (To .. To + Count - 1)     := Blocks (From .. From + Count - 1);
      Blocks (From .. From + Count - 1) := (others => (Kind => Free));
   end Move_Blocks;

   procedure Compress_Blocks (Blocks : in out Block_Array) is
      Current_File : Block_Index := Blocks'Last;
      Current_Free : Block_Index := Blocks'First;
   begin
      loop
         exit when not Find_Next_File (Blocks, Current_File);
         exit when not Find_Next_Free_Space (Blocks, Current_Free);
         exit when not (Current_Free < Current_File);

         Swap_Blocks (Blocks, Current_File, Current_Free);

         Current_File := Current_File - 1;
         Current_Free := Current_Free + 1;
      end loop;
   end Compress_Blocks;

   --  This function moves `From` to the first block in the file.
   function Find_Next_File_Start (Blocks : Block_Array; From : in out Block_Index) return Boolean
   is
   begin
      for I in reverse Blocks'First .. From loop
         if Blocks (I).Kind = File then
            From := I - (Natural (Blocks (I).File_Size) - 1);
            return True;
         end if;
      end loop;
      return False;
   end Find_Next_File_Start;

   --  Returns True if the block at `Move_From` with size `Size` can be moved
   --  left. `Queue_Id` will contain the index of the queue with the next
   --  lowest offset.
   function Leftmost_Queue_For_Size
     (Queues    :     Free_Queue_Array;
      Size      :     File_Block_Count;
      Move_From :     Block_Index;
      Queue_Id  : out File_Block_Count)
      return Boolean
   is
      Queue_Exists : Boolean := False;
   begin
      for N in Size .. File_Block_Count (9) loop
         if not Queue_Exists then
            if Queues (N).Length > 0 and then Queues (N).Peek < Move_From then
               Queue_Id     := N;
               Queue_Exists := True;
            end if;
         else
            if Queues (N).Length > 0 and then Queues (N).Peek < Queues (Queue_Id).Peek then
               Queue_Id := N;
            end if;
         end if;
      end loop;
      return Queue_Exists;
   end Leftmost_Queue_For_Size;

   procedure Build_Queues (Blocks : in out Block_Array; Queues : out Free_Queue_Array) is
      Free_First : Integer := Blocks'First;
   begin
      while Free_First <= Blocks'Last loop
         if Blocks (Free_First).Kind = Free then
            declare
               Free_Last : Integer := Free_First;
            begin
               while Blocks (Free_Last).Kind = Free loop
                  exit when Free_Last > Blocks'Last;
                  exit when Free_Last - Free_First = 9;
                  Free_Last := Free_Last + 1;
               end loop;
               Queues (File_Block_Count (Free_Last - Free_First)).Enqueue (Free_First);
               Free_First := Free_Last;
            end;
         else
            Free_First := Free_First + 1;
         end if;
      end loop;
   end Build_Queues;

   procedure Compress_Whole_Files (Blocks : in out Block_Array) is
      Position   : Block_Index := Blocks'Last;
      Queues     : Free_Queue_Array;
      Best_Queue : File_Block_Count;
   begin
      Build_Queues (Blocks, Queues);

      while Find_Next_File_Start (Blocks, Position) loop
         --  Print_Blocks (Blocks);
         --  for L in Free_Lists'Range loop
         --     Put_Line (L'Image & " " & Free_Lists (L).Length'Image);
         --  end loop;

         if Leftmost_Queue_For_Size (Queues, Blocks (Position).File_Size, Position, Best_Queue)
         then
            declare
               Current_Size : constant File_Block_Count := Blocks (Position).File_Size;
               Wasted_Space : constant Block_Count      :=
                 Block_Count (Best_Queue) - Block_Count (Current_Size);

               Free_First : Natural;
            begin
               Queues (Best_Queue).Dequeue (Free_First);

               Move_Blocks
                 (Blocks, From => Position, To => Free_First, Count => Positive (Current_Size));

               --  If we waste any space, save it into an appropriate queue.
               if Wasted_Space > 0 then
                  Queues (File_Block_Count (Wasted_Space)).Enqueue
                    (Free_First + Natural (Current_Size));
               end if;
            end;
         end if;

         exit when Position = 1;
         Position := Position - 1;
      end loop;

      --  Print_Blocks (Blocks);
      --  for L in Free_Lists'Range loop
      --     Put_Line (L'Image & " " & Free_Lists (L).Length'Image);
      --  end loop;
   end Compress_Whole_Files;

   function Checksum (Blocks : Block_Array) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
   begin
      for I in Blocks'Range loop
         if Blocks (I).Kind = File then
            Result := Result + Long_Long_Integer ((I - Blocks'First) * Blocks (I).File_Id);
         end if;
      end loop;
      return Result;
   end Checksum;

   Lines : constant String_Array := Advent.IO.Read_All_Lines (Ada.Command_Line.Argument (1));

begin
   declare
      A : Block_Array (1 .. Measure_Disk_Size (Lines.First_Element));
   begin
      Load_Disk (Lines.First_Element, A);
      Compress_Blocks (A);
      Solution (Checksum (A));

      Load_Disk (Lines.First_Element, A);
      Compress_Whole_Files (A);
      Solution (Checksum (A));
   end;
end Day09;
