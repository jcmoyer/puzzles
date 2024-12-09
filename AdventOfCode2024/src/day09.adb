with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

procedure Day09 is

   type File_Length is range 0 .. 9;

   type Block_Type is (Undefined, Free, File);

   type Block (Kind : Block_Type := Undefined) is record
      case Kind is
         when File =>
            File_Id   : Natural;
            --  TODO: this should be stored externally, once per file_id
            File_Size : File_Length;
         when Free =>
            null;
         when Undefined =>
            null;
      end case;
   end record;

   type Block_Array is array (Positive range <>) of Block;

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

   function Digit_To_File_Length (Digit : Character) return File_Length is
     (File_Length (Character'Pos (Digit) - Character'Pos ('0')));

   function Measure_Disk_Size (S : String) return Natural is
      Size : Natural := 0;
   begin
      for C of S loop
         Size := Size + Natural (Digit_To_File_Length (C));
      end loop;
      return Size;
   end Measure_Disk_Size;

   procedure Load_Disk (S : String; Blocks : out Block_Array) is
      Block_Offset : Positive := 1;
   begin
      for I in S'Range loop
         declare
            Length  : constant File_Length := Digit_To_File_Length (S (I));
            Kind    : constant Block_Type  := (if I rem 2 = 1 then File else Free);
            File_Id : constant Natural     := I / 2;
         begin
            for J in 1 .. Length loop
               Blocks (Block_Offset) :=
                 (if Kind = File then Block'(Kind => File, File_Id => File_Id, File_Size => Length)
                  else Block'(Kind => Free));

               Block_Offset := Block_Offset + 1;
            end loop;
         end;
      end loop;
   end Load_Disk;

   function Find_Next_Free_Space (Blocks : Block_Array; From : in out Integer) return Boolean is
   begin
      for I in From + 1 .. Blocks'Last loop
         if Blocks (I).Kind = Free then
            From := I;
            return True;
         end if;
      end loop;
      return False;
   end Find_Next_Free_Space;

   function Find_Next_File (Blocks : Block_Array; From : in out Integer) return Boolean is
   begin
      for I in reverse Blocks'First .. From - 1 loop
         if Blocks (I).Kind = File then
            From := I;
            return True;
         end if;
      end loop;
      return False;
   end Find_Next_File;

   procedure Swap_Blocks (Blocks : in out Block_Array; First, Second : Positive) is
      A : constant Block := Blocks (First);
   begin
      Blocks (First)  := Blocks (Second);
      Blocks (Second) := A;
   end Swap_Blocks;

   procedure Compress_Blocks (Blocks : in out Block_Array) is
      Current_File : Positive := Blocks'Last + 1;
      Current_Free : Natural  := Blocks'First - 1;
   begin
      loop
         exit when not Find_Next_File (Blocks, Current_File);
         exit when not Find_Next_Free_Space (Blocks, Current_Free);
         exit when not (Current_Free < Current_File);

         Swap_Blocks (Blocks, Current_File, Current_Free);
      end loop;
   end Compress_Blocks;

   --  This function does an O(N) scan of the space to ensure it's at least
   --  `Size` long.
   function Find_Next_Free_Space_Sized
     (Blocks : Block_Array; Size : File_Length; From : in out Integer; To : Integer) return Boolean
   is
   begin
      for I in From + 1 .. Blocks'Last loop
         if Blocks (I).Kind = Free and then (I + Integer (Size) - 1) <= To then
            if (for all J in I .. I + Integer (Size) - 1 => Blocks (J).Kind = Free) then
               From := I;
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Find_Next_Free_Space_Sized;

   --  This function moves `From` to the first block in the file.
   function Find_Next_File_Start (Blocks : Block_Array; From : in out Integer) return Boolean is
   begin
      for I in reverse Blocks'First .. From - 1 loop
         if Blocks (I).Kind = File then
            From := I - (Natural (Blocks (I).File_Size) - 1);
            return True;
         end if;
      end loop;
      return False;
   end Find_Next_File_Start;

   procedure Compress_Whole_Files (Blocks : in out Block_Array) is
      Current_File : Positive := Blocks'Last + 1;
      Current_Free : Natural  := Blocks'First - 1;
   begin
      loop
         --  Print_Blocks (Blocks);
         exit when not Find_Next_File_Start (Blocks, Current_File);

         --  TODO: do we always have to rescan the entire thing?
         Current_Free := Blocks'First - 1;
         if Find_Next_Free_Space_Sized
             (Blocks, Blocks (Current_File).File_Size, Current_Free, Current_File)
         then
            for I in 0 .. Integer (Blocks (Current_File).File_Size) - 1 loop
               Swap_Blocks (Blocks, Current_File + I, Current_Free + I);
            end loop;
         end if;
      end loop;
      --  Print_Blocks (Blocks);
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
      --  Print_Blocks (A);
      Compress_Blocks (A);
      --  Print_Blocks (A);
      Solution (Checksum (A));

      Load_Disk (Lines.First_Element, A);
      Compress_Whole_Files (A);
      Solution (Checksum (A));
   end;
end Day09;
