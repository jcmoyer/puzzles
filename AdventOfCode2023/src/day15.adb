with Advent;                 use Advent;
with Advent.IO;              use Advent.IO;
with Advent.Strings;         use Advent.Strings;
with Ada.Command_Line;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Strings.Maps;       use Ada.Strings.Maps;
with Ada.Assertions;         use Ada.Assertions;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Bounded;

procedure Day15 is
   type Elf_Hash_Type is mod 256;

   function Elf_Hash (S : String) return Elf_Hash_Type is
      H : Elf_Hash_Type := 0;
   begin
      for C of S loop
         H := H + (Character'Pos (C));
         H := H * 17;
      end loop;
      return H;
   end Elf_Hash;

   --  For part 2 we'll just implement a chaining hash table since it's
   --  straightforward and matches the problem pretty well. Could just use a
   --  map of vectors but this problem is already pretty easy so might as well
   --  have some fun.

   --  Input doesn't seem to have longer strings.
   package Label_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 6);

   subtype Label_String is Label_Strings.Bounded_String;

   type Elf_Node is record
      Key   : Label_String;
      Value : Integer;
   end record;

   package Elf_Buckets is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Elf_Node);

   subtype Elf_Bucket is Elf_Buckets.List;

   type Elf_Hash_Table is array (Elf_Hash_Type) of Elf_Bucket;

   procedure Insert (T : in out Elf_Hash_Table; Key : String; Value : Integer) is
      use type Elf_Buckets.Cursor;
      use type Label_String;

      Bucket : constant Elf_Hash_Type := Elf_Hash (Key);
      Cursor : Elf_Buckets.Cursor     := T (Bucket).First;
   begin
      while Cursor /= Elf_Buckets.No_Element loop
         if T (Bucket).Reference (Cursor).Key = Key then
            T (Bucket).Reference (Cursor).Value := Value;
            return;
         end if;
         Elf_Buckets.Next (Cursor);
      end loop;

      --  we reached the end of the list without finding a key; add to end
      T (Bucket).Append (Elf_Node'(Key => Label_Strings.To_Bounded_String (Key), Value => Value));
   end Insert;

   procedure Delete (T : in out Elf_Hash_Table; Key : String) is
      use type Elf_Buckets.Cursor;
      use type Label_String;

      Bucket : constant Elf_Hash_Type := Elf_Hash (Key);
      Cursor : Elf_Buckets.Cursor     := T (Bucket).First;
   begin
      while Cursor /= Elf_Buckets.No_Element loop
         if T (Bucket).Reference (Cursor).Key = Key then
            T (Bucket).Delete (Cursor);
            return;
         end if;
         Elf_Buckets.Next (Cursor);
      end loop;

      --  we reached the end of the list without finding a key; nothing to do
   end Delete;

   function Focusing_Power (List : Elf_Bucket; Box : Integer) return Integer is
      Result : Integer := 0;
      Slot   : Integer := 1;
   begin
      for Element of List loop
         Result := Result + Box * Slot * Element.Value;
         Slot   := Slot + 1;
      end loop;
      return Result;
   end Focusing_Power;

   function Focusing_Power (T : Elf_Hash_Table) return Integer is
      Result : Integer := 0;
   begin
      for I in T'Range loop
         Result := Result + Focusing_Power (T (I), Integer (I) + 1);
      end loop;
      return Result;
   end Focusing_Power;

   Text       : constant String       := Read_All_Text (Ada.Command_Line.Argument (1));
   Components : constant String_Array := Split_Any (Text, "," & LF, Keep_Empty => False);

   Hash_Table : Elf_Hash_Table;

   Part_1 : Integer := 0;

begin

   for Comp of Components loop
      Part_1 := Part_1 + Integer (Elf_Hash (Comp));

      declare
         Op_Index : constant Natural := Index (Comp, To_Set ("-="));
         Label    : constant String  := Comp (Comp'First .. Op_Index - 1);
      begin
         Assert (Op_Index /= 0);

         if Comp (Op_Index) = '=' then
            declare
               Lens_Value : constant Integer := Integer'Value (Comp (Op_Index + 1 .. Comp'Last));
            begin
               Insert (Hash_Table, Label, Lens_Value);
            end;
         else
            Delete (Hash_Table, Label);
         end if;
      end;
   end loop;

   Solution (Part_1);
   Solution (Focusing_Power (Hash_Table));

end Day15;
