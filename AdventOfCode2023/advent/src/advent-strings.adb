with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Advent.Strings is

   function Is_Any_Of (C : Character; Vals : String) return Boolean is
   begin
      for I in Vals'Range loop
         if C = Vals (I) then
            return True;
         end if;
      end loop;
      return False;
   end Is_Any_Of;

   --  When I <= J, returns S(I..J); otherwise returns an empty string.
   function Slice_Empty (S : String; I, J : Integer) return String is
   begin
      if J < I then
         return "";
      else
         return S (I .. J);
      end if;
   end Slice_Empty;

   function Unbounded_Slice_Empty
     (S : Unbounded_String; Low, High : Integer) return Unbounded_String
   is
   begin
      if High < Low then
         return Ada.Strings.Unbounded.Null_Unbounded_String;
      else
         return Unbounded_Slice (S, Low, High);
      end if;
   end Unbounded_Slice_Empty;

   function Split (S : String; Delims : String; Keep_Empty : Boolean := True) return String_Array
   is
      Result : String_Array;

      --  Left and right indices of the current substring
      L : Integer := S'First;
      R : Integer := L;
   begin
      if S'Length = 0 then
         return Result;
      end if;

      while R in S'Range loop
         R := Index (Source => S, Pattern => Delims, From => R);
         exit when R = 0;

         declare
            New_Str : constant String := Slice_Empty (S, L, R - 1);
         begin
            if Keep_Empty or else New_Str'Length > 0 then
               Result.Append (New_Str);
            end if;
         end;

         R := R + Delims'Length;
         L := R;
      end loop;

      R := S'Last;
      declare
         New_Str : constant String := Slice_Empty (S, L, R);
      begin
         if Keep_Empty or else New_Str'Length > 0 then
            Result.Append (New_Str);
         end if;
      end;
      return Result;
   end Split;

   function Split
     (S : Unbounded_String; Delims : String; Keep_Empty : Boolean := True) return String_Array
   is
   begin
      return Split (To_String (S), Delims, Keep_Empty);
   end Split;

   function Split_Any
     (S : String; Delims : String; Keep_Empty : Boolean := True) return String_Array
   is
      Result : String_Array;

      --  Left and right indices of the current substring
      L : Integer := S'First;
      R : Integer := L;
   begin
      if S'Length = 0 then
         return Result;
      end if;

      while R in S'Range loop
         if Is_Any_Of (S (R), Delims) then
            declare
               New_Str : constant String := Slice_Empty (S, L, R - 1);
            begin
               if Keep_Empty or else New_Str'Length > 0 then
                  Result.Append (New_Str);
               end if;
            end;
            L := R + 1;
         end if;
         R := R + 1;
      end loop;

      declare
         New_Str : constant String := Slice_Empty (S, L, S'Last);
      begin
         if Keep_Empty or else New_Str'Length > 0 then
            Result.Append (New_Str);
         end if;
      end;
      return Result;
   end Split_Any;

   function Starts_With (Source, Substr : String) return Boolean is
   begin
      if Source'Length < Substr'Length then
         return False;
      end if;
      return Source (Source'First .. Source'First + Substr'Length - 1) = Substr;
   end Starts_With;

   function Ends_With (Source, Substr : String) return Boolean is
   begin
      if Source'Length < Substr'Length then
         return False;
      end if;
      return Source (Source'Last - Substr'Length + 1 .. Source'Last) = Substr;
   end Ends_With;

   function Delete_Whitespace (Source : String) return String is
      Result : String (1 .. Source'Length);
      Length : Integer := 0;
   begin
      for I in Source'Range loop
         if not Is_Space (Source (I)) then
            Result (Result'First + Length) := Source (I);
            Length                         := Length + 1;
         end if;
      end loop;
      return Result (1 .. Length);
   end Delete_Whitespace;

end Advent.Strings;
