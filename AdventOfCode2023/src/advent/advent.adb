with Ada.Text_IO;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Streams;             use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Characters.Latin_1;

package body Advent is
   package TIO renames Ada.Text_IO;
   package SIO renames Ada.Streams.Stream_IO;
   package Latin_1 renames Ada.Characters.Latin_1;

   procedure Solution (Val : Integer) is
   begin
      TIO.Put_Line (Trim (Val'Image, Ada.Strings.Left));
   end Solution;

   procedure Solution (Val : Long_Long_Integer) is
   begin
      TIO.Put_Line (Trim (Val'Image, Ada.Strings.Left));
   end Solution;

   procedure Solution (Val : String) is
   begin
      TIO.Put_Line (Val);
   end Solution;

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

   function Read_All_Bytes (Filename : String) return Stream_Element_Array_Ptr is
      Input_File : SIO.File_Type;
      Buffer     : Stream_Element_Array_Ptr;
      Last       : Stream_Element_Offset;

      use type SIO.Count;

   begin
      SIO.Open (Input_File, SIO.In_File, Filename);
      Buffer := new Stream_Element_Array (0 .. Stream_Element_Offset (SIO.Size (Input_File) - 1));
      SIO.Read (Input_File, Buffer.all, Last);
      SIO.Close (Input_File);
      return Buffer;
   end Read_All_Bytes;

   function Read_All_Text (Filename : String) return String is
      Buffer : constant Stream_Element_Array_Ptr := Read_All_Bytes (Filename);
      Result : String (1 .. Buffer'Length);
   begin
      for I in Buffer'Range loop
         Result (Result'First + Integer (I - Buffer'First)) := Character'Val (Buffer (I));
      end loop;
      return Result;
   end Read_All_Text;

   function Read_All_Lines (Filename : String) return String_Array is
      Result : String_Array;

      Buffer : constant Stream_Element_Array_Ptr := Read_All_Bytes (Filename);

      Line_Start : Stream_Element_Offset := 0;
      Line_End   : Stream_Element_Offset := 0;
      I          : Stream_Element_Offset := 0;

   begin
      I := Buffer.all'First;

      while I <= Buffer.all'Last loop
         --  scan for newline
         Line_Start := I;
         while I <= Buffer.all'Last
           and then Buffer.all (I) /= Stream_Element (Character'Pos (Latin_1.LF))
         loop
            I := I + 1;
         end loop;
         Line_End := I - 1;

         --  handle \r\n
         if Buffer.all (Line_End) = Stream_Element (Character'Pos (Latin_1.CR)) then
            Line_End := Line_End - 1;
         end if;

         --  skip newline
         I := I + 1;

         --  copy processed line
         declare
            Line : String (1 .. Integer (Line_End - Line_Start + 1));
         begin
            for J in Line_Start .. Line_End loop
               Line (Line'First + Integer (J - Line_Start)) := Character'Val (Buffer.all (J));
            end loop;
            Result.Append (Line);
         end;
      end loop;

      return Result;
   end Read_All_Lines;

   function Read_Tilemap (Filename : String) return Char_Matrix is
      use String_Vectors;
      Lines     : constant String_Array := Read_All_Lines (Filename);
      Row_Count : constant Integer      := Integer (Lines.Length);
      Col_Count : constant Integer      := First_Element (Lines)'Length;
   begin
      return Result : Char_Matrix (1 .. Row_Count, 1 .. Col_Count) do
         for I in 1 .. Row_Count loop
            for J in 1 .. Col_Count loop
               Result (I, J) := Lines (I - 1) (J);
            end loop;
         end loop;
      end return;
   end Read_Tilemap;

   function Rows (M : Char_Matrix) return Integer is
   begin
      return M'Length (1);
   end Rows;

   function Cols (M : Char_Matrix) return Integer is
   begin
      return M'Length (2);
   end Cols;

   function Image (M : Char_Matrix) return String is
      --  Add an additional column for each row to store a newline
      Result_Length : constant Integer            := Rows (M) * Cols (M) + Rows (M);
      Result        : String (1 .. Result_Length) := (others => Ada.Characters.Latin_1.LF);

      Line_Length : constant Integer := Cols (M) + 1;
   begin
      for I in M'First (1) .. M'Last (1) loop
         for J in M'First (2) .. M'Last (2) loop
            Result (1 + (I - M'First (1)) * Line_Length + (J - M'First (2))) := M (I, J);
         end loop;
      end loop;
      return Result;
   end Image;

   function Transpose (M : Char_Matrix) return Char_Matrix is
      Result : Char_Matrix (M'First (2) .. M'Last (2), M'First (1) .. M'Last (1));
   begin
      for I in M'First (1) .. M'Last (1) loop
         for J in M'First (2) .. M'Last (2) loop
            Result (J, I) := M (I, J);
         end loop;
      end loop;
      return Result;
   end Transpose;

   function Contains (R : Rectangle; P : Point) return Boolean is
   begin
      return P.X >= R.Left and then P.X <= R.Right and then P.Y >= R.Bottom and then P.Y <= R.Top;
   end Contains;

   procedure Inflate (R : in out Rectangle; Delta_X, Delta_Y : Integer) is
   begin
      R.Left   := R.Left - Delta_X;
      R.Right  := R.Right + Delta_X;
      R.Top    := R.Top + Delta_Y;
      R.Bottom := R.Bottom - Delta_Y;
   end Inflate;

end Advent;
