with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings;

package body Advent is
   procedure Solution (Val : Integer) is
   begin
      Put_Line (Trim (Val'Image, Ada.Strings.Left));
   end Solution;

   procedure Solution (Val : String) is
   begin
      Put_Line (Val);
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

   function Split
     (S          : String;
      Delims     : String;
      Keep_Empty : Boolean := True)
      return String_Array
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
     (S          : Unbounded_String;
      Delims     : String;
      Keep_Empty : Boolean := True)
      return String_Array
   is
   begin
      return Split (To_String (S), Delims, Keep_Empty);
   end Split;

   --  function Split_Any (S : String; Delims : String) return String_Array is
   --     Result : String_Array;

   --     --  Left and right indices of the current substring
   --     L : Integer := S'First;
   --     R : Integer := L;
   --  begin
   --     if S'Length = 0 then
   --        return Result;
   --     end if;

   --     while R in S'Range loop
   --        if Is_Any_Of (S (R), Delims) then
   --           Result.Append (Slice_Empty (S, L, R - 1));
   --           L := R + 1;
   --        end if;
   --        R := R + 1;
   --     end loop;
   --     Result.Append (Slice_Empty (S, L, R - 1));
   --     return Result;
   --  end Split_Any;

   function Read_All_Lines (Filename : String) return String_Array is
      Input_File : File_Type;
      Result     : String_Array;
   begin
      Open (Input_File, In_File, Filename);
      while not End_Of_File (Input_File) loop
         declare
            Line : constant String := Get_Line (Input_File);
         begin
            Result.Append (Line);
         end;
      end loop;
      Close (Input_File);
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

   function Contains (R : Rectangle; P : Point) return Boolean is
   begin
      return
        P.X >= R.Left and then P.X <= R.Right and then P.Y >= R.Bottom
        and then P.Y <= R.Top;
   end Contains;

   procedure Inflate (R : in out Rectangle; Delta_X, Delta_Y : Integer) is
   begin
      R.Left   := R.Left - Delta_X;
      R.Right  := R.Right + Delta_X;
      R.Top    := R.Top + Delta_Y;
      R.Bottom := R.Bottom - Delta_Y;
   end Inflate;

end Advent;
