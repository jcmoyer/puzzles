with Md5;
with Md5.Hashing;
with Md5.Short_Strings;

package body Day04_Solver is
   protected body Search_Results is
      procedure Set_Best_5 (Value : Unsigned_32) is
      begin
         Best_5 := Unsigned_32'Min (Best_5, Value);
      end Set_Best_5;

      procedure Set_Best_6 (Value : Unsigned_32) is
      begin
         Best_6 := Unsigned_32'Min (Best_6, Value);
      end Set_Best_6;

      function Get_Best_5 return Unsigned_32 is
      begin
         return Best_5;
      end Get_Best_5;

      function Get_Best_6 return Unsigned_32 is
      begin
         return Best_6;
      end Get_Best_6;
   end Search_Results;

   task body Search_Task is
      Position     : Unsigned_32;
      Step         : Unsigned_32;
      Hash         : Md5.Hashing.State;
      Base         : Md5.Short_String (Short_String_Size);
      The_Results  : Search_Results_Ptr;
      Write_Offset : Md5.Short_String_Index;

      function Continue_Search return Boolean is (Position < The_Results.Get_Best_6);
      pragma Inline (Continue_Search);
   begin
      accept Start (Start, Step_By : Unsigned_32; Template_String : String; Results : Search_Results_Ptr) do
         Position     := Start;
         Step         := Step_By;
         The_Results  := Results;
         Write_Offset := Template_String'Length + 1;
         Md5.Short_Strings.Assign (Base, Template_String);
      end Start;

      while Continue_Search loop
         Md5.Short_Strings.Overwrite_Digits (Base, Write_Offset, Position);
         Md5.Hashing.Hash_String (Hash, Base);

         if (Hash.A and 16#00F0_FFFF#) = 0 then
            The_Results.Set_Best_5 (Position);
            if (Hash.A and 16#00FF_FFFF#) = 0 then
               The_Results.Set_Best_6 (Position);
               exit;
            end if;
         end if;
         Position := Position + Step;
      end loop;
   end Search_Task;
end Day04_Solver;
