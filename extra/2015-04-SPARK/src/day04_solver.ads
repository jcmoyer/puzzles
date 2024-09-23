with Interfaces; use Interfaces;

package Day04_Solver is

   pragma Preelaborate;

   protected type Search_Results is
      procedure Set_Best_5 (Value : Unsigned_32);
      procedure Set_Best_6 (Value : Unsigned_32);
      function Get_Best_5 return Unsigned_32;
      function Get_Best_6 return Unsigned_32;
   private
      Best_5 : Unsigned_32 := Unsigned_32'Last;
      Best_6 : Unsigned_32 := Unsigned_32'Last;
   end Search_Results;

   type Search_Results_Ptr is access Search_Results;

   task type Search_Task is
      entry Start (Start, Step_By : Unsigned_32; Template_String : String; Results : Search_Results_Ptr);
   end Search_Task;

   Short_String_Size              : constant := 32;
   --  Maximum number of digits in a U32 is 10; reserve space for them
   Maximum_Template_String_Length : constant := Short_String_Size - 10;

end Day04_Solver;
