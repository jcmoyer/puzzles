--  Some suggested restrictions optimizing for performance
--  https://gcc.gnu.org/onlinedocs/gnat_ugn/Use-of-Restrictions.html
pragma Restrictions (No_Abort_Statements);
pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);
--  Saves 20ms out of 320ms
pragma Restrictions (No_Finalization);

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Real_Time; use Ada.Real_Time;
with System.Multiprocessors;
with Interfaces;    use Interfaces;

with Day04_Solver;

procedure Day04 is
   Results    : constant Day04_Solver.Search_Results_Ptr := new Day04_Solver.Search_Results;
   Start_Time : Time;
   End_Time   : Time;

   function Get_Input return String is
   begin
      if Ada.Command_Line.Argument_Count = 0 then
         return "yzbqklnj";
      else
         return Ada.Command_Line.Argument (1);
      end if;
   end Get_Input;

   Base : constant String := Get_Input;
begin
   if Base'Length > Day04_Solver.Maximum_Template_String_Length then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Input string too long; max is " & Day04_Solver.Maximum_Template_String_Length'Image & " characters");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   Start_Time := Clock;
   declare
      Searches : array (1 .. System.Multiprocessors.Number_Of_CPUs) of Day04_Solver.Search_Task;
   begin
      for I in Searches'Range loop
         Searches (I).Start
           (Start => Unsigned_32 (I), Step_By => Searches'Length, Template_String => Base, Results => Results);
      end loop;
   end;
   End_Time := Clock;

   Ada.Text_IO.Put_Line ("Part 1:" & Results.Get_Best_5'Image);
   Ada.Text_IO.Put_Line ("Part 2:" & Results.Get_Best_6'Image);
   Ada.Text_IO.Put_Line ("Search time:" & Duration'Image (1e6 * To_Duration (End_Time - Start_Time)) & " us");
end Day04;
