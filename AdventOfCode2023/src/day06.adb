with Advent;                use Advent;
with Advent.Intervals;
with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Day06 is
   type Time_Type is new Long_Long_Integer;
   type Distance_Type is new Long_Long_Integer;

   type Race_Record is record
      Time     : Time_Type;
      Distance : Distance_Type;
   end record;

   function Calc_Distance
     (Charge_Time, Available_Time : Time_Type) return Distance_Type
   is
      Remaining_Time : Time_Type := Available_Time - Charge_Time;
   begin
      if Remaining_Time <= 0 then
         return 0;
      end if;
      return Distance_Type (Remaining_Time * Charge_Time);
   end Calc_Distance;

   function Find_Min_Time (R : Race_Record) return Time_Type is
   begin
      for I in 1 .. R.Time loop
         if Calc_Distance (Charge_Time => I, Available_Time => R.Time) >
           R.Distance
         then
            return I;
         end if;
      end loop;
   end Find_Min_Time;

   function Find_Max_Time (R : Race_Record) return Time_Type is
   begin
      for I in reverse 1 .. R.Time loop
         if Calc_Distance (Charge_Time => I, Available_Time => R.Time) >
           R.Distance
         then
            return I;
         end if;
      end loop;
   end Find_Max_Time;

   Lines : constant String_Array := Read_All_Lines ("test/2023-06-input.txt");

   --  TODO: more than 4 records
   Record_P1 : array (1 .. 4) of Race_Record;
   Record_P2 : Race_Record;

begin
   for Line of Lines loop
      if Starts_With (Line, "Time:") then
         declare
            Time_Strs : constant String_Array :=
              Split (Line, " ", Keep_Empty => False);
            First     : Integer               := Time_Strs.First_Index + 1;
            Long_Time : Unbounded_String;
         begin
            for I in First .. Time_Strs.Last_Index loop
               Record_P1 (Record_P1'First + I - First).Time :=
                 Time_Type'Value (Time_Strs (I));
               Append (Long_Time, Time_Strs (I));
            end loop;
            Record_P2.Time := Time_Type'Value (To_String (Long_Time));
         end;
      elsif Starts_With (Line, "Distance:") then
         declare
            Dist_Strs : constant String_Array :=
              Split (Line, " ", Keep_Empty => False);
            First     : Integer               := Dist_Strs.First_Index + 1;
            Long_Dist : Unbounded_String;
         begin
            for I in Dist_Strs.First_Index + 1 .. Dist_Strs.Last_Index loop
               Record_P1 (Record_P1'First + I - First).Distance :=
                 Distance_Type'Value (Dist_Strs (I));
               Append (Long_Dist, Dist_Strs (I));
            end loop;
            Record_P2.Distance := Distance_Type'Value (To_String (Long_Dist));
         end;
      end if;
   end loop;

   declare
      Prod : Time_Type := 1;
   begin
      for R of Record_P1 loop
         Prod := Prod * (Find_Max_Time (R) - Find_Min_Time (R) + 1);
      end loop;
      Solution (Long_Long_Integer (Prod));
   end;

   Solution
     (Long_Long_Integer
        (Find_Max_Time (Record_P2) - Find_Min_Time (Record_P2) + 1));
end Day06;
