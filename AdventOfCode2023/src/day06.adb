with Advent;         use Advent;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Advent.Parsers.Integers;
with Ada.Containers; use Ada.Containers;
with Ada.Command_Line;

procedure Day06 is
   type Time_Type is new Long_Long_Integer;

   package Time_Parsers is new Advent.Parsers.Integers (Element_Type => Time_Type);

   type Distance_Type is new Long_Long_Integer;

   package Distance_Parsers is new Advent.Parsers.Integers (Element_Type => Distance_Type);

   type Race_Records is record
      Time     : Time_Parsers.Vector;
      Distance : Distance_Parsers.Vector;
   end record;

   function Calc_Distance (Charge_Time, Available_Time : Time_Type) return Distance_Type is
      Remaining_Time : constant Time_Type := Available_Time - Charge_Time;
   begin
      if Remaining_Time <= 0 then
         return 0;
      end if;
      return Distance_Type (Remaining_Time * Charge_Time);
   end Calc_Distance;

   No_Solution : exception;

   function Find_Min_Time (T : Time_Type; D : Distance_Type) return Time_Type is
   begin
      for I in 1 .. T loop
         if Calc_Distance (Charge_Time => I, Available_Time => T) > D then
            return I;
         end if;
      end loop;
      raise No_Solution;
   end Find_Min_Time;

   function Find_Max_Time (T : Time_Type; D : Distance_Type) return Time_Type is
   begin
      for I in reverse 1 .. T loop
         if Calc_Distance (Charge_Time => I, Available_Time => T) > D then
            return I;
         end if;
      end loop;
      raise No_Solution;
   end Find_Max_Time;

   function Find_Charge_Time (T : Time_Type; D : Distance_Type) return Time_Type is
     (1 + Find_Max_Time (T, D) - Find_Min_Time (T, D));

   function Way_Product (R : Race_Records) return Long_Long_Integer with
     Pre => R.Distance.Length = R.Time.Length
   is
      Result : Time_Type := 1;
   begin
      for I in R.Distance.First_Index .. R.Distance.Last_Index loop
         Result := Result * Find_Charge_Time (R.Time (I), R.Distance (I));
      end loop;
      return Long_Long_Integer (Result);
   end Way_Product;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Record_P1 : Race_Records;
   Record_P2 : Race_Records;

begin
   for Line of Lines loop
      if Starts_With (Line, "Time:") then
         Record_P1.Time := Time_Parsers.Extract_Integers (Line);
         Record_P2.Time := Time_Parsers.Extract_Integers (Delete_Whitespace (Line));
      elsif Starts_With (Line, "Distance:") then
         Record_P1.Distance := Distance_Parsers.Extract_Integers (Line);
         Record_P2.Distance := Distance_Parsers.Extract_Integers (Delete_Whitespace (Line));
      end if;
   end loop;

   Solution (Way_Product (Record_P1));
   Solution (Way_Product (Record_P2));
end Day06;
