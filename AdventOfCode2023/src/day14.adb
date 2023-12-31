with Advent;                     use Advent;
with Advent.Directions;          use Advent.Directions;
with Ada.Command_Line;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Day14 is
   package Sparse_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Vec2, Element_Type => Character, Hash => Hash, Equivalent_Keys => "=");

   use type Sparse_Maps.Cursor;

   subtype Sparse_Map is Sparse_Maps.Map;

   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Integer);

   subtype Load_History is Integer_Vectors.Vector;

   function Contains (R : Rectangle; P : Vec2) return Boolean is
     (R.Left <= P (1) and then P (1) <= R.Right and then R.Top <= P (0)
      and then P (0) <= R.Bottom);

   procedure Settle (M : in out Sparse_Map; Rock : Vec2; Dir : Direction; Region : Rectangle) is
      Dir_Vec : constant Vec2 := To_Vector (Dir);
      Current : Vec2          := Rock + Dir_Vec;
      Cursor  : Sparse_Maps.Cursor;
   begin
      while Contains (Region, Current) loop
         Cursor := M.Find (Current);
         exit when Cursor /= Sparse_Maps.No_Element
           and then (M (Cursor) = '#' or else M (Cursor) = 'O');
         Current := Current + Dir_Vec;
      end loop;
      --  either hit edge or another object
      M.Delete (Rock);
      M.Insert (Current - Dir_Vec, 'O');
   end Settle;

   procedure Tilt (M : in out Sparse_Map; Dir : Direction; Region : Rectangle) is
      Cursor : Sparse_Maps.Cursor;

      procedure Settle_One (V : Vec2) is
      begin
         Cursor := M.Find (V);
         if Cursor /= Sparse_Maps.No_Element and then Sparse_Maps.Element (Cursor) = 'O' then
            Settle (M, V, Dir, Region);
         end if;
      end Settle_One;
   begin
      case Dir is
         when North =>
            for I in Region.Top .. Region.Bottom loop
               for J in Region.Left .. Region.Right loop
                  Settle_One ((I, J));
               end loop;
            end loop;
         when South =>
            for I in reverse Region.Top .. Region.Bottom loop
               for J in Region.Left .. Region.Right loop
                  Settle_One ((I, J));
               end loop;
            end loop;
         when West =>
            for J in Region.Left .. Region.Right loop
               for I in Region.Top .. Region.Bottom loop
                  Settle_One ((I, J));
               end loop;
            end loop;
         when East =>
            for J in reverse Region.Left .. Region.Right loop
               for I in Region.Top .. Region.Bottom loop
                  Settle_One ((I, J));
               end loop;
            end loop;
      end case;
   end Tilt;

   function Total_Load (M : Sparse_Map; Region : Rectangle) return Integer is
      Cursor : Sparse_Maps.Cursor;
      Result : Integer := 0;
   begin
      for I in Region.Top .. Region.Bottom loop
         for J in Region.Left .. Region.Right loop
            Cursor := M.Find ((I, J));
            if Cursor /= Sparse_Maps.No_Element and then Sparse_Maps.Element (Cursor) = 'O' then
               Result := Result + Region.Bottom - I + 1;
            end if;
         end loop;
      end loop;
      return Result;
   end Total_Load;

   procedure Put_Map (M : Sparse_Map; Region : Rectangle) is
      Cursor : Sparse_Maps.Cursor;
   begin
      for I in Region.Top .. Region.Bottom loop
         for J in Region.Left .. Region.Right loop
            Cursor := M.Find ((I, J));
            if Cursor = Sparse_Maps.No_Element then
               Put ('.');
            else
               Put (Sparse_Maps.Element (Cursor));
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Put_Map;

   type Cycle_Info is record
      Found        : Boolean;
      Base, Length : Integer;
   end record;

   No_Cycle : constant Cycle_Info := (Found => False, others => 0);

   function Extrapolate
     (History : Load_History; Info : Cycle_Info; Time : Integer) return Integer is
     (History.Element (Info.Base + (Time - Info.Base) rem Info.Length)) with
     Pre => Info.Found;

   --  Floyd's tortoise and hare algorithm
   --  Reference: https://en.wikipedia.org/wiki/Cycle_detection
   function Find_Cycle (V : Load_History) return Cycle_Info is
      Tortoise : Integer := 2;
      Hare     : Integer := 3;
      Mu       : Integer := 0;
      Lam      : Integer := 0;

      function Both_In_Bounds return Boolean is
      begin
         return
           Tortoise in V.First_Index .. V.Last_Index
           and then Hare in V.First_Index .. V.Last_Index;
      end Both_In_Bounds;
   begin
      if not Both_In_Bounds then
         return No_Cycle;
      end if;

      while V.Element (Tortoise) /= V.Element (Hare) loop
         Tortoise := Tortoise + 1;
         Hare     := Hare + 2;
         if not Both_In_Bounds then
            return No_Cycle;
         end if;
      end loop;

      Mu       := 0;
      Tortoise := 1;
      while V.Element (Tortoise) /= V.Element (Hare) loop
         Tortoise := Tortoise + 1;
         Hare     := Hare + 1;
         Mu       := Mu + 1;
         if not Both_In_Bounds then
            return No_Cycle;
         end if;
      end loop;

      Lam  := 1;
      Hare := Tortoise + 1;
      while V.Element (Tortoise) /= V.Element (Hare) loop
         Hare := Hare + 1;
         Lam  := Lam + 1;
         if not Both_In_Bounds then
            return No_Cycle;
         end if;
      end loop;

      return Cycle_Info'(Found => True, Base => Mu, Length => Lam);
   end Find_Cycle;

   function Load_Sparse (M : Char_Matrix) return Sparse_Map is
      Result : Sparse_Map;
   begin
      for I in 1 .. Rows (M) loop
         for J in 1 .. Cols (M) loop
            if M (I, J) = 'O' or else M (I, J) = '#' then
               Result.Insert ((I, J), M (I, J));
            end if;
         end loop;
      end loop;
      return Result;
   end Load_Sparse;

   Tilemap : constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));
   Region  : constant Rectangle   :=
     (Left => 1, Right => Cols (Tilemap), Top => 1, Bottom => Rows (Tilemap));

   Map : Sparse_Map := Load_Sparse (Tilemap);

   Order : constant array (0 .. 3) of Direction := (North, West, South, East);

   History : Load_History;
   Cycles  : Cycle_Info;

begin

   Tilt (Map, North, Region);
   Solution (Total_Load (Map, Region));

   loop
      for Dir of Order loop
         Tilt (Map, Dir, Region);
      end loop;
      History.Append (Total_Load (Map, Region));
      Cycles := Find_Cycle (History);
      exit when Cycles.Found;
   end loop;

   Solution (Extrapolate (History, Cycles, 1_000_000_000));

end Day14;
