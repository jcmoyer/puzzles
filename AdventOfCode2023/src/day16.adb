with Advent;                     use Advent;
with Advent.IO;                  use Advent.IO;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers;             use Ada.Containers;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

procedure Day16 is
   type Beam is record
      Position : Vec2;
      Dir      : Direction;
   end record;

   function Hash (B : Beam) return Hash_Type is
   begin
      return Hash (B.Position) xor Hash_Type (Direction'Pos (B.Dir)) * 17;
   end Hash;

   package Beam_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Beam);

   subtype Beam_Vector is Beam_Vectors.Vector;

   package Beam_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Beam, Hash => Hash, Equivalent_Elements => "=");

   subtype Beam_Set is Beam_Sets.Set;

   type Energized_Map is array (Integer range <>, Integer range <>) of Boolean;

   type Const_Char_Matrix_Ptr is access constant Char_Matrix;

   type Energized_Map_Ptr is access all Energized_Map;

   type World_State is record
      Map          : Const_Char_Matrix_Ptr;
      Pending      : Beam_Vector;
      Energized    : Energized_Map_Ptr;
      Resolved_Set : Beam_Set;
   end record;

   procedure Resolve_Splitter
     (Pending : in out Beam_Vector; Position : Vec2; Enter_From : Direction; Kind : Character) with
     Pre => Kind = '|' or else Kind = '-', Post => Pending.Length > Pending'Old.Length
   is
   begin

      if Kind = '|' then

         if Enter_From = West or else Enter_From = East then
            Pending.Append ((Position => Position + To_Vector (North), Dir => North));
            Pending.Append ((Position => Position + To_Vector (South), Dir => South));
         else
            Pending.Append
              ((Position => Position + To_Vector (Opposite (Enter_From)),
                Dir      => Opposite (Enter_From)));
         end if;

      elsif Kind = '-' then

         if Enter_From = West or else Enter_From = East then
            Pending.Append
              ((Position => Position + To_Vector (Opposite (Enter_From)),
                Dir      => Opposite (Enter_From)));
         else
            Pending.Append ((Position => Position + To_Vector (West), Dir => West));
            Pending.Append ((Position => Position + To_Vector (East), Dir => East));
         end if;

      end if;

   end Resolve_Splitter;

   procedure Resolve_Mirror
     (Pending : in out Beam_Vector; Position : Vec2; Enter_From : Direction; Kind : Character) with
     Pre => Kind = '/' or else Kind = '\', Post => Pending.Length > Pending'Old.Length
   is
   begin

      if Kind = '/' then

         case Enter_From is
            when North =>
               Pending.Append ((Position => Position + To_Vector (West), Dir => West));
            when South =>
               Pending.Append ((Position => Position + To_Vector (East), Dir => East));
            when West =>
               Pending.Append ((Position => Position + To_Vector (North), Dir => North));
            when East =>
               Pending.Append ((Position => Position + To_Vector (South), Dir => South));
         end case;

      elsif Kind = '\' then

         case Enter_From is
            when North =>
               Pending.Append ((Position => Position + To_Vector (East), Dir => East));
            when South =>
               Pending.Append ((Position => Position + To_Vector (West), Dir => West));
            when West =>
               Pending.Append ((Position => Position + To_Vector (South), Dir => South));
            when East =>
               Pending.Append ((Position => Position + To_Vector (North), Dir => North));
         end case;

      end if;

   end Resolve_Mirror;

   procedure Resolve (World : in out World_State; B : Beam) is
      Dir_Vec      : constant Vec2 := To_Vector (B.Dir);
      Current_Pos  : Vec2          := B.Position;
      Current_Char : Character;
   begin
      while Current_Pos (0) in World.Map'Range (1) and then Current_Pos (1) in World.Map'Range (2)
      loop
         World.Energized (Current_Pos (0), Current_Pos (1)) := True;

         Current_Char := World.Map (Current_Pos (0), Current_Pos (1));

         if Current_Char = '-' or else Current_Char = '|' then
            Resolve_Splitter (World.Pending, Current_Pos, Opposite (B.Dir), Current_Char);
            exit;
         elsif Current_Char = '/' or else Current_Char = '\' then
            Resolve_Mirror (World.Pending, Current_Pos, Opposite (B.Dir), Current_Char);
            exit;
         end if;

         Current_Pos := Current_Pos + Dir_Vec;
      end loop;
   end Resolve;

   function Count_Energized (Energized : Energized_Map) return Integer is
      Result : Integer := 0;
   begin
      for Is_Energized of Energized loop
         if Is_Energized then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count_Energized;

   procedure Print_Map (Map : Char_Matrix; Energized : Energized_Map) is
   begin
      for I in 1 .. Rows (Map) loop
         for J in 1 .. Cols (Map) loop
            if Energized (I, J) then
               Put ('#');
            else
               Put (Map (I, J));
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Map;

   procedure Reset (World : in out World_State) is
   begin
      World.Resolved_Set.Clear;
      World.Energized.all := (others => (others => False));
   end Reset;

   function Solve (World : in out World_State; Start : Vec2; Dir : Direction) return Integer is
      Current : Beam;
   begin
      Reset (World);

      World.Pending.Append (Beam'(Position => Start, Dir => Dir));
      while World.Pending.Length > 0 loop
         Current := World.Pending.Last_Element;
         World.Pending.Delete_Last;

         if not World.Resolved_Set.Contains (Current) then
            Resolve (World, Current);
            World.Resolved_Set.Include (Current);
         end if;
      end loop;

      return Count_Energized (World.Energized.all);
   end Solve;

   Map : aliased constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));

   World : World_State :=
     (Map       => Map'Access,
      Energized => new Energized_Map (Map'Range (1), Map'Range (2)),
      others    => <>);

   Max_Energized : Integer := 0;

begin

   Solution (Solve (World, (1, 1), Dir => East));

   for I in 1 .. Rows (Map) loop
      Max_Energized := Integer'Max (Max_Energized, Solve (World, (I, 1), Dir => East));
      Max_Energized := Integer'Max (Max_Energized, Solve (World, (I, Cols (Map)), Dir => West));
   end loop;

   for J in 1 .. Cols (Map) loop
      Max_Energized := Integer'Max (Max_Energized, Solve (World, (1, J), Dir => South));
      Max_Energized := Integer'Max (Max_Energized, Solve (World, (Rows (Map), J), Dir => North));
   end loop;

   Solution (Max_Energized);

end Day16;
