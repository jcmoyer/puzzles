with Advent;                     use Advent;
with Advent.IO;                  use Advent.IO;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers;             use Ada.Containers;
with Ada.Text_IO;

procedure Day10 is
   --
   --  Containers for 4-tile adjacent (math) vectors
   --
   package Vec2_Bounded_Vectors is new Ada.Containers.Bounded_Vectors
     (Index_Type => Positive, Element_Type => Vec2);

   subtype Adjacent_Vectors is Vec2_Bounded_Vectors.Vector (4);

   --
   --  Tiles
   --
   type Tile is record
      Connects : Direction_Flags := (others => False);
      Visited  : Boolean         := False;
   end record;

   type Tile_Ptr is access all Tile;

   type Tile_Matrix is array (Integer range <>, Integer range <>) of aliased Tile;

   type Tile_Matrix_Ptr is access all Tile_Matrix;

   function To_Tile (C : Character) return Tile is
   begin
      case C is
         when '|' =>
            return (Connects => (North => True, South => True, others => False), others => <>);
         when '-' =>
            return (Connects => (West => True, East => True, others => False), others => <>);
         when 'L' =>
            return (Connects => (North => True, East => True, others => False), others => <>);
         when 'J' =>
            return (Connects => (North => True, West => True, others => False), others => <>);
         when '7' =>
            return (Connects => (South => True, West => True, others => False), others => <>);
         when 'F' =>
            return (Connects => (South => True, East => True, others => False), others => <>);
         when others =>
            return (others => <>);
      end case;
   end To_Tile;

   --  The pipe has a horizontal component.
   function Has_Horizontal (T : Tile) return Boolean is
     (T.Connects (West) or else T.Connects (East));

   --  The pipe has a vertical component.
   function Has_Vertical (T : Tile) return Boolean is
     (T.Connects (North) or else T.Connects (South));

   --  The entire pipe is vertical.
   function Is_Vertical (T : Tile) return Boolean is
     (T.Connects (North) and then T.Connects (South));

   --  The entire pipe is horizontal.
   function Is_Horizontal (T : Tile) return Boolean is
     (T.Connects (West) and then T.Connects (East));

   function Is_NW (T : Tile) return Boolean is (T.Connects (North) and then T.Connects (West));
   function Is_NE (T : Tile) return Boolean is (T.Connects (North) and then T.Connects (East));
   function Is_SW (T : Tile) return Boolean is (T.Connects (South) and then T.Connects (West));
   function Is_SE (T : Tile) return Boolean is (T.Connects (South) and then T.Connects (East));

   function Is_Corner (T : Tile) return Boolean is (Has_Horizontal (T) and then Has_Vertical (T));
   function Connects_Same_Vertical (A, B : Tile) return Boolean is
     (A.Connects (North) = B.Connects (North));

   --
   --  Tilemap
   --
   type Map is record
      Start      : Vec2;
      Tiles      : Tile_Matrix_Ptr;
      Rows, Cols : Integer;
   end record;

   function Element (M : Map; Index : Vec2) return Tile is
   begin
      return M.Tiles (Index (0), Index (1));
   end Element;

   function Reference (M : Map; Index : Vec2) return Tile_Ptr is
   begin
      return M.Tiles (Index (0), Index (1))'Access;
   end Reference;

   function In_Bounds (M : Map; Index : Vec2) return Boolean is
   begin
      return Index (0) in 1 .. M.Rows and then Index (1) in 1 .. M.Cols;
   end In_Bounds;

   --  Clears the contents of Buffer and replaces them with the relative
   --  offsets from the tile at Index.
   procedure Get_Adjacent_Indices (M : Map; Index : Vec2; Buffer : in out Adjacent_Vectors) is
      T : constant Tile := Element (M, Index);
   begin
      Buffer.Clear;
      for Dir in Direction loop
         if T.Connects (Dir) then
            declare
               Adjacent_Index : constant Vec2 := Index + To_Vector (Dir);
            begin
               if In_Bounds (M, Adjacent_Index) then
                  Buffer.Append (Index + To_Vector (Dir));
               end if;
            end;
         end if;
      end loop;
   end Get_Adjacent_Indices;

   --  Loads character data and converts it to a more ergonomic format.
   function Load_Map (Filename : String) return Map is
      Chars  : constant Char_Matrix := Read_Tilemap (Filename);
      Result : Map;
   begin
      Result.Rows  := Rows (Chars);
      Result.Cols  := Cols (Chars);
      Result.Tiles := new Tile_Matrix (1 .. Result.Rows, 1 .. Result.Cols);

      for I in 1 .. Rows (Chars) loop
         for J in 1 .. Cols (Chars) loop
            if Chars (I, J) = 'S' then
               Result.Start := (I, J);
            end if;
            Reference (Result, (I, J)).all := To_Tile (Chars (I, J));
         end loop;
      end loop;

      --  Determine how the start is connected by looking in each direction
      --  and seeing if any adjacent tiles are connected towards the start.
      for Dir in Direction loop
         declare
            Index : constant Vec2 := Result.Start + To_Vector (Dir);
         begin
            if In_Bounds (Result, Index) then
               declare
                  Tile_At_Index : constant Tile := Element (Result, Index);
               begin
                  if Tile_At_Index.Connects (Opposite (Dir)) then
                     Reference (Result, Result.Start).Connects (Dir) := True;
                  end if;
               end;
            end if;
         end;
      end loop;

      return Result;
   end Load_Map;

   type Path_State is record
      Index : Vec2;
      Steps : Integer := 0;
   end record;

   --  TODO: Ideally should use a queue or deque for this but didn't see a
   --  basic queue in the Ada RM from a quick glance. Not that it matters much
   --  for such a small dataset.
   package Path_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Path_State);

   function Find_Furthest_Connected (M : Map) return Integer is
      Explore         : Path_Vectors.Vector;
      Adjacent_Buffer : Adjacent_Vectors;

      Start_Index : constant Vec2     := M.Start;
      Start_Tile  : constant Tile_Ptr := Reference (M, Start_Index);

      Result : Integer := 0;
   begin
      Start_Tile.Visited := True;

      Get_Adjacent_Indices (M, Start_Index, Adjacent_Buffer);
      for Index of Adjacent_Buffer loop
         Explore.Append ((Index => Index, Steps => 1));
      end loop;

      while Explore.Length > 0 loop
         declare
            Next_State : constant Path_State := Explore.First_Element;
            Next_Tile  : constant Tile_Ptr   := Reference (M, Next_State.Index);
         begin
            Explore.Delete_First;

            Next_Tile.Visited := True;
            Result            := Integer'Max (Result, Next_State.Steps);

            Get_Adjacent_Indices (M, Next_State.Index, Adjacent_Buffer);

            for Index of Adjacent_Buffer loop
               if In_Bounds (M, Index) and then not Element (M, Index).Visited then
                  Explore.Append ((Index => Index, Steps => Next_State.Steps + 1));
               end if;
            end loop;
         end;
      end loop;

      return Result;
   end Find_Furthest_Connected;

   --  Implements https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm
   --
   --  The gist of this algorithm is that we scan left to right counting the
   --  number of intersections between the ray we're casting and edges of the
   --  polygon.
   --
   --  Since we're moving left to right, any vertical edge can potentially put
   --  us inside the polygon. This includes both totally vertical pipes ('|')
   --  and partially vertical pipes ('L', 'F', '7', 'J'). Crossing a vertical
   --  pipe will always flip the inside/outside state. Corner pipes only flip
   --  the inside/outside state if they go in opposite vertical directions.
   --
   --  Totally horizontal pipes have no effect for this algorithm.
   --
   --  Since the grid is 2D and we're scanning left to right, any corner tile
   --  must be followed by any number of horizontal pipes and exactly one
   --  corner tile. More specifically, they must appear in pairs, so we can
   --  always scan from one corner to the next.
   --
   --  Several cases to consider:
   --
   --  .F7
   --  .|L-7.  <--  INSIDE after |, OUTSIDE after 7
   --  .|..|.
   --
   --  .F7.|.
   --  .|L-J.  <--  INSIDE after |, INSIDE after J
   --  .L----
   --
   --  .||.|.  <--  INSIDE after first |, OUTSIDE after second |,
   --               INSIDE after third |
   function Count_Enclosed (M : Map) return Integer is
      Result      : Integer := 0;
      Row         : Integer;
      Col         : Integer;
      N_Crossings : Integer := 0;

      procedure Move_To_Matching_Corner with
        Pre => Is_Corner (Element (M, (Row, Col))), Post => Is_Corner (Element (M, (Row, Col)))
      is
         This_Tile : Tile := Element (M, (Row, Col));
      begin
         Col       := Col + 1;
         This_Tile := Element (M, (Row, Col));
         while not Is_Corner (This_Tile) loop
            Col       := Col + 1;
            This_Tile := Element (M, (Row, Col));
         end loop;
      end Move_To_Matching_Corner;
   begin
      Row := 1;
      while Row <= M.Rows loop
         Col         := 1;
         N_Crossings := 0;
         while Col <= M.Cols loop
            declare
               This_Tile : constant Tile := Element (M, (Row, Col));
            begin
               if This_Tile.Visited then
                  if Is_Vertical (This_Tile) then
                     N_Crossings := N_Crossings + 1;
                  elsif Is_Corner (This_Tile) then
                     declare
                        Left_Corner  : constant Tile := This_Tile;
                        Right_Corner : Tile;
                     begin
                        Move_To_Matching_Corner;
                        Right_Corner := Element (M, (Row, Col));

                        if not Connects_Same_Vertical (Left_Corner, Right_Corner) then
                           N_Crossings := N_Crossings + 1;
                        end if;
                     end;
                  end if;
               else
                  if N_Crossings rem 2 = 1 then
                     Result := Result + 1;
                  end if;
               end if;
               Col := Col + 1;
            end;
         end loop;
         Row := Row + 1;
      end loop;
      return Result;
   end Count_Enclosed;

   procedure Print_Map (M : Map) is
      use Ada.Text_IO;

      This : Tile;
   begin
      for Row in 1 .. M.Rows loop
         for Col in 1 .. M.Cols loop
            This := Element (M, (Row, Col));
            if This.Visited then
               if Is_Vertical (This) then
                  Put ("│");
               elsif Is_Horizontal (This) then
                  Put ("─");
               elsif Is_NE (This) then
                  Put ("└");
               elsif Is_NW (This) then
                  Put ("┘");
               elsif Is_SE (This) then
                  Put ("┌");
               elsif Is_SW (This) then
                  Put ("┐");
               end if;
            else
               Put ('.');
            end if;
         end loop;
      end loop;
   end Print_Map;

   --  locals
   M : constant Map := Load_Map (Ada.Command_Line.Argument (1));

begin

   Solution (Find_Furthest_Connected (M));
   Solution (Count_Enclosed (M));

end Day10;
