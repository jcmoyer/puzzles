with Advent;                     use Advent;
with Advent.IO;                  use Advent.IO;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Interfaces;                 use Interfaces;

procedure Day23 is

   --  Input observations:
   --
   --  paths are all one tile wide, so it should be fast to explore each branch
   --  grid is 141x141 tiles = 19881 cells; 9292 are '.' 10471 are '#'
   --
   --  There are no '^' characters in the input
   --
   --  A dot adjacent to an arrow is always surrounded by multiple (3+?) arrows
   --
   --  A path always leads to exactly one junction
   --
   --  Passing through a junction is the only way to get to a different
   --  junction, there are no gaps in the walls
   --
   --  An arrow is always approached from the opposite direction it points

   type Step_Count is new Natural;

   package Vec2_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Vec2, Hash => Hash, Equivalent_Elements => "=");

   function Try_Element (Map : Char_Matrix; Indices : Vec2; Char : out Character) return Boolean is
   begin
      if In_Bounds (Map, Indices) then
         Char := Element (Map, Indices);
         return True;
      else
         return False;
      end if;
   end Try_Element;

   function Is_Directional (C : Character) return Boolean is
     (C = '<' or else C = '>' or else C = '^' or else C = 'v');

   function Find_All_Junctions (M : Char_Matrix) return Vec2_Sets.Set is
      Result : Vec2_Sets.Set;
   begin
      for I in 1 .. Rows (M) loop
         for J in 1 .. Cols (M) loop
            declare
               N_Directions : Integer := 0;
               Char         : Character;
            begin
               for D in Cardinal_Direction loop
                  if Element (M, (I, J)) = '.'
                    and then Try_Element (M, Vec2'(I, J) + To_Vector (D), Char)
                  then
                     if Is_Directional (Char) then
                        N_Directions := N_Directions + 1;
                     end if;
                  end if;
               end loop;
               if N_Directions >= 2 then
                  Result.Insert ((I, J));
               end if;
            end;
         end loop;
      end loop;
      return Result;
   end Find_All_Junctions;

   type Edge_Index is new Positive;
   type Node_Index is new Positive;

   package Edge_Index_Vectors is new Ada.Containers.Vectors
     (Index_Type => Edge_Index, Element_Type => Integer);

   type Edge is record
      Steps    : Step_Count;
      From, To : Node_Index;
   end record;

   type Node is record
      Position : Vec2;
      Edge_Ids : Edge_Index_Vectors.Vector;
   end record;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type => Node_Index, Element_Type => Node);

   package Node_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Vec2, Element_Type => Node_Index, Equivalent_Keys => "=", Hash => Hash);

   package Edge_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Edge);

   type Graph is record
      Nodes             : Node_Vectors.Vector;
      Nodes_By_Position : Node_Maps.Map;
      Edges             : Edge_Vectors.Vector;
   end record;

   procedure Add_Node (G : in out Graph; Position : Vec2) is
   begin
      G.Nodes.Append ((Position => Position, Edge_Ids => <>));
      G.Nodes_By_Position.Insert (Position, G.Nodes.Last_Index);
   end Add_Node;

   function Get_Node_Index (G : Graph; Key : Vec2) return Node_Index is
   begin
      return G.Nodes_By_Position.Element (Key);
   end Get_Node_Index;

   function Contains_Edge (G : Graph; From, To : Vec2) return Boolean is
   begin
      for E of G.Edges loop
         if G.Nodes (E.From).Position = From and then G.Nodes (E.To).Position = To then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Edge;

   procedure Add_Edge
     (G : in out Graph; From, To : Vec2; Steps : Step_Count; Bidirectional : Boolean) with
     Pre => G.Nodes_By_Position.Contains (From) and then G.Nodes_By_Position.Contains (To)
   is
      From_Index : constant Node_Index := G.Nodes_By_Position.Element (From);
      To_Index   : constant Node_Index := G.Nodes_By_Position.Element (To);
   begin
      if not Contains_Edge (G, From, To) then
         G.Edges.Append ((Steps => Steps, From => From_Index, To => To_Index));
         G.Nodes (From_Index).Edge_Ids.Append (G.Edges.Last_Index);
      end if;
      if Bidirectional and then not Contains_Edge (G, To, From) then
         G.Edges.Append ((Steps => Steps, From => To_Index, To => From_Index));
         G.Nodes (To_Index).Edge_Ids.Append (G.Edges.Last_Index);
      end if;
   end Add_Edge;

   type Tilemap_Path_State is record
      Position : Vec2;
      Steps    : Step_Count;
   end record;

   package Tilemap_Path_State_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Tilemap_Path_State);

   type Adjacent_Junction is record
      Position : Vec2;
      Steps    : Step_Count;
   end record;

   package Adjacent_Junction_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Adjacent_Junction);

   function Find_Adjacent_Junctions
     (M               : Char_Matrix;
      Start           : Vec2;
      Known_Junctions : Vec2_Sets.Set)
      return Adjacent_Junction_Vectors.Vector
   is
      Explore : Tilemap_Path_State_Vectors.Vector;
      Seen    : Vec2_Sets.Set;
      Current : Tilemap_Path_State;
      Result  : Adjacent_Junction_Vectors.Vector;
   begin
      Explore.Append ((Position => Start, Steps => 0));

      while Explore.Length > 0 loop
         Current := Explore.Last_Element;
         Explore.Delete_Last;

         Seen.Insert (Current.Position);

         if Current.Position /= Start and then Known_Junctions.Contains (Current.Position) then
            Result.Append ((Position => Current.Position, Steps => Current.Steps));
         else
            for D in Cardinal_Direction loop
               declare
                  Index : constant Vec2 := Current.Position + To_Vector (D);
                  Value : Character;
               begin
                  if Try_Element (M, Index, Value) and then not Seen.Contains (Index) then
                     case Value is
                        when '.' =>
                           Explore.Append ((Position => Index, Steps => Current.Steps + 1));

                        when '^' =>
                           if D = North then
                              Explore.Append ((Position => Index, Steps => Current.Steps + 1));
                           end if;

                        when 'v' =>
                           if D = South then
                              Explore.Append ((Position => Index, Steps => Current.Steps + 1));
                           end if;

                        when '<' =>
                           if D = West then
                              Explore.Append ((Position => Index, Steps => Current.Steps + 1));
                           end if;

                        when '>' =>
                           if D = East then
                              Explore.Append ((Position => Index, Steps => Current.Steps + 1));
                           end if;

                        when others =>
                           null;
                     end case;
                  end if;
               end;
            end loop;
         end if;

      end loop;

      return Result;

   end Find_Adjacent_Junctions;

   function Hash (I : Node_Index) return Hash_Type is (Hash_Type (I));

   type Path_State is record
      --  Node position
      Position : Node_Index;

      --  Nodes we've already visited. Each node N occupies 1 bit at 1 << N.
      Seen : Interfaces.Unsigned_64;

      --  Total steps taken.
      Steps : Step_Count;
   end record;

   package Path_State_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Path_State);

   function Find_Longest_Path (G : Graph; Start, Goal : Vec2) return Step_Count is
      Explore : Path_State_Vectors.Vector;
      Current : Path_State;

      Best_Steps : Step_Count := 0;

   begin
      Explore.Append (Path_State'(Position => Get_Node_Index (G, Start), Steps => 0, Seen => 0));

      while Explore.Length > 0 loop
         Current := Explore.Last_Element;
         Explore.Delete_Last;

         Current.Seen := Current.Seen or Interfaces.Shift_Left (1, Natural (Current.Position));

         if G.Nodes (Current.Position).Position = Goal then
            Best_Steps := Step_Count'Max (Best_Steps, Current.Steps);
         else
            for E of G.Nodes (Current.Position).Edge_Ids loop
               declare
                  Neighbor : constant Node_Index := G.Edges (E).To;
               begin
                  if (Current.Seen and Interfaces.Shift_Left (1, Natural (Neighbor))) = 0 then
                     Explore.Append
                       ((Position => Neighbor,
                         Seen     => Current.Seen,
                        Steps     => Current.Steps + G.Edges (E).Steps));
                  end if;
               end;
            end loop;
         end if;
      end loop;

      return Best_Steps;
   end Find_Longest_Path;

   function Construct_Graph (M : Char_Matrix; Start, Goal : Vec2; Slopes : Boolean) return Graph is
      Js : Vec2_Sets.Set := Find_All_Junctions (M);
      G  : Graph;
   begin

      --  We can treat the start and end as junctions for simplicity
      Js.Insert (Start);
      Js.Insert (Goal);

      for J of Js loop
         Add_Node (G, J);
      end loop;

      for J of Js loop
         for K of Find_Adjacent_Junctions (M, J, Js) loop
            Add_Edge (G, J, K.Position, K.Steps, Bidirectional => not Slopes);
         end loop;
      end loop;

      return G;

   end Construct_Graph;

   --  locals
   Map     : constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));
   P_Start : constant Vec2        := (1, 2);
   P_Goal  : constant Vec2        := (Rows (Map), Cols (Map) - 1);

begin

   Solution
     (Integer
        (Find_Longest_Path
           (Construct_Graph (Map, P_Start, P_Goal, Slopes => True), P_Start, P_Goal)));

   Solution
     (Integer
        (Find_Longest_Path
           (Construct_Graph (Map, P_Start, P_Goal, Slopes => False), P_Start, P_Goal)));

end Day23;
