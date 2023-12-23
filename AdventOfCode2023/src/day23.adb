with Advent;                     use Advent;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Ada.Command_Line;
with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

procedure Day23 is

   --  Input observations:
   --
   --  paths are all one tile wide, so it should be fast to explore each branch
   --  grid is 141x141 tiles = 19881 cells; 9292 are '.' 10471 are '#'

   --  There are no '^' characters in the input
   --
   --  A dot adjacent to an arrow is always surrounded by multiple (3+?) arrows
   --
   --  A path always leads to exactly one junction
   --
   --  Passing through a junction is the only way to get to a different
   --  junction, there are no gaps in the walls

   type Step_Count is new Natural;

   package Vec2_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Vec2, Hash => Hash, Equivalent_Elements => "=");

   function In_Bounds (Map : Char_Matrix; Indices : Vec2) return Boolean is
     (Indices (0) in Map'Range (1) and then Indices (1) in Map'Range (2));

   function Element (Map : Char_Matrix; Indices : Vec2) return Character is
     (Map (Indices (0), Indices (1)));

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
               for D in Direction loop
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
      Steps          : Integer;
      Node_A, Node_B : Node_Index;
   end record;

   function Other_Node (E : Edge; From : Node_Index) return Node_Index is
   begin
      if From = E.Node_A then
         return E.Node_B;
      else
         return E.Node_A;
      end if;
   end Other_Node;

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

   function Contains_Edge (G : Graph; First, Second : Vec2) return Boolean is
   begin
      for E of G.Edges loop
         if G.Nodes (E.Node_A).Position = First and then G.Nodes (E.Node_B).Position = Second then
            return True;
         end if;
         --  The order may be swapped
         if G.Nodes (E.Node_A).Position = Second and then G.Nodes (E.Node_B).Position = First then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Edge;

   --  Adds an edge to the graph between two nodes. If an edge already exists
   --  between the nodes, this procedure does nothing.
   procedure Add_Edge (G : in out Graph; First, Second : Vec2; Steps : Integer) with
     Pre => G.Nodes_By_Position.Contains (First) and then G.Nodes_By_Position.Contains (Second)
   is
      First_Index  : constant Node_Index := G.Nodes_By_Position.Element (First);
      Second_Index : constant Node_Index := G.Nodes_By_Position.Element (Second);
   begin
      if not Contains_Edge (G, First, Second) then
         G.Edges.Append ((Steps => Steps, Node_A => First_Index, Node_B => Second_Index));
         G.Nodes (First_Index).Edge_Ids.Append (G.Edges.Last_Index);
         G.Nodes (Second_Index).Edge_Ids.Append (G.Edges.Last_Index);
      end if;
   end Add_Edge;

   type Tilemap_Path_State is record
      Position : Vec2;
      Steps    : Integer;
   end record;

   package Tilemap_Path_State_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Tilemap_Path_State);

   type Adjacent_Junction is record
      Position : Vec2;
      Steps    : Integer;
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
            for D in Direction loop
               declare
                  Index : constant Vec2 := Current.Position + To_Vector (D);
                  Value : Character;
               begin
                  if Try_Element (M, Index, Value) then
                     if Value /= '#' and then not Seen.Contains (Index) then
                        Explore.Append ((Position => Index, Steps => Current.Steps + 1));
                     end if;
                  end if;
               end;
            end loop;
         end if;

      end loop;

      return Result;

   end Find_Adjacent_Junctions;

   function Hash (I : Node_Index) return Hash_Type is (Hash_Type (I));

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Node_Index, Hash => Hash, Equivalent_Elements => "=");

   type Path_State is record
      --  Node position
      Position : Node_Index;

      --  Edges we've taken
      Seen : Node_Sets.Set;

      --  Could get this from seen I guess
      Steps : Step_Count;
   end record;

   package Path_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Path_State);

   function Get_Priority (P : Path_State) return Integer is (Integer (P.Steps));

   package Path_Queues is new Ada.Containers.Unbounded_Priority_Queues
     (Queue_Interfaces => Path_Queue_Interfaces,
      Queue_Priority   => Integer,
      Before           => ">",
      Get_Priority     => Get_Priority);

   function Find_Longest_Path (G : Graph; First, Last : Vec2) return Step_Count is
      Explore : Path_Queues.Queue;
      Current : Path_State;

      Best_Steps : Step_Count := 0;

   begin
      Explore.Enqueue
        (Path_State'
           (Position => Get_Node_Index (G, First), Steps => 0, Seen => Node_Sets.Empty_Set));

      while Explore.Current_Use > 0 loop
         Explore.Dequeue (Current);

         Current.Seen.Insert (Current.Position);

         if G.Nodes (Current.Position).Position = Last then
            Best_Steps := Step_Count'Max (Best_Steps, Current.Steps);
            Put_Line ("Goal in " & Current.Steps'Image & " (best " & Best_Steps'Image & ")");
         else
            for E of G.Nodes (Current.Position).Edge_Ids loop
               declare
                  Other : constant Node_Index := Other_Node (G.Edges (E), Current.Position);
               begin
                  if not Current.Seen.Contains (Other) then
                     Explore.Enqueue
                       ((Position => Other,
                         Seen     => Current.Seen.Copy,
                         Steps    => Current.Steps + Step_Count (G.Edges (E).Steps)));
                  end if;
               end;
            end loop;
         end if;

      end loop;

      return Best_Steps;
   end Find_Longest_Path;

   --  locals
   Map : constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));

   Js : constant Vec2_Sets.Set := Find_All_Junctions (Map);

   P_Start : constant Vec2 := (1, 2);
   P_End   : constant Vec2 := (Rows (Map), Cols (Map) - 1);

   G : Graph;

begin

   for J of Js loop
      Add_Node (G, J);
   end loop;

   Add_Node (G, P_Start);
   for K of Find_Adjacent_Junctions (Map, P_Start, Js) loop
      Add_Edge (G, P_Start, K.Position, K.Steps);
   end loop;

   Add_Node (G, P_End);
   for K of Find_Adjacent_Junctions (Map, P_End, Js) loop
      Add_Edge (G, P_End, K.Position, K.Steps);
   end loop;

   for J of Js loop
      for K of Find_Adjacent_Junctions (Map, J, Js) loop
         Add_Edge (G, J, K.Position, K.Steps);
      end loop;
   end loop;

   Solution (Integer (Find_Longest_Path (G, P_Start, P_End)));

end Day23;
