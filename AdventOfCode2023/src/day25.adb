with Advent;         use Advent;
with Advent.Parsers.Integers;
with Advent.Vector_Math;
with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Numerics.Long_Long_Real_Arrays;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings.Hash;

procedure Day25 is

   subtype Node_Name is String (1 .. 3);

   type Node_Index is new Positive;

   package Node_Index_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Node_Index);

   package Node_Index_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Node_Index);

   type Edge_Index is new Positive;

   package Edge_Index_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Edge_Index);

   type Node is record
      Name  : Node_Name;
      Edges : Edge_Index_Vectors.Vector;
   end record;

   procedure Delete_Edge (N : in out Node; E : Edge_Index) is

   begin
      N.Edges.Delete (N.Edges.Find_Index (E));
   end Delete_Edge;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type => Node_Index, Element_Type => Node);

   package Node_Name_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Node_Name,
      Element_Type    => Node_Index,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   --  All edges are bidirectional and de-duplicated as they are inserted into the graph.
   type Edge is record
      From : Node_Index;
      To   : Node_Index;
   end record;

   function Other (E : Edge; Index : Node_Index) return Node_Index is
   begin
      if E.From = Index then
         return E.To;
      else
         return E.From;
      end if;
   end Other;

   subtype Extended_Edge_Index is Edge_Index'Base range Edge_Index'First - 1 .. Edge_Index'Last;

   No_Edge : constant := Extended_Edge_Index'First;

   package Edge_Vectors is new Ada.Containers.Vectors
     (Index_Type => Edge_Index, Element_Type => Edge);

   type Graph is record
      Nodes         : Node_Vectors.Vector;
      Nodes_By_Name : Node_Name_Maps.Map;
      Edges         : Edge_Vectors.Vector;
   end record;

   function Include (G : in out Graph; Name : Node_Name) return Node_Index is
      use type Node_Name_Maps.Cursor;
      Cursor : Node_Name_Maps.Cursor := G.Nodes_By_Name.Find (Name);
   begin
      if Cursor = Node_Name_Maps.No_Element then
         G.Nodes.Append ((Name => Name, Edges => <>));
         G.Nodes_By_Name.Insert (Name, G.Nodes.Last_Index);
         return G.Nodes.Last_Index;
      else
         return Node_Name_Maps.Element (Cursor);
      end if;
   end Include;

   function Find_Edge (G : Graph; From, To : Node_Name) return Extended_Edge_Index is
      E : Edge;
   begin
      for I in G.Edges.First_Index .. G.Edges.Last_Index loop
         E := G.Edges (I);

         if E.From = G.Nodes_By_Name (From) and then E.To = G.Nodes_By_Name (To) then
            return I;
         end if;

         --  Since edges are bidirectional the fields could be reversed.
         if E.To = G.Nodes_By_Name (From) and then E.From = G.Nodes_By_Name (To) then
            return I;
         end if;
      end loop;

      return No_Edge;
   end Find_Edge;

   function Contains_Edge (G : Graph; From, To : Node_Name) return Boolean is
   begin
      return Find_Edge (G, From, To) /= No_Edge;
   end Contains_Edge;

   --  Automatically creates nodes for `From` and `To` if they don't exist.
   procedure Add_Edge (G : in out Graph; From, To : Node_Name) is
      From_Index : constant Node_Index := Include (G, From);
      To_Index   : constant Node_Index := Include (G, To);
   begin
      if not Contains_Edge (G, From, To) then
         G.Edges.Append ((From => From_Index, To => To_Index));
         G.Nodes (From_Index).Edges.Append (G.Edges.Last_Index);
         G.Nodes (To_Index).Edges.Append (G.Edges.Last_Index);
      end if;
   end Add_Edge;

   procedure Delete_Edge (G : in out Graph; From, To : Node_Name) is
      Index      : Extended_Edge_Index := Find_Edge (G, From, To);
      From_Index : constant Node_Index := G.Nodes_By_Name (From);
      To_Index   : constant Node_Index := G.Nodes_By_Name (To);
   begin
      if Index /= No_Edge then
         --  We don't delete from G.Edges since that would invalidate indices stored in nodes.
         Delete_Edge (G.Nodes (From_Index), Index);
         Delete_Edge (G.Nodes (To_Index), Index);
      end if;
   end Delete_Edge;

   function Count_Reachable (G : Graph; Start : Node_Name) return Integer is
      Result  : Integer := 0;
      Visited : Node_Index_Sets.Set;

      Explore : Node_Index_Vectors.Vector;
      Current : Node_Index;

   begin

      Explore.Append (G.Nodes_By_Name.Element (Start));

      while Explore.Length > 0 loop
         Current := Explore.Last_Element;
         Explore.Delete_Last;

         if not Visited.Contains (Current) then
            Visited.Insert (Current);
            Result := Result + 1;
            for E of G.Nodes (Current).Edges loop
               Explore.Append (Other (G.Edges (E), Current));
            end loop;
         end if;
      end loop;

      return Result;

   end Count_Reachable;

   Lines         : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   G             : Graph;
   Before, After : Integer;

begin

   for Line of Lines loop
      declare
         Nodes : String_Array := Split_Any (Line, " :", Keep_Empty => False);
      begin
         for I in Nodes.First_Index + 1 .. Nodes.Last_Index loop
            Add_Edge (G, Nodes (0), Nodes (I));
         end loop;
      end;
   end loop;

   Before := Count_Reachable (G, G.Nodes (1).Name);

   Delete_Edge (G, "mzb", "fjn");
   Delete_Edge (G, "jlt", "sjr");
   Delete_Edge (G, "zqg", "mhb");

   After := Count_Reachable (G, G.Nodes (1).Name);

   Solution ((Before - After) * After);

end Day25;
