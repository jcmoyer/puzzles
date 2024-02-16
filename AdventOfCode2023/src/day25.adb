with Advent;         use Advent;
with Advent.IO;      use Advent.IO;
with Advent.Strings; use Advent.Strings;
with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

procedure Day25 is

   subtype Node_Name is String (1 .. 3);

   --  Note these are zero-based for performance reasons.
   type Node_Index is new Natural;

   package Node_Name_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Node_Name,
      Element_Type    => Node_Index,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Weight_Type is new Natural;

   type Adjacency_Matrix_Data is array (Node_Index range <>, Node_Index range <>) of Weight_Type;

   --  We force zero-based indexing here for performance reasons.
   type Adjacency_Matrix (Last_Index : Node_Index) is record
      Data : Adjacency_Matrix_Data (0 .. Last_Index, 0 .. Last_Index);
   end record;

   type Adjacency_Matrix_Ptr is access all Adjacency_Matrix;

   procedure Unchecked_Free_Matrix is new Ada.Unchecked_Deallocation
     (Adjacency_Matrix, Adjacency_Matrix_Ptr);

   --  Operations that return rows/columns from the matrix return this type.
   type Weight_Array is array (Node_Index range <>) of Weight_Type;

   --  Using a package here so Graph can be a controlled type.
   package Graphs is
      type Graph is new Ada.Finalization.Controlled with private;

      procedure Adjust (G : in out Graph);
      procedure Finalize (G : in out Graph);

      procedure Reserve (G : in out Graph; New_Capacity : Integer);
      function Copy (G : Graph) return Graph;

      function First_Index (G : Graph) return Node_Index;
      pragma Inline (First_Index);

      function Last_Index (G : Graph) return Node_Index;
      pragma Inline (Last_Index);

      function Length (G : Graph) return Integer;
      pragma Inline (Length);

      procedure Set_Weight (G : Graph; U, V : Node_Index; W : Weight_Type);
      pragma Inline (Set_Weight);

      function Get_Weight (G : Graph; U, V : Node_Index) return Weight_Type;
      pragma Inline (Get_Weight);

      No_Space : exception;

      function Include (G : in out Graph; Name : Node_Name) return Node_Index;
      function Contains_Edge (G : Graph; From, To : Node_Name) return Boolean;

      --  Automatically creates nodes for `From` and `To` if they don't exist.
      procedure Add_Edge (G : in out Graph; From, To : Node_Name);
      procedure Delete_Edge (G : in out Graph; From, To : Node_Index);
      procedure Delete_Edge (G : in out Graph; From, To : Node_Name);

      --  Merges vertex V into U. Edges between U and V will be deleted.
      procedure Merge (G : in out Graph; U, V : Node_Index);

   private
      type Graph is new Ada.Finalization.Controlled with record
         Nodes         : Adjacency_Matrix_Ptr := null;
         Length        : Integer              := 0;
         Capacity      : Integer              := 0;
         Nodes_By_Name : Node_Name_Maps.Map;
      end record;
   end Graphs;

   package body Graphs is
      procedure Adjust (G : in out Graph) is
         New_Nodes : constant Adjacency_Matrix_Ptr := new Adjacency_Matrix (G.Nodes.Last_Index);
      begin
         New_Nodes.all := G.Nodes.all;
         G.Nodes       := New_Nodes;
      end Adjust;

      procedure Finalize (G : in out Graph) is
      begin
         Unchecked_Free_Matrix (G.Nodes);
      end Finalize;

      procedure Reserve (G : in out Graph; New_Capacity : Integer) is
      begin
         G.Nodes      := new Adjacency_Matrix (Node_Index (New_Capacity - 1));
         G.Nodes.Data := (others => (others => 0));
         G.Capacity   := New_Capacity;
      end Reserve;

      function Copy (G : Graph) return Graph is
         Result : Graph;
      begin
         Reserve (Result, G.Capacity);
         Result.Nodes.all     := G.Nodes.all;
         Result.Nodes_By_Name := G.Nodes_By_Name.Copy;
         Result.Length        := G.Length;
         return Result;
      end Copy;

      function First_Index (G : Graph) return Node_Index is
      begin
         return 0;
      end First_Index;

      function Last_Index (G : Graph) return Node_Index is
      begin
         return Node_Index (G.Length - 1);
      end Last_Index;

      function Length (G : Graph) return Integer is
      begin
         return G.Length;
      end Length;

      procedure Set_Weight (G : Graph; U, V : Node_Index; W : Weight_Type) is
      begin
         G.Nodes.Data (U, V) := W;
      end Set_Weight;

      function Get_Weight (G : Graph; U, V : Node_Index) return Weight_Type is
      begin
         return G.Nodes.Data (U, V);
      end Get_Weight;

      function Include (G : in out Graph; Name : Node_Name) return Node_Index is
         use type Node_Name_Maps.Cursor;
         Cursor : constant Node_Name_Maps.Cursor := G.Nodes_By_Name.Find (Name);
      begin
         if Cursor = Node_Name_Maps.No_Element then
            if G.Length = G.Capacity then
               raise No_Space;
            end if;

            declare
               New_Index : constant Node_Index := Node_Index (G.Length);
            begin
               G.Length := G.Length + 1;
               G.Nodes_By_Name.Insert (Name, New_Index);
               return New_Index;
            end;
         else
            return Node_Name_Maps.Element (Cursor);
         end if;
      end Include;

      function Contains_Edge (G : Graph; From, To : Node_Name) return Boolean is
      begin
         return G.Nodes.Data (G.Nodes_By_Name.Element (From), G.Nodes_By_Name (To)) /= 0;
      end Contains_Edge;

      --  Automatically creates nodes for `From` and `To` if they don't exist.
      procedure Add_Edge (G : in out Graph; From, To : Node_Name) is
         From_Index : constant Node_Index := Include (G, From);
         To_Index   : constant Node_Index := Include (G, To);
      begin
         G.Nodes.Data (From_Index, To_Index) := 1;
         G.Nodes.Data (To_Index, From_Index) := 1;
      end Add_Edge;

      procedure Delete_Edge (G : in out Graph; From, To : Node_Index) is
      begin
         G.Nodes.Data (From, To) := 0;
         G.Nodes.Data (To, From) := 0;
      end Delete_Edge;

      procedure Delete_Edge (G : in out Graph; From, To : Node_Name) is
         From_Index : constant Node_Index := G.Nodes_By_Name (From);
         To_Index   : constant Node_Index := G.Nodes_By_Name (To);
      begin
         Delete_Edge (G, From_Index, To_Index);
      end Delete_Edge;

      procedure Merge (G : in out Graph; U, V : Node_Index) is
      begin
         for I in 0 .. G.Last_Index loop
            declare
               W : constant Weight_Type := G.Get_Weight (U, I) + G.Get_Weight (V, I);
            begin
               --  U's row and column should be element-wise identical, so we
               --  write W down both.
               G.Set_Weight (U, I, W);
               G.Set_Weight (I, U, W);
               --  Zero out V's row and column.
               G.Set_Weight (V, I, 0);
               G.Set_Weight (I, V, 0);
            end;
         end loop;
         --  Eliminate the edge that was between U and V.
         G.Set_Weight (U, U, 0);
      end Merge;
   end Graphs;

   subtype Graph is Graphs.Graph;

   type Node_Set is array (Node_Index range <>) of Boolean;

   type Node_Set_Ptr is access all Node_Set;

   function Count (S : Node_Set) return Integer is
      Result : Integer := 0;
   begin
      for I in S'Range loop
         if S (I) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Count;

   procedure Union (Left : in out Node_Set; Right : Node_Set) is
   begin
      Left := Left or Right;
   end Union;

   type Min_Cut (Last_Index : Node_Index) is record
      W        : Weight_Type;
      Subgraph : Node_Set (0 .. Last_Index);
   end record;

   type Phase_Result is record
      W    : Weight_Type;
      S, T : Node_Index;
   end record;

   function Extract_Row (G : Graph; I : Node_Index) return Weight_Array is
      Result : Weight_Array (0 .. G.Last_Index);
   begin
      for J in 0 .. G.Last_Index loop
         Result (J) := G.Get_Weight (I, J);
      end loop;
      return Result;
   end Extract_Row;

   function Extract_Row (G : Graph; I : Node_Index; Mask : Node_Set) return Weight_Array is
      Result : Weight_Array (0 .. G.Last_Index);
   begin
      for J in 0 .. G.Last_Index loop
         if Mask (J) then
            Result (J) := G.Get_Weight (I, J);
         else
            Result (J) := 0;
         end if;
      end loop;
      return Result;
   end Extract_Row;

   function Max_Element_Index (W : Weight_Array; Mask : Node_Set) return Node_Index is
      Max_W : Weight_Type := Weight_Type'First;
      Max_I : Node_Index;
   begin
      for I in W'Range loop
         if Mask (I) and then W (I) > Max_W then
            Max_I := I;
            Max_W := W (I);
         end if;
      end loop;
      return Max_I;
   end Max_Element_Index;

   function "+" (A, B : Weight_Array) return Weight_Array is
      Result : Weight_Array (A'Range);
   begin
      for I in A'Range loop
         Result (I) := A (I) + B (I);
      end loop;
      return Result;
   end "+";

   function Stoer_Wagner_Phase (G : in out Graph; Phase : Integer) return Phase_Result is
      W    : Weight_Array                 := Extract_Row (G, 0);
      S, T : Node_Index                   := 0;
      Mask : Node_Set (0 .. G.Last_Index) := (others => True);
   begin
      for Iteration in 0 .. (G.Length - Phase - 1) loop
         Mask (T) := False;
         S        := T;
         T        := Max_Element_Index (W, Mask);
         W        := W + Extract_Row (G, T, Mask);
      end loop;

      G.Merge (S, T);

      return (W => W (T), S => S, T => T);
   end Stoer_Wagner_Phase;

   function Stoer_Wagner (G0 : Graph) return Min_Cut is
      G          : Graph        := G0.Copy;
      Best_Phase : Phase_Result := (W => Weight_Type'Last, others => <>);

      --  Silly stuff to avoid allocation, but really this probably should be
      --  done with a smart pointer or something.
      subtype Node_Set_Type is Node_Set (0 .. G.Last_Index);
      type Node_Sets_Type is array (Node_Index range 0 .. G.Last_Index) of Node_Set_Type;

      Subgraphs : Node_Sets_Type := (others => (others => False));
   begin
      for I in 0 .. G.Last_Index loop
         Subgraphs (I) (I) := True;
      end loop;

      for Phase in 1 .. Integer (G.Length - 1) loop
         declare
            This_Phase : constant Phase_Result := Stoer_Wagner_Phase (G, Phase);
         begin
            if This_Phase.W < Best_Phase.W then
               Best_Phase := This_Phase;
            end if;

            Union (Subgraphs (This_Phase.S), Subgraphs (This_Phase.T));
         end;
      end loop;

      return (Last_Index => G.Last_Index, W => Best_Phase.W, Subgraph => Subgraphs (Best_Phase.T));
   end Stoer_Wagner;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   G     : Graph;

begin

   G.Reserve (2_000);

   for Line of Lines loop
      declare
         Names : String_Array := Split_Any (Line, " :", Keep_Empty => False);
      begin
         for I in Names.First_Index + 1 .. Names.Last_Index loop
            G.Add_Edge (Names (0), Names (I));
         end loop;
      end;
   end loop;

   declare
      Cut : Min_Cut := Stoer_Wagner (G);
   begin
      Solution (Count (Cut.Subgraph) * (G.Length - Count (Cut.Subgraph)));
   end;

end Day25;
