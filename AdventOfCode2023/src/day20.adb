with Advent;         use Advent;
with Ada.Command_Line;
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Day20 is
   package Module_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 16);

   subtype Module_Name is Module_Names.Bounded_String;

   function Hash is new Ada.Strings.Bounded.Hash (Module_Names);

   type Node_Kind is (None, Flip_Flop, Conjunction, Broadcaster, Untyped);

   type Pulse is (High, Low);

   type Module_Id is new Positive;

   function Hash (Id : Module_Id) return Hash_Type is (Hash_Type (Id));

   type Memory_Map_Id is new Positive;

   --  Messages are queued and sent/received by modules.
   type Pulse_Message is record
      Source      : Module_Id;
      Destination : Module_Id;
      Value       : Pulse;
   end record;

   type Module (Kind : Node_Kind := None) is record
      Name : Module_Name;
      case Kind is
         when Flip_Flop =>
            Flip_Flop_On : Boolean;
         when Conjunction =>
            Memory_Map : Memory_Map_Id;
         when others =>
            null;
      end case;
   end record;

   type Module_Ptr is access all Module;

   --  Contiguous storage for graph nodes. Any reference to a specific Module
   --  will be done through a Module_Id instead of pointers.
   package Module_Vectors is new Ada.Containers.Vectors
     (Index_Type => Module_Id, Element_Type => Module);

   --  A sequence of Module_Id.
   package Module_Id_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Module_Id);

   subtype Module_Id_Vector is Module_Id_Vectors.Vector;

   package Module_Name_Id_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Module_Name,
      Element_Type    => Module_Id,
      Hash            => Hash,
      Equivalent_Keys => Module_Names."=");

   package Module_Id_Id_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Module_Id,
      Element_Type    => Module_Id_Vector,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Module_Id_Vectors."=");

   --  Storage for a single conjunction's input memory.
   package Memory_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Module_Id, Element_Type => Pulse, Hash => Hash, Equivalent_Keys => "=");

   subtype Memory_Map is Memory_Maps.Map;

   --  Returns true when all the previous inputs in a memory map are high.
   function All_High (Map : Memory_Map) return Boolean is
   begin
      for Val of Map loop
         if Val = Low then
            return False;
         end if;
      end loop;
      return True;
   end All_High;

   --  Storage for memory maps.
   package Memory_Map_Vectors is new Ada.Containers.Vectors
     (Index_Type => Memory_Map_Id, Element_Type => Memory_Map, "=" => Memory_Maps."=");

   --  Pulse message queue for sending/receiving messages.
   package Pulse_Queues is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Pulse_Message);

   --  Determines the type of module given the left hand side of a module
   --  definition.
   function Parse_Module_Kind (S : String) return Node_Kind is
   begin
      if S (S'First) = '%' then
         return Flip_Flop;
      elsif S (S'First) = '&' then
         return Conjunction;
      else
         return Broadcaster;
      end if;
   end Parse_Module_Kind;

   --  Strips the module kind from the left hand side of a module definition
   --  and returns its undecorated (i.e. without & or %) name.
   function Parse_Module_Name (S : String) return String is
   begin
      if Parse_Module_Kind (S) = Broadcaster then
         return S;
      else
         return S (S'First + 1 .. S'Last);
      end if;
   end Parse_Module_Name;

   --  Stores the total number of low and high pulses.
   type Pulse_Counts_Array is array (Pulse) of Long_Long_Integer;

   --  Callback type for hooking module output.
   type Output_Hook is not null access procedure (Message : Pulse_Message);

   package Hook_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Module_Id, Element_Type => Output_Hook, Hash => Hash, Equivalent_Keys => "=");

   type Module_Graph is record
      --  Storage for modules
      Modules : Module_Vectors.Vector;

      --  Maps string to module id
      Name_To_Id : Module_Name_Id_Maps.Map;

      --  Maps a module id to the module ids it sends to
      Outputs : Module_Id_Id_Maps.Map;

      --  Maps a module id to the module ids that send to it
      Inputs : Module_Id_Id_Maps.Map;

      --  Storage for conjunction input memory
      Memories : Memory_Map_Vectors.Vector;

      --  Queue messages so they are processed in the correct order
      Messages : Pulse_Queues.Vector;

      --  Running count of lows and highs for part 1
      Pulse_Counts : Pulse_Counts_Array := (others => 0);

      --  Number of presses for part 2
      Presses : Long_Long_Integer := 0;

      --  Map of module id to callback that will be invoked when that module
      --  outputs something
      Output_Hooks : Hook_Maps.Map;
   end record;

   --  Allocates one memory map and returns its id.
   function Alloc_Memory_Map (G : in out Module_Graph) return Memory_Map_Id is
   begin
      G.Memories.Append (Memory_Maps.Empty_Map);
      return G.Memories.Last_Index;
   end Alloc_Memory_Map;

   --  If a module named Name exists, returns its id. Otherwise storage for the
   --  module is allocated and returns an id for that storage. The allocated
   --  module will have the None kind and needs to be initialized.
   function Get_Or_Alloc_Module (G : in out Module_Graph; Name : String) return Module_Id is
      use type Module_Name_Id_Maps.Cursor;

      Existing : constant Module_Name_Id_Maps.Cursor :=
        G.Name_To_Id.Find (Module_Names.To_Bounded_String (Name));

   begin
      if Existing /= Module_Name_Id_Maps.No_Element then
         return Module_Name_Id_Maps.Element (Existing);
      else
         declare
            New_Module_Id : constant Module_Id := Module_Id (G.Modules.Length + 1);
         begin
            G.Name_To_Id.Insert (Module_Names.To_Bounded_String (Name), New_Module_Id);
            G.Modules.Append ((Kind => Untyped, Name => Module_Names.Null_Bounded_String));
            G.Outputs.Insert (New_Module_Id, Module_Id_Vectors.Empty_Vector);
            G.Inputs.Insert (New_Module_Id, Module_Id_Vectors.Empty_Vector);
            return New_Module_Id;
         end;
      end if;
   end Get_Or_Alloc_Module;

   No_Module : exception;

   --  Returns the Module_Id for a module named Name. Raises No_Module if the
   --  module doesn't exist.
   function Get_Module_By_Name (G : Module_Graph; Name : String) return Module_Id is
      use type Module_Name_Id_Maps.Cursor;

      Existing : constant Module_Name_Id_Maps.Cursor :=
        G.Name_To_Id.Find (Module_Names.To_Bounded_String (Name));

   begin
      if Existing = Module_Name_Id_Maps.No_Element then
         raise No_Module;
      end if;

      return Module_Name_Id_Maps.Element (Existing);
   end Get_Module_By_Name;

   --  Returns a list of modules that are inputs to Id
   function Get_Module_Parents (G : Module_Graph; Id : Module_Id) return Module_Id_Vector is
   begin
      return G.Inputs.Constant_Reference (Id).Copy;
   end Get_Module_Parents;

   --  Links From and To so that messages sent to From will be processed and
   --  new ones sent to To.
   procedure Add_Output (G : in out Module_Graph; From, To : Module_Id) with
     --  These should have been previously inserted via calls to Get_Or_Alloc_Module
     Pre => G.Outputs.Contains (From) and then G.Inputs.Contains (To)
   is
   begin
      G.Outputs.Reference (From).Append (To);
      G.Inputs.Reference (To).Append (From);
   end Add_Output;

   Unknown_Module_Kind : exception;

   --  Loads a single module definition from the input.
   procedure Load_Module (G : in out Module_Graph; S : String) is
      Left_Right   : constant String_Array := Split (S, " -> ");
      Output_Names : constant String_Array := Split (Left_Right (1), ", ");

      This_Name : constant String    := Parse_Module_Name (Left_Right (0));
      This_Id   : constant Module_Id := Get_Or_Alloc_Module (G, This_Name);
   begin

      for Output_Name of Output_Names loop
         Add_Output (G, This_Id, Get_Or_Alloc_Module (G, Output_Name));
      end loop;

      case Parse_Module_Kind (Left_Right (0)) is
         when Flip_Flop =>
            G.Modules.Replace_Element
              (This_Id,
              (Kind          => Flip_Flop,
                Name         => Module_Names.To_Bounded_String (This_Name),
                Flip_Flop_On => False));

         when Conjunction =>
            G.Modules.Replace_Element
              (This_Id,
              (Kind        => Conjunction,
                Name       => Module_Names.To_Bounded_String (This_Name),
                Memory_Map => Alloc_Memory_Map (G)));

         when Broadcaster =>
            G.Modules.Replace_Element
              (This_Id, (Kind => Broadcaster, Name => Module_Names.To_Bounded_String (This_Name)));

         when others =>
            raise Unknown_Module_Kind;
      end case;
   end Load_Module;

   --  Sets up memory maps for all conjunctions. All modules should be loaded
   --  before calling this.
   procedure Init_Memory_Maps (G : in out Module_Graph) is
   begin
      for I in G.Modules.First_Index .. G.Modules.Last_Index loop
         if G.Modules (I).Kind = Conjunction then
            for Inp of G.Inputs (I) loop
               G.Memories (G.Modules (I).Memory_Map).Include (Inp, Low);
            end loop;
         end if;
      end loop;
   end Init_Memory_Maps;

   --  Loads all modules from an input and returns the complete graph.
   function Load_Modules (Lines : String_Array) return Module_Graph is
      G : Module_Graph;
   begin
      for Line of Lines loop
         Load_Module (G, Line);
      end loop;
      Init_Memory_Maps (G);
      return G;
   end Load_Modules;

   --  Enqueues a single message. All messages should be sent through this
   --  procedure since it handles hook processing.
   procedure Enqueue (G : in out Module_Graph; Message : Pulse_Message) is
      use type Hook_Maps.Cursor;
      Hook_Cursor : constant Hook_Maps.Cursor := G.Output_Hooks.Find (Message.Source);
   begin
      G.Messages.Append (Message);
      if Hook_Cursor /= Hook_Maps.No_Element then
         Hook_Maps.Element (Hook_Cursor) (Message);
      end if;
   end Enqueue;

   --  Sends a message from Sender to all of its linked outputs.
   procedure Broadcast_From (G : in out Module_Graph; Sender : Module_Id; Value : Pulse) is
   begin
      for Output of G.Outputs (Sender) loop
         Enqueue (G, (Source => Sender, Destination => Output, Value => Value));
      end loop;
   end Broadcast_From;

   --  Delivers one message to the module To and processes it.
   procedure Deliver (G : in out Module_Graph; To : Module_Id; Message : Pulse_Message) is
      Processor : Module_Ptr := G.Modules.Reference (To).Element;
   begin
      case Processor.Kind is
         when Broadcaster =>
            Broadcast_From (G, To, Message.Value);

         when Flip_Flop =>
            if Message.Value = Low then
               Processor.Flip_Flop_On := not Processor.Flip_Flop_On;
               Broadcast_From (G, To, (if Processor.Flip_Flop_On then High else Low));
            end if;

         when Conjunction =>
            declare
               Memory : constant Memory_Map_Vectors.Reference_Type :=
                 G.Memories.Reference (Processor.Memory_Map);
            begin
               Memory.Include (Message.Source, Message.Value);
               Broadcast_From (G, To, (if All_High (Memory) then Low else High));
            end;

         when Untyped =>
            --  these intentionally do nothing; see second example and part 2
            null;

         when others =>
            raise Program_Error;

      end case;
   end Deliver;

   --  Processes all messages currently in the queue and any messages generated
   --  during the processing of those messages.
   procedure Simulate (G : in out Module_Graph) is
      Message : Pulse_Message;
   begin
      while G.Messages.Length > 0 loop
         Message := G.Messages.First_Element;
         G.Messages.Delete_First;

         --  record highs/lows for part 1
         G.Pulse_Counts (Message.Value) := G.Pulse_Counts (Message.Value) + 1;

         Deliver (G, Message.Destination, Message);
      end loop;
   end Simulate;

   --  Press the button once, broadcasting a low message from the module called
   --  broadcaster.
   procedure Press_Button (G : in out Module_Graph) is
   begin
      G.Presses := G.Presses + 1;
      Enqueue
        (G,
         (Source      => Module_Id'Last,
          Destination => Get_Module_By_Name (G, "broadcaster"),
          Value       => Low));
      Simulate (G);
   end Press_Button;

   --  Finds the modules that must all emit high messages simultaneously to
   --  signal rx.
   function Find_Terminal_Modules (G : Module_Graph) return Module_Id_Vector is
      Rx           : constant Module_Id        := Get_Module_By_Name (G, "rx");
      Rx_Conj      : constant Module_Id_Vector := Get_Module_Parents (G, Rx);
      Counter_Conj : constant Module_Id_Vector := Get_Module_Parents (G, Rx_Conj.First_Element);
   begin
      return Counter_Conj;
   end Find_Terminal_Modules;

   --  Resets all the intermediate state of a graph.
   procedure Reset (G : in out Module_Graph) is
   begin
      G.Presses      := 0;
      G.Pulse_Counts := (others => 0);
      G.Output_Hooks.Clear;
      Init_Memory_Maps (G);
      for M of G.Modules loop
         if M.Kind = Flip_Flop then
            M.Flip_Flop_On := False;
         end if;
      end loop;
   end Reset;

   --  Holds the number of button presses that happened when a terminal module
   --  emitted high, one element for each module.
   package Long_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Long_Long_Integer);

   --  locals
   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));
   G     : Module_Graph          := Load_Modules (Lines);

   First_Highs : Long_Vectors.Vector;

   procedure Save_Hooked_Output (Message : Pulse_Message) is
   begin
      if Message.Value = High then
         First_Highs.Append (G.Presses);
      end if;
   end Save_Hooked_Output;

begin

   --  Part 1
   for I in 1 .. 1_000 loop
      Press_Button (G);
   end loop;
   Solution (G.Pulse_Counts (High) * G.Pulse_Counts (Low));

   --  Part 2
   Reset (G);
   declare
      --  Modules that we want to monitor for high pulses.
      Mods : constant Module_Id_Vector := Find_Terminal_Modules (G);
   begin
      for M of Mods loop
         G.Output_Hooks.Insert (M, Save_Hooked_Output'Access);
      end loop;

      loop
         exit when First_Highs.Length = Mods.Length;
         Press_Button (G);
      end loop;

      declare
         Product : Long_Long_Integer := 1;
      begin
         for Low of First_Highs loop
            Product := Product * Low;
         end loop;
         Solution (Product);
      end;
   end;

end Day20;
