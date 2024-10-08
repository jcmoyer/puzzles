with Advent.Containers.Priority_Queues;
with Advent.Testing; use Advent.Testing;

procedure Test_Priority_Queues is

   package Int_PQ is new Advent.Containers.Priority_Queues (Element_Type => Integer, "<" => "<");

   Test_Q : Int_PQ.Queue;
   X      : Integer;

begin

   Test_Q.Enqueue (5);
   Assert (Test_Q.Length = 1);
   Test_Q.Enqueue (3);
   Assert (Test_Q.Length = 2);
   Test_Q.Enqueue (1);
   Assert (Test_Q.Length = 5);

   Test_Q.Dequeue (X);
   Assert (X = 1);
   Assert (Test_Q.Length = 2);
   Test_Q.Dequeue (X);
   Assert (X = 3);
   Assert (Test_Q.Length = 1);
   Test_Q.Dequeue (X);
   Assert (X = 5);
   Assert (Test_Q.Length = 0);

end Test_Priority_Queues;
