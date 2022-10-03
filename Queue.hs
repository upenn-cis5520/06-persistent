{-
---
fulltitle: "In class exercise: Purely Functional Queues"
date: October 10, 2022
---

Today's technical challenge is to implement a persistent *queue* data structure.
-}

module Queue where

{-
You should use QuickCheck to test this module. If you want to use additional
library operations to complete this exercise, you may import them here. (For
example, our solution uses at least one function from the `Data.Maybe`
library.)

-}

import Test.QuickCheck

{-
1. Define an *interface* for a purely functional Queue (FIFO).  Your interface
  must (at least) define some data structure, called `Q`, include a
  representation of an empty queue (queue), a way to add an element to the end
  of the queue (enq) and a way to remove the element at the beginning of the
  queue (deq), if the queue is nonempty. The queue must be polymorphic over
  the type of elements that it stores. You may include additional operations
  in your interface, if you wish.
-}

-- replace these definitions with something more appropriate
data Q = Q

empty :: Q
enq :: Q
deq :: Q
{-
2. Now define some properties that your queue should satisfy. (Note: you may want
to add additional operations to your queue interface to help with stating
these properties.)
-}

{-
3. Implement your interface. Hint: the simplest implementation uses a single list. However, you can define a more efficient version using two lists.
-}

empty = undefined

enq = undefined

deq = undefined

{-
4. Make an `Arbitrary` instance, including definitions for both `arbitrary` and `shrink`
-}

{-
5. Run your tests.
-}
