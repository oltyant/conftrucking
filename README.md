# Conference Tracking - Problem description

You are planning a big programming conference and have received many proposals which have passed the initial screen process but you're having trouble fitting them into the time constraints of the day - there are so many possibilities! So you write a program to do it for you.

* The conference has multiple tracks each of which has a morning and afternoon session.
* Each session contains multiple talks.
* Morning sessions begin at 9am and must finish by 12 noon, for lunch.
* Afternoon sessions begin at 1pm and must finish in time for the networking event.
* The networking event can start no earlier than 4:00 and no later than 5:00.
* No talk title has numbers in it.
* All talk lengths are either in minutes (not hours) or lightning (5 minutes).
* Presenters will be very punctual; there needs to be no gap between sessions.

Note that depending on how you choose to complete this problem, your solution may give a different ordering or combination of talks into tracks. This is acceptable; you don't need to exactly duplicate the sample output given here.

## Test Input

[Short test input:](test-input.txt)
```
Writing Fast Tests Against Enterprise Rails 60min
Overdoing it in Python 45min
Lua for the Masses 30min
Ruby Errors from Mismatched Gem Versions 45min
Common Ruby Errors 45min
Rails for Python Developers lightning
Communicating Over Distance 60min
Accounting-Driven Development 45min
Woah 30min
Sit Down and Write 30min
Pair Programming vs Noise 45min
Rails Magic 60min
Ruby on Rails: Why We Should Move On 60min
Clojure Ate Scala (on my project) 45min
Programming in the Boondocks of Seattle 30min
Ruby vs. Clojure for Back-End Development 30min
Ruby on Rails Legacy App Maintenance 60min
A World Without HackerNews 30min
User Interface CSS in Rails Apps 30min
```

## Sample outputs

```
Track 1:
09:00AM Writing Fast Tests Against Enterprise Rails 60min
10:00AM Communicating Over Distance 60min
11:00AM Rails Magic 60min
12:00PM Lunch
01:00PM Ruby on Rails: Why We Should Move On 60min
02:00PM Common Ruby Errors 45min
02:45PM Accounting-Driven Development 45min
03:30PM Pair Programming vs Noise 45min
04:15PM User Interface CSS in Rails Apps 30min
04:45PM Rails for Python Developers lightning
04:50PM Networking Event
```
```
Track 2:
09:00AM Ruby on Rails Legacy App Maintenance 60min
10:00AM Overdoing it in Python 45min
10:45AM Ruby Errors from Mismatched Gem Versions 45min
11:30AM Lua for the Masses 30min
12:00PM Lunch
01:00PM Clojure Ate Scala (on my project) 45min
01:45PM Woah 30min
02:15PM Sit Down and Write 30min
02:45PM Programming in the Boondocks of Seattle 30min
03:15PM Ruby vs. Clojure for Back-End Development 30min
03:45PM A World Without HackerNews 30min
04:15PM Networking Event
```


## First thoughts

My first thought was that the solution is similar to the change making problem where typically we have distinct coins and we would like to change money with the least number of coins. I learnt Data Structures and Algorithm at the University so I revised my materials learnt there.

### Greedy algorithm

The Greedy method is when we would like to achieve a goal with the presumption that for finding a global minimum or maximum it is enough to find the local minimums or maximums respectively. More specifically: If you feel that making the local optimal choices will yield the global optimal then you don't have to solve all the sub-problem at all. Greedy solutions don't usually give you the correct answer.

For instance:
```
Value of banknote to change: 6
Available coins (unlimited number we have from each): 1,3,4

Greedy solution start with 4 and then choose 1 twice so it thinks: 4+1+1 is the best answer however the truth is 3+3
```

For the Conference Tracking problem we can choose first the event which is the longest in time (not longer than the reamining free time) and then do this until we fill up the morning session. With the remaining elements we can do the same for the afternoon session.

My concerns regarding to this approach are:
* Only find one solution at a time (if it finds at all) so we need to do backsteps and cover a coin or a subset of coins in order to use again the Greedy method and find another solution
* For many solutions we need to change the algorithm and play it with the removal of elements so we need to count with the non repeatable permutations problem which requires N! operations, where N=count of events.


### Dynamic Programming algorithm

A dynamic programming algorithm examines the previously solved subproblems and combine their solutions to give the best solution for the given problem. If we can find the optimal substructure for the problem (literally this means we can solve it with recursion) then we can easily reach the proper result(s). For the coin change problem described above:
```
Value of banknote to change: 6
Available coins (unlimited number): 1,3,4
T[1]=1 or 3 or 4
T[2]=T[1] + (1 or 3 or 4) | (1 or 3 or 4)
...
T[n]=T[n - 1] + (1 or 3 or 4) | T[n - 2] + (1 or 3 or 4)

Which will give the 3+3 optimal solution
```
For the Conference Tracking problem here is the proof of optimal substructure:
* For one event it is either part of the solution (one session) or not
* N - 1 events followed by E[N] event or not where N=count of events, E is a list where the Nth element is the Nth Event we can choose

According to the fact that Haskell is purely functional language and it has persistent, recursive data collections I turned to solve the problem with an implementation of **Convolution Probabilistic Tree**. The root start with the initial case when there is no Event which we use. Then we insert into the tree the first event which should be either part of the solution (left child of root) or not (right child of root). With each newly introduced Event we need to insert two times more element that we have on the deepest level in worst case, which means that the space complexity could be really high: O(2<sup>n</sup>).

The space complexity can be reduced by the following inclusion of *dead-end cases*:
* No need to insert any new Node after a Node that contains a solution
* No need to insert any new Node after a Node that already reach overtime (so not fulfills the criteria)

Here is a tree for the following case:
```
Event list (length):
* Event 1 (30 mins)
* Event 2 (10 mins)
* Event 3 (20 mins)
* Event 4 (10 mins)
Target Session length: 40 mins
```
After inserting all to the event tree we get:
![probabilistic_convolution_tree](https://cloud.githubusercontent.com/assets/3776068/19024234/61a22084-88ff-11e6-8bb5-e869d4333014.jpg)

### Improvements

* As the space complexity (thus the memory usage) could be high we can do the insertion of Events step by step finding the first result only:
```
findFirst :: Range -> EventTree -> [Event] -> Result

where

type Range  = (minimum length of session, maximum length of session) :: (Int, Int)
type Event  = (title, length) :: (String, Int)
type Result = (Tree after the first solution found, events for solution) :: (EventTree, [Event])
```
* After the first N events we can check the solution list with:
```
findNStep :: Range -> EventTree -> [Event] -> Int -> Result
```
* Anf last bit not least we can get all the solutions:
```
findAll :: Range -> EventTree -> [Event] -> Result
```
See [EventTree.hs](src/Data/EventTree.hs) for the implementations.


### Problem with the Probabilistic Convolution Tree solution:

* It can be clearly seen that we have a lot of redundancy, so after 4 events inserted the tree contains the No event ([]) Node 5 times (!!), the first Event ([1]) 4 times, the second Event 3 times, the third event and the [2,3] twice. This is called **Overlapping subproblems** in Dynamic programming and usually solved by reducing the space complexity of an array or a vector of elements which - most of the time - unforunately means the elimination of recursion.
* Owing to the redundancy the insertion of the subsequent steps requires more and more time on the right side which results inbalance in the tree and therefore results difficulty when parallel computation comes into picture as different subtrees requires different work resources.

### Current runtime and memory statistics

* with running ```./Main ../test-input.txt 19 +RTS -s```
```
     123,780,200 bytes allocated in the heap
      23,771,296 bytes copied during GC
       2,688,928 bytes maximum residency (11 sample(s))
          53,968 bytes maximum slop
               8 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0       228 colls,     0 par    0.02s    0.02s     0.0001s    0.0007s
  Gen  1        11 colls,     0 par    0.02s    0.02s     0.0014s    0.0033s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.12s  (  0.16s elapsed)
  GC      time    0.04s  (  0.04s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    0.16s  (  0.20s elapsed)

  %GC     time      23.1%  (19.2% elapsed)

  Alloc rate    1,021,138,354 bytes per MUT second

  Productivity  76.7% of total user, 63.6% of total elapsed
```
* with running ```./Main ../test-input-big.txt 190 +RTS -s```
```
  42,423,838,592 bytes allocated in the heap
  32,638,120,368 bytes copied during GC
     649,109,736 bytes maximum residency (131 sample(s))
       7,129,408 bytes maximum slop
            1837 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0     81291 colls,     0 par   14.50s   14.51s     0.0002s    0.0008s
  Gen  1       131 colls,     0 par   17.05s   17.06s     0.1302s    0.6633s

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time   10.53s  ( 11.83s elapsed)
  GC      time   31.55s  ( 31.56s elapsed)
  EXIT    time    0.08s  (  0.08s elapsed)
  Total   time   42.16s  ( 43.47s elapsed)

  %GC     time      74.8%  (72.6% elapsed)

  Alloc rate    4,028,626,629 bytes per MUT second

  Productivity  25.2% of total user, 24.4% of total elapsed
```

__Improvements and further considerations__:
* Eliminate **Overlapping subproblems** with making a list of solutions - this should mean O(N<sup>2</sup>) time complexity -  and creating a simple result of the event lists. We can specify the solutions can be found in the following links:
https://wiki.haskell.org/Dynamic_programming_example and http://jelv.is/blog/Lazy-Dynamic-Programming/
* As the event length is not surely unique therefore it is a good idea to do a classification (groupBy is your friend) based on length and after that based on these classes the dynamic programming solution is easier to apply
* The performance of the EventTree solution can be improved by parallel processing. As we start to build up the tree with two independent subtrees, one of which contains the first event (E1) and the other which does not therefore we can build up 
them through two independent but parallel processes. We can do this simplification on a lower level which requires 2<sup>n</sup> cores on the nth level. Here is a code snippet on the first tree level parallelization:
```
import Control.Parallel
import EventTree

EventTree = left `par` right `pseq` (aggregate left right)
    where
         left                            = findFirst (180, 180) (Node [("Event 1", 30)] 30 EmptyNode EmptyNode) [("Event 2", 10), ...]
         right                           = findFirst (180, 180) (Node [] 0 EmptyNode EmptyNode) [("Event 2", 10), ...]
         aggregate (lt, lres) (rt, rres) = (Node [] 0 lt rt, lres ++ rres)  
       
```
