# aoc2021
## Day 1
- I zip the list together with a lagged version of the list. I toss out all tuples where the lagged version is smaller. Haskell makes this work, because you can take a zip between two lists of unequal lengths. The zip is done when the end of the shorter list is reached.
## Day 2
- I keep track of the state as a tuple and update the tuple at each step according to the current instruction.
## Day 3
- I represent each bit-string as a list of 1s and -1s. I take their sum to find the most frequent bit.
## Day 4
- First time using arrays in haskell. Many off by one errors. I play through all numbers and keep track of the first and last winner that I encounter.
## Day 5
- I map each segment to a set of points and take the intersection between these sets.
## Day 6
- I modeled the growth of one squid and one juvenile that start with 0 offset. Then, I truncated days to get other offsets. There were many off-by-one errors that I had to fix.
- I later learned that the correct approach was to simply keep track of _counts_ of squids for each unique offset.
## Day 7
- I just used brute force to check all possible points.
## Day 8
- The key realization here is to see that each set of segments can be uniquely identified using the number of segments in the set and the intersection of their set with known segments. So, for example, the digit one corresponds with a set with two members. The digit two corresponds with a set that has 5 members, is not the digit three, and contains the `c` segment. The digit nine corresponds with the union of the sets for the digit four and three and so on.
## Day 9
- Each lowpoint sits in a basin that is bordered by 9's and the edge of the map. So, I run a BFS starting from each lowpoint. 
## Day 10
- I use a stack to keep track of which open brackets still needed to be closed. To get the closing brackets needed to complete a string, just look at everything that's left on the stack.
## Day 11
- I kept track of the board state in a map and count the flashed positions at each step.
## Day 12
- DFS is enough to solve this one. For each node, keep track of the path that brought you there.
## Day 13
- For each fold, I find the distance of each point to the fold, and then find the final, absolute position of each folded point. I think I would have been in trouble if any of the folded points ended beyond the edge of the paper, i.e., if a point at (0,3) gets folded around (0,1) and ends at (-1,0). But this never seemed to happen.
## Day 14
- I keep track of the occurrence of character pairs in a map. For every character pair, I apply the production rule, to obtain a new map of character pair counts.
## Day 15
- I worked a long time with the assumption that the ship could only move down and to the right. It turned out that the instructions did _not_ guarantee this, so my DP solution had to be replaced with a Dijkstra's.
## Day 16
- This one was straightforward with the parsec parser combinator library. I've been told that `try x <|> y` is bad for performance, but in this case the input was small enough for it to be fine.
## Day 17
- For part 1, the key realization is to see that the x and y trajectories are completely independent from each other. So I just need to find a y-velocity that doesn't result in the missile overshooting the target on its way down. The other key observation is that the missile will have a velocity of equal magnitude and opposite direction when it reaches position 0 on its way back down. Well, almost equal velocity. There's an off-by-one case to consider:
- The y velocities and positions for the missile will look like 
```
[y0, y0-1, y0-2      ,...,  0   , -1  , -2    ,... -(y0+1)]
[0,  y0  , y0+(y0-1) ,...,  ymax, ymax, ymax-1,... 0      ]
```
- So the missile reaches 0 again with a velocity of -(y0+1). If we don't want the missile to overshoot the lower ybound. we need to set -(y0+1)=(lower     ybound)
- For part 2, to find every valid starting velocity, we upper-bound our y search range with the velocity found in part 1. We lower bound our y search with its negative. Finding the bounds for x is easy: it needs to be bigger than 0 and less than the velocity that would cause the missile to immediately overshoot. For each initial x and y velocity, we check T time-steps of the trajectory. How big should T be? This is determined by the rise and fall time found in part 1, since after that T, all y positions would be outside the target range.
## Day 18
- Doing the split operation was simple enough using recursion. To do the explode operation, I first recurse until I find the exploded node, and then as I return upwards through the recursion, I check whether the sibling branches need to be updated. So, for example, suppose I'm returning from recursing through the left and right branch. The left branch has exploded and indicated that it hasn't yet found its right-affectee. That means I need to find the leftmost neighbor of the _right_ branch and modify it. If I'm able to successfully do this, then I glue the exploded left branch and the modified right branch together and return them, along with a flag indicating that the right-affectee has been found. 
## Day 19
- To check whether two scanners, x and y, match, I hold scanner x fixed and then consider all 24 rotations of scanner y. Next, I need to find all possible displacements between the scanner x and the rotated scanner y. The key realization is to see that I only need to consider displacements that map at least one x beacon to one rotated y beacon. This cuts down the number of comparisons from 1000^3 to ~40^2. I keep a map of matching scanners, as well as their rotations and displacements, and follow back-pointers starting from scanner 0 to obtain the position of all beacons relative to scanner 0.
- I had also wanted to write a filter based on the distances between beacons. So two scanners can only match if they contain the same constellation of beacons. To check this, I find the inter-beacon distances and check whether two scanners contain the same distance structures. I stopped trying to do this once I realized that I would have to write a verification program anyways to weed out false positives and find the rotation and displacement. And once I saw that this verification code could be used for reasonably fast discovery, I just ran with it. 
## Day 20
- The key realization is that even though the field is infinite, the majority of the field will all have the same bit value. Although, this bit value may oscillate with every step. There are two pieces of state to keep track of: the pixels which have been affected by the seed image and the value of the rest of the field. The first group, we keep in a dictionary, the second value, we store in a bit. To update the first group, we find the position of all bits that might possibly be affected and update them with the cellular automata rules. For the second "filler bit" value, we either look at the first or last value in the rule-set, depending on the value of the filler bit.
## Day 21
- For part 2, I use DP on the game state, which consists of (player scores, player positions, whose move it currently is). The memoization stores, for each state, how many wins and losses are possible from that point on for player 1. In one base case, for example, there is 1 possible win and 0 losses from the position where player 1 has greater than 21 points, player 2 has less than 21 points, and it's player 2's move
## Day 22
- Instead of keeping track of each cuboid as a set of points, I only store the bounds for each cuboid. When a new cuboid is toggled, I find the intersections with the existing cuboids. Any existing cuboids that intersects with the toggled cuboid are then split into composite cuboids, and only the intersection is toggled. I split the cuboids into perfectly rectangular pieces, i.e., there are no jagged pieces left over. There could be a concern that the cuboids may be fragmented so much that storage explodes, but in practice, it runs fine. And still much better than keeping track of point sets.
## Day 23
- Attempt 1: Track each snail's state using an FSA. In the first state, a snail can only stay in its original room, in the second state, it can only move into the hallway or to its destination, etc. I keep track of hallway states, which consist of the state of each snail and its location. For each hallway state, I find the valid successor hallway states and use Dijkstra's to find the path to the final hallway state.
- Attempt 2: I noted that some snails would not move to their final destination room, even if the path was clear. This is always the optimal move, so I tried to build that constraint in. It was taking too long to debug, and my search was making alright progress (would finish in under 30 minutes), so I decided to leave it.
- Takeaways: write a pretty print function sooner than later for debugging purposes.
## Day 24
- First attempt: brute force that would have finished in 11 days
- Second attempt: build an AST for the full function and try to reduce it. I only used a very sparse set of simplification rules, and the expression still had many terms.
- Third attempt: Apply the function to a few inputs, and then try to determine whether the partially-applied function is satisfiable under any assignment of the remaining inputs. To do that, start at the root of the expression, and recursively check whether the left and right branch could possibly be a subset of [positive, zero, negative]. The deduction rules I used were too coarse, and it ended up that almost every assignment resulted in an expression that was possibly [positive, zero].
- Fourth attempt: Building on the above idea -- apply the function to a few inputs, and then apply the function to the rest of the inputs, keeping track of the upper and lower bounds for each variable state. So for example, once an input has been loaded into a variable, then that variable is bounded in [1,9]. If that variable is multiplied by 2, then it is bounded in [2, 18], and so on. For a partial assignment, if the final bounds do not contain 0, then the partially applied function cannot be satisfied, and no further continuations need be checked. If the final bounds _do_ contain 0, then further continuations do need to be checked. I recursively check all assignments, filtering out the unsatisfiable ones. This runs in less than a minute.
