---

title: Solving Shuffdle using IDA* in Haskell

description: An explanation of using the IDA* algorithm in Haskell to solve a
             Shuffdle puzzle, aka Caesar's 20-puzzle.

tags: algorithms, haskell

---

This post features an explanation of the IDA* algorithm in Haskell and how it
can be used to solve the [Shuffdle](https://www.shuffdle.com) puzzle, aka Caesar's 20-puzzle.

Following the original presentation of the IDA* algorithm by Korf in
*Depth-First Iterative-Deepening: An Optimal Admissible Tree Search* (1985),
IDA* is presented as a natural series of upgrades to the naive
depth-first-search algorithm until it is suitable to solving arbitrary sliding
puzzles in practical time.

# The Shuffdle puzzle

To set the stage for the puzzle we aim to solve, here's a brief explanation of
Shuffdle (todo: *embed a puzzle example board here*):

The word is LATCH

<!-- Embeded Puzzle {{{-->
<main>
    <style>
        .container {
            display: grid;
            grid-template-columns: 1fr 1fr 1fr 1fr 1fr;
            grid-gap: 5px;
            touch-action: none; /* Don't allow scrolling while touching the grid */
            user-select: none;
            -webkit-user-select: none;
            max-width: 25em;
            margin: auto;
        }

        .tile {
            border: solid var(--gray) 1px;
            border-radius: 2px;
            text-align: center;
            font-size: 2em;
            aspect-ratio: 1/1;
            cursor: pointer;

            /* Center letters in boxes */
            display: flex;
            justify-content: center;
            align-items: center;
        }

        .tile[active=true] {
            border: solid var(--black) 2px;
        }

        .tile[correct=true] {
            transition: border 250ms ease-in;
            border: solid green 2px;
        }

        .tile[hole=true] {
            color: transparent
        }
    </style>
    <div class="container" id="board-container"></div>
    <script defer>
        const the_board_id = ['V','V','X','K','D','N','G','T','S','V','P','X','L','S','F','Y','I','D','S','U','_','_','_','_','_']
        const container = document.getElementById("board-container")
        for (x of the_board_id) {
            const div = document.createElement("div")
            div.classList.add("tile")
            div.innerHTML = x
            container.appendChild(div)

            if (x == "_") {
                div.setAttribute("hole", true)
            }
        }

        const setActive = el => {
            [...tiles].map(t => t.setAttribute("active", false))
            el.setAttribute("active", true)
        }

        const solution = "LATCH"
        const tiles    = document.getElementsByClassName("tile")
        const size     = 5

        const previousLetter = l => l == "A" ? "Z" : String.fromCharCode(l.charCodeAt(0) - 1);
        const nextLetter = l => l == "Z" ? "A" : String.fromCharCode(l.charCodeAt(0) + 1);
        const chLetter = (l, dir) => {
            const c =
                { "ArrowUp"   : previousLetter,
                  "ArrowRight": nextLetter,
                  "ArrowDown" : nextLetter,
                  "ArrowLeft" : previousLetter,
                  "w": previousLetter,
                  "d": nextLetter,
                  "s": nextLetter,
                  "a": previousLetter,
                }[dir]
            return c(l)
        }
        const isTileCorrect = t => {
            const ix = Number(t.getAttribute("ix"))
            const col_ix = ix % size
            const row_ix = ix / size >> 0 // integer division
            const tval = t.innerHTML

            return row_ix == 4 && tval == solution[col_ix]
        }


        // Drag and drop / Touch events
        const validDropDirection = (orig, tgt) => {
            /* no need to check out of bounds since only possible for drag targets */
            if (orig + 1 == tgt)
                return "ArrowRight"
            else if (orig - 1 == tgt)
                return "ArrowLeft"
            else if (orig + 5 == tgt)
                return "ArrowDown"
            else if (orig - 5 == tgt)
                return "ArrowUp"
            else
                return null
        }
        [...document.querySelectorAll("#board-container .tile")].forEach(tile => {
            tile.draggable = true;
            tile.addEventListener("dragstart", ev => {
                ev.dataTransfer.effectAllowed = "move"
                ev.dataTransfer.setData("text/plain", ev.target.getAttribute("ix"))
                setActive(ev.target)
            });

            tile.addEventListener("dragover", ev => {
                ev.preventDefault();
            });

            tile.addEventListener("drop", ev => {
                const origin_ix = Number(ev.dataTransfer.getData("text/plain"));
                const target_ix = Number(ev.target.getAttribute("ix"));
                const dir = validDropDirection(origin_ix, target_ix)
                if (dir != null && ev.target.getAttribute("hole") == "true") {
                    ev.preventDefault();
                    ev.dataTransfer.dropEffect = "move"
                    move(document.querySelector("#board-container .tile[ix=\""+origin_ix+"\"]"), dir)
                }
            });

            /* Touch Events */
            var startX, startY

            tile.addEventListener("touchstart", ev => {
                setActive(ev.target)
                const touch = ev.changedTouches[0]
                startX = touch.pageX
                startY = touch.pageY
            });

            tile.addEventListener("touchend", ev => {
                const origin_ix = Number(ev.target.getAttribute("ix"));
                const touchAtEnd = ev.changedTouches[0]
                const vectorAngle = (x, y) =>
                  Math.acos(
                    x.reduce((acc, n, i) => acc + n * y[i], 0) /
                      (Math.hypot(...x) * Math.hypot(...y))
                  );

                const swipeVec = [touchAtEnd.pageX - startX, touchAtEnd.pageY - startY]
                const swipeAngle = vectorAngle(swipeVec, [1, 0])

                const pi = Math.PI
                const dir = 
                        swipeAngle < pi/4 ? "ArrowRight" :
                        swipeAngle > pi/4 && swipeAngle < 3*pi/4 ?
                            (swipeVec[1] < 0 ? "ArrowUp" :
                             swipeVec[1] > 0 ? "ArrowDown" :
                             null) :
                        swipeAngle > 3*pi/4 ? "ArrowLeft" :
                        null

                if (dir != null) {
                    ev.preventDefault();
                    move(document.querySelector("#board-container .tile[ix=\""+origin_ix+"\"]"), dir)
                }
            });
        });

        const move = (orig, dir) => {
            const ix = Number(orig.getAttribute("ix"))
            const col_ix = ix % size
            const tgt_ix =
              { "ArrowUp"   : ix - size >= 0 ? ix - size : null,
                "w"         : ix - size >= 0 ? ix - size : null,

                "ArrowRight": col_ix + 1 < size ? ix + 1 : null,
                "d"         : col_ix + 1 < size ? ix + 1 : null,

                "ArrowDown" : ix + size < size*size ? ix + size : null,
                "s"         : ix + size < size*size ? ix + size : null,

                "ArrowLeft" : col_ix - 1 >= 0 ? ix - 1 : null,
                "a"         : col_ix - 1 >= 0 ? ix - 1 : null,
              }[dir]

            if (tgt_ix == null) return;

            const tgt = tiles[tgt_ix]

            if (tgt.getAttribute("hole") == "true" && orig.getAttribute("hole") != "true") {
                tgt.innerHTML = chLetter(orig.innerHTML, dir)
                orig.innerHTML = "_"
                tgt.setAttribute("hole", false)
                orig.setAttribute("hole", true)
                setActive(tgt)

                orig.setAttribute("correct", false)
                if (isTileCorrect(tgt)) {
                    tgt.setAttribute("correct", true)
                }
            }
        }

        for (let i=0; i<tiles.length; i++) {
            const t = tiles[i]
            t.setAttribute("ix", i)
            t.addEventListener("click", e => setActive(e.target))
        }

    </script>
</main>
<!--}}}-->

## Coding Shuffdle in Haskell

... *omit some code in collapsible summary/details tag*

# Solving Shuffdle using graph algorithms

Our goal is to find some path (a list of moves) from the initial game board to a
goal state. There may be more than one goal state since, unlike sliding puzzles,
only the last row needs to be checked for the goal word. Moreover, this path
must be under 50 moves as per Shuffdle's rules, and we know that all Shuffdle
puzzles are solvable^[As one might've guessed, the initial board is simply
generated from the *goal state* by randomly taking moves until the last row is
empty.]

Let us come up with the simplest, lowest-effort, solution to find such a
path/solution -- without regard for time nor space:

Let the *problem space* of a given Shuffdle puzzle be all board states reachable
from the initial board. The *problem space* is easily generated by recursively
applying all valid moves to a given board, starting with the initial one. We can
use a `Tree` (i.e. a connected acyclic graph) to naturally represent the problem space:

* Every node's data is a board configuration
* All children nodes are board configurations reachable from that node's state

To find a solution, naively, we could simply traverse the problem space tree,
either breadth-first or depth-first, until we discover a goal state. The path to
reach that tree node is the solution.

## Problem space as lazy data

Haskell's laziness allows us to represent the entire problem space as a data
structure -- albeit one that will be constructed only as needed instead of right
away.

Given
```haskell
-- Definition from Data.Tree
data Tree a = Node a [Tree a]
```
... *laziness*

## Depth-first searching the problem space

Two basic ways to traverse the problem space are breadth-first search and
depth-first search. The representation of our problem space lends itself better
to a depth-first traversal since it matches a naive recursive traversal.

Solver-wise, a breadth-first search is guaranteed to find a solution with the
fewer number of moves since, starting from $d=0$, it searchs all boards reachable
within $d$ moves before proceeding to traverse the boards reachable with $d+1$
moves, until a solution is found. However, a breadth-first search requires all
nodes at a given depth *d* to be kept in memory to later expand into the next
depth $d+1$, when the traversal at depth $d$ is finished.

For a *node branching factor* *b*, where *b* in Shuffdle is the average amount
of boards reachable from a single configuration, breadth-first search has space
complexity $O(b^d)$. The initial board has only 5 reachable configurations, and
a board with two center rows with interleaved holes would have 18, so let's
ballpark the branching factor to 10. At depth 10 we'd need $10^10$ boards in
memory -- and most Shuffdle puzzles need at least 20 moves to be solved. At 4
bytes per board (a very compact representation), this means a wildly impractical
40GB of boards at depth=10. The worst-case time complexity is also $O(b^d)$,
since it will explore $b + b^2 + b^3 + ... + b^d$ boards.

Depth-first search avoids the memory limitation of breadth-first search, but, in
contrast, does not necessarily find the shortest path to a solution. Depth-first
search works by, starting from $d=0$, exploring one board at depth $d$, then
exploring another reachable board at depth $d+1$, then another at depth $d+2$, and
so on, until a depth cutoff is reached. In our case the cutoff could be the 50
moves limit. When the cutoff is reached, depth-first search backtracks to the
last board explored at the previous depth and tries a not-yet-explored board
reachable from that one. Therefore, we must keep in memory only the path of
explored nodes until depth $d$ -- i.e. the space complexity is $O(d)$. In the
worst case, all nodes must still be explored, so the time complexity is still
$O(b^d)$^[Our graph is a tree, so the branching factor $b$ is the same as the
so-called edge branching factor $e$. However, in general, a problem space graph
may have more than one edge branching to the same node. When there is the
difference, it is more correct to attribute to depth-first search a time
complexity of $O(e^d)$. If $e > b$, depth-first search may have a larger time
complexity than breadth-first search.].

![Fig 1. XKCD on BFS vs DFS](https://imgs.xkcd.com/comics/depth_and_breadth.png)

Let us implement a depth-first search (DFS) traversal of the problem space. DFS
is easier to implement given the recursive nature of the tree, and has a
tractable space complexity on our problem space.

Takes the solution word, the lazy tree representing the problem space, and
returns a solution if it can find one.
```haskell
dfs :: String -> Tree (Board, Move) -> Maybe [Move]
dfs sol = go 0 [] where
```

The implementation is carried out by the auxiliary function `go`, which
accumulates the depth and the path to the current board.

```haskell
  go d mvs (Node (b,mv) bs) 
    | d >= 50
    = Nothing
    | checkEasyWin sol b
    = Just (mv:mvs)
    | otherwise
    = case mapMaybe (go (d+1) (mv:mvs)) bs of
        []  -> Nothing
        x:_ -> Just x
```
By case analysis:

* If we've reached a solution, return it
* If we've reached the cut-off depth, then no solution was found on this branch.
* In all other cases, we recurse (depth first) on the boards reachable from the
    current board. `mapMaybe` will discard all branches that return `Nothing`.
    If there are no sub-branches that get us an answer, fail with `Nothing`,
    otherwise, return the first found.

Unfortunately, running this function on our problem space will not give us an
answer -- we're still exploring way too many nodes for it to terminate in
practical time -- and even if we did, it would most likely not be an optimal
solution.

## Iterative Deepening Depth-first Search

There is an algorithm which preserves the constant space complexity ($O(d)$)
characteristics of depth-first search, but which also guarantees an optimal
solution -- it's called *depth-first iterative deepening*.

Depth-first iterative deepening (DFID) is a pretty self explanatory name. Do a
depth-first search with depth $1$, then discard previous results and do a
depth-first search to depth $2$, then to depth $3$, and so on until a goal state
is found.

This algorithm guarantees the optimal solution will be found since we expand all
nodes at any depth before moving on to the next -- yet, at any given point,
we're still doing a depth-first search, thus using $O(d)$ space.

The disadvantage of DFID is performing the same computation multiple times
redundantly, always re-doing the work to get to layer $d$ when moving from the
iteration on depth $d$ to $d+1$. This may seem very innefficient, but
Korf presents an analysis showing that this wasted computation does not
affect the asymptotic growth of the runtime for exponential tree searches [^1].

> The intuitive reason is that almost all the work is done at the deepest level
> of the search.

[^1]: *Depth-First Iterative-Deepening: An Optimal Admissible Tree Search* (1985)

To implement DFID in Haskell we can re-use the above implementation of
depth-first search.

First, add a parameter to control the cut-off depth instead of
hardcoding it to `50`. I've also renamed the worker function from `go` to `dfs`:

```haskel
solve :: String -> Tree (Board, Move) -> Maybe [Move]
solve sol = ... where

  dfs cutoff d mvs (Node (b,mv) bs)
    | checkEasyWin sol b
    = Just (mv:mvs)
    | d >= cutoff
    = Nothing
    | otherwise
    = case mapMaybe (dfs cutoff (d+1) (mv:mvs)) bs of
        []  -> Nothing
        x:_ -> Just x
```

Second, call depth-first search (`dfs`) iteratively, with an increasing cut-off:

```haskell
solve :: String -> Tree (Board, Move) -> Maybe [Move]
solve sol init = dfid where

  dfid
    = case mapMaybe (\cutoff -> dfs cutoff 0 [] init) [1..] of
        [] -> Nothing
        (firstResult:_) -> Just firstResult

  dfs = ... -- as seen above
```

Unfortunately, DFID suffers the same drawback as depth-first search on arbitrary
graphs, namely that it must explore all possible paths to a given depth.
Even though it's more likely we'll find a solution now (and it would even be
optimal!), we're still doing way too much work for the solver to complete in
practical time.

By adding a `Debug.Trace.trace` to print the depth at which we're looking we can
see it reaches depth 6-7 fast enough, but then isn't able to look through all
the nodes at that depth to move on to the 8th layer. Layer 7 will already have
probably more than $10^7$ nodes -- 10 million nodes, which take too long to
process. Granted, we could improve the board representation to be much more
compact and make the win-condition check much faster, but let's instead continue
improving the algorithm for now.

# Solving Shuffdle with IDA*

Depth-first iterative-deepening will naively search through the entire problem
space using constant space $O(d)$ and, *if it terminates*, guarantees that an
optimal solution is found. Those are pretty good properties! The problem is that
on problem spaces as large as ours, it's quite unlikely that it will terminate
as there are simply too many nodes to go through.

The key observation is that we can do better than *naively* searching through
the problem space. By informing the algorithm with a *heuristic*, at any given
node, we can pick the board that is heuristically better out of the boards
reachable from that node, and search that one next. We'd call it a *best-first*
search.

Iterative Deepening A* (IDA\*) is the algorithm resulting from combining depth-first
iterative-deepening with the heuristic used by the A* graph algorithm.
IDA* will traverse *best-first* down the problem space until a certain
*cost* threshold is reached, rather than a *depth* threshold. The total *cost*
of any given node is computed by the heuristic function.
Successive iterations of IDA* correspond to increasing values of the total cost
of the path, rather than increasing depth.
For A*, the cost of any node $n$ factors in the following two values:

* The cost to reach that node, $g(n)$
* The estimated cost to reach the goal from that node, $h(n)$

The total cost is given by $f(n) = g(n) + h(n)$.  If the heuristic is
[*admissible*](https://en.wikipedia.org/wiki/Admissible_heuristic), i.e. if the
estimated cost to reach the goal is never an overestimation of the real cost,
IDA* will always find the optimal solution/cheapest path.

In Haskell, we can tweak our solver to pick the *best node* to explore first and
use a cost-based threshold.
We'll need every node in the lazily-computed tree of the problem space to be
annotated with its cost, but, for now, let's assume it is already there.
The first step is to make `dfs` use the node `cost` as the cutoff instead of
hardcoding it to the depth:

```haskell
type Cost = Int

-- Note that each Tree node now has a Cost.
solve :: String -> Tree (Board, Move, Cost) -> Maybe [Move]
solve sol init = ... where

  dfs cutoff mvs (Node (b,mv,cost) bs)
    | checkEasyWin sol b
    = Just (mv:mvs)
    | cost >= cutoff
    = Nothing
    | otherwise
    = case mapMaybe (dfs cutoff (mv:mvs)) bs of
        []  -> Nothing
        x:_ -> Just x
```

Not much has changed. We now match on the `cost` and compare it with the
`cutoff`. We've also gotten rid of the depth `d` argument.
Then, we update `dfid` to receive the initial board and list of increasing
cutoffs as an argument:

```haskell
solve sol init = ... where

  dfid problem cutoffs
    = case mapMaybe (\cutoff -> dfs cutoff [] problem) cutoffs of
        [] -> Nothing
        (firstResult:_) -> Just firstResult
```

Now that we no longer cutoff on the depth, but rather on the cost, an attentive
reader may notice that `dfid`, despite keeping the name, no longer behaves as
before. However, we could easily restore its previous behaviour by modifying the
problem space tree lazily and replacing the `Cost` of each node with the depth it is
at.
Lastly, we can write `idaStar` in terms of `dfid` simply by

1. Calling `dfid` on a cutoff list of increasing thresholds instead of `[1..]`
2. Sorting by cost the children of every node of the lazy problem space tree
   before passing it to `dfid`.

Since `dfs` will always explore the first of the children, sorting every node's
children by cost in `idaStar` guarantees the `dfs` will explore the best node
first. I find it quite amusing how we can tweak this infinite problem space tree
structure to change the semantics of the algorithm traversal:

```haskell
solve sol init = idaStar where

  idaStar =
    dfid (bestFirst init) [100,200..]
      where
        bestFirst (Node b bs) =
          Node b $
            map bestFirst $
              List.sortOn (\(Node (_,_,c) _ -> c)) bs
```

The `bestFirst` auxiliary function will lazily traverse the problem space and
sort every node's children by increasing cost, and `[100, 200...]` is an
infinite list of thresholds increasing in steps of 100.
Also worthy of note is that Haskell's `sortBy` function is lazy, which means
that if we only explore at the first element of the node's children, we will
only pay for sorting that one element, which can be done in $O(n)$ time as
opposed to $O(n*log n)$ for sorting the whole list. Since traversing the best
nodes first will lead to a solution faster, this means we'll likely avoid a lot
of sorting, because of laziness, for free.

Annotating the infinite problem space tree with the cost of each node amounts to
a simple traversal over every node and adding the `costToWin` of the board with
the cost of the path thus far:

```haskell
annotateCosts :: String -> Tree (Board, Move) -> Tree (Board, Move, Cost)
annotateCosts sol = go 0 where
  go pathCost (Node (b,m) ns) =
    let h = costToWin sol b
        g = pathCost
     in Node (b, m, g + h) (map (go (g+h)) ns)
```

The heuristic function `costToWin` determines the cost of reaching the goal from
that board. To solve Shuffdle, similarly to a common sliding puzzle, we will use
the sum of the Manhattan distance from the tiles that can be used to write the
goal word to their respective positions in the bottom row:

```haskell
```


