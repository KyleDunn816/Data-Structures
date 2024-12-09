
#lang dssl2

# HW4: Graph

import cons
import sbox_hash
import 'hw4-lib/dictionaries.rkt'


###
### `AssociationListAll`
###

class EntryAll[K, V]:
    let key: K
    let value: V

    def __init__(self, key: K, value: V):
        self.key = key
        self.value = value

    def get_key(self) -> K:
        self.key

    def get_value(self) -> V:
        self.value

    def set_value(self, value) -> NoneC:
        self.value = value

# `AssociationListAll` is `AssociationList` from the previous homework
# with the `get_all` method added.
class AssociationListAll[K, V] (DICT):
    let _head: Cons.ListC[EntryAll?[K, V]]

    def __init__(self):
        self._head = None

    def len(self) -> nat?:
        Cons.len(self._head)

    def mem?(self, key: K) -> bool?:
        not(self._find(key) is False)

    def get(self, key: K) -> V:
        let entry = self._find(key)
        error('get: no key ' + str(key)) if entry is False else entry.get_value()

    def put(self, key: K, value: V) -> NoneC:
        self.put_mem(key, value)
        return

    def del(self, key: K) -> NoneC:
        self.del_mem(key)
        return

    # See above.
    def __print__(self, print):
        print('#<object:AssociationListAll head=%p>', self._head)

    # The `entry` with `key` or `False` if there is no such.
    def _find(self, key: K) -> OrC(EntryAll?[K, V], False):
        Cons.ormap(lambda entry: entry if entry.get_key() == key else False,\
                self._head)

    # Does the same as `self.put`.
    # Returns whether the given key was mapped by the dictionary
    # before the call.
    def put_mem(self, key: K, value: V) -> bool?:
        let entry = self._find(key)
        if entry is False:
            self._head = cons(EntryAll[K, V](key, value), self._head)
            False
        else:
            entry.set_value(value)
            True

    # Does the same as `self.del`.
    # Returns whether the given key was mapped by the dictionary
    # before the call.
    def del_mem(self, key: K) -> bool?:
        let l = self._head
        if l is None:
            False
        else:
            if l.data.get_key() == key:
                # delete
                self._head = l.next
                True
            else:
                while True:
                    let next_l = l.next
                    if next_l is None:
                        return False
                    else:
                        if next_l.data.get_key() == key:
                            # delete
                            l.next = next_l.next
                            return True
                        else:
                            l = next_l

    # All entries. The returned entries are copies of the internal entries.
    def get_all(self) -> VecC[EntryAll?[K, V]]:
        let r = vec(self.len())
        let i = 0
        def add(entry: EntryAll?[K, V]) -> NoneC:
            r[i] = EntryAll[K, V](entry.get_key(), entry.get_value())
            i = i + 1
        Cons.foreach(add, self._head)
        assert i == self.len()
        r


###
### REPRESENTATION
###

# A Vertex is a natural number.
let Vertex? = nat?

# A VertexList is either
#  - None, or
#  - cons(v, vs), where v is a Vertex and vs is a VertexList
let VertexList? = Cons.ListC[Vertex?]

# A Weight is a real number. (It’s a number, but it’s neither infinite
# nor not-a-number.)
let Weight? = AndC(num?, NotC(OrC(inf, -inf, nan)))

# An OptWeight is either
# - a Weight, or
# - None
let OptWeight? = OrC(Weight?, NoneC)

# A WEdge is WEdge(Vertex, Vertex, Weight)
struct WEdge:
    let u: Vertex?
    let v: Vertex?
    let w: Weight?

# A WEdgeList is either
#  - None, or
#  - cons(w, ws), where w is a WEdge and ws is a WEdgeList
let WEdgeList? = Cons.ListC[WEdge?]

# A weighted, undirected graph ADT.
interface WUGRAPH:

    # Returns the number of vertices in the graph. (The vertices
    # are numbered 0, 1, ..., k - 1.)
    def len(self) -> nat?

    # Sets the weight of the edge between u and v to be w. Passing a
    # real number for w updates or adds the edge to have that weight,
    # whereas providing providing None for w removes the edge if
    # present. (In other words, this operation is idempotent.)
    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC

    # Gets the weight of the edge between u and v, or None if there
    # is no such edge.
    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?

    # Gets a list of all vertices adjacent to v. (The order of the
    # list is unspecified.)
    def get_adjacent(self, v: Vertex?) -> VertexList?

    # Gets a list of all edges in the graph, in an unspecified order.
    # This list only includes one direction for each edge. For
    # example, if there is an edge of weight 10 between vertices
    # 1 and 3, then exactly one of WEdge(1, 3, 10) or WEdge(3, 1, 10)
    # will be in the result list, but not both.
    def get_all_edges(self) -> WEdgeList?


# Graph represented by adjacency lists.
# Loops are allowed. (Here a loop is an edge going from a vertex
# to the same vertex.)
class WUGraph (WUGRAPH):
    # `_adj[u].mem?(v)` if and only if this graph contains an edge
    # from `u` to `v`. If it does, then `_adj[u].get(v)` is its weight.
    let _adj: VecC[AssociationListAll?[Vertex?, Weight?]]

    def __init__(self, size: nat?):
        self._adj = vec(size, lambda x: AssociationListAll[Vertex?, Weight?]())

    def len(self) -> nat?:
        self._adj.len()

    def _set_edge1(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC:
        let adj_u = self._adj[u]
        if w is None:
            adj_u.del(v)
        else:
            adj_u.put(v, w)

    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC:
        self._set_edge1(u, v, w)
        if u != v:
            self._set_edge1(v, u, w)

    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?:
        let adj_u = self._adj[u]
        adj_u.get(v) if adj_u.mem?(v) else None

    def get_adjacent(self, v: Vertex?) -> VertexList?:
        let r = None
        for entry in self._adj[v].get_all():
            r = cons(entry.get_key(), r)
        r

    def get_all_edges(self) -> WEdgeList?:
        let r = None
        let u = 0
        for adj in self._adj:
            for entry in adj.get_all():
                let v = entry.get_key()
                if u <= v:
                    r = cons(WEdge(u, v, entry.get_value()), r)
            u = u + 1
        r


###
### List helpers
###

# To test methods that return lists with elements in an unspecified
# order, you can use these functions for sorting. Sorting these lists
# will put their elements in a predictable order, order which you can
# use when writing down the expected result part of your tests.

# sort_vertices : ListOf[Vertex] -> ListOf[Vertex]
# Sorts a list of numbers.
def sort_vertices(lst: Cons.list?) -> Cons.list?:
    def vertex_lt?(u, v): return u < v
    return Cons.sort[Vertex?](vertex_lt?, lst)

# sort_edges : ListOf[WEdge] -> ListOf[WEdge]
# Sorts a list of weighted edges, lexicographically
# ASSUMPTION: There's no need to compare weights because
# the same edge can’t appear with different weights.
def sort_edges(lst: Cons.list?) -> Cons.list?:
    def edge_lt?(e1, e2):
        return e1.u < e2.u or (e1.u == e2.u and e1.v < e2.v)
    return Cons.sort[WEdge?](edge_lt?, lst)


###
### `WUGraph` test
###

test 'WUGraph':
    let g = WUGraph(3)
    assert g.len() == 3
    assert Cons.len(g.get_all_edges()) == 0
    assert g.get_edge(0, 0) is None
    g.set_edge(0, 0, 10)
    assert g.get_edge(0, 0) == 10
    assert Cons.len(g.get_all_edges()) == 1
    assert g.get_edge(2, 1) is None
    assert g.get_edge(1, 2) is None
    g.set_edge(2, 1, 21)
    assert g.get_edge(2, 1) == 21
    assert g.get_edge(1, 2) == 21
    g.set_edge(2, 0, 20)
    g.set_edge(2, 1, 21)
    assert g.get_edge(2, 1) == 21
    assert g.get_edge(1, 2) == 21
    assert sort_edges(g.get_all_edges())\
        == Cons.from_vec([WEdge(0, 0, 10), WEdge(0, 2, 20), WEdge(1, 2, 21)])
    assert sort_vertices(g.get_adjacent(0)) == Cons.from_vec([0, 2])
    assert sort_vertices(g.get_adjacent(1)) == Cons.from_vec([2])
    assert sort_vertices(g.get_adjacent(2)) == Cons.from_vec([0, 1])
    g.set_edge(0, 0, None)
    assert sort_edges(g.get_all_edges()) == Cons.from_vec([WEdge(0, 2, 20), WEdge(1, 2, 21)])
    g.set_edge(2, 0, 200)
    assert sort_edges(g.get_all_edges()) == Cons.from_vec([WEdge(0, 2, 200), WEdge(1, 2, 21)])
    g.set_edge(2, 1, None)
    assert sort_edges(g.get_all_edges()) == Cons.from_vec([WEdge(0, 2, 200)])
    g.set_edge(2, 0, None)
    assert sort_edges(g.get_all_edges()) == None
    

###
### BUILDING GRAPHS
###

def example_graph() -> WUGraph?:
    let result = WUGraph(6) # 6-vertex graph from the assignment
    result.set_edge(0, 1, 12)
    result.set_edge(1, 2, 31)
    result.set_edge(2, 4, -2)
    result.set_edge(2, 5, 7)
    result.set_edge(1, 3, 56)
    result.set_edge(3, 4, 9)
    result.set_edge(3, 5, 1)
    result

struct CityMap:
    let graph
    let city_name_to_node_id
    let node_id_to_city_name

def my_neck_of_the_woods():
    let hash_f = make_sbox_hash()

    let c2n = HashTable(6, hash_f)
    c2n.put('Los Angeles', 0)
    c2n.put('San Antonio', 1)
    c2n.put('Houston', 2)
    c2n.put('Bellaire', 3)
    c2n.put('Baton Rouge', 4)
    c2n.put('Huntsville', 5)

    let n2c = HashTable(6, hash_f)
    n2c.put(0, 'Los Angeles')
    n2c.put(1, 'San Antonio')
    n2c.put(2, 'Houston')
    n2c.put(3, 'Bellaire')
    n2c.put(4, 'Baton Rouge')
    n2c.put(5, 'Huntsville')

    let g = WUGraph(6)
    # Edge weight is distance in miles.
    g.set_edge(0, 1, 1353)
    g.set_edge(1, 2, 197)
    g.set_edge(1, 3, 195)
    g.set_edge(2, 3, 9)
    g.set_edge(2, 4, 269)
    g.set_edge(4, 5, 499)
    g.set_edge(0, 5, 2009)

    CityMap(g, c2n, n2c)

test 'my_neck_of_the_woods':
    let city_map = my_neck_of_the_woods()
    assert city_map.graph.get_edge(\
            city_map.city_name_to_node_id.get('Houston'),\
            city_map.city_name_to_node_id.get('Bellaire'))\
            == 9
    assert city_map.graph.get_edge(\
            city_map.city_name_to_node_id.get('Los Angeles'),\
            city_map.city_name_to_node_id.get('Baton Rouge'))\
            is None
    assert city_map.graph.get_edge(\
            city_map.city_name_to_node_id.get('Houston'),\
            city_map.city_name_to_node_id.get('San Antonio'))\
            < 500

    let vs = city_map.graph.get_adjacent(city_map.city_name_to_node_id.get('Houston'))
    let vs2 = Cons.map(lambda n: city_map.node_id_to_city_name.get(n), vs)
    assert Cons.sort(lambda a, b: a < b, vs2)\
            == Cons.from_vec(['Baton Rouge', 'Bellaire', 'San Antonio'])


###
### DFS
###

def dfs_visited(graph: WUGRAPH!,\
        v: Vertex?,\
        f: FunC[Vertex?, AnyC],\
        is_visited: VecC[bool?])\
        -> NoneC:
    if not(is_visited[v]):
        is_visited[v] = True
        f(v)
        def rc(v: Vertex?):
            dfs_visited(graph, v, f, is_visited)
        Cons.foreach(rc, graph.get_adjacent(v))

# dfs : WUGRAPH Vertex [Vertex -> any] -> None
# Performs a depth-first search starting at `start`, applying `f`
# to each vertex once as it is discovered by the search.
def dfs(graph: WUGRAPH!, start: Vertex?, f: FunC[Vertex?, AnyC]) -> NoneC:
    dfs_visited(graph, start, f, vec(graph.len(), lambda x: False))

# dfs_to_list : WUGRAPH Vertex -> ListOf[Vertex]
# Performs a depth-first search starting at `start` and returns a
# list of all reachable vertices.
#
# This function uses your `dfs` function to build a list in the
# order of the search. It will pass the test below if your dfs visits
# each reachable vertex once, regardless of the order in which it calls
# `f` on them. However, you should test it more thoroughly than that
# to make sure it is calling `f` (and thus exploring the graph) in
# a correct order.
def dfs_to_list(graph: WUGRAPH!, start: Vertex?) -> VertexList?:
    let list = None
    # Add to the front when we visit a node
    dfs(graph, start, lambda new: list = cons(new, list))
    # Reverse to the get elements in visiting order.
    return Cons.rev(list)

###
### TESTING
###

## You should test your code thoroughly. Here is one test to get you started:

test 'dfs_to_list(example_graph())':
    # Cons.from_vec is a convenience function from the `cons` library that
    # allows you to write a vector (using the nice vector syntax), and get
    # a linked list with the same elements.
    assert sort_vertices(dfs_to_list(example_graph(), 0)) \
        == Cons.from_vec([0, 1, 2, 3, 4, 5])

test 'dfs/disconnected':
    let g = WUGraph(5)
    g.set_edge(0, 2, 1)
    g.set_edge(4, 0, 2)
    g.set_edge(1, 3, 3)

    for v in [0, 2, 4]:
        assert sort_vertices(dfs_to_list(g, v)) == Cons.from_vec([0, 2, 4])
    for v in [1, 3]:
        assert sort_vertices(dfs_to_list(g, v)) == Cons.from_vec([1, 3])

test 'dfs/or':
    let g = WUGraph(6)
    g.set_edge(0, 1, 1)
    g.set_edge(0, 2, 2)
    g.set_edge(2, 3, 3)
    g.set_edge(2, 4, 4)
    g.set_edge(4, 5, 5)

    def check(expected_vs):
        Cons.to_vec(dfs_to_list(g, 0)) == expected_vs
    assert Cons.ormap(check,\
            Cons.from_vec([[0, 1, 2, 3, 4, 5],\
            [0, 1, 2, 4, 5, 3],\
            [0, 2, 3, 4, 5, 1],\
            [0, 2, 3, 4, 5, 1]]))
