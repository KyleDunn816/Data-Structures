#lang dssl2

# Final project: Trip Planner

import cons
import sbox_hash
import 'project-lib/dictionaries.rkt'
import 'project-lib/binheap.rkt'
import 'project-lib/graph.rkt'


# Abbreviations:
# attr ~ attribute
# cat ~ category
# lat ~ latitude
# lon ~ longitude
# poi ~ point-of-interest
# pos ~ position
# seg ~ road segment

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

    # All entries.
    def get_all(self) -> VecC[EntryAll?[K, V]]:
        let r = vec(self.len())
        let i = 0
        def add(entry: EntryAll?[K, V]) -> NoneC:
            r[i] = entry
            i = i + 1
        Cons.foreach(add, self._head)
        assert i == self.len()
        r


# `HashTableAll` is `HashTable` from the previous homework
# with the `get_all` method added
# and the ability to grow the underlying `VecC`.
class HashTableAll[K, V] (DICT):
    let _hash: FunC[AnyC, nat?]
    let _size: int?
    let _data: VecC[AssociationListAll?[K, V]]

    def _reset(self, nbuckets: nat?):
        if nbuckets <= 0:
            error('HashTableAll: the number of buckets must be positive')
        self._size = 0
        self._data = vec(nbuckets, lambda x: AssociationListAll[K, V]())

    def __init__(self, nbuckets: nat?, hash: FunC[AnyC, nat?]):
        self._hash = hash
        self._reset(nbuckets)

    # The bucket index of `key`.
    # The index of the bucket where all entries with `key` are stored.
    def _bucket_hash(self, key: K) -> nat?:
        self._hash(key) % self._data.len()

    # The bucket of `key`.
    # The bucket where all entries with `key` are stored.
    def _bucket(self, key: K) -> AssociationListAll?[K, V]:
        self._data[self._bucket_hash(key)]

    def _put(self, key: K, value: V) -> NoneC:
        if not(self._bucket(key).put_mem(key, value)):
            self._size = self._size + 1

    def _size_up_if_needed(self):
        if self.len() >= 2 * self._data.len():
            let entries = self.get_all()
            self._reset(self._data.len() * 3 // 2 + 1)
            for entry in entries:
                self._put(entry.get_key(), entry.get_value())

    def len(self) -> nat?:
        self._size

    def mem?(self, key: K) -> bool?:
        self._bucket(key).mem?(key)

    def get(self, key: K) -> V:
        self._bucket(key).get(key)

    def put(self, key: K, value: V) -> NoneC:
        self._size_up_if_needed()
        self._put(key, value)

    def del(self, key: K) -> NoneC:
        if self._bucket(key).del_mem(key):
            self._size = self._size - 1

    # This avoids trying to print the hash function, since it's not really
    # printable and isn’t useful to see anyway:
    def __print__(self, print):
        print("#<object:HashTableAll  _hash=... _size=%p _data=%p>",
              self._size, self._data)

    # All entries.
    def get_all(self) -> VecC[EntryAll?[K, V]]:
        let r = vec(self.len())
        let i = 0
        def add(entry: EntryAll?[K, V]) -> NoneC:
            r[i] = entry
            i = i + 1
        for bucket in self._data:
            for entry in bucket.get_all():
                add(entry)
        assert i == self.len()
        r


### Basic Types ###

#  - Latitudes and longitudes are numbers:
let Lat?  = num?
let Lon?  = num?

#  - Point-of-interest categories and names are strings:
let Cat?  = str?
let Name? = str?

### Raw Item Types ###

#  - Raw positions are 2-element vectors with a latitude and a longitude
let RawPos? = TupC[Lat?, Lon?]

#  - Raw road segments are 4-element vectors with the latitude and
#    longitude of their first endpoint, then the latitude and longitude
#    of their second endpoint
let RawSeg? = TupC[Lat?, Lon?, Lat?, Lon?]

#  - Raw points-of-interest are 4-element vectors with a latitude, a
#    longitude, a point-of-interest category, and a name
let RawPOI? = TupC[Lat?, Lon?, Cat?, Name?]

### Contract Helpers ###

# ListC[T] is a list of `T`s (linear time):
let ListC = Cons.ListC
# List of unspecified element type (constant time):
let List? = Cons.list?


interface TRIP_PLANNER:

    # Returns the positions of all the points-of-interest that belong to
    # the given category.
    def locate_all(
            self,
            dst_cat:  Cat?           # point-of-interest category
        )   ->        ListC[RawPos?] # positions of the POIs

    # Returns the shortest path, if any, from the given source position
    # to the point-of-interest with the given name.
    def plan_route(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_name: Name?          # name of goal
        )   ->        ListC[RawPos?] # path to goal

    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    # "Nearest" is defined with respect to the length of the shortest path.
    def find_nearby(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_cat:  Cat?,          # point-of-interest category
            n:        nat?           # maximum number of results
        )   ->        ListC[RawPOI?] # list of nearby POIs


def square(x: num?) -> num?:
    x*x

# Euclidean length of `seg`.
def seg_length(seg: RawSeg?) -> num?:
    (square(seg[0] - seg[2]) + square(seg[1] - seg[3])).sqrt()

# "Lower than" comparison of `RawPos?`.
def pos_lt(pos0: RawPos?, pos1: RawPos?) -> AnyC:
    pos0[0] < pos1[0] or (pos0[0] == pos1[0] and pos0[1] < pos1[1])

def sort_pos(xs: ListC[RawPos?]) -> ListC[RawPos?]:
    Cons.sort(pos_lt, xs)

# "Lower than" comparison of `RawPOI?`.
def poi_lt(poi0: RawPOI?, poi1: RawPOI?) -> AnyC:
    poi0[3] < poi1[3]

def sort_poi(xs: ListC[RawPOI?]) -> ListC[RawPOI?]:
    Cons.sort(poi_lt, xs)

# `RawPos?` identifier
let PosI? = nat?

# Element of `BinHeap`. For Dijkstra's algorithm.
struct DijkstraSortedVertex:
    let v: Vertex?
    let distance: num?

# The vertex stores distance
# and a predecessor vertex for finding shortest paths.
# For Dijkstra's algorithm.
class DijkstraVertex:
    let _distance: num?
    let _predecessor: Vertex?

    def __init__(self, distance: num?, predecessor: Vertex?):
        self._distance = distance
        self._predecessor = predecessor

    def get_predecessor(self):
        self._predecessor

    # Updates the distance of this vertex to `distance` if there is a need.
    # Returns whether the distance was updated.
    # If the distance was updated, assigns the predecessor
    # (for finding shortest paths) of this vertex to `predecessor`.
    def update_distance(self, distance: num?, predecessor: Vertex?):
        if distance < self._distance:
            self._distance = distance
            self._predecessor = predecessor
            True
        else:
            False

    # Whether `sv` is outdated.
    # `BinHeap` does not allow to change element priorites (distances).
    # Hence we insert the element with the new distance instead of changing.
    # After this, the element with the old distance is considered outdated.
    # `sv` is an element of `BinHeap`.
    def is_outdated(self, sv: DijkstraSortedVertex?):
        self._distance < sv.distance

# non-source vertex
def dijkstra_vertex_create():
    DijkstraVertex(inf, 0)

# source vertex
def dijkstra_vertex_create_src():
    DijkstraVertex(0, 0)

# Calculates the shortest path from the vertex `src` in `graph`
# using Dijkstra's algorithm.
# (`src` and `graph` are arguments of the constructor).
class DijkstraIterator:
    let _graph: WUGRAPH?
    let _src: Vertex?
    let _v_attr: VecC[DijkstraVertex?]
    let _sorted_v: BinHeap?[DijkstraSortedVertex?]

    # All weights in `graph` must be non-negative.
    # The number of edges in `graph` must be at most `edge_limit`.
    def __init__(self, graph: WUGRAPH?, edge_limit: nat?, src: Vertex?):
        self._graph = graph
        self._src = src
        def dv(v):
            if v == src: dijkstra_vertex_create_src()
            else: dijkstra_vertex_create()
        self._v_attr = vec(graph.len(), dv)
        # Every edge may cause relaxation at most one time.
        # Hence the size of `BinHeap`.
        self._sorted_v = BinHeap[DijkstraSortedVertex?](\
            edge_limit,\
            lambda sv0, sv1: sv0.distance < sv1.distance)
        self._sorted_v.insert(DijkstraSortedVertex{v: src, distance: 0})

    # Returns vertices reachable from `src`
    # in order of increasing distance path length.
    # If no such vertices remains, it returns `None`.
    def next_vertex(self) -> OrC(NoneC, Vertex?):
        while True:
            if self._sorted_v.len() == 0:
                return None
            let sv = self._sorted_v.find_min()
            self._sorted_v.remove_min()
            if not(self._v_attr[sv.v].is_outdated(sv)):
                let sv_distance = sv.distance
                def relax_v(v: Vertex?) -> NoneC:
                    let v_distance = sv_distance + self._graph.get_edge(sv.v, v)
                    let is_updated = self._v_attr[v]\
                        .update_distance(v_distance, sv.v)
                    if is_updated:
                        self._sorted_v.insert(\
                            DijkstraSortedVertex{v: v, distance: v_distance})
                Cons.foreach(relax_v, self._graph.get_adjacent(sv.v))
                return sv.v

    # The shortest path from the vertex `src` (given to the constructor) to `v`.
    # `v` must be already returned by `next_vertex`.
    def shortest_path(self, v: Vertex?):
        let path = cons(v, None)
        let u = v
        while u != self._src:
            u = self._v_attr[u].get_predecessor()
            path = cons(u, path)
        return path

struct PosAttr:
    let pos_i: PosI?
    let pos: RawPos?

class TripPlanner (TRIP_PLANNER):
    # It should be `HashTableAll?[PosI?, ListC[RawPOI?]]` instead of `AnyC`,
    # but it does not work because of contract violation.
    # `self._cat_to_set_pos.get(cat)` must contain a key `pos_i`
    # if and only if there is a POI in `cat` at `pos_i`
    # (in other words, all values are non-empty lists).
    let _cat_to_set_pos: HashTableAll?[Cat?, AnyC]

    let _poi_to_pos_attr: HashTableAll?[Name?, PosAttr?]
    let _pos_to_pos_attr: HashTableAll?[RawPos?, PosAttr?]
    let _pos_i_to_pos_attr: VecC[OrC(NoneC, PosAttr?)]
    let _graph: WUGRAPH?

    # The number of edges in `_graph` must be at most `_edge_limit`.
    # `WUGRAPH` does not provide a method that returns the number of edges,
    # thus we need to keep `_edge_limit` along the graph.
    # Why the limit on the number of edges and not the number of edges
    # is explained in the code where `_edge_limit` is calculated.
    let _edge_limit: nat?

    def __init__(self, segs: VecC[RawSeg?], pois: VecC[RawPOI?]):
        let hashf = make_sbox_hash()
        self._cat_to_set_pos = HashTableAll[Cat?, AnyC](2, hashf)
        self._poi_to_pos_attr = HashTableAll[Name?, PosAttr?](2, hashf)
        self._pos_to_pos_attr = HashTableAll[RawPos?, PosAttr?](2, hashf)

        let pos_i = 0

        def add_pos(pos: RawPos?) -> PosAttr?:
            if self._pos_to_pos_attr.mem?(pos):
                self._pos_to_pos_attr.get(pos)
            else:
                let pos_attr = PosAttr{pos_i: pos_i, pos: pos}
                self._pos_to_pos_attr.put(pos, pos_attr)
                pos_i = pos_i + 1
                pos_attr

        for seg in segs:
            add_pos([seg[0], seg[1]])
            add_pos([seg[2], seg[3]])

        for poi in pois:
            let pos_attr = add_pos([poi[0], poi[1]])
            let pos_i = pos_attr.pos_i

            let name = poi[3]
            self._poi_to_pos_attr.put(name, pos_attr)

            let cat = poi[2]
            if self._cat_to_set_pos.mem?(cat):
                let set_pos = self._cat_to_set_pos.get(cat)
                let pois
                if set_pos.mem?(pos_i):
                    pois = set_pos.get(pos_i)
                else:
                    pois = None
                set_pos.put(pos_i, cons(poi, pois))
            else:
                let set_pos = HashTableAll[PosI?, ListC[RawPOI?]](2, hashf)
                set_pos.put(pos_i, cons(poi, None))
                self._cat_to_set_pos.put(cat, set_pos)

        self._pos_i_to_pos_attr = [None; self._pos_to_pos_attr.len()]
        for entry in self._pos_to_pos_attr.get_all():
            let pos_attr = entry.get_value()
            self._pos_i_to_pos_attr[pos_attr.pos_i] = pos_attr

        self._graph = WUGraph(self._pos_i_to_pos_attr.len())
        for seg in segs:
            let u = self._pos_to_pos_attr.get([seg[0], seg[1]]).pos_i
            let v = self._pos_to_pos_attr.get([seg[2], seg[3]]).pos_i
            self._graph.set_edge(u, v, seg_length(seg))
        # `segs` may contain two edges between the same pair of vertices.
        # Thus the number of edges in `self._graph`
        # may be less than `Cons.len(segs)`.
        self._edge_limit = segs.len()

    def __print__(self, print):
        let pos_i = 0
        println('_pos_i_to_pos_attr')
        for pos_attr in self._pos_i_to_pos_attr:
            print('  %p: %p [%p, %p] *', pos_i, pos_attr.pos_i,\
                pos_attr.pos[0], pos_attr.pos[1])
            Cons.foreach(lambda poi: print(' %p', poi), pos_attr.pois)
            println('')
            pos_i = pos_i + 1

        println('_pos_to_pos_attr')
        for entry in self._pos_to_pos_attr.get_all():
            println('  [%p, %p]: %p',\
                entry.get_key()[0], entry.get_key()[1],\
                entry.get_value().pos_i)

        println('_cat_to_set_pos')
        for entry in self._cat_to_set_pos.get_all():
            print('  %p:', entry.get_key())
            for entry in entry.get_value().get_all():
                print(' %p', entry.get_key())
            println('')

        println('_poi_to_pos_attr')
        for entry in self._poi_to_pos_attr.get_all():
            println('  %p: %p', entry.get_key(), entry.get_value().pos_i)

        print('_graph')
        def print_edge(edge: WEdge?) -> NoneC:
            print(' ((%p %p) %p)', edge.u, edge.v, edge.w)
        Cons.foreach(print_edge, self._graph.get_all_edges())

        print('_edge_limit: %p', self._edge_limit)

    def locate_all(
            self,
            dst_cat:  Cat?
        )   ->        ListC[RawPos?]:
        if not(self._cat_to_set_pos.mem?(dst_cat)):
            return None
        let r_vec = self._cat_to_set_pos.get(dst_cat).get_all()\
            .map(lambda entry: self._pos_i_to_pos_attr[entry.get_key()].pos)
        Cons.from_vec(r_vec)

    def plan_route(
            self,
            src_lat:  Lat?,
            src_lon:  Lon?,
            dst_name: Name?
        )   ->        ListC[RawPos?]:
        let src_pos = [src_lat, src_lon]
        if not(self._pos_to_pos_attr.mem?(src_pos)):
            return None
        let src_v = self._pos_to_pos_attr.get(src_pos).pos_i

        if not(self._poi_to_pos_attr.mem?(dst_name)):
            return None
        let dst_v = self._poi_to_pos_attr.get(dst_name).pos_i

        let di = DijkstraIterator(self._graph, self._edge_limit, src_v)
        while True:
            let v = di.next_vertex()
            if v is None:
                return None
            if v == dst_v:
                return Cons.map(\
                    lambda v: self._pos_i_to_pos_attr.get(v).pos,\
                    di.shortest_path(v))

    def find_nearby(
            self,
            src_lat:   Lat?,
            src_lon:   Lon?,
            dst_cat:   Cat?,
            max_poi_n: nat?
        )   ->         ListC[RawPOI?]:
        let src_pos = [src_lat, src_lon]
        if not(self._pos_to_pos_attr.mem?(src_pos)):
            return None
        let src_v = self._pos_to_pos_attr.get(src_pos).pos_i

        if not(self._cat_to_set_pos.mem?(dst_cat)):
            return None
        let dst_set_pos = self._cat_to_set_pos.get(dst_cat)

        let di = DijkstraIterator(self._graph, self._edge_limit, src_v)
        let pois = None
        # `poi_n == Cons.len(pois)` must always be true.
        # Of course, we may call `Cons.len` in the loop below,
        # but `Cons.len` requires $O(n)$ time on a singly-linked list.
        # Using `poi_n` requires $O(1)$ time.
        # Using a data structure where calculating its length
        # requires $O(1)$ time (a dynamic array, a singly-linked list
        # with an embedded length) would simplify the algorithm,
        # but they are not provided.
        let poi_n = 0
        while True:
            let v = di.next_vertex()
            if v is None:
                return pois
            if dst_set_pos.mem?(v):
                let pois_at_pos = dst_set_pos.get(v)
                while poi_n != max_poi_n and not(pois_at_pos is None):
                    pois = cons(pois_at_pos.data, pois)
                    poi_n = poi_n + 1
                    pois_at_pos = pois_at_pos.next
                if poi_n == max_poi_n:
                    return pois


def extended_assignment_example() -> TripPlanner?:
    TripPlanner([[0,0, 0,1],
        [0,1, 0,2],\
        [0,0, 1,0],\
        [0,1, 1,1],\
        [0,2, 1,2],\
        [1,2, 1,3],\
        [1,0, 1,1],\
        [1,1, 1,2],\
        [1,2, 1,3],\
        [-0.2,3.3, 1,3],\
        [0.9,0.1, 0,0],\
        [0.9,0.1, 1,1]],\
        [[0,0, 'food', 'Sandwiches'],\
        [0,1, 'food', 'Pasta'],\
        [0,1, 'clothes', 'Pants'],\
        [-0.2,3.3, 'food', 'Burritos'],\
        [1,1, 'bank', 'Local Credit Union'],\
        [1,3, 'bar', 'Bar None'],\
        [1,3, 'bar', 'H Bar'],\
        [2,0, 'clothes', 'Xara']])

test 'extended_assignment_example/locate_all/food':
    assert sort_pos(extended_assignment_example().locate_all('food'))\
        == Cons.from_vec([[-0.2,3.3], [0,0], [0,1]])

test 'extended_assignment_example/locate_all/bar':
    assert extended_assignment_example().locate_all('bar')\
        == Cons.from_vec([[1,3]])

test 'extended_assignment_example/plan_route/0':
    assert extended_assignment_example().plan_route(0,2, 'Burritos')\
        == Cons.from_vec([[0,2], [1,2], [1,3], [-0.2,3.3]])

test 'extended_assignment_example/plan_route/1':
    assert extended_assignment_example().plan_route(0,2, 'Sandwiches')\
        == Cons.from_vec([[0,2], [0,1], [0,0]])

test 'extended_assignment_example/plan_route/2':
    assert extended_assignment_example().plan_route(-0.2,3.3, 'Local Credit Union')\
        == Cons.from_vec([[-0.2,3.3], [1,3], [1,2], [1,1]])

test 'extended_assignment_example/plan_route/3':
    assert extended_assignment_example().plan_route(0,0, 'Burritos')\
        == Cons.from_vec([[0,0], [0.9,0.1], [1,1], [1,2], [1,3], [-0.2,3.3]])

test 'extended_assignment_example/plan_route/4':
    assert extended_assignment_example().plan_route(2,0, 'Sandwiches') == None

test 'extended_assignment_example/find_nearby/no_cat':
    assert extended_assignment_example().find_nearby(0,0, 'hospital', 10) == None

test 'extended_assignment_example/find_nearby/unreachable':
    assert extended_assignment_example().find_nearby(2,0, 'bank', 10) == None

test 'extended_assignment_example/find_nearby/0':
    assert extended_assignment_example().find_nearby(2,0, 'clothes', 10)\
        == cons([2,0, 'clothes', 'Xara'], None)

test 'extended_assignment_example/find_nearby/1':
    assert extended_assignment_example().find_nearby(1,0, 'clothes', 10)\
        == cons([0,1, 'clothes', 'Pants'], None)

test 'extended_assignment_example/find_nearby/2':
    assert extended_assignment_example().find_nearby(1,2, 'food', 0) == None

test 'extended_assignment_example/find_nearby/3':
    assert extended_assignment_example().find_nearby(1,2, 'food', 1)\
        == cons([0,1, 'food', 'Pasta'], None)

test 'extended_assignment_example/find_nearby/4':
    assert sort_poi(extended_assignment_example().find_nearby(1,2, 'food', 2))\
        == Cons.from_vec([[-0.2,3.3, 'food', 'Burritos'], [0,1, 'food', 'Pasta']])

test 'extended_assignment_example/find_nearby/5':
    assert sort_poi(extended_assignment_example().find_nearby(1,2, 'food', 3))\
        == Cons.from_vec([[-0.2,3.3, 'food', 'Burritos'],\
        [0,1, 'food', 'Pasta'],\
        [0,0, 'food', 'Sandwiches']])

test 'extended_assignment_example/find_nearby/6':
    assert sort_poi(extended_assignment_example().find_nearby(1,2, 'food', 4))\
        == Cons.from_vec([[-0.2,3.3, 'food', 'Burritos'],\
        [0,1, 'food', 'Pasta'],\
        [0,0, 'food', 'Sandwiches']])

test 'extended_assignment_example/find_nearby/7':
    assert extended_assignment_example().find_nearby(1,1, 'bank', 10)\
        == cons([1,1, 'bank', 'Local Credit Union'], None)

test 'extended_assignment_example/find_nearby/8':
    assert extended_assignment_example().find_nearby(2,0, 'clothes', 10)\
        == cons([2,0, 'clothes', 'Xara'], None)

test 'no_key/0':
    assert TripPlanner([[0, 0, 1, 0]], []).locate_all('bank') == None

test 'no_key/1':
    assert TripPlanner(\
      [[0, 0, 1, 0]],\
      [[1, 0, 'bank', 'Union']])\
      .locate_all('food')\
      == None

test 'no_key/2':
    assert TripPlanner(\
        [[0, 0, 1.5, 0],\
        [1.5, 0, 2.5, 0],\
        [2.5, 0, 3, 0]],\
        [[1.5, 0, 'bank', 'Union'],\
        [3, 0, 'barber', 'Tony']])\
        .plan_route(0, 0, 'Judy')\
        == None


def my_first_example():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0]],
                       [[0,0, "bar", "The Empty Bottle"],
                        [0,1, "food", "Pierogi"]])

test 'My first locate_all test':
    assert my_first_example().locate_all("food") == \
        cons([0,1], None)

test 'My first plan_route test':
   assert my_first_example().plan_route(0, 0, "Pierogi") == \
       cons([0,0], cons([0,1], None))

test 'My first find_nearby test':
    assert my_first_example().find_nearby(0, 0, "food", 1) == \
        cons([0,1, "food", "Pierogi"], None)
