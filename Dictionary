
#lang dssl2

# HW3: Dictionaries

import cons
import sbox_hash

# A signature for the dictionary ADT. The contract parameters `K` and
# `V` are the key and value types of the dictionary, respectively.
interface DICT[K, V]:
    # Returns the number of key-value pairs in the dictionary.
    def len(self) -> nat?
    # Is the given key mapped by the dictionary?
    # Notation: `key` is the name of the parameter. `K` is its contract.
    def mem?(self, key: K) -> bool?
    # Gets the value associated with the given key; calls `error` if the
    # key is not present.
    def get(self, key: K) -> V
    # Modifies the dictionary to associate the given key and value. If the
    # key already exists, its value is replaced.
    def put(self, key: K, value: V) -> NoneC
    # Modifes the dictionary by deleting the association of the given key.
    def del(self, key: K) -> NoneC
    # The following method allows dictionaries to be printed
    def __print__(self, print)

class Entry[K, V]:
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

class AssociationList[K, V] (DICT):
    let _head: Cons.ListC[Entry?[K, V]]

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
        print("#<object:AssociationList head=%p>", self._head)

    # The `entry` with `key` or `False` if there is no such.
    def _find(self, key: K) -> OrC(Entry?[K, V], False):
        Cons.ormap(lambda entry: entry if entry.get_key() == key else False,\
                self._head)

    # Does the same as `self.put`.
    # Returns whether the given key was mapped by the dictionary
    # before the call.
    def put_mem(self, key: K, value: V) -> bool?:
        let entry = self._find(key)
        if entry is False:
            self._head = cons(Entry[K, V](key, value), self._head)
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


def dict_test(a: DICT![str?, int?]) -> NoneC:
    assert not a.mem?('hello')

    a.put('hello', 5)
    assert a.len() == 1
    assert a.mem?('hello')
    assert a.get('hello') == 5

    a.put('cat', 1)
    assert a.len() == 2
    assert a.get('hello') == 5
    assert a.mem?('cat')
    assert a.get('cat') == 1
    assert not a.mem?('mouse')

    a.del('ca')
    assert a.len() == 2
    a.del('c')
    assert a.len() == 2
    a.del('hello0')
    assert a.len() == 2

    a.put('a0', 100)
    a.put('a1', 101)
    a.put('a2', 102)
    a.put('b0', 200)
    a.put('b1', 201)
    a.put('b2', 202)
    assert a.len() == 8
    assert a.get('a0') == 100
    assert a.mem?('a0')
    assert a.get('a1') == 101
    assert a.mem?('a1')
    assert a.get('a2') == 102
    assert not a.mem?('a3')
    assert not a.mem?('a4')

    a.put('cat', 12)
    assert a.get('cat') == 12
    assert a.get('hello') == 5
    a.put('hello', 13)
    assert a.get('cat') == 12
    assert a.get('hello') == 13

    a.del('b2')
    a.del('a1')
    a.del('b0')
    assert a.len() == 5
    assert a.mem?('a0')
    assert not a.mem?('a1')
    assert a.mem?('a2')
    assert not a.mem?('b0')
    assert a.mem?('b1')
    assert not a.mem?('b2')

    a.del('hello')
    a.del('cat')
    a.del('a2')
    a.del('b1')
    a.del('a0')
    assert a.len() == 0

test 'AssociationList':
    dict_test(AssociationList())
    

class HashTable[K, V] (DICT):
    let _hash: FunC[AnyC, nat?]
    let _size: int?
    let _data: VecC[AssociationList?[K, V]]

    def __init__(self, nbuckets: nat?, hash: FunC[AnyC, nat?]):
        if nbuckets <= 0:
            error('HashTable: the number of buckets must be positive')
        self._hash = hash
        self._size = 0
        self._data = vec(nbuckets, lambda x: AssociationList[K, V]())

    def len(self) -> nat?:
        self._size

    def mem?(self, key: K) -> bool?:
        self._bucket(key).mem?(key)

    def get(self, key: K) -> V:
        self._bucket(key).get(key)

    def put(self, key: K, value: V) -> NoneC:
        if not(self._bucket(key).put_mem(key, value)):
            self._size = self._size + 1

    def del(self, key: K) -> NoneC:
        if self._bucket(key).del_mem(key):
            self._size = self._size - 1

    # This avoids trying to print the hash function, since it's not really
    # printable and isn’t useful to see anyway:
    def __print__(self, print):
        print("#<object:HashTable  _hash=... _size=%p _data=%p>",
              self._size, self._data)

    # The bucket index of `key`.
    # The index of the bucket where all entries with `key` are stored.
    def _bucket_hash(self, key: K):
        self._hash(key) % len(self._data)

    # The bucket of `key`.
    # The bucket where all entries with `key` are stored.
    def _bucket(self, key: K):
        self._data[self._bucket_hash(key)]


# first_char_hasher(String) -> Natural
# A simple and bad hash function that just returns the ASCII code
# of the first character.
# Useful for debugging because it's easily predictable.
def first_char_hasher(s: str?) -> int?:
    if s.len() == 0:
        return 0
    else:
        return int(s[0])

test 'HashTable/first_char_hasher':
    dict_test(HashTable(3, first_char_hasher))

test 'HashTable/make_sbox_hash':
    dict_test(HashTable(4, make_sbox_hash()))


def compose_phrasebook(d: DICT!) -> DICT?:
    d.put('denaro', ['money', 'deˈnaro'])
    d.put('polizia', ['police', 'politˈtsia'])
    d.put('cibo', ['food', 'ˈtʃibo'])
    d.put('treno', ['train', 'ˈtrɛno'])
    d.put('gabinetto', ['toilet', 'ɡabiˈnetto'])
    d.put('macchina', ['car', 'ˈmakkina'])
    d.put('mare', ['sea', 'ˈmare'])
    d.put('occhiali', ['glasses', 'okˈkjali '])
    d

test "AssociationList phrasebook":
    let d = compose_phrasebook(AssociationList[str?, VecC[str?]]())
    assert d.get('cibo')[1] == 'ˈtʃibo'

test "HashTable phrasebook":
    let d = compose_phrasebook(HashTable(100, make_sbox_hash()))
    assert d.get('macchina')[1] == 'ˈmakkina'
