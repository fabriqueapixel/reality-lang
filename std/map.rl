import std.iterator;

type Map<K, V> = List<Tuple<K, V>>;

impl fn (m: Map<K, V>) keys<K, V>() -> List<K> {
    let result = &List.init<K>();

    let i = 0;
    while i < m.length {
        if m[i] is Pair(let k, let v) {
            result.push(k);
        }

        i = i + 1;
    };

    return *result;
}

impl fn (m: Map<K, V>) get<K, V>(key: K) -> Option<V> {
    let i = 0;
    while i < m.length {
        if m[i] is Pair(let k, let v) && k == key {
            return Some(v);
        }

        i = i + 1;
    };

    return None;
}

impl fn (m: *Map<K, V>) set<K, V>(key: K, value: V) -> unit {
    let i = 0;
    while i < m->length {
        if m->get_index(i) is Pair(let k, let v) && k == key {
            *m->get_index_mut(i) = Pair(key, value);
            return unit;
        }

        i = i + 1;
    };

    m.push(Pair(key, value));

    return unit;
}

mod Map {
    fn from_list<K, V>(lst: List<Tuple<K, V>>) -> Map<K, V> {
        return lst;
    }

    fn empty<K, V>() -> Map<K, V> {
        return List.init<Tuple<K, V>>();
    }

    fn init<K, V>() -> Map<K, V> {
        return List.init<Tuple<K, V>>();
    }
}

impl fn (m: Map<K, V>) insert<K, V>(key: K, value: V) -> Map<K, V> {
    let i = 0;
    while i < m.length {
        if m[i] is Pair(let k, let v) && k == key {
            m[i] = Pair(key, value);
            return m;
        }
        i = i + 1;
    };

    m.push(Pair(key, value));
    return m;
}

impl fn (m: Map<K, V>) has_key<K, V>(key: K) -> bool {
    let i = 0;
    while i < m.length {
        if m[i] is Pair(let k, let v) && k == key {
            return true;
        }
        i = i + 1;
    };

    return false;
}

impl fn (m: *Map<K, V>) set_with<K, V>(key: K, f: fn(K, V) -> V, default: V) -> unit {
    let i = 0;
    while i < m->length {
        if m->get_index(i) is Pair(let k, let v) && k == key {
            *m->get_index_mut(i) = Pair(key, f(k, v));
            return unit;
        }
        i = i + 1;
    };

    m.push(Pair(key, default));
    return unit;
}

impl fn (it: Map<K, V>) iter<K, V>() -> Iterator<Map<K, V>> {
    {
        container: it,
        index: GC.allocate(0)
    }
}

impl fn (it: Iterator<Map<K, V>>) next<K, V>() -> Option<Tuple<K, V>> {
    if *it.index < it.container.length {
        let value = it.container.at(*it.index);
        *it.index = *it.index + 1;
        Some(value)
    } else {
        None
    }
}
