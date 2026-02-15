type Iterator<A> = {
    container: A,
    index: *int
}

impl fn (it: List<A>) iter<A>() -> Iterator<List<A>> {
    {
        container: it,
        index: GC.allocate(0)
    }
}

impl fn (self: List<A>) at<A>(index: int) -> A {
    self.get_index(index)
}

impl fn (it: Iterator<List<A>>) next<A>() -> Option<A> {
    if *it.index < it.container.length {
        let value = it.container.at(*it.index);
        *it.index = *it.index + 1;
        Some(value)
    } else {
        None
    }
}

type Range = {
    start: int,
    end: int
}

type RangeIterator = {
    range: Range,
    current: *int
}

impl fn (r: Range) iter() -> RangeIterator {
    {
        range: r,
        current: GC.allocate(r.start)
    }
}

pub fn range(start: int, end: int) -> Range {
    {
        start: start,
        end: end
    }
}

impl fn (it: RangeIterator) next() -> Option<int> {
    if *it.current < it.range.end {
        let value = *it.current;
        *it.current = *it.current + 1;
        Some(value)
    } else {
        None
    }
}

impl fn (it: Range) to_list() -> List<int> {
    let list = new List.init<int>();

    for value in it {
        list.push(value);
    }

    *list
}
