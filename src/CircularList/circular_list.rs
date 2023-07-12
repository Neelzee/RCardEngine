use std::{ops::Not, borrow::Cow, any::Any, fmt};

pub struct CircularList<T> {
    list: Vec<T>,
    index: usize,
    rot: Rotation
}

#[derive(Debug, Clone, Copy)]
pub enum Rotation {
    Left,
    Right
}

impl Not for Rotation {
    type Output = Rotation;

    fn not(self) -> Self::Output {
        match self {
            Rotation::Left => Rotation::Right,
            Rotation::Right => Rotation::Left,
        }
    }
}

impl<T: Any> CircularList<T> {
    pub fn new<V>() -> CircularList<T>
    where
        V: Any
    {
        CircularList { list: Vec::new(), index: 0, rot: Rotation::Left }
    }

    pub fn from_list<K, I>(list: I) -> CircularList<T>
    where
        I: IntoIterator<Item = T>
    {
        let mut v: Vec<T> = Vec::new();

        for el in list {
            v.push(el);
        }

        CircularList { list: v, index: 0, rot: Rotation::Left }
    }

    pub fn update(&mut self, new_focus: T) {
        self.list.remove(self.index);
        self.list.insert(self.index, new_focus);
    }

    pub fn focus(&mut self) -> Option<T>
    where T: Clone
    {
        let m = self.list.len();

        if m == 0 {
            self.index = 0;
        } else {
            self.index %= m;
        }


        self.list.get(self.index as usize).cloned()
    }

    /**
     * Insert an element into the CList as the new focus. The old focus is now the next element to the left.
     */
    pub fn insert_l(&mut self, new_focus: T) {
        self.list.push(new_focus);
        self.rot_r();
    }

    /**
     * Insert an element into the CList as the new focus. The old focus is now the next element to the right.
     */
    pub fn insert_r(&mut self, new_focus: T) {
        self.list.insert(self.index, new_focus);
    }

    pub fn remove_l(&mut self) -> T {
        let c = self.list.remove(self.index);

        match self.index.checked_add_signed(-1) {
            Some(e) => self.index = e,

            None => self.index = if self.list.len() == 0 { 0 } else { self.list.len() - 1 }
        }

        c
    }

    pub fn remove_r(&mut self) -> T {
        self.rot_r();
        let c = self.list.remove(if self.index.clone() == 0 { self.list.len() - 1 } else { self.index.clone() - 1 });

        match self.index.checked_add_signed(-1) {
            Some(e) => self.index = e,

            None => self.index = if self.list.len() == 0 { 0 } else { self.list.len() - 1 }
        }

        c
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn rot_r(&mut self) {
        self.index += 1;

        if self.index >= self.list.len() {
            self.index = 0;
        }
    }

    pub fn rot_l(&mut self) {
        match self.index.checked_add_signed(-1) {
            Some(e) => self.index = e,

            None => self.index = if self.list.len() == 0 { 0 } else { self.list.len() - 1 }
        }
    }

    pub fn reverse_rotation(&mut self) {
        self.rot = !self.rot.clone();
    }

    pub fn rot(&mut self) {
        match self.rot {
            Rotation::Left => self.rot_l(),

            Rotation::Right => self.rot_r(),
        }
    }

    pub fn is_empty(&self) -> bool {
        return self.list.is_empty();
    }

}


impl<T: Any + fmt::Debug> fmt::Debug for CircularList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CircularList").field("list", &self.list).field("index", &self.index).finish()
    }
}

pub struct CLIntoIterator<T> {
    cl: CircularList<T>
}

impl<T: Any + Clone> Iterator for CLIntoIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.cl.rot();

        self.cl.focus()
    }
}

impl<T: Any + Clone> IntoIterator for CircularList<T> {
    type Item = T;

    type IntoIter = CLIntoIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        CLIntoIterator {
            cl: self
        }
    }
}

impl<T: Any + Clone + PartialEq> PartialEq<Vec<T>> for CircularList<T> {
    fn eq(&self, other: &Vec<T>) -> bool {

        let mut vec = Vec::new();

        for el in other {
            vec.push(el.clone().to_owned())
        }

        self.list == vec
    }
}