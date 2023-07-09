use std::{ops::Not, borrow::Cow};

pub struct CircularList<T> {
    list: Vec<T>,
    index: usize,
    rotation: Rotation
}

pub enum Rotation {
    Left,
    Right
}

impl Not for Rotation {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Rotation::Left => Rotation::Right,
            Rotation::Right => Rotation::Left,
        }
    }
}

impl<T: Clone> CircularList<T> {
    fn from_list<K, I>(list: I) -> CircularList<T>
    where
        I: IntoIterator<Item = T>
    {
        let mut v: Vec<T> = Vec::new();

        for el in list {
            v.push(el);
        }

        CircularList { list: v, index: 0, rotation: Rotation::Left }
    }

    fn update(mut self, new_focus: T) {
        self.list.remove(self.index);
        self.list.insert(self.index, new_focus);
    }

    pub fn focus(&mut self) -> Option<&T>
    where T: Clone
{
    let m = self.list.len();

    self.index %= m;

    self.list.get(self.index as usize)
}


    pub fn reverse_direction(mut self) {
        self.rotation = !self.rotation;
    }

    pub fn insert_l(&self, new_focus: T) {
        todo!()
    }

    pub fn insert_r(&self, new_focus: T) {
        todo!()
    }

    pub fn remove_l(&self) -> T {
        todo!()
    }

    pub fn remove_r(&self) -> T {
        todo!()
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn rot_r(&self) {
        todo!()
    }

    pub fn rot_l(&self) {
        todo!()
    }

}