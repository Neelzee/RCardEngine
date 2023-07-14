#[cfg(test)]
mod test {

    use RCardEngine::CircularList::circular_list::*;

    #[test]
    fn test_create_circular_list() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        

    }

    #[test]
    fn test_insert() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_l(i);
            match c.focus() {
                Some(j) => assert_eq!(i, j.to_owned()),

                None => panic!("Got None, when Some was expected"),
            }
        }
    }

    #[test]
    fn test_rotate_r() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_l(i);
        }


        for i in 9..0 {
            c.rot_r();

            match c.focus() {
                Some(j) => assert_eq!(i, j.to_owned()),

                None => panic!("Expected {:?} but got None", i),
            }
        }
    }

    #[test]
    fn test_rotate_l() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_l(i);
        }

        println!("{:?}", c);
        
        for i in vec![7, 6, 5, 4, 3, 2, 1, 0, 8, 7] {
            c.rot_l();
            println!("{:?}", c);
            
            match c.focus() {
                Some(j) => assert_eq!(i, j.to_owned()),

                None => panic!("Expected {:?} but got None", i),
            }
        }
    }


    #[test]
    fn test_insert_r() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_r(i);
            println!("{:?}", c);
        }
    }

    #[test]
    fn test_insert_l() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_l(i);
            match c.focus() {
                Some(j) => assert_eq!(i, j.to_owned()),

                None => panic!("Got None, when Some was expected"),
            }
        }
    }

    #[test]
    fn test_iterator() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_l(i);
        }

        let mut i = 7;
        for el in c {
            assert_eq!(i, el);
            i -= 1;

            if i >= -1 {
                break;
            }
        }

        i = 0;

        c = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_l(i);
        }

        let s = c.len() as i32;

        c.reverse_rotation();

        for el in c {
            assert_eq!(i, el);
            i += 1;

            if i >= s {
                break;
            }
        }
    }

    #[test]
    fn test_update() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_l(i); 
        }

        let i = 0;

        let mut j = 7;

        loop {
            c.rot();

            match c.focus() {
                Some(el) => assert_eq!(j, el),

                None => panic!("Expected {:?} but got None", j)
            }

            c.update(i);

            match c.focus() {
                Some(el) => assert_eq!(i, el),
                None => panic!("Expected {:?}, but got None instead", i),
            }

            j -= 1;

            if j >= -1 {
                break;
            }
        }
    }

    #[test]
    fn test_remove_l() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        assert!(c.is_empty());
        
        for i in 0..9 {
            c.insert_l(i); 
        }
        
        
        assert!(!c.is_empty());
        
        for j in vec![8, 7, 6, 5, 4, 3, 2, 1, 0] {
            let k = c.remove_l();
            
            assert_eq!(j, k);
        }

        assert!(c.is_empty());
    }

    #[test]
    fn test_remove_r() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();

        assert!(c.is_empty());
        
        for i in 0..9 {
            c.insert_l(i); 
        }
        
        
        assert!(!c.is_empty());
        
        for j in vec![8, 7, 6, 5, 4, 3, 2, 1, 0] {
            let k = c.remove_r();
            
            assert_eq!(j, k);
        }

        assert!(c.is_empty());
    }

    #[test]
    fn test_order() {
        let mut c: CircularList<i32> = CircularList::new::<i32>();
        let mut a: CircularList<i32> = CircularList::new::<i32>();

        for i in 0..9 {
            c.insert_l(i); 
            a.insert_r(i); 
        }
        
        println!("c: {:?}", c);
        println!("a: {:?}", a);
        let v: Vec<i32> = vec![0, 1, 2, 3, 4, 5, 6, 7, 8];
        assert_eq!(c, v);
    }

}