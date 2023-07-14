pub fn string_to_list(s: &str) -> Vec<&str> {
    let trimmed = s.trim();
    todo!()
    
    /*
    if trimmed.starts_with('[') && trimmed.ends_with(']') {
        let inner_str = &trimmed[1..trimmed.len() - 1];
        let mut result: Vec<&str> = vec![];
        let mut word = String::new();
        let mut count = 0;


        for ch in inner_str.chars() {
            match ch {
                '[' => {
                    count += 1;
                    word.push(ch);
                }
                ']' => {
                    count -= 1;
                    word.push(ch);
                    if count == 0 {
                        result.push(&word);
                        word.clear();
                    }
                }
                ',' => {
                    if count == 0 {
                        result.push(&word);
                        word.clear();
                    } else {
                        word.push(ch);
                    }
                }
                _ => word.push(ch),
            }
        }
        
        result
    } else {
        vec![s]
    }
         */
}


pub fn partition_option<T, K>(iter: K) -> Vec<T>
where
    K: IntoIterator<Item = Option<T>>
{
    let mut v: Vec<T> = Vec::new();
    for el in iter {
        match el {
            Some(e) => v.push(e),

            None => { }
        }
    }

    return v;
}

pub fn partition_result<T, J, K>(iter: K) -> (Vec<T>, Vec<J>)
where
    K: IntoIterator<Item = Result<T, J>>
{
    let mut ok: Vec<T> = Vec::new();
    let mut err: Vec<J> = Vec::new();

    for el in iter {
        match el {
            Ok(t) => ok.push(t),
            Err(j) => err.push(j),
        }
    }

    (ok, err)
}