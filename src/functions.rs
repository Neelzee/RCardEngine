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
