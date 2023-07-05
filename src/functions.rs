pub fn string_to_list(s: &str) -> Vec<String> {
    let trimmed = s.trim();
    
    if trimmed.starts_with('[') && trimmed.ends_with(']') {
        let inner_str = &trimmed[1..trimmed.len() - 1];
        let mut result: Vec<String> = vec![];
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
                        result.push(word.clone());
                        word.clear();
                    }
                }
                ',' => {
                    if count == 0 {
                        result.push(word.clone());
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
        vec![s.to_string()]
    }
}
