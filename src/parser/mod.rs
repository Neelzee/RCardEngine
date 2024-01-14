#[derive(Debug, Clone)]
pub struct ASTNode {
    token: String,
    /// Line, Char
    pos: (u32, u32),
    next: Option<Box<ASTNode>>,
    prev: Option<Box<ASTNode>>,
}

impl ASTNode {
    pub fn new(token: String, line: u32, char: u32, next: Option<Box<ASTNode>>) -> Self {
        ASTNode {
            token,
            pos: (line, char),
            next,
            prev: None,
        }
    }
}

pub fn connect_nodes(mut node: ASTNode) -> ASTNode {
    let mut prev_node: Option<Box<ASTNode>> = None;

    while let Some(mut current_node) = node.next.take() {
        current_node.prev = Some(Box::new(node.clone()));

        node.next = Some(current_node);

        prev_node = Some(Box::new(node));

        node = *prev_node.take().unwrap();
    }

    node
}

pub fn parse(line: String, line_pos: u32) -> Option<Box<ASTNode>> {
    let mut token = String::new();
    let mut c: u32 = 0;
    for char in line.chars() {
        c += 1;
        if char == ' ' {
            let (_, rem_line) = line.split_at(c as usize);

            return Some(Box::new(ASTNode::new(
                token,
                line_pos,
                c,
                parse(rem_line.to_string(), line_pos),
            )));
        } else {
            token.push(char);
        }
    }

    if token.is_empty() {
        return None;
    }

    return Some(Box::new(ASTNode::new(token, line_pos, c, None)));
}
