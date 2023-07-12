#[cfg(test)]
mod test {
    use RCardEngine::CardGame::card::Card;

    #[test]
    fn test_create_card() {
        let c = Card::new("foo", "bar", 1);

        assert_eq!(1, 1);
    }
}