#![cfg(test)] 

mod tests {
    use crate::common::Result;
    use crate::parser::lex;
    use crate::parser::lexer::{ Token, TokenValue };

    #[test]
    fn lex_numbers() -> Result<()> {
        let tokens = lex("1 2 3")?;
        assert_eq!(tokens[0], TokenValue::Integer(1));
        assert_eq!(tokens[1], TokenValue::Integer(2));
        assert_eq!(tokens[2], TokenValue::Integer(3));
        Ok(())
    }

    #[test]
    fn lex_identifier() -> Result<()> {
        let tokens = lex("hello_world aaa bbb ccc")?;
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], TokenValue::Identifier("hello_world".to_string()));
        Ok(())
    }
}