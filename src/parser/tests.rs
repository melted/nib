#![cfg(test)] 


mod tests {
    use crate::ast::Name;
    use crate::common::Result;
    use crate::parser::helpers::NameOrOperator;
    use crate::parser::{lex, ParserState};
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

    #[test]
    fn parse_names() -> Result<()> {
        let mut state = ParserState::new("a.name");
        let ret = state.parse_name();
        assert!(ret.is_ok());
        match ret {
            Ok(NameOrOperator::Name(Name::Qualified(p, n))) => {
                assert_eq!(n, "name");
                assert_eq!(p.len(), 1);
                assert_eq!(p[0], "a");
            },
            _ => assert!(false)
        }
        Ok(())
    }
}