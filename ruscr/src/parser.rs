use crate::error::{Error, Result};
use crate::syntax::{Interaction, Message, Module, Protocol};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    Ident(String),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semi,
    At,
    Eof,
}

pub fn parse(input: &str) -> Result<Module> {
    let mut parser = Parser::new(input)?;
    parser.module()
}

struct Parser {
    tokens: Vec<Tok>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Result<Self> {
        Ok(Self {
            tokens: lex(input)?,
            pos: 0,
        })
    }

    fn module(&mut self) -> Result<Module> {
        if self.consume_ident("#") {
            return Err(Error::Unsupported("pragmas".to_string()));
        }
        let mut protocols = Vec::new();
        while !matches!(self.peek(), Tok::Eof) {
            if self.consume_ident("nested") {
                return Err(Error::Unsupported("nested protocols".to_string()));
            }
            protocols.push(self.protocol(false)?);
        }
        Ok(Module { protocols })
    }

    fn protocol(&mut self, global_already_consumed: bool) -> Result<Protocol> {
        if !global_already_consumed {
            let _ = self.consume_ident("global");
        }
        self.expect_ident_value("protocol")?;
        let name = self.expect_ident()?;
        self.expect(Tok::LParen)?;
        let mut roles = Vec::new();
        loop {
            self.expect_ident_value("role")?;
            roles.push(self.expect_ident()?);
            if !self.consume(Tok::Comma) {
                break;
            }
        }
        self.expect(Tok::RParen)?;
        self.expect(Tok::LBrace)?;
        let interactions = self.interactions_until_rbrace()?;
        Ok(Protocol {
            name,
            roles,
            interactions,
        })
    }

    fn interactions_until_rbrace(&mut self) -> Result<Vec<Interaction>> {
        let mut interactions = Vec::new();
        while !self.consume(Tok::RBrace) {
            if matches!(self.peek(), Tok::Eof) {
                return Err(Error::Parser(
                    "unexpected end of file in protocol body".to_string(),
                ));
            }
            interactions.push(self.interaction()?);
        }
        Ok(interactions)
    }

    fn block(&mut self) -> Result<Vec<Interaction>> {
        self.expect(Tok::LBrace)?;
        self.interactions_until_rbrace()
    }

    fn interaction(&mut self) -> Result<Interaction> {
        if self.consume_ident("calls") {
            return Err(Error::Unsupported("nested protocol calls".to_string()));
        }
        if self.consume_ident("rec") {
            let var = self.expect_ident()?;
            let body = self.block()?;
            return Ok(Interaction::Rec { var, body });
        }
        if self.consume_ident("continue") {
            let var = self.expect_ident()?;
            self.skip_bracket_exprs()?;
            self.expect(Tok::Semi)?;
            return Ok(Interaction::Continue(var));
        }
        if self.consume_ident("choice") {
            self.expect_ident_value("at")?;
            let at = self.expect_ident()?;
            let mut branches = vec![self.block()?];
            while self.consume_ident("or") {
                branches.push(self.block()?);
            }
            return Ok(Interaction::Choice { at, branches });
        }
        if self.consume_ident("do") {
            let protocol = self.expect_ident()?;
            let roles = if self.consume(Tok::LParen) {
                self.ident_list_until_rparen()?
            } else {
                Vec::new()
            };
            self.expect(Tok::Semi)?;
            self.reject_annotation()?;
            return Ok(Interaction::Do { protocol, roles });
        }
        if self.consume_ident("aux") || self.consume_ident("global") {
            return Err(Error::Unsupported(
                "protocol declarations inside protocol bodies".to_string(),
            ));
        }

        let label = self.expect_ident()?;
        if self.consume_ident("calls") {
            return Err(Error::Unsupported("nested protocol calls".to_string()));
        }
        let payload = if self.consume(Tok::LParen) {
            self.payload_list_until_rparen()?
        } else {
            Vec::new()
        };
        self.expect_ident_value("from")?;
        let from = self.expect_ident()?;
        self.expect_ident_value("to")?;
        let mut to = vec![self.expect_ident()?];
        while self.consume(Tok::Comma) {
            to.push(self.expect_ident()?);
        }
        self.expect(Tok::Semi)?;
        self.reject_annotation()?;
        Ok(Interaction::Message {
            message: Message { label, payload },
            from,
            to,
        })
    }

    fn payload_list_until_rparen(&mut self) -> Result<Vec<String>> {
        let mut payloads = Vec::new();
        if self.consume(Tok::RParen) {
            return Ok(payloads);
        }
        loop {
            let first = self.expect_ident()?;
            let payload = if self.consume(Tok::At) {
                let role = self.expect_ident()?;
                format!("{}@{}", first, role)
            } else if self.consume_colon_like_payload() {
                self.expect_ident()?
            } else {
                first
            };
            payloads.push(payload);
            if self.consume(Tok::RParen) {
                break;
            }
            self.expect(Tok::Comma)?;
        }
        Ok(payloads)
    }

    fn ident_list_until_rparen(&mut self) -> Result<Vec<String>> {
        let mut values = Vec::new();
        if self.consume(Tok::RParen) {
            return Ok(values);
        }
        loop {
            values.push(self.expect_ident()?);
            if self.consume(Tok::RParen) {
                break;
            }
            self.expect(Tok::Comma)?;
        }
        Ok(values)
    }

    fn skip_bracket_exprs(&mut self) -> Result<()> {
        Ok(())
    }

    fn reject_annotation(&mut self) -> Result<()> {
        if self.consume(Tok::At) {
            return Err(Error::Unsupported("annotations".to_string()));
        }
        Ok(())
    }

    fn consume_colon_like_payload(&mut self) -> bool {
        false
    }

    fn expect_ident_value(&mut self, expected: &str) -> Result<()> {
        match self.next() {
            Tok::Ident(s) if s == expected => Ok(()),
            other => Err(Error::Parser(format!(
                "expected `{}`, got {:?}",
                expected, other
            ))),
        }
    }

    fn expect_ident(&mut self) -> Result<String> {
        match self.next() {
            Tok::Ident(s) => Ok(s),
            other => Err(Error::Parser(format!(
                "expected identifier, got {:?}",
                other
            ))),
        }
    }

    fn consume_ident(&mut self, expected: &str) -> bool {
        match self.peek() {
            Tok::Ident(s) if s == expected => {
                self.pos += 1;
                true
            }
            _ => false,
        }
    }

    fn expect(&mut self, expected: Tok) -> Result<()> {
        let actual = self.next();
        if std::mem::discriminant(&actual) == std::mem::discriminant(&expected) {
            Ok(())
        } else {
            Err(Error::Parser(format!(
                "expected {:?}, got {:?}",
                expected, actual
            )))
        }
    }

    fn consume(&mut self, expected: Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn peek(&self) -> &Tok {
        self.tokens.get(self.pos).unwrap_or(&Tok::Eof)
    }

    fn next(&mut self) -> Tok {
        let tok = self.tokens.get(self.pos).cloned().unwrap_or(Tok::Eof);
        self.pos += 1;
        tok
    }
}

fn lex(input: &str) -> Result<Vec<Tok>> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            c if c.is_whitespace() => {}
            '(' if chars.peek() == Some(&'*') => {
                chars.next();
                if chars.peek() == Some(&'#') {
                    return Err(Error::Unsupported("pragmas".to_string()));
                }
                let mut prev = '\0';
                let mut closed = false;
                for c in chars.by_ref() {
                    if prev == '*' && c == ')' {
                        closed = true;
                        break;
                    }
                    prev = c;
                }
                if !closed {
                    return Err(Error::Lexer("unterminated comment".to_string()));
                }
            }
            '(' => tokens.push(Tok::LParen),
            ')' => tokens.push(Tok::RParen),
            '{' => tokens.push(Tok::LBrace),
            '}' => tokens.push(Tok::RBrace),
            ',' => tokens.push(Tok::Comma),
            ';' => tokens.push(Tok::Semi),
            '@' => tokens.push(Tok::At),
            ':' | '[' | ']' | '=' | '<' | '>' | '+' | '-' | '!' | '&' | '|' => {
                return Err(Error::Unsupported("refinement types".to_string()));
            }
            c if is_ident_start(c) => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(next) = chars.peek().copied() {
                    if is_ident_continue(next) {
                        ident.push(next);
                        chars.next();
                    } else {
                        break;
                    }
                }
                tokens.push(Tok::Ident(ident));
            }
            c => return Err(Error::Lexer(format!("unexpected character `{}`", c))),
        }
    }
    tokens.push(Tok::Eof);
    Ok(tokens)
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || c.is_ascii_digit()
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '.'
}
