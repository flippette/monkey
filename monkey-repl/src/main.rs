use std::{
    io::{self, BufRead, BufReader, Write},
    process,
};

use monkey_lex::lexer::Lexer;

fn main() -> io::Result<()> {
    let mut stdout = io::stdout().lock();
    let mut stdin = BufReader::new(io::stdin().lock());

    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;

        let mut src = String::new();
        if stdin.read_line(&mut src)? == 0 {
            writeln!(stdout, "goodbye.")?;
            process::exit(0);
        }
        let lexer = Lexer::new(&src);

        for tok in lexer {
            writeln!(stdout, "    {tok:?}")?;
        }

        stdout.flush()?;
    }
}
