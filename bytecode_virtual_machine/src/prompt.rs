use std::io;

#[allow(unused_imports)]
use crossterm::{
    self,
    cursor::{
        self, MoveDown, MoveLeft, MoveRight, MoveTo, MoveToColumn, MoveToNextLine,
        MoveToPreviousLine, MoveToRow, MoveUp,
    },
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    execute,
    style::Print,
    terminal::{self, ScrollUp, Clear, ClearType},
    Command,
};

pub struct Prompt {
    prompt: String,
    indent: String,
    history: Vec<String>,
}

impl Prompt {
    pub fn new(prompt: &str) -> Self {
        println!("Press enter twice to validate.");
        let mut indent = " ".repeat(prompt.len() - 2);
        indent.push('|');
        indent.push(' ');
        Prompt {
            prompt: prompt.to_string(),
            indent,
            history: Vec::new(),
        }
    }

    pub fn ask(&mut self) -> io::Result<String> {
        let mut stdout = io::stdout();
        let mut buffer = String::new();
        let mut prev: KeyCode = KeyCode::Null;


        terminal::enable_raw_mode().unwrap();

        execute!(stdout, Print(&self.prompt))?;

        let (left_, top_) = cursor::position()?; 
        let (left, mut top) = (left_ as usize, top_ as usize);

        macro_rules! x {
            () => {cursor::position()?.0 as usize - left}
        }
        macro_rules! y {
            () => {cursor::position()?.1 as usize - top}
        }
        
        let mut line_ends = vec![0];

        macro_rules! line_end {
            () => {line_ends[y!()]}
        }

        while let Event::Key(KeyEvent { code, modifiers }) = event::read()? {
            match (code, modifiers) {
                (KeyCode::Enter, _) => {
                    if let KeyCode::Enter = prev {
                        execute!(stdout, MoveToColumn(0), Clear(ClearType::UntilNewLine))?;
                        break;
                    } else {
                        if y!() == line_ends.len() - 1 {
                            if cursor::position()?.1 + 1 == terminal::size()?.1 {
                                execute!(stdout, ScrollUp(1), MoveToNextLine(1), Print(&self.indent))?;
                                top -= 1;
                            } else {
                                execute!(stdout, MoveToNextLine(1), Print(&self.indent))?; 
                            }
                            buffer.push('\n');
                            line_ends.push(x!());
                        } else {
                            //TODO: shift everything down one row
                        }
                    }
                }
                //(KeyCode::Left, _) => {
                //    if x!() > 0 {
                //        execute!(stdout, MoveLeft(1))?;
                //    }
                //}
                //(KeyCode::Right, _) => {
                //    if x!() < line_end!() {
                //        execute!(stdout, MoveRight(1))?;
                //    }
                //}
                (KeyCode::Tab, _) => {
                    if x!() == line_end!() {
                        buffer.push('\t');
                        execute!(stdout, Print("\t"))?;
                        line_end!() = x!();
                    }
                }
                (KeyCode::Backspace, _) => {
                    if buffer.len() > 0 {
                        buffer.pop();
                    }
                    if x!() > 0 {
                        execute!(stdout, MoveLeft(1), Print(" "), MoveLeft(1))?;
                        line_end!() -= 1;
                    }
                }
                //(KeyCode::Delete, _) => {
                //    if 0 < x!() && x!() <= line_end!() {
                //        execute!(stdout,Print(" "), MoveLeft(1))?;
                //        line_end!() -= 1;
                //        //TODO: shift the rest of the line to the left and remove char at right
                //        //index
                //    }
                //}
                (KeyCode::Char(c), _) => {
                    if x!() == line_end!() {
                        buffer.push(c);
                        line_end!() += 1;
                        execute!(stdout, Print(c))?;
                    } else {
                        //TODO: shift the rest of the line to the right and insert char into buffer
                        //at right place
                    }
                }
                _ => {}
            }
            prev = code;
        }

        terminal::disable_raw_mode().unwrap();

        self.history.push(buffer.clone());
        Ok(buffer)
    }
}
