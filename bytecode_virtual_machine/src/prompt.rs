use std::io;

#[allow(unused_imports)]
use crossterm::{
    self,
    cursor::{
        self, CursorShape, MoveDown, MoveLeft, MoveRight, MoveTo, MoveToColumn, MoveToNextLine,
        MoveToPreviousLine, MoveToRow, MoveUp, RestorePosition, SavePosition, SetCursorShape,
    },
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    execute,
    style::Print,
    terminal::{self, Clear, ClearType, ScrollUp},
    Command,
};

pub struct Prompt {
    prompt: String,
    indent: String,
    history: Vec<String>,
}

impl Prompt {
    pub fn new(prompt: &str) -> Self {
        println!("Press enter twice to validate and Ctrl+C to exit.");
        let mut indent = " ".repeat(prompt.len() - 2);
        indent.push('|');
        indent.push(' ');
        Prompt {
            prompt: prompt.to_string(),
            indent,
            history: Vec::new(),
        }
    }

    pub fn ask(&mut self) -> io::Result<Option<String>> {
        let mut stdout = io::stdout();
        let mut buffer = String::new();
        let mut prev: KeyCode = KeyCode::Null;

        terminal::enable_raw_mode().unwrap();

        execute!(
            stdout,
            Print(&self.prompt),
            SetCursorShape(CursorShape::Line)
        )?;

        let (left_, top_) = cursor::position()?;
        let (left, mut top) = (left_ as usize, top_ as usize);

        macro_rules! x {
            () => {
                cursor::position()?.0 as usize - left
            };
        }
        macro_rules! y {
            () => {
                cursor::position()?.1 as usize - top
            };
        }

        let mut line_ends = vec![0];

        macro_rules! line_end {
            () => {
                line_ends[y!()]
            };
        }

        let mut write_head = buffer.len();

        while let Event::Key(KeyEvent { code, modifiers }) = event::read()? {
            match (code, modifiers) {
                (KeyCode::Char('c'), KeyModifiers::CONTROL) => {
                    execute!(stdout, SetCursorShape(CursorShape::Block))?;
                    terminal::disable_raw_mode().unwrap();
                    return Ok(None);
                }
                (KeyCode::Enter, _) => {
                    if let KeyCode::Enter = prev {
                        execute!(stdout, MoveToColumn(0), Clear(ClearType::UntilNewLine))?;
                        break;
                    } else {
                        if y!() == line_ends.len() - 1 {
                            if cursor::position()?.1 + 1 == terminal::size()?.1 {
                                execute!(
                                    stdout,
                                    ScrollUp(1),
                                    MoveToNextLine(1),
                                    Print(&self.indent)
                                )?;
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
                (KeyCode::Left, _) => {
                    if x!() > 0 {
                        execute!(stdout, MoveLeft(1))?;
                        write_head -= 1;
                    }
                }
                (KeyCode::Right, _) => {
                    if x!() < line_end!() {
                        execute!(stdout, MoveRight(1))?;
                        write_head += 1;
                    }
                }
                (KeyCode::Tab, _) => {
                    if x!() == line_end!() {
                        buffer.push('\t');
                        execute!(stdout, Print("\t"))?;
                        write_head += 1;
                        line_end!() = x!();
                    }
                }
                (KeyCode::Backspace, _) => {
                    if buffer.len() > 0 {
                        if x!() == line_end!() {
                            buffer.pop();
                            write_head -= 1;
                            line_end!() -= 1;
                        } else if x!() > 0 && write_head > 0 {
                            buffer.remove(write_head - 1);
                            write_head -= 1;
                            line_end!() -= 1;
                        }
                    }
                    if x!() > 0 {
                        execute!(
                            stdout,
                            MoveLeft(1),
                            SavePosition,
                            Clear(ClearType::FromCursorDown),
                            Print(&buffer[write_head..]),
                            RestorePosition
                        )?;
                    }
                }
                (KeyCode::Delete, _) => {
                    if buffer.len() > 0 {
                        if x!() == line_end!() {
                            buffer.pop();
                            line_end!() -= 1;
                        } else if x!() < line_end!() {
                            buffer.remove(write_head);
                            line_end!() -= 1;
                        }
                        execute!(
                            stdout,
                            SavePosition,
                            Clear(ClearType::FromCursorDown),
                            Print(&buffer[write_head..]),
                            RestorePosition
                        )?;
                    }
                }
                (KeyCode::Char(c), _) => {
                    if x!() == line_end!() {
                        buffer.push(c);
                        line_end!() += 1;
                        write_head += 1;
                        execute!(stdout, Print(c))?;
                    } else {
                        buffer.insert(write_head, c);
                        execute!(
                            stdout,
                            SavePosition,
                            Clear(ClearType::FromCursorDown),
                            Print(&buffer[write_head..]),
                            RestorePosition,
                            MoveRight(1)
                        )?;
                        line_end!() += 1;
                        write_head += 1;
                    }
                }
                _ => {}
            }
            prev = code;
        }

        terminal::disable_raw_mode().unwrap();
        println!("{}", &buffer);
        self.history.push(buffer.clone());
        Ok(Some(buffer))
    }
}
