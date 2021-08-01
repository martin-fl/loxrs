use std::fmt;

#[derive(Debug, Clone)]
pub struct Error {
    content: String,
    message: String,
    help: Option<String>,
    line: usize,
    span: Option<(usize, usize)>, // (start, len)
                                  // file_name: String,
}

impl Error {
    pub fn new(content: &str, message: &str, line: usize) -> Self {
        Self {
            content: content.to_string(),
            message: message.to_string(),
            help: None,
            line,
            span: None,
        }
    }

    pub fn with_help(mut self, help: &str) -> Self {
        self.help = Some(help.to_string());
        self
    }

    pub fn with_span(mut self, start: usize, end: usize) -> Self {
        self.span = Some((start, end));
        self
    }

    pub fn with_content(mut self, content: &str) -> Self {
        self.content = content.to_string();
        self
    }

    pub fn at_line(mut self, line: usize) -> Self {
        self.line = line;
        self
    }
}

pub trait EmitError {
    fn emit_error(&self, message: &str) -> Error;
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "error: {}", self.message)?;
        let indent = self.line.to_string().len() + 1;
        writeln!(f, "{:width$}|", "", width = indent)?;
        writeln!(f, "{:width$}| {}", self.line, self.content, width = indent)?;
        if let Some((start, len)) = self.span {
            writeln!(
                f,
                "{:width$}| {:<start$}{:^<len$}",
                "",
                "",
                "",
                start = start,
                len = len,
                width = indent
            )?;
        } else {
            writeln!(f, "{:width$}|", "", width = indent)?;
        }
        writeln!(f, "{:width$}|", "", width = indent)?;
        if let Some(help) = &self.help {
            writeln!(f, "{:width$} = help: {}", help, width = indent)?;
        }
        Ok(())
    }
}
