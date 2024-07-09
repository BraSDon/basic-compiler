use std::fs::File;
use std::io::Write;

pub struct Emitter {
    path: String,
    code: String,
    header: String,
}

impl Emitter {
    pub fn new(path: String) -> Self {
        Self {
            path,
            code: String::new(),
            header: String::new(),
        }
    }

    pub fn emit(&mut self, code: &str) {
        self.code.push_str(&code);
    }

    pub fn emit_line(&mut self, code: &str) {
        self.code.push_str(&code);
        self.code.push('\n');
    }

    pub fn header_line(&mut self, code: &str) {
        self.header.push_str(&code);
        self.header.push('\n');
    }

    pub fn write(&self) -> Result<(), std::io::Error> {
        let mut file = File::create(&self.path)?;
        file.write_all(self.header.as_bytes())?;
        file.write_all(self.code.as_bytes())?;
        Ok(())
    }
}
