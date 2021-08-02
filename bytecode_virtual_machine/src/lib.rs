#![feature(box_patterns)]

pub mod chunk;
pub mod compiler;
pub mod error;
pub mod lexer;
pub mod prompt;
pub mod value;
pub mod vm;

const DEBUG: bool = true;

#[macro_export]
macro_rules! define_enum {
    ($name:ident, $($variant:ident = $byte:expr,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        #[repr(u8)]
        pub enum $name {
            $($variant = $byte,)*
        }

        impl From<u8> for $name {
            fn from(byte: u8) -> Self {
                use $name::*;
                match byte {
                    $($byte => $variant,)*
                    _ => panic!("Unknown OpCode"),
                }
            }
        }
    };
}
