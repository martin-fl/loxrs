- VM
	- the stack is behind a RefCell and the stack top is behind Cell
	  design issue much?
- Chunk
	- meh

- Compiler
	- Error reporting wtf?
	- Parser needs access to Compiler::emit_\* functions, but is itself 
	  a member of the compiler
	
