# Run Method
- run a file via piping:   `scheme < file.scm`
- run with quiet mode:    `scheme --quiet < file.scm`
- Type `scheme` in the specifc directory of terminal, then get 'scheme' environment. Input `(load "foo.scm")` to load file.
# Debug Method
In No.3 of run method, 
- Type `(debug)` in terminal after running the code.
- Insert `(display specific-variable) (newline)` in procedure.