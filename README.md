# conneries

Conneries programming language

## Installation

```bash
lein uberjar
```

## Usage
```
    $ java -jar conneries-0.1.0-pre-alpha.jar <file>
```
## Examples

```lisp
(do
(define! var 2)
(print (+ 1 var))
(set! var 3)
(if (= var 3)
    (print "set works!")
    (print "set doesn't works")))
```

## License

Copyright © 2018 LEONID

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
