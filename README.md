# transforms
A number of transforms to lisp syntax, mostly infix.
I'm just calling them transforms for now, syntax extensions might be a more accurate term.

This process occurs before macroexpansion, so everything looks as it was before by the time it arrives at the macro.
Yes, this clobbers certain variables, until I get lexical variable analysis up and running.

Currently only works on sbcl, might port to other implementations if the need arises.

The ones marked (todo) are unimplemented, just started on this project. Ideas for new transforms/syntax extensions more than welcome.

## usage
`enable-transforms [transforms] transform-name(s)`

## transforms

### right-arrow-lambda

```cl
(mapcar (x y -> (+ x y)) '(1 2 3) '(4 5 6)) => (5 7 9)
```

### left-arrow-bind (todo)

```cl
(and (bar <- (really-long-cond foo)) 
     (qux <- (other-really-long-cond baz))
     (+ foo bar))
   
(defun foo (bar baz)
   (qux <- (apply baz bar))
   (list qux bar))

(if (> 6 (value <- (gethash key table))) 
   (print value) 
   (error "whoops"))
```

### underscore-lambda and hole-lambda

```cl
(map (+ 10 _) '(1 2 3)) => (11 12 13)

(reduce #'max list :key (gethash <> table))
```

### simple-infix (todo)

Just arithmetic infix transforms. Normal operator precedence.
More than one element in isolation means function application.

```cl
(4 + 5)

(let ((foo (bar / baz)))
      (foo + bar - baz))

(foo + (bar - baz))

(foo - ((bar qux) + baz))
```

### full-infix (todo)

Flow control and access infix expressions.
Some concepts left unimplemented, because they'd be done better with full arity, type, lexical variable, and whitespace information.

```cl
(x = (array !! n))

(0 < x < 1)

((0 < x < length) && (0 < y < length))
```

### haskell-type-decleration (todo)

Also usable with a variable in the left side, and a single type.

```cl
(fibo :: integer -> integer)
(defun fibo (n)
   (if (n < 2)
       n
       (fibo (n - 2) + fibo (n - 1))))

```

## dependencies and installation

Depends on two of my own libraries not in quicklisp, provide-toplevel and destructuring-match, both of which can just
be cloned into local-projects.

This project requires quicklisp to run.
To install quicklisp, head over to [quicklisp's website](https://www.quicklisp.org/beta/) and follow 
the instructions there. Make sure you run `(ql:add-to-init-file)`, otherwise quicklisp won't be avaliable 
when you start your interpreter.

To use it, clone this repo into `~/quicklisp/local-projects`, and run `(ql:quickload 'provide-toplevel)`.
