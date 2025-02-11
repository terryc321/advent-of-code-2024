

(require-library chicken-doc)


#|



#;5> ,doc (scheme define)

-- syntax: (define <variable> <expression>)
-- syntax: (define (<variable> <formals>) <body>)

<Formals> should be either a sequence of zero or more variables, or a
sequence of one or more variables followed by a space-delimited period and
another variable (as in a lambda expression). This form is equivalent to

  (define <variable>
    (lambda (<formals>) <body>)).
#;10> ,doc (scheme lambda)

-- syntax: (lambda <formals> <body>)

Syntax: <Formals> should be a formal arguments list as described below, and
<body> should be a sequence of one or more expressions.

Semantics: A lambda expression evaluates to a procedure. The environment
in effect when the lambda expression was evaluated is remembered as part
of the procedure. When the procedure is later called with some actual
arguments, the environment in which the lambda expression was evaluated
will be extended by binding the variables in the formal argument list to
fresh locations, the corresponding actual argument values will be stored in
those locations, and the expressions in the body of the lambda expression
will be evaluated sequentially in the extended environment. The result(s)
of the last expression in the body will be returned as the result(s) of the
procedure call.

  (lambda (x) (+ x x))              ===>  a procedure
  ((lambda (x) (+ x x)) 4)          ===>  8
  
  (define reverse-subtract
    (lambda (x y) (- y x)))
  (reverse-subtract 7 10)           ===>  3
  
  (define add4
    (let ((x 4))
      (lambda (y) (+ x y))))
  (add4 6)                          ===>  10

<Formals> should have one of the following forms:

  * (<variable[1]> ...): The procedure takes a fixed number of arguments;
    when the procedure is called, the arguments will be stored in the
    bindings of the corresponding variables.
  * <variable>: The procedure takes any number of arguments; when the
    procedure is called, the sequence of actual arguments is converted into
    a newly allocated list, and the list is stored in the binding of the
    <variable>.
  * (<variable[1]> ... <variable[n]> . <variable[n+1]>): If a
    space-delimited period precedes the last variable, then the procedure
    takes n or more arguments, where n is the number of formal arguments
    before the period (there must be at least one). The value stored in
    the binding of the last variable will be a newly allocated list of the
    actual arguments left over after all the other actual arguments have
    been matched up against the other formal arguments.

It is an error for a <variable> to appear more than once in <formals>.

  ((lambda x x) 3 4 5 6)                  ===>  (3 4 5 6)
  ((lambda (x y . z) z)
   3 4 5 6)                               ===>  (5 6)

Each procedure created as the result of evaluating a lambda expression is
(conceptually) tagged with a storage location, in order to make eqv? and
eq? work on procedures.

As an extension to R5RS, CHICKEN also supports "extended" DSSSL style
parameter lists, which allows embedded special keywords. Such a keyword
gives a special meaning to the `<formal>` it precedes. DSSSL parameter
lists are defined by the following grammar:

  <parameter-list> ==> <required-parameter>*
                       [#!optional <optional-parameter>*]
                       [#!rest <rest-parameter>]
                       [#!key <keyword-parameter>*]
  <required-parameter> ==> <ident>
  <optional-parameter> ==> <ident>
                           | (<ident> <initializer>)
  <rest-parameter> ==> <ident>
  <keyword-parameter> ==> <ident>
                          | (<ident> <initializer>)
  <initializer> ==> <expr>

When a procedure is applied to a list of arguments, the parameters and
arguments are processed from left to right as follows:

  * Required-parameters are bound to successive arguments starting with the
    first argument. It shall be an error if there are fewer arguments than
    required-parameters.
  * Next, the optional-parameters are bound with the remaining arguments.
    If there are fewer arguments than optional-parameters, then the
    remaining optional-parameters are bound to the result of the evaluation
    of their corresponding <initializer>, if one was specified, otherwise
    `#f`. The corresponding <initializer> is evaluated in an environment in
    which all previous parameters have been bound.
  * If there is a rest-parameter, then it is bound to a list containing
    all the remaining arguments left over after the argument bindings with
    required-parameters and optional-parameters have been made.
  * If `#!key` was specified in the parameter-list, there should be an
    even number of remaining arguments. These are interpreted as a series
    of pairs, where the first member of each pair is a keyword specifying
    the parameter name, and the second member is the corresponding value.
    If the same keyword occurs more than once in the list of arguments,
    then the corresponding value of the first keyword is the binding value.
    If there is no argument for a particular keyword-parameter, then the
    variable is bound to the result of evaluating <initializer>, if one was
    specified, otherwise `#f`. The corresponding <initializer> is evaluated
    in an environment in which all previous parameters have been bound.

Needing a special mention is the close relationship between the
rest-parameter and possible keyword-parameters. Declaring a rest-parameter
binds up all remaining arguments in a list, as described above. These
same remaining arguments are also used for attempted matches with
declared keyword-parameters, as described above, in which case a matching
keyword-parameter binds to the corresponding value argument at the same
time that both the keyword and value arguments are added to the rest
parameter list. Note that for efficiency reasons, the keyword-parameter
matching does nothing more than simply attempt to match with pairs that
may exist in the remaining arguments. Extra arguments that don't match
are simply unused and forgotten if no rest-parameter has been declared.
Because of this, the caller of a procedure containing one or more
keyword-parameters cannot rely on any kind of system error to report wrong
keywords being passed in.

It shall be an error for an `<ident>` to appear more than once in a
parameter-list.

If there is no rest-parameter and no keyword-parameters in the
parameter-list, then it shall be an error for any extra arguments to be
passed to the procedure.

Example:

  ((lambda x x) 3 4 5 6)       => (3 4 5 6)
  ((lambda (x y #!rest z) z)
   3 4 5 6)                    => (5 6)
  ((lambda (x y #!optional z #!rest r #!key i (j 1)) 
      (list x y z i: i j: j))
   3 4 5 i: 6 i: 7)            => (3 4 5 i: 6 j: 1)
#;14>

|#
