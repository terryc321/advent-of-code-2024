
;; load this first 
;; start slime 
(require "asdf")
(require "uiop")



(pushnew (uiop:getcwd) asdf:*central-registry*)
(asdf:oos 'asdf:load-op :day5)
(in-package #:aoc)





