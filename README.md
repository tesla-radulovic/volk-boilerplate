# volk-boilerplate
Prototype Indent-based Scheme implementation written in Racket with Interslavic Keywords

Syntax based on Cirru: http://text.cirru.org/

Example (cheking if a given integer is prime): 
```code
izznaczi
  prosty? n
  povratny-nehaj
      testuj
        lambda
          n i
          ako
            >= i n
            , istinny
            ako
              =
                modul n i
                , 0
              , lozsny
              testuj n $ + i 1
    testuj n 2
```
REPL Usage:
```code
prosty? 10
#f
prosty? 97
#t```
