; ((lambda (a) (a b)) c)
;                                  app-exp
;                                  /     \
;    lambda-exp{(lambda (a) (a b))}      var-exp{c}
;            /              \               |
; bound-var{a}           body{(a b)}        c
;        |                    |
;        a                 app-exp{a b}
;                         /           \
;                var-exp{a}            var-exp{b}
;                    |                    |
;                    a                    b

; (lambda (x)
;   (lambda (y)
;     ((lambda (x)
;        (x y))
;      x)))
 
;                          lambda-exp
;                          /        \
;                    bound-var     body
;                        |           |
;                        x       lambda-exp
;                                /        \
;                        bound-var         body
;                             |             |
;                             y           app-exp
;                                        /       \
;                              lambda-exp         var-exp
;                              /        \            |
;                         bound          body        x
;                           |             |
;                           x           app-exp
;                                       /      \
;                                  var-exp   var-exp
;                                     |         |
;                                     x         y