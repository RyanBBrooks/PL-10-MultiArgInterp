#lang plait

(define-type Value
  (boolV [b : Boolean])
  (numV [n : Number])
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (pairV [f : Value]
         [s : Value]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (boolE [b : Boolean])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (equalE [l : Exp]
          [r : Exp])
  (pairE [f : Exp]
         [s : Exp])
  (fstE [p : Exp])
  (sndE [p : Exp])
  (ifE [c : Exp]
       [t : Exp]
       [e : Exp])
  (lamE [ns : (Listof Symbol)]
        [arg-types : (Listof Type)]
        [body : Exp])
  (appE [fun : Exp]
        [args : (Listof Exp)]))

(define-type Type
  (numT)
  (boolT)
  (starT [f : Type]
          [s : Type])
  (arrowT [args : (Listof Type)]
          [result : Type]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define-type Type-Binding
  (tbind [name : Symbol]
         [type : Type]))

(define-type-alias Type-Env (Listof Type-Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `true s) (boolE #t)]
    [(s-exp-match? `false s) (boolE #f)]
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{pair ANY ANY} s)
     (pairE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{fst ANY} s)
     (fstE (parse (second (s-exp->list s))))]
     [(s-exp-match? `{snd ANY} s)
     (sndE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{= ANY ANY} s)
     (equalE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (list (s-exp->symbol (first bs)))
                   (list (parse-type (third bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (fourth bs)))))]
    [(s-exp-match? `{lambda {[SYMBOL : ANY] ...} ANY} s)
     (let ([args (s-exp->list
                         (second (s-exp->list s)))])
       (lamE (map parse-sym-arg args)
             (map parse-type-arg args)
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-sym-arg [arg : S-Exp]) : Symbol
  (s-exp->symbol (first (s-exp->list arg))))

(define (parse-type-arg [arg : S-Exp]) : Type
  (parse-type (third (s-exp->list arg))))
  

(define (parse-type [s : S-Exp]) : Type
  (cond
   [(s-exp-match? `num s) 
    (numT)]
   [(s-exp-match? `bool s)
    (boolT)]
   [(s-exp-match? `(ANY ... -> ANY) s)
    (arrowT (map parse-type (reverse (rest (rest (reverse (s-exp->list s))))))
            (parse-type (first (reverse (s-exp->list s)))))]
   [(s-exp-match? `(ANY * ANY) s)
    (starT (parse-type (first (s-exp->list s)))
           (parse-type (third (s-exp->list s))))]
   [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x : num {+ 1 2}]}
                  y})
        (appE (lamE (list 'x) (list (numT)) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {[x : num]} 9})
        (lamE (list 'x) (list (numT)) (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))
  (test/exn (parse `{})
            "invalid input")

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (list (numT)) (boolT)))
  (test (parse-type `(num * bool))
        (starT (numT) (boolT)))
  (test/exn (parse-type `1)
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(boolE b) (boolV b)]
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(lamE ns ts body)
     (closV ns body env)]

    [(pairE f s) (pairV (interp f env) (interp s env))]

    [(fstE p) (type-case Value (interp p env)
                [(pairV f s) f]
                [else (error 'interp "not a pair")])]

    [(sndE p) (type-case Value (interp p env)
                [(pairV f s) s]
                [else (error 'interp "not a pair")])]
    
    [(ifE c t e) (type-case Value (interp c env)
                   [(boolV b) (if b
                                  (interp t env)
                                  (interp e env))]
                   [else (error 'interp "not a boolean")])]

    [(equalE l r) (if (equal? (interp l env) (interp r env))
                      (boolV #t)
                      (boolV #f))]
    
    [(appE fun args) (type-case Value (interp fun env)
                      [(closV ns body c-env)
                       (interp body
                               (extend-env-multi
                                (map2 bind ns
                                      (map2 interp args (build-list (length args) (lambda (dummy) env))))
                                c-env))]
                      [else (error 'interp "not a function")])]))

(define (extend-env-multi [bs : (Listof Binding)] [env : Env]) : Env
  (if (empty? bs)
      env
      (extend-env-multi
       (rest bs)
       (extend-env (first bs) env))))


(module+ test
  (test (interp (parse `true) mt-env)
        (boolV #t))
  (test (interp (parse `{fst {pair 1 2}}) mt-env)
        (numV 1))
  (test (interp (parse `{snd {pair 1 false}}) mt-env)
        (boolV #f))
  (test (interp (parse `{fst {pair {{lambda {[x : num]} {+ x x}} 3} false}}) mt-env)
        (numV 6))
  (test (interp (parse `false) mt-env)
        (boolV #f))
  (test (interp (parse `{= 1 2}) mt-env)
        (boolV #f))
  (test (interp (parse `{= true true}) mt-env)
        (boolV #t))
  (test (interp (parse `{if true false true}) mt-env)
        (boolV #f))
  (test (interp (parse `{if false 9 8}) mt-env)
        (numV 8))
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test/exn (interp (parse `{fst 3}) mt-env)
            "not a pair")
  (test/exn (interp (parse `{snd false}) mt-env)
            "not a pair")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        
        (numV 12))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[y : num {+ 1 x}]}
                            {+ x y}}})
                mt-env)
        
        (numV 11))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{if 1 2 3}) mt-env)
            "not a boolean")
  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (make-lookup [name-of : ('a -> Symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : Symbol] [vals : (Listof 'a)]) : 'b
    (type-case (Listof 'a) vals
      [empty (error 'find "free variable")]
      [(cons val rst-vals) (if (equal? name (name-of val))
                               (val-of (first vals))
                               ((make-lookup name-of val-of) name rst-vals))])))

(define lookup
  (make-lookup bind-name bind-val))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

;; typecheck ----------------------------------------
(define (typecheck [a : Exp] [tenv : Type-Env]) : Type
  (type-case Exp a
    [(boolE b) (boolT)]
    [(numE n) (numT)]
    [(plusE l r) (typecheck-nums l r tenv)]
    [(multE l r) (typecheck-nums l r tenv)]

    [(pairE f s) (starT (typecheck f tenv) (typecheck s tenv))]

    [(fstE p) (type-case Type (typecheck p tenv)
                [(starT f s) f]
                [else (type-error p "star")])]

    [(sndE p) (type-case Type (typecheck p tenv)
                [(starT f s) s]
                [else (type-error p "star")])]
    
    [(ifE c t e)
     (type-case Type (typecheck c tenv)
       [(boolT) (typecheck-same t e tenv)]
       [else (type-error c "bool")])]

    [(equalE l r) (begin
                    (typecheck-same l r tenv)
                    (boolT))]
    
    [(idE n) (type-lookup n tenv)]
    [(lamE ns arg-types body)
     (arrowT arg-types
             (typecheck body 
                        (foldl extend-env (map2 tbind ns arg-types)
                                    tenv)))]
    [(appE fun args)
     (type-case Type (typecheck fun tenv)
       [(arrowT arg-types result-type)
        (if (equal? arg-types
                    (map2 typecheck args (build-list (length args) (lambda (dummy) tenv))))
            result-type
            (type-error args
                        (to-string arg-types)))]
       [else (type-error fun "function")])]))

(define (typecheck-same l r tenv)
  (let ([t1 (typecheck l tenv)])
          (if (equal? t1 (typecheck r tenv))
              t1
              (type-error r (to-string t1)))))

(define (typecheck-nums l r tenv)
  (type-case Type (typecheck l tenv)
    [(numT)
     (type-case Type (typecheck r tenv)
       [(numT) (numT)]
       [else (type-error r "num")])]
    [else (type-error l "num")]))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define type-lookup
  (make-lookup tbind-name tbind-type))

(module+ test
  (test (typecheck (parse `10) mt-env)
        (numT))
  (test (typecheck (parse `{pair 1 true}) mt-env)
        (starT(numT)(boolT)))
  (test (typecheck (parse `{pair 1 -9}) mt-env)
        (starT(numT)(numT)))
  (test (typecheck (parse `{pair 1 {if false 9 8}}) mt-env)
        (starT(numT)(numT)))
  (test (typecheck (parse `{fst {pair 1 true}}) mt-env)
        (numT))
  (test (typecheck (parse `{snd {pair 1 true}}) mt-env)
        (boolT))
  (test (typecheck (parse `true) mt-env)
        (boolT))
  (test (typecheck (parse `false) mt-env)
        (boolT))
  (test (typecheck (parse `(= 1 1)) mt-env)
        (boolT))
  (test (typecheck (parse `{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{lambda {[x : num]} 12}) mt-env)
        (arrowT (list (numT)) (numT)))
  (test (typecheck (parse `{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (list (numT)) (arrowT (list (boolT))  (numT))))

  (test (typecheck (parse `{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))

  (test (typecheck (parse `{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))

  (test/exn (typecheck (parse `{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{fst 2})
                       mt-env)
            "no type")
   (test/exn (typecheck (parse `{snd {lambda {[x : bool]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{+ 1 {fst {pair false 2}}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{= 1 false})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{if 1 true false})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{if true 1 false})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type"))