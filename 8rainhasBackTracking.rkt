;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 8rainhasBackTracking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))

; colunas[9];
;posicao[9];
; diagonalPrincipal[15];
; diagonalSecundaria[15];
 
 ;Uma rainha e uma de:
   ;(make-rainha (make-list 8 number) (make-list 8 number) (make-list 15 number) (make-list 15 number)) 
(define-struct rainha (colunas solucao dP dS))

(define linhaPrimeiraRainha 1);a primeira rainha vai na linha linhaPrimeiraRainha e coluna 0

;cria as listas colunas, solucao, dP, dS com zeros.
;none -> rainha
(define (inicializarListas) 
  (make-rainha  (make-list 8 0)  (make-list 8 0)  (make-list 15 0)  (make-list 15 0))
)

(define (inserir lista indice valor base)
    (cond 
      [(empty? lista) lista]
      [else
         (cond
           [(equal? indice base) (cons valor (inserir (rest lista) (- indice 1) valor base))];indice e irrelevante agora
           [else
              (cons (first lista) (inserir (rest lista) (- indice 1) valor base)) 
           ]
         ) 
      ]
    )
)  

(define (criarNovaRainha linha coluna rainha)
    (make-rainha (inserir (rainha-colunas rainha) coluna linha 1) (inserir (rainha-solucao rainha) linha coluna 1) (inserir (rainha-dP rainha) (- (+ linha coluna) 2) 1 0)  (inserir (rainha-dS rainha) (+ (- linha coluna) 7) 1 0))
)

(define (eZero? lista indice base)
    (cond 
      [(empty? lista) false]
      [else
         (cond
           [(equal? indice base) 
              (cond
                 [(equal? (first lista) 0) true]
                 [else false]
              ) 
           ]
           [else
               (eZero? (rest lista) (- indice 1) base)
           ]
         ) 
      ]
    )
)

(define (posicaoValida linha coluna rainha)
   (cond
     [(and (and (equal? (eZero? (rainha-colunas rainha) coluna 1) true)
      (equal? (eZero? (rainha-dP rainha) (- (+ linha coluna) 2) 0) true))
      (equal? (eZero? (rainha-dS rainha) (+ (- linha coluna) 7) 0) true))
      true]
     [else false]
   )
)

(define (imprimir solucao)
  (cond
    [(empty? solucao) (newline)]
    [else
    (begin (display (first solucao))
           
       (display " " )
       (imprimir (rest solucao))
       
    )
     ]
  )
)

(define (posicionarRainha linha coluna rainha)
  (cond 
    [(> coluna 8) empty]
    [(<= linha 8)
        
         (begin
           (cond
             [(equal?  (posicaoValida linha coluna rainha) true) 
              (posicionarRainha (+ linha 1)  1  (criarNovaRainha linha coluna rainha))
              ]
             [else rainha]
            )
             
            (posicionarRainha linha (+ coluna 1) rainha)
            
         )    
    ]                
    [else
        (imprimir (rainha-solucao rainha))
       
    ] 
  ) 
) 
 
(posicionarRainha 1 1 (inicializarListas))