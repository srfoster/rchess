#lang racket

(provide chessblunder random-chessblunder
         chessblunder->image)

(require json 
         racket/runtime-path)

(require (only-in 2htdp/image scale text above beside)
         "../../images/main.rkt"
         "../../fen/main.rkt" )


(define-runtime-path chessblunder-cache "./chessblunders-cache")


(define (cache-chessblunder! id data)
  (with-output-to-file #:exists 'replace
                       (build-path chessblunder-cache id)
                       (thunk
                         (displayln (jsexpr->string data)))))

(define (cached-chessblunder id)
  (define path (build-path chessblunder-cache id))  
  (if (file-exists? path)
    (string->jsexpr (file->string path)) 
    #f))

(define (random-chessblunder)
  (define data
    (hash-ref
     (string->jsexpr
      (with-output-to-string
        (thunk
         (system "curl -d '{\"type\": \"rated\"}' -H \"Content-Type: application/json\" -X POST https://chessblunders.org/api/blunder/get"))))
     'data))
  data)

(define (chessblunder id)
 (define data
   (or
     (cached-chessblunder id)
     (hash-ref
       (string->jsexpr
         (with-output-to-string
           (thunk
             (system (~a "curl -d '{\"type\": \"explore\", \"id\":\"" id "\"}' -H \"Content-Type: application/json\" -X POST https://chessblunders.org/api/blunder/get")))))
       'data)))

  (when (not (cached-chessblunder id))
    (cache-chessblunder! id data))

  data)


(define (chessblunder->image #:answer? (answer? #f) r)
 (above
  (text (hash-ref r 'blunderMove) 25 'red)
  (if answer? 
   (text (string-join (hash-ref r 'forcedLine) " ") 25 'green)
   (text "What's the line?" 25 'black))
  (scale 0.5
   (image-chess
    (fen->list 
     (hash-ref r 'fenBefore))))))


