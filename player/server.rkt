#lang racket
(require rhombus/private/treelist
         (prefix-in json: "json.rhm")
         "state.rhm"
         "game.rhm")

(struct connection (index i o))

(define games (make-hash))

(define (handle-connection i o)
  (define id (read i))
  (define players (read i))
  (define player (read i))
  (when (and (exact-nonnegative-integer? id)
             (memv players '(2 3 4))
             (<= 0 player (sub1 players)))
    (define game-key (list id players))
    (define game (hash-ref games game-key
                           (lambda ()
                             (define game (make-game game-key players))
                             (hash-set! games game-key game)
                             game)))
    (channel-put game (connection player i o))))

(define (note-error exn)
  (log-error "~a" (exn-message exn)))

(define (make-game key n)
  (define ch (make-channel))
  (thread (lambda ()
            (define-values (end-s players)
              (drive_game
               n
               #:data (hasheqv)
               (lambda (s player-index players)
                 (define map (hash-set (state_to_map s) 'player player-index))
                 (let loop ([players players])
                   (log-error "~a: ~s ~s" key players player-index)
                   (define c (hash-ref players player-index #f))
                   (when c
                     (with-handlers ([exn:fail? note-error])
                       (json:write map (connection-o c))))
                   (cond
                     [c
                      (define move
                        (with-handlers ([exn:fail? note-error])
                          (json:read (connection-i c))))
                      (log-error "~a: ~s" key move)
                      (cond
                        [(treelist? move)
                         (values (treelist-ref move 0)
                                 players)]
                        [else
                         (loop (hash-remove players player-index))])]
                     [else
                      (define c (channel-get ch))
                      (loop (hash-set players (connection-index c) c))])))))
            (define end-map (state_to_map end-s))
            (for ([c (in-hash-values players)])
              (define map (hash-set end-map 'player (connection-index c)))
              (json:write map (connection-o c)))
            (custodian-shutdown-all (current-custodian))))
  ch)
  
(define l (tcp-listen 5555 5 #t))
         
(let loop ()
  (parameterize ([current-custodian (make-custodian)])
    (define-values (i o) (tcp-accept l))
    (thread (lambda () (handle-connection i o))))
  (loop))
