#lang racket

(require pict racket/draw racket/runtime-path)
(require (only-in slideshow/base para)
         (only-in slideshow/text with-size))
(require json)

(provide (all-defined-out))

;; A card has a name, cost, attack, defense, count, and description
;; count is number of cards per player, and 0 means only 1 copy per game (twist cards)
(struct card (name cost attack defense count description tags) #:transparent)

(define victory-tag 'victory)
(define day-tracker-tag 'day-tracker)
(define reference-tag 'reference)
(define unbuyable-tag 'unbuyable)
(define coin-card-tag 'coin-card)
(define player-1-tag 'player-1)
(define player-2-tag 'player-2)
(define player-3-tag 'player-3)
(define player-4-tag 'player-4)

(define player-yellow (make-object color% 250 248 117))

(define (player-color card)
  (cond
    [(has-tag? card player-1-tag) "light blue"]
    [(has-tag? card player-2-tag) "light green"]
    [(has-tag? card player-3-tag) "light coral"]
    [(has-tag? card player-4-tag) player-yellow]
    [else (error (format "card ~a has no player tag" card))]))

(define (add-tag input-card tag)
  (struct-copy card input-card
               [tags (cons tag (card-tags input-card))]))

(define (reference-card input-card)
  (add-tag input-card reference-tag))

(define (card-count-4-players card)
  (cond
    [(has-tag? card reference-tag) 4]
    [(equal? (card-count card) 0) 1]
    [else (* 4 (card-count card))]))

(define (has-tag? card tag)
  (member tag (card-tags card)))

(define width 822)
(define height 1122)
(define padding (* 0.05 width))

(define card-back
  (filled-rectangle width height #:color "black"))


(define all-cards empty)

(define-syntax-rule (dcard name sname cost attack defense count description tags)
  (begin (define name (card sname cost attack defense count description tags))
         (set! all-cards (cons name all-cards))))

(dcard zero-coins "" 0 0 0 3 "" (list coin-card-tag unbuyable-tag))
(dcard one-coin ""   1 0 0 3 "" (list coin-card-tag unbuyable-tag))
(dcard two-coins ""  2 0 0 1 "" (list coin-card-tag unbuyable-tag))
(dcard five-coins "" 5 0 0 2 "" (list coin-card-tag unbuyable-tag))
(dcard ten-coins ""  10 0 0 1 "" (list coin-card-tag unbuyable-tag))

(dcard pass-card      "Pass"       -1 -1 -1 -1 "Player chose not to buy a card." '(reference-tag unbuyable-tag))
(dcard stipend      "Stipend"       -1 -1 -1 1 "Every day: +1 coin.\nDay 1: +1 coin.\nEach player starts with one of these." (list unbuyable-tag))
(dcard stone-wall   "Stone Wall"    1  1  2 2 "Day 1: +1 coin\nCan defend twice per turn (unless the first makes it feint)" '())
(dcard poison       "Poison"        2  3  2 2 "Day 3: +1 coin" '())
(dcard farmer       "Farmer"        1  1  2 2 "Day 2: +1 coin\nDay 3: +1 coin" '())
(dcard bomb-spirit  "Bomb Spirit"   2  9  2 1 "Cannot attack." '())
(dcard earner       "Buff Farmer"   2  2  2 2 "Every day: +1 coin" '())
(dcard glass        "Gem"           3  1  2 1 "Day 3: +4 coin" '())
(dcard merchant     "Merchant"      3  2  1 1 "Day 1: +1 coin\nDay 2: +1 coin\nDay 3: +1 buy" '())
(dcard thief        "Thief"         3  4  4 1 "Day 2: +1 coin" '())
(dcard armadillo     "Armadillo"      4  2  7 1 "When this card defends: +1 coin (even if it loses)." '())
(dcard spirit       "Spirit"        3  2  2 1 "Income phase: optionally add 1 coin to this card.\n+1 defense and +1 attack for each coin on this card." '())
(dcard brute        "Brute"         5  7  7 1 "" '())
(dcard interest     "Interest"      1  1  1 1 "Every day: +1 coin for every 3 coins the owner has." '())
(dcard pepper "Pepper"  2 1 1 2 "Worth 1 victory point." (list victory-tag))
(dcard pearl  "Pearl"   4 1 1 3 "Worth 3 victory points." (list victory-tag))
(dcard day-tracker  "" -1 -1 -1 0 "Day 1\n\n\nDay 2\n\n\nDay 3" (list day-tracker-tag unbuyable-tag))


(define every-game-shop-cards
 (list pepper
       pearl))

(define every-game
  (append
    (list
      zero-coins
      one-coin
      two-coins
      five-coins
      ten-coins
      stipend
      day-tracker)
    every-game-shop-cards))

(define base-game-cards
  (list
    stone-wall
    bomb-spirit
    poison
    farmer
    earner
    interest
    glass
    merchant
    thief
    armadillo
    brute))

(define base-game-cards-sorted
    (sort base-game-cards
          (lambda (a b)
            (< (card-cost a) (card-cost b)))))


(define shop-base-game
  (append every-game-shop-cards base-game-cards-sorted))

(define base-game-all-without-reference
    (append every-game base-game-cards-sorted))
(define base-game-all
  (append base-game-all-without-reference
          (for/list ([card base-game-all-without-reference]
                      #:when (not (has-tag? card unbuyable-tag)))
            (reference-card card))
          (list pass-card)))

;; not in base game
(dcard underdog     "Underdog"      4  2  2 1 "Every day:\n    If owner has fewer cards than the other:\n        +3 coin." '())
(dcard valhalla     "Valhalla"      4  2  9 1 "Cannot defend.\nWhen this player attacks, if the attacker dies, this player earns 2 coins." '())
(dcard coin-gremlin "Coin Gremlin"  3  1  1 1 "Has +1 to hp and attack for each coin the owner has." '())
(dcard loan         "Loan"          0  1  1 1 "On buy: +7 coins. Every day: -2 coin after the buy phase." '())
(dcard white-flag   "White Flag"    3  1  3 1 "Attack phase: Bid this card instead of coins. Gain all marbles opponent bid, and discard this card." '())
(dcard bunny        "Bunny"         2  3  3 1 "3rd income phase after bought:\nGain a bunny twin from the shop." '())
(dcard bunny-twin   "Bunny Twin"    2  3  3 1 "Cannot be bought." '())
(dcard moppet       "Moppet"        4  4  2 1 "Cannot be blocked by cards with less than 4 attack." '())

(define booster1
  (list
    coin-gremlin
    valhalla
    underdog
    loan
    white-flag
    bunny
    moppet
    spirit))


;; twist cards
(dcard debt "debt" 0 -1 -1 0 "At the end of day 3: lose all your coins." '())
(dcard double "double" 0 -1 -1 0 "At the start of day 1: double all your coins." '())
(dcard battlefield "battlefield" 0 -1 -1 0 "At the start of day 1: do another attack phase." '())
(dcard predict "predict" 0 -1 -1 0 "As a buy, instead of buying a card:\n Predict the card your opponent is buying.\nIf you are correct, get the card and the opponent doesn't.\nIf wrong, lose the money." '())




;; Twists are disabled for now
#;(define twists
  (list
    debt
    double
    battlefield))

(define (area-text str)
  (text str (cons 'bold "Helvetica") 250))

(define (bold-text str)
  (text str (cons 'bold "Helvetica") 100))

(define (bold-underline-text str)
  (text str (cons 'italic (cons 'bold "Helvetica")) 100))

(define (coin-card-text str)
  (text str (cons 'bold "Helvetica") 200))

(define (large-description-text str)
  (description-text str #:font-size 100))

(define (description-text str #:font-size [font-size 80])
  (define newline-split (regexp-split #px"\n" str))
  (with-size
   font-size
   (apply vl-append
          (for/list ([line newline-split])
                    (para (let ([strs (regexp-split #px" *coin( +|$)" line)])
                            (define coin (scale coin-image 0.1) )
                            (map (lambda (e)
                                   (cond
                                     [(and (string? e)
                                           (regexp-match #px"^(Day [1-3]|^Every day)(.*)" e))
                                      => (lambda (m)
                                           (list (colorize (para #:fill? #f (cadr m)) "firebrick")
                                                 (caddr m)))]
                                     [else e]))
                                 (add-between strs (inset coin (* -0.2 (pict-height coin))))))
                          #:width (- width (* 2 padding)))))))



(define (bold-num num)
  (bold-text (number->string num)))

(define (superimpose x y new base)
  (define xpos
    (if (equal? x 'center)
        (/ (- (pict-width base) (pict-width new)) 2)
        x))
  (define ypos
    (if (equal? y 'center)
        (/ (- (pict-height base) (pict-height new)) 2)
        y))
  (lt-superimpose
   base
   (inset new xpos ypos 0 0)))


(define (scale-to-height pict height)
  (define scale-factor (/ height (pict-height pict)))
  (scale pict scale-factor))

(define person-image (bitmap "person.png"))
(define coin-image (bitmap "coin.png"))
(define sword-image (bitmap "sword.png"))
(define shield-image (bitmap "shield.png"))

(define (number-icon image num source-card)
  (define num-text
   (if (equal? num -1)
       (bold-text "-")
       (bold-num num)))
  (define scaled-picture
    (scale-to-height image (pict-height num-text)))
  (if (has-tag? source-card day-tracker-tag)
      (blank)
      (hc-append 20 num-text scaled-picture)))


(define transparent (make-object color% 0 0 0 0))
(define (rect-with-border width height #:color [color "white"] #:border-color [border-color "black"] #:border-width [border-width 1])
  (superimpose 0 0
    (filled-rectangle width border-width  #:color border-color #:border-width 0)
    (superimpose 0 0
      (filled-rectangle border-width height #:color border-color #:border-width 0)
      (superimpose (- width border-width) 0
        (filled-rectangle border-width height #:color border-color #:border-width 0)
        (superimpose 0 (- height border-width)
          (filled-rectangle width border-width #:color border-color #:border-width 0)
          (filled-rectangle width height
      #:color color
      #:border-width 0))))))

(define (draw-base card)
  (define border-width (/ width 30))
  (define outline-width (/ width 100))
  (define border-color
   (cond
      [(has-tag? card reference-tag)
        "white"]
      [(has-tag? card victory-tag)
        "light green"]
      [(has-tag? card coin-card-tag)
        "light yellow"]
      [else "light blue"]))
  (define background-color
   (cond
     [(has-tag? card reference-tag)
      (player-color card)]
     [(has-tag? card coin-card-tag)
      (player-color card)]
     [else "white"]))

  (superimpose 0 0
    (rect-with-border width height #:color transparent #:border-color "black" #:border-width outline-width)
    (rect-with-border width height
      #:color background-color
      #:border-color border-color
      #:border-width border-width)))

(define (with-player-count card pict)
  (define count (number-icon person-image (card-count card) card))
  (superimpose
    (- width padding (pict-width count))
    (- height (pict-height count) padding)
    count
    pict))

(define (render-coin-card card)
  (define base (draw-base card))
  (define coin-text (coin-card-text (number->string (card-cost card))))
  (define coin-pict (scale-to-height coin-image (pict-height coin-text)))
  (define person-pict (scale-to-height person-image (pict-height coin-text)))
  (define coin
    (hc-append 20 coin-text coin-pict))
  (define coin-small (number-icon coin-image (card-cost card) card))

  (with-player-count card
    (superimpose padding padding coin-small
      (superimpose 'center 'center coin base))))

(define (render-card card)
  (cond
    [(has-tag? card coin-card-tag)
     (render-coin-card card)]
    [else
     (render-normal-card card)]))


(define (render-normal-card card)
  (define base (draw-base card))
    
  (define name
   (if (has-tag? card reference-tag)
       (bold-underline-text (card-name card))
       (bold-text (card-name card))))
  
  (define attack
    (number-icon sword-image (card-attack card) card))
  (define defense
    (number-icon shield-image (card-defense card) card))
  (define cost
    (number-icon coin-image (card-cost card) card))
  
  (define description
    (if (has-tag? card day-tracker-tag)
        (large-description-text (card-description card))
        (description-text (card-description card))))

  
  (with-player-count card
     (superimpose
        padding (- height (pict-height cost) padding)
        cost
        (superimpose
          padding (- height (* 5 padding) (pict-height description))
          description
        (superimpose
          (- width padding (pict-width defense))
          padding
          defense
          (superimpose
          padding
          padding
          attack
          (superimpose 'center
           (+ padding (pict-height cost))
           name base)))))))
;; cards folder relative to this script
(define-runtime-path cards-dir "docs")

(define (take-first n lst)
  (take lst (min n (length lst))))

(define (drop-first n lst)
  (drop lst (min n (length lst))))

(define (make-grid picts #:num-columns [num-columns 10] #:spacing [spacing 0])
  (define num-rows (ceiling (/ (length picts) num-columns)))

  ;; split all picts into rows
  (define rows
    (for/list ([row (in-range num-rows)])
      (define start (* row num-columns))
      (apply ht-append spacing
             (take-first num-columns (list-tail picts start)))))

  (println (format "made grid with ~a cards all in ~a columns and ~a rows" (length picts) num-columns num-rows))
  (apply vl-append spacing
         rows))



(define (tabletop-code piles)
  (define number-list
    (string-append
     "{ "
     (string-join
     (for/list
      ([num piles]
       [i (in-range (length piles))])
      (format "[~a]=~a" i num))
     ", "
     )
     " }"))
    

  (format
   "

function onLoad()
self.createButton({
click_function = \"clickedButton\", -- This is a required feild
function_owner = self, -- This is a required feild
label = \"Setup Game\", -- If you want the button to have Text on it.
position = {0, 0.5, 0},
rotation = {0, 0, 0}, -- if you want to rotate the button.
scale = {1, 1, 1}, -- If you want to mess with the scale (helps with text sizes sometimes)
width = 1000, -- Size of the standard Cube
height = 1000, -- Size of the standard Cube
tooltip = \"\", -- If you want a floating tooltip when hovering over button
})
end

function clickedButton()
    local deck = getObjectFromGUID(\"69337f\")
    local other = deck
    print(deck)
    local deckpos = deck.getPosition()
    print(deckpos)
    local rotation = deck.getRotation()

    local nums = ~a
    local index = 0

    local count = 0
    local function printAndReschedule()
    if count > 0 then
       local countl = count-1
       other.setRotation(rotation)
       other.setPosition(deckpos + Vector(3 * (countl % 8), 0, -6 + -6 * math.floor((countl / 8))))
       other.flip()
    end
    if count < ~a then
      local thesplit = deck.cut(nums[~a-count])
      deck = thesplit[1]
          
      other = thesplit[2]
      
      count = count + 1
      Wait.frames(printAndReschedule, 10)
    elseif count == ~a then
       other = deck
       count = count + 1
       Wait.frames(printAndReschedule, 10)
    end
  end

    Wait.frames(printAndReschedule, 10)
end


   " number-list
     (- (length piles) 1)
     (- (length piles) 1)
     (- (length piles) 1)))

(define (current-ps-vmargin)
  (define hmargin (box 0))
  (define vmargin (box 0))
  (send (current-ps-setup) get-margin hmargin vmargin)
  (unbox vmargin))

(define (picts->pdf picts output-name)
  (define pdf-dc (new pdf-dc% [width #f] [height #f] [use-paper-bbox #t]
                      [output output-name] [interactive #f]  [as-eps #f]))
  (define-values (dc-width dc-height) (send pdf-dc get-size))
  (define paper-height dc-height)
  (define num-columns 3)
  (define A4-height 11.7) ;; in inches
  (define Card-height 3.5) ;; in inches


  (define scale-factor (/ (* Card-height num-columns) A4-height))
  (define target-height (* paper-height scale-factor))
  (send pdf-dc start-doc "test.pdf")
  (for ([pict picts])
      (send pdf-dc start-page)
      (draw-pict (scale-to-height pict target-height) pdf-dc 0 0)
      (send pdf-dc end-page))
  (send pdf-dc end-doc))

(define (make-printable picts)
  (define num-columns 3)
  (define num-per-page (* num-columns 3))
  (cond
    [(empty? picts) empty]
    [else
     (define blank
       (ghost (first picts)))
     (define first-page (take-first num-per-page picts))
     (define fill-in (make-list (- num-per-page (length first-page)) blank))
     (define rest (drop-first num-per-page picts))
     (cons (make-grid (append first-page fill-in) #:num-columns num-columns)
           (make-printable rest))]))


(define (with-player-tag card i)
  (define per-player
    (floor
     (/ (card-count-4-players card)
        4)))
  (cond
    [(zero? per-player)
      card]
    [(< i per-player)
     (add-tag card player-1-tag)]
    [(< i (* per-player 2))
     (add-tag card player-2-tag)]
    [(< i (* per-player 3))
     (add-tag card player-3-tag)]
    [(< i (* per-player 4))
     (add-tag card player-4-tag)]
    [else card]))

(define (save-cards cardset output-name)
  (define all-picts
    (apply
     append
     (for/list ([card cardset])
        (for/list ([i (in-range (card-count-4-players card))])
            (render-card (with-player-tag card i))))))
  (println (format "Game requires printing ~a cards" (length all-picts)))

  ;; make printable
  (define picts-printable
    (make-printable all-picts))
  (picts->pdf picts-printable (build-path cards-dir (string-append output-name ".pdf")))

  ;; make small to improve rendering time
  (define all-picts-smaller
    (map (lambda (pict)
           (scale-to-height pict 200))
         all-picts))
  (define picts-appended
    (make-grid all-picts-smaller))
  
  (define all (build-path cards-dir (string-append output-name ".png")))
  (send (pict->bitmap picts-appended) save-file all 'png))

(define (make-game)
  (define numbers
    (map card-count-4-players base-game-all))
  
  ;; save tabletop code
  (define code-str (tabletop-code numbers))
  (define code-file (build-path cards-dir "code.lua"))
  (define output (open-output-file code-file #:exists 'truncate))
  (display code-str output)
  (close-output-port output)

  (send (pict->bitmap card-back) save-file (build-path cards-dir "back.png") 'png)
  
  (save-cards base-game-all "basegame")
  (save-cards booster1 "booster1"))




