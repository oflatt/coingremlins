#lang racket

(require pict)
(require racket/runtime-path)
(require (only-in slideshow/base para)
         (only-in slideshow/text with-size))
(require json)

(provide (all-defined-out))

;; A card has a name, cost, attack, defense, count, and description
;; count is number of cards per player, and 0 means only 1 copy per game (twist cards)
(struct card (name cost attack defense count description tags))

(define (card-count-4-players card)
  (if (equal? (card-count card) 0)
      1
      (* 4 (card-count card))))

(define (has-tag card tag)
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

(define victory-tag 'victory)
(define day-tracker-tag 'day-tracker)


(dcard stipend      "Stipend"       -1 -1 -1 1 "Every day: +2 coin.\nDay 1: +1 coin.\nEach player starts with one of these." '())
(dcard stone-wall   "Stone Wall"    1  1  2 2 "Day 1: +1 coin.\nCan defend twice per turn ( unless the first makes it feint)" '())
(dcard poison       "Poison"        2  3  2 2 "+1 coin on day 3." '())
(dcard farmer       "Farmer"        1  1  2 2 "+1 coin on day 2 and 3." '())
(dcard bomb-spirit  "Bomb Spirit"   2  9  2 1 "Cannot attack." '())
(dcard earner       "Buff Farmer"   2  2  2 2 "+1 coin every day." '())
(dcard glass        "Gem"           3  1  2 1 "Earns 4 coins on day 3." '())
(dcard merchant     "Merchant"      3  2  1 1 "+1 coin on day 1.\n+1 coin on day 2.\n+1 buy on day 3." '())
(dcard thief        "Thief"         3  4  4 1 "+1 coin on day 2." '())
(dcard armadillo     "Armadillo"      4  2  7 1 "When defending, earns one gold (even if it loses)." '())
(dcard spirit       "Spirit"        3  2  2 1 "Income phase: optionally add 1 coin to this card.\n+1 defense and +1 attack for each coin on this card." '())
(dcard brute        "Brute"         5  7  7 1 "" '())
(dcard interest     "Interest"      1  1  1 1 "Every day: +1 coin for every 3 coins the owner has." '())
(dcard pepper "Pepper"  2 1 1 2 "Worth 1 victory point." (list victory-tag))
(dcard pearl  "Pearl"   4 1 1 3 "Worth 3 victory points." (list victory-tag))
(dcard day-tracker  "Day Tracker" -1 -1 -1 0 "Day 1\n\nDay 2\n\nDay 3\n" (list day-tracker-tag))


(define every-game
  (list
   stipend
   day-tracker
   pepper
   pearl))

(define base-game
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
(define basegame-sorted
    (sort base-game
          (lambda (a b)
            (< (card-cost a) (card-cost b)))))
(define shop-base-game
  (append (cdr (cdr every-game)) basegame-sorted))

;; not in base game
(dcard underdog     "Underdog"      4  2  2 1 "Every day:\n    If owner has fewer cards than the other:\n        +3 coin." '())
(dcard valhalla     "Valhalla"      4  2  9 1 "Cannot defend.\nWhen this player attacks, if the attacker dies, this player earns 2 gold." '())
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
(define twists
  empty)
  #;(list
    debt
    double
    battlefield)

(define (area-text str)
  (text str (cons 'bold "Helvetica") 250))

(define (bold-text str)
  (text str (cons 'bold "Helvetica") 100))

(define (large-description-text str)
  (description-text str #:font-size 100))

(define (description-text str #:font-size [font-size 80])
  (define newline-split (regexp-split #px"\n" str))
  (with-size
   font-size
   (apply vl-append
          (for/list ([line newline-split])
                    (para line #:width (- width (* 2 padding)))))))

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


(define (number-icon file num)
  (define num-text
   (if (equal? num -1)
       (bold-text "-")
       (bold-num num)))
  (define scaled-picture
    (scale-to-height (bitmap file) (pict-height num-text)))
  (hc-append 20 num-text scaled-picture))

(define (render-card card)
  (define base
    (filled-rectangle width height
      #:color (if (has-tag card victory-tag)
                  "light green"
                  "white")))
  (define name (bold-text (card-name card)))
  
  (define attack
    (number-icon "sword.png" (card-attack card)))
  (define defense
    (number-icon "shield.png" (card-defense card)))
  (define cost
    (number-icon "coin.png" (card-cost card)))
  (define count
    (if (equal? (card-count card) 0)
        (blank 0 0)
        (number-icon "person.png" (card-count card))))
                  

  
  (define description
    (if (has-tag card day-tracker-tag)
        (large-description-text (card-description card))
        (description-text (card-description card))))

  
  (superimpose
     (- width padding (pict-width count))
     (- height (pict-height count) padding)
     count
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

(define (make-printable picts)
  (define num-columns 4)
  (define num-per-page (* num-columns 4))
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


(define (save-cards cardset output-name)
  (define all-picts
    (apply
     append
     (for/list ([card cardset])
        (for/list ([i (in-range (card-count-4-players card))])
            (define output (build-path cards-dir (string-append (card-name card) ".png")))
            (define rendered (render-card card))
            (send (pict->bitmap rendered) save-file output 'png)
                  rendered))))
  
  (define picts-appended
    (make-grid all-picts))
  
  (define all (build-path cards-dir (string-append output-name ".png")))
  (send (pict->bitmap picts-appended) save-file all 'png)

  ;; make printable
  (define picts-printable
    (make-printable all-picts))
  (for ([pict picts-printable]
        [i (in-range (length picts-printable))])
       (define output (build-path cards-dir (format "print~a-~a.png" output-name i)))
       (send (pict->bitmap pict) save-file output 'png))
  )

(define (make-game)
  ;; add ones in every game
  (define cardset
    (append twists every-game basegame-sorted))

  (define numbers
    (append (list (length twists))
            (map card-count-4-players every-game)
            (map card-count-4-players basegame-sorted)))
  
  ;; save tabletop code
  (define code-str (tabletop-code numbers))
  (define code-file (build-path cards-dir "code.lua"))
  (define output (open-output-file code-file))
  (display code-str output)
  (close-output-port output)

  (send (pict->bitmap card-back) save-file (build-path cards-dir "back.png") 'png)
  
  (save-cards cardset "basegame")
  (save-cards booster1 "booster1"))




