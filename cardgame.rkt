#lang racket

(require pict)
(require racket/runtime-path)
(require (only-in slideshow/base para)
         (only-in slideshow/text with-size))
(require json)


(struct card (name cost attack health description))

(define width 822)
(define height 1122)
(define padding (* 0.05 width))

(define card-back
  (filled-rectangle width height #:color "black"))


(define all-cards empty)

(define-syntax-rule (dcard name sname cost attack health description)
  (begin (define name (card sname cost attack health description))
         (set! all-cards (cons name all-cards))))



(dcard stipend      "stipend"       0 -1 -1 "Every day: +1 coin.\nDay 1: +1 coin.\nEach player starts with one of these.")
(dcard stone-wall   "stone wall"    1  1  2 "Can defend twice per turn (if still living). Cannot attack.")
(dcard poison       "poison"        2  3  2 "+1 coin on day 3.")
(dcard farmer       "farmer"        1  1  2 "+1 coin on day 2 and 3.")
(dcard bomb-spirit  "bomb spirit"   2  9  2 "Cannot attack.")
(dcard earner       "buff farmer"   2  2  2 "+1 coin every day.")
(dcard glass        "gem"           3  1  2 "Earns 4 coins on day 3.")
(dcard merchant     "merchant"      3  2  1 "+1 coin every day.\n+1 buy on day 3.")
(dcard thief        "thief"         3  4  4 "+1 coin on day 2.")
(dcard defender     "defender"      4  2  7 "When defending, earns one gold (even if it loses).")
;; card doesn't make sense anymore- usually everyone has same number of cards
(dcard spirit      "spirit"         3  2  2 "Before bidding: add 1 coin to this card (if possible).\n+1 health and +1 attack for each coin on this card.")
(dcard killer       "killer"        5  7  7 "")
(dcard pepper "pepper"  2 1 1 "Worth 1 victory point.")
(dcard pearl  "pearl"   4 1 1 "Worth 3 victory points.")


(define every-game
  `((2 ,stipend)
    (4 ,pepper)
    (6 ,pearl)))

(define base-game
  `((4 ,stone-wall)
    (2 ,bomb-spirit)
    (4 ,poison)
    (4 ,farmer)
    (4 ,earner)
    (2 ,glass)
    (4 ,merchant)
    (2 ,thief)
    (2 ,defender)
    #;(2 ,underdog)
    (2 ,spirit)
    (4 ,killer)
    #;(2 ,valhalla)))

;; not in base game
(dcard underdog     "underdog"      4  2  2 "Every day:\n    If owner has fewer cards than the other:\n        +3 coin.")
(dcard valhalla     "valhalla"      4  2  9 "Cannot defend.\nWhen this player attacks, if the attacker dies, this player earns 2 gold.")
(dcard coin-gremlin "coin gremlin"  3  1  1 "Has +1 to hp and attack for each coin the owner has.")
(dcard loan         "loan"          0  1  1 "On buy: +7 coins. Every day: -2 coin after the buy phase.")
(dcard interest     "interest"      1  2  2 "Every day: +1 coin for every 3 coins the owner has.")
(dcard white-flag   "white flag"    3  1  3 "Bid this card instead of coins. Gain all marbles opponent bid, and discard this card.")
(dcard bunny        "bunny"         2  3  3 "3 turns after buying: gain another bunny.")
(dcard moppet       "moppet"        4  4  2 "Cannot be blocked by cards with less than 4 attack.")

(define booster1
  `((2 ,coin-gremlin)
    (2 ,valhalla)
    (2 ,underdog)
    (2 ,loan)
    (2 ,interest)
    (2 ,white-flag)
    (2 ,bunny)
    (2 ,moppet)))


;; twist cards
(dcard swap "swap" 0 -1 -1 "At the start of day 1: swap all your cards with all your opponent's cards.")
(dcard debt "debt" 0 -1 -1 "At the end of day 3: lose all your coins.")
(dcard double "double" 0 -1 -1 "At the start of day 1: double all your coins.")
(dcard battlefield "battlefield" 0 -1 -1 "At the start of day 1: do another attack phase.")
(dcard predict "predict" 0 -1 -1 "As a buy, instead of buying a card:\n Spend the same amount to predict your opponent's buy. If you are correct, get the card and the opponent doesn't. If wrong, lose the money.")


(displayln (format "number of cards in every game: ~a"
              (apply + (map car every-game))))

(define twists
  `((1 ,swap)
    (1 ,debt)
    (1 ,double)
    (1 ,battlefield)))



(displayln (format "number of cards in base game (without twists): ~a"
                   (apply + (map car (append every-game base-game)))))




(define (bold-text str)
  (text str (cons 'bold "Helvetica") 60))

(define (description-text str)
  (define newline-split (regexp-split #px"\n" str))
  (with-size
   40
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

(define (render-card card)
  (define base (filled-rectangle width height #:color "white"))
  (define name (bold-text (card-name card)))
  (define cost (bold-num (card-cost card)))
  (define attack-num 
    (if (equal? (card-attack card) -1)
        (bold-text "-")
        (bold-num (card-attack card))))
  (define health-num
    (if (equal? (card-health card) -1)
        (bold-text "-")
        (bold-num (card-health card))))
  (define sword-png (scale-to-height (bitmap "sword.png") (pict-height attack-num)))
  (define shield-png (scale-to-height (bitmap "shield.png") (pict-height attack-num)))
  

  (define attack
    (ht-append
     attack-num sword-png))
  (define health
    (ht-append
     health-num shield-png))
                 

  
  (define description (description-text (card-description card)))

  
   (superimpose
    padding (- height (pict-height cost) padding)
    cost
    (superimpose
     padding (- height (* 4 padding) (pict-height description))
     description
     (superimpose
      (- width padding (pict-width health)) padding 
      health
      (superimpose
       padding padding
       attack
       (superimpose 'center padding name base))))))
  
;; cards folder relative to this script
(define-runtime-path cards-dir "cards")

(define (take-first n lst)
  (take lst (min n (length lst))))

(define (drop-first n lst)
  (drop lst (min n (length lst))))

(define (make-grid picts #:num-columns [num-columns 10])
  (define num-rows (ceiling (/ (length picts) num-columns)))

  ;; split all picts into rows
  (define rows
    (for/list ([row (in-range num-rows)])
      (define start (* row num-columns))
      (apply ht-append
             (take-first num-columns (list-tail picts start)))))

  (println (format "made grid with ~a cards all in ~a columns and ~a rows" (length picts) num-columns num-rows))
  (apply vl-append
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


(define (setup)
  ;; delete the cards directory if it exists
  (when (directory-exists? cards-dir)
      (delete-directory/files cards-dir))

  ;; make the cards directory
  (make-directory cards-dir))


(define (save-cards cardset output-name)
  (define all-picts
    (apply
     append
     (for/list ([pair cardset])
               (define num (first pair))
               (define card (second pair))
               (for/list ([i (in-range num)])
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
  ;; first, sort the cards by cost
  (define cardset-sorted
    (sort base-game
          (lambda (a b)
            (< (card-cost (second a)) (card-cost (second b))))))

  ;; add ones in every game
  (define cardset
    (append twists every-game cardset-sorted))

  (define numbers
    (append (list (length twists))
            (map car every-game)
            (map car cardset-sorted)))
  
  ;; save tabletop code
  (define code-str (tabletop-code numbers))
  (define code-file (build-path cards-dir "code.lua"))
  (define output (open-output-file code-file))
  (display code-str output)
  (close-output-port output)

  (send (pict->bitmap card-back) save-file (build-path cards-dir "back.png") 'png)
  
  (save-cards cardset "basegame")
  (save-cards booster1 "booster1"))


(setup)
(make-game)


