#lang racket

(require pict racket/draw racket/runtime-path)
(require (only-in slideshow/base
                  para
                  current-main-font
                  current-font-size)
         (only-in slideshow/text with-size))
(require json)

(provide (all-defined-out))

;; count is number of cards per player, and 0 means only 1 copy per game (twist cards)
(struct card (png-name name cost attack defense count centered-description detailed-description tags) #:transparent)

(define victory-tag 'victory)
(define day-tracker-tag 'day-tracker)
(define reference-tag 'reference)
(define not-in-shop-tag 'not-in-shop)
(define coin-card-tag 'coin-card)
(define player-1-tag 'player-1)
(define player-2-tag 'player-2)
(define player-3-tag 'player-3)
(define player-4-tag 'player-4)

;; cards that are part of every game
(define every-game-tag 'every-game)
;; cards part of the base game
(define base-game-tag 'base-game)
;; cards in booster 1 pack
(define booster1-tag 'booster1)
;; cards in booster 2 pack
(define booster2-tag 'booster2)

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
    [(and (has-tag? card reference-tag)
          (not (has-tag? card coin-card-tag)))
     4]
    [(equal? (card-count card) 0) 1]
    [else (* 4 (card-count card))]))

(define (has-tag? card tag)
  (member tag (card-tags card)))

(define width 822)
(define height 1122)
(define padding (* 0.05 width))
(define outline-size 10)
(define margin-size (/ width 30))

(define card-back
  (filled-rectangle width height #:color "black"))

(define (sort-by-cost cards)
  (sort cards
        (lambda (a b)
          (< (card-cost a) (card-cost b)))))

(define all-cards empty)

(define-syntax-rule (dcard name png-name sname cost attack defense count centered-description detailed-description tags)
  (begin (define name (card png-name sname cost attack defense count centered-description detailed-description tags))
         (set! all-cards (cons name all-cards))))

(dcard zero-coins #f "Zero Coins" 0 0 0 3 "" "" (list coin-card-tag not-in-shop-tag every-game-tag reference-tag))
(dcard one-coin #f "One Coin"   1 0 0 3 "" "" (list coin-card-tag not-in-shop-tag every-game-tag reference-tag))
(dcard two-coins #f "Two Coins"  2 0 0 1 "" "" (list coin-card-tag not-in-shop-tag every-game-tag reference-tag))
(dcard five-coins #f "Five Coins" 5 0 0 2 "" "" (list coin-card-tag not-in-shop-tag every-game-tag reference-tag))
(dcard ten-coins #f "Ten Coins"  10 0 0 1 "" "" (list coin-card-tag not-in-shop-tag every-game-tag reference-tag))


(dcard pass-card  #f  "Pass"       -1 -1 -1 1 "Player chose not to buy a card" "" (list reference-tag not-in-shop-tag every-game-tag))
(dcard stipend    #f   "Sorcerer's Stipend"       -1 -1 -1 1 "Every day: +1 coin\nDay 1: +1 coin" "Each player starts with one of these." (list not-in-shop-tag every-game-tag))
(dcard day-tracker #f "Day Tracker" -1 -1 -1 0 "" "Day 1\n\n\nDay 2\n\n\nDay 3"  (list day-tracker-tag not-in-shop-tag every-game-tag))
(dcard pepper      "monopoly.png"   "Board of Monopoly"      2 1 1 2 "Worth 1 victory point" "" (list victory-tag every-game-tag))
(dcard pearl      "incantation.png"   "Incantation"   4 1 1 3 "Worth 3 victory points"  "" (list victory-tag every-game-tag))



;; ################### BASE GAME ###############################
(dcard stone-wall  "goldwall.png" "Wall of Wealth"    1  1  2 2 "Day 1: +1 coin" "Can defend twice per turn (unless the first makes it faint)" (list base-game-tag))
(dcard poison    "ghost.png"   "Ghost"         2  3  2 2 "Day 3: +1 coin" "" (list base-game-tag))
(dcard farmer    "worker.png"   "Worker"        1  1  2 2 "Day 2: +1 coin\nDay 3: +1 coin" "" (list base-game-tag))
(dcard bomb-spirit "bubble.png" "Bubble"        2  9  2 1 "" "Cannot attack" (list base-game-tag))
(dcard buff-farmer "senior-worker.png" "Senior Worker"   2  2  2 2 "Every day: +1 coin" "" (list base-game-tag))
(dcard glass    "goldfish.png"   "Gold Fish"           3  1  2 1 "Day 3: +4 coin" "" (list base-game-tag))
(dcard merchant   #f  "Apprentice"      3  2  1 1 "Day 1: +1 coin\nDay 2: +1 coin\nDay 3: +1 buy" "" (list base-game-tag))
(dcard thief    #f    "Thug"         3  4  4 1 "Day 2: +1 coin" "" (list base-game-tag))
(dcard armadillo  #f  "Shield of Greed"  4  2  7 1 "" "When this card defends: +1 coin (even if it loses)" (list base-game-tag))
(dcard brute   "goldem.png"     "Golem"         5  7  7 1 "" "" (list base-game-tag))
(dcard interest  "beanstalk.png"   "Magic Bean Stock"       1 1 1 1 "Every day: +1 coin for every 3 coins the owner has" "" (list base-game-tag))


;; ################  BOOSTER 1 ##################################
(dcard underdog   #f  "Underdog"      4  2  2 1 "Every day:\n    If owner has fewer cards than the other:\n        +3 coin" "" (list booster1-tag))
(dcard lizard    "lizard.png"   "Aggressive Lizard" 3  2  2 1 "" "Attack phase: gain 2 coins when attacking other players." (list booster1-tag))
(dcard coin-gremlin #f "Coin Gremlin"  3  1  1 1 "" "Has +1 to hp and attack for each coin the owner has." (list booster1-tag))
(dcard gold-mine   #f   "Gold mine"    3  2  2 1 "" "Day 2: +1 coin for each card to the right of this card." (list booster1-tag))
(dcard loan    #f     "Loan"          0  1  1 1 "On buy: +7 coins\nEvery day: -2 coin after the buy phase." "" (list booster1-tag))
(dcard moppet    #f   "BuffFarmer2"    4  4  2 1 "" "Cannot be blocked by cards with less than 4 attack." (list booster1-tag))
(dcard spirit   #f   "Spirit"        2  2  2 1 "Income phase: optionally add 1 coin to this card" "+1 defense and +1 attack for each coin on this card" (list booster1-tag))


;; ##################### BOOSTER 2 ###############################
(dcard strange-flower #f  "Strange Flower" 3 1 3 1 "" "Has +1 to attack for every card to its right." (list booster1-tag))
(dcard white-flag #f  "White Flag"    3  1  3 1 "" "Attack phase: Bid this card instead of coins. Gain all marbles opponent bid, and discard this card." (list booster2-tag))
(dcard bunny   #f     "Bunny"         2  3  3 1 "" "3rd income phase after bought:\nGain a bunny twin from the shop." (list booster2-tag))
(dcard bunny-twin  #f  "Bunny Twin"    2  3  3 1 "" "Cannot be bought." (list booster2-tag))
(dcard valhalla   #f  "Valhalla"      4  2  9 1 "" "Cannot defend\nWhen this player attacks, if the attacker dies, +2 coin for owner" (list booster2-tag))

;; card name ideas
;; vertical incantation/integration
;; horizontal incantation/integration
;; bullish bull
;; bearish bear
;; something about inflation
;; cash cow
;; golden goose
;; laundering / laundry
;; lottery related

;; Get cards with tag, reverse so they appear in order
;; they were defined
(define (get-cards-with-tag tag)
  (reverse (filter (lambda (card)
                     (has-tag? card tag))
                   all-cards)))


(define every-game
  (get-cards-with-tag every-game-tag))
(define every-game-shop-cards
  (filter (lambda (card) (not (has-tag? card not-in-shop-tag)))
          every-game))

(define base-game
  (sort-by-cost (get-cards-with-tag base-game-tag)))

(define shop-base-game
  (append every-game-shop-cards base-game))

(define booster1
  (sort-by-cost (get-cards-with-tag booster1-tag)))
(define booster2
  (sort-by-cost (get-cards-with-tag booster2-tag)))

;; twist cards, currently unused
(dcard debt #f "debt" 0 -1 -1 0 "" "At the end of day 3: lose all your coins" '())
(dcard double #f "double" 0 -1 -1 0 "" "At the start of day 1: double all your coins" '())
(dcard battlefield #f "battlefield" 0 -1 -1 0 "" "At the start of day 1: do another attack phase" '())
(dcard predict #f "predict" 0 -1 -1 0 "" "As a buy, instead of buying a card:\n Predict the card your opponent is buying\nIf you are correct, get the card and the opponent doesn't\nIf wrong, lose the money" '())




;; Twists are disabled for now
#;(define twists
    (list
     debt
     double
     battlefield))

(define font-name '("PT Sans" . swiss))

(define (area-text str)
  (text str (cons 'bold font-name) 250))

(define large-text-size 55)
(define centered-text-size 55)

(define (bold-text str #:size [size large-text-size])
  (text str (cons 'bold font-name) size))
(define (bold-small-text str)
  (bold-text str #:size centered-text-size))
(define (victory-text s)
  (colorize (bold-text s #:size (current-font-size)) "forestgreen"))

(define (bold-num num)
  (bold-text (number->string num)))
(define (bold-small-num num)
  (bold-text (number->string num) #:size centered-text-size))


(define (coin-card-text str)
  (text str (cons 'bold font-name) 200))

(define (large-description-text str)
  (description-text str #:font-size large-text-size))

(define (day-tracker-text str)
  (description-text str #:font-size 85))

(define (process-description-line line)
  (define coin (scale-to-height coin-image centered-text-size))
  (map (lambda (e)
         (cond
           [(and (string? e)
                 (regexp-match #px"^(Day [1-3]|^Every day)(.*)" e))
            => (lambda (m)
                 (list (colorize (bold-text (cadr m) #:size (current-font-size))
                                 "firebrick")
                       (caddr m)))]
           [(and (string? e)
                 (regexp-match #px"^(.*) ([0-9]+) victory (points?)(.*)" e))
            => (lambda (m)
                 (list (list-ref m 1)
                       (victory-text (list-ref m 2))
                       (victory-text "victory")
                       (victory-text (list-ref m 3))
                       (list-ref m 4)))]
           [else e]))
       (add-between (regexp-split #px" *coin( +|$)" line)
                    (inset coin (* -0.2 (pict-height coin))))))

(define (description-text str #:font-size [font-size centered-text-size] #:align [align 'left])
  (define newline-split (regexp-split #px"\n" str))
  (with-size
      font-size
    (apply vl-append
           (for/list ([line newline-split])
             (parameterize ([current-main-font font-name])
               (para (process-description-line line)
                     #:width (- width (* 2 padding))
                     #:align align))))))




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

(define (scale-to-width pict width)
  (define scale-factor (/ width (pict-width pict)))
  (scale pict scale-factor))

(define person-image (bitmap "person.png"))
(define coin-image (bitmap "coin.png"))
(define sword-image (bitmap "sword.png"))
(define shield-image (bitmap "shield.png"))

(define (number-icon image num source-card #:slash-text [slash-text ""])
  (define num-text-converted
    (if (equal? num -1)
        "-"
        (number->string num)))
  (define num-text
    (if (equal? slash-text "")
        (bold-text (string-append num-text-converted " "))
        (bold-text (string-append num-text-converted " " slash-text " "))))
  (define scaled-picture
    (scale-to-height image centered-text-size))
  (define num-and-image (hc-append 0 num-text scaled-picture))
  (define background
    (rounded-rect
     (+ outline-size outline-size (pict-width num-and-image))
     (+ outline-size outline-size (pict-height num-and-image))
     outline-size #:color lighten #:draw-border? #f))
  (if (has-tag? source-card day-tracker-tag)
      (blank)
      (superimpose 'center 'center num-and-image background)))


(define transparent (make-object color% 0 0 0 0))
(define lighten (make-object color% 255 255 255 .4))
(define (rounded-rect width height radius #:color [color "white"] #:border-color [border-color #f] #:border-width [border-width 0]
                      #:draw-border? [draw-border? #t])
  (define half (/ border-width 2))
  (inset
   (filled-rounded-rectangle (- width half) (- height half) radius #:color color #:border-color border-color #:border-width (if (equal? border-width 0) #f border-width)
                             #:draw-border? draw-border?)
   half half half half))

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
  (define border-color
    (cond
      [(has-tag? card coin-card-tag)
       "light yellow"]
      [(has-tag? card reference-tag)
       "white"]
      [(has-tag? card victory-tag)
       "light green"]
      [else "light blue"]))
  (define background-color
    (cond
      [(has-tag? card reference-tag)
       (player-color card)]
      [else "white"]))

  (superimpose 0 0
               (rect-with-border width height #:color transparent #:border-color "black" #:border-width outline-size)
               (rect-with-border width height
                                 #:color background-color
                                 #:border-color border-color
                                 #:border-width margin-size)))

(define (with-player-count card pict)
  (define count (number-icon person-image (card-count card) card #:slash-text "/"))
  (superimpose
   (- width margin-size (pict-width count))
   (- height (pict-height count) margin-size)
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

(define-runtime-path card-art-dir "card-art")

(define (get-card-art card)
  (define pwidth (- width (* 2 margin-size)))
  (cond
    [(card-png-name card)
     (define card-art (bitmap (build-path card-art-dir (card-png-name card))))
     (scale-to-width card-art pwidth)]
    [else
     (blank pwidth (* pwidth 0.75))]))

(define (card-file-name card)
  (define name
    ;; avoid characters that create trouble in filenames
    (regexp-replace* #rx"['\"\\./<>|?*]" (card-name card) "_"))
  (cond
    [(and (has-tag? card reference-tag) (has-tag? card player-1-tag))
     (string-append name "-player1" ".png")]
    [(and (has-tag? card reference-tag) (has-tag? card player-2-tag))
     (string-append name "-player2" ".png")]
    [(and (has-tag? card reference-tag) (has-tag? card player-3-tag))
     (string-append name "-player3" ".png")]
    [(and (has-tag? card reference-tag) (has-tag? card player-4-tag))
     (string-append name "-player4" ".png")]
    [else
      (string-append name ".png")]))
     

(define (render-name card)
  (define text (bold-text (card-name card)))
  (superimpose
   'center 'center
   text
   (rounded-rect
    (+ outline-size outline-size (pict-width text))
    (+ outline-size outline-size (pict-height text))
    outline-size #:draw-border? #f #:color lighten)))


(define (render-normal-card card)
  (define base (draw-base card))

  (define name (render-name card))

  (define card-art (get-card-art card))

  (define attack
    (number-icon sword-image (card-attack card) card))
  (define defense
    (number-icon shield-image (card-defense card) card))
  (define cost
    (number-icon coin-image (card-cost card) card))

  (define centered-description
    (description-text (card-centered-description card) #:align 'center))
  (define detailed-description
    (if (has-tag? card day-tracker-tag)
        (day-tracker-text (card-detailed-description card))
        (description-text (card-detailed-description card))))

  (define (inset-art card-art)
    (if (has-tag? card reference-tag)
        (let ([margin 0.2])
          (cc-superimpose (scale card-art (- 1 margin))
                          (ghost card-art)))
        card-art))

  ;; TODO detect when things overlap
  (with-player-count card
    (superimpose
     margin-size
     (- height (pict-height cost) margin-size)
     cost
     (superimpose
      'center
      (+ margin-size (pict-height card-art) margin-size)
      centered-description
      (superimpose
       padding (- height (* 3 padding) (pict-height detailed-description))
       detailed-description
       (superimpose
        (- width margin-size (pict-width defense))
        padding
        defense
        (superimpose
         margin-size
         padding
         attack
         (superimpose 'center
                      padding
                      name
                      (superimpose 'center
                                   margin-size
                                   (inset-art card-art)
                                   base)))))))))
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
  ;; TODO not right size when printed
  (define Card-height 3.8) ;; in inches


  (define scale-factor (/ (* Card-height num-columns) A4-height))
  (define target-height (* paper-height scale-factor))
  (send pdf-dc start-doc "cards")
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

(define (save-cards cardset-without-reference output-name)
  (define cardset
    (append cardset-without-reference
            (for/list ([card cardset-without-reference]
                       #:when (not (has-tag? card not-in-shop-tag)))
              (reference-card card))))

  (define cards-directory (build-path cards-dir output-name))
  ;; make the cards directory
  (make-directory cards-directory)

  (define all-picts
    (apply
     append
     (for/list ([card cardset])
       (for/list ([i (in-range (card-count-4-players card))])
         (define card-for-player (with-player-tag card i))
         (define card-file (build-path cards-directory (card-file-name card-for-player)))
         (define card-pict (render-card card-for-player))
         (unless (file-exists? card-file)
           (send (pict->bitmap card-pict) save-file card-file 'png))
         card-pict))))
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
    (map card-count-4-players base-game))

  ;; save tabletop code
  (define code-str (tabletop-code numbers))
  (define code-file (build-path cards-dir "code.lua"))
  (define output (open-output-file code-file #:exists 'truncate))
  (display code-str output)
  (close-output-port output)

  (send (pict->bitmap card-back) save-file (build-path cards-dir "back.png") 'png)

  (save-cards (append every-game base-game) "base-game")
  (save-cards booster1 "booster1"))




