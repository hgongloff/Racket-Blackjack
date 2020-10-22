#lang racket
(define faces '(2 3 4 5 6 7 8 9 10 J Q K A))
(define suits '(Clubs Diamonds Hearts Spades))

; This function makes and returns a deck of 52 cards 
(define make-deck
  (letrec [ (deck '())
            (card '())
            (make (lambda (list)
                    (set! deck (cons '(2 . Clubs) '()))
                    (set! deck (append deck (cons '(3 . Clubs) '())))
                    (set! deck (append deck (cons '(4 . Clubs) '())))
                    (set! deck (append deck (cons '(5 . Clubs) '())))
                    (set! deck (append deck (cons '(6 . Clubs) '())))
                    (set! deck (append deck (cons '(7 . Clubs) '())))
                    (set! deck (append deck (cons '(8 . Clubs) '())))
                    (set! deck (append deck (cons '(9 . Clubs) '())))
                    (set! deck (append deck (cons '(10 . Clubs) '())))
                    (set! deck (append deck (cons '(J . Clubs) '())))
                    (set! deck (append deck (cons '(Q . Clubs) '())))
                    (set! deck (append deck (cons '(K . Clubs) '())))
                    (set! deck (append deck (cons '(A . Clubs) '())))
                    
                    (set! deck (append deck (cons '(2 . Diamonds) '())))
                    (set! deck (append deck (cons '(3 . Diamonds) '())))
                    (set! deck (append deck (cons '(4 . Diamonds) '())))
                    (set! deck (append deck (cons '(5 . Diamonds) '())))
                    (set! deck (append deck (cons '(6 . Diamonds) '())))
                    (set! deck (append deck (cons '(7 . Diamonds) '())))
                    (set! deck (append deck (cons '(8 . Diamonds) '())))
                    (set! deck (append deck (cons '(9 . Diamonds) '())))
                    (set! deck (append deck (cons '(10 . Diamonds) '())))
                    (set! deck (append deck (cons '(J . Diamonds) '())))
                    (set! deck (append deck (cons '(Q . Diamonds) '())))
                    (set! deck (append deck (cons '(K . Diamonds) '())))
                    (set! deck (append deck (cons '(A . Diamonds) '())))

                    (set! deck (append deck (cons '(2 . Hearts) '())))
                    (set! deck (append deck (cons '(3 . Hearts) '())))
                    (set! deck (append deck (cons '(4 . Hearts) '())))
                    (set! deck (append deck (cons '(5 . Hearts) '())))
                    (set! deck (append deck (cons '(6 . Hearts) '())))
                    (set! deck (append deck (cons '(7 . Hearts) '())))
                    (set! deck (append deck (cons '(8 . Hearts) '())))
                    (set! deck (append deck (cons '(9 . Hearts) '())))
                    (set! deck (append deck (cons '(10 . Hearts) '())))
                    (set! deck (append deck (cons '(J . Hearts) '())))
                    (set! deck (append deck (cons '(Q . Hearts) '())))
                    (set! deck (append deck (cons '(K . Hearts) '())))
                    (set! deck (append deck (cons '(A . Hearts) '())))

                    (set! deck (append deck (cons '(2 . Spades) '())))
                    (set! deck (append deck (cons '(3 . Spades) '())))
                    (set! deck (append deck (cons '(4 . Spades) '())))
                    (set! deck (append deck (cons '(5 . Spades) '())))
                    (set! deck (append deck (cons '(6 . Spades) '())))
                    (set! deck (append deck (cons '(7 . Spades) '())))
                    (set! deck (append deck (cons '(8 . Spades) '())))
                    (set! deck (append deck (cons '(9 . Spades) '())))
                    (set! deck (append deck (cons '(10 . Spades) '())))
                    (set! deck (append deck (cons '(J . Spades) '())))
                    (set! deck (append deck (cons '(Q . Spades) '())))
                    (set! deck (append deck (cons '(K . Spades) '())))
                    (set! deck (append deck (cons '(A . Spades) '())))
                    
                    deck
            ))
            ] (make '())
  
  ))

; This function takes a given hand and adds the values of it together to find its 
; best possible value
(define eval-hand
  (lambda (hand)
; This function checks the given hand recusively to count the total number of aces it has
    (letrec [ (aces (lambda (hand)
                      (if (null? hand)
                          0
                          (if (eqv? (caar hand) 'A)
                              (begin
                                (+ 1 (aces (cdr hand))))
                              (aces (cdr hand))))))
              ]
; This function recursively calls itself to add together the total value of the hand given    
    (letrec [ (total (lambda (hand)
                       (if (null? hand)
                           0
                           (if (number? (caar hand))
                               (+ (total (cdr hand)) (caar hand))
                               (if (eqv? (caar hand) 'A)
                                   (+ (total (cdr hand)) 11)
                                   (+ (total (cdr hand)) 10))))
                           ))
                       ]
; This is the main function of the above function and goes through and gets the total and accounts for 
; if there is multiple aces and decides whether they need to be worth 1 or 11
    (letrec [ (start (lambda (hand)
                       (if (> (total hand) 21)
                           (if (> (aces hand) 0)
                               (if (> (- (total hand) 10) 21)
                                   (if (> (aces hand) 1)
                                       (if (> (- (total hand) 20) 21)
                                           (if (> (aces hand) 2)
                                               (if (> (- (total hand) 30) 21)
                                                   (if (> (aces hand) 3)
                                                       (- (total hand) 40)
                                                       (- (total hand) 30))
                                                   (- (total hand) 30))
                                               (- (total hand) 20))
                                           (- (total hand) 20))
                                       (- (total hand) 10))
                                   (- (total hand) 10))
                               (total hand))
                           (total hand))))]
                               
(start hand)
)))))

; This function deals two hands from the deck. One for the player and one for the dealer
(define-syntax-rule
  (deal! deck)
  (let ([firstcard '(())]
        [secondcard '(())]
        [hand '(())]
        [item 0]
        [item2 0])
    (set! item (caar deck))
    (set! item2 (cdar deck))
    (set! firstcard (cons (cons item item2) '()))
    (set! deck (cdr deck))
     (set! item (caar deck))
    (set! item2 (cdar deck))
    (set! deck (cdr deck))
    (set! secondcard (cons (cons item item2) '()))
    (set! hand (append firstcard secondcard))
    hand
           
         ))

; This function simulates a hit in blackjack.
; Taking the top card from the deck and giving it to the given hand.
(define-syntax-rule
  (hit! deck hand)
  (let ([newcard '(())]
        [item 0]
        [item2 0])
    (set! item (caar deck))
    (set! item2 (cdar deck))
    (set! newcard (cons (cons item item2) '()))
    (set! deck (cdr deck))
    (set! hand (append hand newcard))
    
    ))

; This function shows the given hand and will show either the entire hand 
; or just a part of it depending on what is asked for. 
; It also displays a given short description before showing the hand.
(define show-hand
  (lambda (hand how description)    
; This function displays the hand that is given and either displays all of it or all but the first card                
    (letrec [ (show (lambda (hand how)
                      (if (number? (caar hand))
                          (begin 
                            (display (caar hand))
                            (display " of ")
                            (display (cdar hand))
                            (set! hand (cdr hand))
                            (if (null? hand)
                                (display "")
                                (begin
                                  (display " and a ")
                                  (show hand how)
                                  ))
                            )
                          (if (eqv? 'A (caar hand))
                              (begin 
                                (display "Ace of ")
                                (display (cdar hand))
                                (set! hand (cdr hand))
                                (if (null? hand)
                                    (display "")
                                    (begin
                                      (display " and a ")
                                      (show hand how)
                                      )))
                              (if (eqv? 'K (caar hand))
                                  (begin 
                                    (display "King of ")
                                    (display (cdar hand))
                                    (set! hand (cdr hand))
                                    (if (null? hand)
                                        (display "")
                                        (begin
                                          (display " and a ")
                                          (show hand how)
                                          )))
                                  (if (eqv? 'Q (caar hand))
                                      (begin
                                        
                                        (display "Queen of ")
                                        (display (cdar hand))
                                        (set! hand (cdr hand))
                                        (if (null? hand)
                                            (display "")
                                            (begin
                                              (display " and a ")
                                              (show hand how)
                                              )))
                                      (begin 
                                        (display "Jack of ")
                                        (display (cdar hand))
                                        (set! hand (cdr hand))
                                        (if (null? hand)
                                            (display "")
                                            (begin
                                              (display " and a ")
                                              (show hand how)
                                              )))
                                      )))
                          )
                                          
                          )
                      
                      )
                      ]
; This is the main function of the above functiion
      (letrec [ (start (lambda (hand how description)
                         (display description)
                         (if (eqv? how 'Part)
                             (begin
                               (display "*******")
                               (display " and a ")
                               (set! hand (cdr hand))
                               (show hand how))
                             (begin
                               (display "A ")
                               (show hand how)))))
                ]
        (start hand how description)
                         
))))

; Here I define a few global variables to be used
(define thedeck '(()))
(define playerhand '(()))
(define dealerhand '(()))
(define bet 0)

; This main funtion runs the user interface of the game
(define game
  (lambda (thedeck playerhand dealerhand)
; This function asks the player for a bet and returns the bet so long as it is more than 0 and less 
; than or equal to the max chips the player has
    (letrec [ (getbet (lambda (bet maxbet)
                        (display "How much would you like to bet? ")
                        (display "(you have ")
                        (display (number->string maxbet))
                        (display " chips left) \n")
                        (set! bet (string->number (read-line)))
                        (if (> bet maxbet)
                            (begin
                              (display "That bet is to much try again. \n")
                              (getbet 0 maxbet))
                            (if (< bet 0)
                                (begin
                                  (display "You cant bet less than 0. \n")
                                  (getbet 0 maxbet))
                                bet))))
              ]
; This function asks the player whether they want to play another round and resets the game if they do.
    (letrec [ (playagain (lambda (item)
                           (display "Do you want to play another round? (Enter 1 for yes and anything else for no) \n")
                           (set! item (string->number (read-line)))
                           (if (eqv? item 1)
                               #t
                               #f)))
              ]
; This is the main function of the game and runs the main parts of it.
; It asks the user what they would like to do each hand and does what is asked. 
    (letrec [ 
              (value 0)
              (start (lambda (thedeck playerhand dealerhand deal? input? input chips bet)
                       (if (= chips 0)
                           (display "Sorry you are out of chips goodbye.")
                           (begin
			; This only exevutes if a new game has begun and the deck needs to be reshuffled and 
			; new hands need to be dealt.
                             (if deal?
                                 (begin
                                   (set! bet (getbet 0 chips))
                                   (set! thedeck make-deck)
                                   (set! thedeck (shuffle thedeck))
                                   (set! dealerhand (deal! thedeck))
                                   (set! playerhand (deal! thedeck))
                                   ;(length thedeck)
                                   (display "The dealer has dealt\n")
                                   (show-hand dealerhand 'Part "The dealer has: ")
                                   (set! value (eval-hand (cdr dealerhand)))
                                   (display ". This is worth ")
                                   (display value)
                                   (display " points.")
                                   (display "\n")
                                   (show-hand playerhand 'Full "You have: ")
                                   (set! value (eval-hand playerhand))
                                   (display ". Combined this is worth ")
                                   (display value)
                                   (display " points.")
                                   (display "\n"))
                                 (display ""))
				; This only executes if user input is needed for what to do next
                             (if input?
                                 (begin
                                   (display "What would you like to do?\n (Enter 1-5 for the following)\n 1. Hit \n 2. Stay \n 3. Double Down \n 4. See your hand \n 5. See dealer hand \n 6. See chip count \n 7. Quit \n")
                                   (set! input (string->number (read-line))))
                                 (display ""))
				; This only executes if the player chooses 1 to hit it then simulates the hit 
				; and shows the player their new hand
                             (if (eqv? input 1)
                                 (begin 
                                   (hit! thedeck playerhand)
                                   (show-hand playerhand 'Full "You now have: ")
                                   (display "\n")
                                   (if (> (eval-hand playerhand) 21)
                                       (begin
                                         (display "Sorry but ")
                                         (display (number->string (eval-hand playerhand)))
                                         (display " is greater than 21 so you lose. \n")
                                         (set! chips (- chips bet))
                                         (if (playagain 0)
                                             (start thedeck playerhand dealerhand #t #t 0 chips bet)
                                             (display "Goodbye")
                                             ))
                                       (start thedeck playerhand dealerhand #f #t 0 chips bet)
                                       ))
				; This exevites if the player chooses to stay. It then has the dealer 
				; hit until they reache at least 17
                                 (if (eqv? input 2)
                                     (if (> (eval-hand dealerhand) 16)
                                         (begin
                                           (show-hand dealerhand 'Full "The dealer has: ")
                                           (display "\n")
                                           (display "The dealer stands at ")
                                           (display (number->string (eval-hand dealerhand)))
                                           (display "\n")
                                           (if (> (eval-hand dealerhand) 21)
                                               (begin
                                                 (display "The dealer busts. You win!\n")
                                                  (set! chips (+ chips bet))
                                                 (if (playagain 0)
                                                     (start thedeck playerhand dealerhand #t #t 0 chips bet)
                                                     (display "Goodbye")))                                             
                                               (if (> (eval-hand dealerhand) (eval-hand playerhand))
                                                   (begin
                                                     (display "Sorry but you lose.\n")
                                                      (set! chips (- chips bet))
                                                     (if (playagain 0)
                                                         (start thedeck playerhand dealerhand #t #t 0 chips bet)
                                                         (display "Goodbye")))
                                                   (if (= (eval-hand dealerhand) (eval-hand playerhand))
                                                       (begin 
                                                         (display "It's a tie.\n")
                                                         (if (playagain 0)
                                                             (start thedeck playerhand dealerhand #t #t 0 chips bet)
                                                             (display "Goodbye")))
                                                       (begin 
                                                         (display "You win!\n")
                                                          (set! chips (+ chips bet))
                                                         (if (playagain 0)
                                                             (start thedeck playerhand dealerhand #t #t 0 chips bet)
                                                             (display "Goodbye")))))))
                                         (begin
                                           (show-hand dealerhand 'Full "The dealer has: ")
                                           (display "\n")
                                           (display "The dealer hits.\n")
                                           (hit! thedeck dealerhand)
                                           (start thedeck playerhand dealerhand #f #f 2 chips bet)))
				; This only executes if the player chooses to double down. It then has the player 
				; double their bet for one more hit and then they have to stay 
                                     (if (eqv? input 3)
                                         (if (> (* bet 2) chips)
                                             (begin
                                               (display "Sorry you cant double down you dont have enough chips.\n")
                                               (start thedeck playerhand dealerhand #f #f 0 chips bet))
                                             (begin
                                               (set! bet (* bet 2))
                                               (hit! thedeck playerhand)
                                               (show-hand playerhand 'Full "You now have: ")
                                               (display "\n")
                                               (if (> (eval-hand playerhand) 21)
                                                   (begin
                                                     (display "Sorry but ")
                                                     (display (number->string (eval-hand playerhand)))
                                                     (display " is greater than 21 so you lose. \n")
                                                     (set! chips (- chips bet))
                                                     (if (playagain 0)
                                                         (start thedeck playerhand dealerhand #t #t 0 chips bet)
                                                         (display "Goodbye")
                                                         ))
                                                   (start thedeck playerhand dealerhand #f #f 2 chips bet)
                                                   )))
					; This only executes if the player wants to see their hand. It 
					; then shows the player their current hand
                                         (if (eqv? input 4)
                                             (begin
                                               (show-hand playerhand 'Full "You have: ")
                                               (display ". Combined that is worth ")
                                               (display (number->string (eval-hand playerhand)))
                                               (display " points.")
                                               (display "\n")
                                               (start thedeck playerhand dealerhand #f #t 0 chips bet))
					; This shows the player the dealers hand
                                             (if (eqv? input 5)
                                                 (begin 
                                                   (show-hand dealerhand 'Part "The dealer has: ")
                                                   (display ". Combined that is worth ")
                                                   (display (number->string (eval-hand (cdr dealerhand))))
                                                   (display " points.")
                                                   (display "\n")
                                                   (start thedeck playerhand dealerhand #f #t 0 chips bet))
						; This shows the player how many chips they currently have
                                                 (if (eqv? input 6)
                                                     (begin
                                                       (display "You have ")
                                                       (display chips)
                                                       (display " chips\n")
                                                       (start thedeck playerhand dealerhand #f #t 0 chips bet))
						; This exits the program when the player wants to quit
                                                     (if (eqv? input 7)
                                                         (display "Bye")
                                                         (begin
                                                           (display "That was not a valid input try again\n")
                                                           (start thedeck playerhand dealerhand #f #t 0 chips)
                                                           )))))))
                                           
                                     
                                   
                                 )
                           
                             ))))
              ]
; This function begins the game and does the first round by making and shuffling the deck and and 
; then dealing 2 hands one to the dealer and one to the player 
      (letrec [ (beginn (lambda (value)
                          (set! bet (getbet 0 100))
                          (set! thedeck make-deck)
                          (set! thedeck (shuffle thedeck))
                          (set! dealerhand (deal! thedeck))
                          (set! playerhand (deal! thedeck))
                       
                          (display "The dealer has dealt\n")
                          (show-hand dealerhand 'Part "The dealer has: ")
                          (set! value (eval-hand (cdr dealerhand)))
                          (display ". This is worth ")
                          (display value)
                          (display " points.")
                          (display "\n")
                          (show-hand playerhand 'Full "You have: ")
                          (set! value (eval-hand playerhand))
                          (display ". Combined this is worth ")
                          (display value)
                          (display " points.")
                          (display "\n")
                          (start thedeck playerhand dealerhand #f #t 0 100 bet)       
                                   
        
      ))
                ]
        (beginn 0)
        ))))))

; This is a short intro to the game and starts out by asking if the user wants to play a 
; game of Blackjack.
(display "Welcome to Blackjack!\n")
(display "To start the game please enter 1. To exit enter anything else.\n")
(define input (string->number (read-line)))

; If the player wants to play they need to enter 1 and then the game begins.
(if (eqv? input 1)
    (begin
      (display "The game begins now!\nYou will have 100 chips to play with have fun!\n")
      (game '(()) '(()) '(())))
    (display "Goodbye!\n"))















 

