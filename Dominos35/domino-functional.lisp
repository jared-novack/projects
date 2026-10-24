;;; DOMINO GAME: FUNCTIONAL COMMON LISP IMPLEMENTATION
;;;
;;; Data conventions:
;;;   A tile is (left-pips right-pips spinner-p).
;;;   A player state is (hand round-score tournament-score).
;;;   DEAL-STARTING-HANDS returns (human-hand computer-hand remaining-deck).
;;;   SHUFFLE-DECK returns (shuffled-deck next-random-seed).
;;;
;;; Core game-state functions return new values and do not modify their inputs.
;;; Runtime randomness, input, file access, and printed output are boundary actions.
;;; Each function below has its own documentation header.
;;;
;;; Rule and generator constants used throughout the game.
(defconstant +initial-hand-size+ 7 "Tiles dealt to each player at the start of a round.")
(defconstant +maximum-domino-pips+ 6 "Highest pip count in the double-six domino set.")
(defconstant +round-target-three+ 3 "First permitted round scoring target.")
(defconstant +round-target-five+ 5 "Second permitted round scoring target.")
(defconstant +random-seed-limit+ 2147483646 "Largest positive seed accepted by the game generator.")
(defconstant +random-seed-modulus+ 2147483647 "Modulus used by the Park-Miller random sequence.")
(defconstant +random-seed-multiplier+ 48271 "Multiplier used by the Park-Miller random sequence.")
(defconstant +move-code-first+ -1 "Move record code for the first tile on an empty board.")
(defconstant +move-code-left+ 0 "Move record code for a placement at the left end.")
(defconstant +move-code-right+ 1 "Move record code for a placement at the right end.")
(defconstant +move-code-up+ 2 "Move record code for a placement at the upper spinner end.")
(defconstant +move-code-down+ 3 "Move record code for a placement at the lower spinner end.")


;;; SECTION 1: GAME STARTUP AND INPUT

;;; /***************************************************************************
;;; **Function Name:** start-game
;;; **Purpose:** Prompt for a new game and create its first round setup.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** A new-game tournament result, or the result of resuming a loaded game.
;;; **Algorithm:**
;;;   1) Ask whether to play a new game or load a saved game.
;;;   2) For a new game, read a nonnegative tournament target.
;;;   3) Start a round and return its state with the tournament target.
;;;   4) Read and resume the saved game when L is selected.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The startup prompts and round setup rules came from Round.cpp.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun start-game ()
  (let ((choice (read-game-choice)))
    (cond
      ((eq choice 'play) (start-new-game))
      ((eq choice 'load) (load-lisp-game))
      (t nil))))


;;; /***************************************************************************
;;; **Function Name:** read-game-choice
;;; **Purpose:** Read whether to begin a new game or request a saved-game load.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** The symbol PLAY or LOAD, or NIL when input ends.
;;; **Algorithm:**
;;;   1) Prompt for P or L and read one complete input line.
;;;   2) Normalize the response to uppercase and accept only P or L.
;;;   3) Report invalid input and repeat until a valid choice or end of input.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The prompt choices come from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-game-choice ()
  (format t "Play (P) or Load (L)? ")
  (finish-output)
  (read-game-choice-line (read-line *standard-input* nil nil)))


;;; /***************************************************************************
;;; **Function Name:** read-game-choice-line
;;; **Purpose:** Validate one response to the new-game or load prompt.
;;; **Parameters:**
;;;   response - input line read from the player; string or NIL at end of input.
;;; **Return Value:** The symbol PLAY or LOAD, or NIL when input ends.
;;; **Algorithm:**
;;;   1) Return NIL when there is no input line.
;;;   2) Accept P or L without regard to letter case.
;;;   3) Report invalid input and request another choice.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The prompt choices come from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-game-choice-line (response)
  (cond
    ((null response) nil)
    ((string-equal (string-trim '(#\Space #\Tab #\Return) response) "P") 'play)
    ((string-equal (string-trim '(#\Space #\Tab #\Return) response) "L") 'load)
    (t
     (format t "INVALID INPUT~%")
     (read-game-choice))))


;;; /***************************************************************************
;;; **Function Name:** start-new-game
;;; **Purpose:** Read the tournament target and set up the first round of a new game.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** A two-item game value (tournament-target round-state), or NIL at end of input.
;;;
;;; **Algorithm:**
;;;   1) Read a nonnegative tournament target.
;;;   2) Ask for a valid round target and generate a random seed.
;;;   3) Create and display the initial round state.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The prompts and setup sequence come from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun start-new-game ()
  (let ((tournament-target (read-tournament-target)))
    (cond
      ((null tournament-target) nil)
      (t
       (start-tournament-with-first-round
         tournament-target (start-round (generate-random-seed)))))))


;;; /***************************************************************************
;;; **Function Name:** read-tournament-target
;;; **Purpose:** Read a nonnegative tournament ending score from standard input.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** A nonnegative integer, or NIL when input ends.
;;; **Algorithm:**
;;;   1) Prompt for an integer and read a complete line.
;;;   2) Accept only a whole nonnegative integer.
;;;   3) Report invalid input and repeat until valid input or end of input.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The nonnegative-score rule comes from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-tournament-target ()
  (format t "Enter valid Tournament Score to end Rounds at: ")
  (finish-output)
  (let ((response (read-line *standard-input* nil nil)))
    (cond
      ((null response) nil)
      (t
       (let ((value (parse-complete-integer response)))
         (cond ((and value (>= value 0)) value)
               (t
                (format t "INVALID INPUT~%")
                (read-tournament-target))))))))


;;; /***************************************************************************
;;; **Function Name:** read-tournament-target-line
;;; **Purpose:** Validate a tournament target entered as a line of text.
;;; **Parameters:**
;;;   response - input line read from the player; string or NIL at end of input.
;;; **Return Value:** A nonnegative integer, or NIL when input ends.
;;; **Algorithm:**
;;;   1) Parse the response as one complete integer.
;;;   2) Accept it when its value is zero or greater.
;;;   3) Report invalid input and prompt again otherwise.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The nonnegative-score rule comes from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-tournament-target-line (response)
  (cond
    ((null response) nil)
    (t
     (read-tournament-target-value
       response
       (parse-complete-integer response)))))


;;; /***************************************************************************
;;; **Function Name:** read-tournament-target-value
;;; **Purpose:** Accept a parsed nonnegative tournament target or request another entry.
;;; **Parameters:**
;;;   response - original tournament-target input line; string passed by value.
;;;   parsed-value - integer parsed from the response, or NIL for invalid text.
;;; **Return Value:** A nonnegative integer, or NIL when input ends.
;;; **Algorithm:**
;;;   1) Return a parsed value when it is nonnegative.
;;;   2) Otherwise report invalid input and read another line.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The nonnegative-score rule comes from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-tournament-target-value (response parsed-value)
  (cond
    ((and parsed-value (>= parsed-value 0)) parsed-value)
    (t
     (format t "INVALID INPUT~%")
     (read-tournament-target))))


;;; /***************************************************************************
;;; **Function Name:** parse-complete-integer
;;; **Purpose:** Parse an input line only when the entire non-whitespace line is an integer.
;;; **Parameters:**
;;;   response - input line to parse; string passed by value.
;;; **Return Value:** The parsed integer, or NIL when the line is not one complete integer.
;;; **Algorithm:**
;;;   1) Parse an integer while allowing the parser to report trailing text.
;;;   2) Accept the result only when all remaining characters are whitespace.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun parse-complete-integer (response)
  (multiple-value-bind (value position)
      (parse-integer response :junk-allowed t)
    (cond
      ((null value) nil)
      ((string= "" (string-trim '(#\Space #\Tab #\Return)
                                (subseq response position)))
       value)
      (t nil))))


;;; SECTION 2: ROUND SETUP

;;; /***************************************************************************
;;; **Function Name:** start-round
;;; **Purpose:** Prompt for a valid round target and set up the starting deal.
;;; **Parameters:**
;;;   seed - deterministic random state for the shuffle; integer passed by value.
;;; **Return Value:** A round state with target, players, boneyard, starter, draw history, board,
;;;   and next seed;
;;;   NIL when input ends before a valid target is entered.
;;; **Algorithm:**
;;;   1) Read a round target of 3 or 5.
;;;   2) Shuffle the double-six set, deal both seven-tile hands, and compare starter draws.
;;;   3) Return compared tiles to the boneyard in draw order and package the round state.
;;;   4) Print the initial hands, starter draw, board, and remaining boneyard size.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The round setup sequence comes from Round.cpp and Deck.cpp.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun start-round (seed)
  (start-round-after-target (read-round-target) seed))


;;; /***************************************************************************
;;; **Function Name:** read-round-target
;;; **Purpose:** Read a round scoring target of 3 or 5 from standard input.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** Integer 3 or 5, or NIL when input ends.
;;; **Algorithm:**
;;;   1) Prompt for one complete integer line.
;;;   2) Accept only 3 or 5; report invalid input and repeat otherwise.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The valid round targets come from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-round-target ()
  (format t "Enter Target Score of 3 or 5 for Round: ")
  (finish-output)
  (read-round-target-line (read-line *standard-input* nil nil)))


;;; /***************************************************************************
;;; **Function Name:** read-round-target-line
;;; **Purpose:** Validate one round-target input line.
;;; **Parameters:**
;;;   response - input line read from the player; string or NIL at end of input.
;;; **Return Value:** Integer 3 or 5, or NIL when input ends.
;;; **Algorithm:**
;;;   1) Parse the response as one complete integer.
;;;   2) Accept it only when it is 3 or 5.
;;;   3) Report invalid input and prompt again otherwise.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The valid round targets come from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-round-target-line (response)
  (cond
    ((null response) nil)
    (t
     (read-round-target-value
       (parse-complete-integer response)))))


;;; /***************************************************************************
;;; **Function Name:** read-round-target-value
;;; **Purpose:** Accept a valid round target or ask the player again.
;;; **Parameters:**
;;;   parsed-value - integer parsed from the entry, or NIL for invalid text.
;;; **Return Value:** Integer 3 or 5, or NIL when input ends.
;;; **Algorithm:**
;;;   1) Accept parsed value 3 or 5.
;;;   2) Report invalid input and read another target otherwise.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The valid round targets come from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-round-target-value (parsed-value)
  (cond
    ((and parsed-value (valid-round-target-p parsed-value)) parsed-value)
    (t
     (format t "INVALID INPUT~%")
     (read-round-target))))


;;; /***************************************************************************
;;; **Function Name:** start-round-after-target
;;; **Purpose:** Build a new round state for a chosen target and seed.
;;; **Parameters:**
;;;   target - valid target score, 3 or 5; integer passed by value.
;;;   seed - deterministic random state for the shuffle; integer passed by value.
;;; **Return Value:** A round state, or NIL when the target is NIL.
;;; **Algorithm:**
;;;   1) Shuffle the domino set from the supplied seed.
;;;   2) Deal seven tiles to each player and use the remaining tiles to select the starter.
;;;   3) Store the returned boneyard, player states, empty board, draw history, and next seed.
;;;   4) Print a concise report of the round setup.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Deal and starter rules come from Round.cpp and Deck.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun start-round-after-target (target seed)
  (cond
    ((null target) nil)
    (t
     (print-round-start-result
       (make-round-start-state target seed)))))


;;; /***************************************************************************
;;; **Function Name:** make-round-start-state
;;; **Purpose:** Create a deterministic round state from a target and shuffle seed.
;;; **Parameters:**
;;;   target - valid target score, 3 or 5; integer passed by value.
;;;   seed - deterministic random state for the shuffle; integer passed by value.
;;; **Return Value:** A round state shaped (target human-state computer-state stock starter
;;;   draw-history, board next-seed).
;;; **Algorithm:**
;;;   1) Shuffle the full domino set and deal both starting hands.
;;;   2) Compare pairs from the remaining stock until one player has the higher pip sum.
;;;   3) Return the draw tiles to the stock through starter selection and package the result.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Deal and starter rules come from Round.cpp and Deck.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-round-start-state (target seed)
  (make-round-start-state-from-values
    target
    (shuffle-deck (make-domino-set) seed)))


;;; /***************************************************************************
;;; **Function Name:** make-round-start-state-from-values
;;; **Purpose:** Deal and select the starter from an already shuffled deck result.
;;; **Parameters:**
;;;   target - valid target score, 3 or 5; integer passed by value.
;;;   shuffle-result - shuffled tile list paired with its next seed; list passed by value.
;;; **Return Value:** A round state shaped (target human-state computer-state stock starter
;;;   draw-history, board next-seed).
;;; **Algorithm:**
;;;   1) Deal the human and computer hands from the shuffled deck.
;;;   2) Select the starter from the remaining tiles and retain the resulting boneyard.
;;;   3) Package each hand with zero round and tournament scores.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Deal and starter rules come from Round.cpp and Deck.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-round-start-state-from-values (target shuffle-result)
  (make-round-start-state-from-deal
    target
    (deal-starting-hands (car shuffle-result))
    (cadr shuffle-result)))


;;; /***************************************************************************
;;; **Function Name:** make-round-start-state-from-deal
;;; **Purpose:** Add starter selection and the initial board to the dealt hands.
;;; **Parameters:**
;;;   target - valid target score, 3 or 5; integer passed by value.
;;;   dealt-state - human hand, computer hand, and remaining boneyard.
;;;   next-seed - random seed following the shuffle; integer passed by value.
;;; **Return Value:** A round state shaped (target human-state computer-state stock starter
;;;   draw-history, board next-seed).
;;; **Algorithm:**
;;;   1) Determine who starts from a drawn pair on the stock.
;;;   2) Package the hands, returned stock, starter, draw history, board, and next seed.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Deal and starter rules come from Round.cpp and Deck.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-round-start-state-from-deal (target dealt-state next-seed)
  (make-round-start-state-from-selection
    target
    (car dealt-state)
    (cadr dealt-state)
    (choose-first-player (caddr dealt-state))
    next-seed))


;;; /***************************************************************************
;;; **Function Name:** make-round-start-state-from-selection
;;; **Purpose:** Package both player states and the selected starter into round state.
;;; **Parameters:**
;;;   target - valid target score, 3 or 5; integer passed by value.
;;;   human-hand - seven tiles dealt to the human player.
;;;   computer-hand - seven tiles dealt to the computer player.
;;;   selection - starter, draw history, and boneyard after the draw.
;;;   next-seed - random seed following the shuffle; integer passed by value.
;;; **Return Value:** A round state shaped (target human-state computer-state stock starter
;;;   draw-history, board next-seed).
;;; **Algorithm:**
;;;   1) Create player states with zero round and tournament scores.
;;;   2) Retain the starter draw history and boneyard.
;;;   3) Create an empty board for the selected target and return all round fields.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Deal and starter rules come from Round.cpp and Deck.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-round-start-state-from-selection
    (target human-hand computer-hand selection next-seed)
  (list target
        (list human-hand 0 0)
        (list computer-hand 0 0)
        (caddr selection)
        (car selection)
        (cadr selection)
        (make-empty-board target)
        next-seed))


;;; /***************************************************************************
;;; **Function Name:** print-round-start-result
;;; **Purpose:** Display the round target, human hand, starter selection, and initial board.
;;; **Parameters:**
;;;   round-state - round target, player states, boneyard, starter, history, board, and seed.
;;; **Return Value:** The same round-state value after printing its setup summary.
;;; **Algorithm:**
;;;   1) Print the round target and human starting hand.
;;;   2) Print the starter draw history and selected player.
;;;   3) Print the empty board.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Display conventions come from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-round-start-result (round-state)
  (format t "~%Round target: ~a~%" (car round-state))
  (format t "~%Human hand (7 tiles):~%")
  (print-tile-list (player-hand (cadr round-state)))
  (print-first-player-selection
    (list (fifth round-state) (sixth round-state) (fourth round-state)))
  round-state)


;;; SECTION 3: HUMAN AND COMPUTER TURN PROCESSING

;;; /***************************************************************************
;;; **Function Name:** valid-domino-pips
;;; **Purpose:** Check whether a value is a pip count in a double-six set.
;;; **Parameters:** pips - candidate pip value; integer or NIL.
;;; **Return Value:** T for an integer from 0 through 6; otherwise NIL.
;;; **Algorithm:** Check the value type and inclusive range.
;;; **Reference:** Double-six pip range from Deck.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun valid-domino-pips (pips)
  (and (integerp pips) (<= 0 pips) (<= pips +maximum-domino-pips+)))


;;; /***************************************************************************
;;; **Function Name:** find-hand-tile
;;; **Purpose:** Find a tile matching two pips without changing hand order.
;;; **Parameters:** hand - ordered tiles; first-pips and second-pips - entered pips.
;;; **Return Value:** Matching stored tile or NIL.
;;; **Algorithm:** Recursively compare each tile in both pip orientations.
;;; **Reference:** Domino identity uses the unordered pip pair.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun find-hand-tile (hand first-pips second-pips)
  (cond ((null hand) nil)
        ((or (and (= (tile-left-pips (car hand)) first-pips)
                  (= (tile-right-pips (car hand)) second-pips))
             (and (= (tile-left-pips (car hand)) second-pips) (= (tile-right-pips (car hand))
             first-pips)))
         (car hand))
        (t (find-hand-tile (cdr hand) first-pips second-pips))))


;;; /***************************************************************************
;;; **Function Name:** move-code-direction
;;; **Purpose:** Convert a move record direction code to a board direction.
;;; **Parameters:** code - direction code from a move record.
;;; **Return Value:** FIRST, LEFT, RIGHT, UP, DOWN, or NIL.
;;; **Algorithm:** Match the supplied code to its direction symbol.
;;; **Reference:** Codes are documented by GET-ALL-MOVES.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun move-code-direction (code)
  (cond ((eql code +move-code-first+) 'first) ((eql code +move-code-left+) 'left) ((eql code
  +move-code-right+) 'right)
        ((eql code +move-code-up+) 'up) ((eql code +move-code-down+) 'down) (t nil)))


;;; /***************************************************************************
;;; **Function Name:** remove-hand-tile
;;; **Purpose:** Build a hand without the first matching physical tile.
;;; **Parameters:** hand - original hand; tile - tile to remove by pip pair.
;;; **Return Value:** A new hand with the matching tile removed, or original hand.
;;; **Algorithm:** Scan recursively while carrying a reversed prefix, then restore it.
;;; **Reference:** Hand removal from Player.cpp implemented without mutation.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun remove-hand-tile (hand tile)
  (remove-hand-tile-acc hand tile nil))


;;; /***************************************************************************
;;; **Function Name:** remove-hand-tile-acc
;;; **Purpose:** Scan for a tile while carrying the reversed hand prefix.
;;; **Parameters:** hand - tiles remaining; tile - tile to remove; reversed-prefix - saved prefix.
;;; **Return Value:** Hand with the first matching tile removed.
;;; **Algorithm:** Restore on exhaustion; skip nonmatches; join prefix to suffix on match.
;;; **Reference:** Pure hand removal corresponding to Player.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun remove-hand-tile-acc (hand tile reversed-prefix)
  (cond ((null hand) (restore-prefix reversed-prefix nil))
        ((and (= (tile-left-pips (car hand)) (tile-left-pips tile))
              (= (tile-right-pips (car hand)) (tile-right-pips tile)))
         (restore-prefix reversed-prefix (cdr hand)))
        (t (remove-hand-tile-acc (cdr hand) tile (cons (car hand) reversed-prefix)))))


;;; /***************************************************************************
;;; **Function Name:** player-with-hand
;;; **Purpose:** Return a player state with a replacement hand and existing scores.
;;; **Parameters:** player-state - original state; hand - replacement tile list.
;;; **Return Value:** New player state containing the hand and previous scores.
;;; **Algorithm:** Retain both scores while replacing the hand field.
;;; **Reference:** Player state conventions in this Lisp file.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun player-with-hand (player-state hand)
  (list hand (player-round-score player-state) (player-tournament-score player-state)))


;;; /***************************************************************************
;;; **Function Name:** board-with-pass-check
;;; **Purpose:** Return a board with a new consecutive-pass value.
;;; **Parameters:** board - current board; pass-check - new pass marker.
;;; **Return Value:** New board retaining all fields except PASS-CHECK.
;;; **Algorithm:** Rebuild the board from its current fields and supplied pass marker.
;;; **Reference:** Board state conventions in this Lisp file and Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-with-pass-check (board pass-check)
  (make-board-state (board-horizontal board) (board-vertical board) (board-target board)
                    (board-ends board) (board-spinner-state board) pass-check (board-last-play
                    board)))


;;; /***************************************************************************
;;; **Function Name:** complete-player-placement
;;; **Purpose:** Remove a played tile, update the board, and award turn points.
;;; **Parameters:** player-state - player state; board - current board; stock - boneyard;
;;;   tile - selected tile; direction - legal direction.
;;; **Return Value:** (updated-player updated-board stock PLAYED).
;;; **Algorithm:** Place the tile and score the result, remove it from a new hand, and return
;;;   updated values.
;;; **Reference:** Placement and score rules from Board.cpp and Player.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun complete-player-placement (player-state board stock tile direction)
  (let* ((new-board (place-board-tile board tile direction))
         (score-result (score-player-turn player-state new-board (board-last-play new-board)))
         (new-hand (remove-hand-tile (player-hand player-state) tile)))
    (list (player-with-hand (car score-result) new-hand) new-board stock 'played)))


;;; /***************************************************************************
;;; **Function Name:** list-item-at-index
;;; **Purpose:** Read the item at a zero-based position in a list.
;;; **Parameters:** items - ordered list; index - zero-based integer position.
;;; **Return Value:** The item or NIL when the list ends before the index.
;;; **Algorithm:** Return at zero, otherwise recur on the tail with a decremented index.
;;; **Reference:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun list-item-at-index (items index)
  (cond ((null items) nil) ((= index 0) (car items))
        (t (list-item-at-index (cdr items) (1- index)))))


;;; Human input and turn processing

;;; /***************************************************************************
;;; **Function Name:** human-turn
;;; **Purpose:** Run one interactive human turn, including any required draws.
;;; **Parameters:** player-state - human state; board - current board; stock - boneyard.
;;; **Return Value:** (updated-player updated-board updated-stock result), result PLAYED, PASSED, or
;;;   INPUT-ENDED.
;;; **Algorithm:** Show the position, accept legal actions, and repeat until a play, pass, or input
;;;   end.
;;; **Reference:** Turn rules from Human.cpp; state updates are functional.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun human-turn (player-state board stock)
  (format t "~%HUMAN TURN~%Your hand: ")
  (print-tile-list (player-hand player-state))
  (print-board board)
  (human-turn-action-loop player-state board stock))


;;; /***************************************************************************
;;; **Function Name:** human-turn-action-loop
;;; **Purpose:** Read actions and retry invalid entries without redrawing the board.
;;; **Parameters:** player-state - human state; board - current board; stock - boneyard.
;;; **Return Value:** Turn result tuple with PLAYED, PASSED, or INPUT-ENDED status.
;;; **Algorithm:** Prompt for an action, repeat on invalid input, and route valid actions.
;;; **Reference:** Turn rules from Human.cpp; state updates are functional.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun human-turn-action-loop (player-state board stock)
  (format t "Place (T), draw (D), help (H), or pass (P). Action: ")
  (finish-output)
  (let ((action
          (read-human-turn-action (read-line *standard-input* nil nil))))
    (cond
      ((eq action 'input-ended)
       (format t "Input ended; the human turn was not completed.~%")
       (list player-state board stock 'input-ended))
      ((eq action 'invalid)
       (format t "INVALID INPUT~%")
       (human-turn-action-loop player-state board stock))
      (t
       (let ((result (human-turn-after-action player-state board stock action)))
         (cond
           ((eq (fourth result) 'retry)
            (human-turn (first result) (second result) (third result)))
           (t result)))))))


;;; /***************************************************************************
;;; **Function Name:** read-human-turn-action
;;; **Purpose:** Normalize one human turn action from a line of input.
;;; **Parameters:** response - line entered by the human or NIL at input end.
;;; **Return Value:** PLACE, DRAW, HELP, PASS, INVALID, or INPUT-ENDED.
;;; **Algorithm:** Accept T, D, H, or P regardless of case; return INVALID otherwise.
;;; **Reference:** Action choices from Human.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-human-turn-action (response)
  (cond ((null response) 'input-ended)
        ((string-equal (string-trim '(#\Space #\Tab #\Return) response) "T") 'place)
        ((string-equal (string-trim '(#\Space #\Tab #\Return) response) "D") 'draw)
        ((string-equal (string-trim '(#\Space #\Tab #\Return) response) "H") 'help)
        ((string-equal (string-trim '(#\Space #\Tab #\Return) response) "P") 'pass)
        (t 'invalid)))


;;; /***************************************************************************
;;; **Function Name:** human-turn-after-action
;;; **Purpose:** Enforce action availability and route the human action.
;;; **Parameters:** player-state - human state; board - current board; stock - boneyard; action -
;;;   normalized action symbol.
;;; **Return Value:** Turn result tuple with PLAYED, PASSED, INPUT-ENDED, or RETRY status.
;;; **Algorithm:** Show legal-move help; allow placement only with a legal move, drawing only
;;;   when no move is legal and stock remains, or passing only when both are absent.
;;; **Reference:** Turn rules from Human.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/

(defun human-turn-after-action (player-state board stock action)
  (let ((moves (get-all-moves board (player-hand player-state))))
    (cond
      ((eq action 'input-ended)
       (format t "Input ended; the human turn was not completed.~%")
       (list player-state board stock 'input-ended))
      ((eq action 'help)
       (print-human-move-options moves stock)
       (list player-state board stock 'retry))
      ((and (eq action 'place) moves)
       (human-turn-place player-state board stock))
      ((and (eq action 'draw) (null moves) stock)
       (human-turn-after-draw player-state board stock))
      ((and (eq action 'pass) (null moves) (null stock))
       (format t "Human passes: no legal move and the boneyard is empty.~%")
       (list player-state (board-with-pass-check board t) stock 'passed))
      (t
       (format t "THAT ACTION IS NOT AVAILABLE IN THIS POSITION.~%")
       (list player-state board stock 'retry)))))


;;; /***************************************************************************
;;; **Function Name:** human-turn-after-draw
;;; **Purpose:** Draw one tile and return the updated state for another prompt.
;;; **Parameters:** player-state - human state; board - current board; stock - nonempty boneyard.
;;; **Return Value:** Updated player, board, stock, and RETRY status after the draw.
;;; **Algorithm:** Move the first stock tile to a newly built hand, report the draw, and retry.
;;; **Reference:** Drawing rules from Human.cpp and Deck.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun human-turn-after-draw (player-state board stock)
  (let ((tile (car stock)))
    (format t "Human drew ~a-~a; boneyard tiles left: ~a.~%"
            (tile-left-pips tile) (tile-right-pips tile)
            (list-length-rec (cdr stock)))
    (list (player-with-hand player-state
            (append-to-end (player-hand player-state) (list tile)))
          board (cdr stock) 'retry)))


;;; /***************************************************************************
;;; **Function Name:** read-human-direction
;;; **Purpose:** Convert one case-insensitive direction initial into a board direction.
;;; **Parameters:** response - one letter entered for the selected direction.
;;; **Return Value:** FIRST, LEFT, RIGHT, UP, or DOWN; NIL for any other response.
;;; **Algorithm:** Trim surrounding whitespace and match F, L, R, U, or D without regard to case.
;;; **Reference:** Human direction choices correspond to the board placement directions.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun read-human-direction (response)
  (cond
    ((null response) nil)
    (t
     (let ((choice (string-trim '(#\Space #\Tab #\Return) response)))
       (cond
         ((string-equal choice "F") 'first)
         ((string-equal choice "L") 'left)
         ((string-equal choice "R") 'right)
         ((string-equal choice "U") 'up)
         ((string-equal choice "D") 'down)
         (t nil))))))


;;; /***************************************************************************
;;; **Function Name:** human-turn-place
;;; **Purpose:** Prompt for the tile and direction and apply a legal placement.
;;; **Parameters:** player-state - human state; board - current board; stock - boneyard.
;;; **Return Value:** (updated-player updated-board stock PLAYED) after a legal placement, or
;;;   (player-state board stock RETRY) when input is invalid or the move is unavailable.
;;; **Algorithm:** Read two pip values and a direction initial, then validate hand membership and legal
;;;   placement.
;;; **Reference:** Placement rules from Human.cpp and Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun human-turn-place (player-state board stock)
  (format t "Tile first pip (0-6): ")
  (finish-output)
  (let ((first-pips
          (parse-complete-integer (or (read-line *standard-input* nil nil) ""))))
    (cond
      ((not (valid-domino-pips first-pips))
       (format t "ENTER ONE PIP VALUE FROM 0 THROUGH 6.~%")
       (list player-state board stock 'retry))
      (t
       (format t "Tile second pip (0-6): ")
       (finish-output)
       (let ((second-pips
               (parse-complete-integer
                 (or (read-line *standard-input* nil nil) ""))))
         (cond
           ((not (valid-domino-pips second-pips))
            (format t "ENTER ONE PIP VALUE FROM 0 THROUGH 6.~%")
            (list player-state board stock 'retry))
           (t
            (format t "Direction (F first, L left, R right, U up, D down): ")
            (finish-output)
            (let* ((direction
                      (read-human-direction
                        (or (read-line *standard-input* nil nil) "")))
                   (tile (find-hand-tile
                           (player-hand player-state) first-pips second-pips)))
              (cond
                ((null direction)
                 (format t "ENTER F, L, R, U, OR D FOR THE DIRECTION.~%")
                 (list player-state board stock 'retry))
                ((null tile)
                 (format t "THAT TILE IS NOT IN YOUR HAND.~%")
                 (list player-state board stock 'retry))
                ((not (move-direction-legal-p board tile direction))
                 (format t "THAT TILE CANNOT BE PLAYED AT THAT END.~%")
                 (list player-state board stock 'retry))
                (t
                 (print-player-placement 'human tile direction board)
                 (let* ((new-board (place-board-tile board tile direction))
                        (score-result
                          (score-player-turn
                            player-state new-board (board-last-play new-board)))
                        (new-hand
                          (remove-hand-tile (player-hand player-state) tile)))
                   (list (player-with-hand (car score-result) new-hand)
                         new-board stock 'played))))))))))))


;;; /***************************************************************************
;;; **Function Name:** print-human-turn-result
;;; **Purpose:** Display legal and optimal choices for the current human position.
;;; **Parameters:** board - current board; hand - human tile list.
;;; **Return Value:** Printed legal and optimal moves; game values remain unchanged.
;;; **Algorithm:** Calculate and print all moves followed by their optimal subset.
;;; **Reference:** Help output follows Human.cpp and GET-ALL-MOVES.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-human-turn-result (board hand)
  (print-human-move-options (get-all-moves board hand)))


;;; /***************************************************************************
;;; **Function Name:** print-human-move-options
;;; **Purpose:** Show legal and optimal moves, or recommend drawing or passing when none are legal.
;;; **Parameters:** moves - move records; optional stock - remaining boneyard tiles;
;;;   stock-supplied-p - internal flag indicating whether STOCK was supplied.
;;; **Return Value:** Printed move options and, when stock is supplied, the required draw or pass.
;;; **Algorithm:** Print legal moves and optimal moves, or recommend drawing or passing when none exist.
;;; **Reference:** Help output follows Human.cpp and GET-ALL-MOVES.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-human-move-options (moves &optional (stock nil stock-supplied-p))
  (format t "All legal moves:~%")
  (print-all-moves moves)
  (cond
    ((null moves)
     (cond
       (stock-supplied-p
        (cond
          (stock (format t "You must draw a tile from the stock.~%"))
          (t (format t "You must pass.~%"))))
       (t nil)))
    (t
     (format t "Optimal move(s):~%")
     (print-all-moves (get-optimal-moves moves)))))


;;; Computer move selection and turn processing

;;; /***************************************************************************
;;; **Function Name:** computer-turn
;;; **Purpose:** Run one automatic computer turn and announce its progress.
;;; **Parameters:**
;;;   player-state - computer hand, round score, and tournament score.
;;;     A list value passed by value; not modified.
;;;   board - current immutable board state.
;;;     A list value passed by value; not modified.
;;;   stock - remaining boneyard tiles.
;;;     A list value passed by value; not modified.
;;;   seed - deterministic random state used to break optimal-move ties.
;;;     An integer passed by value; not modified.
;;; **Return Value:** A five-item list containing updated player, board, stock, next seed, and turn
;;;   result.
;;; **Algorithm:**
;;;   1) Announce the computer turn.
;;;   2) Find optimal moves, drawing and retrying when none exist.
;;;   3) Play a selected optimal move or pass when the stock is empty.
;;; **Reference:** Turn rules from Computer.cpp; move ranking uses GET-OPTIMAL-MOVES.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun computer-turn (player-state board stock seed)
  (format t "~%COMPUTER TURN~%")
  (computer-turn-loop player-state board stock seed))


;;; /***************************************************************************
;;; **Function Name:** computer-turn-loop
;;; **Purpose:** Continue the computer turn by playing, drawing, or passing.
;;; **Parameters:**
;;;   player-state - computer hand, round score, and tournament score.
;;;     A list value passed by value; not modified.
;;;   board - current immutable board state.
;;;     A list value passed by value; not modified.
;;;   stock - remaining boneyard tiles.
;;;     A list value passed by value; not modified.
;;;   seed - deterministic random state used to break optimal-move ties.
;;;     An integer passed by value; not modified.
;;; **Return Value:** A five-item list containing updated player, board, stock, next seed, and turn
;;;   result.
;;; **Algorithm:**
;;;   1) Find legal moves in the computer hand.
;;;   2) Draw when no legal move exists, pass when the stock is empty, or play an optimal move.
;;; **Reference:** Computer turn behavior from Computer.cpp; move ranking uses GET-OPTIMAL-MOVES.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun computer-turn-loop (player-state board stock seed)
  (let* ((moves (get-all-moves board (player-hand player-state)))
         (optimal-moves (get-optimal-moves moves)))
    (cond ((null moves)
           (cond ((null stock)
                   (format t (concatenate 'string
                              "Computer passes their turn due to no valid play "
                              "and no tiles left to draw.~%"))
                  (list player-state (board-with-pass-check board t) stock seed 'passed))
                 (t (computer-turn-after-draw player-state board stock seed))))
           (t (format t (concatenate 'string
                      "Computer ranks legal moves by points, then tile pip total, "
                      "and selects randomly among the optimal choices.~%"))
             (computer-play-selected-move player-state board stock seed optimal-moves)))))


;;; /***************************************************************************
;;; **Function Name:** computer-turn-after-draw
;;; **Purpose:** Draw one tile for the computer and retry its turn.
;;; **Parameters:**
;;;   player-state - computer hand, round score, and tournament score.
;;;     A list value passed by value; not modified.
;;;   board - current immutable board state.
;;;     A list value passed by value; not modified.
;;;   stock - nonempty boneyard tile list.
;;;     A list value passed by value; not modified.
;;;   seed - deterministic random state used to break optimal-move ties.
;;;     An integer passed by value; not modified.
;;; **Return Value:** The result of the computer's retried turn.
;;; **Algorithm:**
;;;   1) Move the first stock tile into a new computer hand.
;;;   2) Add the tile to the computer hand without revealing its pips.
;;;   3) Recalculate legal moves and continue the same turn.
;;; **Reference:** Drawing behavior from Computer.cpp and Deck.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun computer-turn-after-draw (player-state board stock seed)
  (let ((tile (car stock)))
    (format t "Computer draws a tile because no legal play is available.~%")
    (computer-turn-loop
      (player-with-hand player-state (append-to-end (player-hand player-state) (list tile)))
      board (cdr stock) seed)))


;;; /***************************************************************************
;;; **Function Name:** computer-play-selected-move
;;; **Purpose:** Choose a seeded optimal move and apply it for the computer.
;;; **Parameters:**
;;;   player-state - computer hand, round score, and tournament score.
;;;     A list value passed by value; not modified.
;;;   board - current immutable board state.
;;;     A list value passed by value; not modified.
;;;   stock - remaining boneyard tiles.
;;;     A list value passed by value; not modified.
;;;   seed - deterministic random state used to choose among tied moves.
;;;     An integer passed by value; not modified.
;;;   optimal-moves - nonempty list of highest-ranked legal moves.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A five-item list containing updated player, board, stock, next seed, and
;;;   PLAYED.
;;; **Algorithm:**
;;;   1) Select an optimal move index using the supplied seed.
;;;   2) Find its tile, report the placement and scoring arithmetic, then apply the move.
;;;   3) Return the updated turn state and next seed.
;;; **Reference:** Random tie selection follows Computer.cpp; selection is deterministic from the
;;;   supplied seed.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun computer-play-selected-move (player-state board stock seed optimal-moves)
  (let* ((index-state (random-index-and-seed (list-length-rec optimal-moves) seed))
         (move (list-item-at-index optimal-moves (car index-state)))
         (tile (find-hand-tile (player-hand player-state) (car move) (cadr move)))
         (direction (move-code-direction (caddr move)))
         (turn-result (progn
                       (print-player-placement 'computer tile direction board)
                       (complete-player-placement player-state board stock tile direction))))
    (list (car turn-result)
          (cadr turn-result)
          (caddr turn-result)
          (cadr index-state)
          (cadddr turn-result))))


;;; /***************************************************************************
;;; **Function Name:** print-player-placement
;;; **Purpose:** Report a player's selected tile, board update, and scoring calculation.
;;; **Parameters:**
;;;   player-label - player making the placement, HUMAN or COMPUTER.
;;;     A symbol passed by value; not modified.
;;;   tile - tile being played.
;;;     A list value passed by value; not modified.
;;;   direction - legal board end selected for the play.
;;;     A symbol passed by value; not modified.
;;;   board - immutable board state before the play.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed placement and scoring explanation; game values remain unchanged.
;;; **Algorithm:**
;;;   1) Build the resulting board and report the tile placement.
;;;   2) Explain when a newly played double opens the spinner ends.
;;;   3) Print exposed-end total, double bonus, target remainder, and points.
;;; **Reference:** Placement report conventions from Human.cpp and Computer.cpp; scoring rules from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-player-placement (player-label tile direction board)
  (let* ((new-board (place-board-tile board tile direction))
         (played-tile (board-last-play new-board)))
    (format t "~a played ~a-~a at ~a; total ~a, points ~a.~%"
            player-label (tile-left-pips tile) (tile-right-pips tile)
            direction (board-scoring-total new-board played-tile)
            (board-turn-score new-board played-tile))
    (cond ((and (not (board-has-spinner-p board))
                (board-has-spinner-p new-board))
           (format t "[~a - ~a] is the spinner! Up and Down ends are now open!~%"
                   (tile-left-pips tile) (tile-right-pips tile))))
    (print-turn-score-breakdown new-board played-tile)))


;;; /***************************************************************************
;;; **Function Name:** print-turn-score-breakdown
;;; **Purpose:** Show the exact arithmetic used to score the latest board play.
;;; **Parameters:**
;;;   board - immutable board state after the latest play.
;;;     A list value passed by value; not modified.
;;;   tile - tile most recently played on the board.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed score calculation; game values remain unchanged.
;;; **Algorithm:**
;;;   1) Add the pips at the left, right, up, and down open ends.
;;;   2) Include one pip for a non-spinner double on a spinner board, when applicable.
;;;   3) Show the resulting total modulo the target and the points awarded.
;;; **Reference:** Board.cpp scoring rules in CALCSUM and CALCTURNSCORE.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-turn-score-breakdown (board tile)
  (let* ((left (board-end board 'left))
         (right (board-end board 'right))
         (up (board-end board 'up))
         (down (board-end board 'down))
         (end-total (+ left right up down))
         (double-bonus (cond ((and (board-has-spinner-p board) (tile-double-p tile) (not
         (tile-spinner-p tile))) (tile-left-pips tile)) (t 0)))
         (total (+ end-total double-bonus))
         (target (board-target board))
         (points (board-turn-score board tile)))
    (format t "Board score calculation:~%  Open ends: L ~a + R ~a + U ~a + D ~a = ~a~%"
            left right up down end-total)
    (cond ((> double-bonus 0)
           (format t "  Non-spinner double bonus: +~a~%" double-bonus))
          (t (format t "  Non-spinner double bonus: +0~%")))
    (format t "  Total: ~a + ~a = ~a~%  Target check: ~a mod ~a = ~a; points awarded: ~a~%"
            end-total double-bonus total total target (mod total target) points)))


;;; SECTION 4: ROUND AND TOURNAMENT FLOW, SCORING, AND RESULTS

;;; /***************************************************************************
;;; **Function Name:** start-tournament-with-first-round
;;; **Purpose:** Enter the game loop after the first round has been created.
;;; **Parameters:** tournament-target - score needed to end the tournament; round-state - first
;;;   roundsetup or NIL.
;;; **Return Value:** Final tournament state and status, or NIL if round setup ended at input.
;;; **Algorithm:** Stop when setup returned NIL; otherwise run the tournament from the prepared
;;;   round.
;;; **Reference:** Top-level flow follows 35DominoC++.cpp and Round.cpp, excluding saved-game
;;;   loading.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun start-tournament-with-first-round (tournament-target round-state)
  (cond ((null round-state) nil)
        (t (play-tournament-from-round tournament-target round-state))))


;;; /***************************************************************************
;;; **Function Name:** play-tournament-from-round
;;; **Purpose:** Recur through rounds until the target is reached or play is interrupted.
;;; **Parameters:** tournament-target - tournament ending score; round-state - current initialized
;;;   round.
;;; **Return Value:** Final round state paired with TOURNAMENT-OVER, INPUT-ENDED, or SUSPENDED.
;;; **Algorithm:** Run a round and process its result. Recur with the returned next-round state
;;;   when play continues.
;;; **Reference:** Round and tournament sequencing follows 35DominoC++.cpp and Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun play-tournament-from-round (tournament-target round-state)
  (let* ((round-result
           (play-round-loop round-state (fifth round-state) nil tournament-target))
         (tournament-result
           (play-tournament-after-round
             tournament-target round-state round-result)))
    (cond
      ((eq (cadr tournament-result) 'continue)
       (play-tournament-from-round tournament-target (car tournament-result)))
      (t tournament-result))))


;;; /***************************************************************************
;;; **Function Name:** play-tournament-after-round
;;; **Purpose:** Award a completed round and decide whether the tournament continues.
;;; **Parameters:**
;;;   tournament-target - score required to end the tournament.
;;;   round-state - round before its final point award.
;;;   round-result - final round state paired with ROUND-OVER, INPUT-ENDED, or SUSPENDED.
;;; **Return Value:** Round result unchanged on input end or suspension; otherwise the next
;;;   tournament result.
;;; **Algorithm:**
;;;   1) Return without scoring or starting another round when play was interrupted.
;;;  2) Otherwise award the hand-difference bonus, record tournament scores, and continue or finish.
;;;
;;; **Reference:** End-of-round and tournament rules from Round.cpp and Tournament.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun play-tournament-after-round (tournament-target round-state round-result)
  (cond ((or (eq (cadr round-result) 'input-ended)
             (eq (cadr round-result) 'suspended))
         round-result)
        (t (let* ((with-bonus (award-round-end-points (car round-result)))
                  (scored-round (record-round-scores-in-tournament with-bonus)))
             (print-round-end-result (car round-result) scored-round)
             (play-tournament-after-award tournament-target scored-round)))))


;;; /***************************************************************************
;;; **Function Name:** play-tournament-after-award
;;; **Purpose:** Explain whether a tournament target was reached, then end or continue the
;;;   tournament.
;;; **Parameters:**
;;;   tournament-target - score required to end the tournament; integer passed by value.
;;;  round-state - completed round with updated human and computer totals; list passed by value.
;;; **Return Value:** A next-round state paired with CONTINUE, or prior state with INPUT-ENDED.
;;; **Algorithm:**
;;;   1) Compare each tournament total with the target and report which player reached it.
;;;   2) Print the tournament result when either total meets or exceeds the target.
;;;   3) Otherwise explain that neither total reached the target and start the next round.
;;; **Reference:** Tournament continuation follows Round.cpp and Tournament.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun play-tournament-after-award (tournament-target round-state)
  (let* ((human-total (player-tournament-score (cadr round-state)))
         (computer-total (player-tournament-score (caddr round-state)))
         (human-reached (>= human-total tournament-target))
         (computer-reached (>= computer-total tournament-target)))
    (cond
      ((or human-reached computer-reached)
       (cond
         ((and human-reached computer-reached)
           (format t (concatenate 'string
                      "~%Both players reached or exceeded the tournament target of ~a "
                      "(human: ~a; computer: ~a).~%")
                  tournament-target human-total computer-total))
         (human-reached
           (format t (concatenate 'string
                      "~%The human reached the tournament target of ~a with ~a "
                      "points; the computer has ~a.~%")
                  tournament-target human-total computer-total))
         (computer-reached
           (format t (concatenate 'string
                      "~%The computer reached the tournament target of ~a with ~a "
                      "points; the human has ~a.~%")
                  tournament-target computer-total human-total)))
       (print-tournament-result round-state)
       (list round-state 'tournament-over))
      (t
        (format t (concatenate 'string
                   "~%Neither player has reached the tournament target of ~a "
                   "(human: ~a; computer: ~a). Starting another round.~%")
               tournament-target human-total computer-total)
       (start-next-tournament-round tournament-target round-state)))))


;;; /***************************************************************************
;;; **Function Name:** start-next-tournament-round
;;; **Purpose:** Read the next round target and preserve tournament totals.
;;; **Parameters:** tournament-target - ending score; previous-round - completed state carrying
;;;   cumulative tournament totals.
;;; **Return Value:** The next tournament result or INPUT-ENDED when no target was entered.
;;; **Algorithm:** Read target; advance the stored seed; create the new deal; copy totals and
;;;   continue.
;;; **Reference:** Round setup and cumulative scores follow Round.cpp and Player.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun start-next-tournament-round (tournament-target previous-round)
  (start-next-tournament-round-after-target
    tournament-target previous-round (read-round-target)))


;;; /***************************************************************************
;;; **Function Name:** start-next-tournament-round-after-target
;;; **Purpose:** Create the next round after the player supplies its target.
;;; **Parameters:** tournament-target - ending score; previous-round - cumulative scores and prior
;;;   seed;target - requested score or NIL.
;;; **Return Value:** Next tournament result, or (previous-round INPUT-ENDED).
;;; **Algorithm:** Stop on end of input; otherwise make a seeded round, preserve tournament scores,
;;;   show setup, and play it.
;;; **Reference:** Round setup follows Round.cpp; saving/loading remains outside this flow.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun start-next-tournament-round-after-target (tournament-target previous-round target)
  (cond
    ((null target) (list previous-round 'input-ended))
    (t
     (list
       (print-round-start-result
         (round-with-tournament-scores
           (make-round-start-state
             target (next-random-seed (eighth previous-round)))
           (player-tournament-score (cadr previous-round))
           (player-tournament-score (caddr previous-round))))
       'continue))))


;;; /***************************************************************************
;;; **Function Name:** round-with-tournament-scores
;;; **Purpose:** Carry tournament totals into newly initialized player states.
;;; **Parameters:** round-state - new round; human-total and computer-total - cumulative scores.
;;; **Return Value:** New round state with preserved tournament scores.
;;; **Algorithm:** Replace each player's tournament score while retaining new hands and zero round
;;;   scores.
;;; **Reference:** Player state convention from this Lisp file and Player.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun round-with-tournament-scores (round-state human-total computer-total)
  (list (car round-state)
        (player-with-tournament-score (cadr round-state) human-total)
        (player-with-tournament-score (caddr round-state) computer-total)
        (cadddr round-state) (fifth round-state) (sixth round-state)
        (seventh round-state) (eighth round-state)))


;;; /***************************************************************************
;;; **Function Name:** player-with-tournament-score
;;; **Purpose:** Return a player state with a supplied cumulative score.
;;; **Parameters:** player-state - current state; tournament-score - cumulative score.
;;; **Return Value:** New player state preserving hand and round score.
;;; **Algorithm:** Rebuild the player value using its hand, round score, and supplied total.
;;; **Reference:** Player state convention from this Lisp file.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun player-with-tournament-score (player-state tournament-score)
  (list (player-hand player-state) (player-round-score player-state) tournament-score))


;;; /***************************************************************************
;;; **Function Name:** play-round-loop
;;; **Purpose:** Alternate human and computer turns until the round ends.
;;; **Parameters:** round-state - current state; current-player - HUMAN or COMPUTER; previous-pass -
;;;   prior-turn pass marker.
;;;   tournament-target - score that ends the tournament.
;;; **Return Value:** (round-state ROUND-OVER), (round-state INPUT-ENDED),
;;;   or (round-state SUSPENDED).
;;; **Algorithm:** Stop if a hand is empty; otherwise run a turn, update state, and stop on two
;;;   passes or input end.
;;; **Reference:** Main turn flow and suspension saves follow Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun play-round-loop (round-state current-player previous-pass tournament-target)
  (cond ((or (null (player-hand (cadr round-state)))
             (null (player-hand (caddr round-state))))
         (list round-state 'round-over))
        (t (play-round-after-turn round-state current-player previous-pass
                                  (play-round-turn current-player round-state tournament-target)
                                  tournament-target))))


;;; /***************************************************************************
;;; **Function Name:** play-round-turn
;;; **Purpose:** Run one selected player's turn and normalize its result for the round loop.
;;; **Parameters:**
;;;   current-player - HUMAN or COMPUTER whose turn is next.
;;;   round-state - current target, player states, boneyard, board, and random seed.
;;;   tournament-target - score that ends the tournament.
;;; **Return Value:** (updated-player updated-board updated-stock next-seed result), where result is
;;;
;;;   PLAYED, PASSED, INPUT-ENDED, or SUSPENDED.
;;; **Algorithm:**
;;;   1) Before a human turn, ask whether to play or suspend the game.
;;;   2) Run the selected turn and keep the random seed in the shared result position.
;;; **Reference:** The human suspend prompt follows Round.cpp; computer turns follow Computer.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun play-round-turn (current-player round-state tournament-target)
  (cond ((eq current-player 'human)
         (human-turn-as-round-result
           (human-turn-with-suspend-choice round-state tournament-target)
           (eighth round-state)))
        (t (computer-turn (caddr round-state) (seventh round-state)
                          (cadddr round-state) (eighth round-state)))))


;;; /***************************************************************************
;;; **Function Name:** human-turn-with-suspend-choice
;;; **Purpose:** Let the human suspend the game before beginning each turn.
;;; **Parameters:**
;;;   round-state - current round values, including human state, board, and boneyard.
;;;   tournament-target - score that ends the tournament.
;;; **Return Value:** (human-state board boneyard PLAYED/PASSED/INPUT-ENDED/SUSPENDED).
;;; **Algorithm:**
;;;   1) Ask whether to play this turn or suspend the current game.
;;;   2) Run the human turn for P; return SUSPENDED for S; repeat after invalid input.
;;;   3) Save the round state and pending human turn when the game is suspended.
;;; **Reference:** Suspend choice and save flow follow Round.cpp and the SerLISP examples.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun human-turn-with-suspend-choice (round-state tournament-target)
  (format t "Continue playing this turn (P) or suspend/save (S): ")
  (finish-output)
  (let ((response (read-line *standard-input* nil nil)))
    (cond ((null response) (list (cadr round-state) (seventh round-state)
                                 (cadddr round-state) 'input-ended))
          ((string-equal (string-trim '(#\Space #\Tab #\Return) response) "P")
           (human-turn (cadr round-state) (seventh round-state) (cadddr round-state)))
          ((string-equal (string-trim '(#\Space #\Tab #\Return) response) "S")
           (save-suspended-lisp-game round-state tournament-target)
           (list (cadr round-state) (seventh round-state) (cadddr round-state) 'suspended))
          (t (format t "INVALID INPUT~%")
             (human-turn-with-suspend-choice round-state tournament-target)))))


;;; /***************************************************************************
;;; **Function Name:** play-round-after-turn
;;; **Purpose:** Apply one turn and either continue play or return its stopping status.
;;; **Parameters:**
;;;   round-state - prior immutable round value.
;;;   current-player - HUMAN or COMPUTER who just acted.
;;;   previous-pass - whether the preceding turn was a pass.
;;;   turn-result - normalized turn values, including result status.
;;;   tournament-target - score that ends the tournament.
;;; **Return Value:** (round-state ROUND-OVER), (round-state INPUT-ENDED), or (round-state
;;;   SUSPENDED).
;;; **Algorithm:**
;;;   1) Apply the turn result to the round state.
;;;  2) Return immediately for input end or suspension; otherwise check round completion and switch
;;;   turns.
;;; **Reference:** Turn alternation and stop rules follow Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun play-round-after-turn (round-state current-player previous-pass turn-result
                              tournament-target)
  (let ((updated-round (round-with-turn-result round-state current-player turn-result)))
    (cond ((or (eq (fifth turn-result) 'input-ended)
               (eq (fifth turn-result) 'suspended))
           (list updated-round (fifth turn-result)))
          (t (play-round-after-turn-state updated-round current-player previous-pass
                                          (fifth turn-result) tournament-target)))))


;;; /***************************************************************************
;;; **Function Name:** play-round-after-turn-state
;;; **Purpose:** Explain why a round ended or continue play after the latest turn.
;;; **Parameters:**
;;;   round-state - updated round state; list passed by value, not modified.
;;;   current-player - player who just acted; symbol passed by value.
;;;   previous-pass - whether the prior turn was a pass; value passed by value.
;;;   result - latest turn result, such as PLAYED or PASSED; symbol passed by value.
;;;   tournament-target - score that ends the tournament.
;;; **Return Value:** (round-state ROUND-OVER), or a continuing round result.
;;;   The continuing result can be INPUT-ENDED or SUSPENDED.
;;; **Algorithm:**
;;;   1) End the round when either hand is empty or both players passed consecutively.
;;;   2) Print the corresponding end reason and current round progress.
;;;   3) Otherwise print progress and continue with the other player.
;;; **Reference:** Round-end conditions follow Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun play-round-after-turn-state (round-state current-player previous-pass result
                                    tournament-target)
  (cond
    ((and (null (player-hand (cadr round-state)))
          (null (player-hand (caddr round-state))))
     (print-round-progress round-state)
     (format t "~%The round has ended because both players have emptied their hands.~%")
     (list round-state 'round-over))
    ((null (player-hand (cadr round-state)))
     (print-round-progress round-state)
     (format t "~%The round has ended because the human emptied their hand.~%")
     (list round-state 'round-over))
    ((null (player-hand (caddr round-state)))
     (print-round-progress round-state)
     (format t "~%The round has ended because the computer emptied its hand.~%")
     (list round-state 'round-over))
    ((and previous-pass (eq result 'passed))
     (print-round-progress round-state)
     (format t "~%The round has ended because both players passed consecutively.~%")
     (list round-state 'round-over))
    (t
     (print-round-progress round-state)
     (play-round-loop round-state (other-player current-player)
                      (eq result 'passed) tournament-target))))


;;; /***************************************************************************
;;; **Function Name:** round-with-turn-result
;;; **Purpose:** Return the round state with one player's turn changes applied.
;;; **Parameters:** round-state - prior state; current-player - HUMAN or COMPUTER; turn-result -
;;;   normalizedturn values.
;;; **Return Value:** New round state with the updated player, board, stock, and seed.
;;; **Algorithm:** Replace the active player's value and retain the other round fields.
;;; **Reference:** Round state layout is documented by MAKE-ROUND-START-STATE-FROM-SELECTION.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun round-with-turn-result (round-state current-player turn-result)
  (list (car round-state)
        (cond ((eq current-player 'human) (car turn-result)) (t (cadr round-state)))
        (cond ((eq current-player 'computer) (car turn-result)) (t (caddr round-state)))
        (caddr turn-result) (fifth round-state) (sixth round-state)
        (cadr turn-result) (fourth turn-result)))


;;; /***************************************************************************
;;; **Function Name:** other-player
;;; **Purpose:** Return the other player symbol for turn alternation.
;;; **Parameters:** player - HUMAN or COMPUTER.
;;; **Return Value:** COMPUTER for HUMAN; otherwise HUMAN.
;;; **Algorithm:** Compare the supplied symbol and return its opponent.
;;; **Reference:** Two-player turn order from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun other-player (player)
  (cond ((eq player 'human) 'computer) (t 'human)))


;;; /***************************************************************************
;;; **Function Name:** print-round-progress
;;; **Purpose:** Display the board, boneyard size, and current round scores.
;;; **Parameters:**
;;;   round-state - current immutable round state.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed progress report; game state is unchanged.
;;; **Algorithm:**
;;;   1) Print the current board.
;;;   2) Print the number of tiles remaining in the boneyard.
;;;   3) Print the human and computer round scores.
;;; **Reference:** Progress display follows Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-round-progress (round-state)
  (print-board (seventh round-state))
  (format t "Tiles in boneyard: ~a~%" (list-length-rec (cadddr round-state)))
  (format t "Round score: human ~a; computer ~a~%"
          (player-round-score (cadr round-state))
          (player-round-score (caddr round-state))))


;;; /***************************************************************************
;;; **Function Name:** award-round-end-points
;;; **Purpose:** Give the lower-pip hand the rounded hand-difference bonus.
;;; **Parameters:** round-state - completed round with player hands and scores.
;;; **Return Value:** New completed round state with any end bonus included.
;;; **Algorithm:** Compare hand pip totals; award the positive difference converted to the nearest
;;;   targetmultiple quotient.
;;; **Reference:** End scoring follows Round.cpp CALC-END-SCORE and Player.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun award-round-end-points (round-state)
  (award-round-end-by-totals round-state
    (hand-pip-total (player-hand (cadr round-state)))
    (hand-pip-total (player-hand (caddr round-state)))))


;;; /***************************************************************************
;;; **Function Name:** award-round-end-by-totals
;;; **Purpose:** Apply the round-end bonus to the player with fewer hand pips.
;;; **Parameters:** round-state - completed round; human-total and computer-total - pip sums.
;;; **Return Value:** New round state with bonus points applied to the appropriate round score.
;;; **Algorithm:** Award zero on a tie; otherwise convert the difference using the round target and
;;;   addit to the lower-hand player.
;;; **Reference:** End bonus rule from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun award-round-end-by-totals (round-state human-total computer-total)
  (cond ((= human-total computer-total) round-state)
        ((< human-total computer-total)
         (round-with-players
           round-state
           (add-round-points (cadr round-state)
             (nearest-target-quotient (- computer-total human-total) (car round-state)))
           (caddr round-state)))
        (t (round-with-players
             round-state (cadr round-state)
             (add-round-points (caddr round-state)
               (nearest-target-quotient (- human-total computer-total) (car round-state)))))))


;;; /***************************************************************************
;;; **Function Name:** nearest-target-quotient
;;; **Purpose:** Convert a pip difference to the closest round-target quotient.
;;; **Parameters:** difference - positive pip difference; target - round target 3 or 5.
;;; **Return Value:** Nearest multiple quotient; equal distances choose the lower quotient.
;;; **Algorithm:** Compare distances to adjacent multiples, then return the closer quotient.
;;; **Reference:** CALC-END-SCORE from Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun nearest-target-quotient (difference target)
  (let* ((low (floor difference target))
         (high (1+ low))
         (low-distance (- difference (* low target)))
         (high-distance (- (* high target) difference)))
    (cond ((< high-distance low-distance) high) (t low))))


;;; /***************************************************************************
;;; **Function Name:** hand-pip-total
;;; **Purpose:** Add the pip counts of all tiles in one hand.
;;; **Parameters:** hand - ordered domino list.
;;; **Return Value:** Total pip count in that hand.
;;; **Algorithm:** Recursively add the first tile sum to the total of the remaining hand.
;;; **Reference:** Round-end hand comparison follows Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun hand-pip-total (hand)
  (cond ((null hand) 0)
        (t (+ (tile-pip-sum (car hand)) (hand-pip-total (cdr hand))))))


;;; /***************************************************************************
;;; **Function Name:** round-with-players
;;; **Purpose:** Return a round state with replacement human and computer states.
;;; **Parameters:** round-state - current round; human-state and computer-state - replacement player
;;;   values.
;;; **Return Value:** New round state with all non-player fields retained.
;;; **Algorithm:** Rebuild the round list with supplied players and existing board, stock, starter,
;;;   history,and seed.
;;; **Reference:** Round state layout documented by MAKE-ROUND-START-STATE-FROM-SELECTION.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun round-with-players (round-state human-state computer-state)
  (list (car round-state) human-state computer-state (cadddr round-state)
        (fifth round-state) (sixth round-state) (seventh round-state) (eighth round-state)))


;;; /***************************************************************************
;;; **Function Name:** print-tournament-result
;;; **Purpose:** Display the tournament winner and cumulative scores.
;;; **Parameters:** round-state - completed round containing cumulative player totals.
;;; **Return Value:** Printed tournament result; game state is unchanged.
;;; **Algorithm:** Compare player tournament scores and report the winner or a draw.
;;; **Reference:** Winner reporting follows Tournament.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-tournament-result (round-state)
  (let ((human-score (player-tournament-score (cadr round-state)))
        (computer-score (player-tournament-score (caddr round-state))))
    (cond ((> human-score computer-score) (format t "YOU WIN: ~a to ~a~%" human-score
    computer-score))
          ((< human-score computer-score) (format t "YOU LOSE: ~a to ~a~%" human-score
          computer-score))
          (t (format t "DRAW: ~a to ~a~%" human-score computer-score)))))


;;; /***************************************************************************
;;; **Function Name:** human-turn-as-round-result
;;; **Purpose:** Normalize the human turn result to the round-loop result layout.
;;; **Parameters:** turn-result - (player board stock status); seed - unchanged random seed.
;;; **Return Value:** (player board stock seed status).
;;; **Algorithm:** Preserve human values and place seed/status in shared result positions.
;;; **Reference:** Turn result conventions used by PLAY-ROUND-TURN.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun human-turn-as-round-result (turn-result seed)
  (list (car turn-result) (cadr turn-result) (caddr turn-result) seed (cadddr turn-result)))


;;; /***************************************************************************
;;; **Function Name:** record-round-scores-in-tournament
;;; **Purpose:** Add completed round scores to cumulative tournament scores.
;;; **Parameters:** round-state - completed round after any hand-difference bonus.
;;; **Return Value:** New round state with cumulative tournament totals updated.
;;; **Algorithm:** Add each player's round score to that player's tournament score.
;;; **Reference:** Score accumulation follows Player.cpp and Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun record-round-scores-in-tournament (round-state)
  (round-with-players
    round-state
    (player-with-tournament-score (cadr round-state)
      (+ (player-tournament-score (cadr round-state)) (player-round-score (cadr round-state))))
    (player-with-tournament-score (caddr round-state)
      (+ (player-tournament-score (caddr round-state)) (player-round-score (caddr round-state))))))


;;; /***************************************************************************
;;; **Function Name:** print-round-end-result
;;; **Purpose:** Explain the end bonus and tournament score additions, then display final scores.
;;; **Parameters:**
;;;  before-award - completed round before its end bonus and tournament update; list passed by
;;;   value.
;;;  after-award - round after the bonus and cumulative tournament update; list passed by value.
;;; **Return Value:** Printed round summary; game values are unchanged.
;;; **Algorithm:**
;;;   1) Display each remaining hand and calculate its pip total.
;;;   2) Explain the lower-hand bonus using the nearest target multiple and quotient.
;;;   3) Show each old tournament total plus the final round score and resulting total.
;;; **Reference:** Round-end bonus and score accumulation follow Round.cpp and Player.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-round-end-result (before-award after-award)
  (let* ((human-total (hand-pip-total (player-hand (cadr before-award))))
         (computer-total (hand-pip-total (player-hand (caddr before-award))))
         (target (car before-award)))
    (format t "~%ROUND END~%Remaining human hand: ")
    (print-tile-list (player-hand (cadr before-award)))
    (format t "Human hand sum: ~a~%" human-total)
    (format t "Remaining computer hand: ")
    (print-tile-list (player-hand (caddr before-award)))
    (format t "Computer hand sum: ~a~%" computer-total)
    (print-round-end-bonus human-total computer-total target)
    (format t "Round scores: human ~a; computer: ~a~%"
            (player-round-score (cadr after-award)) (player-round-score (caddr after-award)))
    (format t "Tournament score additions:~%")
    (format t "  Human: ~a + ~a round points = ~a~%"
            (player-tournament-score (cadr before-award))
            (player-round-score (cadr after-award))
            (player-tournament-score (cadr after-award)))
    (format t "  Computer: ~a + ~a round points = ~a~%"
            (player-tournament-score (caddr before-award))
            (player-round-score (caddr after-award))
            (player-tournament-score (caddr after-award)))
    (format t "Tournament scores: human ~a; computer: ~a~%"
            (player-tournament-score (cadr after-award))
            (player-tournament-score (caddr after-award)))))


;;; /***************************************************************************
;;; **Function Name:** print-round-end-bonus
;;; **Purpose:** Explain how remaining hand totals determine the round-end bonus.
;;; **Parameters:**
;;;   human-total - pips remaining in the human hand; integer passed by value.
;;;   computer-total - pips remaining in the computer hand; integer passed by value.
;;;   target - round scoring target, either 3 or 5; integer passed by value.
;;; **Return Value:** Printed bonus calculation; game values are unchanged.
;;; **Algorithm:**
;;;   1) Report that tied hand totals earn no bonus.
;;;   2) Otherwise identify the player with fewer pips and calculate the difference.
;;;  3) Compare the difference with the adjacent target multiples and explain the nearest quotient
;;;   awarded.
;;; **Reference:** End bonus and nearest-target calculation follow Round.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-round-end-bonus (human-total computer-total target)
  (cond
    ((= human-total computer-total)
     (format t
     "The remaining hand totals tie (~a each), so neither player receives a round-end bonus.~%"
             human-total))
    (t
     (let* ((human-wins (< human-total computer-total))
            (winner (cond (human-wins "Human") (t "Computer")))
            (lower-total (cond (human-wins human-total) (t computer-total)))
            (higher-total (cond (human-wins computer-total) (t human-total)))
            (difference (- higher-total lower-total))
            (low-quotient (floor difference target))
            (high-quotient (1+ low-quotient))
            (low-multiple (* low-quotient target))
            (high-multiple (* high-quotient target))
            (low-distance (- difference low-multiple))
            (high-distance (- high-multiple difference))
            (bonus (nearest-target-quotient difference target))
            (nearest-multiple (* bonus target)))
       (format t "~a has fewer remaining pips: ~a versus ~a.~%"
               winner lower-total higher-total)
       (format t "Hand difference: ~a - ~a = ~a pips.~%"
               higher-total lower-total difference)
       (cond
         ((= low-distance 0)
          (format t "The difference is exactly the target multiple ~a x ~a = ~a.~%"
                  low-quotient target low-multiple))
         ((= high-distance low-distance)
           (format t (concatenate 'string
                      "Nearest multiples tie: ~a x ~a = ~a and ~a x ~a = ~a, "
                      "each ~a pips away; the lower quotient is used.~%")
                  low-quotient target low-multiple
                  high-quotient target high-multiple low-distance))
         (t
           (format t (concatenate 'string
                      "Nearest target multiples: ~a x ~a = ~a (~a away) "
                      "and ~a x ~a = ~a (~a away); ~a is closer.~%")
                  low-quotient target low-multiple low-distance
                  high-quotient target high-multiple high-distance nearest-multiple)))
       (format t "Round-end bonus: ~a / ~a = ~a point~:p to the ~a.~%"
               nearest-multiple target bonus (string-downcase winner))))))




;;; SECTION 5: SAVE/LOAD AND SERIALIZATION

;;; /***************************************************************************
;;; **Function Name:** saved-tiles
;;; **Purpose:** Convert internal tiles to saved pip-pair values.
;;; **Parameters:** tiles - ordered internal domino tiles; list passed by value.
;;; **Return Value:** Tiles represented as (left-pips right-pips) pairs.
;;; **Algorithm:** Recursively copy each tile's two pips into the saved format.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt and C++ save order.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun saved-tiles (tiles)
  (cond ((null tiles) nil)
        (t (cons (list (caar tiles) (cadar tiles))
                 (saved-tiles (cdr tiles))))))


;;; /***************************************************************************
;;; **Function Name:** restored-tiles
;;; **Purpose:** Convert saved pip pairs into internal domino tiles.
;;; **Parameters:** tiles - saved two-pip tile pairs; list passed by value.
;;; **Return Value:** Internal tiles shaped (left-pips right-pips NIL).
;;; **Algorithm:** Recursively add the internal non-spinner marker to each pair.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt and C++ save format.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun restored-tiles (tiles)
  (cond ((null tiles) nil)
        (t (cons (list (caar tiles) (cadar tiles) nil)
                 (restored-tiles (cdr tiles))))))


;;; /***************************************************************************
;;; **Function Name:** saved-player
;;; **Purpose:** Convert a player state to the saved file player layout.
;;; **Parameters:** player-state - hand, round score, and tournament score.
;;; **Return Value:** (saved-hand round-score tournament-score); a one-tile hand is a bare pip pair.
;;; **Algorithm:** Save a single tile as one pair, multiple tiles as a list of pairs, then retain
;;;   both scores.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt; C++ Player order.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun saved-player (player-state)
  (let ((hand (player-hand player-state)))
    (list (cond
            ((and (consp hand) (null (cdr hand)))
             (list (tile-left-pips (car hand))
                   (tile-right-pips (car hand))))
            (t (saved-tiles hand)))
          (player-round-score player-state)
          (player-tournament-score player-state))))


;;; /***************************************************************************
;;; **Function Name:** restored-player
;;; **Purpose:** Convert one saved player value into an internal player state.
;;; **Parameters:** saved-state - saved hand and scores.
;;; **Return Value:** (internal-hand round-score tournament-score).
;;; **Algorithm:** Restore the pip-pair hand and retain both scores.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt; C++ Player order.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun restored-player (saved-state)
  (list (restore-saved-player-hand (car saved-state))
        (cadr saved-state)
        (caddr saved-state)))


;;; /***************************************************************************
;;; **Function Name:** restore-saved-player-hand
;;; **Purpose:** Restore a saved player hand from either a single pip pair or a list of pip pairs.
;;; **Parameters:**
;;;   saved-hand - one saved domino pair or a list of saved domino pairs.
;;;     A list value passed by value; not modified.
;;; **Return Value:** An ordered list of internal tiles shaped (left-pips right-pips NIL).
;;; **Algorithm:**
;;;   1) Recognize a lone two-integer pip pair and wrap it as a one-tile hand.
;;;   2) Restore the resulting list of saved pip pairs as internal tiles.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt and C++ save format.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun restore-saved-player-hand (saved-hand)
  (cond
    ((null saved-hand) nil)
    ((and (consp saved-hand)
          (numberp (car saved-hand))
          (numberp (cadr saved-hand))
          (null (cddr saved-hand)))
     (restored-tiles (list saved-hand)))
    (t (restored-tiles saved-hand))))


;;; /***************************************************************************
;;; **Function Name:** saved-spinner-branch
;;; **Purpose:** Build the saved spinner branch from its D marker, reversed tile pairs, and U marker.
;;; **Parameters:** vertical - internal top-to-bottom board line; branch - optional saved suffix,
;;;   defaulting to the U marker.
;;; **Return Value:** A saved branch shaped (D reversed-tile-pairs ... U).
;;; **Algorithm:** Recursively prepend each reversed tile pair to BRANCH, then add D when no tiles remain.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt and C++ Board layout.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun saved-spinner-branch (vertical &optional (branch (list 'u)))
  (cond
    ((null vertical) (cons 'd branch))
    (t
     (saved-spinner-branch
       (cdr vertical)
       (cons (list (cadar vertical) (caar vertical)) branch)))))


;;; /***************************************************************************
;;; **Function Name:** saved-horizontal-layout
;;; **Purpose:** Serialize the horizontal line and insert a spinner branch.
;;; **Parameters:**
;;;   horizontal - internal left-to-right board line.
;;;   spinner-branch - serialized spinner branch inserted at the spinner.
;;;   tail - optional suffix appended after the horizontal line; defaults to R.
;;; **Return Value:** Saved horizontal layout items with a nested (D ... U) branch.
;;; **Algorithm:** Copy ordinary tile pairs; insert SPINNER-BRANCH at the spinner and continue with TAIL.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun saved-horizontal-layout (horizontal spinner-branch
                                &optional (tail (list 'r)))
  (cond
    ((null horizontal) tail)
    ((tile-spinner-p (car horizontal))
     (cons spinner-branch
           (saved-horizontal-layout (cdr horizontal) spinner-branch tail)))
    (t
     (cons (list (caar horizontal) (cadar horizontal))
           (saved-horizontal-layout (cdr horizontal) spinner-branch tail)))))


;;; /***************************************************************************
;;; **Function Name:** saved-board-layout
;;; **Purpose:** Convert board lines into the saved L-to-R layout value.
;;; **Parameters:** board - internal immutable board state.
;;; **Return Value:** (L ... R), including a nested (D ... U) spinner branch.
;;; **Algorithm:** Add the left marker, serialized lines, and right marker.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt; C++ Board layout.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun saved-board-layout (board)
  (cons 'l
        (saved-horizontal-layout
          (board-horizontal board)
          (saved-spinner-branch (board-vertical board)))))


;;; /***************************************************************************
;;; **Function Name:** make-saved-game-value
;;; **Purpose:** Arrange game values in the SerLISP and C++ case-file order.
;;; **Parameters:** tournament-target - ending score; round-state - current round; next-player -
;;;   next-turn symbol.
;;; **Return Value:** (tournament-target target computer human layout boneyard next-player).
;;; **Algorithm:** Package the seven fields in the order used by C++ save/load.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt; Round.cpp saveGame.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-saved-game-value (tournament-target round-state next-player)
  (let* ((computer (caddr round-state))
         (human (cadr round-state))
         (computer-hand (player-hand computer))
         (human-hand (player-hand human))
         (board (seventh round-state)))
    (list tournament-target
          (board-target board)
          (list (cond
                  ((and computer-hand (null (cdr computer-hand)))
                   (list (caar computer-hand) (cadar computer-hand)))
                  (t (saved-tiles computer-hand)))
                (player-round-score computer)
                (player-tournament-score computer))
          (list (cond
                  ((and human-hand (null (cdr human-hand)))
                   (list (caar human-hand) (cadar human-hand)))
                  (t (saved-tiles human-hand)))
                (player-round-score human)
                (player-tournament-score human))
          (saved-board-layout board)
          (saved-tiles (cadddr round-state))
          next-player)))


;;; /***************************************************************************
;;; **Function Name:** write-lisp-save-file
;;; **Purpose:** Write the current game to a Lisp-readable save file.
;;; **Parameters:** filename - output path; tournament-target - ending score; round-state - current
;;;   round;next-player - next turn.
;;; **Return Value:** The saved value after writing it.
;;; **Algorithm:** Build the seven-field save value and print it as a readable Lisp form.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt; Round.cpp saveGame.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun write-lisp-save-file (filename tournament-target round-state next-player)
  (write-lisp-save-value
    filename (make-saved-game-value tournament-target round-state next-player)))


;;; /***************************************************************************
;;; **Function Name:** save-suspended-lisp-game
;;; **Purpose:** Ask for a path and save the game before the human turn begins.
;;; **Parameters:** round-state - current round; tournament-target - tournament ending score.
;;; **Return Value:** The saved value, or NIL if input ends before a filename is given.
;;; **Algorithm:** Read a filename and write the state with HUMAN as the next player.
;;; **Reference:** Round.cpp saveGame; SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun save-suspended-lisp-game (round-state tournament-target)
  (format t "Name the output save file (.txt added automatically): ")
  (finish-output)
  (save-suspended-lisp-game-to-file
    (read-line *standard-input* nil nil) round-state tournament-target))


;;; /***************************************************************************
;;; **Function Name:** save-suspended-lisp-game-to-file
;;; **Purpose:** Write a suspended game when a filename has been supplied.
;;; **Parameters:** filename - output path, or NIL; round-state - current round; tournament-target -
;;;   endscore.
;;; **Return Value:** The saved value, or NIL when filename input ended.
;;; **Algorithm:** Write the current state with the pending human turn, then confirm the save.
;;; **Reference:** Round.cpp saveGame; SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun save-suspended-lisp-game-to-file (filename round-state tournament-target)
  (cond ((null filename) nil)
        (t (write-lisp-save-file filename tournament-target round-state 'human)
           (format t "GAME SAVED!~%"))))


;;; /***************************************************************************
;;; **Function Name:** write-lisp-save-value
;;; **Purpose:** Write one already assembled save value to a file.
;;; **Parameters:** filename - output path; saved-value - seven-field saved game.
;;; **Return Value:** The saved value.
;;; **Algorithm:** Open the output file, write the save fields in the SerLISP layout, and finish its
;;;   line.
;;; **Reference:** Common Lisp file I/O; saved layout follows the SerLISP samples.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun write-lisp-save-value (filename saved-value)
  (with-open-file (stream (ensure-txt-extension filename) :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
    (write-saved-game-value-pretty stream saved-value))
  saved-value)


;;; /***************************************************************************
;;; **Function Name:** ensure-txt-extension
;;; **Purpose:** Add the .txt extension to a save filename when it is missing.
;;; **Parameters:** filename - save path entered by the player; string passed by value.
;;; **Return Value:** The original filename when it ends in .txt, or the filename with .txt
;;;   appended.
;;; **Algorithm:** Compare the final four characters to .txt without regard to letter case; append
;;;   when absent.
;;; **Reference:** Round.cpp saveGame appends .txt when creating a save file.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun ensure-txt-extension (filename)
  (cond
    ((and (>= (length filename) 4)
          (string-equal (subseq filename (- (length filename) 4)) ".txt"))
     filename)
    (t
     (concatenate 'string filename ".txt"))))


;;; /***************************************************************************
;;; **Function Name:** write-saved-game-value-pretty
;;; **Purpose:** Write the seven save fields using the SerLISP file layout.
;;; **Parameters:** stream - open output stream; saved-value - ordered seven-field save value.
;;; **Return Value:** The saved value after writing it.
;;; **Algorithm:** Write each field in order with a blank line between sections.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt; C++ Case file order.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun write-saved-game-value-pretty (stream saved-value)
  (format stream "(~%   ~a~%~%   ~a~%~%"
          (car saved-value) (cadr saved-value))
  (write-saved-player-pretty stream (caddr saved-value))
  (format stream "~%~%")
  (write-saved-player-pretty stream (cadddr saved-value))
  (format stream "~%~%   ")
  (write-serlisp-value (car (cddddr saved-value)) stream)
  (format stream "~%~%   ")
  (write-serlisp-value (cadr (cddddr saved-value)) stream)
  (format stream "~%~%   ")
  (write (car (last saved-value)) :stream stream :case :capitalize)
  (format stream "~%)~%")
  saved-value)


;;; /***************************************************************************
;;; **Function Name:** write-saved-player-pretty
;;; **Purpose:** Write one player record with the hand and scores on separate lines.
;;; **Parameters:** stream - open output stream; saved-player - saved hand and two scores.
;;; **Return Value:** The saved player record after writing it.
;;; **Algorithm:** Indent the hand and each score inside the player's list.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun write-saved-player-pretty (stream saved-player)
  (format stream "   (~%      ")
  (write-serlisp-value (car saved-player) stream)
  (format stream "~%      ~a~%      ~a~%   )"
          (cadr saved-player) (caddr saved-player))
  saved-player)


;;; /***************************************************************************
;;; **Function Name:** write-serlisp-value
;;; **Purpose:** Write a Lisp value with spaces inside list delimiters like the SerLISP examples.
;;; **Parameters:** value - nested list or atom to write; stream - open output stream.
;;; **Return Value:** The value after writing it.
;;; **Algorithm:** Recursively write list items with spaces; use the Lisp printer for atoms.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun write-serlisp-value (value stream)
  (cond
    ((consp value)
     (format stream "( ")
     (write-serlisp-list-items value stream)
     (format stream " )"))
    (t
     (write value :stream stream :case :capitalize)))
  value)


;;; /***************************************************************************
;;; **Function Name:** write-serlisp-list-items
;;; **Purpose:** Write each item in a saved list with SerLISP spacing.
;;; **Parameters:** items - remaining items of a list; stream - open output stream.
;;; **Return Value:** The list items after writing them.
;;; **Algorithm:** Write one item at a time, adding separators only between items.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun write-serlisp-list-items (items stream)
  (cond
    ((null items) nil)
    (t
     (write-serlisp-value (car items) stream)
     (cond ((cdr items) (format stream " ")))
     (write-serlisp-list-items (cdr items) stream)))
  items)


;;; /***************************************************************************
;;; **Function Name:** read-lisp-save-file
;;; **Purpose:** Read a SerLISP save file and restore a resumable game value.
;;; **Parameters:** filename - path to the Lisp-readable save file.
;;; **Return Value:** (tournament-target round-state next-player), or NIL for an empty file.
;;; **Algorithm:** Read the saved seven fields and rebuild internal player, board, and tile values.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt; C++ Round.cpp loadGame.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
;;; /***************************************************************************
;;; **Function Name:** normalize-lisp-filename
;;; **Purpose:** Convert Windows backslash paths to the slash form accepted by Allegro CL.
;;; **Parameters:** filename - save-file path entered by the player.
;;; **Return Value:** A pathname string normalized for the current platform.
;;; **Algorithm:** Replace backslashes on Windows and preserve the filename on other platforms.
;;; **Reference:** Allegro CL Pathnames documentation, Windows pathname syntax.
;;;   https://examples.franz.com/support/documentation/pathnames.html.
;;; ***************************************************************************/
(defun normalize-lisp-filename (filename)
  #+mswindows (substitute #\/ #\\ filename)
  #-mswindows filename)

(defun read-lisp-save-file (filename)
  (with-open-file (stream (normalize-lisp-filename filename) :direction :input)
    (restore-saved-game-value (read stream nil nil))))


;;; /***************************************************************************
;;; **Function Name:** restore-saved-game-value
;;; **Purpose:** Rebuild the internal round state from the saved field sequence.
;;; **Parameters:** saved-value - seven fields in the saved file's order.
;;; **Return Value:** (tournament-target round-state next-player), or NIL for no value.
;;; **Algorithm:** Restore players and stock, derive the board from its layout, and supply omitted
;;;   runtimefields.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun restore-saved-game-value (saved-value)
  (cond
    ((null saved-value) nil)
    (t
     (let* ((computer-saved (caddr saved-value))
            (human-saved (cadddr saved-value)))
       (list
         (car saved-value)
         (list
           (cadr saved-value)
           (restored-player human-saved)
           (restored-player computer-saved)
           (restored-tiles (cadr (cddddr saved-value)))
           (car (last saved-value))
           nil
           (restore-saved-board (car (cddddr saved-value))
                                (cadr saved-value))
           1)
         (car (last saved-value)))))))


;;; /***************************************************************************
;;; **Function Name:** split-saved-layout
;;; **Purpose:** Separate the saved horizontal lines and optional spinner branch.
;;; **Parameters:** items - layout items; before - tiles preceding the branch.
;;; **Return Value:** (tiles-before branch-items tiles-after).
;;; **Algorithm:** Recursively scan until the right marker or nested D branch; omit R from the
;;;   branchsuffix.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun split-saved-layout (items before)
  (cond ((null items) (list (reverse-list before) nil nil))
        ((eq (car items) 'r) (list (reverse-list before) nil nil))
        ((and (listp (car items)) (eq (caar items) 'd))
         (list (reverse-list before) (car items)
               (saved-layout-items-before-right-marker (cdr items))))
        (t (split-saved-layout (cdr items) (cons (car items) before)))))


;;; /***************************************************************************
;;; **Function Name:** saved-layout-items-before-right-marker
;;; **Purpose:** Remove the terminal R board marker from tiles after a spinner branch.
;;; **Parameters:** items - remaining saved layout values after the spinner branch.
;;; **Return Value:** Horizontal tile pairs up to, but not including, the R marker.
;;; **Algorithm:** Copy layout values recursively and stop when the right-end marker is reached.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt; saved board layout uses R as an end
;;;   marker.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun saved-layout-items-before-right-marker (items)
  (cond ((null items) nil)
        ((eq (car items) 'r) nil)
        (t (cons (car items)
                 (saved-layout-items-before-right-marker (cdr items))))))


;;; /***************************************************************************
;;; **Function Name:** restore-branch-tiles
;;; **Purpose:** Restore the vertical tiles listed from D toward U.
;;; **Parameters:** saved-branch - nested D tile sequence ending in U; tiles - restored accumulator.
;;;
;;;   spinner-pips - pip value of the board spinner.
;;; **Return Value:** Vertical tiles in internal top-to-bottom order.
;;; **Algorithm:** Reverse each saved pip pair and cons it, marking the matching double as spinner.
;;; **Reference:** SerLISP samples and Board.cpp branch orientation.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun restore-branch-tiles (saved-branch spinner-pips tiles)
  (cond ((null saved-branch) tiles)
        ((eq (car saved-branch) 'u) tiles)
        (t (restore-branch-tiles
             (cdr saved-branch) spinner-pips
             (cons (list (cadar saved-branch) (caar saved-branch)
                         (and (= (caar saved-branch) spinner-pips)
                              (= (cadar saved-branch) spinner-pips)))
                   tiles)))))


;;; /***************************************************************************
;;; **Function Name:** find-spinner-tile
;;; **Purpose:** Find the spinner tile inside a restored vertical line.
;;; **Parameters:** vertical - restored vertical board line.
;;; **Return Value:** The spinner tile, or NIL when no branch has one.
;;; **Algorithm:** Recursively return the first tile marked as spinner.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun find-spinner-tile (vertical)
  (cond ((null vertical) nil)
        ((tile-spinner-p (car vertical)) (car vertical))
        (t (find-spinner-tile (cdr vertical)))))


;;; /***************************************************************************
;;; **Function Name:** find-spinner-location
;;; **Purpose:** Find the zero-based position of the spinner in a board line.
;;; **Parameters:** tiles - board line; position - current zero-based position.
;;; **Return Value:** Spinner position, or zero if the marker is missing.
;;; **Algorithm:** Scan recursively, advancing the position for each tile.
;;; **Reference:** Board state conventions in this Lisp file.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun find-spinner-location (tiles position)
  (cond ((null tiles) 0)
        ((tile-spinner-p (car tiles)) position)
        (t (find-spinner-location (cdr tiles) (1+ position)))))


;;; /***************************************************************************
;;; **Function Name:** restore-saved-board
;;; **Purpose:** Rebuild the board lines, open ends, and spinner data from saved layout.
;;; **Parameters:** layout - saved (L ... R) value; target - round target.
;;; **Return Value:** Internal seven-field board state.
;;; **Algorithm:** Split the horizontal line, restore the vertical branch, and derive exposed ends.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun restore-saved-board (layout target)
  (let* ((parts (split-saved-layout (cdr layout) nil))
         (before (restored-tiles (car parts)))
         (branch (cadr parts))
         (after (restored-tiles (caddr parts))))
    (cond
      ((null branch)
       (cond
         ((null before)
          (make-board-state nil nil target (list 0 0 0 0)
                            (list nil 0 0 -1) nil nil))
         (t
          (make-board-state
            before nil target
            (list (caar before) (cadar (last before)) 0 0)
            (list nil 0 0 -1) nil nil))))
      (t
       (let* ((saved-branch (cdr branch))
              (spinner-pips
                (cond
                  ((null before) (saved-branch-spinner-pips saved-branch))
                  (t (cadar (last before)))))
              (vertical
                (restore-branch-tiles saved-branch spinner-pips nil))
              (horizontal
                (append-to-end-acc
                  before (cons (list spinner-pips spinner-pips t) after) nil)))
         (make-board-state
           horizontal vertical target
           (list (caar horizontal) (cadar (last horizontal))
                 (caar vertical) (cadar (last vertical)))
           (list t
                 (find-spinner-location horizontal 0)
                 (find-spinner-location vertical 0)
                 spinner-pips)
           nil nil))))))


;;; /***************************************************************************
;;; **Function Name:** saved-branch-spinner-pips
;;; **Purpose:** Find a double when the spinner has no preceding horizontal tile.
;;; **Parameters:** saved-branch - serialized vertical pip pairs.
;;; **Return Value:** The first double's pip value, or NIL when no double is present.
;;; **Algorithm:** Recursively scan the branch for the first pair with equal pips.
;;; **Reference:** SerLISP1.txt, SerLISP2.txt, SerLISP3.txt layout convention.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun saved-branch-spinner-pips (saved-branch)
  (cond ((null saved-branch) nil)
        ((eq (car saved-branch) 'u) nil)
        ((= (caar saved-branch) (cadar saved-branch))
         (caar saved-branch))
        (t (saved-branch-spinner-pips (cdr saved-branch)))))


;;; /***************************************************************************
;;; **Function Name:** report-load-not-available
;;; **Purpose:** Preserve the previous load entry point by dispatching to the Lisp loader.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** The resumed tournament result, or NIL if no filename was entered.
;;; **Algorithm:**
;;;   1) Prompt for a saved-game filename.
;;;   2) Read the requested file and resume its saved game.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The prompt and saved field order come from Round.cpp and the SerLISP examples.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun report-load-not-available ()
  (load-lisp-game))


;;; /***************************************************************************
;;; **Function Name:** report-load-not-available-for-file
;;; **Purpose:** Load a saved Lisp game from the supplied filename.
;;; **Parameters:**
;;;   filename - requested saved-game filename, or NIL when input ended.
;;; **Return Value:** The resumed tournament result, or NIL at end of input.
;;; **Algorithm:**
;;;   1) Return NIL when no filename was supplied.
;;;   2) Otherwise read and resume the saved game.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The saved field order comes from Round.cpp and the SerLISP examples.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun report-load-not-available-for-file (filename)
  (load-lisp-game-from-filename filename))


;;; /***************************************************************************
;;; **Function Name:** load-lisp-game
;;; **Purpose:** Prompt for and resume a saved Lisp domino game.
;;; **Parameters:** None.
;;; **Return Value:** The resumed tournament result, or NIL if no filename was entered.
;;; **Algorithm:** Read the filename, restore the saved values, and continue from its next player.
;;; **Reference:** Round.cpp loadGame; SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun load-lisp-game ()
  (format t "Enter the filename to load (EX: game.txt): ")
  (finish-output)
  (load-lisp-game-from-filename (read-line *standard-input* nil nil)))


;;; /***************************************************************************
;;; **Function Name:** load-lisp-game-from-filename
;;; **Purpose:** Restore one saved game file and resume its tournament.
;;; **Parameters:** filename - saved Lisp game path, or NIL when input ended.
;;; **Return Value:** The resumed tournament result, or NIL when no filename was entered.
;;; **Algorithm:** Check that the requested file exists; resume its saved game or report the
;;;   missing path and prompt again.
;;; **Reference:** Round.cpp loadGame; SerLISP1.txt, SerLISP2.txt, SerLISP3.txt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun load-lisp-game-from-filename (filename)
  (cond
    ((null filename) nil)
    (t
     (let ((pathname (normalize-lisp-filename filename)))
       (cond
         ((probe-file pathname)
          (resume-lisp-game (read-lisp-save-file pathname)))
         (t
          (format t "Failed to open file: ~a~%" filename)
          (load-lisp-game)))))))


;;; /***************************************************************************
;;; **Function Name:** resume-lisp-game
;;; **Purpose:** Continue play from a restored save value.
;;; **Parameters:** saved-game - (tournament-target round-state next-player), or NIL.
;;; **Return Value:** The tournament result, or NIL for an empty save.
;;; **Algorithm:** Start the tournament loop at the saved next player.
;;; **Reference:** Round.cpp loadGame and play-round flow.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun resume-lisp-game (saved-game)
  (cond ((null saved-game) nil)
        (t (play-tournament-from-round (car saved-game) (cadr saved-game)))))


;;; SECTION 6: INITIAL DEAL, SHUFFLING, AND CORE DATA


;;; /***************************************************************************
;;; **Function Name:** make-domino-set
;;; **Purpose:** Create all 28 distinct pip combinations in the double-six domino set.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** An ordered list of the 28 unique tiles in the double-six set.
;;; **Algorithm:**
;;;   1) Visit each permitted pip combination in the double-six set.
;;;   2) Collect each tile once and return the completed set or row.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-domino-set ()
  (make-domino-row 0 nil))


;;; /***************************************************************************
;;; **Function Name:** deal-starting-hands
;;; **Purpose:** Give seven tiles to each player and identify the remaining boneyard.
;;; **Parameters:**
;;;   deck - ordered tiles available for dealing or shuffling.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A three-item list containing the human hand, computer hand, and remaining
;;;   boneyard.
;;; **Algorithm:**
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun deal-starting-hands (deck)
  (deal-second-hand (deal-one-hand deck +initial-hand-size+ nil)))


;;; /***************************************************************************
;;; **Function Name:** shuffle-deck
;;; **Purpose:** Randomize the order of a supplied set of domino tiles.
;;; **Parameters:**
;;;   deck - ordered tiles available for dealing or shuffling.
;;;     A list value passed by value; not modified.
;;;   seed - integer state for reproducible random choices.
;;;     A value passed by value; not modified.
;;; **Return Value:** A two-item list containing the shuffled tile list and next random seed.
;;; **Algorithm:**
;;;   1) Advance the supplied seed to determine a valid tile position.
;;;   2) Select that tile and carry the remaining tiles and next seed forward.
;;;   3) Return the shuffled result and final seed.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun shuffle-deck (deck seed)
  (shuffle-deck-acc deck seed nil))


;;; /***************************************************************************
;;; **Function Name:** initial-deal-report
;;; **Purpose:** Produce the shuffled deal and starting-player information from a supplied seed.
;;; **Parameters:**
;;;   seed - integer state for reproducible random choices.
;;;     A value passed by value; not modified.
;;; **Return Value:** A report containing the shuffled deal and starting-player information.
;;; **Algorithm:**
;;;   1) Shuffle the domino set, deal both hands, select the starter, and package the report.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun initial-deal-report (seed)
  (make-initial-deal-report
    (shuffle-deck (make-domino-set) seed)))


;;; /***************************************************************************
;;; **Function Name:** generate-random-seed
;;; **Purpose:** Choose a fresh starting value for a shuffled deal.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** A positive integer suitable as the initial shuffle seed.
;;; **Algorithm:**
;;;   1) Request a fresh random state from the Lisp runtime.
;;;   2) Choose and return a positive seed in the range accepted by the game random generator.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun generate-random-seed ()
  (1+ (random +random-seed-limit+ (make-random-state t))))


;;; /***************************************************************************
;;; **Function Name:** print-initial-deal-report
;;; **Purpose:** Show a newly shuffled deal and starting-player result.
;;; **Parameters:**
;;;   None.
;;; **Return Value:** Printed seeded-deal report; game state is unchanged.


;;; **Algorithm:** Generate a seed and forward it to the seeded initial-deal report.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-initial-deal-report ()
  (print-seeded-initial-deal-report (generate-random-seed)))


;;; /***************************************************************************
;;; **Function Name:** print-seeded-initial-deal-report
;;; **Purpose:** Show a shuffled deal generated from the supplied seed.
;;; **Parameters:**
;;;   seed - integer state for reproducible random choices.
;;;     A value passed by value; not modified.
;;; **Return Value:** Printed seeded-deal report; game state is unchanged.


;;; **Algorithm:** Print the supplied seed, shuffle the domino set with it, and display the shuffled deal.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-seeded-initial-deal-report (seed)
  (format t "~%Shuffle seed: ~a~%" seed)
  (print-shuffled-deck-and-deal
    (shuffle-deck (make-domino-set) seed)))


;;; /***************************************************************************
;;; **Function Name:** tile-left-pips
;;; **Purpose:** Read the first pip value on a tile.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The integer pip value on the left side of the tile.
;;; **Algorithm:** Return the first item of TILE.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun tile-left-pips (tile)
  (car tile))


;;; /***************************************************************************
;;; **Function Name:** tile-right-pips
;;; **Purpose:** Read the second pip value on a tile.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The integer pip value on the right side of the tile.
;;; **Algorithm:** Return the second item of TILE.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun tile-right-pips (tile)
  (cadr tile))


;;; /***************************************************************************
;;; **Function Name:** tile-spinner-p
;;; **Purpose:** Read whether a tile is marked as the spinner.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;; **Return Value:** T when the tile is marked as the spinner; otherwise NIL.
;;; **Algorithm:** Return the third item of TILE, which marks whether it is the spinner.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun tile-spinner-p (tile)
  (caddr tile))


;;; /***************************************************************************
;;; **Function Name:** tile-pip-sum
;;; **Purpose:** Calculate the total pip count on a tile.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The sum of the tile pip values.
;;; **Algorithm:**
;;;   1) Read both pip counts on the tile.
;;;   2) Add the counts and return their total.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun tile-pip-sum (tile)
  (+ (tile-left-pips tile)
     (tile-right-pips tile)))


;;; /***************************************************************************
;;; **Function Name:** player-hand
;;; **Purpose:** Read the tiles held by a player.
;;; **Parameters:**
;;;   player-state - player hand and score values.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The list of tiles held by the player.
;;; **Algorithm:** Return the first item of PLAYER-STATE, the hand.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun player-hand (player-state)
  (car player-state))


;;; /***************************************************************************
;;; **Function Name:** player-round-score
;;; **Purpose:** Read a player round score.
;;; **Parameters:**
;;;   player-state - player hand and score values.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The players score for the current round.
;;; **Algorithm:** Return the second item of PLAYER-STATE, the round score.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun player-round-score (player-state)
  (cadr player-state))


;;; /***************************************************************************
;;; **Function Name:** player-tournament-score
;;; **Purpose:** Read a player tournament score.
;;; **Parameters:**
;;;   player-state - player hand and score values.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The players accumulated tournament score.
;;; **Algorithm:** Return the third item of PLAYER-STATE, the tournament score.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun player-tournament-score (player-state)
  (caddr player-state))


;;; SECTION 7: BOARD STATE, ACCESSORS, AND TILE PLACEMENT

;;; /***************************************************************************
;;; **Function Name:** make-board-state
;;; **Purpose:** Represent board lines, exposed ends, spinner, and round information together.
;;; **Parameters:**
;;;   horizontal - ordered horizontal line of board tiles.
;;;     A list value passed by value; not modified.
;;;   vertical - ordered vertical line of board tiles.
;;;     A list value passed by value; not modified.
;;;   target - round target value.
;;;     A value passed by value; not modified.
;;;   ends - pip values exposed at board ends.
;;;     A list value passed by value; not modified.
;;;   spinner - spinner status, locations, and pip value.
;;;     A list value passed by value; not modified.
;;;   pass-check - current consecutive-pass state.
;;;     A value passed by value; not modified.
;;;   last-play - most recently played tile.
;;;     A value passed by value; not modified.
;;; **Return Value:** A board value containing the seven supplied board fields in order.
;;; **Algorithm:**
;;;  1) Place the horizontal line, vertical line, target, open ends, spinner details, pass state,
;;;   andlast play into the board value.
;;;   2) Return that board value.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-board-state (horizontal vertical target ends spinner pass-check last-play)
  (list horizontal vertical target ends spinner pass-check last-play))


;;; /***************************************************************************
;;; **Function Name:** valid-round-target-p
;;; **Purpose:** Determine whether a requested round uses an allowed scoring target.
;;; **Parameters:**
;;;   target - candidate target score for a round.
;;;     A value passed by value; not modified.
;;; **Return Value:** T when target is 3 or 5; otherwise NIL.
;;; **Algorithm:**
;;;   1) Compare the requested value with the two allowed round targets.
;;;   2) Return true for a match and false when neither target matches.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The allowed target values come from the project C++ round setup.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun valid-round-target-p (target)
  (cond
    ((eql target +round-target-three+) t)
    ((eql target +round-target-five+) t)
    (t nil)))


;;; /***************************************************************************
;;; **Function Name:** make-empty-board
;;; **Purpose:** Start a round with no tiles and an allowed target of 3 or 5.
;;; **Parameters:**
;;;   target - round target value, which must be 3 or 5.
;;;     A value passed by value; not modified.
;;; **Return Value:** An empty board for an allowed target, or NIL for an invalid target.
;;; **Algorithm:**
;;;   1) Check whether the supplied target is 3 or 5.
;;;   2) Initialize the empty board for an allowed target; return NIL otherwise.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The allowed target values come from the project C++ round setup.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-empty-board (target)
  (cond
    ((valid-round-target-p target)
     (make-board-state nil nil target (list 0 0 0 0) (list nil 0 0 -1) nil nil))
    (t nil)))


;;; /***************************************************************************
;;; **Function Name:** board-horizontal
;;; **Purpose:** Read the horizontal line of the board.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The ordered list of tiles on the horizontal board line.
;;; **Algorithm:** Return the first field of BOARD, its horizontal line.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-horizontal (board)
  (car board))


;;; /***************************************************************************
;;; **Function Name:** board-vertical
;;; **Purpose:** Read the vertical line of the board.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The ordered list of tiles on the vertical board line.
;;; **Algorithm:** Return the second field of BOARD, its vertical line.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-vertical (board)
  (cadr board))


;;; /***************************************************************************
;;; **Function Name:** board-target
;;; **Purpose:** Read the round target stored on the board.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The scoring target for the current round.
;;; **Algorithm:** Return the third field of BOARD, its round target.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-target (board)
  (caddr board))


;;; /***************************************************************************
;;; **Function Name:** board-ends
;;; **Purpose:** Read the pip values exposed at the board ends.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A four-item list of the exposed left, right, up, and down pip values.
;;; **Algorithm:** Return the fourth field of BOARD, the exposed-end values.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-ends (board)
  (cadddr board))


;;; /***************************************************************************
;;; **Function Name:** board-spinner-state
;;; **Purpose:** Read the spinner status and its board positions.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Spinner status, horizontal and vertical positions, and spinner pip value.
;;; **Algorithm:** Return the fifth field of BOARD, its spinner state.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-spinner-state (board)
  (car (cddddr board)))


;;; /***************************************************************************
;;; **Function Name:** board-pass-check
;;; **Purpose:** Read the board pass status.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The current consecutive-pass state.
;;; **Algorithm:** Return the sixth field of BOARD, its pass marker.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-pass-check (board)
  (cadr (cddddr board)))


;;; /***************************************************************************
;;; **Function Name:** board-last-play
;;; **Purpose:** Read the most recently played tile.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The tile most recently played on the board.
;;; **Algorithm:** Return the seventh field of BOARD, its latest played tile.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-last-play (board)
  (caddr (cddddr board)))


;;; /***************************************************************************
;;; **Function Name:** board-has-spinner-p
;;; **Purpose:** Determine whether the board has a spinner.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** T when the stated tile or board condition is true; otherwise NIL.
;;; **Algorithm:** Read the spinner-present flag from the board spinner state.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-has-spinner-p (board)
  (car (car (cddddr board))))


;;; /***************************************************************************
;;; **Function Name:** board-horizontal-spinner-location
;;; **Purpose:** Read the spinner position on the horizontal line.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The spinner tile position on the horizontal line.
;;; **Algorithm:** Read the horizontal spinner position from the board spinner state.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-horizontal-spinner-location (board)
  (cadr (car (cddddr board))))


;;; /***************************************************************************
;;; **Function Name:** board-vertical-spinner-location
;;; **Purpose:** Read the spinner position on the vertical line.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The spinner tile position on the vertical line.
;;; **Algorithm:** Read the vertical spinner position from the board spinner state.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-vertical-spinner-location (board)
  (caddr (car (cddddr board))))


;;; /***************************************************************************
;;; **Function Name:** tile-double-p
;;; **Purpose:** Determine whether a tile has matching pip values.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;; **Return Value:** T when both tile pips are equal; otherwise NIL.
;;; **Algorithm:**
;;;   1) Compare the two pip counts on the tile.
;;;   2) Return true when they are equal and false otherwise.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun tile-double-p (tile)
  (= (car tile) (cadr tile)))


;;; /***************************************************************************
;;; **Function Name:** tile-with-spinner
;;; **Purpose:** Return a tile marked with the requested spinner status.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;;   flag - value used in this game operation.
;;;     A value passed by value; not modified.
;;; **Return Value:** A tile with the original pips and requested spinner marker.
;;; **Algorithm:**
;;;   1) Copy both pip values and attach the requested spinner marker.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun tile-with-spinner (tile flag)
  (list (tile-left-pips tile) (tile-right-pips tile) flag))


;;; /***************************************************************************
;;; **Function Name:** board-end
;;; **Purpose:** Read the exposed pip value for the requested board direction.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;;   direction - board direction involved in the operation.
;;;     A value passed by value; not modified.
;;; **Return Value:** The exposed pip value in the selected direction, or NIL for an invalid
;;;   direction.
;;; **Algorithm:**
;;;   1) Identify which exposed board end is named by the direction.
;;;   2) Return that ends pip value, or NIL for an unsupported direction.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-end (board direction)
  (let ((ends (cadddr board)))
    (cond
      ((eq direction 'left) (car ends))
      ((eq direction 'right) (cadr ends))
      ((eq direction 'up) (caddr ends))
      ((eq direction 'down) (cadddr ends))
      (t nil))))


;;; /***************************************************************************
;;; **Function Name:** board-spinner-pips
;;; **Purpose:** Read the pip value of the board spinner.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The pip value on the board spinner.
;;; **Algorithm:**
;;;   1) Read the pip value stored for the spinner.
;;;   2) Return that value.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-spinner-pips (board)
  (cadddr (car (cddddr board))))


;;; /***************************************************************************
;;; **Function Name:** swap-tile-pips
;;; **Purpose:** Reverse the pip order of a tile while retaining its marker.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A tile with its pips reversed and spinner marker retained.
;;; **Algorithm:**
;;;   1) Exchange the two pip values and retain the spinner marker.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun swap-tile-pips (tile)
  (list (tile-right-pips tile) (tile-left-pips tile) (tile-spinner-p tile)))


;;; /***************************************************************************
;;; **Function Name:** orient-right-to-match
;;; **Purpose:** Orient a tile so its right side joins the requested pip value.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;;   match-pips - pip value a tile must connect to.
;;;     A value passed by value; not modified.
;;; **Return Value:** An oriented tile, or NIL when neither pip matches.
;;; **Algorithm:**
;;;   1) Keep the tile when its right pip matches; otherwise swap it if its left pip matches.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun orient-right-to-match (tile match-pips)
  (cond
    ((= (cadr tile) match-pips) tile)
    ((= (car tile) match-pips)
     (list (cadr tile) (car tile) (caddr tile)))
    (t nil)))


;;; /***************************************************************************
;;; **Function Name:** orient-left-to-match
;;; **Purpose:** Orient a tile so its left side joins the requested pip value.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;;   match-pips - pip value a tile must connect to.
;;;     A value passed by value; not modified.
;;; **Return Value:** An oriented tile, or NIL when neither pip matches.
;;; **Algorithm:**
;;;   1) Keep the tile when its left pip matches; otherwise swap it if its right pip matches.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun orient-left-to-match (tile match-pips)
  (cond
    ((= (car tile) match-pips) tile)
    ((= (cadr tile) match-pips)
     (list (cadr tile) (car tile) (caddr tile)))
    (t nil)))


;;; /***************************************************************************
;;; **Function Name:** add-first-board-tile
;;; **Purpose:** Start a board with the first tile played.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A board value containing the resulting lines, exposed ends, and spinner
;;;   information.
;;; **Algorithm:**
;;;   1) Mark a first-played double as the spinner; otherwise place it on the horizontal line.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun add-first-board-tile (board tile)
  (cond
    ((tile-double-p tile)
     (add-first-board-spinner board (tile-with-spinner tile t)))
    (t
     (make-board-state
       (list tile) nil (board-target board)
       (list (tile-left-pips tile) (tile-right-pips tile) 0 0)
       (board-spinner-state board) nil tile))))


;;; /***************************************************************************
;;; **Function Name:** add-first-board-spinner
;;; **Purpose:** Start a board with a double that opens the spinner.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;;   spinner-tile - double that starts the spinner.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A board value containing the resulting lines, exposed ends, and spinner
;;;   information.


;;; **Algorithm:** Mark the first double as the spinner and build the initial horizontal and
;;;   vertical lines, exposed ends, and spinner state.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun add-first-board-spinner (board spinner-tile)
  (make-board-state
    (list spinner-tile) (list spinner-tile) (board-target board)
    (list 0 0 (tile-left-pips spinner-tile) (tile-right-pips spinner-tile))
    (list t 0 0 (tile-left-pips spinner-tile)) nil spinner-tile))


;;; /***************************************************************************
;;; **Function Name:** place-board-tile
;;; **Purpose:** Route a tile to the selected legal board end.
;;; **Parameters:** board - current board; tile - tile to place; direction - selected end.
;;; **Return Value:** Updated board state, or the original board when the direction or tile placement is illegal.
;;; **Algorithm:** Start an empty board with its first tile; otherwise dispatch to the selected
;;;   left, right, upper, or lower placement task.
;;; **Reference:** Placement rules from Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun place-board-tile (board tile direction)
  (cond
    ((null (board-horizontal board))
     (add-first-board-tile board tile))
    ((eq direction 'left)
     (place-left-board-tile board tile))
    ((eq direction 'right)
     (place-right-board-tile board tile))
    ((eq direction 'up)
     (cond
       ((board-has-spinner-p board)
        (place-up-board-tile board tile))
       (t board)))
    ((eq direction 'down)
     (cond
       ((board-has-spinner-p board)
        (place-down-board-tile board tile))
       (t board)))
    (t board)))


;;; /***************************************************************************
;;; **Function Name:** place-left-board-tile
;;; **Purpose:** Orient and place a tile at the left horizontal end.
;;; **Parameters:** board - current board; tile - tile to place.
;;; **Return Value:** Updated board state, or the original board when the tile does not match.
;;; **Algorithm:** Match the tile to the left end; open a spinner for the first double, or
;;;   extend the left end and shift any existing spinner position.
;;; **Reference:** Placement rules from Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun place-left-board-tile (board tile)
  (let* ((existing-spinner (board-has-spinner-p board))
         (oriented
           (orient-right-to-match
             tile (tile-left-pips (car (board-horizontal board)))))
         (starts-spinner
           (and oriented (not existing-spinner) (tile-double-p oriented))))
    (cond
      ((null oriented) board)
      (starts-spinner
       (let ((spinner-tile
               (list (tile-left-pips oriented)
                     (tile-right-pips oriented)
                     t)))
         (make-board-state
           (cons spinner-tile (board-horizontal board))
           (list spinner-tile)
           (board-target board)
           (list 0 (board-end board 'right)
                 (tile-left-pips spinner-tile)
                 (tile-right-pips spinner-tile))
           (list t 0 0 (tile-left-pips spinner-tile))
           nil spinner-tile)))
      (t
       (make-board-state
         (cons oriented (board-horizontal board))
         (board-vertical board)
         (board-target board)
         (list (tile-left-pips oriented)
               (board-end board 'right)
               (board-end board 'up)
               (board-end board 'down))
         (cond
           (existing-spinner
            (list t
                  (1+ (board-horizontal-spinner-location board))
                  (board-vertical-spinner-location board)
                  (board-spinner-pips board)))
           (t (board-spinner-state board)))
         nil oriented)))))


;;; /***************************************************************************
;;; **Function Name:** place-right-board-tile
;;; **Purpose:** Orient and place a tile at the right horizontal end.
;;; **Parameters:** board - current board; tile - tile to place.
;;; **Return Value:** Updated board state, or the original board when the tile does not match.
;;; **Algorithm:** Match the tile to the right end; open a spinner for the first double, or
;;;   extend the right end while retaining any existing spinner.
;;; **Reference:** Placement rules from Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun place-right-board-tile (board tile)
  (let* ((existing-spinner (board-has-spinner-p board))
         (oriented
           (orient-left-to-match
             tile (cadar (last (board-horizontal board)))))
         (starts-spinner
           (and oriented (not existing-spinner) (tile-double-p oriented))))
    (cond
      ((null oriented) board)
      (starts-spinner
       (let ((spinner-tile
               (list (tile-left-pips oriented)
                     (tile-right-pips oriented)
                     t)))
         (make-board-state
           (append-to-end-acc
             (board-horizontal board) (list spinner-tile) nil)
           (list spinner-tile)
           (board-target board)
           (list (board-end board 'left) 0
                 (tile-left-pips spinner-tile)
                 (tile-right-pips spinner-tile))
           (list t (list-length-rec (board-horizontal board))
                 0 (tile-left-pips spinner-tile))
           nil spinner-tile)))
      (t
       (make-board-state
         (append-to-end-acc (board-horizontal board) (list oriented) nil)
         (board-vertical board)
         (board-target board)
         (list (board-end board 'left)
               (tile-right-pips oriented)
               (board-end board 'up)
               (board-end board 'down))
         (board-spinner-state board)
         nil oriented)))))


;;; /***************************************************************************
;;; **Function Name:** place-up-board-tile
;;; **Purpose:** Orient and place a tile at the upper spinner end.
;;; **Parameters:** board - current board; tile - tile to place.
;;; **Return Value:** Updated board state, or the original board when the tile cannot connect.
;;; **Algorithm:** Match the tile to the upper open end, prepend it to the vertical line, and
;;;   advance the spinner position.
;;; **Reference:** Placement rules from Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun place-up-board-tile (board tile)
  (let ((oriented
          (orient-right-to-match
            tile (tile-left-pips (car (board-vertical board))))))
    (cond
      ((null oriented) board)
      ((not (board-has-spinner-p board)) board)
      (t
       (make-board-state
         (board-horizontal board)
         (cons oriented (board-vertical board))
         (board-target board)
         (list (board-end board 'left)
               (board-end board 'right)
               (tile-left-pips oriented)
               (board-end board 'down))
         (list t
               (board-horizontal-spinner-location board)
               (1+ (board-vertical-spinner-location board))
               (board-spinner-pips board))
         nil oriented)))))


;;; /***************************************************************************
;;; **Function Name:** place-down-board-tile
;;; **Purpose:** Orient and place a tile at the lower spinner end.
;;; **Parameters:** board - current board; tile - tile to place.
;;; **Return Value:** Updated board state, or the original board when the tile cannot connect.
;;; **Algorithm:** Match the tile to the lower open end, append it to the vertical line, and
;;;   retain the spinner state.
;;; **Reference:** Placement rules from Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun place-down-board-tile (board tile)
  (let ((oriented
          (orient-left-to-match
            tile (cadar (last (board-vertical board))))))
    (cond
      ((null oriented) board)
      ((not (board-has-spinner-p board)) board)
      (t
       (make-board-state
         (board-horizontal board)
         (append-to-end-acc (board-vertical board) (list oriented) nil)
         (board-target board)
         (list (board-end board 'left)
               (board-end board 'right)
               (board-end board 'up)
               (tile-right-pips oriented))
         (board-spinner-state board)
         nil oriented)))))


;;; /***************************************************************************
;;; **Function Name:** last-list-item
;;; **Purpose:** Read the final item in an ordered list.
;;; **Parameters:**
;;;   items - input list of values; list passed by value, not modified.
;;;   last-item - optional fallback returned when ITEMS is empty.
;;; **Return Value:** The final item in the list, or NIL for an empty list.
;;; **Algorithm:**
;;;   1) Walk the list and return its last element.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun last-list-item (items &optional last-item)
  (cond
    ((null items) last-item)
    ((null (cdr items)) (car items))
    (t (last-list-item (cdr items) (car items)))))


;;; /***************************************************************************
;;; **Function Name:** last-list-item-acc
;;; **Purpose:** Find the final item while carrying the most recently visited item.
;;; **Parameters:**
;;;   items - input list of values.
;;;     A list value passed by value; not modified.
;;;   last-item - most recently visited item.
;;;     A value passed by value; not modified.
;;; **Return Value:** The final item in the list, or the supplied fallback for an empty list.
;;; **Algorithm:**
;;;   1) Return the final item when found; otherwise return the carried fallback.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun last-list-item-acc (items last-item)
  (cond
    ((null items) last-item)
    ((null (cdr items)) (car items))
    (t (last-list-item-acc (cdr items) (car items)))))


;;; SECTION 8: BOARD, TILE-SET, DEAL, AND STARTER HELPERS

;;; /***************************************************************************
;;; **Function Name:** make-domino-row
;;; **Purpose:** Generate all rows remaining in the double-six tile set.
;;; **Parameters:**
;;;   left-pips - first pip value of a tile or generated row.
;;;     A value passed by value; not modified.
;;;   tiles-accumulator - tiles generated or collected so far.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The ordered tiles generated for this and all later rows.
;;; **Algorithm:**
;;;   1) Visit each permitted pip combination in the double-six set.
;;;   2) Collect each tile once and return the completed set or row.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-domino-row (left-pips tiles-accumulator)
  (cond
    ((> left-pips +maximum-domino-pips+)
     (reverse-list tiles-accumulator))
    (t
     (make-domino-column left-pips
                         left-pips
                         tiles-accumulator))))


;;; /***************************************************************************
;;; **Function Name:** make-domino-column
;;; **Purpose:** Generate tiles for one first-pip value.
;;; **Parameters:**
;;;   left-pips - first pip value of a tile or generated row.
;;;     A value passed by value; not modified.
;;;   right-pips - second pip value of a tile.
;;;     A value passed by value; not modified.
;;;   tiles-accumulator - tiles generated or collected so far.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The tiles generated for the current first-pip value and later rows.
;;; **Algorithm:**
;;;   1) Visit each permitted pip combination in the double-six set.
;;;   2) Collect each tile once and return the completed set or row.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-domino-column (left-pips right-pips tiles-accumulator)
  (cond
    ((> right-pips +maximum-domino-pips+)
     (make-domino-row (1+ left-pips)
                      tiles-accumulator))
    (t
     (make-domino-column left-pips
                         (1+ right-pips)
                         (cons (list left-pips right-pips nil)
                               tiles-accumulator)))))


;;; /***************************************************************************
;;; **Function Name:** deal-second-hand
;;; **Purpose:** Deal the computer hand from the stock left after the human hand.
;;; **Parameters:**
;;;   human-deal - human hand and stock remaining after that deal.
;;;     A value passed by value; not modified.
;;; **Return Value:** A three-item list containing the human hand, computer hand, and remaining
;;;   boneyard.
;;; **Algorithm:**
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun deal-second-hand (human-deal)
  (make-deal-result (car human-deal)
                    (deal-one-hand (cadr human-deal) +initial-hand-size+ nil)))


;;; /***************************************************************************
;;; **Function Name:** make-deal-result
;;; **Purpose:** Combine both hands with the remaining stock.
;;; **Parameters:**
;;;   human-hand - tiles dealt to the human player.
;;;     A list value passed by value; not modified.
;;;   computer-deal - computer hand and remaining stock.
;;;     A value passed by value; not modified.
;;; **Return Value:** A three-item list containing the human hand, computer hand, and remaining
;;;   boneyard.


;;; **Algorithm:** Package the supplied human hand, computer hand, and remaining stock in order.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-deal-result (human-hand computer-deal)
  (list human-hand
        (car computer-deal)
        (cadr computer-deal)))


;;; /***************************************************************************
;;; **Function Name:** make-initial-deal-report
;;; **Purpose:** Assemble the shuffled deck, deal, and starting-player result.
;;; **Parameters:**
;;;   shuffle-result - shuffled tiles and next random seed.
;;;     A value passed by value; not modified.
;;; **Return Value:** A structured report for the shuffle, deal, and starting-player choice.
;;; **Algorithm:**
;;;   1) Deal from the shuffled deck, select the starter, and assemble the report.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun make-initial-deal-report (shuffle-result)
  (complete-initial-deal-report
    (car shuffle-result)
    (deal-starting-hands (car shuffle-result))
    (cadr shuffle-result)))


;;; /***************************************************************************
;;; **Function Name:** complete-initial-deal-report
;;; **Purpose:** Add the deal and starter selection to the initial report.
;;; **Parameters:**
;;;   shuffled-deck - ordered tiles produced by shuffling.
;;;     A list value passed by value; not modified.
;;;   dealt-state - both dealt hands and remaining stock.
;;;     A list value passed by value; not modified.
;;;   next-seed - random seed for the next choice.
;;;     A value passed by value; not modified.
;;; **Return Value:** A structured report for the shuffle, deal, and starting-player choice.
;;; **Algorithm:**
;;;   1) Select the starter from the remaining stock, then package the report values.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun complete-initial-deal-report (shuffled-deck dealt-state next-seed)
  (complete-deal-report
    shuffled-deck
    dealt-state
    (choose-first-player (caddr dealt-state))
    next-seed))


;;; /***************************************************************************
;;; **Function Name:** complete-deal-report
;;; **Purpose:** Package the shuffled deck, hands, starting player, and seed.
;;; **Parameters:**
;;;   shuffled-deck - ordered tiles produced by shuffling.
;;;     A list value passed by value; not modified.
;;;   dealt-state - both dealt hands and remaining stock.
;;;     A list value passed by value; not modified.
;;;   first-player-state - selected starter, draw history, and remaining stock.
;;;     A list value passed by value; not modified.
;;;   next-seed - random seed for the next choice.
;;;     A value passed by value; not modified.
;;; **Return Value:** A structured report containing the shuffled tiles, hands, starter, and seed.
;;; **Algorithm:**
;;;   1) Package the shuffled deck, hands, starter, draw history, remaining deck, and seed.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun complete-deal-report (shuffled-deck dealt-state first-player-state next-seed)
  (list (list "Shuffled deck" shuffled-deck)
        (list "Human hand" (car dealt-state))
        (list "Computer hand" (cadr dealt-state))
        (list "Starting player" (car first-player-state))
        (list "First-player draw history" (cadr first-player-state))
        (list "Remaining deck" (caddr first-player-state))
        (list "Next random seed" next-seed)))


;;; /***************************************************************************
;;; **Function Name:** print-shuffled-deck-and-deal
;;; **Purpose:** Show the unshuffled set, shuffled set, and dealt tiles.
;;; **Parameters:**
;;;   shuffle-result - shuffled tiles and next random seed.
;;;     A value passed by value; not modified.
;;; **Return Value:** Printed unshuffled and shuffled decks, both hands, and starter details.


;;; **Algorithm:** Print the canonical set and shuffled set, then display both hands and starter selection.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-shuffled-deck-and-deal (shuffle-result)
  (format t "~%Unshuffled deck (28 tiles):~%")
  (print-tile-list (make-domino-set))
  (format t "~%Shuffled deck (28 tiles):~%")
  (print-tile-list (car shuffle-result))
  (print-dealt-hands (deal-starting-hands (car shuffle-result))))


;;; /***************************************************************************
;;; **Function Name:** print-dealt-hands
;;; **Purpose:** Show both players hands and determine who starts.
;;; **Parameters:**
;;;   dealt-state - both dealt hands and remaining stock.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed both hands, starter draw history, selected starter, and initial board.


;;; **Algorithm:** Print both dealt hands, select a starter from the remaining stock, and display the result.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-dealt-hands (dealt-state)
  (format t "~%Human hand (7 tiles):~%")
  (print-tile-list (car dealt-state))
  (format t "~%Computer hand (7 tiles):~%")
  (print-tile-list (cadr dealt-state))
  (print-first-player-selection
    (choose-first-player (caddr dealt-state))))


;;; /***************************************************************************
;;; **Function Name:** choose-first-player
;;; **Purpose:** Select a starter by comparing tile totals drawn from the stock.
;;; **Parameters:**
;;;   stock - ordered tiles in the boneyard.
;;;     A value passed by value; not modified.
;;; **Return Value:** A three-item list containing starter, chronological draw history, and
;;;   remaining stock.
;;; **Algorithm:**
;;;   1) Draw two tiles and compare the players pip totals.
;;;   2) Retain tied pairs and repeat, or select the player with the higher total.
;;;   3) Return the starter, draw history, and stock.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun choose-first-player (stock)
  (choose-first-player-acc stock nil))


;;; /***************************************************************************
;;; **Function Name:** choose-first-player-acc
;;; **Purpose:** Continue starter selection while retaining tied draw pairs.
;;; **Parameters:**
;;;   stock - ordered tiles in the boneyard.
;;;     A value passed by value; not modified.
;;;   draw-history - prior pairs drawn to select the starter.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A three-item list containing starter, chronological draw history, and
;;;   remaining stock.
;;; **Algorithm:**
;;;   1) Draw two tiles and compare the players pip totals.
;;;   2) Retain tied pairs and repeat, or select the player with the higher total.
;;;   3) Return the starter, draw history, and stock.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun choose-first-player-acc (stock draw-history)
  (choose-first-player-after-draw
    (draw-pair-from-stock stock)
    draw-history))


;;; /***************************************************************************
;;; **Function Name:** choose-first-player-after-draw
;;; **Purpose:** Resolve the latest draw or repeat the comparison after a tie.
;;; **Parameters:**
;;;   draw-state - drawn pair and stock after drawing.
;;;     A list value passed by value; not modified.
;;;   draw-history - prior pairs drawn to select the starter.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A three-item list containing starter, chronological draw history, and
;;;   remaining stock.
;;; **Algorithm:**
;;;   1) Draw two tiles and compare the players pip totals.
;;;   2) Retain tied pairs and repeat, or select the player with the higher total.
;;;   3) Return the starter, draw history, and stock.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun choose-first-player-after-draw (draw-state draw-history)
  (cond
    ((null (car draw-state))
     (list 'undetermined
           (reverse-list draw-history)
           (caddr draw-state)))
    ((null (cadr draw-state))
     (list 'undetermined
           (reverse-list draw-history)
           (caddr draw-state)))
    ((= (tile-pip-sum (car draw-state))
        (tile-pip-sum (cadr draw-state)))
     (choose-first-player-acc
       (return-drawn-pair draw-state)
       (cons (list (car draw-state) (cadr draw-state))
             draw-history)))
    ((> (tile-pip-sum (car draw-state))
        (tile-pip-sum (cadr draw-state)))
     (complete-first-player-choice 'human draw-state draw-history))
    (t
     (complete-first-player-choice 'computer draw-state draw-history))))


;;; /***************************************************************************
;;; **Function Name:** complete-first-player-choice
;;; **Purpose:** Record the higher-scoring player and restore the drawn pair to stock.
;;; **Parameters:**
;;;   winner - player selected to start.
;;;     A value passed by value; not modified.
;;;   draw-state - drawn pair and stock after drawing.
;;;     A list value passed by value; not modified.
;;;   draw-history - prior pairs drawn to select the starter.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A three-item list containing starter, chronological draw history, and
;;;   remaining stock.
;;; **Algorithm:**
;;;   1) Draw two tiles and compare the players pip totals.
;;;   2) Retain tied pairs and repeat, or select the player with the higher total.
;;;   3) Return the starter, draw history, and stock.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun complete-first-player-choice (winner draw-state draw-history)
  (list winner
        (reverse-list
          (cons (list (car draw-state) (cadr draw-state))
                draw-history))
        (return-drawn-pair draw-state)))


;;; /***************************************************************************
;;; **Function Name:** draw-pair-from-stock
;;; **Purpose:** Take the next two tiles from the boneyard for comparison.
;;; **Parameters:**
;;;   stock - ordered tiles in the boneyard.
;;;     A value passed by value; not modified.
;;; **Return Value:** A three-item list containing two tiles and the remaining stock.
;;; **Algorithm:**
;;;   1) Draw two tiles and compare the players pip totals.
;;;   2) Retain tied pairs and repeat, or select the player with the higher total.
;;;   3) Return the starter, draw history, and stock.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun draw-pair-from-stock (stock)
  (cond
    ((null stock)
     (list nil nil nil))
    ((null (cdr stock))
     (list (car stock) nil nil))
    (t
     (list (car stock)
           (cadr stock)
           (cddr stock)))))


;;; /***************************************************************************
;;; **Function Name:** return-drawn-pair
;;; **Purpose:** Return compared tiles to the end of the boneyard in draw order.
;;; **Parameters:**
;;;   draw-state - drawn pair and stock after drawing.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The boneyard list with the drawn pair restored at its end.
;;; **Algorithm:**
;;;   1) Append the compared tiles to the end of the remaining stock in draw order.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun return-drawn-pair (draw-state)
  (append-to-end
    (caddr draw-state)
    (list (car draw-state) (cadr draw-state))))


;;; /***************************************************************************
;;; **Function Name:** print-first-player-selection
;;; **Purpose:** Show the draw history, selected starter, and initial board.
;;; **Parameters:**
;;;   selection - selected starter, draw history, and remaining stock.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed starter draw history, the selected starter, and initial board.


;;; **Algorithm:** Print the draw history and starter result, then show the empty board.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-first-player-selection (selection)
  (format t "~%Drawing to determine who plays first:~%")
  (print-first-player-draw-history (cadr selection))
  (cond
    ((eq (car selection) 'human)
     (format t "Human goes first.~%"))
    ((eq (car selection) 'computer)
     (format t "Computer goes first.~%"))
    (t
     (format t "No starting player could be selected.~%")))
  (format t "~%Initial empty board:~%")
  (print-board (make-empty-board +round-target-three+)))


;;; /***************************************************************************
;;; **Function Name:** print-first-player-draw
;;; **Purpose:** Show one pair of tiles and compare their pip totals.
;;; **Parameters:**
;;;   draw-pair - pair of tiles drawn for comparison; list passed by value.
;;; **Return Value:** Printed tile values, pip totals, and the comparison result.
;;; **Algorithm:** Calculate each tile total, display the pair, and report which total is higher.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Starter selection rules came from the project C++ files.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-first-player-draw (draw-pair)
  (let* ((human-tile (car draw-pair))
         (computer-tile (cadr draw-pair))
         (human-total (tile-pip-sum human-tile))
         (computer-total (tile-pip-sum computer-tile)))
    (format t "Human draws ~a-~a (sum ~a); Computer draws ~a-~a (sum ~a). "
            (tile-left-pips human-tile) (tile-right-pips human-tile) human-total
            (tile-left-pips computer-tile) (tile-right-pips computer-tile)
            computer-total)
    (cond
      ((> human-total computer-total)
       (format t "Human has the higher total.~%"))
      ((< human-total computer-total)
       (format t "Computer has the higher total.~%"))
      (t
       (format t "The totals tie; drawing again.~%")))))


;;; /***************************************************************************
;;; **Function Name:** print-first-player-draw-history
;;; **Purpose:** Show each pair used to select the starter.
;;; **Parameters:**
;;;   draw-history - prior pairs drawn to select the starter.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed all recorded draw pairs in chronological order, or a message when
;;;   no draw pairs are available.
;;; **Algorithm:** Report when no draw pairs are available; otherwise print each pair recursively.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-first-player-draw-history (draw-history)
  (cond
    ((null draw-history)
     (format t "No draw pairs were available.~%"))
    (t
     (print-first-player-draw (car draw-history))
     (print-first-player-draw-history (cdr draw-history)))))


;;; Board layout output

;;; /***************************************************************************
;;; **Function Name:** print-board
;;; **Purpose:** Show the current board layout, including its spinner when present.
;;; **Parameters:**
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed the board layout; board state is unchanged.


;;; **Algorithm:** Choose the board layout from its horizontal and vertical lines, then print the open ends and tiles.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-board (board)
  (format t "~%GAME BOARD:~%")
  (cond
    ((null (board-vertical board))
     (format t "L ")
     (print-horizontal-board-line (board-horizontal board))
     (format t "R~%"))
    ((null (board-horizontal board))
     (print-vertical-only-board (board-vertical board)))
    (t
     (print-spaces-to-spinner board)
     (format t "U~%")
     (print-vertical-tiles-above
       (board-vertical board)
       (board-vertical-spinner-location board)
       board)
     (print-spinner-row (board-horizontal board) 'pips)
     (print-spinner-row (board-horizontal board) 'bars)
     (format t "L ")
     (print-horizontal-board-line (board-horizontal board))
     (format t "R~%")
     (print-board-down
       (board-vertical board)
       (1+ (board-vertical-spinner-location board))
       board))))


;;; /***************************************************************************
;;; **Function Name:** print-vertical-only-board
;;; **Purpose:** Print the vertical board line with its end markers.
;;; **Parameters:**
;;;   vertical - top-to-bottom vertical board line; list passed by value, not modified.
;;;   start-p - optional flag indicating whether to print the upper U marker; defaults to true.
;;; **Return Value:** Printed the U marker when requested, all vertical tiles, and the D marker.
;;; **Algorithm:** Print the optional U marker, the vertical line, and the D marker.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-vertical-only-board (vertical &optional (start-p t))
  (cond
    ((and start-p vertical)
     (format t "U~%")
     (print-vertical-only-board vertical nil))
    ((null vertical)
     (format t "D~%"))
    (t
     (format t "~a~%|~%~a~%"
             (caar vertical) (cadar vertical))
     (print-vertical-only-board (cdr vertical) nil))))


;;; /***************************************************************************
;;; **Function Name:** print-vertical-only-tiles
;;; **Purpose:** Show the tiles in a vertical-only board line.
;;; **Parameters:**
;;;   vertical - ordered vertical line of board tiles.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed the tiles in a vertical-only board line.


;;; **Algorithm:** Print the vertical tiles recursively in their board orientation.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-vertical-only-tiles (vertical)
  (cond
    ((null vertical) nil)
    (t
     (format t "~a~%|~%~a~%"
             (tile-left-pips (car vertical))
             (tile-right-pips (car vertical)))
     (print-vertical-only-tiles (cdr vertical)))))


;;; /***************************************************************************
;;; **Function Name:** print-board-up
;;; **Purpose:** Show the vertical tiles above the spinner and spinner rows.
;;; **Parameters:**
;;;   vertical - ordered vertical line of board tiles.
;;;     A list value passed by value; not modified.
;;;   count - number of tiles or items to process.
;;;     A value passed by value; not modified.
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed the upper vertical tiles, spinner labels, and alignment rows.


;;; **Algorithm:** Print the upper board branch, aligned with the spinner position.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-board-up (vertical count board)
  (print-spaces-to-spinner board)
  (format t "U~%")
  (print-vertical-tiles-above vertical count board)
  (print-spinner-row (board-horizontal board) 'pips)
  (print-spinner-row (board-horizontal board) 'bars))


;;; /***************************************************************************
;;; **Function Name:** print-vertical-tiles-above
;;; **Purpose:** Show the vertical tiles that lie above the spinner.
;;; **Parameters:**
;;;   vertical - ordered vertical line of board tiles.
;;;     A list value passed by value; not modified.
;;;   count - number of tiles or items to process.
;;;     A value passed by value; not modified.
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed up to COUNT tiles above the spinner, stopping at the line boundary.
;;; **Algorithm:** Stop at COUNT or the line boundary; print each tile in its vertical orientation
;;;   and recurse toward the spinner.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-vertical-tiles-above (vertical count board)
  (cond
    ((or (= count 0) (null vertical)) nil)
    ((and (= (caar vertical) (cadar vertical))
          (not (caddar vertical)))
     (print-spaces-to-spinner board)
     (format t "~a-~a~%" (caar vertical) (cadar vertical))
     (print-vertical-tiles-above (cdr vertical) (1- count) board))
    (t
     (print-spaces-to-spinner board)
     (format t "~a~%" (caar vertical))
     (print-spaces-to-spinner board)
     (format t "|~%")
     (print-spaces-to-spinner board)
     (format t "~a~%" (cadar vertical))
     (print-vertical-tiles-above (cdr vertical) (1- count) board))))


;;; /***************************************************************************
;;; **Function Name:** print-vertical-tile
;;; **Purpose:** Show a vertical-line tile in the correct orientation for its tile value.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed the selected tile, side by side for a non-spinner double or stacked otherwise.
;;; **Algorithm:** Print a non-spinner double side by side; print other vertical tiles with their pips stacked.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-vertical-tile (tile board)
  (cond
    ((and (tile-double-p tile)
          (not (tile-spinner-p tile)))
     (print-spaces-to-spinner board)
     (format t "~a-~a~%"
             (tile-left-pips tile)
             (tile-right-pips tile)))
    (t
     (print-spaces-to-spinner board)
     (format t "~a~%" (tile-left-pips tile))
     (print-spaces-to-spinner board)
     (format t "|~%")
     (print-spaces-to-spinner board)
     (format t "~a~%" (tile-right-pips tile)))))


;;; /***************************************************************************
;;; **Function Name:** print-spinner-row
;;; **Purpose:** Show one crosswise spinner alignment row.
;;; **Parameters:**
;;;   horizontal - ordered horizontal line of board tiles.
;;;     A list value passed by value; not modified.
;;;   row-kind - spinner display row being printed.
;;;     A value passed by value; not modified.
;;; **Return Value:** Printed one spinner alignment row.


;;; **Algorithm:** Print one spinner row by adding its leading spacing, row marks, and line ending.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-spinner-row (horizontal row-kind)
  (format t "  ")
  (print-spinner-row-tiles horizontal row-kind)
  (format t "~%"))


;;; /***************************************************************************
;;; **Function Name:** print-spinner-row-tiles
;;; **Purpose:** Show spinner marks for the horizontal tiles.
;;; **Parameters:**
;;;   horizontal - ordered horizontal line of board tiles.
;;;     A list value passed by value; not modified.
;;;   row-kind - spinner display row being printed.
;;;     A value passed by value; not modified.
;;; **Return Value:** Printed the pip or divider marks and spacing for the horizontal tiles.
;;; **Algorithm:** Print the appropriate spinner mark for each horizontal tile, then continue across the row.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-spinner-row-tiles (horizontal row-kind)
  (cond
    ((null horizontal) nil)
    ((= (caar horizontal) (cadar horizontal))
     (cond ((eq row-kind 'pips)
            (format t "~a " (caar horizontal)))
           (t (format t "| ")))
     (print-spinner-row-tiles (cdr horizontal) row-kind))
    (t
     (format t "    ")
     (print-spinner-row-tiles (cdr horizontal) row-kind))))


;;; /***************************************************************************
;;; **Function Name:** print-double-spinner-mark
;;; **Purpose:** Show the pip or divider for a double.
;;; **Parameters:**
;;;   tile - domino with two pip values and a spinner marker.
;;;     A list value passed by value; not modified.
;;;   row-kind - spinner display row being printed.
;;;     A value passed by value; not modified.
;;; **Return Value:** Printed a double pip or its divider, depending on ROW-KIND.


;;; **Algorithm:** Print the spinner pip on the pips row or the divider on the other row.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-double-spinner-mark (tile row-kind)
  (cond
    ((eq row-kind 'pips) (format t "~a " (tile-left-pips tile)))
    (t (format t "| "))))


;;; /***************************************************************************
;;; **Function Name:** print-board-down
;;; **Purpose:** Show vertical tiles below the spinner and the lower end label.
;;; **Parameters:**
;;;   vertical - ordered vertical line of board tiles.
;;;     A list value passed by value; not modified.
;;;   skip-count - number of vertical tiles to skip.
;;;     A value passed by value; not modified.
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed the lower vertical branch and its end label.


;;; **Algorithm:** Skip the upper branch, print the lower vertical tiles, and finish with the D marker.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-board-down (vertical skip-count board)
  (cond
    ((and (> skip-count 0) vertical)
     (print-board-down (cdr vertical) (1- skip-count) board))
    ((null vertical)
     (print-spaces-to-spinner board)
     (format t "D~%"))
    ((and (= (caar vertical) (cadar vertical))
          (not (caddar vertical)))
     (print-spaces-to-spinner board)
     (format t "~a-~a~%" (caar vertical) (cadar vertical))
     (print-board-down (cdr vertical) 0 board))
    (t
     (print-spaces-to-spinner board)
     (format t "~a~%" (caar vertical))
     (print-spaces-to-spinner board)
     (format t "|~%")
     (print-spaces-to-spinner board)
     (format t "~a~%" (cadar vertical))
     (print-board-down (cdr vertical) 0 board))))


;;; /***************************************************************************
;;; **Function Name:** drop-list-prefix
;;; **Purpose:** Skip the requested number of leading list items.
;;; **Parameters:**
;;;   items - input list of values.
;;;     A list value passed by value; not modified.
;;;   count - number of tiles or items to process.
;;;     A value passed by value; not modified.
;;; **Return Value:** The list after dropping COUNT leading items, or NIL if exhausted first.


;;; **Algorithm:** Return ITEMS when COUNT is zero; otherwise drop one item and recur until COUNT is zero or the list ends.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun drop-list-prefix (items count)
  (cond
    ((= count 0) items)
    ((null items) nil)
    (t (drop-list-prefix (cdr items) (1- count)))))


;;; /***************************************************************************
;;; **Function Name:** print-vertical-tiles
;;; **Purpose:** Show each tile in a vertical board line.
;;; **Parameters:**
;;;   vertical - ordered vertical line of board tiles.
;;;     A list value passed by value; not modified.
;;;   board - current immutable board value.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed every tile in the supplied vertical line in board orientation.


;;; **Algorithm:** Print the first vertical tile, then recur through the remaining line.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-vertical-tiles (vertical board)
  (cond
    ((null vertical) nil)
    (t (print-vertical-tile (car vertical) board)
       (print-vertical-tiles (cdr vertical) board))))


;;; /***************************************************************************
;;; **Function Name:** print-spaces-to-spinner
;;; **Purpose:** Align output with the spinner position.
;;; **Parameters:**
;;;   board - current immutable board state.
;;;   horizontal - optional horizontal line to scan; defaults to the board horizontal line.
;;;   count - optional number of tiles before the spinner; defaults to its horizontal position.
;;;   start-p - optional flag to print the initial space; defaults to true.
;;; **Return Value:** Printed indentation aligned to the spinner position.


;;; **Algorithm:** Print indentation by scanning the horizontal prefix up to the spinner position.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-spaces-to-spinner
    (board &optional (horizontal (board-horizontal board))
           (count (board-horizontal-spinner-location board))
           (start-p t))
  (cond
    ((and start-p (eql count 0))
     (format t "  "))
    ((and start-p horizontal)
     (format t "  ")
     (print-spaces-to-spinner board horizontal count nil))
    ((or (= count 0) (null horizontal)) nil)
    ((= (caar horizontal) (cadar horizontal))
     (format t "  ")
     (print-spaces-to-spinner board (cdr horizontal) (1- count) nil))
    (t
     (format t "    ")
     (print-spaces-to-spinner board (cdr horizontal) (1- count) nil))))


;;; /***************************************************************************
;;; **Function Name:** print-offset-spaces
;;; **Purpose:** Show spacing for the requested horizontal prefix.
;;; **Parameters:**
;;;   horizontal - ordered horizontal line of board tiles.
;;;     A list value passed by value; not modified.
;;;   count - number of tiles or items to process.
;;;     A value passed by value; not modified.
;;; **Return Value:** Printed indentation for the requested horizontal prefix.


;;; **Algorithm:** Print the spacing required by each tile in the horizontal prefix.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-offset-spaces (horizontal count)
  (cond
    ((= count 0) nil)
    ((null horizontal) nil)
    ((tile-double-p (car horizontal))
     (format t "  ")
     (print-offset-spaces (cdr horizontal) (1- count)))
    (t (format t "    ")
       (print-offset-spaces (cdr horizontal) (1- count)))))


;;; /***************************************************************************
;;; **Function Name:** print-horizontal-board-line
;;; **Purpose:** Show tiles along the horizontal board line.
;;; **Parameters:**
;;;   horizontal - ordered horizontal line of board tiles.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed the horizontal domino line.


;;; **Algorithm:** Print each horizontal tile with its pips in board order.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-horizontal-board-line (horizontal)
  (cond
    ((null horizontal) nil)
    ((= (caar horizontal) (cadar horizontal))
     (format t "~a " (caar horizontal))
     (print-horizontal-board-line (cdr horizontal)))
    (t
     (format t "~a-~a " (caar horizontal) (cadar horizontal))
     (print-horizontal-board-line (cdr horizontal)))))


;;; /***************************************************************************
;;; **Function Name:** print-tile-list
;;; **Purpose:** Display an ordered list of domino tiles without a trailing separator.
;;; **Parameters:**
;;;   tiles - ordered list of domino tiles.
;;;     A list value passed by value; not modified.
;;; **Return Value:** Printed tiles separated by single spaces, followed by a newline.
;;; **Algorithm:**
;;;   1) Print the current tile.
;;;   2) Print one separating space only when another tile follows.
;;;   3) Continue through the list and finish with a newline.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-tile-list (tiles)
  (cond
    ((null tiles)
     (format t "~%"))
    (t
     (format t "~a-~a~a"
             (tile-left-pips (car tiles))
             (tile-right-pips (car tiles))
             (cond ((cdr tiles) " ") (t "")))
     (print-tile-list (cdr tiles)))))


;;; SECTION 9: RECURSIVE LIST, DEAL, AND SHUFFLE HELPERS

;;; /***************************************************************************
;;; **Function Name:** deal-one-hand
;;; **Purpose:** Take a requested number of tiles from the front of a stock.
;;; **Parameters:**
;;;   deck - ordered tiles available for dealing or shuffling.
;;;     A list value passed by value; not modified.
;;;   count - number of tiles or items to process.
;;;     A value passed by value; not modified.
;;;   tiles-accumulator - tiles generated or collected so far.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A two-item list containing the dealt tiles and remaining deck.
;;; **Algorithm:**
;;;   1) Collect up to COUNT tiles from the deck front, then return the hand and remaining deck.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun deal-one-hand (deck count tiles-accumulator)
  (cond
    ((= count 0)
     (list (reverse-list tiles-accumulator)
           deck))
    ((null deck)
     (list (reverse-list tiles-accumulator)
           nil))
    (t
     (deal-one-hand (cdr deck)
                    (1- count)
                    (cons (car deck) tiles-accumulator)))))


;;; /***************************************************************************
;;; **Function Name:** shuffle-deck-acc
;;; **Purpose:** Continue shuffling remaining tiles while collecting selected tiles.
;;; **Parameters:**
;;;   deck - ordered tiles available for dealing or shuffling.
;;;     A list value passed by value; not modified.
;;;   seed - integer state for reproducible random choices.
;;;     A value passed by value; not modified.
;;;   shuffled-accumulator - shuffled tiles selected so far.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A two-item list containing the shuffled tiles and final seed.
;;; **Algorithm:**
;;;   1) Select and remove random tiles until the deck is empty; return them with the final seed.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun shuffle-deck-acc (deck seed shuffled-accumulator)
  (cond
    ((null deck)
     (list (reverse-list shuffled-accumulator)
           seed))
    (t
     (let* ((next-seed (next-random-seed seed))
            (index (mod (1- next-seed) (list-length-rec deck)))
            (selection (remove-at-index deck index nil)))
       (shuffle-deck-acc
         (cadr selection)
         next-seed
         (cons (car selection) shuffled-accumulator))))))


;;; /***************************************************************************
;;; **Function Name:** random-index-and-seed
;;; **Purpose:** Produce a valid random index and the next seed.
;;; **Parameters:**
;;;   limit - number of valid random indices.
;;;     A value passed by value; not modified.
;;;   seed - integer state for reproducible random choices.
;;;     A value passed by value; not modified.
;;; **Return Value:** A two-item list containing a valid index and next seed.
;;; **Algorithm:**
;;;   1) Advance the supplied seed.
;;;   2) Convert the new seed to an index within the limit and return both values.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun random-index-and-seed (limit seed)
  (random-index-from-next-seed limit (next-random-seed seed)))


;;; /***************************************************************************
;;; **Function Name:** random-index-from-next-seed
;;; **Purpose:** Pair the next seed with an index inside the requested limit.
;;; **Parameters:**
;;;   limit - number of valid random indices.
;;;     A value passed by value; not modified.
;;;   next-seed - random seed for the next choice.
;;;     A value passed by value; not modified.
;;; **Return Value:** A two-item list containing a valid index and next seed.
;;; **Algorithm:**
;;;   1) Map the next seed to an index from zero through LIMIT minus one.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun random-index-from-next-seed (limit next-seed)
  (list (mod (1- next-seed) limit)
        next-seed))


;;; /***************************************************************************
;;; **Function Name:** next-random-seed
;;; **Purpose:** Advance the deterministic random sequence by one step.
;;; **Parameters:**
;;;   seed - integer state for reproducible random choices.
;;;     A value passed by value; not modified.
;;; **Return Value:** The next positive seed in the deterministic random sequence.
;;; **Algorithm:**
;;;   1) Multiply the normalized seed and reduce it modulo the generator modulus.
;;;   2) Return the resulting next seed.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun next-random-seed (seed)
  (mod (* +random-seed-multiplier+ (normalize-random-seed seed))
       +random-seed-modulus+))


;;; /***************************************************************************
;;; **Function Name:** normalize-random-seed
;;; **Purpose:** Map a seed into the generator valid nonzero range.
;;; **Parameters:**
;;;   seed - integer state for reproducible random choices.
;;;     A value passed by value; not modified.
;;; **Return Value:** The supplied seed mapped into the generator valid range.
;;; **Algorithm:**
;;;   1) Map the supplied integer to a positive seed no greater than the generator limit.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun normalize-random-seed (seed)
  (1+ (mod (1- seed) +random-seed-limit+)))


;;; /***************************************************************************
;;; **Function Name:** list-length-rec
;;; **Purpose:** Count the items in a list.
;;; **Parameters:**
;;;   items - remaining list items to count; list passed by value, not modified.
;;;   count - optional accumulated item count; defaults to zero.
;;; **Return Value:** The number of items in the supplied list.
;;; **Algorithm:**
;;;   1) Start the count at zero.
;;;   2) Visit each item and return the total count when the list is exhausted.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun list-length-rec (items &optional (count 0))
  (cond
    ((null items) count)
    (t (list-length-rec (cdr items) (1+ count)))))


;;; /***************************************************************************
;;; **Function Name:** list-length-acc
;;; **Purpose:** Count items while carrying the current count.
;;; **Parameters:**
;;;   items - input list of values.
;;;     A list value passed by value; not modified.
;;;   count - number of tiles or items to process.
;;;     A value passed by value; not modified.
;;; **Return Value:** The number of items in the supplied list.
;;; **Algorithm:**
;;;   1) If no items remain, return the current count.
;;;   2) Otherwise add one to the count and continue with the remaining items.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun list-length-acc (items count)
  (cond
    ((null items)
     count)
    (t
     (list-length-acc (cdr items)
                      (1+ count)))))


;;; /***************************************************************************
;;; **Function Name:** remove-at-index
;;; **Purpose:** Select one list item and return the list without it.
;;; **Parameters:**
;;;   items - input list of values.
;;;     A list value passed by value; not modified.
;;;   index - value used in this game operation.
;;;     A value passed by value; not modified.
;;;   prefix-accumulator - saved prefix preceding a selected position.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A two-item list containing the selected item and the list with that item
;;;   removed.
;;; **Algorithm:**
;;;   1) Check whether the input list or requested count has reached its stopping condition.
;;;   2) Process the next item while carrying the partial result forward.
;;;   3) Return the completed list, item, or count.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun remove-at-index (items index prefix-accumulator)
  (cond
    ((null items)
     (list nil (restore-prefix prefix-accumulator nil)))
    ((= index 0)
     (list (car items)
           (restore-prefix prefix-accumulator (cdr items))))
    (t
     (remove-at-index (cdr items)
                      (1- index)
                      (cons (car items) prefix-accumulator)))))


;;; /***************************************************************************
;;; **Function Name:** restore-prefix
;;; **Purpose:** Restore a saved prefix before a list suffix.
;;; **Parameters:**
;;;   reversed-prefix - saved prefix in reverse order.
;;;     A list value passed by value; not modified.
;;;   tail - list suffix after a restored prefix.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The reversed prefix restored before TAIL.
;;; **Algorithm:**
;;;   1) Walk the reversed prefix and cons its items onto TAIL in original order.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun restore-prefix (reversed-prefix tail)
  (cond
    ((null reversed-prefix)
     tail)
    (t
     (restore-prefix (cdr reversed-prefix)
                     (cons (car reversed-prefix) tail)))))


;;; /***************************************************************************
;;; **Function Name:** append-to-end
;;; **Purpose:** Place supplied items after an existing list.
;;; **Parameters:**
;;;   items - input list of values; list passed by value, not modified.
;;;   additions - values to append after ITEMS; list passed by value, not modified.
;;;   reversed-prefix - optional reversed copy of the prefix already visited.
;;; **Return Value:** A new list containing ITEMS followed by ADDITIONS.
;;; **Algorithm:**
;;;   1) Rebuild the existing list, then place ADDITIONS after it.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun append-to-end (items additions &optional reversed-prefix)
  (cond
    ((null items) (restore-prefix reversed-prefix additions))
    (t
     (append-to-end (cdr items) additions
                    (cons (car items) reversed-prefix)))))


;;; /***************************************************************************
;;; **Function Name:** append-to-end-acc
;;; **Purpose:** Rebuild a list prefix before the appended items.
;;; **Parameters:**
;;;   items - input list of values.
;;;     A list value passed by value; not modified.
;;;   additions - values to append after the input list.
;;;     A list value passed by value; not modified.
;;;   reversed-prefix - saved prefix in reverse order.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A new list containing ITEMS followed by ADDITIONS.
;;; **Algorithm:**
;;;   1) Rebuild the saved prefix before ADDITIONS.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun append-to-end-acc (items additions reversed-prefix)
  (cond
    ((null items)
     (restore-prefix reversed-prefix additions))
    (t
     (append-to-end-acc (cdr items)
                        additions
                        (cons (car items) reversed-prefix)))))


;;; /***************************************************************************
;;; **Function Name:** reverse-list
;;; **Purpose:** Return the input list in reverse order.
;;; **Parameters:**
;;;   items - input list of values; list passed by value, not modified.
;;;   result-accumulator - optional reversed prefix accumulated during recursion.
;;; **Return Value:** A new list containing the input items in reverse order.
;;; **Algorithm:**
;;;   1) Accumulate items from the front of the input to produce the reversed list.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun reverse-list (items &optional result-accumulator)
  (cond
    ((null items) result-accumulator)
    (t
     (reverse-list (cdr items)
                   (cons (car items) result-accumulator)))))


;;; /***************************************************************************
;;; **Function Name:** reverse-list-acc
;;; **Purpose:** Reverse a list while collecting items in a result accumulator.
;;; **Parameters:**
;;;   items - input list of values.
;;;     A list value passed by value; not modified.
;;;   result-accumulator - result assembled so far.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The reversed input items prepended to the supplied accumulator.
;;; **Algorithm:**
;;;   1) Cons each input item onto the accumulator until no input remains.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun reverse-list-acc (items result-accumulator)
  (cond
    ((null items)
     result-accumulator)
    (t
     (reverse-list-acc (cdr items)
                       (cons (car items) result-accumulator)))))


;;; SECTION 10: TURN SCORING

;;; /***************************************************************************
;;; **Function Name:** board-scoring-total
;;; **Purpose:** Calculate the scoring total from the exposed board ends after a play.
;;; **Parameters:**
;;;   board - board state after the latest tile was placed.
;;;     A list value passed by value; not modified.
;;;   tile - tile most recently played on the board.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The exposed-end total, including any eligible double bonus.
;;; **Algorithm:**
;;;   1) Add the pip values at the four exposed board ends.
;;;   2) Add one pip value for a non-spinner double when the board has a spinner.
;;;   3) Return the resulting scoring total.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-scoring-total (board tile)
  (add-double-score-bonus
    (+ (board-end board 'left)
       (board-end board 'right)
       (board-end board 'up)
       (board-end board 'down))
    board
    tile))


;;; /***************************************************************************
;;; **Function Name:** add-double-score-bonus
;;; **Purpose:** Include the extra value earned for a non-spinner double on a spinner board.
;;; **Parameters:**
;;;   end-total - sum of the exposed board ends.
;;;     An integer passed by value; not modified.
;;;   board - board state after the latest tile was placed.
;;;     A list value passed by value; not modified.
;;;   tile - tile most recently played on the board.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The end total with the eligible double bonus, if applicable.
;;; **Algorithm:**
;;;   1) Check whether the board has a spinner.
;;;   2) Check whether the latest tile is a double and is not the spinner.
;;;   3) Add one pip value for the double only when both conditions hold.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun add-double-score-bonus (end-total board tile)
  (cond
    ((and (board-has-spinner-p board)
          (tile-double-p tile)
          (not (tile-spinner-p tile)))
     (+ end-total (tile-left-pips tile)))
    (t end-total)))


;;; /***************************************************************************
;;; **Function Name:** board-turn-score
;;; **Purpose:** Determine the points earned by a play under the round target.
;;; **Parameters:**
;;;   board - board state after the latest tile was placed.
;;;     A list value passed by value; not modified.
;;;   tile - tile most recently played on the board.
;;;     A list value passed by value; not modified.
;;; **Return Value:** The scoring total when divisible by target 3 or 5; otherwise zero.
;;; **Algorithm:**
;;;   1) Calculate the exposed-end total, including any eligible double bonus.
;;;   2) Compare its remainder after division by the round target with zero.
;;;   3) Return the total when divisible, or zero when it is not.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-turn-score (board tile)
  (board-turn-score-from-total
    (board-scoring-total board tile)
    (board-target board)))


;;; /***************************************************************************
;;; **Function Name:** board-turn-score-from-total
;;; **Purpose:** Award points only when a scoring total meets the round target rule.
;;; **Parameters:**
;;;   total - exposed-end total, including any eligible double bonus.
;;;     An integer passed by value; not modified.
;;;   target - allowed value of 3 or 5 required to score during the round.
;;;     An integer passed by value; not modified.
;;; **Return Value:** The total when divisible by target; otherwise zero.
;;; **Algorithm:**
;;;   1) Return zero when the target is neither 3 nor 5.
;;;   2) Return the total when division by the target leaves no remainder.
;;;   3) Return zero when the total is not divisible by the target.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun board-turn-score-from-total (total target)
  (cond
    ((not (valid-round-target-p target))
     0)
    ((= (mod total target) 0)
     total)
    (t 0)))


;;; /***************************************************************************
;;; **Function Name:** add-round-points
;;; **Purpose:** Return a player state with earned points added to the current round score.
;;; **Parameters:**
;;;   player-state - player's hand, round score, and tournament score.
;;;     A list value passed by value; not modified.
;;;   points - points earned on the latest play.
;;;     An integer passed by value; not modified.
;;; **Return Value:** A new player state with the updated round score.
;;; **Algorithm:**
;;;   1) Keep the player's hand and tournament score.
;;;   2) Add earned points to the current round score.
;;;   3) Return a new player state containing those values.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun add-round-points (player-state points)
  (list (player-hand player-state)
        (+ (player-round-score player-state) points)
        (player-tournament-score player-state)))


;;; /***************************************************************************
;;; **Function Name:** score-player-turn
;;; **Purpose:** Calculate turn points and return the player's updated state.
;;; **Parameters:**
;;;   player-state - player's hand, round score, and tournament score.
;;;     A list value passed by value; not modified.
;;;   board - board state after the latest tile was placed.
;;;     A list value passed by value; not modified.
;;;   tile - tile most recently played on the board.
;;;     A list value passed by value; not modified.
;;; **Return Value:** A list containing the new player state, points awarded, and scoring total.
;;; **Algorithm:**
;;;   1) Calculate the total from the exposed ends and any eligible double bonus.
;;;   2) Apply the target divisibility rule to determine points earned.
;;;   3) Add those points to the player's round score and return the results.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   Game requirements came from the project C++ files.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun score-player-turn (player-state board tile)
  (let* ((ends (board-ends board))
         (left-pips (tile-left-pips tile))
         (right-pips (tile-right-pips tile))
         (bonus (cond ((and (board-has-spinner-p board)
                            (= left-pips right-pips)
                            (not (tile-spinner-p tile)))
                       left-pips)
                      (t 0)))
         (total (+ (car ends) (cadr ends) (caddr ends) (cadddr ends) bonus))
         (target (board-target board))
         (points (cond ((and (valid-round-target-p target)
                             (= (mod total target) 0))
                        total)
                       (t 0))))
    (list (list (player-hand player-state)
                (+ (player-round-score player-state) points)
                (player-tournament-score player-state))
          points total)))


;;; SECTION 11: LEGAL MOVE GENERATION AND COMPUTER MOVE SELECTION

;;; /***************************************************************************
;;; **Function Name:** get-all-moves
;;; **Purpose:** Find every legal placement available from a player's hand.
;;; **Parameters:**
;;;   board - current immutable board and exposed ends; list passed by value, not modified.
;;;   hand - tiles available to the player; list passed by value, not modified.
;;; **Return Value:** Move records shaped (left-pips right-pips direction-code total points).
;;;   Codes are -1 first tile, 0 left, 1 right, 2 up, and 3 down.
;;; **Algorithm:**
;;;   1) Determine which board directions can be played.
;;;   2) Recursively check each hand tile against each available direction.
;;;   3) Score each legal hypothetical placement and return all move records.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; rules came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun get-all-moves (board hand)
  (scan-all-move-candidates
    hand board (legal-move-directions board) nil nil nil))


;;; /***************************************************************************
;;; **Function Name:** legal-move-directions
;;; **Purpose:** Identify the board ends available for a tile placement.
;;; **Parameters:**
;;;   board - current immutable board and exposed ends; list passed by value, not modified.
;;; **Return Value:** The available directions in first, left, right, up, down order.
;;; **Algorithm:**
;;;   1) Return FIRST for an empty board.
;;;   2) Return all four ends when a spinner is present.
;;;   3) Otherwise return the two horizontal ends.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; rules came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun legal-move-directions (board)
  (cond
    ((null (board-horizontal board)) (list 'first))
    ((board-has-spinner-p board) (list 'left 'right 'up 'down))
    (t (list 'left 'right))))


;;; /***************************************************************************
;;; **Function Name:** scan-all-move-candidates
;;; **Purpose:** Check every tile in a player's hand for legal placements.
;;; **Parameters:**
;;;   hand - tiles remaining to inspect; list passed by value, not modified.
;;;   board - current immutable board; list passed by value, not modified.
;;;   directions - board directions checked for the current tile.
;;;   tile - current tile being evaluated, or NIL before the scan starts.
;;;   directions-left - directions still to check for TILE.
;;;   moves-accumulator - legal move records collected in reverse order.
;;; **Return Value:** Legal move records for the hand, restored to hand and direction order.
;;; **Algorithm:** Check each tile against the available directions, collect legal scored moves,
;;;   and restore their original order when the scan is complete.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; rules came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun scan-all-move-candidates
    (hand board directions tile directions-left moves-accumulator)
  (cond
    ((null directions) (reverse-list moves-accumulator))
    ((null tile)
     (cond
       ((null hand) (reverse-list moves-accumulator))
       (t
        (scan-all-move-candidates
          (cdr hand) board directions (car hand) directions moves-accumulator))))
    ((null directions-left)
     (cond
       ((null hand) (reverse-list moves-accumulator))
       (t
        (scan-all-move-candidates
          (cdr hand) board directions (car hand) directions moves-accumulator))))
    ((move-direction-legal-p board tile (car directions-left))
     (let* ((direction (car directions-left))
            (resulting-board (place-board-tile board tile direction))
            (played-tile (caddr (cddddr resulting-board)))
            (ends (cadddr resulting-board))
            (spinner-state (car (cddddr resulting-board)))
            (left-pips (car played-tile))
            (right-pips (cadr played-tile))
            (bonus
              (cond
                ((and (car spinner-state)
                      (= left-pips right-pips)
                      (not (caddr played-tile)))
                 left-pips)
                (t 0)))
            (total (+ (car ends) (cadr ends) (caddr ends)
                      (cadddr ends) bonus))
            (target (caddr resulting-board))
            (points
              (cond
                ((and (valid-round-target-p target)
                      (= (mod total target) 0))
                 total)
                (t 0)))
            (move (list (tile-left-pips tile)
                        (tile-right-pips tile)
                        (move-direction-code direction)
                        total points)))
       (scan-all-move-candidates
         hand board directions tile (cdr directions-left)
         (cons move moves-accumulator))))
    (t
     (scan-all-move-candidates
       hand board directions tile (cdr directions-left)
       moves-accumulator))))


;;; /***************************************************************************
;;; **Function Name:** move-direction-legal-p
;;; **Purpose:** Determine whether a tile can connect to one selected board end.
;;; **Parameters:**
;;;   board - current immutable board and exposed ends; list passed by value, not modified.
;;;   tile - tile being checked; list passed by value, not modified.
;;;   direction - board end currently being checked; value passed by value, not modified.
;;; **Return Value:** A non-NIL value when the tile can use that end; otherwise NIL.
;;; **Algorithm:**
;;;   1) Read the pip value exposed at the selected end.
;;;   2) Check whether either tile side matches that pip value.
;;;   3) Permit FIRST only when the board is empty and vertical directions only with a spinner.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; rules came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun move-direction-legal-p (board tile direction)
  (let ((horizontal (car board))
        (vertical (cadr board))
        (spinner (car (car (cddddr board)))))
    (cond
      ((eq direction 'first) (null horizontal))
      ((eq direction 'left)
       (and horizontal
            (or (= (car tile) (caar horizontal))
                (= (cadr tile) (caar horizontal)))))
      ((eq direction 'right)
       (and horizontal
            (let ((end-tile (last horizontal)))
              (or (= (car tile) (cadar end-tile))
                  (= (cadr tile) (cadar end-tile))))))
      ((eq direction 'up)
       (and spinner vertical
            (or (= (car tile) (caar vertical))
                (= (cadr tile) (caar vertical)))))
      ((eq direction 'down)
       (and spinner vertical
            (let ((end-tile (last vertical)))
              (or (= (car tile) (cadar end-tile))
                  (= (cadr tile) (cadar end-tile))))))
      (t nil))))


;;; /***************************************************************************
;;; **Function Name:** build-scored-move
;;; **Purpose:** Score a legal placement using the board ends that placement creates.
;;; **Parameters:**
;;;   board - current immutable board and exposed ends; list passed by value, not modified.
;;;   tile - tile selected from the player's hand; list passed by value, not modified.
;;;   direction - board end used for the placement; value passed by value, not modified.
;;; **Return Value:** A five-item move record with tile pips, direction code, total, and points.
;;; **Algorithm:**
;;;   1) Produce the hypothetical board after placing the tile.
;;;   2) Calculate the exposed-end total and the target-based score.
;;;   3) Package those values with the tile pips and direction code.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; rules came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun build-scored-move (board tile direction)
  (package-scored-move
    tile direction
    (place-board-tile board tile direction)))


;;; /***************************************************************************
;;; **Function Name:** package-scored-move
;;; **Purpose:** Package a placement with its resulting total and turn score.
;;; **Parameters:**
;;;   tile - tile selected from the player's hand; list passed by value, not modified.
;;;   direction - board end used for the placement; value passed by value, not modified.
;;;   resulting-board - hypothetical board; list passed by value, not modified.
;;; **Return Value:** (left-pips right-pips direction-code total points).
;;; **Algorithm:**
;;;   1) Read the played tile pips and map the direction to its move-list code.
;;;   2) Calculate total pips and the points earned from the resulting board.
;;;   3) Return the five values as one move record.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; rules came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun package-scored-move (tile direction resulting-board)
  (list (tile-left-pips tile)
        (tile-right-pips tile)
        (move-direction-code direction)
        (board-scoring-total resulting-board (board-last-play resulting-board))
        (board-turn-score resulting-board (board-last-play resulting-board))))


;;; /***************************************************************************
;;; **Function Name:** move-direction-code
;;; **Purpose:** Convert a board direction into the code used by C++ move records.
;;; **Parameters:**
;;;   direction - board direction involved in the move; value passed by value, not modified.
;;; **Return Value:** -1 for first tile, 0 left, 1 right, 2 up, 3 down; NIL if unsupported.
;;; **Algorithm:**
;;;   1) Compare the direction to each supported board end.
;;;   2) Return its move code or NIL when it is unsupported.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; codes came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun move-direction-code (direction)
  (cond
    ((eq direction 'first) +move-code-first+)
    ((eq direction 'left) +move-code-left+)
    ((eq direction 'right) +move-code-right+)
    ((eq direction 'up) +move-code-up+)
    ((eq direction 'down) +move-code-down+)
    (t nil)))


;;; /***************************************************************************
;;; **Function Name:** get-optimal-moves
;;; **Purpose:** Keep the highest-scoring moves, breaking ties by tile pip sum.
;;; **Parameters:**
;;;   moves - move records shaped (left-pips right-pips direction-code total points).
;;;     A list value passed by value; not modified.
;;; **Return Value:** Every move tied for the most points and highest tile pip sum.
;;; **Algorithm:**
;;;   1) Find the highest points value in the move list.
;;;   2) Find the greatest tile pip sum among moves with that points value.
;;;   3) Return every move matching both values, preserving input order.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The selection rules were supplied by the project author.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun get-optimal-moves (moves)
  (cond
    ((null moves) nil)
    (t
     (collect-optimal-moves
       moves
       (maximum-move-points moves)
       (maximum-tile-sum-at-points moves (maximum-move-points moves))
       nil))))


;;; /***************************************************************************
;;; **Function Name:** maximum-move-points
;;; **Purpose:** Find the greatest points value among move records.
;;; **Parameters:**
;;;   moves - nonempty move records shaped (left-pips right-pips direction-code total points).
;;;     A list value passed by value; not modified.
;;; **Return Value:** The greatest points value in the supplied list.
;;; **Algorithm:**
;;;   1) Return the points value when one move remains.
;;;   2) Compare that value with the maximum from the remaining moves.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The move record format comes from this project.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun maximum-move-points (moves)
  (cond
    ((null (cdr moves)) (car (cddddr (car moves))))
    (t
     (max (car (cddddr (car moves)))
          (maximum-move-points (cdr moves))))))


;;; /***************************************************************************
;;; **Function Name:** maximum-tile-sum-at-points
;;; **Purpose:** Find the greatest tile pip sum among moves with the selected points value.
;;; **Parameters:**
;;;   moves - move records shaped (left-pips right-pips direction-code total points).
;;;     A list value passed by value; not modified.
;;;   points - highest points value being considered.
;;;     An integer passed by value; not modified.
;;; **Return Value:** The greatest tile pip sum among matching moves, or zero if none match.
;;; **Algorithm:**
;;;   1) Skip moves whose points differ from the selected value.
;;;   2) Compare matching tile pip sums with the maximum from the remaining moves.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The move record format and selection rule come from this project.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun maximum-tile-sum-at-points (moves points)
  (cond
    ((null moves) 0)
    ((= (car (cddddr (car moves))) points)
     (max (+ (car (car moves)) (cadr (car moves)))
          (maximum-tile-sum-at-points (cdr moves) points)))
    (t
     (maximum-tile-sum-at-points (cdr moves) points))))


;;; /***************************************************************************
;;; **Function Name:** collect-optimal-moves
;;; **Purpose:** Keep every move tied for highest points and tile pip sum.
;;; **Parameters:**
;;;   moves - move records remaining to inspect.
;;;     A list value passed by value; not modified.
;;;   points - greatest points value among all moves.
;;;     An integer passed by value; not modified.
;;;   pip-sum - greatest tile pip sum among moves with the greatest points.
;;;     An integer passed by value; not modified.
;;;   moves-accumulator - matching optimal moves collected in reverse order.
;;;     A list value passed by value; not modified.
;;; **Return Value:** All matching optimal moves, restored to input order.
;;; **Algorithm:**
;;;   1) Keep a move only when both its points and tile pip sum match the maxima.
;;;   2) Continue through the list and restore the collected order at the end.
;;; **Reference:** ChatGPT (OpenAI) assisted with planning, implementation, and review.
;;;   The move record format and selection rule come from this project.
;;; **Construct Source:** For constructs not covered in class, consult the Common Lisp HyperSpec.
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun collect-optimal-moves (moves points pip-sum moves-accumulator)
  (cond
    ((null moves) (reverse-list moves-accumulator))
    ((and (= (car (cddddr (car moves))) points)
          (= (+ (car (car moves)) (cadr (car moves))) pip-sum))
     (collect-optimal-moves
       (cdr moves) points pip-sum (cons (car moves) moves-accumulator)))
    (t
     (collect-optimal-moves (cdr moves) points pip-sum moves-accumulator))))


;;; /***************************************************************************
;;; **Function Name:** print-all-moves
;;; **Purpose:** Display every move record returned by GET-ALL-MOVES.
;;; **Parameters:**
;;;   moves - move records to display; list passed by value, not modified.
;;; **Return Value:** Printed move list; game values remain unchanged.
;;; **Algorithm:**
;;;   1) Report when no legal moves exist.
;;;   2) Otherwise print one move and recursively continue through the list.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; rules came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-all-moves (moves)
  (cond
    ((null moves) (format t "No legal moves.~%"))
    (t (print-all-moves-recursively moves))))


;;; /***************************************************************************
;;; **Function Name:** print-all-moves-recursively
;;; **Purpose:** Print each move in a nonempty move list without an empty-list message.
;;; **Parameters:** moves - remaining move records; list passed by value, not modified.
;;; **Return Value:** Printed move details; game values remain unchanged.
;;; **Algorithm:** Print one move and recurse through the remaining records.
;;; **Reference:** Move records returned by GET-ALL-MOVES.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-all-moves-recursively (moves)
  (cond
    ((null moves) nil)
    (t
     (print-one-move (car moves))
     (print-all-moves-recursively (cdr moves)))))


;;; /***************************************************************************
;;; **Function Name:** move-code-direction-initial
;;; **Purpose:** Convert a move direction code to its matching input letter.
;;; **Parameters:** code - direction code from a move record.
;;; **Return Value:** F, L, R, U, or D; a question mark for an unsupported code.
;;; **Algorithm:** Match the code to the first letter of its board direction.
;;; **Reference:** Direction codes and letters follow Board.cpp and the human turn prompt.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun move-code-direction-initial (code)
  (cond
    ((eql code +move-code-first+) "F")
    ((eql code +move-code-left+) "L")
    ((eql code +move-code-right+) "R")
    ((eql code +move-code-up+) "U")
    ((eql code +move-code-down+) "D")
    (t "?")))


;;; /***************************************************************************
;;; **Function Name:** print-one-move
;;; **Purpose:** Display the tile, direction, total, and points for one legal move.
;;; **Parameters:**
;;;   move - five-item move record; list passed by value, not modified.
;;; **Return Value:** Printed move details; game values remain unchanged.
;;; **Algorithm:** Map the direction code to its letter, then display the move details.
;;; **Reference:** ChatGPT assisted with planning, implementation, and review; codes came from
;;;   Board.cpp.
;;; **Construct Source:** Common Lisp HyperSpec,
;;;   https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm.
;;; ***************************************************************************/
(defun print-one-move (move)
  (format t "  Tile ~a-~a; direction ~a (code ~a); total ~a; points ~a~%"
          (car move) (cadr move)
          (move-code-direction-initial (caddr move)) (caddr move)
          (cadddr move) (car (cddddr move))))

;;; Start the interactive new-game setup when this file is loaded in Allegro.
(eval-when (:load-toplevel :execute)
  (start-game))
