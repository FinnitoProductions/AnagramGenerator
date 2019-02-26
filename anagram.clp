/*
* Finn Frankis
* February 25, 2019
*
* Generates all anagrams for a given eight-letter word without duplicate letters.
*/

(clear) 
(reset)

(batch util/utilities.clp)
(batch util/myutilities.clp)

(deftemplate Letter (slot c) (slot p))

(bind ?WORD_REQUEST_MESSAGE "Enter the 8-letter word of which you would like to see all anagrams: ")
(bind ?INVALID_INPUT_MESSAGE "Your input was invalid. Please try again.")
(bind ?TOTAL_DESIRED_CHARACTERS 8)

/* 
* Generates all possible anagrams of a given set of asserted letters.
*/
(defrule anagramGenerator "Generate an anagram of a set of letters without duplicates"
   (Letter (c ?l1)  (p ?p1)) 
   (Letter (c ?l2)  (p ?p2 &~?p1)) 
   (Letter (c ?l3)  (p ?p3 &~?p2 &~?p1)) 
   (Letter (c ?l4)  (p ?p4 &~?p3 &~?p2 &~?p1)) 
   (Letter (c ?l5)  (p ?p5 &~?p4 &~?p3 &~?p2 &~?p1))
   (Letter (c ?l6)  (p ?p6 &~?p5 &~?p4 &~?p3 &~?p2 &~?p1))
   (Letter (c ?l7)  (p ?p7 &~?p6 &~?p5 &~?p4 &~?p3 &~?p2 &~?p1))
   (Letter (c ?l8)  (p ?p8 &~?p7 &~?p6 &~?p5 &~?p4 &~?p3 &~?p2 &~?p1))
   ;(Letter (c ?l9)  (p ?p9 &~?p8 &~?p7 &~?p6 &~?p5 &~?p4 &~?p3 &~?p2 &~?p1))
   ;(Letter (c ?l10) (p ?p10 &~?p9 &~?p8 &~?p7 &~?p6 &~?p5 &~?p4 &~?p3 &~?p2 &~?p1))
   =>
   (printout t ?l1 ?l2 ?l3 ?l4 ?l5 ?l6 ?l7 ?l8  " ")
)


/*
* Given a string ?letter and its initial position in the word ?position, asserts this letter using the above-defined Letter 
* template.
*/
(deffunction assertLetter (?letter ?position)
   (return (assert (Letter (c ?letter) (p ?position))))
)

/*
* Given a list, asserts each letter individually in the list with its position equal to its list index.
* Returns TRUE if every assertion was successful; false otherwise.
*/
(deffunction assertLetterList (?list)
   (bind ?wasSuccessful TRUE)

   (for (bind ?i 1) (<= ?i (length$ ?list)) (++ ?i)
      (bind ?listVal (nth$ ?i ?list))
      (bind ?wasSuccessful (and ?wasSuccessful (assertLetter ?listVal ?i)))
   )

   (return ?wasSuccessful)
)

/*
* Given a string, breaks it up into its constituent characters and asserts each letter
* individually with each one's position ordered by their location in the string.
* Returns TRUE if every assertion was successful; false otherwise.
*/
(deffunction assertStringChars (?str)
   (return (assertLetterList (slice$ ?str)))
)

/*
* Requests user input for a given word. If the word is valid (a string with length 8 characters and no whitespace), 
* returns the given word; if invalid, returns FALSE.
*/
(deffunction requestValidatedWord ()
   (bind ?returnVal (askline ?WORD_REQUEST_MESSAGE))

   (bind ?isValid (and (stringp ?returnVal) (= (str-length ?returnVal) ?TOTAL_DESIRED_CHARACTERS) (not (hasWhiteSpace ?returnVal))))
   (if (not ?isValid) then (bind ?returnVal FALSE)
   )
   
   (return ?returnVal)
)
/*
* Requests the user for a string and asserts each individual letter in it, with position ordered by location.
* The word must be 8 characters long to be valid.
*/
(deffunction askAndAssertString () 
   (bind ?userInput (requestValidatedWord))
   
   (while (eq ?userInput FALSE)
      (printline ?INVALID_INPUT_MESSAGE)
      (bind ?userInput (requestValidatedWord))
   )

   (return (assertStringChars ?userInput))
)

(askAndAssertString)