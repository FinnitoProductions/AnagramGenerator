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

(bind ?WORD_REQUEST_MESSAGE "Enter the word of which you would like to see all anagrams: ")
(bind ?INVALID_INPUT_MESSAGE "Your input was invalid. Please try again.")
(bind ?SYSTEM_ALLOWED_CHARACTERS 10)

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
* Requests user input for a given word. If the word is valid (a string with length less than 10 characters and no whitespace), 
* returns the given word; if invalid, returns FALSE.
*/
(deffunction requestValidatedWord ()
   (bind ?returnVal (askline ?WORD_REQUEST_MESSAGE))

   (bind ?isValid (and (stringp ?returnVal) (<= (str-length ?returnVal) ?SYSTEM_ALLOWED_CHARACTERS) (not (hasWhiteSpace ?returnVal))))
   (if (not ?isValid) then (bind ?returnVal FALSE)
   )
   
   (return ?returnVal)
)
/*
* Requests the user for a string and asserts each individual letter in it, with position ordered by location.
* The word must be less than 11 characters long to be valid.
*/
(deffunction askAndAssertString () 
   (bind ?userInput (requestValidatedWord))
   
   (while (eq ?userInput FALSE)
      (printline ?INVALID_INPUT_MESSAGE)
      (bind ?userInput (requestValidatedWord))
   )

   (createAnagramGenerator (str-length ?userInput))
   (return (assertStringChars ?userInput))
)

/*
* Dynamically defines a rule to generate an anagram with a given number of letters not exceeding system capacity.
*/
(deffunction createAnagramGenerator (?numLetters)
   (bind ?pattern "")
   (bind ?action "(printout t")

   (for (bind ?i 1) (<= ?i ?numLetters) (++ ?i)
      (bind ?pattern (str-cat ?pattern "(Letter (c ?l" ?i ") (p ?p" ?i))

      (for (bind ?j (- ?i 1)) (>= ?j 1) (-- ?j)
         (bind ?pattern (str-cat ?pattern " &~?p" ?j))
      )

      (bind ?pattern (str-cat ?pattern ")) "))

      (bind ?action (str-cat ?action " ?l" ?i))
   )

   (bind ?action (str-cat ?action " \" \" crlf)"))

   (build (str-cat "(defrule dynamicAnagramGenerator " ?pattern " => " ?action ")"))
)

(askAndAssertString)