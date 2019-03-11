/*
* Finn Frankis
* February 25, 2019
*
* Generates and prints all anagrams for a given word with length not exceeding system capacity, using JESS' native rule engine.
*
* Includes functions for asserting a single letter alone, asserting a list of letters individually, asserting a string
* of letters, generating an anagram rule dynamically, and ultimately one to take user input for a word with which to
* generate all amagrans.
*
* Call the generateAnagrams function (no arguments) to begin the UI for entering a word and to see all anagrams of that word printed out.
*/

(clear)

(batch util/utilities.clp)
(batch util/myutilities.clp)

(deftemplate Letter (slot character) (slot position)) ; defines a template for two characteristics of a letter within a word

(defglobal ?*WORD_REQUEST_MESSAGE* = "Enter a Word: ")
(defglobal ?*INVALID_INPUT_MESSAGE* = "Your input was invalid. Please try again.")
(defglobal ?*SYSTEM_ALLOWED_CHARACTERS* = 10) ; the maximum number of characters which the system can handle for anagram generation

/*
* Given a single-character string, ?letter, and that letter's initial position in a given word, ?position, 
* asserts this letter into the fact base using the above-defined Letter template.
*
* Returns TRUE if the assertion was successful; false otherwise.
*/
(deffunction assertLetter (?letter ?position)
   (return (assert (Letter (character ?letter) (position ?position))))
) ; assertLetter (?letter ?position)

/*
* Given a list of single-character strings, asserts each letter in the list individually 
* as though it were part of a word, with its position in that word equal to its list index.
*
* Returns TRUE if every assertion was successful; false otherwise.
*/
(deffunction assertLetterList (?list)
   (bind ?wasSuccessful TRUE) ; represents if all assertions were successful; will be FALSE if one or more letters within the list fail

   (for (bind ?i 1) (<= ?i (length$ ?list)) (++ ?i)
      (bind ?listVal (nth$ ?i ?list))
      (bind ?wasSuccessful (and ?wasSuccessful (assertLetter ?listVal ?i))) ; asserts the letter in the list; makes ?wasSuccessful FALSE if any assertion failed
   )

   (return ?wasSuccessful)
) ; assertLetterList (?list)

/*
* Given a string, breaks it up into its constituent characters and asserts each letter
* individually, defining the position of each letter by its index in the string (starting at index 1).
* 
* Returns TRUE if every assertion was successful; false otherwise.
*/
(deffunction assertStringChars (?str)
   (return (assertLetterList (slice$ ?str))) ; calls the slice$ function in myutilities.clp 
) ; assertStringChars (?str)

/*
* Requests user input for a given word. If the word is valid (a string with length not exceeding system capacity
* and without whitespace), returns the given word; if invalid, returns FALSE.
*/
(deffunction requestValidatedWord ()
   (bind ?returnVal (askline ?*WORD_REQUEST_MESSAGE*)) ; asks the user for input which will be used to generate anagrams 

   (bind ?isString (stringp ?returnVal))
   (bind ?isValidLength (<= (str-length ?returnVal) ?*SYSTEM_ALLOWED_CHARACTERS*))
   (bind ?hasNoWhiteSpace (not (hasWhiteSpace ?returnVal)))
   (bind ?isValid (and ?isString ?isValidLength ?hasNoWhiteSpace))

   (if (not ?isValid) then (bind ?returnVal FALSE)) ; returns FALSE if the input was invalid
   
   (return ?returnVal)
) ; requestValidatedWord ()

/*
* Dynamically defines a rule to generate an anagram with a given number of letters not exceeding system capacity.
* 
* Uses a single loop construct with number of iterations equal to the number of letters in the word. Defines the pattern
* by adding a new Letter template to the pattern with every iteration of this loop (because the number of letters used to 
* generate the anagram is equal to the total number of letters in the word). Each successive letter pattern guarantees that 
* its letter cannot have the same position as any of the previous letters ones to account for words with multiple of the same 
* letter; accomplishes this by iterating backward starting from one less than the current position index down to the first
* position (index 1) to prevent all previously used position indices from being reused. Generates the action to be performed by the rule by 
* concatenating a new letter variable value to the printout statement with each iteration, along with a space to separate them.
* 
* Does not run the rule engine nor assert any facts; simply defines the rule.
* 
* For example, a five-letter anagram generator rule would appear as follows:
* (defrule dynamicAnagramGenerator 
*    (Letter (character ?l1) (position ?p1)) 
*    (Letter (character ?l2) (position ?p2 &~?p1)) 
*    (Letter (character ?l3) (position ?p3 &~?p2 &~?p1)) 
*    (Letter (character ?l4) (position ?p4 &~?p3 &~?p2 &~?p1)) 
*    (Letter (character ?l5) (position ?p5 &~?p4 &~?p3 &~?p2 &~?p1))  
*    => 
*    (printout t ?l1 ?l2 ?l3 ?l4 ?l5 " " crlf)
* )
*/
(deffunction createAnagramGenerator (?numLetters)
   (bind ?pattern "")           ; the pattern which will be used to trigger the anagram rule 
   (bind ?action "(printout t") ; the action to be executed if the anagram rule is triggered 

   (for (bind ?i 1) (<= ?i ?numLetters) (++ ?i)
      (bind ?nextPattern (str-cat "(Letter (character ?l" ?i ") (position ?p" ?i))
      (bind ?pattern (str-cat ?pattern ?nextPattern))  ; defines each letter in the pattern by its value and position in the word

      (for (bind ?j (- ?i 1)) (>= ?j 1) (-- ?j)
         (bind ?pattern (str-cat ?pattern " &~?p" ?j)) ; ensures that each letter cannot have the same position as any of the previous ones
      )

      (bind ?pattern (str-cat ?pattern ")) "))

      (bind ?action (str-cat ?action " ?l" ?i)) ; defines the action to print out each letter represented by its value, ?l
   )

   (bind ?action (str-cat ?action " \" \" crlf)"))

   (bind ?rule (str-cat "(defrule dynamicAnagramGenerator " ?pattern " => " ?action ")")) ; creates the rule-defining code
 
   (build ?rule)                                                                          ; defines the rule by executing the JESS code stored in ?rule
   (return)
) ; createAnagramGenerator (?numLetters)

/*
* Generates all possible anagrams (with duplicates) of a given word, as specified by the user.
*
* Requests the user for a string and asserts each individual letter in it, with position ordered by location.
* The word must be within the system capacity *SYSTEM_ALLOWED_CHARACTERS as defined at the top of the file.
* For a logical output, the input must be a string of alphabetic characters.
*
* Returns the number of anagrams which were generated.
*/
(deffunction generateAnagrams () 
   (reset)
   (bind ?userInput (requestValidatedWord)) ; will equal FALSE if the input was invalid and the requested word if it was valid
   
   (while (eq ?userInput FALSE)             ; continually asks the user for input until it is valid
      (printline ?*INVALID_INPUT_MESSAGE*)
      (bind ?userInput (requestValidatedWord))
   )

   (createAnagramGenerator (str-length ?userInput)) ; generates the anagram rule
   (assertStringChars ?userInput)                   ; asserts each character in the word as a fact within the rule engine

   (return (run))
) ; generateAnagrams ()

(generateAnagrams)