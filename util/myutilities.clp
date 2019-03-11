/* 
* Finn Frankis
* February 25, 2019
*
* Contains a series of utilities functions with useful applications across multiple JESS programs.
*/

/*
* Determines whether a given parameter ?n (of any acceptable type) is a whole number. 
* If it is not, it will return FALSE; otherwise, it will return the initial number casted to a long.
*/
(deffunction isWholeNumber (?n)
   (bind ?isValidInput (and (numberp ?n) (>= ?n 0) (= (integer ?n) ?n))) ; must be a positive number without fractional part
   (bind ?isLong (longp ?n))

   (if (and ?isValidInput ?isLong) then (bind ?returnVal ?n) 
    elif ?isValidInput then (bind ?returnVal (long ?n))       
    else (bind ?returnVal FALSE)                                         ; if ?n is not a whole number, return false
   )                                                                     

   (return ?returnVal) 
) ; isWholeNumber (?n)

/*
* Returns the representation of a given string as a list of characters, in the same order as provided,
* by first exploding the string into its various words and then iterating over each character in every word.
* Removes all whitespace. Input must be a string for a logical output.
*/
(deffunction slice$ (?str)
   (bind ?list (explode$ ?str))

   (for (bind ?i 1) (<= ?i (length$ ?list)) (++ ?i) 
      (bind ?token (nth$ ?i ?list))
      (bind ?list (replace$ ?list ?i ?i (sliceToken$ ?token)))
   )

   (return ?list)
) ; slice$ (?str)

/*
* Breaks a given non-whitespace token into a list of its constituent characters by iterating over each character
* and adding it to a list.
* Input must be a string without whitespace for a logical output.
*/
(deffunction sliceToken$ (?str)
   (bind ?returnVal (create$))

   (for (bind ?i 1) (<= ?i (str-length ?str)) (++ ?i) ; iterate through every character in the string
      (bind ?returnVal (insert$ ?returnVal ?i (sub-string ?i ?i ?str)))
   )

   (return ?returnVal)
) ; sliceToken$ (?str)

/*
* Determines whether a list contains a given value, based on both value and type.
* Returns TRUE if this value is contained in the list; FALSE otherwise.
*/
(deffunction contains (?list ?val)
   (bind ?returnVal FALSE)

   (foreach ?element ?list
      (bind ?returnVal (or ?returnVal (eq ?element ?val))) ; will become TRUE if the element ever appears
   )

   (return ?returnVal)
) ; contains (?list ?val)

/*
* Determines whether a given string has white space by iterating over each character (including whitespace)
* and determining whether whitespace ever appears.
*/
(deffunction hasWhiteSpace (?string)
   (bind ?returnVal FALSE)

   (for (bind ?i 1) (<= ?i (str-length ?string)) (++ ?i)
      (bind ?currentChar (sub-string ?i ?i ?string))
      (bind ?returnVal (or ?returnVal (eq ?currentChar " "))) ; will become TRUE if a space ever appears in the string
   )
   (return ?returnVal)
) ; hasWhiteSpace (?string)