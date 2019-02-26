/*
* Finn Frankis
* February 25, 2019
*
* Generates all anagrams for a given eight-letter word without duplicate letters.
*/

(clear) 
(reset)

(batch util/utilities.clp)

(deftemplate Letter (slot c) (slot p))

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