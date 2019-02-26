/*
* Finn Frankis
* February 25, 2019
*
* 
*/

(clear) 
(reset)

(batch util/utilities.clp)

(deftemplate Letter (slot c) (slot p))

(assert (Letter (c A) (p 1)))
(assert (Letter (c B) (p 2)))
(assert (Letter (c C) (p 3)))
(assert (Letter (c D) (p 4)))
(assert (Letter (c E) (p 5)))
(assert (Letter (c F) (p 6)))
(assert (Letter (c G) (p 7)))
(assert (Letter (c H) (p 8)))
;(assert (Letter (c I) (p 9)))
;(assert (Letter (c J) (p 10)))

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