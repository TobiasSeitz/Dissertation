# Notes

- the original data was encoded like this 
(1) strongly agree
(2) agree
(3) neither nor
(4) disagree
(5)	strongly disagree

- The original key is:
E1-	A1-	C1	N1	O1
E2 	A2	C2-	N2-	O2
E3-	A3-	C3	N3	O3
E4	A4-	C4	N4	O4
				O5-
Aline re-coded the reversed items correctly.


- then you reverse the scores. 

- Aline made an error when she reversed the scale from 1-5 to 5-1 (which is necessary if you want to 
create plots where the x axis grows). Apparently she created the sum of the items in each sub-scale, and 
then subtracted it from 24. This works perfectly if there are _four_  items because 4\*5=20, so a person
who scored 4 on extraversion would have have 24-4=20 afterwards, because that would be the maximum. 
However, this fails if the sub-scale consists of _five_ items. This explains the wrong scores for
openness.

- what's more, she did not put the right items into the right formula to calculate the scores. 
Translation: O to E, C to A, E to C, A to N, N to O.