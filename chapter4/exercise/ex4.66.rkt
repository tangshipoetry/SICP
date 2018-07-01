#lang racket


;网上的

Ben has realised that query results can contain duplicates – as exemplified by the wheel rule. To find the sum of the salaries of every wheel:

(run-query '(sum ?amount 
                   (and (wheel ?who)
                        (salary ?who ?amount))))


(and (wheel ?who)
     (salary ?who ?amount))





(and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000))
(and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000))
(and (wheel (Bitdiddle Ben)) (salary (Bitdiddle Ben) 60000))
(and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000))
(and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000))

(sum ?amount) => 660000
To overcome this, some method of checking for unique values is needed,
perhaps storing the intermediate results in a hash table or simple association list,
keyed by sub-query and associated with the current frame.
This takes account of the rules of unification – for example the parallel evaluation of or and serial nature of and –
because the handling of frames already explicitly captures those rules.


;网上的

 If Ben want to get to sum of salary of wheels, he can you this query: 
 (and (wheel ?who) (salary ?who ?amount)) 
 and gets: 
 (and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000)) 
 (and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000)) 
 (and (wheel (Bitdiddle Ben)) (salary (Bitdiddle Ben) 60000)) 
 (and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000)) 
 (and (wheel (Warbucks Oliver)) (salary (Warbucks Oliver) 150000)) 
 you can see that Warbucks Oliver's salary occurs four times,
so in the sum, Warbucks Oliver's salary will be duplicated. Ben can use an unique function to filter the duplication in the amount.























