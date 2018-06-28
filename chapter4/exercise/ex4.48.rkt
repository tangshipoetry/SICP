#lang racket

;网上的

#|
First, let's implement adjectives.
 Unlike noun-phrases or verb-phrases,
 there isn't the possibility of any fancy recursive structures.
 For example, in the phrase "a delicious pig in a blanket",
 there is no difference in meaning between "(a delicious pig) in a blanket" or "a delicious (pig in a blanket)".
This is because the phrase "in a blanket" effectively *acts* as an adjective,
so it really doesn't matter the order in which we "attach" adjectives to the noun "pig".

Hence, we can assume that adjectives only affect simple nouns:
|#


(define adjectives '(adjective big small green round)) 
  
;; replaces definition from book 
(define (parse-simple-noun-phrase) 
  (let ((art (parse-word articles)) 
        (tag 'simple-noun-phrase)) 
    (amb (list tag art (parse-word adjectives) (parse-word nouns)) 
         (list tag art (parse-word nouns))))) 


#|
We can make the same assumption for adverbs,
as long as we assume adverbs always *precede* verbs,
and don't allow fancy sentences like "the boy sees through a glass, darkly".
|#

(define adverbs '(adverb quickly slowly lazily eagerly)) 
  
;; new function, not in book 
(define (parse-simple-verb) 
  (amb (list 'simple-verb 
             (parse-word adverbs) 
             (parse-word verbs)) 
       (parse-word verbs))) 
  
;; replaces definition from book 
;; only difference is the call to parse-simple-verb 
(define (parse-verb-phrase) 
  (define (maybe-extend verb-phrase) 
    (amb verb-phrase 
         (maybe-extend (list 'verb-phrase 
                             verb-phrase 
                             (parse-prepositional-phrase))))) 
  (maybe-extend (parse-simple-verb))) 



#|
Compound sentences such as "the man eats the dinner when the sun sets",
 on the other hand, *are* recursive structures,
and just so happen to have the same recursiveness as verb-phrases and noun-phrases.
Suppose A, B, and C are non-compound sentences, and *unparsed* contains them in that order,
separated by conjunctions like "while", "and", "or", "before", etc.
 Then parsing successfully would require outputting all possible "parenthesizations": A(BC) and (AB)C.
Inputs with larger numbers of non-compound sentences separated by conjunctions,
 like ABCDEF..., have even more possible parenthesizations.
 The structure of this problem is similar to that of parenthesizations of matrix multiplications, as discussed in one of the early chapters of CLRS.

Fortunately, the maybe-extend functions from the book makes enumerating all possible parenthesizations of compound sentences very easy and elegant.
|#

(define conjunctions '(conjunction while when but and or)) 
  
;; new function not included in book, but this is just a slight modification of the original parse-sentence 
(define (parse-simple-sentence) 
  (list 'simple-sentence 
        (parse-noun-phrase) 
        (parse-verb-phrase))) 
  
;; replaces definition from book 
(define (parse-sentence) 
  (define (maybe-extend sentence) 
    (amb sentence 
         (maybe-extend (list 'compound-sentence 
                             sentence 
                             (parse-word conjunctions) 
                             (parse-sentence))))) 
  (maybe-extend (parse-simple-sentence))) 


#|
Note the reason that maybe-extend can be copied word-for-word without any modification
is that the structure of compound-sentences "linked" by conjunctions is exactly the same as noun-phrases linked by prepositions.
|#





























