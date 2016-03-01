#lang racket
;Brandon Wright

(define-struct film (title genre rating runTime month day year revenue))

;Create a list that contains 10 movies
(define movies
  (list
   (make-film "Alien" "Thriller" 'R 120 5 25 1979 83)
   (make-film "The Lion King" "Drama" 'G 99 6 24 1994 422)
   (make-film "The Patriot" "Action" 'R 185 6 28 2000 113)
   (make-film "Black Hawk Down" "Action" 'R 144 1 18 2001 108)
   (make-film "The Breakfast Club" "Comedy" 'R 97 2 15 1985 38)
   (make-film "Deadpool" "Action" 'R 108 2 12 2016 135)
   (make-film "Titanic" "Drama" 'PG-13 194 12 19 1997 658)
   (make-film "Fever Pitch" "Comedy" 'PG-13 104 4 8 2005 42)
   (make-film "Inside Out" "Comedy" 'PG 95 6 19 2015 356)
   (make-film "UP" "Comedy" 'PG 96 5 29 2009 293)))

#|Write a program count-suitable-for-children which consumes a film list and shows the
number of movies that are suitable for children. The suitable movies for children have the
ratings of G, PG, or PG-13.|#
(define (count-suitable-for-children aList)
  (cond
    [(empty? aList) 0]
    [else
     (if (equal? (or
           (equal? (film-rating (first aList)) 'R)
           (equal? (film-rating (first aList)) 'NC-17)
           (equal? (film-rating (first aList)) 'NR)
           ) #t)
        (count-suitable-for-children (rest aList))
        (+ (count-suitable-for-children (rest aList)) 1)
        )]))

(count-suitable-for-children movies)

#|Write a program movies-suitable-for-children which consumes a film list and returns the
list of movies that are suitable for children. The suitable movies for children have the ratings of
G, PG, or PG-13.|#
(define (movies-suitable-for-children aList)
  (cond
    [(empty? aList) '()]
    [else
     (if (equal? (or
           (equal? (film-rating (first aList)) 'R)
           (equal? (film-rating (first aList)) 'NC-17)
           (equal? (film-rating (first aList)) 'NR)
           ) #t)
        (movies-suitable-for-children (rest aList))
        (cons (film-title (first aList)) (movies-suitable-for-children (rest aList)))
        )]))

(movies-suitable-for-children movies)

#|Write a program sum-movie-revenue which consumes a film list and returns total box
office receipts of the movies list.|#
(define (sum-movie-revenue aList)
  (cond
    [(empty? aList) 0]
    [else
     (+ (film-revenue (first aList)) (sum-movie-revenue (rest aList)))]))

(sum-movie-revenue movies)

#|Write a program max-movie-revenue which consumes a film list and returns maximum box
office receipts of the movie list.|#
(define (max-movie-revenue aList)
  (cond
    [(empty? (rest aList)) (first aList)]
    [else
     (if (> (film-revenue (first aList)) (film-revenue (max-movie-revenue (rest aList))))
         (first aList)
         (max-movie-revenue (rest aList)))]))

(film-revenue (max-movie-revenue movies))

#|Write a program opens-before, which consumes a list of movies and a date, and produces a
list of movies open before the given date.|#
(define (opens-before aList month day year)
  (cond
    [(empty? aList) '()]
    [else
     (if (< (film-year (first aList)) year)
     (cons (film-title (first aList)) (opens-before (rest aList) month day year))
     (if (= (film-year (first aList)) year)
         (if (< (film-month (first aList)) month)
             (cons (film-title (first aList)) (opens-before (rest aList) month day year))
             (if (< (film-day (first aList)) day)
                 (cons (film-title (first aList)) (opens-before (rest aList) month day year))
                 (opens-before (rest aList) month day year)
             )
          )
      (opens-before (rest aList) month day year)
      ))]))

(opens-before movies 1 1 2015)