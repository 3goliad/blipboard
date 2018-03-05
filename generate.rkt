#lang racket

(require gregor)
(require gregor/period)

(provide create-simulation
         run-simulation
         show-simulation)

(define (create-simulation #:names names)
  (hash "date" (today)
        "generation" 1))

(define (run-simulation sim)
  (let ((current-date (hash-ref sim "date"))
        (current-generation (hash-ref sim "generation")))
    (hash-set* sim
               "date" (+days current-date 1)
               "generation" (+ current-generation 1))))

(define (show-simulation sim)
  (newline)
  (display "Generation ")
  (display (hash-ref sim "generation"))
  (newline)
  (display "The current date is: ")
  (display (hash-ref sim "date"))
  (newline))

(struct entity (name external-id))
(struct person entity (secondary-name tertiary-name employment))
(struct employment (employer
                    department
                    sub-department
                    start-date
                    end-date) #:transparent)
(struct range (start end) #:transparent)

(define org-employments '())

(define empty-employment (employment "" "" "" "" ""))

(define gen-size (make-parameter "small"))

(define (cond-size #:sm s #:md m #:lg l)
  (case (gen-size) [("small") s] [("medium") m] [("large") l]))

(define (gen-population)
  (cond-size #:sm 1000
             #:md 10000
             #:lg 100000))

(define (gen-org-ids)
  (let ((branches (cond-size #:sm '(1 3 2)
                             #:md '(4 6 3)
                             #:flat '(1 1 98)
                             #:lg '(8 31 3)))
        (into-id (λ id-el (list (map (λ (x) (number->string (+ 1 x))) id-el)))))
    (for*/fold ([acc '()]) ([i (car branches)]
                            [j (cadr branches)]
                            [k (caddr branches)])
      (append
       acc
       (if (zero? j) (into-id i) '())
       (if (zero? k) (into-id i j) '())
       (into-id i j k)))))

(define (dept-name org-id)
  (let ((partial-name (for/list ([id org-id]
                                 [i 3])
                        (string-append
                         (gen-size)
                         " TestCo "
                         (case i [(0) "National "] [(1) "Regional "] [(2) "Local "])
                         id))))
    (append partial-name (build-list (- 3 (length partial-name))
                                     (const "")))))

(define (gen-jobs)
  (make-hash (map (λ (org)
                    (cons (dept-name org) 0))
                  (gen-org-ids))))

(define (take-unemployed pop)
  (let ((n (/ pop 5)))
    (for/list ([i n])
      (gen-worker empty-employment))))

(define (take-employed pop)
  (let ((n (/ pop 5)))
    (append
     (for/list ([i n])
       (gen-worker (gen-employment #f #f)))
     (for/list ([i n])
       (gen-worker (gen-employment #t #f))))))

(define (fire-worker worker)
  (let* ((name (entity-name worker))
         (id (entity-external-id worker))
         (n2 (person-secondary-name worker))
         (n3 (person-tertiary-name worker))
         (emp (person-employment worker))
         (sd (case (employment-start-date emp)
               [("") start-date]
               [else (iso8601->date (employment-start-date emp))])))
    (person name id n2 n3 (employment (employment-employer emp)
                                      (employment-department emp)
                                      (employment-sub-department emp)
                                      (employment-start-date emp)
                                      (~t  (+days sd (random (period-ref (date-period-between sd (today) '(days)) 'days)))
                                           "y-MM-dd")))))

(define (run-gen-stages proc)
  (set! org-employments (gen-jobs))
  (let* ((pop (gen-population))
         (to-be-fired (take-employed pop))
         (first-stage (append (take-unemployed pop)
                              (take-employed pop)
                              to-be-fired))
         (second-stage (map fire-worker to-be-fired)))
    (proc (string-append (gen-size) "-1") (print-header) (map print-worker first-stage))
    (proc (string-append (gen-size) "-2") (print-header) (map print-worker second-stage))))

(define (gen-uuid)
  (call-with-input-file "/proc/sys/kernel/random/uuid"
    (lambda (in) (read-line in))))

(define (gen-worker employment)
  (let ((uuid (gen-uuid)))
    (person (string-append "Worker " uuid)
            uuid
            "middlename"
            "lastname"
            employment)))

(define start-date
  (-days (today) 110))

(define (gen-employment-term)
  (let* ((hire-date (+days start-date (random 90)))
         (days-between (period-ref (date-period-between hire-date (today) '(days)) 'days))
         (fire-date (+days hire-date (random days-between))))
    (range (~t hire-date "y-MM-dd") (~t fire-date "y-MM-dd"))))

(define (next-org)
  (let* ((pairs (hash->list org-employments))
         (next (list-ref pairs (random (length pairs)))))
    (begin
      (hash-set! org-employments (car next) (+ 1 (cdr next)))
      (car next))))

(define (gen-employment hired fired)
  (let* ((org (next-org))
         (term (gen-employment-term)))
    (employment (first org)
                (second org)
                (third org)
                (if hired
                    (range-start term)
                    "")
                (if fired
                    (range-end term)
                    ""))))

(define (print-header)
  (string-append
   "First Name, Middle Name, Last Name, "
   "TESTDATA ID, Employer, Department, "
   "Subdepartment, Start Date, Date Ended\n"))

(define (print-worker worker)
  (string-join
   (list
    (entity-name worker)
    (person-secondary-name worker)
    (person-tertiary-name worker)
    (entity-external-id worker)
    (employment-employer
     (person-employment worker))
    (employment-department
     (person-employment worker))
    (employment-sub-department
     (person-employment worker))
    (employment-start-date
     (person-employment worker))
    (employment-end-date
     (person-employment worker)))
   ","
   #:after-last "\n"))
