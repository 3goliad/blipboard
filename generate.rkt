#lang racket

(provide generate-organizations)
(provide simulation-size)
(provide print-header)
(provide print-worker)
(provide gen-worker)
(provide empty-employment)
(provide gen-employment)

(require gregor)
(require gregor/period)

(define simulation-size (make-parameter "small"))

(struct entity (name external-id))
(struct person entity (secondary-name tertiary-name employment))
(struct employment (employer
                    department
                    sub-department
                    start-date
                    end-date) #:transparent)
(struct range (start end) #:transparent)

(define empty-employment (employment "" "" "" "" ""))

(define org-employments '())

(define (generate-organizations)
  (let* ((make-dept (λ (level num-id)
                     (string-append (simulation-size)
                                    " TestCo "
                                    (case level
                                      [(1) "National "]
                                      [(2) "Regional "]
                                      [(3) "Local "])
                                    (number->string num-id))))
        (make-org (case-lambda
                    [(a)
                     (cons (list (make-dept 1 a) "" "") 0)]
                    [(a b)
                     (cons (list (make-dept 1 a) (make-dept 2 b) "") 0)]
                    [(a b c)
                     (cons (list (make-dept 1 a) (make-dept 2 b) (make-dept 3 c)) 0)]))
        (branches (case (simulation-size)
                            [("small") '(2 5)]
                            [("medium") '(5 5 4)]
                            [("large") '(5 50 4)])))
    (set! org-employments
      (make-hash
       (if (equal? "small" (simulation-size))
           (for*/list ([i (first branches)]
                       [j (second branches)])
             (if (zero? j)
                 (make-org i)
                 (make-org i j)))
           (for*/list ([i (first branches)]
                       [j (second branches)]
                       [k (third branches)])
             (cond
              [(and (zero? j)
                    (zero? k)) (make-org i)]
              [(and (zero? j)
                    (= 1 k)) (make-org i j)]
              [(zero? k) (make-org i j)]
              [else (make-org i j k)])))))))

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
    (range (~t hire-date "y-M-d") (~t fire-date "y-M-d"))))

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
  (display (string-append
            "First Name, Middle Name, Last Name, "
            "TESTDATA ID, Employer, Department, "
            "Subdepartment, Start Date, Date Ended\n")))

(define (print-worker worker)
  (display (string-join
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
            #:after-last "\n")))
