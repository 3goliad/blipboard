#lang racket

(struct entity (name external-id))
(struct org entity (depts))
(struct person entity (secondary-name tertiary-name employment))
(struct employment (employer
                    department
                    sub-department
                    start-date
                    end-date))

(define (print-header)
  (display (string-append
            "First Name, Middle Name, Last Name, "
            "TESTDATA ID, Employer, Department, "
            "Subdepartment, Start Date, End Date\n")))

(define (print-worker worker)
  (display (string-join
            (list*
             (entity-name worker)
             (person-secondary-name worker)
             (person-tertiary-name worker)
             (entity-external-id worker)
             (let ((emp (person-employment worker)))
               (if (void? emp)
                   '("" "" "" "" "")
                   '((employment-employer
                      (person-employment worker))
                     (employment-department
                      (person-employment worker))
                     (employment-sub-department
                      (person-employment worker))
                     (employment-start-date
                      (person-employment worker))
                     (employment-end-date
                      (person-employment worker))))))
            ","
            #:after-last "\n")))

(define (gen-uuid)
  (call-with-input-file "/proc/sys/kernel/random/uuid"
    (lambda (in) (read-line in))))

(define (gen-worker)
  (let ((uuid (gen-uuid)))
    (person (string-append "Worker " uuid)
            uuid
            "middlename"
            "lastname"
            (void))))

(with-output-to-file "out.csv"
  (lambda ()
    (begin (print-header)
           (for ([i 10])
             (print-worker (gen-worker))))))
