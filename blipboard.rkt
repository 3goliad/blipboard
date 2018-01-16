#lang racket

(struct entity (name external-id))
(struct org entity (depts))
(struct person entity (secondary-name tertiary-name employment))
(struct employment (employer start-date end-date))
