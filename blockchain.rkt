#lang racket
(require "./block.rkt")
(require "./transaction.rkt")
(require "./wallet.rkt")
(require racket/serialize)
(provide (all-from-out "./block.rkt")
         (all-from-out "./transaction.rkt")
         (all-from-out "./wallet.rkt")
         (struct-out blockchain)
         init-blockchain send-money-blockchain
         balance-wallet-blockchain valid-blockchain?)
(provide (all-defined-out))

(define (true-for-all? predicate list)
  (cond
    [(empty? list) #t]
    [(predicate (car list)) (true-for-all? predicate (cdr list))]
    [else #f]))
;; A simple inline test for true-for-all?
(define test (if (true-for-all? (λ (x) (even? x))
                   (list 2 4 6)) #t
                                 (error "[-] Error: true-for-all? failed to validate correctly")))
#|
"open-output-file returns an object in memory, which we can then write to using write.
 When we do that, it will write to the opened file. close-output-port closes this object in memory.
 This procedure will serialize a struct and then will write the serialized contents to a file."
|#
(define (struct->file object file)
  (let ([out (open-output-file file #:exists 'replace)])
        (write (serialize object) out)
        (close-output-port out)))

(define (file->struct file)
  (let* ([in (open-input-file file)]
           [result (read in)])
    (close-input-port in)
    (deserialize result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct blockchain
  (blocks utxo) #:prefab)

(define (init-blockchain t seed-hash utxo)
  (blockchain (cons (mine-block (process-transaction t) seed-hash) '()) utxo))
;;The block reward starts at 50 coins for the first block and halves on every 210000 blocks -> Bitcoin
(define (reward-for-mining blocks)
  (/ 50 (expt 2 (floor (/ (length blocks) 210000)))))

(define (add-transaction-to-blockchain b t)
   (letrec ([hashed-blockchain
             (mine-block t
             (block-current-hash (car (blockchain-blocks b))))]
            [processed-inputs (transaction-inputs t)]
            [processed-outputs (transaction-outputs t)]
            [utxo (set-union processed-outputs
                             (set-subtract (blockchain-utxo b)
                                           processed-inputs))]
            [new-blocks (cons hashed-blockchain (blockchain-blocks b))]
            [utxo-rewarded (cons
                            (make-transaction-io
                             (reward-for-mining new-blocks)
                             (transaction-from t)) utxo)])
          (blockchain new-blocks utxo-rewarded)))

(define (balance-wallet-blockchain  b  w)
  (letrec ([utxo (blockchain-utxo b)]
           [my-ts (filter
                   (λ (t) (equal? w (transaction-io-owner t)))
                   utxo)])
   (foldr + 0 (map (λ (t) (transaction-io-value t)) my-ts))))


(define (send-money-blockchain b from to value)
  (letrec ([my-ts
            (filter (λ (t) (equal? from (transaction-io-owner t)))
                    (blockchain-utxo b))]
            [t (make-transaction from to value my-ts)])
    (if (transaction? t)
        (let ([processed-transaction (process-transaction t)])
          (if (and
               (>= (balance-wallet-blockchain b from) value)
               (valid-transaction? processed-transaction))
             (add-transaction-to-blockchain b processed-transaction)
             b))
       (add-transaction-to-blockchain b '()))))

(define (valid-blockchain? b)
  (let ([blocks (blockchain-blocks b)])
    (and
     (true-for-all? valid-block? blocks)
     (equal? (drop-right (map block-previous-hash blocks) 1)
             (cdr (map block-current-hash blocks)))
     (true-for-all? valid-transaction? (map
                         (λ (block) (block-data block)) blocks))
     (true-for-all? mined-block? (map block-current-hash blocks)))))
