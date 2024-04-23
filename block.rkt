#lang racket
(require (only-in file/sha1 hex-string->bytes))
(require (only-in sha sha256))
(require (only-in sha bytes->hex-string))
(require racket/serialize)
(provide (struct-out block) mine-block valid-block? mined-block?)

;; TODO: Add actual data instead of just transaction
(struct actual-data
  (data id))

(struct data
  (signature from to value) #:prefab)

(struct block
  (current-hash previous-hash data timestamp nonce)
  #:prefab)
;; sha256 accepts bytes. We need use string->bytes. We need to keep everything as string for blockchain data file
 (define (calculate-block-hash previous-hash timestamp data nonce)
   (bytes->hex-string (sha256 (bytes-append
                                (string->bytes/utf-8 previous-hash) 
                                (string->bytes/utf-8 (number->string timestamp))
                                (string->bytes/utf-8 (~a (serialize data)))
                                (string->bytes/utf-8 (number->string nonce))))))
;; Check the validity of a block. Simply check current hash and the previous hashes.
(define (valid-block? block)
  (equal? (block-current-hash block)
          (calculate-block-hash (block-previous-hash block)
                                (block-timestamp block)
                                (block-data block)
                                (block-nonce block))))
;; Difficulty level for mining. The number of bytes to be same.
(define difficulty 3)
(define target (bytes->hex-string
                 (make-bytes difficulty 32)))
;; In this function we check if the block is mined
;; Of that, we convert the hex-string to bytes
;; Then we take the subbytes that take 2 argument: (start, end)
;; and returns a subbyte. We check if the proof-of-work matches
;; difficulty is 2 by default. It means we take first 2 bytes and check
;; in real world it can be 30 less or more to keep mining ~10 mins
(define (mined-block? block-hash)
  (equal? (subbytes (hex-string->bytes block-hash) 1 difficulty)
          (subbytes (hex-string->bytes target) 1 difficulty)))
;; Make a block
;; If it has the proof-of-work create a block
;; Otherwise check with additional nonce
(define (make-and-mine-block
         previous-hash timestamp data nonce)
  (let ([current-hash (calculate-block-hash
                       previous-hash timestamp data nonce)])
    (if (mined-block? current-hash)
        (block current-hash previous-hash data timestamp nonce)
        (make-and-mine-block
         previous-hash timestamp data (+ 1 nonce)))))

(define (mine-block data previous-hash)
  (make-and-mine-block
   previous-hash (current-milliseconds) data 1))
  