#lang racket

(require "./rsa.rkt")
(provide (struct-out wallet) make-wallet)
;; Wallet struct that keeps the public and private keys
(struct wallet (private-key public-key) #:prefab)
;; #:prefab let us to print and serialize or deserialize

;; Define an associated function for wallet
;; That calls (generate-public-private-pairs) from rsa.rkt
;; Subsequently, it create a new wallet in a serialized way
;; Private and Public keys.
(define (make-wallet)
  (let* ([key-pair (generate-public-private-pairs)]
         [privkey-info (cdr key-pair)]
         [pubkey-info (car key-pair)])
    (wallet (key-to-hex-string privkey-info 'PrivateKey)
            (key-to-hex-string pubkey-info 'PublicKey))))

(define (key-to-hex-string key-info type)
  (bytes->hex-string (serialize-key key-info type)))

(define (serialize-key key-info type)
  (let ([d-or-e (car key-info)]
        [n (cdr key-info)])
    (format "~a: (~a, ~a)" (if (eq? type 'PrivateKey) "Private Key" "Public Key") d-or-e n)))

(define (bytes->hex-string bytes) 
  (let* ([byte-list (string->list bytes)]
         [hex-list (map char->hex byte-list)])
    (apply string-append hex-list)))

(define (char->hex char)
  (format "~x" (char->integer char)))
 (make-wallet)
;;;;;
 ;; datum->pk-key: d2i_PKCS8_PRIV_KEY_INFO: too long [asn1 encoding routines:ASN1_get_object:218570907]
;;;;;
;; In "bytes->hex-string we assume that bytes are a string; This will not work in "block.rkt". Use library version:
#|
wallet.rkt:28:20: string->list: contract violation
  expected: string?
  given: #"4i\342r\303r\245L~Y\251\235\24f\200F\177\5\331M\256\2032m\243!\254qK\246\273\263"
|#