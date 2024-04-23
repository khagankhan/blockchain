#lang racket
(require "./terminal.rkt")
(require "./blockchain.rkt")

(when (file-exists? "blockchain.data")
     (begin
       (printf "Found 'blockchain.data', reading...\n")
       (print-blockchain (file->struct "blockchain.data"))
       (exit)))

(define coin-base (make-wallet))
   (define wallet-a (make-wallet))
   (define wallet-b (make-wallet))
;;wallet-a
(printf "Making genesis transaction...\n")
   (define genesis-t (make-transaction coin-base wallet-a 100 '()))

(define utxo (list (make-transaction-io 100 wallet-a)))

(printf "Mining genesis block...\n")
   (define blockchain (init-blockchain genesis-t "a3q24faaacrgfsgrs" utxo))

 (printf "Mining second transaction...\n")
  (set! blockchain (send-money-blockchain blockchain wallet-a wallet-b 2)) ;; Sorry :(
  (print-wallets blockchain wallet-a wallet-b)

(printf "Mining second transaction...\n")
   (set! blockchain (send-money-blockchain blockchain wallet-a wallet-b 2))
   (print-wallets blockchain wallet-a wallet-b)

(printf "Attempting to mine fourth (not-valid) transaction...\n")
   (set! blockchain (send-money-blockchain blockchain wallet-b wallet-a 3))
   (print-wallets blockchain wallet-a wallet-b)

(printf "Blockchain is valid: ~a\n\n" (valid-blockchain? blockchain))

(for ([block (blockchain-blocks blockchain)])
   (print-block block) (newline))

(struct->file blockchain "blockchain.data")
 (printf "Exported blockchain to 'blockchain.data'...\n")


