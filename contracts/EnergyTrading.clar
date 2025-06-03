;; EnergyTrading: Renewable Energy Credit Trading Platform
;; Version: 1.0.0

(define-data-var grid-regulator principal tx-sender)
(define-data-var generation-pool uint u0)
(define-data-var certificate-rate uint u85) ;; certificates per block
(define-data-var last-certificate-block uint u0) ;; last block when certificates were calculated
(define-map producer-contributions principal uint)

;; Helper function to ensure only the grid regulator can perform certain actions
(define-private (is-regulator (caller principal))
  (begin
    (asserts! (is-eq caller (var-get grid-regulator)) (err u600))
    (ok true)))

;; Initialize the energy trading platform
(define-public (activate-grid (regulator principal))
  (begin
    (asserts! (is-none (map-get? producer-contributions regulator)) (err u601))
    (var-set grid-regulator regulator)
    (ok "EnergyTrading grid activated")))

;; Register renewable energy to the grid
(define-public (register-production (megawatts uint))
  (begin
    (asserts! (> megawatts u0) (err u602))
    (let ((current-contribution (default-to u0 (map-get? producer-contributions tx-sender))))
      (map-set producer-contributions tx-sender (+ current-contribution megawatts))
      (var-set generation-pool (+ (var-get generation-pool) megawatts))
      (ok (+ current-contribution megawatts)))))

;; Calculate renewable energy certificates for all producers
(define-public (issue-certificates)
  (begin
    (try! (is-regulator tx-sender))
    (let ((current-block tenure-height)
          (previous-issuance (var-get last-certificate-block)))
      (asserts! (> current-block previous-issuance) (err u603))
      ;; Calculate certificates based on blocks elapsed
      (let ((elapsed (- current-block previous-issuance))
            (total-certificates (* elapsed (var-get certificate-rate))))
        (var-set last-certificate-block current-block)
        (var-set generation-pool (+ (var-get generation-pool) total-certificates))
        (ok total-certificates)))))

;; Claim energy production and certificates
(define-public (redeem-certificates)
  (begin
    (let ((producer-contribution (default-to u0 (map-get? producer-contributions tx-sender))))
      (asserts! (> producer-contribution u0) (err u604))
      (let ((total-generation (var-get generation-pool))
            (new-certificates (* (var-get certificate-rate) (- tenure-height (var-get last-certificate-block))))
            (production-ratio (/ (* producer-contribution u100000) total-generation)))
        ;; Calculate certificates based on production ratio
        (let ((certificate-amount (/ (* production-ratio new-certificates) u100000)))
          (map-delete producer-contributions tx-sender)
          (var-set generation-pool (- (var-get generation-pool) producer-contribution))
          (ok (+ producer-contribution certificate-amount)))))))