;; Health Data Monetization Platform
;; Consent-based health data sales to researchers

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_INVALID_AMOUNT (err u400))
(define-constant ERR_CONSENT_EXPIRED (err u403))
(define-constant ERR_INSUFFICIENT_FUNDS (err u402))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_DATA (err u422))
(define-constant PLATFORM_FEE_PERCENT u5) ;; 5% platform fee

;; Data Variables
(define-data-var next-data-id uint u1)
(define-data-var next-purchase-id uint u1)
(define-data-var platform-treasury principal CONTRACT_OWNER)

;; Maps
(define-map health-data-records
  uint
  {
    owner: principal,
    data-hash: (buff 32),
    price: uint,
    category: (string-ascii 50),
    consent-expiry: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map user-profiles
  principal
  {
    total-earnings: uint,
    data-count: uint,
    reputation-score: uint,
    is-verified: bool
  }
)

(define-map researcher-profiles
  principal
  {
    total-spent: uint,
    purchases-count: uint,
    is-approved: bool,
    institution: (string-ascii 100)
  }
)

(define-map data-purchases
  uint
  {
    data-id: uint,
    buyer: principal,
    seller: principal,
    amount: uint,
    purchase-date: uint,
    access-granted: bool
  }
)

(define-map consent-permissions
  {data-id: uint, researcher: principal}
  {
    granted: bool,
    expiry: uint,
    specific-consent: bool
  }
)

(define-map access-keys
  {data-id: uint, researcher: principal}
  {
    encrypted-key: (buff 64),
    granted-at: uint,
    expires-at: uint
  }
)

;; Private Functions
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount PLATFORM_FEE_PERCENT) u100)
)

(define-private (is-consent-valid (data-id uint) (researcher principal))
  (let ((consent (map-get? consent-permissions {data-id: data-id, researcher: researcher}))
        (data-record (unwrap! (map-get? health-data-records data-id) false)))
    (match consent
      some-consent (and 
        (get granted some-consent)
        (< block-height (get expiry some-consent))
        (< block-height (get consent-expiry data-record)))
      false
    )
  )
)

;; Public Functions

;; Register user profile
(define-public (register-user)
  (let ((user-exists (map-get? user-profiles tx-sender)))
    (if (is-some user-exists)
      ERR_ALREADY_EXISTS
      (ok (map-set user-profiles tx-sender {
        total-earnings: u0,
        data-count: u0,
        reputation-score: u100,
        is-verified: false
      }))
    )
  )
)

;; Register researcher profile
(define-public (register-researcher (institution (string-ascii 100)))
  (let ((researcher-exists (map-get? researcher-profiles tx-sender)))
    (if (is-some researcher-exists)
      ERR_ALREADY_EXISTS
      (ok (map-set researcher-profiles tx-sender {
        total-spent: u0,
        purchases-count: u0,
        is-approved: false,
        institution: institution
      }))
    )
  )
)

;; Approve researcher (only contract owner)
(define-public (approve-researcher (researcher principal))
  (if (is-eq tx-sender CONTRACT_OWNER)
    (let ((researcher-profile (unwrap! (map-get? researcher-profiles researcher) ERR_NOT_FOUND)))
      (ok (map-set researcher-profiles researcher 
        (merge researcher-profile {is-approved: true})
      ))
    )
    ERR_UNAUTHORIZED
  )
)

;; Upload health data with consent parameters
(define-public (upload-health-data 
  (data-hash (buff 32)) 
  (price uint) 
  (category (string-ascii 50))
  (consent-duration uint))
  (let ((data-id (var-get next-data-id))
        (user-profile (unwrap! (map-get? user-profiles tx-sender) ERR_NOT_FOUND)))
    (if (> price u0)
      (begin
        (map-set health-data-records data-id {
          owner: tx-sender,
          data-hash: data-hash,
          price: price,
          category: category,
          consent-expiry: (+ block-height consent-duration),
          is-active: true,
          created-at: block-height
        })
        (map-set user-profiles tx-sender 
          (merge user-profile {data-count: (+ (get data-count user-profile) u1)})
        )
        (var-set next-data-id (+ data-id u1))
        (ok data-id)
      )
      ERR_INVALID_AMOUNT
    )
  )
)

;; Grant specific consent to researcher
(define-public (grant-consent (data-id uint) (researcher principal) (duration uint))
  (let ((data-record (unwrap! (map-get? health-data-records data-id) ERR_NOT_FOUND)))
    (if (is-eq tx-sender (get owner data-record))
      (ok (map-set consent-permissions {data-id: data-id, researcher: researcher} {
        granted: true,
        expiry: (+ block-height duration),
        specific-consent: true
      }))
      ERR_UNAUTHORIZED
    )
  )
)

;; Purchase health data
(define-public (purchase-data (data-id uint))
  (let ((data-record (unwrap! (map-get? health-data-records data-id) ERR_NOT_FOUND))
        (researcher-profile (unwrap! (map-get? researcher-profiles tx-sender) ERR_NOT_FOUND))
        (user-profile (unwrap! (map-get? user-profiles (get owner data-record)) ERR_NOT_FOUND))
        (purchase-id (var-get next-purchase-id))
        (platform-fee (calculate-platform-fee (get price data-record)))
        (seller-amount (- (get price data-record) platform-fee)))
    
    (if (and 
          (get is-approved researcher-profile)
          (get is-active data-record)
          (< block-height (get consent-expiry data-record)))
      (match (stx-transfer? (get price data-record) tx-sender (get owner data-record))
        success (begin
          ;; Transfer platform fee
          (try! (stx-transfer? platform-fee tx-sender (var-get platform-treasury)))
          
          ;; Record purchase
          (map-set data-purchases purchase-id {
            data-id: data-id,
            buyer: tx-sender,
            seller: (get owner data-record),
            amount: (get price data-record),
            purchase-date: block-height,
            access-granted: true
          })
          
          ;; Update profiles
          (map-set researcher-profiles tx-sender 
            (merge researcher-profile {
              total-spent: (+ (get total-spent researcher-profile) (get price data-record)),
              purchases-count: (+ (get purchases-count researcher-profile) u1)
            })
          )
          
          (map-set user-profiles (get owner data-record)
            (merge user-profile {
              total-earnings: (+ (get total-earnings user-profile) seller-amount)
            })
          )
          
          ;; Grant general consent
          (map-set consent-permissions {data-id: data-id, researcher: tx-sender} {
            granted: true,
            expiry: (get consent-expiry data-record),
            specific-consent: false
          })
          
          (var-set next-purchase-id (+ purchase-id u1))
          (ok purchase-id)
        )
        error ERR_INSUFFICIENT_FUNDS
      )
      ERR_CONSENT_EXPIRED
    )
  )
)

;; Provide encrypted access key (only data owner)
(define-public (provide-access-key 
  (data-id uint) 
  (researcher principal) 
  (encrypted-key (buff 64)))
  (let ((data-record (unwrap! (map-get? health-data-records data-id) ERR_NOT_FOUND)))
    (if (and 
          (is-eq tx-sender (get owner data-record))
          (is-consent-valid data-id researcher))
      (ok (map-set access-keys {data-id: data-id, researcher: researcher} {
        encrypted-key: encrypted-key,
        granted-at: block-height,
        expires-at: (get consent-expiry data-record)
      }))
      ERR_UNAUTHORIZED
    )
  )
)

;; Revoke consent (data owner only)
(define-public (revoke-consent (data-id uint) (researcher principal))
  (let ((data-record (unwrap! (map-get? health-data-records data-id) ERR_NOT_FOUND)))
    (if (is-eq tx-sender (get owner data-record))
      (ok (map-set consent-permissions {data-id: data-id, researcher: researcher} {
        granted: false,
        expiry: block-height,
        specific-consent: false
      }))
      ERR_UNAUTHORIZED
    )
  )
)

;; Update data pricing
(define-public (update-data-price (data-id uint) (new-price uint))
  (let ((data-record (unwrap! (map-get? health-data-records data-id) ERR_NOT_FOUND)))
    (if (and 
          (is-eq tx-sender (get owner data-record))
          (> new-price u0))
      (ok (map-set health-data-records data-id 
        (merge data-record {price: new-price})
      ))
      ERR_UNAUTHORIZED
    )
  )
)

;; Deactivate data listing
(define-public (deactivate-data (data-id uint))
  (let ((data-record (unwrap! (map-get? health-data-records data-id) ERR_NOT_FOUND)))
    (if (is-eq tx-sender (get owner data-record))
      (ok (map-set health-data-records data-id 
        (merge data-record {is-active: false})
      ))
      ERR_UNAUTHORIZED
    )
  )
)

;; Read-only functions

;; Get data record
(define-read-only (get-data-record (data-id uint))
  (map-get? health-data-records data-id)
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles user)
)

;; Get researcher profile  
(define-read-only (get-researcher-profile (researcher principal))
  (map-get? researcher-profiles researcher)
)

;; Get purchase record
(define-read-only (get-purchase-record (purchase-id uint))
  (map-get? data-purchases purchase-id)
)

;; Check consent status
(define-read-only (check-consent-status (data-id uint) (researcher principal))
  (is-consent-valid data-id researcher)
)

;; Get access key
(define-read-only (get-access-key (data-id uint) (researcher principal))
  (if (is-consent-valid data-id researcher)
    (map-get? access-keys {data-id: data-id, researcher: researcher})
    none
  )
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-data-records: (- (var-get next-data-id) u1),
    total-purchases: (- (var-get next-purchase-id) u1),
    platform-fee-percent: PLATFORM_FEE_PERCENT
  }
)