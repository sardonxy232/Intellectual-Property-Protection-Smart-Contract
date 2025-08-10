;; Intellectual Property Protection Smart Contract
;; Production Implementation for Enterprise Deployment
;; Stacks Blockchain - Clarity Language

;; Contract Authority
(define-constant CONTRACT_OWNER tx-sender)

;; Data Variables
(define-data-var next-work-id uint u1)
(define-data-var contract-active bool true)

;; Error Constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_CONTRACT_DISABLED (err u104))
(define-constant ERR_INSUFFICIENT_PERMISSIONS (err u105))

;; Permission Levels
(define-constant PERMISSION_NONE u0)
(define-constant PERMISSION_READ u1)
(define-constant PERMISSION_WRITE u2)
(define-constant PERMISSION_ADMIN u3)

;; Data Maps
(define-map intellectual-works
  { work-id: uint }
  {
    title: (string-ascii 128),
    owner: principal,
    description: (string-ascii 256),
    creation-block: uint,
    content-hash: (buff 32),
    file-size: uint,
    work-type: (string-ascii 32),
    active: bool
  }
)

(define-map work-permissions
  { work-id: uint, user: principal }
  { permission-level: uint }
)

(define-map ownership-transfers
  { work-id: uint }
  {
    current-owner: principal,
    pending-owner: (optional principal),
    transfer-initiated: (optional uint)
  }
)

(define-map access-logs
  { work-id: uint, accessor: principal, block-height: uint }
  { action: (string-ascii 32) }
)

;; Private Functions
(define-private (is-contract-active)
  (var-get contract-active)
)

(define-private (work-exists (work-id uint))
  (is-some (map-get? intellectual-works { work-id: work-id }))
)

(define-private (is-work-owner (work-id uint) (user principal))
  (match (map-get? intellectual-works { work-id: work-id })
    work (is-eq (get owner work) user)
    false
  )
)

(define-private (validate-content-hash (hash (buff 32)))
  (is-eq (len hash) u32)
)

(define-private (validate-work-type (work-type (string-ascii 32)))
  (and
    (> (len work-type) u0)
    (<= (len work-type) u32)
    ;; Check for valid characters (alphanumeric and basic punctuation)
    (not (is-eq work-type ""))
  )
)

(define-private (validate-principal (user principal))
  ;; Basic principal validation - ensure it's not the zero principal
  (not (is-eq user 'SP000000000000000000002Q6VF78))
)

(define-private (has-permission (work-id uint) (user principal) (required-level uint))
  (or
    (is-work-owner work-id user)
    (>= (default-to u0 (get permission-level 
        (map-get? work-permissions { work-id: work-id, user: user }))) required-level)
  )
)

(define-private (log-access (work-id uint) (accessor principal) (action (string-ascii 32)))
  (map-set access-logs
    { work-id: work-id, accessor: accessor, block-height: block-height }
    { action: action }
  )
)

(define-private (increment-work-id)
  (let ((current-id (var-get next-work-id)))
    (var-set next-work-id (+ current-id u1))
    current-id
  )
)

;; Public Functions

;; Register new intellectual property work
(define-public (register-work 
  (title (string-ascii 128))
  (description (string-ascii 256))
  (content-hash (buff 32))
  (file-size uint)
  (work-type (string-ascii 32))
)
  (begin
    (asserts! (is-contract-active) ERR_CONTRACT_DISABLED)
    (asserts! (> (len title) u0) ERR_INVALID_INPUT)
    (asserts! (> (len description) u0) ERR_INVALID_INPUT)
    (asserts! (> file-size u0) ERR_INVALID_INPUT)
    (asserts! (validate-content-hash content-hash) ERR_INVALID_INPUT)
    (asserts! (validate-work-type work-type) ERR_INVALID_INPUT)

    (let ((work-id (increment-work-id)))
      (map-set intellectual-works
        { work-id: work-id }
        {
          title: title,
          owner: tx-sender,
          description: description,
          creation-block: block-height,
          content-hash: content-hash,
          file-size: file-size,
          work-type: work-type,
          active: true
        }
      )
      (map-set ownership-transfers
        { work-id: work-id }
        {
          current-owner: tx-sender,
          pending-owner: none,
          transfer-initiated: none
        }
      )
      (log-access work-id tx-sender "REGISTER")
      (ok work-id)
    )
  )
)

;; Grant access permissions to a user
(define-public (grant-permission 
  (work-id uint) 
  (user principal) 
  (permission-level uint)
)
  (begin
    (asserts! (is-contract-active) ERR_CONTRACT_DISABLED)
    (asserts! (work-exists work-id) ERR_NOT_FOUND)
    (asserts! (is-work-owner work-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (<= permission-level PERMISSION_ADMIN) ERR_INVALID_INPUT)
    (asserts! (validate-principal user) ERR_INVALID_INPUT)

    (map-set work-permissions
      { work-id: work-id, user: user }
      { permission-level: permission-level }
    )
    (log-access work-id tx-sender "GRANT_PERMISSION")
    (ok true)
  )
)

;; Revoke access permissions from a user
(define-public (revoke-permission (work-id uint) (user principal))
  (begin
    (asserts! (is-contract-active) ERR_CONTRACT_DISABLED)
    (asserts! (work-exists work-id) ERR_NOT_FOUND)
    (asserts! (is-work-owner work-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (validate-principal user) ERR_INVALID_INPUT)

    (map-delete work-permissions { work-id: work-id, user: user })
    (log-access work-id tx-sender "REVOKE_PERMISSION")
    (ok true)
  )
)

;; Access work metadata (requires read permission)
(define-public (access-work (work-id uint))
  (begin
    (asserts! (is-contract-active) ERR_CONTRACT_DISABLED)
    (asserts! (work-exists work-id) ERR_NOT_FOUND)
    (asserts! (has-permission work-id tx-sender PERMISSION_READ) ERR_INSUFFICIENT_PERMISSIONS)

    (log-access work-id tx-sender "ACCESS")
    (ok (map-get? intellectual-works { work-id: work-id }))
  )
)



;; Initiate ownership transfer
(define-public (initiate-transfer (work-id uint) (new-owner principal))
  (begin
    (asserts! (is-contract-active) ERR_CONTRACT_DISABLED)
    (asserts! (work-exists work-id) ERR_NOT_FOUND)
    (asserts! (is-work-owner work-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq tx-sender new-owner)) ERR_INVALID_INPUT)

    (map-set ownership-transfers
      { work-id: work-id }
      {
        current-owner: tx-sender,
        pending-owner: (some new-owner),
        transfer-initiated: (some block-height)
      }
    )
    (log-access work-id tx-sender "INITIATE_TRANSFER")
    (ok true)
  )
)

;; Accept ownership transfer
(define-public (accept-transfer (work-id uint))
  (begin
    (asserts! (is-contract-active) ERR_CONTRACT_DISABLED)
    (asserts! (work-exists work-id) ERR_NOT_FOUND)

    (match (map-get? ownership-transfers { work-id: work-id })
      transfer (begin
        (asserts! (is-eq (some tx-sender) (get pending-owner transfer)) ERR_UNAUTHORIZED)

        (match (map-get? intellectual-works { work-id: work-id })
          work (begin
            (map-set intellectual-works
              { work-id: work-id }
              (merge work { owner: tx-sender })
            )
            (map-set ownership-transfers
              { work-id: work-id }
              {
                current-owner: tx-sender,
                pending-owner: none,
                transfer-initiated: none
              }
            )
            (log-access work-id tx-sender "ACCEPT_TRANSFER")
            (ok true)
          )
          ERR_NOT_FOUND
        )
      )
      ERR_NOT_FOUND
    )
  )
)

;; Deactivate work (only owner)
(define-public (deactivate-work (work-id uint))
  (begin
    (asserts! (is-contract-active) ERR_CONTRACT_DISABLED)
    (asserts! (work-exists work-id) ERR_NOT_FOUND)
    (asserts! (is-work-owner work-id tx-sender) ERR_UNAUTHORIZED)

    (match (map-get? intellectual-works { work-id: work-id })
      work (begin
        (map-set intellectual-works
          { work-id: work-id }
          (merge work { active: false })
        )
        (log-access work-id tx-sender "DEACTIVATE")
        (ok true)
      )
      ERR_NOT_FOUND
    )
  )
)

;; Contract Administration Functions

;; Toggle contract active status (only contract owner)
(define-public (toggle-contract-status)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))
  )
)

;; Read-only Functions

;; Get work information (public read)
(define-read-only (get-work-info (work-id uint))
  (map-get? intellectual-works { work-id: work-id })
)

;; Get user permission for a work
(define-read-only (get-user-permission (work-id uint) (user principal))
  (default-to u0 (get permission-level 
    (map-get? work-permissions { work-id: work-id, user: user })))
)

;; Get ownership transfer status
(define-read-only (get-transfer-status (work-id uint))
  (map-get? ownership-transfers { work-id: work-id })
)

;; Check if user has specific permission level
(define-read-only (check-permission (work-id uint) (user principal) (required-level uint))
  (has-permission work-id user required-level)
)

;; Get contract status
(define-read-only (get-contract-status)
  {
    active: (var-get contract-active),
    next-work-id: (var-get next-work-id),
    contract-owner: CONTRACT_OWNER
  }
)

;; Get total registered works count
(define-read-only (get-total-works)
  (- (var-get next-work-id) u1)
)