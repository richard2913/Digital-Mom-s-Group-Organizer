;; contracts/moms-group-organizer.clar
;; Digital Mom's Group Organizer - Main Contract
;; A comprehensive parenting group coordination platform with playdate scheduling

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_EVENT_NOT_FOUND (err u101))
(define-constant ERR_INVALID_DATE (err u102))
(define-constant ERR_ALREADY_REGISTERED (err u103))
(define-constant ERR_NOT_REGISTERED (err u104))
(define-constant ERR_EVENT_FULL (err u105))
(define-constant ERR_INVALID_AGE_RANGE (err u106))
(define-constant ERR_EVENT_PAST (err u107))

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var platform-fee uint u1000000) ;; 1 STX in microSTX

;; Data Maps

;; User profiles for moms/parents
(define-map user-profiles
    principal
    {
        name: (string-ascii 50),
        bio: (string-ascii 200),
        children-ages: (list 10 uint),
        location: (string-ascii 100),
        contact-info: (string-ascii 100),
        reputation-score: uint,
        events-organized: uint,
        events-attended: uint
    })

;; Event details
(define-map events
    uint
    {
        organizer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        event-type: (string-ascii 30), ;; "playdate", "educational", "outdoor", "indoor"
        location: (string-ascii 150),
        date: uint, ;; Block height when event occurs
        duration: uint, ;; Duration in hours
        max-participants: uint,
        current-participants: uint,
        age-range-min: uint,
        age-range-max: uint,
        cost: uint, ;; Cost in microSTX
        activity-suggestions: (list 5 (string-ascii 100)),
        requirements: (string-ascii 200),
        is-active: bool,
        created-at: uint
    })

;; Event registrations
(define-map event-registrations
    {event-id: uint, participant: principal}
    {
        registered-at: uint,
        children-count: uint,
        special-notes: (string-ascii 200),
        payment-status: bool
    })

;; Activity suggestions by age group
(define-map activity-suggestions
    {age-min: uint, age-max: uint}
    (list 20 (string-ascii 100)))

;; User authorization for event management
(define-map event-collaborators
    {event-id: uint, collaborator: principal}
    bool)

;; Private Functions

(define-private (is-event-organizer (event-id uint) (user principal))
    (match (map-get? events event-id)
        event (is-eq (get organizer event) user)
        false))

(define-private (is-event-collaborator (event-id uint) (user principal))
    (default-to false (map-get? event-collaborators {event-id: event-id, collaborator: user})))

(define-private (can-manage-event (event-id uint) (user principal))
    (or (is-event-organizer event-id user)
        (is-event-collaborator event-id user)))

(define-private (is-valid-age-range (min-age uint) (max-age uint))
    (and (<= min-age max-age) (<= max-age u18) (>= min-age u0)))

(define-private (calculate-reputation-bonus (events-organized uint) (events-attended uint))
    (+ (* events-organized u10) (* events-attended u5)))

;; Public Functions

;; User Management

(define-public (create-user-profile
    (name (string-ascii 50))
    (bio (string-ascii 200))
    (children-ages (list 10 uint))
    (location (string-ascii 100))
    (contact-info (string-ascii 100)))
    (begin
        (map-set user-profiles tx-sender {
            name: name,
            bio: bio,
            children-ages: children-ages,
            location: location,
            contact-info: contact-info,
            reputation-score: u100,
            events-organized: u0,
            events-attended: u0
        })
        (ok true)))

(define-public (update-user-profile
    (name (string-ascii 50))
    (bio (string-ascii 200))
    (children-ages (list 10 uint))
    (location (string-ascii 100))
    (contact-info (string-ascii 100)))
    (match (map-get? user-profiles tx-sender)
        current-profile
        (begin
            (map-set user-profiles tx-sender (merge current-profile {
                name: name,
                bio: bio,
                children-ages: children-ages,
                location: location,
                contact-info: contact-info
            }))
            (ok true))
        ERR_NOT_REGISTERED))

;; Event Management

(define-public (create-event
    (title (string-ascii 100))
    (description (string-ascii 500))
    (event-type (string-ascii 30))
    (location (string-ascii 150))
    (event-date uint)
    (duration uint)
    (max-participants uint)
    (age-range-min uint)
    (age-range-max uint)
    (cost uint)
    (event-activities (list 5 (string-ascii 100)))
    (requirements (string-ascii 200)))
    (let ((event-id (var-get next-event-id)))
        (asserts! (> event-date burn-block-height) ERR_INVALID_DATE)
        (asserts! (is-valid-age-range age-range-min age-range-max) ERR_INVALID_AGE_RANGE)
        (asserts! (> max-participants u0) ERR_INVALID_DATE)

        (map-set events event-id {
            organizer: tx-sender,
            title: title,
            description: description,
            event-type: event-type,
            location: location,
            date: event-date,
            duration: duration,
            max-participants: max-participants,
            current-participants: u0,
            age-range-min: age-range-min,
            age-range-max: age-range-max,
            cost: cost,
            activity-suggestions: event-activities,
            requirements: requirements,
            is-active: true,
            created-at: burn-block-height
        })

        ;; Update organizer's profile
        (match (map-get? user-profiles tx-sender)
            profile (map-set user-profiles tx-sender
                (merge profile {
                    events-organized: (+ (get events-organized profile) u1),
                    reputation-score: (+ (get reputation-score profile) u10)
                }))
            true)

        (var-set next-event-id (+ event-id u1))
        (ok event-id)))

(define-public (update-event
    (event-id uint)
    (title (string-ascii 100))
    (description (string-ascii 500))
    (location (string-ascii 150))
    (requirements (string-ascii 200)))
    (match (map-get? events event-id)
        event
        (begin
            (asserts! (can-manage-event event-id tx-sender) ERR_UNAUTHORIZED)
            (asserts! (get is-active event) ERR_EVENT_NOT_FOUND)
            (asserts! (> (get date event) burn-block-height) ERR_EVENT_PAST)

            (map-set events event-id (merge event {
                title: title,
                description: description,
                location: location,
                requirements: requirements
            }))
            (ok true))
        ERR_EVENT_NOT_FOUND))

(define-public (add-event-collaborator (event-id uint) (collaborator principal))
    (match (map-get? events event-id)
        event
        (begin
            (asserts! (is-event-organizer event-id tx-sender) ERR_UNAUTHORIZED)
            (map-set event-collaborators {event-id: event-id, collaborator: collaborator} true)
            (ok true))
        ERR_EVENT_NOT_FOUND))

(define-public (cancel-event (event-id uint))
    (match (map-get? events event-id)
        event
        (begin
            (asserts! (can-manage-event event-id tx-sender) ERR_UNAUTHORIZED)
            (asserts! (get is-active event) ERR_EVENT_NOT_FOUND)

            (map-set events event-id (merge event {is-active: false}))
            (ok true))
        ERR_EVENT_NOT_FOUND))

;; Registration Management

(define-public (register-for-event
    (event-id uint)
    (children-count uint)
    (special-notes (string-ascii 200)))
    (match (map-get? events event-id)
        event
        (begin
            (asserts! (get is-active event) ERR_EVENT_NOT_FOUND)
            (asserts! (> (get date event) burn-block-height) ERR_EVENT_PAST)
            (asserts! (< (get current-participants event) (get max-participants event)) ERR_EVENT_FULL)
            (asserts! (is-none (map-get? event-registrations {event-id: event-id, participant: tx-sender})) ERR_ALREADY_REGISTERED)

            ;; Handle payment if cost > 0
            (if (> (get cost event) u0)
                (try! (stx-transfer? (get cost event) tx-sender (get organizer event)))
                true)

            ;; Register participant
            (map-set event-registrations
                {event-id: event-id, participant: tx-sender}
                {
                    registered-at: burn-block-height,
                    children-count: children-count,
                    special-notes: special-notes,
                    payment-status: true
                })

            ;; Update event participant count
            (map-set events event-id
                (merge event {current-participants: (+ (get current-participants event) u1)}))

            ;; Update participant's profile
            (match (map-get? user-profiles tx-sender)
                profile (map-set user-profiles tx-sender
                    (merge profile {
                        events-attended: (+ (get events-attended profile) u1),
                        reputation-score: (+ (get reputation-score profile) u5)
                    }))
                true)

            (ok true))
        ERR_EVENT_NOT_FOUND))

(define-public (unregister-from-event (event-id uint))
    (match (map-get? events event-id)
        event
        (match (map-get? event-registrations {event-id: event-id, participant: tx-sender})
            registration
            (begin
                (asserts! (> (get date event) burn-block-height) ERR_EVENT_PAST)

                ;; Refund if payment was made and event is more than 24 hours away
                (if (and (> (get cost event) u0)
                         (> (get date event) (+ burn-block-height u144))) ;; ~24 hours
                    (try! (stx-transfer? (get cost event) (get organizer event) tx-sender))
                    true)

                ;; Remove registration
                (map-delete event-registrations {event-id: event-id, participant: tx-sender})

                ;; Update event participant count
                (map-set events event-id
                    (merge event {current-participants: (- (get current-participants event) u1)}))

                (ok true))
            ERR_NOT_REGISTERED)
        ERR_EVENT_NOT_FOUND))

;; Activity Suggestions

(define-public (add-activity-suggestions
    (age-min uint)
    (age-max uint)
    (activities (list 20 (string-ascii 100))))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-valid-age-range age-min age-max) ERR_INVALID_AGE_RANGE)

        (map-set activity-suggestions {age-min: age-min, age-max: age-max} activities)
        (ok true)))

;; Read-Only Functions

(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user))

(define-read-only (get-event (event-id uint))
    (map-get? events event-id))

(define-read-only (get-event-registration (event-id uint) (participant principal))
    (map-get? event-registrations {event-id: event-id, participant: participant}))

(define-read-only (get-activity-suggestions-for-age (age-min uint) (age-max uint))
    (map-get? activity-suggestions {age-min: age-min, age-max: age-max}))

(define-read-only (is-registered-for-event (event-id uint) (participant principal))
    (is-some (map-get? event-registrations {event-id: event-id, participant: participant})))

(define-read-only (get-next-event-id)
    (var-get next-event-id))

(define-read-only (get-user-events-organized (user principal))
    (match (map-get? user-profiles user)
        profile (some (get events-organized profile))
        none))

(define-read-only (get-user-reputation (user principal))
    (match (map-get? user-profiles user)
        profile (some (get reputation-score profile))
        none))

;; Admin Functions

(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set platform-fee new-fee)
        (ok true)))

(define-public (emergency-cancel-event (event-id uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (match (map-get? events event-id)
            event
            (begin
                (map-set events event-id (merge event {is-active: false}))
                (ok true))
            ERR_EVENT_NOT_FOUND)))

;;
;; Project Structure:
;; /contracts/moms-group-organizer.clar - Main contract (this file)
;; /tests/moms-group-organizer_test.ts - Comprehensive test suite
;; /settings/Devnet.toml - Local development configuration
;; /Clarinet.toml - Project configuration
;;

;; Clarinet.toml Configuration:
;; [project]
;; name = "digital-moms-group-organizer"
;; description = "A comprehensive parenting group coordination platform with playdate scheduling"
;; authors = ["Developer"]
;; clarinet_version = "2.0.0"
;; requirements = []
;;
;; [contracts.moms-group-organizer]
;; path = "contracts/moms-group-organizer.clar"
;; clarity_version = 2
;; epoch = 2.1

;; Initialize default activity suggestions
(map-set activity-suggestions {age-min: u0, age-max: u2}
    (list "Sensory play with textured materials" "Bubble play" "Simple music and movement" "Story time with picture books" "Playground swings"))

(map-set activity-suggestions {age-min: u3, age-max: u5}
    (list "Arts and crafts" "Nature scavenger hunt" "Dress-up play" "Simple cooking activities" "Playground games"))

(map-set activity-suggestions {age-min: u6, age-max: u8}
    (list "Board games" "Science experiments" "Sports activities" "Building challenges" "Educational field trips"))

(map-set activity-suggestions {age-min: u9, age-max: u12}
    (list "Team sports" "STEM projects" "Community service" "Advanced board games" "Skill-building workshops"))

(map-set activity-suggestions {age-min: u13, age-max: u18}
    (list "Leadership activities" "Career exploration" "Advanced sports" "Creative projects" "Volunteer opportunities"))
