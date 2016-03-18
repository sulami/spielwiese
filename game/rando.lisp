;; Generate a stat value randomly
(defun random-stat (baseline)
  (let ((min 35)
        (max 100))
    (* baseline (+ min (random (- max min))))))

;; Random element from a list
(defun random-list-elem (lst)
  (if (null lst)
      nil
      (let ((index (random (length lst))))
        (elt lst index))))

(defclass entity ()
  ((name :accessor entity-name
         :initarg :name
         :initform "???")
   (health :accessor entity-health
           :initarg :health
           :initform 100)
   (max-health :accessor entity-max-health
               :initarg :max-health
               :initform 100)
   (ablities :accessor entity-abilities
             :initarg :abilities
             :initform '())))

(defclass item ()
  ((name :accessor item-name
         :initarg :name
         :initform "???")
   (description :accessor item-description
                :initarg :description
                :initform "???")
   (value :accessor item-value
          :initarg :value
          :initform 1)
   (drop-rate :accessor item-drop-rate
              :initarg :drop-rate
              :initform 1)))

(defclass player (entity)
  ((energy :accessor player-energy
           :initarg :energy
           :initform '())
   (inventory :accessor player-inventory
              :initarg :inventory
              :initform '())))

(defclass enemy (entity)
  ((item-pool :accessor enemy-item-pool
              :initarg :item-pool
              :initform '())))

(defun build-item-pool (items)
  (let ((pool '()))
    (dolist (i items pool)
      (let ((this-item (make-list (item-drop-rate i) :initial-element i)))
        (setf pool (append pool this-item))))))

(defmethod initialize-instance :after ((object enemy) &rest args)
  (setf (enemy-item-pool object) (build-item-pool (enemy-item-pool object))))

(defmethod take-damage (amount (object entity))
  (setf (entity-health object)
        (max 0 (- (entity-health object) amount))))

(defclass ability ()
  ((name :accessor ability-name
         :initarg :name
         :initform "???")
   (description :accessor ability-description
                :initarg :description
                :initform "???")
   (cost :accessor ability-cost
         :initarg :cost
         :initform 15)
   (cooldown :accessor ability-cooldown
             :initarg :cooldown
             :initform nil)))

(defvar damage-types
  '(physical
    magic))

(defclass attack (ability)
  ((name :accessor attack-name
         :initarg :name
         :initform "???")
   (cost :accessor attack-cost
         :initarg :cost
         :initform 15)
   (cooldown :accessor attack-cooldown
             :initarg :cooldown
             :initform nil)
   (dmg-type :accessor attack-dmg-type
             :initarg :dmg-type
             :initform 'physical)
   (dmg-base :accessor attack-dmg-base
             :initarg :dmg-base
             :initform 10)
   (crit-chance :accessor attack-crit-chance
                :initarg :crit-chance
                :initform 2)
   (crit-multi :accessor attack-crit-multi
               :initarg :crit-multi
               :initform 2)))

(defclass attack-result ()
  ((attack :accessor attack-result-attack
           :initarg :attack
           :initform (error "Attack result without attack"))
   (dmg :accessor attack-result-dmg
        :initarg :dmg
        :initform (error "Attack result without damage"))
   (crit :accessor attack-result-crit
         :initarg :crit
         :initform nil)
   (source :accessor attack-result-source
           :initarg :source
           :initform nil)
   (target :accessor attack-result-target
           :initarg :target
           :initform nil)))

(defun generate-attack-result (atk)
  (let ((atkr (make-instance 'attack-result
                             :attack atk
                             :dmg (random-stat (attack-dmg-base atk)))))
    (when (<= (random 100) (attack-crit-chance atk))
      (setf (attack-result-crit atkr) t)
      (setf (attack-result-dmg atkr)
            (* (attack-result-dmg atkr) (attack-crit-multi atk))))
    atkr))
