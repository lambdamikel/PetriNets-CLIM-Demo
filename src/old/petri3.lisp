;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: clim-user -*-

;;;
;;; Very simple petri net editor in Common LISP (CLIM/CLOS-Demo)
;;; Lets you create token nets and play with them 
;;; Demonstrates some basic CLIM and CLOS programming techniques
;;; (C) 2003 by Michael Wessel 
;;; 

#-:mswindows
(error "This version if for LispWorks CLIM Windows only!")

#-:lispworks
(error "This version if for LispWorks CLIM Windows only!") 

;;;
;;;
;;;

(require "clim") 

(in-package clim-user)

;;;
;;; "Model" Classes
;;;

(defclass petri-net ()
  ((places :accessor places :initform nil)
   (transitions :accessor transitions :initform nil)
   (edges :accessor edges :initform nil)))

(defclass token-net (petri-net)
  ())

;;;
;;;
;;;

(defclass petri-net-item () 
  ((in-net :accessor in-net :initarg :in-net)))

(defclass petri-net-item-with-capacity (petri-net-item)
  ((capacity :accessor capacity :initarg :capacity :initform 1)))

(defclass place-or-transition (petri-net-item)
  ((outgoing-edges :accessor outgoing-edges :initform nil)
   (incoming-edges :accessor incoming-edges :initform nil)))

;;;
;;;
;;;

(defclass transition (place-or-transition)  
  ())

;;;
;;;
;;;

(defclass place (place-or-transition)
  ())

(defclass place-with-net-tokens (place)
  ((net-tokens :accessor net-tokens :initarg :net-tokens :initform 0)))

(defclass place-with-capacity (place-with-net-tokens petri-net-item-with-capacity)
  ())

;;;
;;;
;;;

(defclass edge (petri-net-item) 
  ((from :accessor from :initarg :from)
   (to :accessor to :initarg :to)))

(defclass edge-with-capacity (edge petri-net-item-with-capacity)
  ())

;;;
;;;
;;;

(defun make-petri-net ()
  (make-instance 'petri-net))

(defun make-token-net ()
  (make-instance 'token-net))

;;;
;;;
;;;

(defmethod initialize-instance :after ((transition transition) &rest initargs)
  (push transition (transitions (in-net transition))))
  
(defmethod make-transition ((net petri-net) &key &allow-other-keys)
  (make-instance 'transition :in-net net))

;;;
;;;
;;;

(defmethod initialize-instance :after ((place place) &rest initargs)
  (push place (places (in-net place))))
  
(defmethod make-place ((net petri-net) &rest args)
  (make-instance 'place :in-net net))

(defmethod make-place ((net token-net) &key (net-tokens 0) capacity &allow-other-keys)
  (if capacity 
      (make-instance 'place-with-capacity 
                     :net-tokens net-tokens :capacity capacity :in-net net)
    (make-instance 'place-with-net-tokens 
                   :net-tokens net-tokens :in-net net)))

;;;
;;;
;;;

(defmethod initialize-instance :after ((edge edge) &rest initargs) 
  (push edge (outgoing-edges (from edge)))
  (push edge (incoming-edges (to edge)))
  (push edge (edges (in-net edge))))

(defmethod link :before ((a  place-or-transition) (b place-or-transition)
                         &key &allow-other-keys)
  (unless (eq (in-net a) (in-net b))
    (error "~A and ~A must be in same net!" a b))
  (when (some #'(lambda (outgoing-edge)
                  (eq (to outgoing-edge) b))
              (outgoing-edges a))
    (error "~A and ~A are already linked!" a b)))

(defmethod link ((a place-or-transition) (b place-or-transition) 
                 &key &allow-other-keys)
  (error "Can only link places with transitions or transitions with places!"))

(defmethod link ((transition transition) (place place) &key &allow-other-keys)
  (make-instance 'edge :in-net (in-net transition) :from transition :to place))

(defmethod link ((place place) (transition transition) &key &allow-other-keys)
  (make-instance 'edge :in-net (in-net transition) :from place :to transition))

(defmethod link ((place place-with-net-tokens) (transition transition)
                 &key (capacity 1) &allow-other-keys)
  (make-instance 'edge-with-capacity :in-net (in-net transition) 
                 :from place :to transition :capacity capacity))

(defmethod link ((transition transition) (place place-with-net-tokens) 
                 &key (capacity 1) &allow-other-keys)
  (make-instance 'edge-with-capacity :in-net (in-net transition) 
                 :from transition :to place :capacity capacity))

;;;
;;;
;;;

(defmethod unlink ((a  place-or-transition) (b place-or-transition))
  (dolist (outgoing-edge (outgoing-edges a))
    (when (eq (to outgoing-edge) b)
      (remove-from-net outgoing-edge))))

;;;
;;;
;;;

(defmethod remove-from-net ((edge edge))
  (setf (outgoing-edges (from edge))
        (delete edge (outgoing-edges (from edge))))
  (setf (incoming-edges (to edge))
        (delete edge (incoming-edges (to edge))))
  (setf (edges (in-net edge))
        (delete edge (edges (in-net edge))))
  (in-net edge))

(defmethod remove-from-net ((place-or-transition place-or-transition))
  (dolist (edge (append (outgoing-edges place-or-transition)
                        (incoming-edges place-or-transition)))
    (remove-from-net edge))
  (in-net place-or-transition))

(defmethod remove-from-net :after ((transition transition))
  (setf (transitions (in-net transition))
        (delete transition (transitions (in-net transition)))))

(defmethod remove-from-net :after ((place place))
  (setf (places (in-net place))
        (delete place (places (in-net place)))))

;;;
;;;
;;;

(defmethod may-have-more-net-tokens-p ((place place-with-net-tokens))
  t)

(defmethod may-have-more-net-tokens-p ((place place-with-capacity))
  (< (net-tokens place) (capacity place)))

;;;
;;;
;;;

(defmethod add-net-tokens ((place place-with-net-tokens) &optional (net-tokens 1))
  (incf (net-tokens place) net-tokens))

(defmethod add-net-tokens :after ((place place-with-capacity) &optional args)
  (setf (net-tokens place) 
        (min (net-tokens place) 
             (capacity place))))

(defmethod remove-net-tokens ((place place-with-net-tokens) &optional (net-tokens 1))
  (unless (zerop (net-tokens place))
    (decf (net-tokens place) net-tokens)))

;;;
;;;
;;;

(defmethod increase-capacity ((item petri-net-item-with-capacity)
                              &optional (increment 1))
  (incf (capacity item) increment))

(defmethod decrease-capacity ((item petri-net-item-with-capacity)
                              &optional (increment 1))
  (unless (zerop (1- (capacity item)))
    (decf (capacity item) increment)))

;;;
;;;
;;;

(defmethod activated-p ((transition transition))
  (active-p (in-net transition) transition))

(defmethod active-p ((net petri-net) (transition transition))
  t)

(defmethod active-p ((net token-net) (transition transition))
  (and (incoming-edges transition)
       (every #'(lambda (incoming-edge)
                  (>= (net-tokens (from incoming-edge))
                      (capacity incoming-edge)))
              (incoming-edges transition))
       (outgoing-edges transition)
       (every #'(lambda (outgoing-edge)
                  (or (not (typep (to outgoing-edge)
                                  'place-with-capacity))
                      (<= (+ (net-tokens (to outgoing-edge))
                             (capacity outgoing-edge))
                          (capacity (to outgoing-edge)))))
              (outgoing-edges transition))))


;;;
;;;
;;;

(defmethod activate :before ((transition transition))
  (unless (activated-p transition)
    (error "Transition ~A is not active!" transition)))

(defmethod activate ((transition transition))
  (make-step transition  (in-net transition)))

;;;
;;;
;;;

(defmethod make-step ((transition transition) (net petri-net))
  net)

(defmethod make-step ((transition transition) (net token-net))
  (dolist (incoming-edge (incoming-edges transition))
    (remove-net-tokens (from incoming-edge)
                   (capacity incoming-edge)))
  (dolist (outgoing-edge (outgoing-edges transition))
    (add-net-tokens (to outgoing-edge)
                (capacity outgoing-edge)))
  net)

;;;
;;;
;;;

(defmethod step-net ((net petri-net))
  t)

(defmethod step-net ((net token-net))  
  (let ((active-transitions (remove-if-not #'activated-p (transitions net))))
    (labels ((one-of (sequence)
               (elt sequence (random (length sequence)))))
      (when active-transitions
        (activate (one-of active-transitions))))))

;;;
;;; "View" Classes
;;;

(defconstant +font+ (make-text-style :sans-serif :bold :small))

(defclass display-object () 
  ((object-color :accessor object-color :initform +black+)))

(defclass positioned-display-object (display-object)
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defclass transition-view (positioned-display-object transition) 
  ((object-color :initform +red+)))

(defclass place-view (positioned-display-object place) 
  ((object-color :initform +blue+)))

(defclass place-with-net-tokens-view (place-view place-with-net-tokens) 
  ((object-color :initform +blue+)))

(defclass place-with-capacity-view (place-with-net-tokens-view place-with-capacity)
  ())

(defclass edge-view (display-object edge)
  ())

(defclass edge-with-capacity-view (edge-view edge-with-capacity)
  ())

;;;
;;; 
;;;

(defclass petri-net-view (petri-net standard-application-frame) 
  ())

(defclass token-net-view (petri-net-view token-net) 
  ())

;;;
;;; Solve the "make isn't generic"-problem (kind of "Factory Pattern") 
;;;

(defmethod make-place ((net petri-net-view) &rest initargs)
  (apply #'change-class-of-instance (call-next-method) initargs))

(defmethod make-transition ((net petri-net-view) &rest initargs)
  (apply #'change-class-of-instance (call-next-method) initargs))

(defmethod link :around ((transition transition-view) (place place-view)
                         &rest initargs)
  (apply #'change-class-of-instance (call-next-method) initargs))

(defmethod link :around ((place place-view) (transition transition-view)
                         &rest initargs)
  (apply #'change-class-of-instance (call-next-method) initargs))

;;;
;;; 
;;;

(defmethod change-class-of-instance ((transition transition) &rest initargs)
  (apply #'change-class transition 'transition-view initargs))

(defmethod change-class-of-instance ((place place) &rest initargs)
  (apply #'change-class place 'place-view initargs))

(defmethod change-class-of-instance ((place place-with-net-tokens) &rest initargs)
  (apply #'change-class place 'place-with-net-tokens-view initargs))

(defmethod change-class-of-instance ((place place-with-capacity) &rest initargs)
  (apply #'change-class place 'place-with-capacity-view initargs))

(defmethod change-class-of-instance ((edge edge) &rest initargs)
  (apply #'change-class edge 'edge-view initargs))

(defmethod change-class-of-instance ((edge edge-with-capacity) &rest initargs) 
  (apply #'change-class edge 'edge-with-capacity-view initargs))

;;;
;;; 
;;;

(defun get-random-net (n m p)
  (let ((net (make-petri-net)))
    (change-class net 'petri-net-view)
    (let ((places
           (loop as i from 1 to n collect (make-place net)))
          (transitions       
           (loop as i from 1 to m collect (make-transition net))))
      (loop as place in places do
            (loop as transition in transitions do
                  (when (zerop (random p))
                    (link place transition)))))
    net))
  
;;;
;;; Define the application frame 
;;; Use inheritance to get a petri net editor 
;;; (instead of, e.g., using association)
;;; 


(define-application-frame petri-net-editor (token-net-view)
  (; (net :accessor net :initform (get-random-net 10 10 3))
   (scaling-factor :accessor scaling-factor :initform 1.0))
  

  (:command-table (petri-net-editor
                   :menu (("Commands" :menu command-table))))
  
  (:panes   
   
   (pointer-documentation-pane 
    (make-clim-stream-pane :type 'pointer-documentation-pane
      :text-style +font+
      :height '(1 :line)
      :min-height '(1 :line)))   

   (command :interactor
            :text-style +font+)
   
   (display :application
            :display-function #'draw
            :incremental-redisplay t                   
            :redisplay-after-commands t)
   
   (scaling-factor :application
                   :text-style +font+
                   :scroll-bars nil
                   :incremental-redisplay t
                   :display-function 
                   #'(lambda (frame stream)
                       (updating-output (stream :unique-id 'scaling-factor
                                                :cache-value (scaling-factor frame)
                                                :cache-test #'=)
                         (format stream "Current Scaling Factor: ~A" 
                                 (scaling-factor frame)))))

   (slider (make-pane 'slider 
                      :text-style +font+
                      :scroll-bars nil
                      :client 'slider
                      :id 'slider
                      :min-value 0.1 :max-value 10
                      :number-of-tick-marks 10
                      :value-changed-callback
                      #'(lambda (slider val)
                          (declare (ignore slider))
                          (with-application-frame (frame)
                            (setf (scaling-factor frame) val)
                            (redisplay-frame-panes frame)))))

   (quit-button
    (make-pane 'push-button
               :label "Quit!"
               :text-style +font+
               :activate-callback #'(lambda (button)
                                      (declare (ignore button))
                                      (with-application-frame (frame)
                                        (frame-exit frame)))))
   (refresh-button
    (make-pane 'push-button
               :label "Refresh!"
               :label "Quit!"
               :activate-callback #'(lambda (button)
                                      (declare (ignore button))
                                      (with-application-frame (frame)
                                        (setf (scaling-factor frame) 1.0)
                                        (redisplay-frame-panes frame :force-p t)))))
   (step-button
    (make-pane 'push-button
               :label "Step!"
               :label "Quit!"
               :activate-callback #'(lambda (button)
                                      (declare (ignore button))
                                      (with-application-frame (frame)
                                        (step-net frame)
                                        (redisplay-frame-panes frame))))))
  (:layouts
   (:default
    (vertically ()
      (3/4
       (vertically ()
         (horizontally () 
           (1/7 quit-button)
           (1/7 refresh-button)
           (1/7 step-button))
         (horizontally ()
           (1/2 slider)
           (1/2 scaling-factor))           
         display))
      (1/4 command)
      pointer-documentation-pane))))

;;;
;;; 
;;;

(defmethod get-pane-size ((stream stream))
  (bounding-rectangle-size (bounding-rectangle (window-viewport stream))))


(defmethod get-relative-coordinates ((frame petri-net-editor) x y)
  (multiple-value-bind (width height)
      (get-pane-size (get-frame-pane frame 'display))
    (values (/ (/ x width) 
               (scaling-factor frame))
            (/ (/ y height)
               (scaling-factor frame)))))


(defmethod get-dimensions ((transition transition-view))
  (with-application-frame (frame) 
    (values (/ 0.05 (scaling-factor frame))
            (/ 0.03 (scaling-factor frame)))))

(defmethod get-dimensions ((place place-view))
  (with-application-frame (frame) 
    (values (/ 0.05 (scaling-factor frame)))))

;;;
;;; Draw the editor's content
;;;                  

(defmethod draw ((frame petri-net-editor) stream)
  (multiple-value-bind (width height)
      (get-pane-size stream)
    (with-scaling (stream (scaling-factor frame) (scaling-factor frame) )
      (with-scaling (stream width height)
        (dolist (object (append (places frame)
                                (transitions frame)))
          (present object (type-of object)
                   :stream stream 
                   :view +gadget-view+ :single-box t))
        (dolist (edge (edges frame))
          (present edge (type-of edge) :stream stream :view +gadget-view+))))))


;;;
;;; Define the presentation methods
;;;

(define-presentation-method present :around (object 
                                             (type positioned-display-object) 
                                             stream
                                             (view gadget-view) &key)
  (with-translation (stream (x object) (y object))
    (call-next-method)))


(define-presentation-method present :around (object 
                                             (type display-object)
                                             stream
                                             (view gadget-view) &key)
  (with-drawing-options (stream :line-thickness 3
                                :ink (object-color object) 
                                :text-style +font+)
    (call-next-method)))


(define-presentation-method present (place (type place-view) stream
                                           (view gadget-view) &key)
  (with-application-frame (frame) 
    (multiple-value-bind (radius)
        (get-dimensions place)  
      (updating-output (stream :unique-id place
                               :cache-value (list (scaling-factor frame) 
                                                  (x place) (y place)
                                                  (object-color place))
                               :cache-test #'equal)        
        (draw-circle* stream 0 0 radius :filled nil)))))

(define-presentation-method present (place (type place-with-net-tokens-view) stream
                                           (view gadget-view) &key)
  (with-application-frame (frame)  
    (labels ((deg-to-rad (phi)
               (* pi (/ phi 180))))

      (multiple-value-bind (radius)
          (get-dimensions place)
        (updating-output (stream :unique-id place
                                 :cache-value (list (scaling-factor frame)
                                                    (x place) (y place)
                                                    (object-color place) 
                                                    (net-tokens place))
                                 :cache-test #'equal)
          (draw-circle* stream 0 0 radius :filled nil))
        (unless (zerop (net-tokens place))
          (let* ((n (net-tokens place))
                 (w (/ 360 n))
                 (r (* 2/3 radius))
                 (s (* 1/8 radius)))
            (loop as a from 1 to n do 
                  (draw-circle* stream
                                (* r (sin (deg-to-rad (* a w))))
                                (* r (cos (deg-to-rad (* a w))))
                                s 
                                :ink +black+))))))))
                            

(define-presentation-type capacity-label-view ())

(define-presentation-method presentation-typep (object (type capacity-label-view))
  (typep object 'petri-net-item-with-capacity))

(define-presentation-method present :after (place (type place-with-capacity-view)
                                                  stream
                                                  (view gadget-view) &key)
  (with-application-frame (frame)  
    (multiple-value-bind (radius)
        (get-dimensions place)  

      (updating-output (stream :unique-id (list place (capacity place))
                               :id-test #'equal
                               :cache-value  (list (scaling-factor frame)
                                                   (x place) (x place) 
                                                   (object-color place)
                                                   (capacity place))
                               :cache-test #'equal)
        (with-output-as-presentation (stream place 'capacity-label-view)
          (draw-text* stream (format nil "~A" (capacity place)) radius radius))))))


(define-presentation-method present (transition (type transition-view) stream
                                                (view gadget-view) &key)
  (with-application-frame (frame)  
    (multiple-value-bind (width height)
        (get-dimensions transition)
      (updating-output (stream :unique-id transition
                               :cache-value (list (activated-p transition)
                                                  (scaling-factor frame)
                                                  (x transition) (x transition) 
                                                  (object-color transition))
                               :cache-test #'equal)
  
        (draw-rectangle* stream (- width) (- height) width height 
                         :filled (activated-p transition))))))


(define-presentation-method present (edge (type edge-view) stream
                                          (view gadget-view) &key)  
  (with-application-frame (frame) 
    (let ((from (from edge))
          (to (to edge)))
      (updating-output (stream :unique-id edge
                               :cache-value (list (scaling-factor frame)
                                                  (x from) (x to) 
                                                  (y from) (y to) 
                                                  (object-color edge))
                               :cache-test #'equal)
        (draw-arrow* stream
                     (x from) (y from)
                     (x to) (y to)
                     :line-thickness 2
                     :head-length (/ 0.03 (scaling-factor frame))
                     :head-width (/ 0.03 (scaling-factor frame)))))))

(define-presentation-method present :after (edge (type edge-with-capacity-view) 
                                                 stream
                                                 (view gadget-view) &key)
  (with-application-frame (frame) 
    (let ((from (from edge))
          (to (to edge)))
      (updating-output (stream :unique-id (list edge (capacity edge))
                               :id-test #'equal
                               :cache-value (list (scaling-factor frame) 
                                                  (x from) (x to)
                                                  (y from) (y to) 
                                                  (object-color edge)
                                                  (capacity edge))
                               :cache-test #'equal)
        (with-output-as-presentation (stream edge 'capacity-label-view)
          (draw-text* stream
                      (format nil "~A" (capacity edge))
                      (/ (+ (x from) (x to)) 2)
                      (/ (+ (y from) (y to)) 2)))))))

;;;
;;; Define some gesture names
;;;

(define-gesture-name :new-place :pointer-button :left)

(define-gesture-name :new-transition :pointer-button (:left :shift))

(define-gesture-name :delete :pointer-button :left)

(define-gesture-name :activate :pointer-button (:right :shift))

(define-gesture-name :move :pointer-button (:control :left))

(define-gesture-name :add-token :pointer-button :middle)

(define-gesture-name :remove-token :pointer-button (:middle :shift))

(define-gesture-name :add-capacity-label :pointer-button (:middle :control))


;;;
;;; Define some editor commands 
;;;

(define-petri-net-editor-command (com-new-transition 
                                  :menu nil
                                  :name "New Transition")
    ((x 'integer) (y 'integer))
  (with-application-frame (frame) 
    (multiple-value-bind (x y)
        (get-relative-coordinates frame x y)
      (make-transition frame :x x :y y))))

(define-petri-net-editor-command (com-new-place 
                                  :menu nil
                                  :name nil)
    ((x 'integer) (y 'integer))
  (with-application-frame (frame) 
    (multiple-value-bind (x y)
        (get-relative-coordinates frame x y)
      (make-place frame :x x :y y))))

;;;
;;;

(define-petri-net-editor-command (com-link-place-with-transition
                                  :menu nil
                                  :name "Link Place with Transition")
    ((place 'place-view) (transition 'transition-view))
  (link place transition))


(define-petri-net-editor-command (com-link-transition-with-place  
                                  :menu nil
                                  :name "Link Transition with Place")
    ((transition 'transition-view) (place 'place-view))
  (link transition place))

;;;
;;;
;;;


(define-petri-net-editor-command (com-unlink-place-and-transition
                                  :menu nil
                                  :name "Unlink Place and Transition")
    ((place 'place-view) (transition 'transition-view))
  (unlink place transition))


(define-petri-net-editor-command (com-unlink-transition-and-place  
                                  :menu nil
                                  :name "Unlink Transition and Place")
    ((transition 'transition-view) (place 'place-view))
  (unlink transition place))

;;;
;;;
;;;

(define-petri-net-editor-command (com-delete-object  
                                  :menu nil
                                  :name "Delete Object")
    ((object 'display-object))
  (remove-from-net object))

;;;
;;;
;;;
;;;


(define-petri-net-editor-command (com-add-token
                                  :menu nil
                                  :name "Add Token")
    ((place-with-net-tokens `(and place-with-net-tokens-view 
                              (satisfies may-have-more-net-tokens-p))))
  (add-net-tokens place-with-net-tokens))


(define-petri-net-editor-command (com-remove-token
                                  :menu nil
                                  :name "Remove Token")
    ((place-with-net-tokens `(and place-with-net-tokens-view
                              (satisfies ,#'(lambda (place)
                                              (not (zerop (net-tokens place))))))))
  (remove-net-tokens place-with-net-tokens))


;;;
;;;
;;;


(define-petri-net-editor-command (com-add-capacity-label
                                  :menu nil
                                  :name "Add Capacity Label")
    ((place-with-net-tokens
      `(and place-with-net-tokens-view
            (satisfies ,#'(lambda (object)
                            (not (typep object 
                                        'place-with-capacity-view))))))
     (capacity 'integer))
  (apply #'change-class place-with-net-tokens 'place-with-capacity-view 
         :capacity capacity nil))

;;;
;;;
;;;

(define-petri-net-editor-command (com-increase-capacity
                                  :menu nil
                                  :name "Increase Capacity")
    ((capacity-label 'capacity-label-view))
  (increase-capacity capacity-label 1))


(define-petri-net-editor-command (com-decrease-capacity
                                  :menu nil
                                  :name "Decrease Capacity")
    ((capacity-label `(and capacity-label-view
                           (satisfies ,#'(lambda (object)
                                           (not (zerop (1- (capacity object)))))))))
  (decrease-capacity capacity-label 1))


;;;
;;;
;;;

(define-petri-net-editor-command (com-activate-transition
                                  :menu nil
                                  :name "Activate Transition")
    ((transition 'transition-view))
  (activate transition))

;;;
;;;
;;;

(define-petri-net-editor-command (com-move-display-object  
                                  :menu nil
                                  :name "Move Display Object")
    ((object 'positioned-display-object))
  (with-application-frame (frame)    
    (let ((ox (x object))
          (oy (y object))
          (stream (get-frame-pane frame 'display)))
      (tracking-pointer (stream)
        (:pointer-motion (x y)     
         (multiple-value-bind (x y)
             (get-relative-coordinates frame x y)
           (setf (x object) x
                 (y object) y)
           (redisplay-frame-pane frame stream)))
        (:pointer-button-press (event)
         (when (= (pointer-event-button event)
                  +pointer-right-button+)
           (setf (x object) ox
                 (y object) oy))
         (return))))))


(define-petri-net-editor-command (com-new-transition-no-arguments
                                  :menu nil
                                  :name "New Transition")
    nil
  (with-application-frame (frame)
    (let ((stream (get-frame-pane frame 'display)))
      (multiple-value-bind (x y)
          (tracking-pointer (stream)
            (:pointer-button-press (x y event)
             (when (= (pointer-event-button event)
                      +pointer-left-button+)
               (return
                (values x y)))))
        (com-new-transition x y)))))


(define-petri-net-editor-command (com-new-place-no-arguments
                                  :menu nil
                                  :name "New Place")
    nil
  (with-application-frame (frame)
    (let ((stream (get-frame-pane frame 'display)))
      (multiple-value-bind (x y)
          (tracking-pointer (stream)
            (:pointer-button-press (x y event)
             (when (= (pointer-event-button event)
                      +pointer-left-button+)
               (return
                (values x y)))))
        (com-new-place x y)))))

;;;
;;; Define some presentation translators
;;;

(define-presentation-to-command-translator move-display-object
    (positioned-display-object com-move-display-object petri-net-editor
		               :gesture :move
		               :documentation ((stream) (format stream "Move This Object"))
		               :echo t :maintain-history nil)
    (object)
  (list object))


(define-presentation-to-command-translator delete-object
    (display-object com-delete-object petri-net-editor
                    :gesture :delete
                    :documentation ((stream) (format stream "Delete This Object"))
                    :echo nil :maintain-history nil)
    (object)
  (list object))

(define-presentation-to-command-translator new-transition
    (blank-area com-new-transition petri-net-editor
		:gesture :new-transition
		:documentation ((stream) (format stream "Create New Transition"))
		:echo nil :maintain-history nil)
    (x y)
  (list x y))


(define-presentation-to-command-translator new-place
    (blank-area com-new-place petri-net-editor
		:gesture :new-place
		:documentation ((stream) (format stream "Create New Place"))
		:echo nil :maintain-history nil)
    (x y)
  (list x y))


(define-presentation-to-command-translator add-token
    (place-with-net-tokens-view com-add-token petri-net-editor
		            :gesture :add-token
                            :tester ((object) (may-have-more-net-tokens-p object))
		            :documentation ((stream) (format stream "Add a Token"))
		            :echo t :maintain-history nil)
    (object)
  (list object))

(define-presentation-to-command-translator remove-token
    (place-with-net-tokens-view com-remove-token petri-net-editor
		            :gesture :remove-token
                            :tester ((object)
                                     (not (zerop (net-tokens object))))
		            :documentation ((stream) (format stream "Remove a Token"))
		            :echo t :maintain-history nil)
    (object)
  (list object))


(define-presentation-to-command-translator add-capacity-label
    (place-with-net-tokens-view com-add-capacity-label petri-net-editor
		            :gesture :add-capacity-label
                            :tester ((object) (not (typep object 'place-with-capacity-view)))
		            :documentation ((stream) (format stream "Add a Capacity Label"))
		            :echo t :maintain-history nil)
    (object)
  (list object 4))



(define-presentation-to-command-translator increase-capacity
    (capacity-label-view com-increase-capacity petri-net-editor
		         :gesture :add-token
		         :documentation ((stream) (format stream "Increase Capacity"))
		         :echo t :maintain-history t)
    (object)
  (list object))

(define-presentation-to-command-translator decrease-capacity
    (capacity-label-view com-decrease-capacity petri-net-editor
		         :gesture :remove-token
                         :tester ((object)
                                  (not (zerop (1- (capacity object)))))
		         :documentation ((stream) (format stream "Decrease Capacity"))
		         :echo t :maintain-history nil)
    (object)
  (list object))


(define-presentation-to-command-translator activate-transition
    (transition-view com-activate-transition petri-net-editor
                     :gesture :activate
                     :tester ((object)
                              (activated-p object))
                     :documentation ((stream) (format stream "Activate Transition"))
                     :echo t :maintain-history nil)
    (object)
  (list object))

;;;
;;; Define the command table
;;;

(define-command-table command-table
                      :menu (("New Transition" :command (com-new-transition-no-arguments))
                             ("New Place" :command (com-new-place-no-arguments))
                             ("Link Place with Transtion" :command (com-link-place-with-transition))
                             ("Link Transtion with Place" :command (com-link-transition-with-place))
	                     ("divide1" :divider nil)
                             ("Delete Object" :command (com-delete-object))
                             ("divide2" :divider nil)
                             ("Add Capacity Label" :command (com-add-capacity-label))
                             ("Increase Capacity" :command (com-increase-capacity))
                             ("Decrease Capacity" :command (com-decrease-capacity))
                             ("divide3" :divider nil)
                             ("Add Token" :command (com-add-token))
                             ("Remove Toke" :command (com-remove-token))
                             ("divide4" :divider nil)
                             ("Activate Transition" :command (com-activate-transition))))
           
;;;
;;; Run the application
;;;

(defun petri ()
  (setf *application-frame*
        (make-application-frame 'petri-net-editor
          :width 700
          :height 700))  
  (run-frame-top-level *application-frame*))



(petri) 

