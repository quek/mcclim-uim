(in-package :mcclim-uim)

(defvar *uim-encoding* "UTF-8")

(defvar *uim-im-name* nil)

(defvar *uim-context* nil)

(defvar *uim-candidate* nil)

(defclass uim-context ()
  ((event :initform nil)
   (context :initform nil)
   (preedits :initform nil)
   (preedit-pane :initform nil)
   (preedit-frame :initform nil)
   (preedit-range :initform nil)
   (candidates :initform nil)
   (candidate-page :initform 0)
   (candidate-display-limit :initform 0)))

(defmethod send-key-press-event ((uim-context uim-context) (char character))
  (let ((sheet (event-sheet (slot-value uim-context 'event))))
    (dispatch-event sheet
                    (make-instance 'key-press-event
                                   :sheet sheet
                                   :modifier-state 0
                                   :x 0
                                   :y 0
                                   :graft-x 0
                                   :graft-y 0
                                   :key-name nil
                                   :key-character char))))

(defmethod send-key-press-event ((uim-context uim-context) (str string))
  (loop for c across str
        do (send-key-press-event uim-context c)))

(defun preedit-cursor-p (preedit)
  (= (car preedit) +preedit-attr-cursor+))

(defmethod preedits-but-last-cursor ((uim-context uim-context))
  "末尾のカーソルを除去したい場合に使う。"
  (with-slots (preedits) uim-context
    (cond ((null preedits) nil)
          ((preedit-cursor-p (car preedits))
           (cdr preedits))
          (t preedits))))

(defun preedits-length (preedits)
  (loop for (attr . str) in preedits
        sum (length str)))

(defmethod get-preedit-text ((uim-context uim-context)
                             (sheet esa:minibuffer-pane))
  (let ((str (format nil "~{~a~}"
                     (mapcar (lambda (x)
                               (if (preedit-cursor-p x)
                                   #\*
                                   (cdr x)))
                             (reverse
                              (preedits-but-last-cursor uim-context))))))
    (if (string= "" str)
        nil
        str)))

(defun remove-annotation (string)
  (let ((pos (position #\; string)))
    (if pos
        (subseq string 0 pos)
        string)))

(defgeneric sheet-preedit-pushback (sheet attr str uim-context))

(defmethod sheet-preedit-pushback ((sheet sheet) attr str (uim-context uim-context))
  (with-slots (preedits) uim-context
    (push (cons attr str) preedits)))

(defgeneric sheet-preedit-update (sheet uim-context))

(defmethod sheet-preedit-update ((sheet esa:minibuffer-pane)
                                 (uim-context uim-context))
  (with-slots (preedit-range) uim-context
    (drei:with-bound-drei-special-variables ((pane-frame sheet))
      (let* ((text (get-preedit-text uim-context sheet))
             (begin (drei-buffer:offset (drei:point)))
             (end (+ begin (length text))))
        (when text
          (setf preedit-range (cons begin end))
          (drei-buffer:insert-buffer-sequence (esa-io:buffer (drei:point))
                                              begin
                                              text)
          (redisplay-frame-panes esa:*esa-instance*))))))

(defmethod sheet-preedit-update ((sheet sheet) (uim-context uim-context))
  (with-slots (preedit-range) uim-context
    (let ((offset 0))
      (mapc (lambda (x)
              (if (preedit-cursor-p x)
                  (progn
                    (send-key-press-event uim-context #\*)
                    (incf offset))
                  (progn
                    (send-key-press-event uim-context (cdr x))
                    (incf offset (length (cdr x))))))
            (reverse (preedits-but-last-cursor uim-context)))
      (setf preedit-range (cons 0 offset)))))

(defgeneric sheet-preedit-clear (sheet uim-context))

(defmethod sheet-preedit-clear ((sheet esa:minibuffer-pane)
                                (uim-context uim-context))
  (drei:with-bound-drei-special-variables ((pane-frame sheet))
    (with-slots (preedit-range) uim-context
      (when preedit-range
        (let ((begin (car preedit-range))
              (end (drei-buffer:make-buffer-mark (drei:buffer (drei:point))
                                                 (cdr preedit-range))))
        (drei-buffer:delete-region begin end)
        (setf preedit-range nil)
        (redisplay-frame-panes esa:*esa-instance*))))))

(defmethod sheet-preedit-clear ((sheet sheet) (uim-context uim-context))
  (with-slots (preedit-range) uim-context
    (when preedit-range
      (loop repeat (cdr preedit-range)
            do (send-key-press-event uim-context #\Backspace))
      (setf preedit-range nil))))

(defgeneric sheet-commit (sheet commit-str uim-context))

(defmethod sheet-commit ((sheet esa:minibuffer-pane)
                         (commit-str string)
                         (uim-context uim-context))
  (let ((commit-str (remove-annotation commit-str)))
    (drei:with-bound-drei-special-variables ((pane-frame sheet))
          (with-slots (preedit-range) uim-context
            (drei-buffer:insert-buffer-sequence
                         (esa-io:buffer (drei:point))
                         (if preedit-range
                             (car preedit-range)
                             (drei-buffer:offset (drei:point)))
                         commit-str)
            (when preedit-range
              (incf (car preedit-range) (length commit-str))
              (incf (cdr preedit-range) (length commit-str)))
            (redisplay-frame-panes esa:*esa-instance*)))))

(defmethod sheet-commit ((sheet sheet)
                         (commit-str string)
                         (uim-context uim-context))
  (sheet-preedit-clear sheet uim-context)
  (send-key-press-event uim-context commit-str))



(defmethod get-preedit-text ((uim-context uim-context)
                             (sheet sheet))
  (reverse (preedits-but-last-cursor uim-context)))

(defmethod preedit-update ((uim-context uim-context))
  (with-slots (event) uim-context
    (with-accessors ((sheet event-sheet)) event
      (sheet-preedit-update sheet uim-context))))

(defmethod preedit-clear ((uim-context uim-context))
  (with-slots (event preedits) uim-context
    (with-accessors ((sheet event-sheet)) event
      (sheet-preedit-clear sheet uim-context))
    (setf preedits nil)))


(defclass preedit-frame (climi::menu-frame) ())

(defclass preedit-pane (clim-stream-pane) ())

(define-presentation-type preedit ())

(defun present-preedit (attr str stream)
  (setf (medium-foreground stream)
        (cond ((/= 0 (logand attr +preedit-attr-reverse+))
               +blue+)
              (t +black+)))
  (write-string str stream))

(define-presentation-method present
    (uim-context (type preedit) stream view &key)
  (with-slots (preedits) uim-context
    (mapc (lambda (x)
            (present-preedit (car x) (cdr x) stream))
          (reverse preedits))))

(define-presentation-type candidate ())

(define-presentation-method present
    (uim-context (type candidate) stream view &key)
  (with-slots (candidates candidate-page) uim-context
    (format stream "~{~{~a~^:~}~^  ~}"
            (mapcar (lambda (x)
                      ;; #\; 以降はとって表示しましょう。横に長くなるから。
                      ;; リストボックス表示とかにしたら復活だね。
                      (list (car x)
                            (let* ((str (cadr x))
                                   (pos (position #\; str)))
                              (if pos
                                  (subseq str 0 pos)
                                  str))))
                    (nth candidate-page candidates)))))

(defmethod handle-repaint ((pane preedit-pane) region)
  (with-bounding-rectangle* (x1 y1 x2 y2) (bounding-rectangle region)
    (with-output-recording-options (pane :record nil)
      (draw-rectangle* pane x1 y1 x2 y2 :filled t :ink +background-ink+)))
  (setf (stream-cursor-position pane) (values 0 0))
  (with-slots (candidates) *uim-context*
    (if candidates
        (present *uim-context* 'candidate :stream pane)
        (present *uim-context* 'preedit :stream pane))))

(defun commit-cb-function (commit-str)
  (with-slots (event) *uim-context*
    (with-accessors ((sheet event-sheet)) event
      (sheet-commit sheet commit-str *uim-context*))))

(cffi:defcallback commit-cb :void ((ptr :pointer) (commit-str :string))
  (declare (ignore ptr))
  (commit-cb-function (remove-annotation commit-str)))

(defun preedit-clear-cb-function ()
  (preedit-clear *uim-context*))

(cffi:defcallback preedit-clear-cb :void ((ptr :pointer))
  (declare (ignore ptr))
  (preedit-clear-cb-function))

(defun preedit-pushback-cb-function (attr str)
  (with-slots (event) *uim-context*
    (with-accessors ((sheet event-sheet)) event
      (sheet-preedit-pushback sheet attr str *uim-context*))))

(cffi:defcallback preedit-pushback-cb :void
    ((ptr :pointer) (attr :int) (str :string))
  (declare (ignore ptr))
  (preedit-pushback-cb-function attr str))

(defun preedit-update-cb-function ()
  (preedit-update *uim-context*)
  (with-slots (preedit-frame preedit-pane candidates) *uim-context*
    (if candidates
        (if preedit-frame
            (handle-repaint preedit-pane (sheet-region preedit-pane))
            (make-preedit-frame *uim-context*))
        (when preedit-frame
          (delete-preedit-frame *uim-context*)))))

(defmethod make-preedit-frame ((uim-context uim-context))
  (with-slots (event preedit-frame preedit-pane) uim-context
    (let* ((sheet (event-sheet event))
           (frame (pane-frame sheet))
           (manager (frame-manager frame))
           (top-level-sheet (frame-top-level-sheet frame)))
      (with-look-and-feel-realization (manager frame)
        (setf preedit-pane (make-pane 'preedit-pane
                                      :width 300 :height 20))
        (with-bounding-rectangle* (cx0 cy0 cx1 cy1) top-level-sheet
          (multiple-value-bind (x0 y0 x1 y1)
              (multiple-value-call #'values
                (transform-position (sheet-delta-transformation
                                     top-level-sheet nil) cx0 cy0)
                (transform-position (sheet-delta-transformation
                                     top-level-sheet nil) cx1 cy1))
            (declare (ignore x1 y0))
            (setf preedit-frame
                  (make-instance 'preedit-frame
                                 :left x0
                                 :top y1
                                 :panes (outlining (:thickness 1)
                                          preedit-pane)))))
        (adopt-frame manager preedit-frame)))))

(defmethod delete-preedit-frame ((uim-context uim-context))
  (with-slots (event preedit-frame) uim-context
    (let* ((sheet (event-sheet event))
           (frame (pane-frame sheet))
           (manager (frame-manager frame)))
      (disown-frame manager preedit-frame)
      (setf preedit-frame nil))))

(cffi:defcallback preedit-update-cb :void ((ptr :pointer))
  (declare (ignore ptr))
  (preedit-update-cb-function))

(defmethod collect-candidate ((uim-context uim-context) nr display-limit)
  (with-slots (context candidates candidate-page candidate-display-limit) uim-context
    (setf candidates nil
          candidate-page 0
          candidate-display-limit display-limit)
    (loop for i from 0 below nr
       with collected = nil
       for accel-enumeration-hint = (if (zerop display-limit)
                                        i
                                        (mod i display-limit))
       do (let ((candidate (uim-get-candidate context
                                              i
                                              accel-enumeration-hint)))
            (unwind-protect
                 (push (list (uim-candidate-get-heading-label candidate)
                             (uim-candidate-get-cand-str candidate))
                       collected)
              (uim-candidate-free candidate)))
       if (= (1+ accel-enumeration-hint) display-limit)
       do (progn (push collected candidates)
                 (setf collected nil))
       finally (when collected (push collected candidates)))
    (setf candidates (mapcar #'nreverse (nreverse candidates)))))

(defun candidate-activate-cb-function (nr display-limit)
  (collect-candidate *uim-context* nr display-limit))

(cffi:defcallback candidate-activate-cb :void
    ((ptr :pointer) (nr :int) (display-limit :int))
  (declare (ignore ptr))
  (candidate-activate-cb-function nr display-limit))

(defun candidate-select-cb-function (index)
  (declare (ignore index)))

(cffi:defcallback candidate-select-cb :void
    ((ptr :pointer) (index :int))
  (declare (ignore ptr))
  (candidate-select-cb-function index))

(defun candidate-shift-page-cb-function (direction)
  (with-slots (context candidates candidate-page candidate-display-limit)
      *uim-context*
    (if (= 1 direction)                 ; next
        (setf candidate-page (min (1- (length candidates))
                                  (1+ candidate-page)))
        (setf candidate-page (max 0 (1- candidate-page))))
    (uim-set-candidate-index context
                             (* candidate-page candidate-display-limit))))

(cffi:defcallback candidate-shift-page-cb :void
    ((ptr :pointer) (direction :int))
  (declare (ignore ptr))
  (candidate-shift-page-cb-function direction))

(defun candidate-deactivate-cb-function ()
  (with-slots (candidates) *uim-context*
    (setf candidates nil)))

(cffi:defcallback candidate-deactivate-cb :void ((ptr :pointer))
  (declare (ignore ptr))
  (candidate-deactivate-cb-function))

#+nil
(trace candidate-activate-cb-function
       candidate-select-cb-function
       candidate-shift-page-cb-function
       candidate-deactivate-cb-function)

(defun initialize-uim ()
  (unless *uim-context*
    (uim-init)
    (setf *uim-context* (make-instance 'uim-context))
    (with-slots (context) *uim-context*
      (setf context (uim-create-context (cffi-sys:null-pointer)
                                        *uim-encoding*
                                        (cffi-sys:null-pointer)
                                        (or *uim-im-name*
                                            (uim-get-default-im-name ""))
                                        (cffi-sys:null-pointer)
                                        (cffi:callback commit-cb)))
      (uim-set-preedit-cb
       context
       (cffi:callback preedit-clear-cb)
       (cffi:callback preedit-pushback-cb)
       (cffi:callback preedit-update-cb))
      (uim-set-candidate-selector-cb
       context
       (cffi:callback candidate-activate-cb)
       (cffi:callback candidate-select-cb)
       (cffi:callback candidate-shift-page-cb)
       (cffi:callback candidate-deactivate-cb)))))

(defun dispose-uim ()
  (when *uim-context*
    (with-slots (context) *uim-context*
      (uim-release-context context))
    (setf *uim-context* nil)
    (uim-quit)))

(defun uim-modifier (clim-modifier)
  (case clim-modifier
    (#.clim-internals::+shift-key+ +umod-shift+)
    (#.clim-internals::+control-key+ +umod-control+)
    (#.clim-internals::+meta-key+ +umod-meta+)
    (#.clim-internals::+super-key+ +umod-super+)
    (#.clim-internals::+hyper-key+ +umod-hyper+)
    (#.clim-internals::+alt-key+ +umod-alt+)
    (t 0)))

(defun get-uim-key-code (event)
  (case (keyboard-event-key-name event)
    (:escape    +ukey-escape+)
    (:tab       +ukey-tab+)
    (:backspace +ukey-backspace+)
    (:delete    +ukey-delete+)
    (:insert    +ukey-insert+)
    (:return    +ukey-return+)
    (:left      +ukey-left+)
    (:up        +ukey-up+)
    (:right     +ukey-right+)
    (:down      +ukey-down+)
    (:prior     +ukey-prior+)
    (:next      +ukey-next+)
    (:home      +ukey-home+)
    (:end       +ukey-end+)
    (t (when (characterp (keyboard-event-character event))
         (char-code (keyboard-event-character event))))))

(defgeneric dispatch-event-to-uim (event))

(defmethod dispatch-event-to-uim :before ((event keyboard-event))
  (unless *uim-context*
    (initialize-uim)))

(defmethod dispatch-event-to-uim ((key-press-event key-press-event))
  (with-slots (event context) *uim-context*
    (setf event key-press-event)
    (let ((key (get-uim-key-code event))
          (mod (uim-modifier (event-modifier-state event))))
      (when key
        (zerop (uim-press-key context key mod))))))

(defmethod dispatch-event-to-uim ((event key-release-event))
  (with-slots (context) *uim-context*
    (let ((key (get-uim-key-code event))
          (mod (uim-modifier (event-modifier-state event))))
      (when key
        (zerop (uim-release-key context key mod))))))

(defmethod distribute-event ((port basic-port) (event keyboard-event))
  (when (not (dispatch-event-to-uim event))
    (dispatch-event (event-sheet event) event)))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 以下、デバッグ用のコード
(require :mcclim-uim)
(require :mcclim-freetype)

(define-application-frame im-frame ()
  ()
  (:menu-bar t)
  (:panes
   (app-pane :application :width 300 :height 100)
   (interactor :interactor :width 300 :height 200))
  (:layouts (default (vertically () app-pane interactor)))
  (:geometry :top 500 :left 600))

(define-im-frame-command (com-quit :menu t :name t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(run-frame-top-level (make-application-frame 'im-frame))
|#