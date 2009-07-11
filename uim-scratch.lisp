(in-package :mcclim-uim)

(define-application-frame im-scratch-frame ()
  ((preedit-frame :accessor preedit-frame))
  (:menu-bar t)
  (:panes
   (app-pane :application :width 300 :height 100
             :display-function 'im-scratch-display)
   (interactor :interactor :width 300 :height 200))
  (:layouts (default (vertically () app-pane interactor)))
  (:geometry :top 500 :left 600))

(defun im-scratch-display (frame pane)
  (declare (ignore frame))
  (let ((preedit (make-instance 'preedit)))
    (with-accessors ((pushbacked pushbacked)) preedit
      (push (cons +preedit-attr-none+ "あ") pushbacked)
      (push (cons +preedit-attr-cursor+ "い") pushbacked)
      (push (cons +preedit-attr-separator+ "う") pushbacked))
    (present preedit 'preedit :stream pane)))

(define-im-scratch-frame-command (com-mes :name t :menu t) ((mes 'string))
  (notify-user *application-frame* mes))

(define-im-scratch-frame-command (com-open-window :name t :menu t) ()
  (open-window-stream
   :width 200 :height 25
   :top 500 :left 400
   :label "uim"
   :borders nil
   :initial-cursor-visibility :on
   ;; input-buffer を指定するともとの frame にイベントが行く。
   :input-buffer (climi::frame-event-queue *application-frame*)
   :scroll-bars nil)
  ;; raise-frame は効果なし。
  (raise-frame *application-frame*))


(defclass preedit-frame (climi::menu-frame)
  ((preedit :initarg :preeidt :initform nil :accessor preedit)))

(defclass my-pane (clim-stream-pane) ())

(defvar *baku* "ばくばっくん")
(defmethod handle-repaint ((pane my-pane) region)
  ;;(break)
  ;;(call-next-method)
  (format pane *baku*))

(defvar *my-pane*)

(define-im-scratch-frame-command (com-popup :name t :menu t) ()
  (let* ((frame *application-frame*)
         (manager (frame-manager *application-frame*)))
    (with-look-and-feel-realization (manager frame)
      (let* ((pane1 (make-pane
                     'my-pane
                     :width 100 :height 20
                     :background +yellow+))
             (top-pane (outlining (:thickness 1) pane1))
             (p-f (make-instance 'preedit-frame
                                 :panes top-pane
                                 :left 450 :top 550)))
        (setf *my-pane* pane1)
        (setf (preedit-frame frame) p-f)
        (adopt-frame manager p-f)))))

(define-im-scratch-frame-command (com-clear :name t :menu t) ()
  (disown-frame (frame-manager *application-frame*)
                (preedit-frame *application-frame*)))

(define-im-scratch-frame-command (com-up :name t :menu t) ((message 'string))
  (setf *baku* message)
  (handle-repaint *my-pane* (sheet-region *my-pane*)))

#+nil
(run-frame-top-level (make-application-frame 'im-scratch-frame))