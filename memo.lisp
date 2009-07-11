(in-package :clim-user)

(define-application-frame uim-frame ()
  ()
  (:panes (int :interactor :width 500 :height 500))
  (:layouts (default int)))

(define-application-frame preedit-frame ()
  ()
  (:menu-bar nil)
  (:panes (app (make-pane 'clim-stream-pane :width 300 :height 20)))
  (:layouts (default app)))

(defvar *preedit-frame*)
(define-uim-frame-command (com-show :menu t :name t) ()
  (setf *preedit-frame*
        (make-instance
         'preedit-frame
         :input-buffer (sheet-event-queue
                        (frame-top-level-sheet *application-frame*))))
  (adopt-frame (frame-manager *application-frame*) *preedit-frame*)
  (enable-frame *preedit-frame*))

(define-uim-frame-command (com-repaint :menu t :name t) ()
  (let ((pane (get-frame-pane *preedit-frame* 'app)))
    (format pane "DEF")))



;; frame mismatch in (SETF PORT-KEYBOARD-INPUT-FOCUS) が新しいフレーム
;; があるまま元のフレームをクリックした時に発生する。
;; :input-buffer の指定はあまり関係ないみたい。
(define-uim-frame-command (com-foo :menu t :name t) ()
  (let ((def (make-application-frame
              'preedit-frame
              :calling-frame *application-frame*)))
;;              :input-buffer (sheet-event-queue
;;                             (frame-top-level-sheet *application-frame*)))))
    (run-frame-top-level def)))

;; print-possibilities を参考にしてみたが、後で消す必要があるし、同じ pane に出すのはちょっと。。。
(define-uim-frame-command (com-ao :menu t :name t) ()
  (let ((stream (get-frame-pane *application-frame* 'int)))
    ;;(with-input-editor-typeout (stream :erase t)
    (surrounding-output-with-border (stream :shape :drop-shadow :background +cornsilk1+)
      (surrounding-output-with-border (stream :shape :rectangle)
        (format stream "Hello")))))

#+nil
(run-frame-top-level (make-application-frame 'uim-frame))