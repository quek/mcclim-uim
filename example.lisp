(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :mcclim-freetype)
  (require :mcclim-uim))

(in-package :clim-user)

(define-application-frame im-frame ()
  ()
  (:menu-bar t)
  (:panes (interactor :interactor :width 300 :height 200))
  (:layouts (default interactor))
  (:geometry :top 500 :left 600))

(define-im-frame-command (com-quit :menu t :name t) ()
  (frame-exit *application-frame*))

(defun run ()
  (run-frame-top-level (make-application-frame 'im-frame)))

;;(run)