#|

SBCL では ./src/runtime/runtime.c:230: setlocale(LC_ALL, ""); と
setlocale は処理系で実行済らいいので省略する。

uif-fep では uim_press_key のあとにすぐ uim_release_key をコールしてる。。。
  raw = uim_press_key(g_context, key, key_state);
  uim_release_key(g_context, key, key_state);

|#
(in-package :mcclim-uim)

(defconstant +preedit-attr-none+ 0)
(defconstant +preedit-attr-under-line+ 1)
(defconstant +preedit-attr-reverse+ 2)
(defconstant +preedit-attr-cursor+ 4)
(defconstant +preedit-attr-separator+ 8)

(defconstant +umod-shift+ 1)
(defconstant +umod-control+ 2)
(defconstant +umod-alt+ 4)
(defconstant +umod-meta+ 8)
(defconstant +umod-pseudo0+ 16)
(defconstant +umod-pseudo1+ 32)
(defconstant +umod-super+ 64)
(defconstant +umod-hyper+ 128)

(defconstant +ukey-escape+ 256)
(defconstant +ukey-tab+ 257)
(defconstant +ukey-backspace+ 258)
(defconstant +ukey-delete+ 259)
(defconstant +ukey-insert+ 260)
(defconstant +ukey-return+ 261)
(defconstant +ukey-left+ 262)
(defconstant +ukey-up+ 263)
(defconstant +ukey-right+ 264)
(defconstant +ukey-down+ 265)
(defconstant +ukey-prior+ 266)
(defconstant +ukey-next+ 267)
(defconstant +ukey-home+ 268)
(defconstant +ukey-end+ 269)

(cffi:load-foreign-library "libuim.so")

(cffi:defctype uim-context :pointer)
(cffi:defctype uim-candidate :pointer)

(cffi:defcfun "uim_init" :int)

(cffi:defcfun "uim_get_default_im_name" :string ; must not free
  (localename :string))

(cffi:defcfun "uim_create_context" uim-context
  (ptr :pointer)
  (enc :string)
  (lang :string)
  (engine :string)
  (conv :pointer)
  (commit_cb :pointer))

(cffi:defcfun "uim_release_context" :void
  (uc uim-context))

(cffi:defcfun "uim_focus_in_context" :void
  (uc uim-context))

(cffi:defcfun "uim_focus_out_context" :void
  (uc uim-context))

(cffi:defcfun "uim_place_context" :void
  (uc uim-context))

(cffi:defcfun "uim_displace_context" :void
  (uc uim-context))

(cffi:defcfun "uim_set_preedit_cb" :void
  (uc uim-context)
  (clear_cb :pointer)
  (pushback_cb :pointer)
  (update_cb :pointer))

(cffi:defcfun "uim_press_key" :int
  (uc uim-context)
  (key :int)
  (state :int))

(cffi:defcfun "uim_release_key" :int
  (uc uim-context)
  (key :int)
  (state :int))

(cffi:defcfun "uim_quit" :void)


(cffi:defcfun "uim_set_prop_list_update_cb" :void
  (uc uim-context)
  (update_cb :pointer)) ; void (*update_cb)(void *ptr, const char *str)


;;;; candidate
(cffi:defcfun "uim_set_candidate_selector_cb" :void
  (uc uim-context)
  (activate-cb :pointer)
  (select-cb :pointer)
  (shift-page-cb :pointer)
  (deactivate-cb :pointer))
(cffi:defcfun "uim_get_candidate" uim-candidate
  (uc uim-context)
  (inedx :int)
  (accel-enumeration-hint :int))
(cffi:defcfun "uim_candidate_free" :void
  (cand uim-candidate))
(cffi:defcfun "uim_get_candidate_index" :int
  (uc uim-context))
(cffi:defcfun "uim_set_candidate_index" :void
  (uc uim-context)
  (index :int))
(cffi:defcfun "uim_candidate_get_cand_str" :string ; must not free
  (cand uim-candidate))
(cffi:defcfun "uim_candidate_get_heading_label" :string ; must not free
  (cand uim-candidate))
(cffi:defcfun "uim_candidate_get_annotation_str" :string ; must not free
  (cand uim-candidate))

;;;; helper 現状は、使わない。
(defvar *helper-fd* nil)
(cffi:defcfun "uim_helper_init_client_fd" :int
  (disconnect_cb :pointer))            ; (void (*disconnect_cb)(void));
(cffi:defcfun "uim_helper_close_client_fd" :void
  (fd :int))
(cffi:defcfun "uim_helper_client_focus_in" :void
  (uc uim-context))
(cffi:defcfun "uim_helper_client_focus_out" :void
  (uc uim-context))
(cffi:defcfun "uim_helper_client_get_prop_list" :void)
(cffi:defcfun "uim_helper_read_proc" :void
  (fd :int))
(cffi:defcfun "uim_helper_get_message" :string) ; 未調査
(cffi:defcfun "uim_helper_send_message" :void
  (fd :int)
  (message :string))


#+nil                                   ; 以下、デバッグ用のコード
(progn
  (uim-init)

  (progn
    (cffi:defcallback commit-cb :void ((ptr :pointer) (commit-str :string))
      (format t "~&commit: ~a"  commit-str))
    (setf *uim-context*
          (uim-create-context
           (cffi-sys:null-pointer)
           "UTF-8"
           (cffi-sys:null-pointer)
           (uim-get-default-im-name "")
           (cffi-sys:null-pointer)
           (cffi:callback commit-cb))))

  (progn
    (let ((preedit ""))
      (cffi:defcallback clear-cb :void ((ptr :pointer))
        (format t "~&clear-cb: preedit => ~a" preedit)
        (setf preedit ""))
      (cffi:defcallback pushback-cb :void
          ((ptr :pointer) (attr :int) (str :string))
        (format t "~&pushback-cb attr: ~d, ~a, ~a" attr str
                (setf preedit (concatenate 'string preedit str))))
      (cffi:defcallback update-cb :void ((ptr :pointer))
        (format t "~&update-cb: preedit => ~a" preedit)
        ))
    (uim-set-preedit-cb *uim-context*
                        (cffi:callback clear-cb)
                        (cffi:callback pushback-cb)
                        (cffi:callback update-cb)))

  (progn
    (cffi:defcallback update-cb :void ((ptr :pointer) (str :string))
      (q:p 'update-cb ptr str))
    (uim-set-prop-list-update-cb *uim-context* (cffi:callback update-cb)))

  ;; candidate
  (progn
    (cffi:defcallback activate-cb :void
        ((ptr :pointer) (nr :int) (display-limit :int))
      (q:p 'activate-cb ptr nr display-limit))
    (cffi:defcallback select-cb :void
        ((ptr :pointer) (index :int))
      (q:p 'select-cb ptr index))
    (cffi:defcallback shift-page-cb :void
        ((ptr :pointer) (direction :int))
      (q:p 'shift-page-cb ptr int))
    (cffi:defcallback deactivate-cb :void ((ptr :pointer))
      (q:p 'deactivate-cb ptr))
    (uim-set-candidate-selector-cb *uim-context*
                                   (cffi:callback activate-cb)
                                   (cffi:callback select-cb)
                                   (cffi:callback shift-page-cb)
                                   (cffi:callback deactivate-cb)))
  ;;(uim-get-candidate-index *uim-context*)
  ;;(setf *uim-candidate* (uim-get-candidate *uim-context* 0 0))

  ;; 0 が戻ったら uim でキー入力が処理されたということ。
  (uim-press-key *uim-context* (char-code #\space) +umod-shift+)
  (uim-press-key *uim-context* (char-code #\a) 0)
  (uim-press-key *uim-context* (char-code #\A) 0)
  (uim-press-key *uim-context* (char-code #\space) 0)
  (uim-press-key *uim-context* (char-code #\j) +umod-control+)
  (uim-press-key *uim-context* (char-code #\space) +umod-shift+)

  ;; T-Code 用
  (uim-press-key *uim-context* (char-code #\space) +umod-shift+)
  (uim-press-key *uim-context* (char-code #\u) 0)
  (uim-press-key *uim-context* (char-code #\h) 0)
  (uim-press-key *uim-context* (char-code #\f) 0)
  (uim-press-key *uim-context* (char-code #\u) 0)
  (uim-press-key *uim-context* (char-code #\space) 0)
  (uim-press-key *uim-context* +ukey-return+ 0)



  ;; helper ???
  (cffi:defcallback disconnect-cb :void ()
    (print 'disconnect-cb))
  (setf *helper-fd* (uim-helper-init-client-fd
                     (cffi:callback disconnect-cb)))
  (uim-helper-send-message *helper-fd* "activate
charset=UTF-8
display_limit=3
a    bano

")
  (uim-helper-send-message *helper-fd* (format nil "im_list_get~%~%"))
  ;; read-proc してから get-message する。
  (uim-helper-read-proc *helper-fd*)
  (uim-helper-get-message)
  (uim-helper-send-message *helper-fd* (format nil "show~%~%"))
  (uim-helper-read-proc *helper-fd*)
  (uim-helper-get-message)

  (uim-helper-close-client-fd *helper-fd*)


  ;; 後処理
  (uim-candidate-free *uim-candidate*)
  (uim-release-context *uim-context*)
  (uim-quit)
  )
