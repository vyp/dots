(setf *inner-gaps-size* 3
      *outer-gaps-size* 3)

(defun home-path (path)
  (pathname-as-directory (concatenate 'string (getenv "HOME") "/" path)))

(load (home-path "env/vendor/stumpwm/swm-gaps/swm-gaps.lisp"))

(in-package :stumpwm)

(setf *maxsize-border-width*   0
      *normal-border-width*    3
      *transient-border-width* 0
      *window-border-style*    :tight
      *mouse-focus-policy*     :sloppy)

;; Doesn't work. :(
;; TODO: Figure out why and fix.
; (set-module-dir (home-path "env/vendor/stumpwm"))
; (load-module "swm-gaps")

(run-commands "toggle-gaps")

;; Remove all default keybindings, assuming there's no available F21 key.
(set-prefix-key (kbd "F21"))
(undefine-key *top-map* (kbd "C-t"))

; (set-prefix-key (kbd "F20"))
;  
; (defmacro fill-keymap (map &rest bindings)
;   "Wipes map and adds list of bindings to it."
;   `(setf ,map
; 	(let ((m (make-sparse-keymap)))
; 	  ,@(loop for i = bindings then (cddr i)
; 		  while i
; 		  collect `(define-key m ,(first i) ,(second i)))
; 	  m)))
;
; (define-interactive-keymap imove-window nil
;   ((kbd "h") "move-window left")
;   ((kbd "j") "move-window down")
;   ((kbd "k") "move-window up")
;   ((kbd "l") "move-window right"))
; 
; (defcommand fullscreen-without-gaps () ()
;   (run-commands "toggle-gaps" "fullscreen"))
; 
; (fill-keymap *top-map*
;   (kbd "s-s")       "hsplit"
;   (kbd "s-d")       "vsplit"
;   (kbd "s-g")       "remove"
;   (kbd "s-h")       "move-focus left"
;   (kbd "s-j")       "move-focus down"
;   (kbd "s-k")       "move-focus up"
;   (kbd "s-l")       "move-focus right"
;   (kbd "s-TAB")     "other-in-frame"
;   (kbd "s-SPC")     "next-in-frame"
;   (kbd "s-;")       "next"
;   (kbd "s-,")       "prev"
;   (kbd "s-w")       "fselect"
;   (kbd "s-u")       "next-urgent"
;   (kbd "s-H")       "exchange-direction left"
;   (kbd "s-J")       "exchange-direction down"
;   (kbd "s-K")       "exchange-direction up"
;   (kbd "s-L")       "exchange-direction right"
;   (kbd "s-r")       "iresize"
;   (kbd "s-b")       "balance-frames"
;   (kbd "s-x")       "delete-window"
;   (kbd "s-Super-R") "exec xterm"
;   (kbd "s-t")       "exec xterm"
;   (kbd "s-c")       "exec st"
;   ; (kbd "s-e")       "emacs"
;   )
; 
; (define-key *top-map* (kbd "s-m") "imove-window")
; (define-key *top-map* (kbd "s-f") "fullscreen-without-gaps")
; 
; (fill-keymap *root-map*
;   (kbd "F20") "other-window"
;   (kbd "b")   "banish"
;   (kbd "m")   "lastmsg"
;   (kbd "x")   "kill-window")

; ----------------------------------------------------------------------------

; (define-key *top-map* (kbd "s-Super_R") "exec xterm")
; (define-key *top-map* (kbd "s-c") "exec st")

; (defun key-press-msg (key seq cmd)
;   (declare (ignore key))
;   (let ((*message-window-gravity* :top-right))
;     (message "~A" (print-key-seq (reverse seq)))))
; 
; (add-hook *key-press-hook* 'key-press-msg)

; (define-key *root-map* (kbd "c") "exec st")
; (set-prefix-key (kbd "F20"))
; (set-prefix-key (kbd "Menu"))
; (set-prefix-key (kbd "Super_L"))
; (define-key *top-map* (kbd "s-RET") "exec xterm")
; (undefine-key *root-map* (kbd "x"))
; (define-key *top-map* (kbd "s-Super_R") "exec xterm")
; (define-key *top-map* (kbd "s-c") "exec st")
