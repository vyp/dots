(setq package-enable-at-startup nil)

;; Disable some gui elements.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Main monospace font.
(push '(font . "Iosevka-12") default-frame-alist)
