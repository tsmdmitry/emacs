;; Package Setup

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

(defvar local-packages
  '(
    solarized-theme
    elpy
    web-mode
    ))

(defun uninstalled-packages (packages)
  (delq nil
        (mapcar (lambda (p) (if (package-installed-p p nil) nil p)) packages)))

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
        (package-install p)))))


;; Настройка
;; ----------------------------------------------------------
(load-theme 'solarized-dark t)
(global-linum-mode t) ;; включить номера строк глобально
(setq inhibit-startup-message t) ;; скрыть стартовое сообщение
(setq line-number-mode t) ;; включить номера строк
(setq column-number-mode t) ;; включить номера столбцов
(menu-bar-mode 1) ;; включаем графическое меню
(tool-bar-mode -1) ;; выключаем tool-bar
(scroll-bar-mode -1) ;; выключаем scroll-bar
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

;; Отключить backup/автосохранение файлов
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)

;; Отображать имя текущего буфера в заголовке окна
(setq frame-title-format "GNU Emacs: %b")

;; Табуляция - 4 пробела
(setq tab-width 4)
(setq c-basic-indent 4)

;; Пробелы вместо табов
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Удаление лишних пробелов
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Шрифт
(set-default-font "Ubuntu Mono-12")
;; ---------------------------------------------------------


;; Python
(elpy-enable)
(custom-set-variables
 '(elpy-modules (quote
                 (elpy-module-company
                  elpy-module-eldoc
                  elpy-module-flymake
                  elpy-module-pyvenv
                  elpy-module-yasnippet
                  elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi"))

;; fill-column
(add-hook 'python-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook
          '(lambda() (set-fill-column 79)))


;; Web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist '(("django" . "\\.html\\'")))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-css-colorization t)
