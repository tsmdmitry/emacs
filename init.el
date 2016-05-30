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
    aurora-theme
    arjen-grey-theme
    elpy
    web-mode
    neotree
    magit
    ;
    rust-mode
    racer
    ;;
    go-autocomplete
    go-mode
    rainbow-delimiters
    ;;
    inf-ruby
    rvm
    robe
    yaml-mode
    ;;
    docker
    dockerfile-mode
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
;; (load-theme 'aurora t)
(load-theme 'arjen-grey t)
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

;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'nerd)
(setq neo-window-width 32) ;; ширина буфера neotree
(setq neo-smart-open t)
(setq neo-window-position 'right) ;; буфер neotree справа
;; ---------------------------------------------------------


;; Python
(elpy-enable)
(setq elpy-modules '(elpy-module-company
                     elpy-module-eldoc
                     elpy-module-flymake
                     elpy-module-pyvenv
                     elpy-module-yasnippet
                     elpy-module-sane-defaults))

(setq elpy-rpc-backend "jedi")


;; fill-column
(add-hook 'python-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook
          '(lambda() (set-fill-column 79)))


;; Web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist '(("django" . "\\.html\\'")))

(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-css-indent-offset 4)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-css-colorization t)


;; Rust
(setq racer-cmd "/home/dmitry/.cargo/bin/racer")
(setq racer-rust-src-path "/home/dmitry/src/rust/src/")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(setq company-tooltip-align-annotations t)


;; Golang
(require 'go-mode)

;; enable autocompletion
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode (require 'go-autocomplete))

;; enable rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook `go-mode-hook `rainbow-delimiters-mode)

;; run gofmt on save (really goimports which calls gofmt)
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)


;; Ruby
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'company-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

; с какими файлами ассоциировать web-mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
; подсвечивать текущий элемент
(setq web-mode-enable-current-element-highlight t)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; Docker
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
