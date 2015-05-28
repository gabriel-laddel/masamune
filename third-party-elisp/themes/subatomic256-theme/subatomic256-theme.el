;;; subatomic256-theme.el --- Fork of subatomic-theme for terminals.

;; Copyright 2012 John Olsson

;; Author: John Olsson <john@cryon.se>
;; Modified by: Chris Corbyn <chris@w3style.co.uk>
;; URL: https://github.com/cryon/subatomic256
;; Version: 20130620.1910
;; X-Original-Version: 1.3

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(deftheme subatomic256
  "subatomic256 emacs theme")

(let ((midnight          "#1c1c1c")
      (midnight-1        "#262626")
      (midnight-2        "#a8a8a8")
      (midnight-3        "#5f5f87")
      (mystic-blue       "#5f5f87")
      (victory-blue      "#87afaf")
      (victory-blue+1    "#87afd7")
      (jungle-green      "#afd75f")
      (undergrowth-green "#87af5f")
      (deep-gold         "#ffaf00")
      (axiomatic-purple  "#af5faf")
      (brick-red         "#d7875f")
      (piggy-pink        "#ffd7d7")
      (relaxed-white     "#d7d7d7")
      (cold-mud          "#d7afaf")

      (full-white        "#ffffff")
      (full-black        "#000000")
      (full-red          "#ff0000")
      (full-green        "#00ff00")
      (full-blue         "#0000ff")
      (full-yellow       "#ffff00")
      (full-magenta      "#ff00ff")
      (full-cyan         "#00ffff"))
  (custom-theme-set-faces
   'subatomic256

   ;; default stuff

   `(default
      ((t (:background ,midnight :foreground ,relaxed-white))))

   `(fringe
     ((t (:background ,midnight))))

   `(vertical-border
     ((t (:foreground ,midnight-2))))

   `(region
     ((t (:background ,mystic-blue :foreground ,full-white))))

   `(show-paren-match-face
     ((t (:foreground ,full-green :bold t))))

   `(show-paren-mismatch-face
     ((t (:foreground ,full-red :bold t))))

   `(isearch
     ((t (:background ,midnight-2 :foreground ,full-green :bold t))))

   `(lazy-highlight
     ((t (:background ,midnight-2 :foreground ,deep-gold :bold t))))

   `(query-replace
     ((t (:inherit lazy-highlight))))

   `(trailing-whitespace
     ((t (:inherit show-paren-mismatch-face :underline t))))

   `(mode-line
     ((t (:background ,midnight-3 :foreground ,full-white :weight bold
		      :box (:line-width 1 :style released-button)))))

   `(powerline-active1
     ((t (:background ,midnight-2))))

   `(powerline-active2
     ((t (:background ,midnight-1))))

   `(modeline-inactive
     ((t (:background ,midnight-2 :foreground ,mystic-blue))))

   `(powerline-inactive1
     ((t (:background ,midnight-2))))

   `(powerline-inactive2
     ((t (:background ,midnight-1))))

   `(header-line
     ((t (:background ,midnight-3 :foreground ,full-white :weight bold))))

   `(hl-line
     ((t (:background ,midnight-1))))

   `(highlight-current-line-face
     ((t (:inherit hl-line))))

   `(minibuffer-prompt
     ((t (:foreground ,axiomatic-purple :weight bold))))

   `(escape-glyph
     ((t (:foreground ,cold-mud :weight bold))))

   `(link
     ((t (:foreground ,victory-blue+1 :weight bold :underline t))))

   ;; font lock

   `(font-lock-keyword-face
     ((t (:foreground ,deep-gold :weight bold))))

   `(font-lock-function-name-face
     ((t (:foreground ,victory-blue))))

   `(font-lock-warning-face
     ((t ((:foreground ,brick-red)))))

   `(font-lock-builtin-face
     ((t (:foreground ,deep-gold))))

   `(font-lock-variable-name-face
     ((t (:foreground ,victory-blue))))

   `(font-lock-constant-face
     ((t (:foreground ,full-white, :weight bold :italic t))))

   `(font-lock-type-face
     ((t (:foreground ,victory-blue+1 :weight bold))))

   `(font-lock-negation-char-face
     ((t (:foreground ,brick-red :weight bold))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,cold-mud))))

   `(font-lock-comment-face
     ((t (:foreground ,mystic-blue))))

   `(font-lock-string-face
     ((t (:foreground ,jungle-green))))

   `(font-lock-comment-delimiter-face
     ((t (:foreground ,midnight-3))))

   `(font-lock-doc-face
     ((t (:foreground ,axiomatic-purple :italic t))))

   ;; flymake

   `(flymake-errline
     ((t (:underline ,full-red))))

   `(flymake-warnline
     ((t (:underline ,full-yellow))))

   ;; eshell

   `(eshell-ls-clutter
     ((t (:inherit font-lock-comment-face))))

   `(eshell-ls-executable
     ((t (:foreground ,jungle-green))))

   `(eshell-ls-directory
     ((t (:foreground ,victory-blue :bold t))))

   `(eshell-ls-archive
     ((t (:foreground ,deep-gold))))

   `(eshell-ls-backup
     ((t (:inherit font-lock-comment-face))))

   `(eshell-ls-missing
     ((t (:inherit font-lock-warning-face))))

   `(eshell-ls-unreadable
     ((t (:inherit font-lock-warning-face))))

   `(eshell-ls-symlink
     ((t (:inherit font-lock-builtin-face))))

   `(eshell-prompt
     ((t (:inherit minibuffer-prompt))))

   `(eshell-ls-backup
     ((t (:foreground ,brick-red :slant italic))))

   `(eshell-ls-product
     ((t (:inherit default :weight bold))))

   `(eshell-ls-readonly
     ((t (:inherit font-lock-comment))))

   `(eshell-ls-special
     ((t (:foreground ,cold-mud))))

   ;; calendar

   `(calendar-today-face
     ((t (:foreground ,jungle-green :bold t))))

   `(holiday-face
     ((t (:foreground ,brick-red))))

   `(diary-face
     ((t (:foreground ,axiomatic-purple))))

   ;; erc

   `(erc-default-face
     ((t (:inherit default))))

   `(erc-current-nick-face
     ((t (:inherit font-lock-keyword-face))))

   `(erc-action-face
     ((t (:foreground ,cold-mud))))

   `(erc-dangerous-host-face
     ((t (:inherit font-lock-warning-face))))

   `(erc-highlight-face
     ((t (:weight bold))))

   `(erc-direct-msg-face
     ((t (:foreground ,jungle-green))))

   `(erc-nick-msg-face
     ((t (:foreground ,victory-blue+1 :weight bold))))

   `(erc-fool-face
     ((t (:inherit font-lock-comment-face))))

   `(erc-input-face
     ((t (:inherit default :weight bold))))

   `(erc-error-face
     ((t (:inherit font-lock-warning-face))))

   `(erc-keyword-face
     ((t (:inherit font-lock-keyword-face))))

   `(erc-nick-default-face
     ((t (:inherit default))))

   `(erc-prompt-face
     ((t (:inherit eshell-prompt))))

   `(erc-notice-face
     ((t (:foreground ,axiomatic-purple))))

   `(erc-timestamp-face
     ((t (:inherit font-lock-comment-face))))

   `(erc-pal-face
     ((t (:foreground ,jungle-green))))

   ;; highlight-symbol

   `(highlight-symbol-face
     ((t (:background ,midnight-3))))

   ;; diff

   `(diff-file-header
     ((t (:background ,midnight :foreground ,victory-blue))))

   `(diff-header
     ((t (:inherit default :foreground ,mystic-blue))))

   `(diff-indicator-changed
     ((t (:foreground ,full-yellow :weight bold))))

   `(diff-changed
     ((t (:foreground ,deep-gold))))

   `(diff-indicator-removed
    ((t (:foreground ,full-red :weight bold))))

   `(diff-removed
     ((t (:foreground ,brick-red))))

   `(diff-indicator-added
     ((t (:foreground ,full-green :weight bold))))

   `(diff-added
     ((t (:foreground ,jungle-green))))

   `(diff-hunk-header
     ((t (:foreground ,full-white))))

   `(diff-refine-change
     ((t (:background ,midnight-3 :foreground ,full-white :weight bold))))


   ;; magit

   `(magit-branch
     ((t (:foreground ,jungle-green :weight bold))))

   `(magit-diff-add
     ((t (:inherit diff-added))))

   `(magit-diff-del
     ((t (:inherit diff-removed))))

   `(magit-diff-file-header
     ((t (:inherit diff-file-header))))

   `(magit-diff-hunk-header
     ((t (:inherit diff-hunk-header))))

   `(magit-diff-none
     ((t (:inherit default))))

   `(magit-header
     ((t (:inherit diff-header))))

   `(magit-item-highlight
     ((t (:background ,midnight-2))))

   `(magit-item-mark
      ((t (:background ,midnight-2))))

   `(magit-log-graph
     ((t (:foreground ,victory-blue))))

   `(magit-log-head-label-bisect-bad
     ((t (:foreground ,brick-red))))

   `(magit-log-head-label-bisect-good
     ((t (:foreground ,jungle-green))))

   `(magit-log-head-label-default
     ((t (:foreground ,axiomatic-purple :weight bold))))

   `(magit-log-head-label-local
     ((t (:inherit magit-log-head-label-default :foreground ,jungle-green))))

   `(magit-log-head-label-patches
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-head-label-remote
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-head-label-tags
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-message
     ((t (:inherit default))))

   `(magit-log-sha1
     ((t (:foreground ,deep-gold))))

   `(magit-section-title
     ((t (:inherit header-line))))

   `(magit-whitespace-warning-face
     ((t (:inherit font-lock-warning))))

   ;; compilation
   `(compilation-info
     ((t (:inherit default))))

   `(compilation-warning
     ((t (:inherit font-lock-warning))))

   ;; twittering-mode
   `(twittering-username-face
     ((t (:inherit font-lock-keyword-face))))

    `(twittering-uri-face
      ((t (:inherit link))))

    `(twittering-timeline-header-face
      ((t (:foreground ,cold-mud :weight bold))))

    `(twittering-timeline-footer-face
      ((t (:inherit twittering-timeline-header-face))))

    ;; outline
    `(outline-1
      ((t (:foreground ,deep-gold :weight bold))))

    `(outline-2
      ((t (:foreground ,jungle-green :weight bold))))

    `(outline-4
      ((t (:foreground ,relaxed-white :weight bold))))

    `(outline-3
      ((t (:foreground ,cold-mud :weight bold))))

    `(outline-5
      ((t (:foreground ,victory-blue+1 :weight bold))))

    `(outline-6
      ((t (:foreground ,axiomatic-purple :weight bold))))

    `(outline-7
      ((t (:foreground ,undergrowth-green :weight bold))))

    `(outline-8
      ((t (:foreground ,mystic-blue :weight bold))))

    ;; org-mode
    `(org-level-1
      ((t (:inherit outline-1))))

    `(org-level-2
      ((t (:inherit outline-2))))

    `(org-level-3
      ((t (:inherit outline-3))))

    `(org-level-4
      ((t (:inherit outline-4))))

    `(org-level-5
      ((t (:inherit outline-5))))

    `(org-level-6
      ((t (:inherit outline-6))))

    `(org-level-7
      ((t (:inherit outline-7))))

    `(org-level-8
      ((t (:inherit outline-8))))

    `(org-hide
      ((t (:foreground ,midnight))))   ;

    `(org-link
      ((t (:inherit link))))

    `(org-checkbox
      ((t (:background ,midnight :foreground ,full-white :weight bold
    		       :box (:line-width 1 :style released-button)))))

    `(org-done
      ((t (:foreground ,jungle-green :weight bold))))

    `(org-todo
      ((t (:foreground ,piggy-pink :weight bold))))

    `(org-table
      ((t (:foreground ,cold-mud))))

    `(org-date
      ((t (:foreground ,piggy-pink :weight bold))))

    `(org-document-info-keyword
      ((t (:foreground ,mystic-blue))))

    `(org-document-info
      ((t (:foreground ,cold-mud :weight bold :slant italic))))

    `(org-block-begin-line
     ((t (:background ,midnight-2 :foreground ,mystic-blue :weight bold))))

    `(org-block-background
      ((t (:background ,midnight-1))))

    `(org-block-end-line
      ((t (:inherit org-block-begin-line))))

    `(org-agenda-date-today
      ((t (:foreground ,jungle-green :background ,midnight-2 :weight bold))))

    `(org-agenda-date
      ((t (:foreground ,victory-blue+1))))

    `(org-agenda-date-weekend
      ((t (:foreground ,piggy-pink))))

    `(org-agenda-structure
      ((t (:inherit header-line))))

    `(org-warning
      ((t (:inherit font-lock-warning-face))))

    `(org-agenda-clocking
      ((t (:inherit org-date))))

    `(org-deadline-announce
      ((t (:inherit font-lock-warning-face))))

    `(org-formula
      ((t (:inherit font-lock-doc-face))))

    `(org-special-keyword
      ((t (:inherit font-lock-keyword))))

    ;; dired+

    `(diredp-compressed-file-suffix
      ((t (:foreground ,deep-gold :weight bold))))

    `(diredp-date-time
      ((t (:foreground ,mystic-blue))))

    `(diredp-deletion
      ((t (:foreground ,brick-red :weight bold :slant italic))))

    `(diredp-deletion-file-name
      ((t (:foreground ,brick-red :underline t))))

    `(diredp-symlink
      ((t (:foreground ,deep-gold))))

    `(diredp-dir-heading
      ((t (:inherit minibuffer-prompt))))

    `(diredp-display-msg
      ((t (:inherit default))))

    `(diredp-exec-priv
      ((t (:foreground ,jungle-green))))

    `(diredp-write-priv
      ((t (:foreground ,brick-red))))

    `(diredp-read-priv
      ((t (:foreground ,deep-gold))))

    `(diredp-dir-priv
      ((t (:foreground ,victory-blue+1 :weight bold))))

    `(diredp-link-priv
      ((t (:foreground ,deep-gold))))

    `(diredp-other-priv
      ((t (:foreground ,deep-gold :weight bold))))

    `(diredp-rare-priv
      ((t (:foreground ,brick-red :weight bold))))

    `(diredp-no-priv
      ((t (:foreground ,mystic-blue))))

    `(diredp-file-name
      ((t (:foreground ,relaxed-white))))

    `(diredp-file-suffix
      ((t (:inherit dired-file-name))))

    `(diredp-number
      ((t (:foreground ,victory-blue))))

    `(diredp-executable-tag
      ((t (:foreground ,jungle-green :weight bold))))

    `(diredp-flag-mark
      ((t (:foreground ,brick-red :weight bold))))

    `(diredp-flag-mark-line
      ((t (:background ,midnight-2))))

    `(diredp-mode-line-marked
      ((t (:foreground ,brick-red))))

    `(diredp-mode-line-flagged
      ((t (:foreground ,deep-gold))))

    `(diredp-ignored-file-name
      ((t (:foreground ,midnight-3))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'subatomic256)

;;; subatomic256-theme.el ends here
