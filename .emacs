(require 'color-theme)
(color-theme-initialize)
(color-theme-robin-hood)
(menu-bar-mode -1)

(require 'server)
(unless (server-running-p)
  (server-start))
(require 'clojure-mode)
