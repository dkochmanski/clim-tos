
(in-package #:asdf-user)

(defsystem #:franz-clim2
  :description "CLIM 2 implementation released by Franz Inc."
  :license "BSD-4-Clause"
  :depends-on (#:franz-clim2/core))

(defsystem #:franz-clim2/core
  :description "Core functionality (doesn't contain backends).")

(defsystem #:franz-clim2/postscript
  :description "Draw-only backend as defined in the specification.")

(defsystem #:franz-clim2/clx-backend
  :description "Reference backend for X-Window system.")

(defsystem #:franz-clim2/examples
  :description "Applications bundled in franz-clim2 repository.")
