;;;; vibe-knight.asd
(defsystem "vibe-knight"
  :description "2D roguelike i Lisp med SDL2"
  :author "Jim Hansson"
  :license "GPL-3"
  :depends-on (:sdl2)
  :serial t
  :components (
    (:file "src/package")
    (:file "src/sdl2-wrapper")
    (:file "src/map-generator")
    (:file "src/player")
    (:file "src/enemy")
    (:file "src/game")
    (:file "src/objectives-graph")
    (:file "src/main")
    (:file "src/wfc")))
