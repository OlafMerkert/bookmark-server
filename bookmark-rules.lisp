(in-package :bookmarks)

(defpar title->category
    (mapcar #'cat-rule
            '("ARMA"
              "DCS"
              "lisp"
              "Warthog"
              ("A-10" . "Warthog")
              ("A10" . "Warthog")
              "Blackshark"
              ("black shark" . "Blackshark")
              ("Ka-50" . "Blackshark")
              "FSX"
              "test"
              ("review" . "test")
              "math"
              "Feynman"
              "algebra"
              "geometry"
              "manual"
              "News"
              ("Nachrichten" . "News")
              "bug"
              "Karl May"
              "Rowan Atkinson"
              "Comic"
              ("Cartoon" . "Comic")
              ("TV Series" . "TV")
              "dansk"
              ("Danmark" . "dansk")
              ("Dänisch" . "dansk")
              ("Dänemark" . "dansk")
              ("det nye talkshow" . "dansk")
              "simulator"
              ("simu" . "simulator")
              "python"
              "numpy"
              "scipy"
              "pyqt"
              "gtk"
              ("eclm" . "lisp")
              "clim"
              "blog"
              "proof"
              "wetter"
              ("meteo" . "wetter")
              ("prevision" . "wetter")
              "dict"
              ("wörterbuch" . "dict")
              "museum"
              ("museen" . "museum")
              "garten"
              "flug"
              ("flight" . "flug")
              ("flieger" . "flug")
              ("fighter jet" . "flug")
              ("gunship" . "heli")
              "heli"
              ("hubschrauber" . "heli")
              "airport"
              ("aeroporto" . "airport")
              ("flughafen" . "airport")
              "modell"
              ("model" . "modell")
              "buch"
              "fahrrad"
              ("bike" . "fahrrad")
              "git"
              "blender"
              "photoshop"
              "emacs"
              "howto"
              ("how to" . "howto")
              ("how-to" . "howto")
              "fedora"
              "debian"
              "arch"
              "windows"
              "linux"
              "gentoo"
              "ubuntu"
              "program"
              "thinkpad"
              "Mathematica"
              "javascript"
              ("jquery" . "javascript")
              "documentation"
              ("docs" . "documentation")
              )))

(defpar url->category
    (mapcar #'cat-rule
            '("SimHQ"
              "ImDB"
              "youtube"
              "wikipedia"
              "GitHub"
              "Unibas"
              "SNS"
              "gutenberg"
              "amazon"
              "geizhals"
              (".dk" . "dansk")
              ("digitalcombatsimulator" . "DCS")
              "forums"
              ("eagle.ru" . "DCS")
              ("bistudio.com" . "ARMA")
              "cliki"
              ("ted.com" . "TEDtalk")
              "lisp"
              ("blogspot" . "blog")
              "arxiv"
              "wetter"
              ("meteo" . "wetter")
              "math"
              "sourceforge"
              "emacswiki"
              "armaholic"
              "kickstarter"
              "twitter"
              )))

(defpar category-logic
    (list (create-category-logic cat::|cliki| cat::|lisp|)
          (create-category-logic cat::|clim| cat::|lisp|)
          (create-category-logic cat::|Klassik| cat::|Musik|)
          (create-category-logic cat::|Schlager| cat::|Musik|)
          (create-category-logic cat::|Pop| cat::|Musik|)
          (create-category-logic cat::|Rowan Atkinson| cat::|funny|)
          (create-category-logic cat::|ImDB| cat::|Film|)
          ;; note if there is a not criterion, it should come last
          (create-category-logic (and cat::|youtube| (not cat::|Musik|)) cat::|video|)))
