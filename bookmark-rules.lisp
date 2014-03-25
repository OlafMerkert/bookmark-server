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
              "Huey"
              ("UH-1" . "Huey")
              "Mustang"
              ("P-51" . "Mustang")
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
              "gnome"
              "program"
              "thinkpad"
              "javascript"
              ("jquery" . "javascript")
              "documentation"
              ("docs" . "documentation")
              "tutorial"
              "gimp"
              "inkscape"
              ("latex" . "TeX")
              ("texlive" . "TeX")
              "sketch"
              ("sql" . "database")
              ("plot" . "plotting")
              "radio"
              "Schlusslicht"
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

(defalias => create-category-logic)

(defpar category-logic
    (list (=> (or cat::|cliki| cat::|clim|) cat::|lisp|)
          (=> (or cat::|Klassik| cat::|Schlager| cat::|Pop|) cat::|Musik|)
          (=> cat::|Rowan Atkinson| cat::|funny|)
          (=> cat::|ImDB| cat::|Film|)
          (=> cat::|python| cat::|program|)
          (=> cat::|numpy| cat::|python|)
          (=> (or cat::|numpy| cat::|scipy|) cat::|numerics|)
          (=> (or cat::|fedora| cat::|debian| cat::|ubuntu|) cat::|linux|)
          (=> (and cat::|simulator| (or cat::|flug| cat::|racing|)) cat::|Spiel|)
          (=> cat::|joystick| cat::|Spiel|)
          (=> (and cat::|event| cat::|Musik|) cat::|Konzert|)
          ;; if there is a `not' criterion, it should come last
          (=> (and cat::|youtube| (not cat::|Musik|)) cat::|video|)
          (=> cat::|gtk| cat::|GUI|)))
