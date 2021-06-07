(TeX-add-style-hook
 "Coordenadas"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("geometry" "top=3cm" "bottom=2cm" "left=3cm" "right=2cm") ("ulem" "normalem") ("tcolorbox" "theorems" "skins") ("biblatex" "style=authoryear" "extrayear" "uniquename=init" "giveninits" "justify" "repeattitles" "doi=false" "isbn=false" "url=true" "maxcitenames=2" "natbib=true" "backend=biber") ("minted" "cache=false") ("hyperref" "linktocpage" "pdfstartview=FitH" "colorlinks" "linkcolor=blue" "anchorcolor=blue" "citecolor=blue" "filecolor=blue" "menucolor=blue" "urlcolor=blue")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "lmodern"
    "fontenc"
    "geometry"
    "graphicx"
    "longtable"
    "float"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "marvosym"
    "wasysym"
    "amssymb"
    "tcolorbox"
    "biblatex"
    "url"
    "minted"
    "hyperref"
    "attachfile"
    "setspace"
    "tikz")
   (LaTeX-add-labels
    "sec:orgb4a445d"
    "sec:orgc299fde"
    "sec:orgcbc5405"
    "sec:org02ee177"
    "sec:orgf1f94d6"
    "violentos"
    "sec:org2ae5444"
    "sec:orge2d5157"
    "sec:org6ec6c3a"
    "sec:org4340ccc"
    "sec:org8fcd006"
    "sec:org86e0de4"
    "sec:org6b9d9c9"
    "sec:org8e36b4d"
    "sec:org615eaae"
    "nao_violentos"
    "sec:org4e72193"
    "sec:org19d467e"
    "sec:orgbbe327a"
    "sec:orgdf5ba37"
    "sec:org6191368"
    "sec:org0c2015f"
    "sec:org7441037"
    "sec:org5d34b97"
    "sec:orgb88821a"
    "sec:org8644a9c"
    "sec:org15a001d"
    "sec:org116642f"
    "sec:org3ac47a1"
    "sec:orgb5c50ff"
    "sec:org5b665e2"
    "sec:org8489b73"
    "sec:orga607e20"
    "dicionario"
    "sec:orgd0ebcb9"
    "sec:orge002732"
    "sec:orgadee556"
    "sec:org0368d69"
    "sec:org91d96f4"
    "sec:org4f1ec1d"
    "sec:org7dced2b"
    "sec:org397cb7f"
    "sec:org2112217"
    "sec:org4dd040a"
    "sec:org0b361c2"
    "sec:org4f7d31c"))
 :latex)

