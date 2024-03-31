;;;; stylesheet.lisp

(in-package :mushub)

(defparameter *stylesheet*
  `((@font-face
     :font-family    "'Fira Code'"
     :src            ,(concatenate 'string
                       "url('/static/fonts/FiraCode-Medium.woff2') format('woff2'),"
                       "url('/static/fonts/FiraCode-Medium.ttf') format('truetype')")
     :font-weight    "500"
     :font-style     "normal")
    (@font-face
     :font-family    "'MavenPro'"
     :src            ,(concatenate 'string
                       "url('/static/fonts/MavenPro-Medium.woff2') format('woff2'),"
                       "url('/static/fonts/MavenPro-Medium.ttf') format('truetype')")
     :font-weight    "normal"
     :font-style     "normal")
    ("body,html"
     :width          "100%"
     :height         "100%"
     :padding        "0pt"
     :margin         "0pt"
     :background     "#f5fcfb"
     :font-family    "'MavenPro',sans-serif"
     :font-size      "20px")
    ("a"        :color "#217867")
    ("a:hover"  :color "#103c33")
    ("a:active" :color "#051411")
    (".action-button"
     :padding        ".5em"
     :margin-bottom   "1em"
     :background     "#217867"
     :border-radius  "16px")
    ("div.center"
     :text-align     "center")
    ("input.action-button[type=submit]"
     :color          "#fff"
     :font-size      "1.25em"
     :cursor         "pointer"
     :border         "none"
     :text-transform "uppercase"
     :border-radius  ".5em")
    ("input.action-button[type=submit]:hover"
     :background     "#165044"
     :color          "#cfefe8")
    ("#wrapper"
     ;:max-width      "1200px"
     :width          "100%"
     :min-width      "700px"
     :margin         "0 auto"
     :padding-top    "16px")
    ("#header"
     :margin         "0px 16px 0px 16px"
     :background     "#2ca089"
     :text-align     "center"
     :-webkit-user-select "none"
     :user-select    "none"
     :border-radius  "16px 16px 0px 0px")
    ("#header img"
     :width          "80px"
     :margin         "8px")
    ("#subheader"
     :background     "#87decd"
     :font-size      "0px"
     :border-radius  "0px")
    ("#subheader input"
     :font-size      "20px"
     :padding        "8px"
     :margin         "16px 0px 14px 0px"
     :outline        "none")
    ("#subheader input[type=text]"
     :font-family    "'Fira Code'"
     :width          "36ch"
     :border         "solid 2px #fff"
     :background     "#fff"
     :border-radius  ".5em 0em 0em .5em")
    ("#subheader input[type=submit]"
     :background     "#217867"
     :border         "solid 2px #217867"
     :color          "#fff"
     :cursor         "pointer"
     :border-radius  "0em .5em .5em 0em")
    ("#subheader input[type=submit]:hover"
     :background     "#165044"
     :border         "solid 2px #165044")
    ("#content"
     :margin         "0px 16px 0px 16px"
     :border         "solid 2px #87decd" ; #cfefe8
     :background     "#fff"
     :height         "auto"
     :bottom         "-20px"
     :border-radius  "0px 0px 16px 16px"
     :padding        "0em 1em 0em 1em")
    ("#footer"
     :margin         "0px auto"
     :padding        "0em 1em 0em 1em"
     :text-align     "center"
     :width          "200px")
    ("#footer p"
     :padding        ".5em 0em .5em 0em"
     :margin         "0px"
     :border-radius  "0px 0px 16px 16px"
     :color          "#2ca089"
     ;:border         "solid 1px #87decd"
     :border-top     "0px"
     ;:background     "radial-gradient(#e4f7f5, #f5fcfb)"
     :background    "none"
     :font-size      "0.75em")
    (".upload-wrapper"
     :text-align     "center")
    (".upload-container"
     :display        "block"
     :width          "auto"
     :border         "2pt dashed #cfefe8"
     :background     "#f8fdfc"
     :border-radius  ".5em"
     :margin         "auto auto 1em auto"
     :vertical-align "middle"
     :text-align     "center"
     :color          "#165044")
    (".upload-container:hover"
     :cursor         "pointer"
     :background     "#f3fbf9")
    (".upload-container p"
     :font-size      "1.2em")
    ("#file"
     :display        "none")
    ("#file-upload h4"
     :font-size     "1.5em"
     :padding       "0em"
     :margin        ".75em 0em .75em 0em")
    ("table#credits-table"
     :border-collapse "collapse"
     :width "100%")
    ("table#credits-table tbody tr:nth-child(even)"
     :background     "#f0f0f0")
    ("table#credits-table thead tr th"
     :text-align    "left"
     :border-bottom "solid 2px #000")
    ("table#credits-table thead tr th"
     :background    "#87decd")
    ("table#credits-table thead tr th, table#credits-table tbody tr td"
     :padding       ".25em")
    ("form#project-form input[type=text]#title"
     :font-size "2em"
     :font-weight "bold"
     :width "100%"
     :border "0px"
     :margin-top "0em"
     :border-bottom "3px dashed #cfefe8")
    ("form#project-form input[type=text]:focus"
     :outline-width "0")
    (".no-select"
     :-webkit-user-select "none"
     :user-select "none")
    ("form#project-form #last-modified p"
     :color          "#217867"
     :font-size      ".7em"
     :text-align     "right"
     :margin         "0px"
     :padding        "0px")
    (".track p"
     :text-size      "0.75em"
     :margin         "0em"
     :padding        "0em 0em 1em 0em")
    (".error-text"
     :color          "#990000 !important"
     :font-weight    "bold")))
