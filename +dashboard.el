;;; +dashboard.el -*- lexical-binding: t; -*-


(defun my-dashboard-widget-simple-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                                'face 'doom-dashboard-banner) " ")
            (insert "\n"))
          '(
               "_-`````-,           ,- '- ."
  ".'   .- - |          | - -.  `."
 "/.'  /                     `.   \\"
":/   :      _...   ..._      ``   :"
"::   :     /._ .`:'_.._\\.    ||   :"
"::    `._ ./  ,`  :    \\ . _.''   ."
"`:.      /   |  -.  \\-. \\\\_      /"
 " \\:._ _/  .'   .@)  \\@) ` `\\ ,.'"
  "   _/,--'       .- .\\,-.`--`."
       ",'/''     (( \\ `  )    "
        "/'/'  \\    `-'  (     "
         "'/''  `._,-----'"
          "''/'    .,---'"
           "''/'      ;:"
             "''/''  ''/"
               "''/''/''"
                 "'/'/'"
                  "`;"

            ))
    ))

(setq +doom-dashboard-functions
      '(
        ;; doom-dashboard-widget-banner
        my-dashboard-widget-simple-banner
        doom-dashboard-widget-loaded
        ;; doom-dashboard-widget-footer
        )
      ;; +doom-dashboard-banner-file "EmacsIcon.svg"
      ;; +doom-dashboard-banner-dir doom-private-dir
      )

(add-hook '+doom-dashboard-mode-hook (lambda ()
                                       (hl-line-mode -1)))
