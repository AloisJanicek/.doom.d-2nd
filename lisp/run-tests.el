;;; lisp/run-tests.el -*- lexical-binding: t; -*-

(defvar aj-modes-tests-alist '()
  "Contains alist specifying shell test tool per major mode.

  Car is a symbol representing major mode and cdr is plist where:

DIR represents directory from within should shell command be executed
and can be string, variable or lambda producing valid value for `default-directory'.

FN represents function launching shell command.
(typically `compile', `shell-command' or `async-shell-command')

CMD is a string representing shell command which will execute tests
(something like \"make test\" or \"ruby *_test.rb\")
")

;;;###autoload
(defun aj/run-some-code-test-tool ()
  "Run shell test tool specified per major modes in `aj-modes-tests-alist'.
If there is no associated entry present for current major mode, throw warning.
"
  (interactive)
  (let* ((item (cdr (assoc major-mode aj-modes-tests-alist )))
         (default-directory (if (eq (type-of (plist-get item :dir)) 'cons)
                                (funcall (plist-get item :dir))
                              (eval (plist-get item :dir))))
         (fn (plist-get item :fn))
         (cmd (plist-get item :cmd)))
    (if (and item fn cmd)
        (funcall fn cmd)
      (warn "%s isn't configured in `aj-modes-tests-alist'" major-mode))))

(add-to-list
 'aj-modes-tests-alist
 '(cfscript-mode . (:dir default-directory
                    :fn shell-command
                    :cmd "box task run TestRunner")))
(add-to-list
 'aj-modes-tests-alist
 '(c-mode . (:dir (lambda () (locate-dominating-file "." "makefile"))
             :fn async-shell-command
             :cmd "make test")))

(add-to-list
 'aj-modes-tests-alist
 '(c++-mode . (:dir default-directory
               :fn async-shell-command
               :cmd "make")))

(add-to-list
 'aj-modes-tests-alist
 '(java-mode . (:dir (lambda () (locate-dominating-file "." "build.gradle"))
                :fn async-shell-command
                :cmd "gradle --warning-mode none test")))
(add-to-list
 'aj-modes-tests-alist
 '(asm-mode . (:dir default-directory :fn async-shell-command :cmd "make")))

(add-to-list
 'aj-modes-tests-alist
 '(coffee-mode . (:dir default-directory
                  :fn shell-command
                  :cmd "jasmine-node --coffee *.spec.coffee")))

(add-to-list
 'aj-modes-tests-alist
 '(crystal-mode . (:dir (lambda () (locate-dominating-file "." "README.md"))
                   :fn shell-command
                   :cmd "crystal spec")))
(add-to-list
 'aj-modes-tests-alist
 '(csharp-mode . (:dir default-directory :fn shell-command :cmd "dotnet test")))

(advice-add #'aj/run-some-code-test-tool
            :after
            (lambda ()
              "When in csharp-mode, jump at the end of the output buffer."
              ;; for some reason can't use async buffers with "dotnet test"
              (when (eq major-mode 'csharp-mode)
                (with-current-buffer (get-buffer "*Shell Command Output*")
                  (goto-char (point-max))))))

(add-to-list
 'aj-modes-tests-alist
 '(groovy-mode . (:dir (lambda () (locate-dominating-file "." "build.gradle"))
                  :fn shell-command
                  :cmd "gradle --warning-mode none --info test")))

(advice-add #'aj/run-some-code-test-tool
            :after
            (lambda ()
              "When in groovy-mode, jump at the end of the output buffer."
              (when (eq major-mode 'groovy-mode)
                (with-current-buffer (get-buffer "*Shell Command Output*")
                  (goto-char (point-max))))))

(add-to-list
 'aj-modes-tests-alist
 '(haskell-mode . (:dir (lambda () (locate-dominating-file "." "stack.yaml"))
                   :fn async-shell-command
                   :cmd "stack test")))
(add-to-list
 'aj-modes-tests-alist
 '(js2-mode . (:dir (lambda () (locate-dominating-file "." "package.json"))
               :fn async-shell-command
               :cmd "npm test")))
(add-to-list
 'aj-modes-tests-alist
 '(julia-mode . (:dir default-directory
                 :fn async-shell-command
                 :cmd "julia runtests.jl")))
(add-to-list
 'aj-modes-tests-alist
 '(kotlin-mode . (:dir (lambda () (locate-dominating-file "." "gradlew"))
                  :fn shell-command
                  :cmd "gradle --warning-mode none test")))
(add-to-list
 'aj-modes-tests-alist
 '(lfe-mode . (:dir (lambda () (locate-dominating-file "." "Makefile"))
               :fn async-shell-command
               :cmd "make test")))
(add-to-list
 'aj-modes-tests-alist
 '(lua-mode . (:dir default-directory
               :fn async-shell-command
               :cmd "busted")))
(add-to-list
 'aj-modes-tests-alist
 '(nim-mode . (:dir default-directory
               :fn async-shell-command
               :cmd "nim c -r *_test.nim")))
(add-to-list
 'aj-modes-tests-alist
 '(perl-mode . (:dir default-directory
                :fn async-shell-command
                :cmd "prove *.t")))
(add-to-list
 'aj-modes-tests-alist
 '(php-mode . (:dir default-directory
               :fn async-shell-command
               :cmd "phpunit *_test.php")))
(add-to-list
 'aj-modes-tests-alist
 '(prolog-mode . (:dir default-directory
                  :fn async-shell-command
                  :cmd "swipl -f *.pl -s *.plt -g run_tests,halt -t 'halt(1)'")))
(add-to-list
 'aj-modes-tests-alist
 '(purescript-mode . (:dir (lambda () (locate-dominating-file "." "bower.json"))
                      :fn async-shell-command
                      :cmd "pulp test --no-check-main")))
(add-to-list
 'aj-modes-tests-alist
 '(raku-mode . (:dir default-directory
                :fn async-shell-command
                :cmd "raku *.t6")))
(add-to-list
 'aj-modes-tests-alist
 '(ruby-mode . (:dir default-directory
                :fn async-shell-command
                :cmd "ruby *_test.rb")))
(add-to-list
 'aj-modes-tests-alist
 '(racket-mode . (:dir default-directory
                  :fn async-shell-command
                  :cmd "raco test *-test.rkt")))
(add-to-list
 'aj-modes-tests-alist
 '(d-mode . (:dir (lambda () (locate-dominating-file "." "dub.sdl"))
             :fn shell-command
             :cmd "dub test")))
(add-to-list
 'aj-modes-tests-alist
 '(erlang-mode . (:dir (lambda () (locate-dominating-file "." "rebar.config"))
                  :fn async-shell-command
                  :cmd "rebar3 eunit")))
(advice-add #'aj/run-some-code-test-tool
            :after
            (lambda ()
              "When in erlang-mode, jump at the end of the output buffer."
              (when (eq major-mode 'erlang-mode)
                (with-current-buffer (get-buffer "*Async Shell Command*")
                  (goto-char (point-max))))))
(add-to-list
 'aj-modes-tests-alist
 '(ess-r-mode . (:dir default-directory
                 :fn async-shell-command
                 :cmd "Rscript test*")))
(add-to-list
 'aj-modes-tests-alist
 '(fsharp-mode . (:dir (lambda () (locate-dominating-file "." "README.md"))
                  :fn shell-command
                  :cmd "dotnet test")))
(advice-add #'aj/run-some-code-test-tool
            :after
            (lambda ()
              "When in fsharp-mode, jump at the end of the output buffer."
              (when (eq major-mode 'fsharp-mode)
                (with-current-buffer (get-buffer "*Shell Command Output*")
                  (goto-char (point-max))))))
(add-to-list
 'aj-modes-tests-alist
 '(reason-mode . (:dir (lambda () (locate-dominating-file "." "package.json"))
                  :fn async-shell-command
                  :cmd "npm test")))
(add-to-list
 'aj-modes-tests-alist
 '(scheme-mode . (:dir default-directory
                  :fn async-shell-command
                  :cmd "make guile")))
(add-to-list
 'aj-modes-tests-alist
 '(scala-mode . (:dir (lambda () (locate-dominating-file "." "build.sbt"))
                 :fn shell-command
                 :cmd "export PATH=\"/usr/lib/jvm/java-8-openjdk/jre/bin/:$PATH\" && sbt test")))
(add-to-list
 'aj-modes-tests-alist
 `(lisp-mode . (:dir default-directory
                :fn async-shell-command
                :cmd ,(concat (executable-find "sbcl")
                              " --load *-test.lisp --quit"))))
(add-to-list
 'aj-modes-tests-alist
 '(sh-mode . (:dir default-directory
              :fn async-shell-command
              :cmd "BATS_RUN_SKIPPED=true bats *_test.sh")))
(add-to-list
 'aj-modes-tests-alist
 '(swift-mode . (:dir default-directory
                 :fn async-shell-command
                 :cmd "swift test")))
(add-to-list
 'aj-modes-tests-alist
 '(sml-mode . (:dir default-directory
               :fn async-shell-command
               :cmd "poly -q --use test.sml")))
(add-to-list
 'aj-modes-tests-alist
 '(tuareg-mode . (:dir default-directory
                  :fn async-shell-command
                  :cmd "make")))
(add-to-list
 'aj-modes-tests-alist
 '(typescript-mode . (:dir default-directory
                      :fn async-shell-command
                      :cmd "yarn test")))

(provide 'run-tests)
