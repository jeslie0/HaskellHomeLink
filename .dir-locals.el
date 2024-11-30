;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((compile-command . "cabal build")
         (eglot-workspace-configuration . (:haskell
                                           (:formattingProvider "fourmolu" :plugin
                                                                (:importLens
                                                                 (:globalOn nil)))
                                           :purescript
                                           (:outputDirectory "./software/web/output" :formatter "purs-tidy")))))
 (auto-mode-alist . (("\\.lock\\'" . json-ts-mode)))
 (haskell-mode . ((eval . (eglot-ensure))))
 (nix-mode . ((eval . (eglot-ensure))))
 (purescript-mode . ((eval . (eglot-ensure)))))
