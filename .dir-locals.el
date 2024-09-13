((nil
  . ((compile-command
      . "cabal build")

     (eglot-workspace-configuration
      . (:haskell (:formatingProvider "floskell")
                  :purescript (:outputDirectory "./web/output" :formatter "purs-tidy"))
      )
     )
  )

 (auto-mode-alist
  . (("\\.lock\\'"
      . json-ts-mode)))

 (haskell-mode
  . ((eval
      . (eglot-ensure))))

 (purescript-mode
  . ((eval
      . (eglot-ensure))))

 (nix-mode
  . ((eval
      . (eglot-ensure))))
 )
