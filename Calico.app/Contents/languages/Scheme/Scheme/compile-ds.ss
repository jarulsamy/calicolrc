(load "ds-transformer.ss")
(delete-file "calicoscheme-ds.ss")
(ds-transform-file "calicoscheme-cps.ss" "calicoscheme-ds.ss")
(exit)
