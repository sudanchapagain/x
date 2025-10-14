(use-modules (guix)
             (guix build-system cmake)
             (guix build-system gnu)
             (guix gexp)
             (guix git)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages gawk)
             (gnu packages glib)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages icu4c)
             (gnu packages llvm)
             (gnu packages ninja)
             (gnu packages node)
             (gnu packages pkg-config)
             (gnu packages perl)
             (gnu packages python)
             (gnu packages texinfo)
             (gnu packages version-control))

(define guile-next-next
  (let ((commit "c8a169d38825d5a21da5392b355ca5fc9f33fa55")
        (revision "1"))
    (package
     (inherit guile-next)
     (version (git-version "3.0.10" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/guile/guile.git")
             (commit commit)))
       (file-name (git-file-name "guile" version))
       (sha256
        (base32 "01gqf6c9rnr5l8qralfwq23xmfxbnim1kqppgrd2l42pak3rm9c2")))))))

(package
  (name "guile-hoot")
  (version "0.6.1-git")
  (source (git-checkout (url (dirname (current-filename)))))
  (build-system gnu-build-system)
  (arguments
   '(#:make-flags '("GUILE_AUTO_COMPILE=0")))
  (native-inputs
   (list autoconf automake guile-syntax-highlight pkg-config texinfo))
  (inputs
   (list guile-next node))
  (synopsis "WebAssembly compiler for Guile Scheme")
  (description "Guile-hoot is an ahead-of-time, whole-program WebAssembly compiler for
GNU Guile.")
  (home-page "https://spritely.institute/hoot")
  (license (list license:asl2.0 license:lgpl3+)))
