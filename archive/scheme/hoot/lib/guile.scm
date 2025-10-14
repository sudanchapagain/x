;;; Copyright (C) 2024, 2025 Igalia, S.L.
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Shim to implement Guile API on top of Hoot.
;;;
;;; Code:

;; bindings not supported:
#;
(define *unimplemented-bindings
  '($sc-dispatch
    %auto-compilation-options
    %char-set-dump
    %compile-fallback-path
    %cond-expand-features
    %cond-expand-table
    %expanded-vtables
    %file-port-name-canonicalization
    %fresh-auto-compile
    %get-pre-modules-obarray
    %get-stack-size
    %global-site-dir
    %guile-build-info
    %host-type
    %init-rdelim-builtins
    %init-rw-builtins
    %library-dir
    %load-announce
    %load-compiled-extensions
    %load-compiled-path
    %load-extensions
    %load-hook
    %load-path
    %load-should-auto-compile
    %load-verbosely
    %package-data-dir
    %port-property
    %print-module
    %read-hash-procedures
    %resolve-variable
    %search-load-path
    %set-port-property!
    %site-ccache-dir
    %site-dir
    %stacks
    %start-stack
    %string-dump
    %symbol-dump
    %warn-auto-compilation-enabled
    &exception-with-kind-and-args
    &programming-error
    &quit-exception
    *features*
    *null-device*
    *random-state*
    *repl-stack*
    ->char-set
    <applicable-struct-vtable>
    <applicable-struct-with-setter-vtable>
    <parameter>
    <standard-vtable>
    @
    AF_INET
    AF_INET6
    AF_UNIX
    AF_UNSPEC
    AI_ADDRCONFIG
    AI_ALL
    AI_CANONNAME
    AI_NUMERICHOST
    AI_NUMERICSERV
    AI_PASSIVE
    AI_V4MAPPED
    AT_EACCESS
    AT_EMPTY_PATH
    AT_NO_AUTOMOUNT
    AT_REMOVEDIR
    AT_SYMLINK_FOLLOW
    AT_SYMLINK_NOFOLLOW
    E2BIG
    EACCES
    EADDRINUSE
    EADDRNOTAVAIL
    EADV
    EAFNOSUPPORT
    EAGAIN
    EAI_ADDRFAMILY
    EAI_AGAIN
    EAI_ALLDONE
    EAI_BADFLAGS
    EAI_CANCELED
    EAI_FAIL
    EAI_FAMILY
    EAI_IDN_ENCODE
    EAI_INPROGRESS
    EAI_INTR
    EAI_MEMORY
    EAI_NODATA
    EAI_NONAME
    EAI_NOTCANCELED
    EAI_OVERFLOW
    EAI_SERVICE
    EAI_SOCKTYPE
    EAI_SYSTEM
    EALREADY
    EBADE
    EBADF
    EBADFD
    EBADMSG
    EBADR
    EBADRQC
    EBADSLT
    EBFONT
    EBUSY
    ECANCELED
    ECHILD
    ECHRNG
    ECOMM
    ECONNABORTED
    ECONNREFUSED
    ECONNRESET
    EDEADLK
    EDEADLOCK
    EDESTADDRREQ
    EDOM
    EDOTDOT
    EDQUOT
    EEXIST
    EFAULT
    EFBIG
    EHOSTDOWN
    EHOSTUNREACH
    EHWPOISON
    EIDRM
    EILSEQ
    EINPROGRESS
    EINTR
    EINVAL
    EIO
    EISCONN
    EISDIR
    EISNAM
    EKEYEXPIRED
    EKEYREJECTED
    EKEYREVOKED
    EL2HLT
    EL2NSYNC
    EL3HLT
    EL3RST
    ELIBACC
    ELIBBAD
    ELIBEXEC
    ELIBMAX
    ELIBSCN
    ELNRNG
    ELOOP
    EMEDIUMTYPE
    EMFILE
    EMLINK
    EMSGSIZE
    EMULTIHOP
    ENAMETOOLONG
    ENAVAIL
    ENETDOWN
    ENETRESET
    ENETUNREACH
    ENFILE
    ENOANO
    ENOBUFS
    ENOCSI
    ENODATA
    ENODEV
    ENOENT
    ENOEXEC
    ENOKEY
    ENOLCK
    ENOLINK
    ENOMEDIUM
    ENOMEM
    ENOMSG
    ENONET
    ENOPKG
    ENOPROTOOPT
    ENOSPC
    ENOSR
    ENOSTR
    ENOSYS
    ENOTBLK
    ENOTCONN
    ENOTDIR
    ENOTEMPTY
    ENOTNAM
    ENOTRECOVERABLE
    ENOTSOCK
    ENOTSUP
    ENOTTY
    ENOTUNIQ
    ENXIO
    EOPNOTSUPP
    EOVERFLOW
    EOWNERDEAD
    EPERM
    EPFNOSUPPORT
    EPIPE
    EPROTO
    EPROTONOSUPPORT
    EPROTOTYPE
    ERANGE
    EREMCHG
    EREMOTE
    EREMOTEIO
    ERESTART
    ERFKILL
    EROFS
    ESHUTDOWN
    ESOCKTNOSUPPORT
    ESPIPE
    ESRCH
    ESRMNT
    ESTALE
    ESTRPIPE
    ETIME
    ETIMEDOUT
    ETOOMANYREFS
    ETXTBSY
    EUCLEAN
    EUNATCH
    EUSERS
    EWOULDBLOCK
    EXDEV
    EXFULL
    EXIT_FAILURE
    EXIT_SUCCESS
    FD_CLOEXEC
    F_DUPFD
    F_GETFD
    F_GETFL
    F_GETOWN
    F_OK
    F_SETFD
    F_SETFL
    F_SETOWN
    IN6ADDR_ANY
    IN6ADDR_LOOPBACK
    INADDR_ANY
    INADDR_BROADCAST
    INADDR_LOOPBACK
    INADDR_NONE
    IPPROTO_IP
    IPPROTO_IPV6
    IPPROTO_TCP
    IPPROTO_UDP
    IPV6_V6ONLY
    IP_ADD_MEMBERSHIP
    IP_DROP_MEMBERSHIP
    IP_MULTICAST_IF
    IP_MULTICAST_TTL
    ITIMER_PROF
    ITIMER_REAL
    ITIMER_VIRTUAL
    LC_ADDRESS
    LC_ALL
    LC_COLLATE
    LC_CTYPE
    LC_IDENTIFICATION
    LC_MEASUREMENT
    LC_MESSAGES
    LC_MONETARY
    LC_NAME
    LC_NUMERIC
    LC_PAPER
    LC_TELEPHONE
    LC_TIME
    LOCK_EX
    LOCK_NB
    LOCK_SH
    LOCK_UN
    MSG_DONTROUTE
    MSG_DONTWAIT
    MSG_OOB
    MSG_PEEK
    NSIG
    OPEN_BOTH
    OPEN_READ
    OPEN_WRITE
    O_APPEND
    O_ASYNC
    O_CLOEXEC
    O_CREAT
    O_DIRECT
    O_DIRECTORY
    O_EXCL
    O_IGNORE_CTTY
    O_LARGEFILE
    O_NDELAY
    O_NOATIME
    O_NOCTTY
    O_NOFOLLOW
    O_NOLINK
    O_NONBLOCK
    O_NOTRANS
    O_PATH
    O_RDONLY
    O_RDWR
    O_SYNC
    O_TMPFILE
    O_TRUNC
    O_WRONLY
    PF_INET
    PF_INET6
    PF_UNIX
    PF_UNSPEC
    PIPE_BUF
    PRIO_PGRP
    PRIO_PROCESS
    PRIO_USER
    R_OK
    SA_NOCLDSTOP
    SA_RESTART
    SEEK_DATA
    SEEK_HOLE
    SIGABRT
    SIGALRM
    SIGBUS
    SIGCHLD
    SIGCLD
    SIGCONT
    SIGFPE
    SIGHUP
    SIGILL
    SIGINT
    SIGIO
    SIGIOT
    SIGKILL
    SIGPIPE
    SIGPOLL
    SIGPROF
    SIGPWR
    SIGQUIT
    SIGRTMAX
    SIGRTMIN
    SIGSEGV
    SIGSTKFLT
    SIGSTKSZ
    SIGSTOP
    SIGSYS
    SIGTERM
    SIGTRAP
    SIGTSTP
    SIGTTIN
    SIGTTOU
    SIGURG
    SIGUSR1
    SIGUSR2
    SIGVTALRM
    SIGWINCH
    SIGXCPU
    SIGXFSZ
    SIG_DFL
    SIG_IGN
    SOCK_CLOEXEC
    SOCK_DGRAM
    SOCK_NONBLOCK
    SOCK_RAW
    SOCK_RDM
    SOCK_SEQPACKET
    SOCK_STREAM
    SOL_SOCKET
    SO_BROADCAST
    SO_DEBUG
    SO_DONTROUTE
    SO_ERROR
    SO_KEEPALIVE
    SO_LINGER
    SO_NO_CHECK
    SO_OOBINLINE
    SO_PRIORITY
    SO_RCVBUF
    SO_RCVTIMEO
    SO_REUSEADDR
    SO_REUSEPORT
    SO_SNDBUF
    SO_SNDTIMEO
    SO_TYPE
    TCP_CORK
    TCP_NODELAY
    WAIT_ANY
    WAIT_MYPGRP
    WNOHANG
    WUNTRACED
    W_OK
    X_OK
    abort-hook
    abort-to-prompt*
    absolute-file-name?
    accept
    access?
    acosh
    add-hook!
    add-to-load-path
    addrinfo:addr
    addrinfo:canonname
    addrinfo:fam
    addrinfo:flags
    addrinfo:protocol
    addrinfo:socktype
    adjust-port-revealed!
    after-backtrace-hook
    after-error-hook
    after-eval-hook
    after-gc-hook
    after-print-hook
    after-read-hook
    alarm
    allocate-struct
    array->list
    array-cell-set!
    array-contents
    array-copy!
    array-copy-in-order!
    array-dimensions
    array-equal?
    array-fill!
    array-in-bounds?
    array-index-map!
    array-map!
    array-map-in-order!
    array-set!
    array-slice
    array-slice-for-each
    array-slice-for-each-in-order
    array-type-code
    asinh
    assert-load-verbosity
    assoc-remove!
    assoc-set!
    assq-remove!
    assq-set!
    assv-remove!
    assv-set!
    atanh
    autoload-done!
    autoload-done-or-in-progress?
    autoload-in-progress!
    autoloads-done
    autoloads-in-progress
    backtrace
    basename
    batch-mode?
    beautify-user-module!
    before-backtrace-hook
    before-error-hook
    before-eval-hook
    before-print-hook
    before-read-hook
    begin-deprecated
    bind
    bind-textdomain-codeset
    bindtextdomain
    bit-count
    bit-count*
    bit-extract
    bit-invert!
    bit-position
    bit-set*!
    bitvector
    bitvector->list
    bitvector-bit-clear?
    bitvector-bit-set?
    bitvector-clear-all-bits!
    bitvector-clear-bit!
    bitvector-clear-bits!
    bitvector-copy
    bitvector-count
    bitvector-count-bits
    bitvector-fill!
    bitvector-flip-all-bits!
    bitvector-position
    bitvector-set-all-bits!
    bitvector-set!
    bitvector-set-bits!
    call-with-blocked-asyncs
    call-with-deferred-observers
    call-with-include-port
    call-with-module-autoload-lock
    call-with-unblocked-asyncs
    canonicalize-path
    centered-quotient
    centered-remainder
    centered/
    char-general-category
    char-is-both?
    char-set-adjoin
    char-set-adjoin!
    char-set-any
    char-set-complement
    char-set-complement!
    char-set-copy
    char-set-count
    char-set-cursor
    char-set-cursor-next
    char-set-delete
    char-set-delete!
    char-set-diff+intersection
    char-set-diff+intersection!
    char-set-difference
    char-set-difference!
    char-set-every
    char-set-filter
    char-set-filter!
    char-set-fold
    char-set-for-each
    char-set-hash
    char-set-intersection
    char-set-intersection!
    char-set-map
    char-set-ref
    char-set-size
    char-set-unfold
    char-set-unfold!
    char-set-union!
    char-set-xor
    char-set-xor!
    char-set:designated
    char-set<=
    char-set=
    char-titlecase
    chdir
    chmod
    chmodat
    chown
    chown-at
    chroot
    close-fdes
    closedir
    compose
    connect
    cons-source
    convert-assignment
    copy-file
    copy-random-state
    cosh
    crypt
    ctermid
    current-filename
    current-language
    current-load-port
    current-module
    current-reader
    current-source-location
    current-warning-port
    datum->random-state
    debug-disable
    debug-enable
    debug-options
    debug-options-interface
    debug-set!
    default-duplicate-binding-handler
    default-duplicate-binding-procedures
    define!
    define-library
    define-macro
    define-module
    define-module*
    define-once
    define-option-interface
    define-private
    define-public
    define-syntax-parameter
    defined?
    defmacro
    defmacro-public
    delete
    delete!
    delete-file
    delete-file-at
    delete1!
    delv
    delv!
    delv1!
    directory-stream?
    dirname
    display-application
    display-backtrace
    display-error
    dup
    dup->fdes
    dup->inport
    dup->outport
    dup->port
    dup2
    duplicate-handlers
    duplicate-port
    dynamic-call
    dynamic-func
    dynamic-link
    dynamic-object?
    dynamic-pointer
    dynamic-unlink
    effective-version
    end-of-char-set?
    endgrent
    endhostent
    endnetent
    endprotoent
    endpwent
    endservent
    ensure-batch-mode!
    environ
    eval
    eval-string
    eval-when
    exception-accessor
    exception-args
    exception-kind
    exception-predicate
    exception-type?
    execl
    execle
    execlp
    exit-hook
    export
    export!
    export-syntax
    fcntl
    fdes->inport
    fdes->outport
    fdes->ports
    fdopen
    file-encoding
    file-exists?
    file-is-directory?
    file-name-separator-string
    file-name-separator?
    file-port?
    file-set-position
    fileno
    flock
    fluid->parameter
    fluid-bound?
    fluid-ref*
    fluid-thread-local?
    fluid-unset!
    flush-all-ports
    frame-address
    frame-arguments
    frame-dynamic-link
    frame-instruction-pointer
    frame-previous
    frame-procedure-name
    frame-return-address
    frame-source
    frame-stack-pointer
    frame?
    fsync
    gai-strerror
    gc
    gc-disable
    gc-dump
    gc-enable
    gc-run-time
    gc-stats
    gensym
    get-internal-real-time
    get-internal-run-time
    get-print-state
    getaddrinfo
    getaffinity
    getcwd
    getegid
    getenv
    geteuid
    getgid
    getgr
    getgrent
    getgrgid
    getgrnam
    getgroups
    gethost
    gethostbyaddr
    gethostbyname
    gethostent
    gethostname
    getitimer
    getlogin
    getnet
    getnetbyaddr
    getnetbyname
    getnetent
    getpass
    getpeername
    getpgrp
    getpid
    getppid
    getpriority
    getproto
    getprotobyname
    getprotobynumber
    getprotoent
    getpw
    getpwent
    getpwnam
    getpwuid
    getrlimit
    getserv
    getservbyname
    getservbyport
    getservent
    getsid
    getsockname
    getsockopt
    gettext
    gettimeofday
    getuid
    gmtime
    group:gid
    group:mem
    group:name
    group:passwd
    has-shown-backtrace-hint?
    hook->list
    hook-empty?
    hook?
    hostent:addr-list
    hostent:addrtype
    hostent:aliases
    hostent:length
    hostent:name
    import
    in-vicinity
    include
    include-ci
    include-deprecated-features
    include-library-declarations
    inet-lnaof
    inet-makeaddr
    inet-netof
    inet-ntop
    inherit-print-state
    install-r6rs!
    install-r7rs!
    integer-expt
    integer-length
    interaction-environment
    internal-time-units-per-second
    isatty?
    keyword-like-symbol->keyword
    kill
    kw-arg-ref
    library
    link
    list->array
    list->bitvector
    list->char-set!
    list->symbol
    list->typed-array
    list-cdr-ref
    list-cdr-set!
    list-index
    listen
    load
    load-compiled
    load-extension
    load-from-path
    load-in-vicinity
    load-user-init
    local-define
    local-define-module
    local-ref
    local-ref-module
    local-remove
    local-set!
    localtime
    log10
    lookup-duplicates-handlers
    lstat
    macro-binding
    macro-name
    macro-transformer
    macro-type
    macro?
    macroexpand
    macroexpanded?
    major-version
    make-array
    make-autoload-interface
    make-exception-type
    make-fresh-user-module
    make-generalized-vector
    make-guardian
    make-hook
    make-module
    make-modules-in
    make-mutable-parameter
    make-object-property
    make-procedure-with-setter
    make-record-type
    make-socket-address
    make-soft-port
    make-stack
    make-struct-layout
    make-struct/no-tail
    make-struct/simple
    make-symbol
    make-syntax-transformer
    make-thread-local-fluid
    make-typed-array
    make-unbound-fluid
    make-undefined-variable
    make-variable-transformer
    make-vtable
    map-in-order
    memoize-expression
    memoized-typecode
    merge
    merge!
    micro-version
    minor-version
    mkdir
    mkdirat
    mkdtemp
    mknod
    mkstemp
    mkstemp!
    mktime
    module-add!
    module-autoload!
    module-binder
    module-bound?
    module-call-observers
    module-clear!
    module-constructor
    module-declarative?
    module-defer-observers
    module-define!
    module-define-submodule!
    module-defined-hook
    module-defined?
    module-duplicates-handlers
    module-ensure-local-variable!
    module-export!
    module-export-all!
    module-filename
    module-for-each
    module-generate-unique-id!
    module-gensym
    module-import-interface
    module-import-obarray
    module-inlinable-exports
    module-kind
    module-local-variable
    module-locally-bound?
    module-make-local-var!
    module-map
    module-modified
    module-name
    module-next-unique-id
    module-obarray
    module-obarray-get-handle
    module-obarray-ref
    module-obarray-remove!
    module-obarray-set!
    module-observe
    module-observe-weak
    module-observers
    module-public-interface
    module-re-export!
    module-ref
    module-ref-submodule
    module-remove!
    module-replace!
    module-replacements
    module-reverse-lookup
    module-search
    module-submodule-binder
    module-submodules
    module-symbol-binding
    module-symbol-interned?
    module-symbol-local-binding
    module-symbol-locally-interned?
    module-transformer
    module-type
    module-unobserve
    module-use!
    module-use-interfaces!
    module-uses
    module-variable
    module-version
    module-weak-observers
    module?
    modulo-expt
    move->fdes
    nested-define!
    nested-define-module!
    nested-ref
    nested-ref-module
    nested-remove!
    nested-set!
    netent:addrtype
    netent:aliases
    netent:name
    netent:net
    ngettext
    nice
    nil?
    noop
    object-properties
    object-property
    open
    open-fdes
    open-fdes-at
    open-file
    open-file
    open-io-file
    openat
    opendir
    parameter-converter
    parameter-fluid
    parse-path
    parse-path-with-ellipsis
    passwd:dir
    passwd:gecos
    passwd:gid
    passwd:name
    passwd:passwd
    passwd:shell
    passwd:uid
    pause
    pipe
    port->fdes
    port-for-each
    port-mode
    port-revealed
    port-with-print-state
    prefab-record-types
    primitive-_exit
    primitive-eval
    primitive-exit
    primitive-fork
    primitive-load
    primitive-load-path
    primitive-move->fdes
    primitive-read
    print-disable
    print-enable
    print-exception
    print-options
    print-options-interface
    print-set!
    procedure
    procedure-documentation
    procedure-minimum-arity
    procedure-properties
    procedure-property
    procedure-source
    procedure-with-setter?
    process-use-modules
    protoent:aliases
    protoent:name
    protoent:proto
    provide
    provided?
    purify-module!
    putenv
    raise
    random
    random-state->datum
    random-state-from-platform
    random:exp
    random:hollow-sphere!
    random:normal
    random:normal-vector!
    random:solid-sphere!
    random:uniform
    re-export
    re-export-syntax
    read-disable
    read-enable
    read-eval?
    read-hash-extend
    read-hash-procedure
    read-hash-procedures
    read-options
    read-options-interface
    read-set!
    read-syntax
    readdir
    readlink
    record-accessor
    record-constructor
    record-modifier
    record-predicate
    record-type-constructor
    record-type-descriptor
    record-type-extensible?
    record-type-fields
    record-type-has-parent?
    record-type-mutable-fields
    record-type-name
    record-type-opaque?
    record-type-parent
    record-type-properties
    record-type-uid
    record-type-vtable
    record-type?
    recv!
    recvfrom!
    redirect-port
    release-port-handle
    reload-module
    remove-hook!
    rename-file
    rename-file-at
    repl-reader
    require-extension
    reset-hook!
    resolve-interface
    resolve-module
    resolve-r6rs-interface
    restore-signals
    reverse-list->string
    rewinddir
    rmdir
    round-ash
    round-quotient
    round-remainder
    round/
    run-hook
    save-module-excursion
    search-path
    seed->random-state
    select
    self-evaluating?
    send
    sendfile
    sendto
    servent:aliases
    servent:name
    servent:port
    servent:proto
    set-autoloaded!
    set-current-dynamic-state
    set-current-error-port
    set-current-input-port
    set-current-module
    set-current-output-port
    set-exception-printer!
    set-module-binder!
    set-module-declarative?!
    set-module-duplicates-handlers!
    set-module-filename!
    set-module-inlinable-exports!
    set-module-kind!
    set-module-name!
    set-module-next-unique-id!
    set-module-obarray!
    set-module-observers!
    set-module-public-interface!
    set-module-submodule-binder!
    set-module-submodules!
    set-module-transformer!
    set-module-uses!
    set-module-version!
    set-object-properties!
    set-object-property!
    set-port-column!
    set-port-filename!
    set-port-line!
    set-port-revealed!
    set-procedure-minimum-arity!
    set-procedure-properties!
    set-procedure-property!
    set-program-arguments
    set-source-properties!
    set-source-property!
    set-struct-vtable-name!
    set-symbol-property!
    set-tm:gmtoff
    set-tm:hour
    set-tm:isdst
    set-tm:mday
    set-tm:min
    set-tm:mon
    set-tm:sec
    set-tm:wday
    set-tm:yday
    set-tm:year
    set-tm:zone
    setaffinity
    setegid
    setenv
    seteuid
    setgid
    setgr
    setgrent
    setgroups
    sethost
    sethostent
    sethostname
    setitimer
    setlocale
    setnet
    setnetent
    setpgid
    setpriority
    setproto
    setprotoent
    setpw
    setpwent
    setrlimit
    setserv
    setservent
    setsid
    setsockopt
    setter
    setuid
    shared-array-increments
    shared-array-offset
    shared-array-root
    shutdown
    sigaction
    signal-handlers
    sinh
    sleep
    sloppy-assoc
    sloppy-assq
    sloppy-assv
    sockaddr:addr
    sockaddr:fam
    sockaddr:flowinfo
    sockaddr:path
    sockaddr:port
    sockaddr:scopeid
    socket
    socketpair
    sorted?
    source-properties
    source-property
    source-whash
    spawn
    stack-id
    stack-length
    stack-ref
    stack?
    standard-vtable-fields
    start-stack
    stat
    stat:atime
    stat:atimensec
    stat:blksize
    stat:blocks
    stat:ctime
    stat:ctimensec
    stat:dev
    stat:gid
    stat:ino
    stat:mode
    stat:mtime
    stat:mtimensec
    stat:nlink
    stat:perms
    stat:rdev
    stat:size
    stat:type
    stat:uid
    statat
    status:exit-val
    status:stop-sig
    status:term-sig
    strerror
    strftime
    string->char-set!
    string-any
    string-any-c-code
    string-append/shared
    string-bytes-per-char
    string-capitalize
    string-capitalize!
    string-ci->symbol
    string-ci<
    string-ci<=
    string-ci<>
    string-ci=
    string-ci>
    string-ci>=
    string-compare
    string-compare-ci
    string-concatenate-reverse/shared
    string-concatenate/shared
    string-contains
    string-contains-ci
    string-count
    string-delete
    string-downcase!
    string-drop
    string-drop-right
    string-every
    string-every-c-code
    string-filter
    string-fold
    string-fold-right
    string-for-each-index
    string-hash
    string-hash-ci
    string-map!
    string-normalize-nfc
    string-normalize-nfd
    string-normalize-nfkc
    string-normalize-nfkd
    string-pad
    string-pad-right
    string-prefix-length
    string-prefix-length-ci
    string-replace
    string-reverse!
    string-skip
    string-skip-right
    string-suffix-length
    string-suffix-length-ci
    string-tabulate
    string-take
    string-take-right
    string-titlecase
    string-titlecase!
    string-tokenize
    string-unfold
    string-unfold-right
    string-upcase!
    string-utf8-length
    string-xcopy!
    string<
    string<=
    string<>
    string=
    string>
    string>=
    strptime
    struct-layout
    struct-ref
    struct-ref/unboxed
    struct-set!
    struct-set!/unboxed
    struct-vtable
    struct-vtable-name
    struct-vtable?
    struct?
    substring-fill!
    substring-move!
    substring/copy
    substring/read-only
    supports-source-properties?
    symbol
    symbol-append
    symbol-fref
    symbol-fset!
    symbol-hash
    symbol-interned?
    symbol-pref
    symbol-prefix-proc
    symbol-property
    symbol-property-remove!
    symbol-pset!
    symlink
    symlinkat
    sync
    syntax-parameterize
    syntax-source
    system
    system*
    system-async-mark
    system-error-errno
    system-file-name-convention
    tanh
    tcgetpgrp
    tcsetpgrp
    textdomain
    the-root-module
    the-scm-module
    thunk?
    times
    tm:gmtoff
    tm:hour
    tm:isdst
    tm:mday
    tm:min
    tm:mon
    tm:sec
    tm:wday
    tm:yday
    tm:year
    tm:zone
    tmpfile
    tmpnam
    tms:clock
    tms:cstime
    tms:cutime
    tms:stime
    tms:utime
    transpose-array
    truncate
    truncate-file
    truncate-quotient
    truncate-remainder
    truncate/
    try-load-module
    try-module-autoload
    ttyname
    typed-array?
    tzset
    ucs-range->char-set
    ucs-range->char-set!
    umask
    uname
    unmemoize-expression
    unsetenv
    use-modules
    use-srfis
    user-modules-declarative?
    using-readline?
    usleep
    utime
    utsname:machine
    utsname:nodename
    utsname:release
    utsname:sysname
    utsname:version
    variable-bound?
    variable-unset!
    vector-move-right!
    version
    version-matches?
    vtable-index-layout
    vtable-index-printer
    vtable-offset-user
    waitpid
    warn
    with-continuation-barrier
    with-ellipsis
    with-fluids*
    xsubstring))

(library (guile)
  (export %default-port-conversion-strategy
          %default-port-encoding
          %make-void-port
          &compound-exception
          &error
          &exception
          &non-continuable
          *
          *unspecified*
          @@
          +
          -
          ->bool
          ...
          /
          1+ 1-
          <
          <=
          =
          =>
          >
          >=
          _
          AF_INET
          AF_INET6
          SEEK_CUR
          SEEK_END
          SEEK_SET
          abort-to-prompt
          abs
          acons
          acos
          and
          and-map
          and=>
          angle
          append
          append!
          apply
          array?
          array-cell-ref
          array-for-each
          array-length
          array-rank
          array-ref
          array-shape
          array-type
          ash
          asin
          assoc
          assoc-ref
          assq
          assq-ref
          assv
          assv-ref
          atan
          begin
          bitvector-length
          bitvector-ref
          bitvector-set-bit!
          bitvector?
          boolean?
          bound-identifier=?
          caaaar
          caaadr
          caaar
          caadar
          caaddr
          caadr
          caar
          cadaar
          cadadr
          cadar
          caddar
          cadddr
          caddr
          cadr
          call-with-current-continuation
          call-with-input-file
          call-with-input-string
          call-with-output-file
          call-with-output-string
          call-with-port
          call-with-prompt
          call-with-values
          call/cc
          car
          case
          case-lambda
          case-lambda*
          catch
          cdaaar
          cdaadr
          cdaar
          cdadar
          cdaddr
          cdadr
          cdar
          cddaar
          cddadr
          cddar
          cdddar
          cddddr
          cdddr
          cddr
          cdr
          ceiling
          ceiling-quotient
          ceiling-remainder
          ceiling/
          char->integer
          char-alphabetic?
          char-ci<=?
          char-ci<?
          char-ci=?
          char-ci>=?
          char-ci>?
          char-downcase
          char-lower-case?
          char-numeric?
          char-ready?
          char-upcase
          char-upper-case?
          char-whitespace?
          char<=?
          char<?
          char=?
          char>=?
          char>?
          char?
          char-set
          char-set-union
          char-set->list
          char-set->string
          char-set?
          char-set-contains?
          char-set:ascii
          char-set:blank
          char-set:digit
          char-set:empty
          char-set:full
          char-set:graphic
          char-set:hex-digit
          char-set:iso-control
          char-set:letter
          char-set:letter+digit
          char-set:lower-case
          char-set:printing
          char-set:punctuation
          char-set:symbol
          char-set:title-case
          char-set:upper-case
          char-set:whitespace
          close
          close-input-port
          close-output-port
          close-port
          command-line
          complex?
          cond
          cond-expand
          cond-expand-provide
          cons
          cons*
          const
          cos
          current-dynamic-state
          current-error-port
          current-input-port
          current-output-port
          current-time
          datum->syntax
          default-prompt-tag
          define
          define*
          define-inlinable
          define-syntax
          define-syntax-rule
          define-values
          delay
          delq
          delq!
          delq1!
          denominator
          display
          do
          doubly-weak-hash-table?
          drain-input
          dynamic-state?
          dynamic-wind
          else
          eof-object?
          eq?
          equal?
          eqv?
          error
          euclidean-quotient
          euclidean-remainder
          euclidean/
          even?
          (rename inexact exact->inexact)
          exact-integer-sqrt
          exact-integer?
          exact?
          exception?
          exit
          exp
          expt
          false-if-exception
          file-position
          filter
          filter!
          finite?
          floor
          floor-quotient
          floor-remainder
          floor/
          fluid-ref
          fluid-set!
          fluid?
          for-each
          force
          force-output
          format
          free-identifier=?
          ftell
          gcd
          generate-temporaries
          get-output-string
          hash
          hash-clear!
          hash-count
          hash-create-handle!
          hash-fold
          hash-for-each
          hash-for-each-handle
          hash-get-handle
          hash-map->list
          hash-ref
          hash-remove!
          hash-set!
          hash-table?
          hashq
          hashq-create-handle!
          hashq-get-handle
          hashq-ref
          hashq-remove!
          hashq-set!
          hashv
          hashv-create-handle!
          hashv-get-handle
          hashv-ref
          hashv-remove!
          hashv-set!
          hashx-create-handle!
          hashx-get-handle
          hashx-ref
          hashx-remove!
          hashx-set!
          identifier-syntax
          identifier?
          identity
          if
          imag-part
          include-from-path
          (rename exact inexact->exact)
          inet-pton
          inexact?
          inf
          (rename infinite? inf?)
          input-port?
          integer->char
          integer?
          iota
          issue-deprecation-warning
          keyword->symbol
          keyword?
          lambda
          lambda*
          last-pair
          lcm
          length
          let
          let*
          let-syntax
          letrec
          letrec*
          letrec-syntax
          list
          list->char-set
          list->string
          list->vector
          list-copy
          list-head
          list-ref
          list-set!
          list-tail
          list?
          log
          logand
          logbit?
          logcount
          logior
          lognot
          logtest
          logxor
          magnitude
          make-bitvector
          make-doubly-weak-hash-table
          make-exception
          make-exception-from-throw
          make-fluid
          make-hash-table
          make-list
          make-parameter
          make-polar
          make-promise
          make-prompt-tag
          make-rectangular
          make-regexp
          make-shared-array
          make-string
          (rename make-box make-variable)
          make-vector
          make-weak-key-hash-table
          make-weak-value-hash-table
          map
          max
          member
          memq
          memv
          min
          module-set!
          modulo
          most-negative-fixnum
          most-positive-fixnum
          nan
          nan?
          negate
          negative?
          newline
          not
          null?
          number->string
          number?
          numerator
          object->string
          object-address
          odd?
          open-input-file
          open-input-string
          open-output-file
          open-output-string
          or
          or-map
          output-port?
          pair?
          parameter?
          parameterize
          peek
          peek-char
          pk
          port-closed?
          port-column
          port-conversion-strategy
          port-encoding
          port-filename
          port-line
          port?
          positive?
          procedure?
          procedure-name
          program-arguments
          promise?
          quasiquote
          quasisyntax
          quit
          quote
          quote-syntax
          quotient
          raise-exception
          rational?
          rationalize
          read
          read-char
          real-part
          real?
          record-type-parents
          record?
          regexp-exec
          regexp/basic
          regexp/extended
          regexp/icase
          regexp/newline
          regexp/notbol
          regexp/noteol
          regexp?
          remainder
          restricted-vector-sort!
          reverse
          reverse!
          round
          seek
          scm-error
          set!
          set-car!
          set-cdr!
          set-port-conversion-strategy!
          set-port-encoding!
          setvbuf
          simple-exceptions
          simple-format
          sin
          sort
          sort!
          sort-list
          sort-list!
          sqrt
          stable-sort
          stable-sort!
          string
          string->char-set
          string->list
          string->number
          string->symbol
          string-append
          string-capitalize
          string-ci<=?
          string-ci<?
          string-ci=?
          string-ci>=?
          string-ci>?
          string-concatenate
          string-concatenate-reverse
          string-copy
          string-copy!
          string-downcase
          string-fill!
          string-for-each
          string-index
          string-index-right
          string-join
          string-length
          string-map
          string-null?
          string-prefix-ci?
          string-prefix?
          string-pad
          string-ref
          string-reverse
          string-rindex
          string-trim
          string-trim-both
          string-trim-right
          string-set!
          string-split
          string-suffix-ci?
          string-suffix?
          string-upcase
          string<=?
          string<?
          string=?
          string>=?
          string>?
          string?
          substring
          substring/shared
          symbol->keyword
          symbol->string
          symbol?
          syntax
          syntax->datum
          syntax-case
          syntax-error
          syntax-rules
          syntax-violation
          tan
          the-eof-object
          throw
          unless
          unquote
          unquote-splicing
          unread-char
          unread-string
          unspecified?
          unsyntax
          unsyntax-splicing
          values
          (rename box-ref variable-ref)
          (rename box-set! variable-set!)
          (rename box? variable?)
          vector
          vector->list
          vector-copy
          vector-copy!
          vector-fill!
          vector-length
          vector-move-left!
          vector-ref
          vector-set!
          vector?
          weak-key-hash-table?
          weak-value-hash-table?
          when
          while
          with-dynamic-state
          with-error-to-file
          with-error-to-port
          with-error-to-string
          with-exception-handler
          with-fluid*
          with-fluids
          with-input-from-file
          with-input-from-port
          with-input-from-string
          with-output-to-file
          with-output-to-port
          with-output-to-string
          with-syntax
          with-throw-handler
          write
          write-char
          zero?
          λ)
  (import (hoot assoc)
          (hoot bitvectors)
          (hoot bitwise)
          (hoot boxes)
          (hoot bytevectors)
          (hoot char)
          (hoot cond-expand)
          (hoot control)
          (hoot dynamic-states)
          (hoot dynamic-wind)
          (hoot eq)
          (hoot equal)
          (hoot error-handling)
          (hoot errors)
          (hoot exceptions)
          (hoot features)
          (hoot fluids)
          (hoot hashtables)
          (hoot keywords)
          (except (hoot lists) sort)
          (prefix (only (hoot lists) sort) list:)
          (hoot not)
          (hoot numbers)
          (hoot pairs)
          (hoot parameters)
          (hoot ports)
          (hoot apply)
          (hoot procedures)
          (only (hoot read) read string->number)
          (hoot records)
          (hoot regexps)
          (except (hoot strings) substring)
          (prefix (only (hoot strings) substring) hoot:)
          (hoot syntax)
          (hoot syntax-objects)
          (hoot symbols)
          (hoot values)
          (hoot vectors)
          (hoot write)
          (ice-9 match)
          (only (scheme base) boolean?)
          (only (scheme char)
                char-ci<? char-ci<=? char-ci=? char-ci>=? char-ci>?
                string-ci<? string-ci<=? string-ci=? string-ci>=? string-ci>?)
          (only (scheme file)
                open-input-file
                open-output-file
                call-with-input-file
                call-with-output-file
                with-input-from-file
                with-output-to-file)
          (scheme lazy)
          (scheme process-context)
          (scheme time)
          (srfi srfi-14))

  ;; FIXME: Guile's SRFI modules use this but it relies on the module
  ;; API which we don't implement.
  (define-syntax-rule (cond-expand-provide module feature)
    (values))

  ;; FIXME: @@ is unsupported, but there are modules in Guile that we
  ;; want to be able to import and expand even if they are partially
  ;; unusable at runtime.  So, we don't throw an error at expansion
  ;; time but rather runtime if the procedure is ever called.  We need
  ;; to fix modules in Guile so that they don't use @@.
  (define-syntax-rule (@@ module name)
    (lambda args (raise (make-unimplemented-error '@@))))
  ;; FIXME: No-op for now to get modules like (ice-9 format) working.
  (define-syntax-rule (module-set! module name value)
    (values))

  (define-syntax define-inlinable
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name formals ...) body0 body ...)
         (identifier? #'name)
         (let ((proc-name (string->symbol
                           (string-append "% "
                                          (symbol->string
                                           (syntax->datum #'name))
                                          "-procedure"))))
           (with-syntax ((proc-name (datum->syntax #'name proc-name))
                         ((args ...) (generate-temporaries #'(formals ...))))
             #'(begin
                 (define (proc-name formals ...)
                   body0 body ...)
                 (define-syntax name
                   (lambda (stx)
                     (syntax-case stx ()
                       ((_ args ...)
                        #'((lambda (formals ...) body0 body ...) args ...))
                       ((_ bad-arg (... ...))
                        (syntax-violation 'name "wrong number of arguments" stx))
                       (_
                        (identifier? stx)
                        #'proc-name)))))))))))

  ;; FIXME: Doesn't support break/continue due to compiler bug.
  ;;
  ;; See https://gitlab.com/spritely/guile-hoot/-/issues/316
  (define-syntax-rule (while cond body ...)
    (let lp () (and cond (begin body ... (lp)))))

  (define (identity x) x)

  (define (const x) (lambda args x))

  ;; TODO: Implement arrays.
  (define (array? obj) #f)
  (define (make-shared-array array proc . dims)
    (raise (make-unimplemented-error 'make-shared-array)))
  (define (array-type array)
    (raise (make-unimplemented-error 'array-type)))
  (define (array-length array)
    (raise (make-unimplemented-error 'array-length)))
  (define (array-rank array)
    (raise (make-unimplemented-error 'array-rank)))
  (define (array-shape array)
    (raise (make-unimplemented-error 'array-shape)))
  (define (array-ref array . idx)
    (raise (make-unimplemented-error 'array-ref)))
  (define (array-cell-ref array . idx)
    (raise (make-unimplemented-error 'array-cell-ref)))
  (define (array-for-each proc . arrays)
    (raise (make-unimplemented-error 'array-for-each)))

  (define (%make-void-port mode)
    (define (mode-prefix-match? test)
      (call-with-input-string mode
        (lambda (p)
          (let lp ((c (read-char p)))
            (cond
             ((eof-object? c) #f)
             ((or (test c)
                  (char-ci=? c #\+)) #t)
             (else (lp (read-char p))))))))
    (define %read
      (and (mode-prefix-match? (lambda (c)
                                 (char-ci=? c #\r)))
           (lambda (bv start count) (eof-object))))
    (define %write
      (and (mode-prefix-match? (lambda (c)
                                 (or (char-ci=? c #\w)
                                     (char-ci=? c #\a))))
           (lambda (bv start count) count)))
    (make-port #:read %read
               #:write %write
               #:repr "void"))

  (define-syntax *unspecified*
    (identifier-syntax (if #f #f)))

  (define (->bool x) (if x #t #f))

  (define (and-map f l)
    (match l
      (() #t)
      ((x . l)
       (and (f x) (and-map f l)))))
  (define (or-map f l)
    (match l
      (() #f)
      ((x . l)
       (or (f x) (or-map f l)))))

  (define (filter pred l)
    (match l
      (() '())
      ((head . tail)
       (if (pred head)
           (cons head (filter pred tail))
           (filter pred tail)))))

  (define* (iota count #:optional (start 0) (step 1))
    "Return a list of length @var{count} containing numbers starting with
@var{start} and incrementing by @var{step}.  @var{start} defaults to 0 and
@var{step} to 1."
    (assert (>= count 0) 'iota)
    (if (zero? count)
        '()
        (cons start (iota (1- count) (+ start step) step))))

  (define (and=> x f) (and x (f x)))

  (define (list-head lst k)
    (if (zero? k)
        '()
        (cons (car lst) (list-head (cdr lst) (1- k)))))

  ;; Guile's manual says that append! and filter! are not required to
  ;; modify the list, so let's not!
  (define append! append)
  (define filter! filter)

  (define* (reverse! lst #:optional (newtail '()))
    (append (reverse lst) newtail))

  (define (delq item lst)
    (match lst
      (() '())
      ((x . rest)
       (if (eq? item x)
           (delq item rest)
           (cons x (delq item rest))))))

  (define (delq! item lst)
    (match lst
      (() '())
      ((x . rest)
       (if (eq? item x)
           (delq! item rest)
           (let ((rest* (delq! item rest)))
             (set-cdr! lst rest*)
             lst)))))

  (define (delq1! item lst)
    (match lst
      (() '())
      ((x . rest)
       (if (eq? item x)
           rest
           (let ((rest* (delq1! item rest)))
             (set-cdr! lst rest*)
             lst)))))

  (define sort-list list:sort)
  (define sort-list! sort-list)
  (define (restricted-vector-sort! v less? start end)
    (vector-sort! v less? start end))
  (define (sort! items <)
    (match items
      (() '())
      ((? pair?) (sort-list items <))
      ((? vector?)
       (restricted-vector-sort! items < 0 (vector-length items))
       items)))
  (define (sort items <)
    (match items
      (() '())
      ((? pair?) (sort-list items <))
      ((? vector?)
       (let ((v (vector-copy items)))
         (restricted-vector-sort! v < 0 (vector-length v))
         v))))
  ;; FIXME: vector-sort! is not stable.
  (define stable-sort sort)
  (define stable-sort! sort!)

  (define (call-with-input-string str proc)
    (proc (open-input-string str)))
  (define (call-with-output-string proc)
    (let ((port (open-output-string)))
      (proc port)
      (get-output-string port)))

  (define (with-input-from-string str thunk)
    (call-with-input-string str
      (lambda (port)
        (with-input-from-port port thunk))))
  (define (with-output-to-string thunk)
    (call-with-output-string
      (lambda (port)
        (with-output-to-port port thunk))))

  (define (with-input-from-port port thunk)
    (parameterize ((current-input-port port))
      (thunk)))
  (define (with-output-to-port port thunk)
    (parameterize ((current-output-port port))
      (thunk)))
  (define (with-error-to-port port thunk)
    (parameterize ((current-error-port port))
      (thunk)))

  (define (with-error-to-file filename thunk)
    (call-with-port (open-output-file filename)
      (lambda (port)
        (with-error-to-port port thunk))))
  (define (with-error-to-string thunk)
    (call-with-output-string
     (lambda (port) (with-error-to-port port thunk))))

  (define (current-time)
    (exact (truncate (current-second))))

  (define (ftell port)
    (seek port 0 'cur))

  (define file-position ftell)

  (define* (drain-input #:optional (port (current-input-port)))
    (flush-input-port port))
  (define* (force-output #:optional (port (current-output-port)))
    (flush-output-port port))

  (define (simple-format port template . args)
    (define (do-format port)
      (call-with-input-string
       template
       (lambda (in)
         (let lp ((args args))
           (match (read-char in)
             ((? eof-object?)
              (match args
                (() (force-output port))
                (_ (error "leftover format args" template args))))
             (#\~
              (match (read-char in)
                ((or #\a #\A)
                 (match args
                   ((x . args) (display x port) (lp args))
                   (_ (error "not enough format args" template))))
                ((or #\s #\S)
                 (match args
                   ((x . args) (write x port) (lp args))
                   (_ (error "not enough format args" template))))
                (#\~ (write-char #\~ port) (lp args))
                (#\% (newline port) (lp args))
                (ch (error "unexpected format directive" template ch))))
             (ch (write-char ch port) (lp args))))
         (if #f #f))))
    (match port
      (#t (do-format (current-output-port)))
      (#f (call-with-output-string do-format))
      ((? output-port?) (do-format port))
      (_ (error "invalid format destination" port))))

  (define format simple-format)

  (define (inf) +inf.0)
  (define (nan) +nan.0)
  (define (negate x) (- x))

  (define* (object->string obj #:optional (print write))
    (call-with-output-string (lambda (port) (print obj port))))
  ;; Object addresses are not visible in Wasm.
  (define (object-address obj) 0)

  (define (string-null? str) (string=? str ""))

  (define (string-concatenate strs)
    (apply string-append strs))

  (define (string-split str char-pred)
    (let ((char-pred (match char-pred
                       ((? char? a) (lambda (b) (char=? a b)))
                       ((? char-set? cs) (lambda (c) (char-set-contains? cs c)))
                       ((? procedure?) char-pred))))
      (call-with-input-string str
        (lambda (port)
          (define (read-to-delimiter)
            (list->string
             (let lp ()
               (match (peek-char port)
                 ((? eof-object?) '())
                 ((? char-pred) '())
                 (c (cons (read-char port) (lp)))))))
          (let lp ()
            (let ((substr (read-to-delimiter)))
              (match (read-char port)
                ((? eof-object?) (list substr))
                (_ (cons substr (lp))))))))))

  (define* (string-join strs #:optional (delimiter " ") (grammar 'infix))
    (define (prefix-join strs)
      (match strs
        (() '())
        ((str . rest)
         (cons* delimiter str (prefix-join rest)))))
    (string-concatenate
     (match grammar
       ('infix
        (match strs
          (() '())
          ((first . rest) (cons first (prefix-join rest)))))
       ('strict-infix
        (match strs
          (() (error "strict-infix join with empty list"))
          ((first . rest)
           (cons first (prefix-join rest)))))
       ('prefix (prefix-join strs))
       ('suffix
        (let lp ((strs strs))
          (match strs
            (() '())
            ((str . rest) (cons* str delimiter (lp rest)))))))))

  (define* (substring str start #:optional (end (string-length str)))
    (hoot:substring str start end))

  (define substring/shared substring)

  (define* (string-concatenate-reverse strs #:optional final end)
    (string-concatenate
     (reverse
      (if final
          (cons (if end (substring final 0 end) final) strs)
          strs))))

  (define* (string-pad str len #:optional
                       (char #\space) (start 0) (end (string-length str)))
    (let ((k (- len (- end start))))
      (cond
       ((zero? k) (substring str start end))
       ((< k 0) (substring str (+ start (* k -1)) end))
       (else
        (string-append (make-string k char)
                       (substring str start end))))))

  (define* (string-index s char-pred #:optional
                         (start 0) (end (string-length s)))
    (let ((char-pred
           (match char-pred
             ((? char?) (lambda (c) (char=? c char-pred)))
             ((? char-set? cs) (lambda (c) (char-set-contains? cs c)))
             ((? procedure?) char-pred))))
      (let lp ((i 0)
               (sl (string->list (substring s start end))))
        (match sl
          (() #f)
          (((? char-pred) . rest) (+ start i))
          ((c . rest) (lp (1+ i) rest))))))

  (define* (string-rindex s char-pred #:optional
                          (start 0) (end (string-length s)))
    (let ((i (string-index (string-reverse (substring s start end))
                           char-pred)))
      (and i (- end i 1))))

  (define string-index-right string-rindex)

  (define (%string-prefix? s1 s2 start1 end1 start2 end2 string-equal?)
    (let ((k (- end1 start1)))
      (and (<= k (- end2 start2))
           (string-equal? (substring s1 start1 end1)
                          (substring s2 start2 (+ start2 k))))))

  (define* (string-prefix? s1 s2 #:optional
                           (start1 0) (end1 (string-length s1))
                           (start2 0) (end2 (string-length s2)))
    (%string-prefix? s1 s2 start1 end1 start2 end2 string=?))

  (define* (string-prefix-ci? s1 s2 #:optional
                              (start1 0) (end1 (string-length s1))
                              (start2 0) (end2 (string-length s2)))
    (%string-prefix? s1 s2 start1 end1 start2 end2 string-ci=?))

  (define (%string-suffix? s1 s2 start1 end1 start2 end2 string-equal?)
    (let ((k (- end1 start1)))
      (and (<= k (- end2 start2))
           (string-equal? (substring s1 start1 end1)
                          (substring s2 (- end2 k) end2)))))

  (define* (string-suffix? s1 s2 #:optional
                           (start1 0) (end1 (string-length s1))
                           (start2 0) (end2 (string-length s2)))
    (%string-suffix? s1 s2 start1 end1 start2 end2 string=?))

  (define* (string-suffix-ci? s1 s2 #:optional
                              (start1 0) (end1 (string-length s1))
                              (start2 0) (end2 (string-length s2)))
    (%string-suffix? s1 s2 start1 end1 start2 end2 string-ci=?))

  (define* (string-reverse str #:optional (start 0) (end (string-length str)))
    (let ((pre (substring str 0 start))
          (post (substring str end (string-length str))))
      (string-append
       pre
       (list->string
        (reverse
         (string->list
          (substring str start end))))
       post)))

  (define (%string-trim sl char-pred)
    (let ((char-pred
           (match char-pred
             ((? char?) (lambda (c) (char=? c char-pred)))
             ((? char-set? cs) (lambda (c) (char-set-contains? cs c)))
             ((? procedure?) char-pred))))
      (let lp ((sl sl))
        (match sl
          (() '())
          (((? char-pred c) . rest) (lp rest))
          (_ sl)))))

  (define* (string-trim s #:optional
                        (char-pred char-set:whitespace)
                        (start 0) (end (string-length s)))
    (list->string
     (%string-trim (string->list (substring s start end))
                   char-pred)))

  (define* (string-trim-right s #:optional
                              (char-pred char-set:whitespace)
                              (start 0) (end (string-length s)))
    (list->string
     (reverse
      (%string-trim (reverse (string->list (substring s start end)))
                    char-pred))))

  (define* (string-trim-both s #:optional
                             (char-pred char-set:whitespace)
                             (start 0) (end (string-length s)))
    (string-trim-right (string-trim s char-pred start end) char-pred))

  (define (string-capitalize str)
    (raise (make-unimplemented-error 'string-capitalize)))

  (define (last-pair l)
    (match l
      ((_ . (and l (_ . _))) (last-pair l))
      ((_ . _) l)
      (_ (error "not a pair" l))))
  (define (peek . stuff)
    (newline)
    (display ";;; ")
    (write stuff)
    (newline)
    (flush-output-port (current-output-port))
    (car (last-pair stuff)))
  (define pk peek)

  (define %default-port-conversion-strategy (make-fluid 'substitute))
  (define %default-port-encoding (make-fluid "UTF-8"))

  (define close close-port)
  (define (port-closed? port) (not (port-open? port)))

  (define SEEK_CUR 'cur)
  (define SEEK_SET 'start)
  (define SEEK_END 'end)

  (define* (unread-char char #:optional (port (current-input-port)))
    (raise (make-unimplemented-error 'unread-char)))
  (define* (unread-string str port)
    (raise (make-unimplemented-error 'unread-string)))

  (define* (setvbuf port mode #:optional size)
    (raise (make-unimplemented-error 'setvbuf)))

  (define (program-arguments) (command-line))
  (define quit exit)
  (define the-eof-object (eof-object))

  (define (unspecified? x) (eq? x *unspecified*))

  (define-syntax-rule (λ formals body ...)
    (lambda formals body ...))

  (define* (catch key thunk handler #:optional pre-unwind-handler)
    (raise (make-unimplemented-error 'catch)))

  ;; TODO: Should we handle Guile's legacy key + args exception
  ;; system?
  (define (with-throw-handler key thunk handler)
    (unless (eq? key #t)
      (raise (make-unimplemented-error 'with-throw-handler)))
    (with-exception-handler
        (lambda (exn)
          (apply handler #t '())
          (raise-exception exn))
      thunk))

  ;; TODO: Implement Guile's conversions based on key.
  (define (make-exception-from-throw key args)
    (make-exception-with-irritants args))

  (define (throw key . args)
    (raise-exception (make-exception-from-throw key args)))

  (define (scm-error key subr message args data)
    (raise
     (make-exception (make-exception-from-throw key args)
                     (make-exception-with-message
                      (apply format #f message args))
                     (make-exception-with-origin subr))))

  (define-syntax-rule (false-if-exception expr)
    (with-exception-handler (lambda (exn) #f)
      (lambda () expr)
      #:unwind? #t))

  (define issue-deprecation-warning
    (let ((past-messages (make-hashtable)))
      (lambda msgs
        (let ((msgs-str (string-concatenate msgs)))
          (unless (hashtable-contains? past-messages msgs-str)
            (hashtable-set! past-messages msgs-str #t)
            (display msgs-str (current-error-port))
            (newline (current-error-port)))))))

  ;; Hash table API compatibility shim:
  ;;
  ;; Guile's legacy hash table API is not so great.  It allows for
  ;; mixing different hash functions in the same table, which is why
  ;; there are *four* variants for ref/set!/remove! procedures.  On
  ;; top of that, the API is also polymorphic.  Those same procedures
  ;; are used on "normal", weak key, weak value, and doubly weak
  ;; tables.
  ;;
  ;; We made a better interface in (hoot hashtables) that resembles
  ;; the R6RS API and is monomorphic.  However, in the interest of
  ;; maximizing the amount of existing Guile code that can be compiled
  ;; as-is with Hoot, we have provided this compatibility shim.
  ;;
  ;; Hoot does *not* provide full compatibility, just partial
  ;; compatibility for common use-cases.  Code that is mixing hash
  ;; functions in the same table or using
  ;; hash-get-handle/hash-create-handle! is not supported.  Also,
  ;; because hashx-* procedures use assoc and friends, which is
  ;; incompatible with how (hoot hashtables) does equality testing, we
  ;; force equal? as the equivalence function.
  (define-record-type <hash-table>
    (%make-hash-table type table)
    hash-table?
    (type hash-table-type) ; normal, weak-key, weak-value, doubly-weak
    (table hash-table-table set-hash-table-table!)) ; lazily initialized

  (define* (make-hash-table #:optional size)
    "Return a new hash table. @var{size} is ignored."
    (%make-hash-table 'normal #f))
  (define* (make-weak-key-hash-table #:optional size)
    "Return a new weak key hash table.  @var{size} is ignored."
    (%make-hash-table 'weak-key #f))
  (define* (make-weak-value-hash-table #:optional size)
    "Return a new weak value hash table.  @var{size} is ignored."
    (%make-hash-table 'weak-value #f))
  (define* (make-doubly-weak-hash-table #:optional size)
    "Return a new doubly weak hash table.  @var{size} is ignored."
    (%make-hash-table 'doubly-weak #f))

  (define (weak-key-hash-table? obj)
    "Return @code{#t} if @var{obj} is a weak key hash table."
    (and (hash-table? obj) (eq? (hash-table-type obj) 'weak-key)))
  (define (weak-value-hash-table? obj)
    "Return @code{#t} if @var{obj} is a weak value hash table."
    (and (hash-table? obj) (eq? (hash-table-type obj) 'weak-value)))
  (define (doubly-weak-hash-table? obj)
    "Return @code{#t} if @var{obj} is a doubly weak hash table."
    (and (hash-table? obj) (eq? (hash-table-type obj) 'doubly-weak)))

  ;; Should these assert that the hash and equiv functions are what we
  ;; expect?  Currently, mixing hash functions on the same table will
  ;; just silently use the hash function of the first ref/set!/remove!
  ;; call.
  (define (maybe-init-equal-hashtable table)
    (unless (hash-table-table table)
      (set-hash-table-table! table
                             (match (hash-table-type table)
                               ('normal
                                (make-hashtable))
                               ('weak-key
                                (make-weak-key-hashtable))
                               ('weak-value
                                (make-weak-value-hashtable))
                               ('doubly-weak
                                (make-doubly-weak-hashtable))))))
  (define (maybe-init-eq-hashtable table)
    (unless (hash-table-table table)
      (set-hash-table-table! table
                             (match (hash-table-type table)
                               ('normal
                                (make-eq-hashtable))
                               ('weak-key
                                (make-eq-weak-key-hashtable))
                               ('weak-value
                                (make-eq-weak-value-hashtable))
                               ('doubly-weak
                                (make-eq-doubly-weak-hashtable))))))
  (define (maybe-init-eqv-hashtable table)
    (unless (hash-table-table table)
      (set-hash-table-table! table
                             (match (hash-table-type table)
                               ('normal
                                (make-eqv-hashtable))
                               ('weak-key
                                (make-eqv-weak-key-hashtable))
                               ('weak-value
                                (make-eqv-weak-value-hashtable))
                               ('doubly-weak
                                (make-eqv-doubly-weak-hashtable))))))
  (define (maybe-init-custom-hashtable table hash equiv)
    (unless (hash-table-table table)
      (set-hash-table-table! table
                             (match (hash-table-type table)
                               ('normal
                                (make-hashtable hash equiv))
                               ('weak-key
                                (make-weak-key-hashtable hash equiv))
                               ('weak-value
                                (make-weak-value-hashtable hash equiv))
                               ('doubly-weak
                                (make-doubly-weak-hashtable hash equiv))))))

  (define (%hash-ref table key default)
    (let ((table* (hash-table-table table)))
      (match (hash-table-type table)
        ('normal (hashtable-ref table* key default))
        ('weak-key (weak-key-hashtable-ref table* key default))
        ('weak-value (weak-value-hashtable-ref table* key default))
        ('doubly-weak (doubly-weak-hashtable-ref table* key default)))))
  (define* (hash-ref table key #:optional default)
    "Look up @var{key} in the hash table @var{table}, and return the
value associated with it.  If @var{key} is not found, return
@var{default} (if specified) or @code{#f}.  Uses @code{equal?} for
equality testing."
    (maybe-init-equal-hashtable table)
    (%hash-ref table key default))
  (define* (hashq-ref table key #:optional default)
    "Look up @var{key} in the hash table @var{table}, and return the
value associated with it.  If @var{key} is not found, return
@var{default} (if specified) or @code{#f}.  Uses @code{eq?} for
equality testing."
    (maybe-init-eq-hashtable table)
    (%hash-ref table key default))
  (define* (hashv-ref table key #:optional default)
    "Look up @var{key} in the hash table @var{table}, and return the
value associated with it.  If @var{key} is not found, return
@var{default} (if specified) or @code{#f}.  Uses @code{eqv?} for
equality testing."
    (maybe-init-eqv-hashtable table)
    (%hash-ref table key default))
  (define* (hashx-ref hash assoc table key #:optional default)
    "Look up @var{key} in the hash table @var{table}, and return the
value associated with it.  If @var{key} is not found, return
@var{default} (if specified) or @code{#f}.  Uses @var{hash} as the
hash function.  @var{assoc} is ignored and @code{equal?} is used for
equality testing."
    (maybe-init-custom-hashtable table hash equal?)
    (%hash-ref table key default))

  (define (%hash-set! table key val)
    (let ((table* (hash-table-table table)))
      (match (hash-table-type table)
        ('normal (hashtable-set! table* key val))
        ('weak-key (weak-key-hashtable-set! table* key val))
        ('weak-value (weak-value-hashtable-set! table* key val))
        ('doubly-weak (doubly-weak-hashtable-set! table* key val)))
      (if #f #f)))
  (define (hash-set! table key val)
    "Find the entry in @var{table} associated with @var{key} and store
@var{val} there.  Uses @code{equal?} for equality testing."
    (maybe-init-equal-hashtable table)
    (%hash-set! table key val))
  (define (hashq-set! table key val)
    "Find the entry in @var{table} associated with @var{key} and store
@var{val} there.  Uses @code{eq?} for equality testing."
    (maybe-init-eq-hashtable table)
    (%hash-set! table key val))
  (define (hashv-set! table key val)
    "Find the entry in @var{table} associated with @var{key} and store
@var{val} there.  Uses @code{eqv?} for equality testing."
    (maybe-init-eqv-hashtable table)
    (%hash-set! table key val))
  (define (hashx-set! hash assoc table key val)
    "Find the entry in @var{table} associated with @var{key} and store
@var{val} there.  Uses @var{hash} as the hash function.  @var{assoc}
is ignored and @code{equal?} is used for equality testing."
    (maybe-init-custom-hashtable table hash equal?)
    (%hash-set! table key val))

  (define (%hash-remove! table key)
    (let ((table* (hash-table-table table)))
      (match (hash-table-type table)
        ('normal (hashtable-delete! table* key))
        ('weak-key (weak-key-hashtable-delete! table* key))
        ('weak-value (weak-value-hashtable-delete! table* key))
        ('doubly-weak (doubly-weak-hashtable-delete! table* key)))
      (if #f #f)))
  (define (hash-remove! table key)
    "Remove @var{key} from @var{table}.  Uses @code{equal?} for equality
testing."
    (maybe-init-equal-hashtable table)
    (%hash-remove! table key))
  (define (hashq-remove! table key)
    "Remove @var{key} from @var{table}.  Uses @code{eq?} for equality
testing."
    (maybe-init-eq-hashtable table)
    (%hash-remove! table key))
  (define (hashv-remove! table key)
    "Remove @var{key} from @var{table}.  Uses @code{eqv?} for equality
testing."
    (maybe-init-eqv-hashtable table)
    (%hash-remove! table key))
  (define (hashx-remove! hash assoc table key)
    "Remove @var{key} from @var{table}.  Uses @var{hash} as the hash
function.  @var{assoc} is ignored and @code{equal?} is used for
equality testing."
    (maybe-init-custom-hashtable table hash equal?)
    (%hash-remove! table key))

  (define (hash-get-handle table key)
    (raise (make-unimplemented-error 'hash-get-handle)))
  (define (hashq-get-handle table key)
    (raise (make-unimplemented-error 'hashq-get-handle)))
  (define (hashv-get-handle table key)
    (raise (make-unimplemented-error 'hashv-get-handle)))
  (define (hashx-get-handle hash assoc table key)
    (raise (make-unimplemented-error 'hashx-get-handle)))

  (define (hash-create-handle! table key init)
    (raise (make-unimplemented-error 'hash-create-handle!)))
  (define (hashq-create-handle! table key init)
    (raise (make-unimplemented-error 'hashq-create-handle!)))
  (define (hashv-create-handle! table key init)
    (raise (make-unimplemented-error 'hashv-create-handle!)))
  (define (hashx-create-handle! hash assoc table key init)
    (raise (make-unimplemented-error 'hashx-create-handle!)))

  (define (hash-clear! table)
    "Remove all items from @var{table}."
    (match (hash-table-table table)
      (#f (values))
      (table*
       (match (hash-table-type table)
         ('normal (hashtable-clear! table*))
         ('weak-key (weak-key-hashtable-clear! table*))
         ('weak-value (weak-value-hashtable-clear! table*))
         ('doubly-weak (doubly-weak-hashtable-clear! table*)))))
    (if #f #f))

  (define (hash-fold proc init table)
    "Accumulate a result by applying @var{proc} with each key/value
association in @var{table} and the result of the previous @var{proc}
call.  Each call is of the form @code{(proc key value prev)}.  For the
first call, @code{prev} is the initial value @var{init}."
    (match (hash-table-table table)
      (#f init)
      (table*
       (match (hash-table-type table)
         ('normal (hashtable-fold proc init table*))
         ('weak-key (weak-key-hashtable-fold proc init table*))
         ('weak-value (weak-value-hashtable-fold proc init table*))
         ('doubly-weak (doubly-weak-hashtable-fold proc init table*))))))

  (define (hash-map->list proc table)
    "Return an association list of key/value mappings in @var{table}."
    (hash-fold (lambda (key value result)
                 (cons (proc key value) result))
               '() table))

  (define (hash-count pred table)
    "Return the number of elements in @var{table} that satisfy @code{(pred
key value)}."
    (hash-fold (lambda (key val count)
                 (if (pred key val)
                     (1+ count)
                     count))
               0 table))

  (define (hash-for-each proc table)
    "Apply @var{proc} to each key/value association in @var{table}.
Each call is of the form @code{(proc key value)}."
    (match (hash-table-table table)
      (#f (values))
      (table*
       (match (hash-table-type table)
         ('normal (hashtable-for-each proc table*))
         ('weak-key (weak-key-hashtable-for-each proc table*))
         ('weak-value (weak-value-hashtable-for-each proc table*))
         ('doubly-weak (doubly-weak-hashtable-for-each proc table*))))))

  (define (hash-for-each-handle proc table)
    (raise (make-unimplemented-error 'hash-for-each-handle)))

  ;; Regular expressions
  (define regexp/basic 'basic)
  (define regexp/extended 'extended)
  (define regexp/icase 'case-insensitive)
  (define regexp/newline 'multiline)
  (define regexp/notbol 'notbol)
  (define regexp/noteol 'noteol)

  ;; Sockets
  (define AF_INET 'ipv4)
  (define AF_INET6 'ipv6)
  (define (inet-pton family address)
    (define (bad-address)
      (error "bad address" family address))
    (define (check-u8 x)
      (unless (and x (<= 0 x 255)) (bad-address))
      x)
    (define (check-number x)
      (unless (number? x) (bad-address))
      x)
    (define (read-decimal port)
      (check-u8
       (string->number
        (list->string
         (let lp ()
           (match (peek-char port)
             ((and char (or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
              (cons (read-char port) (lp)))
             (_ '())))))))
    (define (hex-digit? char)
      (match char
        ((or #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
             #\a #\b #\c #\d #\e #\f
             #\A #\B #\C #\D #\E #\F)
         #t)
        (_ #f)))
    (define (read-hexadecimal port)
      (check-number
       (string->number
        (list->string
         (let lp ((k 0))
           (match (peek-char port)
             ((? hex-digit?)
              (if (= k 4)
                  (bad-address)
                  (cons (read-char port) (lp (1+ k)))))
             (_ '()))))
        16)))
    (define (read-dot port)
      (match (read-char port)
        (#\. (values))
        (_ (bad-address))))
    (define (read-colon port)
      (match (read-char port)
        (#\: (values))
        (_ (bad-address))))
    (define (read-decimal-and-dot port)
      (let ((n (read-decimal port)))
        (read-dot port)
        n))
    (define (read-hexadecimal-and-colon port)
      (let ((n (read-hexadecimal port)))
        (read-colon port)
        n))
    (define (read-ipv6-groups port)
      (define (iter)
        (match (peek-char port)
          ((? eof-object?) '())
          ((? hex-digit?)
           (let ((x (read-hexadecimal port)))
             (match (read-char port)
               ((? eof-object?) (list x))
               (#\: (cons x (iter))))))
          (#\:
           (read-char port)
           '())))
      (match (peek-char port)
        ((? eof-object?) '())
        ((? hex-digit?)
         (iter))
        (#\:
         (read-char port)
         (match (read-char port)
           (#\: '())
           (_ (bad-address))))))
    (match family
      ('ipv4
       (call-with-input-string address
         (lambda (port)
           (let ((a (read-decimal-and-dot port))
                 (b (read-decimal-and-dot port))
                 (c (read-decimal-and-dot port))
                 (d (read-decimal port)))
             (if (eof-object? (peek-char port))
                 (logior (ash a 24)
                         (ash b 16)
                         (ash c 8)
                         d)
                 (bad-address))))))
      ;; TODO: IPv6 addresses with embedded IPv4 address.
      ('ipv6
       (call-with-input-string address
         (lambda (port)
           (let* ((pre (read-ipv6-groups port))
                  (post (read-ipv6-groups port))
                  (pad (- 8 (+ (length pre) (length post)))))
             (if (> pad 0)
                 (match (append pre (make-list pad 0) post)
                   ((a b c d e f g h)
                    (logior (ash a 112)
                            (ash b 96)
                            (ash c 80)
                            (ash d 64)
                            (ash e 48)
                            (ash f 32)
                            (ash g 16)
                            h))
                   (_ (bad-address)))
                 (bad-address)))))))))
