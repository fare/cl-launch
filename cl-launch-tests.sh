foo_provide () {
  echo "(tst \"$1\"(defparameter *$2* 0)(defvar *err* 0)(format t \"--$2 worked, \"))"
}
foo_require () {
  echo "(tst \"$1\"(defvar *$2* 1)(defvar *err* 0)(incf *err* *$2*)
(unless (zerop *$2*) (format t \"--$2 ~A, \" :failed)))"
}
t_env () {
[ -n "$BEGIN_TESTS" ] && return
export DOH=doh
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations :inherit-configuration (\"$PWD\" (\"$PWD\" \"cache\")))"
TCURR=
BEGIN_TESTS='(in-package :cl-user)
;;(eval-when (:compile-toplevel) (format *trace-output* "~&Prologue compiled~%"))
;;(eval-when (:load-toplevel) (format *trace-output* "~&Prologue loaded~%"))
;;(eval-when (:execute) (format *trace-output* "~&Prologue executed~%"))
#+gcl (si::use-fast-links nil) ;; enable debugging information.
(defmacro tst (x &body body) `(eval-when (:compile-toplevel :load-toplevel :execute)(handler-bind ((warning (function muffle-warning))) (eval (quote (progn (defvar *f* ()) (defparameter *n* ,x) (push (quote(progn ,@body)) *f*)))))))
(defparameter *f* ())(defvar *n*)
(defun tt () (dolist (x (reverse *f*)) (eval x)))
(tst`:begin-tests(defvar *err* 0)(defvar *begin* 0)
(format t "Hello, world, ~A speaking.~%" (uiop:implementation-identifier)))
'
END_TESTS="$(foo_require t begin)"'
(tst t(if (equal "won" (first uiop:*command-line-arguments*))
(format t "argument passing worked, ")
(progn (incf *err*) (format t "argument passing failed,~%*c-l-a* = ~S~%r-c-l-a = ~S~%c-l-a = ~S~%"
uiop:*command-line-arguments* (uiop:raw-command-line-arguments) (uiop:command-line-arguments))))
(if (equal "doh" (cl-launch::getenv "DOH")) (format t "getenv worked, ")
(progn (incf *err*) (format t "getenv failed, ")))
(if (zerop *err*) (format t "all tests ~a~a.~%" :o :k) (format t "~a ~a.~%" :error :detected)))'
case "$LISP" in
  ecl) CLOUT="$PWD/clt-out-sh" ;;
  *) CLOUT="$PWD/clt-out.sh" ;;
esac
TFILE="clt-src.lisp"
}
t_begin () {
  remain="$#" ARGS= TORIG= TOUT= TINC2=
  HELLO="$BEGIN_TESTS" GOODBYE= TESTS="" BEGUN= ENDING="$END_TESTS"
  t_lisp "$@" t_end ;}
t_lisp () { if [ -n "$LISP" ] ; then
  ARGS="--lisp $LISP" ; "$@" --lisp $LISP ; else "$@" ; fi ;}
t_end () { if [ -n "$TEXEC" ] ; then t_end_exec "$@" ;
  else t_end_out "$@" ; fi ;}
t_register () {
  # SHOW t_register "$@" ; print_var remain HELLO GOODBYE
  BEGUN=t
  HELLO="$HELLO$TESTS"
  if [ $remain = 1 ] || { [ $remain = 2 ] && [ "t_noinit" = "$2" ]; } ; then
    GOODBYE="$1$ENDING" TESTS= ENDING=
    #foo=1
  else
    GOODBYE="" TESTS="$1"
    #foo=2
  fi
  # print_var HELLO GOODBYE foo
}
t_next () { remain=$(($remain-1)) ; [ -n "$BEGUN" ] && HELLO= ; "$@" ;}
t_args () { ARGS="$ARGS $1" ;}
t_create () {
  create_file 644 "$1" echo "$2"
  TFILES="$TFILES $1" ;}
t_cleanup () { rm $TFILES cache/* ; rmdir cache ;}
t_file () {
  t_register "$(foo_require "$NUM:file" file)" $1
  t_create $TFILE \
	"(in-package :cl-user)
$HELLO
$(foo_provide "$NUM:file" file)
$GOODBYE"
  if [ -n "$TINC2" ] ; then t_args "--file /..." ;
    else t_args "--file ..." ; fi
  t_next "$@" --file "$TFILE"
}
t_system () {
  t_register "$(foo_require "$NUM:system" system)" $1
  t_create clt-asd.asd \
	'(in-package :cl-user)(asdf:defsystem :clt-asd :components ((:file "clt-sys")))'
  t_create clt-sys.lisp \
	"(in-package :cl-user)$HELLO$(foo_provide "$NUM:system" system)$GOODBYE"
  t_args "--system ..."
  t_next "$@" --system clt-asd --source-registry \
  "(:source-registry \
     (:directory \"${PWD}\") \
     :ignore-inherited-configuration)"
  # (:tree ${ASDF_DIR}) \
}
t_init () {
  t_register "$(foo_require "$NUM:init" init)" xxx_t_init
  t_args "--init ..."
  t_next "$@" --init "$HELLO$(foo_provide "$NUM:init" init)$GOODBYE(tt)"
}
t_noinit () {
  t_args "--restart ..."
  t_next "$@" --restart cl-user::tt
}
t_image () {
 t_args "--image ..."
 t_register "$(foo_require "$NUM:image" image)" $1
 t_create clt-preimage.lisp \
       "(in-package :cl-user)$HELLO$(foo_provide "$NUM:image" image)$GOODBYE"
 if ! [ -f clt.preimage ] ; then
   t_make --dump clt.preimage --file clt-preimage.lisp --output clt-preimage.sh
 fi
 t_next "$@" --image "$PWD/clt.preimage"
}
t_dump () {
  t_args "--dump ..."
  t_next "$@" --dump "$PWD/clt.image"
}
t_dump_ () {
  t_args "--dump !"
  t_next "$@" --dump "!"
}
t_inc () {
  ( OPTION --include "$PWD" -B install_path ) >&2
  t_args "--include ..."
  t_next "$@" --include "$PWD"
}
t_inc1 () {
  TFILE=clt-src.lisp ; t_inc "$@"
}
t_inc2 () {
  TINC2=t TFILE="$PWD/clt-src.lisp" ; t_inc "$@"
}
t_noinc () {
  t_args "--no-include"
  t_next "$@" --no-include
}
t_update () {
  t_args "--update ..."
  TORIG=$CLOUT.orig ; cp -f $CLOUT $TORIG
  t_next "$@" --update $CLOUT
}
t_noupdate () {
  TORIG=
  t_next "$@"
}
t_end_out () {
  t_args "--output ... ; out.sh ..."
  TOUT=$CLOUT
  t_make "$@" --output $CLOUT
  t_check $CLOUT
}
t_end_exec () {
  t_args "--execute -- ..."
  t_check t_make "$@" --execute --
}
t_make () {
  XDO t_$TEST_SHELL -x $PROG "$@"
}
t_check () {
  echo "cl-launch $ARGS"
  ( PATH=${PWD}:$PATH "$@" "won" 2>&1) | tee clt.log >&2
  : RESULTS: "$(cat clt.log)"
  if [ -n "$TORIG" ] && [ -n "$TOUT" ] && ! cmp --quiet $TOUT $TORIG ; then
    echo "the updated file differs from the original one, although execution might not show the difference. Double check that with:
	diff -uN $TORIG $TOUT | less - $TORIG
"
    t_check_failed
  elif [ 0 = "$(grep -c OK < clt.log)" ] || [ 0 != "$(grep -c 'ERROR\(:\| DETECTED\)' < clt.log)" ] ; then
    t_check_failed
  else
    t_check_success
  fi
}
t_check_success () {
  echo "success with test $NUM :-)"
  return 0
}
t_check_failed () {
  echo "FAILURE with test $NUM :-("
  [ -n "$NUM" ] && echo "You may restart from this test with:
	$PROG -l $(kwote1 "$LISPS") -B tests $NUM
or
	$PROG -l $(kwote1 "$LISPS") -B tests $(printf %02d $(( ( $num / 4 ) * 4 )) )"
  [ -n "$TCURR" ] && echo "You may re-run just this test with:
	$PROG -B redo_test $TEST_SHELL $LISP $TCURR"
  [ -n "$NO_STOP" ] || ABORT "FIX THAT BUG!"
}
t_out () {
  t_env ; TEXEC= ; t_begin "$@"
}
t_exec () {
  t_env ; TEXEC=t ; t_begin "$@"
}
clisp_tests () { LISPS=clisp ; tests "$@" ;}
all_tests () { NO_STOP=t ; tests "$@" ;}
tests () {
  do_tests "$@" 2> tests.log
}
detect_program () {
  which "$1" 2>&1 > /dev/null
}
detect_shells () {
  # add something wrt ksh, pdksh ?
  TEST_SHELLS=
  for i in sh posh dash zsh pdksh bash busybox ; do
    if detect_program $i ; then
      TEST_SHELLS="$TEST_SHELLS $i"
    fi
  done
}
t_sh () { sh "$@" ;}
t_bash () { bash "$@" ;}
t_posh () { posh "$@" ;}
t_pdksh () { pdksh "$@" ;}
t_dash () { dash "$@" ;}
t_zsh () { zsh -fy "$@" ;}
t_busybox () { busybox sh "$@" ;}
shell_tests () {
  detect_shells
  tests "$@"
}

do_tests () {
  if [ -n "$TEST_SHELLS" ] ; then
    echo "Using test shells $TEST_SHELLS"
  fi
  t_env
  num=0 MIN=${1:-0} MAX=${2:-999999}
  export LISP
  # Use this with
  #    cl-launch.sh -B test
  # beware, it will clobber then remove a lot of file clt-*
  # and exercise your Lisp fasl cache
  for LISP in $LISPS ; do
  case $LISP in
    *) export ASDF_DIR="$($PROG --lisp "$LISP" --quiet --system asdf --init '(uiop:format! t "~%~:@(~A-~A~): ~S~%" :source :registry (asdf:system-source-directory :asdf))' | grep ^SOURCE-REGISTRY: | tail -1 | cut -d' ' -f2- )" ;;
  esac
  for TEST_SHELL in ${TEST_SHELLS:-${TEST_SHELL:-sh}} ; do
  echo "Using lisp implementation $LISP with test shell $TEST_SHELL"
  for TM in "" "image " ; do
  for TD in "" "dump " "dump_ " ; do
  case "$TM:$TD:$LISP" in
    # we don't know how to dump from a dump with ECL
    image*:dump*:ecl) ;;
    # we don't know how to dump at all with ABCL, XCL
    *:dump*:abcl|image*:*:abcl|*:dump*:xcl|image*:*:xcl) ;;
    # Unidentified bug using image on CLISP as of 4.0.7.9
    image*:clisp) ;;
    *)
  for IF in "noinc" "noinc file" "inc" "inc1 file" "inc2 file" ; do
  TDIF="$TM$TD$IF"
  for TS in "" " system" ; do
  TDIFS="$TDIF$TS"
  case "$TD:$TS:$LISP" in
    dump_*:cmucl*|dump_*:gcl*|dump_*:allegro|dump_*:scl)
      : invalid or unsupported combo ;; # actually only available for ecl and sbcl
    *)
  for TI in "noinit" "init" ; do
  TDIFSI="$TDIFS $TI"
  case "$TDIFSI" in
    *"inc noinit") : skipping invalid combination ;;
    *)
  for TU in "noupdate" "update" ; do
  TUDIFSI="$TU $TDIFSI"
  for TO in "exec" "out" ; do
  case "$TU:$TO:$TD" in
    update:*:dump_*) : invalid combo ;;
    *:exec:dump_*) : invalid combo ;;
    *)
  TEUDIFSI="$TO $TUDIFSI"
  do_test $TEUDIFSI
  ;; esac ; done ; done ;; esac ; done ;; esac ; done ; done ; esac ; done ; done ; done ; done
}
redo_test () {
  export TEST_SHELL="$1" LISPS="$2" LISP="$2" ; shift 2
  do_test "$@"
}
do_test () {
  if [ $MIN -le $num ] && [ $num -le $MAX ] ; then
    TCURR="$*"
    if [ -n "$num" ] ; then
      NUM=$(printf "%02d" $num)
      case "$*" in
        *noupdate*)
        # If we don't clean between runs of test/update, then
        # we have bizarre transient failures at test 12 or 40 when we e.g.
        #        DEBUG_RACE_CONDITION=t cl-launch -l clisp -B tests 8 12
        # There is some race condition somewhere in the cacheing layer,
        # and even though (trace ...) shows that cl-launch does try to
        # recompile then file, when it loads, it still find the old version in the cache.
        [ -n "$DEBUG_RACE_CONDITION" ] || test_clean
	;;
      esac
    fi
    eval "$(for i ; do ECHOn " t_$i" ; done)"
  fi
  num=$(($num+1))
}
test () {
  tests $@ && test_clean
}
test_clean () {
  rm -rfv clt* cache/ >&2
}
fakeccl () {
  DO export LISP=ccl CCL=sbcl CCL_OPTIONS="--noinform --sysinit /dev/null --userinit /dev/null --eval (make-package':ccl) --eval (setf(symbol-function'ccl::quit)(symbol-function'sb-ext:quit)) --eval (setf(symbol-function'ccl::getenv)(symbol-function'sb-ext:posix-getenv))"
  OPTION "$@"
}
