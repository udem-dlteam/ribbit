#!/bin/sh

# @@(replace "41 59 39 117 63 62 118 68 63 62 118 82 68 63 62 118 82 65 63 62 118 82 65 63 62 118 82 58 63 62 118 82 61 33 40 58 108 107 109 33 39 58 108 107 118 54 121" (rvm-code-to-bytes (encode 92) " ")
set 41 59 39 117 63 62 118 68 63 62 118 82 68 63 62 118 82 65 63 62 118 82 65 63 62 118 82 58 63 62 118 82 61 33 40 58 108 107 109 33 39 58 108 107 118 54 121
# )@@

_DEBUG=true
# @@(feature (not debug)
_DEBUG=false
# )@@
                                                                # DEBUG
_show()                                                         # DEBUG
{                                                               # DEBUG
_obj=$1                                                         # DEBUG
if [ $_obj = ${_obj#R} ]; then                                  # DEBUG
  printf "%s" "$_obj"                                           # DEBUG
else                                                            # DEBUG
  eval _car=\$_X$_obj _cdr=\$_Y$_obj _type=\$_Z$_obj            # DEBUG
  if [ $_type = 4 ]; then                                       # DEBUG
    printf "#"                                                  # DEBUG
    eval _car=\$_X$_car _cdr=\$_Y$_car _type=\$_Z$_car          # DEBUG
  fi                                                            # DEBUG
  case $_type in                                                # DEBUG
    0)                                                          # DEBUG
      printf "(%s" "$_car"                                      # DEBUG
      _obj=$_cdr                                                # DEBUG
      while true; do                                            # DEBUG
        if [ $_obj = ${_obj#R} ]; then break; fi                # DEBUG
        eval _car=\$_X$_obj _cdr=\$_Y$_obj _type=\$_Z$_obj      # DEBUG
        if [ $_type != 0 ]; then break; fi                      # DEBUG
        printf " %s" "$_car"                                    # DEBUG
        _obj=$_cdr                                              # DEBUG
      done                                                      # DEBUG
      if [ $_obj = ${_obj#R} ]; then                            # DEBUG
        printf " . %s" "$_obj"                                  # DEBUG
      else                                                      # DEBUG
        if [ $_obj != $_N ]; then                               # DEBUG
          eval _car=\$_X$_obj _cdr=\$_Y$_obj _type=\$_Z$_obj    # DEBUG
          printf " . [%s,%s,%s]" "$_car" "$_cdr" "$_type"       # DEBUG
        fi                                                      # DEBUG
      fi                                                        # DEBUG
      printf ")"                                                # DEBUG
      ;;                                                        # DEBUG
    1)                                                          # DEBUG
      if [ $_car = ${_car#R} ]; then                            # DEBUG
        printf "#<primitive %s>" "$_obj"                        # DEBUG
      else                                                      # DEBUG
        eval _car=\$_X$_car _cdr=\$_Y$_car _type=\$_Z$_car      # DEBUG
        printf "#<procedure %s nparams=%s>" "$_obj" "$_car"     # DEBUG
      fi                                                        # DEBUG
      ;;                                                        # DEBUG
    2)                                                          # DEBUG
      eval chars=\$_X$_cdr                                      # DEBUG
      if [ $chars = $_N ]; then                                 # DEBUG
        printf "%s" "$_obj"                                     # DEBUG
      else                                                      # DEBUG
        while true; do                                          # DEBUG
          if [ $chars = ${chars#R} ]; then break; fi            # DEBUG
          eval _car=\$_X$chars _cdr=\$_Y$chars _type=\$_Z$chars # DEBUG
          if [ $_type != 0 ]; then break; fi                    # DEBUG
          printf \\$(($_car/64))$(($_car/8%8))$(($_car%8))      # DEBUG
          chars=$_cdr                                           # DEBUG
        done                                                    # DEBUG
      fi                                                        # DEBUG
      ;;                                                        # DEBUG
    3)                                                          # DEBUG
      chars=$_car                                               # DEBUG
      printf "\""                                               # DEBUG
      while true; do                                            # DEBUG
        if [ $chars = ${chars#R} ]; then break; fi              # DEBUG
        eval _car=\$_X$chars _cdr=\$_Y$chars _type=\$_Z$chars   # DEBUG
        if [ $_type != 0 ]; then break; fi                      # DEBUG
        printf \\$(($_car/64))$(($_car/8%8))$(($_car%8))        # DEBUG
        chars=$_cdr                                             # DEBUG
      done                                                      # DEBUG
      printf "\""                                               # DEBUG
      ;;                                                        # DEBUG
    5)                                                          # DEBUG
      if [ $_obj = $_F ]; then                                  # DEBUG
        printf "#f"                                             # DEBUG
      else                                                      # DEBUG
        if [ $_obj = $_T ]; then                                # DEBUG
          printf "#t"                                           # DEBUG
        else                                                    # DEBUG
          if [ $_obj = $_N ]; then                              # DEBUG
            printf "()"                                         # DEBUG
          else                                                  # DEBUG
            printf "[%s,%s,%s]" "$_car" "$_cdr" "$_type"        # DEBUG
          fi                                                    # DEBUG
        fi                                                      # DEBUG
      fi                                                        # DEBUG
      ;;                                                        # DEBUG
    *)                                                          # DEBUG
      printf "[%s,%s,%s]" "$_car" "$_cdr" "$_type"              # DEBUG
      ;;                                                        # DEBUG
  esac                                                          # DEBUG
fi                                                              # DEBUG
}                                                               # DEBUG
                                                                # DEBUG
_showln()                                                       # DEBUG
{                                                               # DEBUG
_show "$1"                                                      # DEBUG
printf "\n"                                                     # DEBUG
}                                                               # DEBUG
                                                                # DEBUG
_MARKCOUNT=0                                                    # DEBUG
                                                                # DEBUG
_dump()                                                         # DEBUG
{                                                               # DEBUG
_MARKCOUNT=$((_MARKCOUNT+1))                                    # DEBUG
_todo=$1                                                        # DEBUG
while [ "$_todo" != "" ]; do                                    # DEBUG
  _willdo=$_todo                                                # DEBUG
  _todo=                                                        # DEBUG
  for _x in $_willdo; do                                        # DEBUG
    if [ $_x != ${_x#R} ]; then                                 # DEBUG
      eval marked=\${_M$_x-0}                                   # DEBUG
      eval _M$_x=$_MARKCOUNT                                    # DEBUG
      if [ "$marked" != $_MARKCOUNT ]; then                     # DEBUG
        eval _AA=\$_X$_x _BB=\$_Y$_x _CC=\$_Z$_x                # DEBUG
        printf "%s = [%s,%s,%s]\n" $_x $_AA $_BB $_CC           # DEBUG
        _todo="$_todo $_AA $_BB $_CC"                           # DEBUG
      fi                                                        # DEBUG
    fi                                                          # DEBUG
  done                                                          # DEBUG
done                                                            # DEBUG
}                                                               # DEBUG
                                                                # DEBUG
_dump_heap()                                                    # DEBUG
{                                                               # DEBUG
_i=1                                                            # DEBUG
while [ $_i -le $_H ]; do                                       # DEBUG
  eval _AA=\$_XR$_i _BB=\$_YR$_i _CC=\$_ZR$_i                   # DEBUG
  printf "R%s = [%s,%s,%s]\n" $_i $_AA $_BB $_CC                # DEBUG
  _i=$((_i+1))                                                  # DEBUG
done                                                            # DEBUG
}                                                               # DEBUG

_VAR()
{
if [ $_C = ${_C#R} ]; then
  _B=$_S
  while [ $_C != 0 ]; do eval _B=\$_Y$_B; _C=$((_C-1)); done
  _C=$_B
fi
}

_GETCONT()
{
_I=$_S; eval _B=\$_Y$_I _P=\$_Z$_I
while [ $_P = 0 ]; do _I=$_B; eval _B=\$_Y$_I _P=\$_Z$_I; done
}

_PUSH()
{
_H=$((_H+1)); eval _XR$_H=$_C _YR$_H=$_S _ZR$_H=0; _S=R$_H
}

_GC()
{
eval _X$_T=$_S _Y$_T=$_P

_H=$((_H+1)) _I=$_H
while [ $_I -gt 0 ]; do eval _MR$_I=0; _I=$((_I-1)); done

_C=$_F _P=0

while true; do
  eval _I=\$_M$_C
  while [ $_I = 0 ]; do
    eval _M$_C=1
    _B=$_C
    eval _C=\$_X$_C _X$_B=$_P
    _P=$_B
    if [ $_C = ${_C#R} ]; then break; fi
    eval _I=\$_M$_C
  done
  while [ $_P != 0 ]; do
    eval _I=\$_M$_P
    if [ $_I = 1 ]; then
      eval _M$_P=2
      eval _B=\$_X$_P _X$_P=$_C
      eval _C=\$_Y$_P _Y$_P=$_B
      if [ $_C != ${_C#R} ]; then break; fi
    fi
    if [ $_I = 2 ]; then
      eval _M$_P=3
      eval _B=\$_Y$_P _Y$_P=$_C
      eval _C=\$_Z$_P _Z$_P=$_B
      if [ $_C != ${_C#R} ]; then break; fi
    fi
    if [ $_I = 3 ]; then
      eval _B=\$_Z$_P _Z$_P=$_C
      _C=$_P _P=$_B
      break
    fi
  done

  if [ $_P = 0 ]; then break; fi
done

_I=1 _P=$_H
while true; do
  eval _B=\$_MR$_I
  while [ $_B != 0 ]; do _I=$((_I+1)); eval _B=\$_MR$_I; done
  eval _B=\$_MR$_H
  while [ $_B = 0 ]; do _H=$((_H-1)); eval _B=\$_MR$_H; done
  if [ $_I -ge $_H ]; then break; fi
  eval _XR$_I=\$_XR$_H _YR$_I=\$_YR$_H _ZR$_I=\$_ZR$_H _MR$_I=1 _MR$_H=0 _XR$_H=$_I
  _I=$((_I+1)) _H=$((_H-1))
done

_I=$_H _L=$((_H*10+5000))

while [ $_I != 0 ]; do
  eval _C=\$_XR$_I
  if [ $_C != ${_C#R} ]; then
    eval _B=\$_M$_C
    if [ $_B = 0 ]; then eval _XR$_I=R\$_X$_C; fi
  fi
  eval _C=\$_YR$_I
  if [ $_C != ${_C#R} ]; then
    eval _B=\$_M$_C
    if [ $_B = 0 ]; then eval _YR$_I=R\$_X$_C; fi
  fi
  eval _C=\$_ZR$_I
  if [ $_C != ${_C#R} ]; then
    eval _B=\$_M$_C
    if [ $_B = 0 ]; then eval _ZR$_I=R\$_X$_C; fi
  fi
  _I=$((_I-1))
done

eval _S=\$_X$_T _P=\$_Y$_T
}

# build #f, #t, and ()

_H=0
_H=$((_H+1)) _F=R$_H; eval _XR$_H=0 _YR$_H=0 _ZR$_H=5
_H=$((_H+1)) _T=R$_H; eval _XR$_H=0 _YR$_H=0 _ZR$_H=5 _X$_F=$_T
_H=$((_H+1)) _N=R$_H; eval _XR$_H=0 _YR$_H=0 _ZR$_H=5 _Y$_F=$_N

# build symbol table

_P=0 _C=46
while [ $_C -gt 45 ]; do
  _P=$((_P*46+_C-46)); _C=$(($1-35)); shift; if [ $_C -lt 0 ]; then _C=57; fi
done
_P=$((_P*46+_C))

_B=$_N _S=$_N _I=0 _C=0; if [ $_P = 0 ]; then _C=$1; shift; fi
while true; do
  if [ $_P -gt 0 -o $_C = 44 -o $_C = 59 ]; then
    _H=$((_H+1)); eval _XR$_H=$_S _YR$_H=$_I _ZR$_H=3; _S=R$_H # list->string
    _H=$((_H+1)); eval _XR$_H=$_F _YR$_H=$_S _ZR$_H=2; _S=R$_H # string->ui-symbol
    _H=$((_H+1)); eval _XR$_H=$_S _YR$_H=$_B _ZR$_H=0; _B=R$_H
    _S=$_N _I=0 _P=$((_P-1))
    if [ $_C = 59 ]; then break; fi
    if [ $_P -lt 1 ]; then _C=$1; shift; fi
  else
    _H=$((_H+1)); eval _XR$_H=$_C _YR$_H=$_S _ZR$_H=0; _S=R$_H
    _I=$((_I+1)) _C=$1; shift
  fi
done

# decode compacted RVM code

while true; do
  _I=0 _C=$(($1-35)); shift; if [ $_C -lt 0 ]; then _C=57; fi
  for _P in 23 33 3 13 14 7; do
    if [ $_P -gt $_C ]; then break; fi
    _I=$((_I+1)) _C=$((_C-_P))
  done
  if [ $_I = 0 ]; then
    _H=$((_H+1)); eval _XR$_H=0 _YR$_H=$_S _ZR$_H=0; _S=R$_H
  else
    if [ $_I = 5 -a $_C = 5 ]; then
      eval _P=\$_X$_S _S=\$_Y$_S
      _H=$((_H+1)); eval _XR$_H=4 _YR$_H=$_P _ZR$_H=\$_X$_S _X$_S=R$_H
      continue
    fi
    _I=$((_I-1))
  fi
  _P=$((_C-_P+3)) _L=0
  if [ $_P -lt 0 ]; then
    _P=$_C
    if [ $_I -lt 2 ]; then _L=1; fi
  else
    if [ $_P -gt 0 ]; then _P=$((_P-1)); _L=1; fi
    _C=$(($1-35)); shift; if [ $_C -lt 0 ]; then _C=57; fi
    while [ $_C -gt 45 ]; do
      _P=$((_P*46+_C-46)); _C=$(($1-35)); shift; if [ $_C -lt 0 ]; then _C=57; fi
    done
    _P=$((_P*46+_C))
  fi

  if [ $_L = 1 ]; then
    _L=$_P _P=$_B
    while [ $_L -gt 0 ]; do
      eval _P=\$_Y$_P
      _L=$((_L-1))
    done
    eval _P=\$_X$_P
  fi

  if [ $_I -gt 3 ]; then
    _H=$((_H+1)) _I=R$_H; eval _XR$_H=$_P _YR$_H=0 _ZR$_H=\$_X$_S
    _H=$((_H+1)) _P=R$_H; eval _XR$_H=$_I _YR$_H=$_N _ZR$_H=1
    eval _S=\$_Y$_S
    if [ $_S = $_N ]; then break; fi
    _I=3
  fi

  _H=$((_H+1)); eval _XR$_H=$_I _YR$_H=$_P _ZR$_H=\$_X$_S _X$_S=R$_H

done

# setup rib, false, true, and nil global variables

eval _S=\$_X$_B _C=\$_Y$_B
_H=$((_H+1)); eval _XR$_H=0 _YR$_H=$_B _ZR$_H=1 _X$_S=R$_H _S=\$_X$_C _C=\$_Y$_C
eval _X$_S=$_F _S=\$_X$_C _C=\$_Y$_C
eval _X$_S=$_T _S=\$_X$_C _C=\$_Y$_C
eval _X$_S=$_N

# build initial continuation

_H=$((_H+1)) _B=R$_H; eval _XR$_H=5 _YR$_H=0 _ZR$_H=0 _P=\$_X$_P
_H=$((_H+1)) _S=R$_H; eval _XR$_H=0 _YR$_H=0 _ZR$_H=$_B _P=\$_Z$_P

_L=$((_H*10+5000))

# RVM code interpreter main loop

while true; do
  if [ $_H -ge $_L ]; then _GC; fi
  if $_DEBUG; then printf ".............. STACK = "; _showln $_S; fi # DEBUG
  eval _B=\$_X$_P _C=\$_Y$_P _P=\$_Z$_P
  case $_B in
    0)
      if $_DEBUG; then if [ $_P = 0 ]; then printf "jump "; else printf "call "; fi; _showln $_C; fi # DEBUG
      _VAR
      # @@(feature arity-check 
      eval _K=\$_X$_S  # NARGS
      eval _S=\$_Y$_S
      # )@@

      eval _C=\$_X$_C
      eval _B=\$_X$_C
      if [ $_B = ${_B#R} ]; then

case $_B in
  # @@(primitives (gen "\n" index ")\n" body)
  0) # @@(primitive (rib a b c)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then eval printf "\"(rib %s %s %s)\"" \$_X$_S $_B $_C; fi # DEBUG
    _H=$((_H+1)); eval _XR$_H=\$_X$_S _YR$_H=$_B _ZR$_H=$_C _S=\$_Y$_S; _C=R$_H
    _PUSH
    ;; # )@@
  1) # @@(primitive (id x)
    if $_DEBUG; then eval printf "\"(id %s)\"" \$_X$_S; fi # DEBUG
    ;; # )@@
  2) # @@(primitive (arg1 x y)
    eval _C=\$_X$_S # DEBUG
    eval _S=\$_Y$_S
    if $_DEBUG; then eval printf "\"(arg1 %s %s)\"" \$_X$_S $_C; fi # DEBUG
    ;; # )@@
  3) # @@(primitive (arg2 x y)
    eval _C=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then eval printf "\"(arg2 %s %s)\"" \$_X$_S $_C; fi # DEBUG
    eval _S=\$_Y$_S
    _PUSH
    ;; # )@@
  4) # @@(primitive (close rib)
    eval _C=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(close %s)" $_C; fi # DEBUG
    _H=$((_H+1)); eval _XR$_H=\$_X$_C _YR$_H=$_S _ZR$_H=1; _C=R$_H
    _PUSH
    ;; # )@@
  5) # @@(primitive (rib? rib)
    eval _C=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(rib? %s)" $_C; fi # DEBUG
    if [ $_C = ${_C#R} ]; then _C=$_F; else _C=$_T; fi
    _PUSH
    ;; # )@@
  6) # @@(primitive (field0 rib)
    eval _C=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(field0 %s)" $_C; fi # DEBUG
    eval _C=\$_X$_C
    _PUSH
    ;; # )@@
  7) # @@(primitive (field1 rib)
    eval _C=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(field1 %s)" $_C; fi # DEBUG
    eval _C=\$_Y$_C
    _PUSH
    ;; # )@@
  8) # @@(primitive (field2 rib)
    eval _C=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(field2 %s)" $_C; fi # DEBUG
    eval _C=\$_Z$_C
    _PUSH
    ;; # )@@
  9) # @@(primitive (field0-set! rib)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(field0-set! %s %s)" $_B $_C; fi # DEBUG
    eval _X$_B=$_C
    _PUSH
    ;; # )@@
 10) # @@(primitive (field1-set! rib)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(field1-set! %s %s)" $_B $_C; fi # DEBUG
    eval _Y$_B=$_C
    _PUSH
    ;; # )@@
 11) # @@(primitive (field2-set! rib)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(field2-set! %s %s)" $_B $_C; fi # DEBUG
    eval _Z$_B=$_C
    _PUSH
    ;; # )@@
 12) # @@(primitive (eqv? rib1 rib2)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(eqv? %s %s)" $_B $_C; fi # DEBUG
    if [ $_B = $_C ]; then _C=$_T; else _C=$_F; fi
    _PUSH
    ;; # )@@
 13) # @@(primitive (< a b)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(< %s %s)" $_B $_C; fi # DEBUG
    if [ $_B -lt $_C ]; then _C=$_T; else _C=$_F; fi
    _PUSH
    ;; # )@@
 14) # @@(primitive (+ x y)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(+ %s %s)" $_B $_C; fi # DEBUG
    _C=$((_B+_C))
    _PUSH
    ;; # )@@
 15) # @@(primitive (- x y)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(- %s %s)" $_B $_C; fi # DEBUG
    _C=$((_B-_C))
    _PUSH
    ;; # )@@
 16) # @@(primitive (* x y)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(* %s %s)" $_B $_C; fi # DEBUG
    _C=$((_B*_C))
    _PUSH
    ;; # )@@
 17) # @@(primitive (quotient x y)
    eval _C=\$_X$_S _S=\$_Y$_S; eval _B=\$_X$_S _S=\$_Y$_S
    if $_DEBUG; then printf "(quotient %s %s)" $_B $_C; fi # DEBUG
    _C=$((_B/_C))
    _PUSH
    ;; # )@@
 18) # @@(primitive (getchar)
    if $_DEBUG; then printf "(getchar)"; fi # DEBUG
    if [ $# = 0 ]; then
      if [ -t 0 ]; then
        set 0 `sed q | od -v -A n -t u1`
      else
        set 0 `od -v -A n -t u1`
      fi
      if [ $# = 1 ]; then set 0 -1; fi; shift
    fi
    _C=$1; shift
    _PUSH
    ;; # )@@
 19) # @@(primitive (putchar c)
    eval _C=\$_X$_S
    if $_DEBUG; then printf "(putchar %s)" $_C; fi # DEBUG
    printf \\$(($_C/64))$(($_C/8%8))$(($_C%8))
    ;; # )@@
 20) # @@(primitive (exit n)
    if $_DEBUG; then eval printf "\"(exit %s)\\n\"" \$_X$_S; fi # DEBUG
    eval exit \$_X$_S
    ;; # )@@
  # )@@
esac
if $_DEBUG; then eval printf "\" -> %s\\n\"" \$_X$_S; fi # DEBUG

        if [ $_P = ${_P#R} ]; then
          _GETCONT
          eval _Y$_S=\$_X$_I
        fi
      else
        _H=$((_H+1)); eval _XR$_H=0 _YR$_H=$_C _ZR$_H=0; _C=R$_H _A=R$_H _Q=$_B
        eval _I=\$_X$_Q
        
        _G=$((_I/2)) # Number of arguments
        # @@(feature rest-param (use arity-check)
        _V=$((_I%2))
        if  [ "$_V" -eq "0" -a "$_G" -ne "$_K" ] || [ "$_V" -eq "1" -a "$_G" -gt "$_K" ] ; then 
            echo "*** Unexpected number of arguments nargs: " $_K " nparams: " $_G " Variadic : " $_V
            exit 1
        fi
        _K=$((_K-_G))
        if [ "$_V" -eq 1 ]; then 
            _R=$_N

            while [ $_K -gt 0 ]; do
                _H=$((_H+1)); eval _XR$_H=\$_X$_S _YR$_H=$_R _ZR$_H=0 _S=\$_Y$_S; _R=R$_H
                _K=$((_K-1))
            done

            _H=$((_H+1)); eval _XR$_H=$_R _YR$_H=$_A _ZR$_H=0; _A=R$_H
        fi
        # )@@
        
        while [ $_G -gt 0 ]; do
          _H=$((_H+1)); eval _XR$_H=\$_X$_S _YR$_H=$_A _ZR$_H=0 _S=\$_Y$_S; _A=R$_H
          _G=$((_G-1))
        done
        if [ $_P = ${_P#R} ]; then
          _GETCONT
          eval _X$_C=\$_X$_I _Z$_C=$_P
        else
          eval _X$_C=$_S _Z$_C=$_P
        fi
        eval _P=\$_Z$_Q; _S=$_A
      fi
      ;;
    1)
      if $_DEBUG; then printf "set "; _showln $_C; fi # DEBUG
      _VAR
      eval _X$_C=\$_X$_S _S=\$_Y$_S
      ;;
    2)
      if $_DEBUG; then printf "get "; _showln $_C; fi # DEBUG
      _VAR
      eval _C=\$_X$_C
      _PUSH
      ;;
    3)
      if $_DEBUG; then printf "const "; _showln $_C; fi # DEBUG
      _PUSH
      ;;
    4)
      if $_DEBUG; then printf "if\n"; fi # DEBUG
      eval _B=\$_X$_S _S=\$_Y$_S
      if [ $_B != $_F ]; then _P=$_C; fi
      ;;
    5)
      if $_DEBUG; then printf "halt\n"; fi # DEBUG
      exit
      ;;
  esac
done
