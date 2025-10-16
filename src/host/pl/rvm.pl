# @@(location import)@@

# @@(replace ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y" (encode 92)
$input = q");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y";    # RVM code that prints HELLO!
# )@@

use strict;
use warnings;

our $pos = -1;
our $stack = 0;
our $FALSE = [ 0, 0, 5 ];
our $TRUE  = [ 0, 0, 5 ];
our $NIL   = [ 0, 0, 5 ];

########################################################
## Interactions avec le STDIN/STDOUT et traitement.
########################################################

# @@(location decl)@@

sub get_byte {
  $pos++;
  my $my_input = $main::input;
  return ord( substr( $my_input, $pos, 1 ) );
}
sub get_code {
    my $x = get_byte() - 35;
    return ( $x < 0 ) ? 57 : $x;
}
sub get_int {
    my ($n) = @_;
    $n *= 46;
    my $x = get_code();
    return ( $x < 46 ) ? $n + $x : get_int( $n + $x - 46 );
}

# @@(feature %%getchar
sub getchar {
    my $c = getc(STDIN);        # Read from the stdin
    &push( defined $c ? ord($c) : -1 );
    return;
}
# )@@

# @@(feature %%putchar
sub putchar {
  my ($args) = @_;
  print STDOUT chr($args);    # Write to the stdout
  STDOUT->flush();            # Force immediate write via autoflush
  return $args;
}
# )@@

########################################################
## Interactons avec la stack.
########################################################

sub push {
    my $x = shift;
    $stack = [ $x, $stack, 0 ];
}
sub pop {
    my $x = $stack->[0];
    $stack = $stack->[1];
    return $x;
}

# @@(feature scm2list
sub scm2list {
    my ($l) = @_;
    return $l != $NIL ? [$l->[0]] + scm2list($l->[1]) : [];
}
# )@@

# @@(feature list_str2scm (use str2scm)
sub list_str2scm {
    my ($l) = @_;
    return scalar(@$l)? [str2scm($l->[0]),list_str2scm(@$l[1 .. $#$l]),0] : $NIL;
    }
# )@@

# @@(feature scm2str
sub scm2str {
    my ($s) = @_;
    sub chars2str {
      my ($c) = @_;
      return $c != $NIL ? (chr($c->[0]) + chars2str($c->[1])) : "";
    }
    return chars2str($s->[0]);
}
# )@@

# @@(feature str2scm
sub str2scm {
    my ($s) = @_;
    sub chars2scm {
      my ($c) = @_;
      return scalar(@$c) ? [ord($c->[0]),str2scm(@$c[1 .. $#$c]),0] : $NIL;
    }
    return [chars2scm($s),scalar(@$s),3];
}
# )@@

sub prim1 {
  my ($sub_ref) = @_;
  return sub { 
    &push($sub_ref->( &pop())); 
    return;
    };
}
sub prim2 {
  my ($sub_ref) = @_;
  return sub { 
    &push($sub_ref->( &pop(), &pop())); 
    return;
    };
}
sub prim3 {
  my ($sub_ref) = @_;
  return sub { 
    &push($sub_ref->( &pop(), &pop(), &pop())); 
    return;
    };
}

########################################################
## Diverses opérations utilitaires.
########################################################

sub bool2scm {
  my ($x) = @_;
  return $x ? $TRUE : $FALSE;
}

sub is_rib {
  my ($x) = @_;
  return ref($x) eq 'ARRAY';
}

sub field0set {
  my ( $y, $x ) = @_;
  $x->[0] = $y;
  return $y;
}
sub field1set {
  my ( $y, $x ) = @_;
  $x->[1] = $y;
  return $y;
}
sub field2set {
  my ( $y, $x ) = @_;
  $x->[2] = $y;
  return $y;
}

sub vars_cmp {
  my ( $var1, $var2 ) = @_;
  if ($var1 == $var2) {
    return 1;
  }
  elsif (is_rib($var1) and is_rib($var2)) {
    if (@$var1 != @$var2) {
      return 0;
    }
    my $isSame = 1;
    for (my $i = 0; $i < @$var1 && $isSame; $i++) {
      $isSame = vars_cmp($var1->[$i], $var1->[$i]);
    }
    return $isSame;
  }
  return 0;
}

sub list_tail {
    my ( $lst, $k ) = @_;
    
    while ($k > 0) {
      $lst = $lst->[1];
      $k--;
    }
    return $lst;
}

########################################################
## Implémentation des primitives.
########################################################

our $primitives = [
  # @@(primitives (gen body)
  prim3(sub { my ($z, $y, $x) = @_; return [$x, $y, $z]; }),        # @@(primitive (%%rib a b c))@@
  prim1(sub { my ($x) = @_; return $x;}),                           # @@(primitive (%%id x))@@
  sub { return [&pop(), 0]->[1] },                                  # @@(primitive (%%arg1 x y))@@
  sub { &push([&pop(), &pop()]->[0]); return; },                    # @@(primitive (%%arg2 x y))@@
  sub { &push([&pop()->[0], $stack, 1]);return; },                  # @@(primitive (%%close rib))@@
  prim1(sub { my ($x) = @_; return bool2scm(is_rib($x));}),         # @@(primitive (%%rib? rib))@@
  prim1(sub { my ($x) = @_; return $x->[0]}),                       # @@(primitive (%%field0 rib))@@
  prim1(sub { my ($x) = @_; return $x->[1]}),                       # @@(primitive (%%field1 rib))@@
  prim1(sub { my ($x) = @_; return $x->[2]}),                       # @@(primitive (%%field2 rib))@@
  prim2(\&field0set),                                               # @@(primitive (%%field0-set! rib x))@@
  prim2(\&field1set),                                               # @@(primitive (%%field1-set! rib x))@@
  prim2(\&field2set),                                               # @@(primitive (%%field2-set! rib x))@@
  prim2(sub { my ($y, $x) = @_; return bool2scm($x == $y)}),        # @@(primitive (%%eqv? x y))@@
  prim2(sub { my ($y, $x) = @_; return bool2scm($x < $y)}),         # @@(primitive (%%< a b))@@
  prim2(sub { my ($y, $x) = @_; return $x + $y}),                   # @@(primitive (%%+ a b))@@
  prim2(sub { my ($y, $x) = @_; return $x - $y}),                   # @@(primitive (%%- a b))@@
  prim2(sub { my ($y, $x) = @_; return $x * $y}),                   # @@(primitive (%%* a b))@@
  prim2(sub { my ($y, $x) = @_; return int($x/$y)}),                # @@(primitive (%%quotient a b))@@               
  \&getchar,                                                        # @@(primitive (%%getchar))@@
  prim1(\&putchar),                                                 # @@(primitive (%%putchar c))@@
  prim1(\&CORE::exit),                                                    # @@(primitive (%%exit a))@@
  # )@@
];

########################################################
## Génération de la table de symboles.
########################################################

our $symtbl = $NIL;
my $n = get_int(0);
for (my $i = 0; $i < $n; $i++) {
    $symtbl = [ [ $FALSE, [ $NIL, 0, 3 ], 2 ], $symtbl, 0 ];
}

my $symbol_name = $NIL; 
my $symbol_name_len = 0; 
while (1) {
  my $c = get_byte();
    if ( $c == ord(',') ) {
        my $symbol_to_append = [$FALSE, [$symbol_name, $symbol_name_len, 3], 2];
        $symtbl=[$symbol_to_append, $symtbl, 0];
        $symbol_name = $NIL;
        $symbol_name_len = 0;
    }
    else {
        if ( $c == ord(';') ) {
            my $symbol_to_append = [$FALSE, [$symbol_name, $n, 3], 2];
            $symtbl=[$symbol_to_append, $symtbl, 0];
            last;
        }
        $symbol_name=[$c,$symbol_name,0];
        $symbol_name_len++;
    }
}

sub symbol_ref {
  my ($n) = @_;
  return list_tail( $symtbl, $n )->[0];
};

########################################################
## Décodage du bytecode de la RVM.
########################################################

while (1) {
    my $x = get_code();
    $n = $x;
    my $d  = 0;
    my $op = 0;
    while (1) {
        $d = [20, 30, 0, 10, 11, 4]->[$op];
        if ( $n <= 2 + $d ) {
            last;
        }
        $n  -= $d + 3;
        $op += 1;
    }
    if ( $x > 90 ) {
        $n = &pop();
    }
    else {
        if ( $op == 0 ) {
            $stack = [ 0, $stack, 0 ];
            $op += 1;
        }

        if ( $n == $d ) {
            $n = get_int(0);
        }
        elsif ( $n >= $d ) {
            $n = symbol_ref( get_int( $n - $d - 1 ) );
        }
        elsif ( $op < 3 ) {
            $n = symbol_ref($n);
        }
        
        if ( 4 < $op ) {
            $n = [ [ $n, 0, &pop() ],$NIL, 1 ];
            if (!$stack) {
                last;
            }
            $op = 4;
        }
    }
    $stack->[0] = [ $op - 1, $n, $stack->[0] ];
}

########################################################
## Exécution de la RVM.
########################################################

my $pc = $n->[0]->[2];

my $get_opnd = sub {
  my ($o) = @_;
  return is_rib($o) ? $o : list_tail( $stack, $o );
};

sub get_cont {
  my $s = $stack;
  while ( !$s->[2] ) {
    $s = $s->[1];
  }
  return $s;
}

sub set_global {
  my ($val) = @_;
  $symtbl->[0]->[0] = $val;
  $symtbl = $symtbl->[1];
}

set_global( [ 0, $symtbl, 1 ] );
set_global($FALSE);
set_global($TRUE);
set_global($NIL);

$stack = [ 0, 0, [ 5, 0, 0 ] ];

while (1) {

  if ($pc == 0) {
    last;
  }
  my $o = $pc->[1];
  my $i = $pc->[0];

  if ( $i < 1 ) {
    $o = $get_opnd->($o)->[0];
    while (1) {    
      my $c = $o->[0];
      if ( is_rib($c) ) {
        my $nargs   = &pop();           # @@(feature arity-check)@@
        my $c2      = [ 0, $o, 0 ];
        my $s2      = $c2;
        my $nparams = $c->[0] >> 1;
        # @@(feature arity-check 
          if ($c->[0]&1 ? $nparams > $nargs : $nparams != $nargs) {
            print("*** Unexpected number of arguments nargs:", $nargs, "nparams", $nparams, "variadics:", $c->[0]&1);
            exit(1)
          }
        # )@@
        # @@(feature rest-param (use arity-check)
          $nargs -= $nparams;
          if ($c->[0]&1) {
            my $rest = $NIL;
            while ($nargs) {
              $rest= [&pop(), $rest, 0];
              $nargs-=1;
            }
            $s2=[$rest,$s2,0];
          } 
        # )@@
        while ($nparams > 0) {
          $s2=[&pop(), $s2, 0];
          $nparams--;
        }
        if ($pc->[2]) { 
          $c2->[0]=$stack;
          $c2->[2]=$pc->[2];
        }
        else {
          my $k=get_cont();
          $c2->[0]=$k->[0];
          $c2->[2]=$k->[2];
        }
        $stack = $s2;
      }
      else {
        &pop(); # @@(feature (and arity-check (not prim-no-arity)))@@
        $o = $primitives->[$c]->();
        if ( is_rib($o) ) {
          next;
        }
        if ( $pc->[2] ) {
          $c = $pc;
        }
        else {
          $c = get_cont();
          $stack->[1] = $c->[0];
        }
      }
      $pc = $c;
      last;
    }
  }
  elsif ( $i < 2 ) {
    if ($stack == 0) {
      last;
    }
    $get_opnd->($o)->[0] = $stack->[0];
    $stack = $stack->[1];
  }
  elsif ( $i < 3 ) {
    &push( $get_opnd->($o)->[0] );
  }
  elsif ( $i < 4 ) {
    &push($o);
  }
  elsif ( $i < 5 ) {
    if ( &pop() != $FALSE ) {
      $pc = $pc->[1];
      next;
    }
  }
  else {
    last;
  }
  $pc = $pc->[2];
}
