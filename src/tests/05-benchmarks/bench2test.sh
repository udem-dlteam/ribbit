for name in *.scm; do
  cat prefix.txt $name > $name.tmp
  cat $name.tmp postfix.txt > $name
  rm $name.tmp
done

