#!/usr/bin/env bash

# Run in the src directory

getoptions() {
    sed -n 's/^;;;options://p' "$1"
}

getexpected() {
    ed -s "$1" <<'EOF' 2>/dev/null
/^;;;expected:/+1,$ s/^;;;//
/^;;;expected:/+1,$ p
EOF
}

getinput() {
    sed -n 's/^;;;input://p' "$1"
}

expected_file=$(mktemp)
actual_file=$(mktemp)

declare -i pass=0 fail=0 total=0

for testfile in tests/*.scm; do
    ((total++))
    testname=$(basename "$testfile" .scm)
    options=( $(getoptions "$testfile") )
    getexpected "$testfile" | sed '/^?$/d' > "$expected_file"
    if ./rsc -t ml -o test.ml "${options[@]}" "$testfile" && \
            ocamlc test.ml && \
            { getinput "$testfile" | ./a.out > "$actual_file"; } && \
            cmp "$expected_file" "$actual_file" >/dev/null; then
        printf "%s: PASS\n" "$testname"
        ((pass++))
    else
        printf "%s: FAIL\n" "$testname"
        diff "$expected_file" "$actual_file"
        ((fail++))
    fi
done

printf "%d test total, %d passed, %d failed.\n" "$total" "$pass" "$fail"
rm -f a.out test.ml test.cm[io] "$expected_file" "$actual_file"
