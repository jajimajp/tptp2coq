#!/bin/bash

find converted -type f -name "SMT.v" | while read file; do
    perl -pi.bak -e '
        s/\bGenerated\b/\x01/g;
        s/\bGoal\b/\x02/g;
        s/(?<!\w)G(?!\w)/Z/g;
        s/\x01/Generated/g;
        s/\x02/Goal/g;
    ' "$file"
done