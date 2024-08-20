#!/bin/bash

# 指定されたディレクトリを検索し、全ての SMT.v ファイルを見つける
find converted -type f -name "SMT.v" | while read file; do
    # "Generated" と "Goal" を保持し、その他の G を Z に変換
    perl -pi.bak -e '
        s/\bGenerated\b/\x01/g;
        s/\bGoal\b/\x02/g;
        s/(?<!\w)G(?!\w)/Z/g;
        s/\x01/Generated/g;
        s/\x02/Goal/g;
    ' "$file"
done

find . -type f -name "SMT.v.bak" -delete