#!/bin/bash

# 変換対象のフォルダパスを第１引数から取得
SOURCE_DIR=$1
# 変換後のファイルを保存するフォルダパスを第２引数から取得
TARGET_DIR=$2

# 変換処理を行う関数
convert_files() {
    local SRC_DIR=$1
    local TGT_DIR=$2
    for FILE in "$SRC_DIR"/*.p; do
        if [ -f "$FILE" ]; then
            RESULT=$(inspect-tptp "$FILE")
            if [[ $RESULT == YES* ]]; then
                # ファイル名のベース部分を取得
                BASE_NAME=$(basename "$FILE" .p)
                # 変換ディレクトリを作成
                CONVERT_DIR="$TGT_DIR/$BASE_NAME"
                mkdir -p "$CONVERT_DIR"

                # 各変換を行い、出力をファイルに保存
                tptp2coqp "$FILE" h > "$CONVERT_DIR/Hammer.v"
                tptp2coqp "$FILE" l > "$CONVERT_DIR/LPO.v"
                tptp2coqp "$FILE" s > "$CONVERT_DIR/SMT.v"
            fi
        fi
    done
}

# 指定されたディレクトリの処理を開始
convert_files "$SOURCE_DIR" "$TARGET_DIR"