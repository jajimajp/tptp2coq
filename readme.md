## converted/

生成された問題セット

## tptp2coq

TPTP の問題を Coq の形式に出力するツール

## inspect

TPTP の問題が coq-completion で受理できる形式か判断する

## conv.sh

coq-completion で受理できる形式の場合、TPTP 問題セットを変換する

```sh
./conv.sh SOURCE TARGET
```

TPTP の GPR ドメインに対して実行した結果が　converted/ にある。

## smtconv.sh

SMTCoq 用に、パラメータ G を Z に置換する

---

TPTP 問題セットは大きいので、gitignore している。
再現するための参考:
```sh
$ ls tptp
axioms	grp	mixed
yajima@~/Documents/0131deskarc/tptp2coq
$ ls tptp/grp | head
GRP001+6.p
GRP001-1.p
GRP001-2.p
GRP001-3.p
GRP001-4.p
GRP001-5.p
GRP001^5.p
GRP002-1.p
GRP002-10.p
GRP002-2.p
```


