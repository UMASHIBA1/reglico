# 計画書
## コンパイルステップ
1. 字句解析(Lexing)
2. 構文解析(Parsing)
3. 型検査,型推論
4. IRへの変換
5. rust or jsへの変換

## 作業手順
1. add関数を使ったadd計算で小さなコンパイラを作成してみる
    ```
        fn add(a: number, b: number) {
            return a + b;
        }

        const total = add(1,2);
    ```