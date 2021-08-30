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

## 全体像
言語として必要な機能
- 型(type, struct)
- const, let
- 制御構文(if, for, while)
- Error(Result形式)
- 演算子
- array

最終的に作りたい構文
   ```
   // 型
   bool
   number
   string
   symbol
   object // もしくはmap
   bigint
   promise
   array
   dom
   ajax
   binary
   result
   option
   void
   
   // array
   aaa = [1,2,3];
   
   // 宣言文
   const a1: number = 5;
   let b1 = 0;
   // 条件分岐
   if a1 > 10 {
           b1 = a1 + 10;
   } elif a1 > 5 {
           b1 = a1 + 5;
   } else {
           b1 = a1 + 1;
   }
   
   // for文
   for i in 0..a1 {
           
           if i == 5 {
               continue;
           }
   
           if i > 100 {
               break;
           }
           b1 += i;
   }
   
   let c1 = 0;
   for i in [1,2,3] {
               c1 += i;
   }
   
   // 型定義
   type AorB = "A" | "B";
   struct Profile {
       name: string,
       email: string,
       id: number
   };
   
   // 関数
   fn add(a: number, b: number) {
       return a + b;
   }
   
   add(1,2);
   
   // エラー処理
   fn add(a: number, b: number) {
       if a < b {
           return Result.Ok<a + b>;
       } else {
           return Result.Err;
       }
   }
   
   const d1 = add(1,2);
   let e1 = "";
   if typeof d1 == Result.Ok {
       e1 = "OK";
   } else {
       e2 = "Error";
   }
   
   // 演算子
   2 + 1
   2 - 1
   2 * 1
   2 / 1
   2 % 1 // 余り
   2 ** 2 // 冪乗
   a++ 
   a--
   // 比較演算子
   a == 2
   a != 2
   a > 2
   a < 2
   a >= 2
   a <= 2
   
   // bit論理演算子
   a & b
   a | b
   a ^ b // XOR
   ~ a // bit反転
   a << b // 左シフト
   a >> b // 右シフト
   
   // 論理演算子
   a %% b // AND
   a || b // OR
   !a // NOT
   
   // 三項演算子
   bbb = a > b ? 1 : 2;
   ```
