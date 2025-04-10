# Haskell Language Server (HLS) エラー解決手順

## 問題

VSCodeでHaskellプロジェクトを開いた際に、以下のエラーが発生していました：

```
ERROR HLS does not support GHC 9.6.7 yet.
```

これは、プロジェクトで使用しているGHC（Glasgow Haskell Compiler）のバージョン9.6.7が、Haskell Language Server（HLS）によってまだサポートされていないことを示しています。

## 原因

- プロジェクトはGHC 9.6.7を使用していた
- 現在のHLS（バージョン2.9.0.1）はGHC 9.6.7をサポートしていない
- HLSがサポートしているGHCバージョンは、`ghcup list`コマンドの出力で「hls-powered」タグが付いているもの

## 解決方法

1. **GHCのバージョンを確認**
```bash
ghc --version
ghcup list
```

2. **cabalファイルの依存関係を変更**
- `lgchain-hs.cabal`ファイルのbaseライブラリのバージョンを変更
```diff
- base          ^>=4.18.3.0
+ base          ^>=4.17.2.1
```

3. **プロジェクトで使用するGHCバージョンを指定**
- `cabal.project.local`ファイルにGHC 9.4.8を指定
```
with-compiler: /root/.ghcup/ghc/9.4.8/bin/ghc
```

4. **プロジェクトをクリーンアップして再ビルド**
```bash
cabal clean
cabal build
```

5. **VSCodeを再起動**
- VSCodeを再起動してHLSが正常に動作するか確認

## 結果

- プロジェクトがGHC 9.4.8でビルドされるようになった
- HLSが正常に動作するようになり、エラーが解消された

## 学んだこと

1. HLSは特定のGHCバージョンのみをサポートしている
2. `ghcup list`コマンドで「hls-powered」タグが付いているGHCバージョンがHLSでサポートされている
3. プロジェクトのGHCバージョンを変更するには：
   - cabalファイルの依存関係（特にbaseライブラリ）を適切なバージョンに変更する
   - `cabal.project.local`ファイルでGHCコンパイラのパスを明示的に指定する
