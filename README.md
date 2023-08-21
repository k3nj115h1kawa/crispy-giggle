# RでPDFファイルの内容をテキストマイニングする
## 形態素解析を行ってデータ整理を行った後, ワードクラウドと共起ネットワークで図示する
* ワードクラウドの方は比較的上手くいったが, 共起ネットワークの方はあまりうまくいかなかった
* RMeCabパッケージを利用したかったが, 環境による影響(?)で上手くいかなかったので, 代わりにgibasaパッケージを用いた
* PDFファイルは作業ディレクトリ内に配置して実行する
* 例では, 内閣府の行政文書の電子的管理についての基本的な方針（内閣総理大臣決定）のPDFファイルを使用("https://www8.cao.go.jp/chosei/koubun/densi/densi.html")
