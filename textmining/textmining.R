# pdftools, gibasa, wordcloud2, igraphパッケージを読み込む
library(pdftools)
library(gibasa)
library(wordcloud2)
library(igraph)

# PDFファイルのパスを指定
# PDFファイルが存在するフォルダに移動するか、絶対パスを指定
pdf_path <- "kihonntekihousin.pdf"

# PDFファイルからテキストを抽出
text <- pdf_text(pdf_path)

# テキストを空白で区切って連結
# paste0関数を使用
text <- paste0(text, collapse = " ")

# テキストを形態素解析
# リストとして返す
lst <- tokenize(text)

# リストをデータフレームに変換
# リストの要素に名前を付与
# make.names関数を使って列名として適切な名前に変換
# stringsAsFactors = FALSEを指定
names(lst) <- make.names(paste0("page", seq_along(lst)))
df <- data.frame(lst)

# 品詞が名詞である単語のみを抽出し、頻度を集計
df <- subset(df, grepl("名詞", page5))
df <- subset(df, page4!="(" & page4!=")" & page4!="." & page4!="," & page4!=";" & page4!="al" & page4!="et" & page4!=")." & page4!=")," & page4!=".," & page4!=":" & page4!="i" & page4!="ii" & page4!="①") # 不要な単語を消去

int.table <- table(unlist(strsplit(df$page4, " "))) %>% sort(decreasing = TRUE)
df.table <- data.frame(int.table)
df.table2 <- df.table[df.table$Freq > 20, ] # n回以上出現した単語のみ出力
df.table3 <- data.frame(df.table2)

# ワードクラウドを作成する
# 単語と頻度のベクトルを引数として取得
# 単語のサイズや色などのオプションを指定
wordcloud2(df.table2, size = 0.7, color = "random-dark", gridSize=8)

# Nグラム解析
df.ngram <- ngrams(df.table3, n=2)

# 共起ネットワークの作成
df.graph <- graph.data.frame(df.ngram)
# グラフの描画
plot(df.graph, vertex.label=V(df.graph)$name, vertex.size=15)
