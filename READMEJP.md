## # まえがき
![poppySGif](./poppySMovie.gif)
普段遣いできるレベルにはなりましたのでPDFリーダー（二号機）のソースを公開します(試行錯誤の痕跡が残ったソースになっています。いずれ清書します。)。  
[https://github.com/polymonyrks/poppyS](https://github.com/polymonyrks/poppyS)
ご自身でビルドして色々遊んでみてください。(★カメラの説明書に適用した場合の効果が絶大でした。下の方に用意しています。)   
HaskellでGUI(([gi-gtk](https://hackage.haskell.org/package/gi-gtk))をやりたい人も参考になると思います。  
以下、実行環境を整える準備です。  
 
## Ubuntu20.04
OSは[Ubuntu 20.04 LTS (Focal Fossa)](https://releases.ubuntu.com/20.04/)です。  
まだ環境を持ってない人は、  
仮想環境（[VirtualBox](https://www.virtualbox.org/)等）とかご自身が今はつかっていない古いPCに入れたりするのがおすすめです。  
その際は軽量なUbuntuである[lubuntu](https://lubuntu.me/downloads/)を使うといいと思います（下の手順で導入できること確認済みです。）。
他のLinuxディストリビューションやMac、Windowsは動作確認していませんが、下と同様のことをやれば動くと思います([【追記】Windows10 Homeにて動作確認済み。安定して使えるようになったらアナウンス予定)。  
ターミナルで以下のコマンドを打ち込んでいけばいいです。  
(Ubuntuでのターミナル起動はCtrlとAltを押しながらtです)。  
## Update Packages list
```shell
sudo apt update
```
## Haskell Stack
[stack](https://docs.haskellstack.org/en/stable/README/)
```shell
wget -qO- https://get.haskellstack.org/ | sh
```
## gi-gtk
[https://github.com/haskell-gi/haskell-gi](gi-gtk)
```shell
sudo apt install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```
## poppler
```shell
sudo apt install libpoppler-dev libpoppler-glib-dev
```
## stanford CoreNLP Server
### docker
```shell
sudo apt install apt-transport-https ca-certificates curl gnupg-agent software-properties-common
```
```shell
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key  add -
```
```shell
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"
```
```shell
sudo apt install docker-ce
```
[この記事](https://qiita.com/yoshiyasu1111/items/7a3159446888fe556431)を参考にしました。
### stanford CoreNLP Parser
```shell
sudo docker pull graham3333/corenlp-complete
```
[stanford CoreNLP Parser](https://stanfordnlp.github.io/CoreNLP/other-languages.html)のdockerの箇所。  
[Graham MacDonald氏のリンク](https://hub.docker.com/r/graham3333/corenlp-complete)をご参考です。  
## poppyS
PDFリーダーはpoppySと命名しました。  
### git clone
```shell
git clone https://github.com/polymonyrks/poppyS.git
```
### stack build
```shell
cd poppyS
stack build
```
これでインストール完了です。
## プログラムの実行
### PDFファイルの準備
今は英文(English)のみ対応。【追記】日本語文書についても開発開始しました。開発途上の部分も公開しています。それなりのものができたら、公式にアナウンスします（parser部分のチューニングが全然できていません）。）
Stanford CoreNLP Serverを立ち上げた状態で、本PDFリーダー(poppyS)を立ち上げる必要があります。  
下は一例です。この辺りは動作確認できています。  
普通に読んだらなかなか難しいたぐいの文書(法律関係はその筋でないと読む気すら起こらないですね、、)ですが、工夫すると読みやすくなります。  
この辺りが革新的だと確信しています。  
(PDF例) 
1. 情報科学分野
* [Haskell Wikibooks](https://en.wikibooks.org/wiki/Haskell)
* [Basic Category Theory](https://arxiv.org/abs/1612.09375)
* [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf)
* [SICP](https://web.mit.edu/alexmv/6.037/sicp.pdf)
* [Practical Foundations for Programing Languages](http://profs.sci.univr.it/~merro/files/harper.pdf)
* [Homotopy Type Theory](http://saunders.phil.cmu.edu/book/hott-ebook.pdf)
2. 法律関連
* [Copyright Law of the United States](https://www.copyright.gov/title17/title17.pdf)
* [License Agreement Templates](https://otl.stanford.edu/sites/g/files/sbiybj10286/f/exclusive_03-06-2018.pdf)
* [Annual Report (2019) (Apple Inc.)](https://s2.q4cdn.com/470004039/files/doc_financials/2019/ar/_10-K-2019-(As-Filed).pdf)
* [host city contract - Tokyo 2020 Olympic Games](https://gtimg.tokyo2020.org/image/upload/production/jxnoeerdp7hxvgtgxp73.pdf)
3. 医療関連
* [Basics of Molecular Biology](https://homes.cs.washington.edu/~tompa/papers/molbio.pdf)
* [Clinical characteristics of 2019 novel coronavirus infection in China](https://www.medrxiv.org/content/10.1101/2020.02.06.20020974v1)
4. 装置の説明書 <- new
* [カメラの説明書](https://1vision.co.il/pdfs/vieworks/manual/User_Manual_VA_GigE_EN.pdf)

### Stanford CoreNLP Serverの起動
```
sudo docker run -p 9000:9000 nlpbox/corenlp
```
初回だけダウンロードが始まります。ちょっと時間がかかりますが、次回以降はこのプロセスはないです。

### poppySをファイルのパスを引数にして起動
以下のコマンドでPDFリーダー（poppyS）が立ち上がります。
(e.g.1) $HOME/poppySから起動する場合
```shell
stack exec poppyS-exe "pdfs/SICP.pdf"
```
(e.g.2) フルパス指定でも起動できます。
```shell
stack exec poppyS-exe "/home/username/poppyS/pdfs/SICP.pdf"
```
お試しで色々なPDF試したい場合は、stack installしてできたpoppy-exeを($HOME/.local/binにできます).pdfファイルを開くアプリに指定すれば、右クリックのプルダウン選択で実行できます。デフォルトアプリに指定するのはおすすめしません。  
## How to read PDF
PDFリーダー（poppyS）を立ち上げてから少し待つと一部単語が黄色に色が変わります。  
待っても色が変わらないときにはstanford coreNLP server がタイムアウトになっているかもしれません。  
その場合はstack exec poppyS-exeから再度実行してみてください。Ctrl c , Ctrl c (コントロールCを二回)でプログラムの実行を止めれば、  
再度stack exec コマンドが打てるようになります。
以下、操作方法です。
## Keyboard
### 表
|  コマンド  |  効果  |
| ---- | ---- |
|  j  |  ページ送り（順方向・2ページ）  |
|  k  |  ページ送り（逆方向・2ページ）  |
|  Right  |  ページ送り（順方向・1ページ）  |
|  Left  |  ページ送り（逆方向・1ページ）  |
|  Down  |  画像サイズ調整（余白切り取り）  |
|  cc  |  着色を全解除（後述）  |
|  p  |  一つ前の着色設定の復活  |
|  x  |  個別解除モードの起動と終了  |
|  gg  | 最初のページへの移動 |
|  :w Enter  |  着色を保存（後述）  |  

### コメント
Right, Left, Downは矢印キーのことです。Enterはエンターキーです。  
Vimっぽい操作に近づけようとしています。今はこんな感じです。  
## 着色
マウス操作は着色に使います。文章中の適当な箇所をクリックしてみてください。色が変わります。何度か連打すると他の色に変わります。  
色の変わる順番は、赤、青、緑、紫、オレンジ、ピンク、水色、シアン、赤、青、、といった具合です。
ddですべての着色が消えます。
黄色だけ残りますが、これはそのページでの重要語です。重要語の判断基準は、それを指定した際にページ中での着色される面積がどれくらいかが指標になっています（いわゆる機械学習チックな特殊なことはやっていませんが、色々試したらこれが正解でした。もちろん良く有りげな単語は除去しています。日本語版はこの辺りのチューニングもまだまだです）。  
ddを押したあとにpを押すと直前の着色セッティングが復活します。注意としてはddを押したあとに一つでも新しく着色単語指定すると設定が上書きされ、もとに戻せなくなります。コピペする際のクリップボックスと同じと考えてください。
さらに、ddを押したあとも設定は残ります。つまり、次回に同じ単語を着色しようとした場合は、直前の色から始められます。
このddとp、そして直前の色からのスタートを使いこなすのが、けっこう重要です。
一部単語のみ着色解除したい場合は、まずxを押してください。これで削除モードになります。
その状態で対象となる単語をクリックしてください。削除モードを終了させるにはもう一度xを押してください。  
## 戦略とトレーニング
着色は戦略的に行わないと逆に読みにくくなります。ある程度ノウハウを持っており、このHPでも度々ご紹介していますが、  
折を見てここでまとめるつもりです。  
自然言語を着色して読んでいくやり方はまだまだ発展途上です。しかしゆくゆくは電子文書を読むやり方としてスタンダードになっていくとみています。専門性が高かったり、形式的すぎてこれまで読もうとしない、読めたとしても疲れるたぐいの文書を読む技術としてこれまでに無いアプローチです。  
## License
(以下と同様の内容はソースのLICENSEというファイルに英語で書いてあります。)  
BSD3をベースに、個人利用と教育目的の利用は問題なしです。教育目的以外で商用利用したい場合は連絡ください。  
他ページで特許周りに言及してます(まだ出願だけです)が、個人利用と教育目的であればBSD3に則って運用すれば権利主張することはないです。  
プログラミング教育の題材として使うと面白いことになると思っているからです。  
(余談)  
Haskellは難しいことをやらなければ初心者向け言語であり、  
学べば学ぶほど面白くなってくる言語だと、このソフトウェアの開発を通じて今も実感しています。  
