# poppyS - Syntactical Highlighting for Natural Language in Haskell

# まえがき
自然言語(PDFファイル)にもシンタックスハイライト的な何か（着色）を試みるものです。
文法的に視認性が良くなる自然な単位を発見しました。
テクノロジの力（自然言語処理、構文解析）を活かして、
難しい文書、読む気すら起こらない文書を攻略します（速読・精読）。  

お手軽なWindows向けビルド済みバイナリファイルを用意しています。  
[こちらのサイト(関数型玩具製作所、筆者のサイト)](https://polymony.net/2020/12/19/post-3765/)を参照ください。  


![setsumei](./setsumei.png)  


* (1) 初期状態（白黒）  
* (2) 「自然」を含む「自然な単位」をピンクでハイライト  
* (3) 「自然な単位」を赤青で交互に示したもの  
* (4) 「自然でない単位」を赤青で交互に示したもの  
* (5) 「自然」をそのまま「自然でない単位」でピンクでハイライト  


理論的(?)背景、詳しい攻略法は[技術書を自作PDFリーダーで読む - Haskellでつくる不思議な読書体験 (1)](https://techbookfest.org/product/6282870020636672),もしくは[同Booth版](https://functoy.booth.pm/items/2449634)にまとめています。ご興味ありましたら参照ください。  


以下のGif動画は日本語ですが、英語も対応しています（もともと[英語から開発は始まっています](./README.md)）。  
まだまだ実験的なもの(トイプログラム)ですが、おそらく今後主流になっていく技術だとみています。  
これまでの速読・精読の技術は、大概は読者が慣れている領域にのみ適用可能なたぐいのものですが、  
これは慣れていない領域にも適用可能です。  
更に難しい類のもの(漢字が使われるもの（日本語文）、ギリシア語、ラテン語起源の単語(英語文)、難しい同じような単語が何度も出てくるもの)であればあるほど有効です。  


![poppySGif](./poppySMovieJP.gif)  



[英語のGIF動画](./README.md)もあります。  



ぜひご自身でビルドして色々遊んでみてください。  
HaskellでGUIをやる例にもなっています。([gi-gtk](https://hackage.haskell.org/package/gi-gtk))。参考になるかもしれません。  
以下、実行環境を整える準備です。  
# インストール方法
## OS
推奨OSは[Ubuntu 20.04 LTS (Focal Fossa)](https://releases.ubuntu.com/20.04/)もしくは、[Ubuntu 18.04 LTS (Bionic Beaver) Desktop](https://releases.ubuntu.com/18.04/)です。  
[lubuntu 20.04 Desktop](https://lubuntu.me/downloads/)でも動作確認できました。
Windows10も動きます。([別ページ用意しました](./READMEWINJPWSL2.md))。  
ただしWindowsは環境構築が難しいところがあるので、ビルド済みバイナリファイルも用意しています。  
[こちらのサイト(関数型玩具製作所、筆者のサイト)](https://polymony.net/2020/12/19/post-3765/)を参照ください。


MacOSも動くこと確認済みです([別ページ用意しました](./READMEMACOSJP.md))。ただし、慣れていない人はParallels、VMWare、VirtualBox等の仮想環境でUbuntuを動かすことをおすすめします。  

Ubuntuでのインストールに戻ります。  
インストールはターミナルで以下のコマンドを打ち込んでいけばいいです。  
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
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu focal stable"
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

### 日本語モードの搭載
少しチューニングがまだ残っていますが、日本語文書にも対応しました。Mecabのインストールが必要です。  
英語・日本語はソフト起動時に自動判定していますが、構文解析結果がおかしい感じのときは判定ミスしているはずなので、  
space l tで切り替えてください(lはlanguage, tはtoggleという気持ちで設定しています)。  

#### Mecabのインストール
##### Mecab本体
```shell
sudo apt install mecab
sudo apt install libmecab-dev
sudo apt install mecab-ipadic-utf8
```

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

# プログラムの実行
## PDFファイルの準備
Stanford CoreNLP Serverを立ち上げた状態で、本PDFリーダー(poppyS)を立ち上げる必要があります。  
下は一例です。この辺りは動作確認できています。  
普通に読んだらなかなか難しいたぐいの文書(法律関係はその筋でないと読む気すら起こらないですね、、)ですが、工夫すると読みやすくなります。  
この辺りが革新的だと確信しています。  
(PDF例) 
1. 情報科学分野
* [Rustの日本語ドキュメント](https://doc.rust-jp.rs/)
* [Docker Engine ユーザガイド基礎編](https://docs.docker.jp/pdf-download.html)
* [Haskell Wikibooks](https://en.wikibooks.org/wiki/Haskell)
* [Basic Category Theory](https://arxiv.org/abs/1612.09375)
* [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf)
* [SICP](https://web.mit.edu/alexmv/6.037/sicp.pdf)
* [Practical Foundations for Programing Languages](http://profs.sci.univr.it/~merro/files/harper.pdf)
* [Homotopy Type Theory](http://saunders.phil.cmu.edu/book/hott-ebook.pdf)
* [Introduction to Categorical Quantum Mechanics (Chris Heunen and Jamie Vicary)](http://www.cs.ox.ac.uk/people/jamie.vicary/IntroductionToCategoricalQuantumMechanics.pdf)]


2. 法律関連

* [所得税及び復興特別所得税の確定申告書 申請書A　手引き](https://www.nta.go.jp/taxes/shiraberu/shinkoku/tokushu/yoshiki.htm)
* [技術書オンライン販売サービス利用規約](https://techbookfest.org/market-terms-of-use.html)

* [Epic Games, Inc. v. Apple Inc. (4:20-cv-05640) (Compliant)](https://www.courtlistener.com/docket/17442392/epic-games-inc-v-apple-inc/)

* [US6520699](http://patft.uspto.gov/netacgi/nph-Parser?Sect1=PTO1&Sect2=HITOFF&d=PALL&p=1&u=%2Fnetahtml%2FPTO%2Fsrchnum.htm&r=1&f=G&l=50&s1=6520699.PN.&OS=PN/6520699&RS=PN/6520699)

* [第８１期第１四半期　四半期報告書 (2020) (任天堂)](https://www.nintendo.co.jp/ir/library/securities/index.html)

* [Copyright Law of the United States](https://www.copyright.gov/title17/title17.pdf)
* [License Agreement Templates](https://otl.stanford.edu/sites/g/files/sbiybj10286/f/exclusive_03-06-2018.pdf)
* [Annual Report (2019) (Apple Inc.)](https://s2.q4cdn.com/470004039/files/doc_financials/2019/ar/_10-K-2019-(As-Filed).pdf)
* [host city contract - Tokyo 2020 Olympic Games](https://gtimg.tokyo2020.org/image/upload/production/jxnoeerdp7hxvgtgxp73.pdf)


3. 医療関連
* [Basics of Molecular Biology](https://homes.cs.washington.edu/~tompa/papers/molbio.pdf)
* [Clinical characteristics of 2019 novel coronavirus infection in China](https://www.medrxiv.org/content/10.1101/2020.02.06.20020974v1)
4. 装置の説明書
* [カメラの説明書](https://1vision.co.il/pdfs/vieworks/manual/User_Manual_VA_GigE_EN.pdf)

## Stanford CoreNLP Serverの起動
```
sudo docker run -p 9000:9000 nlpbox/corenlp
```
初回だけダウンロードが始まります。ちょっと時間がかかりますが、次回以降はこのプロセスはないです。

Dockerをいちいち立ち上げるのが面倒な人は、以下のコマンドで対応します。
--restart=alwaysというオプションを指定するだけです。
```
sudo docker run --restart=always -p 9000:9000 nlpbox/corenlp
```


poppySのプログラムの設定上、dockerはlocalhostを使っていることを想定しています。異なる環境の場合はエラーが出ると思いますので、fromPDF.hsの  
```haskell
command = "http://localhost:9000/?annotators=parse&outputFormat=json&timeout=50000"
```
上のlocalhostの箇所をご自身の環境に合うものに変更してください。  
(例)
```haskell
command = "http://192.168.99.100:9000/?annotators=parse&outputFormat=json&timeout=50000"
```


## poppySをファイルのパスを引数にして起動
以下のコマンドでPDFリーダー（poppyS）が立ち上がります。
(e.g.1) $HOME/poppySから起動する場合
```shell
stack exec poppyS-exe "pdfs/SICP.pdf"
```
(e.g.2) フルパス指定でも起動できます。
```shell
stack exec poppyS-exe "/home/username/poppyS/pdfs/SICP.pdf"
```
お試しで色々なPDF試したい場合は、stack installしてできたpoppy-exeを($HOME/.local/binにできます)pdfファイルを開くアプリに指定すれば、右クリックのプルダウン選択で実行できます。デフォルトアプリに指定するのはおすすめしませんが、一時的に様々なPDFに試して見る際にはおすすめです。Windowsの場合はアイコンを右クリックで簡単に指定できますが、Ubuntuだと少し手間です。  

poppySディレクトリに、poppyS.desktopというファイルがあります。以下のとおりに、ご自身のユーザー名で書き換えてみてその後、
```shell
[Desktop Entry]
Version=1.0
Name=poppyS
Comment=poppyS is a genuin PDF Reader for syntactically highlighting Natural Languages (Now Only Eng. and Jp.)
Keywords=pdf;
Exec=/home/yourUserName/.local/bin/poppyS-exe %u
Path=/home/yourUserName/.local/bin/
Icon=/home/yourUserName/poppyS/poppyS.png
Terminal=false
Type=Application
Categories=Viewer;Utility;Development;
MimeType=application/pdf;
```

上記の、yourUserNameの箇所を書き換えます。
/usr/share/applications/に、このpoppyS.desktopをコピーすれば、
Ubuntuでも右クリックでPDFを開くプログラムの一覧にpoppySを挙げることができます。


## How to read PDF
PDFリーダー（poppyS）を立ち上げてから少し待つと一部単語が黄色に色が変わります。  
待っても色が変わらないときにはstanford coreNLP server がタイムアウトになっているかもしれません。  
その場合はstack exec poppyS-exeから再度実行してみてください。Ctrl c , Ctrl c (コントロールCを二回)でプログラムの実行を止めれば、  
再度stack exec コマンドが打てるようになります。
以下、操作方法です。
### Keyboard
#### 表
|  コマンド  |  効果  |
| ---- | ---- |
|  j  |  ページ送り（順方向・2ページ）  |
|  k  |  ページ送り（逆方向・2ページ）  |
|  Right  |  ページ送り（順方向・1ページ）  |
|  Left  |  ページ送り（逆方向・1ページ）  |
|  Down  | 画像サイズ調整（余白切り取り、crop） |
|  v  |  Vanilla(白黒)モード切り替え  |
|  n  | モード切替（順方向） |
|  m  | モード切替（逆方向） |
|  dd  |  着色を全解除（後述）  |
|  p  |  一つ前の着色設定の復活  |
|  x  |  個別解除モードの起動と終了  |
|  gg  | 最初のページへの移動 |
|  w  | ２ページモード（最大化）と１ページモードの切り替え |
|  :w Enter  |  着色設定を保存(ドキュメント)（後述）  |  
|  :w@★  |  着色設定を#slot ★に保存（★はa .. z, 1 .. 9）  |  
|  @★  |  着色設定を#slot ★から呼出（★はa .. z, 0 .. 9）  |  
|  space l t  |  日本語モードと英語モードの切り替え |  
### mouse
#### 表
|  コマンド  |  効果  |
| ---- | ---- |
|  単語を左クリック  |  色をトグル（順方向）  |
|  単語を右クリック  |  色をトグル（逆方向）  |
|  空白域を左クリック  |  ページ送り（順方向・2ページ）  |
|  空白域を右クリック  |  ページ送り（逆方向・2ページ）  |
### コメント
Right, Left, Downは矢印キーのことです。Enterはエンターキーです。  
Vimっぽい操作に近づけようとしています。今はこんな感じです。  
### 着色
マウス操作は着色に使います。文章中の適当な箇所をクリックしてみてください。色が変わります。何度か連打すると他の色に変わります。  
色の変わる順番は、赤、青、緑、紫、オレンジ、ピンク、水色、シアン、赤、青、、といった具合です。
ddですべての着色が消えます。
黄色だけ残りますが、これはそのページでの重要語です。重要語の判断基準は、それを指定した際にページ中での着色される面積がどれくらいかが指標になっています（いわゆる機械学習チックな特殊なことはやっていませんが、色々試したらこれが正解でした。もちろん良く有りげな単語は除去しています。日本語版はこの辺りのチューニングもまだまだです）。  
ddを押したあとにpを押すと直前の着色セッティングが復活します。注意としてはddを押したあとに一つでも新しく着色単語指定すると設定が上書きされ、もとに戻せなくなります。コピペする際のクリップボックスと同じと考えてください。
さらに、ddを押したあとも設定は残ります。つまり、次回に同じ単語を着色しようとした場合は、直前の色から始められます。
ddによる登録解除以外にも、モードとして着色あり・なしを方向キー上で切り替えられます。
これら着色有無切り替えと、そして直前の色からのスタートを使いこなすのが、けっこう重要です。
一部単語のみ着色解除したい場合は、まずxを押してください。これで削除モードになります。
その状態で対象となる単語をクリックしてください。削除モードを終了させるにはもう一度xを押してください。  
### モード切替
nキー, mキー, もしくはvキーで読書モードの切り替えができます。デフォルトではHintモードです。これは、先述のとおりに重要語が黄色でマーキングされるモードです。  
モードはあと6つあって、１つ目はHintモード, ２つ目はLocalモード、３つ目がAdhocモード、4つ目がVanillaモード、5つ目はGramaticaモード、6つ目はPrimitiveモードです。  
Hintモードはそのページ内の語根についてヒストグラムをとった際に上位2つを黄色で着色します。それ以外は後述のAdhocモードと同様です。  
LocalモードはHintモードと同様ですが、着色のトグルがピンク、水色、シアン、オレンジ、紫、緑、青、赤になっていること、dd(着色設定を全削除)した際にピンク色から開始するのが違っています。その他のモードでは、dd後の開始はその時点でのトグル色が維持されます。Localモードは章や節が終わるたびにddを押し、ピンク、水色、シアン、といったものを章タイトルや節タイトルに対して順番に何も考えずハイライトするような着色戦略に使います。重要語が出現順にピンク、水色と読者が把握して着色したいのでこのような仕様になっています。LocalモードはWindowsのプレビルド版には未実装です。  
Adhocモードは単語をクリックするとその単語の語根に対して着色設定を施します。赤、青、緑、紫、オレンジ、ピンク、水色、シアン、赤、青、、というサイクリックな順番で設定がなされます。同じ単語を連打すると同様にトグルしますし、他の単語を選択しても同じ順でトグルされます。
Vanillaモードはすべての着色をなしにします。  
これはddとは異なり、一時的に見た目を白黒にするものです。  
Gramaticaモードは構文解析関数のデバッグ用に使います。着色させたい単位について、  
赤と青でサイクリックに確認できます。上記関数をチューニングしない場合は不要ですが、一度見てみると面白いかもしれません。  
PrimitiveモードはAdhocモードと基本的には同じですが、単語をそのまま（句構造で一様の着色という縛りなし）に着色します。単語を探しているような読書の際に使います。  
mキーもしくはnキーでLocal, Hint, Primitive, Gramatica, Adhocを切り替えます。
vキーでHintモードとVanillaモードの切り替えをします。  

# 戦略とトレーニング
着色は戦略的に行わないと逆に読みにくくなります。ある程度ノウハウを持っており、このHPでも度々ご紹介していますが、  
折を見てここでまとめるつもりです(電子書籍の形でまとめることにしました(後述))。  
自然言語を着色して読んでいくやり方はまだまだ発展途上です。しかしゆくゆくは電子文書を読むやり方としてスタンダードになっていくとみています。専門性が高かったり、形式的すぎてこれまで読もうとしない、読めたとしても疲れるたぐいの文書を読む技術としてこれまでに無いアプローチです。  

# 関連書籍
技術書を頒布しています。シリーズ化する予定です。よろしければ参照ください。
[「技術書を自作PDFリーダーで読む - Haskellでつくる不思議な読書体験（１）」 ](https://techbookfest.org/product/6282870020636672)


# License
(以下と同様の内容はソースのLICENSEというファイルに英語で書いてあります。)  
BSD3をベースに、個人利用と教育目的の利用は問題なしです。教育目的以外で商用利用したい場合は連絡ください。  
他ページで特許周りに言及してます(まだ出願だけです)が、個人利用と教育目的であればBSD3に則って運用すれば権利主張することはないです。  
プログラミング教育の題材として使うと面白いことになると思っているからです（上記シリーズ刊行もその目的あり）。  
(余談)  
Haskellは難しいことをやらなければ初心者向け言語であり、  
学べば学ぶほど面白くなってくる言語だと、このソフトウェアの開発を通じて今も実感しています。  
