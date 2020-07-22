## # まえがき
![poppySGif](./poppySMovie.gif)
## Windows10
Windows10(pro/home)でも動作することを確認しました(Ubuntuでのインストール手順は[こちら](./READMEJP.gif))。  
ソフトを使えるようにするには、以下の手順が必要です。  
## Haskell Stack
[stack](https://docs.haskellstack.org/en/stable/README/)
中段のwindows 64bitInstallerをダウンロードしてインストールしてください。  
## gi-gtk
[using haskell gi in windows](https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows)
上記ページで指定されているやり方で進めます。  
### Msys2
[msys2](https://msys2.github.io/)
Linux系ライブラリをWindowsで動かす際に必要です。
### Haskell Platform
[Haskell Platform](https://www.haskell.org/platform/download/8.0.2/HaskellPlatform-8.0.2-a-minimal-x86_64-setup.exe)
バージョンの指定があるようなので注意。上のは良いとされるバージョンへの直リンクです。  
### システムの環境変数
以下の4つの環境変数を指定する必要があります。  
PATHは3つ、PKG_CONFIG_PATHは2つ、XDG_DATA_DIRSは1つ、HOMEは1つです。  
ユーザーの環境変数でなくシステムの環境変数側に指定します。  
環境変数の指定画面を出すには、windowsマークで右クリックして出てくる検索ボックスにenvと打ち込みます。  
```shell
PATH
C:\msys64\mingw64\bin
C:\msys64\usr\bin
%PATH%
```
```shell
PKG_CONFIG_PATH
C:\msys64\mingw64\lib\pkgconfig
C:\msys64\mingw64\share\pkgconfig
```
```shell
XDG_DATA_DIRS
C:\msys64\mingw64\share
```
```shell
HOME
C:\Users\yourUsersName\poppyS
```
yourUsersNameはご自身のホームフォルダ(コマンドプロンプトを立ち上げた際のデフォルトフォルダ)に対応する名前になります。  
このソフト(poppyS)はこの位置に置かれることを前提としています(下のgit cloneコマンドはこの位置でなされる)。  
### 各種Linux系ライブラリ
以下は、上の方でインストールしたMsys2のターミナルで入力します。  
#### Gtk系
```shell
pacman -S -q --noconfirm mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3
```
#### icu
```shell
pacman -S mingw-w64-x86_64-icu
```
## poppler
msys2のターミナルで続けてpopplerもインストールしておきます。  
```shell
pacman -S mingw-w64-x86_64-poppler mingw-w64-x86_64-poppler-data
```
## stanford CoreNLP Server with Docker
### docker
[Windows10 Pro](https://docs.docker.com/docker-for-windows/install/)
[Windows10 Home](https://docs.docker.com/docker-for-windows/install-windows-home/)
### stanford CoreNLP Parser
```shell
docker pull graham3333/corenlp-complete
```
[stanford CoreNLP Parser](https://stanfordnlp.github.io/CoreNLP/other-languages.html)のdockerの箇所。  
[Graham MacDonald氏のリンク](https://hub.docker.com/r/graham3333/corenlp-complete)をご参考です。  
## git
gitも入っていない人は、インストールします。  
[https://gitforwindows.org/](https://gitforwindows.org/)  
## poppyS
PDFリーダーはpoppySと命名しました。以下、poppySのインストールです。  
### IEによる指定アドレスへのアクセス
Stackをこれまで利用していない人は、一度、[https://www.haskell.org/](https://www.haskell.org)へInternet Explorerでアクセスする必要があります。  
これをやっておかないと、"certificate has unknown CA.."みたいなエラーがpoppySのstack build時に出てくるはずです。  
### git clone
以下は、Windowsのコマンドプロンプトで行います。スタートメニューのWindowsシステムツール内に存在します。  
```shell
git clone https://github.com/polymonyrks/poppyS.git
```
### 再起動
このあたりで一度再起動しておきます。  
### stack install
```shell
stack upgrade
stack update
chcp 65001
cd poppyS
stack install --extra-lib-dirs=c:\msys64\mingw64\lib --extra-include-dirs=c:\msys64\mingw64\include
```
chcp 65001がポイントです。Haskell系のbuildをWindows上で行う際には、これを打ち込んでおく必要が出てきます。  
途中でDLL絡みのエラーが出てくると思いますので、その際には以下の処置が必要になります。  
### zlib1.dllの差し替え
C:\msys64\mingw64\bin\zlib1.dllを以下のディレクトリにコピーしてください。zlib1.dllを上書きすることになります。  
C:\Users\yourUsersName\AppData\Local\Programs\stack\x86_64-windows\ghc8.6.5\mingw\bin  
yourUsersNameはご自身のホームフォルダ(コマンドプロンプトを立ち上げた際のデフォルトフォルダ)に対応する名前になります。  

これでインストール完了です。
## プログラムの実行
### 実行ファイルのショートカットの作成
poppySの実行ファイルは、"C:\Users\yourUsersName\AppData\Roaming\local\bin\poppyS-exe.exe"に生成されています。これのショートカットをデスクトップあたりに作っておきます。PDFファイルをこのショートカットアイコンにドラッグアンドドロップで起動できるようになっています(後述)。  
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
docker run -p 9000:9000 nlpbox/corenlp
```
初回だけダウンロードが始まります。ちょっと時間がかかりますが、次回以降はこのプロセスはないです。
### poppySの実行
ドラッグアンドドロップでやれます。先程作成したpoppyS-exe.exeのショートカットアイコンにPDFファイルをドラッグ・アンド・ドロップしてみてください。  
## How to read PDF
PDFリーダー（poppyS）を立ち上げてから少し待つと一部単語が黄色に色が変わります。  
待っても色が変わらないときにはstanford coreNLP server がタイムアウトになっているかもしれません。  
その場合は再度ドラッグ・アンド・ドロップしてみてください。  
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
|  dd  |  着色を全解除（後述）  |
|  p  |  一つ前の着色設定の復活  |
|  x  |  個別解除モードの起動と終了  |
|  gg  | 最初のページへの移動 |
|  w  | ２ページモード（最大化）と１ページモードの切り替え |
|  :w Enter  |  着色を保存（後述）  |  
## mouse
### 表
|  コマンド  |  効果  |
| ---- | ---- |
|  単語を左クリック  |  色をトグル（順方向）  |
|  単語を右クリック  |  色をトグル（逆方向）  |
|  空白域を左クリック  |  ページ送り（順方向・2ページ）  |
|  空白域を右クリック  |  ページ送り（逆方向・2ページ）  |

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
