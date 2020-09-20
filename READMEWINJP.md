# poppyS - Syntactical Highlighting for Natural Language in Haskell
# まえがき
PDFファイルを構文解析技術を応用して、いい感じで着色して読みやすくするソフトです。  
まだまだ実験的なもの(トイプログラム)ですが、おそらく今後主流になっていく技術だと確認しています。  
これまでの速読・精読の技術は、大概は読者が慣れている領域にのみ適用可能なたぐいのものですが、  
これは慣れていない領域にも適用可能です。  
更に難しい類のもの(ギリシア語、ラテン語起源の単語、難しい同じような単語が何度も出てくるもの)であればあるほど有効です。  
![poppySGif](./poppySMovieJP.gif)
ぜひご自身でビルドして色々遊んでみてください。(★カメラの説明書に適用した場合の効果が絶大でした。下の方に用意しています。)   
HaskellでGUIをやる例にもなっています。([gi-gtk](https://hackage.haskell.org/package/gi-gtk))。参考になるかもしれません。  
以下、実行環境を整える準備です。  

# 注意
20200917現在、gi-pangoのバグによりWindowsでは不思議な読書体験が出来ない状態です。stackというパッケージ管理ソフトが新しいgi-pangoに対応すれば[おそらく解消される](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=968729)と思われますが、そもそもHaskellはLinuxと親和的な言語なので、WindowsでもWSL2でLinux上で動かしたほうが、環境構築の観点では頭を悩ませなくていいと考えました。  
[WSL2でのインストール方法](./READMEWINJPWSL2.md))にWSL2上で環境構築する方法を用意しましたので、ご検討ください。  
Msys2による方法（本ページ）、[WSL2による方法](./READMEWINJPWSL2.md)のどちらの方法もメリットがあります。

 * WSL2: HaskellはLinuxと親和的なので環境構築の面でバグったりするリスクが小さい
 * Msys2: PDFリーダーの実行ファイル(.exe)を.pdfファイルと関連付けられるので、マウスクリックで楽に起動できる。

# インストール方法(Msys2によるもの)
## Windows10
Windows10(pro/home)でも動作することを確認しました([Ubuntuでのインストール手順はこちら](./READMEJP.md))。  
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
実行時にも上のzlib1.dll絡みのエラーが出た場合には、zlib1.dllをpoppyS-exe.exeのあるフォルダにコピーすればエラーが出なくなります。  
### PDFファイルの準備
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
poppySのプログラムの設定上、dockerはlocalhostを使っていることを想定しています。異なる環境の場合はエラーが出ると思いますので、fromPDF.hsの  
```haskell
command = "http://localhost:9000/?annotators=parse&outputFormat=json&timeout=50000"
```
上のlocalhostの箇所をご自身の環境に合うものに変更してください。  
(例)
```haskell
command = "http://192.168.99.100:9000/?annotators=parse&outputFormat=json&timeout=50000"
```
windows homeのdockerの場合、自分の環境だと"docker is configured to use the dafault machine with IP 192.168.99.100"と書かれていたので、  
上のように変更したらうまく行きました。
### poppySの実行
ドラッグアンドドロップでやれます。先程作成したpoppyS-exe.exeのショートカットアイコンにPDFファイルをドラッグ・アンド・ドロップしてみてください。  
## How to read PDF
以下は[Ubuntuのページ](./READMEJP.md)と内容が重複するので、そちらを参照ください。  
### 日本語モードの搭載(Windowsの場合)
少しチューニングがまだ残っていますが、日本語文書にも対応しました。Mecabのインストールが必要です。  
英語・日本語はソフト起動時に自動判定していますが、構文解析結果がおかしい感じのときは判定ミスしているはずなので、  
space l tで切り替えてください(lはlanguage, tはtoggleという気持ちで設定しています)。  
#### Mecabのインストール
[公式ページ](https://taku910.github.io/mecab/)の、Binary package for MS-Windowsをインストールしてください。  
インストール時にはSHIFT-JISでなくUTF8を選択してください。  
更に、Pathを通しておいてください。Mecabインストール時の設定に依りますがデフォルトでは、
```shell
C:\Program Files (x86)\Mecab\bin
```
みたいな場所になっているはずです。  
