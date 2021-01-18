# poppyS - Syntactical Highlighting for Natural Language in Haskell (WSL2 ver.)
# まえがき

自然言語(PDFファイル)にもシンタックスハイライト的な何か（着色）を試みるものです。
文法的に視認性が最大となる自然な単位を発見しました。
テクノロジの力（自然言語処理、構文解析）を活かして、
難しい文書、読む気すら起こらない文書を攻略します（速読・精読）。  


**簡単に動かせるWindows向けビルド済みバイナリファイル** を用意しています。  
[こちらのサイト(関数型玩具製作所、筆者のサイト)](https://polymony.net/2020/12/19/post-3765/)を参照ください。  

理論的背景、詳しい攻略法は[技術書を自作PDFリーダーで読む - Haskellでつくる不思議な読書体験 (1)](https://techbookfest.org/product/6282870020636672), もしくは[同Booth版](https://functoy.booth.pm/items/2449634)にまとめています。ご興味ありましたら参照ください。  

まだまだ実験的なもの(トイプログラム)ですが、おそらく今後主流になっていく技術だと確認しています。  
これまでの速読・精読の技術は、大概は読者が慣れている領域にのみ適用可能なたぐいのものですが、  
これは慣れていない領域にも適用可能です。  
更に難しい類のもの(ギリシア語、ラテン語起源の単語、難しい同じような単語が何度も出てくるもの)であればあるほど有効です。  

![poppySGif](./poppySMovieJP.gif)  

ぜひご自身でビルドして色々遊んでみてください。  


HaskellでGUIをやる例にもなっています。([gi-gtk](https://hackage.haskell.org/package/gi-gtk))。参考になるかもしれません。  
以下、実行環境を整える準備です。  

# インストール方法(WSL2によるもの)
## Windows10
Windows10(pro/home)でも動作することを確認しました([Ubuntuでのインストール手順はこちら](./READMEJP.md))。  
ソフトを使えるようにするには、以下の手順が必要です。  
WSL2を使いますのでUbuntuの方法と同じですが、DockerについてはWindows版を用意します(Ubuntu版でも動くかもですが、動作確認していません)。
## WSL2
[https://docs.microsoft.com/ja-jp/windows/wsl/install-win10](https://docs.microsoft.com/ja-jp/windows/wsl/install-win10)

## Update Packages list
Ubuntuのターミナルで、
```shell
sudo apt update
```
## Haskell Stack
Ubuntuのターミナルで、
[stack](https://docs.haskellstack.org/en/stable/README/)
```shell
wget -qO- https://get.haskellstack.org/ | sh
stack update
```
## gi-gtk
Ubuntuのターミナルで、
[https://github.com/haskell-gi/haskell-gi](gi-gtk)
```shell
sudo apt install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```
## poppler
Ubuntuのターミナルで、
```shell
sudo apt install libpoppler-dev libpoppler-glib-dev
```
## stanford CoreNLP Server
### docker
https://docs.docker.com/docker-for-windows/install/(https://docs.docker.com/docker-for-windows/install/)

### stanford CoreNLP Parser
Windowsのターミナル(コマンドプロンプト or PowerShell)で、
```shell
sudo docker pull graham3333/corenlp-complete
```
[stanford CoreNLP Parser](https://stanfordnlp.github.io/CoreNLP/other-languages.html)のdockerの箇所。  
[Graham MacDonald氏のリンク](https://hub.docker.com/r/graham3333/corenlp-complete)をご参考です。  

### Mecabのインストール
Ubuntuのターミナルで、
```shell
sudo apt install mecab
sudo apt install libmecab-dev
sudo apt install mecab-ipadic-utf8
```
## poppyS
PDFリーダーはpoppySと命名しました。  
### git clone
Ubuntuのターミナルで、
```shell
git clone https://github.com/polymonyrks/poppyS.git
```
### stack build
Ubuntuのターミナルで、
```shell
cd poppyS
stack install
```

### IEによる指定アドレスへのアクセス
"certificate has unknown CA.."みたいなエラーが出た場合は、
一度、[https://www.haskell.org/](https://www.haskell.org)へInternet Explorerでアクセスすると解消できるかもです。  

## GUIの準備
WSL2のUbuntu単体ではGUIがないので、準備します。
[このあたり](https://qiita.com/momomo_rimoto/items/51d533ae9529872696ce)を参考にしますが、リンク切れの際に備えて簡単な手順を記します。

### GUIの準備
#### xorg
Ubuntuのターミナルで、
```shell
sudo apt install libgl1-mesa-dev xorg-dev
```
[vcxsrv](https://sourceforge.net/projects/vcxsrv/)
を導入します。

起動の際には、以下の二点に注意してください。

 * 複数Windowsにする。
 * Additional parameters for VcXsrvに「-ac」を入れる
 
#### .bashrc
以下を最終行に追記します。
```shell
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0.0
```

これですべてインストール完了です。
## プログラムの実行
Ubuntuのターミナル上で、Windows側にあるPDFファイルを実行します。
```shell
poppyS-exe WindowsのPDFのPath
```
WindowsのPDFのPathは、例えば、C:\Users\polymony\hogehoge.pdfであれば、
```shell
poppyS-exe /mnt/c/Users/polymony/hogehoge.pdf
```
とやれば、起動できます。  


これ以降は、Ubuntuと同じなので、そちらを参照ください。
([Ubuntuでの解説](./READMEJP.md))。  
dockerをWindowsのTerminal(コマンドプロンプト、PowerShell)で起動する点注意です。
