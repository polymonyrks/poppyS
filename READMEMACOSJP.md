## # まえがき
![poppySGif](./poppySMovieJP.gif)
 
## MacOS
MacOSでも動作することを確認しました。ただし、[Ubuntu](./READMEJP.md)と比較して試しの要素が大きいです。  
Macportを利用するので、finkやHomebrewをパッケージマネージャとして既に利用している場合はやめておいたほうがいいですし、  
GUI表示するためにxquartzを入れる必要がある辺りも玄人向けだと思います。  
途中で色々なオプション（あるパッケージを無効化したり、有効に戻したり）などを指定する必要もあります。  
普段遣いにするための力量が残念ですが私にはありませんでした（とりあえず動く、というところまで持っていったところで力尽きました）。
ParalellsやVMWARE、VirtualBoxでUbuntuを入れて試したほうが楽ですが、人によってはあと少し手を加えればやれる、とかあるかと思いますので、  
情報として残しておきます。
ソフトを使えるようにするには、以下の手順が必要です。  
## Macports
[Macports](https://www.macports.org/)  

HomeBrewも試みましたが、popplerで文字をRenderする際にバグって表示できなかったのでやめました。  
近年はHomeBrewが流行りのようですが（Macユーザーでなかった自分も知っているくらい）、  
Macportのほうが老舗で安定しているみたいです。  
## Haskell Stack
[stack](https://docs.haskellstack.org/en/stable/README/)
```shell
curl -sSL https://get.haskellstack.org/ | sh
```
## gi-gtk
[https://github.com/haskell-gi/haskell-gi](gi-gtk)
```shell
sudo port install gobject-introspection webkit2-gtk gtksourceview3
```
## poppler
```shell
sudo port install poppler poppler-data
```
## XQuartz
[XQuartz](https://www.xquartz.org/)  
PDFを描画するウィンドウを立ち上げるのに必要です。  
## stanford CoreNLP Server with Docker
### docker
[https://hub.docker.com/editions/community/docker-ce-desktop-mac/](https://hub.docker.com/editions/community/docker-ce-desktop-mac/)
### stanford CoreNLP Parser
```shell
sudo docker pull graham3333/corenlp-complete
```
[stanford CoreNLP Parser](https://stanfordnlp.github.io/CoreNLP/other-languages.html)のdockerの箇所。  
[Graham MacDonald氏のリンク](https://hub.docker.com/r/graham3333/corenlp-complete)をご参考です。  
## Mecab
[こちら](https://joppot.info/2013/12/08/80)を参考にしました。
### Mecabのインストール
```shell
sudo port install mecab
sudo port install mecab-ipadic-utf8
```
### 辞書の変更
/opt/local/etc/にあるmecabrcの中身を書き換えます(sysdicからipadic-utf8へ変更)。

Before:
```shell
dicdir = /opt/local/lib/mecab/dic/sysdic
```

After:
```shell
dicdir = /opt/local/lib/mecab/dic/ipadic-utf8
```


## poppyS
PDFリーダーはpoppySと命名しました。  
### git clone
```shell
git clone https://github.com/polymonyrks/poppyS.git
```
### stack build
#### caution
##### environmental value
以下の環境変数を指定してください。
```shell
export XDG_DATA_DIRS=/opt/local/share
export XDG_DATA_HOME=/opt/local/share
export XDG_CONFIG_DIRS/opt/local/etc/xdg
```

```shell
cd poppyS
stack build --extra-lib-dirs="/opt/local/lib" --extra-include-dirs="/opt/local/include"
```
iconvに関するエラーが出た場合には(多分出ると思います)、
以下のコマンドでlib-iconvを一旦無効化してから再度上のコマンドでビルド、その後に「必ず」activateして有効に戻すようにしてください。  
```shell
sudo port deactivate libiconv
stack build --extra-lib-dirs="/opt/local/lib" --extra-include-dirs="/opt/local/include"
sudo port activate libiconv
```

これでインストール完了です。
## プログラムの実行
[Ubuntu](./READMEJP.md)と同様なので、そちらを参照ください。

## 注意点（全体が映らない場合）
注意点として、初っ端の表示時に見切れたり、全体が表示されなかったりします。  
その際はwボタンを２回押せば全体が映ります。
