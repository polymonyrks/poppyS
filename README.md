# PoppyS - Syntactical Highlighting for Natural Language
# Preamble
Syntactically highlighting PDF Documents.  
Experimental.  
An example of Haskell GUI(([gi-gtk](https://hackage.haskell.org/package/gi-gtk))
![demo](./poppySMovie.gif)
# Installation
## OS
Recommended OS:[Ubuntu 20.04 LTS (Focal Fossa)](https://releases.ubuntu.com/20.04/)  
lubuntu 20.04 also works.  
For other Distros, MacOS, or Windows et.c., equivalent process may work (not checked).  
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
## Stanford CoreNLP Server with docker
### Docker
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
### Stanford CoreNLP Parser
```shell
sudo docker pull graham3333/corenlp-complete
```
See [stanford CoreNLP Parser](https://stanfordnlp.github.io/CoreNLP/other-languages.html) and [Graham MacDonald](https://hub.docker.com/r/graham3333/corenlp-complete)  
## poppyS(PDF Document Reader)
### git clone
```shell

git clone https://github.com/polymonyrks/poppyS.git
```
### stack build
```shell
cd poppyS
stack build
```
# Execution of PoppyS
1. Put a PDF File into ./pdfs Directory. Only English and one Column Documents.  
2. Run Stanford CoreNLP Server.
3. Execute PoppyS.
## PDF Examples
1. Computer Science
* [Haskell Wikibooks](https://en.wikibooks.org/wiki/Haskell)
* [Basic Category Theory](https://arxiv.org/abs/1612.09375)
* [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf)
* [SICP](https://web.mit.edu/alexmv/6.037/sicp.pdf)
* [Practical Foundations for Programing Languages](http://profs.sci.univr.it/~merro/files/harper.pdf)
* [Homotopy Type Theory](http://saunders.phil.cmu.edu/book/hott-ebook.pdf)
2. Legal
* [Copyright Law of the United States](https://www.copyright.gov/title17/title17.pdf)
* [License Agreement Templates](https://otl.stanford.edu/sites/g/files/sbiybj10286/f/exclusive_03-06-2018.pdf)
* [Annual Report (2019) (Apple Inc.)](https://s2.q4cdn.com/470004039/files/doc_financials/2019/ar/_10-K-2019-(As-Filed).pdf)
* [host city contract - Tokyo 2020 Olympic Games](https://gtimg.tokyo2020.org/image/upload/production/jxnoeerdp7hxvgtgxp73.pdf)
3. Medical
* [Basics of Molecular Biology](https://homes.cs.washington.edu/~tompa/papers/molbio.pdf)
* [Clinical characteristics of 2019 novel coronavirus infection in China](https://www.medrxiv.org/content/10.1101/2020.02.06.20020974v1)

## Run Stanford CoreNLP Server
```
sudo docker run -p 9000:9000 nlpbox/corenlp
```
## Execute poppyS
```shell
stack exec poppyS-exe
```
# How to read PDF
## Wait a second
Once execute poppyS, wait for a few seconds, then some words will be colored by yellow.
If no word is colored, then re-Execute poppyS.
## Keyboard
|  command  |  effect  |
| ---- | ---- |
|  j  | increase page (2 pages) |
|  k  | decrease page (2 pages) |
|  Right  | increase page (1 pages) |
|  Left  | decrease page (1 pages) |
|  Down  | crop merginal white zones and adjust page size|
|  x  | decoloring of some phrases |
|  dd  | decoloring of all phrases |
|  :w Enter  | save coloring setting |
## Mouse
Click some words, then some phrases are colored. This coloring is toggled. Click the same word multiple times.  
# License
(see also LICENSE file included with Haskell source.)  
Based on BSD3. For personal use and/or educational purpose use are O.K.  

I also have some patent applications relating to this source files and technology used there.  
If you comply with the LICENSE Terms, then I never assert the mentioned usage in LICENSE Terms by such intellectual properties.  

If you interested in commercial use, then please contact me.  
  * Functional Toy Manufactureing (Japanese Homepage) (https://www.polymony.net)  
  * Email: polymonyrks@polymony.net or polymonyrks@gmail.com  
