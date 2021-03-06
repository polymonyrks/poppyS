# poppyS - Syntactical Highlighting for Natural Language in Haskell
# Preamble
Syntactically highlighting PDF Documents in Haskell.  
This is a Functional Toy Programming. Experimental yet.  
Also an example of Haskell GUI(([gi-gtk](https://hackage.haskell.org/package/gi-gtk))
Japanese page is [here](./READMEJP.md).
![demo](./poppySMovie.gif)
# Installation
## OS
Recommended: [Ubuntu 20.04 LTS (Focal Fossa) Desktop](https://releases.ubuntu.com/20.04/) or [Ubuntu 18.04 LTS (Bionic Beaver) Desktop](https://releases.ubuntu.com/18.04/).  
[lubuntu 20.04 Desktop](https://lubuntu.me/downloads/) also works.  
Instruction for [Windows 10 (home / pro)](https://www.microsoft.com/) is [here(other page)](./READMEWIN.md)
For other Distros, or other OS, equivalent process may work.  
## Update Package List
```shell
sudo apt update
```
## Haskell Stack
[stack](https://docs.haskellstack.org/en/stable/README/)
```shell
wget -qO- https://get.haskellstack.org/ | sh
```
## gi-gtk
[https://github.com/haskell-gi/haskell-gi](https://github.com/haskell-gi/haskell-gi)
```shell
sudo apt install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
```
## poppler
```shell
sudo apt install libpoppler-dev libpoppler-glib-dev
```
## Stanford CoreNLP Server with Docker
### Docker
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
### Stanford CoreNLP Server
```shell
sudo docker pull nlpbox/corenlp:2018-10-27
sudo docker pull graham3333/corenlp-complete
```
See [Stanford CoreNLP](https://stanfordnlp.github.io/CoreNLP/other-languages.html) and [Graham MacDonald](https://hub.docker.com/r/graham3333/corenlp-complete)  
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
Assumes you have installed poppyS at $HOME/poppyS. Like below  
```shell
/home/username/poppyS
```
You can check your $HOME by the following command.
```shell
echo $HOME
```
# Execution of poppyS
1. Prepare some PDF Files.  
2. Run Stanford CoreNLP Server.  
3. Execute poppyS with args (target PDF File's path. Only English Documents supported. (Japanese Documents support(more experimental) is [here](./READMEJP.md))).  
for more details, see below.  
## Prepare some PDF Files.
This program poppyS is suitable for hard to read documents such as a bit greek or latin ones.  
Such kind of examples are below.  
Read them yourself with this poppyS.  
1. Computer Science
* [Haskell Wikibooks](https://en.wikibooks.org/wiki/Haskell)
* [Basic Category Theory](https://arxiv.org/abs/1612.09375)
* [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf)
* [SICP](https://web.mit.edu/alexmv/6.037/sicp.pdf)
* [Practical Foundations for Programing Languages](http://profs.sci.univr.it/~merro/files/harper.pdf)
* [Homotopy Type Theory](http://saunders.phil.cmu.edu/book/hott-ebook.pdf)
2. Legal / Financial
* [Copyright Law of the United States](https://www.copyright.gov/title17/title17.pdf)
* [License Agreement Templates](https://otl.stanford.edu/sites/g/files/sbiybj10286/f/exclusive_03-06-2018.pdf)
* [Annual Report (2019) (Apple Inc.)](https://s2.q4cdn.com/470004039/files/doc_financials/2019/ar/_10-K-2019-(As-Filed).pdf)
* [host city contract - Tokyo 2020 Olympic Games](https://gtimg.tokyo2020.org/image/upload/production/jxnoeerdp7hxvgtgxp73.pdf)
3. Biology / Medical
* [Basics of Molecular Biology](https://homes.cs.washington.edu/~tompa/papers/molbio.pdf)
* [Clinical characteristics of 2019 novel coronavirus infection in China](https://www.medrxiv.org/content/10.1101/2020.02.06.20020974v1)
4. User's Manual
* [Camera Manual](https://1vision.co.il/pdfs/vieworks/manual/User_Manual_VA_GigE_EN.pdf)

## Run Stanford CoreNLP Server.
```
sudo docker run -p 9000:9000 nlpbox/corenlp
```
This poppyS program assumes docker uses localhost, so if your environment is different from that you may have error, 
so modify the line below in fromPDF.hs.  
```haskell
command = "http://localhost:9000/?annotators=parse&outputFormat=json&timeout=50000"
```
## Execute poppyS with Args.
```shell
stack exec poppyS-exe TARGETPDFPATH
```
Like below.
(e.g.1) When you are at $HOME/poppyS. 
```shell
stack exec poppyS-exe "pdfs/SICP.pdf"
```
(e.g.2) Full Path also O.K.
```shell
stack exec poppyS-exe "/home/username/poppyS/pdfs/SICP.pdf"
```
# How to read PDF
## Wait a few Seconds
Once execute poppyS, wait a few seconds. Some words will be colored by yellow.  
If no word is colored, this is probably timeout of Stanford CoreNLP Server, so re-execute poppyS.  
## Keyboard
Similar to [Vim Keybindings](https://www.vim.org/).
|  command  |  effect  |
| ---- | ---- |
|  j  | increase page (2 pages) |
|  k  | decrease page (2 pages) |
|  Down  | crop merginal white zones and adjust page size|
|  Right  | increase page (1 page) |
|  Left  | decrease page (1 page) |
|  w  | toggle number of pages displayed (2 pages(maximized) to 1 page and vice versa)|
|  dd  | decolor all phrases |
|  p  | paste(recover) Coloring |
|  x  | cut coloring (enter in or leave of Deleting Mode) |
|  gg  | goto first page |
|  v  | toggle mode (Vanilla)|
|  n  | toggle mode (backward)|
|  m  | toggle mode (forward) |
|  :w Enter  | save the state |
## Mouse Click
|  action  |  effect  |
| ---- | ---- |
|  left  click a word  | coloring corresponding phrases (toggle forward) |
|  right click a word  | coloring corresponding phrases (toggle backword) |
|  left  click blank space  | increase page (2 pages) |
|  right click blank space  | decrease page (2 pages) |
## Coloring
Click some words, then some corresponding phrases are colored. When you click the same word multiple times, the coloring is toggled.  
The toggle schedule is Red -> Blue -> Green -> Purple -> Orange -> Pink -> Aqua -> Cyan -> Red .. .  
Pressing Key dd decolors all phrases. Some special words are remained yellowed.   
Yellowed words are special ones in respect to how much area to be colored when you select them.   
After pressing dd, press key p then it recovers the previous state  
(Caveat: If you click another word after dd then the previous state is updated(destroyed).).  
Even you decolor all the phrases, the tuples (e.g. (Red, word1), (Green, word2), (Aqua, word3) ..) are memoried.  
Next time you click such words, you can start from the previous color.  
This temporary decoloring(dd), recovering(p) and/or starting previous colors is effective for keeping visibility levels.  
Pressing Key x enters into Deleting Mode. Click some words, then the correspoinding phrases become decolored.  
Pressing Key x again leaves Deleting Mode.
# Tactics and Training
Coloring words makes it easy to look clearly some phrases, but too-much-coloring increase entropy(become chaotic).  
Use some tactics and train yourself the use of multiple colors(the latter is like a VR game). I'll show some tactics soon.  
The art of reading natural language with coloring is imcomplete and experimental yet (especially active adhoc coloring by readers (not by writer)), but someday will be a common art I appreciate.  
# License
(see also LICENSE file included with Haskell source.)  
Modified BSD3. For personal use and/or educational purpose use are O.K.  

I also have some patent applications relating to this source files and technology used therein.  
If you comply with the LICENSE Terms, then I never assert the mentioned usage described in LICENSE Terms by such intellectual properties.  

If you are interested in commercial use, then please contact me.  
  * Functional Toy Manufactureing (Japanese Homepage) (https://www.polymony.net)  
  * Email: polymonyrks@polymony.net or polymonyrks@gmail.com  
