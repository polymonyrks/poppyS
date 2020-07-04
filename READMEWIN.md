# poppyS - Syntactical Highlighting for Natural Language in Haskell
# Preamble
Syntactically highlighting PDF Documents in Haskell.  
This is a Functional Toy Programming. Experimental yet.  
Also an example of Haskell GUI(([gi-gtk](https://hackage.haskell.org/package/gi-gtk))
![demo](./poppySMovie.gif)
# Installation
## Windows 10
This ReadMe is for Windows 10 (pro / home).  
Instruction for Ubuntu is [here](./README.md).  
## Haskell Stack
[stack](https://docs.haskellstack.org/en/stable/README/)  
Download and execute win64bitInstaller.  
## gi-gtk
add some modification to [this manual](https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows).
### Msys2
[msys2](https://msys2.github.io/)
### Haskell Platform
[Haskell Platform](https://www.haskell.org/platform/download/8.0.2/HaskellPlatform-8.0.2-a-minimal-x86_64-setup.exe)
### Set Environment Variables(System Variables).
Set four environment variables(PATH, PKG_CONFIG_PATH, XDG_DATA_DIRS, HOME).  
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
C:\Users\yourUsersName
```
yourUserName is your Home Folder. Assumes git clone below is done at this folder.  

### Gtk
execute below in msys2 terminal.
```shell
pacman -S -q --noconfirm mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3
```
### icu
```shell
pacman -S mingw-w64-x86_64-icu
```
### poppler
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
See [Stanford CoreNLP](https://stanfordnlp.github.io/CoreNLP/other-languages.html) and [Graham MacDonald](https://hub.docker.com/r/graham3333/corenlp-complete)  



## poppyS(PDF Document Reader)
### git
[https://gitforwindows.org/](https://gitforwindows.org/)  
### git clone
```shell
git clone https://github.com/polymonyrks/poppyS.git
```
### reboot system
reboot system here.
### stack install
```shell
stack update
chcp 65001
cd poppyS
stack install --extra-lib-dirs=c:\msys64\mingw64\lib --extra-include-dirs=c:\msys64\mingw64\include
```
Assumes you have installed poppyS at $HOME/poppyS. Like below  
```shell
C:\Users\username\poppyS
```
### replace "zlib1.dll"
Perhaps you encounter a dll error. You have to copy "C:\msys64\mingw64\bin\zlib1.dll" to "C:\Users\yourUsersName\AppData\Local\Programs\stack\x86_64-windows\ghc8.6.5\mingw\bin".
# Execution of poppyS
0. Create Symbolic link of poppyS.
1. Prepare some PDF Files.  
2. Run Stanford CoreNLP Server.  
3. Drag and Drop the PDF File to the Symbolic link.  
for more details, see below.  
## Create Symbolic link of poppyS.
poppyS's exe File is installed at "C:\Users\yourUsersName\AppData\Roaming\local\bin\poppyS-exe.exe". Create Symbolic link of "poppyS-exe.exe" to Some place(e.g. Desktop).
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
docker run -p 9000:9000 nlpbox/corenlp
```
## Drag and Drop a PDF File to the Symbolic link Icon. 
Drag and Drop works. This executes the poppyS with args automatically.  
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
|  Right  | increase page (1 page) |
|  Left  | decrease page (1 page) |
|  Down  | crop merginal white zones and adjust page size|
|  w  | toggle number of pages displayed (2 pages(maximized) to 1 page and vice versa)|
|  dd  | decolor all phrases |
|  p  | paste(recover) Coloring |
|  x  | cut coloring (enter in or leave of Deleting Mode) |
|  gg  | goto first page |
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
