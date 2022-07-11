# PassQ
Fortranで書かれた自動パスワード生成器です。<br>
わざわざFortranで書いた意味はないです。<br>

## 強度について
※このプログラムでは、予め英数字と記号を含んでおります。(多分)最強です。<br>
32桁が強いですが、根性がある人は1024桁にしてみるのもありかもしれないです。<br>

## 動作確認済みOS
* Debina WSL<br>
それくらい<br>

## インスール方法
```
sudo apt install gfortran make
git clone https://github.com/ware255/PassQ.git
cd PassQ
make
```
実行は `./PassQ` です。
