# Systemu do weryfikacji układów cyfrowych
<i>Metody Programowania, semestr letni 2016/2017 II UWr </i>
<i>Język Programowania: Prolog</i> <br>
<br>
Implementacja systemu do weryfikacji prostych układów cyfrowych. System korzysta z: 
 - [x] interpreter języka HDML (język opisu sprzętu);
 - [x] program obliczający rezolwentę dwóch klauzul oraz szukający rezolucyjnego dowodu sprzeczności;
 - [x] program szukający wartościowań spełniających zadany zbiór klauzul;
 
 ## Interpreter języka HDML
Opis jezyka HDML: [prac3_extra.pdf](./prac3_extra.pdf) <br>
Interpreter: [kamil_breczko_eval.pl](./kamil_breczko_eval.pl) <br>
Sprawdzarka: [verify.pl](./verify.pl) <br>
<br>
Testy poprawnościowe należy uruchomić za pomocą polecenia: <br>
swipl -f verify.pl -t main PROGRAM TEST_SUITE <br>
gdzie: <br>
PROGRAM      jest ścieżką do programu w języku HDML <br>
TEST_SUITE   jest ścieżką do pliku z testami <br>

