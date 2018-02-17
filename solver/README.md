# Pracownia nr. 1
<i>Metody Programowania, semestr letni 2016/2017 II UWr</i><br>
<i>Program szukający wartościowań spełniających zadany zbiór klauzul </i><br>
<i>Język Programowania: Prolog</i>
<br>
<br>
Sposób rozwiązania:<br>
<i>
 1. Wstępne przesortowanie klauzul. 
 2. Nadanie wartościowań zmiennym tym, które występują jako pojedynczy literał w klauzuli.
 3. Sprawdzanie spełnialności klauzul złożonych z kilka literałów względem literałów które już nadano wartość. 
    Wyjmowane są te klauzule z listy które są zawsze spełnialne a literały z niej zapamiętane, nadając wartość 
 ogólną 'x'. 
 4. Zmiennym, w klauzulach, które pozostały w liście zostaną nadane wartościowania. W każdym kroku
 sprawdzając czy inna klauzula jest już spełanialna (postępując jak w kroku 3).
</i>
<br>
<br>
<br>
<br>
<br>

Tresc zadania: <b>prac1.pdf</b><br>
Testy poprawnościowe/wydajnościowe: <b>kamil_breczko_tests.pl</b><br>
Rozwiązanie zadania: <b>kamil_breczko.pl</b><br>
Sprawdzarka: <b>prac1.pl</b><br>
<br>
<br>
<br>

Sprawdzaczkę należy uruchomić w katalogu w którym znajdują się testy i rozwiązania, poleceniem: <br>
<i>swipl prac1.pl</i>

Za pomocą predykatu: 
<ul>
<li>run_test/1 można uruchomić pojedynczy test</li>
<li>test_all/0 wszystkie testy</li>
</ul>


<b>Widok po uruchomieniu sprawdzarki:</b><br>
<img src="./tests.png" alt="Testy" />
