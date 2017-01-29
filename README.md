Wymagania:
 - Haskell/GHC
 - Alex
 - Happy

Instalacja na Ubuntu:
```
sudo apt-get install haskell-platform
```

Kompilacja
```
./make.sh
```

Testy analizy statycznej (mało czytelne):
```
./test.sh
```

Sposób użycia
```
cat program.imp | ./compiler > code.mr; ./interpreter code.mr
```
Zrobione:
 - Lekser
 - Parser
 - Analiza statyczna
 - Drobne optymalizacje (zwijanie stałych, tożsamości arytmetyczne)
 - Generator kodu (z optymalnym mnożeniem, dzieleniem i modulo)

Do zrobienia:
 - Lepsze optymalizacje (częściowa ewaluacja). Zrobić przegląd tożsamości arytmetycznych (na poziomie generatora kodu).
 - Lepsze wiadomości o błędach (error token https://www.haskell.org/happy/doc/html/sec-error.html)
 - Wydajniejsze dzielenie (pewnie da się).
 - Usunąć martwy kod i dodać komentarze.
 - Lepsze zarządzanie pamięcią.
 - Nie oszukujmy się, komu chce się to robić?

Testy:
 - Testy na analizę statyczną działały jak ostatnio sprawdzałem.
 - Testy od wykładowcy (tests/speed) działają. Czasy (dla wejść jak w komentarzach):
   - program0.imp — 5034
   - program1.imp — 4573804
   - program2.imp — 41317005
   - program3.imp — 2281087
   - program4.imp — 15281
   - program5.imp — 20242
   - program6.imp — 3056
   - program7.imp — 19114

Uwagi:
 - Specyfikacja języka Imp oraz maszyny rejestrowej jest w pliku labor4.pdf
 - Autorem interpretera (symulatora maszyny) jest dr Maciej Gębala.
