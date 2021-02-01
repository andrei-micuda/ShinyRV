# ShinyRV - Proiect Probabilități și Statistică

## Introducere

Proiectul de față urmărește crearea unei aplicații web (folosind pachetul Shiny) care permite lucrul cu variabile aleatoare discrete și continue. Acesta a fost realizat de Anghel Alin, Blănar George și Micudă Andrei.

## Dependențele Proiectului

Aplicația folosește multiple pachete oferite de R prin CRAN, utilitatea fiecăruia fiind detaliată mai jos:

* [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html) - framework-ul de bază cu ajutorul căruia a fost realizată aplicația
* [`hash`](https://cran.r-project.org/web/packages/hash/index.html) - permite definirea unor structuri similare cu obiectele de tip JSON, utilizat pentru stocarea mai usoara a datelor
* [`Rlab`](https://cran.r-project.org/web/packages/Rlab/index.html) -
* [`stringr`](https://cran.r-project.org/web/packages/stringr/index.html) - folosit pentru parsarea input-ului dat de utilizator, precum și realizarea diferitor operații pe string-uri
* [`discreteRV`](https://cran.r-project.org/web/packages/discreteRV/index.html) - 

## Comentarea codului


## Dificultăți în realizarea cerințelor
1. O problema întampinată de toți trei a fost cea de parsare a inputurilor complexe (exemple ar fi inputurile in care ceream vectori, functii care trebuiau aplicate pe distributii sau inputuri care implicau lucruri cu evenimente). In cazul inputurilor care trebuiau evaluate, precum erau vectorii functiile folosite au fost `exec` si `eval`, cu ajutorul cărora puteam executa cod R dintr-un string. Pentru validarea inputuri-lor complexe (calculul unor probabilități condiționate), am utilizat expresii regulate împreună cu funcțiile oferite de `stringr`.
2. Acomodarea cu workflow-ul impus de Shiny, întrucât crearea unei aplicații web în R este destul de diferită și restrictivă comparativ cu alte framework-uri ca React sau Angular