Večnivojsko Hiearhično Modeliranje
================

``` r
library('lme4')
```

    ## Loading required package: Matrix

``` r
library('lmerTest')
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
df <- read.csv('data_frame.csv', header=TRUE)
```

Večinivojsko modeliranje je predvsem primerno pri raziskavah, kjer lahko
podatke grupiramo po določeni spremenljivki in gre za generalizacijo
linearne regresije. Poznamo več vrst različnih modelov: <br />

#### Random intercept model

Predstavlja modele, kjer dovolimo, da se intercepti oz. začetne
vrednosti regresijskih premic spreminjajo. Ta model predpostavlja, da so
koeficienti premic enaki. Z njim si lahko pomagamo pri računanju ICC oz.
intraclass correlation koeficientu, ki pove ali je večnivojsko
modeliranje potrebno (tipično vrednosti nad 0.05 nakazujejo, da je
večnivojsko modeliranje potrebno).

#### Random slopes model

Predstavlja modele, kjer se koeficienti lahko spreminjajo znotraj
različnih grup oz. skupin. Ta model predpostavlja, da so začetne
vrednosti enake.

#### Random intercept and slopes model

Najbolj realističen model, ki vsebuje različne začetne vrednosti kot
tudi koeficiente premic v različnih skupinah in je najbolj kompleksen.

Pri sami izgradnji modelov ni tako pomembno število vnosov znotraj
posamezne skupine. Bolj je pomembno je število različnih skupin, ki
znaša minimalno 5.

V našem primeru je najbolj smiselno podatke grupirati po spremenljivki
`str_lay_i`. Pri deskriptivni analizi smo namreč opazili, da na kakovost
prikaza novic najbolj vpliva izbira določenega pogleda (spremenljivka
`layout`) in prisotnost slik (spremenljivka `images`). Spremenljivka
`str_lay_i` pa predstavlja niz znakov, doližnine dva (`layout` +
`images`), ki pove informacije o trenutem pogledu in ali so slike
pristone. Možne vrednosti spremenljivke `str_lay_i` so torej sledeče:

<ul>

<li>

gw - `gridView` + `withImages`

</li>

<li>

ln - `largeCards` + `noImages`

</li>

<li>

lw - `largeCards` + `withImages`

</li>

<li>

mw - `miniCards` + `withImages`

</li>

<li>

xn - `xLargeCards` + `noImages`

</li>

<li>

xw - `xLargeCards` + `withImages`

</li>

</ul>

Gradnjo modelov bomo začeli pri manj kompleksnih modelih (z manj
parametri) in postoma dodajali nove. Modele bomo skušali zgraditi tako,
da bodo čim bolj jasno predstavljene smiselne interakcije. Modele bomo
primerjali s pomočjo funkcije `anova`. Najboljši model bomo potem
primerjali še z ostalimi (Random forest, Bayesov klasifikator,
odločitveno drevo, AdaBoost).

### Izgradnja večnivojskega modela (angl. Multilevel Modela)

Začeli bomo torej s t.i. `random intercept modelom`, ki nam bo povedal,
ali je smiselno izvajati večnivojsko modeliranje. V našem primeru smo
opazili, da večnivojsko modeliranje je smiselno, saj vrednost ICC znaša
5.611 / (5.611 + 12.385) = 0.312, kar pomeni, da lahko nadaljujemo z
večnivojskim modeliranjem, saj do 31% variance prihaja na podlagi
izbire pogleda in prisotnosti slik (spremenljivka `str_lay_i`).

Iz ispisa funkcije `summary` lahko sklepamo, da povprečna vrednost ocen
pogledov znaša 1.4349 (vrednost spremenljivke `(Intercept)`).

Na podlagi izpisa funkcije `coefficients` pa opazimo, da imajo pogledi s
slikami v splošnem višje ocene, kot tisti brez. To je tudi pričakovano,
saj prikazi novic brez slik niso tipični. V današnjih aplikacijah ali
spletnih straneh redko beremo novice brez slik, saj slike dodajo
estetsko vrednost aplikaciji hkrati pa lahko več povejo o novici kot
zgolj le besedilo in so v splošnem ljudem bolj zanimive od samega
besedila.

Prav tako opazimo negativne vrednosti pri vrednosti `xn` (`xLargeCards +
noImages`), saj tak način prikaza ni standarden. Po aplikaciji
navigiramo s premiki levo-desno in ne z navigacijo gor-dol. Na zaslonu
lahko vidimo torej le eno novico hkrati, saj se uporabnik lahko tako
lažje osredotoči na samo novico. V primeru ko slik torej ni, je na
zaslonu podanih premalo informacij (le besedilo). Tako si lahko
razložimo, zakaj ima prikaz `largeCards` višje ocene od prikaza
`xLargeCards`, ko slik ni, saj uporabnik ima na voljo več informacij,
ker je prikazanih več novic hkrati.

Vrednost 0.003151307 pri vrednosti `gw` (`gridView + withImages`) prav
tako ni presenetljiva, saj uporabniku ponuja premalo informacij na
zaslonu, ker ima na voljo le slike s kratkim besedilom o novici. To
lahko sklepamo na podlagi tega, da vrednost `mw` (`miniCards +
withImages`)

``` r
null.model <- lmer(score ~ 1 + (1|str_lay_i), data=df)
summary(null.model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: score ~ 1 + (1 | str_lay_i)
    ##    Data: df
    ## 
    ## REML criterion at convergence: 4497.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0152 -0.4579  0.3038  0.5879  2.2139 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  str_lay_i (Intercept)  5.611   2.369   
    ##  Residual              12.385   3.519   
    ## Number of obs: 836, groups:  str_lay_i, 6
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error     df t value Pr(>|t|)
    ## (Intercept)   1.4349     0.9777 5.0271   1.468    0.202

``` r
coefficients(null.model)
```

    ## $str_lay_i
    ##     (Intercept)
    ## gw  0.003151307
    ## ln  0.275125959
    ## lw  4.611445404
    ## mw  2.580268579
    ## xn -1.791197939
    ## xw  2.930906540
    ## 
    ## attr(,"class")
    ## [1] "coef.mer"

V nadaljevanju si bomo pogledali, kako posamezne spremenljivke na prvem
nivoju vplivajo na končno oceno uporabnika. Nato bomo poskušali dodajati
korelacije med spremenljivkami

#### Vpliv spremenljivke `user.activity`

Na podlagi spodnjega izpisa lahko vidimo, da v povprečju najslabše ocene
dobivajo prikazi novic, ko se uporabnik vozi (vrednost `(Inercept)`
znaša -2.3775). Nato pa ocene rastejo, ko se uporabnik sprehaja
(vrednost `user.activityON_FOOT` 3.3328) , ko pa je uporabnik pri miru
pa daje najvišje ocene (vrednost `user.activitySTILL` 5.2966).

Ta pojav si lahko razložimo tako, da je uporabnik slabše osredotočen na
novice, saj ga bolj zanima dogajanje v okolici in si težje nastavi
primeren pogled, ki bi mu lahko podal dovolj informacij.

Ocene potem rastejo med hojo in ko je uporabnik v stanju
mirovanja.

``` r
user_activity.model <- lmer(score ~ 1 + user.activity + (1|str_lay_i), data=df)
summary(user_activity.model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: score ~ 1 + user.activity + (1 | str_lay_i)
    ##    Data: df
    ## 
    ## REML criterion at convergence: 4238.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6891 -0.3885  0.2775  0.6062  3.0725 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  str_lay_i (Intercept) 3.377    1.838   
    ##  Residual              9.097    3.016   
    ## Number of obs: 836, groups:  str_lay_i, 6
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)           -2.3775     0.7966   6.0390  -2.985   0.0243 *  
    ## user.activityON_FOOT   3.3328     0.3635 828.9871   9.168   <2e-16 ***
    ## user.activitySTILL     5.2966     0.3082 830.1711  17.187   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) u.ON_F
    ## usr.ON_FOOT -0.244       
    ## usr.ctSTILL -0.291  0.669

#### Vpliv spremenljivke `font.size`

Iz spodnjega modela lahko ugotovimo vpliv velikosti pisave na oceno
uporabnika. Opazimo, da je v splošnem ocena manjša, ko ima uporabnik
manjšo pisavo (vrednost spremenljivke `font.sizesmall-font` znaša
-1.0398). To si lahko razložimo, s tem da je velika pisava bolj razločna
in pregleda, uporabnik lahko z manj truda prebere novice. Hkrati pa
vrednost ni veliko manjša koeficient znaša namreč le -1.0398. V
nadaljevanju bomo skušali ugotoviti ali morda fizična aktivnost
uporabnika vpliva na izbiro pisave (ko uporabnik hodi je smiselno imeti
večjo pisavo medtem ko je pri miru pa ima lahko tudi manjšo, saj se
lahko bolj osredotoči na aplikacijo).

``` r
font_size.model <- lmer(score ~ 1 + font.size + (1|str_lay_i), data=df)
summary(font_size.model)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: score ~ 1 + font.size + (1 | str_lay_i)
    ##    Data: df
    ## 
    ## REML criterion at convergence: 4481.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1843 -0.5052  0.2575  0.5789  2.3744 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  str_lay_i (Intercept)  5.135   2.266   
    ##  Residual              12.156   3.487   
    ## Number of obs: 836, groups:  str_lay_i, 6
    ## 
    ## Fixed effects:
    ##                     Estimate Std. Error       df t value Pr(>|t|)    
    ## (Intercept)           2.0212     0.9466   5.2589   2.135   0.0831 .  
    ## font.sizesmall-font  -1.0398     0.2516 832.7682  -4.133 3.94e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## fnt.szsmll- -0.149

#### Vpliv spremenljivk `font.size` in `user.activity` v kombinaciji

Na podlagi spodnjih dveh modelov lahko sklepamo, da moramo upoštevati
interakcijo met uporabniško aktivnostjo in velikosjo pisave. Vrednosti
ocen `AIC` in `BIC` so manjše. Prav tako pa p vrednost (znaša 0.0001826)
nakazuje na statistično relevantno razliko med modeloma. Če podrobneje
analiziramo boljši model na podlagi klica funkcije `summary` lahko
ugotovimo sledeče:

<ul>

<li>

Vrednost ocen, ko je uporabnik v avtomobilu je še vedno manjša od
vrednosti ocen, ko uporabnik hodi ali pa miruje (to lahko sklepamo na
podlagi vrednosti za `(Intercept)`, ki znaša -2.31541,
`user.activityON_FOOT`, ki znaša 4.52285 in `user.activitySTILL`, ki
znaša 5.48944.

</li>

<li>

Na podlagi vrednosti `font.sizesmall-font`, ki znaša -0.06605 opazimo,
da je večja pisava bolj priljubljena, a je vpliv v splošnem zanemarljivo
majhen, saj je skoraj 0, prav tako pa to potrjuje p-vrednost, ki znaša
0.904112

</li>

<li>

Lahko pa opazimo, da ima manjša velikost pisave veliko bolj negativne
vrednosti kot velika pisava, ko uporabnik hodi, vrednost koeficienta
`font.sizesmall-font:user.activityON_FOOT` znaša kar -2.46884

</li>

<li>

Vrednost koeficienta `font.sizesmall-font:user.activitySTILL` pa znaša
le -0.36350, kar pomeni, da nekoliko slabše ocene pridobivajo pogledi z
manjšo pisavo, a je to skoraj zanemarljivo, kar nakazuje tudi
p-vrednost, ki znaša 0.544209

</li>

</ul>

Na podlagi zgornih sklepov sledi, da ko se uporabnik vozi, so ocene
negativne tako za veliko kot tudi za majhno pisavo, a so razlike
zanemarljivo majhne (manjša pisava je slabše ocenjena za faktor
`font.sizesmall-font`, ki znaša -0.06605). Med hojo pa so opazne velike
razlike med pisavami, saj so veliko slabše ocenjeni pogledi z manjšo
pisavo od tistih z veliko (manjša pisava je slabše ocenjena za faktor
-2.46884). Ko pa uporabnik miruje velja podobno kot za vožnjo, vrednost
manjše pisave je slabša le za faktor -0.36350, ki je zanemarljivo
majhen.

``` r
font_size_use_activity.model.no_interaction <- lmer(score ~ 1 + font.size + user.activity + (1|str_lay_i), data=df)

# je morda model z upoštevanjem interakcije boljši ? 
font_size_use_activity.model.with_interaction <- lmer(score ~ 1 + font.size*user.activity + (1|str_lay_i), data=df)

# ugotovimo lahko s klicem funkcije anova
anova(font_size_use_activity.model.no_interaction, font_size_use_activity.model.with_interaction)
```

    ## refitting model(s) with ML (instead of REML)

    ## Data: df
    ## Models:
    ## font_size_use_activity.model.no_interaction: score ~ 1 + font.size + user.activity + (1 | str_lay_i)
    ## font_size_use_activity.model.with_interaction: score ~ 1 + font.size * user.activity + (1 | str_lay_i)
    ##                                               npar    AIC    BIC  logLik
    ## font_size_use_activity.model.no_interaction      6 4237.3 4265.7 -2112.6
    ## font_size_use_activity.model.with_interaction    8 4224.1 4261.9 -2104.0
    ##                                               deviance  Chisq Df Pr(>Chisq)    
    ## font_size_use_activity.model.no_interaction     4225.3                         
    ## font_size_use_activity.model.with_interaction   4208.1 17.217  2  0.0001826 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# podrobnejša analiza boljšega modela
summary(font_size_use_activity.model.with_interaction)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: score ~ 1 + font.size * user.activity + (1 | str_lay_i)
    ##    Data: df
    ## 
    ## REML criterion at convergence: 4208.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8204 -0.4520  0.2217  0.5759  3.1052 
    ## 
    ## Random effects:
    ##  Groups    Name        Variance Std.Dev.
    ##  str_lay_i (Intercept) 3.052    1.747   
    ##  Residual              8.813    2.969   
    ## Number of obs: 836, groups:  str_lay_i, 6
    ## 
    ## Fixed effects:
    ##                                           Estimate Std. Error        df t value
    ## (Intercept)                               -2.31541    0.83376   8.82886  -2.777
    ## font.sizesmall-font                       -0.06605    0.54813 826.93223  -0.121
    ## user.activityON_FOOT                       4.52285    0.53677 825.57599   8.426
    ## user.activitySTILL                         5.48944    0.46535 825.96179  11.796
    ## font.sizesmall-font:user.activityON_FOOT  -2.46884    0.72182 825.51977  -3.420
    ## font.sizesmall-font:user.activitySTILL    -0.36350    0.59913 825.49579  -0.607
    ##                                          Pr(>|t|)    
    ## (Intercept)                              0.021890 *  
    ## font.sizesmall-font                      0.904112    
    ## user.activityON_FOOT                      < 2e-16 ***
    ## user.activitySTILL                        < 2e-16 ***
    ## font.sizesmall-font:user.activityON_FOOT 0.000656 ***
    ## font.sizesmall-font:user.activitySTILL   0.544209    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) fnt.s- u.ON_F u.STIL f.-:.O
    ## fnt.szsmll- -0.410                            
    ## usr.ON_FOOT -0.401  0.609                     
    ## usr.ctSTILL -0.467  0.707  0.732              
    ## f.-:.ON_FOO  0.304 -0.747 -0.741 -0.539       
    ## fn.-:.STILL  0.368 -0.902 -0.560 -0.756  0.682

#### Vpliv spremenljivke `theme`

#### Vpliv spremenljivke `time.of.day`

#### Vpliv spremenljivke `env.brightness`

#### Vpliv spremenljivke `battery.level`

#### Vpliv spremenljivke `theme` in `time.of.day` v kombinaciji

#### Vpliv spremenljivke `theme` in `env.brightness` v kombinaciji

#### Vpliv spremenljivke `theme` in `battery.level` v kombinaciji

#### Izgradnja končnega modela

#### Analiza končnega modela

#### Testiranje končnega modela
