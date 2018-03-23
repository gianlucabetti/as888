000100971103     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200971114     F*----------------------------------------------------
000300971114     F* Reperimento librerie per trasmissioni
000400971114     f*
000500971114     f* ATTENZIONE!!! il chiamante di questo programma si deve preoccupare
000600020715     f*  di aggiungere in lista la GAITRAGRU se passa uno dei 2 P.O.
000700971114     f*  in input perche' deve leggere azorg
000800971114     F*----------------------------------------------------
000900971114     Fazorg01l  iF   E           K DISK    usropn
001000971107     d* SCHIERE
001100020715     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MSG VIDEO
001200971201     D minu            S              1    DIM(26) CTDATA PERRCD(26)            MSG VIDEO
001300971201     D maiu            S              1    DIM(26) CTDATA PERRCD(26)            MSG VIDEO
001400981210     D lis             S             11    DIM(25)
001500981210     D lis8            S              8    DIM(25)
001600981210     D lis6            S              6    DIM(25)
001700981210     D azi             S              1    DIM(25)
001800981210     D fine            S              5    DIM(25)
001900971107     d**
002000971107     D k10             S              1    DIM(10)
002100971201     D k10mi           S              1    DIM(10)
002200971201     D k10ma           S              1    DIM(10)
002300971107     d**
002400020715     D Og143         E DS
002500020715     D dsbs55        E DS                  extname(tibs55ds)
002600971103     d libsdi          ds
002700971104DAGR D   sdisif                      10    INZ('SDITRA101 ')
002800020529DAGR D   sdiuni                      11    INZ('UNITRAGRU ')
002900020529DAGR D   sdigru                      11    INZ('GAITRAGRU ')
003000020409DAGR D   sdilra                      11    INZ('GAITRAAZM ')
003100971104DAGR D   sdiazi                      11    INZ('SDITRAxxx ')
003200971103     d libbar          ds
003300971104DAGR D   barsif                      10    INZ('GAITRA201 ')
003400020529DAGR D   barUNI                      11    INZ('UNITRAGRU ')
003500020529DAGR D   bargru                      11    INZ('GAITRAGRU ')
003600971104DAGR D   barlra                      11    INZ('GAITRAAZM ')
003700971104DAGR D   barazi                      11    INZ('GAITRAxxx ')
003800020715     d libuni          ds
003900020715DAGR D   unisif                      10    INZ('FILTRA201 ')
004000020715DAGR D   uniUNI                      11    INZ('UNITRAGRU ')
004100020715DAGR D   unigru                      11    INZ('FILTRAGRU ')
004200020715DAGR D   unilra                      11    INZ('FILTRAAZM ')
004300020715DAGR D   uniazi                      11    INZ('FILTRAxxx ')
004400971111     d libpro          ds
004500020322DAGR D   propro                      11    INZ('GAITRAFIL ')
004600020322     D* LIBRERIA PER RICERCA DEL S.I.
004700971104     c***
004800971104     c     *entry        plist
004900020715     C                   PARM                    dsbs55
005000971104     c**
005100971104     c**
005200020715     c     *like         define    i50psi        wsif
005300020715     c     *like         define    orgfel        savfel
005400020715     c     *like         define    §ogntw        savntw
005500020715     c     *like         define    §ogas         savas
005600020715     c     *like         define    §ogas         apoas
005700020715     c     *like         define    §ogas         ppoas
005800971104     c     *like         define    o50plg        wplg
005900971104     c     *like         define    o50plr        wplr
006000971104     c     *like         define    o50pla        wpla
006100020715     c     *like         define    o50plu        wuni
006200971104     c     *like         define    orgfel        apofel
006300971104     c     *like         define    orgfel        ppofel
006400020715     c     *like         define    orgdit        wdit
006500020715     c     *like         define    orgfil        wpo
006600971104     C* i50TLA =' '  ELABORO E CHIUDO CON RETRN
006700971104     C* i50TLA ='L'  ELABORO E CHIUDO CON LR
006800971104     C* i50TLA ='C'            CHIUDO CON LR   SOLO PER CHIUSURA FILE
006900020715    0c     i50tla        ifeq      'C'
007000971104     C                   SETON                                        lr
007100971104     c**
007200971104     c* controllo se immessi i P.O.
007300971104     c**
007400971104     c                   else
007500971104     c                   clear                   o50lcl
007600020715     c                   clear                   o50sas
007700971104     c                   clear                   o50err
007800971104     c                   clear                   o50msg
007900971104     c                   clear                   o50psi
008000020529     c                   clear                   o50pnt
008100020529     c                   clear                   o50plu
008200020529     c                   clear                   o50plg
008300971104     c                   clear                   o50plr
008400971104     c                   clear                   o50pla
008500971104     c                   clear                   o50psi
008600020529     c                   clear                   o50ant
008700020529     c                   clear                   o50alu
008800020529     c                   clear                   o50alg
008900971104     c                   clear                   o50alr
009000971104     c                   clear                   o50ala
009100020715     c                   clear                   apofel
009200020715     c                   clear                   ppofel
009300020715     c                   clear                   apoas
009400020715     c                   clear                   ppoas
009500020322     c                   clear                   k10
009600971104     c*
009700971107     c                   movel     'PRD  '       wprovafil         5
009800020717     c* se P-O- arrivo pieno e vuoto il p.o. partenza, do per scontato
009900020717     c*  che il p.o. partenza sia la sede
010000020717     c                   if        i50apo>0 and i50ppo=0
010100020717     c                   eval      i50ppo=46
010200020717     c                   endif
010300971114     c*** apro azog solo se mi serve
010400971114     C     i50apo        ifgt      0
010500971114     C     i50ppo        orgt      0
010600971117     c* apro solo il 1 giro
010700971117     c     wopen         ifeq      ' '
010800971114     c                   open      azorg01l
010900971117     c                   movel     '1'           wopen             1
011000971117     c                   endif
011100971114     c                   endif
011200971104     c***
011300971104     c**  PUNTO OPERATIVO DI ARRIVO
011400971104     c***
011500020715    1C     i50apo        ifgt      0
011600020715     c                   eval      WPO=i50apo
011700020715     c                   exsr      chaazorg
011800020715     c* Errore
011900020715    2c                   if        o50err='1'
012000020715    3c                   if        o50msg=*blanks
012100020715     c                   movel     msg(1)        o50msg
012200020715    3c                   endif
012300020715     c
012400020715     c                   goto      endjob
012500020715   x2c                   else
012600020715     c* memorizzo i campi che mi servono
012700020715     c                   movel     savfel        apofel
012800020715     c                   movel     savntw        o50ant
012900020715     c                   movel     savas         apoas
013000020715     c* se wsif pieno è stato già calcolato
013100020715     c                   select
013200020715     c                   when      wsif<>*blanks
013300020715     c                   eval      o50asi=wsif
013400020715     c                   when      wdit='SDI'
013500020715     c                   eval      o50asi=sdisif
013600020715     c                   when      wdit='BAR'
013700020715     c                   eval      o50asi=barsif
013800020715     c                   endsl
013900020715     c
014000020715    2c                   endif
014100020715    1c                   endif
014200971104     c***
014300971104     c**  PUNTO OPERATIVO DI PARTENZA
014400971104     c***
014500020715    1C     i50ppo        ifgt      0
014600020715     c                   eval      WPO=i50ppo
014700020715     c                   exsr      chaazorg
014800020715     c* Errore
014900020715    2c                   if        o50err='1'
015000020715    3c                   if        o50msg=*blanks
015100020715     c                   movel     msg(2)        o50msg
015200020715    3c                   endif
015300020715     c
015400020715     c                   goto      endjob
015500020715   x2c                   else
015600020715     c* memorizzo i campi che mi servono
015700020715     c                   movel     savfel        ppofel
015800020715     c                   movel     savntw        o50pnt
015900020715     c                   movel     savas         ppoas
016000020715     c* se wsif pieno è stato già calcolato
016100020715     c                   select
016200020715     c                   when      wsif<>*blanks
016300020715     c                   eval      o50psi=wsif
016400020715     c                   when      wdit='SDI'
016500020715     c                   eval      o50psi=sdisif
016600020715     c                   when      wdit='BAR'
016700020715     c                   eval      o50psi=barsif
016800020715     c                   endsl
016900020715    2c                   endif
017000020715    1c                   endif
017100020715     c**
017200971104     c**
017300971104     c** p.o. locali: presenti sullo stesso AS
017400020715     c                   if        i50ppo>0 and i50apo>0
017500971104     c     apofel        ifeq      ppofel
017600971104     c                   movel     'S'           o50lcl
017700971104     c                   endif
017800020715     c**
017900020715     c** p.o. stesso §OGAS
018000020715     c     apoas         ifeq      ppoas
018100020715     c                   movel     'S'           o50sas
018200020715     c                   endif
018300020715     c                   endif
018400971104     c***
018500971104     c** SISTEMA INFORMATIVO DI PARTENZA
018600971104     c***
018700020715     c
018800020715     c* Lo determino dal po partenza se c'e'.
018900020715     c* se manca o p.o. partenza =46 lo determino se passato
019000020715     c* altrimenti dalla lista di librerie
019100020715     c* se il p.o. partenza c'e' devo comunque decidere, anche se il S.I.
019200020715     c* l'ho già determinato se sono in prova o in buona
019300020715     c* quindi deve reperire lo stesso la lista di libr. se non passato
019400020322     c                   setoff                                       0102
019500020715     c
019600020715    1c                   if        i50psi=*blanks or o50psi<>*blanks
019700971106     c                   call      'TIBS51C'
019800971106     c                   parm                    lis
019900971104     c**
020000981210    2c                   do        10            x
020100971107     c                   movel     lis(x)        lis6(x)
020200971107     c                   movel     lis(x)        lis8(x)
020300971104     c                   move      lis(x)        fine(x)
020400971104     c                   movel     fine(x)       azi(x)
020500971107    2c                   enddo
020600971104     c
020700971104     c**
020800971104     c                   z-add     1             x                 3 0
020900981203     c
021000020322     C* Ricerco con i primi 6 caratteri della libreria azienda
021100020715     c                   movel     UNIazi        uniricerca        6
021200020715     c     UNIricerca    lookup    lis6(x)                                31
021300020715    2c     *in31         ifeq      *on
021400020322     c                   movel     lis6(x)       w006a
021500020322     c                   movea     w006a         k10(1)
021600971104     c                   else
021700020715     c                   movel     sdiazi        sdiricerca        6
021800020715     c     SDIricerca    lookup    lis6(x)                                31
021900020715    3c     *in31         ifeq      *on
022000020715     c                   movel     lis6(x)       w006a
022100020715     c                   movea     w006a         k10(1)
022200020715     c                   else
022300020322     c                   movel     barazi        barricerca        6
022400020322     c     BARricerca    lookup    lis6(x)                                32
022500020715    4c     *in32         ifeq      *on
022600020322     c                   movel     lis6(x)       w006a             6
022700020322     c                   movea     w006a         k10(1)
022800020715   x4c                   else
022900020715     c* se non ho sistema informativo buono ma lo ho gia' determinalto
023000020715     c*  dal p.o. non segnalo erroe e considero gli ambienti buoni e non
023100020715     c*  di prova
023200020715    5c                   if        o50psi=*blanks
023300020322     c                   movel     '1'           o50err
023400020322     c                   movel     msg(3)        o50msg
023500020322     c                   goto      endjob
023600020715     c                   else
023700020715     c                   seton                                        02
023800020715     c                   goto      sotto
023900020715    5c                   endif
024000020715    4c                   endif
024100971107    3c                   endif
024200020715    2c                   endif
024300971107     c***
024400020419     c                   dow       x <=10 and azi(x)<>'P' and azi(x) <'1' OR
024500020419     c                             x <=10 and azi(x)<>'P' and azi(x) >'9'
024600020419     c                   add       1             x
024700020419    2c                   enddo
024800020419     c
024900020419     c** Se X = 11 vuol dire che non sono in un sistema informativo previsto
025000020419     c**  dal pgm per cui esco senza fare nulla
025100020419     c                   if        x=11
025200020715     c                   if        o50psi=*blanks
025300020419     c                   movel     '1'           o50err
025400020419     c                   movel     msg(3)        o50msg
025500020419     c                   goto      endjob
025600020715     c                   else
025700020715     c                   seton                                        02
025800020715     c                   goto      sotto
025900020715    4c                   endif
026000020419    4c                   endif
026100971104     c**
026200020715     c** Se c'e' già impostoquello gia' calcolato e cambio solo la finale
026300020715     c                   if        o50psi<>*blanks
026400020715     c                   movea     o50psi        k10
026500020910     c                   else
026600971107     c                   movel     azi(x)        k10(7)
026700020910     c                   endif
026800971107     c* AMBIENTE PROVA
026900971107    2c     azi(x)        ifeq      'P'
027000020910     c                   movel     azi(x)        k10(7)
027100971104     c                   seton                                        01
027200020322     c* determino se e' ambiente di prova di sede (30off) filiale (30on)
027300971107     c     wprovafil     lookup    fine                                   30
027400020322     c* determino se c'e' libreria di prova pgm:no(30off) SI      (30on)
027500020322     c     propro        lookup    lis                                    31
027600971107    3c     *in30         ifeq      *on
027700020322     c                   movel     'F'           k10(8)
027800971107     c                   else
027900020322     c                   movel     'S'           k10(8)
028000971107    3c                   endif
028100020322    3c     *in31         ifeq      *on
028200020322     c                   movel     'P'           k10(9)
028300020322     c                   else
028400020322     c                   movel     'M'           k10(9)
028500020322    3c                   endif
028600971104     c***
028700971107   x2c                   else
028800971104     c                   seton                                        02
028900971104     c***
029000971104     c*** AMBIENTE BUONO
029100020910     c                   if        o50psi=*blanks
029200971111     c                   movel     fine(x)       w004a             4
029300971107     c                   movea     w004a         k10(7)
029400971107    2C                   ENDIF
029500020910    2C                   ENDIF
029600971107     c
029700971107     c***
029800971107     c                   movea     k10           o50psi
029900971104     c**
030000971107   x1c                   else
030100971104     c** se c'e' sistema informativo imposto i dati nel posto 1
030200971104     c*  della schiera
030300971201     c* controllo se e' minuscolo lo metto in maiuscolo
030400971201     c                   movea     i50psi        k10mi
030500971201     c                   exsr      cambia
030600971201     c**
030700971201     c                   movea     k10ma         o50psi
030800971201     c                   move      o50psi        w004a
030900971106     c                   movel     w004a         azi(1)
031000971107    2c     azi(1)        ifeq      'P'
031100971104     c                   seton                                        01
031200971104     c                   else
031300971104     c                   seton                                        02
031400971107    2c                   endif
031500971107    1c                   endif
031600020715     c     sotto         tag
031700971104     c**
031800971104     c**
031900971104     c** trovato s.i. partenza: determino tutti i dati partenza
032000020730     c                   movel     o50psi        wsi              10
032100020730     c                   movel     o50psi        libiniz           3
032200971104     c                   move      o50psi        libfine           4
032300971111     c                   move      o50psi        wtippro           2
032400020322     c                   move      o50psi        w003a             3
032500971111     c                   movel     w003a         wprovased         1
032600971104     c                   exsr      imposta
032700971104     c***
032800971104     c** imposto i campi della ds
032900971104     c***
033000020715     c                   movel     wuni          o50plu
033100020322     c                   movel     wplg          o50plg
033200971104     c                   movel     wplr          o50plr
033300971104     c                   movel     wpla          o50pla
033400971104     c***
033500971104     c**  SISTEMA INFORMATIVO DI   A R R I V O
033600971104     c***
033700020715     c                   select
033800020715     c* se lo ho già determinato lo imposto di prova come la partenza
033900020715     c*  se è di prova
034000020715     c                   when      o50asi<>*blanks and *in01
034100020715     c                   move      libfine       o50asi
034200020717     c* se c'e' gia' è perchè il p.o. arrivo è di filiale per cui in prova
034300020717     c*  c'e' sempre la "F" alla posizione 8 e non la "S"
034400020717     c                   movea     o50asi        k10
034500020717     c                   movel     'F'           k10(8)
034600020717     c                   movea     k10           o50asi
034700020715     c
034800020715     c* Se il p.o. di arrivo è SEDE --> metto stesso s.i. della partenza
034900020715     c* ma come prova sede, se è di prova
035000020715     c                   when      i50apo=46 and *in01
035100020715     c* lo metto nello stesso S.I. ma di sede
035200020715     c                   if        libiniz<>'FIL'
035300020715     c                   movea     o50psi        k10
035400020715     c                   else
035500020715     c                   movea     barsif        k10
035600020715     c                   movea     libfine       k10(7)
035700020715     c                   endif
035800020715     c                   movel     'S'           k10(8)
035900021012     c                   movea     k10           o50asi
036000020715     c*
036100020715     c* Se il p.o. di arrivo + SEDE --> metto stesso s.i. della partenza
036200020715     c*  se s.I. buono
036300020715     c                   when      i50apo=46 and *in02
036400021012     c                   if        libiniz<>'FIL'
036500021012     c                   movea     o50psi        k10
036600021012     c                   else
036700021012     c                   movea     barsif        k10
036800021012     c                   endif
036900021012     c                   movea     k10           o50asi
037000021012     c******             eval      o50asi=o50psi
037100020715     c
037200020715     c* Se non immesso p.o. ma impostato S.I. --> lo reimposto per loutput
037300020715     c                   when      i50asi<>*blanks and i50apo=0
037400020715     c                   movea     i50asi        k10mi
037500020715     c                   exsr      cambia
037600020715     c                   movea     k10ma         o50asi
037700020715     c                   endsl
037800020715     c
037900971104     c**
038000020715    1c     o50asi        ifne      *blanks
038100020730     c                   movel     o50asi        wsi              10
038200971104     c                   movel     o50asi        libiniz           3
038300971104     c                   move      o50asi        libfine           4
038400971111     c                   move      o50asi        wtippro           2
038500971111     c                   move      o50asi        w003a
038600971111     c                   movel     w003a         wprovased         1
038700020715     c                   exsr      imposta
038800971104     c***
038900971104     c** imposto i campi della ds
039000971104     c***
039100020715     c                   movel     wuni          o50alu
039200020322     c                   movel     wplg          o50alg
039300971104     c                   movel     wplr          o50alr
039400971104     c                   movel     wpla          o50ala
039500020715    1c                   endif
039600020715    0c                   endif
039700971104     c
039800971104     c*
039900971104     c     endjob        tag
040000971104     c     i50tla        ifeq      'L'
040100971104     c                   seton                                        lr
040200971104     c                   else
040300971104     c                   seton                                        rt
040400971104     c                   endif
040500971104     c*********************************************************************
040600971104     c* imposto i campi della ds
040700971104     c*********************************************************************
040800971104     c     imposta       begsr
040900971104     c                   select
041000020322     c     libiniz       wheneq    'SDI'
041100020715     c                   movel     sdiuni        wuni
041200971104     c                   movel     sdigru        wplg
041300971104     c                   movel     sdilra        wplr
041400971104     c                   movel     sdiazi        wpla
041500971104     c     libiniz       wheneq    'GAI'
041600020715     c                   movel     baruni        wuni
041700971104     c                   movel     bargru        wplg
041800971104     c                   movel     barlra        wplr
041900971104     c                   movel     barazi        wpla
042000020715     c     libiniz       wheneq    'FIL'
042100020715     c                   movel     uniuni        wuni
042200020715     c                   movel     unigru        wplg
042300020715     c                   movel     unilra        wplr
042400020715     c                   movel     uniazi        wpla
042500020730     c                   other
042600020730     c* se si tratta di un S.I. non previsto passo lo stesso in tutti i camp
042700020730     c*  delle librerie per non lasciarli vuoti
042800020730     c                   movel     wsi           wuni
042900020730     c                   movel     wsi           wplg
043000020730     c                   movel     wsi           wplr
043100020730     c                   movel     wsi           wpla
043200971104     c                   endsl
043300971104     c** prova
043400010713     c                   IF        *IN01 = *ON
043500020715     c                   move      'AZP '        wplr
043600020322     c** sede
043700020322     c     wprovased     ifeq      'S '                                         sede
043800971111     c                   move      'PRV '        wpla
043900010713     c                   move      'PS'          wplg
044000020715     c                   move      'P '          wuni
044100010713     c                   else
044200020322     c** filiale
044300020322     c                   move      'PRD '        wpla                           filiale
044400010713     c                   move      'PF'          wplg
044500020715     c                   move      'PF'          wuni
044600020715     c                   if        libiniz='FIL'
044700020715     c                   move      'P '          wuni
044800971111     c                   endif
044900020715     c                   endif
045000010713      *
045100010713     c                   ENDIF
045200971104     c**
045300971104     c   02              move      libfine       wpla
045400971104     c                   endsr
045500971201     c*********************************************************************
045600971201     c* controllo se stringa minuscola la mwtto maiuscola
045700971201     c*********************************************************************
045800971201     c     cambia        begsr
045900971201     c                   movea     k10mi         k10ma
046000971201     c                   do        10            z                 3 0
046100971201     c     k10mi(z)      ifne      ' '
046200971201     c                   z-add     1             y                 3 0
046300971201     c     k10mi(z)      lookup    minu(y)                                30
046400971201     c     *in30         ifeq      *on
046500971201     c                   movel     maiu(y)       k10ma(z)
046600971201     c                   endif
046700971201     c                   endif
046800971201     c                   enddo
046900971201     c                   endsr
047000020715     c*********************************************************************
047100020715     c* controllo su azorg dei p.o. e reperimento dati necessari
047200020715     c*********************************************************************
047300020715     c     cHAAZORG      BEGSR
047400020715     c                   clear                   wsif
047500020715     c     wpo           chain     azorg01l                           30
047600020715    2c     *in30         ifeq      *on
047700020715     c     orgfva        orne      *blanks
047800020715     c     orgfag        orne      'A'
047900020715     c     orgfag        andne     'F'
048000020903    3c                   if        orgfil <>46 and orgfil<>946 and
048100020903     c                             wpo<>789
048200020715     c                   movel     '1'           o50err
048300020715    3c                   endif
048400020715    2c                   endif
048500020903     c
048600020715    2c                   if        o50err=' '
048700020903     c* per 789 non esiste organigramma --> imposto dati fittizi
048800020903     c                   if        wpo=789
048900020903     c                   eval      savfel=789
049000020903     c                   eval      savntw='COR'
049100020903     c                   eval      savas=789
049200020903     c                   else
049300020715     c* salvo i campi di comodo che mi servo fuori dalla routine
049400020715     c                   eval      savfel=orgfel
049500020715     c                   movel     orgde3        og143
049600020715     c                   eval      savntw=§ogntw
049700020715     c                   eval      savas=§ogas
049800020903     c                   endif
049900020715
050000020715     c                   clear                   wdit
050100020715
050200020715     c* Determino il S.I. dal campo §OGAS
050300020715    3c                   select
050400020903     c* Se 789 --> test fisso siamo nell'ambiente GAITRA201
050500020903     C                   when      wpo=789
050600020903     c                   eval      wsif=BARSIF
050700020903     c
050800020715     c* Se 946 --> test fisso siamo nell'ambiente as unico di sede
050900020715     C                   when      §ogas=946
051000020715     c                   eval      wsif=UNISIF
051100020715     c
051200020715     c* Se 046 --> prendo il S.I. se è passato o lo determino dalla
051300020715     c*            lista di librerie
051400020715     c                   when      §ogas=46
051500020715     c                   eval      wdit='SED'
051600020715     c
051700020715     c* altrimenti reperisco l'orgdit dall'OGAS per sapere il S.I.
051800020715     c                   other
051900020715     c     §ogas         chain     azorg01l                           30
052000020715    4c     *in30         ifeq      *on
052100020715     c     orgfva        orne      *blanks
052200020715     c     orgfag        orne      'A'
052300020715     c     orgfag        andne     'F'
052400020715     c                   movel     '1'           o50err
052500020715     c                   movel     msg(4)        o50msg
052600020715     c                   else
052700020715     c                   eval      wdit=orgdit
052800020715    4c                   endif
052900020715    3c                   endsl
053000020715    2c                   endif
053100020715     c                   ENDSR
053200971104**
053300971104Punto operativo di arrivo inesistente o non utilizzabile
053400971104Punto operativo di partenza inesistente o non utilizzabile
053500020322Non trovata nessuna libreria dai PAGHE SDI BAR   per determinare il S.I.
053600020715Campo §OGAS su organigramma inesistente o non utilizzabile
053700971201**
053800971201abcdefghijklmnopqrstuvwxyz
053900971201**
054000971201ABCDEFGHIJKLMNOPQRSTUVWXYZ
