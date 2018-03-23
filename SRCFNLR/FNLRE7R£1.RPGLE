000100000000     H DECEDIT('0,') DATEDIT(*YMD.)
000200000000      **************************************************************************
000300941228      *  Nome programma:  FNLRE7R
000400000000      *  Descrizione   :  Stampa statistica arrivi.
000500000000      *                   Secondo programma di esecuzione: stampa dal file
000600090929      *                   statistiche FISAR00F; viene richiamato dal CLP
000700941228      *                   FNLRE7C che esegue degli OPNQRYF selezionando per
000800000000      *                   data.
000900000000      *  Data creazione:  04 FEB 1994
001000000000      **************************************************************************
001100950214      *  ??PASSAGGIO PARAMETRI:?
001200950214      *  DS1TBO   'A'=ARRIVI, 'P'=PARTENZE, 'T'=TRANSITI
001300950214      *  DS1FGS   FILIALE RICHIEDENTE  ( 0=ELABORAZ.GIORNALIERE )
001400950214      *  DS1PGM   NOME PROGRAMMA
001500950214      *  DS1FPG   FLAG PROGRAMMA
001600950214      *  DS1FEL   FILIALE ELABORATORE (AREA CHE SI VUOLE STAMPARE)
001700950313      *  DS1LNP   FILIALE DI CUI SI VUOLE LA STAMPA (E' LA LNA)
001800950214      *  DS1GDA   DATA INIZIALE (GG/MM/AAAA)
001900950214      *  DS1GAL   DATA FINALE (GG/MM/AAAA)
002000950214      *  DS1ADA   DATA INIZIALE (AAAA/MM/GG)
002100950214      *  DS1AAL   DATA FINALE (AAAA/MM/GG)
002200950404      *  DS1RIS   'R'=RISTAMPA, 'S'=ELABOR.+STAMPA, ' '=SOLO ELABOR.
002300950214      **************************************************************************
002400000000      *  RIEPILOGO UTILIZZO INDICATORI:
002500090929      *  10        Stampo anche le spediizoni H dalla tabella vpodeco
002600090929      *  20   ON   Provengo da stampe giornaliere (DS1FGS=0)
002700000000      *  21   ON   Stampa per terminal di arrivo
002800000000      *  21   OFF  Stampa per linea di arrivo
002900000000      *  30        Overflow del printer file
003000000000      *  31   ON   Elaborazione fase 1
003100000000      *  31   OFF  Elaborazione fase 2
003200000000      *  34   ON   Stampa il TERMINAL ARRIVO in testa alla pagina
003300000000      *  41        Utilizzato in stampa
003400000000      *  42        Utilizzato in stampa
003500000000      *  43   ON   Stampa i totali e salta a nuova pagina
003600000000      *  44        Utilizzato in stampa
003700980305      *  98        Generico, utilizzabile a piacere
003800000000      *  99        Generico, utilizzabile a piacere
003900000000      **************************************************************************
004000000000     FAZORG01L  IF   E           K DISK
004100090929     FFISAR01L  UF   E           K DISK
004200090929     FFISAR02L  IF   E           K DISK
004300090929     F                                     RENAME(FISAR000:FISAR002)
004400090929     FFISAR04L  IF   E           K DISK
004500090929     F                                     RENAME(FISAR000:FISAR004)
004600000000     FTABEL00F  IF   E           K DISK
004700941228     FFNLRE7P   O    E             PRINTER OFLIND(*IN30)
004800941228     F                                     USROPN
004900000000      *
005000000000      * Le schiere che seguono hanno tre elementi:
005100000000      *         1)  totale transiti
005200000000      *         2)  conto servizi (da considerare come filiale a parte)
005300000000      *         3)  totale generale (transiti esclusi)
005400000619      *         4)  totale P.O. POSTE
005500000619     D CLL             S              7  0 DIM(4)                               COLLI
005600000619     D P88             S              7  0 DIM(4)                               COLLI PISTOLA 88
005700000619     D DIS             S              7  0 DIM(4)                               COLLI DISGUIDI
005800000619     D PAR             S              7  0 DIM(4)                               C.PART.NON SPUNT.
005900000619     D PTA             S              9  1 DIM(4)                               PESO TASSABILE
006000000619     D F35             S              9  1 DIM(4)                               PESO FINO 35 Q.
006100000619     D O35             S              9  1 DIM(4)                               PESO OLTRE 35 Q.
006200000619     D VOL             S              9  4 DIM(4)                               VOLUME
006300090929     D SNF             S              7  0 DIM(4)                               SPE.C+D     FRAN.
006400090929     D SNA             S              7  0 DIM(4)                               SPE.C+D     ASS.
006500090929     D SEF             S              7  0 DIM(4)                               SPE. "E"     FRAN
006600090929     D SEA             S              7  0 DIM(4)                               SPE. "E"     ASS.
006700090929     D ShF             S              7  0 DIM(4)                               SPE. "H"     FRAN
006800090929     D ShA             S              7  0 DIM(4)                               SPE.  "H"    ASS.
006900000000      * Linee di arrivo di cui solo terminal
007000000000     D LNAR            S              3  0 DIM(100)                             LINEE ARRIVO
007100000000     D CLLR            S              7  0 DIM(100)                             COLLI
007200000000     D P88R            S              7  0 DIM(100)                             COLLI PISTOLA 88
007300000000     D PARR            S              7  0 DIM(100)                             C.PART.NON SPUNT.
007400000000      *
007500000000     D L1              S              3  0 DIM(30)                              FILIALI GESTITE
007600040206     D L9              S              3  0 DIM(30)                              FILIALI GESTITE
007700040216     D L9L             S              3  0 DIM(30)                              FILIALI GESTITE
007800950214     D L6              S              3  0 DIM(30)                              FILIALI £6
007900040206     D L69             S              3  0 DIM(30)                              FILIALI £6
008000950214     D LV2             S              3  0 DIM(30)                              FILIALI 2° LVL
008100950214     D LL6             S              3  0 DIM(100)                             FIL GEST IN AR
008200000000     D FIL             S              3    DIM(29)                              FIL. COMODO
008300950214     D LIN             S              3  0 DIM(30)                              FIL. DA STAMPARE
008400040216     D LIL             S              3  0 DIM(30)                              FIL. DA STAMPARE
008500950214     D LI2             S              3  0 DIM(120)                             FIL. DA STAMPARE
008600000000     D ELA             S              3  0 DIM(100)                             FIL.CON ELABORAT.
008700950214     D CMD             S              1    DIM(82) CTDATA PERRCD(80)            COMANDO QCMDEXC
008800000000     D CMD1            S             80    DIM(1) CTDATA PERRCD(1)              COMANDO QCMDEXC
008900000620     D DIC             S             30    DIM(8) CTDATA PERRCD(1)              DICITURE STAMPA
009000020419
009100020419     d sav_t06         S              3  0 dim(30)
009200040206     d sav_t09         S              3  0 dim(30)
009300020419
009400020419     d wc              s              3  0
009500020419
009600000000      *
009700000000     D KPJBA         E DS
009800000704     D DSQT1         E DS
009900000000     D DS3I          E DS
010000000000     D DS3L          E DS
010100970707     D DS5P          E DS
010200090604     d ds5e          e ds
010300000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010400000000     D CNCR80        E DS
010500090929     D DSLS01        E DS
010600971126     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
010700971126     D DSLV55        E DS                  EXTNAME(FNLV55DS)
010800950214     D* DS PER TRUL06R - CARICAMENTO £X
010900950214     D DSUL06        E DS                  EXTNAME(TRUL06DS)
011000950314     D  T06                    1     90  0
011100950314     D                                     DIM(30)                              TRUL06R
011200040205      * Parametri per richiamo a TRUL09R
011300040205     d Trul09ds      e ds
011400040205     d  T09                   19    108  0 dim(30)
011500000608      * DS ESTERNA PER CONTROLLO ELABORAZIONE POSTE
011600000608     D FNLSC4        E DS                  EXTNAME(FNLSC4DS)
011700090929     d
011800090929     D TIBS02        E DS                  EXTNAME(TIBS02DS)
011900090929     d dvpodeco      e ds
012000000000      *
012100000000     C     *ENTRY        PLIST
012200000000     C                   PARM                    KPJBA
012300950214     C                   MOVEL     KPJBU         DSLS01
012400000000      *
012500090929     C     KFISAR        KLIST                                                  FISAR01L
012600040206     C***                KFLD                    ELA(KK)
012700040206     C                   KFLD                    simfel
012800950214     C                   KFLD                    LL6(XX)
012900090929     C***  KCAE2         KLIST                                                  FISAR01L
013000040206     C***                KFLD                    KEPA
013100040206     C***                KFLD                    DS1FGS
013200040206     C***                MOVEL     'A'           KEPA
013300000000      *
013400000000     C     *LIKE         DEFINE    SARLNA        $FILPR                         PRECEDENTE
013500000000     C     *LIKE         DEFINE    SARLNA        $$FILP                         PRECEDENTE
013600000000     C     *LIKE         DEFINE    SARTRE        $TREPR                         PRECEDENTE
013700000000     C     *LIKE         DEFINE    SARLNA        $FILIA
013800040206     C***  *LIKE         DEFINE    CAEEPA        KEPA
013900000000      **************************************************************************
014000000000      *               M A I N      L I N E
014100000000      **************************************************************************
014200000000      *
014300000000     C                   Z-ADD     1             CODUT
014400000000      *
014500000000      * PRENDO TABELLA VARIABILI PGM
014600950214     C                   MOVE      '3I'          KCOD
014700950214     C                   MOVEL     '1       '    KKEY
014800950214     C     KTAB1         CHAIN     TABEL00F                           99
014900000000     C  N99              MOVEL     TBLUNI        DS3I
015000000000     C   99              Z-ADD     20            §3IARR
015100000704      *
015200000704      * AGGANCIO LA TABELLA QT PER ALCUNI DATI AZIENDALI
015300000704     C                   MOVE      'QT'          KCOD
015400000704     C                   MOVEL     '1       '    KKEY
015500000704     C     KTAB1         CHAIN     TABEL00F                           99
015600000704     C  N99              MOVEL     TBLUNI        DSQT1
015700000704     C   99              CLEAR                   DSQT1
015800090604
015900090604      * carico le descrizioni fisse dei tipi servizio
016000090604      * chiodo la descrizione per ogni tipo servizio visto che la descrizione
016100090604      * è da stampare nella testata ed è fissa
016200090604      * le colonne dei tipi servizio da stampare sono
016300090604      * 'C + D' 'E'
016400090604     c                   clear                   ds5e
016500090604     c                   clear                   wdtspc
016600090604     c                   clear                   wdtspd
016700090604     c                   clear                   wdtspe
016800090604     c                   eval      kcod = '5E'
016900090604     c     ktab2         setll     tabel00f
017000090604     c                   do        *hival
017100090604     c     ktab2         reade     tabel00f
017200090604     c                   if        %eof(tabel00f)
017300090604     c                   leave
017400090604     c                   endif
017500090604     c                   eval      ds5e = tbluni
017600090604     c                   if        tblkey = 'C'
017700090604     c                   eval      wdtspc = §5ed05
017800090604     c                   endif
017900090604     c                   if        tblkey = 'D'
018000090604     c                   eval      wdtspd = §5ed05
018100090604     c                   endif
018200090604     c                   if        tblkey = 'E'
018300090604     c                   eval      wdtspe = §5ed08
018400090604     c                   endif
018500090929     c                   if        tblkey = 'H'
018600090929     c                   eval      wdtsph = §5ed08
018700090929     c                   endif
018800090604     c                   enddo
018900000000      *
019000950313      * Se e' scelto 'T'=Terminal devo usare DS1LNP come terminal
019100000000     C                   Z-ADD     *ZERO         £TFA                           TERMINAL ARRIVO
019200950216     C     DS1FL1        IFEQ      'T'                                          'T'=TERMINAL
019300950313     C     DS1LNP        ANDNE     *ZERO
019400950313     C                   Z-ADD     DS1LNP        £TFA              3 0          TERMINAL ARRIVO
019500000000     C                   ENDIF
019600090929
019700090929      * Carico la VPODECO
019800090929     c                   clear                   tibs02
019900090929     c                   clear                   dvpodeco
020000090929     c                   eval      t02mod = 'C'
020100090929     c                   eval      t02sif = knsif
020200090929     c                   eval      t02cod = 'VPO'
020300090929     c                   eval      t02ke1 = 'DECO'
020400090929     c                   call      'TIBS02R'
020500090929     c                   parm                    kpjba
020600090929     c                   parm                    tibs02
020700090929     c                   if        t02err = *blanks
020800090929     c                   eval      dvpodeco = t02uni
020900090929     c                   else
021000090929     c                   eval      §vpotsph=20091001
021100090929     c                   endif
021200090929     c
021300090929      *
021400000000      *
021500950214      * CHIAMO LA ROUTINE PER LE IMPOSTAZIONI DI ELABORAZIONE
021600950214     C                   EXSR      COMUNE                                       PARTE COMUNE
021700971126     C* CHIUSURA FILE
021800971126     C                   CLEAR                   DSLV55
021900971126     C                   MOVEL     'C'           D55TLA
022000971126     C                   CALL      'FNLV55R'
022100971126     C                   PARM                    DSLV55
022200000000      *
022300000000     C                   SETON                                        LR
022400990507     C   99
022500090929     CANN99              UPDATE    FISAR000
022600090929      * Ho dovuto usare FISAR di I/O per poter fare il clear del formato
022700090929      * FISAR000
022800000000      *
022900020212     C*****************************************************************
023000020212     C*  PARTE COMUNE STAMPE GIORNALIERE
023100020212     C*
023200020212     C*  . REPERIMENTO 'SIMFEL'
023300020212     C*  . CARICAMENTO '£1'
023400020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
023500020212     C*  . CODA DI STAMPA ('£OUTQ')
023600020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
023700020212     C*  . MODULO PER LA STAMPA ('£MOD')
023800020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
023900020212     C*****************************************************************
024000020212     C     COMUNE        BEGSR
024100020212     C*
024200020212     C*****************************************************************
024300020212     C*       RIEPILOGO INDICATORI:
024400020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
024500020212     C* 11    -           STAMPA PER 1L
024600020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
024700020212     C*****************************************************************
024800020212     C                   Z-ADD     1             CODUT
024900020212     C*
025000020212     C* Reperimento parametri aziendali
025100020212     C                   CALL      'X§PARUT'
025200020212     C                   PARM                    UT§DSE
025300020212     C                   MOVEL     RAGUT         RSUT             20
025400020212     C                   MOVEL     REC80         CNCR80
025500020212     C*
025600020212     C* Chiave di accesso per il file tabelle
025700020212     C     KTAB1         KLIST                                                  TABEL00F
025800020212     C                   KFLD                    CODUT
025900020212     C                   KFLD                    KCOD
026000020212     C                   KFLD                    KKEY
026100020212     C     KTAB2         KLIST                                                  TABEL00F
026200020212     C                   KFLD                    CODUT
026300020212     C                   KFLD                    KCOD
026400020212     C*
026500020212     C     *LIKE         DEFINE    TBLCOD        KCOD                           COD.TABELLA
026600020212     C     *LIKE         DEFINE    TBLKEY        KKEY                           ELEMENTO
026700020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
026800020212     C*
026900020212     C* DETERMINO DATA E ORA DI ESECUZIONE
027000020212     C                   TIME                    WTIME            14 0          ORA & DATA
027100020212     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
027200020212     C                   MOVEL     WTIME         UTIME             6 0          HH:MM:SS
027300090929
027400090929     c*  stampare anche le spedizioni H
027500090929     c                   if        ds1ada>=§vpotsph
027600090929     c                   seton                                        10
027700090929     c                   endif
027800090929     C*
027900020212     C*
028000020419     C* CARICO '£1'
028100020212     C                   CLEAR                   KPJBU
028200020212     C                   CLEAR                   DSUL06
028300020212     C                   MOVE      '£1'          D06COD
028400020212     C                   MOVEL     SIMFEL        D06KEY
028500050802     C                   MOVE      'S'           D06KEY
028600050802     C                   MOVEL     ds1ada        D06drf
028700020212     C                   MOVEL     DSUL06        KPJBU
028800020212     C                   CALL      'TRUL06R'
028900020212     C                   PARM                    KPJBA
029000020212     C                   MOVEL     KPJBU         DSUL06
029100020419     c                   movea     t06           sav_t06
029200020419
029300020419      * se in arrivo carico la £6
029400020419 b1  c                   if        ds1tbo = 'A'
029500020419     c                   clear                   wc
029600020419 b2  c                   do        30            wa
029700020422     c                   if        sav_t06(wa) = *zeros
029800020422     c                   leave
029900020422     c                   endif
030000020419     c                   clear                   kpjbu
030100020419     c                   clear                   dsul06
030200020419     c                   eval      d06cod = '£6'
030300020419     c                   movel     sav_t06(wa)   d06key
030400050802     C                   MOVE      'S'           D06KEY
030500020419     c                   eval      d06esc = 'S'
030600050802     C                   MOVEL     ds1ada        D06drf
030700020419     c                   movel     dsul06        kpjbu
030800020419     c                   call      'TRUL06R'
030900020419     c                   parm                    kpjba
031000020419     c                   movel     kpjbu         dsul06
031100020419 b3  c                   do        30            wb
031200020419     c                   if        t06(wb) = *zeros
031300020419     c                   leave
031400020419     c                   endif
031500020419     c                   add       1             wc
031600020419     c                   z-add     t06(wb)       l6(wc)
031700020419 e3  c                   enddo
031800020419 e2  c                   enddo
031900020212     C*
032000020419     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
032100020212     C                   Z-ADD     *ZERO         WB
032200020419 b2  C                   DO        29            WA
032300020419     C     sav_T06(WA)   LOOKUP    L6                                     30     ON=TROVATO
032400020212     C  N30              ADD       1             WB
032500020419     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
032600020419 e2  C                   ENDDO
032700020419 x1  C                   ELSE
032800020419     C                   MOVEA     sav_T06       L1
032900020419 e1  C                   ENDIF
033000020212     C*
033100020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
033200020212     C                   MOVEA     L1            LIN
033300020212     C                   CLEAR                   L1
033400020212     C                   Z-ADD     *ZERO         WB                3 0          DI COMODO
033500020212     C     1             DO        30            WPT               3 0          DI COMODO PER POSTE
033600020212     C     LIN(WPT)      IFNE      0
033700020212     C                   CLEAR                   FNLSC4
033800020212     C                   Z-ADD     LIN(WPT)      IC4LIN                         LINEA
033900020212     C                   MOVE      'B'           IC4FLG                         B=BATCH
034000020212     C                   MOVEL     FNLSC4        KPJBU
034100020212     C                   CALL      'FNLSC4R1'
034200020212     C                   PARM                    KPJBA
034300050802     C                   PARM                    ds1ada
034400020212     C                   MOVEL     KPJBU         FNLSC4
034500020212     C     OC4ESE        IFEQ      'S'
034600020212     C                   ADD       1             WB
034700020212     C                   MOVE      LIN(WPT)      L1(WB)
034800020212     C                   ENDIF
034900020212     C                   ENDIF
035000020212     C                   ENDDO
035100070103     C*
035200070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
035300070103     C                   Z-ADD     *ZERO         WB                3 0          DI COMODO
035400070103     C                   DO        29            WA                3 0
035500070103     C     L1(WA)        IFNE      *ZEROS
035600070103     C     L1(WA)        ANDNE     SIMFEL
035700070103     C                   ADD       1             WB
035800070103     C                   Z-ADD     L1(WA)        LV2(WB)
035900070103     C                   ENDIF
036000070103     C                   ENDDO
036100040205     c*
036200040205     c* Carico schiera delle linee da elaborare in base al trul09r
036300040205     c                   clear                   trul09ds
036400040205     c                   eval      i09mod = 'TFU'
036500040205     c     ds1fgs        ifeq      0
036600040205     c                   eval      i09tfa = simfel
036700040205     c                   else
036800040205     c                   eval      i09tfa = ds1fgs
036900040205     c                   endif
037000040205     c                   eval      i09dta = ds1ada
037100040205     c                   call      'TRUL09R'
037200040205     c                   parm                    trul09ds
037300040206     c                   movea     t09           sav_t09
037400040206      * se in arrivo carico la £69
037500040206 b1  c                   if        ds1tbo = 'A'
037600040206     c                   clear                   wc
037700040206 b2  c                   do        30            wa
037800040206     c                   if        sav_t09(wa) = *zeros
037900040206     c                   leave
038000040206     c                   endif
038100040206     c                   clear                   kpjbu
038200040206     c                   clear                   dsul06
038300040206     c                   eval      d06cod = '£6'
038400040206     c                   movel     sav_t09(wa)   d06key
038500050802     C                   MOVE      'S'           D06KEY
038600050802     C                   MOVEL     ds1ada        D06drf
038700040206     c                   eval      d06esc = 'S'
038800040206     c                   movel     dsul06        kpjbu
038900040206     c                   call      'TRUL06R'
039000040206     c                   parm                    kpjba
039100040206     c                   movel     kpjbu         dsul06
039200040206 b3  c                   do        30            wb
039300040206     c                   if        t06(wb) = *zeros
039400040206     c                   leave
039500040206     c                   endif
039600040206     c                   add       1             wc
039700040206     c                   z-add     t06(wb)       l69(wc)
039800040206 e3  c                   enddo
039900040206 e2  c                   enddo
040000040206     C*
040100040206     C*  ESCLUDO DA t09 LE FILIALI DELLA £69
040200040206     C                   Z-ADD     *ZERO         WB
040300040206 b2  C                   DO        29            WA
040400040206     C     sav_T09(WA)   LOOKUP    L69                                    30     ON=TROVATO
040500040206     C  N30              ADD       1             WB
040600040206     C  N30              Z-ADD     sav_T09(WA)   L9(WB)
040700040206 e2  C                   ENDDO
040800040206 x1  C                   ELSE
040900040206     C                   MOVEA     sav_T09       L9
041000040206 e1  C                   ENDIF
041100040206     C* ESCLUDO DA £9 LINEE NON AUTORIZZATE PER POSTE
041200040206     C                   MOVEA     L9            LIN
041300040206     C                   CLEAR                   L9
041400040206     C                   Z-ADD     *ZERO         WB                3 0          DI COMODO
041500040206     C     1             DO        30            WPT               3 0          DI COMODO PER POSTE
041600040206     C     LIN(WPT)      IFNE      0
041700040206     C                   CLEAR                   FNLSC4
041800040206     C                   Z-ADD     LIN(WPT)      IC4LIN                         LINEA
041900040206     C                   MOVE      'B'           IC4FLG                         B=BATCH
042000040206     C                   MOVEL     FNLSC4        KPJBU
042100040206     C                   CALL      'FNLSC4R1'
042200040206     C                   PARM                    KPJBA
042300050802     C                   PARM                    ds1ada
042400040206     C                   MOVEL     KPJBU         FNLSC4
042500040206     C     OC4ESE        IFEQ      'S'
042600040206     C                   ADD       1             WB
042700040206     C                   MOVE      LIN(WPT)      L9(WB)
042800040206     C                   ENDIF
042900040206     C                   ENDIF
043000040206     C                   ENDDO
043100040216     c* per l'elaborazione per linea la schiera l9 non va bene devo
043200040216     c* caricarne un'altra richiamando lsc4r1 con flg="N" altrimenti
043300040216     c* per es. se sono bologna nella l9 non 18, 19, 20... e mi va bene
043400040216     c* nell'elaborazione per terminal ma nell'elaborazione per linea le
043500040216     c* devo avere
043600040216     C                   CLEAR                   L9L
043700040216     C                   Z-ADD     *ZERO         WB                3 0          DI COMODO
043800040216     C     1             DO        30            WPT               3 0          DI COMODO PER POSTE
043900040216     C     LIN(WPT)      IFNE      0
044000040216     C                   CLEAR                   FNLSC4
044100040216     C                   Z-ADD     LIN(WPT)      IC4LIN                         LINEA
044200040216     C                   MOVE      'N'           IC4FLG                         B=BATCH
044300040216     C                   MOVEL     FNLSC4        KPJBU
044400040216     C                   CALL      'FNLSC4R1'
044500040216     C                   PARM                    KPJBA
044600050802     C                   PARM                    ds1ada
044700040216     C                   MOVEL     KPJBU         FNLSC4
044800040216     C     OC4ESE        IFEQ      'S'
044900040216     C                   ADD       1             WB
045000040216     C                   MOVE      LIN(WPT)      L9L(WB)
045100040216     C                   ENDIF
045200040216     C                   ENDIF
045300040216     C                   ENDDO
045400020212     C*
045500020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
045600020212     C*
045700020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
045800020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
045900020212     C* la stampa e' lanciata da menu
046000020212     C                   MOVEL     '3L'          KCOD                           COD.TABELLA
046100020212     C                   MOVEL     DS1PGM        KKEY
046200020212     C                   MOVE      DS1FPG        KKEY                           COD.ELEMENTO
046300020212     C     KTAB1         CHAIN     TABEL00F                           30
046400020212     C  N30              MOVEL     TBLUNI        DS3L
046500020212     C   30              MOVEL     'F'           §3LD1L
046600020212     C   30              MOVEL     ' '           §3LD2L
046700020212     C   30              MOVEL     '*STD    '    §3LDST
046800020212     C*
046900020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
047000020212     C                   MOVEL     §3LDST        £MOD             10            TIPO MODULO
047100020212     C                   CLEAR                   LIN
047200040216     C                   CLEAR                   LIL
047300020212     C*
047400020212     C* Verifico chi ha lanciato la stampa
047500020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
047600020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
047700020212     C* DS1FGS<>0  --->  si proviene da menu
047800020212     C     DS1FGS        IFEQ      0                                            ELAB.GIORNALIER
047900020212     C*
048000020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
048100020212     C* lancio, voglio la stampa;  invece
048200020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
048300020212     C*
048400020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
048500020212     C* creare stampa per 1°lvl. (se stessa)
048600020212     C     DS1LNP        IFEQ      SIMFEL
048700020212     C*
048800020212     C* Il flag §3LD1L ha i seguenti significati:
048900020212     C*    ' ' = tutti i dati
049000020212     C*    '1' = solo filiale di 1' livello
049100020212     C     §3LD1L        IFEQ      '1'                                          '1'=SOLO 1' LIV
049200020212     C                   SETON                                        11
049300020212     C                   END
049400020212     C*
049500020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
049600020212     C* nella stampa
049700020212     C*
049800020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
049900040206     C  N11              MOVEA     L9            LIN
050000040216     C  N11              MOVEA     L9L           LIL
050100020212     C   11              MOVEA     SIMFEL        LIN(1)
050200040220     C   11              MOVEA     SIMFEL        LIL(1)
050300020212     C*
050400020212     C* IMPOSTO LA STAMPANTE:
050500020212     C* STAMPANTE TABULATI
050600020212     C                   MOVEL     '5P'          KCOD
050700020212     C                   MOVEL     'T'           W004A             4
050800020212     C                   MOVE      LIN(1)        W004A
050900020212     C                   MOVEL(P)  W004A         KKEY
051000020212     C     KTAB1         CHAIN     TABEL00F                           99
051100020212     C     *IN99         IFEQ      *OFF
051200020212     C                   MOVEL     TBLUNI        DS5P
051300020212     C                   MOVEL     §5POTQ        £OUTQ            10            CODA DI STAMPA
051400020212     C                   END
051500020212     C*
051600020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
051700020212     C                   EXSR      LEGGI
051800020212     C*
051900020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
052000020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
052100020212     C                   ELSE
052200020212     C*
052300020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
052400020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
052500020212     C*
052600020212     C* Il significato del fla §3LD2L e' il seguente:
052700020212     C*    ' ' = tutti i dati
052800020212     C*    'F' = per ogni 2' livello
052900020212     C     §3LD2L        IFEQ      'F'
053000020212     C                   SETON                                        14
053100020212     C                   ENDIF
053200020212     C*
053300020212     C                   Z-ADD     1             WB
053400020212     C                   MOVEA     *ZEROS        LIN
053500040206     C  N14              MOVEA     L9            LIN
053600040216     C  N14              MOVEA     L9L           LIL
053700020212     C*
053800020212     C* Lancio per tutti i 2° lvl
053900020212     C     DS1LNP        IFEQ      0
054000020212     C*
054100020212     C* LV2 contiene tutte le filiali di 2° lvl
054200020212     C     LV2(WB)       DOWNE     0
054300020212      *
054400020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
054500020212     C                   CLEAR                   FNLSC4
054600020212     C                   Z-ADD     LV2(WB)       IC4LIN                         LINEA
054700020212     C                   MOVEL     FNLSC4        KPJBU
054800020212     C                   CALL      'FNLSC4R1'
054900020212     C                   PARM                    KPJBA
055000050802     C                   PARM                    ds1ada
055100020212     C                   MOVEL     KPJBU         FNLSC4
055200020212      *
055300020212     C     OC4PT1        IFEQ      'S'
055400020212     C     §3LPT         ANDEQ     'N'
055500020212     C                   GOTO      NOLEGG
055600020212     C                   ENDIF
055700020212      *
055800020212     C     OC4EXP        IFEQ      'S'
055900020212     C     §3LEXP        ANDEQ     'N'
056000020212     C                   GOTO      NOLEGG
056100020212     C                   ENDIF
056200020212      *
056300020212     C     OC4DPD        IFEQ      'S'
056400020212     C     §3LDPD        ANDEQ     'N'
056500020212     C                   GOTO      NOLEGG
056600020212     C                   ENDIF
056700020212      *
056800020212     C     OC4BAR        IFEQ      'S'
056900020212     C     §3LBAR        ANDEQ     'N'
057000020212     C                   GOTO      NOLEGG
057100020212     C                   ENDIF
057200020212      *
057300020212     C     OC4FED        IFEQ      'S'
057400020212     C     §3LFED        ANDEQ     'N'
057500020212     C                   GOTO      NOLEGG
057600020212     C                   ENDIF
057700020212      *
057800020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
057900020212     C*  dati degli altri 2° lvl
058000020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
058100020212     C   14              MOVEL     LV2(WB)       LIN(1)
058200040220     C   14              MOVEL     LV2(WB)       LIL(1)
058300020212     C*
058400020212     C* IMPOSTO LA STAMPANTE
058500020212     C* STAMPANTE TABULATI
058600020212     C                   MOVEL     '5P'          KCOD
058700020212     C                   MOVEL     'T'           W004A             4
058800020212     C                   MOVE      LV2(WB)       W004A
058900020212     C                   MOVEL(P)  W004A         KKEY
059000020212     C     KTAB1         CHAIN     TABEL00F                           99
059100020212     C     *IN99         IFEQ      *OFF
059200020212     C                   MOVEL     TBLUNI        DS5P
059300020212     C                   MOVEL     §5POTQ        £OUTQ            10            CODA DI STAMPA
059400020212     C                   END
059500020212     C*
059600020212     C* Compongo il modulo di stampa con il codice della filiale
059700020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
059800020212     C*
059900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
060000020212     C                   EXSR      LEGGI
060100020212     C*
060200020212     C     NOLEGG        TAG
060300020212     C*
060400020212     C                   ADD       1             WB
060500020212     C                   ENDDO
060600020212     C*
060700020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
060800020212     C                   ELSE
060900020212     C*
061000020212     C* Lancio solo per la 2° lvl richiesta
061100020212     C   14              MOVEL     DS1LNP        LIN(1)
061200040220     C   14              MOVEL     DS1LNP        LIL(1)
061300020212     C*
061400020212     C* STAMPANTE
061500020212     C* STAMPANTE TABULATI
061600020212     C                   MOVEL     '5P'          KCOD
061700020212     C                   MOVEL     'T'           W004A             4
061800020212     C                   MOVE      DS1LNP        W004A
061900020212     C                   MOVEL(P)  W004A         KKEY
062000020212     C     KTAB1         CHAIN     TABEL00F                           99
062100020212     C     *IN99         IFEQ      *OFF
062200020212     C                   MOVEL     TBLUNI        DS5P
062300020212     C                   MOVEL     §5POTQ        £OUTQ            10            CODA DI STAMPA
062400020212     C                   END
062500020212     C*
062600020212     C* MODULO STAMPA
062700020212     C******               MOVE DS1LNP    £MOD
062800020212     C*
062900020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
063000020212     C                   EXSR      LEGGI
063100020212     C*
063200020212     C                   ENDIF
063300020212     C                   ENDIF
063400020212     C*
063500020212     C                   ELSE
063600020212     C*
063700020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
063800020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
063900020212     C     DS1LNP        IFEQ      0
064000040206     C                   MOVEA     L9            LIN
064100040216     C                   MOVEA     L9L           LIL
064200020212     C* ...altrimenti elaboro solo se stessa
064300020212     C                   ELSE
064400020212     C                   MOVEL     DS1LNP        LIN(1)
064500040220     C                   MOVEL     DS1LNP        LIL(1)
064600020212     C                   ENDIF
064700020212     C*
064800020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
064900020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
065000020212     C* PRENDO QUELLA PRESENTE IN KPJBA
065100020212     C     DS1FGS        IFEQ      SIMFEL
065200020212     C                   MOVEL     KQOTD         £OUTQ                          CODA DI STAMPA
065300030627     C**!!!              MOVEL     KQOTD         £OUTQF                         CODA DI STAMPA
065400020212     C                   ELSE
065500020212     C* STAMPANTE TABULATI
065600020212     C                   MOVEL     '5P'          KCOD
065700020212     C                   MOVEL     'T'           W004A             4
065800020212     C                   MOVE      DS1FGS        W004A
065900020212     C                   MOVEL(P)  W004A         KKEY
066000020212     C     KTAB1         CHAIN     TABEL00F                           99
066100020212     C     *IN99         IFEQ      *OFF
066200020212     C                   MOVEL     TBLUNI        DS5P
066300020212     C                   MOVEL     §5POTQ        £OUTQ            10            CODA DI STAMPA
066400020212     C                   END
066500020212     C******               MOVE DS1FGS    £MOD             TIPO MODULO
066600020212     C                   ENDIF
066700020212     C*
066800020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
066900020212     C                   EXSR      LEGGI
067000020212     C                   ENDIF
067100020212     C*
067200020212     C                   ENDSR
067300000000      **************************************************************************
067400000000      * Routine primaria; viene letto l'archivio statistica e stampato
067500000000      * LEGGI viene richiamata diverse volte dalla procedure contenuta in
067600950214      * FNLSC4R ( /COPY )
067700000000      *
067800000000      * Breve prospetto riassuntivo
067900000000      *           IN SEDE         FILIALE FILTRO          FILIALE STAMPE GIOR.
068000000000      * LIN()        0                 piena                      piena
068100950214      * DS1FGS       0                 SIMFEL                       0
068200950214      * DS1FEL       ?                 SIMFEL                     SIMFEL
068300000000      **************************************************************************
068400000000     C     LEGGI         BEGSR
068500000000      *
068600980305     C* SE STAMPA PER TUTTI I TERMINAL DI ARRIVO VERIFICO SE DEVO
068700980305     C* AGGIUNGERE DEI TER-ARR SU ALTRO AS
068800040206     C***  £TFA          IFEQ      *ZEROS
068900040206     C**   DS1FGS        ANDGT     *ZEROS
069000040206     C***  DS1FL1        ANDEQ     'T'                                          'T'=TERMINAL
069100040206     C***  KCAE2         SETLL     AZCAE02L
069200040206     C***  KCAE2         READE     AZCAE02L                               99
069300040206     C***  *IN99         DOWEQ     *OFF
069400040206     C***  CAEATB        IFEQ      *BLANKS
069500040206     C***  DS1ADA        ANDGE     CAEDDE
069600040206     C***  DS1ADA        ANDLE     CAEDSC
069700040206     C***                Z-ADD     1             WA
069800040206     C***  CAETFE        LOOKUP    LIN(WA)                                99
069900040206     C**N99000           LOOKUP    LIN(WA)                                98
070000040206     C***98              Z-ADD     CAETFE        LIN(WA)
070100040206     C***                END
070200040206     C***  KCAE2         READE     AZCAE02L                               99
070300040206     C***                ENDDO
070400040206     C***                END
070500980305     C*
070600950214      * Se sono in sede (e provengo quindi per forza da filtro) riempo DS1FGS
070700040206     C***  SIMFEL        IFEQ      *ZERO
070800040206     C***                Z-ADD     999           DS1FGS
070900040206     C***                ENDIF
071000000000      *
071100950214      * DS1FGS = 0 VENGO DA STAMPE GIORNALIERE
071200950214     C     DS1FGS        COMP      *ZERO                                  20     ON=GIORNALIERE
071300940516      *
071400940527      * PARTICOLARITA':  SE SIMFEl e' un secondo livello in arrivo
071500971126      *                  (terminal arrivo >< terminal di part)
071600950214      *                  e siamo nelle stampe giornaliere (DS1FGS=0),
071700940516      * eseguo la stampa per 'L'=Linea invece che per 'T'=Terminal
071800950214     C     DS1FGS        IFEQ      *ZERO                                        STAMPE GIORNALIERE
071900971126     C                   CLEAR                   DSLV55
072000971126     C                   MOVEL     'A'           D55TPT
072100971126     C                   Z-ADD     SIMFEL        D55LIN
072200971126     C                   Z-ADD     DS1ADA        D55DRF
072300971126     C                   CALL      'FNLV55R'
072400971126     C                   PARM                    DSLV55
072500971126     C     D55ERR        IFEQ      *BLANKS
072600971126     C     SIMFEL        ANDNE     D55TFA                                       ELABOR. <> TFA
072700950216     C                   MOVE      'L'           DS1FL1
072800940516     C                   ENDIF
072900940516     C                   ENDIF
073000000000      *
073100000000      * Eseguo OVRPRTF sul file di stampa, solo se non si e' in sede
073200040206     C***  SIMFEL        IFEQ      *ZERO                                        0 = SEDE
073300040206     C***                MOVE      CMD1(1)       COMMAN
073400040206     C***                ELSE                                                   IN FILIALE
073500950215      *
073600950215      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
073700950215     C                   MOVEA     £MOD          CMD(33)                        TIPO MODULO
073800950215     C                   MOVEA     £OUTQ         CMD(51)                        CODA DI STAMPA
073900950214     C                   MOVEL     *BLANKS       COMMAN           82
074000000000     C                   MOVEA     CMD           COMMAN
074100040206     C***                ENDIF
074200040206     c*
074300950214     C                   Z-ADD     82            LUNG             15 5
074400000000     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
074500000000     C                   PARM                    COMMAN
074600000000     C                   PARM                    LUNG
074700941228     C                   OPEN      FNLRE7P                                      PRINTER FILE
074800000000      *
074900000000      * Scrivo, in testata, le selezioni fatte a video
075000950404     C     DS1RIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
075100000000     C                   MOVEL     'STAMPA'      STRIP
075200000000     C                   ELSE
075300000000     C                   MOVEL     'RISTAMPA'    STRIP
075400000000      *
075500000000     C                   MOVE      *BLANKS       STRIP1
075600950313     C     DS1LNP        IFNE      *ZERO
075700950313     C                   MOVE      DS1LNP        STRIP1
075800960926     C                   MOVEL     'P.O.'        STRIP1
075900000000     C                   ELSE
076000000000      *
076100000000      * Solo se in sede
076200040206     C***  SIMFEL        IFEQ      *ZERO
076300040206     C***                MOVEL     'AREA'        STRIP1
076400040206     C***                MOVE      DS1FEL        STRIP1
076500040206     C***                ENDIF
076600000000      *
076700000000     C                   ENDIF
076800000000     C                   ENDIF
076900000000      *
077000000000     C                   SETOFF                                       34        NO TERMINAL
077100950216     C     DS1FL1        IFEQ      'L'                                          'L'=LINEA ARRIV
077200000000     C                   MOVE      'x LINEA'     STRIP
077300000000     C                   SETOFF                                       21
077400000000     C                   ENDIF
077500950216     C     DS1FL1        IFEQ      'T'                                          'T'=TERMINAL AR
077600000000     C                   MOVE      'x TERM.'     STRIP
077700000000     C                   SETON                                        21
077800000000     C     SIMFEL        LOOKUP    LIN                                    34     ON=TROVATA
077900000000     C                   ENDIF
078000000000      *
078100000000      * Riempo una schiera con le filiali elaboratore da stampare
078200040206     C***                EXSR      CRTELA                                       CREA LIN
078300000000      *
078400000000      * Ciclo per ogni filiale elaboratore da stampare
078500000000     C                   Z-ADD     *ZERO         $ALMEN            1 0          ALMENO UNO
078600040206     C***                Z-ADD     1             KK                3 0          CONTATORE
078700040206     C***  ELA(KK)       DOWGT     *ZERO
078800000000      *
078900000000      * Se sono in sede la schiera LIN e' vuota percio' la devo creare ora
079000040206     C                   CLEAR                   LI2
079100040206     C***  SIMFEL        IFEQ      *ZERO
079200040206     C***                EXSR      CRTLIN                                       CREA LIN
079300040206     C***                ELSE
079400040216     C     DS1FL1        IFEQ      'T'                                          'T'=TERMINAL AR
079500950214     C                   MOVEA     LIN           LI2
079600040216     c                   else
079700040216     C                   MOVEA     LIL           LI2
079800040216     c                   endif
079900040206     C***                ENDIF
080000000000      *
080100000000      * Stampo ogni filiale presente nella schiera LIN
080200000000     C                   Z-ADD     *ZERO         $ALME1            1 0          ALMENO UNO
080300000000     C                   Z-ADD     *ZERO         $C                3 0          CONTATORE LNA
080400000000     C                   Z-ADD     1             YY                3 0          CONTATORE
080500950214     C     LI2(YY)       DOWGT     *ZERO
080600000000      *
080700000000      * Con l'elemento della LIN carico la £6 con le filiali associate
080800950214     C                   EXSR      CARLL6                                       CARICA TAB. £6
080900000000      *
081000000000     C                   Z-ADD     *ZERO         $TFAPR                         PRECEDENTE
081100000000      *
081200000000      * Il primo prospetto (per terminal di partenza) deve uscire solo se
081300950216      * richiesta stampa per terminal (DS1FL1='T')
081400950216     C     DS1FL1        IFEQ      'T'                                          ? T E R M I N A L ?
081500000000      *
081600000000     C                   Z-ADD     1             XX                3 0          CONTATORE
081700950214     C     LL6(XX)       DOWGT     *ZERO
081800000000      *
081900000000      * ?  Prospetto  per  terminal  di  partenza  ?
082000000000     C                   SETON                                        31        FASE 1
082100000000     C                   SETON                                        41        STAMPA C/SERV
082200000000     C                   Z-ADD     *ZERO         $PRIMO            1 0          PRIMO GIRO
082300000000     C                   Z-ADD     *ZERO         $PRIM1            1 0          PRIMO GIRO
082400000000     C                   Z-ADD     *ZERO         $FILPR                         PRECEDENTE
082500000000     C                   Z-ADD     *ZERO         $$FILP                         PRECEDENTE
082600000000     C                   MOVE      *BLANKS       $TREPR                         PRECEDENTE
082700090929     C     KFISAR        SETLL     FISAR01L
082800090929     C     KFISAR        READE(N)  FISAR01L                               33
082900000000      *
083000950214      * Leggo tutti i terminal di partenza per LI2,YY
083100000000      * non devo testare le date del periodo in quanto sto leggendo un OPNQRYF
083200000000     C     *IN33         DOWEQ     *OFF
083300000000     C                   Z-ADD     SARTFP        $FILIA
083400000000     C                   EXSR      ELABOR
083500000000     C                   EXSR      MOVFP                                        ELABORAZIONE
083600090929     C     KFISAR        READE(N)  FISAR01L                               33
083700000000     C                   ENDDO                                                  IN33 = OFF
083800000000      *
083900000000      * Se ho fatto almeno un giro elaboro l'ultimo e vado di totali
084000000000     C     $PRIMO        IFNE      *ZERO
084100000000     C                   Z-ADD     *ZERO         $FILIA
084200090929     C                   CLEAR                   FISAR000
084300000000     C                   EXSR      ELABOR
084400000000     C                   EXSR      TOTALI                                       STAMPA TOTALI
084500941228     C                   WRITE     LRE7E3                                       N.B.
084600000000     C                   ENDIF
084700000000      *
084800000000      * ?  Prospetto  per  linea  di  arrivo  ?
084900000000     C                   EXSR      PROSP2                                       2° PROSPETTO
085000000000      *
085100000000     C                   ADD       1             XX
085200000000     C                   ENDDO                                                  IN33 = OFF
085300000000      *
085400000000      * Se a fine elaborazione non ho stampato nulla provo con le linee
085500040209     C***  $ALMEN        IFEQ      *ZERO
085600040209     C***                Z-ADD     *ZERO         £TFA                           TERMINAL ARRIVO
085700040209     C***                SETOFF                                       21
085800040209     C***                EXSR      LINEA
085900040209     C***                ENDIF
086000000000      *
086100950216     C                   ENDIF                                                  DS1FL1 = 'T'
086200000000      *
086300000000      * Se richiesto 'L'=Linea devo stampare solo il secondo prospetto
086400950216     C     DS1FL1        IFEQ      'L'                                          ? L I N E A ?
086500000000     C                   EXSR      LINEA
086600950216     C                   ENDIF                                                  DS1FL1 = 'L'
086700000000      *
086800000000     C                   ADD       1             YY
086900000000     C                   ENDDO                                                  IN33 = OFF
087000040209      * Richiesta stampa per terminal (DS1FL1='T')
087100040209      * e a fine elaborazione non ho stampato nulla provo con le linee
087200040209    1C     DS1FL1        IFEQ      'T'                                          ? T E R M I N A L ?
087300040209     c     $almen        andeq     0
087400040216     c                   clear                   li2
087500040216     C                   MOVEA     LIL           LI2
087600040209      * Stampo ogni filiale presente nella schiera LIN
087700040209     C                   Z-ADD     1             YY                3 0          CONTATORE
087800040209    2C     LI2(YY)       DOWGT     *ZERO
087900040209      * Con l'elemento della LIN carico la £6 con le filiali associate
088000040209     C                   EXSR      CARLL6                                       CARICA TAB. £6
088100040209     C                   z-add     *zero         £tfa                           PRECEDENTE
088200040209     c                   setoff                                       21
088300040209     c                   exsr      linea
088400040209     C                   ADD       1             YY
088500040209    2C                   ENDDO                                                  IN33 = OFF
088600040209    1c                   endif
088700000000      *
088800000000     C     $ALME1        IFNE      *ZERO
088900000000     C                   EXSR      TOTALI                                       STAMPA TOTALI
089000941228     C                   WRITE     LRE7E3                                       N.B.
089100000000     C                   ENDIF
089200000000      *
089300040206     C***                ADD       1             KK
089400040206     C***                ENDDO                                                  IN33 = OFF
089500000000      *
089600000000      * Fine, se non stampato nulla emetto la dicitura
089700000000     C     $ALMEN        IFEQ      *ZERO
089800941228     C                   WRITE     LRE7T1                                       TESTATA
089900941228     C                   WRITE     LRE7R1                                       RIGA
090000941228     C                   WRITE     LRE7E2                                       NULLA
090100000000     C                   ENDIF
090200000000      *
090300941228     C                   CLOSE     FNLRE7P
090400000000      *
090500000000     C                   ENDSR
090600000000      **************************************************************************
090700000000      * Secondo prospetto: per linea di arrivo
090800000000      **************************************************************************
090900000000     C     PROSP2        BEGSR
091000000000      *
091100000000     C                   SETOFF                                       31        FASE 2
091200000000     C                   SETOFF                                       41        NO STAMPA C/SER
091300000000     C                   Z-ADD     *ZERO         $FILPR                         PRECEDENTE
091400000000     C                   Z-ADD     *ZERO         $$FILP                         PRECEDENTE
091500000000     C                   MOVE      *BLANKS       $TREPR                         PRECEDENTE
091600000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
091700000000     C                   Z-ADD     *ZERO         $PRIM1                         PRIMO GIRO
091800000000     C                   Z-ADD     *ZERO         $C                3 0          CONTATORE LNA
091900090929     C     KFISAR        SETLL     FISAR02L
092000090929     C     KFISAR        READE     FISAR02L                               33
092100000000      *
092200000000     C     *IN33         DOWEQ     *OFF
092300000000     C     SARTRE        IFNE      'R'                                          SOLO TERMINAL
092400000000     C                   Z-ADD     SARLNA        $FILIA
092500000000     C                   EXSR      ELABOR                                       ELABORAZIONE
092600000000     C                   ENDIF
092700000000     C                   EXSR      MOVFP                                        RIEMPI CAMPI
092800090929     C     KFISAR        READE     FISAR02L                               33
092900000000     C                   ENDDO                                                  IN33 = OFF
093000000000      *
093100000000     C     $PRIMO        IFNE      *ZERO
093200000000     C                   Z-ADD     *ZERO         $FILIA
093300090929     C                   CLEAR                   FISAR000
093400000000     C                   EXSR      ELABOR
093500000000     C                   EXSR      TOTALI                                       STAMPA TOTALI
093600941228     C                   WRITE     LRE7E3                                       N.B.
093700000000     C                   ENDIF
093800000000      *
093900000000     C                   ENDSR
094000000000      **************************************************************************
094100000000      * Richiesta stampa per 'L'=Linea di arrivo
094200000000      **************************************************************************
094300000000     C     LINEA         BEGSR
094400000000      *
094500000000     C                   SETOFF                                       31        FASE 2
094600000000     C                   SETOFF                                       41        NO STAMPA C/SER
094700000000     C                   Z-ADD     *ZERO         $FILPR                         PRECEDENTE
094800000000     C                   Z-ADD     *ZERO         $$FILP                         PRECEDENTE
094900000000     C                   MOVE      *BLANKS       $TREPR                         PRECEDENTE
095000000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
095100000000     C******               Z-ADD*ZERO     $PRIM1           PRIMO GIRO
095200000000     C                   Z-ADD     *ZERO         $C                3 0          CONTATORE LNA
095300000000      *
095400950214      * Considero solo le linee della schiera LL6
095500000000     C                   Z-ADD     1             XX                3 0          CONTATORE
095600950214     C     LL6(XX)       DOWGT     *ZERO
095700090929     C     KFISAR        SETLL     FISAR04L
095800090929     C     KFISAR        READE     FISAR04L                               33
095900000000      *
096000000000     C     *IN33         DOWEQ     *OFF
096100000000     C     SARTRE        IFNE      'R'                                          SOLO TERMINAL
096200970609     C* SE SARTFA = 0 SIGNIFICA CHE PER QUESTO RECORD TUTTI I DATI
096300970609     C* SONO = 0 E QUINDI LO SCARTO
096400970609     C     SARTFA        ANDGT     *ZEROS
096500000000     C                   Z-ADD     SARLNA        $FILIA
096600000000     C                   EXSR      ELABOR
096700000000     C                   EXSR      MOVFP                                        ELABORAZIONE
096800000000     C                   Z-ADD     1             $ALME1                         ALMENO UNO
096900000000     C                   ENDIF
097000090929     C     KFISAR        READE     FISAR04L                               33
097100000000     C                   ENDDO                                                  IN33 = OFF
097200000000      *
097300000000     C                   ADD       1             XX
097400000000     C                   ENDDO                                                  IN33 = OFF
097500000000      *
097600000000     C     $PRIMO        IFNE      *ZERO
097700000000     C                   Z-ADD     *ZERO         $FILIA
097800090929     C                   CLEAR                   FISAR000
097900000000     C                   EXSR      ELABOR
098000000000     C******               EXSR TOTALI                     STAMPA TOTALI
098100941228     C******               WRITELRE7E3                     N.B.
098200000000     C                   ENDIF
098300000000      *
098400000000     C                   ENDSR
098500000000      **************************************************************************
098600000000      * Riempo la schiera ELA con tutte le filiali con elaboratore da stampare
098700000000      **************************************************************************
098800040206     C***  CRTELA        BEGSR
098900000000      *
099000950214      * Se DS1FEL e' pieno devo stampare solo quella
099100040206     C***  DS1FEL        IFNE      *ZERO
099200040206     C***                Z-ADD     DS1FEL        ELA(1)
099300040206     C***                ELSE
099400000000      *
099500020419      * Altrimenti simfel
099600040206     C***                Z-ADD     simfel        ELA(1)
099700000000      *
099800040206     C***                ENDIF
099900000000      *
100000040206     C***                ENDSR
100100000000      **************************************************************************
100200000000      * Riempo la schiera LIN nel caso la stampa avvenga in sede
100300000000      **************************************************************************
100400040206     C***  CRTLIN        BEGSR
100500000000      *
100600000000      * Verifico se richiesta una singola filiale oppure un'are
100700040206     C***  DS1LNP        IFNE      *ZERO                                        SINGOLA FIL.
100800040206     C***                Z-ADD     DS1LNP        LI2(1)
100900000000      *
101000040206     C***                ELSE                                                   UN'AREA O TUTTO
101100000000      *
101200040206     C***                CLEAR                   DSUL06
101300040206     C***                MOVE      '£1'          D06COD
101400040206     C***                MOVEL     ELA(KK)       D06KEY
101500040206     C***                MOVEL     DSUL06        KPJBU
101600040206     C***                CALL      'TRUL06R'
101700040206     C***                PARM                    KPJBA
101800040206     C***                MOVEL     KPJBU         DSUL06
101900950214     C*
102000040206     C***                Z-ADD     *ZERO         WA
102100040206     C***                Z-ADD     *ZERO         WB
102200040206     C**N99              DO        29            WB
102300040206     C***  T06(WB)       IFNE      *ZEROS
102400000000      *
102500000000      * Nella schiera L1 ho tutte le filiali
102600000000      * Sono escluse quelle gestite da altre filiali
102700040206     C***  T06(WB)       LOOKUP    LI2                                    99
102800040206     C**N99T06(WB)       LOOKUP    L6                                     99
102900040206     C**N99              ADD       1             WA
103000040206     C**N99              Z-ADD     T06(WB)       LI2(WA)
103100040206     C***                ENDIF
103200040206     C***                ENDDO
103300000000      *
103400040206     C***                ENDIF                                                  DS1LNP <> 0
103500000000      *
103600040206     C***                ENDSR
103700000000      **************************************************************************
103800000000      * Riempe i campi di stampa letti dal file
103900000000      **************************************************************************
104000000000     C     MOVFP         BEGSR
104100000620     C**
104200000000     C     SARTRE        IFNE      'C'                                          C/SERVIZIO
104300000000     C     SARTRE        ANDNE     'R'                                          SOLO TERMINAL
104400000000     C                   ADD       SARCLL        CLLP
104500000000     C                   ADD       SARP88        P88P
104600000000     C                   ADD       SARDIS        DISP
104700000000     C                   ADD       SARPAR        PARP
104800000000     C                   ADD       SARPTA        PTAP
104900000000     C                   ADD       SARF35        F35P
105000000000     C                   ADD       SARO35        O35P
105100000000     C                   ADD       SARVOL        VOLP
105200000000     C                   ADD       SARSNF        SNFP
105300000000     C                   ADD       SARSNA        SNAP
105400000000     C                   ADD       SARSEF        SEFP
105500000000     C                   ADD       SARSEA        SEAP
105600090929     C                   ADD       SARSHF        SHFP
105700090929     C                   ADD       SARSHA        SHAP
105800000000      *
105900000620      * Totali per singola filiale
106000000000     C                   ADD       SARCLL        TOTCLL
106100000000     C                   ADD       SARP88        TOTP88
106200000000     C                   ADD       SARDIS        TOTDIS
106300000000     C                   ADD       SARPAR        TOTPAR
106400000000     C                   ADD       SARPTA        TOTPTA
106500000000     C                   ADD       SARF35        TOTF35
106600000000     C                   ADD       SARO35        TOTO35
106700000000     C                   ADD       SARVOL        TOTVOL
106800000000     C                   ADD       SARSNF        TOTSNF
106900000000     C                   ADD       SARSNA        TOTSNA
107000000000     C                   ADD       SARSEF        TOTSEF
107100000000     C                   ADD       SARSEA        TOTSEA
107200090929     C                   ADD       SARSHF        TOTSHF
107300090929     C                   ADD       SARSHA        TOTSHA
107400000619     C                   ENDIF
107500000000      *
107600000620      * Mantengo aggiornati i totali (con indice 3 sono totali PUNTO OP)
107700000000     C     SARTRE        IFEQ      *BLANKS
107800000000     C     SARTRE        OREQ      'C'                                          C/SERVIZIO
107900000000     C                   ADD       SARCLL        CLL(3)
108000000000     C                   ADD       SARP88        P88(3)
108100000000     C                   ADD       SARDIS        DIS(3)
108200000000     C                   ADD       SARPAR        PAR(3)
108300000000     C                   ADD       SARPTA        PTA(3)
108400000000     C                   ADD       SARF35        F35(3)
108500000000     C                   ADD       SARO35        O35(3)
108600000000     C                   ADD       SARVOL        VOL(3)
108700000000     C                   ADD       SARSNF        SNF(3)
108800000000     C                   ADD       SARSNA        SNA(3)
108900000000     C                   ADD       SARSEF        SEF(3)
109000000000     C                   ADD       SARSEA        SEA(3)
109100090929     C                   ADD       SARSHF        SHF(3)
109200090929     C                   ADD       SARSHA        SHA(3)
109300000000     C                   ENDIF
109400000000      *
109500000000      * Totali transiti (con indice 2)
109600000000     C     SARTRE        IFEQ      'T'
109700000000     C                   ADD       SARCLL        CLL(1)
109800000000     C                   ADD       SARP88        P88(1)
109900000000     C                   ADD       SARDIS        DIS(1)
110000000000     C                   ADD       SARPAR        PAR(1)
110100000000     C                   ADD       SARPTA        PTA(1)
110200000000     C                   ADD       SARF35        F35(1)
110300000000     C                   ADD       SARO35        O35(1)
110400000000     C                   ADD       SARVOL        VOL(1)
110500000000     C                   ADD       SARSNF        SNF(1)
110600000000     C                   ADD       SARSNA        SNA(1)
110700000000     C                   ADD       SARSEF        SEF(1)
110800000000     C                   ADD       SARSEA        SEA(1)
110900090929     C                   ADD       SARSHF        SHF(1)
111000090929     C                   ADD       SARSHA        SHA(1)
111100000000     C                   ENDIF
111200000000      *
111300000000      * Totali conto servizi (con indice 1)
111400000000     C     SARTRE        IFEQ      'C'
111500000000     C                   ADD       SARCLL        CLL(2)
111600000000     C                   ADD       SARP88        P88(2)
111700000000     C                   ADD       SARDIS        DIS(2)
111800000000     C                   ADD       SARPAR        PAR(2)
111900000000     C                   ADD       SARPTA        PTA(2)
112000000000     C                   ADD       SARF35        F35(2)
112100000000     C                   ADD       SARO35        O35(2)
112200000000     C                   ADD       SARVOL        VOL(2)
112300000000     C                   ADD       SARSNF        SNF(2)
112400000000     C                   ADD       SARSNA        SNA(2)
112500000000     C                   ADD       SARSEF        SEF(2)
112600000000     C                   ADD       SARSEA        SEA(2)
112700090929     C                   ADD       SARSHF        SHF(2)
112800090929     C                   ADD       SARSHA        SHA(2)
112900000000     C                   ENDIF
113000000000      *
113100000000      * Linee di arrivo di cui solo terminal
113200000000      * riempo delle schiere (con anche la LNA) per stamparle dopo,tutte assieme
113300000000      * Solo se terminal presente in LIN
113400000000     C     SARTFA        LOOKUP    LIN                                    99     ON=TROVATO
113500000000     C   99SARTRE        IFEQ      'R'
113600000000     C                   Z-ADD     1             A
113700000000     C     SARLNA        LOOKUP    LNAR(A)                                99     ON=TROVATO
113800000000     C  N99              ADD       1             $C
113900000000     C  N99              Z-ADD     $C            A
114000000000     C  N99              Z-ADD     SARLNA        LNAR(A)
114100000000     C                   ADD       SARCLL        CLLR(A)
114200000000     C                   ADD       SARP88        P88R(A)
114300000000     C                   ADD       SARPAR        PARR(A)
114400000000     C                   ENDIF
114500000000      *
114600000000     C                   ENDSR
114700000000      **************************************************************************
114800000000      * Calcolo i totali della riga da stampare
114900000000      **************************************************************************
115000000000     C     TOTRIG        BEGSR
115100000000      *
115200000000     C     F35P          ADD       O35P          TOTPEP                         TOT.PESO
115300090929     C     SNFP          ADD       SNAP          TOTNOP                         TOT. C+D
115400090929     C     SEFP          ADD       SEAP          TOTESP                         TOT. "E"
115500090929     C     SHFP          ADD       SHAP          TOTH10                         TOT. "H"
115600000000     C     TOTNOP        ADD       TOTESP        TOTTOP                         TOT.TOTALE
115700090929     C                   ADD       TOTH10        TOTTOP
115800090929     C     SNFP          ADD       SEFP          TOTFRP                         TOT. FRANCHI
115900090929     C                   ADD       SHFP          TOTFRP                         TOT. FRANCHI
116000090929     C     SNAP          ADD       SEAP          TOTASP                         TOT.ASSEGNATI
116100090929     C                   ADD       SHAP          TOTASP                         TOT.ASSEGNATI
116200000000      *
116300000000     C                   ENDSR
116400000000      **************************************************************************
116500000000      * Elaborazione del record letto dall'archivio statistiche
116600000000      **************************************************************************
116700000000     C     ELABOR        BEGSR
116800000622     C* INDCA CAMPO P.O. O TERMINAL
116900000622     C                   CLEAR                   WCAMB
117000000000      *
117100000000      * Al primo giro imposto i "precedenti"
117200000000     C     $PRIMO        IFEQ      *ZERO
117300000622     C                   EXSR      MEMO
117400000000     C                   Z-ADD     1             $PRIMO
117500000000     C                   ENDIF
117600000000      *
117700000622     C                   SETOFF                                       43
117800000000      *
117900000000      * Se cambia term.arrivo stampo la riga coi dati del "precedente"
118000000000      * In questo caso devo pure cambiare pagina
118100940516      * Solo se richiesta la stampa per terminal ('T')
118200000000     C     SARTFA        IFNE      $TFAPR                                       PRECEDENTE
118300000000     C     SARTFA        ANDNE     *ZERO                                        PRECEDENTE
118400950216     C     DS1FL1        ANDEQ     'T'                                          'T'=TERMINAL
118500000000     C                   SETON                                        43        PAGINA NUOVA
118600000622     C                   MOVEL     '1'           WCAMB             1
118700000000     C                   ENDIF
118800000000      *
118900000622      * Se cambia filiale O TRE STAMPO IL PRECEDENTE
119000000622     C     $FILPR        IFNE      999
119100000000     C     $FILIA        IFNE      $FILPR                                       PRECEDENTE
119200000622     C                   MOVEL     '1'           WCAMB             1
119300000000     C                   ENDIF
119400000622     C*
119500000622     C     SARTRE        IFNE      $TREPR                                       PRECEDENTE
119600000622     C     WCAMB         OREQ      '1'                                          PRECEDENTE
119700000622     C                   EXSR      CAMTRE
119800000622     C                   ENDIF
119900000622     C*
120000000622     C* SE COMUNQUE CAMBIATA LA FILIALE O TFA, STAMPO I TOTALI
120100000622     C*  E HO LETTO PIU' DI UN RECORD PER QUEL P.O.
120200000622     C     WCAMB         IFEQ      '1'                                          PRECEDENTE
120300000622     C     $TREPR        ANDNE     ' '
120400000622     C                   EXSR      TOTFIL
120500000622     C                   ENDIF
120600000622     C                   ENDIF
120700000622     C*
120800000000      *
120900000000      * Se ho acceso il 43 stampo i totali e cambio pagina
121000000622     C     *IN43         IFEQ      *ON
121100000000     C                   EXSR      TOTALI
121200000622     C                   WRITE     LRE7E3                                       N.B.
121300000622     C                   EXSR      MEMO
121400941228     C                   WRITE     LRE7T1                                       TESTATA (1)
121500090929     c                   if        not *in10
121600941228     C                   WRITE     LRE7R1                                       RIGA
121700941228     C                   WRITE     LRE7T2                                       TESTATA (2)
121800941228     C                   WRITE     LRE7R1                                       RIGA
121900090929     c                   else
122000090929     C                   WRITE     LRE7R1n                                      RIGA
122100090929     C                   WRITE     LRE7T2n                                      TESTATA (2)
122200090929     C                   WRITE     LRE7R1n                                      RIGA
122300090929     c                   endif
122400090929
122500000000     C                   SETOFF                                       30        OVERFLOW
122600000622     C                   ELSE
122700000622     C                   EXSR      MEMO
122800000622     C                   ENDIF
122900000000      *
123000000000     C                   ENDSR
123100000622      **************************************************************************
123200000622      * MEMORIZZO DATI STD DEL RECORD LETTO
123300000622      **************************************************************************
123400000622     C     MEMO          BEGSR
123500000622     C     SARTFA        IFGT      0
123600000622     C                   Z-ADD     SARTFA        $TFAPR
123700000622     C     $TFAPR        CHAIN     AZORG01L                           99
123800000622     C                   MOVE      *BLANKS       DETERP
123900000622     C  N99              MOVEL     ORGDES        DETERP
124000000622     C                   MOVE      SARTRE        $TREPR
124100000622     C                   Z-ADD     $FILIA        $FILPR
124200000622     C     WCAMB         IFEQ      '1'
124300000622     C                   CLEAR                   $TREPO
124400000622     C                   ENDIF
124500000622     C                   ENDIF
124600000622     C                   ENDSR
124700000622      **************************************************************************
124800000622      * CAMBIATO SARTRE
124900000622      **************************************************************************
125000000622     C     CAMTRE        BEGSR
125100000622     C* VERIFICO SE HO STAMPATO UN TRE=' '
125200000622     C*  SE NON L'HO STAMPO LO FACCIO ORA A ZERO
125300000622     C     $TREPO        IFEQ      ' '
125400000622     C     $TREPR        ANDNE     ' '
125500000622     C******     WCAMB     ANDEQ' '
125600000622      *
125700000622      * Se e' il primissimo giro e non ho ancora emesso l'intestazione...
125800000622     C     $PRIM1        IFEQ      *ZERO
125900000622     C                   WRITE     LRE7T1                                       TESTATA (1)
126000090929     c                   if        not *in10
126100000622     C                   WRITE     LRE7R1                                       RIGA
126200000622     C                   WRITE     LRE7T2                                       TESTATA (2)
126300000622     C                   WRITE     LRE7R1                                       RIGA
126400090929     c                   else
126500090929     C                   WRITE     LRE7R1n                                      RIGA
126600090929     C                   WRITE     LRE7T2n                                      TESTATA (2)
126700090929     C                   WRITE     LRE7R1n                                      RIGA
126800090929     c                   endif
126900090929     c
127000000622     C                   SETOFF                                       30        OVERFLOW
127100000622     C                   Z-ADD     1             $PRIM1
127200000622     C                   ENDIF
127300000622      *
127400000622     C                   MOVE      DIC(7)        DICITP                         ..DELLA FILIALE
127500000622     C                   MOVEL     $FILPR        DICITP                         FILIALE
127600090929     C                   WRITE     LRE7D2
127700000622     C*
127800000622     C                   MOVEL     'S'           $TREPO            1
127900000622     C                   ENDIF
128000000622     C*
128100000622     C* STAMPO IL RECORD PRECEDENTE
128200000622     C     $TREPR        IFEQ      'T'
128300000622     C                   MOVEL     DIC(4)        DICITP                         TRANSITI
128400000622     C                   MOVEL     $FILPR        DICITP                         FILIALE
128500000622     C                   SETON                                        4244
128600000622     C                   ENDIF
128700000622     C     $TREPR        IFEQ      'P'
128800000622     C                   MOVEL     DIC(8)        DICITP                         POSTE
128900000622     C                   MOVEL     $FILPR        DICITP                         FILIALE
129000000622     C                   SETON                                        4244
129100000622     C                   ENDIF
129200000622     C*
129300000622     C     $TREPR        IFEQ      ' '
129400000622     C                   MOVEL     'S'           $TREPO            1
129500000622     C* E QUELLO CHE LEGGO E' <>' ' E NON E' CAMBIATA LA FILIALE O TFA
129600000622     C     WCAMB         IFNE      '1'
129700000622     C     SARTRE        ANDNE     ' '
129800000622     C                   MOVEL     DIC(7)        DICITP                         P.O.
129900000622     C                   MOVEL     $FILPR        DICITP                         FILIALE
130000000622     C                   SETON                                        4244
130100000622     C                   ENDIF
130200000622     C                   ENDIF
130300000622     C*
130400000622     C                   Z-ADD     $FILPR        FILIAP
130500000622     C     FILIAP        IFNE      999
130600000622     C                   EXSR      SCRIVI
130700000622     C                   ENDIF
130800000622     C*
130900000622     C                   ENDSR
131000000000      **************************************************************************
131100000000      * Ho stampato anche i transiti quindi tiro le somme della filiale
131200000000      **************************************************************************
131300000000     C     TOTFIL        BEGSR
131400000000      *
131500000000     C                   Z-ADD     TOTCLL        CLLP
131600000000     C                   Z-ADD     TOTP88        P88P
131700000000     C                   Z-ADD     TOTDIS        DISP
131800000000     C                   Z-ADD     TOTPAR        PARP
131900000000     C                   Z-ADD     TOTPTA        PTAP
132000000000     C                   Z-ADD     TOTF35        F35P
132100000000     C                   Z-ADD     TOTO35        O35P
132200000000     C                   Z-ADD     TOTVOL        VOLP
132300000000     C                   Z-ADD     TOTSNF        SNFP
132400000000     C                   Z-ADD     TOTSNA        SNAP
132500000000     C                   Z-ADD     TOTSEF        SEFP
132600000000     C                   Z-ADD     TOTSEA        SEAP
132700090929     C                   Z-ADD     TOTShF        ShFP
132800090929     C                   Z-ADD     TOTShA        ShAP
132900000000     C                   SETOFF                                       4244
133000000000     C                   EXSR      SCRIVI
133100000000      *
133200000000     C                   ENDSR
133300000000      **************************************************************************
133400000000      * Testa l'overflow e stampa su carta
133500000000      **************************************************************************
133600000000     C     SCRIVI        BEGSR
133700000000      *
133800000000      * Se stampo una filiale la decodifico
133900000000     C                   MOVE      *BLANKS       ORGDEP
134000000000     C  N42              DO
134100000000     C     FILIAP        CHAIN     AZORG01L                           99
134200000000     C  N99              MOVE      ORGDES        ORGDEP                         DECODIFICA
134300000000     C                   ENDDO
134400000000      *
134500000000      * Se e' il primissimo giro e non ho ancora emesso l'intestazione...
134600000000      * oppure per overflow ...
134700000000     C     *IN30         IFEQ      *ON                                          OVERFLOW
134800000000     C     $PRIM1        OREQ      *ZERO                                        PRIMA EMISSIONE
134900941228     C                   WRITE     LRE7T1                                       TESTATA (1)
135000090929     c                   if        not *in10
135100941228     C                   WRITE     LRE7R1                                       RIGA
135200941228     C                   WRITE     LRE7T2                                       TESTATA (2)
135300941228     C                   WRITE     LRE7R1                                       RIGA
135400090929     c                   else
135500090929     C                   WRITE     LRE7R1n                                      RIGA
135600090929     C                   WRITE     LRE7T2n                                      TESTATA (2)
135700090929     C                   WRITE     LRE7R1n                                      RIGA
135800090929     c                   endif
135900000000     C                   SETOFF                                       30        OVERFLOW
136000000000     C                   Z-ADD     1             $PRIM1
136100000000     C                   ENDIF
136200000000      *
136300000000     C                   EXSR      TOTRIG                                       TOTALI RIGA
136400090929     C  n10              WRITE     LRE7D1                                       DETTAGLIO
136500090929     C   10              WRITE     LRE7D1n                                      DETTAGLIO
136600000000     C                   Z-ADD     1             $ALMEN
136700000000      *
136800000000     C                   Z-ADD     *ZERO         CLLP
136900000000     C                   Z-ADD     *ZERO         P88P
137000000000     C                   Z-ADD     *ZERO         DISP
137100000000     C                   Z-ADD     *ZERO         PARP
137200000000     C                   Z-ADD     *ZERO         PTAP
137300000000     C                   Z-ADD     *ZERO         F35P
137400000000     C                   Z-ADD     *ZERO         O35P
137500000000     C                   Z-ADD     *ZERO         VOLP
137600000000     C                   Z-ADD     *ZERO         SNFP
137700000000     C                   Z-ADD     *ZERO         SNAP
137800000000     C                   Z-ADD     *ZERO         SEFP
137900000000     C                   Z-ADD     *ZERO         SEAP
138000090929     C                   Z-ADD     *ZERO         ShFP
138100090929     C                   Z-ADD     *ZERO         ShAP
138200000000      *
138300000000      * Azzero i  totali di filiale della singola filiale
138400000000     C  N42              DO
138500000000     C                   Z-ADD     *ZERO         TOTCLL            7 0
138600000000     C                   Z-ADD     *ZERO         TOTP88            7 0
138700000000     C                   Z-ADD     *ZERO         TOTDIS            7 0
138800000000     C                   Z-ADD     *ZERO         TOTPAR            7 0
138900000000     C                   Z-ADD     *ZERO         TOTPTA            9 1
139000000000     C                   Z-ADD     *ZERO         TOTF35            9 1
139100000000     C                   Z-ADD     *ZERO         TOTO35            9 1
139200000000     C                   Z-ADD     *ZERO         TOTVOL            9 4
139300000000     C                   Z-ADD     *ZERO         TOTSNF            7 0
139400000000     C                   Z-ADD     *ZERO         TOTSNA            7 0
139500000000     C                   Z-ADD     *ZERO         TOTSEF            7 0
139600000000     C                   Z-ADD     *ZERO         TOTSEA            7 0
139700090929     C                   Z-ADD     *ZERO         TOTShF            7 0
139800090929     C                   Z-ADD     *ZERO         TOTShA            7 0
139900000000     C                   ENDDO
140000000000     C                   Z-ADD     FILIAP        $$FILP
140100000000     C                   SETOFF                                       4244
140200000000      *
140300000000     C                   ENDSR
140400000000      **************************************************************************
140500000000      * Stampo i  totali della stampa
140600000000      **************************************************************************
140700000000     C     TOTALI        BEGSR
140800000000      *
140900000000     C                   Z-ADD     *ZERO         FILIAP
141000000000      *
141100000000      *?    T O T A L I    C O N T O    S E R V I Z I    ?
141200000000      * Prima dei totali veri e propri devo stampare i conto servizi
141300000000      * solo per il dettaglio per Terminal di partenza
141400000000     C     *IN41         IFEQ      *ON
141500000000     C     CLL(2)        IFGT      *ZERO
141600000000     C     SNF(2)        ORGT      *ZERO
141700000000     C     SNA(2)        ORGT      *ZERO
141800000000     C     SEF(2)        ORGT      *ZERO
141900000000     C     SEA(2)        ORGT      *ZERO
142000090929     C     ShF(2)        ORGT      *ZERO
142100090929     C     ShA(2)        ORGT      *ZERO
142200000000     C                   Z-ADD     CLL(2)        CLLP
142300000000     C                   Z-ADD     P88(2)        P88P
142400000000     C                   Z-ADD     DIS(2)        DISP
142500000000     C                   Z-ADD     PAR(2)        PARP
142600000000     C                   Z-ADD     PTA(2)        PTAP
142700000000     C                   Z-ADD     F35(2)        F35P
142800000000     C                   Z-ADD     O35(2)        O35P
142900000000     C                   Z-ADD     VOL(2)        VOLP
143000000000     C                   Z-ADD     SNF(2)        SNFP
143100000000     C                   Z-ADD     SNA(2)        SNAP
143200000000     C                   Z-ADD     SEF(2)        SEFP
143300000000     C                   Z-ADD     SEA(2)        SEAP
143400090929     C                   Z-ADD     ShF(2)        ShFP
143500090929     C                   Z-ADD     ShA(2)        ShAP
143600000000     C                   MOVE      DIC(1)        DICITP                         C/SERVIZI
143700000000     C                   SETON                                        4244
143800000000     C                   EXSR      SCRIVI
143900000000     C                   ENDIF
144000000000     C                   ENDIF
144100000000      *
144200090929     C  N10              WRITE     LRE7R1                                       RIGA
144300090929     C   10              WRITE     LRE7R1N                                      RIGA
144400000000     C                   SETOFF                                       44
144500000000      *
144600000000      *?    T O T A L I    D E L L A    F I L I A L E    ?
144700000000     C                   Z-ADD     CLL(3)        CLLP
144800000000     C                   Z-ADD     P88(3)        P88P
144900000000     C                   Z-ADD     DIS(3)        DISP
145000000000     C                   Z-ADD     PAR(3)        PARP
145100000000     C                   Z-ADD     PTA(3)        PTAP
145200000000     C                   Z-ADD     F35(3)        F35P
145300000000     C                   Z-ADD     O35(3)        O35P
145400000000     C                   Z-ADD     VOL(3)        VOLP
145500000000     C                   Z-ADD     SNF(3)        SNFP
145600000000     C                   Z-ADD     SNA(3)        SNAP
145700000000     C                   Z-ADD     SEF(3)        SEFP
145800000000     C                   Z-ADD     SEA(3)        SEAP
145900090929     C                   Z-ADD     ShF(3)        ShFP
146000090929     C                   Z-ADD     ShA(3)        ShAP
146100000622     C     CLL(4)        IFGT      0
146200000000     C                   MOVE      DIC(3)        DICITP                         DELLA FILIALE
146300000622     C                   ELSE
146400000622     C                   MOVE      DIC(7)        DICITP                         DELLA FILIALE
146500000622     C                   ENDIF
146600000000     C                   SETON                                        42
146700000000     C                   SETOFF                                       44        SALTA 1 RIGA DOPO
146800000000     C                   EXSR      SCRIVI
146900000000      *
147000000000      *?    T O T A L I    T R A N S I T I    ?
147100000622     C     CLL(1)        IFGT      0
147200000000     C                   Z-ADD     CLL(1)        CLLP
147300000000     C                   Z-ADD     P88(1)        P88P
147400000000     C                   Z-ADD     DIS(1)        DISP
147500000000     C                   Z-ADD     PAR(1)        PARP
147600000000     C                   Z-ADD     PTA(1)        PTAP
147700000000     C                   Z-ADD     F35(1)        F35P
147800000000     C                   Z-ADD     O35(1)        O35P
147900000000     C                   Z-ADD     VOL(1)        VOLP
148000000000     C                   Z-ADD     SNF(1)        SNFP
148100000000     C                   Z-ADD     SNA(1)        SNAP
148200000000     C                   Z-ADD     SEF(1)        SEFP
148300000000     C                   Z-ADD     SEA(1)        SEAP
148400090929     C                   Z-ADD     ShF(1)        ShFP
148500090929     C                   Z-ADD     ShA(1)        ShAP
148600000000     C                   MOVE      DIC(4)        DICITP                         TRANSITI
148700000000     C                   SETON                                        42
148800000000     C                   SETOFF                                       44        SALTA 1 RIGA DOPO
148900000000     C                   EXSR      SCRIVI
149000000622     C                   ENDIF
149100000622     C* SOLO SE HO DATI POSTE E DATI TRANSITO
149200090929     C**   CLL(4)        IFGT      0
149300090929     C**   CLL(1)        IFGT      0
149400000000      *
149500000620      *?    T O T A L I    SENZA POSTE        (compresi i transiti)    ?
149600090929     C**   CLL(1)        ADD       CLL(3)        CLLP
149700090929     C**   P88(1)        ADD       P88(3)        P88P
149800090929     C**   DIS(1)        ADD       DIS(3)        DISP
149900090929     C**   PAR(1)        ADD       PAR(3)        PARP
150000090929     C**   PTA(1)        ADD       PTA(3)        PTAP
150100090929     C**   F35(1)        ADD       F35(3)        F35P
150200090929     C**   O35(1)        ADD       O35(3)        O35P
150300090929     C**   VOL(1)        ADD       VOL(3)        VOLP
150400090929     C*+   SNF(1)        ADD       SNF(3)        SNFP
150500090929     C**   SNA(1)        ADD       SNA(3)        SNAP
150600090929     C**   SEF(1)        ADD       SEF(3)        SEFP
150700090929     C**   SEA(1)        ADD       SEA(3)        SEAP
150800090929     C**   ShF(1)        ADD       ShF(3)        ShFP
150900090929     C**   ShA(1)        ADD       ShA(3)        ShAP
151000090929     C*+                 MOVE      DIC(6)        DICITP                         TOT.GENERALE
151100090929     C**                 SETON                                        42
151200090929     C**                 SETOFF                                       44        SALTA 1 RIGA DOPO
151300090929     C**                 EXSR      SCRIVI
151400090929     C*+                 ENDIF
151500000620      *
151600000620      *?    T O T A L I   P O S T E                                    ?
151700090929     C**                 Z-ADD     CLL(4)        CLLP
151800090929     C**                 Z-ADD     P88(4)        P88P
151900090929     C**                 Z-ADD     DIS(4)        DISP
152000090929     C**                 Z-ADD     PAR(4)        PARP
152100090929     C**                 Z-ADD     PTA(4)        PTAP
152200090929     C**                 Z-ADD     F35(4)        F35P
152300090929     C**                 Z-ADD     O35(4)        O35P
152400090929     C*+                 Z-ADD     VOL(4)        VOLP
152500090929     C**                 Z-ADD     SNF(4)        SNFP
152600090929     C*+                 Z-ADD     SNA(4)        SNAP
152700090929     C*+                 Z-ADD     SEF(4)        SEFP
152800090929     C**                 Z-ADD     SEA(4)        SEAP
152900090929     C**                 MOVE      DIC(5)        DICITP                         TOT.GENERALE
153000090929     C*+                 SETON                                        4244
153100090929     C*+                 EXSR      SCRIVI
153200000620      *
153300090929     C**                 WRITE     LRE7R1                                       RIGA
153400090929     C**                 ENDIF
153500000620      *
153600000620      *?    T O T A L I    G E N E R A L I    (compresi i transiti)    ?
153700000620     C     CLL(1)        ADD       CLL(3)        CLLP
153800000620     C     P88(1)        ADD       P88(3)        P88P
153900000620     C     DIS(1)        ADD       DIS(3)        DISP
154000000620     C     PAR(1)        ADD       PAR(3)        PARP
154100000620     C     PTA(1)        ADD       PTA(3)        PTAP
154200000620     C     F35(1)        ADD       F35(3)        F35P
154300000620     C     O35(1)        ADD       O35(3)        O35P
154400000620     C     VOL(1)        ADD       VOL(3)        VOLP
154500000620     C     SNF(1)        ADD       SNF(3)        SNFP
154600000620     C     SNA(1)        ADD       SNA(3)        SNAP
154700000620     C     SEF(1)        ADD       SEF(3)        SEFP
154800000620     C     SEA(1)        ADD       SEA(3)        SEAP
154900090929     C     SHF(1)        ADD       SHF(3)        SHFP
155000090929     C     SHA(1)        ADD       SHA(3)        SHAP
155100000620     C                   MOVE      DIC(2)        DICITP                         TOT.GENERALE
155200000620     C                   SETON                                        4244
155300000620     C                   EXSR      SCRIVI
155400000620      *
155500090929     C  N10              WRITE     LRE7R1                                       RIGA
155600090929     C   10              WRITE     LRE7R1N                                      RIGA
155700000000      *
155800950314      * Se sono nella seconda fase devo stampare il prospetto delle linee di
155900000000      * arrivo di cui solo terminal di arrivo
156000950216     C  N31DS1FL1        IFEQ      'T'
156100000000     C                   EXSR      SOLOT                                        SOLO TERMINAL
156200000000     C                   ENDIF
156300000000      *
156400000000     C                   EXSR      AZZER                                        AZZERAMENTI
156500000000      *
156600000000     C                   ENDSR
156700000000      **************************************************************************
156800000000      * Pulizia schiere
156900000000      **************************************************************************
157000000000     C     AZZER         BEGSR
157100000000      *
157200000000     C                   CLEAR                   CLL
157300000000     C                   CLEAR                   P88
157400000000     C                   CLEAR                   DIS
157500000000     C                   CLEAR                   PAR
157600000000     C                   CLEAR                   PTA
157700000000     C                   CLEAR                   F35
157800000000     C                   CLEAR                   O35
157900000000     C                   CLEAR                   VOL
158000000000     C                   CLEAR                   SNF
158100000000     C                   CLEAR                   SNA
158200000000     C                   CLEAR                   SEF
158300000000     C                   CLEAR                   SEA
158400090929     C                   CLEAR                   ShF
158500090929     C                   CLEAR                   ShA
158600000000      *
158700000000     C                   CLEAR                   LNAR
158800000000     C                   CLEAR                   CLLR
158900000000     C                   CLEAR                   P88R
159000000000     C                   CLEAR                   PARR
159100000000     C                   Z-ADD     *ZERO         $C                             CONTATORE
159200000000      *
159300000000     C                   ENDSR
159400000000      **************************************************************************
159500950214      * Carico la tabella £6 nella schiera LL6 (filiali associate)
159600000000      **************************************************************************
159700950214     C     CARLL6        BEGSR
159800000000      *
159900020419      * Per ogni linea presente in LIN carico la £6
160000020419     c                   clear                   dsul06
160100020419     c                   eval      d06cod = '£6'
160200020419     c                   movel     li2(yy)       d06key
160300050802     C                   MOVE      'S'           D06KEY
160400050802     C                   MOVEL     ds1ada        D06drf
160500020419     c                   movel     dsul06        kpjbu
160600020419     c                   call      'TRUL06R'
160700020419     c                   parm                    kpjba
160800020419     c                   movel     kpjbu         dsul06
160900020419     c                   movea     t06           ll6
161000000000      *
161100000000     C                   ENDSR
161200000000      **************************************************************************
161300950214      * Carico la tabella £6 nella schiera LL6 (filiali associate)
161400000000      **************************************************************************
161500000000     C     SOLOT         BEGSR
161600000000      *
161700000000      * Stampo tutta la schiera con le LNA di cui solo terminal
161800000000     C                   Z-ADD     *ZERO         $PRIM2            1 0          PRIMO GIRO
161900000000     C                   Z-ADD     1             A                 3 0          CONTATORE
162000000000     C     LNAR(A)       DOWNE     *ZERO
162100000000      *
162200000000      * Al primo giro stampo l'intestazione
162300000000     C     $PRIM2        IFEQ      *ZERO
162400941228     C   30              WRITE     LRE7T1                                       TESTATA (1)
162500941228     C   30              WRITE     LRE7R1                                       RIGA
162600000000     C   30              SETOFF                                       30        OVERFLOW
162700941228     C                   WRITE     LRE7T3                                       INTESTAZIONE
162800000000     C                   Z-ADD     1             $PRIM2
162900000000     C                   ENDIF
163000000000      *
163100000000     C                   Z-ADD     LNAR(A)       FILIAP                         FILIALE
163200000000     C     LNAR(A)       CHAIN     AZORG01L                           99
163300000000     C  N99              MOVEL     ORGDES        ORGDEP                         DESCRIZIONE
163400000000     C   99              MOVE      *BLANKS       ORGDEP
163500000000     C                   Z-ADD     P88R(A)       P88P
163600000000     C                   Z-ADD     PARR(A)       PARP
163700000000     C                   Z-ADD     CLLR(A)       CLLP                           NR.COLLI
163800000000      *
163900000000      * Stampo il dettaglio della riga
164000941228     C   30              WRITE     LRE7T1                                       TESTATA (1)
164100941228     C   30              WRITE     LRE7R1                                       RIGA
164200000000     C   30              SETOFF                                       30        OVERFLOW
164300941228     C                   WRITE     LRE7D3                                       DETTAGLIO
164400000000      *
164500000000      * Mantengo aggiornati i totali
164600000000     C                   ADD       P88R(A)       P88TO
164700000000     C                   ADD       PARR(A)       PARTO
164800000000     C                   ADD       CLLR(A)       CLLTO
164900000000      *
165000000000     C                   ADD       1             A                              CONTATORE
165100000000     C                   ENDDO
165200000000      *
165300000000      * Se stampata almeno una riga scrivi anche il totale
165400000000     C     $PRIM2        IFNE      *ZERO
165500941228     C                   WRITE     LRE7E4                                       TOTALE
165600000000     C                   Z-ADD     *ZERO         P88TO
165700000000     C                   Z-ADD     *ZERO         PARTO
165800000000     C                   Z-ADD     *ZERO         CLLTO
165900000000     C                   ENDIF
166000000000      *
166100000000     C                   ENDSR
166200000000      **************************************************************************
166300000620** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
166400950214OVRPRTF FILE(FNLRE7P) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.arriv
166500950214')
166600000000** Comando per eseguire QCMDEXC
166700941228OVRPRTF FILE(FNLRE7P) USRDTA('Stat.arriv')
166800000000** Diciture da stampare
166900000000    CONTO SERVIZI
167000000000T O T A L I    G E N E R A L I
167100000620DEL PUNTO OPERATIVO (NO POSTE)
167200000000......... IN TRANSITO
167300000620............... POSTE
167400000620.................. T O T A L E
167500000620......... DEL PUNTO OPERATIVO
167600000620......... POSTE
