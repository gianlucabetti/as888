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
022200100701     C* CHIUSURA FILE
022300100701     C                   CLEAR                   trul09ds
022400100701     C                   MOVEL     'C'           i09TLA
022500100701     C                   CALL      'TRUL09R'
022600100701     C                   PARM                    trul09ds
022700000000      *
022800000000     C                   SETON                                        LR
022900990507     C   99
023000090929     CANN99              UPDATE    FISAR000
023100090929      * Ho dovuto usare FISAR di I/O per poter fare il clear del formato
023200090929      * FISAR000
023300000000      *
023400020212     C*****************************************************************
023500020212     C*  PARTE COMUNE STAMPE GIORNALIERE
023600020212     C*
023700020212     C*  . REPERIMENTO 'SIMFEL'
023800020212     C*  . CARICAMENTO '£1'
023900020212     C*  . DETERMINAZIONE LINEE DA STAMPARE ('LIN')
024000020212     C*  . CODA DI STAMPA ('£OUTQ')
024100020212     C*  . CODA DI STAMPA PER FATTURE ('£OUTQF)
024200020212     C*  . MODULO PER LA STAMPA ('£MOD')
024300020212     C*  . RICHIAMO ROUTINE DI ELABORAZIONE ('LEGGI')
024400020212     C*****************************************************************
024500020212     C     COMUNE        BEGSR
024600020212     C*
024700020212     C*****************************************************************
024800020212     C*       RIEPILOGO INDICATORI:
024900020212     C* 08    - RICHIESTA UNA SPECIFICA LINEA
025000020212     C* 11    -           STAMPA PER 1L
025100020212     C* 14    - PER 2 L : STAMPE PER 2L SUDDIVISE
025200020212     C*****************************************************************
025300020212     C                   Z-ADD     1             CODUT
025400020212     C*
025500020212     C* Reperimento parametri aziendali
025600020212     C                   CALL      'X§PARUT'
025700020212     C                   PARM                    UT§DSE
025800020212     C                   MOVEL     RAGUT         RSUT             20
025900020212     C                   MOVEL     REC80         CNCR80
026000020212     C*
026100020212     C* Chiave di accesso per il file tabelle
026200020212     C     KTAB1         KLIST                                                  TABEL00F
026300020212     C                   KFLD                    CODUT
026400020212     C                   KFLD                    KCOD
026500020212     C                   KFLD                    KKEY
026600020212     C     KTAB2         KLIST                                                  TABEL00F
026700020212     C                   KFLD                    CODUT
026800020212     C                   KFLD                    KCOD
026900020212     C*
027000020212     C     *LIKE         DEFINE    TBLCOD        KCOD                           COD.TABELLA
027100020212     C     *LIKE         DEFINE    TBLKEY        KKEY                           ELEMENTO
027200020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
027300020212     C*
027400020212     C* DETERMINO DATA E ORA DI ESECUZIONE
027500020212     C                   TIME                    WTIME            14 0          ORA & DATA
027600020212     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
027700020212     C                   MOVEL     WTIME         UTIME             6 0          HH:MM:SS
027800090929
027900090929     c*  stampare anche le spedizioni H
028000090929     c                   if        ds1ada>=§vpotsph
028100090929     c                   seton                                        10
028200090929     c                   endif
028300090929     C*
028400020212     C*
028500020419     C* CARICO '£1'
028600020212     C                   CLEAR                   KPJBU
028700020212     C                   CLEAR                   DSUL06
028800020212     C                   MOVE      '£1'          D06COD
028900020212     C                   MOVEL     SIMFEL        D06KEY
029000050802     C                   MOVE      'S'           D06KEY
029100050802     C                   MOVEL     ds1ada        D06drf
029200020212     C                   MOVEL     DSUL06        KPJBU
029300020212     C                   CALL      'TRUL06R'
029400020212     C                   PARM                    KPJBA
029500020212     C                   MOVEL     KPJBU         DSUL06
029600020419     c                   movea     t06           sav_t06
029700020419
029800020419      * se in arrivo carico la £6
029900020419 b1  c                   if        ds1tbo = 'A'
030000020419     c                   clear                   wc
030100020419 b2  c                   do        30            wa
030200020422     c                   if        sav_t06(wa) = *zeros
030300020422     c                   leave
030400020422     c                   endif
030500020419     c                   clear                   kpjbu
030600020419     c                   clear                   dsul06
030700020419     c                   eval      d06cod = '£6'
030800020419     c                   movel     sav_t06(wa)   d06key
030900050802     C                   MOVE      'S'           D06KEY
031000020419     c                   eval      d06esc = 'S'
031100050802     C                   MOVEL     ds1ada        D06drf
031200020419     c                   movel     dsul06        kpjbu
031300020419     c                   call      'TRUL06R'
031400020419     c                   parm                    kpjba
031500020419     c                   movel     kpjbu         dsul06
031600020419 b3  c                   do        30            wb
031700020419     c                   if        t06(wb) = *zeros
031800020419     c                   leave
031900020419     c                   endif
032000020419     c                   add       1             wc
032100020419     c                   z-add     t06(wb)       l6(wc)
032200020419 e3  c                   enddo
032300020419 e2  c                   enddo
032400020212     C*
032500020419     C*  ESCLUDO DA £1 LE FILIALI DELLA £6
032600020212     C                   Z-ADD     *ZERO         WB
032700020419 b2  C                   DO        29            WA
032800020419     C     sav_T06(WA)   LOOKUP    L6                                     30     ON=TROVATO
032900020212     C  N30              ADD       1             WB
033000020419     C  N30              Z-ADD     sav_T06(WA)   L1(WB)
033100020419 e2  C                   ENDDO
033200020419 x1  C                   ELSE
033300020419     C                   MOVEA     sav_T06       L1
033400020419 e1  C                   ENDIF
033500020212     C*
033600020212     C* ESCLUDO DA £1 LINEE NON AUTORIZZATE PER POSTE
033700020212     C                   MOVEA     L1            LIN
033800020212     C                   CLEAR                   L1
033900020212     C                   Z-ADD     *ZERO         WB                3 0          DI COMODO
034000020212     C     1             DO        30            WPT               3 0          DI COMODO PER POSTE
034100020212     C     LIN(WPT)      IFNE      0
034200020212     C                   CLEAR                   FNLSC4
034300020212     C                   Z-ADD     LIN(WPT)      IC4LIN                         LINEA
034400020212     C                   MOVE      'B'           IC4FLG                         B=BATCH
034500020212     C                   MOVEL     FNLSC4        KPJBU
034600020212     C                   CALL      'FNLSC4R1'
034700020212     C                   PARM                    KPJBA
034800050802     C                   PARM                    ds1ada
034900020212     C                   MOVEL     KPJBU         FNLSC4
035000020212     C     OC4ESE        IFEQ      'S'
035100020212     C                   ADD       1             WB
035200020212     C                   MOVE      LIN(WPT)      L1(WB)
035300020212     C                   ENDIF
035400020212     C                   ENDIF
035500020212     C                   ENDDO
035600070103     C*
035700070103     C* RIEMPO UNA SCHIERA CON SOLI I 2° LIVELLO
035800070103     C                   Z-ADD     *ZERO         WB                3 0          DI COMODO
035900070103     C                   DO        29            WA                3 0
036000070103     C     L1(WA)        IFNE      *ZEROS
036100070103     C     L1(WA)        ANDNE     SIMFEL
036200070103     C                   ADD       1             WB
036300070103     C                   Z-ADD     L1(WA)        LV2(WB)
036400070103     C                   ENDIF
036500070103     C                   ENDDO
036600040205     c*
036700040205     c* Carico schiera delle linee da elaborare in base al trul09r
036800040205     c                   clear                   trul09ds
036900100630     c                   eval      i09mod = 'DEF'
037000040205     c     ds1fgs        ifeq      0
037100040205     c                   eval      i09tfa = simfel
037200040205     c                   else
037300040205     c                   eval      i09tfa = ds1fgs
037400040205     c                   endif
037500040205     c                   eval      i09dta = ds1ada
037600040205     c                   call      'TRUL09R'
037700040205     c                   parm                    trul09ds
037800040206     c                   movea     t09           sav_t09
037900040206      * se in arrivo carico la £69
038000040206 b1  c                   if        ds1tbo = 'A'
038100040206     c                   clear                   wc
038200040206 b2  c                   do        30            wa
038300040206     c                   if        sav_t09(wa) = *zeros
038400040206     c                   leave
038500040206     c                   endif
038600040206     c                   clear                   kpjbu
038700040206     c                   clear                   dsul06
038800040206     c                   eval      d06cod = '£6'
038900040206     c                   movel     sav_t09(wa)   d06key
039000050802     C                   MOVE      'S'           D06KEY
039100050802     C                   MOVEL     ds1ada        D06drf
039200040206     c                   eval      d06esc = 'S'
039300040206     c                   movel     dsul06        kpjbu
039400040206     c                   call      'TRUL06R'
039500040206     c                   parm                    kpjba
039600040206     c                   movel     kpjbu         dsul06
039700040206 b3  c                   do        30            wb
039800040206     c                   if        t06(wb) = *zeros
039900040206     c                   leave
040000040206     c                   endif
040100040206     c                   add       1             wc
040200040206     c                   z-add     t06(wb)       l69(wc)
040300040206 e3  c                   enddo
040400040206 e2  c                   enddo
040500040206     C*
040600040206     C*  ESCLUDO DA t09 LE FILIALI DELLA £69
040700040206     C                   Z-ADD     *ZERO         WB
040800040206 b2  C                   DO        29            WA
040900040206     C     sav_T09(WA)   LOOKUP    L69                                    30     ON=TROVATO
041000040206     C  N30              ADD       1             WB
041100040206     C  N30              Z-ADD     sav_T09(WA)   L9(WB)
041200040206 e2  C                   ENDDO
041300040206 x1  C                   ELSE
041400040206     C                   MOVEA     sav_T09       L9
041500040206 e1  C                   ENDIF
041600040206     C* ESCLUDO DA £9 LINEE NON AUTORIZZATE PER POSTE
041700040206     C                   MOVEA     L9            LIN
041800040206     C                   CLEAR                   L9
041900040206     C                   Z-ADD     *ZERO         WB                3 0          DI COMODO
042000040206     C     1             DO        30            WPT               3 0          DI COMODO PER POSTE
042100040206     C     LIN(WPT)      IFNE      0
042200040206     C                   CLEAR                   FNLSC4
042300040206     C                   Z-ADD     LIN(WPT)      IC4LIN                         LINEA
042400040206     C                   MOVE      'B'           IC4FLG                         B=BATCH
042500040206     C                   MOVEL     FNLSC4        KPJBU
042600040206     C                   CALL      'FNLSC4R1'
042700040206     C                   PARM                    KPJBA
042800050802     C                   PARM                    ds1ada
042900040206     C                   MOVEL     KPJBU         FNLSC4
043000040206     C     OC4ESE        IFEQ      'S'
043100040206     C                   ADD       1             WB
043200040206     C                   MOVE      LIN(WPT)      L9(WB)
043300040206     C                   ENDIF
043400040206     C                   ENDIF
043500040206     C                   ENDDO
043600040216     c* per l'elaborazione per linea la schiera l9 non va bene devo
043700040216     c* caricarne un'altra richiamando lsc4r1 con flg="N" altrimenti
043800040216     c* per es. se sono bologna nella l9 non 18, 19, 20... e mi va bene
043900040216     c* nell'elaborazione per terminal ma nell'elaborazione per linea le
044000040216     c* devo avere
044100040216     C                   CLEAR                   L9L
044200040216     C                   Z-ADD     *ZERO         WB                3 0          DI COMODO
044300040216     C     1             DO        30            WPT               3 0          DI COMODO PER POSTE
044400040216     C     LIN(WPT)      IFNE      0
044500040216     C                   CLEAR                   FNLSC4
044600040216     C                   Z-ADD     LIN(WPT)      IC4LIN                         LINEA
044700040216     C                   MOVE      'N'           IC4FLG                         B=BATCH
044800040216     C                   MOVEL     FNLSC4        KPJBU
044900040216     C                   CALL      'FNLSC4R1'
045000040216     C                   PARM                    KPJBA
045100050802     C                   PARM                    ds1ada
045200040216     C                   MOVEL     KPJBU         FNLSC4
045300040216     C     OC4ESE        IFEQ      'S'
045400040216     C                   ADD       1             WB
045500040216     C                   MOVE      LIN(WPT)      L9L(WB)
045600040216     C                   ENDIF
045700040216     C                   ENDIF
045800040216     C                   ENDDO
045900020212     C*
046000020212     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
046100020212     C*
046200020212     C* Carico la tabella 3L per avere le elaborazioni giornaliere
046300020212     C* Va caricata sempre, perche' almeno il tipo modulo serve anche quando
046400020212     C* la stampa e' lanciata da menu
046500020212     C                   MOVEL     '3L'          KCOD                           COD.TABELLA
046600020212     C                   MOVEL     DS1PGM        KKEY
046700020212     C                   MOVE      DS1FPG        KKEY                           COD.ELEMENTO
046800020212     C     KTAB1         CHAIN     TABEL00F                           30
046900020212     C  N30              MOVEL     TBLUNI        DS3L
047000020212     C   30              MOVEL     'F'           §3LD1L
047100020212     C   30              MOVEL     ' '           §3LD2L
047200020212     C   30              MOVEL     '*STD    '    §3LDST
047300020212     C*
047400020212     C* Imposto il tipo modulo (servira' per OVRPRTF)
047500020212     C                   MOVEL     §3LDST        £MOD             10            TIPO MODULO
047600020212     C                   CLEAR                   LIN
047700040216     C                   CLEAR                   LIL
047800020212     C*
047900020212     C* Verifico chi ha lanciato la stampa
048000020212     C* DS1FGS e' filiale richiedente (da dove sono lanciate le stampe)
048100020212     C* DS1FGS=0   --->  si proviene dalle elaborazioni giornaliere
048200020212     C* DS1FGS<>0  --->  si proviene da menu
048300020212     C     DS1FGS        IFEQ      0                                            ELAB.GIORNALIER
048400020212     C*
048500020212     C* Se DS1FGS=0, (elab.giornal.), DS1LNP=filiale per cui io, 1°lvl che
048600020212     C* lancio, voglio la stampa;  invece
048700020212     C* se DS1FGS<>0, (da menu), DS1LNP =linea da elaborare (da stampare)
048800020212     C*
048900020212     C* Se DS1LNP (fil per cui voglio stampa)=SIMFEL, significa che devo
049000020212     C* creare stampa per 1°lvl. (se stessa)
049100020212     C     DS1LNP        IFEQ      SIMFEL
049200020212     C*
049300020212     C* Il flag §3LD1L ha i seguenti significati:
049400020212     C*    ' ' = tutti i dati
049500020212     C*    '1' = solo filiale di 1' livello
049600020212     C     §3LD1L        IFEQ      '1'                                          '1'=SOLO 1' LIV
049700020212     C                   SETON                                        11
049800020212     C                   END
049900020212     C*
050000020212     C* La schiera LIN andra' a contenere tutti i cd.filiale da considerare
050100020212     C* nella stampa
050200020212     C*
050300020212     C* Se scelto tutte metto nella schiera LIN tutta la schiera L1
050400040206     C  N11              MOVEA     L9            LIN
050500040216     C  N11              MOVEA     L9L           LIL
050600020212     C   11              MOVEA     SIMFEL        LIN(1)
050700040220     C   11              MOVEA     SIMFEL        LIL(1)
050800020212     C*
050900020212     C* IMPOSTO LA STAMPANTE:
051000020212     C* STAMPANTE TABULATI
051100020212     C                   MOVEL     '5P'          KCOD
051200020212     C                   MOVEL     'T'           W004A             4
051300020212     C                   MOVE      LIN(1)        W004A
051400020212     C                   MOVEL(P)  W004A         KKEY
051500020212     C     KTAB1         CHAIN     TABEL00F                           99
051600020212     C     *IN99         IFEQ      *OFF
051700020212     C                   MOVEL     TBLUNI        DS5P
051800020212     C                   MOVEL     §5POTQ        £OUTQ            10            CODA DI STAMPA
051900020212     C                   END
052000020212     C*
052100020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
052200020212     C                   EXSR      LEGGI
052300020212     C*
052400020212     C* altrimenti...se elaborazione giornaliera chiesta per fil 2°lvl
052500020212     C* DS1LNP<>SIMFEL ---> stampa per i 2' Livelli
052600020212     C                   ELSE
052700020212     C*
052800020212     C* Se devo stampare per piu' 2°lvl, sottometto comunque
052900020212     C* un unico lavoro batch, che crea una stampa per ognuna di esse
053000020212     C*
053100020212     C* Il significato del fla §3LD2L e' il seguente:
053200020212     C*    ' ' = tutti i dati
053300020212     C*    'F' = per ogni 2' livello
053400020212     C     §3LD2L        IFEQ      'F'
053500020212     C                   SETON                                        14
053600020212     C                   ENDIF
053700020212     C*
053800020212     C                   Z-ADD     1             WB
053900020212     C                   MOVEA     *ZEROS        LIN
054000040206     C  N14              MOVEA     L9            LIN
054100040216     C  N14              MOVEA     L9L           LIL
054200020212     C*
054300020212     C* Lancio per tutti i 2° lvl
054400020212     C     DS1LNP        IFEQ      0
054500020212     C*
054600020212     C* LV2 contiene tutte le filiali di 2° lvl
054700020212     C     LV2(WB)       DOWNE     0
054800020212      *
054900020212      * .. CONTROLLO FLAGS TABELLA 3L E CONDIZIONO ELABORAZIONE
055000020212     C                   CLEAR                   FNLSC4
055100020212     C                   Z-ADD     LV2(WB)       IC4LIN                         LINEA
055200020212     C                   MOVEL     FNLSC4        KPJBU
055300020212     C                   CALL      'FNLSC4R1'
055400020212     C                   PARM                    KPJBA
055500050802     C                   PARM                    ds1ada
055600020212     C                   MOVEL     KPJBU         FNLSC4
055700020212      *
055800020212     C     OC4PT1        IFEQ      'S'
055900020212     C     §3LPT         ANDEQ     'N'
056000020212     C                   GOTO      NOLEGG
056100020212     C                   ENDIF
056200020212      *
056300020212     C     OC4EXP        IFEQ      'S'
056400020212     C     §3LEXP        ANDEQ     'N'
056500020212     C                   GOTO      NOLEGG
056600020212     C                   ENDIF
056700020212      *
056800020212     C     OC4DPD        IFEQ      'S'
056900020212     C     §3LDPD        ANDEQ     'N'
057000020212     C                   GOTO      NOLEGG
057100020212     C                   ENDIF
057200020212      *
057300020212     C     OC4BAR        IFEQ      'S'
057400020212     C     §3LBAR        ANDEQ     'N'
057500020212     C                   GOTO      NOLEGG
057600020212     C                   ENDIF
057700020212      *
057800020212     C     OC4FED        IFEQ      'S'
057900020212     C     §3LFED        ANDEQ     'N'
058000020212     C                   GOTO      NOLEGG
058100020212     C                   ENDIF
058200020212      *
058300020212     C*  14=ON, quando ad ogni 2°lvl do solo i suoi dati (invece che anche i
058400020212     C*  dati degli altri 2° lvl
058500020212     C*  LV2,WB=filiale per cui sto elaborando (richiedente)
058600020212     C   14              MOVEL     LV2(WB)       LIN(1)
058700040220     C   14              MOVEL     LV2(WB)       LIL(1)
058800020212     C*
058900020212     C* IMPOSTO LA STAMPANTE
059000020212     C* STAMPANTE TABULATI
059100020212     C                   MOVEL     '5P'          KCOD
059200020212     C                   MOVEL     'T'           W004A             4
059300020212     C                   MOVE      LV2(WB)       W004A
059400020212     C                   MOVEL(P)  W004A         KKEY
059500020212     C     KTAB1         CHAIN     TABEL00F                           99
059600020212     C     *IN99         IFEQ      *OFF
059700020212     C                   MOVEL     TBLUNI        DS5P
059800020212     C                   MOVEL     §5POTQ        £OUTQ            10            CODA DI STAMPA
059900020212     C                   END
060000020212     C*
060100020212     C* Compongo il modulo di stampa con il codice della filiale
060200020212     C*****                MOVE LV2,WB    £MOD             TIPO MODULO
060300020212     C*
060400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
060500020212     C                   EXSR      LEGGI
060600020212     C*
060700020212     C     NOLEGG        TAG
060800020212     C*
060900020212     C                   ADD       1             WB
061000020212     C                   ENDDO
061100020212     C*
061200020212     C* altrimenti...se DS1LNP<>0, lancio solo per una 2°lvl
061300020212     C                   ELSE
061400020212     C*
061500020212     C* Lancio solo per la 2° lvl richiesta
061600020212     C   14              MOVEL     DS1LNP        LIN(1)
061700040220     C   14              MOVEL     DS1LNP        LIL(1)
061800020212     C*
061900020212     C* STAMPANTE
062000020212     C* STAMPANTE TABULATI
062100020212     C                   MOVEL     '5P'          KCOD
062200020212     C                   MOVEL     'T'           W004A             4
062300020212     C                   MOVE      DS1LNP        W004A
062400020212     C                   MOVEL(P)  W004A         KKEY
062500020212     C     KTAB1         CHAIN     TABEL00F                           99
062600020212     C     *IN99         IFEQ      *OFF
062700020212     C                   MOVEL     TBLUNI        DS5P
062800020212     C                   MOVEL     §5POTQ        £OUTQ            10            CODA DI STAMPA
062900020212     C                   END
063000020212     C*
063100020212     C* MODULO STAMPA
063200020212     C******               MOVE DS1LNP    £MOD
063300020212     C*
063400020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
063500020212     C                   EXSR      LEGGI
063600020212     C*
063700020212     C                   ENDIF
063800020212     C                   ENDIF
063900020212     C*
064000020212     C                   ELSE
064100020212     C*
064200020212     C* Se lancio da menu e linea da elaborare (stampare) e' 0, significa
064300020212     C* che le voglio tutte (se stessa ed i sui 2°lvl)
064400020212     C     DS1LNP        IFEQ      0
064500040206     C                   MOVEA     L9            LIN
064600040216     C                   MOVEA     L9L           LIL
064700020212     C* ...altrimenti elaboro solo se stessa
064800020212     C                   ELSE
064900020212     C                   MOVEL     DS1LNP        LIN(1)
065000040220     C                   MOVEL     DS1LNP        LIL(1)
065100020212     C                   ENDIF
065200020212     C*
065300020212     C* IMPOSTO IL TIPO MODULO ( PER I 2° LVL METTO IL COD.FILIALE)
065400020212     C* PER LANCI DA MENU NON CERCO SU TABEL LA STAMPANTE, MA
065500020212     C* PRENDO QUELLA PRESENTE IN KPJBA
065600020212     C     DS1FGS        IFEQ      SIMFEL
065700020212     C                   MOVEL     KQOTD         £OUTQ                          CODA DI STAMPA
065800030627     C**!!!              MOVEL     KQOTD         £OUTQF                         CODA DI STAMPA
065900020212     C                   ELSE
066000020212     C* STAMPANTE TABULATI
066100020212     C                   MOVEL     '5P'          KCOD
066200020212     C                   MOVEL     'T'           W004A             4
066300020212     C                   MOVE      DS1FGS        W004A
066400020212     C                   MOVEL(P)  W004A         KKEY
066500020212     C     KTAB1         CHAIN     TABEL00F                           99
066600020212     C     *IN99         IFEQ      *OFF
066700020212     C                   MOVEL     TBLUNI        DS5P
066800170220     C****               MOVEL     §5POTQ        £OUTQ            10            CODA DI STAMPA
066900020212     C                   END
067000020212     C******               MOVE DS1FGS    £MOD             TIPO MODULO
067100020212     C                   ENDIF
067200020212     C*
067300020212     C* Esecuzione subroutine di lettura/elaborazione per stampare
067400020212     C                   EXSR      LEGGI
067500020212     C                   ENDIF
067600020212     C*
067700020212     C                   ENDSR
067800000000      **************************************************************************
067900000000      * Routine primaria; viene letto l'archivio statistica e stampato
068000000000      * LEGGI viene richiamata diverse volte dalla procedure contenuta in
068100950214      * FNLSC4R ( /COPY )
068200000000      *
068300000000      * Breve prospetto riassuntivo
068400000000      *           IN SEDE         FILIALE FILTRO          FILIALE STAMPE GIOR.
068500000000      * LIN()        0                 piena                      piena
068600950214      * DS1FGS       0                 SIMFEL                       0
068700950214      * DS1FEL       ?                 SIMFEL                     SIMFEL
068800000000      **************************************************************************
068900000000     C     LEGGI         BEGSR
069000000000      *
069100980305     C* SE STAMPA PER TUTTI I TERMINAL DI ARRIVO VERIFICO SE DEVO
069200980305     C* AGGIUNGERE DEI TER-ARR SU ALTRO AS
069300040206     C***  £TFA          IFEQ      *ZEROS
069400040206     C**   DS1FGS        ANDGT     *ZEROS
069500040206     C***  DS1FL1        ANDEQ     'T'                                          'T'=TERMINAL
069600040206     C***  KCAE2         SETLL     AZCAE02L
069700040206     C***  KCAE2         READE     AZCAE02L                               99
069800040206     C***  *IN99         DOWEQ     *OFF
069900040206     C***  CAEATB        IFEQ      *BLANKS
070000040206     C***  DS1ADA        ANDGE     CAEDDE
070100040206     C***  DS1ADA        ANDLE     CAEDSC
070200040206     C***                Z-ADD     1             WA
070300040206     C***  CAETFE        LOOKUP    LIN(WA)                                99
070400040206     C**N99000           LOOKUP    LIN(WA)                                98
070500040206     C***98              Z-ADD     CAETFE        LIN(WA)
070600040206     C***                END
070700040206     C***  KCAE2         READE     AZCAE02L                               99
070800040206     C***                ENDDO
070900040206     C***                END
071000980305     C*
071100950214      * Se sono in sede (e provengo quindi per forza da filtro) riempo DS1FGS
071200040206     C***  SIMFEL        IFEQ      *ZERO
071300040206     C***                Z-ADD     999           DS1FGS
071400040206     C***                ENDIF
071500000000      *
071600950214      * DS1FGS = 0 VENGO DA STAMPE GIORNALIERE
071700950214     C     DS1FGS        COMP      *ZERO                                  20     ON=GIORNALIERE
071800940516      *
071900940527      * PARTICOLARITA':  SE SIMFEl e' un secondo livello in arrivo
072000971126      *                  (terminal arrivo >< terminal di part)
072100950214      *                  e siamo nelle stampe giornaliere (DS1FGS=0),
072200940516      * eseguo la stampa per 'L'=Linea invece che per 'T'=Terminal
072300950214     C     DS1FGS        IFEQ      *ZERO                                        STAMPE GIORNALIERE
072400971126     C                   CLEAR                   DSLV55
072500971126     C                   MOVEL     'A'           D55TPT
072600971126     C                   Z-ADD     SIMFEL        D55LIN
072700971126     C                   Z-ADD     DS1ADA        D55DRF
072800971126     C                   CALL      'FNLV55R'
072900971126     C                   PARM                    DSLV55
073000971126     C     D55ERR        IFEQ      *BLANKS
073100971126     C     SIMFEL        ANDNE     D55TFA                                       ELABOR. <> TFA
073200950216     C                   MOVE      'L'           DS1FL1
073300940516     C                   ENDIF
073400940516     C                   ENDIF
073500000000      *
073600000000      * Eseguo OVRPRTF sul file di stampa, solo se non si e' in sede
073700040206     C***  SIMFEL        IFEQ      *ZERO                                        0 = SEDE
073800040206     C***                MOVE      CMD1(1)       COMMAN
073900040206     C***                ELSE                                                   IN FILIALE
074000950215      *
074100950215      * IMPOSTAZIONE STAMPANTE E TIPO MODULO
074200950215     C                   MOVEA     £MOD          CMD(33)                        TIPO MODULO
074300170216     c                   if        £outq=*blanks
074400170220     c                   eval      £outq='*JOB     '
074500170216     c                   endif
074600950215     C                   MOVEA     £OUTQ         CMD(51)                        CODA DI STAMPA
074700950214     C                   MOVEL     *BLANKS       COMMAN           82
074800000000     C                   MOVEA     CMD           COMMAN
074900040206     C***                ENDIF
075000040206     c*
075100950214     C                   Z-ADD     82            LUNG             15 5
075200000000     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
075300000000     C                   PARM                    COMMAN
075400000000     C                   PARM                    LUNG
075500941228     C                   OPEN      FNLRE7P                                      PRINTER FILE
075600000000      *
075700000000      * Scrivo, in testata, le selezioni fatte a video
075800950404     C     DS1RIS        IFEQ      'S'                                          'S'=ELAB.+STAMPA
075900000000     C                   MOVEL     'STAMPA'      STRIP
076000000000     C                   ELSE
076100000000     C                   MOVEL     'RISTAMPA'    STRIP
076200000000      *
076300000000     C                   MOVE      *BLANKS       STRIP1
076400950313     C     DS1LNP        IFNE      *ZERO
076500950313     C                   MOVE      DS1LNP        STRIP1
076600960926     C                   MOVEL     'P.O.'        STRIP1
076700000000     C                   ELSE
076800000000      *
076900000000      * Solo se in sede
077000040206     C***  SIMFEL        IFEQ      *ZERO
077100040206     C***                MOVEL     'AREA'        STRIP1
077200040206     C***                MOVE      DS1FEL        STRIP1
077300040206     C***                ENDIF
077400000000      *
077500000000     C                   ENDIF
077600000000     C                   ENDIF
077700000000      *
077800000000     C                   SETOFF                                       34        NO TERMINAL
077900950216     C     DS1FL1        IFEQ      'L'                                          'L'=LINEA ARRIV
078000000000     C                   MOVE      'x LINEA'     STRIP
078100000000     C                   SETOFF                                       21
078200000000     C                   ENDIF
078300950216     C     DS1FL1        IFEQ      'T'                                          'T'=TERMINAL AR
078400000000     C                   MOVE      'x TERM.'     STRIP
078500000000     C                   SETON                                        21
078600000000     C     SIMFEL        LOOKUP    LIN                                    34     ON=TROVATA
078700000000     C                   ENDIF
078800000000      *
078900000000      * Riempo una schiera con le filiali elaboratore da stampare
079000040206     C***                EXSR      CRTELA                                       CREA LIN
079100000000      *
079200000000      * Ciclo per ogni filiale elaboratore da stampare
079300000000     C                   Z-ADD     *ZERO         $ALMEN            1 0          ALMENO UNO
079400040206     C***                Z-ADD     1             KK                3 0          CONTATORE
079500040206     C***  ELA(KK)       DOWGT     *ZERO
079600000000      *
079700000000      * Se sono in sede la schiera LIN e' vuota percio' la devo creare ora
079800040206     C                   CLEAR                   LI2
079900040206     C***  SIMFEL        IFEQ      *ZERO
080000040206     C***                EXSR      CRTLIN                                       CREA LIN
080100040206     C***                ELSE
080200040216     C     DS1FL1        IFEQ      'T'                                          'T'=TERMINAL AR
080300950214     C                   MOVEA     LIN           LI2
080400040216     c                   else
080500040216     C                   MOVEA     LIL           LI2
080600040216     c                   endif
080700040206     C***                ENDIF
080800000000      *
080900000000      * Stampo ogni filiale presente nella schiera LIN
081000000000     C                   Z-ADD     *ZERO         $ALME1            1 0          ALMENO UNO
081100000000     C                   Z-ADD     *ZERO         $C                3 0          CONTATORE LNA
081200000000     C                   Z-ADD     1             YY                3 0          CONTATORE
081300950214     C     LI2(YY)       DOWGT     *ZERO
081400000000      *
081500000000      * Con l'elemento della LIN carico la £6 con le filiali associate
081600950214     C                   EXSR      CARLL6                                       CARICA TAB. £6
081700000000      *
081800000000     C                   Z-ADD     *ZERO         $TFAPR                         PRECEDENTE
081900000000      *
082000000000      * Il primo prospetto (per terminal di partenza) deve uscire solo se
082100950216      * richiesta stampa per terminal (DS1FL1='T')
082200950216     C     DS1FL1        IFEQ      'T'                                          ? T E R M I N A L ?
082300000000      *
082400000000     C                   Z-ADD     1             XX                3 0          CONTATORE
082500950214     C     LL6(XX)       DOWGT     *ZERO
082600000000      *
082700000000      * ?  Prospetto  per  terminal  di  partenza  ?
082800000000     C                   SETON                                        31        FASE 1
082900000000     C                   SETON                                        41        STAMPA C/SERV
083000000000     C                   Z-ADD     *ZERO         $PRIMO            1 0          PRIMO GIRO
083100000000     C                   Z-ADD     *ZERO         $PRIM1            1 0          PRIMO GIRO
083200000000     C                   Z-ADD     *ZERO         $FILPR                         PRECEDENTE
083300000000     C                   Z-ADD     *ZERO         $$FILP                         PRECEDENTE
083400000000     C                   MOVE      *BLANKS       $TREPR                         PRECEDENTE
083500090929     C     KFISAR        SETLL     FISAR01L
083600090929     C     KFISAR        READE(N)  FISAR01L                               33
083700000000      *
083800950214      * Leggo tutti i terminal di partenza per LI2,YY
083900000000      * non devo testare le date del periodo in quanto sto leggendo un OPNQRYF
084000000000     C     *IN33         DOWEQ     *OFF
084100000000     C                   Z-ADD     SARTFP        $FILIA
084200000000     C                   EXSR      ELABOR
084300000000     C                   EXSR      MOVFP                                        ELABORAZIONE
084400090929     C     KFISAR        READE(N)  FISAR01L                               33
084500000000     C                   ENDDO                                                  IN33 = OFF
084600000000      *
084700000000      * Se ho fatto almeno un giro elaboro l'ultimo e vado di totali
084800000000     C     $PRIMO        IFNE      *ZERO
084900000000     C                   Z-ADD     *ZERO         $FILIA
085000090929     C                   CLEAR                   FISAR000
085100000000     C                   EXSR      ELABOR
085200000000     C                   EXSR      TOTALI                                       STAMPA TOTALI
085300941228     C                   WRITE     LRE7E3                                       N.B.
085400000000     C                   ENDIF
085500000000      *
085600000000      * ?  Prospetto  per  linea  di  arrivo  ?
085700000000     C                   EXSR      PROSP2                                       2° PROSPETTO
085800000000      *
085900000000     C                   ADD       1             XX
086000000000     C                   ENDDO                                                  IN33 = OFF
086100000000      *
086200000000      * Se a fine elaborazione non ho stampato nulla provo con le linee
086300040209     C***  $ALMEN        IFEQ      *ZERO
086400040209     C***                Z-ADD     *ZERO         £TFA                           TERMINAL ARRIVO
086500040209     C***                SETOFF                                       21
086600040209     C***                EXSR      LINEA
086700040209     C***                ENDIF
086800000000      *
086900950216     C                   ENDIF                                                  DS1FL1 = 'T'
087000000000      *
087100000000      * Se richiesto 'L'=Linea devo stampare solo il secondo prospetto
087200950216     C     DS1FL1        IFEQ      'L'                                          ? L I N E A ?
087300000000     C                   EXSR      LINEA
087400950216     C                   ENDIF                                                  DS1FL1 = 'L'
087500000000      *
087600000000     C                   ADD       1             YY
087700000000     C                   ENDDO                                                  IN33 = OFF
087800040209      * Richiesta stampa per terminal (DS1FL1='T')
087900040209      * e a fine elaborazione non ho stampato nulla provo con le linee
088000040209    1C     DS1FL1        IFEQ      'T'                                          ? T E R M I N A L ?
088100040209     c     $almen        andeq     0
088200040216     c                   clear                   li2
088300040216     C                   MOVEA     LIL           LI2
088400040209      * Stampo ogni filiale presente nella schiera LIN
088500040209     C                   Z-ADD     1             YY                3 0          CONTATORE
088600040209    2C     LI2(YY)       DOWGT     *ZERO
088700040209      * Con l'elemento della LIN carico la £6 con le filiali associate
088800040209     C                   EXSR      CARLL6                                       CARICA TAB. £6
088900040209     C                   z-add     *zero         £tfa                           PRECEDENTE
089000040209     c                   setoff                                       21
089100040209     c                   exsr      linea
089200040209     C                   ADD       1             YY
089300040209    2C                   ENDDO                                                  IN33 = OFF
089400040209    1c                   endif
089500000000      *
089600000000     C     $ALME1        IFNE      *ZERO
089700000000     C                   EXSR      TOTALI                                       STAMPA TOTALI
089800941228     C                   WRITE     LRE7E3                                       N.B.
089900000000     C                   ENDIF
090000000000      *
090100040206     C***                ADD       1             KK
090200040206     C***                ENDDO                                                  IN33 = OFF
090300000000      *
090400000000      * Fine, se non stampato nulla emetto la dicitura
090500000000     C     $ALMEN        IFEQ      *ZERO
090600941228     C                   WRITE     LRE7T1                                       TESTATA
090700941228     C                   WRITE     LRE7R1                                       RIGA
090800941228     C                   WRITE     LRE7E2                                       NULLA
090900000000     C                   ENDIF
091000000000      *
091100941228     C                   CLOSE     FNLRE7P
091200000000      *
091300000000     C                   ENDSR
091400000000      **************************************************************************
091500000000      * Secondo prospetto: per linea di arrivo
091600000000      **************************************************************************
091700000000     C     PROSP2        BEGSR
091800000000      *
091900000000     C                   SETOFF                                       31        FASE 2
092000000000     C                   SETOFF                                       41        NO STAMPA C/SER
092100000000     C                   Z-ADD     *ZERO         $FILPR                         PRECEDENTE
092200000000     C                   Z-ADD     *ZERO         $$FILP                         PRECEDENTE
092300000000     C                   MOVE      *BLANKS       $TREPR                         PRECEDENTE
092400000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
092500000000     C                   Z-ADD     *ZERO         $PRIM1                         PRIMO GIRO
092600000000     C                   Z-ADD     *ZERO         $C                3 0          CONTATORE LNA
092700090929     C     KFISAR        SETLL     FISAR02L
092800090929     C     KFISAR        READE     FISAR02L                               33
092900000000      *
093000000000     C     *IN33         DOWEQ     *OFF
093100000000     C     SARTRE        IFNE      'R'                                          SOLO TERMINAL
093200000000     C                   Z-ADD     SARLNA        $FILIA
093300000000     C                   EXSR      ELABOR                                       ELABORAZIONE
093400000000     C                   ENDIF
093500000000     C                   EXSR      MOVFP                                        RIEMPI CAMPI
093600090929     C     KFISAR        READE     FISAR02L                               33
093700000000     C                   ENDDO                                                  IN33 = OFF
093800000000      *
093900000000     C     $PRIMO        IFNE      *ZERO
094000000000     C                   Z-ADD     *ZERO         $FILIA
094100090929     C                   CLEAR                   FISAR000
094200000000     C                   EXSR      ELABOR
094300000000     C                   EXSR      TOTALI                                       STAMPA TOTALI
094400941228     C                   WRITE     LRE7E3                                       N.B.
094500000000     C                   ENDIF
094600000000      *
094700000000     C                   ENDSR
094800000000      **************************************************************************
094900000000      * Richiesta stampa per 'L'=Linea di arrivo
095000000000      **************************************************************************
095100000000     C     LINEA         BEGSR
095200000000      *
095300000000     C                   SETOFF                                       31        FASE 2
095400000000     C                   SETOFF                                       41        NO STAMPA C/SER
095500000000     C                   Z-ADD     *ZERO         $FILPR                         PRECEDENTE
095600000000     C                   Z-ADD     *ZERO         $$FILP                         PRECEDENTE
095700000000     C                   MOVE      *BLANKS       $TREPR                         PRECEDENTE
095800000000     C                   Z-ADD     *ZERO         $PRIMO                         PRIMO GIRO
095900000000     C******               Z-ADD*ZERO     $PRIM1           PRIMO GIRO
096000000000     C                   Z-ADD     *ZERO         $C                3 0          CONTATORE LNA
096100000000      *
096200950214      * Considero solo le linee della schiera LL6
096300000000     C                   Z-ADD     1             XX                3 0          CONTATORE
096400950214     C     LL6(XX)       DOWGT     *ZERO
096500090929     C     KFISAR        SETLL     FISAR04L
096600090929     C     KFISAR        READE     FISAR04L                               33
096700000000      *
096800000000     C     *IN33         DOWEQ     *OFF
096900000000     C     SARTRE        IFNE      'R'                                          SOLO TERMINAL
097000970609     C* SE SARTFA = 0 SIGNIFICA CHE PER QUESTO RECORD TUTTI I DATI
097100970609     C* SONO = 0 E QUINDI LO SCARTO
097200970609     C     SARTFA        ANDGT     *ZEROS
097300000000     C                   Z-ADD     SARLNA        $FILIA
097400000000     C                   EXSR      ELABOR
097500000000     C                   EXSR      MOVFP                                        ELABORAZIONE
097600000000     C                   Z-ADD     1             $ALME1                         ALMENO UNO
097700000000     C                   ENDIF
097800090929     C     KFISAR        READE     FISAR04L                               33
097900000000     C                   ENDDO                                                  IN33 = OFF
098000000000      *
098100000000     C                   ADD       1             XX
098200000000     C                   ENDDO                                                  IN33 = OFF
098300000000      *
098400000000     C     $PRIMO        IFNE      *ZERO
098500000000     C                   Z-ADD     *ZERO         $FILIA
098600090929     C                   CLEAR                   FISAR000
098700000000     C                   EXSR      ELABOR
098800000000     C******               EXSR TOTALI                     STAMPA TOTALI
098900941228     C******               WRITELRE7E3                     N.B.
099000000000     C                   ENDIF
099100000000      *
099200000000     C                   ENDSR
099300000000      **************************************************************************
099400000000      * Riempo la schiera ELA con tutte le filiali con elaboratore da stampare
099500000000      **************************************************************************
099600040206     C***  CRTELA        BEGSR
099700000000      *
099800950214      * Se DS1FEL e' pieno devo stampare solo quella
099900040206     C***  DS1FEL        IFNE      *ZERO
100000040206     C***                Z-ADD     DS1FEL        ELA(1)
100100040206     C***                ELSE
100200000000      *
100300020419      * Altrimenti simfel
100400040206     C***                Z-ADD     simfel        ELA(1)
100500000000      *
100600040206     C***                ENDIF
100700000000      *
100800040206     C***                ENDSR
100900000000      **************************************************************************
101000000000      * Riempo la schiera LIN nel caso la stampa avvenga in sede
101100000000      **************************************************************************
101200040206     C***  CRTLIN        BEGSR
101300000000      *
101400000000      * Verifico se richiesta una singola filiale oppure un'are
101500040206     C***  DS1LNP        IFNE      *ZERO                                        SINGOLA FIL.
101600040206     C***                Z-ADD     DS1LNP        LI2(1)
101700000000      *
101800040206     C***                ELSE                                                   UN'AREA O TUTTO
101900000000      *
102000040206     C***                CLEAR                   DSUL06
102100040206     C***                MOVE      '£1'          D06COD
102200040206     C***                MOVEL     ELA(KK)       D06KEY
102300040206     C***                MOVEL     DSUL06        KPJBU
102400040206     C***                CALL      'TRUL06R'
102500040206     C***                PARM                    KPJBA
102600040206     C***                MOVEL     KPJBU         DSUL06
102700950214     C*
102800040206     C***                Z-ADD     *ZERO         WA
102900040206     C***                Z-ADD     *ZERO         WB
103000040206     C**N99              DO        29            WB
103100040206     C***  T06(WB)       IFNE      *ZEROS
103200000000      *
103300000000      * Nella schiera L1 ho tutte le filiali
103400000000      * Sono escluse quelle gestite da altre filiali
103500040206     C***  T06(WB)       LOOKUP    LI2                                    99
103600040206     C**N99T06(WB)       LOOKUP    L6                                     99
103700040206     C**N99              ADD       1             WA
103800040206     C**N99              Z-ADD     T06(WB)       LI2(WA)
103900040206     C***                ENDIF
104000040206     C***                ENDDO
104100000000      *
104200040206     C***                ENDIF                                                  DS1LNP <> 0
104300000000      *
104400040206     C***                ENDSR
104500000000      **************************************************************************
104600000000      * Riempe i campi di stampa letti dal file
104700000000      **************************************************************************
104800000000     C     MOVFP         BEGSR
104900000620     C**
105000000000     C     SARTRE        IFNE      'C'                                          C/SERVIZIO
105100000000     C     SARTRE        ANDNE     'R'                                          SOLO TERMINAL
105200000000     C                   ADD       SARCLL        CLLP
105300000000     C                   ADD       SARP88        P88P
105400000000     C                   ADD       SARDIS        DISP
105500000000     C                   ADD       SARPAR        PARP
105600000000     C                   ADD       SARPTA        PTAP
105700000000     C                   ADD       SARF35        F35P
105800000000     C                   ADD       SARO35        O35P
105900000000     C                   ADD       SARVOL        VOLP
106000000000     C                   ADD       SARSNF        SNFP
106100000000     C                   ADD       SARSNA        SNAP
106200000000     C                   ADD       SARSEF        SEFP
106300000000     C                   ADD       SARSEA        SEAP
106400090929     C                   ADD       SARSHF        SHFP
106500090929     C                   ADD       SARSHA        SHAP
106600000000      *
106700000620      * Totali per singola filiale
106800000000     C                   ADD       SARCLL        TOTCLL
106900000000     C                   ADD       SARP88        TOTP88
107000000000     C                   ADD       SARDIS        TOTDIS
107100000000     C                   ADD       SARPAR        TOTPAR
107200000000     C                   ADD       SARPTA        TOTPTA
107300000000     C                   ADD       SARF35        TOTF35
107400000000     C                   ADD       SARO35        TOTO35
107500000000     C                   ADD       SARVOL        TOTVOL
107600000000     C                   ADD       SARSNF        TOTSNF
107700000000     C                   ADD       SARSNA        TOTSNA
107800000000     C                   ADD       SARSEF        TOTSEF
107900000000     C                   ADD       SARSEA        TOTSEA
108000090929     C                   ADD       SARSHF        TOTSHF
108100090929     C                   ADD       SARSHA        TOTSHA
108200000619     C                   ENDIF
108300000000      *
108400000620      * Mantengo aggiornati i totali (con indice 3 sono totali PUNTO OP)
108500000000     C     SARTRE        IFEQ      *BLANKS
108600000000     C     SARTRE        OREQ      'C'                                          C/SERVIZIO
108700000000     C                   ADD       SARCLL        CLL(3)
108800000000     C                   ADD       SARP88        P88(3)
108900000000     C                   ADD       SARDIS        DIS(3)
109000000000     C                   ADD       SARPAR        PAR(3)
109100000000     C                   ADD       SARPTA        PTA(3)
109200000000     C                   ADD       SARF35        F35(3)
109300000000     C                   ADD       SARO35        O35(3)
109400000000     C                   ADD       SARVOL        VOL(3)
109500000000     C                   ADD       SARSNF        SNF(3)
109600000000     C                   ADD       SARSNA        SNA(3)
109700000000     C                   ADD       SARSEF        SEF(3)
109800000000     C                   ADD       SARSEA        SEA(3)
109900090929     C                   ADD       SARSHF        SHF(3)
110000090929     C                   ADD       SARSHA        SHA(3)
110100000000     C                   ENDIF
110200000000      *
110300000000      * Totali transiti (con indice 2)
110400000000     C     SARTRE        IFEQ      'T'
110500000000     C                   ADD       SARCLL        CLL(1)
110600000000     C                   ADD       SARP88        P88(1)
110700000000     C                   ADD       SARDIS        DIS(1)
110800000000     C                   ADD       SARPAR        PAR(1)
110900000000     C                   ADD       SARPTA        PTA(1)
111000000000     C                   ADD       SARF35        F35(1)
111100000000     C                   ADD       SARO35        O35(1)
111200000000     C                   ADD       SARVOL        VOL(1)
111300000000     C                   ADD       SARSNF        SNF(1)
111400000000     C                   ADD       SARSNA        SNA(1)
111500000000     C                   ADD       SARSEF        SEF(1)
111600000000     C                   ADD       SARSEA        SEA(1)
111700090929     C                   ADD       SARSHF        SHF(1)
111800090929     C                   ADD       SARSHA        SHA(1)
111900000000     C                   ENDIF
112000000000      *
112100000000      * Totali conto servizi (con indice 1)
112200000000     C     SARTRE        IFEQ      'C'
112300000000     C                   ADD       SARCLL        CLL(2)
112400000000     C                   ADD       SARP88        P88(2)
112500000000     C                   ADD       SARDIS        DIS(2)
112600000000     C                   ADD       SARPAR        PAR(2)
112700000000     C                   ADD       SARPTA        PTA(2)
112800000000     C                   ADD       SARF35        F35(2)
112900000000     C                   ADD       SARO35        O35(2)
113000000000     C                   ADD       SARVOL        VOL(2)
113100000000     C                   ADD       SARSNF        SNF(2)
113200000000     C                   ADD       SARSNA        SNA(2)
113300000000     C                   ADD       SARSEF        SEF(2)
113400000000     C                   ADD       SARSEA        SEA(2)
113500090929     C                   ADD       SARSHF        SHF(2)
113600090929     C                   ADD       SARSHA        SHA(2)
113700000000     C                   ENDIF
113800000000      *
113900000000      * Linee di arrivo di cui solo terminal
114000000000      * riempo delle schiere (con anche la LNA) per stamparle dopo,tutte assieme
114100000000      * Solo se terminal presente in LIN
114200000000     C     SARTFA        LOOKUP    LIN                                    99     ON=TROVATO
114300000000     C   99SARTRE        IFEQ      'R'
114400000000     C                   Z-ADD     1             A
114500000000     C     SARLNA        LOOKUP    LNAR(A)                                99     ON=TROVATO
114600000000     C  N99              ADD       1             $C
114700000000     C  N99              Z-ADD     $C            A
114800000000     C  N99              Z-ADD     SARLNA        LNAR(A)
114900000000     C                   ADD       SARCLL        CLLR(A)
115000000000     C                   ADD       SARP88        P88R(A)
115100000000     C                   ADD       SARPAR        PARR(A)
115200000000     C                   ENDIF
115300000000      *
115400000000     C                   ENDSR
115500000000      **************************************************************************
115600000000      * Calcolo i totali della riga da stampare
115700000000      **************************************************************************
115800000000     C     TOTRIG        BEGSR
115900000000      *
116000000000     C     F35P          ADD       O35P          TOTPEP                         TOT.PESO
116100090929     C     SNFP          ADD       SNAP          TOTNOP                         TOT. C+D
116200090929     C     SEFP          ADD       SEAP          TOTESP                         TOT. "E"
116300090929     C     SHFP          ADD       SHAP          TOTH10                         TOT. "H"
116400000000     C     TOTNOP        ADD       TOTESP        TOTTOP                         TOT.TOTALE
116500090929     C                   ADD       TOTH10        TOTTOP
116600090929     C     SNFP          ADD       SEFP          TOTFRP                         TOT. FRANCHI
116700090929     C                   ADD       SHFP          TOTFRP                         TOT. FRANCHI
116800090929     C     SNAP          ADD       SEAP          TOTASP                         TOT.ASSEGNATI
116900090929     C                   ADD       SHAP          TOTASP                         TOT.ASSEGNATI
117000000000      *
117100000000     C                   ENDSR
117200000000      **************************************************************************
117300000000      * Elaborazione del record letto dall'archivio statistiche
117400000000      **************************************************************************
117500000000     C     ELABOR        BEGSR
117600000622     C* INDCA CAMPO P.O. O TERMINAL
117700000622     C                   CLEAR                   WCAMB
117800000000      *
117900000000      * Al primo giro imposto i "precedenti"
118000000000     C     $PRIMO        IFEQ      *ZERO
118100000622     C                   EXSR      MEMO
118200000000     C                   Z-ADD     1             $PRIMO
118300000000     C                   ENDIF
118400000000      *
118500000622     C                   SETOFF                                       43
118600000000      *
118700000000      * Se cambia term.arrivo stampo la riga coi dati del "precedente"
118800000000      * In questo caso devo pure cambiare pagina
118900940516      * Solo se richiesta la stampa per terminal ('T')
119000000000     C     SARTFA        IFNE      $TFAPR                                       PRECEDENTE
119100000000     C     SARTFA        ANDNE     *ZERO                                        PRECEDENTE
119200950216     C     DS1FL1        ANDEQ     'T'                                          'T'=TERMINAL
119300000000     C                   SETON                                        43        PAGINA NUOVA
119400000622     C                   MOVEL     '1'           WCAMB             1
119500000000     C                   ENDIF
119600000000      *
119700000622      * Se cambia filiale O TRE STAMPO IL PRECEDENTE
119800000622     C     $FILPR        IFNE      999
119900000000     C     $FILIA        IFNE      $FILPR                                       PRECEDENTE
120000000622     C                   MOVEL     '1'           WCAMB             1
120100000000     C                   ENDIF
120200000622     C*
120300000622     C     SARTRE        IFNE      $TREPR                                       PRECEDENTE
120400000622     C     WCAMB         OREQ      '1'                                          PRECEDENTE
120500000622     C                   EXSR      CAMTRE
120600000622     C                   ENDIF
120700000622     C*
120800000622     C* SE COMUNQUE CAMBIATA LA FILIALE O TFA, STAMPO I TOTALI
120900000622     C*  E HO LETTO PIU' DI UN RECORD PER QUEL P.O.
121000000622     C     WCAMB         IFEQ      '1'                                          PRECEDENTE
121100000622     C     $TREPR        ANDNE     ' '
121200000622     C                   EXSR      TOTFIL
121300000622     C                   ENDIF
121400000622     C                   ENDIF
121500000622     C*
121600000000      *
121700000000      * Se ho acceso il 43 stampo i totali e cambio pagina
121800000622     C     *IN43         IFEQ      *ON
121900000000     C                   EXSR      TOTALI
122000000622     C                   WRITE     LRE7E3                                       N.B.
122100000622     C                   EXSR      MEMO
122200941228     C                   WRITE     LRE7T1                                       TESTATA (1)
122300090929     c                   if        not *in10
122400941228     C                   WRITE     LRE7R1                                       RIGA
122500941228     C                   WRITE     LRE7T2                                       TESTATA (2)
122600941228     C                   WRITE     LRE7R1                                       RIGA
122700090929     c                   else
122800090929     C                   WRITE     LRE7R1n                                      RIGA
122900090929     C                   WRITE     LRE7T2n                                      TESTATA (2)
123000090929     C                   WRITE     LRE7R1n                                      RIGA
123100090929     c                   endif
123200090929
123300000000     C                   SETOFF                                       30        OVERFLOW
123400000622     C                   ELSE
123500000622     C                   EXSR      MEMO
123600000622     C                   ENDIF
123700000000      *
123800000000     C                   ENDSR
123900000622      **************************************************************************
124000000622      * MEMORIZZO DATI STD DEL RECORD LETTO
124100000622      **************************************************************************
124200000622     C     MEMO          BEGSR
124300000622     C     SARTFA        IFGT      0
124400000622     C                   Z-ADD     SARTFA        $TFAPR
124500000622     C     $TFAPR        CHAIN     AZORG01L                           99
124600000622     C                   MOVE      *BLANKS       DETERP
124700000622     C  N99              MOVEL     ORGDES        DETERP
124800000622     C                   MOVE      SARTRE        $TREPR
124900000622     C                   Z-ADD     $FILIA        $FILPR
125000000622     C     WCAMB         IFEQ      '1'
125100000622     C                   CLEAR                   $TREPO
125200000622     C                   ENDIF
125300000622     C                   ENDIF
125400000622     C                   ENDSR
125500000622      **************************************************************************
125600000622      * CAMBIATO SARTRE
125700000622      **************************************************************************
125800000622     C     CAMTRE        BEGSR
125900000622     C* VERIFICO SE HO STAMPATO UN TRE=' '
126000000622     C*  SE NON L'HO STAMPO LO FACCIO ORA A ZERO
126100000622     C     $TREPO        IFEQ      ' '
126200000622     C     $TREPR        ANDNE     ' '
126300000622     C******     WCAMB     ANDEQ' '
126400000622      *
126500000622      * Se e' il primissimo giro e non ho ancora emesso l'intestazione...
126600000622     C     $PRIM1        IFEQ      *ZERO
126700000622     C                   WRITE     LRE7T1                                       TESTATA (1)
126800090929     c                   if        not *in10
126900000622     C                   WRITE     LRE7R1                                       RIGA
127000000622     C                   WRITE     LRE7T2                                       TESTATA (2)
127100000622     C                   WRITE     LRE7R1                                       RIGA
127200090929     c                   else
127300090929     C                   WRITE     LRE7R1n                                      RIGA
127400090929     C                   WRITE     LRE7T2n                                      TESTATA (2)
127500090929     C                   WRITE     LRE7R1n                                      RIGA
127600090929     c                   endif
127700090929     c
127800000622     C                   SETOFF                                       30        OVERFLOW
127900000622     C                   Z-ADD     1             $PRIM1
128000000622     C                   ENDIF
128100000622      *
128200000622     C                   MOVE      DIC(7)        DICITP                         ..DELLA FILIALE
128300000622     C                   MOVEL     $FILPR        DICITP                         FILIALE
128400090929     C                   WRITE     LRE7D2
128500000622     C*
128600000622     C                   MOVEL     'S'           $TREPO            1
128700000622     C                   ENDIF
128800000622     C*
128900000622     C* STAMPO IL RECORD PRECEDENTE
129000000622     C     $TREPR        IFEQ      'T'
129100000622     C                   MOVEL     DIC(4)        DICITP                         TRANSITI
129200000622     C                   MOVEL     $FILPR        DICITP                         FILIALE
129300000622     C                   SETON                                        4244
129400000622     C                   ENDIF
129500000622     C     $TREPR        IFEQ      'P'
129600000622     C                   MOVEL     DIC(8)        DICITP                         POSTE
129700000622     C                   MOVEL     $FILPR        DICITP                         FILIALE
129800000622     C                   SETON                                        4244
129900000622     C                   ENDIF
130000000622     C*
130100000622     C     $TREPR        IFEQ      ' '
130200000622     C                   MOVEL     'S'           $TREPO            1
130300000622     C* E QUELLO CHE LEGGO E' <>' ' E NON E' CAMBIATA LA FILIALE O TFA
130400000622     C     WCAMB         IFNE      '1'
130500000622     C     SARTRE        ANDNE     ' '
130600000622     C                   MOVEL     DIC(7)        DICITP                         P.O.
130700000622     C                   MOVEL     $FILPR        DICITP                         FILIALE
130800000622     C                   SETON                                        4244
130900000622     C                   ENDIF
131000000622     C                   ENDIF
131100000622     C*
131200000622     C                   Z-ADD     $FILPR        FILIAP
131300000622     C     FILIAP        IFNE      999
131400000622     C                   EXSR      SCRIVI
131500000622     C                   ENDIF
131600000622     C*
131700000622     C                   ENDSR
131800000000      **************************************************************************
131900000000      * Ho stampato anche i transiti quindi tiro le somme della filiale
132000000000      **************************************************************************
132100000000     C     TOTFIL        BEGSR
132200000000      *
132300000000     C                   Z-ADD     TOTCLL        CLLP
132400000000     C                   Z-ADD     TOTP88        P88P
132500000000     C                   Z-ADD     TOTDIS        DISP
132600000000     C                   Z-ADD     TOTPAR        PARP
132700000000     C                   Z-ADD     TOTPTA        PTAP
132800000000     C                   Z-ADD     TOTF35        F35P
132900000000     C                   Z-ADD     TOTO35        O35P
133000000000     C                   Z-ADD     TOTVOL        VOLP
133100000000     C                   Z-ADD     TOTSNF        SNFP
133200000000     C                   Z-ADD     TOTSNA        SNAP
133300000000     C                   Z-ADD     TOTSEF        SEFP
133400000000     C                   Z-ADD     TOTSEA        SEAP
133500090929     C                   Z-ADD     TOTShF        ShFP
133600090929     C                   Z-ADD     TOTShA        ShAP
133700000000     C                   SETOFF                                       4244
133800000000     C                   EXSR      SCRIVI
133900000000      *
134000000000     C                   ENDSR
134100000000      **************************************************************************
134200000000      * Testa l'overflow e stampa su carta
134300000000      **************************************************************************
134400000000     C     SCRIVI        BEGSR
134500000000      *
134600000000      * Se stampo una filiale la decodifico
134700000000     C                   MOVE      *BLANKS       ORGDEP
134800000000     C  N42              DO
134900000000     C     FILIAP        CHAIN     AZORG01L                           99
135000000000     C  N99              MOVE      ORGDES        ORGDEP                         DECODIFICA
135100000000     C                   ENDDO
135200000000      *
135300000000      * Se e' il primissimo giro e non ho ancora emesso l'intestazione...
135400000000      * oppure per overflow ...
135500000000     C     *IN30         IFEQ      *ON                                          OVERFLOW
135600000000     C     $PRIM1        OREQ      *ZERO                                        PRIMA EMISSIONE
135700941228     C                   WRITE     LRE7T1                                       TESTATA (1)
135800090929     c                   if        not *in10
135900941228     C                   WRITE     LRE7R1                                       RIGA
136000941228     C                   WRITE     LRE7T2                                       TESTATA (2)
136100941228     C                   WRITE     LRE7R1                                       RIGA
136200090929     c                   else
136300090929     C                   WRITE     LRE7R1n                                      RIGA
136400090929     C                   WRITE     LRE7T2n                                      TESTATA (2)
136500090929     C                   WRITE     LRE7R1n                                      RIGA
136600090929     c                   endif
136700000000     C                   SETOFF                                       30        OVERFLOW
136800000000     C                   Z-ADD     1             $PRIM1
136900000000     C                   ENDIF
137000000000      *
137100000000     C                   EXSR      TOTRIG                                       TOTALI RIGA
137200090929     C  n10              WRITE     LRE7D1                                       DETTAGLIO
137300090929     C   10              WRITE     LRE7D1n                                      DETTAGLIO
137400000000     C                   Z-ADD     1             $ALMEN
137500000000      *
137600000000     C                   Z-ADD     *ZERO         CLLP
137700000000     C                   Z-ADD     *ZERO         P88P
137800000000     C                   Z-ADD     *ZERO         DISP
137900000000     C                   Z-ADD     *ZERO         PARP
138000000000     C                   Z-ADD     *ZERO         PTAP
138100000000     C                   Z-ADD     *ZERO         F35P
138200000000     C                   Z-ADD     *ZERO         O35P
138300000000     C                   Z-ADD     *ZERO         VOLP
138400000000     C                   Z-ADD     *ZERO         SNFP
138500000000     C                   Z-ADD     *ZERO         SNAP
138600000000     C                   Z-ADD     *ZERO         SEFP
138700000000     C                   Z-ADD     *ZERO         SEAP
138800090929     C                   Z-ADD     *ZERO         ShFP
138900090929     C                   Z-ADD     *ZERO         ShAP
139000000000      *
139100000000      * Azzero i  totali di filiale della singola filiale
139200000000     C  N42              DO
139300000000     C                   Z-ADD     *ZERO         TOTCLL            7 0
139400000000     C                   Z-ADD     *ZERO         TOTP88            7 0
139500000000     C                   Z-ADD     *ZERO         TOTDIS            7 0
139600000000     C                   Z-ADD     *ZERO         TOTPAR            7 0
139700000000     C                   Z-ADD     *ZERO         TOTPTA            9 1
139800000000     C                   Z-ADD     *ZERO         TOTF35            9 1
139900000000     C                   Z-ADD     *ZERO         TOTO35            9 1
140000000000     C                   Z-ADD     *ZERO         TOTVOL            9 4
140100000000     C                   Z-ADD     *ZERO         TOTSNF            7 0
140200000000     C                   Z-ADD     *ZERO         TOTSNA            7 0
140300000000     C                   Z-ADD     *ZERO         TOTSEF            7 0
140400000000     C                   Z-ADD     *ZERO         TOTSEA            7 0
140500090929     C                   Z-ADD     *ZERO         TOTShF            7 0
140600090929     C                   Z-ADD     *ZERO         TOTShA            7 0
140700000000     C                   ENDDO
140800000000     C                   Z-ADD     FILIAP        $$FILP
140900000000     C                   SETOFF                                       4244
141000000000      *
141100000000     C                   ENDSR
141200000000      **************************************************************************
141300000000      * Stampo i  totali della stampa
141400000000      **************************************************************************
141500000000     C     TOTALI        BEGSR
141600000000      *
141700000000     C                   Z-ADD     *ZERO         FILIAP
141800000000      *
141900000000      *?    T O T A L I    C O N T O    S E R V I Z I    ?
142000000000      * Prima dei totali veri e propri devo stampare i conto servizi
142100000000      * solo per il dettaglio per Terminal di partenza
142200000000     C     *IN41         IFEQ      *ON
142300000000     C     CLL(2)        IFGT      *ZERO
142400000000     C     SNF(2)        ORGT      *ZERO
142500000000     C     SNA(2)        ORGT      *ZERO
142600000000     C     SEF(2)        ORGT      *ZERO
142700000000     C     SEA(2)        ORGT      *ZERO
142800090929     C     ShF(2)        ORGT      *ZERO
142900090929     C     ShA(2)        ORGT      *ZERO
143000000000     C                   Z-ADD     CLL(2)        CLLP
143100000000     C                   Z-ADD     P88(2)        P88P
143200000000     C                   Z-ADD     DIS(2)        DISP
143300000000     C                   Z-ADD     PAR(2)        PARP
143400000000     C                   Z-ADD     PTA(2)        PTAP
143500000000     C                   Z-ADD     F35(2)        F35P
143600000000     C                   Z-ADD     O35(2)        O35P
143700000000     C                   Z-ADD     VOL(2)        VOLP
143800000000     C                   Z-ADD     SNF(2)        SNFP
143900000000     C                   Z-ADD     SNA(2)        SNAP
144000000000     C                   Z-ADD     SEF(2)        SEFP
144100000000     C                   Z-ADD     SEA(2)        SEAP
144200090929     C                   Z-ADD     ShF(2)        ShFP
144300090929     C                   Z-ADD     ShA(2)        ShAP
144400000000     C                   MOVE      DIC(1)        DICITP                         C/SERVIZI
144500000000     C                   SETON                                        4244
144600000000     C                   EXSR      SCRIVI
144700000000     C                   ENDIF
144800000000     C                   ENDIF
144900000000      *
145000090929     C  N10              WRITE     LRE7R1                                       RIGA
145100090929     C   10              WRITE     LRE7R1N                                      RIGA
145200000000     C                   SETOFF                                       44
145300000000      *
145400000000      *?    T O T A L I    D E L L A    F I L I A L E    ?
145500000000     C                   Z-ADD     CLL(3)        CLLP
145600000000     C                   Z-ADD     P88(3)        P88P
145700000000     C                   Z-ADD     DIS(3)        DISP
145800000000     C                   Z-ADD     PAR(3)        PARP
145900000000     C                   Z-ADD     PTA(3)        PTAP
146000000000     C                   Z-ADD     F35(3)        F35P
146100000000     C                   Z-ADD     O35(3)        O35P
146200000000     C                   Z-ADD     VOL(3)        VOLP
146300000000     C                   Z-ADD     SNF(3)        SNFP
146400000000     C                   Z-ADD     SNA(3)        SNAP
146500000000     C                   Z-ADD     SEF(3)        SEFP
146600000000     C                   Z-ADD     SEA(3)        SEAP
146700090929     C                   Z-ADD     ShF(3)        ShFP
146800090929     C                   Z-ADD     ShA(3)        ShAP
146900000622     C     CLL(4)        IFGT      0
147000000000     C                   MOVE      DIC(3)        DICITP                         DELLA FILIALE
147100000622     C                   ELSE
147200000622     C                   MOVE      DIC(7)        DICITP                         DELLA FILIALE
147300000622     C                   ENDIF
147400000000     C                   SETON                                        42
147500000000     C                   SETOFF                                       44        SALTA 1 RIGA DOPO
147600000000     C                   EXSR      SCRIVI
147700000000      *
147800000000      *?    T O T A L I    T R A N S I T I    ?
147900000622     C     CLL(1)        IFGT      0
148000000000     C                   Z-ADD     CLL(1)        CLLP
148100000000     C                   Z-ADD     P88(1)        P88P
148200000000     C                   Z-ADD     DIS(1)        DISP
148300000000     C                   Z-ADD     PAR(1)        PARP
148400000000     C                   Z-ADD     PTA(1)        PTAP
148500000000     C                   Z-ADD     F35(1)        F35P
148600000000     C                   Z-ADD     O35(1)        O35P
148700000000     C                   Z-ADD     VOL(1)        VOLP
148800000000     C                   Z-ADD     SNF(1)        SNFP
148900000000     C                   Z-ADD     SNA(1)        SNAP
149000000000     C                   Z-ADD     SEF(1)        SEFP
149100000000     C                   Z-ADD     SEA(1)        SEAP
149200090929     C                   Z-ADD     ShF(1)        ShFP
149300090929     C                   Z-ADD     ShA(1)        ShAP
149400000000     C                   MOVE      DIC(4)        DICITP                         TRANSITI
149500000000     C                   SETON                                        42
149600000000     C                   SETOFF                                       44        SALTA 1 RIGA DOPO
149700000000     C                   EXSR      SCRIVI
149800000622     C                   ENDIF
149900000622     C* SOLO SE HO DATI POSTE E DATI TRANSITO
150000090929     C**   CLL(4)        IFGT      0
150100090929     C**   CLL(1)        IFGT      0
150200000000      *
150300000620      *?    T O T A L I    SENZA POSTE        (compresi i transiti)    ?
150400090929     C**   CLL(1)        ADD       CLL(3)        CLLP
150500090929     C**   P88(1)        ADD       P88(3)        P88P
150600090929     C**   DIS(1)        ADD       DIS(3)        DISP
150700090929     C**   PAR(1)        ADD       PAR(3)        PARP
150800090929     C**   PTA(1)        ADD       PTA(3)        PTAP
150900090929     C**   F35(1)        ADD       F35(3)        F35P
151000090929     C**   O35(1)        ADD       O35(3)        O35P
151100090929     C**   VOL(1)        ADD       VOL(3)        VOLP
151200090929     C*+   SNF(1)        ADD       SNF(3)        SNFP
151300090929     C**   SNA(1)        ADD       SNA(3)        SNAP
151400090929     C**   SEF(1)        ADD       SEF(3)        SEFP
151500090929     C**   SEA(1)        ADD       SEA(3)        SEAP
151600090929     C**   ShF(1)        ADD       ShF(3)        ShFP
151700090929     C**   ShA(1)        ADD       ShA(3)        ShAP
151800090929     C*+                 MOVE      DIC(6)        DICITP                         TOT.GENERALE
151900090929     C**                 SETON                                        42
152000090929     C**                 SETOFF                                       44        SALTA 1 RIGA DOPO
152100090929     C**                 EXSR      SCRIVI
152200090929     C*+                 ENDIF
152300000620      *
152400000620      *?    T O T A L I   P O S T E                                    ?
152500090929     C**                 Z-ADD     CLL(4)        CLLP
152600090929     C**                 Z-ADD     P88(4)        P88P
152700090929     C**                 Z-ADD     DIS(4)        DISP
152800090929     C**                 Z-ADD     PAR(4)        PARP
152900090929     C**                 Z-ADD     PTA(4)        PTAP
153000090929     C**                 Z-ADD     F35(4)        F35P
153100090929     C**                 Z-ADD     O35(4)        O35P
153200090929     C*+                 Z-ADD     VOL(4)        VOLP
153300090929     C**                 Z-ADD     SNF(4)        SNFP
153400090929     C*+                 Z-ADD     SNA(4)        SNAP
153500090929     C*+                 Z-ADD     SEF(4)        SEFP
153600090929     C**                 Z-ADD     SEA(4)        SEAP
153700090929     C**                 MOVE      DIC(5)        DICITP                         TOT.GENERALE
153800090929     C*+                 SETON                                        4244
153900090929     C*+                 EXSR      SCRIVI
154000000620      *
154100090929     C**                 WRITE     LRE7R1                                       RIGA
154200090929     C**                 ENDIF
154300000620      *
154400000620      *?    T O T A L I    G E N E R A L I    (compresi i transiti)    ?
154500000620     C     CLL(1)        ADD       CLL(3)        CLLP
154600000620     C     P88(1)        ADD       P88(3)        P88P
154700000620     C     DIS(1)        ADD       DIS(3)        DISP
154800000620     C     PAR(1)        ADD       PAR(3)        PARP
154900000620     C     PTA(1)        ADD       PTA(3)        PTAP
155000000620     C     F35(1)        ADD       F35(3)        F35P
155100000620     C     O35(1)        ADD       O35(3)        O35P
155200000620     C     VOL(1)        ADD       VOL(3)        VOLP
155300000620     C     SNF(1)        ADD       SNF(3)        SNFP
155400000620     C     SNA(1)        ADD       SNA(3)        SNAP
155500000620     C     SEF(1)        ADD       SEF(3)        SEFP
155600000620     C     SEA(1)        ADD       SEA(3)        SEAP
155700090929     C     SHF(1)        ADD       SHF(3)        SHFP
155800090929     C     SHA(1)        ADD       SHA(3)        SHAP
155900000620     C                   MOVE      DIC(2)        DICITP                         TOT.GENERALE
156000000620     C                   SETON                                        4244
156100000620     C                   EXSR      SCRIVI
156200000620      *
156300090929     C  N10              WRITE     LRE7R1                                       RIGA
156400090929     C   10              WRITE     LRE7R1N                                      RIGA
156500000000      *
156600950314      * Se sono nella seconda fase devo stampare il prospetto delle linee di
156700000000      * arrivo di cui solo terminal di arrivo
156800950216     C  N31DS1FL1        IFEQ      'T'
156900000000     C                   EXSR      SOLOT                                        SOLO TERMINAL
157000000000     C                   ENDIF
157100000000      *
157200000000     C                   EXSR      AZZER                                        AZZERAMENTI
157300000000      *
157400000000     C                   ENDSR
157500000000      **************************************************************************
157600000000      * Pulizia schiere
157700000000      **************************************************************************
157800000000     C     AZZER         BEGSR
157900000000      *
158000000000     C                   CLEAR                   CLL
158100000000     C                   CLEAR                   P88
158200000000     C                   CLEAR                   DIS
158300000000     C                   CLEAR                   PAR
158400000000     C                   CLEAR                   PTA
158500000000     C                   CLEAR                   F35
158600000000     C                   CLEAR                   O35
158700000000     C                   CLEAR                   VOL
158800000000     C                   CLEAR                   SNF
158900000000     C                   CLEAR                   SNA
159000000000     C                   CLEAR                   SEF
159100000000     C                   CLEAR                   SEA
159200090929     C                   CLEAR                   ShF
159300090929     C                   CLEAR                   ShA
159400000000      *
159500000000     C                   CLEAR                   LNAR
159600000000     C                   CLEAR                   CLLR
159700000000     C                   CLEAR                   P88R
159800000000     C                   CLEAR                   PARR
159900000000     C                   Z-ADD     *ZERO         $C                             CONTATORE
160000000000      *
160100000000     C                   ENDSR
160200000000      **************************************************************************
160300950214      * Carico la tabella £6 nella schiera LL6 (filiali associate)
160400000000      **************************************************************************
160500950214     C     CARLL6        BEGSR
160600000000      *
160700020419      * Per ogni linea presente in LIN carico la £6
160800020419     c                   clear                   dsul06
160900020419     c                   eval      d06cod = '£6'
161000020419     c                   movel     li2(yy)       d06key
161100050802     C                   MOVE      'S'           D06KEY
161200050802     C                   MOVEL     ds1ada        D06drf
161300020419     c                   movel     dsul06        kpjbu
161400020419     c                   call      'TRUL06R'
161500020419     c                   parm                    kpjba
161600020419     c                   movel     kpjbu         dsul06
161700020419     c                   movea     t06           ll6
161800000000      *
161900000000     C                   ENDSR
162000000000      **************************************************************************
162100950214      * Carico la tabella £6 nella schiera LL6 (filiali associate)
162200000000      **************************************************************************
162300000000     C     SOLOT         BEGSR
162400000000      *
162500000000      * Stampo tutta la schiera con le LNA di cui solo terminal
162600000000     C                   Z-ADD     *ZERO         $PRIM2            1 0          PRIMO GIRO
162700000000     C                   Z-ADD     1             A                 3 0          CONTATORE
162800000000     C     LNAR(A)       DOWNE     *ZERO
162900000000      *
163000000000      * Al primo giro stampo l'intestazione
163100000000     C     $PRIM2        IFEQ      *ZERO
163200941228     C   30              WRITE     LRE7T1                                       TESTATA (1)
163300941228     C   30              WRITE     LRE7R1                                       RIGA
163400000000     C   30              SETOFF                                       30        OVERFLOW
163500941228     C                   WRITE     LRE7T3                                       INTESTAZIONE
163600000000     C                   Z-ADD     1             $PRIM2
163700000000     C                   ENDIF
163800000000      *
163900000000     C                   Z-ADD     LNAR(A)       FILIAP                         FILIALE
164000000000     C     LNAR(A)       CHAIN     AZORG01L                           99
164100000000     C  N99              MOVEL     ORGDES        ORGDEP                         DESCRIZIONE
164200000000     C   99              MOVE      *BLANKS       ORGDEP
164300000000     C                   Z-ADD     P88R(A)       P88P
164400000000     C                   Z-ADD     PARR(A)       PARP
164500000000     C                   Z-ADD     CLLR(A)       CLLP                           NR.COLLI
164600000000      *
164700000000      * Stampo il dettaglio della riga
164800941228     C   30              WRITE     LRE7T1                                       TESTATA (1)
164900941228     C   30              WRITE     LRE7R1                                       RIGA
165000000000     C   30              SETOFF                                       30        OVERFLOW
165100941228     C                   WRITE     LRE7D3                                       DETTAGLIO
165200000000      *
165300000000      * Mantengo aggiornati i totali
165400000000     C                   ADD       P88R(A)       P88TO
165500000000     C                   ADD       PARR(A)       PARTO
165600000000     C                   ADD       CLLR(A)       CLLTO
165700000000      *
165800000000     C                   ADD       1             A                              CONTATORE
165900000000     C                   ENDDO
166000000000      *
166100000000      * Se stampata almeno una riga scrivi anche il totale
166200000000     C     $PRIM2        IFNE      *ZERO
166300941228     C                   WRITE     LRE7E4                                       TOTALE
166400000000     C                   Z-ADD     *ZERO         P88TO
166500000000     C                   Z-ADD     *ZERO         PARTO
166600000000     C                   Z-ADD     *ZERO         CLLTO
166700000000     C                   ENDIF
166800000000      *
166900000000     C                   ENDSR
167000000000      **************************************************************************
167100000620** Comando per eseguire QCMDEXC        01234567890123456789012345678901234567890
167200950214OVRPRTF FILE(FNLRE7P) FORMTYPE('          ') OUTQ(          ) USRDTA('Stat.arriv
167300950214')
167400000000** Comando per eseguire QCMDEXC
167500941228OVRPRTF FILE(FNLRE7P) USRDTA('Stat.arriv')
167600000000** Diciture da stampare
167700000000    CONTO SERVIZI
167800000000T O T A L I    G E N E R A L I
167900000620DEL PUNTO OPERATIVO (NO POSTE)
168000000000......... IN TRANSITO
168100000620............... POSTE
168200000620.................. T O T A L E
168300000620......... DEL PUNTO OPERATIVO
168400000620......... POSTE
