000100981026     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981026     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300981026     H/title         - FATTURAZIONE AUTOMATICA -FILTRO-              *
000400001115     H*                                                              *
000500001115     H* *!""!* modifiche di prova *!""!*                             *
000600000000     H*--------------------------------------------------------------*
000700980130     FTABEL00F  IF   E           K DISK
000800910111     FAZORG01L  IF   E           K DISK
000900981026     FAncln01L  IF   E           K DISK
001000981026     FAnsog01L  IF   E           K DISK
001100070201     FTipmg01l  IF   E           K DISK
001200101013     ftisis01l  if   e           k disk
001300991230     FTNSF01D   CF   E             WORKSTN
001400971022     D CFF             S              1    DIM(15)                              *TAB FREQUENZA FT
001500971022     D DFF             S             12    DIM(15)                              *TAB DESCR.FREQU
001600971022     D UFF             S             89    DIM(15)                              *TBLUNI DI FREQU
001700971022     D K12             S              1    DIM(12)                              COMODO
001800971022     D K55             S              1    DIM(55)                              COMODO
001900101013     D IAC             S              5  1 DIM(999)                             PERCENTUALI ISTAT
002000101013     D DIA             S              8  0 DIM(999)                             DATE SCATTI ISTAT
002100000217     D cm1             S             80    DIM(1) CTDATA PERRCD(1)              *clrpfm etlav
002200000000     D PARAM           DS
002300981026     D  VIDDFI                        8  0
002400981026     D  VIDDFF                        8  0
002500981026     D  VIDDSP                        8  0
002600981026     D  VIDTFT                        1
002700981026     D  VIDTCL                        1  0
002800981026     D  VIDTSC                        1
002900981026     D  FILx                          3  0 dim(40)
003000981026     D  VIDRB                         2
003100981026     D  VIDDFT                        8  0
003200981026     D  VIDKSC                        7  0
003300981026     D  wsocieta                      3
003400991230     D  VIDDIV                        3                                         *divisa da fatturare
003500991230     D  VIDAGG                        1                                         *AGG.bolle altre div
003600991230     D  VIDSTA                        1                                         *stampa manca tariff
003700000000     D FILDS           DS
003800891016     D  VIDF01                 1      3  0
003900891016     D  VIDF02                 4      6  0
004000891016     D  VIDF03                 7      9  0
004100891016     D  VIDF04                10     12  0
004200891016     D  VIDF05                13     15  0
004300891016     D  VIDF06                16     18  0
004400891016     D  VIDF07                19     21  0
004500891016     D  VIDF08                22     24  0
004600891016     D  VIDF09                25     27  0
004700891016     D  VIDF10                28     30  0
004800891016     D  VIDF11                31     33  0
004900891016     D  VIDF12                34     36  0
005000891016     D  VIDF13                37     39  0
005100891016     D  VIDF14                40     42  0
005200891016     D  VIDF15                43     45  0
005300891016     D  VIDF16                46     48  0
005400891016     D  VIDF17                49     51  0
005500891016     D  VIDF18                52     54  0
005600891016     D  VIDF19                55     57  0
005700891016     D  VIDF20                58     60  0
005800891016     D  VIDF21                61     63  0
005900891016     D  VIDF22                64     66  0
006000891016     D  VIDF23                67     69  0
006100891016     D  VIDF24                70     72  0
006200891016     D  VIDF25                73     75  0
006300891016     D  VIDF26                76     78  0
006400891016     D  VIDF27                79     81  0
006500891016     D  VIDF28                82     84  0
006600891016     D  VIDF29                85     87  0
006700891016     D  VIDF30                88     90  0
006800891016     D  VIDF31                91     93  0
006900891016     D  VIDF32                94     96  0
007000891016     D  VIDF33                97     99  0
007100891016     D  VIDF34               100    102  0
007200891016     D  VIDF35               103    105  0
007300891016     D  VIDF36               106    108  0
007400891016     D  VIDF37               109    111  0
007500891016     D  VIDF38               112    114  0
007600891016     D  VIDF39               115    117  0
007700891016     D  VIDF40               118    120  0
007800891016     D  FIL                    1    120  0
007900891016     D                                     DIM(40)                              FILIALI ESCLUSE
008000110127     d
008100110127     D PARAMSTF1       DS
008200110127     D  stf1dfi                       8  0
008300110127     D  stf1dff                       8  0
008400110127     d
008500110127      * DATA E ORA SCHEDULAZIONE PER KPJBA
008600110127     D SCHEDS          DS
008700110127     D  D1DATA                 1      6
008800110127     D  D1ORAS                 7     10
008900110127     d
009000941104     D DSQC2         E DS
009100951002     D*-------------------
009200951002     D* DS DATI VARI PER C/E
009300951002     D*-------------------
009400971022     D DSFF          E DS
009500971022     D  DGG                   31     40  0
009600971022     D                                     DIM(5)                               *GG FATTURAZ
009700991230     D*-------------------
009800991230     D DSCV          E DS                                                       tabella divise
009900031009     D*-------------------
010000031009     D TIBS02DS      E DS                                                       tabelle
010100031009     D DFAT          E DS                                                       tabella FAT
010200031009     D*-----------
010300000000     D KPJBA         E DS
010400071130     D*-----------
010500981026     D soc001        e ds                  extname(xsoc001ds)
010600981026     D xsocds          DS          1000
010700071203     D xchkautds     e ds
010800071130     D conpnods      e ds
010900071130     D*-----------
011000111104      * - Per manca tariffa
011100111104     d Tnsb44ds      e ds
011200111104
011300040622     d Err_libasp      s              1
011400040622     d Wtft            s             11
011500040622     d Wtext           s             50
011600040622     d W008a           s              8
011700110127     d datasys         s               d   datfmt(*iso) inz(*sys)
011800110127     d datadmy         s               d   datfmt(*dmy)
011900110127     d dataiso         s               d   datfmt(*iso)
012000981026     d dataeur         s               d   datfmt(*eur)
012100981026     D  XNMDAG         S               d   datfmt(*iso)
012200981026     d  XNMDAT         S               d   datfmt(*iso)
012300981026     d amgfat          S               d   datfmt(*iso)
012400981026     d amgddc          S               d   datfmt(*iso)
012500981026     d amgddt          S               d   datfmt(*iso)
012600981026     d savamg          S               d   datfmt(*iso)
012700981026     d datam           S               d   datfmt(*iso)
012800981026    >D Societa         S              3
012900981026    >D Ctb             S              2
013000981026     D TpReg           S              1
013100981026     D DtReg           S             10
013200981026     D DtLGio          S             10
013300981026     D Esito           S              1
013400981026     D Gesmsg          S              1
013500101013     D Pi              s              3  0                                      *indice ISTAT
013600050223     D Dataspe         s              8  0
013700080219     D Dataspe_iso     s               d   datfmt(*iso)
013800070202     D wdata           s              8  0
013900000217     D lenght          S             15  5 INZ(80)                              *Lunghezza cmd CLP
014000000217     D comman          S             80                                         *valore cmd CLP
014100101013     d $EoF            s              1n   inz(*off)
014200991230     C*********--------********-------*******-------********
014300991230     C* INDICATORI
014400991230     C*********--------********-------*******-------********
014500991230      * 50    - CHAIN  tabel00f
014600070202      * 53    - Errore manca prezzo medio del gasolio settimana in corso
014700050223      * 54    - Errore manca tabella o percentuale ISTAT blocco
014800031009      * 55    - ERRORE Tabella pilota di fatturazione impostata in blocco
014900040622      * 56    - ERRORE esiste LIBASP3 ciclo precedente non completato
015000071203      * 57    - ERRORE utente non autorizzato a lanciare la fatturtazione
015100991230      * 89/91 - ERRORE divise di fatturazione
015200991230     C*********--------********-------*******-------********
015300000000     C*---------------------------------------------------------------*
015400000000     C     *ENTRY        PLIST
015500000000     C                   PARM                    KPJBA
015600981026     C                   Z-ADD     1             CODUT             1 0
015700981026     C*---------------------------------------------------------------*
015800981026     C                   SETOFF                                       1195
015900981026     C                   CLEAR                   UNO
016000981026     C                   CLEAR                   SAVGGG
016100981026     C                   MOVEL     *ZEROS        PARAM
016200981026     C                   MOVEL     *BLANKS       VIDTSC
016300981026     C                   MOVEL     *BLANKS       VIDRB
016400981026     C                   Z-ADD     0             FIL
016500981026     C                   Z-ADD     0             VIDDFT
016600981026     C                   Z-ADD     0             VIDNFI
016700981026     C                   Z-ADD     0             VIDNFT
016800981026     C                   Z-ADD     0             VIDNFF
016900981026     C                   MOVEL     2             VIDTCL
017000981026     C                   CLEAR                   VIDDFI
017100981026     C                   CLEAR                   VIDDFF
017200981026     C*---------- RICERCA DITTA :
017300981026     C                   MOVEL     'SOC001'      TIPXSC
017400981026     C                   MOVEL     *BLANK        SOCXSC
017500981026     C                   EXSR      REPSOC
017600981026     C     RTNXSC        IFNE      '1'
017700981026     C                   MOVEL     XSOCDS        SOC001
017800981026     C                   MOVEL     xscrgs        RSUT             20
017900981026     c                   end
018000981026     c                   move      xscsoc        wsocieta
018100000000     C*---------------------------------------------------------------*
018200981026      *parametri per richiamo numeratori
018300981026     C     PNMR          PLIST
018400981026     C                   PARM                    XNMRIC            1
018500981026     C                   PARM                    XNMSOC
018600981026     C                   PARM                    XNMUNI
018700981026     C                   PARM                    XNMCTB
018800981026     C                   PARM                    XNMKEY
018900981026     C                   PARM                    XNMSER
019000981026     C                   PARM                    XNMCMT            1
019100981026     C                   PARM                    XNMDAT
019200981026     C                   PARM                    XNMIVA            1
019300981026     C                   PARM                    XNMREG            1
019400981026     C                   PARM                    XNMERR            1
019500981026     C                   PARM                    XNMNUM            9 0
019600981026     C                   PARM                    XNMAUT            1
019700981026     C                   PARM                    XNMDAG                         opzionale
019800891017     C     KTB           KLIST
019900891017     C                   KFLD                    CODUT
020000891017     C                   KFLD                    COD
020100891017     C     KTAB          KLIST
020200891017     C                   KFLD                    CODUT
020300891017     C                   KFLD                    COD               2
020400891017     C                   KFLD                    KEY               8
020500050223      *
020600050223      * carico la tabella ISTAT
020700050223      *
020800101013     c                   exsr      caristat
020900971022     C*---------------------------------------------------------------*
021000971022     C* CARICO TABELLA    FF  - FREQUENZA FATTURA PER METTERE LA DECOD
021100971022     C*                         A VIDEO
021200971022     C***
021300971022     C                   MOVEL     'FF'          COD
021400971022     C                   Z-ADD     0             B                 3 0
021500971022     C     KTB           CHAIN     TABEL                              70
021600971022     C     *IN70         DOWEQ     *OFF
021700971022     C     TBLFLG        IFEQ      ' '
021800971022     C                   ADD       1             B
021900971022     C                   MOVEL     TBLKEY        CFF(B)
022000971022     C                   MOVEL     TBLUNI        DFF(B)
022100971022     C                   MOVEL     TBLUNI        UFF(B)
022200971022     C                   ENDIF
022300971022     C     KTB           READE     TABEL                                  70
022400971022     C                   ENDDO
022500971022     C* CONTROLLO QUANTE DESCRIZIONI HO CARCATO
022600971022     C     55            DIV       B             RESTO             2 0
022700971022     C     RESTO         IFGT      15
022800971022     C                   Z-ADD     15            RESTO
022900971022     C                   ENDIF
023000971022     C**
023100971022     C     RESTO         SUB       3             LUDES             2 0
023200971022     C**
023300971022     C                   Z-ADD     1             B
023400971022     C                   Z-ADD     1             C                 3 0
023500971022     C**
023600971022     C                   CLEAR                   K55
023700971022     C     CFF(B)        DOWNE     ' '
023800971022     C                   MOVEL     CFF(B)        K55(C)
023900971022     C                   ADD       1             C
024000971022     C                   MOVEL     '='           K55(C)
024100971022     C**
024200971022     C                   MOVEA     DFF(B)        K12
024300981026     C                   Z-ADD     1             X                 2 0
024400971022     C     X             DOWLE     LUDES
024500971022     C                   ADD       1             C
024600971022     C                   MOVEL     K12(X)        K55(C)
024700971022     C                   ADD       1             X
024800971022     C                   ENDDO
024900971022     C**
025000971022     C                   ADD       2             C
025100971022     C                   ADD       1             B
025200971022     C                   ENDDO
025300971022     C                   MOVEA     K55           DESFF1
025400971022     C                   MOVEA     K55(29)       DESFF2
025500031009      *
025600000000     C     START         TAG
025700971022     C                   SETOFF                                       01
025800971022     C                   WRITE     SF01Z01
025900971022     C     FOR01         TAG
026000971022     C                   EXFMT     SF01D01
026100960924     C* FINE LAVORO
026200960924     C   KC              GOTO      FINE
026300071203     c* per 55 o 56 o 54 o 57 vado a FINE
026400050223     C                   if        *in55 = *on OR *in56 = *on  or *in54 = *on
026500071203     c                             or *in57
026600040622     C                   GOTO      FINE
026700040622     C                   endif
026800110127     c
026900110127     c* Data   del  giorno
027000110127     c                   movel     datasys       datadmy
027100110127     c     *dmy          movel     datadmy       w0060             6 0
027200110127     c                   movel     w0060         D1DATA
027300071203      *-------------------------------------------------------------------*
027400071203      * verifico se sono un utente autorizzato a lanciare fatturazione    *
027500071203      *-------------------------------------------------------------------*
027600071203
027700071203     c                   clear                   xchkautds
027800071203     c                   eval      xcasoc = xscsoc
027900071203     c                   eval      xcagrp = xscgrp
028000071203     c                   eval      xcafnc = 'CONPNO'
028100071203     c                   eval      xcaprf = knmus
028200071203     c                   eval      xcatck = 'AP'
028300071203     c                   eval      xcacut = xsccut
028400071203     c                   eval      xcauff = xscuff
028500071203     c                   eval      xcalin = xsculm
028600071203     c                   call      'XCHKAUT'
028700071203     c                   parm                    XCHKAUTDS
028800071203      *
028900071203      * verifico il codice di ritorno  se diverso da zero segnalo l'errore
029000071203     c                   eval      *in57 = xcartn <> 0
029100071203     c   57              goto      for01
029200071203      *
029300071203      * se non risultano errori dal richiamo di xchkautds verifico i flag di abilitazione
029400071203      * immissione dati iva,non iva ed effetti
029500071203     c                   movel     xcadds        conpnods
029600071203     c                   eval      *IN57 = (ImmIVA <> '1' or ImmNON <> '1' or
029700071203     c                                      ImmATT <> '1')
029800071203     c   57              goto      for01
029900040622      **
030000040622      * verifico se esiste la libreria LIBASP3, in tal caso errore          postata sul blocco
030100040622     C                   call      'TNSF01C'
030200040622     C                   parm      *blanks       Err_libasp
030300040622     c                   eval      *in56 = (ERR_libasp = 'E')
030400040622     C   56              GOTO      FOR01
030500031013      **
030600031013      * verifico se la tabella FAT (Tabella che pilota la fatturazione) è impostata sul blocco
030700031013      * della fatturazione
030800031013     c                   clear                   tibs02ds
030900031013     C                   movel     'L'           T02tla
031000031013     C                   movel     'C'           T02mod
031100031013     C                   movel     knsif         t02sif
031200031013     C                   movel     'FAT'         t02cod
031300031013     C                   movel     'FAT'         t02ke1
031400031013     C                   call      'TIBS02R'
031500031013     C                   parm                    KPJBA
031600031013     C                   parm                    TIBS02DS
031700031013     C                   movel     t02uni        DFAT
031800031013      *
031900031013     c                   If        §fatbloc  = 'S'
032000031013     c                   seton                                        55
032100031013     C                   goto      for01
032200031013     c                   endif
032300971022     C* TIPO FATTURAZIONE
032400971022     C                   Z-ADD     1             B
032500971022     C     VIDTFT        LOOKUP    CFF(B)                                 30
032600971022     C     *IN30         IFEQ      *OFF
032700971022     C                   SETON                                        40
032800971022     C                   GOTO      FOR01
032900971022     C                   ENDIF
033000971022     C*
033100971022     C                   SETON                                        01
033200971023     C                   SETOFF                                       95
033300971022     C* TIPO FATTURAZIONE MENSILE: VISUALIZZO LE DUE DATE FATTURA
033400040702      * E IMPOSTO TIPO CLIENTE A 2 (CLIENTI CODIFICATI E NON )
033500971022     C     VIDTFT        IFEQ      '4'
033600971022     C                   SETON                                        02
033700040702     C                   EVAL      VIDTCL = 2
033800971022     C                   ELSE
033900971022     C                   SETOFF                                       02
034000040702      * SE FATTURAZIONE SETTIMANALE IMPOSTO TIPO CLIENTE 0 (SOLO CODIFICATI)
034100040702     C     VIDTFT        IFEQ      '1'
034200040702     C                   EVAL      VIDTCL = 0
034300040702     C                   ENDIF
034400971022     C                   ENDIF
034500971022     C                   MOVEL     UFF(B)        DSFF
034600971023     C                   MOVEL     §FFDES        VIDDTF
034700000208     c* carico a video le divise che devono essere fatturate
034800000210     c******!!!!!*****   exsr      cardiv
034900000210     C* PER ADESSO CARICO COME COSTANTI
035000020221     C                   MOVE      'EUR'         VIDDV1
035100031009      * giro con una unica moneta
035200031009     C*****              MOVE      'ITL'         VIDDV2
035300971022     C****
035400971022     C* EMETTO SECONDA VIDEATA
035500971022     C     FOR02         TAG
035600971022     C                   WRITE     SF01D01
035700971022     C                   EXFMT     SF01D02
035800971022     C** F3  - FINE LAVORO
035900971022     C   KC              GOTO      FINE
036000971022     C** F12 - RITRONO
036100971022     C   KL              GOTO      START
036200960924     C**
036300960924     C** F10 - GESTIONE STAMPE
036400981026     C   10              MOVEL     '3'           KRITB
036500000000     C   10              CALL      'BCH09'
036600000000     C                   PARM                    KPJBA
036700971022     C   10              GOTO      FOR02
036800891017     C*
036900971022     C* TIPO CLIENTI DA FATTURARE
037000961111     C     VIDTCL        IFGT      2
037100891023     C                   SETON                                        92
037200971022     C   92              GOTO      FOR02
037300891023     C                   END
037400040702     C* SE FATTURAZIONE SETTIMANALE ERRORE SE TIPO CLIENTE DIVERSO DA 0
037500040702     C                   IF        VIDTFT = '1' AND VIDTCL <> 0
037600040702     C                   SETON                                        93
037700040702     C   93              GOTO      FOR02
037800040702     C                   END
037900971022     C**
038000971022     C* FATTURAZIONE MENSILE: DATA FATTURA INIZIO MESE
038100971022    1C     *IN02         IFEQ      *ON
038200971022     C**
038300981026     c     *eur          test(d)                 viddfi                 99
038400981026     c                   if        *in99
038500981026     c     *dmy          test(d)                 viddfi                 99
038600981026     c  n99*dmy          move      viddfi        dataeur
038700981026     c  n99              move      dataeur       viddfi
038800981026     c                   else
038900981026     c                   move      viddfi        dataeur
039000981026     c                   end
039100981026     C   99              GOTO      FOR02
039200981026     c                   move      dataeur       dataiso
039300981026     c                   move      dataiso       amgfat
039400981026     c                   exsr      mvc
039500971022     C**
039600981026     c                   if        esito = *on
039700981026     C                   seton                                        99
039800981026     C                   GOTO      FOR02
039900981026     c                   end
040000971022     C** DATA FATTURA FINE MESE
040100981026     c     *eur          test(d)                 viddff                 98
040200981026     c                   if        *in98
040300981026     c     *dmy          test(d)                 viddff                 98
040400981026     c  n98*dmy          move      viddff        dataeur
040500981026     c  n98              move      dataeur       viddff
040600981026     c                   else
040700981026     c                   move      viddff        dataeur
040800981026     c                   end
040900981026     C   98              GOTO      FOR02
041000981026     c                   move      dataeur       dataiso
041100981026     c                   move      dataiso       amgddc
041200981026     c                   exsr      mvc
041300981026     C**
041400981026     c                   if        esito = *on
041500981026     C                   seton                                        98
041600981026     C                   GOTO      FOR02
041700981026     c                   end
041800971022   X1C                   ELSE
041900971022     C** DATA FATTURA SINGOLA
042000981026     c     *eur          test(d)                 viddft                 41
042100981026     c                   if        *in41
042200981026     c     *dmy          test(d)                 viddft                 41
042300981026     c  n41*dmy          move      viddft        dataeur
042400981026     c  n41              move      dataeur       viddft
042500981026     c                   else
042600981026     c                   move      viddft        dataeur
042700981026     c                   end
042800981026     C   41              GOTO      FOR02
042900981026     c                   move      dataeur       dataiso
043000981026     c                   move      dataiso       amgddt
043100981026     c                   exsr      mvc
043200981026     C**
043300981026     c                   if        esito = *on
043400981026     C                   seton                                        41
043500981026     C                   GOTO      FOR02
043600981026     c                   end
043700971210     C* SOTTRAGGO 1 ALLA DATA
043800971210     C**
043900981026     C                   SUBdur    1:*d          dataiso
044000981026     C                   move      dataiso       DATAM
044100971022    1C                   ENDIF
044200971022     C** DATA SPEDIZIONE
044300981026     c                   if        viddsp = 0
044400981026     c                   seton                                          97
044500981026     c                   else
044600981026     c     *eur          test(d)                 viddsp                 97
044700981026     c                   if        *in97
044800981026     c     *dmy          test(d)                 viddsp                 97
044900981026     c  n97*dmy          move      viddsp        dataeur
045000981026     c  n97              move      dataeur       viddsp
045100981026     c                   else
045200981026     c                   move      viddsp        dataeur
045300981026     c                   end
045400981026     c                   end
045500981026     C   97              GOTO      FOR02
045600981026     c                   move      dataeur       dataiso
045700050223      * verifico se esiste percentuale ISTAT
045800050223     c                   move      dataiso       dataspe
045900080219     c                   move      dataiso       dataspe_iso
046000050301     c                   movel     dataspe       annospe           4 0
046100050223     c                   clear                   perce
046200050223
046300101013     c                   do        999           pi
046400101014      * se data = a zero  vado a fine non dovrebbero esserci dei successivi
046500050223     c                   if        dia(pi) = 0
046600050223     c                   leave
046700050223     c                   endif
046800050301     c                   movel(p)  dia(pi)       annoist           4 0
046900050301      * se anno di spedizione uguale a quello dello scaglione prendo la percentuale
047000050301     c                   if        annospe = annoist
047100050223     c                   z-add     iac(pi)       perce             5 1
047200050301      *
047300050223     c                   else
047400060110     c                   iter
047500050223     c                   endif
047600050223
047700050223     c                   enddo
047800050223
047900050223      * Verifico se ho recuperato una percentuale istat
048000050223     c                   if        perce = *zeros
048100050223     c                   seton                                        54
048200050223     c                   goto      for02
048300050223     c                   endif
048400070202
048500070201      * verifico se esiste ultimo prezzo medio del gasolio
048600070202     c                   clear                   wgio              1
048700070202     c                   movel     viddsp        wggmm             4
048800070202     c                   move      viddsp        waa               2
048900070202     c                   movel     wggmm         wggmmaa           6
049000070202     c                   move      waa           wggmmaa
049100070202     c                   call      'XGIOSE1'
049200070202     c                   parm                    wggmmaa
049300070202     c                   parm                    wgio
049400070202     c                   move      wgio          giorno            1 0
049500070202      * cerco il primo lunedì antecedente la data spedizione per cercare il prezzo medio gasolio
049600070202      * calcolo il numero di giorni che devo sottrarre dalla data ultima spedizione
049700070202
049800070202     c                   eval      giorno = giorno - 1
049900070202     c     *eur          movel     viddsp        dataiso
050000070202     c                   subdur    giorno:*d     dataiso
050100070202     c                   move      dataiso       wdata
050200070202     c     wdata         chain     tipmg01l
050300070202     c                   If        not %found(tipmg01l)
050400070202     c                   seton                                        53
050500070202     c                   goto      for02
050600070202     c                   endif
050700070201     C**
050800070201      *
050900971210     C**
051000971210     C** PER FATTURAZIONE NON MENSILE LA DATA SPEDIZIONE DEVE ESSERE
051100971210     C**  - 1 RISPETTO ALLA DATA FATTURA
051200080219     C  N02SAVAMG        IFNE      dataspe_iso
051300971210     C                   CLEAR                   DUE
051400080219     C                   MOVEL     dataspe_iso   SAVAMG
051500971210     C                   ENDIF
051600971210     C**
051700080219     C  N02dataspe_iso   IFNE      DATAM
051800971210     C     DUE           ANDEQ     ' '
051900971210     C                   SETON                                        46
052000971210     C                   MOVEL     '1'           DUE               1
052100971210     C                   GOTO      FOR02
052200971210     C                   ENDIF
052300111028     C**
052400111028     C** PER FATTURAZIONE MENSILE LA DATA SPEDIZIONE NON DEVE ESSERE
052500111028     C** MAGGIORE DELLE DATE FATTURAZIONE
052600111028     C**
052700111028     C                   IF        (*IN02 AND (dataspe_iso > amgfat or
052800111028     c                             dataspe_iso > amgddc)) or
052900111028     C                             (not *IN02 AND dataspe_iso > amgddt)
053000111028     C                   SETON                                        47
053100111028     C                   GOTO      FOR02
053200111028     C                   ENDIF
053300971022     C**
053400971022     C* PER FATTURAZIONE NON MESILE DEVE ESSERE UGUALE ALLA DATA FATT
053500971022     C* CONTROLLO SE LA DATA SPEDIZIONE IMMESSA E' TRA QUELLE
053600971022     C*  PREVISTE NEL TIPO FATTURAZIONE
053700971210     C   02              MOVEL     VIDDFF        W0020             2 0
053800971210     C  N02              MOVEL     VIDDFT        W0020             2 0
053900971022    1C     W0020         IFNE      SAVGGG
054000971022     C                   Z-ADD     W0020         SAVGGG            2 0
054100971022     C                   CLEAR                   UNO
054200971022    1C                   ENDIF
054300971022     C*
054400040702      * controllo solo in caso di fatturazione mensile
054500040702     c                   if        vidtft = '4'
054600971022     C     W0020         LOOKUP    DGG                                    30
054700971022    1C     *IN30         IFEQ      *OFF
054800971022     C     UNO           ANDEQ     ' '
054900971022     C                   SETON                                        44
055000971022     C                   MOVEL     '1'           UNO               1
055100971022     C                   GOTO      FOR02
055200971022    1C                   ENDIF
055300040702     c                   endif
055400971210     C* LA DATA INIZIO MESE NON DEVE INVECE ESSERE NELLA TABELLA DEI GG
055500971210     C*  FATTURABILI
055600971210     C   02              MOVEL     VIDDFI        W0020             2 0
055700971210     C   02W0020         LOOKUP    DGG                                    30
055800001115    1C   02*IN30         IFEQ      *ON
055900001115     C*!""!*             SETON                                        45
056000001115     C*!""!*             GOTO      FOR02
056100971210    1C                   ENDIF
056200991230     c** contolllo le divise inserite
056300991230     c                   clear                   viddd1
056400991230     c                   clear                   viddd2
056500991230      * se entrambe a blank errore
056600991230     c                   if        viddv1 = *blanks and  viddv2 = *blanks
056700991230     c                   seton                                        89
056800991230     c                   goto      for02
056900991230     c                   endif
057000991230      * se la prima = a blank e la seconda no errore
057100991230     c                   if        viddv1 = *blanks and  viddv2 <> *blanks
057200991230     c                   seton                                        89
057300991230     c                   goto      for02
057400991230     c                   endif
057500991230      * se entambi uguali errore
057600991230     c                   if        viddv1 = viddv2
057700991230     c                   seton                                        91
057800991230     c                   goto      for02
057900991230     c                   endif
058000991230      * controllo la prima divisa            1°
058100991230     c                   movel     'CV'          cod
058200991230     c                   movel(p)  viddv1        key
058300991230     c     ktab          chain(n)  tabel                              49
058400991230      * divisa inesistente
058500991230     c                   if        *in49 or tblflg <> ' '
058600991230     c                   seton                                        89
058700991230     c                   goto      for02
058800991230     c                   endif
058900991230      * divisa non abilitata alla fatturazione
059000991230     c                   movel     tbluni        dscv
059100991230     c                   if        §cvabi <> 'S'
059200991230     c                   seton                                        88
059300991230     c                   goto      for02
059400991230     c                   endif
059500991230      * decodifico la divisa
059600991230     c                   movel     §cvdes        viddd1
059700971022     C**
059800991230      * controllo la seconda divisa se diversa da blanks
059900991230     c                   if        viddv2 <> *blanks
060000991230     c                   movel     'CV'          cod
060100991230     c                   movel(p)  viddv2        key
060200991230     c     ktab          chain(n)  tabel                              50
060300991230      * divisa inesistente
060400991230     c                   if        *in50 or tblflg <> ' '
060500991230     c                   seton                                        91
060600991230     c                   goto      for02
060700991230     c                   endif
060800991230      * divisa non abilitata alla fatturazione
060900991230     c                   movel     tbluni        dscv
061000991230     c                   if        §cvabi <> 'S'
061100991230     c                   seton                                        90
061200991230     c                   goto      for02
061300991230     c                   endif
061400991230      * decodifico la divisa
061500991230     c                   movel     §cvdes        viddd2
061600991230      *
061700991230     c                   endif
061800991230     C**
061900000000     C                   SETOFF                                       9411
062000000000     C     VIDTSC        IFNE      *BLANKS
062100000000     C                   DO        40            A
062200000000     C     FIL(A)        IFNE      0
062300000000     C                   SETON                                        11
062400000000     C                   END
062500000000     C                   END
062600000000     C  N11              SETON                                        94
062700971022     C   94              GOTO      FOR02
062800000000     C** 94 ON INDICATA OPZIONE E NON FILIALI
062900000000     C                   ELSE
063000000000     C                   DO        40            A
063100000000     C     FIL(A)        IFGT      0
063200000000     C                   SETON                                        94
063300000000     C                   END
063400000000     C                   END
063500971022     C   94              GOTO      FOR02
063600000000     C                   END
063700000000     C                   DO        40            A                 2 0
063800000000     C     FIL(A)        IFGT      0
063900910111     C     FIL(A)        CHAIN     AZORG                              96
064000971022     C   96              GOTO      FOR02
064100960925     C** P.O. INESISTENTE
064200000000     C                   END
064300000000     C                   END
064400981026     c*controllo cliente se immesso
064500981026    1C     VIDKSC        IFGT      0
064600981026     C     Keycli        klist
064700981026     C                   kfld                    wsocieta
064800981026     C                   kfld                    wksc              8
064900981026     C                   kfld                    k£fil
065000981026     C                   kfld                    k£lin
065100981026     c                   move      *zeros        wksc
065200981026     c                   move      vidksc        wksc
065300981026     C     *LIKE         DEFINE    clnlineav     k£lin
065400981026     C     *LIKE         DEFINE    clnfiliale    k£fil
065500981026     C     Keycli        CHAIN     ancln01l                           30
065600981026     c                   if        clndtfirpc > *loval
065700981026     c                   seton                                        30
065800981026     c                   endif
065900981026     C  N30clnsogg       CHAIN     ansog01l                           30
066000981026    2C     *IN30         IFEQ      *ON
066100981026     C                   SETON                                        43
066200981026     C                   GOTO      FOR02
066300981026    2C                   ENDIF
066400981026     C                   MOVEL     sogdes        VIDDES
066500981026    2C                   ENDIF
066600971022     C**
066700891016     C* REPERISCE  NUMERAZIONE
066800941104     C                   MOVEL     'QC'          COD
066900941104     C                   MOVEL     *BLANKS       KEY
067000941104     C                   MOVEL     '2'           KEY
067100951002     C     KTAB          CHAIN(N)  TABEL                              50
067200971022     C   50              GOTO      FOR02
067300941104     C                   MOVEL     TBLUNI        DSQC2
067400971023     C*
067500981027     C                   MOVE      '1'           XNMRIC            1
067600981026     C                   MOVEl     §QCRIV        wserie            4            REGISTRO VEND.
067700981026     C** PER FATTURE INIZIO MESE
067800981026     C**  E FATTURAZIONE NON MENSILE
067900981027     C   02              MOVEL     amgfat        xnmdat                         ANNO FATTURA
068000981027     C  N02              MOVEL     amgddt        xnmdat                         ANNO FATTURA
068100981027     C   02              MOVEL     amgfat        xnmdag                         ANNO FATTURA
068200981027     C  N02              MOVEL     amgddt        xnmdag                         ANNO FATTURA
068300981026     C                   MOVE      §QCFIV        wserie                         LIBRO IVA VEND.
068400981026     c                   exsr      repnum
068500981026     C     xnmerr        ifgt      '0'
068600981026     C     xnmerr        andlt     '7'
068700981026     C                   seton                                        51
068800981026     C                   GOTO      FOR02
068900981026     C                   end
069000981026     C     xnmerr        IFeq      '7'
069100891017     C                   SETON                                        99
069200971023     C                   GOTO      FOR02
069300971023     C                   END
069400971022     C**
069500981026     C   02              Z-ADD     xnmnum        VIDNFI
069600971022     C**
069700981026     C  N02              Z-ADD     xnmnum        VIDNFT
069800971022     C** PRIMO NUMERO DI FATTURA INIZIO MESE
069900971022     C     *IN02         IFEQ      *ON
070000981026     C                   MOVE      amgddc        xnmdat                         ANNO FATTURAZ.
070100981027     C                   MOVE      amgddc        xnmdag                         ANNO FATTURAZ.
070200981026     C                   MOVE      §QCFI2        wserie                         LIBRO IVA FT.A.
070300981026     c                   exsr      repnum
070400981026     C     xnmerr        ifgt      '0'
070500981026     C     xnmerr        andlt     '7'
070600981026     C                   seton                                        52
070700981026     C                   GOTO      FOR02
070800981026     C                   end
070900981026     C     xnmerr        IFeq      '7'
071000940927     C                   SETON                                        98
071100971023     C                   GOTO      FOR02
071200971023     C                   END
071300971022     C**
071400981026     C                   Z-ADD     xnmnum        VIDNFF
071500971022     C**
071600940927     C** PRIMO NUMERO DI FATTURA FINE MESE
071700971022     C                   ENDIF
071800971022     C**
071900960924     C  NKF              SETON                                        95
072000971022     C   95
072100971022     CANNKF              GOTO      FOR02
072200960924     C* F6 - ELABORAZIONE
072300960924     C     *INKF         IFEQ      *ON
072400960924     C                   MOVEL     FIL           FILX
072500981026     c                   clear                   dataeur
072600000216     c* pulizia del file etichette
072700000217     C                   MOVEL(P)  cm1(1)        comman
072800000217     C                   CALL      'QCMDEXC'
072900000217     C                   PARM                    comman
073000000217     C                   PARM                    lenght
073100000217     c*
073200981026     c                   if        viddfi = 0
073300981026     c                   move      dataeur       viddfi
073400981026     c                   end
073500981026     c                   if        viddff = 0
073600981026     c                   move      dataeur       viddff
073700981026     c                   end
073800981026     c                   if        viddft = 0
073900981026     c                   move      dataeur       viddft
074000981026     c                   end
074100040622      ***
074200040622      *  Creo libreria LIBASP3
074300040622     C                   IF        VIDTFT = '4'
074400040622     c                   eval      Wtft = 'MENSILE    '
074500040622     c                   ELSE
074600040622     c                   eval      Wtft = 'SETTIMANALE'
074700040622     c                   ENDIF
074800040622     c                   movel     VIDDSP        W008A
074900040622     c                   eval      Wtext = 'Fattura ' + wtft +
075000040713     c                             ' data sped. al '   +
075100040622     c                             %subst(W008A:1:2) +  '/' +
075200040622     c                             %subst(W008A:3:2) +  '/' +
075300040713     c                             %subst(W008A:7:2)
075400040622     c                   CALL      'TNSF01C1'
075500040622     c                   parm                    WText
075600040622      ***
075700061009      * preparazione bole fuori misura DPD
075800061009      ***
075900061009      * se lancio fatturazione mensile / nessuna scelta parzializzante codice cliente e filiale
076000061009     c                   if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
076100061009     C                   MOVEL     PARAM         KPJBU
076200061009     C                   MOVEL     'SF50'        KCOAZ
076300061009     C                   CALL      'BCH10'
076400061009     C****               CALL      'TNSF50C'
076500061009     C                   PARM                    KPJBA
076600061009     c                   CLEAR                   KPJBU
076700061009     c                   endif
076800991230     c* gestione delle divise
076900991230     c* un giro per ogni divisa
077000991230     c* se esistono due divise lancio il primo giro
077100991230     c                   if        viddv1 <> *blanks and viddv2 <> *blanks
077200991230     c* 1° giro
077300991230     c* aggiornamento  tassazione  per tutte le bolle
077400991230     c* non si stampa il manca tariffa
077500991230     c                   eval      viddiv = viddv1
077600991230     c                   eval      vidagg = 'S'
077700991230     c                   eval      vidsta = 'N'
077800991230     c*
077900050426     C                   MOVEL     'SF02'        KCOAZ
078000960924     C                   MOVEL     PARAM         KPJBU
078100050426     C                   CALL      'BCH10'
078200050426     C**                 CALL      'TNSF02C'
078300941124     C                   PARM                    KPJBA
078400991230     c* 2° giro
078500991230     c* nesun aggiornamento  tassazione  per le bolle con divisa diversa
078600991230     c* si stampa il manca tariffa
078700991230     c                   eval      viddiv = viddv2
078800991230     c                   eval      vidagg = 'N'
078900991230     c                   eval      vidsta = 'S'
079000991230     c*
079100050426     C                   MOVEL     'SF02'        KCOAZ
079200991230     C                   MOVEL     PARAM         KPJBU
079300050426     C                   CALL      'BCH10'
079400050426     C***                CALL      'TNSF02C'
079500991230     C                   PARM                    KPJBA
079600991230     c*
079700991230     c                   else
079800991230     c* esiste solo una divisa mi comporto come se fossi un secondo giro
079900991230     c*
080000991230     c* nesun aggiornamento  tassazione  per le bolle con divisa diversa
080100991230     c* si stampa il manca tariffa
080200991230     c                   eval      viddiv = viddv1
080300991230     c                   eval      vidagg = 'N'
080400991230     c                   eval      vidsta = 'S'
080500991230     c*
080600050427     C                   MOVEL     'SF02'        KCOAZ
080700991230     C                   MOVEL     PARAM         KPJBU
080800050427     C                   CALL      'BCH10'
080900050427     C***                CALL      'TNSF02C'
081000991230     C                   PARM                    KPJBA
081100991230     c**
081200991230     c                   endif
081300111104      *
081400111104      * lancio manca tariffa solo se fatturazione mensile
081500111104     C                   If        vidtft = '4'
081600111104     c                   clear                   tnsb44ds
081700111104
081800111104     c                   move      00010101      D44dsd
081900111104     c                   move      dataspe       D44dsa
082000111104      * creazione file
082100111104     c                   eval      d44crf = 'S'
082200111104      *
082300111104     c                   movel     d44dsa        com8a             8
082400111104     c                   eval      D44mbr = 'F'+ com8a
082500111104      * stampa imponibile
082600111104     c                   eval      d44fsi = 'N'
082700111104      * stampa separata per P.O.
082800111104     c                   eval      d44spp = 'S'
082900111104      * stampa unica
083000111104     c                   eval      d44sun = 'N'
083100111104      * coda di stampa
083200111104     c                   eval      d44out = 'O'
083300111104      * totali per area
083400111104     c                   eval      d44tot = 'S'
083500111104      * stampante totali per area
083600111104     c                   eval      d44Tou = 'T'
083700111104
083800111104      * LANCIO
083900111104     c                   clear                   kpjbu
084000111104     c                   movel     tnsb44ds      Kpjbu
084100111104
084200111104     C                   movel     'SB46'        KCOAZ
084300111104     C                   CALL      'BCH10'
084400111104     c*****              call      'TNSB46R'
084500111104     c                   parm                    kpjba
084600111104     c                   endif
084700111104      *
084800991230     c*
084900000209     c*lancio la stampa delle etichette
085000000209     c*
085100000209     C                   MOVEL     'SF15'        KCOAZ
085200000209     C                   MOVEL     PARAM         KPJBU
085300001221     C                   CALL      'BCH10'
085400001221     C**                 CALL      'TNSF15R'
085500000209     C                   PARM                    KPJBA
085600110127     c
085700110127     c** Sottometto azione schedulata dati statistiche clienti Roma
085800110127     C                   IF        VIDTFT = '4'
085900110127     c                   movel     amgfat        stf1DFI
086000110127     c                   movel     amgddc        stf1DFF
086100110127     C                   MOVEL     'STF1'        KCOAZ
086200110127     c                   eval      d1oras='1900'
086300110127     c                   movel     scheds        kbuff
086400110127     C                   MOVEL     PARAMSTF1     KPJBU
086500110127     c
086600110127     C                   CALL      'BCH10'
086700110127     C                   PARM                    KPJBA
086800110127     c                   endif
086900110127     c
087000960924     C                   ENDIF
087100960924     C**
087200000000     C     FINE          TAG
087300000000     C                   SETON                                        LR
087400981026     C*----------------------------------------------------*
087500981026     C* Reperisco numeratore
087600981026     C*----------------------------------------------------*
087700981026     c     repnum        begsr
087800981026     C                   MOVE      wsocieta      XNMSOC            3
087900981026     C                   MOVE      *BLANKS       XNMUNI            8
088000981026     C                   MOVE      'CG'          XNMCTB            2
088100981026     C                   MOVE      *BLANKS       XNMKEY            8
088200981026     C                   MOVE      WSERIE        XNMSER            4
088300981026     C                   MOVE      *OFF          XNMCMT            1
088400981026     C                   MOVE      *On           XNMIVA            1
088500981026     C                   MOVE      *OFF          XNMREG            1
088600981027     C                   MOVE      '0'           XNMerr            1
088700981026     C*
088800981026     C                   CALLB     'XNMR'        PNMR
088900981026     C                   endsr
089000981026     C*----------------------------------------------------*
089100981026     C* controllo libro giornale
089200981026     C*----------------------------------------------------*
089300981026     c     mvc           begsr
089400981026     c                   move      dataiso       dtreg
089500981026     c                   move      dataiso       dtlgio
089600981026     C                   CALLB     'NDMVC106'
089700981026     C                   PARM      xscsoc        Societa
089800981026     C                   PARM      'CG'          CTB
089900981026    >C                   PARM      'D'           TpReg
090000981026    >C                   PARM                    DtLGio
090100981026    >C                   PARM                    DtReg
090200981026     C                   PARM      *on           GesMsg
090300981026     C                   PARM                    Esito
090400981026     c                   endsr
090500981026     C*----------------------------------------------------*
090600981026     C* Reperimento dati società
090700981026     C*----------------------------------------------------*
090800981026     C     REPSOC        BEGSR
090900981026     C                   CALL      'XSOC'
091000981026     C                   PARM                    TIPXSC            6
091100981026     C                   PARM                    SOCXSC            3
091200981026     C                   PARM                    CDSXSC            9 0
091300981026     C                   PARM                    MODXSC            3
091400981026     C                   PARM      *blanks       RTNXSC            1
091500981026     C                   PARM                    XSOCDS
091600981026     C                   PARM                    KPJBA
091700981026     C                   ENDSR
091800000208     C*******************************************************************
091900000208     C** CARICO DIVISE A VIDEO
092000000208     C*******************************************************************
092100000208     C     CARDIV        BEGSR
092200000208     C**
092300000208     C                   MOVEL     'CV'          COD
092400000208     c                   clear                   key
092500000208     C     KTab          SETLL     TABEL
092600000208     C                   DO        *HIVAL
092700000208     C     KTb           READE     TABEL                                  07
092800000208     c   07              leave
092900000208     c*
093000000208     c                   if        tblflg <> ' '
093100000208     c                   iter
093200000208     c                   endif
093300000208     c*
093400000208     C     TBLKEY        IFNE      *BLANKS
093500000208     C                   MOVEL     TBLUNI        DSCV
093600000208     c*
093700000208     c                   if        §cvabi = 'S'
093800000208     c*
093900000208     c                   if         viddv1 = *blank
094000000208     c                   movel     tblkey        viddv1
094100000208     c                   iter
094200000208     c                   endif
094300000208     c*
094400000208     c                   if         viddv2 = *blank
094500000208     c                   movel     tblkey        viddv2
094600000208     c                   leave
094700000208     c                   endif
094800000208     c*
094900000208     c                   endif                                                  cvabi
095000000208     c*
095100000208     c                   endif                                                  key > '    '
095200000208     c* torno a leggere
095300000208     C                   ENDDO
095400000208     C*
095500000208     C                   ENDSR
095600050223      ****************************************************
095700050223      ** CARICAMENTO IAC
095800050223      ****************************************************
095900101013     c     CARISTAT      BEGSR
096000101013
096100101013      /free
096200101013       //?Carico scatti ISTAT
096300101013         SISsca = 0;
096400101013         clear DIA;
096500101013         setll (SISsca) TISIS01L;
096600101013
096700101013         DOW  not $EoF;
096800101013
096900101013           read TISIS01L;
097000101013
097100101013           IF  %Eof(TISIS01L);
097200101013             $EoF = *on;
097300101013             leave;
097400101013           ENDIF;
097500101013
097600101013           DIA(SISsca) = SISdata;
097700101013           IAC(SISsca) = SISpunti;
097800101013
097900101013         ENDDO;
098000101013
098100101013      /end-free
098200050223     c                   ENDSR
098300000217**   cm1 - comandi
098400000217CLRPFM FILE(ETLAV00F)
