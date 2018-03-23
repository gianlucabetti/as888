000100981026     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981026     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300981026     H/title         - FATTURAZIONE AUTOMATICA -FILTRO-              *
000400001115     H*                                                              *
000500001115     H* *!""!* modifiche di prova *!""!*                             *
000600000000     H*--------------------------------------------------------------*
000700980130     FTABEL00F  IF   E           K DISK
000800130108     fTNTBE01L  uf   e           k disk
000900910111     FAZORG01L  IF   E           K DISK
001000981026     FAncln01L  IF   E           K DISK
001100981026     FAnsog01L  IF   E           K DISK
001200070201     FTipmg01l  IF   E           K DISK
001300101013     ftisis01l  if   e           k disk
001400991230     FTNSF01D   CF   E             WORKSTN
001500971022     D CFF             S              1    DIM(15)                              *TAB FREQUENZA FT
001600971022     D DFF             S             12    DIM(15)                              *TAB DESCR.FREQU
001700971022     D UFF             S             89    DIM(15)                              *TBLUNI DI FREQU
001800971022     D K12             S              1    DIM(12)                              COMODO
001900971022     D K55             S              1    DIM(55)                              COMODO
002000150205     D IAC             S              5  2 DIM(999)                             PERCENTUALI ISTAT
002100101013     D DIA             S              8  0 DIM(999)                             DATE SCATTI ISTAT
002200000217     D cm1             S             80    DIM(1) CTDATA PERRCD(1)              *clrpfm etlav
002300000000     D PARAM           DS
002400981026     D  VIDDFI                        8  0
002500981026     D  VIDDFF                        8  0
002600981026     D  VIDDSP                        8  0
002700981026     D  VIDTFT                        1
002800981026     D  VIDTCL                        1  0
002900981026     D  VIDTSC                        1
003000981026     D  FILx                          3  0 dim(40)
003100981026     D  VIDRB                         2
003200981026     D  VIDDFT                        8  0
003300981026     D  VIDKSC                        7  0
003400981026     D  wsocieta                      3
003500991230     D  VIDDIV                        3                                         *divisa da fatturare
003600991230     D  VIDAGG                        1                                         *AGG.bolle altre div
003700991230     D  VIDSTA                        1                                         *stampa manca tariff
003800000000     D FILDS           DS
003900891016     D  VIDF01                 1      3  0
004000891016     D  VIDF02                 4      6  0
004100891016     D  VIDF03                 7      9  0
004200891016     D  VIDF04                10     12  0
004300891016     D  VIDF05                13     15  0
004400891016     D  VIDF06                16     18  0
004500891016     D  VIDF07                19     21  0
004600891016     D  VIDF08                22     24  0
004700891016     D  VIDF09                25     27  0
004800891016     D  VIDF10                28     30  0
004900891016     D  VIDF11                31     33  0
005000891016     D  VIDF12                34     36  0
005100891016     D  VIDF13                37     39  0
005200891016     D  VIDF14                40     42  0
005300891016     D  VIDF15                43     45  0
005400891016     D  VIDF16                46     48  0
005500891016     D  VIDF17                49     51  0
005600891016     D  VIDF18                52     54  0
005700891016     D  VIDF19                55     57  0
005800891016     D  VIDF20                58     60  0
005900891016     D  VIDF21                61     63  0
006000891016     D  VIDF22                64     66  0
006100891016     D  VIDF23                67     69  0
006200891016     D  VIDF24                70     72  0
006300891016     D  VIDF25                73     75  0
006400891016     D  VIDF26                76     78  0
006500891016     D  VIDF27                79     81  0
006600891016     D  VIDF28                82     84  0
006700891016     D  VIDF29                85     87  0
006800891016     D  VIDF30                88     90  0
006900891016     D  VIDF31                91     93  0
007000891016     D  VIDF32                94     96  0
007100891016     D  VIDF33                97     99  0
007200891016     D  VIDF34               100    102  0
007300891016     D  VIDF35               103    105  0
007400891016     D  VIDF36               106    108  0
007500891016     D  VIDF37               109    111  0
007600891016     D  VIDF38               112    114  0
007700891016     D  VIDF39               115    117  0
007800891016     D  VIDF40               118    120  0
007900891016     D  FIL                    1    120  0
008000891016     D                                     DIM(40)                              FILIALI ESCLUSE
008100110127     d
008200110127     D PARAMSTF1       DS
008300110127     D  stf1dfi                       8  0
008400110127     D  stf1dff                       8  0
008500110127     d
008600121211
008700121211     d ParmSF3         ds
008800121212     d  sf3AA                         2  0
008900121212     d  sf3MM                         2  0
009000121211
009100110127      * DATA E ORA SCHEDULAZIONE PER KPJBA
009200110127     D SCHEDS          DS
009300110127     D  D1DATA                 1      6
009400110127     D  D1ORAS                 7     10
009500110127     d
009600131008
009700131008     d wData1          ds
009800131009     D  Wgiorno1               1      2  0
009900131009     D  Wmese1                 3      4  0
010000131009     D  Wanno1                 5      8  0
010100131008
010200941104     D DSQC2         E DS
010300951002     D*-------------------
010400951002     D* DS DATI VARI PER C/E
010500951002     D*-------------------
010600971022     D DSFF          E DS
010700971022     D  DGG                   31     40  0
010800971022     D                                     DIM(5)                               *GG FATTURAZ
010900991230     D*-------------------
011000991230     D DSCV          E DS                                                       tabella divise
011100031009     D*-------------------
011200031009     D TIBS02DS      E DS                                                       tabelle
011300031009     D DFAT          E DS                                                       tabella FAT
011400031009     D*-----------
011500000000     D KPJBA         E DS
011600071130     D*-----------
011700981026     D soc001        e ds                  extname(xsoc001ds)
011800981026     D xsocds          DS          1000
011900071203     D xchkautds     e ds
012000071130     D conpnods      e ds
012100071130     D*-----------
012200130108
012300130108       //?Ds tabella RPF (data ultima spedizione fatturata)
012400130108     d dRPFfatgio    e ds
012500120214
012600040622     d Err_libasp      s              1
012700040622     d Wtft            s             11
012800040622     d Wtext           s             50
012900040622     d W008a           s              8
013000110127     d datasys         s               d   datfmt(*iso) inz(*sys)
013100110127     d datadmy         s               d   datfmt(*dmy)
013200110127     d dataiso         s               d   datfmt(*iso)
013300981026     d dataeur         s               d   datfmt(*eur)
013400981026     D  XNMDAG         S               d   datfmt(*iso)
013500981026     d  XNMDAT         S               d   datfmt(*iso)
013600981026     d amgfat          S               d   datfmt(*iso)
013700981026     d amgddc          S               d   datfmt(*iso)
013800981026     d amgddt          S               d   datfmt(*iso)
013900981026     d savamg          S               d   datfmt(*iso)
014000981026     d datam           S               d   datfmt(*iso)
014100981026    >D Societa         S              3
014200981026    >D Ctb             S              2
014300981026     D TpReg           S              1
014400981026     D DtReg           S             10
014500981026     D DtLGio          S             10
014600981026     D Esito           S              1
014700981026     D Gesmsg          S              1
014800131009     D Gg              S              2  0
014900101013     D Pi              s              3  0                                      *indice ISTAT
015000050223     D Dataspe         s              8  0
015100080219     D Dataspe_iso     s               d   datfmt(*iso)
015200070202     D wdata           s              8  0
015300111216     D PRIMO           s              8  0
015400131008     d wggmmaa         s              6
015500131008     d wgioset         s              1
015600131009     d w0140           s             14  0
015700000217     D lenght          S             15  5 INZ(80)                              *Lunghezza cmd CLP
015800000217     D comman          S             80                                         *valore cmd CLP
015900101013     d $EoF            s              1n   inz(*off)
016000130325     d $Fine           s               n   inz(*off)
016100121212     d wanno           s              4  0
016200991230     C*********--------********-------*******-------********
016300991230     C* INDICATORI
016400991230     C*********--------********-------*******-------********
016500991230      * 50    - CHAIN  tabel00f
016600070202      * 53    - Errore manca prezzo medio del gasolio settimana in corso
016700050223      * 54    - Errore manca tabella o percentuale ISTAT blocco
016800031009      * 55    - ERRORE Tabella pilota di fatturazione impostata in blocco
016900040622      * 56    - ERRORE esiste LIBASP3 ciclo precedente non completato
017000071203      * 57    - ERRORE utente non autorizzato a lanciare la fatturtazione
017100991230      * 89/91 - ERRORE divise di fatturazione
017200991230     C*********--------********-------*******-------********
017300000000     C*---------------------------------------------------------------*
017400000000     C     *ENTRY        PLIST
017500000000     C                   PARM                    KPJBA
017600981026     C                   Z-ADD     1             CODUT             1 0
017700981026     C*---------------------------------------------------------------*
017800981026     C                   SETOFF                                       1195
017900981026     C                   CLEAR                   UNO
018000981026     C                   CLEAR                   SAVGGG
018100981026     C                   MOVEL     *ZEROS        PARAM
018200981026     C                   MOVEL     *BLANKS       VIDTSC
018300981026     C                   MOVEL     *BLANKS       VIDRB
018400981026     C                   Z-ADD     0             FIL
018500981026     C                   Z-ADD     0             VIDDFT
018600981026     C                   Z-ADD     0             VIDNFI
018700981026     C                   Z-ADD     0             VIDNFT
018800981026     C                   Z-ADD     0             VIDNFF
018900981026     C                   MOVEL     2             VIDTCL
019000981026     C                   CLEAR                   VIDDFI
019100981026     C                   CLEAR                   VIDDFF
019200981026     C*---------- RICERCA DITTA :
019300981026     C                   MOVEL     'SOC001'      TIPXSC
019400981026     C                   MOVEL     *BLANK        SOCXSC
019500981026     C                   EXSR      REPSOC
019600981026     C     RTNXSC        IFNE      '1'
019700981026     C                   MOVEL     XSOCDS        SOC001
019800981026     C                   MOVEL     xscrgs        RSUT             20
019900981026     c                   end
020000981026     c                   move      xscsoc        wsocieta
020100000000     C*---------------------------------------------------------------*
020200981026      *parametri per richiamo numeratori
020300981026     C     PNMR          PLIST
020400981026     C                   PARM                    XNMRIC            1
020500981026     C                   PARM                    XNMSOC
020600981026     C                   PARM                    XNMUNI
020700981026     C                   PARM                    XNMCTB
020800981026     C                   PARM                    XNMKEY
020900981026     C                   PARM                    XNMSER
021000981026     C                   PARM                    XNMCMT            1
021100981026     C                   PARM                    XNMDAT
021200981026     C                   PARM                    XNMIVA            1
021300981026     C                   PARM                    XNMREG            1
021400981026     C                   PARM                    XNMERR            1
021500981026     C                   PARM                    XNMNUM            9 0
021600981026     C                   PARM                    XNMAUT            1
021700981026     C                   PARM                    XNMDAG                         opzionale
021800891017     C     KTB           KLIST
021900891017     C                   KFLD                    CODUT
022000891017     C                   KFLD                    COD
022100891017     C     KTAB          KLIST
022200891017     C                   KFLD                    CODUT
022300891017     C                   KFLD                    COD               2
022400891017     C                   KFLD                    KEY               8
022500050223      *
022600050223      * carico la tabella ISTAT
022700050223      *
022800101013     c                   exsr      caristat
022900971022     C*---------------------------------------------------------------*
023000971022     C* CARICO TABELLA    FF  - FREQUENZA FATTURA PER METTERE LA DECOD
023100971022     C*                         A VIDEO
023200971022     C***
023300971022     C                   MOVEL     'FF'          COD
023400971022     C                   Z-ADD     0             B                 3 0
023500971022     C     KTB           CHAIN     TABEL                              70
023600971022     C     *IN70         DOWEQ     *OFF
023700971022     C     TBLFLG        IFEQ      ' '
023800971022     C                   ADD       1             B
023900971022     C                   MOVEL     TBLKEY        CFF(B)
024000971022     C                   MOVEL     TBLUNI        DFF(B)
024100971022     C                   MOVEL     TBLUNI        UFF(B)
024200971022     C                   ENDIF
024300971022     C     KTB           READE     TABEL                                  70
024400971022     C                   ENDDO
024500971022     C* CONTROLLO QUANTE DESCRIZIONI HO CARCATO
024600971022     C     55            DIV       B             RESTO             2 0
024700971022     C     RESTO         IFGT      15
024800971022     C                   Z-ADD     15            RESTO
024900971022     C                   ENDIF
025000971022     C**
025100971022     C     RESTO         SUB       3             LUDES             2 0
025200971022     C**
025300971022     C                   Z-ADD     1             B
025400971022     C                   Z-ADD     1             C                 3 0
025500971022     C**
025600971022     C                   CLEAR                   K55
025700971022     C     CFF(B)        DOWNE     ' '
025800971022     C                   MOVEL     CFF(B)        K55(C)
025900971022     C                   ADD       1             C
026000971022     C                   MOVEL     '='           K55(C)
026100971022     C**
026200971022     C                   MOVEA     DFF(B)        K12
026300981026     C                   Z-ADD     1             X                 2 0
026400971022     C     X             DOWLE     LUDES
026500971022     C                   ADD       1             C
026600971022     C                   MOVEL     K12(X)        K55(C)
026700971022     C                   ADD       1             X
026800971022     C                   ENDDO
026900971022     C**
027000971022     C                   ADD       2             C
027100971022     C                   ADD       1             B
027200971022     C                   ENDDO
027300971022     C                   MOVEA     K55           DESFF1
027400971022     C                   MOVEA     K55(29)       DESFF2
027500031009      *
027600000000     C     START         TAG
027700971022     C                   SETOFF                                       01
027800971022     C                   WRITE     SF01Z01
027900971022     C     FOR01         TAG
028000971022     C                   EXFMT     SF01D01
028100960924     C* FINE LAVORO
028200960924     C   KC              GOTO      FINE
028300071203     c* per 55 o 56 o 54 o 57 vado a FINE
028400050223     C                   if        *in55 = *on OR *in56 = *on  or *in54 = *on
028500071203     c                             or *in57
028600040622     C                   GOTO      FINE
028700040622     C                   endif
028800110127     c
028900110127     c* Data   del  giorno
029000110127     c                   movel     datasys       datadmy
029100110127     c     *dmy          movel     datadmy       w0060             6 0
029200110127     c                   movel     w0060         D1DATA
029300111215      * imposto data inizio anno corrente x fattura inizio mese anno corrente
029400111215     c                   time                    oggi             14 0
029500111216     C                   MOVE      OGGI          ANNOCOR           4 0
029600111216     C                   EVAL      PRIMO=(ANNOCOR *10000) + 101
029700071203      *-------------------------------------------------------------------*
029800071203      * verifico se sono un utente autorizzato a lanciare fatturazione    *
029900071203      *-------------------------------------------------------------------*
030000071203
030100071203     c                   clear                   xchkautds
030200071203     c                   eval      xcasoc = xscsoc
030300071203     c                   eval      xcagrp = xscgrp
030400071203     c                   eval      xcafnc = 'CONPNO'
030500071203     c                   eval      xcaprf = knmus
030600071203     c                   eval      xcatck = 'AP'
030700071203     c                   eval      xcacut = xsccut
030800071203     c                   eval      xcauff = xscuff
030900071203     c                   eval      xcalin = xsculm
031000071203     c                   call      'XCHKAUT'
031100071203     c                   parm                    XCHKAUTDS
031200071203      *
031300071203      * verifico il codice di ritorno  se diverso da zero segnalo l'errore
031400071203     c                   eval      *in57 = xcartn <> 0
031500071203     c   57              goto      for01
031600071203      *
031700071203      * se non risultano errori dal richiamo di xchkautds verifico i flag di abilitazione
031800071203      * immissione dati iva,non iva ed effetti
031900071203     c                   movel     xcadds        conpnods
032000071203     c                   eval      *IN57 = (ImmIVA <> '1' or ImmNON <> '1' or
032100071203     c                                      ImmATT <> '1')
032200071203     c   57              goto      for01
032300040622      **
032400040622      * verifico se esiste la libreria LIBASP3, in tal caso errore          postata sul blocco
032500040622     C                   call      'TNSF01C'
032600040622     C                   parm      *blanks       Err_libasp
032700040622     c                   eval      *in56 = (ERR_libasp = 'E')
032800040622     C   56              GOTO      FOR01
032900031013      **
033000031013      * verifico se la tabella FAT (Tabella che pilota la fatturazione) è impostata sul blocco
033100031013      * della fatturazione
033200031013     c                   clear                   tibs02ds
033300031013     C                   movel     'L'           T02tla
033400031013     C                   movel     'C'           T02mod
033500031013     C                   movel     knsif         t02sif
033600031013     C                   movel     'FAT'         t02cod
033700031013     C                   movel     'FAT'         t02ke1
033800031013     C                   call      'TIBS02R'
033900031013     C                   parm                    KPJBA
034000031013     C                   parm                    TIBS02DS
034100031013     C                   movel     t02uni        DFAT
034200031013      *
034300031013     c                   If        §fatbloc  = 'S'
034400031013     c                   seton                                        55
034500031013     C                   goto      for01
034600031013     c                   endif
034700971022     C* TIPO FATTURAZIONE
034800971022     C                   Z-ADD     1             B
034900971022     C     VIDTFT        LOOKUP    CFF(B)                                 30
035000971022     C     *IN30         IFEQ      *OFF
035100971022     C                   SETON                                        40
035200971022     C                   GOTO      FOR01
035300971022     C                   ENDIF
035400971022     C*
035500971022     C                   SETON                                        01
035600971023     C                   SETOFF                                       95
035700971022     C* TIPO FATTURAZIONE MENSILE: VISUALIZZO LE DUE DATE FATTURA
035800040702      * E IMPOSTO TIPO CLIENTE A 2 (CLIENTI CODIFICATI E NON )
035900971022     C     VIDTFT        IFEQ      '4'
036000971022     C                   SETON                                        02
036100131009     c                   exsr      Proposta_date
036200040702     C                   EVAL      VIDTCL = 2
036300971022     C                   ELSE
036400971022     C                   SETOFF                                       02
036500040702      * SE FATTURAZIONE SETTIMANALE IMPOSTO TIPO CLIENTE 0 (SOLO CODIFICATI)
036600040702     C     VIDTFT        IFEQ      '1'
036700040702     C                   EVAL      VIDTCL = 0
036800131008     c                   exsr      Proposta_date
036900040702     C                   ENDIF
037000971022     C                   ENDIF
037100971022     C                   MOVEL     UFF(B)        DSFF
037200971023     C                   MOVEL     §FFDES        VIDDTF
037300000208     c* carico a video le divise che devono essere fatturate
037400000210     c******!!!!!*****   exsr      cardiv
037500000210     C* PER ADESSO CARICO COME COSTANTI
037600020221     C                   MOVE      'EUR'         VIDDV1
037700031009      * giro con una unica moneta
037800031009     C*****              MOVE      'ITL'         VIDDV2
037900971022     C****
038000971022     C* EMETTO SECONDA VIDEATA
038100971022     C     FOR02         TAG
038200971022     C                   WRITE     SF01D01
038300971022     C                   EXFMT     SF01D02
038400971022     C** F3  - FINE LAVORO
038500971022     C   KC              GOTO      FINE
038600971022     C** F12 - RITRONO
038700971022     C   KL              GOTO      START
038800960924     C**
038900960924     C** F10 - GESTIONE STAMPE
039000981026     C   10              MOVEL     '3'           KRITB
039100000000     C   10              CALL      'BCH09'
039200000000     C                   PARM                    KPJBA
039300971022     C   10              GOTO      FOR02
039400891017     C*
039500971022     C* TIPO CLIENTI DA FATTURARE
039600961111     C     VIDTCL        IFGT      2
039700891023     C                   SETON                                        92
039800971022     C   92              GOTO      FOR02
039900891023     C                   END
040000040702     C* SE FATTURAZIONE SETTIMANALE ERRORE SE TIPO CLIENTE DIVERSO DA 0
040100040702     C                   IF        VIDTFT = '1' AND VIDTCL <> 0
040200040702     C                   SETON                                        93
040300040702     C   93              GOTO      FOR02
040400040702     C                   END
040500971022     C**
040600971022     C* FATTURAZIONE MENSILE: DATA FATTURA INIZIO MESE
040700971022    1C     *IN02         IFEQ      *ON
040800971022     C**
040900981026     c     *eur          test(d)                 viddfi                 99
041000981026     c                   if        *in99
041100981026     c     *dmy          test(d)                 viddfi                 99
041200981026     c  n99*dmy          move      viddfi        dataeur
041300981026     c  n99              move      dataeur       viddfi
041400981026     c                   else
041500981026     c                   move      viddfi        dataeur
041600981026     c                   end
041700981026     C   99              GOTO      FOR02
041800981026     c                   move      dataeur       dataiso
041900981026     c                   move      dataiso       amgfat
042000981026     c                   exsr      mvc
042100971022     C**
042200981026     c                   if        esito = *on
042300981026     C                   seton                                        99
042400981026     C                   GOTO      FOR02
042500981026     c                   end
042600971022     C** DATA FATTURA FINE MESE
042700981026     c     *eur          test(d)                 viddff                 98
042800981026     c                   if        *in98
042900981026     c     *dmy          test(d)                 viddff                 98
043000981026     c  n98*dmy          move      viddff        dataeur
043100981026     c  n98              move      dataeur       viddff
043200981026     c                   else
043300981026     c                   move      viddff        dataeur
043400981026     c                   end
043500981026     C   98              GOTO      FOR02
043600981026     c                   move      dataeur       dataiso
043700981026     c                   move      dataiso       amgddc
043800981026     c                   exsr      mvc
043900981026     C**
044000981026     c                   if        esito = *on
044100981026     C                   seton                                        98
044200981026     C                   GOTO      FOR02
044300981026     c                   end
044400971022   X1C                   ELSE
044500971022     C** DATA FATTURA SINGOLA
044600981026     c     *eur          test(d)                 viddft                 41
044700981026     c                   if        *in41
044800981026     c     *dmy          test(d)                 viddft                 41
044900981026     c  n41*dmy          move      viddft        dataeur
045000981026     c  n41              move      dataeur       viddft
045100981026     c                   else
045200981026     c                   move      viddft        dataeur
045300981026     c                   end
045400981026     C   41              GOTO      FOR02
045500981026     c                   move      dataeur       dataiso
045600981026     c                   move      dataiso       amgddt
045700981026     c                   exsr      mvc
045800981026     C**
045900981026     c                   if        esito = *on
046000981026     C                   seton                                        41
046100981026     C                   GOTO      FOR02
046200981026     c                   end
046300971210     C* SOTTRAGGO 1 ALLA DATA
046400971210     C**
046500981026     C                   SUBdur    1:*d          dataiso
046600981026     C                   move      dataiso       DATAM
046700971022    1C                   ENDIF
046800971022     C** DATA SPEDIZIONE
046900981026     c                   if        viddsp = 0
047000981026     c                   seton                                          97
047100981026     c                   else
047200981026     c     *eur          test(d)                 viddsp                 97
047300981026     c                   if        *in97
047400981026     c     *dmy          test(d)                 viddsp                 97
047500981026     c  n97*dmy          move      viddsp        dataeur
047600981026     c  n97              move      dataeur       viddsp
047700981026     c                   else
047800981026     c                   move      viddsp        dataeur
047900981026     c                   end
048000981026     c                   end
048100981026     C   97              GOTO      FOR02
048200981026     c                   move      dataeur       dataiso
048300050223      * verifico se esiste percentuale ISTAT
048400050223     c                   move      dataiso       dataspe
048500080219     c                   move      dataiso       dataspe_iso
048600050301     c                   movel     dataspe       annospe           4 0
048700050223     c                   clear                   perce
048800050223
048900101013     c                   do        999           pi
049000101014      * se data = a zero  vado a fine non dovrebbero esserci dei successivi
049100050223     c                   if        dia(pi) = 0
049200050223     c                   leave
049300050223     c                   endif
049400050301     c                   movel(p)  dia(pi)       annoist           4 0
049500050301      * se anno di spedizione uguale a quello dello scaglione prendo la percentuale
049600050301     c                   if        annospe = annoist
049700150205     c                   z-add     iac(pi)       perce             5 2
049800050301      *
049900050223     c                   else
050000060110     c                   iter
050100050223     c                   endif
050200050223
050300050223     c                   enddo
050400050223
050500050223      * Verifico se ho recuperato una percentuale istat
050600050223     c                   if        perce = *zeros
050700050223     c                   seton                                        54
050800050223     c                   goto      for02
050900050223     c                   endif
051000070202
051100070201      * verifico se esiste ultimo prezzo medio del gasolio
051200131009     c                   clear                   wgioset
051300070202     c                   movel     viddsp        wggmm             4
051400070202     c                   move      viddsp        waa               2
051500131009     c                   movel     wggmm         wggmmaa
051600070202     c                   move      waa           wggmmaa
051700070202     c                   call      'XGIOSE1'
051800070202     c                   parm                    wggmmaa
051900131009     c                   parm                    wgioset
052000131009     c                   move      wgioset       giorno            1 0
052100070202      * cerco il primo lunedì antecedente la data spedizione per cercare il prezzo medio gasolio
052200070202      * calcolo il numero di giorni che devo sottrarre dalla data ultima spedizione
052300070202
052400070202     c                   eval      giorno = giorno - 1
052500070202     c     *eur          movel     viddsp        dataiso
052600070202     c                   subdur    giorno:*d     dataiso
052700070202     c                   move      dataiso       wdata
052800070202     c     wdata         chain     tipmg01l
052900070202     c                   If        not %found(tipmg01l)
053000070202     c                   seton                                        53
053100070202     c                   goto      for02
053200070202     c                   endif
053300070201     C**
053400070201      *
053500971210     C**
053600971210     C** PER FATTURAZIONE NON MENSILE LA DATA SPEDIZIONE DEVE ESSERE
053700971210     C**  - 1 RISPETTO ALLA DATA FATTURA
053800080219     C  N02SAVAMG        IFNE      dataspe_iso
053900971210     C                   CLEAR                   DUE
054000080219     C                   MOVEL     dataspe_iso   SAVAMG
054100971210     C                   ENDIF
054200971210     C**
054300080219     C  N02dataspe_iso   IFNE      DATAM
054400971210     C     DUE           ANDEQ     ' '
054500971210     C                   SETON                                        46
054600971210     C                   MOVEL     '1'           DUE               1
054700971210     C                   GOTO      FOR02
054800971210     C                   ENDIF
054900111028     C**
055000111028     C** PER FATTURAZIONE MENSILE LA DATA SPEDIZIONE NON DEVE ESSERE
055100111028     C** MAGGIORE DELLE DATE FATTURAZIONE
055200111028     C**
055300111028     C                   IF        (*IN02 AND (dataspe_iso > amgfat or
055400111028     c                             dataspe_iso > amgddc)) or
055500111028     C                             (not *IN02 AND dataspe_iso > amgddt)
055600111028     C                   SETON                                        47
055700111028     C                   GOTO      FOR02
055800111028     C                   ENDIF
055900971022     C**
056000971022     C* PER FATTURAZIONE NON MESILE DEVE ESSERE UGUALE ALLA DATA FATT
056100971022     C* CONTROLLO SE LA DATA SPEDIZIONE IMMESSA E' TRA QUELLE
056200971022     C*  PREVISTE NEL TIPO FATTURAZIONE
056300971210     C   02              MOVEL     VIDDFF        W0020             2 0
056400971210     C  N02              MOVEL     VIDDFT        W0020             2 0
056500971022    1C     W0020         IFNE      SAVGGG
056600971022     C                   Z-ADD     W0020         SAVGGG            2 0
056700971022     C                   CLEAR                   UNO
056800971022    1C                   ENDIF
056900971022     C*
057000040702      * controllo solo in caso di fatturazione mensile
057100040702     c                   if        vidtft = '4'
057200971022     C     W0020         LOOKUP    DGG                                    30
057300971022    1C     *IN30         IFEQ      *OFF
057400971022     C     UNO           ANDEQ     ' '
057500971022     C                   SETON                                        44
057600971022     C                   MOVEL     '1'           UNO               1
057700971022     C                   GOTO      FOR02
057800971022    1C                   ENDIF
057900040702     c                   endif
058000971210     C* LA DATA INIZIO MESE NON DEVE INVECE ESSERE NELLA TABELLA DEI GG
058100971210     C*  FATTURABILI
058200971210     C   02              MOVEL     VIDDFI        W0020             2 0
058300971210     C   02W0020         LOOKUP    DGG                                    30
058400001115    1C   02*IN30         IFEQ      *ON
058500001115     C*!""!*             SETON                                        45
058600001115     C*!""!*             GOTO      FOR02
058700971210    1C                   ENDIF
058800991230     c** contolllo le divise inserite
058900991230     c                   clear                   viddd1
059000991230     c                   clear                   viddd2
059100991230      * se entrambe a blank errore
059200991230     c                   if        viddv1 = *blanks and  viddv2 = *blanks
059300991230     c                   seton                                        89
059400991230     c                   goto      for02
059500991230     c                   endif
059600991230      * se la prima = a blank e la seconda no errore
059700991230     c                   if        viddv1 = *blanks and  viddv2 <> *blanks
059800991230     c                   seton                                        89
059900991230     c                   goto      for02
060000991230     c                   endif
060100991230      * se entambi uguali errore
060200991230     c                   if        viddv1 = viddv2
060300991230     c                   seton                                        91
060400991230     c                   goto      for02
060500991230     c                   endif
060600991230      * controllo la prima divisa            1°
060700991230     c                   movel     'CV'          cod
060800991230     c                   movel(p)  viddv1        key
060900131009     c     ktab          chain     tabel                              49
061000991230      * divisa inesistente
061100991230     c                   if        *in49 or tblflg <> ' '
061200991230     c                   seton                                        89
061300991230     c                   goto      for02
061400991230     c                   endif
061500991230      * divisa non abilitata alla fatturazione
061600991230     c                   movel     tbluni        dscv
061700991230     c                   if        §cvabi <> 'S'
061800991230     c                   seton                                        88
061900991230     c                   goto      for02
062000991230     c                   endif
062100991230      * decodifico la divisa
062200991230     c                   movel     §cvdes        viddd1
062300971022     C**
062400991230      * controllo la seconda divisa se diversa da blanks
062500991230     c                   if        viddv2 <> *blanks
062600991230     c                   movel     'CV'          cod
062700991230     c                   movel(p)  viddv2        key
062800131009     c     ktab          chain     tabel                              50
062900991230      * divisa inesistente
063000991230     c                   if        *in50 or tblflg <> ' '
063100991230     c                   seton                                        91
063200991230     c                   goto      for02
063300991230     c                   endif
063400991230      * divisa non abilitata alla fatturazione
063500991230     c                   movel     tbluni        dscv
063600991230     c                   if        §cvabi <> 'S'
063700991230     c                   seton                                        90
063800991230     c                   goto      for02
063900991230     c                   endif
064000991230      * decodifico la divisa
064100991230     c                   movel     §cvdes        viddd2
064200991230      *
064300991230     c                   endif
064400991230     C**
064500000000     C                   SETOFF                                       9411
064600000000     C     VIDTSC        IFNE      *BLANKS
064700000000     C                   DO        40            A
064800000000     C     FIL(A)        IFNE      0
064900000000     C                   SETON                                        11
065000000000     C                   END
065100000000     C                   END
065200000000     C  N11              SETON                                        94
065300971022     C   94              GOTO      FOR02
065400000000     C** 94 ON INDICATA OPZIONE E NON FILIALI
065500000000     C                   ELSE
065600000000     C                   DO        40            A
065700000000     C     FIL(A)        IFGT      0
065800000000     C                   SETON                                        94
065900000000     C                   END
066000000000     C                   END
066100971022     C   94              GOTO      FOR02
066200000000     C                   END
066300000000     C                   DO        40            A                 2 0
066400000000     C     FIL(A)        IFGT      0
066500910111     C     FIL(A)        CHAIN     AZORG                              96
066600971022     C   96              GOTO      FOR02
066700960925     C** P.O. INESISTENTE
066800000000     C                   END
066900000000     C                   END
067000981026     c*controllo cliente se immesso
067100981026    1C     VIDKSC        IFGT      0
067200981026     C     Keycli        klist
067300981026     C                   kfld                    wsocieta
067400981026     C                   kfld                    wksc              8
067500981026     C                   kfld                    k£fil
067600981026     C                   kfld                    k£lin
067700981026     c                   move      *zeros        wksc
067800981026     c                   move      vidksc        wksc
067900981026     C     *LIKE         DEFINE    clnlineav     k£lin
068000981026     C     *LIKE         DEFINE    clnfiliale    k£fil
068100981026     C     Keycli        CHAIN     ancln01l                           30
068200981026     c                   if        clndtfirpc > *loval
068300981026     c                   seton                                        30
068400981026     c                   endif
068500981026     C  N30clnsogg       CHAIN     ansog01l                           30
068600981026    2C     *IN30         IFEQ      *ON
068700981026     C                   SETON                                        43
068800981026     C                   GOTO      FOR02
068900981026    2C                   ENDIF
069000981026     C                   MOVEL     sogdes        VIDDES
069100981026    2C                   ENDIF
069200971022     C**
069300891016     C* REPERISCE  NUMERAZIONE
069400941104     C                   MOVEL     'QC'          COD
069500941104     C                   MOVEL     *BLANKS       KEY
069600941104     C                   MOVEL     '2'           KEY
069700131009     C     KTAB          CHAIN     TABEL                              50
069800971022     C   50              GOTO      FOR02
069900941104     C                   MOVEL     TBLUNI        DSQC2
070000971023     C*
070100981027     C                   MOVE      '1'           XNMRIC            1
070200981026     C                   MOVEl     §QCRIV        wserie            4            REGISTRO VEND.
070300981026     C** PER FATTURE INIZIO MESE
070400981026     C**  E FATTURAZIONE NON MENSILE
070500981027     C   02              MOVEL     amgfat        xnmdat                         ANNO FATTURA
070600981027     C  N02              MOVEL     amgddt        xnmdat                         ANNO FATTURA
070700981027     C   02              MOVEL     amgfat        xnmdag                         ANNO FATTURA
070800981027     C  N02              MOVEL     amgddt        xnmdag                         ANNO FATTURA
070900981026     C                   MOVE      §QCFIV        wserie                         LIBRO IVA VEND.
071000981026     c                   exsr      repnum
071100981026     C     xnmerr        ifgt      '0'
071200981026     C     xnmerr        andlt     '7'
071300981026     C                   seton                                        51
071400981026     C                   GOTO      FOR02
071500981026     C                   end
071600981026     C     xnmerr        IFeq      '7'
071700891017     C                   SETON                                        99
071800971023     C                   GOTO      FOR02
071900971023     C                   END
072000971022     C**
072100981026     C   02              Z-ADD     xnmnum        VIDNFI
072200971022     C**
072300981026     C  N02              Z-ADD     xnmnum        VIDNFT
072400971022     C** PRIMO NUMERO DI FATTURA INIZIO MESE
072500971022     C     *IN02         IFEQ      *ON
072600981026     C                   MOVE      amgddc        xnmdat                         ANNO FATTURAZ.
072700981027     C                   MOVE      amgddc        xnmdag                         ANNO FATTURAZ.
072800981026     C                   MOVE      §QCFI2        wserie                         LIBRO IVA FT.A.
072900981026     c                   exsr      repnum
073000981026     C     xnmerr        ifgt      '0'
073100981026     C     xnmerr        andlt     '7'
073200981026     C                   seton                                        52
073300981026     C                   GOTO      FOR02
073400981026     C                   end
073500981026     C     xnmerr        IFeq      '7'
073600940927     C                   SETON                                        98
073700971023     C                   GOTO      FOR02
073800971023     C                   END
073900971022     C**
074000981026     C                   Z-ADD     xnmnum        VIDNFF
074100971022     C**
074200940927     C** PRIMO NUMERO DI FATTURA FINE MESE
074300971022     C                   ENDIF
074400971022     C**
074500960924     C  NKF              SETON                                        95
074600971022     C   95
074700971022     CANNKF              GOTO      FOR02
074800960924     C* F6 - ELABORAZIONE
074900960924     C     *INKF         IFEQ      *ON
075000960924     C                   MOVEL     FIL           FILX
075100981026     c                   clear                   dataeur
075200000216     c* pulizia del file etichette
075300000217     C                   MOVEL(P)  cm1(1)        comman
075400000217     C                   CALL      'QCMDEXC'
075500000217     C                   PARM                    comman
075600000217     C                   PARM                    lenght
075700000217     c*
075800981026     c                   if        viddfi = 0
075900981026     c                   move      dataeur       viddfi
076000981026     c                   end
076100981026     c                   if        viddff = 0
076200981026     c                   move      dataeur       viddff
076300981026     c                   end
076400981026     c                   if        viddft = 0
076500981026     c                   move      dataeur       viddft
076600981026     c                   end
076700130325     c*
076800130325     c* se fatturazione mensile emissione window  per il lancio del recupero variazioni
076900130325     c* post-fatturazione
077000130325      * se lancio fatturazione mensile / nessuna scelta parzializzante codice cliente e filiale
077100130325     c                   if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
077200130325     c                   exsr      sr_rpf
077300130325     c                   endif
077400040622      ***
077500040622      *  Creo libreria LIBASP3
077600040622     C                   IF        VIDTFT = '4'
077700040622     c                   eval      Wtft = 'MENSILE    '
077800040622     c                   ELSE
077900040622     c                   eval      Wtft = 'SETTIMANALE'
078000040622     c                   ENDIF
078100040622     c                   movel     VIDDSP        W008A
078200040622     c                   eval      Wtext = 'Fattura ' + wtft +
078300040713     c                             ' data sped. al '   +
078400040622     c                             %subst(W008A:1:2) +  '/' +
078500040622     c                             %subst(W008A:3:2) +  '/' +
078600040713     c                             %subst(W008A:7:2)
078700040622     c                   CALL      'TNSF01C1'
078800040622     c                   parm                    WText
078900111215      ***
079000111215      * se data fattura inizio mese è uguale al 1/1/ anno in corso e non ci sono
079100111215      * parzializzazioni cliente / filiale piloto la copia delle tariffe annuale in EDPCONF_TA
079200111215      ***
079300140528      * 28/05/14 AG non copiamo più le traiffe nell EDPCONF_TA in quanto ora teniamo le tariffa
079400140528      * oltre 365 giorni. manteniamo quella del mese precedente
079500111215     c                   if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
079600111216     c                   move      amgfat        wdata
079700140528     c****               If        wdata = primo
079800140528     C****               MOVEL     PARAM         KPJBU
079900140528     C****               MOVEL     'SF%1'        KCOAZ
080000140528     C***                CALL      'BCH10'
080100111216     C****               CALL      'TNSF01C2'
080200140528     C****               PARM                    KPJBA
080300140528     c****               endif
080400121211      * se fatturazione senza parzializzazioni, fatturazione mensile
080500121211      * devo copiare le tariffe per confronto fatturazione al mese precedente
080600121212     c                   eval      dataiso = amgfat - %Months(1)
080700121212     c                   eval      wanno = %subdt(dataiso:*Years)
080800121212     c                   eval      sf3AA =
080900121212     c                             %dec(%subst(%editc(wanno:'X'):3:2):2:0)
081000121212     c                   eval      sf3MM = %subdt(dataiso:*Months)
081100121211     c                   eval      kpjbu = ParmSF3
081200121211     c                   eval      kcoaz = 'SF%2'
081300121211     c                   call      'BCH10'
081400121211     c****               call      'TNSF01C3'
081500121211     c                   parm                    kpjba
081600121211
081700111216     c                   endif
081800130325      ***
081900130325      * creazione bolle di recupero variazioni post-fatturazione
082000130325      ***
082100130325      * se lancio fatturazione mensile / nessuna scelta parzializzante codice cliente e filiale
082200130325     c                   if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
082300130325     c* simulazione
082400130325     c                   if        w01sml='SI'
082500130325     C                   clear                   KPJBU
082600130326     C                   MOVEL     'SF55'        KCOAZ
082700130325     C                   CALL      'BCH10'
082800130325     C****               CALL      'TNSF55C'
082900130325     C                   PARM                    KPJBA
083000130325     c                   CLEAR                   KPJBU
083100130325     c                   endif
083200130325     c* creazione bolle
083300130325     c                   if        w01cre='SI'
083400130325     C                   clear                   KPJBU
083500130326     C                   MOVEL     'SF5B'        KCOAZ
083600130325     C                   CALL      'BCH10'
083700130325     C****               CALL      'TNSF55R'
083800130325     C                   PARM                    KPJBA
083900130325     c                   CLEAR                   KPJBU
084000130325     c                   endif
084100040622      ***
084200061009      * preparazione bole fuori misura DPD
084300150122      *** dalla fatturazione di Febbraio 2015 non si creano più le bolle Fuori misura DPD
084400150122      *** asterisco le istruzioni ... in seguito tolgo il programma
084500061009      * se lancio fatturazione mensile / nessuna scelta parzializzante codice cliente e filiale
084600130325     c*****              if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
084700150122     C** 22/01           MOVEL     PARAM         KPJBU
084800150122     C** 22/01           MOVEL     'SF50'        KCOAZ
084900150122     C** 22/01           CALL      'BCH10'
085000061009     C****               CALL      'TNSF50C'
085100150122     C** 22/01           PARM                    KPJBA
085200150122     c** 22/01           CLEAR                   KPJBU
085300061009     c                   endif
085400991230     c* gestione delle divise
085500991230     c* un giro per ogni divisa
085600991230     c* se esistono due divise lancio il primo giro
085700991230     c                   if        viddv1 <> *blanks and viddv2 <> *blanks
085800991230     c* 1° giro
085900991230     c* aggiornamento  tassazione  per tutte le bolle
086000991230     c* non si stampa il manca tariffa
086100991230     c                   eval      viddiv = viddv1
086200991230     c                   eval      vidagg = 'S'
086300991230     c                   eval      vidsta = 'N'
086400991230     c*
086500050426     C                   MOVEL     'SF02'        KCOAZ
086600960924     C                   MOVEL     PARAM         KPJBU
086700050426     C                   CALL      'BCH10'
086800050426     C**                 CALL      'TNSF02C'
086900941124     C                   PARM                    KPJBA
087000991230     c* 2° giro
087100991230     c* nesun aggiornamento  tassazione  per le bolle con divisa diversa
087200991230     c* si stampa il manca tariffa
087300991230     c                   eval      viddiv = viddv2
087400991230     c                   eval      vidagg = 'N'
087500991230     c                   eval      vidsta = 'S'
087600991230     c*
087700050426     C                   MOVEL     'SF02'        KCOAZ
087800991230     C                   MOVEL     PARAM         KPJBU
087900050426     C                   CALL      'BCH10'
088000050426     C***                CALL      'TNSF02C'
088100991230     C                   PARM                    KPJBA
088200991230     c*
088300991230     c                   else
088400991230     c* esiste solo una divisa mi comporto come se fossi un secondo giro
088500991230     c*
088600991230     c* nesun aggiornamento  tassazione  per le bolle con divisa diversa
088700991230     c* si stampa il manca tariffa
088800991230     c                   eval      viddiv = viddv1
088900991230     c                   eval      vidagg = 'N'
089000991230     c                   eval      vidsta = 'S'
089100991230     c*
089200050427     C                   MOVEL     'SF02'        KCOAZ
089300991230     C                   MOVEL     PARAM         KPJBU
089400050427     C                   CALL      'BCH10'
089500050427     C***                CALL      'TNSF02C'
089600991230     C                   PARM                    KPJBA
089700991230     c**
089800991230     c                   endif
089900991230     c*
090000000209     c*lancio la stampa delle etichette
090100000209     c*
090200000209     C                   MOVEL     'SF15'        KCOAZ
090300000209     C                   MOVEL     PARAM         KPJBU
090400001221     C                   CALL      'BCH10'
090500001221     C**                 CALL      'TNSF15R'
090600000209     C                   PARM                    KPJBA
090700110127     c
090800110127     c** Sottometto azione schedulata dati statistiche clienti Roma
090900110127     C                   IF        VIDTFT = '4'
091000110127     c                   movel     amgfat        stf1DFI
091100110127     c                   movel     amgddc        stf1DFF
091200110127     C                   MOVEL     'STF1'        KCOAZ
091300110127     c                   eval      d1oras='1900'
091400110127     c                   movel     scheds        kbuff
091500110127     C                   MOVEL     PARAMSTF1     KPJBU
091600110127     c
091700110127     C                   CALL      'BCH10'
091800110127     C                   PARM                    KPJBA
091900110127     c                   endif
092000110127     c
092100960924     C                   ENDIF
092200130108
092300130108      /free
092400130108       //?Se fatturazione senza parzializzazioni e fatturazione mensile
092500130108       //?devo memorizzare la data ultima spedizione fatturata in tabella RPF
092600130108        IF  VIDtft = '4' and VIDksc = 0 and VIDtsc = ' ';
092700130108          clear dRPFfatgio;
092800130108          TBEcod = 'RPF';
092900130108          TBEke1 = 'FATTGIO';
093000130108          chain (TBEcod:TBEke1) TNTBE01L;
093100130108          IF  %found(TNTBE01L);
093200130108            §RPFfatdsp = dataSPE;
093300130108            TBEuni = dRPFfatgio;
093400130108            UPDATE  TNTBE000;
093500130108          ENDIF;
093600130108        ENDIF;
093700130108      /end-free
093800130108
093900960924     C**
094000000000     C     FINE          TAG
094100000000     C                   SETON                                        LR
094200981026     C*----------------------------------------------------*
094300981026     C* Reperisco numeratore
094400981026     C*----------------------------------------------------*
094500981026     c     repnum        begsr
094600981026     C                   MOVE      wsocieta      XNMSOC            3
094700981026     C                   MOVE      *BLANKS       XNMUNI            8
094800981026     C                   MOVE      'CG'          XNMCTB            2
094900981026     C                   MOVE      *BLANKS       XNMKEY            8
095000981026     C                   MOVE      WSERIE        XNMSER            4
095100981026     C                   MOVE      *OFF          XNMCMT            1
095200981026     C                   MOVE      *On           XNMIVA            1
095300981026     C                   MOVE      *OFF          XNMREG            1
095400981027     C                   MOVE      '0'           XNMerr            1
095500981026     C*
095600981026     C                   CALLB     'XNMR'        PNMR
095700981026     C                   endsr
095800981026     C*----------------------------------------------------*
095900981026     C* controllo libro giornale
096000981026     C*----------------------------------------------------*
096100981026     c     mvc           begsr
096200981026     c                   move      dataiso       dtreg
096300981026     c                   move      dataiso       dtlgio
096400981026     C                   CALLB     'NDMVC106'
096500981026     C                   PARM      xscsoc        Societa
096600981026     C                   PARM      'CG'          CTB
096700981026    >C                   PARM      'D'           TpReg
096800981026    >C                   PARM                    DtLGio
096900981026    >C                   PARM                    DtReg
097000981026     C                   PARM      *on           GesMsg
097100981026     C                   PARM                    Esito
097200981026     c                   endsr
097300981026     C*----------------------------------------------------*
097400981026     C* Reperimento dati società
097500981026     C*----------------------------------------------------*
097600981026     C     REPSOC        BEGSR
097700981026     C                   CALL      'XSOC'
097800981026     C                   PARM                    TIPXSC            6
097900981026     C                   PARM                    SOCXSC            3
098000981026     C                   PARM                    CDSXSC            9 0
098100981026     C                   PARM                    MODXSC            3
098200981026     C                   PARM      *blanks       RTNXSC            1
098300981026     C                   PARM                    XSOCDS
098400981026     C                   PARM                    KPJBA
098500981026     C                   ENDSR
098600000208     C*******************************************************************
098700000208     C** CARICO DIVISE A VIDEO
098800000208     C*******************************************************************
098900000208     C     CARDIV        BEGSR
099000000208     C**
099100000208     C                   MOVEL     'CV'          COD
099200000208     c                   clear                   key
099300000208     C     KTab          SETLL     TABEL
099400000208     C                   DO        *HIVAL
099500000208     C     KTb           READE     TABEL                                  07
099600000208     c   07              leave
099700000208     c*
099800000208     c                   if        tblflg <> ' '
099900000208     c                   iter
100000000208     c                   endif
100100000208     c*
100200000208     C     TBLKEY        IFNE      *BLANKS
100300000208     C                   MOVEL     TBLUNI        DSCV
100400000208     c*
100500000208     c                   if        §cvabi = 'S'
100600000208     c*
100700000208     c                   if         viddv1 = *blank
100800000208     c                   movel     tblkey        viddv1
100900000208     c                   iter
101000000208     c                   endif
101100000208     c*
101200000208     c                   if         viddv2 = *blank
101300000208     c                   movel     tblkey        viddv2
101400000208     c                   leave
101500000208     c                   endif
101600000208     c*
101700000208     c                   endif                                                  cvabi
101800000208     c*
101900000208     c                   endif                                                  key > '    '
102000000208     c* torno a leggere
102100000208     C                   ENDDO
102200000208     C*
102300000208     C                   ENDSR
102400050223      ****************************************************
102500050223      ** CARICAMENTO IAC
102600050223      ****************************************************
102700101013     c     CARISTAT      BEGSR
102800101013
102900101013      /free
103000101013       //?Carico scatti ISTAT
103100101013         SISsca = 0;
103200101013         clear DIA;
103300101013         setll (SISsca) TISIS01L;
103400101013
103500101013         DOW  not $EoF;
103600101013
103700101013           read TISIS01L;
103800101013
103900101013           IF  %Eof(TISIS01L);
104000101013             $EoF = *on;
104100101013             leave;
104200101013           ENDIF;
104300101013
104400101013           DIA(SISsca) = SISdata;
104500101013           IAC(SISsca) = SISpunti;
104600101013
104700101013         ENDDO;
104800101013
104900101013      /end-free
105000050223     c                   ENDSR
105100130325      ****************************************************
105200130325      ** gestione window per recupero variazioni post-fatt
105300130325      ****************************************************
105400130325     c     sr_rpf        BEGSR
105500130325     c                   seton                                        04
105600130325     c                   eval      w01sml='NO'
105700130325     c                   eval      w01cre='SI'
105800130325     c* su as888 imposto default al contrario
105900130325     c                   if        %subst(knsif:7:3)<>'201'
106000130326     c                   eval      w01sml='NO'
106100130325     c                   eval      w01cre='NO'
106200130325     c                   endif
106300130325     c                   eval      $fine='0'
106400130325     c*
106500130325     c                   dow       $fine='0'
106600130325     c                   exfmt     sf01w01
106700130325     c                   select
106800130325     c* f15=Modifica richieste
106900130325     c                   when      *inkp
107000130325     c                   setoff                                       04
107100130325     c                   iter
107200130325     c                   other
107300130325     c* f6=conferma
107400130325     c                   if        *inkf
107500130325     c                   eval      $fine='1'
107600130325     c                   endif
107700130325     c                   endsl
107800130325     c                   enddo
107900130325     c
108000130325     c                   endsr
108100131008     C*----------------------------------------------------*
108200131008     C* Proposta date di fatturazione
108300131008     C*----------------------------------------------------*
108400131008     c     Proposta_date begsr
108500131008
108600131009     c                   time                    w0140
108700131008      *
108800131008      * Se fatturazione mensile propongo le date fattura di inizio mese (primo giorno del mese
108900131008      * in cui sto fatturando) e data fattura fine mese (giorno precedente fattura inizio mese)
109000131008      * e data spedizione (uguale alla data fattura fine mese)
109100131008      *
109200131009     c                   If        VIDtft = '4'
109300131009     c                   move      w0140         wData1
109400131009     c                   z-add     01            wgiorno1
109500131008     c                   movel     wdata1        viddfi
109600131009     c                   move      viddfi        dataeur
109700131008     c                   move      dataeur       dataiso
109800131008     c                   subdur    1:*d          dataiso
109900131009     c                   move      dataiso       dataeur
110000131009     c                   move      dataeur       viddff
110100131009     c                   move      viddff        viddsp
110200131008     c                   Endif
110300131008     c
110400131008      *
110500131008      * Se fatturazione settimanale propongo come data fattura il lunedì della settimana in
110600131008      * corso e come data spedizione il giorno precedente la fattura
110700131008      *
110800131009     c                   If        VIDtft = '1'
110900131008
111000131008     c                   Clear                   wgioset
111100131009     c                   movel     datasys       datadmy
111200131009     c     *dmy          movel     datadmy       w0060
111300131009     c                   move      w0060         wGGMMAA
111400131008     c                   Call      'XGIOSE1'
111500131008     c                   Parm                    wggmmaa
111600131008     c                   Parm                    wgioset
111700131009     c                   move      wgioset       giorno
111800131008     c
111900131009     c                   movel     datadmy       dataiso
112000131009      * se oggi che lancio non è Lunedì devo trovare che giorno è Lunedì
112100131009     c                   If        giorno  > 1
112200131008      * calcolo il numero dei giorni da diminuire
112300131009     c                   eval      gg = giorno -1
112400131008     c                   subdur    gg:*d         dataiso
112500131008     c                   endif
112600131009     c                   move      dataiso       dataeur
112700131009     c                   move      dataeur       viddft
112800131008      * data spedizione
112900131008     c                   subdur    1:*d          dataiso
113000131009     c                   move      dataiso       dataeur
113100131009     c                   move      dataeur       viddsp
113200131008     c                   Endif
113300131008     c
113400131008     c                   endsr
113500000217**   cm1 - comandi
113600000217CLRPFM FILE(ETLAV00F)
