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
001400151015     fSIPES02L  if   e           k disk
001500991230     FTNSF01D   CF   E             WORKSTN
001600971022     D CFF             S              1    DIM(15)                              *TAB FREQUENZA FT
001700971022     D DFF             S             12    DIM(15)                              *TAB DESCR.FREQU
001800971022     D UFF             S             89    DIM(15)                              *TBLUNI DI FREQU
001900971022     D K12             S              1    DIM(12)                              COMODO
002000971022     D K55             S              1    DIM(55)                              COMODO
002100150205     D IAC             S              5  2 DIM(999)                             PERCENTUALI ISTAT
002200101013     D DIA             S              8  0 DIM(999)                             DATE SCATTI ISTAT
002300000217     D cm1             S             80    DIM(1) CTDATA PERRCD(1)              *clrpfm etlav
002400000000     D PARAM           DS
002500981026     D  VIDDFI                        8  0
002600981026     D  VIDDFF                        8  0
002700981026     D  VIDDSP                        8  0
002800981026     D  VIDTFT                        1
002900981026     D  VIDTCL                        1  0
003000981026     D  VIDTSC                        1
003100981026     D  FILx                          3  0 dim(40)
003200981026     D  VIDRB                         2
003300981026     D  VIDDFT                        8  0
003400981026     D  VIDKSC                        7  0
003500981026     D  wsocieta                      3
003600991230     D  VIDDIV                        3                                         *divisa da fatturare
003700991230     D  VIDAGG                        1                                         *AGG.bolle altre div
003800991230     D  VIDSTA                        1                                         *stampa manca tariff
003900000000     D FILDS           DS
004000891016     D  VIDF01                 1      3  0
004100891016     D  VIDF02                 4      6  0
004200891016     D  VIDF03                 7      9  0
004300891016     D  VIDF04                10     12  0
004400891016     D  VIDF05                13     15  0
004500891016     D  VIDF06                16     18  0
004600891016     D  VIDF07                19     21  0
004700891016     D  VIDF08                22     24  0
004800891016     D  VIDF09                25     27  0
004900891016     D  VIDF10                28     30  0
005000891016     D  VIDF11                31     33  0
005100891016     D  VIDF12                34     36  0
005200891016     D  VIDF13                37     39  0
005300891016     D  VIDF14                40     42  0
005400891016     D  VIDF15                43     45  0
005500891016     D  VIDF16                46     48  0
005600891016     D  VIDF17                49     51  0
005700891016     D  VIDF18                52     54  0
005800891016     D  VIDF19                55     57  0
005900891016     D  VIDF20                58     60  0
006000891016     D  VIDF21                61     63  0
006100891016     D  VIDF22                64     66  0
006200891016     D  VIDF23                67     69  0
006300891016     D  VIDF24                70     72  0
006400891016     D  VIDF25                73     75  0
006500891016     D  VIDF26                76     78  0
006600891016     D  VIDF27                79     81  0
006700891016     D  VIDF28                82     84  0
006800891016     D  VIDF29                85     87  0
006900891016     D  VIDF30                88     90  0
007000891016     D  VIDF31                91     93  0
007100891016     D  VIDF32                94     96  0
007200891016     D  VIDF33                97     99  0
007300891016     D  VIDF34               100    102  0
007400891016     D  VIDF35               103    105  0
007500891016     D  VIDF36               106    108  0
007600891016     D  VIDF37               109    111  0
007700891016     D  VIDF38               112    114  0
007800891016     D  VIDF39               115    117  0
007900891016     D  VIDF40               118    120  0
008000891016     D  FIL                    1    120  0
008100891016     D                                     DIM(40)                              FILIALI ESCLUSE
008200110127     d
008300110127     D PARAMSTF1       DS
008400110127     D  stf1dfi                       8  0
008500110127     D  stf1dff                       8  0
008600110127     d
008700121211
008800121211     d ParmSF3         ds
008900121212     d  sf3AA                         2  0
009000121212     d  sf3MM                         2  0
009100121211
009200110127      * DATA E ORA SCHEDULAZIONE PER KPJBA
009300110127     D SCHEDS          DS
009400110127     D  D1DATA                 1      6
009500110127     D  D1ORAS                 7     10
009600110127     d
009700131008
009800131008     d wData1          ds
009900131009     D  Wgiorno1               1      2  0
010000131009     D  Wmese1                 3      4  0
010100131009     D  Wanno1                 5      8  0
010200131008
010300941104     D DSQC2         E DS
010400951002     D*-------------------
010500951002     D* DS DATI VARI PER C/E
010600951002     D*-------------------
010700971022     D DSFF          E DS
010800971022     D  DGG                   31     40  0
010900971022     D                                     DIM(5)                               *GG FATTURAZ
011000991230     D*-------------------
011100991230     D DSCV          E DS                                                       tabella divise
011200031009     D*-------------------
011300031009     D TIBS02DS      E DS                                                       tabelle
011400031009     D DFAT          E DS                                                       tabella FAT
011500031009     D*-----------
011600000000     D KPJBA         E DS
011700071130     D*-----------
011800981026     D soc001        e ds                  extname(xsoc001ds)
011900981026     D xsocds          DS          1000
012000071203     D xchkautds     e ds
012100071130     D conpnods      e ds
012200071130     D*-----------
012300130108
012400130108       //?Ds tabella RPF (data ultima spedizione fatturata)
012500130108     d dRPFfatgio    e ds
012600120214
012700040622     d Err_libasp      s              1
012800040622     d Wtft            s             11
012900040622     d Wtext           s             50
013000040622     d W008a           s              8
013100110127     d datasys         s               d   datfmt(*iso) inz(*sys)
013200110127     d datadmy         s               d   datfmt(*dmy)
013300110127     d dataiso         s               d   datfmt(*iso)
013400981026     d dataeur         s               d   datfmt(*eur)
013500981026     D  XNMDAG         S               d   datfmt(*iso)
013600981026     d  XNMDAT         S               d   datfmt(*iso)
013700981026     d amgfat          S               d   datfmt(*iso)
013800981026     d amgddc          S               d   datfmt(*iso)
013900981026     d amgddt          S               d   datfmt(*iso)
014000981026     d savamg          S               d   datfmt(*iso)
014100981026     d datam           S               d   datfmt(*iso)
014200981026    >D Societa         S              3
014300981026    >D Ctb             S              2
014400981026     D TpReg           S              1
014500981026     D DtReg           S             10
014600981026     D DtLGio          S             10
014700981026     D Esito           S              1
014800981026     D Gesmsg          S              1
014900131009     D Gg              S              2  0
015000101013     D Pi              s              3  0                                      *indice ISTAT
015100050223     D Dataspe         s              8  0
015200080219     D Dataspe_iso     s               d   datfmt(*iso)
015300151015     d mesespe         s              2s 0 inz
015400070202     D wdata           s              8  0
015500111216     D PRIMO           s              8  0
015600131008     d wggmmaa         s              6
015700131008     d wgioset         s              1
015800131009     d w0140           s             14  0
015900000217     D lenght          S             15  5 INZ(80)                              *Lunghezza cmd CLP
016000000217     D comman          S             80                                         *valore cmd CLP
016100101013     d $EoF            s              1n   inz(*off)
016200130325     d $Fine           s               n   inz(*off)
016300121212     d wanno           s              4  0
016400991230     C*********--------********-------*******-------********
016500991230     C* INDICATORI
016600991230     C*********--------********-------*******-------********
016700991230      * 50    - CHAIN  tabel00f
016800070202      * 53    - Errore manca prezzo medio del gasolio settimana in corso
016900050223      * 54    - Errore manca tabella o percentuale ISTAT blocco
017000031009      * 55    - ERRORE Tabella pilota di fatturazione impostata in blocco
017100040622      * 56    - ERRORE esiste LIBASP3 ciclo precedente non completato
017200071203      * 57    - ERRORE utente non autorizzato a lanciare la fatturtazione
017300151015      * 58    - ERRORE non elaborata la statistica pesi per il mese da fatt.
017400991230      * 89/91 - ERRORE divise di fatturazione
017500991230     C*********--------********-------*******-------********
017600000000     C*---------------------------------------------------------------*
017700000000     C     *ENTRY        PLIST
017800000000     C                   PARM                    KPJBA
017900981026     C                   Z-ADD     1             CODUT             1 0
018000981026     C*---------------------------------------------------------------*
018100981026     C                   SETOFF                                       1195
018200981026     C                   CLEAR                   UNO
018300981026     C                   CLEAR                   SAVGGG
018400981026     C                   MOVEL     *ZEROS        PARAM
018500981026     C                   MOVEL     *BLANKS       VIDTSC
018600981026     C                   MOVEL     *BLANKS       VIDRB
018700981026     C                   Z-ADD     0             FIL
018800981026     C                   Z-ADD     0             VIDDFT
018900981026     C                   Z-ADD     0             VIDNFI
019000981026     C                   Z-ADD     0             VIDNFT
019100981026     C                   Z-ADD     0             VIDNFF
019200981026     C                   MOVEL     2             VIDTCL
019300981026     C                   CLEAR                   VIDDFI
019400981026     C                   CLEAR                   VIDDFF
019500981026     C*---------- RICERCA DITTA :
019600981026     C                   MOVEL     'SOC001'      TIPXSC
019700981026     C                   MOVEL     *BLANK        SOCXSC
019800981026     C                   EXSR      REPSOC
019900981026     C     RTNXSC        IFNE      '1'
020000981026     C                   MOVEL     XSOCDS        SOC001
020100981026     C                   MOVEL     xscrgs        RSUT             20
020200981026     c                   end
020300981026     c                   move      xscsoc        wsocieta
020400000000     C*---------------------------------------------------------------*
020500981026      *parametri per richiamo numeratori
020600981026     C     PNMR          PLIST
020700981026     C                   PARM                    XNMRIC            1
020800981026     C                   PARM                    XNMSOC
020900981026     C                   PARM                    XNMUNI
021000981026     C                   PARM                    XNMCTB
021100981026     C                   PARM                    XNMKEY
021200981026     C                   PARM                    XNMSER
021300981026     C                   PARM                    XNMCMT            1
021400981026     C                   PARM                    XNMDAT
021500981026     C                   PARM                    XNMIVA            1
021600981026     C                   PARM                    XNMREG            1
021700981026     C                   PARM                    XNMERR            1
021800981026     C                   PARM                    XNMNUM            9 0
021900981026     C                   PARM                    XNMAUT            1
022000981026     C                   PARM                    XNMDAG                         opzionale
022100891017     C     KTB           KLIST
022200891017     C                   KFLD                    CODUT
022300891017     C                   KFLD                    COD
022400891017     C     KTAB          KLIST
022500891017     C                   KFLD                    CODUT
022600891017     C                   KFLD                    COD               2
022700891017     C                   KFLD                    KEY               8
022800050223      *
022900050223      * carico la tabella ISTAT
023000050223      *
023100101013     c                   exsr      caristat
023200971022     C*---------------------------------------------------------------*
023300971022     C* CARICO TABELLA    FF  - FREQUENZA FATTURA PER METTERE LA DECOD
023400971022     C*                         A VIDEO
023500971022     C***
023600971022     C                   MOVEL     'FF'          COD
023700971022     C                   Z-ADD     0             B                 3 0
023800971022     C     KTB           CHAIN     TABEL                              70
023900971022     C     *IN70         DOWEQ     *OFF
024000971022     C     TBLFLG        IFEQ      ' '
024100971022     C                   ADD       1             B
024200971022     C                   MOVEL     TBLKEY        CFF(B)
024300971022     C                   MOVEL     TBLUNI        DFF(B)
024400971022     C                   MOVEL     TBLUNI        UFF(B)
024500971022     C                   ENDIF
024600971022     C     KTB           READE     TABEL                                  70
024700971022     C                   ENDDO
024800971022     C* CONTROLLO QUANTE DESCRIZIONI HO CARCATO
024900971022     C     55            DIV       B             RESTO             2 0
025000971022     C     RESTO         IFGT      15
025100971022     C                   Z-ADD     15            RESTO
025200971022     C                   ENDIF
025300971022     C**
025400971022     C     RESTO         SUB       3             LUDES             2 0
025500971022     C**
025600971022     C                   Z-ADD     1             B
025700971022     C                   Z-ADD     1             C                 3 0
025800971022     C**
025900971022     C                   CLEAR                   K55
026000971022     C     CFF(B)        DOWNE     ' '
026100971022     C                   MOVEL     CFF(B)        K55(C)
026200971022     C                   ADD       1             C
026300971022     C                   MOVEL     '='           K55(C)
026400971022     C**
026500971022     C                   MOVEA     DFF(B)        K12
026600981026     C                   Z-ADD     1             X                 2 0
026700971022     C     X             DOWLE     LUDES
026800971022     C                   ADD       1             C
026900971022     C                   MOVEL     K12(X)        K55(C)
027000971022     C                   ADD       1             X
027100971022     C                   ENDDO
027200971022     C**
027300971022     C                   ADD       2             C
027400971022     C                   ADD       1             B
027500971022     C                   ENDDO
027600971022     C                   MOVEA     K55           DESFF1
027700971022     C                   MOVEA     K55(29)       DESFF2
027800031009      *
027900000000     C     START         TAG
028000971022     C                   SETOFF                                       01
028100971022     C                   WRITE     SF01Z01
028200971022     C     FOR01         TAG
028300971022     C                   EXFMT     SF01D01
028400960924     C* FINE LAVORO
028500960924     C   KC              GOTO      FINE
028600071203     c* per 55 o 56 o 54 o 57 vado a FINE
028700050223     C                   if        *in55 = *on OR *in56 = *on  or *in54 = *on
028800071203     c                             or *in57
028900040622     C                   GOTO      FINE
029000040622     C                   endif
029100110127     c
029200110127     c* Data   del  giorno
029300110127     c                   movel     datasys       datadmy
029400110127     c     *dmy          movel     datadmy       w0060             6 0
029500110127     c                   movel     w0060         D1DATA
029600111215      * imposto data inizio anno corrente x fattura inizio mese anno corrente
029700111215     c                   time                    oggi             14 0
029800111216     C                   MOVE      OGGI          ANNOCOR           4 0
029900111216     C                   EVAL      PRIMO=(ANNOCOR *10000) + 101
030000071203      *-------------------------------------------------------------------*
030100071203      * verifico se sono un utente autorizzato a lanciare fatturazione    *
030200071203      *-------------------------------------------------------------------*
030300071203
030400071203     c                   clear                   xchkautds
030500071203     c                   eval      xcasoc = xscsoc
030600071203     c                   eval      xcagrp = xscgrp
030700071203     c                   eval      xcafnc = 'CONPNO'
030800071203     c                   eval      xcaprf = knmus
030900071203     c                   eval      xcatck = 'AP'
031000071203     c                   eval      xcacut = xsccut
031100071203     c                   eval      xcauff = xscuff
031200071203     c                   eval      xcalin = xsculm
031300071203     c                   call      'XCHKAUT'
031400071203     c                   parm                    XCHKAUTDS
031500071203      *
031600071203      * verifico il codice di ritorno  se diverso da zero segnalo l'errore
031700071203     c                   eval      *in57 = xcartn <> 0
031800071203     c   57              goto      for01
031900071203      *
032000071203      * se non risultano errori dal richiamo di xchkautds verifico i flag di abilitazione
032100071203      * immissione dati iva,non iva ed effetti
032200071203     c                   movel     xcadds        conpnods
032300071203     c                   eval      *IN57 = (ImmIVA <> '1' or ImmNON <> '1' or
032400071203     c                                      ImmATT <> '1')
032500071203     c   57              goto      for01
032600040622      **
032700040622      * verifico se esiste la libreria LIBASP3, in tal caso errore          postata sul blocco
032800040622     C                   call      'TNSF01C'
032900040622     C                   parm      *blanks       Err_libasp
033000040622     c                   eval      *in56 = (ERR_libasp = 'E')
033100040622     C   56              GOTO      FOR01
033200031013      **
033300031013      * verifico se la tabella FAT (Tabella che pilota la fatturazione) è impostata sul blocco
033400031013      * della fatturazione
033500031013     c                   clear                   tibs02ds
033600031013     C                   movel     'L'           T02tla
033700031013     C                   movel     'C'           T02mod
033800031013     C                   movel     knsif         t02sif
033900031013     C                   movel     'FAT'         t02cod
034000031013     C                   movel     'FAT'         t02ke1
034100031013     C                   call      'TIBS02R'
034200031013     C                   parm                    KPJBA
034300031013     C                   parm                    TIBS02DS
034400031013     C                   movel     t02uni        DFAT
034500031013      *
034600031013     c                   If        §fatbloc  = 'S'
034700031013     c                   seton                                        55
034800031013     C                   goto      for01
034900031013     c                   endif
035000971022     C* TIPO FATTURAZIONE
035100971022     C                   Z-ADD     1             B
035200971022     C     VIDTFT        LOOKUP    CFF(B)                                 30
035300971022     C     *IN30         IFEQ      *OFF
035400971022     C                   SETON                                        40
035500971022     C                   GOTO      FOR01
035600971022     C                   ENDIF
035700971022     C*
035800971022     C                   SETON                                        01
035900971023     C                   SETOFF                                       95
036000971022     C* TIPO FATTURAZIONE MENSILE: VISUALIZZO LE DUE DATE FATTURA
036100040702      * E IMPOSTO TIPO CLIENTE A 2 (CLIENTI CODIFICATI E NON )
036200971022     C     VIDTFT        IFEQ      '4'
036300971022     C                   SETON                                        02
036400131009     c                   exsr      Proposta_date
036500040702     C                   EVAL      VIDTCL = 2
036600971022     C                   ELSE
036700971022     C                   SETOFF                                       02
036800040702      * SE FATTURAZIONE SETTIMANALE IMPOSTO TIPO CLIENTE 0 (SOLO CODIFICATI)
036900040702     C     VIDTFT        IFEQ      '1'
037000040702     C                   EVAL      VIDTCL = 0
037100131008     c                   exsr      Proposta_date
037200040702     C                   ENDIF
037300971022     C                   ENDIF
037400971022     C                   MOVEL     UFF(B)        DSFF
037500971023     C                   MOVEL     §FFDES        VIDDTF
037600000208     c* carico a video le divise che devono essere fatturate
037700000210     c******!!!!!*****   exsr      cardiv
037800000210     C* PER ADESSO CARICO COME COSTANTI
037900020221     C                   MOVE      'EUR'         VIDDV1
038000031009      * giro con una unica moneta
038100031009     C*****              MOVE      'ITL'         VIDDV2
038200971022     C****
038300971022     C* EMETTO SECONDA VIDEATA
038400971022     C     FOR02         TAG
038500971022     C                   WRITE     SF01D01
038600971022     C                   EXFMT     SF01D02
038700971022     C** F3  - FINE LAVORO
038800971022     C   KC              GOTO      FINE
038900971022     C** F12 - RITRONO
039000971022     C   KL              GOTO      START
039100960924     C**
039200960924     C** F10 - GESTIONE STAMPE
039300981026     C   10              MOVEL     '3'           KRITB
039400000000     C   10              CALL      'BCH09'
039500000000     C                   PARM                    KPJBA
039600971022     C   10              GOTO      FOR02
039700891017     C*
039800971022     C* TIPO CLIENTI DA FATTURARE
039900961111     C     VIDTCL        IFGT      2
040000891023     C                   SETON                                        92
040100971022     C   92              GOTO      FOR02
040200891023     C                   END
040300040702     C* SE FATTURAZIONE SETTIMANALE ERRORE SE TIPO CLIENTE DIVERSO DA 0
040400040702     C                   IF        VIDTFT = '1' AND VIDTCL <> 0
040500040702     C                   SETON                                        93
040600040702     C   93              GOTO      FOR02
040700040702     C                   END
040800971022     C**
040900971022     C* FATTURAZIONE MENSILE: DATA FATTURA INIZIO MESE
041000971022    1C     *IN02         IFEQ      *ON
041100971022     C**
041200981026     c     *eur          test(d)                 viddfi                 99
041300981026     c                   if        *in99
041400981026     c     *dmy          test(d)                 viddfi                 99
041500981026     c  n99*dmy          move      viddfi        dataeur
041600981026     c  n99              move      dataeur       viddfi
041700981026     c                   else
041800981026     c                   move      viddfi        dataeur
041900981026     c                   end
042000981026     C   99              GOTO      FOR02
042100981026     c                   move      dataeur       dataiso
042200981026     c                   move      dataiso       amgfat
042300981026     c                   exsr      mvc
042400971022     C**
042500981026     c                   if        esito = *on
042600981026     C                   seton                                        99
042700981026     C                   GOTO      FOR02
042800981026     c                   end
042900971022     C** DATA FATTURA FINE MESE
043000981026     c     *eur          test(d)                 viddff                 98
043100981026     c                   if        *in98
043200981026     c     *dmy          test(d)                 viddff                 98
043300981026     c  n98*dmy          move      viddff        dataeur
043400981026     c  n98              move      dataeur       viddff
043500981026     c                   else
043600981026     c                   move      viddff        dataeur
043700981026     c                   end
043800981026     C   98              GOTO      FOR02
043900981026     c                   move      dataeur       dataiso
044000981026     c                   move      dataiso       amgddc
044100981026     c                   exsr      mvc
044200981026     C**
044300981026     c                   if        esito = *on
044400981026     C                   seton                                        98
044500981026     C                   GOTO      FOR02
044600981026     c                   end
044700971022   X1C                   ELSE
044800971022     C** DATA FATTURA SINGOLA
044900981026     c     *eur          test(d)                 viddft                 41
045000981026     c                   if        *in41
045100981026     c     *dmy          test(d)                 viddft                 41
045200981026     c  n41*dmy          move      viddft        dataeur
045300981026     c  n41              move      dataeur       viddft
045400981026     c                   else
045500981026     c                   move      viddft        dataeur
045600981026     c                   end
045700981026     C   41              GOTO      FOR02
045800981026     c                   move      dataeur       dataiso
045900981026     c                   move      dataiso       amgddt
046000981026     c                   exsr      mvc
046100981026     C**
046200981026     c                   if        esito = *on
046300981026     C                   seton                                        41
046400981026     C                   GOTO      FOR02
046500981026     c                   end
046600971210     C* SOTTRAGGO 1 ALLA DATA
046700971210     C**
046800981026     C                   SUBdur    1:*d          dataiso
046900981026     C                   move      dataiso       DATAM
047000971022    1C                   ENDIF
047100971022     C** DATA SPEDIZIONE
047200981026     c                   if        viddsp = 0
047300981026     c                   seton                                          97
047400981026     c                   else
047500981026     c     *eur          test(d)                 viddsp                 97
047600981026     c                   if        *in97
047700981026     c     *dmy          test(d)                 viddsp                 97
047800981026     c  n97*dmy          move      viddsp        dataeur
047900981026     c  n97              move      dataeur       viddsp
048000981026     c                   else
048100981026     c                   move      viddsp        dataeur
048200981026     c                   end
048300981026     c                   end
048400981026     C   97              GOTO      FOR02
048500981026     c                   move      dataeur       dataiso
048600050223      * verifico se esiste percentuale ISTAT
048700050223     c                   move      dataiso       dataspe
048800080219     c                   move      dataiso       dataspe_iso
048900050301     c                   movel     dataspe       annospe           4 0
049000050223     c                   clear                   perce
049100050223
049200101013     c                   do        999           pi
049300101014      * se data = a zero  vado a fine non dovrebbero esserci dei successivi
049400050223     c                   if        dia(pi) = 0
049500050223     c                   leave
049600050223     c                   endif
049700050301     c                   movel(p)  dia(pi)       annoist           4 0
049800050301      * se anno di spedizione uguale a quello dello scaglione prendo la percentuale
049900050301     c                   if        annospe = annoist
050000150205     c                   z-add     iac(pi)       perce             5 2
050100050301      *
050200050223     c                   else
050300060110     c                   iter
050400050223     c                   endif
050500050223
050600050223     c                   enddo
050700050223
050800050223      * Verifico se ho recuperato una percentuale istat
050801160105      * inserisco blocco per anno 2016 in quanto la % ISTAT è uguale a 0 !!!!! e quindi
050802160105      * non do messaggio bloccante
050900160105     c                   if        perce = *zeros and annospe <> 2016
051000050223     c                   seton                                        54
051100050223     c                   goto      for02
051200050223     c                   endif
051300070202
051400070201      * verifico se esiste ultimo prezzo medio del gasolio
051500131009     c                   clear                   wgioset
051600070202     c                   movel     viddsp        wggmm             4
051700070202     c                   move      viddsp        waa               2
051800131009     c                   movel     wggmm         wggmmaa
051900070202     c                   move      waa           wggmmaa
052000070202     c                   call      'XGIOSE1'
052100070202     c                   parm                    wggmmaa
052200131009     c                   parm                    wgioset
052300131009     c                   move      wgioset       giorno            1 0
052400070202      * cerco il primo lunedì antecedente la data spedizione per cercare il prezzo medio gasolio
052500070202      * calcolo il numero di giorni che devo sottrarre dalla data ultima spedizione
052600070202
052700070202     c                   eval      giorno = giorno - 1
052800070202     c     *eur          movel     viddsp        dataiso
052900070202     c                   subdur    giorno:*d     dataiso
053000070202     c                   move      dataiso       wdata
053100070202     c     wdata         chain     tipmg01l
053200070202     c                   If        not %found(tipmg01l)
053300070202     c                   seton                                        53
053400070202     c                   goto      for02
053500070202     c                   endif
053600070201     C**
053700151015      /free
053800151015       //?Se fatturazione mensile
053900151015       //?controllo se è già stata elaborata la statistica pesi
054000151015       //?per il mese di spedizioni che devo fatturare
054100151015         *in58 = *off;
054200151015         IF  *in02;
054300151015           mesespe = %dec(%subst(%editc(dataspe:'X'):5:2):2:0);
054400151015           setll (annospe:mesespe) SIPES02L;
054500151015           IF  not %equal(SIPES02L);
054600151015             *in58 = *on;
054700151027      /end-free
054800151027     c                   GOTO      FOR02
054900151027     c                   ENDIF
055000151027     c                   ENDIF
055100070201      *
055200971210     C**
055300971210     C** PER FATTURAZIONE NON MENSILE LA DATA SPEDIZIONE DEVE ESSERE
055400971210     C**  - 1 RISPETTO ALLA DATA FATTURA
055500080219     C  N02SAVAMG        IFNE      dataspe_iso
055600971210     C                   CLEAR                   DUE
055700080219     C                   MOVEL     dataspe_iso   SAVAMG
055800971210     C                   ENDIF
055900971210     C**
056000080219     C  N02dataspe_iso   IFNE      DATAM
056100971210     C     DUE           ANDEQ     ' '
056200971210     C                   SETON                                        46
056300971210     C                   MOVEL     '1'           DUE               1
056400971210     C                   GOTO      FOR02
056500971210     C                   ENDIF
056600111028     C**
056700111028     C** PER FATTURAZIONE MENSILE LA DATA SPEDIZIONE NON DEVE ESSERE
056800111028     C** MAGGIORE DELLE DATE FATTURAZIONE
056900111028     C**
057000111028     C                   IF        (*IN02 AND (dataspe_iso > amgfat or
057100111028     c                             dataspe_iso > amgddc)) or
057200111028     C                             (not *IN02 AND dataspe_iso > amgddt)
057300111028     C                   SETON                                        47
057400111028     C                   GOTO      FOR02
057500111028     C                   ENDIF
057600971022     C**
057700971022     C* PER FATTURAZIONE NON MESILE DEVE ESSERE UGUALE ALLA DATA FATT
057800971022     C* CONTROLLO SE LA DATA SPEDIZIONE IMMESSA E' TRA QUELLE
057900971022     C*  PREVISTE NEL TIPO FATTURAZIONE
058000971210     C   02              MOVEL     VIDDFF        W0020             2 0
058100971210     C  N02              MOVEL     VIDDFT        W0020             2 0
058200971022    1C     W0020         IFNE      SAVGGG
058300971022     C                   Z-ADD     W0020         SAVGGG            2 0
058400971022     C                   CLEAR                   UNO
058500971022    1C                   ENDIF
058600971022     C*
058700040702      * controllo solo in caso di fatturazione mensile
058800040702     c                   if        vidtft = '4'
058900971022     C     W0020         LOOKUP    DGG                                    30
059000971022    1C     *IN30         IFEQ      *OFF
059100971022     C     UNO           ANDEQ     ' '
059200971022     C                   SETON                                        44
059300971022     C                   MOVEL     '1'           UNO               1
059400971022     C                   GOTO      FOR02
059500971022    1C                   ENDIF
059600040702     c                   endif
059700971210     C* LA DATA INIZIO MESE NON DEVE INVECE ESSERE NELLA TABELLA DEI GG
059800971210     C*  FATTURABILI
059900971210     C   02              MOVEL     VIDDFI        W0020             2 0
060000971210     C   02W0020         LOOKUP    DGG                                    30
060100001115    1C   02*IN30         IFEQ      *ON
060200001115     C*!""!*             SETON                                        45
060300001115     C*!""!*             GOTO      FOR02
060400971210    1C                   ENDIF
060500991230     c** contolllo le divise inserite
060600991230     c                   clear                   viddd1
060700991230     c                   clear                   viddd2
060800991230      * se entrambe a blank errore
060900991230     c                   if        viddv1 = *blanks and  viddv2 = *blanks
061000991230     c                   seton                                        89
061100991230     c                   goto      for02
061200991230     c                   endif
061300991230      * se la prima = a blank e la seconda no errore
061400991230     c                   if        viddv1 = *blanks and  viddv2 <> *blanks
061500991230     c                   seton                                        89
061600991230     c                   goto      for02
061700991230     c                   endif
061800991230      * se entambi uguali errore
061900991230     c                   if        viddv1 = viddv2
062000991230     c                   seton                                        91
062100991230     c                   goto      for02
062200991230     c                   endif
062300991230      * controllo la prima divisa            1°
062400991230     c                   movel     'CV'          cod
062500991230     c                   movel(p)  viddv1        key
062600131009     c     ktab          chain     tabel                              49
062700991230      * divisa inesistente
062800991230     c                   if        *in49 or tblflg <> ' '
062900991230     c                   seton                                        89
063000991230     c                   goto      for02
063100991230     c                   endif
063200991230      * divisa non abilitata alla fatturazione
063300991230     c                   movel     tbluni        dscv
063400991230     c                   if        §cvabi <> 'S'
063500991230     c                   seton                                        88
063600991230     c                   goto      for02
063700991230     c                   endif
063800991230      * decodifico la divisa
063900991230     c                   movel     §cvdes        viddd1
064000971022     C**
064100991230      * controllo la seconda divisa se diversa da blanks
064200991230     c                   if        viddv2 <> *blanks
064300991230     c                   movel     'CV'          cod
064400991230     c                   movel(p)  viddv2        key
064500131009     c     ktab          chain     tabel                              50
064600991230      * divisa inesistente
064700991230     c                   if        *in50 or tblflg <> ' '
064800991230     c                   seton                                        91
064900991230     c                   goto      for02
065000991230     c                   endif
065100991230      * divisa non abilitata alla fatturazione
065200991230     c                   movel     tbluni        dscv
065300991230     c                   if        §cvabi <> 'S'
065400991230     c                   seton                                        90
065500991230     c                   goto      for02
065600991230     c                   endif
065700991230      * decodifico la divisa
065800991230     c                   movel     §cvdes        viddd2
065900991230      *
066000991230     c                   endif
066100991230     C**
066200000000     C                   SETOFF                                       9411
066300000000     C     VIDTSC        IFNE      *BLANKS
066400000000     C                   DO        40            A
066500000000     C     FIL(A)        IFNE      0
066600000000     C                   SETON                                        11
066700000000     C                   END
066800000000     C                   END
066900000000     C  N11              SETON                                        94
067000971022     C   94              GOTO      FOR02
067100000000     C** 94 ON INDICATA OPZIONE E NON FILIALI
067200000000     C                   ELSE
067300000000     C                   DO        40            A
067400000000     C     FIL(A)        IFGT      0
067500000000     C                   SETON                                        94
067600000000     C                   END
067700000000     C                   END
067800971022     C   94              GOTO      FOR02
067900000000     C                   END
068000000000     C                   DO        40            A                 2 0
068100000000     C     FIL(A)        IFGT      0
068200910111     C     FIL(A)        CHAIN     AZORG                              96
068300971022     C   96              GOTO      FOR02
068400960925     C** P.O. INESISTENTE
068500000000     C                   END
068600000000     C                   END
068700981026     c*controllo cliente se immesso
068800981026    1C     VIDKSC        IFGT      0
068900981026     C     Keycli        klist
069000981026     C                   kfld                    wsocieta
069100981026     C                   kfld                    wksc              8
069200981026     C                   kfld                    k£fil
069300981026     C                   kfld                    k£lin
069400981026     c                   move      *zeros        wksc
069500981026     c                   move      vidksc        wksc
069600981026     C     *LIKE         DEFINE    clnlineav     k£lin
069700981026     C     *LIKE         DEFINE    clnfiliale    k£fil
069800981026     C     Keycli        CHAIN     ancln01l                           30
069900981026     c                   if        clndtfirpc > *loval
070000981026     c                   seton                                        30
070100981026     c                   endif
070200981026     C  N30clnsogg       CHAIN     ansog01l                           30
070300981026    2C     *IN30         IFEQ      *ON
070400981026     C                   SETON                                        43
070500981026     C                   GOTO      FOR02
070600981026    2C                   ENDIF
070700981026     C                   MOVEL     sogdes        VIDDES
070800981026    2C                   ENDIF
070900971022     C**
071000891016     C* REPERISCE  NUMERAZIONE
071100941104     C                   MOVEL     'QC'          COD
071200941104     C                   MOVEL     *BLANKS       KEY
071300941104     C                   MOVEL     '2'           KEY
071400131009     C     KTAB          CHAIN     TABEL                              50
071500971022     C   50              GOTO      FOR02
071600941104     C                   MOVEL     TBLUNI        DSQC2
071700971023     C*
071800981027     C                   MOVE      '1'           XNMRIC            1
071900981026     C                   MOVEl     §QCRIV        wserie            4            REGISTRO VEND.
072000981026     C** PER FATTURE INIZIO MESE
072100981026     C**  E FATTURAZIONE NON MENSILE
072200981027     C   02              MOVEL     amgfat        xnmdat                         ANNO FATTURA
072300981027     C  N02              MOVEL     amgddt        xnmdat                         ANNO FATTURA
072400981027     C   02              MOVEL     amgfat        xnmdag                         ANNO FATTURA
072500981027     C  N02              MOVEL     amgddt        xnmdag                         ANNO FATTURA
072600981026     C                   MOVE      §QCFIV        wserie                         LIBRO IVA VEND.
072700981026     c                   exsr      repnum
072800981026     C     xnmerr        ifgt      '0'
072900981026     C     xnmerr        andlt     '7'
073000981026     C                   seton                                        51
073100981026     C                   GOTO      FOR02
073200981026     C                   end
073300981026     C     xnmerr        IFeq      '7'
073400891017     C                   SETON                                        99
073500971023     C                   GOTO      FOR02
073600971023     C                   END
073700971022     C**
073800981026     C   02              Z-ADD     xnmnum        VIDNFI
073900971022     C**
074000981026     C  N02              Z-ADD     xnmnum        VIDNFT
074100971022     C** PRIMO NUMERO DI FATTURA INIZIO MESE
074200971022     C     *IN02         IFEQ      *ON
074300981026     C                   MOVE      amgddc        xnmdat                         ANNO FATTURAZ.
074400981027     C                   MOVE      amgddc        xnmdag                         ANNO FATTURAZ.
074500981026     C                   MOVE      §QCFI2        wserie                         LIBRO IVA FT.A.
074600981026     c                   exsr      repnum
074700981026     C     xnmerr        ifgt      '0'
074800981026     C     xnmerr        andlt     '7'
074900981026     C                   seton                                        52
075000981026     C                   GOTO      FOR02
075100981026     C                   end
075200981026     C     xnmerr        IFeq      '7'
075300940927     C                   SETON                                        98
075400971023     C                   GOTO      FOR02
075500971023     C                   END
075600971022     C**
075700981026     C                   Z-ADD     xnmnum        VIDNFF
075800971022     C**
075900940927     C** PRIMO NUMERO DI FATTURA FINE MESE
076000971022     C                   ENDIF
076100971022     C**
076200960924     C  NKF              SETON                                        95
076300971022     C   95
076400971022     CANNKF              GOTO      FOR02
076500960924     C* F6 - ELABORAZIONE
076600960924     C     *INKF         IFEQ      *ON
076700960924     C                   MOVEL     FIL           FILX
076800981026     c                   clear                   dataeur
076900000216     c* pulizia del file etichette
077000000217     C                   MOVEL(P)  cm1(1)        comman
077100000217     C                   CALL      'QCMDEXC'
077200000217     C                   PARM                    comman
077300000217     C                   PARM                    lenght
077400000217     c*
077500981026     c                   if        viddfi = 0
077600981026     c                   move      dataeur       viddfi
077700981026     c                   end
077800981026     c                   if        viddff = 0
077900981026     c                   move      dataeur       viddff
078000981026     c                   end
078100981026     c                   if        viddft = 0
078200981026     c                   move      dataeur       viddft
078300981026     c                   end
078400130325     c*
078500130325     c* se fatturazione mensile emissione window  per il lancio del recupero variazioni
078600130325     c* post-fatturazione
078700130325      * se lancio fatturazione mensile / nessuna scelta parzializzante codice cliente e filiale
078800130325     c                   if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
078900130325     c                   exsr      sr_rpf
079000130325     c                   endif
079100040622      ***
079200040622      *  Creo libreria LIBASP3
079300040622     C                   IF        VIDTFT = '4'
079400040622     c                   eval      Wtft = 'MENSILE    '
079500040622     c                   ELSE
079600040622     c                   eval      Wtft = 'SETTIMANALE'
079700040622     c                   ENDIF
079800040622     c                   movel     VIDDSP        W008A
079900040622     c                   eval      Wtext = 'Fattura ' + wtft +
080000040713     c                             ' data sped. al '   +
080100040622     c                             %subst(W008A:1:2) +  '/' +
080200040622     c                             %subst(W008A:3:2) +  '/' +
080300040713     c                             %subst(W008A:7:2)
080400040622     c                   CALL      'TNSF01C1'
080500040622     c                   parm                    WText
080600111215      ***
080700111215      * se data fattura inizio mese è uguale al 1/1/ anno in corso e non ci sono
080800111215      * parzializzazioni cliente / filiale piloto la copia delle tariffe annuale in EDPCONF_TA
080900111215      ***
081000140528      * 28/05/14 AG non copiamo più le traiffe nell EDPCONF_TA in quanto ora teniamo le tariffa
081100140528      * oltre 365 giorni. manteniamo quella del mese precedente
081200111215     c                   if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
081300111216     c                   move      amgfat        wdata
081400140528     c****               If        wdata = primo
081500140528     C****               MOVEL     PARAM         KPJBU
081600140528     C****               MOVEL     'SF%1'        KCOAZ
081700140528     C***                CALL      'BCH10'
081800111216     C****               CALL      'TNSF01C2'
081900140528     C****               PARM                    KPJBA
082000140528     c****               endif
082100121211      * se fatturazione senza parzializzazioni, fatturazione mensile
082200121211      * devo copiare le tariffe per confronto fatturazione al mese precedente
082300121212     c                   eval      dataiso = amgfat - %Months(1)
082400121212     c                   eval      wanno = %subdt(dataiso:*Years)
082500121212     c                   eval      sf3AA =
082600121212     c                             %dec(%subst(%editc(wanno:'X'):3:2):2:0)
082700121212     c                   eval      sf3MM = %subdt(dataiso:*Months)
082800121211     c                   eval      kpjbu = ParmSF3
082900121211     c                   eval      kcoaz = 'SF%2'
083000121211     c                   call      'BCH10'
083100121211     c****               call      'TNSF01C3'
083200121211     c                   parm                    kpjba
083300121211
083400111216     c                   endif
083500130325      ***
083600130325      * creazione bolle di recupero variazioni post-fatturazione
083700130325      ***
083800130325      * se lancio fatturazione mensile / nessuna scelta parzializzante codice cliente e filiale
083900130325     c                   if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
084000130325     c* simulazione
084100130325     c                   if        w01sml='SI'
084200130325     C                   clear                   KPJBU
084300130326     C                   MOVEL     'SF55'        KCOAZ
084400130325     C                   CALL      'BCH10'
084500130325     C****               CALL      'TNSF55C'
084600130325     C                   PARM                    KPJBA
084700130325     c                   CLEAR                   KPJBU
084800130325     c                   endif
084900130325     c* creazione bolle
085000130325     c                   if        w01cre='SI'
085100130325     C                   clear                   KPJBU
085200130326     C                   MOVEL     'SF5B'        KCOAZ
085300130325     C                   CALL      'BCH10'
085400130325     C****               CALL      'TNSF55R'
085500130325     C                   PARM                    KPJBA
085600130325     c                   CLEAR                   KPJBU
085700130325     c                   endif
085800040622      ***
085900061009      * preparazione bole fuori misura DPD
086000150122      *** dalla fatturazione di Febbraio 2015 non si creano più le bolle Fuori misura DPD
086100150122      *** asterisco le istruzioni ... in seguito tolgo il programma
086200061009      * se lancio fatturazione mensile / nessuna scelta parzializzante codice cliente e filiale
086300130325     c*****              if        vidtft = '4' and vidksc = 0 and vidtsc = ' '
086400150122     C** 22/01           MOVEL     PARAM         KPJBU
086500150122     C** 22/01           MOVEL     'SF50'        KCOAZ
086600150122     C** 22/01           CALL      'BCH10'
086700061009     C****               CALL      'TNSF50C'
086800150122     C** 22/01           PARM                    KPJBA
086900150122     c** 22/01           CLEAR                   KPJBU
087000061009     c                   endif
087100991230     c* gestione delle divise
087200991230     c* un giro per ogni divisa
087300991230     c* se esistono due divise lancio il primo giro
087400991230     c                   if        viddv1 <> *blanks and viddv2 <> *blanks
087500991230     c* 1° giro
087600991230     c* aggiornamento  tassazione  per tutte le bolle
087700991230     c* non si stampa il manca tariffa
087800991230     c                   eval      viddiv = viddv1
087900991230     c                   eval      vidagg = 'S'
088000991230     c                   eval      vidsta = 'N'
088100991230     c*
088200050426     C                   MOVEL     'SF02'        KCOAZ
088300960924     C                   MOVEL     PARAM         KPJBU
088400050426     C                   CALL      'BCH10'
088500050426     C**                 CALL      'TNSF02C'
088600941124     C                   PARM                    KPJBA
088700991230     c* 2° giro
088800991230     c* nesun aggiornamento  tassazione  per le bolle con divisa diversa
088900991230     c* si stampa il manca tariffa
089000991230     c                   eval      viddiv = viddv2
089100991230     c                   eval      vidagg = 'N'
089200991230     c                   eval      vidsta = 'S'
089300991230     c*
089400050426     C                   MOVEL     'SF02'        KCOAZ
089500991230     C                   MOVEL     PARAM         KPJBU
089600050426     C                   CALL      'BCH10'
089700050426     C***                CALL      'TNSF02C'
089800991230     C                   PARM                    KPJBA
089900991230     c*
090000991230     c                   else
090100991230     c* esiste solo una divisa mi comporto come se fossi un secondo giro
090200991230     c*
090300991230     c* nesun aggiornamento  tassazione  per le bolle con divisa diversa
090400991230     c* si stampa il manca tariffa
090500991230     c                   eval      viddiv = viddv1
090600991230     c                   eval      vidagg = 'N'
090700991230     c                   eval      vidsta = 'S'
090800991230     c*
090900050427     C                   MOVEL     'SF02'        KCOAZ
091000991230     C                   MOVEL     PARAM         KPJBU
091100050427     C                   CALL      'BCH10'
091200050427     C***                CALL      'TNSF02C'
091300991230     C                   PARM                    KPJBA
091400991230     c**
091500991230     c                   endif
091600991230     c*
091700000209     c*lancio la stampa delle etichette
091800000209     c*
091900000209     C                   MOVEL     'SF15'        KCOAZ
092000000209     C                   MOVEL     PARAM         KPJBU
092100001221     C                   CALL      'BCH10'
092200001221     C**                 CALL      'TNSF15R'
092300000209     C                   PARM                    KPJBA
092400110127     c
092500110127     c** Sottometto azione schedulata dati statistiche clienti Roma
092600110127     C                   IF        VIDTFT = '4'
092700110127     c                   movel     amgfat        stf1DFI
092800110127     c                   movel     amgddc        stf1DFF
092900110127     C                   MOVEL     'STF1'        KCOAZ
093000110127     c                   eval      d1oras='1900'
093100110127     c                   movel     scheds        kbuff
093200110127     C                   MOVEL     PARAMSTF1     KPJBU
093300110127     c
093400110127     C                   CALL      'BCH10'
093500110127     C                   PARM                    KPJBA
093600110127     c                   endif
093700110127     c
093800960924     C                   ENDIF
093900130108
094000130108      /free
094100130108       //?Se fatturazione senza parzializzazioni e fatturazione mensile
094200130108       //?devo memorizzare la data ultima spedizione fatturata in tabella RPF
094300130108        IF  VIDtft = '4' and VIDksc = 0 and VIDtsc = ' ';
094400130108          clear dRPFfatgio;
094500130108          TBEcod = 'RPF';
094600130108          TBEke1 = 'FATTGIO';
094700130108          chain (TBEcod:TBEke1) TNTBE01L;
094800130108          IF  %found(TNTBE01L);
094900130108            §RPFfatdsp = dataSPE;
095000130108            TBEuni = dRPFfatgio;
095100130108            UPDATE  TNTBE000;
095200130108          ENDIF;
095300130108        ENDIF;
095400130108      /end-free
095500130108
095600960924     C**
095700000000     C     FINE          TAG
095800000000     C                   SETON                                        LR
095900981026     C*----------------------------------------------------*
096000981026     C* Reperisco numeratore
096100981026     C*----------------------------------------------------*
096200981026     c     repnum        begsr
096300981026     C                   MOVE      wsocieta      XNMSOC            3
096400981026     C                   MOVE      *BLANKS       XNMUNI            8
096500981026     C                   MOVE      'CG'          XNMCTB            2
096600981026     C                   MOVE      *BLANKS       XNMKEY            8
096700981026     C                   MOVE      WSERIE        XNMSER            4
096800981026     C                   MOVE      *OFF          XNMCMT            1
096900981026     C                   MOVE      *On           XNMIVA            1
097000981026     C                   MOVE      *OFF          XNMREG            1
097100981027     C                   MOVE      '0'           XNMerr            1
097200981026     C*
097300981026     C                   CALLB     'XNMR'        PNMR
097400981026     C                   endsr
097500981026     C*----------------------------------------------------*
097600981026     C* controllo libro giornale
097700981026     C*----------------------------------------------------*
097800981026     c     mvc           begsr
097900981026     c                   move      dataiso       dtreg
098000981026     c                   move      dataiso       dtlgio
098100981026     C                   CALLB     'NDMVC106'
098200981026     C                   PARM      xscsoc        Societa
098300981026     C                   PARM      'CG'          CTB
098400981026    >C                   PARM      'D'           TpReg
098500981026    >C                   PARM                    DtLGio
098600981026    >C                   PARM                    DtReg
098700981026     C                   PARM      *on           GesMsg
098800981026     C                   PARM                    Esito
098900981026     c                   endsr
099000981026     C*----------------------------------------------------*
099100981026     C* Reperimento dati società
099200981026     C*----------------------------------------------------*
099300981026     C     REPSOC        BEGSR
099400981026     C                   CALL      'XSOC'
099500981026     C                   PARM                    TIPXSC            6
099600981026     C                   PARM                    SOCXSC            3
099700981026     C                   PARM                    CDSXSC            9 0
099800981026     C                   PARM                    MODXSC            3
099900981026     C                   PARM      *blanks       RTNXSC            1
100000981026     C                   PARM                    XSOCDS
100100981026     C                   PARM                    KPJBA
100200981026     C                   ENDSR
100300000208     C*******************************************************************
100400000208     C** CARICO DIVISE A VIDEO
100500000208     C*******************************************************************
100600000208     C     CARDIV        BEGSR
100700000208     C**
100800000208     C                   MOVEL     'CV'          COD
100900000208     c                   clear                   key
101000000208     C     KTab          SETLL     TABEL
101100000208     C                   DO        *HIVAL
101200000208     C     KTb           READE     TABEL                                  07
101300000208     c   07              leave
101400000208     c*
101500000208     c                   if        tblflg <> ' '
101600000208     c                   iter
101700000208     c                   endif
101800000208     c*
101900000208     C     TBLKEY        IFNE      *BLANKS
102000000208     C                   MOVEL     TBLUNI        DSCV
102100000208     c*
102200000208     c                   if        §cvabi = 'S'
102300000208     c*
102400000208     c                   if         viddv1 = *blank
102500000208     c                   movel     tblkey        viddv1
102600000208     c                   iter
102700000208     c                   endif
102800000208     c*
102900000208     c                   if         viddv2 = *blank
103000000208     c                   movel     tblkey        viddv2
103100000208     c                   leave
103200000208     c                   endif
103300000208     c*
103400000208     c                   endif                                                  cvabi
103500000208     c*
103600000208     c                   endif                                                  key > '    '
103700000208     c* torno a leggere
103800000208     C                   ENDDO
103900000208     C*
104000000208     C                   ENDSR
104100050223      ****************************************************
104200050223      ** CARICAMENTO IAC
104300050223      ****************************************************
104400101013     c     CARISTAT      BEGSR
104500101013
104600101013      /free
104700101013       //?Carico scatti ISTAT
104800101013         SISsca = 0;
104900101013         clear DIA;
105000101013         setll (SISsca) TISIS01L;
105100101013
105200101013         DOW  not $EoF;
105300101013
105400101013           read TISIS01L;
105500101013
105600101013           IF  %Eof(TISIS01L);
105700101013             $EoF = *on;
105800101013             leave;
105900101013           ENDIF;
106000101013
106100101013           DIA(SISsca) = SISdata;
106200101013           IAC(SISsca) = SISpunti;
106300101013
106400101013         ENDDO;
106500101013
106600101013      /end-free
106700050223     c                   ENDSR
106800130325      ****************************************************
106900130325      ** gestione window per recupero variazioni post-fatt
107000130325      ****************************************************
107100130325     c     sr_rpf        BEGSR
107200130325     c                   seton                                        04
107300130325     c                   eval      w01sml='NO'
107400130325     c                   eval      w01cre='SI'
107500130325     c* su as888 imposto default al contrario
107600130325     c                   if        %subst(knsif:7:3)<>'201'
107700130326     c                   eval      w01sml='NO'
107800130325     c                   eval      w01cre='NO'
107900130325     c                   endif
108000130325     c                   eval      $fine='0'
108100130325     c*
108200130325     c                   dow       $fine='0'
108300130325     c                   exfmt     sf01w01
108400130325     c                   select
108500130325     c* f15=Modifica richieste
108600130325     c                   when      *inkp
108700130325     c                   setoff                                       04
108800130325     c                   iter
108900130325     c                   other
109000130325     c* f6=conferma
109100130325     c                   if        *inkf
109200130325     c                   eval      $fine='1'
109300130325     c                   endif
109400130325     c                   endsl
109500130325     c                   enddo
109600130325     c
109700130325     c                   endsr
109800131008     C*----------------------------------------------------*
109900131008     C* Proposta date di fatturazione
110000131008     C*----------------------------------------------------*
110100131008     c     Proposta_date begsr
110200131008
110300131009     c                   time                    w0140
110400131008      *
110500131008      * Se fatturazione mensile propongo le date fattura di inizio mese (primo giorno del mese
110600131008      * in cui sto fatturando) e data fattura fine mese (giorno precedente fattura inizio mese)
110700131008      * e data spedizione (uguale alla data fattura fine mese)
110800131008      *
110900131009     c                   If        VIDtft = '4'
111000131009     c                   move      w0140         wData1
111100131009     c                   z-add     01            wgiorno1
111200131008     c                   movel     wdata1        viddfi
111300131009     c                   move      viddfi        dataeur
111400131008     c                   move      dataeur       dataiso
111500131008     c                   subdur    1:*d          dataiso
111600131009     c                   move      dataiso       dataeur
111700131009     c                   move      dataeur       viddff
111800131009     c                   move      viddff        viddsp
111900131008     c                   Endif
112000131008     c
112100131008      *
112200131008      * Se fatturazione settimanale propongo come data fattura il lunedì della settimana in
112300131008      * corso e come data spedizione il giorno precedente la fattura
112400131008      *
112500131009     c                   If        VIDtft = '1'
112600131008
112700131008     c                   Clear                   wgioset
112800131009     c                   movel     datasys       datadmy
112900131009     c     *dmy          movel     datadmy       w0060
113000131009     c                   move      w0060         wGGMMAA
113100131008     c                   Call      'XGIOSE1'
113200131008     c                   Parm                    wggmmaa
113300131008     c                   Parm                    wgioset
113400131009     c                   move      wgioset       giorno
113500131008     c
113600131009     c                   movel     datadmy       dataiso
113700131009      * se oggi che lancio non è Lunedì devo trovare che giorno è Lunedì
113800131009     c                   If        giorno  > 1
113900131008      * calcolo il numero dei giorni da diminuire
114000131009     c                   eval      gg = giorno -1
114100131008     c                   subdur    gg:*d         dataiso
114200131008     c                   endif
114300131009     c                   move      dataiso       dataeur
114400131009     c                   move      dataeur       viddft
114500131008      * data spedizione
114600131008     c                   subdur    1:*d          dataiso
114700131009     c                   move      dataiso       dataeur
114800131009     c                   move      dataeur       viddsp
114900131008     c                   Endif
115000131008     c
115100131008     c                   endsr
115200000217**   cm1 - comandi
115300000217CLRPFM FILE(ETLAV00F)
