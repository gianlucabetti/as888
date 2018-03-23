000100000211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200000218     H*PARMS ACTGRP(*CALLER)
000300951128     H*------------------------------------------------------------------------*
000400960212     H*      CONFERMA FOGLI VIAGGIO NON ABBINATI                       NO      *
000500960212     H*  OPPURE:
000600960212     H*      ELENCO   FOGLI VIAGGIO DA ABBINARE MANUALMENTE
000700960311     H*  OPPURE:
000800960311     H*      CONTROLLO ABBINABILITA' MANUALE DI UN FV A TRAINO
000900010827
001000010827      * ATTENZIONE: questo programma era il vecchio TNTL32R
001100010827
001200951128     H*------------------------------------------------------------------------*
001300951128     H DECEDIT('0,') DATEDIT(*DMY.)
001400951128     F*------------------------------------------------------------------------*
001500010827     FTNTL42D   CF   E             WORKSTN USROPN
001600010827     F                                     SFILE(TL42S01:NRR1)
001700960219     FTNTSR01L  UF   E           K DISK
001800951128     FTNFVS02L  IF   E           K DISK
001900960214     F                                     RENAME(TNFVS000:TNFVS2)
002000960214     FTNFVS01L  UF   E           K DISK
002100010827     Ftnfws01l  uF   E           K DISK
002200951128     FAZORG01L  IF   E           K DISK
002300960215     FTABEL00F  IF   E           K DISK
002400000218     FANATB01L  IF   E           K DISK
002500960215     FTNTLZ01L  IF   E           K DISK
002600960216     FFNFV201L  IF   E           K DISK    USROPN
002700960219     FTNTLM01L  UF A E           K DISK    USROPN
002800960219     FTNTLR01L  UF A E           K DISK    USROPN
002900960219     FTNTLO01L  UF A E           K DISK    USROPN
003000960219     FTNTLT01L  UF A E           K DISK    USROPN
003100010828     FTITLL01L  UF A E           K DISK    USROPN
003200960219     FTNTST01L  UF   E           K DISK    USROPN
003300960319     FTNTSR03L  IF   E           K DISK
003400960219     F                                     RENAME(TNTSR000:TNTSR3)
003500960319     FTNTSR04L  IF   E           K DISK    USROPN
003600960319     F                                     RENAME(TNTSR000:TNTSR4)
003700960219     FAZCLN01L  IF   E           K DISK    USROPN
003800960322     FFNFVD01L  IF   E           K DISK    USROPN
003900960312     D PFLP            S              3    DIM(149)                             FIL.SCARIC.TNFVS
004000010828     D MSG             S             78    DIM(25) CTDATA PERRCD(1)             MSG VIDEO
004100960215     D W78             S              1    DIM(78)                              DI COMODO
004200951222     D FFA             S              3  0 DIM(50)                              FIL.FERM.ARRIVO
004300960220     D NUM             S              2  0 DIM(20) CTDATA PERRCD(20)            X GIORNO(A,B,C..)
004400960219     D ALF             S              1    DIM(20) CTDATA PERRCD(20)            *ALFABETO (1÷20)
004500960219     D S1Y             S              1    DIM(10)                              SIMBOLI CALEND
004600960219     D FET             S              1    DIM(10)                              FLAG.FESTIVO
004700960216     D* SCHIERE RELATIVE AI FV SCELTI
004800960216     D LNP             S              3    DIM(100)                             LNP FV
004900960215     D SCE             S              2    DIM(100)                             NUMERI SCELTE
005000960215     D SSC             S              2    DIM(100)                             SORT NR.SCELTE
005100960216     D DFV             S              8  0 DIM(100)                             DATE FV
005200960216     D NRR             S              4  0 DIM(100)                             NRR FV NEL SFL
005300960216     D* SCHIERE PER CREAZIONE ANAGRAFICA TRAINI
005400960216     D CAR             S              3    DIM(100)                             FIL.CARICO
005500960219     D SNF             S              5  0 DIM(100)                             N.FV. ORDINATI
005600960216     D PAR             S             12  0 DIM(100)                             DATA/ORA PART.
005700960216     D SCA             S              3    DIM(150)                             FIL.SCARICO
005800960216     D ARR             S             12  0 DIM(150)                             DATA/ORA ARRIVO
005900960219     D SAR             S             12  0 DIM(150)                             SORT DATA/ORA ARR.
006000010827
006100010827     d wdiv            s                   like(fwsvip)
006200010827     d wdiv1           s                   like(fwsvip)
006300010828     d w_importo       s                   like(fwsipt)
006400010828     d w_divisa        s                   like(fwsvip)
006500010828     d w_delta         s                   like(fwsipt)
006600010828     d sav_tstili      s                   like(tstili)
006700011001     d sav_v2cdiv      s                   like(v2cdiv)
006800011001     d w_decimali      s              2  0
006900011012     d flgged          s              1    inz('0')
007000010827
007100960220     D                 DS
007200960220     D  TLTGSE                 1      7
007300960220     D  GSE                    1      7
007400960220     D                                     DIM(7)                               X TLTGSE
007500960219     D                 DS
007600960219     D  CLNPOM                 1     31
007700960219     D  POM                    1     31
007800960219     D                                     DIM(31)                              FESTIV.POMER.
007900960325     D DSFIL           DS           920    OCCURS(100)
008000960216     D  NFV                    1      5  0
008100960325     D  FVG                    6    455
008200960325     D                                     DIM(150)                             FIL.FV
008300960325     D  FSC                  456    905
008400960325     D                                     DIM(150)                             FIL.SCAR.FV
008500960325     D  DDRP                 906    913  0
008600960325     D  DHRP                 914    917  0
008700960325     D  DLNA                 918    920
008800951128     D WLBDA8          DS
008900951128     D  G08DAT                 1      8  0
009000951128     D  G08INV                 9     16  0
009100951128     D  G08ERR                17     17
009200951128     D  G08TGI                18     22  0
009300960219     D WGIDAT          DS
009400960219     D  GI8DAT                 1      8  0
009500960219     D  GI8INV                 9     16  0
009600960219     D  GI8TGI                17     21  0
009700960216     D DSFVS         E DS                  EXTNAME(TNFVS00F)
009800960301     D  FFV                   97    543
009900960219     D                                     DIM(149)                             FIL.TNFVS
010000960301     D  FLP                  544    990
010100960216     D                                     DIM(149)                             FIL.SCARIC.TNFVS
010200960301     D WDSFVS          DS          1129
010300960216     D                 DS
010400960216     D  DATORA                 1     12  0
010500960216     D  FV2DPA                 1      8  0
010600960216     D  FV2HPA                 9     12  0
010700951128     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010800960229     D TNTL30        E DS                  EXTNAME(TNTL30DS)
010900960219     D DS00          E DS                  EXTNAME(TNTL00DS)
011000960320     D DSTL37        E DS                  EXTNAME(TNTL37DS)
011100960322     D DSTL33        E DS                  EXTNAME(TNTL33DS)
011200960219     D DS1Y          E DS
011300951128     D KPJBA         E DS
011400960216     D*OG148     E DS
011500010827     D DSTL42        E DS                  EXTNAME(TNTL42DS)
011600971118     D DSLV55        E DS                  EXTNAME(FNLV55DS)
011700010828
011800010828     D yeurcods      E DS
011900010828     D tibs02ds      E DS
012000010828     D dscv          E DS
012100010907     D dged          E DS
012200010828     D dgei          E DS
012300010828
012400960215     D* PASSAGGIO DATI ALLA RICERCA TRAZIONISTI  - TRUL16R -
012500960215     D PARAM3          DS
012600000807     D* TIPOLOGIA T_=TRAZIONISTA
012700000807     D  PA3TIP                 1      2
012800000807     D* RAGIONE SOCIALE TRAZIONISTA
012900000807     D  PA3RSC                 3     37
013000000807     D* S.I. TRAZIONISTA
013100000807     D  PA3CSF                38     40
013200960215     D* CODICE TRAZIONISTA
013300000807     D  PA3PDR                41     47  0
013400960215     D* PA3FLG = "3" --> NON ESISTONO TRAZIONISTI PER LA CHIAVE
013500960215     D*                  ALFABETICA RICHIESTA
013600000807     D  PA3FLG                48     48
013700960219     D* PASSAGGIO PARAMETRI A PGM DI CREAZIONE GRIGLIA
013800960219     D DSTL24          DS
013900960219     D* DATA TRAINO DAL
014000960219     D  D24DTD                 1      8  0
014100960219     D* DATA TRAINO AL
014200960219     D  D24DTA                 9     16  0
014300960219     D* NUMERO TRAINO
014400960219     D  D24TRN                17     23  0
014500960219     D* TERMINAL DI PARTENZA
014600960219     D  D24TFP                24     26  0
014700960219     D* DATA DECORRENZA TRAINO
014800960219     D  D24DDE                27     34  0
014900960219     D* CREAZIONE SE GIORNO FESTIVO (' '=No controllo festivo
015000960219     D*                             ('S'=Controllo festivo e crea solo
015100960219     D*                                  se non festivo)
015200960219     D  D24FES                35     35
015300960322     D                 DS
015400960322     D  FVDVL1                 1      9  4
015500960322     D  FVDVL2                10     18  4
015600960322     D  FVDVL3                19     27  4
015700960322     D  FVDVL4                28     36  4
015800960322     D  FVDVL5                37     45  4
015900960322     D  FVDVL6                46     54  4
016000960322     D  FVD                    1     54  4
016100960322     D                                     DIM(6)                               CAMPI TOT.VOL.L
016200951130     D                SDS
016300960214     D  V1DPGM                 1     10
016400960213     D TIT1            C                   CONST('* ELENCO FOGLI VIAGG-
016500960213     D                                     IO *')
016600960213     D TIT2            C                   CONST('*      DA ABBINARE  -
016700960213     D                                        *')
016800960213     D TIT3            C                   CONST('*CONFERMA FOGLI VIAG-
016900960213     D                                     GIO*')
017000960213     D TIT4            C                   CONST('*      NON ABBINATI -
017100960213     D                                        *')
017200000211     D*---------------------
017300000211     D* DS CAUSALI CONTABILIZZAZIONE
017400000211     D*---------------------
017500000211     D ANGY4ZDS      E DS
017600000211     D XTABDS        E DS                  INZ
017700000211     D XATBDS        E DS                  INZ
017800000211     D*---------------------
017900000211     D* DS per reperimento società
018000000211     D*---------------------
018100000211     D soc001        E DS                  EXTNAME(xsoc001ds)
018200000211     D xsocds          DS          1000
018300951128     C****************************************************************
018400951128     C*  RIEPILOGO INDICATORI
018500951128     C***************************************************************
018600960214     C* 01    -  ON --> RICEVUTI PARAMETRI DI INPUT > 0
018700960213     C* 05    -  ON --> ELENCO FOGLI VIAGGIO DA ABBINARE MANUALMENTE
018800960213     C*          OFF--> CONFERMA FOGLI VIAGGIO NON ABBINATI
018900960215     C* 28    -  ERRORE GENERICO VIDEATE
019000951129     C* 29    -  READC SUBFILE
019100951128     C* 30    -  COMODO
019200960215     C* 40    -  ERRORE CODICE TRAZIONISTA
019300960215     C* 41    -  ERRORE IMPORTO DA CONFERMARE
019400960215     C* 42    -  ERRORE CAUSALE CONTABILIZZAZIONE
019500010827     C* 43    -  ERRORE CAMPO SCELTA TL42S01
019600010828     C* 45    -  ERRORE divisa
019700951128     C***************************************************************
019800010828    1C     D42MOD        IFEQ      'C'
019900010828     C                   MOVE      D42TRN        V2CTRN
020000010828     C                   MOVE      D42DTN        WDTN
020100010828     C                   MOVE      D42LNP        FVSLNP
020200010828     C                   MOVE      D42LNA        FVSLNA
020300960312     C                   MOVEA     PFLP          FLP
020400960311     C                   EXSR      CHKABB
020500960312    2C     WOKABB        IFEQ      ' '
020600010828     C                   MOVEL     '1'           D42ERR
020700960311    2C                   END
020800960311     C                   GOTO      FINE
020900960313   X1C                   ELSE
021000010827     C                   OPEN      TNTL42D
021100010827     C* HO ASTERISCATO QUESTE SPECIFICHE PER EVITARE CHE IL TNTL42R
021200960313     C* VENGA RICHIAMATO IN MODO RICORSIVO E LE HO MESSE IN UN CLP
021300010827     C* A CUI HO ASSOCIATO L'AZIONE INTERATTIVA TL42 DI CONFERMA FV
021400960313     C* NON ABBINATI
021500010828    2C*          D42MOD    IFEQ *BLANKS
021600960220     C* Richiamo abbinamento automatico fv a traini
021700960313     C*                    CLEARKPJBU
021800960313     C*                    CALL 'TNTL25R'
021900960313     C*                    PARM           KPJBA
022000960313    2C*                    END
022100960311    1C                   END
022200960215     C* CARICAMENTO SUBFILE SCELTA FOGLI VIAGGIO
022300960223     C                   CLEAR                   SNF
022400960215     C                   EXSR      RIEINI
022500951129     C* EMISSIONE SUBFILE
022600960215     C     EMC01         TAG
022700010827     C  N05              WRITE     TL42Z01
022800010827     C   05              WRITE     TL42Z03
022900010827     C                   WRITE     TL42D01
023000010827     C                   EXFMT     TL42C01
023100951128     C                   SETOFF                                       28
023200960228     C* Tasto F3=Fine
023300960228     C     *INKC         CABEQ     *ON           FINE
023400960228     C* Tasto F12=Ritorno
023500960228     C     *INKL         CABEQ     *ON           FINE
023600960228     C* Tasto F5=Rivisualizzazione
023700960228     C     *INKE         IFEQ      *ON
023800960228     C                   EXSR      RIEINI
023900960228     C                   GOTO      EMC01
024000960228     C                   END
024100960320     C* Tasto F7=Interrogazione fogli viaggio
024200960228    1C     *INKG         IFEQ      *ON
024300960320     C                   CLEAR                   DSTL37
024400960320     C                   MOVEL     'P'           D37FLG
024500960320     C                   MOVEL     DSTL37        KPJBU
024600960320     C                   CALL      'TNTL37R'
024700960320     C                   PARM                    KPJBA
024800960228     C                   GOTO      EMC01
024900960228    1C                   END
025000960228     C* Tasto F14=Manutenzione anagrafica traini
025100960228    1C     *INKN         IFEQ      *ON
025200960228     C                   CLEAR                   DS00
025300960228     C                   MOVEL     'T02'         D00OP0                         *PARAMETRI FISSI
025400960228     C                   MOVEL     'F14'         D00OP1
025500960229     C                   MOVEL     '0'           D00F03
025600960229     C                   MOVEL     '0'           D00F12
025700960229     C                   MOVEL     '0'           D00ERR
025800960229     C                   MOVEL     *BLANKS       D00MSG
025900960228     C                   MOVEL     DS00          KPJBU
026000960229     C                   CALL      'TNTL01R'
026100960228     C                   PARM                    KPJBA
026200960228     C                   GOTO      EMC01
026300960228     C                   END
026400960228     C* Tasto F6=Annullamento/Ripristino FV
026500960228     C     *INKF         IFEQ      *ON
026600960228     C                   EXSR      ANNRIP
026700960228     C                   GOTO      EMC01
026800960228     C                   END
026900960320     C* GESTIONE OPZIONI DI SELEZIONE FV:
027000960215     C* Se passano da visualizz. FV annullati a non visualizzazione o
027100960215     C* viceversa ricarico il subfile
027200960214     C     V1CANN        IFNE      WCANN
027300960214     C                   MOVE      V1CANN        WCANN
027400960215     C                   EXSR      RIEINI
027500960215     C                   GOTO      EMC01
027600960214     C                   END
027700960228     C* Se cambiano visualizza bis/normali/tutti ricarico subfile
027800960228     C     V1CBIS        IFNE      WCBIS
027900960228     C                   MOVE      V1CBIS        WCBIS
028000960228     C                   EXSR      RIEINI
028100960228     C                   GOTO      EMC01
028200960228     C                   END
028300960307     C* FV controllati/non controllati/tutti
028400960307     C     V1CCNT        IFNE      WCCNT
028500960307     C                   MOVE      V1CCNT        WCCNT
028600960307     C                   EXSR      RIEINI
028700960307     C                   GOTO      EMC01
028800960307     C                   END
028900951129     C* Solo se ho caricato qualcosa:
029000951129    1C     NRR1          IFGT      *ZEROS
029100960215     C* Tasto Rollup
029200951129    2C     *IN25         IFEQ      *ON
029300951128     C* Fine lettura --> emetto messaggio fine scorrimento
029400951129    3C     *IN23         IFEQ      *ON                                          FINE LETTURA
029500960214     C                   Z-ADD     W1CNBR        V1CNBR
029600951129     C                   MOVEL     MSG(1)        V1ZMSG
029700951129     C                   SETON                                        28
029800951129     C                   ELSE
029900960215     C                   EXSR      RIES01
030000951129    3C                   END
030100960215     C                   GOTO      EMC01
030200951129    2C                   END
030300960215     C* ENTER: Controlli
030400960320     C                   EXSR      CTRVIS
030500960320     C* Se interrogati fogli viaggio riemetto il subfile
030600960320     C     WVIS          CABEQ     '1'           EMC01
030700960213     C   05              EXSR      CT1S01
030800960213     C  N05              EXSR      CT2S01
030900960215     C   28              GOTO      EMC01
031000960215     C*
031100960215     C* VIDEATA DI RICHIESTA PARAMETRI PER CREAZIONE ANAGRAFICA TRAINO
031200960215    2C     *IN05         IFEQ      *OFF
031300960215     C                   EXSR      RIED02
031400960220     C   28              GOTO      EMC01
031500960215     C     EMD02         TAG
031600010827     C                   WRITE     TL42Z02
031700010827     C                   WRITE     TL42D01
031800010827     C                   EXFMT     TL42D02
031900960215     C                   SETOFF                                       28
032000960215     C* Tasto F3=Fine
032100960215     C     *INKC         CABEQ     *ON           FINE
032200960215     C* Tasto F12=Ritorno
032300960220     C     *INKL         IFEQ      *ON
032400960220     C                   SETOFF                                       28
032500960220     C                   GOTO      EMC01
032600960220     C                   END
032700960215     C* CONTROLLI
032800960215     C                   EXSR      CTRD02
032900960215     C   28              GOTO      EMD02
033000960215     C* F6= CONFERMA
033100960215     C   KF              EXSR      CONFE
033200960219     C   KF              GOTO      EMC01
033300960215     C  NKF              GOTO      EMD02
033400960215    2C                   END
033500960215     C*
033600951129    1C                   END
033700971118     C**
033800971118     C                   CLEAR                   DSLV55
033900971118     C                   MOVEL     'C'           D55TLA
034000971118     C                   CALL      'FNLV55R'
034100971118     C                   PARM                    DSLV55
034200971118     C**
034300951128     C     FINE          TAG
034400010827     C                   MOVEL     DSTL42        KPJBU
034500951128     C                   SETON                                        LR
034600951129     C**************  CARICAMENTO INIZIALE SUBFILE  *******************
034700960215     C     RIEINI        BEGSR
034800951129     C* PULIZIA SUBFILE
034900951129     C                   SETOFF                                       2123
035000010827     C                   WRITE     TL42C01
035100951129     C                   SETON                                        2021
035200951129     C                   MOVE      *ZEROS        NRR1              5 0
035300951129     C                   CLEAR                   NRRSAV
035400960214     C                   CLEAR                   WNRR1
035500960214     C                   CLEAR                   V1CNBR
035600960214     C                   CLEAR                   W1CNBR
035700960214     C     V1CANN        IFEQ      *BLANKS
035800960214     C                   MOVE      'N'           V1CANN
035900960214     C                   MOVE      'N'           WCANN
036000960228     C                   MOVE      'T'           V1CBIS
036100960228     C                   MOVE      'T'           WCBIS
036200960307     C                   MOVE      'C'           V1CCNT
036300960307     C                   MOVE      'C'           WCCNT
036400960214     C                   END
036500960424     C* REPERISCO ULTIMO TRAINO CONFERMATO
036600960315    1C  N05V1CTRN        IFEQ      *ZEROS
036700960315     C     *HIVAL        SETLL     TNTST01L
036800960315     C                   READP(N)  TNTST01L                               30
036900960315    2C     *IN30         DOWEQ     *OFF
037000960315     C     TSTTRN        ANDGE     9000000
037100960424    3C     TSTATB        IFEQ      *BLANKS
037200960315     C                   MOVE      TSTTRN        V1CTRN
037300960315     C                   CLEAR                   WLBDA8
037400960315     C                   MOVE      TSTDTN        G08INV
037500960315     C                   MOVEL     '3'           G08ERR
037600960315     C                   CALL      'XSRDA8'
037700960315     C                   PARM                    WLBDA8
037800960315     C                   MOVE      G08DAT        V1CDTN
037900960315     C                   SETON                                        30
038000960315   X3C                   ELSE
038100960315     C                   READP(N)  TNTST01L                               30
038200960315    3C                   END
038300960315    2C                   ENDDO
038400960315    1C                   END
038500951129     C* POSIZIONAMENTO INIZIALE ARCHIVIO F.V. PER CARICAMENTO SUBFILE
038600951129     C     KFVS          SETLL     TNFVS02L
038700951129     C   01KFVS          READE     TNFVS02L                               30
038800951129     C  N01KFVS2         READE     TNFVS02L                               30
038900960214     C  N30              MOVEL     DSFVS         WDSFVS
039000960215     C  N30              EXSR      RIES01
039100951129     C* SE NON HO CARICATO NULLA SPENGO INDICATORE SFLDSP
039200951129     C     NRR1          IFEQ      *ZEROS
039300951129     C                   SETOFF                                       20
039400951129     C                   END
039500951129     C                   ENDSR
039600960213     C**************  CARICAMENTO SUBFILE  ****************************
039700960215     C     RIES01        BEGSR
039800960220     C                   CLEAR                   WLNPS
039900960214     C                   MOVEL     WDSFVS        DSFVS
040000951128     C                   Z-ADD     NRRSAV        NRR1
040100960314     C                   ADD       16            WNRR1
040200951222    1C     NRR1          DOWLT     WNRR1
040300951128     C     *IN30         ANDEQ     *OFF
040400960223     C                   SETOFF                                       44
040500960228     C     FVSATB        CABEQ     '*'           LEGGI
040600960228     C     V1CANN        IFEQ      'N'
040700960228     C     FVSATB        ANDNE     ' '
040800960228     C                   GOTO      LEGGI
040900960228     C                   END
041000960307     C  N05V1CBIS        IFEQ      'B'
041100960228     C     FVSBIS        ANDEQ     *BLANKS
041200960228     C     V1CBIS        OREQ      'N'
041300960228     C     FVSBIS        ANDNE     *BLANKS
041400960228     C                   GOTO      LEGGI
041500960228     C                   END
041600960307     C  N05V1CCNT        IFEQ      'C'
041700960307     C     FVSDKF        ANDEQ     *ZEROS
041800960307     C     V1CCNT        OREQ      'N'
041900960307     C     FVSDKF        ANDGT     *ZEROS
042000960307     C                   GOTO      LEGGI
042100960307     C                   END
042200960229     C*
042300951222     C                   MOVE      ' '           WTROV
042400951222     C* SE PARAMETRI DI INPUT > 0 VERIFICO CHE UNA DELLE FFA CARICATE
042500951222     C* SIA UNA DELLE LINEE DI SCARICO DEL F.V.
042600951222    2C     *IN01         IFEQ      *ON
042700951222     C     FVSLNA        LOOKUP    FFA                                    30
042800951222    3C     *IN30         IFEQ      *OFF
042900951222     C                   Z-ADD     1             II
043000951222     C                   MOVE      FFA(II)       W003A             3
043100951222    4C     *IN30         DOWEQ     *OFF
043200951222     C     FFA(II)       ANDNE     *ZEROS
043300951222     C     W003A         LOOKUP    FLP                                    30
043400951222     C  N30              ADD       1             II
043500951222     C  N30              MOVE      FFA(II)       W003A
043600951222     C   30              MOVE      '1'           WTROV
043700951222    4C                   ENDDO
043800951222   X3C                   ELSE
043900951222     C                   MOVE      '1'           WTROV             1
044000951222    3C                   END
044100951222    2C                   END
044200951128     C*
044300951222    2C     *IN01         IFEQ      *OFF
044400951222     C     WTROV         OREQ      '1'
044500951128     C                   MOVE      *BLANKS       V1SSCE
044600951128     C                   MOVE      FVSLNP        V1SLNP
044700951128     C                   MOVE      FVSNFV        V1SNFV
044800960220     C                   CLEAR                   V1DLNP
044900960219     C* A rottura, decodifico linea di partenza
045000960223    3C     FVSLNP        IFNE      WLNPS
045100951130     C     FVSLNP        CHAIN     AZORG01L                           30
045200960223    4C     *IN30         IFEQ      *OFF
045300951130     C     ORGFVA        ANDEQ     *BLANKS
045400960219     C                   MOVEL     ORGDES        V1DLNP
045500960220     C                   MOVEL     ORGDES        V1HLNP
045600960220     C                   ELSE
045700960220     C                   CLEAR                   V1HLNP
045800960223    4C                   END
045900960219     C                   MOVE      FVSLNP        WLNPS
046000960223    3C                   END
046100960223    3C                   SELECT
046200960123     C     FVSBIS        WHENEQ    'B'
046300960123     C                   MOVEL     'BIS'         V1SBIS
046400960123     C     FVSBIS        WHENEQ    'E'
046500960123     C                   MOVEL     'ECC'         V1SBIS
046600960123     C     FVSBIS        WHENEQ    ' '
046700960123     C                   CLEAR                   V1SBIS
046800960223    3C                   ENDSL
046900951128     C* Rovescio data foglio viaggio
047000951128     C                   CLEAR                   WLBDA8
047100951128     C                   MOVE      FVSDFV        G08INV
047200951128     C                   MOVEL     '3'           G08ERR
047300951128     C                   CALL      'XSRDA8'
047400951128     C                   PARM                    WLBDA8
047500951128     C                   MOVE      G08DAT        V1SDFV
047600951128     C                   MOVE      FVSLNA        V1SLNA
047700951128     C* Decodifico fil.finale arrivo
047800951128     C     FVSLNA        CHAIN     AZORG01L                           30
047900951222    3C     *IN30         IFEQ      *OFF
048000951128     C     ORGFVA        ANDEQ     *BLANKS
048100951128     C                   MOVEL     ORGDES        V1DLNA
048200951128     C                   ELSE
048300951128     C                   CLEAR                   V1DLNA
048400951222    3C                   END
048500960223    3C     FVSATB        IFEQ      *BLANKS
048600960214     C                   CLEAR                   V1SANN
048700960214     C                   ELSE
048800960214     C                   MOVE      'A'           V1SANN
048900960223    3C                   END
049000951128     C* SCRITTURA RECORD DI SFL
049100960223     C                   Z-ADD     1             W
049200960223     C                   MOVE      V1SLNP        W003A             3
049300960223     C     V1SNFV        LOOKUP    SNF(W)                                 30
049400960223     C     *IN30         IFEQ      *ON
049500960223     C     CAR(W)        ANDEQ     W003A
049600960223     C                   SETON                                        44
049700960223     C                   END
049800951128     C                   ADD       1             NRR1
049900010827     C                   WRITE     TL42S01
050000951222    2C                   END
050100960228     C     LEGGI         TAG
050200951128     C*
050300951128     C   01KFVS          READE     TNFVS02L                               30
050400951128     C  N01KFVS2         READE     TNFVS02L                               30
050500951222    1C                   ENDDO
050600951129     C* ACCENDO INDICATORE DI FINE LETTURA
050700951129     C   30              SETON                                        23
050800951128     C* SE HO CARICATO ALMENO UN RECORD DI SFL INCREMENTO SFLRCDNBR
050900951222    1C     NRR1          IFGT      NRRSAV
051000951128     C     NRRSAV        ADD       1             V1CNBR
051100951222    1C                   END
051200960214     C* SE FINE LETTURA MEMORIZZO SFLRCDNBR DELL'ULTIMA PAGINA
051300960214     C   30              Z-ADD     V1CNBR        W1CNBR
051400951128     C* MEMORIZZO NRR
051500951128     C                   Z-ADD     NRR1          NRRSAV
051600960214     C* MEMORIZZO DATI DELL'ULTIMO FV LETTO PER ESSERE SICURA DI AVERE
051700960214     C* IN LINEA QUESTO RECORD IN CASO DI ROLLUP
051800960214     C  N30              MOVEL     DSFVS         WDSFVS
051900960223     C                   SETOFF                                       44
052000960223     C* SE IN SEGUITO A CONFERMA PRECEDENTE NON SONO RIUSCITA AD
052100960223     C* ABBINARE TUTTI I FV, SOLO LA PRIMA VOLTA EMETTO MESSAGGIO
052200960223     C     WMSG          IFEQ      *BLANKS
052300960223     C                   XFOOT     SNF           W0070             7 0
052400960223     C     W0070         IFGT      *ZEROS
052500960223     C                   SETON                                        28
052600960223     C                   MOVEL     MSG(19)       V1ZMSG
052700960223     C                   MOVE      '1'           WMSG              1
052800960223     C                   END
052900960223     C                   END
053000951128     C                   ENDSR
053100960214     C**************  ANNULLAMENTO/RIPRISTINO FV   ********************
053200960214     C     ANNRIP        BEGSR
053300960214     C                   SETON                                        22
053400960214     C                   SETOFF                                       43
053500010827     C                   READC     TL42S01                                29
053600960214    1C     *IN29         DOWEQ     *OFF
053700960214     C* INTERRUZIONE ANNULLAMENTO/RIPRISTINO SE RICHIESTO L'ANNULL.
053800960214     C* DI UN FV GIA' ANNULLATO
053900960215     C     V1SSCE        IFEQ      'AN'
054000960214     C     V1SANN        ANDEQ     'A'
054100960214     C                   SETON                                        4328
054200960214     C                   MOVEL     MSG(7)        V1ZMSG
054300960214     C                   GOTO      ENDANN
054400960214     C                   END
054500960214     C* INTERRUZIONE ANNULLAMENTO/RIPRISTINO SE RICHIESTO IL RIPRISTINO
054600960214     C* DI UN FV NON ANNULLATO
054700960215     C     V1SSCE        IFEQ      'RP'
054800960214     C     V1SANN        ANDEQ     *BLANKS
054900960214     C                   SETON                                        4328
055000960214     C                   MOVEL     MSG(8)        V1ZMSG
055100960214     C                   GOTO      ENDANN
055200960214     C                   END
055300960215    2C     V1SSCE        IFEQ      'AN'
055400960215     C     V1SSCE        OREQ      'RP'
055500960219     C                   MOVE      V1SLNP        KLNP
055600960219     C                   MOVE      V1SNFV        KNFV
055700960214     C     KFVS1         CHAIN     TNFVS01L                           30
055800960214    3C     *IN30         IFEQ      *OFF
055900960215    4C     V1SSCE        IFEQ      'AN'
056000960214     C                   MOVE      'A'           FVSATB
056100960214     C                   UPDATE    TNFVS000
056200960214     C                   MOVE      'A'           V1SANN
056300960214   X4C                   ELSE
056400960214     C                   MOVE      *BLANKS       FVSATB
056500960214     C                   UPDATE    TNFVS000
056600960214     C                   CLEAR                   V1SANN
056700960214    4C                   END
056800960214    3C                   END
056900960214     C                   CLEAR                   V1SSCE
057000960214    2C                   END
057100960214     C     ENDANN        TAG
057200960214     C   28              Z-ADD     NRR1          V1CNBR
057300010827     C                   UPDATE    TL42S01
057400960214     C   28              SETON                                        29
057500010827     C  N28              READC     TL42S01                                29
057600960214    1C                   ENDDO
057700960214     C                   ENDSR
057800960320     C*******  PRIMO GIRO LETTURA SUBFILE PER VISUALIZZAZIONE FV ******
057900960320     C     CTRVIS        BEGSR
058000960320     C                   SETON                                        22
058100960320     C                   MOVE      *BLANKS       WVIS              1
058200010827     C                   READC     TL42S01                                29
058300960320     C     *IN29         DOWEQ     *OFF
058400960320     C     V1SSCE        IFEQ      'FV'
058500960320     C                   MOVE      '1'           WVIS
058600960320     C                   CLEAR                   DSTL37
058700960320     C                   MOVEL     'R'           D37FLG
058800960320     C                   MOVE      V1SLNP        D37LNP
058900960320     C                   MOVE      V1SNFV        D37NFV
059000960320     C                   MOVEL     DSTL37        KPJBU
059100960320     C                   CALL      'TNTL37R'
059200960320     C                   PARM                    KPJBA
059300960320     C                   CLEAR                   V1SSCE
059400960320     C                   END
059500010827     C                   UPDATE    TL42S01
059600010827     C                   READC     TL42S01                                29
059700960320     C                   ENDDO
059800960320     C                   ENDSR
059900960213     C*******  CONTROLLI SUBFILE MODO ELENCO FOGLI DA ABBINARE  *******
060000960213     C     CT1S01        BEGSR
060100960219     C* N.B.: NON CONTROLLO CHE IL CAMPO SCELTA SIA=1
060200960219     C*       E' POSSIBILE SCEGLIERE CON QUALUNQUE CARATTERE.
060300960219     C*       TUTTAVIA, A VIDEO VIENE INDICATO 1 COME CARATTERE DI
060400960219     C*       SCELTA SEMPLICEMENTE PER PROPORRE QUALCOSA.
060500010827     C                   READC     TL42S01                                29
060600960213    2C     *IN29         DOWEQ     *OFF
060700960214     C     V1SSCE        ANDEQ     *BLANKS
060800010827     C                   READC     TL42S01                                29
060900960213    2C                   ENDDO
061000960213    2C     *IN29         IFEQ      *OFF
061100960213     C* Se scelto f.v. imposto parametri di output
061200010828     C                   MOVE      V1SLNP        D42LNP
061300010828     C                   MOVE      V1SNFV        D42NFV
061400960213     C                   ELSE
061500960213     C* Se f.v non è stato scelto riemetto sfl con msg
061600960213     C                   MOVEL     MSG(2)        V1ZMSG
061700960213     C                   SETON                                        28
061800960213    2C                   END
061900960213     C                   ENDSR
062000960213     C*******  CONTROLLI SUBFILE MODO ELENCO CONFERMA FV NON ABB. *****
062100960213     C     CT2S01        BEGSR
062200960213     C                   SETON                                        22
062300960214     C                   SETOFF                                       43
062400960213     C                   CLEAR                   LNP
062500960213     C                   CLEAR                   SCE
062600960215     C                   CLEAR                   WSCE
062700960216     C                   CLEAR                   WPDR
062800960216     C                   CLEAR                   WDPD
062900960216     C                   CLEAR                   WIPT
063000960320     C                   CLEAR                   WIPT1
063100010827     c                   clear                   wdiv
063200010827     c                   clear                   wdiv1
063300960424     C                   CLEAR                   WBIS1
063400960219     C                   CLEAR                   WTMZ
063500960319     C                   CLEAR                   WFES
063600960228     C                   CLEAR                   WPDR1
063700960228     C                   CLEAR                   WDPD1
063800960228     C                   CLEAR                   WTMZ1
063900960319     C                   CLEAR                   WFES1
064000960213     C                   CLEAR                   NRR
064100960213     C                   CLEAR                   X
064200960214     C* Pulizia ds Multipla
064300960213     C                   Z-ADD     1             II                3 0
064400960215    1C     II            DOWLE     100
064500960215     C     II            OCCUR     DSFIL
064600960216     C                   CLEAR                   NFV
064700960216     C                   CLEAR                   FSC
064800960216     C                   CLEAR                   FVG
064900960216     C                   CLEAR                   DDRP
065000960216     C                   CLEAR                   DHRP
065100960216     C                   CLEAR                   DLNA
065200960213     C                   ADD       1             II
065300960214    1C                   ENDDO
065400960213     C*
065500010827     C                   READC     TL42S01                                29
065600960213    1C     *IN29         DOWEQ     *OFF
065700960213    2C     V1SSCE        IFNE      *BLANKS
065800960214     C* Errore se campo scelta contiene caratteri alfabetici diversi da
065900960320     C* 'AN' o 'RP'
066000960219    3C     V1SSCE        IFNE      'AN'
066100960215     C     V1SSCE        ANDNE     'RP'
066200960219     C                   TESTN                   V1SSCE               30
066300960219    4C     *IN30         IFEQ      *ON
066400960219     C                   MOVE      V1SSCE        W001A             1
066500960219    5C     W001A         IFLT      '0'
066600960219     C                   SETOFF                                       30
066700960219    5C                   END
066800960219    4C                   END
066900960219    4C     *IN30         IFEQ      *OFF
067000960219     C                   SETON                                        4328
067100960219     C                   MOVEL     MSG(3)        V1ZMSG
067200960219     C                   GOTO      AGGIO
067300960219    4C                   END
067400960219    3C                   END
067500960320     C* Se scelta = 'AN' o 'RP' ignoro e clearo campo di scelta
067600960215    3C     V1SSCE        IFEQ      'AN'
067700960215     C     V1SSCE        OREQ      'RP'
067800960214     C                   CLEAR                   V1SSCE
067900960214    3C                   END
068000960228     C     V1SSCE        IFEQ      '00'
068100960228     C                   MOVE      *BLANKS       V1SSCE
068200960228     C                   END
068300960214     C* ELABORO RECORDS CON NUMERO SCELTA >0
068400960228    3C     V1SSCE        IFGT      '00'
068500960214     C* Errore se attribuito un numero scelta ad un FV annullato
068600960214    4C     V1SANN        IFEQ      'A'
068700960214     C                   SETON                                        4328
068800960214     C                   MOVEL     MSG(10)       V1ZMSG
068900960214     C                   GOTO      AGGIO
069000960214    4C                   END
069100960214     C* Errore se già immesso questo numero scelta
069200960213     C     V1SSCE        LOOKUP    SCE                                    30
069300960214    4C     *IN30         IFEQ      *ON
069400960213     C                   SETON                                        4328
069500960213     C                   MOVEL     MSG(4)        V1ZMSG
069600960213     C                   GOTO      AGGIO
069700960214    4C                   END
069800960214     C* Errore se già scelto un fv con questa lnp
069900960214     C                   MOVE      V1SLNP        W003A             3
070000960214     C     W003A         LOOKUP    LNP                                    30
070100960214    4C     *IN30         IFEQ      *ON
070200960213     C                   SETON                                        4328
070300960215     C                   MOVEA     MSG(5)        W78
070400960215     C                   MOVE      V1SLNP        W003A             3
070500960215     C                   MOVEA     W003A         W78(50)
070600960220     C                   MOVEA     V1HLNP        W78(54)
070700960215     C                   MOVEA     W78           V1ZMSG
070800960213     C                   GOTO      AGGIO
070900960214    4C                   END
071000960214     C* SE OK MEMORIZZO i dati del foglio viaggio e numero scelta
071100960219     C                   MOVE      V1SLNP        KLNP
071200960219     C                   MOVE      V1SNFV        KNFV
071300960219     C     KFVS1         CHAIN     TNFVS01L                           30
071400010828     C     *IN30         IFEQ      *OFF
071500011012     c                   eval      flgged = '0'
071600010828      * aggancio il file delle estensioni foglio viaggio per gli importi in
071700010907      * divisa (in questo momento mi serve solo IPT
071800010910     c                   clear                   fwsipt
071900010910     c                   clear                   fwsvip
072000010828     c     kfws          chain     tnfws01l
072100010907      * se l'importo dell'estensione è = 0 e l'importo sul file
072200010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
072300010907      * la divisa è lire
072400010907     c                   if        fwsipt = *zeros and fvsipt <> 0
072500010907     c                   eval      fwsvip = 'ITL'
072600010907     c                   z-add     fvsipt        fwsipt
072700010907     c                   endif
072800010907      * se l'importo dell'estensione è = 0 e l'importo sul file
072900010907      * principale è = 0, imposto la divisa a blank per poi prendere quella che
073000010907      * mi serve caso per caso
073100010907     c                   if        fwsipt = *zeros and fvsipt = 0
073200010907     c                   z-add     fvsipt        fwsipt
073300010907     c                   clear                   fwsvip
073400010907     c                   endif
073500010907
073600010907      * in questo caso devo emettere la divisa a video quindi se vuota devo prendere
073700010907      * la moneta corrente §GEDCN
073800010907     c                   if        fwsvip = *blanks
073900010907     c                   eval      fwsvip = §gedcn
074000011012     c                   eval      flgged = '1'
074100010907     c                   endif
074200010828
074300960213     C                   ADD       1             X                 3 0
074400960213     C                   MOVE      V1SLNP        LNP(X)
074500960213     C                   MOVE      V1SSCE        SCE(X)
074600960216     C                   MOVE      FVSDFV        DFV(X)
074700960213     C                   MOVE      NRR1          NRR(X)
074800960215     C     X             OCCUR     DSFIL
074900960216     C                   MOVE      V1SNFV        NFV
075000960325     C                   MOVE      FVSLNA        FSC(1)
075100960325     C                   MOVE      FVSLNA        FVG(1)
075200960325     C                   MOVEA     FLP           FSC(2)
075300960325     C                   MOVEA     FFV           FVG(2)
075400960216     C                   MOVE      FVSDRP        DDRP
075500960216     C                   MOVE      FVSHRP        DHRP
075600960216     C                   MOVE      FVSLNA        DLNA
075700960216     C* MEMORIZZO NUMERO SCELTA PIù BASSO ED I SEGUENTI DATI:
075800960215     C     V1SSCE        IFLT      WSCE
075900960215     C     WSCE          OREQ      *BLANKS
076000960215     C                   MOVE      V1SSCE        WSCE
076100960228     C                   MOVE      FVSPDR        WPDR1
076200960228     C                   MOVEL     FVSDPD        WDPD1
076300960228     C                   MOVE      FVSTMZ        WTMZ1
076400960319     C                   MOVE      FVSLNP        WFES1
076500010828     C**!!!              MOVE      FVSIPT        WIPT1
076600010828     c                   z-add     fwsipt        wipt1
076700010828     c                   movel     fwsvip        wdiv1
076800960424     C                   MOVE      FVSBIS        WBIS1
076900960320     C                   END
077000960228     C* SOLO SE DIVERSI DA 0
077100010828     C**!!!FVSIPT        IFGT      *ZEROS
077200010828     C**!!!WIPT          ANDEQ     *ZEROS
077300010828     c                   if        fwsipt > 0 and wipt = 0
077400960216     C                   MOVE      FVSPDR        WPDR
077500960216     C                   MOVEL     FVSDPD        WDPD
077600010828     C**!!!              MOVE      FVSIPT        WIPT
077700010828     c                   z-add     fwsipt        wipt
077800010828     c                   move      fwsvip        wdiv
077900960219     C                   MOVE      FVSTMZ        WTMZ
078000960319     C                   MOVE      FVSLNP        WFES
078100960228     C                   END
078200960216     C                   END
078300960214    3C                   END
078400960214    2C                   END
078500960213     C     AGGIO         TAG
078600960214     C   28              Z-ADD     NRR1          V1CNBR
078700010827     C                   UPDATE    TL42S01
078800960213     C   28              SETON                                        29
078900010827     C  N28              READC     TL42S01                                29
079000960213    1C                   ENDDO
079100960215     C   28              GOTO      ECTS01
079200960214     C* CONTROLLI FINALI
079300960214     C* Errore se non è stato scelto nemmeno un foglio viaggio
079400960215    1C     WSCE          IFEQ      *BLANKS
079500960213     C                   MOVEL     MSG(2)        V1ZMSG
079600960213     C                   SETON                                        28
079700960215     C                   GOTO      ECTS01
079800960215    1C                   END
079900960214     C* Se non trovati errori eseguo ulteriori controlli
080000960215     C* Errore se date fogli viaggio scelti non in sequenza
080100960215     C                   MOVEA     SCE           SSC
080200960215     C                   SORTA     SSC
080300960215     C                   MOVE      *ZEROS        WDFV
080400960213     C                   Z-ADD     1             X
080500960215     C     WSCE          LOOKUP    SSC(X)                                 30
080600960215     C* Memorizzo indice di schiera del primo FV
080700960215     C                   MOVE      X             WFIRST
080800960215    1C     X             DOWLE     100
080900960215     C                   Z-ADD     1             II
081000960215     C     SSC(X)        LOOKUP    SCE(II)                                30
081100960216    2C     WDFV          IFNE      *ZEROS
081200960216     C     DFV(II)       ANDLT     WDFV
081300010827     C     NRR(II)       CHAIN     TL42S01                            30
081400960215     C                   SETON                                        2843
081500960215     C                   MOVEL     MSG(9)        V1ZMSG
081600960215     C                   Z-ADD     NRR(II)       V1CNBR
081700010827     C                   UPDATE    TL42S01
081800960215     C                   GOTO      ECTS01
081900960216    2C                   END
082000960219     C* MEMORIZZO DATA TRAINO
082100960219     C     WDFV          IFEQ      *ZEROS
082200960219     C                   MOVE      DFV(II)       WDTN
082300960219     C                   END
082400960219     C* SE DATA FOGLIO VIAGGIO DIVERSA DA DATA TRAINO CONTROLLO CHE
082500960219     C* NON SIA UN GIORNO FESTIVO
082600960219     C     WDTN          IFNE      DFV(II)
082700960220     C                   MOVE      DFV(II)       W0080             8 0
082800960219     C                   EXSR      CTRFES
082900960319     C     WFEST         IFNE      *BLANKS
083000010827     C     NRR(II)       CHAIN     TL42S01                            30
083100960219     C                   SETON                                        2843
083200960219     C                   MOVEL     MSG(16)       V1ZMSG
083300960219     C                   Z-ADD     NRR(II)       V1CNBR
083400010827     C                   UPDATE    TL42S01
083500960219     C                   GOTO      ECTS01
083600960219     C                   END
083700960219     C                   END
083800960215     C                   MOVE      DFV(II)       WDFV
083900960215     C                   ADD       1             X
084000960215    1C                   ENDDO
084100960214     C* Errore se nei FV scelti esiste almeno una filiale di scarico
084200960214     C* = alla lnp del primo FV
084300960215     C                   Z-ADD     1             X
084400960215     C     WSCE          LOOKUP    SCE(X)                                 30
084500960213     C                   Z-ADD     1             II
084600960215    1C     II            DOWLE     100
084700960215     C     LNP(II)       ANDNE     *BLANKS
084800960213     C* NON LEGGO RICORRENZA RELATIVA AL PRIMO FV
084900960215    2C     II            IFNE      X
085000960215     C     II            OCCUR     DSFIL
085100960216     C     LNP(X)        LOOKUP    FSC                                    30
085200960215    3C     *IN30         IFEQ      *ON
085300010827     C     NRR(II)       CHAIN     TL42S01                            30
085400960213     C                   SETON                                        4328
085500960213     C                   MOVEL     MSG(6)        V1ZMSG
085600960214     C                   Z-ADD     NRR(II)       V1CNBR
085700010827     C                   UPDATE    TL42S01
085800960215     C                   GOTO      ECTS01
085900960215    3C                   END
086000960215    2C                   END
086100960213     C                   ADD       1             II
086200960215    1C                   ENDDO
086300960215     C     ECTS01        ENDSR
086400960215     C***** CARICAM. VIDEATA PARAMETRI PER CREAZIONE TRAINO ECCEZ. ****
086500960215     C     RIED02        BEGSR
086600960215     C* CHAINO RECORD SUBFILE DEL 1° FV DEL TRAINO PER REPERIRE DATI
086700960215     C* DA RIPORTARE IN SECONDA VIDEATA
086800960215     C                   SETON                                        22
086900010827     C     NRR(X)        CHAIN     TL42S01                            30
087000010827     C                   UPDATE    TL42S01
087100960215     C                   MOVE      V1SLNP        V2CLNP
087200960215     C                   MOVE      V1SNFV        V2CNFV
087300960220     C                   MOVEL     V1HLNP        V2DLNP
087400960215     C                   MOVEL     V1SBIS        V2CBIS
087500960215     C                   MOVEL     V1SDFV        V2CDFV
087600960215     C                   MOVE      V1SLNA        V2CLNA
087700960215     C                   MOVEL     V1DLNA        V2DLNA
087800960215     C* NUMERO TRAINO
087900960220     C     *LOCK         IN        TNTL02                               30
088000960220     C* Se data area allocata torno in prima videata con msg
088100960220     C     *IN30         IFEQ      *ON
088200960220     C                   SETON                                        28
088300960220     C                   MOVEL     MSG(18)       V1ZMSG
088400960220     C                   GOTO      ERIED2
088500960220     C                   END
088600960215     C     TNTL02        ADD       1             TNTL02
088700960215     C                   OUT       TNTL02
088800960215     C                   Z-ADD     TNTL02        V2CTRN
088900960215     C* DATA TRAINO = DATA DEL 1° FV
089000960215     C                   MOVE      V1SDFV        V2CDTN
089100010827
089200960422     C* PRENDO IMPORTO E ALTRI DATI DAL PRIMO FV SCELTO SE IMPORTO
089300960422     C* DIVERSO DA 0 O SE TUTTI IMPORTI = 0  ALTRIMENTI PRENDO IMPORTO
089400960422     C* E ALTRI DATI DAL PRIMO DIVERSO DA 0
089500960422     C     WIPT1         IFGT      *ZEROS
089600960422     C     WIPT          OREQ      *ZEROS
089700960228     C                   MOVE      WPDR1         WPDR
089800960228     C                   MOVE      WDPD1         WDPD
089900960228     C                   MOVE      WTMZ1         WTMZ
090000960319     C                   MOVE      WFES1         WFES
090100010828     C**!!!              MOVE      WIPT1         WIPT
090200010828     C                   z-add     WIPT1         WIPT
090300010828     c                   move      wdiv1         wdiv
090400960228     C                   END
090500960312     C**** TRAZIONISTA
090600960216     C     WPDR          IFGT      *ZEROS
090700960216     C                   MOVE      WPDR          V2CPDR
090800960215     C                   ELSE
090900960215     C                   MOVE      '      ?'     V2CPDR
091000960215     C                   END
091100960216     C                   MOVEL     WDPD          V2DDPD
091200010828      * divisa
091300010828     c                   move      wdiv          v2cdiv
091400011001     c                   move      wdiv          sav_v2cdiv
091500960312     C**** IMPORTO DA CONFERMARE
091600010828     C**!!!              MOVE      WIPT          V2CICO
091700010828     C                   z-add     WIPT          V2CICO
091800960215     C**** CAUSALE CONTABILIZZAZIONE  (PRENDO DA TABELLA TW)
091900960215     C                   MOVE      'TW'          KCOD
092000960215     C                   MOVEL(P)  1             KKEY
092100960215     C     KTAB          CHAIN     TABEL00F                           30
092200960215    1C     *IN30         IFEQ      *OFF
092300960215     C                   MOVEL     TBLUNI        V2CCAU
092400960215     C                   MOVEL     TBLUNI        WCAU
092500000211     C* Reperisco i dati della causale di contabilizzazione
092600000218     C* dalle tabelle di PROJ
092700000218     C                   MOVE      KNSIF         WCOMO4            4
092800000218     C                   MOVEL     WCOMO4        WSOC              3
092900000218     C                   movel     WSOC          KAtazi
093000000218     C                   MOVEL     'Y4Z'         Katcod
093100000218     C                   MOVEL     *blanks       Katkey
093200000218     C                   MOVEL(P)  V2CCAU        Katkey
093300000218     C     KEATAB        CHAIN     ANATB01L                           30
093400000218     C                   MOVE      '888'         Katazi
093500000218     C     KEATAB        CHAIN     ANATB01L                           30
093600000218     C                   if        not *in30
093700000218     C                   MOVEL     ATBUNI        V2DCAU
093800000211     C                   else
093900000211     C                   MOVE      *BLANKS       V2DCAU
094000000211     C                   end
094100960215     C                   MOVEL     V2DCAU        WDCAU
094200960215   X1C                   ELSE
094300960215     C                   CLEAR                   V2CCAU
094400960215     C                   CLEAR                   WCAU
094500960215     C                   CLEAR                   V2DCAU
094600960215     C                   CLEAR                   WDCAU
094700960215    1C                   END
094800960220     C     ERIED2        ENDSR
094900010827     C**********   CONTROLLI VIDEATA TL42D02   ************************
095000960215     C     CTRD02        BEGSR
095100960215     C                   SETOFF                                       404142
095200010828     c                   setoff                                       45
095300960215     C****  CODICE TRAZIONISTA
095400960215     C     '?'           SCAN      V2CPDR                                 30
095500960215    1C     *IN30         IFEQ      *ON
095600000807     C                   EVAL      PA3TIP = 'T '                                PAR-1
095700000807     C                   MOVEL(P)  V2DDPD        PA3RSC                         PAR-2
095800000807     C                   CLEAR                   PA3CSF                         PAR-3
095900000807     C                   CLEAR                   PA3PDR                         PAR-4
096000000807     C                   CLEAR                   PA3FLG                         PAR-5
096100960215     C                   MOVEL     PARAM3        KPJBU
096200960215     C                   CALL      'TRUL16R'
096300960215     C                   PARM                    KPJBA
096400960215     C                   MOVEL     KPJBU         PARAM3
096500000807     C                   MOVEL     PA3PDR        V2CPDR                         CODICE
096600000807     C                   MOVEL     PA3RSC        V2DDPD                         RAG.SOCIALE
096700960215     C* PA3FLG = "3"-->NON TROVATI TRAZIONISTI PER LA CHIAVE ALFABETICA
096800960215     C*                RICHIESTA
096900960215    2C     PA3FLG        IFEQ      '3'
097000960215     C                   MOVEL     MSG(11)       V2ZMSG
097100960215     C                   SETON                                        2840
097200960215     C                   GOTO      ECTR02
097300960215    2C                   ENDIF
097400960215    1C                   END
097500960215     C* TRAZIONISTA = BLANKS ---> RIPROPONGO QUELLO DEL FV
097600960215    1C     V2CPDR        IFEQ      *ZEROS
097700960215     C     V2CPDR        OREQ      *BLANKS
097800960215     C     V2DDPD        ANDEQ     *BLANKS
097900960216     C                   MOVE      WPDR          V2CPDR
098000960216     C                   MOVE      WDPD          V2DDPD
098100960215    1C                   END
098200960215     C* CODICE TRAZIONISTA OBBLIGATORIO
098300960215    1C     V2CPDR        IFEQ      *ZEROS
098400960215     C     V2CPDR        OREQ      *BLANKS
098500960215     C                   SETON                                        2840
098600960215     C                   MOVEL     MSG(12)       V2ZMSG
098700960215     C                   GOTO      ECTR02
098800960215    1C                   END
098900960215     C* CONTROLLO VALIDITA' COD.TRAZIONISTA
099000960215     C                   TESTN                   V2CPDR               30
099100960215    1C     *IN30         IFEQ      *ON
099200960215     C                   MOVE      V2CPDR        W001A             1
099300960215    2C     W001A         IFLT      '0'
099400960215     C                   SETOFF                                       30
099500960215    2C                   END
099600960215    1C                   END
099700960215    1C     *IN30         IFEQ      *OFF
099800960215     C                   MOVEL     MSG(13)       V2ZMSG                                     RORE
099900960215     C                   SETON                                        2840
100000960215     C                   CLEAR                   V2DDPD
100100960215     C                   GOTO      ECTR02
100200960215    1C                   END
100300960215     C*
100400960215     C                   MOVE      V2CPDR        KPDR
100500000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
100600960215     C*
100700960215    1C     *IN31         IFEQ      *ON
100800960215     C     TLZATB        ORNE      ' '
100900960215     C                   MOVEL     MSG(13)       V2ZMSG
101000960215     C                   CLEAR                   V2DDPD
101100960215     C                   SETON                                        2840
101200960215     C                   GOTO      ECTR02
101300960215   X1C                   ELSE
101400960215     C                   MOVEL     TLZRSC        V2DDPD
101500960215    1C                   ENDIF
101600010827
101700960215     C**** IMPORTO DA CONFERMARE
101800960215     C* IMPORTO = 0 --> RIPROPONGO QUELLO DEL FV
101900011012     C                   if        v2cico = 0
102000011012     c                             and v2cdiv =*blanks
102100011012     c                             and flgged = *off
102200010828     C**!!!              MOVE      WIPT          V2CICO
102300010828     C                   z-add     WIPT          V2CICO
102400011012     c                   move      wdiv          v2cdiv
102500960215     C                   END
102600960215    1C     V2CICO        IFEQ      *ZEROS
102700960215     C                   SETON                                        2841
102800960215     C                   MOVEL     MSG(14)       V2ZMSG
102900960215     C                   GOTO      ECTR02
103000960215    1C                   END
103100011012
103200011012      * d i v i s a
103300011012      * divisa = ' '---> RIPROPONGO QUELLA DEL FV
103400011012     c                   if        v2cdiv =*blanks
103500011012     c                             and flgged = *off and v2cico = 0
103600011012     c                   move      wdiv          v2cdiv
103700011012     c                   endif
103800011012      * obbligatoria
103900011012     c                   if        v2cdiv = *blanks
104000011012     c                   seton                                        2845
104100011012     c                   movel     msg(20)       v2zmsg
104200011012     c                   goto      ectr02
104300011012     c                   endif
104400011012      * deve essere tabellata
104500011012     c                   clear                   dscv
104600011012     c                   movel     'CV'          kcod
104700011012     c                   movel(p)  v2cdiv        kkey
104800011012     c     ktab          chain     tabel00f
104900011012     c                   if        not%found(tabel00f)
105000011012     c                   seton                                        2845
105100011012     c                   movel     msg(21)       V2ZMSG
105200011012     c                   goto      ectr02
105300011012     c                   endif
105400011012      * deve essere ammessa nei traini
105500011012     c                   movel     tbluni        dscv
105600011012     c                   if        §cvtra <> 'S'
105700011012     c                   seton                                        2845
105800011012     c                   movel     msg(22)       V2ZMSG
105900011012     c                   goto      ectr02
106000011012     c                   endif
106100011012      * se modificata la divisa e non l'importo devo dare msg di errore
106200011012     c                   if        v2cdiv <> sav_v2cdiv
106300011012     c                             and *in66 = *off
106400011012     c                             and v2cico <> *zeros
106500011012     c                             and sav_v2cdiv <> *blanks
106600011012     c                   movel     msg(24)       v2zmsg
106700011012     c                   seton                                        2841
106800011012     c                   goto      ectr02
106900011012     c                   endif
107000011012
107100011012     c                   movel     v2cdiv        sav_v2cdiv
107200010828      * controllo se ok i decimali
107300010828     c                   move      v2cico        w_decimali
107400010828     c                   if        w_decimali > 0  and
107500010828     c                             §cvfdc <> 'S'
107600010828     c                   seton                                        2841
107700010828     c                   movel     msg(23)       v2zmsg
107800010828     c                   goto      ectr02
107900010828     c                   endif
108000010827
108100960215     C**** CAUSALE CONTABILIZZAZIONE
108200000218    1C***  V2CCAU        IFEQ      '?'
108300000218     C***                RESET                   XTABDS
108400000218     C***                MOVE      *BLANK        XTAKEY
108500000218     C***                MOVE      '01'          XTAOPZ
108600000218     C***                MOVE      xscsoc        XTAAZI
108700000218     C***                MOVE      XSCLIN        XTALIN
108800000218     C***                CLEAR                   KPJBU
108900000218     C***                MOVEL     XTABDS        KPJBU
109000000218     C***                CALL      'ANGY4ZR'
109100000218     C***                PARM                    KPJBA
109200000218     C***                MOVEL     KPJBU         XTABDS
109300000218     C***                MOVEL     XTAKEY        V2CCAU                         *CODICE
109400000218    1C***                END
109500960220    1C     V2CCAU        IFEQ      *BLANKS
109600960215     C                   MOVEL     WCAU          V2CCAU
109700960215     C                   MOVEL     WDCAU         V2DCAU
109800960220    1C                   END
109900960220    1C     V2CCAU        IFEQ      *BLANKS
110000960215     C                   SETON                                        2842
110100960215     C                   MOVEL     MSG(15)       V2ZMSG
110200960215     C                   GOTO      ECTR02
110300960220    1C                   END
110400000211     C* Reperisco i dati della causale di contabilizzazione
110500000218     C                   MOVE      KNSIF         WCOMO4            4
110600000218     C                   MOVEL     WCOMO4        WSOC              3
110700000218     C                   movel     WSOC          KAtazi
110800000218     C                   MOVEL     'Y4Z'         Katcod
110900000218     C                   MOVEL     *blanks       Katkey
111000000218     C                   MOVEL(P)  V2CCAU        Katkey
111100000218     C     KEATAB        CHAIN     ANATB01L                           30
111200000218     C                   MOVE      '888'         Katazi
111300000218     C     KEATAB        CHAIN     ANATB01L                           30
111400000218     C                   if        not *in30
111500000218     C                   MOVEL     ATBUNI        V2DCAU
111600000211     C                   else
111700000211     C                   MOVE      *BLANKS       V2DCAU
111800000211     C                   MOVEL     MSG(17)       V2ZMSG
111900000211     C                   SETON                                        2842
112000960220    1C                   END
112100011001
112200011001     c     ectr02        tag
112300011001
112400011001     C                   ENDSR
112500960215     C**************  CONFERMA FV NON ABBINATI  ***********************
112600960215     C     CONFE         BEGSR
112700960219     C*
112800960219     C* DETERMINO GIORNI INIZIO SECOLO DI DATA TRAINO
112900960219     C                   CLEAR                   WLBDA8
113000960219     C                   MOVE      WDTN          G08INV
113100960219     C                   MOVE      '3'           G08ERR
113200960219     C                   CALL      'XSRDA8'
113300960219     C                   PARM                    WLBDA8
113400960219     C                   MOVE      G08TGI        WTGI
113500960219     C* DETERMINO GIORNO DELLA SETTIMANA DI DATA TRAINO
113600960220     C                   MOVEL     G08DAT        WDAT6             6
113700960220     C                   MOVE      G08DAT        W002A             2
113800960220     C                   MOVE      W002A         WDAT6
113900960219     C                   CALL      'XGIOSE1'
114000960219     C                   PARM                    WDAT6
114100960219     C                   PARM                    GIOSET            1
114200960216     C* WFIRST è IL PRIMO ELEMENTO DIVERSO DA BLANKS IN SCHIERA
114300960216     C* ORDINATA DELLE SCELTE
114400960216     C                   MOVE      WFIRST        X
114500960216     C                   CLEAR                   CAR
114600960219     C                   CLEAR                   SNF
114700960223     C                   CLEAR                   WMSG
114800960216     C                   CLEAR                   PAR
114900960216     C                   CLEAR                   SCA
115000960216     C                   CLEAR                   ARR
115100960216     C                   MOVE      *ZEROS        Y                 3 0          INDICE CAR
115200960216     C                   MOVE      *ZEROS        WW                3 0          INDICE SCA
115300960216     C* CICLO LETTURA FV SCELTI IN BASE ALL'ORDINE RICHIESTO
115400960216    1C     X             DOWLE     100
115500960215     C                   Z-ADD     1             II
115600960215     C     SSC(X)        LOOKUP    SCE(II)                                30
115700960216     C* Memorizzo lnp del FV in schiera filiali di carico
115800960216     C                   ADD       1             Y
115900960216     C                   MOVE      LNP(II)       CAR(Y)
116000960216     C* CICLO LETTURA FILIALI DEL FV
116100960216     C     II            OCCUR     DSFIL
116200960216     C                   MOVEL     DDRP          PAR(Y)
116300960216     C                   MOVE      DHRP          PAR(Y)
116400960219     C                   MOVE      NFV           SNF(Y)
116500960216     C                   CLEAR                   WFSC
116600960216     C                   CLEAR                   WTARS
116700960216     C                   Z-ADD     1             Z                 3 0
116800960220     C                   MOVE      *HIVAL        W0120            12 0
116900960220     C                   MOVE      8             W0120
117000960325    2C     Z             DOWLE     150
117100960216     C     FSC(Z)        ANDNE     *BLANKS
117200960216     C* Routine di ricerca terminal di arrivo della filiale del FV
117300960216     C                   EXSR      SRTAR
117400960216     C* Gestione rottura terminal di arrivo
117500960216    3C     WTAR          IFNE      WTARS
117600960322     C     WTARS         IFGT      *ZEROS
117700960216     C* SCRITTURA METRI CUBI
117800960322     C                   MOVE      WFSC1         WTFA
117900960216     C                   EXSR      SRTLM
118000960322     C                   END
118100960322     C                   MOVE      FSC(Z)        WFSC1             3
118200960322     C                   MOVE      WTAR          WTARS
118300960322     C                   CLEAR                   WMCR
118400960216    3C                   END
118500960322     C                   MOVE      LNP(II)       WLNP
118600960322     C                   MOVE      FVG(Z)        WFVG
118700960322     C     KFVD          CHAIN     FNFVD01L                           30
118800960322     C     *IN30         IFEQ      *OFF
118900960322     C                   XFOOT     FVD           W0104            10 4
119000960322     C                   ADD       W0104         WMCR             10 4
119100960322     C                   END
119200960216     C* Gestione rottura filiale di scarico
119300960219    3C     FSC(Z)        IFNE      WFSC
119400960219     C                   MOVE      FSC(Z)        WFSC              3
119500960216     C                   Z-ADD     1             W                 3 0
119600960216     C* Se non ancora memorizzata la memorizzo adesso
119700960216     C     FSC(Z)        LOOKUP    SCA(W)                                 30
119800960216    4C     *IN30         IFEQ      *OFF
119900960216     C                   ADD       1             WW
120000960216     C                   MOVE      FSC(Z)        SCA(WW)
120100960220    4C                   END
120200960307    4C*****      *IN30     IFEQ *OFF
120300960307     C*****      ARR,W     OREQ W0120
120400960307     C* Memorizzo anche data/ora di arrivo
120500960307     C* (In caso di scelta di più di un fv memorizzo la data/ora arrivo
120600960307     C* più alta se ce n'è più di una)
120700960220     C                   MOVE      FSC(Z)        WSCA
120800960219     C                   MOVE      LNP(II)       WLNP
120900960220     C                   EXSR      SRARR
121000960307    4C*****                END
121100960216    3C                   END
121200960216     C                   ADD       1             Z
121300960216    2C                   ENDDO
121400960325     C*
121500960325     C     WTARS         IFGT      *ZEROS
121600960325     C                   MOVE      WFSC1         WTFA
121700960322     C                   EXSR      SRTLM
121800960322     C                   CLEAR                   WMCR
121900960325     C                   END
122000960325     C*
122100960215     C                   ADD       1             X
122200960216    1C                   ENDDO
122300960216     C* FINE CICLO LETTURA FOGLI VIAGGIO: SCRIVO FILE FERMATE (TNTLR)
122400960216     C                   EXSR      SRTLR
122500960219     C* SCRITTURA LISTINO
122600010828     C                   CLEAR                   TITLL000
122700960219     C                   MOVE      V2CTRN        TLLTRN
122800960219     C                   MOVE      WDTN          TLLDDE
122900960219     C                   MOVE      V2CPDR        TLLPDR
123000960219     C                   MOVE      WTMZ          TLLTMZ
123100010828     C**!!!              MOVE      V2CICO        TLLILI
123200010827     C**!!!              MOVE      V2CICO        TLLIES
123300010828     C                   z-add     V2CICO        TLLILI
123400010827     c                   z-add     *zeros        tllies
123500960219     C                   MOVEL     V2CCAU        TLLCAU
123600960219     C                   MOVE      WDATEU        TLLDIR
123700010827     c                   move      v2cdiv        tlldiv
123800010828     C                   WRITE     TITLL000
123900960216     C* SCRITTURA TESTATA TRAINO
124000960216     C                   CLEAR                   TNTLT000
124100960219     C                   MOVE      V2CTRN        TLTTRN
124200960219     C                   MOVE      WDTN          TLTDDE
124300960219     C                   MOVE      WDTN          TLTDSC
124400960219     C                   MOVE      CAR(1)        TLTTFP
124500960220     C                   MOVE      DLNA          TLTTFA
124600960219     C                   MOVE      'S'           TLTTTR
124700960220     C                   MOVE      GIOSET        G                 1 0
124800960220     C                   MOVE      GIOSET        GSE(G)
124900960424     C                   MOVEL     WBIS1         TLTBIS
125000960219     C                   MOVEL     WTMZ          TLTTMZ
125100960319     C                   MOVEL     WFES          TLTFES
125200960219     C                   MOVE      WDATEU        TLTDIR
125300960219     C                   WRITE     TNTLT000
125400960219     C* RICHIAMO PGM DI GESTIONE ANAGRAFICA TRAINI PER PERMETTERE
125500960219     C* DI COMPLETARE E/O CORREGGERE DATI MANCANTI E/O ERRATI
125600960219     C* (MANCANZA DI DATE DI ARRIVO, FERMATE DI SOLO SCARICO NON IN
125700960219     C*  ORDINE....)
125800960307     C                   MOVEL     'C02'         D00OP0                         *PARAMETRI FISSI
125900960219     C                   MOVEL     'O02'         D00OP1
126000960219     C                   MOVEL     '0'           D00F03
126100960219     C                   MOVEL     '0'           D00F12
126200960219     C                   MOVEL     '0'           D00ERR
126300960219     C                   MOVEL     *BLANKS       D00MSG
126400960219     C                   Z-ADD     V2CTRN        D00TRN                         *PARAMETRI SELEZ.
126500960219     C                   Z-ADD     WDTN          D00DDE
126600960219     C                   MOVEL     DS00          KPJBU
126700960219     C                   CALL      'TNTL01C'
126800960219     C                   PARM                    KPJBA
126900960219     C                   MOVEL     KPJBU         DS00
127000960219     C* SE PREMUTO F12 O F3 IL TRAINO NON è STATO CONFERMATO CANCELLO
127100960219     C* QUINDI DATI DA ANAGRAFICA
127200960219    1C     D00F03        IFEQ      '1'
127300960219     C     D00F12        OREQ      '1'
127400960219     C                   EXSR      DELTRN
127500960219   X1C                   ELSE
127600960219     C* CREO GRIGLIA TRAINO
127700960219     C                   CLEAR                   DSTL24
127800960219     C                   MOVE      WDTN          D24DTD
127900960219     C                   MOVE      WDTN          D24DTA
128000960219     C                   MOVE      V2CTRN        D24TRN
128100960315     C***                  MOVE CAR,1     D24TFP
128200960219     C                   MOVE      WDTN          D24DDE
128300960219     C                   MOVEL     DSTL24        KPJBU
128400960219     C                   CALL      'TNTL24R'
128500960219     C                   PARM                    KPJBA
128600960219     C* CICLO LETTURA FOGLI VIAGGIO PER ABBINAMENTO
128700960219     C                   Z-ADD     1             W
128800960219    2C     W             DOWLE     100
128900960219     C     SNF(W)        ANDGT     *ZEROS
129000960219     C                   MOVE      CAR(W)        KLNP
129100960219     C                   MOVE      SNF(W)        KNFV
129200960219     C     KFVS1         CHAIN     TNFVS01L                           30
129300960219    3C     *IN30         IFEQ      *OFF
129400010828
129500010828      * aggancio il file delle estensioni foglio viaggio per gli importi in
129600010828      * divisa
129700010910     c                   clear                   fwsipt
129800010910     c                   clear                   fwsies
129900010910     c                   clear                   fwsvip
130000010910     c                   clear                   fwsves
130100010828     c     kfws          chain     tnfws01l
130200010907      * se l'importo dell'estensione è = 0 e l'importo sul file
130300010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
130400010907      * la divisa è lire
130500010907     c                   if        fwsipt = *zeros and fvsipt <> 0
130600010907     c                   eval      fwsvip = 'ITL'
130700010907     c                   z-add     fvsipt        fwsipt
130800010907     c                   endif
130900010907     c                   if        fwsies = *zeros and fvsies <> 0
131000010907     c                   eval      fwsves = 'ITL'
131100010907     c                   z-add     fvsies        fwsies
131200010907     c                   endif
131300010907      * se l'importo dell'estensione è = 0 e l'importo sul file
131400010907      * principale è = 0, imposto la divisa a blank per poi prendere quella che
131500010907      * mi serve caso per caso
131600010907     c                   if        fwsipt = *zeros and fvsipt = 0
131700010907     c                   z-add     fvsipt        fwsipt
131800010907     c                   clear                   fwsvip
131900010907     c                   endif
132000010907     c                   if        fwsies = *zeros and fvsies = 0
132100010907     c                   z-add     fvsies        fwsies
132200010907     c                   clear                   fwsves
132300010907     c                   endif
132400010828
132500960308     C* Controlli di abbinabilità:
132600960308     C* ........Posso abbinare il fv solo se non risulta già abbinato
132700960219    4C     FVSTRS        IFEQ      *ZEROS
132800960308     C* ........richiamo routine che effettua ulteriori controlli di
132900960308     C*         abbinabilità
133000960308     C                   EXSR      CHKABB
133100960308    5C     WOKABB        IFEQ      '1'
133200960219     C                   EXSR      AGGSTO
133300960223     C                   CLEAR                   SNF(W)
133400960223     C                   CLEAR                   CAR(W)
133500960219     C                   MOVE      V2CTRN        FVSTRS
133600960228     C                   MOVE      V2CTRN        FVSTRN
133700960305     C                   MOVE      'A'           FVSTFM
133800960305     C                   MOVE      'A'           FVSTFS
133900960219     C                   UPDATE    TNFVS000
134000960322     C* Richiamo pgm che appoggia metri cubi reali
134100960322     C                   CLEAR                   DSTL33
134200960322     C                   MOVE      'L'           D33TLA
134300960322     C                   MOVE      FVSLNP        D33LNP
134400960322     C                   MOVE      FVSNFV        D33NFV
134500960322     C                   MOVE      TSRDTN        D33DTN
134600960322     C                   MOVEL     DSTL33        KPJBU
134700960322     C                   CALL      'TNTL33R'
134800960322     C                   PARM                    KPJBA
134900960219    5C                   END
135000960219    4C                   END
135100960219    3C                   END
135200960219     C                   ADD       1             W
135300960219    2C                   ENDDO
135400960319     C* VERIFICO SE POSSO IMPOSTARE FLAG DI CONFERMA IMPORTO DA MANUALE
135500960319     C* AD AUTOMATICO
135600960319     C     KANA          CHAIN     TNTST01L                           30
135700960319    2C     *IN30         IFEQ      *OFF
135800010828      * calcolo il delta e controllo la tolleranza
135900010828     c                   eval      sav_tstili = tstili
136000010828     c                   if        tstipt <> tstili and
136100010828     c                             tstili > 0
136200010828      * aggancio la tabella GEI per la tolleranza
136300010828     c                   clear                   dgei
136400010828     c                   clear                   tibs02ds
136500010828     c                   movel     'L'           t02tla
136600010828     c                   movel     'C'           t02mod
136700010828     c                   movel     knsif         t02sif
136800010828     c                   movel     'GEI'         t02cod
136900010828     c                   movel     tstdiv        t02ke1
137000010828     c                   call      'TIBS02R'
137100010828     c                   parm                    kpjba
137200010828     c                   parm                    tibs02ds
137300010828     c                   movel     t02uni        dgei
137400010828
137500010828     c                   eval      w_delta = (tstipt - tstili)
137600010910      * il delta deve essere come valore assoluto
137700010910     c                   mllzo     1             w_delta
137800010828     c                   if        w_delta <= §gettr
137900010828     c                   eval      sav_tstili = tstipt
138000010828     c                   endif
138100010828     c                   endif
138200010828
138300010828     C**!!!TSTILI        ANDEQ     TSTIPT
138400010828   2aC     sav_tstili    ifeq      TSTIPT
138500960319     C     TSTILI        ANDGT     *ZEROS
138600960319     C     TSTPDR        ANDGT     *ZEROS
138700960319     C     TSTCAU        ANDNE     *BLANKS
138800960319     C     KTSR4         SETLL     TNTSR04L                               30
138900960319    3C     *IN30         IFEQ      *OFF
139000960319     C                   MOVE      '1'           TSTCNF
139100960319     C                   EXCEPT    AGGTST
139200960319   X3C                   ELSE
139300960319     C                   UNLOCK    TNTST01L
139400960319    3C                   END
139500960319   X2C                   ELSE
139600960319     C                   UNLOCK    TNTST01L
139700010828   2aC                   END
139800960319    2C                   END
139900960219     C* RICHIAMO PGM CHE APPOGGIA DATE/ORE ARRIVO SU GRIGLIA
140000960219     C                   CALL      'TNTL26R'
140100960219     C                   PARM                    KPJBA
140200960315     C* IMPOSTO ULTIMO TRAINO CONFERMATO DEL SUBFILE CONTROL
140300960315     C                   MOVE      V2CTRN        V1CTRN
140400960315     C                   MOVE      V2CDTN        V1CDTN
140500960219     C* RICARICO IL SUBFILE DI SCELTA FOGLI VIAGGIO
140600960219     C                   EXSR      RIEINI
140700960219    1C                   END
140800960215     C*
140900960215     C                   ENDSR
141000960308     C**********  CONTROLLO ABBINABILITA DEL FV AL TRAINO   ***********
141100960308     C     CHKABB        BEGSR
141200960308     C                   SETOFF                                       31
141300960308     C                   MOVE      ' '           WOKABB            1
141400960308     C     KTSR3         CHAIN     TNTSR03L                           30
141500960308     C* IL FOGLIO VIAGGIO E' ABBINABILE SOLO SE:
141600960308     C*                  1-ESISTE TRATTA CON FILIALE FERMATA PARTENZA
141700960308     C*                    = LNP DEL FV
141800960308    5C     *IN30         DOWEQ     *OFF
141900960308     C     *IN31         ANDEQ     *OFF
142000960308     C                   MOVEL     TSRFFA        W003A             3
142100960308     C     W003A         LOOKUP    FLP                                    31
142200960308     C  N31FVSLNA        COMP      TSRFFA                                 31
142300960308     C  N31KTSR3         READE     TNTSR03L                               30
142400960308    5C                   ENDDO
142500960308     C*                  2-ALMENO UN FILIALE DI SCARICO DEL FV è = ALLA
142600960308     C*                    FILIALE FERMATA DI ARRIVO DELLA TRATTA
142700960308     C   31              MOVE      '1'           WOKABB
142800960308     C                   ENDSR
142900960220     C**********  RICERCA E MEMORIZZAZIONE DATA/ORA ARRIVO  ***********
143000960220     C     SRARR         BEGSR
143100960220     C     KFV2          CHAIN     FNFV201L                           31
143200960307     C* TROVATA DATA/ORA DI ARRIVO
143300960307    1C     *IN31         IFEQ      *OFF
143400960307     C* 30 on --> Avevo già memorizzato una data/ora di arrivo:
143500960307     C*           mantengo la più alta
143600960307    2C     *IN30         IFEQ      *ON
143700960307    3C     DATORA        IFGT      ARR(W)
143800960307     C     ARR(W)        OREQ      W0120
143900960307     C                   Z-ADD     DATORA        ARR(W)
144000960307    3C                   END
144100960307   X2C                   ELSE
144200960307     C* 30 off --> memorizzo la data/ora arrivo letta
144300960307     C                   Z-ADD     DATORA        ARR(WW)
144400960307    2C                   END
144500960307   X1C                   ELSE
144600960307     C* NON TROVATA DATA/ORA DI ARRIVO
144700960220     C* Se data/ora di arrivo non trovata la imposto=999999999998
144800960220     C* in modo che dopo il sorta della schiera sia l'ultima
144900960307     C  N30              MOVE      W0120         ARR(WW)
145000960307    1C                   END
145100960220     C                   ENDSR
145200960216     C***************  DETERMINA TERMINAL DI ARRIVO  ******************
145300960216     C     SRTAR         BEGSR
145400960216     C                   CLEAR                   WTAR
145500960822     C* SE FIL.FV=FIL.DI SCARICO --> TER-ARR=FIL.FV
145600960822   1AC     FVG(Z)        IFEQ      FSC(Z)
145700960822     C                   MOVE      FVG(Z)        WTAR
145800960822  X1AC                   ELSE
145900960216     C* CERCO IL TERMINAL DI ARRIVO DELLA FILIALE DEL FOGLIO VIAGGIO
146000960216     C* 1-GUARDO PRIMA LE ECCEZIONI
146100971118     C                   CLEAR                   DSLV55
146200971118     C                   MOVEL     'A'           D55TPT
146300971118     C                   MOVE      LNP(II)       D55LNP
146400971118     C                   MOVE      FVG(Z)        D55LIN
146500971118     C                   Z-ADD     WDTN          D55DRF
146600971118     C                   CALL      'FNLV55R'
146700971118     C                   PARM                    DSLV55
146800971118     C     D55ERR        IFNE      ' '
146900971118     C                   MOVEL     D55LIN        D55TFA
147000971118     C                   ENDIF
147100971118     C                   MOVE      D55TFA        WTAR
147200971118     C                   ENDIF
147300960216     C                   ENDSR
147400960219     C*************  CONTROLLO SE GIORNO FESTIVO  *********************
147500960219     C     CTRFES        BEGSR
147600960220     C                   MOVEL     W0080         KANN
147700960220     C                   MOVE      W0080         W0040             4 0
147800960220     C                   MOVEL     W0040         KMES
147900960220     C                   MOVE      W0040         GG                2 0
148000960319     C                   CLEAR                   WFEST
148100960219     C     KCLN          CHAIN     AZCLN01L                           30
148200960219    1C     *IN30         IFEQ      *OFF
148300960219     C     POM(GG)       ANDNE     *BLANKS
148400960219     C                   Z-ADD     1             XY                2 0
148500960219     C     POM(GG)       LOOKUP    S1Y(XY)                                30
148600960319     C   30              MOVE      FET(XY)       WFEST             1
148700960219    1C                   END
148800960219     C                   ENDSR
148900960216     C*******************  SCRITTURA METRI CUBI  **********************
149000960216     C     SRTLM         BEGSR
149100960219     C                   MOVE      LNP(II)       WLNP
149200960322     C     KTLM          CHAIN     TNTLM01L                           30
149300960322     C     *IN30         IFEQ      *ON
149400960216     C                   CLEAR                   TNTLM000
149500960216     C                   MOVE      V2CTRN        TLMTRN
149600960216     C                   MOVE      WDTN          TLMDDE
149700960216     C                   MOVE      LNP(II)       TLMTFP
149800960220     C                   MOVE      WTFA          TLMTFA                         FIL.SCARICO
149900960322     C                   MOVE      WTARS         TLMTFF                         FIL.FINALE
150000960424     C     WBIS1         IFEQ      *BLANKS
150100960322     C                   Z-ADD(H)  WMCR          TLMMCA
150200960424     C                   ELSE
150300960424     C                   Z-ADD(H)  WMCR          TLMMCB
150400960424     C                   END
150500960216     C                   Z-ADD     WDATEU        TLMDIR
150600960216     C                   WRITE     TNTLM000
150700960322     C                   ELSE
150800960424     C     WBIS1         IFEQ      *BLANKS
150900960322     C                   ADD(H)    WMCR          TLMMCA
151000960424     C                   ELSE
151100960424     C                   ADD(H)    WMCR          TLMMCB
151200960424     C                   END
151300960322     C                   UPDATE    TNTLM000
151400960216     C                   END
151500960216     C                   ENDSR
151600960216     C*******************  SCRITTURA FERMATE  *************************
151700960216     C     SRTLR         BEGSR
151800960219     C*
151900960219     C* CALCOLO DATA TRAINO+1
152000960219     C                   CLEAR                   WGIDAT
152100960219     C     WTGI          ADD       1             GI8TGI
152200960219     C                   CALL      'XSRGI8'
152300960219     C                   PARM                    WGIDAT
152400960219     C                   MOVE      GI8INV        WDAT
152500960219     C*
152600960220     C                   Z-ADD     1             WNFM
152700960216     C* SCRIVO LA PRIMA FERMATA DEL TRAINO
152800960216     C                   CLEAR                   TNTLR000
152900960216     C                   MOVE      V2CTRN        TLRTRN
153000960216     C                   MOVE      WDTN          TLRDDE
153100960216     C                   MOVE      WNFM          TLRNFM
153200960216     C                   MOVEL     'A'           TLRTFM
153300960216     C                   MOVE      CAR(1)        TLRFIL
153400960216     C                   MOVEL     'S'           TLRCAR
153500960216     C                   MOVEL     'A'           TLRGFV
153600960216     C                   MOVE      WDATEU        TLRDIR
153700960216     C                   WRITE     TNTLR000
153800960219     C* Scrivo relativo record su FILE ORARI
153900960219     C                   CLEAR                   TNTLO000
154000960219     C                   MOVE      V2CTRN        TLOTRN
154100960219     C                   MOVE      WDTN          TLODDE
154200960219     C                   MOVE      WNFM          TLONFM
154300960219     C* DETERMINO GIORNO DI PARTENZA
154400960219     C                   MOVEL     PAR(1)        WDATAA
154500960219     C     WDTN          IFEQ      WDATAA
154600960219     C                   MOVE      'A'           TLOGGP
154700960219     C                   ELSE
154800960219     C                   EXSR      SRGIO
154900960219     C                   Z-ADD     1             XX
155000960219     C     YY            LOOKUP    NUM(XX)                                30
155100960219     C   30              MOVE      ALF(XX)       TLOGGP
155200960220     C  N30              MOVE      *BLANKS       TLOGGP
155300960219     C                   END
155400960219     C                   MOVE      PAR(1)        TLOHHP
155500960219     C                   MOVE      WDATEU        TLODIR
155600960219     C                   WRITE     TNTLO000
155700960219     C*
155800960219     C                   MOVE      *HIVAL        WARR             12 0
155900960219     C                   MOVE      '8'           WARR
156000960219     C* CICLO LETTURA DELLE ALTRE FERMATE DI CARICO
156100960216     C                   Z-ADD     2             C                 3 0
156200960219     C     C             DOWLE     100
156300960219     C     CAR(C)        ANDNE     *BLANKS
156400960216     C                   Z-ADD     1             W                 3 0
156500960219     C     CAR(C)        LOOKUP    SCA(W)                                 32
156600960220     C                   ADD       1             WNFM
156700960216     C                   Z-ADD     WNFM          TLRNFM
156800960216     C                   MOVE      CAR(C)        TLRFIL
156900960219     C   32              MOVE      'S'           TLRSCA
157000960219     C  N32              MOVE      ' '           TLRSCA
157100960216     C                   MOVE      'S'           TLRCAR
157200960216     C* DETERMINO GIORNO DI APERTURA FV
157300960216     C                   Z-ADD     1             S                 3 0
157400960216     C     CAR(C)        LOOKUP    LNP(S)                                 31
157500960216     C     DFV(S)        IFEQ      WDTN
157600960216     C                   MOVE      'A'           TLRGFV
157700960216     C                   ELSE
157800960219     C                   MOVE      DFV(S)        WDATAA
157900960219     C                   EXSR      SRGIO
158000960219     C                   Z-ADD     1             XX
158100960219     C     YY            LOOKUP    NUM(XX)                                30
158200960219     C   30              MOVE      ALF(XX)       TLRGFV
158300960220     C  N30              MOVE      *BLANKS       TLRGFV
158400960216     C                   END
158500960219     C                   WRITE     TNTLR000
158600960216     C* SCRIVO FILE DEGLI ORARI
158700960219     C                   MOVE      V2CTRN        TLOTRN
158800960219     C                   MOVE      WDTN          TLODDE
158900960219     C                   MOVE      WNFM          TLONFM
159000960219     C* DETERMINO GIORNO PARTENZA
159100960219     C                   MOVEL     PAR(C)        WPAR
159200960219     C     DFV(S)        IFEQ      WPAR
159300960219     C                   MOVE      TLRGFV        TLOGGP
159400960219     C                   ELSE
159500960219     C                   MOVE      WPAR          WDATAA
159600960219     C                   EXSR      SRGIO
159700960219     C                   Z-ADD     1             XX
159800960219     C     YY            LOOKUP    NUM(XX)                                30
159900960219     C   30              MOVE      ALF(XX)       TLOGGP
160000960220     C  N30              MOVE      *BLANKS       TLOGGP
160100960219     C                   END
160200960219     C                   MOVE      PAR(C)        TLOHHP
160300960219     C* DETERMINO GIORNO ARRIVO SE HO LA DATA DI ARRIVO
160400960219     C     *IN32         IFEQ      *ON
160500960219     C     ARR(W)        ANDLT     WARR
160600960219     C                   MOVEL     ARR(W)        WDATAA
160700960219     C     WPAR          IFEQ      WDATAA
160800960219     C                   MOVE      TLOGGP        TLOGGA
160900960219     C                   ELSE
161000960219     C                   EXSR      SRGIO
161100960219     C                   Z-ADD     1             XX
161200960219     C     YY            LOOKUP    NUM(XX)                                30
161300960219     C   30              MOVE      ALF(XX)       TLOGGA
161400960220     C  N30              MOVE      *BLANKS       TLOGGA
161500960219     C                   END
161600960219     C                   MOVE      ARR(W)        TLOHHA
161700960219     C                   ELSE
161800960219     C                   CLEAR                   TLOGGA
161900960219     C                   CLEAR                   TLOHHA
162000960219     C                   END
162100960219     C                   MOVE      WDATEU        TLODIR
162200960219     C                   WRITE     TNTLO000
162300960216     C*
162400960216     C* CLEARO SCARICO E RELATIVA DATA ORA GIà SCRITTA
162500960219     C   32              MOVE      *HIVAL        SCA(W)
162600960219     C   32              MOVE      *HIVAL        ARR(W)
162700960216     C*
162800960216     C                   ADD       1             C
162900960216     C                   ENDDO
163000960216     C* VEDO SE MI SONO RIMASTI SCARICHI DA SCRIVERE
163100960216     C* WW CONTINENE L'INDICE DELL'ULTIMO ELEMENTO DELLA SCHIERA ARR
163200960216     C* VALORIZZATO. DA QUELL'INDICE IN POI IMPOSTO TUTTI 9
163300960216     C* AFFINCHE' LA SCHIERA DOPO IL SORTA CONTENGA LE DATE ORE VALIDE
163400960216     C* A PARTIRE DAL PRIMO ELEMENTO
163500960216     C                   ADD       1             WW
163600960216     C     WW            IFLE      150
163700960216     C                   MOVEA     *HIVAL        ARR(WW)
163800960216     C                   END
163900960216     C                   MOVEA     ARR           SAR
164000960216     C                   SORTA     SAR
164100960216     C                   Z-ADD     1             WW
164200960220     C     WW            DOWLE     150
164300960216     C     SAR(WW)       ANDLT     *ALL'9'
164400960216     C                   Z-ADD     1             W
164500960216     C     SAR(WW)       LOOKUP    ARR(W)                                 30
164600960220     C                   ADD       1             WNFM
164700960216     C                   Z-ADD     WNFM          TLRNFM
164800960216     C                   MOVE      SCA(W)        TLRFIL
164900960216     C                   MOVE      'S'           TLRSCA
165000960216     C                   MOVE      ' '           TLRCAR
165100960219     C                   CLEAR                   TLRGFV
165200960219     C                   WRITE     TNTLR000
165300960219     C* SCRIVO FILE ORARI
165400960219     C                   Z-ADD     WNFM          TLONFM
165500960219     C* DETERMINO GIORNO DI ARRIVO SE HO LA DATA DI ARRIVO
165600960219     C     ARR(W)        IFLT      WARR
165700960219     C                   MOVEL     ARR(W)        WDATAA
165800960219     C     WDTN          IFEQ      WDATAA
165900960219     C                   MOVE      'A'           TLOGGA
166000960219     C                   ELSE
166100960219     C                   EXSR      SRGIO
166200960219     C                   Z-ADD     1             XX
166300960219     C     YY            LOOKUP    NUM(XX)                                30
166400960219     C   30              MOVE      ALF(XX)       TLOGGA
166500960220     C  N30              MOVE      *BLANKS       TLOGGA
166600960219     C                   END
166700960219     C                   MOVE      ARR(W)        TLOHHA
166800960219     C                   ELSE
166900960219     C                   CLEAR                   TLOGGA
167000960219     C                   CLEAR                   TLOHHA
167100960219     C                   END
167200960219     C                   CLEAR                   TLOGGP
167300960219     C                   CLEAR                   TLOHHP
167400960219     C                   WRITE     TNTLO000
167500960216     C                   MOVE      *ZEROS        ARR(W)
167600960216     C                   ADD       1             WW
167700960216     C                   ENDDO
167800960216     C                   ENDSR
167900960219     C************** DETERMINO GIORNO (A,B,C.....)   ******************
168000960219     C     SRGIO         BEGSR
168100960219     C                   Z-ADD     1             XX                2 0
168200960219     C                   Z-ADD     1             YY                2 0
168300960220     C                   MOVE      WDAT          WDATX
168400960219     C*
168500960220     C     WDATX         DOWLT     WDATAA
168600960219     C* VERIFICO SE DATA FESTIVA
168700960220     C                   MOVE      WDATX         W0080             8 0
168800960219     C                   EXSR      CTRFES
168900960319    2C     WFEST         IFNE      'B'
169000960219     C                   ADD       1             YY                             NO fest.
169100960219    2C                   END
169200960219     C* CALCOLO DATA TRAINO+XX
169300960219     C                   ADD       1             XX
169400960219     C                   CLEAR                   WGIDAT
169500960219     C     WTGI          ADD       XX            GI8TGI
169600960219     C                   CALL      'XSRGI8'
169700960219     C                   PARM                    WGIDAT
169800960220     C                   MOVE      GI8INV        WDATX
169900960219     C                   ENDDO
170000960219     C                   ENDSR
170100960219     C*************  DELET ANAGRAFICA TRAINO  *************************
170200960219     C     DELTRN        BEGSR
170300960219     C* LISTINI
170400010828     C     KANA          SETLL     TITLL01L
170500010828     C     KANA          READE     TITLL01L                               30
170600960219     C     *IN30         DOWEQ     *OFF
170700010828     C                   DELETE    TITLL000
170800010828     C     KANA          READE     TITLL01L                               30
170900960219     C                   ENDDO
171000960219     C* METRI CUBI
171100960219     C     KANA          SETLL     TNTLM01L
171200960219     C     KANA          READE     TNTLM01L                               30
171300960219     C     *IN30         DOWEQ     *OFF
171400960219     C                   DELETE    TNTLM000
171500960219     C     KANA          READE     TNTLM01L                               30
171600960219     C                   ENDDO
171700960219     C* ORARI
171800960219     C     KANA          SETLL     TNTLO01L
171900960219     C     KANA          READE     TNTLO01L                               30
172000960219     C     *IN30         DOWEQ     *OFF
172100960219     C                   DELETE    TNTLO000
172200960219     C     KANA          READE     TNTLO01L                               30
172300960219     C                   ENDDO
172400960219     C* FERMATE
172500960219     C     KANA          SETLL     TNTLR01L
172600960219     C     KANA          READE     TNTLR01L                               30
172700960219     C     *IN30         DOWEQ     *OFF
172800960219     C                   DELETE    TNTLR000
172900960219     C     KANA          READE     TNTLR01L                               30
173000960219     C                   ENDDO
173100960219     C* TESTATA
173200960219     C     KANA          CHAIN     TNTLT01L                           30
173300960219     C  N30              DELETE    TNTLT000
173400960219     C                   ENDSR
173500960219     C************ AGGIORNAMENTO STORICO TRAINI PER ABBINAMENTO  ******
173600960219     C     AGGSTO        BEGSR
173700960219     C     KTSR1B        SETLL     TNTSR01L
173800960319     C     KTSR1B        READE     TNTSR01L                               31
173900960219     C* SE è FILIALE CHE ESPONE IMPORTO AGGIORNO TESTATA
174000960319     C  N31KTST1         CHAIN     TNTST01L                           31
174100960319     C  N31TSTNFE        IFEQ      TSRNFP
174200010907
174300010907      * in questo caso devo fare dei confronti con la divisa della griglia, quindi se
174400010907      * vuota quella dell'estensione prendo quella della griglia
174500010907     c                   if        fwsvip = *blanks
174600010907     c                   eval      fwsvip = tstdiv
174700010907     c                   endif
174800010907     c                   if        fwsves = *blanks
174900010907     c                   eval      fwsves = tstdiv
175000010907     c                   endif
175100010828
175200010828      * aggancio tabella divisa griglia
175300010828     c                   clear                   dscv
175400010828     c                   movel     'CV'          kcod
175500010828     c                   movel(p)  tstdiv        kkey
175600010828     c     ktab          chain     tabel00f
175700010828     c                   if        %found(tabel00f)
175800010828     c                   movel     tbluni        dscv
175900010828     c                   endif
176000010828
176100010828      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
176200010828      * devo convertire fwsipt e fwsies nella divisa della griglia
176300010828     c                   eval      w_importo = fwsipt
176400011023     c                   if        fwsvip <> tstdiv and fwsipt <> 1
176500011023     c                             and fwsipt <> *zeros
176600010828     c                   eval      w_divisa = fwsvip
176700010828     c                   exsr      Sr_Converti
176800010828     c                   endif
176900010828     c                   eval      tstipt = w_importo
177000010828
177100010828     c                   eval      w_importo = fwsies
177200011023     c                   if        fwsves <> tstdiv and fwsies <> 1
177300011023     c                             and fwsies <> *zeros
177400010828     c                   eval      w_divisa = fwsves
177500010828     c                   exsr      Sr_Converti
177600010828     c                   endif
177700010828     c                   eval      tsties = w_importo
177800010828
177900010828     C**!!!              MOVE      FVSIPT        TSTIPT
178000010828     C**!!!              MOVE      FVSIES        TSTIES
178100960219     C                   MOVE      V2CPDR        TSTPDR
178200960219     C                   MOVE      FVSTMZ        TSTTMZ
178300960219     C                   EXSR      SRLIST
178400960219     C                   MOVE      V2CCAU        TSTCAU
178500010828     C**!!!              MOVE      V2CICO        TSTICO
178600010828      * se la divisa impostata a video non è uguale alla divisa della griglia
178700010828      * devo convertire v2cico nella divisa della griglia
178800010828     c                   eval      w_importo = v2cico
178900011024     c                   if        v2cdiv <> tstdiv and v2cico <> 1
179000011024     c                             and v2cico <> 0
179100010828     c                   eval      w_divisa = v2cdiv
179200010828     c                   exsr      Sr_Converti
179300010828     c                   endif
179400010828     c                   eval      tstico = w_importo
179500010828
179600960219     C                   MOVE      '2'           TSTCNF
179700960219     C                   UPDATE    TNTST000
179800960219     C                   ELSE
179900960219     C                   UNLOCK    TNTST01L
180000960219     C                   END
180100960319     C     *IN31         DOWEQ     *OFF
180200960219     C                   MOVE      FVSLNP        TSRLNP
180300960219     C                   MOVE      FVSNFV        TSRNFV
180400960219     C                   MOVE      FVSDFV        TSRDFV
180500960219     C                   MOVE      FVSDRP        TSRDRP
180600960219     C                   MOVE      FVSHRP        TSRHRP
180700960219     C                   MOVE      FVSRTP        TSRRTP
180800960219     C     TSRDRP        IFEQ      TSRDTP
180900960219     C     TSRHRP        ANDEQ     TSRHTP
181000970327     C     TSRRTP        ANDEQ     *BLANKS
181100960219     C                   MOVE      *HIVAL        TSRDKP
181200960314     C                   MOVE      'S'           TSRFKP
181300960219     C                   END
181400960219     C                   MOVE      *HIVAL        TSRDKI
181500960314     C                   MOVE      'S'           TSRFKI
181600960219     C                   UPDATE    TNTSR000
181700960319     C     KTSR1B        READE     TNTSR01L                               31
181800960219     C                   ENDDO
181900960219     C*
182000960219     C                   ENDSR
182100960219     C****************  RICERCA LISTINO DA CARICARE  ******************
182200960219     C     SRLIST        BEGSR
182300960219     C* PRIMO TENTATIVO DI RICERCA : TRAZIONISTA E TIPO AUTOMEZZO DATI
182400960219     C                   MOVE      '1'           W001A             1
182500960219     C                   MOVEL     FVSTMZ        KTMZ
182600960219     C                   MOVE      V2CPDR        KPDR
182700010828     C     KTLL          CHAIN(N)  TITLL01L                           30
182800960906     C  N30TLLATB        COMP      ' '                                3030
182900960219    1C     *IN30         DOWEQ     *ON
183000960219     C     W001A         ANDLT     '4'
183100960219    2C                   SELECT
183200960219     C* SECONDO TENTATIVO DI RICERCA : TRAZIONISTA DATO
183300960219     C*                                AUTOMEZZO GENERICO = '99'
183400960219     C     W001A         WHENEQ    '1'
183500960219     C                   MOVE      *ALL'9'       KTMZ
183600960219     C                   MOVE      '2'           W001A
183700960219     C* TERZO TENTATIVO DI RICERCA : TRAZIONISTA GENERICO = 9999999
183800960219     C*                              AUTOMEZZO DEL FOGLIO VIAGGIO
183900960219     C     W001A         WHENEQ    '2'
184000960219     C                   MOVE      *ALL'9'       KPDR
184100960219     C                   MOVEL     FVSTMZ        KTMZ
184200960219     C                   MOVE      '3'           W001A
184300960219     C* QUARTO TENTATIVO DI RICERCA : TRAZIONISTA E AUTOMEZZO GENERICI
184400960219     C     W001A         WHENEQ    '3'
184500960219     C                   MOVE      *ALL'9'       KTMZ
184600960219     C                   MOVE      '4'           W001A
184700960219    2C                   ENDSL
184800010828     C     KTLL          CHAIN(N)  TITLL01L                           30
184900960906     C  N30TLLATB        COMP      ' '                                3030
185000960219    1C                   ENDDO
185100960219     C*
185200960219     C* LISTINO TROVATO: MEMORIZZO IMPORTO
185300960219    1C     *IN30         IFEQ      *OFF
185400010828
185500010828      * se la divisa del listino non è uguale alla divisa della griglia
185600010828      * devo convertire tllili nella divisa della griglia
185700010828     c                   eval      w_importo = tllili
185800011024     c                   if        tlldiv <> tstdiv and tllili <> 1
185900011024     c                             and tllili <> 0
186000010828     c                   eval      w_divisa = tlldiv
186100010828     c                   exsr      Sr_Converti
186200010828     c                   endif
186300010828     c                   eval      tstili = w_importo
186400010828     C**!!!              Z-ADD     TLLILI        TSTILI
186500960219    1C                   END
186600960219     C                   ENDSR
186700010828      *----------------------------------------------------------------
186800010828      * CONVERTI - Routine conversione importo
186900010828      *----------------------------------------------------------------
187000010828     c     Sr_Converti   begsr
187100010828
187200010828     c                   clear                   yeurcods
187300010828     c                   movel     w_divisa      yecdvi
187400010828     c                   movel     tstdiv        yecdvo
187500010828     c                   z-add     w_importo     yecimi
187600010828     c                   move      §cvdec        yecdco
187700010828     c                   call      'YEURCO'
187800010828     c                   parm                    yeurcods
187900010828     c                   if        yecesi  = ' '
188000010828     c                   z-add     yecimo        w_importo
188100010828     c                   endif
188200010828
188300010828     c                   endsr
188400010828
188500951128     C**************  OPERAZIONI INIZIALI  ****************************
188600951128     C     *INZSR        BEGSR
188700951128     C     *ENTRY        PLIST
188800951128     C                   PARM                    KPJBA
188900010828     C                   PARM                    PFLP                           SOLO X D42MOD=C
189000010827     C                   MOVEL     KPJBU         DSTL42
189100951129     C*
189200951129     C                   Z-ADD     1             CODUT
189300951129     C                   CALL      'X§PARUT'
189400951129     C                   PARM                    UT§DSE
189500960214     C                   MOVEL     RAGUT         V1DRSU
189600960229     C* REPERIMENTO DATA ULTIMO CONTROLLO
189700960229     C     *DTAARA       DEFINE                  TNTL30
189800960229     C                   IN        TNTL30
189900960216     C* PULIZIA CAMPI DI OUTPUT
190000010828     C     D42MOD        IFEQ      'R'
190100010828     C                   CLEAR                   D42LNP
190200960311     C                   END
190300010828     C                   CLEAR                   D42NFV
190400010828     C                   CLEAR                   D42ERR
190500960216     C*
190600960216     C     *LIKE         DEFINE    NRR1          WNRR1
190700960216     C     *LIKE         DEFINE    NRR1          NRRSAV
190800960216     C     *LIKE         DEFINE    FVSABN        WABN
190900960216     C     *LIKE         DEFINE    FVSTRS        WTRS
191000960216     C     *LIKE         DEFINE    TSRFFP        WFFP
191100960216     C     *LIKE         DEFINE    V1CANN        WCANN
191200960228     C     *LIKE         DEFINE    V1CBIS        WCBIS
191300960307     C     *LIKE         DEFINE    V1CCNT        WCCNT
191400960216     C     *LIKE         DEFINE    V1CNBR        W1CNBR
191500960216     C     *LIKE         DEFINE    V1SSCE        WSCE
191600960219     C     *LIKE         DEFINE    V1SLNP        WLNPS
191700960219     C     *LIKE         DEFINE    FV2LNP        WLNP
191800960216     C     *LIKE         DEFINE    FVSPDR        KPDR
191900000807     C     *LIKE         DEFINE    TLZTIP        KTIP
192000960216     C     *LIKE         DEFINE    V2CCAU        WCAU
192100960216     C     *LIKE         DEFINE    V2DCAU        WDCAU
192200960216     C     *LIKE         DEFINE    X             WFIRST
192300960216     C     *LIKE         DEFINE    TBLCOD        KCOD
192400960216     C     *LIKE         DEFINE    TBLKEY        KKEY
192500960216     C     *LIKE         DEFINE    FV2EPA        WEPA
192600960216     C     *LIKE         DEFINE    FV2TDH        WTDH
192700960216     C     *LIKE         DEFINE    FVSPDR        WPDR
192800960216     C     *LIKE         DEFINE    FVSDPD        WDPD
192900960228     C     *LIKE         DEFINE    FVSPDR        WPDR1
193000960228     C     *LIKE         DEFINE    FVSDPD        WDPD1
193100011012     C     *LIKE         DEFINE    FWSIPT        WIPT
193200011012     C     *LIKE         DEFINE    FWSIPT        WIPT1
193300960424     C     *LIKE         DEFINE    FVSBIS        WBIS1
193400960219     C     *LIKE         DEFINE    FVSTMZ        WTMZ
193500960228     C     *LIKE         DEFINE    FVSTMZ        WTMZ1
193600960319     C     *LIKE         DEFINE    FVSLNP        WFES
193700960319     C     *LIKE         DEFINE    FVSLNP        WFES1
193800960216     C     *LIKE         DEFINE    FVSDFV        WDFV
193900960219     C     *LIKE         DEFINE    TLTDDE        WDTN
194000971118     C     *LIKE         DEFINE    D55TFA        WTAR
194100971118     C     *LIKE         DEFINE    D55TFA        WTARS
194200960220     C     *LIKE         DEFINE    TLMTFA        WTFA
194300960216     C     *LIKE         DEFINE    TLRNFM        WNFM
194400960219     C     *LIKE         DEFINE    G08TGI        WTGI
194500960219     C     *LIKE         DEFINE    G08INV        WDATAA
194600960219     C     *LIKE         DEFINE    G08INV        WDAT
194700960220     C     *LIKE         DEFINE    G08INV        WDATX
194800960219     C     *LIKE         DEFINE    G08INV        WPAR
194900960219     C     *LIKE         DEFINE    FVSLNP        KLNP
195000960219     C     *LIKE         DEFINE    FVSNFV        KNFV
195100960219     C     *LIKE         DEFINE    FVSTMZ        KTMZ
195200960219     C     *LIKE         DEFINE    CLNANN        KANN
195300960219     C     *LIKE         DEFINE    CLNMES        KMES
195400960219     C     *LIKE         DEFINE    CLNTFP        KTFP
195500960219     C     *LIKE         DEFINE    CLNTFA        KTFA
195600971118     C     *LIKE         DEFINE    D55TFA        WFVG
195700960219     C     *LIKE         DEFINE    FV2LAI        WSCA
195800960319     C     *LIKE         DEFINE    TSRLNP        KLNPT
195900960319     C     *LIKE         DEFINE    TSRNFV        KNFVT
196000960322     C     *LIKE         DEFINE    FVDTRC        WTRC
196100000218     C     *LIKE         DEFINE    ATBCOD        KATCOD
196200000218     C     *LIKE         DEFINE    ATBAZI        KATAZI
196300000218     C     *LIKE         DEFINE    ATBKEY        KATKEY
196400960216     C* DEFINISCO AREA DATI NUMERATORE TRAINI ECCEZIONALI
196500960216     C     *DTAARA       DEFINE                  TNTL02            7 0
196600960216     C*
196700960216     C     KTSR1         KLIST
196800010828     C                   KFLD                    D42TRN
196900010828     C                   KFLD                    D42DTN
197000010828     C                   KFLD                    D42NFP
197100960219     C     KTSR1B        KLIST
197200960219     C                   KFLD                    TSRTRN
197300960219     C                   KFLD                    TSRDTN
197400960219     C                   KFLD                    TSRNFP
197500960219     C     KTSR3         KLIST
197600960219     C                   KFLD                    V2CTRN
197700960219     C                   KFLD                    WDTN
197800960219     C                   KFLD                    FVSLNP
197900960319     C     KTSR4         KLIST
198000960319     C                   KFLD                    KLNPT
198100960319     C                   KFLD                    KNFVT
198200960319     C                   KFLD                    TSTTRN
198300960319     C                   KFLD                    TSTDTN
198400960319     C                   MOVE      *ZEROS        KLNPT
198500960319     C                   MOVE      *ZEROS        KNFVT
198600960216     C     KFVS          KLIST
198700960216     C                   KFLD                    WABN
198800960216     C                   KFLD                    WTRS
198900960216     C                   KFLD                    WFFP
199000960216     C     KFVS2         KLIST
199100960216     C                   KFLD                    WABN
199200960216     C                   KFLD                    WTRS
199300960216     C     KFVS1         KLIST
199400960219     C                   KFLD                    KLNP
199500960219     C                   KFLD                    KNFV
199600960216     C     KFV2          KLIST
199700960219     C                   KFLD                    WLNP
199800960216     C                   KFLD                    NFV
199900960216     C                   KFLD                    WEPA
200000960219     C                   KFLD                    WSCA
200100960216     C                   KFLD                    WTDH
200200010828     C     KFWS          KLIST
200300010828     C                   KFLD                    fvsLNP
200400010828     C                   KFLD                    fvsNFV
200500960216     C     KTAB          KLIST
200600960216     C                   KFLD                    CODUT
200700960216     C                   KFLD                    KCOD
200800960216     C                   KFLD                    KKEY
200900960220     C     KTAB0         KLIST
201000960220     C                   KFLD                    CODUT
201100960220     C                   KFLD                    KCOD
201200960216     C     KTLM          KLIST
201300960216     C                   KFLD                    V2CTRN
201400960216     C                   KFLD                    WDTN
201500960219     C                   KFLD                    WLNP
201600960220     C                   KFLD                    WTFA
201700960322     C                   KFLD                    WTARS
201800960219     C     KANA          KLIST
201900960219     C                   KFLD                    V2CTRN
202000960219     C                   KFLD                    WDTN
202100960219     C     KTST1         KLIST
202200960219     C                   KFLD                    TSRTRN
202300960219     C                   KFLD                    TSRDTN
202400960219     C     KTLL          KLIST
202500960219     C                   KFLD                    TSTTRN
202600960219     C                   KFLD                    TSTDTN
202700960219     C                   KFLD                    KTMZ
202800960219     C                   KFLD                    KPDR
202900000807     C     KTNTLZ        KLIST
203000000807     C                   KFLD                    KTIP
203100000807     C                   KFLD                    KPDR
203200000807     C                   EVAL      KTIP = 'T '                                  Tipol.TRAZIONISTA
203300960219     C     KCLN          KLIST
203400960219     C                   KFLD                    KTFP
203500960219     C                   KFLD                    KTFA
203600960219     C                   KFLD                    KANN
203700960219     C                   KFLD                    KMES
203800960322     C*
203900960322     C     KFVD          KLIST
204000960322     C                   KFLD                    WLNP
204100960322     C                   KFLD                    NFV
204200960322     C                   KFLD                    WFVG
204300960322     C                   KFLD                    WTRC
204400000218     C* LETTURA ANATB01L - PARZIALE
204500000218     C     KEATAB        KLIST
204600000218     C                   KFLD                    KATCOD
204700000218     C                   KFLD                    KATAZI
204800000218     C                   KFLD                    KATKEY
204900000211     C*--------------------
205000000211     C*---------- RICERCA DITTA :
205100000211     C*--------------------
205200000218     C***                MOVEL     'SOC001'      TIPXSC
205300000218     C***                MOVEL     *BLANK        SOCXSC
205400000218     C***                EXSR      REPSOC
205500000218     C***  RTNXSC        IFNE      '1'
205600000218     C***                MOVEL     XSOCDS        SOC001
205700000218     C***                MOVEL     xscrgs        RSUT             20
205800000218     c***                end
205900960216     C*
206000960216     C                   MOVE      'A'           WEPA
206100960216     C                   MOVE      'R'           WTDH
206200960216     C                   MOVEL     'S'           WABN
206300960322     C                   MOVEL     '1'           WTRC
206400960216     C                   CLEAR                   WTRS
206500960216     C                   MOVE      *ZEROS        II                3 0
206600960216     C                   CLEAR                   WFFP
206700960216     C                   SETOFF                                       01
206800960219     C                   Z-ADD     68            KTFP
206900960219     C                   CLEAR                   KTFA
207000960213     C* 05 ON --> RICHIESTO ELENCO FOGLI VIAGGIO DA ABBINARE MANUALM.
207100010828    1C     D42MOD        IFEQ      'R'
207200960319     C                   SETON                                        05
207300960213     C                   MOVEL     TIT1          V1DTI1
207400960213     C                   MOVEL     TIT2          V1DTI2
207500960213     C                   MOVE      ' 1'          V1CSCE
207600960216     C* Carico schiera ffa se parametri di input valorizzati
207700010828    2C     D42TRN        IFGT      *ZEROS
207800960216     C                   SETON                                        01
207900960216     C                   CLEAR                   W001A
208000960216     C     KTSR1         SETLL     TNTSR01L
208100960219     C     KTSR1         READE(N)  TNTSR01L                               30
208200960216    3C     *IN30         DOWEQ     *OFF
208300960220    4C     TSRATB        IFEQ      *BLANKS
208400960216    5C     W001A         IFEQ      ' '
208500960216     C                   MOVE      TSRFFP        WFFP
208600960216     C                   MOVE      '1'           W001A             1
208700960216    5C                   END
208800960216     C                   ADD       1             II
208900960216     C                   MOVE      TSRFFA        FFA(II)
209000960216    4C                   END
209100960315     C     KTSR1         READE(N)  TNTSR01L                               30
209200960216    3C                   ENDDO
209300960216    2C                   END
209400960216   X1C                   ELSE
209500010828     C     D42MOD        IFEQ      ' '
209600960216     C                   OPEN      FNFV201L
209700960216     C                   OPEN      TNTLM01L
209800960219     C                   OPEN      TNTLR01L
209900960219     C                   OPEN      TNTLO01L
210000960219     C                   OPEN      TNTLT01L
210100010828     C                   OPEN      TITLL01L
210200960219     C                   OPEN      TNTST01L
210300960319     C                   OPEN      TNTSR04L
210400960219     C                   OPEN      AZCLN01L
210500960322     C                   OPEN      FNFVD01L
210600960213     C                   MOVEL     TIT3          V1DTI1
210700960213     C                   MOVEL     TIT4          V1DTI2
210800960213     C                   MOVE      'Sc'          V1CSCE
210900960220     C* CARICAMENTO TABELLA 1Y - SIMBOLI CALENDARIO BARTOLINI
211000960220     C* (CARICAMENTO DEI SIMBOLI FESTIVI PER PROCDEURA TRAINI)
211100960220     C                   MOVE      *ZEROS        X
211200960220     C                   MOVEL(P)  '1Y'          KCOD
211300960220     C     KTAB0         SETLL     TABEL00F
211400960220     C     KTAB0         READE     TABEL00F                               30
211500960220     C     *IN30         DOWEQ     *OFF
211600960220     C     TBLFLG        IFEQ      *BLANKS
211700960220     C                   MOVEL     TBLUNI        DS1Y
211800960220     C     §1YFET        IFNE      *BLANKS
211900960220     C                   ADD       1             X
212000960220     C                   MOVEL     TBLKEY        S1Y(X)
212100960220     C                   MOVE      §1YFET        FET(X)
212200960220     C                   END
212300960220     C                   END
212400960220     C     KTAB0         READE     TABEL00F                               30
212500960220     C                   ENDDO
212600960319     C                   END
212700960216    1C                   END
212800960216     C* RILEVO LA DATA DEL GIORNO
212900960216     C                   TIME                    WTIME            14 0          ORA E DATA
213000960216     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
213100960216     C                   CLEAR                   WLBDA8
213200960216     C                   MOVE      WDATE         G08DAT
213300960216     C                   CALL      'XSRDA8'
213400960216     C                   PARM                    WLBDA8
213500960216     C                   MOVE      G08INV        WDATEU            8 0
213600010907
213700010907      * recupero la divisa dalla tabella GED
213800010907     c                   clear                   dged
213900010907     c                   clear                   tibs02ds
214000010907     c                   movel     'L'           t02tla
214100010907     c                   movel     'C'           t02mod
214200010907     c                   movel     knsif         t02sif
214300010907     c                   movel     'GED'         t02cod
214400010907     c                   movel     '1'           t02ke1
214500010907     c                   call      'TIBS02R'
214600010907     c                   parm                    kpjba
214700010907     c                   parm                    tibs02ds
214800010907    5c                   movel     t02uni        dged
214900951128     C*
215000951128     C                   ENDSR
215100000211     C*----------------------------------------------------*
215200000211     C* Reperimento dati società
215300000211     C*----------------------------------------------------*
215400000218     C***  REPSOC        BEGSR
215500000211     C*
215600000218     C***                CALLB     'XSOC'
215700000218     C***                PARM                    TIPXSC            6
215800000218     C***                PARM                    SOCXSC            3
215900000218     C***                PARM                    CDSXSC            9 0
216000000218     C***                PARM                    MODXSC            3
216100000218     C***                PARM      *blanks       RTNXSC            1
216200000218     C***                PARM                    XSOCDS
216300000218     C***                PARM                    KPJBA
216400000211     C*
216500000218     C***                ENDSR
216600960319     OTNTST000  E            AGGTST
216700960319     O                       TSTCNF
216800960219** MSG
216900960213Fine scorrimento                                                              01
217000960213Scegliere un foglio viaggio                                                   02
217100960214Scelta errata                                                                 03
217200960214Numero scelta già immesso                                                     04
217300960214E' già stato scelto un FV con linea di partenza  xxx-xxxxxxxxxx               05
217400960214Presente in questo FV una filiale di scarico = alla linea di part.del 1°FV    06
217500960214Foglio viaggio già annullato                                                  07
217600960214Foglio viaggio già ripristinato                                               08
217700960215Data FV non in sequenza: cambiare numerazione o scegliere un altro foglio vg. 09
217800960214Foglio viaggio annullato: scelta errata                                       10
217900960215Non trovati trazionisti per chiave alfabetica richiesta                       11
218000960215Codice trazionista obbligatorio                                               12
218100960215Codice trazionista inesistente o errato                                       13
218200960215Importo obbligatorio                                                          14
218300960215Causale contabilizzazione obbligatoria                                        15
218400960219Data foglio viaggio festiva e > di data traino: scelta non ammessa            16
218500960220Causale contabilizzazione inesistente o annullata                             17
218600960424Numeratore traini in uso da un altro lavoro                                   18
218700960223Alcuni FV non abbinati a traino a causa delle modifiche fatte all'anagrafica  19
218800011012Immettere la divisa                                                           20
218900010828Divisa errata                                                                 21
219000010828Divisa non ammessa per i traini                                               22
219100010828Non ammessi importi con decimali per la divisa indicata                       23
219200011001Se modificata la divisa ridigitare il relativo importo                        24
219300960219**  NUM
2194009602200102030405060708091011121314151617181920
219500960219**  ALF
219600960219BCDEFGHILMNOPQRSTUVZ
