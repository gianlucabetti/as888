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
002200090120     FAZorg01L  IF   E           K DISK
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
006000090120
006100090120     D ORG             S              3  0 DIM(200)                             *CODICE
006200010827
006300010827     d wdiv            s                   like(fwsvip)
006400010827     d wdiv1           s                   like(fwsvip)
006500010828     d w_importo       s                   like(fwsipt)
006600010828     d w_divisa        s                   like(fwsvip)
006700010828     d w_delta         s                   like(fwsipt)
006800010828     d sav_tstili      s                   like(tstili)
006900011001     d sav_v2cdiv      s                   like(v2cdiv)
007000011001     d w_decimali      s              2  0
007100011012     d flgged          s              1    inz('0')
007200090120     d wfilp           s                   like(tlrfil)
007300010827
007400960220     D                 DS
007500960220     D  TLTGSE                 1      7
007600960220     D  GSE                    1      7
007700960220     D                                     DIM(7)                               X TLTGSE
007800960219     D                 DS
007900960219     D  CLNPOM                 1     31
008000960219     D  POM                    1     31
008100960219     D                                     DIM(31)                              FESTIV.POMER.
008200960325     D DSFIL           DS           920    OCCURS(100)
008300960216     D  NFV                    1      5  0
008400960325     D  FVG                    6    455
008500960325     D                                     DIM(150)                             FIL.FV
008600960325     D  FSC                  456    905
008700960325     D                                     DIM(150)                             FIL.SCAR.FV
008800960325     D  DDRP                 906    913  0
008900960325     D  DHRP                 914    917  0
009000960325     D  DLNA                 918    920
009100951128     D WLBDA8          DS
009200951128     D  G08DAT                 1      8  0
009300951128     D  G08INV                 9     16  0
009400951128     D  G08ERR                17     17
009500951128     D  G08TGI                18     22  0
009600960219     D WGIDAT          DS
009700960219     D  GI8DAT                 1      8  0
009800960219     D  GI8INV                 9     16  0
009900960219     D  GI8TGI                17     21  0
010000960216     D DSFVS         E DS                  EXTNAME(TNFVS00F)
010100960301     D  FFV                   97    543
010200960219     D                                     DIM(149)                             FIL.TNFVS
010300960301     D  FLP                  544    990
010400960216     D                                     DIM(149)                             FIL.SCARIC.TNFVS
010500960301     D WDSFVS          DS          1129
010600960216     D                 DS
010700960216     D  DATORA                 1     12  0
010800960216     D  FV2DPA                 1      8  0
010900960216     D  FV2HPA                 9     12  0
011000951128     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
011100960229     D TNTL30        E DS                  EXTNAME(TNTL30DS)
011200960219     D DS00          E DS                  EXTNAME(TNTL00DS)
011300960320     D DSTL37        E DS                  EXTNAME(TNTL37DS)
011400960322     D DSTL33        E DS                  EXTNAME(TNTL33DS)
011500960219     D DS1Y          E DS
011600951128     D KPJBA         E DS
011700090120     D OG148         E DS
011800090120     D OG143         E DS
011900010827     D DSTL42        E DS                  EXTNAME(TNTL42DS)
012000971118     D DSLV55        E DS                  EXTNAME(FNLV55DS)
012100090120     d Ficna2ds      e ds                  Inz
012200010828
012300010828     D yeurcods      E DS
012400010828     D tibs02ds      E DS
012500010828     D dscv          E DS
012600010907     D dged          E DS
012700010828     D dgei          E DS
012800010828
012900960215     D* PASSAGGIO DATI ALLA RICERCA TRAZIONISTI  - TRUL16R -
013000960215     D PARAM3          DS
013100000807     D* TIPOLOGIA T_=TRAZIONISTA
013200000807     D  PA3TIP                 1      2
013300000807     D* RAGIONE SOCIALE TRAZIONISTA
013400000807     D  PA3RSC                 3     37
013500000807     D* S.I. TRAZIONISTA
013600000807     D  PA3CSF                38     40
013700960215     D* CODICE TRAZIONISTA
013800000807     D  PA3PDR                41     47  0
013900960215     D* PA3FLG = "3" --> NON ESISTONO TRAZIONISTI PER LA CHIAVE
014000960215     D*                  ALFABETICA RICHIESTA
014100000807     D  PA3FLG                48     48
014200960219     D* PASSAGGIO PARAMETRI A PGM DI CREAZIONE GRIGLIA
014300960219     D DSTL24          DS
014400960219     D* DATA TRAINO DAL
014500960219     D  D24DTD                 1      8  0
014600960219     D* DATA TRAINO AL
014700960219     D  D24DTA                 9     16  0
014800960219     D* NUMERO TRAINO
014900960219     D  D24TRN                17     23  0
015000960219     D* TERMINAL DI PARTENZA
015100960219     D  D24TFP                24     26  0
015200960219     D* DATA DECORRENZA TRAINO
015300960219     D  D24DDE                27     34  0
015400960219     D* CREAZIONE SE GIORNO FESTIVO (' '=No controllo festivo
015500960219     D*                             ('S'=Controllo festivo e crea solo
015600960219     D*                                  se non festivo)
015700960219     D  D24FES                35     35
015800960322     D                 DS
015900960322     D  FVDVL1                 1      9  4
016000960322     D  FVDVL2                10     18  4
016100960322     D  FVDVL3                19     27  4
016200960322     D  FVDVL4                28     36  4
016300960322     D  FVDVL5                37     45  4
016400960322     D  FVDVL6                46     54  4
016500960322     D  FVD                    1     54  4
016600960322     D                                     DIM(6)                               CAMPI TOT.VOL.L
016700951130     D                SDS
016800960214     D  V1DPGM                 1     10
016900960213     D TIT1            C                   CONST('* ELENCO FOGLI VIAGG-
017000960213     D                                     IO *')
017100960213     D TIT2            C                   CONST('*      DA ABBINARE  -
017200960213     D                                        *')
017300960213     D TIT3            C                   CONST('*CONFERMA FOGLI VIAG-
017400960213     D                                     GIO*')
017500960213     D TIT4            C                   CONST('*      NON ABBINATI -
017600960213     D                                        *')
017700000211     D*---------------------
017800000211     D* DS CAUSALI CONTABILIZZAZIONE
017900000211     D*---------------------
018000000211     D ANGY4ZDS      E DS
018100000211     D XTABDS        E DS                  INZ
018200000211     D XATBDS        E DS                  INZ
018300000211     D*---------------------
018400000211     D* DS per reperimento società
018500000211     D*---------------------
018600000211     D soc001        E DS                  EXTNAME(xsoc001ds)
018700000211     D xsocds          DS          1000
018800951128     C****************************************************************
018900951128     C*  RIEPILOGO INDICATORI
019000951128     C***************************************************************
019100960214     C* 01    -  ON --> RICEVUTI PARAMETRI DI INPUT > 0
019200960213     C* 05    -  ON --> ELENCO FOGLI VIAGGIO DA ABBINARE MANUALMENTE
019300960213     C*          OFF--> CONFERMA FOGLI VIAGGIO NON ABBINATI
019400960215     C* 28    -  ERRORE GENERICO VIDEATE
019500951129     C* 29    -  READC SUBFILE
019600951128     C* 30    -  COMODO
019700960215     C* 40    -  ERRORE CODICE TRAZIONISTA
019800960215     C* 41    -  ERRORE IMPORTO DA CONFERMARE
019900960215     C* 42    -  ERRORE CAUSALE CONTABILIZZAZIONE
020000010827     C* 43    -  ERRORE CAMPO SCELTA TL42S01
020100010828     C* 45    -  ERRORE divisa
020200951128     C***************************************************************
020300010828    1C     D42MOD        IFEQ      'C'
020400010828     C                   MOVE      D42TRN        V2CTRN
020500010828     C                   MOVE      D42DTN        WDTN
020600010828     C                   MOVE      D42LNP        FVSLNP
020700010828     C                   MOVE      D42LNA        FVSLNA
020800960312     C                   MOVEA     PFLP          FLP
020900960311     C                   EXSR      CHKABB
021000960312    2C     WOKABB        IFEQ      ' '
021100010828     C                   MOVEL     '1'           D42ERR
021200960311    2C                   END
021300960311     C                   GOTO      FINE
021400960313   X1C                   ELSE
021500010827     C                   OPEN      TNTL42D
021600010827     C* HO ASTERISCATO QUESTE SPECIFICHE PER EVITARE CHE IL TNTL42R
021700960313     C* VENGA RICHIAMATO IN MODO RICORSIVO E LE HO MESSE IN UN CLP
021800010827     C* A CUI HO ASSOCIATO L'AZIONE INTERATTIVA TL42 DI CONFERMA FV
021900960313     C* NON ABBINATI
022000010828    2C*          D42MOD    IFEQ *BLANKS
022100960220     C* Richiamo abbinamento automatico fv a traini
022200960313     C*                    CLEARKPJBU
022300960313     C*                    CALL 'TNTL25R'
022400960313     C*                    PARM           KPJBA
022500960313    2C*                    END
022600960311    1C                   END
022700960215     C* CARICAMENTO SUBFILE SCELTA FOGLI VIAGGIO
022800960223     C                   CLEAR                   SNF
022900960215     C                   EXSR      RIEINI
023000951129     C* EMISSIONE SUBFILE
023100960215     C     EMC01         TAG
023200010827     C  N05              WRITE     TL42Z01
023300010827     C   05              WRITE     TL42Z03
023400010827     C                   WRITE     TL42D01
023500010827     C                   EXFMT     TL42C01
023600951128     C                   SETOFF                                       28
023700960228     C* Tasto F3=Fine
023800960228     C     *INKC         CABEQ     *ON           FINE
023900960228     C* Tasto F12=Ritorno
024000960228     C     *INKL         CABEQ     *ON           FINE
024100960228     C* Tasto F5=Rivisualizzazione
024200960228     C     *INKE         IFEQ      *ON
024300960228     C                   EXSR      RIEINI
024400960228     C                   GOTO      EMC01
024500960228     C                   END
024600960320     C* Tasto F7=Interrogazione fogli viaggio
024700960228    1C     *INKG         IFEQ      *ON
024800960320     C                   CLEAR                   DSTL37
024900960320     C                   MOVEL     'P'           D37FLG
025000960320     C                   MOVEL     DSTL37        KPJBU
025100960320     C                   CALL      'TNTL37R'
025200960320     C                   PARM                    KPJBA
025300960228     C                   GOTO      EMC01
025400960228    1C                   END
025500960228     C* Tasto F14=Manutenzione anagrafica traini
025600960228    1C     *INKN         IFEQ      *ON
025700960228     C                   CLEAR                   DS00
025800960228     C                   MOVEL     'T02'         D00OP0                         *PARAMETRI FISSI
025900960228     C                   MOVEL     'F14'         D00OP1
026000960229     C                   MOVEL     '0'           D00F03
026100960229     C                   MOVEL     '0'           D00F12
026200960229     C                   MOVEL     '0'           D00ERR
026300960229     C                   MOVEL     *BLANKS       D00MSG
026400960228     C                   MOVEL     DS00          KPJBU
026500960229     C                   CALL      'TNTL01R'
026600960228     C                   PARM                    KPJBA
026700960228     C                   GOTO      EMC01
026800960228     C                   END
026900960228     C* Tasto F6=Annullamento/Ripristino FV
027000960228     C     *INKF         IFEQ      *ON
027100960228     C                   EXSR      ANNRIP
027200960228     C                   GOTO      EMC01
027300960228     C                   END
027400960320     C* GESTIONE OPZIONI DI SELEZIONE FV:
027500960215     C* Se passano da visualizz. FV annullati a non visualizzazione o
027600960215     C* viceversa ricarico il subfile
027700960214     C     V1CANN        IFNE      WCANN
027800960214     C                   MOVE      V1CANN        WCANN
027900960215     C                   EXSR      RIEINI
028000960215     C                   GOTO      EMC01
028100960214     C                   END
028200960228     C* Se cambiano visualizza bis/normali/tutti ricarico subfile
028300960228     C     V1CBIS        IFNE      WCBIS
028400960228     C                   MOVE      V1CBIS        WCBIS
028500960228     C                   EXSR      RIEINI
028600960228     C                   GOTO      EMC01
028700960228     C                   END
028800960307     C* FV controllati/non controllati/tutti
028900960307     C     V1CCNT        IFNE      WCCNT
029000960307     C                   MOVE      V1CCNT        WCCNT
029100960307     C                   EXSR      RIEINI
029200960307     C                   GOTO      EMC01
029300960307     C                   END
029400951129     C* Solo se ho caricato qualcosa:
029500951129    1C     NRR1          IFGT      *ZEROS
029600960215     C* Tasto Rollup
029700951129    2C     *IN25         IFEQ      *ON
029800951128     C* Fine lettura --> emetto messaggio fine scorrimento
029900951129    3C     *IN23         IFEQ      *ON                                          FINE LETTURA
030000960214     C                   Z-ADD     W1CNBR        V1CNBR
030100951129     C                   MOVEL     MSG(1)        V1ZMSG
030200951129     C                   SETON                                        28
030300951129     C                   ELSE
030400960215     C                   EXSR      RIES01
030500951129    3C                   END
030600960215     C                   GOTO      EMC01
030700951129    2C                   END
030800960215     C* ENTER: Controlli
030900960320     C                   EXSR      CTRVIS
031000960320     C* Se interrogati fogli viaggio riemetto il subfile
031100960320     C     WVIS          CABEQ     '1'           EMC01
031200960213     C   05              EXSR      CT1S01
031300960213     C  N05              EXSR      CT2S01
031400960215     C   28              GOTO      EMC01
031500960215     C*
031600960215     C* VIDEATA DI RICHIESTA PARAMETRI PER CREAZIONE ANAGRAFICA TRAINO
031700960215    2C     *IN05         IFEQ      *OFF
031800960215     C                   EXSR      RIED02
031900960220     C   28              GOTO      EMC01
032000960215     C     EMD02         TAG
032100010827     C                   WRITE     TL42Z02
032200010827     C                   WRITE     TL42D01
032300010827     C                   EXFMT     TL42D02
032400960215     C                   SETOFF                                       28
032500960215     C* Tasto F3=Fine
032600960215     C     *INKC         CABEQ     *ON           FINE
032700960215     C* Tasto F12=Ritorno
032800960220     C     *INKL         IFEQ      *ON
032900960220     C                   SETOFF                                       28
033000960220     C                   GOTO      EMC01
033100960220     C                   END
033200960215     C* CONTROLLI
033300960215     C                   EXSR      CTRD02
033400960215     C   28              GOTO      EMD02
033500960215     C* F6= CONFERMA
033600960215     C   KF              EXSR      CONFE
033700960219     C   KF              GOTO      EMC01
033800960215     C  NKF              GOTO      EMD02
033900960215    2C                   END
034000960215     C*
034100951129    1C                   END
034200971118     C**
034300971118     C                   CLEAR                   DSLV55
034400971118     C                   MOVEL     'C'           D55TLA
034500971118     C                   CALL      'FNLV55R'
034600971118     C                   PARM                    DSLV55
034700971118     C**
034800951128     C     FINE          TAG
034900010827     C                   MOVEL     DSTL42        KPJBU
035000951128     C                   SETON                                        LR
035100951129     C**************  CARICAMENTO INIZIALE SUBFILE  *******************
035200960215     C     RIEINI        BEGSR
035300951129     C* PULIZIA SUBFILE
035400951129     C                   SETOFF                                       2123
035500010827     C                   WRITE     TL42C01
035600951129     C                   SETON                                        2021
035700951129     C                   MOVE      *ZEROS        NRR1              5 0
035800951129     C                   CLEAR                   NRRSAV
035900960214     C                   CLEAR                   WNRR1
036000960214     C                   CLEAR                   V1CNBR
036100960214     C                   CLEAR                   W1CNBR
036200960214     C     V1CANN        IFEQ      *BLANKS
036300960214     C                   MOVE      'N'           V1CANN
036400960214     C                   MOVE      'N'           WCANN
036500960228     C                   MOVE      'T'           V1CBIS
036600960228     C                   MOVE      'T'           WCBIS
036700960307     C                   MOVE      'C'           V1CCNT
036800960307     C                   MOVE      'C'           WCCNT
036900960214     C                   END
037000960424     C* REPERISCO ULTIMO TRAINO CONFERMATO
037100960315    1C  N05V1CTRN        IFEQ      *ZEROS
037200960315     C     *HIVAL        SETLL     TNTST01L
037300960315     C                   READP(N)  TNTST01L                               30
037400960315    2C     *IN30         DOWEQ     *OFF
037500960315     C     TSTTRN        ANDGE     9000000
037600960424    3C     TSTATB        IFEQ      *BLANKS
037700960315     C                   MOVE      TSTTRN        V1CTRN
037800960315     C                   CLEAR                   WLBDA8
037900960315     C                   MOVE      TSTDTN        G08INV
038000960315     C                   MOVEL     '3'           G08ERR
038100960315     C                   CALL      'XSRDA8'
038200960315     C                   PARM                    WLBDA8
038300960315     C                   MOVE      G08DAT        V1CDTN
038400960315     C                   SETON                                        30
038500960315   X3C                   ELSE
038600960315     C                   READP(N)  TNTST01L                               30
038700960315    3C                   END
038800960315    2C                   ENDDO
038900960315    1C                   END
039000951129     C* POSIZIONAMENTO INIZIALE ARCHIVIO F.V. PER CARICAMENTO SUBFILE
039100951129     C     KFVS          SETLL     TNFVS02L
039200951129     C   01KFVS          READE     TNFVS02L                               30
039300951129     C  N01KFVS2         READE     TNFVS02L                               30
039400960214     C  N30              MOVEL     DSFVS         WDSFVS
039500960215     C  N30              EXSR      RIES01
039600951129     C* SE NON HO CARICATO NULLA SPENGO INDICATORE SFLDSP
039700951129     C     NRR1          IFEQ      *ZEROS
039800951129     C                   SETOFF                                       20
039900951129     C                   END
040000951129     C                   ENDSR
040100960213     C**************  CARICAMENTO SUBFILE  ****************************
040200960215     C     RIES01        BEGSR
040300960220     C                   CLEAR                   WLNPS
040400960214     C                   MOVEL     WDSFVS        DSFVS
040500951128     C                   Z-ADD     NRRSAV        NRR1
040600960314     C                   ADD       16            WNRR1
040700951222    1C     NRR1          DOWLT     WNRR1
040800951128     C     *IN30         ANDEQ     *OFF
040900960223     C                   SETOFF                                       44
041000960228     C     FVSATB        CABEQ     '*'           LEGGI
041100960228     C     V1CANN        IFEQ      'N'
041200960228     C     FVSATB        ANDNE     ' '
041300960228     C                   GOTO      LEGGI
041400960228     C                   END
041500960307     C  N05V1CBIS        IFEQ      'B'
041600960228     C     FVSBIS        ANDEQ     *BLANKS
041700960228     C     V1CBIS        OREQ      'N'
041800960228     C     FVSBIS        ANDNE     *BLANKS
041900960228     C                   GOTO      LEGGI
042000960228     C                   END
042100960307     C  N05V1CCNT        IFEQ      'C'
042200960307     C     FVSDKF        ANDEQ     *ZEROS
042300960307     C     V1CCNT        OREQ      'N'
042400960307     C     FVSDKF        ANDGT     *ZEROS
042500960307     C                   GOTO      LEGGI
042600960307     C                   END
042700960229     C*
042800951222     C                   MOVE      ' '           WTROV
042900951222     C* SE PARAMETRI DI INPUT > 0 VERIFICO CHE UNA DELLE FFA CARICATE
043000951222     C* SIA UNA DELLE LINEE DI SCARICO DEL F.V.
043100951222    2C     *IN01         IFEQ      *ON
043200951222     C     FVSLNA        LOOKUP    FFA                                    30
043300951222    3C     *IN30         IFEQ      *OFF
043400951222     C                   Z-ADD     1             II
043500951222     C                   MOVE      FFA(II)       W003A             3
043600951222    4C     *IN30         DOWEQ     *OFF
043700951222     C     FFA(II)       ANDNE     *ZEROS
043800951222     C     W003A         LOOKUP    FLP                                    30
043900951222     C  N30              ADD       1             II
044000951222     C  N30              MOVE      FFA(II)       W003A
044100951222     C   30              MOVE      '1'           WTROV
044200951222    4C                   ENDDO
044300951222   X3C                   ELSE
044400951222     C                   MOVE      '1'           WTROV             1
044500951222    3C                   END
044600951222    2C                   END
044700951128     C*
044800951222    2C     *IN01         IFEQ      *OFF
044900951222     C     WTROV         OREQ      '1'
045000951128     C                   MOVE      *BLANKS       V1SSCE
045100951128     C                   MOVE      FVSLNP        V1SLNP
045200951128     C                   MOVE      FVSNFV        V1SNFV
045300960220     C                   CLEAR                   V1DLNP
045400960219     C* A rottura, decodifico linea di partenza
045500960223    3C     FVSLNP        IFNE      WLNPS
045600951130     C     FVSLNP        CHAIN     AZORG01L                           30
045700960223    4C     *IN30         IFEQ      *OFF
045800951130     C     ORGFVA        ANDEQ     *BLANKS
045900960219     C                   MOVEL     ORGDES        V1DLNP
046000960220     C                   MOVEL     ORGDES        V1HLNP
046100960220     C                   ELSE
046200960220     C                   CLEAR                   V1HLNP
046300960223    4C                   END
046400960219     C                   MOVE      FVSLNP        WLNPS
046500960223    3C                   END
046600960223    3C                   SELECT
046700960123     C     FVSBIS        WHENEQ    'B'
046800960123     C                   MOVEL     'BIS'         V1SBIS
046900960123     C     FVSBIS        WHENEQ    'E'
047000960123     C                   MOVEL     'ECC'         V1SBIS
047100960123     C     FVSBIS        WHENEQ    ' '
047200960123     C                   CLEAR                   V1SBIS
047300960223    3C                   ENDSL
047400951128     C* Rovescio data foglio viaggio
047500951128     C                   CLEAR                   WLBDA8
047600951128     C                   MOVE      FVSDFV        G08INV
047700951128     C                   MOVEL     '3'           G08ERR
047800951128     C                   CALL      'XSRDA8'
047900951128     C                   PARM                    WLBDA8
048000951128     C                   MOVE      G08DAT        V1SDFV
048100951128     C                   MOVE      FVSLNA        V1SLNA
048200951128     C* Decodifico fil.finale arrivo
048300951128     C     FVSLNA        CHAIN     AZORG01L                           30
048400951222    3C     *IN30         IFEQ      *OFF
048500951128     C     ORGFVA        ANDEQ     *BLANKS
048600951128     C                   MOVEL     ORGDES        V1DLNA
048700951128     C                   ELSE
048800951128     C                   CLEAR                   V1DLNA
048900951222    3C                   END
049000960223    3C     FVSATB        IFEQ      *BLANKS
049100960214     C                   CLEAR                   V1SANN
049200960214     C                   ELSE
049300960214     C                   MOVE      'A'           V1SANN
049400960223    3C                   END
049500951128     C* SCRITTURA RECORD DI SFL
049600960223     C                   Z-ADD     1             W
049700960223     C                   MOVE      V1SLNP        W003A             3
049800960223     C     V1SNFV        LOOKUP    SNF(W)                                 30
049900960223     C     *IN30         IFEQ      *ON
050000960223     C     CAR(W)        ANDEQ     W003A
050100960223     C                   SETON                                        44
050200960223     C                   END
050300951128     C                   ADD       1             NRR1
050400010827     C                   WRITE     TL42S01
050500951222    2C                   END
050600960228     C     LEGGI         TAG
050700951128     C*
050800951128     C   01KFVS          READE     TNFVS02L                               30
050900951128     C  N01KFVS2         READE     TNFVS02L                               30
051000951222    1C                   ENDDO
051100951129     C* ACCENDO INDICATORE DI FINE LETTURA
051200951129     C   30              SETON                                        23
051300951128     C* SE HO CARICATO ALMENO UN RECORD DI SFL INCREMENTO SFLRCDNBR
051400951222    1C     NRR1          IFGT      NRRSAV
051500951128     C     NRRSAV        ADD       1             V1CNBR
051600951222    1C                   END
051700960214     C* SE FINE LETTURA MEMORIZZO SFLRCDNBR DELL'ULTIMA PAGINA
051800960214     C   30              Z-ADD     V1CNBR        W1CNBR
051900951128     C* MEMORIZZO NRR
052000951128     C                   Z-ADD     NRR1          NRRSAV
052100960214     C* MEMORIZZO DATI DELL'ULTIMO FV LETTO PER ESSERE SICURA DI AVERE
052200960214     C* IN LINEA QUESTO RECORD IN CASO DI ROLLUP
052300960214     C  N30              MOVEL     DSFVS         WDSFVS
052400960223     C                   SETOFF                                       44
052500960223     C* SE IN SEGUITO A CONFERMA PRECEDENTE NON SONO RIUSCITA AD
052600960223     C* ABBINARE TUTTI I FV, SOLO LA PRIMA VOLTA EMETTO MESSAGGIO
052700960223     C     WMSG          IFEQ      *BLANKS
052800960223     C                   XFOOT     SNF           W0070             7 0
052900960223     C     W0070         IFGT      *ZEROS
053000960223     C                   SETON                                        28
053100960223     C                   MOVEL     MSG(19)       V1ZMSG
053200960223     C                   MOVE      '1'           WMSG              1
053300960223     C                   END
053400960223     C                   END
053500951128     C                   ENDSR
053600960214     C**************  ANNULLAMENTO/RIPRISTINO FV   ********************
053700960214     C     ANNRIP        BEGSR
053800960214     C                   SETON                                        22
053900960214     C                   SETOFF                                       43
054000010827     C                   READC     TL42S01                                29
054100960214    1C     *IN29         DOWEQ     *OFF
054200960214     C* INTERRUZIONE ANNULLAMENTO/RIPRISTINO SE RICHIESTO L'ANNULL.
054300960214     C* DI UN FV GIA' ANNULLATO
054400960215     C     V1SSCE        IFEQ      'AN'
054500960214     C     V1SANN        ANDEQ     'A'
054600960214     C                   SETON                                        4328
054700960214     C                   MOVEL     MSG(7)        V1ZMSG
054800960214     C                   GOTO      ENDANN
054900960214     C                   END
055000960214     C* INTERRUZIONE ANNULLAMENTO/RIPRISTINO SE RICHIESTO IL RIPRISTINO
055100960214     C* DI UN FV NON ANNULLATO
055200960215     C     V1SSCE        IFEQ      'RP'
055300960214     C     V1SANN        ANDEQ     *BLANKS
055400960214     C                   SETON                                        4328
055500960214     C                   MOVEL     MSG(8)        V1ZMSG
055600960214     C                   GOTO      ENDANN
055700960214     C                   END
055800960215    2C     V1SSCE        IFEQ      'AN'
055900960215     C     V1SSCE        OREQ      'RP'
056000960219     C                   MOVE      V1SLNP        KLNP
056100960219     C                   MOVE      V1SNFV        KNFV
056200960214     C     KFVS1         CHAIN     TNFVS01L                           30
056300960214    3C     *IN30         IFEQ      *OFF
056400960215    4C     V1SSCE        IFEQ      'AN'
056500960214     C                   MOVE      'A'           FVSATB
056600960214     C                   UPDATE    TNFVS000
056700960214     C                   MOVE      'A'           V1SANN
056800960214   X4C                   ELSE
056900960214     C                   MOVE      *BLANKS       FVSATB
057000960214     C                   UPDATE    TNFVS000
057100960214     C                   CLEAR                   V1SANN
057200960214    4C                   END
057300960214    3C                   END
057400960214     C                   CLEAR                   V1SSCE
057500960214    2C                   END
057600960214     C     ENDANN        TAG
057700960214     C   28              Z-ADD     NRR1          V1CNBR
057800010827     C                   UPDATE    TL42S01
057900960214     C   28              SETON                                        29
058000010827     C  N28              READC     TL42S01                                29
058100960214    1C                   ENDDO
058200960214     C                   ENDSR
058300960320     C*******  PRIMO GIRO LETTURA SUBFILE PER VISUALIZZAZIONE FV ******
058400960320     C     CTRVIS        BEGSR
058500960320     C                   SETON                                        22
058600960320     C                   MOVE      *BLANKS       WVIS              1
058700010827     C                   READC     TL42S01                                29
058800960320     C     *IN29         DOWEQ     *OFF
058900960320     C     V1SSCE        IFEQ      'FV'
059000960320     C                   MOVE      '1'           WVIS
059100960320     C                   CLEAR                   DSTL37
059200960320     C                   MOVEL     'R'           D37FLG
059300960320     C                   MOVE      V1SLNP        D37LNP
059400960320     C                   MOVE      V1SNFV        D37NFV
059500960320     C                   MOVEL     DSTL37        KPJBU
059600960320     C                   CALL      'TNTL37R'
059700960320     C                   PARM                    KPJBA
059800960320     C                   CLEAR                   V1SSCE
059900960320     C                   END
060000010827     C                   UPDATE    TL42S01
060100010827     C                   READC     TL42S01                                29
060200960320     C                   ENDDO
060300960320     C                   ENDSR
060400960213     C*******  CONTROLLI SUBFILE MODO ELENCO FOGLI DA ABBINARE  *******
060500960213     C     CT1S01        BEGSR
060600960219     C* N.B.: NON CONTROLLO CHE IL CAMPO SCELTA SIA=1
060700960219     C*       E' POSSIBILE SCEGLIERE CON QUALUNQUE CARATTERE.
060800960219     C*       TUTTAVIA, A VIDEO VIENE INDICATO 1 COME CARATTERE DI
060900960219     C*       SCELTA SEMPLICEMENTE PER PROPORRE QUALCOSA.
061000010827     C                   READC     TL42S01                                29
061100960213    2C     *IN29         DOWEQ     *OFF
061200960214     C     V1SSCE        ANDEQ     *BLANKS
061300010827     C                   READC     TL42S01                                29
061400960213    2C                   ENDDO
061500960213    2C     *IN29         IFEQ      *OFF
061600960213     C* Se scelto f.v. imposto parametri di output
061700010828     C                   MOVE      V1SLNP        D42LNP
061800010828     C                   MOVE      V1SNFV        D42NFV
061900960213     C                   ELSE
062000960213     C* Se f.v non è stato scelto riemetto sfl con msg
062100960213     C                   MOVEL     MSG(2)        V1ZMSG
062200960213     C                   SETON                                        28
062300960213    2C                   END
062400960213     C                   ENDSR
062500960213     C*******  CONTROLLI SUBFILE MODO ELENCO CONFERMA FV NON ABB. *****
062600960213     C     CT2S01        BEGSR
062700960213     C                   SETON                                        22
062800960214     C                   SETOFF                                       43
062900960213     C                   CLEAR                   LNP
063000960213     C                   CLEAR                   SCE
063100960215     C                   CLEAR                   WSCE
063200960216     C                   CLEAR                   WPDR
063300960216     C                   CLEAR                   WDPD
063400960216     C                   CLEAR                   WIPT
063500960320     C                   CLEAR                   WIPT1
063600010827     c                   clear                   wdiv
063700010827     c                   clear                   wdiv1
063800960424     C                   CLEAR                   WBIS1
063900960219     C                   CLEAR                   WTMZ
064000960319     C                   CLEAR                   WFES
064100960228     C                   CLEAR                   WPDR1
064200960228     C                   CLEAR                   WDPD1
064300960228     C                   CLEAR                   WTMZ1
064400960319     C                   CLEAR                   WFES1
064500960213     C                   CLEAR                   NRR
064600960213     C                   CLEAR                   X
064700960214     C* Pulizia ds Multipla
064800960213     C                   Z-ADD     1             II                3 0
064900960215    1C     II            DOWLE     100
065000960215     C     II            OCCUR     DSFIL
065100960216     C                   CLEAR                   NFV
065200960216     C                   CLEAR                   FSC
065300960216     C                   CLEAR                   FVG
065400960216     C                   CLEAR                   DDRP
065500960216     C                   CLEAR                   DHRP
065600960216     C                   CLEAR                   DLNA
065700960213     C                   ADD       1             II
065800960214    1C                   ENDDO
065900960213     C*
066000010827     C                   READC     TL42S01                                29
066100960213    1C     *IN29         DOWEQ     *OFF
066200960213    2C     V1SSCE        IFNE      *BLANKS
066300960214     C* Errore se campo scelta contiene caratteri alfabetici diversi da
066400960320     C* 'AN' o 'RP'
066500960219    3C     V1SSCE        IFNE      'AN'
066600960215     C     V1SSCE        ANDNE     'RP'
066700960219     C                   TESTN                   V1SSCE               30
066800960219    4C     *IN30         IFEQ      *ON
066900960219     C                   MOVE      V1SSCE        W001A             1
067000960219    5C     W001A         IFLT      '0'
067100960219     C                   SETOFF                                       30
067200960219    5C                   END
067300960219    4C                   END
067400960219    4C     *IN30         IFEQ      *OFF
067500960219     C                   SETON                                        4328
067600960219     C                   MOVEL     MSG(3)        V1ZMSG
067700960219     C                   GOTO      AGGIO
067800960219    4C                   END
067900960219    3C                   END
068000960320     C* Se scelta = 'AN' o 'RP' ignoro e clearo campo di scelta
068100960215    3C     V1SSCE        IFEQ      'AN'
068200960215     C     V1SSCE        OREQ      'RP'
068300960214     C                   CLEAR                   V1SSCE
068400960214    3C                   END
068500960228     C     V1SSCE        IFEQ      '00'
068600960228     C                   MOVE      *BLANKS       V1SSCE
068700960228     C                   END
068800960214     C* ELABORO RECORDS CON NUMERO SCELTA >0
068900960228    3C     V1SSCE        IFGT      '00'
069000960214     C* Errore se attribuito un numero scelta ad un FV annullato
069100960214    4C     V1SANN        IFEQ      'A'
069200960214     C                   SETON                                        4328
069300960214     C                   MOVEL     MSG(10)       V1ZMSG
069400960214     C                   GOTO      AGGIO
069500960214    4C                   END
069600960214     C* Errore se già immesso questo numero scelta
069700960213     C     V1SSCE        LOOKUP    SCE                                    30
069800960214    4C     *IN30         IFEQ      *ON
069900960213     C                   SETON                                        4328
070000960213     C                   MOVEL     MSG(4)        V1ZMSG
070100960213     C                   GOTO      AGGIO
070200960214    4C                   END
070300960214     C* Errore se già scelto un fv con questa lnp
070400960214     C                   MOVE      V1SLNP        W003A             3
070500960214     C     W003A         LOOKUP    LNP                                    30
070600960214    4C     *IN30         IFEQ      *ON
070700960213     C                   SETON                                        4328
070800960215     C                   MOVEA     MSG(5)        W78
070900960215     C                   MOVE      V1SLNP        W003A             3
071000960215     C                   MOVEA     W003A         W78(50)
071100960220     C                   MOVEA     V1HLNP        W78(54)
071200960215     C                   MOVEA     W78           V1ZMSG
071300960213     C                   GOTO      AGGIO
071400960214    4C                   END
071500960214     C* SE OK MEMORIZZO i dati del foglio viaggio e numero scelta
071600960219     C                   MOVE      V1SLNP        KLNP
071700960219     C                   MOVE      V1SNFV        KNFV
071800960219     C     KFVS1         CHAIN     TNFVS01L                           30
071900010828     C     *IN30         IFEQ      *OFF
072000011012     c                   eval      flgged = '0'
072100010828      * aggancio il file delle estensioni foglio viaggio per gli importi in
072200010907      * divisa (in questo momento mi serve solo IPT
072300010910     c                   clear                   fwsipt
072400010910     c                   clear                   fwsvip
072500010828     c     kfws          chain     tnfws01l
072600010907      * se l'importo dell'estensione è = 0 e l'importo sul file
072700010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
072800010907      * la divisa è lire
072900010907     c                   if        fwsipt = *zeros and fvsipt <> 0
073000010907     c                   eval      fwsvip = 'ITL'
073100010907     c                   z-add     fvsipt        fwsipt
073200010907     c                   endif
073300010907      * se l'importo dell'estensione è = 0 e l'importo sul file
073400010907      * principale è = 0, imposto la divisa a blank per poi prendere quella che
073500010907      * mi serve caso per caso
073600010907     c                   if        fwsipt = *zeros and fvsipt = 0
073700010907     c                   z-add     fvsipt        fwsipt
073800010907     c                   clear                   fwsvip
073900010907     c                   endif
074000010907
074100010907      * in questo caso devo emettere la divisa a video quindi se vuota devo prendere
074200010907      * la moneta corrente §GEDCN
074300010907     c                   if        fwsvip = *blanks
074400010907     c                   eval      fwsvip = §gedcn
074500011012     c                   eval      flgged = '1'
074600010907     c                   endif
074700010828
074800960213     C                   ADD       1             X                 3 0
074900960213     C                   MOVE      V1SLNP        LNP(X)
075000960213     C                   MOVE      V1SSCE        SCE(X)
075100960216     C                   MOVE      FVSDFV        DFV(X)
075200960213     C                   MOVE      NRR1          NRR(X)
075300960215     C     X             OCCUR     DSFIL
075400960216     C                   MOVE      V1SNFV        NFV
075500960325     C                   MOVE      FVSLNA        FSC(1)
075600960325     C                   MOVE      FVSLNA        FVG(1)
075700960325     C                   MOVEA     FLP           FSC(2)
075800960325     C                   MOVEA     FFV           FVG(2)
075900960216     C                   MOVE      FVSDRP        DDRP
076000960216     C                   MOVE      FVSHRP        DHRP
076100960216     C                   MOVE      FVSLNA        DLNA
076200960216     C* MEMORIZZO NUMERO SCELTA PIù BASSO ED I SEGUENTI DATI:
076300960215     C     V1SSCE        IFLT      WSCE
076400960215     C     WSCE          OREQ      *BLANKS
076500960215     C                   MOVE      V1SSCE        WSCE
076600960228     C                   MOVE      FVSPDR        WPDR1
076700960228     C                   MOVEL     FVSDPD        WDPD1
076800960228     C                   MOVE      FVSTMZ        WTMZ1
076900960319     C                   MOVE      FVSLNP        WFES1
077000010828     C**!!!              MOVE      FVSIPT        WIPT1
077100010828     c                   z-add     fwsipt        wipt1
077200010828     c                   movel     fwsvip        wdiv1
077300960424     C                   MOVE      FVSBIS        WBIS1
077400960320     C                   END
077500960228     C* SOLO SE DIVERSI DA 0
077600010828     C**!!!FVSIPT        IFGT      *ZEROS
077700010828     C**!!!WIPT          ANDEQ     *ZEROS
077800010828     c                   if        fwsipt > 0 and wipt = 0
077900960216     C                   MOVE      FVSPDR        WPDR
078000960216     C                   MOVEL     FVSDPD        WDPD
078100010828     C**!!!              MOVE      FVSIPT        WIPT
078200010828     c                   z-add     fwsipt        wipt
078300010828     c                   move      fwsvip        wdiv
078400960219     C                   MOVE      FVSTMZ        WTMZ
078500960319     C                   MOVE      FVSLNP        WFES
078600960228     C                   END
078700960216     C                   END
078800960214    3C                   END
078900960214    2C                   END
079000960213     C     AGGIO         TAG
079100960214     C   28              Z-ADD     NRR1          V1CNBR
079200010827     C                   UPDATE    TL42S01
079300960213     C   28              SETON                                        29
079400010827     C  N28              READC     TL42S01                                29
079500960213    1C                   ENDDO
079600960215     C   28              GOTO      ECTS01
079700960214     C* CONTROLLI FINALI
079800960214     C* Errore se non è stato scelto nemmeno un foglio viaggio
079900960215    1C     WSCE          IFEQ      *BLANKS
080000960213     C                   MOVEL     MSG(2)        V1ZMSG
080100960213     C                   SETON                                        28
080200960215     C                   GOTO      ECTS01
080300960215    1C                   END
080400960214     C* Se non trovati errori eseguo ulteriori controlli
080500960215     C* Errore se date fogli viaggio scelti non in sequenza
080600960215     C                   MOVEA     SCE           SSC
080700960215     C                   SORTA     SSC
080800960215     C                   MOVE      *ZEROS        WDFV
080900960213     C                   Z-ADD     1             X
081000960215     C     WSCE          LOOKUP    SSC(X)                                 30
081100960215     C* Memorizzo indice di schiera del primo FV
081200960215     C                   MOVE      X             WFIRST
081300960215    1C     X             DOWLE     100
081400960215     C                   Z-ADD     1             II
081500960215     C     SSC(X)        LOOKUP    SCE(II)                                30
081600960216    2C     WDFV          IFNE      *ZEROS
081700960216     C     DFV(II)       ANDLT     WDFV
081800010827     C     NRR(II)       CHAIN     TL42S01                            30
081900960215     C                   SETON                                        2843
082000960215     C                   MOVEL     MSG(9)        V1ZMSG
082100960215     C                   Z-ADD     NRR(II)       V1CNBR
082200010827     C                   UPDATE    TL42S01
082300960215     C                   GOTO      ECTS01
082400960216    2C                   END
082500960219     C* MEMORIZZO DATA TRAINO
082600960219     C     WDFV          IFEQ      *ZEROS
082700960219     C                   MOVE      DFV(II)       WDTN
082800960219     C                   END
082900960219     C* SE DATA FOGLIO VIAGGIO DIVERSA DA DATA TRAINO CONTROLLO CHE
083000960219     C* NON SIA UN GIORNO FESTIVO
083100960219     C     WDTN          IFNE      DFV(II)
083200960220     C                   MOVE      DFV(II)       W0080             8 0
083300960219     C                   EXSR      CTRFES
083400960319     C     WFEST         IFNE      *BLANKS
083500010827     C     NRR(II)       CHAIN     TL42S01                            30
083600960219     C                   SETON                                        2843
083700960219     C                   MOVEL     MSG(16)       V1ZMSG
083800960219     C                   Z-ADD     NRR(II)       V1CNBR
083900010827     C                   UPDATE    TL42S01
084000960219     C                   GOTO      ECTS01
084100960219     C                   END
084200960219     C                   END
084300960215     C                   MOVE      DFV(II)       WDFV
084400960215     C                   ADD       1             X
084500960215    1C                   ENDDO
084600960214     C* Errore se nei FV scelti esiste almeno una filiale di scarico
084700960214     C* = alla lnp del primo FV
084800960215     C                   Z-ADD     1             X
084900960215     C     WSCE          LOOKUP    SCE(X)                                 30
085000960213     C                   Z-ADD     1             II
085100960215    1C     II            DOWLE     100
085200960215     C     LNP(II)       ANDNE     *BLANKS
085300960213     C* NON LEGGO RICORRENZA RELATIVA AL PRIMO FV
085400960215    2C     II            IFNE      X
085500960215     C     II            OCCUR     DSFIL
085600960216     C     LNP(X)        LOOKUP    FSC                                    30
085700960215    3C     *IN30         IFEQ      *ON
085800010827     C     NRR(II)       CHAIN     TL42S01                            30
085900960213     C                   SETON                                        4328
086000960213     C                   MOVEL     MSG(6)        V1ZMSG
086100960214     C                   Z-ADD     NRR(II)       V1CNBR
086200010827     C                   UPDATE    TL42S01
086300960215     C                   GOTO      ECTS01
086400960215    3C                   END
086500960215    2C                   END
086600960213     C                   ADD       1             II
086700960215    1C                   ENDDO
086800960215     C     ECTS01        ENDSR
086900960215     C***** CARICAM. VIDEATA PARAMETRI PER CREAZIONE TRAINO ECCEZ. ****
087000960215     C     RIED02        BEGSR
087100960215     C* CHAINO RECORD SUBFILE DEL 1° FV DEL TRAINO PER REPERIRE DATI
087200960215     C* DA RIPORTARE IN SECONDA VIDEATA
087300960215     C                   SETON                                        22
087400010827     C     NRR(X)        CHAIN     TL42S01                            30
087500010827     C                   UPDATE    TL42S01
087600960215     C                   MOVE      V1SLNP        V2CLNP
087700960215     C                   MOVE      V1SNFV        V2CNFV
087800960220     C                   MOVEL     V1HLNP        V2DLNP
087900960215     C                   MOVEL     V1SBIS        V2CBIS
088000960215     C                   MOVEL     V1SDFV        V2CDFV
088100960215     C                   MOVE      V1SLNA        V2CLNA
088200960215     C                   MOVEL     V1DLNA        V2DLNA
088300960215     C* NUMERO TRAINO
088400960220     C     *LOCK         IN        TNTL02                               30
088500960220     C* Se data area allocata torno in prima videata con msg
088600960220     C     *IN30         IFEQ      *ON
088700960220     C                   SETON                                        28
088800960220     C                   MOVEL     MSG(18)       V1ZMSG
088900960220     C                   GOTO      ERIED2
089000960220     C                   END
089100960215     C     TNTL02        ADD       1             TNTL02
089200960215     C                   OUT       TNTL02
089300960215     C                   Z-ADD     TNTL02        V2CTRN
089400960215     C* DATA TRAINO = DATA DEL 1° FV
089500960215     C                   MOVE      V1SDFV        V2CDTN
089600010827
089700960422     C* PRENDO IMPORTO E ALTRI DATI DAL PRIMO FV SCELTO SE IMPORTO
089800960422     C* DIVERSO DA 0 O SE TUTTI IMPORTI = 0  ALTRIMENTI PRENDO IMPORTO
089900960422     C* E ALTRI DATI DAL PRIMO DIVERSO DA 0
090000960422     C     WIPT1         IFGT      *ZEROS
090100960422     C     WIPT          OREQ      *ZEROS
090200960228     C                   MOVE      WPDR1         WPDR
090300960228     C                   MOVE      WDPD1         WDPD
090400960228     C                   MOVE      WTMZ1         WTMZ
090500960319     C                   MOVE      WFES1         WFES
090600010828     C**!!!              MOVE      WIPT1         WIPT
090700010828     C                   z-add     WIPT1         WIPT
090800010828     c                   move      wdiv1         wdiv
090900960228     C                   END
091000960312     C**** TRAZIONISTA
091100960216     C     WPDR          IFGT      *ZEROS
091200960216     C                   MOVE      WPDR          V2CPDR
091300960215     C                   ELSE
091400960215     C                   MOVE      '      ?'     V2CPDR
091500960215     C                   END
091600960216     C                   MOVEL     WDPD          V2DDPD
091700010828      * divisa
091800010828     c                   move      wdiv          v2cdiv
091900011001     c                   move      wdiv          sav_v2cdiv
092000960312     C**** IMPORTO DA CONFERMARE
092100010828     C**!!!              MOVE      WIPT          V2CICO
092200010828     C                   z-add     WIPT          V2CICO
092300960215     C**** CAUSALE CONTABILIZZAZIONE  (PRENDO DA TABELLA TW)
092400960215     C                   MOVE      'TW'          KCOD
092500960215     C                   MOVEL(P)  1             KKEY
092600960215     C     KTAB          CHAIN     TABEL00F                           30
092700960215    1C     *IN30         IFEQ      *OFF
092800960215     C                   MOVEL     TBLUNI        V2CCAU
092900960215     C                   MOVEL     TBLUNI        WCAU
093000000211     C* Reperisco i dati della causale di contabilizzazione
093100000218     C* dalle tabelle di PROJ
093200000218     C                   MOVE      KNSIF         WCOMO4            4
093300000218     C                   MOVEL     WCOMO4        WSOC              3
093400000218     C                   movel     WSOC          KAtazi
093500000218     C                   MOVEL     'Y4Z'         Katcod
093600000218     C                   MOVEL     *blanks       Katkey
093700000218     C                   MOVEL(P)  V2CCAU        Katkey
093800000218     C     KEATAB        CHAIN     ANATB01L                           30
093900000218     C                   MOVE      '888'         Katazi
094000000218     C     KEATAB        CHAIN     ANATB01L                           30
094100000218     C                   if        not *in30
094200000218     C                   MOVEL     ATBUNI        V2DCAU
094300000211     C                   else
094400000211     C                   MOVE      *BLANKS       V2DCAU
094500000211     C                   end
094600960215     C                   MOVEL     V2DCAU        WDCAU
094700960215   X1C                   ELSE
094800960215     C                   CLEAR                   V2CCAU
094900960215     C                   CLEAR                   WCAU
095000960215     C                   CLEAR                   V2DCAU
095100960215     C                   CLEAR                   WDCAU
095200960215    1C                   END
095300960220     C     ERIED2        ENDSR
095400010827     C**********   CONTROLLI VIDEATA TL42D02   ************************
095500960215     C     CTRD02        BEGSR
095600960215     C                   SETOFF                                       404142
095700010828     c                   setoff                                       45
095800960215     C****  CODICE TRAZIONISTA
095900960215     C     '?'           SCAN      V2CPDR                                 30
096000960215    1C     *IN30         IFEQ      *ON
096100000807     C                   EVAL      PA3TIP = 'T '                                PAR-1
096200000807     C                   MOVEL(P)  V2DDPD        PA3RSC                         PAR-2
096300000807     C                   CLEAR                   PA3CSF                         PAR-3
096400000807     C                   CLEAR                   PA3PDR                         PAR-4
096500000807     C                   CLEAR                   PA3FLG                         PAR-5
096600960215     C                   MOVEL     PARAM3        KPJBU
096700960215     C                   CALL      'TRUL16R'
096800960215     C                   PARM                    KPJBA
096900960215     C                   MOVEL     KPJBU         PARAM3
097000000807     C                   MOVEL     PA3PDR        V2CPDR                         CODICE
097100000807     C                   MOVEL     PA3RSC        V2DDPD                         RAG.SOCIALE
097200960215     C* PA3FLG = "3"-->NON TROVATI TRAZIONISTI PER LA CHIAVE ALFABETICA
097300960215     C*                RICHIESTA
097400960215    2C     PA3FLG        IFEQ      '3'
097500960215     C                   MOVEL     MSG(11)       V2ZMSG
097600960215     C                   SETON                                        2840
097700960215     C                   GOTO      ECTR02
097800960215    2C                   ENDIF
097900960215    1C                   END
098000960215     C* TRAZIONISTA = BLANKS ---> RIPROPONGO QUELLO DEL FV
098100960215    1C     V2CPDR        IFEQ      *ZEROS
098200960215     C     V2CPDR        OREQ      *BLANKS
098300960215     C     V2DDPD        ANDEQ     *BLANKS
098400960216     C                   MOVE      WPDR          V2CPDR
098500960216     C                   MOVE      WDPD          V2DDPD
098600960215    1C                   END
098700960215     C* CODICE TRAZIONISTA OBBLIGATORIO
098800960215    1C     V2CPDR        IFEQ      *ZEROS
098900960215     C     V2CPDR        OREQ      *BLANKS
099000960215     C                   SETON                                        2840
099100960215     C                   MOVEL     MSG(12)       V2ZMSG
099200960215     C                   GOTO      ECTR02
099300960215    1C                   END
099400960215     C* CONTROLLO VALIDITA' COD.TRAZIONISTA
099500960215     C                   TESTN                   V2CPDR               30
099600960215    1C     *IN30         IFEQ      *ON
099700960215     C                   MOVE      V2CPDR        W001A             1
099800960215    2C     W001A         IFLT      '0'
099900960215     C                   SETOFF                                       30
100000960215    2C                   END
100100960215    1C                   END
100200960215    1C     *IN30         IFEQ      *OFF
100300960215     C                   MOVEL     MSG(13)       V2ZMSG                                     RORE
100400960215     C                   SETON                                        2840
100500960215     C                   CLEAR                   V2DDPD
100600960215     C                   GOTO      ECTR02
100700960215    1C                   END
100800960215     C*
100900960215     C                   MOVE      V2CPDR        KPDR
101000000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
101100960215     C*
101200960215    1C     *IN31         IFEQ      *ON
101300960215     C     TLZATB        ORNE      ' '
101400960215     C                   MOVEL     MSG(13)       V2ZMSG
101500960215     C                   CLEAR                   V2DDPD
101600960215     C                   SETON                                        2840
101700960215     C                   GOTO      ECTR02
101800960215   X1C                   ELSE
101900960215     C                   MOVEL     TLZRSC        V2DDPD
102000960215    1C                   ENDIF
102100010827
102200960215     C**** IMPORTO DA CONFERMARE
102300960215     C* IMPORTO = 0 --> RIPROPONGO QUELLO DEL FV
102400011012     C                   if        v2cico = 0
102500011012     c                             and v2cdiv =*blanks
102600011012     c                             and flgged = *off
102700010828     C**!!!              MOVE      WIPT          V2CICO
102800010828     C                   z-add     WIPT          V2CICO
102900011012     c                   move      wdiv          v2cdiv
103000960215     C                   END
103100960215    1C     V2CICO        IFEQ      *ZEROS
103200960215     C                   SETON                                        2841
103300960215     C                   MOVEL     MSG(14)       V2ZMSG
103400960215     C                   GOTO      ECTR02
103500960215    1C                   END
103600171212     c
103700171212     C                   MLLZO     1             v2cico
103800011012
103900011012      * d i v i s a
104000011012      * divisa = ' '---> RIPROPONGO QUELLA DEL FV
104100011012     c                   if        v2cdiv =*blanks
104200011012     c                             and flgged = *off and v2cico = 0
104300011012     c                   move      wdiv          v2cdiv
104400011012     c                   endif
104500011012      * obbligatoria
104600011012     c                   if        v2cdiv = *blanks
104700011012     c                   seton                                        2845
104800011012     c                   movel     msg(20)       v2zmsg
104900011012     c                   goto      ectr02
105000011012     c                   endif
105100011012      * deve essere tabellata
105200011012     c                   clear                   dscv
105300011012     c                   movel     'CV'          kcod
105400011012     c                   movel(p)  v2cdiv        kkey
105500011012     c     ktab          chain     tabel00f
105600011012     c                   if        not%found(tabel00f)
105700011012     c                   seton                                        2845
105800011012     c                   movel     msg(21)       V2ZMSG
105900011012     c                   goto      ectr02
106000011012     c                   endif
106100011012      * deve essere ammessa nei traini
106200011012     c                   movel     tbluni        dscv
106300011012     c                   if        §cvtra <> 'S'
106400011012     c                   seton                                        2845
106500011012     c                   movel     msg(22)       V2ZMSG
106600011012     c                   goto      ectr02
106700011012     c                   endif
106800011012      * se modificata la divisa e non l'importo devo dare msg di errore
106900011012     c                   if        v2cdiv <> sav_v2cdiv
107000011012     c                             and *in66 = *off
107100011012     c                             and v2cico <> *zeros
107200011012     c                             and sav_v2cdiv <> *blanks
107300011012     c                   movel     msg(24)       v2zmsg
107400011012     c                   seton                                        2841
107500011012     c                   goto      ectr02
107600011012     c                   endif
107700011012
107800011012     c                   movel     v2cdiv        sav_v2cdiv
107900010828      * controllo se ok i decimali
108000010828     c                   move      v2cico        w_decimali
108100010828     c                   if        w_decimali > 0  and
108200010828     c                             §cvfdc <> 'S'
108300010828     c                   seton                                        2841
108400010828     c                   movel     msg(23)       v2zmsg
108500010828     c                   goto      ectr02
108600010828     c                   endif
108700010827
108800960215     C**** CAUSALE CONTABILIZZAZIONE
108900000218    1C***  V2CCAU        IFEQ      '?'
109000000218     C***                RESET                   XTABDS
109100000218     C***                MOVE      *BLANK        XTAKEY
109200000218     C***                MOVE      '01'          XTAOPZ
109300000218     C***                MOVE      xscsoc        XTAAZI
109400000218     C***                MOVE      XSCLIN        XTALIN
109500000218     C***                CLEAR                   KPJBU
109600000218     C***                MOVEL     XTABDS        KPJBU
109700000218     C***                CALL      'ANGY4ZR'
109800000218     C***                PARM                    KPJBA
109900000218     C***                MOVEL     KPJBU         XTABDS
110000000218     C***                MOVEL     XTAKEY        V2CCAU                         *CODICE
110100000218    1C***                END
110200960220    1C     V2CCAU        IFEQ      *BLANKS
110300960215     C                   MOVEL     WCAU          V2CCAU
110400960215     C                   MOVEL     WDCAU         V2DCAU
110500960220    1C                   END
110600960220    1C     V2CCAU        IFEQ      *BLANKS
110700960215     C                   SETON                                        2842
110800960215     C                   MOVEL     MSG(15)       V2ZMSG
110900960215     C                   GOTO      ECTR02
111000960220    1C                   END
111100000211     C* Reperisco i dati della causale di contabilizzazione
111200000218     C                   MOVE      KNSIF         WCOMO4            4
111300000218     C                   MOVEL     WCOMO4        WSOC              3
111400000218     C                   movel     WSOC          KAtazi
111500000218     C                   MOVEL     'Y4Z'         Katcod
111600000218     C                   MOVEL     *blanks       Katkey
111700000218     C                   MOVEL(P)  V2CCAU        Katkey
111800000218     C     KEATAB        CHAIN     ANATB01L                           30
111900000218     C                   MOVE      '888'         Katazi
112000000218     C     KEATAB        CHAIN     ANATB01L                           30
112100000218     C                   if        not *in30
112200000218     C                   MOVEL     ATBUNI        V2DCAU
112300000211     C                   else
112400000211     C                   MOVE      *BLANKS       V2DCAU
112500000211     C                   MOVEL     MSG(17)       V2ZMSG
112600000211     C                   SETON                                        2842
112700960220    1C                   END
112800011001
112900011001     c     ectr02        tag
113000011001
113100011001     C                   ENDSR
113200960215     C**************  CONFERMA FV NON ABBINATI  ***********************
113300960215     C     CONFE         BEGSR
113400960219     C*
113500960219     C* DETERMINO GIORNI INIZIO SECOLO DI DATA TRAINO
113600960219     C                   CLEAR                   WLBDA8
113700960219     C                   MOVE      WDTN          G08INV
113800960219     C                   MOVE      '3'           G08ERR
113900960219     C                   CALL      'XSRDA8'
114000960219     C                   PARM                    WLBDA8
114100960219     C                   MOVE      G08TGI        WTGI
114200960219     C* DETERMINO GIORNO DELLA SETTIMANA DI DATA TRAINO
114300960220     C                   MOVEL     G08DAT        WDAT6             6
114400960220     C                   MOVE      G08DAT        W002A             2
114500960220     C                   MOVE      W002A         WDAT6
114600960219     C                   CALL      'XGIOSE1'
114700960219     C                   PARM                    WDAT6
114800960219     C                   PARM                    GIOSET            1
114900960216     C* WFIRST è IL PRIMO ELEMENTO DIVERSO DA BLANKS IN SCHIERA
115000960216     C* ORDINATA DELLE SCELTE
115100960216     C                   MOVE      WFIRST        X
115200960216     C                   CLEAR                   CAR
115300960219     C                   CLEAR                   SNF
115400960223     C                   CLEAR                   WMSG
115500960216     C                   CLEAR                   PAR
115600960216     C                   CLEAR                   SCA
115700960216     C                   CLEAR                   ARR
115800960216     C                   MOVE      *ZEROS        Y                 3 0          INDICE CAR
115900960216     C                   MOVE      *ZEROS        WW                3 0          INDICE SCA
116000960216     C* CICLO LETTURA FV SCELTI IN BASE ALL'ORDINE RICHIESTO
116100960216    1C     X             DOWLE     100
116200960215     C                   Z-ADD     1             II
116300960215     C     SSC(X)        LOOKUP    SCE(II)                                30
116400960216     C* Memorizzo lnp del FV in schiera filiali di carico
116500960216     C                   ADD       1             Y
116600960216     C                   MOVE      LNP(II)       CAR(Y)
116700960216     C* CICLO LETTURA FILIALI DEL FV
116800960216     C     II            OCCUR     DSFIL
116900960216     C                   MOVEL     DDRP          PAR(Y)
117000960216     C                   MOVE      DHRP          PAR(Y)
117100960219     C                   MOVE      NFV           SNF(Y)
117200960216     C                   CLEAR                   WFSC
117300960216     C                   CLEAR                   WTARS
117400960216     C                   Z-ADD     1             Z                 3 0
117500960220     C                   MOVE      *HIVAL        W0120            12 0
117600960220     C                   MOVE      8             W0120
117700960325    2C     Z             DOWLE     150
117800960216     C     FSC(Z)        ANDNE     *BLANKS
117900960216     C* Routine di ricerca terminal di arrivo della filiale del FV
118000960216     C                   EXSR      SRTAR
118100960216     C* Gestione rottura terminal di arrivo
118200960216    3C     WTAR          IFNE      WTARS
118300960322     C     WTARS         IFGT      *ZEROS
118400960216     C* SCRITTURA METRI CUBI
118500960322     C                   MOVE      WFSC1         WTFA
118600960216     C                   EXSR      SRTLM
118700960322     C                   END
118800960322     C                   MOVE      FSC(Z)        WFSC1             3
118900960322     C                   MOVE      WTAR          WTARS
119000960322     C                   CLEAR                   WMCR
119100960216    3C                   END
119200960322     C                   MOVE      LNP(II)       WLNP
119300960322     C                   MOVE      FVG(Z)        WFVG
119400960322     C     KFVD          CHAIN     FNFVD01L                           30
119500960322     C     *IN30         IFEQ      *OFF
119600960322     C                   XFOOT     FVD           W0104            10 4
119700960322     C                   ADD       W0104         WMCR             10 4
119800960322     C                   END
119900960216     C* Gestione rottura filiale di scarico
120000960219    3C     FSC(Z)        IFNE      WFSC
120100960219     C                   MOVE      FSC(Z)        WFSC              3
120200960216     C                   Z-ADD     1             W                 3 0
120300960216     C* Se non ancora memorizzata la memorizzo adesso
120400960216     C     FSC(Z)        LOOKUP    SCA(W)                                 30
120500960216    4C     *IN30         IFEQ      *OFF
120600960216     C                   ADD       1             WW
120700960216     C                   MOVE      FSC(Z)        SCA(WW)
120800960220    4C                   END
120900960307    4C*****      *IN30     IFEQ *OFF
121000960307     C*****      ARR,W     OREQ W0120
121100960307     C* Memorizzo anche data/ora di arrivo
121200960307     C* (In caso di scelta di più di un fv memorizzo la data/ora arrivo
121300960307     C* più alta se ce n'è più di una)
121400960220     C                   MOVE      FSC(Z)        WSCA
121500960219     C                   MOVE      LNP(II)       WLNP
121600960220     C                   EXSR      SRARR
121700960307    4C*****                END
121800960216    3C                   END
121900960216     C                   ADD       1             Z
122000960216    2C                   ENDDO
122100960325     C*
122200960325     C     WTARS         IFGT      *ZEROS
122300960325     C                   MOVE      WFSC1         WTFA
122400960322     C                   EXSR      SRTLM
122500960322     C                   CLEAR                   WMCR
122600960325     C                   END
122700960325     C*
122800960215     C                   ADD       1             X
122900960216    1C                   ENDDO
123000960216     C* FINE CICLO LETTURA FOGLI VIAGGIO: SCRIVO FILE FERMATE (TNTLR)
123100960216     C                   EXSR      SRTLR
123200960219     C* SCRITTURA LISTINO
123300010828     C                   CLEAR                   TITLL000
123400960219     C                   MOVE      V2CTRN        TLLTRN
123500960219     C                   MOVE      WDTN          TLLDDE
123600960219     C                   MOVE      V2CPDR        TLLPDR
123700960219     C                   MOVE      WTMZ          TLLTMZ
123800010828     C**!!!              MOVE      V2CICO        TLLILI
123900010827     C**!!!              MOVE      V2CICO        TLLIES
124000010828     C                   z-add     V2CICO        TLLILI
124100010827     c                   z-add     *zeros        tllies
124200960219     C                   MOVEL     V2CCAU        TLLCAU
124300960219     C                   MOVE      WDATEU        TLLDIR
124400010827     c                   move      v2cdiv        tlldiv
124500010828     C                   WRITE     TITLL000
124600960216     C* SCRITTURA TESTATA TRAINO
124700960216     C                   CLEAR                   TNTLT000
124800960219     C                   MOVE      V2CTRN        TLTTRN
124900960219     C                   MOVE      WDTN          TLTDDE
125000960219     C                   MOVE      WDTN          TLTDSC
125100960219     C                   MOVE      CAR(1)        TLTTFP
125200960220     C                   MOVE      DLNA          TLTTFA
125300960219     C                   MOVE      'S'           TLTTTR
125400960220     C                   MOVE      GIOSET        G                 1 0
125500960220     C                   MOVE      GIOSET        GSE(G)
125600960424     C                   MOVEL     WBIS1         TLTBIS
125700960219     C                   MOVEL     WTMZ          TLTTMZ
125800960319     C                   MOVEL     WFES          TLTFES
125900960219     C                   MOVE      WDATEU        TLTDIR
126000960219     C                   WRITE     TNTLT000
126100960219     C* RICHIAMO PGM DI GESTIONE ANAGRAFICA TRAINI PER PERMETTERE
126200960219     C* DI COMPLETARE E/O CORREGGERE DATI MANCANTI E/O ERRATI
126300960219     C* (MANCANZA DI DATE DI ARRIVO, FERMATE DI SOLO SCARICO NON IN
126400960219     C*  ORDINE....)
126500960307     C                   MOVEL     'C02'         D00OP0                         *PARAMETRI FISSI
126600960219     C                   MOVEL     'O02'         D00OP1
126700960219     C                   MOVEL     '0'           D00F03
126800960219     C                   MOVEL     '0'           D00F12
126900960219     C                   MOVEL     '0'           D00ERR
127000960219     C                   MOVEL     *BLANKS       D00MSG
127100960219     C                   Z-ADD     V2CTRN        D00TRN                         *PARAMETRI SELEZ.
127200960219     C                   Z-ADD     WDTN          D00DDE
127300960219     C                   MOVEL     DS00          KPJBU
127400960219     C                   CALL      'TNTL01C'
127500960219     C                   PARM                    KPJBA
127600960219     C                   MOVEL     KPJBU         DS00
127700960219     C* SE PREMUTO F12 O F3 IL TRAINO NON è STATO CONFERMATO CANCELLO
127800960219     C* QUINDI DATI DA ANAGRAFICA
127900960219    1C     D00F03        IFEQ      '1'
128000960219     C     D00F12        OREQ      '1'
128100960219     C                   EXSR      DELTRN
128200960219   X1C                   ELSE
128300960219     C* CREO GRIGLIA TRAINO
128400960219     C                   CLEAR                   DSTL24
128500960219     C                   MOVE      WDTN          D24DTD
128600960219     C                   MOVE      WDTN          D24DTA
128700960219     C                   MOVE      V2CTRN        D24TRN
128800960315     C***                  MOVE CAR,1     D24TFP
128900960219     C                   MOVE      WDTN          D24DDE
129000960219     C                   MOVEL     DSTL24        KPJBU
129100960219     C                   CALL      'TNTL24R'
129200960219     C                   PARM                    KPJBA
129300960219     C* CICLO LETTURA FOGLI VIAGGIO PER ABBINAMENTO
129400960219     C                   Z-ADD     1             W
129500960219    2C     W             DOWLE     100
129600960219     C     SNF(W)        ANDGT     *ZEROS
129700960219     C                   MOVE      CAR(W)        KLNP
129800960219     C                   MOVE      SNF(W)        KNFV
129900960219     C     KFVS1         CHAIN     TNFVS01L                           30
130000960219    3C     *IN30         IFEQ      *OFF
130100010828
130200010828      * aggancio il file delle estensioni foglio viaggio per gli importi in
130300010828      * divisa
130400010910     c                   clear                   fwsipt
130500010910     c                   clear                   fwsies
130600010910     c                   clear                   fwsvip
130700010910     c                   clear                   fwsves
130800010828     c     kfws          chain     tnfws01l
130900010907      * se l'importo dell'estensione è = 0 e l'importo sul file
131000010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
131100010907      * la divisa è lire
131200010907     c                   if        fwsipt = *zeros and fvsipt <> 0
131300010907     c                   eval      fwsvip = 'ITL'
131400010907     c                   z-add     fvsipt        fwsipt
131500010907     c                   endif
131600010907     c                   if        fwsies = *zeros and fvsies <> 0
131700010907     c                   eval      fwsves = 'ITL'
131800010907     c                   z-add     fvsies        fwsies
131900010907     c                   endif
132000010907      * se l'importo dell'estensione è = 0 e l'importo sul file
132100010907      * principale è = 0, imposto la divisa a blank per poi prendere quella che
132200010907      * mi serve caso per caso
132300010907     c                   if        fwsipt = *zeros and fvsipt = 0
132400010907     c                   z-add     fvsipt        fwsipt
132500010907     c                   clear                   fwsvip
132600010907     c                   endif
132700010907     c                   if        fwsies = *zeros and fvsies = 0
132800010907     c                   z-add     fvsies        fwsies
132900010907     c                   clear                   fwsves
133000010907     c                   endif
133100010828
133200960308     C* Controlli di abbinabilità:
133300960308     C* ........Posso abbinare il fv solo se non risulta già abbinato
133400960219    4C     FVSTRS        IFEQ      *ZEROS
133500960308     C* ........richiamo routine che effettua ulteriori controlli di
133600960308     C*         abbinabilità
133700960308     C                   EXSR      CHKABB
133800960308    5C     WOKABB        IFEQ      '1'
133900960219     C                   EXSR      AGGSTO
134000960223     C                   CLEAR                   SNF(W)
134100960223     C                   CLEAR                   CAR(W)
134200960219     C                   MOVE      V2CTRN        FVSTRS
134300960228     C                   MOVE      V2CTRN        FVSTRN
134400960305     C                   MOVE      'A'           FVSTFM
134500960305     C                   MOVE      'A'           FVSTFS
134600960219     C                   UPDATE    TNFVS000
134700960322     C* Richiamo pgm che appoggia metri cubi reali
134800960322     C                   CLEAR                   DSTL33
134900960322     C                   MOVE      'L'           D33TLA
135000960322     C                   MOVE      FVSLNP        D33LNP
135100960322     C                   MOVE      FVSNFV        D33NFV
135200960322     C                   MOVE      TSRDTN        D33DTN
135300960322     C                   MOVEL     DSTL33        KPJBU
135400960322     C                   CALL      'TNTL33R'
135500960322     C                   PARM                    KPJBA
135600960219    5C                   END
135700960219    4C                   END
135800960219    3C                   END
135900960219     C                   ADD       1             W
136000960219    2C                   ENDDO
136100960319     C* VERIFICO SE POSSO IMPOSTARE FLAG DI CONFERMA IMPORTO DA MANUALE
136200960319     C* AD AUTOMATICO
136300960319     C     KANA          CHAIN     TNTST01L                           30
136400960319    2C     *IN30         IFEQ      *OFF
136500010828      * calcolo il delta e controllo la tolleranza
136600010828     c                   eval      sav_tstili = tstili
136700010828     c                   if        tstipt <> tstili and
136800010828     c                             tstili > 0
136900010828      * aggancio la tabella GEI per la tolleranza
137000010828     c                   clear                   dgei
137100010828     c                   clear                   tibs02ds
137200010828     c                   movel     'L'           t02tla
137300010828     c                   movel     'C'           t02mod
137400010828     c                   movel     knsif         t02sif
137500010828     c                   movel     'GEI'         t02cod
137600010828     c                   movel     tstdiv        t02ke1
137700010828     c                   call      'TIBS02R'
137800010828     c                   parm                    kpjba
137900010828     c                   parm                    tibs02ds
138000010828     c                   movel     t02uni        dgei
138100010828
138200010828     c                   eval      w_delta = (tstipt - tstili)
138300010910      * il delta deve essere come valore assoluto
138400010910     c                   mllzo     1             w_delta
138500010828     c                   if        w_delta <= §gettr
138600010828     c                   eval      sav_tstili = tstipt
138700010828     c                   endif
138800010828     c                   endif
138900010828
139000010828     C**!!!TSTILI        ANDEQ     TSTIPT
139100010828   2aC     sav_tstili    ifeq      TSTIPT
139200960319     C     TSTILI        ANDGT     *ZEROS
139300960319     C     TSTPDR        ANDGT     *ZEROS
139400960319     C     TSTCAU        ANDNE     *BLANKS
139500960319     C     KTSR4         SETLL     TNTSR04L                               30
139600960319    3C     *IN30         IFEQ      *OFF
139700960319     C                   MOVE      '1'           TSTCNF
139800960319     C                   EXCEPT    AGGTST
139900960319   X3C                   ELSE
140000960319     C                   UNLOCK    TNTST01L
140100960319    3C                   END
140200960319   X2C                   ELSE
140300960319     C                   UNLOCK    TNTST01L
140400010828   2aC                   END
140500960319    2C                   END
140600960219     C* RICHIAMO PGM CHE APPOGGIA DATE/ORE ARRIVO SU GRIGLIA
140700960219     C                   CALL      'TNTL26R'
140800960219     C                   PARM                    KPJBA
140900960315     C* IMPOSTO ULTIMO TRAINO CONFERMATO DEL SUBFILE CONTROL
141000960315     C                   MOVE      V2CTRN        V1CTRN
141100960315     C                   MOVE      V2CDTN        V1CDTN
141200960219     C* RICARICO IL SUBFILE DI SCELTA FOGLI VIAGGIO
141300960219     C                   EXSR      RIEINI
141400960219    1C                   END
141500960215     C*
141600960215     C                   ENDSR
141700960308     C**********  CONTROLLO ABBINABILITA DEL FV AL TRAINO   ***********
141800960308     C     CHKABB        BEGSR
141900960308     C                   SETOFF                                       31
142000960308     C                   MOVE      ' '           WOKABB            1
142100960308     C     KTSR3         CHAIN     TNTSR03L                           30
142200960308     C* IL FOGLIO VIAGGIO E' ABBINABILE SOLO SE:
142300960308     C*                  1-ESISTE TRATTA CON FILIALE FERMATA PARTENZA
142400960308     C*                    = LNP DEL FV
142500960308    5C     *IN30         DOWEQ     *OFF
142600960308     C     *IN31         ANDEQ     *OFF
142700960308     C                   MOVEL     TSRFFA        W003A             3
142800960308     C     W003A         LOOKUP    FLP                                    31
142900960308     C  N31FVSLNA        COMP      TSRFFA                                 31
143000960308     C  N31KTSR3         READE     TNTSR03L                               30
143100960308    5C                   ENDDO
143200960308     C*                  2-ALMENO UN FILIALE DI SCARICO DEL FV è = ALLA
143300960308     C*                    FILIALE FERMATA DI ARRIVO DELLA TRATTA
143400960308     C   31              MOVE      '1'           WOKABB
143500960308     C                   ENDSR
143600960220     C**********  RICERCA E MEMORIZZAZIONE DATA/ORA ARRIVO  ***********
143700960220     C     SRARR         BEGSR
143800960220     C     KFV2          CHAIN     FNFV201L                           31
143900960307     C* TROVATA DATA/ORA DI ARRIVO
144000960307    1C     *IN31         IFEQ      *OFF
144100960307     C* 30 on --> Avevo già memorizzato una data/ora di arrivo:
144200960307     C*           mantengo la più alta
144300960307    2C     *IN30         IFEQ      *ON
144400960307    3C     DATORA        IFGT      ARR(W)
144500960307     C     ARR(W)        OREQ      W0120
144600960307     C                   Z-ADD     DATORA        ARR(W)
144700960307    3C                   END
144800960307   X2C                   ELSE
144900960307     C* 30 off --> memorizzo la data/ora arrivo letta
145000960307     C                   Z-ADD     DATORA        ARR(WW)
145100960307    2C                   END
145200960307   X1C                   ELSE
145300960307     C* NON TROVATA DATA/ORA DI ARRIVO
145400960220     C* Se data/ora di arrivo non trovata la imposto=999999999998
145500960220     C* in modo che dopo il sorta della schiera sia l'ultima
145600960307     C  N30              MOVE      W0120         ARR(WW)
145700960307    1C                   END
145800960220     C                   ENDSR
145900960216     C***************  DETERMINA TERMINAL DI ARRIVO  ******************
146000960216     C     SRTAR         BEGSR
146100960216     C                   CLEAR                   WTAR
146200960822     C* SE FIL.FV=FIL.DI SCARICO --> TER-ARR=FIL.FV
146300960822   1AC     FVG(Z)        IFEQ      FSC(Z)
146400960822     C                   MOVE      FVG(Z)        WTAR
146500960822  X1AC                   ELSE
146600960216     C* CERCO IL TERMINAL DI ARRIVO DELLA FILIALE DEL FOGLIO VIAGGIO
146700960216     C* 1-GUARDO PRIMA LE ECCEZIONI
146800971118     C                   CLEAR                   DSLV55
146900971118     C                   MOVEL     'A'           D55TPT
147000971118     C                   MOVE      LNP(II)       D55LNP
147100971118     C                   MOVE      FVG(Z)        D55LIN
147200971118     C                   Z-ADD     WDTN          D55DRF
147300971118     C                   CALL      'FNLV55R'
147400971118     C                   PARM                    DSLV55
147500971118     C     D55ERR        IFNE      ' '
147600971118     C                   MOVEL     D55LIN        D55TFA
147700971118     C                   ENDIF
147800971118     C                   MOVE      D55TFA        WTAR
147900971118     C                   ENDIF
148000960216     C                   ENDSR
148100960219     C*************  CONTROLLO SE GIORNO FESTIVO  *********************
148200960219     C     CTRFES        BEGSR
148300960220     C                   MOVEL     W0080         KANN
148400960220     C                   MOVE      W0080         W0040             4 0
148500960220     C                   MOVEL     W0040         KMES
148600960220     C                   MOVE      W0040         GG                2 0
148700960319     C                   CLEAR                   WFEST
148800960219     C     KCLN          CHAIN     AZCLN01L                           30
148900960219    1C     *IN30         IFEQ      *OFF
149000960219     C     POM(GG)       ANDNE     *BLANKS
149100960219     C                   Z-ADD     1             XY                2 0
149200960219     C     POM(GG)       LOOKUP    S1Y(XY)                                30
149300960319     C   30              MOVE      FET(XY)       WFEST             1
149400960219    1C                   END
149500960219     C                   ENDSR
149600960216     C*******************  SCRITTURA METRI CUBI  **********************
149700960216     C     SRTLM         BEGSR
149800960219     C                   MOVE      LNP(II)       WLNP
149900960322     C     KTLM          CHAIN     TNTLM01L                           30
150000960322     C     *IN30         IFEQ      *ON
150100960216     C                   CLEAR                   TNTLM000
150200960216     C                   MOVE      V2CTRN        TLMTRN
150300960216     C                   MOVE      WDTN          TLMDDE
150400960216     C                   MOVE      LNP(II)       TLMTFP
150500960220     C                   MOVE      WTFA          TLMTFA                         FIL.SCARICO
150600960322     C                   MOVE      WTARS         TLMTFF                         FIL.FINALE
150700960424     C     WBIS1         IFEQ      *BLANKS
150800960322     C                   Z-ADD(H)  WMCR          TLMMCA
150900960424     C                   ELSE
151000960424     C                   Z-ADD(H)  WMCR          TLMMCB
151100960424     C                   END
151200960216     C                   Z-ADD     WDATEU        TLMDIR
151300960216     C                   WRITE     TNTLM000
151400960322     C                   ELSE
151500960424     C     WBIS1         IFEQ      *BLANKS
151600960322     C                   ADD(H)    WMCR          TLMMCA
151700960424     C                   ELSE
151800960424     C                   ADD(H)    WMCR          TLMMCB
151900960424     C                   END
152000960322     C                   UPDATE    TNTLM000
152100960216     C                   END
152200960216     C                   ENDSR
152300960216     C*******************  SCRITTURA FERMATE  *************************
152400960216     C     SRTLR         BEGSR
152500960219     C*
152600960219     C* CALCOLO DATA TRAINO+1
152700960219     C                   CLEAR                   WGIDAT
152800960219     C     WTGI          ADD       1             GI8TGI
152900960219     C                   CALL      'XSRGI8'
153000960219     C                   PARM                    WGIDAT
153100960219     C                   MOVE      GI8INV        WDAT
153200960219     C*
153300960220     C                   Z-ADD     1             WNFM
153400960216     C* SCRIVO LA PRIMA FERMATA DEL TRAINO
153500960216     C                   CLEAR                   TNTLR000
153600960216     C                   MOVE      V2CTRN        TLRTRN
153700960216     C                   MOVE      WDTN          TLRDDE
153800960216     C                   MOVE      WNFM          TLRNFM
153900960216     C                   MOVEL     'A'           TLRTFM
154000960216     C                   MOVE      CAR(1)        TLRFIL
154100090120     c                   z-add     tlrfil        wfilp
154200960216     C                   MOVEL     'S'           TLRCAR
154300960216     C                   MOVEL     'A'           TLRGFV
154400960216     C                   MOVE      WDATEU        TLRDIR
154500960216     C                   WRITE     TNTLR000
154600960219     C* Scrivo relativo record su FILE ORARI
154700960219     C                   CLEAR                   TNTLO000
154800960219     C                   MOVE      V2CTRN        TLOTRN
154900960219     C                   MOVE      WDTN          TLODDE
155000960219     C                   MOVE      WNFM          TLONFM
155100960219     C* DETERMINO GIORNO DI PARTENZA
155200960219     C                   MOVEL     PAR(1)        WDATAA
155300960219     C     WDTN          IFEQ      WDATAA
155400960219     C                   MOVE      'A'           TLOGGP
155500960219     C                   ELSE
155600960219     C                   EXSR      SRGIO
155700960219     C                   Z-ADD     1             XX
155800960219     C     YY            LOOKUP    NUM(XX)                                30
155900960219     C   30              MOVE      ALF(XX)       TLOGGP
156000960220     C  N30              MOVE      *BLANKS       TLOGGP
156100960219     C                   END
156200960219     C                   MOVE      PAR(1)        TLOHHP
156300960219     C                   MOVE      WDATEU        TLODIR
156400960219     C                   WRITE     TNTLO000
156500960219     C*
156600960219     C                   MOVE      *HIVAL        WARR             12 0
156700960219     C                   MOVE      '8'           WARR
156800960219     C* CICLO LETTURA DELLE ALTRE FERMATE DI CARICO
156900960216     C                   Z-ADD     2             C                 3 0
157000960219     C     C             DOWLE     100
157100960219     C     CAR(C)        ANDNE     *BLANKS
157200960216     C                   Z-ADD     1             W                 3 0
157300960219     C     CAR(C)        LOOKUP    SCA(W)                                 32
157400960220     C                   ADD       1             WNFM
157500960216     C                   Z-ADD     WNFM          TLRNFM
157600960216     C                   MOVE      CAR(C)        TLRFIL
157700960219     C   32              MOVE      'S'           TLRSCA
157800960219     C  N32              MOVE      ' '           TLRSCA
157900960216     C                   MOVE      'S'           TLRCAR
158000960216     C* DETERMINO GIORNO DI APERTURA FV
158100960216     C                   Z-ADD     1             S                 3 0
158200960216     C     CAR(C)        LOOKUP    LNP(S)                                 31
158300960216     C     DFV(S)        IFEQ      WDTN
158400960216     C                   MOVE      'A'           TLRGFV
158500960216     C                   ELSE
158600960219     C                   MOVE      DFV(S)        WDATAA
158700960219     C                   EXSR      SRGIO
158800960219     C                   Z-ADD     1             XX
158900960219     C     YY            LOOKUP    NUM(XX)                                30
159000960219     C   30              MOVE      ALF(XX)       TLRGFV
159100960220     C  N30              MOVE      *BLANKS       TLRGFV
159200960216     C                   END
159300090120     c* reperisco eventuali km
159400090508     c***                clear                   ficna2ds
159500090508IF  6c***                if        %lookup(wfilp:org)=0 or
159600090508     c***                          %lookup(tlrfil:org)=0
159700090508     c***                z-add     wfilp         ia2lnp
159800090508     c***                z-add     tlrfil        ia2lna
159900090508     c***                call      'FICNA2R1'
160000090508     c***                parm                    ficna2ds
160100090508IF  7c***                if        oa2error<>'1'
160200090508     c***                z-add     oa2km         tlrkmt
160300090508     c***                endif
160400090508     c***                endif
160500960219     C                   WRITE     TNTLR000
160600090120     c                   z-add     tlrfil        wfilp
160700960216     C* SCRIVO FILE DEGLI ORARI
160800960219     C                   MOVE      V2CTRN        TLOTRN
160900960219     C                   MOVE      WDTN          TLODDE
161000960219     C                   MOVE      WNFM          TLONFM
161100960219     C* DETERMINO GIORNO PARTENZA
161200960219     C                   MOVEL     PAR(C)        WPAR
161300960219     C     DFV(S)        IFEQ      WPAR
161400960219     C                   MOVE      TLRGFV        TLOGGP
161500960219     C                   ELSE
161600960219     C                   MOVE      WPAR          WDATAA
161700960219     C                   EXSR      SRGIO
161800960219     C                   Z-ADD     1             XX
161900960219     C     YY            LOOKUP    NUM(XX)                                30
162000960219     C   30              MOVE      ALF(XX)       TLOGGP
162100960220     C  N30              MOVE      *BLANKS       TLOGGP
162200960219     C                   END
162300960219     C                   MOVE      PAR(C)        TLOHHP
162400960219     C* DETERMINO GIORNO ARRIVO SE HO LA DATA DI ARRIVO
162500960219     C     *IN32         IFEQ      *ON
162600960219     C     ARR(W)        ANDLT     WARR
162700960219     C                   MOVEL     ARR(W)        WDATAA
162800960219     C     WPAR          IFEQ      WDATAA
162900960219     C                   MOVE      TLOGGP        TLOGGA
163000960219     C                   ELSE
163100960219     C                   EXSR      SRGIO
163200960219     C                   Z-ADD     1             XX
163300960219     C     YY            LOOKUP    NUM(XX)                                30
163400960219     C   30              MOVE      ALF(XX)       TLOGGA
163500960220     C  N30              MOVE      *BLANKS       TLOGGA
163600960219     C                   END
163700960219     C                   MOVE      ARR(W)        TLOHHA
163800960219     C                   ELSE
163900960219     C                   CLEAR                   TLOGGA
164000960219     C                   CLEAR                   TLOHHA
164100960219     C                   END
164200960219     C                   MOVE      WDATEU        TLODIR
164300960219     C                   WRITE     TNTLO000
164400960216     C*
164500960216     C* CLEARO SCARICO E RELATIVA DATA ORA GIà SCRITTA
164600960219     C   32              MOVE      *HIVAL        SCA(W)
164700960219     C   32              MOVE      *HIVAL        ARR(W)
164800960216     C*
164900960216     C                   ADD       1             C
165000960216     C                   ENDDO
165100960216     C* VEDO SE MI SONO RIMASTI SCARICHI DA SCRIVERE
165200960216     C* WW CONTINENE L'INDICE DELL'ULTIMO ELEMENTO DELLA SCHIERA ARR
165300960216     C* VALORIZZATO. DA QUELL'INDICE IN POI IMPOSTO TUTTI 9
165400960216     C* AFFINCHE' LA SCHIERA DOPO IL SORTA CONTENGA LE DATE ORE VALIDE
165500960216     C* A PARTIRE DAL PRIMO ELEMENTO
165600960216     C                   ADD       1             WW
165700960216     C     WW            IFLE      150
165800960216     C                   MOVEA     *HIVAL        ARR(WW)
165900960216     C                   END
166000960216     C                   MOVEA     ARR           SAR
166100960216     C                   SORTA     SAR
166200960216     C                   Z-ADD     1             WW
166300960220     C     WW            DOWLE     150
166400960216     C     SAR(WW)       ANDLT     *ALL'9'
166500960216     C                   Z-ADD     1             W
166600960216     C     SAR(WW)       LOOKUP    ARR(W)                                 30
166700960220     C                   ADD       1             WNFM
166800960216     C                   Z-ADD     WNFM          TLRNFM
166900960216     C                   MOVE      SCA(W)        TLRFIL
167000960216     C                   MOVE      'S'           TLRSCA
167100960216     C                   MOVE      ' '           TLRCAR
167200960219     C                   CLEAR                   TLRGFV
167300090508     c***                clear                   ficna2ds
167400090508IF  6c***                if        %lookup(wfilp:org)=0 or
167500090508     c***                          %lookup(tlrfil:org)=0
167600090508     c***                z-add     wfilp         ia2lnp
167700090508     c***                z-add     tlrfil        ia2lna
167800090508     c***                call      'FICNA2R1'
167900090508     c***                parm                    ficna2ds
168000090508IF  7c***                if        oa2error<>'1'
168100090508     c***                z-add     oa2km         tlrkmt
168200090508     c***                endif
168300090508     c***                endif
168400960219     C                   WRITE     TNTLR000
168500090120     c                   z-add     tlrfil        wfilp
168600960219     C* SCRIVO FILE ORARI
168700960219     C                   Z-ADD     WNFM          TLONFM
168800960219     C* DETERMINO GIORNO DI ARRIVO SE HO LA DATA DI ARRIVO
168900960219     C     ARR(W)        IFLT      WARR
169000960219     C                   MOVEL     ARR(W)        WDATAA
169100960219     C     WDTN          IFEQ      WDATAA
169200960219     C                   MOVE      'A'           TLOGGA
169300960219     C                   ELSE
169400960219     C                   EXSR      SRGIO
169500960219     C                   Z-ADD     1             XX
169600960219     C     YY            LOOKUP    NUM(XX)                                30
169700960219     C   30              MOVE      ALF(XX)       TLOGGA
169800960220     C  N30              MOVE      *BLANKS       TLOGGA
169900960219     C                   END
170000960219     C                   MOVE      ARR(W)        TLOHHA
170100960219     C                   ELSE
170200960219     C                   CLEAR                   TLOGGA
170300960219     C                   CLEAR                   TLOHHA
170400960219     C                   END
170500960219     C                   CLEAR                   TLOGGP
170600960219     C                   CLEAR                   TLOHHP
170700960219     C                   WRITE     TNTLO000
170800960216     C                   MOVE      *ZEROS        ARR(W)
170900960216     C                   ADD       1             WW
171000960216     C                   ENDDO
171100960216     C                   ENDSR
171200960219     C************** DETERMINO GIORNO (A,B,C.....)   ******************
171300960219     C     SRGIO         BEGSR
171400960219     C                   Z-ADD     1             XX                2 0
171500960219     C                   Z-ADD     1             YY                2 0
171600960220     C                   MOVE      WDAT          WDATX
171700960219     C*
171800960220     C     WDATX         DOWLT     WDATAA
171900960219     C* VERIFICO SE DATA FESTIVA
172000960220     C                   MOVE      WDATX         W0080             8 0
172100960219     C                   EXSR      CTRFES
172200960319    2C     WFEST         IFNE      'B'
172300960219     C                   ADD       1             YY                             NO fest.
172400960219    2C                   END
172500960219     C* CALCOLO DATA TRAINO+XX
172600960219     C                   ADD       1             XX
172700960219     C                   CLEAR                   WGIDAT
172800960219     C     WTGI          ADD       XX            GI8TGI
172900960219     C                   CALL      'XSRGI8'
173000960219     C                   PARM                    WGIDAT
173100960220     C                   MOVE      GI8INV        WDATX
173200960219     C                   ENDDO
173300960219     C                   ENDSR
173400960219     C*************  DELET ANAGRAFICA TRAINO  *************************
173500960219     C     DELTRN        BEGSR
173600960219     C* LISTINI
173700010828     C     KANA          SETLL     TITLL01L
173800010828     C     KANA          READE     TITLL01L                               30
173900960219     C     *IN30         DOWEQ     *OFF
174000010828     C                   DELETE    TITLL000
174100010828     C     KANA          READE     TITLL01L                               30
174200960219     C                   ENDDO
174300960219     C* METRI CUBI
174400960219     C     KANA          SETLL     TNTLM01L
174500960219     C     KANA          READE     TNTLM01L                               30
174600960219     C     *IN30         DOWEQ     *OFF
174700960219     C                   DELETE    TNTLM000
174800960219     C     KANA          READE     TNTLM01L                               30
174900960219     C                   ENDDO
175000960219     C* ORARI
175100960219     C     KANA          SETLL     TNTLO01L
175200960219     C     KANA          READE     TNTLO01L                               30
175300960219     C     *IN30         DOWEQ     *OFF
175400960219     C                   DELETE    TNTLO000
175500960219     C     KANA          READE     TNTLO01L                               30
175600960219     C                   ENDDO
175700960219     C* FERMATE
175800960219     C     KANA          SETLL     TNTLR01L
175900960219     C     KANA          READE     TNTLR01L                               30
176000960219     C     *IN30         DOWEQ     *OFF
176100960219     C                   DELETE    TNTLR000
176200960219     C     KANA          READE     TNTLR01L                               30
176300960219     C                   ENDDO
176400960219     C* TESTATA
176500960219     C     KANA          CHAIN     TNTLT01L                           30
176600960219     C  N30              DELETE    TNTLT000
176700960219     C                   ENDSR
176800960219     C************ AGGIORNAMENTO STORICO TRAINI PER ABBINAMENTO  ******
176900960219     C     AGGSTO        BEGSR
177000960219     C     KTSR1B        SETLL     TNTSR01L
177100960319     C     KTSR1B        READE     TNTSR01L                               31
177200960219     C* SE è FILIALE CHE ESPONE IMPORTO AGGIORNO TESTATA
177300960319     C  N31KTST1         CHAIN     TNTST01L                           31
177400960319     C  N31TSTNFE        IFEQ      TSRNFP
177500010907
177600010907      * in questo caso devo fare dei confronti con la divisa della griglia, quindi se
177700010907      * vuota quella dell'estensione prendo quella della griglia
177800010907     c                   if        fwsvip = *blanks
177900010907     c                   eval      fwsvip = tstdiv
178000010907     c                   endif
178100010907     c                   if        fwsves = *blanks
178200010907     c                   eval      fwsves = tstdiv
178300010907     c                   endif
178400010828
178500010828      * aggancio tabella divisa griglia
178600010828     c                   clear                   dscv
178700010828     c                   movel     'CV'          kcod
178800010828     c                   movel(p)  tstdiv        kkey
178900010828     c     ktab          chain     tabel00f
179000010828     c                   if        %found(tabel00f)
179100010828     c                   movel     tbluni        dscv
179200010828     c                   endif
179300010828
179400010828      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
179500010828      * devo convertire fwsipt e fwsies nella divisa della griglia
179600010828     c                   eval      w_importo = fwsipt
179700011023     c                   if        fwsvip <> tstdiv and fwsipt <> 1
179800011023     c                             and fwsipt <> *zeros
179900010828     c                   eval      w_divisa = fwsvip
180000010828     c                   exsr      Sr_Converti
180100010828     c                   endif
180200010828     c                   eval      tstipt = w_importo
180300010828
180400010828     c                   eval      w_importo = fwsies
180500011023     c                   if        fwsves <> tstdiv and fwsies <> 1
180600011023     c                             and fwsies <> *zeros
180700010828     c                   eval      w_divisa = fwsves
180800010828     c                   exsr      Sr_Converti
180900010828     c                   endif
181000010828     c                   eval      tsties = w_importo
181100010828
181200010828     C**!!!              MOVE      FVSIPT        TSTIPT
181300010828     C**!!!              MOVE      FVSIES        TSTIES
181400960219     C                   MOVE      V2CPDR        TSTPDR
181500960219     C                   MOVE      FVSTMZ        TSTTMZ
181600960219     C                   EXSR      SRLIST
181700960219     C                   MOVE      V2CCAU        TSTCAU
181800010828     C**!!!              MOVE      V2CICO        TSTICO
181900010828      * se la divisa impostata a video non è uguale alla divisa della griglia
182000010828      * devo convertire v2cico nella divisa della griglia
182100010828     c                   eval      w_importo = v2cico
182200011024     c                   if        v2cdiv <> tstdiv and v2cico <> 1
182300011024     c                             and v2cico <> 0
182400010828     c                   eval      w_divisa = v2cdiv
182500010828     c                   exsr      Sr_Converti
182600010828     c                   endif
182700010828     c                   eval      tstico = w_importo
182800010828
182900960219     C                   MOVE      '2'           TSTCNF
183000960219     C                   UPDATE    TNTST000
183100960219     C                   ELSE
183200960219     C                   UNLOCK    TNTST01L
183300960219     C                   END
183400960319     C     *IN31         DOWEQ     *OFF
183500960219     C                   MOVE      FVSLNP        TSRLNP
183600960219     C                   MOVE      FVSNFV        TSRNFV
183700960219     C                   MOVE      FVSDFV        TSRDFV
183800960219     C                   MOVE      FVSDRP        TSRDRP
183900960219     C                   MOVE      FVSHRP        TSRHRP
184000960219     C                   MOVE      FVSRTP        TSRRTP
184100960219     C     TSRDRP        IFEQ      TSRDTP
184200960219     C     TSRHRP        ANDEQ     TSRHTP
184300970327     C     TSRRTP        ANDEQ     *BLANKS
184400960219     C                   MOVE      *HIVAL        TSRDKP
184500960314     C                   MOVE      'S'           TSRFKP
184600960219     C                   END
184700960219     C                   MOVE      *HIVAL        TSRDKI
184800960314     C                   MOVE      'S'           TSRFKI
184900960219     C                   UPDATE    TNTSR000
185000960319     C     KTSR1B        READE     TNTSR01L                               31
185100960219     C                   ENDDO
185200960219     C*
185300960219     C                   ENDSR
185400960219     C****************  RICERCA LISTINO DA CARICARE  ******************
185500960219     C     SRLIST        BEGSR
185600960219     C* PRIMO TENTATIVO DI RICERCA : TRAZIONISTA E TIPO AUTOMEZZO DATI
185700960219     C                   MOVE      '1'           W001A             1
185800960219     C                   MOVEL     FVSTMZ        KTMZ
185900960219     C                   MOVE      V2CPDR        KPDR
186000010828     C     KTLL          CHAIN(N)  TITLL01L                           30
186100960906     C  N30TLLATB        COMP      ' '                                3030
186200960219    1C     *IN30         DOWEQ     *ON
186300960219     C     W001A         ANDLT     '4'
186400960219    2C                   SELECT
186500960219     C* SECONDO TENTATIVO DI RICERCA : TRAZIONISTA DATO
186600960219     C*                                AUTOMEZZO GENERICO = '99'
186700960219     C     W001A         WHENEQ    '1'
186800960219     C                   MOVE      *ALL'9'       KTMZ
186900960219     C                   MOVE      '2'           W001A
187000960219     C* TERZO TENTATIVO DI RICERCA : TRAZIONISTA GENERICO = 9999999
187100960219     C*                              AUTOMEZZO DEL FOGLIO VIAGGIO
187200960219     C     W001A         WHENEQ    '2'
187300960219     C                   MOVE      *ALL'9'       KPDR
187400960219     C                   MOVEL     FVSTMZ        KTMZ
187500960219     C                   MOVE      '3'           W001A
187600960219     C* QUARTO TENTATIVO DI RICERCA : TRAZIONISTA E AUTOMEZZO GENERICI
187700960219     C     W001A         WHENEQ    '3'
187800960219     C                   MOVE      *ALL'9'       KTMZ
187900960219     C                   MOVE      '4'           W001A
188000960219    2C                   ENDSL
188100010828     C     KTLL          CHAIN(N)  TITLL01L                           30
188200960906     C  N30TLLATB        COMP      ' '                                3030
188300960219    1C                   ENDDO
188400960219     C*
188500960219     C* LISTINO TROVATO: MEMORIZZO IMPORTO
188600960219    1C     *IN30         IFEQ      *OFF
188700010828
188800010828      * se la divisa del listino non è uguale alla divisa della griglia
188900010828      * devo convertire tllili nella divisa della griglia
189000010828     c                   eval      w_importo = tllili
189100011024     c                   if        tlldiv <> tstdiv and tllili <> 1
189200011024     c                             and tllili <> 0
189300010828     c                   eval      w_divisa = tlldiv
189400010828     c                   exsr      Sr_Converti
189500010828     c                   endif
189600010828     c                   eval      tstili = w_importo
189700010828     C**!!!              Z-ADD     TLLILI        TSTILI
189800960219    1C                   END
189900960219     C                   ENDSR
190000010828      *----------------------------------------------------------------
190100010828      * CONVERTI - Routine conversione importo
190200010828      *----------------------------------------------------------------
190300010828     c     Sr_Converti   begsr
190400010828
190500010828     c                   clear                   yeurcods
190600010828     c                   movel     w_divisa      yecdvi
190700010828     c                   movel     tstdiv        yecdvo
190800010828     c                   z-add     w_importo     yecimi
190900010828     c                   move      §cvdec        yecdco
191000010828     c                   call      'YEURCO'
191100010828     c                   parm                    yeurcods
191200010828     c                   if        yecesi  = ' '
191300010828     c                   z-add     yecimo        w_importo
191400010828     c                   endif
191500010828
191600010828     c                   endsr
191700010828
191800951128     C**************  OPERAZIONI INIZIALI  ****************************
191900951128     C     *INZSR        BEGSR
192000951128     C     *ENTRY        PLIST
192100951128     C                   PARM                    KPJBA
192200010828     C                   PARM                    PFLP                           SOLO X D42MOD=C
192300010827     C                   MOVEL     KPJBU         DSTL42
192400951129     C*
192500951129     C                   Z-ADD     1             CODUT
192600951129     C                   CALL      'X§PARUT'
192700951129     C                   PARM                    UT§DSE
192800960214     C                   MOVEL     RAGUT         V1DRSU
192900960229     C* REPERIMENTO DATA ULTIMO CONTROLLO
193000960229     C     *DTAARA       DEFINE                  TNTL30
193100960229     C                   IN        TNTL30
193200960216     C* PULIZIA CAMPI DI OUTPUT
193300010828     C     D42MOD        IFEQ      'R'
193400010828     C                   CLEAR                   D42LNP
193500960311     C                   END
193600010828     C                   CLEAR                   D42NFV
193700010828     C                   CLEAR                   D42ERR
193800960216     C*
193900960216     C     *LIKE         DEFINE    NRR1          WNRR1
194000960216     C     *LIKE         DEFINE    NRR1          NRRSAV
194100960216     C     *LIKE         DEFINE    FVSABN        WABN
194200960216     C     *LIKE         DEFINE    FVSTRS        WTRS
194300960216     C     *LIKE         DEFINE    TSRFFP        WFFP
194400960216     C     *LIKE         DEFINE    V1CANN        WCANN
194500960228     C     *LIKE         DEFINE    V1CBIS        WCBIS
194600960307     C     *LIKE         DEFINE    V1CCNT        WCCNT
194700960216     C     *LIKE         DEFINE    V1CNBR        W1CNBR
194800960216     C     *LIKE         DEFINE    V1SSCE        WSCE
194900960219     C     *LIKE         DEFINE    V1SLNP        WLNPS
195000960219     C     *LIKE         DEFINE    FV2LNP        WLNP
195100960216     C     *LIKE         DEFINE    FVSPDR        KPDR
195200000807     C     *LIKE         DEFINE    TLZTIP        KTIP
195300960216     C     *LIKE         DEFINE    V2CCAU        WCAU
195400960216     C     *LIKE         DEFINE    V2DCAU        WDCAU
195500960216     C     *LIKE         DEFINE    X             WFIRST
195600960216     C     *LIKE         DEFINE    TBLCOD        KCOD
195700960216     C     *LIKE         DEFINE    TBLKEY        KKEY
195800960216     C     *LIKE         DEFINE    FV2EPA        WEPA
195900960216     C     *LIKE         DEFINE    FV2TDH        WTDH
196000960216     C     *LIKE         DEFINE    FVSPDR        WPDR
196100960216     C     *LIKE         DEFINE    FVSDPD        WDPD
196200960228     C     *LIKE         DEFINE    FVSPDR        WPDR1
196300960228     C     *LIKE         DEFINE    FVSDPD        WDPD1
196400011012     C     *LIKE         DEFINE    FWSIPT        WIPT
196500011012     C     *LIKE         DEFINE    FWSIPT        WIPT1
196600960424     C     *LIKE         DEFINE    FVSBIS        WBIS1
196700960219     C     *LIKE         DEFINE    FVSTMZ        WTMZ
196800960228     C     *LIKE         DEFINE    FVSTMZ        WTMZ1
196900960319     C     *LIKE         DEFINE    FVSLNP        WFES
197000960319     C     *LIKE         DEFINE    FVSLNP        WFES1
197100960216     C     *LIKE         DEFINE    FVSDFV        WDFV
197200960219     C     *LIKE         DEFINE    TLTDDE        WDTN
197300971118     C     *LIKE         DEFINE    D55TFA        WTAR
197400971118     C     *LIKE         DEFINE    D55TFA        WTARS
197500960220     C     *LIKE         DEFINE    TLMTFA        WTFA
197600960216     C     *LIKE         DEFINE    TLRNFM        WNFM
197700960219     C     *LIKE         DEFINE    G08TGI        WTGI
197800960219     C     *LIKE         DEFINE    G08INV        WDATAA
197900960219     C     *LIKE         DEFINE    G08INV        WDAT
198000960220     C     *LIKE         DEFINE    G08INV        WDATX
198100960219     C     *LIKE         DEFINE    G08INV        WPAR
198200960219     C     *LIKE         DEFINE    FVSLNP        KLNP
198300960219     C     *LIKE         DEFINE    FVSNFV        KNFV
198400960219     C     *LIKE         DEFINE    FVSTMZ        KTMZ
198500960219     C     *LIKE         DEFINE    CLNANN        KANN
198600960219     C     *LIKE         DEFINE    CLNMES        KMES
198700960219     C     *LIKE         DEFINE    CLNTFP        KTFP
198800960219     C     *LIKE         DEFINE    CLNTFA        KTFA
198900971118     C     *LIKE         DEFINE    D55TFA        WFVG
199000960219     C     *LIKE         DEFINE    FV2LAI        WSCA
199100960319     C     *LIKE         DEFINE    TSRLNP        KLNPT
199200960319     C     *LIKE         DEFINE    TSRNFV        KNFVT
199300960322     C     *LIKE         DEFINE    FVDTRC        WTRC
199400000218     C     *LIKE         DEFINE    ATBCOD        KATCOD
199500000218     C     *LIKE         DEFINE    ATBAZI        KATAZI
199600000218     C     *LIKE         DEFINE    ATBKEY        KATKEY
199700960216     C* DEFINISCO AREA DATI NUMERATORE TRAINI ECCEZIONALI
199800960216     C     *DTAARA       DEFINE                  TNTL02            7 0
199900960216     C*
200000960216     C     KTSR1         KLIST
200100010828     C                   KFLD                    D42TRN
200200010828     C                   KFLD                    D42DTN
200300010828     C                   KFLD                    D42NFP
200400960219     C     KTSR1B        KLIST
200500960219     C                   KFLD                    TSRTRN
200600960219     C                   KFLD                    TSRDTN
200700960219     C                   KFLD                    TSRNFP
200800960219     C     KTSR3         KLIST
200900960219     C                   KFLD                    V2CTRN
201000960219     C                   KFLD                    WDTN
201100960219     C                   KFLD                    FVSLNP
201200960319     C     KTSR4         KLIST
201300960319     C                   KFLD                    KLNPT
201400960319     C                   KFLD                    KNFVT
201500960319     C                   KFLD                    TSTTRN
201600960319     C                   KFLD                    TSTDTN
201700960319     C                   MOVE      *ZEROS        KLNPT
201800960319     C                   MOVE      *ZEROS        KNFVT
201900960216     C     KFVS          KLIST
202000960216     C                   KFLD                    WABN
202100960216     C                   KFLD                    WTRS
202200960216     C                   KFLD                    WFFP
202300960216     C     KFVS2         KLIST
202400960216     C                   KFLD                    WABN
202500960216     C                   KFLD                    WTRS
202600960216     C     KFVS1         KLIST
202700960219     C                   KFLD                    KLNP
202800960219     C                   KFLD                    KNFV
202900960216     C     KFV2          KLIST
203000960219     C                   KFLD                    WLNP
203100960216     C                   KFLD                    NFV
203200960216     C                   KFLD                    WEPA
203300960219     C                   KFLD                    WSCA
203400960216     C                   KFLD                    WTDH
203500010828     C     KFWS          KLIST
203600010828     C                   KFLD                    fvsLNP
203700010828     C                   KFLD                    fvsNFV
203800960216     C     KTAB          KLIST
203900960216     C                   KFLD                    CODUT
204000960216     C                   KFLD                    KCOD
204100960216     C                   KFLD                    KKEY
204200960220     C     KTAB0         KLIST
204300960220     C                   KFLD                    CODUT
204400960220     C                   KFLD                    KCOD
204500960216     C     KTLM          KLIST
204600960216     C                   KFLD                    V2CTRN
204700960216     C                   KFLD                    WDTN
204800960219     C                   KFLD                    WLNP
204900960220     C                   KFLD                    WTFA
205000960322     C                   KFLD                    WTARS
205100960219     C     KANA          KLIST
205200960219     C                   KFLD                    V2CTRN
205300960219     C                   KFLD                    WDTN
205400960219     C     KTST1         KLIST
205500960219     C                   KFLD                    TSRTRN
205600960219     C                   KFLD                    TSRDTN
205700960219     C     KTLL          KLIST
205800960219     C                   KFLD                    TSTTRN
205900960219     C                   KFLD                    TSTDTN
206000960219     C                   KFLD                    KTMZ
206100960219     C                   KFLD                    KPDR
206200000807     C     KTNTLZ        KLIST
206300000807     C                   KFLD                    KTIP
206400000807     C                   KFLD                    KPDR
206500000807     C                   EVAL      KTIP = 'T '                                  Tipol.TRAZIONISTA
206600960219     C     KCLN          KLIST
206700960219     C                   KFLD                    KTFP
206800960219     C                   KFLD                    KTFA
206900960219     C                   KFLD                    KANN
207000960219     C                   KFLD                    KMES
207100960322     C*
207200960322     C     KFVD          KLIST
207300960322     C                   KFLD                    WLNP
207400960322     C                   KFLD                    NFV
207500960322     C                   KFLD                    WFVG
207600960322     C                   KFLD                    WTRC
207700000218     C* LETTURA ANATB01L - PARZIALE
207800000218     C     KEATAB        KLIST
207900000218     C                   KFLD                    KATCOD
208000000218     C                   KFLD                    KATAZI
208100000218     C                   KFLD                    KATKEY
208200000211     C*--------------------
208300000211     C*---------- RICERCA DITTA :
208400000211     C*--------------------
208500000218     C***                MOVEL     'SOC001'      TIPXSC
208600000218     C***                MOVEL     *BLANK        SOCXSC
208700000218     C***                EXSR      REPSOC
208800000218     C***  RTNXSC        IFNE      '1'
208900000218     C***                MOVEL     XSOCDS        SOC001
209000000218     C***                MOVEL     xscrgs        RSUT             20
209100000218     c***                end
209200960216     C*
209300960216     C                   MOVE      'A'           WEPA
209400960216     C                   MOVE      'R'           WTDH
209500960216     C                   MOVEL     'S'           WABN
209600960322     C                   MOVEL     '1'           WTRC
209700960216     C                   CLEAR                   WTRS
209800960216     C                   MOVE      *ZEROS        II                3 0
209900960216     C                   CLEAR                   WFFP
210000960216     C                   SETOFF                                       01
210100960219     C                   Z-ADD     68            KTFP
210200960219     C                   CLEAR                   KTFA
210300960213     C* 05 ON --> RICHIESTO ELENCO FOGLI VIAGGIO DA ABBINARE MANUALM.
210400010828    1C     D42MOD        IFEQ      'R'
210500960319     C                   SETON                                        05
210600960213     C                   MOVEL     TIT1          V1DTI1
210700960213     C                   MOVEL     TIT2          V1DTI2
210800960213     C                   MOVE      ' 1'          V1CSCE
210900960216     C* Carico schiera ffa se parametri di input valorizzati
211000010828    2C     D42TRN        IFGT      *ZEROS
211100960216     C                   SETON                                        01
211200960216     C                   CLEAR                   W001A
211300960216     C     KTSR1         SETLL     TNTSR01L
211400960219     C     KTSR1         READE(N)  TNTSR01L                               30
211500960216    3C     *IN30         DOWEQ     *OFF
211600960220    4C     TSRATB        IFEQ      *BLANKS
211700960216    5C     W001A         IFEQ      ' '
211800960216     C                   MOVE      TSRFFP        WFFP
211900960216     C                   MOVE      '1'           W001A             1
212000960216    5C                   END
212100960216     C                   ADD       1             II
212200960216     C                   MOVE      TSRFFA        FFA(II)
212300960216    4C                   END
212400960315     C     KTSR1         READE(N)  TNTSR01L                               30
212500960216    3C                   ENDDO
212600960216    2C                   END
212700960216   X1C                   ELSE
212800010828     C     D42MOD        IFEQ      ' '
212900960216     C                   OPEN      FNFV201L
213000960216     C                   OPEN      TNTLM01L
213100960219     C                   OPEN      TNTLR01L
213200960219     C                   OPEN      TNTLO01L
213300960219     C                   OPEN      TNTLT01L
213400010828     C                   OPEN      TITLL01L
213500960219     C                   OPEN      TNTST01L
213600960319     C                   OPEN      TNTSR04L
213700960219     C                   OPEN      AZCLN01L
213800960322     C                   OPEN      FNFVD01L
213900960213     C                   MOVEL     TIT3          V1DTI1
214000960213     C                   MOVEL     TIT4          V1DTI2
214100960213     C                   MOVE      'Sc'          V1CSCE
214200960220     C* CARICAMENTO TABELLA 1Y - SIMBOLI CALENDARIO BARTOLINI
214300960220     C* (CARICAMENTO DEI SIMBOLI FESTIVI PER PROCDEURA TRAINI)
214400960220     C                   MOVE      *ZEROS        X
214500960220     C                   MOVEL(P)  '1Y'          KCOD
214600960220     C     KTAB0         SETLL     TABEL00F
214700960220     C     KTAB0         READE     TABEL00F                               30
214800960220     C     *IN30         DOWEQ     *OFF
214900960220     C     TBLFLG        IFEQ      *BLANKS
215000960220     C                   MOVEL     TBLUNI        DS1Y
215100960220     C     §1YFET        IFNE      *BLANKS
215200960220     C                   ADD       1             X
215300960220     C                   MOVEL     TBLKEY        S1Y(X)
215400960220     C                   MOVE      §1YFET        FET(X)
215500960220     C                   END
215600960220     C                   END
215700960220     C     KTAB0         READE     TABEL00F                               30
215800960220     C                   ENDDO
215900960319     C                   END
216000960216    1C                   END
216100960216     C* RILEVO LA DATA DEL GIORNO
216200960216     C                   TIME                    WTIME            14 0          ORA E DATA
216300960216     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
216400960216     C                   CLEAR                   WLBDA8
216500960216     C                   MOVE      WDATE         G08DAT
216600960216     C                   CALL      'XSRDA8'
216700960216     C                   PARM                    WLBDA8
216800960216     C                   MOVE      G08INV        WDATEU            8 0
216900010907
217000010907      * recupero la divisa dalla tabella GED
217100010907     c                   clear                   dged
217200010907     c                   clear                   tibs02ds
217300010907     c                   movel     'L'           t02tla
217400010907     c                   movel     'C'           t02mod
217500010907     c                   movel     knsif         t02sif
217600010907     c                   movel     'GED'         t02cod
217700010907     c                   movel     '1'           t02ke1
217800010907     c                   call      'TIBS02R'
217900010907     c                   parm                    kpjba
218000010907     c                   parm                    tibs02ds
218100010907    5c                   movel     t02uni        dged
218200090120     c*
218300090120     C* FILIALI  estere
218400090120     C                   CLEAR                   OG143
218500090120     C                   CLEAR                   OG148
218600090120     C                   Z-ADD     *ZEROS        J                 4 0
218700090120     C     *LOVAL        SETLL     AZORG01L
218800090120     C                   READ      AZORG01L                               99
218900090120DO  1C     *IN99         DOWEQ     *OFF
219000090120IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
219100090120     C                   MOVEL     ORGDE8        OG148
219200090120     C                   MOVEL     ORGDE3        OG143
219300090120IF  3C                   IF        §oglfv<>'N' and (§ogntw='DPD'                *IN GESTIONE FV
219400090120     c                             or §ogntw='FED' or §ogntw='EEX')
219500090120     C                   ADD       1             J
219600090120     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
219700090120E   3C                   ENDIF
219800090120E   2C                   ENDIF
219900090120     C                   READ      AZORG01L                               99
220000090120E   1C                   ENDDO
220100951128     C*
220200951128     C                   ENDSR
220300000211     C*----------------------------------------------------*
220400000211     C* Reperimento dati società
220500000211     C*----------------------------------------------------*
220600000218     C***  REPSOC        BEGSR
220700000211     C*
220800000218     C***                CALLB     'XSOC'
220900000218     C***                PARM                    TIPXSC            6
221000000218     C***                PARM                    SOCXSC            3
221100000218     C***                PARM                    CDSXSC            9 0
221200000218     C***                PARM                    MODXSC            3
221300000218     C***                PARM      *blanks       RTNXSC            1
221400000218     C***                PARM                    XSOCDS
221500000218     C***                PARM                    KPJBA
221600000211     C*
221700000218     C***                ENDSR
221800960319     OTNTST000  E            AGGTST
221900960319     O                       TSTCNF
222000960219** MSG
222100960213Fine scorrimento                                                              01
222200960213Scegliere un foglio viaggio                                                   02
222300960214Scelta errata                                                                 03
222400960214Numero scelta già immesso                                                     04
222500960214E' già stato scelto un FV con linea di partenza  xxx-xxxxxxxxxx               05
222600960214Presente in questo FV una filiale di scarico = alla linea di part.del 1°FV    06
222700960214Foglio viaggio già annullato                                                  07
222800960214Foglio viaggio già ripristinato                                               08
222900960215Data FV non in sequenza: cambiare numerazione o scegliere un altro foglio vg. 09
223000960214Foglio viaggio annullato: scelta errata                                       10
223100960215Non trovati trazionisti per chiave alfabetica richiesta                       11
223200960215Codice trazionista obbligatorio                                               12
223300960215Codice trazionista inesistente o errato                                       13
223400960215Importo obbligatorio                                                          14
223500960215Causale contabilizzazione obbligatoria                                        15
223600960219Data foglio viaggio festiva e > di data traino: scelta non ammessa            16
223700960220Causale contabilizzazione inesistente o annullata                             17
223800960424Numeratore traini in uso da un altro lavoro                                   18
223900960223Alcuni FV non abbinati a traino a causa delle modifiche fatte all'anagrafica  19
224000011012Immettere la divisa                                                           20
224100010828Divisa errata                                                                 21
224200010828Divisa non ammessa per i traini                                               22
224300010828Non ammessi importi con decimali per la divisa indicata                       23
224400011001Se modificata la divisa ridigitare il relativo importo                        24
224500960219**  NUM
2246009602200102030405060708091011121314151617181920
224700960219**  ALF
224800960219BCDEFGHILMNOPQRSTUVZ
