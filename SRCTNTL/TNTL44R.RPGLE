000100000214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200000218     H*PARMS ACTGRP(*CALLER)
000300951009     H*------------------------------------------------------------------------*
000400951102     H* GESTIONE GRIGLIA MANUTENZIONE INTERROGAZIONE                   NO      *
000500010827
000600010827      * ATTENZIONE: questo programma era il vecchio TNTL34R
000700010827
000800951009     H*------------------------------------------------------------------------*
000900951009     H DECEDIT('0,') DATEDIT(*DMY.)
001000951009     F*------------------------------------------------------------------------*
001100010824     FTNTL44D   CF   E             WORKSTN
001200010824     F                                     SFILE(TL44S02:NRR2)
001300951108     F                                     INFDS(DSSF02)
001400010824     F                                     SFILE(TL44S03:NRR3)
001500010824     F                                     SFILE(TL44S13:NR13)
001600010824     F                                     SFILE(TL44S04:NRR4)
001700950922     FAZORG01L  IF   E           K DISK
001800951106     FTABEL00F  IF   E           K DISK
001900000218     FANATB01L  IF   E           K DISK
002000960223     FTNFVS01L  UF   E           K DISK
002100000504     FTNFWS01L  UF   E           K DISK
002200960223     FFNFV201L  UF   E           K DISK
002300951106     FTNTLZ01L  IF   E           K DISK
002400951108     FTNTLT01L  IF   E           K DISK
002500010824     FTITLL01L  IF   E           K DISK
002600960226     FTNTLR01L  IF   E           K DISK
002700960226     FTNTLM01L  IF   E           K DISK
002800951103     FFNFV401L  IF   E           K DISK
002900960223     FTNTST01L  UF   E           K DISK
003000951113     FTNTST02L  IF   E           K DISK
003100951113     F                                     RENAME(TNTST000:TNTST002)
003200951102     FTNTST04L  IF   E           K DISK
003300951102     F                                     RENAME(TNTST000:TNTST004)
003400960226     FTNTSR01L  UF A E           K DISK
003500951212     FTNTSR02L  IF   E           K DISK
003600951212     F                                     RENAME(TNTSR000:TNTSR002)
003700951212     FTNTSR03L  IF   E           K DISK
003800951212     F                                     RENAME(TNTSR000:TNTSR003)
003900960226     FTNTSR04L  UF   E           K DISK
004000951114     F                                     RENAME(TNTSR000:TNTSR004)
004100960226     FTNTSM01L  UF   E           K DISK
004200951106     FTNTSW01L  UF A E           K DISK
004300951106     FTNTSW02L  UF A E           K DISK
004400951106     F                                     RENAME(TNTSW000:TNTSW002)
004500960226     FTNTSW03L  UF   E           K DISK
004600960213     F                                     RENAME(TNTSW000:TNTSW003)
004700960223     FTNTSW04L  UF   E           K DISK
004800960223     F                                     RENAME(TNTSW000:TNTSW004)
004900020315     D MSG             S             78    DIM(45) CTDATA PERRCD(1)             MSG VIDEO
005000960212     D FN2S            S             56    DIM(2) CTDATA PERRCD(1)              SFL2 T.FUNZ 10ON
005100960212     D FN2N            S             56    DIM(2) CTDATA PERRCD(1)              SFL2 T.FUNZ 10OFF
005200951214     D FN4N            S             78    DIM(2) CTDATA PERRCD(1)              SFL4 T.FUNZ 10OFF
005300951102     D K78             S              1    DIM(78)                              DI COMODO
005400951109     D K7              S              1    DIM(7)                               DI COMODO
005500951113     D K13             S              1    DIM(13)                              DI COMODO
005600951109     D SKI             S              1    DIM(15)                              COMODO
005700000823     D MCT             S              6  0 DIM(400)                             MC X VISUAL GRIGLIA
005800010827
005900020315     d w_tsrtrn        s                   like(tsrtrn)
006000041001     d w_tsrdtn        s                   like(tsrdtn)
006100010827     d w_importo       s                   like(fwsipt)
006200010827     d w_divisa        s                   like(fwsvip)
006300011009     d w_decimali      s              2  0
006400011012     d flggei          s              1    inz('0')
006500011012     d w_delta         s                   like(fwsipt)
006600011012     d sav_wili        s                   like(wili)
006700011012     d sav_v2cimp      s                   like(v2cimp)
006800011012     d sav_v2hipt      s                   like(v2hipt)
006900010827
007000951108     D DSSF02          DS
007100951214     D  V2SNR1               378    379B 0
007200951106     D WLBDAT          DS
007300951106     D  G02DAT                 1      8  0
007400951106     D  G02INV                 9     16  0
007500951106     D  G02ERR                17     17
007600951106     D  G02TGI                18     22  0
007700951102     D WGIDAT          DS                  INZ
007800951102     D  GIODAT                 1      8  0
007900951102     D  GIOINV                 9     16  0
008000951102     D  GIOTGI                17     21  0
008100951102     D                 DS
008200951102     D  WAA                    1      4
008300951102     D  WMM                    5      6
008400951102     D  WGG                    7      8
008500951102     D  WDATA                  1      8
008600951117     D                 DS
008700000504    +D  FLP                    1    897
008800000504     D                                     DIM(299)                             LINEE TRANSITO
008900951117    +D  FVSFLP                 1    240
009000951117    +D  FVSFL2               241    447
009100000504    +D  FWSFL3               448    687
009200000504    +D  FWSFL4               688    897
009300010827
009400011012     D tibs02ds      E DS
009500010827     D yeurcods      E DS
009600010827     D dscv          E DS
009700011012     D dgei          E DS
009800010827
009900010827
010000000214     D*---------------------
010100000214     D* DS CAUSALI CONTABILIZZAZIONE
010200000214     D*---------------------
010300000214     D ANGY4ZDS      E DS
010400000214     D XTABDS        E DS                  INZ
010500000214     D XATBDS        E DS                  INZ
010600951103     D*****
010700951103     D* PASSAGGIO DATI ALLA RICERCA TRAZIONISTI  - TRUL16R -
010800951113     D*****
010900951103     D PARAM3          DS
011000000807     D* TIPOLOGIA T_=TRAZIONISTA
011100000807     D  PA3TIP                 1      2
011200000807     D* RAGIONE SOCIALE TRAZIONISTA
011300000807     D  PA3RSC                 3     37
011400000807     D* S.I. TRAZIONISTA
011500000807     D  PA3CSF                38     40
011600000807     D* CODICE TRAZIONISTA
011700000807     D  PA3PDR                41     47  0
011800000807     D* PA3FLG = "3" --> NON ESISTONO TRAZIONISTI PER LA CHIAVE
011900000807     D*                  ALFABETICA RICHIESTA
012000000807     D  PA3FLG                48     48
012100960223     D DSTL24          DS
012200960223     D  D24DTD                 1      8  0
012300960223     D  D24DTA                 9     16  0
012400960223     D  D24TRN                17     23  0
012500960223     D  D24TFP                24     26  0
012600960223     D  D24DDE                27     34  0
012700960223     D* CREAZIONE SE GIORNO FESTIVO (' '=No controllo festivo S=SI)
012800960223     D  D24FES                35     35
012900950921     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
013000950921     D KPJBA         E DS
013100951124     D TNTL30        E DS                  EXTNAME(TNTL30DS)
013200960904     D DSTL33        E DS                  EXTNAME(TNTL33DS)
013300951124     D DSTL00        E DS                  EXTNAME(TNTL00DS)
013400971204     D DSLV55        E DS                  EXTNAME(FNLV55DS)
013500960131     D* PASSAGGIO DATI A PGM FNLV15R - INTERROGAZIONE DESCRIZIONI RITAR
013600960131     D DSLV15        E DS                  EXTNAME(FNLV15DS)
013700960322     D* PASSAGGIO DATI A PGM TNTL37R - INTERROGAZIONE FV
013800960322     D DSTL37        E DS                  EXTNAME(TNTL37DS)
013900951130     D* PARAMETRI PER PGM DI ELENCO FOGLI VIAGGIO DA ABBINARE
014000010827     D DSTL42        E DS                  EXTNAME(TNTL42DS)
014100000627     D og143         E DS
014200960301     D DSTL34          DS
014300960301     D* PARAMETRI DI INPUT (FACOLTATIVI)
014400960301     D* =' ' MANUTENZIONE + AGGIORNAMENTO
014500960301     D* ='I' INTERROGAZIONE              PER ORA NON GESTITO
014600960301     D* ='A' AGGIORNAMENTO
014700960301     D  D34MOD                 1      1
014800960222     D*
014900960222     D* CONSTANTI
015000960222     D CANNUL          C                   CONST(' A N N U L L A T O')
015100960305     D CABBIN          C                   CONST('Abbinato a FV Reale')
015200000214     D*---------------------
015300000214     D* DS per reperimento società
015400000214     D*---------------------
015500000214     D soc001        E DS                  EXTNAME(xsoc001ds)
015600000214     D xsocds          DS          1000
015700000214     D*---------------------
015800960305     ITNTSR003
015900960305     I              TSRATB                      T03ATB
016000960305     I              TSRTRN                      T03TRN
016100960305     I              TSRDTN                      T03DTN
016200960305     I              TSRDTT                      T03DTT
016300960305     I              TSRNFP                      T03NFP
016400960305     I              TSRFFP                      T03FFP
016500960305     I              TSRNFA                      T03NFA
016600960305     I              TSRFFA                      T03FFA
016700960305     I              TSRDTP                      T03DTP
016800960305     I              TSRHTP                      T03HTP
016900960305     I              TSRDRP                      T03DRP
017000960305     I              TSRHRP                      T03HRP
017100960305     I              TSRRTP                      T03RTP
017200960305     I              TSRDTA                      T03DTA
017300960305     I              TSRHTA                      T03HTA
017400960305     I              TSRDRA                      T03DRA
017500960305     I              TSRHRA                      T03HRA
017600960305     I              TSRRTA                      T03RTA
017700960305     I              TSRDKP                      T03DKP
017800960305     I              TSRFKP                      T03FKP
017900960305     I              TSRDKA                      T03DKA
018000960305     I              TSRFKA                      T03FKA
018100960305     I              TSRDKI                      T03DKI
018200960305     I              TSRFKI                      T03FKI
018300960305     I              TSRTFM                      T03TFM
018400960305     I              TSRLNP                      T03LNP
018500960305     I              TSRNFV                      T03NFV
018600960305     I              TSRDFV                      T03DFV
018700951009     C****************************************************************
018800951009     C*  RIEPILOGO INDICATORI
018900951009     C***************************************************************
019000951102     C* 01    - GESTIONE PER DATA TRAINO
019100951102     C* 02    - IMMESSO TERMINAL PARTENZA TRAINO
019200951127     C* 03    - IMMESSO NUMERO TRAINO
019300951127     C* 04    -
019400951215     C* 05    - GESTIONE PER DATA FV TEORICA
019500951107     C* 06    - IMMESSA LNP FV
019600951127     C* 07    - IMMESSO NUM FV
019700951103     C* 09    - IMMESSA DATA DAL/AL
019800951103     C* 10    - ABILITATO F20 E F19 SE RICHIESTE + DATE E + TERMINAL
019900951103     C* 11    - OFF-EMETTO DATI TESTATA   ON-EMETTO DATI TRATTE
020000951103     C* 12    - PROTEZIONE CAMPI DI INPUT SE GIA' CONTABILIZZATA
020100951215     C* 13    - METTO IN RI NUMERO FV SE NON C'E' O DATA FV<>DAT.FV TEOR
020200951215     C* 14    - PROTEZIONE CAMPI DEL FV SE NON RICHIESTA VIS-SFL3
020300951109     C* 15    - PROTEZIONE CAMPP VISUALIZZAZIONE RITARDO PARTENZA
020400951109     C* 16    - PROTEZIONE CAMPO VISUALIZZAZIONE RITARDO ARRIVO
020500951108     C* 17    - RI DELL'IMPORTO LISTINO SE CAMBIATO
020600010911     C* 18    - NON VISUALIZZAZIONE di alcuni campi dalla seconda tratta in poi
020700951115     C* 19    - RI DELL'IMPORTO DA CONFERMARE SE PROPOSTO IN AUTOMATICO
020800960301     C* 20    - TRAINO ANNULLATO: VISUALIZZO GRIGLIA
020900951214     C* 21/23 - INDICATORI SI SFL
021000951214     C* 25/26 - VARIAZIONE DEI CAMPI PER LA CONFERMA DEGLI IMPORTI
021100960625     C* 27    - PREMUTO F7=MANUTENZIONE IN VIDEATA VIS.GRIGLIA (27 ON)
021200960710     C*         + PROTEZIONE CAMPI METRI CUBI (PER 27 OFF)
021300951121     C* 31    - COMODO
021400960625     C* 35/39 - OFF --> PROTEZIONE CAUSALI RITARDO ANTICIPO
021500961029     C* 87    - COMODO
021600960305     C* 88    - TRATTA ANNULLATA
021700960305     C* 89    - ANNULLAMENTO SUI MC
021800960301     C* 90    - ERRORE GENERICO DSPF
021900951214     C* 91    - PROTEZIONE CAMPO VISUALIZZAZIONE RITARDO ARRIVO
022000951214     C* 92    - PROTEZIONE CAMPO VISUALIZZAZIONE RITARDO ARRIVO
022100951214     C* 93    - PROTEZIONE CAMPO VISUALIZZAZIONE RITARDO ARRIVO
022200960213     C* 94    - PROTEZIONE CAMPO FV
022300960213     C* 95    - PROTEZIONE CAMPO ATB
022400960322     C* 96    - BASSA INTENSITA DEL CAMPO NFV IN SFL4 SE NO CONTROLLATO
022500960213     C* 97    - PROTEZIONE CAMPI TRAZIONISTA E CAUSALE CONTABILIZZAZ
022600960213     C* 98    - PROTEZIONE CAMPO IMPORTO DA CONFERMARE
022700951009     C***************************************************************
022800960301     C* SOLO AGGIORNAMENTO
022900960301     C     D34MOD        IFEQ      'A'
023000960301     C                   EXSR      REGIS2
023100960301     C                   ELSE
023200960301     C**
023300950922     C     INIZIO        TAG
023400010824     C                   CLEAR                   TL44D01
023500950921     C* Emissione formato video
023600951102     C     FOR01         TAG
023700010824     C                   WRITE     TL44T01
023800010824     C                   EXFMT     TL44D01
023900951102     C                   SETOFF                                       9028
024000951102     C* F3 - FINE LAVORO
024100950921     C     *INKC         CABEQ     *ON           FINE
024200951102     C*
024300950921     C* Richiamo routine controlli
024400950927     C                   EXSR      CTRD01
024500951102     C   90              GOTO      FOR01
024600960322     C*
024700960322     C                   CLEAR                   WRICH
024800960322     C**
024900951102     C** G E S T I O N E   P E R   D A T A   T R A I N O
025000951103     C   01              EXSR      GESTRN
025100951103     C**
025200951103     C** G E S T I O N E   P E R   D A T A   T R A T T A
025300951214     C   07
025400951214     COR 05              EXSR      GESTRT
025500960305     C**
025600960305     C* PULISCO I RECORD RIMASTI IL TNTSW
025700960322     C                   CLEAR                   KSFL
025800951107     C                   EXSR      DELTSW
025900951103     C   90
026000951103     COR KL              GOTO      FOR01
026100951103     C   KC              GOTO      FINE
026200951103     C   KF              GOTO      INIZIO
026300951103     C*
026400950921     C     FINE          TAG
026500960301     C                   ENDIF
026600960226     C*
026700960226     C* SE DEVO, RICHIAMO IL PGM DELLE DATE/ORA ARRIVO REALI
026800960226     C     WTL26         IFEQ      '1'
026900960226     C                   CALL      'TNTL26R'
027000960226     C                   PARM                    KPJBA
027100960226     C                   ENDIF
027200960904     C* RICHIAMO PGM PER CHIUSURA FILES
027300960904    2C     WCL33         IFEQ      '1'
027400960904     C                   MOVEL     'C'           D33TLA
027500960904     C                   MOVEL     DSTL33        KPJBU
027600960904     C                   CALL      'TNTL33R'
027700960904     C                   PARM                    KPJBA
027800960904    2C                   END
027900971204     C                   CLEAR                   DSLV55
028000971204     C                   MOVEL     'C'           D55TLA
028100971204     C                   CALL      'FNLV55R'
028200971204     C                   PARM                    DSLV55
028300960226     C*
028400950921     C                   SETON                                        LR
028500951103     C**
028600950921     C****************  CONTROLLI VIDEO   *****************************
028700950927     C     CTRD01        BEGSR
028800951102     C                   SETOFF                                       404142
028900951102     C                   SETOFF                                       434445
029000951114     C                   SETOFF                                       464748
029100951127     C                   SETOFF                                       49
029200951114     C                   SETOFF                                       289004
029300951102     C                   SETOFF                                       010203
029400951127     C                   SETOFF                                       050607
029500951127     C                   SETOFF                                       09
029600951102     C                   CLEAR                   AMGTRD
029700951102     C                   CLEAR                   AMGTRA
029800951102     C                   CLEAR                   AMGTTD
029900951102     C                   CLEAR                   AMGTTA
030000951106     C                   CLEAR                   WTRN
030100951113     C                   CLEAR                   WPDR
030200951124     C* REPERISCO LA DATA AREA PER VERIFICARE SE POSSO MANUTENZ
030300951124     C                   IN        TNTL30
030400951102     C*
030500951102     C* IMMETTERE SOLO UNA DELLE SCELTE
030600951102     C     V1CTRD        IFNE      0
030700951102     C     V1CTRA        ORNE      0
030800951102     C     V1CTFP        ORNE      0
030900951113     C     V1CPDR        ORNE      *BLANKS
031000951106     C     V1CTRN        ORNE      *ZEROS
031100951109     C     V1CTRN        ANDNE     *BLANKS
031200951102     C*
031300951102     C     V1CTTD        IFNE      0
031400951102     C     V1CTTA        ORNE      0
031500951102     C     V1CLNP        ORNE      0
031600951127     C     V1CNFV        ORNE      0
031700951102     C                   SETON                                        409028
031800951102     C                   MOVEL     MSG(1)        V1CMSG
031900951102     C                   GOTO      ENDCT1
032000951102     C                   ENDIF
032100951102     C                   ENDIF
032200951109     C     V1CTRD        IFEQ      0
032300951109     C     V1CTRA        ANDEQ     0
032400951109     C     V1CTFP        ANDEQ     0
032500951113     C     V1CPDR        ANDEQ     *BLANKS
032600951109     C     V1CTTD        ANDEQ     0
032700951109     C     V1CTTA        ANDEQ     0
032800951109     C     V1CLNP        ANDEQ     0
032900951127     C     V1CNFV        ANDEQ     0
033000951109     C     V1CTRN        IFEQ      *ZEROS
033100951109     C     V1CTRN        OREQ      *BLANKS
033200951109     C                   SETON                                        409028
033300951109     C                   MOVEL     MSG(1)        V1CMSG
033400951109     C                   GOTO      ENDCT1
033500951109     C                   ENDIF
033600951109     C                   ENDIF
033700951109     C*
033800951109     C* IMMETTERE SOLO O NUM TRAINO O TERMINAL PARTENZA
033900951106     C     V1CTRN        IFNE      *ZEROS
034000951106     C     V1CTRN        ANDNE     *BLANKS
034100951102     C     V1CTFP        ANDNE     0
034200951102     C                   SETON                                        429028
034300951102     C                   MOVEL     MSG(11)       V1CMSG
034400951102     C                   GOTO      ENDCT1
034500951102     C                   ENDIF
034600951113     C* SE IMMESSO TRAINO O TERMINAL PARTENZA O PADRONCINO
034700951113     C*  IMMETTERE ANCHE LE DATE
034800951106     C     V1CTRN        IFNE      *ZEROS
034900951109     C     V1CTRN        ANDNE     *BLANKS
035000951102     C     V1CTFP        ORNE      0
035100951113     C     V1CPDR        ORNE      *BLANKS
035200951102     C*
035300951102     C     V1CTRD        IFEQ      0
035400951102     C     V1CTRA        ANDEQ     0
035500951102     C                   SETON                                        409028
035600951102     C                   MOVEL     MSG(12)       V1CMSG
035700951102     C                   GOTO      ENDCT1
035800951102     C                   ENDIF
035900951102     C                   ENDIF
036000951102     c*
036100951127     C* SE IMMESSA LNP FV E NON NUMERO FV IMMETTERE ANCHE LE DATE
036200951102     C     V1CLNP        IFNE      0
036300951127     C     V1CNFV        ANDEQ     0
036400951109     C     V1CTTD        ANDEQ     0
036500951109     C     V1CTTA        ANDEQ     0
036600951102     C                   SETON                                        439028
036700951102     C                   MOVEL     MSG(13)       V1CMSG
036800951102     C                   GOTO      ENDCT1
036900951102     C                   ENDIF
037000951215     C* SE IMMESSO NUMERO FV IMMETTERE ANCHE LNP FV
037100951127     C     V1CNFV        IFNE      0
037200951127     C     V1CLNP        ANDEQ     0
037300951127     C                   SETON                                        459028
037400951127     C                   MOVEL     MSG(35)       V1CMSG
037500951127     C                   GOTO      ENDCT1
037600951127     C                   ENDIF
037700951102     C**** CONTROLLI DATE
037800951102     C     V1CTRD        IFEQ      0
037900951102     C     V1CTRA        ANDGT     0
038000951102     C                   MOVEL     MSG(2)        V1CMSG
038100951102     C                   SETON                                        409028
038200951102     C                   GOTO      ENDCT1
038300951102    1C                   ENDIF
038400951106     C* DATA TRAINO DAL
038500951102    1C     V1CTRD        IFGT      0
038600951106     C                   CLEAR                   WLBDAT
038700951106     C                   MOVE      V1CTRD        G02DAT
038800950921     C                   CALL      'XSRDA8'
038900951106     C                   PARM                    WLBDAT
039000951214    2C     G02ERR        IFEQ      '1'
039100951102     C                   MOVEL     MSG(2)        V1CMSG
039200951102     C                   SETON                                        409028
039300951102     C                   GOTO      ENDCT1
039400951102    2C                   END
039500951106     C                   MOVE      G02DAT        V1CTRD
039600951106     C                   MOVE      G02INV        AMGTRD
039700951106     C                   Z-ADD     G02TGI        WTOTGG
039800951102     C*
039900950922     C* Controllo data al se immessa
040000951102    2C     V1CTRA        IFNE      *ZEROS
040100951106     C                   CLEAR                   WLBDAT
040200951106     C                   MOVE      V1CTRA        G02DAT
040300950921     C                   CALL      'XSRDA8'
040400951106     C                   PARM                    WLBDAT
040500951106    3C     G02ERR        IFEQ      '1'                                          ERRORE
040600951102     C                   MOVEL     MSG(2)        V1CMSG
040700951102     C                   SETON                                        412890
040800951102     C                   GOTO      ENDCT1
040900951102   X3C                   ELSE
041000951106     C                   MOVE      G02DAT        V1CTRA
041100951106     C                   MOVE      G02INV        AMGTRA
041200951102    3C                   ENDIF
041300950922     C* Controllo se data dal > data al
041400951102    3C     AMGTRD        IFGT      AMGTRA
041500951102     C                   MOVEL     MSG(3)        V1CMSG
041600951109     C                   SETON                                        402890
041700951102     C                   GOTO      ENDCT1
041800951102    3C                   ENDIF
041900951102    2C                   ENDIF
042000951102     C*
042100951102     C* CONTROLLO DATA CON DATA AREA
042200960322     C     AMGTRD        IFGT      0
042300951102     C                   Z-ADD     AMGTRD        W0080
042400951103     C                   Z-ADD     AMGTRA        WDTAL
042500951102     C                   EXSR      CTRDAT
042600960322     C     *IN90         IFEQ      *ON
042700960322     C                   SETON                                        40
042800960322     C                   GOTO      ENDCT1
042900960322     C                   ENDIF
043000960322     C                   ENDIF
043100951102    1C                   ENDIF
043200951113     C*
043300951113     C* TRAZIONISTA
043400951113     C* SE SELEZIONE=" " NON POSSO INDICARE UN CODICE PADRONCINO
043500951113     C     V1CSPD        IFEQ      ' '
043600951113     C     V1CPDR        IFNE      *ZEROS
043700951113     C     V1CPDR        ANDNE     *BLANKS
043800951114     C                   MOVEL     MSG(26)       V1CMSG
043900951113     C                   SETON                                        289048
044000951113     C                   GOTO      ENDCT1
044100951113     C                   ENDIF
044200951113     C                   ENDIF
044300951113     C*
044400951113    1C     V1CPDR        IFNE      *BLANKS
044500951113    1C     V1CPDR        ANDNE     *ZEROS
044600951113     C* RICERCA
044700951113     C     '?'           SCAN      V1CPDR                                 90
044800951113    2C     *IN90         IFEQ      *ON
044900000807     C                   EVAL      PA3TIP = 'T '                                PAR-1
045000000807     C                   MOVEL(P)  V1DPDR        PA3RSC                         PAR-2
045100000807     C                   CLEAR                   PA3CSF                         PAR-3
045200000807     C                   CLEAR                   PA3PDR                         PAR-4
045300000807     C                   CLEAR                   PA3FLG                         PAR-5
045400951113     C                   MOVEL     PARAM3        KPJBU
045500951113     C                   CALL      'TRUL16R'
045600951113     C                   PARM                    KPJBA
045700951113     C                   MOVEL     KPJBU         PARAM3
045800951113     C* SE PA3FLG = "3" --> NON ESISTONO TRAZIONISTI CHIAVE ALFABETICA
045900951113     C*                     RICHIESTA
046000951113    3C     PA3FLG        IFEQ      '3'
046100951113     C                   CLEAR                   V1CPDR
046200951114     C                   MOVEL     MSG(20)       V1CMSG
046300951113     C                   SETON                                        28
046400951113   X3C                   ELSE
046500000807     C                   MOVEL     PA3PDR        V1CPDR                         CODICE
046600000807     C                   MOVEL     PA3RSC        V1DPDR                         RAG.SOCIALE
046700951113    3C                   ENDIF
046800951113     C                   SETON                                        47
046900951113     C                   GOTO      ENDCT1
047000951113    2C                   ENDIF
047100951113     C*
047200951113     C                   TESTN                   V1CPDR               31
047300951114     C                   MOVE      V1CPDR        W001A             1
047400951113    2C     *IN31         IFEQ      *OFF
047500951113     C     W001A         ORLT      '0'
047600951114     C                   MOVEL     MSG(21)       V1CMSG
047700951113     C                   SETON                                        289047
047800951113     C                   GOTO      ENDCT1
047900951113   X2C                   ELSE
048000951113     C* CONTROLLO SULL'ANAGRAFICA
048100951113     C                   MOVEL     V1CPDR        WPDR
048200000807     C                   Z-ADD     WPDR          KTLPDR
048300000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
048400951113    3C     *IN31         IFEQ      *ON
048500951113     C     TLZATB        ORNE      ' '
048600951114     C                   MOVEL     MSG(21)       V1CMSG
048700951113     C                   SETON                                        289047
048800951113     C                   GOTO      ENDCT1
048900951113   X3C                   ELSE
049000951113     C                   MOVEL     TLZRSC        V1DPDR
049100951113    3C                   ENDIF
049200951113    2C                   ENDIF
049300951113    1C                   ENDIF
049400951102     C*
049500951102     C* TERM PARTENZA TRAINO
049600951106    1C     V1CTFP        IFGT      0
049700951102     C     V1CTFP        CHAIN     AZORG01L                           30
049800951106    2C     *IN30         IFEQ      *ON
049900951102     C     ORGFVA        ORNE      *BLANKS
050000971204     C     ORGFAG        ORNE      'A'
050100971204     C     ORGFAG        ANDNE     'F'
050200951102     C                   MOVEL     MSG(9)        V1CMSG
050300951102     C                   SETON                                        422890
050400951102     C                   GOTO      ENDCT1
050500951102    2C                   ENDIF
050600951102     C* Non è terminal
050700000627     c*  se e' poste a bene
050800000627     c                   movel     orgde3        og143
050900000627     c     §ogpt         ifne      'S'
051000971204     C                   CLEAR                   DSLV55
051100971204     C                   MOVEL     'P'           D55TPT
051200971204     C                   MOVEL     V1CTFP        D55LIN
051300971204     C                   MOVEL     AMGTRD        D55DRF
051400971204     C                   CALL      'FNLV55R'
051500971204     C                   PARM                    DSLV55
051600971204    2C     D55ERR        IFNE      ' '
051700971204     C     D55TFP        ORNE      V1CTFP
051800951102     C                   SETON                                        429028
051900951102     C                   MOVEL     MSG(10)       V1CMSG
052000951102     C                   GOTO      ENDCT1
052100951102    2C                   ENDIF
052200000627    2C                   ENDIF
052300951102    1C                   ENDIF
052400951102     c*
052500951215     C* CONTROLLO DATA FV T E LNP FV
052600951106    1C     V1CTTA        IFGT      0
052700951102     C     V1CTTD        ANDEQ     0
052800951102     C                   MOVEL     MSG(4)        V1CMSG
052900951102     C                   SETON                                        439028
053000951102     C                   GOTO      ENDCT1
053100951106    1C                   ENDIF
053200951215     C* DATA FV T DAL
053300951102    1C     V1CTTD        IFGT      0
053400951106     C                   CLEAR                   WLBDAT
053500951106     C                   MOVE      V1CTTD        G02DAT
053600951102     C                   CALL      'XSRDA8'
053700951106     C                   PARM                    WLBDAT
053800951106    2C     G02ERR        IFEQ      '1'
053900951102     C                   MOVEL     MSG(4)        V1CMSG
054000951102     C                   SETON                                        439028
054100951102     C                   GOTO      ENDCT1
054200951102    2C                   END
054300951106     C                   MOVE      G02DAT        V1CTTD
054400951106     C                   MOVE      G02INV        AMGTTD
054500951106     C                   Z-ADD     G02TGI        WTOTGG
054600951102     C* Controllo data al se immessa
054700951102    2C     V1CTTA        IFNE      *ZEROS
054800951106     C                   CLEAR                   WLBDAT
054900951106     C                   MOVE      V1CTTA        G02DAT
055000951102     C                   CALL      'XSRDA8'
055100951106     C                   PARM                    WLBDAT
055200951106    3C     G02ERR        IFEQ      '1'
055300951102     C                   MOVEL     MSG(4)        V1CMSG
055400951102     C                   SETON                                        442890
055500951102     C                   GOTO      ENDCT1
055600951102   X3C                   ELSE
055700951106     C                   MOVE      G02DAT        V1CTTA
055800951106     C                   MOVE      G02INV        AMGTTA
055900951102    3C                   ENDIF
056000951102     C* Controllo se data dal > data al
056100951102    3C     AMGTTD        IFGT      AMGTTA
056200951102     C                   MOVEL     MSG(5)        V1CMSG
056300951109     C                   SETON                                        432890
056400951102     C                   GOTO      ENDCT1
056500951102    3C                   ENDIF
056600951102    2C                   ENDIF
056700951102     C*
056800951102     C* CONTROLLO DATA CON DATA AREA
056900960322     C     AMGTTD        IFGT      0
057000951102     C                   Z-ADD     AMGTTD        W0080
057100951103     C                   Z-ADD     AMGTTA        WDTAL
057200951102     C                   EXSR      CTRDAT
057300960322     C     *IN90         IFEQ      *ON
057400960322     C                   SETON                                        43
057500960322     C                   GOTO      ENDCT1
057600960322     C                   ENDIF
057700960322     C                   ENDIF
057800951102    1C                   ENDIF
057900951102     C*
058000951102     C* LINEA PARTENZA FV
058100951106    1C     V1CLNP        IFGT      0
058200951102     C     V1CLNP        CHAIN     AZORG01L                           30
058300951106    2C     *IN30         IFEQ      *ON
058400951102     C     ORGFVA        ORNE      *BLANKS
058500971204     C     ORGFAG        ORNE      'A'
058600971204     C     ORGFAG        ANDNE     'F'
058700951102     C                   MOVEL     MSG(6)        V1CMSG
058800951102     C                   SETON                                        452890
058900951102     C                   GOTO      ENDCT1
059000951102    2C                   ENDIF
059100951102     C* Non è terminal
059200971204    2C     AMGTTD        IFGT      0
059300971204     C                   CLEAR                   DSLV55
059400971204     C                   MOVEL     'P'           D55TPT
059500971204     C                   MOVEL     V1CLNP        D55LIN
059600971204     C                   MOVEL     AMGTTD        D55DRF
059700971204     C                   CALL      'FNLV55R'
059800971204     C                   PARM                    DSLV55
059900971204    3C     D55ERR        IFNE      ' '
060000971204     C     D55TFP        ORNE      V1CLNP
060100951102     C                   SETON                                        459028
060200951102     C                   MOVEL     MSG(7)        V1CMSG
060300951102     C                   GOTO      ENDCT1
060400971204    3C                   ENDIF
060500971204    2C                   ENDIF
060600971204    1C                   ENDIF
060700951127     c* se immesso numero fv lo controllo
060800951127    1C     V1CNFV        IFGT      0
060900951127     C* VED SE E' ABBINATO
061000951127     C                   Z-ADD     V1CLNP        KLNP
061100951127     C                   Z-ADD     V1CNFV        KNFV
061200951212     C     KTSR4         SETLL     TNTSR04L                               31
061300951127     C* SE NON TROVATO VEDO SE E' NON ABBINATO O INESISTENTE
061400951127    2C     *IN31         IFEQ      *OFF
061500960223     C     KTSR4         CHAIN(N)  TNFVS01L                           31
061600951127    3C     *IN31         IFEQ      *OFF
061700960223     C     FVSATB        ANDEQ     ' '
061800951127     C                   MOVEL     MSG(36)       V1CMSG
061900951127     C                   ELSE
062000951127     C                   MOVEL     MSG(29)       V1CMSG
062100951127    3C                   ENDIF
062200951127     C*
062300960322     C                   SETON                                        492890
062400951127     C                   GOTO      ENDCT1
062500960306    2C                   ENDIF
062600951127    1C                   ENDIF
062700951102     c*
062800950922     C**** CONTROLLI NUMERO TRAINO
062900950922     C* Gestione '?' su numero traino
063000951102     C     '?'           SCAN      V1CTRN                                 90
063100951102    1C     *IN90         IFEQ      *ON
063200951130     C                   CLEAR                   DSTL00
063300951130     C     AMGTRA        IFEQ      *ZEROS
063400951130     C                   MOVE      AMGTRD        D00DDE
063500951130     C                   END
063600951130     C                   MOVEL     DSTL00        KPJBU
063700951130     C                   CALL      'TNTL17R'
063800951130     C                   PARM                    KPJBA
063900951130     C                   MOVEL     KPJBU         DSTL00
064000951130     C                   MOVE      D00TRN        V1CTRN
064100951102     C                   SETON                                        46
064200951102     C                   GOTO      ENDCT1
064300951102    1C                   ENDIF
064400950922     C* Controllo numero traino se immesso
064500950922    1C     V1CTRN        IFNE      *BLANKS
064600950922     C     V1CTRN        ANDNE     *ZEROS
064700950922     C                   TESTN                   V1CTRN               30
064800950922    2C     *IN30         IFEQ      *ON
064900950922     C                   MOVE      V1CTRN        W001A             1
065000950922    3C     W001A         IFLT      '0'
065100950922     C                   SETOFF                                       30
065200950922    3C                   END
065300950922    2C                   END
065400950922    2C     *IN30         IFEQ      *OFF
065500951102     C                   SETON                                        469028
065600951102     C                   MOVEL     MSG(8)        V1CMSG
065700951102     C                   GOTO      ENDCT1
065800950922    2C                   END
065900951102     C                   MOVE      V1CTRN        WTRN
066000951106     C                   MOVE      V1CTRN        KTRN
066100951102     C* CONTROLLO SE ESISTE IL TRAINO IMMESSO NEL RANGE DI DATE
066200951102     C                   Z-ADD     AMGTRD        KDTN
066300951102     c*
066400951106    2C     V1CTRA        IFNE      *ZEROS
066500951214     C     KTS1          SETLL     TNTST01L
066600951106     C     KTRN          READE     TNTST01L                               30
066700951102     C* 30 ON - NON TROVATO O CON DATA TRAINO SUPERIORE
066800951102    3C     *IN30         IFEQ      *ON
066900951102     C     TSRDTN        ORGT      AMGTRA
067000951102     C                   SETON                                        469028
067100951102     C                   MOVEL     MSG(14)       V1CMSG
067200951102     C                   GOTO      ENDCT1
067300951102    3C                   ENDIF
067400951102     c* NON C'E' DATA AL --> CERCO SOLO CON DATA DAL
067500951102   x2C                   ELSE
067600951214     C     KTS1          CHAIN     TNTST01L                           30
067700951102    3C     *IN30         IFEQ      *ON
067800951109     C                   SETON                                        469028
067900951102     C                   MOVEL     MSG(14)       V1CMSG
068000951102     C                   GOTO      ENDCT1
068100951102    3C                   ENDIF
068200951102    2C                   ENDIF
068300951102    1C                   ENDIF
068400951102     C*
068500951102     C* STABILISCO QUALE SFL EMETTERE
068600951106    1C     AMGTRD        IFGT      0
068700951102     C*
068800951102     C                   SETON                                        01        DATA TRAINO
068900951102     C* IMMESSA DATA AL
069000951106    2C     AMGTRA        IFGT      0
069100951115    2C     AMGTRA        ANDGT     AMGTRD
069200951102     C                   SETON                                        09
069300951102     C                   ELSE
069400951102     C                   Z-ADD     AMGTRD        AMGTRA
069500951106    2C                   ENDIF
069600951102     C*
069700951106    2C     V1CTFP        IFGT      0
069800951102     C                   SETON                                        02        TERPAR
069900951108     C                   ENDIF
070000951108    2C     WTRN          IFGT      0
070100951102     C                   SETON                                        03        NUM.TRAINO
070200951106    2C                   ENDIF
070300951114    2C     V1CSPD        IFEQ      '1'
070400951124     C                   SETON                                        04        TRAZIONISTA
070500951113    2C                   ENDIF
070600951215     C* PER DATA FVT
070700951106   X1C                   ELSE
070800951127     C     AMGTTD        IFGT      0
070900951215     C                   SETON                                        05        DATA FVT
071000951127     C                   ENDIF
071100951102     C* IMMESSA DATA AL
071200951106    2C     AMGTTA        IFGT      0
071300951102     C                   SETON                                        09
071400951102     C                   ELSE
071500951102     C                   Z-ADD     AMGTTD        AMGTTA
071600951106    2C                   ENDIF
071700951102     C*
071800951106    2C     V1CLNP        IFGT      0
071900951102     C                   SETON                                        06        LNP FV
072000951106    2C                   ENDIF
072100951127    2C     V1CNFV        IFGT      0
072200951127     C                   SETON                                        07        NUMERO FV
072300951127    2C                   ENDIF
072400951106    1C                   ENDIF
072500951102     C*
072600951102     C     ENDCT1        ENDSR
072700951102     C**
072800951106     C****************  GESTIONE PER DATA TRAINO SFL2 *****************
072900951102     C     GESTRN        BEGSR
073000951103     C                   SETOFF                                       10
073100951106     C                   Z-ADD     2             KSFL
073200951214     C                   Z-ADD     19            WRIPAG
073300951103     C                   CLEAR                   KTFP
073400951103     C                   CLEAR                   KDTN
073500951215     C                   CLEAR                   WPOSIZ
073600960325     C* SE ROUTINE RICHIAMATA DAL SFL4 CARICO, LEGGENDO IL SFL
073700960322    1C     WRICH         IFEQ      ' '
073800960322     C*
073900960322     C* SE IMMESSA 1 DATA SOLA/1 TERMINAL SOLO/1 NUMERO TRAINO SOLO
074000951106     C*  -->  SOLO TASTO ROLLUP, SENZA F20 E F19  - 10 OFF -
074100951103     C     *IN09         IFEQ      *ON
074200951103     C     *IN02         ANDEQ     *OFF
074300951103     C     *IN03         ANDEQ     *OFF
074400951113     C     *IN04         ANDEQ     *OFF
074500951103     C                   SETON                                        10
074600951103     C                   ENDIF
074700960322    1C                   ENDIF
074800951103     C*
074900951113     C* C A R I C A M E N T O   S F L C T L
075000951106     C                   CLEAR                   V2CRTP
075100951102     C                   CLEAR                   V2CRDT
075200951113     C                   MOVE      WTRN          V2CNTS
075300951114     C     V1CPDR        IFLE      *ZEROS
075400951114     C  N04              CLEAR                   V1CPDR
075500951114     C   04              MOVEL     *ZEROS        V1CPDR
075600951113     C                   ENDIF
075700951113     C                   CLEAR                   K13
075800951102     C* DATA TRAINO DAL
075900951102     C     V1CTRD        IFGT      0
076000951113     C                   MOVE      V1CTRD        W002A
076100951113     C                   MOVEL     V1CTRD        W006A
076200951113     C                   MOVEA     W006A         K13(1)
076300951113     C                   MOVEA     W002A         K13(5)
076400951102     C                   ENDIF
076500951102     C* DATA TRAINO AL
076600951102     C     V1CTRA        IFGT      0
076700951113     C                   MOVEA     '-'           K13(7)
076800951113     C                   MOVE      V1CTRA        W002A             2
076900951113     C                   MOVEL     V1CTRA        W006A             6
077000951113     C                   MOVEA     W006A         K13(8)
077100951113     C                   MOVEA     W002A         K13(12)
077200951102     C                   ENDIF
077300951113     C                   MOVEA     K13           V2DDAT
077400951124     C*
077500951124     C* IMPOSTO LA RIGA DI TASTI FUNZIONALI: LA PIRMA
077600951212     C   10              MOVEL     FN2S(1)       V2CFUN
077700951212     C  N10              MOVEL     FN2N(1)       V2CFUN
077800951124     C* WF CONTIENE LA RIGA DI TASTI FUNZIONALI IN VISUALIZZAZIONE
077900951124     C                   Z-ADD     1             WF                2 0
078000951102     C* PULIZIA SFL
078100951214     C                   EXSR      PULSFL
078200951102     C*
078300951102     C* C A R I C A M E N T O  S F L
078400960322    1C     WRICH         IFEQ      ' '
078500951102     C                   Z-ADD     AMGTRD        KDTN
078600951103     C*
078700951103     C* SE E' ABILITATO F20 CERCO UNA DATA ALL'INTERNO DELLA QUALE
078800951113     C*  CARICARE I TRAINI
078900960322    2C     *IN10         IFEQ      *ON
079000951103     C                   Z-ADD     AMGTRD        W0080
079100951103     C                   Z-ADD     AMGTRD        WDTDAL
079200951103     C                   Z-ADD     AMGTRA        WDTAL
079300951106     C                   Z-ADD     1             WTOADD            1 0
079400951212     C                   EXSR      RICDTN
079500951113     C                   ELSE
079600951113     C                   Z-ADD     V1CTFP        KTFP
079700960322    2C                   ENDIF
079800951103     C*
079900951106     C                   Z-ADD     WTRN          KTRN
080000951113     C                   Z-ADD     WPDR          KPDR
080100951113     C* POSIZIONAMENTO TNTST
080200951113     C                   EXSR      SETST2
080300951103     C*
080400951103     C* LETTURA PER CARICAMENTO SFL
080500951106     C                   CLEAR                   WF20
080600951113     C                   MOVEL     'S'           WLCTE
080700951116     C                   Z-ADD     0             L                 5 0
080800951102     C                   EXSR      LEGS02
080900951103     C*
081000951103     C* SE NON HO CARICATO NULLA --> ERRORE
081100960322    2C     NRR2          IFEQ      0
081200951103     C                   MOVEL     MSG(15)       V1CMSG
081300951103     C                   SETON                                        289040
081400951103     C                   GOTO      ENDGTR
081500960322    2C                   ENDIF
081600960322     C*
081700960322   X1C                   ELSE
081800960322     C* LETTURA DEL SFL4
081900960322     C                   SETOFF                                       31
082000960322     C                   SETON                                        0103
082100960322     C                   Z-ADD     99999         WRIPAG
082200960322     C                   Z-ADD     0             L                 5 0
082300960322     C                   CLEAR                   WF20
082400960322    2C     *IN31         DOWEQ     *OFF
082500960322     C*
082600960322    3C     V4CSCE        IFEQ      'M'
082700960322     C* SISTEMO GLI INDICATORI
082800960325     C                   Z-ADD     4             KSFL
082900960322     C                   EXSR      CTRIND
083000960322     C                   CLEAR                   V4CSCE
083100010824     C                   UPDATE    TL44S04
083200960325     C* SALVO L'ULTIMO RECORD SU CUI HO MESSO LA "M"
083300960325     C                   Z-ADD     NRR4          SAVNR4            4 0
083400960325     C                   Z-ADD     2             KSFL
083500960322     C*
083600960322     C                   Z-ADD     V4CTRN        WTRN
083700960322     C                   Z-ADD     V4HDTN        AMGTRD
083800960322     C                   Z-ADD     V4HDTN        AMGTRA
083900960322     C                   Z-ADD     AMGTRD        KDTN
084000960322     C                   SETOFF                                       99
084100960322     C* POSIZIONAMENTO TNTST
084200960322     C                   EXSR      SETST2
084300960322     C* LETTURA PER CARICAMENTO SFL
084400960322     C                   MOVEL     'S'           WLCTE
084500960322     C                   EXSR      LEGS02
084600960322    2C                   ENDIF
084700960322     C*
084800010824     C                   READC     TL44S04                                31
084900960322    1C                   ENDDO
085000960322     C                   SETOFF                                       0103
085100960322     C                   SETON                                        99
085200960322    0C                   ENDIF
085300960322     C*
085400951115     C                   Z-ADD     1             V2CREC
085500951103     C*
085600951103     C     FORS02        TAG
085700010824     C                   WRITE     TL44T01
085800010824     C                   WRITE     TL44Z02
085900010824     C                   EXFMT     TL44C02
086000951106     C*
086100951106     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
086200951115     C                   SETOFF                                       2890
086300951115     C                   SETOFF                                       404142
086400951115     C                   SETOFF                                       5051
086500951115     C                   SETOFF                                       4344
086600960325     C                   Z-ADD     V2SNR1        V2CREC
086700951103     C*
086800951103     C* F3 - FINE
086900951103     C* F12 - RITORNO
087000951103     C   KL
087100951106     COR KC              GOTO      ENDGTR
087200951124     C* F24-ALTRI TASTI
087300951124     C     *INKY         IFEQ      *ON
087400951124     C*
087500951124     C     WF            IFLT      WRFUNZ
087600951124     C                   ADD       1             WF
087700951212     C   10              MOVEL     FN2S(WF)      V2CFUN
087800951212     C  N10              MOVEL     FN2N(WF)      V2CFUN
087900951124     C                   ELSE
088000951124     C                   Z-ADD     1             WF
088100951212     C   10              MOVEL     FN2S(1)       V2CFUN
088200951212     C  N10              MOVEL     FN2N(1)       V2CFUN
088300951124     C                   ENDIF
088400951124     C                   GOTO      FORS02
088500951124     C                   ENDIF
088600951108     C* ENTER CONTROLLI
088700951108     C                   EXSR      CTRS02
088800951108     C   90              GOTO      FORS02
088900951124     C*
089000951124     C* F8 -VISUALIZZA GRIGLIA DEL TRAINO
089100951124     C* F14-VISUALIZZA ANAGRAFICA TRAINO
089200960322     C* F10-VISUALIZZA FOGLIO VIAGGIO
089300951124    1C     *INKH         IFEQ      *ON
089400951124     C     *INKN         OREQ      *ON
089500960322     C     *INKJ         OREQ      *ON
089600951214     C                   Z-ADD     V2CPOS        KVPOS
089700951215     C                   EXSR      CALPG2
089800951124     C                   GOTO      FORS02
089900951124    1C                   ENDIF
090000951103     C*
090100951103     C* F19 - SINISTRA
090200951106     C   KTWF20          IFLE      0
090300951214     C                   MOVEL     MSG(17)       VSCMSG
090400951103     C                   SETON                                        28
090500951103     C                   GOTO      FORS02
090600951103     C                   ELSE
090700951108     C                   Z-ADD     -1            WTOADD
090800951103     C                   EXSR      F19F20
090900951103     C                   GOTO      FORS02
091000951103     C                   ENDIF
091100951103     C* F20 - DESTRA
091200951108     C     *INKU         IFEQ      *ON
091300951108     C                   Z-ADD     1             WTOADD
091400951103     C                   EXSR      F19F20
091500951103     C                   GOTO      FORS02
091600951103     C                   ENDIF
091700951103     C* 25 ON - ROLLUP
091800951106     C     *IN25         IFEQ      *ON
091900951116     C* FINE LETTURA --> EMETTO MESSAGGIO FINE SCORRIMENTO
092000951116     C     *IN99         IFEQ      *ON                                          FINEL LETTUR
092100951214     C                   MOVEL     MSG(19)       VSCMSG
092200951116     C                   SETON                                        28
092300951116     C                   ELSE
092400951116     C*
092500951103     C                   Z-ADD     1             WTOADD
092600951116     C                   Z-ADD     0             L
092700951115     C                   EXSR      LEGS02                                       LETTURA
092800951214     C                   EXSR      POSSFL                                       POS CURSORE
092900951116     C                   ENDIF
093000951103     C                   GOTO      FORS02
093100951103     C                   ENDIF
093200951103     C*
093300951108     C  NKF              GOTO      FORS02
093400960301     C**
093500960301     C* NON ESEGUO IN AGGIORNAMENTO
093600960301     C   KFD34MOD        IFEQ      ' '
093700960301     C                   EXSR      REGIS2
093800960301     C                   ENDIF
093900951102     C*
094000951102     C     ENDGTR        ENDSR
094100951124     C**
094200960221     C******** CHIAMO PGM DI VISUALIZZAZIONE GRIGLIA E ANAGRAFICA *****
094300951215     C     CALPG2        BEGSR
094400951127     C                   MOVEL     'S'           WPOSIZ            1
094500951124     C*
094600951215    1C     V2CPOS        IFGT      0
094700010824     C     V2CPOS        CHAIN     TL44S02                            31
094800951214     C                   Z-ADD     V2HTRN        KVTRN
094900951214     C                   Z-ADD     V2HDTN        KVDTN
095000951214    2C     V2HTRC        IFEQ      'R'
095100951214     C                   MOVEL     V2HNFP        KVNFP
095200960322     C                   MOVEL     V2CFFP        KVFFP
095300960322     C*
095400960322     C* ERRORE SE IL NUMERO DI FV MANCA
095500960325     C   KJV2CNFV        IFLE      *ZEROS
095600960322     C                   MOVEL     MSG(37)       VSCMSG
095700960322     C                   SETON                                        289042
095800960322     C                   SETON                                        22
095900960322     C                   EXSR      CTRIND
096000010824     C                   UPDATE    TL44S02
096100960322     C                   SETOFF                                       22
096200960322     C                   ENDIF
096300960322     C*
096400951214     C                   ELSE
096500951214     C                   CLEAR                   KVNFP
096600960222     C                   CLEAR                   KVFFP
096700960322     C* SE HO CHIESTO LA VISUALIZZAZIONE DI UN FV ERRORE
096800960322     C                   MOVEL     MSG(38)       VSCMSG
096900960322     C                   SETON                                        2890
097000951214    2C                   ENDIF
097100951214    1C                   ENDIF
097200960322     C* 90 ON - ERRORE
097300960322    0C     *IN90         IFEQ      *OFF
097400951124     C*
097500951124     C* ERRORE --> CURSORE NON POSIZIONATO SU RIGA DI TRAINO
097600951215    1C     V2CPOS        IFEQ      0
097700951124     C     *IN31         OREQ      *ON
097800951214     C                   MOVEL     MSG(33)       VSCMSG
097900951124     C                   SETON                                        2890
098000951124   X1C                   ELSE
098100951124     C*
098200951124    2C     *INKH         IFEQ      *ON                                          F8-GRIGIA
098300951124     C                   EXSR      VISGRL                                       VISUAL GRIGL
098400951124    2C                   ENDIF
098500960322     C**
098600960322     C     *INKJ         IFEQ      *ON
098700960322     C                   CLEAR                   DSTL37
098800960322     C                   MOVEL     'R'           D37FLG
098900960322     C                   MOVEL     V2CFFP        D37LNP
099000960322     C                   MOVEL     V2CNFV        D37NFV
099100960322     C                   MOVEL     DSTL37        KPJBU
099200960322     C                   CALL      'TNTL37R'
099300960322     C                   PARM                    KPJBA
099400960322     C                   ENDIF
099500951124     C**
099600951124    2C     *INKN         IFEQ      *ON                                          F14-INT ANAG
099700951124     C                   CLEAR                   DSTL00
099800951124     C                   MOVEL     'T05'         D00OP0
099900951213     C                   MOVEL     'O05'         D00OP1
100000951214     C                   Z-ADD     KVTRN         D00TRN
100100951124     C* CERCO LA DECORRENZA
100200951124     C                   EXSR      RICDEC
100300951124    3C     KDDE          IFEQ      0
100400951124     C                   Z-ADD     KDDE          D00DDE
100500951124    3C                   ENDIF
100600951124     C* VISULIZZO L'UNLTIMO SCADUTO
100700951124    3C     TLTDDE        IFGT      0
100800951124     C                   Z-ADD     TLTDDE        D00DDE
100900951124    3C                   ENDIF
101000951124     c*
101100951124    3C     D00DDE        IFGT      0
101200951124     C                   MOVEL     DSTL00        KPJBU
101300951124     C                   CALL      'TNTL01C'                                    VISUAL GRIGL
101400951124     C                   PARM                    KPJBA
101500951124   x3C                   ELSE
101600951124     C                   MOVEL     'E'           D00ERR
101700951124     C                   MOVEL     MSG(34)       D00MSG
101800951124    3C                   ENDIF
101900951124     C*
102000951124    3C     D00MSG        IFNE      *BLANKS
102100951214     C                   MOVEL     D00MSG        VSCMSG
102200951124     C                   SETON                                        2890
102300951124    3C                   ENDIF
102400951124    2C                   ENDIF
102500951124     c**
102600951215     C                   Z-ADD     V2CPOS        V2CREC
102700951214    1C                   ENDIF
102800960322    0C                   ENDIF
102900951124     C                   ENDSR
103000951215     C**
103100951215     C******** CHIAMO PGM DI VISUALIZZAZIONE GRIGLIA E AMAGRAFICA *****
103200951215     C     CALPG4        BEGSR
103300951215     C                   MOVEL     'S'           WPOSIZ            1
103400951215     C*
103500951215    1C     V4CPOS        IFGT      0
103600010824     C     V4CPOS        CHAIN     TL44S04                            31
103700951215     C                   Z-ADD     V4CTRN        KVTRN
103800951215     C                   Z-ADD     V4HNFP        KVNFP
103900960322     C                   Z-ADD     V4CFFP        KVFFP
104000951215     C                   Z-ADD     V4HDTN        KVDTN
104100951215    1C                   ENDIF
104200951215     C*
104300951215     C* ERRORE --> CURSORE NON POSIZIONATO SU RIGA DI TRAINO
104400951215    1C     V4CPOS        IFEQ      0
104500951215     C     *IN31         OREQ      *ON
104600951215     C                   MOVEL     MSG(33)       VSCMSG
104700951215     C                   SETON                                        2890
104800960322    1C                   ENDIF
104900960322     C* SE NON C'E' IL NUMERO DI FV NON POSSO INTERROGARE
105000960322     C     *INKJ         IFEQ      *ON
105100960322     C     V4CNFV        ANDLE     *ZEROS
105200960322     C                   MOVEL     MSG(38)       VSCMSG
105300960322     C                   SETON                                        289042
105400960322     C                   SETON                                        22
105500960322     C                   EXSR      CTRIND
105600010824     C                   UPDATE    TL44S04
105700960322     C                   SETOFF                                       22
105800960322     C                   ENDIF
105900960322     C* NON TROVATI ERRORI
106000960322    1C     *IN90         IFEQ      *OFF
106100960322     C*
106200960322    2C     *INKJ         IFEQ      *ON                                          F10-F.VIAGG
106300960322     C                   CLEAR                   DSTL37                         VISUAL FV
106400960322     C                   MOVEL     'R'           D37FLG
106500960322     C                   MOVEL     V4CFFP        D37LNP
106600960322     C                   MOVEL     V4CNFV        D37NFV
106700960322     C                   MOVEL     DSTL37        KPJBU
106800960322     C                   CALL      'TNTL37R'
106900960322     C                   PARM                    KPJBA
107000960322    2C                   ENDIF
107100960322     C*
107200951215     C*
107300951215    2C     *INKH         IFEQ      *ON                                          F8-GRIGIA
107400951215     C                   EXSR      VISGRL                                       VISUAL GRIGL
107500951215    2C                   ENDIF
107600951215     C**
107700951215    2C     *INKN         IFEQ      *ON                                          F14-INT ANAG
107800951215     C                   CLEAR                   DSTL00
107900951215     C                   MOVEL     'T05'         D00OP0
108000951215     C                   MOVEL     'O05'         D00OP1
108100951215     C                   Z-ADD     KVTRN         D00TRN
108200951215     C* CERCO LA DECORRENZA
108300951215     C                   EXSR      RICDEC
108400951215    3C     KDDE          IFEQ      0
108500951215     C                   Z-ADD     KDDE          D00DDE
108600951215    3C                   ENDIF
108700951215     C* VISULIZZO L'UNLTIMO SCADUTO
108800951215    3C     TLTDDE        IFGT      0
108900951215     C                   Z-ADD     TLTDDE        D00DDE
109000951215    3C                   ENDIF
109100951215     c*
109200951215    3C     D00DDE        IFGT      0
109300951215     C                   MOVEL     DSTL00        KPJBU
109400951215     C                   CALL      'TNTL01C'                                    VISUAL GRIGL
109500951215     C                   PARM                    KPJBA
109600951215   x3C                   ELSE
109700951215     C                   MOVEL     'E'           D00ERR
109800951215     C                   MOVEL     MSG(34)       D00MSG
109900951215    3C                   ENDIF
110000951215     C*
110100951215    3C     D00MSG        IFNE      *BLANKS
110200951215     C                   MOVEL     D00MSG        VSCMSG
110300951215     C                   SETON                                        2890
110400951215    3C                   ENDIF
110500951215    2C                   ENDIF
110600951215     C**
110700951215    2C     *INKR         IFEQ      *ON                                          F17-MAN TRAI
110800951215     C                   Z-ADD     KVTRN         WTRN
110900951215     C                   Z-ADD     KVDTN         AMGTRD
111000951215     C                   Z-ADD     KVDTN         AMGTRA
111100951215     C* GIRO LA DATA TRAINO
111200951215     C                   CLEAR                   WLBDAT
111300951215     C                   MOVEL     '3'           G02ERR
111400951215     C                   MOVE      KVDTN         G02INV
111500951215     C                   CALL      'XSRDA8'
111600951215     C                   PARM                    WLBDAT
111700951215     C                   Z-ADD     G02DAT        V1CTRD
111800951215     C                   SETON                                        0301
111900951215     C**
112000951215     C                   EXSR      GESTRN
112100951215     C**
112200951215     C                   CLEAR                   WTRN
112300951215     C                   CLEAR                   AMGTRD
112400951215     C                   CLEAR                   V1CTRD
112500951215     C                   CLEAR                   AMGTRA
112600951215     C                   SETOFF                                       0301
112700951215     C                   Z-ADD     4             KSFL
112800951215    2C                   ENDIF
112900951215     c**
113000951215     C                   Z-ADD     V4CPOS        V4CREC
113100960322     C*
113200951215    1C                   ENDIF
113300951215     C                   ENDSR
113400951103     C**
113500951108     C********* PREMUTO F19 SINISTRA O F20 DESTRA *********************
113600951103     C     F19F20        BEGSR
113700951113     C                   MOVEL     'S'           WPOSIZ            1
113800951116     C                   SETOFF                                       99
113900951116     C* VISULIZZARO LA PAGINA CHE CONTIENE IL NUMERO DI TRAINO
114000951116     C*   SU CUI E' POSIZIONANTO IL CURSORE, SE C'E' ALTRIMENTI
114100951116     C*   IL TERMINAL DI ARRIVO CHE SI STA VISULIZZANDO
114200951116    1C     V2CPOS        IFEQ      0
114300951116     C                   Z-ADD     V2CREC        V2CPOS
114400951116     C                   ENDIF
114500951116     C*
114600010824     C     V2CPOS        CHAIN     TL44S02                            31
114700010824     C  N31              Z-ADD     V2hTFP        WCTFP
114800951116     C  N31              Z-ADD     V2HTRN        WCTRN
114900951106     C   31              Z-ADD     KTFP          WCTFP
115000951116     C   31              CLEAR                   WCTRN
115100951106     C*
115200951106     C* VERIFICO SE LA VIDEATA RICHIESTA E' GIA' STATA VISUALIZZATA
115300951106     C* SE SI RICARICO DA FILE DI WORK
115400951106     C                   ADD       WTOADD        WF20              2 0
115500951117     C     KTSW1         CHAIN(N)  TNTSW01L                           31
115600951106     C*
115700951106     C* TROVATO --> PAGINA GIA' CARICATA
115800951106    1C     *IN31         IFEQ      *OFF
115900951108     C* PULIZIA SFL
116000951214     C                   EXSR      PULSFL
116100951106     C* CARICO SFL DA LETTURA FILE TNTSW00F
116200951106     C                   EXSR      CARTSW
116300951116     C* SE NON HO CARICATO UNA PAGINA INTERA --> FINISCO DI CARICARE
116400951214     C     NRR2          DIV       WRIPAG        W0050
116500951116     C                   MVR                     L
116600951116     C* SE RESTO = 0 ALLORA HO CARICATO UNA PAGINA INTERA
116700951116     C     L             IFGT      0
116800951116     C                   EXSR      LEGS02
116900951116     C                   ENDIF
117000951113     C*
117100951106    2C     KTFP          IFLT      WCTFP
117200951106    3C     KTFP          DOWLT     WCTFP
117300951106     C     *IN99         ANDEQ     *OFF
117400951116     C                   Z-ADD     0             L
117500951106     C                   EXSR      LEGS02
117600951106    3C                   ENDDO
117700951106    2C                   ENDIF
117800951115     C*
117900951214     C                   EXSR      POSSFL                                        POSIZ CURSO
118000951106     C*
118100951106   X1C                   ELSE
118200951108     C*
118300951108     C* VERIFICO SE HO GIA' CARICATO GLI ULTIMI DATI
118400951108     C* PER F20
118500951108   1AC   KUW0080         IFGE      AMGTRA
118600951214     C                   MOVEL     MSG(18)       VSCMSG
118700951116     C                   SUB       WTOADD        WF20              2 0
118800951108     C                   SETON                                        2890
118900951108  X1AC                   ELSE
119000951108     C*
119100951103     C* AGGIUNGO(F20) O TOLGO(F19) ALL'ULTIMA DATA ELABORATA 1
119200951103     C* PER CERCARE UNA NUOVA DATA DA ELABORARE
119300951103     C                   ADD       WTOADD        WTOTGG
119400951103     C                   Z-ADD     WTOTGG        GIOTGI
119500951103     C                   CALL      'XSRGI8'
119600951103     C                   PARM                    WGIDAT
119700951103     C*
119800951103     C                   Z-ADD     GIOINV        W0080
119900951103     C* MEMORIZZO IL TERMINAL FINO AL QUALE DEVO CARICARE
120000951103     C                   Z-ADD     KTFP          WSTFP
120100951103     C                   Z-ADD     KDTN          WSDTN
120200951103     C*
120300951103     C                   CLEAR                   KTFP
120400951103     C                   CLEAR                   KDTN
120500951103     C*
120600951103     C* CERCO NUOVA DATA DA ELABORARE
120700951212     C                   EXSR      RICDTN
120800951103     C* NON TROVATO PIU' NULLA
120900951106    2C     W0080         IFGT      AMGTRA
121000951103     C     W0080         ORLT      AMGTRD
121100951103     C                   Z-ADD     WSDTN         KDTN
121200951103     C                   Z-ADD     WSTFP         KTFP
121300951106     C                   SUB       WTOADD        WF20              2 0
121400951106    3C     W0080         IFGT      AMGTRA
121500951103     C                   Z-ADD     AMGTRA        W0080
121600951214     C                   MOVEL     MSG(18)       VSCMSG
121700951106   X3C                   ELSE
121800951103     C                   Z-ADD     AMGTRD        W0080
121900951214     C                   MOVEL     MSG(17)       VSCMSG
122000951106    3C                   ENDIF
122100951103     C                   SETON                                        2890
122200951103     C*
122300951106   X2C                   ELSE
122400951103     C* PULIZIA SFL
122500951113     C                   CLEAR                   WPOSIZ
122600951214     C                   EXSR      PULSFL
122700951103     C*
122800951116     C* LETTURA FINO A QUANDO NON CARICO TUTTO IL TRAINO DEL
122900951103     C*  TERMINAL CHE STAVO LEGGENDO
123000951116     C                   CLEAR                   WNRRA
123100951116     C                   CLEAR                   WNRRB
123200951116     C                   CLEAR                   WNRRC
123300951116     C                   CLEAR                   WSTFP
123400951116     C*
123500951116    3C     KTFP          DOUGT     WCTFP
123600951108     C     *IN99         OREQ      *ON
123700951116     C                   Z-ADD     0             L
123800951103     C                   EXSR      LEGS02
123900951106    3C                   ENDDO
124000951108     C*
124100951214     C                   EXSR      POSSFL
124200951106    2C                   ENDIF
124300951108   1AC                   ENDIF
124400951108    1C                   ENDIF
124500951103     C*
124600951116     C                   CLEAR                   WCTFP
124700951116     C                   CLEAR                   WCTRN
124800951103     C                   ENDSR
124900951115     C**
125000951115     C******************** POSIZIONAMENTO CURSORE ********************
125100951214     C     POSSFL        BEGSR
125200951214     C     KSFL          IFEQ      2
125300951214     C                   Z-ADD     NRR2          WNRR
125400951214     C                   ELSE
125500951214     C                   Z-ADD     NRR4          WNRR
125600951214     C                   ENDIF
125700951116     C**
125800951214    1C     WNRR          IFLE      WRIPAG
125900951214     C                   Z-ADD     1             WNREC
126000951115   X1C                   ELSE
126100951214     C     WNRR          DIV       WRIPAG        W0020
126200951115     C                   MVR                     W0050
126300960212     C                   SELECT
126400951115     C* RESTO  = 1 --> E' IL 1 RECORD DELLA PAGINA
126500960212    2C     W0050         WHENEQ    1
126600951214     C                   Z-ADD     WNRR          WNREC
126700951115     C*
126800960212    2C     W0050         WHENGT    1
126900951214     C     WNRR          SUB       W0050         WNREC
127000951214     C                   ADD       1             WNREC
127100960212     C*
127200960212    2C     W0050         WHENEQ    0
127300960212     C     WNRR          SUB       WRIPAG        WNREC
127400960212     C                   ADD       1             WNREC
127500960212    2C                   ENDSL
127600951115    1C                   ENDIF
127700951116     C*
127800951116     C                   SELECT
127900951116     C     WNRRA         WHENGT    0
128000951214     C                   Z-ADD     WNRRA         WNREC
128100951116     C     WNRRB         WHENGT    0
128200951214     C                   Z-ADD     WNRRB         WNREC
128300951116     C     WNRRC         WHENGT    0
128400951214     C                   Z-ADD     WNRRC         WNREC
128500951116     C                   ENDSL
128600951116     C                   CLEAR                   WNRRA
128700951116     C                   CLEAR                   WNRRB
128800951116     C                   CLEAR                   WNRRC
128900951214     C**
129000951214     C     KSFL          IFEQ      2
129100951214     C                   Z-ADD     WNREC         V2CREC
129200951214     C                   ELSE
129300951214     C                   Z-ADD     WNREC         V4CREC
129400951214     C                   ENDIF
129500951115     C                   ENDSR
129600951107     C**
129700951107     C********** SISTEMO GLI INDICATORI NEL SFL ***********************
129800951107     C     CTRIND        BEGSR
129900951213     C* SFL 2
130000951213    0C     KSFL          IFEQ      2
130100951213     C*
130200951213    1C     V2HTRC        IFEQ      'T'                                          TRAINO
130300960213     C                   SETOFF                                       119897
130400960213     C                   SETOFF                                       95
130500960213     C* SE ANNULLATO
130600960213     C     V2CSCE        IFEQ      'A'
130700960213     C                   SETON                                        9897
130800960222     C                   SETOFF                                       19
130900960213     C                   ENDIF
131000960213     C* PROTEGGO CAMPO IMPORTO CONFERMATO
131100960213     C     V2HPRI        IFEQ      'S'
131200960213     C                   SETON                                        98
131300960213     C                   ENDIF
131400960213     C* PROTEGGO CAMPO PADRONCINO E CAUSALE
131500960213     C     V2HPRP        IFEQ      'S'
131600960213     C                   SETON                                        97
131700960213     C                   ENDIF
131800960213     C*
131900951215   X1C                   ELSE                                                   FVT
132000951107     C                   SETON                                        11
132100960213     C                   SETOFF                                       139495
132200951107     C*
132300960213     C* TRATTA ANNULLATA : PROTEGGO CAMPO DEL FV
132400960213    2C     V2CSCE        IFEQ      'A'
132500960213     C                   SETON                                        94
132600960222   X2C                   ELSE
132700960222    3C     V2HRFV        IFEQ      'S'                                          RI IL NFV
132800960222     C                   SETON                                        13
132900960222    3C                   ENDIF
133000960213    2C                   ENDIF
133100960213     C* SE LA TESTATA E' ANNULLATA PROTEGGO FV E FLAG ANNULLAMENTO
133200960213     C*  ANCHE SE IL FV NON E' STATO ANCORA CONTROLLATO
133300960213    2C     V2HATT        IFEQ      'A'
133400960213    2C     V2HPRF        OREQ      'S'
133500960213     C                   SETON                                        9495
133600960222     C                   SETOFF                                       13
133700960213    2C                   ENDIF
133800960213     C*
133900951213    1C                   ENDIF
134000951107     C*
134100951213    1C     V2HFAT        IFEQ      'S'                                          FATTURATO
134200951107     C                   SETON                                        12
134300951107     C                   ELSE
134400951107     C                   SETOFF                                       12
134500951108    1C                   ENDIF
134600951108     C*
134700951108    1C     V2HNRC        IFLE      2
134800951108     C                   SETOFF                                       18
134900951108     C                   ELSE
135000951108     C                   SETON                                        18
135100951108    1C                   ENDIF
135200960213     C*
135300951213     C*
135400951213   X0C                   ELSE
135500951213     C* S F L  4
135600960322     C                   SETOFF                                       139495
135700960322     C                   SETOFF                                       1296
135800960322     C* TRATTA ANNULLATA
135900960322     C     V4HATB        IFEQ      'A'
136000960322     C                   SETON                                        94
136100960322     C                   ELSE
136200951213    1C     V4HRFV        IFEQ      'S'                                          RI IL NFV
136300951213     C                   SETON                                        13
136400951213    1C                   ENDIF
136500960322     C                   ENDIF
136600960322     C* SE LA TESTATA E' ANNULLATA PROTEGGO ANCHE IL FLAG DI
136700960322     C*  ANNULLAMENTO MA LO VISUALIZZO
136800960322     C     V4HATT        IFEQ      'A'
136900960322     C                   SETON                                        9495
137000960322     C                   SETOFF                                       13
137100960322    2C                   ENDIF
137200960322     C*
137300960322     C* SE NON ANCORA CONTROLLATO LE EMETTO IN BASSA INTENSITA'
137400960325     C****       V4HPRF    IFEQ 'S'
137500960325     C****                 SETOF                     13
137600960325     C****                 SETON                     96
137700960325     C****                 ENDIF
137800951213     C*
137900951213    1C     V4HFAT        IFEQ      'S'                                          FATTURATO
138000951213     C                   SETON                                        12
138100951213    1C                   ENDIF
138200951213    0C                   ENDIF
138300951107     C*
138400951107     C                   ENDSR
138500951103     C**
138600951103     C********** RICERCO DATA IN CUI CI SONO TRAINI VALIDI ************
138700951212     C     RICDTN        BEGSR
138800951103     C*
138900951103     C                   SETOFF                                       3132
139000951103     C* CICLO PER CAMBIO DATA
139100951103    1C     W0080         DOWLE     WDTAL
139200951103    1C     W0080         ANDGE     WDTDAL
139300951103     C     *IN31         ANDEQ     *OFF
139400951103     C                   Z-ADD     W0080         KDTN
139500951103     C* CICLO PER CAMBIO FILIALE
139600951103    2C     *IN31         DOWEQ     *OFF
139700951103     C     KTFP          SETGT     TNTST04L
139800951103     C                   READ      TNTST04L                               32
139900951103    3C     *IN32         IFEQ      *OFF
140000951103     C                   Z-ADD     TSTTFP        KTFP
140100951103     C                   ELSE
140200951103     C                   SETON                                        31
140300951103    3C                   ENDIF
140400951103     C*
140500951103     C  N32KTST4         SETLL     TNTST04L                               31
140600951103    2C                   ENDDO
140700951103     C* SE NON TROVATA PER QUESTA DATA FACCIO ADD 1
140800951103    2C     *IN32         IFEQ      *ON
140900951103     C                   ADD       WTOADD        WTOTGG
141000951103     C                   Z-ADD     WTOTGG        GIOTGI
141100951103     C                   CALL      'XSRGI8'
141200951103     C                   PARM                    WGIDAT
141300951103     C*
141400951103     C                   Z-ADD     GIOINV        W0080
141500951103     C*
141600951103     C                   CLEAR                   KTFP
141700951103     C                   SETOFF                                       31
141800951103    2C                   ENDIF
141900951103    1C                   ENDDO
142000951103     C                   ENDSR
142100951212     C**
142200951106     C********** CARICO SFL02 DA LETTURA TNTSW00F *********************
142300951106     C     CARTSW        BEGSR
142400951107     C                   Z-ADD     1             KNRR
142500951116     C                   CLEAR                   WNRRA
142600951116     C                   CLEAR                   WNRRB
142700951116     C                   CLEAR                   WNRRC
142800951116     C                   CLEAR                   WSTFP
142900951107     C     KTSW11        SETLL     TNTSW01L
143000951117     C     KTSW1         READE(N)  TNTSW01L                               31
143100951106     C*
143200951106    1C     *IN31         DOWEQ     *OFF
143300951107     C                   CLEAR                   V2HNFV
143400951107     C                   CLEAR                   V2CNFV
143500951113     C                   CLEAR                   V2CDTT
143600951113     C                   CLEAR                   V2CBIS
143700010824     C                   CLEAR                   V2Cdiv
143800951107     C                   CLEAR                   V2HRFV
143900951107     C                   CLEAR                   V2HNFP
144000951117     C                   CLEAR                   V2CFFP
144100951106     C                   MOVEL     TSWTRC        V2HTRC
144200951106     C                   MOVEL     TSWFAT        V2HFAT
144300951214     C                   MOVEL     TSWDTN        V2HDTN
144400010824     C                   MOVEL     TSWTFP        V2hTFP
144500010824     C                   MOVEL     TSWTFP        V2CTer
144600010824     C**!!!              MOVEL     TSWTFA        V2CTFA
144700951113     C                   MOVEL     TSWTRN        V2HTRN
144800960213     C                   MOVEL     TSWATV        V2CSCE
144900960213     C                   MOVEL     TSWATT        V2HATT
145000951106    2C     TSWTRC        IFEQ      'T'                                          TRAINO
145100951113     C                   Z-ADD     1             V2HNRC
145200951116     C* CERCO NUM RELATIVO SFL DA VISULIZZARE
145300951116     C                   EXSR      RICNRR
145400951113     C* DATA TRAINO
145500951113     C                   MOVEL     TSWDTN        WDATA
145600951113     C                   MOVEA     WMM           K7(6)
145700951113     C                   MOVEA     WGG           K7(3)
145800951113     C                   MOVEL     '/'           K7(5)
145900951113     C                   CLEAR                   V2CDES
146000951113     C                   MOVEA     K7            V2CDES
146100951113     C*
146200951113     C                   MOVEL     TSWBIS        V2CBIS
146300010824     C                   MOVEL     TSWTFA        V2CTer
146400010910     C**!!!              MOVEL     TSWIPT        V2HIPT
146500010910     C                   z-add     TSWIPT        V2HIPT
146600010910     C**!!!              MOVEL     TSWILI        V2CIMP
146700010910     C                   z-add     TSWILI        V2CIMP
146800960221     C                   MOVEL     TSWCNF        V2HCNF
146900010910     C**!!!              MOVEL     TSWVIC        V2CICO
147000010910     C**!!!              MOVEL     TSWVIC        V2HICO
147100010910     C                   z-add     TSWVIC        V2CICO
147200010910     C                   z-add     TSWVIC        V2HICO
147300010824     C                   MOVEL     TSWdiv        V2cdiv
147400951113     C                   MOVEL     TSWVCU        V2CCAU
147500951108     C     TSWVPD        IFGT      0
147600951108     C                   MOVEL     TSWVPD        V2CPDR
147700951108     C                   MOVEL     TSWVPD        V2HPDR
147800951108     C                   ELSE
147900951108     C                   CLEAR                   V2CPDR
148000951108     C                   CLEAR                   V2HPDR
148100951108     C                   ENDIF
148200951107     C                   MOVEL     TSWDPD        V2DPDR
148300951108     C                   MOVEL     TSWTMZ        V2HTMZ
148400960213     C                   MOVEL     TSWPRP        V2HPRP
148500960213     C                   MOVEL     TSWPRI        V2HPRI
148600951106     C*
148700960229   X2C                   ELSE                                                   FV TEORICO
148800951113     C                   ADD       1             V2HNRC
148900951113     C* NUMERO TRAINO
149000951113     C                   MOVEL     TSWTRN        W007A             7
149100951113     C                   MOVEA     W007A         SKI(9)
149200951113     C                   Z-ADD     9             I                 3 0
149300951113     C                   EXSR      ABBL
149400951113     C                   MOVEA     SKI(9)        V2CDES
149500951106     C                   MOVEL     TSWRFV        V2HRFV
149600951108     C     TSWVNF        IFGT      0
149700951108     C                   MOVEL     TSWVNF        V2CNFV
149800951113     C                   MOVEL     TSWVNF        V2HNFV
149900951108     C                   ENDIF
150000960213     C                   MOVEL     TSWPRF        V2HPRF
150100960213     C                   MOVEL     TSWFFP        V2CFFP
150200951107     C                   MOVEL     TSWNFP        V2HNFP
150300951107     C                   MOVEL     TSWDTT        WDATA
150400951107     C                   MOVE      WMM           V2CDTT
150500951107     C                   MOVEL     WGG           V2CDTT
150600951113     C     V2HNRC        IFEQ      2
150700951113     C                   Z-ADD     TSWIPT        V2CIMP
150800010824     C                   MOVEL     TSWTFA        V2CTer
150900951113     C                   CLEAR                   V2HIPT
151000010824     C                   CLEAR                   V2Htfp
151100951113     C                   ELSE
151200951113     C                   CLEAR                   V2CIMP
151300010824     C                   CLEAR                   V2Cter
151400951113     C                   CLEAR                   V2CDES
151500951113     C                   ENDIF
151600960213     C*
151700960213     C*
151800951106    2C                   ENDIF
151900951107     C* SISTEMO GLI INDICATORI
152000951107     C                   EXSR      CTRIND
152100951106     C*
152200951106     C                   Z-ADD     TSWNRR        NRR2              5 0
152300010824     C                   WRITE     TL44S02
152400951106     C*
152500951117     C     KTSW1         READE(N)  TNTSW01L                               31
152600951106    1C                   ENDDO
152700951115     C                   Z-ADD     NRR2          WNRR2
152800010824     C                   Z-ADD     V2hTFP        WCTFP
152900951106     C                   ENDSR
153000951214     C*
153100951214     C* ***************   CONTROLLO I CAMPI DELLA RICERCA ************
153200971204     C***        CTRRIC    BEGSR
153300951214     C* TERM PARTENZA TRAINO
153400971204    1C***        WRTP      IFGT 0
153500971204     C***        WRTP      CHAINAZORG01L             30
153600971204    2C***        *IN30     IFEQ *ON
153700971204     C***        ORGFVA    ORNE *BLANKS
153800971204     C***                  MOVELMSG,9     VSCMSG
153900971204     C***                  SETON                     502890
154000971204    2C***                  ENDIF
154100951214     C* Non è terminal
154200971204    2C***        ORGFEL    IFNE ORGFIL
154300971204     C***                  SETON                     509028
154400971204     C***                  MOVELMSG,10    VSCMSG
154500971204    2C***                  ENDIF
154600971204    1C***                  ENDIF
154700951214     C*
154800971204     C**N90      WRDT      IFGT 0
154900971204     C****                 CLEARWLBDAT
155000971204     C****                 MOVE WRDT      G02DAT
155100971204     C****                 CALL 'XSRDA8'
155200971204     C***                  PARM           WLBDAT
155300971204    2C****       G02ERR    IFEQ '1'
155400971204     C***                  MOVELMSG,31    VSCMSG
155500971204     C***                  SETON                     519028
155600971204    2C****                 END
155700971204     C***                  MOVE G02INV    AMGRDT
155800971204     C****                 ENDIF
155900971204     C****                 ENDSR
156000951103     C**
156100951103     C********** CONTROLLO DATI SFL02 *********************************
156200951103     C     CTRS02        BEGSR
156300951117     C                   CLEAR                   W0040
156400960213     C**
156500951214     C* CONTROLLO I CAMPI DELLA RICERCA NEL SFLCTL
156600971204     C* LA RICERCA NON E' ATTIVATA !!!!1
156700971204     C*****                Z-ADDV2CRTP    WRTP
156800971204     C****                 Z-ADDV2CRDT    WRDT
156900971204     C****                 CLEARAMGRDT
157000971204     C****                 EXSR CTRRIC
157100971204     C***90                GOTO ENDCS2
157200960213     C**
157300960213     C* NEI CONTROLLI FACCIO 2 GIRI: PRIMA CONTROLLO I DATI FV
157400960213     C* POI CONTROLLERO' I DATI TESTATA TRAINI
157500951115     C*
157600010824     C                   READC     TL44S02                                32
157700951107     C*
157800951113     C* SE POI ROLLERO' MI DOVRO' RIPOSIZIONARE
157900951115     C  N32              MOVEL     'S'           WPOSIZ
158000951103     C*
158100951103    1C     *IN32         DOWEQ     *OFF
158200951103    2C     V2HTRC        IFEQ      'T'
158300020315
158400020315      * controllo se posso annullare il traino, non posso nel caso di
158500020315      * traino caricato a mezzo.
158600020315     c                   if        v2csce = 'A' and v2htrn = w_tsrtrn
158700041001     c                             and v2hdtn = W_tsrdtn
158800020315     c                   eval      *in90 = *on
158900020315     c                   eval      *in28 = *on
159000020315     c                   eval      *in43 = *on
159100020315     c                   eval      vscmsg = msg(41)
159200020315     c**!!!              goto      endcs2
159300020315     c                   endif
159400951103     C*
159500951214    3C     V2HFAT        IFNE      'S'
159600960213     C* ACCENDO IL SFLNEXCHG
159700960213     C                   SETON                                        22
159800960213     C                   EXSR      CTRIND
159900960213     C*
160000010824     C                   UPDATE    TL44S02
160100960213     C                   SETOFF                                       22
160200960213    3C                   ENDIF
160300960213     C*
160400960213   X2C                   ELSE
160500960213     C* C O N T R O L L O    N U M E R O    F V
160600960213     C                   MOVEL     V2HNFV        HNFV
160700960213     C                   MOVEL     V2CNFV        CNFV
160800960213     C                   Z-ADD     V2HTRN        CTRN
160900960213     C                   Z-ADD     V2HDTN        CDTN
161000960213     C                   Z-ADD     V2CFFP        CFFP
161100960213     C                   Z-ADD     V2HNFP        HNFP
161200960213     C                   Z-ADD     V2CDTT        CDTT
161300960213     C                   MOVEL     V2HRFV        HRFV
161400960213     C                   MOVEL     V2CSCE        CSCE
161500960213     C                   EXSR      CTRNFV
161600960213     C                   MOVEL     CNFV          V2CNFV
161700960213     C                   MOVEL     HRFV          V2HRFV
161800020315
161900020315      * controllo se posso annullare la tratta, non posso nel caso di
162000020315      * traino caricato a mezzo.
162100020315     c                   if        v2csce = 'A' and v2htrn = w_tsrtrn
162200041001     c                             and v2hdtn = W_tsrdtn
162300020315     c                   eval      *in90 = *on
162400020315     c                   eval      *in28 = *on
162500020315     c                   eval      *in43 = *on
162600020315     c                   eval      vscmsg = msg(41)
162700020315     c**!!!              goto      endcs2
162800020315     c                   endif
162900960213     C*
163000960213     C**
163100960213     C* VEDO SE C'E' ERRORE E AGGIORNO TNTSW
163200960213     C                   EXSR      ERRORE
163300960213    2C                   ENDIF
163400960213     C*
163500010824     C  N32              READC     TL44S02                                32
163600960213    1C                   ENDDO
163700960213     C*
163800960213     C* ERRORE  --> ESCO
163900960213     C   90              GOTO      ENDCS2
164000960213     C**
164100960213     C* C O N T R O L L O    T E S T A T A    T R A I N I
164200010824     C                   READC     TL44S02                                32
164300960213     C*
164400960213    1C     *IN32         DOWEQ     *OFF
164500960213     C*
164600960213    2C     V2HTRC        IFEQ      'T'
164700960213     C     V2HFAT        ANDNE     'S'
164800960223     C** RIPRISTINO TRAINO :
164900960223     C* NON POSSO RIPRISTINARE SE NON ESISTE PIU' L'ANAGRAFICA TRAINO
165000960223    3C     V2CSCE        IFEQ      ' '
165100960223     C     V2HATT        ANDEQ     'A'
165200960223     C* CERCO CON TRAINO E DATA TRAINO LA DECORRENZA
165300960223     C                   Z-ADD     V2HTRN        KVTRN
165400960223     C                   Z-ADD     V2HDTN        KVDTN
165500960223     C                   EXSR      RICDEC
165600960223    4C     KDDE          IFEQ      0
165700960223     C                   MOVEL     MSG(24)       VSCMSG
165800960223     C                   SETON                                        289043
165900960223    4C                   ENDIF
166000960223    3C                   ENDIF
166100960213     C**
166200951103     C* RICERCA SUL CODICE TRAZIONISTA
166300951103     C     '?'           SCAN      V2CPDR                                 90
166400960213    3C     *IN90         IFEQ      *ON
166500000807     C                   EVAL      PA3TIP = 'T '                                PAR-1
166600000807     C                   MOVEL(P)  V2DPDR        PA3RSC                         PAR-2
166700000807     C                   CLEAR                   PA3CSF                         PAR-3
166800000807     C                   CLEAR                   PA3PDR                         PAR-4
166900000807     C                   CLEAR                   PA3FLG                         PAR-5
167000951103     C                   MOVEL     PARAM3        KPJBU
167100951103     C                   CALL      'TRUL16R'
167200951103     C                   PARM                    KPJBA
167300951103     C                   MOVEL     KPJBU         PARAM3
167400951103     C* SE PA3FLG = "3" --> NON ESISTONO VISITE PER CHIAVE ALFABETICA
167500951103     C*                     RICHIESTA
167600960213    4C     PA3FLG        IFEQ      '3'
167700951106     C                   CLEAR                   V2CPDR
167800951214     C                   MOVEL     MSG(20)       VSCMSG
167900951103     C                   SETON                                        28
168000960213   X4C                   ELSE
168100000807     C                   MOVEL     PA3PDR        V2CPDR                         CODICE
168200000807     C                   MOVEL     PA3RSC        V2DPDR                         RAG.SOCIALE
168300960213    4C                   ENDIF
168400951103     C                   SETON                                        41
168500951103     C*
168600960213    3C                   ENDIF
168700951124     C* OBBLIGATORIO SOLO SE C'E' IMPORTO DA CONFERMARE
168800180103    3C     V2CICO        IFne      0
168900951124     C     V2CPDR        ANDLE     *ZEROS
169000951214     C                   MOVEL     MSG(21)       VSCMSG
169100951124     C                   SETON                                        289041
169200960213    3C                   ENDIF
169300180103     c* se V2CICO <0 --> errore!!!
169400180103     c                   if        v2cico<0
169500180103     C                   MOVEL     MSG(42)       VSCMSG
169600180103     C                   SETON                                        289040
169700180103    3C                   ENDIF
169800951103     C*
169900960213    3C     V2CPDR        IFNE      *BLANKS
170000951124     C     V2CPDR        ANDNE     *ZEROS
170100951103     C                   TESTN                   V2CPDR               31
170200951103     C                   MOVE      V2CPDR        W001A             1
170300960213    4C     *IN31         IFEQ      *OFF
170400951103     C     W001A         ORLT      '0'
170500951214     C                   MOVEL     MSG(21)       VSCMSG
170600951103     C                   SETON                                        289041
170700960213   X4C                   ELSE
170800951107     C* CONTROLLO SULL'ANAGRAFICA
170900951107     C                   MOVEL     V2CPDR        W0070             7 0
171000000807     C                   Z-ADD     W0070         KTLPDR
171100000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
171200960213    5C     *IN31         IFEQ      *ON
171300951107     C     TLZATB        ORNE      ' '
171400951214     C                   MOVEL     MSG(21)       VSCMSG
171500951107     C                   SETON                                        289041
171600960213   X5C                   ELSE
171700951107     C                   MOVEL     TLZRSC        V2DPDR
171800960213    5C                   ENDIF
171900960213    4C                   ENDIF
172000960221     C*
172100960221     C* SE MODIFICATO ICO --> DIVENTA CONFERMA MANUALE
172200180103    4C     V2CICO        IFne      0
172300960221     C     V2CICO        ANDNE     V2HICO
172400960221     C                   MOVEL     '2'           V2HCNF
172500960221    4C                   ENDIF
172600960221     C**
172700951108     C* SE MODIFICATO VEDO SU ANAGRAFICA A RIPRENDERE L'IMPORTO LISTIN
172800960213    4C     V2HPDR        IFNE      V2CPDR
172900951108     C     V2CPDR        ANDGT     *ZEROS
173000951108     C                   EXSR      RICLIS
173100951115     C                   CLEAR                   WFORZ
173200951108     C                   MOVEL     V2CPDR        V2HPDR
173300011012      * devo calcolare la tolleranza
173400011012     c                   eval      sav_wili = wili
173500011012      * calcolo il delta e controllo la tolleranza
173600011012     c                   if        sav_wili <> v2cimp
173700011012     c                   eval      w_delta = (sav_wili - v2cimp)
173800011012      * il delta deve essere come valore assoluto
173900011012     c                   mllzo     1             w_delta
174000011012     c                   if        w_delta <= §gettr
174100011012     c                   eval      sav_wili = v2cimp
174200011012     c                   endif
174300011012     c                   endif
174400951115     C* SE NON HO MODIFICATO IL LISTINO, NON METTO IN RI
174500011012    5C     V2CIMP        IFEQ      sav_WILI
174600951116     C     V2CCAU        ANDEQ     WCAU
174700951116     C                   SETOFF                                       1790
174800960213    5C                   ENDIF
174900951115     C*
175000951108     C                   Z-ADD     WILI          V2CIMP
175100951115     C                   MOVEL     WCAU          V2CCAU
175200960221    4C                   ENDIF
175300960222    C**
175400960222     C* CONTROLLO SE POSSO CONFERMARE L'IMPORTO
175500960222     C                   EXSR      CONFIM
175600960221     C*
175700960221     C* SE CONFERMA AUTOMATICA --> SCONFERMO
175800960221    4C     WBUCO         IFNE      ' '
175900960221     C     V2HCNF        ANDEQ     '1'
176000960221     C     V2CICO        ANDGT     0
176100960221     C                   CLEAR                   V2CICO
176200960221     C                   SETON                                        19
176300960221    4C                   ENDIF
176400960222    3C                   ENDIF
176500951103     C*
176600951103     C* IMPORTO DA CONFERMARE
176700960222    3C  N90V2CICO        IFNE      V2HICO
176800951103     C                   CLEAR                   WFORZ
176900960221     C                   MOVEL     V2CICO        V2HICO
177000960221     C                   ELSE
177100960221     C                   MOVEL     'S'           WFORZ             1
177200960213    3C                   ENDIF
177300011012      * devo calcolare la tolleranza
177400011012     c                   eval      sav_v2cimp = v2cimp
177500011012     c                   eval      sav_v2hipt = v2hipt
177600011012      * calcolo il delta e controllo la tolleranza
177700011012     c                   if        sav_v2cimp <> v2cico
177800011012     c                   eval      w_delta = (sav_v2cimp - v2cico)
177900011012      * il delta deve essere come valore assoluto
178000011012     c                   mllzo     1             w_delta
178100011012     c                   if        w_delta <= §gettr
178200011012     c                   eval      sav_v2cimp = v2cico
178300011012     c                   endif
178400011012     c                   endif
178500011012      * calcolo il delta e controllo la tolleranza
178600011012     c                   if        sav_v2hipt <> v2cico
178700011012     c                   eval      w_delta = (sav_v2hipt - v2cico)
178800011012      * il delta deve essere come valore assoluto
178900011012     c                   mllzo     1             w_delta
179000011012     c                   if        w_delta <= §gettr
179100011012     c                   eval      sav_v2hipt = v2cico
179200011012     c                   endif
179300011012     c                   endif
179400951103     C*
179500960213    3C  N90WFORZ         IFEQ      ' '
179600180103     C     V2CICO        ANDne     0
179700011012    4C     V2CICO        IFNE      sav_V2CIMP
179800011012     C     V2CICO        ANDNE     sav_V2HIPT
179900960222     C     WBUCO         ORNE      ' '
180000951214     C                   MOVEL     MSG(23)       VSCMSG
180100951103     C                   SETON                                        289040
180200960222    4C                   ENDIF
180300960222    3C                   ENDIF
180400011009      * aggancio la tabella CV
180500011009     c                   clear                   dscv
180600011009     c                   movel     'CV'          cod
180700020103     c                   movel(p)  v2cdiv        key
180800011009     c     ktab          chain     tabel00f
180900011009     c                   if        %found(tabel00f)
181000011009     c                   movel     tbluni        dscv
181100011009     c                   endif
181200011009      * controllo se ok i decimali
181300011009     c                   move      v2cico        w_decimali
181400011009     c                   if        w_decimali > 0  and
181500011009     c                             §cvfdc <> 'S'
181600011009     c                   seton                                        289040
181700011009     c                   movel     msg(40)       vscmsg
181800011009     c                   endif
181900951113     C*
182000951113     C* CONTROLLO CAUSALE CONTABILIZZAZIONE OBBLIGATORIA SE IMPORTO
182100960222    3C  N90V2CICO        IFGT      0
182200951113     C     V2CCAU        ANDEQ     ' '
182300951214     C                   MOVEL     MSG(27)       VSCMSG
182400951113     C                   SETON                                        289044
182500960213    3C                   ENDIF
182600951113     C* RICERCA
182700000218    3C**N90V2CCAU        IFEQ      '?'
182800000218     C**                 RESET                   XTABDS
182900000218     C**                 MOVE      *BLANK        XTAKEY
183000000218     C**                 MOVE      '01'          XTAOPZ
183100000218     C**                 MOVE      xscsoc        XTAAZI
183200000218     C**                 MOVE      XSCLIN        XTALIN
183300000218     C**                 CLEAR                   KPJBU
183400000218     C**                 MOVEL     XTABDS        KPJBU
183500000218     C**                 CALL      'ANGY4ZR'
183600000218     C**                 PARM                    KPJBA
183700000218     C**                 MOVEL     KPJBU         XTABDS
183800000218     C**                 MOVEL     XTAKEY        V2CCAU                         *CODICE
183900000218     C**                 SETON                                        9044
184000000218    3C**                 ENDIF
184100951113     C* CONTROLLO
184200960222    3C  N90V2CCAU        IFNE      ' '
184300000214     C* Reperisco i dati della causale di contabilizzazione
184400000218     C                   MOVE      KNSIF         WCOMO4            4
184500000218     C                   MOVEL     WCOMO4        WSOC              3
184600000218     C                   movel     WSOC          KAtazi
184700000218     C                   MOVEL     'Y4Z'         Katcod
184800000218     C                   MOVEL     *blanks       Katkey
184900000218     C                   MOVEL(P)  V2CCAU        Katkey
185000000218     C     KEATAB        CHAIN     ANATB01L                           30
185100060727     c                   if        not %found(anatb01l)
185200000218     C                   MOVE      '888'         Katazi
185300000218     C     KEATAB        CHAIN     ANATB01L                           30
185400060727     c                   end
185500000218     C                   if        not *in30
185600000218     C                   MOVEL     ATBUNI        ANGY4ZDS
185700000214     C                   end
185800000218     C                   if        *in30 or §4ZFLG<>' '
185900060727     c                             or §4ZTA <> 'T'
186000060727     C*                  MOVEL     '4Z'          COD
186100951214     C                   MOVEL     MSG(28)       VSCMSG
186200951113     C                   SETON                                        289044
186300960213    4C                   ENDIF
186400960213    3C                   ENDIF
186500951103     C*
186600951107     C**
186700960213     C* CONTROLLO SE C'E' ERRORE E AGGIORNO TNTSW
186800960213     C                   EXSR      ERRORE
186900960222    2C                   ENDIF
187000951117     C*
187100010824     C  N32              READC     TL44S02                                32
187200951103    1C                   ENDDO
187300951103     C*
187400951115     C     ENDCS2        ENDSR
187500960222     C**
187600960222     C************ CONTROLLO SE POSSO CONFERMARE L'IMPORTO ************
187700960222     C     CONFIM        BEGSR
187800011012      * devo calcolare la tolleranza
187900011012     c                   eval      sav_v2cimp = v2cimp
188000011012      * calcolo il delta e controllo la tolleranza
188100011012     c                   if        sav_v2cimp <> v2hipt
188200011012     c                   eval      w_delta = (sav_v2cimp - v2hipt)
188300011012      * il delta deve essere come valore assoluto
188400011012     c                   mllzo     1             w_delta
188500011012     c                   if        w_delta <= §gettr
188600011012     c                   eval      sav_v2cimp = v2hipt
188700011012     c                   endif
188800011012     c                   endif
188900960222     C* SE E' POSSIBILE PROPONGO L'IMPORTO CONFERMATO
189000960222    1C     V2CIMP        IFGT      0
189100011012     C     V2HIPT        ANDEQ     sav_V2CIMP
189200960222     C     V2CCAU        ANDNE     ' '
189300960222     C     V2CSCE        ANDEQ     ' '
189400960222     C* VERIFICARE SE CI SONO BUCHI
189500960222     C*  SOLO SE L'IMPORTO CONFERMATO ERA STATO CONTROLLATO
189600960222    2C     V2HPRI        IFEQ      ' '
189700960222     C                   EXSR      BUCHI
189800960222     C                   ELSE
189900960222     C                   MOVEL     '1'           WBUCO
190000960222    2C                   ENDIF
190100960222     C* NON CI SONO BUCHI
190200960222    2C     WBUCO         IFEQ      ' '
190300960222     C* SOLO SE LA PRECEDENTE CONFERMA E' AUTOMATICA
190400960222    3C     V2HCNF        IFEQ      '1'
190500960222     C* ACCENDO IL 19 SOLO SE E' CAMBIATO
190600960222     C     V2CICO        OREQ      0
190700960222     C     V2CICO        IFEQ      0
190800960222     C     V2CICO        ORNE      V2HICO
190900960222     C                   SETON                                        19
191000960222     C                   ENDIF
191100960222     C*
191200960222     C                   MOVEL     '1'           V2HCNF
191300960222     C                   Z-ADD     V2CIMP        V2CICO
191400960222     C                   Z-ADD     V2CIMP        V2HICO
191500960222    3C                   ENDIF
191600011012      * devo calcolare la tolleranza
191700011012     c                   eval      sav_v2cimp = v2cimp
191800011012      * calcolo il delta e controllo la tolleranza
191900011012     c                   if        sav_v2cimp <> v2cico
192000011012     c                   eval      w_delta = (sav_v2cimp - v2cico)
192100011012      * il delta deve essere come valore assoluto
192200011012     c                   mllzo     1             w_delta
192300011012     c                   if        w_delta <= §gettr
192400011012     c                   eval      sav_v2cimp = v2cico
192500011012     c                   endif
192600011012     c                   endif
192700960222     C* SE LA PRECEDENTE E' MANUALE LA TRASFORMO IN AUTOMATICA
192800960222    3C     V2HCNF        IFEQ      '2'
192900011012     C     V2CICO        ANDEQ     sav_V2CIMP
193000960222     C                   MOVEL     '1'           V2HCNF
193100960222    3C                   ENDIF
193200960222     C*
193300960222    2C                   ENDIF
193400960222   X1C                   ELSE
193500960306     C* VERIFICARE SE CI SONO BUCHI
193600960306     C*  SOLO SE L'IMPORTO CONFERMATO ERA STATO CONTROLLATO
193700960306    2C     V2HPRI        IFEQ      ' '
193800960306     C                   EXSR      BUCHI
193900960306     C                   ELSE
194000960306     C                   MOVEL     '1'           WBUCO
194100960306    2C                   ENDIF
194200960222    1C                   ENDIF
194300960222     C*
194400960222     C                   ENDSR
194500951214     C**
194600951214     C************** CONTROLLO NUMERO DI FV *************************
194700951214     C     CTRNFV        BEGSR
194800951214     C**
194900951214     C* RICERCA FOGLIO VIAGGIO DA ABBINARE
195000960213     C* SAE E' ANNULLATO PULISCO IL CAMPO DEL FV E NON FACCIO NULLA
195100960213    0C     CSCE          IFNE      'A'
195200960213     C*
195300951214     C     '?'           SCAN      CNFV                                   31
195400951214    1C     *IN31         IFEQ      *ON
195500010827     C                   CLEAR                   DSTL42
195600010827RM*  C                   Z-ADD     CTRN          D42TRN
195700010827RM*  C                   Z-ADD     CDTN          D42DTN
195800010827RM*  C                   Z-ADD     HNFP          D42NFP
195900010827     C                   MOVEL     'R'           D42MOD
196000010827     C                   MOVEL     DSTL42        KPJBU
196100010827     C                   CALL      'TNTL42R'
196200951214     C                   PARM                    KPJBA
196300010827     C                   MOVEL     KPJBU         DSTL42
196400010827     C                   MOVE      D42NFV        CNFV
196500960221    2C     CNFV          IFEQ      *ZEROS
196600960221     C                   CLEAR                   CNFV
196700960221    2C                   ENDIF
196800960221    1C                   ENDIF
196900960213     C*
197000951214     C* CONTROLLO FV
197100960213    1C     CNFV          IFNE      *ZEROS
197200951214     C     CNFV          ANDNE     *BLANKS
197300951214     C     CNFV          ANDNE     HNFV
197400951214     C                   TESTN                   CNFV                 31
197500951214     C                   MOVE      CNFV          W001A             1
197600951214     C*
197700951214    2C     *IN31         IFEQ      *OFF
197800951214     C     W001A         ORLT      '0'
197900951214     C                   MOVEL     MSG(29)       VSCMSG
198000951214     C                   SETON                                        289042
198100951214   X2C                   ELSE
198200951214     C*
198300951214     C                   MOVEL     CNFV          KNFV
198400951214     C                   Z-ADD     CFFP          KLNP
198500960223     C     KTSR4         CHAIN(N)  TNFVS01L                           30
198600951214    3C     *IN30         IFEQ      *ON
198700960226     C     FVSATB        ORNE      ' '
198800951214     C                   MOVEL     MSG(29)       VSCMSG
198900951214     C                   SETON                                        289042
199000951214   X3C                   ELSE
199100951214     C*
199200951214     C* CONTROLLO PRIMA IN TNTSW00F SE E' ABBINATO O SBINATO
199300960213     C                   MOVEL     'A'           KWTRC
199400951214     C     KTSW2         CHAIN(N)  TNTSW02L                           30
199500951214    4C     *IN30         IFEQ      *OFF
199600951214     C                   MOVEL     TSWTRN        W007A
199700951214     C                   MOVEL     TSWDTN        W008A             8
199800951214     C                   EXSR      PREMSG
199900951214     C                   SETON                                        289042
200000951214   X4C                   ELSE
200100951214     C* SE E' SBINATO --> OK
200200960213     C                   MOVEL     'S'           KWTRC
200300951214     C     KTSW2         SETLL     TNTSW02L                               30
200400951214    5C     *IN30         IFEQ      *OFF
200500951214     C* SE NON L'HO TROVATO IN TNTSW --> VEDO DAL FV
200600951214    6C     FVSTRS        IFNE      0
200700951214     C* CHAIN SU TNTSR PER PRENDERE LA DATA TRAINO
200800960223     C     KTSR4         CHAIN(N)  TNTSR04L                           30
200900951214     C  N30              MOVEL     TSRTRN        W007A
201000951214     C  N30              MOVEL     TSRDTN        W008A
201100951214     C   30              MOVEL     *ALL'9'       W007A
201200951214     C   30              CLEAR                   W008A
201300951214     C                   EXSR      PREMSG
201400951214     C                   SETON                                        289042
201500951214    6C                   ENDIF
201600951214     C*
201700951214    5C                   ENDIF
201800951214     C* SE FV OK VERIFICO SE HA ALMENO UNA LINEA DI SCARICO =FERMATA
201900951215     C*  ARRIVO FVT
202000951214    5C     *IN28         IFEQ      *OFF
202100951214     C                   Z-ADD     CTRN          KTRN
202200951214     C                   Z-ADD     CDTN          KDTN
202300951214     C                   Z-ADD     HNFP          KNFP
202400951214     C                   SETOFF                                       30
202500980121     C***        KTSR11    CHAINTNTSR01L             31
202600980121     C     KTSR11        CHAIN(N)  TNTSR01L                           31
202700951214    6C     *IN31         DOWEQ     *OFF
202800951214     C     *IN30         ANDEQ     *OFF
202900951214     C                   MOVEL     TSRFFA        W003A             3
203000000504     C     Kfws          CHAIN     TNFWS01L                           39
203100000504     c                   if        *in39 or fwsatb<>' '
203200000504     c                   clear                   fwsff3
203300000504     c                   clear                   fwsff4
203400000504     c                   clear                   fwsfl3
203500000504     c                   clear                   fwsfl4
203600010910
203700010910     c                   clear                   fwsipt
203800010910     c                   clear                   fwsies
203900010910     c                   clear                   fwsvip
204000010910     c                   clear                   fwsves
204100011016     c                   endif
204200010907      * se l'importo dell'estensione è = 0 e l'importo sul file
204300010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
204400010907      * la divisa è lire
204500010907     c                   if        fwsipt = *zeros and fvsipt <> 0
204600010907     c                   eval      fwsvip = 'ITL'
204700010911     c                   z-add     fvsipt        fwsipt
204800010907     c                   endif
204900010907     c                   if        fwsies = *zeros and fvsies <> 0
205000010907     c                   eval      fwsves = 'ITL'
205100010911     c                   z-add     fvsies        fwsies
205200010907     c                   endif
205300010907      * se l'importo dell'estensione è = 0 e l'importo sul file
205400010907      * principale è = 0, imposto la divisa a blank per poi prendere quella della
205500010907      * griglia, così non sto a convertire un importo a 0
205600010907     c                   if        fwsipt = *zeros and fvsipt = 0
205700010907     c                   clear                   fwsvip
205800010911     c                   z-add     fvsipt        fwsipt
205900010907     c                   endif
206000010907     c                   if        fwsies = *zeros and fvsies = 0
206100010907     c                   clear                   fwsves
206200010911     c                   z-add     fvsies        fwsies
206300010907     c                   endif
206400000504     c
206500951214     C     W003A         LOOKUP    FLP                                    30
206600951214     C  N30FVSLNA        COMP      TSRFFA                                 30
206700980121     C**N30      KTSR11    READETNTSR01L                 31
206800980121     C  N30KTSR11        READE(N)  TNTSR01L                               31
206900951214    6C                   ENDDO
207000951214     C*
207100951214    6C     *IN31         IFEQ      *ON
207200951214     C                   MOVEA     MSG(32)       K78
207300951215     C                   MOVEA     W003A         K78(63)
207400951214     C                   MOVEA     K78           VSCMSG
207500951214     C                   SETON                                        289042
207600951214    6C                   ENDIF
207700951214    5C                   ENDIF
207800951214     C*
207900951214    5C     *IN28         IFEQ      *OFF
208000951215     C* VERIFICO SE FV ABBINATO HA DATA = DATA FVT
208100951214     C                   MOVEL     FVSDFV        WDATA
208200951214     C                   MOVEL     WGG           W0040             4 0
208300951214     C                   MOVE      WMM           W0040
208400951214    6C  N28W0040         IFNE      CDTT
208500951214     C                   MOVEL     'S'           HRFV
208600951214     C                   ELSE
208700951214     C                   CLEAR                   HRFV
208800951214    6C                   ENDIF
208900951214     C*
209000951214    5C                   ENDIF
209100951214    4C                   ENDIF
209200951214    3C                   ENDIF
209300951214    2C                   ENDIF
209400951214    1C                   ENDIF
209500960213     C*
209600960213     C                   ENDIF
209700951214     C*
209800951214     C                   ENDSR
209900960213     C**
210000960213     C************** CONTROLLO SE CI SONO BUCHI NEL TRAINO **********
210100960213     C     BUCHI         BEGSR
210200960213     C                   CLEAR                   WBUCO
210300960213     C* LETTURA IN TNTSW03L
210400960213     C                   Z-ADD     V2HTRN        KWTRN
210500960213     C                   Z-ADD     V2HDTN        KWDTN
210600960213     C                   MOVEL     'R'           KWTRC
210700960213     C     KTSW3         SETLL     TNTSW03L
210800960223     C     KTSW3         READE(N)  TNTSW03L                               30
210900960213    1C     *IN30         DOWEQ     *OFF
211000960213     C*
211100960213     C* SE ANNULLATO NON ABBINO SE E' QUELLA CHE ESPONE
211200960213    2C     TSWATV        IFNE      ' '
211300960213    3C     TSWNFE        IFEQ      TSWNFP
211400960213     C                   MOVEL     '1'           WBUCO             1
211500960213    3C                   ENDIF
211600960213     C*
211700960213   X2C                   ELSE
211800960213     C* SE TROVO ALMENO UNA TRATTA NON ABBINATA : '1' IN WBUCO
211900960213    3C     TSWVNF        IFEQ      0
212000960213     C                   MOVEL     '1'           WBUCO             1
212100960213     C                   SETON                                            30
212200960213    3C                   ENDIF
212300960213    2C                   ENDIF
212400960213     C*
212500960223     C  N30KTSW3         READE(N)  TNTSW03L                               30
212600960213    1C                   ENDDO
212700960213     C                   ENDSR
212800960213     C**
212900960213     C****** VEDO SE C'E' ERRORE E AGGIORNO TNTSW *******************
213000960213     C     ERRORE        BEGSR
213100960213     C*
213200960213     C   90              SETON                                        2232
213300960213     C   90              Z-ADD     NRR2          V2CREC
213400960213     C*
213500960213     C                   EXSR      AGGTSW
213600960213     C**
213700960213     C**
213800960213     C* SE TUTTE LE TRATTE DEL TRAINO SONO ANNULLATE, METTO LA "A"
213900960213     C* NELLA TESTATA
214000960213    2C  N90V2HTRC        IFEQ      'R'
214100960213     C     V2CSCE        ANDEQ     'A'
214200960213     C                   CLEAR                   WTTANN
214300960213     C                   MOVEL     V2HTRN        KWTRN
214400960213     C                   MOVEL     V2HDTN        KWDTN
214500960213     C                   MOVEL     'R'           KWTRC
214600960213     C     KTSW3         SETGT     TNTSW03L
214700960223     C     KTSW3         READPE(N) TNTSW03L                               31
214800960213    3C     *IN31         DOWEQ     *OFF
214900960213     C* SE NON E' ANNULLATA MEMORIZZO
215000960213    4C     TSWATV        IFEQ      ' '
215100960213     C                   MOVEL     'N'           WTTANN            1
215200960213     C                   SETON                                        31
215300960213    4C                   ENDIF
215400960213     C*
215500960223     C  N31KTSW3         READPE(N) TNTSW03L                               31
215600960213    3C                   ENDDO
215700960213     C*
215800960213     C* SE TUTTE ANNULLATE METTO LA "A" SULLA TESTATA
215900960213    3C     WTTANN        IFEQ      ' '
216000960213     C                   MOVEL     'T'           KWTRC
216100960223     C     KTSW3         CHAIN(N)  TNTSW03L                           31
216200960213     C* PRENDO IL NUMERO DI RECORD
216300010824     C     TSWNRR        CHAIN     TL44S02                            31
216400960213     C                   EXSR      CTRIND
216500960213     C  N31              MOVEL     'A'           V2CSCE
216600960213     C  N31              SETON                                        22
216700010824     C  N31              UPDATE    TL44S02
216800960213     C                   SETOFF                                       22
216900960213    3C                   ENDIF
217000960222     C*
217100960213   X2C                   ELSE
217200960213     C**
217300960213     C* CONTROLLO L'ANNULLAMENTO DEL TRAINO E/O TRATTA
217400960213    3C  N90V2HTRC        IFEQ      'T'
217500960213     C     SAVATB        ANDNE     V2CSCE
217600960213     C*
217700960222     C* MI SALVO IL NUMERO RELATIVO DI RECORD
217800960222     C                   Z-ADD     NRR2          SAVNRR
217900960222     C*
218000960213    4C     V2CSCE        IFEQ      'A'
218100960221     C                   MOVEL     'A'           WANN              1
218200960213     C                   ELSE
218300960221     C                   CLEAR                   WANN
218400960213    4C                   ENDIF
218500960213     C*
218600960222     C                   EXSR      ANNRIP
218700960222     C*
218800960222     C                   Z-ADD     SAVNRR        NRR2
218900010824     C     NRR2          CHAIN     TL44S02                            31
219000960222     C* SE HO RIPRISTINATO
219100960222     C*  CHAIN SULLA TESTATA PER VEDERE SE POSSO RICONFERMARE L'IMPORT
219200960222    4C     V2CSCE        IFEQ      ' '
219300960222     C                   EXSR      CONFIM
219400960222    5C     V2CICO        IFGT      0
219500960222     C                   EXSR      AGGTSW
219600960222    5C                   ENDIF
219700960222     C*
219800960222   X4C                   ELSE
219900960222     C* SE HO ANNULLATO
220000960222     C*   SCONFERMO L'IMPORTO
220100960222     C                   CLEAR                   V2CICO
220200960222     C                   EXSR      AGGTSW
220300960222    4C                   ENDIF
220400960222     C*
220500960222    3C                   ENDIF
220600960213    2C                   ENDIF
220700960213     C*
220800960213     C* ABBINAMENTO DI 1 FV: VEDO SE CONFERMARE L'IMPORTO
220900960222     C  N90WABBFV        IFEQ      'A'
221000960213     C                   EXSR      ABBIN
221100960222     C                   ENDIF
221200960222     C* SBIAMENTO DI 1 FV: VEDO SE SCONFERMARE
221300960222     C  N90WABBFV        IFEQ      'S'
221400960222     C                   EXSR      SBINAM
221500960222     C                   ENDIF
221600960213     C                   CLEAR                   WABBFV
221700960213     C                   SETOFF                                       221719
221800960213     C*
221900960213     C                   ENDSR
222000960213     C**
222100960213     C****** SE NON C'E' ERRORE AGGIORNO TNTSW **********************
222200960213     C     AGGTSW        BEGSR
222300960213     C*
222400960213    0C     *IN90         IFEQ      *OFF
222500960213     C                   Z-ADD     NRR2          KNRR
222600960213     C     KTSW11        CHAIN     TNTSW01L                           31
222700960213    1C     TSWTRC        IFEQ      'T'                                          TRAINO
222800960213     C                   MOVEL     TSWATV        SAVATB            1
222900960213     C*
223000960222     C     V2CICO        IFEQ      0
223100960222     C                   CLEAR                   V2HCNF
223200960222     C                   ENDIF
223300960222     C                   Z-ADD     V2CICO        TSWVIC
223400960222     C                   MOVEL     V2HCNF        TSWCNF
223500960213     C                   MOVEL     V2CPDR        TSWVPD
223600960213     C                   MOVEL     V2DPDR        TSWDPD
223700960213     C                   MOVEL     V2CSCE        TSWATV
223800960223     C                   MOVEL     V2CSCE        TSWATT
223900010910     C**!!!              MOVEL     V2CIMP        TSWILI
224000010910     C                   z-add     V2CIMP        TSWILI
224100960213     C                   MOVEL     V2CCAU        TSWVCU
224200960213     C* SOLO SE HO VARIATO QUALCHE COSA METTO LA "S" DI VARIATO RECORD
224300960213    2C     TSWVIC        IFNE      TSWICO
224400960213     C     TSWVPD        ORNE      TSWPDR
224500960213     C     TSWVCU        ORNE      TSWCAU
224600960222     C     TSWATV        ORNE      TSWATB
224700960213     C                   MOVEL     'S'           TSWFVR
224800960213   X2C                   ELSE
224900960213     C                   MOVEL     ' '           TSWFVR
225000960213    2C                   ENDIF
225100960213     C*
225200960213   X1C                   ELSE                                                   FVT
225300960213     C*
225400960213     C* SE ERA ANNULLATO E RIMANE ANNULLATO : NON FACCIO NULLA
225500960213    2C     TSWATV        IFNE      V2CSCE
225600960213     C     V2CSCE        OREQ      ' '
225700960213     C*
225800960213     C* SE ANNULLATO E' COME SE IL FV FOSSE STATO TOLTO
225900960213    3C     V2CSCE        IFEQ      'A'
226000960213     C                   CLEAR                   V2CNFV
226100960213     C* SE ERA ANNULLATO E L'HO RIATTIVATO: IMPOSTO IL FV CHE C'ERA
226200960213     C* NEL FILE ORIGINALE
226300960213   X3C                   ELSE
226400960213    4C     TSWATV        IFEQ      'A'
226500960213     C     TSWNFV        ANDGT     0
226600960213     C                   MOVEL     TSWRFI        V2HRFV
226700960213     C                   MOVEL     TSWNFV        V2CNFV
226800960222     C                   MOVEL     'A'           WABBFV            1
226900960213    4C                   ENDIF
227000960213    3C                   ENDIF
227100960213     C*
227200960213     C                   MOVEL     V2CSCE        TSWATV
227300960213     C                   MOVEL     V2HATT        TSWATT
227400960213     C                   MOVEL     V2CNFV        TSWVNF
227500960213    3C     TSWVNF        IFNE      TSWNFV
227600960222    3C     TSWATV        ORNE      TSWATB
227700960213     C                   MOVEL     'S'           TSWFVR
227800960213     C                   ELSE
227900960213     C                   MOVEL     ' '           TSWFVR
228000960213    3C                   ENDIF
228100960213     C*
228200960213    3C     V2CNFV        IFLE      *ZEROS
228300960213     C                   MOVEL     'S'           TSWRFV
228400960213     C                   ELSE
228500960213     C                   CLEAR                   TSWRFV
228600960213     C* CI RIMUOVO QUELLO CHE C'ERA NEL SFL, CHE MAGARI E'CAMBIATO PER
228700960213     C*  CAMBIO NUMERO DI FV
228800960213     C                   MOVEL     V2HRFV        TSWRFV
228900960213    3C                   ENDIF
229000960213     C*
229100960213     C                   MOVEL     TSWRFV        V2HRFV
229200960213    2C                   ENDIF
229300960213    1C                   ENDIF
229400960213     C*
229500960213     C                   UPDATE    TNTSW000
229600960213     C*
229700960213     C*
229800960213     C* SE HO ANNULLATO O RIABBINATO --> MEMORIZZO
229900960213    1C     V2HTRC        IFEQ      'R'
230000960213     C     V2CNFV        ANDNE     V2HNFV
230100960213     C*
230200960213     C                   MOVEL     V2HNFV        WONFV
230300960213    2C     V2CNFV        IFGT      *ZEROS
230400960213     C                   MOVEL     V2CNFV        WNNFV
230500960213     C                   Z-ADD     V2HDTN        WADTN
230600960213     C                   Z-ADD     V2HTRN        WATRN
230700960222     C                   MOVEL     'A'           WABBFV
230800960213   X2C                   ELSE
230900960213     C                   CLEAR                   WNNFV
231000960222     C                   MOVEL     'S'           WABBFV
231100960213    2C                   ENDIF
231200960213     C                   MOVE      V2CFFP        KLNP
231300960222     C                   MOVE      V2HNFP        WANFP
231400960213     C*
231500960213     C                   EXSR      UPTSW
231600960213     C*
231700960213     C* SE NON' C'E' ERRORE --> MEMORIZZO IL NUOVO FV
231800960213     C                   MOVEL     V2CNFV        V2HNFV
231900960213    2C     V2HNFV        IFLE      *ZEROS
232000960213     C                   MOVEL     *ZEROS        V2HNFV
232100960213    2C                   ENDIF
232200960213     C*
232300960213    1C                   ENDIF
232400960213    0C                   ENDIF
232500960213     C*
232600960213     C* SISTEMO GLI INDICATORI
232700960213     C                   EXSR      CTRIND
232800960213     C*
232900010824     C                   UPDATE    TL44S02
233000960213     C                   ENDSR
233100960213     C**
233200960221     C****** AGGIORNO NEL SFL E IN TSW LE TRATTE IN BASE ALLA TESTA****
233300960222     C     ANNRIP        BEGSR
233400960213     C                   Z-ADD     V2HTRN        KWTRN
233500960213     C                   Z-ADD     V2HDTN        KWDTN
233600960213     C                   MOVEL     'R'           KWTRC
233700960223     C     KTSW3         CHAIN(N)  TNTSW03L                           31
233800960213     C     *IN31         DOWEQ     *OFF
233900960213     C* CHAIN SUL SFL
234000010824     C     TSWNRR        CHAIN     TL44S02                            32
234100960213     C     *IN32         IFEQ      *OFF
234200960213     C*
234300960221     C     WANN          IFEQ      'A'
234400960213     C                   MOVEL     'A'           V2CSCE
234500960213     C                   MOVEL     'A'           V2HATT
234600960213     C                   ELSE
234700960213     C                   CLEAR                   V2CSCE
234800960213     C                   CLEAR                   V2HATT
234900960213     C                   ENDIF
235000960213     C* FACCIO GIRARE L'AGGIORNAMENTO
235100960213     C                   EXSR      AGGTSW
235200960213     C                   ENDIF
235300960213     C*
235400960222     C                   MOVEL     'R'           KWTRC
235500960223     C     KTSW3         READE(N)  TNTSW03L                               31
235600960213     C                   ENDDO
235700960222     C*
235800960222     C* NON DEVO ANDARE IN NESSUN ABBINAMENTO E/O SBINAMENTO
235900960222     C                   CLEAR                   WABBFV
236000960213     C                   ENDSR
236100960222     C**
236200960223     C********** AGGIORNAMENTO GRIGLIA ********************************
236300960222     C     REGIS2        BEGSR
236400960222     C                   CLEAR                   WF20
236500960222     C                   CLEAR                   KNRR
236600960223     C*
236700960223     C* PRIMA LEGGO LE TESTATE TRAINI ANNULLATE O RIPRISTINATE
236800960223     C                   MOVEL     'T'           KWTRC
236900960223     C                   MOVEL     'S'           KWFVR
237000960223     C     KTSW4         SETLL     TNTSW04L
237100960223     C     KTSW4         READE     TNTSW04L                               31
237200960222     C*
237300960222    1C     *IN31         DOWEQ     *OFF
237400960223     C*
237500960226    2C     TSWATV        IFNE      TSWATB
237600960223     C                   MOVEL     TSWTRN        KVTRN
237700960223     C                   MOVEL     TSWDTN        KVDTN
237800960223     C***
237900960223     C* ANNULLAMENTO DI UN TRAINO
238000960223     C***
238100960226    3C     TSWATV        IFEQ      'A'
238200960223     C                   EXSR      ANNTRN
238300980206     C                   ELSE
238400960223     C**
238500960223     C** RIPRISTINO DI UNA GRIGLIA
238600960223     C**
238700980206    4C     TSWATV        IFEQ      ' '
238800960223     C                   EXSR      RIPTRN
238900980206    4C                   ENDIF
239000980206    3C                   ENDIF
239100960226    2C                   ENDIF
239200960223     C*
239300960223     C                   MOVEL     'T'           KWTRC
239400960223     C     KTSW4         READE     TNTSW04L                               31
239500960223    1C                   ENDDO
239600960222     C**
239700960223     C** LEGGO I RIPRISTINI DI TRATTA
239800960223     C**
239900960223     C                   MOVEL     ' '           WATB              1
240000960223     C                   EXSR      ARTRA
240100960223     C**
240200960223     C* LEGGO GLI ANNULLAMENTI DI TRATTA
240300960223     C**
240400960223     C                   MOVEL     'A'           WATB              1
240500960223     C                   EXSR      ARTRA
240600960223     C**
240700960223     C* LEGGO LE TRATTE VARIATE RIMASTE
240800960223     C**
240900960223     C                   MOVEL     'R'           KWTRC
241000960223     C     KTSW4         SETLL     TNTSW04L
241100960223     C     KTSW4         READE     TNTSW04L                               31
241200960223     C*
241300960223    1C     *IN31         DOWEQ     *OFF
241400960223     C*
241500960223    2C     TSWVNF        IFEQ      0
241600960305     C     TSWVNF        ANDNE     TSWNFV
241700960223     C* SBINO IL FV
241800960223     C                   MOVEL     '4'           WAGTSR            1
241900960223     C                   MOVEL     TSWNFV        KNFV
242000960223     C                   MOVEL     TSWFFP        KLNP
242100960223     C     KTSR4         SETLL     TNTSR04L
242200960223     C     KTSR4         READE     TNTSR04L                               32
242300960223    3C     *IN32         DOWEQ     *OFF
242400960223     C* SBINAMENTO TRATTA
242500960223     C                   EXSR      SBITSR
242600960223     C*
242700960223     C     KTSR4         READE     TNTSR04L                               32
242800960223    3C                   ENDDO
242900960223     C* SBINAMENTO FV
243000960223     C                   EXSR      SBIFVS
243100960223    2C                   ENDIF
243200960223     C**
243300960223    2C     TSWVNF        IFGT      0
243400960305     C     TSWVNF        ANDNE     TSWNFV
243500960223     C* ABBINO IL FV
243600960226     C                   MOVE      ' '           WVOLTA            1
243700960226     C                   MOVEL     TSWDTN        KVDTN
243800960226     C                   MOVEL     TSWTRN        KVTRN
243900960226     C                   MOVEL     TSWNFP        KVNFP
244000960226     C     KTSR12        SETLL     TNTSR01L
244100960226     C     KTSR12        READE     TNTSR01L                               32
244200960226     C     *IN32         DOWEQ     *OFF
244300960226     C                   EXSR      ABBTSR
244400960226     C*
244500960226     C     KTSR12        READE     TNTSR01L                               32
244600960226     C                   ENDDO
244700020109     C* SBINAMENTO FV vecchio
244800020110     c     tswnfv        ifgt      0
244900020109     C                   MOVEL     TSWNFV        KNFV
245000020109     C                   MOVEL     TSWFFP        KLNP
245100020109     C                   EXSR      SBIFVS
245200020110     c                   endif
245300960223     C                   ENDIF
245400960223     C*
245500960223     C* AGGIORNO TSW
245600960223     C                   MOVEL     TSWVNF        TSWNFV
245700960223     C                   MOVEL     TSWRFV        TSWRFI
245800960223     C                   CLEAR                   TSWFVR
245900960223     C                   UPDATE    TNTSW004
246000960223     C**
246100960223     C     KTSW4         READE     TNTSW04L                               31
246200960223    2C                   ENDDO
246300960226     C**
246400960226     C* LEGGO LE TESTATE
246500960226     C**
246600960226     C                   MOVEL     'T'           KWTRC
246700960226     C     KTSW4         SETLL     TNTSW04L
246800960226     C     KTSW4         READE     TNTSW04L                               31
246900960226     C*
247000960226    1C     *IN31         DOWEQ     *OFF
247100960226     C* CHAIN SULLA TESTATA
247200960226     C                   MOVEL     TSWDTN        KVDTN
247300960226     C                   MOVEL     TSWTRN        KVTRN
247400960226     C     KTSR13        CHAIN     TNTST01L                           32
247500960226     C                   EXSR      UPTST
247600960226     C**
247700960226     C* AGGIORNO TSW
247800960226     C                   MOVEL     TSWVCU        TSWCAU
247900960226     C                   MOVEL     TSWVPD        TSWPDR
248000960226     C                   MOVEL     TSWVIC        TSWICO
248100960226     C                   CLEAR                   TSWFVR
248200960229     C                   UPDATE    TNTSW004
248300960229     C*
248400960229     C* CHAIN TSR  IL RECORD CON L'IMPORTO DA CONFERMARE
248500960229     C                   CLEAR                   WVOLTA
248600960229     C                   Z-ADD     TSTNFE        KVNFP
248700960229     C     KTSR12        SETLL     TNTSR01L
248800960229     C     KTSR12        READE     TNTSR01L                               30
248900960229    2C     *IN30         DOWEQ     *OFF
249000960229     C*
249100960229     C* IL PRIMO RECORD COSI', GLIALTRI SEMPRE 9999999
249200960229     C     WVOLTA        IFEQ      ' '
249300960229     C                   MOVEL     '1'           WVOLTA
249400960229     C*
249500960229    3C     TSTICO        IFGT      0
249600960229    4C     TSTCNF        IFEQ      '1'
249700960229     C                   MOVEL     *ALL'9'       TSRDKI
249800960305     C                   MOVEL     'S'           TSRFKI
249900960229     C                   ELSE
250000960229     C                   MOVEL     TSRDTT        TSRDKI
250100960305     C                   MOVEL     'S'           TSRFKI
250200960229    4C                   ENDIF
250300960229   X3C                   ELSE
250400960229     C**
250500960229    4C     TSRDKI        IFEQ      *HIVAL
250600960229     C                   CLEAR                   TSRDKI
250700960229    4C                   ENDIF
250800960229    3C                   ENDIF
250900960229     C**
251000960229   X2C                   ELSE
251100960229     C                   MOVEL     *ALL'9'       TSRDKI
251200960305     C                   MOVEL     'S'           TSRFKI
251300960229    2C                   ENDIF
251400960229     C**
251500960229     C                   UPDATE    TNTSR000
251600960229     C     KTSR12        READE     TNTSR01L                               30
251700960229    1C                   ENDDO
251800960229     C**
251900960226     C     KTSW4         READE     TNTSW04L                               31
252000960226    2C                   ENDDO
252100960226     C**
252200960222     C                   ENDSR
252300960223     C**
252400960223     C************** ANNULLAMENTO DI UN TRAINO ************************
252500960223     C     ANNTRN        BEGSR
252600960223     C                   MOVEL     'A'           TSWATB
252700960223     C                   CLEAR                   TSWFVR
252800960223     C                   UPDATE    TNTSW004
252900960223     C**
253000960223     C* CHAIN SULLA TESTATA E AGGIORNO
253100960223     C     KTSR13        CHAIN     TNTST01L                           32
253200960223    1C     *IN32         IFEQ      *OFF
253300960223     C                   EXSR      UPTST
253400960304     C**
253500960304     C* LETTURA E ANNULLAMENTO  DEI MC
253600960229     C     KTSR13        SETLL     TNTSM01L
253700960229    2C     KTSR13        READE     TNTSM01L                               30
253800960229     C     *IN30         DOWEQ     *OFF
253900960304     C                   MOVEL     'A'           TSMATB
254000960304     C                   UPDATE    TNTSM000
254100960304     C*
254200960229     C     KTSR13        READE     TNTSM01L                               30
254300960229    2C                   ENDDO
254400960304     C**
254500960223     C* LEGGO LE TRATTE E LE SBINO
254600960223     C                   MOVEL     'R'           KWTRC
254700960223     C                   MOVEL     TSWTRN        KWTRN
254800960223     C                   MOVEL     TSWDTN        KWDTN
254900960223     C     KTSW3         SETLL     TNTSW03L
255000960223     C     KTSW3         READE     TNTSW03L                               32
255100960229     C*
255200960223    2C     *IN32         DOWEQ     *OFF
255300960223     C                   MOVEL     TSWNFP        KVNFP
255400960223     C     KTSR12        SETLL     TNTSR01L
255500960223     C     KTSR12        READE     TNTSR01L                               32
255600960229    3C     *IN32         DOWEQ     *OFF
255700960223     C                   MOVEL     '1'           WAGTSR            1
255800960229     C                   MOVEL     'A'           TSRATB
255900960223     C                   EXSR      SBITSR
256000960223     C*
256100960223     C     KTSR12        READE     TNTSR01L                               32
256200960229    3C                   ENDDO
256300960223     C*
256400960229     C* SE C'E' FV
256500960229     C* CHAIN SU TNTSW02L PER CANCELLARE IL RECORD DEL FV SBINATO
256600960229    3C     TSWNFV        IFGT      0
256700960223     C                   MOVEL     'S'           KWTRC
256800960223     C                   MOVEL     TSWNFV        KNFV
256900960223     C                   MOVEL     TSWFFP        KLNP
257000960223     C     KTSW2         DELETE    TNTSW002                           32
257100960223     C*
257200960223     C* SBINO IL FOGLIO  VIAGGIO SU FVS
257300960223     C                   EXSR      SBIFVS
257400960223    3C                   ENDIF
257500960223     C**
257600960229     C* TOLGO LA "S" DI VARIATO
257700960229     C                   MOVEL     'A'           TSWATB
257800960229     C                   CLEAR                   TSWRFI
257900960229     C                   CLEAR                   TSWNFV
258000960229     C                   CLEAR                   TSWFVR
258100960229     C                   UPDATE    TNTSW003
258200960229     C**
258300960223     C     KTSW3         READE     TNTSW03L                               32
258400960223    2C                   ENDDO
258500960223    1C                   ENDIF
258600960223     C                   ENDSR
258700960223     C**
258800960223     C***************** RIPRISTINO UN TRAINO *************************+
258900960223     C     RIPTRN        BEGSR
259000960223     C                   MOVEL     ' '           TSWATB
259100960223     C                   UPDATE    TNTSW004
259200960223     C*
259300960223     C* DELETO LA GRIGLIA E LA RICREO
259400960223     C     KTSR13        CHAIN     TNTST01L                           32
259500960223    3C     *IN32         IFEQ      *OFF
259600960223     C*
259700960305     C     KTSR13        SETLL     TNTSR01L
259800960305     C     KTSR13        READE     TNTSR01L                               32
259900960223    4C     *IN32         DOWEQ     *OFF
260000960223     C                   DELETE    TNTSR000
260100960305     C     KTSR13        READE     TNTSR01L                               32
260200960223    4C                   ENDDO
260300960223     C*
260400960305     C     KTSR13        SETLL     TNTSM01L
260500960305     C     KTSR13        READE     TNTSM01L                               32
260600960223    4C     *IN32         DOWEQ     *OFF
260700960223     C                   DELETE    TNTSM000
260800960305     C     KTSR13        READE     TNTSM01L                               32
260900960223    4C                   ENDDO
261000960223     C*
261100960223     C                   DELETE    TNTST000
261200960223    3C                   ENDIF
261300960223     C* RICREO
261400960226     C                   CLEAR                   DSTL24
261500960223     C                   Z-ADD     TSWTRN        D24TRN
261600960223     C                   Z-ADD     TSWDTN        D24DTD
261700960223     C                   MOVEL     DSTL24        KPJBU
261800960223     C                   CALL      'TNTL24R'
261900960223     C                   PARM                    KPJBA
262000960304     C**
262100960304     C* LEGGO LE TRATTE SU TSW E SISTEMO IL FLAG DI ANNULLAMENTO
262200960304     C                   MOVEL     'R'           KWTRC
262300960304     C                   MOVEL     KVDTN         KWDTN
262400960304     C                   MOVEL     KVTRN         KWTRN
262500960304     C     KTSW3         SETLL     TNTSW03L
262600960304     C     KTSW3         READE     TNTSW03L                               32
262700960304     C     *IN32         DOWEQ     *OFF
262800960304     C                   MOVEL     TSWATV        TSWATB
262900960304     C                   UPDATE    TNTSW003
263000960304     C*
263100960304     C     KTSW3         READE     TNTSW03L                               32
263200960304     C                   ENDDO
263300960304     C*
263400960223     C                   ENDSR
263500960223     C**
263600960226     C************** ANNULLA/RIPRISTINA UNA TRATTA *******************
263700960223     C     ARTRA         BEGSR
263800960305     C                   MOVEL     'R'           KWTRC
263900960223     C*
264000960223     C     KTSW4         SETLL     TNTSW04L
264100960223     C     KTSW4         READE     TNTSW04L                               31
264200960223     C*
264300960223    1C     *IN31         DOWEQ     *OFF
264400960223     C*
264500960226    2C     TSWATB        IFNE      TSWATV
264600960223     C     TSWATV        ANDEQ     WATB
264700960223     C                   MOVEL     TSWATV        TSWATB
264800960305     C*
264900960305     C* MI SALVO I CAMPI CHE DOPO USO PER RIPOSIZIONARMI
265000960305     C                   MOVEL     TSWNFE        SAVNFE
265100960305     C                   MOVEL     TSWNFP        SAVNFP
265200960305     C                   MOVEL     TSWFFP        SAVFFP
265300960305     C                   MOVEL     TSWNFV        SAVNFV
265400960305     C*
265500960226    3C     WATB          IFEQ      'A'
265600960223     C                   CLEAR                   TSWRFI
265700960223     C                   CLEAR                   TSWNFV
265800960305     C                   CLEAR                   TSWRFV
265900960305     C                   CLEAR                   TSWVNF
266000960223     C                   CLEAR                   TSWFVR
266100960226    3C                   ENDIF
266200960223     C*
266300960223     C                   UPDATE    TNTSW004
266400960223     C                   MOVEL     TSWTRN        KVTRN
266500960223     C                   MOVEL     TSWDTN        KVDTN
266600960223     C                   MOVEL     TSWNFP        KVNFP
266700960226     C**
266800960226     C* AGGIORNO IL FILE DEI MC
266900960226     C     KTSR13        SETLL     TNTSM01L
267000960226     C     KTSR13        READE     TNTSM01L                               30
267100960226     C     *IN30         DOWEQ     *OFF
267200960226     C*
267300960226     C     TSMTFP        IFEQ      TSWFFP
267400960226     C                   MOVEL     WATB          TSMATB
267500960226     C                   UPDATE    TNTSM000
267600960226     C                   ENDIF
267700960226     C*
267800960226     C     KTSR13        READE     TNTSM01L                               30
267900960226     C                   ENDDO
268000960226     C**
268100960223     C*
268200960223     C* CERCO LA DATA DECORRENZA
268300960223     C                   EXSR      RICDEC
268400960223     C     KTLR          CHAIN     TNTLR01L                           30
268500960223     C*
268600960223    3C     TLRCAR        IFEQ      'S'
268700960223     C     TLRSCA        ANDEQ     'S'
268800960223     C                   MOVEL     'S'           WDELWR            1
268900960223     C                   MOVEL     TLRTFM        WTFM
269000960223     C                   ELSE
269100960223     C                   CLEAR                   WDELWR
269200960223    3C                   ENDIF
269300960223     C*
269400960223     C     KTSR12        SETLL     TNTSR01L
269500960223     C     KTSR12        READE     TNTSR01L                               32
269600960223    3C     *IN32         DOWEQ     *OFF
269700960223     C* RIPRISTINO
269800960223    4C     WATB          IFEQ      ' '
269900960223     C                   CLEAR                   TSRATB
270000960223     C                   CLEAR                   TSRDKP
270100960223     C                   CLEAR                   TSRDKA
270200960223     C                   UPDATE    TNTSR000
270300960226   X4C                   ELSE
270400960305     C**
270500960305     C* ANNULLAMENTO
270600960305     C                   MOVEL     'A'           TSRATB
270700960305     C                   MOVEL     '1'           WAGTSR            1
270800960223     C                   EXSR      SBITSR
270900960223    4C                   ENDIF
271000960223     C*
271100960223     C* VEDO SE DEVO DELETARE/SCRIVERE DEI RECORD
271200960223    4C     WDELWR        IFEQ      'S'
271300960223     C                   SETON                                        34
271400960223     C     KTLR          SETLL     TNTLR01L
271500960223     C     KTLR1         READPE    TNTLR01L                               30
271600960223     C*
271700960223    5C     *IN30         DOWEQ     *OFF
271800960223     C     *IN34         ANDEQ     *ON
271900960223     C     WTFM          ANDEQ     TLRTFM
272000960223     C* VEDO SE E' FERMATA DI CARICO PRESENTE NEI MC CON TLMTFA=TSRTFA
272100960226    6C     TLRCAR        IFEQ      'S'
272200960223     C     KTLM          CHAIN     TNTLM01L                           34
272300960223    7C     *IN34         DOWEQ     *OFF
272400960223     C     TLMATB        ANDNE     ' '
272500960223     C     KTLM          READE     TNTLM01L                               34
272600960223    7C                   ENDDO
272700960223     C*
272800960223    7C     *IN34         IFEQ      *OFF
272900960223     C* PREPARO LA KEY
273000960223    8C     WATB          IFEQ      ' '
273100960223     C                   MOVEL     TLRNFM        KVNFP
273200960223     C                   MOVEL     TLRFIL        KVFFP
273300960223     C     KTSR14        DELETE    TNTSR000                           33        DELETO
273400960223    8C                   ELSE
273500960223     C* PREPARO IL RECORD TSR
273600960305     C* CHAINO TSR03L PER PRENDERE I DATI DELLA FERMATA DI PARTENZA
273700960305     C                   MOVEL     TLRNFM        TSRNFP
273800960305     C                   MOVEL     TLRFIL        TSRFFP
273900960305     C                   MOVEL     ' '           TSRATB
274000960305     C     KTSR31        CHAIN     TNTSR03L                           33
274100960305     C                   MOVEL     T03DTP        TSRDTP
274200960305     C                   MOVEL     T03HTP        TSRHTP
274300960305     C                   MOVEL     T03HRP        TSRHRP
274400960305     C                   MOVEL     T03DRP        TSRDRP
274500960305     C                   MOVEL     T03RTP        TSRRTP
274600960305     C                   MOVEL     T03LNP        TSRLNP
274700960305     C                   MOVEL     T03NFV        TSRNFV
274800960305     C                   MOVEL     T03DFV        TSRDFV
274900960305     C                   MOVEL     *ALL'9'       TSRDKP
275000960305     C                   MOVEL     'S'           TSRFKP
275100960305     C                   MOVEL     *ALL'9'       TSRDKI
275200960305     C                   MOVEL     'S'           TSRFKI
275300960305     C* MUOVO TUTTI I CAMPI RELATIVI ALLA PARTENZA DI QUESTO NEW REC
275400020312     C                   WRITE     TNTSR000                             33      WRITE
275500960223     C* SE LA FERMATA IN TSR ERA GIA' ABBINATA, DEVO ABBINARE ANCHE
275600960223     C*  QUESTA TRATTA
275700960223    8C                   ENDIF
275800960223    7C                   ENDIF
275900960223     C*
276000960223    6C                   ENDIF
276100960223     C*
276200960223     C     KTLR1         READPE    TNTLR01L                               30
276300960223    5C                   ENDDO
276400960223    4C                   ENDIF
276500960223     C*
276600960223     C* MI RIPOSIZIONO SUL RECORD LETTO
276700960305     C                   MOVEL     SAVNFP        KVNFP
276800960305     C                   MOVEL     SAVFFP        KVFFP
276900960223     C     KTSR14        SETGT     TNTSR000
277000960223     C*
277100980203     C     KTSR12        READE     TNTSR01L                               32
277200980203     C***        KTSR12    READETNTSR01L            N    32
277300960223    3C                   ENDDO
277400960223     C**
277500960223     C* SE STO ANNULLANDO FACCIO ALTRA COSE:
277600960223     C*  1) SBINO FVS
277700960305    3C     WATB          IFEQ      'A'
277800960305     C*
277900960305   3AC     SAVNFV        IFGT      0
278000960305     C* DELETO IL TSW IL RECORD DI SBINAMENTO
278100960305     C                   MOVEL     'S'           KWTRC
278200960305     C                   MOVEL     SAVNFV        KNFV
278300960305     C                   MOVEL     SAVFFP        KLNP
278400960305     C     KTSW2         DELETE    TNTSW002                           33
278500960305     C*
278600960305     C                   Z-ADD     SAVNFV        KNFV
278700960305     C                   Z-ADD     SAVFFP        KLNP
278800960223     C                   EXSR      SBIFVS
278900960305   3AC                   ENDIF
279000960223     C*
279100960223     C* SE E' IL FV CHE ESPONE, MODIFICO FES SULLA TESTATA
279200960305    4C     SAVNFE        IFEQ      KVNFP
279300960223     C*
279400960223     C* LEGGO LA PRIMA FERMATA DI CARICO <> DA KVNFP E LA METTO
279500980121     C****       KTSR13    CHAINTNTSR01L             33
279600980121     C     KTSR13        CHAIN(N)  TNTSR01L                           33
279700960223     C*
279800960223    5C     *IN33         DOWEQ     *OFF
279900960223     C*
280000960223    6C     TSRNFP        IFNE      KVNFP
280100960223     C     TSRATB        ANDEQ     ' '
280200960223     C     KTSR13        CHAIN     TNTST01L                           33
280300960223    7C     *IN33         IFEQ      *OFF
280400960223     C                   Z-ADD     TSRNFP        TSTNFE
280500960223     C                   Z-ADD     TSRFFP        TSTFES
280600960223     C                   UPDATE    TNTST000
280700960223    7C                   ENDIF
280800960223     C*
280900960223     C                   SETON                                        33
281000960223   X6C                   ELSE
281100980121     C*****      KTSR13    READETNTSR01L                 33
281200980121     C     KTSR13        READE(N)  TNTSR01L                               33
281300960223    6C                   ENDIF
281400960223    5C                   ENDDO
281500960223    4C                   ENDIF
281600960223    3C                   ENDIF
281700960223    2C                   ENDIF
281800960223     C*
281900960305     C                   MOVEL     'R'           KWTRC
282000960223     C     KTSW4         READE     TNTSW04L                               31
282100960223    1C                   ENDDO
282200960223     C                   ENDSR
282300960223     C**
282400960223     C************* AGGIORNAMENTO TESTATA GRIGLIA *********************
282500960223     C     UPTST         BEGSR
282600960223     C                   MOVEL     TSWATV        TSTATB
282700960223     C                   MOVEL     TSWVPD        TSTPDR
282800960226     C                   MOVEL     TSWVCU        TSTCAU
282900010827     C**!!!              MOVEL     TSWVIC        TSTICO
283000010827     C                   z-add     TSWVIC        TSTICO
283100960223     C                   MOVEL     TSWCNF        TSTCNF
283200960223     C                   MOVEL     TSWTMZ        TSTTMZ
283300010827     C**!!!              MOVEL     TSWILI        TSTILI
283400010827     C                   z-add     TSWILI        TSTILI
283500960226     C* SE HO SCONFERMATO L'IMPORTO LO MEMORIZZO
283600960223     C     TSWICO        IFNE      TSWVIC
283700960223     C     TSWVIC        ANDEQ     0
283800960223     C                   MOVEL     'S'           WSCONF            1
283900960223     C                   ELSE
284000960223     C                   CLEAR                   WSCONF
284100960223     C                   ENDIF
284200010827     C**!!!              MOVEL     TSWIPT        TSTIPT
284300010827     C**!!!              MOVEL     TSWIES        TSTIES
284400010827     C                   z-add     TSWIPT        TSTIPT
284500010827     C                   z-add     TSWIES        TSTIES
284600960223     C                   UPDATE    TNTST000
284700960223     C                   ENDSR
284800960223     C**
284900960223     C************* SBINAMENTO SU TSR *********************************
285000960223     C     SBITSR        BEGSR
285100960223     C                   MOVE      *ZEROS        TSRDRP
285200960223     C                   MOVE      *ZEROS        TSRHRP
285300960223     C                   MOVE      *BLANKS       TSRRTP
285400960223     C                   MOVE      *ZEROS        TSRDRA
285500960223     C                   MOVE      *ZEROS        TSRHRA
285600960223     C                   MOVE      *BLANKS       TSRRTA
285700960223     C                   MOVE      *ZEROS        TSRLNP
285800960223     C                   MOVE      *ZEROS        TSRNFV
285900960223     C                   MOVE      *ZEROS        TSRDFV
286000960223     C                   MOVE      *ZEROS        TSRDKP
286100960223     C                   MOVE      *ZEROS        TSRDKA
286200960223     C                   MOVE      *ZEROS        TSRDKI
286300960223     C     WAGTSR        IFEQ      '1'
286400960223     C                   UPDATE    TNTSR000
286500960223     C                   ELSE
286600960223     C                   UPDATE    TNTSR004
286700960223     C                   ENDIF
286800960223     C                   ENDSR
286900960226     C**
287000960226     C************* ABBINAMENTO SU TSR ********************************
287100960226     C     ABBTSR        BEGSR
287200960226     C* DA ESEGUIRE SOLO LA PRIMA VOLTA
287300960226    1C     WVOLTA        IFEQ      *BLANKS
287400960226     C*
287500970328     C* Memorizzo se data/ora partenza del FV = data/ora partenza teor.
287600970328     C     FVSDRP        COMP      TSRDTP                                 18
287700970328     C   18FVSHRP        COMP      TSRHTP                                 18
287800960226     C* Controlli già effettuati: memorizzo se variato qualcosa
287900960229    2C     TSRFKP        IFNE      ' '
288000960226    3C     FVSDRP        IFNE      TSRDRP
288100960226     C     FVSHRP        ORNE      TSRHRP
288200960226     C     FVSRTP        ORNE      TSRRTP
288300960226     C                   MOVE      'M'           WMODIF            1
288400970328     C* Non segnalo comunque variazione di data/ora se partenza puntua-
288500970328     C* le sia prima della variazione che adesso
288600970328    4C     *IN18         IFEQ      *ON
288700970328     C     TSRDKP        ANDEQ     99999999
288800970328     C     FVSRTP        ANDEQ     *BLANKS
288900970328     C     TSRRTP        ANDEQ     *BLANKS
289000970328     C                   CLEAR                   WMODIF
289100970328    4C                   ENDIF
289200960226    3C                   ENDIF
289300960226    2C                   ENDIF
289400960226     C                   MOVE      '1'           WVOLTA
289500960226    1C                   ENDIF
289600960226     C*
289700960226     C* Se variato qualcosa e controlli già effettuati, solo sul primo
289800960226     C* record azzero la data controllo importi mentre sui record suc-
289900960226     C* cessivi la imposto=99999999 per non essere letti da pgm control
290000960305    2C     WMODIF        IFEQ      'M'
290100960226    3C     WVOLTA        IFEQ      '1'
290200960226     C                   MOVE      *ZEROS        TSRDKP
290300960226     C                   MOVE      '2'           WVOLTA
290400960226   X3C                   ELSE
290500960226     C                   MOVE      99999999      TSRDKP
290600960229     C                   MOVE      'S'           TSRFKP
290700960226    3C                   END
290800960305    2C                   END
290900960226     C*
291000960226     C* CHAIN SU FVS PER AGGIORNAMENTO
291100960226     C                   MOVEL     TSWVNF        KNFV
291200960226     C                   MOVEL     TSWFFP        KLNP
291300960226     C     KTSR4         CHAIN     TNFVS01L                           33
291400960226     C                   MOVE      FVSDRP        TSRDRP                         DATA PART.R.
291500960226     C                   MOVE      FVSHRP        TSRHRP                         ORA  PART.R.
291600960226     C                   MOVE      FVSRTP        TSRRTP                         RITARD PART
291700960226     C                   MOVE      FVSLNP        TSRLNP                         LNP F.V.
291800960226     C                   MOVE      FVSNFV        TSRNFV                         NR.F.V.
291900960226     C                   MOVE      FVSDFV        TSRDFV                         DATA F.V.
292000960226     C                   UPDATE    TNTSR000
292100960226     C*
292200960305     C                   MOVEL     TSRTFM        FVSTFS
292300960226     C                   Z-ADD     KVTRN         FVSTRS
292400960226     C                   MOVEL     'S'           FVSABN
292500960226     C                   UPDATE    TNFVS000
292600960226     C* MEMORIZZO CHE DEVO LANCIARE IL PGM DELLE DATE/ORE DI ARRIVO
292700960226     C                   MOVEL     '1'           WTL26             1
292800960904     C* RICHIAMO PGM CHE APPOGGIA MC REALI SU GRIGLIA
292900960904     C                   MOVEL     '1'           WCL33             1
293000960904     C                   CLEAR                   DSTL33
293100960904     C                   MOVE      FVSLNP        D33LNP
293200960904     C                   MOVE      FVSNFV        D33NFV
293300960904     C                   MOVE      TSRDTN        D33DTN
293400960904     C                   MOVEL     DSTL33        KPJBU
293500960904     C                   CALL      'TNTL33R'
293600960904     C                   PARM                    KPJBA
293700960226     C                   ENDSR
293800960223     C**
293900960223     C************* SBINAMENTO SU FVS *********************************
294000960223     C     SBIFVS        BEGSR
294100960223     C     KTSR4         CHAIN     TNFVS01L                           31
294200960223     C                   CLEAR                   FVSTRS
294300960305     C                   CLEAR                   FVSTFS
294400960223     C     FVSTRN        IFGT      *ZEROS
294500960223     C                   MOVE      *BLANKS       FVSABN
294600960223     C                   END
294700960304     C*
294800960223     C                   UPDATE    TNFVS000
294900960223     C**
295000960226     C     KTSR4         SETLL     FNFV201L
295100960223     C     KTSR4         READE     FNFV201L                               30
295200960223    4C     *IN30         DOWEQ     *OFF
295300960223     C                   MOVE      *BLANKS       FV2ABN
295400960223     C                   UPDATE    FNFV2000
295500960223     C     KTSR4         READE     FNFV201L                               30
295600960223    4C                   ENDDO
295700960223     C                   ENDSR
295800960223     C**
295900951213     C********** CONTROLLO DATI SFL04 *********************************
296000951213     C     CTRS04        BEGSR
296100951214     C                   CLEAR                   W0040
296200951214     C* CONTROLLO I CAMPI DELLA RICERCA NEL SFLCTL
296300971204     C* LA RICERCA NON E' ATTIVATA!!!1
296400971204     C***                  Z-ADDV4CRLP    WRTP
296500971204     C***                  Z-ADDV4CRDT    WRDT
296600971204     C***                  CLEARAMGRDT
296700971204     C***                  EXSR CTRRIC
296800971204     C***90                GOTO ENDCS4
296900951214     C*
297000010824     C                   READC     TL44S04                                32
297100951214     C*
297200951214    1C     *IN32         DOWEQ     *OFF
297300960322     C* LEGGO TUTTI I FV CHE MI HANNO SCELTO E CARICO L'ALTRA VIDEATA
297400960322    2C     V4CSCE        IFEQ      'M'
297500960322     C                   MOVEL     'S'           WRICH             1
297600960325     C                   MOVEL     *IN99         SAV99             1
297700960322     C                   EXSR      GESTRN
297800960322     C                   SETON                                        32
297900960322     C*
298000960322     C* SE POI ROLLERO' MI DOVRO' RIPOSIZIONARE
298100960322     C                   MOVEL     'S'           WPOSIZ
298200960322    2C                   ENDIF
298300960322     C*
298400010824     C  N32              READC     TL44S04                                32
298500951214    1C                   ENDDO
298600960322     C                   Z-ADD     19            WRIPAG
298700960325     C                   CLEAR                   WRICH
298800960322     C                   CLEAR                   WTRN
298900960322     C                   CLEAR                   AMGTRD
299000960322     C                   CLEAR                   V1CTRD
299100960322     C                   CLEAR                   AMGTRA
299200960325     C                   MOVEL     AMGTTD        KDTT
299300960325     C                   MOVEL     V1CLNP        KLNP
299400960325     C                   MOVEL     V1CNFV        KNFV
299500960325     C                   MOVEL     SAV99         *IN99
299600960325     C* DELETO I RECORD DEL SFL2
299700960322     C                   Z-ADD     2             KSFL
299800960322     C                   EXSR      DELTSW
299900960322     C*
300000960322     C                   Z-ADD     4             KSFL
300100960325     C*
300200960325     C* SE E' STATO PREMUTO F6 DI AGGIORNAMENTO RICARICO SFL4
300300960325     C     *INKF         IFEQ      *ON
300400960325     C                   MOVEL     WNRR4         W0050
300500960325     C* PULISCO IL SFL
300600960325     C                   EXSR      PULSFL
300700960325     C* PULISCO TNTSW
300800960325     C                   EXSR      DELTSW
300900960322     C*
301000960325     C* CARICO FIN TANTO CHE NON NE HO CARICATI COME PRIMA
301100960325     C                   EXSR      SETSR4
301200960325     C*
301300960325     C                   CLEAR                   WNRR4
301400960325     C     WNRR4         DOWLT     W0050
301500960325     C                   Z-ADD     0             L
301600960325     C                   EXSR      LEGS04
301700040422     c* se non ho caricato niente esco dal ciclo
301800040422     c     wnrr4         ifeq      0
301900040422     c                   leave
302000040422     c                   endif
302100960325     C                   ENDDO
302200960325     C                   Z-ADD     SAVNR4        V4CREC
302300960325     C                   ENDIF
302400960325     C*
302500951213     C     ENDCS4        ENDSR
302600951114     C**
302700951114     C********** PREPARA MESSAGGIO DI FV GIA' ABBINATO ****************
302800951114     C     PREMSG        BEGSR
302900951114     C                   MOVEA     MSG(30)       K78
303000951114     C                   MOVEA     W007A         SKI(9)                         NUM  TRAINO
303100951114     C                   Z-ADD     9             I
303200951114     C                   EXSR      ABBL
303300951114     C                   MOVEA     SKI(9)        K78(50)
303400951114     C*
303500951117     C                   MOVEL     W008A         WDATA                          DATA TRAINO
303600951114     C                   MOVEA     WGG           K78(62)
303700951114     C                   MOVEA     WMM           K78(65)
303800951114     C                   MOVEA     WAA           K78(68)
303900951114     C*
304000951214     C                   MOVEA     CNFV          SKI(11)                        NUM  FV
304100951114     C                   Z-ADD     11            I
304200951114     C                   EXSR      ABBL
304300951114     C                   MOVEA     SKI(11)       K78(16)
304400951114     C*
304500951214     C                   MOVEA     K78           VSCMSG
304600951114     C                   ENDSR
304700951106     C**
304800951106     C********** AGGIORNAMENTO FILE TNTSW02L **************************
304900951106     C     UPTSW         BEGSR
305000951106     C* SBINAMENTO FV
305100960221    1C     WONFV         IFGT      0
305200960213     C                   MOVEL     'S'           KWTRC
305300951106     C                   Z-ADD     WONFV         KNFV
305400951106     C     KTSW2         SETLL     TNTSW02L                               31
305500951106     C* C'E' GIA' NON FACCIO NULLA
305600951106    2C     *IN31         IFEQ      *OFF
305700951106     C*
305800960213     C                   MOVEL     'A'           KWTRC
305900951106     C     KTSW2         CHAIN     TNTSW02L                           31
306000951106    3C     *IN31         IFEQ      *OFF
306100951106     C                   MOVEL     'S'           TSWTRC
306200960221     C                   Z-ADD     KSFL          TSWSFL
306300960222     C                   MOVE      WANFP         TSWNFP
306400951114     C                   CLEAR                   TSWTRN
306500951114     C                   CLEAR                   TSWDTN
306600951106     C                   UPDATE    TNTSW002
306700951106   X3C                   ELSE
306800951106     C*
306900951106     C                   CLEAR                   TNTSW002
307000951107     C                   MOVEL     KNMUS         TSWNUS
307100951107     C                   MOVEL     KNRJO         TSWNJO
307200951107     C                   MOVEL     KNMEB         TSWNMB
307300951106     C                   MOVEL     'S'           TSWTRC
307400951106     C                   Z-ADD     WONFV         TSWNFV
307500951107     C                   MOVE      KLNP          TSWFFP
307600960222     C                   MOVE      WANFP         TSWNFP
307700951106     C                   Z-ADD     KSFL          TSWSFL
307800951106     C                   WRITE     TNTSW002
307900951106    3C                   ENDIF
308000951106    2C                   ENDIF
308100960221    1C                   ENDIF
308200960213     C*
308300951106     C* ABBINAMENTO FV
308400960221    1C     WNNFV         IFGT      0
308500960221     C                   MOVEL     'A'           KWTRC
308600951117     C                   Z-ADD     WNNFV         KNFV
308700960221     C     KTSW2         SETLL     TNTSW02L                               31
308800951117     C*
308900960221    2C     *IN31         IFEQ      *OFF
309000960221     C*
309100960221     C                   MOVEL     'S'           KWTRC
309200951106     C     KTSW2         CHAIN     TNTSW02L                           31
309300960221    3C     *IN31         IFEQ      *OFF
309400951106     C                   MOVEL     'A'           TSWTRC
309500951114     C                   Z-ADD     WATRN         TSWTRN
309600951114     C                   Z-ADD     WADTN         TSWDTN
309700960221     C                   Z-ADD     WNNFV         TSWNFV
309800960222     C                   MOVE      WANFP         TSWNFP
309900960221     C                   Z-ADD     KSFL          TSWSFL
310000951106     C                   UPDATE    TNTSW002
310100951106     C*
310200960221   X3C                   ELSE
310300951106     C                   CLEAR                   TNTSW002
310400951107     C                   MOVEL     KNMUS         TSWNUS
310500951107     C                   MOVEL     KNRJO         TSWNJO
310600951107     C                   MOVEL     KNMEB         TSWNMB
310700951106     C                   MOVEL     'A'           TSWTRC
310800951106     C                   Z-ADD     WNNFV         TSWNFV
310900951107     C                   MOVE      KLNP          TSWFFP
311000951106     C                   Z-ADD     KSFL          TSWSFL
311100960222     C                   MOVE      WANFP         TSWNFP
311200951114     C                   Z-ADD     WATRN         TSWTRN
311300951114     C                   Z-ADD     WADTN         TSWDTN
311400951106     C                   WRITE     TNTSW002
311500960221    3C                   ENDIF
311600960221    2C                   ENDIF
311700951106    1C                   ENDIF
311800951106     C*
311900951106     C                   ENDSR
312000951124     C**
312100951124     C*********** RICERCO DECORRENZA VALIDA ***************************
312200951124     C     RICDEC        BEGSR
312300951124     C                   CLEAR                   KDDE
312400951124     C                   CLEAR                   TLTDDE
312500951124     C* MI POSIZIONO CON NUMERO TRAINO E DATA TRAINO
312600951124     C*   PER CARICARE L'ANAGRAFICA DECORRENTE
312700951214     C     KVTRN         SETLL     TNTLT01L
312800951214     C     KVTRN         READE     TNTLT01L                               31
312900951124     C     *IN31         DOWEQ     *OFF
313000951124     C* SE NON E' ANNULLATO E RIENTRA NELLA SCADENZA -->MEMORIZZO
313100951124     C     TLTATB        IFEQ      ' '
313200951214     C     TLTDDE        ANDLE     KVDTN
313300951214     C     TLTDSC        ANDGE     KVDTN
313400951124     C                   Z-ADD     TLTDDE        KDDE
313500951124     C                   SETON                                        31
313600951124     C                   ENDIF
313700951124     C*
313800951214     C  N31KVTRN         READE     TNTLT01L                               31
313900951124     C                   ENDDO
314000951124     C                   ENDSR
314100951108     C**
314200951108     C********** CERCO IL NUOVO IMPORTO LISTINO ***********************
314300951108     C     RICLIS        BEGSR
314400951108     C                   CLEAR                   WILI
314500951115     C                   CLEAR                   WCAU
314600951214     C                   Z-ADD     V2HTRN        KVTRN
314700951214     C                   Z-ADD     V2HDTN        KVDTN
314800951124     C                   EXSR      RICDEC
314900960212     C* PER CERCARE L'IMPORTO 4 TENTATIVI:
315000951108     C     KDDE          IFGT      0
315100960212     C*  1) CON CODICI SPECIFICI
315200960212     C                   MOVEL     V2HTMZ        KTMZ
315300951108     C                   MOVEL     V2CPDR        W0070
315400960212     C                   EXSR      REDTLL
315500960212     C*  2) CON TRAZIONISTA SPECIFICO E TIPO AUTOMEZZO = 99
315600960212    1C     *IN31         IFEQ      *ON
315700960212     C                   MOVEL     '99'          KTMZ
315800960212     C                   EXSR      REDTLL
315900960212    1C                   ENDIF
316000960212     C*  3) CON TRAZIONISTA = 9999999 E TIPO AUTOMEZZO SPECIFICO
316100960212    1C     *IN31         IFEQ      *ON
316200960212     C                   CLEAR                   KTMZ
316300960212     C                   MOVEL     V2HTMZ        KTMZ
316400960212     C                   MOVEL     9999999       W0070
316500960212     C                   EXSR      REDTLL
316600960212    1C                   ENDIF
316700960212     C*  4) CON TRAZIONISTA = 9999999 E TIPO AUTOMEZZO = 99
316800960212    1C     *IN31         IFEQ      *ON
316900960212     C                   MOVEL     '99'          KTMZ
317000960212     C                   MOVEL     9999999       W0070
317100960212     C                   EXSR      REDTLL
317200960212    1C                   ENDIF
317300960212     C*  31 OFF - TROVATO
317400010911     c                   if        *in31 = *off
317500010911      * devo convertire il listino nella divisa della griglia
317600010911     c                   eval      w_importo = tllili
317700011024     c                   if        tlldiv <> tswdiv and tllili <> 1
317800011024     c                             and tllili <> 0
317900010911     c                   eval      w_divisa = tlldiv
318000010911     c                   exsr      Sr_Converti
318100010911     c                   endif
318200010911     c                   z-add     w_importo     wili
318300010911     C**N31              Z-ADD     TLLILI        WILI
318400010911     C**N31              MOVEL     TLLCAU        WCAU
318500010911     C                   MOVEL     TLLCAU        WCAU
318600010911     c                   endif
318700951108     C                   ENDIF
318800960212     C**
318900951116     C                   SETON                                        1790
319000951108     C                   ENDSR
319100951102     C**
319200960212     C********* LETTURA FILE LISTINI **********************************
319300960212     C     REDTLL        BEGSR
319400010824     C     KTLL1         SETLL     TITLL01L
319500010824     C     KTLL1         READE     TITLL01L                               31
319600960212    2C     *IN31         DOWEQ     *OFF
319700960212     C     TLLATB        ANDNE     ' '
319800010824     C     KTLL1         READE     TITLL01L                               31
319900960212    2C                   ENDDO
320000960212     C                   ENDSR
320100951113     C********* LETTURA PER CARICAMENTO SFL02 *************************
320200951102     C     LEGS02        BEGSR
320300020315
320400020315     c                   clear                   w_tsrtrn
320500041001     c                   clear                   w_tsrdtn
320600020315
320700951113     C* SE E' PREVISTO, LEGGO L'ULTIMO RECORD SCRITTO PER
320800951113     C* POSIZIONARMI DA LI' IN POI
320900951113    1C     WPOSIZ        IFEQ      'S'
321000951113     C                   CLEAR                   KNFP
321100951113     C                   CLEAR                   WPOSIZ
321200951113     C*
321300951115     C                   Z-ADD     WNRR2         NRR2
321400010824     C     NRR2          CHAIN     TL44S02                            30
321500951113    2C     *IN30         IFEQ      *OFF
321600951214     C                   Z-ADD     V2HDTN        KDTN
321700951113     C                   Z-ADD     V2HTRN        KTRN
321800010824     C                   Z-ADD     V2hTFP        KTFP
321900951113    3C     V2HTRC        IFEQ      'R'
322000951113     C                   Z-ADD     V2HNFP        KNFP
322100951113    3C                   ENDIF
322200951113     C   04              Z-ADD     WPDR          KPDR
322300951113     C* POSIZIONAMENTO E LETTURA DELL'ULTIMO USATO
322400951113     C                   EXSR      SETST2                                       SETLL
322500951113     C                   EXSR      LEGST2                                       READ
322600951113     C*
322700951113   X2C                   ELSE
322800951214     C                   EXSR      PULSFL
322900951113    2C                   ENDIF
323000951113    1C                   ENDIF
323100951113     C*
323200951113     C                   SETOFF                                       9930
323300951103     C* 99 ON -FINE LETTURA TOTALE
323400951103    1C     *IN99         DOWEQ     *OFF
323500951214     C     L             ANDLT     WRIPAG
323600951113     C*
323700951113     C     WLCTE         IFEQ      'S'
323800951113     C                   EXSR      LEGST2                                       READ
323900951113     C                   ENDIF
324000951103     C*
324100951103    2C     *IN30         DOWEQ     *OFF
324200951103     C     TSTDTN        ANDLE     AMGTRA
324300951214     C     L             ANDLT     WRIPAG
324400951106     C*
324500951106     C* LEGGO LE TRATTE DI QUESTO TRAINO
324600951106     C* CARICO RIGA TESTATA
324700951113     C     WLCTE         IFEQ      'S'
324800951103     C                   EXSR      CTST2
324900951113     C                   CLEAR                   WLCTE
325000951113     C                   ENDIF
325100951116     C*
325200951116     C* SE E' LA 19 LETTURA --> VADO A FINE FILE
325300951214   2AC     L             IFLT      WRIPAG
325400951116     C*
325500951113     C                   MOVEL     TSTTRN        KTRN
325600951113     C                   MOVEL     TSTDTN        KDTN
325700951214     C                   Z-ADD     999           WLNFP
325800951113     C*
325900951113     C* POSIZIONAMENTO : SENZA NUMERO FERMATA
326000951113     C     KNFP          IFEQ      0
326100951214     C     KTS1          SETLL     TNTSR01L
326200951106     C                   ELSE
326300951113     C*                  CON   NUMERO FERMATA
326400951106     C     KTSR11        SETGT     TNTSR01L
326500951106     C                   ENDIF
326600951106     C*
326700980121     C     KTS1          READE(N)  TNTSR01L                               33
326800951103    3C     *IN33         DOWEQ     *OFF
326900951214     C     L             ANDLT     WRIPAG
327000951215     C* CARICO RIGA TRATTE  SOLO SE FVT  CON FV <> DAL PRECEDENTE
327100951214     C     TSRNFP        IFNE      WLNFP
327200951103     C                   EXSR      CTSR2
327300951214     C                   MOVEL     TSRNFP        WLNFP
327400951213     C                   ENDIF
327500951103     C*
327600980121     C     KTS1          READE(N)  TNTSR01L                               33
327700951103    3C                   ENDDO
327800100629     c
327900951113     C* FINE CARICAMENTO SFL
328000951214    3C     L             IFEQ      WRIPAG
328100100629     c                   if        not *in33
328200100629     c* Se la fermata è = a quella appena memorizzata --> lascio
328300100629     c*    altrimenti memorizzo fermata-1 per rileggere questa e non saltarla
328400100629     c                   if        tsrnfp<>wlnfp  and tsrnfp>1
328500100629     c     tsrnfp        sub       1             kNFP
328600100629     c                   else
328700100629     C                   Z-ADD     TSRNFP        KNFP
328800100629     c                   endif
328900100629     c
329000100629     C                   CLEAR                   WLCTE
329100100629     c                   eval      w_tsrtrn = tsrtrn
329200100629     c                   eval      w_tsrdtn = tsrdtn
329300100629     c                   else
329400100629     C                   CLEAR                   KNFP
329500100629     C                   MOVEL     'S'           WLCTE
329600100629     c                   endif
329700951113     C*
329800951116   X3C                   ELSE
329900951113     C                   CLEAR                   KNFP
330000951113     C                   MOVEL     'S'           WLCTE             1
330100951106     C                   EXSR      LEGST2
330200951116    3C                   ENDIF
330300951103     C*
330400951116   2AC                   ENDIF
330500951103    2C                   ENDDO
330600951103     C*
330700951106     C* SE FINE LETTURA --> SE PREVISTO CERCO NUOVO TERMINAL
330800951103    2C     *IN30         IFEQ      *ON
330900951103    3C     *IN02         IFEQ      *OFF
331000960322     C     *IN03         ANDEQ     *OFF
331100951103     C*
331200951103     C     KTFP          SETGT     TNTST04L
331300951103     C                   READ      TNTST04L                               31
331400951103    4C     *IN31         IFEQ      *OFF
331500951103     C                   Z-ADD     TSTTFP        KTFP
331600951103     C     KTST4         SETLL     TNTST04L
331700951103   X4C                   ELSE
331800951103     C                   SETON                                            99
331900951103    4C                   ENDIF
332000951103     C*
332100951103   X3C                   ELSE
332200951103     C                   SETON                                        99
332300951103    3C                   ENDIF
332400951103    2C                   ENDIF
332500951103     C*
332600951103     C  N30TSTDTN        IFGT      AMGTRA
332700951103     C                   SETON                                        99
332800951103     C                   ENDIF
332900951103     C*
333000951103     C                   ENDDO
333100020408
333200020408
333300951102     C*
333400951103     C                   Z-ADD     NRR2          V2CREC
333500951103     C                   Z-ADD     NRR2          WNRR2                          TOTALE REC
333600951102     C                   ENDSR
333700951212     C**
333800951212     C********* LETTURA PER CARICAMENTO SFL04 *************************
333900951212     C     LEGS04        BEGSR
334000960322     C     AMGTTA        IFEQ      0
334100960322     C                   Z-ADD     99999999      AMGTTA
334200960322     C                   ENDIF
334300960322     C*
334400951212     C* SE E' PREVISTO, LEGGO L'ULTIMO RECORD SCRITTO PER
334500951212     C* POSIZIONARMI DA LI' IN POI
334600951212    1C     WPOSIZ        IFEQ      'S'
334700951212     C                   CLEAR                   WPOSIZ
334800951212     C*
334900951212     C                   Z-ADD     WNRR4         NRR4
335000010824     C     NRR4          CHAIN     TL44S04                            30
335100951212    2C     *IN30         IFEQ      *OFF
335200951212     C                   Z-ADD     V4HDTT        KDTT
335300951212     C                   Z-ADD     V4CFFP        KLNP
335400951212     C                   Z-ADD     V4HDTN        KDTN
335500951212     C                   Z-ADD     V4CTRN        KTRN
335600951212     C* POSIZIONAMENTO E LETTURA DELL'ULTIMO USATO
335700951212     C     KTSR21        SETLL     TNTSR02L
335800951212     C                   EXSR      LEGSR4                                       READ
335900951212     C*
336000951212   X2C                   ELSE
336100951214     C                   EXSR      PULSFL
336200951212    2C                   ENDIF
336300951212    1C                   ENDIF
336400951212     C*
336500951212     C                   SETOFF                                       9930
336600951212     C* 99 ON -FINE LETTURA TOTALE
336700951212    1C     *IN99         DOWEQ     *OFF
336800951212     C     L             ANDLT     WRIPAG
336900951212     C*
337000951212     C                   EXSR      LEGSR4
337100951212     C*
337200951212    2C     *IN30         DOWEQ     *OFF
337300951213     C     TSRDTT        ANDLE     AMGTTA
337400951212     C     L             ANDLT     WRIPAG
337500951212     C*
337600951214     C     WLNFP         IFNE      TSRNFP
337700951214     C     WLTRN         ORNE      TSRTRN
337800951214     C     WLDTN         ORNE      TSRDTN
337900951212     C                   EXSR      CTSR4
338000951214     C                   MOVEL     TSRNFP        WLNFP
338100951214     C                   MOVEL     TSRTRN        WLTRN
338200951214     C                   MOVEL     TSRDTN        WLDTN
338300951214     C                   ENDIF
338400951212     C*
338500951213     C                   EXSR      LEGSR4
338600951212    2C                   ENDDO
338700951212     C*
338800951212     C* SE FINE LETTURA --> SE PREVISTO CERCO NUOVA LNP  FV
338900951212    2C     *IN30         IFEQ      *ON
339000951212    3C     *IN06         IFEQ      *OFF
339100951212     C*
339200951212     C     KLNP          SETGT     TNTSR02L
339300951212     C                   READ      TNTSR02L                               31
339400951212    4C     *IN31         IFEQ      *OFF
339500951212     C                   Z-ADD     TSRFFP        KLNP
339600951212     C     KTSR2         SETLL     TNTSR02L
339700951212   X4C                   ELSE
339800951212     C                   SETON                                            99
339900951212    4C                   ENDIF
340000951212     C*
340100951212   X3C                   ELSE
340200951212     C                   SETON                                        99
340300951212    3C                   ENDIF
340400951212    2C                   ENDIF
340500951212     C*
340600951212     C  N30TSRDTT        IFGT      AMGTTA
340700951212     C                   SETON                                        99
340800951212     C                   ENDIF
340900951212     C*
341000951212     C                   ENDDO
341100951212     C*
341200951212     C                   Z-ADD     NRR4          V4CREC
341300951212     C                   Z-ADD     NRR4          WNRR4                          TOTALE REC
341400960322     C*
341500960322     C     AMGTTA        IFEQ      99999999
341600960322     C                   CLEAR                   AMGTTA
341700960322     C                   ENDIF
341800951212     C                   ENDSR
341900951103     C**
342000951113     C******** POSIZIONAMENTO TESTATA TNTST PER SFL02 *****************
342100951113     C     SETST2        BEGSR
342200951113    1C                   SELECT
342300951113     C     WTRN          WHENGT    0                                            NUM. TRAINO
342400960322     C                   MOVEL     WTRN          KTRN
342500951214     C     KTS1          SETLL     TNTST01L
342600951113     C*
342700951113     C     *IN04         WHENEQ    *ON                                          TRAZIONISTA
342800951113     C     KTST2         SETLL     TNTST02L
342900951113     C                   OTHER
343000951113     C*
343100951113     C* SE IMMESSO TERMINAL PARTENZA MI POSIZIONO
343200951113    2C     KTFP          IFEQ      0                                            TER.PARTENZA
343300951113     C     KTFP          SETGT     TNTST04L
343400951113     C                   READ      TNTST04L                               31
343500951113     C  N31              Z-ADD     TSTTFP        KTFP
343600951113     C   31              Z-ADD     999           KTFP
343700951113    2C                   ENDIF
343800951113     C*
343900951113     C     KTRN          IFEQ      0
344000951113     C     KTST4         SETLL     TNTST04L
344100951113     C                   ELSE
344200951113     C     KTST41        SETLL     TNTST04L
344300951113     C                   ENDIF
344400951113    1C                   ENDSL
344500951113     C                   ENDSR
344600951113     C**
344700951212     C******** POSIZIONAMENTO RIGHE TRATTE PER SFL04 ******************
344800951212     C     SETSR4        BEGSR
344900951212     C     V1CNFV        IFGT      0                                            NUM. FV
345000951213     C                   Z-ADD     V1CNFV        KNFV
345100951213     C                   Z-ADD     V1CLNP        KLNP
345200951212     C     KTSR4         SETLL     TNTSR04L
345300951212     C*
345400951212     C                   ELSE
345500951212     C*
345600951212     C* SE IMMESSO TERMINAL PARTENZA MI POSIZIONO
345700951212    2C     KLNP          IFEQ      0                                            LIN.PARTENZA
345800951212     C     KLNP          SETGT     TNTSR02L
345900951214     C                   READ      TNTSR02L                               31
346000951212     C  N31              Z-ADD     TSRFFP        KLNP
346100951212     C   31              Z-ADD     999           KLNP
346200951212    2C                   ENDIF
346300951212     C*
346400951212     C     KTSR2         SETLL     TNTSR02L
346500951212    1C                   ENDIF
346600951212     C                   ENDSR
346700951212     C**
346800951103     C**********LETTURA TESTATA TNTST PER SFL02 ***********************
346900951103     C     LEGST2        BEGSR
347000951113     C*
347100951113     C                   SETOFF                                       30
347200951113     C                   CLEAR                   WSEL
347300011012     C                   eval      flggei = '0'
347400951113    0C     WSEL          DOWEQ     ' '
347500951113     C     *IN30         ANDEQ     *OFF
347600951103     C* NUMERO TRAINO
347700951113    1C                   SELECT
347800951113     C     WTRN          WHENGT    0                                            NUM TRAINO
347900951113     C                   Z-ADD     WTRN          KTRN
348000980121     C**N09      KTS1      READETNTST01L                 30
348100980121     C** 09      KTRN      READETNTST01L                 30
348200980121     C  N09KTS1          READE(N)  TNTST01L                               30
348300980121     C   09KTRN          READE(N)  TNTST01L                               30
348400951113     C*
348500951113     C     *IN04         WHENEQ    *ON
348600951113     C                   Z-ADD     WPDR          KPDR
348700951113     C  N09KTST2         READE     TNTST02L                               30
348800951113     C   09KPDR          READE     TNTST02L                               30
348900951113     C*
349000951113   X1C                   OTHER                                                  TER.PARTENZA
349100951113    2C     *IN10         IFEQ      *OFF
349200951103     C     *IN09         ANDEQ     *ON
349300951103     C     KTFP          READE     TNTST04L                               30
349400951113   X2C                   ELSE
349500951103     C     KTST4         READE     TNTST04L                               30
349600951113    2C                   ENDIF
349700951113    1C                   ENDSL
349800011012      * aggancio solo una volta la tabella GEI
349900011012      * (solo una volta xchè la divisa è unica per tutta la griglia)
350000011012     C                   if        flggei = '0'
350100011012     C                   clear                   dgei
350200011012     C                   clear                   tibs02ds
350300011012     C                   movel     'L'           t02tla
350400011012     C                   movel     'C'           t02mod
350500011012     C                   movel     knsif         t02sif
350600011012     C                   movel     'GEI'         t02cod
350700011012     C                   movel     tstdiv        t02ke1
350800011012     C                   call      'TIBS02R'
350900011012     C                   parm                    kpjba
351000011012     C                   parm                    tibs02ds
351100011012     C                   movel     t02uni        dgei
351200011012     C                   eval      flggei = '1'
351300011012     C                   endif
351400951113     C* CONTROLLO LE SELEZIONI --> SE NON RIENTRA RILEGGO
351500960212    1C  N30*IN04         IFEQ      *OFF
351600951113     C     TSTPDR        OREQ      WPDR
351700951113     C*
351800960212    2C     *IN03         IFEQ      *OFF
351900951113     C     TSTTRN        OREQ      WTRN
352000951113     C*
352100960212    3C     *IN02         IFEQ      *OFF
352200951113     C     TSTTFP        OREQ      V1CTFP
352300951215     C*
352400960212    4C     V1CVIM        IFEQ      'C'
352500951215     C     TSTCNF        ANDNE     ' '
352600960213    5C     V1CVIM        OREQ      ' '
352700951113     C                   MOVEL     'S'           WSEL              1
352800960212   X4C                   ELSE
352900960212     C*
353000960212     C* SE HO RICHIESTO GLI IMPORTO DA CONFERMARE O TUTTI
353100960212     C*  DO SOLO QUELLI GIA' CONTROLLATI
353200960213     C     V1CVIM        IFEQ      'N'
353300960212     C     TSTCNF        ANDEQ     ' '
353400960212     C* CHAIN SULLA FERMATA
353500960212     C                   Z-ADD     TSTTRN        KVTRN
353600960212     C                   Z-ADD     TSTDTN        KVDTN
353700960212     C                   Z-ADD     TSTNFE        KVNFP
353800980121     C*****      KTSR12    CHAINTNTSR01L             31
353900980121     C     KTSR12        CHAIN(N)  TNTSR01L                           31
354000960229    6C  N31TSRFKI        IFNE      ' '
354100960212     C                   MOVEL     'S'           WSEL              1
354200960212    6C                   ENDIF
354300960212    5C                   ENDIF
354400960212    4C                   ENDIF
354500960212    3C                   ENDIF
354600960212    2C                   ENDIF
354700960212    1C                   ENDIF
354800960212    0C                   ENDDO
354900951106     C*
355000951103     C                   ENDSR
355100951213     C**
355200951213     C**********LETTURA TRATTE TNTSR PER SFL04 ************************
355300951213     C     LEGSR4        BEGSR
355400951213     C*
355500951213     C                   SETOFF                                       30
355600951213     C* NUMERO FV
355700951213    1C     V1CNFV        IFGT      0                                            NUM FV
355800951213     C                   Z-ADD     V1CNFV        KNFV
355900980121     C*****      KTSR4     READETNTSR04L                 30
356000980121     C     KTSR4         READE(N)  TNTSR04L                               30
356100951213     C*
356200951213   X1C                   ELSE                                                   TER.PARTENZA
356300960322    2C     *IN09         IFEQ      *ON
356400951213     C     KLNP          READE     TNTSR02L                               30
356500951213   X2C                   ELSE
356600951213     C     KTSR2         READE     TNTSR02L                               30
356700951213    2C                   ENDIF
356800951213    1C                   ENDIF
356900951213     C*
357000951213     C                   ENDSR
357100951102     C**
357200951102     C********** CONTROLLA LE DATE RICHIESTA CON LA DATA AREA *********
357300951102     C     CTRDAT        BEGSR
357400951102     C*
357500951103     C     WDTAL         IFEQ      0
357600951103     C                   Z-ADD     W0080         WDTAL
357700951102     C                   ENDIF
357800951103     C*
357900951108     C                   Z-ADD     WTOTGG        W0050
358000951102     C*
358100951103    1C     W0080         DOWLE     WDTAL
358200951102     C     *IN90         ANDEQ     *OFF
358300951102     C*
358400951102    2C     W0080         IFGT      D30DAT
358500951102     C                   MOVEL     MSG(16)       V1CMSG
358600951102     C                   MOVEA     MSG(16)       K78
358700951102     C                   MOVEL     W0080         WDATA
358800951102     C                   MOVEA     WGG           K78(65)
358900951102     C                   MOVEA     WMM           K78(68)
359000951102     C                   MOVEA     WAA           K78(71)
359100951106     C                   MOVEA     K78           V1CMSG
359200960322     C                   SETON                                        9028
359300951102   X2C                   ELSE
359400951102     C*
359500951108     C                   ADD       1             W0050             5 0
359600951108     C                   Z-ADD     W0050         GIOTGI
359700951102     C                   CALL      'XSRGI8'
359800951102     C                   PARM                    WGIDAT
359900951102     C*
360000951102     C                   Z-ADD     GIOINV        W0080
360100951102    1C                   ENDIF
360200951102     C                   ENDDO
360300951102     C*
360400951102     C                   ENDSR
360500951103     C**
360600951214     C**************** PULIZIA  SFL02 E SFL04 *************************
360700951214     C     PULSFL        BEGSR
360800951103     C                   SETOFF                                       21
360900951214     C*
361000951214     C     KSFL          IFEQ      2
361100010824     C                   WRITE     TL44C02
361200951113     C                   CLEAR                   NRR2
361300951113     C                   CLEAR                   V2CREC
361400951113     C                   CLEAR                   KNFP
361500960322     C                   CLEAR                   WLNFP
361600951116     C                   MOVEL     'S'           WLCTE
361700951214     C                   ELSE
361800010824     C                   WRITE     TL44C04
361900951214     C                   CLEAR                   NRR4
362000951214     C                   CLEAR                   V4CREC
362100960322     C                   CLEAR                   WLNFP
362200960322     C                   CLEAR                   WLTRN
362300960322     C                   CLEAR                   WLDTN
362400951214     C                   ENDIF
362500951214     C*
362600951214     C                   SETON                                        21
362700951212     C                   ENDSR
362800951103     C**
362900951103     C**************** CARICAMENTO TESTATA IN SFL02 *******************
363000951103     C     CTST2         BEGSR
363100951109     C                   CLEAR                   TNTSW000                       file
363200951106     C                   Z-ADD     KSFL          TSWSFL
363300951106     C                   MOVEL     KNMUS         TSWNUS
363400951106     C                   MOVEL     KNRJO         TSWNJO
363500951106     C                   MOVEL     KNMEB         TSWNMB
363600951106     C                   Z-ADD     WF20          TSWF20
363700951109     C                   MOVEL     'T'           TSWTRC
363800951109     C                   Z-ADD     TSTTFP        TSWTFP
363900951109     C                   Z-ADD     TSTTFA        TSWTFA
364000951109     C                   MOVEL     TSTDTN        TSWDTN
364100951109     C                   MOVEL     TSTTRN        TSWTRN
364200951117     C                   MOVEL     TSTFES        TSWFES
364300951124     C                   MOVEL     TSTNFE        TSWNFE
364400010827     C**!!!              MOVEL     TSTILI        TSWILI
364500010827     C**!!!              MOVEL     TSTIPT        TSWIPT
364600010827     C**!!!              MOVEL     TSTIES        TSWIES
364700010827     C                   z-add     TSTILI        TSWILI
364800010827     C                   z-add     TSTIPT        TSWIPT
364900010827     C                   z-add     TSTIES        TSWIES
365000951113     C                   MOVEL     TSTCAU        TSWCAU
365100951113     C                   MOVEL     TSTCAU        TSWVCU
365200010827     C**!!!              MOVEL     TSTICO        TSWICO
365300010827     C**!!!              MOVEL     TSTICO        TSWVIC
365400010827     C                   z-add     TSTICO        TSWICO
365500010827     C                   z-add     TSTICO        TSWVIC
365600951124     C                   MOVEL     TSTCNF        TSWCNF
365700951109     C                   MOVEL     TSTTMZ        TSWTMZ
365800951109     C                   MOVEL     TSTBIS        TSWBIS
365900951109     C                   MOVEL     TSTPDR        TSWPDR
366000951109     C                   MOVEL     TSTPDR        TSWVPD
366100010827
366200010827     c                   movel     tstdiv        tswdiv
366300010827
366400960229     C* SE FATTURATO NON MODIFICO PIU' NULLA
366500951109     C     TSTDFT        IFGT      0
366600951109     C                   MOVEL     'S'           TSWFAT
366700951109     C                   ENDIF
366800960213     C* SE ANNULLATO PROTEGGO TUTTO
366900960213     C                   MOVEL     TSTATB        TSWATB
367000960213     C                   MOVEL     TSTATB        TSWATV
367100960229     C                   MOVEL     TSTATB        TSWATT
367200960229     C**
367300960213     C                   MOVEL     TSTATB        V2CSCE
367400960229     C                   MOVEL     TSTATB        V2HATT
367500960213     C* SE L'IMPORTO NON C'E' VERIFICO SE E' GIA' STATO CONTROLLATO
367600960213     C     TSTICO        IFEQ      0
367700960213     C                   Z-ADD     TSTTRN        KVTRN
367800960213     C                   Z-ADD     TSTDTN        KVDTN
367900960213     C                   Z-ADD     TSTNFE        KVNFP
368000980121     C***        KTSR12    CHAINTNTSR01L             31
368100980121     C     KTSR12        CHAIN(N)  TNTSR01L                           31
368200960213     C* PROTEZIONE IMPORTO DA CONFERMARE
368300960213     C                   CLEAR                   V2HPRP
368400960213     C                   CLEAR                   V2HPRI
368500960229     C  N31TSRFKI        IFEQ      ' '
368600960213     C                   MOVEL     'S'           TSWPRI
368700960213     C                   MOVEL     'S'           V2HPRI
368800960213     C                   ENDIF
368900960213     C*
369000960229     C* PROTEZIONE TRAZIONISTA E CASUALE CONTABILIZZAZIONE
369100960229     C  N31TSRFKP        IFEQ      ' '
369200960213     C                   MOVEL     'S'           TSWPRP
369300960213     C                   MOVEL     'S'           V2HPRP
369400960213     C                   ENDIF
369500960213     C                   ENDIF
369600951109     c*
369700960229     C                   CLEAR                   V2CDTT
369800951107     C                   CLEAR                   V2CFFP
369900951109     C                   CLEAR                   V2CNFV
370000951109     C                   CLEAR                   V2HRFV
370100951109     C                   CLEAR                   V2HNFV
370200951109     C                   CLEAR                   V2HNFP
370300960229     C                   CLEAR                   V2HPRF
370400960229     C                   CLEAR                   V2HTMZ
370500960229     C                   MOVEL     'T'           V2HTRC                         SFL
370600951109     C                   Z-ADD     1             V2HNRC
370700951106     C*
370800010824     C                   Z-ADD     TSTTFP        V2hTFP
370900010824     C                   Z-ADD     TSTTFP        V2cter
371000951113     C                   MOVEL     TSTBIS        V2CBIS
371100010827     C                   movel     tstdiv        V2cdiv
371200951109     C*
371300951214     C                   MOVEL     TSTDTN        V2HDTN
371400951103     C                   MOVEL     TSTDTN        WDATA
371500951109     C                   MOVEA     WMM           K7(6)
371600951109     C                   MOVEA     WGG           K7(3)
371700951109     C                   MOVEL     '/'           K7(5)
371800951109     C                   CLEAR                   V2CDES
371900951109     C                   MOVEA     K7            V2CDES
372000951109     C*
372100951113     C                   MOVEL     TSTTRN        V2HTRN
372200010824     C**!!!              MOVEL     TSTTFA        V2CTFA
372300951108     C                   MOVEL     TSTTMZ        V2HTMZ
372400010827     C**!!!              MOVEL     TSTILI        V2CIMP
372500010827     C**!!!              MOVEL     TSTIPT        V2HIPT
372600010827     C                   z-add     TSTILI        V2CIMP
372700010827     C                   z-add     TSTIPT        V2HIPT
372800951113     C                   MOVEL     TSTCAU        V2CCAU
372900960221     C                   MOVEL     TSTCNF        V2HCNF
373000010827     C**!!!              MOVEL     TSTICO        V2CICO
373100010827     C**!!!              MOVEL     TSTICO        V2HICO
373200010827     C**!!!              MOVEL     TSTICO        WSICO
373300010827     C                   z-add     TSTICO        V2CICO
373400010827     C                   z-add     TSTICO        V2HICO
373500010827     C                   z-add     TSTICO        WSICO
373600951107     C                   MOVEL     'S'           WFORZ
373700951109     C                   MOVEL     TSWFAT        V2HFAT
373800951103     C* DECODIFICA TRAZIONISTA
373900951106     C     TSTPDR        IFGT      0
374000951108     C                   MOVEL     TSTPDR        V2HPDR
374100951106     C                   MOVEL     TSTPDR        V2CPDR
374200000807     C                   Z-ADD     TSTPDR        KTLPDR
374300000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
374400951107     C  N31              MOVEL     TLZRSC        V2DPDR
374500951106     C  N31              MOVEL     TLZRSC        TSWDPD
374600951107     C   31              CLEAR                   V2DPDR
374700951106     C                   ELSE
374800951108     C                   CLEAR                   V2DPDR
374900951108     C                   CLEAR                   V2CPDR
375000951108     C                   CLEAR                   V2HPDR
375100951108     C* VADO SUI FV A PRENDERE LA DESCRIZIONE
375200951108     C     KTSR3         CHAIN     TNTSR03L                           31
375300951108     C     *IN31         IFEQ      *OFF
375400960305     C                   Z-ADD     T03LNP        KLNP
375500960305     C                   Z-ADD     T03NFV        KNFV
375600980121     C***        KTSR4     CHAINTNFVS01L             31
375700980121     C     KTSR4         CHAIN(N)  TNFVS01L                           31
375800951108     C  N31              MOVEL     FVSDPD        V2DPDR
375900951109     C  N31              MOVEL     FVSDPD        TSWDPD
376000951108     C                   ENDIF
376100951106     C                   ENDIF
376200951103     C*
376300951107     C* SISTEMO GLI INDICATORI
376400951107     C                   EXSR      CTRIND
376500951103     C*
376600951103     C                   ADD       1             L
376700951103     C                   ADD       1             NRR2
376800951106     C                   Z-ADD     NRR2          TSWNRR
376900010824     C                   WRITE     TL44S02
377000951106     C                   WRITE     TNTSW000
377100951116     C*
377200951116     C                   EXSR      RICNRR
377300020408      * se è la riga 19
377400020408     c                   if        l = wripag
377500020408     c                   eval      w_tsrtrn = tsttrn
377600041001     c                   eval      w_tsrdtn = tstdtn
377700020408     c                   endif
377800020408
377900951116     C                   ENDSR
378000951116     C**
378100951116     C*********** CERCO NRR DEL RECORD DA VISUALIZZARE PER F19F20******
378200951116     C     RICNRR        BEGSR
378300951116     C* CERCO 3 NUMERI DI RECORD:
378400960221     C* 1) QUELLO CON TRAINO = A QUELLO RICHIESTO
378500951116     C     TSWTRN        IFGT      0
378600951116     C     WNRRA         ANDEQ     0
378700951116     C     TSWTRN        ANDEQ     WCTRN
378800951116     C                   Z-ADD     TSWNRR        WNRRA
378900951116     C                   ENDIF
379000951116     C* 2) QUELLO CON TERMINAL PARTENZA = A QUELLO RICHIESTO
379100951116     C     TSWTFP        IFGT      0
379200951116     C     WNRRB         IFEQ      0
379300951116     C     TSWTFP        ANDEQ     WCTFP
379400951116     C                   Z-ADD     TSWNRR        WNRRB
379500951116     C                   ENDIF
379600951116     C* 3) QUELLO CON TERMINAL PARTENZA SUBITO PRIMA DEL TERMINAL RICH
379700951116     C     TSWTFP        IFLT      WCTFP
379800951116     C     TSWTFP        ANDGT     WSTFP
379900951116     C                   Z-ADD     TSWNRR        WNRRC
380000951116     C                   Z-ADD     TSWTFP        WSTFP
380100951116     C                   ENDIF
380200951116     C                   ENDIF
380300951116     C                   ENDSR
380400951113     C**
380500951113     C*************** ABBLANCA ZERO NON SIGNIFICATIVI *****************
380600951109     C     ABBL          BEGSR
380700951109     C*
380800951109     C     SKI(I)        DOWEQ     '0'
380900951109     C     I             ANDLT     15
381000951109     C                   MOVEL     ' '           SKI(I)
381100951109     C                   ADD       1             I
381200951109     C                   ENDDO
381300951109     C*
381400951109     C                   ENDSR
381500951103     C**
381600951103     C**************** CARICAMENTO TRATTE IN SFL02 ********************
381700951103     C     CTSR2         BEGSR
381800951113     C                   CLEAR                   TNTSW000                       file
381900951113     C                   Z-ADD     KSFL          TSWSFL
382000951113     C                   MOVEL     KNMUS         TSWNUS
382100951113     C                   MOVEL     KNRJO         TSWNJO
382200951113     C                   MOVEL     KNMEB         TSWNMB
382300951113     C                   Z-ADD     WF20          TSWF20
382400960213     C                   MOVEL     TSTATB        TSWATT
382500960213     C                   Z-ADD     TSTTFP        TSWTFP
382600951113     C                   Z-ADD     TSTTFA        TSWTFA
382700951113     C                   MOVEL     TSTDTN        TSWDTN
382800951113     C                   MOVEL     TSTTRN        TSWTRN
382900010827     C**!!!              MOVEL     TSTIPT        TSWIPT
383000010827     C                   z-add     TSTIPT        TSWIPT
383100010827     C                   MOVEL     TSTFES        TSWFES
383200951124     C                   MOVEL     TSTNFE        TSWNFE
383300010827
383400010827     c                   movel     tstdiv        tswdiv
383500010827
383600960229     C* SE FATTURATO NON MODIFICO PIU' NULLA
383700960229     C     TSTDFT        IFGT      0
383800960229     C                   MOVEL     'S'           TSWFAT
383900960229     C                   ENDIF
384000951113     C*
384100951113     C                   MOVEL     'R'           TSWTRC
384200951109     C                   Z-ADD     TSRNFV        TSWNFV
384300951109     C                   Z-ADD     TSRNFV        TSWVNF
384400960229     C                   MOVEL     TSRATB        TSWATB
384500960213     C                   MOVEL     TSRATB        TSWATV
384600951109     C                   MOVEL     TSRDTT        TSWDTT
384700951109     C                   Z-ADD     TSRNFP        TSWNFP
384800951109     C                   Z-ADD     TSRFFP        TSWFFP
384900951109     C*
385000951109     C                   MOVEL     'R'           V2HTRC                         SFL
385100951108     C                   ADD       1             V2HNRC
385200960213     C                   MOVEL     TSRATB        V2CSCE
385300960229     C                   MOVEL     TSTATB        V2HATT                         ANN. TESTATA
385400960229     C                   MOVEL     TSWFAT        V2HFAT
385500960229     C                   MOVEL     TSTDTN        V2HDTN
385600960229     C                   MOVEL     TSTTRN        V2HTRN
385700010827     C**!!!              CLEAR                   V2CTFA
385800010827     C                   CLEAR                   V2Cdiv
385900951109     C                   CLEAR                   V2HIPT
386000951109     C                   CLEAR                   V2CBIS
386100960213     C                   CLEAR                   V2HPRP
386200960213     C                   CLEAR                   V2HPRI
386300960229     C                   CLEAR                   V2HPRF
386400960229     C* I CAMPI V2HFAT, V2HTRN, V2HDTN RIMANGONO IMPOSTATI DALLA
386500960229     C*  LETTURA DI TNTST
386600960229     C**
386700960229     C* SE NON E' STATO CONTROLLATO, PROTEGGO IL CAMPO
386800960229     C*  E NON POSSO NEMMENO GESTIRE L'ANNULLAMENTO
386900960229    1C     TSRFKP        IFEQ      ' '
387000960229     C                   MOVEL     'S'           TSWPRF
387100960229     C                   MOVEL     'S'           V2HPRF
387200960229    1C                   ENDIF
387300951215     C* SOLO NELLA 1 RIGA DI FV  MEMORIZZO L'IMPORTO FV E NUM.TRAINO
387400960213    1C     V2HNRC        IFEQ      2
387500951109     C* RIEMPO IL CAMPO IMPORTO FV
387600951113     C                   Z-ADD     TSTIPT        V2CIMP
387700951109     C* NUMERO TRAINO
387800951113     C                   MOVEL     TSTTRN        W007A             7
387900951109     C                   MOVEA     W007A         SKI(9)
388000951109     C                   Z-ADD     9             I                 3 0
388100951109     C                   EXSR      ABBL
388200951109     C                   MOVEA     SKI(9)        V2CDES
388300010827
388400010827     C                   z-add     tsttfa        V2cter
388500010827
388600951108     C*
388700960213   X1C                   ELSE
388800951108     C                   CLEAR                   V2CIMP
388900951113     C                   CLEAR                   V2CDES
389000960213    1C                   ENDIF
389100960213     C*
389200960213    1C     TSRNFV        IFEQ      0
389300951107     C                   CLEAR                   V2CNFV
389400951107     C                   MOVEL     *ZEROS        V2HNFV
389500951107     C                   ELSE
389600951107     C                   MOVEL     TSRNFV        V2CNFV
389700951107     C                   MOVEL     TSRNFV        V2HNFV
389800960213    1C                   ENDIF
389900951107     C                   MOVEL     TSRDTT        WDATA
390000951107     C                   MOVE      WMM           V2CDTT
390100951107     C                   MOVEL     WGG           V2CDTT
390200951103     C*
390300951107     C                   Z-ADD     TSRNFP        V2HNFP
390400951107     C                   Z-ADD     TSRFFP        V2CFFP
390500951215     C* SE LA DATA FVT E' DIVERSA DALLA DATA FV O NON C'E' FV
390600951103     C* METTO IN RI IL CAMPO DEL FV
390700960213    1C     TSRNFV        IFEQ      0
390800951103     C     TSRDTT        ORNE      TSRDFV
390900951106     C                   MOVEL     'S'           V2HRFV
391000951106     C                   MOVEL     'S'           TSWRFV
391100960213     C                   MOVEL     'S'           TSWRFI
391200951103     C                   ELSE
391300951103     C                   MOVEL     ' '           V2HRFV
391400951106     C                   MOVEL     ' '           TSWRFV
391500960213     C                   MOVEL     ' '           TSWRFI
391600960213    1C                   ENDIF
391700951107     C* SISTEMO GLI INDICATORI
391800951107     C                   EXSR      CTRIND
391900951103     C*
392000951103     C                   ADD       1             L
392100951103     C                   ADD       1             NRR2
392200951106     C                   Z-ADD     NRR2          TSWNRR
392300010824     C                   WRITE     TL44S02
392400951107     C                   WRITE     TNTSW000
392500951103     C                   ENDSR
392600951213     C**
392700951213     C**************** CARICAMENTO TRATTE IN SFL04 ********************
392800951213     C     CTSR4         BEGSR
392900951213     C                   CLEAR                   TNTSW000                       file
393000951213     C                   Z-ADD     KSFL          TSWSFL
393100951213     C                   MOVEL     KNMUS         TSWNUS
393200951213     C                   MOVEL     KNRJO         TSWNJO
393300951213     C                   MOVEL     KNMEB         TSWNMB
393400951213     C                   Z-ADD     WF20          TSWF20
393500951213     C                   Z-ADD     TSRFFP        TSWFFP
393600951213     C                   Z-ADD     TSRNFP        TSWNFP
393700951213     C                   Z-ADD     TSRDTT        TSWDTT
393800951213     C                   MOVEL     TSRDTN        TSWDTN
393900951213     C                   MOVEL     TSRTRN        TSWTRN
394000960322     C                   MOVEL     TSRATB        TSWATB
394100960322     C                   MOVEL     TSRATB        TSWATV
394200951213     C*
394300951213     C                   MOVEL     'R'           TSWTRC
394400951213     C                   Z-ADD     TSRNFV        TSWNFV
394500951213     C                   Z-ADD     TSRNFV        TSWVNF
394600951213     C*
394700960322    1C     TSRNFV        IFEQ      0
394800951213     C                   CLEAR                   V4CNFV
394900951213     C                   MOVEL     *ZEROS        V4HNFV
395000951213     C                   ELSE
395100951213     C                   MOVEL     TSRNFV        V4CNFV
395200951213     C                   MOVEL     TSRNFV        V4HNFV
395300960322    1C                   ENDIF
395400951213     C*
395500960322     C                   CLEAR                   V4CSCE
395600960322     C                   MOVEL     TSRATB        V4HATB
395700960322     C                   MOVEL     TSRTRN        V4CTRN
395800951213     C                   MOVEL     TSRDTN        V4HDTN
395900951213     C                   MOVEL     TSRDTN        WDATA
396000951213     C                   MOVE      WMM           V4CDTN
396100951213     C                   MOVEL     WGG           V4CDTN
396200951213     C*
396300951213     C                   MOVEL     TSRDTT        V4HDTT
396400951213     C                   MOVEL     TSRDTT        WDATA
396500951213     C                   MOVE      WMM           V4CDTT
396600951213     C                   MOVEL     WGG           V4CDTT
396700951213     C*
396800951213     C                   Z-ADD     TSRFFP        V4CFFP
396900951214     C                   Z-ADD     TSRNFP        V4HNFP
397000951215     C* SE LA DATA FVT  E' DIVERSA DALLA DATA FV O NON C'E' FV
397100951213     C* METTO IN RI IL CAMPO DEL FV
397200960322    1C     TSRNFV        IFEQ      0
397300951213     C     TSRDTT        ORNE      TSRDFV
397400951213     C                   MOVEL     'S'           V4HRFV
397500951213     C                   MOVEL     'S'           TSWRFV
397600960322     C                   MOVEL     'S'           TSWRFI
397700960322   X1C                   ELSE
397800951213     C                   MOVEL     ' '           V4HRFV
397900951213     C                   MOVEL     ' '           TSWRFV
398000960322     C                   MOVEL     ' '           TSWRFI
398100960322    1C                   ENDIF
398200960322     C*
398300960322     C* SE NON E' STATO CONTROLLATO PROTEGGO IL CAMPO
398400960322     C*  E NON POSSO NEMMENO ANNULLARLO
398500960322    1C     TSRFKP        IFEQ      ' '
398600960322     C                   MOVEL     'S'           TSWPRF
398700960322     C                   MOVEL     'S'           V4HPRF
398800960322     C                   ELSE
398900960322     C                   CLEAR                   V4HPRF
399000960322    1C                   ENDIF
399100960322     C* DATA PARTENZA TEORICA
399200960322     C                   MOVEL     TSRDTP        WDATA
399300960322     C                   MOVE      WMM           V4CDTP
399400960322     C                   MOVEL     WGG           V4CDTP
399500960322     C                   MOVEL     TSRHTP        V4CHTP
399600951213     C* CHAIN SU TESTATA
399700951213     C                   Z-ADD     TSRTRN        KTRN
399800951213     C                   Z-ADD     TSRDTN        KDTN
399900951214     C     KTS1          CHAIN     TNTST01L                           31
400000960322    1C     *IN31         IFEQ      *OFF
400100951213     C                   MOVEL     TSTTFP        V4CTFP
400200951213     C                   MOVEL     TSTTFA        V4CTFA
400300951213     C                   MOVEL     TSTTFP        TSWTFP
400400951213     C                   MOVEL     TSTTFA        TSWTFA
400500960322     C                   MOVEL     TSTBIS        TSWBIS
400600960322     C                   MOVEL     TSTBIS        V4CBIS
400700960322     C                   MOVEL     TSTATB        TSWATT
400800960322     C                   MOVEL     TSTATB        V4HATT
400900951213     C*
401000951213     C* SE IMPORTO GIA' CONFERMATO NON POSSO FARE PIU' MODOFICARE I CAM
401100960322    2C     TSTDFT        IFGT      0
401200951213     C                   MOVEL     'S'           TSWFAT
401300951213     C                   MOVEL     'S'           V4HFAT
401400960322   X2C                   ELSE
401500951214     C                   CLEAR                   TSWFAT
401600951214     C                   CLEAR                   V4HFAT
401700960322    2C                   ENDIF
401800951213     C*
401900951213     C                   ELSE
402000951213     C                   CLEAR                   V4CTFP
402100960322     C                   CLEAR                   V4CTFA
402200960322     C                   CLEAR                   V4HFAT
402300960322     C                   CLEAR                   V4HATT
402400951213     C                   ENDIF
402500960325     C*
402600951213     C* SISTEMO GLI INDICATORI
402700951213     C                   EXSR      CTRIND
402800951213     C*
402900951213     C                   ADD       1             L
403000951213     C                   ADD       1             NRR4
403100951213     C                   Z-ADD     NRR4          TSWNRR
403200010824     C                   WRITE     TL44S04
403300951213     C                   WRITE     TNTSW000
403400951213     C                   ENDSR
403500951103     C**
403600951103     C**************** VISUALIZZA TUTTI I DATI DEL TRAINO *************
403700951124     C     VISGRL        BEGSR
403800010824     C                   CLEAR                   TL44Z03
403900010824     C                   CLEAR                   TL44C03
404000980114     C* sostituisco la visualizzazione dei dati arrivo del traino con un
404100980114     C* subfile in quanto risulta necessario avere + di 4 righe arrivi
404200010824     C                   CLEAR                   TL44C13
404300951103     C*
404400960624     C                   SETOFF                                       212027
404500980114     C                   SETOFF                                       3738
404600980114     C*****                SETOF                     353637
404700980114     C*****                SETOF                     3839
404800010824     C                   WRITE     TL44C03
404900951107     C                   SETON                                        2123
405000951214     C                   SETON                                        151691
405100951214     C                   SETON                                        9293
405200951106     C                   Z-ADD     0             NRR3              5 0
405300951103     C*
405400951103     C* CARICO DATI --> T E S T A T A   T R A I N O
405500951214     C     KTST11        CHAIN     TNTST01L                           31
405600951106    1C     *IN31         IFEQ      *OFF
405700951103     C                   Z-ADD     TSTTRN        V3CTRN
405800951103     C                   MOVE      TSTDTN        G02INV
405900951103     C                   MOVE      *ZEROS        G02DAT
406000951103     C                   MOVEL     '3'           G02ERR
406100951103     C                   CALL      'XSRDA8'
406200951103     C                   PARM                    WLBDAT
406300951103     C                   Z-ADD     G02DAT        V3CDTN
406400951103     C* BIS
406500951103    2C     TSTBIS        IFEQ      'B'
406600951103     C                   MOVEL     'B  I  S'     V3DBIS
406700980119    2C                   ENDIF
406800960301    2C     TSTBIS        IFEQ      'E'
406900951103     C                   MOVEL     'ECCEZION'    V3DBIS
407000951103     C                   MOVE      'ALE'         V3DBIS
407100951103    2C                   ENDIF
407200960301     C* ANNULLATO
407300980119    2C     TSTATB        IFEQ      'A'
407400960301     C                   SETON                                        20
407500980119    2C                   ENDIF
407600951103     C* TIPO TRAINO
407700951103     C                   MOVEL     TSTTTR        V3CTTR
407800951103     C                   MOVEL     'TV'          COD
407900951103     C                   MOVEL(P)  TSTTTR        KEY
408000951103     C     KTAB          CHAIN     TABEL                              31
408100951103     C  N31              MOVEL     TBLUNI        V3DTTR
408200951103     C   31              MOVEL     *ALL'?'       V3DTTR
408300951103     C*
408400951103     C                   MOVEL     TSTTFP        V3CTFP
408500951103     C                   MOVEL     TSTTFP        KFIL
408600951103     C                   EXSR      CAZORG
408700951103     C                   MOVEL     ORGDES        V3DTFP
408800951103     C*
408900951103     C                   MOVEL     TSTTMZ        V3CTMZ
409000951103     C                   MOVEL     'TU'          COD
409100951103     C                   MOVEL(P)  TSTTMZ        KEY
409200951103     C     KTAB          CHAIN     TABEL                              31
409300951103     C  N31              MOVEL     TBLUNI        V3DTMZ
409400951103     C   31              MOVEL     *ALL'?'       V3DTMZ
409500951103     C*
409600951103     C                   MOVEL     TSTTFA        V3CTFA
409700951103     C                   MOVEL     TSTTFA        KFIL
409800951103     C                   EXSR      CAZORG
409900951103     C                   MOVEL     ORGDES        V3DTFA
410000951103     C*
410100951103     C                   MOVEL     TSTPDR        V3CPDR
410200951103    2C     TSTPDR        IFGT      0
410300000807     C                   Z-ADD     TSTPDR        KTLPDR
410400000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
410500951103     C  N31              MOVEL     TLZRSC        V3DPDR
410600951103     C   31              MOVEL     *ALL'?'       V3DPDR
410700951103     C                   ELSE
410800951103     C                   MOVEL     *ALL'?'       V3DPDR
410900951103    2C                   ENDIF
411000951103     C*
411100010824     c                   eval      v3cdiv = tstdiv
411200951103     C                   Z-ADD     TSTILI        V3CILI
411300951103     C                   Z-ADD     TSTIPT        V3CIPT
411400951103     C                   Z-ADD     TSTICO        V3CICO
411500960222     C                   Z-ADD     TSTFES        V3CFES
411600951113     C* CAUSALE CONTABILIZZAZIONE
411700951113     C                   MOVEL     TSTCAU        V3CCAU
411800980119    2C     TSTCAU        IFEQ      ' '
411900951113     C                   CLEAR                   V3DCAU
412000951113     C                   ELSE
412100000214     C* Reperisco i dati della causale di contabilizzazione
412200000214     C* dalle tabelle di PROJ con apposita Routine
412300000218     C                   MOVE      KNSIF         WCOMO4            4
412400000218     C                   MOVEL     WCOMO4        WSOC              3
412500000218     C                   movel     WSOC          KAtazi
412600000218     C                   MOVEL     'Y4Z'         Katcod
412700000218     C                   MOVEL     *blanks       Katkey
412800000218     C                   MOVEL(P)  V2CCAU        Katkey
412900000218     C     KEATAB        CHAIN     ANATB01L                           30
413000060727     c                   if        not %found(anatb01l)
413100000218     C                   MOVE      '888'         Katazi
413200000218     C     KEATAB        CHAIN     ANATB01L                           30
413300060727     c                   end
413400000218     C                   if        not *in30
413500000218     C                   MOVEL     ATBUNI        V3DCAU
413600000214     C                   else
413700000214     C                   MOVE      *BLANKS       V3DCAU
413800000214     C   31              MOVEL     *ALL'?'       V3DCAU
413900000214     C                   end
414000000214     C                   end
414100951113     C*
414200951103     C                   MOVEL     TSTNFT        V3CNFT
414300951103    2C     TSTDFT        IFGT      0
414400951103     C                   MOVE      TSTDFT        G02INV
414500951103     C                   MOVE      *ZEROS        G02DAT
414600951103     C                   MOVEL     '3'           G02ERR
414700951103     C                   CALL      'XSRDA8'
414800951103     C                   PARM                    WLBDAT
414900951103     C                   Z-ADD     G02DAT        V3CDFT
415000951103    2C                   ENDIF
415100951103     C**
415200951103     C* CARICO DATI SFL --> M E T R I   C U B I
415300951103     C                   CLEAR                   NRR3
415400960222     C                   CLEAR                   X
415500960222     C                   CLEAR                   MCT
415600951214     C     KTST11        SETLL     TNTSM01L
415700960226     C     KTST11        READE(N)  TNTSM01L                               31
415800951109     C* SE NON TROVATO FACCIO UNA CHAIN FITTIZIA
415900980119    2C     *IN31         IFEQ      *ON
416000951109     C                   Z-ADD     1             NRR3
416100010824     C                   WRITE     TL44S03
416200980119    2C                   ENDIF
416300951109     C*
416400951103    2C     *IN31         DOWEQ     *OFF
416500960222     C* CARICO LA SKIERA DEI MC DELLA FILIALE DI CARICO
416600980119    3C     TSMTFP        IFEQ      KVFFP
416700960222     C                   ADD       1             X                 3 0
416800960222     C                   Z-ADD     TSMTFA        MCT(X)
416900960222     C                   MOVEL     TSMTFP        MCT(X)
417000980119    3C                   ENDIF
417100951103     C*
417200951103     C                   Z-ADD     TSMTFP        V3CMTP
417300951103     C                   Z-ADD     TSMTFP        KFIL
417400951103     C*
417500951103     C                   Z-ADD     TSMTFA        V3CMTA
417600951103     C                   Z-ADD     TSMTFA        KFIL
417700951103     C                   EXSR      CAZORG
417800980119    3C     ORGDE5        IFNE      *BLANKS
417900960301     C                   MOVEL     ORGDE5        V3DMTA
418000960301     C                   ELSE
418100951103     C                   MOVEL     ORGDES        V3DMTA
418200980119    3C                   ENDIF
418300951103     C*
418400951103     C                   Z-ADD     TSMTFF        V3CMTF
418500951103     C                   Z-ADD     TSMTFF        KFIL
418600951103     C                   EXSR      CAZORG
418700980119    3C     ORGDE5        IFNE      *BLANKS
418800960301     C                   MOVEL     ORGDE5        V3DMTF
418900960301     C                   ELSE
419000951103     C                   MOVEL     ORGDES        V3DMTF
419100980119    3C                   ENDIF
419200951103     C*
419300960221     C                   Z-ADD     TSMMCT        V3CMCT
419400960710     C                   Z-ADD     TSMMCT        V3HMCT
419500960221     C                   Z-ADD     TSMMCA        V3CMCA
419600960625     C                   Z-ADD     TSMMCA        V3HMCA
419700960424     C                   Z-ADD     TSMMCB        V3CMCB
419800960710     C                   Z-ADD     TSMMCB        V3HMCB
419900960424     C                   Z-ADD     TSMMCR        V3CMCR
420000980119    3C     TSMATB        IFEQ      'A'
420100960301     C                   SETON                                        89
420200960301     C                   ELSE
420300960301     C                   SETOFF                                       89
420400980119    3C                   ENDIF
420500951103     C*
420600951103     C                   ADD       1             NRR3
420700010824     C                   WRITE     TL44S03
420800951103     C*
420900951103     C*
421000960226     C     KTST11        READE(N)  TNTSM01L                               31
421100951103    2C                   ENDDO
421200951103     C**
421300951214     C** CARICO DATI --> T R A T T E     SE RICHIESTO
421400951107    2C     KVNFP         IFGT      0
421500951103     C                   SETON                                        14
421600980114     C*********            Z-ADD0         WCONT   20
421700980114     C* pulisco il subfile
421800980114     C                   SETOFF                                       3536
421900980114     C                   SETOFF                                       606162
422000010824     C                   WRITE     TL44C13
422100980116     C                   SETON                                        616360
422200980114     C                   Z-ADD     0             NR13              5 0
422300980114     C*
422400951214     C     KTSR12        SETLL     TNTSR01L
422500980121     C***        KTSR12    READETNTSR01L                 31
422600980121     C     KTSR12        READE(N)  TNTSR01L                               31
422700960222     C* TRATTA NON TROVATA
422800951103    3C     *IN31         IFEQ      *ON
422900951103     C                   SETOFF                                       14
423000960222     C                   ELSE
423100960222     C* CARICO PRIMA I DATI PARTENZA
423200960222     C                   EXSR      CARPAR
423300960222    3C                   ENDIF
423400951214     C*
423500960222     C* POI I DATI DI ARRIVO
423600951214    3C     *IN31         DOWEQ     *OFF
423700980114     C********   WCONT     ANDLT5
423800960222     C* SE LA FERMATA PARTENZA NON E' QUELLA DEL FV
423900960222     C*  VERIFICO SE CI SONO DEI MC
424000960222     C                   Z-ADD     TSRFFA        W0060             6 0
424100960222     C                   MOVEL     KVFFP         W0060
424200960305     C     W0060         LOOKUP    MCT                                    32
424300960222     C*
424400980119    4C     *IN32         IFEQ      *ON
424500980114     C********             ADD  1         WCONT
424600010824     C                   CLEAR                   TL44S13
424700960222     C                   EXSR      CARARR
424800960305     C*
424900960305     C* TOLGO DALL SKIERA TUTTI I RECORD APPENA TROVATI
425000960305     C                   Z-ADD     1             X
425100980119    5C     *IN32         DOWEQ     *ON
425200960305     C     W0060         LOOKUP    MCT(X)                                 32
425300960305     C   32              CLEAR                   MCT(X)
425400960305     C   32              ADD       1             X
425500980119    5C                   ENDDO
425600980119    4C                   ENDIF
425700960222     C*
425800980121     C****       KTSR13    READETNTSR01L                 31
425900980121     C     KTSR13        READE(N)  TNTSR01L                               31
426000951214    3C                   ENDDO
426100951214   X2C                   ELSE
426200951214     C                   SETOFF                                       14
426300951214    2C                   ENDIF
426400951103     C*
426500951103     C* E M I S S I O N E   V I D E A T E
426600951103     C     FORS03        TAG
426700010824     C                   WRITE     TL44T01
426800010824     C                   WRITE     TL44C03
426900010824     C                   WRITE     TL44C13
427000010824     C                   EXFMT     TL44Z03
427100960625     C                   SETOFF                                       2890
427200960625     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
427300960625     C                   SETOFF                                       525354
427400960625     C                   SETOFF                                       5556
427500960710     C* F7  - MANUTENZIONE MC. E CAUSALI RITARDO (SPROTEGGO CAMPI)
427600970826     C* SE LA DATA E ORA SONO =0 LI IMPOSTO = AI TEORICI
427700980114     C*
427800980119    2C  N37*INKG         IFEQ      *ON
427900980114     C                   SETON                                        27
428000980114     C                   SETON                                        37
428100960625     C                   Z-ADD     NRR3          WNRR3
428200960624     C                   Z-ADD     1             NRR3
428300980119    3C     NRR3          DOWLE     WNRR3
428400010824     C     NRR3          CHAIN     TL44S03                            31
428500980119    4C  N31V3CMTP        IFGT      0
428600010824     C                   UPDATE    TL44S03
428700980119    4C                   END
428800960624     C                   ADD       1             NRR3
428900980119    3C                   ENDDO
429000980114     C****       V3CDRP    COMP 0                    35
429100980114     C****       V3CDT1    COMP 0                    36
429200980114     C****       V3CDT2    COMP 0                    37
429300980114     C****       V3CDT3    COMP 0                    38
429400980114     C****       V3CDT4    COMP 0                    39
429500980114     C* se c'è il 38 acceso allora significa che è attiva la videata degli
429600980114     C* arrivi quindi vado a riproteggere i campi
429700980119    3C     *IN38         IFEQ      *ON
429800980114     C                   SETOFF                                       38
429900980119     C                   Z-ADD     NR13          WNR13
430000980114     C                   Z-ADD     1             NR13
430100980119    4C     NR13          DOWLE     WNR13
430200010824     C     NR13          CHAIN     TL44S13                            31
430300980119    5C     *IN31         IFEQ      *OFF
430400980114     C                   SETOFF                                       36
430500980114     C                   SETON                                        16
430600980121     C     NR13          COMP      1                                  8181
430700010824     C                   UPDATE    TL44S13
430800980119    5C                   END
430900980114     C                   ADD       1             NR13
431000980119    4C                   ENDDO
431100980116     C                   SETOFF                                       363552
431200980120     C                   SETON                                        15
431300980120     C                   SETOFF                                       16
431400980119    3C                   ENDIF
431500980114     C                   GOTO      FORS03
431600980119    2C                   END
431700980114     C*
431800980119    2C  N38*INKN         IFEQ      *ON
431900980114     C                   SETON                                        38
432000980119     C                   Z-ADD     NR13          WNR13
432100980114     C                   Z-ADD     1             NR13
432200980119    3C     NR13          DOWLE     WNR13
432300010824     C     NR13          CHAIN     TL44S13                            31
432400980119    4C  N31V3CDT1        IFGT      0
432500980116     C                   SETON                                        36
432600980119     C* CONTROLLO SE ESISTE ALMENO UNA RIGA DI DESCRIZIONE
432700980119     C                   Z-ADD     V3CFF1        KFIL
432800980119     C                   MOVEL     'R'           KWTRC
432900980119     C                   MOVEL     'A'           KEPA
433000980119     C     KFV4          SETLL     FNFV401L                               OC
433100980119     C   OC              SETOFF                                       16
433200980119     C  NOC              SETON                                        16
433300980121     C     NR13          COMP      1                                  8181
433400010824     C                   UPDATE    TL44S13
433500980119    4C                   END
433600980114     C                   ADD       1             NR13
433700980119    3C                   ENDDO
433800980114     C* se c'è il 37 acceso allora significa che è attiva la videata degli
433900980114     C* arrivi quindi vado a riproteggere i campi
434000980119    3C     *IN37         IFEQ      *ON
434100980114     C                   SETOFF                                       37
434200980114     C                   Z-ADD     NRR3          WNRR3
434300980114     C                   Z-ADD     1             NRR3
434400980119    4C     NRR3          DOWLE     WNRR3
434500010824     C     NRR3          CHAIN     TL44S03                            31
434600980119    5C     *IN31         IFEQ      *OFF
434700980114     C                   SETOFF                                       27
434800010824     C                   UPDATE    TL44S03
434900980119    5C                   END
435000980114     C                   ADD       1             NRR3
435100980119    4C                   ENDDO
435200980119    3C                   END
435300980116     C                   SETON                                        3552
435400980119     C* CONTROLLO SE ESISTE ALMENO UNA RIGA DI DESCRIZIONE
435500980119     C                   CLEAR                   KFIL
435600980119     C                   MOVEL     'R'           KWTRC
435700980119     C                   MOVEL     'P'           KEPA
435800980119     C     KFV4          SETLL     FNFV401L                               87
435900980119     C   87              SETOFF                                       15
436000980119     C  N87              SETON                                        15
436100980114     C                   GOTO      FORS03
436200980119    2C                   END
436300951103     C* F12 - RITORNO
436400951106    2C     *INKL         IFEQ      *OFF
436500980119    3C     *IN38         IFEQ      *ON
436600951109     C*
436700980119     C                   SETOFF                                       58
436800010824     C                   READ      TL44C13                                58
436900960624     C                   CLEAR                   W001A
437000980119    4C     *IN58         IFEQ      *OFF
437100951109     C* VISUALIZZA RITARDI PARTENZA
437200980119    5C     V3CVDP        IFEQ      'V'
437300960624     C                   MOVE      '1'           W001A             1
437400960131     C                   CLEAR                   DSLV15
437500960131     C                   MOVEL     'I'           D15TUP
437600960131     C                   MOVEL     'R'           D15TRC
437700960131     C                   MOVEL     V3CRTP        D15RTC
437800960131     C                   MOVEL     'P'           D15EPA
437900960131     C                   MOVEL     V3CLNP        D15LNP
438000960222     C                   MOVEL     V3CTTR        D15TTR
438100960131     C                   MOVEL     V3CNFV        D15NFV
438200960131     C                   MOVEL     V3HDFV        D15DFV
438300960131     C                   MOVEL     V3CTRN        D15TRN
438400960131     C                   MOVEL     DSLV15        KPJBU
438500951109     C                   CALL      'FNLV15R'
438600951109     C                   PARM                    KPJBA
438700951109     C                   CLEAR                   V3CVDP
438800980119    5C                   ENDIF
438900980119    4C                   ENDIF
439000951103     C*
439100951103     C* VISUALIZZA RITARDI ARRIVO
439200980114     C* Gestione subfile degli arrivi
439300980119     C                   Z-ADD     NR13          WNR13
439400980114     C                   Z-ADD     1             NR13
439500980119    4C     NR13          DOWLE     WNR13
439600010824     C     NR13          CHAIN     TL44S13                            31
439700980119    5C  N31V3CVD1        IFEQ      'V'
439800980114     C*
439900980114    3C***        V3CVD1    IFEQ 'V'
440000980115     C***        V3CVD2    OREQ 'V'
440100980115     C***        V3CVD3    OREQ 'V'
440200980115     C***        V3CVD4    OREQ 'V'
440300980114     C                   MOVE      '1'           W001A             1
440400980114     C                   CLEAR                   DSLV15
440500980114     C                   MOVEL     'A'           D15EPA
440600980114     C                   MOVEL     'I'           D15TUP
440700980114     C                   MOVEL     'R'           D15TRC
440800980115    4C***                  SELEC
440900980115    4C***        V3CVD1    WHEQ 'V'
441000980114     C                   MOVEL     V3CRT1        D15RTC
441100980114     C                   MOVEL     V3CFF1        D15LAI
441200980114     C                   CLEAR                   V3CVD1
441300980115    4C***        V3CVD2    WHEQ 'V'
441400980115     C***                  MOVELV3CRT2    D15RTC
441500980115     C***                  MOVELV3CFF2    D15LAI
441600980115     C***                  CLEARV3CVD2
441700980115    4C***        V3CVD3    WHEQ 'V'
441800980115     C***                  MOVELV3CRT3    D15RTC
441900980115     C***                  MOVELV3CFF3    D15LAI
442000980115     C***                  CLEARV3CVD3
442100980115   X4C***                  OTHER
442200980115     C***                  MOVELV3CRT4    D15RTC
442300980115     C***                  MOVELV3CFF4    D15LAI
442400980115     C***                  CLEARV3CVD4
442500980115    4C***                  ENDSL
442600980115     C***
442700960222     C                   MOVEL     V3CTTR        D15TTR
442800960222     C                   MOVEL     V3CLNP        D15LNP
442900960131     C                   MOVEL     V3CNFV        D15NFV
443000960131     C                   MOVEL     V3HDFV        D15DFV
443100960131     C                   MOVEL     V3CTRN        D15TRN
443200960131     C                   MOVEL     DSLV15        KPJBU
443300951109     C                   CALL      'FNLV15R'
443400951109     C                   PARM                    KPJBA
443500980120     C                   SETOFF                                       16
443600980121     C     NR13          COMP      1                                  8181
443700010824     C                   UPDATE    TL44S13
443800980119    5C                   END
443900980114     C                   ADD       1             NR13
444000980119    4C                   ENDDO
444100951106    3C                   ENDIF
444200980115     C**27 ON --> E' STATA RICHIESTA LA MANUTENZIONE
444300980115     C**         *IN27     IFEQ *ON
444400980115     C* 38 ON --> E' STATA RICHIESTA LA MANUTENZIONE
444500980119    3C     *IN38         IFEQ      *ON
444600960624     C* SE HO VISUALIZZATO DESCRIZIONI RITARDO RIEMETTO IL SUBFILE
444700980119    4C     W001A         IFEQ      '1'
444800960624     C                   GOTO      FORS03
444900980119    4C                   END
445000960625     C* CONTROLLO CAUSALI RITARDO SE MODIFICATE
445100960625     C   14              EXSR      CTRZ03
445200960625     C   14
445300960625     CAN 90              GOTO      FORS03
445400980119    3C                   END
445500960624     C* F6=CONFERMA
445600980119    3C     *INKF         IFEQ      *ON
445700960625     C* AGGIORNAMENTO GRIGLIA
445800960625     C                   EXSR      AGGGRL
445900980119    3C                   END
446000951103     C*
446100951103     C                   GOTO      FORS03
446200951106    2C                   ENDIF
446300951106    1C                   ENDIF
446400951103     C**
446500951103     C                   ENDSR
446600951214     C**
446700960222     C************* CARICO I DATI PARTENZA TRATTA *********************
446800960222     C     CARPAR        BEGSR
446900960305     C                   SETOFF                                       88
447000951215     C* DATA FVT
447100951215     C                   MOVE      TSRDTT        G02INV                         DATA FVT
447200951214     C                   MOVE      *ZEROS        G02DAT
447300951214     C                   MOVEL     '3'           G02ERR
447400951214     C                   CALL      'XSRDA8'
447500951214     C                   PARM                    WLBDAT
447600951214     C                   MOVEL     G02DAT        V3CDTT
447700951214     C                   Z-ADD     G02DAT        W0020             2 0
447800951214     C                   MOVE      W0020         V3CDTT
447900951214     C* FERMATA PARTENZA
448000951214     C                   Z-ADD     TSRFFP        KFIL
448100951214     C                   EXSR      CAZORG
448200951214     C                   MOVEL     TSRFFP        V3CFFP
448300951214     C                   MOVEL     ORGDES        V3DFFP
448400960305     C* TRATTA ANNULLATA
448500960305     C     TSRATB        IFNE      ' '
448600960305     C                   MOVEL     CANNUL        V3DDFV
448700960305     C                   SETON                                        88
448800960305     C                   ELSE
448900960305     C                   MOVEL     CABBIN        V3DDFV
449000960305    1C                   ENDIF
449100951214     C* NUMERO FOGLIO ABBINATO ESISTENTE
449200951214    1C     TSRNFV        IFGT      0
449300951214     C                   Z-ADD     TSRLNP        V3CLNP
449400951214     C                   MOVEL     TSRNFV        V3CNFV
449500951214     C*
449600951214     C                   MOVE      TSRDFV        G02INV
449700951214     C                   MOVE      *ZEROS        G02DAT
449800951214     C                   MOVEL     '3'           G02ERR
449900951214     C                   CALL      'XSRDA8'
450000951214     C                   PARM                    WLBDAT
450100951214     C                   MOVEL     G02DAT        V3CDFV
450200951214     C                   MOVE      G02DAT        V3HDFV
450300951214     C                   Z-ADD     G02DAT        W0020             2 0
450400951214     C                   MOVE      W0020         V3CDFV
450500960222    1C                   ENDIF
450600951214     C* PARTENZA TEORICA
450700951214     C                   MOVE      TSRDTP        G02INV                         DATA TEORICA
450800951214     C                   MOVE      *ZEROS        G02DAT
450900951214     C                   MOVEL     '3'           G02ERR
451000951214     C                   CALL      'XSRDA8'
451100951214     C                   PARM                    WLBDAT
451200951214     C                   MOVEL     G02DAT        V3CDTP
451300951214     C                   Z-ADD     G02DAT        W0020             2 0
451400951214     C                   MOVE      W0020         V3CDTP
451500951214     C                   MOVEL     TSRHTP        V3CHTP                         ORA  TEORICA
451600951214     C* PARTENZA REALE
451700951214    1C     TSRDRP        IFGT      0                                            DATA REALE
451800951214     C                   MOVE      TSRDRP        G02INV
451900951214     C                   MOVE      *ZEROS        G02DAT
452000951214     C                   MOVEL     '3'           G02ERR
452100951214     C                   CALL      'XSRDA8'
452200951214     C                   PARM                    WLBDAT
452300951214     C                   MOVEL     G02DAT        V3CDRP
452400951214     C                   Z-ADD     G02DAT        W0020             2 0
452500951214     C                   MOVE      W0020         V3CDRP
452600951214     C                   MOVEL     TSRHRP        V3CHRP                         ORA REALE
452700951214     C*
452800951214    2C     TSRRTP        IFNE      ' '                                          RITARDO
452900951214     C                   MOVEL     TSRRTP        V3CRTP
453000960625     C                   MOVEL     TSRRTP        V3HRTP
453100951214     C                   MOVEL     'TC'          COD
453200951214     C                   MOVEL(P)  TSRRTP        KEY
453300961029     C     KTAB          CHAIN     TABEL                              87
453400961029     C  N87              MOVEL     TBLUNI        V3DRTP
453500961029     C   87              MOVEL     *ALL'?'       V3DRTP
453600951214     C*
453700951214     C* CONTROLLO SE ESISTE ALMENO UNA RIGA DI DESCRIZIONE
453800951214     C                   CLEAR                   KFIL
453900960213     C                   MOVEL     'R'           KWTRC
454000951214     C                   MOVEL     'P'           KEPA
454100961029     C     KFV4          SETLL     FNFV401L                               87
454200961029     C   87              SETOFF                                       15
454300951214    2C                   ENDIF
454400951214    1C                   ENDIF
454500960222     C                   ENDSR
454600960222     C**
454700960222     C************* CARICO I DATI DI ARRIVO TRATTA ********************
454800960222     C     CARARR        BEGSR
454900951214     C* FERMATE ARRIVO
455000951214     C                   Z-ADD     TSRFFA        KFIL
455100951214     C                   EXSR      CAZORG
455200951214     C* ARRIVO TEORICO
455300951214     C                   MOVE      TSRDTA        G02INV                         DATA TEORICA
455400951214     C                   MOVE      *ZEROS        G02DAT
455500951214     C                   MOVEL     '3'           G02ERR
455600951214     C                   CALL      'XSRDA8'
455700951214     C                   PARM                    WLBDAT
455800951214     C                   Z-ADD     G02DAT        W0020             2 0
455900951214     C**
456000980114    1C*****                SELEC
456100980114     C                   ADD       1             NR13
456200980114     C     NR13          IFEQ      1
456300980120     C                   SETOFF                                       81
456400980114     C                   ELSE
456500980120     C                   SETON                                        81
456600980114     C                   ENDIF
456700980114     C*****      WCONT     WHEQ 1
456800951214     C                   MOVEL     TSRFFA        V3CFF1
456900951214     C                   MOVEL     ORGDES        V3DFF1
457000951214     C                   MOVEL     G02DAT        V3CDT1
457100951214     C                   MOVE      W0020         V3CDT1
457200951214     C                   MOVEL     TSRHTA        V3CHT1                         ORA  TEORICA
457300960625     C                   MOVE      TSRNFA        V3HNF1
457400980114     C****       WCONT     WHEQ 2
457500980114     C****                 MOVELTSRFFA    V3CFF2
457600980114     C****                 MOVELORGDES    V3DFF2
457700980114     C****                 MOVELG02DAT    V3CDT2
457800980114     C****                 MOVE W0020     V3CDT2
457900980114     C****                 MOVELTSRHTA    V3CHT2           ORA  TEORICA
458000980114     C****                 MOVE TSRNFA    V3HNF2
458100980114     C****       WCONT     WHEQ 3
458200980114     C****                 MOVELTSRFFA    V3CFF3
458300980114     C****                 MOVELORGDES    V3DFF3
458400980114     C****                 MOVELG02DAT    V3CDT3
458500980114     C****                 MOVE W0020     V3CDT3
458600980114     C****                 MOVELTSRHTA    V3CHT3           ORA  TEORICA
458700980114     C****                 MOVE TSRNFA    V3HNF3
458800980114   X1C****                 OTHER
458900980114     C****                 MOVELTSRFFA    V3CFF4
459000980114     C****                 MOVELORGDES    V3DFF4
459100980114     C****                 MOVELG02DAT    V3CDT4
459200980114     C****                 MOVE W0020     V3CDT4
459300980114     C****                 MOVELTSRHTA    V3CHT4           ORA  TEORICA
459400980114     C****                 MOVE TSRNFA    V3HNF4
459500980114    1C****                 ENDSL
459600951214     C* ARRIVO REALE
459700951214     C                   SETOFF                                       31
459800951214     C                   CLEAR                   TBLUNI
459900951214    1C     TSRDRA        IFGT      0                                            DATA REALE
460000951214     C                   MOVE      TSRDRA        G02INV
460100951214     C                   MOVE      *ZEROS        G02DAT
460200951214     C                   MOVEL     '3'           G02ERR
460300951214     C                   CALL      'XSRDA8'
460400951214     C                   PARM                    WLBDAT
460500951214     C                   Z-ADD     G02DAT        W0020             2 0
460600960131     C                   CLEAR                   TBLUNI
460700951214    2C     TSRRTA        IFNE      ' '                                          RITARDO
460800951214     C                   MOVEL     'TC'          COD
460900951214     C                   MOVEL(P)  TSRRTA        KEY
461000951214     C     KTAB          CHAIN     TABEL                              31
461100960131     C   31              MOVEL     *ALL'?'       TBLUNI
461200951214    2C                   ENDIF
461300951214     C*
461400951214     C* CONTROLLO SE ESISTE ALMENO UNA RIGA DI DESCRIZIONE
461500951214     C                   Z-ADD     TSRFFA        KFIL
461600960213     C                   MOVEL     'R'           KWTRC
461700951214     C                   MOVEL     'A'           KEPA
461800951214     C     KFV4          SETLL     FNFV401L                               31
461900980119     C   31              SETOFF                                       16
462000951214     C**
462100980114    2C****                 SELEC
462200980114     C****       WCONT     WHEQ 1
462300951214     C                   MOVEL     G02DAT        V3CDR1
462400951214     C                   MOVE      W0020         V3CDR1
462500960201     C                   MOVEL     TSRHRA        V3CHR1                         ORA REALE
462600951214     C                   MOVEL     TSRRTA        V3CRT1
462700960625     C                   MOVEL     TSRRTA        V3HRT1
462800960131     C                   MOVEL     TBLUNI        V3DRT1
462900980119     C   31              SETOFF                                       16
463000980114     C***
463100980114     C***        WCONT     WHEQ 2
463200980114     C***                  MOVELG02DAT    V3CDR2
463300980114     C***                  MOVE W0020     V3CDR2
463400980114     C***                  MOVELTSRHRA    V3CHR2           ORA REALE
463500980114     C***                  MOVELTSRRTA    V3CRT2
463600980114     C***                  MOVELTSRRTA    V3HRT2
463700980114     C***                  MOVELTBLUNI    V3DRT2
463800980114     C***31                SETOF                     91
463900980114     C***
464000980114     C***        WCONT     WHEQ 3
464100980114     C***                  MOVELG02DAT    V3CDR3
464200980114     C***                  MOVE W0020     V3CDR3
464300980114     C***                  MOVELTSRHRA    V3CHR3           ORA REALE
464400980114     C***                  MOVELTSRRTA    V3CRT3
464500980114     C***                  MOVELTSRRTA    V3HRT3
464600980114     C***                  MOVELTBLUNI    V3DRT3
464700980114     C***31                SETOF                     92
464800980114   X2C***                  OTHER
464900980114     C***
465000980114     C***                  MOVELG02DAT    V3CDR4
465100980114     C***                  MOVE W0020     V3CDR4
465200980114     C***                  MOVELTSRHRA    V3CHR4           ORA REALE
465300980114     C***                  MOVELTSRRTA    V3CRT4
465400980114     C***                  MOVELTSRRTA    V3HRT4
465500980114     C***                  MOVELTBLUNI    V3DRT4
465600980114     C***31                SETOF                     93
465700980114    2C***                  ENDSL
465800960222    1C                   ENDIF
465900951214     C*
466000980119     C                   SETON                                        16
466100010824     C                   WRITE     TL44S13
466200010824     C                   CLEAR                   TL44S13
466300980114     C*
466400951214     C                   ENDSR
466500951103     C**
466600010824     C****************  CONTROLLI TL44Z03 *****************************
466700960625     C     CTRZ03        BEGSR
466800960625     C* GESTIONE ?
466900960625     C                   MOVEL     CODUT         §KUT
467000960625     C                   MOVEL     'TC'          §COD
467100960625     C*
467200960625     C     V3CRTP        IFEQ      '?'
467300960625     C                   MOVEL     *BLANKS       §KEY
467400960625     C                   CALL      'X§TABER'
467500960625     C                   PARM                    §KUT              1 0
467600960625     C                   PARM                    §COD              2
467700960625     C                   PARM                    §KEY              8
467800960625     C                   PARM                    §DES             30
467900960625     C                   MOVEL     §KEY          V3CRTP
468000960625     C                   MOVEL     §DES          V3DRTP
468100960625     C                   SETON                                        90
468200960625     C                   GOTO      ENDZ03
468300960625     C                   END
468400980115     C                   MOVEL     'TC'          COD
468500980115     C* CAUSALE RITARDO/ANTICIPO PARTENZA
468600980115    1C     *IN35         IFEQ      *ON
468700980115     C                   CLEAR                   V3DRTP
468800980115    2C     V3CRTP        IFNE      V3HRTP
468900980115     C     V3CRTP        ANDEQ     *BLANKS
469000980115     C                   MOVEL     MSG(39)       V3CMSG
469100980115     C                   SETON                                        289052
469200980115     C                   GOTO      ENDZ03
469300980115    2C                   END
469400980115    2C     V3CRTP        IFNE      *BLANKS
469500980115     C                   MOVEL(P)  V3CRTP        KEY
469600980115     C     KTAB          CHAIN     TABEL                              30
469700980115    3C     *IN30         IFEQ      *OFF
469800980115     C     TBLFLG        ANDEQ     *BLANKS
469900980115     C                   MOVEL     TBLUNI        V3DRTP
470000980115     C                   ELSE
470100980115     C                   MOVEL     MSG(39)       V3CMSG
470200980115     C                   SETON                                        289052
470300980115     C                   GOTO      ENDZ03
470400980115    3C                   END
470500980115    2C                   END
470600980115    1C                   END
470700980115     C**** CONTROLLO SUBFILE ARRIVI
470800980119     C                   Z-ADD     NR13          WNR13
470900980115     C                   Z-ADD     1             NR13
471000980115     C     NR13          DOWLE     WNR13
471100980115     C                   SETOFF                                       902853
471200010824     C     NR13          CHAIN     TL44S13                            31
471300980115     C     *IN31         IFEQ      *OFF
471400980115     C     V3CRT1        IFEQ      '?'
471500960625     C                   MOVEL     *BLANKS       §KEY
471600960625     C                   CALL      'X§TABER'
471700960625     C                   PARM                    §KUT              1 0
471800960625     C                   PARM                    §COD              2
471900960625     C                   PARM                    §KEY              8
472000960625     C                   PARM                    §DES             30
472100960625     C                   MOVEL     §KEY          V3CRT1
472200960625     C                   MOVEL     §DES          V3DRT1
472300960625     C                   SETON                                        90
472400960625     C                   END
472500980115     C* CAUSALI RITARDO/ANTICIPO ARRIVO
472600980115    1C     *IN36         IFEQ      *ON
472700980115     C                   CLEAR                   V3DRT1
472800980115    2C     V3CRT1        IFNE      V3HRT1
472900980115     C     V3CRT1        ANDEQ     *BLANKS
473000980115     C                   MOVEL     MSG(39)       V3CMSG
473100980115     C                   SETON                                        289053
473200980115    2C                   END
473300980115    2C     V3CRT1        IFNE      *BLANKS
473400980115     C                   MOVEL(P)  V3CRT1        KEY
473500980115     C     KTAB          CHAIN     TABEL                              30
473600980115    3C     *IN30         IFEQ      *OFF
473700980115     C     TBLFLG        ANDEQ     *BLANKS
473800980115     C                   MOVEL     TBLUNI        V3DRT1
473900980115     C                   ELSE
474000980115     C                   MOVEL     MSG(39)       V3CMSG
474100980115     C                   SETON                                        289053
474200980115    3C                   END
474300980115    2C                   END
474400980115    1C                   END
474500980121     C     NR13          COMP      1                                  8181
474600010824     C                   UPDATE    TL44S13
474700980115     C   90              GOTO      ENDZ03
474800980119     C                   END
474900980115     C                   ADD       1             NR13
475000980115     C                   ENDDO
475100980115     C*          V3CRT2    IFEQ '?'
475200980115     C*                    MOVEL*BLANKS   §KEY
475300980115     C*                    CALL 'X§TABER'
475400980115     C*                    PARM           §KUT    10
475500980115     C*                    PARM           §COD    2
475600980115     C*                    PARM           §KEY    8
475700980115     C*                    PARM           §DES   30
475800980115     C*                    MOVEL§KEY      V3CRT2
475900980115     C*                    MOVEL§DES      V3DRT2
476000980115     C*                    SETON                     90
476100980115     C*                    GOTO ENDZ03
476200980115     C*                    END
476300980115     C*          V3CRT3    IFEQ '?'
476400980115     C*                    MOVEL*BLANKS   §KEY
476500980115     C*                    CALL 'X§TABER'
476600980115     C*                    PARM           §KUT    10
476700980115     C*                    PARM           §COD    2
476800980115     C*                    PARM           §KEY    8
476900980115     C*                    PARM           §DES   30
477000980115     C*                    MOVEL§KEY      V3CRT3
477100980115     C*                    MOVEL§DES      V3DRT3
477200980115     C*                    SETON                     90
477300980115     C*                    GOTO ENDZ03
477400980115     C*                    END
477500980115     C*          V3CRT4    IFEQ '?'
477600980115     C*                    MOVEL*BLANKS   §KEY
477700980115     C*                    CALL 'X§TABER'
477800980115     C*                    PARM           §KUT    10
477900980115     C*                    PARM           §COD    2
478000980115     C*                    PARM           §KEY    8
478100980115     C*                    PARM           §DES   30
478200980115     C*                    MOVEL§KEY      V3CRT4
478300980115     C*                    MOVEL§DES      V3DRT4
478400980115     C*                    SETON                     90
478500980115     C*                    GOTO ENDZ03
478600980115     C*                    END
478700960625     C*
478800960625     C*
478900980115    1C***        *IN37     IFEQ *ON
479000980115     C***                  CLEARV3DRT2
479100980115    2C***        V3CRT2    IFNE V3HRT2
479200980115     C***        V3CRT2    ANDEQ*BLANKS
479300980115     C***                  MOVELMSG,39    V3CMSG
479400980115     C***                  SETON                     289054
479500980115     C***                  GOTO ENDZ03
479600980115    2C***                  END
479700980115    2C***        V3CRT2    IFNE *BLANKS
479800980115     C***                  MOVELV3CRT2    KEY       P
479900980115     C***        KTAB      CHAINTABEL                30
480000980115    3C***        *IN30     IFEQ *OFF
480100980115     C***        TBLFLG    ANDEQ*BLANKS
480200980115     C***                  MOVELTBLUNI    V3DRT2
480300980115     C***                  ELSE
480400980115     C***                  MOVELMSG,39    V3CMSG
480500980115     C***                  SETON                     289054
480600980115     C***                  GOTO ENDZ03
480700980115    3C***                  END
480800980115    2C***                  END
480900980115    1C***                  END
481000960625     C*
481100980115    1C***        *IN38     IFEQ *ON
481200980115     C***                  CLEARV3DRT3
481300980115    2C***        V3CRT3    IFNE V3HRT3
481400980115     C***        V3CRT3    ANDEQ*BLANKS
481500980115     C***                  MOVELMSG,39    V3CMSG
481600980115     C***                  SETON                     289055
481700980115     C***                  GOTO ENDZ03
481800980115    2C***                  END
481900980115    2C***        V3CRT3    IFNE *BLANKS
482000980115     C***                  MOVELV3CRT3    KEY       P
482100980115     C***        KTAB      CHAINTABEL                30
482200980115    3C***        *IN30     IFEQ *OFF
482300980115     C***        TBLFLG    ANDEQ*BLANKS
482400980115     C***                  MOVELTBLUNI    V3DRT3
482500980115     C***                  ELSE
482600980115     C***                  MOVELMSG,39    V3CMSG
482700980115     C***                  SETON                     289055
482800980115     C***                  GOTO ENDZ03
482900980115    3C***                  END
483000980115    2C***                  END
483100980115    1C***                  END
483200980115     C***
483300980115    1C***        *IN39     IFEQ *ON
483400980115     C***                  CLEARV3DRT4
483500980115    2C***        V3CRT4    IFNE V3HRT4
483600980115     C***        V3CRT4    ANDEQ*BLANKS
483700980115     C***                  MOVELMSG,39    V3CMSG
483800980115     C***                  SETON                     289056
483900980115     C***                  GOTO ENDZ03
484000980115    2C***                  END
484100980115    2C***        V3CRT4    IFNE *BLANKS
484200980115     C***                  MOVELV3CRT4    KEY       P
484300980115     C***        KTAB      CHAINTABEL                30
484400980115    3C***        *IN30     IFEQ *OFF
484500980115     C***        TBLFLG    ANDEQ*BLANKS
484600980115     C***                  MOVELTBLUNI    V3DRT4
484700980115     C***                  ELSE
484800980115     C***                  MOVELMSG,39    V3CMSG
484900980115     C***                  SETON                     289056
485000980115     C***                  GOTO ENDZ03
485100980115    3C***                  END
485200980115    2C***                  END
485300980115    1C***                  END
485400980115     C***
485500960625     C     ENDZ03        ENDSR
485600960625     C****************  AGGIORNAMENTO GRIGLIA  ************************
485700960625     C     AGGGRL        BEGSR
485800960625     C* RIPROTEGGO CAMPI I/O
485900960625     C                   SETOFF                                       273536
486000960625     C                   SETOFF                                       373839
486100960625     C* AGGIORNAMENTO METRI CUBI ASSEGNATI
486200960625     C                   Z-ADD     NRR3          WNRR3
486300960625     C                   Z-ADD     1             NRR3
486400960625    1C     NRR3          DOWLE     WNRR3
486500010824     C     NRR3          CHAIN     TL44S03                            31
486600960625    2C     *IN31         IFEQ      *OFF
486700960625     C     V3CMTP        ANDGT     0
486800960625    3C     V3CMCA        IFNE      V3HMCA
486900960710    3C     V3CMCT        ORNE      V3HMCT
487000960710    3C     V3CMCB        ORNE      V3HMCB
487100960625     C     KTSM01        CHAIN     TNTSM01L                           31
487200960625    4C     *IN31         IFEQ      *OFF
487300960625     C                   Z-ADD     V3CMCA        TSMMCA
487400960710     C                   Z-ADD     V3CMCT        TSMMCT
487500960710     C                   Z-ADD     V3CMCB        TSMMCB
487600960625     C                   UPDATE    TNTSM000
487700960625    4C                   END
487800960625    3C                   END
487900010824     C                   UPDATE    TL44S03
488000960625    2C                   END
488100960625     C                   ADD       1             NRR3
488200960625    1C                   ENDDO
488300960625     C* AGGIORNAMENTO CAUSALI RITARDO/ANTICIPO SE VARIATI
488400980115    1C***14      V3CRTP    IFNE V3HRTP
488500980115     C***        V3CRT1    ORNE V3HRT1
488600980115     C***        V3CRT2    ORNE V3HRT2
488700980115     C***        V3CRT3    ORNE V3HRT3
488800980115     C***        V3CRT4    ORNE V3HRT4
488900980115     C* FACCIO L'AGGIORNAMENTO SOLO PER 14 ACCESO E SOSTITUISCO LA LETTURA
489000980115     C* DELLE SINGOLE RIGHE DI ARRIVO CON LE RIGHE DEL  SUBFILE
489100980115    1C     *IN14         IFEQ      *ON
489200980115     C*
489300980115     C     KTSR13        SETLL     TNTSR01L
489400960625     C     KTSR13        READE     TNTSR01L                               31
489500960625    2C     *IN31         DOWEQ     *OFF
489600960625     C                   CLEAR                   W001A
489700960625    3C     TSRNFP        IFEQ      KVNFP
489800960625     C     V3CRTP        ANDNE     V3HRTP
489900960625     C                   MOVEL     V3CRTP        TSRRTP
490000960625     C                   MOVE      '1'           W001A             1
490100970410    4C     V3CRTP        IFNE      ' '
490200970410     C     TSRDKP        ANDEQ     99999999
490300980209     C*****                MOVE *ZEROS    TSRDKP
490400980209     C* INVECE CHE AZZERARE LA DATA  LA METTO = A DATA ULTIMO CONTROLL
490500980424     C* NON AZZERO LA DATA E NON LA IMPOSTO COME LA DATA ULTIMO
490600980424     C* CONTROLLO MA CI METTO LA DATA REALE PARTENZA PER PERMETTERE
490700980424     C* DI STAMPARE QUESTO RITARDO NEL MESE DI COMPETENZA PER POTER
490800980424     C* QUADRARE IL PIU' POSSIBILE LA STATISTICA RITARDI CON LA STAMPA
490900980424     C* RITARDI
491000980424     C                   MOVE      TSRDRP        TSRDKP
491100980115    4C                   END
491200960625    3C                   END
491300980115    3C***                  SELEC
491400980115     C***        TSRNFA    WHEQ V3HNF1
491500980115     C* LETTURA DEL SUBFILE PER OGNI RIGA DEL FILE TNTSR
491600980115     C                   Z-ADD     NR13          WNR13
491700980115     C                   Z-ADD     1             NR13
491800980115     C     NR13          DOWLE     WNR13
491900010824     C     NR13          CHAIN     TL44S13                            57
492000980115    2C     *IN57         IFEQ      *OFF
492100980115    3C     TSRNFA        IFEQ      V3HNF1
492200980115     C     V3CRT1        ANDNE     V3HRT1
492300980115     C                   MOVEL     V3CRT1        TSRRTA
492400980115     C                   MOVE      '1'           W001A             1
492500980115     C* SE DATA E ORA REALE SONO =0 IMPOSTO UGUALE ALLA TEORICA
492600980115    4C     TSRDRA        IFEQ      0
492700980115     C                   MOVEL     TSRDTA        TSRDRA
492800980115     C                   MOVEL     TSRHTA        TSRHRA
492900980115    4C                   ENDIF
493000980115    4C     V3CRT1        IFNE      ' '
493100980115     C     TSRDKA        ANDEQ     99999999
493200980209     C*****                MOVE *ZEROS    TSRDKA
493300980424     C* INVECE CHE AZZERARE LA DATA  LA METTO = A DATA ULTIMO CONTROLL
493400980424     C* NON AZZERO LA DATA E NON LA IMPOSTO COME LA DATA ULTIMO
493500980424     C* CONTROLLO MA CI METTO LA DATA REALE ARRIVO PER PERMETTERE
493600980424     C* DI STAMPARE QUESTO RITARDO NEL MESE DI COMPETENZA PER POTER
493700980424     C* QUADRARE IL PIU' POSSIBILE LA STATISTICA RITARDI CON LA STAMPA
493800980424     C* RITARDI
493900980424     C                   MOVE      TSRDRA        TSRDKA
494000980115    4C                   END
494100980115    3C                   END
494200980120     C                   SETON                                        1516
494300980121     C     NR13          COMP      1                                  8181
494400010824     C                   UPDATE    TL44S13
494500980115    2C                   END
494600980115     C                   ADD       1             NR13
494700980115     C                   ENDDO
494800980115     C*
494900980119    3C     W001A         IFEQ      '1'
495000980119     C                   UPDATE    TNTSR000
495100980119     C                   ELSE
495200980119     C                   UNLOCK    TNTSR01L
495300980119    3C                   END
495400980115     C     KTSR13        READE     TNTSR01L                               31
495500980115    2C                   ENDDO
495600980115    1C                   END
495700980115     C***        TSRNFA    WHEQ V3HNF2
495800980115     C***        V3CRT2    ANDNEV3HRT2
495900980115     C***                  MOVELV3CRT2    TSRRTA
496000980115     C***                  MOVE '1'       W001A   1
496100980115     C***        TSRDRA    IFEQ 0
496200980115     C***                  MOVELTSRDTA    TSRDRA
496300980115     C***                  MOVELTSRHTA    TSRHRA
496400980115     C***                  ENDIF
496500980115    4C***        V3CRT2    IFNE ' '
496600980115     C***        TSRDKA    ANDEQ99999999
496700980115     C***                  MOVE *ZEROS    TSRDKA
496800980115    4C***                  END
496900980115     C***        TSRNFA    WHEQ V3HNF3
497000980115     C***        V3CRT3    ANDNEV3HRT3
497100980115     C***                  MOVELV3CRT3    TSRRTA
497200980115     C***                  MOVE '1'       W001A   1
497300980115     C***        TSRDRA    IFEQ 0
497400980115     C***                  MOVELTSRDTA    TSRDRA
497500980115     C***                  MOVELTSRHTA    TSRHRA
497600980115     C***                  ENDIF
497700980115    4C***        V3CRT3    IFNE ' '
497800980115     C***        TSRDKA    ANDEQ99999999
497900980115     C***                  MOVE *ZEROS    TSRDKA
498000980115    4C***                  END
498100980115     C***        TSRNFA    WHEQ V3HNF4
498200980115     C***        V3CRT4    ANDNEV3HRT4
498300980115     C***                  MOVELV3CRT4    TSRRTA
498400980115     C***                  MOVE '1'       W001A   1
498500980115     C***        TSRDRA    IFEQ 0
498600980115     C***                  MOVELTSRDTA    TSRDRA
498700980115     C***                  MOVELTSRHTA    TSRHRA
498800980115     C***                  ENDIF
498900980115    4C***        V3CRT4    IFNE ' '
499000980115     C***        TSRDKA    ANDEQ99999999
499100980115     C***                  MOVE *ZEROS    TSRDKA
499200980115    4C***                  END
499300980115    3C***                  ENDSL
499400960625     C**
499500960625     C                   ENDSR
499600951103     C****************  CHAIN SU AZORG01L *****************************
499700951103     C     CAZORG        BEGSR
499800960305     C     KFIL          CHAIN     AZORG                              31
499900960305     C   31              MOVEL     *ALL'?'       ORGDES
500000951103     C                   ENDSR
500100951107     C**
500200951107     C*************** CANCELLO RECROD IN TNTSW ************************
500300951107     C     DELTSW        BEGSR
500400951107     C     KTSW12        SETLL     TNTSW01L
500500951107     C     KTSW12        READE     TNTSW01L                               31
500600951107     C     *IN31         DOWEQ     *OFF
500700960322     c* se impostato il campo del sfl cancello solo quelli del sfl
500800960322     C     KSFL          IFEQ      0
500900960322     C     KSFL          OREQ      TSWSFL
501000960322     C*
501100951107     C                   DELETE    TNTSW000
501200960322     C                   ENDIF
501300951107     C     KTSW12        READE     TNTSW01L                               31
501400951107     C                   ENDDO
501500951107     C                   ENDSR
501600951117     C**
501700951117     C********** ABBINAMENTO FV VISULIZZO DATI NEL SFL*****************
501800951117     C     ABBIN         BEGSR
501900951117     C                   Z-ADD     NRR2          KNRR
502000951117     C     KTSW11        CHAIN(N)  TNTSW01L                           31
502100951121     C* E' LA FILIALE CHE ESPONE L'IMPORTO DEL FV
502200960221    0C     *IN31         IFEQ      *OFF
502300951124     C*
502400960305     C                   MOVEL     V2HNFP        SAVNFP
502500960305     C*
502600951121     C     V2HNRC        SUB       2             W0050
502700951121     C     NRR2          SUB       W0050         KNRR
502800951121     C*
502900951121     C* CHAIN 2 RECORD PER IMPOSTARE L'IMPORTO DEL FV
503000960221    1C     TSWFES        IFEQ      FVSLNP
503100960305     C     TSWNFE        ANDEQ     SAVNFP
503200960221     C*
503300010824     C     KNRR          CHAIN     TL44S02                            31
503400010824
503500010907      * se ho la divisa del foglio viaggio a blank devo prendere quella
503600010907      * della griglia
503700010907     c                   if        fwsvip = *blanks
503800010907     c                   eval      fwsvip = tstdiv
503900010907     c                   endif
504000010907     c                   if        fwsves = *blanks
504100010907     c                   eval      fwsves = tstdiv
504200010907     c                   endif
504300010827      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
504400010827      * devo convertire fwsipt e fwsies nella divisa della griglia
504500010827     c                   eval      w_importo = fwsipt
504600011023     c                   if        fwsvip <> tswdiv and fwsipt <> 1
504700011023     c                             and fwsipt <> 0
504800010827     c                   eval      w_divisa = fwsvip
504900010827     c                   exsr      Sr_Converti
505000010827     c                   endif
505100010827     c                   z-add     w_importo     v2cimp
505200010827     C**!!!              Z-ADD     FVSIPT        V2CIMP
505300951121     C                   EXSR      CTRIND
505400010824     C                   UPDATE    TL44S02
505500960221    1C                   ENDIF
505600951124     C*
505700951121     C* CHAIN 1 RECORD PER IMPOSTARE GLI ALTRI DATI
505800951121     C                   SUB       1             KNRR
505900951127     C     KTSW11        CHAIN     TNTSW01L                           31
506000010824     C     KNRR          CHAIN     TL44S02                            31
506100951121     C*
506200960221    1C     TSWFES        IFEQ      FVSLNP
506300960305     C     TSWNFE        ANDEQ     SAVNFP
506400960221     C*
506500960221     C* SE TRAZIONISTA MANCA --> GLI LASCIO QUELLO CHE GIA' C'E'
506600960221    2C     FVSPDR        IFEQ      0
506700951124     C                   MOVEL     V2CPDR        FVSPDR
506800960221    2C                   ENDIF
506900960221     C*
507000960221     C                   MOVEL     FVSPDR        W007A             7
507100960221     C*
507200960221    2C     W007A         IFNE      V2CPDR
507300960221     C     FVSTMZ        ORNE      V2HTMZ
507400960221     C                   CLEAR                   V2HPDR
507500960221    2C                   ENDIF
507600951124     C*
507700951124     C                   MOVEL     FVSPDR        V2CPDR
507800960305     C     V2CPDR        IFLE      *ZEROS
507900960305     C                   CLEAR                   V2CPDR
508000960305     C                   ENDIF
508100960305     C                   MOVEL     FVSDPD        V2DPDR
508200951124     C                   MOVEL     FVSTMZ        V2HTMZ
508300010824
508400010827      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
508500010827      * devo convertire fwsipt e fwsies nella divisa della griglia
508600010827     c                   eval      w_importo = fwsipt
508700011023     c                   if        fwsvip <> tswdiv and fwsipt <> 1
508800011023     c                             and fwsipt <> 0
508900010827     c                   eval      w_divisa = fwsvip
509000010827     c                   exsr      Sr_Converti
509100010827     c                   endif
509200010827     c                   eval      tswipt = w_importo
509300010827     c                   z-add     w_importo     v2hipt
509400010827
509500010827     c                   eval      w_importo = fwsies
509600011023     c                   if        fwsves <> tswdiv and fwsies <> 1
509700011023     c                             and fwsies <> 0
509800010827     c                   eval      w_divisa = fwsves
509900010827     c                   exsr      Sr_Converti
510000010827     c                   endif
510100010827     c                   eval      tswies = w_importo
510200010827     C**!!!              Z-ADD     FVSIPT        V2HIPT
510300960221     C*
510400960221     C                   MOVEL     V2CPDR        TSWVPD
510500960305     C                   MOVEL     V2DPDR        TSWDPD
510600960221     C                   MOVEL     V2HTMZ        TSWTMZ
510700010827     C**!!!              MOVEL     FVSIPT        TSWIPT
510800010827     C**!!!              MOVEL     FVSIES        TSWIES
510900960221     C                   MOVEL     'S'           TSWFVR
511000960221     C                   UPDATE    TNTSW000
511100960221    1C                   ENDIF
511200960221     C* ACCENDO INDICATORE DI SFLNEXCHG PER  VEDERE SE POSSO  CONFERMAR
511300960221     C*  L'IMPORTO
511400960221     C                   SETON                                        22
511500951124     C                   EXSR      CTRIND
511600010824     C                   UPDATE    TL44S02
511700960221     C                   SETOFF                                       22
511800960221    0C                   ENDIF
511900951121     C*
512000951117     C                   ENDSR
512100951106     C**
512200960221     C*********** SBINAMENTO FV ***************************************
512300960221     C     SBINAM        BEGSR
512400960221     C                   Z-ADD     NRR2          KNRR
512500960221     C     KTSW11        CHAIN(N)  TNTSW01L                           31
512600960221     C* E' LA FILIALE CHE ESPONE L'IMPORTO DEL FV
512700960221    0C     *IN31         IFEQ      *OFF
512800960221     C*
512900960221     C     V2HNRC        SUB       2             W0050
513000960221     C     NRR2          SUB       W0050         KNRR
513100960221     C*
513200960221     C* CHAIN 2 RECORD PER IMPOSTARE L'IMPORTO DEL FV
513300960223    1C     TSWFES        IFEQ      V2CFFP
513400960221     C     TSWNFE        ANDEQ     V2HNFP
513500960221     C*
513600010824     C     KNRR          CHAIN     TL44S02                            31
513700960223     C                   CLEAR                   V2CIMP
513800960221     C                   EXSR      CTRIND
513900010824     C                   UPDATE    TL44S02
514000960221    1C                   ENDIF
514100960221     C*
514200960221     C* CHAIN 1 RECORD PER IMPOSTARE GLI ALTRI DATI
514300960221     C                   SUB       1             KNRR
514400960221     C     KTSW11        CHAIN     TNTSW01L                           31
514500010824     C     KNRR          CHAIN     TL44S02                            31
514600960221     C*
514700960221    1C     TSWFES        IFEQ      FVSLNP
514800960221     C     TSWNFE        ANDEQ     V2HNFP
514900960221     C*
515000960221     C                   CLEAR                   V2CPDR
515100960221     C                   CLEAR                   V2HPDR
515200960221     C                   CLEAR                   V2HTMZ
515300960221     C                   CLEAR                   V2CICO
515400960221     C                   CLEAR                   V2HICO
515500960221     C                   CLEAR                   V2HIPT
515600960221     C                   CLEAR                   V2CCAU
515700960221     C*
515800960221     C                   CLEAR                   TSWVCU
515900960221     C                   CLEAR                   TSWVPD
516000960221     C                   CLEAR                   TSWTMZ
516100960221     C                   CLEAR                   TSWIPT
516200960223     C                   CLEAR                   TSWIES
516300960221     C                   MOVEL     'S'           TSWFVR
516400960221     C                   UPDATE    TNTSW000
516500960221    1C                   ENDIF
516600960221     C* ACCENDO INDICATORE DI SFLNEXCHG PER  VEDERE SE POSSO  CONFERMAR
516700960221     C*  L'IMPORTO
516800960221     C                   SETON                                        2219
516900960221     C                   EXSR      CTRIND
517000010824     C                   UPDATE    TL44S02
517100960221     C                   SETOFF                                       2219
517200960221    0C                   ENDIF
517300960221     C*
517400960221     C                   ENDSR
517500951215     C********** GESTIONE SFL PER DATA FVT SFL04 **********************
517600951106     C     GESTRT        BEGSR
517700951212     C                   SETOFF                                       10
517800951212     C                   Z-ADD     4             KSFL
517900951212     C                   CLEAR                   KLNP
518000951215     C                   CLEAR                   WPOSIZ
518100960322     C* QUI NON ABILITO F19 F20
518200960322     C                   SETOFF                                       10
518300960322     C                   Z-ADD     19            WRIPAG            5 0
518400951212     C*
518500951212     C* C A R I C A M E N T O   S F L C T L
518600951215     C* DATA FVT
518700951212     C                   MOVEL     V1CTTD        V4CTTD
518800951212     C                   MOVE      V1CTTD        W0020             2 0
518900951212     C                   MOVE      W0020         V4CTTD
519000951212     C*
519100951212     C                   MOVEL     V1CTTA        V4CTTA
519200951212     C                   MOVE      V1CTTA        W0020             2 0
519300951212     C                   MOVE      W0020         V4CTTA
519400951212     C*
519500951212     C                   CLEAR                   V4CRLP
519600951212     C                   CLEAR                   V4CRDT
519700951212     C*
519800951214     C* IMPOSTO LA RIGA DI TASTI FUNZIONALI: LA PRIMA
519900960322     C                   MOVEL     FN4N(1)       V4CFUN
520000951212     C* WF CONTIENE LA RIGA DI TASTI FUNZIONALI IN VISUALIZZAZIONE
520100951212     C                   Z-ADD     1             WF                2 0
520200951212     C* PULIZIA SFL
520300951214     C                   EXSR      PULSFL
520400951212     C*
520500951212     C* C A R I C A M E N T O  S F L
520600951214     C                   Z-ADD     AMGTTD        KDTT
520700951212     C                   Z-ADD     V1CLNP        KLNP
520800951212     C                   Z-ADD     V1CNFV        KNFV
520900951212     C* POSIZIONAMENTO TNTSR
521000951212     C                   EXSR      SETSR4
521100951212     C*
521200951212     C* LETTURA PER CARICAMENTO SFL
521300951212     C                   CLEAR                   WF20
521400951212     C                   Z-ADD     0             L                 5 0
521500951212     C                   EXSR      LEGS04
521600951212     C*
521700951212     C* SE NON HO CARICATO NULLA --> ERRORE
521800951212     C     NRR4          IFEQ      0
521900951212     C                   MOVEL     MSG(15)       V1CMSG
522000951212     C                   SETON                                        289043
522100951212     C                   GOTO      ENDGTT
522200951212     C                   ENDIF
522300951212     C                   Z-ADD     1             V4CREC
522400951212     C*
522500951212     C     FORS04        TAG
522600040422     c     wnrr4         cabeq     0             endgtt
522700010824     C                   WRITE     TL44T01
522800010824     C                   WRITE     TL44Z04
522900010824     C                   EXFMT     TL44C04
523000951212     C*
523100951212     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
523200951212     C                   SETOFF                                       2890
523300951212     C                   SETOFF                                       404142
523400951212     C                   SETOFF                                       5051
523500951212     C                   SETOFF                                       4344
523600951212     C*
523700951212     C* F3 - FINE
523800951212     C* F12 - RITORNO
523900951212     C   KL
524000951212     COR KC              GOTO      ENDGTT
524100951212     C* F24-ALTRI TASTI
524200951212     C     *INKY         IFEQ      *ON
524300951212     C*
524400951212     C     WF            IFLT      WRFUNZ
524500951212     C                   ADD       1             WF
524600960322     C                   MOVEL     FN4N(WF)      V4CFUN
524700951212     C                   ELSE
524800951212     C                   Z-ADD     1             WF
524900960322     C                   MOVEL     FN4N(1)       V4CFUN
525000951212     C                   ENDIF
525100951212     C                   GOTO      FORS04
525200951212     C                   ENDIF
525300951212     C*
525400951212     C* F8 -VISUALIZZA GRIGLIA DEL TRAINO
525500951212     C* F14-VISUALIZZA ANAGRAFICA TRAINO
525600960322     C* F17-MODIFICA   ANAGRAFICA TRAINO
525700960322     C* F10-INTERROGA IL FOGLIO VIAGGIO
525800951212    1C     *INKH         IFEQ      *ON
525900951212     C     *INKN         OREQ      *ON
526000951214     C     *INKR         OREQ      *ON
526100960322     C     *INKJ         OREQ      *ON
526200951214     C                   Z-ADD     V4CPOS        KVPOS
526300951215     C                   EXSR      CALPG4
526400951212     C                   GOTO      FORS04
526500951212    1C                   ENDIF
526600951212     C*
526700951212     C* 25 ON - ROLLUP
526800951212     C     *IN25         IFEQ      *ON
526900951212     C* FINE LETTURA --> EMETTO MESSAGGIO FINE SCORRIMENTO
527000951212     C     *IN99         IFEQ      *ON                                          FINEL LETTUR
527100951214     C                   MOVEL     MSG(19)       VSCMSG
527200951212     C                   SETON                                        28
527300951212     C                   ELSE
527400951212     C*
527500951212     C                   Z-ADD     1             WTOADD
527600951212     C                   Z-ADD     0             L
527700951212     C                   EXSR      LEGS04                                       LETTURA
527800951214     C                   EXSR      POSSFL                                       POS CURSORE
527900951212     C                   ENDIF
528000951212     C                   GOTO      FORS04
528100951212     C                   ENDIF
528200951212     C*
528300960325     C* ENTER CONTROLLI
528400960325     C                   EXSR      CTRS04
528500960322     C                   GOTO      FORS04
528600951212     C*
528700951212     C     ENDGTT        ENDSR
528800951103     C**
528900010827      *----------------------------------------------------------------
529000010827      * CONVERTI - Routine conversione importo
529100010827      *----------------------------------------------------------------
529200010827     c     Sr_Converti   begsr
529300010827     c                   clear                   dscv
529400010913     c                   movel     'CV'          cod
529500020104     c                   movel(p)  tswdiv        key
529600010827     c     ktab          chain     tabel00f
529700010827     c                   if        %found(tabel00f)
529800010827     c                   movel     tbluni        dscv
529900010827     c                   endif
530000010827
530100010827     c                   clear                   yeurcods
530200010827     c                   movel     w_divisa      yecdvi
530300010827     c                   movel     tswdiv        yecdvo
530400010827     c                   z-add     w_importo     yecimi
530500010827     c                   move      §cvdec        yecdco
530600010827     c                   call      'YEURCO'
530700010827     c                   parm                    yeurcods
530800010827    5c                   if        yecesi  = ' '
530900010827     c                   z-add     yecimo        w_importo
531000010827    5c                   endif
531100010827
531200010827     c                   endsr
531300010827
531400950921     C****************  ROUTINE INIZIALE  *****************************
531500950921     C     *INZSR        BEGSR
531600950921     C     *ENTRY        PLIST
531700950921     C                   PARM                    KPJBA
531800960301     C                   MOVEL     KPJBU         DSTL34
531900950921     C*
532000950921     C                   Z-ADD     1             CODUT
532100950921     C                   CALL      'X§PARUT'
532200950921     C                   PARM                    UT§DSE
532300950921     C                   MOVEL     RAGUT         V1CRSU
532400951109     C* CANCELLO EVENTUALI RECORD RIMASTI SE PGM HA DATO ERRORE
532500960322     C                   CLEAR                   KSFL
532600951109     C                   EXSR      DELTSW
532700950922     C*
532800000807     C     KTNTLZ        KLIST
532900000807     C                   KFLD                    KTLTIP
533000000807     C                   KFLD                    KTLPDR
533100000807     C                   EVAL      KTLTIP = 'T '
533200951108     C     KTLL1         KLIST
533300951113     C                   KFLD                    V2HTRN
533400951108     C                   KFLD                    KDDE
533500960212     C                   KFLD                    KTMZ
533600960212     C                   KFLD                    W0070
533700960223     C     KTLR          KLIST
533800960223     C                   KFLD                    KVTRN
533900960223     C                   KFLD                    KDDE
534000960223     C                   KFLD                    KVNFP
534100960223     C     KTLR1         KLIST
534200960223     C                   KFLD                    KVTRN
534300960223     C                   KFLD                    KDDE
534400960223     C     KTLM          KLIST
534500960223     C                   KFLD                    KVTRN
534600960223     C                   KFLD                    KDDE
534700960223     C                   KFLD                    TLRFIL
534800960226     C                   KFLD                    TSRFFA
534900951214     C     KTS1          KLIST
535000951102     C                   KFLD                    KTRN
535100951102     C                   KFLD                    KDTN
535200951214     C     KTST11        KLIST
535300951103     C                   KFLD                    KVTRN
535400951103     C                   KFLD                    KVDTN
535500951113     C     KTST2         KLIST
535600951113     C                   KFLD                    KPDR
535700951113     C                   KFLD                    KDTN
535800951102     C     KTST4         KLIST
535900951102     C                   KFLD                    KTFP
536000951102     C                   KFLD                    KDTN
536100951113     C     KTST41        KLIST
536200951113     C                   KFLD                    KTFP
536300951113     C                   KFLD                    KDTN
536400951113     C                   KFLD                    KTRN
536500951106     C     KTSR11        KLIST
536600951113     C                   KFLD                    KTRN
536700951113     C                   KFLD                    KDTN
536800951113     C                   KFLD                    KNFP
536900951107     C     KTSR12        KLIST
537000951107     C                   KFLD                    KVTRN
537100951107     C                   KFLD                    KVDTN
537200951107     C                   KFLD                    KVNFP
537300960223     C     KTSR13        KLIST
537400960223     C                   KFLD                    KVTRN
537500960223     C                   KFLD                    KVDTN
537600960223     C     KTSR14        KLIST
537700960223     C                   KFLD                    KVTRN
537800960223     C                   KFLD                    KVDTN
537900960223     C                   KFLD                    KVNFP
538000960305     C                   KFLD                    TSRNFA
538100960223     C                   KFLD                    KVFFP
538200960223     C                   KFLD                    TSRFFA
538300951212     C     KTSR2         KLIST
538400951212     C                   KFLD                    KLNP
538500951212     C                   KFLD                    KDTT
538600951212     C     KTSR21        KLIST
538700951212     C                   KFLD                    KLNP
538800951212     C                   KFLD                    KDTT
538900951212     C                   KFLD                    KDTN
539000951212     C                   KFLD                    KTRN
539100951108     C     KTSR3         KLIST
539200951108     C                   KFLD                    TSTTRN
539300951108     C                   KFLD                    TSTDTN
539400951108     C                   KFLD                    TSTFES
539500960305     C     KTSR31        KLIST
539600960305     C                   KFLD                    KVTRN
539700960305     C                   KFLD                    KVDTN
539800990915     C                   KFLD                    TSRFFP
539900990915     C                   KFLD                    TSRNFP
540000951212     C     KTSR4         KLIST
540100951212     C                   KFLD                    KLNP
540200951212     C                   KFLD                    KNFV
540300000504     C     Kfws          KLIST
540400000504     C                   KFLD                    fvslnp
540500000504     C                   KFLD                    fvsnfv
540600960304     C     KFV2          KLIST
540700960304     C                   KFLD                    TSRLNP
540800960304     C                   KFLD                    TSRNFV
540900951103     C     KFV4          KLIST
541000951103     C                   KFLD                    V3CLNP
541100951103     C                   KFLD                    V3CNFV
541200951106     C                   KFLD                    KEPA
541300951106     C                   KFLD                    KFIL
541400960213     C                   KFLD                    KWTRC
541500951030     C     KTAB          KLIST
541600951030     C                   KFLD                    CODUT
541700951103     C                   KFLD                    COD
541800951103     C                   KFLD                    KEY
541900951106     C     KTSW1         KLIST
542000951106     C                   KFLD                    KNMEB
542100951106     C                   KFLD                    KNMUS
542200951106     C                   KFLD                    KNRJO
542300951106     C                   KFLD                    KSFL
542400951106     C                   KFLD                    WF20
542500951106     C     KTSW11        KLIST
542600951106     C                   KFLD                    KNMEB
542700951106     C                   KFLD                    KNMUS
542800951106     C                   KFLD                    KNRJO
542900951106     C                   KFLD                    KSFL
543000951106     C                   KFLD                    WF20
543100951106     C                   KFLD                    KNRR
543200951107     C     KTSW12        KLIST
543300951107     C                   KFLD                    KNMEB
543400951107     C                   KFLD                    KNMUS
543500951107     C                   KFLD                    KNRJO
543600951106     C     KTSW2         KLIST
543700951106     C                   KFLD                    KNMEB
543800951106     C                   KFLD                    KNMUS
543900951106     C                   KFLD                    KNRJO
544000951106     C                   KFLD                    KLNP
544100951106     C                   KFLD                    KNFV
544200960213     C                   KFLD                    KWTRC
544300960213     C     KTSW3         KLIST
544400960213     C                   KFLD                    KNMEB
544500960213     C                   KFLD                    KNMUS
544600960213     C                   KFLD                    KNRJO
544700960213     C                   KFLD                    KWTRN
544800960213     C                   KFLD                    KWDTN
544900960213     C                   KFLD                    KWTRC
545000960223     C     KTSW4         KLIST
545100960223     C                   KFLD                    KNMEB
545200960223     C                   KFLD                    KNMUS
545300960223     C                   KFLD                    KNRJO
545400960223     C                   KFLD                    KWTRC
545500960223     C                   KFLD                    KWFVR
545600960625     C     KTSM01        KLIST
545700960625     C                   KFLD                    KVTRN
545800960625     C                   KFLD                    KVDTN
545900960625     C                   KFLD                    V3CMTP
546000960625     C                   KFLD                    V3CMTA
546100960625     C                   KFLD                    V3CMTF
546200000218     C* LETTURA ANATB01L - PARZIALE
546300000218     C     KEATAB        KLIST
546400000218     C                   KFLD                    KATCOD
546500000218     C                   KFLD                    KATAZI
546600000218     C                   KFLD                    KATKEY
546700950921     C*
546800951103     C     *LIKE         DEFINE    FV4EPA        KEPA
546900960213     C     *LIKE         DEFINE    FV4TRC        KWTRC
547000960223     C     *LIKE         DEFINE    TSWFVR        KWFVR
547100960223     C     *LIKE         DEFINE    TLRTFM        WTFM
547200951103     C*
547300951103     C     *LIKE         DEFINE    TBLCOD        COD
547400951103     C     *LIKE         DEFINE    TBLKEY        KEY
547500951214     C*
547600960222     C     *LIKE         DEFINE    V2CSCE        CSCE
547700951214     C     *LIKE         DEFINE    TSRTRN        CTRN
547800951214     C     *LIKE         DEFINE    TSRDTN        CDTN
547900951214     C     *LIKE         DEFINE    TSRNFP        HNFP
548000951214     C     *LIKE         DEFINE    TSRFFP        CFFP
548100951214     C     *LIKE         DEFINE    V2CDTT        CDTT
548200951214     C     *LIKE         DEFINE    V2CNFV        HNFV
548300951214     C     *LIKE         DEFINE    V2CNFV        CNFV
548400951214     C     *LIKE         DEFINE    V2HRFV        HRFV
548500971204     C***        *LIKE     DEFN V2CRTP    WRTP
548600971204     C***        *LIKE     DEFN V2CRDT    WRDT
548700951106     C*
548800951106     C     *LIKE         DEFINE    TSWSFL        KSFL
548900951107     C     *LIKE         DEFINE    TSWFFP        KLNP
549000951113     C     *LIKE         DEFINE    TSWPDR        KPDR
549100951113     C     *LIKE         DEFINE    TSWPDR        WPDR
549200951108     C*
549300960213     C     *LIKE         DEFINE    FVSTMZ        KTMZ
549400960305     C     *LIKE         DEFINE    TSWNFV        SAVNFV
549500960305     C     *LIKE         DEFINE    TSWNFE        SAVNFE
549600960305     C     *LIKE         DEFINE    TSWNFP        SAVNFP
549700960305     C     *LIKE         DEFINE    TSWFFP        SAVFFP
549800960305     C     *LIKE         DEFINE    FVSPDR        SAVPDR
549900951124     C     *LIKE         DEFINE    FVSTMZ        SAVTMZ
550000951124     C     *LIKE         DEFINE    FVSIPT        SAVIPT
550100951124     C     *LIKE         DEFINE    TSTICO        SAVICO
550200951124     C     *LIKE         DEFINE    TLLILI        SAVILI
550300951124     C     *LIKE         DEFINE    TLLILI        WILI
550400951108     C     *LIKE         DEFINE    TLLDDE        KDDE
550500951115     C     *LIKE         DEFINE    TLLCAU        WCAU
550600951106     C*
550700951106     C     *LIKE         DEFINE    TSWNFV        KNFV
550800951106     C     *LIKE         DEFINE    TSWNFV        WNNFV
550900951213     C     *LIKE         DEFINE    TSRNFV        WONFV
551000951213     C*
551100951214     C     *LIKE         DEFINE    TSRNFP        WLNFP
551200951214     C     *LIKE         DEFINE    TSRTRN        WLTRN
551300951214     C     *LIKE         DEFINE    TSRDTN        WLDTN
551400951103     C*
551500951103     C     *LIKE         DEFINE    NRR2          WNRR2
551600951106     C     *LIKE         DEFINE    NRR2          KNRR
551700960222     C     *LIKE         DEFINE    NRR2          SAVNRR
551800951106     C     *LIKE         DEFINE    NRR2          WANRR
551900951116     C     *LIKE         DEFINE    NRR2          WNRRA
552000951116     C     *LIKE         DEFINE    NRR2          WNRRB
552100951116     C     *LIKE         DEFINE    NRR2          WNRRC
552200951213     C     *LIKE         DEFINE    NRR2          NRR4
552300951213     C     *LIKE         DEFINE    NRR2          WNRR4
552400951214     C     *LIKE         DEFINE    NRR2          WNRR
552500960625     C     *LIKE         DEFINE    NRR3          WNRR3
552600980114     C     *LIKE         DEFINE    NR13          WNR13
552700951106     C     *LIKE         DEFINE    G02TGI        WTOTGG
552800951103     C*
552900951103     C     *LIKE         DEFINE    TSTICO        WSICO
553000951103     C*
553100951214     C     *LIKE         DEFINE    V2CPOS        KVPOS
553200951214     C     *LIKE         DEFINE    V2CREC        WNREC
553300951214     C*
553400951103     C     *LIKE         DEFINE    TSTTFP        KFIL
553500951103     C     *LIKE         DEFINE    TSTTFP        KTFP
553600951106     C     *LIKE         DEFINE    TSTTFP        WCTFP
553700951106     C     *LIKE         DEFINE    TSTTFP        WSTFP
553800951107     C*
553900951113     C     *LIKE         DEFINE    TSRNFP        KNFP
554000951107     C     *LIKE         DEFINE    TSRNFP        KVNFP
554100960222     C     *LIKE         DEFINE    TSRNFP        WANFP
554200960222     C     *LIKE         DEFINE    TSRFFP        KVFFP
554300960325     C     *LIKE         DEFINE    TSRFFP        WCFFP
554400951103     C*
554500951102     C     *LIKE         DEFINE    TSTTRN        WTRN
554600951102     C     *LIKE         DEFINE    TSTTRN        KTRN
554700951103     C     *LIKE         DEFINE    TSTTRN        KVTRN
554800951106     C     *LIKE         DEFINE    TSTTRN        WATRN
554900951116     C     *LIKE         DEFINE    TSTTRN        WCTRN
555000960213     C     *LIKE         DEFINE    TSTTRN        KWTRN
555100951103     C*
555200960213     C     *LIKE         DEFINE    TSTDTN        KWDTN
555300960213     C     *LIKE         DEFINE    TSTDTN        KVDTN
555400951106     C     *LIKE         DEFINE    TSTDTN        WSDTN
555500951114     C     *LIKE         DEFINE    TSTDTN        WADTN
555600951103     C     *LIKE         DEFINE    TSTDTN        KDTN
555700951115     C     *LIKE         DEFINE    TSTDTN        AMGRDT
555800951115     C     *LIKE         DEFINE    TSTDTN        AMGTRD
555900951102     C     *LIKE         DEFINE    TSTDTN        AMGTRA
556000951103     C     *LIKE         DEFINE    TSTDTN        W0080
556100951103     C     *LIKE         DEFINE    TSTDTN        WDTAL
556200951103     C     *LIKE         DEFINE    TSTDTN        WDTDAL
556300951213     C     *LIKE         DEFINE    TSRDTT        AMGTTD
556400951213     C     *LIKE         DEFINE    TSRDTT        AMGTTA
556500951213     C     *LIKE         DEFINE    TSRDTT        KDTT
556600960325     C     *LIKE         DEFINE    TSRDTT        WCDTT
556700000218     C     *LIKE         DEFINE    ATBCOD        KATCOD
556800000218     C     *LIKE         DEFINE    ATBAZI        KATAZI
556900000218     C     *LIKE         DEFINE    ATBKEY        KATKEY
557000000807     C     *LIKE         DEFINE    TLZPDR        KTLPDR
557100000807     C     *LIKE         DEFINE    TLZTIP        KTLTIP
557200951106     C* AREA DATI -- > DATA ULTIMO CONTROLLO
557300951106     C     *DTAARA       DEFINE                  TNTL30
557400951124     C* RIGHE DI TASTI FUNZIONALI
557500951124     C                   Z-ADD     2             WRFUNZ            1 0
557600000214     C*--------------------
557700000214     C*---------- RICERCA DITTA :
557800000214     C*--------------------
557900000218     C**                 MOVEL     'SOC001'      TIPXSC
558000000218     C***                MOVEL     *BLANK        SOCXSC
558100000218     C***                EXSR      REPSOC
558200000218     C**   RTNXSC        IFNE      '1'
558300000218     C***                MOVEL     XSOCDS        SOC001
558400000218     C***                MOVEL     xscrgs        RSUT             20
558500000218     c***                end
558600000214     C*
558700950921     C                   ENDSR
558800000214     C*----------------------------------------------------*
558900000214     C* Reperimento dati società
559000000214     C*----------------------------------------------------*
559100000218     C***  REPSOC        BEGSR
559200000214     C*
559300000218     C***                CALLB     'XSOC'
559400000218     C***                PARM                    TIPXSC            6
559500000218     C***                PARM                    SOCXSC            3
559600000218     C***                PARM                    CDSXSC            9 0
559700000218     C***                PARM                    MODXSC            3
559800000218     C***                PARM      *blanks       RTNXSC            1
559900000218     C***                PARM                    XSOCDS
560000000218     C**                 PARM                    KPJBA
560100000214     C*
560200000218     C***                ENDSR
560300000218** MSG***
560400951215Immettere solo una delle due scelte : per data TRAINO o per data FV
560500951102Data TRAINO  errata                                                            2
560600951102"Data TRAINO dal"  maggiore  della "Data traino al"                            3
560700951215Data FV Teorica errata                                                         4
560800951215"Data FV Teorica dal"  maggiore  della "Data FV teorica al"                    5
560900951102Linea di Partenza Foglio Viaggio inesistente o annullata                       6
561000951102La linea specificata non e' una linea partenza FV                              7
561100951102Numero traino errato                                                           8
561200950922Terminal di partenza inesistente o annullato                                   9
561300950922La filiale specificata non è un Terminal                                      10
561400951102Per data TRAINO immettere o la linea partenza  o il numero traino             11
561500951106Data Traino obbligatoria
561600951215Data FV Teor. o numero FV obbligatorio se immessa la linea partenza F.V.      13
561700951102Il traino non esiste per le selezioni di date specificate                     14
561800951102Non esiste nulla da visualizzare per le selezioni immesse                     15
561900951109Griglia non manutenzionabile :manca il lancio dei controlli del xx/xx/xxxx    16
562000951103Impossibile spostarsi a SINISTRA: gia'visualizzati i primi dati a disposizione17
562100951103Impossibile spostarsi a DESTRA:gia'visualizzati gli ultimi dati a disposizione18
562200951103Fine scorrimento
562300951103Non trovati trazionisti per chiave alfabetica richiesta                       20
562400951124Codice TRAZIONISTA obbligatorio o inesistente in anagrafica
562500951114Impossibile sgrigliare il traino: gia' contabilizzato
562600960222Presenti"BUCHI"o imp.da Confermare diverso da listino o FV: enter x forzare   23
562700960223Impossibile ripristinare il traino: non esiste piu' l'anagrafica !!           24
562800951103Impossibile abbinare un F.Viaggio su un traino che si sta Sgrigliando         25
562900951113Se immesso il codice trazionista immettere "1" nella selezione                26
563000951113Causale contabilizzazione obbligatoria se immesso importo da confermare       27
563100951113Causale contabilizzazione errata
563200960223Numero foglio viaggio errato: inesistente o annullato                         29
563300951124Foglio viaggio xxxxx gia' abbinato a traino n.ro xxxxxxx del xx/xx/xxxx       30
563400951115Data errata                                                                   31
563500951215Il F.viaggio non ha come filiale di scarico la fermata arrivo XXX             32
563600951124Tasto funzionale non ammesso:posizionarsi prima sul traino da visualizzare    33
563700951124Non e' stata trovata l'anagrafica del traino da visualizzare!!                34
563800951127Se immesso numero FV immettere anche la linea di partenza                     35
563900960222Il Foglio Viaggio non e' abbinato!!                                           36
564000960322Impossibile visulizzare il foglio viaggio: numero mancante!!                  37
564100960322Opzione errata per questa riga: posizionarsi sulla riga giusta e ripremere F1038
564200960625Immettere una causale ritardo/anticipo esistente                              39
564300011009Non ammessi importi con decimali per la divisa indicata                       40
564400020315Traino non visual. completamente: avanzare di 1 pag. per permettere l'annull. 41
564500180103Immesso un importo da confermare minore di zero!!!                            42
564600951124**
564700960322F6=CONFERMA F8=VGriglia F14=InTraino F10=InFV F20=Destra
564800960322F3=Fine  F12=Ritorno                        F19=Sinistra
564900951124**
565000960322F6=CONFERMA  F8=VisGriglia  F14=IntTraino  F10=IntFV
565100960322F3=Fine  F12=Ritorno
565200951212**
565300960322M=ManGriglia  F12=Ritorno  F8=VisGriglia  F14=IntTraino  F10=IntFV  F3=Fine
565400960322
