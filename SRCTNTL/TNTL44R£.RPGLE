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
168800960213    3C     V2CICO        IFGT      0
168900951124     C     V2CPDR        ANDLE     *ZEROS
169000951214     C                   MOVEL     MSG(21)       VSCMSG
169100951124     C                   SETON                                        289041
169200960213    3C                   ENDIF
169300951103     C*
169400960213    3C     V2CPDR        IFNE      *BLANKS
169500951124     C     V2CPDR        ANDNE     *ZEROS
169600951103     C                   TESTN                   V2CPDR               31
169700951103     C                   MOVE      V2CPDR        W001A             1
169800960213    4C     *IN31         IFEQ      *OFF
169900951103     C     W001A         ORLT      '0'
170000951214     C                   MOVEL     MSG(21)       VSCMSG
170100951103     C                   SETON                                        289041
170200960213   X4C                   ELSE
170300951107     C* CONTROLLO SULL'ANAGRAFICA
170400951107     C                   MOVEL     V2CPDR        W0070             7 0
170500000807     C                   Z-ADD     W0070         KTLPDR
170600000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
170700960213    5C     *IN31         IFEQ      *ON
170800951107     C     TLZATB        ORNE      ' '
170900951214     C                   MOVEL     MSG(21)       VSCMSG
171000951107     C                   SETON                                        289041
171100960213   X5C                   ELSE
171200951107     C                   MOVEL     TLZRSC        V2DPDR
171300960213    5C                   ENDIF
171400960213    4C                   ENDIF
171500960221     C*
171600960221     C* SE MODIFICATO ICO --> DIVENTA CONFERMA MANUALE
171700960221    4C     V2CICO        IFGT      0
171800960221     C     V2CICO        ANDNE     V2HICO
171900960221     C                   MOVEL     '2'           V2HCNF
172000960221    4C                   ENDIF
172100960221     C**
172200951108     C* SE MODIFICATO VEDO SU ANAGRAFICA A RIPRENDERE L'IMPORTO LISTIN
172300960213    4C     V2HPDR        IFNE      V2CPDR
172400951108     C     V2CPDR        ANDGT     *ZEROS
172500951108     C                   EXSR      RICLIS
172600951115     C                   CLEAR                   WFORZ
172700951108     C                   MOVEL     V2CPDR        V2HPDR
172800011012      * devo calcolare la tolleranza
172900011012     c                   eval      sav_wili = wili
173000011012      * calcolo il delta e controllo la tolleranza
173100011012     c                   if        sav_wili <> v2cimp
173200011012     c                   eval      w_delta = (sav_wili - v2cimp)
173300011012      * il delta deve essere come valore assoluto
173400011012     c                   mllzo     1             w_delta
173500011012     c                   if        w_delta <= §gettr
173600011012     c                   eval      sav_wili = v2cimp
173700011012     c                   endif
173800011012     c                   endif
173900951115     C* SE NON HO MODIFICATO IL LISTINO, NON METTO IN RI
174000011012    5C     V2CIMP        IFEQ      sav_WILI
174100951116     C     V2CCAU        ANDEQ     WCAU
174200951116     C                   SETOFF                                       1790
174300960213    5C                   ENDIF
174400951115     C*
174500951108     C                   Z-ADD     WILI          V2CIMP
174600951115     C                   MOVEL     WCAU          V2CCAU
174700960221    4C                   ENDIF
174800960222    C**
174900960222     C* CONTROLLO SE POSSO CONFERMARE L'IMPORTO
175000960222     C                   EXSR      CONFIM
175100960221     C*
175200960221     C* SE CONFERMA AUTOMATICA --> SCONFERMO
175300960221    4C     WBUCO         IFNE      ' '
175400960221     C     V2HCNF        ANDEQ     '1'
175500960221     C     V2CICO        ANDGT     0
175600960221     C                   CLEAR                   V2CICO
175700960221     C                   SETON                                        19
175800960221    4C                   ENDIF
175900960222    3C                   ENDIF
176000951103     C*
176100951103     C* IMPORTO DA CONFERMARE
176200960222    3C  N90V2CICO        IFNE      V2HICO
176300951103     C                   CLEAR                   WFORZ
176400960221     C                   MOVEL     V2CICO        V2HICO
176500960221     C                   ELSE
176600960221     C                   MOVEL     'S'           WFORZ             1
176700960213    3C                   ENDIF
176800011012      * devo calcolare la tolleranza
176900011012     c                   eval      sav_v2cimp = v2cimp
177000011012     c                   eval      sav_v2hipt = v2hipt
177100011012      * calcolo il delta e controllo la tolleranza
177200011012     c                   if        sav_v2cimp <> v2cico
177300011012     c                   eval      w_delta = (sav_v2cimp - v2cico)
177400011012      * il delta deve essere come valore assoluto
177500011012     c                   mllzo     1             w_delta
177600011012     c                   if        w_delta <= §gettr
177700011012     c                   eval      sav_v2cimp = v2cico
177800011012     c                   endif
177900011012     c                   endif
178000011012      * calcolo il delta e controllo la tolleranza
178100011012     c                   if        sav_v2hipt <> v2cico
178200011012     c                   eval      w_delta = (sav_v2hipt - v2cico)
178300011012      * il delta deve essere come valore assoluto
178400011012     c                   mllzo     1             w_delta
178500011012     c                   if        w_delta <= §gettr
178600011012     c                   eval      sav_v2hipt = v2cico
178700011012     c                   endif
178800011012     c                   endif
178900951103     C*
179000960213    3C  N90WFORZ         IFEQ      ' '
179100951108     C     V2CICO        ANDGT     0
179200011012    4C     V2CICO        IFNE      sav_V2CIMP
179300011012     C     V2CICO        ANDNE     sav_V2HIPT
179400960222     C     WBUCO         ORNE      ' '
179500951214     C                   MOVEL     MSG(23)       VSCMSG
179600951103     C                   SETON                                        289040
179700960222    4C                   ENDIF
179800960222    3C                   ENDIF
179900011009      * aggancio la tabella CV
180000011009     c                   clear                   dscv
180100011009     c                   movel     'CV'          cod
180200020103     c                   movel(p)  v2cdiv        key
180300011009     c     ktab          chain     tabel00f
180400011009     c                   if        %found(tabel00f)
180500011009     c                   movel     tbluni        dscv
180600011009     c                   endif
180700011009      * controllo se ok i decimali
180800011009     c                   move      v2cico        w_decimali
180900011009     c                   if        w_decimali > 0  and
181000011009     c                             §cvfdc <> 'S'
181100011009     c                   seton                                        289040
181200011009     c                   movel     msg(40)       vscmsg
181300011009     c                   endif
181400951113     C*
181500951113     C* CONTROLLO CAUSALE CONTABILIZZAZIONE OBBLIGATORIA SE IMPORTO
181600960222    3C  N90V2CICO        IFGT      0
181700951113     C     V2CCAU        ANDEQ     ' '
181800951214     C                   MOVEL     MSG(27)       VSCMSG
181900951113     C                   SETON                                        289044
182000960213    3C                   ENDIF
182100951113     C* RICERCA
182200000218    3C**N90V2CCAU        IFEQ      '?'
182300000218     C**                 RESET                   XTABDS
182400000218     C**                 MOVE      *BLANK        XTAKEY
182500000218     C**                 MOVE      '01'          XTAOPZ
182600000218     C**                 MOVE      xscsoc        XTAAZI
182700000218     C**                 MOVE      XSCLIN        XTALIN
182800000218     C**                 CLEAR                   KPJBU
182900000218     C**                 MOVEL     XTABDS        KPJBU
183000000218     C**                 CALL      'ANGY4ZR'
183100000218     C**                 PARM                    KPJBA
183200000218     C**                 MOVEL     KPJBU         XTABDS
183300000218     C**                 MOVEL     XTAKEY        V2CCAU                         *CODICE
183400000218     C**                 SETON                                        9044
183500000218    3C**                 ENDIF
183600951113     C* CONTROLLO
183700960222    3C  N90V2CCAU        IFNE      ' '
183800000214     C* Reperisco i dati della causale di contabilizzazione
183900000218     C                   MOVE      KNSIF         WCOMO4            4
184000000218     C                   MOVEL     WCOMO4        WSOC              3
184100000218     C                   movel     WSOC          KAtazi
184200000218     C                   MOVEL     'Y4Z'         Katcod
184300000218     C                   MOVEL     *blanks       Katkey
184400000218     C                   MOVEL(P)  V2CCAU        Katkey
184500000218     C     KEATAB        CHAIN     ANATB01L                           30
184600060727     c                   if        not %found(anatb01l)
184700000218     C                   MOVE      '888'         Katazi
184800000218     C     KEATAB        CHAIN     ANATB01L                           30
184900060727     c                   end
185000000218     C                   if        not *in30
185100000218     C                   MOVEL     ATBUNI        ANGY4ZDS
185200000214     C                   end
185300000218     C                   if        *in30 or §4ZFLG<>' '
185400060727     c                             or §4ZTA <> 'T'
185500060727     C*                  MOVEL     '4Z'          COD
185600951214     C                   MOVEL     MSG(28)       VSCMSG
185700951113     C                   SETON                                        289044
185800960213    4C                   ENDIF
185900960213    3C                   ENDIF
186000951103     C*
186100951107     C**
186200960213     C* CONTROLLO SE C'E' ERRORE E AGGIORNO TNTSW
186300960213     C                   EXSR      ERRORE
186400960222    2C                   ENDIF
186500951117     C*
186600010824     C  N32              READC     TL44S02                                32
186700951103    1C                   ENDDO
186800951103     C*
186900951115     C     ENDCS2        ENDSR
187000960222     C**
187100960222     C************ CONTROLLO SE POSSO CONFERMARE L'IMPORTO ************
187200960222     C     CONFIM        BEGSR
187300011012      * devo calcolare la tolleranza
187400011012     c                   eval      sav_v2cimp = v2cimp
187500011012      * calcolo il delta e controllo la tolleranza
187600011012     c                   if        sav_v2cimp <> v2hipt
187700011012     c                   eval      w_delta = (sav_v2cimp - v2hipt)
187800011012      * il delta deve essere come valore assoluto
187900011012     c                   mllzo     1             w_delta
188000011012     c                   if        w_delta <= §gettr
188100011012     c                   eval      sav_v2cimp = v2hipt
188200011012     c                   endif
188300011012     c                   endif
188400960222     C* SE E' POSSIBILE PROPONGO L'IMPORTO CONFERMATO
188500960222    1C     V2CIMP        IFGT      0
188600011012     C     V2HIPT        ANDEQ     sav_V2CIMP
188700960222     C     V2CCAU        ANDNE     ' '
188800960222     C     V2CSCE        ANDEQ     ' '
188900960222     C* VERIFICARE SE CI SONO BUCHI
189000960222     C*  SOLO SE L'IMPORTO CONFERMATO ERA STATO CONTROLLATO
189100960222    2C     V2HPRI        IFEQ      ' '
189200960222     C                   EXSR      BUCHI
189300960222     C                   ELSE
189400960222     C                   MOVEL     '1'           WBUCO
189500960222    2C                   ENDIF
189600960222     C* NON CI SONO BUCHI
189700960222    2C     WBUCO         IFEQ      ' '
189800960222     C* SOLO SE LA PRECEDENTE CONFERMA E' AUTOMATICA
189900960222    3C     V2HCNF        IFEQ      '1'
190000960222     C* ACCENDO IL 19 SOLO SE E' CAMBIATO
190100960222     C     V2CICO        OREQ      0
190200960222     C     V2CICO        IFEQ      0
190300960222     C     V2CICO        ORNE      V2HICO
190400960222     C                   SETON                                        19
190500960222     C                   ENDIF
190600960222     C*
190700960222     C                   MOVEL     '1'           V2HCNF
190800960222     C                   Z-ADD     V2CIMP        V2CICO
190900960222     C                   Z-ADD     V2CIMP        V2HICO
191000960222    3C                   ENDIF
191100011012      * devo calcolare la tolleranza
191200011012     c                   eval      sav_v2cimp = v2cimp
191300011012      * calcolo il delta e controllo la tolleranza
191400011012     c                   if        sav_v2cimp <> v2cico
191500011012     c                   eval      w_delta = (sav_v2cimp - v2cico)
191600011012      * il delta deve essere come valore assoluto
191700011012     c                   mllzo     1             w_delta
191800011012     c                   if        w_delta <= §gettr
191900011012     c                   eval      sav_v2cimp = v2cico
192000011012     c                   endif
192100011012     c                   endif
192200960222     C* SE LA PRECEDENTE E' MANUALE LA TRASFORMO IN AUTOMATICA
192300960222    3C     V2HCNF        IFEQ      '2'
192400011012     C     V2CICO        ANDEQ     sav_V2CIMP
192500960222     C                   MOVEL     '1'           V2HCNF
192600960222    3C                   ENDIF
192700960222     C*
192800960222    2C                   ENDIF
192900960222   X1C                   ELSE
193000960306     C* VERIFICARE SE CI SONO BUCHI
193100960306     C*  SOLO SE L'IMPORTO CONFERMATO ERA STATO CONTROLLATO
193200960306    2C     V2HPRI        IFEQ      ' '
193300960306     C                   EXSR      BUCHI
193400960306     C                   ELSE
193500960306     C                   MOVEL     '1'           WBUCO
193600960306    2C                   ENDIF
193700960222    1C                   ENDIF
193800960222     C*
193900960222     C                   ENDSR
194000951214     C**
194100951214     C************** CONTROLLO NUMERO DI FV *************************
194200951214     C     CTRNFV        BEGSR
194300951214     C**
194400951214     C* RICERCA FOGLIO VIAGGIO DA ABBINARE
194500960213     C* SAE E' ANNULLATO PULISCO IL CAMPO DEL FV E NON FACCIO NULLA
194600960213    0C     CSCE          IFNE      'A'
194700960213     C*
194800951214     C     '?'           SCAN      CNFV                                   31
194900951214    1C     *IN31         IFEQ      *ON
195000010827     C                   CLEAR                   DSTL42
195100010827RM*  C                   Z-ADD     CTRN          D42TRN
195200010827RM*  C                   Z-ADD     CDTN          D42DTN
195300010827RM*  C                   Z-ADD     HNFP          D42NFP
195400010827     C                   MOVEL     'R'           D42MOD
195500010827     C                   MOVEL     DSTL42        KPJBU
195600010827     C                   CALL      'TNTL42R'
195700951214     C                   PARM                    KPJBA
195800010827     C                   MOVEL     KPJBU         DSTL42
195900010827     C                   MOVE      D42NFV        CNFV
196000960221    2C     CNFV          IFEQ      *ZEROS
196100960221     C                   CLEAR                   CNFV
196200960221    2C                   ENDIF
196300960221    1C                   ENDIF
196400960213     C*
196500951214     C* CONTROLLO FV
196600960213    1C     CNFV          IFNE      *ZEROS
196700951214     C     CNFV          ANDNE     *BLANKS
196800951214     C     CNFV          ANDNE     HNFV
196900951214     C                   TESTN                   CNFV                 31
197000951214     C                   MOVE      CNFV          W001A             1
197100951214     C*
197200951214    2C     *IN31         IFEQ      *OFF
197300951214     C     W001A         ORLT      '0'
197400951214     C                   MOVEL     MSG(29)       VSCMSG
197500951214     C                   SETON                                        289042
197600951214   X2C                   ELSE
197700951214     C*
197800951214     C                   MOVEL     CNFV          KNFV
197900951214     C                   Z-ADD     CFFP          KLNP
198000960223     C     KTSR4         CHAIN(N)  TNFVS01L                           30
198100951214    3C     *IN30         IFEQ      *ON
198200960226     C     FVSATB        ORNE      ' '
198300951214     C                   MOVEL     MSG(29)       VSCMSG
198400951214     C                   SETON                                        289042
198500951214   X3C                   ELSE
198600951214     C*
198700951214     C* CONTROLLO PRIMA IN TNTSW00F SE E' ABBINATO O SBINATO
198800960213     C                   MOVEL     'A'           KWTRC
198900951214     C     KTSW2         CHAIN(N)  TNTSW02L                           30
199000951214    4C     *IN30         IFEQ      *OFF
199100951214     C                   MOVEL     TSWTRN        W007A
199200951214     C                   MOVEL     TSWDTN        W008A             8
199300951214     C                   EXSR      PREMSG
199400951214     C                   SETON                                        289042
199500951214   X4C                   ELSE
199600951214     C* SE E' SBINATO --> OK
199700960213     C                   MOVEL     'S'           KWTRC
199800951214     C     KTSW2         SETLL     TNTSW02L                               30
199900951214    5C     *IN30         IFEQ      *OFF
200000951214     C* SE NON L'HO TROVATO IN TNTSW --> VEDO DAL FV
200100951214    6C     FVSTRS        IFNE      0
200200951214     C* CHAIN SU TNTSR PER PRENDERE LA DATA TRAINO
200300960223     C     KTSR4         CHAIN(N)  TNTSR04L                           30
200400951214     C  N30              MOVEL     TSRTRN        W007A
200500951214     C  N30              MOVEL     TSRDTN        W008A
200600951214     C   30              MOVEL     *ALL'9'       W007A
200700951214     C   30              CLEAR                   W008A
200800951214     C                   EXSR      PREMSG
200900951214     C                   SETON                                        289042
201000951214    6C                   ENDIF
201100951214     C*
201200951214    5C                   ENDIF
201300951214     C* SE FV OK VERIFICO SE HA ALMENO UNA LINEA DI SCARICO =FERMATA
201400951215     C*  ARRIVO FVT
201500951214    5C     *IN28         IFEQ      *OFF
201600951214     C                   Z-ADD     CTRN          KTRN
201700951214     C                   Z-ADD     CDTN          KDTN
201800951214     C                   Z-ADD     HNFP          KNFP
201900951214     C                   SETOFF                                       30
202000980121     C***        KTSR11    CHAINTNTSR01L             31
202100980121     C     KTSR11        CHAIN(N)  TNTSR01L                           31
202200951214    6C     *IN31         DOWEQ     *OFF
202300951214     C     *IN30         ANDEQ     *OFF
202400951214     C                   MOVEL     TSRFFA        W003A             3
202500000504     C     Kfws          CHAIN     TNFWS01L                           39
202600000504     c                   if        *in39 or fwsatb<>' '
202700000504     c                   clear                   fwsff3
202800000504     c                   clear                   fwsff4
202900000504     c                   clear                   fwsfl3
203000000504     c                   clear                   fwsfl4
203100010910
203200010910     c                   clear                   fwsipt
203300010910     c                   clear                   fwsies
203400010910     c                   clear                   fwsvip
203500010910     c                   clear                   fwsves
203600011016     c                   endif
203700010907      * se l'importo dell'estensione è = 0 e l'importo sul file
203800010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
203900010907      * la divisa è lire
204000010907     c                   if        fwsipt = *zeros and fvsipt <> 0
204100010907     c                   eval      fwsvip = 'ITL'
204200010911     c                   z-add     fvsipt        fwsipt
204300010907     c                   endif
204400010907     c                   if        fwsies = *zeros and fvsies <> 0
204500010907     c                   eval      fwsves = 'ITL'
204600010911     c                   z-add     fvsies        fwsies
204700010907     c                   endif
204800010907      * se l'importo dell'estensione è = 0 e l'importo sul file
204900010907      * principale è = 0, imposto la divisa a blank per poi prendere quella della
205000010907      * griglia, così non sto a convertire un importo a 0
205100010907     c                   if        fwsipt = *zeros and fvsipt = 0
205200010907     c                   clear                   fwsvip
205300010911     c                   z-add     fvsipt        fwsipt
205400010907     c                   endif
205500010907     c                   if        fwsies = *zeros and fvsies = 0
205600010907     c                   clear                   fwsves
205700010911     c                   z-add     fvsies        fwsies
205800010907     c                   endif
205900000504     c
206000951214     C     W003A         LOOKUP    FLP                                    30
206100951214     C  N30FVSLNA        COMP      TSRFFA                                 30
206200980121     C**N30      KTSR11    READETNTSR01L                 31
206300980121     C  N30KTSR11        READE(N)  TNTSR01L                               31
206400951214    6C                   ENDDO
206500951214     C*
206600951214    6C     *IN31         IFEQ      *ON
206700951214     C                   MOVEA     MSG(32)       K78
206800951215     C                   MOVEA     W003A         K78(63)
206900951214     C                   MOVEA     K78           VSCMSG
207000951214     C                   SETON                                        289042
207100951214    6C                   ENDIF
207200951214    5C                   ENDIF
207300951214     C*
207400951214    5C     *IN28         IFEQ      *OFF
207500951215     C* VERIFICO SE FV ABBINATO HA DATA = DATA FVT
207600951214     C                   MOVEL     FVSDFV        WDATA
207700951214     C                   MOVEL     WGG           W0040             4 0
207800951214     C                   MOVE      WMM           W0040
207900951214    6C  N28W0040         IFNE      CDTT
208000951214     C                   MOVEL     'S'           HRFV
208100951214     C                   ELSE
208200951214     C                   CLEAR                   HRFV
208300951214    6C                   ENDIF
208400951214     C*
208500951214    5C                   ENDIF
208600951214    4C                   ENDIF
208700951214    3C                   ENDIF
208800951214    2C                   ENDIF
208900951214    1C                   ENDIF
209000960213     C*
209100960213     C                   ENDIF
209200951214     C*
209300951214     C                   ENDSR
209400960213     C**
209500960213     C************** CONTROLLO SE CI SONO BUCHI NEL TRAINO **********
209600960213     C     BUCHI         BEGSR
209700960213     C                   CLEAR                   WBUCO
209800960213     C* LETTURA IN TNTSW03L
209900960213     C                   Z-ADD     V2HTRN        KWTRN
210000960213     C                   Z-ADD     V2HDTN        KWDTN
210100960213     C                   MOVEL     'R'           KWTRC
210200960213     C     KTSW3         SETLL     TNTSW03L
210300960223     C     KTSW3         READE(N)  TNTSW03L                               30
210400960213    1C     *IN30         DOWEQ     *OFF
210500960213     C*
210600960213     C* SE ANNULLATO NON ABBINO SE E' QUELLA CHE ESPONE
210700960213    2C     TSWATV        IFNE      ' '
210800960213    3C     TSWNFE        IFEQ      TSWNFP
210900960213     C                   MOVEL     '1'           WBUCO             1
211000960213    3C                   ENDIF
211100960213     C*
211200960213   X2C                   ELSE
211300960213     C* SE TROVO ALMENO UNA TRATTA NON ABBINATA : '1' IN WBUCO
211400960213    3C     TSWVNF        IFEQ      0
211500960213     C                   MOVEL     '1'           WBUCO             1
211600960213     C                   SETON                                            30
211700960213    3C                   ENDIF
211800960213    2C                   ENDIF
211900960213     C*
212000960223     C  N30KTSW3         READE(N)  TNTSW03L                               30
212100960213    1C                   ENDDO
212200960213     C                   ENDSR
212300960213     C**
212400960213     C****** VEDO SE C'E' ERRORE E AGGIORNO TNTSW *******************
212500960213     C     ERRORE        BEGSR
212600960213     C*
212700960213     C   90              SETON                                        2232
212800960213     C   90              Z-ADD     NRR2          V2CREC
212900960213     C*
213000960213     C                   EXSR      AGGTSW
213100960213     C**
213200960213     C**
213300960213     C* SE TUTTE LE TRATTE DEL TRAINO SONO ANNULLATE, METTO LA "A"
213400960213     C* NELLA TESTATA
213500960213    2C  N90V2HTRC        IFEQ      'R'
213600960213     C     V2CSCE        ANDEQ     'A'
213700960213     C                   CLEAR                   WTTANN
213800960213     C                   MOVEL     V2HTRN        KWTRN
213900960213     C                   MOVEL     V2HDTN        KWDTN
214000960213     C                   MOVEL     'R'           KWTRC
214100960213     C     KTSW3         SETGT     TNTSW03L
214200960223     C     KTSW3         READPE(N) TNTSW03L                               31
214300960213    3C     *IN31         DOWEQ     *OFF
214400960213     C* SE NON E' ANNULLATA MEMORIZZO
214500960213    4C     TSWATV        IFEQ      ' '
214600960213     C                   MOVEL     'N'           WTTANN            1
214700960213     C                   SETON                                        31
214800960213    4C                   ENDIF
214900960213     C*
215000960223     C  N31KTSW3         READPE(N) TNTSW03L                               31
215100960213    3C                   ENDDO
215200960213     C*
215300960213     C* SE TUTTE ANNULLATE METTO LA "A" SULLA TESTATA
215400960213    3C     WTTANN        IFEQ      ' '
215500960213     C                   MOVEL     'T'           KWTRC
215600960223     C     KTSW3         CHAIN(N)  TNTSW03L                           31
215700960213     C* PRENDO IL NUMERO DI RECORD
215800010824     C     TSWNRR        CHAIN     TL44S02                            31
215900960213     C                   EXSR      CTRIND
216000960213     C  N31              MOVEL     'A'           V2CSCE
216100960213     C  N31              SETON                                        22
216200010824     C  N31              UPDATE    TL44S02
216300960213     C                   SETOFF                                       22
216400960213    3C                   ENDIF
216500960222     C*
216600960213   X2C                   ELSE
216700960213     C**
216800960213     C* CONTROLLO L'ANNULLAMENTO DEL TRAINO E/O TRATTA
216900960213    3C  N90V2HTRC        IFEQ      'T'
217000960213     C     SAVATB        ANDNE     V2CSCE
217100960213     C*
217200960222     C* MI SALVO IL NUMERO RELATIVO DI RECORD
217300960222     C                   Z-ADD     NRR2          SAVNRR
217400960222     C*
217500960213    4C     V2CSCE        IFEQ      'A'
217600960221     C                   MOVEL     'A'           WANN              1
217700960213     C                   ELSE
217800960221     C                   CLEAR                   WANN
217900960213    4C                   ENDIF
218000960213     C*
218100960222     C                   EXSR      ANNRIP
218200960222     C*
218300960222     C                   Z-ADD     SAVNRR        NRR2
218400010824     C     NRR2          CHAIN     TL44S02                            31
218500960222     C* SE HO RIPRISTINATO
218600960222     C*  CHAIN SULLA TESTATA PER VEDERE SE POSSO RICONFERMARE L'IMPORT
218700960222    4C     V2CSCE        IFEQ      ' '
218800960222     C                   EXSR      CONFIM
218900960222    5C     V2CICO        IFGT      0
219000960222     C                   EXSR      AGGTSW
219100960222    5C                   ENDIF
219200960222     C*
219300960222   X4C                   ELSE
219400960222     C* SE HO ANNULLATO
219500960222     C*   SCONFERMO L'IMPORTO
219600960222     C                   CLEAR                   V2CICO
219700960222     C                   EXSR      AGGTSW
219800960222    4C                   ENDIF
219900960222     C*
220000960222    3C                   ENDIF
220100960213    2C                   ENDIF
220200960213     C*
220300960213     C* ABBINAMENTO DI 1 FV: VEDO SE CONFERMARE L'IMPORTO
220400960222     C  N90WABBFV        IFEQ      'A'
220500960213     C                   EXSR      ABBIN
220600960222     C                   ENDIF
220700960222     C* SBIAMENTO DI 1 FV: VEDO SE SCONFERMARE
220800960222     C  N90WABBFV        IFEQ      'S'
220900960222     C                   EXSR      SBINAM
221000960222     C                   ENDIF
221100960213     C                   CLEAR                   WABBFV
221200960213     C                   SETOFF                                       221719
221300960213     C*
221400960213     C                   ENDSR
221500960213     C**
221600960213     C****** SE NON C'E' ERRORE AGGIORNO TNTSW **********************
221700960213     C     AGGTSW        BEGSR
221800960213     C*
221900960213    0C     *IN90         IFEQ      *OFF
222000960213     C                   Z-ADD     NRR2          KNRR
222100960213     C     KTSW11        CHAIN     TNTSW01L                           31
222200960213    1C     TSWTRC        IFEQ      'T'                                          TRAINO
222300960213     C                   MOVEL     TSWATV        SAVATB            1
222400960213     C*
222500960222     C     V2CICO        IFEQ      0
222600960222     C                   CLEAR                   V2HCNF
222700960222     C                   ENDIF
222800960222     C                   Z-ADD     V2CICO        TSWVIC
222900960222     C                   MOVEL     V2HCNF        TSWCNF
223000960213     C                   MOVEL     V2CPDR        TSWVPD
223100960213     C                   MOVEL     V2DPDR        TSWDPD
223200960213     C                   MOVEL     V2CSCE        TSWATV
223300960223     C                   MOVEL     V2CSCE        TSWATT
223400010910     C**!!!              MOVEL     V2CIMP        TSWILI
223500010910     C                   z-add     V2CIMP        TSWILI
223600960213     C                   MOVEL     V2CCAU        TSWVCU
223700960213     C* SOLO SE HO VARIATO QUALCHE COSA METTO LA "S" DI VARIATO RECORD
223800960213    2C     TSWVIC        IFNE      TSWICO
223900960213     C     TSWVPD        ORNE      TSWPDR
224000960213     C     TSWVCU        ORNE      TSWCAU
224100960222     C     TSWATV        ORNE      TSWATB
224200960213     C                   MOVEL     'S'           TSWFVR
224300960213   X2C                   ELSE
224400960213     C                   MOVEL     ' '           TSWFVR
224500960213    2C                   ENDIF
224600960213     C*
224700960213   X1C                   ELSE                                                   FVT
224800960213     C*
224900960213     C* SE ERA ANNULLATO E RIMANE ANNULLATO : NON FACCIO NULLA
225000960213    2C     TSWATV        IFNE      V2CSCE
225100960213     C     V2CSCE        OREQ      ' '
225200960213     C*
225300960213     C* SE ANNULLATO E' COME SE IL FV FOSSE STATO TOLTO
225400960213    3C     V2CSCE        IFEQ      'A'
225500960213     C                   CLEAR                   V2CNFV
225600960213     C* SE ERA ANNULLATO E L'HO RIATTIVATO: IMPOSTO IL FV CHE C'ERA
225700960213     C* NEL FILE ORIGINALE
225800960213   X3C                   ELSE
225900960213    4C     TSWATV        IFEQ      'A'
226000960213     C     TSWNFV        ANDGT     0
226100960213     C                   MOVEL     TSWRFI        V2HRFV
226200960213     C                   MOVEL     TSWNFV        V2CNFV
226300960222     C                   MOVEL     'A'           WABBFV            1
226400960213    4C                   ENDIF
226500960213    3C                   ENDIF
226600960213     C*
226700960213     C                   MOVEL     V2CSCE        TSWATV
226800960213     C                   MOVEL     V2HATT        TSWATT
226900960213     C                   MOVEL     V2CNFV        TSWVNF
227000960213    3C     TSWVNF        IFNE      TSWNFV
227100960222    3C     TSWATV        ORNE      TSWATB
227200960213     C                   MOVEL     'S'           TSWFVR
227300960213     C                   ELSE
227400960213     C                   MOVEL     ' '           TSWFVR
227500960213    3C                   ENDIF
227600960213     C*
227700960213    3C     V2CNFV        IFLE      *ZEROS
227800960213     C                   MOVEL     'S'           TSWRFV
227900960213     C                   ELSE
228000960213     C                   CLEAR                   TSWRFV
228100960213     C* CI RIMUOVO QUELLO CHE C'ERA NEL SFL, CHE MAGARI E'CAMBIATO PER
228200960213     C*  CAMBIO NUMERO DI FV
228300960213     C                   MOVEL     V2HRFV        TSWRFV
228400960213    3C                   ENDIF
228500960213     C*
228600960213     C                   MOVEL     TSWRFV        V2HRFV
228700960213    2C                   ENDIF
228800960213    1C                   ENDIF
228900960213     C*
229000960213     C                   UPDATE    TNTSW000
229100960213     C*
229200960213     C*
229300960213     C* SE HO ANNULLATO O RIABBINATO --> MEMORIZZO
229400960213    1C     V2HTRC        IFEQ      'R'
229500960213     C     V2CNFV        ANDNE     V2HNFV
229600960213     C*
229700960213     C                   MOVEL     V2HNFV        WONFV
229800960213    2C     V2CNFV        IFGT      *ZEROS
229900960213     C                   MOVEL     V2CNFV        WNNFV
230000960213     C                   Z-ADD     V2HDTN        WADTN
230100960213     C                   Z-ADD     V2HTRN        WATRN
230200960222     C                   MOVEL     'A'           WABBFV
230300960213   X2C                   ELSE
230400960213     C                   CLEAR                   WNNFV
230500960222     C                   MOVEL     'S'           WABBFV
230600960213    2C                   ENDIF
230700960213     C                   MOVE      V2CFFP        KLNP
230800960222     C                   MOVE      V2HNFP        WANFP
230900960213     C*
231000960213     C                   EXSR      UPTSW
231100960213     C*
231200960213     C* SE NON' C'E' ERRORE --> MEMORIZZO IL NUOVO FV
231300960213     C                   MOVEL     V2CNFV        V2HNFV
231400960213    2C     V2HNFV        IFLE      *ZEROS
231500960213     C                   MOVEL     *ZEROS        V2HNFV
231600960213    2C                   ENDIF
231700960213     C*
231800960213    1C                   ENDIF
231900960213    0C                   ENDIF
232000960213     C*
232100960213     C* SISTEMO GLI INDICATORI
232200960213     C                   EXSR      CTRIND
232300960213     C*
232400010824     C                   UPDATE    TL44S02
232500960213     C                   ENDSR
232600960213     C**
232700960221     C****** AGGIORNO NEL SFL E IN TSW LE TRATTE IN BASE ALLA TESTA****
232800960222     C     ANNRIP        BEGSR
232900960213     C                   Z-ADD     V2HTRN        KWTRN
233000960213     C                   Z-ADD     V2HDTN        KWDTN
233100960213     C                   MOVEL     'R'           KWTRC
233200960223     C     KTSW3         CHAIN(N)  TNTSW03L                           31
233300960213     C     *IN31         DOWEQ     *OFF
233400960213     C* CHAIN SUL SFL
233500010824     C     TSWNRR        CHAIN     TL44S02                            32
233600960213     C     *IN32         IFEQ      *OFF
233700960213     C*
233800960221     C     WANN          IFEQ      'A'
233900960213     C                   MOVEL     'A'           V2CSCE
234000960213     C                   MOVEL     'A'           V2HATT
234100960213     C                   ELSE
234200960213     C                   CLEAR                   V2CSCE
234300960213     C                   CLEAR                   V2HATT
234400960213     C                   ENDIF
234500960213     C* FACCIO GIRARE L'AGGIORNAMENTO
234600960213     C                   EXSR      AGGTSW
234700960213     C                   ENDIF
234800960213     C*
234900960222     C                   MOVEL     'R'           KWTRC
235000960223     C     KTSW3         READE(N)  TNTSW03L                               31
235100960213     C                   ENDDO
235200960222     C*
235300960222     C* NON DEVO ANDARE IN NESSUN ABBINAMENTO E/O SBINAMENTO
235400960222     C                   CLEAR                   WABBFV
235500960213     C                   ENDSR
235600960222     C**
235700960223     C********** AGGIORNAMENTO GRIGLIA ********************************
235800960222     C     REGIS2        BEGSR
235900960222     C                   CLEAR                   WF20
236000960222     C                   CLEAR                   KNRR
236100960223     C*
236200960223     C* PRIMA LEGGO LE TESTATE TRAINI ANNULLATE O RIPRISTINATE
236300960223     C                   MOVEL     'T'           KWTRC
236400960223     C                   MOVEL     'S'           KWFVR
236500960223     C     KTSW4         SETLL     TNTSW04L
236600960223     C     KTSW4         READE     TNTSW04L                               31
236700960222     C*
236800960222    1C     *IN31         DOWEQ     *OFF
236900960223     C*
237000960226    2C     TSWATV        IFNE      TSWATB
237100960223     C                   MOVEL     TSWTRN        KVTRN
237200960223     C                   MOVEL     TSWDTN        KVDTN
237300960223     C***
237400960223     C* ANNULLAMENTO DI UN TRAINO
237500960223     C***
237600960226    3C     TSWATV        IFEQ      'A'
237700960223     C                   EXSR      ANNTRN
237800980206     C                   ELSE
237900960223     C**
238000960223     C** RIPRISTINO DI UNA GRIGLIA
238100960223     C**
238200980206    4C     TSWATV        IFEQ      ' '
238300960223     C                   EXSR      RIPTRN
238400980206    4C                   ENDIF
238500980206    3C                   ENDIF
238600960226    2C                   ENDIF
238700960223     C*
238800960223     C                   MOVEL     'T'           KWTRC
238900960223     C     KTSW4         READE     TNTSW04L                               31
239000960223    1C                   ENDDO
239100960222     C**
239200960223     C** LEGGO I RIPRISTINI DI TRATTA
239300960223     C**
239400960223     C                   MOVEL     ' '           WATB              1
239500960223     C                   EXSR      ARTRA
239600960223     C**
239700960223     C* LEGGO GLI ANNULLAMENTI DI TRATTA
239800960223     C**
239900960223     C                   MOVEL     'A'           WATB              1
240000960223     C                   EXSR      ARTRA
240100960223     C**
240200960223     C* LEGGO LE TRATTE VARIATE RIMASTE
240300960223     C**
240400960223     C                   MOVEL     'R'           KWTRC
240500960223     C     KTSW4         SETLL     TNTSW04L
240600960223     C     KTSW4         READE     TNTSW04L                               31
240700960223     C*
240800960223    1C     *IN31         DOWEQ     *OFF
240900960223     C*
241000960223    2C     TSWVNF        IFEQ      0
241100960305     C     TSWVNF        ANDNE     TSWNFV
241200960223     C* SBINO IL FV
241300960223     C                   MOVEL     '4'           WAGTSR            1
241400960223     C                   MOVEL     TSWNFV        KNFV
241500960223     C                   MOVEL     TSWFFP        KLNP
241600960223     C     KTSR4         SETLL     TNTSR04L
241700960223     C     KTSR4         READE     TNTSR04L                               32
241800960223    3C     *IN32         DOWEQ     *OFF
241900960223     C* SBINAMENTO TRATTA
242000960223     C                   EXSR      SBITSR
242100960223     C*
242200960223     C     KTSR4         READE     TNTSR04L                               32
242300960223    3C                   ENDDO
242400960223     C* SBINAMENTO FV
242500960223     C                   EXSR      SBIFVS
242600960223    2C                   ENDIF
242700960223     C**
242800960223    2C     TSWVNF        IFGT      0
242900960305     C     TSWVNF        ANDNE     TSWNFV
243000960223     C* ABBINO IL FV
243100960226     C                   MOVE      ' '           WVOLTA            1
243200960226     C                   MOVEL     TSWDTN        KVDTN
243300960226     C                   MOVEL     TSWTRN        KVTRN
243400960226     C                   MOVEL     TSWNFP        KVNFP
243500960226     C     KTSR12        SETLL     TNTSR01L
243600960226     C     KTSR12        READE     TNTSR01L                               32
243700960226     C     *IN32         DOWEQ     *OFF
243800960226     C                   EXSR      ABBTSR
243900960226     C*
244000960226     C     KTSR12        READE     TNTSR01L                               32
244100960226     C                   ENDDO
244200020109     C* SBINAMENTO FV vecchio
244300020110     c     tswnfv        ifgt      0
244400020109     C                   MOVEL     TSWNFV        KNFV
244500020109     C                   MOVEL     TSWFFP        KLNP
244600020109     C                   EXSR      SBIFVS
244700020110     c                   endif
244800960223     C                   ENDIF
244900960223     C*
245000960223     C* AGGIORNO TSW
245100960223     C                   MOVEL     TSWVNF        TSWNFV
245200960223     C                   MOVEL     TSWRFV        TSWRFI
245300960223     C                   CLEAR                   TSWFVR
245400960223     C                   UPDATE    TNTSW004
245500960223     C**
245600960223     C     KTSW4         READE     TNTSW04L                               31
245700960223    2C                   ENDDO
245800960226     C**
245900960226     C* LEGGO LE TESTATE
246000960226     C**
246100960226     C                   MOVEL     'T'           KWTRC
246200960226     C     KTSW4         SETLL     TNTSW04L
246300960226     C     KTSW4         READE     TNTSW04L                               31
246400960226     C*
246500960226    1C     *IN31         DOWEQ     *OFF
246600960226     C* CHAIN SULLA TESTATA
246700960226     C                   MOVEL     TSWDTN        KVDTN
246800960226     C                   MOVEL     TSWTRN        KVTRN
246900960226     C     KTSR13        CHAIN     TNTST01L                           32
247000960226     C                   EXSR      UPTST
247100960226     C**
247200960226     C* AGGIORNO TSW
247300960226     C                   MOVEL     TSWVCU        TSWCAU
247400960226     C                   MOVEL     TSWVPD        TSWPDR
247500960226     C                   MOVEL     TSWVIC        TSWICO
247600960226     C                   CLEAR                   TSWFVR
247700960229     C                   UPDATE    TNTSW004
247800960229     C*
247900960229     C* CHAIN TSR  IL RECORD CON L'IMPORTO DA CONFERMARE
248000960229     C                   CLEAR                   WVOLTA
248100960229     C                   Z-ADD     TSTNFE        KVNFP
248200960229     C     KTSR12        SETLL     TNTSR01L
248300960229     C     KTSR12        READE     TNTSR01L                               30
248400960229    2C     *IN30         DOWEQ     *OFF
248500960229     C*
248600960229     C* IL PRIMO RECORD COSI', GLIALTRI SEMPRE 9999999
248700960229     C     WVOLTA        IFEQ      ' '
248800960229     C                   MOVEL     '1'           WVOLTA
248900960229     C*
249000960229    3C     TSTICO        IFGT      0
249100960229    4C     TSTCNF        IFEQ      '1'
249200960229     C                   MOVEL     *ALL'9'       TSRDKI
249300960305     C                   MOVEL     'S'           TSRFKI
249400960229     C                   ELSE
249500960229     C                   MOVEL     TSRDTT        TSRDKI
249600960305     C                   MOVEL     'S'           TSRFKI
249700960229    4C                   ENDIF
249800960229   X3C                   ELSE
249900960229     C**
250000960229    4C     TSRDKI        IFEQ      *HIVAL
250100960229     C                   CLEAR                   TSRDKI
250200960229    4C                   ENDIF
250300960229    3C                   ENDIF
250400960229     C**
250500960229   X2C                   ELSE
250600960229     C                   MOVEL     *ALL'9'       TSRDKI
250700960305     C                   MOVEL     'S'           TSRFKI
250800960229    2C                   ENDIF
250900960229     C**
251000960229     C                   UPDATE    TNTSR000
251100960229     C     KTSR12        READE     TNTSR01L                               30
251200960229    1C                   ENDDO
251300960229     C**
251400960226     C     KTSW4         READE     TNTSW04L                               31
251500960226    2C                   ENDDO
251600960226     C**
251700960222     C                   ENDSR
251800960223     C**
251900960223     C************** ANNULLAMENTO DI UN TRAINO ************************
252000960223     C     ANNTRN        BEGSR
252100960223     C                   MOVEL     'A'           TSWATB
252200960223     C                   CLEAR                   TSWFVR
252300960223     C                   UPDATE    TNTSW004
252400960223     C**
252500960223     C* CHAIN SULLA TESTATA E AGGIORNO
252600960223     C     KTSR13        CHAIN     TNTST01L                           32
252700960223    1C     *IN32         IFEQ      *OFF
252800960223     C                   EXSR      UPTST
252900960304     C**
253000960304     C* LETTURA E ANNULLAMENTO  DEI MC
253100960229     C     KTSR13        SETLL     TNTSM01L
253200960229    2C     KTSR13        READE     TNTSM01L                               30
253300960229     C     *IN30         DOWEQ     *OFF
253400960304     C                   MOVEL     'A'           TSMATB
253500960304     C                   UPDATE    TNTSM000
253600960304     C*
253700960229     C     KTSR13        READE     TNTSM01L                               30
253800960229    2C                   ENDDO
253900960304     C**
254000960223     C* LEGGO LE TRATTE E LE SBINO
254100960223     C                   MOVEL     'R'           KWTRC
254200960223     C                   MOVEL     TSWTRN        KWTRN
254300960223     C                   MOVEL     TSWDTN        KWDTN
254400960223     C     KTSW3         SETLL     TNTSW03L
254500960223     C     KTSW3         READE     TNTSW03L                               32
254600960229     C*
254700960223    2C     *IN32         DOWEQ     *OFF
254800960223     C                   MOVEL     TSWNFP        KVNFP
254900960223     C     KTSR12        SETLL     TNTSR01L
255000960223     C     KTSR12        READE     TNTSR01L                               32
255100960229    3C     *IN32         DOWEQ     *OFF
255200960223     C                   MOVEL     '1'           WAGTSR            1
255300960229     C                   MOVEL     'A'           TSRATB
255400960223     C                   EXSR      SBITSR
255500960223     C*
255600960223     C     KTSR12        READE     TNTSR01L                               32
255700960229    3C                   ENDDO
255800960223     C*
255900960229     C* SE C'E' FV
256000960229     C* CHAIN SU TNTSW02L PER CANCELLARE IL RECORD DEL FV SBINATO
256100960229    3C     TSWNFV        IFGT      0
256200960223     C                   MOVEL     'S'           KWTRC
256300960223     C                   MOVEL     TSWNFV        KNFV
256400960223     C                   MOVEL     TSWFFP        KLNP
256500960223     C     KTSW2         DELETE    TNTSW002                           32
256600960223     C*
256700960223     C* SBINO IL FOGLIO  VIAGGIO SU FVS
256800960223     C                   EXSR      SBIFVS
256900960223    3C                   ENDIF
257000960223     C**
257100960229     C* TOLGO LA "S" DI VARIATO
257200960229     C                   MOVEL     'A'           TSWATB
257300960229     C                   CLEAR                   TSWRFI
257400960229     C                   CLEAR                   TSWNFV
257500960229     C                   CLEAR                   TSWFVR
257600960229     C                   UPDATE    TNTSW003
257700960229     C**
257800960223     C     KTSW3         READE     TNTSW03L                               32
257900960223    2C                   ENDDO
258000960223    1C                   ENDIF
258100960223     C                   ENDSR
258200960223     C**
258300960223     C***************** RIPRISTINO UN TRAINO *************************+
258400960223     C     RIPTRN        BEGSR
258500960223     C                   MOVEL     ' '           TSWATB
258600960223     C                   UPDATE    TNTSW004
258700960223     C*
258800960223     C* DELETO LA GRIGLIA E LA RICREO
258900960223     C     KTSR13        CHAIN     TNTST01L                           32
259000960223    3C     *IN32         IFEQ      *OFF
259100960223     C*
259200960305     C     KTSR13        SETLL     TNTSR01L
259300960305     C     KTSR13        READE     TNTSR01L                               32
259400960223    4C     *IN32         DOWEQ     *OFF
259500960223     C                   DELETE    TNTSR000
259600960305     C     KTSR13        READE     TNTSR01L                               32
259700960223    4C                   ENDDO
259800960223     C*
259900960305     C     KTSR13        SETLL     TNTSM01L
260000960305     C     KTSR13        READE     TNTSM01L                               32
260100960223    4C     *IN32         DOWEQ     *OFF
260200960223     C                   DELETE    TNTSM000
260300960305     C     KTSR13        READE     TNTSM01L                               32
260400960223    4C                   ENDDO
260500960223     C*
260600960223     C                   DELETE    TNTST000
260700960223    3C                   ENDIF
260800960223     C* RICREO
260900960226     C                   CLEAR                   DSTL24
261000960223     C                   Z-ADD     TSWTRN        D24TRN
261100960223     C                   Z-ADD     TSWDTN        D24DTD
261200960223     C                   MOVEL     DSTL24        KPJBU
261300960223     C                   CALL      'TNTL24R'
261400960223     C                   PARM                    KPJBA
261500960304     C**
261600960304     C* LEGGO LE TRATTE SU TSW E SISTEMO IL FLAG DI ANNULLAMENTO
261700960304     C                   MOVEL     'R'           KWTRC
261800960304     C                   MOVEL     KVDTN         KWDTN
261900960304     C                   MOVEL     KVTRN         KWTRN
262000960304     C     KTSW3         SETLL     TNTSW03L
262100960304     C     KTSW3         READE     TNTSW03L                               32
262200960304     C     *IN32         DOWEQ     *OFF
262300960304     C                   MOVEL     TSWATV        TSWATB
262400960304     C                   UPDATE    TNTSW003
262500960304     C*
262600960304     C     KTSW3         READE     TNTSW03L                               32
262700960304     C                   ENDDO
262800960304     C*
262900960223     C                   ENDSR
263000960223     C**
263100960226     C************** ANNULLA/RIPRISTINA UNA TRATTA *******************
263200960223     C     ARTRA         BEGSR
263300960305     C                   MOVEL     'R'           KWTRC
263400960223     C*
263500960223     C     KTSW4         SETLL     TNTSW04L
263600960223     C     KTSW4         READE     TNTSW04L                               31
263700960223     C*
263800960223    1C     *IN31         DOWEQ     *OFF
263900960223     C*
264000960226    2C     TSWATB        IFNE      TSWATV
264100960223     C     TSWATV        ANDEQ     WATB
264200960223     C                   MOVEL     TSWATV        TSWATB
264300960305     C*
264400960305     C* MI SALVO I CAMPI CHE DOPO USO PER RIPOSIZIONARMI
264500960305     C                   MOVEL     TSWNFE        SAVNFE
264600960305     C                   MOVEL     TSWNFP        SAVNFP
264700960305     C                   MOVEL     TSWFFP        SAVFFP
264800960305     C                   MOVEL     TSWNFV        SAVNFV
264900960305     C*
265000960226    3C     WATB          IFEQ      'A'
265100960223     C                   CLEAR                   TSWRFI
265200960223     C                   CLEAR                   TSWNFV
265300960305     C                   CLEAR                   TSWRFV
265400960305     C                   CLEAR                   TSWVNF
265500960223     C                   CLEAR                   TSWFVR
265600960226    3C                   ENDIF
265700960223     C*
265800960223     C                   UPDATE    TNTSW004
265900960223     C                   MOVEL     TSWTRN        KVTRN
266000960223     C                   MOVEL     TSWDTN        KVDTN
266100960223     C                   MOVEL     TSWNFP        KVNFP
266200960226     C**
266300960226     C* AGGIORNO IL FILE DEI MC
266400960226     C     KTSR13        SETLL     TNTSM01L
266500960226     C     KTSR13        READE     TNTSM01L                               30
266600960226     C     *IN30         DOWEQ     *OFF
266700960226     C*
266800960226     C     TSMTFP        IFEQ      TSWFFP
266900960226     C                   MOVEL     WATB          TSMATB
267000960226     C                   UPDATE    TNTSM000
267100960226     C                   ENDIF
267200960226     C*
267300960226     C     KTSR13        READE     TNTSM01L                               30
267400960226     C                   ENDDO
267500960226     C**
267600960223     C*
267700960223     C* CERCO LA DATA DECORRENZA
267800960223     C                   EXSR      RICDEC
267900960223     C     KTLR          CHAIN     TNTLR01L                           30
268000960223     C*
268100960223    3C     TLRCAR        IFEQ      'S'
268200960223     C     TLRSCA        ANDEQ     'S'
268300960223     C                   MOVEL     'S'           WDELWR            1
268400960223     C                   MOVEL     TLRTFM        WTFM
268500960223     C                   ELSE
268600960223     C                   CLEAR                   WDELWR
268700960223    3C                   ENDIF
268800960223     C*
268900960223     C     KTSR12        SETLL     TNTSR01L
269000960223     C     KTSR12        READE     TNTSR01L                               32
269100960223    3C     *IN32         DOWEQ     *OFF
269200960223     C* RIPRISTINO
269300960223    4C     WATB          IFEQ      ' '
269400960223     C                   CLEAR                   TSRATB
269500960223     C                   CLEAR                   TSRDKP
269600960223     C                   CLEAR                   TSRDKA
269700960223     C                   UPDATE    TNTSR000
269800960226   X4C                   ELSE
269900960305     C**
270000960305     C* ANNULLAMENTO
270100960305     C                   MOVEL     'A'           TSRATB
270200960305     C                   MOVEL     '1'           WAGTSR            1
270300960223     C                   EXSR      SBITSR
270400960223    4C                   ENDIF
270500960223     C*
270600960223     C* VEDO SE DEVO DELETARE/SCRIVERE DEI RECORD
270700960223    4C     WDELWR        IFEQ      'S'
270800960223     C                   SETON                                        34
270900960223     C     KTLR          SETLL     TNTLR01L
271000960223     C     KTLR1         READPE    TNTLR01L                               30
271100960223     C*
271200960223    5C     *IN30         DOWEQ     *OFF
271300960223     C     *IN34         ANDEQ     *ON
271400960223     C     WTFM          ANDEQ     TLRTFM
271500960223     C* VEDO SE E' FERMATA DI CARICO PRESENTE NEI MC CON TLMTFA=TSRTFA
271600960226    6C     TLRCAR        IFEQ      'S'
271700960223     C     KTLM          CHAIN     TNTLM01L                           34
271800960223    7C     *IN34         DOWEQ     *OFF
271900960223     C     TLMATB        ANDNE     ' '
272000960223     C     KTLM          READE     TNTLM01L                               34
272100960223    7C                   ENDDO
272200960223     C*
272300960223    7C     *IN34         IFEQ      *OFF
272400960223     C* PREPARO LA KEY
272500960223    8C     WATB          IFEQ      ' '
272600960223     C                   MOVEL     TLRNFM        KVNFP
272700960223     C                   MOVEL     TLRFIL        KVFFP
272800960223     C     KTSR14        DELETE    TNTSR000                           33        DELETO
272900960223    8C                   ELSE
273000960223     C* PREPARO IL RECORD TSR
273100960305     C* CHAINO TSR03L PER PRENDERE I DATI DELLA FERMATA DI PARTENZA
273200960305     C                   MOVEL     TLRNFM        TSRNFP
273300960305     C                   MOVEL     TLRFIL        TSRFFP
273400960305     C                   MOVEL     ' '           TSRATB
273500960305     C     KTSR31        CHAIN     TNTSR03L                           33
273600960305     C                   MOVEL     T03DTP        TSRDTP
273700960305     C                   MOVEL     T03HTP        TSRHTP
273800960305     C                   MOVEL     T03HRP        TSRHRP
273900960305     C                   MOVEL     T03DRP        TSRDRP
274000960305     C                   MOVEL     T03RTP        TSRRTP
274100960305     C                   MOVEL     T03LNP        TSRLNP
274200960305     C                   MOVEL     T03NFV        TSRNFV
274300960305     C                   MOVEL     T03DFV        TSRDFV
274400960305     C                   MOVEL     *ALL'9'       TSRDKP
274500960305     C                   MOVEL     'S'           TSRFKP
274600960305     C                   MOVEL     *ALL'9'       TSRDKI
274700960305     C                   MOVEL     'S'           TSRFKI
274800960305     C* MUOVO TUTTI I CAMPI RELATIVI ALLA PARTENZA DI QUESTO NEW REC
274900020312     C                   WRITE     TNTSR000                             33      WRITE
275000960223     C* SE LA FERMATA IN TSR ERA GIA' ABBINATA, DEVO ABBINARE ANCHE
275100960223     C*  QUESTA TRATTA
275200960223    8C                   ENDIF
275300960223    7C                   ENDIF
275400960223     C*
275500960223    6C                   ENDIF
275600960223     C*
275700960223     C     KTLR1         READPE    TNTLR01L                               30
275800960223    5C                   ENDDO
275900960223    4C                   ENDIF
276000960223     C*
276100960223     C* MI RIPOSIZIONO SUL RECORD LETTO
276200960305     C                   MOVEL     SAVNFP        KVNFP
276300960305     C                   MOVEL     SAVFFP        KVFFP
276400960223     C     KTSR14        SETGT     TNTSR000
276500960223     C*
276600980203     C     KTSR12        READE     TNTSR01L                               32
276700980203     C***        KTSR12    READETNTSR01L            N    32
276800960223    3C                   ENDDO
276900960223     C**
277000960223     C* SE STO ANNULLANDO FACCIO ALTRA COSE:
277100960223     C*  1) SBINO FVS
277200960305    3C     WATB          IFEQ      'A'
277300960305     C*
277400960305   3AC     SAVNFV        IFGT      0
277500960305     C* DELETO IL TSW IL RECORD DI SBINAMENTO
277600960305     C                   MOVEL     'S'           KWTRC
277700960305     C                   MOVEL     SAVNFV        KNFV
277800960305     C                   MOVEL     SAVFFP        KLNP
277900960305     C     KTSW2         DELETE    TNTSW002                           33
278000960305     C*
278100960305     C                   Z-ADD     SAVNFV        KNFV
278200960305     C                   Z-ADD     SAVFFP        KLNP
278300960223     C                   EXSR      SBIFVS
278400960305   3AC                   ENDIF
278500960223     C*
278600960223     C* SE E' IL FV CHE ESPONE, MODIFICO FES SULLA TESTATA
278700960305    4C     SAVNFE        IFEQ      KVNFP
278800960223     C*
278900960223     C* LEGGO LA PRIMA FERMATA DI CARICO <> DA KVNFP E LA METTO
279000980121     C****       KTSR13    CHAINTNTSR01L             33
279100980121     C     KTSR13        CHAIN(N)  TNTSR01L                           33
279200960223     C*
279300960223    5C     *IN33         DOWEQ     *OFF
279400960223     C*
279500960223    6C     TSRNFP        IFNE      KVNFP
279600960223     C     TSRATB        ANDEQ     ' '
279700960223     C     KTSR13        CHAIN     TNTST01L                           33
279800960223    7C     *IN33         IFEQ      *OFF
279900960223     C                   Z-ADD     TSRNFP        TSTNFE
280000960223     C                   Z-ADD     TSRFFP        TSTFES
280100960223     C                   UPDATE    TNTST000
280200960223    7C                   ENDIF
280300960223     C*
280400960223     C                   SETON                                        33
280500960223   X6C                   ELSE
280600980121     C*****      KTSR13    READETNTSR01L                 33
280700980121     C     KTSR13        READE(N)  TNTSR01L                               33
280800960223    6C                   ENDIF
280900960223    5C                   ENDDO
281000960223    4C                   ENDIF
281100960223    3C                   ENDIF
281200960223    2C                   ENDIF
281300960223     C*
281400960305     C                   MOVEL     'R'           KWTRC
281500960223     C     KTSW4         READE     TNTSW04L                               31
281600960223    1C                   ENDDO
281700960223     C                   ENDSR
281800960223     C**
281900960223     C************* AGGIORNAMENTO TESTATA GRIGLIA *********************
282000960223     C     UPTST         BEGSR
282100960223     C                   MOVEL     TSWATV        TSTATB
282200960223     C                   MOVEL     TSWVPD        TSTPDR
282300960226     C                   MOVEL     TSWVCU        TSTCAU
282400010827     C**!!!              MOVEL     TSWVIC        TSTICO
282500010827     C                   z-add     TSWVIC        TSTICO
282600960223     C                   MOVEL     TSWCNF        TSTCNF
282700960223     C                   MOVEL     TSWTMZ        TSTTMZ
282800010827     C**!!!              MOVEL     TSWILI        TSTILI
282900010827     C                   z-add     TSWILI        TSTILI
283000960226     C* SE HO SCONFERMATO L'IMPORTO LO MEMORIZZO
283100960223     C     TSWICO        IFNE      TSWVIC
283200960223     C     TSWVIC        ANDEQ     0
283300960223     C                   MOVEL     'S'           WSCONF            1
283400960223     C                   ELSE
283500960223     C                   CLEAR                   WSCONF
283600960223     C                   ENDIF
283700010827     C**!!!              MOVEL     TSWIPT        TSTIPT
283800010827     C**!!!              MOVEL     TSWIES        TSTIES
283900010827     C                   z-add     TSWIPT        TSTIPT
284000010827     C                   z-add     TSWIES        TSTIES
284100960223     C                   UPDATE    TNTST000
284200960223     C                   ENDSR
284300960223     C**
284400960223     C************* SBINAMENTO SU TSR *********************************
284500960223     C     SBITSR        BEGSR
284600960223     C                   MOVE      *ZEROS        TSRDRP
284700960223     C                   MOVE      *ZEROS        TSRHRP
284800960223     C                   MOVE      *BLANKS       TSRRTP
284900960223     C                   MOVE      *ZEROS        TSRDRA
285000960223     C                   MOVE      *ZEROS        TSRHRA
285100960223     C                   MOVE      *BLANKS       TSRRTA
285200960223     C                   MOVE      *ZEROS        TSRLNP
285300960223     C                   MOVE      *ZEROS        TSRNFV
285400960223     C                   MOVE      *ZEROS        TSRDFV
285500960223     C                   MOVE      *ZEROS        TSRDKP
285600960223     C                   MOVE      *ZEROS        TSRDKA
285700960223     C                   MOVE      *ZEROS        TSRDKI
285800960223     C     WAGTSR        IFEQ      '1'
285900960223     C                   UPDATE    TNTSR000
286000960223     C                   ELSE
286100960223     C                   UPDATE    TNTSR004
286200960223     C                   ENDIF
286300960223     C                   ENDSR
286400960226     C**
286500960226     C************* ABBINAMENTO SU TSR ********************************
286600960226     C     ABBTSR        BEGSR
286700960226     C* DA ESEGUIRE SOLO LA PRIMA VOLTA
286800960226    1C     WVOLTA        IFEQ      *BLANKS
286900960226     C*
287000970328     C* Memorizzo se data/ora partenza del FV = data/ora partenza teor.
287100970328     C     FVSDRP        COMP      TSRDTP                                 18
287200970328     C   18FVSHRP        COMP      TSRHTP                                 18
287300960226     C* Controlli già effettuati: memorizzo se variato qualcosa
287400960229    2C     TSRFKP        IFNE      ' '
287500960226    3C     FVSDRP        IFNE      TSRDRP
287600960226     C     FVSHRP        ORNE      TSRHRP
287700960226     C     FVSRTP        ORNE      TSRRTP
287800960226     C                   MOVE      'M'           WMODIF            1
287900970328     C* Non segnalo comunque variazione di data/ora se partenza puntua-
288000970328     C* le sia prima della variazione che adesso
288100970328    4C     *IN18         IFEQ      *ON
288200970328     C     TSRDKP        ANDEQ     99999999
288300970328     C     FVSRTP        ANDEQ     *BLANKS
288400970328     C     TSRRTP        ANDEQ     *BLANKS
288500970328     C                   CLEAR                   WMODIF
288600970328    4C                   ENDIF
288700960226    3C                   ENDIF
288800960226    2C                   ENDIF
288900960226     C                   MOVE      '1'           WVOLTA
289000960226    1C                   ENDIF
289100960226     C*
289200960226     C* Se variato qualcosa e controlli già effettuati, solo sul primo
289300960226     C* record azzero la data controllo importi mentre sui record suc-
289400960226     C* cessivi la imposto=99999999 per non essere letti da pgm control
289500960305    2C     WMODIF        IFEQ      'M'
289600960226    3C     WVOLTA        IFEQ      '1'
289700960226     C                   MOVE      *ZEROS        TSRDKP
289800960226     C                   MOVE      '2'           WVOLTA
289900960226   X3C                   ELSE
290000960226     C                   MOVE      99999999      TSRDKP
290100960229     C                   MOVE      'S'           TSRFKP
290200960226    3C                   END
290300960305    2C                   END
290400960226     C*
290500960226     C* CHAIN SU FVS PER AGGIORNAMENTO
290600960226     C                   MOVEL     TSWVNF        KNFV
290700960226     C                   MOVEL     TSWFFP        KLNP
290800960226     C     KTSR4         CHAIN     TNFVS01L                           33
290900960226     C                   MOVE      FVSDRP        TSRDRP                         DATA PART.R.
291000960226     C                   MOVE      FVSHRP        TSRHRP                         ORA  PART.R.
291100960226     C                   MOVE      FVSRTP        TSRRTP                         RITARD PART
291200960226     C                   MOVE      FVSLNP        TSRLNP                         LNP F.V.
291300960226     C                   MOVE      FVSNFV        TSRNFV                         NR.F.V.
291400960226     C                   MOVE      FVSDFV        TSRDFV                         DATA F.V.
291500960226     C                   UPDATE    TNTSR000
291600960226     C*
291700960305     C                   MOVEL     TSRTFM        FVSTFS
291800960226     C                   Z-ADD     KVTRN         FVSTRS
291900960226     C                   MOVEL     'S'           FVSABN
292000960226     C                   UPDATE    TNFVS000
292100960226     C* MEMORIZZO CHE DEVO LANCIARE IL PGM DELLE DATE/ORE DI ARRIVO
292200960226     C                   MOVEL     '1'           WTL26             1
292300960904     C* RICHIAMO PGM CHE APPOGGIA MC REALI SU GRIGLIA
292400960904     C                   MOVEL     '1'           WCL33             1
292500960904     C                   CLEAR                   DSTL33
292600960904     C                   MOVE      FVSLNP        D33LNP
292700960904     C                   MOVE      FVSNFV        D33NFV
292800960904     C                   MOVE      TSRDTN        D33DTN
292900960904     C                   MOVEL     DSTL33        KPJBU
293000960904     C                   CALL      'TNTL33R'
293100960904     C                   PARM                    KPJBA
293200960226     C                   ENDSR
293300960223     C**
293400960223     C************* SBINAMENTO SU FVS *********************************
293500960223     C     SBIFVS        BEGSR
293600960223     C     KTSR4         CHAIN     TNFVS01L                           31
293700960223     C                   CLEAR                   FVSTRS
293800960305     C                   CLEAR                   FVSTFS
293900960223     C     FVSTRN        IFGT      *ZEROS
294000960223     C                   MOVE      *BLANKS       FVSABN
294100960223     C                   END
294200960304     C*
294300960223     C                   UPDATE    TNFVS000
294400960223     C**
294500960226     C     KTSR4         SETLL     FNFV201L
294600960223     C     KTSR4         READE     FNFV201L                               30
294700960223    4C     *IN30         DOWEQ     *OFF
294800960223     C                   MOVE      *BLANKS       FV2ABN
294900960223     C                   UPDATE    FNFV2000
295000960223     C     KTSR4         READE     FNFV201L                               30
295100960223    4C                   ENDDO
295200960223     C                   ENDSR
295300960223     C**
295400951213     C********** CONTROLLO DATI SFL04 *********************************
295500951213     C     CTRS04        BEGSR
295600951214     C                   CLEAR                   W0040
295700951214     C* CONTROLLO I CAMPI DELLA RICERCA NEL SFLCTL
295800971204     C* LA RICERCA NON E' ATTIVATA!!!1
295900971204     C***                  Z-ADDV4CRLP    WRTP
296000971204     C***                  Z-ADDV4CRDT    WRDT
296100971204     C***                  CLEARAMGRDT
296200971204     C***                  EXSR CTRRIC
296300971204     C***90                GOTO ENDCS4
296400951214     C*
296500010824     C                   READC     TL44S04                                32
296600951214     C*
296700951214    1C     *IN32         DOWEQ     *OFF
296800960322     C* LEGGO TUTTI I FV CHE MI HANNO SCELTO E CARICO L'ALTRA VIDEATA
296900960322    2C     V4CSCE        IFEQ      'M'
297000960322     C                   MOVEL     'S'           WRICH             1
297100960325     C                   MOVEL     *IN99         SAV99             1
297200960322     C                   EXSR      GESTRN
297300960322     C                   SETON                                        32
297400960322     C*
297500960322     C* SE POI ROLLERO' MI DOVRO' RIPOSIZIONARE
297600960322     C                   MOVEL     'S'           WPOSIZ
297700960322    2C                   ENDIF
297800960322     C*
297900010824     C  N32              READC     TL44S04                                32
298000951214    1C                   ENDDO
298100960322     C                   Z-ADD     19            WRIPAG
298200960325     C                   CLEAR                   WRICH
298300960322     C                   CLEAR                   WTRN
298400960322     C                   CLEAR                   AMGTRD
298500960322     C                   CLEAR                   V1CTRD
298600960322     C                   CLEAR                   AMGTRA
298700960325     C                   MOVEL     AMGTTD        KDTT
298800960325     C                   MOVEL     V1CLNP        KLNP
298900960325     C                   MOVEL     V1CNFV        KNFV
299000960325     C                   MOVEL     SAV99         *IN99
299100960325     C* DELETO I RECORD DEL SFL2
299200960322     C                   Z-ADD     2             KSFL
299300960322     C                   EXSR      DELTSW
299400960322     C*
299500960322     C                   Z-ADD     4             KSFL
299600960325     C*
299700960325     C* SE E' STATO PREMUTO F6 DI AGGIORNAMENTO RICARICO SFL4
299800960325     C     *INKF         IFEQ      *ON
299900960325     C                   MOVEL     WNRR4         W0050
300000960325     C* PULISCO IL SFL
300100960325     C                   EXSR      PULSFL
300200960325     C* PULISCO TNTSW
300300960325     C                   EXSR      DELTSW
300400960322     C*
300500960325     C* CARICO FIN TANTO CHE NON NE HO CARICATI COME PRIMA
300600960325     C                   EXSR      SETSR4
300700960325     C*
300800960325     C                   CLEAR                   WNRR4
300900960325     C     WNRR4         DOWLT     W0050
301000960325     C                   Z-ADD     0             L
301100960325     C                   EXSR      LEGS04
301200040422     c* se non ho caricato niente esco dal ciclo
301300040422     c     wnrr4         ifeq      0
301400040422     c                   leave
301500040422     c                   endif
301600960325     C                   ENDDO
301700960325     C                   Z-ADD     SAVNR4        V4CREC
301800960325     C                   ENDIF
301900960325     C*
302000951213     C     ENDCS4        ENDSR
302100951114     C**
302200951114     C********** PREPARA MESSAGGIO DI FV GIA' ABBINATO ****************
302300951114     C     PREMSG        BEGSR
302400951114     C                   MOVEA     MSG(30)       K78
302500951114     C                   MOVEA     W007A         SKI(9)                         NUM  TRAINO
302600951114     C                   Z-ADD     9             I
302700951114     C                   EXSR      ABBL
302800951114     C                   MOVEA     SKI(9)        K78(50)
302900951114     C*
303000951117     C                   MOVEL     W008A         WDATA                          DATA TRAINO
303100951114     C                   MOVEA     WGG           K78(62)
303200951114     C                   MOVEA     WMM           K78(65)
303300951114     C                   MOVEA     WAA           K78(68)
303400951114     C*
303500951214     C                   MOVEA     CNFV          SKI(11)                        NUM  FV
303600951114     C                   Z-ADD     11            I
303700951114     C                   EXSR      ABBL
303800951114     C                   MOVEA     SKI(11)       K78(16)
303900951114     C*
304000951214     C                   MOVEA     K78           VSCMSG
304100951114     C                   ENDSR
304200951106     C**
304300951106     C********** AGGIORNAMENTO FILE TNTSW02L **************************
304400951106     C     UPTSW         BEGSR
304500951106     C* SBINAMENTO FV
304600960221    1C     WONFV         IFGT      0
304700960213     C                   MOVEL     'S'           KWTRC
304800951106     C                   Z-ADD     WONFV         KNFV
304900951106     C     KTSW2         SETLL     TNTSW02L                               31
305000951106     C* C'E' GIA' NON FACCIO NULLA
305100951106    2C     *IN31         IFEQ      *OFF
305200951106     C*
305300960213     C                   MOVEL     'A'           KWTRC
305400951106     C     KTSW2         CHAIN     TNTSW02L                           31
305500951106    3C     *IN31         IFEQ      *OFF
305600951106     C                   MOVEL     'S'           TSWTRC
305700960221     C                   Z-ADD     KSFL          TSWSFL
305800960222     C                   MOVE      WANFP         TSWNFP
305900951114     C                   CLEAR                   TSWTRN
306000951114     C                   CLEAR                   TSWDTN
306100951106     C                   UPDATE    TNTSW002
306200951106   X3C                   ELSE
306300951106     C*
306400951106     C                   CLEAR                   TNTSW002
306500951107     C                   MOVEL     KNMUS         TSWNUS
306600951107     C                   MOVEL     KNRJO         TSWNJO
306700951107     C                   MOVEL     KNMEB         TSWNMB
306800951106     C                   MOVEL     'S'           TSWTRC
306900951106     C                   Z-ADD     WONFV         TSWNFV
307000951107     C                   MOVE      KLNP          TSWFFP
307100960222     C                   MOVE      WANFP         TSWNFP
307200951106     C                   Z-ADD     KSFL          TSWSFL
307300951106     C                   WRITE     TNTSW002
307400951106    3C                   ENDIF
307500951106    2C                   ENDIF
307600960221    1C                   ENDIF
307700960213     C*
307800951106     C* ABBINAMENTO FV
307900960221    1C     WNNFV         IFGT      0
308000960221     C                   MOVEL     'A'           KWTRC
308100951117     C                   Z-ADD     WNNFV         KNFV
308200960221     C     KTSW2         SETLL     TNTSW02L                               31
308300951117     C*
308400960221    2C     *IN31         IFEQ      *OFF
308500960221     C*
308600960221     C                   MOVEL     'S'           KWTRC
308700951106     C     KTSW2         CHAIN     TNTSW02L                           31
308800960221    3C     *IN31         IFEQ      *OFF
308900951106     C                   MOVEL     'A'           TSWTRC
309000951114     C                   Z-ADD     WATRN         TSWTRN
309100951114     C                   Z-ADD     WADTN         TSWDTN
309200960221     C                   Z-ADD     WNNFV         TSWNFV
309300960222     C                   MOVE      WANFP         TSWNFP
309400960221     C                   Z-ADD     KSFL          TSWSFL
309500951106     C                   UPDATE    TNTSW002
309600951106     C*
309700960221   X3C                   ELSE
309800951106     C                   CLEAR                   TNTSW002
309900951107     C                   MOVEL     KNMUS         TSWNUS
310000951107     C                   MOVEL     KNRJO         TSWNJO
310100951107     C                   MOVEL     KNMEB         TSWNMB
310200951106     C                   MOVEL     'A'           TSWTRC
310300951106     C                   Z-ADD     WNNFV         TSWNFV
310400951107     C                   MOVE      KLNP          TSWFFP
310500951106     C                   Z-ADD     KSFL          TSWSFL
310600960222     C                   MOVE      WANFP         TSWNFP
310700951114     C                   Z-ADD     WATRN         TSWTRN
310800951114     C                   Z-ADD     WADTN         TSWDTN
310900951106     C                   WRITE     TNTSW002
311000960221    3C                   ENDIF
311100960221    2C                   ENDIF
311200951106    1C                   ENDIF
311300951106     C*
311400951106     C                   ENDSR
311500951124     C**
311600951124     C*********** RICERCO DECORRENZA VALIDA ***************************
311700951124     C     RICDEC        BEGSR
311800951124     C                   CLEAR                   KDDE
311900951124     C                   CLEAR                   TLTDDE
312000951124     C* MI POSIZIONO CON NUMERO TRAINO E DATA TRAINO
312100951124     C*   PER CARICARE L'ANAGRAFICA DECORRENTE
312200951214     C     KVTRN         SETLL     TNTLT01L
312300951214     C     KVTRN         READE     TNTLT01L                               31
312400951124     C     *IN31         DOWEQ     *OFF
312500951124     C* SE NON E' ANNULLATO E RIENTRA NELLA SCADENZA -->MEMORIZZO
312600951124     C     TLTATB        IFEQ      ' '
312700951214     C     TLTDDE        ANDLE     KVDTN
312800951214     C     TLTDSC        ANDGE     KVDTN
312900951124     C                   Z-ADD     TLTDDE        KDDE
313000951124     C                   SETON                                        31
313100951124     C                   ENDIF
313200951124     C*
313300951214     C  N31KVTRN         READE     TNTLT01L                               31
313400951124     C                   ENDDO
313500951124     C                   ENDSR
313600951108     C**
313700951108     C********** CERCO IL NUOVO IMPORTO LISTINO ***********************
313800951108     C     RICLIS        BEGSR
313900951108     C                   CLEAR                   WILI
314000951115     C                   CLEAR                   WCAU
314100951214     C                   Z-ADD     V2HTRN        KVTRN
314200951214     C                   Z-ADD     V2HDTN        KVDTN
314300951124     C                   EXSR      RICDEC
314400960212     C* PER CERCARE L'IMPORTO 4 TENTATIVI:
314500951108     C     KDDE          IFGT      0
314600960212     C*  1) CON CODICI SPECIFICI
314700960212     C                   MOVEL     V2HTMZ        KTMZ
314800951108     C                   MOVEL     V2CPDR        W0070
314900960212     C                   EXSR      REDTLL
315000960212     C*  2) CON TRAZIONISTA SPECIFICO E TIPO AUTOMEZZO = 99
315100960212    1C     *IN31         IFEQ      *ON
315200960212     C                   MOVEL     '99'          KTMZ
315300960212     C                   EXSR      REDTLL
315400960212    1C                   ENDIF
315500960212     C*  3) CON TRAZIONISTA = 9999999 E TIPO AUTOMEZZO SPECIFICO
315600960212    1C     *IN31         IFEQ      *ON
315700960212     C                   CLEAR                   KTMZ
315800960212     C                   MOVEL     V2HTMZ        KTMZ
315900960212     C                   MOVEL     9999999       W0070
316000960212     C                   EXSR      REDTLL
316100960212    1C                   ENDIF
316200960212     C*  4) CON TRAZIONISTA = 9999999 E TIPO AUTOMEZZO = 99
316300960212    1C     *IN31         IFEQ      *ON
316400960212     C                   MOVEL     '99'          KTMZ
316500960212     C                   MOVEL     9999999       W0070
316600960212     C                   EXSR      REDTLL
316700960212    1C                   ENDIF
316800960212     C*  31 OFF - TROVATO
316900010911     c                   if        *in31 = *off
317000010911      * devo convertire il listino nella divisa della griglia
317100010911     c                   eval      w_importo = tllili
317200011024     c                   if        tlldiv <> tswdiv and tllili <> 1
317300011024     c                             and tllili <> 0
317400010911     c                   eval      w_divisa = tlldiv
317500010911     c                   exsr      Sr_Converti
317600010911     c                   endif
317700010911     c                   z-add     w_importo     wili
317800010911     C**N31              Z-ADD     TLLILI        WILI
317900010911     C**N31              MOVEL     TLLCAU        WCAU
318000010911     C                   MOVEL     TLLCAU        WCAU
318100010911     c                   endif
318200951108     C                   ENDIF
318300960212     C**
318400951116     C                   SETON                                        1790
318500951108     C                   ENDSR
318600951102     C**
318700960212     C********* LETTURA FILE LISTINI **********************************
318800960212     C     REDTLL        BEGSR
318900010824     C     KTLL1         SETLL     TITLL01L
319000010824     C     KTLL1         READE     TITLL01L                               31
319100960212    2C     *IN31         DOWEQ     *OFF
319200960212     C     TLLATB        ANDNE     ' '
319300010824     C     KTLL1         READE     TITLL01L                               31
319400960212    2C                   ENDDO
319500960212     C                   ENDSR
319600951113     C********* LETTURA PER CARICAMENTO SFL02 *************************
319700951102     C     LEGS02        BEGSR
319800020315
319900020315     c                   clear                   w_tsrtrn
320000041001     c                   clear                   w_tsrdtn
320100020315
320200951113     C* SE E' PREVISTO, LEGGO L'ULTIMO RECORD SCRITTO PER
320300951113     C* POSIZIONARMI DA LI' IN POI
320400951113    1C     WPOSIZ        IFEQ      'S'
320500951113     C                   CLEAR                   KNFP
320600951113     C                   CLEAR                   WPOSIZ
320700951113     C*
320800951115     C                   Z-ADD     WNRR2         NRR2
320900010824     C     NRR2          CHAIN     TL44S02                            30
321000951113    2C     *IN30         IFEQ      *OFF
321100951214     C                   Z-ADD     V2HDTN        KDTN
321200951113     C                   Z-ADD     V2HTRN        KTRN
321300010824     C                   Z-ADD     V2hTFP        KTFP
321400951113    3C     V2HTRC        IFEQ      'R'
321500951113     C                   Z-ADD     V2HNFP        KNFP
321600951113    3C                   ENDIF
321700951113     C   04              Z-ADD     WPDR          KPDR
321800951113     C* POSIZIONAMENTO E LETTURA DELL'ULTIMO USATO
321900951113     C                   EXSR      SETST2                                       SETLL
322000951113     C                   EXSR      LEGST2                                       READ
322100951113     C*
322200951113   X2C                   ELSE
322300951214     C                   EXSR      PULSFL
322400951113    2C                   ENDIF
322500951113    1C                   ENDIF
322600951113     C*
322700951113     C                   SETOFF                                       9930
322800951103     C* 99 ON -FINE LETTURA TOTALE
322900951103    1C     *IN99         DOWEQ     *OFF
323000951214     C     L             ANDLT     WRIPAG
323100951113     C*
323200951113     C     WLCTE         IFEQ      'S'
323300951113     C                   EXSR      LEGST2                                       READ
323400951113     C                   ENDIF
323500951103     C*
323600951103    2C     *IN30         DOWEQ     *OFF
323700951103     C     TSTDTN        ANDLE     AMGTRA
323800951214     C     L             ANDLT     WRIPAG
323900951106     C*
324000951106     C* LEGGO LE TRATTE DI QUESTO TRAINO
324100951106     C* CARICO RIGA TESTATA
324200951113     C     WLCTE         IFEQ      'S'
324300951103     C                   EXSR      CTST2
324400951113     C                   CLEAR                   WLCTE
324500951113     C                   ENDIF
324600951116     C*
324700951116     C* SE E' LA 19 LETTURA --> VADO A FINE FILE
324800951214   2AC     L             IFLT      WRIPAG
324900951116     C*
325000951113     C                   MOVEL     TSTTRN        KTRN
325100951113     C                   MOVEL     TSTDTN        KDTN
325200951214     C                   Z-ADD     999           WLNFP
325300951113     C*
325400951113     C* POSIZIONAMENTO : SENZA NUMERO FERMATA
325500951113     C     KNFP          IFEQ      0
325600951214     C     KTS1          SETLL     TNTSR01L
325700951106     C                   ELSE
325800951113     C*                  CON   NUMERO FERMATA
325900951106     C     KTSR11        SETGT     TNTSR01L
326000951106     C                   ENDIF
326100951106     C*
326200980121     C     KTS1          READE(N)  TNTSR01L                               33
326300951103    3C     *IN33         DOWEQ     *OFF
326400951214     C     L             ANDLT     WRIPAG
326500951215     C* CARICO RIGA TRATTE  SOLO SE FVT  CON FV <> DAL PRECEDENTE
326600951214     C     TSRNFP        IFNE      WLNFP
326700951103     C                   EXSR      CTSR2
326800951214     C                   MOVEL     TSRNFP        WLNFP
326900951213     C                   ENDIF
327000951103     C*
327100980121     C     KTS1          READE(N)  TNTSR01L                               33
327200951103    3C                   ENDDO
327300100629     c
327400951113     C* FINE CARICAMENTO SFL
327500951214    3C     L             IFEQ      WRIPAG
327600100629     c                   if        not *in33
327700100629     c* Se la fermata è = a quella appena memorizzata --> lascio
327800100629     c*    altrimenti memorizzo fermata-1 per rileggere questa e non saltarla
327900100629     c                   if        tsrnfp<>wlnfp  and tsrnfp>1
328000100629     c     tsrnfp        sub       1             kNFP
328100100629     c                   else
328200100629     C                   Z-ADD     TSRNFP        KNFP
328300100629     c                   endif
328400100629     c
328500100629     C                   CLEAR                   WLCTE
328600100629     c                   eval      w_tsrtrn = tsrtrn
328700100629     c                   eval      w_tsrdtn = tsrdtn
328800100629     c                   else
328900100629     C                   CLEAR                   KNFP
329000100629     C                   MOVEL     'S'           WLCTE
329100100629     c                   endif
329200951113     C*
329300951116   X3C                   ELSE
329400951113     C                   CLEAR                   KNFP
329500951113     C                   MOVEL     'S'           WLCTE             1
329600951106     C                   EXSR      LEGST2
329700951116    3C                   ENDIF
329800951103     C*
329900951116   2AC                   ENDIF
330000951103    2C                   ENDDO
330100951103     C*
330200951106     C* SE FINE LETTURA --> SE PREVISTO CERCO NUOVO TERMINAL
330300951103    2C     *IN30         IFEQ      *ON
330400951103    3C     *IN02         IFEQ      *OFF
330500960322     C     *IN03         ANDEQ     *OFF
330600951103     C*
330700951103     C     KTFP          SETGT     TNTST04L
330800951103     C                   READ      TNTST04L                               31
330900951103    4C     *IN31         IFEQ      *OFF
331000951103     C                   Z-ADD     TSTTFP        KTFP
331100951103     C     KTST4         SETLL     TNTST04L
331200951103   X4C                   ELSE
331300951103     C                   SETON                                            99
331400951103    4C                   ENDIF
331500951103     C*
331600951103   X3C                   ELSE
331700951103     C                   SETON                                        99
331800951103    3C                   ENDIF
331900951103    2C                   ENDIF
332000951103     C*
332100951103     C  N30TSTDTN        IFGT      AMGTRA
332200951103     C                   SETON                                        99
332300951103     C                   ENDIF
332400951103     C*
332500951103     C                   ENDDO
332600020408
332700020408
332800951102     C*
332900951103     C                   Z-ADD     NRR2          V2CREC
333000951103     C                   Z-ADD     NRR2          WNRR2                          TOTALE REC
333100951102     C                   ENDSR
333200951212     C**
333300951212     C********* LETTURA PER CARICAMENTO SFL04 *************************
333400951212     C     LEGS04        BEGSR
333500960322     C     AMGTTA        IFEQ      0
333600960322     C                   Z-ADD     99999999      AMGTTA
333700960322     C                   ENDIF
333800960322     C*
333900951212     C* SE E' PREVISTO, LEGGO L'ULTIMO RECORD SCRITTO PER
334000951212     C* POSIZIONARMI DA LI' IN POI
334100951212    1C     WPOSIZ        IFEQ      'S'
334200951212     C                   CLEAR                   WPOSIZ
334300951212     C*
334400951212     C                   Z-ADD     WNRR4         NRR4
334500010824     C     NRR4          CHAIN     TL44S04                            30
334600951212    2C     *IN30         IFEQ      *OFF
334700951212     C                   Z-ADD     V4HDTT        KDTT
334800951212     C                   Z-ADD     V4CFFP        KLNP
334900951212     C                   Z-ADD     V4HDTN        KDTN
335000951212     C                   Z-ADD     V4CTRN        KTRN
335100951212     C* POSIZIONAMENTO E LETTURA DELL'ULTIMO USATO
335200951212     C     KTSR21        SETLL     TNTSR02L
335300951212     C                   EXSR      LEGSR4                                       READ
335400951212     C*
335500951212   X2C                   ELSE
335600951214     C                   EXSR      PULSFL
335700951212    2C                   ENDIF
335800951212    1C                   ENDIF
335900951212     C*
336000951212     C                   SETOFF                                       9930
336100951212     C* 99 ON -FINE LETTURA TOTALE
336200951212    1C     *IN99         DOWEQ     *OFF
336300951212     C     L             ANDLT     WRIPAG
336400951212     C*
336500951212     C                   EXSR      LEGSR4
336600951212     C*
336700951212    2C     *IN30         DOWEQ     *OFF
336800951213     C     TSRDTT        ANDLE     AMGTTA
336900951212     C     L             ANDLT     WRIPAG
337000951212     C*
337100951214     C     WLNFP         IFNE      TSRNFP
337200951214     C     WLTRN         ORNE      TSRTRN
337300951214     C     WLDTN         ORNE      TSRDTN
337400951212     C                   EXSR      CTSR4
337500951214     C                   MOVEL     TSRNFP        WLNFP
337600951214     C                   MOVEL     TSRTRN        WLTRN
337700951214     C                   MOVEL     TSRDTN        WLDTN
337800951214     C                   ENDIF
337900951212     C*
338000951213     C                   EXSR      LEGSR4
338100951212    2C                   ENDDO
338200951212     C*
338300951212     C* SE FINE LETTURA --> SE PREVISTO CERCO NUOVA LNP  FV
338400951212    2C     *IN30         IFEQ      *ON
338500951212    3C     *IN06         IFEQ      *OFF
338600951212     C*
338700951212     C     KLNP          SETGT     TNTSR02L
338800951212     C                   READ      TNTSR02L                               31
338900951212    4C     *IN31         IFEQ      *OFF
339000951212     C                   Z-ADD     TSRFFP        KLNP
339100951212     C     KTSR2         SETLL     TNTSR02L
339200951212   X4C                   ELSE
339300951212     C                   SETON                                            99
339400951212    4C                   ENDIF
339500951212     C*
339600951212   X3C                   ELSE
339700951212     C                   SETON                                        99
339800951212    3C                   ENDIF
339900951212    2C                   ENDIF
340000951212     C*
340100951212     C  N30TSRDTT        IFGT      AMGTTA
340200951212     C                   SETON                                        99
340300951212     C                   ENDIF
340400951212     C*
340500951212     C                   ENDDO
340600951212     C*
340700951212     C                   Z-ADD     NRR4          V4CREC
340800951212     C                   Z-ADD     NRR4          WNRR4                          TOTALE REC
340900960322     C*
341000960322     C     AMGTTA        IFEQ      99999999
341100960322     C                   CLEAR                   AMGTTA
341200960322     C                   ENDIF
341300951212     C                   ENDSR
341400951103     C**
341500951113     C******** POSIZIONAMENTO TESTATA TNTST PER SFL02 *****************
341600951113     C     SETST2        BEGSR
341700951113    1C                   SELECT
341800951113     C     WTRN          WHENGT    0                                            NUM. TRAINO
341900960322     C                   MOVEL     WTRN          KTRN
342000951214     C     KTS1          SETLL     TNTST01L
342100951113     C*
342200951113     C     *IN04         WHENEQ    *ON                                          TRAZIONISTA
342300951113     C     KTST2         SETLL     TNTST02L
342400951113     C                   OTHER
342500951113     C*
342600951113     C* SE IMMESSO TERMINAL PARTENZA MI POSIZIONO
342700951113    2C     KTFP          IFEQ      0                                            TER.PARTENZA
342800951113     C     KTFP          SETGT     TNTST04L
342900951113     C                   READ      TNTST04L                               31
343000951113     C  N31              Z-ADD     TSTTFP        KTFP
343100951113     C   31              Z-ADD     999           KTFP
343200951113    2C                   ENDIF
343300951113     C*
343400951113     C     KTRN          IFEQ      0
343500951113     C     KTST4         SETLL     TNTST04L
343600951113     C                   ELSE
343700951113     C     KTST41        SETLL     TNTST04L
343800951113     C                   ENDIF
343900951113    1C                   ENDSL
344000951113     C                   ENDSR
344100951113     C**
344200951212     C******** POSIZIONAMENTO RIGHE TRATTE PER SFL04 ******************
344300951212     C     SETSR4        BEGSR
344400951212     C     V1CNFV        IFGT      0                                            NUM. FV
344500951213     C                   Z-ADD     V1CNFV        KNFV
344600951213     C                   Z-ADD     V1CLNP        KLNP
344700951212     C     KTSR4         SETLL     TNTSR04L
344800951212     C*
344900951212     C                   ELSE
345000951212     C*
345100951212     C* SE IMMESSO TERMINAL PARTENZA MI POSIZIONO
345200951212    2C     KLNP          IFEQ      0                                            LIN.PARTENZA
345300951212     C     KLNP          SETGT     TNTSR02L
345400951214     C                   READ      TNTSR02L                               31
345500951212     C  N31              Z-ADD     TSRFFP        KLNP
345600951212     C   31              Z-ADD     999           KLNP
345700951212    2C                   ENDIF
345800951212     C*
345900951212     C     KTSR2         SETLL     TNTSR02L
346000951212    1C                   ENDIF
346100951212     C                   ENDSR
346200951212     C**
346300951103     C**********LETTURA TESTATA TNTST PER SFL02 ***********************
346400951103     C     LEGST2        BEGSR
346500951113     C*
346600951113     C                   SETOFF                                       30
346700951113     C                   CLEAR                   WSEL
346800011012     C                   eval      flggei = '0'
346900951113    0C     WSEL          DOWEQ     ' '
347000951113     C     *IN30         ANDEQ     *OFF
347100951103     C* NUMERO TRAINO
347200951113    1C                   SELECT
347300951113     C     WTRN          WHENGT    0                                            NUM TRAINO
347400951113     C                   Z-ADD     WTRN          KTRN
347500980121     C**N09      KTS1      READETNTST01L                 30
347600980121     C** 09      KTRN      READETNTST01L                 30
347700980121     C  N09KTS1          READE(N)  TNTST01L                               30
347800980121     C   09KTRN          READE(N)  TNTST01L                               30
347900951113     C*
348000951113     C     *IN04         WHENEQ    *ON
348100951113     C                   Z-ADD     WPDR          KPDR
348200951113     C  N09KTST2         READE     TNTST02L                               30
348300951113     C   09KPDR          READE     TNTST02L                               30
348400951113     C*
348500951113   X1C                   OTHER                                                  TER.PARTENZA
348600951113    2C     *IN10         IFEQ      *OFF
348700951103     C     *IN09         ANDEQ     *ON
348800951103     C     KTFP          READE     TNTST04L                               30
348900951113   X2C                   ELSE
349000951103     C     KTST4         READE     TNTST04L                               30
349100951113    2C                   ENDIF
349200951113    1C                   ENDSL
349300011012      * aggancio solo una volta la tabella GEI
349400011012      * (solo una volta xchè la divisa è unica per tutta la griglia)
349500011012     C                   if        flggei = '0'
349600011012     C                   clear                   dgei
349700011012     C                   clear                   tibs02ds
349800011012     C                   movel     'L'           t02tla
349900011012     C                   movel     'C'           t02mod
350000011012     C                   movel     knsif         t02sif
350100011012     C                   movel     'GEI'         t02cod
350200011012     C                   movel     tstdiv        t02ke1
350300011012     C                   call      'TIBS02R'
350400011012     C                   parm                    kpjba
350500011012     C                   parm                    tibs02ds
350600011012     C                   movel     t02uni        dgei
350700011012     C                   eval      flggei = '1'
350800011012     C                   endif
350900951113     C* CONTROLLO LE SELEZIONI --> SE NON RIENTRA RILEGGO
351000960212    1C  N30*IN04         IFEQ      *OFF
351100951113     C     TSTPDR        OREQ      WPDR
351200951113     C*
351300960212    2C     *IN03         IFEQ      *OFF
351400951113     C     TSTTRN        OREQ      WTRN
351500951113     C*
351600960212    3C     *IN02         IFEQ      *OFF
351700951113     C     TSTTFP        OREQ      V1CTFP
351800951215     C*
351900960212    4C     V1CVIM        IFEQ      'C'
352000951215     C     TSTCNF        ANDNE     ' '
352100960213    5C     V1CVIM        OREQ      ' '
352200951113     C                   MOVEL     'S'           WSEL              1
352300960212   X4C                   ELSE
352400960212     C*
352500960212     C* SE HO RICHIESTO GLI IMPORTO DA CONFERMARE O TUTTI
352600960212     C*  DO SOLO QUELLI GIA' CONTROLLATI
352700960213     C     V1CVIM        IFEQ      'N'
352800960212     C     TSTCNF        ANDEQ     ' '
352900960212     C* CHAIN SULLA FERMATA
353000960212     C                   Z-ADD     TSTTRN        KVTRN
353100960212     C                   Z-ADD     TSTDTN        KVDTN
353200960212     C                   Z-ADD     TSTNFE        KVNFP
353300980121     C*****      KTSR12    CHAINTNTSR01L             31
353400980121     C     KTSR12        CHAIN(N)  TNTSR01L                           31
353500960229    6C  N31TSRFKI        IFNE      ' '
353600960212     C                   MOVEL     'S'           WSEL              1
353700960212    6C                   ENDIF
353800960212    5C                   ENDIF
353900960212    4C                   ENDIF
354000960212    3C                   ENDIF
354100960212    2C                   ENDIF
354200960212    1C                   ENDIF
354300960212    0C                   ENDDO
354400951106     C*
354500951103     C                   ENDSR
354600951213     C**
354700951213     C**********LETTURA TRATTE TNTSR PER SFL04 ************************
354800951213     C     LEGSR4        BEGSR
354900951213     C*
355000951213     C                   SETOFF                                       30
355100951213     C* NUMERO FV
355200951213    1C     V1CNFV        IFGT      0                                            NUM FV
355300951213     C                   Z-ADD     V1CNFV        KNFV
355400980121     C*****      KTSR4     READETNTSR04L                 30
355500980121     C     KTSR4         READE(N)  TNTSR04L                               30
355600951213     C*
355700951213   X1C                   ELSE                                                   TER.PARTENZA
355800960322    2C     *IN09         IFEQ      *ON
355900951213     C     KLNP          READE     TNTSR02L                               30
356000951213   X2C                   ELSE
356100951213     C     KTSR2         READE     TNTSR02L                               30
356200951213    2C                   ENDIF
356300951213    1C                   ENDIF
356400951213     C*
356500951213     C                   ENDSR
356600951102     C**
356700951102     C********** CONTROLLA LE DATE RICHIESTA CON LA DATA AREA *********
356800951102     C     CTRDAT        BEGSR
356900951102     C*
357000951103     C     WDTAL         IFEQ      0
357100951103     C                   Z-ADD     W0080         WDTAL
357200951102     C                   ENDIF
357300951103     C*
357400951108     C                   Z-ADD     WTOTGG        W0050
357500951102     C*
357600951103    1C     W0080         DOWLE     WDTAL
357700951102     C     *IN90         ANDEQ     *OFF
357800951102     C*
357900951102    2C     W0080         IFGT      D30DAT
358000951102     C                   MOVEL     MSG(16)       V1CMSG
358100951102     C                   MOVEA     MSG(16)       K78
358200951102     C                   MOVEL     W0080         WDATA
358300951102     C                   MOVEA     WGG           K78(65)
358400951102     C                   MOVEA     WMM           K78(68)
358500951102     C                   MOVEA     WAA           K78(71)
358600951106     C                   MOVEA     K78           V1CMSG
358700960322     C                   SETON                                        9028
358800951102   X2C                   ELSE
358900951102     C*
359000951108     C                   ADD       1             W0050             5 0
359100951108     C                   Z-ADD     W0050         GIOTGI
359200951102     C                   CALL      'XSRGI8'
359300951102     C                   PARM                    WGIDAT
359400951102     C*
359500951102     C                   Z-ADD     GIOINV        W0080
359600951102    1C                   ENDIF
359700951102     C                   ENDDO
359800951102     C*
359900951102     C                   ENDSR
360000951103     C**
360100951214     C**************** PULIZIA  SFL02 E SFL04 *************************
360200951214     C     PULSFL        BEGSR
360300951103     C                   SETOFF                                       21
360400951214     C*
360500951214     C     KSFL          IFEQ      2
360600010824     C                   WRITE     TL44C02
360700951113     C                   CLEAR                   NRR2
360800951113     C                   CLEAR                   V2CREC
360900951113     C                   CLEAR                   KNFP
361000960322     C                   CLEAR                   WLNFP
361100951116     C                   MOVEL     'S'           WLCTE
361200951214     C                   ELSE
361300010824     C                   WRITE     TL44C04
361400951214     C                   CLEAR                   NRR4
361500951214     C                   CLEAR                   V4CREC
361600960322     C                   CLEAR                   WLNFP
361700960322     C                   CLEAR                   WLTRN
361800960322     C                   CLEAR                   WLDTN
361900951214     C                   ENDIF
362000951214     C*
362100951214     C                   SETON                                        21
362200951212     C                   ENDSR
362300951103     C**
362400951103     C**************** CARICAMENTO TESTATA IN SFL02 *******************
362500951103     C     CTST2         BEGSR
362600951109     C                   CLEAR                   TNTSW000                       file
362700951106     C                   Z-ADD     KSFL          TSWSFL
362800951106     C                   MOVEL     KNMUS         TSWNUS
362900951106     C                   MOVEL     KNRJO         TSWNJO
363000951106     C                   MOVEL     KNMEB         TSWNMB
363100951106     C                   Z-ADD     WF20          TSWF20
363200951109     C                   MOVEL     'T'           TSWTRC
363300951109     C                   Z-ADD     TSTTFP        TSWTFP
363400951109     C                   Z-ADD     TSTTFA        TSWTFA
363500951109     C                   MOVEL     TSTDTN        TSWDTN
363600951109     C                   MOVEL     TSTTRN        TSWTRN
363700951117     C                   MOVEL     TSTFES        TSWFES
363800951124     C                   MOVEL     TSTNFE        TSWNFE
363900010827     C**!!!              MOVEL     TSTILI        TSWILI
364000010827     C**!!!              MOVEL     TSTIPT        TSWIPT
364100010827     C**!!!              MOVEL     TSTIES        TSWIES
364200010827     C                   z-add     TSTILI        TSWILI
364300010827     C                   z-add     TSTIPT        TSWIPT
364400010827     C                   z-add     TSTIES        TSWIES
364500951113     C                   MOVEL     TSTCAU        TSWCAU
364600951113     C                   MOVEL     TSTCAU        TSWVCU
364700010827     C**!!!              MOVEL     TSTICO        TSWICO
364800010827     C**!!!              MOVEL     TSTICO        TSWVIC
364900010827     C                   z-add     TSTICO        TSWICO
365000010827     C                   z-add     TSTICO        TSWVIC
365100951124     C                   MOVEL     TSTCNF        TSWCNF
365200951109     C                   MOVEL     TSTTMZ        TSWTMZ
365300951109     C                   MOVEL     TSTBIS        TSWBIS
365400951109     C                   MOVEL     TSTPDR        TSWPDR
365500951109     C                   MOVEL     TSTPDR        TSWVPD
365600010827
365700010827     c                   movel     tstdiv        tswdiv
365800010827
365900960229     C* SE FATTURATO NON MODIFICO PIU' NULLA
366000951109     C     TSTDFT        IFGT      0
366100951109     C                   MOVEL     'S'           TSWFAT
366200951109     C                   ENDIF
366300960213     C* SE ANNULLATO PROTEGGO TUTTO
366400960213     C                   MOVEL     TSTATB        TSWATB
366500960213     C                   MOVEL     TSTATB        TSWATV
366600960229     C                   MOVEL     TSTATB        TSWATT
366700960229     C**
366800960213     C                   MOVEL     TSTATB        V2CSCE
366900960229     C                   MOVEL     TSTATB        V2HATT
367000960213     C* SE L'IMPORTO NON C'E' VERIFICO SE E' GIA' STATO CONTROLLATO
367100960213     C     TSTICO        IFEQ      0
367200960213     C                   Z-ADD     TSTTRN        KVTRN
367300960213     C                   Z-ADD     TSTDTN        KVDTN
367400960213     C                   Z-ADD     TSTNFE        KVNFP
367500980121     C***        KTSR12    CHAINTNTSR01L             31
367600980121     C     KTSR12        CHAIN(N)  TNTSR01L                           31
367700960213     C* PROTEZIONE IMPORTO DA CONFERMARE
367800960213     C                   CLEAR                   V2HPRP
367900960213     C                   CLEAR                   V2HPRI
368000960229     C  N31TSRFKI        IFEQ      ' '
368100960213     C                   MOVEL     'S'           TSWPRI
368200960213     C                   MOVEL     'S'           V2HPRI
368300960213     C                   ENDIF
368400960213     C*
368500960229     C* PROTEZIONE TRAZIONISTA E CASUALE CONTABILIZZAZIONE
368600960229     C  N31TSRFKP        IFEQ      ' '
368700960213     C                   MOVEL     'S'           TSWPRP
368800960213     C                   MOVEL     'S'           V2HPRP
368900960213     C                   ENDIF
369000960213     C                   ENDIF
369100951109     c*
369200960229     C                   CLEAR                   V2CDTT
369300951107     C                   CLEAR                   V2CFFP
369400951109     C                   CLEAR                   V2CNFV
369500951109     C                   CLEAR                   V2HRFV
369600951109     C                   CLEAR                   V2HNFV
369700951109     C                   CLEAR                   V2HNFP
369800960229     C                   CLEAR                   V2HPRF
369900960229     C                   CLEAR                   V2HTMZ
370000960229     C                   MOVEL     'T'           V2HTRC                         SFL
370100951109     C                   Z-ADD     1             V2HNRC
370200951106     C*
370300010824     C                   Z-ADD     TSTTFP        V2hTFP
370400010824     C                   Z-ADD     TSTTFP        V2cter
370500951113     C                   MOVEL     TSTBIS        V2CBIS
370600010827     C                   movel     tstdiv        V2cdiv
370700951109     C*
370800951214     C                   MOVEL     TSTDTN        V2HDTN
370900951103     C                   MOVEL     TSTDTN        WDATA
371000951109     C                   MOVEA     WMM           K7(6)
371100951109     C                   MOVEA     WGG           K7(3)
371200951109     C                   MOVEL     '/'           K7(5)
371300951109     C                   CLEAR                   V2CDES
371400951109     C                   MOVEA     K7            V2CDES
371500951109     C*
371600951113     C                   MOVEL     TSTTRN        V2HTRN
371700010824     C**!!!              MOVEL     TSTTFA        V2CTFA
371800951108     C                   MOVEL     TSTTMZ        V2HTMZ
371900010827     C**!!!              MOVEL     TSTILI        V2CIMP
372000010827     C**!!!              MOVEL     TSTIPT        V2HIPT
372100010827     C                   z-add     TSTILI        V2CIMP
372200010827     C                   z-add     TSTIPT        V2HIPT
372300951113     C                   MOVEL     TSTCAU        V2CCAU
372400960221     C                   MOVEL     TSTCNF        V2HCNF
372500010827     C**!!!              MOVEL     TSTICO        V2CICO
372600010827     C**!!!              MOVEL     TSTICO        V2HICO
372700010827     C**!!!              MOVEL     TSTICO        WSICO
372800010827     C                   z-add     TSTICO        V2CICO
372900010827     C                   z-add     TSTICO        V2HICO
373000010827     C                   z-add     TSTICO        WSICO
373100951107     C                   MOVEL     'S'           WFORZ
373200951109     C                   MOVEL     TSWFAT        V2HFAT
373300951103     C* DECODIFICA TRAZIONISTA
373400951106     C     TSTPDR        IFGT      0
373500951108     C                   MOVEL     TSTPDR        V2HPDR
373600951106     C                   MOVEL     TSTPDR        V2CPDR
373700000807     C                   Z-ADD     TSTPDR        KTLPDR
373800000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
373900951107     C  N31              MOVEL     TLZRSC        V2DPDR
374000951106     C  N31              MOVEL     TLZRSC        TSWDPD
374100951107     C   31              CLEAR                   V2DPDR
374200951106     C                   ELSE
374300951108     C                   CLEAR                   V2DPDR
374400951108     C                   CLEAR                   V2CPDR
374500951108     C                   CLEAR                   V2HPDR
374600951108     C* VADO SUI FV A PRENDERE LA DESCRIZIONE
374700951108     C     KTSR3         CHAIN     TNTSR03L                           31
374800951108     C     *IN31         IFEQ      *OFF
374900960305     C                   Z-ADD     T03LNP        KLNP
375000960305     C                   Z-ADD     T03NFV        KNFV
375100980121     C***        KTSR4     CHAINTNFVS01L             31
375200980121     C     KTSR4         CHAIN(N)  TNFVS01L                           31
375300951108     C  N31              MOVEL     FVSDPD        V2DPDR
375400951109     C  N31              MOVEL     FVSDPD        TSWDPD
375500951108     C                   ENDIF
375600951106     C                   ENDIF
375700951103     C*
375800951107     C* SISTEMO GLI INDICATORI
375900951107     C                   EXSR      CTRIND
376000951103     C*
376100951103     C                   ADD       1             L
376200951103     C                   ADD       1             NRR2
376300951106     C                   Z-ADD     NRR2          TSWNRR
376400010824     C                   WRITE     TL44S02
376500951106     C                   WRITE     TNTSW000
376600951116     C*
376700951116     C                   EXSR      RICNRR
376800020408      * se è la riga 19
376900020408     c                   if        l = wripag
377000020408     c                   eval      w_tsrtrn = tsttrn
377100041001     c                   eval      w_tsrdtn = tstdtn
377200020408     c                   endif
377300020408
377400951116     C                   ENDSR
377500951116     C**
377600951116     C*********** CERCO NRR DEL RECORD DA VISUALIZZARE PER F19F20******
377700951116     C     RICNRR        BEGSR
377800951116     C* CERCO 3 NUMERI DI RECORD:
377900960221     C* 1) QUELLO CON TRAINO = A QUELLO RICHIESTO
378000951116     C     TSWTRN        IFGT      0
378100951116     C     WNRRA         ANDEQ     0
378200951116     C     TSWTRN        ANDEQ     WCTRN
378300951116     C                   Z-ADD     TSWNRR        WNRRA
378400951116     C                   ENDIF
378500951116     C* 2) QUELLO CON TERMINAL PARTENZA = A QUELLO RICHIESTO
378600951116     C     TSWTFP        IFGT      0
378700951116     C     WNRRB         IFEQ      0
378800951116     C     TSWTFP        ANDEQ     WCTFP
378900951116     C                   Z-ADD     TSWNRR        WNRRB
379000951116     C                   ENDIF
379100951116     C* 3) QUELLO CON TERMINAL PARTENZA SUBITO PRIMA DEL TERMINAL RICH
379200951116     C     TSWTFP        IFLT      WCTFP
379300951116     C     TSWTFP        ANDGT     WSTFP
379400951116     C                   Z-ADD     TSWNRR        WNRRC
379500951116     C                   Z-ADD     TSWTFP        WSTFP
379600951116     C                   ENDIF
379700951116     C                   ENDIF
379800951116     C                   ENDSR
379900951113     C**
380000951113     C*************** ABBLANCA ZERO NON SIGNIFICATIVI *****************
380100951109     C     ABBL          BEGSR
380200951109     C*
380300951109     C     SKI(I)        DOWEQ     '0'
380400951109     C     I             ANDLT     15
380500951109     C                   MOVEL     ' '           SKI(I)
380600951109     C                   ADD       1             I
380700951109     C                   ENDDO
380800951109     C*
380900951109     C                   ENDSR
381000951103     C**
381100951103     C**************** CARICAMENTO TRATTE IN SFL02 ********************
381200951103     C     CTSR2         BEGSR
381300951113     C                   CLEAR                   TNTSW000                       file
381400951113     C                   Z-ADD     KSFL          TSWSFL
381500951113     C                   MOVEL     KNMUS         TSWNUS
381600951113     C                   MOVEL     KNRJO         TSWNJO
381700951113     C                   MOVEL     KNMEB         TSWNMB
381800951113     C                   Z-ADD     WF20          TSWF20
381900960213     C                   MOVEL     TSTATB        TSWATT
382000960213     C                   Z-ADD     TSTTFP        TSWTFP
382100951113     C                   Z-ADD     TSTTFA        TSWTFA
382200951113     C                   MOVEL     TSTDTN        TSWDTN
382300951113     C                   MOVEL     TSTTRN        TSWTRN
382400010827     C**!!!              MOVEL     TSTIPT        TSWIPT
382500010827     C                   z-add     TSTIPT        TSWIPT
382600010827     C                   MOVEL     TSTFES        TSWFES
382700951124     C                   MOVEL     TSTNFE        TSWNFE
382800010827
382900010827     c                   movel     tstdiv        tswdiv
383000010827
383100960229     C* SE FATTURATO NON MODIFICO PIU' NULLA
383200960229     C     TSTDFT        IFGT      0
383300960229     C                   MOVEL     'S'           TSWFAT
383400960229     C                   ENDIF
383500951113     C*
383600951113     C                   MOVEL     'R'           TSWTRC
383700951109     C                   Z-ADD     TSRNFV        TSWNFV
383800951109     C                   Z-ADD     TSRNFV        TSWVNF
383900960229     C                   MOVEL     TSRATB        TSWATB
384000960213     C                   MOVEL     TSRATB        TSWATV
384100951109     C                   MOVEL     TSRDTT        TSWDTT
384200951109     C                   Z-ADD     TSRNFP        TSWNFP
384300951109     C                   Z-ADD     TSRFFP        TSWFFP
384400951109     C*
384500951109     C                   MOVEL     'R'           V2HTRC                         SFL
384600951108     C                   ADD       1             V2HNRC
384700960213     C                   MOVEL     TSRATB        V2CSCE
384800960229     C                   MOVEL     TSTATB        V2HATT                         ANN. TESTATA
384900960229     C                   MOVEL     TSWFAT        V2HFAT
385000960229     C                   MOVEL     TSTDTN        V2HDTN
385100960229     C                   MOVEL     TSTTRN        V2HTRN
385200010827     C**!!!              CLEAR                   V2CTFA
385300010827     C                   CLEAR                   V2Cdiv
385400951109     C                   CLEAR                   V2HIPT
385500951109     C                   CLEAR                   V2CBIS
385600960213     C                   CLEAR                   V2HPRP
385700960213     C                   CLEAR                   V2HPRI
385800960229     C                   CLEAR                   V2HPRF
385900960229     C* I CAMPI V2HFAT, V2HTRN, V2HDTN RIMANGONO IMPOSTATI DALLA
386000960229     C*  LETTURA DI TNTST
386100960229     C**
386200960229     C* SE NON E' STATO CONTROLLATO, PROTEGGO IL CAMPO
386300960229     C*  E NON POSSO NEMMENO GESTIRE L'ANNULLAMENTO
386400960229    1C     TSRFKP        IFEQ      ' '
386500960229     C                   MOVEL     'S'           TSWPRF
386600960229     C                   MOVEL     'S'           V2HPRF
386700960229    1C                   ENDIF
386800951215     C* SOLO NELLA 1 RIGA DI FV  MEMORIZZO L'IMPORTO FV E NUM.TRAINO
386900960213    1C     V2HNRC        IFEQ      2
387000951109     C* RIEMPO IL CAMPO IMPORTO FV
387100951113     C                   Z-ADD     TSTIPT        V2CIMP
387200951109     C* NUMERO TRAINO
387300951113     C                   MOVEL     TSTTRN        W007A             7
387400951109     C                   MOVEA     W007A         SKI(9)
387500951109     C                   Z-ADD     9             I                 3 0
387600951109     C                   EXSR      ABBL
387700951109     C                   MOVEA     SKI(9)        V2CDES
387800010827
387900010827     C                   z-add     tsttfa        V2cter
388000010827
388100951108     C*
388200960213   X1C                   ELSE
388300951108     C                   CLEAR                   V2CIMP
388400951113     C                   CLEAR                   V2CDES
388500960213    1C                   ENDIF
388600960213     C*
388700960213    1C     TSRNFV        IFEQ      0
388800951107     C                   CLEAR                   V2CNFV
388900951107     C                   MOVEL     *ZEROS        V2HNFV
389000951107     C                   ELSE
389100951107     C                   MOVEL     TSRNFV        V2CNFV
389200951107     C                   MOVEL     TSRNFV        V2HNFV
389300960213    1C                   ENDIF
389400951107     C                   MOVEL     TSRDTT        WDATA
389500951107     C                   MOVE      WMM           V2CDTT
389600951107     C                   MOVEL     WGG           V2CDTT
389700951103     C*
389800951107     C                   Z-ADD     TSRNFP        V2HNFP
389900951107     C                   Z-ADD     TSRFFP        V2CFFP
390000951215     C* SE LA DATA FVT E' DIVERSA DALLA DATA FV O NON C'E' FV
390100951103     C* METTO IN RI IL CAMPO DEL FV
390200960213    1C     TSRNFV        IFEQ      0
390300951103     C     TSRDTT        ORNE      TSRDFV
390400951106     C                   MOVEL     'S'           V2HRFV
390500951106     C                   MOVEL     'S'           TSWRFV
390600960213     C                   MOVEL     'S'           TSWRFI
390700951103     C                   ELSE
390800951103     C                   MOVEL     ' '           V2HRFV
390900951106     C                   MOVEL     ' '           TSWRFV
391000960213     C                   MOVEL     ' '           TSWRFI
391100960213    1C                   ENDIF
391200951107     C* SISTEMO GLI INDICATORI
391300951107     C                   EXSR      CTRIND
391400951103     C*
391500951103     C                   ADD       1             L
391600951103     C                   ADD       1             NRR2
391700951106     C                   Z-ADD     NRR2          TSWNRR
391800010824     C                   WRITE     TL44S02
391900951107     C                   WRITE     TNTSW000
392000951103     C                   ENDSR
392100951213     C**
392200951213     C**************** CARICAMENTO TRATTE IN SFL04 ********************
392300951213     C     CTSR4         BEGSR
392400951213     C                   CLEAR                   TNTSW000                       file
392500951213     C                   Z-ADD     KSFL          TSWSFL
392600951213     C                   MOVEL     KNMUS         TSWNUS
392700951213     C                   MOVEL     KNRJO         TSWNJO
392800951213     C                   MOVEL     KNMEB         TSWNMB
392900951213     C                   Z-ADD     WF20          TSWF20
393000951213     C                   Z-ADD     TSRFFP        TSWFFP
393100951213     C                   Z-ADD     TSRNFP        TSWNFP
393200951213     C                   Z-ADD     TSRDTT        TSWDTT
393300951213     C                   MOVEL     TSRDTN        TSWDTN
393400951213     C                   MOVEL     TSRTRN        TSWTRN
393500960322     C                   MOVEL     TSRATB        TSWATB
393600960322     C                   MOVEL     TSRATB        TSWATV
393700951213     C*
393800951213     C                   MOVEL     'R'           TSWTRC
393900951213     C                   Z-ADD     TSRNFV        TSWNFV
394000951213     C                   Z-ADD     TSRNFV        TSWVNF
394100951213     C*
394200960322    1C     TSRNFV        IFEQ      0
394300951213     C                   CLEAR                   V4CNFV
394400951213     C                   MOVEL     *ZEROS        V4HNFV
394500951213     C                   ELSE
394600951213     C                   MOVEL     TSRNFV        V4CNFV
394700951213     C                   MOVEL     TSRNFV        V4HNFV
394800960322    1C                   ENDIF
394900951213     C*
395000960322     C                   CLEAR                   V4CSCE
395100960322     C                   MOVEL     TSRATB        V4HATB
395200960322     C                   MOVEL     TSRTRN        V4CTRN
395300951213     C                   MOVEL     TSRDTN        V4HDTN
395400951213     C                   MOVEL     TSRDTN        WDATA
395500951213     C                   MOVE      WMM           V4CDTN
395600951213     C                   MOVEL     WGG           V4CDTN
395700951213     C*
395800951213     C                   MOVEL     TSRDTT        V4HDTT
395900951213     C                   MOVEL     TSRDTT        WDATA
396000951213     C                   MOVE      WMM           V4CDTT
396100951213     C                   MOVEL     WGG           V4CDTT
396200951213     C*
396300951213     C                   Z-ADD     TSRFFP        V4CFFP
396400951214     C                   Z-ADD     TSRNFP        V4HNFP
396500951215     C* SE LA DATA FVT  E' DIVERSA DALLA DATA FV O NON C'E' FV
396600951213     C* METTO IN RI IL CAMPO DEL FV
396700960322    1C     TSRNFV        IFEQ      0
396800951213     C     TSRDTT        ORNE      TSRDFV
396900951213     C                   MOVEL     'S'           V4HRFV
397000951213     C                   MOVEL     'S'           TSWRFV
397100960322     C                   MOVEL     'S'           TSWRFI
397200960322   X1C                   ELSE
397300951213     C                   MOVEL     ' '           V4HRFV
397400951213     C                   MOVEL     ' '           TSWRFV
397500960322     C                   MOVEL     ' '           TSWRFI
397600960322    1C                   ENDIF
397700960322     C*
397800960322     C* SE NON E' STATO CONTROLLATO PROTEGGO IL CAMPO
397900960322     C*  E NON POSSO NEMMENO ANNULLARLO
398000960322    1C     TSRFKP        IFEQ      ' '
398100960322     C                   MOVEL     'S'           TSWPRF
398200960322     C                   MOVEL     'S'           V4HPRF
398300960322     C                   ELSE
398400960322     C                   CLEAR                   V4HPRF
398500960322    1C                   ENDIF
398600960322     C* DATA PARTENZA TEORICA
398700960322     C                   MOVEL     TSRDTP        WDATA
398800960322     C                   MOVE      WMM           V4CDTP
398900960322     C                   MOVEL     WGG           V4CDTP
399000960322     C                   MOVEL     TSRHTP        V4CHTP
399100951213     C* CHAIN SU TESTATA
399200951213     C                   Z-ADD     TSRTRN        KTRN
399300951213     C                   Z-ADD     TSRDTN        KDTN
399400951214     C     KTS1          CHAIN     TNTST01L                           31
399500960322    1C     *IN31         IFEQ      *OFF
399600951213     C                   MOVEL     TSTTFP        V4CTFP
399700951213     C                   MOVEL     TSTTFA        V4CTFA
399800951213     C                   MOVEL     TSTTFP        TSWTFP
399900951213     C                   MOVEL     TSTTFA        TSWTFA
400000960322     C                   MOVEL     TSTBIS        TSWBIS
400100960322     C                   MOVEL     TSTBIS        V4CBIS
400200960322     C                   MOVEL     TSTATB        TSWATT
400300960322     C                   MOVEL     TSTATB        V4HATT
400400951213     C*
400500951213     C* SE IMPORTO GIA' CONFERMATO NON POSSO FARE PIU' MODOFICARE I CAM
400600960322    2C     TSTDFT        IFGT      0
400700951213     C                   MOVEL     'S'           TSWFAT
400800951213     C                   MOVEL     'S'           V4HFAT
400900960322   X2C                   ELSE
401000951214     C                   CLEAR                   TSWFAT
401100951214     C                   CLEAR                   V4HFAT
401200960322    2C                   ENDIF
401300951213     C*
401400951213     C                   ELSE
401500951213     C                   CLEAR                   V4CTFP
401600960322     C                   CLEAR                   V4CTFA
401700960322     C                   CLEAR                   V4HFAT
401800960322     C                   CLEAR                   V4HATT
401900951213     C                   ENDIF
402000960325     C*
402100951213     C* SISTEMO GLI INDICATORI
402200951213     C                   EXSR      CTRIND
402300951213     C*
402400951213     C                   ADD       1             L
402500951213     C                   ADD       1             NRR4
402600951213     C                   Z-ADD     NRR4          TSWNRR
402700010824     C                   WRITE     TL44S04
402800951213     C                   WRITE     TNTSW000
402900951213     C                   ENDSR
403000951103     C**
403100951103     C**************** VISUALIZZA TUTTI I DATI DEL TRAINO *************
403200951124     C     VISGRL        BEGSR
403300010824     C                   CLEAR                   TL44Z03
403400010824     C                   CLEAR                   TL44C03
403500980114     C* sostituisco la visualizzazione dei dati arrivo del traino con un
403600980114     C* subfile in quanto risulta necessario avere + di 4 righe arrivi
403700010824     C                   CLEAR                   TL44C13
403800951103     C*
403900960624     C                   SETOFF                                       212027
404000980114     C                   SETOFF                                       3738
404100980114     C*****                SETOF                     353637
404200980114     C*****                SETOF                     3839
404300010824     C                   WRITE     TL44C03
404400951107     C                   SETON                                        2123
404500951214     C                   SETON                                        151691
404600951214     C                   SETON                                        9293
404700951106     C                   Z-ADD     0             NRR3              5 0
404800951103     C*
404900951103     C* CARICO DATI --> T E S T A T A   T R A I N O
405000951214     C     KTST11        CHAIN     TNTST01L                           31
405100951106    1C     *IN31         IFEQ      *OFF
405200951103     C                   Z-ADD     TSTTRN        V3CTRN
405300951103     C                   MOVE      TSTDTN        G02INV
405400951103     C                   MOVE      *ZEROS        G02DAT
405500951103     C                   MOVEL     '3'           G02ERR
405600951103     C                   CALL      'XSRDA8'
405700951103     C                   PARM                    WLBDAT
405800951103     C                   Z-ADD     G02DAT        V3CDTN
405900951103     C* BIS
406000951103    2C     TSTBIS        IFEQ      'B'
406100951103     C                   MOVEL     'B  I  S'     V3DBIS
406200980119    2C                   ENDIF
406300960301    2C     TSTBIS        IFEQ      'E'
406400951103     C                   MOVEL     'ECCEZION'    V3DBIS
406500951103     C                   MOVE      'ALE'         V3DBIS
406600951103    2C                   ENDIF
406700960301     C* ANNULLATO
406800980119    2C     TSTATB        IFEQ      'A'
406900960301     C                   SETON                                        20
407000980119    2C                   ENDIF
407100951103     C* TIPO TRAINO
407200951103     C                   MOVEL     TSTTTR        V3CTTR
407300951103     C                   MOVEL     'TV'          COD
407400951103     C                   MOVEL(P)  TSTTTR        KEY
407500951103     C     KTAB          CHAIN     TABEL                              31
407600951103     C  N31              MOVEL     TBLUNI        V3DTTR
407700951103     C   31              MOVEL     *ALL'?'       V3DTTR
407800951103     C*
407900951103     C                   MOVEL     TSTTFP        V3CTFP
408000951103     C                   MOVEL     TSTTFP        KFIL
408100951103     C                   EXSR      CAZORG
408200951103     C                   MOVEL     ORGDES        V3DTFP
408300951103     C*
408400951103     C                   MOVEL     TSTTMZ        V3CTMZ
408500951103     C                   MOVEL     'TU'          COD
408600951103     C                   MOVEL(P)  TSTTMZ        KEY
408700951103     C     KTAB          CHAIN     TABEL                              31
408800951103     C  N31              MOVEL     TBLUNI        V3DTMZ
408900951103     C   31              MOVEL     *ALL'?'       V3DTMZ
409000951103     C*
409100951103     C                   MOVEL     TSTTFA        V3CTFA
409200951103     C                   MOVEL     TSTTFA        KFIL
409300951103     C                   EXSR      CAZORG
409400951103     C                   MOVEL     ORGDES        V3DTFA
409500951103     C*
409600951103     C                   MOVEL     TSTPDR        V3CPDR
409700951103    2C     TSTPDR        IFGT      0
409800000807     C                   Z-ADD     TSTPDR        KTLPDR
409900000807     C     KTNTLZ        CHAIN     TNTLZ01L                           31
410000951103     C  N31              MOVEL     TLZRSC        V3DPDR
410100951103     C   31              MOVEL     *ALL'?'       V3DPDR
410200951103     C                   ELSE
410300951103     C                   MOVEL     *ALL'?'       V3DPDR
410400951103    2C                   ENDIF
410500951103     C*
410600010824     c                   eval      v3cdiv = tstdiv
410700951103     C                   Z-ADD     TSTILI        V3CILI
410800951103     C                   Z-ADD     TSTIPT        V3CIPT
410900951103     C                   Z-ADD     TSTICO        V3CICO
411000960222     C                   Z-ADD     TSTFES        V3CFES
411100951113     C* CAUSALE CONTABILIZZAZIONE
411200951113     C                   MOVEL     TSTCAU        V3CCAU
411300980119    2C     TSTCAU        IFEQ      ' '
411400951113     C                   CLEAR                   V3DCAU
411500951113     C                   ELSE
411600000214     C* Reperisco i dati della causale di contabilizzazione
411700000214     C* dalle tabelle di PROJ con apposita Routine
411800000218     C                   MOVE      KNSIF         WCOMO4            4
411900000218     C                   MOVEL     WCOMO4        WSOC              3
412000000218     C                   movel     WSOC          KAtazi
412100000218     C                   MOVEL     'Y4Z'         Katcod
412200000218     C                   MOVEL     *blanks       Katkey
412300000218     C                   MOVEL(P)  V2CCAU        Katkey
412400000218     C     KEATAB        CHAIN     ANATB01L                           30
412500060727     c                   if        not %found(anatb01l)
412600000218     C                   MOVE      '888'         Katazi
412700000218     C     KEATAB        CHAIN     ANATB01L                           30
412800060727     c                   end
412900000218     C                   if        not *in30
413000000218     C                   MOVEL     ATBUNI        V3DCAU
413100000214     C                   else
413200000214     C                   MOVE      *BLANKS       V3DCAU
413300000214     C   31              MOVEL     *ALL'?'       V3DCAU
413400000214     C                   end
413500000214     C                   end
413600951113     C*
413700951103     C                   MOVEL     TSTNFT        V3CNFT
413800951103    2C     TSTDFT        IFGT      0
413900951103     C                   MOVE      TSTDFT        G02INV
414000951103     C                   MOVE      *ZEROS        G02DAT
414100951103     C                   MOVEL     '3'           G02ERR
414200951103     C                   CALL      'XSRDA8'
414300951103     C                   PARM                    WLBDAT
414400951103     C                   Z-ADD     G02DAT        V3CDFT
414500951103    2C                   ENDIF
414600951103     C**
414700951103     C* CARICO DATI SFL --> M E T R I   C U B I
414800951103     C                   CLEAR                   NRR3
414900960222     C                   CLEAR                   X
415000960222     C                   CLEAR                   MCT
415100951214     C     KTST11        SETLL     TNTSM01L
415200960226     C     KTST11        READE(N)  TNTSM01L                               31
415300951109     C* SE NON TROVATO FACCIO UNA CHAIN FITTIZIA
415400980119    2C     *IN31         IFEQ      *ON
415500951109     C                   Z-ADD     1             NRR3
415600010824     C                   WRITE     TL44S03
415700980119    2C                   ENDIF
415800951109     C*
415900951103    2C     *IN31         DOWEQ     *OFF
416000960222     C* CARICO LA SKIERA DEI MC DELLA FILIALE DI CARICO
416100980119    3C     TSMTFP        IFEQ      KVFFP
416200960222     C                   ADD       1             X                 3 0
416300960222     C                   Z-ADD     TSMTFA        MCT(X)
416400960222     C                   MOVEL     TSMTFP        MCT(X)
416500980119    3C                   ENDIF
416600951103     C*
416700951103     C                   Z-ADD     TSMTFP        V3CMTP
416800951103     C                   Z-ADD     TSMTFP        KFIL
416900951103     C*
417000951103     C                   Z-ADD     TSMTFA        V3CMTA
417100951103     C                   Z-ADD     TSMTFA        KFIL
417200951103     C                   EXSR      CAZORG
417300980119    3C     ORGDE5        IFNE      *BLANKS
417400960301     C                   MOVEL     ORGDE5        V3DMTA
417500960301     C                   ELSE
417600951103     C                   MOVEL     ORGDES        V3DMTA
417700980119    3C                   ENDIF
417800951103     C*
417900951103     C                   Z-ADD     TSMTFF        V3CMTF
418000951103     C                   Z-ADD     TSMTFF        KFIL
418100951103     C                   EXSR      CAZORG
418200980119    3C     ORGDE5        IFNE      *BLANKS
418300960301     C                   MOVEL     ORGDE5        V3DMTF
418400960301     C                   ELSE
418500951103     C                   MOVEL     ORGDES        V3DMTF
418600980119    3C                   ENDIF
418700951103     C*
418800960221     C                   Z-ADD     TSMMCT        V3CMCT
418900960710     C                   Z-ADD     TSMMCT        V3HMCT
419000960221     C                   Z-ADD     TSMMCA        V3CMCA
419100960625     C                   Z-ADD     TSMMCA        V3HMCA
419200960424     C                   Z-ADD     TSMMCB        V3CMCB
419300960710     C                   Z-ADD     TSMMCB        V3HMCB
419400960424     C                   Z-ADD     TSMMCR        V3CMCR
419500980119    3C     TSMATB        IFEQ      'A'
419600960301     C                   SETON                                        89
419700960301     C                   ELSE
419800960301     C                   SETOFF                                       89
419900980119    3C                   ENDIF
420000951103     C*
420100951103     C                   ADD       1             NRR3
420200010824     C                   WRITE     TL44S03
420300951103     C*
420400951103     C*
420500960226     C     KTST11        READE(N)  TNTSM01L                               31
420600951103    2C                   ENDDO
420700951103     C**
420800951214     C** CARICO DATI --> T R A T T E     SE RICHIESTO
420900951107    2C     KVNFP         IFGT      0
421000951103     C                   SETON                                        14
421100980114     C*********            Z-ADD0         WCONT   20
421200980114     C* pulisco il subfile
421300980114     C                   SETOFF                                       3536
421400980114     C                   SETOFF                                       606162
421500010824     C                   WRITE     TL44C13
421600980116     C                   SETON                                        616360
421700980114     C                   Z-ADD     0             NR13              5 0
421800980114     C*
421900951214     C     KTSR12        SETLL     TNTSR01L
422000980121     C***        KTSR12    READETNTSR01L                 31
422100980121     C     KTSR12        READE(N)  TNTSR01L                               31
422200960222     C* TRATTA NON TROVATA
422300951103    3C     *IN31         IFEQ      *ON
422400951103     C                   SETOFF                                       14
422500960222     C                   ELSE
422600960222     C* CARICO PRIMA I DATI PARTENZA
422700960222     C                   EXSR      CARPAR
422800960222    3C                   ENDIF
422900951214     C*
423000960222     C* POI I DATI DI ARRIVO
423100951214    3C     *IN31         DOWEQ     *OFF
423200980114     C********   WCONT     ANDLT5
423300960222     C* SE LA FERMATA PARTENZA NON E' QUELLA DEL FV
423400960222     C*  VERIFICO SE CI SONO DEI MC
423500960222     C                   Z-ADD     TSRFFA        W0060             6 0
423600960222     C                   MOVEL     KVFFP         W0060
423700960305     C     W0060         LOOKUP    MCT                                    32
423800960222     C*
423900980119    4C     *IN32         IFEQ      *ON
424000980114     C********             ADD  1         WCONT
424100010824     C                   CLEAR                   TL44S13
424200960222     C                   EXSR      CARARR
424300960305     C*
424400960305     C* TOLGO DALL SKIERA TUTTI I RECORD APPENA TROVATI
424500960305     C                   Z-ADD     1             X
424600980119    5C     *IN32         DOWEQ     *ON
424700960305     C     W0060         LOOKUP    MCT(X)                                 32
424800960305     C   32              CLEAR                   MCT(X)
424900960305     C   32              ADD       1             X
425000980119    5C                   ENDDO
425100980119    4C                   ENDIF
425200960222     C*
425300980121     C****       KTSR13    READETNTSR01L                 31
425400980121     C     KTSR13        READE(N)  TNTSR01L                               31
425500951214    3C                   ENDDO
425600951214   X2C                   ELSE
425700951214     C                   SETOFF                                       14
425800951214    2C                   ENDIF
425900951103     C*
426000951103     C* E M I S S I O N E   V I D E A T E
426100951103     C     FORS03        TAG
426200010824     C                   WRITE     TL44T01
426300010824     C                   WRITE     TL44C03
426400010824     C                   WRITE     TL44C13
426500010824     C                   EXFMT     TL44Z03
426600960625     C                   SETOFF                                       2890
426700960625     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
426800960625     C                   SETOFF                                       525354
426900960625     C                   SETOFF                                       5556
427000960710     C* F7  - MANUTENZIONE MC. E CAUSALI RITARDO (SPROTEGGO CAMPI)
427100970826     C* SE LA DATA E ORA SONO =0 LI IMPOSTO = AI TEORICI
427200980114     C*
427300980119    2C  N37*INKG         IFEQ      *ON
427400980114     C                   SETON                                        27
427500980114     C                   SETON                                        37
427600960625     C                   Z-ADD     NRR3          WNRR3
427700960624     C                   Z-ADD     1             NRR3
427800980119    3C     NRR3          DOWLE     WNRR3
427900010824     C     NRR3          CHAIN     TL44S03                            31
428000980119    4C  N31V3CMTP        IFGT      0
428100010824     C                   UPDATE    TL44S03
428200980119    4C                   END
428300960624     C                   ADD       1             NRR3
428400980119    3C                   ENDDO
428500980114     C****       V3CDRP    COMP 0                    35
428600980114     C****       V3CDT1    COMP 0                    36
428700980114     C****       V3CDT2    COMP 0                    37
428800980114     C****       V3CDT3    COMP 0                    38
428900980114     C****       V3CDT4    COMP 0                    39
429000980114     C* se c'è il 38 acceso allora significa che è attiva la videata degli
429100980114     C* arrivi quindi vado a riproteggere i campi
429200980119    3C     *IN38         IFEQ      *ON
429300980114     C                   SETOFF                                       38
429400980119     C                   Z-ADD     NR13          WNR13
429500980114     C                   Z-ADD     1             NR13
429600980119    4C     NR13          DOWLE     WNR13
429700010824     C     NR13          CHAIN     TL44S13                            31
429800980119    5C     *IN31         IFEQ      *OFF
429900980114     C                   SETOFF                                       36
430000980114     C                   SETON                                        16
430100980121     C     NR13          COMP      1                                  8181
430200010824     C                   UPDATE    TL44S13
430300980119    5C                   END
430400980114     C                   ADD       1             NR13
430500980119    4C                   ENDDO
430600980116     C                   SETOFF                                       363552
430700980120     C                   SETON                                        15
430800980120     C                   SETOFF                                       16
430900980119    3C                   ENDIF
431000980114     C                   GOTO      FORS03
431100980119    2C                   END
431200980114     C*
431300980119    2C  N38*INKN         IFEQ      *ON
431400980114     C                   SETON                                        38
431500980119     C                   Z-ADD     NR13          WNR13
431600980114     C                   Z-ADD     1             NR13
431700980119    3C     NR13          DOWLE     WNR13
431800010824     C     NR13          CHAIN     TL44S13                            31
431900980119    4C  N31V3CDT1        IFGT      0
432000980116     C                   SETON                                        36
432100980119     C* CONTROLLO SE ESISTE ALMENO UNA RIGA DI DESCRIZIONE
432200980119     C                   Z-ADD     V3CFF1        KFIL
432300980119     C                   MOVEL     'R'           KWTRC
432400980119     C                   MOVEL     'A'           KEPA
432500980119     C     KFV4          SETLL     FNFV401L                               OC
432600980119     C   OC              SETOFF                                       16
432700980119     C  NOC              SETON                                        16
432800980121     C     NR13          COMP      1                                  8181
432900010824     C                   UPDATE    TL44S13
433000980119    4C                   END
433100980114     C                   ADD       1             NR13
433200980119    3C                   ENDDO
433300980114     C* se c'è il 37 acceso allora significa che è attiva la videata degli
433400980114     C* arrivi quindi vado a riproteggere i campi
433500980119    3C     *IN37         IFEQ      *ON
433600980114     C                   SETOFF                                       37
433700980114     C                   Z-ADD     NRR3          WNRR3
433800980114     C                   Z-ADD     1             NRR3
433900980119    4C     NRR3          DOWLE     WNRR3
434000010824     C     NRR3          CHAIN     TL44S03                            31
434100980119    5C     *IN31         IFEQ      *OFF
434200980114     C                   SETOFF                                       27
434300010824     C                   UPDATE    TL44S03
434400980119    5C                   END
434500980114     C                   ADD       1             NRR3
434600980119    4C                   ENDDO
434700980119    3C                   END
434800980116     C                   SETON                                        3552
434900980119     C* CONTROLLO SE ESISTE ALMENO UNA RIGA DI DESCRIZIONE
435000980119     C                   CLEAR                   KFIL
435100980119     C                   MOVEL     'R'           KWTRC
435200980119     C                   MOVEL     'P'           KEPA
435300980119     C     KFV4          SETLL     FNFV401L                               87
435400980119     C   87              SETOFF                                       15
435500980119     C  N87              SETON                                        15
435600980114     C                   GOTO      FORS03
435700980119    2C                   END
435800951103     C* F12 - RITORNO
435900951106    2C     *INKL         IFEQ      *OFF
436000980119    3C     *IN38         IFEQ      *ON
436100951109     C*
436200980119     C                   SETOFF                                       58
436300010824     C                   READ      TL44C13                                58
436400960624     C                   CLEAR                   W001A
436500980119    4C     *IN58         IFEQ      *OFF
436600951109     C* VISUALIZZA RITARDI PARTENZA
436700980119    5C     V3CVDP        IFEQ      'V'
436800960624     C                   MOVE      '1'           W001A             1
436900960131     C                   CLEAR                   DSLV15
437000960131     C                   MOVEL     'I'           D15TUP
437100960131     C                   MOVEL     'R'           D15TRC
437200960131     C                   MOVEL     V3CRTP        D15RTC
437300960131     C                   MOVEL     'P'           D15EPA
437400960131     C                   MOVEL     V3CLNP        D15LNP
437500960222     C                   MOVEL     V3CTTR        D15TTR
437600960131     C                   MOVEL     V3CNFV        D15NFV
437700960131     C                   MOVEL     V3HDFV        D15DFV
437800960131     C                   MOVEL     V3CTRN        D15TRN
437900960131     C                   MOVEL     DSLV15        KPJBU
438000951109     C                   CALL      'FNLV15R'
438100951109     C                   PARM                    KPJBA
438200951109     C                   CLEAR                   V3CVDP
438300980119    5C                   ENDIF
438400980119    4C                   ENDIF
438500951103     C*
438600951103     C* VISUALIZZA RITARDI ARRIVO
438700980114     C* Gestione subfile degli arrivi
438800980119     C                   Z-ADD     NR13          WNR13
438900980114     C                   Z-ADD     1             NR13
439000980119    4C     NR13          DOWLE     WNR13
439100010824     C     NR13          CHAIN     TL44S13                            31
439200980119    5C  N31V3CVD1        IFEQ      'V'
439300980114     C*
439400980114    3C***        V3CVD1    IFEQ 'V'
439500980115     C***        V3CVD2    OREQ 'V'
439600980115     C***        V3CVD3    OREQ 'V'
439700980115     C***        V3CVD4    OREQ 'V'
439800980114     C                   MOVE      '1'           W001A             1
439900980114     C                   CLEAR                   DSLV15
440000980114     C                   MOVEL     'A'           D15EPA
440100980114     C                   MOVEL     'I'           D15TUP
440200980114     C                   MOVEL     'R'           D15TRC
440300980115    4C***                  SELEC
440400980115    4C***        V3CVD1    WHEQ 'V'
440500980114     C                   MOVEL     V3CRT1        D15RTC
440600980114     C                   MOVEL     V3CFF1        D15LAI
440700980114     C                   CLEAR                   V3CVD1
440800980115    4C***        V3CVD2    WHEQ 'V'
440900980115     C***                  MOVELV3CRT2    D15RTC
441000980115     C***                  MOVELV3CFF2    D15LAI
441100980115     C***                  CLEARV3CVD2
441200980115    4C***        V3CVD3    WHEQ 'V'
441300980115     C***                  MOVELV3CRT3    D15RTC
441400980115     C***                  MOVELV3CFF3    D15LAI
441500980115     C***                  CLEARV3CVD3
441600980115   X4C***                  OTHER
441700980115     C***                  MOVELV3CRT4    D15RTC
441800980115     C***                  MOVELV3CFF4    D15LAI
441900980115     C***                  CLEARV3CVD4
442000980115    4C***                  ENDSL
442100980115     C***
442200960222     C                   MOVEL     V3CTTR        D15TTR
442300960222     C                   MOVEL     V3CLNP        D15LNP
442400960131     C                   MOVEL     V3CNFV        D15NFV
442500960131     C                   MOVEL     V3HDFV        D15DFV
442600960131     C                   MOVEL     V3CTRN        D15TRN
442700960131     C                   MOVEL     DSLV15        KPJBU
442800951109     C                   CALL      'FNLV15R'
442900951109     C                   PARM                    KPJBA
443000980120     C                   SETOFF                                       16
443100980121     C     NR13          COMP      1                                  8181
443200010824     C                   UPDATE    TL44S13
443300980119    5C                   END
443400980114     C                   ADD       1             NR13
443500980119    4C                   ENDDO
443600951106    3C                   ENDIF
443700980115     C**27 ON --> E' STATA RICHIESTA LA MANUTENZIONE
443800980115     C**         *IN27     IFEQ *ON
443900980115     C* 38 ON --> E' STATA RICHIESTA LA MANUTENZIONE
444000980119    3C     *IN38         IFEQ      *ON
444100960624     C* SE HO VISUALIZZATO DESCRIZIONI RITARDO RIEMETTO IL SUBFILE
444200980119    4C     W001A         IFEQ      '1'
444300960624     C                   GOTO      FORS03
444400980119    4C                   END
444500960625     C* CONTROLLO CAUSALI RITARDO SE MODIFICATE
444600960625     C   14              EXSR      CTRZ03
444700960625     C   14
444800960625     CAN 90              GOTO      FORS03
444900980119    3C                   END
445000960624     C* F6=CONFERMA
445100980119    3C     *INKF         IFEQ      *ON
445200960625     C* AGGIORNAMENTO GRIGLIA
445300960625     C                   EXSR      AGGGRL
445400980119    3C                   END
445500951103     C*
445600951103     C                   GOTO      FORS03
445700951106    2C                   ENDIF
445800951106    1C                   ENDIF
445900951103     C**
446000951103     C                   ENDSR
446100951214     C**
446200960222     C************* CARICO I DATI PARTENZA TRATTA *********************
446300960222     C     CARPAR        BEGSR
446400960305     C                   SETOFF                                       88
446500951215     C* DATA FVT
446600951215     C                   MOVE      TSRDTT        G02INV                         DATA FVT
446700951214     C                   MOVE      *ZEROS        G02DAT
446800951214     C                   MOVEL     '3'           G02ERR
446900951214     C                   CALL      'XSRDA8'
447000951214     C                   PARM                    WLBDAT
447100951214     C                   MOVEL     G02DAT        V3CDTT
447200951214     C                   Z-ADD     G02DAT        W0020             2 0
447300951214     C                   MOVE      W0020         V3CDTT
447400951214     C* FERMATA PARTENZA
447500951214     C                   Z-ADD     TSRFFP        KFIL
447600951214     C                   EXSR      CAZORG
447700951214     C                   MOVEL     TSRFFP        V3CFFP
447800951214     C                   MOVEL     ORGDES        V3DFFP
447900960305     C* TRATTA ANNULLATA
448000960305     C     TSRATB        IFNE      ' '
448100960305     C                   MOVEL     CANNUL        V3DDFV
448200960305     C                   SETON                                        88
448300960305     C                   ELSE
448400960305     C                   MOVEL     CABBIN        V3DDFV
448500960305    1C                   ENDIF
448600951214     C* NUMERO FOGLIO ABBINATO ESISTENTE
448700951214    1C     TSRNFV        IFGT      0
448800951214     C                   Z-ADD     TSRLNP        V3CLNP
448900951214     C                   MOVEL     TSRNFV        V3CNFV
449000951214     C*
449100951214     C                   MOVE      TSRDFV        G02INV
449200951214     C                   MOVE      *ZEROS        G02DAT
449300951214     C                   MOVEL     '3'           G02ERR
449400951214     C                   CALL      'XSRDA8'
449500951214     C                   PARM                    WLBDAT
449600951214     C                   MOVEL     G02DAT        V3CDFV
449700951214     C                   MOVE      G02DAT        V3HDFV
449800951214     C                   Z-ADD     G02DAT        W0020             2 0
449900951214     C                   MOVE      W0020         V3CDFV
450000960222    1C                   ENDIF
450100951214     C* PARTENZA TEORICA
450200951214     C                   MOVE      TSRDTP        G02INV                         DATA TEORICA
450300951214     C                   MOVE      *ZEROS        G02DAT
450400951214     C                   MOVEL     '3'           G02ERR
450500951214     C                   CALL      'XSRDA8'
450600951214     C                   PARM                    WLBDAT
450700951214     C                   MOVEL     G02DAT        V3CDTP
450800951214     C                   Z-ADD     G02DAT        W0020             2 0
450900951214     C                   MOVE      W0020         V3CDTP
451000951214     C                   MOVEL     TSRHTP        V3CHTP                         ORA  TEORICA
451100951214     C* PARTENZA REALE
451200951214    1C     TSRDRP        IFGT      0                                            DATA REALE
451300951214     C                   MOVE      TSRDRP        G02INV
451400951214     C                   MOVE      *ZEROS        G02DAT
451500951214     C                   MOVEL     '3'           G02ERR
451600951214     C                   CALL      'XSRDA8'
451700951214     C                   PARM                    WLBDAT
451800951214     C                   MOVEL     G02DAT        V3CDRP
451900951214     C                   Z-ADD     G02DAT        W0020             2 0
452000951214     C                   MOVE      W0020         V3CDRP
452100951214     C                   MOVEL     TSRHRP        V3CHRP                         ORA REALE
452200951214     C*
452300951214    2C     TSRRTP        IFNE      ' '                                          RITARDO
452400951214     C                   MOVEL     TSRRTP        V3CRTP
452500960625     C                   MOVEL     TSRRTP        V3HRTP
452600951214     C                   MOVEL     'TC'          COD
452700951214     C                   MOVEL(P)  TSRRTP        KEY
452800961029     C     KTAB          CHAIN     TABEL                              87
452900961029     C  N87              MOVEL     TBLUNI        V3DRTP
453000961029     C   87              MOVEL     *ALL'?'       V3DRTP
453100951214     C*
453200951214     C* CONTROLLO SE ESISTE ALMENO UNA RIGA DI DESCRIZIONE
453300951214     C                   CLEAR                   KFIL
453400960213     C                   MOVEL     'R'           KWTRC
453500951214     C                   MOVEL     'P'           KEPA
453600961029     C     KFV4          SETLL     FNFV401L                               87
453700961029     C   87              SETOFF                                       15
453800951214    2C                   ENDIF
453900951214    1C                   ENDIF
454000960222     C                   ENDSR
454100960222     C**
454200960222     C************* CARICO I DATI DI ARRIVO TRATTA ********************
454300960222     C     CARARR        BEGSR
454400951214     C* FERMATE ARRIVO
454500951214     C                   Z-ADD     TSRFFA        KFIL
454600951214     C                   EXSR      CAZORG
454700951214     C* ARRIVO TEORICO
454800951214     C                   MOVE      TSRDTA        G02INV                         DATA TEORICA
454900951214     C                   MOVE      *ZEROS        G02DAT
455000951214     C                   MOVEL     '3'           G02ERR
455100951214     C                   CALL      'XSRDA8'
455200951214     C                   PARM                    WLBDAT
455300951214     C                   Z-ADD     G02DAT        W0020             2 0
455400951214     C**
455500980114    1C*****                SELEC
455600980114     C                   ADD       1             NR13
455700980114     C     NR13          IFEQ      1
455800980120     C                   SETOFF                                       81
455900980114     C                   ELSE
456000980120     C                   SETON                                        81
456100980114     C                   ENDIF
456200980114     C*****      WCONT     WHEQ 1
456300951214     C                   MOVEL     TSRFFA        V3CFF1
456400951214     C                   MOVEL     ORGDES        V3DFF1
456500951214     C                   MOVEL     G02DAT        V3CDT1
456600951214     C                   MOVE      W0020         V3CDT1
456700951214     C                   MOVEL     TSRHTA        V3CHT1                         ORA  TEORICA
456800960625     C                   MOVE      TSRNFA        V3HNF1
456900980114     C****       WCONT     WHEQ 2
457000980114     C****                 MOVELTSRFFA    V3CFF2
457100980114     C****                 MOVELORGDES    V3DFF2
457200980114     C****                 MOVELG02DAT    V3CDT2
457300980114     C****                 MOVE W0020     V3CDT2
457400980114     C****                 MOVELTSRHTA    V3CHT2           ORA  TEORICA
457500980114     C****                 MOVE TSRNFA    V3HNF2
457600980114     C****       WCONT     WHEQ 3
457700980114     C****                 MOVELTSRFFA    V3CFF3
457800980114     C****                 MOVELORGDES    V3DFF3
457900980114     C****                 MOVELG02DAT    V3CDT3
458000980114     C****                 MOVE W0020     V3CDT3
458100980114     C****                 MOVELTSRHTA    V3CHT3           ORA  TEORICA
458200980114     C****                 MOVE TSRNFA    V3HNF3
458300980114   X1C****                 OTHER
458400980114     C****                 MOVELTSRFFA    V3CFF4
458500980114     C****                 MOVELORGDES    V3DFF4
458600980114     C****                 MOVELG02DAT    V3CDT4
458700980114     C****                 MOVE W0020     V3CDT4
458800980114     C****                 MOVELTSRHTA    V3CHT4           ORA  TEORICA
458900980114     C****                 MOVE TSRNFA    V3HNF4
459000980114    1C****                 ENDSL
459100951214     C* ARRIVO REALE
459200951214     C                   SETOFF                                       31
459300951214     C                   CLEAR                   TBLUNI
459400951214    1C     TSRDRA        IFGT      0                                            DATA REALE
459500951214     C                   MOVE      TSRDRA        G02INV
459600951214     C                   MOVE      *ZEROS        G02DAT
459700951214     C                   MOVEL     '3'           G02ERR
459800951214     C                   CALL      'XSRDA8'
459900951214     C                   PARM                    WLBDAT
460000951214     C                   Z-ADD     G02DAT        W0020             2 0
460100960131     C                   CLEAR                   TBLUNI
460200951214    2C     TSRRTA        IFNE      ' '                                          RITARDO
460300951214     C                   MOVEL     'TC'          COD
460400951214     C                   MOVEL(P)  TSRRTA        KEY
460500951214     C     KTAB          CHAIN     TABEL                              31
460600960131     C   31              MOVEL     *ALL'?'       TBLUNI
460700951214    2C                   ENDIF
460800951214     C*
460900951214     C* CONTROLLO SE ESISTE ALMENO UNA RIGA DI DESCRIZIONE
461000951214     C                   Z-ADD     TSRFFA        KFIL
461100960213     C                   MOVEL     'R'           KWTRC
461200951214     C                   MOVEL     'A'           KEPA
461300951214     C     KFV4          SETLL     FNFV401L                               31
461400980119     C   31              SETOFF                                       16
461500951214     C**
461600980114    2C****                 SELEC
461700980114     C****       WCONT     WHEQ 1
461800951214     C                   MOVEL     G02DAT        V3CDR1
461900951214     C                   MOVE      W0020         V3CDR1
462000960201     C                   MOVEL     TSRHRA        V3CHR1                         ORA REALE
462100951214     C                   MOVEL     TSRRTA        V3CRT1
462200960625     C                   MOVEL     TSRRTA        V3HRT1
462300960131     C                   MOVEL     TBLUNI        V3DRT1
462400980119     C   31              SETOFF                                       16
462500980114     C***
462600980114     C***        WCONT     WHEQ 2
462700980114     C***                  MOVELG02DAT    V3CDR2
462800980114     C***                  MOVE W0020     V3CDR2
462900980114     C***                  MOVELTSRHRA    V3CHR2           ORA REALE
463000980114     C***                  MOVELTSRRTA    V3CRT2
463100980114     C***                  MOVELTSRRTA    V3HRT2
463200980114     C***                  MOVELTBLUNI    V3DRT2
463300980114     C***31                SETOF                     91
463400980114     C***
463500980114     C***        WCONT     WHEQ 3
463600980114     C***                  MOVELG02DAT    V3CDR3
463700980114     C***                  MOVE W0020     V3CDR3
463800980114     C***                  MOVELTSRHRA    V3CHR3           ORA REALE
463900980114     C***                  MOVELTSRRTA    V3CRT3
464000980114     C***                  MOVELTSRRTA    V3HRT3
464100980114     C***                  MOVELTBLUNI    V3DRT3
464200980114     C***31                SETOF                     92
464300980114   X2C***                  OTHER
464400980114     C***
464500980114     C***                  MOVELG02DAT    V3CDR4
464600980114     C***                  MOVE W0020     V3CDR4
464700980114     C***                  MOVELTSRHRA    V3CHR4           ORA REALE
464800980114     C***                  MOVELTSRRTA    V3CRT4
464900980114     C***                  MOVELTSRRTA    V3HRT4
465000980114     C***                  MOVELTBLUNI    V3DRT4
465100980114     C***31                SETOF                     93
465200980114    2C***                  ENDSL
465300960222    1C                   ENDIF
465400951214     C*
465500980119     C                   SETON                                        16
465600010824     C                   WRITE     TL44S13
465700010824     C                   CLEAR                   TL44S13
465800980114     C*
465900951214     C                   ENDSR
466000951103     C**
466100010824     C****************  CONTROLLI TL44Z03 *****************************
466200960625     C     CTRZ03        BEGSR
466300960625     C* GESTIONE ?
466400960625     C                   MOVEL     CODUT         §KUT
466500960625     C                   MOVEL     'TC'          §COD
466600960625     C*
466700960625     C     V3CRTP        IFEQ      '?'
466800960625     C                   MOVEL     *BLANKS       §KEY
466900960625     C                   CALL      'X§TABER'
467000960625     C                   PARM                    §KUT              1 0
467100960625     C                   PARM                    §COD              2
467200960625     C                   PARM                    §KEY              8
467300960625     C                   PARM                    §DES             30
467400960625     C                   MOVEL     §KEY          V3CRTP
467500960625     C                   MOVEL     §DES          V3DRTP
467600960625     C                   SETON                                        90
467700960625     C                   GOTO      ENDZ03
467800960625     C                   END
467900980115     C                   MOVEL     'TC'          COD
468000980115     C* CAUSALE RITARDO/ANTICIPO PARTENZA
468100980115    1C     *IN35         IFEQ      *ON
468200980115     C                   CLEAR                   V3DRTP
468300980115    2C     V3CRTP        IFNE      V3HRTP
468400980115     C     V3CRTP        ANDEQ     *BLANKS
468500980115     C                   MOVEL     MSG(39)       V3CMSG
468600980115     C                   SETON                                        289052
468700980115     C                   GOTO      ENDZ03
468800980115    2C                   END
468900980115    2C     V3CRTP        IFNE      *BLANKS
469000980115     C                   MOVEL(P)  V3CRTP        KEY
469100980115     C     KTAB          CHAIN     TABEL                              30
469200980115    3C     *IN30         IFEQ      *OFF
469300980115     C     TBLFLG        ANDEQ     *BLANKS
469400980115     C                   MOVEL     TBLUNI        V3DRTP
469500980115     C                   ELSE
469600980115     C                   MOVEL     MSG(39)       V3CMSG
469700980115     C                   SETON                                        289052
469800980115     C                   GOTO      ENDZ03
469900980115    3C                   END
470000980115    2C                   END
470100980115    1C                   END
470200980115     C**** CONTROLLO SUBFILE ARRIVI
470300980119     C                   Z-ADD     NR13          WNR13
470400980115     C                   Z-ADD     1             NR13
470500980115     C     NR13          DOWLE     WNR13
470600980115     C                   SETOFF                                       902853
470700010824     C     NR13          CHAIN     TL44S13                            31
470800980115     C     *IN31         IFEQ      *OFF
470900980115     C     V3CRT1        IFEQ      '?'
471000960625     C                   MOVEL     *BLANKS       §KEY
471100960625     C                   CALL      'X§TABER'
471200960625     C                   PARM                    §KUT              1 0
471300960625     C                   PARM                    §COD              2
471400960625     C                   PARM                    §KEY              8
471500960625     C                   PARM                    §DES             30
471600960625     C                   MOVEL     §KEY          V3CRT1
471700960625     C                   MOVEL     §DES          V3DRT1
471800960625     C                   SETON                                        90
471900960625     C                   END
472000980115     C* CAUSALI RITARDO/ANTICIPO ARRIVO
472100980115    1C     *IN36         IFEQ      *ON
472200980115     C                   CLEAR                   V3DRT1
472300980115    2C     V3CRT1        IFNE      V3HRT1
472400980115     C     V3CRT1        ANDEQ     *BLANKS
472500980115     C                   MOVEL     MSG(39)       V3CMSG
472600980115     C                   SETON                                        289053
472700980115    2C                   END
472800980115    2C     V3CRT1        IFNE      *BLANKS
472900980115     C                   MOVEL(P)  V3CRT1        KEY
473000980115     C     KTAB          CHAIN     TABEL                              30
473100980115    3C     *IN30         IFEQ      *OFF
473200980115     C     TBLFLG        ANDEQ     *BLANKS
473300980115     C                   MOVEL     TBLUNI        V3DRT1
473400980115     C                   ELSE
473500980115     C                   MOVEL     MSG(39)       V3CMSG
473600980115     C                   SETON                                        289053
473700980115    3C                   END
473800980115    2C                   END
473900980115    1C                   END
474000980121     C     NR13          COMP      1                                  8181
474100010824     C                   UPDATE    TL44S13
474200980115     C   90              GOTO      ENDZ03
474300980119     C                   END
474400980115     C                   ADD       1             NR13
474500980115     C                   ENDDO
474600980115     C*          V3CRT2    IFEQ '?'
474700980115     C*                    MOVEL*BLANKS   §KEY
474800980115     C*                    CALL 'X§TABER'
474900980115     C*                    PARM           §KUT    10
475000980115     C*                    PARM           §COD    2
475100980115     C*                    PARM           §KEY    8
475200980115     C*                    PARM           §DES   30
475300980115     C*                    MOVEL§KEY      V3CRT2
475400980115     C*                    MOVEL§DES      V3DRT2
475500980115     C*                    SETON                     90
475600980115     C*                    GOTO ENDZ03
475700980115     C*                    END
475800980115     C*          V3CRT3    IFEQ '?'
475900980115     C*                    MOVEL*BLANKS   §KEY
476000980115     C*                    CALL 'X§TABER'
476100980115     C*                    PARM           §KUT    10
476200980115     C*                    PARM           §COD    2
476300980115     C*                    PARM           §KEY    8
476400980115     C*                    PARM           §DES   30
476500980115     C*                    MOVEL§KEY      V3CRT3
476600980115     C*                    MOVEL§DES      V3DRT3
476700980115     C*                    SETON                     90
476800980115     C*                    GOTO ENDZ03
476900980115     C*                    END
477000980115     C*          V3CRT4    IFEQ '?'
477100980115     C*                    MOVEL*BLANKS   §KEY
477200980115     C*                    CALL 'X§TABER'
477300980115     C*                    PARM           §KUT    10
477400980115     C*                    PARM           §COD    2
477500980115     C*                    PARM           §KEY    8
477600980115     C*                    PARM           §DES   30
477700980115     C*                    MOVEL§KEY      V3CRT4
477800980115     C*                    MOVEL§DES      V3DRT4
477900980115     C*                    SETON                     90
478000980115     C*                    GOTO ENDZ03
478100980115     C*                    END
478200960625     C*
478300960625     C*
478400980115    1C***        *IN37     IFEQ *ON
478500980115     C***                  CLEARV3DRT2
478600980115    2C***        V3CRT2    IFNE V3HRT2
478700980115     C***        V3CRT2    ANDEQ*BLANKS
478800980115     C***                  MOVELMSG,39    V3CMSG
478900980115     C***                  SETON                     289054
479000980115     C***                  GOTO ENDZ03
479100980115    2C***                  END
479200980115    2C***        V3CRT2    IFNE *BLANKS
479300980115     C***                  MOVELV3CRT2    KEY       P
479400980115     C***        KTAB      CHAINTABEL                30
479500980115    3C***        *IN30     IFEQ *OFF
479600980115     C***        TBLFLG    ANDEQ*BLANKS
479700980115     C***                  MOVELTBLUNI    V3DRT2
479800980115     C***                  ELSE
479900980115     C***                  MOVELMSG,39    V3CMSG
480000980115     C***                  SETON                     289054
480100980115     C***                  GOTO ENDZ03
480200980115    3C***                  END
480300980115    2C***                  END
480400980115    1C***                  END
480500960625     C*
480600980115    1C***        *IN38     IFEQ *ON
480700980115     C***                  CLEARV3DRT3
480800980115    2C***        V3CRT3    IFNE V3HRT3
480900980115     C***        V3CRT3    ANDEQ*BLANKS
481000980115     C***                  MOVELMSG,39    V3CMSG
481100980115     C***                  SETON                     289055
481200980115     C***                  GOTO ENDZ03
481300980115    2C***                  END
481400980115    2C***        V3CRT3    IFNE *BLANKS
481500980115     C***                  MOVELV3CRT3    KEY       P
481600980115     C***        KTAB      CHAINTABEL                30
481700980115    3C***        *IN30     IFEQ *OFF
481800980115     C***        TBLFLG    ANDEQ*BLANKS
481900980115     C***                  MOVELTBLUNI    V3DRT3
482000980115     C***                  ELSE
482100980115     C***                  MOVELMSG,39    V3CMSG
482200980115     C***                  SETON                     289055
482300980115     C***                  GOTO ENDZ03
482400980115    3C***                  END
482500980115    2C***                  END
482600980115    1C***                  END
482700980115     C***
482800980115    1C***        *IN39     IFEQ *ON
482900980115     C***                  CLEARV3DRT4
483000980115    2C***        V3CRT4    IFNE V3HRT4
483100980115     C***        V3CRT4    ANDEQ*BLANKS
483200980115     C***                  MOVELMSG,39    V3CMSG
483300980115     C***                  SETON                     289056
483400980115     C***                  GOTO ENDZ03
483500980115    2C***                  END
483600980115    2C***        V3CRT4    IFNE *BLANKS
483700980115     C***                  MOVELV3CRT4    KEY       P
483800980115     C***        KTAB      CHAINTABEL                30
483900980115    3C***        *IN30     IFEQ *OFF
484000980115     C***        TBLFLG    ANDEQ*BLANKS
484100980115     C***                  MOVELTBLUNI    V3DRT4
484200980115     C***                  ELSE
484300980115     C***                  MOVELMSG,39    V3CMSG
484400980115     C***                  SETON                     289056
484500980115     C***                  GOTO ENDZ03
484600980115    3C***                  END
484700980115    2C***                  END
484800980115    1C***                  END
484900980115     C***
485000960625     C     ENDZ03        ENDSR
485100960625     C****************  AGGIORNAMENTO GRIGLIA  ************************
485200960625     C     AGGGRL        BEGSR
485300960625     C* RIPROTEGGO CAMPI I/O
485400960625     C                   SETOFF                                       273536
485500960625     C                   SETOFF                                       373839
485600960625     C* AGGIORNAMENTO METRI CUBI ASSEGNATI
485700960625     C                   Z-ADD     NRR3          WNRR3
485800960625     C                   Z-ADD     1             NRR3
485900960625    1C     NRR3          DOWLE     WNRR3
486000010824     C     NRR3          CHAIN     TL44S03                            31
486100960625    2C     *IN31         IFEQ      *OFF
486200960625     C     V3CMTP        ANDGT     0
486300960625    3C     V3CMCA        IFNE      V3HMCA
486400960710    3C     V3CMCT        ORNE      V3HMCT
486500960710    3C     V3CMCB        ORNE      V3HMCB
486600960625     C     KTSM01        CHAIN     TNTSM01L                           31
486700960625    4C     *IN31         IFEQ      *OFF
486800960625     C                   Z-ADD     V3CMCA        TSMMCA
486900960710     C                   Z-ADD     V3CMCT        TSMMCT
487000960710     C                   Z-ADD     V3CMCB        TSMMCB
487100960625     C                   UPDATE    TNTSM000
487200960625    4C                   END
487300960625    3C                   END
487400010824     C                   UPDATE    TL44S03
487500960625    2C                   END
487600960625     C                   ADD       1             NRR3
487700960625    1C                   ENDDO
487800960625     C* AGGIORNAMENTO CAUSALI RITARDO/ANTICIPO SE VARIATI
487900980115    1C***14      V3CRTP    IFNE V3HRTP
488000980115     C***        V3CRT1    ORNE V3HRT1
488100980115     C***        V3CRT2    ORNE V3HRT2
488200980115     C***        V3CRT3    ORNE V3HRT3
488300980115     C***        V3CRT4    ORNE V3HRT4
488400980115     C* FACCIO L'AGGIORNAMENTO SOLO PER 14 ACCESO E SOSTITUISCO LA LETTURA
488500980115     C* DELLE SINGOLE RIGHE DI ARRIVO CON LE RIGHE DEL  SUBFILE
488600980115    1C     *IN14         IFEQ      *ON
488700980115     C*
488800980115     C     KTSR13        SETLL     TNTSR01L
488900960625     C     KTSR13        READE     TNTSR01L                               31
489000960625    2C     *IN31         DOWEQ     *OFF
489100960625     C                   CLEAR                   W001A
489200960625    3C     TSRNFP        IFEQ      KVNFP
489300960625     C     V3CRTP        ANDNE     V3HRTP
489400960625     C                   MOVEL     V3CRTP        TSRRTP
489500960625     C                   MOVE      '1'           W001A             1
489600970410    4C     V3CRTP        IFNE      ' '
489700970410     C     TSRDKP        ANDEQ     99999999
489800980209     C*****                MOVE *ZEROS    TSRDKP
489900980209     C* INVECE CHE AZZERARE LA DATA  LA METTO = A DATA ULTIMO CONTROLL
490000980424     C* NON AZZERO LA DATA E NON LA IMPOSTO COME LA DATA ULTIMO
490100980424     C* CONTROLLO MA CI METTO LA DATA REALE PARTENZA PER PERMETTERE
490200980424     C* DI STAMPARE QUESTO RITARDO NEL MESE DI COMPETENZA PER POTER
490300980424     C* QUADRARE IL PIU' POSSIBILE LA STATISTICA RITARDI CON LA STAMPA
490400980424     C* RITARDI
490500980424     C                   MOVE      TSRDRP        TSRDKP
490600980115    4C                   END
490700960625    3C                   END
490800980115    3C***                  SELEC
490900980115     C***        TSRNFA    WHEQ V3HNF1
491000980115     C* LETTURA DEL SUBFILE PER OGNI RIGA DEL FILE TNTSR
491100980115     C                   Z-ADD     NR13          WNR13
491200980115     C                   Z-ADD     1             NR13
491300980115     C     NR13          DOWLE     WNR13
491400010824     C     NR13          CHAIN     TL44S13                            57
491500980115    2C     *IN57         IFEQ      *OFF
491600980115    3C     TSRNFA        IFEQ      V3HNF1
491700980115     C     V3CRT1        ANDNE     V3HRT1
491800980115     C                   MOVEL     V3CRT1        TSRRTA
491900980115     C                   MOVE      '1'           W001A             1
492000980115     C* SE DATA E ORA REALE SONO =0 IMPOSTO UGUALE ALLA TEORICA
492100980115    4C     TSRDRA        IFEQ      0
492200980115     C                   MOVEL     TSRDTA        TSRDRA
492300980115     C                   MOVEL     TSRHTA        TSRHRA
492400980115    4C                   ENDIF
492500980115    4C     V3CRT1        IFNE      ' '
492600980115     C     TSRDKA        ANDEQ     99999999
492700980209     C*****                MOVE *ZEROS    TSRDKA
492800980424     C* INVECE CHE AZZERARE LA DATA  LA METTO = A DATA ULTIMO CONTROLL
492900980424     C* NON AZZERO LA DATA E NON LA IMPOSTO COME LA DATA ULTIMO
493000980424     C* CONTROLLO MA CI METTO LA DATA REALE ARRIVO PER PERMETTERE
493100980424     C* DI STAMPARE QUESTO RITARDO NEL MESE DI COMPETENZA PER POTER
493200980424     C* QUADRARE IL PIU' POSSIBILE LA STATISTICA RITARDI CON LA STAMPA
493300980424     C* RITARDI
493400980424     C                   MOVE      TSRDRA        TSRDKA
493500980115    4C                   END
493600980115    3C                   END
493700980120     C                   SETON                                        1516
493800980121     C     NR13          COMP      1                                  8181
493900010824     C                   UPDATE    TL44S13
494000980115    2C                   END
494100980115     C                   ADD       1             NR13
494200980115     C                   ENDDO
494300980115     C*
494400980119    3C     W001A         IFEQ      '1'
494500980119     C                   UPDATE    TNTSR000
494600980119     C                   ELSE
494700980119     C                   UNLOCK    TNTSR01L
494800980119    3C                   END
494900980115     C     KTSR13        READE     TNTSR01L                               31
495000980115    2C                   ENDDO
495100980115    1C                   END
495200980115     C***        TSRNFA    WHEQ V3HNF2
495300980115     C***        V3CRT2    ANDNEV3HRT2
495400980115     C***                  MOVELV3CRT2    TSRRTA
495500980115     C***                  MOVE '1'       W001A   1
495600980115     C***        TSRDRA    IFEQ 0
495700980115     C***                  MOVELTSRDTA    TSRDRA
495800980115     C***                  MOVELTSRHTA    TSRHRA
495900980115     C***                  ENDIF
496000980115    4C***        V3CRT2    IFNE ' '
496100980115     C***        TSRDKA    ANDEQ99999999
496200980115     C***                  MOVE *ZEROS    TSRDKA
496300980115    4C***                  END
496400980115     C***        TSRNFA    WHEQ V3HNF3
496500980115     C***        V3CRT3    ANDNEV3HRT3
496600980115     C***                  MOVELV3CRT3    TSRRTA
496700980115     C***                  MOVE '1'       W001A   1
496800980115     C***        TSRDRA    IFEQ 0
496900980115     C***                  MOVELTSRDTA    TSRDRA
497000980115     C***                  MOVELTSRHTA    TSRHRA
497100980115     C***                  ENDIF
497200980115    4C***        V3CRT3    IFNE ' '
497300980115     C***        TSRDKA    ANDEQ99999999
497400980115     C***                  MOVE *ZEROS    TSRDKA
497500980115    4C***                  END
497600980115     C***        TSRNFA    WHEQ V3HNF4
497700980115     C***        V3CRT4    ANDNEV3HRT4
497800980115     C***                  MOVELV3CRT4    TSRRTA
497900980115     C***                  MOVE '1'       W001A   1
498000980115     C***        TSRDRA    IFEQ 0
498100980115     C***                  MOVELTSRDTA    TSRDRA
498200980115     C***                  MOVELTSRHTA    TSRHRA
498300980115     C***                  ENDIF
498400980115    4C***        V3CRT4    IFNE ' '
498500980115     C***        TSRDKA    ANDEQ99999999
498600980115     C***                  MOVE *ZEROS    TSRDKA
498700980115    4C***                  END
498800980115    3C***                  ENDSL
498900960625     C**
499000960625     C                   ENDSR
499100951103     C****************  CHAIN SU AZORG01L *****************************
499200951103     C     CAZORG        BEGSR
499300960305     C     KFIL          CHAIN     AZORG                              31
499400960305     C   31              MOVEL     *ALL'?'       ORGDES
499500951103     C                   ENDSR
499600951107     C**
499700951107     C*************** CANCELLO RECROD IN TNTSW ************************
499800951107     C     DELTSW        BEGSR
499900951107     C     KTSW12        SETLL     TNTSW01L
500000951107     C     KTSW12        READE     TNTSW01L                               31
500100951107     C     *IN31         DOWEQ     *OFF
500200960322     c* se impostato il campo del sfl cancello solo quelli del sfl
500300960322     C     KSFL          IFEQ      0
500400960322     C     KSFL          OREQ      TSWSFL
500500960322     C*
500600951107     C                   DELETE    TNTSW000
500700960322     C                   ENDIF
500800951107     C     KTSW12        READE     TNTSW01L                               31
500900951107     C                   ENDDO
501000951107     C                   ENDSR
501100951117     C**
501200951117     C********** ABBINAMENTO FV VISULIZZO DATI NEL SFL*****************
501300951117     C     ABBIN         BEGSR
501400951117     C                   Z-ADD     NRR2          KNRR
501500951117     C     KTSW11        CHAIN(N)  TNTSW01L                           31
501600951121     C* E' LA FILIALE CHE ESPONE L'IMPORTO DEL FV
501700960221    0C     *IN31         IFEQ      *OFF
501800951124     C*
501900960305     C                   MOVEL     V2HNFP        SAVNFP
502000960305     C*
502100951121     C     V2HNRC        SUB       2             W0050
502200951121     C     NRR2          SUB       W0050         KNRR
502300951121     C*
502400951121     C* CHAIN 2 RECORD PER IMPOSTARE L'IMPORTO DEL FV
502500960221    1C     TSWFES        IFEQ      FVSLNP
502600960305     C     TSWNFE        ANDEQ     SAVNFP
502700960221     C*
502800010824     C     KNRR          CHAIN     TL44S02                            31
502900010824
503000010907      * se ho la divisa del foglio viaggio a blank devo prendere quella
503100010907      * della griglia
503200010907     c                   if        fwsvip = *blanks
503300010907     c                   eval      fwsvip = tstdiv
503400010907     c                   endif
503500010907     c                   if        fwsves = *blanks
503600010907     c                   eval      fwsves = tstdiv
503700010907     c                   endif
503800010827      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
503900010827      * devo convertire fwsipt e fwsies nella divisa della griglia
504000010827     c                   eval      w_importo = fwsipt
504100011023     c                   if        fwsvip <> tswdiv and fwsipt <> 1
504200011023     c                             and fwsipt <> 0
504300010827     c                   eval      w_divisa = fwsvip
504400010827     c                   exsr      Sr_Converti
504500010827     c                   endif
504600010827     c                   z-add     w_importo     v2cimp
504700010827     C**!!!              Z-ADD     FVSIPT        V2CIMP
504800951121     C                   EXSR      CTRIND
504900010824     C                   UPDATE    TL44S02
505000960221    1C                   ENDIF
505100951124     C*
505200951121     C* CHAIN 1 RECORD PER IMPOSTARE GLI ALTRI DATI
505300951121     C                   SUB       1             KNRR
505400951127     C     KTSW11        CHAIN     TNTSW01L                           31
505500010824     C     KNRR          CHAIN     TL44S02                            31
505600951121     C*
505700960221    1C     TSWFES        IFEQ      FVSLNP
505800960305     C     TSWNFE        ANDEQ     SAVNFP
505900960221     C*
506000960221     C* SE TRAZIONISTA MANCA --> GLI LASCIO QUELLO CHE GIA' C'E'
506100960221    2C     FVSPDR        IFEQ      0
506200951124     C                   MOVEL     V2CPDR        FVSPDR
506300960221    2C                   ENDIF
506400960221     C*
506500960221     C                   MOVEL     FVSPDR        W007A             7
506600960221     C*
506700960221    2C     W007A         IFNE      V2CPDR
506800960221     C     FVSTMZ        ORNE      V2HTMZ
506900960221     C                   CLEAR                   V2HPDR
507000960221    2C                   ENDIF
507100951124     C*
507200951124     C                   MOVEL     FVSPDR        V2CPDR
507300960305     C     V2CPDR        IFLE      *ZEROS
507400960305     C                   CLEAR                   V2CPDR
507500960305     C                   ENDIF
507600960305     C                   MOVEL     FVSDPD        V2DPDR
507700951124     C                   MOVEL     FVSTMZ        V2HTMZ
507800010824
507900010827      * se la divisa del foglio viaggio non è uguale alla divisa della griglia
508000010827      * devo convertire fwsipt e fwsies nella divisa della griglia
508100010827     c                   eval      w_importo = fwsipt
508200011023     c                   if        fwsvip <> tswdiv and fwsipt <> 1
508300011023     c                             and fwsipt <> 0
508400010827     c                   eval      w_divisa = fwsvip
508500010827     c                   exsr      Sr_Converti
508600010827     c                   endif
508700010827     c                   eval      tswipt = w_importo
508800010827     c                   z-add     w_importo     v2hipt
508900010827
509000010827     c                   eval      w_importo = fwsies
509100011023     c                   if        fwsves <> tswdiv and fwsies <> 1
509200011023     c                             and fwsies <> 0
509300010827     c                   eval      w_divisa = fwsves
509400010827     c                   exsr      Sr_Converti
509500010827     c                   endif
509600010827     c                   eval      tswies = w_importo
509700010827     C**!!!              Z-ADD     FVSIPT        V2HIPT
509800960221     C*
509900960221     C                   MOVEL     V2CPDR        TSWVPD
510000960305     C                   MOVEL     V2DPDR        TSWDPD
510100960221     C                   MOVEL     V2HTMZ        TSWTMZ
510200010827     C**!!!              MOVEL     FVSIPT        TSWIPT
510300010827     C**!!!              MOVEL     FVSIES        TSWIES
510400960221     C                   MOVEL     'S'           TSWFVR
510500960221     C                   UPDATE    TNTSW000
510600960221    1C                   ENDIF
510700960221     C* ACCENDO INDICATORE DI SFLNEXCHG PER  VEDERE SE POSSO  CONFERMAR
510800960221     C*  L'IMPORTO
510900960221     C                   SETON                                        22
511000951124     C                   EXSR      CTRIND
511100010824     C                   UPDATE    TL44S02
511200960221     C                   SETOFF                                       22
511300960221    0C                   ENDIF
511400951121     C*
511500951117     C                   ENDSR
511600951106     C**
511700960221     C*********** SBINAMENTO FV ***************************************
511800960221     C     SBINAM        BEGSR
511900960221     C                   Z-ADD     NRR2          KNRR
512000960221     C     KTSW11        CHAIN(N)  TNTSW01L                           31
512100960221     C* E' LA FILIALE CHE ESPONE L'IMPORTO DEL FV
512200960221    0C     *IN31         IFEQ      *OFF
512300960221     C*
512400960221     C     V2HNRC        SUB       2             W0050
512500960221     C     NRR2          SUB       W0050         KNRR
512600960221     C*
512700960221     C* CHAIN 2 RECORD PER IMPOSTARE L'IMPORTO DEL FV
512800960223    1C     TSWFES        IFEQ      V2CFFP
512900960221     C     TSWNFE        ANDEQ     V2HNFP
513000960221     C*
513100010824     C     KNRR          CHAIN     TL44S02                            31
513200960223     C                   CLEAR                   V2CIMP
513300960221     C                   EXSR      CTRIND
513400010824     C                   UPDATE    TL44S02
513500960221    1C                   ENDIF
513600960221     C*
513700960221     C* CHAIN 1 RECORD PER IMPOSTARE GLI ALTRI DATI
513800960221     C                   SUB       1             KNRR
513900960221     C     KTSW11        CHAIN     TNTSW01L                           31
514000010824     C     KNRR          CHAIN     TL44S02                            31
514100960221     C*
514200960221    1C     TSWFES        IFEQ      FVSLNP
514300960221     C     TSWNFE        ANDEQ     V2HNFP
514400960221     C*
514500960221     C                   CLEAR                   V2CPDR
514600960221     C                   CLEAR                   V2HPDR
514700960221     C                   CLEAR                   V2HTMZ
514800960221     C                   CLEAR                   V2CICO
514900960221     C                   CLEAR                   V2HICO
515000960221     C                   CLEAR                   V2HIPT
515100960221     C                   CLEAR                   V2CCAU
515200960221     C*
515300960221     C                   CLEAR                   TSWVCU
515400960221     C                   CLEAR                   TSWVPD
515500960221     C                   CLEAR                   TSWTMZ
515600960221     C                   CLEAR                   TSWIPT
515700960223     C                   CLEAR                   TSWIES
515800960221     C                   MOVEL     'S'           TSWFVR
515900960221     C                   UPDATE    TNTSW000
516000960221    1C                   ENDIF
516100960221     C* ACCENDO INDICATORE DI SFLNEXCHG PER  VEDERE SE POSSO  CONFERMAR
516200960221     C*  L'IMPORTO
516300960221     C                   SETON                                        2219
516400960221     C                   EXSR      CTRIND
516500010824     C                   UPDATE    TL44S02
516600960221     C                   SETOFF                                       2219
516700960221    0C                   ENDIF
516800960221     C*
516900960221     C                   ENDSR
517000951215     C********** GESTIONE SFL PER DATA FVT SFL04 **********************
517100951106     C     GESTRT        BEGSR
517200951212     C                   SETOFF                                       10
517300951212     C                   Z-ADD     4             KSFL
517400951212     C                   CLEAR                   KLNP
517500951215     C                   CLEAR                   WPOSIZ
517600960322     C* QUI NON ABILITO F19 F20
517700960322     C                   SETOFF                                       10
517800960322     C                   Z-ADD     19            WRIPAG            5 0
517900951212     C*
518000951212     C* C A R I C A M E N T O   S F L C T L
518100951215     C* DATA FVT
518200951212     C                   MOVEL     V1CTTD        V4CTTD
518300951212     C                   MOVE      V1CTTD        W0020             2 0
518400951212     C                   MOVE      W0020         V4CTTD
518500951212     C*
518600951212     C                   MOVEL     V1CTTA        V4CTTA
518700951212     C                   MOVE      V1CTTA        W0020             2 0
518800951212     C                   MOVE      W0020         V4CTTA
518900951212     C*
519000951212     C                   CLEAR                   V4CRLP
519100951212     C                   CLEAR                   V4CRDT
519200951212     C*
519300951214     C* IMPOSTO LA RIGA DI TASTI FUNZIONALI: LA PRIMA
519400960322     C                   MOVEL     FN4N(1)       V4CFUN
519500951212     C* WF CONTIENE LA RIGA DI TASTI FUNZIONALI IN VISUALIZZAZIONE
519600951212     C                   Z-ADD     1             WF                2 0
519700951212     C* PULIZIA SFL
519800951214     C                   EXSR      PULSFL
519900951212     C*
520000951212     C* C A R I C A M E N T O  S F L
520100951214     C                   Z-ADD     AMGTTD        KDTT
520200951212     C                   Z-ADD     V1CLNP        KLNP
520300951212     C                   Z-ADD     V1CNFV        KNFV
520400951212     C* POSIZIONAMENTO TNTSR
520500951212     C                   EXSR      SETSR4
520600951212     C*
520700951212     C* LETTURA PER CARICAMENTO SFL
520800951212     C                   CLEAR                   WF20
520900951212     C                   Z-ADD     0             L                 5 0
521000951212     C                   EXSR      LEGS04
521100951212     C*
521200951212     C* SE NON HO CARICATO NULLA --> ERRORE
521300951212     C     NRR4          IFEQ      0
521400951212     C                   MOVEL     MSG(15)       V1CMSG
521500951212     C                   SETON                                        289043
521600951212     C                   GOTO      ENDGTT
521700951212     C                   ENDIF
521800951212     C                   Z-ADD     1             V4CREC
521900951212     C*
522000951212     C     FORS04        TAG
522100040422     c     wnrr4         cabeq     0             endgtt
522200010824     C                   WRITE     TL44T01
522300010824     C                   WRITE     TL44Z04
522400010824     C                   EXFMT     TL44C04
522500951212     C*
522600951212     C* SPENGO INDICATORI DI POSIZIONAMENTO CURSORE
522700951212     C                   SETOFF                                       2890
522800951212     C                   SETOFF                                       404142
522900951212     C                   SETOFF                                       5051
523000951212     C                   SETOFF                                       4344
523100951212     C*
523200951212     C* F3 - FINE
523300951212     C* F12 - RITORNO
523400951212     C   KL
523500951212     COR KC              GOTO      ENDGTT
523600951212     C* F24-ALTRI TASTI
523700951212     C     *INKY         IFEQ      *ON
523800951212     C*
523900951212     C     WF            IFLT      WRFUNZ
524000951212     C                   ADD       1             WF
524100960322     C                   MOVEL     FN4N(WF)      V4CFUN
524200951212     C                   ELSE
524300951212     C                   Z-ADD     1             WF
524400960322     C                   MOVEL     FN4N(1)       V4CFUN
524500951212     C                   ENDIF
524600951212     C                   GOTO      FORS04
524700951212     C                   ENDIF
524800951212     C*
524900951212     C* F8 -VISUALIZZA GRIGLIA DEL TRAINO
525000951212     C* F14-VISUALIZZA ANAGRAFICA TRAINO
525100960322     C* F17-MODIFICA   ANAGRAFICA TRAINO
525200960322     C* F10-INTERROGA IL FOGLIO VIAGGIO
525300951212    1C     *INKH         IFEQ      *ON
525400951212     C     *INKN         OREQ      *ON
525500951214     C     *INKR         OREQ      *ON
525600960322     C     *INKJ         OREQ      *ON
525700951214     C                   Z-ADD     V4CPOS        KVPOS
525800951215     C                   EXSR      CALPG4
525900951212     C                   GOTO      FORS04
526000951212    1C                   ENDIF
526100951212     C*
526200951212     C* 25 ON - ROLLUP
526300951212     C     *IN25         IFEQ      *ON
526400951212     C* FINE LETTURA --> EMETTO MESSAGGIO FINE SCORRIMENTO
526500951212     C     *IN99         IFEQ      *ON                                          FINEL LETTUR
526600951214     C                   MOVEL     MSG(19)       VSCMSG
526700951212     C                   SETON                                        28
526800951212     C                   ELSE
526900951212     C*
527000951212     C                   Z-ADD     1             WTOADD
527100951212     C                   Z-ADD     0             L
527200951212     C                   EXSR      LEGS04                                       LETTURA
527300951214     C                   EXSR      POSSFL                                       POS CURSORE
527400951212     C                   ENDIF
527500951212     C                   GOTO      FORS04
527600951212     C                   ENDIF
527700951212     C*
527800960325     C* ENTER CONTROLLI
527900960325     C                   EXSR      CTRS04
528000960322     C                   GOTO      FORS04
528100951212     C*
528200951212     C     ENDGTT        ENDSR
528300951103     C**
528400010827      *----------------------------------------------------------------
528500010827      * CONVERTI - Routine conversione importo
528600010827      *----------------------------------------------------------------
528700010827     c     Sr_Converti   begsr
528800010827     c                   clear                   dscv
528900010913     c                   movel     'CV'          cod
529000020104     c                   movel(p)  tswdiv        key
529100010827     c     ktab          chain     tabel00f
529200010827     c                   if        %found(tabel00f)
529300010827     c                   movel     tbluni        dscv
529400010827     c                   endif
529500010827
529600010827     c                   clear                   yeurcods
529700010827     c                   movel     w_divisa      yecdvi
529800010827     c                   movel     tswdiv        yecdvo
529900010827     c                   z-add     w_importo     yecimi
530000010827     c                   move      §cvdec        yecdco
530100010827     c                   call      'YEURCO'
530200010827     c                   parm                    yeurcods
530300010827    5c                   if        yecesi  = ' '
530400010827     c                   z-add     yecimo        w_importo
530500010827    5c                   endif
530600010827
530700010827     c                   endsr
530800010827
530900950921     C****************  ROUTINE INIZIALE  *****************************
531000950921     C     *INZSR        BEGSR
531100950921     C     *ENTRY        PLIST
531200950921     C                   PARM                    KPJBA
531300960301     C                   MOVEL     KPJBU         DSTL34
531400950921     C*
531500950921     C                   Z-ADD     1             CODUT
531600950921     C                   CALL      'X§PARUT'
531700950921     C                   PARM                    UT§DSE
531800950921     C                   MOVEL     RAGUT         V1CRSU
531900951109     C* CANCELLO EVENTUALI RECORD RIMASTI SE PGM HA DATO ERRORE
532000960322     C                   CLEAR                   KSFL
532100951109     C                   EXSR      DELTSW
532200950922     C*
532300000807     C     KTNTLZ        KLIST
532400000807     C                   KFLD                    KTLTIP
532500000807     C                   KFLD                    KTLPDR
532600000807     C                   EVAL      KTLTIP = 'T '
532700951108     C     KTLL1         KLIST
532800951113     C                   KFLD                    V2HTRN
532900951108     C                   KFLD                    KDDE
533000960212     C                   KFLD                    KTMZ
533100960212     C                   KFLD                    W0070
533200960223     C     KTLR          KLIST
533300960223     C                   KFLD                    KVTRN
533400960223     C                   KFLD                    KDDE
533500960223     C                   KFLD                    KVNFP
533600960223     C     KTLR1         KLIST
533700960223     C                   KFLD                    KVTRN
533800960223     C                   KFLD                    KDDE
533900960223     C     KTLM          KLIST
534000960223     C                   KFLD                    KVTRN
534100960223     C                   KFLD                    KDDE
534200960223     C                   KFLD                    TLRFIL
534300960226     C                   KFLD                    TSRFFA
534400951214     C     KTS1          KLIST
534500951102     C                   KFLD                    KTRN
534600951102     C                   KFLD                    KDTN
534700951214     C     KTST11        KLIST
534800951103     C                   KFLD                    KVTRN
534900951103     C                   KFLD                    KVDTN
535000951113     C     KTST2         KLIST
535100951113     C                   KFLD                    KPDR
535200951113     C                   KFLD                    KDTN
535300951102     C     KTST4         KLIST
535400951102     C                   KFLD                    KTFP
535500951102     C                   KFLD                    KDTN
535600951113     C     KTST41        KLIST
535700951113     C                   KFLD                    KTFP
535800951113     C                   KFLD                    KDTN
535900951113     C                   KFLD                    KTRN
536000951106     C     KTSR11        KLIST
536100951113     C                   KFLD                    KTRN
536200951113     C                   KFLD                    KDTN
536300951113     C                   KFLD                    KNFP
536400951107     C     KTSR12        KLIST
536500951107     C                   KFLD                    KVTRN
536600951107     C                   KFLD                    KVDTN
536700951107     C                   KFLD                    KVNFP
536800960223     C     KTSR13        KLIST
536900960223     C                   KFLD                    KVTRN
537000960223     C                   KFLD                    KVDTN
537100960223     C     KTSR14        KLIST
537200960223     C                   KFLD                    KVTRN
537300960223     C                   KFLD                    KVDTN
537400960223     C                   KFLD                    KVNFP
537500960305     C                   KFLD                    TSRNFA
537600960223     C                   KFLD                    KVFFP
537700960223     C                   KFLD                    TSRFFA
537800951212     C     KTSR2         KLIST
537900951212     C                   KFLD                    KLNP
538000951212     C                   KFLD                    KDTT
538100951212     C     KTSR21        KLIST
538200951212     C                   KFLD                    KLNP
538300951212     C                   KFLD                    KDTT
538400951212     C                   KFLD                    KDTN
538500951212     C                   KFLD                    KTRN
538600951108     C     KTSR3         KLIST
538700951108     C                   KFLD                    TSTTRN
538800951108     C                   KFLD                    TSTDTN
538900951108     C                   KFLD                    TSTFES
539000960305     C     KTSR31        KLIST
539100960305     C                   KFLD                    KVTRN
539200960305     C                   KFLD                    KVDTN
539300990915     C                   KFLD                    TSRFFP
539400990915     C                   KFLD                    TSRNFP
539500951212     C     KTSR4         KLIST
539600951212     C                   KFLD                    KLNP
539700951212     C                   KFLD                    KNFV
539800000504     C     Kfws          KLIST
539900000504     C                   KFLD                    fvslnp
540000000504     C                   KFLD                    fvsnfv
540100960304     C     KFV2          KLIST
540200960304     C                   KFLD                    TSRLNP
540300960304     C                   KFLD                    TSRNFV
540400951103     C     KFV4          KLIST
540500951103     C                   KFLD                    V3CLNP
540600951103     C                   KFLD                    V3CNFV
540700951106     C                   KFLD                    KEPA
540800951106     C                   KFLD                    KFIL
540900960213     C                   KFLD                    KWTRC
541000951030     C     KTAB          KLIST
541100951030     C                   KFLD                    CODUT
541200951103     C                   KFLD                    COD
541300951103     C                   KFLD                    KEY
541400951106     C     KTSW1         KLIST
541500951106     C                   KFLD                    KNMEB
541600951106     C                   KFLD                    KNMUS
541700951106     C                   KFLD                    KNRJO
541800951106     C                   KFLD                    KSFL
541900951106     C                   KFLD                    WF20
542000951106     C     KTSW11        KLIST
542100951106     C                   KFLD                    KNMEB
542200951106     C                   KFLD                    KNMUS
542300951106     C                   KFLD                    KNRJO
542400951106     C                   KFLD                    KSFL
542500951106     C                   KFLD                    WF20
542600951106     C                   KFLD                    KNRR
542700951107     C     KTSW12        KLIST
542800951107     C                   KFLD                    KNMEB
542900951107     C                   KFLD                    KNMUS
543000951107     C                   KFLD                    KNRJO
543100951106     C     KTSW2         KLIST
543200951106     C                   KFLD                    KNMEB
543300951106     C                   KFLD                    KNMUS
543400951106     C                   KFLD                    KNRJO
543500951106     C                   KFLD                    KLNP
543600951106     C                   KFLD                    KNFV
543700960213     C                   KFLD                    KWTRC
543800960213     C     KTSW3         KLIST
543900960213     C                   KFLD                    KNMEB
544000960213     C                   KFLD                    KNMUS
544100960213     C                   KFLD                    KNRJO
544200960213     C                   KFLD                    KWTRN
544300960213     C                   KFLD                    KWDTN
544400960213     C                   KFLD                    KWTRC
544500960223     C     KTSW4         KLIST
544600960223     C                   KFLD                    KNMEB
544700960223     C                   KFLD                    KNMUS
544800960223     C                   KFLD                    KNRJO
544900960223     C                   KFLD                    KWTRC
545000960223     C                   KFLD                    KWFVR
545100960625     C     KTSM01        KLIST
545200960625     C                   KFLD                    KVTRN
545300960625     C                   KFLD                    KVDTN
545400960625     C                   KFLD                    V3CMTP
545500960625     C                   KFLD                    V3CMTA
545600960625     C                   KFLD                    V3CMTF
545700000218     C* LETTURA ANATB01L - PARZIALE
545800000218     C     KEATAB        KLIST
545900000218     C                   KFLD                    KATCOD
546000000218     C                   KFLD                    KATAZI
546100000218     C                   KFLD                    KATKEY
546200950921     C*
546300951103     C     *LIKE         DEFINE    FV4EPA        KEPA
546400960213     C     *LIKE         DEFINE    FV4TRC        KWTRC
546500960223     C     *LIKE         DEFINE    TSWFVR        KWFVR
546600960223     C     *LIKE         DEFINE    TLRTFM        WTFM
546700951103     C*
546800951103     C     *LIKE         DEFINE    TBLCOD        COD
546900951103     C     *LIKE         DEFINE    TBLKEY        KEY
547000951214     C*
547100960222     C     *LIKE         DEFINE    V2CSCE        CSCE
547200951214     C     *LIKE         DEFINE    TSRTRN        CTRN
547300951214     C     *LIKE         DEFINE    TSRDTN        CDTN
547400951214     C     *LIKE         DEFINE    TSRNFP        HNFP
547500951214     C     *LIKE         DEFINE    TSRFFP        CFFP
547600951214     C     *LIKE         DEFINE    V2CDTT        CDTT
547700951214     C     *LIKE         DEFINE    V2CNFV        HNFV
547800951214     C     *LIKE         DEFINE    V2CNFV        CNFV
547900951214     C     *LIKE         DEFINE    V2HRFV        HRFV
548000971204     C***        *LIKE     DEFN V2CRTP    WRTP
548100971204     C***        *LIKE     DEFN V2CRDT    WRDT
548200951106     C*
548300951106     C     *LIKE         DEFINE    TSWSFL        KSFL
548400951107     C     *LIKE         DEFINE    TSWFFP        KLNP
548500951113     C     *LIKE         DEFINE    TSWPDR        KPDR
548600951113     C     *LIKE         DEFINE    TSWPDR        WPDR
548700951108     C*
548800960213     C     *LIKE         DEFINE    FVSTMZ        KTMZ
548900960305     C     *LIKE         DEFINE    TSWNFV        SAVNFV
549000960305     C     *LIKE         DEFINE    TSWNFE        SAVNFE
549100960305     C     *LIKE         DEFINE    TSWNFP        SAVNFP
549200960305     C     *LIKE         DEFINE    TSWFFP        SAVFFP
549300960305     C     *LIKE         DEFINE    FVSPDR        SAVPDR
549400951124     C     *LIKE         DEFINE    FVSTMZ        SAVTMZ
549500951124     C     *LIKE         DEFINE    FVSIPT        SAVIPT
549600951124     C     *LIKE         DEFINE    TSTICO        SAVICO
549700951124     C     *LIKE         DEFINE    TLLILI        SAVILI
549800951124     C     *LIKE         DEFINE    TLLILI        WILI
549900951108     C     *LIKE         DEFINE    TLLDDE        KDDE
550000951115     C     *LIKE         DEFINE    TLLCAU        WCAU
550100951106     C*
550200951106     C     *LIKE         DEFINE    TSWNFV        KNFV
550300951106     C     *LIKE         DEFINE    TSWNFV        WNNFV
550400951213     C     *LIKE         DEFINE    TSRNFV        WONFV
550500951213     C*
550600951214     C     *LIKE         DEFINE    TSRNFP        WLNFP
550700951214     C     *LIKE         DEFINE    TSRTRN        WLTRN
550800951214     C     *LIKE         DEFINE    TSRDTN        WLDTN
550900951103     C*
551000951103     C     *LIKE         DEFINE    NRR2          WNRR2
551100951106     C     *LIKE         DEFINE    NRR2          KNRR
551200960222     C     *LIKE         DEFINE    NRR2          SAVNRR
551300951106     C     *LIKE         DEFINE    NRR2          WANRR
551400951116     C     *LIKE         DEFINE    NRR2          WNRRA
551500951116     C     *LIKE         DEFINE    NRR2          WNRRB
551600951116     C     *LIKE         DEFINE    NRR2          WNRRC
551700951213     C     *LIKE         DEFINE    NRR2          NRR4
551800951213     C     *LIKE         DEFINE    NRR2          WNRR4
551900951214     C     *LIKE         DEFINE    NRR2          WNRR
552000960625     C     *LIKE         DEFINE    NRR3          WNRR3
552100980114     C     *LIKE         DEFINE    NR13          WNR13
552200951106     C     *LIKE         DEFINE    G02TGI        WTOTGG
552300951103     C*
552400951103     C     *LIKE         DEFINE    TSTICO        WSICO
552500951103     C*
552600951214     C     *LIKE         DEFINE    V2CPOS        KVPOS
552700951214     C     *LIKE         DEFINE    V2CREC        WNREC
552800951214     C*
552900951103     C     *LIKE         DEFINE    TSTTFP        KFIL
553000951103     C     *LIKE         DEFINE    TSTTFP        KTFP
553100951106     C     *LIKE         DEFINE    TSTTFP        WCTFP
553200951106     C     *LIKE         DEFINE    TSTTFP        WSTFP
553300951107     C*
553400951113     C     *LIKE         DEFINE    TSRNFP        KNFP
553500951107     C     *LIKE         DEFINE    TSRNFP        KVNFP
553600960222     C     *LIKE         DEFINE    TSRNFP        WANFP
553700960222     C     *LIKE         DEFINE    TSRFFP        KVFFP
553800960325     C     *LIKE         DEFINE    TSRFFP        WCFFP
553900951103     C*
554000951102     C     *LIKE         DEFINE    TSTTRN        WTRN
554100951102     C     *LIKE         DEFINE    TSTTRN        KTRN
554200951103     C     *LIKE         DEFINE    TSTTRN        KVTRN
554300951106     C     *LIKE         DEFINE    TSTTRN        WATRN
554400951116     C     *LIKE         DEFINE    TSTTRN        WCTRN
554500960213     C     *LIKE         DEFINE    TSTTRN        KWTRN
554600951103     C*
554700960213     C     *LIKE         DEFINE    TSTDTN        KWDTN
554800960213     C     *LIKE         DEFINE    TSTDTN        KVDTN
554900951106     C     *LIKE         DEFINE    TSTDTN        WSDTN
555000951114     C     *LIKE         DEFINE    TSTDTN        WADTN
555100951103     C     *LIKE         DEFINE    TSTDTN        KDTN
555200951115     C     *LIKE         DEFINE    TSTDTN        AMGRDT
555300951115     C     *LIKE         DEFINE    TSTDTN        AMGTRD
555400951102     C     *LIKE         DEFINE    TSTDTN        AMGTRA
555500951103     C     *LIKE         DEFINE    TSTDTN        W0080
555600951103     C     *LIKE         DEFINE    TSTDTN        WDTAL
555700951103     C     *LIKE         DEFINE    TSTDTN        WDTDAL
555800951213     C     *LIKE         DEFINE    TSRDTT        AMGTTD
555900951213     C     *LIKE         DEFINE    TSRDTT        AMGTTA
556000951213     C     *LIKE         DEFINE    TSRDTT        KDTT
556100960325     C     *LIKE         DEFINE    TSRDTT        WCDTT
556200000218     C     *LIKE         DEFINE    ATBCOD        KATCOD
556300000218     C     *LIKE         DEFINE    ATBAZI        KATAZI
556400000218     C     *LIKE         DEFINE    ATBKEY        KATKEY
556500000807     C     *LIKE         DEFINE    TLZPDR        KTLPDR
556600000807     C     *LIKE         DEFINE    TLZTIP        KTLTIP
556700951106     C* AREA DATI -- > DATA ULTIMO CONTROLLO
556800951106     C     *DTAARA       DEFINE                  TNTL30
556900951124     C* RIGHE DI TASTI FUNZIONALI
557000951124     C                   Z-ADD     2             WRFUNZ            1 0
557100000214     C*--------------------
557200000214     C*---------- RICERCA DITTA :
557300000214     C*--------------------
557400000218     C**                 MOVEL     'SOC001'      TIPXSC
557500000218     C***                MOVEL     *BLANK        SOCXSC
557600000218     C***                EXSR      REPSOC
557700000218     C**   RTNXSC        IFNE      '1'
557800000218     C***                MOVEL     XSOCDS        SOC001
557900000218     C***                MOVEL     xscrgs        RSUT             20
558000000218     c***                end
558100000214     C*
558200950921     C                   ENDSR
558300000214     C*----------------------------------------------------*
558400000214     C* Reperimento dati società
558500000214     C*----------------------------------------------------*
558600000218     C***  REPSOC        BEGSR
558700000214     C*
558800000218     C***                CALLB     'XSOC'
558900000218     C***                PARM                    TIPXSC            6
559000000218     C***                PARM                    SOCXSC            3
559100000218     C***                PARM                    CDSXSC            9 0
559200000218     C***                PARM                    MODXSC            3
559300000218     C***                PARM      *blanks       RTNXSC            1
559400000218     C***                PARM                    XSOCDS
559500000218     C**                 PARM                    KPJBA
559600000214     C*
559700000218     C***                ENDSR
559800000218** MSG***
559900951215Immettere solo una delle due scelte : per data TRAINO o per data FV
560000951102Data TRAINO  errata                                                            2
560100951102"Data TRAINO dal"  maggiore  della "Data traino al"                            3
560200951215Data FV Teorica errata                                                         4
560300951215"Data FV Teorica dal"  maggiore  della "Data FV teorica al"                    5
560400951102Linea di Partenza Foglio Viaggio inesistente o annullata                       6
560500951102La linea specificata non e' una linea partenza FV                              7
560600951102Numero traino errato                                                           8
560700950922Terminal di partenza inesistente o annullato                                   9
560800950922La filiale specificata non è un Terminal                                      10
560900951102Per data TRAINO immettere o la linea partenza  o il numero traino             11
561000951106Data Traino obbligatoria
561100951215Data FV Teor. o numero FV obbligatorio se immessa la linea partenza F.V.      13
561200951102Il traino non esiste per le selezioni di date specificate                     14
561300951102Non esiste nulla da visualizzare per le selezioni immesse                     15
561400951109Griglia non manutenzionabile :manca il lancio dei controlli del xx/xx/xxxx    16
561500951103Impossibile spostarsi a SINISTRA: gia'visualizzati i primi dati a disposizione17
561600951103Impossibile spostarsi a DESTRA:gia'visualizzati gli ultimi dati a disposizione18
561700951103Fine scorrimento
561800951103Non trovati trazionisti per chiave alfabetica richiesta                       20
561900951124Codice TRAZIONISTA obbligatorio o inesistente in anagrafica
562000951114Impossibile sgrigliare il traino: gia' contabilizzato
562100960222Presenti"BUCHI"o imp.da Confermare diverso da listino o FV: enter x forzare   23
562200960223Impossibile ripristinare il traino: non esiste piu' l'anagrafica !!           24
562300951103Impossibile abbinare un F.Viaggio su un traino che si sta Sgrigliando         25
562400951113Se immesso il codice trazionista immettere "1" nella selezione                26
562500951113Causale contabilizzazione obbligatoria se immesso importo da confermare       27
562600951113Causale contabilizzazione errata
562700960223Numero foglio viaggio errato: inesistente o annullato                         29
562800951124Foglio viaggio xxxxx gia' abbinato a traino n.ro xxxxxxx del xx/xx/xxxx       30
562900951115Data errata                                                                   31
563000951215Il F.viaggio non ha come filiale di scarico la fermata arrivo XXX             32
563100951124Tasto funzionale non ammesso:posizionarsi prima sul traino da visualizzare    33
563200951124Non e' stata trovata l'anagrafica del traino da visualizzare!!                34
563300951127Se immesso numero FV immettere anche la linea di partenza                     35
563400960222Il Foglio Viaggio non e' abbinato!!                                           36
563500960322Impossibile visulizzare il foglio viaggio: numero mancante!!                  37
563600960322Opzione errata per questa riga: posizionarsi sulla riga giusta e ripremere F1038
563700960625Immettere una causale ritardo/anticipo esistente                              39
563800011009Non ammessi importi con decimali per la divisa indicata                       40
563900020315Traino non visual. completamente: avanzare di 1 pag. per permettere l'annull. 41
564000951124**
564100960322F6=CONFERMA F8=VGriglia F14=InTraino F10=InFV F20=Destra
564200960322F3=Fine  F12=Ritorno                        F19=Sinistra
564300951124**
564400960322F6=CONFERMA  F8=VisGriglia  F14=IntTraino  F10=IntFV
564500960322F3=Fine  F12=Ritorno
564600951212**
564700960322M=ManGriglia  F12=Ritorno  F8=VisGriglia  F14=IntTraino  F10=IntFV  F3=Fine
564800960322
