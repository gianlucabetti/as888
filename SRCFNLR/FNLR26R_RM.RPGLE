000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200960219     H* FNLR26R *-----------------------------------------------------*
000300960219     H*    INTERROGAZIONE FOGLI DI VIAGGIO PARTENZA ALL'ARRIVO        *
000400960219     H*---------------------------------------------------------------*
000500941013     FFNLR26D   CF   E             WORKSTN
000600941013     F                                     SFILE(LR26S01:NRR1)
000700941013     F                                     SFILE(LR26S02:NRR2)
000800941028     FFNFVA03L  IF   E           K DISK
000900941028     F                                     RENAME(FNFVA000:FNFVA003)
001000941013     FFNFVA02L  IF   E           K DISK
001100960220     FFNFVA01L  UF   E           K DISK
001200941014     F                                     RENAME(FNFVA000:FNFVA01)
001300960220     FFNFVA05L  IF   E           K DISK
001400960220     F                                     RENAME(FNFVA000:FNFVA005)
001500000428     FFNFWA01L  IF   E           K DISK
001600951221     FFNFV401L  IF   E           K DISK
001700900515     FAZORG01L  IF   E           K DISK
001800951218     FTABEL00F  IF   E           K DISK
001900960402     FFNARB06L  IF   E           K DISK
001901130625     FFNFgv01l  IF   E           K DISK
002000960219     D*
002100960219     D* DEFINIZIONE SCHIERE
002200911010     D FVA             S              9  0 DIM(70)                              FVA SELEZ
002300980112     D*
002400980112     D FVC             S              8  0 DIM(900)                             FVA CARICATI
002500941014     D TES             S             34    DIM(2) CTDATA PERRCD(1)              TESTATA
002600980112     D L6              S              3  0 DIM(30)                              L6 DI PA1LAI
002700980112     D LEL             S              3  0 DIM(31)                              LINEE DA ELAB
002800960220     D MSG             S             78    DIM(11) CTDATA PERRCD(1)             MESSAGGI ERRORE
002900960219     D*
003000921125     D*LINEE   DI ARRIVO
003100921125     D                 DS
003200921125     D  FVAFFV                 1    240
003300921125     D  FVAFF2               241    447
003400000428     D  FWAFF3               448    687
003500000428     D  FWAFF4               688    897
003600000428     D  VUOTO                898    900
003700000428     D  FFV                    1    900
003800000428     D                                     DIM(300)                             LINEE FOGLIO
003900921125     D*FILIALE DI SCARICO
004000921125     D                 DS
004100921125     D  FVAFLP                 1    240
004200921125     D  FVAFL2               241    447
004300000428     D  FWAFL3               448    687
004400000428     D  FWAFL4               688    897
004500000428     D  VUOTO2               898    900
004600000428     D  FLP                    1    900
004700000428     D                                     DIM(300)                             LINEE TRANSITO
004800980112     D                 DS
004900980112     D  FVALNP                 1      3  0
005000980112     D  FVANFV                 4      8  0
005100980112     D  FVAFVC                 1      8  0
005200010704     D DSFV4V        E DS
005300960216     D DSTC          E DS
005400960216     D KPJBA         E DS
005500020424     D***                                     1   3 REM
005600020424     D***                                     4   6 REMFIL
005700960219     D CNCR80        E DS
005800941014     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005900000612     D OG143         E DS
006000960219     D*
006100960219     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
006200911204     D WLBDAT          DS
006300941013     D  G02DAT                 1      8  0
006400941013     D  G02INV                 9     16  0
006500941013     D  G02ERR                17     17
006600941013     D  G02TGI                18     22  0
006700960219     D***
006800960219     D* PARAMETRI PASSATI DAI PGM CHIAMANTI
006900960219     D***
007000941013     D PARAM1          DS
007100941014     D* PA1PGM: NOME PROGRAMMA CHIAMANTE
007200941014     D  PA1PGM                 1      7
007300941028     D* FIL DI INVIO DI CUI DEVO CARICARE I FOGLI
007400941014     D  PA1LAI                 8     10  0
007500941028     D* FILIALE ELABORATORE
007600941014     D  PA1FEL                11     13  0
007700941014     D* PA1ABB: 'N'=F.V.NON ABBIANTI, 'A'=SOLO ABBINATI, ' '=TUTTI
007800941014     D  PA1ABB                14     14
007900941014     D* PA1DSD: DATA INIZIALE (AAAA/MM/GG)
008000941014     D  PA1DSD                15     22  0
008100941014     D* PA1DSA: DATA FINALE (AAAA/MM/GG)
008200941014     D  PA1DSA                23     30  0
008300941014     D* PA1FLG: ' '=ABBINAMENTO F.V., 'I'=INTERROGAZIONE
008400941014     D  PA1FLG                31     31
008500941028     D* LINEA PARTENZA FOGLI DA VISUALIZZARE: 0 = TUTTI
008600941028     D  PA1LNP                32     34  0
008700971002     D* PGM SE RICHIAMATO E DA CHI
008800971002     D  PA1LAN                35     35
008900980204     D* CAMPO MESSAGGI DA PASSARE AL CHIAMANTE
009000980204     D  PA1MSG                36    113
009100921120     D*
009200960219     D***
009300960219     D* RICHIAMO PGM GESTIONE SPUNTE                   - FNLR04R -
009400960219     D***
009500941220     D PARA04          DS
009600941220     D  P04NFV                 1      5
009700941220     D  P04LNP                 6      8  0
009800941220     D  P04LNA                 9     11  0
009900941220     D  P04NRS                12     13  0
010000941220     D  P04NCD                14     20  0
010100941220     D  P04NCA                21     27  0
010200941220     D  P04NPS                28     29  0
010300941220     D  P04NPG                30     30  0
010400941220     D  P04FLG                31     31
010500941220     D  P04PGM                32     38
010600941220     D  P04FGS                39     41  0
010700101202     d* bolla
010800101202     d  p04bolp               42     44  0
010900101202     d  p04bors               45     46  0
011000101202     d  p04bosp               47     53  0
011100101202     d  p04boan               54     57  0
011200101202     D  P04ERR                58     58
011300101202     d
011400951221     D*
011500960221     D***
011600960221     D* RICHIAMO PGM TRASMISSIONE FNFVA/FNFV4          - FNLR20R -
011700960221     D***
011800960221     D PARAM2          DS
011900960221     D* PARNUM E' USATO PER RISOTTOMETTERE L'AZIONE DI TRASMISSIONE
012000960221     D  PA2NUM                 1      1  0 INZ
012100960221     D*
012200960205     D* DS PER FNLV15R - GESTIONE DESCRIZIONE CAUSALE RITARDO/ANTICIPO
012300960205     D DSLV15        E DS                  EXTNAME(FNLV15DS)
012400960219     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
012500960219     D DSLV50        E DS                  EXTNAME(FNLV50DS)
012600971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
012700980109     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012800980109     D  LIN                    1     90  0
012900980109     D                                     DIM(30)                              FIL. COMODO
013000960219     I*
013100960219     C*****************************************************************
013200960219     C* RIEPILOGO INDICATORI
013300960219     C*****************************************************************
013400960219     C* 01    - FOGLIO VIAGGIO IN R.I. NEL SFL
013500020424     C* 05    - SONO UN SECONDO LIVELLO
013600960219     C* 06    - CAMBIO FILIALE IN GESTIONE PER CMD18
013700960221     C* 11    - PROTEGGO CAMPO CAUSALE RITARDO ARRIVO
013800010703     C* 12    - NON VISUALIZZAZIONE TIPO AUTOMEZZO
013900960220     C* 13    - ESISTE CAUSALE RITARDO ARRIVO: ABILITO F6-AGGIORNAMENTO
014000010703     C* 14    - NON VISUALIZZAZIONE CATEG. TRAINO
014100971002     C* 15    - NON ABILITO IL RICHIAMO DELL'ABBINAMENTO PERCHE' VENGO
014200971002     C*         DA LI'
014300971002     C* 16    - ESISTE DESCRIZ. RITARDO/ANTICIPO PER LA PARTENZA
014400960219     C* 17    - ESISTE DESCRIZ. RITARDO/ANTICIPO PER L' ARRIVO
014500010703     C* 18    - NON VISUALIZZAZIONE PIOMBI
014600010703     C* 19    - NON VISUALIZZAZIONE CELLULARE
014700960221     C* 20    - ON --> PGM RICHIAMATO      OFF --> PGM DA MENU'
014800960219     C* 27    - INDICATORE DI EMISSIONE CAMPO MESSAGGI
014900960219     C* 28    - IMMESSA UNA SPECIFICA LINEA DI PARTENZA FOGLIO
015000960219     C* 29    - RICHIESTI SOLO F.V. NON ABBINATI
015100960220     C* 30/34 - DI COMODO
015200960219     C* 36    - INDICATORE DI SFLNXTCHG
015300960219     C* 37    - USATO PER RIEMETTERE IL SFL DI SELEZIONE FOGLIO
015400960220     C* 40/48 - ERRORI
015500960219     C* 72/74 - INDICATORI DI SFLCLR
015600960219     C* 73/75 - INDICATORI DI SFLDSP
015700960219     C*****************************************************************
015800000000     C     *ENTRY        PLIST
015900000000     C                   PARM                    KPJBA
016000960220     C                   PARM                    FVA
016100921119     C*
016200941013     C                   MOVEL     KPJBU         PARAM1
016300980204     C                   CLEAR                   PA1MSG
016400960219     C*
016500000000     C                   Z-ADD     1             CODUT
016600941014     C                   CALL      'X§PARUT'
016700941014     C                   PARM                    UT§DSE
016800960219     C                   MOVEL     REC80         CNCR80
016900941013     C                   MOVEL     RAGUT         RSUT
017000960219     C*---------------------------------------------------------------*
017100020424     C****  VEDO SE SONO PRIMO O SECONDO LIVELLO ***
017200020424    1C***        REM       IFEQ 'REM'
017300020424     C***        REMFIL    ANDGT*ZEROS
017400020424     C***                  MOVELREMFIL    V1CFGS
017500020424     C***                  SETON                     05
017600020424   X1C***                  ELSE
017700020424     C***                  MOVELSIMFEL    V1CFGS
017800020424    1C***                  ENDIF
017900020424     C                   Z-ADD     SIMPOU        V1CFGS
018000020424     C     SIMTPP        IFEQ      '2'
018100020424     C     SIMTPP        OREQ      *BLANKS
018200020424     C                   SETON                                        05
018300020424     C                   ENDIF
018400960220     C* TESTATA PROGRAMMA
018500951120    1C     PA1FLG        IFEQ      *BLANKS
018600951120     C                   MOVE      TES(1)        VIDTES
018700951120     C                   ELSE
018800951120     C                   MOVE      TES(2)        VIDTES
018900951120    1C                   ENDIF
019000960219     C*
019100960221     C* 11 ON  - PROTEGGO CAMPO CAUSALE RITARDO ARRIVO
019200960221     C                   SETON                                        11
019300960221     C*
019400960220     C* PA1PGM >< *BLANKS  ===> P R O G R A M M A   R I C H I A M A T O
019500960220    1C     PA1PGM        IFNE      *BLANKS
019600960221     C                   SETON                                        20
019700960220     C                   CLEAR                   V1CRTP
019800960220     C                   CLEAR                   V1CRTA
019900960220     C                   CLEAR                   WDAD
020000960220     C                   CLEAR                   WDAA
020100971002     C* VEDO SE ABILITARE F7 DI ABBINAMENTO FOGLI VIAGGIO
020200971002     C     PA1LAN        IFEQ      '5'
020300971002     C                   SETOFF                                       15
020400971002     C                   ELSE
020500971002     C                   SETON                                        15
020600971002     C                   ENDIF
020700960220     C*
020800960220   X1C                   ELSE
020900971002     C                   SETON                                        15
021000960220     C* PA1PGM =  *BLANKS  ===> P R O G R A M M A   D A   M E N U'
021100960220     C*
021200960220     C     FMTD01        TAG
021300960219     C* PULIZIA CAMPI VIDEATA
021400960219     C                   EXSR      PULD01
021500960219     C* EMISSIONE FORMATO
021600960220     C                   WRITE     LR26T01                                      TESTATA VIDEO
021700960220     C     FOR01         TAG
021800960219     C                   EXFMT     LR26D01
021900960220     c*
022000960220     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN27)
022100960220     C                   CLEAR                   V1CMSG
022200960220     C                   SETOFF                                       404142
022300960220     C                   SETOFF                                       434445
022400960220     C                   SETOFF                                       464727
022500980223     C                   MOVE      *ZEROS        PA1LAI
022600960219     C*
022700960219     C** CMD3 - FINE LAVORO
022800960219     C   KC              GOTO      FINE
022900960219     C** CMD18- CAMBIO FILIALE
023000960219     C     *INKS         IFEQ      *ON
023100960220     C                   SETON                                        0640
023200960219     C                   GOTO      FOR01
023300960219     C                   ENDIF
023400960219     C*
023500960219     C* 06 ON  - CONTROLLO FILIALE IN GESTIONE
023600960219    2C     *IN06         IFEQ      *ON
023700960219     C                   MOVEL     V1CFGS        WFIL
023800960219     C                   EXSR      CTRFGS
023900960219     C   27              GOTO      FOR01
024000960219    2C                   ENDIF
024100960219     C*
024200960219     C* CONTROLLI
024300960219     C                   EXSR      CTRD01
024400960219     C   27              GOTO      FOR01
024500960219     C*
024600960219     C* SE E' STATO IMMESSO UNO SPECIFICO FOGLIO VIAGGIO VISUALIZZO
024700960219     C*   SOLO QUELLO
024800960219     C                   Z-ADD     V1CNFV        FVA(1)
024900960219     C                   MOVEL     V1CFGS        PA1LAI
025000960219     C                   MOVEL     V1CABB        PA1ABB
025100960219     C                   MOVEL     WDFD          PA1DSD
025200960219     C                   MOVEL     WDFA          PA1DSA
025300960219     C                   MOVEL     V1CLNP        PA1LNP
025400960219     C                   MOVEL     SIMFEL        PA1FEL
025500960219    1C                   ENDIF
025600960219     C*
025700951120     C*
025800951121     C* SE E' STATO PASSATO UN FOGLIO VIAGGIO NELLA SCHIERA FVA EMETTO
025900951121     C*   DIRETTAMENTE IL SUBFILE CON SOLO QUEL FOGLIO VIAGGIO
026000951121    1C     FVA(1)        IFGT      0
026100951121     C                   EXSR      CARSFL
026200951120     C*
026300951121   X1C                   ELSE
026400951120     C*
026500941028     C* OPERAZIONI INIZIALI
026600941028     C                   EXSR      INIZIO
026700941027     C* CARICO I FOGLI
026800910423     C                   EXSR      PULIZ
026900951121    1C                   ENDIF
027000911009     C*
027100941014     C* SE IL SUBFILE E' VUOTO ESCO DAL PROGRAMMA
027200960220    1C     NRR1          IFEQ      1
027300960220     C* SE PGM DA MENU' EMETTO MESSAGGIO PER AVVISARE CHE NON C'E'
027400960220     C*   NULLA DA VISUALIZZARE
027500960220    2C     PA1PGM        IFEQ      *BLANKS
027600960220     C                   MOVEL     MSG(10)       V1CMSG
027700960220     C                   SETON                                        4227
027800960220     C                   GOTO      FOR01
027900960220     C*
028000960220   X2C                   ELSE
028100960220     C* SE PGM RICHIAMATO VADO A FINE PGM
028200911010     C                   GOTO      FINE
028300960220    2C                   ENDIF
028400960220    1C                   ENDIF
028500941014     C*
028600941014     C* EMETTO IL SUBFILE DI SCELTA FOGLIO VIAGGIO
028700941013     C                   Z-ADD     1             NRR1
028800941028     C*
028900900515     C     FMT00         TAG
029000941013     C                   WRITE     LR26T01                                      TESTATA VIDEO
029100941013     C                   WRITE     LR26Z01
029200920225     C                   SETOFF                                       37
029300941013     C                   EXFMT     LR26C01
029400961010     C* F12 - RITORNO
029500960220     C* SE PGM RICHIAMATO VADO A FINE ALTRIMENTI RIEMETTO LA 1° VIDEATA
029600960220     C   KLPA1PGM        IFNE      *BLANKS
029700960220     C                   GOTO      FINE
029800960220     C                   ELSE
029900960220     C                   GOTO      FMTD01
030000960220     C                   ENDIF
030100961010     C**
030200961010     C* F7- ABBINAMENTO FOGLI VIAGGIO
030300961010     C     *INKG         IFEQ      *ON
030400971002     C                   MOVEL     '6'           KPJBU
030500961010     C                   CALL      'FNLR25R'
030600961010     C                   PARM                    KPJBA
030700961010     C                   GOTO      FMT00
030800961010     C                   ENDIF
030900961010     C* F14- MANUTENZIONE FOGLI VARI
031000961010     C     *INKN         IFEQ      *ON
031100961010     C                   CALL      'FNLR01R'
031200961010     C                   PARM                    KPJBA
031300961010     C                   GOTO      FMT00
031400961010     C                   ENDIF
031500911009     C*
031600920225     C* ENTER - CERCO LE SPUNTE DA VISUALIZZARE
031700941013     C                   Z-ADD     1             NRR1
031800941013     C                   READC     LR26S01                                30
031900911009     C*
032000941028    1C     *IN30         DOWEQ     '0'
032100941027     C*
032200941028    2C     VIDSCE        IFEQ      '1'
032300920225     C                   SETON                                        36
032400941028    2C                   END
032500921125     C* VISUALIZZA SPUNTE
032600941028    2C     VIDSCE        IFEQ      'S'
032700941028     C                   MOVEL     ' '           VIDSCE
032800920225     C                   SETON                                        37
032900921120     C*
033000941028     C* SE SONO PA1FEL VEDO TUTTE LE SPUNTE
033100921120     C* SE SONO SECONDARIA VEDO SOLO LE MIE SPUNTE
033200941220    3C****       PA1FEL    IFEQ PA1LAI
033300941220     C****                 Z-ADD0         VIDLAF
033400941220     C****                 ELSE
033500941220     C****                 Z-ADDPA1LAI    VIDLAF
033600941220    3C****                 END
033700921120     C*
033800941220     C                   CLEAR                   PARA04
033900941220     C                   Z-ADD     VIDLNP        P04FGS                         FIL.GESTIONE
034000941220     C                   Z-ADD     1             P04NPG                         CATEGORIA
034100941220     C                   MOVEL     VIDNFV        P04NFV                         NR.FOGLIO
034200941220     C                   Z-ADD     99            P04NRS
034300941220     C                   Z-ADD     99            P04NPS
034400941220     C                   MOVEL     '1'           P04FLG                         '1'=INTERROGAZIONE
034500941220     C                   MOVEL     'FNLR26R '    P04PGM                         RICHIAMATO
034600941220     C                   MOVEL     PARA04        KPJBU
034700941220     C                   CALL      'FNLR04R'
034800920225     C                   PARM                    KPJBA
034900941028    2C                   ENDIF
035000920225     C*
035100911009     C*
035200921125     C* VISUALIZZA FOGLIO
035300941028    2C     VIDSCE        IFEQ      '5'
035400921125     C                   SETON                                        37
035500921125     C                   EXSR      VISUAL
035600941028     C                   MOVEL     ' '           VIDSCE
035700941028    2C                   ENDIF
035800921125     C*
035900921125     C* 01 ON - METTO IN R.I.
036000941028    2C     VIDATR        IFEQ      'T'
036100921125     C                   SETON                                        01
036200921125     C                   ELSE
036300921125     C                   SETOFF                                       01
036400941028    2C                   END
036500941027     C**
036600941013     C                   UPDATE    LR26S01
036700941027     C                   SETOFF                                       36
036800941027     C**
036900941013     C                   READC     LR26S01                                30
037000941028    1C                   END
037100920225     C*
037200920225     C   37              GOTO      FMT00
037300900515      *
037400920225     C*       - CERCO I FV DA PASSARE
037500980204     C                   Z-ADD     0             I
037600941013     C                   READC     LR26S01                                30
037700920225     C*
037800980204     C     *IN30         DOWEQ     *OFF
037900980204     C* SELEZIONE
038000920225     C     VIDSCE        IFEQ      '1'
038100980204     C                   ADD       1             I
038200980204     C**
038300980204     C     I             IFLE      70
038400920225     C                   Z-ADD     VIDNFV        FVA(I)
038500920225     C                   MOVEL     VIDLNP        FVA(I)
038600980204     C                   ELSE
038700980204     C                   MOVEL     MSG(2)        PA1MSG
038800980204     C                   SETON                                        30
038900980204     C                   ENDIF
039000980204     C                   ENDIF
039100920225     C*
039200980204     C  N30              READC     LR26S01                                30
039300980204     C                   ENDDO
039400980204     C*
039500920225     C*
039600960221     C* SE PROGRAMMA RICHIAMATO DA MENU' CON ENTER RIEMETTO LA VIDEATA
039700960221     C     PA1PGM        IFEQ      *BLANKS
039800960221     C                   GOTO      FMT00
039900960221     C                   ENDIF
040000960221     C*
040100920327     C     FINE          TAG
040200960221     C*
040300960221     C* SOTTOMETTO AZIONE DI TRASMISSIONE FNFVA00F/FNFV400F IN SEDE E
040400960221     C*   ALLA FILIALE DI PARTENZA SOLO SE SONO STATI VARIATI
040500960221     C     WTRASM        IFNE      ' '
040600960221     C                   MOVEL     PARAM2        KPJBU
040700960221     C                   MOVEL     'LR20'        KCOAZ
040800960221     C                   CALL      'BCH10'
040900960221     C                   PARM                    KPJBA
041000960221     C                   ENDIF
041100971128     C*
041200971128     C                   MOVEL     'C'           D55TLA
041300971128     C                   CALL      'FNLV55R'
041400971128     C                   PARM                    DSLV55
041500960221     C*
041600941013     C                   MOVEL     PARAM1        KPJBU
041700000000     C                   SETON                                        LR
041800960219     C*-----------------------------------------------------***********
041900960219     C* PULIZIA CAMPI VIDEATA SELEZIONI                     *  PULD01 *
042000960219     C*-----------------------------------------------------***********
042100960219     C     PULD01        BEGSR
042200960219     C*
042300960219     C*
042400960220     C                   CLEAR                   FVA
042500960219     C                   CLEAR                   V1CLNP
042600960219     C                   CLEAR                   V1CNFV
042700960219     C                   CLEAR                   V1CDFD
042800960219     C                   CLEAR                   V1CDFA
042900960219     C                   CLEAR                   V1CDAD
043000960219     C                   CLEAR                   V1CDAA
043100960219     C                   CLEAR                   V1CABB
043200960220     C                   CLEAR                   V1CRTP
043300960220     C                   CLEAR                   V1CRTA
043400960220     C                   CLEAR                   WDFD
043500960220     C                   CLEAR                   WDFA
043600960220     C                   CLEAR                   WDAD
043700960220     C                   CLEAR                   WDAA
043800960219     C                   ENDSR
043900960219     C*
044000960219     C*-----------------------------------------------------***********
044100960219     C* CONTROLLO LA FILIALE IN GESTIONE                    *  CTRFGS *
044200960219     C*-----------------------------------------------------***********
044300960219     C     CTRFGS        BEGSR
044400960219     C                   CLEAR                   DSLV50
044500960219     C                   MOVEL     KNMUS         D50PRU
044600960219     C                   MOVEL     WFIL          D50FGS
044700960219     C                   CALL      'FNLV50R'
044800960219     C                   PARM                    DSLV50
044900960219     C*
045000960219    1C     D50ERR        IFNE      ' '
045100960219     C                   MOVEL     D50MSG        V1CMSG
045200960219     C                   SETON                                        4027
045300960219     C                   GOTO      ENDCTF
045400960219    1C                   ENDIF
045500960219     C*
045600960219     C     ENDCTF        ENDSR
045700960219     C*
045800960219     C*-----------------------------------------------------***********
045900960219     C* CONTROLLO CAMPI VIDEATA SELEZIONI                   *  CTRD01 *
046000960219     C*-----------------------------------------------------***********
046100960219     C     CTRD01        BEGSR
046200960220     C                   SETOFF                                       06
046300960219     C*
046400960219     C****  LINEA DI PARTENZA  ****
046500960220    1C     V1CLNP        IFNE      0
046600960219     C     V1CLNP        CHAIN     AZORG                              31
046700960220    2C     *IN31         IFEQ      *ON
046800960219     C                   MOVEL     MSG(1)        V1CMSG
046900960219     C                   SETON                                        4127
047000960219     C                   GOTO      ENDCTR
047100960220    2C                   ENDIF
047200960220    1C                   ENDIF
047300960219     C*
047400960219     C****  OCCORRE IMMETTERE O IL NUMERO FOGLIO O UNA DATA  ****
047500960219    1C     V1CNFV        IFEQ      0
047600960219     C     V1CDFD        ANDEQ     0
047700960219     C     V1CDAD        ANDEQ     0
047800960219     C                   MOVEL     MSG(3)        V1CMSG
047900960219     C                   SETON                                        4227
048000960219     C                   GOTO      ENDCTR
048100960219    1C                   ENDIF
048200960219     C*
048300960219     C****  IMPOSSIBILE INSERIRE TUTTI E TRE  ****
048400960219    1C     V1CNFV        IFNE      0
048500960220     C     V1CDFD        IFNE      0
048600960220     C     V1CDAD        ORNE      0
048700960220     C                   MOVEL     MSG(4)        V1CMSG
048800960220     C                   SETON                                        4227
048900960220     C                   GOTO      ENDCTR
049000960220     C                   ENDIF
049100960219    1C                   ENDIF
049200960219     C*
049300960219     C****  NUMERO FOLGIO VIAGGIO  ****
049400960219    1C     V1CNFV        IFNE      0
049500960219     C* SE IMMESSO IL FOGLIO VIAGGIO OCCORRE INSERIRE ANCHE LA LNP
049600960219    2C     V1CLNP        IFEQ      0
049700960219     C                   MOVEL     MSG(5)        V1CMSG
049800960219     C                   SETON                                        4127
049900960219     C                   GOTO      ENDCTR
050000960219    2C                   ENDIF
050100960219     C*
050200960219     C* CONTROLLO VALIDITA' NUMERO FOGLIO VIAGGIO
050300980112     C* CARICO LA L6
050400980112     C                   CLEAR                   DSUL06
050500980112     C                   MOVE      '£6'          D06COD
050600980112     C                   MOVEL     V1CFGS        D06KEY
050700980112     C                   MOVEL     'L'           D06TLA
050800980112     C                   MOVEL     DSUL06        KPJBU
050900980112     C*
051000980112     C                   CALL      'TRUL06R'
051100980112     C                   PARM                    KPJBA
051200980112     C                   MOVEL     KPJBU         DSUL06
051300980112     C                   MOVEA     LIN           L6
051400980112     C**
051500980112     C* CONTROLLO SE ESISTE IL FOGLIO PER UNA DELLA FIL DI L6
051600980112     C                   Z-ADD     1             XX
051700980112     C                   CLEAR                   WTROV
051800980112     C     L6(XX)        DOWGT     0
051900980112     C                   Z-ADD     L6(XX)        WLAI
052000960220     C     KFVA1         CHAIN(N)  FNFVA01L                           31
052100980112    3C     *IN31         IFEQ      *ON
052200980112     C                   ADD       1             XX
052300980112     C                   ELSE
052400980112     C                   Z-ADD     30            XX
052500980112     C                   MOVEL     'S'           WTROV             1
052600980112    3C                   ENDIF
052700980112    2C                   ENDDO
052800980112     C** NON ANCORA TROVATO
052900980112    2C     WTROV         IFEQ      ' '
053000980112     C* CERCO PER IL TERMINAL DI ARRIVO DI LAI SE NON C'E' GIA' IL L6
053100980112     C     KFVA7         CHAIN(N)  FNFVA01L                           31
053200980112    3C     *IN31         IFEQ      *OFF
053300980112     C                   CLEAR                   DSLV55
053400980112     C                   MOVEL     'A'           D55TPT
053500980223     C     PA1LAI        IFGT      *ZEROS
053600980223     C                   MOVE      PA1LAI        D55LIN
053700980223     C                   ELSE
053800980223     C                   MOVE      V1CFGS        D55LIN
053900980223     C                   END
054000980112     C                   Z-ADD     FVADFV        D55DRF
054100980112     C                   CALL      'FNLV55R'
054200980112     C                   PARM                    DSLV55
054300980112     C     D55TFA        LOOKUP    L6                                     32
054400980112    4C     *IN32         IFEQ      *OFF
054500980112     C                   Z-ADD     D55TFA        WLAI
054600980112     C     KFVA7         CHAIN(N)  FNFVA01L                           31
054700980112     C  N31              MOVEL     'S'           WTROV
054800980112    4C                   ENDIF
054900980112    3C                   ENDIF
055000980112    2C                   ENDIF
055100980112     C**
055200980112    2C     WTROV         IFEQ      ' '
055300960219     C                   MOVEL     MSG(6)        V1CMSG
055400960219     C                   SETON                                        4227
055500960219     C                   GOTO      ENDCTR
055600960219    2C                   ENDIF
055700960219    1C                   ENDIF
055800960219     C*
055900960219     C****  DATA FOGLIO VIAGGIO  DAL  ****
056000960219    1C     V1CDFD        IFNE      0
056100960219     C                   MOVEL     V1CDFD        G02DAT
056200960219     C                   MOVEL     *ZERO         G02INV
056300960219     C                   MOVEL     *BLANKS       G02ERR
056400960219     C                   CALL      'XSRDA8'
056500960219     C                   PARM                    WLBDAT
056600960219     C     G02ERR        IFEQ      '1'
056700960219     C                   MOVEL     MSG(7)        V1CMSG
056800960219     C                   SETON                                        4327
056900960219     C                   GOTO      ENDCTR
057000960219     C                   ENDIF
057100960219     C                   Z-ADD     G02DAT        V1CDFD
057200960219     C                   Z-ADD     G02INV        WDFD
057300960219    1C                   ENDIF
057400960219     C*
057500960219     C****  DATA FOGLIO VIAGGIO  AL  ****
057600960219    1C     V1CDFA        IFNE      0
057700960219     C                   MOVEL     V1CDFA        G02DAT
057800960219     C                   MOVEL     *ZERO         G02INV
057900960219     C                   MOVEL     *BLANKS       G02ERR
058000960219     C                   CALL      'XSRDA8'
058100960219     C                   PARM                    WLBDAT
058200960219     C     G02ERR        IFEQ      '1'
058300960219     C                   MOVEL     MSG(7)        V1CMSG
058400960219     C                   SETON                                        4427
058500960219     C                   GOTO      ENDCTR
058600960219     C                   ENDIF
058700960219     C                   Z-ADD     G02DAT        V1CDFA
058800960219     C                   Z-ADD     G02INV        WDFA
058900960219     C*
059000960219     C* DATA DAL NO MAGGIORE DATA AL
059100960219    2C     WDFD          IFGT      WDFA
059200960219     C                   MOVEL     MSG(8)        V1CMSG
059300960219     C                   SETON                                        4327
059400960219     C                   GOTO      ENDCTR
059500960219    2C                   ENDIF
059600960219    1C                   ENDIF
059700960219     C*
059800960219     C****  DATA FOGLIO ARRIVI   DAL  ****
059900960219    1C     V1CDAD        IFNE      0
060000960219     C                   MOVEL     V1CDAD        G02DAT
060100960219     C                   MOVEL     *ZERO         G02INV
060200960219     C                   MOVEL     *BLANKS       G02ERR
060300960219     C                   CALL      'XSRDA8'
060400960219     C                   PARM                    WLBDAT
060500960219     C     G02ERR        IFEQ      '1'
060600960219     C                   MOVEL     MSG(7)        V1CMSG
060700960219     C                   SETON                                        4527
060800960219     C                   GOTO      ENDCTR
060900960219     C                   ENDIF
061000960219     C                   Z-ADD     G02DAT        V1CDAD
061100960219     C                   Z-ADD     G02INV        WDAD
061200960219    1C                   ENDIF
061300960219     C*
061400960219     C****  DATA FOGLIO ARRIVI   AL  ****
061500960219    1C     V1CDAA        IFNE      0
061600960219     C                   MOVEL     V1CDAA        G02DAT
061700960219     C                   MOVEL     *ZERO         G02INV
061800960219     C                   MOVEL     *BLANKS       G02ERR
061900960219     C                   CALL      'XSRDA8'
062000960219     C                   PARM                    WLBDAT
062100960219     C     G02ERR        IFEQ      '1'
062200960219     C                   MOVEL     MSG(7)        V1CMSG
062300960219     C                   SETON                                        4627
062400960219     C                   GOTO      ENDCTR
062500960219     C                   ENDIF
062600960219     C                   Z-ADD     G02DAT        V1CDAA
062700960219     C                   Z-ADD     G02INV        WDAA
062800960219     C*
062900960219     C* DATA DAL NO MAGGIORE DATA AL
063000960219    2C     WDAD          IFGT      WDAA
063100960219     C                   MOVEL     MSG(8)        V1CMSG
063200960219     C                   SETON                                        4527
063300960219     C                   GOTO      ENDCTR
063400960219    2C                   ENDIF
063500960219    1C                   ENDIF
063600960219     C*
063700960219     C****  RICHIESTA FOGLIO VIAGGIO NON ABBINATI  ****
063800960220     C* SE IMMMESSA LA DATA FOGLIO ARRIVI E' IMPOSSIBILE RICHIEDERE I
063900960220     C*   FOGLI VIAGGIO ANCORA DA ABBINARE
064000960219     C     V1CABB        IFEQ      'N'
064100960219     C     V1CDAD        ANDNE     0
064200960219     C                   MOVEL     MSG(9)        V1CMSG
064300960219     C                   SETON                                        4727
064400960219     C                   GOTO      ENDCTR
064500960219     C                   ENDIF
064600960219     C*
064700960219     C     ENDCTR        ENDSR
064800960220     C*
064900960219     C*-----------------------------------------------------***********
065000960219     C* OPERAZIONI INIZIALI                                 * INIZIO  *
065100960219     C*-----------------------------------------------------***********
065200941028     C     INIZIO        BEGSR
065300941028     C*
065400941028     C* SE IL CODICE FILIALE CUI INVIO E' VUOTO, LO DETERMINO
065500941028    1C     PA1LAI        IFEQ      *ZERO
065600941028     C*
065700020424    2C***        REM       IFEQ 'REM'                      DAL NOME UTENTE
065800020424     C***                  MOVELREMFIL    PA1LAI
065900020424   X2C***                  ELSE
066000020424     C***                  Z-ADDPA1FEL    PA1LAI
066100020424    2C***                  ENDIF
066200020424     C     SIMTPP        IFEQ      '2'
066300020424     C     SIMTPP        OREQ      *BLANKS
066400020424     C                   Z-ADD     SIMPOU        PA1LAI
066500020424     C                   ELSE
066600020424     C                   Z-ADD     PA1FEL        PA1LAI
066700020424     C                   ENDIF
066800941028    1C                   ENDIF
066900980112     C* CARICO LA L6
067000980112     C                   CLEAR                   DSUL06
067100980112     C                   MOVE      '£6'          D06COD
067200980112     C                   MOVEL     PA1LAI        D06KEY
067300980112     C                   MOVEL     'L'           D06TLA
067400980112     C                   MOVEL     DSUL06        KPJBU
067500980112     C*
067600980112     C                   CALL      'TRUL06R'
067700980112     C                   PARM                    KPJBA
067800980112     C                   MOVEL     KPJBU         DSUL06
067900980112     C                   MOVEA     LIN           L6
068000941028     C*
068100980112     C* MEMORIZZO IL TERMINAL ARRIVO DI LAI PER VEDERE SE CARICARE ANCH
068200980112     C*  E I SUOI FOGLI VIAGGIO
068300971127     C                   CLEAR                   DSLV55
068400971127     C                   MOVEL     'A'           D55TPT
068500971127     C                   MOVE      PA1LAI        D55LIN
068600971128     C     WDFD          IFGT      *ZEROS
068700971128     C                   Z-ADD     WDFD          D55DRF
068800971128     C                   ELSE
068900971128     C                   Z-ADD     WDAD          D55DRF
069000971128     C                   END
069100971127     C                   CALL      'FNLV55R'
069200971127     C                   PARM                    DSLV55
069300980112     C                   MOVEL     D55TFA        LAITFA
069400941028     C*
069500960219     C* 29 ON - VISUALIZZO SOLO FOGLI NON ABBINATI A FOGLIO ARRIVI
069600941028     C     PA1ABB        COMP      'N'                                    29     NON ABBINATI
069700941028     C*
069800941028     C*
069900960219     C* 28 ON - IMMESSA SPECIFICA LINEA DI PARTENZA FOGLIO
070000960219     C     PA1LNP        COMP      0                                  28
070100941028     C*
070200960220     C* SOLO SE PGM RICHIAMATO: SE DATA INIZIALE E FINALE SONO NULLE,
070300960220     C*   IMPOSTO QUELLA FINALE
070400941028     C     PA1DSD        IFEQ      *ZERO
070500941028     C     PA1DSA        ANDEQ     *ZERO
070600960220     C     PA1PGM        ANDNE     *BLANKS
070700941028     C                   Z-ADD     99991231      PA1DSA
070800941028     C                   ENDIF
070900941028     C*
071000941028     C* SE C'E' SOLO DATA DAL E NON AL : LE IMPOSTO UGUALI
071100960220     C* DATA FOGLIO VIAGGIO
071200941220     C     PA1DSD        IFNE      *ZERO
071300941220     C     PA1DSA        ANDEQ     *ZERO
071400941028     C                   Z-ADD     PA1DSD        PA1DSA
071500941028     C                   ENDIF
071600960220     C* DATA FOGLIO ARRIVI
071700960220     C     WDAD          IFNE      *ZERO
071800960220     C     WDAA          ANDEQ     *ZERO
071900960220     C                   Z-ADD     WDAD          WDAA
072000960220     C                   ENDIF
072100941028     C*
072200941028     C                   ENDSR
072300960219     C*-----------------------------------------------------***********
072400960219     C* PULIZIA CAMPI VIDEO                                 *  PULIZ  *
072500960219     C*-----------------------------------------------------***********
072600900515     C     PULIZ         BEGSR
072700960219     C*
072800911009     C                   SETON                                        72
072900941013     C                   WRITE     LR26C01
073000911009     C                   SETOFF                                       72
073100911009     C                   SETON                                        73
073200911009     C*
073300980112     C                   CLEAR                   FVC
073400911009     C                   MOVEA     *ZEROS        FVA
073500941013     C                   Z-ADD     1             NRR1              5 0
073600911009     C                   Z-ADD     0             VI2NFV            5 0
073700930519     C                   MOVEL     ' '           VI2NPG            1 0
073800941027     C*
073900921119     C* DECODIFICA LINEA ARRIVO FOGLI
074000941014     C     PA1LAI        CHAIN     AZORG01L                           30
074100921119     C  N30              MOVEL     ORGDES        DESLAI
074200980112     C**
074300980112     C* CARICO TUTTI I FV DELLA L6 COMPRESI QUELLI DEL TERMINAL DI
074400980112     C*  ARRIVO PER RICHIAMAO DA ABBINAMENTO
074500980112     C                   CLEAR                   LEL
074600980112     C                   MOVEA     L6            LEL
074700980112     C     LAITFA        IFNE      PA1LAI
074800980112     C     LAITFA        LOOKUP    L6                                     32
074900980112     C     *IN32         IFEQ      *OFF
075000980112     C                   Z-ADD     1             XX
075100980112     C     000           LOOKUP    LEL(XX)                                30
075200980112     C                   Z-ADD     LAITFA        LEL(XX)
075300980112     C                   ENDIF
075400980112     C                   ENDIF
075500941028     C**
075600980112     C* RICHIESTI SOLO FV DA ABBINARE
075700941028     C**
075800941028    1C     *IN29         IFEQ      *ON
075900980112     C                   Z-ADD     1             XX
076000980112     C**
076100980112    2C     XX            DOWLE     30
076200980112     C     LEL(XX)       ANDGT     0
076300980112     C                   Z-ADD     LEL(XX)       WLAI
076400941028     C*
076500960402     C* NON IMMESSA UNA SPECIFICA LINEA DI PARTENZA
076600941028     C  N28KFVA3         SETLL     FNFVA02L
076700941028     C  N28KFVA3         READE     FNFVA02L                               30
076800941028     C*
076900941028     C* IMMESSA UNA SPECIFICA LINEA DI PARTEZA
077000941028     C   28KFVA5         SETLL     FNFVA02L
077100941028     C   28KFVA5         READE     FNFVA02L                               30
077200911009     C*
077300941028    3C     *IN30         DOWEQ     *OFF
077400941014     C*
077500941014     C* VERIFICO LE INCLUSIONI/ESCLUSIONI
077600941014     C                   EXSR      ESCLUS                                       ESCLUSIONI
077700911009     C*
077800941028     C  N28KFVA3         READE     FNFVA02L                               30
077900941028     C   28KFVA5         READE     FNFVA02L                               30
078000941028    3C                   ENDDO
078100941028     C*
078200980112     C                   ADD       1             XX                3 0
078300941028    2C                   ENDDO
078400941028     C*
078500941028   X1C                   ELSE
078600941028     C*
078700941028     C* RICHIESTI TUTTI I FOGLI O SOLO GLI ABBINATI: LEGGO PER DATA
078800941028     C                   Z-ADD     PA1LNP        WLNP
078900941028     C*
079000960220     C* LETTURA PER   D A T A   F O G L I O   V I A G G I O
079100960220     C     PA1DSD        IFNE      0
079200960220     C*
079300941028     C* NON IMMESSA UNA SPECIFICA LINEA DI PARTEZA
079400941028     C  N28*LOVAL        SETLL     FNFVA03L
079500941028     C  N28              READ      FNFVA03L                               30
079600941028     C*
079700941028     C* IMMESSA UNA SPECIFICA LINEA DI PARTENZA
079800941028     C   28KFVA4         SETLL     FNFVA03L
079900941028     C   28WLNP          READE     FNFVA03L                               30
080000960220     C*
080100960220     C                   ELSE
080200960220     C* LETTURA PER   D A T A   F O G L I O   A R R I V I
080300960220     C*
080400960220     C     KFVA6         SETLL     FNFVA05L
080500960220     C     V1CFGS        READE     FNFVA05L                               30
080600960220     C                   ENDIF
080700941028     C*
080800941028    2C     *IN30         DOWEQ     *OFF
080900941028     C*
081000941028     C                   EXSR      ESCLUS
081100941028     C*
081200960220     C* LETTURA PER   D A T A   F O G L I O   V I A G G I O
081300960220     C     PA1DSD        IFNE      0
081400960220     C*
081500941028     C* SE PER CASO DATA MAGGIORE DATA AL MI POSIZIONO SULLA LINEA SUCC
081600941028    3C     FVADFV        IFGT      PA1DSA
081700941028     C                   Z-ADD     FVALNP        WLNP
081800941028     C     WLNP          SETGT     FNFVA003
081900941028    3C                   ENDIF
082000941028     C*
082100941028     C  N28              READ      FNFVA03L                               30
082200941028     C   28WLNP          READE     FNFVA03L                               30
082300960220     C*
082400960220     C                   ELSE
082500960220     C* LETTURA PER   D A T A   F O G L I O   A R R I V I
082600960220     C*
082700960220     C     FVADFA        IFGT      WDAA
082800960220     C                   SETON                                        30
082900960220     C                   ELSE
083000960220     C     V1CFGS        READE     FNFVA05L                               30
083100960220     C                   ENDIF
083200960220     C                   ENDIF
083300960220     C*
083400941028    2C                   ENDDO
083500941028    1C                   ENDIF
083600960219     C*
083700900514     C                   ENDSR
083800951121     C*-----------------------------------------------------***********
083900951121     C* CARICO NEL SFL SOLO IL FOGLIO PASSATO               * CARSFL  *
084000951121     C*-----------------------------------------------------***********
084100951121     C     CARSFL        BEGSR
084200951121     C*
084300951121     C                   SETON                                        72
084400951121     C                   WRITE     LR26C01
084500951121     C                   SETOFF                                       72
084600951121     C                   SETON                                        73
084700951121     C*
084800960219     C                   Z-ADD     1             NRR1
084900951121     C                   MOVEL     PA1LNP        VIDLNP
085000951121     C                   Z-ADD     FVA(1)        VIDNFV
085100951121     C                   MOVEL     PA1LAI        VIDLAI
085200951121     C*
085300951121     C* DECODIFICA LINEA ARRIVO FOGLI
085400951121     C     PA1LAI        CHAIN     AZORG01L                           30
085500951121     C  N30              MOVEL     ORGDES        DESLAI
085600951121     C*
085700960220     C     KFVA2         CHAIN(N)  FNFVA01L                           30
085800951121     C*
085900951121    1C     *IN30         IFEQ      *OFF
086000960205     C                   EXSR      RIEMPI
086100960205     C*
086200951121     C                   WRITE     LR26S01
086300951121     C                   ADD       1             NRR1
086400951121    1C                   ENDIF
086500951121     C*
086600951121     C                   ENDSR
086700960205     C*-----------------------------------------------------***********
086800960205     C* RIEMPO CAMPI SFL                                    * RIEMPI  *
086900960205     C*-----------------------------------------------------***********
087000960205     C     RIEMPI        BEGSR
087100960205     C*
087200960205     C                   MOVEL     FVANFV        VIDNFV
087300960205     C* GIRO DATA FOGLIO VIAGGIO
087400960205     C                   MOVE      FVADFV        G02INV
087500960205     C                   MOVE      *ZEROS        G02DAT
087600960205     C                   MOVEL     3             G02ERR
087700960205     C                   CALL      'XSRDA8'
087800960205     C                   PARM                    WLBDAT
087900960205     C                   Z-ADD     G02DAT        VIDDFV
088000000612     C* P.O.GESTIONE
088100000612     C     KFWA1         CHAIN     FNFWA01L                           30
088200000612     C     *IN30         IFEQ      *OFF
088300000612     C     FWAFGS        ANDNE     0
088400000612     C                   Z-ADD     FWAFGS        WORKPT            3 0
088500000612     C                   ELSE
088600000612     C                   Z-ADD     FVALNP        WORKPT            3 0
088700000612     C                   ENDIF
088800000612     C                   CLEAR                   OG143
088900000612     C     WORKPT        CHAIN     AZORG                              30
089000000612     C   30              MOVE      '  '          VIDPT1
089100000612     C  N30              MOVEL     ORGDE3        OG143
089200020211     C  N30§OGNTW        IFEQ      'PPT'
089300000612     C                   MOVE      'PT'          VIDPT1
089400000612     C                   ELSE
089500000612     C                   MOVE      '  '          VIDPT1
089600000612     C                   ENDIF
089700960205     C* DECODIFICA LINEA DI PARTENZA
089800960205     C                   Z-ADD     FVALNP        VIDLNP
089900960205     C     VIDLNP        CHAIN     AZORG01L                           30
090000960205     C     *IN30         IFEQ      *OFF
090100960402     C**
090200960402     C     ORGDE5        IFNE      *BLANKS
090300960402     C                   MOVEL     ORGDE5        DESLNP
090400960402     C                   ELSE
090500960402     C                   MOVEL     ORGDES        DESLNP
090600960402     C                   ENDIF
090700960402     C**
090800960205     C                   ELSE
090900960206     C                   MOVEL     *BLANKS       DESLNP
091000960205     C                   ENDIF
091100960205     C*
091200960205     C                   MOVEL     FVANPG        VIDNPG                         CATEG.FOGLIO ARRIVI
091300960205     C                   Z-ADD     FVANFA        VIDNFA                         NR.FOGLIO ARRIVI
091400960205     C* DATA FOGLIO ARRIVI
091500960205     C     FVADFA        IFNE      *ZERO
091600960205     C                   MOVE      FVADFA        G02INV                         AAAA/MM/GG
091700960205     C                   MOVE      *ZEROS        G02DAT
091800960205     C                   MOVEL     3             G02ERR
091900960205     C                   CALL      'XSRDA8'
092000960205     C                   PARM                    WLBDAT
092100960205     C                   Z-ADD     G02DAT        VIDDFA                         GG/MM/AAAA
092200960205     C                   ELSE
092300960205     C                   Z-ADD     *ZERO         VIDDFA
092400960205     C                   ENDIF
092500960205      *
092600960205     C                   MOVEL     FVAATR        VIDATR
092700960205     C                   MOVEL     FVALAI        VIDLAI
092800960205     C* 01 ON - METTO IN R.I.
092900960205     C     FVAATR        COMP      'T'                                    01
093000960205     C*
093100960205     C* NUMERO TRAINO
093200960205     C                   MOVEL     FVATRN        VIDTRN
093300960205     C*
093400960205     C* TIPO TRAINO CON DECODIFICA
093500960205     C                   CLEAR                   VIDTTR
093600960205     C     FVATTR        IFNE      ' '
093700960205     C                   MOVEL     FVATTR        W002A             2
093800960205     C                   MOVE      '-'           W002A
093900960205     C                   MOVEL     'TV'          COD
094000960205     C                   MOVEL(P)  FVATTR        KEY
094100960205     C     KTAB          CHAIN     TABEL                              32
094200960205     C  N32              MOVEL     TBLUNI        W004A             4
094300960205     C   32              CLEAR                   W004A
094400960205     C                   MOVEL     W002A         VIDTTR
094500960205     C                   MOVE      W004A         VIDTTR
094600960205     C                   ENDIF
094700960402     C* VERIFICO SE IL FV HA BOLLE
094800960402     C     KARB          SETLL     FNARB06L                               32
094900960402     C  N32              MOVEL     '**'          VIDAST
095000960402     C   32              MOVEL     '  '          VIDAST
095100960205     C*
095200960205     C                   ENDSR
095300960219     C*-----------------------------------------------------***********
095400960219     C* ESCLUSIONI LETTURE DALL'ARCHIVIO                    *  ESCLUS *
095500960219     C*-----------------------------------------------------***********
095600941014     C     ESCLUS        BEGSR
095700941014     C*
095800941014     C                   MOVEL     'S'           WOK               1            RECORD VALIDO
095900941014     C*
096000941028     C* SOLO FOGLI ABBINATI
096100941014     C     PA1ABB        IFEQ      'A'                                          'A'=SOLO ABBINATI
096200941014     C     FVANFA        ANDEQ     *ZERO                                        NR.FOGLIO ARRIVI
096300941028     C* DEVE ESSERCI NR.FOGLIO ARRIVI
096400941014     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
096500941014     C                   ENDIF
096600941014     C*
096700960220     C* VERIFICO LA DATA FOGLIO VIAGGIO PARTENZA
096800960220    1C     PA1DSD        IFNE      0
096900960220    2C     FVADFV        IFLT      PA1DSD                                       DATA INIZIALE
097000941014     C     FVADFV        ORGT      PA1DSA                                       DATA FINALE
097100941014     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
097200960220    2C                   ENDIF
097300960220    1C                   ENDIF
097400960220     C*
097500960220     C* VERIFICO LA DATA FOGLIO ARRIVI
097600960220    1C     WDAD          IFNE      0
097700960220    2C     FVADFA        IFLT      WDAD                                         DATA INIZIALE
097800960220     C     FVADFA        ORGT      WDAA                                         DATA FINALE
097900960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
098000960220    2C                   ENDIF
098100960220     C*
098200960220     C* SE IMMESSA CONTROLLO LA LINEA DI PARTENZA
098300960220    2C   28FVALNP        IFNE      PA1LNP
098400960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
098500960220    2C                   ENDIF
098600960220    1C                   ENDIF
098700941014     C*
098800960220     C* SE RICHIESTO CONTROLLO IL RITARDO IN PARTENZA
098900960220    1C     V1CRTP        IFEQ      'R'
099000960220     C     FVARTP        ANDEQ     ' '
099100960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
099200960220    1C                   ENDIF
099300960220    1C     V1CRTP        IFEQ      'N'
099400960220     C     FVARTP        ANDNE     ' '
099500960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
099600960220    1C                   ENDIF
099700960220     C* SE RICHIESTO CONTROLLO IL RITARDO IN ARRIVO
099800960220    1C     V1CRTA        IFEQ      'R'
099900960220     C     FVARTA        ANDEQ     ' '
100000960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
100100960220    1C                   ENDIF
100200960220    1C     V1CRTA        IFEQ      'N'
100300960220     C     FVARTA        ANDNE     ' '
100400960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
100500960220    1C                   ENDIF
100600960220     C*
100700941014     C* VERIFICO LA LINEA ARRIVO CUI INVIO
100800960220    1C     FVALAI        IFNE      PA1LAI
100900980112     C     FVALAI        LOOKUP    LEL                                    32
101000980112     C  N32              MOVEL     *BLANKS       WOK                            NOBBUONO !!
101100960220    1C                   ENDIF
101200980112     C* ESCLUDO SE LO STESSO FOGLIO L'HO GIA' CARICO PER ALTRA FIL
101300980112     C*  DI L6
101400980112     C     WOK           IFEQ      'S'
101500980112     C     FVAFVC        LOOKUP    FVC                                    32
101600980112     C   32              MOVEL     *BLANKS       WOK                            NOBBUONO !!
101700980112    1C                   ENDIF
101800980112    C**
101900960220    1C     WOK           IFEQ      'S'                                          SOLO SE VALIDO
102000960205     C                   EXSR      RIEMPI
102100960205     C*
102200941028     C                   WRITE     LR26S01
102300980112     C                   Z-ADD     NRR1          YY                3 0
102400980112     C                   Z-ADD     FVAFVC        FVC(YY)
102500980112     C**
102600980112     C                   ADD       1             NRR1
102700980112     C**
102800960220    1C                   ENDIF                                                  WOK = 'S'
102900960220     C*
103000941014     C                   ENDSR
103100960219     C*-----------------------------------------------------***********
103200960219     C* VISUALIZZA FOGLIO                                   *  VISUAL *
103300960219     C*-----------------------------------------------------***********
103400921125     C     VISUAL        BEGSR
103500960220     C                   SETOFF                                       161713
103600130625     C                   SETON                                        1109
103700941014     C*
103800960206     C* PULIZIA SFL
103900921125     C                   Z-ADD     1             NRR2              5 0
104000921125     C                   SETON                                        74
104100941013     C                   WRITE     LR26C02
104200921125     C                   SETOFF                                       7475
104300921125     C*
104400960221     C* SOLO SE PGM DA MENU' VINCOLO IL RECORD
104500960221    1C     *IN20         IFEQ      *OFF
104600960627     C     KFVA2         CHAIN(N)  FNFVA01L                           32
104700960221   X1C                   ELSE
104800000428     C     KFVA2         CHAIN(N)  FNFVA01L                           32
104900960221    1C                   ENDIF
105000921125     C*
105100960206    1C     *IN32         IFEQ      '0'
105200000612     C* P.O.GESTIONE
105300000612     C     KFWA1         CHAIN     FNFWA01L                           30
105400000612     C     *IN30         IFEQ      *OFF
105500000612     C     FWAFGS        ANDNE     0
105600000612     C                   Z-ADD     FWAFGS        WORKPT            3 0
105700000612     C                   ELSE
105800000612     C                   Z-ADD     FVALNP        WORKPT            3 0
105900000612     C                   ENDIF
106000000612     C                   CLEAR                   OG143
106100000612     C     WORKPT        CHAIN     AZORG                              30
106200000612     C   30              MOVE      '  '          VIDPT2
106300000612     C  N30              MOVEL     ORGDE3        OG143
106400020211     C  N30§OGNTW        IFEQ      'PPT'
106500000612     C                   MOVE      'PT'          VIDPT2
106600000612     C                   ELSE
106700000612     C                   MOVE      '  '          VIDPT2
106800000612     C                   ENDIF
106900960220     C* SALVO CAUSALE RITARDO ARRIVO PERCHE' POTREBBE ESSERE VARIATA
107000960220     C                   MOVEL     FVARTA        SAVRTA
107100960220     C*
107200960206     C* PULIZIA CAMPI VIDEATA
107300960206     C                   CLEAR                   VIDDTA
107400090907     C                   CLEAR                   VIDhTA
107500090907     C                   CLEAR                   VIDaed
107600090907     C                   CLEAR                   VIDaeh
107700960206     C                   CLEAR                   VIDDTP
107800090907     C                   CLEAR                   VIDhTP
107900090907     C                   CLEAR                   VIDDRP
108000090907     C                   CLEAR                   VIDhRP
108100960206     C                   CLEAR                   VIDDTN
108200960206     C                   CLEAR                   DESTTR
108300960216     C                   CLEAR                   DESRA1
108400960216     C                   CLEAR                   DESRP1
108500960216     C                   CLEAR                   DESRA2
108600960216     C                   CLEAR                   DESRP2
108700960206     C                   CLEAR                   VIDDPA
108800960206     C                   CLEAR                   VIDDAR
108900010703     C                   CLEAR                   VIDCNC
109000010703     C                   CLEAR                   VIDCAM
109100010703     C                   CLEAR                   VIDPIO
109200010704     C                   CLEAR                   DESTMZ
109300010704     C*
109400010704     C                   SETOFF                                       121418
109500010704     C                   SETOFF                                       19
109600941220     C*
109700960205     C* DATA REALE ARRIVO
109800941220     C                   Z-ADD     FVAAED        G02INV
109900941220     C                   Z-ADD     *ZEROS        G02DAT
110000941220     C                   MOVEL     3             G02ERR
110100941220     C                   CALL      'XSRDA8'
110200941220     C                   PARM                    WLBDAT
110300941220     C                   Z-ADD     G02DAT        VIDAED
110400090907     c* Ora teorica
110500090907     c                   if        fvaaed>0
110600090907     c                   movel     fvaaeh        w004a
110700090907     c                   eval      vidaeh=%subst(w004a:1:2)+':'+
110800090907     c                                    %subst(w004a:3:2)
110900090907     c                   endif
111000960205     C* DATA REALE PARTENZA
111100960205     C                   Z-ADD     FVADRP        G02INV
111200960205     C                   Z-ADD     *ZEROS        G02DAT
111300960205     C                   MOVEL     3             G02ERR
111400960205     C                   CALL      'XSRDA8'
111500960205     C                   PARM                    WLBDAT
111600960205     C                   Z-ADD     G02DAT        VIDDRP
111700090907     c* Ora teorica
111800090907     c                   if        fvadrp>0
111900090907     c                   movel     fvahrp        w004a
112000090907     c                   eval      vidhrp=%subst(w004a:1:2)+':'+
112100090907     c                                    %subst(w004a:3:2)
112200090907     c                   endif
112300090907     c
112400960205     C* DATA TEORICA ARRIVO
112500960206    2C     FVADTA        IFGT      0
112600951218     C                   Z-ADD     FVADTA        G02INV
112700951218     C                   Z-ADD     *ZEROS        G02DAT
112800951218     C                   MOVEL     3             G02ERR
112900951218     C                   CALL      'XSRDA8'
113000951218     C                   PARM                    WLBDAT
113100951218     C                   Z-ADD     G02DAT        VIDDTA
113200090907     c* Ora teorica
113300090907     c                   movel     fvahta        w004a
113400090907     c                   eval      vidhta=%subst(w004a:1:2)+':'+
113500090907     c                                    %subst(w004a:3:2)
113600960206    2C                   ENDIF
113700960205     C* DATA TEORICA PARTENZA
113800960206    2C     FVADTP        IFGT      0
113900960205     C                   Z-ADD     FVADTP        G02INV
114000960205     C                   Z-ADD     *ZEROS        G02DAT
114100960205     C                   MOVEL     3             G02ERR
114200960205     C                   CALL      'XSRDA8'
114300960205     C                   PARM                    WLBDAT
114400960205     C                   Z-ADD     G02DAT        VIDDTP
114500090907     c* Ora teorica
114600090907     c                   movel     fvahtp        w004a
114700090907     c                   eval      vidhtp=%subst(w004a:1:2)+':'+
114800090907     c                                    %subst(w004a:3:2)
114900960206    2C                   ENDIF
115000090907     c*
115100951218     C* DATA TRAINO
115200960206    2C     FVADTN        IFGT      0
115300951218     C                   Z-ADD     FVADTN        G02INV
115400951218     C                   Z-ADD     *ZEROS        G02DAT
115500951218     C                   MOVEL     3             G02ERR
115600951218     C                   CALL      'XSRDA8'
115700951218     C                   PARM                    WLBDAT
115800951218     C                   Z-ADD     G02DAT        VIDDTN
115900960206    2C                   ENDIF
116000960206     C* DECODIFICA TIPO TRAINO
116100960206    2C     FVATTR        IFNE      ' '
116200960206     C                   MOVEL     'TV'          COD
116300960206     C                   MOVEL(P)  FVATTR        KEY
116400960206     C     KTAB          CHAIN     TABEL                              32
116500960206     C  N32              MOVEL     TBLUNI        DESTTR
116600960206    2C                   ENDIF
116700960206     C*
116800960205     C* DECODIFICA CAUSALE RITARDO ARRIVO
116900960206    2C     FVARTA        IFNE      ' '
117000960220     C                   SETON                                        13
117100951218     C                   MOVEL     'TC'          COD
117200951218     C                   MOVEL(P)  FVARTA        KEY
117300951218     C     KTAB          CHAIN     TABEL                              32
117400960206    3C     *IN32         IFEQ      *OFF
117500951218     C     TBLFLG        ANDEQ     ' '
117600960216     C                   MOVEL     TBLUNI        DSTC
117700960216     C                   MOVEL     §TCDES        DESRA1
117800960216     C                   MOVE      §TCDES        DESRA2
117900960206    3C                   ENDIF
118000960206     C*
118100960206     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
118200960206     C                   MOVEL     'A'           WEPA
118300960206     C                   MOVEL     FVALAI        ZERO
118400960206     C                   EXSR      CTRDRA
118500960206     C* 17 ON  - ESISTE DESCRIZIONE RITARDO/ANTICIPO PER LA PARTENZA
118600960206    3C     W001A         IFNE      ' '
118700960206     C                   SETON                                        17
118800960206    3C                   ENDIF
118900960206    2C                   ENDIF
119000960205     C*
119100960205     C* DECODIFICA CAUSALE RITARDO PARTENZA
119200960206    2C     FVARTP        IFNE      ' '
119300960205     C                   MOVEL     'TC'          COD
119400960205     C                   MOVEL(P)  FVARTP        KEY
119500960205     C     KTAB          CHAIN     TABEL                              32
119600960206    3C     *IN32         IFEQ      *OFF
119700960205     C     TBLFLG        ANDEQ     ' '
119800960216     C                   MOVEL     TBLUNI        DSTC
119900960216     C                   MOVEL     §TCDES        DESRP1
120000960216     C                   MOVE      §TCDES        DESRP2
120100960206    3C                   ENDIF
120200951221     C*
120300951221     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
120400960206     C                   MOVEL     'P'           WEPA
120500960206     C                   CLEAR                   ZERO
120600960206     C                   EXSR      CTRDRA
120700960206     C* 16 ON  - ESISTE DESCRIZIONE RITARDO/ANTICIPO PER L'ARRIVO
120800960206    3C     W001A         IFNE      ' '
120900960206     C                   SETON                                        16
121000960206    3C                   ENDIF
121100960206    2C                   ENDIF
121200921125     C*
121300921125     C* FILIALE DI INVIO
121400941017     C     FVALAI        CHAIN     AZORG01L                           33
121500921125     C  N33              MOVEL     ORGDES        DE2LAI
121600941014     C   33              MOVEL     *BLANKS       DE2LAI
121700921125     C*
121800921125     C* FILIALE ARRIVO FINALE
121900941013     C     FVALNA        CHAIN     AZORG01L                           33
122000921125     C  N33              MOVEL     ORGDES        DESLNA
122100921125     C   33              MOVEL     *BLANKS       DESLNA
122200960206     C*
122300921125     C* 01 ON - METTO IN R.I.
122400960206    2C     FVAATR        IFEQ      'T'
122500921125     C                   SETON                                        01
122600960206   X2C                   ELSE
122700921125     C                   SETOFF                                       01
122800960206    2C                   END
122900000428     C* ESTENSIONE FOGLI
123000000428     C     KFVA2         CHAIN     FNFWA01L                           33
123100000428     C     *IN33         IFEQ      *ON
123200000428     C     FWAATB        ORNE      ' '
123300000428     C                   CLEAR                   FWAFF3
123400000428     C                   CLEAR                   FWAFF4
123500000428     C                   CLEAR                   FWAFL3
123600000428     C                   CLEAR                   FWAFL4
123700000428     C                   ENDIF
123800921125     C*
123900010704     C*REPERISCO IL NUMERO CELLULARE DEL CONDUCENTE
124000010704     C                   MOVEL     'P'           WEPA
124100010704     C                   CLEAR                   ZERO
124200010704     C                   MOVE      'L'           WTRC
124300010704     C     KFV4          CHAIN     FNFV401L                           30
124400010704     C     *IN30         IFEQ      *OFF
124500010704     C                   MOVEL     FV4NOT        VIDCNC
124600010704     C                   ENDIF
124700010704     C     VIDCNC        IFEQ      *BLANKS
124800010704     C                   SETON                                        19
124900010704     C                   ENDIF
125000010704     C*
125100010704     C*REPERISCO I PIOMBI
125200010704     C                   Z-ADD     0             CONT              1 0
125300010704     C                   MOVEL     'P'           WEPA
125400010704     C                   CLEAR                   ZERO
125500010704     C                   MOVE      'P'           WTRC
125600010704     C     KFV4          SETLL     FNFV401L
125700010704     C                   DO        *HIVAL
125800010704     C     KFV4          READE     FNFV401L                               30
125900010704     C     *IN30         IFEQ      *ON
126000010704     C                   LEAVE
126100010704     C                   ENDIF
126200010704     C                   ADD       1             CONT
126300010704     C     CONT          IFEQ      1
126400010704     C                   MOVEL     FV4NOT        VIDPIO
126500010704     C                   ENDIF
126600010704     C     CONT          IFEQ      2
126700010704     C                   MOVEL     FV4NOT        WPIO             13
126800010704     C                   MOVE      WPIO          VIDPIO
126900010704     C                   ENDIF
127000010704     C                   ENDDO
127100010704     C     VIDPIO        IFEQ      *BLANKS
127200010704     C                   SETON                                        18
127300010704     C                   ENDIF
127400010704     C*
127500010704     C*REPERISCO TIPO AUTOMEZZO E CATEGORIA TRAINO
127600010704     C                   MOVEL     'P'           WEPA
127700010704     C                   CLEAR                   ZERO
127800010704     C                   MOVE      'V'           WTRC
127900010704     C     KFV4          CHAIN     FNFV401L                           30
128000010704     C     *IN30         IFEQ      *OFF
128100010704     C                   MOVEL     FV4NOT        DSFV4V
128200010704     C     §V4TMZ        IFEQ      *BLANKS
128300010704     C                   SETON                                        12
128400010704     C                   ELSE
128500010704     C                   MOVEL     §V4TMZ        VIDCAM
128600010704     C                   MOVEL     'TU'          COD
128700010704     C                   MOVEL(P)  VIDCAM        KEY
128800010704     C     KTAB          CHAIN     TABEL                              83
128900010704    1C  N83TBLFLG        IFEQ      ' '
129000010704     C                   MOVEL     TBLUNI        DESTMZ
129100010704    1C                   ENDIF
129200010704     C                   ENDIF
129300010704     C     §V4TTR        IFNE      'B'
129400010704     C                   SETON                                        14
129500010704     C                   ENDIF
129600010704     C                   ELSE
129700010704     C                   SETON                                        1214
129800010704     C                   ENDIF
129900010704     C*
130000960402     C* VERIFICO SE IL FOGLIO VIAGGIO HA BOLLE
130100960402     C     KARB          SETLL     FNARB06L                               02
130200921125     C*
130300921125     C                   Z-ADD     1             I                 3 0
130400960206    2C     FFV(I)        DOWGT     *BLANKS
130500921125     C*
130600921125     C                   MOVEL     FFV(I)        NUM3              3 0
130700921125     C                   MOVEL     FFV(I)        VIDFFV
130800941013     C     NUM3          CHAIN     AZORG01L                           33
130900921125     C  N33              MOVEL     ORGDES        DESFFV
131000921125     C   33              MOVEL     *BLANKS       DESFFV
131100921125     C*
131200921125     C                   MOVEL     FLP(I)        NUM3
131300921125     C                   MOVEL     FLP(I)        VIDFLP
131400941013     C     NUM3          CHAIN     AZORG01L                           31
131500921125     C  N31              MOVEL     ORGDES        DESFLP
131600921125     C   31              MOVEL     *BLANKS       DESFLP
131700921125     C*
131800941013     C                   WRITE     LR26S02
131900010703     C*
132000921125     C                   ADD       1             I
132100921125     C                   ADD       1             NRR2
132200960206    2C                   END
132300921125     C*
132400960206    2C     NRR2          IFEQ      1
132500921125     C                   SETON                                        75
132600960206   X2C                   ELSE
132700921125     C                   Z-ADD     1             NRR2
132800960206    2C                   END
132900921125     C*
133000921125     C     FOR02         TAG
133100941013     C                   WRITE     LR26T01                                      TESTATA VIDEO
133200951221     C                   WRITE     LR26Z02
133300941013     C                   EXFMT     LR26C02
133400960220     C*
133500960220     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN27)
133600960220     C                   CLEAR                   VIDMSG
133700960220     C                   SETOFF                                       2748
133800951221     C*
133900960221     C****
134000960221     C* SOLO SE PGM RICHIAMATO DA MENU' (20 OFF)
134100960221     C****
134200960221   2AC     *IN20         IFEQ      *OFF
134300960221     C****
134400960221     C* F7 - VARIAZIONE CAUSALE RITARDO ARRIVO
134500960221     C****
134600960221    2C     *INKG         IFEQ      *ON
134700960221     C                   SETOFF                                       11
134800960221     C                   GOTO      FOR02
134900960221    2C                   ENDIF
135000960221     C*
135100960220     C****
135200960220     C* CONTROLLO CAUSALE RITARDO ARRIVO
135300960220     C****
135400960220    2C     FVARTA        IFEQ      '?'
135500960220     C                   MOVEL     'TC'          §COD
135600960220     C                   MOVEL     *BLANKS       FVARTA
135700960220     C                   MOVEL     *BLANKS       §DES
135800960220     C                   CALL      'X§TABER'
135900960220     C                   PARM                    CODUT
136000960220     C                   PARM                    §COD              2
136100960220     C                   PARM                    §KEY              8
136200960220     C                   PARM                    §DES             25
136300960220     C                   MOVEL     §KEY          FVARTA
136400960220     C                   MOVEL     §DES          DESRA1
136500960220     C                   MOVE      §DES          W005A             5
136600960221     C                   MOVEL(P)  W005A         DESRA2
136700960220     C                   GOTO      FOR02
136800960220    2C                   ENDIF
136900960220     C*
137000960220    2C     FVARTA        IFNE      ' '
137100960220     C                   MOVEL     'TC'          COD
137200960220     C                   MOVEL(P)  FVARTA        KEY
137300960220     C     KTAB          CHAIN     TABEL                              34
137400960220    3C     *IN34         IFEQ      *OFF
137500960220     C     TBLFLG        ANDNE     ' '
137600960220     C     *IN34         OREQ      *ON
137700960220     C                   MOVEL     MSG(11)       VIDMSG
137800960220     C                   SETON                                        2748
137900960220     C                   GOTO      FOR02
138000960220   X3C                   ELSE
138100960220     C                   MOVEL     TBLUNI        DSTC
138200960220     C                   MOVEL     §TCDES        DESRA1
138300960220     C                   MOVE      §TCDES        DESRA2
138400960220    3C                   ENDIF
138500960220     C*
138600960220   X2C                   ELSE
138700960220     C                   CLEAR                   DESRA1
138800960220     C                   CLEAR                   DESRA2
138900960220    2C                   ENDIF
139000960221   2AC                   ENDIF
139100960220     C*
139200960220     C****
139300960206     C* VISUALIZZO DESCRIZIONE RITARDO/ANTICIPO PER LA PARTENZA
139400960220     C****
139500960206    2C   16VIDDPA        IFEQ      'V'
139600960205     C                   CLEAR                   DSLV15
139700960220     C                   MOVEL     'I'           D15TUP                         *INTERROGAZ.
139800960205     C                   MOVEL     'P'           D15EPA
139900960205     C                   MOVEL     FVARTP        D15RTC
140000960206     C                   EXSR      VISDRA
140100960206     C*
140200960206     C                   CLEAR                   VIDDPA
140300951221     C                   GOTO      FOR02
140400960206    2C                   ENDIF
140500960206     C*
140600960220     C****
140700960206     C* VISUALIZZO DESCRIZIONE RITARDO/ANTICIPO PER L' ARRIVO
140800960220     C****
140900960206    2C   17VIDDAR        IFEQ      'V'
141000960206     C                   CLEAR                   DSLV15
141100960221     C* SE PGM RICHIAMATO DA MENU' VADO IN MANUTENZIONE DELLA DESCRIZ.
141200960221     C     *IN20         IFEQ      *OFF
141300960221     C  N11              MOVEL     'M'           D15TUP                         *VARIAZIONE
141400960221     C   11              MOVEL     'I'           D15TUP                         *INTERROGAZ.
141500960221     C                   ELSE
141600960221     C                   MOVEL     'I'           D15TUP                         *INTERROGAZ.
141700960221     C                   ENDIF
141800960221     C*
141900960206     C                   MOVEL     'A'           D15EPA
142000960206     C                   MOVEL     FVARTA        D15RTC
142100960206     C                   MOVEL     FVALAI        D15LAI
142200960305     C                   MOVEL     FVAATR        D15ATR
142300960206     C                   EXSR      VISDRA
142400960220     C                   MOVEL     KPJBU         DSLV15
142500960206     C*
142600960206     C                   CLEAR                   VIDDAR
142700960206     C                   GOTO      FOR02
142800960206    2C                   ENDIF
142900960220     C*
143000960220     C****
143100960221     C* SE PGM RICHIAMATO DA MENU' CON ENTER RIEMETTO IL FORMATO
143200960220     C****
143300960221     C  N20
143400960221     CANNKF
143500960221     CANNKL              GOTO      FOR02
143600960220     C*
143700960220     C****
143800960220     C* AGGIORNO E SFLAGGO IL RECORD SOLO SE E' STATA MODIFICATA LA
143900960220     C*  CAUSALE RITARDO ARRIVO OPPURE SE MODIFICATA LA DESCRIZ.RITARDO
144000960220     C****
144100960221    2C  N20*INKF         IFEQ      *ON
144200960220     C*
144300960220    3C     SAVRTA        IFNE      FVARTA
144400960220     C     D15FLG        ORNE      ' '
144500960220     C                   CLEAR                   FVAFTR
144600960220     C                   CLEAR                   FVADTR
144700960220     C                   UPDATE    FNFVA01
144800960221     C* WTRASM --> USATO PER SAPERE CHE IL RECORD E' DA TRASMETTERE
144900960221     C                   MOVEL     '1'           WTRASM            1
145000960220    3C                   ENDIF
145100960220     C*
145200960220   X2C                   ELSE
145300960220     C* D15FLG >< " " --> E' STATA VARIATA LA DESCRIZIONE RITARDO
145400960220    3C     D15FLG        IFNE      ' '
145500960220     C*
145600960220     C* NON ESSENDO STATO PREMUTO F6 NON AGGIORNO LA CAUSALE; QUINDI
145700960220     C*   SE E' STATA MODIFICATA CI RIMETTO QUEL CHE C'ERA
145800960220     C     SAVRTA        IFNE      FVARTA
145900960220     C                   MOVEL     SAVRTA        FVARTA
146000960220     C                   ENDIF
146100960220     C*
146200960220     C                   CLEAR                   FVAFTR
146300960220     C                   CLEAR                   FVADTR
146400960220     C                   UPDATE    FNFVA01
146500960221     C* WTRASM --> USATO PER SAPERE CHE IL RECORD E' DA TRASMETTERE
146600960221     C                   MOVEL     '1'           WTRASM            1
146700960220    3C                   ENDIF
146800960220    2C                   ENDIF
146900960206     C*
147000960206    1C                   ENDIF
147100921125     C*
147200921125     C                   ENDSR
147300960206     C*-----------------------------------------------------***********
147400960206     C* CALL PGM VISUALIZZAZIONE DESCRIZIONE RITARDO/ANTIC. *  VISDRA *
147500960206     C*-----------------------------------------------------***********
147600960206     C     VISDRA        BEGSR
147700960206     C                   MOVEL     'L'           D15TLA
147800960206     C                   MOVEL     FVALNP        D15LNP
147900960206     C                   MOVEL     FVANFV        D15NFV
148000960206     C                   MOVEL     'R'           D15TRC
148100960206     C                   MOVEL     FVATRN        D15TRN
148200960222     C                   MOVEL     FVATTR        D15TTR
148300960206     C                   MOVEL     FVADFV        D15DFV
148400960206     C*
148500960206     C                   MOVEL     DSLV15        KPJBU
148600960206     C                   CALL      'FNLV15R'
148700960206     C                   PARM                    KPJBA
148800960206     C*
148900960206     C                   ENDSR
149000960206     C*-----------------------------------------------------***********
149100960206     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO    *  CTRDRA *
149200960206     C*-----------------------------------------------------***********
149300960206     C     CTRDRA        BEGSR
149400960206     C                   CLEAR                   W001A
149500960206     C*
149600960206     C     KFV4          SETLL     FNFV401L                               33
149700960206    1C     *IN33         IFEQ      *ON
149800960206     C     KFV4          READE     FNFV401L                               33
149900960206    2C     *IN33         DOWEQ     *OFF
150000960206    3C     FV4ATB        IFEQ      ' '
150100960206     C                   MOVEL     '1'           W001A             1
150200960206     C                   SETON                                        33
150300960206   X3C                   ELSE
150400960206     C     KFV4          READE     FNFV401L                               33
150500960206    3C                   ENDIF
150600960206    2C                   ENDDO
150700960206    1C                   ENDIF
150800960206     C                   ENDSR
150900960219     C*-----------------------------------------------------***********
151000960219     C* ROUTINE INIZIALE                                    *  *INZSR *
151100960219     C*-----------------------------------------------------***********
151200941014     C     *INZSR        BEGSR
151300941014     C*
151400960220     C     KFVA1         KLIST
151500960220     C                   KFLD                    V1CLNP
151600960220     C                   KFLD                    V1CNFV
151700980112     C                   KFLD                    WLAI
151800941014     C     KFVA2         KLIST
151900941014     C                   KFLD                    VIDLNP
152000941014     C                   KFLD                    VIDNFV
152100941014     C                   KFLD                    VIDLAI
152200941028     C     KFVA3         KLIST
152300941028     C                   KFLD                    VI2NPG
152400941028     C                   KFLD                    VI2NFV
152500941028     C                   KFLD                    WLAI
152600941028     C     KFVA4         KLIST
152700941028     C                   KFLD                    WLNP
152800941028     C                   KFLD                    PA1DSD
152900941028     C     KFVA5         KLIST
153000941028     C                   KFLD                    VI2NPG
153100941028     C                   KFLD                    VI2NFV
153200941028     C                   KFLD                    WLAI
153300941028     C                   KFLD                    PA1LNP
153400960220     C     KFVA6         KLIST
153500960220     C                   KFLD                    V1CFGS
153600960220     C                   KFLD                    WDAD
153700980112     C     KFVA7         KLIST
153800980112     C                   KFLD                    V1CLNP
153900980112     C                   KFLD                    V1CNFV
154000980112     C     KTAB          KLIST
154100951218     C                   KFLD                    CODUT
154200951218     C                   KFLD                    COD
154300951218     C                   KFLD                    KEY
154400960402     C     KARB          KLIST
154500960402     C                   KFLD                    FVALNP                                     FOGLIO
154600960402     C                   KFLD                    FVANFV
154700951221     C     KFV4          KLIST
154800951221     C                   KFLD                    FVALNP                                     FOGLIO
154900951221     C                   KFLD                    FVANFV
155000951221     C                   KFLD                    WEPA
155100960205     C                   KFLD                    ZERO              3 0                      NE
155200951221     C                   KFLD                    WTRC
155300000612     C*
155400000612     C     KFWA1         KLIST
155500000612     C                   KFLD                    FVALNP
155600000612     C                   KFLD                    FVANFV
155700000612     C                   KFLD                    FVALAI
155800941028     C**
155900941028     C* DEFINIZIONE CAMPI
156000941028     C**
156100980112     C     *LIKE         DEFINE    D55TFA        LAITFA
156200980112     C     *LIKE         DEFINE    PA1LAI        WLAI
156300941028     C     *LIKE         DEFINE    FVALNP        WLNP
156400951218     C     *LIKE         DEFINE    TBLCOD        COD
156500951218     C     *LIKE         DEFINE    TBLKEY        KEY
156600951221     C     *LIKE         DEFINE    FV4EPA        WEPA
156700951221     C     *LIKE         DEFINE    FV4TRC        WTRC
156800960219     C     *LIKE         DEFINE    D50FGS        WFIL
156900960220     C     *LIKE         DEFINE    V1CDFD        WDFD
157000960220     C     *LIKE         DEFINE    V1CDFA        WDFA
157100960220     C     *LIKE         DEFINE    V1CDAD        WDAD
157200960220     C     *LIKE         DEFINE    V1CDAA        WDAA
157300960220     C     *LIKE         DEFINE    FVARTA        SAVRTA
157400951221     C*
157500951221     C****  IMPOSTAZIONE CAMPI FISSI  ****
157600951221     C                   MOVEL     'R'           WTRC
157700941014     C                   ENDSR
157800951221     C*---------------------------------------------------------------*
157900960220** 4567890123456789012345678901234
158000941014*  ABBINAMENTO  FOGLI  VIAGGIO   *
158100941014* INTERROGAZ. FV PART.ALL'ARRIVO *
158200960219**  MSG
158300960219Linea di partenza inesistente                                                 1
158400980204Impossibile selezionare piu'di 70 fogli: riesegure la ricerca per i mancanti
158500960219Occorre immettere il numero f.viaggio o la data f.viaggio o la data f.arrivi  3
158600960220Immettere o il numero f.viaggio o una data                                    4
158700960219Se immesso il numero foglio viaggio occorre inserire la linea di partenza     5
158800960219Numero foglio viaggio inesistente                                             6
158900960219Data errata                                                                   7
159000960219La data foglio DAL non puo' essere maggiore della data foglio AL              8
159100960219Se immessa data f.arrivi impossibile richiedere i f.viaggio ancora da abbinare9
159200960220Non esistono dati da visualizzare per la selezione effettuata !!              10
159300960220Immettere causale valida                                                      11
