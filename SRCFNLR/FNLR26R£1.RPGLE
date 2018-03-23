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
010700951221     D*
010800960221     D***
010900960221     D* RICHIAMO PGM TRASMISSIONE FNFVA/FNFV4          - FNLR20R -
011000960221     D***
011100960221     D PARAM2          DS
011200960221     D* PARNUM E' USATO PER RISOTTOMETTERE L'AZIONE DI TRASMISSIONE
011300960221     D  PA2NUM                 1      1  0 INZ
011400960221     D*
011500960205     D* DS PER FNLV15R - GESTIONE DESCRIZIONE CAUSALE RITARDO/ANTICIPO
011600960205     D DSLV15        E DS                  EXTNAME(FNLV15DS)
011700960219     D* DS PER FNLV50R - CONTROLLO FILIALE GESTIONE
011800960219     D DSLV50        E DS                  EXTNAME(FNLV50DS)
011900971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
012000980109     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012100980109     D  LIN                    1     90  0
012200980109     D                                     DIM(30)                              FIL. COMODO
012300960219     I*
012400960219     C*****************************************************************
012500960219     C* RIEPILOGO INDICATORI
012600960219     C*****************************************************************
012700960219     C* 01    - FOGLIO VIAGGIO IN R.I. NEL SFL
012800020424     C* 05    - SONO UN SECONDO LIVELLO
012900960219     C* 06    - CAMBIO FILIALE IN GESTIONE PER CMD18
013000960221     C* 11    - PROTEGGO CAMPO CAUSALE RITARDO ARRIVO
013100010703     C* 12    - NON VISUALIZZAZIONE TIPO AUTOMEZZO
013200960220     C* 13    - ESISTE CAUSALE RITARDO ARRIVO: ABILITO F6-AGGIORNAMENTO
013300010703     C* 14    - NON VISUALIZZAZIONE CATEG. TRAINO
013400971002     C* 15    - NON ABILITO IL RICHIAMO DELL'ABBINAMENTO PERCHE' VENGO
013500971002     C*         DA LI'
013600971002     C* 16    - ESISTE DESCRIZ. RITARDO/ANTICIPO PER LA PARTENZA
013700960219     C* 17    - ESISTE DESCRIZ. RITARDO/ANTICIPO PER L' ARRIVO
013800010703     C* 18    - NON VISUALIZZAZIONE PIOMBI
013900010703     C* 19    - NON VISUALIZZAZIONE CELLULARE
014000960221     C* 20    - ON --> PGM RICHIAMATO      OFF --> PGM DA MENU'
014100960219     C* 27    - INDICATORE DI EMISSIONE CAMPO MESSAGGI
014200960219     C* 28    - IMMESSA UNA SPECIFICA LINEA DI PARTENZA FOGLIO
014300960219     C* 29    - RICHIESTI SOLO F.V. NON ABBINATI
014400960220     C* 30/34 - DI COMODO
014500960219     C* 36    - INDICATORE DI SFLNXTCHG
014600960219     C* 37    - USATO PER RIEMETTERE IL SFL DI SELEZIONE FOGLIO
014700960220     C* 40/48 - ERRORI
014800960219     C* 72/74 - INDICATORI DI SFLCLR
014900960219     C* 73/75 - INDICATORI DI SFLDSP
015000960219     C*****************************************************************
015100000000     C     *ENTRY        PLIST
015200000000     C                   PARM                    KPJBA
015300960220     C                   PARM                    FVA
015400921119     C*
015500941013     C                   MOVEL     KPJBU         PARAM1
015600980204     C                   CLEAR                   PA1MSG
015700960219     C*
015800000000     C                   Z-ADD     1             CODUT
015900941014     C                   CALL      'X§PARUT'
016000941014     C                   PARM                    UT§DSE
016100960219     C                   MOVEL     REC80         CNCR80
016200941013     C                   MOVEL     RAGUT         RSUT
016300960219     C*---------------------------------------------------------------*
016400020424     C****  VEDO SE SONO PRIMO O SECONDO LIVELLO ***
016500020424    1C***        REM       IFEQ 'REM'
016600020424     C***        REMFIL    ANDGT*ZEROS
016700020424     C***                  MOVELREMFIL    V1CFGS
016800020424     C***                  SETON                     05
016900020424   X1C***                  ELSE
017000020424     C***                  MOVELSIMFEL    V1CFGS
017100020424    1C***                  ENDIF
017200020424     C                   Z-ADD     SIMPOU        V1CFGS
017300020424     C     SIMTPP        IFEQ      '2'
017400020424     C     SIMTPP        OREQ      *BLANKS
017500020424     C                   SETON                                        05
017600020424     C                   ENDIF
017700960220     C* TESTATA PROGRAMMA
017800951120    1C     PA1FLG        IFEQ      *BLANKS
017900951120     C                   MOVE      TES(1)        VIDTES
018000951120     C                   ELSE
018100951120     C                   MOVE      TES(2)        VIDTES
018200951120    1C                   ENDIF
018300960219     C*
018400960221     C* 11 ON  - PROTEGGO CAMPO CAUSALE RITARDO ARRIVO
018500960221     C                   SETON                                        11
018600960221     C*
018700960220     C* PA1PGM >< *BLANKS  ===> P R O G R A M M A   R I C H I A M A T O
018800960220    1C     PA1PGM        IFNE      *BLANKS
018900960221     C                   SETON                                        20
019000960220     C                   CLEAR                   V1CRTP
019100960220     C                   CLEAR                   V1CRTA
019200960220     C                   CLEAR                   WDAD
019300960220     C                   CLEAR                   WDAA
019400971002     C* VEDO SE ABILITARE F7 DI ABBINAMENTO FOGLI VIAGGIO
019500971002     C     PA1LAN        IFEQ      '5'
019600971002     C                   SETOFF                                       15
019700971002     C                   ELSE
019800971002     C                   SETON                                        15
019900971002     C                   ENDIF
020000960220     C*
020100960220   X1C                   ELSE
020200971002     C                   SETON                                        15
020300960220     C* PA1PGM =  *BLANKS  ===> P R O G R A M M A   D A   M E N U'
020400960220     C*
020500960220     C     FMTD01        TAG
020600960219     C* PULIZIA CAMPI VIDEATA
020700960219     C                   EXSR      PULD01
020800960219     C* EMISSIONE FORMATO
020900960220     C                   WRITE     LR26T01                                      TESTATA VIDEO
021000960220     C     FOR01         TAG
021100960219     C                   EXFMT     LR26D01
021200960220     c*
021300960220     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN27)
021400960220     C                   CLEAR                   V1CMSG
021500960220     C                   SETOFF                                       404142
021600960220     C                   SETOFF                                       434445
021700960220     C                   SETOFF                                       464727
021800980223     C                   MOVE      *ZEROS        PA1LAI
021900960219     C*
022000960219     C** CMD3 - FINE LAVORO
022100960219     C   KC              GOTO      FINE
022200960219     C** CMD18- CAMBIO FILIALE
022300960219     C     *INKS         IFEQ      *ON
022400960220     C                   SETON                                        0640
022500960219     C                   GOTO      FOR01
022600960219     C                   ENDIF
022700960219     C*
022800960219     C* 06 ON  - CONTROLLO FILIALE IN GESTIONE
022900960219    2C     *IN06         IFEQ      *ON
023000960219     C                   MOVEL     V1CFGS        WFIL
023100960219     C                   EXSR      CTRFGS
023200960219     C   27              GOTO      FOR01
023300960219    2C                   ENDIF
023400960219     C*
023500960219     C* CONTROLLI
023600960219     C                   EXSR      CTRD01
023700960219     C   27              GOTO      FOR01
023800960219     C*
023900960219     C* SE E' STATO IMMESSO UNO SPECIFICO FOGLIO VIAGGIO VISUALIZZO
024000960219     C*   SOLO QUELLO
024100960219     C                   Z-ADD     V1CNFV        FVA(1)
024200960219     C                   MOVEL     V1CFGS        PA1LAI
024300960219     C                   MOVEL     V1CABB        PA1ABB
024400960219     C                   MOVEL     WDFD          PA1DSD
024500960219     C                   MOVEL     WDFA          PA1DSA
024600960219     C                   MOVEL     V1CLNP        PA1LNP
024700960219     C                   MOVEL     SIMFEL        PA1FEL
024800960219    1C                   ENDIF
024900960219     C*
025000951120     C*
025100951121     C* SE E' STATO PASSATO UN FOGLIO VIAGGIO NELLA SCHIERA FVA EMETTO
025200951121     C*   DIRETTAMENTE IL SUBFILE CON SOLO QUEL FOGLIO VIAGGIO
025300951121    1C     FVA(1)        IFGT      0
025400951121     C                   EXSR      CARSFL
025500951120     C*
025600951121   X1C                   ELSE
025700951120     C*
025800941028     C* OPERAZIONI INIZIALI
025900941028     C                   EXSR      INIZIO
026000941027     C* CARICO I FOGLI
026100910423     C                   EXSR      PULIZ
026200951121    1C                   ENDIF
026300911009     C*
026400941014     C* SE IL SUBFILE E' VUOTO ESCO DAL PROGRAMMA
026500960220    1C     NRR1          IFEQ      1
026600960220     C* SE PGM DA MENU' EMETTO MESSAGGIO PER AVVISARE CHE NON C'E'
026700960220     C*   NULLA DA VISUALIZZARE
026800960220    2C     PA1PGM        IFEQ      *BLANKS
026900960220     C                   MOVEL     MSG(10)       V1CMSG
027000960220     C                   SETON                                        4227
027100960220     C                   GOTO      FOR01
027200960220     C*
027300960220   X2C                   ELSE
027400960220     C* SE PGM RICHIAMATO VADO A FINE PGM
027500911010     C                   GOTO      FINE
027600960220    2C                   ENDIF
027700960220    1C                   ENDIF
027800941014     C*
027900941014     C* EMETTO IL SUBFILE DI SCELTA FOGLIO VIAGGIO
028000941013     C                   Z-ADD     1             NRR1
028100941028     C*
028200900515     C     FMT00         TAG
028300941013     C                   WRITE     LR26T01                                      TESTATA VIDEO
028400941013     C                   WRITE     LR26Z01
028500920225     C                   SETOFF                                       37
028600941013     C                   EXFMT     LR26C01
028700961010     C* F12 - RITORNO
028800960220     C* SE PGM RICHIAMATO VADO A FINE ALTRIMENTI RIEMETTO LA 1° VIDEATA
028900960220     C   KLPA1PGM        IFNE      *BLANKS
029000960220     C                   GOTO      FINE
029100960220     C                   ELSE
029200960220     C                   GOTO      FMTD01
029300960220     C                   ENDIF
029400961010     C**
029500961010     C* F7- ABBINAMENTO FOGLI VIAGGIO
029600961010     C     *INKG         IFEQ      *ON
029700971002     C                   MOVEL     '6'           KPJBU
029800961010     C                   CALL      'FNLR25R'
029900961010     C                   PARM                    KPJBA
030000961010     C                   GOTO      FMT00
030100961010     C                   ENDIF
030200961010     C* F14- MANUTENZIONE FOGLI VARI
030300961010     C     *INKN         IFEQ      *ON
030400961010     C                   CALL      'FNLR01R'
030500961010     C                   PARM                    KPJBA
030600961010     C                   GOTO      FMT00
030700961010     C                   ENDIF
030800911009     C*
030900920225     C* ENTER - CERCO LE SPUNTE DA VISUALIZZARE
031000941013     C                   Z-ADD     1             NRR1
031100941013     C                   READC     LR26S01                                30
031200911009     C*
031300941028    1C     *IN30         DOWEQ     '0'
031400941027     C*
031500941028    2C     VIDSCE        IFEQ      '1'
031600920225     C                   SETON                                        36
031700941028    2C                   END
031800921125     C* VISUALIZZA SPUNTE
031900941028    2C     VIDSCE        IFEQ      'S'
032000941028     C                   MOVEL     ' '           VIDSCE
032100920225     C                   SETON                                        37
032200921120     C*
032300941028     C* SE SONO PA1FEL VEDO TUTTE LE SPUNTE
032400921120     C* SE SONO SECONDARIA VEDO SOLO LE MIE SPUNTE
032500941220    3C****       PA1FEL    IFEQ PA1LAI
032600941220     C****                 Z-ADD0         VIDLAF
032700941220     C****                 ELSE
032800941220     C****                 Z-ADDPA1LAI    VIDLAF
032900941220    3C****                 END
033000921120     C*
033100941220     C                   CLEAR                   PARA04
033200941220     C                   Z-ADD     VIDLNP        P04FGS                         FIL.GESTIONE
033300941220     C                   Z-ADD     1             P04NPG                         CATEGORIA
033400941220     C                   MOVEL     VIDNFV        P04NFV                         NR.FOGLIO
033500941220     C                   Z-ADD     99            P04NRS
033600941220     C                   Z-ADD     99            P04NPS
033700941220     C                   MOVEL     '1'           P04FLG                         '1'=INTERROGAZIONE
033800941220     C                   MOVEL     'FNLR26R '    P04PGM                         RICHIAMATO
033900941220     C                   MOVEL     PARA04        KPJBU
034000941220     C                   CALL      'FNLR04R'
034100920225     C                   PARM                    KPJBA
034200941028    2C                   ENDIF
034300920225     C*
034400911009     C*
034500921125     C* VISUALIZZA FOGLIO
034600941028    2C     VIDSCE        IFEQ      '5'
034700921125     C                   SETON                                        37
034800921125     C                   EXSR      VISUAL
034900941028     C                   MOVEL     ' '           VIDSCE
035000941028    2C                   ENDIF
035100921125     C*
035200921125     C* 01 ON - METTO IN R.I.
035300941028    2C     VIDATR        IFEQ      'T'
035400921125     C                   SETON                                        01
035500921125     C                   ELSE
035600921125     C                   SETOFF                                       01
035700941028    2C                   END
035800941027     C**
035900941013     C                   UPDATE    LR26S01
036000941027     C                   SETOFF                                       36
036100941027     C**
036200941013     C                   READC     LR26S01                                30
036300941028    1C                   END
036400920225     C*
036500920225     C   37              GOTO      FMT00
036600900515      *
036700920225     C*       - CERCO I FV DA PASSARE
036800980204     C                   Z-ADD     0             I
036900941013     C                   READC     LR26S01                                30
037000920225     C*
037100980204     C     *IN30         DOWEQ     *OFF
037200980204     C* SELEZIONE
037300920225     C     VIDSCE        IFEQ      '1'
037400980204     C                   ADD       1             I
037500980204     C**
037600980204     C     I             IFLE      70
037700920225     C                   Z-ADD     VIDNFV        FVA(I)
037800920225     C                   MOVEL     VIDLNP        FVA(I)
037900980204     C                   ELSE
038000980204     C                   MOVEL     MSG(2)        PA1MSG
038100980204     C                   SETON                                        30
038200980204     C                   ENDIF
038300980204     C                   ENDIF
038400920225     C*
038500980204     C  N30              READC     LR26S01                                30
038600980204     C                   ENDDO
038700980204     C*
038800920225     C*
038900960221     C* SE PROGRAMMA RICHIAMATO DA MENU' CON ENTER RIEMETTO LA VIDEATA
039000960221     C     PA1PGM        IFEQ      *BLANKS
039100960221     C                   GOTO      FMT00
039200960221     C                   ENDIF
039300960221     C*
039400920327     C     FINE          TAG
039500960221     C*
039600960221     C* SOTTOMETTO AZIONE DI TRASMISSIONE FNFVA00F/FNFV400F IN SEDE E
039700960221     C*   ALLA FILIALE DI PARTENZA SOLO SE SONO STATI VARIATI
039800960221     C     WTRASM        IFNE      ' '
039900960221     C                   MOVEL     PARAM2        KPJBU
040000960221     C                   MOVEL     'LR20'        KCOAZ
040100960221     C                   CALL      'BCH10'
040200960221     C                   PARM                    KPJBA
040300960221     C                   ENDIF
040400971128     C*
040500971128     C                   MOVEL     'C'           D55TLA
040600971128     C                   CALL      'FNLV55R'
040700971128     C                   PARM                    DSLV55
040800960221     C*
040900941013     C                   MOVEL     PARAM1        KPJBU
041000000000     C                   SETON                                        LR
041100960219     C*-----------------------------------------------------***********
041200960219     C* PULIZIA CAMPI VIDEATA SELEZIONI                     *  PULD01 *
041300960219     C*-----------------------------------------------------***********
041400960219     C     PULD01        BEGSR
041500960219     C*
041600960219     C*
041700960220     C                   CLEAR                   FVA
041800960219     C                   CLEAR                   V1CLNP
041900960219     C                   CLEAR                   V1CNFV
042000960219     C                   CLEAR                   V1CDFD
042100960219     C                   CLEAR                   V1CDFA
042200960219     C                   CLEAR                   V1CDAD
042300960219     C                   CLEAR                   V1CDAA
042400960219     C                   CLEAR                   V1CABB
042500960220     C                   CLEAR                   V1CRTP
042600960220     C                   CLEAR                   V1CRTA
042700960220     C                   CLEAR                   WDFD
042800960220     C                   CLEAR                   WDFA
042900960220     C                   CLEAR                   WDAD
043000960220     C                   CLEAR                   WDAA
043100960219     C                   ENDSR
043200960219     C*
043300960219     C*-----------------------------------------------------***********
043400960219     C* CONTROLLO LA FILIALE IN GESTIONE                    *  CTRFGS *
043500960219     C*-----------------------------------------------------***********
043600960219     C     CTRFGS        BEGSR
043700960219     C                   CLEAR                   DSLV50
043800960219     C                   MOVEL     KNMUS         D50PRU
043900960219     C                   MOVEL     WFIL          D50FGS
044000960219     C                   CALL      'FNLV50R'
044100960219     C                   PARM                    DSLV50
044200960219     C*
044300960219    1C     D50ERR        IFNE      ' '
044400960219     C                   MOVEL     D50MSG        V1CMSG
044500960219     C                   SETON                                        4027
044600960219     C                   GOTO      ENDCTF
044700960219    1C                   ENDIF
044800960219     C*
044900960219     C     ENDCTF        ENDSR
045000960219     C*
045100960219     C*-----------------------------------------------------***********
045200960219     C* CONTROLLO CAMPI VIDEATA SELEZIONI                   *  CTRD01 *
045300960219     C*-----------------------------------------------------***********
045400960219     C     CTRD01        BEGSR
045500960220     C                   SETOFF                                       06
045600960219     C*
045700960219     C****  LINEA DI PARTENZA  ****
045800960220    1C     V1CLNP        IFNE      0
045900960219     C     V1CLNP        CHAIN     AZORG                              31
046000960220    2C     *IN31         IFEQ      *ON
046100960219     C                   MOVEL     MSG(1)        V1CMSG
046200960219     C                   SETON                                        4127
046300960219     C                   GOTO      ENDCTR
046400960220    2C                   ENDIF
046500960220    1C                   ENDIF
046600960219     C*
046700960219     C****  OCCORRE IMMETTERE O IL NUMERO FOGLIO O UNA DATA  ****
046800960219    1C     V1CNFV        IFEQ      0
046900960219     C     V1CDFD        ANDEQ     0
047000960219     C     V1CDAD        ANDEQ     0
047100960219     C                   MOVEL     MSG(3)        V1CMSG
047200960219     C                   SETON                                        4227
047300960219     C                   GOTO      ENDCTR
047400960219    1C                   ENDIF
047500960219     C*
047600960219     C****  IMPOSSIBILE INSERIRE TUTTI E TRE  ****
047700960219    1C     V1CNFV        IFNE      0
047800960220     C     V1CDFD        IFNE      0
047900960220     C     V1CDAD        ORNE      0
048000960220     C                   MOVEL     MSG(4)        V1CMSG
048100960220     C                   SETON                                        4227
048200960220     C                   GOTO      ENDCTR
048300960220     C                   ENDIF
048400960219    1C                   ENDIF
048500960219     C*
048600960219     C****  NUMERO FOLGIO VIAGGIO  ****
048700960219    1C     V1CNFV        IFNE      0
048800960219     C* SE IMMESSO IL FOGLIO VIAGGIO OCCORRE INSERIRE ANCHE LA LNP
048900960219    2C     V1CLNP        IFEQ      0
049000960219     C                   MOVEL     MSG(5)        V1CMSG
049100960219     C                   SETON                                        4127
049200960219     C                   GOTO      ENDCTR
049300960219    2C                   ENDIF
049400960219     C*
049500960219     C* CONTROLLO VALIDITA' NUMERO FOGLIO VIAGGIO
049600980112     C* CARICO LA L6
049700980112     C                   CLEAR                   DSUL06
049800980112     C                   MOVE      '£6'          D06COD
049900980112     C                   MOVEL     V1CFGS        D06KEY
050000980112     C                   MOVEL     'L'           D06TLA
050100980112     C                   MOVEL     DSUL06        KPJBU
050200980112     C*
050300980112     C                   CALL      'TRUL06R'
050400980112     C                   PARM                    KPJBA
050500980112     C                   MOVEL     KPJBU         DSUL06
050600980112     C                   MOVEA     LIN           L6
050700980112     C**
050800980112     C* CONTROLLO SE ESISTE IL FOGLIO PER UNA DELLA FIL DI L6
050900980112     C                   Z-ADD     1             XX
051000980112     C                   CLEAR                   WTROV
051100980112     C     L6(XX)        DOWGT     0
051200980112     C                   Z-ADD     L6(XX)        WLAI
051300960220     C     KFVA1         CHAIN(N)  FNFVA01L                           31
051400980112    3C     *IN31         IFEQ      *ON
051500980112     C                   ADD       1             XX
051600980112     C                   ELSE
051700980112     C                   Z-ADD     30            XX
051800980112     C                   MOVEL     'S'           WTROV             1
051900980112    3C                   ENDIF
052000980112    2C                   ENDDO
052100980112     C** NON ANCORA TROVATO
052200980112    2C     WTROV         IFEQ      ' '
052300980112     C* CERCO PER IL TERMINAL DI ARRIVO DI LAI SE NON C'E' GIA' IL L6
052400980112     C     KFVA7         CHAIN(N)  FNFVA01L                           31
052500980112    3C     *IN31         IFEQ      *OFF
052600980112     C                   CLEAR                   DSLV55
052700980112     C                   MOVEL     'A'           D55TPT
052800980223     C     PA1LAI        IFGT      *ZEROS
052900980223     C                   MOVE      PA1LAI        D55LIN
053000980223     C                   ELSE
053100980223     C                   MOVE      V1CFGS        D55LIN
053200980223     C                   END
053300980112     C                   Z-ADD     FVADFV        D55DRF
053400980112     C                   CALL      'FNLV55R'
053500980112     C                   PARM                    DSLV55
053600980112     C     D55TFA        LOOKUP    L6                                     32
053700980112    4C     *IN32         IFEQ      *OFF
053800980112     C                   Z-ADD     D55TFA        WLAI
053900980112     C     KFVA7         CHAIN(N)  FNFVA01L                           31
054000980112     C  N31              MOVEL     'S'           WTROV
054100980112    4C                   ENDIF
054200980112    3C                   ENDIF
054300980112    2C                   ENDIF
054400980112     C**
054500980112    2C     WTROV         IFEQ      ' '
054600960219     C                   MOVEL     MSG(6)        V1CMSG
054700960219     C                   SETON                                        4227
054800960219     C                   GOTO      ENDCTR
054900960219    2C                   ENDIF
055000960219    1C                   ENDIF
055100960219     C*
055200960219     C****  DATA FOGLIO VIAGGIO  DAL  ****
055300960219    1C     V1CDFD        IFNE      0
055400960219     C                   MOVEL     V1CDFD        G02DAT
055500960219     C                   MOVEL     *ZERO         G02INV
055600960219     C                   MOVEL     *BLANKS       G02ERR
055700960219     C                   CALL      'XSRDA8'
055800960219     C                   PARM                    WLBDAT
055900960219     C     G02ERR        IFEQ      '1'
056000960219     C                   MOVEL     MSG(7)        V1CMSG
056100960219     C                   SETON                                        4327
056200960219     C                   GOTO      ENDCTR
056300960219     C                   ENDIF
056400960219     C                   Z-ADD     G02DAT        V1CDFD
056500960219     C                   Z-ADD     G02INV        WDFD
056600960219    1C                   ENDIF
056700960219     C*
056800960219     C****  DATA FOGLIO VIAGGIO  AL  ****
056900960219    1C     V1CDFA        IFNE      0
057000960219     C                   MOVEL     V1CDFA        G02DAT
057100960219     C                   MOVEL     *ZERO         G02INV
057200960219     C                   MOVEL     *BLANKS       G02ERR
057300960219     C                   CALL      'XSRDA8'
057400960219     C                   PARM                    WLBDAT
057500960219     C     G02ERR        IFEQ      '1'
057600960219     C                   MOVEL     MSG(7)        V1CMSG
057700960219     C                   SETON                                        4427
057800960219     C                   GOTO      ENDCTR
057900960219     C                   ENDIF
058000960219     C                   Z-ADD     G02DAT        V1CDFA
058100960219     C                   Z-ADD     G02INV        WDFA
058200960219     C*
058300960219     C* DATA DAL NO MAGGIORE DATA AL
058400960219    2C     WDFD          IFGT      WDFA
058500960219     C                   MOVEL     MSG(8)        V1CMSG
058600960219     C                   SETON                                        4327
058700960219     C                   GOTO      ENDCTR
058800960219    2C                   ENDIF
058900960219    1C                   ENDIF
059000960219     C*
059100960219     C****  DATA FOGLIO ARRIVI   DAL  ****
059200960219    1C     V1CDAD        IFNE      0
059300960219     C                   MOVEL     V1CDAD        G02DAT
059400960219     C                   MOVEL     *ZERO         G02INV
059500960219     C                   MOVEL     *BLANKS       G02ERR
059600960219     C                   CALL      'XSRDA8'
059700960219     C                   PARM                    WLBDAT
059800960219     C     G02ERR        IFEQ      '1'
059900960219     C                   MOVEL     MSG(7)        V1CMSG
060000960219     C                   SETON                                        4527
060100960219     C                   GOTO      ENDCTR
060200960219     C                   ENDIF
060300960219     C                   Z-ADD     G02DAT        V1CDAD
060400960219     C                   Z-ADD     G02INV        WDAD
060500960219    1C                   ENDIF
060600960219     C*
060700960219     C****  DATA FOGLIO ARRIVI   AL  ****
060800960219    1C     V1CDAA        IFNE      0
060900960219     C                   MOVEL     V1CDAA        G02DAT
061000960219     C                   MOVEL     *ZERO         G02INV
061100960219     C                   MOVEL     *BLANKS       G02ERR
061200960219     C                   CALL      'XSRDA8'
061300960219     C                   PARM                    WLBDAT
061400960219     C     G02ERR        IFEQ      '1'
061500960219     C                   MOVEL     MSG(7)        V1CMSG
061600960219     C                   SETON                                        4627
061700960219     C                   GOTO      ENDCTR
061800960219     C                   ENDIF
061900960219     C                   Z-ADD     G02DAT        V1CDAA
062000960219     C                   Z-ADD     G02INV        WDAA
062100960219     C*
062200960219     C* DATA DAL NO MAGGIORE DATA AL
062300960219    2C     WDAD          IFGT      WDAA
062400960219     C                   MOVEL     MSG(8)        V1CMSG
062500960219     C                   SETON                                        4527
062600960219     C                   GOTO      ENDCTR
062700960219    2C                   ENDIF
062800960219    1C                   ENDIF
062900960219     C*
063000960219     C****  RICHIESTA FOGLIO VIAGGIO NON ABBINATI  ****
063100960220     C* SE IMMMESSA LA DATA FOGLIO ARRIVI E' IMPOSSIBILE RICHIEDERE I
063200960220     C*   FOGLI VIAGGIO ANCORA DA ABBINARE
063300960219     C     V1CABB        IFEQ      'N'
063400960219     C     V1CDAD        ANDNE     0
063500960219     C                   MOVEL     MSG(9)        V1CMSG
063600960219     C                   SETON                                        4727
063700960219     C                   GOTO      ENDCTR
063800960219     C                   ENDIF
063900960219     C*
064000960219     C     ENDCTR        ENDSR
064100960220     C*
064200960219     C*-----------------------------------------------------***********
064300960219     C* OPERAZIONI INIZIALI                                 * INIZIO  *
064400960219     C*-----------------------------------------------------***********
064500941028     C     INIZIO        BEGSR
064600941028     C*
064700941028     C* SE IL CODICE FILIALE CUI INVIO E' VUOTO, LO DETERMINO
064800941028    1C     PA1LAI        IFEQ      *ZERO
064900941028     C*
065000020424    2C***        REM       IFEQ 'REM'                      DAL NOME UTENTE
065100020424     C***                  MOVELREMFIL    PA1LAI
065200020424   X2C***                  ELSE
065300020424     C***                  Z-ADDPA1FEL    PA1LAI
065400020424    2C***                  ENDIF
065500020424     C     SIMTPP        IFEQ      '2'
065600020424     C     SIMTPP        OREQ      *BLANKS
065700020424     C                   Z-ADD     SIMPOU        PA1LAI
065800020424     C                   ELSE
065900020424     C                   Z-ADD     PA1FEL        PA1LAI
066000020424     C                   ENDIF
066100941028    1C                   ENDIF
066200980112     C* CARICO LA L6
066300980112     C                   CLEAR                   DSUL06
066400980112     C                   MOVE      '£6'          D06COD
066500980112     C                   MOVEL     PA1LAI        D06KEY
066600980112     C                   MOVEL     'L'           D06TLA
066700980112     C                   MOVEL     DSUL06        KPJBU
066800980112     C*
066900980112     C                   CALL      'TRUL06R'
067000980112     C                   PARM                    KPJBA
067100980112     C                   MOVEL     KPJBU         DSUL06
067200980112     C                   MOVEA     LIN           L6
067300941028     C*
067400980112     C* MEMORIZZO IL TERMINAL ARRIVO DI LAI PER VEDERE SE CARICARE ANCH
067500980112     C*  E I SUOI FOGLI VIAGGIO
067600971127     C                   CLEAR                   DSLV55
067700971127     C                   MOVEL     'A'           D55TPT
067800971127     C                   MOVE      PA1LAI        D55LIN
067900971128     C     WDFD          IFGT      *ZEROS
068000971128     C                   Z-ADD     WDFD          D55DRF
068100971128     C                   ELSE
068200971128     C                   Z-ADD     WDAD          D55DRF
068300971128     C                   END
068400971127     C                   CALL      'FNLV55R'
068500971127     C                   PARM                    DSLV55
068600980112     C                   MOVEL     D55TFA        LAITFA
068700941028     C*
068800960219     C* 29 ON - VISUALIZZO SOLO FOGLI NON ABBINATI A FOGLIO ARRIVI
068900941028     C     PA1ABB        COMP      'N'                                    29     NON ABBINATI
069000941028     C*
069100941028     C*
069200960219     C* 28 ON - IMMESSA SPECIFICA LINEA DI PARTENZA FOGLIO
069300960219     C     PA1LNP        COMP      0                                  28
069400941028     C*
069500960220     C* SOLO SE PGM RICHIAMATO: SE DATA INIZIALE E FINALE SONO NULLE,
069600960220     C*   IMPOSTO QUELLA FINALE
069700941028     C     PA1DSD        IFEQ      *ZERO
069800941028     C     PA1DSA        ANDEQ     *ZERO
069900960220     C     PA1PGM        ANDNE     *BLANKS
070000941028     C                   Z-ADD     99991231      PA1DSA
070100941028     C                   ENDIF
070200941028     C*
070300941028     C* SE C'E' SOLO DATA DAL E NON AL : LE IMPOSTO UGUALI
070400960220     C* DATA FOGLIO VIAGGIO
070500941220     C     PA1DSD        IFNE      *ZERO
070600941220     C     PA1DSA        ANDEQ     *ZERO
070700941028     C                   Z-ADD     PA1DSD        PA1DSA
070800941028     C                   ENDIF
070900960220     C* DATA FOGLIO ARRIVI
071000960220     C     WDAD          IFNE      *ZERO
071100960220     C     WDAA          ANDEQ     *ZERO
071200960220     C                   Z-ADD     WDAD          WDAA
071300960220     C                   ENDIF
071400941028     C*
071500941028     C                   ENDSR
071600960219     C*-----------------------------------------------------***********
071700960219     C* PULIZIA CAMPI VIDEO                                 *  PULIZ  *
071800960219     C*-----------------------------------------------------***********
071900900515     C     PULIZ         BEGSR
072000960219     C*
072100911009     C                   SETON                                        72
072200941013     C                   WRITE     LR26C01
072300911009     C                   SETOFF                                       72
072400911009     C                   SETON                                        73
072500911009     C*
072600980112     C                   CLEAR                   FVC
072700911009     C                   MOVEA     *ZEROS        FVA
072800941013     C                   Z-ADD     1             NRR1              5 0
072900911009     C                   Z-ADD     0             VI2NFV            5 0
073000930519     C                   MOVEL     ' '           VI2NPG            1 0
073100941027     C*
073200921119     C* DECODIFICA LINEA ARRIVO FOGLI
073300941014     C     PA1LAI        CHAIN     AZORG01L                           30
073400921119     C  N30              MOVEL     ORGDES        DESLAI
073500980112     C**
073600980112     C* CARICO TUTTI I FV DELLA L6 COMPRESI QUELLI DEL TERMINAL DI
073700980112     C*  ARRIVO PER RICHIAMAO DA ABBINAMENTO
073800980112     C                   CLEAR                   LEL
073900980112     C                   MOVEA     L6            LEL
074000980112     C     LAITFA        IFNE      PA1LAI
074100980112     C     LAITFA        LOOKUP    L6                                     32
074200980112     C     *IN32         IFEQ      *OFF
074300980112     C                   Z-ADD     1             XX
074400980112     C     000           LOOKUP    LEL(XX)                                30
074500980112     C                   Z-ADD     LAITFA        LEL(XX)
074600980112     C                   ENDIF
074700980112     C                   ENDIF
074800941028     C**
074900980112     C* RICHIESTI SOLO FV DA ABBINARE
075000941028     C**
075100941028    1C     *IN29         IFEQ      *ON
075200980112     C                   Z-ADD     1             XX
075300980112     C**
075400980112    2C     XX            DOWLE     30
075500980112     C     LEL(XX)       ANDGT     0
075600980112     C                   Z-ADD     LEL(XX)       WLAI
075700941028     C*
075800960402     C* NON IMMESSA UNA SPECIFICA LINEA DI PARTENZA
075900941028     C  N28KFVA3         SETLL     FNFVA02L
076000941028     C  N28KFVA3         READE     FNFVA02L                               30
076100941028     C*
076200941028     C* IMMESSA UNA SPECIFICA LINEA DI PARTEZA
076300941028     C   28KFVA5         SETLL     FNFVA02L
076400941028     C   28KFVA5         READE     FNFVA02L                               30
076500911009     C*
076600941028    3C     *IN30         DOWEQ     *OFF
076700941014     C*
076800941014     C* VERIFICO LE INCLUSIONI/ESCLUSIONI
076900941014     C                   EXSR      ESCLUS                                       ESCLUSIONI
077000911009     C*
077100941028     C  N28KFVA3         READE     FNFVA02L                               30
077200941028     C   28KFVA5         READE     FNFVA02L                               30
077300941028    3C                   ENDDO
077400941028     C*
077500980112     C                   ADD       1             XX                3 0
077600941028    2C                   ENDDO
077700941028     C*
077800941028   X1C                   ELSE
077900941028     C*
078000941028     C* RICHIESTI TUTTI I FOGLI O SOLO GLI ABBINATI: LEGGO PER DATA
078100941028     C                   Z-ADD     PA1LNP        WLNP
078200941028     C*
078300960220     C* LETTURA PER   D A T A   F O G L I O   V I A G G I O
078400960220     C     PA1DSD        IFNE      0
078500960220     C*
078600941028     C* NON IMMESSA UNA SPECIFICA LINEA DI PARTEZA
078700941028     C  N28*LOVAL        SETLL     FNFVA03L
078800941028     C  N28              READ      FNFVA03L                               30
078900941028     C*
079000941028     C* IMMESSA UNA SPECIFICA LINEA DI PARTENZA
079100941028     C   28KFVA4         SETLL     FNFVA03L
079200941028     C   28WLNP          READE     FNFVA03L                               30
079300960220     C*
079400960220     C                   ELSE
079500960220     C* LETTURA PER   D A T A   F O G L I O   A R R I V I
079600960220     C*
079700960220     C     KFVA6         SETLL     FNFVA05L
079800960220     C     V1CFGS        READE     FNFVA05L                               30
079900960220     C                   ENDIF
080000941028     C*
080100941028    2C     *IN30         DOWEQ     *OFF
080200941028     C*
080300941028     C                   EXSR      ESCLUS
080400941028     C*
080500960220     C* LETTURA PER   D A T A   F O G L I O   V I A G G I O
080600960220     C     PA1DSD        IFNE      0
080700960220     C*
080800941028     C* SE PER CASO DATA MAGGIORE DATA AL MI POSIZIONO SULLA LINEA SUCC
080900941028    3C     FVADFV        IFGT      PA1DSA
081000941028     C                   Z-ADD     FVALNP        WLNP
081100941028     C     WLNP          SETGT     FNFVA003
081200941028    3C                   ENDIF
081300941028     C*
081400941028     C  N28              READ      FNFVA03L                               30
081500941028     C   28WLNP          READE     FNFVA03L                               30
081600960220     C*
081700960220     C                   ELSE
081800960220     C* LETTURA PER   D A T A   F O G L I O   A R R I V I
081900960220     C*
082000960220     C     FVADFA        IFGT      WDAA
082100960220     C                   SETON                                        30
082200960220     C                   ELSE
082300960220     C     V1CFGS        READE     FNFVA05L                               30
082400960220     C                   ENDIF
082500960220     C                   ENDIF
082600960220     C*
082700941028    2C                   ENDDO
082800941028    1C                   ENDIF
082900960219     C*
083000900514     C                   ENDSR
083100951121     C*-----------------------------------------------------***********
083200951121     C* CARICO NEL SFL SOLO IL FOGLIO PASSATO               * CARSFL  *
083300951121     C*-----------------------------------------------------***********
083400951121     C     CARSFL        BEGSR
083500951121     C*
083600951121     C                   SETON                                        72
083700951121     C                   WRITE     LR26C01
083800951121     C                   SETOFF                                       72
083900951121     C                   SETON                                        73
084000951121     C*
084100960219     C                   Z-ADD     1             NRR1
084200951121     C                   MOVEL     PA1LNP        VIDLNP
084300951121     C                   Z-ADD     FVA(1)        VIDNFV
084400951121     C                   MOVEL     PA1LAI        VIDLAI
084500951121     C*
084600951121     C* DECODIFICA LINEA ARRIVO FOGLI
084700951121     C     PA1LAI        CHAIN     AZORG01L                           30
084800951121     C  N30              MOVEL     ORGDES        DESLAI
084900951121     C*
085000960220     C     KFVA2         CHAIN(N)  FNFVA01L                           30
085100951121     C*
085200951121    1C     *IN30         IFEQ      *OFF
085300960205     C                   EXSR      RIEMPI
085400960205     C*
085500951121     C                   WRITE     LR26S01
085600951121     C                   ADD       1             NRR1
085700951121    1C                   ENDIF
085800951121     C*
085900951121     C                   ENDSR
086000960205     C*-----------------------------------------------------***********
086100960205     C* RIEMPO CAMPI SFL                                    * RIEMPI  *
086200960205     C*-----------------------------------------------------***********
086300960205     C     RIEMPI        BEGSR
086400960205     C*
086500960205     C                   MOVEL     FVANFV        VIDNFV
086600960205     C* GIRO DATA FOGLIO VIAGGIO
086700960205     C                   MOVE      FVADFV        G02INV
086800960205     C                   MOVE      *ZEROS        G02DAT
086900960205     C                   MOVEL     3             G02ERR
087000960205     C                   CALL      'XSRDA8'
087100960205     C                   PARM                    WLBDAT
087200960205     C                   Z-ADD     G02DAT        VIDDFV
087300000612     C* P.O.GESTIONE
087400000612     C     KFWA1         CHAIN     FNFWA01L                           30
087500000612     C     *IN30         IFEQ      *OFF
087600000612     C     FWAFGS        ANDNE     0
087700000612     C                   Z-ADD     FWAFGS        WORKPT            3 0
087800000612     C                   ELSE
087900000612     C                   Z-ADD     FVALNP        WORKPT            3 0
088000000612     C                   ENDIF
088100000612     C                   CLEAR                   OG143
088200000612     C     WORKPT        CHAIN     AZORG                              30
088300000612     C   30              MOVE      '  '          VIDPT1
088400000612     C  N30              MOVEL     ORGDE3        OG143
088500020211     C  N30§OGNTW        IFEQ      'PPT'
088600000612     C                   MOVE      'PT'          VIDPT1
088700000612     C                   ELSE
088800000612     C                   MOVE      '  '          VIDPT1
088900000612     C                   ENDIF
089000960205     C* DECODIFICA LINEA DI PARTENZA
089100960205     C                   Z-ADD     FVALNP        VIDLNP
089200960205     C     VIDLNP        CHAIN     AZORG01L                           30
089300960205     C     *IN30         IFEQ      *OFF
089400960402     C**
089500960402     C     ORGDE5        IFNE      *BLANKS
089600960402     C                   MOVEL     ORGDE5        DESLNP
089700960402     C                   ELSE
089800960402     C                   MOVEL     ORGDES        DESLNP
089900960402     C                   ENDIF
090000960402     C**
090100960205     C                   ELSE
090200960206     C                   MOVEL     *BLANKS       DESLNP
090300960205     C                   ENDIF
090400960205     C*
090500960205     C                   MOVEL     FVANPG        VIDNPG                         CATEG.FOGLIO ARRIVI
090600960205     C                   Z-ADD     FVANFA        VIDNFA                         NR.FOGLIO ARRIVI
090700960205     C* DATA FOGLIO ARRIVI
090800960205     C     FVADFA        IFNE      *ZERO
090900960205     C                   MOVE      FVADFA        G02INV                         AAAA/MM/GG
091000960205     C                   MOVE      *ZEROS        G02DAT
091100960205     C                   MOVEL     3             G02ERR
091200960205     C                   CALL      'XSRDA8'
091300960205     C                   PARM                    WLBDAT
091400960205     C                   Z-ADD     G02DAT        VIDDFA                         GG/MM/AAAA
091500960205     C                   ELSE
091600960205     C                   Z-ADD     *ZERO         VIDDFA
091700960205     C                   ENDIF
091800960205      *
091900960205     C                   MOVEL     FVAATR        VIDATR
092000960205     C                   MOVEL     FVALAI        VIDLAI
092100960205     C* 01 ON - METTO IN R.I.
092200960205     C     FVAATR        COMP      'T'                                    01
092300960205     C*
092400960205     C* NUMERO TRAINO
092500960205     C                   MOVEL     FVATRN        VIDTRN
092600960205     C*
092700960205     C* TIPO TRAINO CON DECODIFICA
092800960205     C                   CLEAR                   VIDTTR
092900960205     C     FVATTR        IFNE      ' '
093000960205     C                   MOVEL     FVATTR        W002A             2
093100960205     C                   MOVE      '-'           W002A
093200960205     C                   MOVEL     'TV'          COD
093300960205     C                   MOVEL(P)  FVATTR        KEY
093400960205     C     KTAB          CHAIN     TABEL                              32
093500960205     C  N32              MOVEL     TBLUNI        W004A             4
093600960205     C   32              CLEAR                   W004A
093700960205     C                   MOVEL     W002A         VIDTTR
093800960205     C                   MOVE      W004A         VIDTTR
093900960205     C                   ENDIF
094000960402     C* VERIFICO SE IL FV HA BOLLE
094100960402     C     KARB          SETLL     FNARB06L                               32
094200960402     C  N32              MOVEL     '**'          VIDAST
094300960402     C   32              MOVEL     '  '          VIDAST
094400960205     C*
094500960205     C                   ENDSR
094600960219     C*-----------------------------------------------------***********
094700960219     C* ESCLUSIONI LETTURE DALL'ARCHIVIO                    *  ESCLUS *
094800960219     C*-----------------------------------------------------***********
094900941014     C     ESCLUS        BEGSR
095000941014     C*
095100941014     C                   MOVEL     'S'           WOK               1            RECORD VALIDO
095200941014     C*
095300941028     C* SOLO FOGLI ABBINATI
095400941014     C     PA1ABB        IFEQ      'A'                                          'A'=SOLO ABBINATI
095500941014     C     FVANFA        ANDEQ     *ZERO                                        NR.FOGLIO ARRIVI
095600941028     C* DEVE ESSERCI NR.FOGLIO ARRIVI
095700941014     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
095800941014     C                   ENDIF
095900941014     C*
096000960220     C* VERIFICO LA DATA FOGLIO VIAGGIO PARTENZA
096100960220    1C     PA1DSD        IFNE      0
096200960220    2C     FVADFV        IFLT      PA1DSD                                       DATA INIZIALE
096300941014     C     FVADFV        ORGT      PA1DSA                                       DATA FINALE
096400941014     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
096500960220    2C                   ENDIF
096600960220    1C                   ENDIF
096700960220     C*
096800960220     C* VERIFICO LA DATA FOGLIO ARRIVI
096900960220    1C     WDAD          IFNE      0
097000960220    2C     FVADFA        IFLT      WDAD                                         DATA INIZIALE
097100960220     C     FVADFA        ORGT      WDAA                                         DATA FINALE
097200960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
097300960220    2C                   ENDIF
097400960220     C*
097500960220     C* SE IMMESSA CONTROLLO LA LINEA DI PARTENZA
097600960220    2C   28FVALNP        IFNE      PA1LNP
097700960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
097800960220    2C                   ENDIF
097900960220    1C                   ENDIF
098000941014     C*
098100960220     C* SE RICHIESTO CONTROLLO IL RITARDO IN PARTENZA
098200960220    1C     V1CRTP        IFEQ      'R'
098300960220     C     FVARTP        ANDEQ     ' '
098400960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
098500960220    1C                   ENDIF
098600960220    1C     V1CRTP        IFEQ      'N'
098700960220     C     FVARTP        ANDNE     ' '
098800960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
098900960220    1C                   ENDIF
099000960220     C* SE RICHIESTO CONTROLLO IL RITARDO IN ARRIVO
099100960220    1C     V1CRTA        IFEQ      'R'
099200960220     C     FVARTA        ANDEQ     ' '
099300960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
099400960220    1C                   ENDIF
099500960220    1C     V1CRTA        IFEQ      'N'
099600960220     C     FVARTA        ANDNE     ' '
099700960220     C                   MOVEL     *BLANKS       WOK                            NOBBUONO !!
099800960220    1C                   ENDIF
099900960220     C*
100000941014     C* VERIFICO LA LINEA ARRIVO CUI INVIO
100100960220    1C     FVALAI        IFNE      PA1LAI
100200980112     C     FVALAI        LOOKUP    LEL                                    32
100300980112     C  N32              MOVEL     *BLANKS       WOK                            NOBBUONO !!
100400960220    1C                   ENDIF
100500980112     C* ESCLUDO SE LO STESSO FOGLIO L'HO GIA' CARICO PER ALTRA FIL
100600980112     C*  DI L6
100700980112     C     WOK           IFEQ      'S'
100800980112     C     FVAFVC        LOOKUP    FVC                                    32
100900980112     C   32              MOVEL     *BLANKS       WOK                            NOBBUONO !!
101000980112    1C                   ENDIF
101100980112    C**
101200960220    1C     WOK           IFEQ      'S'                                          SOLO SE VALIDO
101300960205     C                   EXSR      RIEMPI
101400960205     C*
101500941028     C                   WRITE     LR26S01
101600980112     C                   Z-ADD     NRR1          YY                3 0
101700980112     C                   Z-ADD     FVAFVC        FVC(YY)
101800980112     C**
101900980112     C                   ADD       1             NRR1
102000980112     C**
102100960220    1C                   ENDIF                                                  WOK = 'S'
102200960220     C*
102300941014     C                   ENDSR
102400960219     C*-----------------------------------------------------***********
102500960219     C* VISUALIZZA FOGLIO                                   *  VISUAL *
102600960219     C*-----------------------------------------------------***********
102700921125     C     VISUAL        BEGSR
102800960220     C                   SETOFF                                       161713
102900960221     C                   SETON                                        11
103000941014     C*
103100960206     C* PULIZIA SFL
103200921125     C                   Z-ADD     1             NRR2              5 0
103300921125     C                   SETON                                        74
103400941013     C                   WRITE     LR26C02
103500921125     C                   SETOFF                                       7475
103600921125     C*
103700960221     C* SOLO SE PGM DA MENU' VINCOLO IL RECORD
103800960221    1C     *IN20         IFEQ      *OFF
103900960627     C     KFVA2         CHAIN(N)  FNFVA01L                           32
104000960221   X1C                   ELSE
104100000428     C     KFVA2         CHAIN(N)  FNFVA01L                           32
104200960221    1C                   ENDIF
104300921125     C*
104400960206    1C     *IN32         IFEQ      '0'
104500000612     C* P.O.GESTIONE
104600000612     C     KFWA1         CHAIN     FNFWA01L                           30
104700000612     C     *IN30         IFEQ      *OFF
104800000612     C     FWAFGS        ANDNE     0
104900000612     C                   Z-ADD     FWAFGS        WORKPT            3 0
105000000612     C                   ELSE
105100000612     C                   Z-ADD     FVALNP        WORKPT            3 0
105200000612     C                   ENDIF
105300000612     C                   CLEAR                   OG143
105400000612     C     WORKPT        CHAIN     AZORG                              30
105500000612     C   30              MOVE      '  '          VIDPT2
105600000612     C  N30              MOVEL     ORGDE3        OG143
105700020211     C  N30§OGNTW        IFEQ      'PPT'
105800000612     C                   MOVE      'PT'          VIDPT2
105900000612     C                   ELSE
106000000612     C                   MOVE      '  '          VIDPT2
106100000612     C                   ENDIF
106200960220     C* SALVO CAUSALE RITARDO ARRIVO PERCHE' POTREBBE ESSERE VARIATA
106300960220     C                   MOVEL     FVARTA        SAVRTA
106400960220     C*
106500960206     C* PULIZIA CAMPI VIDEATA
106600960206     C                   CLEAR                   VIDDTA
106700960206     C                   CLEAR                   VIDDTP
106800960206     C                   CLEAR                   VIDDTN
106900960206     C                   CLEAR                   DESTTR
107000960216     C                   CLEAR                   DESRA1
107100960216     C                   CLEAR                   DESRP1
107200960216     C                   CLEAR                   DESRA2
107300960216     C                   CLEAR                   DESRP2
107400960206     C                   CLEAR                   VIDDPA
107500960206     C                   CLEAR                   VIDDAR
107600010703     C                   CLEAR                   VIDCNC
107700010703     C                   CLEAR                   VIDCAM
107800010703     C                   CLEAR                   VIDPIO
107900010704     C                   CLEAR                   DESTMZ
108000010704     C*
108100010704     C                   SETOFF                                       121418
108200010704     C                   SETOFF                                       19
108300941220     C*
108400960205     C* DATA REALE ARRIVO
108500941220     C                   Z-ADD     FVAAED        G02INV
108600941220     C                   Z-ADD     *ZEROS        G02DAT
108700941220     C                   MOVEL     3             G02ERR
108800941220     C                   CALL      'XSRDA8'
108900941220     C                   PARM                    WLBDAT
109000941220     C                   Z-ADD     G02DAT        VIDAED
109100960205     C* DATA REALE PARTENZA
109200960205     C                   Z-ADD     FVADRP        G02INV
109300960205     C                   Z-ADD     *ZEROS        G02DAT
109400960205     C                   MOVEL     3             G02ERR
109500960205     C                   CALL      'XSRDA8'
109600960205     C                   PARM                    WLBDAT
109700960205     C                   Z-ADD     G02DAT        VIDDRP
109800960205     C* DATA TEORICA ARRIVO
109900960206    2C     FVADTA        IFGT      0
110000951218     C                   Z-ADD     FVADTA        G02INV
110100951218     C                   Z-ADD     *ZEROS        G02DAT
110200951218     C                   MOVEL     3             G02ERR
110300951218     C                   CALL      'XSRDA8'
110400951218     C                   PARM                    WLBDAT
110500951218     C                   Z-ADD     G02DAT        VIDDTA
110600960206    2C                   ENDIF
110700960205     C* DATA TEORICA PARTENZA
110800960206    2C     FVADTP        IFGT      0
110900960205     C                   Z-ADD     FVADTP        G02INV
111000960205     C                   Z-ADD     *ZEROS        G02DAT
111100960205     C                   MOVEL     3             G02ERR
111200960205     C                   CALL      'XSRDA8'
111300960205     C                   PARM                    WLBDAT
111400960205     C                   Z-ADD     G02DAT        VIDDTP
111500960206    2C                   ENDIF
111600951218     C* DATA TRAINO
111700960206    2C     FVADTN        IFGT      0
111800951218     C                   Z-ADD     FVADTN        G02INV
111900951218     C                   Z-ADD     *ZEROS        G02DAT
112000951218     C                   MOVEL     3             G02ERR
112100951218     C                   CALL      'XSRDA8'
112200951218     C                   PARM                    WLBDAT
112300951218     C                   Z-ADD     G02DAT        VIDDTN
112400960206    2C                   ENDIF
112500960206     C* DECODIFICA TIPO TRAINO
112600960206    2C     FVATTR        IFNE      ' '
112700960206     C                   MOVEL     'TV'          COD
112800960206     C                   MOVEL(P)  FVATTR        KEY
112900960206     C     KTAB          CHAIN     TABEL                              32
113000960206     C  N32              MOVEL     TBLUNI        DESTTR
113100960206    2C                   ENDIF
113200960206     C*
113300960205     C* DECODIFICA CAUSALE RITARDO ARRIVO
113400960206    2C     FVARTA        IFNE      ' '
113500960220     C                   SETON                                        13
113600951218     C                   MOVEL     'TC'          COD
113700951218     C                   MOVEL(P)  FVARTA        KEY
113800951218     C     KTAB          CHAIN     TABEL                              32
113900960206    3C     *IN32         IFEQ      *OFF
114000951218     C     TBLFLG        ANDEQ     ' '
114100960216     C                   MOVEL     TBLUNI        DSTC
114200960216     C                   MOVEL     §TCDES        DESRA1
114300960216     C                   MOVE      §TCDES        DESRA2
114400960206    3C                   ENDIF
114500960206     C*
114600960206     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
114700960206     C                   MOVEL     'A'           WEPA
114800960206     C                   MOVEL     FVALAI        ZERO
114900960206     C                   EXSR      CTRDRA
115000960206     C* 17 ON  - ESISTE DESCRIZIONE RITARDO/ANTICIPO PER LA PARTENZA
115100960206    3C     W001A         IFNE      ' '
115200960206     C                   SETON                                        17
115300960206    3C                   ENDIF
115400960206    2C                   ENDIF
115500960205     C*
115600960205     C* DECODIFICA CAUSALE RITARDO PARTENZA
115700960206    2C     FVARTP        IFNE      ' '
115800960205     C                   MOVEL     'TC'          COD
115900960205     C                   MOVEL(P)  FVARTP        KEY
116000960205     C     KTAB          CHAIN     TABEL                              32
116100960206    3C     *IN32         IFEQ      *OFF
116200960205     C     TBLFLG        ANDEQ     ' '
116300960216     C                   MOVEL     TBLUNI        DSTC
116400960216     C                   MOVEL     §TCDES        DESRP1
116500960216     C                   MOVE      §TCDES        DESRP2
116600960206    3C                   ENDIF
116700951221     C*
116800951221     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
116900960206     C                   MOVEL     'P'           WEPA
117000960206     C                   CLEAR                   ZERO
117100960206     C                   EXSR      CTRDRA
117200960206     C* 16 ON  - ESISTE DESCRIZIONE RITARDO/ANTICIPO PER L'ARRIVO
117300960206    3C     W001A         IFNE      ' '
117400960206     C                   SETON                                        16
117500960206    3C                   ENDIF
117600960206    2C                   ENDIF
117700921125     C*
117800921125     C* FILIALE DI INVIO
117900941017     C     FVALAI        CHAIN     AZORG01L                           33
118000921125     C  N33              MOVEL     ORGDES        DE2LAI
118100941014     C   33              MOVEL     *BLANKS       DE2LAI
118200921125     C*
118300921125     C* FILIALE ARRIVO FINALE
118400941013     C     FVALNA        CHAIN     AZORG01L                           33
118500921125     C  N33              MOVEL     ORGDES        DESLNA
118600921125     C   33              MOVEL     *BLANKS       DESLNA
118700960206     C*
118800921125     C* 01 ON - METTO IN R.I.
118900960206    2C     FVAATR        IFEQ      'T'
119000921125     C                   SETON                                        01
119100960206   X2C                   ELSE
119200921125     C                   SETOFF                                       01
119300960206    2C                   END
119400000428     C* ESTENSIONE FOGLI
119500000428     C     KFVA2         CHAIN     FNFWA01L                           33
119600000428     C     *IN33         IFEQ      *ON
119700000428     C     FWAATB        ORNE      ' '
119800000428     C                   CLEAR                   FWAFF3
119900000428     C                   CLEAR                   FWAFF4
120000000428     C                   CLEAR                   FWAFL3
120100000428     C                   CLEAR                   FWAFL4
120200000428     C                   ENDIF
120300921125     C*
120400010704     C*REPERISCO IL NUMERO CELLULARE DEL CONDUCENTE
120500010704     C                   MOVEL     'P'           WEPA
120600010704     C                   CLEAR                   ZERO
120700010704     C                   MOVE      'L'           WTRC
120800010704     C     KFV4          CHAIN     FNFV401L                           30
120900010704     C     *IN30         IFEQ      *OFF
121000010704     C                   MOVEL     FV4NOT        VIDCNC
121100010704     C                   ENDIF
121200010704     C     VIDCNC        IFEQ      *BLANKS
121300010704     C                   SETON                                        19
121400010704     C                   ENDIF
121500010704     C*
121600010704     C*REPERISCO I PIOMBI
121700010704     C                   Z-ADD     0             CONT              1 0
121800010704     C                   MOVEL     'P'           WEPA
121900010704     C                   CLEAR                   ZERO
122000010704     C                   MOVE      'P'           WTRC
122100010704     C     KFV4          SETLL     FNFV401L
122200010704     C                   DO        *HIVAL
122300010704     C     KFV4          READE     FNFV401L                               30
122400010704     C     *IN30         IFEQ      *ON
122500010704     C                   LEAVE
122600010704     C                   ENDIF
122700010704     C                   ADD       1             CONT
122800010704     C     CONT          IFEQ      1
122900010704     C                   MOVEL     FV4NOT        VIDPIO
123000010704     C                   ENDIF
123100010704     C     CONT          IFEQ      2
123200010704     C                   MOVEL     FV4NOT        WPIO             13
123300010704     C                   MOVE      WPIO          VIDPIO
123400010704     C                   ENDIF
123500010704     C                   ENDDO
123600010704     C     VIDPIO        IFEQ      *BLANKS
123700010704     C                   SETON                                        18
123800010704     C                   ENDIF
123900010704     C*
124000010704     C*REPERISCO TIPO AUTOMEZZO E CATEGORIA TRAINO
124100010704     C                   MOVEL     'P'           WEPA
124200010704     C                   CLEAR                   ZERO
124300010704     C                   MOVE      'V'           WTRC
124400010704     C     KFV4          CHAIN     FNFV401L                           30
124500010704     C     *IN30         IFEQ      *OFF
124600010704     C                   MOVEL     FV4NOT        DSFV4V
124700010704     C     §V4TMZ        IFEQ      *BLANKS
124800010704     C                   SETON                                        12
124900010704     C                   ELSE
125000010704     C                   MOVEL     §V4TMZ        VIDCAM
125100010704     C                   MOVEL     'TU'          COD
125200010704     C                   MOVEL(P)  VIDCAM        KEY
125300010704     C     KTAB          CHAIN     TABEL                              83
125400010704    1C  N83TBLFLG        IFEQ      ' '
125500010704     C                   MOVEL     TBLUNI        DESTMZ
125600010704    1C                   ENDIF
125700010704     C                   ENDIF
125800010704     C     §V4TTR        IFNE      'B'
125900010704     C                   SETON                                        14
126000010704     C                   ENDIF
126100010704     C                   ELSE
126200010704     C                   SETON                                        1214
126300010704     C                   ENDIF
126400010704     C*
126500960402     C* VERIFICO SE IL FOGLIO VIAGGIO HA BOLLE
126600960402     C     KARB          SETLL     FNARB06L                               02
126700921125     C*
126800921125     C                   Z-ADD     1             I                 3 0
126900960206    2C     FFV(I)        DOWGT     *BLANKS
127000921125     C*
127100921125     C                   MOVEL     FFV(I)        NUM3              3 0
127200921125     C                   MOVEL     FFV(I)        VIDFFV
127300941013     C     NUM3          CHAIN     AZORG01L                           33
127400921125     C  N33              MOVEL     ORGDES        DESFFV
127500921125     C   33              MOVEL     *BLANKS       DESFFV
127600921125     C*
127700921125     C                   MOVEL     FLP(I)        NUM3
127800921125     C                   MOVEL     FLP(I)        VIDFLP
127900941013     C     NUM3          CHAIN     AZORG01L                           31
128000921125     C  N31              MOVEL     ORGDES        DESFLP
128100921125     C   31              MOVEL     *BLANKS       DESFLP
128200921125     C*
128300941013     C                   WRITE     LR26S02
128400010703     C*
128500921125     C                   ADD       1             I
128600921125     C                   ADD       1             NRR2
128700960206    2C                   END
128800921125     C*
128900960206    2C     NRR2          IFEQ      1
129000921125     C                   SETON                                        75
129100960206   X2C                   ELSE
129200921125     C                   Z-ADD     1             NRR2
129300960206    2C                   END
129400921125     C*
129500921125     C     FOR02         TAG
129600941013     C                   WRITE     LR26T01                                      TESTATA VIDEO
129700951221     C                   WRITE     LR26Z02
129800941013     C                   EXFMT     LR26C02
129900960220     C*
130000960220     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN27)
130100960220     C                   CLEAR                   VIDMSG
130200960220     C                   SETOFF                                       2748
130300951221     C*
130400960221     C****
130500960221     C* SOLO SE PGM RICHIAMATO DA MENU' (20 OFF)
130600960221     C****
130700960221   2AC     *IN20         IFEQ      *OFF
130800960221     C****
130900960221     C* F7 - VARIAZIONE CAUSALE RITARDO ARRIVO
131000960221     C****
131100960221    2C     *INKG         IFEQ      *ON
131200960221     C                   SETOFF                                       11
131300960221     C                   GOTO      FOR02
131400960221    2C                   ENDIF
131500960221     C*
131600960220     C****
131700960220     C* CONTROLLO CAUSALE RITARDO ARRIVO
131800960220     C****
131900960220    2C     FVARTA        IFEQ      '?'
132000960220     C                   MOVEL     'TC'          §COD
132100960220     C                   MOVEL     *BLANKS       FVARTA
132200960220     C                   MOVEL     *BLANKS       §DES
132300960220     C                   CALL      'X§TABER'
132400960220     C                   PARM                    CODUT
132500960220     C                   PARM                    §COD              2
132600960220     C                   PARM                    §KEY              8
132700960220     C                   PARM                    §DES             25
132800960220     C                   MOVEL     §KEY          FVARTA
132900960220     C                   MOVEL     §DES          DESRA1
133000960220     C                   MOVE      §DES          W005A             5
133100960221     C                   MOVEL(P)  W005A         DESRA2
133200960220     C                   GOTO      FOR02
133300960220    2C                   ENDIF
133400960220     C*
133500960220    2C     FVARTA        IFNE      ' '
133600960220     C                   MOVEL     'TC'          COD
133700960220     C                   MOVEL(P)  FVARTA        KEY
133800960220     C     KTAB          CHAIN     TABEL                              34
133900960220    3C     *IN34         IFEQ      *OFF
134000960220     C     TBLFLG        ANDNE     ' '
134100960220     C     *IN34         OREQ      *ON
134200960220     C                   MOVEL     MSG(11)       VIDMSG
134300960220     C                   SETON                                        2748
134400960220     C                   GOTO      FOR02
134500960220   X3C                   ELSE
134600960220     C                   MOVEL     TBLUNI        DSTC
134700960220     C                   MOVEL     §TCDES        DESRA1
134800960220     C                   MOVE      §TCDES        DESRA2
134900960220    3C                   ENDIF
135000960220     C*
135100960220   X2C                   ELSE
135200960220     C                   CLEAR                   DESRA1
135300960220     C                   CLEAR                   DESRA2
135400960220    2C                   ENDIF
135500960221   2AC                   ENDIF
135600960220     C*
135700960220     C****
135800960206     C* VISUALIZZO DESCRIZIONE RITARDO/ANTICIPO PER LA PARTENZA
135900960220     C****
136000960206    2C   16VIDDPA        IFEQ      'V'
136100960205     C                   CLEAR                   DSLV15
136200960220     C                   MOVEL     'I'           D15TUP                         *INTERROGAZ.
136300960205     C                   MOVEL     'P'           D15EPA
136400960205     C                   MOVEL     FVARTP        D15RTC
136500960206     C                   EXSR      VISDRA
136600960206     C*
136700960206     C                   CLEAR                   VIDDPA
136800951221     C                   GOTO      FOR02
136900960206    2C                   ENDIF
137000960206     C*
137100960220     C****
137200960206     C* VISUALIZZO DESCRIZIONE RITARDO/ANTICIPO PER L' ARRIVO
137300960220     C****
137400960206    2C   17VIDDAR        IFEQ      'V'
137500960206     C                   CLEAR                   DSLV15
137600960221     C* SE PGM RICHIAMATO DA MENU' VADO IN MANUTENZIONE DELLA DESCRIZ.
137700960221     C     *IN20         IFEQ      *OFF
137800960221     C  N11              MOVEL     'M'           D15TUP                         *VARIAZIONE
137900960221     C   11              MOVEL     'I'           D15TUP                         *INTERROGAZ.
138000960221     C                   ELSE
138100960221     C                   MOVEL     'I'           D15TUP                         *INTERROGAZ.
138200960221     C                   ENDIF
138300960221     C*
138400960206     C                   MOVEL     'A'           D15EPA
138500960206     C                   MOVEL     FVARTA        D15RTC
138600960206     C                   MOVEL     FVALAI        D15LAI
138700960305     C                   MOVEL     FVAATR        D15ATR
138800960206     C                   EXSR      VISDRA
138900960220     C                   MOVEL     KPJBU         DSLV15
139000960206     C*
139100960206     C                   CLEAR                   VIDDAR
139200960206     C                   GOTO      FOR02
139300960206    2C                   ENDIF
139400960220     C*
139500960220     C****
139600960221     C* SE PGM RICHIAMATO DA MENU' CON ENTER RIEMETTO IL FORMATO
139700960220     C****
139800960221     C  N20
139900960221     CANNKF
140000960221     CANNKL              GOTO      FOR02
140100960220     C*
140200960220     C****
140300960220     C* AGGIORNO E SFLAGGO IL RECORD SOLO SE E' STATA MODIFICATA LA
140400960220     C*  CAUSALE RITARDO ARRIVO OPPURE SE MODIFICATA LA DESCRIZ.RITARDO
140500960220     C****
140600960221    2C  N20*INKF         IFEQ      *ON
140700960220     C*
140800960220    3C     SAVRTA        IFNE      FVARTA
140900960220     C     D15FLG        ORNE      ' '
141000960220     C                   CLEAR                   FVAFTR
141100960220     C                   CLEAR                   FVADTR
141200960220     C                   UPDATE    FNFVA01
141300960221     C* WTRASM --> USATO PER SAPERE CHE IL RECORD E' DA TRASMETTERE
141400960221     C                   MOVEL     '1'           WTRASM            1
141500960220    3C                   ENDIF
141600960220     C*
141700960220   X2C                   ELSE
141800960220     C* D15FLG >< " " --> E' STATA VARIATA LA DESCRIZIONE RITARDO
141900960220    3C     D15FLG        IFNE      ' '
142000960220     C*
142100960220     C* NON ESSENDO STATO PREMUTO F6 NON AGGIORNO LA CAUSALE; QUINDI
142200960220     C*   SE E' STATA MODIFICATA CI RIMETTO QUEL CHE C'ERA
142300960220     C     SAVRTA        IFNE      FVARTA
142400960220     C                   MOVEL     SAVRTA        FVARTA
142500960220     C                   ENDIF
142600960220     C*
142700960220     C                   CLEAR                   FVAFTR
142800960220     C                   CLEAR                   FVADTR
142900960220     C                   UPDATE    FNFVA01
143000960221     C* WTRASM --> USATO PER SAPERE CHE IL RECORD E' DA TRASMETTERE
143100960221     C                   MOVEL     '1'           WTRASM            1
143200960220    3C                   ENDIF
143300960220    2C                   ENDIF
143400960206     C*
143500960206    1C                   ENDIF
143600921125     C*
143700921125     C                   ENDSR
143800960206     C*-----------------------------------------------------***********
143900960206     C* CALL PGM VISUALIZZAZIONE DESCRIZIONE RITARDO/ANTIC. *  VISDRA *
144000960206     C*-----------------------------------------------------***********
144100960206     C     VISDRA        BEGSR
144200960206     C                   MOVEL     'L'           D15TLA
144300960206     C                   MOVEL     FVALNP        D15LNP
144400960206     C                   MOVEL     FVANFV        D15NFV
144500960206     C                   MOVEL     'R'           D15TRC
144600960206     C                   MOVEL     FVATRN        D15TRN
144700960222     C                   MOVEL     FVATTR        D15TTR
144800960206     C                   MOVEL     FVADFV        D15DFV
144900960206     C*
145000960206     C                   MOVEL     DSLV15        KPJBU
145100960206     C                   CALL      'FNLV15R'
145200960206     C                   PARM                    KPJBA
145300960206     C*
145400960206     C                   ENDSR
145500960206     C*-----------------------------------------------------***********
145600960206     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO    *  CTRDRA *
145700960206     C*-----------------------------------------------------***********
145800960206     C     CTRDRA        BEGSR
145900960206     C                   CLEAR                   W001A
146000960206     C*
146100960206     C     KFV4          SETLL     FNFV401L                               33
146200960206    1C     *IN33         IFEQ      *ON
146300960206     C     KFV4          READE     FNFV401L                               33
146400960206    2C     *IN33         DOWEQ     *OFF
146500960206    3C     FV4ATB        IFEQ      ' '
146600960206     C                   MOVEL     '1'           W001A             1
146700960206     C                   SETON                                        33
146800960206   X3C                   ELSE
146900960206     C     KFV4          READE     FNFV401L                               33
147000960206    3C                   ENDIF
147100960206    2C                   ENDDO
147200960206    1C                   ENDIF
147300960206     C                   ENDSR
147400960219     C*-----------------------------------------------------***********
147500960219     C* ROUTINE INIZIALE                                    *  *INZSR *
147600960219     C*-----------------------------------------------------***********
147700941014     C     *INZSR        BEGSR
147800941014     C*
147900960220     C     KFVA1         KLIST
148000960220     C                   KFLD                    V1CLNP
148100960220     C                   KFLD                    V1CNFV
148200980112     C                   KFLD                    WLAI
148300941014     C     KFVA2         KLIST
148400941014     C                   KFLD                    VIDLNP
148500941014     C                   KFLD                    VIDNFV
148600941014     C                   KFLD                    VIDLAI
148700941028     C     KFVA3         KLIST
148800941028     C                   KFLD                    VI2NPG
148900941028     C                   KFLD                    VI2NFV
149000941028     C                   KFLD                    WLAI
149100941028     C     KFVA4         KLIST
149200941028     C                   KFLD                    WLNP
149300941028     C                   KFLD                    PA1DSD
149400941028     C     KFVA5         KLIST
149500941028     C                   KFLD                    VI2NPG
149600941028     C                   KFLD                    VI2NFV
149700941028     C                   KFLD                    WLAI
149800941028     C                   KFLD                    PA1LNP
149900960220     C     KFVA6         KLIST
150000960220     C                   KFLD                    V1CFGS
150100960220     C                   KFLD                    WDAD
150200980112     C     KFVA7         KLIST
150300980112     C                   KFLD                    V1CLNP
150400980112     C                   KFLD                    V1CNFV
150500980112     C     KTAB          KLIST
150600951218     C                   KFLD                    CODUT
150700951218     C                   KFLD                    COD
150800951218     C                   KFLD                    KEY
150900960402     C     KARB          KLIST
151000960402     C                   KFLD                    FVALNP                                     FOGLIO
151100960402     C                   KFLD                    FVANFV
151200951221     C     KFV4          KLIST
151300951221     C                   KFLD                    FVALNP                                     FOGLIO
151400951221     C                   KFLD                    FVANFV
151500951221     C                   KFLD                    WEPA
151600960205     C                   KFLD                    ZERO              3 0                      NE
151700951221     C                   KFLD                    WTRC
151800000612     C*
151900000612     C     KFWA1         KLIST
152000000612     C                   KFLD                    FVALNP
152100000612     C                   KFLD                    FVANFV
152200000612     C                   KFLD                    FVALAI
152300941028     C**
152400941028     C* DEFINIZIONE CAMPI
152500941028     C**
152600980112     C     *LIKE         DEFINE    D55TFA        LAITFA
152700980112     C     *LIKE         DEFINE    PA1LAI        WLAI
152800941028     C     *LIKE         DEFINE    FVALNP        WLNP
152900951218     C     *LIKE         DEFINE    TBLCOD        COD
153000951218     C     *LIKE         DEFINE    TBLKEY        KEY
153100951221     C     *LIKE         DEFINE    FV4EPA        WEPA
153200951221     C     *LIKE         DEFINE    FV4TRC        WTRC
153300960219     C     *LIKE         DEFINE    D50FGS        WFIL
153400960220     C     *LIKE         DEFINE    V1CDFD        WDFD
153500960220     C     *LIKE         DEFINE    V1CDFA        WDFA
153600960220     C     *LIKE         DEFINE    V1CDAD        WDAD
153700960220     C     *LIKE         DEFINE    V1CDAA        WDAA
153800960220     C     *LIKE         DEFINE    FVARTA        SAVRTA
153900951221     C*
154000951221     C****  IMPOSTAZIONE CAMPI FISSI  ****
154100951221     C                   MOVEL     'R'           WTRC
154200941014     C                   ENDSR
154300951221     C*---------------------------------------------------------------*
154400960220** 4567890123456789012345678901234
154500941014*  ABBINAMENTO  FOGLI  VIAGGIO   *
154600941014* INTERROGAZ. FV PART.ALL'ARRIVO *
154700960219**  MSG
154800960219Linea di partenza inesistente                                                 1
154900980204Impossibile selezionare piu'di 70 fogli: riesegure la ricerca per i mancanti
155000960219Occorre immettere il numero f.viaggio o la data f.viaggio o la data f.arrivi  3
155100960220Immettere o il numero f.viaggio o una data                                    4
155200960219Se immesso il numero foglio viaggio occorre inserire la linea di partenza     5
155300960219Numero foglio viaggio inesistente                                             6
155400960219Data errata                                                                   7
155500960219La data foglio DAL non puo' essere maggiore della data foglio AL              8
155600960219Se immessa data f.arrivi impossibile richiedere i f.viaggio ancora da abbinare9
155700960220Non esistono dati da visualizzare per la selezione effettuata !!              10
155800960220Immettere causale valida                                                      11
