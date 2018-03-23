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
106700090907     C                   CLEAR                   VIDhTA
106800090907     C                   CLEAR                   VIDaed
106900090907     C                   CLEAR                   VIDaeh
107000960206     C                   CLEAR                   VIDDTP
107100090907     C                   CLEAR                   VIDhTP
107200090907     C                   CLEAR                   VIDDRP
107300090907     C                   CLEAR                   VIDhRP
107400960206     C                   CLEAR                   VIDDTN
107500960206     C                   CLEAR                   DESTTR
107600960216     C                   CLEAR                   DESRA1
107700960216     C                   CLEAR                   DESRP1
107800960216     C                   CLEAR                   DESRA2
107900960216     C                   CLEAR                   DESRP2
108000960206     C                   CLEAR                   VIDDPA
108100960206     C                   CLEAR                   VIDDAR
108200010703     C                   CLEAR                   VIDCNC
108300010703     C                   CLEAR                   VIDCAM
108400010703     C                   CLEAR                   VIDPIO
108500010704     C                   CLEAR                   DESTMZ
108600010704     C*
108700010704     C                   SETOFF                                       121418
108800010704     C                   SETOFF                                       19
108900941220     C*
109000960205     C* DATA REALE ARRIVO
109100941220     C                   Z-ADD     FVAAED        G02INV
109200941220     C                   Z-ADD     *ZEROS        G02DAT
109300941220     C                   MOVEL     3             G02ERR
109400941220     C                   CALL      'XSRDA8'
109500941220     C                   PARM                    WLBDAT
109600941220     C                   Z-ADD     G02DAT        VIDAED
109700090907     c* Ora teorica
109800090907     c                   if        fvaaed>0
109900090907     c                   movel     fvaaeh        w004a
110000090907     c                   eval      vidaeh=%subst(w004a:1:2)+':'+
110100090907     c                                    %subst(w004a:3:2)
110200090907     c                   endif
110300960205     C* DATA REALE PARTENZA
110400960205     C                   Z-ADD     FVADRP        G02INV
110500960205     C                   Z-ADD     *ZEROS        G02DAT
110600960205     C                   MOVEL     3             G02ERR
110700960205     C                   CALL      'XSRDA8'
110800960205     C                   PARM                    WLBDAT
110900960205     C                   Z-ADD     G02DAT        VIDDRP
111000090907     c* Ora teorica
111100090907     c                   if        fvadrp>0
111200090907     c                   movel     fvahrp        w004a
111300090907     c                   eval      vidhrp=%subst(w004a:1:2)+':'+
111400090907     c                                    %subst(w004a:3:2)
111500090907     c                   endif
111600090907     c
111700960205     C* DATA TEORICA ARRIVO
111800960206    2C     FVADTA        IFGT      0
111900951218     C                   Z-ADD     FVADTA        G02INV
112000951218     C                   Z-ADD     *ZEROS        G02DAT
112100951218     C                   MOVEL     3             G02ERR
112200951218     C                   CALL      'XSRDA8'
112300951218     C                   PARM                    WLBDAT
112400951218     C                   Z-ADD     G02DAT        VIDDTA
112500090907     c* Ora teorica
112600090907     c                   movel     fvahta        w004a
112700090907     c                   eval      vidhta=%subst(w004a:1:2)+':'+
112800090907     c                                    %subst(w004a:3:2)
112900960206    2C                   ENDIF
113000960205     C* DATA TEORICA PARTENZA
113100960206    2C     FVADTP        IFGT      0
113200960205     C                   Z-ADD     FVADTP        G02INV
113300960205     C                   Z-ADD     *ZEROS        G02DAT
113400960205     C                   MOVEL     3             G02ERR
113500960205     C                   CALL      'XSRDA8'
113600960205     C                   PARM                    WLBDAT
113700960205     C                   Z-ADD     G02DAT        VIDDTP
113800090907     c* Ora teorica
113900090907     c                   movel     fvahtp        w004a
114000090907     c                   eval      vidhtp=%subst(w004a:1:2)+':'+
114100090907     c                                    %subst(w004a:3:2)
114200960206    2C                   ENDIF
114300090907     c*
114400951218     C* DATA TRAINO
114500960206    2C     FVADTN        IFGT      0
114600951218     C                   Z-ADD     FVADTN        G02INV
114700951218     C                   Z-ADD     *ZEROS        G02DAT
114800951218     C                   MOVEL     3             G02ERR
114900951218     C                   CALL      'XSRDA8'
115000951218     C                   PARM                    WLBDAT
115100951218     C                   Z-ADD     G02DAT        VIDDTN
115200960206    2C                   ENDIF
115300960206     C* DECODIFICA TIPO TRAINO
115400960206    2C     FVATTR        IFNE      ' '
115500960206     C                   MOVEL     'TV'          COD
115600960206     C                   MOVEL(P)  FVATTR        KEY
115700960206     C     KTAB          CHAIN     TABEL                              32
115800960206     C  N32              MOVEL     TBLUNI        DESTTR
115900960206    2C                   ENDIF
116000960206     C*
116100960205     C* DECODIFICA CAUSALE RITARDO ARRIVO
116200960206    2C     FVARTA        IFNE      ' '
116300960220     C                   SETON                                        13
116400951218     C                   MOVEL     'TC'          COD
116500951218     C                   MOVEL(P)  FVARTA        KEY
116600951218     C     KTAB          CHAIN     TABEL                              32
116700960206    3C     *IN32         IFEQ      *OFF
116800951218     C     TBLFLG        ANDEQ     ' '
116900960216     C                   MOVEL     TBLUNI        DSTC
117000960216     C                   MOVEL     §TCDES        DESRA1
117100960216     C                   MOVE      §TCDES        DESRA2
117200960206    3C                   ENDIF
117300960206     C*
117400960206     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
117500960206     C                   MOVEL     'A'           WEPA
117600960206     C                   MOVEL     FVALAI        ZERO
117700960206     C                   EXSR      CTRDRA
117800960206     C* 17 ON  - ESISTE DESCRIZIONE RITARDO/ANTICIPO PER LA PARTENZA
117900960206    3C     W001A         IFNE      ' '
118000960206     C                   SETON                                        17
118100960206    3C                   ENDIF
118200960206    2C                   ENDIF
118300960205     C*
118400960205     C* DECODIFICA CAUSALE RITARDO PARTENZA
118500960206    2C     FVARTP        IFNE      ' '
118600960205     C                   MOVEL     'TC'          COD
118700960205     C                   MOVEL(P)  FVARTP        KEY
118800960205     C     KTAB          CHAIN     TABEL                              32
118900960206    3C     *IN32         IFEQ      *OFF
119000960205     C     TBLFLG        ANDEQ     ' '
119100960216     C                   MOVEL     TBLUNI        DSTC
119200960216     C                   MOVEL     §TCDES        DESRP1
119300960216     C                   MOVE      §TCDES        DESRP2
119400960206    3C                   ENDIF
119500951221     C*
119600951221     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO
119700960206     C                   MOVEL     'P'           WEPA
119800960206     C                   CLEAR                   ZERO
119900960206     C                   EXSR      CTRDRA
120000960206     C* 16 ON  - ESISTE DESCRIZIONE RITARDO/ANTICIPO PER L'ARRIVO
120100960206    3C     W001A         IFNE      ' '
120200960206     C                   SETON                                        16
120300960206    3C                   ENDIF
120400960206    2C                   ENDIF
120500921125     C*
120600921125     C* FILIALE DI INVIO
120700941017     C     FVALAI        CHAIN     AZORG01L                           33
120800921125     C  N33              MOVEL     ORGDES        DE2LAI
120900941014     C   33              MOVEL     *BLANKS       DE2LAI
121000921125     C*
121100921125     C* FILIALE ARRIVO FINALE
121200941013     C     FVALNA        CHAIN     AZORG01L                           33
121300921125     C  N33              MOVEL     ORGDES        DESLNA
121400921125     C   33              MOVEL     *BLANKS       DESLNA
121500960206     C*
121600921125     C* 01 ON - METTO IN R.I.
121700960206    2C     FVAATR        IFEQ      'T'
121800921125     C                   SETON                                        01
121900960206   X2C                   ELSE
122000921125     C                   SETOFF                                       01
122100960206    2C                   END
122200000428     C* ESTENSIONE FOGLI
122300000428     C     KFVA2         CHAIN     FNFWA01L                           33
122400000428     C     *IN33         IFEQ      *ON
122500000428     C     FWAATB        ORNE      ' '
122600000428     C                   CLEAR                   FWAFF3
122700000428     C                   CLEAR                   FWAFF4
122800000428     C                   CLEAR                   FWAFL3
122900000428     C                   CLEAR                   FWAFL4
123000000428     C                   ENDIF
123100921125     C*
123200010704     C*REPERISCO IL NUMERO CELLULARE DEL CONDUCENTE
123300010704     C                   MOVEL     'P'           WEPA
123400010704     C                   CLEAR                   ZERO
123500010704     C                   MOVE      'L'           WTRC
123600010704     C     KFV4          CHAIN     FNFV401L                           30
123700010704     C     *IN30         IFEQ      *OFF
123800010704     C                   MOVEL     FV4NOT        VIDCNC
123900010704     C                   ENDIF
124000010704     C     VIDCNC        IFEQ      *BLANKS
124100010704     C                   SETON                                        19
124200010704     C                   ENDIF
124300010704     C*
124400010704     C*REPERISCO I PIOMBI
124500010704     C                   Z-ADD     0             CONT              1 0
124600010704     C                   MOVEL     'P'           WEPA
124700010704     C                   CLEAR                   ZERO
124800010704     C                   MOVE      'P'           WTRC
124900010704     C     KFV4          SETLL     FNFV401L
125000010704     C                   DO        *HIVAL
125100010704     C     KFV4          READE     FNFV401L                               30
125200010704     C     *IN30         IFEQ      *ON
125300010704     C                   LEAVE
125400010704     C                   ENDIF
125500010704     C                   ADD       1             CONT
125600010704     C     CONT          IFEQ      1
125700010704     C                   MOVEL     FV4NOT        VIDPIO
125800010704     C                   ENDIF
125900010704     C     CONT          IFEQ      2
126000010704     C                   MOVEL     FV4NOT        WPIO             13
126100010704     C                   MOVE      WPIO          VIDPIO
126200010704     C                   ENDIF
126300010704     C                   ENDDO
126400010704     C     VIDPIO        IFEQ      *BLANKS
126500010704     C                   SETON                                        18
126600010704     C                   ENDIF
126700010704     C*
126800010704     C*REPERISCO TIPO AUTOMEZZO E CATEGORIA TRAINO
126900010704     C                   MOVEL     'P'           WEPA
127000010704     C                   CLEAR                   ZERO
127100010704     C                   MOVE      'V'           WTRC
127200010704     C     KFV4          CHAIN     FNFV401L                           30
127300010704     C     *IN30         IFEQ      *OFF
127400010704     C                   MOVEL     FV4NOT        DSFV4V
127500010704     C     §V4TMZ        IFEQ      *BLANKS
127600010704     C                   SETON                                        12
127700010704     C                   ELSE
127800010704     C                   MOVEL     §V4TMZ        VIDCAM
127900010704     C                   MOVEL     'TU'          COD
128000010704     C                   MOVEL(P)  VIDCAM        KEY
128100010704     C     KTAB          CHAIN     TABEL                              83
128200010704    1C  N83TBLFLG        IFEQ      ' '
128300010704     C                   MOVEL     TBLUNI        DESTMZ
128400010704    1C                   ENDIF
128500010704     C                   ENDIF
128600010704     C     §V4TTR        IFNE      'B'
128700010704     C                   SETON                                        14
128800010704     C                   ENDIF
128900010704     C                   ELSE
129000010704     C                   SETON                                        1214
129100010704     C                   ENDIF
129200010704     C*
129300960402     C* VERIFICO SE IL FOGLIO VIAGGIO HA BOLLE
129400960402     C     KARB          SETLL     FNARB06L                               02
129500921125     C*
129600921125     C                   Z-ADD     1             I                 3 0
129700960206    2C     FFV(I)        DOWGT     *BLANKS
129800921125     C*
129900921125     C                   MOVEL     FFV(I)        NUM3              3 0
130000921125     C                   MOVEL     FFV(I)        VIDFFV
130100941013     C     NUM3          CHAIN     AZORG01L                           33
130200921125     C  N33              MOVEL     ORGDES        DESFFV
130300921125     C   33              MOVEL     *BLANKS       DESFFV
130400921125     C*
130500921125     C                   MOVEL     FLP(I)        NUM3
130600921125     C                   MOVEL     FLP(I)        VIDFLP
130700941013     C     NUM3          CHAIN     AZORG01L                           31
130800921125     C  N31              MOVEL     ORGDES        DESFLP
130900921125     C   31              MOVEL     *BLANKS       DESFLP
131000921125     C*
131100941013     C                   WRITE     LR26S02
131200010703     C*
131300921125     C                   ADD       1             I
131400921125     C                   ADD       1             NRR2
131500960206    2C                   END
131600921125     C*
131700960206    2C     NRR2          IFEQ      1
131800921125     C                   SETON                                        75
131900960206   X2C                   ELSE
132000921125     C                   Z-ADD     1             NRR2
132100960206    2C                   END
132200921125     C*
132300921125     C     FOR02         TAG
132400941013     C                   WRITE     LR26T01                                      TESTATA VIDEO
132500951221     C                   WRITE     LR26Z02
132600941013     C                   EXFMT     LR26C02
132700960220     C*
132800960220     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN27)
132900960220     C                   CLEAR                   VIDMSG
133000960220     C                   SETOFF                                       2748
133100951221     C*
133200960221     C****
133300960221     C* SOLO SE PGM RICHIAMATO DA MENU' (20 OFF)
133400960221     C****
133500960221   2AC     *IN20         IFEQ      *OFF
133600960221     C****
133700960221     C* F7 - VARIAZIONE CAUSALE RITARDO ARRIVO
133800960221     C****
133900960221    2C     *INKG         IFEQ      *ON
134000960221     C                   SETOFF                                       11
134100960221     C                   GOTO      FOR02
134200960221    2C                   ENDIF
134300960221     C*
134400960220     C****
134500960220     C* CONTROLLO CAUSALE RITARDO ARRIVO
134600960220     C****
134700960220    2C     FVARTA        IFEQ      '?'
134800960220     C                   MOVEL     'TC'          §COD
134900960220     C                   MOVEL     *BLANKS       FVARTA
135000960220     C                   MOVEL     *BLANKS       §DES
135100960220     C                   CALL      'X§TABER'
135200960220     C                   PARM                    CODUT
135300960220     C                   PARM                    §COD              2
135400960220     C                   PARM                    §KEY              8
135500960220     C                   PARM                    §DES             25
135600960220     C                   MOVEL     §KEY          FVARTA
135700960220     C                   MOVEL     §DES          DESRA1
135800960220     C                   MOVE      §DES          W005A             5
135900960221     C                   MOVEL(P)  W005A         DESRA2
136000960220     C                   GOTO      FOR02
136100960220    2C                   ENDIF
136200960220     C*
136300960220    2C     FVARTA        IFNE      ' '
136400960220     C                   MOVEL     'TC'          COD
136500960220     C                   MOVEL(P)  FVARTA        KEY
136600960220     C     KTAB          CHAIN     TABEL                              34
136700960220    3C     *IN34         IFEQ      *OFF
136800960220     C     TBLFLG        ANDNE     ' '
136900960220     C     *IN34         OREQ      *ON
137000960220     C                   MOVEL     MSG(11)       VIDMSG
137100960220     C                   SETON                                        2748
137200960220     C                   GOTO      FOR02
137300960220   X3C                   ELSE
137400960220     C                   MOVEL     TBLUNI        DSTC
137500960220     C                   MOVEL     §TCDES        DESRA1
137600960220     C                   MOVE      §TCDES        DESRA2
137700960220    3C                   ENDIF
137800960220     C*
137900960220   X2C                   ELSE
138000960220     C                   CLEAR                   DESRA1
138100960220     C                   CLEAR                   DESRA2
138200960220    2C                   ENDIF
138300960221   2AC                   ENDIF
138400960220     C*
138500960220     C****
138600960206     C* VISUALIZZO DESCRIZIONE RITARDO/ANTICIPO PER LA PARTENZA
138700960220     C****
138800960206    2C   16VIDDPA        IFEQ      'V'
138900960205     C                   CLEAR                   DSLV15
139000960220     C                   MOVEL     'I'           D15TUP                         *INTERROGAZ.
139100960205     C                   MOVEL     'P'           D15EPA
139200960205     C                   MOVEL     FVARTP        D15RTC
139300960206     C                   EXSR      VISDRA
139400960206     C*
139500960206     C                   CLEAR                   VIDDPA
139600951221     C                   GOTO      FOR02
139700960206    2C                   ENDIF
139800960206     C*
139900960220     C****
140000960206     C* VISUALIZZO DESCRIZIONE RITARDO/ANTICIPO PER L' ARRIVO
140100960220     C****
140200960206    2C   17VIDDAR        IFEQ      'V'
140300960206     C                   CLEAR                   DSLV15
140400960221     C* SE PGM RICHIAMATO DA MENU' VADO IN MANUTENZIONE DELLA DESCRIZ.
140500960221     C     *IN20         IFEQ      *OFF
140600960221     C  N11              MOVEL     'M'           D15TUP                         *VARIAZIONE
140700960221     C   11              MOVEL     'I'           D15TUP                         *INTERROGAZ.
140800960221     C                   ELSE
140900960221     C                   MOVEL     'I'           D15TUP                         *INTERROGAZ.
141000960221     C                   ENDIF
141100960221     C*
141200960206     C                   MOVEL     'A'           D15EPA
141300960206     C                   MOVEL     FVARTA        D15RTC
141400960206     C                   MOVEL     FVALAI        D15LAI
141500960305     C                   MOVEL     FVAATR        D15ATR
141600960206     C                   EXSR      VISDRA
141700960220     C                   MOVEL     KPJBU         DSLV15
141800960206     C*
141900960206     C                   CLEAR                   VIDDAR
142000960206     C                   GOTO      FOR02
142100960206    2C                   ENDIF
142200960220     C*
142300960220     C****
142400960221     C* SE PGM RICHIAMATO DA MENU' CON ENTER RIEMETTO IL FORMATO
142500960220     C****
142600960221     C  N20
142700960221     CANNKF
142800960221     CANNKL              GOTO      FOR02
142900960220     C*
143000960220     C****
143100960220     C* AGGIORNO E SFLAGGO IL RECORD SOLO SE E' STATA MODIFICATA LA
143200960220     C*  CAUSALE RITARDO ARRIVO OPPURE SE MODIFICATA LA DESCRIZ.RITARDO
143300960220     C****
143400960221    2C  N20*INKF         IFEQ      *ON
143500960220     C*
143600960220    3C     SAVRTA        IFNE      FVARTA
143700960220     C     D15FLG        ORNE      ' '
143800960220     C                   CLEAR                   FVAFTR
143900960220     C                   CLEAR                   FVADTR
144000960220     C                   UPDATE    FNFVA01
144100960221     C* WTRASM --> USATO PER SAPERE CHE IL RECORD E' DA TRASMETTERE
144200960221     C                   MOVEL     '1'           WTRASM            1
144300960220    3C                   ENDIF
144400960220     C*
144500960220   X2C                   ELSE
144600960220     C* D15FLG >< " " --> E' STATA VARIATA LA DESCRIZIONE RITARDO
144700960220    3C     D15FLG        IFNE      ' '
144800960220     C*
144900960220     C* NON ESSENDO STATO PREMUTO F6 NON AGGIORNO LA CAUSALE; QUINDI
145000960220     C*   SE E' STATA MODIFICATA CI RIMETTO QUEL CHE C'ERA
145100960220     C     SAVRTA        IFNE      FVARTA
145200960220     C                   MOVEL     SAVRTA        FVARTA
145300960220     C                   ENDIF
145400960220     C*
145500960220     C                   CLEAR                   FVAFTR
145600960220     C                   CLEAR                   FVADTR
145700960220     C                   UPDATE    FNFVA01
145800960221     C* WTRASM --> USATO PER SAPERE CHE IL RECORD E' DA TRASMETTERE
145900960221     C                   MOVEL     '1'           WTRASM            1
146000960220    3C                   ENDIF
146100960220    2C                   ENDIF
146200960206     C*
146300960206    1C                   ENDIF
146400921125     C*
146500921125     C                   ENDSR
146600960206     C*-----------------------------------------------------***********
146700960206     C* CALL PGM VISUALIZZAZIONE DESCRIZIONE RITARDO/ANTIC. *  VISDRA *
146800960206     C*-----------------------------------------------------***********
146900960206     C     VISDRA        BEGSR
147000960206     C                   MOVEL     'L'           D15TLA
147100960206     C                   MOVEL     FVALNP        D15LNP
147200960206     C                   MOVEL     FVANFV        D15NFV
147300960206     C                   MOVEL     'R'           D15TRC
147400960206     C                   MOVEL     FVATRN        D15TRN
147500960222     C                   MOVEL     FVATTR        D15TTR
147600960206     C                   MOVEL     FVADFV        D15DFV
147700960206     C*
147800960206     C                   MOVEL     DSLV15        KPJBU
147900960206     C                   CALL      'FNLV15R'
148000960206     C                   PARM                    KPJBA
148100960206     C*
148200960206     C                   ENDSR
148300960206     C*-----------------------------------------------------***********
148400960206     C* CONTROLLO SE ESISTE DESCRIZIONE RITARDO/ANTICIPO    *  CTRDRA *
148500960206     C*-----------------------------------------------------***********
148600960206     C     CTRDRA        BEGSR
148700960206     C                   CLEAR                   W001A
148800960206     C*
148900960206     C     KFV4          SETLL     FNFV401L                               33
149000960206    1C     *IN33         IFEQ      *ON
149100960206     C     KFV4          READE     FNFV401L                               33
149200960206    2C     *IN33         DOWEQ     *OFF
149300960206    3C     FV4ATB        IFEQ      ' '
149400960206     C                   MOVEL     '1'           W001A             1
149500960206     C                   SETON                                        33
149600960206   X3C                   ELSE
149700960206     C     KFV4          READE     FNFV401L                               33
149800960206    3C                   ENDIF
149900960206    2C                   ENDDO
150000960206    1C                   ENDIF
150100960206     C                   ENDSR
150200960219     C*-----------------------------------------------------***********
150300960219     C* ROUTINE INIZIALE                                    *  *INZSR *
150400960219     C*-----------------------------------------------------***********
150500941014     C     *INZSR        BEGSR
150600941014     C*
150700960220     C     KFVA1         KLIST
150800960220     C                   KFLD                    V1CLNP
150900960220     C                   KFLD                    V1CNFV
151000980112     C                   KFLD                    WLAI
151100941014     C     KFVA2         KLIST
151200941014     C                   KFLD                    VIDLNP
151300941014     C                   KFLD                    VIDNFV
151400941014     C                   KFLD                    VIDLAI
151500941028     C     KFVA3         KLIST
151600941028     C                   KFLD                    VI2NPG
151700941028     C                   KFLD                    VI2NFV
151800941028     C                   KFLD                    WLAI
151900941028     C     KFVA4         KLIST
152000941028     C                   KFLD                    WLNP
152100941028     C                   KFLD                    PA1DSD
152200941028     C     KFVA5         KLIST
152300941028     C                   KFLD                    VI2NPG
152400941028     C                   KFLD                    VI2NFV
152500941028     C                   KFLD                    WLAI
152600941028     C                   KFLD                    PA1LNP
152700960220     C     KFVA6         KLIST
152800960220     C                   KFLD                    V1CFGS
152900960220     C                   KFLD                    WDAD
153000980112     C     KFVA7         KLIST
153100980112     C                   KFLD                    V1CLNP
153200980112     C                   KFLD                    V1CNFV
153300980112     C     KTAB          KLIST
153400951218     C                   KFLD                    CODUT
153500951218     C                   KFLD                    COD
153600951218     C                   KFLD                    KEY
153700960402     C     KARB          KLIST
153800960402     C                   KFLD                    FVALNP                                     FOGLIO
153900960402     C                   KFLD                    FVANFV
154000951221     C     KFV4          KLIST
154100951221     C                   KFLD                    FVALNP                                     FOGLIO
154200951221     C                   KFLD                    FVANFV
154300951221     C                   KFLD                    WEPA
154400960205     C                   KFLD                    ZERO              3 0                      NE
154500951221     C                   KFLD                    WTRC
154600000612     C*
154700000612     C     KFWA1         KLIST
154800000612     C                   KFLD                    FVALNP
154900000612     C                   KFLD                    FVANFV
155000000612     C                   KFLD                    FVALAI
155100941028     C**
155200941028     C* DEFINIZIONE CAMPI
155300941028     C**
155400980112     C     *LIKE         DEFINE    D55TFA        LAITFA
155500980112     C     *LIKE         DEFINE    PA1LAI        WLAI
155600941028     C     *LIKE         DEFINE    FVALNP        WLNP
155700951218     C     *LIKE         DEFINE    TBLCOD        COD
155800951218     C     *LIKE         DEFINE    TBLKEY        KEY
155900951221     C     *LIKE         DEFINE    FV4EPA        WEPA
156000951221     C     *LIKE         DEFINE    FV4TRC        WTRC
156100960219     C     *LIKE         DEFINE    D50FGS        WFIL
156200960220     C     *LIKE         DEFINE    V1CDFD        WDFD
156300960220     C     *LIKE         DEFINE    V1CDFA        WDFA
156400960220     C     *LIKE         DEFINE    V1CDAD        WDAD
156500960220     C     *LIKE         DEFINE    V1CDAA        WDAA
156600960220     C     *LIKE         DEFINE    FVARTA        SAVRTA
156700951221     C*
156800951221     C****  IMPOSTAZIONE CAMPI FISSI  ****
156900951221     C                   MOVEL     'R'           WTRC
157000941014     C                   ENDSR
157100951221     C*---------------------------------------------------------------*
157200960220** 4567890123456789012345678901234
157300941014*  ABBINAMENTO  FOGLI  VIAGGIO   *
157400941014* INTERROGAZ. FV PART.ALL'ARRIVO *
157500960219**  MSG
157600960219Linea di partenza inesistente                                                 1
157700980204Impossibile selezionare piu'di 70 fogli: riesegure la ricerca per i mancanti
157800960219Occorre immettere il numero f.viaggio o la data f.viaggio o la data f.arrivi  3
157900960220Immettere o il numero f.viaggio o una data                                    4
158000960219Se immesso il numero foglio viaggio occorre inserire la linea di partenza     5
158100960219Numero foglio viaggio inesistente                                             6
158200960219Data errata                                                                   7
158300960219La data foglio DAL non puo' essere maggiore della data foglio AL              8
158400960219Se immessa data f.arrivi impossibile richiedere i f.viaggio ancora da abbinare9
158500960220Non esistono dati da visualizzare per la selezione effettuata !!              10
158600960220Immettere causale valida                                                      11
