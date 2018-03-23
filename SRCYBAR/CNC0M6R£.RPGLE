000100040719     H DECEDIT('0,') DATEDIT(*DMY.)
000200930720     H* CNC0M6R *-----------------------------------------------------*
000300930720     H*-----------------*                                             *
000400930720     H*        - MANUTENZIONE ANAGRAFICA SPEDIZIONIERI/LUOGHI -       *
000500041201
000600041201
000700041201      *****************************************************************
000800041201      * A T T E N Z I O N E:
000900041201      * --------------------
001000041201      * LA GESTIONE DEI LUOGHI È STATA TOLTA DA QUESTO PGM E GESTITA
001100041201      * NEL NUOVO PGM TNTA80R
001200041201      *****************************************************************
001300041201
001400041201
001500041201
001600930720     H*---------------------------------------------------------------*
001700930720     H* FILE   : -------- - -------------------------------------------
001800970804     H*          FNSPE01L - ANAGRAFICA SPEDIZIONIERI/LUOGHI
001900930727     H*          CNACO00F - ANAGRAFICO CLIENTI P.D.C.
002000930727     H*          AZIS001L - CAPPARIO
002100930720     H*          TABEL00F - TABELLE GENERICHE
002200961111     H*          AZORG01L - ORGANIGRAMMA FILIALI
002300930720     H*          CNC0M6D  - FILE VIDEO
002400930720     H* FMT    : -------- - -------------------------------------------
002500930720     H*          C006D1   - RICHIESTA TIPO DI MANUTENZIONE
002600930721     H*          C006D3   - RICHIESTA CODICE  SPEDIZIONIERE
002700930721     H*          C006D4   - RICHIESTA DATI    SPEDIZIONIERE/LUOGO
002800930721     H*          C006D5   - RICHIESTA CODICE  LUOGO
002900930720     H* PGM    : -------- - -------------------------------------------
003000930720     H*          CNC0M1R  - RICERCA ALFABETICA SPEDIZIONIERI
003100930720     H*          CNC0M2R  - RICERCA LUOGHI DI DESTINAZIONE
003200930727     H*          TRSE62R  - RICERCA LOCALITA
003300930720     H*          X§TABER  - RICERCA GENERICA SUL FILE TABELLE
003400930720     H*          X§PARUT  - RICERCA PARAMETRI UTENTE
003500930727     H*          XPARIVA  - CONTROLLO PARTITA IVA
003600930720     H* SBR    : -------- - -------------------------------------------
003700940901     H*          *INZSR   - DEFINIZIONE VARIABILI KLIST E PLIST
003800930722     H*          CONTR1   - CONTROLLO DATI PRIMO   FORMATO VIDEO
003900930721     H*          CONTR2   - CONTROLLO DATI SECONDO FORMATO VIDEO
004000930722     H*          CONTR3   - CONTROLLO DATI TERZO   FORMATO VIDEO
004100930723     H*          AGGIO    - AGGIORNAMENTO RECORD
004200930723     H*          ANNUL    - ANNULLAMENTO  RECORD
004300930727     H*          CAZIS0   - CONTROLLO CAP
004400930727     H*          CTRIVA   - CONTROLLO PARTITA IVA
004500930727     H*          CARTAB   - CARICAMENTO FILIALI IN GESTIONE
004600930721     H* DTAARA : -------- - -------------------------------------------
004700930721     H*          SPEDTA   - NUMERATORE NUOVI SPEDIZIONIERI
004800930720     H*---------------------------------------------------------------*
004900930720     H* INDICATORI USATI                                              *
005000930720     H*---------------------------------------------------------------*
005100930727     H* 01    - RICHIESTO PUNTO INTERROGATIVO                      SCAN
005200930727     H* 02    - SOLO VALORI NUMERICI PER CODICE SPED/LUOG         TESTN
005300961112     H* 08    - ON --> SONO IN FILIALE      OFF --> SONO IN SEDE
005400961009     H* 09    - ON PROGRAMMA RICHIAMATO
005500930727     H* 10    - ON RECORD IN INSERIMENTO  / OFF IN VARIAZIONE
005600930727     H* 12    - ON  ANAGRAFICA LUOGHI / OFF ANAGRAFICA SPEDIZIONIERI
005700930727     H* 13    - A VIDEO CONDIZIONA I CAMPI SE LUOGHI O SPEDIZIONIERI
005800940901     H* 28    - CAMPO MESSGGIO
005900930727     H* 30    - LETTURA TABELLE
006000930727     H* 31    - LETTURA CAPPARIO
006100930727     H* 32    - LETTURA ANAGRAFICO CLIENTI
006200930727     H* 33    - RICERCA IN FILIALI IN GESTIONE                    LOKUP
006300961111     H* 35    - DI COMODO
006400940901     H* 40    - ERRORE CAMPO SELEZIONE ANAGRAFICA DA MANUTENZIONARE
006500940901     H* 41    - ERRORE CAMPO CODICE LUOGO
006600940901     H* 42    - ERRORE CAMPO CODICE CLIENTE
006700940901     H* 43    - ERRORE CAMPO CODICE SPEDIZIONE
006800940901     H* 44    - ERRORE CAMPO RAGIONE SOCIALE
006900940901     H* 45    - ERRORE CAMPO INDIRIZZO
007000940901     H* 46    - ERRORE CAMPO C.A.P.
007100940901     H* 47    - ERRORE CAMPO LOCALITA'
007200940901     H* 48    - ERRORE CAMPO PROVINCIA
007300940901     H* 49    - ERRORE CAMPO IVA
007400960304     H* 50    - ERRORE NUMERO FAX
007500961112     H* 52    - ERRORE
007600040622     H* 54    - ERRORE ind. Mail
007700930727     H* 90    - GENERICO DI ERRORE
007800930720     F*---------------------------------------------------------------*
007900930720     F* DATA BASE                                                     *
008000930720     F*---------------------------------------------------------------*
008100970804     FFNSPE01L  UF A E           K DISK
008200040622     FFNSP201L  UF A E           K DISK
008300930727     FCNACO00F  IF   E           K DISK
008400960412     FFNCLS01L  IF   E           K DISK    USROPN
008500961111     FAZORG01L  IF   E           K DISK
008600930720     FTABEL00F  IF   E           K DISK
008700930720     FCNC0M6D   CF   E             WORKSTN
008800930721     D*---------------------------------------------------------------*
008900930721     D* SCHIERE                                                       *
009000930721     D*---------------------------------------------------------------*
009100930721     D* INTESTAZIONE A VIDEO
009200930721     D INT             S             37    DIM(2) CTDATA PERRCD(1)
009300040715     D OPE             S             10    DIM(2) CTDATA PERRCD(1)
009400930727     D* FILIALI IN GESTIONE
009500020507     D L1              S              3  0 DIM(30)
009600020507     D*FIL             S              3    DIM(29)
009700960305     D* TIPI INVI COMUNICAZIONE MITT.
009800960305     D C2F             S              1    DIM(100)                             invio aper
009900960305     D T2F             S              3    DIM(100)                             tipo invio
010000960305     D C2H             S              1    DIM(100)                             invio chiu
010100960305     D T2H             S              1    DIM(100)                             Tipo invio
010200930727     D* CAPOCONTO
010300940901     D* ERRORI
010400000630     D ERR             S             78    DIM(27) CTDATA PERRCD(1)
010500960412     D* CARATTERI NON VALIDI
010600960412     D CAR             S              1    DIM(10) CTDATA PERRCD(10)            CAR NON VA
010700961112     D* CHKOBJ PER FILE FNCLS01L
010800961112     D CMD1            S             35    DIM(1) CTDATA PERRCD(1)              CHKOBJ
010900930720     D*---------------------------------------------------------------*
011000930720     D* DS                                                            *
011100930720     D*---------------------------------------------------------------*
011200930720     D*-----------------
011300930720     D* PARAMETRI ESTERNI
011400930720     D*-----------------
011500930720     D KPJBA         E DS
011600930720     D  LIBSYS                92    100
011700961009     D DSC0M6          DS
011800961009     D* Cod.luogo
011900961009     D  DM6SPE                 1      3
012000961009     D* Codice cliente
012100961009     D  DM6COD                 4     10  0
012200961009     D* ric = 'S' -> pgm richiamato
012300961009     D  DM6RIC                11     11
012400930720     D*-----------------
012500930721     D* NUMERATORE NUOVI SPEDIZIONIERI
012600930720     D*-----------------
012700930720     D SPEDTA          DS
012800930720     D  SPENUO                 1      3  0
012900930720     D*------------------
013000930720     D* DS RICERCA PARAMETRI UTENTE
013100930720     D*------------------
013200930720     D CNCR80        E DS
013300940901     D XIVAEU        E DS
013400960305     D DS2F          E DS
013500960305     D DS2H          E DS
013600960305     D DS4L          E DS
013700010706     D**!!! DS3D          E DS
013800980219     D DS15          E DS
013900020507     D TR06DS        E DS                  EXTNAME(TRUL06DS)
014000020507     D  LIN                    1     90  0 dim(30)
014100930720     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
014200930727     D  TCU                  398    697
014300930727     D                                     DIM(50)
014400930727     D  KCU                  698    847P 0
014500930727     D                                     DIM(50)
014600930727     D                                     PACKEVEN
014700930727     D*------------------
014800930727     D* DS RICERCA CAPOCONTO
014900930727     D*------------------
015000930727     D TCUDS           DS
015100930727     D  F1                     1      1
015200930727     D  F3                     3      3
015300930727     D  F2                     2      2
015400930727     D  F4                     4      4
015500930727     D  F56                    5      6
015600960305     D*------------------
015700960305     D* DS x scomposzione clsflo
015800960305     D*------------------
015900960305     D                 DS
016000960305     D  WCHIUS                 1      1
016100960305     D  WAPERT                10     10
016200960305     D  CLSFLO                 1     10
016300990312     D*------------------
016400990312     D* DS x scomposzione KNSIF
016500990312     D*------------------
016600990312     D                 DS
016700990312     D  WSITST                 7      7
016800990312     D  WSITRE                 7     10
016900990312     D  WKNSIF                 1     10
017000960304     D* Ds esterna x richiamo pgm controllo fax
017100960304     D TRUL42        E DS                  EXTNAME(TRUL42DS)
017200040622     D dsemail       E DS
017300940901     D VIDIVA          DS
017400940901     D  EIVA                   1      2
017500940901     D  VIVA                   3     16
017600970709     D* PARAMETRI PASSATI AL PGM CONTROLLO CAP
017700970709     D FNLV13        E DS                  EXTNAME(FNLV13DS)
017800970703     D* PARAMETRI PASSATI AL PGM CONTROLLO CAP TISI95R
017900970703     D TISI95        E DS                  EXTNAME(TISI95DS)
018000970711     D TISI00        E DS                  EXTNAME(TISI00DS)
018100980219     D WLBDAT          DS                  INZ
018200980219     D  G02DAT                 1      8  0
018300980219     D  G02INV                 9     16  0
018400980219     D  G02ERR                17     17
018500980219     D  G02TGI                18     22  0
018600930720     C*---------------------------------------------------------------*
018700930720     C*                    MAIN                                       *
018800930720     C* PROGRAMMA DI MANUTENZIONE ANAGRAFICA SPEDIZIONIERI/LUOGHI     *
018900930720     C*---------------------------------------------------------------*
019000990312     C***
019100990312     C* IL PROGRAMMA è ESEGUIBILE SOLO SDI, BARTOLINI ED IN TEST
019200990312     C***
019300990312     C                   MOVEL     KNSIF         WKNSIF
019400990312     C     WSITRE        IFNE      '101'
019500990312     C     WSITRE        ANDNE     '201'
019600990312     C     WSITST        ANDNE     'P'
019700990312     C                   GOTO      FINE
019800990312     C                   ENDIF
019900961112     C***
020000961112     C* CONTROLLO ESISTENZA FILE FNCLS PRIMA DI APRIRLO (CHKOBJ)
020100961112     C***
020200961112     C                   SETOFF                                       15
020300961112     C                   Z-ADD     35            LUNG             15 5
020400961112     C                   MOVEL     *BLANKS       COMMAN           80
020500961112     C                   MOVEA     CMD1(1)       COMMAN
020600961112     C                   CALL      'QCMDEXC'                            15
020700961112     C                   PARM                    COMMAN
020800961112     C                   PARM                    LUNG
020900961112     C  N15              OPEN      FNCLS01L
021000930720     C*
021100930727     C* CARICAMENTO TABELLA FILIALI IN GESTIONE
021200930727     C                   EXSR      CARTAB
021300961009     C***
021400961009     C*  PGM RICHIAMATO
021500961009     C***
021600961009     C     *IN09         IFEQ      *ON
021700961009     C                   EXSR      PGMRIC
021800961009     C* Vado direttamente alla videata di immissione dati
021900970820     C     DM6SPE        IFEQ      *BLANKS
022000970820     C     DM6COD        OREQ      0
022100970820     C                   GOTO      FMTD2
022200970820     C                   ELSE
022300970820     C                   GOTO      FMTD3
022400961009     C                   END
022500970820     C                   END
022600930720     C*
022700930721     C* RICHIESTA ANAGRAFICA DA MANUTENZIONARE
022800930720     C     INIZIO        TAG
022900041122     C**!!!              EXFMT     C006D1
023000930721     C*
023100930721     C* CMD3 - FINE LAVORO
023200041122     C*! KC              GOTO      FINE
023300930721     C*
023400930721     C* CONTROLLO DATI PRIMO FORMATO VIDEO
023500041122     c                   Eval      speluo = 'S'
023600930721     C                   EXSR      CONTR1
023700930721     C*
023800930721     C* ERRORE: RIEMETTO LA VIDEATA
023900930721     C   90              GOTO      INIZIO
024000930721     C*
024100930721     C* RICHIESTA CODICI
024200930722     C                   MOVEL     *BLANKS       DESSPE
024300930723     C                   MOVEL     *BLANKS       CODSPE
024400930723     C                   MOVEL     *BLANKS       CODCLI
024500961112     C   12
024600961112     CAN 08              MOVEL     999           V5CFIL
024700961112     C   12
024800961112     CANN08              CLEAR                   V5CFIL
024900940901     C                   SETOFF                                           43
025000940901     C                   SETOFF                                       284142
025100930721     C     FMTD2         TAG
025200961112     C   12
025300961112     CANN52              MOVEL     *BLANKS       VCODL
025400961111     C   12              EXFMT     C006D5
025500930723     C     FMTD2B        TAG
025600930723     C*
025700930721     C  N12              EXFMT     C006D3
025800930721     C*
025900930721     C* CMD3 - FINE LAVORO
026000930721     C   KC              GOTO      FINE
026100930721     C*
026200930721     C* CM12 - RITORNO VIDEATA PRECEDENTE
026300940901     C   KL              SETOFF                                       2840
026400970820     C   KL*IN09         IFEQ      *ON
026500970820     C                   GOTO      FINE
026600970820     C                   ELSE
026700970820     C                   GOTO      INIZIO
026800970820     C                   END
026900930721     C*
027000930721     C* CONTROLLO DATI SECONDO FORMATO VIDEO
027100930721     C                   EXSR      CONTR2
027200930721     C*
027300930721     C* ERRORE: RIEMETTO LA VIDEATA
027400930723     C   90
027500930723     CAN 12              GOTO      FMTD2
027600930723     C   90
027700930723     CANN12              GOTO      FMTD2B
027800930722     C*
027900930722     C* RICHIESTA DATI
028000940901     C                   SETOFF                                           28
028100940901     C                   SETOFF                                       444546
028200940901     C                   SETOFF                                       474849
028300970703     C                   SETOFF                                       505153
028400040622     C                   SETOFF                                       54
028500930722     C     FMTD3         TAG
028600930722     C                   EXFMT     C006D4
028700930722     C*
028800930723     C* CMD3 - FINE LAVORO
028900930722     C   KC              GOTO      FINE
029000930722     C*
029100930722     C* CM12 - RITORNO VIDEATA PRECEDENTE
029200961009     C*        (se pgm richiamato vado a fine)
029300940901     C                   SETOFF                                           43
029400940901     C                   SETOFF                                       284142
029500961009     C   KL
029600961009     CAN 09              GOTO      FINE
029700930723     C   KL
029800930723     CAN 12              GOTO      FMTD2
029900930723     C   KL
030000930723     CANN12              GOTO      FMTD2B
030100930722     C*
030200930722     C* CONTROLLO DATI TERZO FORMATO VIDEO
030300930722     C                   EXSR      CONTR3
030400930722     C*
030500930722     C* ERRORE: RIEMETTO LA VIDEATA
030600930722     C   90              GOTO      FMTD3
030700930723     C*
030800930723     C* CMD6 - AGGIORNAMENTO
030900930723     C   KF              EXSR      AGGIO
031000940901     C   KF              SETOFF                                       2840
031100961009     C   KF
031200961009     CAN 09              GOTO      FINE
031300930723     C   KF              GOTO      INIZIO
031400930723     C*
031500930723     C* CM16 - ANNULLAMENTO
031600961011     C* Se annullamento di luogo controllo che non sia presente in
031700961011     C* tabella 3D
031800010706     C**!!!   KQ
031900010706     C**!!!AN 12              EXSR      CTR3D
032000010706     C**!!!   KQ
032100010706     C**!!!AN 12
032200010706     C**!!!AN 90              GOTO      FMTD3
032300930723     C   KQ              EXSR      ANNUL
032400940901     C   KQ              SETOFF                                       2840
032500961009     C   KQ
032600961009     CAN 09              GOTO      FINE
032700961009     C   KQ              GOTO      INIZIO
032800930721     C*
032900930723     C* ENTER - RIMANE NELLA VIDEATA
033000930723     C                   GOTO      FMTD3
033100930723     C*
033200930721     C     FINE          TAG
033300930720     C                   SETON                                        LR
033400930721     C*---------------------------------------------------------------*
033500930721     C*                    CONTR1                                     *
033600930727     C* ROUTINE DI CONTROLLO DATI PRIMO FORMATO VIDEO                 *
033700930721     C*---------------------------------------------------------------*
033800930721     C     CONTR1        BEGSR
033900930721     C*
034000930723     C                   SETOFF                                       901213
034100940901     C                   SETOFF                                       2840
034200930721     C*
034300930721     C* ANAGRAFICA DA MANUTENZIONARE
034400930721IF  1C     SPELUO        IFEQ      *BLANKS
034500940901     C                   SETON                                        409028
034600940901     C                   MOVEL     ERR(2)        $MSG
034700930721     C                   GOTO      ENDCT1
034800930721E   1C                   ENDIF
034900930722     C*
035000930722     C* 12 ON  - SCELTA ANAGRAFICA LUOGHI
035100930722     C* 12 OFF - SCELTA ANAGRAFICA SPEDIZIONIERI
035200930722     C*
035300930722IF  1C     SPELUO        IFEQ      'L'
035400930723     C                   SETON                                        1213
035500930722E   1C                   ENDIF
035600930722     C*
035700930723     C* SPEDIZIONIERI
035800930722IF  1C     *IN12         IFEQ      *OFF
035900930722     C*
036000930722     C* LETTURA CODICE ULTIMO SPEDIZIONIERE
036100930722     C     *LOCK         IN        SPEDTA                               90
036200930722     C*
036300930722     C* SE L'ALLOCAZIONE NON RIESCE RITENTO ALTRE 5 VOLTE
036400930722IF  2C     *IN90         IFEQ      *ON
036500930722     C                   Z-ADD     *ZEROS        I
036600930722DO  3C     *IN90         DOWEQ     *ON
036700930722     C     I             ANDLE     5
036800930722     C                   ADD       1             I                 1 0
036900930722     C     *LOCK         IN        SPEDTA                               90
037000930722E   3C                   ENDDO
037100930722E   2C                   ENDIF
037200930722     C*
037300930722     C* ALLOCAZIONE NON RIUSCITA: EMETTO UN MESSAGGIO
037400930722IF  2C     *IN90         IFEQ      *ON
037500940901     C                   SETON                                        4028
037600940901     C                   MOVEL     ERR(1)        $MSG
037700930722     C                   GOTO      INIZIO
037800930722E   2C                   ENDIF
037900930722     C*
038000930722     C* PRIMO CODICE SPEDIZIONIERE LIBERO
038100930722     C                   ADD       1             SPENUO
038200930723E   1C                   ENDIF
038300930721     C*
038400930721     C     ENDCT1        ENDSR
038500930722     C*---------------------------------------------------------------*
038600930722     C*                    CONTR2                                     *
038700930722     C* ROUTINE DI CONTROLLO DATI SECONDO FORMATO VIDEO               *
038800930722     C*---------------------------------------------------------------*
038900930722     C     CONTR2        BEGSR
039000930722     C*
039100930722     C                   SETOFF                                       900102
039200040715     C                   SETOFF                                       105243
039300940901     C                   SETOFF                                       284142
039400930722     C                   Z-ADD     *ZEROS        COMSPE            3 0
039500020507     C                   Z-ADD     *ZEROS        COMFIL            3 0
039600930722     C*
039700930722     C* L U O G H I
039800930722     C*
039900930722IF  1C     *IN12         IFEQ      *ON
040000940502     C*
040100940502     C* RICHIESTA INTERROGAZIONE TABELLA CODICI LUOGHI UTILIZZABILI
040200940412IF  2C     VCODL         IFEQ      'L'
040300940412     C                   MOVEL     '4L'          COD
040400940412     C                   MOVEL     *BLANKS       KEY
040500940412     C                   MOVEL     *BLANKS       DES
040600940412     C                   CALL      'X§TABER'
040700940412     C                   PARM                    CODUT
040800940412     C                   PARM                    COD
040900940412     C                   PARM                    KEY
041000940412     C                   PARM                    DES              25
041100940412     C*
041200940412     C                   MOVEL     KEY           CODSPE
041300940412     C                   SETON                                        90
041400940412     C                   GOTO      ENDCT2
041500940412E   2C                   ENDIF
041600940502     C*
041700940502     C* RICHIESTI TUTTI I CODICI LUOGO PER STESSO CLIENTE
041800940412IF  2C     VCODL         IFEQ      'S'
041900940412     C*
042000940412     C* PER LA RICERCA IL CODICE CLIENTE DEVE ESSERE INSERITO
042100000630IF  3C     CODCLI        IFEQ      *ZEROS
042200940901     C                   SETON                                        904228
042300940901     C                   MOVEL     ERR(19)       $MSG
042400940412     C                   GOTO      ENDCT2
042500000630      *
042600000630X   3C                   ELSE
042700000630      *
042800000630     C                   MOVE      CODCLI        COD4              4 0
042900000630IF  4C     COD4          IFEQ      *ALL'9'
043000000630IF  4C     COD4          OREQ      *ALL'8'
043100000630     C                   SETON                                        904228
043200000630     C                   MOVEL     ERR(27)       $MSG
043300000630     C                   GOTO      ENDCT2
043400000630E   4C                   END
043500000630E   3C                   ENDIF
043600940412     C*
043700041201     C*******!!!!        MOVEL     *ZEROS        CODSPE
043800041201     C*******!!!!        CLEAR                   PARFIL
043900041201     C*******!!!!        CALL      'CNC0M2R'
044000041201     C*******!!!!        PARM                    CODCLI
044100041201     C*******!!!!        PARM                    CODSPE
044200041201     C*******!!!!        PARM                    PARFIL
044300041201     C*******!!!!        SETON                                            90
044400041201     C*******!!!!        GOTO      ENDCT2
044500940412E   2C                   ENDIF
044600930722     C*
044700930722     C* CODICE LUOGO OBBLIGATORIO
044800930722IF  2C     CODSPE        IFEQ      *BLANKS
044900930722     C     CODSPE        OREQ      *ZEROS
045000940901     C                   SETON                                        904128
045100940901     C                   MOVEL     ERR(14)       $MSG
045200930722     C                   GOTO      ENDCT2
045300930722X   2C                   ELSE
045400940502     C*
045500940502     C* RICHIESTA INTERROGAZIONE DI TUTTI I CLIENTI CON STESSO CODICE LUOGO
045600940412IF  3C     VCODL         IFEQ      'C'
045700961112     C*
045800961111     C* SE RICHIESTI I CODICI DI UN SOLO PUNTO OPERATIVO CONTROLLO CHE
045900961111     C*   SIA VALIDO
046000961112    4C     *IN08         IFEQ      *ON                                          *FILIALE
046100961112     C     V5CFIL        ANDNE     0
046200961112     C     V5CFIL        ANDNE     999
046300961112     C     *IN08         OREQ      *OFF                                         *SEDE
046400961112     C     V5CFIL        ANDNE     0
046500961111     C     V5CFIL        CHAIN     AZORG01L                           35
046600961112    5C     *IN35         IFEQ      *ON
046700961111     C     ORGFVA        ORNE      ' '
046800961111     C                   SETON                                        905228
046900961111     C                   MOVEL     ERR(21)       $MSG
047000961111     C                   GOTO      ENDCT2
047100961111    5C                   ENDIF
047200961112    4C                   ENDIF
047300961111     C*
047400940412     C                   MOVEL     *ZEROS        CODCLI
047500961111     C                   MOVEL     V5CFIL        PARFIL
047600940412     C                   CALL      'CNC0M2R'
047700940412     C                   PARM                    CODCLI
047800940412     C                   PARM                    CODSPE
047900961111     C                   PARM                    PARFIL
048000940412     C                   SETON                                            90
048100940412     C                   GOTO      ENDCT2
048200940412E   3C                   ENDIF
048300930722     C*
048400930722     C* VOGLIO SOLO DEI VALORI NUMERICI
048500930726IF  3C                   TESTN                   CODSPE               02  02
048600930722     C     *IN02         IFEQ      *OFF
048700940901     C                   SETON                                        904128
048800940901     C                   MOVEL     ERR(15)       $MSG
048900930723     C                   GOTO      ENDCT2
049000930722E   3C                   ENDIF
049100940502     C*
049200940502     C* CONTROLLO CODICE LUOGO IN TABELLA
049300940412     C                   MOVEL     '4L'          COD
049400940412     C                   MOVEL     *BLANKS       KEY
049500940412     C                   MOVEL     CODSPE        KEY
049600940412     C     KTAB          CHAIN     TABEL00F                           30
049700940412IF  3C     *IN30         IFEQ      *ON
049800940901     C                   SETON                                        904128
049900940901     C                   MOVEL     ERR(15)       $MSG
050000940412     C                   GOTO      ENDCT2
050100940412     C                   ELSE
050200940413     C                   MOVEL     TBLUNI        DS4L
050300940502     C*
050400940413     C* NON UTILIZZABILE IN FILIALE
050500940413     C     SIMFEL        IFNE      *ZEROS
050600940413     C     §4LUFI        ANDNE     'S'
050700940901     C                   SETON                                        904128
050800940901     C                   MOVEL     ERR(15)       $MSG
050900940413     C                   GOTO      ENDCT2
051000940413     C                   END
051100940413     C                   MOVEL     §4LDES        VDESLU
051200940412E   3C                   ENDIF
051300930722E   2C                   ENDIF
051400930722     C*
051500930722     C* CODICE CLIENTE OBBLIGATORIO
051600930722IF  2C     CODCLI        IFEQ      *ZEROS
051700940901     C                   SETON                                        904228
051800940901     C                   MOVEL     ERR(17)       $MSG
051900930722     C                   GOTO      ENDCT2
052000930722X   2C                   ELSE
052100000630     C*
052200000630     C*  CLIENTI GENERICI BLOCCATO INSERIMENTO  -----
052300000630     C                   MOVE      CODCLI        COD4              4 0
052400000630IF  4C     COD4          IFEQ      *ALL'9'
052500000630IF  4C     COD4          OREQ      *ALL'8'
052600000630     C                   SETON                                        904228
052700000630     C                   MOVEL     ERR(27)       $MSG
052800000630     C                   GOTO      ENDCT2
052900000630E   4C                   END
053000000630     C*  -----------------------------------
053100930722     C*
053200930722     C* CODICE CLIENTE DIVERSO DA 9999999
053300930722IF  3C     CODCLI        IFEQ      9999999
053400940901     C                   SETON                                        904228
053500940901     C                   MOVEL     ERR(16)       $MSG
053600930722     C                   GOTO      ENDCT2
053700930722E   3C                   ENDIF
053800930727     C*
053900930727     C* CODICE CLIENTE PRESENTE NEL P.D.C.
054000930727     C                   Z-ADD     CODCLI        KSC
054100930727     C     KACO          CHAIN     CNACO00F                           32
054200930727IF  3C     *IN32         IFEQ      *OFF
054300930727     C*
054400930727     C* CODICE CLIENTE ANNULLATO
054500930727IF  4C     ACOFLG        IFNE      *BLANKS
054600930727     C                   SETON                                        32
054700930727X   4C                   ELSE
054800930727     C*
054900931215     C* CODICE CLIENTE IN GESTIONE ALLA FILIALE (IN SEDE LI POSSO GESTIRE TUTTI)
055000931215IF  5C     SIMFEL        IFNE      *ZEROS
055100930727     C                   MOVEL     CODCLI        COMFIL
055200020507     C     COMFIL        LOOKUP    L1                                     33
055300990312     C  N33              SETON                                        32
055400990312X   5C                   ELSE
055500990312      * P.O. CLIENTE 46 GESTIBILE SOLO DA SISTEMA INFORMATIVO 201
055600990312     C                   MOVEL     CODCLI        POCLI             3 0
055700990312IF  6C     POCLI         IFEQ      46
055800990312     C     WSITRE        ANDNE     '201'
055900990312     C                   SETON                                        32
056000931215E   6C                   ENDIF
056100990312E   5C                   ENDIF
056200990312      *
056300930727E   4C                   ENDIF
056400930727E   3C                   ENDIF
056500930727     C*
056600930727     C* CODICE CLIENTE INESISTENTE O NON GESTIBILE
056700930727IF  3C     *IN32         IFEQ      *ON
056800940901     C                   SETON                                        904228
056900940901     C                   MOVEL     ERR(18)       $MSG
057000930727     C                   GOTO      ENDCT2
057100930727E   3C                   ENDIF
057200930722E   2C                   ENDIF
057300930722     C*
057400930722     C* INTESTAZIONE A VIDEO
057500930722     C                   MOVEL     INT(2)        VIDINT
057600930722     C*
057700930722     C* CONTROLLO ESISTENZA RECORD
057800970804     C                   MOVEL     'L'           KFLS
057900970804     C     KEYSPE        CHAIN     FNSPE01L                           10
058000040622     C     KEYSP2        CHAIN     FNSP201L                           14
058100930722     C*
058200930722     C* S P E D I Z I O N I E R I
058300930722     C*
058400930722X   1C                   ELSE
058500930722     C*
058600930722     C* CODICE SPEDIZIONIERE OBBLIGATORIO
058700930722IF  2C     DESSPE        IFEQ      *BLANKS
058800930722     C     DESSPE        OREQ      *ZEROS
058900930722     C*
059000930722IF  3C     CODSPE        IFEQ      *BLANKS
059100930722     C     CODSPE        OREQ      *ZEROS
059200940901     C                   SETON                                        439028
059300940901     C                   MOVEL     ERR(3)        $MSG
059400930722     C                   GOTO      ENDCT2
059500930722E   3C                   ENDIF
059600930722E   2C                   ENDIF
059700930722     C*
059800930722     C* RICHIESTA RICERCA SPEDIZIONIERI PER RAGIONE SOCIALE
059900930722IF  2C     DESSPE        IFNE      *BLANKS
060000930723     C     DESSPE        ANDNE     *ZEROS
060100930723     C     CODSPE        ANDEQ     *BLANKS
060200930723     C     CODSPE        OREQ      *ZEROS
060300930722     C*
060400930722     C                   MOVEL     *ZEROS        CODSPE
060500930722     C                   MOVEL     9999999       CODCLI
060600930722     C                   CALL      'CNC0M1R'
060700930722     C                   PARM                    CODCLI
060800930722     C                   PARM                    CODSPE
060900930722     C                   PARM                    DESSPE
061000930722     C                   SETON                                            90
061100930722     C                   GOTO      ENDCT2
061200930722E   2C                   ENDIF
061300930722     C*
061400930722     C* VOGLIO SOLO DEI VALORI NUMERICI
061500930726     C                   TESTN                   CODSPE               02  02
061600930723IF  2C     *IN02         IFEQ      *OFF
061700940901     C                   SETON                                        904328
061800940901     C                   MOVEL     ERR(4)        $MSG
061900930723     C                   GOTO      ENDCT2
062000930722E   2C                   ENDIF
062100930722     C*
062200930722     C* INTESTAZIONE A VIDEO
062300930722     C                   MOVEL     INT(1)        VIDINT
062400930722     C*
062500930722     C* CONTROLLO ESISTENZA RECORD
062600970804     C                   MOVEL     'S'           KFLS
062700930722     C                   Z-ADD     9999999       CODCLI
062800970804     C     KEYSPE        CHAIN     FNSPE01L                           10
062900040630     C     KEYSP2        CHAIN     FNSP201L                           14
063000930722     C*
063100930722     C* SE INESISTENTE IL CODICE DEVE ESSERE QUELLO DELLA DTAARA
063200930722IF  2C     *IN10         IFEQ      *ON
063300930722     C                   MOVEL     CODSPE        COMSPE
063400930723IF  3C     COMSPE        IFNE      SPENUO
063500940901     C                   SETON                                        904328
063600940901     C                   MOVEL     ERR(5)        $MSG
063700930722     C                   GOTO      ENDCT2
063800930722E   3C                   ENDIF
063900930722E   2C                   ENDIF
064000930722E   1C                   ENDIF
064100930722     C*
064200930722     C* I M P O S T A Z I O N I  CAMPI A VIDEO
064300930722     C*
064400930722     C* INSERIMENTO
064500930722IF  1C     *IN10         IFEQ      *ON
064600040715     C                   MOVEL     OPE(1)        VIDOPE
064700930722X   1C                   ELSE
064800040715     C                   MOVEL     OPE(2)        VIDOPE
064900040715E   1C                   ENDIF
065000930722     C*
065100930722     C* PER INSERIMENTO
065200960305     C                   MOVEL     *BLANKS       WSVTEL
065300040622     C                   MOVEL     *BLANKS       WSVmail
065400960305     C                   MOVEL     *BLANKS       WSVFAX
065500930722IF  1C     *IN10         IFEQ      *ON
065600930722     C                   MOVEL     *BLANKS       VIDRAG
065700930722     C                   MOVEL     *BLANKS       VIDIND
065800970804     C                   CLEAR                   VIDCAP
065900930722     C                   MOVEL     *BLANKS       VIDLOC
066000930722     C                   MOVEL     *BLANKS       VIDPRO
066100930722     C                   MOVEL     *BLANKS       VIDNAZ
066200930722     C                   MOVEL     *BLANKS       VIDIVA
066300930722     C                   MOVEL     *BLANKS       VIDTEL
066400930722     C                   MOVEL     *BLANKS       VIDTEX
066500960305     C                   MOVEL     *BLANKS       V4CFAX
066600930722     C                   MOVEL     *BLANKS       VIDRIV
066700040901     C                   MOVEL     *BLANKS       VIDmail
066800930722     C*
066900930722     C* PER VARIAZIONE
067000930722X   1C                   ELSE
067100930722     C                   MOVEL     SPERAG        VIDRAG
067200930722     C                   MOVEL     SPEIND        VIDIND
067300970804     C                   MOVEL     SPECAP        VIDCAP
067400930722     C                   MOVEL     SPELOC        VIDLOC
067500930722     C                   MOVEL     SPEPRO        VIDPRO
067600930722     C                   MOVEL     SPENAZ        VIDNAZ
067700940901     C                   MOVEL     SPEPIV        CNT2              2
067800940901     C     CNT2          IFGT      *ZEROS
067900940901     C                   MOVEL     SPEPIV        VIVA
068000940901     C                   MOVEL     *BLANKS       EIVA
068100940901     C                   ELSE
068200940901     C                   MOVEL     SPEPIV        VIDIVA
068300940901     C                   END
068400930722     C                   MOVEL     SPETEL        VIDTEL
068500970804     C                   MOVEL     SPEFAX        V4CFAX
068600930722     C                   MOVEL     SPETEX        VIDTEX
068700930722     C                   MOVEL     SPEPER        VIDRIV
068800040715     C  n14              MOVEL     SP2est        VIDmail
068900930722E   1C                   ENDIF
069000930723     C*
069100930723     C   12              MOVEL     CODSPE        CODSP2
069200930723     C  N12              Z-ADD     9999999       CODCLI
069300970709     C* PULISCO  CAMPI DI SALVATAGGIO FNLV13
069400970709     C                   CLEAR                   WSVCAP
069500970709     C                   CLEAR                   WSVLOC
069600970709     C                   CLEAR                   WFZV
069700970709     C                   CLEAR                   WFZ1
069800970709     C                   CLEAR                   WFZ2
069900930722     C*
070000930722     C     ENDCT2        ENDSR
070100930722     C*---------------------------------------------------------------*
070200930722     C*                    CONTR3                                     *
070300930722     C* ROUTINE DI CONTROLLO DATI TERZO FORMATO VIDEO                 *
070400930722     C*---------------------------------------------------------------*
070500930722     C     CONTR3        BEGSR
070600930722     C*
070700940901     C                   SETOFF                                       900128
070800940901     C                   SETOFF                                       444546
070900940901     C                   SETOFF                                       474849
071000970703     C                   SETOFF                                       505153
071100040622     C                   SETOFF                                       54
071200930723     C*
071300930723     C* RAGIONE SOCIALE OBBLIGATORIA
071400930723IF  1C     VIDRAG        IFEQ      *BLANKS
071500940901     C                   SETON                                        904428
071600940901     C                   MOVEL     ERR(6)        $MSG
071700930723     C                   GOTO      ENDCT3
071800930723E   1C                   ENDIF
071900960412I    C* CONTROLLO CHE RAG.SOCIALE CONTENGA SOLO DATI VALIDI
072000960412     C                   Z-ADD     1             XX
072100960412     C     XX            DOWLE     10
072200960412     C     CAR(XX)       ANDNE     *BLANKS
072300960412     C     CAR(XX)       SCAN      VIDRAG                                 96
072400960412     C     *IN96         IFEQ      *ON
072500960412     C                   Z-ADD     10            XX
072600960412     C                   SETON                                        904428
072700960412     C                   MOVEL     ERR(6)        $MSG
072800960412     C                   ENDIF
072900960412     C                   ADD       1             XX
073000960412E    C                   ENDDO
073100960412     C   44              GOTO      ENDCT3
073200930723     C*
073300930723     C* INDIRIZZO OBBLIGATORIO
073400930723IF  1C     VIDIND        IFEQ      *BLANKS
073500940901     C                   SETON                                        904528
073600940901     C                   MOVEL     ERR(7)        $MSG
073700930723     C                   GOTO      ENDCT3
073800930723E   1C                   ENDIF
073900960412I    C* CONTROLLO CHE INDIRIZZO   CONTENGA SOLO DATI VALIDI
074000960412     C                   Z-ADD     1             XX
074100960412     C     XX            DOWLE     10
074200960412     C     CAR(XX)       ANDNE     *BLANKS
074300960412     C     CAR(XX)       SCAN      VIDIND                                 96
074400960412     C     *IN96         IFEQ      *ON
074500960412     C                   Z-ADD     10            XX
074600960412     C                   SETON                                        904528
074700960412     C                   MOVEL     ERR(7)        $MSG
074800960412     C                   ENDIF
074900960412     C                   ADD       1             XX
075000960412E    C                   ENDDO
075100960412     C   45              GOTO      ENDCT3
075200930727     C*
075300930727     C* LOCALITA OBBLIGATORIA
075400930727IF  1C     VIDLOC        IFEQ      *ZEROS
075500930727     C     VIDLOC        OREQ      *BLANKS
075600940901     C                   SETON                                        904728
075700940901     C                   MOVEL     ERR(10)       $MSG
075800930727     C                   GOTO      ENDCT3
075900980219    1C                   END
076000980219     C* CONTROLLO NAZIONE
076100980219     C                   MOVEL     '15'          COD
076200980219     C                   MOVEL(P)  VIDNAZ        KEY
076300980219     C     KTAB          CHAIN     TABEL                              30
076400980219     C  N30TBLFLG        IFNE      ' '
076500980219     C                   SETON                                        30
076600980219     C                   END
076700980219     C*
076800980219    1C     *IN30         IFEQ      *ON
076900980219     C                   MOVEL     ERR(26)       $MSG
077000980220     C                   SETON                                        905328
077100980219     C                   GOTO      ENDCT3
077200980219    1C                   ENDIF
077300980219     C*
077400980219     C                   MOVEL     TBLUNI        DS15
077500930727     C*
077600930727     C* RICHIESTA RICERCA LOCALITA
077700980219     C* (SOLO SE NAZIONE CON CAPPARIO)
077800980219     C     §15CPP        IFEQ      *BLANKS
077900930727     C                   MOVEL     VIDLOC        PUNT              1
078000930727IF  2C     PUNT          IFEQ      '?'
078100930727     C                   MOVE      VIDLOC        VIDL19           19
078200930727     C*
078300930727     C* SE IMMESSO SOLO '?' E NESSUNA LETTERA SEGUENTE SI PARTE DA 'A'
078400930727IF  3C     VIDL19        IFEQ      *BLANKS
078500930727     C                   MOVEL     'A'           VIDL19
078600930727E   3C                   ENDIF
078700970711     C* RICHIAMO L'INTERROGAZIONE
078800970711     C                   CLEAR                   TISI00
078900970711     C                   MOVEL     'L01'         D00OP0
079000970711     C                   MOVEL     VIDL19        D00LOC
079100980219     C                   Z-ADD     DATEU         D00DRI
079200970711     C                   CALL      'TISI30R'
079300970711     C                   PARM                    KPJBA
079400970711     C                   PARM                    TISI00
079500970711     C* SE SELEZIONE
079600970711    5C     D00CAP        IFNE      *BLANKS
079700970711     C                   MOVEL     D00CAP        VIDCAP
079800970711     C                   MOVEL     D00LOC        VIDLOC
079900970711     C                   MOVEL     D00NAR        VIDNAZ
080000970711     C                   MOVEL     D00PRV        VIDPRO
080100970711     C                   ELSE
080200970711     C                   MOVEL     *BLANKS       VIDLOC
080300970711    5C                   ENDIF
080400930727     C*
080500930727     C                   SETON                                        90
080600930727     C                   GOTO      ENDCT3
080700930727E   2C                   ENDIF
080800980219    1C                   ENDIF
080900960412I    C* CONTROLLO CHE LOCALITA    CONTENGA SOLO DATI VALIDI
081000960412     C                   Z-ADD     1             XX
081100960412     C     XX            DOWLE     10
081200960412     C     CAR(XX)       ANDNE     *BLANKS
081300960412     C     CAR(XX)       SCAN      VIDLOC                                 96
081400960412     C     *IN96         IFEQ      *ON
081500960412     C                   Z-ADD     10            XX
081600960412     C                   SETON                                        904728
081700960412     C                   MOVEL     ERR(10)       $MSG
081800960412     C                   ENDIF
081900960412     C                   ADD       1             XX
082000960412E    C                   ENDDO
082100960412     C   47              GOTO      ENDCT3
082200930723     C*
082300930723     C* C.A.P.  OBBLIGATORIO
082400970709 F  1C***        VIDCAP    IFEQ *ZEROS
082500970709     C***                  SETON                     904628
082600970709     C***                  MOVELERR,8     $MSG
082700970709     C***                  GOTO ENDCT3
082800970709X   1C***                  ELSE
082900930727     C*
083000980219     C* CONTROLLO ESISTENZA CAP (SOLO SE NAZIONE CON CAPPARIO)
083100980219     C     §15CPP        IFEQ      *BLANKS
083200941125     C                   MOVEL     VIDCAP        KCAP              9
083300961218     C                   EXSR      CAZIS0
083400970703     C   28              GOTO      ENDCT3
083500980219     C                   END
083600930727     C*
083700970709E   1C***                  ENDIF
083800930727     C*
083900980219     C* PROVINCIA OBBLIGATORIA SE NAZIONE ITALIA
084000961218IF  1C     VIDPRO        IFEQ      *ZEROS
084100980219     C     VIDNAZ        ANDEQ     *BLANKS
084200930727     C     VIDPRO        OREQ      *BLANKS
084300980219     C     VIDNAZ        ANDEQ     *BLANKS
084400940901     C                   SETON                                        902848
084500940901     C                   MOVEL     ERR(12)       $MSG
084600930727     C                   GOTO      ENDCT3
084700980219    1C                   END
084800930727     C*
084900930915     C* RICHIESTA RICERCA PROVINCE
085000930723     C     '?'           SCAN      VIDPRO                                 01
085100930723IF  2C     *IN01         IFEQ      *ON
085200930723     C                   MOVEL     'PR'          COD
085300930723     C                   MOVEL     *BLANKS       KEY
085400930723     C                   MOVEL     *BLANKS       DES
085500930722     C                   CALL      'X§TABER'
085600930723     C                   PARM                    CODUT
085700930723     C                   PARM                    COD
085800930723     C                   PARM                    KEY
085900930723     C                   PARM                    DES              25
086000930723     C*
086100930723     C                   MOVEL     KEY           VIDPRO
086200930723     C                   SETON                                        90
086300930723     C                   GOTO      ENDCT3
086400980219    2C                   END
086500930723     C*
086600930723     C* CONTROLLO ESISTENZA PROVINCIA IMMESSA
086700980219    1C     VIDPRO        IFNE      *BLANKS
086800930723     C                   MOVEL     'PR'          COD
086900930723     C                   MOVEL     *BLANKS       KEY
087000930723     C                   MOVEL     VIDPRO        KEY
087100930727     C     KTAB          SETLL     TABEL00F                               30
087200930723IF  3C     *IN30         IFEQ      *OFF
087300940901     C                   SETON                                        904828
087400940901     C                   MOVEL     ERR(11)       $MSG
087500930723     C                   GOTO      ENDCT3
087600930723E   3C                   ENDIF
087700980219    1C                   ENDIF
087800930727     C*
087900960412I    C* CONTROLLO CHE PROVINCIA   CONTENGA SOLO DATI VALIDI
088000960412     C                   Z-ADD     1             XX
088100960412     C     XX            DOWLE     10
088200960412     C     CAR(XX)       ANDNE     *BLANKS
088300960412     C     CAR(XX)       SCAN      VIDPRO                                 96
088400960412     C     *IN96         IFEQ      *ON
088500960412     C                   Z-ADD     10            XX
088600960412     C                   SETON                                        904828
088700960412     C                   MOVEL     ERR(12)       $MSG
088800960412     C                   ENDIF
088900960412     C                   ADD       1             XX                2 0
089000960412E    C                   ENDDO
089100960412     C   48              GOTO      ENDCT3
089200930727     C*
089300930727     C* CONTROLLO  P.I. SE IMMESSA
089400930727IF  1C     VIDIVA        IFNE      *ZEROS
089500930727     C     VIDIVA        ANDNE     *BLANKS
089600940901     C***                  MOVELVIDIVA    PIVA
089700930727     C                   EXSR      CTRIVA
089800930727     C*
089900930727     C* P.I. ERRATA
090000940901IF  2C     *IN49         IFEQ      *ON
090100930727     C                   GOTO      ENDCT3
090200930727E   2C                   ENDIF
090300930727E   1C                   ENDIF
090400960305     C*  Se il luogo immesso è per l'invio delle comuni. di giace.
090500960305     C                   MOVEL     'N'           WFAXOB
090600960305     C     §4LGIA        IFEQ      'S'
090700960305     C                   Z-ADD     SPECLI        KKSC              7 0
090800960305     C     KKSC          CHAIN     FNCLS01L                           34
090900960305     C     *IN34         IFEQ      '0'
091000960305     C                   Z-ADD     1             §F
091100960305     C     WAPERT        LOOKUP    C2F(§F)                                34
091200960305     C     *IN34         IFEQ      '1'
091300960305     C     T2F(§F)       ANDEQ     'F'
091400960305     C                   MOVEL     'S'           WFAXOB            1            Fax obblig
091500960305     C                   ELSE
091600960305     C                   Z-ADD     1             §F
091700960305     C     WCHIUS        LOOKUP    C2H(§F)                                34
091800960305     C     *IN34         IFEQ      '1'
091900960305     C     T2H(§F)       ANDEQ     'F'
092000960305     C                   MOVEL     'S'           WFAXOB            1            Fax obblig
092100960305     C                   END                                                    T2H
092200960305     C                   END                                                    T2F
092300960305     C                   END                                                    trov.FNCLS
092400960305     C                   END                                                    §4LGIA
092500960305     C*  Controllo il numero del fax
092600960305     C     V4CFAX        IFNE      *BLANKS
092700960305     C     V4CFAX        ANDNE     WSVFAX
092800960305     C     §4LFAX        OREQ      'S'
092900960305     C     WFAXOB        OREQ      'S'
093000960305     C                   CLEAR                   TRUL42
093100960305     C                   MOVEL     V4CFAX        D42FAX
093200960305     C                   MOVEL     V4CFAX        WSVFAX           15
093300960305     C                   CALL      'TRUL42R'
093400960305     C                   PARM                    TRUL42
093500960305     C     D42ERR        IFEQ      '1'
093600960314     C                   SETON                                        285090
093700960305     C                   MOVEL     D42MSG        $MSG
093800960305     C                   END
093900960305     C                   END
094000960305     C*  Controllo il numero del telefono
094100960305     C  N50VIDTEL        IFNE      *BLANKS
094200960305     C     VIDTEL        ANDNE     WSVTEL
094300960305     C                   CLEAR                   TRUL42
094400960305     C                   MOVEL     VIDTEL        WSVTEL           20
094500960305     C                   MOVEL     VIDTEL        D42FAX
094600960305     C                   CALL      'TRUL42R'
094700960305     C                   PARM                    TRUL42
094800960305     C     D42ERR        IFEQ      '1'
094900960314     C                   SETON                                        285190
095000960305     C                   MOVEL     D42MSG        $MSG
095100960305     C                   END
095200960305     C                   END
095300930727     C*
095400040622     C*  Controllo l'indirizzo e-mail
095500040622     C     VIDmail       IFNE      *BLANKS
095600040622     C     VIDmail       ANDNE     WSVmail
095700040622     C                   CLEAR                   dsemail
095800040622     C                   MOVEL     VIDmail       §emlindi
095900040622     C                   CALL      'XEMAIL'
096000040622     C                   PARM                    dsemail
096100040622     C     §emlerro      IFEQ      '1'
096200040622     C                   SETON                                        285490
096300040622     C                   MOVEL     §emlmsgo      $MSG
096400040707     c                   else
096500040622     c                   movel     §emlindo      vidmail
096600040707     C                   END
096700040622     C                   END
096800040622     C*
096900961219     C     ENDCT3        ENDSR
097000961011     C*---------------------------------------------------------------*
097100961011     C*                    CTR3D                                      *
097200961011     C* SE ANNULLAMENTO CONTROLLO CHE NON ESISTA RECORD TABELLA 3D    *
097300961011     C*---------------------------------------------------------------*
097400010706     C**!!!     CTR3D         BEGSR
097500010706     C**!!!                   MOVEL     '3D'          COD
097600010706     C**!!!     KTAB1         SETLL     TABEL00F
097700010706     C**!!!     KTAB1         READE     TABEL00F                               31
097800010706     C**!!!     *IN31         DOWEQ     *OFF
097900010706     C**!!!     TBLFLG        IFEQ      *BLANKS
098000010706     C**!!!                   MOVEL     TBLUNI        DS3D
098100010706     C**!!!     §3DKSC        IFEQ      CODCLI
098200010706     C**!!!     §3DLRT        ANDEQ     CODSP2
098300010706     C**!!!                   SETON                                        902831
098400010706     C**!!!                   MOVEL     ERR(20)       $MSG
098500010706     C**!!!                   END
098600010706     C**!!!                   END
098700010706     C**!!!  N31KTAB1         READE     TABEL00F                               31
098800010706     C**!!!                   ENDDO
098900010706     C**!!!                   ENDSR
099000930727     C*---------------------------------------------------------------*
099100930727     C*                    CAZIS0                                     *
099200930727     C* ROUTINE DI CONTROLLO ESISTENZA CAP                            *
099300930727     C*---------------------------------------------------------------*
099400930727     C     CAZIS0        BEGSR
099500930727     C*
099600970709     C* Mi salvo i dati Località/Provincia/Nazione/P.Iva
099700970709     C* immessi a video in campi di appoggio per vedere se
099800980219     C* verrano modificati dai pgm richiamati (LV13)
099900970709     C                   MOVEL     VIDLOC        WVDLOC
100000970709     C                   MOVEL     VIDPRO        WVDPRV
100100970709     C                   MOVEL     VIDNAZ        WVDNAR
100200970709     C                   MOVEL     VIDCAP        WVDCAP
100300930727     C* CONTROLLO CAP PER INTERO
100400970703     C                   CLEAR                   TISI95
100500970709     C                   CLEAR                   FNLV13
100600970709     C                   MOVEL     ' '           I95TLA
100700970709     C                   MOVEL     '7'           I95TCN
100800970703     C                   MOVEL     VIDCAP        I95CAP
100900970703     C                   MOVEL     VIDNAZ        I95NAR
101000970703     C                   MOVEL     VIDIND        I95IND
101100970703     C                   MOVEL     VIDLOC        I95LOC
101200970703     C                   MOVEL     VIDPRO        I95PRV
101300970711     C***                  MOVEL19970715  I95DAT
101400970709     C                   MOVEL     'S'           I13AF0
101500970709     C                   MOVEL     'S'           I13AF1
101600970709     C                   MOVEL     'S'           I13SZ2
101700970710     C                   MOVEL     'S'           I13LA3
101800970710     C                   MOVEL     'S'           I13SZ3
101900970709     C                   MOVEL     'S'           I13CNV
102000970709     C                   MOVEL     'S'           I13SZV
102100970703     C     SIMFEL        IFEQ      0
102200970703     C                   Z-ADD     46            I95TFP
102300970703     C                   ELSE
102400970703     C                   Z-ADD     SIMFEL        I95TFP
102500970703     C                   END
102600980602     C                   MOVEL     'S'           I95FRE
102700970709     C* Gestione errori forzabili: se cambiato CAP/Località
102800970709     C* riazzero i valori di errori forzabili
102900970709    2C     VIDCAP        IFNE      WSVCAP
103000970709     C                   CLEAR                   WFZV
103100970709     C                   CLEAR                   WFZ1
103200970709     C                   CLEAR                   WFZ2
103300970710     C                   CLEAR                   WFZ3
103400970709     C                   MOVEL     VIDCAP        WSVCAP
103500970709    2C                   ENDIF
103600970709     C* Modifica località
103700970709    2C     VIDLOC        IFNE      WSVLOC
103800970709     C                   MOVEL     VIDLOC        WSVLOC
103900970709     C                   CLEAR                   WFZ1
104000970709     C                   CLEAR                   WFZ2
104100970710     C                   CLEAR                   WFZ3
104200970709    2C                   ENDIF
104300970710     C* Modifica PROVINCIA
104400970710    2C     VIDPRO        IFNE      WSVPRO
104500970710     C                   MOVEL     VIDPRO        WSVPRO
104600970710     C                   CLEAR                   WFZ3
104700970710    2C                   ENDIF
104800970709     C** CALL
104900970709     C                   MOVEL     WFZV          E13FZV
105000970709     C                   MOVEL     WFZ1          E13FZ1
105100970709     C                   MOVEL     WFZ2          E13FZ2
105200970710     C                   MOVEL     WFZ3          E13FZ3
105300970709     C                   CALL      'FNLV13R'
105400970709     C                   PARM                    KPJBA
105500970709     C                   PARM                    FNLV13
105600970709     C                   PARM                    TISI95
105700970709     C* Ricontrollo errori
105800970709     C                   MOVEL     O13ERR        WERR
105900970709     C                   MOVEL     O13MSG        WMSG
106000970709     C                   MOVEL     E13FZV        WFZV
106100970709     C                   MOVEL     E13FZ1        WFZ1
106200970709     C                   MOVEL     E13FZ2        WFZ2
106300970710     C                   MOVEL     E13FZ3        WFZ3
106400970709     C*
106500970709     C* Se il livello di affidabilità non è inferiore a 2 e
106600970709     C* il livello di reperimento dati non è inferiore a 3
106700970709     C* reimposto i dati da ds TISI95
106800970709     C     O13RON        IFEQ      'S'
106900970709     C     O13ROC        OREQ      'S'
107000970709     C     O13ROP        OREQ      'S'
107100970709     C     O13ROL        OREQ      'S'
107200970709     C                   MOVEL     O95LOC        VIDLOC
107300970709     C                   MOVEL     O95PRV        VIDPRO
107400970709     C                   MOVEL     O95NAR        VIDNAZ
107500970709     C                   MOVEL     O95CAP        VIDCAP
107600970709     C                   END
107700970709     C*
107800970709     C                   EXSR      CTRIND
107900970709     C* Se non ci sono errori proseguo
108000970709     C   28              GOTO      FINCTR
108100970709     C*
108200970709     C* Controllo se reimpostata località
108300970710     C     O13ROL        IFEQ      'S'
108400970709     C                   SETON                                        4728
108500970710     C                   MOVEL     ERR(22)       $MSG
108600970709     C                   GOTO      FINCTR
108700970709     C                   END
108800970709     C* Controllo se reimpostata Provincia
108900970710     C     O13ROP        IFEQ      'S'
109000970709     C                   SETON                                        4828
109100970710     C                   MOVEL     ERR(24)       $MSG
109200970709     C                   GOTO      FINCTR
109300970709     C                   END
109400970709     C* Controllo se reimpostato CAP
109500970710     C     O13ROC        IFEQ      'S'
109600970709     C                   SETON                                        4628
109700970710     C                   MOVEL     ERR(23)       $MSG
109800970709     C                   GOTO      FINCTR
109900970709     C                   END
110000970709     C* Controllo se reimpostata Nazione
110100970710     C     O13RON        IFEQ      'S'
110200970709     C                   SETON                                        5328
110300970710     C                   MOVEL     ERR(25)       $MSG
110400970709     C                   GOTO      FINCTR
110500970709     C                   END
110600930727     C*
110700970709     C     FINCTR        ENDSR
110800970709     C*------------------------------------------------------*
110900970709     C*  Controllo dati indirizzo
111000970709     C*------------------------------------------------------*
111100970709     C     CTRIND        BEGSR
111200970709     C*
111300970709     C* Errore !!!!
111400970709     C     WERR          IFNE      ' '
111500970709     C     WMSG          ANDNE     *BLANKS
111600970709     C* Manca la Ragione sociale
111700970709     C                   SELECT
111800970709     C* Errore su località/città
111900970709     C     WERR          WHENEQ    '3'
112000970709     C                   SETON                                        4728
112100970709     C* Errore su Cap
112200970709     C     WERR          WHENEQ    '5'
112300970709     C                   SETON                                        4628
112400970709     C* Errore su provincia
112500970709     C     WERR          WHENEQ    '4'
112600970709     C                   SETON                                        4828
112700970709     C* Nazione  errata
112800970709     C     WERR          WHENEQ    '6'
112900970709     C                   SETON                                        5328
113000970709    2C                   ENDSL
113100970709     C                   MOVEL     WMSG          $MSG
113200970709     C                   END
113300010124      *
113400010124     C   28              EVAL      *IN90 = *ON
113500010124      *
113600970709     C                   ENDSR
113700930727     C*---------------------------------------------------------------*
113800930727     C*                    CTRIVA                                     *
113900930727     C* ROUTINE DI CONTROLLO PARTITA IVA                              *
114000930727     C*---------------------------------------------------------------*
114100930727     C     CTRIVA        BEGSR
114200930727     C*
114300050726     c                   Clear                   xivaprv           2
114400940901     C                   CLEAR                   XIVAEU
114500940901     C                   MOVEL     VIVA          IVAPAR
114600940901     C                   MOVEL     EIVA          IVASTA
114700050726     c                   Movel     vidnaz        ivanar
114800050726     c                   Movel     vidpro        xivaprv
114900940901     C                   CALL      'XIVAEUR'
115000940901     C                   PARM                    XIVAEU
115100050726     c                   Parm                    xivaprv
115200940901     C     IVAFLG        IFLT      0
115300050726     C                   SETON                                        904928
115400940901     C                   MOVEL     ERR(13)       $MSG
115500940901     C                   END
115600940901     C*
115700930727     C                   ENDSR
115800930723     C*---------------------------------------------------------------*
115900930723     C*                    ANNUL                                      *
116000930723     C* ROUTINE DI ANNULLAMENTO RECORD                                *
116100930723     C*---------------------------------------------------------------*
116200930723     C     ANNUL         BEGSR
116300930723     C*
116400970804     C* LUOGHI/SPEDIZIONIERI
116500040715     C                   delete    FNSPE000
116600040715     C  n14              delete    FNSP2000
116700930723     C* SPEDIZIONIERI
116800970804     C***  N12                UPDATANSPE1
116900930723     C*
117000930723     C                   ENDSR
117100930723     C*---------------------------------------------------------------*
117200930723     C*                    AGGIO                                      *
117300930723     C* ROUTINE DI AGGIORNAMENTO RECORD                               *
117400930723     C*---------------------------------------------------------------*
117500930723     C     AGGIO         BEGSR
117600930723     C*
117700930723     C* IMPOSTAZIONE CAMPI
117800930723     C                   MOVE      *BLANKS       SPEFLG
117900930723     C                   Z-ADD     CODCLI        SPECLI
118000970804     C   12              MOVEL     'L'           SPEFLS
118100970804     C  N12              MOVEL     'S'           SPEFLS
118200930723     C  N12              MOVEL     CODSPE        SPECOD
118300930723     C   12              MOVEL     CODSP2        SPECOD
118400930723     C                   MOVEL     VIDRAG        SPERAG
118500930723     C                   MOVEL     VIDIND        SPEIND
118600970804     C                   MOVEL     VIDCAP        SPECAP
118700930723     C                   MOVEL     VIDLOC        SPELOC
118800930723     C                   MOVEL     VIDPRO        SPEPRO
118900930723     C                   MOVEL     VIDNAZ        SPENAZ
119000940901     C     EIVA          IFEQ      *BLANKS
119100940901     C                   MOVEL     VIVA          SPEPIV
119200940901     C                   ELSE
119300940901     C                   MOVEL     VIDIVA        SPEPIV
119400940901     C                   END
119500930723     C                   MOVEL     VIDTEL        SPETEL
119600970804     C                   MOVEL     V4CFAX        SPEFAX
119700930723     C                   MOVEL     VIDTEX        SPETEX
119800930723     C                   MOVEL     VIDRIV        SPEPER
119900040719     C                   move      DATEU         SPEDTR
120000930723     C                   MOVEL     *BLANKS       SPEFTR
120100040622     C                   MOVEL     VIDmail       SP2est
120200930723     C*
120300930723     C* V A R I A Z I O N E
120400930723     C*
120500930723IF  1C     *IN10         IFEQ      *OFF
120600970804     C* LUOGHI/SPEDIZIONIERI
120700970804     C                   UPDATE    FNSPE000
120800930723     C* SPEDIZIONIERI
120900970804     C**  N12                UPDATANSPE1
121000930723X   1C                   ELSE
121100930723     C*
121200930723     C* I N S E R I M E N T O
121300930723     C*
121400930723     C* LUOGHI
121500970804     C   12              WRITE     FNSPE000
121600930723     C* SPEDIZIONIERI
121700930723     C     *IN12         IFEQ      *OFF
121800930723     C*
121900930723     C* AGGIORNO CODICE ULTIMO SPEDIZIONIERE IMMESSO
122000930723     C                   MOVEL     COMSPE        SPENUO
122100930723     C                   OUT       SPEDTA
122200970804     C                   WRITE     FNSPE000
122300930723E   2C                   ENDIF
122400930723E   1C                   ENDIF
122500040622IF  1C     *IN14         IFEQ      *OFF
122600040622     C* LUOGHI/SPEDIZIONIERI
122700040622     C                   UPDATE    FNSP2000
122800040622     c                   else
122900040622     c                   move      specod        sp2cod
123000040622     C                   Z-ADD     speCLI        SP2CLI
123100040622     c                   move      'EM'          sp2tpe
123200040622     c                   move      *blank        sp2flg
123300040622     C                   WRITE     FNSP2000
123400040622     c                   endif
123500930723     C*
123600930723     C                   ENDSR
123700930727     C*---------------------------------------------------------------*
123800930727     C*                    CARTAB                                     *
123900020507     C* ROUTINE DI CARICAMENTO TABELLA*L3 l1                          *
124000930727     C*---------------------------------------------------------------*
124100930727     C     CARTAB        BEGSR
124200930727     C*
124300020507     C* TABELLA L1
124400930727     C                   Z-ADD     1             C                 2 0
124500020507     C                   CLEAR                   TR06DS
124600020507     C                   MOVEL     '£1'          D06COD
124700020507     C                   MOVEL     simfel        D06KEY
124800020507     C                   MOVEL     TR06DS        KPJBU
124900020507     C                   CALL      'TRUL06R'
125000020507     C                   PARM                    KPJBA
125100020507     C                   MOVEL     KPJBU         TR06DS
125200020507     C                   MOVEA     LIN           l1
125300020507     C*
125400020507     C*                  Z-ADD     SIMFEL        L3(1)
125500020507     C*                  MOVE      '£3'          COD
125600020507     C*                  MOVEL     *BLANKS       KEY
125700020507     C*                  MOVEL     SIMFEL        KEY
125800020507     C*    KTAB          CHAIN     TABEL00F                           30
125900020507IF  1C*    *IN30         IFEQ      *OFF
126000020507     C*    TBLKEY        ANDNE     *BLANKS
126100020507     C*                  MOVEA     TBLUNI        FIL
126200930727     C*
126300020507DO  2C*                  DO        29            B                 3 0
126400020507IF  3C*    FIL(B)        IFGT      *ZEROS
126500020507     C*                  MOVEL     FIL(B)        COMFIL            3 0
126600020507     C*    COMFIL        LOOKUP    L3                                     33
126700020507     C* N33              ADD       1             C
126800020507     C* N33              Z-ADD     COMFIL        L3(C)
126900020507E   3C*                  ENDIF
127000020507E   2C*                  ENDDO
127100020507E   1C*                  ENDIF
127200940901     C*
127300940901     C                   SETOFF                                       2840
127400960305     C*
127500960305     C* Tabella 2F
127600960305     C                   Z-ADD     0             §F                3 0
127700960305     C                   MOVEA     *BLANKS       C2F
127800960305     C                   MOVEA     *BLANKS       T2F
127900960305     C                   MOVE      '2F'          COD
128000960305     C     KTAB1         CHAIN     TABEL00F                           30
128100960305IF  1C     *IN30         DOWEQ     '0'
128200960305IF  1C     §F            ANDLT     100
128300960305     C     TBLFLG        IFEQ      *BLANKS
128400960305     C                   ADD       1             §F
128500960305     C                   MOVEL     TBLUNI        DS2F
128600960305     C                   MOVEL     TBLKEY        C2F(§F)
128700960305     C                   MOVEL     §2FFTP        T2F(§F)
128800960305     C                   END
128900960305     C     KTAB1         READE     TABEL00F                               30
129000960305     C                   END
129100960305     C*
129200960305     C* Tabella 2H
129300960305     C                   Z-ADD     0             §F                3 0
129400960305     C                   MOVEA     *BLANKS       C2H
129500960305     C                   MOVEA     *BLANKS       T2H
129600960305     C                   MOVE      '2H'          COD
129700960305     C     KTAB1         CHAIN     TABEL00F                           30
129800960305IF  1C     *IN30         DOWEQ     '0'
129900960305IF  1C     §F            ANDLT     100
130000960305     C     TBLFLG        IFEQ      *BLANKS
130100960305     C                   ADD       1             §F
130200960305     C                   MOVEL     TBLUNI        DS2H
130300960305     C                   MOVEL     TBLKEY        C2H(§F)
130400960305     C                   MOVEL     §2HFAX        T2H(§F)
130500960305     C                   END
130600960305     C     KTAB1         READE     TABEL00F                               30
130700960305     C                   END
130800930727     C*
130900930727     C                   ENDSR
131000961009     C*---------------------------------------------------------------*
131100961009     C*                    PGMRIC                                     *
131200961009     C* ROUTINE DI IMPOSTAZIONE CAMPI SE PGM RICHIAMATO               *
131300961009     C*---------------------------------------------------------------*
131400961009     C     PGMRIC        BEGSR
131500961009     C                   SETON                                        1213
131600961009     C                   MOVEL     INT(2)        VIDINT
131700961009     C                   MOVEL     DM6SPE        CODSPE
131800961009     C                   MOVE      DM6COD        CODCLI
131900961009     C* CHAINO TABELLA 4L = COD.LUOGHI
132000961009     C                   MOVEL     '4L'          COD
132100961009     C                   MOVEL(P)  CODSPE        KEY
132200961009     C     KTAB          CHAIN     TABEL00F                           30
132300961009     C  N30              MOVEL     TBLUNI        DS4L
132400961009     C   30              CLEAR                   DS4L
132500961009     C*
132600961009     C* CHAINO ANGRAFICA LUOGO CON CHIAVE PASSATA
132700970807     C                   MOVEL     'L'           KFLS
132800970804     C     KEYSPE        CHAIN     FNSPE01L                           10
132900040622     C     KEYSP2        CHAIN     FNSP201L                           14
133000961009     C* IMPOSTO CAMPI A VIDEO
133100961009    1C     *IN10         IFEQ      *ON
133200040715     C                   MOVEL     OPE(1)        VIDOPE
133300961009   X1C                   ELSE
133400961009     C*
133500040715     C                   MOVEL     OPE(2)        VIDOPE
133600961009E   1C                   ENDIF
133700961009     C*
133800961009     C* PER INSERIMENTO
133900961009     C                   MOVEL     *BLANKS       WSVTEL
134000040630     C                   MOVEL     *BLANKS       WSVmail          60
134100961009     C                   MOVEL     *BLANKS       WSVFAX
134200040715     C   14              MOVEL     *BLANKS       VIDmail
134300961009IF  1C     *IN10         IFEQ      *ON
134400961009     C                   MOVEL     *BLANKS       VIDRAG
134500961009     C                   MOVEL     *BLANKS       VIDIND
134600970804     C                   CLEAR                   VIDCAP
134700961009     C                   MOVEL     *BLANKS       VIDLOC
134800961009     C                   MOVEL     *BLANKS       VIDPRO
134900961009     C                   MOVEL     *BLANKS       VIDNAZ
135000961009     C                   MOVEL     *BLANKS       VIDIVA
135100961009     C                   MOVEL     *BLANKS       VIDTEL
135200961009     C                   MOVEL     *BLANKS       VIDTEX
135300961009     C                   MOVEL     *BLANKS       V4CFAX
135400961009     C                   MOVEL     *BLANKS       VIDRIV
135500961009     C*
135600961009     C* PER VARIAZIONE
135700961009X   1C                   ELSE
135800961009     C                   MOVEL     SPERAG        VIDRAG
135900961009     C                   MOVEL     SPEIND        VIDIND
136000970804     C                   MOVEL     SPECAP        VIDCAP
136100961009     C                   MOVEL     SPELOC        VIDLOC
136200961009     C                   MOVEL     SPEPRO        VIDPRO
136300961009     C                   MOVEL     SPENAZ        VIDNAZ
136400961009     C                   MOVEL     SPEPIV        CNT2              2
136500961009     C     CNT2          IFGT      *ZEROS
136600961009     C                   MOVEL     SPEPIV        VIVA
136700961009     C                   MOVEL     *BLANKS       EIVA
136800961009     C                   ELSE
136900961009     C                   MOVEL     SPEPIV        VIDIVA
137000961009     C                   END
137100961009     C                   MOVEL     SPETEL        VIDTEL
137200970804     C                   MOVEL     SPEFAX        V4CFAX
137300961009     C                   MOVEL     SPETEX        VIDTEX
137400961009     C                   MOVEL     SPEPER        VIDRIV
137500040622     C                   MOVEL     SP2est        VIDmail
137600961009E   1C                   ENDIF
137700961009     C*
137800961009     C                   MOVEL     CODSPE        CODSP2
137900961009     C*
138000961009     C                   SETOFF                                           28
138100961009     C                   SETOFF                                       444546
138200961009     C                   SETOFF                                       474849
138300970703     C                   SETOFF                                       505153
138400040622     C                   SETOFF                                       54
138500961009     C*
138600961009     C                   ENDSR
138700930720     C*---------------------------------------------------------------*
138800940901     C*                    *INZSR                                     *
138900930720     C* ROUTINE DI DEFINIZIONI VARIABILI, KLIST E PLIST               *
139000930720     C*---------------------------------------------------------------*
139100940901     C     *INZSR        BEGSR
139200930720     C*
139300930720     C     *ENTRY        PLIST
139400930720     C                   PARM                    KPJBA
139500961009     C                   MOVEL     KPJBU         DSC0M6
139600961009     C*
139700961009     C* 09 ON --> PGM RICHIAMATO
139800961009     C     DM6RIC        COMP      ' '                                0909
139900930720     C*
140000930720     C* RICERCA FILIALE IN GESTIONE
140100930720     C                   Z-ADD     1             CODUT
140200930720     C                   CALL      'X§PARUT'
140300930720     C                   PARM                    UT§DSE
140400930720     C                   MOVEL     REC80         CNCR80
140500930720     C                   MOVEL     RAGUT         RSUT             20
140600961112     C* 08 ON  - SONO IN FILIALE
140700961112     C     SIMFEL        IFGT      0
140800961112     C                   SETON                                        08
140900961112     C                   ENDIF
141000930727     C*
141100930727     C* RICERCA CAPOCONTI
141200930727DO  1C                   DO        50            X                 2 0
141300930727     C                   MOVE      TCU(X)        TCUDS
141400940901     C*  CLIENTI
141500940901     C     F56           IFEQ      'CG'
141600940901     C     F4            ANDEQ     '1'
141700940901     C     F3            ANDEQ     '0'
141800940901     C                   Z-ADD     KCU(X)        KCI               4 0
141900940901     C                   END
142000930727E   1C                   ENDDO
142100930720     C*
142200930720     C     *DTAARA       DEFINE                  SPEDTA
142300930720     C*
142400961111     C     *LIKE         DEFINE    V5CFIL        PARFIL
142500970709     C     *LIKE         DEFINE    VIDLOC        WVDLOC
142600970709     C     *LIKE         DEFINE    VIDPRO        WVDPRV
142700970709     C     *LIKE         DEFINE    VIDNAZ        WVDNAR
142800970709     C     *LIKE         DEFINE    VIDCAP        WVDCAP
142900970709     C     *LIKE         DEFINE    O13ERR        WERR
143000970709     C     *LIKE         DEFINE    O13MSG        WMSG
143100970709     C     *LIKE         DEFINE    VIDCAP        WSVCAP
143200970709     C     *LIKE         DEFINE    VIDLOC        WSVLOC
143300970710     C     *LIKE         DEFINE    VIDPRO        WSVPRO
143400970709     C     *LIKE         DEFINE    E13FZV        WFZV
143500970709     C     *LIKE         DEFINE    E13FZ1        WFZ1
143600970709     C     *LIKE         DEFINE    E13FZ2        WFZ2
143700970710     C     *LIKE         DEFINE    E13FZ3        WFZ3
143800970804     C     *LIKE         DEFINE    SPEFLS        KFLS
143900040622     C     *LIKE         DEFINE    sp2tpe        ktpe
144000930723     C* ANSPE00F
144100970804     C     KEYSPE        KLIST
144200970804     C                   KFLD                    KFLS
144300930720     C                   KFLD                    CODCLI
144400930720     C                   KFLD                    CODSPE
144500040622     C* fnsp2
144600040622     C     KEYSP2        KLIST
144700040622     C                   KFLD                    specli
144800040622     C                   KFLD                    specod
144900040622     C                   KFLD                    ktpe
145000040622     c                   move      'EM'          ktpe
145100930720     C* TABEL00F
145200930720     C     KTAB          KLIST
145300930720     C                   KFLD                    CODUT             1 0
145400930720     C                   KFLD                    COD               2
145500930723     C                   KFLD                    KEY               8
145600960305     C     KTAB1         KLIST
145700960305     C                   KFLD                    CODUT             1 0
145800960305     C                   KFLD                    COD               2
145900930727     C*
146000930727     C* CNACO00F
146100930727     C     KACO          KLIST
146200930727     C                   KFLD                    CODUT
146300930727     C                   KFLD                    KCI
146400930727     C                   KFLD                    KSC               7 0
146500980219     C* GIRO UDATE
146600980219     C                   TIME                    W0140            14 0
146700980219     C                   MOVE      W0140         WDTGIO            8 0
146800980219     C                   Z-ADD     WDTGIO        G02DAT
146900980219     C                   MOVEL     *BLANK        G02ERR
147000980219     C                   CALL      'XSRDA8'
147100980219     C                   PARM                    WLBDAT
147200980219     C                   MOVEL     G02INV        DATEU             8 0
147300941125     C*
147400930720     C                   ENDSR
147500930721     O*---------------------------------------------------------------*
147600930721     O*               *** INTESTAZIONE VIDEO ***                      *
147700930721     O*---------------------------------------------------------------*
147800961112**
147900930721***   ANAGRAFICA  SPEDIZIONIERI   ***
148000930721* ANAGRAFICA LUOGHI DI DESTINAZIONE *
148100930722**
148200930722IMMISSIONE
148300930722VARIAZIONE
148400940901**
148500970709E' MOMENTANEAMENTE Impossibile gestire l'anagrafica Spedizionieri             01
148600970709Scegliere l'anagrafica da manutenzionare                                      02
148700970709Codice Spedizioniere obbligatorio                                             03
148800970709Codice Spedizioniere errato                                                   04
148900970709Codice nuovo spedizioniere errato                                             05
149000970709Ragione sociale obbligatoria o inseriti caratteri non validi                  06
149100970709Indirizzo Obbligatorio o inseriti caratteri non validi                        07
149200970709C.A.P. Obbligatorio                                                           08
149300970709C.A.P. inesistente o incongruente con la provincia                            09
149400970709Località obbligatoria o inseriti caratteri non validi                         10
149500970709Provincia inesistente                                                         11
149600970709Provincia obbligatoria o inseriti caratteri non validi                        12
149700970709Partita IVA formalmente errata                                                13
149800970709Codice Luogo obbligatorio                                                     14
149900970709Codice Luogo Errato o non utilizzabile                                        15
150000970709Il Codice cliente deve essere diverso da 9999999                              16
150100970709Codice cliente obbligatorio                                                   17
150200970709Cliente Inesistente o non gestibile                                           18
150300970709Per la ricerca dei luoghi inserire il codice cliente                          19
150400970709Luogo presente in tab.3D-Cod.clienti ordini ritiro: annull.non ammesso        20
150500970709Immettere punto operativo valido                                              21
150600970710Attenzione !! Modificata Località                                             22
150700970710Attenzione !! Modificato CAP                                                  23
150800970710Attenzione !! Modificata Provincia                                            24
150900970710Attenzione !! Modificata Nazione                                              25
151000980219Nazione inesistente                                                           26
151100000630Cliente non gestibile con 9999 o 8888                                         27
151200960412**
151300960412!:§
151400961112**         CMD1
151500961112CHKOBJ OBJ(FNCLS01L) OBJTYPE(*FILE)
