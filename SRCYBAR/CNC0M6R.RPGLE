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
010900151021     D*---------------------------------------------------------------*
011000151021     d wforzaiva       s              1    inz(*Off)
011100930720     D*---------------------------------------------------------------*
011200930720     D* DS                                                            *
011300930720     D*---------------------------------------------------------------*
011400930720     D*-----------------
011500930720     D* PARAMETRI ESTERNI
011600930720     D*-----------------
011700930720     D KPJBA         E DS
011800930720     D  LIBSYS                92    100
011900961009     D DSC0M6          DS
012000961009     D* Cod.luogo
012100961009     D  DM6SPE                 1      3
012200961009     D* Codice cliente
012300961009     D  DM6COD                 4     10  0
012400961009     D* ric = 'S' -> pgm richiamato
012500961009     D  DM6RIC                11     11
012600930720     D*-----------------
012700930721     D* NUMERATORE NUOVI SPEDIZIONIERI
012800930720     D*-----------------
012900930720     D SPEDTA          DS
013000930720     D  SPENUO                 1      3  0
013100930720     D*------------------
013200930720     D* DS RICERCA PARAMETRI UTENTE
013300930720     D*------------------
013400930720     D CNCR80        E DS
013500151021     D XCFIVA1DS     E DS
013600151021     d  ivaeu                  3      4
013700151021     d  ivait                  5     18
013800960305     D DS2F          E DS
013900960305     D DS2H          E DS
014000960305     D DS4L          E DS
014100010706     D**!!! DS3D          E DS
014200980219     D DS15          E DS
014300020507     D TR06DS        E DS                  EXTNAME(TRUL06DS)
014400020507     D  LIN                    1     90  0 dim(30)
014500930720     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
014600930727     D  TCU                  398    697
014700930727     D                                     DIM(50)
014800930727     D  KCU                  698    847P 0
014900930727     D                                     DIM(50)
015000930727     D                                     PACKEVEN
015100930727     D*------------------
015200930727     D* DS RICERCA CAPOCONTO
015300930727     D*------------------
015400930727     D TCUDS           DS
015500930727     D  F1                     1      1
015600930727     D  F3                     3      3
015700930727     D  F2                     2      2
015800930727     D  F4                     4      4
015900930727     D  F56                    5      6
016000960305     D*------------------
016100960305     D* DS x scomposzione clsflo
016200960305     D*------------------
016300960305     D                 DS
016400960305     D  WCHIUS                 1      1
016500960305     D  WAPERT                10     10
016600960305     D  CLSFLO                 1     10
016700990312     D*------------------
016800990312     D* DS x scomposzione KNSIF
016900990312     D*------------------
017000990312     D                 DS
017100990312     D  WSITST                 7      7
017200990312     D  WSITRE                 7     10
017300990312     D  WKNSIF                 1     10
017400960304     D* Ds esterna x richiamo pgm controllo fax
017500960304     D TRUL42        E DS                  EXTNAME(TRUL42DS)
017600040622     D dsemail       E DS
017700940901     D VIDIVA          DS
017800940901     D  EIVA                   1      2
017900940901     D  VIVA                   3     16
018000970709     D* PARAMETRI PASSATI AL PGM CONTROLLO CAP
018100970709     D FNLV13        E DS                  EXTNAME(FNLV13DS)
018200970703     D* PARAMETRI PASSATI AL PGM CONTROLLO CAP TISI95R
018300970703     D TISI95        E DS                  EXTNAME(TISI95DS)
018400970711     D TISI00        E DS                  EXTNAME(TISI00DS)
018500980219     D WLBDAT          DS                  INZ
018600980219     D  G02DAT                 1      8  0
018700980219     D  G02INV                 9     16  0
018800980219     D  G02ERR                17     17
018900980219     D  G02TGI                18     22  0
019000930720     C*---------------------------------------------------------------*
019100930720     C*                    MAIN                                       *
019200930720     C* PROGRAMMA DI MANUTENZIONE ANAGRAFICA SPEDIZIONIERI/LUOGHI     *
019300930720     C*---------------------------------------------------------------*
019400990312     C***
019500990312     C* IL PROGRAMMA è ESEGUIBILE SOLO SDI, BARTOLINI ED IN TEST
019600990312     C***
019700990312     C                   MOVEL     KNSIF         WKNSIF
019800990312     C     WSITRE        IFNE      '101'
019900990312     C     WSITRE        ANDNE     '201'
020000990312     C     WSITST        ANDNE     'P'
020100990312     C                   GOTO      FINE
020200990312     C                   ENDIF
020300961112     C***
020400961112     C* CONTROLLO ESISTENZA FILE FNCLS PRIMA DI APRIRLO (CHKOBJ)
020500961112     C***
020600961112     C                   SETOFF                                       15
020700961112     C                   Z-ADD     35            LUNG             15 5
020800961112     C                   MOVEL     *BLANKS       COMMAN           80
020900961112     C                   MOVEA     CMD1(1)       COMMAN
021000961112     C                   CALL      'QCMDEXC'                            15
021100961112     C                   PARM                    COMMAN
021200961112     C                   PARM                    LUNG
021300961112     C  N15              OPEN      FNCLS01L
021400930720     C*
021500930727     C* CARICAMENTO TABELLA FILIALI IN GESTIONE
021600930727     C                   EXSR      CARTAB
021700961009     C***
021800961009     C*  PGM RICHIAMATO
021900961009     C***
022000961009     C     *IN09         IFEQ      *ON
022100961009     C                   EXSR      PGMRIC
022200961009     C* Vado direttamente alla videata di immissione dati
022300970820     C     DM6SPE        IFEQ      *BLANKS
022400970820     C     DM6COD        OREQ      0
022500970820     C                   GOTO      FMTD2
022600970820     C                   ELSE
022700970820     C                   GOTO      FMTD3
022800961009     C                   END
022900970820     C                   END
023000930720     C*
023100930721     C* RICHIESTA ANAGRAFICA DA MANUTENZIONARE
023200930720     C     INIZIO        TAG
023300041122     C**!!!              EXFMT     C006D1
023400930721     C*
023500930721     C* CMD3 - FINE LAVORO
023600041122     C*! KC              GOTO      FINE
023700930721     C*
023800930721     C* CONTROLLO DATI PRIMO FORMATO VIDEO
023900041122     c                   Eval      speluo = 'S'
024000930721     C                   EXSR      CONTR1
024100930721     C*
024200930721     C* ERRORE: RIEMETTO LA VIDEATA
024300930721     C   90              GOTO      INIZIO
024400930721     C*
024500930721     C* RICHIESTA CODICI
024600930722     C                   MOVEL     *BLANKS       DESSPE
024700930723     C                   MOVEL     *BLANKS       CODSPE
024800930723     C                   MOVEL     *BLANKS       CODCLI
024900961112     C   12
025000961112     CAN 08              MOVEL     999           V5CFIL
025100961112     C   12
025200961112     CANN08              CLEAR                   V5CFIL
025300940901     C                   SETOFF                                           43
025400940901     C                   SETOFF                                       284142
025500930721     C     FMTD2         TAG
025600961112     C   12
025700961112     CANN52              MOVEL     *BLANKS       VCODL
025800961111     C   12              EXFMT     C006D5
025900930723     C     FMTD2B        TAG
026000930723     C*
026100930721     C  N12              EXFMT     C006D3
026200930721     C*
026300930721     C* CMD3 - FINE LAVORO
026400930721     C   KC              GOTO      FINE
026500930721     C*
026600930721     C* CM12 - RITORNO VIDEATA PRECEDENTE
026700940901     C   KL              SETOFF                                       2840
026800970820     C   KL*IN09         IFEQ      *ON
026900970820     C                   GOTO      FINE
027000970820     C                   ELSE
027100970820     C                   GOTO      INIZIO
027200970820     C                   END
027300930721     C*
027400930721     C* CONTROLLO DATI SECONDO FORMATO VIDEO
027500930721     C                   EXSR      CONTR2
027600930721     C*
027700930721     C* ERRORE: RIEMETTO LA VIDEATA
027800930723     C   90
027900930723     CAN 12              GOTO      FMTD2
028000930723     C   90
028100930723     CANN12              GOTO      FMTD2B
028200930722     C*
028300930722     C* RICHIESTA DATI
028400940901     C                   SETOFF                                           28
028500940901     C                   SETOFF                                       444546
028600940901     C                   SETOFF                                       474849
028700970703     C                   SETOFF                                       505153
028800040622     C                   SETOFF                                       54
028900930722     C     FMTD3         TAG
029000930722     C                   EXFMT     C006D4
029100930722     C*
029200930723     C* CMD3 - FINE LAVORO
029300930722     C   KC              GOTO      FINE
029400930722     C*
029500930722     C* CM12 - RITORNO VIDEATA PRECEDENTE
029600961009     C*        (se pgm richiamato vado a fine)
029700940901     C                   SETOFF                                           43
029800940901     C                   SETOFF                                       284142
029900961009     C   KL
030000961009     CAN 09              GOTO      FINE
030100930723     C   KL
030200930723     CAN 12              GOTO      FMTD2
030300930723     C   KL
030400930723     CANN12              GOTO      FMTD2B
030500930722     C*
030600930722     C* CONTROLLO DATI TERZO FORMATO VIDEO
030700930722     C                   EXSR      CONTR3
030800930722     C*
030900930722     C* ERRORE: RIEMETTO LA VIDEATA
031000930722     C   90              GOTO      FMTD3
031100930723     C*
031200930723     C* CMD6 - AGGIORNAMENTO
031300930723     C   KF              EXSR      AGGIO
031400940901     C   KF              SETOFF                                       2840
031500961009     C   KF
031600961009     CAN 09              GOTO      FINE
031700930723     C   KF              GOTO      INIZIO
031800930723     C*
031900930723     C* CM16 - ANNULLAMENTO
032000961011     C* Se annullamento di luogo controllo che non sia presente in
032100961011     C* tabella 3D
032200010706     C**!!!   KQ
032300010706     C**!!!AN 12              EXSR      CTR3D
032400010706     C**!!!   KQ
032500010706     C**!!!AN 12
032600010706     C**!!!AN 90              GOTO      FMTD3
032700930723     C   KQ              EXSR      ANNUL
032800940901     C   KQ              SETOFF                                       2840
032900961009     C   KQ
033000961009     CAN 09              GOTO      FINE
033100961009     C   KQ              GOTO      INIZIO
033200930721     C*
033300930723     C* ENTER - RIMANE NELLA VIDEATA
033400930723     C                   GOTO      FMTD3
033500930723     C*
033600930721     C     FINE          TAG
033700930720     C                   SETON                                        LR
033800930721     C*---------------------------------------------------------------*
033900930721     C*                    CONTR1                                     *
034000930727     C* ROUTINE DI CONTROLLO DATI PRIMO FORMATO VIDEO                 *
034100930721     C*---------------------------------------------------------------*
034200930721     C     CONTR1        BEGSR
034300930721     C*
034400930723     C                   SETOFF                                       901213
034500940901     C                   SETOFF                                       2840
034600930721     C*
034700930721     C* ANAGRAFICA DA MANUTENZIONARE
034800930721IF  1C     SPELUO        IFEQ      *BLANKS
034900940901     C                   SETON                                        409028
035000940901     C                   MOVEL     ERR(2)        $MSG
035100930721     C                   GOTO      ENDCT1
035200930721E   1C                   ENDIF
035300930722     C*
035400930722     C* 12 ON  - SCELTA ANAGRAFICA LUOGHI
035500930722     C* 12 OFF - SCELTA ANAGRAFICA SPEDIZIONIERI
035600930722     C*
035700930722IF  1C     SPELUO        IFEQ      'L'
035800930723     C                   SETON                                        1213
035900930722E   1C                   ENDIF
036000930722     C*
036100930723     C* SPEDIZIONIERI
036200930722IF  1C     *IN12         IFEQ      *OFF
036300930722     C*
036400930722     C* LETTURA CODICE ULTIMO SPEDIZIONIERE
036500930722     C     *LOCK         IN        SPEDTA                               90
036600930722     C*
036700930722     C* SE L'ALLOCAZIONE NON RIESCE RITENTO ALTRE 5 VOLTE
036800930722IF  2C     *IN90         IFEQ      *ON
036900930722     C                   Z-ADD     *ZEROS        I
037000930722DO  3C     *IN90         DOWEQ     *ON
037100930722     C     I             ANDLE     5
037200930722     C                   ADD       1             I                 1 0
037300930722     C     *LOCK         IN        SPEDTA                               90
037400930722E   3C                   ENDDO
037500930722E   2C                   ENDIF
037600930722     C*
037700930722     C* ALLOCAZIONE NON RIUSCITA: EMETTO UN MESSAGGIO
037800930722IF  2C     *IN90         IFEQ      *ON
037900940901     C                   SETON                                        4028
038000940901     C                   MOVEL     ERR(1)        $MSG
038100930722     C                   GOTO      INIZIO
038200930722E   2C                   ENDIF
038300930722     C*
038400930722     C* PRIMO CODICE SPEDIZIONIERE LIBERO
038500930722     C                   ADD       1             SPENUO
038600930723E   1C                   ENDIF
038700930721     C*
038800930721     C     ENDCT1        ENDSR
038900930722     C*---------------------------------------------------------------*
039000930722     C*                    CONTR2                                     *
039100930722     C* ROUTINE DI CONTROLLO DATI SECONDO FORMATO VIDEO               *
039200930722     C*---------------------------------------------------------------*
039300930722     C     CONTR2        BEGSR
039400930722     C*
039500930722     C                   SETOFF                                       900102
039600040715     C                   SETOFF                                       105243
039700940901     C                   SETOFF                                       284142
039800930722     C                   Z-ADD     *ZEROS        COMSPE            3 0
039900020507     C                   Z-ADD     *ZEROS        COMFIL            3 0
040000930722     C*
040100930722     C* L U O G H I
040200930722     C*
040300930722IF  1C     *IN12         IFEQ      *ON
040400940502     C*
040500940502     C* RICHIESTA INTERROGAZIONE TABELLA CODICI LUOGHI UTILIZZABILI
040600940412IF  2C     VCODL         IFEQ      'L'
040700940412     C                   MOVEL     '4L'          COD
040800940412     C                   MOVEL     *BLANKS       KEY
040900940412     C                   MOVEL     *BLANKS       DES
041000940412     C                   CALL      'X§TABER'
041100940412     C                   PARM                    CODUT
041200940412     C                   PARM                    COD
041300940412     C                   PARM                    KEY
041400940412     C                   PARM                    DES              25
041500940412     C*
041600940412     C                   MOVEL     KEY           CODSPE
041700940412     C                   SETON                                        90
041800940412     C                   GOTO      ENDCT2
041900940412E   2C                   ENDIF
042000940502     C*
042100940502     C* RICHIESTI TUTTI I CODICI LUOGO PER STESSO CLIENTE
042200940412IF  2C     VCODL         IFEQ      'S'
042300940412     C*
042400940412     C* PER LA RICERCA IL CODICE CLIENTE DEVE ESSERE INSERITO
042500000630IF  3C     CODCLI        IFEQ      *ZEROS
042600940901     C                   SETON                                        904228
042700940901     C                   MOVEL     ERR(19)       $MSG
042800940412     C                   GOTO      ENDCT2
042900000630      *
043000000630X   3C                   ELSE
043100000630      *
043200000630     C                   MOVE      CODCLI        COD4              4 0
043300000630IF  4C     COD4          IFEQ      *ALL'9'
043400000630IF  4C     COD4          OREQ      *ALL'8'
043500000630     C                   SETON                                        904228
043600000630     C                   MOVEL     ERR(27)       $MSG
043700000630     C                   GOTO      ENDCT2
043800000630E   4C                   END
043900000630E   3C                   ENDIF
044000940412     C*
044100041201     C*******!!!!        MOVEL     *ZEROS        CODSPE
044200041201     C*******!!!!        CLEAR                   PARFIL
044300041201     C*******!!!!        CALL      'CNC0M2R'
044400041201     C*******!!!!        PARM                    CODCLI
044500041201     C*******!!!!        PARM                    CODSPE
044600041201     C*******!!!!        PARM                    PARFIL
044700041201     C*******!!!!        SETON                                            90
044800041201     C*******!!!!        GOTO      ENDCT2
044900940412E   2C                   ENDIF
045000930722     C*
045100930722     C* CODICE LUOGO OBBLIGATORIO
045200930722IF  2C     CODSPE        IFEQ      *BLANKS
045300930722     C     CODSPE        OREQ      *ZEROS
045400940901     C                   SETON                                        904128
045500940901     C                   MOVEL     ERR(14)       $MSG
045600930722     C                   GOTO      ENDCT2
045700930722X   2C                   ELSE
045800940502     C*
045900940502     C* RICHIESTA INTERROGAZIONE DI TUTTI I CLIENTI CON STESSO CODICE LUOGO
046000940412IF  3C     VCODL         IFEQ      'C'
046100961112     C*
046200961111     C* SE RICHIESTI I CODICI DI UN SOLO PUNTO OPERATIVO CONTROLLO CHE
046300961111     C*   SIA VALIDO
046400961112    4C     *IN08         IFEQ      *ON                                          *FILIALE
046500961112     C     V5CFIL        ANDNE     0
046600961112     C     V5CFIL        ANDNE     999
046700961112     C     *IN08         OREQ      *OFF                                         *SEDE
046800961112     C     V5CFIL        ANDNE     0
046900961111     C     V5CFIL        CHAIN     AZORG01L                           35
047000961112    5C     *IN35         IFEQ      *ON
047100961111     C     ORGFVA        ORNE      ' '
047200961111     C                   SETON                                        905228
047300961111     C                   MOVEL     ERR(21)       $MSG
047400961111     C                   GOTO      ENDCT2
047500961111    5C                   ENDIF
047600961112    4C                   ENDIF
047700961111     C*
047800940412     C                   MOVEL     *ZEROS        CODCLI
047900961111     C                   MOVEL     V5CFIL        PARFIL
048000940412     C                   CALL      'CNC0M2R'
048100940412     C                   PARM                    CODCLI
048200940412     C                   PARM                    CODSPE
048300961111     C                   PARM                    PARFIL
048400940412     C                   SETON                                            90
048500940412     C                   GOTO      ENDCT2
048600940412E   3C                   ENDIF
048700930722     C*
048800930722     C* VOGLIO SOLO DEI VALORI NUMERICI
048900930726IF  3C                   TESTN                   CODSPE               02  02
049000930722     C     *IN02         IFEQ      *OFF
049100940901     C                   SETON                                        904128
049200940901     C                   MOVEL     ERR(15)       $MSG
049300930723     C                   GOTO      ENDCT2
049400930722E   3C                   ENDIF
049500940502     C*
049600940502     C* CONTROLLO CODICE LUOGO IN TABELLA
049700940412     C                   MOVEL     '4L'          COD
049800940412     C                   MOVEL     *BLANKS       KEY
049900940412     C                   MOVEL     CODSPE        KEY
050000940412     C     KTAB          CHAIN     TABEL00F                           30
050100940412IF  3C     *IN30         IFEQ      *ON
050200940901     C                   SETON                                        904128
050300940901     C                   MOVEL     ERR(15)       $MSG
050400940412     C                   GOTO      ENDCT2
050500940412     C                   ELSE
050600940413     C                   MOVEL     TBLUNI        DS4L
050700940502     C*
050800940413     C* NON UTILIZZABILE IN FILIALE
050900940413     C     SIMFEL        IFNE      *ZEROS
051000940413     C     §4LUFI        ANDNE     'S'
051100940901     C                   SETON                                        904128
051200940901     C                   MOVEL     ERR(15)       $MSG
051300940413     C                   GOTO      ENDCT2
051400940413     C                   END
051500940413     C                   MOVEL     §4LDES        VDESLU
051600940412E   3C                   ENDIF
051700930722E   2C                   ENDIF
051800930722     C*
051900930722     C* CODICE CLIENTE OBBLIGATORIO
052000930722IF  2C     CODCLI        IFEQ      *ZEROS
052100940901     C                   SETON                                        904228
052200940901     C                   MOVEL     ERR(17)       $MSG
052300930722     C                   GOTO      ENDCT2
052400930722X   2C                   ELSE
052500000630     C*
052600000630     C*  CLIENTI GENERICI BLOCCATO INSERIMENTO  -----
052700000630     C                   MOVE      CODCLI        COD4              4 0
052800000630IF  4C     COD4          IFEQ      *ALL'9'
052900000630IF  4C     COD4          OREQ      *ALL'8'
053000000630     C                   SETON                                        904228
053100000630     C                   MOVEL     ERR(27)       $MSG
053200000630     C                   GOTO      ENDCT2
053300000630E   4C                   END
053400000630     C*  -----------------------------------
053500930722     C*
053600930722     C* CODICE CLIENTE DIVERSO DA 9999999
053700930722IF  3C     CODCLI        IFEQ      9999999
053800940901     C                   SETON                                        904228
053900940901     C                   MOVEL     ERR(16)       $MSG
054000930722     C                   GOTO      ENDCT2
054100930722E   3C                   ENDIF
054200930727     C*
054300930727     C* CODICE CLIENTE PRESENTE NEL P.D.C.
054400930727     C                   Z-ADD     CODCLI        KSC
054500930727     C     KACO          CHAIN     CNACO00F                           32
054600930727IF  3C     *IN32         IFEQ      *OFF
054700930727     C*
054800930727     C* CODICE CLIENTE ANNULLATO
054900930727IF  4C     ACOFLG        IFNE      *BLANKS
055000930727     C                   SETON                                        32
055100930727X   4C                   ELSE
055200930727     C*
055300931215     C* CODICE CLIENTE IN GESTIONE ALLA FILIALE (IN SEDE LI POSSO GESTIRE TUTTI)
055400931215IF  5C     SIMFEL        IFNE      *ZEROS
055500930727     C                   MOVEL     CODCLI        COMFIL
055600020507     C     COMFIL        LOOKUP    L1                                     33
055700990312     C  N33              SETON                                        32
055800990312X   5C                   ELSE
055900990312      * P.O. CLIENTE 46 GESTIBILE SOLO DA SISTEMA INFORMATIVO 201
056000990312     C                   MOVEL     CODCLI        POCLI             3 0
056100990312IF  6C     POCLI         IFEQ      46
056200990312     C     WSITRE        ANDNE     '201'
056300990312     C                   SETON                                        32
056400931215E   6C                   ENDIF
056500990312E   5C                   ENDIF
056600990312      *
056700930727E   4C                   ENDIF
056800930727E   3C                   ENDIF
056900930727     C*
057000930727     C* CODICE CLIENTE INESISTENTE O NON GESTIBILE
057100930727IF  3C     *IN32         IFEQ      *ON
057200940901     C                   SETON                                        904228
057300940901     C                   MOVEL     ERR(18)       $MSG
057400930727     C                   GOTO      ENDCT2
057500930727E   3C                   ENDIF
057600930722E   2C                   ENDIF
057700930722     C*
057800930722     C* INTESTAZIONE A VIDEO
057900930722     C                   MOVEL     INT(2)        VIDINT
058000930722     C*
058100930722     C* CONTROLLO ESISTENZA RECORD
058200970804     C                   MOVEL     'L'           KFLS
058300970804     C     KEYSPE        CHAIN     FNSPE01L                           10
058400040622     C     KEYSP2        CHAIN     FNSP201L                           14
058500930722     C*
058600930722     C* S P E D I Z I O N I E R I
058700930722     C*
058800930722X   1C                   ELSE
058900930722     C*
059000930722     C* CODICE SPEDIZIONIERE OBBLIGATORIO
059100930722IF  2C     DESSPE        IFEQ      *BLANKS
059200930722     C     DESSPE        OREQ      *ZEROS
059300930722     C*
059400930722IF  3C     CODSPE        IFEQ      *BLANKS
059500930722     C     CODSPE        OREQ      *ZEROS
059600940901     C                   SETON                                        439028
059700940901     C                   MOVEL     ERR(3)        $MSG
059800930722     C                   GOTO      ENDCT2
059900930722E   3C                   ENDIF
060000930722E   2C                   ENDIF
060100930722     C*
060200930722     C* RICHIESTA RICERCA SPEDIZIONIERI PER RAGIONE SOCIALE
060300930722IF  2C     DESSPE        IFNE      *BLANKS
060400930723     C     DESSPE        ANDNE     *ZEROS
060500930723     C     CODSPE        ANDEQ     *BLANKS
060600930723     C     CODSPE        OREQ      *ZEROS
060700930722     C*
060800930722     C                   MOVEL     *ZEROS        CODSPE
060900930722     C                   MOVEL     9999999       CODCLI
061000930722     C                   CALL      'CNC0M1R'
061100930722     C                   PARM                    CODCLI
061200930722     C                   PARM                    CODSPE
061300930722     C                   PARM                    DESSPE
061400930722     C                   SETON                                            90
061500930722     C                   GOTO      ENDCT2
061600930722E   2C                   ENDIF
061700930722     C*
061800930722     C* VOGLIO SOLO DEI VALORI NUMERICI
061900930726     C                   TESTN                   CODSPE               02  02
062000930723IF  2C     *IN02         IFEQ      *OFF
062100940901     C                   SETON                                        904328
062200940901     C                   MOVEL     ERR(4)        $MSG
062300930723     C                   GOTO      ENDCT2
062400930722E   2C                   ENDIF
062500930722     C*
062600930722     C* INTESTAZIONE A VIDEO
062700930722     C                   MOVEL     INT(1)        VIDINT
062800930722     C*
062900930722     C* CONTROLLO ESISTENZA RECORD
063000970804     C                   MOVEL     'S'           KFLS
063100930722     C                   Z-ADD     9999999       CODCLI
063200970804     C     KEYSPE        CHAIN     FNSPE01L                           10
063300040630     C     KEYSP2        CHAIN     FNSP201L                           14
063400930722     C*
063500930722     C* SE INESISTENTE IL CODICE DEVE ESSERE QUELLO DELLA DTAARA
063600930722IF  2C     *IN10         IFEQ      *ON
063700930722     C                   MOVEL     CODSPE        COMSPE
063800930723IF  3C     COMSPE        IFNE      SPENUO
063900940901     C                   SETON                                        904328
064000940901     C                   MOVEL     ERR(5)        $MSG
064100930722     C                   GOTO      ENDCT2
064200930722E   3C                   ENDIF
064300930722E   2C                   ENDIF
064400930722E   1C                   ENDIF
064500930722     C*
064600930722     C* I M P O S T A Z I O N I  CAMPI A VIDEO
064700930722     C*
064800930722     C* INSERIMENTO
064900930722IF  1C     *IN10         IFEQ      *ON
065000040715     C                   MOVEL     OPE(1)        VIDOPE
065100930722X   1C                   ELSE
065200040715     C                   MOVEL     OPE(2)        VIDOPE
065300040715E   1C                   ENDIF
065400930722     C*
065500930722     C* PER INSERIMENTO
065600960305     C                   MOVEL     *BLANKS       WSVTEL
065700040622     C                   MOVEL     *BLANKS       WSVmail
065800960305     C                   MOVEL     *BLANKS       WSVFAX
065900930722IF  1C     *IN10         IFEQ      *ON
066000930722     C                   MOVEL     *BLANKS       VIDRAG
066100930722     C                   MOVEL     *BLANKS       VIDIND
066200970804     C                   CLEAR                   VIDCAP
066300930722     C                   MOVEL     *BLANKS       VIDLOC
066400930722     C                   MOVEL     *BLANKS       VIDPRO
066500930722     C                   MOVEL     *BLANKS       VIDNAZ
066600930722     C                   MOVEL     *BLANKS       VIDIVA
066700930722     C                   MOVEL     *BLANKS       VIDTEL
066800930722     C                   MOVEL     *BLANKS       VIDTEX
066900960305     C                   MOVEL     *BLANKS       V4CFAX
067000930722     C                   MOVEL     *BLANKS       VIDRIV
067100040901     C                   MOVEL     *BLANKS       VIDmail
067200930722     C*
067300930722     C* PER VARIAZIONE
067400930722X   1C                   ELSE
067500930722     C                   MOVEL     SPERAG        VIDRAG
067600930722     C                   MOVEL     SPEIND        VIDIND
067700970804     C                   MOVEL     SPECAP        VIDCAP
067800930722     C                   MOVEL     SPELOC        VIDLOC
067900930722     C                   MOVEL     SPEPRO        VIDPRO
068000930722     C                   MOVEL     SPENAZ        VIDNAZ
068100940901     C                   MOVEL     SPEPIV        CNT2              2
068200940901     C     CNT2          IFGT      *ZEROS
068300940901     C                   MOVEL     SPEPIV        VIVA
068400940901     C                   MOVEL     *BLANKS       EIVA
068500940901     C                   ELSE
068600940901     C                   MOVEL     SPEPIV        VIDIVA
068700940901     C                   END
068800930722     C                   MOVEL     SPETEL        VIDTEL
068900970804     C                   MOVEL     SPEFAX        V4CFAX
069000930722     C                   MOVEL     SPETEX        VIDTEX
069100930722     C                   MOVEL     SPEPER        VIDRIV
069200040715     C  n14              MOVEL     SP2est        VIDmail
069300930722E   1C                   ENDIF
069400930723     C*
069500930723     C   12              MOVEL     CODSPE        CODSP2
069600930723     C  N12              Z-ADD     9999999       CODCLI
069700970709     C* PULISCO  CAMPI DI SALVATAGGIO FNLV13
069800970709     C                   CLEAR                   WSVCAP
069900970709     C                   CLEAR                   WSVLOC
070000970709     C                   CLEAR                   WFZV
070100970709     C                   CLEAR                   WFZ1
070200970709     C                   CLEAR                   WFZ2
070300930722     C*
070400930722     C     ENDCT2        ENDSR
070500930722     C*---------------------------------------------------------------*
070600930722     C*                    CONTR3                                     *
070700930722     C* ROUTINE DI CONTROLLO DATI TERZO FORMATO VIDEO                 *
070800930722     C*---------------------------------------------------------------*
070900930722     C     CONTR3        BEGSR
071000930722     C*
071100940901     C                   SETOFF                                       900128
071200940901     C                   SETOFF                                       444546
071300940901     C                   SETOFF                                       474849
071400970703     C                   SETOFF                                       505153
071500040622     C                   SETOFF                                       54
071600930723     C*
071700930723     C* RAGIONE SOCIALE OBBLIGATORIA
071800930723IF  1C     VIDRAG        IFEQ      *BLANKS
071900940901     C                   SETON                                        904428
072000940901     C                   MOVEL     ERR(6)        $MSG
072100930723     C                   GOTO      ENDCT3
072200930723E   1C                   ENDIF
072300960412I    C* CONTROLLO CHE RAG.SOCIALE CONTENGA SOLO DATI VALIDI
072400960412     C                   Z-ADD     1             XX
072500960412     C     XX            DOWLE     10
072600960412     C     CAR(XX)       ANDNE     *BLANKS
072700960412     C     CAR(XX)       SCAN      VIDRAG                                 96
072800960412     C     *IN96         IFEQ      *ON
072900960412     C                   Z-ADD     10            XX
073000960412     C                   SETON                                        904428
073100960412     C                   MOVEL     ERR(6)        $MSG
073200960412     C                   ENDIF
073300960412     C                   ADD       1             XX
073400960412E    C                   ENDDO
073500960412     C   44              GOTO      ENDCT3
073600930723     C*
073700930723     C* INDIRIZZO OBBLIGATORIO
073800930723IF  1C     VIDIND        IFEQ      *BLANKS
073900940901     C                   SETON                                        904528
074000940901     C                   MOVEL     ERR(7)        $MSG
074100930723     C                   GOTO      ENDCT3
074200930723E   1C                   ENDIF
074300960412I    C* CONTROLLO CHE INDIRIZZO   CONTENGA SOLO DATI VALIDI
074400960412     C                   Z-ADD     1             XX
074500960412     C     XX            DOWLE     10
074600960412     C     CAR(XX)       ANDNE     *BLANKS
074700960412     C     CAR(XX)       SCAN      VIDIND                                 96
074800960412     C     *IN96         IFEQ      *ON
074900960412     C                   Z-ADD     10            XX
075000960412     C                   SETON                                        904528
075100960412     C                   MOVEL     ERR(7)        $MSG
075200960412     C                   ENDIF
075300960412     C                   ADD       1             XX
075400960412E    C                   ENDDO
075500960412     C   45              GOTO      ENDCT3
075600930727     C*
075700930727     C* LOCALITA OBBLIGATORIA
075800930727IF  1C     VIDLOC        IFEQ      *ZEROS
075900930727     C     VIDLOC        OREQ      *BLANKS
076000940901     C                   SETON                                        904728
076100940901     C                   MOVEL     ERR(10)       $MSG
076200930727     C                   GOTO      ENDCT3
076300980219    1C                   END
076400980219     C* CONTROLLO NAZIONE
076500980219     C                   MOVEL     '15'          COD
076600980219     C                   MOVEL(P)  VIDNAZ        KEY
076700980219     C     KTAB          CHAIN     TABEL                              30
076800980219     C  N30TBLFLG        IFNE      ' '
076900980219     C                   SETON                                        30
077000980219     C                   END
077100980219     C*
077200980219    1C     *IN30         IFEQ      *ON
077300980219     C                   MOVEL     ERR(26)       $MSG
077400980220     C                   SETON                                        905328
077500980219     C                   GOTO      ENDCT3
077600980219    1C                   ENDIF
077700980219     C*
077800980219     C                   MOVEL     TBLUNI        DS15
077900930727     C*
078000930727     C* RICHIESTA RICERCA LOCALITA
078100980219     C* (SOLO SE NAZIONE CON CAPPARIO)
078200980219     C     §15CPP        IFEQ      *BLANKS
078300930727     C                   MOVEL     VIDLOC        PUNT              1
078400930727IF  2C     PUNT          IFEQ      '?'
078500930727     C                   MOVE      VIDLOC        VIDL19           19
078600930727     C*
078700930727     C* SE IMMESSO SOLO '?' E NESSUNA LETTERA SEGUENTE SI PARTE DA 'A'
078800930727IF  3C     VIDL19        IFEQ      *BLANKS
078900930727     C                   MOVEL     'A'           VIDL19
079000930727E   3C                   ENDIF
079100970711     C* RICHIAMO L'INTERROGAZIONE
079200970711     C                   CLEAR                   TISI00
079300970711     C                   MOVEL     'L01'         D00OP0
079400970711     C                   MOVEL     VIDL19        D00LOC
079500980219     C                   Z-ADD     DATEU         D00DRI
079600970711     C                   CALL      'TISI30R'
079700970711     C                   PARM                    KPJBA
079800970711     C                   PARM                    TISI00
079900970711     C* SE SELEZIONE
080000970711    5C     D00CAP        IFNE      *BLANKS
080100970711     C                   MOVEL     D00CAP        VIDCAP
080200970711     C                   MOVEL     D00LOC        VIDLOC
080300970711     C                   MOVEL     D00NAR        VIDNAZ
080400970711     C                   MOVEL     D00PRV        VIDPRO
080500970711     C                   ELSE
080600970711     C                   MOVEL     *BLANKS       VIDLOC
080700970711    5C                   ENDIF
080800930727     C*
080900930727     C                   SETON                                        90
081000930727     C                   GOTO      ENDCT3
081100930727E   2C                   ENDIF
081200980219    1C                   ENDIF
081300960412I    C* CONTROLLO CHE LOCALITA    CONTENGA SOLO DATI VALIDI
081400960412     C                   Z-ADD     1             XX
081500960412     C     XX            DOWLE     10
081600960412     C     CAR(XX)       ANDNE     *BLANKS
081700960412     C     CAR(XX)       SCAN      VIDLOC                                 96
081800960412     C     *IN96         IFEQ      *ON
081900960412     C                   Z-ADD     10            XX
082000960412     C                   SETON                                        904728
082100960412     C                   MOVEL     ERR(10)       $MSG
082200960412     C                   ENDIF
082300960412     C                   ADD       1             XX
082400960412E    C                   ENDDO
082500960412     C   47              GOTO      ENDCT3
082600930723     C*
082700930723     C* C.A.P.  OBBLIGATORIO
082800970709 F  1C***        VIDCAP    IFEQ *ZEROS
082900970709     C***                  SETON                     904628
083000970709     C***                  MOVELERR,8     $MSG
083100970709     C***                  GOTO ENDCT3
083200970709X   1C***                  ELSE
083300930727     C*
083400980219     C* CONTROLLO ESISTENZA CAP (SOLO SE NAZIONE CON CAPPARIO)
083500980219     C     §15CPP        IFEQ      *BLANKS
083600941125     C                   MOVEL     VIDCAP        KCAP              9
083700961218     C                   EXSR      CAZIS0
083800970703     C   28              GOTO      ENDCT3
083900980219     C                   END
084000930727     C*
084100970709E   1C***                  ENDIF
084200930727     C*
084300980219     C* PROVINCIA OBBLIGATORIA SE NAZIONE ITALIA
084400961218IF  1C     VIDPRO        IFEQ      *ZEROS
084500980219     C     VIDNAZ        ANDEQ     *BLANKS
084600930727     C     VIDPRO        OREQ      *BLANKS
084700980219     C     VIDNAZ        ANDEQ     *BLANKS
084800940901     C                   SETON                                        902848
084900940901     C                   MOVEL     ERR(12)       $MSG
085000930727     C                   GOTO      ENDCT3
085100980219    1C                   END
085200930727     C*
085300930915     C* RICHIESTA RICERCA PROVINCE
085400930723     C     '?'           SCAN      VIDPRO                                 01
085500930723IF  2C     *IN01         IFEQ      *ON
085600930723     C                   MOVEL     'PR'          COD
085700930723     C                   MOVEL     *BLANKS       KEY
085800930723     C                   MOVEL     *BLANKS       DES
085900930722     C                   CALL      'X§TABER'
086000930723     C                   PARM                    CODUT
086100930723     C                   PARM                    COD
086200930723     C                   PARM                    KEY
086300930723     C                   PARM                    DES              25
086400930723     C*
086500930723     C                   MOVEL     KEY           VIDPRO
086600930723     C                   SETON                                        90
086700930723     C                   GOTO      ENDCT3
086800980219    2C                   END
086900930723     C*
087000930723     C* CONTROLLO ESISTENZA PROVINCIA IMMESSA
087100980219    1C     VIDPRO        IFNE      *BLANKS
087200930723     C                   MOVEL     'PR'          COD
087300930723     C                   MOVEL     *BLANKS       KEY
087400930723     C                   MOVEL     VIDPRO        KEY
087500930727     C     KTAB          SETLL     TABEL00F                               30
087600930723IF  3C     *IN30         IFEQ      *OFF
087700940901     C                   SETON                                        904828
087800940901     C                   MOVEL     ERR(11)       $MSG
087900930723     C                   GOTO      ENDCT3
088000930723E   3C                   ENDIF
088100980219    1C                   ENDIF
088200930727     C*
088300960412I    C* CONTROLLO CHE PROVINCIA   CONTENGA SOLO DATI VALIDI
088400960412     C                   Z-ADD     1             XX
088500960412     C     XX            DOWLE     10
088600960412     C     CAR(XX)       ANDNE     *BLANKS
088700960412     C     CAR(XX)       SCAN      VIDPRO                                 96
088800960412     C     *IN96         IFEQ      *ON
088900960412     C                   Z-ADD     10            XX
089000960412     C                   SETON                                        904828
089100960412     C                   MOVEL     ERR(12)       $MSG
089200960412     C                   ENDIF
089300960412     C                   ADD       1             XX                2 0
089400960412E    C                   ENDDO
089500960412     C   48              GOTO      ENDCT3
089600930727     C*
089700930727     C* CONTROLLO  P.I. SE IMMESSA
089800930727IF  1C     VIDIVA        IFNE      *ZEROS
089900930727     C     VIDIVA        ANDNE     *BLANKS
090000940901     C***                  MOVELVIDIVA    PIVA
090100930727     C                   EXSR      CTRIVA
090200930727     C*
090300930727     C* P.I. ERRATA
090400940901IF  2C     *IN49         IFEQ      *ON
090500930727     C                   GOTO      ENDCT3
090600930727E   2C                   ENDIF
090700930727E   1C                   ENDIF
090800960305     C*  Se il luogo immesso è per l'invio delle comuni. di giace.
090900960305     C                   MOVEL     'N'           WFAXOB
091000960305     C     §4LGIA        IFEQ      'S'
091100960305     C                   Z-ADD     SPECLI        KKSC              7 0
091200960305     C     KKSC          CHAIN     FNCLS01L                           34
091300960305     C     *IN34         IFEQ      '0'
091400960305     C                   Z-ADD     1             §F
091500960305     C     WAPERT        LOOKUP    C2F(§F)                                34
091600960305     C     *IN34         IFEQ      '1'
091700960305     C     T2F(§F)       ANDEQ     'F'
091800960305     C                   MOVEL     'S'           WFAXOB            1            Fax obblig
091900960305     C                   ELSE
092000960305     C                   Z-ADD     1             §F
092100960305     C     WCHIUS        LOOKUP    C2H(§F)                                34
092200960305     C     *IN34         IFEQ      '1'
092300960305     C     T2H(§F)       ANDEQ     'F'
092400960305     C                   MOVEL     'S'           WFAXOB            1            Fax obblig
092500960305     C                   END                                                    T2H
092600960305     C                   END                                                    T2F
092700960305     C                   END                                                    trov.FNCLS
092800960305     C                   END                                                    §4LGIA
092900960305     C*  Controllo il numero del fax
093000960305     C     V4CFAX        IFNE      *BLANKS
093100960305     C     V4CFAX        ANDNE     WSVFAX
093200960305     C     §4LFAX        OREQ      'S'
093300960305     C     WFAXOB        OREQ      'S'
093400960305     C                   CLEAR                   TRUL42
093500960305     C                   MOVEL     V4CFAX        D42FAX
093600960305     C                   MOVEL     V4CFAX        WSVFAX           15
093700960305     C                   CALL      'TRUL42R'
093800960305     C                   PARM                    TRUL42
093900960305     C     D42ERR        IFEQ      '1'
094000960314     C                   SETON                                        285090
094100960305     C                   MOVEL     D42MSG        $MSG
094200960305     C                   END
094300960305     C                   END
094400960305     C*  Controllo il numero del telefono
094500960305     C  N50VIDTEL        IFNE      *BLANKS
094600960305     C     VIDTEL        ANDNE     WSVTEL
094700960305     C                   CLEAR                   TRUL42
094800960305     C                   MOVEL     VIDTEL        WSVTEL           20
094900960305     C                   MOVEL     VIDTEL        D42FAX
095000960305     C                   CALL      'TRUL42R'
095100960305     C                   PARM                    TRUL42
095200960305     C     D42ERR        IFEQ      '1'
095300960314     C                   SETON                                        285190
095400960305     C                   MOVEL     D42MSG        $MSG
095500960305     C                   END
095600960305     C                   END
095700930727     C*
095800040622     C*  Controllo l'indirizzo e-mail
095900040622     C     VIDmail       IFNE      *BLANKS
096000040622     C     VIDmail       ANDNE     WSVmail
096100040622     C                   CLEAR                   dsemail
096200040622     C                   MOVEL     VIDmail       §emlindi
096300040622     C                   CALL      'XEMAIL'
096400040622     C                   PARM                    dsemail
096500040622     C     §emlerro      IFEQ      '1'
096600040622     C                   SETON                                        285490
096700040622     C                   MOVEL     §emlmsgo      $MSG
096800040707     c                   else
096900040622     c                   movel     §emlindo      vidmail
097000040707     C                   END
097100040622     C                   END
097200040622     C*
097300961219     C     ENDCT3        ENDSR
097400961011     C*---------------------------------------------------------------*
097500961011     C*                    CTR3D                                      *
097600961011     C* SE ANNULLAMENTO CONTROLLO CHE NON ESISTA RECORD TABELLA 3D    *
097700961011     C*---------------------------------------------------------------*
097800010706     C**!!!     CTR3D         BEGSR
097900010706     C**!!!                   MOVEL     '3D'          COD
098000010706     C**!!!     KTAB1         SETLL     TABEL00F
098100010706     C**!!!     KTAB1         READE     TABEL00F                               31
098200010706     C**!!!     *IN31         DOWEQ     *OFF
098300010706     C**!!!     TBLFLG        IFEQ      *BLANKS
098400010706     C**!!!                   MOVEL     TBLUNI        DS3D
098500010706     C**!!!     §3DKSC        IFEQ      CODCLI
098600010706     C**!!!     §3DLRT        ANDEQ     CODSP2
098700010706     C**!!!                   SETON                                        902831
098800010706     C**!!!                   MOVEL     ERR(20)       $MSG
098900010706     C**!!!                   END
099000010706     C**!!!                   END
099100010706     C**!!!  N31KTAB1         READE     TABEL00F                               31
099200010706     C**!!!                   ENDDO
099300010706     C**!!!                   ENDSR
099400930727     C*---------------------------------------------------------------*
099500930727     C*                    CAZIS0                                     *
099600930727     C* ROUTINE DI CONTROLLO ESISTENZA CAP                            *
099700930727     C*---------------------------------------------------------------*
099800930727     C     CAZIS0        BEGSR
099900930727     C*
100000970709     C* Mi salvo i dati Località/Provincia/Nazione/P.Iva
100100970709     C* immessi a video in campi di appoggio per vedere se
100200980219     C* verrano modificati dai pgm richiamati (LV13)
100300970709     C                   MOVEL     VIDLOC        WVDLOC
100400970709     C                   MOVEL     VIDPRO        WVDPRV
100500970709     C                   MOVEL     VIDNAZ        WVDNAR
100600970709     C                   MOVEL     VIDCAP        WVDCAP
100700930727     C* CONTROLLO CAP PER INTERO
100800970703     C                   CLEAR                   TISI95
100900970709     C                   CLEAR                   FNLV13
101000970709     C                   MOVEL     ' '           I95TLA
101100970709     C                   MOVEL     '7'           I95TCN
101200970703     C                   MOVEL     VIDCAP        I95CAP
101300970703     C                   MOVEL     VIDNAZ        I95NAR
101400970703     C                   MOVEL     VIDIND        I95IND
101500970703     C                   MOVEL     VIDLOC        I95LOC
101600970703     C                   MOVEL     VIDPRO        I95PRV
101700970711     C***                  MOVEL19970715  I95DAT
101800970709     C                   MOVEL     'S'           I13AF0
101900970709     C                   MOVEL     'S'           I13AF1
102000970709     C                   MOVEL     'S'           I13SZ2
102100970710     C                   MOVEL     'S'           I13LA3
102200970710     C                   MOVEL     'S'           I13SZ3
102300970709     C                   MOVEL     'S'           I13CNV
102400970709     C                   MOVEL     'S'           I13SZV
102500970703     C     SIMFEL        IFEQ      0
102600970703     C                   Z-ADD     46            I95TFP
102700970703     C                   ELSE
102800970703     C                   Z-ADD     SIMFEL        I95TFP
102900970703     C                   END
103000980602     C                   MOVEL     'S'           I95FRE
103100970709     C* Gestione errori forzabili: se cambiato CAP/Località
103200970709     C* riazzero i valori di errori forzabili
103300970709    2C     VIDCAP        IFNE      WSVCAP
103400970709     C                   CLEAR                   WFZV
103500970709     C                   CLEAR                   WFZ1
103600970709     C                   CLEAR                   WFZ2
103700970710     C                   CLEAR                   WFZ3
103800970709     C                   MOVEL     VIDCAP        WSVCAP
103900970709    2C                   ENDIF
104000970709     C* Modifica località
104100970709    2C     VIDLOC        IFNE      WSVLOC
104200970709     C                   MOVEL     VIDLOC        WSVLOC
104300970709     C                   CLEAR                   WFZ1
104400970709     C                   CLEAR                   WFZ2
104500970710     C                   CLEAR                   WFZ3
104600970709    2C                   ENDIF
104700970710     C* Modifica PROVINCIA
104800970710    2C     VIDPRO        IFNE      WSVPRO
104900970710     C                   MOVEL     VIDPRO        WSVPRO
105000970710     C                   CLEAR                   WFZ3
105100970710    2C                   ENDIF
105200970709     C** CALL
105300970709     C                   MOVEL     WFZV          E13FZV
105400970709     C                   MOVEL     WFZ1          E13FZ1
105500970709     C                   MOVEL     WFZ2          E13FZ2
105600970710     C                   MOVEL     WFZ3          E13FZ3
105700970709     C                   CALL      'FNLV13R'
105800970709     C                   PARM                    KPJBA
105900970709     C                   PARM                    FNLV13
106000970709     C                   PARM                    TISI95
106100970709     C* Ricontrollo errori
106200970709     C                   MOVEL     O13ERR        WERR
106300970709     C                   MOVEL     O13MSG        WMSG
106400970709     C                   MOVEL     E13FZV        WFZV
106500970709     C                   MOVEL     E13FZ1        WFZ1
106600970709     C                   MOVEL     E13FZ2        WFZ2
106700970710     C                   MOVEL     E13FZ3        WFZ3
106800970709     C*
106900970709     C* Se il livello di affidabilità non è inferiore a 2 e
107000970709     C* il livello di reperimento dati non è inferiore a 3
107100970709     C* reimposto i dati da ds TISI95
107200970709     C     O13RON        IFEQ      'S'
107300970709     C     O13ROC        OREQ      'S'
107400970709     C     O13ROP        OREQ      'S'
107500970709     C     O13ROL        OREQ      'S'
107600970709     C                   MOVEL     O95LOC        VIDLOC
107700970709     C                   MOVEL     O95PRV        VIDPRO
107800970709     C                   MOVEL     O95NAR        VIDNAZ
107900970709     C                   MOVEL     O95CAP        VIDCAP
108000970709     C                   END
108100970709     C*
108200970709     C                   EXSR      CTRIND
108300970709     C* Se non ci sono errori proseguo
108400970709     C   28              GOTO      FINCTR
108500970709     C*
108600970709     C* Controllo se reimpostata località
108700970710     C     O13ROL        IFEQ      'S'
108800970709     C                   SETON                                        4728
108900970710     C                   MOVEL     ERR(22)       $MSG
109000970709     C                   GOTO      FINCTR
109100970709     C                   END
109200970709     C* Controllo se reimpostata Provincia
109300970710     C     O13ROP        IFEQ      'S'
109400970709     C                   SETON                                        4828
109500970710     C                   MOVEL     ERR(24)       $MSG
109600970709     C                   GOTO      FINCTR
109700970709     C                   END
109800970709     C* Controllo se reimpostato CAP
109900970710     C     O13ROC        IFEQ      'S'
110000970709     C                   SETON                                        4628
110100970710     C                   MOVEL     ERR(23)       $MSG
110200970709     C                   GOTO      FINCTR
110300970709     C                   END
110400970709     C* Controllo se reimpostata Nazione
110500970710     C     O13RON        IFEQ      'S'
110600970709     C                   SETON                                        5328
110700970710     C                   MOVEL     ERR(25)       $MSG
110800970709     C                   GOTO      FINCTR
110900970709     C                   END
111000930727     C*
111100970709     C     FINCTR        ENDSR
111200970709     C*------------------------------------------------------*
111300970709     C*  Controllo dati indirizzo
111400970709     C*------------------------------------------------------*
111500970709     C     CTRIND        BEGSR
111600970709     C*
111700970709     C* Errore !!!!
111800970709     C     WERR          IFNE      ' '
111900970709     C     WMSG          ANDNE     *BLANKS
112000970709     C* Manca la Ragione sociale
112100970709     C                   SELECT
112200970709     C* Errore su località/città
112300970709     C     WERR          WHENEQ    '3'
112400970709     C                   SETON                                        4728
112500970709     C* Errore su Cap
112600970709     C     WERR          WHENEQ    '5'
112700970709     C                   SETON                                        4628
112800970709     C* Errore su provincia
112900970709     C     WERR          WHENEQ    '4'
113000970709     C                   SETON                                        4828
113100970709     C* Nazione  errata
113200970709     C     WERR          WHENEQ    '6'
113300970709     C                   SETON                                        5328
113400970709    2C                   ENDSL
113500970709     C                   MOVEL     WMSG          $MSG
113600970709     C                   END
113700010124      *
113800010124     C   28              EVAL      *IN90 = *ON
113900010124      *
114000970709     C                   ENDSR
114100930727     C*---------------------------------------------------------------*
114200930727     C*                    CTRIVA                                     *
114300930727     C* ROUTINE DI CONTROLLO PARTITA IVA                              *
114400930727     C*---------------------------------------------------------------*
114500930727     C     CTRIVA        BEGSR
114600930727     C*
114700151021     c                   clear                   xcfiva1ds
114800151021     c                   eval      xcfivamod = 'P'
114900151021     c                   eval      xcfivapar = vidiva
115000151021     c                   eval      xcfivanar = vidnaz
115100151021     c                   eval      xcfivaprv = vidpro
115200151021     c                   eval      xcfivaloc = vidloc
115300151021     c                   eval      xcfivacap = vidcap
115400151021     c                   call      'XCFIVAR1'
115500151021     c                   parm                    xcfiva1ds
115600151021     c                   If        xcfivaflg = -9 and wforzaiva = *off
115700151021     c                   Eval      *In28 = *On
115800151021     c                   Eval      *In49 = *On
115900151021     c                   Eval      $msg = xcfivamsg
116000151021     c                   Eval      $msg = %trim($msg) + ' Enter x forzare'
116100151021     c                   eval      wforzaiva = *on
116200151021     c                   Leavesr
116300151021     c                   EndIf
116400151021      * --> errore
116500151021     c                   if        xcfivaflg < *zeros and xcfivaflg <> -9
116600151021     c                   Eval      *In28 = *On
116700151021     c                   Eval      *In49 = *On
116800151021     c                   eval      $msg = xcfivamsg
116900151021     c                   Leavesr
117000151021     c                   EndIf
117100151021      * se non impostato devo riportare il codice iso della partita iva che
117200151021      * mi viene passato dal pgm di controllo
117300151021     c                   if        eiva <> ivaeu
117400151021     c                   eval      eiva = ivaeu
117500151021     c                   eval      *in49 = *on
117600151021     c                   eval      *in90 = *on
117700151021     c                   endif
117800940901     C*
117900930727     C                   ENDSR
118000930723     C*---------------------------------------------------------------*
118100930723     C*                    ANNUL                                      *
118200930723     C* ROUTINE DI ANNULLAMENTO RECORD                                *
118300930723     C*---------------------------------------------------------------*
118400930723     C     ANNUL         BEGSR
118500930723     C*
118600970804     C* LUOGHI/SPEDIZIONIERI
118700040715     C                   delete    FNSPE000
118800040715     C  n14              delete    FNSP2000
118900930723     C* SPEDIZIONIERI
119000970804     C***  N12                UPDATANSPE1
119100930723     C*
119200930723     C                   ENDSR
119300930723     C*---------------------------------------------------------------*
119400930723     C*                    AGGIO                                      *
119500930723     C* ROUTINE DI AGGIORNAMENTO RECORD                               *
119600930723     C*---------------------------------------------------------------*
119700930723     C     AGGIO         BEGSR
119800930723     C*
119900930723     C* IMPOSTAZIONE CAMPI
120000930723     C                   MOVE      *BLANKS       SPEFLG
120100930723     C                   Z-ADD     CODCLI        SPECLI
120200970804     C   12              MOVEL     'L'           SPEFLS
120300970804     C  N12              MOVEL     'S'           SPEFLS
120400930723     C  N12              MOVEL     CODSPE        SPECOD
120500930723     C   12              MOVEL     CODSP2        SPECOD
120600930723     C                   MOVEL     VIDRAG        SPERAG
120700930723     C                   MOVEL     VIDIND        SPEIND
120800970804     C                   MOVEL     VIDCAP        SPECAP
120900930723     C                   MOVEL     VIDLOC        SPELOC
121000930723     C                   MOVEL     VIDPRO        SPEPRO
121100930723     C                   MOVEL     VIDNAZ        SPENAZ
121200940901     C     EIVA          IFEQ      *BLANKS
121300940901     C                   MOVEL     VIVA          SPEPIV
121400940901     C                   ELSE
121500940901     C                   MOVEL     VIDIVA        SPEPIV
121600940901     C                   END
121700930723     C                   MOVEL     VIDTEL        SPETEL
121800970804     C                   MOVEL     V4CFAX        SPEFAX
121900930723     C                   MOVEL     VIDTEX        SPETEX
122000930723     C                   MOVEL     VIDRIV        SPEPER
122100040719     C                   move      DATEU         SPEDTR
122200930723     C                   MOVEL     *BLANKS       SPEFTR
122300040622     C                   MOVEL     VIDmail       SP2est
122400930723     C*
122500930723     C* V A R I A Z I O N E
122600930723     C*
122700930723IF  1C     *IN10         IFEQ      *OFF
122800970804     C* LUOGHI/SPEDIZIONIERI
122900970804     C                   UPDATE    FNSPE000
123000930723     C* SPEDIZIONIERI
123100970804     C**  N12                UPDATANSPE1
123200930723X   1C                   ELSE
123300930723     C*
123400930723     C* I N S E R I M E N T O
123500930723     C*
123600930723     C* LUOGHI
123700970804     C   12              WRITE     FNSPE000
123800930723     C* SPEDIZIONIERI
123900930723     C     *IN12         IFEQ      *OFF
124000930723     C*
124100930723     C* AGGIORNO CODICE ULTIMO SPEDIZIONIERE IMMESSO
124200930723     C                   MOVEL     COMSPE        SPENUO
124300930723     C                   OUT       SPEDTA
124400970804     C                   WRITE     FNSPE000
124500930723E   2C                   ENDIF
124600930723E   1C                   ENDIF
124700040622IF  1C     *IN14         IFEQ      *OFF
124800040622     C* LUOGHI/SPEDIZIONIERI
124900040622     C                   UPDATE    FNSP2000
125000040622     c                   else
125100040622     c                   move      specod        sp2cod
125200040622     C                   Z-ADD     speCLI        SP2CLI
125300040622     c                   move      'EM'          sp2tpe
125400040622     c                   move      *blank        sp2flg
125500040622     C                   WRITE     FNSP2000
125600040622     c                   endif
125700930723     C*
125800930723     C                   ENDSR
125900930727     C*---------------------------------------------------------------*
126000930727     C*                    CARTAB                                     *
126100020507     C* ROUTINE DI CARICAMENTO TABELLA*L3 l1                          *
126200930727     C*---------------------------------------------------------------*
126300930727     C     CARTAB        BEGSR
126400930727     C*
126500020507     C* TABELLA L1
126600930727     C                   Z-ADD     1             C                 2 0
126700020507     C                   CLEAR                   TR06DS
126800020507     C                   MOVEL     '£1'          D06COD
126900020507     C                   MOVEL     simfel        D06KEY
127000020507     C                   MOVEL     TR06DS        KPJBU
127100020507     C                   CALL      'TRUL06R'
127200020507     C                   PARM                    KPJBA
127300020507     C                   MOVEL     KPJBU         TR06DS
127400020507     C                   MOVEA     LIN           l1
127500020507     C*
127600020507     C*                  Z-ADD     SIMFEL        L3(1)
127700020507     C*                  MOVE      '£3'          COD
127800020507     C*                  MOVEL     *BLANKS       KEY
127900020507     C*                  MOVEL     SIMFEL        KEY
128000020507     C*    KTAB          CHAIN     TABEL00F                           30
128100020507IF  1C*    *IN30         IFEQ      *OFF
128200020507     C*    TBLKEY        ANDNE     *BLANKS
128300020507     C*                  MOVEA     TBLUNI        FIL
128400930727     C*
128500020507DO  2C*                  DO        29            B                 3 0
128600020507IF  3C*    FIL(B)        IFGT      *ZEROS
128700020507     C*                  MOVEL     FIL(B)        COMFIL            3 0
128800020507     C*    COMFIL        LOOKUP    L3                                     33
128900020507     C* N33              ADD       1             C
129000020507     C* N33              Z-ADD     COMFIL        L3(C)
129100020507E   3C*                  ENDIF
129200020507E   2C*                  ENDDO
129300020507E   1C*                  ENDIF
129400940901     C*
129500940901     C                   SETOFF                                       2840
129600960305     C*
129700960305     C* Tabella 2F
129800960305     C                   Z-ADD     0             §F                3 0
129900960305     C                   MOVEA     *BLANKS       C2F
130000960305     C                   MOVEA     *BLANKS       T2F
130100960305     C                   MOVE      '2F'          COD
130200960305     C     KTAB1         CHAIN     TABEL00F                           30
130300960305IF  1C     *IN30         DOWEQ     '0'
130400960305IF  1C     §F            ANDLT     100
130500960305     C     TBLFLG        IFEQ      *BLANKS
130600960305     C                   ADD       1             §F
130700960305     C                   MOVEL     TBLUNI        DS2F
130800960305     C                   MOVEL     TBLKEY        C2F(§F)
130900960305     C                   MOVEL     §2FFTP        T2F(§F)
131000960305     C                   END
131100960305     C     KTAB1         READE     TABEL00F                               30
131200960305     C                   END
131300960305     C*
131400960305     C* Tabella 2H
131500960305     C                   Z-ADD     0             §F                3 0
131600960305     C                   MOVEA     *BLANKS       C2H
131700960305     C                   MOVEA     *BLANKS       T2H
131800960305     C                   MOVE      '2H'          COD
131900960305     C     KTAB1         CHAIN     TABEL00F                           30
132000960305IF  1C     *IN30         DOWEQ     '0'
132100960305IF  1C     §F            ANDLT     100
132200960305     C     TBLFLG        IFEQ      *BLANKS
132300960305     C                   ADD       1             §F
132400960305     C                   MOVEL     TBLUNI        DS2H
132500960305     C                   MOVEL     TBLKEY        C2H(§F)
132600960305     C                   MOVEL     §2HFAX        T2H(§F)
132700960305     C                   END
132800960305     C     KTAB1         READE     TABEL00F                               30
132900960305     C                   END
133000930727     C*
133100930727     C                   ENDSR
133200961009     C*---------------------------------------------------------------*
133300961009     C*                    PGMRIC                                     *
133400961009     C* ROUTINE DI IMPOSTAZIONE CAMPI SE PGM RICHIAMATO               *
133500961009     C*---------------------------------------------------------------*
133600961009     C     PGMRIC        BEGSR
133700961009     C                   SETON                                        1213
133800961009     C                   MOVEL     INT(2)        VIDINT
133900961009     C                   MOVEL     DM6SPE        CODSPE
134000961009     C                   MOVE      DM6COD        CODCLI
134100961009     C* CHAINO TABELLA 4L = COD.LUOGHI
134200961009     C                   MOVEL     '4L'          COD
134300961009     C                   MOVEL(P)  CODSPE        KEY
134400961009     C     KTAB          CHAIN     TABEL00F                           30
134500961009     C  N30              MOVEL     TBLUNI        DS4L
134600961009     C   30              CLEAR                   DS4L
134700961009     C*
134800961009     C* CHAINO ANGRAFICA LUOGO CON CHIAVE PASSATA
134900970807     C                   MOVEL     'L'           KFLS
135000970804     C     KEYSPE        CHAIN     FNSPE01L                           10
135100040622     C     KEYSP2        CHAIN     FNSP201L                           14
135200961009     C* IMPOSTO CAMPI A VIDEO
135300961009    1C     *IN10         IFEQ      *ON
135400040715     C                   MOVEL     OPE(1)        VIDOPE
135500961009   X1C                   ELSE
135600961009     C*
135700040715     C                   MOVEL     OPE(2)        VIDOPE
135800961009E   1C                   ENDIF
135900961009     C*
136000961009     C* PER INSERIMENTO
136100961009     C                   MOVEL     *BLANKS       WSVTEL
136200040630     C                   MOVEL     *BLANKS       WSVmail          60
136300961009     C                   MOVEL     *BLANKS       WSVFAX
136400040715     C   14              MOVEL     *BLANKS       VIDmail
136500961009IF  1C     *IN10         IFEQ      *ON
136600961009     C                   MOVEL     *BLANKS       VIDRAG
136700961009     C                   MOVEL     *BLANKS       VIDIND
136800970804     C                   CLEAR                   VIDCAP
136900961009     C                   MOVEL     *BLANKS       VIDLOC
137000961009     C                   MOVEL     *BLANKS       VIDPRO
137100961009     C                   MOVEL     *BLANKS       VIDNAZ
137200961009     C                   MOVEL     *BLANKS       VIDIVA
137300961009     C                   MOVEL     *BLANKS       VIDTEL
137400961009     C                   MOVEL     *BLANKS       VIDTEX
137500961009     C                   MOVEL     *BLANKS       V4CFAX
137600961009     C                   MOVEL     *BLANKS       VIDRIV
137700961009     C*
137800961009     C* PER VARIAZIONE
137900961009X   1C                   ELSE
138000961009     C                   MOVEL     SPERAG        VIDRAG
138100961009     C                   MOVEL     SPEIND        VIDIND
138200970804     C                   MOVEL     SPECAP        VIDCAP
138300961009     C                   MOVEL     SPELOC        VIDLOC
138400961009     C                   MOVEL     SPEPRO        VIDPRO
138500961009     C                   MOVEL     SPENAZ        VIDNAZ
138600961009     C                   MOVEL     SPEPIV        CNT2              2
138700961009     C     CNT2          IFGT      *ZEROS
138800961009     C                   MOVEL     SPEPIV        VIVA
138900961009     C                   MOVEL     *BLANKS       EIVA
139000961009     C                   ELSE
139100961009     C                   MOVEL     SPEPIV        VIDIVA
139200961009     C                   END
139300961009     C                   MOVEL     SPETEL        VIDTEL
139400970804     C                   MOVEL     SPEFAX        V4CFAX
139500961009     C                   MOVEL     SPETEX        VIDTEX
139600961009     C                   MOVEL     SPEPER        VIDRIV
139700040622     C                   MOVEL     SP2est        VIDmail
139800961009E   1C                   ENDIF
139900961009     C*
140000961009     C                   MOVEL     CODSPE        CODSP2
140100961009     C*
140200961009     C                   SETOFF                                           28
140300961009     C                   SETOFF                                       444546
140400961009     C                   SETOFF                                       474849
140500970703     C                   SETOFF                                       505153
140600040622     C                   SETOFF                                       54
140700961009     C*
140800961009     C                   ENDSR
140900930720     C*---------------------------------------------------------------*
141000940901     C*                    *INZSR                                     *
141100930720     C* ROUTINE DI DEFINIZIONI VARIABILI, KLIST E PLIST               *
141200930720     C*---------------------------------------------------------------*
141300940901     C     *INZSR        BEGSR
141400930720     C*
141500930720     C     *ENTRY        PLIST
141600930720     C                   PARM                    KPJBA
141700961009     C                   MOVEL     KPJBU         DSC0M6
141800961009     C*
141900961009     C* 09 ON --> PGM RICHIAMATO
142000961009     C     DM6RIC        COMP      ' '                                0909
142100930720     C*
142200930720     C* RICERCA FILIALE IN GESTIONE
142300930720     C                   Z-ADD     1             CODUT
142400930720     C                   CALL      'X§PARUT'
142500930720     C                   PARM                    UT§DSE
142600930720     C                   MOVEL     REC80         CNCR80
142700930720     C                   MOVEL     RAGUT         RSUT             20
142800961112     C* 08 ON  - SONO IN FILIALE
142900961112     C     SIMFEL        IFGT      0
143000961112     C                   SETON                                        08
143100961112     C                   ENDIF
143200930727     C*
143300930727     C* RICERCA CAPOCONTI
143400930727DO  1C                   DO        50            X                 2 0
143500930727     C                   MOVE      TCU(X)        TCUDS
143600940901     C*  CLIENTI
143700940901     C     F56           IFEQ      'CG'
143800940901     C     F4            ANDEQ     '1'
143900940901     C     F3            ANDEQ     '0'
144000940901     C                   Z-ADD     KCU(X)        KCI               4 0
144100940901     C                   END
144200930727E   1C                   ENDDO
144300930720     C*
144400930720     C     *DTAARA       DEFINE                  SPEDTA
144500930720     C*
144600961111     C     *LIKE         DEFINE    V5CFIL        PARFIL
144700970709     C     *LIKE         DEFINE    VIDLOC        WVDLOC
144800970709     C     *LIKE         DEFINE    VIDPRO        WVDPRV
144900970709     C     *LIKE         DEFINE    VIDNAZ        WVDNAR
145000970709     C     *LIKE         DEFINE    VIDCAP        WVDCAP
145100970709     C     *LIKE         DEFINE    O13ERR        WERR
145200970709     C     *LIKE         DEFINE    O13MSG        WMSG
145300970709     C     *LIKE         DEFINE    VIDCAP        WSVCAP
145400970709     C     *LIKE         DEFINE    VIDLOC        WSVLOC
145500970710     C     *LIKE         DEFINE    VIDPRO        WSVPRO
145600970709     C     *LIKE         DEFINE    E13FZV        WFZV
145700970709     C     *LIKE         DEFINE    E13FZ1        WFZ1
145800970709     C     *LIKE         DEFINE    E13FZ2        WFZ2
145900970710     C     *LIKE         DEFINE    E13FZ3        WFZ3
146000970804     C     *LIKE         DEFINE    SPEFLS        KFLS
146100040622     C     *LIKE         DEFINE    sp2tpe        ktpe
146200930723     C* ANSPE00F
146300970804     C     KEYSPE        KLIST
146400970804     C                   KFLD                    KFLS
146500930720     C                   KFLD                    CODCLI
146600930720     C                   KFLD                    CODSPE
146700040622     C* fnsp2
146800040622     C     KEYSP2        KLIST
146900040622     C                   KFLD                    specli
147000040622     C                   KFLD                    specod
147100040622     C                   KFLD                    ktpe
147200040622     c                   move      'EM'          ktpe
147300930720     C* TABEL00F
147400930720     C     KTAB          KLIST
147500930720     C                   KFLD                    CODUT             1 0
147600930720     C                   KFLD                    COD               2
147700930723     C                   KFLD                    KEY               8
147800960305     C     KTAB1         KLIST
147900960305     C                   KFLD                    CODUT             1 0
148000960305     C                   KFLD                    COD               2
148100930727     C*
148200930727     C* CNACO00F
148300930727     C     KACO          KLIST
148400930727     C                   KFLD                    CODUT
148500930727     C                   KFLD                    KCI
148600930727     C                   KFLD                    KSC               7 0
148700980219     C* GIRO UDATE
148800980219     C                   TIME                    W0140            14 0
148900980219     C                   MOVE      W0140         WDTGIO            8 0
149000980219     C                   Z-ADD     WDTGIO        G02DAT
149100980219     C                   MOVEL     *BLANK        G02ERR
149200980219     C                   CALL      'XSRDA8'
149300980219     C                   PARM                    WLBDAT
149400980219     C                   MOVEL     G02INV        DATEU             8 0
149500941125     C*
149600930720     C                   ENDSR
149700930721     O*---------------------------------------------------------------*
149800930721     O*               *** INTESTAZIONE VIDEO ***                      *
149900930721     O*---------------------------------------------------------------*
150000961112**
150100930721***   ANAGRAFICA  SPEDIZIONIERI   ***
150200930721* ANAGRAFICA LUOGHI DI DESTINAZIONE *
150300930722**
150400930722IMMISSIONE
150500930722VARIAZIONE
150600940901**
150700970709E' MOMENTANEAMENTE Impossibile gestire l'anagrafica Spedizionieri             01
150800970709Scegliere l'anagrafica da manutenzionare                                      02
150900970709Codice Spedizioniere obbligatorio                                             03
151000970709Codice Spedizioniere errato                                                   04
151100970709Codice nuovo spedizioniere errato                                             05
151200970709Ragione sociale obbligatoria o inseriti caratteri non validi                  06
151300970709Indirizzo Obbligatorio o inseriti caratteri non validi                        07
151400970709C.A.P. Obbligatorio                                                           08
151500970709C.A.P. inesistente o incongruente con la provincia                            09
151600970709Località obbligatoria o inseriti caratteri non validi                         10
151700970709Provincia inesistente                                                         11
151800970709Provincia obbligatoria o inseriti caratteri non validi                        12
151900970709Partita IVA formalmente errata                                                13
152000970709Codice Luogo obbligatorio                                                     14
152100970709Codice Luogo Errato o non utilizzabile                                        15
152200970709Il Codice cliente deve essere diverso da 9999999                              16
152300970709Codice cliente obbligatorio                                                   17
152400970709Cliente Inesistente o non gestibile                                           18
152500970709Per la ricerca dei luoghi inserire il codice cliente                          19
152600970709Luogo presente in tab.3D-Cod.clienti ordini ritiro: annull.non ammesso        20
152700970709Immettere punto operativo valido                                              21
152800970710Attenzione !! Modificata Località                                             22
152900970710Attenzione !! Modificato CAP                                                  23
153000970710Attenzione !! Modificata Provincia                                            24
153100970710Attenzione !! Modificata Nazione                                              25
153200980219Nazione inesistente                                                           26
153300000630Cliente non gestibile con 9999 o 8888                                         27
153400960412**
153500960412!:§
153600961112**         CMD1
153700961112CHKOBJ OBJ(FNCLS01L) OBJTYPE(*FILE)
