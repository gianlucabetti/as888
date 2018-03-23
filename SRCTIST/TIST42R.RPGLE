000100000000     H*------------------------------------------------------------------------*
000200001207     H* STATISTICA DATI POSTE - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000927     FTIST42D   CF   E             WORKSTN INFDS(DEVDS1)
000900000927     F                                     SFILE(ST42DMS:MSGULT)
001000000927     FTABEL00F  IF   E           K DISK
001100000927     FAZORG01L  IF   E           K DISK
001200001207     FANSIF01L  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI
001800000000     D*-------------------
001900000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000000000     D*-------------------
002100000000     D* COMANDI
002200000000     D*-------------------
002300000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002400000000     D*-------------------
002500000000     D* DESCRIZIONI TASTI FUNZIONALI
002600000000     D*-------------------
002700000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002800000000     D*-------------------
002900000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003000000000     D*-------------------
003100000000     D FUV             S             19    DIM(24)
003200000927     D*-------------------
003300000927     D* CAPOCONTO / SOTTOCONTO DI RITORNO "XCLIR"
003400000927     D*-------------------
003500000927     D KSC             S              7  0 DIM(30)
003600000927     D KSA             S              4    DIM(30)                              *ALFANUMERICO
003700000914     D*------------------
003800000914     D* DS "XSRDA8" - CONTROLLA DATA (8)
003900000914     D*------------------
004000000914     D WLBDA8          DS                  INZ
004100000914     D  G08DAT                 1      8  0
004200000914     D  G08INV                 9     16  0
004300000914     D  G08ERR                17     17
004400000914     D  G08TGI                18     22  0
004500000927     D*------------
004600000927     D* FILIALI "VALIDE"
004700000927     D*------------
004800000927     D FILAGE          s              3  0 DIM(300)                             * FILIALI/AGENZIE
004900000927     D*-------------------
005000000927     D* DS CONTROLLO CAP
005100000927     D*-------------------
005200000927     D DSSI95        E DS                  EXTNAME(TISI95DS)
005300000927     D*-------------------
005400000927     D* DS PER FNLV13R - CONTROLLO CAP
005500000927     D*-------------------
005600000927     D DSLV13        E DS                  EXTNAME(FNLV13DS)
005700000927     D*------------------
005800000927     D* DS "XCLIR"
005900000927     D*------------------
006000000927     D DSXC          E DS                  EXTNAME(XCLIRDS)
006100000927     D*------------------
006200000927     D* DS PER PASSAGGIO CAMPI PACKED
006300000927     D*------------------
006400000927     D                 DS
006500000927     D  DSKSN                  1      4P 0
006600000927     D  DSKSA                  1      4
006700000927     D*------------------
006800000927     D* DS REPERIMENTO DATI UTENTE
006900000927     D*-------------------
007000000927     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
007100000927     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
007200000927     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
007300000927     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
007400000927     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
007500020923     D TIBS34DS      E DS                                                       *Profili utente
007600020923     D DDATIUTE      E DS                                                       *Dati utente
007700020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007800060615     D*-------------------
007900060615     D* DS DATI STATISTICHE/SALDI SEDE
008000060615     D*-------------------
008100060615     D DS5S          E DS
008200000000     D*------------------------------------------------------------------------*
008300000000     D* INPUT
008400000000     D*------------------------------------------------------------------------*
008500000000     D*-------------------
008600000000     D* RIDEFINIZIONE SCHIERA -FUD-
008700000000     D*-------------------
008800000000     D FUDDS           DS
008900000000     D*                -DESCRIZIONE PARZIALE
009000000000     D  FUDDE1                 1      1
009100000000     D*                -FUNZIONE
009200000000     D  FUDFUN                 2      3
009300000000     D*                -DESCRIZIONE PARZIALE
009400000000     D  FUDDE2                 4     19
009500000000     D*                -FILLER
009600000000     D  FUDFI0                20     34
009700000000     D*                -FLAG FUNZ.ATTIVA (S/N)
009800000000     D  FUDATT                35     35
009900000000     D*                -FILLER
010000000000     D  FUDFI1                36     80
010100000000     D*                -DESCRIZIONE COMPLETA
010200000000     D  FUDDES                 1     19
010300000000     D*                -RIDEFINIZIONE RIGA
010400000000     D  FUDRIG                 1     80
010500000000     D*-------------------
010600000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
010700000000     D*-------------------
010800000000     D DSMSG         E DS
010900000000     D*-------------------
011000000000     D* DS PARAMETRI DI LANCIO PGM
011100000000     D*-------------------
011200000927     D PARAM           DS
011300060714     D  PARDAI                        8  0
011400060714     D  PARDAF                        8  0
011500060714     D  PARFAT                       17  0
011600060714     D  PARKSC                        7
011700060714     D  PARFIL                        3  0
011800060714     D  PARNAZ                        3
011900060714     D  PARCAP                        9
012000060714     D  PARPRV                        2
012100060714     D  PARBLK                        1
012200060714     D  PARCIC                        1
012300060714     D  PARBAR                        1
012400060714     D  PARSDI                        1
012500060714     D  PARPIC                        1
012600060714     D  PARFOA                        1
012700060714     D  PARNTN1                       2
012800060714     D  PARNTE1                       2
012900060714     D  PARSOC                        3
013000060714     D  PARCOF                        1
013100060714     D  PARCPI                        1
013200060714     D  PARIND                        1
013300060714     D  PARNTN2                       2
013400060714     D  PARNTN3                       2
013500060714     D  PARNTN4                       2
013600060714     D  PARNTE2                       2
013700060714     D  PARNTE3                       2
013800060714     D  PARNTE4                       2
013900060714     D  PARNTE5                       2
014000060714     D  PARNTE6                       2
014100060714     D  PARNTE7                       2
014200060714     D  PARNTE8                       2
014300060714     D  PARTBC                        1
014400060714     D  PARRGR                        1
014500060714     D  PAROVR                        1
014600060714     D  PARCHK                        1
014700060714     D  PARSOGN                       1
014800000000     D*-------------------
014900000000     D* DS PER GESTIONE TASTI FUNZIONALI
015000000000     D*-------------------
015100000000     D DSKEY         E DS
015200000000     D DEVDS1          DS
015300000000     D  KEY                  369    369
015400000000     D*-------------------
015500000000     D* ARCHITETTURA
015600000000     D*-------------------
015700000000     D KPJBA         E DS
015800000927     D*-------------------
015900000927     D* DEFINIZIONE COSTANTI
016000000927     D*-------------------
016100000927     D CNAZ            C                   CONST('999=TUTTE ''   ''=ITA')
016200000927     D*-------------------
016300000927     D* DS DI WORK PER OPERAZIONI VARIE SU DATE
016400000927     D*-------------------
016500000927     D                 DS
016600000927     D  GGINV                  1      2  0
016700000927     D  MMINV                  3      4  0
016800000927     D  ANNOINV                5      8  0
016900000927     D  DATAINV                1      8  0
017000000927     D*------------------------------------------------------------------------*
017100000927     D* VARIABILI DI WRK
017200000927     D*------------------------------------------------------------------------*
017300000927     D  idx            s              5i 0 inz(*zeros)
017400000000     C*------------------------------------------------------------------------*
017500000000     C* MAIN LINE
017600000000     C*------------------------------------------------------------------------*
017700000000     C*
017800000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
017900000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
018000000000     C*
018100000000     C* CICLO FINO A CHE:
018200000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
018300000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
018400000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
018500000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
018600000000     C     F12L00        OREQ      '1'
018700000000     C     FLFINE        OREQ      '1'
018800000000     C*
018900000000     C* EMETTE FORMATO VIDEO
019000000000     C                   EXSR      BUTFMR
019100000000     C*
019200000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
019300000000     C                   EXSR      FUNCON
019400000000     C*
019500000914     C                   ENDDO                                                  <-- 2 --
019600000000     C*
019700000914     C                   ENDIF                                                  <-- 1 --
019800991118     C*
019900000000     C                   SETON                                        LR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* EMISSIONE FORMATO A VIDEO
020200000000     C*------------------------------------------------------------------------*
020300000000     C     BUTFMR        BEGSR
020400000000     C*
020500000000     C* WRITE RECORD TESTATA
020600000927     C                   WRITE     ST42D01
020700000000     C*
020800000000     C* VISUALIZZAZIONE MESSAGGIO
020900000000     C                   EXSR      SNDMSG
021000000000     C*
021100000000     C* WRITE RECORD FUNZIONI
021200000927     C                   WRITE     ST42D02
021300991019     C*
021400991019     C* EXFMT RECORD DETTAGLIO
021500000927     C                   EXFMT     ST42D03
021600991019     C*
021700000000     C* AZZERO FLAG DI ERRORE LIVELLO
021800000000     C                   MOVEL     '0'           ERRL00
021900000000     C*
022000000000     C* AZZERO FLAG DI ERRORE
022100991025     C                   SETOFF                                         7071
022200000000     C*
022300000000     C                   ENDSR
022400000000     C*------------------------------------------------------------------------*
022500000000     C* SNDMSG - MANDA MESSAGGIO
022600000000     C*------------------------------------------------------------------------*
022700000000     C     SNDMSG        BEGSR
022800000000     C*
022900000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
023000000000     C                   MOVEL     '1'           *IN40
023100000927     C                   WRITE     ST42DMC
023200000000     C                   MOVEL     '0'           *IN40
023300000000     C*
023400000000     C* VISUALIZZAZIONE MESSAGGIO
023500000000     C                   Z-ADD     0             MSGULT            4 0
023600000000     C                   CALL      'XMSG'        XMSGPA
023700000000     C     DSMSER        IFEQ      0
023800000000     C                   MOVEL     DSMSKE        MSGKEY
023900000000     C                   END
024000000000     C                   Z-ADD     1             MSGULT
024100000927     C                   WRITE     ST42DMS                                      SFL MESS
024200000927     C                   WRITE     ST42DMC                                      CONTR MESS
024300000000     C*
024400000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
024500000000     C                   MOVEL     *BLANKS       DSMSMS
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
025000000000     C*------------------------------------------------------------------------*
025100000000     C     ENTGES        BEGSR
025200000000     C*
025300000000     C* CONTROLLA I CAMPI IMMESSI
025400000000     C                   EXSR      DECODI
025500000000     C*
025600000000     C                   ENDSR
025700000000     C*------------------------------------------------------------------------*
025800000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
025900000000     C*------------------------------------------------------------------------*
026000000000     C     F03GES        BEGSR
026100000000     C*
026200000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
026300000000     C                   MOVEL     '1'           F03L00
026400000000     C*
026500000000     C                   ENDSR
026600000927     C*------------------------------------------------------------------------*
026700000927     C* F07GES -GESTIONE TASTO FUNZIONALE F07 - IMOSTA VALORI DI DEFAULT
026800000927     C*------------------------------------------------------------------------*
026900000927     C     F07GES        BEGSR
027000000927     C*
027100000927     C*
027200000927     C                   ENDSR
027300000000     C*------------------------------------------------------------------------*
027400000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
027500000000     C*------------------------------------------------------------------------*
027600991020     C     F06GES        BEGSR
027700000000     C*
027800991105     C                   EXSR      DECODI
027900991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
028000991105     C     ERRL00        IFEQ      '0'
028100991105     C*
028200000914     C* LANCIO IL PROGRAMMA DI GENERAZIONE ARCHIVIO
028300000927     C                   MOVEL     'ST43'        KCOAZ
028400000000     C                   EXSR      VIDPAR
028500011127     C                   MOVEL     PARAM         KPJBU
028600071231     C***                CALL      'BCH10'
028700071231 MOM C                   CALL      'TIST43R'
028800991021     C                   PARM                    KPJBA
028900000927     C                   MOVEL     '1'           F03L00
029000000914     C*
029100991105     C                   ENDIF
029200991105     C*
029300000000     C                   ENDSR
029400000000     C*------------------------------------------------------------------------*
029500000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
029600000000     C*------------------------------------------------------------------------*
029700000000     C     F12GES        BEGSR
029800000000     C*
029900000000     C* IMPOSTO FLAG DI LIVELLO
030000000000     C                   MOVEL     '1'           F12L00
030100000000     C*
030200000000     C                   ENDSR
030300000000     C*------------------------------------------------------------------------*
030400991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
030500000000     C*------------------------------------------------------------------------*
030600000000     C     F24GES        BEGSR
030700000000     C*
030800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
030900000000     C                   EXSR      FUNGES
031000000000     C*
031100000000     C                   ENDSR
031200000914     C*------------------------------------------------------------------------*
031300000914     C* CHKDA8 - CONTROLLA LA DATA (8)
031400000914     C*------------------------------------------------------------------------*
031500000914     C     CHKDA8        BEGSR
031600000914     C*
031700000927     C     *LIKE         DEFINE    VIDDAD        WDA8
031800000927     C     *LIKE         DEFINE    VIDDAA        DA8W
031900000914     C*
032000000914     C                   Z-ADD     WDA8          G08DAT
032100000914     C                   Z-ADD     *ZEROS        G08INV
032200000914     C                   MOVEL     '0'           G08ERR
032300000914     C                   CALL      'XSRDA8'
032400000914     C                   PARM                    WLBDA8
032500000914     C                   Z-ADD     G08DAT        WDA8
032600000914     C                   Z-ADD     G08INV        DA8W
032700000914IF  1C     G08ERR        IFEQ      '1'
032800000914     C                   MOVEL     '1'           ERRL00
032900000914E   1C                   ENDIF
033000000914     C*
033100000914     C                   ENDSR
033200991020     C*------------------------------------------------------------------------*
033300000914     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
033400000000     C*------------------------------------------------------------------------*
033500000000     C     DECODI        BEGSR
033600000914     C*
033700000927     C* AZZERO GLI INDICATORI DI ERRORE
033800000927     C                   SETOFF                                       707172
033900000927     C                   SETOFF                                       737475
034000001120     C                   SETOFF                                       767778
034100001207     C                   SETOFF                                       7980
034200000927     C*
034300000914     C                   MOVEL     '0'           ERRL00
034400000927     C*--------------------
034500000927     C* CODICE CLIENTE
034600000927     C*--------------------
034700000927     C* RICHIESTA RICERCA CON IL ?
034800001207     C     ERRL00        IFEQ      '0'                                          *ANCORA VALIDO
034900000927     C     '?'           SCAN      VIDKSC                                 01
035000001207     C     *IN01         IFEQ      *ON
035100000927     C                   CLEAR                   DSXC
035200020923     C                   Z-ADD     DUTKCI        DXCCAP                         *CAPOCONTO
035300000927     C                   CALL      'XCLIR'
035400000927     C                   PARM                    DSXC
035500000927     C                   MOVEA     DXCKSC        KSA                            *SCOMPATTA CODICI
035600001207     C     1             DO        30            J                 4 0
035700000927     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
035800000927     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
035900001207     C                   ENDDO
036000000927     C                   MOVEL     KSC(1)        VIDKSC
036100001207     C                   ENDIF
036200001207     C                   ENDIF
036300000927     C* CONTROLLA SE CLIENTE NUMERICO
036400000927     C                   IF        VIDKSC <> *blanks
036500001207     C     ERRL00        IFEQ      '0'                                          *ANCORA VALIDO
036600000927     C                   SETOFF                                       28
036700000927     C                   TESTN                   VIDKSC               28
036800001207     C     *IN28         IFEQ      *ON                                          DA TESTN E'NUMERO
036900000927     C                   MOVE(P)   VIDKSC        W001              1
037000001207     C     W001          IFLT      '0'                                          *TROVATA UN LETTERA
037100000927     C                   MOVEL     '1'           ERRL00
037200000927     C                   MOVEL     MSG(13)       DSMSMS
037300000927     C                   SETON                                        70
037400001207     C                   ENDIF
037500001207     C                   ELSE                                                   DA TESTN NON E'NUMER
037600000927     C                   MOVEL     '1'           ERRL00
037700000927     C                   MOVEL     MSG(13)       DSMSMS
037800000927     C                   SETON                                        70
037900001207     C                   ENDIF
038000001207     C                   ENDIF
038100000927     C* CONTROLLA SE CLIENTE VALIDO
038200001207     C     ERRL00        IFEQ      '0'
038300000927     C                   CLEAR                   BS69DS
038400000927     C                   CLEAR                   ACODS
038500000927     C                   MOVEL     KNSIF         I69SIF
038600000927     C                   Z-ADD     151           I69KCC
038700000927     C                   MOVE(P)   VIDKSC        I69KAC
038800000927     C                   CALL      'TIBS69R'
038900000927     C                   PARM                    BS69DS
039000000927     C                   PARM                    ACODS
039100000927     C                   PARM                    INDDS
039200000927     C                   PARM                    CLPDS
039300000927     C                   PARM                    CLSDS
039400001207     C     O69ERR        IFNE      '1'
039500001207     C                   ELSE
039600000927     C                   MOVEL     '1'           ERRL00
039700000927     C                   MOVEL     MSG(13)       DSMSMS
039800000927     C                   SETON                                        70
039900001207     C                   ENDIF
040000001207     C                   ENDIF
040100000927     C                   ENDIF
040200000914     C*--------------------
040300000927     C* DATA INIZIALE PERIODO FATTURAZIONE
040400000914     C*--------------------
040500000927     C                   IF        ERRL00 = '0'
040600000927     C     *LIKE         DEFINE    VIDDAD        DAIVID
040700001207     C     ERRL00        IFEQ      '0'
040800000927     C                   Z-ADD     VIDDAD        WDA8
040900000914     C                   Z-ADD     *ZEROS        DA8W
041000000914     C                   EXSR      CHKDA8
041100000927     C                   Z-ADD     WDA8          VIDDAD
041200000914     C                   Z-ADD     DA8W          DAIVID
041300001207     C     ERRL00        IFEQ      '1'
041400000927     C                   SETON                                        75
041500000927     C                   MOVEL     MSG(03)       DSMSMS
041600001207     C                   ENDIF
041700001207     C                   ENDIF
041800000927     C                   ENDIF
041900000914     C*--------------------
042000000927     C* DATA FINALE PERIODO FATTURAZIONE
042100000914     C*--------------------
042200000927     C                   IF        ERRL00 = '0'
042300000927     C     *LIKE         DEFINE    VIDDAA        DAFVID
042400001207     C     ERRL00        IFEQ      '0'
042500000927     C                   Z-ADD     VIDDAA        WDA8
042600000914     C                   Z-ADD     *ZEROS        DA8W
042700000914     C                   EXSR      CHKDA8
042800000927     C                   Z-ADD     WDA8          VIDDAA
042900000914     C                   Z-ADD     DA8W          DAFVID
043000001207     C     ERRL00        IFEQ      '1'
043100000927     C                   SETON                                        76
043200000927     C                   MOVEL     MSG(03)       DSMSMS
043300001207     C                   ENDIF
043400001207     C                   ENDIF
043500000927     C                   ENDIF
043600000914     C*--------------------
043700000914     C* DATA INIZIALE > DATA FINALE
043800000914     C*--------------------
043900001207     C     ERRL00        IFEQ      '0'
044000000914     C     DAIVID        IFGT      DAFVID
044100000914     C                   MOVEL     '1'           ERRL00
044200000927     C                   SETON                                        75
044300000927     C                   SETON                                        76
044400000927     C                   MOVEL     MSG(06)       DSMSMS
044500000914     C                   ENDIF
044600001207     C                   ENDIF
044700000927     C*--------------------
044800000927     C*  PUNTO OPERATIVO "VALIDO"
044900000927     C*--------------------
045000000927     C                   IF        ERRL00 = '0'
045100000927     C                   IF        VIDFIL <> *ZEROS
045200000927     C     VIDFIL        LOOKUP    FILAGE                                 20
045300000927     C                   IF        *IN20 = *OFF
045400000927     C                   MOVEL     '1'           ERRL00
045500000927     C                   SETON                                        71
045600000927     C                   MOVEL     MSG(10)       DSMSMS
045700000927     C                   ENDIF
045800000927     C                   ENDIF
045900000927     C                   ENDIF
046000001207     C*--------------------
046100001207     C*  SOCIETA "VALIDA"
046200001207     C*--------------------
046300001207     C                   IF        ERRL00 = '0'
046400001207     C                   IF        VIDSOC <> *BLANKS
046500001207     C     VIDSOC        CHAIN     ANSIF01L
046600001207     C                   IF        not %found(ANSIF01L)
046700001207     C                   MOVEL     '1'           ERRL00
046800001207     C                   SETON                                        80
046900001207     C                   MOVEL     MSG(4)        DSMSMS
047000001207     C                   MOVEL     *all'*'       DESSOC
047100001207     C                   ELSE
047200001207     C                   MOVEL(p)  SIFDESBREV    DESSOC
047300001207     C                   ENDIF
047400001207     C                   ENDIF
047500001207     C                   ENDIF
047600000927     C*--------------------
047700000927     C*  NAZIONE
047800000927     C*--------------------
047900000927     C                   IF        ERRL00 = '0'
048000000927     C     VIDNAR        IFNE      '999'
048100000927     C     '?'           SCAN      VIDNAR                                 90
048200000927     C*
048300000927     C     *IN90         IFEQ      *ON
048400000927     C                   MOVEL     '15'          COD
048500000927     C                   MOVEL     *BLANKS       VIDNAR
048600000927     C                   MOVEL     *BLANKS       DES              25
048700000927     C                   CLEAR                   KEYT
048800000927     C                   CALL      'X§TABER'
048900000927     C                   PARM                    CODUT
049000000927     C                   PARM                    COD
049100000927     C                   PARM                    KEYT
049200000927     C                   PARM                    DES
049300000927     C                   MOVEL     KEYT          VIDNAR
049400000927     C                   MOVEL     DES           DESNAR
049500000927     C                   ENDIF
049600000927     C* CONTROLLO VALIDITA'
049700000927     C                   MOVEL     '15'          COD
049800000927     C                   MOVEL     *BLANKS       KEYT
049900000927     C                   MOVEL     VIDNAR        KEYT
050000000927     C*
050100000927     C     KTAB          CHAIN     TABEL00F                           30
050200000927     C                   IF        *IN30 = *ON OR TBLFLG <> ' '
050300000927     C                   MOVEL     '1'           ERRL00
050400000927     C                   SETON                                        72
050500000927     C                   MOVEL     MSG(07)       DSMSMS
050600000927     C                   ENDIF
050700000927     C                   ENDIF
050800000927     C                   ENDIF
050900000927     C*--------------------
051000000927     C*  PROVINCIA
051100000927     C*--------------------
051200000927     C                   IF        ERRL00 = '0'
051300000927     C     '?'           SCAN      VIDPRO                                 90
051400000927     C*
051500000927     C     *IN90         IFEQ      *ON
051600000927     C                   MOVEL     'PR'          COD
051700000927     C                   MOVEL     *BLANKS       VIDPRO
051800000927     C                   MOVEL     *BLANKS       DES              25
051900000927     C                   CLEAR                   KEYT
052000000927     C                   CALL      'X§TABER'
052100000927     C                   PARM                    CODUT
052200000927     C                   PARM                    COD
052300000927     C                   PARM                    KEYT
052400000927     C                   PARM                    DES
052500000927     C                   MOVEL     KEYT          VIDPRO
052600000927     C                   MOVEL     DES           DESPRO
052700000927     C                   ENDIF
052800000927     C* CONTROLLO VALIDITA' DELLA PROVINCIA IMMESSA
052900000927     C     VIDPRO        IFNE      *BLANKS
053000000927     C                   MOVEL     'PR'          COD
053100000927     C                   MOVEL     *BLANKS       KEYT
053200000927     C                   MOVEL     VIDPRO        KEYT
053300000927     C*
053400000927     C     KTAB          CHAIN     TABEL                              30
053500000927     C                   IF        *IN30 = *OFF AND TBLFLG = ' '
053600000927     C* DECODIFICA PROVINCIA
053700000927     C                   MOVEL     TBLUNI        DESPRO
053800000927     C                   ELSE
053900000927     C* PROVINCIA ERRATA
054000000927     C                   MOVEL     '1'           ERRL00
054100000927     C                   SETON                                        74
054200000927     C                   MOVEL     MSG(08)       DSMSMS
054300000927     C                   CLEAR                   DESPRO
054400000927     C                   ENDIF
054500000927     C                   ENDIF
054600000927     C                   ENDIF
054700000927     C*--------------------
054800000927     C*  C. A. P.
054900000927     C*--------------------
055000000927     C                   IF        ERRL00 = '0'
055100000927     C     VIDCAP        IFNE      *BLANKS
055200000927     C                   CLEAR                   DSSI95
055300000927     C                   CLEAR                   DSLV13
055400000927     C                   MOVEL     VIDPRO        I95PRV
055500000927     C                   MOVEL     VIDCAP        I95CAP
055600000927     C                   MOVEL     VIDNAR        I95NAR
055700000927     C                   MOVEL     DATCOR        I95DAT
055800000927     C                   MOVEL     '3'           I95TCN
055900000927     C* CONTROLLO CON LA PROVNCIA SOLO SE C'E'
056000000927     C     VIDPRO        IFNE      *BLANKS
056100000927     C                   MOVEL     'S'           I13CNG
056200000927     C                   MOVEL     'S'           I13AF0
056300000927     C                   ENDIF
056400000927     C*
056500000927     C                   CALL      'FNLV13R'
056600000927     C                   PARM                    KPJBA
056700000927     C                   PARM                    DSLV13
056800000927     C                   PARM                    DSSI95
056900000927     C* ERRORE
057000000927     C                   IF        O13ERR <> ' '
057100000927     C                   MOVEL     '1'           ERRL00
057200000927     C                   SETON                                        73
057300000927     C                   MOVEL     MSG(09)       DSMSMS
057400000927     C                   ENDIF
057500000927     C                   ENDIF
057600000927     C                   ENDIF
057700000927     C*--------------------
057800000927     C*  CONTROLLO SCELTE ALTERNATIVE
057900000927     C*--------------------
058000000927     C                   IF        ERRL00 = '0'
058100000927     C                   IF        VIDKSC = *ALL'0'
058200000927     C                   EVAL      VIDKSC = *BLANKS
058300000927     C                   ENDIF
058400000927     C                   IF         VIDKSC <> *blanks    and
058500000927     C                             (VIDFIL <> *zeros     or
058600000927     C                              VIDABL <> *blanks    or
058700060220     C                              VIDTBC <> *blanks    or
058800060221     C                              VIDRGR <> *blanks    or
058900060222     C                              VIDOVR <> *blanks    or
059000060222     C                              VIDCHK <> *blanks    or
059100000927     C                              VIDCAP <> *blanks    or
059200000927     C                              VIDPRO <> *blanks    or
059300000927     C                              VIDBAR <> *blanks    or
059400000927     C                              VIDSDI <> *blanks    or
059500000927     C                              VIDPIC <> *blanks)
059600000927     C                   MOVEL     '1'           ERRL00
059700000927     C                   SETON                                        707172
059800000927     C                   SETON                                        737475
059900060529     C                   SETON                                        767778
060000000927     C                   MOVEL     MSG(11)       DSMSMS
060100000927     C                   ENDIF
060200000927     C                   IF         VIDKSC = *blanks    and
060300000927     C                              VIDFIL = *zeros     and
060400000927     C                              VIDABL = *blanks    and
060500060220     C                              VIDTBC = *blanks    and
060600060221     C                              VIDRGR = *blanks    and
060700060222     C                              VIDOVR = *blanks    and
060800060222     C                              VIDCHK = *blanks    and
060900000927     C                              VIDCAP = *blanks    and
061000000927     C                              VIDPRO = *blanks    and
061100000927     C                              VIDBAR = *blanks    and
061200000927     C                              VIDSDI = *blanks    and
061300000927     C                              VIDPIC = *blanks
061400000927     C                   MOVEL     '1'           ERRL00
061500000927     C                   SETON                                        707172
061600000927     C                   SETON                                        737477
061700060529     C                   SETON                                        78
061800000927     C                   MOVEL     MSG(12)       DSMSMS
061900000927     C                   ENDIF
062000000927     C                   ENDIF
062100000927     C*--------------------
062200000927     C*  ALMENO UN FLAG VALORIZZATO A 'S' PER BAR/SDI/PICCOLE
062300000927     C*--------------------
062400000927     C                   IF        ERRL00 = '0'
062500001211     C                   IF        VIDSOC = *blanks
062600000927     C                   IF        VIDKSC=*blanks
062700000927     C                   IF        VIDBAR=*blanks
062800000927     C                   MOVEL     'N'           VIDBAR
062900000927     C                   ENDIF
063000000927     C                   IF        VIDSDI=*blanks
063100000927     C                   MOVEL     'N'           VIDSDI
063200000927     C                   ENDIF
063300000927     C                   IF        VIDPIC=*blanks
063400000927     C                   MOVEL     'N'           VIDPIC
063500000927     C                   ENDIF
063600000927     C                   IF        (VIDBAR='N' OR VIDBAR=' ') AND
063700000927     C                             (VIDSDI='N' OR VIDSDI=' ') AND
063800000927     C                             (VIDPIC='N' OR VIDPIC=' ')
063900000927     C                   MOVEL     '1'           ERRL00
064000000927     C                   SETON                                        77
064100000927     C                   MOVEL     MSG(14)       DSMSMS
064200000927     C                   ENDIF
064300000927     C                   ENDIF
064400001211     C                   ENDIF
064500000927     C                   ENDIF
064600000000     C*
064700000000     C                   ENDSR
064800000000     C*------------------------------------------------------------------------*
064900000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
065000000000     C*------------------------------------------------------------------------*
065100000000     C     AZZVID        BEGSR
065200000000     C*
065300060615     C* REPERISCO L'ULTIMA DATA SALDI CLIENTE
065400060615     C                   MOVEL     '5S'          COD
065500060615     C                   MOVEL     *BLANKS       KEYT
065600060615     C*
065700060615     C     KTAB          CHAIN     TABEL00F                           30
065800060615     C                   IF        *IN30 = *ON OR TBLFLG <> ' '
065900060615     C                   Z-ADD     DATCOR        G08INV
066000060615     C                   ELSE
066100060615     C                   EVAL      DS5S = TBLUNI
066200060615     C                   Z-ADD     §5SDAC        G08INV
066300060615     C                   ENDIF
066400060615     C*
066500060615     C* GIRO LA DATA
066600060615     C                   Z-ADD     *ZEROS        G08DAT
066700060615     C                   MOVEL     '3'           G08ERR
066800060615     C                   CALL      'XSRDA8'
066900060615     C                   PARM                    WLBDA8
067000060615     C                   Z-ADD     G08DAT        VIDDAA
067100060615     C*
067200060615     C* Calcolo 12 mesi addietro la data di lancio
067300060615     C                   Z-ADD     VIDDAA        DATAINV
067400060615     C                   EVAL      ANNOINV=ANNOINV-1
067500060615     C                   Z-ADD     DATAINV       VIDDAD
067600060717     C                   MOVEL     '01'          VIDDAD
067700060615     C*
067800000927     C                   MOVEL     *blanks       VIDKSC
067900000927     C                   MOVEL     *blanks       VIDABL
068000060220     C                   MOVEL     'S'           VIDTBC
068100060221     C                   MOVEL     'N'           VIDRGR
068200060313     C                   MOVEL     'N'           VIDOVR
068300060222     C                   MOVEL     'S'           VIDCHK
068400060615     C                   MOVEL     'S'           VIDSOGN
068500000927     C                   Z-ADD     *zeros        VIDFIL
068600001207     C                   MOVEL     *blanks       VIDSOC
068700001207     C                   MOVEL     'C'           VIDCOF
068800060313     C                   MOVEL     '999'         VIDNAR
068900000927     C                   MOVEL     *blanks       VIDCAP
069000000927     C                   MOVEL     *blanks       VIDPRO
069100000927     C                   Z-ADD     1             VIDFAT
069200001207     C                   MOVEL     'N'           VIDCPI
069300000927     C                   MOVEL     'S'           VIDBAR
069400000927     C                   MOVEL     'S'           VIDSDI
069500000927     C                   MOVEL     'N'           VIDPIC
069600050711     C***                MOVEL     '02'          VIDNTN1
069700050711     C***                MOVEL     '05'          VIDNTN2
069800050711     C***                MOVEL     *blanks       VIDNTN3
069900050711     C***                MOVEL     *blanks       VIDNTN4
070000100730     C                   MOVEL     '03'          VIDNTE1
070100100730     C                   MOVEL     '06'          VIDNTE2
070200050711     C                   MOVEL     '88'          VIDNTE3
070300050711     C                   MOVEL     '85'          VIDNTE4
070400060202     C                   MOVEL     '84'          VIDNTE5
070500060202     C                   MOVEL     '87'          VIDNTE6
070600060202     C                   MOVEL     '08'          VIDNTE7
070700050711     C                   MOVEL     *blanks       VIDNTE8
070800050711     C                   MOVEL     'S'           VIDIND
070900000000     C*
071000000927     C* Descrizioni
071100000927     C                   MOVEL     *blanks       DESKSC
071200000927     C                   MOVEL     *blanks       DESFIL
071300001207     C                   MOVEL     *blanks       DESSOC
071400000927     C                   MOVEL(p)  CNAZ          DESNAR
071500000927     C                   MOVEL     *blanks       DESPRO
071600000927     C*
071700000000     C                   ENDSR
071800000000     C*------------------------------------------------------------------------*
071900991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
072000000000     C*------------------------------------------------------------------------*
072100000000     C     VIDPAR        BEGSR
072200000000     C*
072300000914     C                   Z-ADD     DAIVID        PARDAI
072400000914     C                   Z-ADD     DAFVID        PARDAF
072500000927     C                   Z-ADD     VIDFAT        PARFAT
072600000927     C                   MOVEL     VIDKSC        PARKSC
072700000927     C                   Z-ADD     VIDFIL        PARFIL
072800000927     C                   MOVEL     VIDNAR        PARNAZ
072900000927     C                   MOVEL     VIDCAP        PARCAP
073000000927     C                   MOVEL     VIDPRO        PARPRV
073100000927     C                   MOVEL     VIDABL        PARBLK
073200060220     C                   MOVEL     VIDTBC        PARTBC
073300060221     C                   MOVEL     VIDRGR        PARRGR
073400060222     C                   MOVEL     VIDOVR        PAROVR
073500060222     C                   MOVEL     VIDCHK        PARCHK
073600000927     C                   MOVEL     *blanks       PARCIC
073700000927     C                   MOVEL     VIDBAR        PARBAR
073800000927     C                   MOVEL     VIDSDI        PARSDI
073900000927     C                   MOVEL     VIDPIC        PARPIC
074000000927     C                   MOVEL     'S'           PARFOA
074100050711     C***                MOVEL     VIDNTN1       PARNTN1
074200050711     C***                MOVEL     VIDNTN2       PARNTN2
074300050711     C***                MOVEL     VIDNTN3       PARNTN3
074400050711     C***                MOVEL     VIDNTN4       PARNTN4
074500050711     C                   MOVEL     VIDNTE1       PARNTE1
074600050711     C                   MOVEL     VIDNTE2       PARNTE2
074700050711     C                   MOVEL     VIDNTE3       PARNTE3
074800050711     C                   MOVEL     VIDNTE4       PARNTE4
074900050711     C                   MOVEL     VIDNTE5       PARNTE5
075000050711     C                   MOVEL     VIDNTE6       PARNTE6
075100050711     C                   MOVEL     VIDNTE7       PARNTE7
075200050711     C                   MOVEL     VIDNTE8       PARNTE8
075300001207     C                   MOVEL     VIDSOC        PARSOC
075400001207     C                   MOVEL     VIDCOF        PARCOF
075500001207     C                   MOVEL     VIDCPI        PARCPI
075600011016     C                   MOVEL     VIDIND        PARIND
075700060615     C                   MOVEL     VIDSOGN       PARSOGN
075800000000     C*
075900000000     C                   ENDSR
076000000927     C*------------------------------------------------------------------------*
076100000927     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
076200000927     C*------------------------------------------------------------------------*
076300000927     C     CARORG        BEGSR
076400000927     C*
076500000927     C*---
076600000927     C* FILIALI O AGENZIE
076700000927     C*---
076800000927     C                   z-add     1             idx
076900000927     C*
077000000927     C     *loval        setll(e)  azorg01l
077100000927     C                   read(e)   azorg01l
077200000927     C                   dow       not(%eof)
077300000927     C                   if        (orgfag='A' or orgfag='F') and orgfva=*blanks
077400000927     C                   z-add     orgfil        FILAGE(idx)
077500000927     C                   add       1             idx
077600000927     C                   endif
077700000927     C                   read(e)   azorg01l
077800000927     C                   enddo
077900000927     C*
078000000927     C                   ENDSR
078100020923     C*--------------------------------------------------------------------------------------------*
078200020923     C* REPERISCE I DATI UTENTE
078300020923     C*--------------------------------------------------------------------------------------------*
078400020923     C     REPDATIUTE    BEGSR
078500020923     C*
078600020923     C* INIZIALIZZA VARIABILI DI WRK
078700020923     C                   CLEAR                   TIBS34DS
078800020923     C                   CLEAR                   AZUTEDS
078900020923     C                   CLEAR                   DDATIUTE
079000020923     C*
079100020923     C     *DTAARA       DEFINE    §azute        azuteds
079200020923     C     *DTAARA       DEFINE    §datiute      ddatiute
079300020923     C                   IN(E)     *DTAARA
079400020923if  1C                   IF        %Error
079500020923     c                   EVAL      I34Tla = 'L'
079600020923     C                   CALL      'TIBS34R'
079700020923     C                   PARM                    Tibs34Ds
079800020923     C                   IN        *DTAARA
079900020923e   1C                   ENDIF
080000020923     C*
080100020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
080200020923     C                   MOVEL(P)  RSUT          DSFIRS           20
080300020923     C*
080400020923     C                   ENDSR
080500000000     C*------------------------------------------------------------------------*
080600980403     C* *INZSR - OPERAZIONI INIZIALI
080700000000     C*------------------------------------------------------------------------*
080800980403     C     *INZSR        BEGSR
080900000000     C*
081000000000     C* RICEVIMENTO PARAMETRI
081100000000     C     *ENTRY        PLIST
081200000000     C                   PARM                    KPJBA
081300020923     C*
081400020923     C* REPERISCE I DATI DELL'UTENTE
081500020923     C                   EXSR      REPDATIUTE
081600000000     C*
081700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
081800000000     C     XMSGPA        PLIST
081900000000     C                   PARM                    DSMSMS
082000000000     C                   PARM                    DSMSPG
082100000000     C                   PARM                    DSMSKE
082200000000     C                   PARM                    DSMSER
082300000000     C*
082400000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
082500000000     C                   CALL      'XKEY'
082600000000     C                   PARM                    DSKEY
082700000000     C*
082800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
082900000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
083000000927     C*
083100000927     C* DEFINIZIONE VARIABILI RIFERITE AL DATABASE
083200000927     C     *LIKE         DEFINE    TBLCOD        COD
083300000927     C     *LIKE         DEFINE    TBLKEY        KEYT
083400000000     C*
083500000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
083600000000     C*   -NOME PGM CHIAMANTE
083700000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
083800000927     C                   MOVEL     'TIST42R'     DSMSPG
083900000927     C                   MOVEL     'TIST42R'     MSGQUE
084000000000     C                   MOVEL     *BLANKS       MSGKEY
084100000000     C                   MOVEL     *BLANKS       DSMSMS
084200020923     C*
084300020923     C* INIZIALIZZO VARIABILI DI WRK
084400020923     C                   Z-ADD     1             CODUT             1 0
084500000000     C*
084600000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
084700000000     C                   Z-ADD     0             FUNGRU            5 0
084800000000     C*
084900000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
085000000000     C                   EXSR      FUNGES
085100000000     C*
085200000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
085300000000     C                   MOVEL     '0'           F12L00            1
085400000000     C                   MOVEL     '0'           F03L00            1
085500000000     C                   MOVEL     '0'           FINL00            1
085600000000     C                   MOVEL     '0'           ERRL00            1
085700000000     C                   MOVEL     '0'           FLFINE            1
085800000927     C*
085900000927     C* DEFINIZIONE CHIAVI - TABEL00F
086000000927     C     KTAB          KLIST
086100000927     C                   KFLD                    CODUT
086200000927     C                   KFLD                    COD
086300000927     C                   KFLD                    KEYT
086400000927     C*
086500000927     C* CALCOLA LA DATA CORRENTE
086600000927     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
086700000927     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
086800000927     C                   Z-ADD     WN8           G08DAT
086900000927     C                   Z-ADD     *ZEROS        G08INV
087000000927     C                   MOVEL     '0'           G08ERR
087100000927     C                   CALL      'XSRDA8'
087200000927     C                   PARM                    WLBDA8
087300000927     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
087400000927     C*
087500000927     C* CARICO LE FILIALI O AGENZIE DA ORGANIGRAMMA
087600000927     C                   EXSR      CARORG
087700000000     C*
087800000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
087900000000     C                   EXSR      AZZVID
088000000000     C*
088100000000     C                   ENDSR
088200000000     C*------------------------------------------------------------------------*
088300000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
088400000000     C*------------------------------------------------------------------------*
088500000000     C     FUNGES        BEGSR
088600000000     C*
088700000000     C* PULISCO CAMPI DI LAVORO
088800000000     C                   Z-ADD     0             W                 5 0
088900000000     C*
089000000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
089100000000     C* E CARICO TABELLA FUNZIONI ATTIVE
089200000000     C     1             DO        24            I
089300000000     C                   MOVEL     FUD(I)        FUDRIG
089400000000     C*
089500000000     C* PULIZIA FUNZIONI PRECEDENTI
089600000000     C                   MOVEL     *BLANKS       FUV(I)
089700000000     C*
089800000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
089900000000     C* (SALTA F24)
090000000000     C     FUDATT        IFEQ      'S'
090100000000     C     FUDFUN        ANDNE     '24'
090200000000     C                   ADD       1             W
090300000000     C                   MOVEL     FUDDES        FUV(W)
090400000000     C                   END
090500000000     C                   END                                                    DO WHILE
090600000000     C*
090700000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
090800000000     C* ALTRIMENTI DISATTIVO F24
090900000000     C     1             DO        24            I
091000000000     C                   MOVEL     FUD(I)        FUDRIG
091100000000     C*
091200000000     C     FUDFUN        IFEQ      '24'
091300000000     C     W             IFGT      6
091400000000     C                   MOVEL     'S'           FUDATT
091500000000     C                   ELSE
091600000000     C                   MOVEL     *BLANKS       FUDATT
091700000000     C                   END                                                    W GT 8
091800000000     C                   MOVEL     FUDRIG        FUD(I)
091900000000     C                   END
092000000000     C                   END                                                    DO WHILE
092100000000     C*
092200000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
092300000000     C     W             DIV       6             I
092400000000     C                   MVR                     Z
092500000000     C*
092600000000     C     Z             IFNE      0
092700000000     C     I             ADD       1             Z
092800000000     C                   ELSE
092900000000     C                   Z-ADD     I             Z                 5 0
093000000000     C                   END
093100000000     C*
093200000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
093300000000     C                   ADD       1             FUNGRU
093400000000     C     FUNGRU        IFGT      Z
093500000000     C     FUNGRU        ORLT      1
093600000000     C                   Z-ADD     1             FUNGRU
093700000000     C                   END
093800000000     C*
093900000000     C* PRIMO GRUPPO DI 6 FUNZIONI
094000000000     C     FUNGRU        IFEQ      1
094100000000     C                   MOVEL     FUV(1)        FUN001
094200000000     C                   MOVEL     FUV(2)        FUN002
094300000000     C                   MOVEL     FUV(3)        FUN003
094400000000     C                   MOVEL     FUV(4)        FUN004
094500000000     C                   MOVEL     FUV(5)        FUN005
094600000000     C                   MOVEL     FUV(6)        FUN006
094700000000     C                   END
094800000000     C*
094900000000     C* SECONDO GRUPPO DI 6 FUNZIONI
095000000000     C     FUNGRU        IFEQ      2
095100000000     C                   MOVEL     FUV(7)        FUN001
095200000000     C                   MOVEL     FUV(8)        FUN002
095300000000     C                   MOVEL     FUV(9)        FUN003
095400000000     C                   MOVEL     FUV(10)       FUN004
095500000000     C                   MOVEL     FUV(11)       FUN005
095600000000     C                   MOVEL     FUV(12)       FUN006
095700000000     C                   END
095800000000     C*
095900000000     C* TERZO GRUPPO DI 6 FUNZIONI
096000000000     C     FUNGRU        IFEQ      3
096100000000     C                   MOVEL     FUV(13)       FUN001
096200000000     C                   MOVEL     FUV(14)       FUN002
096300000000     C                   MOVEL     FUV(15)       FUN003
096400000000     C                   MOVEL     FUV(16)       FUN004
096500000000     C                   MOVEL     FUV(17)       FUN005
096600000000     C                   MOVEL     FUV(18)       FUN006
096700000000     C                   END
096800000000     C*
096900000000     C* QUARTO GRUPPO DI 6 FUNZIONI
097000000000     C     FUNGRU        IFEQ      4
097100000000     C                   MOVEL     FUV(19)       FUN001
097200000000     C                   MOVEL     FUV(20)       FUN002
097300000000     C                   MOVEL     FUV(21)       FUN003
097400000000     C                   MOVEL     FUV(22)       FUN004
097500000000     C                   MOVEL     *BLANKS       FUN005
097600000000     C                   MOVEL     *BLANKS       FUN006
097700000000     C                   END
097800000000     C*
097900000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
098000000000     C                   MOVEL     *BLANKS       FUN007
098100000000     C                   MOVEL     *BLANKS       FUN008
098200000000     C*
098300000000     C     1             DO        24            I
098400000000     C                   MOVEL     FUD(I)        FUDRIG
098500000000     C     FUDFUN        IFEQ      '24'
098600000000     C     FUDATT        ANDEQ     'S'
098700000000     C                   MOVEL     FUDDES        FUN008
098800000000     C                   END
098900000000     C                   END                                                    DO
099000000000     C                   ENDSR
099100000000     C*------------------------------------------------------------------------*
099200000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
099300000000     C*------------------------------------------------------------------------*
099400000000     C     FUNCON        BEGSR
099500000000     C*
099600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
099700000000     C                   EXSR      FUNVAR
099800000000     C*
099900000000     C* PULISCO CAMPI DI LAVORO
100000000000     C                   Z-ADD     0             I                 5 0
100100000000     C                   Z-ADD     0             W                 5 0
100200000000     C*
100300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
100400000000     C     FUNKEY        IFNE      *BLANKS
100500000000     C     1             DO        24            I
100600000000     C                   MOVEL     FUD(I)        FUDRIG
100700000000     C*
100800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
100900000000     C     FUNKEY        IFEQ      FUDFUN
101000000000     C     FUDATT        ANDEQ     'S'
101100000000     C                   Z-ADD     I             W
101200000000     C                   END
101300000000     C                   END                                                    DO WHILE
101400000000     C*
101500000000     C* FUNZIONE NON ATTIVA (ERRORE)
101600000000     C     W             IFEQ      0
101700000000     C*
101800000000     C* EMISSIONE MESSAGGIO DI ERRORE
101900000000     C                   MOVEL     MSG(1)        DSMSMS
102000000000     C*
102100000000     C                   END                                                    W=0
102200000000     C                   END                                                    FUNKEY NE *BLANKS
102300000000     C*
102400000000     C* FUNZIONE ATTIVA (PROCEDI ...)
102500000000     C     W             IFNE      0
102600000000     C     FUNKEY        OREQ      *BLANKS
102700000000     C*
102800000000     C     FUNKEY        IFEQ      *BLANKS
102900000000     C                   EXSR      ENTGES
103000000000     C                   END
103100000000     C     FUNKEY        IFEQ      '03'
103200000000     C                   EXSR      F03GES
103300000000     C                   END
103400000000     C     FUNKEY        IFEQ      '06'
103500000000     C                   EXSR      F06GES
103600000000     C                   END
103700000927     C     FUNKEY        IFEQ      '07'
103800000927     C                   EXSR      F07GES
103900000927     C                   END
104000000000     C     FUNKEY        IFEQ      '12'
104100000000     C                   EXSR      F12GES
104200000000     C                   END
104300000000     C     FUNKEY        IFEQ      '24'
104400000000     C                   EXSR      F24GES
104500000000     C                   END
104600000000     C*
104700000000     C                   END
104800000000     C*
104900000000     C                   ENDSR
105000000000     C*------------------------------------------------------------------------*
105100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
105200000000     C*------------------------------------------------------------------------*
105300000000     C     FUNVAR        BEGSR
105400000000     C*
105500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
105600000000     C*
105700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
105800000000     C     KEY           IFEQ      ENTER
105900000000     C                   MOVEL     *BLANKS       FUNKEY
106000000000     C                   END
106100000000     C     KEY           IFEQ      F1
106200000000     C                   MOVEL     '01'          FUNKEY
106300000000     C                   END
106400000000     C     KEY           IFEQ      F2
106500000000     C                   MOVEL     '02'          FUNKEY
106600000000     C                   END
106700000000     C     KEY           IFEQ      F3
106800000000     C                   MOVEL     '03'          FUNKEY
106900000000     C                   END
107000000000     C     KEY           IFEQ      F4
107100000000     C                   MOVEL     '04'          FUNKEY
107200000000     C                   END
107300000000     C     KEY           IFEQ      F5
107400000000     C                   MOVEL     '05'          FUNKEY
107500000000     C                   END
107600000000     C     KEY           IFEQ      F6
107700000000     C                   MOVEL     '06'          FUNKEY
107800000000     C                   END
107900000000     C     KEY           IFEQ      F7
108000000000     C                   MOVEL     '07'          FUNKEY
108100000000     C                   END
108200000000     C     KEY           IFEQ      F8
108300000000     C                   MOVEL     '08'          FUNKEY
108400000000     C                   END
108500000000     C     KEY           IFEQ      F9
108600000000     C                   MOVEL     '09'          FUNKEY
108700000000     C                   END
108800000000     C     KEY           IFEQ      F10
108900000000     C                   MOVEL     '10'          FUNKEY
109000000000     C                   END
109100000000     C     KEY           IFEQ      F11
109200000000     C                   MOVEL     '11'          FUNKEY
109300000000     C                   END
109400000000     C     KEY           IFEQ      F12
109500000000     C                   MOVEL     '12'          FUNKEY
109600000000     C                   END
109700000000     C     KEY           IFEQ      F13
109800000000     C                   MOVEL     '13'          FUNKEY
109900000000     C                   END
110000000000     C     KEY           IFEQ      F14
110100000000     C                   MOVEL     '14'          FUNKEY
110200000000     C                   END
110300000000     C     KEY           IFEQ      F15
110400000000     C                   MOVEL     '15'          FUNKEY
110500000000     C                   END
110600000000     C     KEY           IFEQ      F16
110700000000     C                   MOVEL     '16'          FUNKEY
110800000000     C                   END
110900000000     C     KEY           IFEQ      F17
111000000000     C                   MOVEL     '17'          FUNKEY
111100000000     C                   END
111200000000     C     KEY           IFEQ      F18
111300000000     C                   MOVEL     '18'          FUNKEY
111400000000     C                   END
111500000000     C     KEY           IFEQ      F19
111600000000     C                   MOVEL     '19'          FUNKEY
111700000000     C                   END
111800000000     C     KEY           IFEQ      F20
111900000000     C                   MOVEL     '20'          FUNKEY
112000000000     C                   END
112100000000     C     KEY           IFEQ      F21
112200000000     C                   MOVEL     '21'          FUNKEY
112300000000     C                   END
112400000000     C     KEY           IFEQ      F22
112500000000     C                   MOVEL     '22'          FUNKEY
112600000000     C                   END
112700000000     C     KEY           IFEQ      F23
112800000000     C                   MOVEL     '23'          FUNKEY
112900000000     C                   END
113000000000     C     KEY           IFEQ      F24
113100000000     C                   MOVEL     '24'          FUNKEY
113200000000     C                   END
113300000000     C     KEY           IFEQ      ROLLUP
113400000000     C                   MOVEL     'RU'          FUNKEY
113500000000     C                   END
113600000000     C     KEY           IFEQ      HELP
113700000000     C                   MOVEL     'HP'          FUNKEY
113800000000     C                   END
113900000000     C                   ENDSR
114000000000     C*------------------------------------------------------------------------*
114100000000     C* NOTE PER COMPILAZIONE SCHIERE
114200000000     C*------------------------------------------------------------------------*
114300000000     C* -MSG-
114400000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
114500000000     C* (NON TOCCARE MSG,1)
114600000000     C*
114700000000     C* -CMD-
114800000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
114900000000     C*
115000000000     C* -FUD-
115100000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
115200000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
115300000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
115400000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
115500000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
115600000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
115700000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
115800000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
115900000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
116000000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
116100000000     C*
116200000000     C*------------------------------------------------------------------------*
116300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
116400000927TIST42R- Tasto funzionale non gestito.                                         1
116500000927TIST42R-
116600000927TIST42R- Data non valida.                                                      3
116700001207TIST42R- Società indicata non presente in Contabilità, correggere.             4
116800001207TIST42R-                                                                       5
116900000927TIST42R- Data inizio maggiore data fine.                                       6
117000000927TIST42R- Nazione inesistente o errata.                                         7
117100000927TIST42R- Provincia inesistente o errata.                                       8
117200000927TIST42R- Deve essere effettuata almeno una scelta.                             9
117300000927TIST42R- Punto Operativo inesistente o annullato.                             10
117400000927TIST42R- Sono state effettuate scelte alternative, correggere.                11
117500000927TIST42R- Effettuare almeno una scelta di parzializzazione.                    12
117600000927TIST42R- Codice cliente errato o non valido.                                  13
117700000927TIST42R- Occorre specificare almeno una delle opzioni.                        14
117800000927TIST42R-
117900000927TIST42R-
118000000927TIST42R-
118100000927TIST42R-
118200000927TIST42R-
118300000927TIST42R-
118400000000** ======== SCHIERA: CMD  (COMANDI)                        ================
118500000000WRKSPLF
118600000000
118700000000
118800000000
118900000000
119000000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
119100000000                   -        (S/N)=N                        funz.01¬
119200000000                   -        (S/N)=N                        funz.02¬
119300000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
119400000000                   -        (S/N)=N                        funz.04¬
119500000000                   -        (S/N)=N                        funz.05¬
119600000000F06=Lancio         -        (S/N)=S                        funz.06¬
119700000927F07=Imposta DFT    -        (S/N)=N                        funz.07¬
119800000000                   -        (S/N)=N                        funz.08¬
119900000000                   -        (S/N)=N                        funz.09¬
120000000000                   -        (S/N)=N                        funz.10¬
120100000000                   -        (S/N)=N                        funz.11¬
120200000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
120300000000                   -        (S/N)=N                        funz.13¬
120400000000                   -        (S/N)=N                        funz.14¬
120500000000                   -        (S/N)=N                        funz.15¬
120600000000                   -        (S/N)=N                        funz.16¬
120700000000                   -        (S/N)=N                        funz.17¬
120800000000                   -        (S/N)=N                        funz.18¬
120900000000                   -        (S/N)=N                        funz.19¬
121000000000                   -        (S/N)=N                        funz.20¬
121100000000                   -        (S/N)=N                        funz.21¬
121200000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
121300000000                   -        (S/N)=N                        funz.23¬
121400000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
