000100000000     H*------------------------------------------------------------------------*
000200040827     H* SITUAZIONE CLIENTI COME "VAS"
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800040827     FTIST31D   CF   E             WORKSTN INFDS(DEVDS1)
000900040827     F                                     SFILE(ST31DMS:MSGULT)
001000991117     FTABEL00F  IF   E           K DISK
001100000000     FAZORG01L  IF   E           K DISK
001200991118     FTIVSS02L  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI
001800000000     D*-------------------
001900011228     D MSG             S             78    DIM(21) CTDATA PERRCD(1)
002000000000     D*-------------------
002100000000     D* DESCRIZIONI TASTI FUNZIONALI
002200000000     D*-------------------
002300000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400000000     D*-------------------
002500000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002600000000     D*-------------------
002700000000     D FUV             S             19    DIM(24)
002800000000     D*------------------------------------------------------------------------*
002900000000     D* INPUT
003000000000     D*------------------------------------------------------------------------*
003100000000     D*-------------------
003200000000     D* RIDEFINIZIONE SCHIERA -FUD-
003300000000     D*-------------------
003400000000     D FUDDS           DS
003500000000     D*                -DESCRIZIONE PARZIALE
003600000000     D  FUDDE1                 1      1
003700000000     D*                -FUNZIONE
003800000000     D  FUDFUN                 2      3
003900000000     D*                -DESCRIZIONE PARZIALE
004000000000     D  FUDDE2                 4     19
004100000000     D*                -FILLER
004200000000     D  FUDFI0                20     34
004300000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004400000000     D  FUDATT                35     35
004500000000     D*                -FILLER
004600000000     D  FUDFI1                36     80
004700000000     D*                -DESCRIZIONE COMPLETA
004800000000     D  FUDDES                 1     19
004900000000     D*                -RIDEFINIZIONE RIGA
005000000000     D  FUDRIG                 1     80
005100000000     D*-------------------
005200000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005300000000     D*-------------------
005400000000     D DSMSG         E DS
005500000000     D*-------------------
005600020917     D* REPERIMENTO DATI UTENTE
005700020917     D*-------------------
005800020917     D TIBS34DS      E DS                                                       *Profili utente
005900020917     D DDATIUTE      E DS                                                       *Dati utente
006000020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006100970401     D*----------------
006200970401     D* DS PER TABELLA CODICI AREA
006300970401     D*----------------
006400970401     D DS05          E DS
006500970401     D*----------------
006600970401     D* DS PER TABELLA CODICI DISTRETTO
006700970401     D*----------------
006800970401     D DS17          E DS
006900000000     D*-------------------
007000000000     D* DS PARAMETRI DI LANCIO PGM
007100000000     D*-------------------
007200950703     D PARAM           DS
007300040827     D  PARDIV                        1
007400040827     D  PARARE                        3  0
007500040827     D  PARFIL                        3  0
007600040827     D  PARKSC                        7  0
007700060109     D  PARTTNU                       1
007800060109     D  PARSDNU                       1
007900060109     D  PARDISA                       1
008000060109     D  PARWTAB                       1
008100000000     D*-------------------
008200000000     D* DS PER GESTIONE TASTI FUNZIONALI
008300000000     D*-------------------
008400000000     D DSKEY         E DS
008500000000     D DEVDS1          DS
008600000000     D  KEY                  369    369
008700000000     D*-------------------
008800000000     D* ARCHITETTURA
008900000000     D*-------------------
009000000000     D KPJBA         E DS
009100011228     D*-------------------
009200991118     D* DS REPERIMENTO DATI UTENTE
009300991118     D*-------------------
009400991118     DBS69DS         E DS                  extname(TIBS69DS)
009500991118     DACODS          E DS                  extname(CNACO00F)
009600991118     DINDDS          E DS                  extname(CNIND00F)
009700991118     DCLPDS          E DS                  extname(CNCLP00F)
009800991118     DCLSDS          E DS                  extname(FNCLS00F)
009900020805     D*-------------------
010000020805     D* CAPOCONTO / SOTTOCONTO DI RITORNO "XCLIR"
010100020805     D*-------------------
010200040827     D KSC             S              7  0 DIM(30)
010300020805     D KSA             S              4    DIM(30)                              *ALFANUMERICO
010400020805     D*------------------
010500020805     D* DS "XCLIR"
010600020805     D*------------------
010700040827     D DSXC          E DS                  EXTNAME(XCLIRDS)
010800020805     D*------------------
010900020805     D* DS PER PASSAGGIO CAMPI PACKED
011000020805     D*------------------
011100020805     D                 DS
011200020805     D  DSKSN                  1      4P 0
011300020805     D  DSKSA                  1      4
011400000000     C*------------------------------------------------------------------------*
011500000000     C* MAIN LINE
011600000000     C*------------------------------------------------------------------------*
011700000000     C*
011800000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
011900000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
012000060109     C*
012100060109     C* INIZIALIZZO CAMPI A VIDEO
012200060109     C                   EXSR      AZZVID
012300000000     C*
012400000000     C* CICLO FINO A CHE:
012500000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012600000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012700000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
012800000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
012900000000     C     F12L00        OREQ      '1'
013000000000     C*
013100000000     C* EMETTE FORMATO VIDEO
013200000000     C                   EXSR      BUTFMR
013300000000     C*
013400000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
013500000000     C                   EXSR      FUNCON
013600000000     C*
013700000000     C                   END                                                    <-- 2 --
013800000000     C*
013900000000     C                   END                                                    <-- 1 --
014000991118     C*
014100991118     C* OPERAZIONI FINALI
014200991118     C                   EXSR      FINSR
014300991118     C*
014400000000     C                   SETON                                        LR
014500991118     C*------------------------------------------------------------------------*
014600991118     C* FINSR - OPERAZIONI FINALI
014700991118     C*------------------------------------------------------------------------*
014800991118     C     FINSR         BEGSR
014900991118     C*
015000991118     C* CHIUDE I FILE DEI PGM CHIAMATI
015100991118     C                   CLEAR                   BS69DS
015200991118     C                   CLEAR                   ACODS
015300991118     C                   CLEAR                   INDDS
015400991118     C                   CLEAR                   CLPDS
015500991118     C                   CLEAR                   CLSDS
015600991118     C                   MOVEL     'C'           I69TLA
015700991118     C                   MOVEL     KNSIF         I69SIF
015800991118     C                   CALL      'TIBS69R'
015900991118     C                   PARM                    BS69DS
016000991118     C                   PARM                    ACODS
016100991118     C                   PARM                    INDDS
016200991118     C                   PARM                    CLPDS
016300991118     C                   PARM                    CLSDS
016400991118     C*
016500991118     C                   ENDSR
016600060109     C*------------------------------------------------------------------------*
016700060109     C* AZZVID - INIZIALIZZAZIONE CAMPI A VIDEO
016800060109     C*------------------------------------------------------------------------*
016900060109     C     AZZVID        BEGSR
017000060109     C*
017100060109     C                   EVAL      VIDTTNU = 'S'
017200060109     C                   EVAL      VIDSDNU = 'S'
017300060109     C                   EVAL      VIDDISA = 'S'
017400060109     C                   EVAL      VIDWTAB = 'S'
017500060109     C*
017600060109     C                   ENDSR
017700000000     C*------------------------------------------------------------------------*
017800000000     C* EMISSIONE FORMATO A VIDEO
017900000000     C*------------------------------------------------------------------------*
018000000000     C     BUTFMR        BEGSR
018100000000     C*
018200000000     C* WRITE RECORD TESTATA
018300040827     C                   WRITE     ST31D01
018400000000     C*
018500000000     C* VISUALIZZAZIONE MESSAGGIO
018600000000     C                   EXSR      SNDMSG
018700000000     C*
018800000000     C* WRITE RECORD FUNZIONI
018900040827     C                   WRITE     ST31D02
019000991019     C*
019100991019     C* EXFMT RECORD DETTAGLIO
019200040827     C                   EXFMT     ST31D03
019300991019     C*
019400000000     C* AZZERO FLAG DI ERRORE LIVELLO
019500000000     C                   MOVEL     '0'           ERRL00
019600000000     C*
019700040827     C* AZZERO INDICATORI DI ERRORE
019800040827     C                   SETOFF                                       787980
019900040827     C                   SETOFF                                       81
020000000000     C*
020100000000     C                   ENDSR
020200000000     C*------------------------------------------------------------------------*
020300000000     C* SNDMSG - MANDA MESSAGGIO
020400000000     C*------------------------------------------------------------------------*
020500000000     C     SNDMSG        BEGSR
020600000000     C*
020700000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
020800000000     C                   MOVEL     '1'           *IN40
020900040827     C                   WRITE     ST31DMC
021000000000     C                   MOVEL     '0'           *IN40
021100000000     C*
021200000000     C* VISUALIZZAZIONE MESSAGGIO
021300000000     C                   Z-ADD     0             MSGULT            4 0
021400000000     C                   CALL      'XMSG'        XMSGPA
021500000000     C     DSMSER        IFEQ      0
021600000000     C                   MOVEL     DSMSKE        MSGKEY
021700000000     C                   END
021800000000     C                   Z-ADD     1             MSGULT
021900040827     C                   WRITE     ST31DMS                                      SFL MESS
022000040827     C                   WRITE     ST31DMC                                      CONTR MESS
022100000000     C*
022200000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
022300000000     C                   MOVEL     *BLANKS       DSMSMS
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
022800000000     C*------------------------------------------------------------------------*
022900000000     C     ENTGES        BEGSR
023000000000     C*
023100000000     C* CONTROLLA I CAMPI IMMESSI
023200000000     C                   EXSR      DECODI
023300000000     C*
023400000000     C                   ENDSR
023500000000     C*------------------------------------------------------------------------*
023600000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
023700000000     C*------------------------------------------------------------------------*
023800000000     C     F03GES        BEGSR
023900000000     C*
024000000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
024100000000     C                   MOVEL     '1'           F03L00
024200000000     C*
024300000000     C                   ENDSR
024400000000     C*------------------------------------------------------------------------*
024500000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
024600000000     C*------------------------------------------------------------------------*
024700991020     C     F06GES        BEGSR
024800000000     C*
024900991105     C                   EXSR      DECODI
025000991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
025100991105     C     ERRL00        IFEQ      '0'
025200991105     C*
025300000000     C* LANCIO IL PROGRAMMA DI STAMPA
025400040827     C                   MOVEL     'ST32'        KCOAZ
025500000000     C                   EXSR      VIDPAR
025600950703     C                   MOVEL     PARAM         KPJBU
025700060116 MOM C                   CALL      'BCH09'
025800040903     C*                  CALL      'TIST32C'
025900060116     C                   PARM                    KPJBA
026000060116     C                   CALL      'BCH10'
026100991021     C                   PARM                    KPJBA
026200040901     C* DOPO IL LANCIO ESCO DALLA VIDEATA
026300040901     C                   EVAL      F03L00 = '1'
026400991105     C                   ENDIF
026500991105     C*
026600000000     C                   ENDSR
026700000000     C*------------------------------------------------------------------------*
026800000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
026900000000     C*------------------------------------------------------------------------*
027000000000     C     F12GES        BEGSR
027100000000     C*
027200000000     C* IMPOSTO FLAG DI LIVELLO
027300000000     C                   MOVEL     '1'           F12L00
027400000000     C*
027500000000     C                   ENDSR
027600000000     C*------------------------------------------------------------------------*
027700991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
027800000000     C*------------------------------------------------------------------------*
027900000000     C     F24GES        BEGSR
028000000000     C*
028100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
028200000000     C                   EXSR      FUNGES
028300000000     C*
028400000000     C                   ENDSR
028500991020     C*------------------------------------------------------------------------*
028600991105     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
028700000000     C*------------------------------------------------------------------------*
028800000000     C     DECODI        BEGSR
028900000000     C*
029000000000     C* CONTROLLO LA DIVISIONE
029100000000     C                   MOVEL     *BLANKS       VDDDIV
029200000000     C     ERRL00        IFEQ      '0'
029300000000     C     VIDDIV        IFNE      *BLANKS
029400000000     C                   EXSR      CHKDIV
029500000000     C     ERRL00        IFEQ      '1'
029600000000     C                   MOVEL     '1'           ERRL00
029700000000     C                   MOVEL     MSG(12)       DSMSMS
029800000000     C                   SETON                                        78
029900040827     C                   ENDIF
030000040827     C                   ENDIF
030100040827     C                   ENDIF
030200000000     C*
030300000000     C* CONTROLLO L'AREA
030400000000     C                   MOVEL     *BLANKS       VDDARE
030500000000     C     ERRL00        IFEQ      '0'
030600000000     C     VIDARE        IFNE      *BLANKS
030700000000     C                   EXSR      CHKARE
030800000000     C     ERRL00        IFEQ      '1'
030900000000     C                   MOVEL     '1'           ERRL00
031000000000     C                   MOVEL     MSG(13)       DSMSMS
031100000000     C                   SETON                                        79
031200040827     C                   ENDIF
031300040827     C                   ENDIF
031400040827     C                   ENDIF
031500000000     C*
031600000000     C* CONTROLLO LA FILIALE
031700000000     C                   MOVEL     *BLANKS       VDDFLP
031800000000     C     ERRL00        IFEQ      '0'
031900000000     C     VIDFLP        IFNE      *BLANKS
032000000000     C                   MOVE      VIDFLP        WLIN
032100000000     C                   EXSR      CHKLIN
032200000000     C     ERRL00        IFEQ      '0'
032300000000     C                   MOVEL     WLIN          VIDFLP
032400000000     C                   MOVEL     WDLIN         VDDFLP
032500000000     C                   ELSE
032600000000     C                   SETON                                        80
032700040827     C                   ENDIF
032800040827     C                   ENDIF
032900040827     C                   ENDIF
033000991117     C*
033100991117     C* CONTROLLO IL CLIENTE
033200991117     C                   MOVEL     *BLANKS       VDDKSC
033300991117     C     ERRL00        IFEQ      '0'
033400991117     C     VIDKSC        IFNE      *BLANKS
033500991117     C                   MOVE      VIDKSC        WKSC
033600991117     C                   EXSR      CHKKSC
033700991117     C     ERRL00        IFEQ      '0'
033800991117     C                   MOVEL     WKSC          VIDKSC
033900991117     C                   MOVEL     WDKSC         VDDKSC
034000991117     C                   ELSE
034100991117     C                   SETON                                        81
034200040827     C                   ENDIF
034300040827     C                   ENDIF
034400040827     C                   ENDIF
034500000000     C*
034600000000     C                   ENDSR
034700000000     C*------------------------------------------------------------------------*
034800000000     C*  CHKDIV - CONTROLLA LA DIVISIONE
034900000000     C*------------------------------------------------------------------------*
035000000000     C     CHKDIV        BEGSR
035100000000     C*
035200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
035300000000     C     VIDDIV        IFEQ      '?'                                          --- 1 -->
035400000000     C                   MOVEL     CODUT         §KUT              1 0
035500000000     C                   MOVEL     '17'          §COD              2
035600000000     C                   MOVE      *BLANKS       §DES1
035700000000     C                   CALL      'X§TABER'
035800000000     C                   PARM                    §KUT
035900000000     C                   PARM                    §COD
036000000000     C                   PARM                    §KEY              8
036100970401     C                   PARM                    §DES             25
036200000000     C                   MOVEL     §KEY          VIDDIV
036300000000     C                   MOVEL     §DES          VDDDIV
036400000000     C*
036500000000     C                   ELSE                                                   <-- 1 -->
036600000000     C*
036700000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
036800000000     C                   MOVEL     CODUT         KTBUTE
036900000000     C                   MOVE      '17'          KTBCOD
037000000000     C                   MOVE      *BLANKS       KTBKEY
037100000000     C                   MOVEL     VIDDIV        KTBKEY
037200000000     C     KEYTAB        CHAIN     TABEL                              99
037300000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
037400000000     C     TBLFLG        OREQ      '*'
037500000000     C                   MOVEL     '1'           ERRL00
037600000000     C                   MOVEL     *ALL'*'       VDDDIV
037700000000     C                   ELSE                                                   <-- 2 -->
037800970401     C                   MOVEL     TBLUNI        DS17
037900970401     C                   MOVEL     §17DES        VDDDIV
038000040827     C                   ENDIF                                                  <-- 2 ---
038100000000     C*
038200040827     C                   ENDIF                                                  <-- 1 ---
038300000000     C*
038400000000     C                   ENDSR
038500000000     C*------------------------------------------------------------------------*
038600000000     C*  CHKARE - CONTROLLA L'AREA
038700000000     C*------------------------------------------------------------------------*
038800000000     C     CHKARE        BEGSR
038900000000     C*
039000000000     C* CONTROLLO SE E' STATO INSERITO '?'
039100000000     C     '?'           SCAN      VIDARE        A                 1 0    99
039200000000     C*
039300000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
039400000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
039500000000     C                   MOVEL     CODUT         §KUT              1 0
039600000000     C                   MOVEL     '05'          §COD              2
039700000000     C                   MOVE      *BLANKS       §DES1
039800000000     C                   CALL      'X§TABER'
039900000000     C                   PARM                    §KUT
040000000000     C                   PARM                    §COD
040100000000     C                   PARM                    §KEY              8
040200970401     C                   PARM                    §DES             25
040300000000     C                   MOVEL     §KEY          VIDARE
040400000000     C                   MOVEL     §DES          VDDARE
040500000000     C*
040600000000     C                   ELSE                                                   <-- 1 -->
040700000000     C*
040800000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
040900000000     C                   MOVEL     CODUT         KTBUTE
041000000000     C                   MOVE      '05'          KTBCOD
041100000000     C                   MOVE      *BLANKS       KTBKEY
041200000000     C                   MOVEL     VIDARE        KTBKEY
041300000000     C     KEYTAB        CHAIN     TABEL                              99
041400000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
041500000000     C     TBLFLG        OREQ      '*'
041600000000     C                   MOVEL     '1'           ERRL00
041700000000     C                   MOVEL     *ALL'*'       VDDARE
041800000000     C                   ELSE                                                   <-- 2 -->
041900970401     C                   MOVEL     TBLUNI        DS05
042000970401     C                   MOVEL     §05DES        VDDARE
042100040827     C                   ENDIF                                                  <-- 2 ---
042200000000     C*
042300040827     C                   ENDIF                                                  <-- 1 ---
042400000000     C*
042500000000     C                   ENDSR
042600000000     C*------------------------------------------------------------------------*
042700000000     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
042800000000     C*------------------------------------------------------------------------*
042900000000     C     CHKLIN        BEGSR
043000000000     C*
043100000000     C* CONTROLLO SE E' STATO INSERITO '?'
043200000000     C     '?'           SCAN      WLIN          A                 1 0    99
043300000000     C*
043400000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
043500000000IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
043600000000     C                   MOVEL     *BLANKS       §COD1             3
043700000000     C                   MOVE      *BLANKS       §DES1
043800971203     C                   MOVE      *BLANKS       §PRA
043900971203     C                   CALL      'TNSD24R'
044000000000     C                   PARM                    §COD1
044100000000     C                   PARM                    §TIP              1
044200991117     C                   PARM                    §DES1            25
044300971203     C                   PARM                    §PRA              3
044400000000     C                   MOVEL     §COD1         WLIN              3
044500000000     C                   MOVEL     §DES1         WDLIN            20
044600000000     C*
044700000000X   1C                   ELSE
044800000000     C*
044900000000     C* CARATTERI ALFABETICI NON VALIDI
045000000000IF  2C     WLIN          IFLT      *ZEROS
045100000000     C                   MOVEL     '1'           ERRL00
045200000000     C                   MOVEL     MSG(14)       DSMSMS
045300000000     C                   MOVEL     *ALL'*'       WDLIN
045400000000X   2C                   ELSE
045500000000     C*
045600000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
045700000000     C                   MOVE      WLIN          KORFIL
045800000000     C     KEYORG        CHAIN     AZORG                              99
045900000000IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
046000000000     C                   MOVEL     ORGDES        WDLIN
046100000000X   3C                   ELSE                                                   <-- 2 -->
046200000000     C                   MOVEL     '1'           ERRL00
046300000000     C                   MOVEL     MSG(14)       DSMSMS
046400000000     C                   MOVEL     *ALL'*'       WDLIN
046500040827E   3C                   ENDIF                                                  <-- 2 ---
046600000000IF  3C     ORGFAG        IFEQ      'V'                                          --- 2 -->
046700000000     C                   MOVEL     '1'           ERRL00
046800000000     C                   MOVEL     MSG(14)       DSMSMS
046900000000     C                   MOVEL     *ALL'*'       WDLIN
047000040827E   3C                   ENDIF                                                  <-- 2 ---
047100040827E   2C                   ENDIF                                                  <-- 2 ---
047200000000E   1C                   ENDIF
047300000000     C*
047400000000     C                   ENDSR
047500991117     C*------------------------------------------------------------------------*
047600991117     C*  CHKKSC - CONTROLLA CLIENTE
047700991117     C*------------------------------------------------------------------------*
047800991117     C     CHKKSC        BEGSR
047900991117     C*
048000991117     C* CONTROLLO SE E' STATO INSERITO '?'
048100991117     C     '?'           SCAN      WKSC          A                 1 0    99
048200991117     C*
048300991117     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
048400020805IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
048500020805     C                   CLEAR                   DSXC
048600020923     C                   Z-ADD     DUTKCI        DXCCAP                         *CAPOCONTO
048700020805     C                   CALL      'XCLIR'
048800020805     C                   PARM                    DSXC
048900020805     C                   MOVEA     DXCKSC        KSA                            *SCOMPATTA CODICI
049000020805     C     1             DO        30            J                 4 0
049100020805     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
049200020805     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
049300020805     C                   ENDDO
049400020805     C                   MOVEL     KSC(1)        WKSC              7
049500991117     C*
049600991117X   1C                   ELSE
049700991117     C*
049800991117     C* CARATTERI ALFABETICI NON VALIDI
049900991117IF  2C     WKSC          IFLT      *ZEROS
050000991117     C                   MOVEL     '1'           ERRL00
050100040827     C                   MOVEL     MSG(15)       DSMSMS
050200020805     C                   MOVEL     *ALL'*'       WDKSC            30
050300991117X   2C                   ELSE
050400040827     C*
050500040827     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA E REPERISCO LA SUA RAGIONE SOCIALE
050600040827     C                   CLEAR                   BS69DS
050700040827     C                   CLEAR                   ACODS
050800040827     C                   MOVEL     KNSIF         I69SIF
050900040827     C                   MOVE(P)   WKSC          I69KAC
051000040827     C                   CALL      'TIBS69R'
051100040827     C                   PARM                    BS69DS
051200040827     C                   PARM                    ACODS
051300040827     C                   PARM                    INDDS
051400040827     C                   PARM                    CLPDS
051500040827     C                   PARM                    CLSDS
051600040827     C     O69ERR        IFNE      '1'
051700040827     C                   MOVEL     ACORAG        VDDKSC
051800991117     C*
051900040827     C* SE CLIENTE ESISTE CONTROLLO CHE ESISTA IN TIVSS
052000991118     C                   MOVEL     *ALL'0'       KVSKSU
052100991118     C                   MOVE      WKSC          KVSKSU
052200991118     C     KVSKSU        CHAIN     TIVSS02L                           99
052300991117IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
052400991117X   3C                   ELSE                                                   <-- 2 -->
052500991117     C                   MOVEL     '1'           ERRL00
052600040827     C                   MOVEL     MSG(16)       DSMSMS
052700991118     C                   MOVEL     *ALL'*'       VDDKSC
052800040827E   3C                   ENDIF                                                  <-- 2 ---
052900040827     C                   ELSE
053000040827     C                   MOVEL     '1'           ERRL00
053100040827     C                   MOVEL     MSG(15)       DSMSMS
053200040827     C                   MOVEL     *ALL'*'       VDDKSC
053300040827     C                   ENDIF
053400040827E   2C                   ENDIF                                                  <-- 2 ---
053500991117E   1C                   ENDIF
053600991117     C*
053700991117     C                   ENDSR
053800000000     C*------------------------------------------------------------------------*
053900991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
054000000000     C*------------------------------------------------------------------------*
054100000000     C     VIDPAR        BEGSR
054200000000     C*
054300000000     C                   MOVEL     VIDDIV        PARDIV
054400991122     C                   MOVEL     VIDARE        PARARE
054500991122     C                   MOVEL     VIDFLP        PARFIL
054600991117     C                   MOVE      VIDKSC        PARKSC
054700060109     C                   MOVEL     VIDTTNU       PARTTNU
054800060109     C                   MOVEL     VIDSDNU       PARSDNU
054900060109     C                   MOVEL     VIDDISA       PARDISA
055000060109     C                   MOVEL     VIDWTAB       PARWTAB
055100000000     C*
055200000000     C                   ENDSR
055300020917     C*--------------------------------------------------------------------------------------------*
055400020917     C* REPERISCE I DATI UTENTE
055500020917     C*--------------------------------------------------------------------------------------------*
055600020917     C     REPDATIUTE    BEGSR
055700020917     C*
055800020917     C* INIZIALIZZA VARIABILI DI WRK
055900020917     C                   CLEAR                   TIBS34DS
056000020917     C                   CLEAR                   AZUTEDS
056100020917     C                   CLEAR                   DDATIUTE
056200020917     C*
056300020917     C     *DTAARA       DEFINE    §azute        azuteds
056400020917     C     *DTAARA       DEFINE    §datiute      ddatiute
056500020917     C                   IN(E)     *DTAARA
056600020917if  1C                   IF        %Error
056700020917     c                   EVAL      I34Tla = 'L'
056800020917     C                   CALL      'TIBS34R'
056900020917     C                   PARM                    Tibs34Ds
057000020917     C                   IN        *DTAARA
057100020917e   1C                   ENDIF
057200020917     C*
057300020917     C                   ENDSR
057400000000     C*------------------------------------------------------------------------*
057500980403     C* *INZSR - OPERAZIONI INIZIALI
057600000000     C*------------------------------------------------------------------------*
057700980403     C     *INZSR        BEGSR
057800000000     C*
057900000000     C* RICEVIMENTO PARAMETRI
058000000000     C     *ENTRY        PLIST
058100000000     C                   PARM                    KPJBA
058200991117     C*
058300991117     C* CHIAVE LETTURA TABEL00F - COMPLETA
058400991117     C     KEYTAB        KLIST
058500991117     C                   KFLD                    KTBUTE
058600991117     C                   KFLD                    KTBCOD
058700991117     C                   KFLD                    KTBKEY
058800000000     C*
058900000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
059000000000     C     KEYORG        KLIST
059100000000     C                   KFLD                    KORFIL
059200000000     C*
059300000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
059400000000     C     XMSGPA        PLIST
059500000000     C                   PARM                    DSMSMS
059600000000     C                   PARM                    DSMSPG
059700000000     C                   PARM                    DSMSKE
059800000000     C                   PARM                    DSMSER
059900000000     C*
060000000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
060100000000     C                   CALL      'XKEY'
060200000000     C                   PARM                    DSKEY
060300000000     C*
060400000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
060500000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
060600000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
060700991118     C     *LIKE         DEFINE    VSSKSU        KVSKSU
060800000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
060900000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
061000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
061100000000     C*
061200000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
061300000000     C*   -NOME PGM CHIAMANTE
061400000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
061500040827     C                   MOVEL     'TIST31R'     DSMSPG
061600040827     C                   MOVEL     'TIST31R'     MSGQUE
061700000000     C                   MOVEL     *BLANKS       MSGKEY
061800000000     C                   MOVEL     *BLANKS       DSMSMS
061900000000     C*
062000000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
062100000000     C                   Z-ADD     0             FUNGRU            5 0
062200000000     C*
062300000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
062400000000     C                   EXSR      FUNGES
062500000000     C*
062600000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
062700000000     C                   MOVEL     '0'           F12L00            1
062800000000     C                   MOVEL     '0'           F03L00            1
062900000000     C                   MOVEL     '0'           FINL00            1
063000000000     C                   MOVEL     '0'           ERRL00            1
063100000000     C*
063200000000     C* LANCIO ROUTINE PER VERIFICARE SE
063300000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
063400000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
063500020917     C                   EXSR      REPDATIUTE
063600020917     C                   Z-ADD     1             CODUT             1 0
063700000000     C*
063800000000     C                   ENDSR
063900000000     C*------------------------------------------------------------------------*
064000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
064100000000     C*------------------------------------------------------------------------*
064200000000     C     FUNGES        BEGSR
064300000000     C*
064400000000     C* PULISCO CAMPI DI LAVORO
064500000000     C                   Z-ADD     0             W                 5 0
064600000000     C*
064700000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
064800000000     C* E CARICO TABELLA FUNZIONI ATTIVE
064900000000     C     1             DO        24            I
065000000000     C                   MOVEL     FUD(I)        FUDRIG
065100000000     C*
065200000000     C* PULIZIA FUNZIONI PRECEDENTI
065300000000     C                   MOVEL     *BLANKS       FUV(I)
065400000000     C*
065500000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
065600000000     C* (SALTA F24)
065700000000     C     FUDATT        IFEQ      'S'
065800000000     C     FUDFUN        ANDNE     '24'
065900000000     C                   ADD       1             W
066000000000     C                   MOVEL     FUDDES        FUV(W)
066100000000     C                   END
066200000000     C                   END                                                    DO WHILE
066300000000     C*
066400000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
066500000000     C* ALTRIMENTI DISATTIVO F24
066600000000     C     1             DO        24            I
066700000000     C                   MOVEL     FUD(I)        FUDRIG
066800000000     C*
066900000000     C     FUDFUN        IFEQ      '24'
067000000000     C     W             IFGT      6
067100000000     C                   MOVEL     'S'           FUDATT
067200000000     C                   ELSE
067300000000     C                   MOVEL     *BLANKS       FUDATT
067400000000     C                   END                                                    W GT 8
067500000000     C                   MOVEL     FUDRIG        FUD(I)
067600000000     C                   END
067700000000     C                   END                                                    DO WHILE
067800000000     C*
067900000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
068000000000     C     W             DIV       6             I
068100000000     C                   MVR                     Z
068200000000     C*
068300000000     C     Z             IFNE      0
068400000000     C     I             ADD       1             Z
068500000000     C                   ELSE
068600000000     C                   Z-ADD     I             Z                 5 0
068700000000     C                   END
068800000000     C*
068900000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
069000000000     C                   ADD       1             FUNGRU
069100000000     C     FUNGRU        IFGT      Z
069200000000     C     FUNGRU        ORLT      1
069300000000     C                   Z-ADD     1             FUNGRU
069400000000     C                   END
069500000000     C*
069600000000     C* PRIMO GRUPPO DI 6 FUNZIONI
069700000000     C     FUNGRU        IFEQ      1
069800000000     C                   MOVEL     FUV(1)        FUN001
069900000000     C                   MOVEL     FUV(2)        FUN002
070000000000     C                   MOVEL     FUV(3)        FUN003
070100000000     C                   MOVEL     FUV(4)        FUN004
070200000000     C                   MOVEL     FUV(5)        FUN005
070300000000     C                   MOVEL     FUV(6)        FUN006
070400000000     C                   END
070500000000     C*
070600000000     C* SECONDO GRUPPO DI 6 FUNZIONI
070700000000     C     FUNGRU        IFEQ      2
070800000000     C                   MOVEL     FUV(7)        FUN001
070900000000     C                   MOVEL     FUV(8)        FUN002
071000000000     C                   MOVEL     FUV(9)        FUN003
071100000000     C                   MOVEL     FUV(10)       FUN004
071200000000     C                   MOVEL     FUV(11)       FUN005
071300000000     C                   MOVEL     FUV(12)       FUN006
071400000000     C                   END
071500000000     C*
071600000000     C* TERZO GRUPPO DI 6 FUNZIONI
071700000000     C     FUNGRU        IFEQ      3
071800000000     C                   MOVEL     FUV(13)       FUN001
071900000000     C                   MOVEL     FUV(14)       FUN002
072000000000     C                   MOVEL     FUV(15)       FUN003
072100000000     C                   MOVEL     FUV(16)       FUN004
072200000000     C                   MOVEL     FUV(17)       FUN005
072300000000     C                   MOVEL     FUV(18)       FUN006
072400000000     C                   END
072500000000     C*
072600000000     C* QUARTO GRUPPO DI 6 FUNZIONI
072700000000     C     FUNGRU        IFEQ      4
072800000000     C                   MOVEL     FUV(19)       FUN001
072900000000     C                   MOVEL     FUV(20)       FUN002
073000000000     C                   MOVEL     FUV(21)       FUN003
073100000000     C                   MOVEL     FUV(22)       FUN004
073200000000     C                   MOVEL     *BLANKS       FUN005
073300000000     C                   MOVEL     *BLANKS       FUN006
073400000000     C                   END
073500000000     C*
073600000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
073700000000     C                   MOVEL     *BLANKS       FUN007
073800000000     C                   MOVEL     *BLANKS       FUN008
073900000000     C*
074000000000     C     1             DO        24            I
074100000000     C                   MOVEL     FUD(I)        FUDRIG
074200000000     C     FUDFUN        IFEQ      '24'
074300000000     C     FUDATT        ANDEQ     'S'
074400000000     C                   MOVEL     FUDDES        FUN008
074500000000     C                   END
074600000000     C                   END                                                    DO
074700000000     C                   ENDSR
074800000000     C*------------------------------------------------------------------------*
074900000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
075000000000     C*------------------------------------------------------------------------*
075100000000     C     FUNCON        BEGSR
075200000000     C*
075300000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
075400000000     C                   EXSR      FUNVAR
075500000000     C*
075600000000     C* PULISCO CAMPI DI LAVORO
075700000000     C                   Z-ADD     0             I                 5 0
075800000000     C                   Z-ADD     0             W                 5 0
075900000000     C*
076000000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
076100000000     C     FUNKEY        IFNE      *BLANKS
076200000000     C     1             DO        24            I
076300000000     C                   MOVEL     FUD(I)        FUDRIG
076400000000     C*
076500000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
076600000000     C     FUNKEY        IFEQ      FUDFUN
076700000000     C     FUDATT        ANDEQ     'S'
076800000000     C                   Z-ADD     I             W
076900000000     C                   END
077000000000     C                   END                                                    DO WHILE
077100000000     C*
077200000000     C* FUNZIONE NON ATTIVA (ERRORE)
077300000000     C     W             IFEQ      0
077400000000     C*
077500000000     C* EMISSIONE MESSAGGIO DI ERRORE
077600000000     C                   MOVEL     MSG(1)        DSMSMS
077700000000     C*
077800000000     C                   END                                                    W=0
077900000000     C                   END                                                    FUNKEY NE *BLANKS
078000000000     C*
078100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
078200000000     C     W             IFNE      0
078300000000     C     FUNKEY        OREQ      *BLANKS
078400000000     C*
078500000000     C     FUNKEY        IFEQ      *BLANKS
078600000000     C                   EXSR      ENTGES
078700000000     C                   END
078800000000     C     FUNKEY        IFEQ      '03'
078900000000     C                   EXSR      F03GES
079000000000     C                   END
079100000000     C     FUNKEY        IFEQ      '06'
079200000000     C                   EXSR      F06GES
079300000000     C                   END
079400000000     C     FUNKEY        IFEQ      '12'
079500000000     C                   EXSR      F12GES
079600000000     C                   END
079700000000     C     FUNKEY        IFEQ      '24'
079800000000     C                   EXSR      F24GES
079900000000     C                   END
080000000000     C*
080100000000     C                   END
080200000000     C*
080300000000     C                   ENDSR
080400000000     C*------------------------------------------------------------------------*
080500000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
080600000000     C*------------------------------------------------------------------------*
080700000000     C     FUNVAR        BEGSR
080800000000     C*
080900000000     C* INIZIALIZZA VARIABILE -FUNKEY-
081000000000     C*
081100000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
081200000000     C     KEY           IFEQ      ENTER
081300000000     C                   MOVEL     *BLANKS       FUNKEY
081400000000     C                   END
081500000000     C     KEY           IFEQ      F1
081600000000     C                   MOVEL     '01'          FUNKEY
081700000000     C                   END
081800000000     C     KEY           IFEQ      F2
081900000000     C                   MOVEL     '02'          FUNKEY
082000000000     C                   END
082100000000     C     KEY           IFEQ      F3
082200000000     C                   MOVEL     '03'          FUNKEY
082300000000     C                   END
082400000000     C     KEY           IFEQ      F4
082500000000     C                   MOVEL     '04'          FUNKEY
082600000000     C                   END
082700000000     C     KEY           IFEQ      F5
082800000000     C                   MOVEL     '05'          FUNKEY
082900000000     C                   END
083000000000     C     KEY           IFEQ      F6
083100000000     C                   MOVEL     '06'          FUNKEY
083200000000     C                   END
083300000000     C     KEY           IFEQ      F7
083400000000     C                   MOVEL     '07'          FUNKEY
083500000000     C                   END
083600000000     C     KEY           IFEQ      F8
083700000000     C                   MOVEL     '08'          FUNKEY
083800000000     C                   END
083900000000     C     KEY           IFEQ      F9
084000000000     C                   MOVEL     '09'          FUNKEY
084100000000     C                   END
084200000000     C     KEY           IFEQ      F10
084300000000     C                   MOVEL     '10'          FUNKEY
084400000000     C                   END
084500000000     C     KEY           IFEQ      F11
084600000000     C                   MOVEL     '11'          FUNKEY
084700000000     C                   END
084800000000     C     KEY           IFEQ      F12
084900000000     C                   MOVEL     '12'          FUNKEY
085000000000     C                   END
085100000000     C     KEY           IFEQ      F13
085200000000     C                   MOVEL     '13'          FUNKEY
085300000000     C                   END
085400000000     C     KEY           IFEQ      F14
085500000000     C                   MOVEL     '14'          FUNKEY
085600000000     C                   END
085700000000     C     KEY           IFEQ      F15
085800000000     C                   MOVEL     '15'          FUNKEY
085900000000     C                   END
086000000000     C     KEY           IFEQ      F16
086100000000     C                   MOVEL     '16'          FUNKEY
086200000000     C                   END
086300000000     C     KEY           IFEQ      F17
086400000000     C                   MOVEL     '17'          FUNKEY
086500000000     C                   END
086600000000     C     KEY           IFEQ      F18
086700000000     C                   MOVEL     '18'          FUNKEY
086800000000     C                   END
086900000000     C     KEY           IFEQ      F19
087000000000     C                   MOVEL     '19'          FUNKEY
087100000000     C                   END
087200000000     C     KEY           IFEQ      F20
087300000000     C                   MOVEL     '20'          FUNKEY
087400000000     C                   END
087500000000     C     KEY           IFEQ      F21
087600000000     C                   MOVEL     '21'          FUNKEY
087700000000     C                   END
087800000000     C     KEY           IFEQ      F22
087900000000     C                   MOVEL     '22'          FUNKEY
088000000000     C                   END
088100000000     C     KEY           IFEQ      F23
088200000000     C                   MOVEL     '23'          FUNKEY
088300000000     C                   END
088400000000     C     KEY           IFEQ      F24
088500000000     C                   MOVEL     '24'          FUNKEY
088600000000     C                   END
088700000000     C     KEY           IFEQ      ROLLUP
088800000000     C                   MOVEL     'RU'          FUNKEY
088900000000     C                   END
089000000000     C     KEY           IFEQ      HELP
089100000000     C                   MOVEL     'HP'          FUNKEY
089200000000     C                   END
089300000000     C                   ENDSR
089400000000     C*------------------------------------------------------------------------*
089500000000     C* NOTE PER COMPILAZIONE SCHIERE
089600000000     C*------------------------------------------------------------------------*
089700000000     C* -MSG-
089800000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
089900000000     C* (NON TOCCARE MSG,1)
090000000000     C*
090100000000     C* -CMD-
090200000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
090300000000     C*
090400000000     C* -FUD-
090500000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
090600000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
090700000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
090800000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
090900000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
091000000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
091100000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
091200000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
091300000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
091400000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
091500000000     C*
091600000000     C*------------------------------------------------------------------------*
091700000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
091800040827TIST31R- Tasto funzionale non gestito.                                         1
091900040827TIST31R- 2
092000040827TIST31R- 3
092100040827TIST31R- 4                                                                     4
092200040827TIST31R- 5
092300040827TIST31R- 6
092400040827TIST31R- 7
092500040827TIST31R- 8
092600040827TIST31R- 9
092700040827TIST31R- 10
092800040827TIST31R- 11
092900040827TIST31R- Divisione errata.                                                    12
093000040827TIST31R- Area errata.                                                         13
093100040827TIST31R- Punto operativo/terminal errato.                                     14
093200040827TIST31R- Cliente errato.                                                      15
093300040827TIST31R- Nessun servizio VAS abilitao per il cliente.                         16
093400040827TIST31R- 17
093500040827TIST31R- 18
093600040827TIST31R- 19
093700040827TIST31R- 20
093800040827TIST31R- 21
093900000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
094000000000                   -        (S/N)=N                        funz.01¬
094100000000                   -        (S/N)=N                        funz.02¬
094200000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
094300000000                   -        (S/N)=N                        funz.04¬
094400000000                   -        (S/N)=N                        funz.05¬
094500000000F06=Lancio         -        (S/N)=S                        funz.06¬
094600000000                   -        (S/N)=N                        funz.07¬
094700000000                   -        (S/N)=N                        funz.08¬
094800000000                   -        (S/N)=N                        funz.09¬
094900000000                   -        (S/N)=N                        funz.10¬
095000000000                   -        (S/N)=N                        funz.11¬
095100000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
095200000000                   -        (S/N)=N                        funz.13¬
095300000000                   -        (S/N)=N                        funz.14¬
095400000000                   -        (S/N)=N                        funz.15¬
095500000000                   -        (S/N)=N                        funz.16¬
095600000000                   -        (S/N)=N                        funz.17¬
095700000000                   -        (S/N)=N                        funz.18¬
095800000000                   -        (S/N)=N                        funz.19¬
095900000000                   -        (S/N)=N                        funz.20¬
096000000000                   -        (S/N)=N                        funz.21¬
096100000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
096200000000                   -        (S/N)=N                        funz.23¬
096300000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
