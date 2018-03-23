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
007700000000     D*-------------------
007800000000     D* DS PER GESTIONE TASTI FUNZIONALI
007900000000     D*-------------------
008000000000     D DSKEY         E DS
008100000000     D DEVDS1          DS
008200000000     D  KEY                  369    369
008300000000     D*-------------------
008400000000     D* ARCHITETTURA
008500000000     D*-------------------
008600000000     D KPJBA         E DS
008700011228     D*-------------------
008800991118     D* DS REPERIMENTO DATI UTENTE
008900991118     D*-------------------
009000991118     DBS69DS         E DS                  extname(TIBS69DS)
009100991118     DACODS          E DS                  extname(CNACO00F)
009200991118     DINDDS          E DS                  extname(CNIND00F)
009300991118     DCLPDS          E DS                  extname(CNCLP00F)
009400991118     DCLSDS          E DS                  extname(FNCLS00F)
009500020805     D*-------------------
009600020805     D* CAPOCONTO / SOTTOCONTO DI RITORNO "XCLIR"
009700020805     D*-------------------
009800040827     D KSC             S              7  0 DIM(30)
009900020805     D KSA             S              4    DIM(30)                              *ALFANUMERICO
010000020805     D*------------------
010100020805     D* DS "XCLIR"
010200020805     D*------------------
010300040827     D DSXC          E DS                  EXTNAME(XCLIRDS)
010400020805     D*------------------
010500020805     D* DS PER PASSAGGIO CAMPI PACKED
010600020805     D*------------------
010700020805     D                 DS
010800020805     D  DSKSN                  1      4P 0
010900020805     D  DSKSA                  1      4
011000000000     C*------------------------------------------------------------------------*
011100000000     C* MAIN LINE
011200000000     C*------------------------------------------------------------------------*
011300000000     C*
011400000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
011500000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
011600000000     C*
011700000000     C* CICLO FINO A CHE:
011800000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011900000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012000000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
012100000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
012200000000     C     F12L00        OREQ      '1'
012300000000     C*
012400000000     C* EMETTE FORMATO VIDEO
012500000000     C                   EXSR      BUTFMR
012600000000     C*
012700000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
012800000000     C                   EXSR      FUNCON
012900000000     C*
013000000000     C                   END                                                    <-- 2 --
013100000000     C*
013200000000     C                   END                                                    <-- 1 --
013300991118     C*
013400991118     C* OPERAZIONI FINALI
013500991118     C                   EXSR      FINSR
013600991118     C*
013700000000     C                   SETON                                        LR
013800991118     C*------------------------------------------------------------------------*
013900991118     C* FINSR - OPERAZIONI FINALI
014000991118     C*------------------------------------------------------------------------*
014100991118     C     FINSR         BEGSR
014200991118     C*
014300991118     C* CHIUDE I FILE DEI PGM CHIAMATI
014400991118     C                   CLEAR                   BS69DS
014500991118     C                   CLEAR                   ACODS
014600991118     C                   CLEAR                   INDDS
014700991118     C                   CLEAR                   CLPDS
014800991118     C                   CLEAR                   CLSDS
014900991118     C                   MOVEL     'C'           I69TLA
015000991118     C                   MOVEL     KNSIF         I69SIF
015100991118     C                   CALL      'TIBS69R'
015200991118     C                   PARM                    BS69DS
015300991118     C                   PARM                    ACODS
015400991118     C                   PARM                    INDDS
015500991118     C                   PARM                    CLPDS
015600991118     C                   PARM                    CLSDS
015700991118     C*
015800991118     C                   ENDSR
015900000000     C*------------------------------------------------------------------------*
016000000000     C* EMISSIONE FORMATO A VIDEO
016100000000     C*------------------------------------------------------------------------*
016200000000     C     BUTFMR        BEGSR
016300000000     C*
016400000000     C* WRITE RECORD TESTATA
016500040827     C                   WRITE     ST31D01
016600000000     C*
016700000000     C* VISUALIZZAZIONE MESSAGGIO
016800000000     C                   EXSR      SNDMSG
016900000000     C*
017000000000     C* WRITE RECORD FUNZIONI
017100040827     C                   WRITE     ST31D02
017200991019     C*
017300991019     C* EXFMT RECORD DETTAGLIO
017400040827     C                   EXFMT     ST31D03
017500991019     C*
017600000000     C* AZZERO FLAG DI ERRORE LIVELLO
017700000000     C                   MOVEL     '0'           ERRL00
017800000000     C*
017900040827     C* AZZERO INDICATORI DI ERRORE
018000040827     C                   SETOFF                                       787980
018100040827     C                   SETOFF                                       81
018200000000     C*
018300000000     C                   ENDSR
018400000000     C*------------------------------------------------------------------------*
018500000000     C* SNDMSG - MANDA MESSAGGIO
018600000000     C*------------------------------------------------------------------------*
018700000000     C     SNDMSG        BEGSR
018800000000     C*
018900000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
019000000000     C                   MOVEL     '1'           *IN40
019100040827     C                   WRITE     ST31DMC
019200000000     C                   MOVEL     '0'           *IN40
019300000000     C*
019400000000     C* VISUALIZZAZIONE MESSAGGIO
019500000000     C                   Z-ADD     0             MSGULT            4 0
019600000000     C                   CALL      'XMSG'        XMSGPA
019700000000     C     DSMSER        IFEQ      0
019800000000     C                   MOVEL     DSMSKE        MSGKEY
019900000000     C                   END
020000000000     C                   Z-ADD     1             MSGULT
020100040827     C                   WRITE     ST31DMS                                      SFL MESS
020200040827     C                   WRITE     ST31DMC                                      CONTR MESS
020300000000     C*
020400000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
020500000000     C                   MOVEL     *BLANKS       DSMSMS
020600000000     C*
020700000000     C                   ENDSR
020800000000     C*------------------------------------------------------------------------*
020900000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
021000000000     C*------------------------------------------------------------------------*
021100000000     C     ENTGES        BEGSR
021200000000     C*
021300000000     C* CONTROLLA I CAMPI IMMESSI
021400000000     C                   EXSR      DECODI
021500000000     C*
021600000000     C                   ENDSR
021700000000     C*------------------------------------------------------------------------*
021800000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
021900000000     C*------------------------------------------------------------------------*
022000000000     C     F03GES        BEGSR
022100000000     C*
022200000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
022300000000     C                   MOVEL     '1'           F03L00
022400000000     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
022800000000     C*------------------------------------------------------------------------*
022900991020     C     F06GES        BEGSR
023000000000     C*
023100991105     C                   EXSR      DECODI
023200991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
023300991105     C     ERRL00        IFEQ      '0'
023400991105     C*
023500000000     C* LANCIO IL PROGRAMMA DI STAMPA
023600040827     C                   MOVEL     'ST32'        KCOAZ
023700000000     C                   EXSR      VIDPAR
023800950703     C                   MOVEL     PARAM         KPJBU
023900040903 MOM C                   CALL      'BCH10'
024000040903     C*                  CALL      'TIST32C'
024100991021     C                   PARM                    KPJBA
024200040901     C* DOPO IL LANCIO ESCO DALLA VIDEATA
024300040901     C                   EVAL      F03L00 = '1'
024400991105     C                   ENDIF
024500991105     C*
024600000000     C                   ENDSR
024700000000     C*------------------------------------------------------------------------*
024800000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
024900000000     C*------------------------------------------------------------------------*
025000000000     C     F12GES        BEGSR
025100000000     C*
025200000000     C* IMPOSTO FLAG DI LIVELLO
025300000000     C                   MOVEL     '1'           F12L00
025400000000     C*
025500000000     C                   ENDSR
025600000000     C*------------------------------------------------------------------------*
025700991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
025800000000     C*------------------------------------------------------------------------*
025900000000     C     F24GES        BEGSR
026000000000     C*
026100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
026200000000     C                   EXSR      FUNGES
026300000000     C*
026400000000     C                   ENDSR
026500991020     C*------------------------------------------------------------------------*
026600991105     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
026700000000     C*------------------------------------------------------------------------*
026800000000     C     DECODI        BEGSR
026900000000     C*
027000000000     C* CONTROLLO LA DIVISIONE
027100000000     C                   MOVEL     *BLANKS       VDDDIV
027200000000     C     ERRL00        IFEQ      '0'
027300000000     C     VIDDIV        IFNE      *BLANKS
027400000000     C                   EXSR      CHKDIV
027500000000     C     ERRL00        IFEQ      '1'
027600000000     C                   MOVEL     '1'           ERRL00
027700000000     C                   MOVEL     MSG(12)       DSMSMS
027800000000     C                   SETON                                        78
027900040827     C                   ENDIF
028000040827     C                   ENDIF
028100040827     C                   ENDIF
028200000000     C*
028300000000     C* CONTROLLO L'AREA
028400000000     C                   MOVEL     *BLANKS       VDDARE
028500000000     C     ERRL00        IFEQ      '0'
028600000000     C     VIDARE        IFNE      *BLANKS
028700000000     C                   EXSR      CHKARE
028800000000     C     ERRL00        IFEQ      '1'
028900000000     C                   MOVEL     '1'           ERRL00
029000000000     C                   MOVEL     MSG(13)       DSMSMS
029100000000     C                   SETON                                        79
029200040827     C                   ENDIF
029300040827     C                   ENDIF
029400040827     C                   ENDIF
029500000000     C*
029600000000     C* CONTROLLO LA FILIALE
029700000000     C                   MOVEL     *BLANKS       VDDFLP
029800000000     C     ERRL00        IFEQ      '0'
029900000000     C     VIDFLP        IFNE      *BLANKS
030000000000     C                   MOVE      VIDFLP        WLIN
030100000000     C                   EXSR      CHKLIN
030200000000     C     ERRL00        IFEQ      '0'
030300000000     C                   MOVEL     WLIN          VIDFLP
030400000000     C                   MOVEL     WDLIN         VDDFLP
030500000000     C                   ELSE
030600000000     C                   SETON                                        80
030700040827     C                   ENDIF
030800040827     C                   ENDIF
030900040827     C                   ENDIF
031000991117     C*
031100991117     C* CONTROLLO IL CLIENTE
031200991117     C                   MOVEL     *BLANKS       VDDKSC
031300991117     C     ERRL00        IFEQ      '0'
031400991117     C     VIDKSC        IFNE      *BLANKS
031500991117     C                   MOVE      VIDKSC        WKSC
031600991117     C                   EXSR      CHKKSC
031700991117     C     ERRL00        IFEQ      '0'
031800991117     C                   MOVEL     WKSC          VIDKSC
031900991117     C                   MOVEL     WDKSC         VDDKSC
032000991117     C                   ELSE
032100991117     C                   SETON                                        81
032200040827     C                   ENDIF
032300040827     C                   ENDIF
032400040827     C                   ENDIF
032500000000     C*
032600000000     C                   ENDSR
032700000000     C*------------------------------------------------------------------------*
032800000000     C*  CHKDIV - CONTROLLA LA DIVISIONE
032900000000     C*------------------------------------------------------------------------*
033000000000     C     CHKDIV        BEGSR
033100000000     C*
033200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
033300000000     C     VIDDIV        IFEQ      '?'                                          --- 1 -->
033400000000     C                   MOVEL     CODUT         §KUT              1 0
033500000000     C                   MOVEL     '17'          §COD              2
033600000000     C                   MOVE      *BLANKS       §DES1
033700000000     C                   CALL      'X§TABER'
033800000000     C                   PARM                    §KUT
033900000000     C                   PARM                    §COD
034000000000     C                   PARM                    §KEY              8
034100970401     C                   PARM                    §DES             25
034200000000     C                   MOVEL     §KEY          VIDDIV
034300000000     C                   MOVEL     §DES          VDDDIV
034400000000     C*
034500000000     C                   ELSE                                                   <-- 1 -->
034600000000     C*
034700000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
034800000000     C                   MOVEL     CODUT         KTBUTE
034900000000     C                   MOVE      '17'          KTBCOD
035000000000     C                   MOVE      *BLANKS       KTBKEY
035100000000     C                   MOVEL     VIDDIV        KTBKEY
035200000000     C     KEYTAB        CHAIN     TABEL                              99
035300000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
035400000000     C     TBLFLG        OREQ      '*'
035500000000     C                   MOVEL     '1'           ERRL00
035600000000     C                   MOVEL     *ALL'*'       VDDDIV
035700000000     C                   ELSE                                                   <-- 2 -->
035800970401     C                   MOVEL     TBLUNI        DS17
035900970401     C                   MOVEL     §17DES        VDDDIV
036000040827     C                   ENDIF                                                  <-- 2 ---
036100000000     C*
036200040827     C                   ENDIF                                                  <-- 1 ---
036300000000     C*
036400000000     C                   ENDSR
036500000000     C*------------------------------------------------------------------------*
036600000000     C*  CHKARE - CONTROLLA L'AREA
036700000000     C*------------------------------------------------------------------------*
036800000000     C     CHKARE        BEGSR
036900000000     C*
037000000000     C* CONTROLLO SE E' STATO INSERITO '?'
037100000000     C     '?'           SCAN      VIDARE        A                 1 0    99
037200000000     C*
037300000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
037400000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
037500000000     C                   MOVEL     CODUT         §KUT              1 0
037600000000     C                   MOVEL     '05'          §COD              2
037700000000     C                   MOVE      *BLANKS       §DES1
037800000000     C                   CALL      'X§TABER'
037900000000     C                   PARM                    §KUT
038000000000     C                   PARM                    §COD
038100000000     C                   PARM                    §KEY              8
038200970401     C                   PARM                    §DES             25
038300000000     C                   MOVEL     §KEY          VIDARE
038400000000     C                   MOVEL     §DES          VDDARE
038500000000     C*
038600000000     C                   ELSE                                                   <-- 1 -->
038700000000     C*
038800000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
038900000000     C                   MOVEL     CODUT         KTBUTE
039000000000     C                   MOVE      '05'          KTBCOD
039100000000     C                   MOVE      *BLANKS       KTBKEY
039200000000     C                   MOVEL     VIDARE        KTBKEY
039300000000     C     KEYTAB        CHAIN     TABEL                              99
039400000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
039500000000     C     TBLFLG        OREQ      '*'
039600000000     C                   MOVEL     '1'           ERRL00
039700000000     C                   MOVEL     *ALL'*'       VDDARE
039800000000     C                   ELSE                                                   <-- 2 -->
039900970401     C                   MOVEL     TBLUNI        DS05
040000970401     C                   MOVEL     §05DES        VDDARE
040100040827     C                   ENDIF                                                  <-- 2 ---
040200000000     C*
040300040827     C                   ENDIF                                                  <-- 1 ---
040400000000     C*
040500000000     C                   ENDSR
040600000000     C*------------------------------------------------------------------------*
040700000000     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
040800000000     C*------------------------------------------------------------------------*
040900000000     C     CHKLIN        BEGSR
041000000000     C*
041100000000     C* CONTROLLO SE E' STATO INSERITO '?'
041200000000     C     '?'           SCAN      WLIN          A                 1 0    99
041300000000     C*
041400000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
041500000000IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
041600000000     C                   MOVEL     *BLANKS       §COD1             3
041700000000     C                   MOVE      *BLANKS       §DES1
041800971203     C                   MOVE      *BLANKS       §PRA
041900971203     C                   CALL      'TNSD24R'
042000000000     C                   PARM                    §COD1
042100000000     C                   PARM                    §TIP              1
042200991117     C                   PARM                    §DES1            25
042300971203     C                   PARM                    §PRA              3
042400000000     C                   MOVEL     §COD1         WLIN              3
042500000000     C                   MOVEL     §DES1         WDLIN            20
042600000000     C*
042700000000X   1C                   ELSE
042800000000     C*
042900000000     C* CARATTERI ALFABETICI NON VALIDI
043000000000IF  2C     WLIN          IFLT      *ZEROS
043100000000     C                   MOVEL     '1'           ERRL00
043200000000     C                   MOVEL     MSG(14)       DSMSMS
043300000000     C                   MOVEL     *ALL'*'       WDLIN
043400000000X   2C                   ELSE
043500000000     C*
043600000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
043700000000     C                   MOVE      WLIN          KORFIL
043800000000     C     KEYORG        CHAIN     AZORG                              99
043900000000IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
044000000000     C                   MOVEL     ORGDES        WDLIN
044100000000X   3C                   ELSE                                                   <-- 2 -->
044200000000     C                   MOVEL     '1'           ERRL00
044300000000     C                   MOVEL     MSG(14)       DSMSMS
044400000000     C                   MOVEL     *ALL'*'       WDLIN
044500040827E   3C                   ENDIF                                                  <-- 2 ---
044600000000IF  3C     ORGFAG        IFEQ      'V'                                          --- 2 -->
044700000000     C                   MOVEL     '1'           ERRL00
044800000000     C                   MOVEL     MSG(14)       DSMSMS
044900000000     C                   MOVEL     *ALL'*'       WDLIN
045000040827E   3C                   ENDIF                                                  <-- 2 ---
045100040827E   2C                   ENDIF                                                  <-- 2 ---
045200000000E   1C                   ENDIF
045300000000     C*
045400000000     C                   ENDSR
045500991117     C*------------------------------------------------------------------------*
045600991117     C*  CHKKSC - CONTROLLA CLIENTE
045700991117     C*------------------------------------------------------------------------*
045800991117     C     CHKKSC        BEGSR
045900991117     C*
046000991117     C* CONTROLLO SE E' STATO INSERITO '?'
046100991117     C     '?'           SCAN      WKSC          A                 1 0    99
046200991117     C*
046300991117     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
046400020805IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
046500020805     C                   CLEAR                   DSXC
046600020923     C                   Z-ADD     DUTKCI        DXCCAP                         *CAPOCONTO
046700020805     C                   CALL      'XCLIR'
046800020805     C                   PARM                    DSXC
046900020805     C                   MOVEA     DXCKSC        KSA                            *SCOMPATTA CODICI
047000020805     C     1             DO        30            J                 4 0
047100020805     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
047200020805     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
047300020805     C                   ENDDO
047400020805     C                   MOVEL     KSC(1)        WKSC              7
047500991117     C*
047600991117X   1C                   ELSE
047700991117     C*
047800991117     C* CARATTERI ALFABETICI NON VALIDI
047900991117IF  2C     WKSC          IFLT      *ZEROS
048000991117     C                   MOVEL     '1'           ERRL00
048100040827     C                   MOVEL     MSG(15)       DSMSMS
048200020805     C                   MOVEL     *ALL'*'       WDKSC            30
048300991117X   2C                   ELSE
048400040827     C*
048500040827     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA E REPERISCO LA SUA RAGIONE SOCIALE
048600040827     C                   CLEAR                   BS69DS
048700040827     C                   CLEAR                   ACODS
048800040827     C                   MOVEL     KNSIF         I69SIF
048900040827     C                   MOVE(P)   WKSC          I69KAC
049000040827     C                   CALL      'TIBS69R'
049100040827     C                   PARM                    BS69DS
049200040827     C                   PARM                    ACODS
049300040827     C                   PARM                    INDDS
049400040827     C                   PARM                    CLPDS
049500040827     C                   PARM                    CLSDS
049600040827     C     O69ERR        IFNE      '1'
049700040827     C                   MOVEL     ACORAG        VDDKSC
049800991117     C*
049900040827     C* SE CLIENTE ESISTE CONTROLLO CHE ESISTA IN TIVSS
050000991118     C                   MOVEL     *ALL'0'       KVSKSU
050100991118     C                   MOVE      WKSC          KVSKSU
050200991118     C     KVSKSU        CHAIN     TIVSS02L                           99
050300991117IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
050400991117X   3C                   ELSE                                                   <-- 2 -->
050500991117     C                   MOVEL     '1'           ERRL00
050600040827     C                   MOVEL     MSG(16)       DSMSMS
050700991118     C                   MOVEL     *ALL'*'       VDDKSC
050800040827E   3C                   ENDIF                                                  <-- 2 ---
050900040827     C                   ELSE
051000040827     C                   MOVEL     '1'           ERRL00
051100040827     C                   MOVEL     MSG(15)       DSMSMS
051200040827     C                   MOVEL     *ALL'*'       VDDKSC
051300040827     C                   ENDIF
051400040827E   2C                   ENDIF                                                  <-- 2 ---
051500991117E   1C                   ENDIF
051600991117     C*
051700991117     C                   ENDSR
051800000000     C*------------------------------------------------------------------------*
051900991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
052000000000     C*------------------------------------------------------------------------*
052100000000     C     VIDPAR        BEGSR
052200000000     C*
052300000000     C                   MOVEL     VIDDIV        PARDIV
052400991122     C                   MOVEL     VIDARE        PARARE
052500991122     C                   MOVEL     VIDFLP        PARFIL
052600991117     C                   MOVE      VIDKSC        PARKSC
052700000000     C*
052800000000     C                   ENDSR
052900020917     C*--------------------------------------------------------------------------------------------*
053000020917     C* REPERISCE I DATI UTENTE
053100020917     C*--------------------------------------------------------------------------------------------*
053200020917     C     REPDATIUTE    BEGSR
053300020917     C*
053400020917     C* INIZIALIZZA VARIABILI DI WRK
053500020917     C                   CLEAR                   TIBS34DS
053600020917     C                   CLEAR                   AZUTEDS
053700020917     C                   CLEAR                   DDATIUTE
053800020917     C*
053900020917     C     *DTAARA       DEFINE    §azute        azuteds
054000020917     C     *DTAARA       DEFINE    §datiute      ddatiute
054100020917     C                   IN(E)     *DTAARA
054200020917if  1C                   IF        %Error
054300020917     c                   EVAL      I34Tla = 'L'
054400020917     C                   CALL      'TIBS34R'
054500020917     C                   PARM                    Tibs34Ds
054600020917     C                   IN        *DTAARA
054700020917e   1C                   ENDIF
054800020917     C*
054900020917     C                   ENDSR
055000000000     C*------------------------------------------------------------------------*
055100980403     C* *INZSR - OPERAZIONI INIZIALI
055200000000     C*------------------------------------------------------------------------*
055300980403     C     *INZSR        BEGSR
055400000000     C*
055500000000     C* RICEVIMENTO PARAMETRI
055600000000     C     *ENTRY        PLIST
055700000000     C                   PARM                    KPJBA
055800991117     C*
055900991117     C* CHIAVE LETTURA TABEL00F - COMPLETA
056000991117     C     KEYTAB        KLIST
056100991117     C                   KFLD                    KTBUTE
056200991117     C                   KFLD                    KTBCOD
056300991117     C                   KFLD                    KTBKEY
056400000000     C*
056500000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
056600000000     C     KEYORG        KLIST
056700000000     C                   KFLD                    KORFIL
056800000000     C*
056900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
057000000000     C     XMSGPA        PLIST
057100000000     C                   PARM                    DSMSMS
057200000000     C                   PARM                    DSMSPG
057300000000     C                   PARM                    DSMSKE
057400000000     C                   PARM                    DSMSER
057500000000     C*
057600000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
057700000000     C                   CALL      'XKEY'
057800000000     C                   PARM                    DSKEY
057900000000     C*
058000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
058100000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
058200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
058300991118     C     *LIKE         DEFINE    VSSKSU        KVSKSU
058400000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
058500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
058600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
058700000000     C*
058800000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
058900000000     C*   -NOME PGM CHIAMANTE
059000000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
059100040827     C                   MOVEL     'TIST31R'     DSMSPG
059200040827     C                   MOVEL     'TIST31R'     MSGQUE
059300000000     C                   MOVEL     *BLANKS       MSGKEY
059400000000     C                   MOVEL     *BLANKS       DSMSMS
059500000000     C*
059600000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
059700000000     C                   Z-ADD     0             FUNGRU            5 0
059800000000     C*
059900000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
060000000000     C                   EXSR      FUNGES
060100000000     C*
060200000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
060300000000     C                   MOVEL     '0'           F12L00            1
060400000000     C                   MOVEL     '0'           F03L00            1
060500000000     C                   MOVEL     '0'           FINL00            1
060600000000     C                   MOVEL     '0'           ERRL00            1
060700000000     C*
060800000000     C* LANCIO ROUTINE PER VERIFICARE SE
060900000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
061000000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
061100020917     C                   EXSR      REPDATIUTE
061200020917     C                   Z-ADD     1             CODUT             1 0
061300000000     C*
061400000000     C                   ENDSR
061500000000     C*------------------------------------------------------------------------*
061600000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
061700000000     C*------------------------------------------------------------------------*
061800000000     C     FUNGES        BEGSR
061900000000     C*
062000000000     C* PULISCO CAMPI DI LAVORO
062100000000     C                   Z-ADD     0             W                 5 0
062200000000     C*
062300000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
062400000000     C* E CARICO TABELLA FUNZIONI ATTIVE
062500000000     C     1             DO        24            I
062600000000     C                   MOVEL     FUD(I)        FUDRIG
062700000000     C*
062800000000     C* PULIZIA FUNZIONI PRECEDENTI
062900000000     C                   MOVEL     *BLANKS       FUV(I)
063000000000     C*
063100000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
063200000000     C* (SALTA F24)
063300000000     C     FUDATT        IFEQ      'S'
063400000000     C     FUDFUN        ANDNE     '24'
063500000000     C                   ADD       1             W
063600000000     C                   MOVEL     FUDDES        FUV(W)
063700000000     C                   END
063800000000     C                   END                                                    DO WHILE
063900000000     C*
064000000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
064100000000     C* ALTRIMENTI DISATTIVO F24
064200000000     C     1             DO        24            I
064300000000     C                   MOVEL     FUD(I)        FUDRIG
064400000000     C*
064500000000     C     FUDFUN        IFEQ      '24'
064600000000     C     W             IFGT      6
064700000000     C                   MOVEL     'S'           FUDATT
064800000000     C                   ELSE
064900000000     C                   MOVEL     *BLANKS       FUDATT
065000000000     C                   END                                                    W GT 8
065100000000     C                   MOVEL     FUDRIG        FUD(I)
065200000000     C                   END
065300000000     C                   END                                                    DO WHILE
065400000000     C*
065500000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
065600000000     C     W             DIV       6             I
065700000000     C                   MVR                     Z
065800000000     C*
065900000000     C     Z             IFNE      0
066000000000     C     I             ADD       1             Z
066100000000     C                   ELSE
066200000000     C                   Z-ADD     I             Z                 5 0
066300000000     C                   END
066400000000     C*
066500000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
066600000000     C                   ADD       1             FUNGRU
066700000000     C     FUNGRU        IFGT      Z
066800000000     C     FUNGRU        ORLT      1
066900000000     C                   Z-ADD     1             FUNGRU
067000000000     C                   END
067100000000     C*
067200000000     C* PRIMO GRUPPO DI 6 FUNZIONI
067300000000     C     FUNGRU        IFEQ      1
067400000000     C                   MOVEL     FUV(1)        FUN001
067500000000     C                   MOVEL     FUV(2)        FUN002
067600000000     C                   MOVEL     FUV(3)        FUN003
067700000000     C                   MOVEL     FUV(4)        FUN004
067800000000     C                   MOVEL     FUV(5)        FUN005
067900000000     C                   MOVEL     FUV(6)        FUN006
068000000000     C                   END
068100000000     C*
068200000000     C* SECONDO GRUPPO DI 6 FUNZIONI
068300000000     C     FUNGRU        IFEQ      2
068400000000     C                   MOVEL     FUV(7)        FUN001
068500000000     C                   MOVEL     FUV(8)        FUN002
068600000000     C                   MOVEL     FUV(9)        FUN003
068700000000     C                   MOVEL     FUV(10)       FUN004
068800000000     C                   MOVEL     FUV(11)       FUN005
068900000000     C                   MOVEL     FUV(12)       FUN006
069000000000     C                   END
069100000000     C*
069200000000     C* TERZO GRUPPO DI 6 FUNZIONI
069300000000     C     FUNGRU        IFEQ      3
069400000000     C                   MOVEL     FUV(13)       FUN001
069500000000     C                   MOVEL     FUV(14)       FUN002
069600000000     C                   MOVEL     FUV(15)       FUN003
069700000000     C                   MOVEL     FUV(16)       FUN004
069800000000     C                   MOVEL     FUV(17)       FUN005
069900000000     C                   MOVEL     FUV(18)       FUN006
070000000000     C                   END
070100000000     C*
070200000000     C* QUARTO GRUPPO DI 6 FUNZIONI
070300000000     C     FUNGRU        IFEQ      4
070400000000     C                   MOVEL     FUV(19)       FUN001
070500000000     C                   MOVEL     FUV(20)       FUN002
070600000000     C                   MOVEL     FUV(21)       FUN003
070700000000     C                   MOVEL     FUV(22)       FUN004
070800000000     C                   MOVEL     *BLANKS       FUN005
070900000000     C                   MOVEL     *BLANKS       FUN006
071000000000     C                   END
071100000000     C*
071200000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
071300000000     C                   MOVEL     *BLANKS       FUN007
071400000000     C                   MOVEL     *BLANKS       FUN008
071500000000     C*
071600000000     C     1             DO        24            I
071700000000     C                   MOVEL     FUD(I)        FUDRIG
071800000000     C     FUDFUN        IFEQ      '24'
071900000000     C     FUDATT        ANDEQ     'S'
072000000000     C                   MOVEL     FUDDES        FUN008
072100000000     C                   END
072200000000     C                   END                                                    DO
072300000000     C                   ENDSR
072400000000     C*------------------------------------------------------------------------*
072500000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
072600000000     C*------------------------------------------------------------------------*
072700000000     C     FUNCON        BEGSR
072800000000     C*
072900000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
073000000000     C                   EXSR      FUNVAR
073100000000     C*
073200000000     C* PULISCO CAMPI DI LAVORO
073300000000     C                   Z-ADD     0             I                 5 0
073400000000     C                   Z-ADD     0             W                 5 0
073500000000     C*
073600000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
073700000000     C     FUNKEY        IFNE      *BLANKS
073800000000     C     1             DO        24            I
073900000000     C                   MOVEL     FUD(I)        FUDRIG
074000000000     C*
074100000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
074200000000     C     FUNKEY        IFEQ      FUDFUN
074300000000     C     FUDATT        ANDEQ     'S'
074400000000     C                   Z-ADD     I             W
074500000000     C                   END
074600000000     C                   END                                                    DO WHILE
074700000000     C*
074800000000     C* FUNZIONE NON ATTIVA (ERRORE)
074900000000     C     W             IFEQ      0
075000000000     C*
075100000000     C* EMISSIONE MESSAGGIO DI ERRORE
075200000000     C                   MOVEL     MSG(1)        DSMSMS
075300000000     C*
075400000000     C                   END                                                    W=0
075500000000     C                   END                                                    FUNKEY NE *BLANKS
075600000000     C*
075700000000     C* FUNZIONE ATTIVA (PROCEDI ...)
075800000000     C     W             IFNE      0
075900000000     C     FUNKEY        OREQ      *BLANKS
076000000000     C*
076100000000     C     FUNKEY        IFEQ      *BLANKS
076200000000     C                   EXSR      ENTGES
076300000000     C                   END
076400000000     C     FUNKEY        IFEQ      '03'
076500000000     C                   EXSR      F03GES
076600000000     C                   END
076700000000     C     FUNKEY        IFEQ      '06'
076800000000     C                   EXSR      F06GES
076900000000     C                   END
077000000000     C     FUNKEY        IFEQ      '12'
077100000000     C                   EXSR      F12GES
077200000000     C                   END
077300000000     C     FUNKEY        IFEQ      '24'
077400000000     C                   EXSR      F24GES
077500000000     C                   END
077600000000     C*
077700000000     C                   END
077800000000     C*
077900000000     C                   ENDSR
078000000000     C*------------------------------------------------------------------------*
078100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
078200000000     C*------------------------------------------------------------------------*
078300000000     C     FUNVAR        BEGSR
078400000000     C*
078500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
078600000000     C*
078700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
078800000000     C     KEY           IFEQ      ENTER
078900000000     C                   MOVEL     *BLANKS       FUNKEY
079000000000     C                   END
079100000000     C     KEY           IFEQ      F1
079200000000     C                   MOVEL     '01'          FUNKEY
079300000000     C                   END
079400000000     C     KEY           IFEQ      F2
079500000000     C                   MOVEL     '02'          FUNKEY
079600000000     C                   END
079700000000     C     KEY           IFEQ      F3
079800000000     C                   MOVEL     '03'          FUNKEY
079900000000     C                   END
080000000000     C     KEY           IFEQ      F4
080100000000     C                   MOVEL     '04'          FUNKEY
080200000000     C                   END
080300000000     C     KEY           IFEQ      F5
080400000000     C                   MOVEL     '05'          FUNKEY
080500000000     C                   END
080600000000     C     KEY           IFEQ      F6
080700000000     C                   MOVEL     '06'          FUNKEY
080800000000     C                   END
080900000000     C     KEY           IFEQ      F7
081000000000     C                   MOVEL     '07'          FUNKEY
081100000000     C                   END
081200000000     C     KEY           IFEQ      F8
081300000000     C                   MOVEL     '08'          FUNKEY
081400000000     C                   END
081500000000     C     KEY           IFEQ      F9
081600000000     C                   MOVEL     '09'          FUNKEY
081700000000     C                   END
081800000000     C     KEY           IFEQ      F10
081900000000     C                   MOVEL     '10'          FUNKEY
082000000000     C                   END
082100000000     C     KEY           IFEQ      F11
082200000000     C                   MOVEL     '11'          FUNKEY
082300000000     C                   END
082400000000     C     KEY           IFEQ      F12
082500000000     C                   MOVEL     '12'          FUNKEY
082600000000     C                   END
082700000000     C     KEY           IFEQ      F13
082800000000     C                   MOVEL     '13'          FUNKEY
082900000000     C                   END
083000000000     C     KEY           IFEQ      F14
083100000000     C                   MOVEL     '14'          FUNKEY
083200000000     C                   END
083300000000     C     KEY           IFEQ      F15
083400000000     C                   MOVEL     '15'          FUNKEY
083500000000     C                   END
083600000000     C     KEY           IFEQ      F16
083700000000     C                   MOVEL     '16'          FUNKEY
083800000000     C                   END
083900000000     C     KEY           IFEQ      F17
084000000000     C                   MOVEL     '17'          FUNKEY
084100000000     C                   END
084200000000     C     KEY           IFEQ      F18
084300000000     C                   MOVEL     '18'          FUNKEY
084400000000     C                   END
084500000000     C     KEY           IFEQ      F19
084600000000     C                   MOVEL     '19'          FUNKEY
084700000000     C                   END
084800000000     C     KEY           IFEQ      F20
084900000000     C                   MOVEL     '20'          FUNKEY
085000000000     C                   END
085100000000     C     KEY           IFEQ      F21
085200000000     C                   MOVEL     '21'          FUNKEY
085300000000     C                   END
085400000000     C     KEY           IFEQ      F22
085500000000     C                   MOVEL     '22'          FUNKEY
085600000000     C                   END
085700000000     C     KEY           IFEQ      F23
085800000000     C                   MOVEL     '23'          FUNKEY
085900000000     C                   END
086000000000     C     KEY           IFEQ      F24
086100000000     C                   MOVEL     '24'          FUNKEY
086200000000     C                   END
086300000000     C     KEY           IFEQ      ROLLUP
086400000000     C                   MOVEL     'RU'          FUNKEY
086500000000     C                   END
086600000000     C     KEY           IFEQ      HELP
086700000000     C                   MOVEL     'HP'          FUNKEY
086800000000     C                   END
086900000000     C                   ENDSR
087000000000     C*------------------------------------------------------------------------*
087100000000     C* NOTE PER COMPILAZIONE SCHIERE
087200000000     C*------------------------------------------------------------------------*
087300000000     C* -MSG-
087400000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
087500000000     C* (NON TOCCARE MSG,1)
087600000000     C*
087700000000     C* -CMD-
087800000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
087900000000     C*
088000000000     C* -FUD-
088100000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
088200000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
088300000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
088400000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
088500000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
088600000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
088700000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
088800000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
088900000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
089000000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
089100000000     C*
089200000000     C*------------------------------------------------------------------------*
089300000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
089400040827TIST31R- Tasto funzionale non gestito.                                         1
089500040827TIST31R- 2
089600040827TIST31R- 3
089700040827TIST31R- 4                                                                     4
089800040827TIST31R- 5
089900040827TIST31R- 6
090000040827TIST31R- 7
090100040827TIST31R- 8
090200040827TIST31R- 9
090300040827TIST31R- 10
090400040827TIST31R- 11
090500040827TIST31R- Divisione errata.                                                    12
090600040827TIST31R- Area errata.                                                         13
090700040827TIST31R- Punto operativo/terminal errato.                                     14
090800040827TIST31R- Cliente errato.                                                      15
090900040827TIST31R- Nessun servizio VAS abilitao per il cliente.                         16
091000040827TIST31R- 17
091100040827TIST31R- 18
091200040827TIST31R- 19
091300040827TIST31R- 20
091400040827TIST31R- 21
091500000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
091600000000                   -        (S/N)=N                        funz.01¬
091700000000                   -        (S/N)=N                        funz.02¬
091800000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
091900000000                   -        (S/N)=N                        funz.04¬
092000000000                   -        (S/N)=N                        funz.05¬
092100000000F06=Lancio         -        (S/N)=S                        funz.06¬
092200000000                   -        (S/N)=N                        funz.07¬
092300000000                   -        (S/N)=N                        funz.08¬
092400000000                   -        (S/N)=N                        funz.09¬
092500000000                   -        (S/N)=N                        funz.10¬
092600000000                   -        (S/N)=N                        funz.11¬
092700000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
092800000000                   -        (S/N)=N                        funz.13¬
092900000000                   -        (S/N)=N                        funz.14¬
093000000000                   -        (S/N)=N                        funz.15¬
093100000000                   -        (S/N)=N                        funz.16¬
093200000000                   -        (S/N)=N                        funz.17¬
093300000000                   -        (S/N)=N                        funz.18¬
093400000000                   -        (S/N)=N                        funz.19¬
093500000000                   -        (S/N)=N                        funz.20¬
093600000000                   -        (S/N)=N                        funz.21¬
093700000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
093800000000                   -        (S/N)=N                        funz.23¬
093900000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
