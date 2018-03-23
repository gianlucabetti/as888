000100020325     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020325      *------------------------------------------------------------------------*
000300020325      * GESTISCE TUTTE LE FUNZIONI SULL'ANAGRAFICA DOTAZIONI FILIALI UNITA EDP *
000400020325      *      - MODIFICA,CANCELLAZIONE,VISUALIZZAZIONE,RIPRISTINO,INSERIMENTO   *
000500020325      *------------------------------------------------------------------------*
000600020325
000700020325      *------------------------------------------------------------------------*
000800020325      * DATA BASE
000900020325      *------------------------------------------------------------------------*
001000941215     FTNSDUDD   CF   E             WORKSTN INFDS(DEVDS1)
001100941215     F                                     SFILE(SDUDDMS:MSGULT)
001200941215     FUNDOT01L  UF A E           K DISK
001300020528     FAZORG21l  IF   E           K DISK
001400020528     fazorg01l  if   e           k disk
001500020325     ftntbe01l  if   e           k disk
001600020325
001700941215     D*------------------------------------------------------------------------*
001800941215     D* SCHIERE
001900941215     D*------------------------------------------------------------------------*
002000000000     D* MESSAGGI
002100930208     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200941216     D* COMANDI
002300941216     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002400000000     D* DESCRIZIONI TASTI FUNZIONALI
002500000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002700000000     D FUV             S             19    DIM(24)
002800941215     D* FILIALI E LORO DECODIFICHE
002900971124     D FIL             S              3  0 DIM(500)
003000971124     D DFIL            S             20    DIM(500)
003100941215     D* TIPI UNITA E LORO DECODIFICHE
003200941215     D UNI             S              2  0 DIM(100)
003300941215     D DUNI            S             25    DIM(100)
003400020325
003500941215     D*------------------------------------------------------------------------*
003600941215     D* INPUT
003700941215     D*------------------------------------------------------------------------*
003800941215     D*-------------------
003900000000     D* RIDEFINIZIONE SCHIERA -FUD-
004000941215     D*-------------------
004100000000     D FUDDS           DS
004200000000     D*                -DESCRIZIONE PARZIALE
004300000000     D  FUDDE1                 1      1
004400000000     D*                -FUNZIONE
004500000000     D  FUDFUN                 2      3
004600000000     D*                -DESCRIZIONE PARZIALE
004700000000     D  FUDDE2                 4     19
004800000000     D*                -FILLER
004900000000     D  FUDFI0                20     34
005000000000     D*                -FLAG FUNZ.ATTIVA (S/N)
005100000000     D  FUDATT                35     35
005200000000     D*                -FILLER
005300000000     D  FUDFI1                36     80
005400000000     D*                -DESCRIZIONE COMPLETA
005500000000     D  FUDDES                 1     19
005600000000     D*                -RIDEFINIZIONE RIGA
005700000000     D  FUDRIG                 1     80
005800941215     D*-------------------
005900930201     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
006000941215     D*-------------------
006100930201     D DSMSG         E DS
006200941215     D*-------------------
006300930312     D* PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
006400941215     D*-------------------
006500930709     D DSUNIT        E DS
006600941215     D*-------------------
006700930201     D* DS PER GESTIONE TASTI FUNZIONALI
006800941215     D*-------------------
006900930201     D DSKEY         E DS
007000941215     D DEVDS1          DS
007100941215     D  KEY                  369    369
007200941215     D*-------------------
007300930201     D* ARCHITETTURA
007400941215     D*-------------------
007500930201     D KPJBA         E DS
007600020325
007700020325     d tibs02ds      e ds
007800020325
007900020709     d ddatiute      e ds
008000020709     d azuteds       e ds                  extname(azute00f)
008100020709     d tibs34ds      e ds                  inz
008200941215     C*------------------------------------------------------------------------*
008300941215     C* MAIN LINE
008400941215     C*------------------------------------------------------------------------*
008500000000     C*
008600930225     C* SE CONTROLLI INIZIALI NON OK, RITORNO A PGM CHIAMANTE
008700930225     C     ERRL00        IFEQ      '1'                                          --- 1 -->
008800930225     C*
008900930225     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
009000930709     C                   MOVEL     '1'           DSUERR
009100930709     C                   MOVEL     DSMSMS        DSUMSG
009200930225     C*
009300930225     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
009400930225     C                   ELSE                                                   <-- 1 -->
009500000000     C*
009600000000     C* CICLO FINO A CHE:
009700000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009800000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009900930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
010000000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
010100000000     C     F12L00        OREQ      '1'
010200930202     C     FLFINE        OREQ      '1'
010300000000     C*
010400000000     C* EMETTE FORMATO VIDEO
010500000000     C                   EXSR      BUTFMR
010600000000     C*
010700000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010800000000     C                   EXSR      FUNCON
010900000000     C*
011000930225     C                   END                                                    <-- 2 ---
011100000000     C*
011200930225     C                   END                                                    <-- 1 ---
011300000000     C*
011400940105     C                   MOVEL     DSUNIT        KPJBU
011500000000     C                   SETON                                        LR
011600000000     C*================================================================
011700000000     C* EMISSIONE FORMATO A VIDEO
011800000000     C*================================================================
011900000000     C     BUTFMR        BEGSR
012000930204     C*
012100930204     C* WRITE RECORD TESTATA
012200941215     C                   WRITE     SDUDD01
012300930205     C*
012400930205     C* VISUALIZZAZIONE MESSAGGIO
012500930205     C                   EXSR      SNDMSG
012600930204     C*
012700930204     C* WRITE RECORD FUNZIONI
012800941215     C                   WRITE     SDUDD02
012900000000     C*
013000930204     C* EXFMT RECORD DETTAGLIO
013100941215     C                   EXFMT     SDUDD03
013200000000     C*
013300000000     C* AZZERO FLAG DI ERRORE LIVELLO
013400000000     C                   MOVEL     '0'           ERRL00
013500000000     C*
013600000000     C* AZZERO FLAG DI ERRORE
013700930709     C                   SETOFF                                       606162
013800000000     C*
013900000000     C                   ENDSR
014000000000     C*================================================================
014100000000     C* SNDMSG - MANDA MESSAGGIO
014200000000     C*================================================================
014300000000     C     SNDMSG        BEGSR
014400000000     C*
014500000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014600000000     C                   MOVEL     '1'           *IN40
014700941215     C                   WRITE     SDUDDMC
014800000000     C                   MOVEL     '0'           *IN40
014900000000     C*
015000000000     C* VISUALIZZAZIONE MESSAGGIO
015100000000     C                   Z-ADD     0             MSGULT            4 0
015200930202     C                   CALL      'XMSG'        XMSGPA
015300930201     C     DSMSER        IFEQ      0
015400930201     C                   MOVEL     DSMSKE        MSGKEY
015500000000     C                   END
015600000000     C                   Z-ADD     1             MSGULT
015700941215     C                   WRITE     SDUDDMS                                      SFL MESS
015800941215     C                   WRITE     SDUDDMC                                      CONTR MESS
015900000000     C*
016000000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
016100930201     C                   MOVEL     *BLANKS       DSMSMS
016200000000     C*
016300000000     C                   ENDSR
016400000000     C*================================================================
016500000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016600000000     C*================================================================
016700000000     C     ENTGES        BEGSR
016800930201     C*
016900930201     C* CONTROLLA I CAMPI IMMESSI
017000930201     C                   EXSR      DECODI
017100930202     C*
017200930202     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
017300930709     C     DSUOP1        IFEQ      'O05'
017400930202     C                   MOVEL     '1'           FLFINE
017500930202     C                   END
017600000000     C*
017700000000     C                   ENDSR
017800000000     C*================================================================
017900930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
018000000000     C*================================================================
018100000000     C     F01GES        BEGSR
018200000000     C                   ENDSR
018300000000     C*================================================================
018400930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
018500000000     C*================================================================
018600000000     C     F03GES        BEGSR
018700000000     C*
018800930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
018900000000     C                   MOVEL     '1'           F03L00
019000930202     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
019100930709     C                   MOVEL     '1'           DSUF03
019200000000     C*
019300000000     C                   ENDSR
019400930201     C*================================================================
019500930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
019600930201     C*================================================================
019700930201     C     F06GES        BEGSR
019800930201     C*
019900930201     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
020000930709     C     DSUOP1        IFEQ      'F06'
020100930201     C                   EXSR      INSERI
020200930201     C                   END
020300930709     C     DSUOP1        IFEQ      'O02'
020400930202     C                   EXSR      MODIFI
020500930201     C                   END
020600930709     C     DSUOP1        IFEQ      'O04'
020700930201     C                   EXSR      CANCEL
020800930201     C                   END
020900930709     C     DSUOP1        IFEQ      'O07'
021000930201     C                   EXSR      RIPRIS
021100930201     C                   END
021200930201     C*
021300930201     C                   ENDSR
021400000000     C*================================================================
021500930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
021600000000     C*================================================================
021700000000     C     F07GES        BEGSR
021800000000     C                   ENDSR
021900000000     C*================================================================
022000930709     C* F08GES -GESTIONE TASTO FUNZIONALE F08 - GEST.MOVIMENTI
022100000000     C*================================================================
022200000000     C     F08GES        BEGSR
022300000000     C                   ENDSR
022400000000     C*================================================================
022500930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
022600000000     C*================================================================
022700000000     C     F12GES        BEGSR
022800000000     C*
022900000000     C* IMPOSTO FLAG DI LIVELLO
023000000000     C                   MOVEL     '1'           F12L00
023100000000     C*
023200000000     C                   ENDSR
023300930201     C*================================================================
023400930709     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ANAGR.UNITA
023500930201     C*================================================================
023600930201     C     F21GES        BEGSR
023700941216     C*
023800941216     C* LANCIA PROGRAMMA DI STAMPA DELLE UNITA
023900941216     C                   MOVEL     DSUNIT        KPJBU
024000941216     C                   CALL      'TNSDUER'
024100941216     C                   PARM                    KPJBA
024200941216     C                   MOVEL     KPJBU         DSUNIT
024300941216     C*
024400941216     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
024500941216     C     DSUF03        IFEQ      '1'
024600941216     C                   MOVEL     '1'           F03L00
024700941216     C                   MOVEL     '1'           ERRL00
024800941216     C                   ENDIF
024900941216     C*
025000930201     C                   ENDSR
025100000000     C*================================================================
025200930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
025300000000     C*================================================================
025400000000     C     F22GES        BEGSR
025500941216     C*
025600941216     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
025700941216     C                   Z-ADD     80            LENGH            15 5
025800941216     C                   CALL      'QCAEXEC'
025900941216     C                   PARM                    CMD(1)
026000941216     C                   PARM                    LENGH
026100941216     C*
026200000000     C                   ENDSR
026300000000     C*================================================================
026400930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
026500000000     C*================================================================
026600000000     C     F24GES        BEGSR
026700000000     C*
026800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
026900000000     C                   EXSR      FUNGES
027000000000     C*
027100000000     C                   ENDSR
027200000000     C*================================================================
027300000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
027400000000     C*================================================================
027500000000     C     HELGES        BEGSR
027600000000     C                   ENDSR
027700930201     C*================================================================
027800930201     C* INSERI - FUNZIONE DI INSERIMENTO
027900930201     C*================================================================
028000930201     C     INSERI        BEGSR
028100930201     C*
028200930201     C* CONTROLLA I CAMPI IMMESSI
028300930201     C                   EXSR      DECODI
028400930201     C*
028500930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
028600941215     C*
028700930709     C* IMPOSTO IL RECORD DAL VIDEO
028800930709     C                   EXSR      VIDREC
028900971014     C                   MOVEL     *BLANKS       DOTFTR
029000971014     C                   Z-ADD     *ZEROS        DOTDTR
029100971014     C                   MOVEL     *BLANKS       DOTATB
029200941215     C                   WRITE     UNDOT000
029300941215     C*
029400941220     C                   CLEAR                   SDUDD03
029500930518     C                   MOVEL     MSG(3)        DSMSMS
029600930201     C                   END                                                    <-- 1 ---
029700930201     C*
029800930201     C                   ENDSR
029900930201     C*================================================================
030000930201     C* MODIFI - FUNZIONE DI MODIFICA
030100930201     C*================================================================
030200930201     C     MODIFI        BEGSR
030300930201     C*
030400930201     C* CONTROLLA I CAMPI IMMESSI
030500930201     C                   EXSR      DECODI
030600930201     C*
030700930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
030800930201     C*
030900930709     C* IMPOSTO IL RECORD DAL VIDEO E RISCRIVO
031000930709     C                   EXSR      VIDREC
031100971014     C                   MOVEL     *BLANKS       DOTFTR
031200971014     C                   MOVEL     *BLANKS       DOTATB
031300941215     C                   UPDATE    UNDOT000
031400941215     C*
031500930518     C                   MOVEL     '1'           FLFINE
031600930709     C                   MOVEL     MSG(4)        DSUMSG
031700930201     C                   END                                                    <-- 1 ---
031800930201     C*
031900930201     C                   ENDSR
032000930201     C*================================================================
032100930201     C* CANCEL - FUNZIONE DI CANCELLAZIONE
032200930201     C*================================================================
032300930201     C     CANCEL        BEGSR
032400930201     C*
032500971014     C* ANNULLO RECORD
032600971014IF  1C     DOTDTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
032700971014     C                   DELETE    UNDOT000
032800971014X   1C                   ELSE                                                   *GIA' TRASMESSO
032900971014     C                   MOVEL     'A'           DOTATB
033000971014     C                   MOVEL     *BLANKS       DOTFTR
033100971014     C                   UPDATE    UNDOT000
033200971014E   1C                   ENDIF
033300930201     C*
033400930201     C                   MOVEL     '1'           FLFINE
033500930709     C                   MOVEL     MSG(5)        DSUMSG
033600930201     C*
033700930201     C                   ENDSR
033800930201     C*================================================================
033900930201     C* RIPRIS - FUNZIONE DI RIPRISTINO
034000930201     C*================================================================
034100930201     C     RIPRIS        BEGSR
034200930202     C*
034300930709     C* RIPRISTINO IL RECORD
034400941215     C                   MOVEL     *BLANKS       DOTFTR
034500941215     C                   MOVEL     *BLANKS       DOTATB
034600941215     C                   UPDATE    UNDOT000
034700930202     C*
034800930202     C                   MOVEL     '1'           FLFINE
034900930709     C                   MOVEL     MSG(6)        DSUMSG
035000930202     C*
035100930201     C                   ENDSR
035200000000     C*================================================================
035300930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
035400000000     C*================================================================
035500000000     C     DECODI        BEGSR
035600930709     C*
035700930709     C* CONTROLLO E DECODIFICO IL TIPO UNITA
035800941215     C                   EXSR      CHKTIP
035900941215     C                   MOVEL     WTIP          VIDTIP
036000941215     C                   MOVEL     WDTIP         VIDTID
036100941215     C*
036200941215     C* CONTROLLO E DECODIFICO IL MAGAZZINO
036300941219     C     ERRL00        IFEQ      '0'
036400941215     C                   EXSR      CHKMAG
036500941216     C                   MOVEL     WFIL          VIDFIL
036600941216     C                   MOVEL     WDFIL         VIDFID
036700941219     C                   ENDIF
036800941215     C***
036900941215     C* SE INSERIMENTO CONTROLLA ESISTENZA RECORD
037000941215     C***
037100930709     C     ERRL00        IFEQ      '0'
037200930709     C     DSUOP1        IFEQ      'F06'
037300941215     C                   Z-ADD     WTIP          KDOTIP
037400941215     C                   Z-ADD     WFIL          KDOFIL
037500941215     C     KEYDOT        CHAIN     UNDOT01L                           99
037600941215     C     *IN99         IFEQ      *OFF
037700930518     C                   MOVEL     '1'           ERRL00
037800930518     C                   MOVEL     MSG(2)        DSMSMS
037900930709     C                   SETON                                        6061
038000930518     C                   END
038100930709     C                   END
038200930518     C                   END
038300941220     C*
038400941215     C* QUANTITA' IN DOTAZIONE NO UGUALE A ZERO
038500941220     C*----------ERRL00    IFEQ '0'
038600941220     C*----------VIDQTA    IFEQ *ZEROS
038700941220     C*----------          MOVEL'1'       ERRL00
038800941220     C*----------          MOVELMSG,8     DSMSMS
038900941220     C*----------          SETON                     62
039000941220     C*----------          ENDIF
039100941220     C*----------          ENDIF
039200000000     C*
039300000000     C                   ENDSR
039400930709     C*================================================================
039500930709     C*  CHKTIP - CONTROLLA IL TIPO DI UNITA
039600930709     C*================================================================
039700930709     C     CHKTIP        BEGSR
039800941219     C*
039900941219     C                   Z-ADD     *ZEROS        WTIP
040000941219     C                   MOVEL     *BLANKS       WDTIP
040100930709     C*
040200930709     C* CONTROLLO SE E' STATO INSERITO '?'
040300941215     C                   SETOFF                                           28
040400941215     C     '?'           SCAN      VIDTIP                                 28
040500930709     C*
040600930709     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
040700020325     c                   if        *in28
040800020325     c                   clear                   tibs02ds
040900020325     c                   movel     'R'           t02mod
041000020325     c                   movel     knsif         t02sif
041100020325     c                   movel     'UNT'         t02cod
041200020325     c                   call      'TIBS02R'
041300020325     c                   parm                    kpjba
041400020325     c                   parm                    tibs02ds
041500020325     c                   movel     t02ke1        wtip              2 0
041600020325     c                   movel     t02uni        wdtip            25
041700020325     C                   MOVEL     '1'           ERRL00
041800020325     c                   endif                                                  <-- 1 ---
041900020325
042000941215     C* NO CARATTERI
042100020325IF  1C     VIDTIP        IFLE      *ZEROS
042200941215     C                   MOVEL     *ALL'*'       WDTIP
042300941215     C                   MOVEL     '1'           ERRL00
042400941215     C                   MOVEL     MSG(7)        DSMSMS
042500941215     C                   SETON                                        60
042600020325X   1C                   ELSE
042700930709     C*
042800930709     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
042900941215     C                   Z-ADD     1             J
043000941215     C                   SETOFF                                           28
043100941215     C                   MOVEL     VIDTIP        WTIP
043200941215     C     WTIP          LOOKUP    UNI(J)                                 28
043300020325IF  2C     *IN28         IFEQ      *OFF
043400941215     C                   MOVEL     *ALL'*'       WDTIP
043500941215     C                   MOVEL     '1'           ERRL00
043600941215     C                   MOVEL     MSG(7)        DSMSMS
043700941215     C                   SETON                                        60
043800941215     C                   ELSE
043900941215     C                   MOVEL     DUNI(J)       WDTIP
044000020325E   2C                   ENDIF
044100020325E   1C                   ENDIF
044200930709     C*
044300930709     C                   ENDSR
044400930709     C*================================================================
044500930709     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
044600930709     C*================================================================
044700930709     C     CHKMAG        BEGSR
044800941219     C*
044900941219     C                   Z-ADD     *ZEROS        WFIL
045000941219     C                   MOVEL     *BLANKS       WDFIL
045100941215     C*
045200941215     C* CONTROLLO SE E' STATO INSERITO '?'
045300941215     C                   SETOFF                                           28
045400941215     C     '?'           SCAN      VIDFIL                                 28
045500941215     C*
045600941215     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE FILIALI
045700941215IF  1C     *IN28         IFEQ      *ON
045800941215     C                   MOVEL     *BLANKS       §COD1             3
045900941215     C                   MOVEL     *BLANKS       §DES1
046000941215     C                   CALL      'TNSD20R'
046100941215     C                   PARM                    §COD1
046200941215     C                   PARM                    §TIP              1
046300941215     C                   PARM                    §DES1            25
046400941219     C                   MOVEL     §COD1         WFIL              3 0
046500941219     C                   MOVEL     §DES1         WDFIL            20
046600941215     C                   MOVEL     '1'           ERRL00
046700941215     C*
046800941215X   1C                   ELSE
046900941215     C* NO CARATTERI
047000941219IF  2C     VIDFIL        IFLE      *ZEROS
047100941215     C                   MOVEL     *ALL'*'       WDFIL
047200941215     C                   MOVEL     '1'           ERRL00
047300941215     C                   MOVEL     MSG(15)       DSMSMS
047400941215     C                   SETON                                        61
047500941215X   2C                   ELSE
047600941215     C*
047700941215     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
047800941215     C                   Z-ADD     1             J
047900941215     C                   SETOFF                                           28
048000941215     C                   MOVEL     VIDFIL        WFIL
048100941215     C     WFIL          LOOKUP    FIL(J)                                 28
048200941215IF  3C     *IN28         IFEQ      *OFF
048300941215     C                   MOVEL     *ALL'*'       WDFIL
048400941215     C                   MOVEL     '1'           ERRL00
048500941215     C                   MOVEL     MSG(15)       DSMSMS
048600941215     C                   SETON                                        61
048700941215     C                   ELSE
048800941215     C                   MOVEL     DFIL(J)       WDFIL
048900941215E   3C                   ENDIF
049000941215E   2C                   ENDIF
049100941215E   1C                   ENDIF
049200941215     C*
049300930709     C                   ENDSR
049400930201     C*================================================================
049500930709     C*  RECVID - PORTA I CAMPI DAL RECORD AL VIDEO
049600930201     C*================================================================
049700930709     C     RECVID        BEGSR
049800930201     C*
049900941215     C                   MOVEL     DOTTIP        VIDTIP
050000941215     C                   MOVEL     DOTFIL        VIDFIL
050100941215     C                   Z-ADD     DOTQTA        VIDQTA
050200941215     C*
050300941215     C                   MOVEL     *BLANKS       VIDTID
050400941215     C                   MOVEL     *BLANKS       VIDFID
050500930202     C*
050600941215     C     DOTATB        IFEQ      'A'
050700930202     C                   MOVEL     'ANNULLAT'    VIDATB
050800930202     C                   MOVE      'O'           VIDATB
050900930202     C                   ELSE
051000930202     C                   MOVEL     *BLANKS       VIDATB
051100930202     C                   END
051200930201     C*
051300930201     C                   ENDSR
051400941215     C*================================================================
051500930201     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
051600930201     C*================================================================
051700930201     C     AZZVID        BEGSR
051800941215     C*
051900941215     C                   MOVEL     *ZEROS        VIDTIP
052000941215     C                   MOVEL     *ZEROS        VIDFIL
052100941215     C                   Z-ADD     *ZEROS        VIDQTA
052200941215     C*
052300941215     C                   MOVEL     *BLANKS       VIDTID
052400941215     C                   MOVEL     *BLANKS       VIDFID
052500941215     C                   MOVEL     *BLANKS       VIDATB
052600930201     C*
052700930201     C                   ENDSR
052800930201     C*================================================================
052900930709     C*  VIDREC - IMPOSTA CAMPI DA VIDEO A RECORD
053000930201     C*================================================================
053100930709     C     VIDREC        BEGSR
053200941215     C*
053300941215     C                   MOVEL     VIDTIP        DOTTIP
053400941215     C                   MOVEL     VIDFIL        DOTFIL
053500941215     C                   Z-ADD     VIDQTA        DOTQTA
053600930201     C*
053700930201     C                   ENDSR
053800000000     C*================================================================
053900941215     C* *INZSR - ROUTINE INIZIALE
054000000000     C*================================================================
054100941215     C     *INZSR        BEGSR
054200930201     C*
054300930201     C* RICEVIMENTO PARAMETRI
054400930201     C     *ENTRY        PLIST
054500940105     C                   PARM                    KPJBA
054600940105     C                   MOVEL     KPJBU         DSUNIT
054700930201     C*
054800930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
054900930201     C     XMSGPA        PLIST
055000930201     C                   PARM                    DSMSMS
055100930201     C                   PARM                    DSMSPG
055200930201     C                   PARM                    DSMSKE
055300930201     C                   PARM                    DSMSER
055400941215     C* DEFINIZIONE CAMPI
055500941215     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
055600941215     C     *LIKE         DEFINE    DOTTIP        KDOTIP                         *UNDOT00F
055700941215     C     *LIKE         DEFINE    DOTFIL        KDOFIL
055800941215     C*
055900941215     C* CHIAVE LETTURA UNDOT01L - PARZIALE
056000941216     C     KEYDOT        KLIST
056100941215     C                   KFLD                    KDOTIP
056200941215     C                   KFLD                    KDOFIL
056300941215     C*
056400020528     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
056500941215     C     KEYORG        KLIST
056600941215     C                   KFLD                    KORFIL
056700930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
056800930201     C                   CALL      'XKEY'
056900930201     C                   PARM                    DSKEY
057000930201     C*
057100930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
057200930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
057300930201     C*
057400930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
057500930201     C*   -NOME PGM CHIAMANTE
057600930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
057700941215     C                   MOVEL     'TNSDUDR'     DSMSPG
057800941215     C                   MOVEL     'TNSDUDR'     MSGQUE
057900930201     C                   MOVEL     *BLANKS       MSGKEY
058000930201     C                   MOVEL     *BLANKS       DSMSMS
058100930202     C*
058200930202     C* SE VISUALIZZAZIONE TOLGO FUNZIONE AGGIORNAMENTO
058300930709     C     DSUOP1        IFEQ      'O05'
058400930202     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
058500930202     C                   MOVEL     'N'           FUDATT
058600930202     C                   MOVEL     FUDRIG        FUD(6)
058700930202     C                   END
058800940105     C*
058900940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DEI MOVIMENTI UNITA'
059000940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
059100940105     C     DSUPRO        IFEQ      'M'
059200940105     C                   MOVEL     FUD(8)        FUDRIG                         *GEST.MOVIMENTI
059300940105     C                   MOVEL     'N'           FUDATT
059400940105     C                   MOVEL     FUDRIG        FUD(8)
059500940105     C                   END
059600930201     C*
059700930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
059800930201     C                   Z-ADD     0             FUNGRU            5 0
059900930201     C*
060000930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
060100930201     C                   EXSR      FUNGES
060200930201     C*
060300930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
060400930201     C                   MOVEL     '0'           F12L00            1
060500930201     C                   MOVEL     '0'           F03L00            1
060600930201     C                   MOVEL     '0'           FINL00            1
060700930201     C                   MOVEL     '0'           ERRL00            1
060800930201     C                   MOVEL     '0'           FLFINE            1
060900930204     C*
061000930204     C* LANCIO ROUTINE PER VERIFICARE SE
061100930204     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
061200930204     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
061300020709
061400020709     c     *dtaara       define    §azute        azuteds
061500020709     c     *dtaara       define    §datiute      ddatiute
061600020709
061700020709     c                   in(E)     *dtaara
061800020709     c                   if        %Error or rsut = *blanks
061900020709     c                   clear                   tibs34ds
062000020709     c                   call      'TIBS34R'
062100020709     c                   parm                    Tibs34Ds
062200020709     c                   in        *dtaara
062300020709     c                   endif
062400941215     C* CARICA ORGANIGRAMMA
062500941215     C                   EXSR      CARORG
062600941215     C* CARICA TABELLE
062700941215     C                   EXSR      CARTAB
062800930201     C*
062900930201     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
063000930201     C* 0=NON PROTETTO, 1=PROTETTO
063100930709     C*   91 - CAMPI CHIAVE
063200930709     C*   90 - DATI FILE
063300930201     C* --> UTENTE ENTRATO IN GESTIONE
063400940105     C     DSUOP0        IFEQ      ' 2'                                         --- 1 -->
063500930201     C*
063600930709     C     DSUOP1        IFEQ      'F06'                                        *INS.
063700941215     C                   SETOFF                                       9091
063800930201     C                   END
063900930201     C*
064000930709     C     DSUOP1        IFEQ      'O02'                                        *MOD.
064100930709     C                   SETON                                        91
064200020709     c                   If        DutLpo = 'S'
064300930709     C                   SETOFF                                       90
064400930201     C                   ELSE                                                   *FIL.
064500930709     C                   SETON                                        90
064600930201     C                   END
064700930201     C                   END
064800930201     C*
064900930709     C     DSUOP1        IFEQ      'O05'                                        *VIS+CANC+RIPR
065000930709     C     DSUOP1        OREQ      'O04'
065100930709     C     DSUOP1        OREQ      'O07'
065200941215     C                   SETON                                        9091
065300930201     C                   END
065400930201     C*
065500930201     C                   END                                                    <-- 1 ---
065600930201     C* --> UTENTE ENTRATO IN INTERROGAZIONE
065700940105     C     DSUOP0        IFEQ      ' 5'
065800941215     C                   SETON                                        9091
065900930201     C                   END
066000930201     C*
066100930709     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO RECORD
066200930709     C     DSUOP1        IFEQ      'O02'                                        --- 1 -->
066300930709     C     DSUOP1        OREQ      'O04'
066400930709     C     DSUOP1        OREQ      'O05'
066500930709     C     DSUOP1        OREQ      'O07'
066600930709     C* SE IL REC.NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
066700941215     C                   Z-ADD     DSUTIP        KDOTIP
066800941215     C                   Z-ADD     DSUMAG        KDOFIL
066900941215     C     KEYDOT        CHAIN     UNDOT01L                           99
067000930201     C     *IN99         IFEQ      '1'
067100930201     C                   MOVEL     '1'           ERRL00
067200930709     C                   MOVEL     MSG(16)       DSMSMS
067300930202     C                   ELSE
067400930709     C                   EXSR      RECVID
067500930201     C                   END
067600930208     C*
067700930208     C* SE TUTTO OK, DECODIFICO I CAMPI A VIDEO
067800020325     c                   if        errl00 = *off
067900930208     C                   EXSR      DECODI
068000020325     c                   endif
068100930201     C*
068200930202     C                   END                                                    <-- 1 ---
068300930202     C*
068400930202     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
068500930709     C     DSUOP1        IFEQ      'F06'
068600930202     C                   EXSR      AZZVID
068700930202     C                   END
068800930201     C*
068900930201     C* IMPOSTO DESCRIZIONE DELLA FUNZIONE IN CORSO A VIDEO
069000930709     C     DSUOP1        IFEQ      'F06'
069100930201     C                   MOVEL     ' INSERIM'    VIDFUN
069200930201     C                   MOVE      'ENTO   '     VIDFUN
069300930201     C                   END
069400930709     C     DSUOP1        IFEQ      'O02'
069500930201     C                   MOVEL     '   MODIF'    VIDFUN
069600930201     C                   MOVE      'ICA    '     VIDFUN
069700930201     C                   END
069800930709     C     DSUOP1        IFEQ      'O04'
069900930201     C                   MOVEL     ' CANCELL'    VIDFUN
070000930201     C                   MOVE      'AZIONE '     VIDFUN
070100930201     C                   END
070200930709     C     DSUOP1        IFEQ      'O05'
070300930201     C                   MOVEL     'VISUALIZ'    VIDFUN
070400930201     C                   MOVE      'ZAZIONE'     VIDFUN
070500930201     C                   END
070600930709     C     DSUOP1        IFEQ      'O07'
070700930201     C                   MOVEL     ' RIPRIST'    VIDFUN
070800930201     C                   MOVE      'INO    '     VIDFUN
070900930201     C                   END
071000000000     C*
071100000000     C                   ENDSR
071200941215     C*================================================================
071300941215     C* CARORG - CARICA ORGANIGRAMMA
071400941215     C*================================================================
071500941215     C     CARORG        BEGSR
071600941215     C*
071700941215     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
071800941215     C                   Z-ADD     *ZEROS        KORFIL
071900020528     C     KEYORG        SETLL     AZORG21l                           99        *FINE FILE
072000020528     C  N99              READ      AZORG21l                               99
072100941215     C*
072200941215     C     *IN99         DOWEQ     *OFF
072300941215     C                   ADD       1             J
072400941215     C                   Z-ADD     ORGFIL        FIL(J)
072500941215     C                   MOVEL     ORGDES        DFIL(J)
072600941215     C*
072700020528     C                   READ      AZORG21l                               99
072800941215     C                   ENDDO
072900020528
073000020528     c     keyorg        setll     azorg01l
073100020528     c                   do        *hival
073200020528     c                   read      azorg01l
073300020528     c                   if        %eof(azorg01l)
073400020528     c                   leave
073500020528     c                   endif
073600020528     c                   z-add     1             j
073700020528     c     orgfil        lookup    fil(j)                                 99
073800020528     c                   if        not *in99
073900020528     c                   z-add     1             j
074000020528     c     *zeros        lookup    fil(j)                                 99
074100020528     c                   if        *in99
074200020528     c                   movel     orgfil        fil(j)
074300020528     c                   movel     orgdes        dfil(j)
074400020528     c                   endif
074500020528     c                   endif
074600020528     c                   enddo
074700941215     C*
074800941215     C                   ENDSR
074900941215     C*================================================================
075000941215     C* CARTAB - CARICA TABELLE
075100941215     C*================================================================
075200941215     C     CARTAB        BEGSR
075300941215     C*--------------------
075400941215     C* TIPI UNITA'
075500941215     C*--------------------
075600941215     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
075700020325     C                   MOVEL     'UNT'         tbecod
075800020325     C     tbecod        SETLL     tntbe01l                           99        *FINE FILE
075900020325     C  N99tbecod        READE     tntbe01l                               99
076000941215     C*
076100941215     C     *IN99         DOWEQ     *OFF
076200020325     C     TBeatb        IFEQ      *BLANKS
076300941215     C                   ADD       1             J
076400020325     C                   MOVEL     TBeke1        UNI(J)
076500020325     C                   MOVEL     tbeuni        DUNI(J)
076600941215     C                   ENDIF
076700941215     C*
076800020325     C  N99tbecod        READE     tntbe01l                               99
076900941215     C                   ENDDO
077000941215     C*
077100941215     C                   ENDSR
077200000000     C*================================================================
077300000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
077400000000     C*================================================================
077500000000     C     FUNGES        BEGSR
077600000000     C*
077700000000     C* PULISCO CAMPI DI LAVORO
077800000000     C                   Z-ADD     0             W                 5 0
077900000000     C*
078000000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
078100000000     C* E CARICO TABELLA FUNZIONI ATTIVE
078200000000     C     1             DO        24            I
078300000000     C                   MOVEL     FUD(I)        FUDRIG
078400000000     C*
078500000000     C* PULIZIA FUNZIONI PRECEDENTI
078600000000     C                   MOVEL     *BLANKS       FUV(I)
078700000000     C*
078800000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
078900000000     C* (SALTA F24)
079000000000     C     FUDATT        IFEQ      'S'
079100000000     C     FUDFUN        ANDNE     '24'
079200000000     C                   ADD       1             W
079300000000     C                   MOVEL     FUDDES        FUV(W)
079400000000     C                   END
079500000000     C                   END                                                    DO WHILE
079600000000     C*
079700000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
079800000000     C* ALTRIMENTI DISATTIVO F24
079900000000     C     1             DO        24            I
080000000000     C                   MOVEL     FUD(I)        FUDRIG
080100000000     C*
080200000000     C     FUDFUN        IFEQ      '24'
080300000000     C     W             IFGT      6
080400000000     C                   MOVEL     'S'           FUDATT
080500000000     C                   ELSE
080600000000     C                   MOVEL     *BLANKS       FUDATT
080700000000     C                   END                                                    W GT 8
080800000000     C                   MOVEL     FUDRIG        FUD(I)
080900000000     C                   END
081000000000     C                   END                                                    DO WHILE
081100000000     C*
081200000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
081300000000     C     W             DIV       6             I
081400000000     C                   MVR                     Z
081500000000     C*
081600000000     C     Z             IFNE      0
081700000000     C     I             ADD       1             Z
081800000000     C                   ELSE
081900000000     C                   Z-ADD     I             Z                 5 0
082000000000     C                   END
082100000000     C*
082200000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
082300000000     C                   ADD       1             FUNGRU
082400000000     C     FUNGRU        IFGT      Z
082500000000     C     FUNGRU        ORLT      1
082600000000     C                   Z-ADD     1             FUNGRU
082700000000     C                   END
082800000000     C*
082900000000     C* PRIMO GRUPPO DI 6 FUNZIONI
083000000000     C     FUNGRU        IFEQ      1
083100000000     C                   MOVEL     FUV(1)        FUN001
083200000000     C                   MOVEL     FUV(2)        FUN002
083300000000     C                   MOVEL     FUV(3)        FUN003
083400000000     C                   MOVEL     FUV(4)        FUN004
083500000000     C                   MOVEL     FUV(5)        FUN005
083600000000     C                   MOVEL     FUV(6)        FUN006
083700000000     C                   END
083800000000     C*
083900000000     C* SECONDO GRUPPO DI 6 FUNZIONI
084000000000     C     FUNGRU        IFEQ      2
084100000000     C                   MOVEL     FUV(7)        FUN001
084200000000     C                   MOVEL     FUV(8)        FUN002
084300000000     C                   MOVEL     FUV(9)        FUN003
084400000000     C                   MOVEL     FUV(10)       FUN004
084500000000     C                   MOVEL     FUV(11)       FUN005
084600000000     C                   MOVEL     FUV(12)       FUN006
084700000000     C                   END
084800000000     C*
084900000000     C* TERZO GRUPPO DI 6 FUNZIONI
085000000000     C     FUNGRU        IFEQ      3
085100000000     C                   MOVEL     FUV(13)       FUN001
085200000000     C                   MOVEL     FUV(14)       FUN002
085300000000     C                   MOVEL     FUV(15)       FUN003
085400000000     C                   MOVEL     FUV(16)       FUN004
085500000000     C                   MOVEL     FUV(17)       FUN005
085600000000     C                   MOVEL     FUV(18)       FUN006
085700000000     C                   END
085800000000     C*
085900000000     C* QUARTO GRUPPO DI 6 FUNZIONI
086000000000     C     FUNGRU        IFEQ      4
086100000000     C                   MOVEL     FUV(19)       FUN001
086200000000     C                   MOVEL     FUV(20)       FUN002
086300000000     C                   MOVEL     FUV(21)       FUN003
086400000000     C                   MOVEL     FUV(22)       FUN004
086500000000     C                   MOVEL     *BLANKS       FUN005
086600000000     C                   MOVEL     *BLANKS       FUN006
086700000000     C                   END
086800000000     C*
086900000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
087000000000     C                   MOVEL     *BLANKS       FUN007
087100000000     C                   MOVEL     *BLANKS       FUN008
087200000000     C*
087300000000     C     1             DO        24            I
087400000000     C                   MOVEL     FUD(I)        FUDRIG
087500000000     C     FUDFUN        IFEQ      '24'
087600000000     C     FUDATT        ANDEQ     'S'
087700000000     C                   MOVEL     FUDDES        FUN008
087800000000     C                   END
087900000000     C                   END                                                    DO
088000000000     C                   ENDSR
088100000000     C*================================================================
088200000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
088300000000     C*================================================================
088400000000     C     FUNCON        BEGSR
088500000000     C*
088600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
088700000000     C                   EXSR      FUNVAR
088800000000     C*
088900000000     C* PULISCO CAMPI DI LAVORO
089000000000     C                   Z-ADD     0             I                 5 0
089100000000     C                   Z-ADD     0             W                 5 0
089200000000     C*
089300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
089400000000     C     FUNKEY        IFNE      *BLANKS
089500000000     C     1             DO        24            I
089600000000     C                   MOVEL     FUD(I)        FUDRIG
089700000000     C*
089800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
089900000000     C     FUNKEY        IFEQ      FUDFUN
090000000000     C     FUDATT        ANDEQ     'S'
090100000000     C                   Z-ADD     I             W
090200000000     C                   END
090300000000     C                   END                                                    DO WHILE
090400000000     C*
090500000000     C* FUNZIONE NON ATTIVA (ERRORE)
090600000000     C     W             IFEQ      0
090700000000     C*
090800000000     C* EMISSIONE MESSAGGIO DI ERRORE
090900930201     C                   MOVEL     MSG(1)        DSMSMS
091000930201     C*
091100930201     C                   END                                                    W=0
091200000000     C                   END                                                    FUNKEY NE *BLANKS
091300000000     C*
091400000000     C* FUNZIONE ATTIVA (PROCEDI ...)
091500000000     C     W             IFNE      0
091600000000     C     FUNKEY        OREQ      *BLANKS
091700000000     C*
091800000000     C     FUNKEY        IFEQ      *BLANKS
091900000000     C                   EXSR      ENTGES
092000000000     C                   END
092100000000     C     FUNKEY        IFEQ      '01'
092200000000     C                   EXSR      F01GES
092300000000     C                   END
092400000000     C     FUNKEY        IFEQ      '03'
092500000000     C                   EXSR      F03GES
092600000000     C                   END
092700930202     C     FUNKEY        IFEQ      '06'
092800930202     C                   EXSR      F06GES
092900930202     C                   END
093000000000     C     FUNKEY        IFEQ      '07'
093100000000     C                   EXSR      F07GES
093200000000     C                   END
093300000000     C     FUNKEY        IFEQ      '08'
093400000000     C                   EXSR      F08GES
093500000000     C                   END
093600000000     C     FUNKEY        IFEQ      '12'
093700000000     C                   EXSR      F12GES
093800000000     C                   END
093900930201     C     FUNKEY        IFEQ      '21'
094000930201     C                   EXSR      F21GES
094100930201     C                   END
094200000000     C     FUNKEY        IFEQ      '22'
094300000000     C                   EXSR      F22GES
094400000000     C                   END
094500000000     C     FUNKEY        IFEQ      '24'
094600000000     C                   EXSR      F24GES
094700000000     C                   END
094800000000     C     FUNKEY        IFEQ      'HP'
094900000000     C                   EXSR      HELGES
095000000000     C                   END
095100000000     C*
095200000000     C                   END
095300000000     C*
095400000000     C                   ENDSR
095500000000     C*================================================================
095600000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
095700000000     C*================================================================
095800000000     C     FUNVAR        BEGSR
095900000000     C*
096000000000     C* INIZIALIZZA VARIABILE -FUNKEY-
096100000000     C*
096200000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
096300000000     C     KEY           IFEQ      ENTER
096400000000     C                   MOVEL     *BLANKS       FUNKEY
096500000000     C                   END
096600000000     C     KEY           IFEQ      F1
096700000000     C                   MOVEL     '01'          FUNKEY
096800000000     C                   END
096900000000     C     KEY           IFEQ      F2
097000000000     C                   MOVEL     '02'          FUNKEY
097100000000     C                   END
097200000000     C     KEY           IFEQ      F3
097300000000     C                   MOVEL     '03'          FUNKEY
097400000000     C                   END
097500000000     C     KEY           IFEQ      F4
097600000000     C                   MOVEL     '04'          FUNKEY
097700000000     C                   END
097800000000     C     KEY           IFEQ      F5
097900000000     C                   MOVEL     '05'          FUNKEY
098000000000     C                   END
098100000000     C     KEY           IFEQ      F6
098200000000     C                   MOVEL     '06'          FUNKEY
098300000000     C                   END
098400000000     C     KEY           IFEQ      F7
098500000000     C                   MOVEL     '07'          FUNKEY
098600000000     C                   END
098700000000     C     KEY           IFEQ      F8
098800000000     C                   MOVEL     '08'          FUNKEY
098900000000     C                   END
099000000000     C     KEY           IFEQ      F9
099100000000     C                   MOVEL     '09'          FUNKEY
099200000000     C                   END
099300000000     C     KEY           IFEQ      F10
099400000000     C                   MOVEL     '10'          FUNKEY
099500000000     C                   END
099600000000     C     KEY           IFEQ      F11
099700000000     C                   MOVEL     '11'          FUNKEY
099800000000     C                   END
099900000000     C     KEY           IFEQ      F12
100000000000     C                   MOVEL     '12'          FUNKEY
100100000000     C                   END
100200000000     C     KEY           IFEQ      F13
100300000000     C                   MOVEL     '13'          FUNKEY
100400000000     C                   END
100500000000     C     KEY           IFEQ      F14
100600000000     C                   MOVEL     '14'          FUNKEY
100700000000     C                   END
100800000000     C     KEY           IFEQ      F15
100900000000     C                   MOVEL     '15'          FUNKEY
101000000000     C                   END
101100000000     C     KEY           IFEQ      F16
101200000000     C                   MOVEL     '16'          FUNKEY
101300000000     C                   END
101400000000     C     KEY           IFEQ      F17
101500000000     C                   MOVEL     '17'          FUNKEY
101600000000     C                   END
101700000000     C     KEY           IFEQ      F18
101800000000     C                   MOVEL     '18'          FUNKEY
101900000000     C                   END
102000000000     C     KEY           IFEQ      F19
102100000000     C                   MOVEL     '19'          FUNKEY
102200000000     C                   END
102300000000     C     KEY           IFEQ      F20
102400000000     C                   MOVEL     '20'          FUNKEY
102500000000     C                   END
102600000000     C     KEY           IFEQ      F21
102700000000     C                   MOVEL     '21'          FUNKEY
102800000000     C                   END
102900000000     C     KEY           IFEQ      F22
103000000000     C                   MOVEL     '22'          FUNKEY
103100000000     C                   END
103200000000     C     KEY           IFEQ      F23
103300000000     C                   MOVEL     '23'          FUNKEY
103400000000     C                   END
103500000000     C     KEY           IFEQ      F24
103600000000     C                   MOVEL     '24'          FUNKEY
103700000000     C                   END
103800000000     C     KEY           IFEQ      ROLLUP
103900000000     C                   MOVEL     'RU'          FUNKEY
104000000000     C                   END
104100000000     C     KEY           IFEQ      HELP
104200000000     C                   MOVEL     'HP'          FUNKEY
104300000000     C                   END
104400000000     C                   ENDSR
104500000000     C*================================================================
104600000000     C* NOTE PER COMPILAZIONE SCHIERE
104700000000     C*================================================================
104800000000     C* -MSG-
104900000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
105000000000     C* (NON TOCCARE MSG,1)
105100000000     C*
105200000000     C* -CMD-
105300000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
105400000000     C*
105500000000     C* -FUD-
105600000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
105700000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
105800000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
105900000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
106000000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
106100000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
106200000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
106300000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
106400000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
106500000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
106600000000     C*
106700000000     C*================================================================
106800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
106900941215TNSDUDR- Tasto funzionale non gestito                                          1
107000941215TNSDUDR- Codice già esistente.                                                 2
107100941215TNSDUDR- Inserimento effettuato.                                               3
107200941215TNSDUDR- Modifica effettuata.                                                  4
107300941215TNSDUDR- Cancellazione effettuata.                                             5
107400941215TNSDUDR- Ripristino effettuato.                                                6
107500941215TNSDUDR- Tipo unità non accettabile.                                           7
107600960927TNSDUDR- Dotare il punto operativo di almeno un tipo unità                     8
107700941215TNSDUDR- 9
107800941215TNSDUDR- 10
107900941215TNSDUDR- 11
108000941215TNSDUDR- 12
108100941215TNSDUDR- 13
108200941215TNSDUDR- 14
108300941215TNSDUDR- Magazzino non accettabile.                                           15
108400941215TNSDUDR- Richiesta gestione di record inesistente.                            16
108500941215TNSDUDR- 17
108600941215TNSDUDR-                                                                      18
108700941215TNSDUDR-                                                                      19
108800941215TNSDUDR-                                                                      20
108900941216** ======== SCHIERA: CMD  (COMANDI)                        ================
109000941216WRKSPLF
109100941216
109200941216
109300941216
109400941216
109500000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
109600930201                   -        (S/N)=N                        funz.01¬
109700000000                   -        (S/N)=N                        funz.02¬
109800000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
109900000000                   -        (S/N)=N                        funz.04¬
110000000000                   -        (S/N)=N                        funz.05¬
110100930202F06=Aggiornamento  -        (S/N)=S                        funz.06¬
110200930201                   -        (S/N)=N                        funz.07¬
110300941215                   -        (S/N)=N                        funz.08¬
110400000000                   -        (S/N)=N                        funz.09¬
110500000000                   -        (S/N)=N                        funz.10¬
110600000000                   -        (S/N)=N                        funz.11¬
110700930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
110800000000                   -        (S/N)=N                        funz.13¬
110900000000                   -        (S/N)=N                        funz.14¬
111000000000                   -        (S/N)=N                        funz.15¬
111100000000                   -        (S/N)=N                        funz.16¬
111200000000                   -        (S/N)=N                        funz.17¬
111300000000                   -        (S/N)=N                        funz.18¬
111400000000                   -        (S/N)=N                        funz.19¬
111500000000                   -        (S/N)=N                        funz.20¬
111600941216F21=St.Dotazioni   -        (S/N)=S                        funz.21¬
111700941216F22=Gestione stampe-        (S/N)=N                        funz.22¬
111800000000                   -        (S/N)=N                        funz.23¬
111900000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
