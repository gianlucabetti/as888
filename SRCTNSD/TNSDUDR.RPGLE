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
007800130410     d tnsdmagds     e ds                  inz
007900020325
008000020709     d ddatiute      e ds
008100020709     d azuteds       e ds                  extname(azute00f)
008200020709     d tibs34ds      e ds                  inz
008300941215     C*------------------------------------------------------------------------*
008400941215     C* MAIN LINE
008500941215     C*------------------------------------------------------------------------*
008600000000     C*
008700930225     C* SE CONTROLLI INIZIALI NON OK, RITORNO A PGM CHIAMANTE
008800930225     C     ERRL00        IFEQ      '1'                                          --- 1 -->
008900930225     C*
009000930225     C* MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
009100930709     C                   MOVEL     '1'           DSUERR
009200930709     C                   MOVEL     DSMSMS        DSUMSG
009300930225     C*
009400930225     C* SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
009500930225     C                   ELSE                                                   <-- 1 -->
009600000000     C*
009700000000     C* CICLO FINO A CHE:
009800000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009900000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
010000930202     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
010100000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
010200000000     C     F12L00        OREQ      '1'
010300930202     C     FLFINE        OREQ      '1'
010400000000     C*
010500000000     C* EMETTE FORMATO VIDEO
010600000000     C                   EXSR      BUTFMR
010700000000     C*
010800000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010900000000     C                   EXSR      FUNCON
011000000000     C*
011100930225     C                   END                                                    <-- 2 ---
011200000000     C*
011300930225     C                   END                                                    <-- 1 ---
011400000000     C*
011500940105     C                   MOVEL     DSUNIT        KPJBU
011600000000     C                   SETON                                        LR
011700000000     C*================================================================
011800000000     C* EMISSIONE FORMATO A VIDEO
011900000000     C*================================================================
012000000000     C     BUTFMR        BEGSR
012100930204     C*
012200930204     C* WRITE RECORD TESTATA
012300941215     C                   WRITE     SDUDD01
012400930205     C*
012500930205     C* VISUALIZZAZIONE MESSAGGIO
012600930205     C                   EXSR      SNDMSG
012700930204     C*
012800930204     C* WRITE RECORD FUNZIONI
012900941215     C                   WRITE     SDUDD02
013000000000     C*
013100930204     C* EXFMT RECORD DETTAGLIO
013200941215     C                   EXFMT     SDUDD03
013300000000     C*
013400000000     C* AZZERO FLAG DI ERRORE LIVELLO
013500000000     C                   MOVEL     '0'           ERRL00
013600000000     C*
013700000000     C* AZZERO FLAG DI ERRORE
013800930709     C                   SETOFF                                       606162
013900000000     C*
014000000000     C                   ENDSR
014100000000     C*================================================================
014200000000     C* SNDMSG - MANDA MESSAGGIO
014300000000     C*================================================================
014400000000     C     SNDMSG        BEGSR
014500000000     C*
014600000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
014700000000     C                   MOVEL     '1'           *IN40
014800941215     C                   WRITE     SDUDDMC
014900000000     C                   MOVEL     '0'           *IN40
015000000000     C*
015100000000     C* VISUALIZZAZIONE MESSAGGIO
015200000000     C                   Z-ADD     0             MSGULT            4 0
015300930202     C                   CALL      'XMSG'        XMSGPA
015400930201     C     DSMSER        IFEQ      0
015500930201     C                   MOVEL     DSMSKE        MSGKEY
015600000000     C                   END
015700000000     C                   Z-ADD     1             MSGULT
015800941215     C                   WRITE     SDUDDMS                                      SFL MESS
015900941215     C                   WRITE     SDUDDMC                                      CONTR MESS
016000000000     C*
016100000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
016200930201     C                   MOVEL     *BLANKS       DSMSMS
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*================================================================
016600000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
016700000000     C*================================================================
016800000000     C     ENTGES        BEGSR
016900930201     C*
017000930201     C* CONTROLLA I CAMPI IMMESSI
017100930201     C                   EXSR      DECODI
017200930202     C*
017300930202     C* SE VISUALIZZAZIONE CON ENTER RITORNA PGM CHIAMANTE
017400930709     C     DSUOP1        IFEQ      'O05'
017500930202     C                   MOVEL     '1'           FLFINE
017600930202     C                   END
017700000000     C*
017800000000     C                   ENDSR
017900000000     C*================================================================
018000930201     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
018100000000     C*================================================================
018200000000     C     F01GES        BEGSR
018300000000     C                   ENDSR
018400000000     C*================================================================
018500930201     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
018600000000     C*================================================================
018700000000     C     F03GES        BEGSR
018800000000     C*
018900930202     C* IMPOSTO FLAG F03 DI LIVELLO PGM
019000000000     C                   MOVEL     '1'           F03L00
019100930202     C* IMPOSTO FLAG F03 DI LIVELLO PROCEDURA
019200930709     C                   MOVEL     '1'           DSUF03
019300000000     C*
019400000000     C                   ENDSR
019500930201     C*================================================================
019600930201     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
019700930201     C*================================================================
019800930201     C     F06GES        BEGSR
019900930201     C*
020000930201     C* LANCIO LA ROUTINE DI CONFERMA INTERESSATA
020100930709     C     DSUOP1        IFEQ      'F06'
020200930201     C                   EXSR      INSERI
020300930201     C                   END
020400930709     C     DSUOP1        IFEQ      'O02'
020500930202     C                   EXSR      MODIFI
020600930201     C                   END
020700930709     C     DSUOP1        IFEQ      'O04'
020800930201     C                   EXSR      CANCEL
020900930201     C                   END
021000930709     C     DSUOP1        IFEQ      'O07'
021100930201     C                   EXSR      RIPRIS
021200930201     C                   END
021300930201     C*
021400930201     C                   ENDSR
021500000000     C*================================================================
021600930201     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
021700000000     C*================================================================
021800000000     C     F07GES        BEGSR
021900000000     C                   ENDSR
022000000000     C*================================================================
022100930709     C* F08GES -GESTIONE TASTO FUNZIONALE F08 - GEST.MOVIMENTI
022200000000     C*================================================================
022300000000     C     F08GES        BEGSR
022400000000     C                   ENDSR
022500000000     C*================================================================
022600930201     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
022700000000     C*================================================================
022800000000     C     F12GES        BEGSR
022900000000     C*
023000000000     C* IMPOSTO FLAG DI LIVELLO
023100000000     C                   MOVEL     '1'           F12L00
023200000000     C*
023300000000     C                   ENDSR
023400930201     C*================================================================
023500930709     C* F21GES -GESTIONE TASTO FUNZIONALE F21 - STAMPA ANAGR.UNITA
023600930201     C*================================================================
023700930201     C     F21GES        BEGSR
023800941216     C*
023900941216     C* LANCIA PROGRAMMA DI STAMPA DELLE UNITA
024000941216     C                   MOVEL     DSUNIT        KPJBU
024100941216     C                   CALL      'TNSDUER'
024200941216     C                   PARM                    KPJBA
024300941216     C                   MOVEL     KPJBU         DSUNIT
024400941216     C*
024500941216     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
024600941216     C     DSUF03        IFEQ      '1'
024700941216     C                   MOVEL     '1'           F03L00
024800941216     C                   MOVEL     '1'           ERRL00
024900941216     C                   ENDIF
025000941216     C*
025100930201     C                   ENDSR
025200000000     C*================================================================
025300930201     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
025400000000     C*================================================================
025500000000     C     F22GES        BEGSR
025600941216     C*
025700941216     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
025800941216     C                   Z-ADD     80            LENGH            15 5
025900941216     C                   CALL      'QCAEXEC'
026000941216     C                   PARM                    CMD(1)
026100941216     C                   PARM                    LENGH
026200941216     C*
026300000000     C                   ENDSR
026400000000     C*================================================================
026500930201     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
026600000000     C*================================================================
026700000000     C     F24GES        BEGSR
026800000000     C*
026900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
027000000000     C                   EXSR      FUNGES
027100000000     C*
027200000000     C                   ENDSR
027300000000     C*================================================================
027400000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
027500000000     C*================================================================
027600000000     C     HELGES        BEGSR
027700000000     C                   ENDSR
027800930201     C*================================================================
027900930201     C* INSERI - FUNZIONE DI INSERIMENTO
028000930201     C*================================================================
028100930201     C     INSERI        BEGSR
028200930201     C*
028300930201     C* CONTROLLA I CAMPI IMMESSI
028400930201     C                   EXSR      DECODI
028500930201     C*
028600930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
028700941215     C*
028800930709     C* IMPOSTO IL RECORD DAL VIDEO
028900930709     C                   EXSR      VIDREC
029000971014     C                   MOVEL     *BLANKS       DOTFTR
029100971014     C                   Z-ADD     *ZEROS        DOTDTR
029200971014     C                   MOVEL     *BLANKS       DOTATB
029300941215     C                   WRITE     UNDOT000
029400941215     C*
029500941220     C                   CLEAR                   SDUDD03
029600930518     C                   MOVEL     MSG(3)        DSMSMS
029700930201     C                   END                                                    <-- 1 ---
029800930201     C*
029900930201     C                   ENDSR
030000930201     C*================================================================
030100930201     C* MODIFI - FUNZIONE DI MODIFICA
030200930201     C*================================================================
030300930201     C     MODIFI        BEGSR
030400930201     C*
030500930201     C* CONTROLLA I CAMPI IMMESSI
030600930201     C                   EXSR      DECODI
030700930201     C*
030800930201     C     ERRL00        IFEQ      '0'                                          --- 1 -->
030900930201     C*
031000930709     C* IMPOSTO IL RECORD DAL VIDEO E RISCRIVO
031100930709     C                   EXSR      VIDREC
031200971014     C                   MOVEL     *BLANKS       DOTFTR
031300971014     C                   MOVEL     *BLANKS       DOTATB
031400941215     C                   UPDATE    UNDOT000
031500941215     C*
031600930518     C                   MOVEL     '1'           FLFINE
031700930709     C                   MOVEL     MSG(4)        DSUMSG
031800930201     C                   END                                                    <-- 1 ---
031900930201     C*
032000930201     C                   ENDSR
032100930201     C*================================================================
032200930201     C* CANCEL - FUNZIONE DI CANCELLAZIONE
032300930201     C*================================================================
032400930201     C     CANCEL        BEGSR
032500930201     C*
032600971014     C* ANNULLO RECORD
032700971014IF  1C     DOTDTR        IFEQ      *ZEROS                                       *MAI TRASMESSO
032800971014     C                   DELETE    UNDOT000
032900971014X   1C                   ELSE                                                   *GIA' TRASMESSO
033000971014     C                   MOVEL     'A'           DOTATB
033100971014     C                   MOVEL     *BLANKS       DOTFTR
033200971014     C                   UPDATE    UNDOT000
033300971014E   1C                   ENDIF
033400930201     C*
033500930201     C                   MOVEL     '1'           FLFINE
033600930709     C                   MOVEL     MSG(5)        DSUMSG
033700930201     C*
033800930201     C                   ENDSR
033900930201     C*================================================================
034000930201     C* RIPRIS - FUNZIONE DI RIPRISTINO
034100930201     C*================================================================
034200930201     C     RIPRIS        BEGSR
034300930202     C*
034400930709     C* RIPRISTINO IL RECORD
034500941215     C                   MOVEL     *BLANKS       DOTFTR
034600941215     C                   MOVEL     *BLANKS       DOTATB
034700941215     C                   UPDATE    UNDOT000
034800930202     C*
034900930202     C                   MOVEL     '1'           FLFINE
035000930709     C                   MOVEL     MSG(6)        DSUMSG
035100930202     C*
035200930201     C                   ENDSR
035300000000     C*================================================================
035400930201     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
035500000000     C*================================================================
035600000000     C     DECODI        BEGSR
035700930709     C*
035800930709     C* CONTROLLO E DECODIFICO IL TIPO UNITA
035900941215     C                   EXSR      CHKTIP
036000941215     C                   MOVEL     WTIP          VIDTIP
036100941215     C                   MOVEL     WDTIP         VIDTID
036200941215     C*
036300941215     C* CONTROLLO E DECODIFICO IL MAGAZZINO
036400941219     C     ERRL00        IFEQ      '0'
036500941215     C                   EXSR      CHKMAG
036600941216     C                   MOVEL     WFIL          VIDFIL
036700941216     C                   MOVEL     WDFIL         VIDFID
036800941219     C                   ENDIF
036900941215     C***
037000941215     C* SE INSERIMENTO CONTROLLA ESISTENZA RECORD
037100941215     C***
037200930709     C     ERRL00        IFEQ      '0'
037300930709     C     DSUOP1        IFEQ      'F06'
037400941215     C                   Z-ADD     WTIP          KDOTIP
037500941215     C                   Z-ADD     WFIL          KDOFIL
037600941215     C     KEYDOT        CHAIN     UNDOT01L                           99
037700941215     C     *IN99         IFEQ      *OFF
037800930518     C                   MOVEL     '1'           ERRL00
037900930518     C                   MOVEL     MSG(2)        DSMSMS
038000930709     C                   SETON                                        6061
038100930518     C                   END
038200930709     C                   END
038300930518     C                   END
038400941220     C*
038500941215     C* QUANTITA' IN DOTAZIONE NO UGUALE A ZERO
038600941220     C*----------ERRL00    IFEQ '0'
038700941220     C*----------VIDQTA    IFEQ *ZEROS
038800941220     C*----------          MOVEL'1'       ERRL00
038900941220     C*----------          MOVELMSG,8     DSMSMS
039000941220     C*----------          SETON                     62
039100941220     C*----------          ENDIF
039200941220     C*----------          ENDIF
039300000000     C*
039400000000     C                   ENDSR
039500930709     C*================================================================
039600930709     C*  CHKTIP - CONTROLLA IL TIPO DI UNITA
039700930709     C*================================================================
039800930709     C     CHKTIP        BEGSR
039900941219     C*
040000941219     C                   Z-ADD     *ZEROS        WTIP
040100941219     C                   MOVEL     *BLANKS       WDTIP
040200930709     C*
040300930709     C* CONTROLLO SE E' STATO INSERITO '?'
040400941215     C                   SETOFF                                           28
040500941215     C     '?'           SCAN      VIDTIP                                 28
040600930709     C*
040700930709     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
040800020325     c                   if        *in28
040900020325     c                   clear                   tibs02ds
041000020325     c                   movel     'R'           t02mod
041100020325     c                   movel     knsif         t02sif
041200020325     c                   movel     'UNT'         t02cod
041300020325     c                   call      'TIBS02R'
041400020325     c                   parm                    kpjba
041500020325     c                   parm                    tibs02ds
041600020325     c                   movel     t02ke1        wtip              2 0
041700020325     c                   movel     t02uni        wdtip            25
041800020325     C                   MOVEL     '1'           ERRL00
041900020325     c                   endif                                                  <-- 1 ---
042000020325
042100941215     C* NO CARATTERI
042200020325IF  1C     VIDTIP        IFLE      *ZEROS
042300941215     C                   MOVEL     *ALL'*'       WDTIP
042400941215     C                   MOVEL     '1'           ERRL00
042500941215     C                   MOVEL     MSG(7)        DSMSMS
042600941215     C                   SETON                                        60
042700020325X   1C                   ELSE
042800930709     C*
042900930709     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
043000941215     C                   Z-ADD     1             J
043100941215     C                   SETOFF                                           28
043200941215     C                   MOVEL     VIDTIP        WTIP
043300941215     C     WTIP          LOOKUP    UNI(J)                                 28
043400020325IF  2C     *IN28         IFEQ      *OFF
043500941215     C                   MOVEL     *ALL'*'       WDTIP
043600941215     C                   MOVEL     '1'           ERRL00
043700941215     C                   MOVEL     MSG(7)        DSMSMS
043800941215     C                   SETON                                        60
043900941215     C                   ELSE
044000941215     C                   MOVEL     DUNI(J)       WDTIP
044100020325E   2C                   ENDIF
044200020325E   1C                   ENDIF
044300930709     C*
044400930709     C                   ENDSR
044500930709     C*================================================================
044600930709     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
044700930709     C*================================================================
044800930709     C     CHKMAG        BEGSR
044900130410     C                   Z-ADD     *ZEROS        WFIL              3 0
045000130410     C                   MOVEL     *BLANKS       WDFIL            20
045100130410     c                   clear                   tnsdmagds
045200130410     c
045300130410     c                   eval      §magmag=vidfil
045400130410     c                   eval      §imagtorg='E  '
045500130410     c                   call      'TNSDMAGR'
045600130410     c                   parm                    kpjba
045700130410     c                   parm                    tnsdmagds
045800130410     c
045900130410     c* Eseguita Ricerca
046000130410    1c                   if        §omagerr='R'
046100130410     c                   eval      wfil=%int(§magmag)
046200130410     c                   eval      wdfil=§omagdes
046300130410     C                   MOVEL     '1'           ERRL00
046400130410   x1c                   else
046500130410     c* Errore nel magazzino
046600130410    2c                   if        §omagerr='E'
046700130410     C                   MOVEL     '1'           ERRL00
046800130410     C                   MOVEL     MSG(15)       DSMSMS
046900130410     C                   SETON                                        61
047000130410     c                   eval      wdfil=*all'?'
047100130410     c                   eval      wfil=%int(§magmag)
047200130410     c* se tutto ok prendo decodifica e flag
047300130410   x2c                   else
047400130410     c                   eval      wdfil=§omagdes
047500130410     c                   eval      wfil=%int(§magmag)
047600130410    2c                   endif
047700130410    1c                   endif
047800941219     C*
047900941215     C*
048000941215     C* CONTROLLO SE E' STATO INSERITO '?'
048100130410     C**                 SETOFF                                           28
048200130410     C*+   '?'           SCAN      VIDFIL                                 28
048300941215     C*
048400941215     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE FILIALI
048500130410IF  1C**   *IN28         IFEQ      *ON
048600130410     C**                 MOVEL     *BLANKS       §COD1             3
048700130410     C**                 MOVEL     *BLANKS       §DES1
048800130410     C**                 CALL      'TNSD20R'
048900130410     C**                 PARM                    §COD1
049000130410     C**                 PARM                    §TIP              1
049100130410     C**                 PARM                    §DES1            25
049200130410     C**                 MOVEL     §COD1         WFIL              3 0
049300130410     C**                 MOVEL     §DES1         WDFIL            20
049400130410     C**                 MOVEL     '1'           ERRL00
049500941215     C*
049600130410X   1C**                 ELSE
049700941215     C* NO CARATTERI
049800130410IF  2C**   VIDFIL        IFLE      *ZEROS
049900130410     C**                 MOVEL     *ALL'*'       WDFIL
050000130410     C**                 MOVEL     '1'           ERRL00
050100130410     C**                 MOVEL     MSG(15)       DSMSMS
050200130410     C**                 SETON                                        61
050300130410X   2C**                 ELSE
050400941215     C*
050500941215     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
050600130410     C**                 Z-ADD     1             J
050700130410     C**                 SETOFF                                           28
050800130410     C**                 MOVEL     VIDFIL        WFIL
050900130410     C**   WFIL          LOOKUP    FIL(J)                                 28
051000130410IF  3C**   *IN28         IFEQ      *OFF
051100130410     C**                 MOVEL     *ALL'*'       WDFIL
051200130410     C**                 MOVEL     '1'           ERRL00
051300130410     C**                 MOVEL     MSG(15)       DSMSMS
051400130410     C**                 SETON                                        61
051500130410     C**                 ELSE
051600130410     C**                 MOVEL     DFIL(J)       WDFIL
051700130410E   3C**                 ENDIF
051800130410E   2C**                 ENDIF
051900130410E   1C**                 ENDIF
052000941215     C*
052100930709     C                   ENDSR
052200930201     C*================================================================
052300930709     C*  RECVID - PORTA I CAMPI DAL RECORD AL VIDEO
052400930201     C*================================================================
052500930709     C     RECVID        BEGSR
052600930201     C*
052700941215     C                   MOVEL     DOTTIP        VIDTIP
052800941215     C                   MOVEL     DOTFIL        VIDFIL
052900941215     C                   Z-ADD     DOTQTA        VIDQTA
053000941215     C*
053100941215     C                   MOVEL     *BLANKS       VIDTID
053200941215     C                   MOVEL     *BLANKS       VIDFID
053300930202     C*
053400941215     C     DOTATB        IFEQ      'A'
053500930202     C                   MOVEL     'ANNULLAT'    VIDATB
053600930202     C                   MOVE      'O'           VIDATB
053700930202     C                   ELSE
053800930202     C                   MOVEL     *BLANKS       VIDATB
053900930202     C                   END
054000930201     C*
054100930201     C                   ENDSR
054200941215     C*================================================================
054300930201     C*  AZZVID - AZZERA O ABBLENCA TUTTI I CAMPI DEL FILE VIDEO
054400930201     C*================================================================
054500930201     C     AZZVID        BEGSR
054600941215     C*
054700941215     C                   MOVEL     *ZEROS        VIDTIP
054800941215     C                   MOVEL     *ZEROS        VIDFIL
054900941215     C                   Z-ADD     *ZEROS        VIDQTA
055000941215     C*
055100941215     C                   MOVEL     *BLANKS       VIDTID
055200941215     C                   MOVEL     *BLANKS       VIDFID
055300941215     C                   MOVEL     *BLANKS       VIDATB
055400930201     C*
055500930201     C                   ENDSR
055600930201     C*================================================================
055700930709     C*  VIDREC - IMPOSTA CAMPI DA VIDEO A RECORD
055800930201     C*================================================================
055900930709     C     VIDREC        BEGSR
056000941215     C*
056100941215     C                   MOVEL     VIDTIP        DOTTIP
056200941215     C                   MOVEL     VIDFIL        DOTFIL
056300941215     C                   Z-ADD     VIDQTA        DOTQTA
056400930201     C*
056500930201     C                   ENDSR
056600000000     C*================================================================
056700941215     C* *INZSR - ROUTINE INIZIALE
056800000000     C*================================================================
056900941215     C     *INZSR        BEGSR
057000930201     C*
057100930201     C* RICEVIMENTO PARAMETRI
057200930201     C     *ENTRY        PLIST
057300940105     C                   PARM                    KPJBA
057400940105     C                   MOVEL     KPJBU         DSUNIT
057500930201     C*
057600930201     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
057700930201     C     XMSGPA        PLIST
057800930201     C                   PARM                    DSMSMS
057900930201     C                   PARM                    DSMSPG
058000930201     C                   PARM                    DSMSKE
058100930201     C                   PARM                    DSMSER
058200941215     C* DEFINIZIONE CAMPI
058300941215     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
058400941215     C     *LIKE         DEFINE    DOTTIP        KDOTIP                         *UNDOT00F
058500941215     C     *LIKE         DEFINE    DOTFIL        KDOFIL
058600941215     C*
058700941215     C* CHIAVE LETTURA UNDOT01L - PARZIALE
058800941216     C     KEYDOT        KLIST
058900941215     C                   KFLD                    KDOTIP
059000941215     C                   KFLD                    KDOFIL
059100941215     C*
059200020528     C* CHIAVE LETTURA AZORG21l/azorg01l - COMPLETA
059300941215     C     KEYORG        KLIST
059400941215     C                   KFLD                    KORFIL
059500930201     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
059600930201     C                   CALL      'XKEY'
059700930201     C                   PARM                    DSKEY
059800930201     C*
059900930201     C* DEFINISCE LE VARIABILI DI PROGRAMMA
060000930201     C     *LIKE         DEFINE    FUDFUN        FUNKEY
060100930201     C*
060200930201     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
060300930201     C*   -NOME PGM CHIAMANTE
060400930201     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
060500941215     C                   MOVEL     'TNSDUDR'     DSMSPG
060600941215     C                   MOVEL     'TNSDUDR'     MSGQUE
060700930201     C                   MOVEL     *BLANKS       MSGKEY
060800930201     C                   MOVEL     *BLANKS       DSMSMS
060900930202     C*
061000930202     C* SE VISUALIZZAZIONE TOLGO FUNZIONE AGGIORNAMENTO
061100930709     C     DSUOP1        IFEQ      'O05'
061200930202     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
061300930202     C                   MOVEL     'N'           FUDATT
061400930202     C                   MOVEL     FUDRIG        FUD(6)
061500930202     C                   END
061600940105     C*
061700940105     C* SE PGM E' STATO CHIAMATO DA UN PGM DI GESTIONE DEI MOVIMENTI UNITA'
061800940105     C* NON CONSENTO DI RICHIAMARE UN PGM DI QUELLA GESTIONE
061900940105     C     DSUPRO        IFEQ      'M'
062000940105     C                   MOVEL     FUD(8)        FUDRIG                         *GEST.MOVIMENTI
062100940105     C                   MOVEL     'N'           FUDATT
062200940105     C                   MOVEL     FUDRIG        FUD(8)
062300940105     C                   END
062400930201     C*
062500930201     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
062600930201     C                   Z-ADD     0             FUNGRU            5 0
062700930201     C*
062800930201     C* GESTIONE TASTI FUNZIONALI A VIDEO
062900930201     C                   EXSR      FUNGES
063000930201     C*
063100930201     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
063200930201     C                   MOVEL     '0'           F12L00            1
063300930201     C                   MOVEL     '0'           F03L00            1
063400930201     C                   MOVEL     '0'           FINL00            1
063500930201     C                   MOVEL     '0'           ERRL00            1
063600930201     C                   MOVEL     '0'           FLFINE            1
063700930204     C*
063800930204     C* LANCIO ROUTINE PER VERIFICARE SE
063900930204     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
064000930204     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
064100020709
064200020709     c     *dtaara       define    §azute        azuteds
064300020709     c     *dtaara       define    §datiute      ddatiute
064400020709
064500020709     c                   in(E)     *dtaara
064600020709     c                   if        %Error or rsut = *blanks
064700020709     c                   clear                   tibs34ds
064800020709     c                   call      'TIBS34R'
064900020709     c                   parm                    Tibs34Ds
065000020709     c                   in        *dtaara
065100020709     c                   endif
065200941215     C* CARICA ORGANIGRAMMA
065300941215     C                   EXSR      CARORG
065400941215     C* CARICA TABELLE
065500941215     C                   EXSR      CARTAB
065600930201     C*
065700930201     C* IMPOSTO GLI INDICATORI PER LA GESTIONE DELLA PROTEZIONE CAMPI
065800930201     C* 0=NON PROTETTO, 1=PROTETTO
065900930709     C*   91 - CAMPI CHIAVE
066000930709     C*   90 - DATI FILE
066100930201     C* --> UTENTE ENTRATO IN GESTIONE
066200940105     C     DSUOP0        IFEQ      ' 2'                                         --- 1 -->
066300930201     C*
066400930709     C     DSUOP1        IFEQ      'F06'                                        *INS.
066500941215     C                   SETOFF                                       9091
066600930201     C                   END
066700930201     C*
066800930709     C     DSUOP1        IFEQ      'O02'                                        *MOD.
066900930709     C                   SETON                                        91
067000020709     c                   If        DutLpo = 'S'
067100930709     C                   SETOFF                                       90
067200930201     C                   ELSE                                                   *FIL.
067300930709     C                   SETON                                        90
067400930201     C                   END
067500930201     C                   END
067600930201     C*
067700930709     C     DSUOP1        IFEQ      'O05'                                        *VIS+CANC+RIPR
067800930709     C     DSUOP1        OREQ      'O04'
067900930709     C     DSUOP1        OREQ      'O07'
068000941215     C                   SETON                                        9091
068100930201     C                   END
068200930201     C*
068300930201     C                   END                                                    <-- 1 ---
068400930201     C* --> UTENTE ENTRATO IN INTERROGAZIONE
068500940105     C     DSUOP0        IFEQ      ' 5'
068600941215     C                   SETON                                        9091
068700930201     C                   END
068800930201     C*
068900930709     C* SE SONO IN MOD. O VIS. O CANC. O RIPR. LEGGO RECORD
069000930709     C     DSUOP1        IFEQ      'O02'                                        --- 1 -->
069100930709     C     DSUOP1        OREQ      'O04'
069200930709     C     DSUOP1        OREQ      'O05'
069300930709     C     DSUOP1        OREQ      'O07'
069400930709     C* SE IL REC.NON ESISTE RIMANDO ERRORE AL PGM CHIAMANTE
069500941215     C                   Z-ADD     DSUTIP        KDOTIP
069600941215     C                   Z-ADD     DSUMAG        KDOFIL
069700941215     C     KEYDOT        CHAIN     UNDOT01L                           99
069800930201     C     *IN99         IFEQ      '1'
069900930201     C                   MOVEL     '1'           ERRL00
070000930709     C                   MOVEL     MSG(16)       DSMSMS
070100930202     C                   ELSE
070200930709     C                   EXSR      RECVID
070300930201     C                   END
070400930208     C*
070500930208     C* SE TUTTO OK, DECODIFICO I CAMPI A VIDEO
070600020325     c                   if        errl00 = *off
070700930208     C                   EXSR      DECODI
070800020325     c                   endif
070900930201     C*
071000930202     C                   END                                                    <-- 1 ---
071100930202     C*
071200930202     C* SE INSERIMENTO AZZERO TUTTI I CAMPI DEL VIDEO
071300930709     C     DSUOP1        IFEQ      'F06'
071400930202     C                   EXSR      AZZVID
071500930202     C                   END
071600930201     C*
071700930201     C* IMPOSTO DESCRIZIONE DELLA FUNZIONE IN CORSO A VIDEO
071800930709     C     DSUOP1        IFEQ      'F06'
071900930201     C                   MOVEL     ' INSERIM'    VIDFUN
072000930201     C                   MOVE      'ENTO   '     VIDFUN
072100930201     C                   END
072200930709     C     DSUOP1        IFEQ      'O02'
072300930201     C                   MOVEL     '   MODIF'    VIDFUN
072400930201     C                   MOVE      'ICA    '     VIDFUN
072500930201     C                   END
072600930709     C     DSUOP1        IFEQ      'O04'
072700930201     C                   MOVEL     ' CANCELL'    VIDFUN
072800930201     C                   MOVE      'AZIONE '     VIDFUN
072900930201     C                   END
073000930709     C     DSUOP1        IFEQ      'O05'
073100930201     C                   MOVEL     'VISUALIZ'    VIDFUN
073200930201     C                   MOVE      'ZAZIONE'     VIDFUN
073300930201     C                   END
073400930709     C     DSUOP1        IFEQ      'O07'
073500930201     C                   MOVEL     ' RIPRIST'    VIDFUN
073600930201     C                   MOVE      'INO    '     VIDFUN
073700930201     C                   END
073800000000     C*
073900000000     C                   ENDSR
074000941215     C*================================================================
074100941215     C* CARORG - CARICA ORGANIGRAMMA
074200941215     C*================================================================
074300941215     C     CARORG        BEGSR
074400941215     C*
074500941215     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
074600941215     C                   Z-ADD     *ZEROS        KORFIL
074700020528     C     KEYORG        SETLL     AZORG21l                           99        *FINE FILE
074800020528     C  N99              READ      AZORG21l                               99
074900941215     C*
075000941215     C     *IN99         DOWEQ     *OFF
075100941215     C                   ADD       1             J
075200941215     C                   Z-ADD     ORGFIL        FIL(J)
075300941215     C                   MOVEL     ORGDES        DFIL(J)
075400941215     C*
075500020528     C                   READ      AZORG21l                               99
075600941215     C                   ENDDO
075700020528
075800020528     c     keyorg        setll     azorg01l
075900020528     c                   do        *hival
076000020528     c                   read      azorg01l
076100020528     c                   if        %eof(azorg01l)
076200020528     c                   leave
076300020528     c                   endif
076400020528     c                   z-add     1             j
076500020528     c     orgfil        lookup    fil(j)                                 99
076600020528     c                   if        not *in99
076700020528     c                   z-add     1             j
076800020528     c     *zeros        lookup    fil(j)                                 99
076900020528     c                   if        *in99
077000020528     c                   movel     orgfil        fil(j)
077100020528     c                   movel     orgdes        dfil(j)
077200020528     c                   endif
077300020528     c                   endif
077400020528     c                   enddo
077500941215     C*
077600941215     C                   ENDSR
077700941215     C*================================================================
077800941215     C* CARTAB - CARICA TABELLE
077900941215     C*================================================================
078000941215     C     CARTAB        BEGSR
078100941215     C*--------------------
078200941215     C* TIPI UNITA'
078300941215     C*--------------------
078400941215     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
078500020325     C                   MOVEL     'UNT'         tbecod
078600020325     C     tbecod        SETLL     tntbe01l                           99        *FINE FILE
078700020325     C  N99tbecod        READE     tntbe01l                               99
078800941215     C*
078900941215     C     *IN99         DOWEQ     *OFF
079000020325     C     TBeatb        IFEQ      *BLANKS
079100941215     C                   ADD       1             J
079200020325     C                   MOVEL     TBeke1        UNI(J)
079300020325     C                   MOVEL     tbeuni        DUNI(J)
079400941215     C                   ENDIF
079500941215     C*
079600020325     C  N99tbecod        READE     tntbe01l                               99
079700941215     C                   ENDDO
079800941215     C*
079900941215     C                   ENDSR
080000000000     C*================================================================
080100000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
080200000000     C*================================================================
080300000000     C     FUNGES        BEGSR
080400000000     C*
080500000000     C* PULISCO CAMPI DI LAVORO
080600000000     C                   Z-ADD     0             W                 5 0
080700000000     C*
080800000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
080900000000     C* E CARICO TABELLA FUNZIONI ATTIVE
081000000000     C     1             DO        24            I
081100000000     C                   MOVEL     FUD(I)        FUDRIG
081200000000     C*
081300000000     C* PULIZIA FUNZIONI PRECEDENTI
081400000000     C                   MOVEL     *BLANKS       FUV(I)
081500000000     C*
081600000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
081700000000     C* (SALTA F24)
081800000000     C     FUDATT        IFEQ      'S'
081900000000     C     FUDFUN        ANDNE     '24'
082000000000     C                   ADD       1             W
082100000000     C                   MOVEL     FUDDES        FUV(W)
082200000000     C                   END
082300000000     C                   END                                                    DO WHILE
082400000000     C*
082500000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
082600000000     C* ALTRIMENTI DISATTIVO F24
082700000000     C     1             DO        24            I
082800000000     C                   MOVEL     FUD(I)        FUDRIG
082900000000     C*
083000000000     C     FUDFUN        IFEQ      '24'
083100000000     C     W             IFGT      6
083200000000     C                   MOVEL     'S'           FUDATT
083300000000     C                   ELSE
083400000000     C                   MOVEL     *BLANKS       FUDATT
083500000000     C                   END                                                    W GT 8
083600000000     C                   MOVEL     FUDRIG        FUD(I)
083700000000     C                   END
083800000000     C                   END                                                    DO WHILE
083900000000     C*
084000000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
084100000000     C     W             DIV       6             I
084200000000     C                   MVR                     Z
084300000000     C*
084400000000     C     Z             IFNE      0
084500000000     C     I             ADD       1             Z
084600000000     C                   ELSE
084700000000     C                   Z-ADD     I             Z                 5 0
084800000000     C                   END
084900000000     C*
085000000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
085100000000     C                   ADD       1             FUNGRU
085200000000     C     FUNGRU        IFGT      Z
085300000000     C     FUNGRU        ORLT      1
085400000000     C                   Z-ADD     1             FUNGRU
085500000000     C                   END
085600000000     C*
085700000000     C* PRIMO GRUPPO DI 6 FUNZIONI
085800000000     C     FUNGRU        IFEQ      1
085900000000     C                   MOVEL     FUV(1)        FUN001
086000000000     C                   MOVEL     FUV(2)        FUN002
086100000000     C                   MOVEL     FUV(3)        FUN003
086200000000     C                   MOVEL     FUV(4)        FUN004
086300000000     C                   MOVEL     FUV(5)        FUN005
086400000000     C                   MOVEL     FUV(6)        FUN006
086500000000     C                   END
086600000000     C*
086700000000     C* SECONDO GRUPPO DI 6 FUNZIONI
086800000000     C     FUNGRU        IFEQ      2
086900000000     C                   MOVEL     FUV(7)        FUN001
087000000000     C                   MOVEL     FUV(8)        FUN002
087100000000     C                   MOVEL     FUV(9)        FUN003
087200000000     C                   MOVEL     FUV(10)       FUN004
087300000000     C                   MOVEL     FUV(11)       FUN005
087400000000     C                   MOVEL     FUV(12)       FUN006
087500000000     C                   END
087600000000     C*
087700000000     C* TERZO GRUPPO DI 6 FUNZIONI
087800000000     C     FUNGRU        IFEQ      3
087900000000     C                   MOVEL     FUV(13)       FUN001
088000000000     C                   MOVEL     FUV(14)       FUN002
088100000000     C                   MOVEL     FUV(15)       FUN003
088200000000     C                   MOVEL     FUV(16)       FUN004
088300000000     C                   MOVEL     FUV(17)       FUN005
088400000000     C                   MOVEL     FUV(18)       FUN006
088500000000     C                   END
088600000000     C*
088700000000     C* QUARTO GRUPPO DI 6 FUNZIONI
088800000000     C     FUNGRU        IFEQ      4
088900000000     C                   MOVEL     FUV(19)       FUN001
089000000000     C                   MOVEL     FUV(20)       FUN002
089100000000     C                   MOVEL     FUV(21)       FUN003
089200000000     C                   MOVEL     FUV(22)       FUN004
089300000000     C                   MOVEL     *BLANKS       FUN005
089400000000     C                   MOVEL     *BLANKS       FUN006
089500000000     C                   END
089600000000     C*
089700000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
089800000000     C                   MOVEL     *BLANKS       FUN007
089900000000     C                   MOVEL     *BLANKS       FUN008
090000000000     C*
090100000000     C     1             DO        24            I
090200000000     C                   MOVEL     FUD(I)        FUDRIG
090300000000     C     FUDFUN        IFEQ      '24'
090400000000     C     FUDATT        ANDEQ     'S'
090500000000     C                   MOVEL     FUDDES        FUN008
090600000000     C                   END
090700000000     C                   END                                                    DO
090800000000     C                   ENDSR
090900000000     C*================================================================
091000000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
091100000000     C*================================================================
091200000000     C     FUNCON        BEGSR
091300000000     C*
091400000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
091500000000     C                   EXSR      FUNVAR
091600000000     C*
091700000000     C* PULISCO CAMPI DI LAVORO
091800000000     C                   Z-ADD     0             I                 5 0
091900000000     C                   Z-ADD     0             W                 5 0
092000000000     C*
092100000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
092200000000     C     FUNKEY        IFNE      *BLANKS
092300000000     C     1             DO        24            I
092400000000     C                   MOVEL     FUD(I)        FUDRIG
092500000000     C*
092600000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
092700000000     C     FUNKEY        IFEQ      FUDFUN
092800000000     C     FUDATT        ANDEQ     'S'
092900000000     C                   Z-ADD     I             W
093000000000     C                   END
093100000000     C                   END                                                    DO WHILE
093200000000     C*
093300000000     C* FUNZIONE NON ATTIVA (ERRORE)
093400000000     C     W             IFEQ      0
093500000000     C*
093600000000     C* EMISSIONE MESSAGGIO DI ERRORE
093700930201     C                   MOVEL     MSG(1)        DSMSMS
093800930201     C*
093900930201     C                   END                                                    W=0
094000000000     C                   END                                                    FUNKEY NE *BLANKS
094100000000     C*
094200000000     C* FUNZIONE ATTIVA (PROCEDI ...)
094300000000     C     W             IFNE      0
094400000000     C     FUNKEY        OREQ      *BLANKS
094500000000     C*
094600000000     C     FUNKEY        IFEQ      *BLANKS
094700000000     C                   EXSR      ENTGES
094800000000     C                   END
094900000000     C     FUNKEY        IFEQ      '01'
095000000000     C                   EXSR      F01GES
095100000000     C                   END
095200000000     C     FUNKEY        IFEQ      '03'
095300000000     C                   EXSR      F03GES
095400000000     C                   END
095500930202     C     FUNKEY        IFEQ      '06'
095600930202     C                   EXSR      F06GES
095700930202     C                   END
095800000000     C     FUNKEY        IFEQ      '07'
095900000000     C                   EXSR      F07GES
096000000000     C                   END
096100000000     C     FUNKEY        IFEQ      '08'
096200000000     C                   EXSR      F08GES
096300000000     C                   END
096400000000     C     FUNKEY        IFEQ      '12'
096500000000     C                   EXSR      F12GES
096600000000     C                   END
096700930201     C     FUNKEY        IFEQ      '21'
096800930201     C                   EXSR      F21GES
096900930201     C                   END
097000000000     C     FUNKEY        IFEQ      '22'
097100000000     C                   EXSR      F22GES
097200000000     C                   END
097300000000     C     FUNKEY        IFEQ      '24'
097400000000     C                   EXSR      F24GES
097500000000     C                   END
097600000000     C     FUNKEY        IFEQ      'HP'
097700000000     C                   EXSR      HELGES
097800000000     C                   END
097900000000     C*
098000000000     C                   END
098100000000     C*
098200000000     C                   ENDSR
098300000000     C*================================================================
098400000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
098500000000     C*================================================================
098600000000     C     FUNVAR        BEGSR
098700000000     C*
098800000000     C* INIZIALIZZA VARIABILE -FUNKEY-
098900000000     C*
099000000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
099100000000     C     KEY           IFEQ      ENTER
099200000000     C                   MOVEL     *BLANKS       FUNKEY
099300000000     C                   END
099400000000     C     KEY           IFEQ      F1
099500000000     C                   MOVEL     '01'          FUNKEY
099600000000     C                   END
099700000000     C     KEY           IFEQ      F2
099800000000     C                   MOVEL     '02'          FUNKEY
099900000000     C                   END
100000000000     C     KEY           IFEQ      F3
100100000000     C                   MOVEL     '03'          FUNKEY
100200000000     C                   END
100300000000     C     KEY           IFEQ      F4
100400000000     C                   MOVEL     '04'          FUNKEY
100500000000     C                   END
100600000000     C     KEY           IFEQ      F5
100700000000     C                   MOVEL     '05'          FUNKEY
100800000000     C                   END
100900000000     C     KEY           IFEQ      F6
101000000000     C                   MOVEL     '06'          FUNKEY
101100000000     C                   END
101200000000     C     KEY           IFEQ      F7
101300000000     C                   MOVEL     '07'          FUNKEY
101400000000     C                   END
101500000000     C     KEY           IFEQ      F8
101600000000     C                   MOVEL     '08'          FUNKEY
101700000000     C                   END
101800000000     C     KEY           IFEQ      F9
101900000000     C                   MOVEL     '09'          FUNKEY
102000000000     C                   END
102100000000     C     KEY           IFEQ      F10
102200000000     C                   MOVEL     '10'          FUNKEY
102300000000     C                   END
102400000000     C     KEY           IFEQ      F11
102500000000     C                   MOVEL     '11'          FUNKEY
102600000000     C                   END
102700000000     C     KEY           IFEQ      F12
102800000000     C                   MOVEL     '12'          FUNKEY
102900000000     C                   END
103000000000     C     KEY           IFEQ      F13
103100000000     C                   MOVEL     '13'          FUNKEY
103200000000     C                   END
103300000000     C     KEY           IFEQ      F14
103400000000     C                   MOVEL     '14'          FUNKEY
103500000000     C                   END
103600000000     C     KEY           IFEQ      F15
103700000000     C                   MOVEL     '15'          FUNKEY
103800000000     C                   END
103900000000     C     KEY           IFEQ      F16
104000000000     C                   MOVEL     '16'          FUNKEY
104100000000     C                   END
104200000000     C     KEY           IFEQ      F17
104300000000     C                   MOVEL     '17'          FUNKEY
104400000000     C                   END
104500000000     C     KEY           IFEQ      F18
104600000000     C                   MOVEL     '18'          FUNKEY
104700000000     C                   END
104800000000     C     KEY           IFEQ      F19
104900000000     C                   MOVEL     '19'          FUNKEY
105000000000     C                   END
105100000000     C     KEY           IFEQ      F20
105200000000     C                   MOVEL     '20'          FUNKEY
105300000000     C                   END
105400000000     C     KEY           IFEQ      F21
105500000000     C                   MOVEL     '21'          FUNKEY
105600000000     C                   END
105700000000     C     KEY           IFEQ      F22
105800000000     C                   MOVEL     '22'          FUNKEY
105900000000     C                   END
106000000000     C     KEY           IFEQ      F23
106100000000     C                   MOVEL     '23'          FUNKEY
106200000000     C                   END
106300000000     C     KEY           IFEQ      F24
106400000000     C                   MOVEL     '24'          FUNKEY
106500000000     C                   END
106600000000     C     KEY           IFEQ      ROLLUP
106700000000     C                   MOVEL     'RU'          FUNKEY
106800000000     C                   END
106900000000     C     KEY           IFEQ      HELP
107000000000     C                   MOVEL     'HP'          FUNKEY
107100000000     C                   END
107200000000     C                   ENDSR
107300000000     C*================================================================
107400000000     C* NOTE PER COMPILAZIONE SCHIERE
107500000000     C*================================================================
107600000000     C* -MSG-
107700000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
107800000000     C* (NON TOCCARE MSG,1)
107900000000     C*
108000000000     C* -CMD-
108100000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
108200000000     C*
108300000000     C* -FUD-
108400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
108500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
108600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
108700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
108800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
108900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
109000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
109100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
109200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
109300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
109400000000     C*
109500000000     C*================================================================
109600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
109700941215TNSDUDR- Tasto funzionale non gestito                                          1
109800941215TNSDUDR- Codice già esistente.                                                 2
109900941215TNSDUDR- Inserimento effettuato.                                               3
110000941215TNSDUDR- Modifica effettuata.                                                  4
110100941215TNSDUDR- Cancellazione effettuata.                                             5
110200941215TNSDUDR- Ripristino effettuato.                                                6
110300941215TNSDUDR- Tipo unità non accettabile.                                           7
110400960927TNSDUDR- Dotare il punto operativo di almeno un tipo unità                     8
110500941215TNSDUDR- 9
110600941215TNSDUDR- 10
110700941215TNSDUDR- 11
110800941215TNSDUDR- 12
110900941215TNSDUDR- 13
111000941215TNSDUDR- 14
111100941215TNSDUDR- Magazzino non accettabile.                                           15
111200941215TNSDUDR- Richiesta gestione di record inesistente.                            16
111300941215TNSDUDR- 17
111400941215TNSDUDR-                                                                      18
111500941215TNSDUDR-                                                                      19
111600941215TNSDUDR-                                                                      20
111700941216** ======== SCHIERA: CMD  (COMANDI)                        ================
111800941216WRKSPLF
111900941216
112000941216
112100941216
112200941216
112300000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
112400930201                   -        (S/N)=N                        funz.01¬
112500000000                   -        (S/N)=N                        funz.02¬
112600000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
112700000000                   -        (S/N)=N                        funz.04¬
112800000000                   -        (S/N)=N                        funz.05¬
112900930202F06=Aggiornamento  -        (S/N)=S                        funz.06¬
113000930201                   -        (S/N)=N                        funz.07¬
113100941215                   -        (S/N)=N                        funz.08¬
113200000000                   -        (S/N)=N                        funz.09¬
113300000000                   -        (S/N)=N                        funz.10¬
113400000000                   -        (S/N)=N                        funz.11¬
113500930202F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
113600000000                   -        (S/N)=N                        funz.13¬
113700000000                   -        (S/N)=N                        funz.14¬
113800000000                   -        (S/N)=N                        funz.15¬
113900000000                   -        (S/N)=N                        funz.16¬
114000000000                   -        (S/N)=N                        funz.17¬
114100000000                   -        (S/N)=N                        funz.18¬
114200000000                   -        (S/N)=N                        funz.19¬
114300000000                   -        (S/N)=N                        funz.20¬
114400941216F21=St.Dotazioni   -        (S/N)=S                        funz.21¬
114500941216F22=Gestione stampe-        (S/N)=N                        funz.22¬
114600000000                   -        (S/N)=N                        funz.23¬
114700000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
