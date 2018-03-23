000100970526     H*------------------------------------------------------------------------*
000200001116     H* Gestione codici clienti strategi
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800010228     FtiS735d   cf   e             WORKSTN INFDS(DEVDS1)
000900010228     F                                     SFILE(S735DMS:MSGULT)
001000970527     F*------------
001100010228     Ftivtd01L  UF A E           K DISK
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500970526     D*------------
001600970526     D* DECODIFICHE
001700970526     D*------------
001800991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
001900000215     D*------------
002000000215     D* DESCRIZIONI
002100000215     D*------------
002200000215     D DES             S             66    DIM(5) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* MESSAGGI
002500970526     D*------------
002600000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* DESCRIZIONI TASTI FUNZIONALI
002900970526     D*------------
003000970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* FUNZIONI DA CARICARE A VIDEO
003300970526     D*------------
003400970526     D FUV             S             19    DIM(24)
003500970526     D*------------------------------------------------------------------------*
003600970526     D* INPUT
003700970526     D*------------------------------------------------------------------------*
003800970526     D*------------
003900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004000970526     D*------------
004100970526     D FUDDS           DS
004200970526     D  FUDDE1                 1      1
004300970526     D  FUDFUN                 2      3
004400970526     D  FUDDE2                 4     19
004500970526     D  FUDFI0                20     34
004600970526     D  FUDATT                35     35
004700970526     D  FUDFI1                36     80
004800970526     D  FUDDES                 1     19
004900970526     D  FUDRIG                 1     80
005000970526     D*------------
005100970526     D* DS GESTIONE TASTI FUNZIONALI
005200970526     D*------------
005300970526     D DSKEY         E DS
005400970526     D DEVDS1          DS
005500970526     D  KEY                  369    369
005600970526     D*------------
005700970526     D* DS "XMSG" - GESTIONE MESSAGGI
005800970526     D*------------
005900970526     D DSMSG         E DS
006000010228     D*------------
006100970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200010228     D*------------
006300000317     D WLBDA8          DS                  INZ
006400970528     D  G08DAT                 1      8  0
006500970528     D  G08INV                 9     16  0
006600970528     D  G08ERR                17     17
006700970528     D  G08TGI                18     22  0
006800970526     D*------------
006900970526     D* DS DI PROCEDURA
007000970526     D*------------
007100010228     D tis734        E DS                  EXTNAME(tis734ds)
007200970526     D*------------
007300970526     D* ARCHITETTURA
007400970526     D*------------
007500970526     D KPJBA         E DS
007600000807     D*------------
007700010228     D* VARIABILI NON RIFERITE AL DATABASE
007800010228     D*------------
007900000719
008000010228
008100010228
008200970526     C*------------------------------------------------------------------------*
008300970526     C* MAIN LINES
008400970526     C*------------------------------------------------------------------------*
008500970526     C*
008600970526     C* NON CI SONO ERRORI IN ENTRATA
008700010228     C*    ERRL00        IFEQ      '0'
008800970526     C*
008900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009000010228     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009300970526     C*
009400970526     C* EMETTE FORMATO VIDEO
009500970526     C                   EXSR      BUTFMR
009600970526     C*
009700970526     C* GESTIONE TASTI FUNZIONALI
009800970526     C                   EXSR      FUNCON
009900010228     C                   ENDDO
010000970526     C*
010100010228     C* FINE PGM
010200970526     C                   SETON                                        LR
010300970526     C*------------------------------------------------------------------------*
010400970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010500970526     C*------------------------------------------------------------------------*
010600970526     C     BUTFMR        BEGSR
010700970526     C*
010800970526     C* EMETTE LA TESTATA
010900010228     C                   WRITE     S735D01
011000970526     C*
011100970526     C* SPEDISCE EVENTUALE MESSAGGIO
011200970526     C                   EXSR      SNDMSG
011300970526     C*
011400970526     C* EMETTE LE FUNZIONI
011500010228     C                   WRITE     S735D02
011600970526     C*
011700970526     C* EMETTE IL VIDEO
011800010228     C                   EXFMT     S735D03
011900970526     C*
012000970526     C* REIMPOSTA FLAG DI ERRORE
012100970526     C                   MOVEL     '0'           ERRL00
012200970526     C*
012300970526     C* REIMPOSTA INDICATORI DI ERRORE
012400010228     C                   SETOFF                                       616263
012500010228     C                   SETOFF                                       6465
012600970526     C*
012700970526     C                   ENDSR
012800970526     C*------------------------------------------------------------------------*
012900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013000970526     C*------------------------------------------------------------------------*
013100970526     C     SNDMSG        BEGSR
013200970526     C*
013300970526     C* PULISCE IL SFILE MESSAGGI
013400970526     C                   MOVEL     '1'           *IN40
013500010228     C                   WRITE     S735DMC
013600970526     C                   MOVEL     '0'           *IN40
013700970526     C*
013800970526     C* VISUALIZZA IL MESSAGGIO
013900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014000970526     C                   CALL      'XMSG'        XMSGPA
014100010228     C     DSMSER        IFEQ      *ZEROS
014200970526     C                   MOVEL     DSMSKE        MSGKEY
014300010228     C                   ENDIF
014400970526     C                   Z-ADD     1             MSGULT
014500010228     C                   WRITE     S735DMS
014600010228     C                   WRITE     S735DMC
014700970526     C*
014800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
014900970526     C                   MOVEL     *BLANKS       DSMSMS
015000970526     C*
015100970526     C                   ENDSR
015200970526     C*------------------------------------------------------------------------*
015300970526     C* ENTGES - GESTIONE ENTER -> INVIO
015400970526     C*------------------------------------------------------------------------*
015500970526     C     ENTGES        BEGSR
015600970526     C*
015700970526     C* CONTROLLA I CAMPI IMMESSI
015800970526     C                   EXSR      DECODI
015900970526     C*
016000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
016100010228     C     DEPOP1        IFEQ      'O05'
016200970526     C                   MOVEL     '1'           FINL00
016300010228     C                   ENDIF
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700010228     C* F03GES - GESTIONE F03 -> FINE
016800970526     C*------------------------------------------------------------------------*
016900970526     C     F03GES        BEGSR
017000970526     C*
017100970526     C* IMPOSTA "PREMUTO F03"
017200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017300010228     C                   MOVEL     '1'           s34F03                         *DI PROCEDURA
017400970526     C*
017500970526     C                   ENDSR
017600970526     C*------------------------------------------------------------------------*
017700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
017800970526     C*------------------------------------------------------------------------*
017900970526     C     F06GES        BEGSR
018000970526     C*
018100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
018200010228     C     DEPOP1        IFEQ      'F06'
018300970526     C                   EXSR      INSERI
018400010228     C                   ENDIF
018500010228     C     DEPOP1        IFEQ      'O02'
018600010228     C                   EXSR      MODIFI
018700010228     C                   ENDIF
018800010228     C     DEPOP1        IFEQ      'O04'
018900010228     C                   EXSR      ANNULL
019000010228     C                   ENDIF
019100010228     C     DEPOP1        IFEQ      'O07'
019200970526     C                   EXSR      RIPRIS
019300010228     C                   ENDIF
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* F12GES - GESTIONE F12 -> RITORNO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     F12GES        BEGSR
020000970526     C*
020100970526     C* IMPOSTA "PREMUTO F12"
020200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020300010228     C                   MOVEL     '1'           s34F12                         *DI PROCEDURA
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F24GES        BEGSR
021000970526     C*
021100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500010228     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
021600970526     C*------------------------------------------------------------------------*
021700970526     C     INSERI        BEGSR
021800970526     C*
021900970526     C* CONTROLLA I CAMPI IMMESSI
022000970526     C                   EXSR      DECODI
022100970526     C*
022200970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
022300010228     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022400010228     C                   EXSR      IMPVTD
022500010228     C                   WRITE     tivtd000
022600010228     C                   MOVEL     MSG(2)        s34MSG                         *MESSAGGIO
022700000719     C                   MOVEL     MSG(2)        DSMSMS                         *MESSAGGIO
022800970602     C*
022900970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
023000970602     C                   EXSR      AZZVID
023100010228     C                   ENDIF
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500010228     C* MODIFI - OPERAZIONI DI MODIFICA
023600970526     C*------------------------------------------------------------------------*
023700970526     C     MODIFI        BEGSR
023800970526     C*
023900970526     C* CONTROLLA I CAMPI IMMESSI
024000970526     C                   EXSR      DECODI
024100970526     C*
024200970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
024300010228     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024400010228     C                   EXSR      LETVTD
024500970526     C*
024600970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024700010228     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024800010228     C                   EXSR      IMPVTD
024900010228     C                   UPDATE    tivtd000
025000010228     C                   MOVEL     MSG(3)        s34MSG
025100000719     C                   MOVEL     MSG(3)        DSMSMS
025200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
025300010228     C                   ENDIF
025400010228     C                   ENDIF
025500970526     C*
025600970526     C                   ENDSR
025700991014     C*------------------------------------------------------------------------*
025800000719     C* COPIA  - OPERAZIONI DI COPIA
025900991014     C*------------------------------------------------------------------------*
026000000719     C     COPIA         BEGSR
026100991014     C*
026200991014     C* CONTROLLA I CAMPI IMMESSI
026300991014     C                   EXSR      DECODI
026400991014     C*
026500991014     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
026600010228     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026700010228     C                   EXSR      IMPVTD
026800010228     C                   WRITE     tivtd000
026900010228     C                   MOVEL     MSG(3)        s34MSG
027000000719     C                   MOVEL     MSG(3)        DSMSMS
027100991014     C                   MOVEL     '1'           FINL00                         *FINE PGM
027200010228     C                   ENDIF
027300991014     C*
027400991014     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600970526     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
027700970526     C*------------------------------------------------------------------------*
027800010228     C     ANNULL        BEGSR
027900970526     C*
028000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
028100010228     C                   EXSR      LETVTD
028200970527     C*
028300970527     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028400010228     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028500010228     C                   MOVEL     'A'           VTDatb                         *FLAG ANNULLAMENTO
028600010228     C                   UPDATE    tivtd000                                     *ANNULLA
028700010228     C                   MOVEL     MSG(4)        s34MSG                         *MESSAGGIO
028800010228     C                   MOVEL     '1'           FINL00                         *FINE PGM
028900010228     C                   ENDIF
029000970526     C*
029100010228     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* RIPRIS - OPERAZIONI DI RIPRISTINO
029400970526     C*------------------------------------------------------------------------*
029500970526     C     RIPRIS        BEGSR
029600970526     C*
029700970526     C* CONTROLLA I CAMPI IMMESSI
029800970526     C                   EXSR      DECODI
029900970526     C*
030000970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030100010228     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030200010228     C                   EXSR      LETVTD
030300970526     C*
030400970526     C* IMPOSTA IL RECORD DA SCRIVERE
030500010228     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030600010228     C                   MOVEL     *blanks       VTDatb
030700010228     C                   UPDATE    tivtd000                                     *ANNULLA
030800010228     C                   ENDIF
030900010228     C                   MOVEL     MSG(5)        s34MSG                         *MESSAGGIO
031000000719     C                   MOVEL     MSG(5)        DSMSMS                         *MESSAGGIO
031100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031200010228     C                   ENDIF
031300970526     C*
031400970526     C                   ENDSR
031500970526     C*------------------------------------------------------------------------*
031600010228     C* LETVTD - LEGGE IL RECORD PERL'AGGIORNAMENTO
031700970526     C*------------------------------------------------------------------------*
031800010228     C     LETVTD        BEGSR
031900970526     C*
032000010717     C                   MOVEL     VIDksu        KVDksu
032100010228     C                   MOVEL     VIDsed        KVDsed
032200010228     C     KEYvtd        CHAIN     tivtd01L                           9998
032300010228     C     *IN98         IFEQ      *ON                                          *ALLOCATO
032400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
032500010228     C                   MOVEL     MSG(7)        s34MSG                         *MESSAGGIO
032600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
032700010228     C                   ENDIF
032800010228     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
032900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
033000010228     C                   MOVEL     MSG(6)        s34msg                         *MESSAGGIO
033100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
033200010228     C                   ENDIF
033300970526     C*
033400970526     C                   ENDSR
033500970526     C*------------------------------------------------------------------------*
033600010228     C* DECODI - CONTROLLA CAMPI E DECODIFICA
033700970526     C*------------------------------------------------------------------------*
033800970526     C     DECODI        BEGSR
033900000719     C*
034000000719     C                   MOVEL     '0'           ERRL00
034100000131     C*
034200991015     C*--------------------
034300010228     C* SEGNACOLLO DAL >= SEGNACOLLO AL
034400991015     C*--------------------
034500010228     C     ERRL00        IFEQ      '0'
034600010228     C                   IF        VIDsea < VIDsed
034700010228     C                   SETON                                        6465
034800010228     C                   MOVEL     MSG(15)       DSMSMS
034900010228     C                   ENDIF
035000010228     C                   ENDIF
035100010228     C*--------------------
035200970528     C* ACCAVALLAMENTO DATI
035300010228     C*--------------------
035400010228     C     ERRL00        IFEQ      '0'
035500010717     C     VIDksu        SETLL     tivtd01l
035600010717     C     VIDksu        READE(N)  tivtd01l                               99
035700010228     C     *IN99         DOWEQ     *OFF
035800010228     C                   IF        (DEPOP1 <> 'F06' AND VIDsed <> VTDsed) OR
035900010228     C                              DEPOP1 = 'F06'
036000010228     C                   IF        (VIDsed >= VTDsed AND VIDsed <= VTDsea) OR
036100010228     C                             (VIDsea >= VTDsed AND VIDsea <= VTDsea)
036200010228     C                   SETON                                        6465
036300010228     C                   MOVEL     MSG(17)       DSMSMS
036400970528     C                   MOVEL     '1'           ERRL00
036500970528     C                   LEAVE
036600010228     C                   ENDIF
036700010228     C                   ENDIF
036800010717     C     VIDksu        READE(N)  tivtd01L                               99
036900010228     C                   ENDDO
037000010228     C                   ENDIF
037100000731     C*
037200970528     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
037500970526     C*------------------------------------------------------------------------*
037600970526     C     IREVID        BEGSR
037700010228     C*
037800970526     C* CAMPI
037900010717     C                   MOVEL     VTDksu        VIDksu
038000010228     C                   MOVEL     VTDute        VIDute
038100010228     C                   MOVEL     VTDpwd        VIDpwd
038200010228     C                   MOVEL     VTDsed        VIDsed
038300010228     C                   MOVEL     VTDsea        VIDsea
038400970527     C*
038500970526     C                   ENDSR
038600970526     C*------------------------------------------------------------------------*
038700010228     C* AZZVID - AZZERA I CAMPI DEL VIDEO
038800970526     C*------------------------------------------------------------------------*
038900970526     C     AZZVID        BEGSR
039000010228     C*
039100010228     C                   CLEAR                   S735D03
039200970526     C*
039300970526     C                   ENDSR
039400970526     C*------------------------------------------------------------------------*
039500010228     C* IMPVTD - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
039600970526     C*------------------------------------------------------------------------*
039700010228     C     IMPVTD        BEGSR
039800970526     C*
039900010717     C                   MOVEL     VIDksu        VTDksu
040000010228     C                   MOVEL     VIDute        VTDute
040100010228     C                   MOVEL     VIDpwd        VTDpwd
040200010228     C                   MOVEL     VIDsed        VTDsed
040300010228     C                   MOVEL     VIDsea        VTDsea
040400970526     C*
040500970526     C                   ENDSR
040600970526     C*------------------------------------------------------------------------*
040700970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
040800970526     C*------------------------------------------------------------------------*
040900970526     C     IMPFUN        BEGSR
041000970526     C*
041100970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
041200010228     C     DEPOP1        IFEQ      'O05'
041300970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
041400970526     C                   MOVEL     'N'           FUDATT
041500970526     C                   MOVEL     FUDRIG        FUD(6)
041600010228     C                   ENDIF
041700970526     C*
041800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
041900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
042000970526     C                   EXSR      FUNGES
042100970526     C*
042200970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
042300970526     C*---
042400970526     C* 90 -> PROTEZIONE CHIAVE
042500970526     C* 91 -> PROTEZIONE TOTALE
042600970526     C*---
042700010228     C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
042800010228     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
042900970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
043000970526     C                   SETOFF                                       91
043100010228     C                   ENDIF
043200010228     C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
043300970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
043400970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
043500010228     C                   ENDIF
043600010228     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
043700970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
043800970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
043900970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
044000970526     C                   SETON                                        91
044100010228     C                   ENDIF
044200010228     C                   ELSE                                                   *I N T E R R O G A
044300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
044400970526     C                   SETON                                        91
044500010228     C                   ENDIF
044600970526     C*
044700970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
044800010228     C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
044900010717     C                   MOVEL     s34ksu        KVDksu
045000010301     C                   MOVEL     s34sed        KVDsed
045100010228     C     KEYvtd        CHAIN     tivtd01L                           9998
045200010228     C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
045300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
045400970526     C                   MOVEL     MSG(6)        DSMSMS
045500010228     C                   ELSE
045600970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
045700010228     C     DEPOP1        IFEQ      'O05'
045800001116     C     DEPOP1        OREQ      'O02'
045900980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
046000980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
046100010228     C                   ENDIF
046200010228     C                   ENDIF
046300010228     C                   ELSE                                                   *INSERIMENTO
046400970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
046500010228     C                   ENDIF
046600970526     C*
046700970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
046800010228     C     DEPOP1        IFEQ      'O02'
046900970526     C                   MOVEL     DEC(1)        VIDFUN
047000010228     C                   ENDIF
047100010228     C     DEPOP1        IFEQ      'O03'
047200991014     C                   MOVEL     DEC(6)        VIDFUN
047300010228     C                   ENDIF
047400010228     C     DEPOP1        IFEQ      'O04'
047500970526     C                   MOVEL     DEC(2)        VIDFUN
047600010228     C                   ENDIF
047700010228     C     DEPOP1        IFEQ      'O05'
047800970526     C                   MOVEL     DEC(3)        VIDFUN
047900010228     C                   ENDIF
048000010228     C     DEPOP1        IFEQ      'O07'
048100970526     C                   MOVEL     DEC(4)        VIDFUN
048200010228     C                   ENDIF
048300010228     C     DEPOP1        IFEQ      'F06'
048400970526     C                   MOVEL     DEC(5)        VIDFUN
048500010228     C                   ENDIF
048600970526     C*
048700970526     C                   ENDSR
048800970526     C*------------------------------------------------------------------------*
048900970526     C* *INZSR - OPERAZIONI INIZIALI
049000970526     C*------------------------------------------------------------------------*
049100970526     C     *INZSR        BEGSR
049200970526     C*---
049300970526     C* RICEVIMENTO PARAMETRI
049400970526     C*---
049500970526     C     *ENTRY        PLIST
049600970612     C                   PARM                    KPJBA
049700010228     C                   MOVEL     KPJBU         tis734
049800970526     C*
049900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
050000970526     C     XMSGPA        PLIST
050100970526     C                   PARM                    DSMSMS
050200970526     C                   PARM                    DSMSPG
050300970526     C                   PARM                    DSMSKE
050400970526     C                   PARM                    DSMSER
050500970526     C*
050600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
050700970526     C                   CALL      'XKEY'
050800970526     C                   PARM                    DSKEY
050900001116     C*---
051000001116     C* VARIABILI NON RIFERITE AL DATA BASE
051100001116     C*---
051200001116     C     *LIKE         DEFINE    FUDFUN        FUNKEY
051300010228     C     *LIKE         DEFINE    s34OP0        DEPOP0
051400010228     C     *LIKE         DEFINE    s34OP1        DEPOP1
051500010228     C     *LIKE         DEFINE    s34OP0        WOP0
051600010228     C     *LIKE         DEFINE    s34OP1        WOP1
051700001116     C*---
051800001116     C* VARIABILI RIFERITE AL DATA BASE
051900001116     C*---
052000010717     C     *LIKE         DEFINE    VTDksu        KVDksu
052100010228     C     *LIKE         DEFINE    VTDsed        KVDsed
052200001116     C*---
052300001116     C* VARIABILI DI CONTROLLO
052400001116     C*---
052500001116     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
052600001116     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
052700001116     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
052800001116     C                   MOVEL     '0'           ERRL00            1            *ERRORE
052900001116     C*---
053000001116     C* CHIAVI DI LETTURA
053100001116     C*---
053200010228     C* LETTURA tivtd01L - COMPLETA
053300010228     C     KEYvtd        KLIST
053400010717     C                   KFLD                    KVDksu                         *UTENTE STRATEGI
053500010228     C                   KFLD                    KVDsed                         *SEGNACOLLO DAL
053600970526     C*
053700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
053800010228     C                   MOVEL     'TIS735R'     DSMSPG
053900010228     C                   MOVEL     'TIS735R'     MSGQUE
054000970526     C                   MOVEL     *BLANKS       MSGKEY
054100970526     C                   MOVEL     *BLANKS       DSMSMS
054200970528     C*
054300970528     C* CALCOLA LA DATA CORRENTE
054400970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
054500970528     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
054600970528     C                   Z-ADD     WN8           G08DAT
054700970528     C                   Z-ADD     *ZEROS        G08INV
054800970528     C                   MOVEL     '0'           G08ERR
054900970528     C                   CALL      'XSRDA8'
055000970528     C                   PARM                    WLBDA8
055100970528     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
055200970526     C*
055300970526     C* DEPOSITA LE OPZIONI DI ENTRATA
055400010228     C                   MOVEL     s34OP0        DEPOP0                         *LIVELLO PROCEDURA
055500010228     C                   MOVEL     s34OP1        DEPOP1                         *LIVELLO PROGRAMMA
055600970526     C*
055700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
055800970526     C                   EXSR      IMPFUN
055900000131     C*
056000970526     C                   ENDSR
056100970526     C*------------------------------------------------------------------------*
056200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
056300970526     C*------------------------------------------------------------------------*
056400970526     C     FUNGES        BEGSR
056500970526     C*
056600970526     C* PULISCO CAMPI DI LAVORO
056700970526     C                   Z-ADD     0             W                 5 0
056800970526     C*
056900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
057000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
057100970526     C     1             DO        24            I
057200970526     C                   MOVEL     FUD(I)        FUDRIG
057300970526     C*
057400970526     C* PULIZIA FUNZIONI PRECEDENTI
057500970526     C                   MOVEL     *BLANKS       FUV(I)
057600970526     C*
057700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
057800970526     C* (SALTA F24)
057900970526     C     FUDATT        IFEQ      'S'
058000970526     C     FUDFUN        ANDNE     '24'
058100970526     C                   ADD       1             W
058200970526     C                   MOVEL     FUDDES        FUV(W)
058300970526     C                   END
058400970526     C                   END                                                    DO WHILE
058500970526     C*
058600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
058700970526     C* ALTRIMENTI DISATTIVO F24
058800970526     C     1             DO        24            I
058900970526     C                   MOVEL     FUD(I)        FUDRIG
059000970526     C*
059100970526     C     FUDFUN        IFEQ      '24'
059200970526     C     W             IFGT      6
059300970526     C                   MOVEL     'S'           FUDATT
059400970526     C                   ELSE
059500970526     C                   MOVEL     *BLANKS       FUDATT
059600970526     C                   END                                                    W GT 8
059700970526     C                   MOVEL     FUDRIG        FUD(I)
059800970526     C                   END
059900970526     C                   END                                                    DO WHILE
060000970526     C*
060100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
060200970526     C     W             DIV       6             I
060300970526     C                   MVR                     Z
060400970526     C*
060500970526     C     Z             IFNE      0
060600970526     C     I             ADD       1             Z
060700970526     C                   ELSE
060800970526     C                   Z-ADD     I             Z                 5 0
060900970526     C                   END
061000970526     C*
061100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
061200970526     C                   ADD       1             FUNGRU
061300970526     C     FUNGRU        IFGT      Z
061400970526     C     FUNGRU        ORLT      1
061500970526     C                   Z-ADD     1             FUNGRU
061600970526     C                   END
061700970526     C*
061800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
061900970526     C     FUNGRU        IFEQ      1
062000970526     C                   MOVEL     FUV(1)        FUN001
062100970526     C                   MOVEL     FUV(2)        FUN002
062200970526     C                   MOVEL     FUV(3)        FUN003
062300970526     C                   MOVEL     FUV(4)        FUN004
062400970526     C                   MOVEL     FUV(5)        FUN005
062500970526     C                   MOVEL     FUV(6)        FUN006
062600970526     C                   END
062700970526     C*
062800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
062900970526     C     FUNGRU        IFEQ      2
063000970526     C                   MOVEL     FUV(7)        FUN001
063100970526     C                   MOVEL     FUV(8)        FUN002
063200970526     C                   MOVEL     FUV(9)        FUN003
063300970526     C                   MOVEL     FUV(10)       FUN004
063400970526     C                   MOVEL     FUV(11)       FUN005
063500970526     C                   MOVEL     FUV(12)       FUN006
063600970526     C                   END
063700970526     C*
063800970526     C* TERZO GRUPPO DI 6 FUNZIONI
063900970526     C     FUNGRU        IFEQ      3
064000970526     C                   MOVEL     FUV(13)       FUN001
064100970526     C                   MOVEL     FUV(14)       FUN002
064200970526     C                   MOVEL     FUV(15)       FUN003
064300970526     C                   MOVEL     FUV(16)       FUN004
064400970526     C                   MOVEL     FUV(17)       FUN005
064500970526     C                   MOVEL     FUV(18)       FUN006
064600970526     C                   END
064700970526     C*
064800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
064900970526     C     FUNGRU        IFEQ      4
065000970526     C                   MOVEL     FUV(19)       FUN001
065100970526     C                   MOVEL     FUV(20)       FUN002
065200970526     C                   MOVEL     FUV(21)       FUN003
065300970526     C                   MOVEL     FUV(22)       FUN004
065400970526     C                   MOVEL     *BLANKS       FUN005
065500970526     C                   MOVEL     *BLANKS       FUN006
065600970526     C                   END
065700970526     C*
065800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
065900970526     C                   MOVEL     *BLANKS       FUN007
066000970526     C                   MOVEL     *BLANKS       FUN008
066100970526     C*
066200970526     C     1             DO        24            I
066300970526     C                   MOVEL     FUD(I)        FUDRIG
066400970526     C     FUDFUN        IFEQ      '24'
066500970526     C     FUDATT        ANDEQ     'S'
066600970526     C                   MOVEL     FUDDES        FUN008
066700970526     C                   END
066800970526     C                   END                                                    DO
066900970526     C*
067000970526     C                   ENDSR
067100970526     C*------------------------------------------------------------------------*
067200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
067300970526     C*------------------------------------------------------------------------*
067400970526     C     FUNCON        BEGSR
067500970526     C*
067600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
067700970526     C                   EXSR      FUNVAR
067800970526     C*
067900970526     C* PULISCO CAMPI DI LAVORO
068000970526     C                   Z-ADD     0             I                 5 0
068100970526     C                   Z-ADD     0             W                 5 0
068200970526     C*
068300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
068400970526     C     FUNKEY        IFNE      *BLANKS
068500970526     C     1             DO        24            I
068600970526     C                   MOVEL     FUD(I)        FUDRIG
068700970526     C*
068800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
068900970526     C     FUNKEY        IFEQ      FUDFUN
069000970526     C     FUDATT        ANDEQ     'S'
069100970526     C                   Z-ADD     I             W
069200970526     C                   END
069300970526     C                   END                                                    DO WHILE
069400970526     C*
069500970526     C* FUNZIONE NON ATTIVA (ERRORE)
069600970526     C     W             IFEQ      0
069700970526     C*
069800970526     C* EMISSIONE MESSAGGIO DI ERRORE
069900970526     C                   MOVEL     MSG(1)        DSMSMS
070000970526     C*
070100970526     C                   END                                                    W=0
070200970526     C                   END                                                    FUNKEY NE *BLANKS
070300970526     C*
070400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
070500970526     C     W             IFNE      0
070600970526     C     FUNKEY        OREQ      *BLANKS
070700970526     C     FUNKEY        IFEQ      *BLANKS
070800970526     C                   EXSR      ENTGES
070900970526     C                   END
071000970526     C     FUNKEY        IFEQ      '03'
071100970526     C                   EXSR      F03GES
071200970526     C                   END
071300970526     C     FUNKEY        IFEQ      '06'
071400970526     C                   EXSR      F06GES
071500970526     C                   END
071600970526     C     FUNKEY        IFEQ      '12'
071700970526     C                   EXSR      F12GES
071800970526     C                   END
071900970526     C     FUNKEY        IFEQ      '24'
072000970526     C                   EXSR      F24GES
072100970526     C                   END
072200970526     C                   END
072300970526     C*
072400970526     C                   ENDSR
072500970526     C*------------------------------------------------------------------------*
072600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
072700970526     C*------------------------------------------------------------------------*
072800970526     C     FUNVAR        BEGSR
072900970526     C*
073000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
073100970526     C*
073200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
073300970526     C     KEY           IFEQ      ENTER
073400970526     C                   MOVEL     *BLANKS       FUNKEY
073500970526     C                   END
073600970526     C     KEY           IFEQ      F1
073700970526     C                   MOVEL     '01'          FUNKEY
073800970526     C                   END
073900970526     C     KEY           IFEQ      F2
074000970526     C                   MOVEL     '02'          FUNKEY
074100970526     C                   END
074200970526     C     KEY           IFEQ      F3
074300970526     C                   MOVEL     '03'          FUNKEY
074400970526     C                   END
074500970526     C     KEY           IFEQ      F4
074600970526     C                   MOVEL     '04'          FUNKEY
074700970526     C                   END
074800970526     C     KEY           IFEQ      F5
074900970526     C                   MOVEL     '05'          FUNKEY
075000970526     C                   END
075100970526     C     KEY           IFEQ      F6
075200970526     C                   MOVEL     '06'          FUNKEY
075300970526     C                   END
075400970526     C     KEY           IFEQ      F7
075500970526     C                   MOVEL     '07'          FUNKEY
075600970526     C                   END
075700970526     C     KEY           IFEQ      F8
075800970526     C                   MOVEL     '08'          FUNKEY
075900970526     C                   END
076000970526     C     KEY           IFEQ      F9
076100970526     C                   MOVEL     '09'          FUNKEY
076200970526     C                   END
076300970526     C     KEY           IFEQ      F10
076400970526     C                   MOVEL     '10'          FUNKEY
076500970526     C                   END
076600970526     C     KEY           IFEQ      F11
076700970526     C                   MOVEL     '11'          FUNKEY
076800970526     C                   END
076900970526     C     KEY           IFEQ      F12
077000970526     C                   MOVEL     '12'          FUNKEY
077100970526     C                   END
077200970526     C     KEY           IFEQ      F13
077300970526     C                   MOVEL     '13'          FUNKEY
077400970526     C                   END
077500970526     C     KEY           IFEQ      F14
077600970526     C                   MOVEL     '14'          FUNKEY
077700970526     C                   END
077800970526     C     KEY           IFEQ      F15
077900970526     C                   MOVEL     '15'          FUNKEY
078000970526     C                   END
078100970526     C     KEY           IFEQ      F16
078200970526     C                   MOVEL     '16'          FUNKEY
078300970526     C                   END
078400970526     C     KEY           IFEQ      F17
078500970526     C                   MOVEL     '17'          FUNKEY
078600970526     C                   END
078700970526     C     KEY           IFEQ      F18
078800970526     C                   MOVEL     '18'          FUNKEY
078900970526     C                   END
079000970526     C     KEY           IFEQ      F19
079100970526     C                   MOVEL     '19'          FUNKEY
079200970526     C                   END
079300970526     C     KEY           IFEQ      F20
079400970526     C                   MOVEL     '20'          FUNKEY
079500970526     C                   END
079600970526     C     KEY           IFEQ      F21
079700970526     C                   MOVEL     '21'          FUNKEY
079800970526     C                   END
079900970526     C     KEY           IFEQ      F22
080000970526     C                   MOVEL     '22'          FUNKEY
080100970526     C                   END
080200970526     C     KEY           IFEQ      F23
080300970526     C                   MOVEL     '23'          FUNKEY
080400970526     C                   END
080500970526     C     KEY           IFEQ      F24
080600970526     C                   MOVEL     '24'          FUNKEY
080700970526     C                   END
080800970526     C     KEY           IFEQ      ROLLUP
080900970526     C                   MOVEL     'RU'          FUNKEY
081000970526     C                   END
081100970526     C     KEY           IFEQ      HELP
081200970526     C                   MOVEL     'HP'          FUNKEY
081300970526     C                   END
081400970526     C                   ENDSR
081500970526     C*------------------------------------------------------------------------*
081600970526     C* NOTE COSTRUZIONE SCHIERE
081700970526     C*------------------------------------------------------------------------*
081800970526     C*
081900970526     C* -MSG-MESSAGGI
082000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
082100970526     C* (NON TOCCARE MSG,1 E MSG,2)
082200970526     C*
082300970526     C* -CMD-COMANDI
082400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
082500970526     C*
082600970526     C* -OPD-OPZIONI
082700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
082800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
082900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
083000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
083100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
083200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
083300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
083400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
083500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
083600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
083700970526     C*
083800970526     C* -FUD-FUNZIONI
083900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
084000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
084100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
084200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
084300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
084400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
084500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
084600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
084700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
084800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
084900970526     O*------------------------------------------------------------------------*
085000970526** DEC - DECODIFICHE
085100970526    MODIFICA                                                      1
085200970526  ANNULLAMENTO                                                    2
085300970526VISUALIZZAZIONE                                                   3
085400970526   RIPRISTINO                                                     4
085500970526  INSERIMENTO                                                     5
085600991105     COPIA                                                        6
085700000215** DES - DESCRIZIONI
085800000215ATTENZIONE: non esistono figli legati                             1
085900000215ATTENZIONE: esistono figli legati                                 2
086000000215                                                                  3
086100000215                                                                  4
086200000215                                                                  5
086300970526** MSG - MESSAGGI
086400010228TIS735R- Tasto funzionale non gestito                                          1
086500010228TIS735R- Inserimento effettuato                                                2
086600010228TIS735R- Modifica effettuata                                                   3
086700010228TIS735R- Annullamento effettuato                                               4
086800010228TIS735R- Ripristino effettuato                                                 5
086900010228TIS735R- Richiesta funzione per record inesistente                             6
087000010228TIS735R- Record momentaneamente allocato: riprovare più tardi                  7
087100010228TIS735R- Richiesta funzione per record esistente                               8
087200010228TIS735R- Codice tipo servizio non presente in tabella                          9
087300010228TIS735R- Msg: Cliente figlio di cliente già abilitato per lo stesso servizio   10
087400010228TIS735R- Il ciente è già abilitato per lo stesso servizio con altro utente     11
087500010228TIS735R- Tipo legame errato verificarne sulla tabella l'esistenza              12
087600010228TIS735R- Codice padre errato                                                   13
087700010228TIS735R- Data errata                                                           14
087800010228TIS735R- Il segnacollo AL deve essere maggiore del segnacollo DAL              15
087900010228TIS735R- Il record non può scadere in un giorno già trascorso                  16
088000010228TIS735R- Il record immesso si sovrappone ad uno già esistente                  17
088100010228TIS735R- Codice figlio errato                                                  18
088200010228TIS735R- Codice padre obbligatorio                                             19
088300010228TIS735R- Il codice figlio non può essere uguale al codice padre                20
088400010228TIS735R- ATTENZIONE!!! Codice STRATEGI inesistente, F06 per forzatura.         21
088500010228TIS735R- Tipo legame non autorizzato.                                          22
088600010228TIS735R- Filiale non abilitata alla gestione dei clienti scelti.               23
088700010228TIS735R- Servizio DL già presente nel periodo per il cliente scelto.           24
088800970526** FUD - FUNZIONI
088900970526                   -        (S/N)=N                        funz.01¬
089000970526                   -        (S/N)=N                        funz.02¬
089100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
089200970526                   -        (S/N)=N                        funz.04¬
089300970526                   -        (S/N)=N                        funz.05¬
089400970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
089500970527                   -        (S/N)=N                        funz.07¬
089600970527                   -        (S/N)=N                        funz.08¬
089700970527                   -        (S/N)=N                        funz.09¬
089800970527                   -        (S/N)=N                        funz.10¬
089900970526                   -        (S/N)=N                        funz.11¬
090000970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
090100970526                   -        (S/N)=N                        funz.13¬
090200970526                   -        (S/N)=N                        funz.14¬
090300970526                   -        (S/N)=N                        funz.15¬
090400970526                   -        (S/N)=N                        funz.16¬
090500970526                   -        (S/N)=N                        funz.17¬
090600970526                   -        (S/N)=N                        funz.18¬
090700970526                   -        (S/N)=N                        funz.19¬
090800970526                   -        (S/N)=N                        funz.20¬
090900970526                   -        (S/N)=N                        funz.21¬
091000970526                   -        (S/N)=N                        funz.22¬
091100970526                   -        (S/N)=N                        funz.23¬
091200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
