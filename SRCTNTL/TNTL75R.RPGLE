000100971007     H*------------------------------------------------------------------------*
000200971007     H* GESTIONE VOCI DI CONTO ECONOMICO TRAINI - ROUTINE
000300971007     H*------------------------------------------------------------------------*
000400971007     H DECEDIT('0,') DATEDIT(*DMY.)
000500971007     F*------------------------------------------------------------------------*
000600971007     F* DATA BASE
000700971007     F*------------------------------------------------------------------------*
000800971007     FTNTL75D   CF   E             WORKSTN INFDS(DEVDS1)
000900971007     F                                     SFILE(TL75DMS:MSGULT)
001000011019     FTITCG01L  UF A E           K DISK
001100971007     D*------------------------------------------------------------------------*
001200971007     D* SCHIERE
001300971007     D*------------------------------------------------------------------------*
001400971007     D*------------
001500971007     D* DECODIFICHE
001600971007     D*------------
001700971007     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
001800971007     D*------------
001900971007     D* MESSAGGI
002000971007     D*------------
002100971007     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200971007     D*------------
002300971007     D* DESCRIZIONI TASTI FUNZIONALI
002400971007     D*------------
002500971007     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002600971007     D*------------
002700971007     D* FUNZIONI DA CARICARE A VIDEO
002800971007     D*------------
002900971007     D FUV             S             19    DIM(24)
003000971007     D*------------------------------------------------------------------------*
003100971007     D* INPUT
003200971007     D*------------------------------------------------------------------------*
003300971007     D*------------
003400971007     D* RIDEFINIZIONE FUNZIONI VIDEO
003500971007     D*------------
003600971007     D FUDDS           DS
003700971007     D  FUDDE1                 1      1
003800971007     D  FUDFUN                 2      3
003900971007     D  FUDDE2                 4     19
004000971007     D  FUDFI0                20     34
004100971007     D  FUDATT                35     35
004200971007     D  FUDFI1                36     80
004300971007     D  FUDDES                 1     19
004400971007     D  FUDRIG                 1     80
004500971007     D*------------
004600971007     D* DS GESTIONE TASTI FUNZIONALI
004700971007     D*------------
004800971007     D DSKEY         E DS
004900971007     D DEVDS1          DS
005000971007     D  KEY                  369    369
005100971007     D*------------
005200971007     D* DS "XMSG" - GESTIONE MESSAGGI
005300971007     D*------------
005400971007     D DSMSG         E DS
005500971007     D*------------------
005600971007     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700971007     D*------------------
005800971007     D WLBDA8          DS                  INZ
005900971007     D  G08DAT                 1      8  0
006000971007     D  G08INV                 9     16  0
006100971007     D  G08ERR                17     17
006200971007     D  G08TGI                18     22  0
006300971007     D*------------
006400971007     D* DS DI PROCEDURA
006500971007     D*------------
006600971007     D DSTL73        E DS                  EXTNAME(TNTL73DS)
006700971007     D*------------
006800971007     D* ARCHITETTURA
006900971007     D*------------
007000971007     D KPJBA         E DS
007100971007     C*------------------------------------------------------------------------*
007200971007     C* MAIN LINES
007300971007     C*------------------------------------------------------------------------*
007400971007     C*
007500971007     C* NON CI SONO ERRORI IN ENTRATA
007600971007IF  1C     ERRL00        IFEQ      '0'
007700971007     C*
007800971007     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
007900971007DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008000971007     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
008100971007     C     FINL00        OREQ      '1'                                          *FINE PGM
008200971007     C*
008300971007     C* EMETTE FORMATO VIDEO
008400971007     C                   EXSR      BUTFMR
008500971007     C*
008600971007     C* GESTIONE TASTI FUNZIONALI
008700971007     C                   EXSR      FUNCON
008800971007E   2C                   ENDDO
008900971007     C*
009000971007     C* CI SONO ERRORI IN ENTRATA
009100971007X   1C                   ELSE
009200971007     C                   MOVEL     '1'           D73ERR
009300971007     C                   MOVEL     DSMSMS        D73MSG
009400971007E   1C                   ENDIF
009500971007     C* FINE PGM
009600971007     C                   SETON                                        LR
009700971007     C*------------------------------------------------------------------------*
009800971007     C* BUTFMR - EMISSIONE FORMATO A VIDEO
009900971007     C*------------------------------------------------------------------------*
010000971007     C     BUTFMR        BEGSR
010100971007     C*
010200971007     C* EMETTE LA TESTATA
010300971007     C                   WRITE     TL75D01
010400971007     C*
010500971007     C* SPEDISCE EVENTUALE MESSAGGIO
010600971007     C                   EXSR      SNDMSG
010700971007     C*
010800971007     C* EMETTE LE FUNZIONI
010900971007     C                   WRITE     TL75D02
011000971007     C*
011100971007     C* EMETTE IL VIDEO
011200971007     C                   EXFMT     TL75D03
011300971007     C*
011400971007     C* REIMPOSTA FLAG DI ERRORE
011500971007     C                   MOVEL     '0'           ERRL00
011600971007     C*
011700971007     C* REIMPOSTA INDICATORI DI ERRORE
011800971007     C                   SETOFF                                       606162
011900971007     C                   SETOFF                                       636465
012000971007     C                   SETOFF                                       666768
012100971007     C                   SETOFF                                       697071
012200971007     C                   SETOFF                                       727374
012300971007     C                   SETOFF                                       757677
012400971007     C                   SETOFF                                       787980
012500971007     C                   SETOFF                                       818283
012600971007     C                   SETOFF                                       848586
012700971007     C                   SETOFF                                       878889
012800971007     C*
012900971007     C                   ENDSR
013000971007     C*------------------------------------------------------------------------*
013100971007     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013200971007     C*------------------------------------------------------------------------*
013300971007     C     SNDMSG        BEGSR
013400971007     C*
013500971007     C* PULISCE IL SFILE MESSAGGI
013600971007     C                   MOVEL     '1'           *IN40
013700971007     C                   WRITE     TL75DMC
013800971007     C                   MOVEL     '0'           *IN40
013900971007     C*
014000971007     C* VISUALIZZA IL MESSAGGIO
014100971007     C                   Z-ADD     *ZEROS        MSGULT            4 0
014200971007     C                   CALL      'XMSG'        XMSGPA
014300971007IF  1C     DSMSER        IFEQ      *ZEROS
014400971007     C                   MOVEL     DSMSKE        MSGKEY
014500971007E   1C                   ENDIF
014600971007     C                   Z-ADD     1             MSGULT
014700971007     C                   WRITE     TL75DMS
014800971007     C                   WRITE     TL75DMC
014900971007     C*
015000971007     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015100971007     C                   MOVEL     *BLANKS       DSMSMS
015200971007     C*
015300971007     C                   ENDSR
015400971007     C*------------------------------------------------------------------------*
015500971007     C* ENTGES - GESTIONE ENTER -> INVIO
015600971007     C*------------------------------------------------------------------------*
015700971007     C     ENTGES        BEGSR
015800971007     C*
015900971007     C* CONTROLLA I CAMPI IMMESSI
016000971007     C                   EXSR      DECODI
016100971007     C*
016200971007     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
016300971007IF  1C     DEPOP1        IFEQ      'O05'
016400971007     C                   MOVEL     '1'           FINL00
016500971007E   1C                   ENDIF
016600971007     C*
016700971007     C                   ENDSR
016800971007     C*------------------------------------------------------------------------*
016900971007     C* F03GES - GESTIONE F03 -> FINE
017000971007     C*------------------------------------------------------------------------*
017100971007     C     F03GES        BEGSR
017200971007     C*
017300971007     C* IMPOSTA "PREMUTO F03"
017400971007     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017500971007     C                   MOVEL     '1'           D73F03                         *DI PROCEDURA
017600971007     C*
017700971007     C                   ENDSR
017800971007     C*------------------------------------------------------------------------*
017900971007     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
018000971007     C*------------------------------------------------------------------------*
018100971007     C     F06GES        BEGSR
018200971007     C*
018300971007     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
018400971007IF  1C     DEPOP1        IFEQ      'F06'
018500971007     C                   EXSR      INSERI
018600971007E   1C                   ENDIF
018700971007IF  1C     DEPOP1        IFEQ      'O02'
018800971007     C                   EXSR      MODIFI
018900971007E   1C                   ENDIF
019000971007IF  1C     DEPOP1        IFEQ      'O04'
019100971007     C                   EXSR      CANCEL
019200971007E   1C                   ENDIF
019300971007     C*
019400971007     C                   ENDSR
019500971007     C*------------------------------------------------------------------------*
019600971007     C* F12GES - GESTIONE F12 -> RITORNO
019700971007     C*------------------------------------------------------------------------*
019800971007     C     F12GES        BEGSR
019900971007     C*
020000971007     C* IMPOSTA "PREMUTO F12"
020100971007     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020200971007     C                   MOVEL     '1'           D73F12                         *DI PROCEDURA
020300971007     C*
020400971007     C                   ENDSR
020500971007     C*------------------------------------------------------------------------*
020600971007     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020700971007     C*------------------------------------------------------------------------*
020800971007     C     F24GES        BEGSR
020900971007     C*
021000971007     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021100971007     C*
021200971007     C                   ENDSR
021300971007     C*------------------------------------------------------------------------*
021400971007     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
021500971007     C*------------------------------------------------------------------------*
021600971007     C     INSERI        BEGSR
021700971007     C*
021800971007     C* CONTROLLA I CAMPI IMMESSI
021900971007     C                   EXSR      DECODI
022000971007     C*
022100971007     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
022200971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022300971007     C                   Z-ADD     DATV1C        TCGDAT                         *CHIAVE
022400971007     C                   EXSR      IMPTCG                                       *ALTRI CAMPI
022500971007     C                   Z-ADD     *ZEROS        TCGDTR                         *DATA TRASMISSIONE
022600971007     C                   MOVEL     *BLANKS       TCGFTR                         *FLAG TRASMISSIONE
022700971007     C                   MOVEL     *BLANKS       TCGATB                         *CANCELLAZIONE
022800011019     C                   WRITE     TITCG000
022900971007     C                   MOVEL     MSG(2)        D73MSG                         *MESSAGGIO
023000971007     C                   MOVEL     '1'           FINL00                         *FINE PGM
023100971007E   1C                   ENDIF
023200971007     C*
023300971007     C                   ENDSR
023400971007     C*------------------------------------------------------------------------*
023500971007     C* MODIFI - OPERAZIONI DI MODIFICA
023600971007     C*------------------------------------------------------------------------*
023700971007     C     MODIFI        BEGSR
023800971007     C*
023900971007     C* CONTROLLA I CAMPI IMMESSI
024000971007     C                   EXSR      DECODI
024100971007     C*
024200971007     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
024300971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024400971007     C                   EXSR      LETTCG
024500971007     C*
024600971007     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024700971007IF  2C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024800971007     C                   EXSR      IMPTCG                                       *IMPOSTA I CAMPI
024900971007     C                   MOVEL     *BLANKS       TCGFTR                         *FLAG TRASMISSIONE
025000971007     C                   MOVEL     *BLANKS       TCGATB                         *CANCELLAZIONE
025100011019     C                   UPDATE    TITCG000
025200971007     C                   MOVEL     MSG(3)        D73MSG
025300971007     C                   MOVEL     '1'           FINL00                         *FINE PGM
025400971007E   2C                   ENDIF
025500971007E   1C                   ENDIF
025600971007     C*
025700971007     C                   ENDSR
025800971007     C*------------------------------------------------------------------------*
025900971007     C* CANCEL - OPERAZIONI DI CANCELLAZIONE
026000971007     C*------------------------------------------------------------------------*
026100971007     C     CANCEL        BEGSR
026200971007     C*
026300971007     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
026400971007     C                   EXSR      LETTCG
026500971007     C*
026600971007     C* CANCELLA IL RECORD
026700011019     C                   DELETE    TITCG000
026800971007     C                   MOVEL     MSG(4)        D73MSG                         *MESSAGGIO
026900971007     C                   MOVEL     '1'           FINL00                         *FINE PGM
027000971007     C*
027100971007     C                   ENDSR
027200971007     C*------------------------------------------------------------------------*
027300971007     C* LETTCG - LEGGE IL RECORD PERL'AGGIORNAMENTO
027400971007     C*------------------------------------------------------------------------*
027500971007     C     LETTCG        BEGSR
027600971007     C*
027700971007     C                   Z-ADD     DATV1C        KTCDAT
027800011019     C     KEYTCG        CHAIN     TITCG01L                           9998
027900971007IF  1C     *IN98         IFEQ      *ON                                          *ALLOCATO
028000971007     C                   MOVEL     '1'           ERRL00                         *ERRORE
028100971007     C                   MOVEL     MSG(7)        D73MSG                         *MESSAGGIO
028200971007     C                   MOVEL     '1'           FINL00                         *FINE PGM
028300971007E   1C                   ENDIF
028400971007IF  1C     *IN99         IFEQ      *ON                                          *INSESISTENTE
028500971007     C                   MOVEL     '1'           ERRL00                         *ERRORE
028600971007     C                   MOVEL     MSG(6)        D73MSG                         *MESSAGGIO
028700971007     C                   MOVEL     '1'           FINL00                         *FINE PGM
028800971007E   1C                   ENDIF
028900971007     C*
029000971007     C                   ENDSR
029100971007     C*------------------------------------------------------------------------*
029200971007     C* DECODI - CONTROLLA CAMPI E DECODIFICA
029300971007     C*------------------------------------------------------------------------*
029400971007     C     DECODI        BEGSR
029500971007     C*--------------------
029600971007     C* DATA
029700971007     C*--------------------
029800971007     C     *LIKE         DEFINE    V1CDAT        DATV1C
029900971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030000971007     C                   Z-ADD     *ZEROS        DATV1C
030100971007     C                   Z-ADD     V1CDAT        WDA8
030200971007     C                   Z-ADD     *ZEROS        DA8W
030300971007     C                   EXSR      CHKDA8
030400971007     C                   Z-ADD     WDA8          V1CDAT
030500971007     C                   Z-ADD     DA8W          DATV1C
030600971007IF  2C     ERRL00        IFEQ      '1'
030700971007     C                   SETON                                        60
030800011019     C                   MOVEL     MSG(9)        DSMSMS
030900971007E   2C                   ENDIF
031000971007E   1C                   ENDIF
031100971007     C*
031200971007     C* INSERIMENTO -> RECORD INESISTENTE
031300971007IF  1C     ERRL00        IFEQ      '0'
031400971007IF  2C     DEPOP1        IFEQ      'F06'
031500971007     C                   Z-ADD     DATV1C        KTCDAT
031600011019     C     KEYTCG        CHAIN(N)  TITCG01L                           99
031700971007IF  3C     *IN99         IFEQ      *OFF
031800971007     C                   SETON                                        60
031900971007     C                   MOVEL     '1'           ERRL00
032000971007     C                   MOVEL     MSG(8)        DSMSMS
032100971007E   3C                   ENDIF
032200971007E   2C                   ENDIF
032300971007E   1C                   ENDIF
032400011019     C*--------------------
032500011019     C* DIVISA
032600011019     C*--------------------
032700011019IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032800011019IF  2C     V1CDIV        IFEQ      'ITL'
032900011019     C     V1CDIV        OREQ      'EUR'
033000011019X   2C                   ELSE
033100011019     C                   MOVEL     '1'           ERRL00
033200011019     C                   SETON                                        78
033300011019     C                   MOVEL     MSG(10)       DSMSMS
033400011019E   2C                   ENDIF
033500011019E   1C                   ENDIF
033600011019     C*
033700011019IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033800011019IF  2C     V1CDIV        IFEQ      'ITL'
033900011019     C     DATV1C        ANDGE     20020101
034000011019     C                   MOVEL     '1'           ERRL00
034100011019     C                   SETON                                        78
034200011019     C                   MOVEL     MSG(11)       DSMSMS
034300011019E   2C                   ENDIF
034400011019E   1C                   ENDIF
034500971007     C*
034600971007     C                   ENDSR
034700971007     C*------------------------------------------------------------------------*
034800971007     C* CHKDA8 - CONTROLLA DATA (8)
034900971007     C*------------------------------------------------------------------------*
035000971007     C     CHKDA8        BEGSR
035100971007     C*
035200971007     C     *LIKE         DEFINE    V1CDAT        WDA8
035300971007     C     *LIKE         DEFINE    V1CDAT        DA8W
035400971007     C*
035500971007     C                   Z-ADD     WDA8          G08DAT
035600971007     C                   Z-ADD     *ZEROS        G08INV
035700971007     C                   MOVEL     '0'           G08ERR
035800971007     C                   CALL      'XSRDA8'
035900971007     C                   PARM                    WLBDA8
036000971007     C                   Z-ADD     G08DAT        WDA8
036100971007     C                   Z-ADD     G08INV        DA8W
036200971007IF  1C     G08ERR        IFEQ      '1'
036300971007     C                   MOVEL     '1'           ERRL00
036400971007E   1C                   ENDIF
036500971007     C*
036600971007     C                   ENDSR
036700971007     C*------------------------------------------------------------------------*
036800971007     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
036900971007     C*------------------------------------------------------------------------*
037000971007     C     IREVID        BEGSR
037100971007     C*
037200971007     C                   Z-ADD     TCGDAT        G08INV
037300971007     C                   Z-ADD     *ZEROS        G08DAT
037400971007     C                   MOVEL     '3'           G08ERR
037500971007     C                   CALL      'XSRDA8'
037600971007     C                   PARM                    WLBDA8
037700971007     C                   Z-ADD     G08DAT        V1CDAT
037800011019     C                   MOVEL     TCGDIV        V1CDIV
037900011019     C                   Z-ADD     TCGRCI        V1CRCI
038000971007     C                   Z-ADD     TCGCCA        V1CCCA
038100971007     C                   Z-ADD     TCGCSA        V1CCSA
038200971007     C                   Z-ADD     TCGCAA        V1CCAA
038300971007     C                   Z-ADD     TCGCCV        V1CCCV
038400971007     C                   Z-ADD     TCGCSV        V1CCSV
038500971007     C                   Z-ADD     TCGCAV        V1CCAV
038600971007     C                   Z-ADD     TCGCOP        V1CCOP
038700971007     C                   Z-ADD     TCGCSP        V1CCSP
038800971007     C                   Z-ADD     TCGCIM        V1CCIM
038900971007     C                   Z-ADD     TCGCFA        V1CCFA
039000971007     C                   Z-ADD     TCGCVI        V1CCVI
039100971007     C                   Z-ADD     TCGCCT        V1CCCT
039200971007     C                   Z-ADD     TCGCTM        V1CCTM
039300971007     C                   Z-ADD     TCGCVS        V1CCVS
039400971007     C                   Z-ADD     TCGCTE        V1CCTE
039500971007     C                   Z-ADD     TCGCVA        V1CCVA
039600971007     C*
039700971007     C                   ENDSR
039800971007     C*------------------------------------------------------------------------*
039900971007     C* AZZVID - AZZERA I CAMPI DEL VIDEO
040000971007     C*------------------------------------------------------------------------*
040100971007     C     AZZVID        BEGSR
040200971007     C*
040300971007     C                   Z-ADD     *ZEROS        V1CDAT
040400011019     C                   MOVEL     *BLANKS       V1CDIV
040500011019     C                   Z-ADD     *ZEROS        V1CRCI
040600971007     C                   Z-ADD     *ZEROS        V1CCCA
040700971007     C                   Z-ADD     *ZEROS        V1CCSA
040800971007     C                   Z-ADD     *ZEROS        V1CCAA
040900971007     C                   Z-ADD     *ZEROS        V1CCCV
041000971007     C                   Z-ADD     *ZEROS        V1CCSV
041100971007     C                   Z-ADD     *ZEROS        V1CCAV
041200971007     C                   Z-ADD     *ZEROS        V1CCOP
041300971007     C                   Z-ADD     *ZEROS        V1CCSP
041400971007     C                   Z-ADD     *ZEROS        V1CCIM
041500971007     C                   Z-ADD     *ZEROS        V1CCFA
041600971007     C                   Z-ADD     *ZEROS        V1CCVI
041700971007     C                   Z-ADD     *ZEROS        V1CCCT
041800971007     C                   Z-ADD     *ZEROS        V1CCTM
041900971007     C                   Z-ADD     *ZEROS        V1CCVS
042000971007     C                   Z-ADD     *ZEROS        V1CCTE
042100971007     C                   Z-ADD     *ZEROS        V1CCVA
042200971007     C*
042300971007     C                   ENDSR
042400971007     C*------------------------------------------------------------------------*
042500971007     C* IMPTCG - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
042600971007     C*------------------------------------------------------------------------*
042700971007     C     IMPTCG        BEGSR
042800971007     C*
042900011019     C                   MOVEL     V1CDIV        TCGDIV
043000011019     C                   Z-ADD     V1CRCI        TCGRCI
043100971007     C                   Z-ADD     V1CCCA        TCGCCA
043200971007     C                   Z-ADD     V1CCSA        TCGCSA
043300971007     C                   Z-ADD     V1CCAA        TCGCAA
043400971007     C                   Z-ADD     V1CCCV        TCGCCV
043500971007     C                   Z-ADD     V1CCSV        TCGCSV
043600971007     C                   Z-ADD     V1CCAV        TCGCAV
043700971007     C                   Z-ADD     V1CCOP        TCGCOP
043800971007     C                   Z-ADD     V1CCSP        TCGCSP
043900971007     C                   Z-ADD     V1CCIM        TCGCIM
044000971007     C                   Z-ADD     V1CCFA        TCGCFA
044100971007     C                   Z-ADD     V1CCVI        TCGCVI
044200971007     C                   Z-ADD     V1CCCT        TCGCCT
044300971007     C                   Z-ADD     V1CCTM        TCGCTM
044400971007     C                   Z-ADD     V1CCVS        TCGCVS
044500971007     C                   Z-ADD     V1CCTE        TCGCTE
044600971007     C                   Z-ADD     V1CCVA        TCGCVA
044700971007     C*
044800971007     C                   ENDSR
044900971007     C*------------------------------------------------------------------------*
045000971007     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
045100971007     C*------------------------------------------------------------------------*
045200971007     C     IMPFUN        BEGSR
045300971007     C*
045400971007     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
045500971007IF  1C     DEPOP1        IFEQ      'O05'
045600971007     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
045700971007     C                   MOVEL     'N'           FUDATT
045800971007     C                   MOVEL     FUDRIG        FUD(6)
045900971007E   1C                   ENDIF
046000971007     C*
046100971007     C* GESTIONE TASTI FUNZIONALI A VIDEO
046200971007     C                   Z-ADD     *ZEROS        FUNGRU            5 0
046300971007     C                   EXSR      FUNGES
046400971007     C*
046500971007     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
046600971007     C*---
046700971007     C* 90 -> PROTEZIONE CHIAVE
046800971007     C* 91 -> PROTEZIONE TOTALE
046900971007     C*---
047000971007IF  1C     DEPOP0        IFEQ      'V02'                                        G E S T I O N E
047100971007IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
047200971007     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
047300971007     C                   SETOFF                                       91
047400971007E   2C                   ENDIF
047500971007IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
047600971007     C                   SETON                                        90        *PROTEZIONE CHIAVE
047700971007     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
047800971007E   2C                   ENDIF
047900971007IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
048000971007     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
048100971007     C                   SETON                                        90        *TUTTE LE PROTEZIONI
048200971007     C                   SETON                                        91
048300971007E   2C                   ENDIF
048400971007X   1C                   ELSE                                                   *I N T E R R O G A
048500971007     C                   SETON                                        90        *TUTTE LE PROTEZIONI
048600971007     C                   SETON                                        91
048700971007E   1C                   ENDIF
048800971007     C*
048900971007     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
049000971007IF  1C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
049100971007     C                   Z-ADD     D73DAT        KTCDAT
049200011019     C     KEYTCG        CHAIN(N)  TITCG01L                           99
049300971007IF  2C     *IN99         IFEQ      *ON                                          *RECORD NON TROVATO
049400971007     C                   MOVEL     '1'           ERRL00                         *ERRORE
049500971007     C                   MOVEL     MSG(6)        DSMSMS
049600971007X   2C                   ELSE
049700971007     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
049800971007     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
049900971007E   2C                   ENDIF
050000971007X   1C                   ELSE                                                   *INSERIMENTO
050100971007     C                   EXSR      AZZVID                                       *AZZERA CAMPI
050200971007E   1C                   ENDIF
050300971007     C*
050400971007     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
050500971007IF  1C     DEPOP1        IFEQ      'O02'
050600971007     C                   MOVEL     DEC(1)        VIDFUN
050700971007E   1C                   ENDIF
050800971007IF  1C     DEPOP1        IFEQ      'O04'
050900971007     C                   MOVEL     DEC(2)        VIDFUN
051000971007E   1C                   ENDIF
051100971007IF  1C     DEPOP1        IFEQ      'O05'
051200971007     C                   MOVEL     DEC(3)        VIDFUN
051300971007E   1C                   ENDIF
051400971007IF  1C     DEPOP1        IFEQ      'F06'
051500971007     C                   MOVEL     DEC(5)        VIDFUN
051600971007E   1C                   ENDIF
051700971007     C*
051800971007     C                   ENDSR
051900971007     C*------------------------------------------------------------------------*
052000971007     C* *INZSR - OPERAZIONI INIZIALI
052100971007     C*------------------------------------------------------------------------*
052200971007     C     *INZSR        BEGSR
052300971007     C*---
052400971007     C* RICEVIMENTO PARAMETRI
052500971007     C*---
052600971007     C     *ENTRY        PLIST
052700971007     C                   PARM                    KPJBA
052800971007     C                   PARM                    DSTL73
052900971007     C*
053000971007     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
053100971007     C     XMSGPA        PLIST
053200971007     C                   PARM                    DSMSMS
053300971007     C                   PARM                    DSMSPG
053400971007     C                   PARM                    DSMSKE
053500971007     C                   PARM                    DSMSER
053600971007     C*
053700971007     C* IMPOSTA LA DS -TASTI PREMUTI-
053800971007     C                   CALL      'XKEY'
053900971007     C                   PARM                    DSKEY
054000971007     C*---
054100971007     C* VARIABILI NON RIFERITE AL DATA BASE
054200971007     C*---
054300971007     C     *LIKE         DEFINE    FUDFUN        FUNKEY
054400971007     C     *LIKE         DEFINE    D73OP0        DEPOP0                         *DI DEPOSITO
054500971007     C     *LIKE         DEFINE    D73OP1        DEPOP1
054600971007     C*---
054700971007     C* VARIABILI DI CONTROLLO
054800971007     C*---
054900971007     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
055000971007     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055100971007     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
055200971007     C                   MOVEL     '0'           ERRL00            1            *ERRORE
055300971007     C                   MOVEL     'S'           $LOBS             1            *LOCALITA' OBSOLETE
055400971007     C*
055500971007     C* IMPOSTA LA DS -INVIO MESSAGGI-
055600971007     C                   MOVEL     'TNTL75R'     DSMSPG
055700971007     C                   MOVEL     'TNTL75R'     MSGQUE
055800971007     C                   MOVEL     *BLANKS       MSGKEY
055900971007     C                   MOVEL     *BLANKS       DSMSMS
056000971007     C*---
056100971007     C* VARIABILI RIFERITE AL DATA BASE
056200971007     C*---
056300011019     C     *LIKE         DEFINE    TCGDAT        KTCDAT                         *TITCG00F
056400971007     C*---
056500971007     C* CHIAVI DI LETTURA
056600971007     C*---
056700011019     C* LETTURA TITCG01L - COMPLETA
056800971007     C     KEYTCG        KLIST
056900971007     C                   KFLD                    KTCDAT                         *DATA
057000971007     C*
057100971007     C* DEPOSITA LE OPZIONI DI ENTRATA
057200971007     C                   MOVEL     D73OP0        DEPOP0                         *LIVELLO PROCEDURA
057300971007     C                   MOVEL     D73OP1        DEPOP1                         *LIVELLO PROGRAMMA
057400971007     C*
057500971007     C* IMPOSTA LE FUNZIONI DI INGRESSO
057600971007     C                   EXSR      IMPFUN
057700971007     C*
057800971007     C                   ENDSR
057900971007     C*------------------------------------------------------------------------*
058000971007     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
058100971007     C*------------------------------------------------------------------------*
058200971007     C     FUNGES        BEGSR
058300971007     C*
058400971007     C* PULISCO CAMPI DI LAVORO
058500971007     C                   Z-ADD     0             W                 5 0
058600971007     C*
058700971007     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
058800971007     C* E CARICO TABELLA FUNZIONI ATTIVE
058900971007     C     1             DO        24            I
059000971007     C                   MOVEL     FUD(I)        FUDRIG
059100971007     C*
059200971007     C* PULIZIA FUNZIONI PRECEDENTI
059300971007     C                   MOVEL     *BLANKS       FUV(I)
059400971007     C*
059500971007     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
059600971007     C* (SALTA F24)
059700971007     C     FUDATT        IFEQ      'S'
059800971007     C     FUDFUN        ANDNE     '24'
059900971007     C                   ADD       1             W
060000971007     C                   MOVEL     FUDDES        FUV(W)
060100971007     C                   END
060200971007     C                   END                                                    DO WHILE
060300971007     C*
060400971007     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
060500971007     C* ALTRIMENTI DISATTIVO F24
060600971007     C     1             DO        24            I
060700971007     C                   MOVEL     FUD(I)        FUDRIG
060800971007     C*
060900971007     C     FUDFUN        IFEQ      '24'
061000971007     C     W             IFGT      6
061100971007     C                   MOVEL     'S'           FUDATT
061200971007     C                   ELSE
061300971007     C                   MOVEL     *BLANKS       FUDATT
061400971007     C                   END                                                    W GT 8
061500971007     C                   MOVEL     FUDRIG        FUD(I)
061600971007     C                   END
061700971007     C                   END                                                    DO WHILE
061800971007     C*
061900971007     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
062000971007     C     W             DIV       6             I
062100971007     C                   MVR                     Z
062200971007     C*
062300971007     C     Z             IFNE      0
062400971007     C     I             ADD       1             Z
062500971007     C                   ELSE
062600971007     C                   Z-ADD     I             Z                 5 0
062700971007     C                   END
062800971007     C*
062900971007     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
063000971007     C                   ADD       1             FUNGRU
063100971007     C     FUNGRU        IFGT      Z
063200971007     C     FUNGRU        ORLT      1
063300971007     C                   Z-ADD     1             FUNGRU
063400971007     C                   END
063500971007     C*
063600971007     C* PRIMO GRUPPO DI 6 FUNZIONI
063700971007     C     FUNGRU        IFEQ      1
063800971007     C                   MOVEL     FUV(1)        FUN001
063900971007     C                   MOVEL     FUV(2)        FUN002
064000971007     C                   MOVEL     FUV(3)        FUN003
064100971007     C                   MOVEL     FUV(4)        FUN004
064200971007     C                   MOVEL     FUV(5)        FUN005
064300971007     C                   MOVEL     FUV(6)        FUN006
064400971007     C                   END
064500971007     C*
064600971007     C* SECONDO GRUPPO DI 6 FUNZIONI
064700971007     C     FUNGRU        IFEQ      2
064800971007     C                   MOVEL     FUV(7)        FUN001
064900971007     C                   MOVEL     FUV(8)        FUN002
065000971007     C                   MOVEL     FUV(9)        FUN003
065100971007     C                   MOVEL     FUV(10)       FUN004
065200971007     C                   MOVEL     FUV(11)       FUN005
065300971007     C                   MOVEL     FUV(12)       FUN006
065400971007     C                   END
065500971007     C*
065600971007     C* TERZO GRUPPO DI 6 FUNZIONI
065700971007     C     FUNGRU        IFEQ      3
065800971007     C                   MOVEL     FUV(13)       FUN001
065900971007     C                   MOVEL     FUV(14)       FUN002
066000971007     C                   MOVEL     FUV(15)       FUN003
066100971007     C                   MOVEL     FUV(16)       FUN004
066200971007     C                   MOVEL     FUV(17)       FUN005
066300971007     C                   MOVEL     FUV(18)       FUN006
066400971007     C                   END
066500971007     C*
066600971007     C* QUARTO GRUPPO DI 6 FUNZIONI
066700971007     C     FUNGRU        IFEQ      4
066800971007     C                   MOVEL     FUV(19)       FUN001
066900971007     C                   MOVEL     FUV(20)       FUN002
067000971007     C                   MOVEL     FUV(21)       FUN003
067100971007     C                   MOVEL     FUV(22)       FUN004
067200971007     C                   MOVEL     *BLANKS       FUN005
067300971007     C                   MOVEL     *BLANKS       FUN006
067400971007     C                   END
067500971007     C*
067600971007     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
067700971007     C                   MOVEL     *BLANKS       FUN007
067800971007     C                   MOVEL     *BLANKS       FUN008
067900971007     C*
068000971007     C     1             DO        24            I
068100971007     C                   MOVEL     FUD(I)        FUDRIG
068200971007     C     FUDFUN        IFEQ      '24'
068300971007     C     FUDATT        ANDEQ     'S'
068400971007     C                   MOVEL     FUDDES        FUN008
068500971007     C                   END
068600971007     C                   END                                                    DO
068700971007     C*
068800971007     C                   ENDSR
068900971007     C*------------------------------------------------------------------------*
069000971007     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
069100971007     C*------------------------------------------------------------------------*
069200971007     C     FUNCON        BEGSR
069300971007     C*
069400971007     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
069500971007     C                   EXSR      FUNVAR
069600971007     C*
069700971007     C* PULISCO CAMPI DI LAVORO
069800971007     C                   Z-ADD     0             I                 5 0
069900971007     C                   Z-ADD     0             W                 5 0
070000971007     C*
070100971007     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
070200971007     C     FUNKEY        IFNE      *BLANKS
070300971007     C     1             DO        24            I
070400971007     C                   MOVEL     FUD(I)        FUDRIG
070500971007     C*
070600971007     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
070700971007     C     FUNKEY        IFEQ      FUDFUN
070800971007     C     FUDATT        ANDEQ     'S'
070900971007     C                   Z-ADD     I             W
071000971007     C                   END
071100971007     C                   END                                                    DO WHILE
071200971007     C*
071300971007     C* FUNZIONE NON ATTIVA (ERRORE)
071400971007     C     W             IFEQ      0
071500971007     C*
071600971007     C* EMISSIONE MESSAGGIO DI ERRORE
071700971007     C                   MOVEL     MSG(1)        DSMSMS
071800971007     C*
071900971007     C                   END                                                    W=0
072000971007     C                   END                                                    FUNKEY NE *BLANKS
072100971007     C*
072200971007     C* FUNZIONE ATTIVA (PROCEDI ...)
072300971007     C     W             IFNE      0
072400971007     C     FUNKEY        OREQ      *BLANKS
072500971007     C     FUNKEY        IFEQ      *BLANKS
072600971007     C                   EXSR      ENTGES
072700971007     C                   END
072800971007     C     FUNKEY        IFEQ      '03'
072900971007     C                   EXSR      F03GES
073000971007     C                   END
073100971007     C     FUNKEY        IFEQ      '06'
073200971007     C                   EXSR      F06GES
073300971007     C                   END
073400971007     C     FUNKEY        IFEQ      '12'
073500971007     C                   EXSR      F12GES
073600971007     C                   END
073700971007     C     FUNKEY        IFEQ      '24'
073800971007     C                   EXSR      F24GES
073900971007     C                   END
074000971007     C                   END
074100971007     C*
074200971007     C                   ENDSR
074300971007     C*------------------------------------------------------------------------*
074400971007     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
074500971007     C*------------------------------------------------------------------------*
074600971007     C     FUNVAR        BEGSR
074700971007     C*
074800971007     C* INIZIALIZZA VARIABILE -FUNKEY-
074900971007     C*
075000971007     C* FUNKEY=*BLANKS SIGNIFICA ENTER
075100971007     C     KEY           IFEQ      ENTER
075200971007     C                   MOVEL     *BLANKS       FUNKEY
075300971007     C                   END
075400971007     C     KEY           IFEQ      F1
075500971007     C                   MOVEL     '01'          FUNKEY
075600971007     C                   END
075700971007     C     KEY           IFEQ      F2
075800971007     C                   MOVEL     '02'          FUNKEY
075900971007     C                   END
076000971007     C     KEY           IFEQ      F3
076100971007     C                   MOVEL     '03'          FUNKEY
076200971007     C                   END
076300971007     C     KEY           IFEQ      F4
076400971007     C                   MOVEL     '04'          FUNKEY
076500971007     C                   END
076600971007     C     KEY           IFEQ      F5
076700971007     C                   MOVEL     '05'          FUNKEY
076800971007     C                   END
076900971007     C     KEY           IFEQ      F6
077000971007     C                   MOVEL     '06'          FUNKEY
077100971007     C                   END
077200971007     C     KEY           IFEQ      F7
077300971007     C                   MOVEL     '07'          FUNKEY
077400971007     C                   END
077500971007     C     KEY           IFEQ      F8
077600971007     C                   MOVEL     '08'          FUNKEY
077700971007     C                   END
077800971007     C     KEY           IFEQ      F9
077900971007     C                   MOVEL     '09'          FUNKEY
078000971007     C                   END
078100971007     C     KEY           IFEQ      F10
078200971007     C                   MOVEL     '10'          FUNKEY
078300971007     C                   END
078400971007     C     KEY           IFEQ      F11
078500971007     C                   MOVEL     '11'          FUNKEY
078600971007     C                   END
078700971007     C     KEY           IFEQ      F12
078800971007     C                   MOVEL     '12'          FUNKEY
078900971007     C                   END
079000971007     C     KEY           IFEQ      F13
079100971007     C                   MOVEL     '13'          FUNKEY
079200971007     C                   END
079300971007     C     KEY           IFEQ      F14
079400971007     C                   MOVEL     '14'          FUNKEY
079500971007     C                   END
079600971007     C     KEY           IFEQ      F15
079700971007     C                   MOVEL     '15'          FUNKEY
079800971007     C                   END
079900971007     C     KEY           IFEQ      F16
080000971007     C                   MOVEL     '16'          FUNKEY
080100971007     C                   END
080200971007     C     KEY           IFEQ      F17
080300971007     C                   MOVEL     '17'          FUNKEY
080400971007     C                   END
080500971007     C     KEY           IFEQ      F18
080600971007     C                   MOVEL     '18'          FUNKEY
080700971007     C                   END
080800971007     C     KEY           IFEQ      F19
080900971007     C                   MOVEL     '19'          FUNKEY
081000971007     C                   END
081100971007     C     KEY           IFEQ      F20
081200971007     C                   MOVEL     '20'          FUNKEY
081300971007     C                   END
081400971007     C     KEY           IFEQ      F21
081500971007     C                   MOVEL     '21'          FUNKEY
081600971007     C                   END
081700971007     C     KEY           IFEQ      F22
081800971007     C                   MOVEL     '22'          FUNKEY
081900971007     C                   END
082000971007     C     KEY           IFEQ      F23
082100971007     C                   MOVEL     '23'          FUNKEY
082200971007     C                   END
082300971007     C     KEY           IFEQ      F24
082400971007     C                   MOVEL     '24'          FUNKEY
082500971007     C                   END
082600971007     C     KEY           IFEQ      ROLLUP
082700971007     C                   MOVEL     'RU'          FUNKEY
082800971007     C                   END
082900971007     C     KEY           IFEQ      HELP
083000971007     C                   MOVEL     'HP'          FUNKEY
083100971007     C                   END
083200971007     C                   ENDSR
083300971007     C*------------------------------------------------------------------------*
083400971007     C* NOTE COSTRUZIONE SCHIERE
083500971007     C*------------------------------------------------------------------------*
083600971007     C*
083700971007     C* -MSG-MESSAGGI
083800971007     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
083900971007     C* (NON TOCCARE MSG,1 E MSG,2)
084000971007     C*
084100971007     C* -CMD-COMANDI
084200971007     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
084300971007     C*
084400971007     C* -OPD-OPZIONI
084500971007     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
084600971007     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
084700971007     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
084800971007     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
084900971007     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
085000971007     C*  . POSSONO ESSERE IN ORDINE SPARSO
085100971007     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
085200971007     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
085300971007     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
085400971007     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
085500971007     C*
085600971007     C* -FUD-FUNZIONI
085700971007     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
085800971007     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
085900971007     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
086000971007     C*  . POSSONO ESSERE IN ORDINE SPARSO
086100971007     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
086200971007     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
086300971007     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
086400971007     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
086500971007     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
086600971007     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
086700971007     O*------------------------------------------------------------------------*
086800971007** DEC - DECODIFICHE
086900971007    MODIFICA                                                      1
087000971007 CANCELLAZIONE                                                    2
087100971007VISUALIZZAZIONE                                                   3
087200971007
087300971007  INSERIMENTO                                                     5
087400971007** MSG - MESSAGGI
087500971007TNTL75R- Tasto funzionale non gestito                                          1
087600971007TNTL75R- Inserimento effettuato                                                2
087700971007TNTL75R- Modifica effettuata                                                   3
087800971007TNTL75R- Cancellazione effettuata                                              4
087900971007TNTL75R- 5
088000971007TNTL75R- Richiesta funzione per record inesistente                             6
088100971007TNTL75R- Record momentaneamente allocato: riprovare più tardi                  7
088200971007TNTL75R- Richiesta funzione per record esistente                               8
088300971007TNTL75R- Data errata                                                           9
088400011019TNTL75R- La divisa può essere solo ITL o EUR                                  10
088500011019TNTL75R- La divisa, a questa data, può solo essere EUR                        11
088600971007TNTL75R- 12
088700971007TNTL75R- 13
088800971007TNTL75R- 14
088900971007TNTL75R- 15
089000971007TNTL75R- 16
089100971007TNTL75R- 17
089200971007TNTL75R- 18
089300971007TNTL75R- 19
089400971007TNTL75R- 20
089500971007** FUD - FUNZIONI
089600971007                   -        (S/N)=N                        funz.01¬
089700971007                   -        (S/N)=N                        funz.02¬
089800971007F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
089900971007                   -        (S/N)=N                        funz.04¬
090000971007                   -        (S/N)=N                        funz.05¬
090100971007F06=Aggiornamento  -        (S/N)=S                        funz.06¬
090200971007                   -        (S/N)=N                        funz.07¬
090300971007                   -        (S/N)=N                        funz.08¬
090400971007                   -        (S/N)=N                        funz.09¬
090500971007                   -        (S/N)=N                        funz.10¬
090600971007                   -        (S/N)=N                        funz.11¬
090700971007F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
090800971007                   -        (S/N)=N                        funz.13¬
090900971007                   -        (S/N)=N                        funz.14¬
091000971007                   -        (S/N)=N                        funz.15¬
091100971007                   -        (S/N)=N                        funz.16¬
091200971007                   -        (S/N)=N                        funz.17¬
091300971007                   -        (S/N)=N                        funz.18¬
091400971007                   -        (S/N)=N                        funz.19¬
091500971007                   -        (S/N)=N                        funz.20¬
091600971007                   -        (S/N)=N                        funz.21¬
091700971007                   -        (S/N)=N                        funz.22¬
091800971007                   -        (S/N)=N                        funz.23¬
091900971007F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
