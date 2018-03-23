000100971007     H*------------------------------------------------------------------------*
000200971007     H* GESTIONE VOCI DI CONTO ECONOMICO TRAINI - SCELTA
000300971007     H*------------------------------------------------------------------------*
000400971007     H DECEDIT('0,') DATEDIT(*DMY.)
000500971007     F*------------------------------------------------------------------------*
000600971007     F* DATA BASE
000700971007     F*------------------------------------------------------------------------*
000800971007     FTNTL74D   CF   E             WORKSTN INFDS(DEVDS1)
000900971007     F                                     SFILE(TL74D05:SFLULT)
001000971007     F                                     SFILE(TL74DMS:MSGULT)
001100011019     FTITCG01L  IF   E           K DISK
001200971007     D*------------------------------------------------------------------------*
001300971007     D* SCHIERE
001400971007     D*------------------------------------------------------------------------*
001500971007     D*------------
001600971007     D* DECODIFICHE
001700971007     D*------------
001800971007     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
001900971007     D*------------
002000971007     D* MESSAGGI
002100971007     D*------------
002200971007     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002300971007     D*------------
002400971007     D* DESCRIZIONI OPZIONI
002500971007     D*------------
002600971007     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002700971007     D*------------
002800971007     D* DESCRIZIONI TASTI FUNZIONALI
002900971007     D*------------
003000971007     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100971007     D*------------
003200971007     D* OPZIONI DA CARICARE A VIDEO
003300971007     D*------------
003400971007     D OPV             S             19    DIM(24)
003500971007     D*------------
003600971007     D* FUNZIONI DA CARICARE A VIDEO
003700971007     D*------------
003800971007     D FUV             S             19    DIM(24)
003900971007     D*------------------------------------------------------------------------*
004000971007     D* INPUT
004100971007     D*------------------------------------------------------------------------*
004200971007     D*------------
004300971007     D* RIDEFINIZIONE OPZIONI VIDEO
004400971007     D*------------
004500971007     D OPDDS           DS
004600971007     D  OPDOPZ                 1      2
004700971007     D  OPDDE1                 3     19
004800971007     D  OPDOP1                21     22
004900971007     D  OPDFI0                23     34
005000971007     D  OPDATT                35     35
005100971007     D  OPDFI1                36     80
005200971007     D  OPDDES                 1     19
005300971007     D  OPDRIG                 1     80
005400971007     D*------------
005500971007     D* RIDEFINIZIONE FUNZIONI VIDEO
005600971007     D*------------
005700971007     D FUDDS           DS
005800971007     D  FUDDE1                 1      1
005900971007     D  FUDFUN                 2      3
006000971007     D  FUDDE2                 4     19
006100971007     D  FUDFI0                20     34
006200971007     D  FUDATT                35     35
006300971007     D  FUDFI1                36     80
006400971007     D  FUDDES                 1     19
006500971007     D  FUDRIG                 1     80
006600971007     D*------------
006700971007     D* DS GESTIONE TASTI FUNZIONALI
006800971007     D*------------
006900971007     D DSKEY         E DS
007000971007     D DEVDS1          DS
007100971007     D  KEY                  369    369
007200971007     D*------------
007300971007     D* DS "XMSG" - GESTIONE MESSAGGI
007400971007     D*------------
007500971007     D DSMSG         E DS
007600971007     D*------------------
007700971007     D* DS "XSRDA8" - CONTROLLA DATA (8)
007800971007     D*------------------
007900971007     D WLBDA8          DS                  INZ
008000971007     D  G08DAT                 1      8  0
008100971007     D  G08INV                 9     16  0
008200971007     D  G08ERR                17     17
008300971007     D  G08TGI                18     22  0
008400971007     D*------------
008500971007     D* DS DI PROCEDURA
008600971007     D*------------
008700971007     D DSTL73        E DS                  EXTNAME(TNTL73DS)
008800971007     D*------------
008900971007     D* ARCHITETTURA
009000971007     D*------------
009100971007     D KPJBA         E DS
009200971007     C*------------------------------------------------------------------------*
009300971007     C* MAIN LINES
009400971007     C*------------------------------------------------------------------------*
009500971007     C*
009600971007     C* SE NON CI SONO ERRORI, PROSEGUE
009700971007IF  1C     ERRL00        IFNE      '1'
009800971007     C*
009900971007     C* CARICA IL SFILE
010000971007     C                   EXSR      POSSFL
010100971007     C*
010200971007     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010300971007DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010400971007     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010500971007     C     FINL00        OREQ      '1'                                          *FINE PGM
010600971007     C*
010700971007     C* REIMPOSTA ERRORE DI PROCERURA
010800971007     C                   MOVEL     '0'           ERRL00
010900971007     C*
011000971007     C* EMETTE IL SFILE
011100971007     C                   EXSR      OUTSFL
011200971007     C*
011300971007     C* GESTIONE TASTI FUNZIONALI
011400971007     C                   EXSR      FUNCON
011500971007     C*
011600971007E   2C                   ENDDO
011700971007     C*
011800971007     C* CI SONO ERRORI IN ENTRATA
011900971007X   1C                   ELSE
012000971007     C                   MOVEL     '1'           D73ERR
012100971007     C                   MOVEL     DSMSMS        D73MSG
012200971007E   1C                   ENDIF
012300971007     C* FINE PGM
012400971007     C                   SETON                                        LR
012500971007     C*------------------------------------------------------------------------*
012600971007     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
012700971007     C*------------------------------------------------------------------------*
012800971007     C     F03GES        BEGSR
012900971007     C*
013000971007     C* IMPOSTA "PREMUTO F03"
013100971007     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
013200971007     C                   MOVEL     '1'           D73F03                         *DI PROCEDURA
013300971007     C*
013400971007     C                   ENDSR
013500971007     C*------------------------------------------------------------------------*
013600971007     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
013700971007     C*------------------------------------------------------------------------*
013800971007     C     F05GES        BEGSR
013900971007     C*
014000971007     C                   EXSR      POSSFL                                       *RICARICA SFILE
014100971007     C*
014200971007     C                   ENDSR
014300971007     C*------------------------------------------------------------------------*
014400971007     C* F06GES - GESTIONE F06 -> INSERIMENTO
014500971007     C*------------------------------------------------------------------------*
014600971007     C     F06GES        BEGSR
014700971007     C*
014800971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
014900971007     C                   Z-ADD     *ZEROS        DDAT
015000971007     C                   MOVEL     DEPOP0        DOP0
015100971007     C                   MOVEL     'F06'         DOP1
015200971007     C                   MOVEL     'TNTL75R'     DPGM              8
015300971007     C                   EXSR      LANPGM                                       *LANCIA PGM
015400971007     C                   EXSR      POSSFL                                       *RICARICA SFILE
015500971007E   1C                   ENDIF
015600971007     C*
015700971007     C                   ENDSR
015800971007     C*------------------------------------------------------------------------*
015900971007     C* F12GES - GESTIONE F12 -> RITORNO
016000971007     C*------------------------------------------------------------------------*
016100971007     C     F12GES        BEGSR
016200971007     C*
016300971007     C* IMPOSTA "PREMUTO F12"
016400971007     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
016500971007     C                   MOVEL     '1'           D73F12                         *DI PROCEDURA
016600971007     C*
016700971007     C                   ENDSR
016800971007     C*------------------------------------------------------------------------*
016900971007     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
017000971007     C*------------------------------------------------------------------------*
017100971007     C     F23GES        BEGSR
017200971007     C*
017300971007     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
017400971007     C*
017500971007     C                   ENDSR
017600971007     C*------------------------------------------------------------------------*
017700971007     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
017800971007     C*------------------------------------------------------------------------*
017900971007     C     F24GES        BEGSR
018000971007     C*
018100971007     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
018200971007     C*
018300971007     C                   ENDSR
018400971007     C*------------------------------------------------------------------------*
018500971007     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
018600971007     C*------------------------------------------------------------------------*
018700971007     C     RUPGES        BEGSR
018800971007     C*
018900971007     C                   EXSR      CARSFL                                       *LEGGE DATABASE
019000971007     C*
019100971007     C                   ENDSR
019200971007     C*------------------------------------------------------------------------*
019300971007     C* ENTGES - GESTIONE ENTER -> INVIO
019400971007     C*------------------------------------------------------------------------*
019500971007     C     ENTGES        BEGSR
019600971007     C*
019700971007     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
019800971007     C                   EXSR      NEWPOS
019900971007     C*
020000971007     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
020100971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
020200971007IF  2C     SWPOSI        IFNE      0
020300971007     C                   EXSR      POSSFL                                       *RICARICA SFILE
020400971007X   2C                   ELSE
020500971007IF  3C     SFLULT        IFNE      0
020600971007     C                   EXSR      ENTATT                                       *GESTIONE ENTER
020700971007     C*
020800971007     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
020900971007IF  4C     SWMODI        IFNE      0
021000971007     C                   EXSR      POSSFL
021100971007E   4C                   ENDIF
021200971007E   3C                   ENDIF
021300971007E   2C                   ENDIF
021400971007E   1C                   ENDIF
021500971007     C*
021600971007     C                   ENDSR
021700971007     C*------------------------------------------------------------------------*
021800971007     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
021900971007     C*------------------------------------------------------------------------*
022000971007     C     O02GES        BEGSR
022100971007     C*
022200971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022300971007     C                   Z-ADD     V1HDAT        DDAT
022400971007     C                   MOVEL     DEPOP0        DOP0
022500971007     C                   MOVEL     'O02'         DOP1
022600971007     C                   MOVEL     'TNTL75R'     DPGM
022700971007     C                   EXSR      LANPGM
022800971007     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
022900971007E   1C                   ENDIF
023000971007     C*
023100971007     C                   ENDSR
023200971007     C*------------------------------------------------------------------------*
023300971007     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
023400971007     C*------------------------------------------------------------------------*
023500971007     C     O03GES        BEGSR
023600971007     C*
023700971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023800971007     C                   Z-ADD     V1HDAT        DDAT
023900971007     C                   MOVEL     DEPOP0        DOP0
024000971007     C                   MOVEL     'O03'         DOP1
024100971007     C                   MOVEL     'TNTL76R'     DPGM
024200971007     C                   EXSR      LANPGM
024300971007     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
024400971007E   1C                   ENDIF
024500971007     C*
024600971007     C                   ENDSR
024700971007     C*------------------------------------------------------------------------*
024800971007     C* O04GES - GESTIONE OPZIONE 4=CANELLAZIONE
024900971007     C*------------------------------------------------------------------------*
025000971007     C     O04GES        BEGSR
025100971007     C*
025200971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025300971007     C                   Z-ADD     V1HDAT        DDAT
025400971007     C                   MOVEL     DEPOP0        DOP0
025500971007     C                   MOVEL     'O04'         DOP1
025600971007     C                   MOVEL     'TNTL75R'     DPGM
025700971007     C                   EXSR      LANPGM
025800971007     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
025900971007E   1C                   ENDIF
026000971007     C*
026100971007     C                   ENDSR
026200971007     C*------------------------------------------------------------------------*
026300971007     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
026400971007     C*------------------------------------------------------------------------*
026500971007     C     O05GES        BEGSR
026600971007     C*
026700971007     C                   Z-ADD     V1HDAT        DDAT
026800971007     C                   MOVEL     DEPOP0        DOP0
026900971007     C                   MOVEL     'O05'         DOP1
027000971007     C                   MOVEL     'TNTL75R'     DPGM
027100971007     C                   EXSR      LANPGM
027200971007     C*
027300971007     C                   ENDSR
027400971007     C*------------------------------------------------------------------------*
027500971007     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
027600971007     C*------------------------------------------------------------------------*
027700971007     C     NEWPOS        BEGSR
027800971007     C*
027900971007     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
028000971007     C*         1= RICHIESTO POSIZIONAMENTO
028100971007     C                   Z-ADD     0             SWPOSI            7 0
028200971007     C*
028300971007     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
028400971007     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
028500971007     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
028600971007     C                   Z-ADD     0             SWMODI            1 0
028700971007     C*
028800971007     C* REIMPOSTA INDICATORI DI ERRORE
028900971007     C                   SETOFF                                       8081
029000971007     C*
029100971007     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
029200971007IF  1C     V1CDAO        IFNE      DEPDAO
029300971007     C     V1CDAS        ORNE      DEPDAS
029400971007     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
029500971007IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
029600971007     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
029700971007     C                   Z-ADD     V1CDAO        DEPDAO                         *DEPOSITI
029800971007     C                   Z-ADD     V1CDAS        DEPDAS
029900971007E   2C                   ENDIF
030000971007E   1C                   ENDIF
030100971007     C*
030200971007     C                   ENDSR
030300971007     C*------------------------------------------------------------------------*
030400971007     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
030500971007     C*------------------------------------------------------------------------*
030600971007     C     SETFIL        BEGSR
030700971007     C*
030800971007     C* POSIZIONAMENTO SUL FILE
030900971007     C                   Z-ADD     DAOV1C        KTCDAT
031000011019     C     KEYTCG        SETLL     TITCG01L                           99
031100971007IF  1C     *IN99         IFEQ      *OFF
031200971007     C                   EXSR      LETFIL
031300971007E   1C                   ENDIF
031400971007IF  1C     *IN99         IFEQ      *ON                                          *FINE FILE
031500971007     C                   Z-ADD     1             SFLEOF                         *FINE CARICAMENTO
031600971007E   1C                   ENDIF
031700971007     C*
031800971007     C                   ENDSR
031900971007     C*------------------------------------------------------------------------*
032000971007     C* LETFIL - LETTURA RECORD
032100971007     C*------------------------------------------------------------------------*
032200971007     C     LETFIL        BEGSR
032300971007     C*
032400971007     C* LEGGE FINO A:
032500971007     C*     - FINE FILE
032600971007     C*     - TROVATO RECORD VALIDO
032700971007     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
032800971007DO  1C     *IN99         DOWEQ     *OFF
032900971007     C     WRECOK        ANDEQ     'N'
033000011019     C                   READ      TITCG01L                               99
033100971007IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
033200971007     C                   Z-ADD     1             SFLEOF                         *FINE CARICAMENTO
033300971007X   2C                   ELSE
033400971007     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
033500971007E   2C                   ENDIF
033600971007E   1C                   ENDDO
033700971007     C*
033800971007     C                   ENDSR
033900971007     C*------------------------------------------------------------------------*
034000971007     C* CHKREC - CONTROLLA VALIDITA' RECORD
034100971007     C*------------------------------------------------------------------------*
034200971007     C     CHKREC        BEGSR
034300971007     C*
034400971007     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
034500971007     C*
034600971007     C* SELEZIONE PER DATA
034700971007IF  1C     V1CDAS        IFGT      *ZEROS
034800971007IF  2C     TCGDAT        IFNE      DASV1C
034900971007     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALDIO
035000971007E   2C                   ENDIF
035100971007E   1C                   ENDIF
035200971007     C*
035300971007     C                   ENDSR
035400971007     C*------------------------------------------------------------------------*
035500971007     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
035600971007     C*------------------------------------------------------------------------*
035700971007     C     IRESFL        BEGSR
035800971007     C*
035900971007     C                   MOVEL     *BLANKS       OPZKEY                         *OPZIONE
036000971007     C                   Z-ADD     TCGDAT        V1HDAT                         *HIDDEN
036100971007     C                   Z-ADD     TCGDAT        G08INV                         *CAMPI
036200971007     C                   Z-ADD     *ZEROS        G08DAT
036300971007     C                   MOVEL     '3'           G08ERR
036400971007     C                   CALL      'XSRDA8'
036500971007     C                   PARM                    WLBDA8
036600971007     C                   Z-ADD     G08DAT        V1SDAT
036700011019     C                   MOVEL     TCGDIV        V1SDIV
036800971007     C                   Z-ADD     *ZEROS        V1SRIC
036900971007     C                   ADD       TCGRCI        V1SRIC
037000971007     C                   Z-ADD     *ZEROS        V1SCOS
037100971007     C                   ADD       TCGCCA        V1SCOS
037200971007     C                   ADD       TCGCSA        V1SCOS
037300971007     C                   ADD       TCGCAA        V1SCOS
037400971007     C                   ADD       TCGCCV        V1SCOS
037500971007     C                   ADD       TCGCSV        V1SCOS
037600971007     C                   ADD       TCGCAV        V1SCOS
037700971007     C                   ADD       TCGCOP        V1SCOS
037800971007     C                   ADD       TCGCSP        V1SCOS
037900971007     C                   ADD       TCGCIM        V1SCOS
038000971007     C                   ADD       TCGCFA        V1SCOS
038100971007     C                   ADD       TCGCVI        V1SCOS
038200971007     C                   ADD       TCGCCT        V1SCOS
038300971007     C                   ADD       TCGCTM        V1SCOS
038400971007     C                   ADD       TCGCVS        V1SCOS
038500971007     C                   ADD       TCGCTE        V1SCOS
038600971007     C                   ADD       TCGCVA        V1SCOS
038700971007     C*
038800971007     C                   ENDSR
038900971007     C*------------------------------------------------------------------------*
039000971007     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
039100971007     C*------------------------------------------------------------------------*
039200971007     C     CHKPAR        BEGSR
039300971007     C*
039400971007     C* DATA ORDINAMENTO
039500971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039600971007     C     *LIKE         DEFINE    V1CDAO        DAOV1C
039700971007     C                   Z-ADD     *ZEROS        DAOV1C
039800971007IF  2C     V1CDAO        IFGT      *ZEROS
039900971007     C                   Z-ADD     V1CDAO        WDA8
040000971007     C                   Z-ADD     *ZEROS        DA8W
040100971007     C                   EXSR      CHKDA8
040200971007     C                   Z-ADD     WDA8          V1CDAO
040300971007     C                   Z-ADD     DA8W          DAOV1C
040400971007IF  3C     ERRL00        IFEQ      '1'
040500971007     C                   SETON                                        80
040600971007     C                   MOVEL     MSG(10)       DSMSMS
040700971007E   3C                   ENDIF
040800971007E   2C                   ENDIF
040900971007E   1C                   ENDIF
041000971007     C*
041100971007     C* DATA SELEZIONE
041200971007IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041300971007     C     *LIKE         DEFINE    V1CDAS        DASV1C
041400971007     C                   Z-ADD     *ZEROS        DASV1C
041500971007IF  2C     V1CDAS        IFGT      *ZEROS
041600971007     C                   Z-ADD     V1CDAS        WDA8
041700971007     C                   Z-ADD     *ZEROS        DA8W
041800971007     C                   EXSR      CHKDA8
041900971007     C                   Z-ADD     WDA8          V1CDAS
042000971007     C                   Z-ADD     DA8W          DASV1C
042100971007IF  3C     ERRL00        IFEQ      '1'
042200971007     C                   SETON                                        81
042300971007     C                   MOVEL     MSG(10)       DSMSMS
042400971007E   3C                   ENDIF
042500971007E   2C                   ENDIF
042600971007E   1C                   ENDIF
042700971007     C*
042800971007     C                   ENDSR
042900971007     C*------------------------------------------------------------------------*
043000971007     C* CHKDA8 - CONTROLLA DATA (8)
043100971007     C*------------------------------------------------------------------------*
043200971007     C     CHKDA8        BEGSR
043300971007     C*
043400971007     C     *LIKE         DEFINE    V1CDAO        WDA8
043500971007     C     *LIKE         DEFINE    V1CDAO        DA8W
043600971007     C*
043700971007     C                   Z-ADD     WDA8          G08DAT
043800971007     C                   Z-ADD     *ZEROS        G08INV
043900971007     C                   MOVEL     '0'           G08ERR
044000971007     C                   CALL      'XSRDA8'
044100971007     C                   PARM                    WLBDA8
044200971007     C                   Z-ADD     G08DAT        WDA8
044300971007     C                   Z-ADD     G08INV        DA8W
044400971007IF  1C     G08ERR        IFEQ      '1'
044500971007     C                   MOVEL     '1'           ERRL00
044600971007E   1C                   ENDIF
044700971007     C*
044800971007     C                   ENDSR
044900971007     C*------------------------------------------------------------------------*
045000971007     C* LANPGM - LANCIA UN PROGRAMMA
045100971007     C*------------------------------------------------------------------------*
045200971007     C     LANPGM        BEGSR
045300971007     C*
045400971007     C* IMPOSTA LA DS DI PROCEDURA
045500971007     C                   MOVEL     DOP0          D73OP0                         *LIVELLO PROCEDURA
045600971007     C                   MOVEL     DOP1          D73OP1                         *LIVELLO PGM
045700971007     C                   MOVEL     '0'           D73F03                         *PREMUTO F03=FINE
045800971007     C                   MOVEL     '0'           D73F12                         *PREMUTO F12=RITORNO
045900971007     C                   MOVEL     '0'           D73ERR                         *'1'=ERRORE
046000971007     C                   MOVEL     *BLANKS       D73MSG                         *MESSAGGIO DI RITORN
046100971007     C                   Z-ADD     DDAT          D73DAT                         *INPUT
046200971007     C                   CALL      DPGM
046300971007     C                   PARM                    KPJBA
046400971007     C                   PARM                    DSTL73
046500971007     C*
046600971007     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
046700971007IF  1C     D73F03        IFEQ      '1'                                          *PREMUTO F03
046800971007     C                   MOVEL     '1'           ERRL00                         *ERRORE
046900971007     C                   MOVEL     '1'           F03L00                         *FINE PGM
047000971007X   1C                   ELSE                                                   *NON PREMUTO F03
047100971007     C                   MOVEL     D73MSG        DSMSMS                         *MESSAGGIO DI RITORN
047200971007E   1C                   ENDIF
047300971007     C*
047400971007     C                   ENDSR
047500971007     C*------------------------------------------------------------------------*
047600971007     C* CARTAB - CARICA LE TABELLE OCCORRENTI
047700971007     C*------------------------------------------------------------------------*
047800971007     C     CARTAB        BEGSR
047900971007     C*
048000971007     C                   ENDSR
048100971007     C*------------------------------------------------------------------------*
048200971007     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
048300971007     C*------------------------------------------------------------------------*
048400971007     C     CHKFOP        BEGSR
048500971007     C*
048600971007     C* SE UTENTE IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
048700971007IF  1C     DEPOP0        IFEQ      'V02'
048800971007     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
048900971007     C                   MOVEL     'S'           OPDATT
049000971007     C                   MOVEL     OPDRIG        OPD(2)
049100971007     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
049200971007     C                   MOVEL     'S'           OPDATT
049300971007     C                   MOVEL     OPDRIG        OPD(3)
049400971007     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
049500971007     C                   MOVEL     'S'           OPDATT
049600971007     C                   MOVEL     OPDRIG        OPD(4)
049700971007     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
049800971007     C                   MOVEL     'S'           FUDATT
049900971007     C                   MOVEL     FUDRIG        FUD(6)
050000971007E   1C                   ENDIF
050100971007     C*
050200971007     C* GESTIONE OPZIONI A VIDEO
050300971007     C                   Z-ADD     *ZEROS        OPZGRU            5 0          *INDICE X OPZIONI
050400971007     C                   EXSR      OPZGES
050500971007     C*
050600971007     C* GESTIONE TASTI FUNZIONALI A VIDEO
050700971007     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
050800971007     C                   EXSR      FUNGES
050900971007     C*
051000971007     C                   ENDSR
051100971007     C*------------------------------------------------------------------------*
051200971007     C* *INZSR - OPERAZIONI INIZIALI
051300971007     C*------------------------------------------------------------------------*
051400971007     C     *INZSR        BEGSR
051500971007     C*---
051600971007     C* RICEVIMENTO PARAMETRI
051700971007     C*---
051800971007     C     *ENTRY        PLIST
051900971007     C                   PARM                    KPJBA
052000971007     C                   PARM                    DSTL73
052100971007     C*
052200971007     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
052300971007     C     XMSGPA        PLIST
052400971007     C                   PARM                    DSMSMS
052500971007     C                   PARM                    DSMSPG
052600971007     C                   PARM                    DSMSKE
052700971007     C                   PARM                    DSMSER
052800971007     C*
052900971007     C* IMPOSTA LA DS -TASTI PREMUTI-
053000971007     C                   CALL      'XKEY'
053100971007     C                   PARM                    DSKEY
053200971007     C*---
053300971007     C* VARIABILI NON RIFERITE AL DATA BASE
053400971007     C*---
053500971007     C     *LIKE         DEFINE    SFLNBR        SFLPAG
053600971007     C     *LIKE         DEFINE    SFLPAG        WFLPAG
053700971007     C     *LIKE         DEFINE    SFLNBR        SFLEOF
053800971007     C     *LIKE         DEFINE    SFLNBR        SFLERR
053900971007     C     *LIKE         DEFINE    SFLNBR        SFLULT
054000971007     C     *LIKE         DEFINE    SFLNBR        WFLULT
054100971007     C     *LIKE         DEFINE    SFLNBR        MSGULT
054200971007     C     *LIKE         DEFINE    SFLNBR        SFLRIG
054300971007     C     *LIKE         DEFINE    FUDFUN        FUNKEY
054400971007     C     *LIKE         DEFINE    D73OP0        DEPOP0                         *DI DEPOSITO
054500971007     C     *LIKE         DEFINE    D73OP1        DEPOP1
054600971007     C     *LIKE         DEFINE    D73OP0        DOP0                           *DI LAVORO
054700971007     C     *LIKE         DEFINE    D73OP1        DOP1
054800971007     C     *LIKE         DEFINE    D73DAT        DDAT
054900971007     C*---
055000971007     C* VARIABILI DI CONTROLLO
055100971007     C*---
055200971007     C                   MOVEL     '0'           FINL00            1            *FINE PGM
055300971007     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
055400971007     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
055500971007     C                   MOVEL     '0'           ERRL00            1            *ERRORE
055600971007     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
055700971007     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
055800971007     C*
055900971007     C* IMPOSTA LA DS -INVIO MESSAGGI-
056000971007     C                   MOVEL     'TNTL74R'     DSMSPG
056100971007     C                   MOVEL     'TNTL74R'     MSGQUE
056200971007     C                   MOVEL     *BLANKS       MSGKEY
056300971007     C                   MOVEL     *BLANKS       DSMSMS
056400971007     C                   MOVEL     *BLANKS       DSMSKE
056500971007     C                   Z-ADD     *ZEROS        DSMSER
056600971007     C*---
056700971007     C* VARIABILI RIFERITE AL DATA BASE
056800971007     C*---
056900011019     C     *LIKE         DEFINE    TCGDAT        KTCDAT                         *TITCG00F
057000971007     C     *LIKE         DEFINE    TCGDAT        DEPDAO                         *DEPOSITI
057100971007     C     *LIKE         DEFINE    TCGDAT        DEPDAS
057200971007     C*---
057300971007     C* CHIAVI DI LETTURA
057400971007     C*---
057500011019     C* LETTURA TITCG01L - COMPLETA
057600971007     C     KEYTCG        KLIST
057700971007     C                   KFLD                    KTCDAT                         *DATA
057800971007     C*
057900971007     C* CARICA LE TABELLE OCCORRENTI
058000971007     C                   EXSR      CARTAB
058100971007     C*
058200971007     C* DEPOSITA LE OPZIONI DI ENTRATA
058300971007     C                   MOVEL     D73OP0        DEPOP0                         *LIVELLO PROCEDURA
058400971007     C                   MOVEL     D73OP1        DEPOP1                         *LIVELLO PROGRAMMA
058500971007     C*
058600971007     C* CALCOLA LA DATA CORRENTE
058700971007     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
058800971007     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
058900971007     C                   Z-ADD     WN8           G08DAT
059000971007     C                   Z-ADD     *ZEROS        G08INV
059100971007     C                   MOVEL     '0'           G08ERR
059200971007     C                   CALL      'XSRDA8'
059300971007     C                   PARM                    WLBDA8
059400971007     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
059500971007     C*
059600971007     C* IMPOSTA I PARAMETRI DI INGRESSO
059700971007     C                   Z-ADD     WN8           V1CDAO                         *VIDEO
059800971007     C                   Z-ADD     DATCOR        DAOV1C
059900971007     C                   Z-ADD     *ZEROS        V1CDAS
060000971007     C                   Z-ADD     *ZEROS        DASV1C
060100971007     C                   Z-ADD     DATCOR        DEPDAO                         *DEPOSITI
060200971007     C                   Z-ADD     *ZEROS        DEPDAS
060300971007     C*
060400971007     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
060500971007     C                   EXSR      CHKFOP
060600971007     C*
060700971007     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
060800971007IF  1C     DEPOP0        IFEQ      'V02'
060900971007     C                   MOVEL     DEC(1)        VIDING
061000971007E   1C                   ENDIF
061100971007IF  1C     DEPOP0        IFEQ      'V05'
061200971007     C                   MOVEL     DEC(2)        VIDING
061300971007E   1C                   ENDIF
061400971007     C*
061500971007     C                   ENDSR
061600971007     C*------------------------------------------------------------------------*
061700971007     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
061800971007     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
061900971007     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
062000971007     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
062100971007     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
062200971007     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
062300971007     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
062400971007     C*------------------------------------------------------------------------*
062500971007     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
062600971007     C*------------------------------------------------------------------------*
062700971007     C     POSSFL        BEGSR
062800971007     C*
062900971007     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
063000971007     C                   EXSR      CLRSFL
063100971007     C*
063200971007     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
063300971007     C                   EXSR      SETFIL
063400971007     C*
063500971007     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
063600971007     C     SFLEOF        IFEQ      0
063700971007     C                   EXSR      CARSFL
063800971007     C                   END
063900971007     C*
064000971007     C                   ENDSR
064100971007     C*------------------------------------------------------------------------*
064200971007     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
064300971007     C*------------------------------------------------------------------------*
064400971007     C     ENTATT        BEGSR
064500971007     C*
064600971007     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
064700971007     C                   Z-ADD     0             OPZOK             1 0
064800971007     C*
064900971007     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
065000971007     C                   Z-ADD     0             SFLVAR            5 0
065100971007     C*
065200971007     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
065300971007     C                   Z-ADD     0             SFLERR
065400971007     C*
065500971007     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
065600971007     C                   Z-ADD     SFLULT        WFLULT
065700971007     C*
065800971007     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
065900971007     C                   READC     TL74D05                                98
066000971007     C     *IN98         DOWEQ     '0'
066100971007     C*
066200971007     C* ADD CONTATORE DI RIGHE SFL VARIATE
066300971007     C                   ADD       1             SFLVAR
066400971007     C*
066500971007     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
066600971007     C     OPZOK         IFEQ      0
066700971007     C*
066800971007     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
066900971007     C     OPZKEY        IFNE      *BLANKS
067000971007     C                   EXSR      OPZCON
067100971007     C                   END
067200971007     C*
067300971007     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
067400971007     C*            2. PULIZIA SCELTA IN SFL
067500971007     C     OPZOK         IFEQ      0
067600971007     C                   MOVEL     '0'           *IN41
067700971007     C                   MOVEL     *BLANKS       OPZKEY
067800971007     C                   ELSE
067900971007     C*
068000971007     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
068100971007     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
068200971007     C                   MOVEL     '1'           *IN41
068300971007     C                   Z-ADD     SFLULT        SFLERR
068400971007     C                   END
068500971007     C                   ELSE
068600971007     C*
068700971007     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
068800971007     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
068900971007     C                   MOVEL     '1'           *IN41
069000971007     C                   END                                                    OPZOK=0
069100971007     C*
069200971007     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
069300971007     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
069400971007     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
069500971007     C     OPZOK         IFNE      0
069600971007     C                   Z-ADD     SFLERR        SFLNBR
069700971007     C                   ELSE
069800971007     C                   Z-ADD     SFLULT        SFLNBR
069900971007     C                   END
070000971007     C*
070100971007     C* AGGIORNO RECORD SFL
070200971007     C                   UPDATE    TL74D05
070300971007     C*
070400971007     C                   READC     TL74D05                                98
070500971007     C                   END                                                    DO WHILE
070600971007     C*
070700971007     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
070800971007     C                   Z-ADD     WFLULT        SFLULT
070900971007     C*
071000971007     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
071100971007     C     SFLVAR        IFEQ      0
071200971007     C*---                 MOVEL'1'       FINL00
071300971007     C                   END
071400971007     C*
071500971007     C                   ENDSR
071600971007     C*------------------------------------------------------------------------*
071700971007     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
071800971007     C*------------------------------------------------------------------------*
071900971007     C     CLRSFL        BEGSR
072000971007     C*
072100971007     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
072200971007     C* (0=NON FINE FILE  1=FINE FILE)
072300971007     C                   Z-ADD     0             SFLEOF
072400971007     C*
072500971007     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
072600971007     C                   Z-ADD     0             SFLULT
072700971007     C*
072800971007     C* ACCENSIONE INDICATORE PER CLEAR SFL
072900971007     C                   MOVEL     '1'           *IN40
073000971007     C                   WRITE     TL74D06
073100971007     C                   MOVEL     '0'           *IN40
073200971007     C*
073300971007     C                   ENDSR
073400971007     C*------------------------------------------------------------------------*
073500971007     C* CARSFL - CARICAMENTO SFL
073600971007     C*------------------------------------------------------------------------*
073700971007     C     CARSFL        BEGSR
073800971007     C*
073900971007     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
074000971007     C     SFLULT        IFEQ      0
074100971007     C                   EXSR      SETFIL
074200971007     C                   END
074300971007     C*
074400971007     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
074500971007     C                   Z-ADD     SFLULT        WFLULT
074600971007     C*
074700971007     C* CICLO:
074800971007     C*    -CARICO NUMERO DI RIGHE=SFLPAG
074900971007     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
075000971007     C                   Z-ADD     1             SFLRIG
075100971007     C     SFLRIG        DOWLE     SFLPAG
075200971007     C     SFLEOF        ANDEQ     0
075300971007     C*
075400971007     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
075500971007     C                   MOVEL     *BLANKS       OPZKEY
075600971007     C*
075700971007     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
075800971007     C                   EXSR      IRESFL
075900971007     C*
076000971007     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
076100971007     C                   ADD       1             SFLULT
076200971007     C*
076300971007     C* SCRITTURA RECORD SFL
076400971007     C                   WRITE     TL74D05
076500971007     C                   ADD       1             SFLRIG
076600971007     C*
076700971007     C* LETTURA FILE DA CARICARE
076800971007     C                   EXSR      LETFIL
076900971007     C                   END                                                    DO WHILE
077000971007     C*
077100971007     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
077200971007     C     WFLULT        IFNE      SFLULT
077300971007     C                   Z-ADD     WFLULT        SFLNBR
077400971007     C                   ADD       1             SFLNBR
077500971007     C                   END
077600971007     C*
077700971007     C                   ENDSR
077800971007     C*------------------------------------------------------------------------*
077900971007     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
078000971007     C*------------------------------------------------------------------------*
078100971007     C     OUTSFL        BEGSR
078200971007     C*
078300971007     C* WRITE RECORD TESTATA VIDEO
078400971007     C                   WRITE     TL74D01
078500971007     C*
078600971007     C* VISUALIZZAZIONE MESSAGGIO
078700971007     C                   EXSR      SNDMSG
078800971007     C*
078900971007     C* WRITE RECORD OPZIONI VIDEO
079000971007     C                   WRITE     TL74D02
079100971007     C*
079200971007     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
079300971007     C                   WRITE     TL74D03
079400971007     C*
079500971007     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
079600971007     C                   WRITE     TL74D04
079700971007     C*
079800971007     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
079900971007     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
080000971007     C     SFLEOF        IFEQ      0
080100971007     C                   MOVEL     '0'           *IN42
080200971007     C                   ELSE
080300971007     C*
080400971007     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
080500971007     C                   MOVEL     '1'           *IN42
080600971007     C                   END
080700971007     C*
080800971007     C* WRITE RECORD TASTI FUNZIONALI
080900971007     C                   WRITE     TL74D07
081000971007     C*
081100971007     C* CASO DI SFL VUOTO
081200971007     C     SFLULT        IFLE      0
081300971007     C*
081400971007     C*    - WRITE RECORD VUOTO
081500971007     C                   WRITE     TL74D0V
081600971007     C                   READ      TL74D0V                                99
081700971007     C*
081800971007     C                   ELSE
081900971007     C*
082000971007     C* CASO DI SFL CON RECORDS
082100971007     C*
082200971007     C*    - WRITE RECORD CONTROLLO SFL
082300971007     C                   WRITE     TL74D06
082400971007     C                   READ      TL74D06                                99
082500971007     C                   END
082600971007     C*
082700971007     C* READ RECORD PER POSIZIONAMEMTO VIDEO
082800971007     C                   READ      TL74D03                                99
082900971007     C*
083000971007     C                   ENDSR
083100971007     C*------------------------------------------------------------------------*
083200971007     C* SNDMSG - MANDA MESSAGGIO
083300971007     C*------------------------------------------------------------------------*
083400971007     C     SNDMSG        BEGSR
083500971007     C*
083600971007     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
083700971007     C                   MOVEL     '1'           *IN40
083800971007     C                   WRITE     TL74DMC
083900971007     C                   MOVEL     '0'           *IN40
084000971007     C*
084100971007     C* VISUALIZZAZIONE MESSAGGIO
084200971007     C                   Z-ADD     0             MSGULT
084300971007     C                   CALL      'XMSG'        XMSGPA
084400971007     C     DSMSER        IFEQ      0
084500971007     C                   MOVEL     DSMSKE        MSGKEY
084600971007     C                   END
084700971007     C                   Z-ADD     1             MSGULT
084800971007     C                   WRITE     TL74DMS                                      SFL MESS
084900971007     C                   WRITE     TL74DMC                                      CONTR MESS
085000971007     C*
085100971007     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
085200971007     C                   MOVEL     *BLANKS       DSMSMS
085300971007     C*
085400971007     C                   ENDSR
085500971007     C*------------------------------------------------------------------------*
085600971007     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
085700971007     C*------------------------------------------------------------------------*
085800971007     C     FUNCON        BEGSR
085900971007     C*
086000971007     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
086100971007     C                   EXSR      FUNVAR
086200971007     C*
086300971007     C* PULISCO CAMPI DI LAVORO
086400971007     C                   Z-ADD     0             I                 5 0
086500971007     C                   Z-ADD     0             W                 5 0
086600971007     C*
086700971007     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
086800971007     C     FUNKEY        IFNE      *BLANKS
086900971007     C     1             DO        24            I
087000971007     C                   MOVEL     FUD(I)        FUDRIG
087100971007     C*
087200971007     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
087300971007     C     FUNKEY        IFEQ      FUDFUN
087400971007     C     FUDATT        ANDEQ     'S'
087500971007     C                   Z-ADD     I             W
087600971007     C                   END
087700971007     C                   END                                                    DO WHILE
087800971007     C*
087900971007     C* FUNZIONE NON ATTIVA (ERRORE)
088000971007     C     W             IFEQ      0
088100971007     C*
088200971007     C* EMISSIONE MESSAGGIO DI ERRORE
088300971007     C                   MOVEL     MSG(2)        DSMSMS
088400971007     C                   END                                                    W=0
088500971007     C                   END                                                    FUNKEY NE *BLANKS
088600971007     C*
088700971007     C* FUNZIONE ATTIVA (PROCEDI ...)
088800971007     C     W             IFNE      0
088900971007     C     FUNKEY        OREQ      *BLANKS
089000971007     C*
089100971007     C     FUNKEY        IFEQ      *BLANKS
089200971007     C                   EXSR      ENTGES
089300971007     C                   END
089400971007     C     FUNKEY        IFEQ      '03'
089500971007     C                   EXSR      F03GES
089600971007     C                   END
089700971007     C     FUNKEY        IFEQ      '05'
089800971007     C                   EXSR      F05GES
089900971007     C                   END
090000971007     C     FUNKEY        IFEQ      '06'
090100971007     C                   EXSR      F06GES
090200971007     C                   END
090300971007     C     FUNKEY        IFEQ      '12'
090400971007     C                   EXSR      F12GES
090500971007     C                   END
090600971007     C     FUNKEY        IFEQ      '23'
090700971007     C                   EXSR      F23GES
090800971007     C                   END
090900971007     C     FUNKEY        IFEQ      '24'
091000971007     C                   EXSR      F24GES
091100971007     C                   END
091200971007     C     FUNKEY        IFEQ      'RU'
091300971007     C                   EXSR      RUPGES
091400971007     C                   END
091500971007     C                   END
091600971007     C*
091700971007     C                   ENDSR
091800971007     C*------------------------------------------------------------------------*
091900971007     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
092000971007     C*------------------------------------------------------------------------*
092100971007     C     FUNVAR        BEGSR
092200971007     C*
092300971007     C* FUNKEY=*BLANKS SIGNIFICA ENTER
092400971007     C     KEY           IFEQ      ENTER
092500971007     C                   MOVEL     *BLANKS       FUNKEY
092600971007     C                   END
092700971007     C     KEY           IFEQ      F1
092800971007     C                   MOVEL     '01'          FUNKEY
092900971007     C                   END
093000971007     C     KEY           IFEQ      F2
093100971007     C                   MOVEL     '02'          FUNKEY
093200971007     C                   END
093300971007     C     KEY           IFEQ      F3
093400971007     C                   MOVEL     '03'          FUNKEY
093500971007     C                   END
093600971007     C     KEY           IFEQ      F4
093700971007     C                   MOVEL     '04'          FUNKEY
093800971007     C                   END
093900971007     C     KEY           IFEQ      F5
094000971007     C                   MOVEL     '05'          FUNKEY
094100971007     C                   END
094200971007     C     KEY           IFEQ      F6
094300971007     C                   MOVEL     '06'          FUNKEY
094400971007     C                   END
094500971007     C     KEY           IFEQ      F7
094600971007     C                   MOVEL     '07'          FUNKEY
094700971007     C                   END
094800971007     C     KEY           IFEQ      F8
094900971007     C                   MOVEL     '08'          FUNKEY
095000971007     C                   END
095100971007     C     KEY           IFEQ      F9
095200971007     C                   MOVEL     '09'          FUNKEY
095300971007     C                   END
095400971007     C     KEY           IFEQ      F10
095500971007     C                   MOVEL     '10'          FUNKEY
095600971007     C                   END
095700971007     C     KEY           IFEQ      F11
095800971007     C                   MOVEL     '11'          FUNKEY
095900971007     C                   END
096000971007     C     KEY           IFEQ      F12
096100971007     C                   MOVEL     '12'          FUNKEY
096200971007     C                   END
096300971007     C     KEY           IFEQ      F13
096400971007     C                   MOVEL     '13'          FUNKEY
096500971007     C                   END
096600971007     C     KEY           IFEQ      F14
096700971007     C                   MOVEL     '14'          FUNKEY
096800971007     C                   END
096900971007     C     KEY           IFEQ      F15
097000971007     C                   MOVEL     '15'          FUNKEY
097100971007     C                   END
097200971007     C     KEY           IFEQ      F16
097300971007     C                   MOVEL     '16'          FUNKEY
097400971007     C                   END
097500971007     C     KEY           IFEQ      F17
097600971007     C                   MOVEL     '17'          FUNKEY
097700971007     C                   END
097800971007     C     KEY           IFEQ      F18
097900971007     C                   MOVEL     '18'          FUNKEY
098000971007     C                   END
098100971007     C     KEY           IFEQ      F19
098200971007     C                   MOVEL     '19'          FUNKEY
098300971007     C                   END
098400971007     C     KEY           IFEQ      F20
098500971007     C                   MOVEL     '20'          FUNKEY
098600971007     C                   END
098700971007     C     KEY           IFEQ      F21
098800971007     C                   MOVEL     '21'          FUNKEY
098900971007     C                   END
099000971007     C     KEY           IFEQ      F22
099100971007     C                   MOVEL     '22'          FUNKEY
099200971007     C                   END
099300971007     C     KEY           IFEQ      F23
099400971007     C                   MOVEL     '23'          FUNKEY
099500971007     C                   END
099600971007     C     KEY           IFEQ      F24
099700971007     C                   MOVEL     '24'          FUNKEY
099800971007     C                   END
099900971007     C     KEY           IFEQ      ROLLUP
100000971007     C                   MOVEL     'RU'          FUNKEY
100100971007     C                   END
100200971007     C     KEY           IFEQ      HELP
100300971007     C                   MOVEL     'HP'          FUNKEY
100400971007     C                   END
100500971007     C*
100600971007     C                   ENDSR
100700971007     C*------------------------------------------------------------------------*
100800971007     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
100900971007     C*------------------------------------------------------------------------*
101000971007     C     FUNGES        BEGSR
101100971007     C*
101200971007     C* PULISCO CAMPI DI LAVORO
101300971007     C                   Z-ADD     0             W                 5 0
101400971007     C*
101500971007     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
101600971007     C* E CARICO TABELLA FUNZIONI ATTIVE
101700971007     C     1             DO        24            I
101800971007     C                   MOVEL     FUD(I)        FUDRIG
101900971007     C*
102000971007     C* PULIZIA FUNZIONI PRECEDENTI
102100971007     C                   MOVEL     *BLANKS       FUV(I)
102200971007     C*
102300971007     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
102400971007     C* (SALTA F23 E F24)
102500971007     C     FUDATT        IFEQ      'S'
102600971007     C     FUDFUN        ANDNE     '23'
102700971007     C     FUDFUN        ANDNE     '24'
102800971007     C                   ADD       1             W
102900971007     C                   MOVEL     FUDDES        FUV(W)
103000971007     C                   END
103100971007     C                   END                                                    DO WHILE
103200971007     C*
103300971007     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
103400971007     C* ALTRIMENTI DISATTIVO F24
103500971007     C     1             DO        24            I
103600971007     C                   MOVEL     FUD(I)        FUDRIG
103700971007     C*
103800971007     C     FUDFUN        IFEQ      '24'
103900971007     C     W             IFGT      6
104000971007     C                   MOVEL     'S'           FUDATT
104100971007     C                   ELSE
104200971007     C                   MOVEL     *BLANKS       FUDATT
104300971007     C                   END                                                    W GT 8
104400971007     C                   MOVEL     FUDRIG        FUD(I)
104500971007     C                   END
104600971007     C                   END                                                    DO WHILE
104700971007     C*
104800971007     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
104900971007     C     W             DIV       6             I
105000971007     C                   MVR                     Z
105100971007     C*
105200971007     C     Z             IFNE      0
105300971007     C     I             ADD       1             Z
105400971007     C                   ELSE
105500971007     C                   Z-ADD     I             Z
105600971007     C                   END
105700971007     C*
105800971007     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
105900971007     C                   ADD       1             FUNGRU
106000971007     C     FUNGRU        IFGT      Z
106100971007     C     FUNGRU        ORLT      1
106200971007     C                   Z-ADD     1             FUNGRU
106300971007     C                   END
106400971007     C*
106500971007     C* PRIMO GRUPPO DI 6 FUNZIONI
106600971007     C     FUNGRU        IFEQ      1
106700971007     C                   MOVEL     FUV(1)        FUN001
106800971007     C                   MOVEL     FUV(2)        FUN002
106900971007     C                   MOVEL     FUV(3)        FUN003
107000971007     C                   MOVEL     FUV(4)        FUN004
107100971007     C                   MOVEL     FUV(5)        FUN005
107200971007     C                   MOVEL     FUV(6)        FUN006
107300971007     C                   END
107400971007     C*
107500971007     C* SECONDO GRUPPO DI 6 FUNZIONI
107600971007     C     FUNGRU        IFEQ      2
107700971007     C                   MOVEL     FUV(7)        FUN001
107800971007     C                   MOVEL     FUV(8)        FUN002
107900971007     C                   MOVEL     FUV(9)        FUN003
108000971007     C                   MOVEL     FUV(10)       FUN004
108100971007     C                   MOVEL     FUV(11)       FUN005
108200971007     C                   MOVEL     FUV(12)       FUN006
108300971007     C                   END
108400971007     C*
108500971007     C* TERZO GRUPPO DI 6 FUNZIONI
108600971007     C     FUNGRU        IFEQ      3
108700971007     C                   MOVEL     FUV(13)       FUN001
108800971007     C                   MOVEL     FUV(14)       FUN002
108900971007     C                   MOVEL     FUV(15)       FUN003
109000971007     C                   MOVEL     FUV(16)       FUN004
109100971007     C                   MOVEL     FUV(17)       FUN005
109200971007     C                   MOVEL     FUV(18)       FUN006
109300971007     C                   END
109400971007     C*
109500971007     C* QUARTO GRUPPO DI 6 FUNZIONI
109600971007     C     FUNGRU        IFEQ      4
109700971007     C                   MOVEL     FUV(19)       FUN001
109800971007     C                   MOVEL     FUV(20)       FUN002
109900971007     C                   MOVEL     FUV(21)       FUN003
110000971007     C                   MOVEL     FUV(22)       FUN004
110100971007     C                   MOVEL     *BLANKS       FUN005
110200971007     C                   MOVEL     *BLANKS       FUN006
110300971007     C                   END
110400971007     C*
110500971007     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
110600971007     C                   MOVEL     *BLANKS       FUN007
110700971007     C                   MOVEL     *BLANKS       FUN008
110800971007     C*
110900971007     C     1             DO        24            I
111000971007     C                   MOVEL     FUD(I)        FUDRIG
111100971007     C     FUDFUN        IFEQ      '23'
111200971007     C     FUDATT        ANDEQ     'S'
111300971007     C                   MOVEL     FUDDES        FUN007
111400971007     C                   END
111500971007     C     FUDFUN        IFEQ      '24'
111600971007     C     FUDATT        ANDEQ     'S'
111700971007     C                   MOVEL     FUDDES        FUN008
111800971007     C                   END
111900971007     C                   END                                                    DO
112000971007     C*
112100971007     C                   ENDSR
112200971007     C*------------------------------------------------------------------------*
112300971007     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
112400971007     C*------------------------------------------------------------------------*
112500971007     C     OPZGES        BEGSR
112600971007     C*
112700971007     C* PULISCO CAMPI DI LAVORO
112800971007     C                   Z-ADD     0             W                 5 0
112900971007     C*
113000971007     C* RICERCO QUANTE OPZIONI SONO ATTIVE
113100971007     C* E CARICO TABELLA OPZIONI ATTIVE
113200971007     C     1             DO        24            I
113300971007     C                   MOVEL     OPD(I)        OPDRIG
113400971007     C*
113500971007     C* PULIZIA OPZIONI PRECEDENTI
113600971007     C                   MOVEL     *BLANKS       OPV(I)
113700971007     C*
113800971007     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
113900971007     C     OPDATT        IFEQ      'S'
114000971007     C                   ADD       1             W
114100971007     C                   MOVEL     OPDDES        OPV(W)
114200971007     C                   END
114300971007     C                   END                                                    DO WHILE
114400971007     C*
114500971007     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
114600971007     C* ALTRIMENTI DISATTIVO F23
114700971007     C     1             DO        24            I
114800971007     C                   MOVEL     FUD(I)        FUDRIG
114900971007     C*
115000971007     C     FUDFUN        IFEQ      '23'
115100971007     C     W             IFGT      8
115200971007     C                   MOVEL     'S'           FUDATT
115300971007     C                   ELSE
115400971007     C                   MOVEL     *BLANKS       FUDATT
115500971007     C                   END                                                    W GT 8
115600971007     C                   MOVEL     FUDRIG        FUD(I)
115700971007     C                   END
115800971007     C                   END                                                    DO WHILE
115900971007     C*
116000971007     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
116100971007     C     W             DIV       8             I
116200971007     C                   MVR                     Z                 5 0
116300971007     C*
116400971007     C     Z             IFNE      0
116500971007     C     I             ADD       1             Z
116600971007     C                   ELSE
116700971007     C                   Z-ADD     I             Z
116800971007     C                   END
116900971007     C*
117000971007     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
117100971007     C                   ADD       1             OPZGRU
117200971007     C     OPZGRU        IFGT      Z
117300971007     C     OPZGRU        ORLT      1
117400971007     C                   Z-ADD     1             OPZGRU
117500971007     C                   END
117600971007     C*
117700971007     C* PRIMO GRUPPO DI 8 OPZIONI
117800971007     C     OPZGRU        IFEQ      1
117900971007     C                   MOVEL     OPV(1)        OPZ001
118000971007     C                   MOVEL     OPV(2)        OPZ002
118100971007     C                   MOVEL     OPV(3)        OPZ003
118200971007     C                   MOVEL     OPV(4)        OPZ004
118300971007     C                   MOVEL     OPV(5)        OPZ005
118400971007     C                   MOVEL     OPV(6)        OPZ006
118500971007     C                   MOVEL     OPV(7)        OPZ007
118600971007     C                   MOVEL     OPV(8)        OPZ008
118700971007     C                   END
118800971007     C*
118900971007     C* SECONDO GRUPPO DI 8 OPZIONI
119000971007     C     OPZGRU        IFEQ      2
119100971007     C                   MOVEL     OPV(9)        OPZ001
119200971007     C                   MOVEL     OPV(10)       OPZ002
119300971007     C                   MOVEL     OPV(11)       OPZ003
119400971007     C                   MOVEL     OPV(12)       OPZ004
119500971007     C                   MOVEL     OPV(13)       OPZ005
119600971007     C                   MOVEL     OPV(14)       OPZ006
119700971007     C                   MOVEL     OPV(15)       OPZ007
119800971007     C                   MOVEL     OPV(16)       OPZ008
119900971007     C                   END
120000971007     C*
120100971007     C* TERZO GRUPPO DI 8 OPZIONI
120200971007     C     OPZGRU        IFEQ      3
120300971007     C                   MOVEL     OPV(17)       OPZ001
120400971007     C                   MOVEL     OPV(18)       OPZ002
120500971007     C                   MOVEL     OPV(19)       OPZ003
120600971007     C                   MOVEL     OPV(20)       OPZ004
120700971007     C                   MOVEL     OPV(21)       OPZ005
120800971007     C                   MOVEL     OPV(22)       OPZ006
120900971007     C                   MOVEL     OPV(23)       OPZ007
121000971007     C                   MOVEL     OPV(24)       OPZ008
121100971007     C                   END
121200971007     C*
121300971007     C                   ENDSR
121400971007     C*------------------------------------------------------------------------*
121500971007     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
121600971007     C*------------------------------------------------------------------------*
121700971007     C     OPZCON        BEGSR
121800971007     C*
121900971007     C* PULISCO CAMPI DI LAVORO
122000971007     C                   Z-ADD     0             I                 5 0
122100971007     C                   Z-ADD     0             W                 5 0
122200971007     C*
122300971007     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
122400971007     C     1             DO        24            I
122500971007     C                   MOVEL     OPD(I)        OPDRIG
122600971007     C*
122700971007     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
122800971007     C     OPZKEY        IFEQ      OPDOPZ
122900971007     C     OPZKEY        OREQ      OPDOP1
123000971007     C*
123100971007     C     OPDATT        IFEQ      'S'
123200971007     C                   MOVEL     OPDOPZ        OPZKEY
123300971007     C                   Z-ADD     I             W
123400971007     C                   END
123500971007     C                   END
123600971007     C*
123700971007     C                   END                                                    DO WHILE
123800971007     C*
123900971007     C* OPZIONE NON ATTIVA (ERRORE)
124000971007     C     W             IFEQ      0
124100971007     C*
124200971007     C* OPZIONE NON OK
124300971007     C                   Z-ADD     1             OPZOK
124400971007     C*
124500971007     C* EMISSIONE MESSAGGIO DI ERRORE
124600971007     C                   MOVEL     MSG(1)        DSMSMS
124700971007     C                   ELSE
124800971007     C*
124900971007     C* OPZIONE ATTIVA (PROCEDI ...)
125000971007     C*
125100971007     C     OPZKEY        IFEQ      ' 2'
125200971007     C                   EXSR      O02GES
125300971007     C                   END
125400971007     C     OPZKEY        IFEQ      ' 3'
125500971007     C                   EXSR      O03GES
125600971007     C                   END
125700971007     C     OPZKEY        IFEQ      ' 4'
125800971007     C                   EXSR      O04GES
125900971007     C                   END
126000971007     C     OPZKEY        IFEQ      ' 5'
126100971007     C                   EXSR      O05GES
126200971007     C                   END
126300971007     C                   END                                                    OPZ.NON ATTIVA
126400971007     C*
126500971007     C                   ENDSR
126600971007     C*------------------------------------------------------------------------*
126700971007     C* NOTE COSTRUZIONE SCHIERE
126800971007     C*------------------------------------------------------------------------*
126900971007     C*
127000971007     C* -MSG-MESSAGGI
127100971007     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
127200971007     C* (NON TOCCARE MSG,1 E MSG,2)
127300971007     C*
127400971007     C* -CMD-COMANDI
127500971007     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
127600971007     C*
127700971007     C* -OPD-OPZIONI
127800971007     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
127900971007     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
128000971007     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
128100971007     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
128200971007     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
128300971007     C*  . POSSONO ESSERE IN ORDINE SPARSO
128400971007     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
128500971007     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
128600971007     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
128700971007     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
128800971007     C*
128900971007     C* -FUD-FUNZIONI
129000971007     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
129100971007     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
129200971007     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
129300971007     C*  . POSSONO ESSERE IN ORDINE SPARSO
129400971007     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
129500971007     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
129600971007     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
129700971007     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
129800971007     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
129900971007     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
130000971007     O*------------------------------------------------------------------------*
130100971007** DEC - DECODIFICHE
130200971007   GESTIONE                                                      1
130300971007INTERROGAZIONE                                                   2
1304009710073
1305009710074
1306009710075
130700971007** MSG - MESSAGGI
130800971007TNTL74R- Opzione non gestita                                                   1
130900971007TNTL74R- Tasto funzionale non gestito                                          2
131000971007TNTL74R- 3
131100971007TNTL74R- 4
131200971007TNTL74R- 5
131300971007TNTL74R- 6
131400971007TNTL74R- 7
131500971007TNTL74R- 8
131600971007TNTL74R- 9
131700971007TNTL74R- Data errata                                                          10
131800971007TNTL74R- 11
131900971007TNTL74R- 12
132000971007TNTL74R- 13
132100971007TNTL74R- 14
132200971007TNTL74R- 15
132300971007TNTL74R- 16
132400971007TNTL74R- 17
132500971007TNTL74R- 18
132600971007TNTL74R- 19
132700971007TNTL74R- 20
132800971007** OPD - OPZIONI
132900971007                   -1 -     (S/N)=N                        opz. 01¬
133000971007 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
133100971007 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
133200971007 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
133300971007 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
133400971007                   -  -     (S/N)=N                        opz. 06¬
133500971007                   -  -     (S/N)=N                        opz. 07¬
133600971007                   -  -     (S/N)=N                        opz. 08¬
133700971007                   -  -     (S/N)=N                        opz. 09¬
133800971007                   -  -     (S/N)=N                        opz. 10¬
133900971007                   -  -     (S/N)=N                        opz. 11¬
134000971007                   -  -     (S/N)=N                        opz. 12¬
134100971007                   -  -     (S/N)=N                        opz. 13¬
134200971007                   -  -     (S/N)=N                        opz. 14¬
134300971007                   -  -     (S/N)=N                        opz. 15¬
134400971007                   -  -     (S/N)=N                        opz. 16¬
134500971007                   -  -     (S/N)=N                        opz. 17¬
134600971007                   -  -     (S/N)=N                        opz. 18¬
134700971007                   -  -     (S/N)=N                        opz. 19¬
134800971007                   -  -     (S/N)=N                        opz. 20¬
134900971007                   -  -     (S/N)=N                        opz. 21¬
135000971007                   -  -     (S/N)=N                        opz. 22¬
135100971007                   -  -     (S/N)=N                        opz. 23¬
135200971007                   -  -     (S/N)=N                        opz. 24¬
135300971007** FUD - FUNZIONI
135400971007                   -        (S/N)=N                        funz.01¬
135500971007                   -        (S/N)=N                        funz.02¬
135600971007F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
135700971007                   -        (S/N)=N                        funz.04¬
135800971007F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
135900971007F06=Inserimento    -        (S/N)=N                        funz.06¬
136000971007                   -        (S/N)=N                        funz.07¬
136100971007                   -        (S/N)=N                        funz.08¬
136200971007                   -        (S/N)=N                        funz.09¬
136300971007                   -        (S/N)=N                        funz.10¬
136400971007                   -        (S/N)=N                        funz.11¬
136500971007F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
136600971007                   -        (S/N)=N                        funz.13¬
136700971007                   -        (S/N)=N                        funz.14¬
136800971007                   -        (S/N)=N                        funz.15¬
136900971007                   -        (S/N)=N                        funz.16¬
137000971007                   -        (S/N)=N                        funz.17¬
137100971007                   -        (S/N)=N                        funz.18¬
137200971007 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
137300971007 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
137400971007                   -        (S/N)=N                        funz.21¬
137500971007                   -        (S/N)=N                        funz.22¬
137600971007F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
137700971007F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
