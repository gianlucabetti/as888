000100020718     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* PERMETTE LA VISUALIZZAZIONE E LA STAMPA DEI DOCUMENTI IN FILIALE       *
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNSD551L  IF   E           K DISK    USROPN
000900000000     FTNSD55D   CF   E             WORKSTN INFDS(DEVDS1)
001000000000     F                                     SFILE(SD55D05:SFLULT)
001100000000     F                                     SFILE(SD55DMS:MSGULT)
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* MESSAGGI
001700000000     D*-------------------
001800000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001900000000     D*-------------------
002000000000     D* COMANDI
002100000000     D*-------------------
002200000000     D CMD             S             80    DIM(15) CTDATA PERRCD(1)
002300000000     D*-------------------
002400000000     D* DESCRIZIONI OPZIONI
002500000000     D*-------------------
002600000000     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002700000000     D*-------------------
002800000000     D* DESCRIZIONI TASTI FUNZIONALI
002900000000     D*-------------------
003000000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003100000000     D*-------------------
003200000000     D* OPZIONI DA CARICARE A VIDEO
003300000000     D*-------------------
003400000000     D OPV             S             19    DIM(24)
003500000000     D*-------------------
003600000000     D* TASTI FUNZIONALI DA CARICARE A VIDEO
003700000000     D*-------------------
003800000000     D FUV             S             19    DIM(24)
003900000000     D*------------------------------------------------------------------------*
004000000000     D* INPUT
004100000000     D*------------------------------------------------------------------------*
004200000000     D*-------------------
004300000000     D* RIDEFINIZIONE SCHIERA -OPD-
004400000000     D*-------------------
004500000000     D OPDDS           DS
004600000000     D  OPDOPZ                 1      2
004700000000     D  OPDDE1                 3     19
004800000000     D  OPDOP1                21     22
004900000000     D  OPDFI0                23     34
005000000000     D  OPDATT                35     35
005100000000     D  OPDFI1                36     80
005200000000     D  OPDDES                 1     19
005300000000     D  OPDRIG                 1     80
005400000000     D*-------------------
005500000000     D* RIDEFINIZIONE SCHIERA -FUD-
005600000000     D*-------------------
005700000000     D FUDDS           DS
005800000000     D  FUDDE1                 1      1
005900000000     D  FUDFUN                 2      3
006000000000     D  FUDDE2                 4     19
006100000000     D  FUDFI0                20     34
006200000000     D  FUDATT                35     35
006300000000     D  FUDFI1                36     80
006400000000     D  FUDDES                 1     19
006500000000     D  FUDRIG                 1     80
006600000000     D*-------------------
006700000000     D* DS GESTIONE TASTI FUNZIONALI
006800000000     D*-------------------
006900000000     D DSKEY         E DS
007000000000     D DEVDS1          DS
007100000000     D  KEY                  369    369
007200000000     D*-------------------
007300000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007400000000     D*-------------------
007500000000     D DSMSG         E DS
007600020718
007700020718     d DdatiUte      e ds
007800020718     d AzuteDs       e ds                  ExtName(Azute00f)
007900020718     d Tibs34Ds      e ds                  Inz
008000020718
008100971107     D*-------------------
008200971107     D* DS "TIBS55R" - REPERIMENTO LIBRERIE JOB
008300971107     D*-------------------
008400020730     d tibs55ds      e ds
008500000000     D*-------------------
008600000000     D* ARCHITETTURA
008700000000     D*-------------------
008800000000     D KPJBA         E DS
008900000000     D  KPJFUN               247    248
009000971107     D*-------------------
009100971107     D* DS CMD,2
009200971107     D*-------------------
009300971107     D                 DS
009400971107     D  D2PLG                 33     42
009500971107     D  D2DS                   1     80
009600971107     D*-------------------
009700971107     D* DS CMD,3
009800971107     D*-------------------
009900971107     D                 DS
010000971107     D  D3PLG                 33     42
010100971107     D  D3DS                   1     80
010200960801     D*-------------------
010300000000     D* DS CMD,4
010400000000     D*-------------------
010500000000     D                 DS
010600000000     D  D4FIL1                 1     11
010700000000     D  D4CAR                 12     31
010800000000     D  D4FIL2                32     80
010900000000     D  D4DS                   1     80
011000960801     D*-------------------
011100000000     D* DS CMD,6
011200000000     D*-------------------
011300000000     D                 DS
011400000000     D  D6FIL1                 1     14
011500000000     D  D6DOC                 15     26
011600000000     D  D6FIL2                27     32
011700000000     D  D6CAR                 33     52
011800000000     D  D6FIL3                53     80
011900000000     D  D6DS                   1     80
012000960801     D*-------------------
012100960801     D* DS CMD,12
012200960801     D*-------------------
012300960801     D                 DS
012400960801     D  D12XX1                 1     11
012500960801     D  D12DOC                12     23
012600960801     D  D12XX2                24     29
012700960801     D  D12CAR                30     49
012800960801     D  D12XX3                50     80
012900960801     D  D12DS                  1     80
013000000000     C*------------------------------------------------------------------------*
013100000000     C* MAIN LINE
013200000000     C*------------------------------------------------------------------------*
013300000000     C*
013400000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
013500000000     C                   EXSR      POSSFL
013600000000     C*
013700000000     C* CICLO FINO A CHE:
013800000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)
013900000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)
014000000000     C*  -FINE PGM PER SCELTA EFFETTUATA O ENTER SENZA SCELTA
014100000000DO  1C     F03L00        DOUEQ     '1'
014200000000     C     F12L00        OREQ      '1'
014300000000     C     FINL00        OREQ      '1'
014400000000     C*
014500000000     C* AZZERA FLAG DI ERROE
014600000000     C                   MOVEL     '0'           ERRL00
014700020718
014800020718      * Se Profilo non OK devo uscire dal pgm
014900020718     c                   If        *In76
015000020718     c                   Eval      F03l00 = *On
015100020718     c                   EndIf
015200000000     C*
015300000000     C* EMETTE SFL A VIDEO
015400000000     C                   EXSR      OUTSFL
015500000000     C*
015600000000     C* CONTROLLA E GESTISCE I TASTI FUNZIONALI
015700000000     C                   EXSR      FUNCON
015800000000E   2C                   ENDDO
015900000000     C*
016000000000     C* CHIUDE IL FILE DI LAVORO
016100000000     C                   CLOSE     TNSD551L
016200000000     C*
016300000000     C                   SETON                                        LR
016400000000     C*------------------------------------------------------------------------*
016500000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03=FINE LAVORO
016600000000     C*------------------------------------------------------------------------*
016700000000     C     F03GES        BEGSR
016800000000     C*
016900000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
017000000000     C                   MOVEL     '1'           F03L00
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*------------------------------------------------------------------------*
017400000000     C* F05GES - GESTIONE TASTO FUNZIONALE F05=RIVISUALIZZA
017500000000     C*------------------------------------------------------------------------*
017600000000     C     F05GES        BEGSR
017700000000     C*
017800000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL FILE DI LAVORO
017900000000     C                   EXSR      CARQTE
018000000000     C*
018100000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
018200000000     C                   EXSR      POSSFL
018300000000     C*
018400000000     C                   ENDSR
018500000000     C*------------------------------------------------------------------------*
018600000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06=INSERIMENTO
018700000000     C*------------------------------------------------------------------------*
018800000000     C     F06GES        BEGSR
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12=RITORNO
019200000000     C*------------------------------------------------------------------------*
019300000000     C     F12GES        BEGSR
019400000000     C*
019500000000     C* IMPOSTA FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
019600000000     C                   MOVEL     '1'           F12L00
019700000000     C*
019800000000     C                   ENDSR
019900000000     C*------------------------------------------------------------------------*
020000000000     C* F17GES - GESTIONE TASTO FUNZIONALE F17=INIZIO DATI
020100000000     C*------------------------------------------------------------------------*
020200000000     C     F17GES        BEGSR
020300000000     C*
020400000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
020500000000     C                   Z-ADD     1             SFLNBR
020600000000     C*
020700000000     C                   ENDSR
020800000000     C*------------------------------------------------------------------------*
020900000000     C* F18GES - GESTIONE TASTO FUNZIONALE F18=FINE DATI
021000000000     C*------------------------------------------------------------------------*
021100000000     C     F18GES        BEGSR
021200000000     C*
021300000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
021400000000     C                   Z-ADD     SFLULT        SFLNBR
021500000000     C*
021600000000     C                   ENDSR
021700000000     C*------------------------------------------------------------------------*
021800000000     C* F21GES - GESTIONE TASTO FUNZIONALE F21=STAMPA ELENCO
021900000000     C*------------------------------------------------------------------------*
022000000000     C     F21GES        BEGSR
022100000000     C                   ENDSR
022200000000     C*------------------------------------------------------------------------*
022300000000     C* F22GES - GESTIONE TASTO FUNZIONALE F22=GESTIONE STAMPE
022400000000     C*------------------------------------------------------------------------*
022500000000     C     F22GES        BEGSR
022600000000     C*
022700000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
022800000000     C                   Z-ADD     80            LENGH            15 5
022900000000     C                   CALL      'QCMDEXC'
023000000000     C                   PARM                    CMD(1)
023100000000     C                   PARM                    LENGH
023200000000     C*
023300000000     C                   ENDSR
023400000000     C*------------------------------------------------------------------------*
023500000000     C* F23GES - GESTIONE TASTO FUNZIONALE F23=ALTRE OPZIONI
023600000000     C*------------------------------------------------------------------------*
023700000000     C     F23GES        BEGSR
023800000000     C*
023900000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
024000000000     C                   EXSR      OPZGES
024100000000     C*
024200000000     C                   ENDSR
024300000000     C*------------------------------------------------------------------------*
024400000000     C* F24GES - GESTIONE TASTO FUNZIONALE F24=ALTRI TASTI
024500000000     C*------------------------------------------------------------------------*
024600000000     C     F24GES        BEGSR
024700000000     C*
024800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
024900000000     C                   EXSR      FUNGES
025000000000     C*
025100000000     C                   ENDSR
025200000000     C*------------------------------------------------------------------------*
025300000000     C* RUPGES - GESTIONE TASTO FUNZIONALE ROLLUP=PAGINA SUCCESSIVA
025400000000     C*------------------------------------------------------------------------*
025500000000     C     RUPGES        BEGSR
025600000000     C*
025700000000     C* CARICA IL SFL DA FILE FISICO
025800000000     C                   EXSR      CARSFL
025900000000     C*
026000000000     C                   ENDSR
026100000000     C*------------------------------------------------------------------------*
026200000000     C* HELGES - GESTIONE TASTO FUNZIONALE HELP=AIUTO
026300000000     C*------------------------------------------------------------------------*
026400000000     C     HELGES        BEGSR
026500000000     C                   ENDSR
026600000000     C*------------------------------------------------------------------------*
026700000000     C* ENTGES - GESTIONE TASTO FUNZIONALE ENTER=INVIO
026800000000     C*------------------------------------------------------------------------*
026900000000     C     ENTGES        BEGSR
027000000000     C*
027100000000     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
027200000000     C                   EXSR      NEWPOS
027300000000     C*
027400000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
027500000000IF  1C     ERRL00        IFEQ      '0'
027600000000     C*
027700000000     C* SWPOSI: 0=NO NEW POSIZIONAMENTO, 1=NEW POSIZIONAMENTO
027800000000IF  2C     SWPOSI        IFNE      0
027900000000     C                   EXSR      POSSFL
028000000000X   2C                   ELSE
028100000000     C*
028200000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
028300000000IF  3C     SFLULT        IFNE      0
028400000000     C                   EXSR      ENTATT
028500000000     C*
028600000000     C* SWMODI: 0=NON RICARICO IL SUBFILE, 1=RICARICO IL SUBFILE
028700000000IF  4C     SWMODI        IFNE      0
028800000000     C                   EXSR      POSSFL
028900000000E   4C                   ENDIF
029000000000     C*
029100000000E   3C                   ENDIF
029200000000E   2C                   ENDIF
029300000000E   1C                   ENDIF
029400000000     C*
029500000000     C                   ENDSR
029600000000     C*------------------------------------------------------------------------*
029700000000     C* O01GES - GESTIONE OPZIONE 1=SCELTA
029800000000     C*------------------------------------------------------------------------*
029900000000     C     O01GES        BEGSR
030000000000     C                   ENDSR
030100000000     C*------------------------------------------------------------------------*
030200000000     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
030300000000     C*------------------------------------------------------------------------*
030400000000     C     O02GES        BEGSR
030500000000     C                   ENDSR
030600000000     C*------------------------------------------------------------------------*
030700000000     C* O03GES - GESTIONE OPZIONE 3=COPIA
030800000000     C*------------------------------------------------------------------------*
030900000000     C     O03GES        BEGSR
031000000000     C                   ENDSR
031100000000     C*------------------------------------------------------------------------*
031200000000     C* O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
031300000000     C*------------------------------------------------------------------------*
031400000000     C     O04GES        BEGSR
031500000000     C                   ENDSR
031600000000     C*------------------------------------------------------------------------*
031700000000     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
031800000000     C*------------------------------------------------------------------------*
031900000000     C     O05GES        BEGSR
032000000000     C*
032100000000     C* LANCIA IL COMANDO PER LA VISUALIZZAZIONE DEL DOCUMENTO
032200000000     C                   MOVEL     CMD(6)        D6DS
032300000000     C                   MOVEL     *BLANKS       D6CAR
032400000000     C                   MOVEL     VIDPCA        D6CAR                          *CARTELLA
032500000000     C                   MOVEL     *BLANKS       D6DOC
032600000000     C                   MOVEL     VIDDOC        D6DOC                          *DOCUMENTO
032700000000     C                   MOVEL     D6DS          CMD(6)
032800000000     C                   Z-ADD     80            LENGH            15 5
032900000000     C                   SETOFF                                         20
033000000000     C                   CALL      'QCMDEXC'                            20
033100000000     C                   PARM                    CMD(6)                         *DSPHLPDOC
033200000000     C                   PARM                    LENGH
033300000000     C*
033400000000     C* SE SI E'VERIFICATO UN ERRORE SIGNIFICA CHE IL DOCUMENTO NON E' LEGGIBILE
033500000000IF  1C     *IN20         IFEQ      *ON
033600000000     C                   MOVEL     MSG(5)        DSMSMS
033700000000     C                   MOVEL     '1'           ERRL00
033800000000E   1C                   ENDIF
033900000000     C*
034000000000     C                   ENDSR
034100960801     C*------------------------------------------------------------------------*
034200960801     C* O06GES - GESTIONE OPZIONE 6=STAMPA
034300960801     C*------------------------------------------------------------------------*
034400960801     C     O06GES        BEGSR
034500960801     C*
034600960801     C* LANCIA IL COMANDO PER LA STAMPA DEL DOCUMENTO
034700960801     C                   MOVEL     CMD(12)       D12DS
034800960801     C                   MOVEL     *BLANKS       D12CAR
034900960801     C                   MOVEL     VIDPCA        D12CAR                         *CARTELLA
035000960801     C                   MOVEL     *BLANKS       D12DOC
035100960801     C                   MOVEL     VIDDOC        D12DOC                         *DOCUMENTO
035200960801     C                   MOVEL     D12DS         CMD(12)
035300960801     C                   Z-ADD     80            LENGH            15 5
035400960801     C                   SETOFF                                         20
035500960801     C                   CALL      'QCMDEXC'                            20
035600960801     C                   PARM                    CMD(12)                        *PRTDOC
035700960801     C                   PARM                    LENGH
035800960801     C*
035900960801     C* SE SI E'VERIFICATO UN ERRORE SIGNIFICA CHE IL DOCUMENTO NON E' LEGGIBILE
036000960801IF  1C     *IN20         IFEQ      *ON
036100960801     C                   MOVEL     MSG(5)        DSMSMS
036200960801     C                   MOVEL     '1'           ERRL00
036300960801E   1C                   ENDIF
036400960801     C*
036500960801     C                   ENDSR
036600000000     C*------------------------------------------------------------------------*
036700000000     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
036800000000     C*------------------------------------------------------------------------*
036900000000     C     O07GES        BEGSR
037000000000     C                   ENDSR
037100000000     C*------------------------------------------------------------------------*
037200000000     C* NEWPOS - CONTROLLA NUOVO POSIZIONAMENTO SFILE
037300000000     C*------------------------------------------------------------------------*
037400000000     C     NEWPOS        BEGSR
037500000000     C*
037600000000     C* SWPOSI: 0= NO RICHIESTA POSIZIONAMENTO, 1=RICHIESTA POSIZIONAMENTO
037700000000     C                   Z-ADD     0             SWPOSI            7 0
037800000000     C*
037900000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
038000000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
038100000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
038200000000     C                   Z-ADD     0             SWMODI            1 0
038300000000     C*
038400000000     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
038500000000IF  1C     VIDPCA        IFNE      DEPCAR
038600000000     C     VIDPDO        ORNE      DEPDOC
038700000000     C*
038800000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLA I PARAMETRI DI PARTENZA
038900000000     C                   EXSR      CHKPAR
039000000000     C*
039100000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZA E IMPOSTA FLAG PARAMETRI MODIFICATI
039200000000IF  2C     ERRL00        IFEQ      '0'
039300000000     C                   Z-ADD     1             SWPOSI
039400000000     C                   MOVEL     VIDPCA        DEPCAR
039500000000     C                   MOVEL     VIDPDO        DEPDOC
039600000000E   2C                   ENDIF
039700000000E   1C                   ENDIF
039800000000     C*
039900000000     C                   ENDSR
040000000000     C*------------------------------------------------------------------------*
040100000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO
040200000000     C*------------------------------------------------------------------------*
040300000000     C     POSSFL        BEGSR
040400000000     C*
040500000000     C* ELIMINA CONTENUTO SFL
040600000000     C                   EXSR      CLRSFL
040700000000     C*
040800000000     C* CONTROLLA SE CI SONO RECORD CHE SODDISFANO I PARAMETRI IN INPUT
040900000000     C                   EXSR      SETFIL
041000000000     C*
041100000000     C* SE ESISTONO RECORD VALIDI CARICA IL SUBFILE
041200000000IF  1C     SFLEOF        IFEQ      0
041300000000     C                   EXSR      CARSFL
041400000000E   1C                   ENDIF
041500000000     C*
041600000000     C                   ENDSR
041700000000     C*------------------------------------------------------------------------*
041800000000     C* SETFIL- POSIZIONAMENTO E PRIMA LETTURA RECORD
041900000000     C*------------------------------------------------------------------------*
042000000000     C     SETFIL        BEGSR
042100000000     C*
042200000000     C* POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
042300000000     C                   MOVEL     VIDPCA        K55CAR
042400000000     C                   MOVEL     VIDPDO        K55DOC
042500000000     C     K55KEY        SETLL     TNSD551L                           99        *FINE ARCHIVIO
042600000000     C  N99              READ      TNSD551L                               99
042700000000     C*
042800000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
042900000000IF  1C     *IN99         IFEQ      *ON
043000000000     C                   Z-ADD     1             SFLEOF
043100000000E   1C                   ENDIF
043200000000     C*
043300000000     C                   ENDSR
043400000000     C*------------------------------------------------------------------------*
043500000000     C* LETFIL - LETTURA RECORD SUCCESSIVA
043600000000     C*------------------------------------------------------------------------*
043700000000     C     LETFIL        BEGSR
043800000000     C*
043900000000     C* LETTURE SUCCESSIVE ALLA PRIMA
044000000000     C                   READ      TNSD551L                               99
044100000000     C*
044200000000     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
044300000000IF  1C     *IN99         IFEQ      *ON
044400000000     C                   Z-ADD     1             SFLEOF
044500000000E   1C                   ENDIF
044600000000     C*
044700000000     C                   ENDSR
044800000000     C*------------------------------------------------------------------------*
044900000000     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE FISICO
045000000000     C*------------------------------------------------------------------------*
045100000000     C     IRESFL        BEGSR
045200000000     C*
045300000000     C                   MOVEL     S55DOC        VIDDOC                         *DOCUMENTO
045400000000     C                   MOVEL     S55DES        VIDDES                         *DESCRIZIONE
045500000000     C                   MOVEL     S55DTR        VIDDTR                         *DATA REVIISONE
045600000000     C*
045700000000     C                   ENDSR
045800000000     C*------------------------------------------------------------------------*
045900000000     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
046000000000     C*------------------------------------------------------------------------*
046100000000     C     CHKPAR        BEGSR
046200000000     C*
046300000000     C* IMPOSTA FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
046400000000     C                   SETOFF                                       8081
046500000000     C*
046600000000     C* NON ACCETTA IL NOME CARTELLA VUOTO
046700000000IF  1C     VIDPCA        IFEQ      *BLANKS
046800000000     C                   MOVEL     MSG(3)        DSMSMS
046900000000     C                   MOVEL     '1'           ERRL00
047000000000     C                   SETON                                        80
047100000000E   1C                   ENDIF
047200000000     C*
047300000000     C* SE E' STATA CAMBIATA LA CARTELLA RICARICA IL FILE
047400000000IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
047500000000IF  2C     VIDPCA        IFNE      DEPCAR
047600000000     C                   EXSR      CARQTE
047700000000E   2C                   ENDIF
047800000000E   1C                   ENDIF
047900000000     C*
048000000000     C                   ENDSR
048100000000     C*------------------------------------------------------------------------*
048200000000     C* CARQTE - CARICA IL FILE DI LAVORO IN QTEMP
048300000000     C*------------------------------------------------------------------------*
048400000000     C     CARQTE        BEGSR
048500000000     C*
048600000000     C* CHIUDE IL FILE DI LAVORO
048700000000     C                   CLOSE     TNSD551L
048800000000     C*
048900000000     C* LANCIA IL COMANDO DI VISUALIZZAZIONE CARTELLA
049000000000     C                   MOVEL     CMD(4)        D4DS
049100000000     C                   MOVEL     *BLANKS       D4CAR
049200000000     C                   MOVEL     VIDPCA        D4CAR
049300000000     C                   MOVEL     D4DS          CMD(4)
049400000000     C                   Z-ADD     80            LENGH            15 5
049500000000     C                   SETOFF                                         20
049600000000     C                   CALL      'QCMDEXC'                            20
049700000000     C                   PARM                    CMD(4)                         *DSPFLR
049800000000     C                   PARM                    LENGH
049900000000     C*
050000000000     C* SE CARTELLA NON TROVATA, ERRORE
050100000000IF  1C     *IN20         IFEQ      *ON
050200000000     C                   MOVEL     MSG(4)        DSMSMS
050300000000     C                   MOVEL     '1'           ERRL00
050400000000     C                   SETON                                        80
050500000000     C*
050600000000     C* SE CARTELLA TROVATA, COPIA I RECORD NEL FILE DI LAVORO
050700000000X   1C                   ELSE
050800000000     C                   Z-ADD     80            LENGH            15 5
050900000000     C                   SETOFF                                         20
051000000000     C                   CALL      'QCMDEXC'                            20
051100000000     C                   PARM                    CMD(5)                         *CPYF
051200000000     C                   PARM                    LENGH
051300000000     C*
051400000000     C* SE C'E' ERRORE NELLA COPIA SIGNIFICA CHE LA CARTELLA E' VUOTA
051500000000IF  2C     *IN20         IFEQ      *ON
051600000000     C                   MOVEL     MSG(6)        DSMSMS
051700000000     C                   MOVEL     '1'           ERRL00
051800000000     C                   SETON                                        80
051900000000E   2C                   ENDIF
052000000000E   1C                   ENDIF
052100000000     C*
052200000000     C* RIAPRE IL FILE DI LAVORO
052300000000     C                   OPEN      TNSD551L
052400000000     C*
052500000000     C                   ENDSR
052600000000     C*------------------------------------------------------------------------*
052700000000     C* COPQTE -COPIA IL FILE DI LAVORO IN QTEMP
052800000000     C*------------------------------------------------------------------------*
052900000000     C     COPQTE        BEGSR
053000000000     C*
053100000000     C* CONTROLLA SE ESISTE IN QTEMP IL FILE DI LAVORO (LOGICO)
053200000000     C                   Z-ADD     80            LENGH            15 5
053300000000     C                   SETOFF                                         20
053400000000     C                   CALL      'QCMDEXC'                            20
053500000000     C                   PARM                    CMD(7)
053600000000     C                   PARM                    LENGH
053700000000     C*
053800000000     C* CANCELLA IL FILE LOGICO DI LAVORO DELLA QTEMP (SE E' STATO TROVATO)
053900000000IF  1C     *IN20         IFEQ      *OFF
054000000000     C                   Z-ADD     80            LENGH            15 5
054100000000     C                   CALL      'QCMDEXC'
054200000000     C                   PARM                    CMD(8)
054300000000     C                   PARM                    LENGH
054400000000E   1C                   ENDIF
054500000000     C*
054600000000     C* CONTROLLA SE ESISTE IN QTEMP IL FILE DI LAVORO (FISICO)
054700000000     C                   Z-ADD     80            LENGH            15 5
054800000000     C                   SETOFF                                         20
054900000000     C                   CALL      'QCMDEXC'                            20
055000000000     C                   PARM                    CMD(9)
055100000000     C                   PARM                    LENGH
055200000000     C*
055300000000     C* CANCELLA IL FILE FISICO DI LAVORO DELLA QTEMP (SE C'E')
055400000000IF  1C     *IN20         IFEQ      *OFF
055500000000     C                   Z-ADD     80            LENGH            15 5
055600000000     C                   CALL      'QCMDEXC'
055700000000     C                   PARM                    CMD(10)
055800000000     C                   PARM                    LENGH
055900000000E   1C                   ENDIF
056000000000     C*
056100000000     C* COPIA IL FILE FISICO DI LAVORO IN QTEMP (SE E' VUOTO)
056200000000     C                   Z-ADD     80            LENGH            15 5
056300971107     C                   MOVEL     *BLANKS       COMMAN           80
056400971107     C                   MOVEL     CMD(2)        D2DS
056500971107     C                   MOVEL     O50PLG        D2PLG
056600971107     C                   MOVEL     D2DS          COMMAN
056700000000     C                   CALL      'QCMDEXC'
056800971107     C                   PARM                    COMMAN
056900000000     C                   PARM                    LENGH
057000000000     C*
057100000000     C* COPIA IL FILE LOGICO DI LAVORO IN QTEMP
057200000000     C                   Z-ADD     80            LENGH            15 5
057300971107     C                   MOVEL     *BLANKS       COMMAN           80
057400971107     C                   MOVEL     CMD(3)        D3DS
057500971107     C                   MOVEL     O50PLG        D3PLG
057600971107     C                   MOVEL     D3DS          COMMAN
057700000000     C                   CALL      'QCMDEXC'
057800971107     C                   PARM                    COMMAN
057900000000     C                   PARM                    LENGH
058000000000     C*
058100000000     C                   ENDSR
058200000000     C*------------------------------------------------------------------------*
058300000000     C* *INZSR - OPERAZIONI INIZIALI
058400000000     C*------------------------------------------------------------------------*
058500000000     C     *INZSR        BEGSR
058600000000     C*--------------------
058700000000     C* RICEVIMENTO PARAMETRI
058800000000     C*--------------------
058900000000     C     *ENTRY        PLIST
059000000000     C                   PARM                    KPJBA
059100000000     C*--------------------
059200000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
059300000000     C*--------------------
059400000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
059500000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
059600000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
059700000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
059800000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
059900000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
060000000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
060100000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
060200000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
060300000000     C     *LIKE         DEFINE    S55CAR        K55CAR
060400000000     C     *LIKE         DEFINE    S55DOC        K55DOC
060500000000     C     *LIKE         DEFINE    S55CAR        DEPCAR
060600000000     C     *LIKE         DEFINE    S55DOC        DEPDOC
060700000000     C*--------------------
060800000000     C* CHIAVI DI LETTURA
060900000000     C*--------------------
061000000000     C* LETTURA TNSD551L - COMPLETA
061100000000     C     K55KEY        KLIST
061200000000     C                   KFLD                    K55CAR
061300000000     C                   KFLD                    K55DOC
061400000000     C*
061500000000     C* CHIAMA IL "XMSG" - GESTIONE MESSAGGI
061600000000     C     XMSGPA        PLIST
061700000000     C                   PARM                    DSMSMS
061800000000     C                   PARM                    DSMSPG
061900000000     C                   PARM                    DSMSKE
062000000000     C                   PARM                    DSMSER
062100000000     C*
062200000000     C* CHIAMA IL "XKEY" - GESTIONE TASTI PREMUTI
062300000000     C                   CALL      'XKEY'
062400000000     C                   PARM                    DSKEY
062500000000     C*
062600000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
062700000000     C*   -NOME PGM CHIAMANTE
062800000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
062900000000     C                   MOVEL     'TNSD55R'     DSMSPG
063000000000     C                   MOVEL     'TNSD55R'     MSGQUE
063100000000     C                   MOVEL     *BLANKS       MSGKEY
063200000000     C                   MOVEL     *BLANKS       DSMSMS
063300000000     C                   MOVEL     *BLANKS       DSMSKE
063400000000     C                   Z-ADD     *ZEROS        DSMSER
063500000000     C*
063600000000     C* INIZIALIZZA SWITCH DI LIVELLO 0 (PGM)
063700000000     C                   MOVEL     '0'           F12L00            1
063800000000     C                   MOVEL     '0'           F03L00            1
063900000000     C                   MOVEL     '0'           FINL00            1
064000000000     C                   MOVEL     '0'           ERRL00            1
064100000000     C                   MOVEL     '0'           FLFINE            1
064200000000     C*
064300000000     C* CHIAMA "XFIL" - REPERIMENTO DATI UTENTE
064400020718
064500020718     c     *dtaara       define    §azute        azuteds
064600020718     c     *dtaara       define    §datiute      ddatiute
064700020718
064800020718     c                   in(E)     *dtaara
064900020718     c                   if        %Error or rsut = *blanks
065000020718     c                   clear                   tibs34ds
065100020718     c                   call      'TIBS34R'
065200020718     c                   parm                    Tibs34Ds
065300020718     c                   in        *dtaara
065400020718     c                   endif
065500020718
065600020718      * Controllo il profilo utente
065700020718     c                   ExSr      CtrUte
065800971107     C*
065900971107     C* REPERIMENTO LIBRERIE JOB
066000020730     C                   CLEAR                   tibs55ds
066100971107     C                   MOVEL     'L'           I50TLA
066200971107     C                   MOVEL     KNSIF         I50PSI
066300020730     C                   CALL      'TIBS55R'
066400020730     C                   PARM                    tibs55ds
066500000000     C*
066600000000     C* INIZIALIZZA CONTATORE DI NUMERO RIGHE PER PAGINA
066700000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
066800000000     C                   Z-ADD     12            SFLPAG
066900000000     C*
067000000000     C* IMPOSTA INDICE PER EMISSIONE OPZIONI A VIDEO
067100000000     C                   Z-ADD     *ZEROS        OPZGRU            5 0
067200000000     C*
067300000000     C* GESTIONE OPZIONI A VIDEO
067400000000     C                   EXSR      OPZGES
067500000000     C*
067600000000     C* IMPOSTA INDICE PER EMISSIONE FUNZIONI A VIDEO
067700000000     C                   Z-ADD     *ZEROS        FUNGRU            5 0
067800000000     C*
067900000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
068000000000     C                   EXSR      FUNGES
068100000000     C*
068200000000     C* COPIA I FILE DI LAVORO IN QTEMP
068300000000     C                   EXSR      COPQTE
068400000000     C*
068500000000     C* APRE IL FILE DI LAVORO
068600000000     C                   OPEN      TNSD551L
068700000000     C*
068800000000     C* IMPOSTA I PARAMETRI DI INIZIO ELENCO DI DEFAULT
068900971002     C* CARTELLA ==> EDP_FILQ
069000971002     C                   MOVEL     'EDP_FILQ'    VIDPCA
069100000000     C                   MOVEL     *BLANKS       VIDPDO
069200000000     C*
069300000000     C* INIZIALIZZA DEPOSITI PER CONFRONTI
069400000000     C                   MOVEL     VIDPCA        DEPCAR
069500000000     C                   MOVEL     VIDPDO        DEPDOC
069600000000     C*
069700000000     C* CARICA IL FILE DI LAVORO
069800000000     C                   EXSR      CARQTE
069900000000     C*
070000000000     C* CARICA IL SUBFILE
070100000000     C                   EXSR      POSSFL
070200000000     C*
070300000000     C                   ENDSR
070400020718      *------------------------------------------------------------------------*
070500020718      *  CTRUTE - ROUTINE CONTROLLO UTENTE
070600020718      *------------------------------------------------------------------------*
070700020718     c     CtrUte        BegSr
070800020718
070900020718     c                   Eval      *In76 = *Off
071000020718
071100020718      * Controllo se il profilo è ok da tibs34
071200020718     c                   If        DutErr = 'E'
071300020718     c                   Eval      DsmSms = DutMsg
071400020718     c                   Eval      *In76 = *On
071500020718     c                   EndIf
071600020718
071700020718     c                   EndSr
071800000000     C*------------------------------------------------------------------------*
071900000000     C* ENTATT - GESTIONE TASTO FUNZIONALE ENTER=INVIO
072000000000     C*------------------------------------------------------------------------*
072100000000     C     ENTATT        BEGSR
072200000000     C*
072300000000     C* AZZERA SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
072400000000     C                   Z-ADD     0             OPZOK             1 0
072500000000     C*
072600000000     C* AZZERA CONTATORE DI RIGHE SFL VARIATE
072700000000     C                   Z-ADD     0             SFLVAR            5 0
072800000000     C*
072900000000     C* INIZIALIZZA VARIABILE DI COMODO CON PRIMO RECORD SFL IN ERRORE
073000000000     C                   Z-ADD     0             SFLERR
073100000000     C*
073200000000     C* SALVA ULTIMO RECORD SCRITTO IN SFL IN VARIABILE DI COMODO
073300000000     C                   Z-ADD     SFLULT        WFLULT
073400000000     C*
073500000000     C* LETTURA DEI SOLI RECORD VARIATI NEL SFL
073600000000     C                   READC     SD55D05                                98
073700000000DO  1C     *IN98         DOWEQ     *OFF
073800000000     C*
073900000000     C* INCREMENTE CONTATORE DI RIGHE SFL VARIATE
074000000000     C                   ADD       1             SFLVAR
074100000000     C*
074200000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
074300000000IF  2C     OPZOK         IFEQ      *ZEROS
074400000000     C*
074500000000     C* CONTROLLA ED ESEGUE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
074600000000IF  3C     OPZKEY        IFNE      *BLANKS
074700000000     C                   EXSR      OPZCON
074800000000E   3C                   ENDIF
074900000000     C*
075000000000     C* SE OK: 1. AZZERA SFLNXTCHG PER RECORD SFL
075100000000     C*        2. PULISCE SCELTA IN SFL
075200000000IF  3C     OPZOK         IFEQ      0
075300000000     C                   MOVEL     '0'           *IN41
075400000000     C                   MOVEL     *BLANKS       OPZKEY
075500000000X   3C                   ELSE
075600000000     C*
075700000000     C* SE NON OK: 1. ACCENDE SFLNXTCHG COSI' COSTRINGE ALLA CORREZIONE
075800000000     C*            2. IMPOSTA RECORD CON PRIMO ERRORE
075900000000     C                   MOVEL     '1'           *IN41
076000000000     C                   Z-ADD     SFLULT        SFLERR
076100000000E   3C                   ENDIF
076200000000     C*
076300000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
076400000000     C*   . ACCENDE SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
076500000000X   2C                   ELSE
076600000000     C                   MOVEL     '1'           *IN41
076700000000E   2C                   ENDIF
076800000000     C*
076900000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
077000000000     C*   . SE ERRORE    SI POSIZIONA SULLA PRIMA RIGA IN ERRORE
077100000000     C*   . SE NO ERRORE SI POSIZIONA SULL'ULTIMA RIGA VARIATA
077200000000IF  2C     OPZOK         IFNE      *ZEROS
077300000000     C                   Z-ADD     SFLERR        SFLNBR
077400000000X   2C                   ELSE
077500000000     C                   Z-ADD     SFLULT        SFLNBR
077600000000E   2C                   ENDIF
077700000000     C*
077800000000     C* AGGIORNA RECORD SFL
077900000000     C                   UPDATE    SD55D05
078000000000     C*
078100000000     C* LEGGE ALTRO RECORD TOCCATO
078200000000     C                   READC     SD55D05                                98
078300000000E   1C                   ENDDO
078400000000     C*
078500000000     C* REIMPOSTA ULTIMO RECORD SCRITTO IN SFL DA VARIABILE DI COMODO
078600000000     C                   Z-ADD     WFLULT        SFLULT
078700000000     C*
078800000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
078900000000IF  1C     SFLVAR        IFEQ      *ZEROS
079000000000     C                   MOVEL     '1'           FINL00
079100000000E   1C                   ENDIF
079200000000     C*
079300000000     C                   ENDSR
079400000000     C*------------------------------------------------------------------------*
079500000000     C* CLRSFL -ELIMINA IL CONTENUTO DEL SFILE
079600000000     C*------------------------------------------------------------------------*
079700000000     C     CLRSFL        BEGSR
079800000000     C*
079900000000     C* INIZIALIZZA SWITCH DI FINE FILE FISICO
080000000000     C* 0=NON FINE FILE, 1=FINE FILE
080100000000     C                   Z-ADD     0             SFLEOF
080200000000     C*
080300000000     C* INIZIALIZZA CONTATORI DI ULTIMO RECORD IN SFL
080400000000     C                   Z-ADD     0             SFLULT
080500000000     C*
080600000000     C* ACCENDE L'INDICATORE PER PULIRE IL SFILE
080700000000     C                   MOVEL     '1'           *IN40
080800000000     C                   WRITE     SD55D06
080900000000     C                   MOVEL     '0'           *IN40
081000000000     C*
081100000000     C                   ENDSR
081200000000     C*------------------------------------------------------------------------*
081300000000     C* CARSFL - CARICAMENTO SFILE DAL FILE FISICO
081400000000     C*------------------------------------------------------------------------*
081500000000     C     CARSFL        BEGSR
081600000000     C*
081700000000     C* SE IL SFILE E' VUOTO INIZIA LA LETTURA DAL POSIZIONAMENTO RICHIESTO
081800000000IF  1C     SFLULT        IFEQ      0
081900000000     C                   EXSR      SETFIL
082000000000E   1C                   ENDIF
082100000000     C*
082200000000     C* SALVA SFLRCDNBR PER CONFRONTI SUCCESSIVI
082300000000     C                   Z-ADD     SFLULT        WFLULT
082400000000     C*
082500000000     C* CICLO:
082600000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
082700000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
082800000000     C                   Z-ADD     1             SFLRIG
082900000000DO  1C     SFLRIG        DOWLE     SFLPAG
083000000000     C     SFLEOF        ANDEQ     0
083100000000     C*
083200000000     C* IMPOSTA CAMPI DEL RECORD SFL SOLO SE RECORD E' OK
083300000000     C                   MOVEL     *BLANKS       OPZKEY
083400000000     C*
083500000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE DA CARICARE
083600000000     C                   EXSR      IRESFL
083700000000     C*
083800000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
083900000000     C                   ADD       1             SFLULT
084000000000     C*
084100000000     C* SCRIVE RECORD SFILE
084200000000     C                   WRITE     SD55D05
084300000000     C                   ADD       1             SFLRIG
084400000000     C*
084500000000     C* LEGGE PROSSIMO RECORD DA CARICARE
084600000000     C                   EXSR      LETFIL
084700000000E   1C                   ENDDO
084800000000     C*
084900000000     C* IMPOSTA SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
085000000000IF  1C     WFLULT        IFNE      SFLULT
085100000000     C                   Z-ADD     WFLULT        SFLNBR
085200000000     C                   ADD       1             SFLNBR
085300000000E   1C                   ENDIF
085400000000     C*
085500000000     C                   ENDSR
085600000000     C*------------------------------------------------------------------------*
085700000000     C* OUTSFL - EMISSIONE SFILE A VIDEO
085800000000     C*------------------------------------------------------------------------*
085900000000     C     OUTSFL        BEGSR
086000000000     C*
086100000000     C* SCRIVE RECORD TESTATA VIDEO
086200000000     C                   WRITE     SD55D01
086300000000     C*
086400000000     C* VISUALIZZAZIONE MESSAGGIO
086500000000     C                   EXSR      SNDMSG
086600000000     C*
086700000000     C* SCRIVE RECORD OPZIONI VIDEO
086800000000     C                   WRITE     SD55D02
086900000000     C*
087000000000     C* SCRIVE RECORD PER POSIZIONAMEMTO VIDEO
087100000000     C                   WRITE     SD55D03
087200000000     C*
087300000000     C* SCRIVE RECORD CON DESCRIZIONI COLONNE SFL
087400000000     C                   WRITE     SD55D04
087500000000     C*
087600000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
087700000000     C* . SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
087800000000     C* . SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
087900000000IF  1C     SFLEOF        IFEQ      0
088000000000     C                   MOVEL     '0'           *IN42
088100000000X   1C                   ELSE
088200000000     C                   MOVEL     '1'           *IN42
088300000000E   1C                   ENDIF
088400000000     C*
088500000000     C* SCRIVE RECORD TASTI FUNZIONALI
088600000000     C                   WRITE     SD55D09
088700000000     C*
088800000000     C* CASO DI SFL VUOTO
088900000000     C* . SCRIVE RECORD VUOTO
089000000000IF  1C     SFLULT        IFLE      0
089100000000     C                   WRITE     SD55D0A
089200000000     C                   READ      SD55D0A                                99
089300000000     C*
089400000000     C* CASO DI SFL CON RECORDS
089500000000     C* . SCRIVE SFILE CONTROL
089600000000X   1C                   ELSE
089700000000     C                   WRITE     SD55D06
089800000000     C                   READ      SD55D06                                99
089900000000E   1C                   ENDIF
090000000000     C*
090100000000     C* LEGGE RECORD PER POSIZIONAMEMTO A VIDEO
090200000000     C                   READ      SD55D03                                99
090300000000     C*
090400000000     C                   ENDSR
090500000000     C*------------------------------------------------------------------------*
090600000000     C* SNDMSG - INVIA MESSAGGIO A VIDEO
090700000000     C*------------------------------------------------------------------------*
090800000000     C     SNDMSG        BEGSR
090900000000     C*
091000000000     C* PULIZIA SUBFILE MESSAGGI (SCRIVE RECORD CONTROLLO SFLMSG)
091100000000     C                   MOVEL     '1'           *IN40
091200000000     C                   WRITE     SD55DMC
091300000000     C                   MOVEL     '0'           *IN40
091400000000     C*
091500000000     C* VISUALIZZAZIONE MESSAGGIO
091600000000     C                   Z-ADD     0             MSGULT
091700000000     C                   CALL      'XMSG'        XMSGPA
091800000000IF  1C     DSMSER        IFEQ      *ZEROS
091900000000     C                   MOVEL     DSMSKE        MSGKEY
092000000000E   1C                   ENDIF
092100000000     C                   Z-ADD     1             MSGULT
092200000000     C                   WRITE     SD55DMS                                      SFL MESS
092300000000     C                   WRITE     SD55DMC                                      CONTR MESS
092400000000     C*
092500000000     C* INVIA MESSAGGIO VUOTO PER PULIRE SFLMSG
092600000000     C                   MOVEL     *BLANKS       DSMSMS
092700000000     C*
092800000000     C                   ENDSR
092900000000     C     FUNCON        BEGSR
093000000000     C*------------------------------------------------------------------------*
093100000000     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
093200000000     C*------------------------------------------------------------------------*
093300000000     C*
093400000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
093500000000     C                   EXSR      FUNVAR
093600000000     C*
093700000000     C* PULISCE CAMPI DI LAVORO
093800000000     C                   Z-ADD     0             I                 5 0
093900000000     C                   Z-ADD     0             W                 5 0
094000000000     C*
094100000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
094200000000IF  1C     FUNKEY        IFNE      *BLANKS
094300000000DO  2C     1             DO        24            I
094400000000     C                   MOVEL     FUD(I)        FUDRIG
094500000000     C*
094600000000     C* SALVA INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
094700000000IF  3C     FUNKEY        IFEQ      FUDFUN
094800000000     C     FUDATT        ANDEQ     'S'
094900000000     C                   Z-ADD     I             W
095000000000E   3C                   ENDIF
095100000000E   2C                   ENDDO
095200000000     C*
095300000000     C* FUNZIONE NON ATTIVA (ERRORE)
095400000000IF  2C     W             IFEQ      0
095500000000     C*
095600000000     C* EMISSIONE MESSAGGIO DI ERRORE
095700000000     C                   MOVEL     MSG(2)        DSMSMS
095800000000E   2C                   ENDIF
095900000000E   1C                   ENDIF
096000000000     C*
096100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
096200000000IF  1C     W             IFNE      0
096300000000     C     FUNKEY        OREQ      *BLANKS
096400000000IF  2C     FUNKEY        IFEQ      *BLANKS
096500000000     C                   EXSR      ENTGES
096600000000E   2C                   ENDIF
096700000000IF  2C     FUNKEY        IFEQ      '03'
096800000000     C                   EXSR      F03GES
096900000000E   2C                   ENDIF
097000000000IF  2C     FUNKEY        IFEQ      '05'
097100000000     C                   EXSR      F05GES
097200000000E   2C                   ENDIF
097300000000IF  2C     FUNKEY        IFEQ      '06'
097400000000     C                   EXSR      F06GES
097500000000E   2C                   ENDIF
097600000000IF  2C     FUNKEY        IFEQ      '12'
097700000000     C                   EXSR      F12GES
097800000000E   2C                   ENDIF
097900000000IF  2C     FUNKEY        IFEQ      '17'
098000000000     C                   EXSR      F17GES
098100000000E   2C                   ENDIF
098200000000IF  2C     FUNKEY        IFEQ      '18'
098300000000     C                   EXSR      F18GES
098400000000E   2C                   ENDIF
098500000000IF  2C     FUNKEY        IFEQ      '21'
098600000000     C                   EXSR      F21GES
098700000000E   2C                   ENDIF
098800000000IF  2C     FUNKEY        IFEQ      '22'
098900000000     C                   EXSR      F22GES
099000000000E   2C                   ENDIF
099100000000IF  2C     FUNKEY        IFEQ      '23'
099200000000     C                   EXSR      F23GES
099300000000E   2C                   ENDIF
099400000000IF  2C     FUNKEY        IFEQ      '24'
099500000000     C                   EXSR      F24GES
099600000000E   2C                   ENDIF
099700000000IF  2C     FUNKEY        IFEQ      'RU'
099800000000     C                   EXSR      RUPGES
099900000000E   2C                   ENDIF
100000000000IF  2C     FUNKEY        IFEQ      'HP'
100100000000     C                   EXSR      HELGES
100200000000E   2C                   ENDIF
100300000000E   1C                   ENDIF
100400000000     C*
100500000000     C                   ENDSR
100600000000     C*------------------------------------------------------------------------*
100700000000     C* FUNVAR - TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILI
100800000000     C*------------------------------------------------------------------------*
100900000000     C     FUNVAR        BEGSR
101000000000     C*
101100000000     C* INIZIALIZZA VARIABILE -FUNKEY-
101200000000     C*
101300000000     C* FUNKEY=BLANKS SIGNIFICA ENTER
101400000000IF  1C     KEY           IFEQ      ENTER
101500000000     C                   MOVEL     *BLANKS       FUNKEY
101600000000E   1C                   ENDIF
101700000000IF  1C     KEY           IFEQ      F1
101800000000     C                   MOVEL     '01'          FUNKEY
101900000000E   1C                   ENDIF
102000000000IF  1C     KEY           IFEQ      F2
102100000000     C                   MOVEL     '02'          FUNKEY
102200000000E   1C                   ENDIF
102300000000IF  1C     KEY           IFEQ      F3
102400000000     C                   MOVEL     '03'          FUNKEY
102500000000E   1C                   ENDIF
102600000000IF  1C     KEY           IFEQ      F4
102700000000     C                   MOVEL     '04'          FUNKEY
102800000000E   1C                   ENDIF
102900000000IF  1C     KEY           IFEQ      F5
103000000000     C                   MOVEL     '05'          FUNKEY
103100000000E   1C                   ENDIF
103200000000IF  1C     KEY           IFEQ      F6
103300000000     C                   MOVEL     '06'          FUNKEY
103400000000E   1C                   ENDIF
103500000000IF  1C     KEY           IFEQ      F7
103600000000     C                   MOVEL     '07'          FUNKEY
103700000000E   1C                   ENDIF
103800000000IF  1C     KEY           IFEQ      F8
103900000000     C                   MOVEL     '08'          FUNKEY
104000000000E   1C                   ENDIF
104100000000IF  1C     KEY           IFEQ      F9
104200000000     C                   MOVEL     '09'          FUNKEY
104300000000E   1C                   ENDIF
104400000000IF  1C     KEY           IFEQ      F10
104500000000     C                   MOVEL     '10'          FUNKEY
104600000000E   1C                   ENDIF
104700000000IF  1C     KEY           IFEQ      F11
104800000000     C                   MOVEL     '11'          FUNKEY
104900000000E   1C                   ENDIF
105000000000IF  1C     KEY           IFEQ      F12
105100000000     C                   MOVEL     '12'          FUNKEY
105200000000E   1C                   ENDIF
105300000000IF  1C     KEY           IFEQ      F13
105400000000     C                   MOVEL     '13'          FUNKEY
105500000000E   1C                   ENDIF
105600000000IF  1C     KEY           IFEQ      F14
105700000000     C                   MOVEL     '14'          FUNKEY
105800000000E   1C                   ENDIF
105900000000IF  1C     KEY           IFEQ      F15
106000000000     C                   MOVEL     '15'          FUNKEY
106100000000E   1C                   ENDIF
106200000000IF  1C     KEY           IFEQ      F16
106300000000     C                   MOVEL     '16'          FUNKEY
106400000000E   1C                   ENDIF
106500000000IF  1C     KEY           IFEQ      F17
106600000000     C                   MOVEL     '17'          FUNKEY
106700000000E   1C                   ENDIF
106800000000IF  1C     KEY           IFEQ      F18
106900000000     C                   MOVEL     '18'          FUNKEY
107000000000E   1C                   ENDIF
107100000000IF  1C     KEY           IFEQ      F19
107200000000     C                   MOVEL     '19'          FUNKEY
107300000000E   1C                   ENDIF
107400000000IF  1C     KEY           IFEQ      F20
107500000000     C                   MOVEL     '20'          FUNKEY
107600000000E   1C                   ENDIF
107700000000IF  1C     KEY           IFEQ      F21
107800000000     C                   MOVEL     '21'          FUNKEY
107900000000E   1C                   ENDIF
108000000000IF  1C     KEY           IFEQ      F22
108100000000     C                   MOVEL     '22'          FUNKEY
108200000000E   1C                   ENDIF
108300000000IF  1C     KEY           IFEQ      F23
108400000000     C                   MOVEL     '23'          FUNKEY
108500000000E   1C                   ENDIF
108600000000IF  1C     KEY           IFEQ      F24
108700000000     C                   MOVEL     '24'          FUNKEY
108800000000E   1C                   ENDIF
108900000000IF  1C     KEY           IFEQ      ROLLUP
109000000000     C                   MOVEL     'RU'          FUNKEY
109100000000E   1C                   ENDIF
109200000000IF  1C     KEY           IFEQ      HELP
109300000000     C                   MOVEL     'HP'          FUNKEY
109400000000E   1C                   ENDIF
109500000000     C*
109600000000     C                   ENDSR
109700000000     C*------------------------------------------------------------------------*
109800000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
109900000000     C*------------------------------------------------------------------------*
110000000000     C     FUNGES        BEGSR
110100000000     C*
110200000000     C* PULISCE CAMPI DI LAVORO
110300000000     C                   Z-ADD     0             W                 5 0
110400000000     C*
110500000000     C* RICERCA QUANTE FUNZIONI SONO ATTIVE E LE CARICA IN TABELLA
110600000000DO  1C     1             DO        24            I
110700000000     C                   MOVEL     FUD(I)        FUDRIG
110800000000     C*
110900000000     C* PULIZIA FUNZIONI PRECEDENTI
111000000000     C                   MOVEL     *BLANKS       FUV(I)
111100000000     C*
111200000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA (SALTA F23 E F24)
111300000000IF  2C     FUDATT        IFEQ      'S'
111400000000     C     FUDFUN        ANDNE     '23'
111500000000     C     FUDFUN        ANDNE     '24'
111600000000     C                   ADD       1             W
111700000000     C                   MOVEL     FUDDES        FUV(W)
111800000000E   2C                   ENDIF
111900000000E   1C                   ENDDO
112000000000     C*
112100000000     C* SE CI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVA FUNZIONE F24=ALTRI TASTI
112200000000DO  1C     1             DO        24            I
112300000000     C                   MOVEL     FUD(I)        FUDRIG
112400000000IF  2C     FUDFUN        IFEQ      '24'
112500000000IF  3C     W             IFGT      6
112600000000     C                   MOVEL     'S'           FUDATT
112700000000X   3C                   ELSE
112800000000     C                   MOVEL     *BLANKS       FUDATT
112900000000E   3C                   ENDIF
113000000000     C                   MOVEL     FUDRIG        FUD(I)
113100000000E   2C                   ENDIF
113200000000E   1C                   ENDDO
113300000000     C*
113400000000     C* IMPOSTA QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
113500000000     C     W             DIV       6             I
113600000000     C                   MVR                     Z
113700000000IF  1C     Z             IFNE      0
113800000000     C     I             ADD       1             Z
113900000000X   1C                   ELSE
114000000000     C                   Z-ADD     I             Z
114100000000E   1C                   ENDIF
114200000000     C*
114300000000     C* IMPOSTA SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
114400000000     C                   ADD       1             FUNGRU
114500000000IF  1C     FUNGRU        IFGT      Z
114600000000     C     FUNGRU        ORLT      1
114700000000     C                   Z-ADD     1             FUNGRU
114800000000E   1C                   ENDIF
114900000000     C*
115000000000     C* PRIMO GRUPPO DI 6 FUNZIONI
115100000000IF  1C     FUNGRU        IFEQ      1
115200000000     C                   MOVEL     FUV(1)        FUN001
115300000000     C                   MOVEL     FUV(2)        FUN002
115400000000     C                   MOVEL     FUV(3)        FUN003
115500000000     C                   MOVEL     FUV(4)        FUN004
115600000000     C                   MOVEL     FUV(5)        FUN005
115700000000     C                   MOVEL     FUV(6)        FUN006
115800000000E   1C                   ENDIF
115900000000     C*
116000000000     C* SECONDO GRUPPO DI 6 FUNZIONI
116100000000IF  1C     FUNGRU        IFEQ      2
116200000000     C                   MOVEL     FUV(7)        FUN001
116300000000     C                   MOVEL     FUV(8)        FUN002
116400000000     C                   MOVEL     FUV(9)        FUN003
116500000000     C                   MOVEL     FUV(10)       FUN004
116600000000     C                   MOVEL     FUV(11)       FUN005
116700000000     C                   MOVEL     FUV(12)       FUN006
116800000000E   1C                   ENDIF
116900000000     C*
117000000000     C* TERZO GRUPPO DI 6 FUNZIONI
117100000000IF  1C     FUNGRU        IFEQ      3
117200000000     C                   MOVEL     FUV(13)       FUN001
117300000000     C                   MOVEL     FUV(14)       FUN002
117400000000     C                   MOVEL     FUV(15)       FUN003
117500000000     C                   MOVEL     FUV(16)       FUN004
117600000000     C                   MOVEL     FUV(17)       FUN005
117700000000     C                   MOVEL     FUV(18)       FUN006
117800000000E   1C                   ENDIF
117900000000     C*
118000000000     C* QUARTO GRUPPO DI 6 FUNZIONI
118100000000IF  1C     FUNGRU        IFEQ      4
118200000000     C                   MOVEL     FUV(19)       FUN001
118300000000     C                   MOVEL     FUV(20)       FUN002
118400000000     C                   MOVEL     FUV(21)       FUN003
118500000000     C                   MOVEL     FUV(22)       FUN004
118600000000     C                   MOVEL     *BLANKS       FUN005
118700000000     C                   MOVEL     *BLANKS       FUN006
118800000000E   1C                   ENDIF
118900000000     C*
119000000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
119100000000     C                   MOVEL     *BLANKS       FUN007
119200000000     C                   MOVEL     *BLANKS       FUN008
119300000000DO  1C     1             DO        24            I
119400000000     C                   MOVEL     FUD(I)        FUDRIG
119500000000IF  2C     FUDFUN        IFEQ      '23'
119600000000     C     FUDATT        ANDEQ     'S'
119700000000     C                   MOVEL     FUDDES        FUN007
119800000000E   2C                   ENDIF
119900000000IF  2C     FUDFUN        IFEQ      '24'
120000000000     C     FUDATT        ANDEQ     'S'
120100000000     C                   MOVEL     FUDDES        FUN008
120200000000E   2C                   ENDIF
120300000000E   1C                   ENDDO
120400000000     C*
120500000000     C                   ENDSR
120600000000     C*------------------------------------------------------------------------*
120700000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
120800000000     C*------------------------------------------------------------------------*
120900000000     C     OPZGES        BEGSR
121000000000     C*
121100000000     C* PULISCE CAMPI DI LAVORO
121200000000     C                   Z-ADD     0             W                 5 0
121300000000     C*
121400000000     C* RICERCA QUANTE OPZIONI SONO ATTIVE E LE CARICA IN TABELLA
121500000000DO  1C     1             DO        24            I
121600000000     C                   MOVEL     OPD(I)        OPDRIG
121700000000     C*
121800000000     C* PULIZIA OPZIONI PRECEDENTI
121900000000     C                   MOVEL     *BLANKS       OPV(I)
122000000000     C*
122100000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
122200000000IF  2C     OPDATT        IFEQ      'S'
122300000000     C                   ADD       1             W
122400000000     C                   MOVEL     OPDDES        OPV(W)
122500000000E   2C                   ENDIF
122600000000E   1C                   ENDDO
122700000000     C*
122800000000     C* SE CI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVA FUNZIONE F23
122900000000DO  1C     1             DO        24            I
123000000000     C                   MOVEL     FUD(I)        FUDRIG
123100000000IF  2C     FUDFUN        IFEQ      '23'
123200000000IF  3C     W             IFGT      8
123300000000     C                   MOVEL     'S'           FUDATT
123400000000X   3C                   ELSE
123500000000     C                   MOVEL     *BLANKS       FUDATT
123600000000E   3C                   ENDIF
123700000000     C                   MOVEL     FUDRIG        FUD(I)
123800000000E   2C                   ENDIF
123900000000E   1C                   ENDDO
124000000000     C*
124100000000     C* IMPOSTA QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
124200000000     C     W             DIV       8             I
124300000000     C                   MVR                     Z                 5 0
124400000000IF  1C     Z             IFNE      0
124500000000     C     I             ADD       1             Z
124600000000X   1C                   ELSE
124700000000     C                   Z-ADD     I             Z
124800000000E   1C                   ENDIF
124900000000     C*
125000000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
125100000000     C                   ADD       1             OPZGRU
125200000000IF  1C     OPZGRU        IFGT      Z
125300000000     C     OPZGRU        ORLT      1
125400000000     C                   Z-ADD     1             OPZGRU
125500000000E   1C                   ENDIF
125600000000     C*
125700000000     C* PRIMO GRUPPO DI 8 OPZIONI
125800000000IF  1C     OPZGRU        IFEQ      1
125900000000     C                   MOVEL     OPV(1)        OPZ001
126000000000     C                   MOVEL     OPV(2)        OPZ002
126100000000     C                   MOVEL     OPV(3)        OPZ003
126200000000     C                   MOVEL     OPV(4)        OPZ004
126300000000     C                   MOVEL     OPV(5)        OPZ005
126400000000     C                   MOVEL     OPV(6)        OPZ006
126500000000     C                   MOVEL     OPV(7)        OPZ007
126600000000     C                   MOVEL     OPV(8)        OPZ008
126700000000E   1C                   ENDIF
126800000000     C*
126900000000     C* SECONDO GRUPPO DI 8 OPZIONI
127000000000IF  1C     OPZGRU        IFEQ      2
127100000000     C                   MOVEL     OPV(9)        OPZ001
127200000000     C                   MOVEL     OPV(10)       OPZ002
127300000000     C                   MOVEL     OPV(11)       OPZ003
127400000000     C                   MOVEL     OPV(12)       OPZ004
127500000000     C                   MOVEL     OPV(13)       OPZ005
127600000000     C                   MOVEL     OPV(14)       OPZ006
127700000000     C                   MOVEL     OPV(15)       OPZ007
127800000000     C                   MOVEL     OPV(16)       OPZ008
127900000000E   1C                   ENDIF
128000000000     C*
128100000000     C* TERZO GRUPPO DI 8 OPZIONI
128200000000IF  1C     OPZGRU        IFEQ      3
128300000000     C                   MOVEL     OPV(17)       OPZ001
128400000000     C                   MOVEL     OPV(18)       OPZ002
128500000000     C                   MOVEL     OPV(19)       OPZ003
128600000000     C                   MOVEL     OPV(20)       OPZ004
128700000000     C                   MOVEL     OPV(21)       OPZ005
128800000000     C                   MOVEL     OPV(22)       OPZ006
128900000000     C                   MOVEL     OPV(23)       OPZ007
129000000000     C                   MOVEL     OPV(24)       OPZ008
129100000000E   1C                   ENDIF
129200000000     C*
129300000000     C                   ENDSR
129400000000     C*------------------------------------------------------------------------*
129500000000     C* OPZCON - CONTROLLA ED ESEGUE OPZIONI SCELTE
129600000000     C*------------------------------------------------------------------------*
129700000000     C     OPZCON        BEGSR
129800000000     C*
129900000000     C* PULISCE CAMPI DI LAVORO
130000000000     C                   Z-ADD     0             I                 5 0
130100000000     C                   Z-ADD     0             W                 5 0
130200000000     C*
130300000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
130400000000DO  1C     1             DO        24            I
130500000000     C                   MOVEL     OPD(I)        OPDRIG
130600000000     C*
130700000000     C* SALVA INDICE DI OPZIONE TROVATA ED ATTIVA (W)
130800000000IF  2C     OPZKEY        IFEQ      OPDOPZ
130900000000     C     OPZKEY        OREQ      OPDOP1
131000000000IF  3C     OPDATT        IFEQ      'S'
131100000000     C                   MOVEL     OPDOPZ        OPZKEY
131200000000     C                   Z-ADD     I             W
131300000000E   3C                   ENDIF
131400000000E   2C                   ENDIF
131500000000E   1C                   ENDDO
131600000000     C*
131700000000     C* OPZIONE NON ATTIVA (ERRORE)
131800000000IF  1C     W             IFEQ      0
131900000000     C*
132000000000     C* OPZIONE NON OK
132100000000     C                   Z-ADD     1             OPZOK
132200000000     C*
132300000000     C* EMISSIONE MESSAGGIO DI ERRORE
132400000000     C                   MOVEL     MSG(1)        DSMSMS
132500000000     C*
132600000000     C* OPZIONE ATTIVA (PROCEDI ...)
132700000000X   1C                   ELSE
132800000000IF  2C     OPZKEY        IFEQ      ' 1'
132900000000     C                   EXSR      O01GES
133000000000E   2C                   ENDIF
133100000000IF  2C     OPZKEY        IFEQ      ' 2'
133200000000     C                   EXSR      O02GES
133300000000E   2C                   ENDIF
133400000000IF  2C     OPZKEY        IFEQ      ' 3'
133500000000     C                   EXSR      O03GES
133600000000E   2C                   ENDIF
133700000000IF  2C     OPZKEY        IFEQ      ' 4'
133800000000     C                   EXSR      O04GES
133900000000E   2C                   ENDIF
134000000000IF  2C     OPZKEY        IFEQ      ' 5'
134100000000     C                   EXSR      O05GES
134200000000E   2C                   ENDIF
134300000000IF  2C     OPZKEY        IFEQ      ' 6'
134400000000     C                   EXSR      O06GES
134500000000E   2C                   ENDIF
134600000000IF  2C     OPZKEY        IFEQ      ' 7'
134700000000     C                   EXSR      O07GES
134800000000E   2C                   ENDIF
134900000000E   1C                   ENDIF
135000000000     C*
135100000000     C                   ENDSR
135200000000     C*------------------------------------------------------------------------*
135300000000     C* NOTE PER LA COMPILAZIONE DELLE SCHIERE
135400000000     C*------------------------------------------------------------------------*
135500000000     C* -MSG-
135600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
135700000000     C* (NON TOCCARE MSG,1 E MSG,2
135800000000     C*
135900000000     C* -CMD-
136000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
136100000000     C*
136200000000     C* -OPD-
136300000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
136400000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
136500000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
136600000000     C*    ESEMPIO: OPZIONI -1 -
136700000000     C*                    - 1-  SONO LA STESSA COSA
136800000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
136900000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
137000000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
137100000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
137200000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
137300000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
137400000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
137500000000     C*
137600000000     C* -FUD-
137700000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
137800000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
137900000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
138000000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
138100000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
138200000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
138300000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
138400000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
138500000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
138600000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
138700000000     C*------------------------------------------------------------------------*
138800000000** MSG - MESSAGGI
138900000000TNSD55R- Opzione non gestita.                                                  1
139000000000TNSD55R- Tasto funzionale non gestito.                                         2
139100000000TNSD55R- Cartella non accettabile.                                             3
139200000000TNSD55R- Cartella non trovata.                                                 4
139300000000TNSD55R- Documento in forma non leggibile.                                     5
139400000000TNSD55R- Cartella vuota.                                                       6
139500000000TNSD55R- 7                                                                     7
139600000000TNSD55R- 8                                                                     8
139700000000TNSD55R- 9                                                                     9
139800000000TNSD55R- 10                                                                   10
139900000000TNSD55R- 11                                                                   11
140000000000TNSD55R- 12                                                                   12
140100000000TNSD55R- 13                                                                   13
140200000000TNSD55R- 14                                                                   14
140300000000TNSD55R- 15                                                                   15
140400000000TNSD55R- 16                                                                   16
140500000000TNSD55R- 17                                                                   17
140600000000TNSD55R- 18                                                                   18
140700000000TNSD55R- 19                                                                   19
140800000000TNSD55R- 20                                                                   20
140900000000** CMD - COMANDI CLP
141000000000WRKSPLF
141100971107CRTDUPOBJ OBJ(TNSD550F) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
141200971107CRTDUPOBJ OBJ(TNSD551L) FROMLIB(XXXXXXXXXX) OBJTYPE(*FILE) TOLIB(QTEMP)
141300000000DSPFLR FLR(XXXXXXXXXXXXXXXXXXXX) TYPE(*DOC) OUTPUT(*OUTFILE) OUTFILE(QTEMP/SD55)
141400000000CPYF FROMFILE(QTEMP/SD55) TOFILE(QTEMP/TNSD550F) MBROPT(*REPLACE) FMTOPT(*NOCHK)
141500000000DSPHLPDOC DOC(XXXXXXXXXXXX) FLR(XXXXXXXXXXXXXXXXXXXX)
141600000000CHKOBJ    OBJ(QTEMP/TNSD551L) OBJTYPE(*FILE)
141700000000DLTF      FILE(QTEMP/TNSD551L)
141800000000CHKOBJ    OBJ(QTEMP/TNSD550F) OBJTYPE(*FILE)
141900000000DLTF      FILE(QTEMP/TNSD550F)
14200096080111
142100960801PRTDOC DOC(XXXXXXXXXXXX) FLR(XXXXXXXXXXXXXXXXXXXX) DEV(*WRKSTN   ) HOLD(*YES)
14220000000013
14230000000014
14240000000015
142500000000** OPD - OPZIONI (ATTIVA=S)
142600000000                   -1 -     (S/N)=N                        opz. 01¬
142700000000                   -2 -     (S/N)=N                        opz. 02¬
142800000000                   -3 -     (S/N)=N                        opz. 03¬
142900000000                   -4 -     (S/N)=N                        opz. 04¬
143000960802 5=Visualizza      -5 -     (S/N)=S                        opz. 05¬
143100960801 6=Stampa          -6 -     (S/N)=S                        opz. 06¬
143200000000                   -7 -     (S/N)=N                        opz. 07¬
143300000000                   -8 -     (S/N)=N                        opz. 08¬
143400000000                   -9 -     (S/N)=N                        opz. 09¬
143500000000                   -  -     (S/N)=N                        opz. 10¬
143600000000                   -  -     (S/N)=N                        opz. 11¬
143700000000                   -  -     (S/N)=N                        opz. 12¬
143800000000                   -  -     (S/N)=N                        opz. 13¬
143900000000                   -  -     (S/N)=N                        opz. 14¬
144000000000                   -  -     (S/N)=N                        opz. 15¬
144100000000                   -  -     (S/N)=N                        opz. 16¬
144200000000                   -  -     (S/N)=N                        opz. 17¬
144300000000                   -  -     (S/N)=N                        opz. 18¬
144400000000                   -  -     (S/N)=N                        opz. 19¬
144500000000                   -  -     (S/N)=N                        opz. 20¬
144600000000                   -  -     (S/N)=N                        opz. 21¬
144700000000                   -  -     (S/N)=N                        opz. 22¬
144800000000                   -  -     (S/N)=N                        opz. 23¬
144900000000                   -  -     (S/N)=N                        opz. 24¬
145000000000** FUD - TASTI FUNZIONALI (ATTIVO=S)
145100000000                   -        (S/N)=N                        funz.01¬
145200000000                   -        (S/N)=N                        funz.02¬
145300000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
145400000000                   -        (S/N)=N                        funz.04¬
145500000000F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
145600000000                   -        (S/N)=N                        funz.06¬
145700000000                   -        (S/N)=N                        funz.07¬
145800000000                   -        (S/N)=N                        funz.08¬
145900000000                   -        (S/N)=N                        funz.09¬
146000000000 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.10¬
146100000000 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.11¬
146200000000F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
146300000000                   -        (S/N)=N                        funz.13¬
146400000000                   -        (S/N)=N                        funz.14¬
146500000000                   -        (S/N)=N                        funz.15¬
146600000000                   -        (S/N)=N                        funz.16¬
146700000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
146800000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
146900000000                   -        (S/N)=N                        funz.19¬
147000000000                   -        (S/N)=N                        funz.20¬
147100000000                   -        (S/N)=N                        funz.21¬
147200000000F22=Gestione stampe-        (S/N)=S                        funz.22¬
147300000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
147400000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
