000100980429     H*------------------------------------------------------------------------*
000200980429     H* GESTIONE DATI TIPO CODICI TASSAZIONE - SCELTA                          *
000300980429     H*------------------------------------------------------------------------*
000400980429     H DECEDIT('0,') DATEDIT(*DMY.)
000500980429     F*------------------------------------------------------------------------*
000600980429     F* DATA BASE
000700980429     F*------------------------------------------------------------------------*
000800980429     FTNTE36D   CF   E             WORKSTN INFDS(DEVDS1)
000900980429     F                                     SFILE(TE36D05:SFLULT)
001000980429     F                                     SFILE(TE36DMS:MSGULT)
001100011001     FTEETT01L  IF   E           K DISK
001200980429     FTABEL00F  IF   E           K DISK
001300041202     FTNVIS05L  IF   E           K DISK    usropn
001400980429     D*------------------------------------------------------------------------*
001500980429     D* SCHIERE
001600980429     D*------------------------------------------------------------------------*
001700980429     D*------------
001800980429     D* MESSAGGI
001900980429     D*------------
002000980429     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002100980429     D*------------
002200980429     D* DESCRIZIONI OPZIONI
002300980429     D*------------
002400980429     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002500980429     D*------------
002600980429     D* DESCRIZIONI TASTI FUNZIONALI
002700980429     D*------------
002800980429     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002900980429     D*------------
003000980429     D* OPZIONI DA CARICARE A VIDEO
003100980429     D*------------
003200980429     D OPV             S             19    DIM(24)
003300980429     D*------------
003400980429     D* FUNZIONI DA CARICARE A VIDEO
003500980429     D*------------
003600980429     D FUV             S             19    DIM(24)
003700980429     D*------------
003800980429     D* DECODIFICHE
003900980429     D*------------
004000980429     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
004100980429     D*------------
004200980429     D* RAGGRUPPAMENTI CODICI TASSAZIONE
004300980429     D*------------
004400980429     D RDES            S             20    DIM(10) CTDATA PERRCD(1)
004500980429     D*------------
004600980429     D* CODICI TASSAZIONE
004700980429     D*------------
004800050103     D CTS             S              2    DIM(600)                             *CODICE
004900050103     D CDES            S             25    DIM(600)                             *DESCRIZIONE
005000050103     D CRAP            S              2    DIM(600)                             *REGIONE
005100050103     D CNAZ            S              2    DIM(600)                             *NAZIONE
005200980429     D*------------------------------------------------------------------------*
005300980429     D* INPUT
005400980429     D*------------------------------------------------------------------------*
005500980429     D*------------
005600980429     D* RIDEFINIZIONE OPZIONI VIDEO
005700980429     D*------------
005800980429     D OPDDS           DS
005900980429     D  OPDOPZ                 1      2
006000980429     D  OPDDE1                 3     19
006100980429     D  OPDOP1                21     22
006200980429     D  OPDFI0                23     34
006300980429     D  OPDATT                35     35
006400980429     D  OPDFI1                36     80
006500980429     D  OPDDES                 1     19
006600980429     D  OPDRIG                 1     80
006700980429     D*------------
006800980429     D* RIDEFINIZIONE FUNZIONI VIDEO
006900980429     D*------------
007000980429     D FUDDS           DS
007100980429     D  FUDDE1                 1      1
007200980429     D  FUDFUN                 2      3
007300980429     D  FUDDE2                 4     19
007400980429     D  FUDFI0                20     34
007500980429     D  FUDATT                35     35
007600980429     D  FUDFI1                36     80
007700980429     D  FUDDES                 1     19
007800980429     D  FUDRIG                 1     80
007900980429     D*------------
008000980429     D* DS GESTIONE TASTI FUNZIONALI
008100980429     D*------------
008200980429     D DSKEY         E DS
008300980429     D DEVDS1          DS
008400980429     D  KEY                  369    369
008500980429     D*------------
008600980429     D* DS "XMSG" - GESTIONE MESSAGGI
008700980429     D*------------
008800980429     D DSMSG         E DS
008900980429     D*------------
009000980429     D* DS CODICI TASSAZIONE
009100980429     D*------------
009200980429     D DSCT          E DS
009300980429     D*------------
009400020923     D* DS REPERIMENTO DATI UTENTE
009500980429     D*------------
009600020923     D TIBS34DS      E DS                                                       *Profili utente
009700020923     D DDATIUTE      E DS                                                       *Dati utente
009800020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009900020925     D*------------
010000020925     D* DS CONTROLLO/SCELTA TABELLE
010100020925     D*------------
010200020925     D TIBS02DS      E DS                                                       *Ricerca tabella
010300020925     D*------------------
010400020925     D* DS ABILITAZIONI SPECIFICHE UTENTEI AZIENDALI
010500020925     D*-------------------
010600020925     D DUTE01        E DS                  INZ                                  *Abilitazioni utenti
010700020925     D*------------------
010800020925     D* DS FUNZIONI AZIENDALI
010900020925     D*-------------------
011000020925     D DLAT          E DS                  INZ                                  *Funzioni aziendali
011100020925     D*------------------
011200020925     D* DS CARICAMENTO P.O. GESTITI
011300020925     D*-------------------
011400020925     D TRUL31DS      E DS                  INZ
011500020925     D*------------------
011600020925     D* SCHIERA X LINEEE CONSENTITE
011700020925     D*------------------
011800020925     D L1              S              3    DIM(30)
011900980429     D*------------------
012000980429     D* DS "XSRDA8" - CONTROLLA DATA (8)
012100980429     D*------------------
012200980429     D WLBDA8          DS                  INZ
012300980429     D  G08DAT                 1      8  0
012400980429     D  G08INV                 9     16  0
012500980429     D  G08ERR                17     17
012600980429     D  G08TGI                18     22  0
012700980429     D*------------------
012800980429     D* DS SCOMPOSIZIONE DESCRIZIONE VIDEO
012900980429     D*------------------
013000980429     D                 DS
013100980429     D  D05RCT                11     25
013200980429     D  RDES05                 1     25
013300980429     D*------------
013400980429     D* DS DI PROCEDURA
013500980429     D*------------
013600980429     D DSTE30        E DS                  EXTNAME(TNTE30DS)
013700980429     D*------------
013800980429     D* ARCHITETTURA
013900980429     D*------------
014000980429     D KPJBA         E DS
014100980429     C*------------------------------------------------------------------------*
014200980429     C* MAIN LINES
014300980429     C*------------------------------------------------------------------------*
014400980429     C*
014500980429     C* SE NON CI SONO ERRORI, PROSEGUE
014600980429IF  1C     ERRL00        IFNE      '1'
014700980429     C*
014800980429     C* CARICA IL SFILE
014900980429     C                   EXSR      POSSFL
015000980429     C*
015100980429     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015200980429DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015300980429     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015400980429     C     FINL00        OREQ      '1'                                          *FINE PGM
015500980429     C*
015600980429     C* REIMPOSTA ERRORE DI PROCERURA
015700980429     C                   MOVEL     '0'           ERRL00
015800980429     C*
015900980429     C* EMETTE IL SFILE
016000980429     C                   EXSR      OUTSFL
016100980429     C*
016200980429     C* GESTIONE TASTI FUNZIONALI
016300980429     C                   EXSR      FUNCON
016400980429     C*
016500980429E   2C                   ENDDO
016600980429     C*
016700980429     C* CI SONO ERRORI IN ENTRATA
016800980429X   1C                   ELSE
016900980429     C                   MOVEL     '1'           D30ERR
017000980429     C                   MOVEL     DSMSMS        D30MSG
017100980429E   1C                   ENDIF
017200980429     C* FINE PGM
017300980429     C                   SETON                                        LR
017400980429     C*------------------------------------------------------------------------*
017500980429     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017600980429     C*------------------------------------------------------------------------*
017700980429     C     F03GES        BEGSR
017800980429     C*
017900980429     C* IMPOSTA "PREMUTO F03"
018000980429     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018100980429     C                   MOVEL     '1'           D30F03                         *DI PROCEDURA
018200980429     C*
018300980429     C                   ENDSR
018400980429     C*------------------------------------------------------------------------*
018500980429     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
018600980429     C*------------------------------------------------------------------------*
018700980429     C     F05GES        BEGSR
018800980429     C*
018900980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
019000980429     C*
019100980429     C                   ENDSR
019200980429     C*------------------------------------------------------------------------*
019300980429     C* F06GES - GESTIONE F06 -> INSERIMENTO
019400980429     C*------------------------------------------------------------------------*
019500980429     C     F06GES        BEGSR
019600980429     C*
019700980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019800980429     C                   MOVEL     *BLANKS       DCTS
019900980429     C                   MOVEL     *BLANKS       DFIE
020000980429     C                   MOVEL     DEPOP0        WOP0
020100980429     C                   MOVEL     'F06'         WOP1
020200980429     C                   MOVEL     'TNTE37R'     WPGM              8
020300980429     C                   EXSR      LANPGM                                       *LANCIA PGM
020400980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
020500980429E   1C                   ENDIF
020600980429     C*
020700980429     C                   ENDSR
020800980429     C*------------------------------------------------------------------------*
020900980429     C* F12GES - GESTIONE F12 -> RITORNO
021000980429     C*------------------------------------------------------------------------*
021100980429     C     F12GES        BEGSR
021200980429     C*
021300980429     C* IMPOSTA "PREMUTO F12"
021400980429     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021500980429     C                   MOVEL     '1'           D30F12                         *DI PROCEDURA
021600980429     C*
021700980429     C                   ENDSR
021800980429     C*------------------------------------------------------------------------*
021900980429     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022000980429     C*------------------------------------------------------------------------*
022100980429     C     F23GES        BEGSR
022200980429     C*
022300980429     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022400980429     C*
022500980429     C                   ENDSR
022600980429     C*------------------------------------------------------------------------*
022700980429     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022800980429     C*------------------------------------------------------------------------*
022900980429     C     F24GES        BEGSR
023000980429     C*
023100980429     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023200980429     C*
023300980429     C                   ENDSR
023400980429     C*------------------------------------------------------------------------*
023500980429     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023600980429     C*------------------------------------------------------------------------*
023700980429     C     RUPGES        BEGSR
023800980429     C*
023900980429     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024000980429     C*
024100980429     C                   ENDSR
024200980429     C*------------------------------------------------------------------------*
024300980429     C* ENTGES - GESTIONE ENTER -> INVIO
024400980429     C*------------------------------------------------------------------------*
024500980429     C     ENTGES        BEGSR
024600980429     C*
024700980429     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024800980429     C                   EXSR      NEWPOS
024900980429     C*
025000980429     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025100980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025200980429IF  2C     SWPOSI        IFNE      0
025300980429     C                   EXSR      POSSFL                                       *RICARICA SFILE
025400980429X   2C                   ELSE
025500980429IF  3C     SFLULT        IFNE      0
025600980429     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025700980429     C*
025800980429     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
025900980429IF  4C     SWMODI        IFNE      0
026000980429     C                   EXSR      POSSFL
026100980429E   4C                   ENDIF
026200980429X   3C                   ELSE
026300980429     C                   MOVEL     '1'           F12L00                         *FINE PROGRAMMA
026400980429     C                   MOVEL     '1'           D30F12                         *FINE PROCEDURA
026500980429E   3C                   ENDIF
026600980429E   2C                   ENDIF
026700980429E   1C                   ENDIF
026800980429     C*
026900980429     C                   ENDSR
027000980429     C*------------------------------------------------------------------------*
027100980429     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
027200980429     C*------------------------------------------------------------------------*
027300980429     C     O02GES        BEGSR
027400980429     C*
027500980429     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
027600980429IF  1C     V1SATB        IFEQ      'A'
027700980429     C                   MOVEL     '1'           ERRL00
027800980429     C                   MOVEL     MSG(4)        DSMSMS
027900980429E   1C                   ENDIF
028000980429     C* MODIFICA
028100980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028200980429     C                   MOVEL     V1SCTS        DCTS
028300980429     C                   MOVEL     V1SFIE        DFIE
028400980429     C                   MOVEL     DEPOP0        WOP0
028500980429     C                   MOVEL     'O02'         WOP1
028600980429     C                   MOVEL     'TNTE37R'     WPGM
028700980429     C                   EXSR      LANPGM
028800980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
028900980429E   1C                   ENDIF
029000980429     C*
029100980429     C                   ENDSR
029200980429     C*------------------------------------------------------------------------*
029300980429     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
029400980429     C*------------------------------------------------------------------------*
029500980429     C     O04GES        BEGSR
029600980429     C*
029700980429     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
029800980429IF  1C     V1SATB        IFEQ      'A'
029900980429     C                   MOVEL     '1'           ERRL00
030000980429     C                   MOVEL     MSG(4)        DSMSMS
030100980429E   1C                   ENDIF
030200980429     C* ANNULLAMENTO
030300980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030400980429     C                   MOVEL     V1SCTS        DCTS
030500980429     C                   MOVEL     V1SFIE        DFIE
030600980429     C                   MOVEL     DEPOP0        WOP0
030700980429     C                   MOVEL     'O04'         WOP1
030800980429     C                   MOVEL     'TNTE37R'     WPGM
030900980429     C                   EXSR      LANPGM
031000980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
031100980429E   1C                   ENDIF
031200980429     C*
031300980429     C                   ENDSR
031400980429     C*------------------------------------------------------------------------*
031500980429     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
031600980429     C*------------------------------------------------------------------------*
031700980429     C     O05GES        BEGSR
031800980429     C*
031900980429     C                   MOVEL     V1SCTS        DCTS
032000980429     C                   MOVEL     V1SFIE        DFIE
032100980429     C                   MOVEL     DEPOP0        WOP0
032200980429     C                   MOVEL     'O05'         WOP1
032300980429     C                   MOVEL     'TNTE37R'     WPGM
032400980429     C                   EXSR      LANPGM
032500980429     C*
032600980429     C                   ENDSR
032700980429     C*------------------------------------------------------------------------*
032800980429     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
032900980429     C*------------------------------------------------------------------------*
033000980429     C     O07GES        BEGSR
033100980429     C*
033200980429     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
033300980429IF  1C     V1SATB        IFNE      'A'
033400980429     C                   MOVEL     '1'           ERRL00
033500980429     C                   MOVEL     MSG(5)        DSMSMS
033600980429E   1C                   ENDIF
033700980429     C* RIPRISITINO
033800980429IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033900980429     C                   MOVEL     V1SCTS        DCTS
034000980429     C                   MOVEL     V1SFIE        DFIE
034100980429     C                   MOVEL     DEPOP0        WOP0
034200980429     C                   MOVEL     'O07'         WOP1
034300980429     C                   MOVEL     'TNTE37R'     WPGM
034400980429     C                   EXSR      LANPGM
034500980429     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
034600980429E   1C                   ENDIF
034700980429     C*
034800980429     C                   ENDSR
034900980429     C*------------------------------------------------------------------------*
035000980429     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
035100980429     C*------------------------------------------------------------------------*
035200980429     C     NEWPOS        BEGSR
035300980429     C*
035400980429     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
035500980429     C*         1= RICHIESTO POSIZIONAMENTO
035600980429     C                   Z-ADD     *ZEROS        SWPOSI            7 0
035700980429     C*
035800980429     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
035900980429     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
036000980429     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
036100980429     C                   Z-ADD     *ZEROS        SWMODI            1 0
036200980429     C*
036300980429     C* REIMPOSTA INDICATORI DI ERRORE
036400980429     C                   SETOFF                                       80
036500980429     C*
036600980429     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
036700980429IF  1C     V1CFIE        IFNE      DEPFIE
036800980429     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
036900980429IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
037000980429     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
037100980429     C                   MOVEL     V1CFIE        DEPFIE                         *DEPOSITI
037200980429E   2C                   ENDIF
037300980429E   1C                   ENDIF
037400980429     C*
037500980429     C                   ENDSR
037600980429     C*------------------------------------------------------------------------*
037700980429     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
037800980429     C*------------------------------------------------------------------------*
037900980429     C     CHKPAR        BEGSR
038000980429     C*
038100980429     C* DATI ITALIA/ESTERO/TUTTI
038200980429IF  1C     V1CFIE        IFEQ      *BLANKS
038300980429     C     V1CFIE        OREQ      'I'
038400980429     C     V1CFIE        OREQ      'E'
038500980429X   1C                   ELSE
038600980429     C                   MOVEL     '1'           ERRL00                         *ERRORE
038700980429E   1C                   ENDIF
038800980429     C*
038900980429     C                   ENDSR
039000980429     C*------------------------------------------------------------------------*
039100980429     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
039200980429     C*------------------------------------------------------------------------*
039300980429     C     SETFIL        BEGSR
039400980429     C*
039500980429     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
039600980429     C                   MOVEL     D30DSF        KTTDSF
039700980429     C                   MOVEL     D30CTO        KTTCTO
039800980429     C                   Z-ADD     D30KSC        KTTKSC
039900980429     C                   Z-ADD     D30CTR        KTTCTR
040000980429     C                   Z-ADD     D30PRG        KTTPRG
040100980429     C                   Z-ADD     D30LNP        KTTLNP
040200011001     C     KEYETT        SETLL     TEETT01L                           99
040300980429     C* LETTURA
040400980429IF  1C     *IN99         IFEQ      *OFF
040500980429     C                   EXSR      LETFIL
040600980429E   1C                   ENDIF
040700980429     C*
040800980429     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
040900980429IF  1C     *IN99         IFEQ      *ON
041000980429     C                   Z-ADD     1             SFLEOF
041100980429E   1C                   ENDIF
041200980429     C*
041300980429     C                   ENDSR
041400980429     C*------------------------------------------------------------------------*
041500980429     C* LETFIL - LETTURA RECORD
041600980429     C*------------------------------------------------------------------------*
041700980429     C     LETFIL        BEGSR
041800980429     C*
041900980429     C* LEGGE FINO A:
042000980429     C*     - FINE FILE
042100980429     C*     - TROVATO RECORD VALIDO
042200980429     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
042300980429DO  1C     *IN99         DOWEQ     *OFF
042400980429     C     WRECOK        ANDEQ     'N'
042500011001     C     KEYETT        READE     TEETT01L                               99
042600980429     C*
042700980429     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
042800980429IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
042900980429     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
043000980429X   2C                   ELSE
043100980429     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
043200980429E   2C                   ENDIF
043300980429E   1C                   ENDDO
043400980429     C*
043500980429     C                   ENDSR
043600980429     C*------------------------------------------------------------------------*
043700980429     C* CHKREC - CONTROLLA VALIDITA' RECORD
043800980429     C*------------------------------------------------------------------------*
043900980429     C     CHKREC        BEGSR
044000980429     C*
044100980429     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
044200980505     C*
044300980505     C* RECORD ANNULLATO, ESCLUDE
044400980505IF  1C     ETTATB        IFNE      *BLANKS
044500980505     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
044600980505E   1C                   ENDIF
044700980429     C*
044800980429     C* DATI ITALIA/ESTRO <> DA QUELLI RICHIESTI, ESCLUDE
044900980429IF  1C     V1CFIE        IFNE      *BLANKS
045000980429IF  2C     V1CFIE        IFNE      ETTFIE
045100980429     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
045200980429E   2C                   ENDIF
045300980429E   1C                   ENDIF
045400020313     C*
045500020313IF  1C     WRECOK        IFEQ      'S'
045600020313     C* OFFERTA
045700020313IF  2C     ETTCTO        IFEQ      'O'
045800020313     C     ETTKSC        CHAIN     TNVIS05L
045900020313     C                   IF        %found(TNVIS05L)
046000020313     C                   IF        VISKSC > *zeros
046100020925     C                   MOVEL     VISKSC        DEPFIL            3
046200020313     C                   ELSE
046300020313     C                   MOVEL     VISCMM        DEPFIL
046400020313     C                   ENDIF
046500020313     C     DEPFIL        LOOKUP    L1                                     55
046600020313     C                   IF        *IN55 = *OFF
046700020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046800020313     C                   ENDIF
046900020313     C                   ELSE
047000020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047100020313     C                   ENDIF
047200020313     C* TARIFFA
047300020313     C                   ELSE
047400020313     C* VERIFICO SE TRATTASI DI TARIFFA DI CARTELLO (VA SEMPRE BENE)
047500020313     C                   MOVEL     ETTKSC        DEPCART           5 0
047600020313     C                   IF        DEPCART = 88888
047700020313     C                   ELSE
047800020313     C                   MOVEL     ETTKSC        DEPFIL
047900020313     C     DEPFIL        LOOKUP    L1                                     55
048000020313     C                   IF        *IN55 = *OFF
048100020313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048200020313     C                   ENDIF
048300020313     C                   ENDIF
048400020313E   2C                   ENDIF
048500020313E   1C                   ENDIF
048600980429     C*
048700980429     C                   ENDSR
048800980429     C*------------------------------------------------------------------------*
048900980429     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
049000980429     C*------------------------------------------------------------------------*
049100980429     C     IRESFL        BEGSR
049200980429     C* CAMPI
049300980429     C                   MOVEL     *BLANKS       OPZKEY
049400980429     C                   MOVEL     ETTATB        V1SATB
049500980429     C                   MOVEL     ETTFIE        V1SFIE
049600980429     C                   MOVEL     ETTCTS        V1SCTS
049700980429     C                   MOVEL     ETTRCT        V1SRCT
049800980429     C                   MOVEL     ETTPPE        V1SPPE
049900980429     C* DECODIFICHE
050000980429     C                   SETOFF                                           28
050100980429     C                   Z-ADD     1             J
050200980429     C     ETTCTS        LOOKUP    CTS(J)                                 28
050300980429IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
050400980429     C                   MOVEL     CDES(J)       V1SDCT
050500980429X   1C                   ELSE                                                   *NON TROVATO
050600980429     C                   MOVEL     *ALL'*'       V1SDCT                         *DESCR.COD.TASSAZION
050700980429E   1C                   ENDIF
050800980429     C*
050900980429     C                   MOVEL     V1SCTS        WCTS
051000980429     C                   MOVEL     V1SRCT        WRCT
051100980429     C                   MOVEL     *BLANKS       WDRCT
051200980429     C                   EXSR      DECRCT
051300980429     C                   MOVEL     WDRCT         V1SDRC                         *DESCR.RAGGRUPPAMENT
051400980429     C*
051500980429     C                   ENDSR
051600980429     C*------------------------------------------------------------------------*
051700980429     C* DECRCT - DECODIFICA IL RAGGRUPPAMENTO CODICE TASSAZIONE
051800980429     C*------------------------------------------------------------------------*
051900980429     C     DECRCT        BEGSR
052000980429     C*
052100980429     C     *LIKE         DEFINE    V1SCTS        WCTS
052200980429     C     *LIKE         DEFINE    V1SRCT        WRCT
052300980429     C     *LIKE         DEFINE    V1SDRC        WDRCT
052400980429     C*
052500980429IF  1C     WRCT          IFEQ      ' '                                          *NON CALCOLARE
052600980429     C                   MOVEL     RDES(3)       WDRCT                          *DESCR.RAGGRUPPAMENT
052700980429E   1C                   ENDIF
052800980429IF  1C     WRCT          IFEQ      'T'                                          *CALCOLARE
052900980429     C                   MOVEL     RDES(4)       WDRCT                          *DESCR.RAGGRUPPAMENT
053000980429E   1C                   ENDIF
053100980429IF  1C     WRCT          IFEQ      'R'                                          *CALCOLARE IN REGION
053200980429     C     WRCT          OREQ      'N'                                          *CALCOLARE IN NAZION
053300980429     C                   MOVEL     RDES(5)       RDES05
053400980429     C                   SETOFF                                           98
053500980429     C                   Z-ADD     1             X                 5 0
053600980429     C     WCTS          LOOKUP    CTS(X)                                 98
053700980429     C                   SETOFF                                           98
053800980429     C                   Z-ADD     1             J                 5 0
053900980429IF  2C     WRCT          IFEQ      'R'
054000980429     C     CRAP(X)       LOOKUP    CTS(J)                                 98
054100980429X   2C                   ELSE
054200980429     C     CNAZ(X)       LOOKUP    CTS(J)                                 98
054300980429E   2C                   ENDIF
054400980429     C                   MOVEL     CDES(J)       D05RCT
054500980429     C                   MOVEL     RDES05        WDRCT                          *DESCR.RAGGRUPPAMENT
054600980429E   1C                   ENDIF
054700980429IF  1C     WRCT          IFEQ      'M'                                          *SITUAZIONE MISTA
054800980429     C                   MOVEL     RDES(6)       WDRCT                          *DESCR.RAGGRUPPAMENT
054900980429E   1C                   ENDIF
055000980429     C*
055100980429     C                   ENDSR
055200980429     C*------------------------------------------------------------------------*
055300980429     C* LANPGM - LANCIA UN PROGRAMMA
055400980429     C*------------------------------------------------------------------------*
055500980429     C     LANPGM        BEGSR
055600980429     C*
055700980429     C* IMPOSTA LA DS DI PROCEDURA
055800980429     C                   MOVEL     WOP0          D30OP0                         *LIVELLO PROCEDURA
055900980429     C                   MOVEL     WOP1          D30OP1                         *LIVELLO PGM
056000980429     C                   MOVEL     '0'           D30F03                         *PREMUTO F03=FINE
056100980429     C                   MOVEL     '0'           D30F12                         *PREMUTO F12=RITORNO
056200980429     C                   MOVEL     '0'           D30ERR                         *'1'=ERRORE
056300980429     C                   MOVEL     *BLANKS       D30MSG                         *MESSAGGIO DI RITORN
056400980429     C*
056500980429     C                   MOVEL     DCTS          D30CTS
056600980429     C                   MOVEL     DFIE          D30FIE
056700980429     C                   CALL      WPGM
056800980429     C                   PARM                    KPJBA
056900980429     C                   PARM                    DSTE30
057000980429     C*
057100980429     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
057200980429IF  1C     D30F03        IFEQ      '1'                                          *PREMUTO F03
057300980429     C                   MOVEL     '1'           ERRL00                         *ERRORE
057400980429     C                   MOVEL     '1'           F03L00                         *FINE PGM
057500980429X   1C                   ELSE                                                   *NON PREMUTO F03
057600980429     C                   MOVEL     D30MSG        DSMSMS                         *MESSAGGIO DI RITORN
057700980429E   1C                   ENDIF
057800980429     C*
057900980429     C                   ENDSR
058000980429     C*------------------------------------------------------------------------*
058100980429     C* CARTAB - CARICA LE TABELLE OCCORRENTI
058200980429     C*------------------------------------------------------------------------*
058300980429     C     CARTAB        BEGSR
058400980429     C*---
058500980429     C* CODICI TASSAZIONE
058600980429     C*---
058700980429     C                   Z-ADD     *ZEROS        J                 5 0
058800020923     C                   Z-ADD     1             KTBKUT
058900980429     C                   MOVEL     'CT'          KTBCOD
059000980429     C     KEYTAB        SETLL     TABEL00F
059100980429     C     KEYTAB        READE     TABEL00F                               99
059200980429DO  1C     *IN99         DOWEQ     *OFF
059300980429IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
059400980429     C                   ADD       1             J
059500980429     C                   MOVEL     TBLKEY        CTS(J)                         *CODICE
059600980429     C                   MOVEL     TBLUNI        DSCT
059700980429     C                   MOVEL     §CTDES        CDES(J)                        *DESCRIZIONE
059800980429     C                   MOVEL     §CTRAP        CRAP(J)                        *REGIONE
059900980429IF  3C     §CTNAR        IFEQ      *BLANKS
060000980429     C                   MOVEL     'IT'          CNAZ(J)                        *NAZIONE ITALIA
060100980429X   3C                   ELSE
060200980429     C                   MOVEL     'EE'          CNAZ(J)                        *NAZIONE ESTERO
060300980429E   3C                   ENDIF
060400980429E   2C                   ENDIF
060500980429     C     KEYTAB        READE     TABEL00F                               99
060600980429E   1C                   ENDDO
060700980429     C*
060800980429     C                   ENDSR
060900980429     C*------------------------------------------------------------------------*
061000980429     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
061100980429     C*------------------------------------------------------------------------*
061200980429     C     CHKFOP        BEGSR
061300980429     C* OPZIONI
061400980429     C*---                 MOVELOPD,5     OPDRIG           *INTERROGAZIONE
061500980429     C*---                 MOVEL'S'       OPDATT
061600980429     C*---                 MOVELOPDRIG    OPD,5
061700980429     C* FUNZIONI
061800980429     C*
061900980429     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
062000980429IF  1C     DEPOP0        IFEQ      'C02'
062100980429     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
062200980429     C                   MOVEL     'S'           OPDATT
062300980429     C                   MOVEL     OPDRIG        OPD(2)
062400980429     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
062500980429     C                   MOVEL     'S'           OPDATT
062600980429     C                   MOVEL     OPDRIG        OPD(4)
062700980429     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
062800980429     C                   MOVEL     'S'           OPDATT
062900980429     C                   MOVEL     OPDRIG        OPD(7)
063000980429     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
063100980429     C                   MOVEL     'S'           FUDATT
063200980429     C                   MOVEL     FUDRIG        FUD(6)
063300980429E   1C                   ENDIF
063400980429     C*
063500980429     C* GESTIONE OPZIONI A VIDEO
063600980429     C                   Z-ADD     *ZEROS        OPZGRU            5 0          *INDICE X OPZIONI
063700980429     C                   EXSR      OPZGES
063800980429     C*
063900980429     C* GESTIONE TASTI FUNZIONALI A VIDEO
064000980429     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
064100980429     C                   EXSR      FUNGES
064200980429     C*
064300980429     C                   ENDSR
064400020925     C*--------------------------------------------------------------------------------------------*
064500020925     C* REPERISCE DATI UTENTE
064600020925     C*--------------------------------------------------------------------------------------------*
064700020925     C     REPDATIUTE    BEGSR
064800020925     C*
064900020925     C* REimposta le variabili di lavoro
065000020925     C                   CLEAR                   TIBS34DS
065100020925     C                   CLEAR                   AZUTEDS
065200020925     C                   CLEAR                   DDATIUTE
065300020925     C                   CLEAR                   WABI              2
065400020925     C                   CLEAR                   DLAT
065500020925     C*
065600020925     C* Reperisce i dati del profilo in ingresso
065700020925     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
065800020925     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
065900020925     C                   IN(E)     *DTAARA
066000020925     C                   IF        %Error                                       *NO errori
066100020925     C                   EVAL      I34TLA = 'L'
066200020925     C                   CALL      'TIBS34R'
066300020925     C                   PARM                    TIBS34DS
066400020925     C                   IN        *DTAARA
066500020925     C                   ENDIF
066600020925     C*
066700020925     C* Se ci sono degli errori sull'abilitazione utente, esce
066800020925     C                   SELECT
066900020925     C                   WHEN      DUTERR = 'E'
067000020925     C                   EVAL      ERRL00 = '1'
067100020925     C                   EVAL      FINL00 = '1'
067200020925     C*
067300020925     C* NON siamo in sede (filiale remota o filiale locale)
067400020925     C* --> se 1° livello, abilitazioni al terminal
067500020925     C*     se 2° livello, abilitazioni al punto operativo
067600020925     C                   WHEN      UTEAUT = *blanks
067700020925     C                   IF        DUTLPO = '1'                                 *1° LIVELLO
067800020925     C                   EVAL      WABI   = 'TP'
067900020925     C                   ENDIF
068000020925     C                   IF        DUTLPO = '2'                                 *2° LIVELLO
068100020925     C                   EVAL      WABI   = 'PO'
068200020925     C                   ENDIF
068300020925     C*
068400020925     C* siamo in sede
068500020925     C* --> carica le abilitazioni proprie dell'appicazione "statistiche", o in mancanza
068600020925     C*     carica le abilitazioni proprie del profilo
068700020925     C                   OTHER
068800020925     C                   MOVEL     UTEFAF        DUTE01
068900060913     C                   IF        §UTECTC <> *blanks                           *esiste specifica
069000060913     C                   EVAL      WABI = §UTECTC
069100020925     C                   ELSE                                                   *NON esiste specific
069200020925     C                   EVAL      WABI = UTEAUT
069300020925     C                   ENDIF
069400020925     C                   ENDSL
069500020925     C*
069600020925     C* controlla se l'abilitazione dell'utente -anche quella forzata- sia valida
069700020925     C                   CLEAR                   TIBS02DS
069800020925     C                   MOVEL     'C'           T02MOD
069900020925     C                   MOVEL     KNSIF         T02SIF
070000020925     C                   MOVEL     'LAT'         T02COD
070100020925     C                   MOVEL(P)  WABI          T02KE1
070200020925     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
070300020925     C                   CALL      'TIBS02R'
070400020925     C                   PARM                    KPJBA
070500020925     C                   PARM                    TIBS02DS
070600020925     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
070700020925     C                   IF        T02ERR = *blanks
070800020925     C                   MOVEL     T02UNI        DLAT
070900020925     C                   ENDIF
071000020925     C                   IF        T02ERR <> *blanks OR
071100020925     C                             §LATABI = 'S'
071200020925     C                   EVAL      ERRL00  = '1'
071300020925     C                   EVAL      FINL00  = '1'
071400020925     C                   ENDIF
071500020925     C*
071600020925     C* Reperisce i P.O. gestibili dall'utente
071700020925     C                   IF        ERRL00 <> '1'                                  *NO errori
071800020925     C                   CLEAR                   TRUL31DS
071900020925     C                   EVAL      I31ABI   = WABI
072000020925     C                   EVAL      I31CDI   = DUTDIS
072100020925     C                   EVAL      I31CAR   = DUTARE
072200020925     C                   EVAL      I31CPO   = DUTPOU
072300020925     C                   EVAL      DEPKPJBU = KPJBU                             *deposito KPJ
072400020925     C                   CALL      'TRUL31R'
072500020925     C                   PARM                    KPJBA
072600020925     C                   PARM                    TRUL31DS
072700020925     C                   EVAL      KPJBU = DEPKPJBU                             *RIdeposito KPJ
072800020925if  2C                   IF        O31POG > *zeros
072900020925     C                   MOVEA     O31POG        L1
073000020925x   2C                   ELSE
073100020925     C                   EVAL      ERRL00 = '1'
073200020925     C                   EVAL      FINL00 = '1'
073300020925e   2C                   ENDIF
073400020925e   1C                   ENDIF
073500020925     C*
073600020925     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
073700020925     C                   MOVEL(P)  RSUT          DSFIRS           20
073800020925     C*
073900020925     C                   ENDSR
074000980429     C*------------------------------------------------------------------------*
074100980429     C* *INZSR - OPERAZIONI INIZIALI
074200980429     C*------------------------------------------------------------------------*
074300980429     C     *INZSR        BEGSR
074400980429     C*---
074500980429     C* RICEVIMENTO PARAMETRI
074600980429     C*---
074700980429     C     *ENTRY        PLIST
074800980429     C                   PARM                    KPJBA
074900980429     C                   PARM                    DSTE30
075000041202     c                   open      tnvis05l                             95
075100980429     C*
075200980429     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
075300980429     C     XMSGPA        PLIST
075400980429     C                   PARM                    DSMSMS
075500980429     C                   PARM                    DSMSPG
075600980429     C                   PARM                    DSMSKE
075700980429     C                   PARM                    DSMSER
075800980429     C*---
075900980429     C* VARIABILI RIFERITE AL DATA BASE
076000980429     C*---
076100011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
076200980429     C     *LIKE         DEFINE    ETTCTO        KTTCTO
076300980429     C     *LIKE         DEFINE    ETTKSC        KTTKSC
076400980429     C     *LIKE         DEFINE    ETTCTR        KTTCTR
076500980429     C     *LIKE         DEFINE    ETTPRG        KTTPRG
076600980429     C     *LIKE         DEFINE    ETTLNP        KTTLNP
076700980429     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
076800980429     C     *LIKE         DEFINE    TBLCOD        KTBCOD
076900980429     C     *LIKE         DEFINE    ETTFIE        DEPFIE                         *DEPOSITI
077000980429     C     *LIKE         DEFINE    D30OP0        DEPOP0                         *DI DEPOSITO
077100980429     C     *LIKE         DEFINE    D30OP1        DEPOP1
077200980429     C     *LIKE         DEFINE    D30OP0        WOP0                           *DI LAVORO
077300980429     C     *LIKE         DEFINE    D30OP1        WOP1
077400980429     C     *LIKE         DEFINE    D30FIE        DFIE
077500980429     C     *LIKE         DEFINE    D30CTS        DCTS
077600020925     C     *LIKE         DEFINE    KPJBU         DEPKPJBU
077700980429     C*---
077800980429     C* VARIABILI  N O N  RIFERITE AL DATA BASE
077900980429     C*---
078000980429     C     *LIKE         DEFINE    SFLNBR        SFLPAG
078100980429     C     *LIKE         DEFINE    SFLPAG        WFLPAG
078200980429     C     *LIKE         DEFINE    SFLNBR        SFLEOF
078300980429     C     *LIKE         DEFINE    SFLNBR        SFLERR
078400980429     C     *LIKE         DEFINE    SFLNBR        SFLULT
078500980429     C     *LIKE         DEFINE    SFLNBR        WFLULT
078600980429     C     *LIKE         DEFINE    SFLNBR        MSGULT
078700980429     C     *LIKE         DEFINE    SFLNBR        SFLRIG
078800980429     C     *LIKE         DEFINE    FUDFUN        FUNKEY
078900980429     C*---
079000980429     C* CHIAVI DI LETTURA
079100980429     C*---
079200980429     C     KEYTAB        KLIST                                                  *TABEL00F
079300980429     C                   KFLD                    KTBKUT                          -UTENTE
079400980429     C                   KFLD                    KTBCOD                          -TABELLA
079500011001     C     KEYETT        KLIST                                                  *TEETT01L
079600980429     C                   KFLD                    KTTDSF                          -SEDE/FILIALE
079700980429     C                   KFLD                    KTTCTO                          -TARIFFA/OFFERTA
079800980429     C                   KFLD                    KTTKSC                          -CLIENTE/VISITA
079900980429     C                   KFLD                    KTTCTR                          -TARIFFA
080000980429     C                   KFLD                    KTTPRG                          -PROGRESSIVO
080100980429     C                   KFLD                    KTTLNP                          -LINEA PARTENZA
080200980429     C*---
080300980429     C* VARIABILI DI CONTROLLO
080400980429     C*---
080500980429     C                   MOVEL     '0'           FINL00            1            *FINE PGM
080600980429     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
080700980429     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
080800980429     C                   MOVEL     '0'           ERRL00            1            *ERRORE
080900980429     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
081000980429     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
081100980429     C*
081200980429     C* IMPOSTA LA DS -INVIO MESSAGGI-
081300980429     C                   MOVEL     'TNTE36R'     DSMSPG
081400980429     C                   MOVEL     'TNTE36R'     MSGQUE
081500980429     C                   MOVEL     *BLANKS       MSGKEY
081600980429     C                   MOVEL     *BLANKS       DSMSMS
081700980429     C                   MOVEL     *BLANKS       DSMSKE
081800980429     C                   Z-ADD     *ZEROS        DSMSER
081900980429     C*
082000980429     C* IMPOSTA LA DS -TASTI PREMUTI-
082100980429     C                   CALL      'XKEY'
082200980429     C                   PARM                    DSKEY
082300980429     C*
082400980429     C* REPERIMENTO DATI UTENTE
082500020923     C                   EXSR      REPDATIUTE
082600980429     C*
082700980429     C* CARICA LE TABELLE OCCORRENTI
082800980429     C                   EXSR      CARTAB
082900980429     C*
083000980429     C* DEPOSITA LE OPZIONI DI ENTRATA
083100980429     C                   MOVEL     D30OP0        DEPOP0                         *LIVELLO PROCEDURA
083200980429     C                   MOVEL     D30OP1        DEPOP1                         *LIVELLO PROGRAMMA
083300980429     C*
083400980429     C* CALCOLA LA DATA CORRENTE
083500980429     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
083600980429     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
083700980429     C                   Z-ADD     WN8           G08DAT
083800980429     C                   Z-ADD     *ZEROS        G08INV
083900980429     C                   MOVEL     '0'           G08ERR
084000980429     C                   CALL      'XSRDA8'
084100980429     C                   PARM                    WLBDA8
084200980429     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
084300980429     C*
084400980429     C* IMPOSTA I PARAMETRI DI INGRESSO
084500980429     C                   MOVEL     D30DSF        V1CDSF
084600980429     C                   MOVEL     D30CTO        V1CCTO
084700980429     C                   Z-ADD     D30KSC        V1CKSC
084800980429     C                   Z-ADD     D30CTR        V1CCTR
084900980429     C                   Z-ADD     D30PRG        V1CPRG
085000980429     C                   Z-ADD     D30LNP        V1CLNP
085100980429     C                   MOVEL     D30FIE        V1CFIE
085200011003     C                   MOVEL     D30DIV        V1CDIV
085300980429     C                   EXSR      CHKPAR
085400980429IF  1C     ERRL00        IFEQ      '1'
085500980429     C                   MOVEL     '1'           D30ERR
085600980429     C                   MOVEL     DSMSMS        D30MSG
085700980429X   1C                   ELSE
085800980429     C                   MOVEL     V1CFIE        DEPFIE                         *DEPOSITI
085900980429E   1C                   ENDIF
086000980429     C*
086100980429     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
086200980429     C                   EXSR      CHKFOP
086300980429     C*
086400980429     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
086401060912IF  1C     DEPOP0        IFEQ      'C01'                                        *SCELTA
086402060912     C                   MOVEL     DEC(3)        VIDING
086403060912E   1C                   ENDIF
086500980429IF  1C     DEPOP0        IFEQ      'C02'                                        *INTERROGAZIONE
086600980429     C                   MOVEL     DEC(1)        VIDING
086700980429E   1C                   ENDIF
086800980429IF  1C     DEPOP0        IFEQ      'C05'                                        *GESTIONE
086900980429     C                   MOVEL     DEC(2)        VIDING
087000980429E   1C                   ENDIF
087100980429     C*
087200980429     C                   ENDSR
087300980429     C*------------------------------------------------------------------------*
087400980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
087500980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
087600980429     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
087700980429     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
087800980429     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
087900980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
088000980429     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
088100980429     C*------------------------------------------------------------------------*
088200980429     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
088300980429     C*------------------------------------------------------------------------*
088400980429     C     POSSFL        BEGSR
088500980429     C*
088600980429     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
088700980429     C                   EXSR      CLRSFL
088800980429     C*
088900980429     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
089000980429     C                   EXSR      SETFIL
089100980429     C*
089200980429     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
089300980429     C     SFLEOF        IFEQ      0
089400980429     C                   EXSR      CARSFL
089500980429     C                   END
089600980429     C*
089700980429     C                   ENDSR
089800980429     C*------------------------------------------------------------------------*
089900980429     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
090000980429     C*------------------------------------------------------------------------*
090100980429     C     ENTATT        BEGSR
090200980429     C*
090300980429     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
090400980429     C                   Z-ADD     0             OPZOK             1 0
090500980429     C*
090600980429     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
090700980429     C                   Z-ADD     0             SFLVAR            5 0
090800980429     C*
090900980429     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
091000980429     C                   Z-ADD     0             SFLERR
091100980429     C*
091200980429     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
091300980429     C                   Z-ADD     SFLULT        WFLULT
091400980429     C*
091500980429     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
091600980429     C                   READC     TE36D05                                98
091700980429     C     *IN98         DOWEQ     '0'
091800980429     C*
091900980429     C* ADD CONTATORE DI RIGHE SFL VARIATE
092000980429     C                   ADD       1             SFLVAR
092100980429     C*
092200980429     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
092300980429     C     OPZOK         IFEQ      0
092400980429     C*
092500980429     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
092600980429     C     OPZKEY        IFNE      *BLANKS
092700980429     C                   EXSR      OPZCON
092800980429     C                   END
092900980429     C*
093000980429     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
093100980429     C*            2. PULIZIA SCELTA IN SFL
093200980429     C     OPZOK         IFEQ      0
093300980429     C                   MOVEL     '0'           *IN41
093400980429     C                   MOVEL     *BLANKS       OPZKEY
093500980429     C                   ELSE
093600980429     C*
093700980429     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
093800980429     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
093900980429     C                   MOVEL     '1'           *IN41
094000980429     C                   Z-ADD     SFLULT        SFLERR
094100980429     C                   END
094200980429     C                   ELSE
094300980429     C*
094400980429     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
094500980429     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
094600980429     C                   MOVEL     '1'           *IN41
094700980429     C                   END                                                    OPZOK=0
094800980429     C*
094900980429     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
095000980429     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
095100980429     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
095200980429     C     OPZOK         IFNE      0
095300980429     C                   Z-ADD     SFLERR        SFLNBR
095400980429     C                   ELSE
095500980429     C                   Z-ADD     SFLULT        SFLNBR
095600980429     C                   END
095700980429     C*
095800980429     C* AGGIORNO RECORD SFL
095900980429     C                   UPDATE    TE36D05
096000980429     C*
096100980429     C                   READC     TE36D05                                98
096200980429     C                   END                                                    DO WHILE
096300980429     C*
096400980429     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
096500980429     C                   Z-ADD     WFLULT        SFLULT
096600980429     C*
096700980429     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
096800980429     C     SFLVAR        IFEQ      0
096900980429     C                   MOVEL     '1'           F12L00                         *FINE PROGRAMMA
097000980429     C                   MOVEL     '1'           D30F12                         *FINE PROCEDURA
097100980429     C                   END
097200980429     C*
097300980429     C                   ENDSR
097400980429     C*------------------------------------------------------------------------*
097500980429     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
097600980429     C*------------------------------------------------------------------------*
097700980429     C     CLRSFL        BEGSR
097800980429     C*
097900980429     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
098000980429     C* (0=NON FINE FILE  1=FINE FILE)
098100980429     C                   Z-ADD     0             SFLEOF
098200980429     C*
098300980429     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
098400980429     C                   Z-ADD     0             SFLULT
098500980429     C*
098600980429     C* ACCENSIONE INDICATORE PER CLEAR SFL
098700980429     C                   MOVEL     '1'           *IN40
098800980429     C                   WRITE     TE36D06
098900980429     C                   MOVEL     '0'           *IN40
099000980429     C*
099100980429     C                   ENDSR
099200980429     C*------------------------------------------------------------------------*
099300980429     C* CARSFL - CARICAMENTO SFL
099400980429     C*------------------------------------------------------------------------*
099500980429     C     CARSFL        BEGSR
099600980429     C*
099700980429     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
099800980429     C     SFLULT        IFEQ      0
099900980429     C                   EXSR      SETFIL
100000980429     C                   END
100100980429     C*
100200980429     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
100300980429     C                   Z-ADD     SFLULT        WFLULT
100400980429     C*
100500980429     C* CICLO:
100600980429     C*    -CARICO NUMERO DI RIGHE=SFLPAG
100700980429     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
100800980429     C                   Z-ADD     1             SFLRIG
100900980429     C     SFLRIG        DOWLE     SFLPAG
101000980429     C     SFLEOF        ANDEQ     0
101100980429     C*
101200980429     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
101300980429     C                   MOVEL     *BLANKS       OPZKEY
101400980429     C*
101500980429     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
101600980429     C                   EXSR      IRESFL
101700980429     C*
101800980429     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
101900980429     C                   ADD       1             SFLULT
102000980429     C*
102100980429     C* SCRITTURA RECORD SFL
102200980429     C                   WRITE     TE36D05
102300980429     C                   ADD       1             SFLRIG
102400980429     C*
102500980429     C* LETTURA FILE DA CARICARE
102600980429     C                   EXSR      LETFIL
102700980429     C                   END                                                    DO WHILE
102800980429     C*
102900980429     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
103000980429     C     WFLULT        IFNE      SFLULT
103100980429     C                   Z-ADD     WFLULT        SFLNBR
103200980429     C                   ADD       1             SFLNBR
103300980429     C                   END
103400980429     C*
103500980429     C                   ENDSR
103600980429     C*------------------------------------------------------------------------*
103700980429     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
103800980429     C*------------------------------------------------------------------------*
103900980429     C     OUTSFL        BEGSR
104000980429     C*
104100980429     C* WRITE RECORD TESTATA VIDEO
104200980429     C                   WRITE     TE36D01
104300980429     C*
104400980429     C* VISUALIZZAZIONE MESSAGGIO
104500980429     C                   EXSR      SNDMSG
104600980429     C*
104700980429     C* WRITE RECORD OPZIONI VIDEO
104800980429     C                   WRITE     TE36D02
104900980429     C*
105000980429     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
105100980429     C                   WRITE     TE36D03
105200980429     C*
105300980429     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
105400980429     C                   WRITE     TE36D04
105500980429     C*
105600980429     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
105700980429     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
105800980429     C     SFLEOF        IFEQ      0
105900980429     C                   MOVEL     '0'           *IN42
106000980429     C                   ELSE
106100980429     C*
106200980429     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
106300980429     C                   MOVEL     '1'           *IN42
106400980429     C                   END
106500980429     C*
106600980429     C* WRITE RECORD TASTI FUNZIONALI
106700980429     C                   WRITE     TE36D09
106800980429     C*
106900980429     C* CASO DI SFL VUOTO
107000980429     C     SFLULT        IFLE      0
107100980429     C*
107200980429     C*    - WRITE RECORD VUOTO
107300980429     C                   WRITE     TE36D0A
107400980429     C                   READ      TE36D0A                                99
107500980429     C*
107600980429     C                   ELSE
107700980429     C*
107800980429     C* CASO DI SFL CON RECORDS
107900980429     C*
108000980429     C*    - WRITE RECORD CONTROLLO SFL
108100980429     C                   WRITE     TE36D06
108200980429     C                   READ      TE36D06                                99
108300980429     C                   END
108400980429     C*
108500980429     C* READ RECORD PER POSIZIONAMEMTO VIDEO
108600980429     C                   READ      TE36D03                                99
108700980429     C*
108800980429     C                   ENDSR
108900980429     C*------------------------------------------------------------------------*
109000980429     C* SNDMSG - MANDA MESSAGGIO
109100980429     C*------------------------------------------------------------------------*
109200980429     C     SNDMSG        BEGSR
109300980429     C*
109400980429     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
109500980429     C                   MOVEL     '1'           *IN40
109600980429     C                   WRITE     TE36DMC
109700980429     C                   MOVEL     '0'           *IN40
109800980429     C*
109900980429     C* VISUALIZZAZIONE MESSAGGIO
110000980429     C                   Z-ADD     0             MSGULT
110100980429     C                   CALL      'XMSG'        XMSGPA
110200980429     C     DSMSER        IFEQ      0
110300980429     C                   MOVEL     DSMSKE        MSGKEY
110400980429     C                   END
110500980429     C                   Z-ADD     1             MSGULT
110600980429     C                   WRITE     TE36DMS                                      SFL MESS
110700980429     C                   WRITE     TE36DMC                                      CONTR MESS
110800980429     C*
110900980429     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
111000980429     C                   MOVEL     *BLANKS       DSMSMS
111100980429     C*
111200980429     C                   ENDSR
111300980429     C*------------------------------------------------------------------------*
111400980429     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
111500980429     C*------------------------------------------------------------------------*
111600980429     C     FUNCON        BEGSR
111700980429     C*
111800980429     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
111900980429     C                   EXSR      FUNVAR
112000980429     C*
112100980429     C* PULISCO CAMPI DI LAVORO
112200980429     C                   Z-ADD     0             I                 5 0
112300980429     C                   Z-ADD     0             W                 5 0
112400980429     C*
112500980429     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
112600980429     C     FUNKEY        IFNE      *BLANKS
112700980429     C     1             DO        24            I
112800980429     C                   MOVEL     FUD(I)        FUDRIG
112900980429     C*
113000980429     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
113100980429     C     FUNKEY        IFEQ      FUDFUN
113200980429     C     FUDATT        ANDEQ     'S'
113300980429     C                   Z-ADD     I             W
113400980429     C                   END
113500980429     C                   END                                                    DO WHILE
113600980429     C*
113700980429     C* FUNZIONE NON ATTIVA (ERRORE)
113800980429     C     W             IFEQ      0
113900980429     C*
114000980429     C* EMISSIONE MESSAGGIO DI ERRORE
114100980429     C                   MOVEL     MSG(2)        DSMSMS
114200980429     C                   END                                                    W=0
114300980429     C                   END                                                    FUNKEY NE *BLANKS
114400980429     C*
114500980429     C* FUNZIONE ATTIVA (PROCEDI ...)
114600980429     C     W             IFNE      0
114700980429     C     FUNKEY        OREQ      *BLANKS
114800980429     C*
114900980429     C     FUNKEY        IFEQ      *BLANKS
115000980429     C                   EXSR      ENTGES
115100980429     C                   END
115200980429     C     FUNKEY        IFEQ      '03'
115300980429     C                   EXSR      F03GES
115400980429     C                   END
115500980429     C     FUNKEY        IFEQ      '05'
115600980429     C                   EXSR      F05GES
115700980429     C                   END
115800980429     C     FUNKEY        IFEQ      '06'
115900980429     C                   EXSR      F06GES
116000980429     C                   END
116100980429     C     FUNKEY        IFEQ      '12'
116200980429     C                   EXSR      F12GES
116300980429     C                   END
116400980429     C     FUNKEY        IFEQ      '23'
116500980429     C                   EXSR      F23GES
116600980429     C                   END
116700980429     C     FUNKEY        IFEQ      '24'
116800980429     C                   EXSR      F24GES
116900980429     C                   END
117000980429     C     FUNKEY        IFEQ      'RU'
117100980429     C                   EXSR      RUPGES
117200980429     C                   END
117300980429     C                   END
117400980429     C*
117500980429     C                   ENDSR
117600980429     C*------------------------------------------------------------------------*
117700980429     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
117800980429     C*------------------------------------------------------------------------*
117900980429     C     FUNVAR        BEGSR
118000980429     C*
118100980429     C* FUNKEY=*BLANKS SIGNIFICA ENTER
118200980429     C     KEY           IFEQ      ENTER
118300980429     C                   MOVEL     *BLANKS       FUNKEY
118400980429     C                   END
118500980429     C     KEY           IFEQ      F1
118600980429     C                   MOVEL     '01'          FUNKEY
118700980429     C                   END
118800980429     C     KEY           IFEQ      F2
118900980429     C                   MOVEL     '02'          FUNKEY
119000980429     C                   END
119100980429     C     KEY           IFEQ      F3
119200980429     C                   MOVEL     '03'          FUNKEY
119300980429     C                   END
119400980429     C     KEY           IFEQ      F4
119500980429     C                   MOVEL     '04'          FUNKEY
119600980429     C                   END
119700980429     C     KEY           IFEQ      F5
119800980429     C                   MOVEL     '05'          FUNKEY
119900980429     C                   END
120000980429     C     KEY           IFEQ      F6
120100980429     C                   MOVEL     '06'          FUNKEY
120200980429     C                   END
120300980429     C     KEY           IFEQ      F7
120400980429     C                   MOVEL     '07'          FUNKEY
120500980429     C                   END
120600980429     C     KEY           IFEQ      F8
120700980429     C                   MOVEL     '08'          FUNKEY
120800980429     C                   END
120900980429     C     KEY           IFEQ      F9
121000980429     C                   MOVEL     '09'          FUNKEY
121100980429     C                   END
121200980429     C     KEY           IFEQ      F10
121300980429     C                   MOVEL     '10'          FUNKEY
121400980429     C                   END
121500980429     C     KEY           IFEQ      F11
121600980429     C                   MOVEL     '11'          FUNKEY
121700980429     C                   END
121800980429     C     KEY           IFEQ      F12
121900980429     C                   MOVEL     '12'          FUNKEY
122000980429     C                   END
122100980429     C     KEY           IFEQ      F13
122200980429     C                   MOVEL     '13'          FUNKEY
122300980429     C                   END
122400980429     C     KEY           IFEQ      F14
122500980429     C                   MOVEL     '14'          FUNKEY
122600980429     C                   END
122700980429     C     KEY           IFEQ      F15
122800980429     C                   MOVEL     '15'          FUNKEY
122900980429     C                   END
123000980429     C     KEY           IFEQ      F16
123100980429     C                   MOVEL     '16'          FUNKEY
123200980429     C                   END
123300980429     C     KEY           IFEQ      F17
123400980429     C                   MOVEL     '17'          FUNKEY
123500980429     C                   END
123600980429     C     KEY           IFEQ      F18
123700980429     C                   MOVEL     '18'          FUNKEY
123800980429     C                   END
123900980429     C     KEY           IFEQ      F19
124000980429     C                   MOVEL     '19'          FUNKEY
124100980429     C                   END
124200980429     C     KEY           IFEQ      F20
124300980429     C                   MOVEL     '20'          FUNKEY
124400980429     C                   END
124500980429     C     KEY           IFEQ      F21
124600980429     C                   MOVEL     '21'          FUNKEY
124700980429     C                   END
124800980429     C     KEY           IFEQ      F22
124900980429     C                   MOVEL     '22'          FUNKEY
125000980429     C                   END
125100980429     C     KEY           IFEQ      F23
125200980429     C                   MOVEL     '23'          FUNKEY
125300980429     C                   END
125400980429     C     KEY           IFEQ      F24
125500980429     C                   MOVEL     '24'          FUNKEY
125600980429     C                   END
125700980429     C     KEY           IFEQ      ROLLUP
125800980429     C                   MOVEL     'RU'          FUNKEY
125900980429     C                   END
126000980429     C     KEY           IFEQ      HELP
126100980429     C                   MOVEL     'HP'          FUNKEY
126200980429     C                   END
126300980429     C*
126400980429     C                   ENDSR
126500980429     C*------------------------------------------------------------------------*
126600980429     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
126700980429     C*------------------------------------------------------------------------*
126800980429     C     FUNGES        BEGSR
126900980429     C*
127000980429     C* PULISCO CAMPI DI LAVORO
127100980429     C                   Z-ADD     0             W                 5 0
127200980429     C*
127300980429     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
127400980429     C* E CARICO TABELLA FUNZIONI ATTIVE
127500980429     C     1             DO        24            I
127600980429     C                   MOVEL     FUD(I)        FUDRIG
127700980429     C*
127800980429     C* PULIZIA FUNZIONI PRECEDENTI
127900980429     C                   MOVEL     *BLANKS       FUV(I)
128000980429     C*
128100980429     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
128200980429     C* (SALTA F23 E F24)
128300980429     C     FUDATT        IFEQ      'S'
128400980429     C     FUDFUN        ANDNE     '23'
128500980429     C     FUDFUN        ANDNE     '24'
128600980429     C                   ADD       1             W
128700980429     C                   MOVEL     FUDDES        FUV(W)
128800980429     C                   END
128900980429     C                   END                                                    DO WHILE
129000980429     C*
129100980429     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
129200980429     C* ALTRIMENTI DISATTIVO F24
129300980429     C     1             DO        24            I
129400980429     C                   MOVEL     FUD(I)        FUDRIG
129500980429     C*
129600980429     C     FUDFUN        IFEQ      '24'
129700980429     C     W             IFGT      6
129800980429     C                   MOVEL     'S'           FUDATT
129900980429     C                   ELSE
130000980429     C                   MOVEL     *BLANKS       FUDATT
130100980429     C                   END                                                    W GT 8
130200980429     C                   MOVEL     FUDRIG        FUD(I)
130300980429     C                   END
130400980429     C                   END                                                    DO WHILE
130500980429     C*
130600980429     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
130700980429     C     W             DIV       6             I
130800980429     C                   MVR                     Z
130900980429     C*
131000980429     C     Z             IFNE      0
131100980429     C     I             ADD       1             Z
131200980429     C                   ELSE
131300980429     C                   Z-ADD     I             Z
131400980429     C                   END
131500980429     C*
131600980429     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
131700980429     C                   ADD       1             FUNGRU
131800980429     C     FUNGRU        IFGT      Z
131900980429     C     FUNGRU        ORLT      1
132000980429     C                   Z-ADD     1             FUNGRU
132100980429     C                   END
132200980429     C*
132300980429     C* PRIMO GRUPPO DI 6 FUNZIONI
132400980429     C     FUNGRU        IFEQ      1
132500980429     C                   MOVEL     FUV(1)        FUN001
132600980429     C                   MOVEL     FUV(2)        FUN002
132700980429     C                   MOVEL     FUV(3)        FUN003
132800980429     C                   MOVEL     FUV(4)        FUN004
132900980429     C                   MOVEL     FUV(5)        FUN005
133000980429     C                   MOVEL     FUV(6)        FUN006
133100980429     C                   END
133200980429     C*
133300980429     C* SECONDO GRUPPO DI 6 FUNZIONI
133400980429     C     FUNGRU        IFEQ      2
133500980429     C                   MOVEL     FUV(7)        FUN001
133600980429     C                   MOVEL     FUV(8)        FUN002
133700980429     C                   MOVEL     FUV(9)        FUN003
133800980429     C                   MOVEL     FUV(10)       FUN004
133900980429     C                   MOVEL     FUV(11)       FUN005
134000980429     C                   MOVEL     FUV(12)       FUN006
134100980429     C                   END
134200980429     C*
134300980429     C* TERZO GRUPPO DI 6 FUNZIONI
134400980429     C     FUNGRU        IFEQ      3
134500980429     C                   MOVEL     FUV(13)       FUN001
134600980429     C                   MOVEL     FUV(14)       FUN002
134700980429     C                   MOVEL     FUV(15)       FUN003
134800980429     C                   MOVEL     FUV(16)       FUN004
134900980429     C                   MOVEL     FUV(17)       FUN005
135000980429     C                   MOVEL     FUV(18)       FUN006
135100980429     C                   END
135200980429     C*
135300980429     C* QUARTO GRUPPO DI 6 FUNZIONI
135400980429     C     FUNGRU        IFEQ      4
135500980429     C                   MOVEL     FUV(19)       FUN001
135600980429     C                   MOVEL     FUV(20)       FUN002
135700980429     C                   MOVEL     FUV(21)       FUN003
135800980429     C                   MOVEL     FUV(22)       FUN004
135900980429     C                   MOVEL     *BLANKS       FUN005
136000980429     C                   MOVEL     *BLANKS       FUN006
136100980429     C                   END
136200980429     C*
136300980429     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
136400980429     C                   MOVEL     *BLANKS       FUN007
136500980429     C                   MOVEL     *BLANKS       FUN008
136600980429     C*
136700980429     C     1             DO        24            I
136800980429     C                   MOVEL     FUD(I)        FUDRIG
136900980429     C     FUDFUN        IFEQ      '23'
137000980429     C     FUDATT        ANDEQ     'S'
137100980429     C                   MOVEL     FUDDES        FUN007
137200980429     C                   END
137300980429     C     FUDFUN        IFEQ      '24'
137400980429     C     FUDATT        ANDEQ     'S'
137500980429     C                   MOVEL     FUDDES        FUN008
137600980429     C                   END
137700980429     C                   END                                                    DO
137800980429     C*
137900980429     C                   ENDSR
138000980429     C*------------------------------------------------------------------------*
138100980429     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
138200980429     C*------------------------------------------------------------------------*
138300980429     C     OPZGES        BEGSR
138400980429     C*
138500980429     C* PULISCO CAMPI DI LAVORO
138600980429     C                   Z-ADD     0             W                 5 0
138700980429     C*
138800980429     C* RICERCO QUANTE OPZIONI SONO ATTIVE
138900980429     C* E CARICO TABELLA OPZIONI ATTIVE
139000980429     C     1             DO        24            I
139100980429     C                   MOVEL     OPD(I)        OPDRIG
139200980429     C*
139300980429     C* PULIZIA OPZIONI PRECEDENTI
139400980429     C                   MOVEL     *BLANKS       OPV(I)
139500980429     C*
139600980429     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
139700980429     C     OPDATT        IFEQ      'S'
139800980429     C                   ADD       1             W
139900980429     C                   MOVEL     OPDDES        OPV(W)
140000980429     C                   END
140100980429     C                   END                                                    DO WHILE
140200980429     C*
140300980429     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
140400980429     C* ALTRIMENTI DISATTIVO F23
140500980429     C     1             DO        24            I
140600980429     C                   MOVEL     FUD(I)        FUDRIG
140700980429     C*
140800980429     C     FUDFUN        IFEQ      '23'
140900980429     C     W             IFGT      8
141000980429     C                   MOVEL     'S'           FUDATT
141100980429     C                   ELSE
141200980429     C                   MOVEL     *BLANKS       FUDATT
141300980429     C                   END                                                    W GT 8
141400980429     C                   MOVEL     FUDRIG        FUD(I)
141500980429     C                   END
141600980429     C                   END                                                    DO WHILE
141700980429     C*
141800980429     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
141900980429     C     W             DIV       8             I
142000980429     C                   MVR                     Z                 5 0
142100980429     C*
142200980429     C     Z             IFNE      0
142300980429     C     I             ADD       1             Z
142400980429     C                   ELSE
142500980429     C                   Z-ADD     I             Z
142600980429     C                   END
142700980429     C*
142800980429     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
142900980429     C                   ADD       1             OPZGRU
143000980429     C     OPZGRU        IFGT      Z
143100980429     C     OPZGRU        ORLT      1
143200980429     C                   Z-ADD     1             OPZGRU
143300980429     C                   END
143400980429     C*
143500980429     C* PRIMO GRUPPO DI 8 OPZIONI
143600980429     C     OPZGRU        IFEQ      1
143700980429     C                   MOVEL     OPV(1)        OPZ001
143800980429     C                   MOVEL     OPV(2)        OPZ002
143900980429     C                   MOVEL     OPV(3)        OPZ003
144000980429     C                   MOVEL     OPV(4)        OPZ004
144100980429     C                   MOVEL     OPV(5)        OPZ005
144200980429     C                   MOVEL     OPV(6)        OPZ006
144300980429     C                   MOVEL     OPV(7)        OPZ007
144400980429     C                   MOVEL     OPV(8)        OPZ008
144500980429     C                   END
144600980429     C*
144700980429     C* SECONDO GRUPPO DI 8 OPZIONI
144800980429     C     OPZGRU        IFEQ      2
144900980429     C                   MOVEL     OPV(9)        OPZ001
145000980429     C                   MOVEL     OPV(10)       OPZ002
145100980429     C                   MOVEL     OPV(11)       OPZ003
145200980429     C                   MOVEL     OPV(12)       OPZ004
145300980429     C                   MOVEL     OPV(13)       OPZ005
145400980429     C                   MOVEL     OPV(14)       OPZ006
145500980429     C                   MOVEL     OPV(15)       OPZ007
145600980429     C                   MOVEL     OPV(16)       OPZ008
145700980429     C                   END
145800980429     C*
145900980429     C* TERZO GRUPPO DI 8 OPZIONI
146000980429     C     OPZGRU        IFEQ      3
146100980429     C                   MOVEL     OPV(17)       OPZ001
146200980429     C                   MOVEL     OPV(18)       OPZ002
146300980429     C                   MOVEL     OPV(19)       OPZ003
146400980429     C                   MOVEL     OPV(20)       OPZ004
146500980429     C                   MOVEL     OPV(21)       OPZ005
146600980429     C                   MOVEL     OPV(22)       OPZ006
146700980429     C                   MOVEL     OPV(23)       OPZ007
146800980429     C                   MOVEL     OPV(24)       OPZ008
146900980429     C                   END
147000980429     C*
147100980429     C                   ENDSR
147200980429     C*------------------------------------------------------------------------*
147300980429     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
147400980429     C*------------------------------------------------------------------------*
147500980429     C     OPZCON        BEGSR
147600980429     C*
147700980429     C* PULISCO CAMPI DI LAVORO
147800980429     C                   Z-ADD     0             I                 5 0
147900980429     C                   Z-ADD     0             W                 5 0
148000980429     C*
148100980429     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
148200980429     C     1             DO        24            I
148300980429     C                   MOVEL     OPD(I)        OPDRIG
148400980429     C*
148500980429     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
148600980429     C     OPZKEY        IFEQ      OPDOPZ
148700980429     C     OPZKEY        OREQ      OPDOP1
148800980429     C*
148900980429     C     OPDATT        IFEQ      'S'
149000980429     C                   MOVEL     OPDOPZ        OPZKEY
149100980429     C                   Z-ADD     I             W
149200980429     C                   END
149300980429     C                   END
149400980429     C*
149500980429     C                   END                                                    DO WHILE
149600980429     C*
149700980429     C* OPZIONE NON ATTIVA (ERRORE)
149800980429     C     W             IFEQ      0
149900980429     C*
150000980429     C* OPZIONE NON OK
150100980429     C                   Z-ADD     1             OPZOK
150200980429     C*
150300980429     C* EMISSIONE MESSAGGIO DI ERRORE
150400980429     C                   MOVEL     MSG(1)        DSMSMS
150500980429     C                   ELSE
150600980429     C*
150700980429     C* OPZIONE ATTIVA (PROCEDI ...)
150800980429     C*
150900980429     C     OPZKEY        IFEQ      ' 2'
151000980429     C*---                 EXSR O02GES
151100980429     C                   END
151200980429     C     OPZKEY        IFEQ      ' 4'
151300980429     C*---                 EXSR O04GES
151400980429     C                   END
151500980429     C     OPZKEY        IFEQ      ' 5'
151600980429     C*---                 EXSR O05GES
151700980429     C                   END
151800980429     C     OPZKEY        IFEQ      ' 7'
151900980429     C*---                 EXSR O07GES
152000980429     C                   END
152100980429     C                   END                                                    OPZ.NON ATTIVA
152200980429     C*
152300980429     C                   ENDSR
152400980429     C*------------------------------------------------------------------------*
152500980429     C* NOTE COSTRUZIONE SCHIERE
152600980429     C*------------------------------------------------------------------------*
152700980429     C*
152800980429     C* -MSG-MESSAGGI
152900980429     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
153000980429     C* (NON TOCCARE MSG,1 E MSG,2)
153100980429     C*
153200980429     C* -CMD-COMANDI
153300980429     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
153400980429     C*
153500980429     C* -OPD-OPZIONI
153600980429     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
153700980429     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
153800980429     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
153900980429     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
154000980429     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
154100980429     C*  . POSSONO ESSERE IN ORDINE SPARSO
154200980429     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
154300980429     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
154400980429     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
154500980429     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
154600980429     C*
154700980429     C* -FUD-FUNZIONI
154800980429     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
154900980429     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
155000980429     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
155100980429     C*  . POSSONO ESSERE IN ORDINE SPARSO
155200980429     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
155300980429     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
155400980429     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
155500980429     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
155600980429     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
155700980429     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
155800980429     O*------------------------------------------------------------------------*
155900980429** MSG - MESSAGGI
156000980429TNTE36R- Opzione non gestita                                                   1
156100980429TNTE36R- Tasto funzionale non gestito                                          2
156200980429TNTE36R- 3
156300980429TNTE36R- Record annullato. Consentito solo il ripristino                       4
156400980429TNTE36R- Record non annullato. Impossibile ripristinare                        5
156500980429TNTE36R- 6                                                                     6
156600980429TNTE36R- 7                                                                     7
156700980429TNTE36R- 8                                                                     8
156800980429TNTE36R- 9                                                                     9
156900980429TNTE36R- Immettere uno dei valori possibili                                   10
157000980429TNTE36R- 11                                                                   11
157100980429TNTE36R- 12                                                                   12
157200980429TNTE36R- 13                                                                   13
157300980429TNTE36R- 14                                                                   14
157400980429TNTE36R- 15                                                                   15
157500980429TNTE36R- 16                                                                   16
157600980429TNTE36R- 17                                                                   17
157700980429TNTE36R- 18                                                                   18
157800980429TNTE36R- 19                                                                   19
157900980429TNTE36R- 20                                                                   20
158000980429** OPD - OPZIONI
158100980429                   -  -     (S/N)=N                        opz. 01¬
158200980429 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
158300980429                   -  -     (S/N)=N                        opz. 03¬
158400980429 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
158500980429 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
158600980429                   -  -     (S/N)=N                        opz. 06¬
158700980429 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
158800980429                   -8 -     (S/N)=N                        opz. 08¬
158900980429                   -9 -     (S/N)=N                        opz. 09¬
159000980429                   -  -     (S/N)=N                        opz. 10¬
159100980429                   -  -     (S/N)=N                        opz. 11¬
159200980429                   -  -     (S/N)=N                        opz. 12¬
159300980429                   -  -     (S/N)=N                        opz. 13¬
159400980429                   -  -     (S/N)=N                        opz. 14¬
159500980429                   -  -     (S/N)=N                        opz. 15¬
159600980429                   -  -     (S/N)=N                        opz. 16¬
159700980429                   -  -     (S/N)=N                        opz. 17¬
159800980429                   -  -     (S/N)=N                        opz. 18¬
159900980429                   -  -     (S/N)=N                        opz. 19¬
160000980429                   -  -     (S/N)=N                        opz. 20¬
160100980429                   -  -     (S/N)=N                        opz. 21¬
160200980429                   -  -     (S/N)=N                        opz. 22¬
160300980429                   -  -     (S/N)=N                        opz. 23¬
160400980429                   -  -     (S/N)=N                        opz. 24¬
160500980429** FUD - FUNZIONI
160600980429                   -        (S/N)=N                        funz.01¬
160700980429                   -        (S/N)=N                        funz.02¬
160800980429F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
160900980429                   -        (S/N)=N                        funz.04¬
161000980429F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
161100980429F06=Inserimento    -        (S/N)=N                        funz.06¬
161200980429                   -        (S/N)=N                        funz.07¬
161300980429                   -        (S/N)=N                        funz.08¬
161400980429                   -        (S/N)=N                        funz.09¬
161500980429                   -        (S/N)=N                        funz.10¬
161600980429                   -        (S/N)=N                        funz.11¬
161700980429F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
161800980429                   -        (S/N)=N                        funz.13¬
161900980429                   -        (S/N)=N                        funz.14¬
162000980429                   -        (S/N)=N                        funz.15¬
162100980429                   -        (S/N)=N                        funz.16¬
162200980429                   -        (S/N)=N                        funz.17¬
162300980429                   -        (S/N)=N                        funz.18¬
162400980429 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
162500980429 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
162600980429                   -        (S/N)=N                        funz.21¬
162700980429                   -        (S/N)=N                        funz.22¬
162800980429F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
162900980429F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
163000980429** DEC - DECODIFICHE
163100980429    GESTIONE                                                     1
163200980429INTERROGAZIONE                                                   2
163201060912     SCELTA                                                      3
1634009804294
1635009804295
163600980429** -RDES-
1637009804291
1638009804292
163900980429NON APPARE IN TARIFFA    3
164000980429APPARE IN TARIFFA        4
164100980429APPARE IN XXXXXXXXXXXXXXX5
164200980429SITUAZIONE MISTA         6
1643009804297
1644009804298
1645009804299
16460098042910
