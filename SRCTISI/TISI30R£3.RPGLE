000100970526     H*------------------------------------------------------------------------*
000200970529     H* CAPPARIO - GESTIONE LOCALITA' - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970529     FTISI30D   CF   E             WORKSTN INFDS(DEVDS1)
000900970529     F                                     SFILE(SI30D05:SFLULT)
001000970529     F                                     SFILE(SI30DMS:MSGULT)
001100970526     F*------------
001200970529     F* LOCALITA'
001300970526     F*------------
001400970529     FAZCPL01L  IF   E           K DISK
001500970529     FAZCPL02L  IF   E           K DISK
001600970529     F                                     RENAME(AZCPL000:AZCPL2)
001700970529     F*------------
001800970529     F* CAPPARIO
001900970529     F*------------
002000970529     FAZCPC01L  IF   E           K DISK
002100130823
002200130823     FAZcos01L  IF   E           K DISK
002300970526     D*------------------------------------------------------------------------*
002400970526     D* SCHIERE
002500970526     D*------------------------------------------------------------------------*
002600970526     D*------------
002700970526     D* DECODIFICHE
002800970526     D*------------
002900970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* MESSAGGI
003200970526     D*------------
003300970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* DESCRIZIONI OPZIONI
003600970526     D*------------
003700970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003800970526     D*------------
003900970526     D* DESCRIZIONI TASTI FUNZIONALI
004000970526     D*------------
004100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004200970526     D*------------
004300970526     D* OPZIONI DA CARICARE A VIDEO
004400970526     D*------------
004500970526     D OPV             S             19    DIM(24)
004600970526     D*------------
004700970526     D* FUNZIONI DA CARICARE A VIDEO
004800970526     D*------------
004900970527     D FUV             S             19    DIM(24)
005000970526     D*------------------------------------------------------------------------*
005100970526     D* INPUT
005200970526     D*------------------------------------------------------------------------*
005300970526     D*------------
005400970526     D* RIDEFINIZIONE OPZIONI VIDEO
005500970526     D*------------
005600970526     D OPDDS           DS
005700970526     D  OPDOPZ                 1      2
005800970526     D  OPDDE1                 3     19
005900970526     D  OPDOP1                21     22
006000970526     D  OPDFI0                23     34
006100970526     D  OPDATT                35     35
006200970526     D  OPDFI1                36     80
006300970526     D  OPDDES                 1     19
006400970526     D  OPDRIG                 1     80
006500970526     D*------------
006600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006700970526     D*------------
006800970526     D FUDDS           DS
006900970526     D  FUDDE1                 1      1
007000970526     D  FUDFUN                 2      3
007100970526     D  FUDDE2                 4     19
007200970526     D  FUDFI0                20     34
007300970526     D  FUDATT                35     35
007400970526     D  FUDFI1                36     80
007500970526     D  FUDDES                 1     19
007600970526     D  FUDRIG                 1     80
007700970526     D*------------
007800970526     D* DS GESTIONE TASTI FUNZIONALI
007900970526     D*------------
008000970526     D DSKEY         E DS
008100970526     D DEVDS1          DS
008200970526     D  KEY                  369    369
008300970526     D*------------
008400970526     D* DS "XMSG" - GESTIONE MESSAGGI
008500970526     D*------------
008600970526     D DSMSG         E DS
008700970526     D*------------
008800020919     D* DS REPERIMENTO DATI UTENTE
008900970526     D*------------
009000020919     D TIBS34DS      E DS                                                       *Profili utente
009100020919     D DDATIUTE      E DS                                                       *Dati utente
009200020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009300061026     D DUTE01        E DS                                                       *Abilitazioni
009400970527     D*------------------
009500970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009600970527     D*------------------
009700970527     D WLBDA8          DS                  INZ
009800970527     D  G08DAT                 1      8  0
009900970527     D  G08INV                 9     16  0
010000970527     D  G08ERR                17     17
010100970527     D  G08TGI                18     22  0
010200970526     D*------------
010300970526     D* DS DI PROCEDURA
010400970526     D*------------
010500970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010600060404     D tisip0ds      E DS
010700970526     D*------------
010800970526     D* ARCHITETTURA
010900970526     D*------------
011000970526     D KPJBA         E DS
011100060404     d
011200060404     d tipopgm         s              1
011300060404     d wdri            s                   like(Dp0dri)
011400060404     d dprv            s                   like(Dp0prv)
011500970526     C*------------------------------------------------------------------------*
011600970526     C* MAIN LINES
011700970526     C*------------------------------------------------------------------------*
011800970526     C*
011900970526     C* SE NON CI SONO ERRORI, PROSEGUE
012000970526IF  1C     ERRL00        IFNE      '1'
012100970526     C*
012200970526     C* CARICA IL SFILE
012300970526     C                   EXSR      POSSFL
012400970526     C*
012500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012900970526     C*
013000970526     C* REIMPOSTA ERRORE DI PROCERURA
013100970526     C                   MOVEL     '0'           ERRL00
013200970526     C*
013300970526     C* EMETTE IL SFILE
013400970526     C                   EXSR      OUTSFL
013500970526     C*
013600970526     C* GESTIONE TASTI FUNZIONALI
013700970526     C                   EXSR      FUNCON
013800970526     C*
013900970526E   2C                   ENDDO
014000970527     C*
014100970527     C* CI SONO ERRORI IN ENTRATA
014200970527X   1C                   ELSE
014300970527     C                   MOVEL     '1'           D00ERR
014400970527     C                   MOVEL     DSMSMS        D00MSG
014500970526E   1C                   ENDIF
014600970526     C* FINE PGM
014700970526     C                   SETON                                        LR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015000970526     C*------------------------------------------------------------------------*
015100970526     C     F03GES        BEGSR
015200970526     C*
015300970526     C* IMPOSTA "PREMUTO F03"
015400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015500970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
015600970526     C*
015700970526     C                   ENDSR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016000970526     C*------------------------------------------------------------------------*
016100970526     C     F05GES        BEGSR
016200970526     C*
016300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016800970526     C*------------------------------------------------------------------------*
016900970526     C     F06GES        BEGSR
017000970526     C*
017100970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
017200020919IF  1C     DUTLPO        IFNE      'S'
017300970526     C                   MOVEL     '1'           ERRL00
017400970526     C                   MOVEL     MSG(3)        DSMSMS
017500970526E   1C                   ENDIF
017600970526     C*
017700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017800970603IF  2C     D00NAR        IFNE      *BLANKS
017900970603     C     D00CAP        ORNE      *BLANKS
018000970603     C                   MOVEL     D00NAR        DNAR
018100970603     C                   MOVEL     D00CAP        DCAP
018200970603X   2C                   ELSE
018300970603     C                   MOVEL     *BLANKS       DNAR
018400970602     C                   MOVEL     *BLANKS       DCAP
018500970603E   2C                   ENDIF
018600970602     C                   MOVEL     *BLANKS       DLOC
018700970529     C                   MOVEL     DEPOP0        WOP0
018800970529     C                   MOVEL     'F06'         WOP1
018900970529     C                   MOVEL     'TISI31R'     WPGM              8
019000970526     C                   EXSR      LANPGM                                       *LANCIA PGM
019100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019200970526E   1C                   ENDIF
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* F12GES - GESTIONE F12 -> RITORNO
019700970526     C*------------------------------------------------------------------------*
019800970526     C     F12GES        BEGSR
019900970526     C*
020000970526     C* IMPOSTA "PREMUTO F12"
020100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020200970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
020300970526     C*
020400970526     C                   ENDSR
020500970529     C*------------------------------------------------------------------------*
020600970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
020700970529     C*------------------------------------------------------------------------*
020800970529     C     F13GES        BEGSR
020900970529     C*
021000970602     C                   MOVEL     *BLANKS       DNAR
021100970602     C                   MOVEL     *BLANKS       DCAP
021200970602     C                   MOVEL     *BLANKS       DLOC
021300970529     C                   MOVEL     DEPOP0        WOP0
021400970529     C                   MOVEL     'S'           WOP0
021500970529     C                   MOVEL     'F13'         WOP1
021600970529     C                   MOVEL     'TISI35R'     WPGM              8
021700970529     C                   EXSR      LANPGM
021800970529     C*
021900970529     C                   ENDSR
022000060404     C*------------------------------------------------------------------------*
022100060404     C* F14GES - GESTIONE F14 -> calendario eventi particolari
022200060404     C*------------------------------------------------------------------------*
022300060404     C     F14GES        BEGSR
022400060404     C*
022500060404     C                   MOVEL     *BLANKS       DNAR
022600060404     C                   MOVEL     *BLANKS       DCAP
022700060404     C                   MOVEL     *BLANKS       Dloc
022800060404     C                   MOVEL     *BLANKS       Dprv
022900060404     C                   MOVEL     DEPOP0        WOP0
023000060404     c                   clear                   wop1
023100060404     C                   MOVEL     datcor        wdri
023200060404     C                   MOVEL     'TISIP0R'     WPGM              8
023300060404     C                   EXSR      LANPGM
023400060404     C*
023500060404     C                   ENDSR
023600970526     C*------------------------------------------------------------------------*
023700970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023800970526     C*------------------------------------------------------------------------*
023900970526     C     F23GES        BEGSR
024000970526     C*
024100970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
024200970526     C*
024300970526     C                   ENDSR
024400970526     C*------------------------------------------------------------------------*
024500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024600970526     C*------------------------------------------------------------------------*
024700970526     C     F24GES        BEGSR
024800970526     C*
024900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
025000970526     C*
025100970526     C                   ENDSR
025200970526     C*------------------------------------------------------------------------*
025300970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
025400970526     C*------------------------------------------------------------------------*
025500970526     C     RUPGES        BEGSR
025600970526     C*
025700970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025800970526     C*
025900970526     C                   ENDSR
026000970526     C*------------------------------------------------------------------------*
026100970526     C* ENTGES - GESTIONE ENTER -> INVIO
026200970526     C*------------------------------------------------------------------------*
026300970526     C     ENTGES        BEGSR
026400970526     C*
026500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
026600970526     C                   EXSR      NEWPOS
026700970526     C*
026800970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027000970526IF  2C     SWPOSI        IFNE      0
027100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
027200970526X   2C                   ELSE
027300970526IF  3C     SFLULT        IFNE      0
027400970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
027500970526     C*
027600970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027700970526IF  4C     SWMODI        IFNE      0
027800970526     C                   EXSR      POSSFL
027900970526E   4C                   ENDIF
028000970526E   3C                   ENDIF
028100970526E   2C                   ENDIF
028200970526E   1C                   ENDIF
028300970526     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
028700970526     C*------------------------------------------------------------------------*
028800970526     C     O01GES        BEGSR
028900970526     C*
029000970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
029100970527     C                   MOVEL     VIDNAR        D00NAR
029200970527     C                   MOVEL     VIDCAP        D00CAP
029300970529     C                   MOVEL     VIDLOC        D00LOC
029400970702     C                   MOVEL     VIDCPR        D00PRV
029500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029600970526     C*
029700970526     C                   ENDSR
029800970526     C*------------------------------------------------------------------------*
029900970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
030000970526     C*------------------------------------------------------------------------*
030100970526     C     O02GES        BEGSR
030200970526     C*
030300970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
030400970526IF  1C     VIDATB        IFEQ      'A'
030500970526     C                   MOVEL     '1'           ERRL00
030600970526     C                   MOVEL     MSG(4)        DSMSMS
030700970526E   1C                   ENDIF
030800970527     C* MODIFICA
030900970704IF  1C     ERRL00        IFEQ      '0'
031000970602     C                   MOVEL     VIDNAR        DNAR
031100970602     C                   MOVEL     VIDCAP        DCAP
031200970602     C                   MOVEL     VIDLOC        DLOC
031300970529     C                   MOVEL     DEPOP0        WOP0
031400970526     C                   MOVEL     'O02'         WOP1
031500970529     C                   MOVEL     'TISI31R'     WPGM
031600970526     C                   EXSR      LANPGM
031700970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
031800970528E   1C                   ENDIF
031900970526     C*
032000970526     C                   ENDSR
032100970611     C*------------------------------------------------------------------------*
032200970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
032300970611     C*------------------------------------------------------------------------*
032400970611     C     O03GES        BEGSR
032500970611     C*
032600970611     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
032700970611IF  1C     VIDATB        IFEQ      'A'
032800970611     C                   MOVEL     '1'           ERRL00
032900970611     C                   MOVEL     MSG(4)        DSMSMS
033000970611E   1C                   ENDIF
033100970611     C* DUPLICA
033200970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033300970611     C                   MOVEL     VIDNAR        DNAR
033400970611     C                   MOVEL     VIDCAP        DCAP
033500970611     C                   MOVEL     VIDLOC        DLOC
033600970611     C                   MOVEL     DEPOP0        WOP0
033700970611     C                   MOVEL     'O03'         WOP1
033800970611     C                   MOVEL     'TISI32R'     WPGM
033900970611     C                   EXSR      LANPGM
034000970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
034100970611E   1C                   ENDIF
034200970611     C*
034300970611     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
034600970526     C*------------------------------------------------------------------------*
034700970526     C     O04GES        BEGSR
034800970526     C*
034900970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
035000970526IF  1C     VIDATB        IFEQ      'A'
035100970526     C                   MOVEL     '1'           ERRL00
035200970526     C                   MOVEL     MSG(4)        DSMSMS
035300970526E   1C                   ENDIF
035400970527     C* ANNULLAMENTO
035500970704IF  1C     ERRL00        IFEQ      '0'
035600970602     C                   MOVEL     VIDNAR        DNAR
035700970602     C                   MOVEL     VIDCAP        DCAP
035800970602     C                   MOVEL     VIDLOC        DLOC
035900970529     C                   MOVEL     DEPOP0        WOP0
036000970526     C                   MOVEL     'O04'         WOP1
036100970529     C                   MOVEL     'TISI31R'     WPGM
036200970526     C                   EXSR      LANPGM
036300970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
036400970528E   1C                   ENDIF
036500970526     C*
036600970526     C                   ENDSR
036700970526     C*------------------------------------------------------------------------*
036800970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
036900970526     C*------------------------------------------------------------------------*
037000970526     C     O05GES        BEGSR
037100970526     C*
037200970602     C                   MOVEL     VIDNAR        DNAR
037300970602     C                   MOVEL     VIDCAP        DCAP
037400970602     C                   MOVEL     VIDLOC        DLOC
037500970529     C                   MOVEL     DEPOP0        WOP0
037600970526     C                   MOVEL     'O05'         WOP1
037700970529     C                   MOVEL     'TISI31R'     WPGM
037800970526     C                   EXSR      LANPGM
037900970526     C*
038000970526     C                   ENDSR
038100970526     C*------------------------------------------------------------------------*
038200970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
038300970526     C*------------------------------------------------------------------------*
038400970526     C     O07GES        BEGSR
038500970526     C*
038600970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
038700970526IF  1C     VIDATB        IFNE      'A'
038800970526     C                   MOVEL     '1'           ERRL00
038900970526     C                   MOVEL     MSG(5)        DSMSMS
039000970526E   1C                   ENDIF
039100970526     C* RIPRISITINO
039200970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039300970602     C                   MOVEL     VIDNAR        DNAR
039400970602     C                   MOVEL     VIDCAP        DCAP
039500970602     C                   MOVEL     VIDLOC        DLOC
039600970529     C                   MOVEL     DEPOP0        WOP0
039700970526     C                   MOVEL     'O07'         WOP1
039800970529     C                   MOVEL     'TISI31R'     WPGM
039900970526     C                   EXSR      LANPGM
040000970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
040100970528E   1C                   ENDIF
040200970526     C*
040300970526     C                   ENDSR
040400970529     C*------------------------------------------------------------------------*
040500970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
040600970529     C*------------------------------------------------------------------------*
040700970529     C     O13GES        BEGSR
040800970529     C*
040900970602     C                   MOVEL     VIDNAR        DNAR
041000970602     C                   MOVEL     VIDCAP        DCAP
041100970602     C                   MOVEL     VIDLOC        DLOC
041200970529     C                   MOVEL     DEPOP0        WOP0
041300970529     C                   MOVEL     'S'           WOP0
041400970529     C                   MOVEL     'O13'         WOP1
041500970529     C                   MOVEL     'TISI35R'     WPGM
041600970529     C                   EXSR      LANPGM
041700970529     C*
041800970529     C                   ENDSR
041900060404     C*------------------------------------------------------------------------*
042000060404     C* O14GES - GESTIONE calendario eventi particolari
042100060404     C*------------------------------------------------------------------------*
042200060404     C     O14GES        BEGSR
042300060404     C*
042400060404     C                   MOVEL     VIDNAR        DNAR
042500060404     C                   MOVEL     VIDCAP        DCAP
042600060404     c                   clear                   dloc
042700060404     c                   clear                   dprv
042800060404     C                   MOVEL     DEPOP0        WOP0
042900060404     C                   clear                   WOP1
043000060404     C                   MOVEL     datcor        wdri
043100060404     c                   move      0101          wdri
043200060404     C                   MOVEL     'TISIP0R'     WPGM
043300060404     C                   EXSR      LANPGM
043400060404     C*
043500060404     C                   ENDSR
043600060404     C*------------------------------------------------------------------------*
043700060404     C* O15GES - Inserisci  evento particolare
043800060404     C*------------------------------------------------------------------------*
043900060404     C     O15GES        BEGSR
044000060404     C*
044100060404     C                   MOVEL     VIDNAR        DNAR
044200060404     C                   MOVEL     VIDCAP        DCAP
044300060404     C                   MOVEL     VIDcpr        Dprv
044400060404     C                   MOVEL     VIDLOC        DLOC
044500060404     C                   MOVEL     DEPOP0        WOP0
044600060404     C                   movel     'F06'         WOP1
044700060404     C                   MOVEL     datcor        wdri
044800060404     C                   MOVEL     'TISIP1R'     WPGM
044900060404     C                   EXSR      LANPGM
045000060404     C*
045100060404     C                   ENDSR
045200970526     C*------------------------------------------------------------------------*
045300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
045400970526     C*------------------------------------------------------------------------*
045500970526     C     NEWPOS        BEGSR
045600970526     C*
045700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
045800970526     C*         1= RICHIESTO POSIZIONAMENTO
045900970526     C                   Z-ADD     0             SWPOSI            7 0
046000970526     C*
046100970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
046200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
046300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
046400970526     C                   Z-ADD     0             SWMODI            1 0
046500970530     C*
046600970530     C* REIMPOSTA INDICATORI DI ERRORE
046700970530     C                   SETOFF                                       808182
046800970526     C*
046900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
047000970527IF  1C     VIDPNA        IFNE      DEPNAR
047100970527     C     VIDPCA        ORNE      DEPCAP
047200120515     C     VIDPPR        ORNE      DEPPPR
047300970529     C     VIDPLO        ORNE      DEPLOC
047400970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
047500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
047600970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
047700970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
047800970527     C                   MOVEL     VIDPCA        DEPCAP
047900120515     C                   MOVEL     VIDPPR        DEPPPR
048000970529     C                   MOVEL     VIDPLO        DEPLOC
048100970526E   2C                   ENDIF
048200970526E   1C                   ENDIF
048300970526     C*
048400970526     C                   ENDSR
048500970526     C*------------------------------------------------------------------------*
048600970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
048700970526     C*------------------------------------------------------------------------*
048800970526     C     SETFIL        BEGSR
048900970529     C*
049000970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
049100970529IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
049200970529     C                   Z-ADD     D00VER        KCLVER
049300970529     C                   MOVEL     VIDPNA        KCLNAR
049400970529     C                   MOVEL     VIDPCA        KCLCAP
049500970529     C     KEYCPL        SETLL     AZCPL01L                           99
049600970529E   1C                   ENDIF
049700970529IF  1C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
049800970529     C                   Z-ADD     D00VER        KCLVER
049900970605     C                   MOVEL     VIDPNA        KCLNAR
050000970529     C                   MOVEL     VIDPLO        KCLLOC
050100970529     C     KE2CPL        SETLL     AZCPL02L                           99
050200970529E   1C                   ENDIF
050300970526     C* LETTURA RECORD
050400970526IF  1C     *IN99         IFEQ      *OFF
050500970526     C                   EXSR      LETFIL
050600970526E   1C                   ENDIF
050700970526     C*
050800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
050900970526IF  1C     *IN99         IFEQ      *ON
051000970526     C                   Z-ADD     1             SFLEOF
051100970526E   1C                   ENDIF
051200970526     C*
051300970526     C                   ENDSR
051400970526     C*------------------------------------------------------------------------*
051500970526     C* LETFIL - LETTURA RECORD
051600970526     C*------------------------------------------------------------------------*
051700970526     C     LETFIL        BEGSR
051800970526     C*
051900970526     C* LEGGE FINO A:
052000970526     C*     - FINE FILE
052100970526     C*     - TROVATO RECORD VALIDO
052200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
052300970526DO  1C     *IN99         DOWEQ     *OFF
052400970526     C     WRECOK        ANDEQ     'N'
052500970529     C*
052600970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
052700970529IF  2C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
052800120515     C* CAP è un parametro di selezione, per cui si devono leggere solo i rcd con quei CAP
052900120515     C* (CAP è in chiave)
053000970603     C     KCLVER        READE     AZCPL01L                               99
053100970529E   2C                   ENDIF
053200970529IF  2C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
053300120515     C* Località è un parametro di inizio, per cui si devono leggere i rcd a partire da quella
053400120515     C* Località (Località NON è in chiave)
053500970603     C     KCLVER        READE     AZCPL02L                               99
053600970529E   2C                   ENDIF
053700970526     C*
053800970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
053900970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
054000970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
054100970530X   2C                   ELSE
054200970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
054300970530E   2C                   ENDIF
054400970526E   1C                   ENDDO
054500970526     C*
054600970526     C                   ENDSR
054700970526     C*------------------------------------------------------------------------*
054800970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
054900970526     C*------------------------------------------------------------------------*
055000970526     C     CHKREC        BEGSR
055100970526     C*
055200970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
055300970530     C*
055400970530     C* SE RICHIESTO ORDINAMENTO PER CAP, ESCLUDE LE LOCALITA' NON DEL CAP
055500970530IF  1C     VIDPNA        IFNE      *BLANKS
055600970530     C     VIDPCA        ORNE      *BLANKS
055700970530IF  2C     VIDPNA        IFNE      CPLNAR
055800970530     C     VIDPCA        ORNE      CPLCAP
055900970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056000970530E   2C                   ENDIF
056100970530E   1C                   ENDIF
056200001005     C*
056300001005     C* LEGGE IL CAP DELLA LOCALIÀ'
056400001005IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
056500001005     C                   Z-ADD     CPLVER        KCCVER
056600001005     C                   MOVEL     CPLNAR        KCCNAR
056700001005     C                   MOVEL     CPLCAP        KCCCAP
056800001005     C     KEYCPC        CHAIN     AZCPC01L                           98
056900001005IF  2C     *IN98         IFEQ      *ON                                          *NON TROVATO
057000001005     C                   CLEAR                   AZCPC000                       *AZZERA CAMPI CAP
057100001005E   2C                   ENDIF
057200001005E   1C                   ENDIF
057300001005     C*
057400120515     C* se è stata scelta la Selezione per Provincia
057500120515IF  1C                   IF        WRecOK = 'S' and                             *RECORD VALIDO
057600120515     C                             VIDPPR <> *blank
057700120515     C* controllo che la provincia della località sia quella da selezionare
057800120515     C                             and VIDPPR <> CPCPrv
057900120515     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
058000120515E   2C                   ENDIF
058100120515     C*
058200120515     C* SE L'UTENTE E' IN SCELTA, ESCLUDE LE LOCALITÀ OBSOLETE
058300001005IF  1C     DEPOP0        IFEQ      'L01'
058400061010IF  2C***  CPLLIV        IFEQ      'L'
058500061010     C***  CPLFI1        ANDNE     *BLANKS
058600061010     C***  CPLLIV        OREQ      'C'
058700061010     C***  CPCFI1        ANDNE     *BLANKS
058800061010     c* escludere sempre a prescindere dal livello gestione
058900061010     c     cplfi1        ifne      *blanks
059000001005     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
059100001005E   2C                   ENDIF
059200001005E   1C                   ENDIF
059300001005     C*
059400970526     C                   ENDSR
059500970526     C*------------------------------------------------------------------------*
059600970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
059700970526     C*------------------------------------------------------------------------*
059800970526     C     IRESFL        BEGSR
059900970529     C*
060000970529     C                   MOVEL     *BLANKS       OPZKEY
060100970925     C     CPLATB        IFEQ      *BLANKS
060200970925     C                   MOVEL     CPLFI1        VIDATB
060300970925     C                   ELSE
060400970529     C                   MOVEL     CPLATB        VIDATB
060500970925     C                   ENDIF
060600970529     C                   MOVEL     CPLNAR        VIDNAR
060700970529     C                   MOVEL     CPLCAP        VIDCAP
060800970529     C                   MOVEL     CPLLOC        VIDLOC
060900970530     C                   MOVEL     CPLLIV        VIDLIV
061000970529     C                   Z-ADD     CPLTTC        VIDTTC
061100970529     C                   MOVEL     CPLISO        VIDISO
061200970529     C                   Z-ADD     CPLLNA        VIDLNA
061300970529     C                   Z-ADD     CPLZNC        VIDZNC
061400970529     C                   MOVEL     CPLDLV        VIDDLV
061500970529     C                   MOVEL     *BLANKS       VIDESS
061600970529IF  1C     CPLESS        IFEQ      'S'
061700970529     C                   MOVEL     'S'           VIDESS                         *ESISTONO SINONIMI
061800970529E   1C                   ENDIF
061900130823     c* se presente un orario a livello della località --> metto H
062000130823     c     kcos          chain     azcos01l
062100130823     c                   if        %found(azcos01l)
062200130823     C                   MOVE      'H'           VIDESS                         *ESISTONO orari serv
062300130823E   1C                   ENDIF
062400001005     C* DATI DAL CAP
062500970530     C                   MOVEL     CPCPRV        VIDCPR
062600970530     C                   Z-ADD     CPCLNA        VIHCLA
062700001005IF  1C     CPLLIV        IFEQ      'C'                                          *LIV.GESTIONE 'CAP'
062800970529     C                   Z-ADD     CPCTTC        VIDTTC
062900970529     C                   MOVEL     CPCISO        VIDISO
063000970529     C                   Z-ADD     CPCLNA        VIDLNA
063100970529     C                   Z-ADD     CPCZNC        VIDZNC
063200001005E   1C                   ENDIF
063300970526     C*
063400970526     C                   ENDSR
063500970526     C*------------------------------------------------------------------------*
063600970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
063700970526     C*------------------------------------------------------------------------*
063800970526     C     CHKPAR        BEGSR
063900970529     C*
064000970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
064100970529IF  1C     VIDPCA        IFEQ      *BLANKS
064200970529     C     VIDPLO        ANDEQ     *BLANKS
064300970529     C                   MOVEL     'CAP'         WTIPRI            3
064400970529E   1C                   ENDIF
064500970529     C*
064600970529     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
064700970529IF  1C     ERRL00        IFEQ      '0'
064800120515     C* avendo fatto diventare in data 15/05/12 il CAP un dato non di "Inizio da" ma "Selezione per"
064900120515     C* ed essendo rimasto solo la Località come "Inizio da",
065000120515     C* questo test non ha più senso.
065100120515IF  2C***  VIDPCA        IFNE      *BLANKS
065200120515     C***  VIDPLO        ANDNE     *BLANKS
065300120515     C***                MOVEL     MSG(6)        DSMSMS
065400120515     C***                MOVEL     '1'           ERRL00
065500120515     C***                SETON                                        808182
065600120515E   2C***                ENDIF
065700120515E   1C                   ENDIF
065800970529     C*
065900970529     C* CONTROLLA IL TIPO RICERCA
066000970529IF  1C     ERRL00        IFEQ      '0'
066100970529IF  2C     VIDPCA        IFNE      *BLANKS
066200970529     C                   MOVEL     'CAP'         WTIPRI
066300970529E   2C                   ENDIF
066400970529IF  2C     VIDPLO        IFNE      *BLANKS
066500970529     C                   MOVEL     'LOC'         WTIPRI
066600970529E   2C                   ENDIF
066700970529E   1C                   ENDIF
066800970526     C*
066900970526     C                   ENDSR
067000970526     C*------------------------------------------------------------------------*
067100970526     C* LANPGM - LANCIA UN PROGRAMMA
067200970526     C*------------------------------------------------------------------------*
067300970526     C     LANPGM        BEGSR
067400970526     C*
067500970526     C* IMPOSTA LA DS DI PROCEDURA
067600060404     c                   eval      tipopgm=%subst(wpgm:5:1)
067700060404     c                   if        tipopgm='P'
067800060404     c                   clear                   tisip0ds
067900060404     C                   MOVEL     DNAR          Dp0NAR                         *NAZIONE
068000060404     C                   MOVEL     DCAP          Dp0CAP                         *CAP
068100060404     C                   MOVEL     Dloc          Dp0loc                         *CAP
068200060404     C                   MOVEL     Dprv          Dp0prv                         *CAP
068300060404     C                   MOVEL     wop0          dp0OP0
068400060404     C                   MOVEL     WOP1          Dp0OP1                         *LIVELLO PGM
068500060404     c                   movel     wdri          dp0dri
068600060404     c                   call      WPGM
068700060404     c                   parm                    kpjba
068800060404     c                   parm                    tisip0ds
068900060404     C*
069000060404     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
069100060404IF  1C     Dp0F03        IFEQ      '1'                                          *PREMUTO F03
069200060404     C                   MOVEL     '1'           ERRL00                         *ERRORE
069300060404     C                   MOVEL     '1'           F03L00                         *FINE PGM
069400060404     c                   movel     dp0f03        d00f03
069500060404X   1C                   ELSE                                                   *NON PREMUTO F03
069600060404     C                   MOVEL     Dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
069700060404E   1C                   ENDIF
069800060404     c
069900060404     c                   else
070000970529     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
070100970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
070200970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
070300970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
070400970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
070500970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
070600970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
070700970602     C                   MOVEL     DCAP          D00CAP                         *CAP
070800970602     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
070900970603     C                   MOVEL     *BLANKS       D00SIN                         *ALTRI CAMPI
071000970603     C                   Z-ADD     *ZEROS        D00TTC
071100970530     C                   MOVEL     *BLANKS       D00TSP
071200970530     C                   Z-ADD     *ZEROS        D00TFP
071300970530     C                   MOVEL     *BLANKS       D00EPA
071400970530     C                   Z-ADD     *ZEROS        D00ETP
071500970530     C                   Z-ADD     *ZEROS        D00ETA
071600970530     C                   Z-ADD     *ZEROS        D00EDD
071700970530     C                   MOVEL     *BLANKS       D00SPT
071800970530     C                   Z-ADD     *ZEROS        D00SLP
071900970530     C                   Z-ADD     *ZEROS        D00SLA
072000970530     C                   MOVEL     *BLANKS       D00STS
072100970530     C                   Z-ADD     *ZEROS        D00SDD
072200970526     C*
072300970526     C                   CALL      WPGM
072400970612     C                   PARM                    KPJBA
072500970612     C                   PARM                    DSSI00
072600970526     C*
072700970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
072800970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
072900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
073000970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
073100970526X   1C                   ELSE                                                   *NON PREMUTO F03
073200970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
073300970526E   1C                   ENDIF
073400060404E   1C                   ENDIF
073500970526     C*
073600970526     C                   ENDSR
073700970527     C*------------------------------------------------------------------------*
073800970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
073900970527     C*------------------------------------------------------------------------*
074000970527     C     CARTAB        BEGSR
074100970527     C*
074200970527     C                   ENDSR
074300970526     C*------------------------------------------------------------------------*
074400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
074500970526     C*------------------------------------------------------------------------*
074600061026     C     CHKFOP        BEGSR
074700061026     C*
074800061026     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
074900061026     C                   SETON                                        93
075000061026     C*
075100061026     C                   IF        §UTECAP = 'C'
075200061026     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
075300061026     C                   SETOFF                                       93
075400061026     C                   ENDIF
075500970626     C*
075600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
075700970626IF  1C     DEPOP0        IFEQ      'L01'
075800970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
075900970626     C                   MOVEL     'S'           OPDATT
076000970626     C                   MOVEL     OPDRIG        OPD(1)
076100970626X   1C                   ELSE
076200970626     C* OPZIONI
076300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
076400970626     C                   MOVEL     'S'           OPDATT
076500970626     C                   MOVEL     OPDRIG        OPD(5)
076600970626     C                   MOVEL     OPD(13)       OPDRIG                         *SINONIMI
076700970626     C                   MOVEL     'S'           OPDATT
076800970626     C                   MOVEL     OPDRIG        OPD(13)
076900060404     C                   MOVEL     OPD(14)       OPDRIG                         *SINONIMI
077000060404     C                   MOVEL     'S'           OPDATT
077100060404     C                   MOVEL     OPDRIG        OPD(14)
077200970626     C* FUNZIONI
077300970626     C                   MOVEL     FUD(13)       FUDRIG                         *SINONIMI
077400970626     C                   MOVEL     'S'           FUDATT
077500970626     C                   MOVEL     FUDRIG        FUD(13)
077600060404     C                   MOVEL     FUD(14)       FUDRIG                         *SINONIMI
077700060404     C                   MOVEL     'S'           FUDATT
077800060404     C                   MOVEL     FUDRIG        FUD(14)
077900970626E   1C                   ENDIF
078000970626     C*
078100970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
078200970626IF  1C     DEPOP0        IFEQ      'L02'
078300970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
078400970626     C                   MOVEL     'S'           OPDATT
078500970626     C                   MOVEL     OPDRIG        OPD(2)
078600970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
078700970627     C                   MOVEL     'S'           OPDATT
078800970627     C                   MOVEL     OPDRIG        OPD(3)
078900061026     C  N93              MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
079000061026     C  N93              MOVEL     'S'           OPDATT
079100061026     C  N93              MOVEL     OPDRIG        OPD(4)
079200061026     C  N93              MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
079300061026     C  N93              MOVEL     'S'           OPDATT
079400061026     C  N93              MOVEL     OPDRIG        OPD(7)
079500060404     C                   MOVEL     OPD(15)       OPDRIG                         *RIPRISTINO
079600060404     C                   MOVEL     'S'           OPDATT
079700060404     C                   MOVEL     OPDRIG        OPD(15)
079800061016     C* CONSENTITO INSERIMENTO SOLO A PERSONALE "EDP"
079900061026     C                   IF        %subst(KNMUS:1:3) = 'EDP' AND *IN93 = *off
080000970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
080100970626     C                   MOVEL     'S'           FUDATT
080200970626     C                   MOVEL     FUDRIG        FUD(6)
080300061016     C                   ENDIF
080400970626E   1C                   ENDIF
080500970626     C*
080600970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
080700060404IF  1C**   DUTLPO        IFNE      'S'                                          *FILIALE
080800060404     C**                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
080900060404     C**                 MOVEL     'N'           OPDATT
081000060404     C**                 MOVEL     OPDRIG        OPD(4)
081100060404     C**                 MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
081200060404     C**                 MOVEL     'N'           OPDATT
081300060404     C**                 MOVEL     OPDRIG        OPD(7)
081400060404     C**                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
081500060404     C**                 MOVEL     'N'           FUDATT
081600060404     C**                 MOVEL     FUDRIG        FUD(6)
081700060404E   1C*+                 ENDIF
081800970526     C*
081900970526     C* GESTIONE OPZIONI A VIDEO
082000970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
082100970526     C                   EXSR      OPZGES
082200970526     C*
082300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
082400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
082500970526     C                   EXSR      FUNGES
082600970526     C*
082700970526     C                   ENDSR
082800020919     C*--------------------------------------------------------------------------------------------*
082900020919     C* REPERISCE I DATI UTENTE
083000020919     C*--------------------------------------------------------------------------------------------*
083100020919     C     REPDATIUTE    BEGSR
083200020919     C*
083300020919     C* INIZIALIZZA VARIABILI DI WRK
083400020919     C                   CLEAR                   TIBS34DS
083500020919     C                   CLEAR                   AZUTEDS
083600020919     C                   CLEAR                   DDATIUTE
083700020919     C*
083800020919     C     *DTAARA       DEFINE    §azute        azuteds
083900020919     C     *DTAARA       DEFINE    §datiute      ddatiute
084000020919     C                   IN(E)     *DTAARA
084100020919if  1C                   IF        %Error
084200020919     c                   EVAL      I34Tla = 'L'
084300020919     C                   CALL      'TIBS34R'
084400020919     C                   PARM                    Tibs34Ds
084500020919     C                   IN        *DTAARA
084600020919e   1C                   ENDIF
084700020919     C*
084800020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
084900020919     C                   MOVEL(P)  RSUT          DSFIRS           20
085000061026     C*
085100061026     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
085200061026     C                   EVAL      DUTE01 = UTEFAF
085300020919     C*
085400020919     C                   ENDSR
085500970526     C*------------------------------------------------------------------------*
085600970526     C* *INZSR - OPERAZIONI INIZIALI
085700970526     C*------------------------------------------------------------------------*
085800970526     C     *INZSR        BEGSR
085900970526     C*---
086000970526     C* RICEVIMENTO PARAMETRI
086100970526     C*---
086200970526     C     *ENTRY        PLIST
086300970612     C                   PARM                    KPJBA
086400970612     C                   PARM                    DSSI00
086500970526     C*
086600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
086700970526     C     XMSGPA        PLIST
086800970526     C                   PARM                    DSMSMS
086900970526     C                   PARM                    DSMSPG
087000970526     C                   PARM                    DSMSKE
087100970526     C                   PARM                    DSMSER
087200970526     C*
087300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
087400970526     C                   CALL      'XKEY'
087500970526     C                   PARM                    DSKEY
087600970526     C*---
087700970526     C* VARIABILI NON RIFERITE AL DATA BASE
087800970526     C*---
087900970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
088000970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
088100970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
088200970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
088300970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
088400970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
088500970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
088600970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
088700970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
088800970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
088900970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
089000970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
089100970529     C     *LIKE         DEFINE    D00OP1        WOP1
089200970602     C     *LIKE         DEFINE    D00NAR        DNAR
089300970602     C     *LIKE         DEFINE    D00CAP        DCAP
089400970602     C     *LIKE         DEFINE    D00LOC        DLOC
089500970526     C*---
089600970526     C* VARIABILI DI CONTROLLO
089700970526     C*---
089800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
089900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
090000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
090100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
090200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
090300970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
090400970526     C*
090500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
090600970529     C                   MOVEL     'TISI30R'     DSMSPG
090700970529     C                   MOVEL     'TISI30R'     MSGQUE
090800970526     C                   MOVEL     *BLANKS       MSGKEY
090900970526     C                   MOVEL     *BLANKS       DSMSMS
091000970526     C                   MOVEL     *BLANKS       DSMSKE
091100970526     C                   Z-ADD     *ZEROS        DSMSER
091200970526     C*---
091300970526     C* VARIABILI RIFERITE AL DATA BASE
091400970526     C*---
091500970529     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
091600970529     C     *LIKE         DEFINE    CPLNAR        KCLNAR
091700970529     C     *LIKE         DEFINE    CPLCAP        KCLCAP
091800970529     C     *LIKE         DEFINE    CPLLOC        KCLLOC
091900970528     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
092000970528     C     *LIKE         DEFINE    CPCNAR        KCCNAR
092100970528     C     *LIKE         DEFINE    CPCCAP        KCCCAP
092200970529     C     *LIKE         DEFINE    CPLNAR        DEPNAR                         *DEPOSITI
092300970529     C     *LIKE         DEFINE    CPLCAP        DEPCAP
092400120515     C     *LIKE         DEFINE    CPCPRV        DEPPPR
092500970529     C     *LIKE         DEFINE    CPLLOC        DEPLOC
092600970526     C*---
092700970526     C* CHIAVI DI LETTURA
092800970526     C*---
092900970529     C* LETTURA AZCPL01L - PARZIALE
093000970529     C     KEYCPL        KLIST
093100970529     C                   KFLD                    KCLVER                         *VERSIONE
093200970529     C                   KFLD                    KCLNAR                         *NAZIONE
093300970529     C                   KFLD                    KCLCAP                         *CAP
093400970529     C* LETTURA AZCPL02L - COMPLETA
093500970529     C     KE2CPL        KLIST
093600970529     C                   KFLD                    KCLVER                         *VERSIONE
093700970605     C                   KFLD                    KCLNAR                         *NAZIONE
093800970529     C                   KFLD                    KCLLOC                         *LOCALITA'
093900970528     C* LETTURA AZCPC01L - COMPLETA
094000970528     C     KEYCPC        KLIST
094100970528     C                   KFLD                    KCCVER                         *VERSIONE
094200970528     C                   KFLD                    KCCNAR                         *NAZIONE
094300970528     C                   KFLD                    KCCCAP                         *CAP
094400130823     C     KCOS          KLIST
094500130823     C                   KFLD                    cplVER                         *VERSIONE
094600130823     C                   KFLD                    cplNAR                         *NAZIONE
094700130823     C                   KFLD                    cplCAP                         *CAP
094800130823     C                   KFLD                    cplloc
094900970527     C*
095000970527     C* REPERIMENTO DATI UTENTE
095100020919     C                   EXSR      REPDATIUTE
095200970527     C*
095300970527     C* CARICA LE TABELLE OCCORRENTI
095400970527     C                   EXSR      CARTAB
095500970526     C*
095600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
095700970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
095800970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
095900970526     C*
096000970526     C* CALCOLA LA DATA CORRENTE
096100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
096200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
096300970526     C                   Z-ADD     WN8           G08DAT
096400970526     C                   Z-ADD     *ZEROS        G08INV
096500970526     C                   MOVEL     '0'           G08ERR
096600970526     C                   CALL      'XSRDA8'
096700970526     C                   PARM                    WLBDA8
096800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
096900970526     C*
097000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
097100970526     C                   Z-ADD     D00DRI        G08INV
097200970526     C                   Z-ADD     *ZEROS        G08DAT
097300970526     C                   MOVEL     '3'           G08ERR
097400970526     C                   CALL      'XSRDA8'
097500970526     C                   PARM                    WLBDA8
097600970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
097700970527     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
097800970527     C                   MOVEL     D00CAP        VIDPCA                         *CAP
097900970529     C                   MOVEL     D00LOC        VIDPLO                         *LOCALITA
098000970526     C                   EXSR      CHKPAR
098100970526IF  1C     ERRL00        IFEQ      '1'
098200970526     C                   MOVEL     '1'           D00ERR
098300970526     C                   MOVEL     DSMSMS        D00MSG
098400970526X   1C                   ELSE
098500970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
098600970527     C                   MOVEL     VIDPCA        DEPCAP
098700120515     C                   MOVEL     VIDPPR        DEPPPR
098800970529     C                   MOVEL     VIDPLO        DEPLOC
098900970526E   1C                   ENDIF
099000970703     C*
099100970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
099200970703IF  1C     D00VER        IFEQ      *ZEROS
099300970707     C                   Z-ADD     D00DRI        P96DRI            8 0
099400970707     C                   Z-ADD     *ZEROS        P96VER            5 0
099500970707     C                   CALL      'TISI96R'
099600970707     C                   PARM                    P96DRI
099700970707     C                   PARM                    P96VER
099800970707     C                   Z-ADD     P96VER        D00VER
099900970703E   1C                   ENDIF
100000970526     C*
100100970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
100200970526     C                   EXSR      CHKFOP
100300970526     C*
100400970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
100500970530IF  1C     DEPOP0        IFEQ      'L01'
100600970530     C                   MOVEL     DEC(3)        VIDING
100700970530E   1C                   ENDIF
100800970530IF  1C     DEPOP0        IFEQ      'L02'
100900970530     C                   MOVEL     DEC(1)        VIDING
101000970530E   1C                   ENDIF
101100970530IF  1C     DEPOP0        IFEQ      'L05'
101200970530     C                   MOVEL     DEC(2)        VIDING
101300970530E   1C                   ENDIF
101400970526     C*
101500970526     C                   ENDSR
101600970526     C*------------------------------------------------------------------------*
101700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101900970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
102000970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
102100970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
102200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
102400970526     C*------------------------------------------------------------------------*
102500970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
102600970526     C*------------------------------------------------------------------------*
102700970526     C     POSSFL        BEGSR
102800970526     C*
102900970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
103000970526     C                   EXSR      CLRSFL
103100970526     C*
103200970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
103300970526     C                   EXSR      SETFIL
103400970526     C*
103500970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
103600970526     C     SFLEOF        IFEQ      0
103700970526     C                   EXSR      CARSFL
103800970526     C                   END
103900970526     C*
104000970526     C                   ENDSR
104100970526     C*------------------------------------------------------------------------*
104200970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
104300970526     C*------------------------------------------------------------------------*
104400970526     C     ENTATT        BEGSR
104500970526     C*
104600970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
104700970526     C                   Z-ADD     0             OPZOK             1 0
104800970526     C*
104900970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
105000970526     C                   Z-ADD     0             SFLVAR            5 0
105100970526     C*
105200970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
105300970526     C                   Z-ADD     0             SFLERR
105400970526     C*
105500970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
105600970526     C                   Z-ADD     SFLULT        WFLULT
105700970526     C*
105800970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
105900970529     C                   READC     SI30D05                                98
106000970526     C     *IN98         DOWEQ     '0'
106100970526     C*
106200970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
106300970526     C                   ADD       1             SFLVAR
106400970526     C*
106500970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
106600970526     C     OPZOK         IFEQ      0
106700970526     C*
106800970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
106900970526     C     OPZKEY        IFNE      *BLANKS
107000970526     C                   EXSR      OPZCON
107100970526     C                   END
107200970526     C*
107300970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
107400970526     C*            2. PULIZIA SCELTA IN SFL
107500970526     C     OPZOK         IFEQ      0
107600970526     C                   MOVEL     '0'           *IN41
107700970526     C                   MOVEL     *BLANKS       OPZKEY
107800970526     C                   ELSE
107900970526     C*
108000970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
108100970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
108200970526     C                   MOVEL     '1'           *IN41
108300970526     C                   Z-ADD     SFLULT        SFLERR
108400970526     C                   END
108500970526     C                   ELSE
108600970526     C*
108700970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
108800970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
108900970526     C                   MOVEL     '1'           *IN41
109000970526     C                   END                                                    OPZOK=0
109100970526     C*
109200970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
109300970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
109400970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
109500970526     C     OPZOK         IFNE      0
109600970526     C                   Z-ADD     SFLERR        SFLNBR
109700970526     C                   ELSE
109800970526     C                   Z-ADD     SFLULT        SFLNBR
109900970526     C                   END
110000970526     C*
110100970526     C* AGGIORNO RECORD SFL
110200970529     C                   UPDATE    SI30D05
110300970526     C*
110400970529     C                   READC     SI30D05                                98
110500970526     C                   END                                                    DO WHILE
110600970526     C*
110700970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
110800970526     C                   Z-ADD     WFLULT        SFLULT
110900970526     C*
111000970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
111100970526     C     SFLVAR        IFEQ      0
111200970526     C*---                 MOVEL'1'       FINL00
111300970526     C                   END
111400970526     C*
111500970526     C                   ENDSR
111600970526     C*------------------------------------------------------------------------*
111700970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
111800970526     C*------------------------------------------------------------------------*
111900970526     C     CLRSFL        BEGSR
112000970526     C*
112100970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
112200970526     C* (0=NON FINE FILE  1=FINE FILE)
112300970526     C                   Z-ADD     0             SFLEOF
112400970526     C*
112500970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
112600970526     C                   Z-ADD     0             SFLULT
112700970526     C*
112800970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
112900970526     C                   MOVEL     '1'           *IN40
113000970529     C                   WRITE     SI30D06
113100970526     C                   MOVEL     '0'           *IN40
113200970526     C*
113300970526     C                   ENDSR
113400970526     C*------------------------------------------------------------------------*
113500970526     C* CARSFL - CARICAMENTO SFL
113600970526     C*------------------------------------------------------------------------*
113700970526     C     CARSFL        BEGSR
113800970526     C*
113900970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
114000970526     C     SFLULT        IFEQ      0
114100970526     C                   EXSR      SETFIL
114200970526     C                   END
114300970526     C*
114400970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
114500970526     C                   Z-ADD     SFLULT        WFLULT
114600970526     C*
114700970526     C* CICLO:
114800970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
114900970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
115000970526     C                   Z-ADD     1             SFLRIG
115100970526     C     SFLRIG        DOWLE     SFLPAG
115200970526     C     SFLEOF        ANDEQ     0
115300970526     C*
115400970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
115500970526     C                   MOVEL     *BLANKS       OPZKEY
115600970526     C*
115700970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
115800970526     C                   EXSR      IRESFL
115900970526     C*
116000970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
116100970526     C                   ADD       1             SFLULT
116200970526     C*
116300970526     C* SCRITTURA RECORD SFL
116400970529     C                   WRITE     SI30D05
116500970526     C                   ADD       1             SFLRIG
116600970526     C*
116700970526     C* LETTURA FILE DA CARICARE
116800970526     C                   EXSR      LETFIL
116900970526     C                   END                                                    DO WHILE
117000970526     C*
117100970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
117200970526     C     WFLULT        IFNE      SFLULT
117300970526     C                   Z-ADD     WFLULT        SFLNBR
117400970526     C                   ADD       1             SFLNBR
117500970526     C                   END
117600970526     C*
117700970526     C                   ENDSR
117800970526     C*------------------------------------------------------------------------*
117900970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
118000970526     C*------------------------------------------------------------------------*
118100970526     C     OUTSFL        BEGSR
118200970526     C*
118300970526     C* WRITE RECORD TESTATA VIDEO
118400970529     C                   WRITE     SI30D01
118500970526     C*
118600970526     C* VISUALIZZAZIONE MESSAGGIO
118700970526     C                   EXSR      SNDMSG
118800970526     C*
118900970526     C* WRITE RECORD OPZIONI VIDEO
119000970529     C                   WRITE     SI30D02
119100970526     C*
119200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
119300970529     C                   WRITE     SI30D03
119400970526     C*
119500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
119600970529     C                   WRITE     SI30D04
119700970526     C*
119800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
119900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
120000970526     C     SFLEOF        IFEQ      0
120100970526     C                   MOVEL     '0'           *IN42
120200970526     C                   ELSE
120300970526     C*
120400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
120500970526     C                   MOVEL     '1'           *IN42
120600970526     C                   END
120700970526     C*
120800970526     C* WRITE RECORD TASTI FUNZIONALI
120900970529     C                   WRITE     SI30D09
121000970526     C*
121100970526     C* CASO DI SFL VUOTO
121200970526     C     SFLULT        IFLE      0
121300970526     C*
121400970526     C*    - WRITE RECORD VUOTO
121500970529     C                   WRITE     SI30D0A
121600970529     C                   READ      SI30D0A                                99
121700970526     C*
121800970526     C                   ELSE
121900970526     C*
122000970526     C* CASO DI SFL CON RECORDS
122100970526     C*
122200970526     C*    - WRITE RECORD CONTROLLO SFL
122300970529     C                   WRITE     SI30D06
122400970529     C                   READ      SI30D06                                99
122500970526     C                   END
122600970526     C*
122700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
122800970529     C                   READ      SI30D03                                99
122900970526     C*
123000970526     C                   ENDSR
123100970526     C*------------------------------------------------------------------------*
123200970526     C* SNDMSG - MANDA MESSAGGIO
123300970526     C*------------------------------------------------------------------------*
123400970526     C     SNDMSG        BEGSR
123500970526     C*
123600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
123700970526     C                   MOVEL     '1'           *IN40
123800970529     C                   WRITE     SI30DMC
123900970526     C                   MOVEL     '0'           *IN40
124000970526     C*
124100970526     C* VISUALIZZAZIONE MESSAGGIO
124200970526     C                   Z-ADD     0             MSGULT
124300970526     C                   CALL      'XMSG'        XMSGPA
124400970526     C     DSMSER        IFEQ      0
124500970526     C                   MOVEL     DSMSKE        MSGKEY
124600970526     C                   END
124700970526     C                   Z-ADD     1             MSGULT
124800970529     C                   WRITE     SI30DMS                                      SFL MESS
124900970529     C                   WRITE     SI30DMC                                      CONTR MESS
125000970526     C*
125100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
125200970526     C                   MOVEL     *BLANKS       DSMSMS
125300970526     C*
125400970526     C                   ENDSR
125500970526     C*------------------------------------------------------------------------*
125600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
125700970526     C*------------------------------------------------------------------------*
125800970526     C     FUNCON        BEGSR
125900970526     C*
126000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
126100970526     C                   EXSR      FUNVAR
126200970526     C*
126300970526     C* PULISCO CAMPI DI LAVORO
126400970526     C                   Z-ADD     0             I                 5 0
126500970526     C                   Z-ADD     0             W                 5 0
126600970526     C*
126700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
126800970526     C     FUNKEY        IFNE      *BLANKS
126900970526     C     1             DO        24            I
127000970526     C                   MOVEL     FUD(I)        FUDRIG
127100970526     C*
127200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
127300970526     C     FUNKEY        IFEQ      FUDFUN
127400970526     C     FUDATT        ANDEQ     'S'
127500970526     C                   Z-ADD     I             W
127600970526     C                   END
127700970526     C                   END                                                    DO WHILE
127800970526     C*
127900970526     C* FUNZIONE NON ATTIVA (ERRORE)
128000970526     C     W             IFEQ      0
128100970526     C*
128200970526     C* EMISSIONE MESSAGGIO DI ERRORE
128300970526     C                   MOVEL     MSG(2)        DSMSMS
128400970526     C                   END                                                    W=0
128500970526     C                   END                                                    FUNKEY NE *BLANKS
128600970526     C*
128700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
128800970526     C     W             IFNE      0
128900970526     C     FUNKEY        OREQ      *BLANKS
129000970526     C*
129100970526     C     FUNKEY        IFEQ      *BLANKS
129200970526     C                   EXSR      ENTGES
129300970526     C                   END
129400970526     C     FUNKEY        IFEQ      '03'
129500970526     C                   EXSR      F03GES
129600970526     C                   END
129700970526     C     FUNKEY        IFEQ      '05'
129800970526     C                   EXSR      F05GES
129900970526     C                   END
130000970526     C     FUNKEY        IFEQ      '06'
130100970526     C                   EXSR      F06GES
130200970526     C                   END
130300970526     C     FUNKEY        IFEQ      '12'
130400970526     C                   EXSR      F12GES
130500970526     C                   END
130600970529     C     FUNKEY        IFEQ      '13'
130700970529     C                   EXSR      F13GES
130800970529     C                   END
130900060404     C     FUNKEY        IFEQ      '14'
131000060404     C                   EXSR      F14GES
131100060404     C                   END
131200970526     C     FUNKEY        IFEQ      '23'
131300970526     C                   EXSR      F23GES
131400970526     C                   END
131500970526     C     FUNKEY        IFEQ      '24'
131600970526     C                   EXSR      F24GES
131700970526     C                   END
131800970526     C     FUNKEY        IFEQ      'RU'
131900970526     C                   EXSR      RUPGES
132000970526     C                   END
132100970526     C*
132200970526     C                   END
132300970526     C*
132400970526     C                   ENDSR
132500970526     C*------------------------------------------------------------------------*
132600970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
132700970526     C*------------------------------------------------------------------------*
132800970526     C     FUNVAR        BEGSR
132900970526     C*
133000970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
133100970526     C     KEY           IFEQ      ENTER
133200970526     C                   MOVEL     *BLANKS       FUNKEY
133300970526     C                   END
133400970526     C     KEY           IFEQ      F1
133500970526     C                   MOVEL     '01'          FUNKEY
133600970526     C                   END
133700970526     C     KEY           IFEQ      F2
133800970526     C                   MOVEL     '02'          FUNKEY
133900970526     C                   END
134000970526     C     KEY           IFEQ      F3
134100970526     C                   MOVEL     '03'          FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F4
134400970526     C                   MOVEL     '04'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F5
134700970526     C                   MOVEL     '05'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F6
135000970526     C                   MOVEL     '06'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F7
135300970526     C                   MOVEL     '07'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      F8
135600970526     C                   MOVEL     '08'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      F9
135900970526     C                   MOVEL     '09'          FUNKEY
136000970526     C                   END
136100970526     C     KEY           IFEQ      F10
136200970526     C                   MOVEL     '10'          FUNKEY
136300970526     C                   END
136400970526     C     KEY           IFEQ      F11
136500970526     C                   MOVEL     '11'          FUNKEY
136600970526     C                   END
136700970526     C     KEY           IFEQ      F12
136800970526     C                   MOVEL     '12'          FUNKEY
136900970526     C                   END
137000970526     C     KEY           IFEQ      F13
137100970526     C                   MOVEL     '13'          FUNKEY
137200970526     C                   END
137300970526     C     KEY           IFEQ      F14
137400970526     C                   MOVEL     '14'          FUNKEY
137500970526     C                   END
137600970526     C     KEY           IFEQ      F15
137700970526     C                   MOVEL     '15'          FUNKEY
137800970526     C                   END
137900970526     C     KEY           IFEQ      F16
138000970526     C                   MOVEL     '16'          FUNKEY
138100970526     C                   END
138200970526     C     KEY           IFEQ      F17
138300970526     C                   MOVEL     '17'          FUNKEY
138400970526     C                   END
138500970526     C     KEY           IFEQ      F18
138600970526     C                   MOVEL     '18'          FUNKEY
138700970526     C                   END
138800970526     C     KEY           IFEQ      F19
138900970526     C                   MOVEL     '19'          FUNKEY
139000970526     C                   END
139100970526     C     KEY           IFEQ      F20
139200970526     C                   MOVEL     '20'          FUNKEY
139300970526     C                   END
139400970526     C     KEY           IFEQ      F21
139500970526     C                   MOVEL     '21'          FUNKEY
139600970526     C                   END
139700970526     C     KEY           IFEQ      F22
139800970526     C                   MOVEL     '22'          FUNKEY
139900970526     C                   END
140000970526     C     KEY           IFEQ      F23
140100970526     C                   MOVEL     '23'          FUNKEY
140200970526     C                   END
140300970526     C     KEY           IFEQ      F24
140400970526     C                   MOVEL     '24'          FUNKEY
140500970526     C                   END
140600970526     C     KEY           IFEQ      ROLLUP
140700970526     C                   MOVEL     'RU'          FUNKEY
140800970526     C                   END
140900970526     C     KEY           IFEQ      HELP
141000970526     C                   MOVEL     'HP'          FUNKEY
141100970526     C                   END
141200970526     C*
141300970526     C                   ENDSR
141400970526     C*------------------------------------------------------------------------*
141500970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
141600970526     C*------------------------------------------------------------------------*
141700970526     C     FUNGES        BEGSR
141800970526     C*
141900970526     C* PULISCO CAMPI DI LAVORO
142000970526     C                   Z-ADD     0             W                 5 0
142100970526     C*
142200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
142300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
142400970526     C     1             DO        24            I
142500970526     C                   MOVEL     FUD(I)        FUDRIG
142600970526     C*
142700970526     C* PULIZIA FUNZIONI PRECEDENTI
142800970526     C                   MOVEL     *BLANKS       FUV(I)
142900970526     C*
143000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
143100970526     C* (SALTA F23 E F24)
143200970526     C     FUDATT        IFEQ      'S'
143300970526     C     FUDFUN        ANDNE     '23'
143400970526     C     FUDFUN        ANDNE     '24'
143500970526     C                   ADD       1             W
143600970526     C                   MOVEL     FUDDES        FUV(W)
143700970526     C                   END
143800970526     C                   END                                                    DO WHILE
143900970526     C*
144000970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
144100970526     C* ALTRIMENTI DISATTIVO F24
144200970526     C     1             DO        24            I
144300970526     C                   MOVEL     FUD(I)        FUDRIG
144400970526     C*
144500970526     C     FUDFUN        IFEQ      '24'
144600970526     C     W             IFGT      6
144700970526     C                   MOVEL     'S'           FUDATT
144800970526     C                   ELSE
144900970526     C                   MOVEL     *BLANKS       FUDATT
145000970526     C                   END                                                    W GT 8
145100970526     C                   MOVEL     FUDRIG        FUD(I)
145200970526     C                   END
145300970526     C                   END                                                    DO WHILE
145400970526     C*
145500970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
145600970526     C     W             DIV       6             I
145700970526     C                   MVR                     Z
145800970526     C*
145900970526     C     Z             IFNE      0
146000970526     C     I             ADD       1             Z
146100970526     C                   ELSE
146200970526     C                   Z-ADD     I             Z
146300970526     C                   END
146400970526     C*
146500970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
146600970526     C                   ADD       1             FUNGRU
146700970526     C     FUNGRU        IFGT      Z
146800970526     C     FUNGRU        ORLT      1
146900970526     C                   Z-ADD     1             FUNGRU
147000970526     C                   END
147100970526     C*
147200970526     C* PRIMO GRUPPO DI 6 FUNZIONI
147300970526     C     FUNGRU        IFEQ      1
147400970526     C                   MOVEL     FUV(1)        FUN001
147500970526     C                   MOVEL     FUV(2)        FUN002
147600970526     C                   MOVEL     FUV(3)        FUN003
147700970526     C                   MOVEL     FUV(4)        FUN004
147800970526     C                   MOVEL     FUV(5)        FUN005
147900970526     C                   MOVEL     FUV(6)        FUN006
148000970526     C                   END
148100970526     C*
148200970526     C* SECONDO GRUPPO DI 6 FUNZIONI
148300970526     C     FUNGRU        IFEQ      2
148400970526     C                   MOVEL     FUV(7)        FUN001
148500970526     C                   MOVEL     FUV(8)        FUN002
148600970526     C                   MOVEL     FUV(9)        FUN003
148700970526     C                   MOVEL     FUV(10)       FUN004
148800970526     C                   MOVEL     FUV(11)       FUN005
148900970526     C                   MOVEL     FUV(12)       FUN006
149000970526     C                   END
149100970526     C*
149200970526     C* TERZO GRUPPO DI 6 FUNZIONI
149300970526     C     FUNGRU        IFEQ      3
149400970526     C                   MOVEL     FUV(13)       FUN001
149500970526     C                   MOVEL     FUV(14)       FUN002
149600970526     C                   MOVEL     FUV(15)       FUN003
149700970526     C                   MOVEL     FUV(16)       FUN004
149800970526     C                   MOVEL     FUV(17)       FUN005
149900970526     C                   MOVEL     FUV(18)       FUN006
150000970526     C                   END
150100970526     C*
150200970526     C* QUARTO GRUPPO DI 6 FUNZIONI
150300970526     C     FUNGRU        IFEQ      4
150400970526     C                   MOVEL     FUV(19)       FUN001
150500970526     C                   MOVEL     FUV(20)       FUN002
150600970526     C                   MOVEL     FUV(21)       FUN003
150700970526     C                   MOVEL     FUV(22)       FUN004
150800970526     C                   MOVEL     *BLANKS       FUN005
150900970526     C                   MOVEL     *BLANKS       FUN006
151000970526     C                   END
151100970526     C*
151200970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
151300970526     C                   MOVEL     *BLANKS       FUN007
151400970526     C                   MOVEL     *BLANKS       FUN008
151500970526     C*
151600970526     C     1             DO        24            I
151700970526     C                   MOVEL     FUD(I)        FUDRIG
151800970526     C     FUDFUN        IFEQ      '23'
151900970526     C     FUDATT        ANDEQ     'S'
152000970526     C                   MOVEL     FUDDES        FUN007
152100970526     C                   END
152200970526     C     FUDFUN        IFEQ      '24'
152300970526     C     FUDATT        ANDEQ     'S'
152400970526     C                   MOVEL     FUDDES        FUN008
152500970526     C                   END
152600970526     C                   END                                                    DO
152700970526     C*
152800970526     C                   ENDSR
152900970526     C*------------------------------------------------------------------------*
153000970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
153100970526     C*------------------------------------------------------------------------*
153200970526     C     OPZGES        BEGSR
153300970526     C*
153400970526     C* PULISCO CAMPI DI LAVORO
153500970526     C                   Z-ADD     0             W                 5 0
153600970526     C*
153700970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
153800970526     C* E CARICO TABELLA OPZIONI ATTIVE
153900970526     C     1             DO        24            I
154000970526     C                   MOVEL     OPD(I)        OPDRIG
154100970526     C*
154200970526     C* PULIZIA OPZIONI PRECEDENTI
154300970526     C                   MOVEL     *BLANKS       OPV(I)
154400970526     C*
154500970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
154600970526     C     OPDATT        IFEQ      'S'
154700970526     C                   ADD       1             W
154800970526     C                   MOVEL     OPDDES        OPV(W)
154900970526     C                   END
155000970526     C                   END                                                    DO WHILE
155100970526     C*
155200970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
155300970526     C* ALTRIMENTI DISATTIVO F23
155400970526     C     1             DO        24            I
155500970526     C                   MOVEL     FUD(I)        FUDRIG
155600970526     C*
155700970526     C     FUDFUN        IFEQ      '23'
155800970526     C     W             IFGT      8
155900970526     C                   MOVEL     'S'           FUDATT
156000970526     C                   ELSE
156100970526     C                   MOVEL     *BLANKS       FUDATT
156200970526     C                   END                                                    W GT 8
156300970526     C                   MOVEL     FUDRIG        FUD(I)
156400970526     C                   END
156500970526     C                   END                                                    DO WHILE
156600970526     C*
156700970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
156800970526     C     W             DIV       8             I
156900970526     C                   MVR                     Z                 5 0
157000970526     C*
157100970526     C     Z             IFNE      0
157200970526     C     I             ADD       1             Z
157300970526     C                   ELSE
157400970526     C                   Z-ADD     I             Z
157500970526     C                   END
157600970526     C*
157700970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
157800970526     C                   ADD       1             OPZGRU
157900970526     C     OPZGRU        IFGT      Z
158000970526     C     OPZGRU        ORLT      1
158100970526     C                   Z-ADD     1             OPZGRU
158200970526     C                   END
158300970526     C*
158400970526     C* PRIMO GRUPPO DI 8 OPZIONI
158500970526     C     OPZGRU        IFEQ      1
158600970526     C                   MOVEL     OPV(1)        OPZ001
158700970526     C                   MOVEL     OPV(2)        OPZ002
158800970526     C                   MOVEL     OPV(3)        OPZ003
158900970526     C                   MOVEL     OPV(4)        OPZ004
159000970526     C                   MOVEL     OPV(5)        OPZ005
159100970526     C                   MOVEL     OPV(6)        OPZ006
159200970526     C                   MOVEL     OPV(7)        OPZ007
159300970526     C                   MOVEL     OPV(8)        OPZ008
159400970526     C                   END
159500970526     C*
159600970526     C* SECONDO GRUPPO DI 8 OPZIONI
159700970526     C     OPZGRU        IFEQ      2
159800970526     C                   MOVEL     OPV(9)        OPZ001
159900970526     C                   MOVEL     OPV(10)       OPZ002
160000970526     C                   MOVEL     OPV(11)       OPZ003
160100970526     C                   MOVEL     OPV(12)       OPZ004
160200970526     C                   MOVEL     OPV(13)       OPZ005
160300970526     C                   MOVEL     OPV(14)       OPZ006
160400970526     C                   MOVEL     OPV(15)       OPZ007
160500970526     C                   MOVEL     OPV(16)       OPZ008
160600970526     C                   END
160700970526     C*
160800970526     C* TERZO GRUPPO DI 8 OPZIONI
160900970526     C     OPZGRU        IFEQ      3
161000970526     C                   MOVEL     OPV(17)       OPZ001
161100970526     C                   MOVEL     OPV(18)       OPZ002
161200970526     C                   MOVEL     OPV(19)       OPZ003
161300970526     C                   MOVEL     OPV(20)       OPZ004
161400970526     C                   MOVEL     OPV(21)       OPZ005
161500970526     C                   MOVEL     OPV(22)       OPZ006
161600970526     C                   MOVEL     OPV(23)       OPZ007
161700970526     C                   MOVEL     OPV(24)       OPZ008
161800970526     C                   END
161900970526     C*
162000970526     C                   ENDSR
162100970526     C*------------------------------------------------------------------------*
162200970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
162300970526     C*------------------------------------------------------------------------*
162400970526     C     OPZCON        BEGSR
162500970526     C*
162600970526     C* PULISCO CAMPI DI LAVORO
162700970526     C                   Z-ADD     0             I                 5 0
162800970526     C                   Z-ADD     0             W                 5 0
162900970526     C*
163000970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
163100970526     C     1             DO        24            I
163200970526     C                   MOVEL     OPD(I)        OPDRIG
163300970526     C*
163400970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
163500970526     C     OPZKEY        IFEQ      OPDOPZ
163600970526     C     OPZKEY        OREQ      OPDOP1
163700970526     C*
163800970526     C     OPDATT        IFEQ      'S'
163900970526     C                   MOVEL     OPDOPZ        OPZKEY
164000970526     C                   Z-ADD     I             W
164100970526     C                   END
164200970526     C                   END
164300970526     C*
164400970526     C                   END                                                    DO WHILE
164500970526     C*
164600970526     C* OPZIONE NON ATTIVA (ERRORE)
164700970526     C     W             IFEQ      0
164800970526     C*
164900970526     C* OPZIONE NON OK
165000970526     C                   Z-ADD     1             OPZOK
165100970526     C*
165200970526     C* EMISSIONE MESSAGGIO DI ERRORE
165300970526     C                   MOVEL     MSG(1)        DSMSMS
165400970526     C                   ELSE
165500970526     C*
165600970526     C* OPZIONE ATTIVA (PROCEDI ...)
165700970526     C*
165800970526     C     OPZKEY        IFEQ      ' 1'
165900970526     C                   EXSR      O01GES
166000970526     C                   END
166100970526     C     OPZKEY        IFEQ      ' 2'
166200970526     C                   EXSR      O02GES
166300970526     C                   END
166400970611     C     OPZKEY        IFEQ      ' 3'
166500970611     C                   EXSR      O03GES
166600970611     C                   END
166700970526     C     OPZKEY        IFEQ      ' 4'
166800970526     C                   EXSR      O04GES
166900970526     C                   END
167000970526     C     OPZKEY        IFEQ      ' 5'
167100970526     C                   EXSR      O05GES
167200970526     C                   END
167300970526     C     OPZKEY        IFEQ      ' 7'
167400970526     C                   EXSR      O07GES
167500970526     C                   END
167600970529     C     OPZKEY        IFEQ      '13'
167700970529     C                   EXSR      O13GES
167800970529     C                   END
167900060404     C     OPZKEY        IFEQ      '14'
168000060404     C                   EXSR      O14GES
168100060404     C                   END
168200060404     C     OPZKEY        IFEQ      '15'
168300060404     C                   EXSR      O15GES
168400060404     C                   END
168500970526     C                   END                                                    OPZ.NON ATTIVA
168600970526     C*
168700970526     C                   ENDSR
168800970526     C*------------------------------------------------------------------------*
168900970526     C* NOTE COSTRUZIONE SCHIERE
169000970526     C*------------------------------------------------------------------------*
169100970526     C*
169200970526     C* -MSG-MESSAGGI
169300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
169400970526     C* (NON TOCCARE MSG,1 E MSG,2)
169500970526     C*
169600970526     C* -CMD-COMANDI
169700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
169800970526     C*
169900970526     C* -OPD-OPZIONI
170000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
170100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
170200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
170300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
170400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
170500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
170600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
170700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
170800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
170900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
171000970526     C*
171100970526     C* -FUD-FUNZIONI
171200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
171300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
171400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
171500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
171600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
171700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
171800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
171900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
172000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
172100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
172200970526     O*------------------------------------------------------------------------*
172300970526** DEC - DECODIFICHE
172400970526    GESTIONE                                                     1
172500970530INTERROGAZIONE                                                   2
172600970530     SCELTA                                                      3
1727009705264
1728009705265
172900970526** MSG - MESSAGGI
173000970529TISI30R- Opzione non gestita                                                   1
173100970529TISI30R- Tasto funzionale non gestito                                          2
173200970704TISI30R- 3
173300970529TISI30R- Record annullato. Consentito solo il ripristino                       4
173400970529TISI30R- Record non annullato. Impossibile ripristinare                        5
173500970529TISI30R- Accettato solo un parametro di inizio lista                           6
173600970529TISI30R- 7                                                                     7
173700970529TISI30R- 8                                                                     8
173800970529TISI30R- 9                                                                     9
173900970529TISI30R- 10                                                                   10
174000970529TISI30R- 11                                                                   11
174100970529TISI30R- 12                                                                   12
174200970529TISI30R- 13                                                                   13
174300970529TISI30R- 14                                                                   14
174400970529TISI30R- 15                                                                   15
174500970529TISI30R- 16                                                                   16
174600970529TISI30R- 17                                                                   17
174700970529TISI30R- 18                                                                   18
174800970529TISI30R- 19                                                                   19
174900970529TISI30R- 20                                                                   20
175000970526** OPD - OPZIONI
175100970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
175200970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
175300970611 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
175400970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
175500970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
175600970526                   -6 -     (S/N)=N                        opz. 06¬
175700970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
175800970527                   -8 -     (S/N)=N                        opz. 08¬
175900970527                   -9 -     (S/N)=N                        opz. 09¬
176000970527                   -10-     (S/N)=N                        opz. 10¬
176100970529                   -11-     (S/N)=N                        opz. 11¬
176200970529                   -12-     (S/N)=N                        opz. 12¬
17630097062613=Sinonimi        -13-     (S/N)=N                        opz. 13¬
17640006040314=Cal.Eventi Part -14-     (S/N)=N                        opz. 14¬
17650006040315=InserisciEvePart-15-     (S/N)=N                        opz. 15¬
176600970526                   -  -     (S/N)=N                        opz. 16¬
176700970526                   -  -     (S/N)=N                        opz. 17¬
176800970526                   -  -     (S/N)=N                        opz. 18¬
176900970526                   -  -     (S/N)=N                        opz. 19¬
177000970526                   -  -     (S/N)=N                        opz. 20¬
177100970526                   -  -     (S/N)=N                        opz. 21¬
177200970526                   -  -     (S/N)=N                        opz. 22¬
177300970526                   -  -     (S/N)=N                        opz. 23¬
177400970526                   -  -     (S/N)=N                        opz. 24¬
177500970526** FUD - FUNZIONI
177600970526                   -        (S/N)=N                        funz.01¬
177700970526                   -        (S/N)=N                        funz.02¬
177800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
177900970526                   -        (S/N)=N                        funz.04¬
178000970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
178100970526F06=Inserimento    -        (S/N)=N                        funz.06¬
178200970526                   -        (S/N)=N                        funz.07¬
178300970527                   -        (S/N)=N                        funz.08¬
178400970527                   -        (S/N)=N                        funz.09¬
178500970527                   -        (S/N)=N                        funz.10¬
178600970526                   -        (S/N)=N                        funz.11¬
178700970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
178800970626F13=Sinonimi       -        (S/N)=N                        funz.13¬
178900060403F14=Cal.Eventi Part-        (S/N)=N                        funz.14¬
179000970526                   -        (S/N)=N                        funz.15¬
179100970526                   -        (S/N)=N                        funz.16¬
179200970526                   -        (S/N)=N                        funz.17¬
179300970526                   -        (S/N)=N                        funz.18¬
179400970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
179500970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
179600970527                   -        (S/N)=N                        funz.21¬
179700970527                   -        (S/N)=N                        funz.22¬
179800970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
179900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
