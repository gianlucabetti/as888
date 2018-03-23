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
002100970526     D*------------------------------------------------------------------------*
002200970526     D* SCHIERE
002300970526     D*------------------------------------------------------------------------*
002400970526     D*------------
002500970526     D* DECODIFICHE
002600970526     D*------------
002700970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* MESSAGGI
003000970526     D*------------
003100970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI OPZIONI
003400970526     D*------------
003500970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI TASTI FUNZIONALI
003800970526     D*------------
003900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* OPZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970526     D OPV             S             19    DIM(24)
004400970526     D*------------
004500970526     D* FUNZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970527     D FUV             S             19    DIM(24)
004800970526     D*------------------------------------------------------------------------*
004900970526     D* INPUT
005000970526     D*------------------------------------------------------------------------*
005100970526     D*------------
005200970526     D* RIDEFINIZIONE OPZIONI VIDEO
005300970526     D*------------
005400970526     D OPDDS           DS
005500970526     D  OPDOPZ                 1      2
005600970526     D  OPDDE1                 3     19
005700970526     D  OPDOP1                21     22
005800970526     D  OPDFI0                23     34
005900970526     D  OPDATT                35     35
006000970526     D  OPDFI1                36     80
006100970526     D  OPDDES                 1     19
006200970526     D  OPDRIG                 1     80
006300970526     D*------------
006400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006500970526     D*------------
006600970526     D FUDDS           DS
006700970526     D  FUDDE1                 1      1
006800970526     D  FUDFUN                 2      3
006900970526     D  FUDDE2                 4     19
007000970526     D  FUDFI0                20     34
007100970526     D  FUDATT                35     35
007200970526     D  FUDFI1                36     80
007300970526     D  FUDDES                 1     19
007400970526     D  FUDRIG                 1     80
007500970526     D*------------
007600970526     D* DS GESTIONE TASTI FUNZIONALI
007700970526     D*------------
007800970526     D DSKEY         E DS
007900970526     D DEVDS1          DS
008000970526     D  KEY                  369    369
008100970526     D*------------
008200970526     D* DS "XMSG" - GESTIONE MESSAGGI
008300970526     D*------------
008400970526     D DSMSG         E DS
008500970526     D*------------
008600020919     D* DS REPERIMENTO DATI UTENTE
008700970526     D*------------
008800020919     D TIBS34DS      E DS                                                       *Profili utente
008900020919     D DDATIUTE      E DS                                                       *Dati utente
009000020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009100061026     D DUTE01        E DS                                                       *Abilitazioni
009200970527     D*------------------
009300970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009400970527     D*------------------
009500970527     D WLBDA8          DS                  INZ
009600970527     D  G08DAT                 1      8  0
009700970527     D  G08INV                 9     16  0
009800970527     D  G08ERR                17     17
009900970527     D  G08TGI                18     22  0
010000970526     D*------------
010100970526     D* DS DI PROCEDURA
010200970526     D*------------
010300970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010400060404     D tisip0ds      E DS
010500970526     D*------------
010600970526     D* ARCHITETTURA
010700970526     D*------------
010800970526     D KPJBA         E DS
010900060404     d
011000060404     d tipopgm         s              1
011100060404     d wdri            s                   like(Dp0dri)
011200060404     d dprv            s                   like(Dp0prv)
011300970526     C*------------------------------------------------------------------------*
011400970526     C* MAIN LINES
011500970526     C*------------------------------------------------------------------------*
011600970526     C*
011700970526     C* SE NON CI SONO ERRORI, PROSEGUE
011800970526IF  1C     ERRL00        IFNE      '1'
011900970526     C*
012000970526     C* CARICA IL SFILE
012100970526     C                   EXSR      POSSFL
012200970526     C*
012300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012700970526     C*
012800970526     C* REIMPOSTA ERRORE DI PROCERURA
012900970526     C                   MOVEL     '0'           ERRL00
013000970526     C*
013100970526     C* EMETTE IL SFILE
013200970526     C                   EXSR      OUTSFL
013300970526     C*
013400970526     C* GESTIONE TASTI FUNZIONALI
013500970526     C                   EXSR      FUNCON
013600970526     C*
013700970526E   2C                   ENDDO
013800970527     C*
013900970527     C* CI SONO ERRORI IN ENTRATA
014000970527X   1C                   ELSE
014100970527     C                   MOVEL     '1'           D00ERR
014200970527     C                   MOVEL     DSMSMS        D00MSG
014300970526E   1C                   ENDIF
014400970526     C* FINE PGM
014500970526     C                   SETON                                        LR
014600970526     C*------------------------------------------------------------------------*
014700970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014800970526     C*------------------------------------------------------------------------*
014900970526     C     F03GES        BEGSR
015000970526     C*
015100970526     C* IMPOSTA "PREMUTO F03"
015200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015300970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
015400970526     C*
015500970526     C                   ENDSR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015800970526     C*------------------------------------------------------------------------*
015900970526     C     F05GES        BEGSR
016000970526     C*
016100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016200970526     C*
016300970526     C                   ENDSR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016600970526     C*------------------------------------------------------------------------*
016700970526     C     F06GES        BEGSR
016800970526     C*
016900970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
017000020919IF  1C     DUTLPO        IFNE      'S'
017100970526     C                   MOVEL     '1'           ERRL00
017200970526     C                   MOVEL     MSG(3)        DSMSMS
017300970526E   1C                   ENDIF
017400970526     C*
017500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
017600970603IF  2C     D00NAR        IFNE      *BLANKS
017700970603     C     D00CAP        ORNE      *BLANKS
017800970603     C                   MOVEL     D00NAR        DNAR
017900970603     C                   MOVEL     D00CAP        DCAP
018000970603X   2C                   ELSE
018100970603     C                   MOVEL     *BLANKS       DNAR
018200970602     C                   MOVEL     *BLANKS       DCAP
018300970603E   2C                   ENDIF
018400970602     C                   MOVEL     *BLANKS       DLOC
018500970529     C                   MOVEL     DEPOP0        WOP0
018600970529     C                   MOVEL     'F06'         WOP1
018700970529     C                   MOVEL     'TISI31R'     WPGM              8
018800970526     C                   EXSR      LANPGM                                       *LANCIA PGM
018900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019000970526E   1C                   ENDIF
019100970526     C*
019200970526     C                   ENDSR
019300970526     C*------------------------------------------------------------------------*
019400970526     C* F12GES - GESTIONE F12 -> RITORNO
019500970526     C*------------------------------------------------------------------------*
019600970526     C     F12GES        BEGSR
019700970526     C*
019800970526     C* IMPOSTA "PREMUTO F12"
019900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020000970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
020100970526     C*
020200970526     C                   ENDSR
020300970529     C*------------------------------------------------------------------------*
020400970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
020500970529     C*------------------------------------------------------------------------*
020600970529     C     F13GES        BEGSR
020700970529     C*
020800970602     C                   MOVEL     *BLANKS       DNAR
020900970602     C                   MOVEL     *BLANKS       DCAP
021000970602     C                   MOVEL     *BLANKS       DLOC
021100970529     C                   MOVEL     DEPOP0        WOP0
021200970529     C                   MOVEL     'S'           WOP0
021300970529     C                   MOVEL     'F13'         WOP1
021400970529     C                   MOVEL     'TISI35R'     WPGM              8
021500970529     C                   EXSR      LANPGM
021600970529     C*
021700970529     C                   ENDSR
021800060404     C*------------------------------------------------------------------------*
021900060404     C* F14GES - GESTIONE F14 -> calendario eventi particolari
022000060404     C*------------------------------------------------------------------------*
022100060404     C     F14GES        BEGSR
022200060404     C*
022300060404     C                   MOVEL     *BLANKS       DNAR
022400060404     C                   MOVEL     *BLANKS       DCAP
022500060404     C                   MOVEL     *BLANKS       Dloc
022600060404     C                   MOVEL     *BLANKS       Dprv
022700060404     C                   MOVEL     DEPOP0        WOP0
022800060404     c                   clear                   wop1
022900060404     C                   MOVEL     datcor        wdri
023000060404     C                   MOVEL     'TISIP0R'     WPGM              8
023100060404     C                   EXSR      LANPGM
023200060404     C*
023300060404     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023600970526     C*------------------------------------------------------------------------*
023700970526     C     F23GES        BEGSR
023800970526     C*
023900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024400970526     C*------------------------------------------------------------------------*
024500970526     C     F24GES        BEGSR
024600970526     C*
024700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
025200970526     C*------------------------------------------------------------------------*
025300970526     C     RUPGES        BEGSR
025400970526     C*
025500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025600970526     C*
025700970526     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900970526     C* ENTGES - GESTIONE ENTER -> INVIO
026000970526     C*------------------------------------------------------------------------*
026100970526     C     ENTGES        BEGSR
026200970526     C*
026300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
026400970526     C                   EXSR      NEWPOS
026500970526     C*
026600970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026800970526IF  2C     SWPOSI        IFNE      0
026900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
027000970526X   2C                   ELSE
027100970526IF  3C     SFLULT        IFNE      0
027200970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
027300970526     C*
027400970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027500970526IF  4C     SWMODI        IFNE      0
027600970526     C                   EXSR      POSSFL
027700970526E   4C                   ENDIF
027800970526E   3C                   ENDIF
027900970526E   2C                   ENDIF
028000970526E   1C                   ENDIF
028100970526     C*
028200970526     C                   ENDSR
028300970526     C*------------------------------------------------------------------------*
028400970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
028500970526     C*------------------------------------------------------------------------*
028600970526     C     O01GES        BEGSR
028700970526     C*
028800970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028900970527     C                   MOVEL     VIDNAR        D00NAR
029000970527     C                   MOVEL     VIDCAP        D00CAP
029100970529     C                   MOVEL     VIDLOC        D00LOC
029200970702     C                   MOVEL     VIDCPR        D00PRV
029300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029400970526     C*
029500970526     C                   ENDSR
029600970526     C*------------------------------------------------------------------------*
029700970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
029800970526     C*------------------------------------------------------------------------*
029900970526     C     O02GES        BEGSR
030000970526     C*
030100970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
030200970526IF  1C     VIDATB        IFEQ      'A'
030300970526     C                   MOVEL     '1'           ERRL00
030400970526     C                   MOVEL     MSG(4)        DSMSMS
030500970526E   1C                   ENDIF
030600970527     C* MODIFICA
030700970704IF  1C     ERRL00        IFEQ      '0'
030800970602     C                   MOVEL     VIDNAR        DNAR
030900970602     C                   MOVEL     VIDCAP        DCAP
031000970602     C                   MOVEL     VIDLOC        DLOC
031100970529     C                   MOVEL     DEPOP0        WOP0
031200970526     C                   MOVEL     'O02'         WOP1
031300970529     C                   MOVEL     'TISI31R'     WPGM
031400970526     C                   EXSR      LANPGM
031500970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
031600970528E   1C                   ENDIF
031700970526     C*
031800970526     C                   ENDSR
031900970611     C*------------------------------------------------------------------------*
032000970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
032100970611     C*------------------------------------------------------------------------*
032200970611     C     O03GES        BEGSR
032300970611     C*
032400970611     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
032500970611IF  1C     VIDATB        IFEQ      'A'
032600970611     C                   MOVEL     '1'           ERRL00
032700970611     C                   MOVEL     MSG(4)        DSMSMS
032800970611E   1C                   ENDIF
032900970611     C* DUPLICA
033000970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033100970611     C                   MOVEL     VIDNAR        DNAR
033200970611     C                   MOVEL     VIDCAP        DCAP
033300970611     C                   MOVEL     VIDLOC        DLOC
033400970611     C                   MOVEL     DEPOP0        WOP0
033500970611     C                   MOVEL     'O03'         WOP1
033600970611     C                   MOVEL     'TISI32R'     WPGM
033700970611     C                   EXSR      LANPGM
033800970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
033900970611E   1C                   ENDIF
034000970611     C*
034100970611     C                   ENDSR
034200970526     C*------------------------------------------------------------------------*
034300970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
034400970526     C*------------------------------------------------------------------------*
034500970526     C     O04GES        BEGSR
034600970526     C*
034700970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
034800970526IF  1C     VIDATB        IFEQ      'A'
034900970526     C                   MOVEL     '1'           ERRL00
035000970526     C                   MOVEL     MSG(4)        DSMSMS
035100970526E   1C                   ENDIF
035200970527     C* ANNULLAMENTO
035300970704IF  1C     ERRL00        IFEQ      '0'
035400970602     C                   MOVEL     VIDNAR        DNAR
035500970602     C                   MOVEL     VIDCAP        DCAP
035600970602     C                   MOVEL     VIDLOC        DLOC
035700970529     C                   MOVEL     DEPOP0        WOP0
035800970526     C                   MOVEL     'O04'         WOP1
035900970529     C                   MOVEL     'TISI31R'     WPGM
036000970526     C                   EXSR      LANPGM
036100970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
036200970528E   1C                   ENDIF
036300970526     C*
036400970526     C                   ENDSR
036500970526     C*------------------------------------------------------------------------*
036600970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
036700970526     C*------------------------------------------------------------------------*
036800970526     C     O05GES        BEGSR
036900970526     C*
037000970602     C                   MOVEL     VIDNAR        DNAR
037100970602     C                   MOVEL     VIDCAP        DCAP
037200970602     C                   MOVEL     VIDLOC        DLOC
037300970529     C                   MOVEL     DEPOP0        WOP0
037400970526     C                   MOVEL     'O05'         WOP1
037500970529     C                   MOVEL     'TISI31R'     WPGM
037600970526     C                   EXSR      LANPGM
037700970526     C*
037800970526     C                   ENDSR
037900970526     C*------------------------------------------------------------------------*
038000970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
038100970526     C*------------------------------------------------------------------------*
038200970526     C     O07GES        BEGSR
038300970526     C*
038400970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
038500970526IF  1C     VIDATB        IFNE      'A'
038600970526     C                   MOVEL     '1'           ERRL00
038700970526     C                   MOVEL     MSG(5)        DSMSMS
038800970526E   1C                   ENDIF
038900970526     C* RIPRISITINO
039000970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039100970602     C                   MOVEL     VIDNAR        DNAR
039200970602     C                   MOVEL     VIDCAP        DCAP
039300970602     C                   MOVEL     VIDLOC        DLOC
039400970529     C                   MOVEL     DEPOP0        WOP0
039500970526     C                   MOVEL     'O07'         WOP1
039600970529     C                   MOVEL     'TISI31R'     WPGM
039700970526     C                   EXSR      LANPGM
039800970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
039900970528E   1C                   ENDIF
040000970526     C*
040100970526     C                   ENDSR
040200970529     C*------------------------------------------------------------------------*
040300970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
040400970529     C*------------------------------------------------------------------------*
040500970529     C     O13GES        BEGSR
040600970529     C*
040700970602     C                   MOVEL     VIDNAR        DNAR
040800970602     C                   MOVEL     VIDCAP        DCAP
040900970602     C                   MOVEL     VIDLOC        DLOC
041000970529     C                   MOVEL     DEPOP0        WOP0
041100970529     C                   MOVEL     'S'           WOP0
041200970529     C                   MOVEL     'O13'         WOP1
041300970529     C                   MOVEL     'TISI35R'     WPGM
041400970529     C                   EXSR      LANPGM
041500970529     C*
041600970529     C                   ENDSR
041700060404     C*------------------------------------------------------------------------*
041800060404     C* O14GES - GESTIONE calendario eventi particolari
041900060404     C*------------------------------------------------------------------------*
042000060404     C     O14GES        BEGSR
042100060404     C*
042200060404     C                   MOVEL     VIDNAR        DNAR
042300060404     C                   MOVEL     VIDCAP        DCAP
042400060404     c                   clear                   dloc
042500060404     c                   clear                   dprv
042600060404     C                   MOVEL     DEPOP0        WOP0
042700060404     C                   clear                   WOP1
042800060404     C                   MOVEL     datcor        wdri
042900060404     c                   move      0101          wdri
043000060404     C                   MOVEL     'TISIP0R'     WPGM
043100060404     C                   EXSR      LANPGM
043200060404     C*
043300060404     C                   ENDSR
043400060404     C*------------------------------------------------------------------------*
043500060404     C* O15GES - Inserisci  evento particolare
043600060404     C*------------------------------------------------------------------------*
043700060404     C     O15GES        BEGSR
043800060404     C*
043900060404     C                   MOVEL     VIDNAR        DNAR
044000060404     C                   MOVEL     VIDCAP        DCAP
044100060404     C                   MOVEL     VIDcpr        Dprv
044200060404     C                   MOVEL     VIDLOC        DLOC
044300060404     C                   MOVEL     DEPOP0        WOP0
044400060404     C                   movel     'F06'         WOP1
044500060404     C                   MOVEL     datcor        wdri
044600060404     C                   MOVEL     'TISIP1R'     WPGM
044700060404     C                   EXSR      LANPGM
044800060404     C*
044900060404     C                   ENDSR
045000970526     C*------------------------------------------------------------------------*
045100970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
045200970526     C*------------------------------------------------------------------------*
045300970526     C     NEWPOS        BEGSR
045400970526     C*
045500970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
045600970526     C*         1= RICHIESTO POSIZIONAMENTO
045700970526     C                   Z-ADD     0             SWPOSI            7 0
045800970526     C*
045900970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
046000970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
046100970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
046200970526     C                   Z-ADD     0             SWMODI            1 0
046300970530     C*
046400970530     C* REIMPOSTA INDICATORI DI ERRORE
046500970530     C                   SETOFF                                       808182
046600970526     C*
046700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
046800970527IF  1C     VIDPNA        IFNE      DEPNAR
046900970527     C     VIDPCA        ORNE      DEPCAP
047000970529     C     VIDPLO        ORNE      DEPLOC
047100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
047200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
047300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
047400970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
047500970527     C                   MOVEL     VIDPCA        DEPCAP
047600970529     C                   MOVEL     VIDPLO        DEPLOC
047700970526E   2C                   ENDIF
047800970526E   1C                   ENDIF
047900970526     C*
048000970526     C                   ENDSR
048100970526     C*------------------------------------------------------------------------*
048200970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
048300970526     C*------------------------------------------------------------------------*
048400970526     C     SETFIL        BEGSR
048500970529     C*
048600970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
048700970529IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
048800970529     C                   Z-ADD     D00VER        KCLVER
048900970529     C                   MOVEL     VIDPNA        KCLNAR
049000970529     C                   MOVEL     VIDPCA        KCLCAP
049100970529     C     KEYCPL        SETLL     AZCPL01L                           99
049200970529E   1C                   ENDIF
049300970529IF  1C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
049400970529     C                   Z-ADD     D00VER        KCLVER
049500970605     C                   MOVEL     VIDPNA        KCLNAR
049600970529     C                   MOVEL     VIDPLO        KCLLOC
049700970529     C     KE2CPL        SETLL     AZCPL02L                           99
049800970529E   1C                   ENDIF
049900970526     C* LETTURA RECORD
050000970526IF  1C     *IN99         IFEQ      *OFF
050100970526     C                   EXSR      LETFIL
050200970526E   1C                   ENDIF
050300970526     C*
050400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
050500970526IF  1C     *IN99         IFEQ      *ON
050600970526     C                   Z-ADD     1             SFLEOF
050700970526E   1C                   ENDIF
050800970526     C*
050900970526     C                   ENDSR
051000970526     C*------------------------------------------------------------------------*
051100970526     C* LETFIL - LETTURA RECORD
051200970526     C*------------------------------------------------------------------------*
051300970526     C     LETFIL        BEGSR
051400970526     C*
051500970526     C* LEGGE FINO A:
051600970526     C*     - FINE FILE
051700970526     C*     - TROVATO RECORD VALIDO
051800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
051900970526DO  1C     *IN99         DOWEQ     *OFF
052000970526     C     WRECOK        ANDEQ     'N'
052100970529     C*
052200970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
052300970529IF  2C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
052400970603     C     KCLVER        READE     AZCPL01L                               99
052500970529E   2C                   ENDIF
052600970529IF  2C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
052700970603     C     KCLVER        READE     AZCPL02L                               99
052800970529E   2C                   ENDIF
052900970526     C*
053000970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
053100970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
053200970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
053300970530X   2C                   ELSE
053400970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
053500970530E   2C                   ENDIF
053600970526E   1C                   ENDDO
053700970526     C*
053800970526     C                   ENDSR
053900970526     C*------------------------------------------------------------------------*
054000970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
054100970526     C*------------------------------------------------------------------------*
054200970526     C     CHKREC        BEGSR
054300970526     C*
054400970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
054500970530     C*
054600970530     C* SE RICHIESTO ORDINAMENTO PER CAP, ESCLUDE LE LOCALITA' NON DEL CAP
054700970530IF  1C     VIDPNA        IFNE      *BLANKS
054800970530     C     VIDPCA        ORNE      *BLANKS
054900970530IF  2C     VIDPNA        IFNE      CPLNAR
055000970530     C     VIDPCA        ORNE      CPLCAP
055100970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055200970530E   2C                   ENDIF
055300970530E   1C                   ENDIF
055400001005     C*
055500001005     C* LEGGE IL CAP DELLA LOCALIÀ'
055600001005IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
055700001005     C                   Z-ADD     CPLVER        KCCVER
055800001005     C                   MOVEL     CPLNAR        KCCNAR
055900001005     C                   MOVEL     CPLCAP        KCCCAP
056000001005     C     KEYCPC        CHAIN     AZCPC01L                           98
056100001005IF  2C     *IN98         IFEQ      *ON                                          *NON TROVATO
056200001005     C                   CLEAR                   AZCPC000                       *AZZERA CAMPI CAP
056300001005E   2C                   ENDIF
056400001005E   1C                   ENDIF
056500001005     C*
056600001005     C* SE L'UTENTE E' IN SCELTA, ESCLUDE LE LOCALITÀ OBSOLETE
056700001005IF  1C     DEPOP0        IFEQ      'L01'
056800061010IF  2C***  CPLLIV        IFEQ      'L'
056900061010     C***  CPLFI1        ANDNE     *BLANKS
057000061010     C***  CPLLIV        OREQ      'C'
057100061010     C***  CPCFI1        ANDNE     *BLANKS
057200061010     c* escludere sempre a prescindere dal livello gestione
057300061010     c     cplfi1        ifne      *blanks
057400001005     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057500001005E   2C                   ENDIF
057600001005E   1C                   ENDIF
057700001005     C*
057800970526     C                   ENDSR
057900970526     C*------------------------------------------------------------------------*
058000970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
058100970526     C*------------------------------------------------------------------------*
058200970526     C     IRESFL        BEGSR
058300970529     C*
058400970529     C                   MOVEL     *BLANKS       OPZKEY
058500970925     C     CPLATB        IFEQ      *BLANKS
058600970925     C                   MOVEL     CPLFI1        VIDATB
058700970925     C                   ELSE
058800970529     C                   MOVEL     CPLATB        VIDATB
058900970925     C                   ENDIF
059000970529     C                   MOVEL     CPLNAR        VIDNAR
059100970529     C                   MOVEL     CPLCAP        VIDCAP
059200970529     C                   MOVEL     CPLLOC        VIDLOC
059300970530     C                   MOVEL     CPLLIV        VIDLIV
059400970529     C                   Z-ADD     CPLTTC        VIDTTC
059500970529     C                   MOVEL     CPLISO        VIDISO
059600970529     C                   Z-ADD     CPLLNA        VIDLNA
059700970529     C                   Z-ADD     CPLZNC        VIDZNC
059800970529     C                   MOVEL     CPLDLV        VIDDLV
059900970529     C                   MOVEL     *BLANKS       VIDESS
060000970529IF  1C     CPLESS        IFEQ      'S'
060100970529     C                   MOVEL     'S'           VIDESS                         *ESISTONO SINONIMI
060200970529E   1C                   ENDIF
060300001005     C* DATI DAL CAP
060400970530     C                   MOVEL     CPCPRV        VIDCPR
060500970530     C                   Z-ADD     CPCLNA        VIHCLA
060600001005IF  1C     CPLLIV        IFEQ      'C'                                          *LIV.GESTIONE 'CAP'
060700970529     C                   Z-ADD     CPCTTC        VIDTTC
060800970529     C                   MOVEL     CPCISO        VIDISO
060900970529     C                   Z-ADD     CPCLNA        VIDLNA
061000970529     C                   Z-ADD     CPCZNC        VIDZNC
061100001005E   1C                   ENDIF
061200970526     C*
061300970526     C                   ENDSR
061400970526     C*------------------------------------------------------------------------*
061500970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
061600970526     C*------------------------------------------------------------------------*
061700970526     C     CHKPAR        BEGSR
061800970529     C*
061900970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
062000970529IF  1C     VIDPCA        IFEQ      *BLANKS
062100970529     C     VIDPLO        ANDEQ     *BLANKS
062200970529     C                   MOVEL     'CAP'         WTIPRI            3
062300970529E   1C                   ENDIF
062400970529     C*
062500970529     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
062600970529IF  1C     ERRL00        IFEQ      '0'
062700970529IF  2C     VIDPCA        IFNE      *BLANKS
062800970529     C     VIDPLO        ANDNE     *BLANKS
062900970529     C                   MOVEL     MSG(6)        DSMSMS
063000970529     C                   MOVEL     '1'           ERRL00
063100970529     C                   SETON                                        808182
063200970529E   2C                   ENDIF
063300970529E   1C                   ENDIF
063400970529     C*
063500970529     C* CONTROLLA IL TIPO RICERCA
063600970529IF  1C     ERRL00        IFEQ      '0'
063700970529IF  2C     VIDPCA        IFNE      *BLANKS
063800970529     C                   MOVEL     'CAP'         WTIPRI
063900970529E   2C                   ENDIF
064000970529IF  2C     VIDPLO        IFNE      *BLANKS
064100970529     C                   MOVEL     'LOC'         WTIPRI
064200970529E   2C                   ENDIF
064300970529E   1C                   ENDIF
064400970526     C*
064500970526     C                   ENDSR
064600970526     C*------------------------------------------------------------------------*
064700970526     C* LANPGM - LANCIA UN PROGRAMMA
064800970526     C*------------------------------------------------------------------------*
064900970526     C     LANPGM        BEGSR
065000970526     C*
065100970526     C* IMPOSTA LA DS DI PROCEDURA
065200060404     c                   eval      tipopgm=%subst(wpgm:5:1)
065300060404     c                   if        tipopgm='P'
065400060404     c                   clear                   tisip0ds
065500060404     C                   MOVEL     DNAR          Dp0NAR                         *NAZIONE
065600060404     C                   MOVEL     DCAP          Dp0CAP                         *CAP
065700060404     C                   MOVEL     Dloc          Dp0loc                         *CAP
065800060404     C                   MOVEL     Dprv          Dp0prv                         *CAP
065900060404     C                   MOVEL     wop0          dp0OP0
066000060404     C                   MOVEL     WOP1          Dp0OP1                         *LIVELLO PGM
066100060404     c                   movel     wdri          dp0dri
066200060404     c                   call      WPGM
066300060404     c                   parm                    kpjba
066400060404     c                   parm                    tisip0ds
066500060404     C*
066600060404     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
066700060404IF  1C     Dp0F03        IFEQ      '1'                                          *PREMUTO F03
066800060404     C                   MOVEL     '1'           ERRL00                         *ERRORE
066900060404     C                   MOVEL     '1'           F03L00                         *FINE PGM
067000060404     c                   movel     dp0f03        d00f03
067100060404X   1C                   ELSE                                                   *NON PREMUTO F03
067200060404     C                   MOVEL     Dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
067300060404E   1C                   ENDIF
067400060404     c
067500060404     c                   else
067600970529     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
067700970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
067800970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
067900970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
068000970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
068100970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
068200970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
068300970602     C                   MOVEL     DCAP          D00CAP                         *CAP
068400970602     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
068500970603     C                   MOVEL     *BLANKS       D00SIN                         *ALTRI CAMPI
068600970603     C                   Z-ADD     *ZEROS        D00TTC
068700970530     C                   MOVEL     *BLANKS       D00TSP
068800970530     C                   Z-ADD     *ZEROS        D00TFP
068900970530     C                   MOVEL     *BLANKS       D00EPA
069000970530     C                   Z-ADD     *ZEROS        D00ETP
069100970530     C                   Z-ADD     *ZEROS        D00ETA
069200970530     C                   Z-ADD     *ZEROS        D00EDD
069300970530     C                   MOVEL     *BLANKS       D00SPT
069400970530     C                   Z-ADD     *ZEROS        D00SLP
069500970530     C                   Z-ADD     *ZEROS        D00SLA
069600970530     C                   MOVEL     *BLANKS       D00STS
069700970530     C                   Z-ADD     *ZEROS        D00SDD
069800970526     C*
069900970526     C                   CALL      WPGM
070000970612     C                   PARM                    KPJBA
070100970612     C                   PARM                    DSSI00
070200970526     C*
070300970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
070400970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
070500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
070600970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
070700970526X   1C                   ELSE                                                   *NON PREMUTO F03
070800970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
070900970526E   1C                   ENDIF
071000060404E   1C                   ENDIF
071100970526     C*
071200970526     C                   ENDSR
071300970527     C*------------------------------------------------------------------------*
071400970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
071500970527     C*------------------------------------------------------------------------*
071600970527     C     CARTAB        BEGSR
071700970527     C*
071800970527     C                   ENDSR
071900970526     C*------------------------------------------------------------------------*
072000970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
072100970526     C*------------------------------------------------------------------------*
072200061026     C     CHKFOP        BEGSR
072300061026     C*
072400061026     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
072500061026     C                   SETON                                        93
072600061026     C*
072700061026     C                   IF        §UTECAP = 'C'
072800061026     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
072900061026     C                   SETOFF                                       93
073000061026     C                   ENDIF
073100970626     C*
073200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
073300970626IF  1C     DEPOP0        IFEQ      'L01'
073400970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
073500970626     C                   MOVEL     'S'           OPDATT
073600970626     C                   MOVEL     OPDRIG        OPD(1)
073700970626X   1C                   ELSE
073800970626     C* OPZIONI
073900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
074000970626     C                   MOVEL     'S'           OPDATT
074100970626     C                   MOVEL     OPDRIG        OPD(5)
074200970626     C                   MOVEL     OPD(13)       OPDRIG                         *SINONIMI
074300970626     C                   MOVEL     'S'           OPDATT
074400970626     C                   MOVEL     OPDRIG        OPD(13)
074500060404     C                   MOVEL     OPD(14)       OPDRIG                         *SINONIMI
074600060404     C                   MOVEL     'S'           OPDATT
074700060404     C                   MOVEL     OPDRIG        OPD(14)
074800970626     C* FUNZIONI
074900970626     C                   MOVEL     FUD(13)       FUDRIG                         *SINONIMI
075000970626     C                   MOVEL     'S'           FUDATT
075100970626     C                   MOVEL     FUDRIG        FUD(13)
075200060404     C                   MOVEL     FUD(14)       FUDRIG                         *SINONIMI
075300060404     C                   MOVEL     'S'           FUDATT
075400060404     C                   MOVEL     FUDRIG        FUD(14)
075500970626E   1C                   ENDIF
075600970626     C*
075700970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
075800970626IF  1C     DEPOP0        IFEQ      'L02'
075900970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
076000970626     C                   MOVEL     'S'           OPDATT
076100970626     C                   MOVEL     OPDRIG        OPD(2)
076200970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
076300970627     C                   MOVEL     'S'           OPDATT
076400970627     C                   MOVEL     OPDRIG        OPD(3)
076500061026     C  N93              MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
076600061026     C  N93              MOVEL     'S'           OPDATT
076700061026     C  N93              MOVEL     OPDRIG        OPD(4)
076800061026     C  N93              MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
076900061026     C  N93              MOVEL     'S'           OPDATT
077000061026     C  N93              MOVEL     OPDRIG        OPD(7)
077100060404     C                   MOVEL     OPD(15)       OPDRIG                         *RIPRISTINO
077200060404     C                   MOVEL     'S'           OPDATT
077300060404     C                   MOVEL     OPDRIG        OPD(15)
077400061016     C* CONSENTITO INSERIMENTO SOLO A PERSONALE "EDP"
077500061026     C                   IF        %subst(KNMUS:1:3) = 'EDP' AND *IN93 = *off
077600970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
077700970626     C                   MOVEL     'S'           FUDATT
077800970626     C                   MOVEL     FUDRIG        FUD(6)
077900061016     C                   ENDIF
078000970626E   1C                   ENDIF
078100970626     C*
078200970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
078300060404IF  1C**   DUTLPO        IFNE      'S'                                          *FILIALE
078400060404     C**                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
078500060404     C**                 MOVEL     'N'           OPDATT
078600060404     C**                 MOVEL     OPDRIG        OPD(4)
078700060404     C**                 MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
078800060404     C**                 MOVEL     'N'           OPDATT
078900060404     C**                 MOVEL     OPDRIG        OPD(7)
079000060404     C**                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
079100060404     C**                 MOVEL     'N'           FUDATT
079200060404     C**                 MOVEL     FUDRIG        FUD(6)
079300060404E   1C*+                 ENDIF
079400970526     C*
079500970526     C* GESTIONE OPZIONI A VIDEO
079600970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
079700970526     C                   EXSR      OPZGES
079800970526     C*
079900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
080000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
080100970526     C                   EXSR      FUNGES
080200970526     C*
080300970526     C                   ENDSR
080400020919     C*--------------------------------------------------------------------------------------------*
080500020919     C* REPERISCE I DATI UTENTE
080600020919     C*--------------------------------------------------------------------------------------------*
080700020919     C     REPDATIUTE    BEGSR
080800020919     C*
080900020919     C* INIZIALIZZA VARIABILI DI WRK
081000020919     C                   CLEAR                   TIBS34DS
081100020919     C                   CLEAR                   AZUTEDS
081200020919     C                   CLEAR                   DDATIUTE
081300020919     C*
081400020919     C     *DTAARA       DEFINE    §azute        azuteds
081500020919     C     *DTAARA       DEFINE    §datiute      ddatiute
081600020919     C                   IN(E)     *DTAARA
081700020919if  1C                   IF        %Error
081800020919     c                   EVAL      I34Tla = 'L'
081900020919     C                   CALL      'TIBS34R'
082000020919     C                   PARM                    Tibs34Ds
082100020919     C                   IN        *DTAARA
082200020919e   1C                   ENDIF
082300020919     C*
082400020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
082500020919     C                   MOVEL(P)  RSUT          DSFIRS           20
082600061026     C*
082700061026     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
082800061026     C                   EVAL      DUTE01 = UTEFAF
082900020919     C*
083000020919     C                   ENDSR
083100970526     C*------------------------------------------------------------------------*
083200970526     C* *INZSR - OPERAZIONI INIZIALI
083300970526     C*------------------------------------------------------------------------*
083400970526     C     *INZSR        BEGSR
083500970526     C*---
083600970526     C* RICEVIMENTO PARAMETRI
083700970526     C*---
083800970526     C     *ENTRY        PLIST
083900970612     C                   PARM                    KPJBA
084000970612     C                   PARM                    DSSI00
084100970526     C*
084200970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
084300970526     C     XMSGPA        PLIST
084400970526     C                   PARM                    DSMSMS
084500970526     C                   PARM                    DSMSPG
084600970526     C                   PARM                    DSMSKE
084700970526     C                   PARM                    DSMSER
084800970526     C*
084900970526     C* IMPOSTA LA DS -TASTI PREMUTI-
085000970526     C                   CALL      'XKEY'
085100970526     C                   PARM                    DSKEY
085200970526     C*---
085300970526     C* VARIABILI NON RIFERITE AL DATA BASE
085400970526     C*---
085500970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
085600970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
085700970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
085800970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
085900970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
086000970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
086100970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
086200970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
086300970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
086400970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
086500970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
086600970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
086700970529     C     *LIKE         DEFINE    D00OP1        WOP1
086800970602     C     *LIKE         DEFINE    D00NAR        DNAR
086900970602     C     *LIKE         DEFINE    D00CAP        DCAP
087000970602     C     *LIKE         DEFINE    D00LOC        DLOC
087100970526     C*---
087200970526     C* VARIABILI DI CONTROLLO
087300970526     C*---
087400970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
087500970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
087600970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
087700970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
087800970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
087900970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
088000970526     C*
088100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
088200970529     C                   MOVEL     'TISI30R'     DSMSPG
088300970529     C                   MOVEL     'TISI30R'     MSGQUE
088400970526     C                   MOVEL     *BLANKS       MSGKEY
088500970526     C                   MOVEL     *BLANKS       DSMSMS
088600970526     C                   MOVEL     *BLANKS       DSMSKE
088700970526     C                   Z-ADD     *ZEROS        DSMSER
088800970526     C*---
088900970526     C* VARIABILI RIFERITE AL DATA BASE
089000970526     C*---
089100970529     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
089200970529     C     *LIKE         DEFINE    CPLNAR        KCLNAR
089300970529     C     *LIKE         DEFINE    CPLCAP        KCLCAP
089400970529     C     *LIKE         DEFINE    CPLLOC        KCLLOC
089500970528     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
089600970528     C     *LIKE         DEFINE    CPCNAR        KCCNAR
089700970528     C     *LIKE         DEFINE    CPCCAP        KCCCAP
089800970529     C     *LIKE         DEFINE    CPLNAR        DEPNAR                         *DEPOSITI
089900970529     C     *LIKE         DEFINE    CPLCAP        DEPCAP
090000970529     C     *LIKE         DEFINE    CPLLOC        DEPLOC
090100970526     C*---
090200970526     C* CHIAVI DI LETTURA
090300970526     C*---
090400970529     C* LETTURA AZCPL01L - PARZIALE
090500970529     C     KEYCPL        KLIST
090600970529     C                   KFLD                    KCLVER                         *VERSIONE
090700970529     C                   KFLD                    KCLNAR                         *NAZIONE
090800970529     C                   KFLD                    KCLCAP                         *CAP
090900970529     C* LETTURA AZCPL02L - COMPLETA
091000970529     C     KE2CPL        KLIST
091100970529     C                   KFLD                    KCLVER                         *VERSIONE
091200970605     C                   KFLD                    KCLNAR                         *NAZIONE
091300970529     C                   KFLD                    KCLLOC                         *LOCALITA'
091400970528     C* LETTURA AZCPC01L - COMPLETA
091500970528     C     KEYCPC        KLIST
091600970528     C                   KFLD                    KCCVER                         *VERSIONE
091700970528     C                   KFLD                    KCCNAR                         *NAZIONE
091800970528     C                   KFLD                    KCCCAP                         *CAP
091900970527     C*
092000970527     C* REPERIMENTO DATI UTENTE
092100020919     C                   EXSR      REPDATIUTE
092200970527     C*
092300970527     C* CARICA LE TABELLE OCCORRENTI
092400970527     C                   EXSR      CARTAB
092500970526     C*
092600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
092700970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
092800970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
092900970526     C*
093000970526     C* CALCOLA LA DATA CORRENTE
093100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
093200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
093300970526     C                   Z-ADD     WN8           G08DAT
093400970526     C                   Z-ADD     *ZEROS        G08INV
093500970526     C                   MOVEL     '0'           G08ERR
093600970526     C                   CALL      'XSRDA8'
093700970526     C                   PARM                    WLBDA8
093800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
093900970526     C*
094000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
094100970526     C                   Z-ADD     D00DRI        G08INV
094200970526     C                   Z-ADD     *ZEROS        G08DAT
094300970526     C                   MOVEL     '3'           G08ERR
094400970526     C                   CALL      'XSRDA8'
094500970526     C                   PARM                    WLBDA8
094600970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
094700970527     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
094800970527     C                   MOVEL     D00CAP        VIDPCA                         *CAP
094900970529     C                   MOVEL     D00LOC        VIDPLO                         *LOCALITA
095000970526     C                   EXSR      CHKPAR
095100970526IF  1C     ERRL00        IFEQ      '1'
095200970526     C                   MOVEL     '1'           D00ERR
095300970526     C                   MOVEL     DSMSMS        D00MSG
095400970526X   1C                   ELSE
095500970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
095600970527     C                   MOVEL     VIDPCA        DEPCAP
095700970529     C                   MOVEL     VIDPLO        DEPLOC
095800970526E   1C                   ENDIF
095900970703     C*
096000970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
096100970703IF  1C     D00VER        IFEQ      *ZEROS
096200970707     C                   Z-ADD     D00DRI        P96DRI            8 0
096300970707     C                   Z-ADD     *ZEROS        P96VER            5 0
096400970707     C                   CALL      'TISI96R'
096500970707     C                   PARM                    P96DRI
096600970707     C                   PARM                    P96VER
096700970707     C                   Z-ADD     P96VER        D00VER
096800970703E   1C                   ENDIF
096900970526     C*
097000970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
097100970526     C                   EXSR      CHKFOP
097200970526     C*
097300970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
097400970530IF  1C     DEPOP0        IFEQ      'L01'
097500970530     C                   MOVEL     DEC(3)        VIDING
097600970530E   1C                   ENDIF
097700970530IF  1C     DEPOP0        IFEQ      'L02'
097800970530     C                   MOVEL     DEC(1)        VIDING
097900970530E   1C                   ENDIF
098000970530IF  1C     DEPOP0        IFEQ      'L05'
098100970530     C                   MOVEL     DEC(2)        VIDING
098200970530E   1C                   ENDIF
098300970526     C*
098400970526     C                   ENDSR
098500970526     C*------------------------------------------------------------------------*
098600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
098800970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
098900970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
099000970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
099100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
099300970526     C*------------------------------------------------------------------------*
099400970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
099500970526     C*------------------------------------------------------------------------*
099600970526     C     POSSFL        BEGSR
099700970526     C*
099800970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
099900970526     C                   EXSR      CLRSFL
100000970526     C*
100100970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
100200970526     C                   EXSR      SETFIL
100300970526     C*
100400970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
100500970526     C     SFLEOF        IFEQ      0
100600970526     C                   EXSR      CARSFL
100700970526     C                   END
100800970526     C*
100900970526     C                   ENDSR
101000970526     C*------------------------------------------------------------------------*
101100970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
101200970526     C*------------------------------------------------------------------------*
101300970526     C     ENTATT        BEGSR
101400970526     C*
101500970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
101600970526     C                   Z-ADD     0             OPZOK             1 0
101700970526     C*
101800970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
101900970526     C                   Z-ADD     0             SFLVAR            5 0
102000970526     C*
102100970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
102200970526     C                   Z-ADD     0             SFLERR
102300970526     C*
102400970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
102500970526     C                   Z-ADD     SFLULT        WFLULT
102600970526     C*
102700970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
102800970529     C                   READC     SI30D05                                98
102900970526     C     *IN98         DOWEQ     '0'
103000970526     C*
103100970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
103200970526     C                   ADD       1             SFLVAR
103300970526     C*
103400970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
103500970526     C     OPZOK         IFEQ      0
103600970526     C*
103700970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
103800970526     C     OPZKEY        IFNE      *BLANKS
103900970526     C                   EXSR      OPZCON
104000970526     C                   END
104100970526     C*
104200970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
104300970526     C*            2. PULIZIA SCELTA IN SFL
104400970526     C     OPZOK         IFEQ      0
104500970526     C                   MOVEL     '0'           *IN41
104600970526     C                   MOVEL     *BLANKS       OPZKEY
104700970526     C                   ELSE
104800970526     C*
104900970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
105000970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
105100970526     C                   MOVEL     '1'           *IN41
105200970526     C                   Z-ADD     SFLULT        SFLERR
105300970526     C                   END
105400970526     C                   ELSE
105500970526     C*
105600970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
105700970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
105800970526     C                   MOVEL     '1'           *IN41
105900970526     C                   END                                                    OPZOK=0
106000970526     C*
106100970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
106200970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
106300970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
106400970526     C     OPZOK         IFNE      0
106500970526     C                   Z-ADD     SFLERR        SFLNBR
106600970526     C                   ELSE
106700970526     C                   Z-ADD     SFLULT        SFLNBR
106800970526     C                   END
106900970526     C*
107000970526     C* AGGIORNO RECORD SFL
107100970529     C                   UPDATE    SI30D05
107200970526     C*
107300970529     C                   READC     SI30D05                                98
107400970526     C                   END                                                    DO WHILE
107500970526     C*
107600970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
107700970526     C                   Z-ADD     WFLULT        SFLULT
107800970526     C*
107900970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
108000970526     C     SFLVAR        IFEQ      0
108100970526     C*---                 MOVEL'1'       FINL00
108200970526     C                   END
108300970526     C*
108400970526     C                   ENDSR
108500970526     C*------------------------------------------------------------------------*
108600970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
108700970526     C*------------------------------------------------------------------------*
108800970526     C     CLRSFL        BEGSR
108900970526     C*
109000970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
109100970526     C* (0=NON FINE FILE  1=FINE FILE)
109200970526     C                   Z-ADD     0             SFLEOF
109300970526     C*
109400970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
109500970526     C                   Z-ADD     0             SFLULT
109600970526     C*
109700970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
109800970526     C                   MOVEL     '1'           *IN40
109900970529     C                   WRITE     SI30D06
110000970526     C                   MOVEL     '0'           *IN40
110100970526     C*
110200970526     C                   ENDSR
110300970526     C*------------------------------------------------------------------------*
110400970526     C* CARSFL - CARICAMENTO SFL
110500970526     C*------------------------------------------------------------------------*
110600970526     C     CARSFL        BEGSR
110700970526     C*
110800970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
110900970526     C     SFLULT        IFEQ      0
111000970526     C                   EXSR      SETFIL
111100970526     C                   END
111200970526     C*
111300970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
111400970526     C                   Z-ADD     SFLULT        WFLULT
111500970526     C*
111600970526     C* CICLO:
111700970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
111800970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
111900970526     C                   Z-ADD     1             SFLRIG
112000970526     C     SFLRIG        DOWLE     SFLPAG
112100970526     C     SFLEOF        ANDEQ     0
112200970526     C*
112300970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
112400970526     C                   MOVEL     *BLANKS       OPZKEY
112500970526     C*
112600970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
112700970526     C                   EXSR      IRESFL
112800970526     C*
112900970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
113000970526     C                   ADD       1             SFLULT
113100970526     C*
113200970526     C* SCRITTURA RECORD SFL
113300970529     C                   WRITE     SI30D05
113400970526     C                   ADD       1             SFLRIG
113500970526     C*
113600970526     C* LETTURA FILE DA CARICARE
113700970526     C                   EXSR      LETFIL
113800970526     C                   END                                                    DO WHILE
113900970526     C*
114000970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
114100970526     C     WFLULT        IFNE      SFLULT
114200970526     C                   Z-ADD     WFLULT        SFLNBR
114300970526     C                   ADD       1             SFLNBR
114400970526     C                   END
114500970526     C*
114600970526     C                   ENDSR
114700970526     C*------------------------------------------------------------------------*
114800970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
114900970526     C*------------------------------------------------------------------------*
115000970526     C     OUTSFL        BEGSR
115100970526     C*
115200970526     C* WRITE RECORD TESTATA VIDEO
115300970529     C                   WRITE     SI30D01
115400970526     C*
115500970526     C* VISUALIZZAZIONE MESSAGGIO
115600970526     C                   EXSR      SNDMSG
115700970526     C*
115800970526     C* WRITE RECORD OPZIONI VIDEO
115900970529     C                   WRITE     SI30D02
116000970526     C*
116100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
116200970529     C                   WRITE     SI30D03
116300970526     C*
116400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
116500970529     C                   WRITE     SI30D04
116600970526     C*
116700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
116800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
116900970526     C     SFLEOF        IFEQ      0
117000970526     C                   MOVEL     '0'           *IN42
117100970526     C                   ELSE
117200970526     C*
117300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
117400970526     C                   MOVEL     '1'           *IN42
117500970526     C                   END
117600970526     C*
117700970526     C* WRITE RECORD TASTI FUNZIONALI
117800970529     C                   WRITE     SI30D09
117900970526     C*
118000970526     C* CASO DI SFL VUOTO
118100970526     C     SFLULT        IFLE      0
118200970526     C*
118300970526     C*    - WRITE RECORD VUOTO
118400970529     C                   WRITE     SI30D0A
118500970529     C                   READ      SI30D0A                                99
118600970526     C*
118700970526     C                   ELSE
118800970526     C*
118900970526     C* CASO DI SFL CON RECORDS
119000970526     C*
119100970526     C*    - WRITE RECORD CONTROLLO SFL
119200970529     C                   WRITE     SI30D06
119300970529     C                   READ      SI30D06                                99
119400970526     C                   END
119500970526     C*
119600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
119700970529     C                   READ      SI30D03                                99
119800970526     C*
119900970526     C                   ENDSR
120000970526     C*------------------------------------------------------------------------*
120100970526     C* SNDMSG - MANDA MESSAGGIO
120200970526     C*------------------------------------------------------------------------*
120300970526     C     SNDMSG        BEGSR
120400970526     C*
120500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
120600970526     C                   MOVEL     '1'           *IN40
120700970529     C                   WRITE     SI30DMC
120800970526     C                   MOVEL     '0'           *IN40
120900970526     C*
121000970526     C* VISUALIZZAZIONE MESSAGGIO
121100970526     C                   Z-ADD     0             MSGULT
121200970526     C                   CALL      'XMSG'        XMSGPA
121300970526     C     DSMSER        IFEQ      0
121400970526     C                   MOVEL     DSMSKE        MSGKEY
121500970526     C                   END
121600970526     C                   Z-ADD     1             MSGULT
121700970529     C                   WRITE     SI30DMS                                      SFL MESS
121800970529     C                   WRITE     SI30DMC                                      CONTR MESS
121900970526     C*
122000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
122100970526     C                   MOVEL     *BLANKS       DSMSMS
122200970526     C*
122300970526     C                   ENDSR
122400970526     C*------------------------------------------------------------------------*
122500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
122600970526     C*------------------------------------------------------------------------*
122700970526     C     FUNCON        BEGSR
122800970526     C*
122900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
123000970526     C                   EXSR      FUNVAR
123100970526     C*
123200970526     C* PULISCO CAMPI DI LAVORO
123300970526     C                   Z-ADD     0             I                 5 0
123400970526     C                   Z-ADD     0             W                 5 0
123500970526     C*
123600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
123700970526     C     FUNKEY        IFNE      *BLANKS
123800970526     C     1             DO        24            I
123900970526     C                   MOVEL     FUD(I)        FUDRIG
124000970526     C*
124100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
124200970526     C     FUNKEY        IFEQ      FUDFUN
124300970526     C     FUDATT        ANDEQ     'S'
124400970526     C                   Z-ADD     I             W
124500970526     C                   END
124600970526     C                   END                                                    DO WHILE
124700970526     C*
124800970526     C* FUNZIONE NON ATTIVA (ERRORE)
124900970526     C     W             IFEQ      0
125000970526     C*
125100970526     C* EMISSIONE MESSAGGIO DI ERRORE
125200970526     C                   MOVEL     MSG(2)        DSMSMS
125300970526     C                   END                                                    W=0
125400970526     C                   END                                                    FUNKEY NE *BLANKS
125500970526     C*
125600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
125700970526     C     W             IFNE      0
125800970526     C     FUNKEY        OREQ      *BLANKS
125900970526     C*
126000970526     C     FUNKEY        IFEQ      *BLANKS
126100970526     C                   EXSR      ENTGES
126200970526     C                   END
126300970526     C     FUNKEY        IFEQ      '03'
126400970526     C                   EXSR      F03GES
126500970526     C                   END
126600970526     C     FUNKEY        IFEQ      '05'
126700970526     C                   EXSR      F05GES
126800970526     C                   END
126900970526     C     FUNKEY        IFEQ      '06'
127000970526     C                   EXSR      F06GES
127100970526     C                   END
127200970526     C     FUNKEY        IFEQ      '12'
127300970526     C                   EXSR      F12GES
127400970526     C                   END
127500970529     C     FUNKEY        IFEQ      '13'
127600970529     C                   EXSR      F13GES
127700970529     C                   END
127800060404     C     FUNKEY        IFEQ      '14'
127900060404     C                   EXSR      F14GES
128000060404     C                   END
128100970526     C     FUNKEY        IFEQ      '23'
128200970526     C                   EXSR      F23GES
128300970526     C                   END
128400970526     C     FUNKEY        IFEQ      '24'
128500970526     C                   EXSR      F24GES
128600970526     C                   END
128700970526     C     FUNKEY        IFEQ      'RU'
128800970526     C                   EXSR      RUPGES
128900970526     C                   END
129000970526     C*
129100970526     C                   END
129200970526     C*
129300970526     C                   ENDSR
129400970526     C*------------------------------------------------------------------------*
129500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
129600970526     C*------------------------------------------------------------------------*
129700970526     C     FUNVAR        BEGSR
129800970526     C*
129900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
130000970526     C     KEY           IFEQ      ENTER
130100970526     C                   MOVEL     *BLANKS       FUNKEY
130200970526     C                   END
130300970526     C     KEY           IFEQ      F1
130400970526     C                   MOVEL     '01'          FUNKEY
130500970526     C                   END
130600970526     C     KEY           IFEQ      F2
130700970526     C                   MOVEL     '02'          FUNKEY
130800970526     C                   END
130900970526     C     KEY           IFEQ      F3
131000970526     C                   MOVEL     '03'          FUNKEY
131100970526     C                   END
131200970526     C     KEY           IFEQ      F4
131300970526     C                   MOVEL     '04'          FUNKEY
131400970526     C                   END
131500970526     C     KEY           IFEQ      F5
131600970526     C                   MOVEL     '05'          FUNKEY
131700970526     C                   END
131800970526     C     KEY           IFEQ      F6
131900970526     C                   MOVEL     '06'          FUNKEY
132000970526     C                   END
132100970526     C     KEY           IFEQ      F7
132200970526     C                   MOVEL     '07'          FUNKEY
132300970526     C                   END
132400970526     C     KEY           IFEQ      F8
132500970526     C                   MOVEL     '08'          FUNKEY
132600970526     C                   END
132700970526     C     KEY           IFEQ      F9
132800970526     C                   MOVEL     '09'          FUNKEY
132900970526     C                   END
133000970526     C     KEY           IFEQ      F10
133100970526     C                   MOVEL     '10'          FUNKEY
133200970526     C                   END
133300970526     C     KEY           IFEQ      F11
133400970526     C                   MOVEL     '11'          FUNKEY
133500970526     C                   END
133600970526     C     KEY           IFEQ      F12
133700970526     C                   MOVEL     '12'          FUNKEY
133800970526     C                   END
133900970526     C     KEY           IFEQ      F13
134000970526     C                   MOVEL     '13'          FUNKEY
134100970526     C                   END
134200970526     C     KEY           IFEQ      F14
134300970526     C                   MOVEL     '14'          FUNKEY
134400970526     C                   END
134500970526     C     KEY           IFEQ      F15
134600970526     C                   MOVEL     '15'          FUNKEY
134700970526     C                   END
134800970526     C     KEY           IFEQ      F16
134900970526     C                   MOVEL     '16'          FUNKEY
135000970526     C                   END
135100970526     C     KEY           IFEQ      F17
135200970526     C                   MOVEL     '17'          FUNKEY
135300970526     C                   END
135400970526     C     KEY           IFEQ      F18
135500970526     C                   MOVEL     '18'          FUNKEY
135600970526     C                   END
135700970526     C     KEY           IFEQ      F19
135800970526     C                   MOVEL     '19'          FUNKEY
135900970526     C                   END
136000970526     C     KEY           IFEQ      F20
136100970526     C                   MOVEL     '20'          FUNKEY
136200970526     C                   END
136300970526     C     KEY           IFEQ      F21
136400970526     C                   MOVEL     '21'          FUNKEY
136500970526     C                   END
136600970526     C     KEY           IFEQ      F22
136700970526     C                   MOVEL     '22'          FUNKEY
136800970526     C                   END
136900970526     C     KEY           IFEQ      F23
137000970526     C                   MOVEL     '23'          FUNKEY
137100970526     C                   END
137200970526     C     KEY           IFEQ      F24
137300970526     C                   MOVEL     '24'          FUNKEY
137400970526     C                   END
137500970526     C     KEY           IFEQ      ROLLUP
137600970526     C                   MOVEL     'RU'          FUNKEY
137700970526     C                   END
137800970526     C     KEY           IFEQ      HELP
137900970526     C                   MOVEL     'HP'          FUNKEY
138000970526     C                   END
138100970526     C*
138200970526     C                   ENDSR
138300970526     C*------------------------------------------------------------------------*
138400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
138500970526     C*------------------------------------------------------------------------*
138600970526     C     FUNGES        BEGSR
138700970526     C*
138800970526     C* PULISCO CAMPI DI LAVORO
138900970526     C                   Z-ADD     0             W                 5 0
139000970526     C*
139100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
139200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
139300970526     C     1             DO        24            I
139400970526     C                   MOVEL     FUD(I)        FUDRIG
139500970526     C*
139600970526     C* PULIZIA FUNZIONI PRECEDENTI
139700970526     C                   MOVEL     *BLANKS       FUV(I)
139800970526     C*
139900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
140000970526     C* (SALTA F23 E F24)
140100970526     C     FUDATT        IFEQ      'S'
140200970526     C     FUDFUN        ANDNE     '23'
140300970526     C     FUDFUN        ANDNE     '24'
140400970526     C                   ADD       1             W
140500970526     C                   MOVEL     FUDDES        FUV(W)
140600970526     C                   END
140700970526     C                   END                                                    DO WHILE
140800970526     C*
140900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
141000970526     C* ALTRIMENTI DISATTIVO F24
141100970526     C     1             DO        24            I
141200970526     C                   MOVEL     FUD(I)        FUDRIG
141300970526     C*
141400970526     C     FUDFUN        IFEQ      '24'
141500970526     C     W             IFGT      6
141600970526     C                   MOVEL     'S'           FUDATT
141700970526     C                   ELSE
141800970526     C                   MOVEL     *BLANKS       FUDATT
141900970526     C                   END                                                    W GT 8
142000970526     C                   MOVEL     FUDRIG        FUD(I)
142100970526     C                   END
142200970526     C                   END                                                    DO WHILE
142300970526     C*
142400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
142500970526     C     W             DIV       6             I
142600970526     C                   MVR                     Z
142700970526     C*
142800970526     C     Z             IFNE      0
142900970526     C     I             ADD       1             Z
143000970526     C                   ELSE
143100970526     C                   Z-ADD     I             Z
143200970526     C                   END
143300970526     C*
143400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
143500970526     C                   ADD       1             FUNGRU
143600970526     C     FUNGRU        IFGT      Z
143700970526     C     FUNGRU        ORLT      1
143800970526     C                   Z-ADD     1             FUNGRU
143900970526     C                   END
144000970526     C*
144100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
144200970526     C     FUNGRU        IFEQ      1
144300970526     C                   MOVEL     FUV(1)        FUN001
144400970526     C                   MOVEL     FUV(2)        FUN002
144500970526     C                   MOVEL     FUV(3)        FUN003
144600970526     C                   MOVEL     FUV(4)        FUN004
144700970526     C                   MOVEL     FUV(5)        FUN005
144800970526     C                   MOVEL     FUV(6)        FUN006
144900970526     C                   END
145000970526     C*
145100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
145200970526     C     FUNGRU        IFEQ      2
145300970526     C                   MOVEL     FUV(7)        FUN001
145400970526     C                   MOVEL     FUV(8)        FUN002
145500970526     C                   MOVEL     FUV(9)        FUN003
145600970526     C                   MOVEL     FUV(10)       FUN004
145700970526     C                   MOVEL     FUV(11)       FUN005
145800970526     C                   MOVEL     FUV(12)       FUN006
145900970526     C                   END
146000970526     C*
146100970526     C* TERZO GRUPPO DI 6 FUNZIONI
146200970526     C     FUNGRU        IFEQ      3
146300970526     C                   MOVEL     FUV(13)       FUN001
146400970526     C                   MOVEL     FUV(14)       FUN002
146500970526     C                   MOVEL     FUV(15)       FUN003
146600970526     C                   MOVEL     FUV(16)       FUN004
146700970526     C                   MOVEL     FUV(17)       FUN005
146800970526     C                   MOVEL     FUV(18)       FUN006
146900970526     C                   END
147000970526     C*
147100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
147200970526     C     FUNGRU        IFEQ      4
147300970526     C                   MOVEL     FUV(19)       FUN001
147400970526     C                   MOVEL     FUV(20)       FUN002
147500970526     C                   MOVEL     FUV(21)       FUN003
147600970526     C                   MOVEL     FUV(22)       FUN004
147700970526     C                   MOVEL     *BLANKS       FUN005
147800970526     C                   MOVEL     *BLANKS       FUN006
147900970526     C                   END
148000970526     C*
148100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
148200970526     C                   MOVEL     *BLANKS       FUN007
148300970526     C                   MOVEL     *BLANKS       FUN008
148400970526     C*
148500970526     C     1             DO        24            I
148600970526     C                   MOVEL     FUD(I)        FUDRIG
148700970526     C     FUDFUN        IFEQ      '23'
148800970526     C     FUDATT        ANDEQ     'S'
148900970526     C                   MOVEL     FUDDES        FUN007
149000970526     C                   END
149100970526     C     FUDFUN        IFEQ      '24'
149200970526     C     FUDATT        ANDEQ     'S'
149300970526     C                   MOVEL     FUDDES        FUN008
149400970526     C                   END
149500970526     C                   END                                                    DO
149600970526     C*
149700970526     C                   ENDSR
149800970526     C*------------------------------------------------------------------------*
149900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
150000970526     C*------------------------------------------------------------------------*
150100970526     C     OPZGES        BEGSR
150200970526     C*
150300970526     C* PULISCO CAMPI DI LAVORO
150400970526     C                   Z-ADD     0             W                 5 0
150500970526     C*
150600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
150700970526     C* E CARICO TABELLA OPZIONI ATTIVE
150800970526     C     1             DO        24            I
150900970526     C                   MOVEL     OPD(I)        OPDRIG
151000970526     C*
151100970526     C* PULIZIA OPZIONI PRECEDENTI
151200970526     C                   MOVEL     *BLANKS       OPV(I)
151300970526     C*
151400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
151500970526     C     OPDATT        IFEQ      'S'
151600970526     C                   ADD       1             W
151700970526     C                   MOVEL     OPDDES        OPV(W)
151800970526     C                   END
151900970526     C                   END                                                    DO WHILE
152000970526     C*
152100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
152200970526     C* ALTRIMENTI DISATTIVO F23
152300970526     C     1             DO        24            I
152400970526     C                   MOVEL     FUD(I)        FUDRIG
152500970526     C*
152600970526     C     FUDFUN        IFEQ      '23'
152700970526     C     W             IFGT      8
152800970526     C                   MOVEL     'S'           FUDATT
152900970526     C                   ELSE
153000970526     C                   MOVEL     *BLANKS       FUDATT
153100970526     C                   END                                                    W GT 8
153200970526     C                   MOVEL     FUDRIG        FUD(I)
153300970526     C                   END
153400970526     C                   END                                                    DO WHILE
153500970526     C*
153600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
153700970526     C     W             DIV       8             I
153800970526     C                   MVR                     Z                 5 0
153900970526     C*
154000970526     C     Z             IFNE      0
154100970526     C     I             ADD       1             Z
154200970526     C                   ELSE
154300970526     C                   Z-ADD     I             Z
154400970526     C                   END
154500970526     C*
154600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
154700970526     C                   ADD       1             OPZGRU
154800970526     C     OPZGRU        IFGT      Z
154900970526     C     OPZGRU        ORLT      1
155000970526     C                   Z-ADD     1             OPZGRU
155100970526     C                   END
155200970526     C*
155300970526     C* PRIMO GRUPPO DI 8 OPZIONI
155400970526     C     OPZGRU        IFEQ      1
155500970526     C                   MOVEL     OPV(1)        OPZ001
155600970526     C                   MOVEL     OPV(2)        OPZ002
155700970526     C                   MOVEL     OPV(3)        OPZ003
155800970526     C                   MOVEL     OPV(4)        OPZ004
155900970526     C                   MOVEL     OPV(5)        OPZ005
156000970526     C                   MOVEL     OPV(6)        OPZ006
156100970526     C                   MOVEL     OPV(7)        OPZ007
156200970526     C                   MOVEL     OPV(8)        OPZ008
156300970526     C                   END
156400970526     C*
156500970526     C* SECONDO GRUPPO DI 8 OPZIONI
156600970526     C     OPZGRU        IFEQ      2
156700970526     C                   MOVEL     OPV(9)        OPZ001
156800970526     C                   MOVEL     OPV(10)       OPZ002
156900970526     C                   MOVEL     OPV(11)       OPZ003
157000970526     C                   MOVEL     OPV(12)       OPZ004
157100970526     C                   MOVEL     OPV(13)       OPZ005
157200970526     C                   MOVEL     OPV(14)       OPZ006
157300970526     C                   MOVEL     OPV(15)       OPZ007
157400970526     C                   MOVEL     OPV(16)       OPZ008
157500970526     C                   END
157600970526     C*
157700970526     C* TERZO GRUPPO DI 8 OPZIONI
157800970526     C     OPZGRU        IFEQ      3
157900970526     C                   MOVEL     OPV(17)       OPZ001
158000970526     C                   MOVEL     OPV(18)       OPZ002
158100970526     C                   MOVEL     OPV(19)       OPZ003
158200970526     C                   MOVEL     OPV(20)       OPZ004
158300970526     C                   MOVEL     OPV(21)       OPZ005
158400970526     C                   MOVEL     OPV(22)       OPZ006
158500970526     C                   MOVEL     OPV(23)       OPZ007
158600970526     C                   MOVEL     OPV(24)       OPZ008
158700970526     C                   END
158800970526     C*
158900970526     C                   ENDSR
159000970526     C*------------------------------------------------------------------------*
159100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
159200970526     C*------------------------------------------------------------------------*
159300970526     C     OPZCON        BEGSR
159400970526     C*
159500970526     C* PULISCO CAMPI DI LAVORO
159600970526     C                   Z-ADD     0             I                 5 0
159700970526     C                   Z-ADD     0             W                 5 0
159800970526     C*
159900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
160000970526     C     1             DO        24            I
160100970526     C                   MOVEL     OPD(I)        OPDRIG
160200970526     C*
160300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
160400970526     C     OPZKEY        IFEQ      OPDOPZ
160500970526     C     OPZKEY        OREQ      OPDOP1
160600970526     C*
160700970526     C     OPDATT        IFEQ      'S'
160800970526     C                   MOVEL     OPDOPZ        OPZKEY
160900970526     C                   Z-ADD     I             W
161000970526     C                   END
161100970526     C                   END
161200970526     C*
161300970526     C                   END                                                    DO WHILE
161400970526     C*
161500970526     C* OPZIONE NON ATTIVA (ERRORE)
161600970526     C     W             IFEQ      0
161700970526     C*
161800970526     C* OPZIONE NON OK
161900970526     C                   Z-ADD     1             OPZOK
162000970526     C*
162100970526     C* EMISSIONE MESSAGGIO DI ERRORE
162200970526     C                   MOVEL     MSG(1)        DSMSMS
162300970526     C                   ELSE
162400970526     C*
162500970526     C* OPZIONE ATTIVA (PROCEDI ...)
162600970526     C*
162700970526     C     OPZKEY        IFEQ      ' 1'
162800970526     C                   EXSR      O01GES
162900970526     C                   END
163000970526     C     OPZKEY        IFEQ      ' 2'
163100970526     C                   EXSR      O02GES
163200970526     C                   END
163300970611     C     OPZKEY        IFEQ      ' 3'
163400970611     C                   EXSR      O03GES
163500970611     C                   END
163600970526     C     OPZKEY        IFEQ      ' 4'
163700970526     C                   EXSR      O04GES
163800970526     C                   END
163900970526     C     OPZKEY        IFEQ      ' 5'
164000970526     C                   EXSR      O05GES
164100970526     C                   END
164200970526     C     OPZKEY        IFEQ      ' 7'
164300970526     C                   EXSR      O07GES
164400970526     C                   END
164500970529     C     OPZKEY        IFEQ      '13'
164600970529     C                   EXSR      O13GES
164700970529     C                   END
164800060404     C     OPZKEY        IFEQ      '14'
164900060404     C                   EXSR      O14GES
165000060404     C                   END
165100060404     C     OPZKEY        IFEQ      '15'
165200060404     C                   EXSR      O15GES
165300060404     C                   END
165400970526     C                   END                                                    OPZ.NON ATTIVA
165500970526     C*
165600970526     C                   ENDSR
165700970526     C*------------------------------------------------------------------------*
165800970526     C* NOTE COSTRUZIONE SCHIERE
165900970526     C*------------------------------------------------------------------------*
166000970526     C*
166100970526     C* -MSG-MESSAGGI
166200970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
166300970526     C* (NON TOCCARE MSG,1 E MSG,2)
166400970526     C*
166500970526     C* -CMD-COMANDI
166600970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
166700970526     C*
166800970526     C* -OPD-OPZIONI
166900970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
167000970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
167100970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
167200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
167300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
167400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
167500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
167600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
167700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
167800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
167900970526     C*
168000970526     C* -FUD-FUNZIONI
168100970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
168200970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
168300970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
168400970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
168500970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
168600970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
168700970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
168800970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
168900970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
169000970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
169100970526     O*------------------------------------------------------------------------*
169200970526** DEC - DECODIFICHE
169300970526    GESTIONE                                                     1
169400970530INTERROGAZIONE                                                   2
169500970530     SCELTA                                                      3
1696009705264
1697009705265
169800970526** MSG - MESSAGGI
169900970529TISI30R- Opzione non gestita                                                   1
170000970529TISI30R- Tasto funzionale non gestito                                          2
170100970704TISI30R- 3
170200970529TISI30R- Record annullato. Consentito solo il ripristino                       4
170300970529TISI30R- Record non annullato. Impossibile ripristinare                        5
170400970529TISI30R- Accettato solo un parametro di inizio lista                           6
170500970529TISI30R- 7                                                                     7
170600970529TISI30R- 8                                                                     8
170700970529TISI30R- 9                                                                     9
170800970529TISI30R- 10                                                                   10
170900970529TISI30R- 11                                                                   11
171000970529TISI30R- 12                                                                   12
171100970529TISI30R- 13                                                                   13
171200970529TISI30R- 14                                                                   14
171300970529TISI30R- 15                                                                   15
171400970529TISI30R- 16                                                                   16
171500970529TISI30R- 17                                                                   17
171600970529TISI30R- 18                                                                   18
171700970529TISI30R- 19                                                                   19
171800970529TISI30R- 20                                                                   20
171900970526** OPD - OPZIONI
172000970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
172100970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
172200970611 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
172300970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
172400970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
172500970526                   -6 -     (S/N)=N                        opz. 06¬
172600970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
172700970527                   -8 -     (S/N)=N                        opz. 08¬
172800970527                   -9 -     (S/N)=N                        opz. 09¬
172900970527                   -10-     (S/N)=N                        opz. 10¬
173000970529                   -11-     (S/N)=N                        opz. 11¬
173100970529                   -12-     (S/N)=N                        opz. 12¬
17320097062613=Sinonimi        -13-     (S/N)=N                        opz. 13¬
17330006040314=Cal.Eventi Part -14-     (S/N)=N                        opz. 14¬
17340006040315=InserisciEvePart-15-     (S/N)=N                        opz. 15¬
173500970526                   -  -     (S/N)=N                        opz. 16¬
173600970526                   -  -     (S/N)=N                        opz. 17¬
173700970526                   -  -     (S/N)=N                        opz. 18¬
173800970526                   -  -     (S/N)=N                        opz. 19¬
173900970526                   -  -     (S/N)=N                        opz. 20¬
174000970526                   -  -     (S/N)=N                        opz. 21¬
174100970526                   -  -     (S/N)=N                        opz. 22¬
174200970526                   -  -     (S/N)=N                        opz. 23¬
174300970526                   -  -     (S/N)=N                        opz. 24¬
174400970526** FUD - FUNZIONI
174500970526                   -        (S/N)=N                        funz.01¬
174600970526                   -        (S/N)=N                        funz.02¬
174700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
174800970526                   -        (S/N)=N                        funz.04¬
174900970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
175000970526F06=Inserimento    -        (S/N)=N                        funz.06¬
175100970526                   -        (S/N)=N                        funz.07¬
175200970527                   -        (S/N)=N                        funz.08¬
175300970527                   -        (S/N)=N                        funz.09¬
175400970527                   -        (S/N)=N                        funz.10¬
175500970526                   -        (S/N)=N                        funz.11¬
175600970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
175700970626F13=Sinonimi       -        (S/N)=N                        funz.13¬
175800060403F14=Cal.Eventi Part-        (S/N)=N                        funz.14¬
175900970526                   -        (S/N)=N                        funz.15¬
176000970526                   -        (S/N)=N                        funz.16¬
176100970526                   -        (S/N)=N                        funz.17¬
176200970526                   -        (S/N)=N                        funz.18¬
176300970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
176400970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
176500970527                   -        (S/N)=N                        funz.21¬
176600970527                   -        (S/N)=N                        funz.22¬
176700970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
176800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
