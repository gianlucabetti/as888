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
047000120515     C     VIDPPR        ORNE      DEPPPR
047100970529     C     VIDPLO        ORNE      DEPLOC
047200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
047300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
047400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
047500970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
047600970527     C                   MOVEL     VIDPCA        DEPCAP
047700120515     C                   MOVEL     VIDPPR        DEPPPR
047800970529     C                   MOVEL     VIDPLO        DEPLOC
047900970526E   2C                   ENDIF
048000970526E   1C                   ENDIF
048100970526     C*
048200970526     C                   ENDSR
048300970526     C*------------------------------------------------------------------------*
048400970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
048500970526     C*------------------------------------------------------------------------*
048600970526     C     SETFIL        BEGSR
048700970529     C*
048800970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
048900970529IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
049000970529     C                   Z-ADD     D00VER        KCLVER
049100970529     C                   MOVEL     VIDPNA        KCLNAR
049200970529     C                   MOVEL     VIDPCA        KCLCAP
049300970529     C     KEYCPL        SETLL     AZCPL01L                           99
049400970529E   1C                   ENDIF
049500970529IF  1C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
049600970529     C                   Z-ADD     D00VER        KCLVER
049700970605     C                   MOVEL     VIDPNA        KCLNAR
049800970529     C                   MOVEL     VIDPLO        KCLLOC
049900970529     C     KE2CPL        SETLL     AZCPL02L                           99
050000970529E   1C                   ENDIF
050100970526     C* LETTURA RECORD
050200970526IF  1C     *IN99         IFEQ      *OFF
050300970526     C                   EXSR      LETFIL
050400970526E   1C                   ENDIF
050500970526     C*
050600970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
050700970526IF  1C     *IN99         IFEQ      *ON
050800970526     C                   Z-ADD     1             SFLEOF
050900970526E   1C                   ENDIF
051000970526     C*
051100970526     C                   ENDSR
051200970526     C*------------------------------------------------------------------------*
051300970526     C* LETFIL - LETTURA RECORD
051400970526     C*------------------------------------------------------------------------*
051500970526     C     LETFIL        BEGSR
051600970526     C*
051700970526     C* LEGGE FINO A:
051800970526     C*     - FINE FILE
051900970526     C*     - TROVATO RECORD VALIDO
052000970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
052100970526DO  1C     *IN99         DOWEQ     *OFF
052200970526     C     WRECOK        ANDEQ     'N'
052300970529     C*
052400970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
052500970529IF  2C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
052600120515     C* CAP è un parametro di selezione, per cui si devono leggere solo i rcd con quei CAP
052700120515     C* (CAP è in chiave)
052800970603     C     KCLVER        READE     AZCPL01L                               99
052900970529E   2C                   ENDIF
053000970529IF  2C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
053100120515     C* Località è un parametro di inizio, per cui si devono leggere i rcd a partire da quella
053200120515     C* Località (Località NON è in chiave)
053300970603     C     KCLVER        READE     AZCPL02L                               99
053400970529E   2C                   ENDIF
053500970526     C*
053600970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
053700970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
053800970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
053900970530X   2C                   ELSE
054000970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
054100970530E   2C                   ENDIF
054200970526E   1C                   ENDDO
054300970526     C*
054400970526     C                   ENDSR
054500970526     C*------------------------------------------------------------------------*
054600970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
054700970526     C*------------------------------------------------------------------------*
054800970526     C     CHKREC        BEGSR
054900970526     C*
055000970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
055100970530     C*
055200970530     C* SE RICHIESTO ORDINAMENTO PER CAP, ESCLUDE LE LOCALITA' NON DEL CAP
055300970530IF  1C     VIDPNA        IFNE      *BLANKS
055400970530     C     VIDPCA        ORNE      *BLANKS
055500970530IF  2C     VIDPNA        IFNE      CPLNAR
055600970530     C     VIDPCA        ORNE      CPLCAP
055700970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
055800970530E   2C                   ENDIF
055900970530E   1C                   ENDIF
056000001005     C*
056100001005     C* LEGGE IL CAP DELLA LOCALIÀ'
056200001005IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
056300001005     C                   Z-ADD     CPLVER        KCCVER
056400001005     C                   MOVEL     CPLNAR        KCCNAR
056500001005     C                   MOVEL     CPLCAP        KCCCAP
056600001005     C     KEYCPC        CHAIN     AZCPC01L                           98
056700001005IF  2C     *IN98         IFEQ      *ON                                          *NON TROVATO
056800001005     C                   CLEAR                   AZCPC000                       *AZZERA CAMPI CAP
056900001005E   2C                   ENDIF
057000001005E   1C                   ENDIF
057100001005     C*
057200120515     C* se è stata scelta la Selezione per Provincia
057300120515IF  1C                   IF        WRecOK = 'S' and                             *RECORD VALIDO
057400120515     C                             VIDPPR <> *blank
057500120515     C* controllo che la provincia della località sia quella da selezionare
057600120515     C                             and VIDPPR <> CPCPrv
057700120515     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
057800120515E   2C                   ENDIF
057900120515     C*
058000120515     C* SE L'UTENTE E' IN SCELTA, ESCLUDE LE LOCALITÀ OBSOLETE
058100001005IF  1C     DEPOP0        IFEQ      'L01'
058200061010IF  2C***  CPLLIV        IFEQ      'L'
058300061010     C***  CPLFI1        ANDNE     *BLANKS
058400061010     C***  CPLLIV        OREQ      'C'
058500061010     C***  CPCFI1        ANDNE     *BLANKS
058600061010     c* escludere sempre a prescindere dal livello gestione
058700061010     c     cplfi1        ifne      *blanks
058800001005     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
058900001005E   2C                   ENDIF
059000001005E   1C                   ENDIF
059100001005     C*
059200970526     C                   ENDSR
059300970526     C*------------------------------------------------------------------------*
059400970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
059500970526     C*------------------------------------------------------------------------*
059600970526     C     IRESFL        BEGSR
059700970529     C*
059800970529     C                   MOVEL     *BLANKS       OPZKEY
059900970925     C     CPLATB        IFEQ      *BLANKS
060000970925     C                   MOVEL     CPLFI1        VIDATB
060100970925     C                   ELSE
060200970529     C                   MOVEL     CPLATB        VIDATB
060300970925     C                   ENDIF
060400970529     C                   MOVEL     CPLNAR        VIDNAR
060500970529     C                   MOVEL     CPLCAP        VIDCAP
060600970529     C                   MOVEL     CPLLOC        VIDLOC
060700970530     C                   MOVEL     CPLLIV        VIDLIV
060800970529     C                   Z-ADD     CPLTTC        VIDTTC
060900970529     C                   MOVEL     CPLISO        VIDISO
061000970529     C                   Z-ADD     CPLLNA        VIDLNA
061100970529     C                   Z-ADD     CPLZNC        VIDZNC
061200970529     C                   MOVEL     CPLDLV        VIDDLV
061300970529     C                   MOVEL     *BLANKS       VIDESS
061400970529IF  1C     CPLESS        IFEQ      'S'
061500970529     C                   MOVEL     'S'           VIDESS                         *ESISTONO SINONIMI
061600970529E   1C                   ENDIF
061700001005     C* DATI DAL CAP
061800970530     C                   MOVEL     CPCPRV        VIDCPR
061900970530     C                   Z-ADD     CPCLNA        VIHCLA
062000001005IF  1C     CPLLIV        IFEQ      'C'                                          *LIV.GESTIONE 'CAP'
062100970529     C                   Z-ADD     CPCTTC        VIDTTC
062200970529     C                   MOVEL     CPCISO        VIDISO
062300970529     C                   Z-ADD     CPCLNA        VIDLNA
062400970529     C                   Z-ADD     CPCZNC        VIDZNC
062500001005E   1C                   ENDIF
062600970526     C*
062700970526     C                   ENDSR
062800970526     C*------------------------------------------------------------------------*
062900970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
063000970526     C*------------------------------------------------------------------------*
063100970526     C     CHKPAR        BEGSR
063200970529     C*
063300970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
063400970529IF  1C     VIDPCA        IFEQ      *BLANKS
063500970529     C     VIDPLO        ANDEQ     *BLANKS
063600970529     C                   MOVEL     'CAP'         WTIPRI            3
063700970529E   1C                   ENDIF
063800970529     C*
063900970529     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
064000970529IF  1C     ERRL00        IFEQ      '0'
064100120515     C* avendo fatto diventare in data 15/05/12 il CAP un dato non di "Inizio da" ma "Selezione per"
064200120515     C* ed essendo rimasto solo la Località come "Inizio da",
064300120515     C* questo test non ha più senso.
064400120515IF  2C***  VIDPCA        IFNE      *BLANKS
064500120515     C***  VIDPLO        ANDNE     *BLANKS
064600120515     C***                MOVEL     MSG(6)        DSMSMS
064700120515     C***                MOVEL     '1'           ERRL00
064800120515     C***                SETON                                        808182
064900120515E   2C***                ENDIF
065000120515E   1C                   ENDIF
065100970529     C*
065200970529     C* CONTROLLA IL TIPO RICERCA
065300970529IF  1C     ERRL00        IFEQ      '0'
065400970529IF  2C     VIDPCA        IFNE      *BLANKS
065500970529     C                   MOVEL     'CAP'         WTIPRI
065600970529E   2C                   ENDIF
065700970529IF  2C     VIDPLO        IFNE      *BLANKS
065800970529     C                   MOVEL     'LOC'         WTIPRI
065900970529E   2C                   ENDIF
066000970529E   1C                   ENDIF
066100970526     C*
066200970526     C                   ENDSR
066300970526     C*------------------------------------------------------------------------*
066400970526     C* LANPGM - LANCIA UN PROGRAMMA
066500970526     C*------------------------------------------------------------------------*
066600970526     C     LANPGM        BEGSR
066700970526     C*
066800970526     C* IMPOSTA LA DS DI PROCEDURA
066900060404     c                   eval      tipopgm=%subst(wpgm:5:1)
067000060404     c                   if        tipopgm='P'
067100060404     c                   clear                   tisip0ds
067200060404     C                   MOVEL     DNAR          Dp0NAR                         *NAZIONE
067300060404     C                   MOVEL     DCAP          Dp0CAP                         *CAP
067400060404     C                   MOVEL     Dloc          Dp0loc                         *CAP
067500060404     C                   MOVEL     Dprv          Dp0prv                         *CAP
067600060404     C                   MOVEL     wop0          dp0OP0
067700060404     C                   MOVEL     WOP1          Dp0OP1                         *LIVELLO PGM
067800060404     c                   movel     wdri          dp0dri
067900060404     c                   call      WPGM
068000060404     c                   parm                    kpjba
068100060404     c                   parm                    tisip0ds
068200060404     C*
068300060404     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
068400060404IF  1C     Dp0F03        IFEQ      '1'                                          *PREMUTO F03
068500060404     C                   MOVEL     '1'           ERRL00                         *ERRORE
068600060404     C                   MOVEL     '1'           F03L00                         *FINE PGM
068700060404     c                   movel     dp0f03        d00f03
068800060404X   1C                   ELSE                                                   *NON PREMUTO F03
068900060404     C                   MOVEL     Dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
069000060404E   1C                   ENDIF
069100060404     c
069200060404     c                   else
069300970529     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
069400970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
069500970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
069600970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
069700970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
069800970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
069900970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
070000970602     C                   MOVEL     DCAP          D00CAP                         *CAP
070100970602     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
070200970603     C                   MOVEL     *BLANKS       D00SIN                         *ALTRI CAMPI
070300970603     C                   Z-ADD     *ZEROS        D00TTC
070400970530     C                   MOVEL     *BLANKS       D00TSP
070500970530     C                   Z-ADD     *ZEROS        D00TFP
070600970530     C                   MOVEL     *BLANKS       D00EPA
070700970530     C                   Z-ADD     *ZEROS        D00ETP
070800970530     C                   Z-ADD     *ZEROS        D00ETA
070900970530     C                   Z-ADD     *ZEROS        D00EDD
071000970530     C                   MOVEL     *BLANKS       D00SPT
071100970530     C                   Z-ADD     *ZEROS        D00SLP
071200970530     C                   Z-ADD     *ZEROS        D00SLA
071300970530     C                   MOVEL     *BLANKS       D00STS
071400970530     C                   Z-ADD     *ZEROS        D00SDD
071500970526     C*
071600970526     C                   CALL      WPGM
071700970612     C                   PARM                    KPJBA
071800970612     C                   PARM                    DSSI00
071900970526     C*
072000970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
072100970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
072200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
072300970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
072400970526X   1C                   ELSE                                                   *NON PREMUTO F03
072500970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
072600970526E   1C                   ENDIF
072700060404E   1C                   ENDIF
072800970526     C*
072900970526     C                   ENDSR
073000970527     C*------------------------------------------------------------------------*
073100970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
073200970527     C*------------------------------------------------------------------------*
073300970527     C     CARTAB        BEGSR
073400970527     C*
073500970527     C                   ENDSR
073600970526     C*------------------------------------------------------------------------*
073700970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
073800970526     C*------------------------------------------------------------------------*
073900061026     C     CHKFOP        BEGSR
074000061026     C*
074100061026     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
074200061026     C                   SETON                                        93
074300061026     C*
074400061026     C                   IF        §UTECAP = 'C'
074500061026     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
074600061026     C                   SETOFF                                       93
074700061026     C                   ENDIF
074800970626     C*
074900970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
075000970626IF  1C     DEPOP0        IFEQ      'L01'
075100970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
075200970626     C                   MOVEL     'S'           OPDATT
075300970626     C                   MOVEL     OPDRIG        OPD(1)
075400970626X   1C                   ELSE
075500970626     C* OPZIONI
075600970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
075700970626     C                   MOVEL     'S'           OPDATT
075800970626     C                   MOVEL     OPDRIG        OPD(5)
075900970626     C                   MOVEL     OPD(13)       OPDRIG                         *SINONIMI
076000970626     C                   MOVEL     'S'           OPDATT
076100970626     C                   MOVEL     OPDRIG        OPD(13)
076200060404     C                   MOVEL     OPD(14)       OPDRIG                         *SINONIMI
076300060404     C                   MOVEL     'S'           OPDATT
076400060404     C                   MOVEL     OPDRIG        OPD(14)
076500970626     C* FUNZIONI
076600970626     C                   MOVEL     FUD(13)       FUDRIG                         *SINONIMI
076700970626     C                   MOVEL     'S'           FUDATT
076800970626     C                   MOVEL     FUDRIG        FUD(13)
076900060404     C                   MOVEL     FUD(14)       FUDRIG                         *SINONIMI
077000060404     C                   MOVEL     'S'           FUDATT
077100060404     C                   MOVEL     FUDRIG        FUD(14)
077200970626E   1C                   ENDIF
077300970626     C*
077400970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
077500970626IF  1C     DEPOP0        IFEQ      'L02'
077600970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
077700970626     C                   MOVEL     'S'           OPDATT
077800970626     C                   MOVEL     OPDRIG        OPD(2)
077900970627     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
078000970627     C                   MOVEL     'S'           OPDATT
078100970627     C                   MOVEL     OPDRIG        OPD(3)
078200061026     C  N93              MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
078300061026     C  N93              MOVEL     'S'           OPDATT
078400061026     C  N93              MOVEL     OPDRIG        OPD(4)
078500061026     C  N93              MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
078600061026     C  N93              MOVEL     'S'           OPDATT
078700061026     C  N93              MOVEL     OPDRIG        OPD(7)
078800060404     C                   MOVEL     OPD(15)       OPDRIG                         *RIPRISTINO
078900060404     C                   MOVEL     'S'           OPDATT
079000060404     C                   MOVEL     OPDRIG        OPD(15)
079100061016     C* CONSENTITO INSERIMENTO SOLO A PERSONALE "EDP"
079200061026     C                   IF        %subst(KNMUS:1:3) = 'EDP' AND *IN93 = *off
079300970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
079400970626     C                   MOVEL     'S'           FUDATT
079500970626     C                   MOVEL     FUDRIG        FUD(6)
079600061016     C                   ENDIF
079700970626E   1C                   ENDIF
079800970626     C*
079900970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
080000060404IF  1C**   DUTLPO        IFNE      'S'                                          *FILIALE
080100060404     C**                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
080200060404     C**                 MOVEL     'N'           OPDATT
080300060404     C**                 MOVEL     OPDRIG        OPD(4)
080400060404     C**                 MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
080500060404     C**                 MOVEL     'N'           OPDATT
080600060404     C**                 MOVEL     OPDRIG        OPD(7)
080700060404     C**                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
080800060404     C**                 MOVEL     'N'           FUDATT
080900060404     C**                 MOVEL     FUDRIG        FUD(6)
081000060404E   1C*+                 ENDIF
081100970526     C*
081200970526     C* GESTIONE OPZIONI A VIDEO
081300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
081400970526     C                   EXSR      OPZGES
081500970526     C*
081600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
081700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
081800970526     C                   EXSR      FUNGES
081900970526     C*
082000970526     C                   ENDSR
082100020919     C*--------------------------------------------------------------------------------------------*
082200020919     C* REPERISCE I DATI UTENTE
082300020919     C*--------------------------------------------------------------------------------------------*
082400020919     C     REPDATIUTE    BEGSR
082500020919     C*
082600020919     C* INIZIALIZZA VARIABILI DI WRK
082700020919     C                   CLEAR                   TIBS34DS
082800020919     C                   CLEAR                   AZUTEDS
082900020919     C                   CLEAR                   DDATIUTE
083000020919     C*
083100020919     C     *DTAARA       DEFINE    §azute        azuteds
083200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
083300020919     C                   IN(E)     *DTAARA
083400020919if  1C                   IF        %Error
083500020919     c                   EVAL      I34Tla = 'L'
083600020919     C                   CALL      'TIBS34R'
083700020919     C                   PARM                    Tibs34Ds
083800020919     C                   IN        *DTAARA
083900020919e   1C                   ENDIF
084000020919     C*
084100020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
084200020919     C                   MOVEL(P)  RSUT          DSFIRS           20
084300061026     C*
084400061026     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
084500061026     C                   EVAL      DUTE01 = UTEFAF
084600020919     C*
084700020919     C                   ENDSR
084800970526     C*------------------------------------------------------------------------*
084900970526     C* *INZSR - OPERAZIONI INIZIALI
085000970526     C*------------------------------------------------------------------------*
085100970526     C     *INZSR        BEGSR
085200970526     C*---
085300970526     C* RICEVIMENTO PARAMETRI
085400970526     C*---
085500970526     C     *ENTRY        PLIST
085600970612     C                   PARM                    KPJBA
085700970612     C                   PARM                    DSSI00
085800970526     C*
085900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
086000970526     C     XMSGPA        PLIST
086100970526     C                   PARM                    DSMSMS
086200970526     C                   PARM                    DSMSPG
086300970526     C                   PARM                    DSMSKE
086400970526     C                   PARM                    DSMSER
086500970526     C*
086600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
086700970526     C                   CALL      'XKEY'
086800970526     C                   PARM                    DSKEY
086900970526     C*---
087000970526     C* VARIABILI NON RIFERITE AL DATA BASE
087100970526     C*---
087200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
087300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
087400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
087500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
087600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
087700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
087800970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
087900970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
088000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
088100970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
088200970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
088300970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
088400970529     C     *LIKE         DEFINE    D00OP1        WOP1
088500970602     C     *LIKE         DEFINE    D00NAR        DNAR
088600970602     C     *LIKE         DEFINE    D00CAP        DCAP
088700970602     C     *LIKE         DEFINE    D00LOC        DLOC
088800970526     C*---
088900970526     C* VARIABILI DI CONTROLLO
089000970526     C*---
089100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
089200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
089300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
089400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
089500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
089600970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
089700970526     C*
089800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
089900970529     C                   MOVEL     'TISI30R'     DSMSPG
090000970529     C                   MOVEL     'TISI30R'     MSGQUE
090100970526     C                   MOVEL     *BLANKS       MSGKEY
090200970526     C                   MOVEL     *BLANKS       DSMSMS
090300970526     C                   MOVEL     *BLANKS       DSMSKE
090400970526     C                   Z-ADD     *ZEROS        DSMSER
090500970526     C*---
090600970526     C* VARIABILI RIFERITE AL DATA BASE
090700970526     C*---
090800970529     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
090900970529     C     *LIKE         DEFINE    CPLNAR        KCLNAR
091000970529     C     *LIKE         DEFINE    CPLCAP        KCLCAP
091100970529     C     *LIKE         DEFINE    CPLLOC        KCLLOC
091200970528     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
091300970528     C     *LIKE         DEFINE    CPCNAR        KCCNAR
091400970528     C     *LIKE         DEFINE    CPCCAP        KCCCAP
091500970529     C     *LIKE         DEFINE    CPLNAR        DEPNAR                         *DEPOSITI
091600970529     C     *LIKE         DEFINE    CPLCAP        DEPCAP
091700120515     C     *LIKE         DEFINE    CPCPRV        DEPPPR
091800970529     C     *LIKE         DEFINE    CPLLOC        DEPLOC
091900970526     C*---
092000970526     C* CHIAVI DI LETTURA
092100970526     C*---
092200970529     C* LETTURA AZCPL01L - PARZIALE
092300970529     C     KEYCPL        KLIST
092400970529     C                   KFLD                    KCLVER                         *VERSIONE
092500970529     C                   KFLD                    KCLNAR                         *NAZIONE
092600970529     C                   KFLD                    KCLCAP                         *CAP
092700970529     C* LETTURA AZCPL02L - COMPLETA
092800970529     C     KE2CPL        KLIST
092900970529     C                   KFLD                    KCLVER                         *VERSIONE
093000970605     C                   KFLD                    KCLNAR                         *NAZIONE
093100970529     C                   KFLD                    KCLLOC                         *LOCALITA'
093200970528     C* LETTURA AZCPC01L - COMPLETA
093300970528     C     KEYCPC        KLIST
093400970528     C                   KFLD                    KCCVER                         *VERSIONE
093500970528     C                   KFLD                    KCCNAR                         *NAZIONE
093600970528     C                   KFLD                    KCCCAP                         *CAP
093700970527     C*
093800970527     C* REPERIMENTO DATI UTENTE
093900020919     C                   EXSR      REPDATIUTE
094000970527     C*
094100970527     C* CARICA LE TABELLE OCCORRENTI
094200970527     C                   EXSR      CARTAB
094300970526     C*
094400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
094500970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
094600970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
094700970526     C*
094800970526     C* CALCOLA LA DATA CORRENTE
094900970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
095000970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
095100970526     C                   Z-ADD     WN8           G08DAT
095200970526     C                   Z-ADD     *ZEROS        G08INV
095300970526     C                   MOVEL     '0'           G08ERR
095400970526     C                   CALL      'XSRDA8'
095500970526     C                   PARM                    WLBDA8
095600970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
095700970526     C*
095800970526     C* IMPOSTA I PARAMETRI DI INGRESSO
095900970526     C                   Z-ADD     D00DRI        G08INV
096000970526     C                   Z-ADD     *ZEROS        G08DAT
096100970526     C                   MOVEL     '3'           G08ERR
096200970526     C                   CALL      'XSRDA8'
096300970526     C                   PARM                    WLBDA8
096400970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
096500970527     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
096600970527     C                   MOVEL     D00CAP        VIDPCA                         *CAP
096700970529     C                   MOVEL     D00LOC        VIDPLO                         *LOCALITA
096800970526     C                   EXSR      CHKPAR
096900970526IF  1C     ERRL00        IFEQ      '1'
097000970526     C                   MOVEL     '1'           D00ERR
097100970526     C                   MOVEL     DSMSMS        D00MSG
097200970526X   1C                   ELSE
097300970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
097400970527     C                   MOVEL     VIDPCA        DEPCAP
097500120515     C                   MOVEL     VIDPPR        DEPPPR
097600970529     C                   MOVEL     VIDPLO        DEPLOC
097700970526E   1C                   ENDIF
097800970703     C*
097900970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
098000970703IF  1C     D00VER        IFEQ      *ZEROS
098100970707     C                   Z-ADD     D00DRI        P96DRI            8 0
098200970707     C                   Z-ADD     *ZEROS        P96VER            5 0
098300970707     C                   CALL      'TISI96R'
098400970707     C                   PARM                    P96DRI
098500970707     C                   PARM                    P96VER
098600970707     C                   Z-ADD     P96VER        D00VER
098700970703E   1C                   ENDIF
098800970526     C*
098900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
099000970526     C                   EXSR      CHKFOP
099100970526     C*
099200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
099300970530IF  1C     DEPOP0        IFEQ      'L01'
099400970530     C                   MOVEL     DEC(3)        VIDING
099500970530E   1C                   ENDIF
099600970530IF  1C     DEPOP0        IFEQ      'L02'
099700970530     C                   MOVEL     DEC(1)        VIDING
099800970530E   1C                   ENDIF
099900970530IF  1C     DEPOP0        IFEQ      'L05'
100000970530     C                   MOVEL     DEC(2)        VIDING
100100970530E   1C                   ENDIF
100200970526     C*
100300970526     C                   ENDSR
100400970526     C*------------------------------------------------------------------------*
100500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
100600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
100700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
100800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
100900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
101000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
101200970526     C*------------------------------------------------------------------------*
101300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
101400970526     C*------------------------------------------------------------------------*
101500970526     C     POSSFL        BEGSR
101600970526     C*
101700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
101800970526     C                   EXSR      CLRSFL
101900970526     C*
102000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
102100970526     C                   EXSR      SETFIL
102200970526     C*
102300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
102400970526     C     SFLEOF        IFEQ      0
102500970526     C                   EXSR      CARSFL
102600970526     C                   END
102700970526     C*
102800970526     C                   ENDSR
102900970526     C*------------------------------------------------------------------------*
103000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
103100970526     C*------------------------------------------------------------------------*
103200970526     C     ENTATT        BEGSR
103300970526     C*
103400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
103500970526     C                   Z-ADD     0             OPZOK             1 0
103600970526     C*
103700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
103800970526     C                   Z-ADD     0             SFLVAR            5 0
103900970526     C*
104000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
104100970526     C                   Z-ADD     0             SFLERR
104200970526     C*
104300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
104400970526     C                   Z-ADD     SFLULT        WFLULT
104500970526     C*
104600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
104700970529     C                   READC     SI30D05                                98
104800970526     C     *IN98         DOWEQ     '0'
104900970526     C*
105000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
105100970526     C                   ADD       1             SFLVAR
105200970526     C*
105300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
105400970526     C     OPZOK         IFEQ      0
105500970526     C*
105600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
105700970526     C     OPZKEY        IFNE      *BLANKS
105800970526     C                   EXSR      OPZCON
105900970526     C                   END
106000970526     C*
106100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
106200970526     C*            2. PULIZIA SCELTA IN SFL
106300970526     C     OPZOK         IFEQ      0
106400970526     C                   MOVEL     '0'           *IN41
106500970526     C                   MOVEL     *BLANKS       OPZKEY
106600970526     C                   ELSE
106700970526     C*
106800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
106900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
107000970526     C                   MOVEL     '1'           *IN41
107100970526     C                   Z-ADD     SFLULT        SFLERR
107200970526     C                   END
107300970526     C                   ELSE
107400970526     C*
107500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
107600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
107700970526     C                   MOVEL     '1'           *IN41
107800970526     C                   END                                                    OPZOK=0
107900970526     C*
108000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
108100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
108200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
108300970526     C     OPZOK         IFNE      0
108400970526     C                   Z-ADD     SFLERR        SFLNBR
108500970526     C                   ELSE
108600970526     C                   Z-ADD     SFLULT        SFLNBR
108700970526     C                   END
108800970526     C*
108900970526     C* AGGIORNO RECORD SFL
109000970529     C                   UPDATE    SI30D05
109100970526     C*
109200970529     C                   READC     SI30D05                                98
109300970526     C                   END                                                    DO WHILE
109400970526     C*
109500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
109600970526     C                   Z-ADD     WFLULT        SFLULT
109700970526     C*
109800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
109900970526     C     SFLVAR        IFEQ      0
110000970526     C*---                 MOVEL'1'       FINL00
110100970526     C                   END
110200970526     C*
110300970526     C                   ENDSR
110400970526     C*------------------------------------------------------------------------*
110500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
110600970526     C*------------------------------------------------------------------------*
110700970526     C     CLRSFL        BEGSR
110800970526     C*
110900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
111000970526     C* (0=NON FINE FILE  1=FINE FILE)
111100970526     C                   Z-ADD     0             SFLEOF
111200970526     C*
111300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
111400970526     C                   Z-ADD     0             SFLULT
111500970526     C*
111600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
111700970526     C                   MOVEL     '1'           *IN40
111800970529     C                   WRITE     SI30D06
111900970526     C                   MOVEL     '0'           *IN40
112000970526     C*
112100970526     C                   ENDSR
112200970526     C*------------------------------------------------------------------------*
112300970526     C* CARSFL - CARICAMENTO SFL
112400970526     C*------------------------------------------------------------------------*
112500970526     C     CARSFL        BEGSR
112600970526     C*
112700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
112800970526     C     SFLULT        IFEQ      0
112900970526     C                   EXSR      SETFIL
113000970526     C                   END
113100970526     C*
113200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
113300970526     C                   Z-ADD     SFLULT        WFLULT
113400970526     C*
113500970526     C* CICLO:
113600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
113700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
113800970526     C                   Z-ADD     1             SFLRIG
113900970526     C     SFLRIG        DOWLE     SFLPAG
114000970526     C     SFLEOF        ANDEQ     0
114100970526     C*
114200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
114300970526     C                   MOVEL     *BLANKS       OPZKEY
114400970526     C*
114500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
114600970526     C                   EXSR      IRESFL
114700970526     C*
114800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
114900970526     C                   ADD       1             SFLULT
115000970526     C*
115100970526     C* SCRITTURA RECORD SFL
115200970529     C                   WRITE     SI30D05
115300970526     C                   ADD       1             SFLRIG
115400970526     C*
115500970526     C* LETTURA FILE DA CARICARE
115600970526     C                   EXSR      LETFIL
115700970526     C                   END                                                    DO WHILE
115800970526     C*
115900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
116000970526     C     WFLULT        IFNE      SFLULT
116100970526     C                   Z-ADD     WFLULT        SFLNBR
116200970526     C                   ADD       1             SFLNBR
116300970526     C                   END
116400970526     C*
116500970526     C                   ENDSR
116600970526     C*------------------------------------------------------------------------*
116700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
116800970526     C*------------------------------------------------------------------------*
116900970526     C     OUTSFL        BEGSR
117000970526     C*
117100970526     C* WRITE RECORD TESTATA VIDEO
117200970529     C                   WRITE     SI30D01
117300970526     C*
117400970526     C* VISUALIZZAZIONE MESSAGGIO
117500970526     C                   EXSR      SNDMSG
117600970526     C*
117700970526     C* WRITE RECORD OPZIONI VIDEO
117800970529     C                   WRITE     SI30D02
117900970526     C*
118000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
118100970529     C                   WRITE     SI30D03
118200970526     C*
118300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
118400970529     C                   WRITE     SI30D04
118500970526     C*
118600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
118700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
118800970526     C     SFLEOF        IFEQ      0
118900970526     C                   MOVEL     '0'           *IN42
119000970526     C                   ELSE
119100970526     C*
119200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
119300970526     C                   MOVEL     '1'           *IN42
119400970526     C                   END
119500970526     C*
119600970526     C* WRITE RECORD TASTI FUNZIONALI
119700970529     C                   WRITE     SI30D09
119800970526     C*
119900970526     C* CASO DI SFL VUOTO
120000970526     C     SFLULT        IFLE      0
120100970526     C*
120200970526     C*    - WRITE RECORD VUOTO
120300970529     C                   WRITE     SI30D0A
120400970529     C                   READ      SI30D0A                                99
120500970526     C*
120600970526     C                   ELSE
120700970526     C*
120800970526     C* CASO DI SFL CON RECORDS
120900970526     C*
121000970526     C*    - WRITE RECORD CONTROLLO SFL
121100970529     C                   WRITE     SI30D06
121200970529     C                   READ      SI30D06                                99
121300970526     C                   END
121400970526     C*
121500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
121600970529     C                   READ      SI30D03                                99
121700970526     C*
121800970526     C                   ENDSR
121900970526     C*------------------------------------------------------------------------*
122000970526     C* SNDMSG - MANDA MESSAGGIO
122100970526     C*------------------------------------------------------------------------*
122200970526     C     SNDMSG        BEGSR
122300970526     C*
122400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
122500970526     C                   MOVEL     '1'           *IN40
122600970529     C                   WRITE     SI30DMC
122700970526     C                   MOVEL     '0'           *IN40
122800970526     C*
122900970526     C* VISUALIZZAZIONE MESSAGGIO
123000970526     C                   Z-ADD     0             MSGULT
123100970526     C                   CALL      'XMSG'        XMSGPA
123200970526     C     DSMSER        IFEQ      0
123300970526     C                   MOVEL     DSMSKE        MSGKEY
123400970526     C                   END
123500970526     C                   Z-ADD     1             MSGULT
123600970529     C                   WRITE     SI30DMS                                      SFL MESS
123700970529     C                   WRITE     SI30DMC                                      CONTR MESS
123800970526     C*
123900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
124000970526     C                   MOVEL     *BLANKS       DSMSMS
124100970526     C*
124200970526     C                   ENDSR
124300970526     C*------------------------------------------------------------------------*
124400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
124500970526     C*------------------------------------------------------------------------*
124600970526     C     FUNCON        BEGSR
124700970526     C*
124800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
124900970526     C                   EXSR      FUNVAR
125000970526     C*
125100970526     C* PULISCO CAMPI DI LAVORO
125200970526     C                   Z-ADD     0             I                 5 0
125300970526     C                   Z-ADD     0             W                 5 0
125400970526     C*
125500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
125600970526     C     FUNKEY        IFNE      *BLANKS
125700970526     C     1             DO        24            I
125800970526     C                   MOVEL     FUD(I)        FUDRIG
125900970526     C*
126000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
126100970526     C     FUNKEY        IFEQ      FUDFUN
126200970526     C     FUDATT        ANDEQ     'S'
126300970526     C                   Z-ADD     I             W
126400970526     C                   END
126500970526     C                   END                                                    DO WHILE
126600970526     C*
126700970526     C* FUNZIONE NON ATTIVA (ERRORE)
126800970526     C     W             IFEQ      0
126900970526     C*
127000970526     C* EMISSIONE MESSAGGIO DI ERRORE
127100970526     C                   MOVEL     MSG(2)        DSMSMS
127200970526     C                   END                                                    W=0
127300970526     C                   END                                                    FUNKEY NE *BLANKS
127400970526     C*
127500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
127600970526     C     W             IFNE      0
127700970526     C     FUNKEY        OREQ      *BLANKS
127800970526     C*
127900970526     C     FUNKEY        IFEQ      *BLANKS
128000970526     C                   EXSR      ENTGES
128100970526     C                   END
128200970526     C     FUNKEY        IFEQ      '03'
128300970526     C                   EXSR      F03GES
128400970526     C                   END
128500970526     C     FUNKEY        IFEQ      '05'
128600970526     C                   EXSR      F05GES
128700970526     C                   END
128800970526     C     FUNKEY        IFEQ      '06'
128900970526     C                   EXSR      F06GES
129000970526     C                   END
129100970526     C     FUNKEY        IFEQ      '12'
129200970526     C                   EXSR      F12GES
129300970526     C                   END
129400970529     C     FUNKEY        IFEQ      '13'
129500970529     C                   EXSR      F13GES
129600970529     C                   END
129700060404     C     FUNKEY        IFEQ      '14'
129800060404     C                   EXSR      F14GES
129900060404     C                   END
130000970526     C     FUNKEY        IFEQ      '23'
130100970526     C                   EXSR      F23GES
130200970526     C                   END
130300970526     C     FUNKEY        IFEQ      '24'
130400970526     C                   EXSR      F24GES
130500970526     C                   END
130600970526     C     FUNKEY        IFEQ      'RU'
130700970526     C                   EXSR      RUPGES
130800970526     C                   END
130900970526     C*
131000970526     C                   END
131100970526     C*
131200970526     C                   ENDSR
131300970526     C*------------------------------------------------------------------------*
131400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
131500970526     C*------------------------------------------------------------------------*
131600970526     C     FUNVAR        BEGSR
131700970526     C*
131800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
131900970526     C     KEY           IFEQ      ENTER
132000970526     C                   MOVEL     *BLANKS       FUNKEY
132100970526     C                   END
132200970526     C     KEY           IFEQ      F1
132300970526     C                   MOVEL     '01'          FUNKEY
132400970526     C                   END
132500970526     C     KEY           IFEQ      F2
132600970526     C                   MOVEL     '02'          FUNKEY
132700970526     C                   END
132800970526     C     KEY           IFEQ      F3
132900970526     C                   MOVEL     '03'          FUNKEY
133000970526     C                   END
133100970526     C     KEY           IFEQ      F4
133200970526     C                   MOVEL     '04'          FUNKEY
133300970526     C                   END
133400970526     C     KEY           IFEQ      F5
133500970526     C                   MOVEL     '05'          FUNKEY
133600970526     C                   END
133700970526     C     KEY           IFEQ      F6
133800970526     C                   MOVEL     '06'          FUNKEY
133900970526     C                   END
134000970526     C     KEY           IFEQ      F7
134100970526     C                   MOVEL     '07'          FUNKEY
134200970526     C                   END
134300970526     C     KEY           IFEQ      F8
134400970526     C                   MOVEL     '08'          FUNKEY
134500970526     C                   END
134600970526     C     KEY           IFEQ      F9
134700970526     C                   MOVEL     '09'          FUNKEY
134800970526     C                   END
134900970526     C     KEY           IFEQ      F10
135000970526     C                   MOVEL     '10'          FUNKEY
135100970526     C                   END
135200970526     C     KEY           IFEQ      F11
135300970526     C                   MOVEL     '11'          FUNKEY
135400970526     C                   END
135500970526     C     KEY           IFEQ      F12
135600970526     C                   MOVEL     '12'          FUNKEY
135700970526     C                   END
135800970526     C     KEY           IFEQ      F13
135900970526     C                   MOVEL     '13'          FUNKEY
136000970526     C                   END
136100970526     C     KEY           IFEQ      F14
136200970526     C                   MOVEL     '14'          FUNKEY
136300970526     C                   END
136400970526     C     KEY           IFEQ      F15
136500970526     C                   MOVEL     '15'          FUNKEY
136600970526     C                   END
136700970526     C     KEY           IFEQ      F16
136800970526     C                   MOVEL     '16'          FUNKEY
136900970526     C                   END
137000970526     C     KEY           IFEQ      F17
137100970526     C                   MOVEL     '17'          FUNKEY
137200970526     C                   END
137300970526     C     KEY           IFEQ      F18
137400970526     C                   MOVEL     '18'          FUNKEY
137500970526     C                   END
137600970526     C     KEY           IFEQ      F19
137700970526     C                   MOVEL     '19'          FUNKEY
137800970526     C                   END
137900970526     C     KEY           IFEQ      F20
138000970526     C                   MOVEL     '20'          FUNKEY
138100970526     C                   END
138200970526     C     KEY           IFEQ      F21
138300970526     C                   MOVEL     '21'          FUNKEY
138400970526     C                   END
138500970526     C     KEY           IFEQ      F22
138600970526     C                   MOVEL     '22'          FUNKEY
138700970526     C                   END
138800970526     C     KEY           IFEQ      F23
138900970526     C                   MOVEL     '23'          FUNKEY
139000970526     C                   END
139100970526     C     KEY           IFEQ      F24
139200970526     C                   MOVEL     '24'          FUNKEY
139300970526     C                   END
139400970526     C     KEY           IFEQ      ROLLUP
139500970526     C                   MOVEL     'RU'          FUNKEY
139600970526     C                   END
139700970526     C     KEY           IFEQ      HELP
139800970526     C                   MOVEL     'HP'          FUNKEY
139900970526     C                   END
140000970526     C*
140100970526     C                   ENDSR
140200970526     C*------------------------------------------------------------------------*
140300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
140400970526     C*------------------------------------------------------------------------*
140500970526     C     FUNGES        BEGSR
140600970526     C*
140700970526     C* PULISCO CAMPI DI LAVORO
140800970526     C                   Z-ADD     0             W                 5 0
140900970526     C*
141000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
141100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
141200970526     C     1             DO        24            I
141300970526     C                   MOVEL     FUD(I)        FUDRIG
141400970526     C*
141500970526     C* PULIZIA FUNZIONI PRECEDENTI
141600970526     C                   MOVEL     *BLANKS       FUV(I)
141700970526     C*
141800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
141900970526     C* (SALTA F23 E F24)
142000970526     C     FUDATT        IFEQ      'S'
142100970526     C     FUDFUN        ANDNE     '23'
142200970526     C     FUDFUN        ANDNE     '24'
142300970526     C                   ADD       1             W
142400970526     C                   MOVEL     FUDDES        FUV(W)
142500970526     C                   END
142600970526     C                   END                                                    DO WHILE
142700970526     C*
142800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
142900970526     C* ALTRIMENTI DISATTIVO F24
143000970526     C     1             DO        24            I
143100970526     C                   MOVEL     FUD(I)        FUDRIG
143200970526     C*
143300970526     C     FUDFUN        IFEQ      '24'
143400970526     C     W             IFGT      6
143500970526     C                   MOVEL     'S'           FUDATT
143600970526     C                   ELSE
143700970526     C                   MOVEL     *BLANKS       FUDATT
143800970526     C                   END                                                    W GT 8
143900970526     C                   MOVEL     FUDRIG        FUD(I)
144000970526     C                   END
144100970526     C                   END                                                    DO WHILE
144200970526     C*
144300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
144400970526     C     W             DIV       6             I
144500970526     C                   MVR                     Z
144600970526     C*
144700970526     C     Z             IFNE      0
144800970526     C     I             ADD       1             Z
144900970526     C                   ELSE
145000970526     C                   Z-ADD     I             Z
145100970526     C                   END
145200970526     C*
145300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
145400970526     C                   ADD       1             FUNGRU
145500970526     C     FUNGRU        IFGT      Z
145600970526     C     FUNGRU        ORLT      1
145700970526     C                   Z-ADD     1             FUNGRU
145800970526     C                   END
145900970526     C*
146000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
146100970526     C     FUNGRU        IFEQ      1
146200970526     C                   MOVEL     FUV(1)        FUN001
146300970526     C                   MOVEL     FUV(2)        FUN002
146400970526     C                   MOVEL     FUV(3)        FUN003
146500970526     C                   MOVEL     FUV(4)        FUN004
146600970526     C                   MOVEL     FUV(5)        FUN005
146700970526     C                   MOVEL     FUV(6)        FUN006
146800970526     C                   END
146900970526     C*
147000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
147100970526     C     FUNGRU        IFEQ      2
147200970526     C                   MOVEL     FUV(7)        FUN001
147300970526     C                   MOVEL     FUV(8)        FUN002
147400970526     C                   MOVEL     FUV(9)        FUN003
147500970526     C                   MOVEL     FUV(10)       FUN004
147600970526     C                   MOVEL     FUV(11)       FUN005
147700970526     C                   MOVEL     FUV(12)       FUN006
147800970526     C                   END
147900970526     C*
148000970526     C* TERZO GRUPPO DI 6 FUNZIONI
148100970526     C     FUNGRU        IFEQ      3
148200970526     C                   MOVEL     FUV(13)       FUN001
148300970526     C                   MOVEL     FUV(14)       FUN002
148400970526     C                   MOVEL     FUV(15)       FUN003
148500970526     C                   MOVEL     FUV(16)       FUN004
148600970526     C                   MOVEL     FUV(17)       FUN005
148700970526     C                   MOVEL     FUV(18)       FUN006
148800970526     C                   END
148900970526     C*
149000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
149100970526     C     FUNGRU        IFEQ      4
149200970526     C                   MOVEL     FUV(19)       FUN001
149300970526     C                   MOVEL     FUV(20)       FUN002
149400970526     C                   MOVEL     FUV(21)       FUN003
149500970526     C                   MOVEL     FUV(22)       FUN004
149600970526     C                   MOVEL     *BLANKS       FUN005
149700970526     C                   MOVEL     *BLANKS       FUN006
149800970526     C                   END
149900970526     C*
150000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
150100970526     C                   MOVEL     *BLANKS       FUN007
150200970526     C                   MOVEL     *BLANKS       FUN008
150300970526     C*
150400970526     C     1             DO        24            I
150500970526     C                   MOVEL     FUD(I)        FUDRIG
150600970526     C     FUDFUN        IFEQ      '23'
150700970526     C     FUDATT        ANDEQ     'S'
150800970526     C                   MOVEL     FUDDES        FUN007
150900970526     C                   END
151000970526     C     FUDFUN        IFEQ      '24'
151100970526     C     FUDATT        ANDEQ     'S'
151200970526     C                   MOVEL     FUDDES        FUN008
151300970526     C                   END
151400970526     C                   END                                                    DO
151500970526     C*
151600970526     C                   ENDSR
151700970526     C*------------------------------------------------------------------------*
151800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
151900970526     C*------------------------------------------------------------------------*
152000970526     C     OPZGES        BEGSR
152100970526     C*
152200970526     C* PULISCO CAMPI DI LAVORO
152300970526     C                   Z-ADD     0             W                 5 0
152400970526     C*
152500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
152600970526     C* E CARICO TABELLA OPZIONI ATTIVE
152700970526     C     1             DO        24            I
152800970526     C                   MOVEL     OPD(I)        OPDRIG
152900970526     C*
153000970526     C* PULIZIA OPZIONI PRECEDENTI
153100970526     C                   MOVEL     *BLANKS       OPV(I)
153200970526     C*
153300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
153400970526     C     OPDATT        IFEQ      'S'
153500970526     C                   ADD       1             W
153600970526     C                   MOVEL     OPDDES        OPV(W)
153700970526     C                   END
153800970526     C                   END                                                    DO WHILE
153900970526     C*
154000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
154100970526     C* ALTRIMENTI DISATTIVO F23
154200970526     C     1             DO        24            I
154300970526     C                   MOVEL     FUD(I)        FUDRIG
154400970526     C*
154500970526     C     FUDFUN        IFEQ      '23'
154600970526     C     W             IFGT      8
154700970526     C                   MOVEL     'S'           FUDATT
154800970526     C                   ELSE
154900970526     C                   MOVEL     *BLANKS       FUDATT
155000970526     C                   END                                                    W GT 8
155100970526     C                   MOVEL     FUDRIG        FUD(I)
155200970526     C                   END
155300970526     C                   END                                                    DO WHILE
155400970526     C*
155500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
155600970526     C     W             DIV       8             I
155700970526     C                   MVR                     Z                 5 0
155800970526     C*
155900970526     C     Z             IFNE      0
156000970526     C     I             ADD       1             Z
156100970526     C                   ELSE
156200970526     C                   Z-ADD     I             Z
156300970526     C                   END
156400970526     C*
156500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
156600970526     C                   ADD       1             OPZGRU
156700970526     C     OPZGRU        IFGT      Z
156800970526     C     OPZGRU        ORLT      1
156900970526     C                   Z-ADD     1             OPZGRU
157000970526     C                   END
157100970526     C*
157200970526     C* PRIMO GRUPPO DI 8 OPZIONI
157300970526     C     OPZGRU        IFEQ      1
157400970526     C                   MOVEL     OPV(1)        OPZ001
157500970526     C                   MOVEL     OPV(2)        OPZ002
157600970526     C                   MOVEL     OPV(3)        OPZ003
157700970526     C                   MOVEL     OPV(4)        OPZ004
157800970526     C                   MOVEL     OPV(5)        OPZ005
157900970526     C                   MOVEL     OPV(6)        OPZ006
158000970526     C                   MOVEL     OPV(7)        OPZ007
158100970526     C                   MOVEL     OPV(8)        OPZ008
158200970526     C                   END
158300970526     C*
158400970526     C* SECONDO GRUPPO DI 8 OPZIONI
158500970526     C     OPZGRU        IFEQ      2
158600970526     C                   MOVEL     OPV(9)        OPZ001
158700970526     C                   MOVEL     OPV(10)       OPZ002
158800970526     C                   MOVEL     OPV(11)       OPZ003
158900970526     C                   MOVEL     OPV(12)       OPZ004
159000970526     C                   MOVEL     OPV(13)       OPZ005
159100970526     C                   MOVEL     OPV(14)       OPZ006
159200970526     C                   MOVEL     OPV(15)       OPZ007
159300970526     C                   MOVEL     OPV(16)       OPZ008
159400970526     C                   END
159500970526     C*
159600970526     C* TERZO GRUPPO DI 8 OPZIONI
159700970526     C     OPZGRU        IFEQ      3
159800970526     C                   MOVEL     OPV(17)       OPZ001
159900970526     C                   MOVEL     OPV(18)       OPZ002
160000970526     C                   MOVEL     OPV(19)       OPZ003
160100970526     C                   MOVEL     OPV(20)       OPZ004
160200970526     C                   MOVEL     OPV(21)       OPZ005
160300970526     C                   MOVEL     OPV(22)       OPZ006
160400970526     C                   MOVEL     OPV(23)       OPZ007
160500970526     C                   MOVEL     OPV(24)       OPZ008
160600970526     C                   END
160700970526     C*
160800970526     C                   ENDSR
160900970526     C*------------------------------------------------------------------------*
161000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
161100970526     C*------------------------------------------------------------------------*
161200970526     C     OPZCON        BEGSR
161300970526     C*
161400970526     C* PULISCO CAMPI DI LAVORO
161500970526     C                   Z-ADD     0             I                 5 0
161600970526     C                   Z-ADD     0             W                 5 0
161700970526     C*
161800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
161900970526     C     1             DO        24            I
162000970526     C                   MOVEL     OPD(I)        OPDRIG
162100970526     C*
162200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
162300970526     C     OPZKEY        IFEQ      OPDOPZ
162400970526     C     OPZKEY        OREQ      OPDOP1
162500970526     C*
162600970526     C     OPDATT        IFEQ      'S'
162700970526     C                   MOVEL     OPDOPZ        OPZKEY
162800970526     C                   Z-ADD     I             W
162900970526     C                   END
163000970526     C                   END
163100970526     C*
163200970526     C                   END                                                    DO WHILE
163300970526     C*
163400970526     C* OPZIONE NON ATTIVA (ERRORE)
163500970526     C     W             IFEQ      0
163600970526     C*
163700970526     C* OPZIONE NON OK
163800970526     C                   Z-ADD     1             OPZOK
163900970526     C*
164000970526     C* EMISSIONE MESSAGGIO DI ERRORE
164100970526     C                   MOVEL     MSG(1)        DSMSMS
164200970526     C                   ELSE
164300970526     C*
164400970526     C* OPZIONE ATTIVA (PROCEDI ...)
164500970526     C*
164600970526     C     OPZKEY        IFEQ      ' 1'
164700970526     C                   EXSR      O01GES
164800970526     C                   END
164900970526     C     OPZKEY        IFEQ      ' 2'
165000970526     C                   EXSR      O02GES
165100970526     C                   END
165200970611     C     OPZKEY        IFEQ      ' 3'
165300970611     C                   EXSR      O03GES
165400970611     C                   END
165500970526     C     OPZKEY        IFEQ      ' 4'
165600970526     C                   EXSR      O04GES
165700970526     C                   END
165800970526     C     OPZKEY        IFEQ      ' 5'
165900970526     C                   EXSR      O05GES
166000970526     C                   END
166100970526     C     OPZKEY        IFEQ      ' 7'
166200970526     C                   EXSR      O07GES
166300970526     C                   END
166400970529     C     OPZKEY        IFEQ      '13'
166500970529     C                   EXSR      O13GES
166600970529     C                   END
166700060404     C     OPZKEY        IFEQ      '14'
166800060404     C                   EXSR      O14GES
166900060404     C                   END
167000060404     C     OPZKEY        IFEQ      '15'
167100060404     C                   EXSR      O15GES
167200060404     C                   END
167300970526     C                   END                                                    OPZ.NON ATTIVA
167400970526     C*
167500970526     C                   ENDSR
167600970526     C*------------------------------------------------------------------------*
167700970526     C* NOTE COSTRUZIONE SCHIERE
167800970526     C*------------------------------------------------------------------------*
167900970526     C*
168000970526     C* -MSG-MESSAGGI
168100970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
168200970526     C* (NON TOCCARE MSG,1 E MSG,2)
168300970526     C*
168400970526     C* -CMD-COMANDI
168500970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
168600970526     C*
168700970526     C* -OPD-OPZIONI
168800970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
168900970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
169000970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
169100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
169200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
169300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
169400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
169500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
169600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
169700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
169800970526     C*
169900970526     C* -FUD-FUNZIONI
170000970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
170100970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
170200970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
170300970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
170400970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
170500970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
170600970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
170700970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
170800970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
170900970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
171000970526     O*------------------------------------------------------------------------*
171100970526** DEC - DECODIFICHE
171200970526    GESTIONE                                                     1
171300970530INTERROGAZIONE                                                   2
171400970530     SCELTA                                                      3
1715009705264
1716009705265
171700970526** MSG - MESSAGGI
171800970529TISI30R- Opzione non gestita                                                   1
171900970529TISI30R- Tasto funzionale non gestito                                          2
172000970704TISI30R- 3
172100970529TISI30R- Record annullato. Consentito solo il ripristino                       4
172200970529TISI30R- Record non annullato. Impossibile ripristinare                        5
172300970529TISI30R- Accettato solo un parametro di inizio lista                           6
172400970529TISI30R- 7                                                                     7
172500970529TISI30R- 8                                                                     8
172600970529TISI30R- 9                                                                     9
172700970529TISI30R- 10                                                                   10
172800970529TISI30R- 11                                                                   11
172900970529TISI30R- 12                                                                   12
173000970529TISI30R- 13                                                                   13
173100970529TISI30R- 14                                                                   14
173200970529TISI30R- 15                                                                   15
173300970529TISI30R- 16                                                                   16
173400970529TISI30R- 17                                                                   17
173500970529TISI30R- 18                                                                   18
173600970529TISI30R- 19                                                                   19
173700970529TISI30R- 20                                                                   20
173800970526** OPD - OPZIONI
173900970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
174000970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
174100970611 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
174200970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
174300970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
174400970526                   -6 -     (S/N)=N                        opz. 06¬
174500970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
174600970527                   -8 -     (S/N)=N                        opz. 08¬
174700970527                   -9 -     (S/N)=N                        opz. 09¬
174800970527                   -10-     (S/N)=N                        opz. 10¬
174900970529                   -11-     (S/N)=N                        opz. 11¬
175000970529                   -12-     (S/N)=N                        opz. 12¬
17510097062613=Sinonimi        -13-     (S/N)=N                        opz. 13¬
17520006040314=Cal.Eventi Part -14-     (S/N)=N                        opz. 14¬
17530006040315=InserisciEvePart-15-     (S/N)=N                        opz. 15¬
175400970526                   -  -     (S/N)=N                        opz. 16¬
175500970526                   -  -     (S/N)=N                        opz. 17¬
175600970526                   -  -     (S/N)=N                        opz. 18¬
175700970526                   -  -     (S/N)=N                        opz. 19¬
175800970526                   -  -     (S/N)=N                        opz. 20¬
175900970526                   -  -     (S/N)=N                        opz. 21¬
176000970526                   -  -     (S/N)=N                        opz. 22¬
176100970526                   -  -     (S/N)=N                        opz. 23¬
176200970526                   -  -     (S/N)=N                        opz. 24¬
176300970526** FUD - FUNZIONI
176400970526                   -        (S/N)=N                        funz.01¬
176500970526                   -        (S/N)=N                        funz.02¬
176600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
176700970526                   -        (S/N)=N                        funz.04¬
176800970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
176900970526F06=Inserimento    -        (S/N)=N                        funz.06¬
177000970526                   -        (S/N)=N                        funz.07¬
177100970527                   -        (S/N)=N                        funz.08¬
177200970527                   -        (S/N)=N                        funz.09¬
177300970527                   -        (S/N)=N                        funz.10¬
177400970526                   -        (S/N)=N                        funz.11¬
177500970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
177600970626F13=Sinonimi       -        (S/N)=N                        funz.13¬
177700060403F14=Cal.Eventi Part-        (S/N)=N                        funz.14¬
177800970526                   -        (S/N)=N                        funz.15¬
177900970526                   -        (S/N)=N                        funz.16¬
178000970526                   -        (S/N)=N                        funz.17¬
178100970526                   -        (S/N)=N                        funz.18¬
178200970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
178300970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
178400970527                   -        (S/N)=N                        funz.21¬
178500970527                   -        (S/N)=N                        funz.22¬
178600970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
178700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
