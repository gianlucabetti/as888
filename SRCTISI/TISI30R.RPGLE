000100970526     H*------------------------------------------------------------------------*
000200970529     H* CAPPARIO - GESTIONE LOCALITA' - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800170116     fazcpr01l  IF   E           K DISK                                         *versioni cappario
000900970529     FTISI30D   CF   E             WORKSTN INFDS(DEVDS1)
001000970529     F                                     SFILE(SI30D05:SFLULT)
001100970529     F                                     SFILE(SI30DMS:MSGULT)
001200970526     F*------------
001300970529     F* LOCALITA'
001400970526     F*------------
001500970529     FAZCPL01L  IF   E           K DISK
001600970529     FAZCPL02L  IF   E           K DISK
001700970529     F                                     RENAME(AZCPL000:AZCPL2)
001800970529     F*------------
001900970529     F* CAPPARIO
002000970529     F*------------
002100970529     FAZCPC01L  IF   E           K DISK
002200130823
002300130823     FAZcos01L  IF   E           K DISK
002400970526     D*------------------------------------------------------------------------*
002500970526     D* SCHIERE
002600970526     D*------------------------------------------------------------------------*
002700970526     D*------------
002800970526     D* DECODIFICHE
002900970526     D*------------
003000970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* MESSAGGI
003300970526     D*------------
003400970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* DESCRIZIONI OPZIONI
003700970526     D*------------
003800970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* DESCRIZIONI TASTI FUNZIONALI
004100970526     D*------------
004200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004300970526     D*------------
004400970526     D* OPZIONI DA CARICARE A VIDEO
004500970526     D*------------
004600970526     D OPV             S             19    DIM(24)
004700970526     D*------------
004800970526     D* FUNZIONI DA CARICARE A VIDEO
004900970526     D*------------
005000970527     D FUV             S             19    DIM(24)
005100131028     d* Filiali abilitate  agli orari servizi
005200131028     D FIL_AB          S              3    DIM(85)
005300970526     D*------------------------------------------------------------------------*
005400970526     D* INPUT
005500970526     D*------------------------------------------------------------------------*
005600970526     D*------------
005700970526     D* RIDEFINIZIONE OPZIONI VIDEO
005800970526     D*------------
005900970526     D OPDDS           DS
006000970526     D  OPDOPZ                 1      2
006100970526     D  OPDDE1                 3     19
006200970526     D  OPDOP1                21     22
006300970526     D  OPDFI0                23     34
006400970526     D  OPDATT                35     35
006500970526     D  OPDFI1                36     80
006600970526     D  OPDDES                 1     19
006700970526     D  OPDRIG                 1     80
006800970526     D*------------
006900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007000970526     D*------------
007100970526     D FUDDS           DS
007200970526     D  FUDDE1                 1      1
007300970526     D  FUDFUN                 2      3
007400970526     D  FUDDE2                 4     19
007500970526     D  FUDFI0                20     34
007600970526     D  FUDATT                35     35
007700970526     D  FUDFI1                36     80
007800970526     D  FUDDES                 1     19
007900970526     D  FUDRIG                 1     80
008000970526     D*------------
008100970526     D* DS GESTIONE TASTI FUNZIONALI
008200970526     D*------------
008300970526     D DSKEY         E DS
008400970526     D DEVDS1          DS
008500970526     D  KEY                  369    369
008600970526     D*------------
008700970526     D* DS "XMSG" - GESTIONE MESSAGGI
008800970526     D*------------
008900970526     D DSMSG         E DS
009000970526     D*------------
009100020919     D* DS REPERIMENTO DATI UTENTE
009200970526     D*------------
009300020919     D TIBS34DS      E DS                                                       *Profili utente
009400020919     D DDATIUTE      E DS                                                       *Dati utente
009500020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009600061026     D DUTE01        E DS                                                       *Abilitazioni
009700970527     D*------------------
009800970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009900970527     D*------------------
010000970527     D WLBDA8          DS                  INZ
010100970527     D  G08DAT                 1      8  0
010200970527     D  G08INV                 9     16  0
010300970527     D  G08ERR                17     17
010400970527     D  G08TGI                18     22  0
010500970526     D*------------
010600970526     D* DS DI PROCEDURA
010700970526     D*------------
010800970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010900150731     D tisi30ds      E DS
011000060404     D tisip0ds      E DS
011100131028     D tisiO0ds      E DS
011200970526     D*------------
011300970526     D* ARCHITETTURA
011400970526     D*------------
011500970526     D KPJBA         E DS
011600131028
011700131028     d dvpodeco      e ds
011800131028     d Tibs02Ds      e ds
011900060404     d
012000060404     d tipopgm         s              1
012100060404     d wdri            s                   like(Dp0dri)
012200060404     d dprv            s                   like(Dp0prv)
012300131028     d w003a           s              3
012400170116     d wabiute         s              1
012500170116     d XX              s              3  0
012600131028     d YY              s              3  0
012700131028
012800131028
012900131028      /copy gaitrasrc/srcprotopr,tibs02r
013000970526     C*------------------------------------------------------------------------*
013100970526     C* MAIN LINES
013200970526     C*------------------------------------------------------------------------*
013300970526     C*
013400970526     C* SE NON CI SONO ERRORI, PROSEGUE
013500970526IF  1C     ERRL00        IFNE      '1'
013600970526     C*
013700970526     C* CARICA IL SFILE
013800970526     C                   EXSR      POSSFL
013900970526     C*
014000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014400970526     C*
014500970526     C* REIMPOSTA ERRORE DI PROCERURA
014600970526     C                   MOVEL     '0'           ERRL00
014700970526     C*
014800970526     C* EMETTE IL SFILE
014900970526     C                   EXSR      OUTSFL
015000970526     C*
015100970526     C* GESTIONE TASTI FUNZIONALI
015200970526     C                   EXSR      FUNCON
015300970526     C*
015400970526E   2C                   ENDDO
015500970527     C*
015600970527     C* CI SONO ERRORI IN ENTRATA
015700970527X   1C                   ELSE
015800970527     C                   MOVEL     '1'           D00ERR
015900970527     C                   MOVEL     DSMSMS        D00MSG
016000970526E   1C                   ENDIF
016100970526     C* FINE PGM
016200970526     C                   SETON                                        LR
016300970526     C*------------------------------------------------------------------------*
016400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016500970526     C*------------------------------------------------------------------------*
016600970526     C     F03GES        BEGSR
016700970526     C*
016800970526     C* IMPOSTA "PREMUTO F03"
016900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017000970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F05GES        BEGSR
017700970526     C*
017800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F06GES        BEGSR
018500970526     C*
018600970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
018700020919IF  1C     DUTLPO        IFNE      'S'
018800970526     C                   MOVEL     '1'           ERRL00
018900970526     C                   MOVEL     MSG(3)        DSMSMS
019000970526E   1C                   ENDIF
019100970526     C*
019200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019300970603IF  2C     D00NAR        IFNE      *BLANKS
019400970603     C     D00CAP        ORNE      *BLANKS
019500970603     C                   MOVEL     D00NAR        DNAR
019600970603     C                   MOVEL     D00CAP        DCAP
019700970603X   2C                   ELSE
019800970603     C                   MOVEL     *BLANKS       DNAR
019900970602     C                   MOVEL     *BLANKS       DCAP
020000970603E   2C                   ENDIF
020100970602     C                   MOVEL     *BLANKS       DLOC
020200970529     C                   MOVEL     DEPOP0        WOP0
020300970529     C                   MOVEL     'F06'         WOP1
020400970529     C                   MOVEL     'TISI31R'     WPGM              8
020500970526     C                   EXSR      LANPGM                                       *LANCIA PGM
020600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
020700970526E   1C                   ENDIF
020800970526     C*
020900970526     C                   ENDSR
021000970526     C*------------------------------------------------------------------------*
021100970526     C* F12GES - GESTIONE F12 -> RITORNO
021200970526     C*------------------------------------------------------------------------*
021300970526     C     F12GES        BEGSR
021400970526     C*
021500970526     C* IMPOSTA "PREMUTO F12"
021600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021700970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
021800970526     C*
021900970526     C                   ENDSR
022000970529     C*------------------------------------------------------------------------*
022100970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
022200970529     C*------------------------------------------------------------------------*
022300970529     C     F13GES        BEGSR
022400970529     C*
022500970602     C                   MOVEL     *BLANKS       DNAR
022600970602     C                   MOVEL     *BLANKS       DCAP
022700970602     C                   MOVEL     *BLANKS       DLOC
022800970529     C                   MOVEL     DEPOP0        WOP0
022900970529     C                   MOVEL     'S'           WOP0
023000970529     C                   MOVEL     'F13'         WOP1
023100970529     C                   MOVEL     'TISI35R'     WPGM              8
023200970529     C                   EXSR      LANPGM
023300970529     C*
023400970529     C                   ENDSR
023500060404     C*------------------------------------------------------------------------*
023600060404     C* F14GES - GESTIONE F14 -> calendario eventi particolari
023700060404     C*------------------------------------------------------------------------*
023800060404     C     F14GES        BEGSR
023900060404     C*
024000060404     C                   MOVEL     *BLANKS       DNAR
024100060404     C                   MOVEL     *BLANKS       DCAP
024200060404     C                   MOVEL     *BLANKS       Dloc
024300060404     C                   MOVEL     *BLANKS       Dprv
024400060404     C                   MOVEL     DEPOP0        WOP0
024500060404     c                   clear                   wop1
024600060404     C                   MOVEL     datcor        wdri
024700060404     C                   MOVEL     'TISIP0R'     WPGM              8
024800060404     C                   EXSR      LANPGM
024900060404     C*
025000060404     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
025300970526     C*------------------------------------------------------------------------*
025400970526     C     F23GES        BEGSR
025500970526     C*
025600970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
025700970526     C*
025800970526     C                   ENDSR
025900970526     C*------------------------------------------------------------------------*
026000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
026100970526     C*------------------------------------------------------------------------*
026200970526     C     F24GES        BEGSR
026300970526     C*
026400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026500970526     C*
026600970526     C                   ENDSR
026700970526     C*------------------------------------------------------------------------*
026800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
026900970526     C*------------------------------------------------------------------------*
027000970526     C     RUPGES        BEGSR
027100970526     C*
027200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
027300970526     C*
027400970526     C                   ENDSR
027500970526     C*------------------------------------------------------------------------*
027600970526     C* ENTGES - GESTIONE ENTER -> INVIO
027700970526     C*------------------------------------------------------------------------*
027800970526     C     ENTGES        BEGSR
027900970526     C*
028000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
028100970526     C                   EXSR      NEWPOS
028200970526     C*
028300970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
028400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028500970526IF  2C     SWPOSI        IFNE      0
028600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
028700970526X   2C                   ELSE
028800970526IF  3C     SFLULT        IFNE      0
028900970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
029000970526     C*
029100970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
029200970526IF  4C     SWMODI        IFNE      0
029300970526     C                   EXSR      POSSFL
029400970526E   4C                   ENDIF
029500970526E   3C                   ENDIF
029600970526E   2C                   ENDIF
029700970526E   1C                   ENDIF
029800970526     C*
029900970526     C                   ENDSR
030000970526     C*------------------------------------------------------------------------*
030100970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
030200970526     C*------------------------------------------------------------------------*
030300970526     C     O01GES        BEGSR
030400970526     C*
030500970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
030600970527     C                   MOVEL     VIDNAR        D00NAR
030700970527     C                   MOVEL     VIDCAP        D00CAP
030800970529     C                   MOVEL     VIDLOC        D00LOC
030900970702     C                   MOVEL     VIDCPR        D00PRV
031000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031100970526     C*
031200970526     C                   ENDSR
031300970526     C*------------------------------------------------------------------------*
031400970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
031500970526     C*------------------------------------------------------------------------*
031600970526     C     O02GES        BEGSR
031700970526     C*
031800970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
031900970526IF  1C     VIDATB        IFEQ      'A'
032000970526     C                   MOVEL     '1'           ERRL00
032100970526     C                   MOVEL     MSG(4)        DSMSMS
032200970526E   1C                   ENDIF
032300970527     C* MODIFICA
032400970704IF  1C     ERRL00        IFEQ      '0'
032500970602     C                   MOVEL     VIDNAR        DNAR
032600970602     C                   MOVEL     VIDCAP        DCAP
032700970602     C                   MOVEL     VIDLOC        DLOC
032800970529     C                   MOVEL     DEPOP0        WOP0
032900970526     C                   MOVEL     'O02'         WOP1
033000970529     C                   MOVEL     'TISI31R'     WPGM
033100970526     C                   EXSR      LANPGM
033200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
033300970528E   1C                   ENDIF
033400970526     C*
033500970526     C                   ENDSR
033600970611     C*------------------------------------------------------------------------*
033700970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
033800970611     C*------------------------------------------------------------------------*
033900970611     C     O03GES        BEGSR
034000970611     C*
034100970611     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
034200970611IF  1C     VIDATB        IFEQ      'A'
034300970611     C                   MOVEL     '1'           ERRL00
034400970611     C                   MOVEL     MSG(4)        DSMSMS
034500970611E   1C                   ENDIF
034600970611     C* DUPLICA
034700970611IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034800970611     C                   MOVEL     VIDNAR        DNAR
034900970611     C                   MOVEL     VIDCAP        DCAP
035000970611     C                   MOVEL     VIDLOC        DLOC
035100970611     C                   MOVEL     DEPOP0        WOP0
035200970611     C                   MOVEL     'O03'         WOP1
035300970611     C                   MOVEL     'TISI32R'     WPGM
035400970611     C                   EXSR      LANPGM
035500970611     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
035600970611E   1C                   ENDIF
035700970611     C*
035800970611     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
036100970526     C*------------------------------------------------------------------------*
036200970526     C     O04GES        BEGSR
036300970526     C*
036400970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
036500970526IF  1C     VIDATB        IFEQ      'A'
036600970526     C                   MOVEL     '1'           ERRL00
036700970526     C                   MOVEL     MSG(4)        DSMSMS
036800970526E   1C                   ENDIF
036900970527     C* ANNULLAMENTO
037000970704IF  1C     ERRL00        IFEQ      '0'
037100970602     C                   MOVEL     VIDNAR        DNAR
037200970602     C                   MOVEL     VIDCAP        DCAP
037300970602     C                   MOVEL     VIDLOC        DLOC
037400970529     C                   MOVEL     DEPOP0        WOP0
037500970526     C                   MOVEL     'O04'         WOP1
037600970529     C                   MOVEL     'TISI31R'     WPGM
037700970526     C                   EXSR      LANPGM
037800970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
037900970528E   1C                   ENDIF
038000970526     C*
038100970526     C                   ENDSR
038200970526     C*------------------------------------------------------------------------*
038300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
038400970526     C*------------------------------------------------------------------------*
038500970526     C     O05GES        BEGSR
038600970526     C*
038700970602     C                   MOVEL     VIDNAR        DNAR
038800970602     C                   MOVEL     VIDCAP        DCAP
038900970602     C                   MOVEL     VIDLOC        DLOC
039000970529     C                   MOVEL     DEPOP0        WOP0
039100970526     C                   MOVEL     'O05'         WOP1
039200970529     C                   MOVEL     'TISI31R'     WPGM
039300970526     C                   EXSR      LANPGM
039400970526     C*
039500970526     C                   ENDSR
039600131028     C*------------------------------------------------------------------------*
039700131028     C* O07GES - GESTIONE OPZIONE 7=Orari Servizi
039800131028     C*------------------------------------------------------------------------*
039900131028     C     O07GES        BEGSR
040000131028     C*
040100131028     C                   MOVEL     VIDNAR        DNAR
040200131028     C                   MOVEL     VIDCAP        DCAP
040300131028     C                   MOVEL     VIDLOC        DLOC
040400131028     C                   MOVEL     'TISIO2R'     WPGM
040500131028     C                   EXSR      LANPGM
040600131028     C*
040700131028     c                   endsr
040800970526     C*------------------------------------------------------------------------*
040900970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
041000970526     C*------------------------------------------------------------------------*
041100131028     C**   O07GES        BEGSR
041200970526     C*
041300970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
041400131028IF  1C**   VIDATB        IFNE      'A'
041500131028     C**                 MOVEL     '1'           ERRL00
041600131028     C**                 MOVEL     MSG(5)        DSMSMS
041700131028E   1C**                 ENDIF
041800970526     C* RIPRISITINO
041900131028IF  1C**   ERRL00        IFEQ      '0'                                          *NO ERRORI
042000131028     C**                 MOVEL     VIDNAR        DNAR
042100131028     C**                 MOVEL     VIDCAP        DCAP
042200131028     C**                 MOVEL     VIDLOC        DLOC
042300131028     C**                 MOVEL     DEPOP0        WOP0
042400131028     C**                 MOVEL     'O07'         WOP1
042500131028     C**                 MOVEL     'TISI31R'     WPGM
042600131028     C**                 EXSR      LANPGM
042700970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
042800131028E   1C**                 ENDIF
042900970526     C*
043000131028     C**                 ENDSR
043100970529     C*------------------------------------------------------------------------*
043200970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
043300970529     C*------------------------------------------------------------------------*
043400970529     C     O13GES        BEGSR
043500970529     C*
043600970602     C                   MOVEL     VIDNAR        DNAR
043700970602     C                   MOVEL     VIDCAP        DCAP
043800970602     C                   MOVEL     VIDLOC        DLOC
043900970529     C                   MOVEL     DEPOP0        WOP0
044000970529     C                   MOVEL     'S'           WOP0
044100970529     C                   MOVEL     'O13'         WOP1
044200970529     C                   MOVEL     'TISI35R'     WPGM
044300970529     C                   EXSR      LANPGM
044400970529     C*
044500970529     C                   ENDSR
044600060404     C*------------------------------------------------------------------------*
044700060404     C* O14GES - GESTIONE calendario eventi particolari
044800060404     C*------------------------------------------------------------------------*
044900060404     C     O14GES        BEGSR
045000060404     C*
045100060404     C                   MOVEL     VIDNAR        DNAR
045200060404     C                   MOVEL     VIDCAP        DCAP
045300060404     c                   clear                   dloc
045400060404     c                   clear                   dprv
045500060404     C                   MOVEL     DEPOP0        WOP0
045600060404     C                   clear                   WOP1
045700060404     C                   MOVEL     datcor        wdri
045800060404     c                   move      0101          wdri
045900060404     C                   MOVEL     'TISIP0R'     WPGM
046000060404     C                   EXSR      LANPGM
046100060404     C*
046200060404     C                   ENDSR
046300060404     C*------------------------------------------------------------------------*
046400060404     C* O15GES - Inserisci  evento particolare
046500060404     C*------------------------------------------------------------------------*
046600060404     C     O15GES        BEGSR
046700060404     C*
046800060404     C                   MOVEL     VIDNAR        DNAR
046900060404     C                   MOVEL     VIDCAP        DCAP
047000060404     C                   MOVEL     VIDcpr        Dprv
047100060404     C                   MOVEL     VIDLOC        DLOC
047200060404     C                   MOVEL     DEPOP0        WOP0
047300060404     C                   movel     'F06'         WOP1
047400060404     C                   MOVEL     datcor        wdri
047500060404     C                   MOVEL     'TISIP1R'     WPGM
047600060404     C                   EXSR      LANPGM
047700060404     C*
047800060404     C                   ENDSR
047900970526     C*------------------------------------------------------------------------*
048000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
048100970526     C*------------------------------------------------------------------------*
048200970526     C     NEWPOS        BEGSR
048300970526     C*
048400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
048500970526     C*         1= RICHIESTO POSIZIONAMENTO
048600970526     C                   Z-ADD     0             SWPOSI            7 0
048700970526     C*
048800970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
048900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
049000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
049100970526     C                   Z-ADD     0             SWMODI            1 0
049200970530     C*
049300970530     C* REIMPOSTA INDICATORI DI ERRORE
049400970530     C                   SETOFF                                       808182
049500970526     C*
049600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
049700970527IF  1C     VIDPNA        IFNE      DEPNAR
049800970527     C     VIDPCA        ORNE      DEPCAP
049900120515     C     VIDPPR        ORNE      DEPPPR
050000970529     C     VIDPLO        ORNE      DEPLOC
050100970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
050200970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
050300970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
050400970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
050500970527     C                   MOVEL     VIDPCA        DEPCAP
050600120515     C                   MOVEL     VIDPPR        DEPPPR
050700970529     C                   MOVEL     VIDPLO        DEPLOC
050800970526E   2C                   ENDIF
050900970526E   1C                   ENDIF
051000970526     C*
051100970526     C                   ENDSR
051200970526     C*------------------------------------------------------------------------*
051300970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
051400970526     C*------------------------------------------------------------------------*
051500970526     C     SETFIL        BEGSR
051600970529     C*
051700970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
051800970529IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
051900970529     C                   Z-ADD     D00VER        KCLVER
052000970529     C                   MOVEL     VIDPNA        KCLNAR
052100970529     C                   MOVEL     VIDPCA        KCLCAP
052200970529     C     KEYCPL        SETLL     AZCPL01L                           99
052300970529E   1C                   ENDIF
052400970529IF  1C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
052500970529     C                   Z-ADD     D00VER        KCLVER
052600970605     C                   MOVEL     VIDPNA        KCLNAR
052700970529     C                   MOVEL     VIDPLO        KCLLOC
052800970529     C     KE2CPL        SETLL     AZCPL02L                           99
052900970529E   1C                   ENDIF
053000970526     C* LETTURA RECORD
053100970526IF  1C     *IN99         IFEQ      *OFF
053200970526     C                   EXSR      LETFIL
053300970526E   1C                   ENDIF
053400970526     C*
053500970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
053600970526IF  1C     *IN99         IFEQ      *ON
053700970526     C                   Z-ADD     1             SFLEOF
053800970526E   1C                   ENDIF
053900970526     C*
054000970526     C                   ENDSR
054100970526     C*------------------------------------------------------------------------*
054200970526     C* LETFIL - LETTURA RECORD
054300970526     C*------------------------------------------------------------------------*
054400970526     C     LETFIL        BEGSR
054500970526     C*
054600970526     C* LEGGE FINO A:
054700970526     C*     - FINE FILE
054800970526     C*     - TROVATO RECORD VALIDO
054900970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
055000970526DO  1C     *IN99         DOWEQ     *OFF
055100970526     C     WRECOK        ANDEQ     'N'
055200970529     C*
055300970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
055400970529IF  2C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
055500120515     C* CAP è un parametro di selezione, per cui si devono leggere solo i rcd con quei CAP
055600120515     C* (CAP è in chiave)
055700970603     C     KCLVER        READE     AZCPL01L                               99
055800970529E   2C                   ENDIF
055900970529IF  2C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
056000120515     C* Località è un parametro di inizio, per cui si devono leggere i rcd a partire da quella
056100120515     C* Località (Località NON è in chiave)
056200970603     C     KCLVER        READE     AZCPL02L                               99
056300970529E   2C                   ENDIF
056400970526     C*
056500970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
056600970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
056700970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
056800970530X   2C                   ELSE
056900970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
057000970530E   2C                   ENDIF
057100970526E   1C                   ENDDO
057200970526     C*
057300970526     C                   ENDSR
057400970526     C*------------------------------------------------------------------------*
057500970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
057600970526     C*------------------------------------------------------------------------*
057700970526     C     CHKREC        BEGSR
057800970526     C*
057900970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
058000970530     C*
058100970530     C* SE RICHIESTO ORDINAMENTO PER CAP, ESCLUDE LE LOCALITA' NON DEL CAP
058200970530IF  1C     VIDPNA        IFNE      *BLANKS
058300970530     C     VIDPCA        ORNE      *BLANKS
058400970530IF  2C     VIDPNA        IFNE      CPLNAR
058500970530     C     VIDPCA        ORNE      CPLCAP
058600970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
058700970530E   2C                   ENDIF
058800970530E   1C                   ENDIF
058900001005     C*
059000001005     C* LEGGE IL CAP DELLA LOCALIÀ'
059100001005IF  1C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
059200001005     C                   Z-ADD     CPLVER        KCCVER
059300001005     C                   MOVEL     CPLNAR        KCCNAR
059400001005     C                   MOVEL     CPLCAP        KCCCAP
059500001005     C     KEYCPC        CHAIN     AZCPC01L                           98
059600001005IF  2C     *IN98         IFEQ      *ON                                          *NON TROVATO
059700001005     C                   CLEAR                   AZCPC000                       *AZZERA CAMPI CAP
059800001005E   2C                   ENDIF
059900001005E   1C                   ENDIF
060000001005     C*
060100120515     C* se è stata scelta la Selezione per Provincia
060200120515IF  1C                   IF        WRecOK = 'S' and                             *RECORD VALIDO
060300120515     C                             VIDPPR <> *blank
060400120515     C* controllo che la provincia della località sia quella da selezionare
060500120515     C                             and VIDPPR <> CPCPrv
060600120515     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
060700120515E   2C                   ENDIF
060800120515     C*
060900120515     C* SE L'UTENTE E' IN SCELTA, ESCLUDE LE LOCALITÀ OBSOLETE
061000001005IF  1C     DEPOP0        IFEQ      'L01'
061100061010IF  2C***  CPLLIV        IFEQ      'L'
061200061010     C***  CPLFI1        ANDNE     *BLANKS
061300061010     C***  CPLLIV        OREQ      'C'
061400061010     C***  CPCFI1        ANDNE     *BLANKS
061500061010     c* escludere sempre a prescindere dal livello gestione
061600061010     c     cplfi1        ifne      *blanks
061700001005     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
061800001005E   2C                   ENDIF
061900001005E   1C                   ENDIF
062000001005     C*
062100970526     C                   ENDSR
062200970526     C*------------------------------------------------------------------------*
062300970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
062400970526     C*------------------------------------------------------------------------*
062500970526     C     IRESFL        BEGSR
062600970529     C*
062700970529     C                   MOVEL     *BLANKS       OPZKEY
062800970925     C     CPLATB        IFEQ      *BLANKS
062900970925     C                   MOVEL     CPLFI1        VIDATB
063000970925     C                   ELSE
063100970529     C                   MOVEL     CPLATB        VIDATB
063200970925     C                   ENDIF
063300970529     C                   MOVEL     CPLNAR        VIDNAR
063400970529     C                   MOVEL     CPLCAP        VIDCAP
063500970529     C                   MOVEL     CPLLOC        VIDLOC
063600970530     C                   MOVEL     CPLLIV        VIDLIV
063700970529     C                   Z-ADD     CPLTTC        VIDTTC
063800970529     C                   MOVEL     CPLISO        VIDISO
063900970529     C                   Z-ADD     CPLLNA        VIDLNA
064000970529     C                   Z-ADD     CPLZNC        VIDZNC
064100970529     C                   MOVEL     CPLDLV        VIDDLV
064200970529     C                   MOVEL     *BLANKS       VIDESS
064300970529IF  1C     CPLESS        IFEQ      'S'
064400970529     C                   MOVEL     'S'           VIDESS                         *ESISTONO SINONIMI
064500970529E   1C                   ENDIF
064600130823     c* se presente un orario a livello della località --> metto H
064700130823     c     kcos          chain     azcos01l
064800130823     c                   if        %found(azcos01l)
064900130823     C                   MOVE      'H'           VIDESS                         *ESISTONO orari serv
065000130823E   1C                   ENDIF
065100001005     C* DATI DAL CAP
065200970530     C                   MOVEL     CPCPRV        VIDCPR
065300970530     C                   Z-ADD     CPCLNA        VIHCLA
065400001005IF  1C     CPLLIV        IFEQ      'C'                                          *LIV.GESTIONE 'CAP'
065500970529     C                   Z-ADD     CPCTTC        VIDTTC
065600970529     C                   MOVEL     CPCISO        VIDISO
065700970529     C                   Z-ADD     CPCLNA        VIDLNA
065800970529     C                   Z-ADD     CPCZNC        VIDZNC
065900001005E   1C                   ENDIF
066000970526     C*
066100970526     C                   ENDSR
066200970526     C*------------------------------------------------------------------------*
066300970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
066400970526     C*------------------------------------------------------------------------*
066500970526     C     CHKPAR        BEGSR
066600970529     C*
066700970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
066800970529IF  1C     VIDPCA        IFEQ      *BLANKS
066900970529     C     VIDPLO        ANDEQ     *BLANKS
067000970529     C                   MOVEL     'CAP'         WTIPRI            3
067100970529E   1C                   ENDIF
067200970529     C*
067300970529     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
067400970529IF  1C     ERRL00        IFEQ      '0'
067500120515     C* avendo fatto diventare in data 15/05/12 il CAP un dato non di "Inizio da" ma "Selezione per"
067600120515     C* ed essendo rimasto solo la Località come "Inizio da",
067700120515     C* questo test non ha più senso.
067800120515IF  2C***  VIDPCA        IFNE      *BLANKS
067900120515     C***  VIDPLO        ANDNE     *BLANKS
068000120515     C***                MOVEL     MSG(6)        DSMSMS
068100120515     C***                MOVEL     '1'           ERRL00
068200120515     C***                SETON                                        808182
068300120515E   2C***                ENDIF
068400120515E   1C                   ENDIF
068500970529     C*
068600970529     C* CONTROLLA IL TIPO RICERCA
068700970529IF  1C     ERRL00        IFEQ      '0'
068800970529IF  2C     VIDPCA        IFNE      *BLANKS
068900970529     C                   MOVEL     'CAP'         WTIPRI
069000970529E   2C                   ENDIF
069100970529IF  2C     VIDPLO        IFNE      *BLANKS
069200970529     C                   MOVEL     'LOC'         WTIPRI
069300970529E   2C                   ENDIF
069400970529E   1C                   ENDIF
069500970526     C*
069600970526     C                   ENDSR
069700970526     C*------------------------------------------------------------------------*
069800970526     C* LANPGM - LANCIA UN PROGRAMMA
069900970526     C*------------------------------------------------------------------------*
070000970526     C     LANPGM        BEGSR
070100970526     C*
070200970526     C* IMPOSTA LA DS DI PROCEDURA
070300060404     c                   eval      tipopgm=%subst(wpgm:5:1)
070400060404     c                   if        tipopgm='P'
070500060404     c                   clear                   tisip0ds
070600060404     C                   MOVEL     DNAR          Dp0NAR                         *NAZIONE
070700060404     C                   MOVEL     DCAP          Dp0CAP                         *CAP
070800060404     C                   MOVEL     Dloc          Dp0loc                         *CAP
070900060404     C                   MOVEL     Dprv          Dp0prv                         *CAP
071000060404     C                   MOVEL     wop0          dp0OP0
071100060404     C                   MOVEL     WOP1          Dp0OP1                         *LIVELLO PGM
071200060404     c                   movel     wdri          dp0dri
071300060404     c                   call      WPGM
071400060404     c                   parm                    kpjba
071500060404     c                   parm                    tisip0ds
071600060404     C*
071700060404     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
071800060404IF  1C     Dp0F03        IFEQ      '1'                                          *PREMUTO F03
071900060404     C                   MOVEL     '1'           ERRL00                         *ERRORE
072000060404     C                   MOVEL     '1'           F03L00                         *FINE PGM
072100060404     c                   movel     dp0f03        d00f03
072200060404X   1C                   ELSE                                                   *NON PREMUTO F03
072300060404     C                   MOVEL     Dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
072400060404E   1C                   ENDIF
072500131028     c                   else
072600131028     c                   if        tipopgm='O'
072700131028     c                   clear                   tisiO0ds
072800131028     c                   if        %subst(depop0:2:2)='02'
072900131028     C                   MOVEL     'G'           do0tric
073000131028     c                   else
073100131028     C                   MOVEL     'I'           do0tric
073200131028     c                   endif
073300131028     c                   move      d00ver        do0ver
073400131028     C                   MOVEL     DNAR          DO0NAR                         *NAZIONE
073500131028     C                   MOVEL     DCAP          DO0CAP                         *CAP
073600131028     C                   MOVEL     Dloc          DO0loc                         *CAP
073700131028     c                   call      WPGM
073800131028     c                   parm                    kpjba
073900131028     c                   parm                    tisio0ds
074000131028     C*
074100131028     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
074200131028IF  1C     DO0F03        IFEQ      '1'                                          *PREMUTO F03
074300131028     C                   MOVEL     '1'           ERRL00                         *ERRORE
074400131028     C                   MOVEL     '1'           F03L00                         *FINE PGM
074500131028     c                   movel     dO0f03        d00f03
074600131028X   1C                   ELSE                                                   *NON PREMUTO F03
074700131028     C                   MOVEL     DO0MSG        DSMSMS                         *MESSAGGIO DI RITORN
074800131028E   1C                   ENDIF
074900060404     c
075000060404     c                   else
075100970529     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
075200970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
075300970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
075400970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
075500970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
075600970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
075700970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
075800970602     C                   MOVEL     DCAP          D00CAP                         *CAP
075900970602     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
076000970603     C                   MOVEL     *BLANKS       D00SIN                         *ALTRI CAMPI
076100970603     C                   Z-ADD     *ZEROS        D00TTC
076200970530     C                   MOVEL     *BLANKS       D00TSP
076300970530     C                   Z-ADD     *ZEROS        D00TFP
076400970530     C                   MOVEL     *BLANKS       D00EPA
076500970530     C                   Z-ADD     *ZEROS        D00ETP
076600970530     C                   Z-ADD     *ZEROS        D00ETA
076700970530     C                   Z-ADD     *ZEROS        D00EDD
076800970530     C                   MOVEL     *BLANKS       D00SPT
076900970530     C                   Z-ADD     *ZEROS        D00SLP
077000970530     C                   Z-ADD     *ZEROS        D00SLA
077100970530     C                   MOVEL     *BLANKS       D00STS
077200970530     C                   Z-ADD     *ZEROS        D00SDD
077300970526     C*
077400970526     C                   CALL      WPGM
077500970612     C                   PARM                    KPJBA
077600970612     C                   PARM                    DSSI00
077700970526     C*
077800970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
077900970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
078000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
078100970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
078200970526X   1C                   ELSE                                                   *NON PREMUTO F03
078300970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
078400970526E   1C                   ENDIF
078500060404E   1C                   ENDIF
078600131028E   1C                   ENDIF
078700970526     C*
078800970526     C                   ENDSR
078900970527     C*------------------------------------------------------------------------*
079000970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
079100970527     C*------------------------------------------------------------------------*
079200970527     C     CARTAB        BEGSR
079300970527     C*
079400131028      /free
079500131028        // Carico filiali di test abilitate
079600131028         if dutpou<>46  ;
079700131028
079800131028         clear TIBS02ds;
079900131028         T02Mod = 'C';
080000131028         T02sif = knsif;
080100131028         T02cod = 'VPO';
080200131028         t02tla = 'L'   ;
080300131028         T02ke1 ='DECOFI816' ;
080400131028         TNTBE_RicercaControllo  (kpjba : tibs02ds);
080500131028
080600131028         if  T02err  = *blank;
080700131028         xx= 1 ;
080800131028         yy= 1 ;
080900131028
081000131028         dow xx< 256   ;
081100131028           fil_ab(yy)= %subst(t02uni:xx:3)  ;
081200131028           yy=yy+ 1   ;
081300131028           xx=xx+ 3   ;
081400131028         enddo  ;
081500131028
081600131028         endif;
081700131028
081800131028         // se c'e' 999 tutte abilitate
081900131028         w003a='999'  ;
082000131028         if   %lookup(w003a:fil_ab) = 0  ;
082100131028         w003a=%editc(dutpou:'X')   ;
082200131028         if   %lookup(w003a:fil_ab) = 0  ;
082300131028         *in10 =*on  ;
082400131028         endif  ;
082500131028         endif  ;
082600131028
082700131028         endif  ;
082800131028
082900131028      /end-free
083000970527     C                   ENDSR
083100970526     C*------------------------------------------------------------------------*
083200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
083300970526     C*------------------------------------------------------------------------*
083400061026     C     CHKFOP        BEGSR
083500061026     C*
083600061026     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
083700061026     C                   SETON                                        93
083800061026     C*
083900170116     C                   IF        wabiute = 'C'
084000061026     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
084100061026     C                   SETOFF                                       93
084200061026     C                   ENDIF
084300170116     c
084400170116     c* Se utente abilitato alla mantenzione futura, verifico dalla versione se ancora
084500170116     c*  da decorrere
084600170116     c                   if        wabiute='V'
084700170116     c     d00ver        chain     azcpr01l
084800170116     c                   if        %found(azcpr01l) and cprdde>datcor
084900170116     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
085000170116     C                   SETOFF                                       93
085100170116     c                   else
085200170116     c                   eval      wabiute='I'
085300170116     C                   ENDIF
085400170116     C                   ENDIF
085500970626     C*
085600970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
085700970626IF  1C     DEPOP0        IFEQ      'L01'
085800970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
085900970626     C                   MOVEL     'S'           OPDATT
086000970626     C                   MOVEL     OPDRIG        OPD(1)
086100970626X   1C                   ELSE
086200970626     C* OPZIONI
086300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
086400970626     C                   MOVEL     'S'           OPDATT
086500970626     C                   MOVEL     OPDRIG        OPD(5)
086600970626     C                   MOVEL     OPD(13)       OPDRIG                         *SINONIMI
086700970626     C                   MOVEL     'S'           OPDATT
086800970626     C                   MOVEL     OPDRIG        OPD(13)
086900060404     C                   MOVEL     OPD(14)       OPDRIG                         *SINONIMI
087000060404     C                   MOVEL     'S'           OPDATT
087100060404     C                   MOVEL     OPDRIG        OPD(14)
087200970626     C* FUNZIONI
087300970626     C                   MOVEL     FUD(13)       FUDRIG                         *SINONIMI
087400970626     C                   MOVEL     'S'           FUDATT
087500970626     C                   MOVEL     FUDRIG        FUD(13)
087600060404     C                   MOVEL     FUD(14)       FUDRIG                         *SINONIMI
087700060404     C                   MOVEL     'S'           FUDATT
087800060404     C                   MOVEL     FUDRIG        FUD(14)
087900970626E   1C                   ENDIF
088000131028     c* Se utente non abilitato, non attivo opzione 7=orari servizi
088100131028     c* lascio in manutenzione
088200131028     c                   if        *in10 and  depop0 = 'L05'
088300131028     C                   MOVEL     OPD(7)        OPDRIG
088400131028     C                   MOVEL     'N'           OPDATT
088500131028     C                   MOVEL     OPDRIG        OPD(7)
088600131028     c                   endif
088700970626     C*
088800970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
088900970626IF  1C     DEPOP0        IFEQ      'L02'
089000970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
089100970626     C                   MOVEL     'S'           OPDATT
089200970626     C                   MOVEL     OPDRIG        OPD(2)
089300170120     c
089400170120     C  n93              MOVEL     OPD(3)        OPDRIG                         *DUPLICA
089500170120     C  n93              MOVEL     'S'           OPDATT
089600170120     C  n93              MOVEL     OPDRIG        OPD(3)
089700061026     C  N93              MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
089800061026     C  N93              MOVEL     'S'           OPDATT
089900061026     C  N93              MOVEL     OPDRIG        OPD(4)
090000131028     c* non essendoci più l'annullamento logico il ripristino diventa inutile
090100131028     c* quindi asterisco le seguenti specifiche e così riutilizzo l'opzione 7 per
090200131028     c* gli Orari Servizi
090300131028     C* N93              MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
090400131028     C* N93              MOVEL     'S'           OPDATT
090500131028     C* N93              MOVEL     OPDRIG        OPD(7)
090600060404     C                   MOVEL     OPD(15)       OPDRIG                         *RIPRISTINO
090700060404     C                   MOVEL     'S'           OPDATT
090800060404     C                   MOVEL     OPDRIG        OPD(15)
090900061016     C* CONSENTITO INSERIMENTO SOLO A PERSONALE "EDP"
091000061026     C                   IF        %subst(KNMUS:1:3) = 'EDP' AND *IN93 = *off
091100970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
091200970626     C                   MOVEL     'S'           FUDATT
091300970626     C                   MOVEL     FUDRIG        FUD(6)
091400061016     C                   ENDIF
091500970626E   1C                   ENDIF
091600970626     C*
091700970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
091800060404IF  1C**   DUTLPO        IFNE      'S'                                          *FILIALE
091900060404     C**                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
092000060404     C**                 MOVEL     'N'           OPDATT
092100060404     C**                 MOVEL     OPDRIG        OPD(4)
092200060404     C**                 MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
092300060404     C**                 MOVEL     'N'           OPDATT
092400060404     C**                 MOVEL     OPDRIG        OPD(7)
092500060404     C**                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
092600060404     C**                 MOVEL     'N'           FUDATT
092700060404     C**                 MOVEL     FUDRIG        FUD(6)
092800060404E   1C*+                 ENDIF
092900970526     C*
093000970526     C* GESTIONE OPZIONI A VIDEO
093100970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
093200970526     C                   EXSR      OPZGES
093300970526     C*
093400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
093500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
093600970526     C                   EXSR      FUNGES
093700970526     C*
093800970526     C                   ENDSR
093900020919     C*--------------------------------------------------------------------------------------------*
094000020919     C* REPERISCE I DATI UTENTE
094100020919     C*--------------------------------------------------------------------------------------------*
094200020919     C     REPDATIUTE    BEGSR
094300020919     C*
094400020919     C* INIZIALIZZA VARIABILI DI WRK
094500020919     C                   CLEAR                   TIBS34DS
094600020919     C                   CLEAR                   AZUTEDS
094700020919     C                   CLEAR                   DDATIUTE
094800020919     C*
094900020919     C     *DTAARA       DEFINE    §azute        azuteds
095000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
095100020919     C                   IN(E)     *DTAARA
095200020919if  1C                   IF        %Error
095300020919     c                   EVAL      I34Tla = 'L'
095400020919     C                   CALL      'TIBS34R'
095500020919     C                   PARM                    Tibs34Ds
095600020919     C                   IN        *DTAARA
095700020919e   1C                   ENDIF
095800020919     C*
095900020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
096000020919     C                   MOVEL(P)  RSUT          DSFIRS           20
096100061026     C*
096200061026     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
096300061026     C                   EVAL      DUTE01 = UTEFAF
096400020919     C*
096500020919     C                   ENDSR
096600970526     C*------------------------------------------------------------------------*
096700970526     C* *INZSR - OPERAZIONI INIZIALI
096800970526     C*------------------------------------------------------------------------*
096900970526     C     *INZSR        BEGSR
097000970526     C*---
097100970526     C* RICEVIMENTO PARAMETRI
097200970526     C*---
097300970526     C     *ENTRY        PLIST
097400970612     C                   PARM                    KPJBA
097500970612     C                   PARM                    DSSI00
097600150731     C                   PARM                    tisi30ds
097700970526     C*
097800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
097900970526     C     XMSGPA        PLIST
098000970526     C                   PARM                    DSMSMS
098100970526     C                   PARM                    DSMSPG
098200970526     C                   PARM                    DSMSKE
098300970526     C                   PARM                    DSMSER
098400970526     C*
098500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
098600970526     C                   CALL      'XKEY'
098700970526     C                   PARM                    DSKEY
098800970526     C*---
098900970526     C* VARIABILI NON RIFERITE AL DATA BASE
099000970526     C*---
099100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
099200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
099300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
099400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
099500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
099600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
099700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
099800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
099900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
100000970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
100100970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
100200970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
100300970529     C     *LIKE         DEFINE    D00OP1        WOP1
100400970602     C     *LIKE         DEFINE    D00NAR        DNAR
100500970602     C     *LIKE         DEFINE    D00CAP        DCAP
100600970602     C     *LIKE         DEFINE    D00LOC        DLOC
100700970526     C*---
100800970526     C* VARIABILI DI CONTROLLO
100900970526     C*---
101000970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
101100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
101200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
101300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
101400970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
101500970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
101600970526     C*
101700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
101800970529     C                   MOVEL     'TISI30R'     DSMSPG
101900970529     C                   MOVEL     'TISI30R'     MSGQUE
102000970526     C                   MOVEL     *BLANKS       MSGKEY
102100970526     C                   MOVEL     *BLANKS       DSMSMS
102200970526     C                   MOVEL     *BLANKS       DSMSKE
102300970526     C                   Z-ADD     *ZEROS        DSMSER
102400970526     C*---
102500970526     C* VARIABILI RIFERITE AL DATA BASE
102600970526     C*---
102700970529     C     *LIKE         DEFINE    CPLVER        KCLVER                         *AZCPL00F
102800970529     C     *LIKE         DEFINE    CPLNAR        KCLNAR
102900970529     C     *LIKE         DEFINE    CPLCAP        KCLCAP
103000970529     C     *LIKE         DEFINE    CPLLOC        KCLLOC
103100970528     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
103200970528     C     *LIKE         DEFINE    CPCNAR        KCCNAR
103300970528     C     *LIKE         DEFINE    CPCCAP        KCCCAP
103400970529     C     *LIKE         DEFINE    CPLNAR        DEPNAR                         *DEPOSITI
103500970529     C     *LIKE         DEFINE    CPLCAP        DEPCAP
103600120515     C     *LIKE         DEFINE    CPCPRV        DEPPPR
103700970529     C     *LIKE         DEFINE    CPLLOC        DEPLOC
103800970526     C*---
103900970526     C* CHIAVI DI LETTURA
104000970526     C*---
104100970529     C* LETTURA AZCPL01L - PARZIALE
104200970529     C     KEYCPL        KLIST
104300970529     C                   KFLD                    KCLVER                         *VERSIONE
104400970529     C                   KFLD                    KCLNAR                         *NAZIONE
104500970529     C                   KFLD                    KCLCAP                         *CAP
104600970529     C* LETTURA AZCPL02L - COMPLETA
104700970529     C     KE2CPL        KLIST
104800970529     C                   KFLD                    KCLVER                         *VERSIONE
104900970605     C                   KFLD                    KCLNAR                         *NAZIONE
105000970529     C                   KFLD                    KCLLOC                         *LOCALITA'
105100970528     C* LETTURA AZCPC01L - COMPLETA
105200970528     C     KEYCPC        KLIST
105300970528     C                   KFLD                    KCCVER                         *VERSIONE
105400970528     C                   KFLD                    KCCNAR                         *NAZIONE
105500970528     C                   KFLD                    KCCCAP                         *CAP
105600130823     C     KCOS          KLIST
105700130823     C                   KFLD                    cplVER                         *VERSIONE
105800130823     C                   KFLD                    cplNAR                         *NAZIONE
105900130823     C                   KFLD                    cplCAP                         *CAP
106000130823     C                   KFLD                    cplloc
106100970527     C*
106200970527     C* REPERIMENTO DATI UTENTE
106300020919     C                   EXSR      REPDATIUTE
106400170116     c* Imposto il flag della abilitazione
106500170116     c                   eval      wabiute=§utecap
106600970527     C*
106700970527     C* CARICA LE TABELLE OCCORRENTI
106800970527     C                   EXSR      CARTAB
106900150731     c*
107000150731     c                   setoff                                       01
107100150731     c***                movel     kpjbu         tisi30ds
107200150731     c                   if        %parms>2 and i30flg='S'
107300150731     c                   seton                                        01
107400150731     c                   eval      v2dnaz=i30nar
107500150731     c                   eval      V2DCAP=i30cap
107600150731     c                   eval      V2DPRD=i30prv
107700150731     c                   eval      V2DLOD=i30loc
107800150731     c                   endif
107900970526     C*
108000970526     C* DEPOSITA LE OPZIONI DI ENTRATA
108100970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
108200970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
108300970526     C*
108400970526     C* CALCOLA LA DATA CORRENTE
108500970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
108600970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
108700970526     C                   Z-ADD     WN8           G08DAT
108800970526     C                   Z-ADD     *ZEROS        G08INV
108900970526     C                   MOVEL     '0'           G08ERR
109000970526     C                   CALL      'XSRDA8'
109100970526     C                   PARM                    WLBDA8
109200970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
109300970526     C*
109400970526     C* IMPOSTA I PARAMETRI DI INGRESSO
109500970526     C                   Z-ADD     D00DRI        G08INV
109600970526     C                   Z-ADD     *ZEROS        G08DAT
109700970526     C                   MOVEL     '3'           G08ERR
109800970526     C                   CALL      'XSRDA8'
109900970526     C                   PARM                    WLBDA8
110000970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
110100970527     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
110200970527     C                   MOVEL     D00CAP        VIDPCA                         *CAP
110300970529     C                   MOVEL     D00LOC        VIDPLO                         *LOCALITA
110400970526     C                   EXSR      CHKPAR
110500970526IF  1C     ERRL00        IFEQ      '1'
110600970526     C                   MOVEL     '1'           D00ERR
110700970526     C                   MOVEL     DSMSMS        D00MSG
110800970526X   1C                   ELSE
110900970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
111000970527     C                   MOVEL     VIDPCA        DEPCAP
111100120515     C                   MOVEL     VIDPPR        DEPPPR
111200970529     C                   MOVEL     VIDPLO        DEPLOC
111300970526E   1C                   ENDIF
111400970703     C*
111500970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
111600970703IF  1C     D00VER        IFEQ      *ZEROS
111700970707     C                   Z-ADD     D00DRI        P96DRI            8 0
111800970707     C                   Z-ADD     *ZEROS        P96VER            5 0
111900970707     C                   CALL      'TISI96R'
112000970707     C                   PARM                    P96DRI
112100970707     C                   PARM                    P96VER
112200970707     C                   Z-ADD     P96VER        D00VER
112300970703E   1C                   ENDIF
112400970526     C*
112500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
112600970526     C                   EXSR      CHKFOP
112700970526     C*
112800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
112900970530IF  1C     DEPOP0        IFEQ      'L01'
113000970530     C                   MOVEL     DEC(3)        VIDING
113100970530E   1C                   ENDIF
113200970530IF  1C     DEPOP0        IFEQ      'L02'
113300970530     C                   MOVEL     DEC(1)        VIDING
113400970530E   1C                   ENDIF
113500970530IF  1C     DEPOP0        IFEQ      'L05'
113600970530     C                   MOVEL     DEC(2)        VIDING
113700970530E   1C                   ENDIF
113800970526     C*
113900970526     C                   ENDSR
114000970526     C*------------------------------------------------------------------------*
114100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
114400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
114500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
114600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
114800970526     C*------------------------------------------------------------------------*
114900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
115000970526     C*------------------------------------------------------------------------*
115100970526     C     POSSFL        BEGSR
115200970526     C*
115300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
115400970526     C                   EXSR      CLRSFL
115500970526     C*
115600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
115700970526     C                   EXSR      SETFIL
115800970526     C*
115900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
116000970526     C     SFLEOF        IFEQ      0
116100970526     C                   EXSR      CARSFL
116200970526     C                   END
116300970526     C*
116400970526     C                   ENDSR
116500970526     C*------------------------------------------------------------------------*
116600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
116700970526     C*------------------------------------------------------------------------*
116800970526     C     ENTATT        BEGSR
116900970526     C*
117000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
117100970526     C                   Z-ADD     0             OPZOK             1 0
117200970526     C*
117300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
117400970526     C                   Z-ADD     0             SFLVAR            5 0
117500970526     C*
117600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
117700970526     C                   Z-ADD     0             SFLERR
117800970526     C*
117900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
118000970526     C                   Z-ADD     SFLULT        WFLULT
118100970526     C*
118200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
118300970529     C                   READC     SI30D05                                98
118400970526     C     *IN98         DOWEQ     '0'
118500970526     C*
118600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
118700970526     C                   ADD       1             SFLVAR
118800970526     C*
118900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
119000970526     C     OPZOK         IFEQ      0
119100970526     C*
119200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
119300970526     C     OPZKEY        IFNE      *BLANKS
119400970526     C                   EXSR      OPZCON
119500970526     C                   END
119600970526     C*
119700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
119800970526     C*            2. PULIZIA SCELTA IN SFL
119900970526     C     OPZOK         IFEQ      0
120000970526     C                   MOVEL     '0'           *IN41
120100970526     C                   MOVEL     *BLANKS       OPZKEY
120200970526     C                   ELSE
120300970526     C*
120400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
120500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
120600970526     C                   MOVEL     '1'           *IN41
120700970526     C                   Z-ADD     SFLULT        SFLERR
120800970526     C                   END
120900970526     C                   ELSE
121000970526     C*
121100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
121200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
121300970526     C                   MOVEL     '1'           *IN41
121400970526     C                   END                                                    OPZOK=0
121500970526     C*
121600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
121700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
121800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
121900970526     C     OPZOK         IFNE      0
122000970526     C                   Z-ADD     SFLERR        SFLNBR
122100970526     C                   ELSE
122200970526     C                   Z-ADD     SFLULT        SFLNBR
122300970526     C                   END
122400970526     C*
122500970526     C* AGGIORNO RECORD SFL
122600970529     C                   UPDATE    SI30D05
122700970526     C*
122800970529     C                   READC     SI30D05                                98
122900970526     C                   END                                                    DO WHILE
123000970526     C*
123100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
123200970526     C                   Z-ADD     WFLULT        SFLULT
123300970526     C*
123400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
123500970526     C     SFLVAR        IFEQ      0
123600970526     C*---                 MOVEL'1'       FINL00
123700970526     C                   END
123800970526     C*
123900970526     C                   ENDSR
124000970526     C*------------------------------------------------------------------------*
124100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
124200970526     C*------------------------------------------------------------------------*
124300970526     C     CLRSFL        BEGSR
124400970526     C*
124500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
124600970526     C* (0=NON FINE FILE  1=FINE FILE)
124700970526     C                   Z-ADD     0             SFLEOF
124800970526     C*
124900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
125000970526     C                   Z-ADD     0             SFLULT
125100970526     C*
125200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
125300970526     C                   MOVEL     '1'           *IN40
125400970529     C                   WRITE     SI30D06
125500970526     C                   MOVEL     '0'           *IN40
125600970526     C*
125700970526     C                   ENDSR
125800970526     C*------------------------------------------------------------------------*
125900970526     C* CARSFL - CARICAMENTO SFL
126000970526     C*------------------------------------------------------------------------*
126100970526     C     CARSFL        BEGSR
126200970526     C*
126300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
126400970526     C     SFLULT        IFEQ      0
126500970526     C                   EXSR      SETFIL
126600970526     C                   END
126700970526     C*
126800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
126900970526     C                   Z-ADD     SFLULT        WFLULT
127000970526     C*
127100970526     C* CICLO:
127200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
127300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
127400970526     C                   Z-ADD     1             SFLRIG
127500970526     C     SFLRIG        DOWLE     SFLPAG
127600970526     C     SFLEOF        ANDEQ     0
127700970526     C*
127800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
127900970526     C                   MOVEL     *BLANKS       OPZKEY
128000970526     C*
128100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
128200970526     C                   EXSR      IRESFL
128300970526     C*
128400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
128500970526     C                   ADD       1             SFLULT
128600970526     C*
128700970526     C* SCRITTURA RECORD SFL
128800970529     C                   WRITE     SI30D05
128900970526     C                   ADD       1             SFLRIG
129000970526     C*
129100970526     C* LETTURA FILE DA CARICARE
129200970526     C                   EXSR      LETFIL
129300970526     C                   END                                                    DO WHILE
129400970526     C*
129500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
129600970526     C     WFLULT        IFNE      SFLULT
129700970526     C                   Z-ADD     WFLULT        SFLNBR
129800970526     C                   ADD       1             SFLNBR
129900970526     C                   END
130000970526     C*
130100970526     C                   ENDSR
130200970526     C*------------------------------------------------------------------------*
130300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
130400970526     C*------------------------------------------------------------------------*
130500970526     C     OUTSFL        BEGSR
130600970526     C*
130700970526     C* WRITE RECORD TESTATA VIDEO
130800970529     C                   WRITE     SI30D01
130900970526     C*
131000970526     C* VISUALIZZAZIONE MESSAGGIO
131100970526     C                   EXSR      SNDMSG
131200970526     C*
131300970526     C* WRITE RECORD OPZIONI VIDEO
131400970529     C                   WRITE     SI30D02
131500970526     C*
131600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
131700970529     C                   WRITE     SI30D03
131800970526     C*
131900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
132000970529     C                   WRITE     SI30D04
132100970526     C*
132200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
132300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
132400970526     C     SFLEOF        IFEQ      0
132500970526     C                   MOVEL     '0'           *IN42
132600970526     C                   ELSE
132700970526     C*
132800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
132900970526     C                   MOVEL     '1'           *IN42
133000970526     C                   END
133100970526     C*
133200970526     C* WRITE RECORD TASTI FUNZIONALI
133300970529     C                   WRITE     SI30D09
133400970526     C*
133500970526     C* CASO DI SFL VUOTO
133600970526     C     SFLULT        IFLE      0
133700970526     C*
133800970526     C*    - WRITE RECORD VUOTO
133900970529     C                   WRITE     SI30D0A
134000970529     C                   READ      SI30D0A                                99
134100970526     C*
134200970526     C                   ELSE
134300970526     C*
134400970526     C* CASO DI SFL CON RECORDS
134500970526     C*
134600970526     C*    - WRITE RECORD CONTROLLO SFL
134700970529     C                   WRITE     SI30D06
134800970529     C                   READ      SI30D06                                99
134900970526     C                   END
135000970526     C*
135100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
135200970529     C                   READ      SI30D03                                99
135300970526     C*
135400970526     C                   ENDSR
135500970526     C*------------------------------------------------------------------------*
135600970526     C* SNDMSG - MANDA MESSAGGIO
135700970526     C*------------------------------------------------------------------------*
135800970526     C     SNDMSG        BEGSR
135900970526     C*
136000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
136100970526     C                   MOVEL     '1'           *IN40
136200970529     C                   WRITE     SI30DMC
136300970526     C                   MOVEL     '0'           *IN40
136400970526     C*
136500970526     C* VISUALIZZAZIONE MESSAGGIO
136600970526     C                   Z-ADD     0             MSGULT
136700970526     C                   CALL      'XMSG'        XMSGPA
136800970526     C     DSMSER        IFEQ      0
136900970526     C                   MOVEL     DSMSKE        MSGKEY
137000970526     C                   END
137100970526     C                   Z-ADD     1             MSGULT
137200970529     C                   WRITE     SI30DMS                                      SFL MESS
137300970529     C                   WRITE     SI30DMC                                      CONTR MESS
137400970526     C*
137500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
137600970526     C                   MOVEL     *BLANKS       DSMSMS
137700970526     C*
137800970526     C                   ENDSR
137900970526     C*------------------------------------------------------------------------*
138000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
138100970526     C*------------------------------------------------------------------------*
138200970526     C     FUNCON        BEGSR
138300970526     C*
138400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
138500970526     C                   EXSR      FUNVAR
138600970526     C*
138700970526     C* PULISCO CAMPI DI LAVORO
138800970526     C                   Z-ADD     0             I                 5 0
138900970526     C                   Z-ADD     0             W                 5 0
139000970526     C*
139100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
139200970526     C     FUNKEY        IFNE      *BLANKS
139300970526     C     1             DO        24            I
139400970526     C                   MOVEL     FUD(I)        FUDRIG
139500970526     C*
139600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
139700970526     C     FUNKEY        IFEQ      FUDFUN
139800970526     C     FUDATT        ANDEQ     'S'
139900970526     C                   Z-ADD     I             W
140000970526     C                   END
140100970526     C                   END                                                    DO WHILE
140200970526     C*
140300970526     C* FUNZIONE NON ATTIVA (ERRORE)
140400970526     C     W             IFEQ      0
140500970526     C*
140600970526     C* EMISSIONE MESSAGGIO DI ERRORE
140700970526     C                   MOVEL     MSG(2)        DSMSMS
140800970526     C                   END                                                    W=0
140900970526     C                   END                                                    FUNKEY NE *BLANKS
141000970526     C*
141100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
141200970526     C     W             IFNE      0
141300970526     C     FUNKEY        OREQ      *BLANKS
141400970526     C*
141500970526     C     FUNKEY        IFEQ      *BLANKS
141600970526     C                   EXSR      ENTGES
141700970526     C                   END
141800970526     C     FUNKEY        IFEQ      '03'
141900970526     C                   EXSR      F03GES
142000970526     C                   END
142100970526     C     FUNKEY        IFEQ      '05'
142200970526     C                   EXSR      F05GES
142300970526     C                   END
142400970526     C     FUNKEY        IFEQ      '06'
142500970526     C                   EXSR      F06GES
142600970526     C                   END
142700970526     C     FUNKEY        IFEQ      '12'
142800970526     C                   EXSR      F12GES
142900970526     C                   END
143000970529     C     FUNKEY        IFEQ      '13'
143100970529     C                   EXSR      F13GES
143200970529     C                   END
143300060404     C     FUNKEY        IFEQ      '14'
143400060404     C                   EXSR      F14GES
143500060404     C                   END
143600970526     C     FUNKEY        IFEQ      '23'
143700970526     C                   EXSR      F23GES
143800970526     C                   END
143900970526     C     FUNKEY        IFEQ      '24'
144000970526     C                   EXSR      F24GES
144100970526     C                   END
144200970526     C     FUNKEY        IFEQ      'RU'
144300970526     C                   EXSR      RUPGES
144400970526     C                   END
144500970526     C*
144600970526     C                   END
144700970526     C*
144800970526     C                   ENDSR
144900970526     C*------------------------------------------------------------------------*
145000970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
145100970526     C*------------------------------------------------------------------------*
145200970526     C     FUNVAR        BEGSR
145300970526     C*
145400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
145500970526     C     KEY           IFEQ      ENTER
145600970526     C                   MOVEL     *BLANKS       FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      F1
145900970526     C                   MOVEL     '01'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      F2
146200970526     C                   MOVEL     '02'          FUNKEY
146300970526     C                   END
146400970526     C     KEY           IFEQ      F3
146500970526     C                   MOVEL     '03'          FUNKEY
146600970526     C                   END
146700970526     C     KEY           IFEQ      F4
146800970526     C                   MOVEL     '04'          FUNKEY
146900970526     C                   END
147000970526     C     KEY           IFEQ      F5
147100970526     C                   MOVEL     '05'          FUNKEY
147200970526     C                   END
147300970526     C     KEY           IFEQ      F6
147400970526     C                   MOVEL     '06'          FUNKEY
147500970526     C                   END
147600970526     C     KEY           IFEQ      F7
147700970526     C                   MOVEL     '07'          FUNKEY
147800970526     C                   END
147900970526     C     KEY           IFEQ      F8
148000970526     C                   MOVEL     '08'          FUNKEY
148100970526     C                   END
148200970526     C     KEY           IFEQ      F9
148300970526     C                   MOVEL     '09'          FUNKEY
148400970526     C                   END
148500970526     C     KEY           IFEQ      F10
148600970526     C                   MOVEL     '10'          FUNKEY
148700970526     C                   END
148800970526     C     KEY           IFEQ      F11
148900970526     C                   MOVEL     '11'          FUNKEY
149000970526     C                   END
149100970526     C     KEY           IFEQ      F12
149200970526     C                   MOVEL     '12'          FUNKEY
149300970526     C                   END
149400970526     C     KEY           IFEQ      F13
149500970526     C                   MOVEL     '13'          FUNKEY
149600970526     C                   END
149700970526     C     KEY           IFEQ      F14
149800970526     C                   MOVEL     '14'          FUNKEY
149900970526     C                   END
150000970526     C     KEY           IFEQ      F15
150100970526     C                   MOVEL     '15'          FUNKEY
150200970526     C                   END
150300970526     C     KEY           IFEQ      F16
150400970526     C                   MOVEL     '16'          FUNKEY
150500970526     C                   END
150600970526     C     KEY           IFEQ      F17
150700970526     C                   MOVEL     '17'          FUNKEY
150800970526     C                   END
150900970526     C     KEY           IFEQ      F18
151000970526     C                   MOVEL     '18'          FUNKEY
151100970526     C                   END
151200970526     C     KEY           IFEQ      F19
151300970526     C                   MOVEL     '19'          FUNKEY
151400970526     C                   END
151500970526     C     KEY           IFEQ      F20
151600970526     C                   MOVEL     '20'          FUNKEY
151700970526     C                   END
151800970526     C     KEY           IFEQ      F21
151900970526     C                   MOVEL     '21'          FUNKEY
152000970526     C                   END
152100970526     C     KEY           IFEQ      F22
152200970526     C                   MOVEL     '22'          FUNKEY
152300970526     C                   END
152400970526     C     KEY           IFEQ      F23
152500970526     C                   MOVEL     '23'          FUNKEY
152600970526     C                   END
152700970526     C     KEY           IFEQ      F24
152800970526     C                   MOVEL     '24'          FUNKEY
152900970526     C                   END
153000970526     C     KEY           IFEQ      ROLLUP
153100970526     C                   MOVEL     'RU'          FUNKEY
153200970526     C                   END
153300970526     C     KEY           IFEQ      HELP
153400970526     C                   MOVEL     'HP'          FUNKEY
153500970526     C                   END
153600970526     C*
153700970526     C                   ENDSR
153800970526     C*------------------------------------------------------------------------*
153900970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
154000970526     C*------------------------------------------------------------------------*
154100970526     C     FUNGES        BEGSR
154200970526     C*
154300970526     C* PULISCO CAMPI DI LAVORO
154400970526     C                   Z-ADD     0             W                 5 0
154500970526     C*
154600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
154700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
154800970526     C     1             DO        24            I
154900970526     C                   MOVEL     FUD(I)        FUDRIG
155000970526     C*
155100970526     C* PULIZIA FUNZIONI PRECEDENTI
155200970526     C                   MOVEL     *BLANKS       FUV(I)
155300970526     C*
155400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
155500970526     C* (SALTA F23 E F24)
155600970526     C     FUDATT        IFEQ      'S'
155700970526     C     FUDFUN        ANDNE     '23'
155800970526     C     FUDFUN        ANDNE     '24'
155900970526     C                   ADD       1             W
156000970526     C                   MOVEL     FUDDES        FUV(W)
156100970526     C                   END
156200970526     C                   END                                                    DO WHILE
156300970526     C*
156400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
156500970526     C* ALTRIMENTI DISATTIVO F24
156600970526     C     1             DO        24            I
156700970526     C                   MOVEL     FUD(I)        FUDRIG
156800970526     C*
156900970526     C     FUDFUN        IFEQ      '24'
157000970526     C     W             IFGT      6
157100970526     C                   MOVEL     'S'           FUDATT
157200970526     C                   ELSE
157300970526     C                   MOVEL     *BLANKS       FUDATT
157400970526     C                   END                                                    W GT 8
157500970526     C                   MOVEL     FUDRIG        FUD(I)
157600970526     C                   END
157700970526     C                   END                                                    DO WHILE
157800970526     C*
157900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
158000970526     C     W             DIV       6             I
158100970526     C                   MVR                     Z
158200970526     C*
158300970526     C     Z             IFNE      0
158400970526     C     I             ADD       1             Z
158500970526     C                   ELSE
158600970526     C                   Z-ADD     I             Z
158700970526     C                   END
158800970526     C*
158900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
159000970526     C                   ADD       1             FUNGRU
159100970526     C     FUNGRU        IFGT      Z
159200970526     C     FUNGRU        ORLT      1
159300970526     C                   Z-ADD     1             FUNGRU
159400970526     C                   END
159500970526     C*
159600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
159700970526     C     FUNGRU        IFEQ      1
159800970526     C                   MOVEL     FUV(1)        FUN001
159900970526     C                   MOVEL     FUV(2)        FUN002
160000970526     C                   MOVEL     FUV(3)        FUN003
160100970526     C                   MOVEL     FUV(4)        FUN004
160200970526     C                   MOVEL     FUV(5)        FUN005
160300970526     C                   MOVEL     FUV(6)        FUN006
160400970526     C                   END
160500970526     C*
160600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
160700970526     C     FUNGRU        IFEQ      2
160800970526     C                   MOVEL     FUV(7)        FUN001
160900970526     C                   MOVEL     FUV(8)        FUN002
161000970526     C                   MOVEL     FUV(9)        FUN003
161100970526     C                   MOVEL     FUV(10)       FUN004
161200970526     C                   MOVEL     FUV(11)       FUN005
161300970526     C                   MOVEL     FUV(12)       FUN006
161400970526     C                   END
161500970526     C*
161600970526     C* TERZO GRUPPO DI 6 FUNZIONI
161700970526     C     FUNGRU        IFEQ      3
161800970526     C                   MOVEL     FUV(13)       FUN001
161900970526     C                   MOVEL     FUV(14)       FUN002
162000970526     C                   MOVEL     FUV(15)       FUN003
162100970526     C                   MOVEL     FUV(16)       FUN004
162200970526     C                   MOVEL     FUV(17)       FUN005
162300970526     C                   MOVEL     FUV(18)       FUN006
162400970526     C                   END
162500970526     C*
162600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
162700970526     C     FUNGRU        IFEQ      4
162800970526     C                   MOVEL     FUV(19)       FUN001
162900970526     C                   MOVEL     FUV(20)       FUN002
163000970526     C                   MOVEL     FUV(21)       FUN003
163100970526     C                   MOVEL     FUV(22)       FUN004
163200970526     C                   MOVEL     *BLANKS       FUN005
163300970526     C                   MOVEL     *BLANKS       FUN006
163400970526     C                   END
163500970526     C*
163600970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
163700970526     C                   MOVEL     *BLANKS       FUN007
163800970526     C                   MOVEL     *BLANKS       FUN008
163900970526     C*
164000970526     C     1             DO        24            I
164100970526     C                   MOVEL     FUD(I)        FUDRIG
164200970526     C     FUDFUN        IFEQ      '23'
164300970526     C     FUDATT        ANDEQ     'S'
164400970526     C                   MOVEL     FUDDES        FUN007
164500970526     C                   END
164600970526     C     FUDFUN        IFEQ      '24'
164700970526     C     FUDATT        ANDEQ     'S'
164800970526     C                   MOVEL     FUDDES        FUN008
164900970526     C                   END
165000970526     C                   END                                                    DO
165100970526     C*
165200970526     C                   ENDSR
165300970526     C*------------------------------------------------------------------------*
165400970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
165500970526     C*------------------------------------------------------------------------*
165600970526     C     OPZGES        BEGSR
165700970526     C*
165800970526     C* PULISCO CAMPI DI LAVORO
165900970526     C                   Z-ADD     0             W                 5 0
166000970526     C*
166100970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
166200970526     C* E CARICO TABELLA OPZIONI ATTIVE
166300970526     C     1             DO        24            I
166400970526     C                   MOVEL     OPD(I)        OPDRIG
166500970526     C*
166600970526     C* PULIZIA OPZIONI PRECEDENTI
166700970526     C                   MOVEL     *BLANKS       OPV(I)
166800970526     C*
166900970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
167000970526     C     OPDATT        IFEQ      'S'
167100970526     C                   ADD       1             W
167200970526     C                   MOVEL     OPDDES        OPV(W)
167300970526     C                   END
167400970526     C                   END                                                    DO WHILE
167500970526     C*
167600970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
167700970526     C* ALTRIMENTI DISATTIVO F23
167800970526     C     1             DO        24            I
167900970526     C                   MOVEL     FUD(I)        FUDRIG
168000970526     C*
168100970526     C     FUDFUN        IFEQ      '23'
168200970526     C     W             IFGT      8
168300970526     C                   MOVEL     'S'           FUDATT
168400970526     C                   ELSE
168500970526     C                   MOVEL     *BLANKS       FUDATT
168600970526     C                   END                                                    W GT 8
168700970526     C                   MOVEL     FUDRIG        FUD(I)
168800970526     C                   END
168900970526     C                   END                                                    DO WHILE
169000970526     C*
169100970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
169200970526     C     W             DIV       8             I
169300970526     C                   MVR                     Z                 5 0
169400970526     C*
169500970526     C     Z             IFNE      0
169600970526     C     I             ADD       1             Z
169700970526     C                   ELSE
169800970526     C                   Z-ADD     I             Z
169900970526     C                   END
170000970526     C*
170100970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
170200970526     C                   ADD       1             OPZGRU
170300970526     C     OPZGRU        IFGT      Z
170400970526     C     OPZGRU        ORLT      1
170500970526     C                   Z-ADD     1             OPZGRU
170600970526     C                   END
170700970526     C*
170800970526     C* PRIMO GRUPPO DI 8 OPZIONI
170900970526     C     OPZGRU        IFEQ      1
171000970526     C                   MOVEL     OPV(1)        OPZ001
171100970526     C                   MOVEL     OPV(2)        OPZ002
171200970526     C                   MOVEL     OPV(3)        OPZ003
171300970526     C                   MOVEL     OPV(4)        OPZ004
171400970526     C                   MOVEL     OPV(5)        OPZ005
171500970526     C                   MOVEL     OPV(6)        OPZ006
171600970526     C                   MOVEL     OPV(7)        OPZ007
171700970526     C                   MOVEL     OPV(8)        OPZ008
171800970526     C                   END
171900970526     C*
172000970526     C* SECONDO GRUPPO DI 8 OPZIONI
172100970526     C     OPZGRU        IFEQ      2
172200970526     C                   MOVEL     OPV(9)        OPZ001
172300970526     C                   MOVEL     OPV(10)       OPZ002
172400970526     C                   MOVEL     OPV(11)       OPZ003
172500970526     C                   MOVEL     OPV(12)       OPZ004
172600970526     C                   MOVEL     OPV(13)       OPZ005
172700970526     C                   MOVEL     OPV(14)       OPZ006
172800970526     C                   MOVEL     OPV(15)       OPZ007
172900970526     C                   MOVEL     OPV(16)       OPZ008
173000970526     C                   END
173100970526     C*
173200970526     C* TERZO GRUPPO DI 8 OPZIONI
173300970526     C     OPZGRU        IFEQ      3
173400970526     C                   MOVEL     OPV(17)       OPZ001
173500970526     C                   MOVEL     OPV(18)       OPZ002
173600970526     C                   MOVEL     OPV(19)       OPZ003
173700970526     C                   MOVEL     OPV(20)       OPZ004
173800970526     C                   MOVEL     OPV(21)       OPZ005
173900970526     C                   MOVEL     OPV(22)       OPZ006
174000970526     C                   MOVEL     OPV(23)       OPZ007
174100970526     C                   MOVEL     OPV(24)       OPZ008
174200970526     C                   END
174300970526     C*
174400970526     C                   ENDSR
174500970526     C*------------------------------------------------------------------------*
174600970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
174700970526     C*------------------------------------------------------------------------*
174800970526     C     OPZCON        BEGSR
174900970526     C*
175000970526     C* PULISCO CAMPI DI LAVORO
175100970526     C                   Z-ADD     0             I                 5 0
175200970526     C                   Z-ADD     0             W                 5 0
175300970526     C*
175400970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
175500970526     C     1             DO        24            I
175600970526     C                   MOVEL     OPD(I)        OPDRIG
175700970526     C*
175800970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
175900970526     C     OPZKEY        IFEQ      OPDOPZ
176000970526     C     OPZKEY        OREQ      OPDOP1
176100970526     C*
176200970526     C     OPDATT        IFEQ      'S'
176300970526     C                   MOVEL     OPDOPZ        OPZKEY
176400970526     C                   Z-ADD     I             W
176500970526     C                   END
176600970526     C                   END
176700970526     C*
176800970526     C                   END                                                    DO WHILE
176900970526     C*
177000970526     C* OPZIONE NON ATTIVA (ERRORE)
177100970526     C     W             IFEQ      0
177200970526     C*
177300970526     C* OPZIONE NON OK
177400970526     C                   Z-ADD     1             OPZOK
177500970526     C*
177600970526     C* EMISSIONE MESSAGGIO DI ERRORE
177700970526     C                   MOVEL     MSG(1)        DSMSMS
177800970526     C                   ELSE
177900970526     C*
178000970526     C* OPZIONE ATTIVA (PROCEDI ...)
178100970526     C*
178200970526     C     OPZKEY        IFEQ      ' 1'
178300970526     C                   EXSR      O01GES
178400970526     C                   END
178500970526     C     OPZKEY        IFEQ      ' 2'
178600970526     C                   EXSR      O02GES
178700970526     C                   END
178800970611     C     OPZKEY        IFEQ      ' 3'
178900970611     C                   EXSR      O03GES
179000970611     C                   END
179100970526     C     OPZKEY        IFEQ      ' 4'
179200970526     C                   EXSR      O04GES
179300970526     C                   END
179400970526     C     OPZKEY        IFEQ      ' 5'
179500970526     C                   EXSR      O05GES
179600970526     C                   END
179700970526     C     OPZKEY        IFEQ      ' 7'
179800970526     C                   EXSR      O07GES
179900970526     C                   END
180000970529     C     OPZKEY        IFEQ      '13'
180100970529     C                   EXSR      O13GES
180200970529     C                   END
180300060404     C     OPZKEY        IFEQ      '14'
180400060404     C                   EXSR      O14GES
180500060404     C                   END
180600060404     C     OPZKEY        IFEQ      '15'
180700060404     C                   EXSR      O15GES
180800060404     C                   END
180900970526     C                   END                                                    OPZ.NON ATTIVA
181000970526     C*
181100970526     C                   ENDSR
181200970526     C*------------------------------------------------------------------------*
181300970526     C* NOTE COSTRUZIONE SCHIERE
181400970526     C*------------------------------------------------------------------------*
181500970526     C*
181600970526     C* -MSG-MESSAGGI
181700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
181800970526     C* (NON TOCCARE MSG,1 E MSG,2)
181900970526     C*
182000970526     C* -CMD-COMANDI
182100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
182200970526     C*
182300970526     C* -OPD-OPZIONI
182400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
182500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
182600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
182700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
182800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
182900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
183000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
183100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
183200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
183300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
183400970526     C*
183500970526     C* -FUD-FUNZIONI
183600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
183700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
183800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
183900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
184000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
184100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
184200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
184300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
184400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
184500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
184600970526     O*------------------------------------------------------------------------*
184700970526** DEC - DECODIFICHE
184800970526    GESTIONE                                                     1
184900970530INTERROGAZIONE                                                   2
185000970530     SCELTA                                                      3
1851009705264
1852009705265
185300970526** MSG - MESSAGGI
185400970529TISI30R- Opzione non gestita                                                   1
185500970529TISI30R- Tasto funzionale non gestito                                          2
185600970704TISI30R- 3
185700970529TISI30R- Record annullato. Consentito solo il ripristino                       4
185800970529TISI30R- Record non annullato. Impossibile ripristinare                        5
185900970529TISI30R- Accettato solo un parametro di inizio lista                           6
186000970529TISI30R- 7                                                                     7
186100970529TISI30R- 8                                                                     8
186200970529TISI30R- 9                                                                     9
186300970529TISI30R- 10                                                                   10
186400970529TISI30R- 11                                                                   11
186500970529TISI30R- 12                                                                   12
186600970529TISI30R- 13                                                                   13
186700970529TISI30R- 14                                                                   14
186800970529TISI30R- 15                                                                   15
186900970529TISI30R- 16                                                                   16
187000970529TISI30R- 17                                                                   17
187100970529TISI30R- 18                                                                   18
187200970529TISI30R- 19                                                                   19
187300970529TISI30R- 20                                                                   20
187400970526** OPD - OPZIONI
187500970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
187600970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
187700970611 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
187800970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
187900970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
188000970526                   -6 -     (S/N)=N                        opz. 06¬
188100131028 7=Orari Servizi   -7 -     (S/N)=S                        opz. 07¬
188200970527                   -8 -     (S/N)=N                        opz. 08¬
188300970527                   -9 -     (S/N)=N                        opz. 09¬
188400970527                   -10-     (S/N)=N                        opz. 10¬
188500970529                   -11-     (S/N)=N                        opz. 11¬
188600970529                   -12-     (S/N)=N                        opz. 12¬
18870097062613=Sinonimi        -13-     (S/N)=N                        opz. 13¬
18880006040314=Cal.Eventi Part -14-     (S/N)=N                        opz. 14¬
18890006040315=InserisciEvePart-15-     (S/N)=N                        opz. 15¬
189000970526                   -  -     (S/N)=N                        opz. 16¬
189100970526                   -  -     (S/N)=N                        opz. 17¬
189200970526                   -  -     (S/N)=N                        opz. 18¬
189300970526                   -  -     (S/N)=N                        opz. 19¬
189400970526                   -  -     (S/N)=N                        opz. 20¬
189500970526                   -  -     (S/N)=N                        opz. 21¬
189600970526                   -  -     (S/N)=N                        opz. 22¬
189700970526                   -  -     (S/N)=N                        opz. 23¬
189800970526                   -  -     (S/N)=N                        opz. 24¬
189900970526** FUD - FUNZIONI
190000970526                   -        (S/N)=N                        funz.01¬
190100970526                   -        (S/N)=N                        funz.02¬
190200970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
190300970526                   -        (S/N)=N                        funz.04¬
190400970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
190500970526F06=Inserimento    -        (S/N)=N                        funz.06¬
190600970526                   -        (S/N)=N                        funz.07¬
190700970527                   -        (S/N)=N                        funz.08¬
190800970527                   -        (S/N)=N                        funz.09¬
190900970527                   -        (S/N)=N                        funz.10¬
191000970526                   -        (S/N)=N                        funz.11¬
191100970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
191200970626F13=Sinonimi       -        (S/N)=N                        funz.13¬
191300060403F14=Cal.Eventi Part-        (S/N)=N                        funz.14¬
191400970526                   -        (S/N)=N                        funz.15¬
191500970526                   -        (S/N)=N                        funz.16¬
191600970526                   -        (S/N)=N                        funz.17¬
191700970526                   -        (S/N)=N                        funz.18¬
191800970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
191900970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
192000970527                   -        (S/N)=N                        funz.21¬
192100970527                   -        (S/N)=N                        funz.22¬
192200970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
192300970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
