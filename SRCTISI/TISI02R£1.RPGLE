000100970526     H*------------------------------------------------------------------------*
000200970526     H* CAPPARIO - GESTIONE CAP - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970526     FTISI02D   CF   E             WORKSTN INFDS(DEVDS1)
000900970526     F                                     SFILE(SI02D05:SFLULT)
001000970526     F                                     SFILE(SI02DMS:MSGULT)
001100970526     F*------------
001200970526     F* CAPPARIO
001300970526     F*------------
001400970526     FAZCPC01L  IF   E           K DISK
001500970526     FAZCPC02L  IF   E           K DISK
001600970526     F                                     RENAME(AZCPC000:AZCPC2)
001700970526     FAZCPC03L  IF   E           K DISK
001800970526     F                                     RENAME(AZCPC000:AZCPC3)
001900970526     D*------------------------------------------------------------------------*
002000970526     D* SCHIERE
002100970526     D*------------------------------------------------------------------------*
002200970526     D*------------
002300970526     D* DECODIFICHE
002400970526     D*------------
002500970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* MESSAGGI
002800970526     D*------------
002900970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* COMANDI
003200970526     D*------------
003300970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
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
004900970526     D FUV             S             19    DIM(24)
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
008800020916     D* DS REPERIMENTO DATI UTENTE
008900020916     D*-------------------
009000020916     D TIBS34DS      E DS                                                       *Profili utente
009100020916     D DDATIUTE      E DS                                                       *Dati utente
009200020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009300061026     D DUTE01        E DS                                                       *Abilitazioni
009400970526     D*------------------
009500970526     D* DS "XSRDAT" - CONTROLLA DATA (6)
009600970526     D*------------------
009700970526     D WLBDAT          DS                  INZ
009800970526     D  G06DAT                 1      6  0
009900970526     D  G06INV                 7     12  0
010000970526     D  G06ERR                13     13
010100970526     D  G06TGI                14     18  0
010200970526     D*------------------
010300970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
010400970526     D*------------------
010500970526     D WLBDA8          DS                  INZ
010600970526     D  G08DAT                 1      8  0
010700970526     D  G08INV                 9     16  0
010800970526     D  G08ERR                17     17
010900970526     D  G08TGI                18     22  0
011000970526     D*------------
011100970526     D* DS DI PROCEDURA
011200970526     D*------------
011300970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
011400060403     D tisip0ds      E DS
011500060927     D tisiq0ds      E DS
011600970526     D*------------
011700970526     D* ARCHITETTURA
011800970526     D*------------
011900970526     D KPJBA         E DS
012000060403     d tipopgm         s              1
012100060403     d wdri            s                   like(Dp0dri)
012200060403     d dprv            s                   like(Dp0prv)
012300970526     C*------------------------------------------------------------------------*
012400970526     C* MAIN LINES
012500970526     C*------------------------------------------------------------------------*
012600970526     C*
012700970526     C* SE NON CI SONO ERRORI, PROSEGUE
012800970526IF  1C     ERRL00        IFNE      '1'
012900970526     C*
013000970526     C* CARICA IL SFILE
013100970526     C                   EXSR      POSSFL
013200970526     C*
013300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013700970526     C*
013800970526     C* REIMPOSTA ERRORE DI PROCERURA
013900970526     C                   MOVEL     '0'           ERRL00
014000970526     C*
014100970526     C* EMETTE IL SFILE
014200970526     C                   EXSR      OUTSFL
014300970526     C*
014400970526     C* GESTIONE TASTI FUNZIONALI
014500970526     C                   EXSR      FUNCON
014600970526     C*
014700970526E   2C                   ENDDO
014800970527     C*
014900970527     C* CI SONO ERRORI IN ENTRATA
015000970527X   1C                   ELSE
015100970527     C                   MOVEL     '1'           D00ERR
015200970527     C                   MOVEL     DSMSMS        D00MSG
015300970526E   1C                   ENDIF
015400970526     C* FINE PGM
015500970526     C                   SETON                                        LR
015600970526     C*------------------------------------------------------------------------*
015700970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015800970526     C*------------------------------------------------------------------------*
015900970526     C     F03GES        BEGSR
016000970526     C*
016100970526     C* IMPOSTA "PREMUTO F03"
016200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016300970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016800970526     C*------------------------------------------------------------------------*
016900970526     C     F05GES        BEGSR
017000970526     C*
017100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017200970526     C*
017300970526     C                   ENDSR
017400970526     C*------------------------------------------------------------------------*
017500970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
017600970526     C*------------------------------------------------------------------------*
017700970526     C     F06GES        BEGSR
017800970526     C*
017900970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
018000020916IF  1C     DUTLPO        IFNE      'S'
018100970526     C                   MOVEL     '1'           ERRL00
018200970526     C                   MOVEL     MSG(3)        DSMSMS
018300970526E   1C                   ENDIF
018400970526     C*
018500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018600970602     C                   MOVEL     *BLANKS       DNAR
018700970602     C                   MOVEL     *BLANKS       DCAP
018800970529     C                   MOVEL     DEPOP0        WOP0
018900970529     C                   MOVEL     'F06'         WOP1
019000970526     C                   MOVEL     'TISI03R'     WPGM              8
019100970526     C                   EXSR      LANPGM                                       *LANCIA PGM
019200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019300970526E   1C                   ENDIF
019400970526     C*
019500970526     C                   ENDSR
019600970528     C*------------------------------------------------------------------------*
019700970528     C* F08GES - GESTIONE F08 -> GESTIONE LOCALITA'
019800970528     C*------------------------------------------------------------------------*
019900970528     C     F08GES        BEGSR
020000970528     C*
020100970602     C                   MOVEL     *BLANKS       DNAR
020200970602     C                   MOVEL     *BLANKS       DCAP
020300970528     C                   MOVEL     DEPOP0        WOP0
020400970528     C                   MOVEL     'L'           WOP0
020500970528     C                   MOVEL     'F08'         WOP1
020600970528     C                   MOVEL     'TISI30R'     WPGM              8
020700970528     C                   EXSR      LANPGM
020800970528     C*
020900970528     C                   ENDSR
021000970528     C*------------------------------------------------------------------------*
021100970528     C* F09GES - GESTIONE F09 -> ECCEZIONI LINEE/ZONE
021200970528     C*------------------------------------------------------------------------*
021300970528     C     F09GES        BEGSR
021400970528     C*
021500970602     C                   MOVEL     *BLANKS       DNAR
021600970602     C                   MOVEL     *BLANKS       DCAP
021700970528     C                   MOVEL     DEPOP0        WOP0
021800970528     C                   MOVEL     'E'           WOP0
021900970528     C                   MOVEL     'F09'         WOP1
022000970528     C                   MOVEL     'TISI11R'     WPGM              8
022100970528     C                   EXSR      LANPGM
022200970528     C*
022300970528     C                   ENDSR
022400970528     C*------------------------------------------------------------------------*
022500970528     C* F10GES - GESTIONE F10 -> TEMPI DI CONSEGNA
022600970528     C*------------------------------------------------------------------------*
022700970602     C     F10GES        BEGSR
022800970528     C*
022900970602     C                   MOVEL     *BLANKS       DNAR
023000970602     C                   MOVEL     *BLANKS       DCAP
023100970528     C                   MOVEL     DEPOP0        WOP0
023200970528     C                   MOVEL     'T'           WOP0
023300970528     C                   MOVEL     'F10'         WOP1
023400970528     C                   MOVEL     'TISI41R'     WPGM              8
023500970528     C                   EXSR      LANPGM
023600970528     C*
023700970528     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900970526     C* F12GES - GESTIONE F12 -> RITORNO
024000970526     C*------------------------------------------------------------------------*
024100970526     C     F12GES        BEGSR
024200970526     C*
024300970526     C* IMPOSTA "PREMUTO F12"
024400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024500970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
024600970526     C*
024700970526     C                   ENDSR
024800060403     C*------------------------------------------------------------------------*
024900060403     C* F14GES - GESTIONE F14 -> calendario eventi particolari
025000060403     C*------------------------------------------------------------------------*
025100060403     C     F14GES        BEGSR
025200060403     C*
025300060403     C                   MOVEL     *BLANKS       DNAR
025400060403     C                   MOVEL     *BLANKS       DCAP
025500060403     C                   MOVEL     DEPOP0        WOP0
025600060403     c                   clear                   wop1
025700060403     C                   MOVEL     datcor        wdri
025800060403     C                   MOVEL     'TISIP0R'     WPGM              8
025900060403     C                   EXSR      LANPGM
026000060403     C*
026100060403     C                   ENDSR
026200060927     C*------------------------------------------------------------------------*
026300060927     C* F15GES - GESTIONE F15 -> cappario utilizzi particolari
026400060927     C*------------------------------------------------------------------------*
026500060927     C     F15GES        BEGSR
026600060927     C*
026700060927     c                   clear                   tisiq0ds
026800060927     c                   MOVEL     'C05'         dq0op0                         *livello procedura
026900060927     c                   clear                   dq0op1                         *livello programma
027000060927     c                   MOVEL     '0'           dq0f03                         *premuto F03
027100060927     c                   MOVEL     '0'           dq0f12                         *premuto F12
027200060927     c                   MOVEL     '0'           dq0err                         *errore
027300060927     c                   call      'TISIQ0R'
027400060927     c                   parm                    kpjba
027500060927     c                   parm                    tisiQ0ds
027600060927     C*
027700060927     C                   ENDSR
027800970526     C*------------------------------------------------------------------------*
027900970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
028000970526     C*------------------------------------------------------------------------*
028100970526     C     F21GES        BEGSR
028200970612     C*
028300970612     C                   MOVEL     VIDNAR        DNAR
028400970612     C                   MOVEL     VIDCAP        DCAP
028500970612     C                   MOVEL     DEPOP0        WOP0
028600970612     C                   MOVE      '03'          WOP0
028700970612     C                   MOVEL     'F21'         WOP1
028800970612     C                   MOVEL     'TISI04R'     WPGM              8
028900970612     C                   EXSR      LANPGM
029000970526     C*
029100970526     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
029400970526     C*------------------------------------------------------------------------*
029500970526     C     F22GES        BEGSR
029600970526     C*
029700970526     C                   Z-ADD     80            LENGH            15 5
029800970526     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
029900970526     C                   PARM                    CMD(1)
030000970526     C                   PARM                    LENGH
030100970526     C*
030200970526     C                   ENDSR
030300970526     C*------------------------------------------------------------------------*
030400970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
030500970526     C*------------------------------------------------------------------------*
030600970526     C     F23GES        BEGSR
030700970526     C*
030800970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
030900970526     C*
031000970526     C                   ENDSR
031100970526     C*------------------------------------------------------------------------*
031200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
031300970526     C*------------------------------------------------------------------------*
031400970526     C     F24GES        BEGSR
031500970526     C*
031600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
031700970526     C*
031800970526     C                   ENDSR
031900970526     C*------------------------------------------------------------------------*
032000970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
032100970526     C*------------------------------------------------------------------------*
032200970526     C     RUPGES        BEGSR
032300970526     C*
032400970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
032500970526     C*
032600970526     C                   ENDSR
032700970526     C*------------------------------------------------------------------------*
032800970526     C* ENTGES - GESTIONE ENTER -> INVIO
032900970526     C*------------------------------------------------------------------------*
033000970526     C     ENTGES        BEGSR
033100970526     C*
033200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
033300970526     C                   EXSR      NEWPOS
033400970526     C*
033500970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
033600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033700970526IF  2C     SWPOSI        IFNE      0
033800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
033900970526X   2C                   ELSE
034000970526IF  3C     SFLULT        IFNE      0
034100970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
034200970526     C*
034300970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
034400970526IF  4C     SWMODI        IFNE      0
034500970526     C                   EXSR      POSSFL
034600970526E   4C                   ENDIF
034700970526E   3C                   ENDIF
034800970526E   2C                   ENDIF
034900970526E   1C                   ENDIF
035000970526     C*
035100970526     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
035400970526     C*------------------------------------------------------------------------*
035500970526     C     O01GES        BEGSR
035600970526     C*
035700970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
035800970526     C                   MOVEL     VIDNAR        D00NAR
035900970526     C                   MOVEL     VIDCAP        D00CAP
036000970704     C                   MOVEL     VIDPRV        D00PRV
036100970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036200970526     C*
036300970526     C                   ENDSR
036400970526     C*------------------------------------------------------------------------*
036500970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
036600970526     C*------------------------------------------------------------------------*
036700970526     C     O02GES        BEGSR
036800970526     C*
036900970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
037000970526IF  1C     VIDATB        IFEQ      'A'
037100970526     C                   MOVEL     '1'           ERRL00
037200970526     C                   MOVEL     MSG(4)        DSMSMS
037300970526E   1C                   ENDIF
037400970526     C* MODIFICA
037500970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037600970602     C                   MOVEL     VIDNAR        DNAR
037700970602     C                   MOVEL     VIDCAP        DCAP
037800970529     C                   MOVEL     DEPOP0        WOP0
037900970526     C                   MOVEL     'O02'         WOP1
038000970526     C                   MOVEL     'TISI03R'     WPGM
038100970526     C                   EXSR      LANPGM
038200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
038300970526E   1C                   ENDIF
038400970526     C*
038500970526     C                   ENDSR
038600970917     C*------------------------------------------------------------------------*
038700970917     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
038800970917     C*------------------------------------------------------------------------*
038900970917     C     O03GES        BEGSR
039000970917     C*
039100970917     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
039200970917IF  1C     VIDATB        IFEQ      'A'
039300970917     C                   MOVEL     '1'           ERRL00
039400970917     C                   MOVEL     MSG(4)        DSMSMS
039500970917E   1C                   ENDIF
039600970917     C* DUPLICA
039700970917IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039800970917     C                   MOVEL     VIDNAR        DNAR
039900970917     C                   MOVEL     VIDCAP        DCAP
040000970917     C                   MOVEL     DEPOP0        WOP0
040100970917     C                   MOVEL     'O03'         WOP1
040200970917     C                   MOVEL     'TISI06R'     WPGM
040300970917     C                   EXSR      LANPGM
040400970917     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
040500970917E   1C                   ENDIF
040600970917     C*
040700970917     C                   ENDSR
040800970526     C*------------------------------------------------------------------------*
040900970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
041000970526     C*------------------------------------------------------------------------*
041100970526     C     O04GES        BEGSR
041200970526     C*
041300970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
041400970526IF  1C     VIDATB        IFEQ      'A'
041500970526     C                   MOVEL     '1'           ERRL00
041600970526     C                   MOVEL     MSG(4)        DSMSMS
041700970526E   1C                   ENDIF
041800970526     C* ANNULLAMENTO
041900970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042000970602     C                   MOVEL     VIDNAR        DNAR
042100970602     C                   MOVEL     VIDCAP        DCAP
042200970529     C                   MOVEL     DEPOP0        WOP0
042300970526     C                   MOVEL     'O04'         WOP1
042400970526     C                   MOVEL     'TISI03R'     WPGM
042500970526     C                   EXSR      LANPGM
042600970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
042700970526E   1C                   ENDIF
042800970526     C*
042900970526     C                   ENDSR
043000970526     C*------------------------------------------------------------------------*
043100970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
043200970526     C*------------------------------------------------------------------------*
043300970526     C     O05GES        BEGSR
043400970526     C*
043500970602     C                   MOVEL     VIDNAR        DNAR
043600970602     C                   MOVEL     VIDCAP        DCAP
043700970529     C                   MOVEL     DEPOP0        WOP0
043800970526     C                   MOVEL     'O05'         WOP1
043900970526     C                   MOVEL     'TISI03R'     WPGM
044000970526     C                   EXSR      LANPGM
044100970526     C*
044200970526     C                   ENDSR
044300970526     C*------------------------------------------------------------------------*
044400970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
044500970526     C*------------------------------------------------------------------------*
044600970526     C     O07GES        BEGSR
044700970526     C*
044800970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
044900970526IF  1C     VIDATB        IFNE      'A'
045000970526     C                   MOVEL     '1'           ERRL00
045100970526     C                   MOVEL     MSG(5)        DSMSMS
045200970526E   1C                   ENDIF
045300970526     C* RIPRISITINO
045400970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
045500970602     C                   MOVEL     VIDNAR        DNAR
045600970602     C                   MOVEL     VIDCAP        DCAP
045700970529     C                   MOVEL     DEPOP0        WOP0
045800970526     C                   MOVEL     'O07'         WOP1
045900970526     C                   MOVEL     'TISI03R'     WPGM
046000970526     C                   EXSR      LANPGM
046100970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
046200970526E   1C                   ENDIF
046300970526     C*
046400970526     C                   ENDSR
046500970526     C*------------------------------------------------------------------------*
046600970526     C* O08GES - GESTIONE OPZIONE 8=LOCALITA'
046700970526     C*------------------------------------------------------------------------*
046800970526     C     O08GES        BEGSR
046900970528     C*
047000970602     C                   MOVEL     VIDNAR        DNAR
047100970602     C                   MOVEL     VIDCAP        DCAP
047200970528     C                   MOVEL     DEPOP0        WOP0
047300970528     C                   MOVEL     'L'           WOP0
047400970526     C                   MOVEL     'O08'         WOP1
047500970526     C                   MOVEL     'TISI30R'     WPGM
047600970526     C                   EXSR      LANPGM
047700970526     C*
047800970526     C                   ENDSR
047900970526     C*------------------------------------------------------------------------*
048000970526     C* O09GES - GESTIONE OPZIONE 9=ECCEZIONI
048100970526     C*------------------------------------------------------------------------*
048200970526     C     O09GES        BEGSR
048300970528     C*
048400970602     C                   MOVEL     VIDNAR        DNAR
048500970602     C                   MOVEL     VIDCAP        DCAP
048600970528     C                   MOVEL     DEPOP0        WOP0
048700970528     C                   MOVEL     'E'           WOP0
048800970526     C                   MOVEL     'O09'         WOP1
048900970526     C                   MOVEL     'TISI11R'     WPGM
049000970526     C                   EXSR      LANPGM
049100970526     C*
049200970526     C                   ENDSR
049300970526     C*------------------------------------------------------------------------*
049400970526     C* O10GES - GESTIONE OPZIONE 10=TEMPI DI CONSEGNA
049500970526     C*------------------------------------------------------------------------*
049600970526     C     O10GES        BEGSR
049700970526     C*
049800970602     C                   MOVEL     VIDNAR        DNAR
049900970602     C                   MOVEL     VIDCAP        DCAP
050000970528     C                   MOVEL     DEPOP0        WOP0
050100970528     C                   MOVEL     'T'           WOP0
050200970526     C                   MOVEL     'O10'         WOP1
050300970526     C                   MOVEL     'TISI41R'     WPGM
050400970526     C                   EXSR      LANPGM
050500970526     C*
050600970526     C                   ENDSR
050700060403     C*------------------------------------------------------------------------*
050800060403     C* O14GES - GESTIONE calendario eventi particolari
050900060403     C*------------------------------------------------------------------------*
051000060403     C     O14GES        BEGSR
051100060403     C*
051200060403     C                   MOVEL     VIDNAR        DNAR
051300060403     C                   MOVEL     VIDCAP        DCAP
051400060403     C                   MOVEL     DEPOP0        WOP0
051500060403     C                   clear                   WOP1
051600060403     C                   MOVEL     datcor        wdri
051700060403     c                   move      0101          wdri
051800060403     C                   MOVEL     'TISIP0R'     WPGM
051900060403     C                   EXSR      LANPGM
052000060403     C*
052100060403     C                   ENDSR
052200060403     C*------------------------------------------------------------------------*
052300060403     C* O15GES - Inserisci  evento particolare
052400060403     C*------------------------------------------------------------------------*
052500060403     C     O15GES        BEGSR
052600060403     C*
052700060403     C                   MOVEL     VIDNAR        DNAR
052800060403     C                   MOVEL     VIDCAP        DCAP
052900060403     C                   MOVEL     VIDprv        Dprv
053000060403     C                   MOVEL     DEPOP0        WOP0
053100060403     C                   movel     'F06'         WOP1
053200060403     C                   MOVEL     datcor        wdri
053300060403     C                   MOVEL     'TISIP1R'     WPGM
053400060403     C                   EXSR      LANPGM
053500060403     C*
053600060403     C                   ENDSR
053700970526     C*------------------------------------------------------------------------*
053800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
053900970526     C*------------------------------------------------------------------------*
054000970526     C     NEWPOS        BEGSR
054100970526     C*
054200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
054300970526     C*         1= RICHIESTO POSIZIONAMENTO
054400970526     C                   Z-ADD     0             SWPOSI            7 0
054500970526     C*
054600970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
054700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
054800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
054900970526     C                   Z-ADD     0             SWMODI            1 0
055000970530     C*
055100970530     C* REIMPOSTA INDICATORI DI ERRORE
055200970530     C                   SETOFF                                       808182
055300970530     C                   SETOFF                                       83
055400970526     C*
055500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
055600970526IF  1C     VIDPNA        IFNE      DEPNAR
055700970526     C     VIDPCA        ORNE      DEPCAP
055800970526     C     VIDPLN        ORNE      DEPLNA
055900970526     C     VIDPPR        ORNE      DEPPRV
056000970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
056100970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
056200970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
056300970526     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
056400970526     C                   MOVEL     VIDPCA        DEPCAP
056500970526     C                   Z-ADD     VIDPLN        DEPLNA
056600970526     C                   MOVEL     VIDPPR        DEPPRV
056700970526E   2C                   ENDIF
056800970526E   1C                   ENDIF
056900970526     C*
057000970526     C                   ENDSR
057100970526     C*------------------------------------------------------------------------*
057200970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
057300970526     C*------------------------------------------------------------------------*
057400970526     C     SETFIL        BEGSR
057500970526     C*
057600970528     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
057700970526IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
057800970526     C                   Z-ADD     D00VER        KCCVER
057900970526     C                   MOVEL     VIDPNA        KCCNAR
058000970526     C                   MOVEL     VIDPCA        KCCCAP
058100970527     C     KEYCPC        SETLL     AZCPC01L                           99
058200970526E   1C                   ENDIF
058300970526     C*
058400970526IF  1C     WTIPRI        IFEQ      'LNA'                                        *PER LINEA ARRIVO
058500970526     C                   Z-ADD     D00VER        KCCVER
058600970526     C                   MOVEL     VIDPNA        KCCNAR
058700970526     C                   Z-ADD     VIDPLN        KCCLNA
058800970527     C     KE3CPC        SETLL     AZCPC03L                           99
058900970526E   1C                   ENDIF
059000970526     C*
059100970526IF  1C     WTIPRI        IFEQ      'PRV'                                        *PER PROVINCIA
059200970526     C                   Z-ADD     D00VER        KCCVER
059300970526     C                   MOVEL     VIDPNA        KCCNAR
059400970526     C                   MOVEL     VIDPPR        KCCPRV
059500970527     C     KE2CPC        SETLL     AZCPC02L                           99
059600970526E   1C                   ENDIF
059700970526     C* LETTURA RECORD
059800970526IF  1C     *IN99         IFEQ      *OFF
059900970526     C                   EXSR      LETFIL
060000970526E   1C                   ENDIF
060100970526     C*
060200970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
060300970526IF  1C     *IN99         IFEQ      *ON
060400970526     C                   Z-ADD     1             SFLEOF
060500970526E   1C                   ENDIF
060600970526     C*
060700970526     C                   ENDSR
060800970526     C*------------------------------------------------------------------------*
060900970526     C* LETFIL - LETTURA RECORD
061000970526     C*------------------------------------------------------------------------*
061100970526     C     LETFIL        BEGSR
061200970526     C*
061300970526     C* LEGGE FINO A:
061400970526     C*     - FINE FILE
061500970526     C*     - TROVATO RECORD VALIDO
061600970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
061700970526DO  1C     *IN99         DOWEQ     *OFF
061800970526     C     WRECOK        ANDEQ     'N'
061900970526     C*
062000970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
062100970526IF  2C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
062200970603     C     KCCVER        READE     AZCPC01L                               99
062300970526E   2C                   ENDIF
062400970526IF  2C     WTIPRI        IFEQ      'LNA'                                        *PER LINEA ARRIVO
062500970603     C     KCCVER        READE     AZCPC03L                               99
062600970526E   2C                   ENDIF
062700970526IF  2C     WTIPRI        IFEQ      'PRV'                                        *PER PROVINCIA
062800970603     C     KCCVER        READE     AZCPC02L                               99
062900970526E   2C                   ENDIF
063000970526     C*
063100970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
063200970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
063300970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
063400970530X   2C                   ELSE
063500970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
063600970530E   2C                   ENDIF
063700970526E   1C                   ENDDO
063800970526     C*
063900970526     C                   ENDSR
064000970526     C*------------------------------------------------------------------------*
064100970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
064200970526     C*------------------------------------------------------------------------*
064300970526     C     CHKREC        BEGSR
064400970526     C*
064500970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
064600970526     C*
064700970526     C                   ENDSR
064800970526     C*------------------------------------------------------------------------*
064900970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
065000970526     C*------------------------------------------------------------------------*
065100970526     C     IRESFL        BEGSR
065200970526     C*
065300970526     C                   MOVEL     *BLANKS       OPZKEY
065400970526     C                   MOVEL     CPCATB        VIDATB
065500970917     C                   MOVEL     CPCFI1        VIDFI1
065600970526     C                   MOVEL     CPCNAR        VIDNAR
065700970526     C                   MOVEL     CPCCAP        VIDCAP
065800970526     C                   Z-ADD     CPCTTC        VIDTTC
065900970526     C                   MOVEL     CPCISO        VIDISO
066000970526     C                   Z-ADD     CPCLNA        VIDLNA
066100970526     C                   Z-ADD     CPCZNC        VIDZNC
066200970526     C                   MOVEL     CPCPRV        VIDPRV
066300970526     C                   MOVEL     CPCPRA        VIDPRA
066400970526     C                   MOVEL     CPCCTS        VIDCTS
066500970529     C                   MOVEL     CPCCTE        VIDCTE
066600970526     C                   Z-ADD     CPCLOL        VIDLOL
066700970526     C                   Z-ADD     CPCZOL        VIDZOL
066800970604     C                   Z-ADD     CPCLOS        VIDLOS
066900970604     C                   Z-ADD     CPCZOS        VIDZOS
067000970526     C                   MOVEL     CPCFLG        VIDFLG
067100970526     C                   MOVEL     CPCFIT        VIDFIT
067200970528     C                   MOVEL     *BLANKS       VIDESE
067300970527IF  1C     CPCESE        IFEQ      'S'
067400970526     C                   MOVEL     'E'           VIDESE                         *ESISTONO ECCEZIONI
067500970527E   1C                   ENDIF
067600970528     C                   MOVEL     *BLANKS       VIDESL
067700970527IF  1C     CPCESL        IFEQ      'S'
067800970527     C                   MOVEL     'L'           VIDESL                         *LOCALITA' GESTITE
067900970527E   1C                   ENDIF
068000970526     C*
068100970526     C                   ENDSR
068200970526     C*------------------------------------------------------------------------*
068300970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
068400970526     C*------------------------------------------------------------------------*
068500970526     C     CHKPAR        BEGSR
068600970526     C*
068700970526     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
068800970526IF  1C     VIDPCA        IFEQ      *BLANKS
068900970526     C     VIDPLN        ANDEQ     *ZEROS
069000970526     C     VIDPPR        ANDEQ     *BLANKS
069100970526     C                   MOVEL     'CAP'         WTIPRI            3
069200970526E   1C                   ENDIF
069300970526     C*
069400970526     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
069500970526IF  1C     ERRL00        IFEQ      '0'
069600970526IF  2C     VIDPCA        IFNE      *BLANKS
069700970526     C     VIDPLN        ANDNE     *ZEROS
069800970526     C                   MOVEL     MSG(6)        DSMSMS
069900970526     C                   MOVEL     '1'           ERRL00
070000970529     C                   SETON                                        808182
070100970526E   2C                   ENDIF
070200970526E   1C                   ENDIF
070300970526     C*
070400970526IF  1C     ERRL00        IFEQ      '0'
070500970526IF  2C     VIDPCA        IFNE      *BLANKS
070600970526     C     VIDPPR        ANDNE     *BLANKS
070700970526     C                   MOVEL     MSG(6)        DSMSMS
070800970526     C                   MOVEL     '1'           ERRL00
070900970529     C                   SETON                                        808183
071000970526E   2C                   ENDIF
071100970526E   1C                   ENDIF
071200970526     C*
071300970526IF  1C     ERRL00        IFEQ      '0'
071400970526IF  2C     VIDPLN        IFNE      *ZEROS
071500970526     C     VIDPPR        ANDNE     *BLANKS
071600970526     C                   MOVEL     MSG(6)        DSMSMS
071700970526     C                   MOVEL     '1'           ERRL00
071800970529     C                   SETON                                        8283
071900970526E   2C                   ENDIF
072000970526E   1C                   ENDIF
072100970526     C*
072200970526     C* CONTROLLA IL TIPO RICERCA
072300970526IF  1C     ERRL00        IFEQ      '0'
072400970526IF  2C     VIDPCA        IFNE      *BLANKS
072500970526     C                   MOVEL     'CAP'         WTIPRI
072600970526E   2C                   ENDIF
072700970526IF  2C     VIDPLN        IFNE      *ZEROS
072800970526     C                   MOVEL     'LNA'         WTIPRI
072900970526E   2C                   ENDIF
073000970526IF  2C     VIDPPR        IFNE      *BLANKS
073100970526     C                   MOVEL     'PRV'         WTIPRI
073200970526E   2C                   ENDIF
073300970526E   1C                   ENDIF
073400970526     C*
073500970526     C                   ENDSR
073600970526     C*------------------------------------------------------------------------*
073700970526     C* LANPGM - LANCIA UN PROGRAMMA
073800970526     C*------------------------------------------------------------------------*
073900970526     C     LANPGM        BEGSR
074000970526     C*
074100970526     C* IMPOSTA LA DS DI PROCEDURA
074200060403     c                   eval      tipopgm=%subst(wpgm:5:1)
074300060403     c                   if        tipopgm='P'
074400060403     c                   clear                   tisip0ds
074500060403     C                   MOVEL     DNAR          Dp0NAR                         *NAZIONE
074600060403     C                   MOVEL     DCAP          Dp0CAP                         *CAP
074700060403     C                   MOVEL     Dprv          Dp0prv                         *CAP
074800060403     C                   MOVEL     wop0          dp0OP0
074900060403     C                   MOVEL     WOP1          Dp0OP1                         *LIVELLO PGM
075000060403     c                   movel     wdri          dp0dri
075100060403     c                   call      WPGM
075200060403     c                   parm                    kpjba
075300060403     c                   parm                    tisip0ds
075400060403     C*
075500060403     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
075600060403IF  1C     Dp0F03        IFEQ      '1'                                          *PREMUTO F03
075700060403     C                   MOVEL     '1'           ERRL00                         *ERRORE
075800060403     C                   MOVEL     '1'           F03L00                         *FINE PGM
075900060403     c                   movel     dp0f03        d00f03
076000060403X   1C                   ELSE                                                   *NON PREMUTO F03
076100060403     C                   MOVEL     Dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
076200060403E   1C                   ENDIF
076300060403     c
076400060403     c                   else
076500970528     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
076600970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
076700970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
076800970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
076900970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
077000970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
077100970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
077200970602     C                   MOVEL     DCAP          D00CAP                         *CAP
077300970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
077400970530     C                   MOVEL     *BLANKS       D00TSP
077500970530     C                   Z-ADD     *ZEROS        D00TFP
077600970530     C                   MOVEL     *BLANKS       D00LOC
077700970603     C                   MOVEL     *BLANKS       D00SIN
077800970530     C                   MOVEL     *BLANKS       D00EPA
077900970530     C                   Z-ADD     *ZEROS        D00ETP
078000970530     C                   Z-ADD     *ZEROS        D00ETA
078100970530     C                   Z-ADD     *ZEROS        D00EDD
078200970530     C                   MOVEL     *BLANKS       D00SPT
078300970530     C                   Z-ADD     *ZEROS        D00SLP
078400970530     C                   Z-ADD     *ZEROS        D00SLA
078500970530     C                   MOVEL     *BLANKS       D00STS
078600970530     C                   Z-ADD     *ZEROS        D00SDD
078700970526     C*
078800970526     C                   CALL      WPGM
078900970612     C                   PARM                    KPJBA
079000970612     C                   PARM                    DSSI00
079100970526     C*
079200970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
079300970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
079400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
079500970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
079600970526X   1C                   ELSE                                                   *NON PREMUTO F03
079700970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
079800970526E   1C                   ENDIF
079900060403     c                   endif
080000970526     C*
080100970526     C                   ENDSR
080200970527     C*------------------------------------------------------------------------*
080300970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
080400970527     C*------------------------------------------------------------------------*
080500970527     C     CARTAB        BEGSR
080600970527     C*
080700970527     C                   ENDSR
080800970526     C*------------------------------------------------------------------------*
080900970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
081000970526     C*------------------------------------------------------------------------*
081100970526     C     CHKFOP        BEGSR
081200061026     C*
081300061026     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
081400061026     C                   SETON                                        93
081500061026     C*
081600061026     C                   IF        §UTECAP = 'C'
081700061026     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
081800061026     C                   SETOFF                                       93
081900061026     C                   ENDIF
082000970526     C*
082100970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
082200970526IF  1C     DEPOP0        IFEQ      'C01'
082300970526     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
082400970526     C                   MOVEL     'S'           OPDATT
082500970526     C                   MOVEL     OPDRIG        OPD(1)
082600970626X   1C                   ELSE
082700970626     C* OPZIONI
082800970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
082900970626     C                   MOVEL     'S'           OPDATT
083000970626     C                   MOVEL     OPDRIG        OPD(5)
083100970626     C                   MOVEL     OPD(8)        OPDRIG                         *LOCALITA'
083200970626     C                   MOVEL     'S'           OPDATT
083300970626     C                   MOVEL     OPDRIG        OPD(8)
083400970626     C                   MOVEL     OPD(9)        OPDRIG                         *ECCEZIONI
083500970626     C                   MOVEL     'S'           OPDATT
083600970626     C                   MOVEL     OPDRIG        OPD(9)
083700970626     C                   MOVEL     OPD(10)       OPDRIG                         *TEMPI CONSEGNA
083800970626     C                   MOVEL     'S'           OPDATT
083900970626     C                   MOVEL     OPDRIG        OPD(10)
084000970626     C* FUNZIONI
084100970626     C                   MOVEL     FUD(8)        FUDRIG                         *LOCALITA'
084200970626     C                   MOVEL     'S'           FUDATT
084300970626     C                   MOVEL     FUDRIG        FUD(8)
084400970626     C                   MOVEL     FUD(9)        FUDRIG                         *ECCEZIONI
084500970626     C                   MOVEL     'S'           FUDATT
084600970626     C                   MOVEL     FUDRIG        FUD(9)
084700970626     C                   MOVEL     FUD(10)       FUDRIG                         *TEMPI CONSEGNA
084800970626     C                   MOVEL     'S'           FUDATT
084900970626     C                   MOVEL     FUDRIG        FUD(10)
085000970526E   1C                   ENDIF
085100970526     C*
085200970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
085300970526IF  1C     DEPOP0        IFEQ      'C02'
085400970526     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
085500970526     C                   MOVEL     'S'           OPDATT
085600970526     C                   MOVEL     OPDRIG        OPD(2)
085700970917     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
085800970917     C                   MOVEL     'S'           OPDATT
085900970917     C                   MOVEL     OPDRIG        OPD(3)
086000061026     C  N93              MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
086100061026     C  N93              MOVEL     'S'           OPDATT
086200061026     C  N93              MOVEL     OPDRIG        OPD(4)
086300061026     C  N93              MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
086400061026     C  N93              MOVEL     'S'           OPDATT
086500061026     C  N93              MOVEL     OPDRIG        OPD(7)
086600060403     C                   MOVEL     opd(15)       opdRIG                         *INSERIMENTO
086700060403     C                   MOVEL     'S'           opdATT
086800060403     C                   MOVEL     opdRIG        opd(15)
086900061016     C* CONSENTITO INSERIMENTO SOLO A PERSONALE "EDP"
087000061026     C                   IF        %subst(KNMUS:1:3) = 'EDP' AND *IN93 = *off
087100970526     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
087200970526     C                   MOVEL     'S'           FUDATT
087300970526     C                   MOVEL     FUDRIG        FUD(6)
087400061016     C                   ENDIF
087500970526E   1C                   ENDIF
087600970526     C*
087700970526     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
087800060403IF  1C**   DUTLPO        IFNE      'S'                                          *FILIALE
087900060403     C**                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
088000060403     C**                 MOVEL     'N'           OPDATT
088100060403     C**                 MOVEL     OPDRIG        OPD(4)
088200060403     C**                 MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
088300060403     C**                 MOVEL     'N'           OPDATT
088400060403     C**                 MOVEL     OPDRIG        OPD(7)
088500060403     C**                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
088600060403     C**                 MOVEL     'N'           FUDATT
088700060403     C**                 MOVEL     FUDRIG        FUD(6)
088800060403E   1C**                 ENDIF
088900970526     C*
089000970526     C* GESTIONE OPZIONI A VIDEO
089100970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
089200970526     C                   EXSR      OPZGES
089300970526     C*
089400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
089500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
089600970526     C                   EXSR      FUNGES
089700970526     C*
089800970526     C                   ENDSR
089900020916     C*--------------------------------------------------------------------------------------------*
090000020916     C* REPERISCE I DATI UTENTE
090100020916     C*--------------------------------------------------------------------------------------------*
090200020916     C     REPDATIUTE    BEGSR
090300020916     C*
090400020916     C* INIZIALIZZA VARIABILI DI WRK
090500020916     C                   CLEAR                   TIBS34DS
090600020916     C                   CLEAR                   AZUTEDS
090700020916     C                   CLEAR                   DDATIUTE
090800020916     C*
090900020916     C     *DTAARA       DEFINE    §azute        azuteds
091000020916     C     *DTAARA       DEFINE    §datiute      ddatiute
091100020916     C                   IN(E)     *DTAARA
091200020916if  1C                   IF        %Error
091300020916     c                   EVAL      I34Tla = 'L'
091400020916     C                   CALL      'TIBS34R'
091500020916     C                   PARM                    Tibs34Ds
091600020916     C                   IN        *DTAARA
091700020916e   1C                   ENDIF
091800020916     C*
091900020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
092000020916     C                   MOVEL(P)  RSUT          DSFIRS           20
092100061026     C*
092200061026     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
092300061026     C                   EVAL      DUTE01 = UTEFAF
092400020916     C*
092500020916     C                   ENDSR
092600970526     C*------------------------------------------------------------------------*
092700970526     C* *INZSR - OPERAZIONI INIZIALI
092800970526     C*------------------------------------------------------------------------*
092900970526     C     *INZSR        BEGSR
093000970526     C*---
093100970526     C* RICEVIMENTO PARAMETRI
093200970526     C*---
093300970526     C     *ENTRY        PLIST
093400970612     C                   PARM                    KPJBA
093500970612     C                   PARM                    DSSI00
093600970526     C*
093700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
093800970526     C     XMSGPA        PLIST
093900970526     C                   PARM                    DSMSMS
094000970526     C                   PARM                    DSMSPG
094100970526     C                   PARM                    DSMSKE
094200970526     C                   PARM                    DSMSER
094300970526     C*
094400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
094500970526     C                   CALL      'XKEY'
094600970526     C                   PARM                    DSKEY
094700970526     C*---
094800970526     C* VARIABILI NON RIFERITE AL DATA BASE
094900970526     C*---
095000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
095100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
095200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
095300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
095400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
095500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
095600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
095700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
095800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
095900970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
096000970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
096100970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
096200970529     C     *LIKE         DEFINE    D00OP1        WOP1
096300970602     C     *LIKE         DEFINE    D00NAR        DNAR
096400970602     C     *LIKE         DEFINE    D00CAP        DCAP
096500970526     C*---
096600970526     C* VARIABILI DI CONTROLLO
096700970526     C*---
096800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
096900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
097000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
097100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
097200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
097300970526     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
097400970526     C*
097500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
097600970526     C                   MOVEL     'TISI02R'     DSMSPG
097700970526     C                   MOVEL     'TISI02R'     MSGQUE
097800970526     C                   MOVEL     *BLANKS       MSGKEY
097900970526     C                   MOVEL     *BLANKS       DSMSMS
098000970526     C                   MOVEL     *BLANKS       DSMSKE
098100970526     C                   Z-ADD     *ZEROS        DSMSER
098200970526     C*---
098300970526     C* VARIABILI RIFERITE AL DATA BASE
098400970526     C*---
098500970526     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
098600970526     C     *LIKE         DEFINE    CPCNAR        KCCNAR
098700970526     C     *LIKE         DEFINE    CPCCAP        KCCCAP
098800970526     C     *LIKE         DEFINE    CPCPRV        KCCPRV
098900970526     C     *LIKE         DEFINE    CPCLNA        KCCLNA
099000970526     C     *LIKE         DEFINE    CPCNAR        DEPNAR                         *DEPOSITI
099100970526     C     *LIKE         DEFINE    CPCCAP        DEPCAP
099200970526     C     *LIKE         DEFINE    CPCLNA        DEPLNA
099300970526     C     *LIKE         DEFINE    CPCPRV        DEPPRV
099400970526     C*---
099500970526     C* CHIAVI DI LETTURA
099600970526     C*---
099700970526     C* LETTURA AZCPC01L - COMPLETA
099800970527     C     KEYCPC        KLIST
099900970526     C                   KFLD                    KCCVER                         *VERSIONE
100000970526     C                   KFLD                    KCCNAR                         *NAZIONE
100100970526     C                   KFLD                    KCCCAP                         *CAP
100200970526     C* LETTURA AZCPC02L - PARZIALE
100300970527     C     KE2CPC        KLIST
100400970526     C                   KFLD                    KCCVER                         *VERSIONE
100500970526     C                   KFLD                    KCCNAR                         *NAZIONE
100600970526     C                   KFLD                    KCCPRV                         *PROVINCIA
100700970526     C* LETTURA AZCPC03L - PARZIALE
100800970527     C     KE3CPC        KLIST
100900970526     C                   KFLD                    KCCVER                         *VERSIONE
101000970526     C                   KFLD                    KCCNAR                         *NAZIONE
101100970526     C                   KFLD                    KCCLNA                         *LINEA ARRIVO
101200970527     C*
101300970527     C* REPERIMENTO DATI UTENTE
101400020916     C                   EXSR      REPDATIUTE
101500970527     C*
101600970527     C* CARICA LE TABELLE OCCORRENTI
101700970527     C                   EXSR      CARTAB
101800970526     C*
101900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
102000970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
102100970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
102200970526     C*
102300970526     C* CALCOLA LA DATA CORRENTE
102400970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
102500970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
102600970526     C                   Z-ADD     WN8           G08DAT
102700970526     C                   Z-ADD     *ZEROS        G08INV
102800970526     C                   MOVEL     '0'           G08ERR
102900970526     C                   CALL      'XSRDA8'
103000970526     C                   PARM                    WLBDA8
103100970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
103200970526     C*
103300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
103400970526     C                   Z-ADD     D00DRI        G08INV
103500970526     C                   Z-ADD     *ZEROS        G08DAT
103600970526     C                   MOVEL     '3'           G08ERR
103700970526     C                   CALL      'XSRDA8'
103800970526     C                   PARM                    WLBDA8
103900970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
104000970526     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
104100970526     C                   MOVEL     D00CAP        VIDPCA                         *CAP
104200970526     C                   EXSR      CHKPAR
104300970526IF  1C     ERRL00        IFEQ      '1'
104400970526     C                   MOVEL     '1'           D00ERR
104500970526     C                   MOVEL     DSMSMS        D00MSG
104600970526X   1C                   ELSE
104700970526     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
104800970526     C                   MOVEL     VIDPCA        DEPCAP
104900970526E   1C                   ENDIF
105000970703     C*
105100970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
105200970703IF  1C     D00VER        IFEQ      *ZEROS
105300970707     C                   Z-ADD     D00DRI        P96DRI            8 0
105400970707     C                   Z-ADD     *ZEROS        P96VER            5 0
105500970707     C                   CALL      'TISI96R'
105600970707     C                   PARM                    P96DRI
105700970707     C                   PARM                    P96VER
105800970707     C                   Z-ADD     P96VER        D00VER
105900970703E   1C                   ENDIF
106000970526     C*
106100970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
106200970526     C                   EXSR      CHKFOP
106300970530     C*
106400970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
106500970530IF  1C     DEPOP0        IFEQ      'C01'
106600970530     C                   MOVEL     DEC(3)        VIDING
106700970530E   1C                   ENDIF
106800970530IF  1C     DEPOP0        IFEQ      'C02'
106900970530     C                   MOVEL     DEC(1)        VIDING
107000970530E   1C                   ENDIF
107100970530IF  1C     DEPOP0        IFEQ      'C05'
107200970530     C                   MOVEL     DEC(2)        VIDING
107300970530E   1C                   ENDIF
107400970526     C*
107500970526     C                   ENDSR
107600970526     C*------------------------------------------------------------------------*
107700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
107900970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
108000970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
108100970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
108200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108400970526     C*------------------------------------------------------------------------*
108500970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
108600970526     C*------------------------------------------------------------------------*
108700970526     C     POSSFL        BEGSR
108800970526     C*
108900970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
109000970526     C                   EXSR      CLRSFL
109100970526     C*
109200970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
109300970526     C                   EXSR      SETFIL
109400970526     C*
109500970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
109600970526     C     SFLEOF        IFEQ      0
109700970526     C                   EXSR      CARSFL
109800970526     C                   END
109900970526     C*
110000970526     C                   ENDSR
110100970526     C*------------------------------------------------------------------------*
110200970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
110300970526     C*------------------------------------------------------------------------*
110400970526     C     ENTATT        BEGSR
110500970526     C*
110600970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
110700970526     C                   Z-ADD     0             OPZOK             1 0
110800970526     C*
110900970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
111000970526     C                   Z-ADD     0             SFLVAR            5 0
111100970526     C*
111200970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
111300970526     C                   Z-ADD     0             SFLERR
111400970526     C*
111500970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
111600970526     C                   Z-ADD     SFLULT        WFLULT
111700970526     C*
111800970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
111900970526     C                   READC     SI02D05                                98
112000970526     C     *IN98         DOWEQ     '0'
112100970526     C*
112200970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
112300970526     C                   ADD       1             SFLVAR
112400970526     C*
112500970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
112600970526     C     OPZOK         IFEQ      0
112700970526     C*
112800970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
112900970526     C     OPZKEY        IFNE      *BLANKS
113000970526     C                   EXSR      OPZCON
113100970526     C                   END
113200970526     C*
113300970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
113400970526     C*            2. PULIZIA SCELTA IN SFL
113500970526     C     OPZOK         IFEQ      0
113600970526     C                   MOVEL     '0'           *IN41
113700970526     C                   MOVEL     *BLANKS       OPZKEY
113800970526     C                   ELSE
113900970526     C*
114000970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
114100970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
114200970526     C                   MOVEL     '1'           *IN41
114300970526     C                   Z-ADD     SFLULT        SFLERR
114400970526     C                   END
114500970526     C                   ELSE
114600970526     C*
114700970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
114800970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
114900970526     C                   MOVEL     '1'           *IN41
115000970526     C                   END                                                    OPZOK=0
115100970526     C*
115200970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
115300970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
115400970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
115500970526     C     OPZOK         IFNE      0
115600970526     C                   Z-ADD     SFLERR        SFLNBR
115700970526     C                   ELSE
115800970526     C                   Z-ADD     SFLULT        SFLNBR
115900970526     C                   END
116000970526     C*
116100970526     C* AGGIORNO RECORD SFL
116200970526     C                   UPDATE    SI02D05
116300970526     C*
116400970526     C                   READC     SI02D05                                98
116500970526     C                   END                                                    DO WHILE
116600970526     C*
116700970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
116800970526     C                   Z-ADD     WFLULT        SFLULT
116900970526     C*
117000970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
117100970526     C     SFLVAR        IFEQ      0
117200970526     C*---                 MOVEL'1'       FINL00
117300970526     C                   END
117400970526     C*
117500970526     C                   ENDSR
117600970526     C*------------------------------------------------------------------------*
117700970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
117800970526     C*------------------------------------------------------------------------*
117900970526     C     CLRSFL        BEGSR
118000970526     C*
118100970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
118200970526     C* (0=NON FINE FILE  1=FINE FILE)
118300970526     C                   Z-ADD     0             SFLEOF
118400970526     C*
118500970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
118600970526     C                   Z-ADD     0             SFLULT
118700970526     C*
118800970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
118900970526     C                   MOVEL     '1'           *IN40
119000970526     C                   WRITE     SI02D06
119100970526     C                   MOVEL     '0'           *IN40
119200970526     C*
119300970526     C                   ENDSR
119400970526     C*------------------------------------------------------------------------*
119500970526     C* CARSFL - CARICAMENTO SFL
119600970526     C*------------------------------------------------------------------------*
119700970526     C     CARSFL        BEGSR
119800970526     C*
119900970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
120000970526     C     SFLULT        IFEQ      0
120100970526     C                   EXSR      SETFIL
120200970526     C                   END
120300970526     C*
120400970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
120500970526     C                   Z-ADD     SFLULT        WFLULT
120600970526     C*
120700970526     C* CICLO:
120800970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
120900970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
121000970526     C                   Z-ADD     1             SFLRIG
121100970526     C     SFLRIG        DOWLE     SFLPAG
121200970526     C     SFLEOF        ANDEQ     0
121300970526     C*
121400970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
121500970526     C                   MOVEL     *BLANKS       OPZKEY
121600970526     C*
121700970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
121800970526     C                   EXSR      IRESFL
121900970526     C*
122000970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
122100970526     C                   ADD       1             SFLULT
122200970526     C*
122300970526     C* SCRITTURA RECORD SFL
122400970526     C                   WRITE     SI02D05
122500970526     C                   ADD       1             SFLRIG
122600970526     C*
122700970526     C* LETTURA FILE DA CARICARE
122800970526     C                   EXSR      LETFIL
122900970526     C                   END                                                    DO WHILE
123000970526     C*
123100970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
123200970526     C     WFLULT        IFNE      SFLULT
123300970526     C                   Z-ADD     WFLULT        SFLNBR
123400970526     C                   ADD       1             SFLNBR
123500970526     C                   END
123600970526     C*
123700970526     C                   ENDSR
123800970526     C*------------------------------------------------------------------------*
123900970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
124000970526     C*------------------------------------------------------------------------*
124100970526     C     OUTSFL        BEGSR
124200970526     C*
124300970526     C* WRITE RECORD TESTATA VIDEO
124400970526     C                   WRITE     SI02D01
124500970526     C*
124600970526     C* VISUALIZZAZIONE MESSAGGIO
124700970526     C                   EXSR      SNDMSG
124800970526     C*
124900970526     C* WRITE RECORD OPZIONI VIDEO
125000970526     C                   WRITE     SI02D02
125100970526     C*
125200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
125300970526     C                   WRITE     SI02D03
125400970526     C*
125500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
125600970526     C                   WRITE     SI02D04
125700970526     C*
125800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
125900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
126000970526     C     SFLEOF        IFEQ      0
126100970526     C                   MOVEL     '0'           *IN42
126200970526     C                   ELSE
126300970526     C*
126400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
126500970526     C                   MOVEL     '1'           *IN42
126600970526     C                   END
126700970526     C*
126800970526     C* WRITE RECORD TASTI FUNZIONALI
126900970526     C                   WRITE     SI02D09
127000970526     C*
127100970526     C* CASO DI SFL VUOTO
127200970526     C     SFLULT        IFLE      0
127300970526     C*
127400970526     C*    - WRITE RECORD VUOTO
127500970526     C                   WRITE     SI02D0A
127600970526     C                   READ      SI02D0A                                99
127700970526     C*
127800970526     C                   ELSE
127900970526     C*
128000970526     C* CASO DI SFL CON RECORDS
128100970526     C*
128200970526     C*    - WRITE RECORD CONTROLLO SFL
128300970526     C                   WRITE     SI02D06
128400970526     C                   READ      SI02D06                                99
128500970526     C                   END
128600970526     C*
128700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
128800970526     C                   READ      SI02D03                                99
128900970526     C*
129000970526     C                   ENDSR
129100970526     C*------------------------------------------------------------------------*
129200970526     C* SNDMSG - MANDA MESSAGGIO
129300970526     C*------------------------------------------------------------------------*
129400970526     C     SNDMSG        BEGSR
129500970526     C*
129600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
129700970526     C                   MOVEL     '1'           *IN40
129800970526     C                   WRITE     SI02DMC
129900970526     C                   MOVEL     '0'           *IN40
130000970526     C*
130100970526     C* VISUALIZZAZIONE MESSAGGIO
130200970526     C                   Z-ADD     0             MSGULT
130300970526     C                   CALL      'XMSG'        XMSGPA
130400970526     C     DSMSER        IFEQ      0
130500970526     C                   MOVEL     DSMSKE        MSGKEY
130600970526     C                   END
130700970526     C                   Z-ADD     1             MSGULT
130800970526     C                   WRITE     SI02DMS                                      SFL MESS
130900970526     C                   WRITE     SI02DMC                                      CONTR MESS
131000970526     C*
131100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
131200970526     C                   MOVEL     *BLANKS       DSMSMS
131300970526     C*
131400970526     C                   ENDSR
131500970526     C*------------------------------------------------------------------------*
131600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
131700970526     C*------------------------------------------------------------------------*
131800970526     C     FUNCON        BEGSR
131900970526     C*
132000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
132100970526     C                   EXSR      FUNVAR
132200970526     C*
132300970526     C* PULISCO CAMPI DI LAVORO
132400970526     C                   Z-ADD     0             I                 5 0
132500970526     C                   Z-ADD     0             W                 5 0
132600970526     C*
132700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
132800970526     C     FUNKEY        IFNE      *BLANKS
132900970526     C     1             DO        24            I
133000970526     C                   MOVEL     FUD(I)        FUDRIG
133100970526     C*
133200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
133300970526     C     FUNKEY        IFEQ      FUDFUN
133400970526     C     FUDATT        ANDEQ     'S'
133500970526     C                   Z-ADD     I             W
133600970526     C                   END
133700970526     C                   END                                                    DO WHILE
133800970526     C*
133900970526     C* FUNZIONE NON ATTIVA (ERRORE)
134000970526     C     W             IFEQ      0
134100970526     C*
134200970526     C* EMISSIONE MESSAGGIO DI ERRORE
134300970526     C                   MOVEL     MSG(2)        DSMSMS
134400970526     C                   END                                                    W=0
134500970526     C                   END                                                    FUNKEY NE *BLANKS
134600970526     C*
134700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
134800970526     C     W             IFNE      0
134900970526     C     FUNKEY        OREQ      *BLANKS
135000970526     C*
135100970526     C     FUNKEY        IFEQ      *BLANKS
135200970526     C                   EXSR      ENTGES
135300970526     C                   END
135400970526     C     FUNKEY        IFEQ      '03'
135500970526     C                   EXSR      F03GES
135600970526     C                   END
135700970526     C     FUNKEY        IFEQ      '05'
135800970526     C                   EXSR      F05GES
135900970526     C                   END
136000970526     C     FUNKEY        IFEQ      '06'
136100970526     C                   EXSR      F06GES
136200970526     C                   END
136300970528     C     FUNKEY        IFEQ      '08'
136400970528     C                   EXSR      F08GES
136500970528     C                   END
136600970528     C     FUNKEY        IFEQ      '09'
136700980225     C                   EXSR      F09GES
136800970528     C                   END
136900970528     C     FUNKEY        IFEQ      '10'
137000980225     C                   EXSR      F10GES
137100970528     C                   END
137200060403     C     FUNKEY        IFEQ      '14'
137300060403     C                   EXSR      F14GES
137400060403     C                   END
137500060927     C     FUNKEY        IFEQ      '15'
137600060927     C                   EXSR      F15GES
137700060927     C                   END
137800970526     C     FUNKEY        IFEQ      '12'
137900970526     C                   EXSR      F12GES
138000970526     C                   END
138100970526     C     FUNKEY        IFEQ      '21'
138200970526     C                   EXSR      F21GES
138300970526     C                   END
138400970526     C     FUNKEY        IFEQ      '22'
138500970526     C                   EXSR      F22GES
138600970526     C                   END
138700970526     C     FUNKEY        IFEQ      '23'
138800970526     C                   EXSR      F23GES
138900970526     C                   END
139000970526     C     FUNKEY        IFEQ      '24'
139100970526     C                   EXSR      F24GES
139200970526     C                   END
139300970526     C     FUNKEY        IFEQ      'RU'
139400970526     C                   EXSR      RUPGES
139500970526     C                   END
139600970526     C*
139700970526     C                   END
139800970526     C*
139900970526     C                   ENDSR
140000970526     C*------------------------------------------------------------------------*
140100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
140200970526     C*------------------------------------------------------------------------*
140300970526     C     FUNVAR        BEGSR
140400970526     C*
140500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
140600970526     C     KEY           IFEQ      ENTER
140700970526     C                   MOVEL     *BLANKS       FUNKEY
140800970526     C                   END
140900970526     C     KEY           IFEQ      F1
141000970526     C                   MOVEL     '01'          FUNKEY
141100970526     C                   END
141200970526     C     KEY           IFEQ      F2
141300970526     C                   MOVEL     '02'          FUNKEY
141400970526     C                   END
141500970526     C     KEY           IFEQ      F3
141600970526     C                   MOVEL     '03'          FUNKEY
141700970526     C                   END
141800970526     C     KEY           IFEQ      F4
141900970526     C                   MOVEL     '04'          FUNKEY
142000970526     C                   END
142100970526     C     KEY           IFEQ      F5
142200970526     C                   MOVEL     '05'          FUNKEY
142300970526     C                   END
142400970526     C     KEY           IFEQ      F6
142500970526     C                   MOVEL     '06'          FUNKEY
142600970526     C                   END
142700970526     C     KEY           IFEQ      F7
142800970526     C                   MOVEL     '07'          FUNKEY
142900970526     C                   END
143000970526     C     KEY           IFEQ      F8
143100970526     C                   MOVEL     '08'          FUNKEY
143200970526     C                   END
143300970526     C     KEY           IFEQ      F9
143400970526     C                   MOVEL     '09'          FUNKEY
143500970526     C                   END
143600970526     C     KEY           IFEQ      F10
143700970526     C                   MOVEL     '10'          FUNKEY
143800970526     C                   END
143900970526     C     KEY           IFEQ      F11
144000970526     C                   MOVEL     '11'          FUNKEY
144100970526     C                   END
144200970526     C     KEY           IFEQ      F12
144300970526     C                   MOVEL     '12'          FUNKEY
144400970526     C                   END
144500970526     C     KEY           IFEQ      F13
144600970526     C                   MOVEL     '13'          FUNKEY
144700970526     C                   END
144800970526     C     KEY           IFEQ      F14
144900970526     C                   MOVEL     '14'          FUNKEY
145000970526     C                   END
145100970526     C     KEY           IFEQ      F15
145200970526     C                   MOVEL     '15'          FUNKEY
145300970526     C                   END
145400970526     C     KEY           IFEQ      F16
145500970526     C                   MOVEL     '16'          FUNKEY
145600970526     C                   END
145700970526     C     KEY           IFEQ      F17
145800970526     C                   MOVEL     '17'          FUNKEY
145900970526     C                   END
146000970526     C     KEY           IFEQ      F18
146100970526     C                   MOVEL     '18'          FUNKEY
146200970526     C                   END
146300970526     C     KEY           IFEQ      F19
146400970526     C                   MOVEL     '19'          FUNKEY
146500970526     C                   END
146600970526     C     KEY           IFEQ      F20
146700970526     C                   MOVEL     '20'          FUNKEY
146800970526     C                   END
146900970526     C     KEY           IFEQ      F21
147000970526     C                   MOVEL     '21'          FUNKEY
147100970526     C                   END
147200970526     C     KEY           IFEQ      F22
147300970526     C                   MOVEL     '22'          FUNKEY
147400970526     C                   END
147500970526     C     KEY           IFEQ      F23
147600970526     C                   MOVEL     '23'          FUNKEY
147700970526     C                   END
147800970526     C     KEY           IFEQ      F24
147900970526     C                   MOVEL     '24'          FUNKEY
148000970526     C                   END
148100970526     C     KEY           IFEQ      ROLLUP
148200970526     C                   MOVEL     'RU'          FUNKEY
148300970526     C                   END
148400970526     C     KEY           IFEQ      HELP
148500970526     C                   MOVEL     'HP'          FUNKEY
148600970526     C                   END
148700970526     C*
148800970526     C                   ENDSR
148900970526     C*------------------------------------------------------------------------*
149000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
149100970526     C*------------------------------------------------------------------------*
149200970526     C     FUNGES        BEGSR
149300970526     C*
149400970526     C* PULISCO CAMPI DI LAVORO
149500970526     C                   Z-ADD     0             W                 5 0
149600970526     C*
149700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
149800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
149900970526     C     1             DO        24            I
150000970526     C                   MOVEL     FUD(I)        FUDRIG
150100970526     C*
150200970526     C* PULIZIA FUNZIONI PRECEDENTI
150300970526     C                   MOVEL     *BLANKS       FUV(I)
150400970526     C*
150500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
150600970526     C* (SALTA F23 E F24)
150700970526     C     FUDATT        IFEQ      'S'
150800970526     C     FUDFUN        ANDNE     '23'
150900970526     C     FUDFUN        ANDNE     '24'
151000970526     C                   ADD       1             W
151100970526     C                   MOVEL     FUDDES        FUV(W)
151200970526     C                   END
151300970526     C                   END                                                    DO WHILE
151400970526     C*
151500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
151600970526     C* ALTRIMENTI DISATTIVO F24
151700970526     C     1             DO        24            I
151800970526     C                   MOVEL     FUD(I)        FUDRIG
151900970526     C*
152000970526     C     FUDFUN        IFEQ      '24'
152100970526     C     W             IFGT      6
152200970526     C                   MOVEL     'S'           FUDATT
152300970526     C                   ELSE
152400970526     C                   MOVEL     *BLANKS       FUDATT
152500970526     C                   END                                                    W GT 8
152600970526     C                   MOVEL     FUDRIG        FUD(I)
152700970526     C                   END
152800970526     C                   END                                                    DO WHILE
152900970526     C*
153000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
153100970526     C     W             DIV       6             I
153200970526     C                   MVR                     Z
153300970526     C*
153400970526     C     Z             IFNE      0
153500970526     C     I             ADD       1             Z
153600970526     C                   ELSE
153700970526     C                   Z-ADD     I             Z
153800970526     C                   END
153900970526     C*
154000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
154100970526     C                   ADD       1             FUNGRU
154200970526     C     FUNGRU        IFGT      Z
154300970526     C     FUNGRU        ORLT      1
154400970526     C                   Z-ADD     1             FUNGRU
154500970526     C                   END
154600970526     C*
154700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
154800970526     C     FUNGRU        IFEQ      1
154900970526     C                   MOVEL     FUV(1)        FUN001
155000970526     C                   MOVEL     FUV(2)        FUN002
155100970526     C                   MOVEL     FUV(3)        FUN003
155200970526     C                   MOVEL     FUV(4)        FUN004
155300970526     C                   MOVEL     FUV(5)        FUN005
155400970526     C                   MOVEL     FUV(6)        FUN006
155500970526     C                   END
155600970526     C*
155700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
155800970526     C     FUNGRU        IFEQ      2
155900970526     C                   MOVEL     FUV(7)        FUN001
156000970526     C                   MOVEL     FUV(8)        FUN002
156100970526     C                   MOVEL     FUV(9)        FUN003
156200970526     C                   MOVEL     FUV(10)       FUN004
156300970526     C                   MOVEL     FUV(11)       FUN005
156400970526     C                   MOVEL     FUV(12)       FUN006
156500970526     C                   END
156600970526     C*
156700970526     C* TERZO GRUPPO DI 6 FUNZIONI
156800970526     C     FUNGRU        IFEQ      3
156900970526     C                   MOVEL     FUV(13)       FUN001
157000970526     C                   MOVEL     FUV(14)       FUN002
157100970526     C                   MOVEL     FUV(15)       FUN003
157200970526     C                   MOVEL     FUV(16)       FUN004
157300970526     C                   MOVEL     FUV(17)       FUN005
157400970526     C                   MOVEL     FUV(18)       FUN006
157500970526     C                   END
157600970526     C*
157700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
157800970526     C     FUNGRU        IFEQ      4
157900970526     C                   MOVEL     FUV(19)       FUN001
158000970526     C                   MOVEL     FUV(20)       FUN002
158100970526     C                   MOVEL     FUV(21)       FUN003
158200970526     C                   MOVEL     FUV(22)       FUN004
158300970526     C                   MOVEL     *BLANKS       FUN005
158400970526     C                   MOVEL     *BLANKS       FUN006
158500970526     C                   END
158600970526     C*
158700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
158800970526     C                   MOVEL     *BLANKS       FUN007
158900970526     C                   MOVEL     *BLANKS       FUN008
159000970526     C*
159100970526     C     1             DO        24            I
159200970526     C                   MOVEL     FUD(I)        FUDRIG
159300970526     C     FUDFUN        IFEQ      '23'
159400970526     C     FUDATT        ANDEQ     'S'
159500970526     C                   MOVEL     FUDDES        FUN007
159600970526     C                   END
159700970526     C     FUDFUN        IFEQ      '24'
159800970526     C     FUDATT        ANDEQ     'S'
159900970526     C                   MOVEL     FUDDES        FUN008
160000970526     C                   END
160100970526     C                   END                                                    DO
160200970526     C*
160300970526     C                   ENDSR
160400970526     C*------------------------------------------------------------------------*
160500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
160600970526     C*------------------------------------------------------------------------*
160700970526     C     OPZGES        BEGSR
160800970526     C*
160900970526     C* PULISCO CAMPI DI LAVORO
161000970526     C                   Z-ADD     0             W                 5 0
161100970526     C*
161200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
161300970526     C* E CARICO TABELLA OPZIONI ATTIVE
161400970526     C     1             DO        24            I
161500970526     C                   MOVEL     OPD(I)        OPDRIG
161600970526     C*
161700970526     C* PULIZIA OPZIONI PRECEDENTI
161800970526     C                   MOVEL     *BLANKS       OPV(I)
161900970526     C*
162000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
162100970526     C     OPDATT        IFEQ      'S'
162200970526     C                   ADD       1             W
162300970526     C                   MOVEL     OPDDES        OPV(W)
162400970526     C                   END
162500970526     C                   END                                                    DO WHILE
162600970526     C*
162700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
162800970526     C* ALTRIMENTI DISATTIVO F23
162900970526     C     1             DO        24            I
163000970526     C                   MOVEL     FUD(I)        FUDRIG
163100970526     C*
163200970526     C     FUDFUN        IFEQ      '23'
163300970526     C     W             IFGT      8
163400970526     C                   MOVEL     'S'           FUDATT
163500970526     C                   ELSE
163600970526     C                   MOVEL     *BLANKS       FUDATT
163700970526     C                   END                                                    W GT 8
163800970526     C                   MOVEL     FUDRIG        FUD(I)
163900970526     C                   END
164000970526     C                   END                                                    DO WHILE
164100970526     C*
164200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
164300970526     C     W             DIV       8             I
164400970526     C                   MVR                     Z                 5 0
164500970526     C*
164600970526     C     Z             IFNE      0
164700970526     C     I             ADD       1             Z
164800970526     C                   ELSE
164900970526     C                   Z-ADD     I             Z
165000970526     C                   END
165100970526     C*
165200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
165300970526     C                   ADD       1             OPZGRU
165400970526     C     OPZGRU        IFGT      Z
165500970526     C     OPZGRU        ORLT      1
165600970526     C                   Z-ADD     1             OPZGRU
165700970526     C                   END
165800970526     C*
165900970526     C* PRIMO GRUPPO DI 8 OPZIONI
166000970526     C     OPZGRU        IFEQ      1
166100970526     C                   MOVEL     OPV(1)        OPZ001
166200970526     C                   MOVEL     OPV(2)        OPZ002
166300970526     C                   MOVEL     OPV(3)        OPZ003
166400970526     C                   MOVEL     OPV(4)        OPZ004
166500970526     C                   MOVEL     OPV(5)        OPZ005
166600970526     C                   MOVEL     OPV(6)        OPZ006
166700970526     C                   MOVEL     OPV(7)        OPZ007
166800970526     C                   MOVEL     OPV(8)        OPZ008
166900970526     C                   END
167000970526     C*
167100970526     C* SECONDO GRUPPO DI 8 OPZIONI
167200970526     C     OPZGRU        IFEQ      2
167300970526     C                   MOVEL     OPV(9)        OPZ001
167400970526     C                   MOVEL     OPV(10)       OPZ002
167500970526     C                   MOVEL     OPV(11)       OPZ003
167600970526     C                   MOVEL     OPV(12)       OPZ004
167700970526     C                   MOVEL     OPV(13)       OPZ005
167800970526     C                   MOVEL     OPV(14)       OPZ006
167900970526     C                   MOVEL     OPV(15)       OPZ007
168000970526     C                   MOVEL     OPV(16)       OPZ008
168100970526     C                   END
168200970526     C*
168300970526     C* TERZO GRUPPO DI 8 OPZIONI
168400970526     C     OPZGRU        IFEQ      3
168500970526     C                   MOVEL     OPV(17)       OPZ001
168600970526     C                   MOVEL     OPV(18)       OPZ002
168700970526     C                   MOVEL     OPV(19)       OPZ003
168800970526     C                   MOVEL     OPV(20)       OPZ004
168900970526     C                   MOVEL     OPV(21)       OPZ005
169000970526     C                   MOVEL     OPV(22)       OPZ006
169100970526     C                   MOVEL     OPV(23)       OPZ007
169200970526     C                   MOVEL     OPV(24)       OPZ008
169300970526     C                   END
169400970526     C*
169500970526     C                   ENDSR
169600970526     C*------------------------------------------------------------------------*
169700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
169800970526     C*------------------------------------------------------------------------*
169900970526     C     OPZCON        BEGSR
170000970526     C*
170100970526     C* PULISCO CAMPI DI LAVORO
170200970526     C                   Z-ADD     0             I                 5 0
170300970526     C                   Z-ADD     0             W                 5 0
170400970526     C*
170500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
170600970526     C     1             DO        24            I
170700970526     C                   MOVEL     OPD(I)        OPDRIG
170800970526     C*
170900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
171000970526     C     OPZKEY        IFEQ      OPDOPZ
171100970526     C     OPZKEY        OREQ      OPDOP1
171200970526     C*
171300970526     C     OPDATT        IFEQ      'S'
171400970526     C                   MOVEL     OPDOPZ        OPZKEY
171500970526     C                   Z-ADD     I             W
171600970526     C                   END
171700970526     C                   END
171800970526     C*
171900970526     C                   END                                                    DO WHILE
172000970526     C*
172100970526     C* OPZIONE NON ATTIVA (ERRORE)
172200970526     C     W             IFEQ      0
172300970526     C*
172400970526     C* OPZIONE NON OK
172500970526     C                   Z-ADD     1             OPZOK
172600970526     C*
172700970526     C* EMISSIONE MESSAGGIO DI ERRORE
172800970526     C                   MOVEL     MSG(1)        DSMSMS
172900970526     C                   ELSE
173000970526     C*
173100970526     C* OPZIONE ATTIVA (PROCEDI ...)
173200970526     C*
173300970526     C     OPZKEY        IFEQ      ' 1'
173400970526     C                   EXSR      O01GES
173500970526     C                   END
173600970526     C     OPZKEY        IFEQ      ' 2'
173700970526     C                   EXSR      O02GES
173800970526     C                   END
173900970917     C     OPZKEY        IFEQ      ' 3'
174000970917     C                   EXSR      O03GES
174100970917     C                   END
174200970526     C     OPZKEY        IFEQ      ' 4'
174300970526     C                   EXSR      O04GES
174400970526     C                   END
174500970526     C     OPZKEY        IFEQ      ' 5'
174600970526     C                   EXSR      O05GES
174700970526     C                   END
174800970526     C     OPZKEY        IFEQ      ' 7'
174900970526     C                   EXSR      O07GES
175000970526     C                   END
175100970526     C     OPZKEY        IFEQ      ' 8'
175200970526     C                   EXSR      O08GES
175300970526     C                   END
175400970526     C     OPZKEY        IFEQ      ' 9'
175500970526     C                   EXSR      O09GES
175600970526     C                   END
175700970526     C     OPZKEY        IFEQ      '10'
175800970526     C                   EXSR      O10GES
175900970526     C                   END
176000060403     C     OPZKEY        IFEQ      '14'
176100060403     C                   EXSR      O14GES
176200060403     C                   END
176300060403     C     OPZKEY        IFEQ      '15'
176400060403     C                   EXSR      O15GES
176500060403     C                   END
176600970526     C*
176700970526     C                   END                                                    OPZ.NON ATTIVA
176800970526     C*
176900970526     C                   ENDSR
177000970526     C*------------------------------------------------------------------------*
177100970526     C* NOTE COSTRUZIONE SCHIERE
177200970526     C*------------------------------------------------------------------------*
177300970526     C*
177400970526     C* -MSG-MESSAGGI
177500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
177600970526     C* (NON TOCCARE MSG,1 E MSG,2)
177700970526     C*
177800970526     C* -CMD-COMANDI
177900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
178000970526     C*
178100970526     C* -OPD-OPZIONI
178200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
178300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
178400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
178500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
178600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
178700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
178800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
178900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
179000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
179100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
179200970526     C*
179300970526     C* -FUD-FUNZIONI
179400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
179500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
179600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
179700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
179800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
179900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
180000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
180100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
180200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
180300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
180400970526     O*------------------------------------------------------------------------*
180500970526** DEC - DECODIFICHE
180600970526    GESTIONE                                                     1
180700970526INTERROGAZIONE                                                   2
180800970530     SCELTA                                                      3
1809009705264
1810009705265
181100970526** MSG - MESSAGGI
181200970526TISI02R- Opzione non gestita                                                   1
181300970526TISI02R- Tasto funzionale non gestito                                          2
181400970704TISI02R- 3
181500970526TISI02R- Record annullato. Consentito solo il ripristino                       4
181600970526TISI02R- Record non annullato. Impossibile ripristinare                        5
181700970526TISI02R- Accettato solo un parametro di inizio lista                           6
181800970526TISI02R- Errori nei parametri di ingresso                                      7
181900970526TISI02R- Data errata                                                           8
182000970526TISI02R- 9                                                                     9
182100970526TISI02R- 10                                                                   10
182200970526TISI02R- 11                                                                   11
182300970526TISI02R- 12                                                                   12
182400970526TISI02R- 13                                                                   13
182500970526TISI02R- 14                                                                   14
182600970526TISI02R- 15                                                                   15
182700970526TISI02R- 16                                                                   16
182800970526TISI02R- 17                                                                   17
182900970526TISI02R- 18                                                                   18
183000970526TISI02R- 19                                                                   19
183100970526TISI02R- 20                                                                   20
183200970526** CMD - COMANDI CL
183300970526WRKSPLF
183400970526
183500970526
183600970526
183700970526
183800970526** OPD - OPZIONI
183900970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
184000970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
184100970917 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
184200970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
184300970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
184400970526                   -6 -     (S/N)=N                        opz. 06¬
184500970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
184600970626 8=Località        -8 -     (S/N)=N                        opz. 08¬
184700970626 9=Eccezioni       -9 -     (S/N)=N                        opz. 09¬
18480097062610=Tempi consegna  -10-     (S/N)=N                        opz. 10¬
184900970526                   -  -     (S/N)=N                        opz. 11¬
185000060403                   -  -     (S/N)=N                        opz. 12¬
185100970526                   -  -     (S/N)=N                        opz. 13¬
18520006040314=Cal.Eventi Part -14-     (S/N)=S                        opz. 14¬
18530006040315=InserisciEvePart-15-     (S/N)=N                        opz. 15¬
185400970526                   -  -     (S/N)=N                        opz. 16¬
185500970526                   -  -     (S/N)=N                        opz. 17¬
185600970526                   -  -     (S/N)=N                        opz. 18¬
185700970526                   -  -     (S/N)=N                        opz. 19¬
185800970526                   -  -     (S/N)=N                        opz. 20¬
185900970526                   -  -     (S/N)=N                        opz. 21¬
186000970526                   -  -     (S/N)=N                        opz. 22¬
186100970526                   -  -     (S/N)=N                        opz. 23¬
186200970526                   -  -     (S/N)=N                        opz. 24¬
186300970526** FUD - FUNZIONI
186400970526                   -        (S/N)=N                        funz.01¬
186500970526                   -        (S/N)=N                        funz.02¬
186600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
186700970526                   -        (S/N)=N                        funz.04¬
186800060927F14=Cal.Eventi Part-        (S/N)=S  <== S-FISSO           funz.05¬
186900970526F06=Inserimento    -        (S/N)=N                        funz.06¬
187000970526                   -        (S/N)=N                        funz.07¬
187100970626F08=Località       -        (S/N)=N                        funz.08¬
187200970626F09=Eccezioni      -        (S/N)=N                        funz.09¬
187300970626F10=Tempi consegna -        (S/N)=N                        funz.10¬
187400970526                   -        (S/N)=N                        funz.11¬
187500060927F15=CapUtilizziPart-        (S/N)=S  <== S-FISSO           funz.12¬
187600970526                   -        (S/N)=N                        funz.13¬
187700060927F05=Rivisualizza   -        (S/N)=S                        funz.14¬
187800060927F12=Ritorno        -        (S/N)=S                        funz.15¬
187900970526                   -        (S/N)=N                        funz.16¬
188000970526                   -        (S/N)=N                        funz.17¬
188100970526                   -        (S/N)=N                        funz.18¬
188200970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
188300970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
188400970528F21=Stampa cappario-        (S/N)=S                        funz.21¬
188500970528F22=Gestione stampe-        (S/N)=S                        funz.22¬
188600970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
188700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
