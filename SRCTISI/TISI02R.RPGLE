000100970526     H*------------------------------------------------------------------------*
000200970526     H* CAPPARIO - GESTIONE CAP - SCELTA
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800170116     fazcpr01l  IF   E           K DISK                                         *versioni cappario
000900970526     FTISI02D   CF   E             WORKSTN INFDS(DEVDS1)
001000970526     F                                     SFILE(SI02D05:SFLULT)
001100970526     F                                     SFILE(SI02DMS:MSGULT)
001200970526     F*------------
001300970526     F* CAPPARIO
001400970526     F*------------
001500130823     FAZcos01L  IF   E           K DISK
001600130823     FAZCPC01L  IF   E           K DISK
001700970526     FAZCPC02L  IF   E           K DISK
001800970526     F                                     RENAME(AZCPC000:AZCPC2)
001900970526     FAZCPC03L  IF   E           K DISK
002000970526     F                                     RENAME(AZCPC000:AZCPC3)
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
003300970526     D* COMANDI
003400970526     D*------------
003500970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI OPZIONI
003800970526     D*------------
003900970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* DESCRIZIONI TASTI FUNZIONALI
004200970526     D*------------
004300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004400970526     D*------------
004500970526     D* OPZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970526     D OPV             S             19    DIM(24)
004800970526     D*------------
004900970526     D* FUNZIONI DA CARICARE A VIDEO
005000970526     D*------------
005100970526     D FUV             S             19    DIM(24)
005200131028     d* Filiali abilitate  agli orari servizi
005300131028     D FIL_AB          S              3    DIM(85)
005400970526     D*------------------------------------------------------------------------*
005500970526     D* INPUT
005600970526     D*------------------------------------------------------------------------*
005700970526     D*------------
005800970526     D* RIDEFINIZIONE OPZIONI VIDEO
005900970526     D*------------
006000970526     D OPDDS           DS
006100970526     D  OPDOPZ                 1      2
006200970526     D  OPDDE1                 3     19
006300970526     D  OPDOP1                21     22
006400970526     D  OPDFI0                23     34
006500970526     D  OPDATT                35     35
006600970526     D  OPDFI1                36     80
006700970526     D  OPDDES                 1     19
006800970526     D  OPDRIG                 1     80
006900970526     D*------------
007000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007100970526     D*------------
007200970526     D FUDDS           DS
007300970526     D  FUDDE1                 1      1
007400970526     D  FUDFUN                 2      3
007500970526     D  FUDDE2                 4     19
007600970526     D  FUDFI0                20     34
007700970526     D  FUDATT                35     35
007800970526     D  FUDFI1                36     80
007900970526     D  FUDDES                 1     19
008000970526     D  FUDRIG                 1     80
008100970526     D*------------
008200970526     D* DS GESTIONE TASTI FUNZIONALI
008300970526     D*------------
008400970526     D DSKEY         E DS
008500970526     D DEVDS1          DS
008600970526     D  KEY                  369    369
008700970526     D*------------
008800970526     D* DS "XMSG" - GESTIONE MESSAGGI
008900970526     D*------------
009000970526     D DSMSG         E DS
009100970526     D*------------
009200020916     D* DS REPERIMENTO DATI UTENTE
009300020916     D*-------------------
009400020916     D TIBS34DS      E DS                                                       *Profili utente
009500020916     D DDATIUTE      E DS                                                       *Dati utente
009600020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009700061026     D DUTE01        E DS                                                       *Abilitazioni
009800970526     D*------------------
009900970526     D* DS "XSRDAT" - CONTROLLA DATA (6)
010000970526     D*------------------
010100970526     D WLBDAT          DS                  INZ
010200970526     D  G06DAT                 1      6  0
010300970526     D  G06INV                 7     12  0
010400970526     D  G06ERR                13     13
010500970526     D  G06TGI                14     18  0
010600970526     D*------------------
010700970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
010800970526     D*------------------
010900970526     D WLBDA8          DS                  INZ
011000970526     D  G08DAT                 1      8  0
011100970526     D  G08INV                 9     16  0
011200970526     D  G08ERR                17     17
011300970526     D  G08TGI                18     22  0
011400970526     D*------------
011500970526     D* DS DI PROCEDURA
011600970526     D*------------
011700970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
011800060403     D tisip0ds      E DS
011900060927     D tisiq0ds      E DS
012000131028     D tisiO0ds      E DS
012100970526     D*------------
012200970526     D* ARCHITETTURA
012300970526     D*------------
012400970526     D KPJBA         E DS
012500131028
012600131028     d dvpodeco      e ds
012700131028     d Tibs02Ds      e ds
012800131028
012900060403     d tipopgm         s              1
013000170116     d wabiute         s              1
013100060403     d wdri            s                   like(Dp0dri)
013200060403     d dprv            s                   like(Dp0prv)
013300130823     d kloc            s                   like(cosloc)
013400131028     d w003a           s              3
013500131028     d XX              s              3  0
013600131028     d YY              s              3  0
013700131028
013800131028
013900131028      /copy gaitrasrc/srcprotopr,tibs02r
014000970526     C*------------------------------------------------------------------------*
014100970526     C* MAIN LINES
014200970526     C*------------------------------------------------------------------------*
014300970526     C*
014400970526     C* SE NON CI SONO ERRORI, PROSEGUE
014500970526IF  1C     ERRL00        IFNE      '1'
014600970526     C*
014700970526     C* CARICA IL SFILE
014800970526     C                   EXSR      POSSFL
014900970526     C*
015000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
015100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
015200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015400970526     C*
015500970526     C* REIMPOSTA ERRORE DI PROCERURA
015600970526     C                   MOVEL     '0'           ERRL00
015700970526     C*
015800970526     C* EMETTE IL SFILE
015900970526     C                   EXSR      OUTSFL
016000970526     C*
016100970526     C* GESTIONE TASTI FUNZIONALI
016200970526     C                   EXSR      FUNCON
016300970526     C*
016400970526E   2C                   ENDDO
016500970527     C*
016600970527     C* CI SONO ERRORI IN ENTRATA
016700970527X   1C                   ELSE
016800970527     C                   MOVEL     '1'           D00ERR
016900970527     C                   MOVEL     DSMSMS        D00MSG
017000970526E   1C                   ENDIF
017100970526     C* FINE PGM
017200970526     C                   SETON                                        LR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F03GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F03"
017900970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018000970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
018100970526     C*
018200970526     C                   ENDSR
018300970526     C*------------------------------------------------------------------------*
018400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
018500970526     C*------------------------------------------------------------------------*
018600970526     C     F05GES        BEGSR
018700970526     C*
018800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018900970526     C*
019000970526     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
019300970526     C*------------------------------------------------------------------------*
019400970526     C     F06GES        BEGSR
019500970526     C*
019600970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
019700020916IF  1C     DUTLPO        IFNE      'S'
019800970526     C                   MOVEL     '1'           ERRL00
019900970526     C                   MOVEL     MSG(3)        DSMSMS
020000970526E   1C                   ENDIF
020100970526     C*
020200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
020300970602     C                   MOVEL     *BLANKS       DNAR
020400970602     C                   MOVEL     *BLANKS       DCAP
020500970529     C                   MOVEL     DEPOP0        WOP0
020600970529     C                   MOVEL     'F06'         WOP1
020700970526     C                   MOVEL     'TISI03R'     WPGM              8
020800970526     C                   EXSR      LANPGM                                       *LANCIA PGM
020900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021000970526E   1C                   ENDIF
021100970526     C*
021200970526     C                   ENDSR
021300970528     C*------------------------------------------------------------------------*
021400970528     C* F08GES - GESTIONE F08 -> GESTIONE LOCALITA'
021500970528     C*------------------------------------------------------------------------*
021600970528     C     F08GES        BEGSR
021700970528     C*
021800970602     C                   MOVEL     *BLANKS       DNAR
021900970602     C                   MOVEL     *BLANKS       DCAP
022000970528     C                   MOVEL     DEPOP0        WOP0
022100970528     C                   MOVEL     'L'           WOP0
022200970528     C                   MOVEL     'F08'         WOP1
022300970528     C                   MOVEL     'TISI30R'     WPGM              8
022400970528     C                   EXSR      LANPGM
022500970528     C*
022600970528     C                   ENDSR
022700970528     C*------------------------------------------------------------------------*
022800970528     C* F09GES - GESTIONE F09 -> ECCEZIONI LINEE/ZONE
022900970528     C*------------------------------------------------------------------------*
023000970528     C     F09GES        BEGSR
023100970528     C*
023200970602     C                   MOVEL     *BLANKS       DNAR
023300970602     C                   MOVEL     *BLANKS       DCAP
023400970528     C                   MOVEL     DEPOP0        WOP0
023500970528     C                   MOVEL     'E'           WOP0
023600970528     C                   MOVEL     'F09'         WOP1
023700970528     C                   MOVEL     'TISI11R'     WPGM              8
023800970528     C                   EXSR      LANPGM
023900970528     C*
024000970528     C                   ENDSR
024100970528     C*------------------------------------------------------------------------*
024200970528     C* F10GES - GESTIONE F10 -> TEMPI DI CONSEGNA
024300970528     C*------------------------------------------------------------------------*
024400970602     C     F10GES        BEGSR
024500970528     C*
024600970602     C                   MOVEL     *BLANKS       DNAR
024700970602     C                   MOVEL     *BLANKS       DCAP
024800970528     C                   MOVEL     DEPOP0        WOP0
024900970528     C                   MOVEL     'T'           WOP0
025000970528     C                   MOVEL     'F10'         WOP1
025100970528     C                   MOVEL     'TISI41R'     WPGM              8
025200970528     C                   EXSR      LANPGM
025300970528     C*
025400970528     C                   ENDSR
025500970526     C*------------------------------------------------------------------------*
025600970526     C* F12GES - GESTIONE F12 -> RITORNO
025700970526     C*------------------------------------------------------------------------*
025800970526     C     F12GES        BEGSR
025900970526     C*
026000970526     C* IMPOSTA "PREMUTO F12"
026100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
026200970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
026300970526     C*
026400970526     C                   ENDSR
026500060403     C*------------------------------------------------------------------------*
026600060403     C* F14GES - GESTIONE F14 -> calendario eventi particolari
026700060403     C*------------------------------------------------------------------------*
026800060403     C     F14GES        BEGSR
026900060403     C*
027000060403     C                   MOVEL     *BLANKS       DNAR
027100060403     C                   MOVEL     *BLANKS       DCAP
027200060403     C                   MOVEL     DEPOP0        WOP0
027300060403     c                   clear                   wop1
027400060403     C                   MOVEL     datcor        wdri
027500060403     C                   MOVEL     'TISIP0R'     WPGM              8
027600060403     C                   EXSR      LANPGM
027700060403     C*
027800060403     C                   ENDSR
027900060927     C*------------------------------------------------------------------------*
028000060927     C* F15GES - GESTIONE F15 -> cappario utilizzi particolari
028100060927     C*------------------------------------------------------------------------*
028200060927     C     F15GES        BEGSR
028300060927     C*
028400060927     c                   clear                   tisiq0ds
028500060927     c                   MOVEL     'C05'         dq0op0                         *livello procedura
028600060927     c                   clear                   dq0op1                         *livello programma
028700060927     c                   MOVEL     '0'           dq0f03                         *premuto F03
028800060927     c                   MOVEL     '0'           dq0f12                         *premuto F12
028900060927     c                   MOVEL     '0'           dq0err                         *errore
029000060927     c                   call      'TISIQ0R'
029100060927     c                   parm                    kpjba
029200060927     c                   parm                    tisiQ0ds
029300060927     C*
029400060927     C                   ENDSR
029500970526     C*------------------------------------------------------------------------*
029600970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
029700970526     C*------------------------------------------------------------------------*
029800970526     C     F21GES        BEGSR
029900970612     C*
030000970612     C                   MOVEL     VIDNAR        DNAR
030100970612     C                   MOVEL     VIDCAP        DCAP
030200970612     C                   MOVEL     DEPOP0        WOP0
030300970612     C                   MOVE      '03'          WOP0
030400970612     C                   MOVEL     'F21'         WOP1
030500970612     C                   MOVEL     'TISI04R'     WPGM              8
030600970612     C                   EXSR      LANPGM
030700970526     C*
030800970526     C                   ENDSR
030900970526     C*------------------------------------------------------------------------*
031000970526     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
031100970526     C*------------------------------------------------------------------------*
031200970526     C     F22GES        BEGSR
031300970526     C*
031400970526     C                   Z-ADD     80            LENGH            15 5
031500970526     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
031600970526     C                   PARM                    CMD(1)
031700970526     C                   PARM                    LENGH
031800970526     C*
031900970526     C                   ENDSR
032000970526     C*------------------------------------------------------------------------*
032100970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
032200970526     C*------------------------------------------------------------------------*
032300970526     C     F23GES        BEGSR
032400970526     C*
032500970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
032600970526     C*
032700970526     C                   ENDSR
032800970526     C*------------------------------------------------------------------------*
032900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
033000970526     C*------------------------------------------------------------------------*
033100970526     C     F24GES        BEGSR
033200970526     C*
033300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
033400970526     C*
033500970526     C                   ENDSR
033600970526     C*------------------------------------------------------------------------*
033700970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
033800970526     C*------------------------------------------------------------------------*
033900970526     C     RUPGES        BEGSR
034000970526     C*
034100970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
034200970526     C*
034300970526     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* ENTGES - GESTIONE ENTER -> INVIO
034600970526     C*------------------------------------------------------------------------*
034700970526     C     ENTGES        BEGSR
034800970526     C*
034900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
035000970526     C                   EXSR      NEWPOS
035100970526     C*
035200970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
035300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035400970526IF  2C     SWPOSI        IFNE      0
035500970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
035600970526X   2C                   ELSE
035700970526IF  3C     SFLULT        IFNE      0
035800970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
035900970526     C*
036000970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
036100970526IF  4C     SWMODI        IFNE      0
036200970526     C                   EXSR      POSSFL
036300970526E   4C                   ENDIF
036400970526E   3C                   ENDIF
036500970526E   2C                   ENDIF
036600970526E   1C                   ENDIF
036700970526     C*
036800970526     C                   ENDSR
036900970526     C*------------------------------------------------------------------------*
037000970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
037100970526     C*------------------------------------------------------------------------*
037200970526     C     O01GES        BEGSR
037300970526     C*
037400970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
037500970526     C                   MOVEL     VIDNAR        D00NAR
037600970526     C                   MOVEL     VIDCAP        D00CAP
037700970704     C                   MOVEL     VIDPRV        D00PRV
037800970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
037900970526     C*
038000970526     C                   ENDSR
038100970526     C*------------------------------------------------------------------------*
038200970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
038300970526     C*------------------------------------------------------------------------*
038400970526     C     O02GES        BEGSR
038500970526     C*
038600970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
038700970526IF  1C     VIDATB        IFEQ      'A'
038800970526     C                   MOVEL     '1'           ERRL00
038900970526     C                   MOVEL     MSG(4)        DSMSMS
039000970526E   1C                   ENDIF
039100970526     C* MODIFICA
039200970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039300970602     C                   MOVEL     VIDNAR        DNAR
039400970602     C                   MOVEL     VIDCAP        DCAP
039500970529     C                   MOVEL     DEPOP0        WOP0
039600970526     C                   MOVEL     'O02'         WOP1
039700970526     C                   MOVEL     'TISI03R'     WPGM
039800970526     C                   EXSR      LANPGM
039900970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
040000970526E   1C                   ENDIF
040100970526     C*
040200970526     C                   ENDSR
040300970917     C*------------------------------------------------------------------------*
040400970917     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
040500970917     C*------------------------------------------------------------------------*
040600970917     C     O03GES        BEGSR
040700970917     C*
040800970917     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
040900970917IF  1C     VIDATB        IFEQ      'A'
041000970917     C                   MOVEL     '1'           ERRL00
041100970917     C                   MOVEL     MSG(4)        DSMSMS
041200970917E   1C                   ENDIF
041300970917     C* DUPLICA
041400970917IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
041500970917     C                   MOVEL     VIDNAR        DNAR
041600970917     C                   MOVEL     VIDCAP        DCAP
041700970917     C                   MOVEL     DEPOP0        WOP0
041800970917     C                   MOVEL     'O03'         WOP1
041900970917     C                   MOVEL     'TISI06R'     WPGM
042000970917     C                   EXSR      LANPGM
042100970917     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
042200970917E   1C                   ENDIF
042300970917     C*
042400970917     C                   ENDSR
042500970526     C*------------------------------------------------------------------------*
042600970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
042700970526     C*------------------------------------------------------------------------*
042800970526     C     O04GES        BEGSR
042900970526     C*
043000970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
043100970526IF  1C     VIDATB        IFEQ      'A'
043200970526     C                   MOVEL     '1'           ERRL00
043300970526     C                   MOVEL     MSG(4)        DSMSMS
043400970526E   1C                   ENDIF
043500970526     C* ANNULLAMENTO
043600970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
043700970602     C                   MOVEL     VIDNAR        DNAR
043800970602     C                   MOVEL     VIDCAP        DCAP
043900970529     C                   MOVEL     DEPOP0        WOP0
044000970526     C                   MOVEL     'O04'         WOP1
044100970526     C                   MOVEL     'TISI03R'     WPGM
044200970526     C                   EXSR      LANPGM
044300970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
044400970526E   1C                   ENDIF
044500970526     C*
044600970526     C                   ENDSR
044700970526     C*------------------------------------------------------------------------*
044800970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
044900970526     C*------------------------------------------------------------------------*
045000970526     C     O05GES        BEGSR
045100970526     C*
045200970602     C                   MOVEL     VIDNAR        DNAR
045300970602     C                   MOVEL     VIDCAP        DCAP
045400970529     C                   MOVEL     DEPOP0        WOP0
045500970526     C                   MOVEL     'O05'         WOP1
045600970526     C                   MOVEL     'TISI03R'     WPGM
045700970526     C                   EXSR      LANPGM
045800970526     C*
045900970526     C                   ENDSR
046000131028     C*------------------------------------------------------------------------*
046100131028     C* O07GES - GESTIONE OPZIONE 7=Orari servizi
046200131028     C*------------------------------------------------------------------------*
046300131028     C     O07GES        BEGSR
046400131028     C*
046500131028     C                   MOVEL     VIDNAR        DNAR
046600131028     C                   MOVEL     VIDCAP        DCAP
046700131028     C                   MOVEL     'TISIO2R'     WPGM
046800131028     C                   EXSR      LANPGM
046900131028     C*
047000131028     C                   ENDSR
047100970526     C*------------------------------------------------------------------------*
047200131028     C* O13GES - GESTIONE OPZIONE 13=RIPRISTINO
047300970526     C*------------------------------------------------------------------------*
047400131028     C     O13GES        BEGSR
047500970526     C*
047600970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
047700970526IF  1C     VIDATB        IFNE      'A'
047800970526     C                   MOVEL     '1'           ERRL00
047900970526     C                   MOVEL     MSG(5)        DSMSMS
048000970526E   1C                   ENDIF
048100970526     C* RIPRISITINO
048200970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
048300970602     C                   MOVEL     VIDNAR        DNAR
048400970602     C                   MOVEL     VIDCAP        DCAP
048500970529     C                   MOVEL     DEPOP0        WOP0
048600970526     C                   MOVEL     'O07'         WOP1
048700970526     C                   MOVEL     'TISI03R'     WPGM
048800970526     C                   EXSR      LANPGM
048900970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
049000970526E   1C                   ENDIF
049100970526     C*
049200970526     C                   ENDSR
049300970526     C*------------------------------------------------------------------------*
049400970526     C* O08GES - GESTIONE OPZIONE 8=LOCALITA'
049500970526     C*------------------------------------------------------------------------*
049600970526     C     O08GES        BEGSR
049700970528     C*
049800970602     C                   MOVEL     VIDNAR        DNAR
049900970602     C                   MOVEL     VIDCAP        DCAP
050000970528     C                   MOVEL     DEPOP0        WOP0
050100970528     C                   MOVEL     'L'           WOP0
050200970526     C                   MOVEL     'O08'         WOP1
050300970526     C                   MOVEL     'TISI30R'     WPGM
050400970526     C                   EXSR      LANPGM
050500970526     C*
050600970526     C                   ENDSR
050700970526     C*------------------------------------------------------------------------*
050800970526     C* O09GES - GESTIONE OPZIONE 9=ECCEZIONI
050900970526     C*------------------------------------------------------------------------*
051000970526     C     O09GES        BEGSR
051100970528     C*
051200970602     C                   MOVEL     VIDNAR        DNAR
051300970602     C                   MOVEL     VIDCAP        DCAP
051400970528     C                   MOVEL     DEPOP0        WOP0
051500970528     C                   MOVEL     'E'           WOP0
051600970526     C                   MOVEL     'O09'         WOP1
051700970526     C                   MOVEL     'TISI11R'     WPGM
051800970526     C                   EXSR      LANPGM
051900970526     C*
052000970526     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* O10GES - GESTIONE OPZIONE 10=TEMPI DI CONSEGNA
052300970526     C*------------------------------------------------------------------------*
052400970526     C     O10GES        BEGSR
052500970526     C*
052600970602     C                   MOVEL     VIDNAR        DNAR
052700970602     C                   MOVEL     VIDCAP        DCAP
052800970528     C                   MOVEL     DEPOP0        WOP0
052900970528     C                   MOVEL     'T'           WOP0
053000970526     C                   MOVEL     'O10'         WOP1
053100970526     C                   MOVEL     'TISI41R'     WPGM
053200970526     C                   EXSR      LANPGM
053300970526     C*
053400970526     C                   ENDSR
053500060403     C*------------------------------------------------------------------------*
053600060403     C* O14GES - GESTIONE calendario eventi particolari
053700060403     C*------------------------------------------------------------------------*
053800060403     C     O14GES        BEGSR
053900060403     C*
054000060403     C                   MOVEL     VIDNAR        DNAR
054100060403     C                   MOVEL     VIDCAP        DCAP
054200060403     C                   MOVEL     DEPOP0        WOP0
054300060403     C                   clear                   WOP1
054400060403     C                   MOVEL     datcor        wdri
054500060403     c                   move      0101          wdri
054600060403     C                   MOVEL     'TISIP0R'     WPGM
054700060403     C                   EXSR      LANPGM
054800060403     C*
054900060403     C                   ENDSR
055000060403     C*------------------------------------------------------------------------*
055100060403     C* O15GES - Inserisci  evento particolare
055200060403     C*------------------------------------------------------------------------*
055300060403     C     O15GES        BEGSR
055400060403     C*
055500060403     C                   MOVEL     VIDNAR        DNAR
055600060403     C                   MOVEL     VIDCAP        DCAP
055700060403     C                   MOVEL     VIDprv        Dprv
055800060403     C                   MOVEL     DEPOP0        WOP0
055900060403     C                   movel     'F06'         WOP1
056000060403     C                   MOVEL     datcor        wdri
056100060403     C                   MOVEL     'TISIP1R'     WPGM
056200060403     C                   EXSR      LANPGM
056300060403     C*
056400060403     C                   ENDSR
056500970526     C*------------------------------------------------------------------------*
056600970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
056700970526     C*------------------------------------------------------------------------*
056800970526     C     NEWPOS        BEGSR
056900970526     C*
057000970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
057100970526     C*         1= RICHIESTO POSIZIONAMENTO
057200970526     C                   Z-ADD     0             SWPOSI            7 0
057300970526     C*
057400970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
057500970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
057600970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
057700970526     C                   Z-ADD     0             SWMODI            1 0
057800970530     C*
057900970530     C* REIMPOSTA INDICATORI DI ERRORE
058000970530     C                   SETOFF                                       808182
058100970530     C                   SETOFF                                       83
058200970526     C*
058300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
058400970526IF  1C     VIDPNA        IFNE      DEPNAR
058500970526     C     VIDPCA        ORNE      DEPCAP
058600970526     C     VIDPLN        ORNE      DEPLNA
058700970526     C     VIDPPR        ORNE      DEPPRV
058800970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
058900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
059000970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
059100970526     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
059200970526     C                   MOVEL     VIDPCA        DEPCAP
059300970526     C                   Z-ADD     VIDPLN        DEPLNA
059400970526     C                   MOVEL     VIDPPR        DEPPRV
059500970526E   2C                   ENDIF
059600970526E   1C                   ENDIF
059700970526     C*
059800970526     C                   ENDSR
059900970526     C*------------------------------------------------------------------------*
060000970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
060100970526     C*------------------------------------------------------------------------*
060200970526     C     SETFIL        BEGSR
060300970526     C*
060400970528     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
060500970526IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
060600970526     C                   Z-ADD     D00VER        KCCVER
060700970526     C                   MOVEL     VIDPNA        KCCNAR
060800970526     C                   MOVEL     VIDPCA        KCCCAP
060900970527     C     KEYCPC        SETLL     AZCPC01L                           99
061000970526E   1C                   ENDIF
061100970526     C*
061200970526IF  1C     WTIPRI        IFEQ      'LNA'                                        *PER LINEA ARRIVO
061300970526     C                   Z-ADD     D00VER        KCCVER
061400970526     C                   MOVEL     VIDPNA        KCCNAR
061500970526     C                   Z-ADD     VIDPLN        KCCLNA
061600970527     C     KE3CPC        SETLL     AZCPC03L                           99
061700970526E   1C                   ENDIF
061800970526     C*
061900970526IF  1C     WTIPRI        IFEQ      'PRV'                                        *PER PROVINCIA
062000970526     C                   Z-ADD     D00VER        KCCVER
062100970526     C                   MOVEL     VIDPNA        KCCNAR
062200970526     C                   MOVEL     VIDPPR        KCCPRV
062300970527     C     KE2CPC        SETLL     AZCPC02L                           99
062400970526E   1C                   ENDIF
062500970526     C* LETTURA RECORD
062600970526IF  1C     *IN99         IFEQ      *OFF
062700970526     C                   EXSR      LETFIL
062800970526E   1C                   ENDIF
062900970526     C*
063000970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
063100970526IF  1C     *IN99         IFEQ      *ON
063200970526     C                   Z-ADD     1             SFLEOF
063300970526E   1C                   ENDIF
063400970526     C*
063500970526     C                   ENDSR
063600970526     C*------------------------------------------------------------------------*
063700970526     C* LETFIL - LETTURA RECORD
063800970526     C*------------------------------------------------------------------------*
063900970526     C     LETFIL        BEGSR
064000970526     C*
064100970526     C* LEGGE FINO A:
064200970526     C*     - FINE FILE
064300970526     C*     - TROVATO RECORD VALIDO
064400970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
064500970526DO  1C     *IN99         DOWEQ     *OFF
064600970526     C     WRECOK        ANDEQ     'N'
064700970526     C*
064800970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
064900970526IF  2C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
065000970603     C     KCCVER        READE     AZCPC01L                               99
065100970526E   2C                   ENDIF
065200970526IF  2C     WTIPRI        IFEQ      'LNA'                                        *PER LINEA ARRIVO
065300970603     C     KCCVER        READE     AZCPC03L                               99
065400970526E   2C                   ENDIF
065500970526IF  2C     WTIPRI        IFEQ      'PRV'                                        *PER PROVINCIA
065600970603     C     KCCVER        READE     AZCPC02L                               99
065700970526E   2C                   ENDIF
065800970526     C*
065900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
066000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
066100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
066200970530X   2C                   ELSE
066300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
066400970530E   2C                   ENDIF
066500970526E   1C                   ENDDO
066600970526     C*
066700970526     C                   ENDSR
066800970526     C*------------------------------------------------------------------------*
066900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
067000970526     C*------------------------------------------------------------------------*
067100970526     C     CHKREC        BEGSR
067200970526     C*
067300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
067400970526     C*
067500970526     C                   ENDSR
067600970526     C*------------------------------------------------------------------------*
067700970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
067800970526     C*------------------------------------------------------------------------*
067900970526     C     IRESFL        BEGSR
068000970526     C*
068100970526     C                   MOVEL     *BLANKS       OPZKEY
068200970526     C                   MOVEL     CPCATB        VIDATB
068300970917     C                   MOVEL     CPCFI1        VIDFI1
068400970526     C                   MOVEL     CPCNAR        VIDNAR
068500970526     C                   MOVEL     CPCCAP        VIDCAP
068600970526     C                   Z-ADD     CPCTTC        VIDTTC
068700970526     C                   MOVEL     CPCISO        VIDISO
068800970526     C                   Z-ADD     CPCLNA        VIDLNA
068900970526     C                   Z-ADD     CPCZNC        VIDZNC
069000970526     C                   MOVEL     CPCPRV        VIDPRV
069100970526     C                   MOVEL     CPCPRA        VIDPRA
069200970526     C                   MOVEL     CPCCTS        VIDCTS
069300970529     C                   MOVEL     CPCCTE        VIDCTE
069400970526     C                   Z-ADD     CPCLOL        VIDLOL
069500970526     C                   Z-ADD     CPCZOL        VIDZOL
069600970604     C                   Z-ADD     CPCLOS        VIDLOS
069700970604     C                   Z-ADD     CPCZOS        VIDZOS
069800970526     C                   MOVEL     CPCFLG        VIDFLG
069900970526     C                   MOVEL     CPCFIT        VIDFIT
070000970528     C                   MOVEL     *BLANKS       VIDESE
070100970527IF  1C     CPCESE        IFEQ      'S'
070200970526     C                   MOVEL     'E'           VIDESE                         *ESISTONO ECCEZIONI
070300970527E   1C                   ENDIF
070400970528     C                   MOVEL     *BLANKS       VIDESL
070500970527IF  1C     CPCESL        IFEQ      'S'
070600970527     C                   MOVEL     'L'           VIDESL                         *LOCALITA' GESTITE
070700970527E   1C                   ENDIF
070800130828     c* esistenza eccezioni di orari servizi per il CAP
070900130823     C                   MOVEL     *BLANKS       VIDESos
071000130828     C**                 MOVEL     *BLANKS       VIhcos
071100130828     c                   setoff                                       02
071200130823     C                   MOVEL     cpccap        KCCCAP
071300130828     c                   clear                   kloc
071400130828     c     kcos          chain     azcos01l
071500130828     c                   if        %found(azcos01l)
071600130828     C                   MOVEL     'C'           VIDESos                        *eccez.orari serviz
071700130823E   1C                   ENDIF
071800130828     c* esistenza eccezioni di orari servizi per località
071900130828     C                   MOVEL     cpccap        KCCCAP
072000130828     c                   movel     'A'           kloc
072100130828     c     kcos          setll     azcos01l
072200130828     c     keycpc        reade     azcos01l
072300130828     c                   if        not %eof(azcos01l)
072400130828     C                   MOVE      'L'           VIDESos                        *eccez.orari serviz
072500130828     C**                 MOVEL     'S'           VIhcos
072600130828     c                   seton                                        02
072700130828E   1C                   ENDIF
072800130823     c
072900970526     C*
073000970526     C                   ENDSR
073100970526     C*------------------------------------------------------------------------*
073200970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
073300970526     C*------------------------------------------------------------------------*
073400970526     C     CHKPAR        BEGSR
073500970526     C*
073600970526     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
073700970526IF  1C     VIDPCA        IFEQ      *BLANKS
073800970526     C     VIDPLN        ANDEQ     *ZEROS
073900970526     C     VIDPPR        ANDEQ     *BLANKS
074000970526     C                   MOVEL     'CAP'         WTIPRI            3
074100970526E   1C                   ENDIF
074200970526     C*
074300970526     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
074400970526IF  1C     ERRL00        IFEQ      '0'
074500970526IF  2C     VIDPCA        IFNE      *BLANKS
074600970526     C     VIDPLN        ANDNE     *ZEROS
074700970526     C                   MOVEL     MSG(6)        DSMSMS
074800970526     C                   MOVEL     '1'           ERRL00
074900970529     C                   SETON                                        808182
075000970526E   2C                   ENDIF
075100970526E   1C                   ENDIF
075200970526     C*
075300970526IF  1C     ERRL00        IFEQ      '0'
075400970526IF  2C     VIDPCA        IFNE      *BLANKS
075500970526     C     VIDPPR        ANDNE     *BLANKS
075600970526     C                   MOVEL     MSG(6)        DSMSMS
075700970526     C                   MOVEL     '1'           ERRL00
075800970529     C                   SETON                                        808183
075900970526E   2C                   ENDIF
076000970526E   1C                   ENDIF
076100970526     C*
076200970526IF  1C     ERRL00        IFEQ      '0'
076300970526IF  2C     VIDPLN        IFNE      *ZEROS
076400970526     C     VIDPPR        ANDNE     *BLANKS
076500970526     C                   MOVEL     MSG(6)        DSMSMS
076600970526     C                   MOVEL     '1'           ERRL00
076700970529     C                   SETON                                        8283
076800970526E   2C                   ENDIF
076900970526E   1C                   ENDIF
077000970526     C*
077100970526     C* CONTROLLA IL TIPO RICERCA
077200970526IF  1C     ERRL00        IFEQ      '0'
077300970526IF  2C     VIDPCA        IFNE      *BLANKS
077400970526     C                   MOVEL     'CAP'         WTIPRI
077500970526E   2C                   ENDIF
077600970526IF  2C     VIDPLN        IFNE      *ZEROS
077700970526     C                   MOVEL     'LNA'         WTIPRI
077800970526E   2C                   ENDIF
077900970526IF  2C     VIDPPR        IFNE      *BLANKS
078000970526     C                   MOVEL     'PRV'         WTIPRI
078100970526E   2C                   ENDIF
078200970526E   1C                   ENDIF
078300970526     C*
078400970526     C                   ENDSR
078500970526     C*------------------------------------------------------------------------*
078600970526     C* LANPGM - LANCIA UN PROGRAMMA
078700970526     C*------------------------------------------------------------------------*
078800970526     C     LANPGM        BEGSR
078900970526     C*
079000970526     C* IMPOSTA LA DS DI PROCEDURA
079100060403     c                   eval      tipopgm=%subst(wpgm:5:1)
079200060403     c                   if        tipopgm='P'
079300060403     c                   clear                   tisip0ds
079400060403     C                   MOVEL     DNAR          Dp0NAR                         *NAZIONE
079500060403     C                   MOVEL     DCAP          Dp0CAP                         *CAP
079600060403     C                   MOVEL     Dprv          Dp0prv                         *CAP
079700060403     C                   MOVEL     wop0          dp0OP0
079800060403     C                   MOVEL     WOP1          Dp0OP1                         *LIVELLO PGM
079900060403     c                   movel     wdri          dp0dri
080000060403     c                   call      WPGM
080100060403     c                   parm                    kpjba
080200060403     c                   parm                    tisip0ds
080300060403     C*
080400060403     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
080500060403IF  1C     Dp0F03        IFEQ      '1'                                          *PREMUTO F03
080600060403     C                   MOVEL     '1'           ERRL00                         *ERRORE
080700060403     C                   MOVEL     '1'           F03L00                         *FINE PGM
080800060403     c                   movel     dp0f03        d00f03
080900060403X   1C                   ELSE                                                   *NON PREMUTO F03
081000060403     C                   MOVEL     Dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
081100060403E   1C                   ENDIF
081200060403     c
081300060403     c                   else
081400131028     c                   if        tipopgm='O'
081500131028     c                   clear                   tisiO0ds
081600131028     c                   if        %subst(depop0:2:2)='02'
081700131028     C                   MOVEL     'G'           do0tric
081800131028     c                   else
081900131028     C                   MOVEL     'I'           do0tric
082000131028     c                   endif
082100131028     c                   move      d00ver        do0ver
082200131028     C                   MOVEL     DNAR          DO0NAR                         *NAZIONE
082300131028     C                   MOVEL     DCAP          DO0CAP                         *CAP
082400131028     c                   call      WPGM
082500131028     c                   parm                    kpjba
082600131028     c                   parm                    tisio0ds
082700131028     C*
082800131028     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
082900131028IF  1C     DO0F03        IFEQ      '1'                                          *PREMUTO F03
083000131028     C                   MOVEL     '1'           ERRL00                         *ERRORE
083100131028     C                   MOVEL     '1'           F03L00                         *FINE PGM
083200131028     c                   movel     dO0f03        d00f03
083300131028X   1C                   ELSE                                                   *NON PREMUTO F03
083400131028     C                   MOVEL     DO0MSG        DSMSMS                         *MESSAGGIO DI RITORN
083500131028E   1C                   ENDIF
083600131028     c                   else
083700970528     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
083800970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
083900970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
084000970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
084100970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
084200970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
084300970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
084400970602     C                   MOVEL     DCAP          D00CAP                         *CAP
084500970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
084600970530     C                   MOVEL     *BLANKS       D00TSP
084700970530     C                   Z-ADD     *ZEROS        D00TFP
084800970530     C                   MOVEL     *BLANKS       D00LOC
084900970603     C                   MOVEL     *BLANKS       D00SIN
085000970530     C                   MOVEL     *BLANKS       D00EPA
085100970530     C                   Z-ADD     *ZEROS        D00ETP
085200970530     C                   Z-ADD     *ZEROS        D00ETA
085300970530     C                   Z-ADD     *ZEROS        D00EDD
085400970530     C                   MOVEL     *BLANKS       D00SPT
085500970530     C                   Z-ADD     *ZEROS        D00SLP
085600970530     C                   Z-ADD     *ZEROS        D00SLA
085700970530     C                   MOVEL     *BLANKS       D00STS
085800970530     C                   Z-ADD     *ZEROS        D00SDD
085900970526     C*
086000970526     C                   CALL      WPGM
086100970612     C                   PARM                    KPJBA
086200970612     C                   PARM                    DSSI00
086300970526     C*
086400970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
086500970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
086600970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
086700970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
086800970526X   1C                   ELSE                                                   *NON PREMUTO F03
086900970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
087000970526E   1C                   ENDIF
087100060403     c                   endif
087200131028     c                   endif
087300970526     C*
087400970526     C                   ENDSR
087500970527     C*------------------------------------------------------------------------*
087600970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
087700970527     C*------------------------------------------------------------------------*
087800970527     C     CARTAB        BEGSR
087900970527     C*
088000131028      /free
088100131028        // Carico filiali di test abilitate
088200131028         if dutpou<>46  ;
088300131028
088400131028         clear TIBS02ds;
088500131028         T02Mod = 'C';
088600131028         T02sif = knsif;
088700131028         T02cod = 'VPO';
088800131028         t02tla = 'L'   ;
088900131028         T02ke1 ='DECOFI816' ;
089000131028         TNTBE_RicercaControllo  (kpjba : tibs02ds);
089100131028
089200131028         if  T02err  = *blank;
089300131028         xx= 1 ;
089400131028         yy= 1 ;
089500131028
089600131028         dow xx< 256   ;
089700131028           fil_ab(yy)= %subst(t02uni:xx:3)  ;
089800131028           yy=yy+ 1   ;
089900131028           xx=xx+ 3   ;
090000131028         enddo  ;
090100131028
090200131028         endif;
090300131028
090400131028         // se c'e' 999 tutte abilitate
090500131028         w003a='999'  ;
090600131028         if   %lookup(w003a:fil_ab) = 0  ;
090700131028         w003a=%editc(dutpou:'X')   ;
090800131028         if   %lookup(w003a:fil_ab) = 0  ;
090900131028         *in10 =*on  ;
091000131028         endif  ;
091100131028         endif  ;
091200131028
091300131028         endif  ;
091400131028
091500131028      /end-free
091600970527     C                   ENDSR
091700970526     C*------------------------------------------------------------------------*
091800970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
091900970526     C*------------------------------------------------------------------------*
092000970526     C     CHKFOP        BEGSR
092100061026     C*
092200061026     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
092300061026     C                   SETON                                        93
092400061026     C*
092500170116     C                   IF        wabiute = 'C'
092600061026     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
092700061026     C                   SETOFF                                       93
092800061026     C                   ENDIF
092900170116     c* Se utente abilitato alla mantenzione futura, verifico dalla versione se ancora
093000170116     c*  da decorrere
093100170116     c                   if        wabiute='V'
093200170116     c     d00ver        chain     azcpr01l
093300170116     c                   if        %found(azcpr01l) and cprdde>datcor
093400170116     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
093500170116     C                   SETOFF                                       93
093600170116     c                   else
093700170116     c                   eval      wabiute='I'
093800170116     C                   ENDIF
093900170116     C                   ENDIF
094000970526     C*
094100970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
094200970526IF  1C     DEPOP0        IFEQ      'C01'
094300970526     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
094400970526     C                   MOVEL     'S'           OPDATT
094500970526     C                   MOVEL     OPDRIG        OPD(1)
094600970626X   1C                   ELSE
094700970626     C* OPZIONI
094800970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
094900970626     C                   MOVEL     'S'           OPDATT
095000970626     C                   MOVEL     OPDRIG        OPD(5)
095100970626     C                   MOVEL     OPD(8)        OPDRIG                         *LOCALITA'
095200970626     C                   MOVEL     'S'           OPDATT
095300970626     C                   MOVEL     OPDRIG        OPD(8)
095400970626     C                   MOVEL     OPD(9)        OPDRIG                         *ECCEZIONI
095500970626     C                   MOVEL     'S'           OPDATT
095600970626     C                   MOVEL     OPDRIG        OPD(9)
095700970626     C                   MOVEL     OPD(10)       OPDRIG                         *TEMPI CONSEGNA
095800970626     C                   MOVEL     'S'           OPDATT
095900970626     C                   MOVEL     OPDRIG        OPD(10)
096000970626     C* FUNZIONI
096100970626     C                   MOVEL     FUD(8)        FUDRIG                         *LOCALITA'
096200970626     C                   MOVEL     'S'           FUDATT
096300970626     C                   MOVEL     FUDRIG        FUD(8)
096400970626     C                   MOVEL     FUD(9)        FUDRIG                         *ECCEZIONI
096500970626     C                   MOVEL     'S'           FUDATT
096600970626     C                   MOVEL     FUDRIG        FUD(9)
096700970626     C                   MOVEL     FUD(10)       FUDRIG                         *TEMPI CONSEGNA
096800970626     C                   MOVEL     'S'           FUDATT
096900970626     C                   MOVEL     FUDRIG        FUD(10)
097000970526E   1C                   ENDIF
097100131028     c* Se utente non abilitato, non attivo opzione 7=orari servizi
097200131028     c* lascio in manutenzione
097300131028     c                   if        *in10 and  depop0 = 'C05'
097400131028     C                   MOVEL     OPD(7)        OPDRIG
097500131028     C                   MOVEL     'N'           OPDATT
097600131028     C                   MOVEL     OPDRIG        OPD(7)
097700131028     c                   endif
097800970526     C*
097900970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
098000970526IF  1C     DEPOP0        IFEQ      'C02'
098100970526     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
098200970526     C                   MOVEL     'S'           OPDATT
098300970526     C                   MOVEL     OPDRIG        OPD(2)
098400170120     c
098500170120     c* la duplica solo per manut completa
098600170120     C  n93              MOVEL     OPD(3)        OPDRIG                         *DUPLICA
098700170120     C  n93              MOVEL     'S'           OPDATT
098800170120     C  n93              MOVEL     OPDRIG        OPD(3)
098900170120
099000061026     C  N93              MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
099100061026     C  N93              MOVEL     'S'           OPDATT
099200061026     C  N93              MOVEL     OPDRIG        OPD(4)
099300131028     c* non essendoci più l'annullamento logico il ripristino diventa inutile
099400131028     c* quindi asterisco le seguenti specifiche
099500131028     C* N93              MOVEL     OPD(13)       OPDRIG                         *RIPRISTINO
099600131028     C* N93              MOVEL     'S'           OPDATT
099700131028     C* N93              MOVEL     OPDRIG        OPD(13)
099800060403     C                   MOVEL     opd(15)       opdRIG                         *INSERIMENTO
099900060403     C                   MOVEL     'S'           opdATT
100000131028     C                   MOVEL     opdRIG        opd(15)
100100061016     C* CONSENTITO INSERIMENTO SOLO A PERSONALE "EDP"
100200170116     C                   IF        %subst(KNMUS:1:3) = 'EDD' AND *IN93 = *off
100300970526     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
100400970526     C                   MOVEL     'S'           FUDATT
100500970526     C                   MOVEL     FUDRIG        FUD(6)
100600061016     C                   ENDIF
100700970526E   1C                   ENDIF
100800970526     C*
100900970526     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
101000060403IF  1C**   DUTLPO        IFNE      'S'                                          *FILIALE
101100060403     C**                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
101200060403     C**                 MOVEL     'N'           OPDATT
101300060403     C**                 MOVEL     OPDRIG        OPD(4)
101400060403     C**                 MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
101500060403     C**                 MOVEL     'N'           OPDATT
101600060403     C**                 MOVEL     OPDRIG        OPD(7)
101700060403     C**                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
101800060403     C**                 MOVEL     'N'           FUDATT
101900060403     C**                 MOVEL     FUDRIG        FUD(6)
102000060403E   1C**                 ENDIF
102100970526     C*
102200970526     C* GESTIONE OPZIONI A VIDEO
102300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
102400970526     C                   EXSR      OPZGES
102500970526     C*
102600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
102700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
102800970526     C                   EXSR      FUNGES
102900970526     C*
103000970526     C                   ENDSR
103100020916     C*--------------------------------------------------------------------------------------------*
103200020916     C* REPERISCE I DATI UTENTE
103300020916     C*--------------------------------------------------------------------------------------------*
103400020916     C     REPDATIUTE    BEGSR
103500020916     C*
103600020916     C* INIZIALIZZA VARIABILI DI WRK
103700020916     C                   CLEAR                   TIBS34DS
103800020916     C                   CLEAR                   AZUTEDS
103900020916     C                   CLEAR                   DDATIUTE
104000020916     C*
104100020916     C     *DTAARA       DEFINE    §azute        azuteds
104200020916     C     *DTAARA       DEFINE    §datiute      ddatiute
104300020916     C                   IN(E)     *DTAARA
104400020916if  1C                   IF        %Error
104500020916     c                   EVAL      I34Tla = 'L'
104600020916     C                   CALL      'TIBS34R'
104700020916     C                   PARM                    Tibs34Ds
104800020916     C                   IN        *DTAARA
104900020916e   1C                   ENDIF
105000020916     C*
105100020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
105200020916     C                   MOVEL(P)  RSUT          DSFIRS           20
105300061026     C*
105400061026     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
105500061026     C                   EVAL      DUTE01 = UTEFAF
105600020916     C*
105700020916     C                   ENDSR
105800970526     C*------------------------------------------------------------------------*
105900970526     C* *INZSR - OPERAZIONI INIZIALI
106000970526     C*------------------------------------------------------------------------*
106100970526     C     *INZSR        BEGSR
106200970526     C*---
106300970526     C* RICEVIMENTO PARAMETRI
106400970526     C*---
106500970526     C     *ENTRY        PLIST
106600970612     C                   PARM                    KPJBA
106700970612     C                   PARM                    DSSI00
106800970526     C*
106900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
107000970526     C     XMSGPA        PLIST
107100970526     C                   PARM                    DSMSMS
107200970526     C                   PARM                    DSMSPG
107300970526     C                   PARM                    DSMSKE
107400970526     C                   PARM                    DSMSER
107500970526     C*
107600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
107700970526     C                   CALL      'XKEY'
107800970526     C                   PARM                    DSKEY
107900970526     C*---
108000970526     C* VARIABILI NON RIFERITE AL DATA BASE
108100970526     C*---
108200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
108300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
108400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
108500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
108600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
108700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
108800970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
108900970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
109000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
109100970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
109200970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
109300970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
109400970529     C     *LIKE         DEFINE    D00OP1        WOP1
109500970602     C     *LIKE         DEFINE    D00NAR        DNAR
109600970602     C     *LIKE         DEFINE    D00CAP        DCAP
109700970526     C*---
109800970526     C* VARIABILI DI CONTROLLO
109900970526     C*---
110000970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
110100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
110200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
110300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
110400970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
110500970526     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
110600970526     C*
110700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
110800970526     C                   MOVEL     'TISI02R'     DSMSPG
110900970526     C                   MOVEL     'TISI02R'     MSGQUE
111000970526     C                   MOVEL     *BLANKS       MSGKEY
111100970526     C                   MOVEL     *BLANKS       DSMSMS
111200970526     C                   MOVEL     *BLANKS       DSMSKE
111300970526     C                   Z-ADD     *ZEROS        DSMSER
111400970526     C*---
111500970526     C* VARIABILI RIFERITE AL DATA BASE
111600970526     C*---
111700970526     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
111800970526     C     *LIKE         DEFINE    CPCNAR        KCCNAR
111900970526     C     *LIKE         DEFINE    CPCCAP        KCCCAP
112000970526     C     *LIKE         DEFINE    CPCPRV        KCCPRV
112100970526     C     *LIKE         DEFINE    CPCLNA        KCCLNA
112200970526     C     *LIKE         DEFINE    CPCNAR        DEPNAR                         *DEPOSITI
112300970526     C     *LIKE         DEFINE    CPCCAP        DEPCAP
112400970526     C     *LIKE         DEFINE    CPCLNA        DEPLNA
112500970526     C     *LIKE         DEFINE    CPCPRV        DEPPRV
112600970526     C*---
112700970526     C* CHIAVI DI LETTURA
112800970526     C*---
112900970526     C* LETTURA AZCPC01L - COMPLETA
113000970527     C     KEYCPC        KLIST
113100970526     C                   KFLD                    KCCVER                         *VERSIONE
113200970526     C                   KFLD                    KCCNAR                         *NAZIONE
113300970526     C                   KFLD                    KCCCAP                         *CAP
113400970526     C* LETTURA AZCPC02L - PARZIALE
113500970527     C     KE2CPC        KLIST
113600970526     C                   KFLD                    KCCVER                         *VERSIONE
113700970526     C                   KFLD                    KCCNAR                         *NAZIONE
113800970526     C                   KFLD                    KCCPRV                         *PROVINCIA
113900970526     C* LETTURA AZCPC03L - PARZIALE
114000970527     C     KE3CPC        KLIST
114100970526     C                   KFLD                    KCCVER                         *VERSIONE
114200970526     C                   KFLD                    KCCNAR                         *NAZIONE
114300970526     C                   KFLD                    KCCLNA                         *LINEA ARRIVO
114400130823     C     Kcos          KLIST
114500130823     C                   KFLD                    KCCVER                         *VERSIONE
114600130823     C                   KFLD                    KCCNAR                         *NAZIONE
114700130823     C                   KFLD                    KCCCAP                         *CAP
114800130823     C                   KFLD                    Kloc
114900970527     C*
115000970527     C* REPERIMENTO DATI UTENTE
115100020916     C                   EXSR      REPDATIUTE
115200170116     c
115300170116     c* Imposto il flag della abilitazione
115400170116     c                   eval      wabiute=§utecap
115500970527     C*
115600970527     C* CARICA LE TABELLE OCCORRENTI
115700970527     C                   EXSR      CARTAB
115800970526     C*
115900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
116000970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
116100970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
116200970526     C*
116300970526     C* CALCOLA LA DATA CORRENTE
116400970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
116500970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
116600970526     C                   Z-ADD     WN8           G08DAT
116700970526     C                   Z-ADD     *ZEROS        G08INV
116800970526     C                   MOVEL     '0'           G08ERR
116900970526     C                   CALL      'XSRDA8'
117000970526     C                   PARM                    WLBDA8
117100970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
117200970526     C*
117300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
117400970526     C                   Z-ADD     D00DRI        G08INV
117500970526     C                   Z-ADD     *ZEROS        G08DAT
117600970526     C                   MOVEL     '3'           G08ERR
117700970526     C                   CALL      'XSRDA8'
117800970526     C                   PARM                    WLBDA8
117900970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
118000970526     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
118100970526     C                   MOVEL     D00CAP        VIDPCA                         *CAP
118200970526     C                   EXSR      CHKPAR
118300970526IF  1C     ERRL00        IFEQ      '1'
118400970526     C                   MOVEL     '1'           D00ERR
118500970526     C                   MOVEL     DSMSMS        D00MSG
118600970526X   1C                   ELSE
118700970526     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
118800970526     C                   MOVEL     VIDPCA        DEPCAP
118900970526E   1C                   ENDIF
119000970703     C*
119100970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
119200970703IF  1C     D00VER        IFEQ      *ZEROS
119300970707     C                   Z-ADD     D00DRI        P96DRI            8 0
119400970707     C                   Z-ADD     *ZEROS        P96VER            5 0
119500970707     C                   CALL      'TISI96R'
119600970707     C                   PARM                    P96DRI
119700970707     C                   PARM                    P96VER
119800970707     C                   Z-ADD     P96VER        D00VER
119900970703E   1C                   ENDIF
120000970526     C*
120100970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
120200970526     C                   EXSR      CHKFOP
120300970530     C*
120400970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
120500970530IF  1C     DEPOP0        IFEQ      'C01'
120600970530     C                   MOVEL     DEC(3)        VIDING
120700970530E   1C                   ENDIF
120800970530IF  1C     DEPOP0        IFEQ      'C02'
120900970530     C                   MOVEL     DEC(1)        VIDING
121000970530E   1C                   ENDIF
121100970530IF  1C     DEPOP0        IFEQ      'C05'
121200970530     C                   MOVEL     DEC(2)        VIDING
121300970530E   1C                   ENDIF
121400970526     C*
121500970526     C                   ENDSR
121600970526     C*------------------------------------------------------------------------*
121700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
121800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
121900970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
122000970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
122100970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
122200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
122300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
122400970526     C*------------------------------------------------------------------------*
122500970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
122600970526     C*------------------------------------------------------------------------*
122700970526     C     POSSFL        BEGSR
122800970526     C*
122900970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
123000970526     C                   EXSR      CLRSFL
123100970526     C*
123200970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
123300970526     C                   EXSR      SETFIL
123400970526     C*
123500970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
123600970526     C     SFLEOF        IFEQ      0
123700970526     C                   EXSR      CARSFL
123800970526     C                   END
123900970526     C*
124000970526     C                   ENDSR
124100970526     C*------------------------------------------------------------------------*
124200970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
124300970526     C*------------------------------------------------------------------------*
124400970526     C     ENTATT        BEGSR
124500970526     C*
124600970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
124700970526     C                   Z-ADD     0             OPZOK             1 0
124800970526     C*
124900970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
125000970526     C                   Z-ADD     0             SFLVAR            5 0
125100970526     C*
125200970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
125300970526     C                   Z-ADD     0             SFLERR
125400970526     C*
125500970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
125600970526     C                   Z-ADD     SFLULT        WFLULT
125700970526     C*
125800970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
125900970526     C                   READC     SI02D05                                98
126000970526     C     *IN98         DOWEQ     '0'
126100970526     C*
126200970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
126300970526     C                   ADD       1             SFLVAR
126400970526     C*
126500970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
126600970526     C     OPZOK         IFEQ      0
126700970526     C*
126800970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
126900970526     C     OPZKEY        IFNE      *BLANKS
127000970526     C                   EXSR      OPZCON
127100970526     C                   END
127200970526     C*
127300970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
127400970526     C*            2. PULIZIA SCELTA IN SFL
127500970526     C     OPZOK         IFEQ      0
127600970526     C                   MOVEL     '0'           *IN41
127700970526     C                   MOVEL     *BLANKS       OPZKEY
127800970526     C                   ELSE
127900970526     C*
128000970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
128100970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
128200970526     C                   MOVEL     '1'           *IN41
128300970526     C                   Z-ADD     SFLULT        SFLERR
128400970526     C                   END
128500970526     C                   ELSE
128600970526     C*
128700970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
128800970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
128900970526     C                   MOVEL     '1'           *IN41
129000970526     C                   END                                                    OPZOK=0
129100970526     C*
129200970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
129300970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
129400970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
129500970526     C     OPZOK         IFNE      0
129600970526     C                   Z-ADD     SFLERR        SFLNBR
129700970526     C                   ELSE
129800970526     C                   Z-ADD     SFLULT        SFLNBR
129900970526     C                   END
130000970526     C*
130100970526     C* AGGIORNO RECORD SFL
130200130828     c**                 if        vihcos='S'
130300130828     c*+                 seton                                        02
130400130828     c**                 else
130500130828     c**                 setoff                                       02
130600130828     c**                 endif
130700130828     c
130800970526     C                   UPDATE    SI02D05
130900970526     C*
131000970526     C                   READC     SI02D05                                98
131100970526     C                   END                                                    DO WHILE
131200970526     C*
131300970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
131400970526     C                   Z-ADD     WFLULT        SFLULT
131500970526     C*
131600970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
131700970526     C     SFLVAR        IFEQ      0
131800970526     C*---                 MOVEL'1'       FINL00
131900970526     C                   END
132000970526     C*
132100970526     C                   ENDSR
132200970526     C*------------------------------------------------------------------------*
132300970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
132400970526     C*------------------------------------------------------------------------*
132500970526     C     CLRSFL        BEGSR
132600970526     C*
132700970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
132800970526     C* (0=NON FINE FILE  1=FINE FILE)
132900970526     C                   Z-ADD     0             SFLEOF
133000970526     C*
133100970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
133200970526     C                   Z-ADD     0             SFLULT
133300970526     C*
133400970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
133500970526     C                   MOVEL     '1'           *IN40
133600970526     C                   WRITE     SI02D06
133700970526     C                   MOVEL     '0'           *IN40
133800970526     C*
133900970526     C                   ENDSR
134000970526     C*------------------------------------------------------------------------*
134100970526     C* CARSFL - CARICAMENTO SFL
134200970526     C*------------------------------------------------------------------------*
134300970526     C     CARSFL        BEGSR
134400970526     C*
134500970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
134600970526     C     SFLULT        IFEQ      0
134700970526     C                   EXSR      SETFIL
134800970526     C                   END
134900970526     C*
135000970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
135100970526     C                   Z-ADD     SFLULT        WFLULT
135200970526     C*
135300970526     C* CICLO:
135400970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
135500970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
135600970526     C                   Z-ADD     1             SFLRIG
135700970526     C     SFLRIG        DOWLE     SFLPAG
135800970526     C     SFLEOF        ANDEQ     0
135900970526     C*
136000970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
136100970526     C                   MOVEL     *BLANKS       OPZKEY
136200970526     C*
136300970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
136400970526     C                   EXSR      IRESFL
136500970526     C*
136600970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
136700970526     C                   ADD       1             SFLULT
136800970526     C*
136900970526     C* SCRITTURA RECORD SFL
137000970526     C                   WRITE     SI02D05
137100970526     C                   ADD       1             SFLRIG
137200970526     C*
137300970526     C* LETTURA FILE DA CARICARE
137400970526     C                   EXSR      LETFIL
137500970526     C                   END                                                    DO WHILE
137600970526     C*
137700970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
137800970526     C     WFLULT        IFNE      SFLULT
137900970526     C                   Z-ADD     WFLULT        SFLNBR
138000970526     C                   ADD       1             SFLNBR
138100970526     C                   END
138200970526     C*
138300970526     C                   ENDSR
138400970526     C*------------------------------------------------------------------------*
138500970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
138600970526     C*------------------------------------------------------------------------*
138700970526     C     OUTSFL        BEGSR
138800970526     C*
138900970526     C* WRITE RECORD TESTATA VIDEO
139000970526     C                   WRITE     SI02D01
139100970526     C*
139200970526     C* VISUALIZZAZIONE MESSAGGIO
139300970526     C                   EXSR      SNDMSG
139400970526     C*
139500970526     C* WRITE RECORD OPZIONI VIDEO
139600970526     C                   WRITE     SI02D02
139700970526     C*
139800970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
139900970526     C                   WRITE     SI02D03
140000970526     C*
140100970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
140200970526     C                   WRITE     SI02D04
140300970526     C*
140400970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
140500970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
140600970526     C     SFLEOF        IFEQ      0
140700970526     C                   MOVEL     '0'           *IN42
140800970526     C                   ELSE
140900970526     C*
141000970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
141100970526     C                   MOVEL     '1'           *IN42
141200970526     C                   END
141300970526     C*
141400970526     C* WRITE RECORD TASTI FUNZIONALI
141500970526     C                   WRITE     SI02D09
141600970526     C*
141700970526     C* CASO DI SFL VUOTO
141800970526     C     SFLULT        IFLE      0
141900970526     C*
142000970526     C*    - WRITE RECORD VUOTO
142100970526     C                   WRITE     SI02D0A
142200970526     C                   READ      SI02D0A                                99
142300970526     C*
142400970526     C                   ELSE
142500970526     C*
142600970526     C* CASO DI SFL CON RECORDS
142700970526     C*
142800970526     C*    - WRITE RECORD CONTROLLO SFL
142900970526     C                   WRITE     SI02D06
143000970526     C                   READ      SI02D06                                99
143100970526     C                   END
143200970526     C*
143300970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
143400970526     C                   READ      SI02D03                                99
143500970526     C*
143600970526     C                   ENDSR
143700970526     C*------------------------------------------------------------------------*
143800970526     C* SNDMSG - MANDA MESSAGGIO
143900970526     C*------------------------------------------------------------------------*
144000970526     C     SNDMSG        BEGSR
144100970526     C*
144200970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
144300970526     C                   MOVEL     '1'           *IN40
144400970526     C                   WRITE     SI02DMC
144500970526     C                   MOVEL     '0'           *IN40
144600970526     C*
144700970526     C* VISUALIZZAZIONE MESSAGGIO
144800970526     C                   Z-ADD     0             MSGULT
144900970526     C                   CALL      'XMSG'        XMSGPA
145000970526     C     DSMSER        IFEQ      0
145100970526     C                   MOVEL     DSMSKE        MSGKEY
145200970526     C                   END
145300970526     C                   Z-ADD     1             MSGULT
145400970526     C                   WRITE     SI02DMS                                      SFL MESS
145500970526     C                   WRITE     SI02DMC                                      CONTR MESS
145600970526     C*
145700970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
145800970526     C                   MOVEL     *BLANKS       DSMSMS
145900970526     C*
146000970526     C                   ENDSR
146100970526     C*------------------------------------------------------------------------*
146200970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
146300970526     C*------------------------------------------------------------------------*
146400970526     C     FUNCON        BEGSR
146500970526     C*
146600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
146700970526     C                   EXSR      FUNVAR
146800970526     C*
146900970526     C* PULISCO CAMPI DI LAVORO
147000970526     C                   Z-ADD     0             I                 5 0
147100970526     C                   Z-ADD     0             W                 5 0
147200970526     C*
147300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
147400970526     C     FUNKEY        IFNE      *BLANKS
147500970526     C     1             DO        24            I
147600970526     C                   MOVEL     FUD(I)        FUDRIG
147700970526     C*
147800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
147900970526     C     FUNKEY        IFEQ      FUDFUN
148000970526     C     FUDATT        ANDEQ     'S'
148100970526     C                   Z-ADD     I             W
148200970526     C                   END
148300970526     C                   END                                                    DO WHILE
148400970526     C*
148500970526     C* FUNZIONE NON ATTIVA (ERRORE)
148600970526     C     W             IFEQ      0
148700970526     C*
148800970526     C* EMISSIONE MESSAGGIO DI ERRORE
148900970526     C                   MOVEL     MSG(2)        DSMSMS
149000970526     C                   END                                                    W=0
149100970526     C                   END                                                    FUNKEY NE *BLANKS
149200970526     C*
149300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
149400970526     C     W             IFNE      0
149500970526     C     FUNKEY        OREQ      *BLANKS
149600970526     C*
149700970526     C     FUNKEY        IFEQ      *BLANKS
149800970526     C                   EXSR      ENTGES
149900970526     C                   END
150000970526     C     FUNKEY        IFEQ      '03'
150100970526     C                   EXSR      F03GES
150200970526     C                   END
150300970526     C     FUNKEY        IFEQ      '05'
150400970526     C                   EXSR      F05GES
150500970526     C                   END
150600970526     C     FUNKEY        IFEQ      '06'
150700970526     C                   EXSR      F06GES
150800970526     C                   END
150900970528     C     FUNKEY        IFEQ      '08'
151000970528     C                   EXSR      F08GES
151100970528     C                   END
151200970528     C     FUNKEY        IFEQ      '09'
151300980225     C                   EXSR      F09GES
151400970528     C                   END
151500970528     C     FUNKEY        IFEQ      '10'
151600980225     C                   EXSR      F10GES
151700970528     C                   END
151800060403     C     FUNKEY        IFEQ      '14'
151900060403     C                   EXSR      F14GES
152000060403     C                   END
152100060927     C     FUNKEY        IFEQ      '15'
152200060927     C                   EXSR      F15GES
152300060927     C                   END
152400970526     C     FUNKEY        IFEQ      '12'
152500970526     C                   EXSR      F12GES
152600970526     C                   END
152700970526     C     FUNKEY        IFEQ      '21'
152800970526     C                   EXSR      F21GES
152900970526     C                   END
153000970526     C     FUNKEY        IFEQ      '22'
153100970526     C                   EXSR      F22GES
153200970526     C                   END
153300970526     C     FUNKEY        IFEQ      '23'
153400970526     C                   EXSR      F23GES
153500970526     C                   END
153600970526     C     FUNKEY        IFEQ      '24'
153700970526     C                   EXSR      F24GES
153800970526     C                   END
153900970526     C     FUNKEY        IFEQ      'RU'
154000970526     C                   EXSR      RUPGES
154100970526     C                   END
154200970526     C*
154300970526     C                   END
154400970526     C*
154500970526     C                   ENDSR
154600970526     C*------------------------------------------------------------------------*
154700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
154800970526     C*------------------------------------------------------------------------*
154900970526     C     FUNVAR        BEGSR
155000970526     C*
155100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
155200970526     C     KEY           IFEQ      ENTER
155300970526     C                   MOVEL     *BLANKS       FUNKEY
155400970526     C                   END
155500970526     C     KEY           IFEQ      F1
155600970526     C                   MOVEL     '01'          FUNKEY
155700970526     C                   END
155800970526     C     KEY           IFEQ      F2
155900970526     C                   MOVEL     '02'          FUNKEY
156000970526     C                   END
156100970526     C     KEY           IFEQ      F3
156200970526     C                   MOVEL     '03'          FUNKEY
156300970526     C                   END
156400970526     C     KEY           IFEQ      F4
156500970526     C                   MOVEL     '04'          FUNKEY
156600970526     C                   END
156700970526     C     KEY           IFEQ      F5
156800970526     C                   MOVEL     '05'          FUNKEY
156900970526     C                   END
157000970526     C     KEY           IFEQ      F6
157100970526     C                   MOVEL     '06'          FUNKEY
157200970526     C                   END
157300970526     C     KEY           IFEQ      F7
157400970526     C                   MOVEL     '07'          FUNKEY
157500970526     C                   END
157600970526     C     KEY           IFEQ      F8
157700970526     C                   MOVEL     '08'          FUNKEY
157800970526     C                   END
157900970526     C     KEY           IFEQ      F9
158000970526     C                   MOVEL     '09'          FUNKEY
158100970526     C                   END
158200970526     C     KEY           IFEQ      F10
158300970526     C                   MOVEL     '10'          FUNKEY
158400970526     C                   END
158500970526     C     KEY           IFEQ      F11
158600970526     C                   MOVEL     '11'          FUNKEY
158700970526     C                   END
158800970526     C     KEY           IFEQ      F12
158900970526     C                   MOVEL     '12'          FUNKEY
159000970526     C                   END
159100970526     C     KEY           IFEQ      F13
159200970526     C                   MOVEL     '13'          FUNKEY
159300970526     C                   END
159400970526     C     KEY           IFEQ      F14
159500970526     C                   MOVEL     '14'          FUNKEY
159600970526     C                   END
159700970526     C     KEY           IFEQ      F15
159800970526     C                   MOVEL     '15'          FUNKEY
159900970526     C                   END
160000970526     C     KEY           IFEQ      F16
160100970526     C                   MOVEL     '16'          FUNKEY
160200970526     C                   END
160300970526     C     KEY           IFEQ      F17
160400970526     C                   MOVEL     '17'          FUNKEY
160500970526     C                   END
160600970526     C     KEY           IFEQ      F18
160700970526     C                   MOVEL     '18'          FUNKEY
160800970526     C                   END
160900970526     C     KEY           IFEQ      F19
161000970526     C                   MOVEL     '19'          FUNKEY
161100970526     C                   END
161200970526     C     KEY           IFEQ      F20
161300970526     C                   MOVEL     '20'          FUNKEY
161400970526     C                   END
161500970526     C     KEY           IFEQ      F21
161600970526     C                   MOVEL     '21'          FUNKEY
161700970526     C                   END
161800970526     C     KEY           IFEQ      F22
161900970526     C                   MOVEL     '22'          FUNKEY
162000970526     C                   END
162100970526     C     KEY           IFEQ      F23
162200970526     C                   MOVEL     '23'          FUNKEY
162300970526     C                   END
162400970526     C     KEY           IFEQ      F24
162500970526     C                   MOVEL     '24'          FUNKEY
162600970526     C                   END
162700970526     C     KEY           IFEQ      ROLLUP
162800970526     C                   MOVEL     'RU'          FUNKEY
162900970526     C                   END
163000970526     C     KEY           IFEQ      HELP
163100970526     C                   MOVEL     'HP'          FUNKEY
163200970526     C                   END
163300970526     C*
163400970526     C                   ENDSR
163500970526     C*------------------------------------------------------------------------*
163600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
163700970526     C*------------------------------------------------------------------------*
163800970526     C     FUNGES        BEGSR
163900970526     C*
164000970526     C* PULISCO CAMPI DI LAVORO
164100970526     C                   Z-ADD     0             W                 5 0
164200970526     C*
164300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
164400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
164500970526     C     1             DO        24            I
164600970526     C                   MOVEL     FUD(I)        FUDRIG
164700970526     C*
164800970526     C* PULIZIA FUNZIONI PRECEDENTI
164900970526     C                   MOVEL     *BLANKS       FUV(I)
165000970526     C*
165100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
165200970526     C* (SALTA F23 E F24)
165300970526     C     FUDATT        IFEQ      'S'
165400970526     C     FUDFUN        ANDNE     '23'
165500970526     C     FUDFUN        ANDNE     '24'
165600970526     C                   ADD       1             W
165700970526     C                   MOVEL     FUDDES        FUV(W)
165800970526     C                   END
165900970526     C                   END                                                    DO WHILE
166000970526     C*
166100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
166200970526     C* ALTRIMENTI DISATTIVO F24
166300970526     C     1             DO        24            I
166400970526     C                   MOVEL     FUD(I)        FUDRIG
166500970526     C*
166600970526     C     FUDFUN        IFEQ      '24'
166700970526     C     W             IFGT      6
166800970526     C                   MOVEL     'S'           FUDATT
166900970526     C                   ELSE
167000970526     C                   MOVEL     *BLANKS       FUDATT
167100970526     C                   END                                                    W GT 8
167200970526     C                   MOVEL     FUDRIG        FUD(I)
167300970526     C                   END
167400970526     C                   END                                                    DO WHILE
167500970526     C*
167600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
167700970526     C     W             DIV       6             I
167800970526     C                   MVR                     Z
167900970526     C*
168000970526     C     Z             IFNE      0
168100970526     C     I             ADD       1             Z
168200970526     C                   ELSE
168300970526     C                   Z-ADD     I             Z
168400970526     C                   END
168500970526     C*
168600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
168700970526     C                   ADD       1             FUNGRU
168800970526     C     FUNGRU        IFGT      Z
168900970526     C     FUNGRU        ORLT      1
169000970526     C                   Z-ADD     1             FUNGRU
169100970526     C                   END
169200970526     C*
169300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
169400970526     C     FUNGRU        IFEQ      1
169500970526     C                   MOVEL     FUV(1)        FUN001
169600970526     C                   MOVEL     FUV(2)        FUN002
169700970526     C                   MOVEL     FUV(3)        FUN003
169800970526     C                   MOVEL     FUV(4)        FUN004
169900970526     C                   MOVEL     FUV(5)        FUN005
170000970526     C                   MOVEL     FUV(6)        FUN006
170100970526     C                   END
170200970526     C*
170300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
170400970526     C     FUNGRU        IFEQ      2
170500970526     C                   MOVEL     FUV(7)        FUN001
170600970526     C                   MOVEL     FUV(8)        FUN002
170700970526     C                   MOVEL     FUV(9)        FUN003
170800970526     C                   MOVEL     FUV(10)       FUN004
170900970526     C                   MOVEL     FUV(11)       FUN005
171000970526     C                   MOVEL     FUV(12)       FUN006
171100970526     C                   END
171200970526     C*
171300970526     C* TERZO GRUPPO DI 6 FUNZIONI
171400970526     C     FUNGRU        IFEQ      3
171500970526     C                   MOVEL     FUV(13)       FUN001
171600970526     C                   MOVEL     FUV(14)       FUN002
171700970526     C                   MOVEL     FUV(15)       FUN003
171800970526     C                   MOVEL     FUV(16)       FUN004
171900970526     C                   MOVEL     FUV(17)       FUN005
172000970526     C                   MOVEL     FUV(18)       FUN006
172100970526     C                   END
172200970526     C*
172300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
172400970526     C     FUNGRU        IFEQ      4
172500970526     C                   MOVEL     FUV(19)       FUN001
172600970526     C                   MOVEL     FUV(20)       FUN002
172700970526     C                   MOVEL     FUV(21)       FUN003
172800970526     C                   MOVEL     FUV(22)       FUN004
172900970526     C                   MOVEL     *BLANKS       FUN005
173000970526     C                   MOVEL     *BLANKS       FUN006
173100970526     C                   END
173200970526     C*
173300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
173400970526     C                   MOVEL     *BLANKS       FUN007
173500970526     C                   MOVEL     *BLANKS       FUN008
173600970526     C*
173700970526     C     1             DO        24            I
173800970526     C                   MOVEL     FUD(I)        FUDRIG
173900970526     C     FUDFUN        IFEQ      '23'
174000970526     C     FUDATT        ANDEQ     'S'
174100970526     C                   MOVEL     FUDDES        FUN007
174200970526     C                   END
174300970526     C     FUDFUN        IFEQ      '24'
174400970526     C     FUDATT        ANDEQ     'S'
174500970526     C                   MOVEL     FUDDES        FUN008
174600970526     C                   END
174700970526     C                   END                                                    DO
174800970526     C*
174900970526     C                   ENDSR
175000970526     C*------------------------------------------------------------------------*
175100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
175200970526     C*------------------------------------------------------------------------*
175300970526     C     OPZGES        BEGSR
175400970526     C*
175500970526     C* PULISCO CAMPI DI LAVORO
175600970526     C                   Z-ADD     0             W                 5 0
175700970526     C*
175800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
175900970526     C* E CARICO TABELLA OPZIONI ATTIVE
176000970526     C     1             DO        24            I
176100970526     C                   MOVEL     OPD(I)        OPDRIG
176200970526     C*
176300970526     C* PULIZIA OPZIONI PRECEDENTI
176400970526     C                   MOVEL     *BLANKS       OPV(I)
176500970526     C*
176600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
176700970526     C     OPDATT        IFEQ      'S'
176800970526     C                   ADD       1             W
176900970526     C                   MOVEL     OPDDES        OPV(W)
177000970526     C                   END
177100970526     C                   END                                                    DO WHILE
177200970526     C*
177300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
177400970526     C* ALTRIMENTI DISATTIVO F23
177500970526     C     1             DO        24            I
177600970526     C                   MOVEL     FUD(I)        FUDRIG
177700970526     C*
177800970526     C     FUDFUN        IFEQ      '23'
177900970526     C     W             IFGT      8
178000970526     C                   MOVEL     'S'           FUDATT
178100970526     C                   ELSE
178200970526     C                   MOVEL     *BLANKS       FUDATT
178300970526     C                   END                                                    W GT 8
178400970526     C                   MOVEL     FUDRIG        FUD(I)
178500970526     C                   END
178600970526     C                   END                                                    DO WHILE
178700970526     C*
178800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
178900970526     C     W             DIV       8             I
179000970526     C                   MVR                     Z                 5 0
179100970526     C*
179200970526     C     Z             IFNE      0
179300970526     C     I             ADD       1             Z
179400970526     C                   ELSE
179500970526     C                   Z-ADD     I             Z
179600970526     C                   END
179700970526     C*
179800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
179900970526     C                   ADD       1             OPZGRU
180000970526     C     OPZGRU        IFGT      Z
180100970526     C     OPZGRU        ORLT      1
180200970526     C                   Z-ADD     1             OPZGRU
180300970526     C                   END
180400970526     C*
180500970526     C* PRIMO GRUPPO DI 8 OPZIONI
180600970526     C     OPZGRU        IFEQ      1
180700970526     C                   MOVEL     OPV(1)        OPZ001
180800970526     C                   MOVEL     OPV(2)        OPZ002
180900970526     C                   MOVEL     OPV(3)        OPZ003
181000970526     C                   MOVEL     OPV(4)        OPZ004
181100970526     C                   MOVEL     OPV(5)        OPZ005
181200970526     C                   MOVEL     OPV(6)        OPZ006
181300970526     C                   MOVEL     OPV(7)        OPZ007
181400970526     C                   MOVEL     OPV(8)        OPZ008
181500970526     C                   END
181600970526     C*
181700970526     C* SECONDO GRUPPO DI 8 OPZIONI
181800970526     C     OPZGRU        IFEQ      2
181900970526     C                   MOVEL     OPV(9)        OPZ001
182000970526     C                   MOVEL     OPV(10)       OPZ002
182100970526     C                   MOVEL     OPV(11)       OPZ003
182200970526     C                   MOVEL     OPV(12)       OPZ004
182300970526     C                   MOVEL     OPV(13)       OPZ005
182400970526     C                   MOVEL     OPV(14)       OPZ006
182500970526     C                   MOVEL     OPV(15)       OPZ007
182600970526     C                   MOVEL     OPV(16)       OPZ008
182700970526     C                   END
182800970526     C*
182900970526     C* TERZO GRUPPO DI 8 OPZIONI
183000970526     C     OPZGRU        IFEQ      3
183100970526     C                   MOVEL     OPV(17)       OPZ001
183200970526     C                   MOVEL     OPV(18)       OPZ002
183300970526     C                   MOVEL     OPV(19)       OPZ003
183400970526     C                   MOVEL     OPV(20)       OPZ004
183500970526     C                   MOVEL     OPV(21)       OPZ005
183600970526     C                   MOVEL     OPV(22)       OPZ006
183700970526     C                   MOVEL     OPV(23)       OPZ007
183800970526     C                   MOVEL     OPV(24)       OPZ008
183900970526     C                   END
184000970526     C*
184100970526     C                   ENDSR
184200970526     C*------------------------------------------------------------------------*
184300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
184400970526     C*------------------------------------------------------------------------*
184500970526     C     OPZCON        BEGSR
184600970526     C*
184700970526     C* PULISCO CAMPI DI LAVORO
184800970526     C                   Z-ADD     0             I                 5 0
184900970526     C                   Z-ADD     0             W                 5 0
185000970526     C*
185100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
185200970526     C     1             DO        24            I
185300970526     C                   MOVEL     OPD(I)        OPDRIG
185400970526     C*
185500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
185600970526     C     OPZKEY        IFEQ      OPDOPZ
185700970526     C     OPZKEY        OREQ      OPDOP1
185800970526     C*
185900970526     C     OPDATT        IFEQ      'S'
186000970526     C                   MOVEL     OPDOPZ        OPZKEY
186100970526     C                   Z-ADD     I             W
186200970526     C                   END
186300970526     C                   END
186400970526     C*
186500970526     C                   END                                                    DO WHILE
186600970526     C*
186700970526     C* OPZIONE NON ATTIVA (ERRORE)
186800970526     C     W             IFEQ      0
186900970526     C*
187000970526     C* OPZIONE NON OK
187100970526     C                   Z-ADD     1             OPZOK
187200970526     C*
187300970526     C* EMISSIONE MESSAGGIO DI ERRORE
187400970526     C                   MOVEL     MSG(1)        DSMSMS
187500970526     C                   ELSE
187600970526     C*
187700970526     C* OPZIONE ATTIVA (PROCEDI ...)
187800970526     C*
187900970526     C     OPZKEY        IFEQ      ' 1'
188000970526     C                   EXSR      O01GES
188100970526     C                   END
188200970526     C     OPZKEY        IFEQ      ' 2'
188300970526     C                   EXSR      O02GES
188400970526     C                   END
188500970917     C     OPZKEY        IFEQ      ' 3'
188600970917     C                   EXSR      O03GES
188700970917     C                   END
188800970526     C     OPZKEY        IFEQ      ' 4'
188900970526     C                   EXSR      O04GES
189000970526     C                   END
189100970526     C     OPZKEY        IFEQ      ' 5'
189200970526     C                   EXSR      O05GES
189300970526     C                   END
189400970526     C     OPZKEY        IFEQ      ' 7'
189500970526     C                   EXSR      O07GES
189600970526     C                   END
189700970526     C     OPZKEY        IFEQ      ' 8'
189800970526     C                   EXSR      O08GES
189900970526     C                   END
190000970526     C     OPZKEY        IFEQ      ' 9'
190100970526     C                   EXSR      O09GES
190200970526     C                   END
190300970526     C     OPZKEY        IFEQ      '10'
190400970526     C                   EXSR      O10GES
190500970526     C                   END
190600131028     C     OPZKEY        IFEQ      '13'
190700131028     C                   EXSR      O13GES
190800131028     C                   END
190900060403     C     OPZKEY        IFEQ      '14'
191000060403     C                   EXSR      O14GES
191100060403     C                   END
191200060403     C     OPZKEY        IFEQ      '15'
191300060403     C                   EXSR      O15GES
191400060403     C                   END
191500970526     C*
191600970526     C                   END                                                    OPZ.NON ATTIVA
191700970526     C*
191800970526     C                   ENDSR
191900970526     C*------------------------------------------------------------------------*
192000970526     C* NOTE COSTRUZIONE SCHIERE
192100970526     C*------------------------------------------------------------------------*
192200970526     C*
192300970526     C* -MSG-MESSAGGI
192400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
192500970526     C* (NON TOCCARE MSG,1 E MSG,2)
192600970526     C*
192700970526     C* -CMD-COMANDI
192800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
192900970526     C*
193000970526     C* -OPD-OPZIONI
193100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
193200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
193300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
193400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
193500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
193600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
193700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
193800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
193900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
194000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
194100970526     C*
194200970526     C* -FUD-FUNZIONI
194300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
194400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
194500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
194600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
194700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
194800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
194900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
195000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
195100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
195200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
195300970526     O*------------------------------------------------------------------------*
195400970526** DEC - DECODIFICHE
195500970526    GESTIONE                                                     1
195600970526INTERROGAZIONE                                                   2
195700970530     SCELTA                                                      3
1958009705264
1959009705265
196000970526** MSG - MESSAGGI
196100970526TISI02R- Opzione non gestita                                                   1
196200970526TISI02R- Tasto funzionale non gestito                                          2
196300970704TISI02R- 3
196400970526TISI02R- Record annullato. Consentito solo il ripristino                       4
196500970526TISI02R- Record non annullato. Impossibile ripristinare                        5
196600970526TISI02R- Accettato solo un parametro di inizio lista                           6
196700970526TISI02R- Errori nei parametri di ingresso                                      7
196800970526TISI02R- Data errata                                                           8
196900970526TISI02R- 9                                                                     9
197000970526TISI02R- 10                                                                   10
197100970526TISI02R- 11                                                                   11
197200970526TISI02R- 12                                                                   12
197300970526TISI02R- 13                                                                   13
197400970526TISI02R- 14                                                                   14
197500970526TISI02R- 15                                                                   15
197600970526TISI02R- 16                                                                   16
197700970526TISI02R- 17                                                                   17
197800970526TISI02R- 18                                                                   18
197900970526TISI02R- 19                                                                   19
198000970526TISI02R- 20                                                                   20
198100970526** CMD - COMANDI CL
198200970526WRKSPLF
198300970526
198400970526
198500970526
198600970526
198700970526** OPD - OPZIONI
198800970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
198900970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
199000970917 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
199100970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
199200970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
199300970526                   -6 -     (S/N)=N                        opz. 06¬
199400131028 7=Orari Servizi   -7 -     (S/N)=S                        opz. 07¬
199500970626 8=Località        -8 -     (S/N)=N                        opz. 08¬
199600970626 9=Eccezioni       -9 -     (S/N)=N                        opz. 09¬
19970097062610=Tempi consegna  -10-     (S/N)=N                        opz. 10¬
199800970526                   -  -     (S/N)=N                        opz. 11¬
199900060403                   -  -     (S/N)=N                        opz. 12¬
20000013102813=Ripristino      -13-     (S/N)=N                        opz. 13¬
20010006040314=Cal.Eventi Part -14-     (S/N)=S                        opz. 14¬
20020006040315=InserisciEvePart-15-     (S/N)=N                        opz. 15¬
200300970526                   -  -     (S/N)=N                        opz. 16¬
200400970526                   -  -     (S/N)=N                        opz. 17¬
200500970526                   -  -     (S/N)=N                        opz. 18¬
200600970526                   -  -     (S/N)=N                        opz. 19¬
200700970526                   -  -     (S/N)=N                        opz. 20¬
200800970526                   -  -     (S/N)=N                        opz. 21¬
200900970526                   -  -     (S/N)=N                        opz. 22¬
201000970526                   -  -     (S/N)=N                        opz. 23¬
201100970526                   -  -     (S/N)=N                        opz. 24¬
201200970526** FUD - FUNZIONI
201300970526                   -        (S/N)=N                        funz.01¬
201400970526                   -        (S/N)=N                        funz.02¬
201500970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
201600970526                   -        (S/N)=N                        funz.04¬
201700060927F14=Cal.Eventi Part-        (S/N)=S  <== S-FISSO           funz.05¬
201800970526F06=Inserimento    -        (S/N)=N                        funz.06¬
201900970526                   -        (S/N)=N                        funz.07¬
202000970626F08=Località       -        (S/N)=N                        funz.08¬
202100970626F09=Eccezioni      -        (S/N)=N                        funz.09¬
202200970626F10=Tempi consegna -        (S/N)=N                        funz.10¬
202300970526                   -        (S/N)=N                        funz.11¬
202400060927F15=CapUtilizziPart-        (S/N)=S  <== S-FISSO           funz.12¬
202500970526                   -        (S/N)=N                        funz.13¬
202600060927F05=Rivisualizza   -        (S/N)=S                        funz.14¬
202700060927F12=Ritorno        -        (S/N)=S                        funz.15¬
202800970526                   -        (S/N)=N                        funz.16¬
202900970526                   -        (S/N)=N                        funz.17¬
203000970526                   -        (S/N)=N                        funz.18¬
203100970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
203200970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
203300970528F21=Stampa cappario-        (S/N)=S                        funz.21¬
203400970528F22=Gestione stampe-        (S/N)=S                        funz.22¬
203500970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
203600970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
