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
001400130823     FAZcos01L  IF   E           K DISK
001500130823     FAZCPC01L  IF   E           K DISK
001600970526     FAZCPC02L  IF   E           K DISK
001700970526     F                                     RENAME(AZCPC000:AZCPC2)
001800970526     FAZCPC03L  IF   E           K DISK
001900970526     F                                     RENAME(AZCPC000:AZCPC3)
002000970526     D*------------------------------------------------------------------------*
002100970526     D* SCHIERE
002200970526     D*------------------------------------------------------------------------*
002300970526     D*------------
002400970526     D* DECODIFICHE
002500970526     D*------------
002600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* MESSAGGI
002900970526     D*------------
003000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003100970526     D*------------
003200970526     D* COMANDI
003300970526     D*------------
003400970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
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
005000970526     D FUV             S             19    DIM(24)
005001131028     d* Filiali abilitate  agli orari servizi
005002131028     D FIL_AB          S              3    DIM(85)
005100970526     D*------------------------------------------------------------------------*
005200970526     D* INPUT
005300970526     D*------------------------------------------------------------------------*
005400970526     D*------------
005500970526     D* RIDEFINIZIONE OPZIONI VIDEO
005600970526     D*------------
005700970526     D OPDDS           DS
005800970526     D  OPDOPZ                 1      2
005900970526     D  OPDDE1                 3     19
006000970526     D  OPDOP1                21     22
006100970526     D  OPDFI0                23     34
006200970526     D  OPDATT                35     35
006300970526     D  OPDFI1                36     80
006400970526     D  OPDDES                 1     19
006500970526     D  OPDRIG                 1     80
006600970526     D*------------
006700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006800970526     D*------------
006900970526     D FUDDS           DS
007000970526     D  FUDDE1                 1      1
007100970526     D  FUDFUN                 2      3
007200970526     D  FUDDE2                 4     19
007300970526     D  FUDFI0                20     34
007400970526     D  FUDATT                35     35
007500970526     D  FUDFI1                36     80
007600970526     D  FUDDES                 1     19
007700970526     D  FUDRIG                 1     80
007800970526     D*------------
007900970526     D* DS GESTIONE TASTI FUNZIONALI
008000970526     D*------------
008100970526     D DSKEY         E DS
008200970526     D DEVDS1          DS
008300970526     D  KEY                  369    369
008400970526     D*------------
008500970526     D* DS "XMSG" - GESTIONE MESSAGGI
008600970526     D*------------
008700970526     D DSMSG         E DS
008800970526     D*------------
008900020916     D* DS REPERIMENTO DATI UTENTE
009000020916     D*-------------------
009100020916     D TIBS34DS      E DS                                                       *Profili utente
009200020916     D DDATIUTE      E DS                                                       *Dati utente
009300020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009400061026     D DUTE01        E DS                                                       *Abilitazioni
009500970526     D*------------------
009600970526     D* DS "XSRDAT" - CONTROLLA DATA (6)
009700970526     D*------------------
009800970526     D WLBDAT          DS                  INZ
009900970526     D  G06DAT                 1      6  0
010000970526     D  G06INV                 7     12  0
010100970526     D  G06ERR                13     13
010200970526     D  G06TGI                14     18  0
010300970526     D*------------------
010400970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
010500970526     D*------------------
010600970526     D WLBDA8          DS                  INZ
010700970526     D  G08DAT                 1      8  0
010800970526     D  G08INV                 9     16  0
010900970526     D  G08ERR                17     17
011000970526     D  G08TGI                18     22  0
011100970526     D*------------
011200970526     D* DS DI PROCEDURA
011300970526     D*------------
011400970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
011500060403     D tisip0ds      E DS
011600060927     D tisiq0ds      E DS
011601131028     D tisiO0ds      E DS
011700970526     D*------------
011800970526     D* ARCHITETTURA
011900970526     D*------------
012000970526     D KPJBA         E DS
012001131028
012002131028     d dvpodeco      e ds
012003131028     d Tibs02Ds      e ds
012004131028
012100060403     d tipopgm         s              1
012200060403     d wdri            s                   like(Dp0dri)
012300060403     d dprv            s                   like(Dp0prv)
012400130823     d kloc            s                   like(cosloc)
012401131028     d w003a           s              3
012402131028     d XX              s              3  0
012403131028     d YY              s              3  0
012404131028
012405131028
012406131028      /copy gaitrasrc/srcprotopr,tibs02r
012500970526     C*------------------------------------------------------------------------*
012600970526     C* MAIN LINES
012700970526     C*------------------------------------------------------------------------*
012800970526     C*
012900970526     C* SE NON CI SONO ERRORI, PROSEGUE
013000970526IF  1C     ERRL00        IFNE      '1'
013100970526     C*
013200970526     C* CARICA IL SFILE
013300970526     C                   EXSR      POSSFL
013400970526     C*
013500970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013600970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013700970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013800970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013900970526     C*
014000970526     C* REIMPOSTA ERRORE DI PROCERURA
014100970526     C                   MOVEL     '0'           ERRL00
014200970526     C*
014300970526     C* EMETTE IL SFILE
014400970526     C                   EXSR      OUTSFL
014500970526     C*
014600970526     C* GESTIONE TASTI FUNZIONALI
014700970526     C                   EXSR      FUNCON
014800970526     C*
014900970526E   2C                   ENDDO
015000970527     C*
015100970527     C* CI SONO ERRORI IN ENTRATA
015200970527X   1C                   ELSE
015300970527     C                   MOVEL     '1'           D00ERR
015400970527     C                   MOVEL     DSMSMS        D00MSG
015500970526E   1C                   ENDIF
015600970526     C* FINE PGM
015700970526     C                   SETON                                        LR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016000970526     C*------------------------------------------------------------------------*
016100970526     C     F03GES        BEGSR
016200970526     C*
016300970526     C* IMPOSTA "PREMUTO F03"
016400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016500970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
016600970526     C*
016700970526     C                   ENDSR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017000970526     C*------------------------------------------------------------------------*
017100970526     C     F05GES        BEGSR
017200970526     C*
017300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017400970526     C*
017500970526     C                   ENDSR
017600970526     C*------------------------------------------------------------------------*
017700970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
017800970526     C*------------------------------------------------------------------------*
017900970526     C     F06GES        BEGSR
018000970526     C*
018100970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
018200020916IF  1C     DUTLPO        IFNE      'S'
018300970526     C                   MOVEL     '1'           ERRL00
018400970526     C                   MOVEL     MSG(3)        DSMSMS
018500970526E   1C                   ENDIF
018600970526     C*
018700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018800970602     C                   MOVEL     *BLANKS       DNAR
018900970602     C                   MOVEL     *BLANKS       DCAP
019000970529     C                   MOVEL     DEPOP0        WOP0
019100970529     C                   MOVEL     'F06'         WOP1
019200970526     C                   MOVEL     'TISI03R'     WPGM              8
019300970526     C                   EXSR      LANPGM                                       *LANCIA PGM
019400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
019500970526E   1C                   ENDIF
019600970526     C*
019700970526     C                   ENDSR
019800970528     C*------------------------------------------------------------------------*
019900970528     C* F08GES - GESTIONE F08 -> GESTIONE LOCALITA'
020000970528     C*------------------------------------------------------------------------*
020100970528     C     F08GES        BEGSR
020200970528     C*
020300970602     C                   MOVEL     *BLANKS       DNAR
020400970602     C                   MOVEL     *BLANKS       DCAP
020500970528     C                   MOVEL     DEPOP0        WOP0
020600970528     C                   MOVEL     'L'           WOP0
020700970528     C                   MOVEL     'F08'         WOP1
020800970528     C                   MOVEL     'TISI30R'     WPGM              8
020900970528     C                   EXSR      LANPGM
021000970528     C*
021100970528     C                   ENDSR
021200970528     C*------------------------------------------------------------------------*
021300970528     C* F09GES - GESTIONE F09 -> ECCEZIONI LINEE/ZONE
021400970528     C*------------------------------------------------------------------------*
021500970528     C     F09GES        BEGSR
021600970528     C*
021700970602     C                   MOVEL     *BLANKS       DNAR
021800970602     C                   MOVEL     *BLANKS       DCAP
021900970528     C                   MOVEL     DEPOP0        WOP0
022000970528     C                   MOVEL     'E'           WOP0
022100970528     C                   MOVEL     'F09'         WOP1
022200970528     C                   MOVEL     'TISI11R'     WPGM              8
022300970528     C                   EXSR      LANPGM
022400970528     C*
022500970528     C                   ENDSR
022600970528     C*------------------------------------------------------------------------*
022700970528     C* F10GES - GESTIONE F10 -> TEMPI DI CONSEGNA
022800970528     C*------------------------------------------------------------------------*
022900970602     C     F10GES        BEGSR
023000970528     C*
023100970602     C                   MOVEL     *BLANKS       DNAR
023200970602     C                   MOVEL     *BLANKS       DCAP
023300970528     C                   MOVEL     DEPOP0        WOP0
023400970528     C                   MOVEL     'T'           WOP0
023500970528     C                   MOVEL     'F10'         WOP1
023600970528     C                   MOVEL     'TISI41R'     WPGM              8
023700970528     C                   EXSR      LANPGM
023800970528     C*
023900970528     C                   ENDSR
024000970526     C*------------------------------------------------------------------------*
024100970526     C* F12GES - GESTIONE F12 -> RITORNO
024200970526     C*------------------------------------------------------------------------*
024300970526     C     F12GES        BEGSR
024400970526     C*
024500970526     C* IMPOSTA "PREMUTO F12"
024600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
024700970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
024800970526     C*
024900970526     C                   ENDSR
025000060403     C*------------------------------------------------------------------------*
025100060403     C* F14GES - GESTIONE F14 -> calendario eventi particolari
025200060403     C*------------------------------------------------------------------------*
025300060403     C     F14GES        BEGSR
025400060403     C*
025500060403     C                   MOVEL     *BLANKS       DNAR
025600060403     C                   MOVEL     *BLANKS       DCAP
025700060403     C                   MOVEL     DEPOP0        WOP0
025800060403     c                   clear                   wop1
025900060403     C                   MOVEL     datcor        wdri
026000060403     C                   MOVEL     'TISIP0R'     WPGM              8
026100060403     C                   EXSR      LANPGM
026200060403     C*
026300060403     C                   ENDSR
026400060927     C*------------------------------------------------------------------------*
026500060927     C* F15GES - GESTIONE F15 -> cappario utilizzi particolari
026600060927     C*------------------------------------------------------------------------*
026700060927     C     F15GES        BEGSR
026800060927     C*
026900060927     c                   clear                   tisiq0ds
027000060927     c                   MOVEL     'C05'         dq0op0                         *livello procedura
027100060927     c                   clear                   dq0op1                         *livello programma
027200060927     c                   MOVEL     '0'           dq0f03                         *premuto F03
027300060927     c                   MOVEL     '0'           dq0f12                         *premuto F12
027400060927     c                   MOVEL     '0'           dq0err                         *errore
027500060927     c                   call      'TISIQ0R'
027600060927     c                   parm                    kpjba
027700060927     c                   parm                    tisiQ0ds
027800060927     C*
027900060927     C                   ENDSR
028000970526     C*------------------------------------------------------------------------*
028100970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
028200970526     C*------------------------------------------------------------------------*
028300970526     C     F21GES        BEGSR
028400970612     C*
028500970612     C                   MOVEL     VIDNAR        DNAR
028600970612     C                   MOVEL     VIDCAP        DCAP
028700970612     C                   MOVEL     DEPOP0        WOP0
028800970612     C                   MOVE      '03'          WOP0
028900970612     C                   MOVEL     'F21'         WOP1
029000970612     C                   MOVEL     'TISI04R'     WPGM              8
029100970612     C                   EXSR      LANPGM
029200970526     C*
029300970526     C                   ENDSR
029400970526     C*------------------------------------------------------------------------*
029500970526     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
029600970526     C*------------------------------------------------------------------------*
029700970526     C     F22GES        BEGSR
029800970526     C*
029900970526     C                   Z-ADD     80            LENGH            15 5
030000970526     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
030100970526     C                   PARM                    CMD(1)
030200970526     C                   PARM                    LENGH
030300970526     C*
030400970526     C                   ENDSR
030500970526     C*------------------------------------------------------------------------*
030600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
030700970526     C*------------------------------------------------------------------------*
030800970526     C     F23GES        BEGSR
030900970526     C*
031000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
031100970526     C*
031200970526     C                   ENDSR
031300970526     C*------------------------------------------------------------------------*
031400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
031500970526     C*------------------------------------------------------------------------*
031600970526     C     F24GES        BEGSR
031700970526     C*
031800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
031900970526     C*
032000970526     C                   ENDSR
032100970526     C*------------------------------------------------------------------------*
032200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
032300970526     C*------------------------------------------------------------------------*
032400970526     C     RUPGES        BEGSR
032500970526     C*
032600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
032700970526     C*
032800970526     C                   ENDSR
032900970526     C*------------------------------------------------------------------------*
033000970526     C* ENTGES - GESTIONE ENTER -> INVIO
033100970526     C*------------------------------------------------------------------------*
033200970526     C     ENTGES        BEGSR
033300970526     C*
033400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
033500970526     C                   EXSR      NEWPOS
033600970526     C*
033700970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
033800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033900970526IF  2C     SWPOSI        IFNE      0
034000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
034100970526X   2C                   ELSE
034200970526IF  3C     SFLULT        IFNE      0
034300970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
034400970526     C*
034500970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
034600970526IF  4C     SWMODI        IFNE      0
034700970526     C                   EXSR      POSSFL
034800970526E   4C                   ENDIF
034900970526E   3C                   ENDIF
035000970526E   2C                   ENDIF
035100970526E   1C                   ENDIF
035200970526     C*
035300970526     C                   ENDSR
035400970526     C*------------------------------------------------------------------------*
035500970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
035600970526     C*------------------------------------------------------------------------*
035700970526     C     O01GES        BEGSR
035800970526     C*
035900970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
036000970526     C                   MOVEL     VIDNAR        D00NAR
036100970526     C                   MOVEL     VIDCAP        D00CAP
036200970704     C                   MOVEL     VIDPRV        D00PRV
036300970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
036400970526     C*
036500970526     C                   ENDSR
036600970526     C*------------------------------------------------------------------------*
036700970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
036800970526     C*------------------------------------------------------------------------*
036900970526     C     O02GES        BEGSR
037000970526     C*
037100970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
037200970526IF  1C     VIDATB        IFEQ      'A'
037300970526     C                   MOVEL     '1'           ERRL00
037400970526     C                   MOVEL     MSG(4)        DSMSMS
037500970526E   1C                   ENDIF
037600970526     C* MODIFICA
037700970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037800970602     C                   MOVEL     VIDNAR        DNAR
037900970602     C                   MOVEL     VIDCAP        DCAP
038000970529     C                   MOVEL     DEPOP0        WOP0
038100970526     C                   MOVEL     'O02'         WOP1
038200970526     C                   MOVEL     'TISI03R'     WPGM
038300970526     C                   EXSR      LANPGM
038400970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
038500970526E   1C                   ENDIF
038600970526     C*
038700970526     C                   ENDSR
038800970917     C*------------------------------------------------------------------------*
038900970917     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
039000970917     C*------------------------------------------------------------------------*
039100970917     C     O03GES        BEGSR
039200970917     C*
039300970917     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
039400970917IF  1C     VIDATB        IFEQ      'A'
039500970917     C                   MOVEL     '1'           ERRL00
039600970917     C                   MOVEL     MSG(4)        DSMSMS
039700970917E   1C                   ENDIF
039800970917     C* DUPLICA
039900970917IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040000970917     C                   MOVEL     VIDNAR        DNAR
040100970917     C                   MOVEL     VIDCAP        DCAP
040200970917     C                   MOVEL     DEPOP0        WOP0
040300970917     C                   MOVEL     'O03'         WOP1
040400970917     C                   MOVEL     'TISI06R'     WPGM
040500970917     C                   EXSR      LANPGM
040600970917     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
040700970917E   1C                   ENDIF
040800970917     C*
040900970917     C                   ENDSR
041000970526     C*------------------------------------------------------------------------*
041100970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
041200970526     C*------------------------------------------------------------------------*
041300970526     C     O04GES        BEGSR
041400970526     C*
041500970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
041600970526IF  1C     VIDATB        IFEQ      'A'
041700970526     C                   MOVEL     '1'           ERRL00
041800970526     C                   MOVEL     MSG(4)        DSMSMS
041900970526E   1C                   ENDIF
042000970526     C* ANNULLAMENTO
042100970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042200970602     C                   MOVEL     VIDNAR        DNAR
042300970602     C                   MOVEL     VIDCAP        DCAP
042400970529     C                   MOVEL     DEPOP0        WOP0
042500970526     C                   MOVEL     'O04'         WOP1
042600970526     C                   MOVEL     'TISI03R'     WPGM
042700970526     C                   EXSR      LANPGM
042800970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
042900970526E   1C                   ENDIF
043000970526     C*
043100970526     C                   ENDSR
043200970526     C*------------------------------------------------------------------------*
043300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
043400970526     C*------------------------------------------------------------------------*
043500970526     C     O05GES        BEGSR
043600970526     C*
043700970602     C                   MOVEL     VIDNAR        DNAR
043800970602     C                   MOVEL     VIDCAP        DCAP
043900970529     C                   MOVEL     DEPOP0        WOP0
044000970526     C                   MOVEL     'O05'         WOP1
044100970526     C                   MOVEL     'TISI03R'     WPGM
044200970526     C                   EXSR      LANPGM
044300970526     C*
044400970526     C                   ENDSR
044401131028     C*------------------------------------------------------------------------*
044402131028     C* O07GES - GESTIONE OPZIONE 7=Orari servizi
044403131028     C*------------------------------------------------------------------------*
044404131028     C     O07GES        BEGSR
044405131028     C*
044406131028     C                   MOVEL     VIDNAR        DNAR
044407131028     C                   MOVEL     VIDCAP        DCAP
044411131028     C                   MOVEL     'TISIO2R'     WPGM
044412131028     C                   EXSR      LANPGM
044413131028     C*
044414131028     C                   ENDSR
044500970526     C*------------------------------------------------------------------------*
044600131028     C* O13GES - GESTIONE OPZIONE 13=RIPRISTINO
044700970526     C*------------------------------------------------------------------------*
044800131028     C     O13GES        BEGSR
044900970526     C*
045000970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
045100970526IF  1C     VIDATB        IFNE      'A'
045200970526     C                   MOVEL     '1'           ERRL00
045300970526     C                   MOVEL     MSG(5)        DSMSMS
045400970526E   1C                   ENDIF
045500970526     C* RIPRISITINO
045600970704IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
045700970602     C                   MOVEL     VIDNAR        DNAR
045800970602     C                   MOVEL     VIDCAP        DCAP
045900970529     C                   MOVEL     DEPOP0        WOP0
046000970526     C                   MOVEL     'O07'         WOP1
046100970526     C                   MOVEL     'TISI03R'     WPGM
046200970526     C                   EXSR      LANPGM
046300970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
046400970526E   1C                   ENDIF
046500970526     C*
046600970526     C                   ENDSR
046700970526     C*------------------------------------------------------------------------*
046800970526     C* O08GES - GESTIONE OPZIONE 8=LOCALITA'
046900970526     C*------------------------------------------------------------------------*
047000970526     C     O08GES        BEGSR
047100970528     C*
047200970602     C                   MOVEL     VIDNAR        DNAR
047300970602     C                   MOVEL     VIDCAP        DCAP
047400970528     C                   MOVEL     DEPOP0        WOP0
047500970528     C                   MOVEL     'L'           WOP0
047600970526     C                   MOVEL     'O08'         WOP1
047700970526     C                   MOVEL     'TISI30R'     WPGM
047800970526     C                   EXSR      LANPGM
047900970526     C*
048000970526     C                   ENDSR
048100970526     C*------------------------------------------------------------------------*
048200970526     C* O09GES - GESTIONE OPZIONE 9=ECCEZIONI
048300970526     C*------------------------------------------------------------------------*
048400970526     C     O09GES        BEGSR
048500970528     C*
048600970602     C                   MOVEL     VIDNAR        DNAR
048700970602     C                   MOVEL     VIDCAP        DCAP
048800970528     C                   MOVEL     DEPOP0        WOP0
048900970528     C                   MOVEL     'E'           WOP0
049000970526     C                   MOVEL     'O09'         WOP1
049100970526     C                   MOVEL     'TISI11R'     WPGM
049200970526     C                   EXSR      LANPGM
049300970526     C*
049400970526     C                   ENDSR
049500970526     C*------------------------------------------------------------------------*
049600970526     C* O10GES - GESTIONE OPZIONE 10=TEMPI DI CONSEGNA
049700970526     C*------------------------------------------------------------------------*
049800970526     C     O10GES        BEGSR
049900970526     C*
050000970602     C                   MOVEL     VIDNAR        DNAR
050100970602     C                   MOVEL     VIDCAP        DCAP
050200970528     C                   MOVEL     DEPOP0        WOP0
050300970528     C                   MOVEL     'T'           WOP0
050400970526     C                   MOVEL     'O10'         WOP1
050500970526     C                   MOVEL     'TISI41R'     WPGM
050600970526     C                   EXSR      LANPGM
050700970526     C*
050800970526     C                   ENDSR
050900060403     C*------------------------------------------------------------------------*
051000060403     C* O14GES - GESTIONE calendario eventi particolari
051100060403     C*------------------------------------------------------------------------*
051200060403     C     O14GES        BEGSR
051300060403     C*
051400060403     C                   MOVEL     VIDNAR        DNAR
051500060403     C                   MOVEL     VIDCAP        DCAP
051600060403     C                   MOVEL     DEPOP0        WOP0
051700060403     C                   clear                   WOP1
051800060403     C                   MOVEL     datcor        wdri
051900060403     c                   move      0101          wdri
052000060403     C                   MOVEL     'TISIP0R'     WPGM
052100060403     C                   EXSR      LANPGM
052200060403     C*
052300060403     C                   ENDSR
052400060403     C*------------------------------------------------------------------------*
052500060403     C* O15GES - Inserisci  evento particolare
052600060403     C*------------------------------------------------------------------------*
052700060403     C     O15GES        BEGSR
052800060403     C*
052900060403     C                   MOVEL     VIDNAR        DNAR
053000060403     C                   MOVEL     VIDCAP        DCAP
053100060403     C                   MOVEL     VIDprv        Dprv
053200060403     C                   MOVEL     DEPOP0        WOP0
053300060403     C                   movel     'F06'         WOP1
053400060403     C                   MOVEL     datcor        wdri
053500060403     C                   MOVEL     'TISIP1R'     WPGM
053600060403     C                   EXSR      LANPGM
053700060403     C*
053800060403     C                   ENDSR
053900970526     C*------------------------------------------------------------------------*
054000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
054100970526     C*------------------------------------------------------------------------*
054200970526     C     NEWPOS        BEGSR
054300970526     C*
054400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
054500970526     C*         1= RICHIESTO POSIZIONAMENTO
054600970526     C                   Z-ADD     0             SWPOSI            7 0
054700970526     C*
054800970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
054900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
055000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
055100970526     C                   Z-ADD     0             SWMODI            1 0
055200970530     C*
055300970530     C* REIMPOSTA INDICATORI DI ERRORE
055400970530     C                   SETOFF                                       808182
055500970530     C                   SETOFF                                       83
055600970526     C*
055700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
055800970526IF  1C     VIDPNA        IFNE      DEPNAR
055900970526     C     VIDPCA        ORNE      DEPCAP
056000970526     C     VIDPLN        ORNE      DEPLNA
056100970526     C     VIDPPR        ORNE      DEPPRV
056200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
056300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
056400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
056500970526     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
056600970526     C                   MOVEL     VIDPCA        DEPCAP
056700970526     C                   Z-ADD     VIDPLN        DEPLNA
056800970526     C                   MOVEL     VIDPPR        DEPPRV
056900970526E   2C                   ENDIF
057000970526E   1C                   ENDIF
057100970526     C*
057200970526     C                   ENDSR
057300970526     C*------------------------------------------------------------------------*
057400970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
057500970526     C*------------------------------------------------------------------------*
057600970526     C     SETFIL        BEGSR
057700970526     C*
057800970528     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
057900970526IF  1C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
058000970526     C                   Z-ADD     D00VER        KCCVER
058100970526     C                   MOVEL     VIDPNA        KCCNAR
058200970526     C                   MOVEL     VIDPCA        KCCCAP
058300970527     C     KEYCPC        SETLL     AZCPC01L                           99
058400970526E   1C                   ENDIF
058500970526     C*
058600970526IF  1C     WTIPRI        IFEQ      'LNA'                                        *PER LINEA ARRIVO
058700970526     C                   Z-ADD     D00VER        KCCVER
058800970526     C                   MOVEL     VIDPNA        KCCNAR
058900970526     C                   Z-ADD     VIDPLN        KCCLNA
059000970527     C     KE3CPC        SETLL     AZCPC03L                           99
059100970526E   1C                   ENDIF
059200970526     C*
059300970526IF  1C     WTIPRI        IFEQ      'PRV'                                        *PER PROVINCIA
059400970526     C                   Z-ADD     D00VER        KCCVER
059500970526     C                   MOVEL     VIDPNA        KCCNAR
059600970526     C                   MOVEL     VIDPPR        KCCPRV
059700970527     C     KE2CPC        SETLL     AZCPC02L                           99
059800970526E   1C                   ENDIF
059900970526     C* LETTURA RECORD
060000970526IF  1C     *IN99         IFEQ      *OFF
060100970526     C                   EXSR      LETFIL
060200970526E   1C                   ENDIF
060300970526     C*
060400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
060500970526IF  1C     *IN99         IFEQ      *ON
060600970526     C                   Z-ADD     1             SFLEOF
060700970526E   1C                   ENDIF
060800970526     C*
060900970526     C                   ENDSR
061000970526     C*------------------------------------------------------------------------*
061100970526     C* LETFIL - LETTURA RECORD
061200970526     C*------------------------------------------------------------------------*
061300970526     C     LETFIL        BEGSR
061400970526     C*
061500970526     C* LEGGE FINO A:
061600970526     C*     - FINE FILE
061700970526     C*     - TROVATO RECORD VALIDO
061800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
061900970526DO  1C     *IN99         DOWEQ     *OFF
062000970526     C     WRECOK        ANDEQ     'N'
062100970526     C*
062200970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
062300970526IF  2C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
062400970603     C     KCCVER        READE     AZCPC01L                               99
062500970526E   2C                   ENDIF
062600970526IF  2C     WTIPRI        IFEQ      'LNA'                                        *PER LINEA ARRIVO
062700970603     C     KCCVER        READE     AZCPC03L                               99
062800970526E   2C                   ENDIF
062900970526IF  2C     WTIPRI        IFEQ      'PRV'                                        *PER PROVINCIA
063000970603     C     KCCVER        READE     AZCPC02L                               99
063100970526E   2C                   ENDIF
063200970526     C*
063300970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
063400970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
063500970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
063600970530X   2C                   ELSE
063700970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
063800970530E   2C                   ENDIF
063900970526E   1C                   ENDDO
064000970526     C*
064100970526     C                   ENDSR
064200970526     C*------------------------------------------------------------------------*
064300970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
064400970526     C*------------------------------------------------------------------------*
064500970526     C     CHKREC        BEGSR
064600970526     C*
064700970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
064800970526     C*
064900970526     C                   ENDSR
065000970526     C*------------------------------------------------------------------------*
065100970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
065200970526     C*------------------------------------------------------------------------*
065300970526     C     IRESFL        BEGSR
065400970526     C*
065500970526     C                   MOVEL     *BLANKS       OPZKEY
065600970526     C                   MOVEL     CPCATB        VIDATB
065700970917     C                   MOVEL     CPCFI1        VIDFI1
065800970526     C                   MOVEL     CPCNAR        VIDNAR
065900970526     C                   MOVEL     CPCCAP        VIDCAP
066000970526     C                   Z-ADD     CPCTTC        VIDTTC
066100970526     C                   MOVEL     CPCISO        VIDISO
066200970526     C                   Z-ADD     CPCLNA        VIDLNA
066300970526     C                   Z-ADD     CPCZNC        VIDZNC
066400970526     C                   MOVEL     CPCPRV        VIDPRV
066500970526     C                   MOVEL     CPCPRA        VIDPRA
066600970526     C                   MOVEL     CPCCTS        VIDCTS
066700970529     C                   MOVEL     CPCCTE        VIDCTE
066800970526     C                   Z-ADD     CPCLOL        VIDLOL
066900970526     C                   Z-ADD     CPCZOL        VIDZOL
067000970604     C                   Z-ADD     CPCLOS        VIDLOS
067100970604     C                   Z-ADD     CPCZOS        VIDZOS
067200970526     C                   MOVEL     CPCFLG        VIDFLG
067300970526     C                   MOVEL     CPCFIT        VIDFIT
067400970528     C                   MOVEL     *BLANKS       VIDESE
067500970527IF  1C     CPCESE        IFEQ      'S'
067600970526     C                   MOVEL     'E'           VIDESE                         *ESISTONO ECCEZIONI
067700970527E   1C                   ENDIF
067800970528     C                   MOVEL     *BLANKS       VIDESL
067900970527IF  1C     CPCESL        IFEQ      'S'
068000970527     C                   MOVEL     'L'           VIDESL                         *LOCALITA' GESTITE
068100970527E   1C                   ENDIF
068200130828     c* esistenza eccezioni di orari servizi per il CAP
068300130823     C                   MOVEL     *BLANKS       VIDESos
068400130828     C**                 MOVEL     *BLANKS       VIhcos
068500130828     c                   setoff                                       02
068600130823     C                   MOVEL     cpccap        KCCCAP
068700130828     c                   clear                   kloc
068800130828     c     kcos          chain     azcos01l
068900130828     c                   if        %found(azcos01l)
069000130828     C                   MOVEL     'C'           VIDESos                        *eccez.orari serviz
069100130823E   1C                   ENDIF
069200130828     c* esistenza eccezioni di orari servizi per località
069300130828     C                   MOVEL     cpccap        KCCCAP
069400130828     c                   movel     'A'           kloc
069500130828     c     kcos          setll     azcos01l
069600130828     c     keycpc        reade     azcos01l
069700130828     c                   if        not %eof(azcos01l)
069800130828     C                   MOVE      'L'           VIDESos                        *eccez.orari serviz
069900130828     C**                 MOVEL     'S'           VIhcos
070000130828     c                   seton                                        02
070100130828E   1C                   ENDIF
070200130823     c
070300970526     C*
070400970526     C                   ENDSR
070500970526     C*------------------------------------------------------------------------*
070600970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
070700970526     C*------------------------------------------------------------------------*
070800970526     C     CHKPAR        BEGSR
070900970526     C*
071000970526     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
071100970526IF  1C     VIDPCA        IFEQ      *BLANKS
071200970526     C     VIDPLN        ANDEQ     *ZEROS
071300970526     C     VIDPPR        ANDEQ     *BLANKS
071400970526     C                   MOVEL     'CAP'         WTIPRI            3
071500970526E   1C                   ENDIF
071600970526     C*
071700970526     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
071800970526IF  1C     ERRL00        IFEQ      '0'
071900970526IF  2C     VIDPCA        IFNE      *BLANKS
072000970526     C     VIDPLN        ANDNE     *ZEROS
072100970526     C                   MOVEL     MSG(6)        DSMSMS
072200970526     C                   MOVEL     '1'           ERRL00
072300970529     C                   SETON                                        808182
072400970526E   2C                   ENDIF
072500970526E   1C                   ENDIF
072600970526     C*
072700970526IF  1C     ERRL00        IFEQ      '0'
072800970526IF  2C     VIDPCA        IFNE      *BLANKS
072900970526     C     VIDPPR        ANDNE     *BLANKS
073000970526     C                   MOVEL     MSG(6)        DSMSMS
073100970526     C                   MOVEL     '1'           ERRL00
073200970529     C                   SETON                                        808183
073300970526E   2C                   ENDIF
073400970526E   1C                   ENDIF
073500970526     C*
073600970526IF  1C     ERRL00        IFEQ      '0'
073700970526IF  2C     VIDPLN        IFNE      *ZEROS
073800970526     C     VIDPPR        ANDNE     *BLANKS
073900970526     C                   MOVEL     MSG(6)        DSMSMS
074000970526     C                   MOVEL     '1'           ERRL00
074100970529     C                   SETON                                        8283
074200970526E   2C                   ENDIF
074300970526E   1C                   ENDIF
074400970526     C*
074500970526     C* CONTROLLA IL TIPO RICERCA
074600970526IF  1C     ERRL00        IFEQ      '0'
074700970526IF  2C     VIDPCA        IFNE      *BLANKS
074800970526     C                   MOVEL     'CAP'         WTIPRI
074900970526E   2C                   ENDIF
075000970526IF  2C     VIDPLN        IFNE      *ZEROS
075100970526     C                   MOVEL     'LNA'         WTIPRI
075200970526E   2C                   ENDIF
075300970526IF  2C     VIDPPR        IFNE      *BLANKS
075400970526     C                   MOVEL     'PRV'         WTIPRI
075500970526E   2C                   ENDIF
075600970526E   1C                   ENDIF
075700970526     C*
075800970526     C                   ENDSR
075900970526     C*------------------------------------------------------------------------*
076000970526     C* LANPGM - LANCIA UN PROGRAMMA
076100970526     C*------------------------------------------------------------------------*
076200970526     C     LANPGM        BEGSR
076300970526     C*
076400970526     C* IMPOSTA LA DS DI PROCEDURA
076500060403     c                   eval      tipopgm=%subst(wpgm:5:1)
076600060403     c                   if        tipopgm='P'
076700060403     c                   clear                   tisip0ds
076800060403     C                   MOVEL     DNAR          Dp0NAR                         *NAZIONE
076900060403     C                   MOVEL     DCAP          Dp0CAP                         *CAP
077000060403     C                   MOVEL     Dprv          Dp0prv                         *CAP
077100060403     C                   MOVEL     wop0          dp0OP0
077200060403     C                   MOVEL     WOP1          Dp0OP1                         *LIVELLO PGM
077300060403     c                   movel     wdri          dp0dri
077400060403     c                   call      WPGM
077500060403     c                   parm                    kpjba
077600060403     c                   parm                    tisip0ds
077700060403     C*
077800060403     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
077900060403IF  1C     Dp0F03        IFEQ      '1'                                          *PREMUTO F03
078000060403     C                   MOVEL     '1'           ERRL00                         *ERRORE
078100060403     C                   MOVEL     '1'           F03L00                         *FINE PGM
078200060403     c                   movel     dp0f03        d00f03
078300060403X   1C                   ELSE                                                   *NON PREMUTO F03
078400060403     C                   MOVEL     Dp0MSG        DSMSMS                         *MESSAGGIO DI RITORN
078500060403E   1C                   ENDIF
078600060403     c
078700060403     c                   else
078701131028     c                   if        tipopgm='O'
078702131028     c                   clear                   tisiO0ds
078703131028     c                   if        %subst(depop0:2:2)='02'
078704131028     C                   MOVEL     'G'           do0tric
078705131028     c                   else
078706131028     C                   MOVEL     'I'           do0tric
078707131028     c                   endif
078712131028     c                   move      d00ver        do0ver
078713131028     C                   MOVEL     DNAR          DO0NAR                         *NAZIONE
078714131028     C                   MOVEL     DCAP          DO0CAP                         *CAP
078719131028     c                   call      WPGM
078720131028     c                   parm                    kpjba
078721131028     c                   parm                    tisio0ds
078722131028     C*
078723131028     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
078724131028IF  1C     DO0F03        IFEQ      '1'                                          *PREMUTO F03
078725131028     C                   MOVEL     '1'           ERRL00                         *ERRORE
078726131028     C                   MOVEL     '1'           F03L00                         *FINE PGM
078727131028     c                   movel     dO0f03        d00f03
078728131028X   1C                   ELSE                                                   *NON PREMUTO F03
078729131028     C                   MOVEL     DO0MSG        DSMSMS                         *MESSAGGIO DI RITORN
078730131028E   1C                   ENDIF
078731131028     c                   else
078800970528     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
078900970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
079000970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
079100970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
079200970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
079300970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
079400970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
079500970602     C                   MOVEL     DCAP          D00CAP                         *CAP
079600970530     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI CAMPI
079700970530     C                   MOVEL     *BLANKS       D00TSP
079800970530     C                   Z-ADD     *ZEROS        D00TFP
079900970530     C                   MOVEL     *BLANKS       D00LOC
080000970603     C                   MOVEL     *BLANKS       D00SIN
080100970530     C                   MOVEL     *BLANKS       D00EPA
080200970530     C                   Z-ADD     *ZEROS        D00ETP
080300970530     C                   Z-ADD     *ZEROS        D00ETA
080400970530     C                   Z-ADD     *ZEROS        D00EDD
080500970530     C                   MOVEL     *BLANKS       D00SPT
080600970530     C                   Z-ADD     *ZEROS        D00SLP
080700970530     C                   Z-ADD     *ZEROS        D00SLA
080800970530     C                   MOVEL     *BLANKS       D00STS
080900970530     C                   Z-ADD     *ZEROS        D00SDD
081000970526     C*
081100970526     C                   CALL      WPGM
081200970612     C                   PARM                    KPJBA
081300970612     C                   PARM                    DSSI00
081400970526     C*
081500970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
081600970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
081700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
081800970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
081900970526X   1C                   ELSE                                                   *NON PREMUTO F03
082000970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
082100970526E   1C                   ENDIF
082200060403     c                   endif
082201131028     c                   endif
082300970526     C*
082400970526     C                   ENDSR
082500970527     C*------------------------------------------------------------------------*
082600970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
082700970527     C*------------------------------------------------------------------------*
082800970527     C     CARTAB        BEGSR
082900970527     C*
082901131028      /free
082902131028        // Carico filiali di test abilitate
082903131028         if dutpou<>46  ;
082904131028
082905131028         clear TIBS02ds;
082906131028         T02Mod = 'C';
082907131028         T02sif = knsif;
082908131028         T02cod = 'VPO';
082909131028         t02tla = 'L'   ;
082910131028         T02ke1 ='DECOFI816' ;
082911131028         TNTBE_RicercaControllo  (kpjba : tibs02ds);
082912131028
082913131028         if  T02err  = *blank;
082914131028         xx= 1 ;
082915131028         yy= 1 ;
082916131028
082917131028         dow xx< 256   ;
082918131028           fil_ab(yy)= %subst(t02uni:xx:3)  ;
082919131028           yy=yy+ 1   ;
082920131028           xx=xx+ 3   ;
082921131028         enddo  ;
082922131028
082923131028         endif;
082924131028
082925131028         // se c'e' 999 tutte abilitate
082926131028         w003a='999'  ;
082927131028         if   %lookup(w003a:fil_ab) = 0  ;
082928131028         w003a=%editc(dutpou:'X')   ;
082929131028         if   %lookup(w003a:fil_ab) = 0  ;
082930131028         *in10 =*on  ;
082931131028         endif  ;
082932131028         endif  ;
082933131028
082934131028         endif  ;
082935131028
082936131028      /end-free
083000970527     C                   ENDSR
083100970526     C*------------------------------------------------------------------------*
083200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
083300970526     C*------------------------------------------------------------------------*
083400970526     C     CHKFOP        BEGSR
083500061026     C*
083600061026     C* INIZIALIZZO INDICATORI RELATIVI A CONTROLLI ED AUTORIZZAZIONI UTENTE SPECIALI
083700061026     C                   SETON                                        93
083800061026     C*
083900061026     C                   IF        §UTECAP = 'C'
084000061026     C* ABILITO CAMPI SOTTO AUTORIZZAZIONE SPECIALE "COMPLETA"
084100061026     C                   SETOFF                                       93
084200061026     C                   ENDIF
084300970526     C*
084400970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
084500970526IF  1C     DEPOP0        IFEQ      'C01'
084600970526     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
084700970526     C                   MOVEL     'S'           OPDATT
084800970526     C                   MOVEL     OPDRIG        OPD(1)
084900970626X   1C                   ELSE
085000970626     C* OPZIONI
085100970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
085200970626     C                   MOVEL     'S'           OPDATT
085300970626     C                   MOVEL     OPDRIG        OPD(5)
085400970626     C                   MOVEL     OPD(8)        OPDRIG                         *LOCALITA'
085500970626     C                   MOVEL     'S'           OPDATT
085600970626     C                   MOVEL     OPDRIG        OPD(8)
085700970626     C                   MOVEL     OPD(9)        OPDRIG                         *ECCEZIONI
085800970626     C                   MOVEL     'S'           OPDATT
085900970626     C                   MOVEL     OPDRIG        OPD(9)
086000970626     C                   MOVEL     OPD(10)       OPDRIG                         *TEMPI CONSEGNA
086100970626     C                   MOVEL     'S'           OPDATT
086200970626     C                   MOVEL     OPDRIG        OPD(10)
086300970626     C* FUNZIONI
086400970626     C                   MOVEL     FUD(8)        FUDRIG                         *LOCALITA'
086500970626     C                   MOVEL     'S'           FUDATT
086600970626     C                   MOVEL     FUDRIG        FUD(8)
086700970626     C                   MOVEL     FUD(9)        FUDRIG                         *ECCEZIONI
086800970626     C                   MOVEL     'S'           FUDATT
086900970626     C                   MOVEL     FUDRIG        FUD(9)
087000970626     C                   MOVEL     FUD(10)       FUDRIG                         *TEMPI CONSEGNA
087100970626     C                   MOVEL     'S'           FUDATT
087200970626     C                   MOVEL     FUDRIG        FUD(10)
087300970526E   1C                   ENDIF
087301131028     c* Se utente non abilitato, non attivo opzione 7=orari servizi
087302131028     c* lascio in manutenzione
087303131028     c                   if        *in10 and  depop0 = 'C05'
087304131028     C                   MOVEL     OPD(7)        OPDRIG
087305131028     C                   MOVEL     'N'           OPDATT
087306131028     C                   MOVEL     OPDRIG        OPD(7)
087307131028     c                   endif
087400970526     C*
087500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
087600970526IF  1C     DEPOP0        IFEQ      'C02'
087700970526     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
087800970526     C                   MOVEL     'S'           OPDATT
087900970526     C                   MOVEL     OPDRIG        OPD(2)
088000970917     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
088100970917     C                   MOVEL     'S'           OPDATT
088200970917     C                   MOVEL     OPDRIG        OPD(3)
088300061026     C  N93              MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
088400061026     C  N93              MOVEL     'S'           OPDATT
088500061026     C  N93              MOVEL     OPDRIG        OPD(4)
088501131028     c* non essendoci più l'annullamento logico il ripristino diventa inutile
088502131028     c* quindi asterisco le seguenti specifiche
088600131028     C* N93              MOVEL     OPD(13)       OPDRIG                         *RIPRISTINO
088700131028     C* N93              MOVEL     'S'           OPDATT
088800131028     C* N93              MOVEL     OPDRIG        OPD(13)
088900060403     C                   MOVEL     opd(15)       opdRIG                         *INSERIMENTO
089000060403     C                   MOVEL     'S'           opdATT
089100131028     C                   MOVEL     opdRIG        opd(15)
089200061016     C* CONSENTITO INSERIMENTO SOLO A PERSONALE "EDP"
089300061026     C                   IF        %subst(KNMUS:1:3) = 'EDP' AND *IN93 = *off
089400970526     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
089500970526     C                   MOVEL     'S'           FUDATT
089600970526     C                   MOVEL     FUDRIG        FUD(6)
089700061016     C                   ENDIF
089800970526E   1C                   ENDIF
089900970526     C*
090000970526     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
090100060403IF  1C**   DUTLPO        IFNE      'S'                                          *FILIALE
090200060403     C**                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
090300060403     C**                 MOVEL     'N'           OPDATT
090400060403     C**                 MOVEL     OPDRIG        OPD(4)
090500060403     C**                 MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
090600060403     C**                 MOVEL     'N'           OPDATT
090700060403     C**                 MOVEL     OPDRIG        OPD(7)
090800060403     C**                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
090900060403     C**                 MOVEL     'N'           FUDATT
091000060403     C**                 MOVEL     FUDRIG        FUD(6)
091100060403E   1C**                 ENDIF
091200970526     C*
091300970526     C* GESTIONE OPZIONI A VIDEO
091400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
091500970526     C                   EXSR      OPZGES
091600970526     C*
091700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
091800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
091900970526     C                   EXSR      FUNGES
092000970526     C*
092100970526     C                   ENDSR
092200020916     C*--------------------------------------------------------------------------------------------*
092300020916     C* REPERISCE I DATI UTENTE
092400020916     C*--------------------------------------------------------------------------------------------*
092500020916     C     REPDATIUTE    BEGSR
092600020916     C*
092700020916     C* INIZIALIZZA VARIABILI DI WRK
092800020916     C                   CLEAR                   TIBS34DS
092900020916     C                   CLEAR                   AZUTEDS
093000020916     C                   CLEAR                   DDATIUTE
093100020916     C*
093200020916     C     *DTAARA       DEFINE    §azute        azuteds
093300020916     C     *DTAARA       DEFINE    §datiute      ddatiute
093400020916     C                   IN(E)     *DTAARA
093500020916if  1C                   IF        %Error
093600020916     c                   EVAL      I34Tla = 'L'
093700020916     C                   CALL      'TIBS34R'
093800020916     C                   PARM                    Tibs34Ds
093900020916     C                   IN        *DTAARA
094000020916e   1C                   ENDIF
094100020916     C*
094200020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
094300020916     C                   MOVEL(P)  RSUT          DSFIRS           20
094400061026     C*
094500061026     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
094600061026     C                   EVAL      DUTE01 = UTEFAF
094700020916     C*
094800020916     C                   ENDSR
094900970526     C*------------------------------------------------------------------------*
095000970526     C* *INZSR - OPERAZIONI INIZIALI
095100970526     C*------------------------------------------------------------------------*
095200970526     C     *INZSR        BEGSR
095300970526     C*---
095400970526     C* RICEVIMENTO PARAMETRI
095500970526     C*---
095600970526     C     *ENTRY        PLIST
095700970612     C                   PARM                    KPJBA
095800970612     C                   PARM                    DSSI00
095900970526     C*
096000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
096100970526     C     XMSGPA        PLIST
096200970526     C                   PARM                    DSMSMS
096300970526     C                   PARM                    DSMSPG
096400970526     C                   PARM                    DSMSKE
096500970526     C                   PARM                    DSMSER
096600970526     C*
096700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
096800970526     C                   CALL      'XKEY'
096900970526     C                   PARM                    DSKEY
097000970526     C*---
097100970526     C* VARIABILI NON RIFERITE AL DATA BASE
097200970526     C*---
097300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
097400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
097500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
097600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
097700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
097800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
097900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
098000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
098100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
098200970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
098300970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
098400970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
098500970529     C     *LIKE         DEFINE    D00OP1        WOP1
098600970602     C     *LIKE         DEFINE    D00NAR        DNAR
098700970602     C     *LIKE         DEFINE    D00CAP        DCAP
098800970526     C*---
098900970526     C* VARIABILI DI CONTROLLO
099000970526     C*---
099100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
099200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
099300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
099400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
099500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
099600970526     C                   Z-ADD     11            SFLPAG                         *N°RIGHE X PAG
099700970526     C*
099800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
099900970526     C                   MOVEL     'TISI02R'     DSMSPG
100000970526     C                   MOVEL     'TISI02R'     MSGQUE
100100970526     C                   MOVEL     *BLANKS       MSGKEY
100200970526     C                   MOVEL     *BLANKS       DSMSMS
100300970526     C                   MOVEL     *BLANKS       DSMSKE
100400970526     C                   Z-ADD     *ZEROS        DSMSER
100500970526     C*---
100600970526     C* VARIABILI RIFERITE AL DATA BASE
100700970526     C*---
100800970526     C     *LIKE         DEFINE    CPCVER        KCCVER                         *AZCPC00F
100900970526     C     *LIKE         DEFINE    CPCNAR        KCCNAR
101000970526     C     *LIKE         DEFINE    CPCCAP        KCCCAP
101100970526     C     *LIKE         DEFINE    CPCPRV        KCCPRV
101200970526     C     *LIKE         DEFINE    CPCLNA        KCCLNA
101300970526     C     *LIKE         DEFINE    CPCNAR        DEPNAR                         *DEPOSITI
101400970526     C     *LIKE         DEFINE    CPCCAP        DEPCAP
101500970526     C     *LIKE         DEFINE    CPCLNA        DEPLNA
101600970526     C     *LIKE         DEFINE    CPCPRV        DEPPRV
101700970526     C*---
101800970526     C* CHIAVI DI LETTURA
101900970526     C*---
102000970526     C* LETTURA AZCPC01L - COMPLETA
102100970527     C     KEYCPC        KLIST
102200970526     C                   KFLD                    KCCVER                         *VERSIONE
102300970526     C                   KFLD                    KCCNAR                         *NAZIONE
102400970526     C                   KFLD                    KCCCAP                         *CAP
102500970526     C* LETTURA AZCPC02L - PARZIALE
102600970527     C     KE2CPC        KLIST
102700970526     C                   KFLD                    KCCVER                         *VERSIONE
102800970526     C                   KFLD                    KCCNAR                         *NAZIONE
102900970526     C                   KFLD                    KCCPRV                         *PROVINCIA
103000970526     C* LETTURA AZCPC03L - PARZIALE
103100970527     C     KE3CPC        KLIST
103200970526     C                   KFLD                    KCCVER                         *VERSIONE
103300970526     C                   KFLD                    KCCNAR                         *NAZIONE
103400970526     C                   KFLD                    KCCLNA                         *LINEA ARRIVO
103500130823     C     Kcos          KLIST
103600130823     C                   KFLD                    KCCVER                         *VERSIONE
103700130823     C                   KFLD                    KCCNAR                         *NAZIONE
103800130823     C                   KFLD                    KCCCAP                         *CAP
103900130823     C                   KFLD                    Kloc
104000970527     C*
104100970527     C* REPERIMENTO DATI UTENTE
104200020916     C                   EXSR      REPDATIUTE
104300970527     C*
104400970527     C* CARICA LE TABELLE OCCORRENTI
104500970527     C                   EXSR      CARTAB
104600970526     C*
104700970526     C* DEPOSITA LE OPZIONI DI ENTRATA
104800970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
104900970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
105000970526     C*
105100970526     C* CALCOLA LA DATA CORRENTE
105200970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
105300970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
105400970526     C                   Z-ADD     WN8           G08DAT
105500970526     C                   Z-ADD     *ZEROS        G08INV
105600970526     C                   MOVEL     '0'           G08ERR
105700970526     C                   CALL      'XSRDA8'
105800970526     C                   PARM                    WLBDA8
105900970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
106000970526     C*
106100970526     C* IMPOSTA I PARAMETRI DI INGRESSO
106200970526     C                   Z-ADD     D00DRI        G08INV
106300970526     C                   Z-ADD     *ZEROS        G08DAT
106400970526     C                   MOVEL     '3'           G08ERR
106500970526     C                   CALL      'XSRDA8'
106600970526     C                   PARM                    WLBDA8
106700970526     C                   Z-ADD     G08DAT        VIDPDR                         *DATA RIFERIMENTO
106800970526     C                   MOVEL     D00NAR        VIDPNA                         *NAZIONE
106900970526     C                   MOVEL     D00CAP        VIDPCA                         *CAP
107000970526     C                   EXSR      CHKPAR
107100970526IF  1C     ERRL00        IFEQ      '1'
107200970526     C                   MOVEL     '1'           D00ERR
107300970526     C                   MOVEL     DSMSMS        D00MSG
107400970526X   1C                   ELSE
107500970526     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
107600970526     C                   MOVEL     VIDPCA        DEPCAP
107700970526E   1C                   ENDIF
107800970703     C*
107900970703     C* REPERISCE LA VERSIONE CAPPARIO -SE VUOTA-
108000970703IF  1C     D00VER        IFEQ      *ZEROS
108100970707     C                   Z-ADD     D00DRI        P96DRI            8 0
108200970707     C                   Z-ADD     *ZEROS        P96VER            5 0
108300970707     C                   CALL      'TISI96R'
108400970707     C                   PARM                    P96DRI
108500970707     C                   PARM                    P96VER
108600970707     C                   Z-ADD     P96VER        D00VER
108700970703E   1C                   ENDIF
108800970526     C*
108900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
109000970526     C                   EXSR      CHKFOP
109100970530     C*
109200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
109300970530IF  1C     DEPOP0        IFEQ      'C01'
109400970530     C                   MOVEL     DEC(3)        VIDING
109500970530E   1C                   ENDIF
109600970530IF  1C     DEPOP0        IFEQ      'C02'
109700970530     C                   MOVEL     DEC(1)        VIDING
109800970530E   1C                   ENDIF
109900970530IF  1C     DEPOP0        IFEQ      'C05'
110000970530     C                   MOVEL     DEC(2)        VIDING
110100970530E   1C                   ENDIF
110200970526     C*
110300970526     C                   ENDSR
110400970526     C*------------------------------------------------------------------------*
110500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
110600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
110700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
110800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
110900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
111000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
111100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
111200970526     C*------------------------------------------------------------------------*
111300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
111400970526     C*------------------------------------------------------------------------*
111500970526     C     POSSFL        BEGSR
111600970526     C*
111700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
111800970526     C                   EXSR      CLRSFL
111900970526     C*
112000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
112100970526     C                   EXSR      SETFIL
112200970526     C*
112300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
112400970526     C     SFLEOF        IFEQ      0
112500970526     C                   EXSR      CARSFL
112600970526     C                   END
112700970526     C*
112800970526     C                   ENDSR
112900970526     C*------------------------------------------------------------------------*
113000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
113100970526     C*------------------------------------------------------------------------*
113200970526     C     ENTATT        BEGSR
113300970526     C*
113400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
113500970526     C                   Z-ADD     0             OPZOK             1 0
113600970526     C*
113700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
113800970526     C                   Z-ADD     0             SFLVAR            5 0
113900970526     C*
114000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
114100970526     C                   Z-ADD     0             SFLERR
114200970526     C*
114300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
114400970526     C                   Z-ADD     SFLULT        WFLULT
114500970526     C*
114600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
114700970526     C                   READC     SI02D05                                98
114800970526     C     *IN98         DOWEQ     '0'
114900970526     C*
115000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
115100970526     C                   ADD       1             SFLVAR
115200970526     C*
115300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
115400970526     C     OPZOK         IFEQ      0
115500970526     C*
115600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
115700970526     C     OPZKEY        IFNE      *BLANKS
115800970526     C                   EXSR      OPZCON
115900970526     C                   END
116000970526     C*
116100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
116200970526     C*            2. PULIZIA SCELTA IN SFL
116300970526     C     OPZOK         IFEQ      0
116400970526     C                   MOVEL     '0'           *IN41
116500970526     C                   MOVEL     *BLANKS       OPZKEY
116600970526     C                   ELSE
116700970526     C*
116800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
116900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
117000970526     C                   MOVEL     '1'           *IN41
117100970526     C                   Z-ADD     SFLULT        SFLERR
117200970526     C                   END
117300970526     C                   ELSE
117400970526     C*
117500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
117600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
117700970526     C                   MOVEL     '1'           *IN41
117800970526     C                   END                                                    OPZOK=0
117900970526     C*
118000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
118100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
118200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
118300970526     C     OPZOK         IFNE      0
118400970526     C                   Z-ADD     SFLERR        SFLNBR
118500970526     C                   ELSE
118600970526     C                   Z-ADD     SFLULT        SFLNBR
118700970526     C                   END
118800970526     C*
118900970526     C* AGGIORNO RECORD SFL
119000130828     c**                 if        vihcos='S'
119100130828     c*+                 seton                                        02
119200130828     c**                 else
119300130828     c**                 setoff                                       02
119400130828     c**                 endif
119500130828     c
119600970526     C                   UPDATE    SI02D05
119700970526     C*
119800970526     C                   READC     SI02D05                                98
119900970526     C                   END                                                    DO WHILE
120000970526     C*
120100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
120200970526     C                   Z-ADD     WFLULT        SFLULT
120300970526     C*
120400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
120500970526     C     SFLVAR        IFEQ      0
120600970526     C*---                 MOVEL'1'       FINL00
120700970526     C                   END
120800970526     C*
120900970526     C                   ENDSR
121000970526     C*------------------------------------------------------------------------*
121100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
121200970526     C*------------------------------------------------------------------------*
121300970526     C     CLRSFL        BEGSR
121400970526     C*
121500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
121600970526     C* (0=NON FINE FILE  1=FINE FILE)
121700970526     C                   Z-ADD     0             SFLEOF
121800970526     C*
121900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
122000970526     C                   Z-ADD     0             SFLULT
122100970526     C*
122200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
122300970526     C                   MOVEL     '1'           *IN40
122400970526     C                   WRITE     SI02D06
122500970526     C                   MOVEL     '0'           *IN40
122600970526     C*
122700970526     C                   ENDSR
122800970526     C*------------------------------------------------------------------------*
122900970526     C* CARSFL - CARICAMENTO SFL
123000970526     C*------------------------------------------------------------------------*
123100970526     C     CARSFL        BEGSR
123200970526     C*
123300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
123400970526     C     SFLULT        IFEQ      0
123500970526     C                   EXSR      SETFIL
123600970526     C                   END
123700970526     C*
123800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
123900970526     C                   Z-ADD     SFLULT        WFLULT
124000970526     C*
124100970526     C* CICLO:
124200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
124300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
124400970526     C                   Z-ADD     1             SFLRIG
124500970526     C     SFLRIG        DOWLE     SFLPAG
124600970526     C     SFLEOF        ANDEQ     0
124700970526     C*
124800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
124900970526     C                   MOVEL     *BLANKS       OPZKEY
125000970526     C*
125100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
125200970526     C                   EXSR      IRESFL
125300970526     C*
125400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
125500970526     C                   ADD       1             SFLULT
125600970526     C*
125700970526     C* SCRITTURA RECORD SFL
125800970526     C                   WRITE     SI02D05
125900970526     C                   ADD       1             SFLRIG
126000970526     C*
126100970526     C* LETTURA FILE DA CARICARE
126200970526     C                   EXSR      LETFIL
126300970526     C                   END                                                    DO WHILE
126400970526     C*
126500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
126600970526     C     WFLULT        IFNE      SFLULT
126700970526     C                   Z-ADD     WFLULT        SFLNBR
126800970526     C                   ADD       1             SFLNBR
126900970526     C                   END
127000970526     C*
127100970526     C                   ENDSR
127200970526     C*------------------------------------------------------------------------*
127300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
127400970526     C*------------------------------------------------------------------------*
127500970526     C     OUTSFL        BEGSR
127600970526     C*
127700970526     C* WRITE RECORD TESTATA VIDEO
127800970526     C                   WRITE     SI02D01
127900970526     C*
128000970526     C* VISUALIZZAZIONE MESSAGGIO
128100970526     C                   EXSR      SNDMSG
128200970526     C*
128300970526     C* WRITE RECORD OPZIONI VIDEO
128400970526     C                   WRITE     SI02D02
128500970526     C*
128600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
128700970526     C                   WRITE     SI02D03
128800970526     C*
128900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
129000970526     C                   WRITE     SI02D04
129100970526     C*
129200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
129300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
129400970526     C     SFLEOF        IFEQ      0
129500970526     C                   MOVEL     '0'           *IN42
129600970526     C                   ELSE
129700970526     C*
129800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
129900970526     C                   MOVEL     '1'           *IN42
130000970526     C                   END
130100970526     C*
130200970526     C* WRITE RECORD TASTI FUNZIONALI
130300970526     C                   WRITE     SI02D09
130400970526     C*
130500970526     C* CASO DI SFL VUOTO
130600970526     C     SFLULT        IFLE      0
130700970526     C*
130800970526     C*    - WRITE RECORD VUOTO
130900970526     C                   WRITE     SI02D0A
131000970526     C                   READ      SI02D0A                                99
131100970526     C*
131200970526     C                   ELSE
131300970526     C*
131400970526     C* CASO DI SFL CON RECORDS
131500970526     C*
131600970526     C*    - WRITE RECORD CONTROLLO SFL
131700970526     C                   WRITE     SI02D06
131800970526     C                   READ      SI02D06                                99
131900970526     C                   END
132000970526     C*
132100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
132200970526     C                   READ      SI02D03                                99
132300970526     C*
132400970526     C                   ENDSR
132500970526     C*------------------------------------------------------------------------*
132600970526     C* SNDMSG - MANDA MESSAGGIO
132700970526     C*------------------------------------------------------------------------*
132800970526     C     SNDMSG        BEGSR
132900970526     C*
133000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
133100970526     C                   MOVEL     '1'           *IN40
133200970526     C                   WRITE     SI02DMC
133300970526     C                   MOVEL     '0'           *IN40
133400970526     C*
133500970526     C* VISUALIZZAZIONE MESSAGGIO
133600970526     C                   Z-ADD     0             MSGULT
133700970526     C                   CALL      'XMSG'        XMSGPA
133800970526     C     DSMSER        IFEQ      0
133900970526     C                   MOVEL     DSMSKE        MSGKEY
134000970526     C                   END
134100970526     C                   Z-ADD     1             MSGULT
134200970526     C                   WRITE     SI02DMS                                      SFL MESS
134300970526     C                   WRITE     SI02DMC                                      CONTR MESS
134400970526     C*
134500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
134600970526     C                   MOVEL     *BLANKS       DSMSMS
134700970526     C*
134800970526     C                   ENDSR
134900970526     C*------------------------------------------------------------------------*
135000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
135100970526     C*------------------------------------------------------------------------*
135200970526     C     FUNCON        BEGSR
135300970526     C*
135400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
135500970526     C                   EXSR      FUNVAR
135600970526     C*
135700970526     C* PULISCO CAMPI DI LAVORO
135800970526     C                   Z-ADD     0             I                 5 0
135900970526     C                   Z-ADD     0             W                 5 0
136000970526     C*
136100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
136200970526     C     FUNKEY        IFNE      *BLANKS
136300970526     C     1             DO        24            I
136400970526     C                   MOVEL     FUD(I)        FUDRIG
136500970526     C*
136600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
136700970526     C     FUNKEY        IFEQ      FUDFUN
136800970526     C     FUDATT        ANDEQ     'S'
136900970526     C                   Z-ADD     I             W
137000970526     C                   END
137100970526     C                   END                                                    DO WHILE
137200970526     C*
137300970526     C* FUNZIONE NON ATTIVA (ERRORE)
137400970526     C     W             IFEQ      0
137500970526     C*
137600970526     C* EMISSIONE MESSAGGIO DI ERRORE
137700970526     C                   MOVEL     MSG(2)        DSMSMS
137800970526     C                   END                                                    W=0
137900970526     C                   END                                                    FUNKEY NE *BLANKS
138000970526     C*
138100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
138200970526     C     W             IFNE      0
138300970526     C     FUNKEY        OREQ      *BLANKS
138400970526     C*
138500970526     C     FUNKEY        IFEQ      *BLANKS
138600970526     C                   EXSR      ENTGES
138700970526     C                   END
138800970526     C     FUNKEY        IFEQ      '03'
138900970526     C                   EXSR      F03GES
139000970526     C                   END
139100970526     C     FUNKEY        IFEQ      '05'
139200970526     C                   EXSR      F05GES
139300970526     C                   END
139400970526     C     FUNKEY        IFEQ      '06'
139500970526     C                   EXSR      F06GES
139600970526     C                   END
139700970528     C     FUNKEY        IFEQ      '08'
139800970528     C                   EXSR      F08GES
139900970528     C                   END
140000970528     C     FUNKEY        IFEQ      '09'
140100980225     C                   EXSR      F09GES
140200970528     C                   END
140300970528     C     FUNKEY        IFEQ      '10'
140400980225     C                   EXSR      F10GES
140500970528     C                   END
140600060403     C     FUNKEY        IFEQ      '14'
140700060403     C                   EXSR      F14GES
140800060403     C                   END
140900060927     C     FUNKEY        IFEQ      '15'
141000060927     C                   EXSR      F15GES
141100060927     C                   END
141200970526     C     FUNKEY        IFEQ      '12'
141300970526     C                   EXSR      F12GES
141400970526     C                   END
141500970526     C     FUNKEY        IFEQ      '21'
141600970526     C                   EXSR      F21GES
141700970526     C                   END
141800970526     C     FUNKEY        IFEQ      '22'
141900970526     C                   EXSR      F22GES
142000970526     C                   END
142100970526     C     FUNKEY        IFEQ      '23'
142200970526     C                   EXSR      F23GES
142300970526     C                   END
142400970526     C     FUNKEY        IFEQ      '24'
142500970526     C                   EXSR      F24GES
142600970526     C                   END
142700970526     C     FUNKEY        IFEQ      'RU'
142800970526     C                   EXSR      RUPGES
142900970526     C                   END
143000970526     C*
143100970526     C                   END
143200970526     C*
143300970526     C                   ENDSR
143400970526     C*------------------------------------------------------------------------*
143500970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
143600970526     C*------------------------------------------------------------------------*
143700970526     C     FUNVAR        BEGSR
143800970526     C*
143900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
144000970526     C     KEY           IFEQ      ENTER
144100970526     C                   MOVEL     *BLANKS       FUNKEY
144200970526     C                   END
144300970526     C     KEY           IFEQ      F1
144400970526     C                   MOVEL     '01'          FUNKEY
144500970526     C                   END
144600970526     C     KEY           IFEQ      F2
144700970526     C                   MOVEL     '02'          FUNKEY
144800970526     C                   END
144900970526     C     KEY           IFEQ      F3
145000970526     C                   MOVEL     '03'          FUNKEY
145100970526     C                   END
145200970526     C     KEY           IFEQ      F4
145300970526     C                   MOVEL     '04'          FUNKEY
145400970526     C                   END
145500970526     C     KEY           IFEQ      F5
145600970526     C                   MOVEL     '05'          FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      F6
145900970526     C                   MOVEL     '06'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      F7
146200970526     C                   MOVEL     '07'          FUNKEY
146300970526     C                   END
146400970526     C     KEY           IFEQ      F8
146500970526     C                   MOVEL     '08'          FUNKEY
146600970526     C                   END
146700970526     C     KEY           IFEQ      F9
146800970526     C                   MOVEL     '09'          FUNKEY
146900970526     C                   END
147000970526     C     KEY           IFEQ      F10
147100970526     C                   MOVEL     '10'          FUNKEY
147200970526     C                   END
147300970526     C     KEY           IFEQ      F11
147400970526     C                   MOVEL     '11'          FUNKEY
147500970526     C                   END
147600970526     C     KEY           IFEQ      F12
147700970526     C                   MOVEL     '12'          FUNKEY
147800970526     C                   END
147900970526     C     KEY           IFEQ      F13
148000970526     C                   MOVEL     '13'          FUNKEY
148100970526     C                   END
148200970526     C     KEY           IFEQ      F14
148300970526     C                   MOVEL     '14'          FUNKEY
148400970526     C                   END
148500970526     C     KEY           IFEQ      F15
148600970526     C                   MOVEL     '15'          FUNKEY
148700970526     C                   END
148800970526     C     KEY           IFEQ      F16
148900970526     C                   MOVEL     '16'          FUNKEY
149000970526     C                   END
149100970526     C     KEY           IFEQ      F17
149200970526     C                   MOVEL     '17'          FUNKEY
149300970526     C                   END
149400970526     C     KEY           IFEQ      F18
149500970526     C                   MOVEL     '18'          FUNKEY
149600970526     C                   END
149700970526     C     KEY           IFEQ      F19
149800970526     C                   MOVEL     '19'          FUNKEY
149900970526     C                   END
150000970526     C     KEY           IFEQ      F20
150100970526     C                   MOVEL     '20'          FUNKEY
150200970526     C                   END
150300970526     C     KEY           IFEQ      F21
150400970526     C                   MOVEL     '21'          FUNKEY
150500970526     C                   END
150600970526     C     KEY           IFEQ      F22
150700970526     C                   MOVEL     '22'          FUNKEY
150800970526     C                   END
150900970526     C     KEY           IFEQ      F23
151000970526     C                   MOVEL     '23'          FUNKEY
151100970526     C                   END
151200970526     C     KEY           IFEQ      F24
151300970526     C                   MOVEL     '24'          FUNKEY
151400970526     C                   END
151500970526     C     KEY           IFEQ      ROLLUP
151600970526     C                   MOVEL     'RU'          FUNKEY
151700970526     C                   END
151800970526     C     KEY           IFEQ      HELP
151900970526     C                   MOVEL     'HP'          FUNKEY
152000970526     C                   END
152100970526     C*
152200970526     C                   ENDSR
152300970526     C*------------------------------------------------------------------------*
152400970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
152500970526     C*------------------------------------------------------------------------*
152600970526     C     FUNGES        BEGSR
152700970526     C*
152800970526     C* PULISCO CAMPI DI LAVORO
152900970526     C                   Z-ADD     0             W                 5 0
153000970526     C*
153100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
153200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
153300970526     C     1             DO        24            I
153400970526     C                   MOVEL     FUD(I)        FUDRIG
153500970526     C*
153600970526     C* PULIZIA FUNZIONI PRECEDENTI
153700970526     C                   MOVEL     *BLANKS       FUV(I)
153800970526     C*
153900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
154000970526     C* (SALTA F23 E F24)
154100970526     C     FUDATT        IFEQ      'S'
154200970526     C     FUDFUN        ANDNE     '23'
154300970526     C     FUDFUN        ANDNE     '24'
154400970526     C                   ADD       1             W
154500970526     C                   MOVEL     FUDDES        FUV(W)
154600970526     C                   END
154700970526     C                   END                                                    DO WHILE
154800970526     C*
154900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
155000970526     C* ALTRIMENTI DISATTIVO F24
155100970526     C     1             DO        24            I
155200970526     C                   MOVEL     FUD(I)        FUDRIG
155300970526     C*
155400970526     C     FUDFUN        IFEQ      '24'
155500970526     C     W             IFGT      6
155600970526     C                   MOVEL     'S'           FUDATT
155700970526     C                   ELSE
155800970526     C                   MOVEL     *BLANKS       FUDATT
155900970526     C                   END                                                    W GT 8
156000970526     C                   MOVEL     FUDRIG        FUD(I)
156100970526     C                   END
156200970526     C                   END                                                    DO WHILE
156300970526     C*
156400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
156500970526     C     W             DIV       6             I
156600970526     C                   MVR                     Z
156700970526     C*
156800970526     C     Z             IFNE      0
156900970526     C     I             ADD       1             Z
157000970526     C                   ELSE
157100970526     C                   Z-ADD     I             Z
157200970526     C                   END
157300970526     C*
157400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
157500970526     C                   ADD       1             FUNGRU
157600970526     C     FUNGRU        IFGT      Z
157700970526     C     FUNGRU        ORLT      1
157800970526     C                   Z-ADD     1             FUNGRU
157900970526     C                   END
158000970526     C*
158100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
158200970526     C     FUNGRU        IFEQ      1
158300970526     C                   MOVEL     FUV(1)        FUN001
158400970526     C                   MOVEL     FUV(2)        FUN002
158500970526     C                   MOVEL     FUV(3)        FUN003
158600970526     C                   MOVEL     FUV(4)        FUN004
158700970526     C                   MOVEL     FUV(5)        FUN005
158800970526     C                   MOVEL     FUV(6)        FUN006
158900970526     C                   END
159000970526     C*
159100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
159200970526     C     FUNGRU        IFEQ      2
159300970526     C                   MOVEL     FUV(7)        FUN001
159400970526     C                   MOVEL     FUV(8)        FUN002
159500970526     C                   MOVEL     FUV(9)        FUN003
159600970526     C                   MOVEL     FUV(10)       FUN004
159700970526     C                   MOVEL     FUV(11)       FUN005
159800970526     C                   MOVEL     FUV(12)       FUN006
159900970526     C                   END
160000970526     C*
160100970526     C* TERZO GRUPPO DI 6 FUNZIONI
160200970526     C     FUNGRU        IFEQ      3
160300970526     C                   MOVEL     FUV(13)       FUN001
160400970526     C                   MOVEL     FUV(14)       FUN002
160500970526     C                   MOVEL     FUV(15)       FUN003
160600970526     C                   MOVEL     FUV(16)       FUN004
160700970526     C                   MOVEL     FUV(17)       FUN005
160800970526     C                   MOVEL     FUV(18)       FUN006
160900970526     C                   END
161000970526     C*
161100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
161200970526     C     FUNGRU        IFEQ      4
161300970526     C                   MOVEL     FUV(19)       FUN001
161400970526     C                   MOVEL     FUV(20)       FUN002
161500970526     C                   MOVEL     FUV(21)       FUN003
161600970526     C                   MOVEL     FUV(22)       FUN004
161700970526     C                   MOVEL     *BLANKS       FUN005
161800970526     C                   MOVEL     *BLANKS       FUN006
161900970526     C                   END
162000970526     C*
162100970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
162200970526     C                   MOVEL     *BLANKS       FUN007
162300970526     C                   MOVEL     *BLANKS       FUN008
162400970526     C*
162500970526     C     1             DO        24            I
162600970526     C                   MOVEL     FUD(I)        FUDRIG
162700970526     C     FUDFUN        IFEQ      '23'
162800970526     C     FUDATT        ANDEQ     'S'
162900970526     C                   MOVEL     FUDDES        FUN007
163000970526     C                   END
163100970526     C     FUDFUN        IFEQ      '24'
163200970526     C     FUDATT        ANDEQ     'S'
163300970526     C                   MOVEL     FUDDES        FUN008
163400970526     C                   END
163500970526     C                   END                                                    DO
163600970526     C*
163700970526     C                   ENDSR
163800970526     C*------------------------------------------------------------------------*
163900970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
164000970526     C*------------------------------------------------------------------------*
164100970526     C     OPZGES        BEGSR
164200970526     C*
164300970526     C* PULISCO CAMPI DI LAVORO
164400970526     C                   Z-ADD     0             W                 5 0
164500970526     C*
164600970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
164700970526     C* E CARICO TABELLA OPZIONI ATTIVE
164800970526     C     1             DO        24            I
164900970526     C                   MOVEL     OPD(I)        OPDRIG
165000970526     C*
165100970526     C* PULIZIA OPZIONI PRECEDENTI
165200970526     C                   MOVEL     *BLANKS       OPV(I)
165300970526     C*
165400970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
165500970526     C     OPDATT        IFEQ      'S'
165600970526     C                   ADD       1             W
165700970526     C                   MOVEL     OPDDES        OPV(W)
165800970526     C                   END
165900970526     C                   END                                                    DO WHILE
166000970526     C*
166100970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
166200970526     C* ALTRIMENTI DISATTIVO F23
166300970526     C     1             DO        24            I
166400970526     C                   MOVEL     FUD(I)        FUDRIG
166500970526     C*
166600970526     C     FUDFUN        IFEQ      '23'
166700970526     C     W             IFGT      8
166800970526     C                   MOVEL     'S'           FUDATT
166900970526     C                   ELSE
167000970526     C                   MOVEL     *BLANKS       FUDATT
167100970526     C                   END                                                    W GT 8
167200970526     C                   MOVEL     FUDRIG        FUD(I)
167300970526     C                   END
167400970526     C                   END                                                    DO WHILE
167500970526     C*
167600970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
167700970526     C     W             DIV       8             I
167800970526     C                   MVR                     Z                 5 0
167900970526     C*
168000970526     C     Z             IFNE      0
168100970526     C     I             ADD       1             Z
168200970526     C                   ELSE
168300970526     C                   Z-ADD     I             Z
168400970526     C                   END
168500970526     C*
168600970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
168700970526     C                   ADD       1             OPZGRU
168800970526     C     OPZGRU        IFGT      Z
168900970526     C     OPZGRU        ORLT      1
169000970526     C                   Z-ADD     1             OPZGRU
169100970526     C                   END
169200970526     C*
169300970526     C* PRIMO GRUPPO DI 8 OPZIONI
169400970526     C     OPZGRU        IFEQ      1
169500970526     C                   MOVEL     OPV(1)        OPZ001
169600970526     C                   MOVEL     OPV(2)        OPZ002
169700970526     C                   MOVEL     OPV(3)        OPZ003
169800970526     C                   MOVEL     OPV(4)        OPZ004
169900970526     C                   MOVEL     OPV(5)        OPZ005
170000970526     C                   MOVEL     OPV(6)        OPZ006
170100970526     C                   MOVEL     OPV(7)        OPZ007
170200970526     C                   MOVEL     OPV(8)        OPZ008
170300970526     C                   END
170400970526     C*
170500970526     C* SECONDO GRUPPO DI 8 OPZIONI
170600970526     C     OPZGRU        IFEQ      2
170700970526     C                   MOVEL     OPV(9)        OPZ001
170800970526     C                   MOVEL     OPV(10)       OPZ002
170900970526     C                   MOVEL     OPV(11)       OPZ003
171000970526     C                   MOVEL     OPV(12)       OPZ004
171100970526     C                   MOVEL     OPV(13)       OPZ005
171200970526     C                   MOVEL     OPV(14)       OPZ006
171300970526     C                   MOVEL     OPV(15)       OPZ007
171400970526     C                   MOVEL     OPV(16)       OPZ008
171500970526     C                   END
171600970526     C*
171700970526     C* TERZO GRUPPO DI 8 OPZIONI
171800970526     C     OPZGRU        IFEQ      3
171900970526     C                   MOVEL     OPV(17)       OPZ001
172000970526     C                   MOVEL     OPV(18)       OPZ002
172100970526     C                   MOVEL     OPV(19)       OPZ003
172200970526     C                   MOVEL     OPV(20)       OPZ004
172300970526     C                   MOVEL     OPV(21)       OPZ005
172400970526     C                   MOVEL     OPV(22)       OPZ006
172500970526     C                   MOVEL     OPV(23)       OPZ007
172600970526     C                   MOVEL     OPV(24)       OPZ008
172700970526     C                   END
172800970526     C*
172900970526     C                   ENDSR
173000970526     C*------------------------------------------------------------------------*
173100970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
173200970526     C*------------------------------------------------------------------------*
173300970526     C     OPZCON        BEGSR
173400970526     C*
173500970526     C* PULISCO CAMPI DI LAVORO
173600970526     C                   Z-ADD     0             I                 5 0
173700970526     C                   Z-ADD     0             W                 5 0
173800970526     C*
173900970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
174000970526     C     1             DO        24            I
174100970526     C                   MOVEL     OPD(I)        OPDRIG
174200970526     C*
174300970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
174400970526     C     OPZKEY        IFEQ      OPDOPZ
174500970526     C     OPZKEY        OREQ      OPDOP1
174600970526     C*
174700970526     C     OPDATT        IFEQ      'S'
174800970526     C                   MOVEL     OPDOPZ        OPZKEY
174900970526     C                   Z-ADD     I             W
175000970526     C                   END
175100970526     C                   END
175200970526     C*
175300970526     C                   END                                                    DO WHILE
175400970526     C*
175500970526     C* OPZIONE NON ATTIVA (ERRORE)
175600970526     C     W             IFEQ      0
175700970526     C*
175800970526     C* OPZIONE NON OK
175900970526     C                   Z-ADD     1             OPZOK
176000970526     C*
176100970526     C* EMISSIONE MESSAGGIO DI ERRORE
176200970526     C                   MOVEL     MSG(1)        DSMSMS
176300970526     C                   ELSE
176400970526     C*
176500970526     C* OPZIONE ATTIVA (PROCEDI ...)
176600970526     C*
176700970526     C     OPZKEY        IFEQ      ' 1'
176800970526     C                   EXSR      O01GES
176900970526     C                   END
177000970526     C     OPZKEY        IFEQ      ' 2'
177100970526     C                   EXSR      O02GES
177200970526     C                   END
177300970917     C     OPZKEY        IFEQ      ' 3'
177400970917     C                   EXSR      O03GES
177500970917     C                   END
177600970526     C     OPZKEY        IFEQ      ' 4'
177700970526     C                   EXSR      O04GES
177800970526     C                   END
177900970526     C     OPZKEY        IFEQ      ' 5'
178000970526     C                   EXSR      O05GES
178100970526     C                   END
178200970526     C     OPZKEY        IFEQ      ' 7'
178300970526     C                   EXSR      O07GES
178400970526     C                   END
178500970526     C     OPZKEY        IFEQ      ' 8'
178600970526     C                   EXSR      O08GES
178700970526     C                   END
178800970526     C     OPZKEY        IFEQ      ' 9'
178900970526     C                   EXSR      O09GES
179000970526     C                   END
179100970526     C     OPZKEY        IFEQ      '10'
179200970526     C                   EXSR      O10GES
179300970526     C                   END
179301131028     C     OPZKEY        IFEQ      '13'
179302131028     C                   EXSR      O13GES
179303131028     C                   END
179400060403     C     OPZKEY        IFEQ      '14'
179500060403     C                   EXSR      O14GES
179600060403     C                   END
179700060403     C     OPZKEY        IFEQ      '15'
179800060403     C                   EXSR      O15GES
179900060403     C                   END
180000970526     C*
180100970526     C                   END                                                    OPZ.NON ATTIVA
180200970526     C*
180300970526     C                   ENDSR
180400970526     C*------------------------------------------------------------------------*
180500970526     C* NOTE COSTRUZIONE SCHIERE
180600970526     C*------------------------------------------------------------------------*
180700970526     C*
180800970526     C* -MSG-MESSAGGI
180900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
181000970526     C* (NON TOCCARE MSG,1 E MSG,2)
181100970526     C*
181200970526     C* -CMD-COMANDI
181300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
181400970526     C*
181500970526     C* -OPD-OPZIONI
181600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
181700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
181800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
181900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
182000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
182100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
182200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
182300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
182400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
182500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
182600970526     C*
182700970526     C* -FUD-FUNZIONI
182800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
182900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
183000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
183100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
183200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
183300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
183400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
183500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
183600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
183700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
183800970526     O*------------------------------------------------------------------------*
183900970526** DEC - DECODIFICHE
184000970526    GESTIONE                                                     1
184100970526INTERROGAZIONE                                                   2
184200970530     SCELTA                                                      3
1843009705264
1844009705265
184500970526** MSG - MESSAGGI
184600970526TISI02R- Opzione non gestita                                                   1
184700970526TISI02R- Tasto funzionale non gestito                                          2
184800970704TISI02R- 3
184900970526TISI02R- Record annullato. Consentito solo il ripristino                       4
185000970526TISI02R- Record non annullato. Impossibile ripristinare                        5
185100970526TISI02R- Accettato solo un parametro di inizio lista                           6
185200970526TISI02R- Errori nei parametri di ingresso                                      7
185300970526TISI02R- Data errata                                                           8
185400970526TISI02R- 9                                                                     9
185500970526TISI02R- 10                                                                   10
185600970526TISI02R- 11                                                                   11
185700970526TISI02R- 12                                                                   12
185800970526TISI02R- 13                                                                   13
185900970526TISI02R- 14                                                                   14
186000970526TISI02R- 15                                                                   15
186100970526TISI02R- 16                                                                   16
186200970526TISI02R- 17                                                                   17
186300970526TISI02R- 18                                                                   18
186400970526TISI02R- 19                                                                   19
186500970526TISI02R- 20                                                                   20
186600970526** CMD - COMANDI CL
186700970526WRKSPLF
186800970526
186900970526
187000970526
187100970526
187200970526** OPD - OPZIONI
187300970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
187400970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
187500970917 3=Duplica         -3 -     (S/N)=N                        opz. 03¬
187600970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
187700970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
187800970526                   -6 -     (S/N)=N                        opz. 06¬
187900131028 7=Orari Servizi   -7 -     (S/N)=S                        opz. 07¬
188000970626 8=Località        -8 -     (S/N)=N                        opz. 08¬
188100970626 9=Eccezioni       -9 -     (S/N)=N                        opz. 09¬
18820097062610=Tempi consegna  -10-     (S/N)=N                        opz. 10¬
188300970526                   -  -     (S/N)=N                        opz. 11¬
188400060403                   -  -     (S/N)=N                        opz. 12¬
18850013102813=Ripristino      -13-     (S/N)=N                        opz. 13¬
18860006040314=Cal.Eventi Part -14-     (S/N)=S                        opz. 14¬
18870006040315=InserisciEvePart-15-     (S/N)=N                        opz. 15¬
188800970526                   -  -     (S/N)=N                        opz. 16¬
188900970526                   -  -     (S/N)=N                        opz. 17¬
189000970526                   -  -     (S/N)=N                        opz. 18¬
189100970526                   -  -     (S/N)=N                        opz. 19¬
189200970526                   -  -     (S/N)=N                        opz. 20¬
189300970526                   -  -     (S/N)=N                        opz. 21¬
189400970526                   -  -     (S/N)=N                        opz. 22¬
189500970526                   -  -     (S/N)=N                        opz. 23¬
189600970526                   -  -     (S/N)=N                        opz. 24¬
189700970526** FUD - FUNZIONI
189800970526                   -        (S/N)=N                        funz.01¬
189900970526                   -        (S/N)=N                        funz.02¬
190000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
190100970526                   -        (S/N)=N                        funz.04¬
190200060927F14=Cal.Eventi Part-        (S/N)=S  <== S-FISSO           funz.05¬
190300970526F06=Inserimento    -        (S/N)=N                        funz.06¬
190400970526                   -        (S/N)=N                        funz.07¬
190500970626F08=Località       -        (S/N)=N                        funz.08¬
190600970626F09=Eccezioni      -        (S/N)=N                        funz.09¬
190700970626F10=Tempi consegna -        (S/N)=N                        funz.10¬
190800970526                   -        (S/N)=N                        funz.11¬
190900060927F15=CapUtilizziPart-        (S/N)=S  <== S-FISSO           funz.12¬
191000970526                   -        (S/N)=N                        funz.13¬
191100060927F05=Rivisualizza   -        (S/N)=S                        funz.14¬
191200060927F12=Ritorno        -        (S/N)=S                        funz.15¬
191300970526                   -        (S/N)=N                        funz.16¬
191400970526                   -        (S/N)=N                        funz.17¬
191500970526                   -        (S/N)=N                        funz.18¬
191600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
191700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
191800970528F21=Stampa cappario-        (S/N)=S                        funz.21¬
191900970528F22=Gestione stampe-        (S/N)=S                        funz.22¬
192000970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
192100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
