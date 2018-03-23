000100970526     H*------------------------------------------------------------------------*
000200030821     H* Gestione - regole invii spool via e-mail
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800100219     FTRBMM5D   CF   E             WORKSTN INFDS(DEVDS1)
000900030821     F                                     SFILE(TCM5D05:SFLULT)
001000030821     F                                     SFILE(TCM5DMS:MSGULT)
001100970526     F*------------
001200040929     Ftimar01l  UF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* DESCRIZIONI OPZIONI
002600970526     D*------------
002700970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* OPZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D OPV             S             19    DIM(24)
003600970526     D*------------
003700970526     D* FUNZIONI DA CARICARE A VIDEO
003800970526     D*------------
003900970527     D FUV             S             19    DIM(24)
004000970526     D*------------------------------------------------------------------------*
004100970526     D* INPUT
004200970526     D*------------------------------------------------------------------------*
004300970526     D*------------
004400970526     D* RIDEFINIZIONE OPZIONI VIDEO
004500970526     D*------------
004600970526     D OPDDS           DS
004700970526     D  OPDOPZ                 1      2
004800970526     D  OPDDE1                 3     19
004900970526     D  OPDOP1                21     22
005000970526     D  OPDFI0                23     34
005100970526     D  OPDATT                35     35
005200970526     D  OPDFI1                36     80
005300970526     D  OPDDES                 1     19
005400970526     D  OPDRIG                 1     80
005500970526     D*------------
005600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005700970526     D*------------
005800970526     D FUDDS           DS
005900970526     D  FUDDE1                 1      1
006000970526     D  FUDFUN                 2      3
006100970526     D  FUDDE2                 4     19
006200970526     D  FUDFI0                20     34
006300970526     D  FUDATT                35     35
006400970526     D  FUDFI1                36     80
006500970526     D  FUDDES                 1     19
006600970526     D  FUDRIG                 1     80
006700970526     D*------------
006800970526     D* DS GESTIONE TASTI FUNZIONALI
006900970526     D*------------
007000970526     D DSKEY         E DS
007100970526     D DEVDS1          DS
007200970526     D  KEY                  369    369
007300970526     D*------------
007400970526     D* DS "XMSG" - GESTIONE MESSAGGI
007500970526     D*------------
007600970526     D DSMSG         E DS
007700970526     D*------------
007800030821     D* DS REPERIMENTO DATI UTENTE
007900970526     D*------------
008000020916     D TIBS34DS      E DS                                                       *Profili utente
008100020916     D DDATIUTE      E DS                                                       *Dati utente
008200020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008300970527     D*------------------
008400970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
008500970527     D*------------------
008600970527     D WLBDA8          DS                  INZ
008700970527     D  G08DAT                 1      8  0
008800970527     D  G08INV                 9     16  0
008900970527     D  G08ERR                17     17
009000970527     D  G08TGI                18     22  0
009100970526     D*------------
009200970526     D* ARCHITETTURA
009300970526     D*------------
009400970526     D KPJBA         E DS
009500030822     D*------------------
009600030822     D* PASSAGGIO PARAMETRI
009700030822     D*------------------
009800030822     D PARAM           DS                  INZ
009900030822     D  POP0                   1      3
010000030822     D  POP1                   4      6
010100030822     D  PCODR                  7      9
010200030822     D  PMSG                  10     89
010300970526     C*------------------------------------------------------------------------*
010400970526     C* MAIN LINES
010500970526     C*------------------------------------------------------------------------*
010600970526     C*
010700970526     C* SE NON CI SONO ERRORI, PROSEGUE
010800970526IF  1C     ERRL00        IFNE      '1'
010900970526     C*
011000970526     C* CARICA IL SFILE
011100970526     C                   EXSR      POSSFL
011200970526     C*
011300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011400970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011700970526     C*
011800970526     C* REIMPOSTA ERRORE DI PROCERURA
011900970526     C                   MOVEL     '0'           ERRL00
012000970526     C*
012100970526     C* EMETTE IL SFILE
012200970526     C                   EXSR      OUTSFL
012300970526     C*
012400970526     C* GESTIONE TASTI FUNZIONALI
012500970526     C                   EXSR      FUNCON
012600970526     C*
012700970526E   2C                   ENDDO
012800970527     C*
012900970527     C* CI SONO ERRORI IN ENTRATA
013000970527X   1C                   ELSE
013100030822     C                   MOVEL     DSMSMS        PMSG
013200970526E   1C                   ENDIF
013300970526     C* FINE PGM
013400970526     C                   SETON                                        LR
013500970526     C*------------------------------------------------------------------------*
013600970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013700970526     C*------------------------------------------------------------------------*
013800970526     C     F03GES        BEGSR
013900970526     C*
014000970526     C* IMPOSTA "PREMUTO F03"
014100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014200970526     C*
014300970526     C                   ENDSR
014400970526     C*------------------------------------------------------------------------*
014500970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014600970526     C*------------------------------------------------------------------------*
014700970526     C     F05GES        BEGSR
014800970526     C*
014900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015000970526     C*
015100970526     C                   ENDSR
015200970526     C*------------------------------------------------------------------------*
015300970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015400970526     C*------------------------------------------------------------------------*
015500970526     C     F06GES        BEGSR
015600970526     C*
015700970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
015800020916IF  1C     DUTLPO        IFNE      'S'
015900970526     C                   MOVEL     '1'           ERRL00
016000970526     C                   MOVEL     MSG(3)        DSMSMS
016100970526E   1C                   ENDIF
016200970526     C*
016300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016400030821     C                   MOVEL     *BLANKS       DCODR
016500970602     C                   MOVEL     DEPOP0        WOP0
016600970529     C                   MOVEL     'F06'         WOP1
016700100219     C                   MOVEL     'TRBMM6R'     WPGM              8
016800970526     C                   EXSR      LANPGM                                       *LANCIA PGM
016900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017000970526E   1C                   ENDIF
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F12GES - GESTIONE F12 -> RITORNO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F12GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F12"
017900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018000970526     C*
018100970526     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F23GES        BEGSR
018600970526     C*
018700970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
018800970526     C*
018900970526     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
019200970526     C*------------------------------------------------------------------------*
019300970526     C     F24GES        BEGSR
019400970526     C*
019500970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
019600970526     C*
019700970526     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
020000970526     C*------------------------------------------------------------------------*
020100970526     C     RUPGES        BEGSR
020200970526     C*
020300970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* ENTGES - GESTIONE ENTER -> INVIO
020800970526     C*------------------------------------------------------------------------*
020900970526     C     ENTGES        BEGSR
021000970526     C*
021100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
021200970526     C                   EXSR      NEWPOS
021300970526     C*
021400970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
021500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021600970526IF  2C     SWPOSI        IFNE      0
021700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021800970526X   2C                   ELSE
021900970526IF  3C     SFLULT        IFNE      0
022000970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
022100970526     C*
022200970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
022300970526IF  4C     SWMODI        IFNE      0
022400970526     C                   EXSR      POSSFL
022500970526E   4C                   ENDIF
022600970526E   3C                   ENDIF
022700970526E   2C                   ENDIF
022800970526E   1C                   ENDIF
022900970526     C*
023000970526     C                   ENDSR
023100030822     C*------------------------------------------------------------------------*
023200030822     C* O01GES - GESTIONE OPZIONE 1=SCELTA
023300030822     C*------------------------------------------------------------------------*
023400030822     C     O01GES        BEGSR
023500040929     C*
023600030822     C* SCELTA
023700030822     C                   MOVEL     VIDCODR       PCODR
023800030822     C                   MOVEL     PARAM         KPJBU
023900030822     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
024000030822     C*
024100030822     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
024400970526     C*------------------------------------------------------------------------*
024500970526     C     O02GES        BEGSR
024600040929     C*
024700970527     C* MODIFICA
024800970526IF  1C     ERRL00        IFEQ      '0'
024900030821     C                   MOVEL     VIDCODR       DCODR
025000970602     C                   MOVEL     DEPOP0        WOP0
025100970526     C                   MOVEL     'O02'         WOP1
025200100219     C                   MOVEL     'TRBMM6R'     WPGM
025300970526     C                   EXSR      LANPGM
025400030821     C                   Z-ADD     1             SWMODI
025500970526E   1C                   ENDIF
025600970526     C*
025700970526     C                   ENDSR
025800991014     C*------------------------------------------------------------------------*
025900030821     C* O03GES - GESTIONE OPZIONE 3=COPIA
026000991014     C*------------------------------------------------------------------------*
026100991014     C     O03GES        BEGSR
026200991014     C*
026300030821     C* COPIA
026400991014IF  1C     ERRL00        IFEQ      '0'
026500030821     C                   MOVEL     VIDCODR       DCODR
026600991014     C                   MOVEL     DEPOP0        WOP0
026700991014     C                   MOVEL     'O03'         WOP1
026800100219     C                   MOVEL     'TRBMM6R'     WPGM
026900991014     C                   EXSR      LANPGM
027000030821     C                   Z-ADD     1             SWMODI
027100991014E   1C                   ENDIF
027200991014     C*
027300991014     C                   ENDSR
027400040929     C*------------------------------------------------------------------------*
027500040929     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
027600040929     C*------------------------------------------------------------------------*
027700040929     C     O04GES        BEGSR
027800040929     C*
027900040929     C* MODIFICA
028000040929IF  1C     ERRL00        IFEQ      '0'
028100040929     C     VIDCODR       CHAIN     timar01l
028200040929     C                   IF        %found(timar01l)
028300040929     C                   EVAL      MARATB = 'A'
028400040929     C                   UPDATE    timar000
028500040929     C                   Z-ADD     1             SWMODI
028600040929     C                   ENDIF
028700040929E   1C                   ENDIF
028800040929     C*
028900040929     C                   ENDSR
029000970526     C*------------------------------------------------------------------------*
029100970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
029200970526     C*------------------------------------------------------------------------*
029300970526     C     O05GES        BEGSR
029400970526     C*
029500030821     C                   MOVEL     VIDCODR       DCODR
029600970602     C                   MOVEL     DEPOP0        WOP0
029700970526     C                   MOVEL     'O05'         WOP1
029800100219     C                   MOVEL     'TRBMM6R'     WPGM
029900970526     C                   EXSR      LANPGM
030000970526     C*
030100970526     C                   ENDSR
030200040930     C*------------------------------------------------------------------------*
030300040930     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
030400040930     C*------------------------------------------------------------------------*
030500040930     C     O07GES        BEGSR
030600040930     C*
030700040930     C* RIPRISTINO
030800040930IF  1C     ERRL00        IFEQ      '0'
030900040930     C     VIDCODR       CHAIN     timar01l
031000040930     C                   IF        %found(timar01l)
031100040930     C                   EVAL      MARATB = *blanks
031200040930     C                   UPDATE    timar000
031300040930     C                   Z-ADD     1             SWMODI
031400040930     C                   ENDIF
031500040930E   1C                   ENDIF
031600040930     C*
031700040930     C                   ENDSR
031800970526     C*------------------------------------------------------------------------*
031900970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
032000970526     C*------------------------------------------------------------------------*
032100970526     C     NEWPOS        BEGSR
032200970526     C*
032300970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
032400970526     C*         1= RICHIESTO POSIZIONAMENTO
032500970526     C                   Z-ADD     0             SWPOSI            7 0
032600970526     C*
032700970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
032800970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
032900970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
033000970526     C                   Z-ADD     0             SWMODI            1 0
033100970530     C*
033200970530     C* REIMPOSTA INDICATORI DI ERRORE
033300970530     C                   SETOFF                                       808182
033400970530     C                   SETOFF                                       83
033500970526     C*
033600970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
033700030821IF  1C     VIDcodr1      IFNE      DEPcodr1
033800970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
033900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
034000970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
034100030821     C                   MOVEL     VIDcodr1      DEPcodr1                       *DEPOSITI
034200970526E   2C                   ENDIF
034300970526E   1C                   ENDIF
034400970526     C*
034500970526     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
034800970526     C*------------------------------------------------------------------------*
034900970528     C     SETFIL        BEGSR
035000970526     C*
035100970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
035200030821     C                   MOVEL     VIDcodr1      KMARcodr
035300030821IF  1C     WTIPRI        IFEQ      'COD'
035400030821     C     KEYmar        SETLL     timar01l                           99
035500970528E   1C                   ENDIF
035600980107     C*
035700970526     C* LETTURA RECORD
035800970526IF  1C     *IN99         IFEQ      *OFF
035900970526     C                   EXSR      LETFIL
036000970526E   1C                   ENDIF
036100970526     C*
036200970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
036300970526IF  1C     *IN99         IFEQ      *ON
036400970526     C                   Z-ADD     1             SFLEOF
036500970526E   1C                   ENDIF
036600970526     C*
036700970526     C                   ENDSR
036800970526     C*------------------------------------------------------------------------*
036900970526     C* LETFIL - LETTURA RECORD
037000970526     C*------------------------------------------------------------------------*
037100970526     C     LETFIL        BEGSR
037200970526     C*
037300970526     C* LEGGE FINO A:
037400970526     C*     - FINE FILE
037500970526     C*     - TROVATO RECORD VALIDO
037600970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
037700970526DO  1C     *IN99         DOWEQ     *OFF
037800970526     C     WRECOK        ANDEQ     'N'
037900970526     C*
038000970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
038100030821IF  2C     WTIPRI        IFEQ      'COD'
038200040929     C                   READ(n)   timar01l                               99
038300970528E   2C                   ENDIF
038400970528     C*
038500970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
038600970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
038700970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
038800970530X   2C                   ELSE
038900970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
039000970530E   2C                   ENDIF
039100970526E   1C                   ENDDO
039200970526     C*
039300970526     C                   ENDSR
039400970526     C*------------------------------------------------------------------------*
039500030821     C* CHKREC - CONTROLLA VALIDITA' RECORD
039600970526     C*------------------------------------------------------------------------*
039700970526     C     CHKREC        BEGSR
039800970526     C*
039900970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
040000000731     C*
040100010117     C     WRECOK        IFEQ      'S'
040200030821     C     VIDcodr1      IFEQ      *blanks
040300030821     C                   ELSE
040400030821     C                   IF        %subst(VIDcodr1:1:1)='*'
040500030822     C                   IF        %subst(VIDcodr1:2:1)<>'*'
040600030822     C                   IF        %subst(VIDcodr1:2:1)=%subst(MARcodr:2:1)
040700030822     C                   ELSE
040800030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
040900030822     C                   ENDIF
041000030822     C                   ENDIF
041100030822     C                   IF        %subst(VIDcodr1:3:1)<>'*'
041200030822     C                   IF        %subst(VIDcodr1:3:1)=%subst(MARcodr:3:1)
041300030822     C                   ELSE
041400030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
041500030822     C                   ENDIF
041600030822     C                   ENDIF
041700030821     C                   ELSE
041800030821     C                   IF        %subst(VIDcodr1:2:1)='*'
041900030822     C                   IF        %subst(VIDcodr1:1:1)<>'*'
042000030822     C                   IF        %subst(VIDcodr1:1:1)=%subst(MARcodr:1:1)
042100030822     C                   ELSE
042200030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042300030822     C                   ENDIF
042400030822     C                   ENDIF
042500030822     C                   IF        %subst(VIDcodr1:3:1)<>'*'
042600030822     C                   IF        %subst(VIDcodr1:3:1)=%subst(MARcodr:3:1)
042700030822     C                   ELSE
042800030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042900030822     C                   ENDIF
043000030822     C                   ENDIF
043100030821     C                   ELSE
043200030821     C                   IF        %subst(VIDcodr1:3:1)='*'
043300030822     C                   IF        %subst(VIDcodr1:1:1)<>'*'
043400030822     C                   IF        %subst(VIDcodr1:1:1)=%subst(MARcodr:1:1)
043500030822     C                   ELSE
043600030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043700030822     C                   ENDIF
043800030822     C                   ENDIF
043900030822     C                   IF        %subst(VIDcodr1:2:1)<>'*'
044000030822     C                   IF        %subst(VIDcodr1:2:1)=%subst(MARcodr:2:1)
044100030822     C                   ELSE
044200030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
044300030822     C                   ENDIF
044400030822     C                   ENDIF
044500030821     C                   ELSE
044600030821     C                   IF        VIDcodr1 <> MARcodr
044700030821     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
044800030821     C                   ENDIF
044900030821     C                   ENDIF
045000030821     C                   ENDIF
045100030821     C                   ENDIF
045200030821     C                   ENDIF
045300010117     C                   ENDIF
045400000731     C*
045500970526     C                   ENDSR
045600970526     C*------------------------------------------------------------------------*
045700970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
045800970526     C*------------------------------------------------------------------------*
045900970526     C     IRESFL        BEGSR
046000970526     C*
046100970526     C                   MOVEL     *BLANKS       OPZKEY
046200030821     C                   MOVEL     MARatb        VIDatb
046300030821     C                   MOVEL     MARcodr       VIDcodr
046400030821     C                   MOVEL     MARcodd       VIDcodd
046500030821     C*
046600970526     C                   ENDSR
046700970526     C*------------------------------------------------------------------------*
046800970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
046900970526     C*------------------------------------------------------------------------*
047000970526     C     CHKPAR        BEGSR
047100970528     C*
047200030822     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CODICE
047300030821     C                   MOVEL     'COD'         WTIPRI            3
047400010117     C*
047500970526     C                   ENDSR
047600970526     C*------------------------------------------------------------------------*
047700970526     C* LANPGM - LANCIA UN PROGRAMMA
047800970526     C*------------------------------------------------------------------------*
047900970526     C     LANPGM        BEGSR
048000030822     C*
048100030822     C* IMPOSTA IL PASSAGGIO PARAMETRI
048200030822     C                   CLEAR                   PARAM
048300030822     C                   MOVEL     WOP1          POP1                           *LIVELLO PGM
048400030822     C                   MOVEL     DCODR         PCODR                          *CODICE REGOLA
048500970526     C*
048600030822     C                   MOVEL     PARAM         KPJBU
048700970526     C                   CALL      WPGM
048800970612     C                   PARM                    KPJBA
048900030822     C                   MOVEL     KPJBU         PARAM
049000030822     C                   MOVEL     PMSG          DSMSMS
049100970526     C*
049200970526     C                   ENDSR
049300970526     C*------------------------------------------------------------------------*
049400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
049500970526     C*------------------------------------------------------------------------*
049600970526     C     CHKFOP        BEGSR
049700970626     C*
049800970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
049900970626IF  1C     DEPOP0        IFEQ      'P01'
050000970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
050100970626     C                   MOVEL     'S'           OPDATT
050200970626     C                   MOVEL     OPDRIG        OPD(1)
050300970626X   1C                   ELSE
050400970626     C* OPZIONI
050500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
050600970626     C                   MOVEL     'S'           OPDATT
050700970626     C                   MOVEL     OPDRIG        OPD(5)
050800970626     C* FUNZIONI
050900970626E   1C                   ENDIF
051000970626     C*
051100970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
051200970626IF  1C     DEPOP0        IFEQ      'P02'
051300970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
051400970626     C                   MOVEL     'S'           OPDATT
051500970626     C                   MOVEL     OPDRIG        OPD(2)
051600030821     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
051700991014     C                   MOVEL     'S'           OPDATT
051800991014     C                   MOVEL     OPDRIG        OPD(3)
051900040929     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
052000040929     C                   MOVEL     'S'           OPDATT
052100040929     C                   MOVEL     OPDRIG        OPD(4)
052200040930     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
052300040930     C                   MOVEL     'S'           OPDATT
052400040930     C                   MOVEL     OPDRIG        OPD(7)
052500970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
052600970626     C                   MOVEL     'S'           FUDATT
052700970626     C                   MOVEL     FUDRIG        FUD(6)
052800970626E   1C                   ENDIF
052900970626     C*
053000970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
053100020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
053200030821     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
053300030821     C                   MOVEL     'N'           OPDATT
053400030821     C                   MOVEL     OPDRIG        OPD(4)
053500970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
053600970626     C                   MOVEL     'N'           OPDATT
053700970626     C                   MOVEL     OPDRIG        OPD(7)
053800970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
053900970626     C                   MOVEL     'N'           FUDATT
054000970626     C                   MOVEL     FUDRIG        FUD(6)
054100970626E   1C                   ENDIF
054200970526     C*
054300970526     C* GESTIONE OPZIONI A VIDEO
054400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
054500970526     C                   EXSR      OPZGES
054600970526     C*
054700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
054800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
054900970526     C                   EXSR      FUNGES
055000970526     C*
055100970526     C                   ENDSR
055200020916     C*--------------------------------------------------------------------------------------------*
055300020916     C* REPERISCE I DATI UTENTE
055400020916     C*--------------------------------------------------------------------------------------------*
055500020916     C     REPDATIUTE    BEGSR
055600020916     C*
055700020916     C* INIZIALIZZA VARIABILI DI WRK
055800020916     C                   CLEAR                   TIBS34DS
055900020916     C                   CLEAR                   AZUTEDS
056000020916     C                   CLEAR                   DDATIUTE
056100020916     C*
056200020916     C     *DTAARA       DEFINE    §azute        azuteds
056300020916     C     *DTAARA       DEFINE    §datiute      ddatiute
056400020916     C                   IN(E)     *DTAARA
056500030821     C                   IF        %Error
056600030821     C                   EVAL      I34Tla = 'L'
056700020916     C                   CALL      'TIBS34R'
056800020916     C                   PARM                    Tibs34Ds
056900020916     C                   IN        *DTAARA
057000030821     C                   ENDIF
057100020916     C*
057200020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
057300020916     C                   MOVEL(P)  RSUT          DSFIRS           20
057400020916     C*
057500020916     C                   ENDSR
057600970526     C*------------------------------------------------------------------------*
057700970526     C* *INZSR - OPERAZIONI INIZIALI
057800970526     C*------------------------------------------------------------------------*
057900970526     C     *INZSR        BEGSR
058000970526     C*---
058100970526     C* RICEVIMENTO PARAMETRI
058200970526     C*---
058300970526     C     *ENTRY        PLIST
058400970612     C                   PARM                    KPJBA
058500030821     C*
058600030822     C                   MOVEL     KPJBU         PARAM
058700970526     C*
058800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
058900970526     C     XMSGPA        PLIST
059000970526     C                   PARM                    DSMSMS
059100970526     C                   PARM                    DSMSPG
059200970526     C                   PARM                    DSMSKE
059300970526     C                   PARM                    DSMSER
059400970526     C*
059500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
059600970526     C                   CALL      'XKEY'
059700970526     C                   PARM                    DSKEY
059800970526     C*---
059900970526     C* VARIABILI NON RIFERITE AL DATA BASE
060000970526     C*---
060100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
060200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
060300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
060400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
060500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
060600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
060700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
060800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
060900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
061000030822     C     *LIKE         DEFINE    POP0          DEPOP0                         *DI DEPOSITO
061100030822     C     *LIKE         DEFINE    POP1          DEPOP1
061200030822     C     *LIKE         DEFINE    POP0          WOP0                           *DI LAVORO
061300030822     C     *LIKE         DEFINE    POP1          WOP1
061400030822     C     *LIKE         DEFINE    PCODR         DCODR
061500970526     C*---
061600030821     C     *LIKE         DEFINE    VIDcodr1      DEPcodr1
061700030821     C*---
061800970526     C* VARIABILI DI CONTROLLO
061900970526     C*---
062000970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
062100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
062200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
062300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
062400970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
062500040929     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
062600970526     C*
062700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
062800100219     C                   MOVEL     'TRBMM5R'     DSMSPG
062900100219     C                   MOVEL     'TRBMM5R'     MSGQUE
063000970526     C                   MOVEL     *BLANKS       MSGKEY
063100970526     C                   MOVEL     *BLANKS       DSMSMS
063200970526     C                   MOVEL     *BLANKS       DSMSKE
063300970526     C                   Z-ADD     *ZEROS        DSMSER
063400970526     C*---
063500970526     C* VARIABILI RIFERITE AL DATA BASE
063600970526     C*---
063700030821     C     *LIKE         DEFINE    MARcodr       KMARcodr
063800970526     C*---
063900970526     C* CHIAVI DI LETTURA
064000970526     C*---
064100030821     C* LETTURA timar01l - COMPLETA
064200030821     C     KEYmar        KLIST
064300030821     C                   KFLD                    KMARcodr                       *CODICE REGOLA
064400970527     C*
064500970527     C* REPERIMENTO DATI UTENTE
064600020916     C                   EXSR      REPDATIUTE
064700970526     C*
064800970526     C* DEPOSITA LE OPZIONI DI ENTRATA
064900030822     C                   MOVEL     POP0          DEPOP0                         *LIVELLO PROCEDURA
065000030822     C                   MOVEL     POP1          DEPOP1                         *LIVELLO PROGRAMMA
065100970526     C*
065200970526     C* CALCOLA LA DATA CORRENTE
065300970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
065400970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
065500970526     C                   Z-ADD     WN8           G08DAT
065600970526     C                   Z-ADD     *ZEROS        G08INV
065700970526     C                   MOVEL     '0'           G08ERR
065800970526     C                   CALL      'XSRDA8'
065900970526     C                   PARM                    WLBDA8
066000970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
066100970526     C*
066200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
066300030821     C                   MOVEL     *blanks       VIDcodr1
066400970526     C                   EXSR      CHKPAR
066500970526IF  1C     ERRL00        IFEQ      '1'
066600030822     C                   MOVEL     DSMSMS        PMSG
066700970526X   1C                   ELSE
066800030821     C                   MOVEL     VIDcodr1      DEPcodr1                       *DEPOSITI
066900970526E   1C                   ENDIF
067000970526     C*
067100970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
067200970526     C                   EXSR      CHKFOP
067300970526     C*
067400970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
067500970530IF  1C     DEPOP0        IFEQ      'P01'
067600970530     C                   MOVEL     DEC(3)        VIDING
067700970530E   1C                   ENDIF
067800970530IF  1C     DEPOP0        IFEQ      'P02'
067900970526     C                   MOVEL     DEC(1)        VIDING
068000970530E   1C                   ENDIF
068100970530IF  1C     DEPOP0        IFEQ      'P05'
068200970526     C                   MOVEL     DEC(2)        VIDING
068300970526E   1C                   ENDIF
068400970526     C*
068500970526     C                   ENDSR
068600970526     C*------------------------------------------------------------------------*
068700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
068800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
068900970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
069000970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
069100970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
069200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
069300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
069400970526     C*------------------------------------------------------------------------*
069500970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
069600970526     C*------------------------------------------------------------------------*
069700970526     C     POSSFL        BEGSR
069800970526     C*
069900970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
070000970526     C                   EXSR      CLRSFL
070100970526     C*
070200970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
070300970526     C                   EXSR      SETFIL
070400970526     C*
070500970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
070600970526     C     SFLEOF        IFEQ      0
070700970526     C                   EXSR      CARSFL
070800970526     C                   END
070900970526     C*
071000970526     C                   ENDSR
071100970526     C*------------------------------------------------------------------------*
071200970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
071300970526     C*------------------------------------------------------------------------*
071400970526     C     ENTATT        BEGSR
071500970526     C*
071600970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
071700970526     C                   Z-ADD     0             OPZOK             1 0
071800970526     C*
071900970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
072000970526     C                   Z-ADD     0             SFLVAR            5 0
072100970526     C*
072200970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
072300970526     C                   Z-ADD     0             SFLERR
072400970526     C*
072500970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
072600970526     C                   Z-ADD     SFLULT        WFLULT
072700970526     C*
072800970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
072900030821     C                   READC     TCM5D05                                98
073000970526     C     *IN98         DOWEQ     '0'
073100970526     C*
073200970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
073300970526     C                   ADD       1             SFLVAR
073400970526     C*
073500970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
073600970526     C     OPZOK         IFEQ      0
073700970526     C*
073800970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
073900970526     C     OPZKEY        IFNE      *BLANKS
074000970526     C                   EXSR      OPZCON
074100970526     C                   END
074200970526     C*
074300970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
074400970526     C*            2. PULIZIA SCELTA IN SFL
074500970526     C     OPZOK         IFEQ      0
074600970526     C                   MOVEL     '0'           *IN41
074700970526     C                   MOVEL     *BLANKS       OPZKEY
074800970526     C                   ELSE
074900970526     C*
075000970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
075100970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
075200970526     C                   MOVEL     '1'           *IN41
075300970526     C                   Z-ADD     SFLULT        SFLERR
075400970526     C                   END
075500970526     C                   ELSE
075600970526     C*
075700970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
075800970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
075900970526     C                   MOVEL     '1'           *IN41
076000970526     C                   END                                                    OPZOK=0
076100970526     C*
076200970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
076300970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
076400970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
076500970526     C     OPZOK         IFNE      0
076600970526     C                   Z-ADD     SFLERR        SFLNBR
076700970526     C                   ELSE
076800970526     C                   Z-ADD     SFLULT        SFLNBR
076900970526     C                   END
077000970526     C*
077100970526     C* AGGIORNO RECORD SFL
077200030821     C                   UPDATE    TCM5D05
077300970526     C*
077400030821     C                   READC     TCM5D05                                98
077500970526     C                   END                                                    DO WHILE
077600970526     C*
077700970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
077800970526     C                   Z-ADD     WFLULT        SFLULT
077900970526     C*
078000970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
078100970526     C     SFLVAR        IFEQ      0
078200970526     C*---                 MOVEL'1'       FINL00
078300970526     C                   END
078400970526     C*
078500970526     C                   ENDSR
078600970526     C*------------------------------------------------------------------------*
078700970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
078800970526     C*------------------------------------------------------------------------*
078900970526     C     CLRSFL        BEGSR
079000970526     C*
079100970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
079200970526     C* (0=NON FINE FILE  1=FINE FILE)
079300970526     C                   Z-ADD     0             SFLEOF
079400970526     C*
079500970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
079600970526     C                   Z-ADD     0             SFLULT
079700970526     C*
079800970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
079900970526     C                   MOVEL     '1'           *IN40
080000030821     C                   WRITE     TCM5D06
080100970526     C                   MOVEL     '0'           *IN40
080200970526     C*
080300970526     C                   ENDSR
080400970526     C*------------------------------------------------------------------------*
080500970526     C* CARSFL - CARICAMENTO SFL
080600970526     C*------------------------------------------------------------------------*
080700970526     C     CARSFL        BEGSR
080800970526     C*
080900970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
081000970526     C     SFLULT        IFEQ      0
081100970526     C                   EXSR      SETFIL
081200970526     C                   END
081300970526     C*
081400970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
081500970526     C                   Z-ADD     SFLULT        WFLULT
081600970526     C*
081700970526     C* CICLO:
081800970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
081900970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
082000970526     C                   Z-ADD     1             SFLRIG
082100970526     C     SFLRIG        DOWLE     SFLPAG
082200970526     C     SFLEOF        ANDEQ     0
082300970526     C*
082400970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
082500970526     C                   MOVEL     *BLANKS       OPZKEY
082600970526     C*
082700970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
082800970526     C                   EXSR      IRESFL
082900970526     C*
083000970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
083100970526     C                   ADD       1             SFLULT
083200970526     C*
083300970526     C* SCRITTURA RECORD SFL
083400030821     C                   WRITE     TCM5D05
083500970526     C                   ADD       1             SFLRIG
083600970526     C*
083700970526     C* LETTURA FILE DA CARICARE
083800970526     C                   EXSR      LETFIL
083900970526     C                   END                                                    DO WHILE
084000970526     C*
084100970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
084200970526     C     WFLULT        IFNE      SFLULT
084300970526     C                   Z-ADD     WFLULT        SFLNBR
084400970526     C                   ADD       1             SFLNBR
084500970526     C                   END
084600970526     C*
084700970526     C                   ENDSR
084800970526     C*------------------------------------------------------------------------*
084900970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
085000970526     C*------------------------------------------------------------------------*
085100970526     C     OUTSFL        BEGSR
085200970526     C*
085300970526     C* WRITE RECORD TESTATA VIDEO
085400030821     C                   WRITE     TCM5D01
085500970526     C*
085600970526     C* VISUALIZZAZIONE MESSAGGIO
085700970526     C                   EXSR      SNDMSG
085800970526     C*
085900970526     C* WRITE RECORD OPZIONI VIDEO
086000030821     C                   WRITE     TCM5D02
086100970526     C*
086200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
086300030821     C                   WRITE     TCM5D03
086400970526     C*
086500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
086600030821     C                   WRITE     TCM5D04
086700970526     C*
086800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
086900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
087000970526     C     SFLEOF        IFEQ      0
087100970526     C                   MOVEL     '0'           *IN42
087200970526     C                   ELSE
087300970526     C*
087400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
087500970526     C                   MOVEL     '1'           *IN42
087600970526     C                   END
087700970526     C*
087800970526     C* WRITE RECORD TASTI FUNZIONALI
087900030821     C                   WRITE     TCM5D09
088000970526     C*
088100970526     C* CASO DI SFL VUOTO
088200970526     C     SFLULT        IFLE      0
088300970526     C*
088400970526     C*    - WRITE RECORD VUOTO
088500030821     C                   WRITE     TCM5D0A
088600030821     C                   READ      TCM5D0A                                99
088700970526     C*
088800970526     C                   ELSE
088900970526     C*
089000970526     C* CASO DI SFL CON RECORDS
089100970526     C*
089200970526     C*    - WRITE RECORD CONTROLLO SFL
089300030821     C                   WRITE     TCM5D06
089400030821     C                   READ      TCM5D06                                99
089500970526     C                   END
089600970526     C*
089700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
089800030821     C                   READ      TCM5D03                                99
089900970526     C*
090000970526     C                   ENDSR
090100970526     C*------------------------------------------------------------------------*
090200970526     C* SNDMSG - MANDA MESSAGGIO
090300970526     C*------------------------------------------------------------------------*
090400970526     C     SNDMSG        BEGSR
090500970526     C*
090600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
090700970526     C                   MOVEL     '1'           *IN40
090800030821     C                   WRITE     TCM5DMC
090900970526     C                   MOVEL     '0'           *IN40
091000970526     C*
091100970526     C* VISUALIZZAZIONE MESSAGGIO
091200970526     C                   Z-ADD     0             MSGULT
091300970526     C                   CALL      'XMSG'        XMSGPA
091400970526     C     DSMSER        IFEQ      0
091500970526     C                   MOVEL     DSMSKE        MSGKEY
091600970526     C                   END
091700970526     C                   Z-ADD     1             MSGULT
091800030821     C                   WRITE     TCM5DMS                                      SFL MESS
091900030821     C                   WRITE     TCM5DMC                                      CONTR MESS
092000970526     C*
092100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
092200970526     C                   MOVEL     *BLANKS       DSMSMS
092300970526     C*
092400970526     C                   ENDSR
092500970526     C*------------------------------------------------------------------------*
092600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
092700970526     C*------------------------------------------------------------------------*
092800970526     C     FUNCON        BEGSR
092900970526     C*
093000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
093100970526     C                   EXSR      FUNVAR
093200970526     C*
093300970526     C* PULISCO CAMPI DI LAVORO
093400970526     C                   Z-ADD     0             I                 5 0
093500970526     C                   Z-ADD     0             W                 5 0
093600970526     C*
093700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
093800970526     C     FUNKEY        IFNE      *BLANKS
093900970526     C     1             DO        24            I
094000970526     C                   MOVEL     FUD(I)        FUDRIG
094100970526     C*
094200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
094300970526     C     FUNKEY        IFEQ      FUDFUN
094400970526     C     FUDATT        ANDEQ     'S'
094500970526     C                   Z-ADD     I             W
094600970526     C                   END
094700970526     C                   END                                                    DO WHILE
094800970526     C*
094900970526     C* FUNZIONE NON ATTIVA (ERRORE)
095000970526     C     W             IFEQ      0
095100970526     C*
095200970526     C* EMISSIONE MESSAGGIO DI ERRORE
095300970526     C                   MOVEL     MSG(2)        DSMSMS
095400970526     C                   END                                                    W=0
095500970526     C                   END                                                    FUNKEY NE *BLANKS
095600970526     C*
095700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
095800970526     C     W             IFNE      0
095900970526     C     FUNKEY        OREQ      *BLANKS
096000970526     C*
096100970526     C     FUNKEY        IFEQ      *BLANKS
096200970526     C                   EXSR      ENTGES
096300970526     C                   END
096400970526     C     FUNKEY        IFEQ      '03'
096500970526     C                   EXSR      F03GES
096600970526     C                   END
096700970526     C     FUNKEY        IFEQ      '05'
096800970526     C                   EXSR      F05GES
096900970526     C                   END
097000970526     C     FUNKEY        IFEQ      '06'
097100970526     C                   EXSR      F06GES
097200970526     C                   END
097300970526     C     FUNKEY        IFEQ      '12'
097400970526     C                   EXSR      F12GES
097500970526     C                   END
097600970526     C     FUNKEY        IFEQ      '23'
097700970526     C                   EXSR      F23GES
097800970526     C                   END
097900970526     C     FUNKEY        IFEQ      '24'
098000970526     C                   EXSR      F24GES
098100970526     C                   END
098200970526     C     FUNKEY        IFEQ      'RU'
098300970526     C                   EXSR      RUPGES
098400970526     C                   END
098500970526     C*
098600970526     C                   END
098700970526     C*
098800970526     C                   ENDSR
098900970526     C*------------------------------------------------------------------------*
099000970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
099100970526     C*------------------------------------------------------------------------*
099200970526     C     FUNVAR        BEGSR
099300970526     C*
099400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
099500970526     C     KEY           IFEQ      ENTER
099600970526     C                   MOVEL     *BLANKS       FUNKEY
099700970526     C                   END
099800970526     C     KEY           IFEQ      F1
099900970526     C                   MOVEL     '01'          FUNKEY
100000970526     C                   END
100100970526     C     KEY           IFEQ      F2
100200970526     C                   MOVEL     '02'          FUNKEY
100300970526     C                   END
100400970526     C     KEY           IFEQ      F3
100500970526     C                   MOVEL     '03'          FUNKEY
100600970526     C                   END
100700970526     C     KEY           IFEQ      F4
100800970526     C                   MOVEL     '04'          FUNKEY
100900970526     C                   END
101000970526     C     KEY           IFEQ      F5
101100970526     C                   MOVEL     '05'          FUNKEY
101200970526     C                   END
101300970526     C     KEY           IFEQ      F6
101400970526     C                   MOVEL     '06'          FUNKEY
101500970526     C                   END
101600970526     C     KEY           IFEQ      F7
101700970526     C                   MOVEL     '07'          FUNKEY
101800970526     C                   END
101900970526     C     KEY           IFEQ      F8
102000970526     C                   MOVEL     '08'          FUNKEY
102100970526     C                   END
102200970526     C     KEY           IFEQ      F9
102300970526     C                   MOVEL     '09'          FUNKEY
102400970526     C                   END
102500970526     C     KEY           IFEQ      F10
102600970526     C                   MOVEL     '10'          FUNKEY
102700970526     C                   END
102800970526     C     KEY           IFEQ      F11
102900970526     C                   MOVEL     '11'          FUNKEY
103000970526     C                   END
103100970526     C     KEY           IFEQ      F12
103200970526     C                   MOVEL     '12'          FUNKEY
103300970526     C                   END
103400970526     C     KEY           IFEQ      F13
103500970526     C                   MOVEL     '13'          FUNKEY
103600970526     C                   END
103700970526     C     KEY           IFEQ      F14
103800970526     C                   MOVEL     '14'          FUNKEY
103900970526     C                   END
104000970526     C     KEY           IFEQ      F15
104100970526     C                   MOVEL     '15'          FUNKEY
104200970526     C                   END
104300970526     C     KEY           IFEQ      F16
104400970526     C                   MOVEL     '16'          FUNKEY
104500970526     C                   END
104600970526     C     KEY           IFEQ      F17
104700970526     C                   MOVEL     '17'          FUNKEY
104800970526     C                   END
104900970526     C     KEY           IFEQ      F18
105000970526     C                   MOVEL     '18'          FUNKEY
105100970526     C                   END
105200970526     C     KEY           IFEQ      F19
105300970526     C                   MOVEL     '19'          FUNKEY
105400970526     C                   END
105500970526     C     KEY           IFEQ      F20
105600970526     C                   MOVEL     '20'          FUNKEY
105700970526     C                   END
105800970526     C     KEY           IFEQ      F21
105900970526     C                   MOVEL     '21'          FUNKEY
106000970526     C                   END
106100970526     C     KEY           IFEQ      F22
106200970526     C                   MOVEL     '22'          FUNKEY
106300970526     C                   END
106400970526     C     KEY           IFEQ      F23
106500970526     C                   MOVEL     '23'          FUNKEY
106600970526     C                   END
106700970526     C     KEY           IFEQ      F24
106800970526     C                   MOVEL     '24'          FUNKEY
106900970526     C                   END
107000970526     C     KEY           IFEQ      ROLLUP
107100970526     C                   MOVEL     'RU'          FUNKEY
107200970526     C                   END
107300970526     C     KEY           IFEQ      HELP
107400970526     C                   MOVEL     'HP'          FUNKEY
107500970526     C                   END
107600970526     C*
107700970526     C                   ENDSR
107800970526     C*------------------------------------------------------------------------*
107900970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
108000970526     C*------------------------------------------------------------------------*
108100970526     C     FUNGES        BEGSR
108200970526     C*
108300970526     C* PULISCO CAMPI DI LAVORO
108400970526     C                   Z-ADD     0             W                 5 0
108500970526     C*
108600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
108700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
108800970526     C     1             DO        24            I
108900970526     C                   MOVEL     FUD(I)        FUDRIG
109000970526     C*
109100970526     C* PULIZIA FUNZIONI PRECEDENTI
109200970526     C                   MOVEL     *BLANKS       FUV(I)
109300970526     C*
109400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
109500970526     C* (SALTA F23 E F24)
109600970526     C     FUDATT        IFEQ      'S'
109700970526     C     FUDFUN        ANDNE     '23'
109800970526     C     FUDFUN        ANDNE     '24'
109900970526     C                   ADD       1             W
110000970526     C                   MOVEL     FUDDES        FUV(W)
110100970526     C                   END
110200970526     C                   END                                                    DO WHILE
110300970526     C*
110400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
110500970526     C* ALTRIMENTI DISATTIVO F24
110600970526     C     1             DO        24            I
110700970526     C                   MOVEL     FUD(I)        FUDRIG
110800970526     C*
110900970526     C     FUDFUN        IFEQ      '24'
111000970526     C     W             IFGT      6
111100970526     C                   MOVEL     'S'           FUDATT
111200970526     C                   ELSE
111300970526     C                   MOVEL     *BLANKS       FUDATT
111400970526     C                   END                                                    W GT 8
111500970526     C                   MOVEL     FUDRIG        FUD(I)
111600970526     C                   END
111700970526     C                   END                                                    DO WHILE
111800970526     C*
111900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
112000970526     C     W             DIV       6             I
112100970526     C                   MVR                     Z
112200970526     C*
112300970526     C     Z             IFNE      0
112400970526     C     I             ADD       1             Z
112500970526     C                   ELSE
112600970526     C                   Z-ADD     I             Z
112700970526     C                   END
112800970526     C*
112900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
113000970526     C                   ADD       1             FUNGRU
113100970526     C     FUNGRU        IFGT      Z
113200970526     C     FUNGRU        ORLT      1
113300970526     C                   Z-ADD     1             FUNGRU
113400970526     C                   END
113500970526     C*
113600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
113700970526     C     FUNGRU        IFEQ      1
113800970526     C                   MOVEL     FUV(1)        FUN001
113900970526     C                   MOVEL     FUV(2)        FUN002
114000970526     C                   MOVEL     FUV(3)        FUN003
114100970526     C                   MOVEL     FUV(4)        FUN004
114200970526     C                   MOVEL     FUV(5)        FUN005
114300970526     C                   MOVEL     FUV(6)        FUN006
114400970526     C                   END
114500970526     C*
114600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
114700970526     C     FUNGRU        IFEQ      2
114800970526     C                   MOVEL     FUV(7)        FUN001
114900970526     C                   MOVEL     FUV(8)        FUN002
115000970526     C                   MOVEL     FUV(9)        FUN003
115100970526     C                   MOVEL     FUV(10)       FUN004
115200970526     C                   MOVEL     FUV(11)       FUN005
115300970526     C                   MOVEL     FUV(12)       FUN006
115400970526     C                   END
115500970526     C*
115600970526     C* TERZO GRUPPO DI 6 FUNZIONI
115700970526     C     FUNGRU        IFEQ      3
115800970526     C                   MOVEL     FUV(13)       FUN001
115900970526     C                   MOVEL     FUV(14)       FUN002
116000970526     C                   MOVEL     FUV(15)       FUN003
116100970526     C                   MOVEL     FUV(16)       FUN004
116200970526     C                   MOVEL     FUV(17)       FUN005
116300970526     C                   MOVEL     FUV(18)       FUN006
116400970526     C                   END
116500970526     C*
116600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
116700970526     C     FUNGRU        IFEQ      4
116800970526     C                   MOVEL     FUV(19)       FUN001
116900970526     C                   MOVEL     FUV(20)       FUN002
117000970526     C                   MOVEL     FUV(21)       FUN003
117100970526     C                   MOVEL     FUV(22)       FUN004
117200970526     C                   MOVEL     *BLANKS       FUN005
117300970526     C                   MOVEL     *BLANKS       FUN006
117400970526     C                   END
117500970526     C*
117600970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
117700970526     C                   MOVEL     *BLANKS       FUN007
117800970526     C                   MOVEL     *BLANKS       FUN008
117900970526     C*
118000970526     C     1             DO        24            I
118100970526     C                   MOVEL     FUD(I)        FUDRIG
118200970526     C     FUDFUN        IFEQ      '23'
118300970526     C     FUDATT        ANDEQ     'S'
118400970526     C                   MOVEL     FUDDES        FUN007
118500970526     C                   END
118600970526     C     FUDFUN        IFEQ      '24'
118700970526     C     FUDATT        ANDEQ     'S'
118800970526     C                   MOVEL     FUDDES        FUN008
118900970526     C                   END
119000970526     C                   END                                                    DO
119100970526     C*
119200970526     C                   ENDSR
119300970526     C*------------------------------------------------------------------------*
119400970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
119500970526     C*------------------------------------------------------------------------*
119600970526     C     OPZGES        BEGSR
119700970526     C*
119800970526     C* PULISCO CAMPI DI LAVORO
119900970526     C                   Z-ADD     0             W                 5 0
120000970526     C*
120100970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
120200970526     C* E CARICO TABELLA OPZIONI ATTIVE
120300970526     C     1             DO        24            I
120400970526     C                   MOVEL     OPD(I)        OPDRIG
120500970526     C*
120600970526     C* PULIZIA OPZIONI PRECEDENTI
120700970526     C                   MOVEL     *BLANKS       OPV(I)
120800970526     C*
120900970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
121000970526     C     OPDATT        IFEQ      'S'
121100970526     C                   ADD       1             W
121200970526     C                   MOVEL     OPDDES        OPV(W)
121300970526     C                   END
121400970526     C                   END                                                    DO WHILE
121500970526     C*
121600970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
121700970526     C* ALTRIMENTI DISATTIVO F23
121800970526     C     1             DO        24            I
121900970526     C                   MOVEL     FUD(I)        FUDRIG
122000970526     C*
122100970526     C     FUDFUN        IFEQ      '23'
122200970526     C     W             IFGT      8
122300970526     C                   MOVEL     'S'           FUDATT
122400970526     C                   ELSE
122500970526     C                   MOVEL     *BLANKS       FUDATT
122600970526     C                   END                                                    W GT 8
122700970526     C                   MOVEL     FUDRIG        FUD(I)
122800970526     C                   END
122900970526     C                   END                                                    DO WHILE
123000970526     C*
123100970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
123200970526     C     W             DIV       8             I
123300970526     C                   MVR                     Z                 5 0
123400970526     C*
123500970526     C     Z             IFNE      0
123600970526     C     I             ADD       1             Z
123700970526     C                   ELSE
123800970526     C                   Z-ADD     I             Z
123900970526     C                   END
124000970526     C*
124100970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
124200970526     C                   ADD       1             OPZGRU
124300970526     C     OPZGRU        IFGT      Z
124400970526     C     OPZGRU        ORLT      1
124500970526     C                   Z-ADD     1             OPZGRU
124600970526     C                   END
124700970526     C*
124800970526     C* PRIMO GRUPPO DI 8 OPZIONI
124900970526     C     OPZGRU        IFEQ      1
125000970526     C                   MOVEL     OPV(1)        OPZ001
125100970526     C                   MOVEL     OPV(2)        OPZ002
125200970526     C                   MOVEL     OPV(3)        OPZ003
125300970526     C                   MOVEL     OPV(4)        OPZ004
125400970526     C                   MOVEL     OPV(5)        OPZ005
125500970526     C                   MOVEL     OPV(6)        OPZ006
125600970526     C                   MOVEL     OPV(7)        OPZ007
125700970526     C                   MOVEL     OPV(8)        OPZ008
125800970526     C                   END
125900970526     C*
126000970526     C* SECONDO GRUPPO DI 8 OPZIONI
126100970526     C     OPZGRU        IFEQ      2
126200970526     C                   MOVEL     OPV(9)        OPZ001
126300970526     C                   MOVEL     OPV(10)       OPZ002
126400970526     C                   MOVEL     OPV(11)       OPZ003
126500970526     C                   MOVEL     OPV(12)       OPZ004
126600970526     C                   MOVEL     OPV(13)       OPZ005
126700970526     C                   MOVEL     OPV(14)       OPZ006
126800970526     C                   MOVEL     OPV(15)       OPZ007
126900970526     C                   MOVEL     OPV(16)       OPZ008
127000970526     C                   END
127100970526     C*
127200970526     C* TERZO GRUPPO DI 8 OPZIONI
127300970526     C     OPZGRU        IFEQ      3
127400970526     C                   MOVEL     OPV(17)       OPZ001
127500970526     C                   MOVEL     OPV(18)       OPZ002
127600970526     C                   MOVEL     OPV(19)       OPZ003
127700970526     C                   MOVEL     OPV(20)       OPZ004
127800970526     C                   MOVEL     OPV(21)       OPZ005
127900970526     C                   MOVEL     OPV(22)       OPZ006
128000970526     C                   MOVEL     OPV(23)       OPZ007
128100970526     C                   MOVEL     OPV(24)       OPZ008
128200970526     C                   END
128300970526     C*
128400970526     C                   ENDSR
128500970526     C*------------------------------------------------------------------------*
128600970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
128700970526     C*------------------------------------------------------------------------*
128800970526     C     OPZCON        BEGSR
128900970526     C*
129000970526     C* PULISCO CAMPI DI LAVORO
129100970526     C                   Z-ADD     0             I                 5 0
129200970526     C                   Z-ADD     0             W                 5 0
129300970526     C*
129400970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
129500970526     C     1             DO        24            I
129600970526     C                   MOVEL     OPD(I)        OPDRIG
129700970526     C*
129800970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
129900970526     C     OPZKEY        IFEQ      OPDOPZ
130000970526     C     OPZKEY        OREQ      OPDOP1
130100970526     C*
130200970526     C     OPDATT        IFEQ      'S'
130300970526     C                   MOVEL     OPDOPZ        OPZKEY
130400970526     C                   Z-ADD     I             W
130500970526     C                   END
130600970526     C                   END
130700970526     C*
130800970526     C                   END                                                    DO WHILE
130900970526     C*
131000970526     C* OPZIONE NON ATTIVA (ERRORE)
131100970526     C     W             IFEQ      0
131200970526     C*
131300970526     C* OPZIONE NON OK
131400970526     C                   Z-ADD     1             OPZOK
131500970526     C*
131600970526     C* EMISSIONE MESSAGGIO DI ERRORE
131700970526     C                   MOVEL     MSG(1)        DSMSMS
131800970526     C                   ELSE
131900970526     C*
132000970526     C* OPZIONE ATTIVA (PROCEDI ...)
132100970526     C*
132200030822     C     OPZKEY        IFEQ      ' 1'
132300030822     C                   EXSR      O01GES
132400030822     C                   END
132500970526     C     OPZKEY        IFEQ      ' 2'
132600970526     C                   EXSR      O02GES
132700970526     C                   END
132800991014     C     OPZKEY        IFEQ      ' 3'
132900991014     C                   EXSR      O03GES
133000991014     C                   END
133100040929     C     OPZKEY        IFEQ      ' 4'
133200040929     C                   EXSR      O04GES
133300040929     C                   END
133400970526     C     OPZKEY        IFEQ      ' 5'
133500970526     C                   EXSR      O05GES
133600970526     C                   END
133700040930     C     OPZKEY        IFEQ      ' 7'
133800040930     C                   EXSR      O07GES
133900040930     C                   END
134000970526     C                   END                                                    OPZ.NON ATTIVA
134100970526     C*
134200970526     C                   ENDSR
134300970526     C*------------------------------------------------------------------------*
134400970526     C* NOTE COSTRUZIONE SCHIERE
134500970526     C*------------------------------------------------------------------------*
134600970526     C*
134700970526     C* -MSG-MESSAGGI
134800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
134900970526     C* (NON TOCCARE MSG,1 E MSG,2)
135000970526     C*
135100970526     C* -CMD-COMANDI
135200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
135300970526     C*
135400970526     C* -OPD-OPZIONI
135500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
135600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
135700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
135800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
135900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
136000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
136100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
136200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
136300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
136400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
136500970526     C*
136600970526     C* -FUD-FUNZIONI
136700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
136800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
136900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
137000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
137100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
137200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
137300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
137400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
137500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
137600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
137700970526     O*------------------------------------------------------------------------*
137800970526** DEC - DECODIFICHE
137900970526    GESTIONE                                                     1
138000970526INTERROGAZIONE                                                   2
138100970530     SCELTA                                                      3
1382009705264
1383009705265
138400970526** MSG - MESSAGGI
138500100219TRBMM5R- Opzione non gestita                                                   1
138600100219TRBMM5R- Tasto funzionale non gestito                                          2
138700100219TRBMM5R- 3                                                                     3
138800100219TRBMM5R- Record annullato. Consentito solo il ripristino                       4
138900100219TRBMM5R- Record non annullato. Impossibile ripristinare                        5
139000100219TRBMM5R- 6                                                                     6
139100100219TRBMM5R- 7                                                                     7
139200100219TRBMM5R- 8                                                                     8
139300100219TRBMM5R- 9                                                                     9
139400100219TRBMM5R- 10                                                                   10
139500100219TRBMM5R- 11                                                                   11
139600100219TRBMM5R- Data errata                                                          12
139700100219TRBMM5R- 13                                                                   13
139800100219TRBMM5R- 14                                                                   14
139900100219TRBMM5R- 15                                                                   15
140000100219TRBMM5R- 16                                                                   16
140100100219TRBMM5R- 17                                                                   17
140200100219TRBMM5R- 18                                                                   18
140300100219TRBMM5R- 19                                                                   19
140400100219TRBMM5R- 20                                                                   20
140500970526** OPD - OPZIONI
140600030822 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
140700970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
140800991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
140900040929 4=Annulla         -4 -     (S/N)=N                        opz. 04¬
141000970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
141100040930                   -6 -     (S/N)=N                        opz. 06¬
141200040930 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
141300970527                   -8 -     (S/N)=N                        opz. 08¬
141400030821                   -9 -     (S/N)=N                        opz. 09¬
141500970527                   -10-     (S/N)=N                        opz. 10¬
141600970526                   -  -     (S/N)=N                        opz. 11¬
141700970526                   -  -     (S/N)=N                        opz. 12¬
141800970526                   -  -     (S/N)=N                        opz. 13¬
141900970526                   -  -     (S/N)=N                        opz. 14¬
142000970526                   -  -     (S/N)=N                        opz. 15¬
142100970526                   -  -     (S/N)=N                        opz. 16¬
142200970526                   -  -     (S/N)=N                        opz. 17¬
142300970526                   -  -     (S/N)=N                        opz. 18¬
142400970526                   -  -     (S/N)=N                        opz. 19¬
142500970526                   -  -     (S/N)=N                        opz. 20¬
142600970526                   -  -     (S/N)=N                        opz. 21¬
142700970526                   -  -     (S/N)=N                        opz. 22¬
142800970526                   -  -     (S/N)=N                        opz. 23¬
142900970526                   -  -     (S/N)=N                        opz. 24¬
143000970526** FUD - FUNZIONI
143100970526                   -        (S/N)=N                        funz.01¬
143200970526                   -        (S/N)=N                        funz.02¬
143300970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
143400970526                   -        (S/N)=N                        funz.04¬
143500970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
143600970526F06=Inserimento    -        (S/N)=N                        funz.06¬
143700970526                   -        (S/N)=N                        funz.07¬
143800970527                   -        (S/N)=N                        funz.08¬
143900970527                   -        (S/N)=N                        funz.09¬
144000970527                   -        (S/N)=N                        funz.10¬
144100970526                   -        (S/N)=N                        funz.11¬
144200970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
144300970526                   -        (S/N)=N                        funz.13¬
144400970526                   -        (S/N)=N                        funz.14¬
144500970526                   -        (S/N)=N                        funz.15¬
144600970526                   -        (S/N)=N                        funz.16¬
144700970526                   -        (S/N)=N                        funz.17¬
144800970526                   -        (S/N)=N                        funz.18¬
144900970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
145000970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
145100030821                   -        (S/N)=N                        funz.21¬
145200030821                   -        (S/N)=N                        funz.22¬
145300970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
145400970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
