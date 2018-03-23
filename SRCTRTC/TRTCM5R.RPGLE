000100970526     H*------------------------------------------------------------------------*
000200030821     H* Gestione - regole invii spool via e-mail
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030821     FTRTCM5D   CF   E             WORKSTN INFDS(DEVDS1)
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
016700030821     C                   MOVEL     'TRTCM6R'     WPGM              8
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
025200030821     C                   MOVEL     'TRTCM6R'     WPGM
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
026800030821     C                   MOVEL     'TRTCM6R'     WPGM
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
029800030821     C                   MOVEL     'TRTCM6R'     WPGM
029900970526     C                   EXSR      LANPGM
030000970526     C*
030100970526     C                   ENDSR
030101040930     C*------------------------------------------------------------------------*
030102040930     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
030103040930     C*------------------------------------------------------------------------*
030104040930     C     O07GES        BEGSR
030105040930     C*
030106040930     C* RIPRISTINO
030107040930IF  1C     ERRL00        IFEQ      '0'
030108040930     C     VIDCODR       CHAIN     timar01l
030109040930     C                   IF        %found(timar01l)
030110040930     C                   EVAL      MARATB = *blanks
030111040930     C                   UPDATE    timar000
030112040930     C                   Z-ADD     1             SWMODI
030113040930     C                   ENDIF
030114040930E   1C                   ENDIF
030115040930     C*
030116040930     C                   ENDSR
030200970526     C*------------------------------------------------------------------------*
030300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
030400970526     C*------------------------------------------------------------------------*
030500970526     C     NEWPOS        BEGSR
030600970526     C*
030700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
030800970526     C*         1= RICHIESTO POSIZIONAMENTO
030900970526     C                   Z-ADD     0             SWPOSI            7 0
031000970526     C*
031100970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
031200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
031300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
031400970526     C                   Z-ADD     0             SWMODI            1 0
031500970530     C*
031600970530     C* REIMPOSTA INDICATORI DI ERRORE
031700970530     C                   SETOFF                                       808182
031800970530     C                   SETOFF                                       83
031900970526     C*
032000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
032100030821IF  1C     VIDcodr1      IFNE      DEPcodr1
032200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
032300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
032400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
032500030821     C                   MOVEL     VIDcodr1      DEPcodr1                       *DEPOSITI
032600970526E   2C                   ENDIF
032700970526E   1C                   ENDIF
032800970526     C*
032900970526     C                   ENDSR
033000970526     C*------------------------------------------------------------------------*
033100970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
033200970526     C*------------------------------------------------------------------------*
033300970528     C     SETFIL        BEGSR
033400970526     C*
033500970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
033600030821     C                   MOVEL     VIDcodr1      KMARcodr
033700030821IF  1C     WTIPRI        IFEQ      'COD'
033800030821     C     KEYmar        SETLL     timar01l                           99
033900970528E   1C                   ENDIF
034000980107     C*
034100970526     C* LETTURA RECORD
034200970526IF  1C     *IN99         IFEQ      *OFF
034300970526     C                   EXSR      LETFIL
034400970526E   1C                   ENDIF
034500970526     C*
034600970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
034700970526IF  1C     *IN99         IFEQ      *ON
034800970526     C                   Z-ADD     1             SFLEOF
034900970526E   1C                   ENDIF
035000970526     C*
035100970526     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* LETFIL - LETTURA RECORD
035400970526     C*------------------------------------------------------------------------*
035500970526     C     LETFIL        BEGSR
035600970526     C*
035700970526     C* LEGGE FINO A:
035800970526     C*     - FINE FILE
035900970526     C*     - TROVATO RECORD VALIDO
036000970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
036100970526DO  1C     *IN99         DOWEQ     *OFF
036200970526     C     WRECOK        ANDEQ     'N'
036300970526     C*
036400970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
036500030821IF  2C     WTIPRI        IFEQ      'COD'
036600040929     C                   READ(n)   timar01l                               99
036700970528E   2C                   ENDIF
036800970528     C*
036900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
037000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
037100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
037200970530X   2C                   ELSE
037300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
037400970530E   2C                   ENDIF
037500970526E   1C                   ENDDO
037600970526     C*
037700970526     C                   ENDSR
037800970526     C*------------------------------------------------------------------------*
037900030821     C* CHKREC - CONTROLLA VALIDITA' RECORD
038000970526     C*------------------------------------------------------------------------*
038100970526     C     CHKREC        BEGSR
038200970526     C*
038300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
038400000731     C*
038500010117     C     WRECOK        IFEQ      'S'
038600030821     C     VIDcodr1      IFEQ      *blanks
038700030821     C                   ELSE
038800030821     C                   IF        %subst(VIDcodr1:1:1)='*'
038900030822     C                   IF        %subst(VIDcodr1:2:1)<>'*'
039000030822     C                   IF        %subst(VIDcodr1:2:1)=%subst(MARcodr:2:1)
039100030822     C                   ELSE
039200030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
039300030822     C                   ENDIF
039400030822     C                   ENDIF
039500030822     C                   IF        %subst(VIDcodr1:3:1)<>'*'
039600030822     C                   IF        %subst(VIDcodr1:3:1)=%subst(MARcodr:3:1)
039700030822     C                   ELSE
039800030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
039900030822     C                   ENDIF
040000030822     C                   ENDIF
040100030821     C                   ELSE
040200030821     C                   IF        %subst(VIDcodr1:2:1)='*'
040300030822     C                   IF        %subst(VIDcodr1:1:1)<>'*'
040400030822     C                   IF        %subst(VIDcodr1:1:1)=%subst(MARcodr:1:1)
040500030822     C                   ELSE
040600030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
040700030822     C                   ENDIF
040800030822     C                   ENDIF
040900030822     C                   IF        %subst(VIDcodr1:3:1)<>'*'
041000030822     C                   IF        %subst(VIDcodr1:3:1)=%subst(MARcodr:3:1)
041100030822     C                   ELSE
041200030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
041300030822     C                   ENDIF
041400030822     C                   ENDIF
041500030821     C                   ELSE
041600030821     C                   IF        %subst(VIDcodr1:3:1)='*'
041700030822     C                   IF        %subst(VIDcodr1:1:1)<>'*'
041800030822     C                   IF        %subst(VIDcodr1:1:1)=%subst(MARcodr:1:1)
041900030822     C                   ELSE
042000030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042100030822     C                   ENDIF
042200030822     C                   ENDIF
042300030822     C                   IF        %subst(VIDcodr1:2:1)<>'*'
042400030822     C                   IF        %subst(VIDcodr1:2:1)=%subst(MARcodr:2:1)
042500030822     C                   ELSE
042600030822     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
042700030822     C                   ENDIF
042800030822     C                   ENDIF
042900030821     C                   ELSE
043000030821     C                   IF        VIDcodr1 <> MARcodr
043100030821     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043200030821     C                   ENDIF
043300030821     C                   ENDIF
043400030821     C                   ENDIF
043500030821     C                   ENDIF
043600030821     C                   ENDIF
043700010117     C                   ENDIF
043800000731     C*
043900970526     C                   ENDSR
044000970526     C*------------------------------------------------------------------------*
044100970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
044200970526     C*------------------------------------------------------------------------*
044300970526     C     IRESFL        BEGSR
044400970526     C*
044500970526     C                   MOVEL     *BLANKS       OPZKEY
044600030821     C                   MOVEL     MARatb        VIDatb
044700030821     C                   MOVEL     MARcodr       VIDcodr
044800030821     C                   MOVEL     MARcodd       VIDcodd
044900030821     C*
045000970526     C                   ENDSR
045100970526     C*------------------------------------------------------------------------*
045200970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
045300970526     C*------------------------------------------------------------------------*
045400970526     C     CHKPAR        BEGSR
045500970528     C*
045600030822     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CODICE
045700030821     C                   MOVEL     'COD'         WTIPRI            3
045800010117     C*
045900970526     C                   ENDSR
046000970526     C*------------------------------------------------------------------------*
046100970526     C* LANPGM - LANCIA UN PROGRAMMA
046200970526     C*------------------------------------------------------------------------*
046300970526     C     LANPGM        BEGSR
046400030822     C*
046500030822     C* IMPOSTA IL PASSAGGIO PARAMETRI
046600030822     C                   CLEAR                   PARAM
046700030822     C                   MOVEL     WOP1          POP1                           *LIVELLO PGM
046800030822     C                   MOVEL     DCODR         PCODR                          *CODICE REGOLA
046900970526     C*
047000030822     C                   MOVEL     PARAM         KPJBU
047100970526     C                   CALL      WPGM
047200970612     C                   PARM                    KPJBA
047300030822     C                   MOVEL     KPJBU         PARAM
047400030822     C                   MOVEL     PMSG          DSMSMS
047500970526     C*
047600970526     C                   ENDSR
047700970526     C*------------------------------------------------------------------------*
047800970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
047900970526     C*------------------------------------------------------------------------*
048000970526     C     CHKFOP        BEGSR
048100970626     C*
048200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
048300970626IF  1C     DEPOP0        IFEQ      'P01'
048400970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
048500970626     C                   MOVEL     'S'           OPDATT
048600970626     C                   MOVEL     OPDRIG        OPD(1)
048700970626X   1C                   ELSE
048800970626     C* OPZIONI
048900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
049000970626     C                   MOVEL     'S'           OPDATT
049100970626     C                   MOVEL     OPDRIG        OPD(5)
049200970626     C* FUNZIONI
049300970626E   1C                   ENDIF
049400970626     C*
049500970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
049600970626IF  1C     DEPOP0        IFEQ      'P02'
049700970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
049800970626     C                   MOVEL     'S'           OPDATT
049900970626     C                   MOVEL     OPDRIG        OPD(2)
050000030821     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
050100991014     C                   MOVEL     'S'           OPDATT
050200991014     C                   MOVEL     OPDRIG        OPD(3)
050300040929     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
050400040929     C                   MOVEL     'S'           OPDATT
050500040929     C                   MOVEL     OPDRIG        OPD(4)
050501040930     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
050502040930     C                   MOVEL     'S'           OPDATT
050503040930     C                   MOVEL     OPDRIG        OPD(7)
050600970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
050700970626     C                   MOVEL     'S'           FUDATT
050800970626     C                   MOVEL     FUDRIG        FUD(6)
050900970626E   1C                   ENDIF
051000970626     C*
051100970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
051200020916IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
051300030821     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
051400030821     C                   MOVEL     'N'           OPDATT
051500030821     C                   MOVEL     OPDRIG        OPD(4)
051600970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
051700970626     C                   MOVEL     'N'           OPDATT
051800970626     C                   MOVEL     OPDRIG        OPD(7)
051900970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
052000970626     C                   MOVEL     'N'           FUDATT
052100970626     C                   MOVEL     FUDRIG        FUD(6)
052200970626E   1C                   ENDIF
052300970526     C*
052400970526     C* GESTIONE OPZIONI A VIDEO
052500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
052600970526     C                   EXSR      OPZGES
052700970526     C*
052800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
052900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
053000970526     C                   EXSR      FUNGES
053100970526     C*
053200970526     C                   ENDSR
053300020916     C*--------------------------------------------------------------------------------------------*
053400020916     C* REPERISCE I DATI UTENTE
053500020916     C*--------------------------------------------------------------------------------------------*
053600020916     C     REPDATIUTE    BEGSR
053700020916     C*
053800020916     C* INIZIALIZZA VARIABILI DI WRK
053900020916     C                   CLEAR                   TIBS34DS
054000020916     C                   CLEAR                   AZUTEDS
054100020916     C                   CLEAR                   DDATIUTE
054200020916     C*
054300020916     C     *DTAARA       DEFINE    §azute        azuteds
054400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
054500020916     C                   IN(E)     *DTAARA
054600030821     C                   IF        %Error
054700030821     C                   EVAL      I34Tla = 'L'
054800020916     C                   CALL      'TIBS34R'
054900020916     C                   PARM                    Tibs34Ds
055000020916     C                   IN        *DTAARA
055100030821     C                   ENDIF
055200020916     C*
055300020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
055400020916     C                   MOVEL(P)  RSUT          DSFIRS           20
055500020916     C*
055600020916     C                   ENDSR
055700970526     C*------------------------------------------------------------------------*
055800970526     C* *INZSR - OPERAZIONI INIZIALI
055900970526     C*------------------------------------------------------------------------*
056000970526     C     *INZSR        BEGSR
056100970526     C*---
056200970526     C* RICEVIMENTO PARAMETRI
056300970526     C*---
056400970526     C     *ENTRY        PLIST
056500970612     C                   PARM                    KPJBA
056600030821     C*
056700030822     C                   MOVEL     KPJBU         PARAM
056800970526     C*
056900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
057000970526     C     XMSGPA        PLIST
057100970526     C                   PARM                    DSMSMS
057200970526     C                   PARM                    DSMSPG
057300970526     C                   PARM                    DSMSKE
057400970526     C                   PARM                    DSMSER
057500970526     C*
057600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
057700970526     C                   CALL      'XKEY'
057800970526     C                   PARM                    DSKEY
057900970526     C*---
058000970526     C* VARIABILI NON RIFERITE AL DATA BASE
058100970526     C*---
058200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
058300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
058400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
058500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
058600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
058700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
058800970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
058900970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
059000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
059100030822     C     *LIKE         DEFINE    POP0          DEPOP0                         *DI DEPOSITO
059200030822     C     *LIKE         DEFINE    POP1          DEPOP1
059300030822     C     *LIKE         DEFINE    POP0          WOP0                           *DI LAVORO
059400030822     C     *LIKE         DEFINE    POP1          WOP1
059500030822     C     *LIKE         DEFINE    PCODR         DCODR
059600970526     C*---
059700030821     C     *LIKE         DEFINE    VIDcodr1      DEPcodr1
059800030821     C*---
059900970526     C* VARIABILI DI CONTROLLO
060000970526     C*---
060100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
060200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
060300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
060400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
060500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
060600040929     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
060700970526     C*
060800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
060900030821     C                   MOVEL     'TRTCM5R'     DSMSPG
061000030821     C                   MOVEL     'TRTCM5R'     MSGQUE
061100970526     C                   MOVEL     *BLANKS       MSGKEY
061200970526     C                   MOVEL     *BLANKS       DSMSMS
061300970526     C                   MOVEL     *BLANKS       DSMSKE
061400970526     C                   Z-ADD     *ZEROS        DSMSER
061500970526     C*---
061600970526     C* VARIABILI RIFERITE AL DATA BASE
061700970526     C*---
061800030821     C     *LIKE         DEFINE    MARcodr       KMARcodr
061900970526     C*---
062000970526     C* CHIAVI DI LETTURA
062100970526     C*---
062200030821     C* LETTURA timar01l - COMPLETA
062300030821     C     KEYmar        KLIST
062400030821     C                   KFLD                    KMARcodr                       *CODICE REGOLA
062500970527     C*
062600970527     C* REPERIMENTO DATI UTENTE
062700020916     C                   EXSR      REPDATIUTE
062800970526     C*
062900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
063000030822     C                   MOVEL     POP0          DEPOP0                         *LIVELLO PROCEDURA
063100030822     C                   MOVEL     POP1          DEPOP1                         *LIVELLO PROGRAMMA
063200970526     C*
063300970526     C* CALCOLA LA DATA CORRENTE
063400970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
063500970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
063600970526     C                   Z-ADD     WN8           G08DAT
063700970526     C                   Z-ADD     *ZEROS        G08INV
063800970526     C                   MOVEL     '0'           G08ERR
063900970526     C                   CALL      'XSRDA8'
064000970526     C                   PARM                    WLBDA8
064100970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
064200970526     C*
064300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
064400030821     C                   MOVEL     *blanks       VIDcodr1
064500970526     C                   EXSR      CHKPAR
064600970526IF  1C     ERRL00        IFEQ      '1'
064700030822     C                   MOVEL     DSMSMS        PMSG
064800970526X   1C                   ELSE
064900030821     C                   MOVEL     VIDcodr1      DEPcodr1                       *DEPOSITI
065000970526E   1C                   ENDIF
065100970526     C*
065200970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
065300970526     C                   EXSR      CHKFOP
065400970526     C*
065500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
065600970530IF  1C     DEPOP0        IFEQ      'P01'
065700970530     C                   MOVEL     DEC(3)        VIDING
065800970530E   1C                   ENDIF
065900970530IF  1C     DEPOP0        IFEQ      'P02'
066000970526     C                   MOVEL     DEC(1)        VIDING
066100970530E   1C                   ENDIF
066200970530IF  1C     DEPOP0        IFEQ      'P05'
066300970526     C                   MOVEL     DEC(2)        VIDING
066400970526E   1C                   ENDIF
066500970526     C*
066600970526     C                   ENDSR
066700970526     C*------------------------------------------------------------------------*
066800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
066900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
067100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
067200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
067300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
067500970526     C*------------------------------------------------------------------------*
067600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
067700970526     C*------------------------------------------------------------------------*
067800970526     C     POSSFL        BEGSR
067900970526     C*
068000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
068100970526     C                   EXSR      CLRSFL
068200970526     C*
068300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
068400970526     C                   EXSR      SETFIL
068500970526     C*
068600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
068700970526     C     SFLEOF        IFEQ      0
068800970526     C                   EXSR      CARSFL
068900970526     C                   END
069000970526     C*
069100970526     C                   ENDSR
069200970526     C*------------------------------------------------------------------------*
069300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
069400970526     C*------------------------------------------------------------------------*
069500970526     C     ENTATT        BEGSR
069600970526     C*
069700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
069800970526     C                   Z-ADD     0             OPZOK             1 0
069900970526     C*
070000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
070100970526     C                   Z-ADD     0             SFLVAR            5 0
070200970526     C*
070300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
070400970526     C                   Z-ADD     0             SFLERR
070500970526     C*
070600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
070700970526     C                   Z-ADD     SFLULT        WFLULT
070800970526     C*
070900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
071000030821     C                   READC     TCM5D05                                98
071100970526     C     *IN98         DOWEQ     '0'
071200970526     C*
071300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
071400970526     C                   ADD       1             SFLVAR
071500970526     C*
071600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
071700970526     C     OPZOK         IFEQ      0
071800970526     C*
071900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
072000970526     C     OPZKEY        IFNE      *BLANKS
072100970526     C                   EXSR      OPZCON
072200970526     C                   END
072300970526     C*
072400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
072500970526     C*            2. PULIZIA SCELTA IN SFL
072600970526     C     OPZOK         IFEQ      0
072700970526     C                   MOVEL     '0'           *IN41
072800970526     C                   MOVEL     *BLANKS       OPZKEY
072900970526     C                   ELSE
073000970526     C*
073100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
073200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
073300970526     C                   MOVEL     '1'           *IN41
073400970526     C                   Z-ADD     SFLULT        SFLERR
073500970526     C                   END
073600970526     C                   ELSE
073700970526     C*
073800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
073900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
074000970526     C                   MOVEL     '1'           *IN41
074100970526     C                   END                                                    OPZOK=0
074200970526     C*
074300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
074400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
074500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
074600970526     C     OPZOK         IFNE      0
074700970526     C                   Z-ADD     SFLERR        SFLNBR
074800970526     C                   ELSE
074900970526     C                   Z-ADD     SFLULT        SFLNBR
075000970526     C                   END
075100970526     C*
075200970526     C* AGGIORNO RECORD SFL
075300030821     C                   UPDATE    TCM5D05
075400970526     C*
075500030821     C                   READC     TCM5D05                                98
075600970526     C                   END                                                    DO WHILE
075700970526     C*
075800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
075900970526     C                   Z-ADD     WFLULT        SFLULT
076000970526     C*
076100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
076200970526     C     SFLVAR        IFEQ      0
076300970526     C*---                 MOVEL'1'       FINL00
076400970526     C                   END
076500970526     C*
076600970526     C                   ENDSR
076700970526     C*------------------------------------------------------------------------*
076800970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
076900970526     C*------------------------------------------------------------------------*
077000970526     C     CLRSFL        BEGSR
077100970526     C*
077200970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
077300970526     C* (0=NON FINE FILE  1=FINE FILE)
077400970526     C                   Z-ADD     0             SFLEOF
077500970526     C*
077600970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
077700970526     C                   Z-ADD     0             SFLULT
077800970526     C*
077900970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
078000970526     C                   MOVEL     '1'           *IN40
078100030821     C                   WRITE     TCM5D06
078200970526     C                   MOVEL     '0'           *IN40
078300970526     C*
078400970526     C                   ENDSR
078500970526     C*------------------------------------------------------------------------*
078600970526     C* CARSFL - CARICAMENTO SFL
078700970526     C*------------------------------------------------------------------------*
078800970526     C     CARSFL        BEGSR
078900970526     C*
079000970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
079100970526     C     SFLULT        IFEQ      0
079200970526     C                   EXSR      SETFIL
079300970526     C                   END
079400970526     C*
079500970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
079600970526     C                   Z-ADD     SFLULT        WFLULT
079700970526     C*
079800970526     C* CICLO:
079900970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
080000970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
080100970526     C                   Z-ADD     1             SFLRIG
080200970526     C     SFLRIG        DOWLE     SFLPAG
080300970526     C     SFLEOF        ANDEQ     0
080400970526     C*
080500970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
080600970526     C                   MOVEL     *BLANKS       OPZKEY
080700970526     C*
080800970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
080900970526     C                   EXSR      IRESFL
081000970526     C*
081100970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
081200970526     C                   ADD       1             SFLULT
081300970526     C*
081400970526     C* SCRITTURA RECORD SFL
081500030821     C                   WRITE     TCM5D05
081600970526     C                   ADD       1             SFLRIG
081700970526     C*
081800970526     C* LETTURA FILE DA CARICARE
081900970526     C                   EXSR      LETFIL
082000970526     C                   END                                                    DO WHILE
082100970526     C*
082200970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
082300970526     C     WFLULT        IFNE      SFLULT
082400970526     C                   Z-ADD     WFLULT        SFLNBR
082500970526     C                   ADD       1             SFLNBR
082600970526     C                   END
082700970526     C*
082800970526     C                   ENDSR
082900970526     C*------------------------------------------------------------------------*
083000970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
083100970526     C*------------------------------------------------------------------------*
083200970526     C     OUTSFL        BEGSR
083300970526     C*
083400970526     C* WRITE RECORD TESTATA VIDEO
083500030821     C                   WRITE     TCM5D01
083600970526     C*
083700970526     C* VISUALIZZAZIONE MESSAGGIO
083800970526     C                   EXSR      SNDMSG
083900970526     C*
084000970526     C* WRITE RECORD OPZIONI VIDEO
084100030821     C                   WRITE     TCM5D02
084200970526     C*
084300970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
084400030821     C                   WRITE     TCM5D03
084500970526     C*
084600970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
084700030821     C                   WRITE     TCM5D04
084800970526     C*
084900970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
085000970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
085100970526     C     SFLEOF        IFEQ      0
085200970526     C                   MOVEL     '0'           *IN42
085300970526     C                   ELSE
085400970526     C*
085500970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
085600970526     C                   MOVEL     '1'           *IN42
085700970526     C                   END
085800970526     C*
085900970526     C* WRITE RECORD TASTI FUNZIONALI
086000030821     C                   WRITE     TCM5D09
086100970526     C*
086200970526     C* CASO DI SFL VUOTO
086300970526     C     SFLULT        IFLE      0
086400970526     C*
086500970526     C*    - WRITE RECORD VUOTO
086600030821     C                   WRITE     TCM5D0A
086700030821     C                   READ      TCM5D0A                                99
086800970526     C*
086900970526     C                   ELSE
087000970526     C*
087100970526     C* CASO DI SFL CON RECORDS
087200970526     C*
087300970526     C*    - WRITE RECORD CONTROLLO SFL
087400030821     C                   WRITE     TCM5D06
087500030821     C                   READ      TCM5D06                                99
087600970526     C                   END
087700970526     C*
087800970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
087900030821     C                   READ      TCM5D03                                99
088000970526     C*
088100970526     C                   ENDSR
088200970526     C*------------------------------------------------------------------------*
088300970526     C* SNDMSG - MANDA MESSAGGIO
088400970526     C*------------------------------------------------------------------------*
088500970526     C     SNDMSG        BEGSR
088600970526     C*
088700970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
088800970526     C                   MOVEL     '1'           *IN40
088900030821     C                   WRITE     TCM5DMC
089000970526     C                   MOVEL     '0'           *IN40
089100970526     C*
089200970526     C* VISUALIZZAZIONE MESSAGGIO
089300970526     C                   Z-ADD     0             MSGULT
089400970526     C                   CALL      'XMSG'        XMSGPA
089500970526     C     DSMSER        IFEQ      0
089600970526     C                   MOVEL     DSMSKE        MSGKEY
089700970526     C                   END
089800970526     C                   Z-ADD     1             MSGULT
089900030821     C                   WRITE     TCM5DMS                                      SFL MESS
090000030821     C                   WRITE     TCM5DMC                                      CONTR MESS
090100970526     C*
090200970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
090300970526     C                   MOVEL     *BLANKS       DSMSMS
090400970526     C*
090500970526     C                   ENDSR
090600970526     C*------------------------------------------------------------------------*
090700970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
090800970526     C*------------------------------------------------------------------------*
090900970526     C     FUNCON        BEGSR
091000970526     C*
091100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
091200970526     C                   EXSR      FUNVAR
091300970526     C*
091400970526     C* PULISCO CAMPI DI LAVORO
091500970526     C                   Z-ADD     0             I                 5 0
091600970526     C                   Z-ADD     0             W                 5 0
091700970526     C*
091800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
091900970526     C     FUNKEY        IFNE      *BLANKS
092000970526     C     1             DO        24            I
092100970526     C                   MOVEL     FUD(I)        FUDRIG
092200970526     C*
092300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
092400970526     C     FUNKEY        IFEQ      FUDFUN
092500970526     C     FUDATT        ANDEQ     'S'
092600970526     C                   Z-ADD     I             W
092700970526     C                   END
092800970526     C                   END                                                    DO WHILE
092900970526     C*
093000970526     C* FUNZIONE NON ATTIVA (ERRORE)
093100970526     C     W             IFEQ      0
093200970526     C*
093300970526     C* EMISSIONE MESSAGGIO DI ERRORE
093400970526     C                   MOVEL     MSG(2)        DSMSMS
093500970526     C                   END                                                    W=0
093600970526     C                   END                                                    FUNKEY NE *BLANKS
093700970526     C*
093800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
093900970526     C     W             IFNE      0
094000970526     C     FUNKEY        OREQ      *BLANKS
094100970526     C*
094200970526     C     FUNKEY        IFEQ      *BLANKS
094300970526     C                   EXSR      ENTGES
094400970526     C                   END
094500970526     C     FUNKEY        IFEQ      '03'
094600970526     C                   EXSR      F03GES
094700970526     C                   END
094800970526     C     FUNKEY        IFEQ      '05'
094900970526     C                   EXSR      F05GES
095000970526     C                   END
095100970526     C     FUNKEY        IFEQ      '06'
095200970526     C                   EXSR      F06GES
095300970526     C                   END
095400970526     C     FUNKEY        IFEQ      '12'
095500970526     C                   EXSR      F12GES
095600970526     C                   END
095700970526     C     FUNKEY        IFEQ      '23'
095800970526     C                   EXSR      F23GES
095900970526     C                   END
096000970526     C     FUNKEY        IFEQ      '24'
096100970526     C                   EXSR      F24GES
096200970526     C                   END
096300970526     C     FUNKEY        IFEQ      'RU'
096400970526     C                   EXSR      RUPGES
096500970526     C                   END
096600970526     C*
096700970526     C                   END
096800970526     C*
096900970526     C                   ENDSR
097000970526     C*------------------------------------------------------------------------*
097100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
097200970526     C*------------------------------------------------------------------------*
097300970526     C     FUNVAR        BEGSR
097400970526     C*
097500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
097600970526     C     KEY           IFEQ      ENTER
097700970526     C                   MOVEL     *BLANKS       FUNKEY
097800970526     C                   END
097900970526     C     KEY           IFEQ      F1
098000970526     C                   MOVEL     '01'          FUNKEY
098100970526     C                   END
098200970526     C     KEY           IFEQ      F2
098300970526     C                   MOVEL     '02'          FUNKEY
098400970526     C                   END
098500970526     C     KEY           IFEQ      F3
098600970526     C                   MOVEL     '03'          FUNKEY
098700970526     C                   END
098800970526     C     KEY           IFEQ      F4
098900970526     C                   MOVEL     '04'          FUNKEY
099000970526     C                   END
099100970526     C     KEY           IFEQ      F5
099200970526     C                   MOVEL     '05'          FUNKEY
099300970526     C                   END
099400970526     C     KEY           IFEQ      F6
099500970526     C                   MOVEL     '06'          FUNKEY
099600970526     C                   END
099700970526     C     KEY           IFEQ      F7
099800970526     C                   MOVEL     '07'          FUNKEY
099900970526     C                   END
100000970526     C     KEY           IFEQ      F8
100100970526     C                   MOVEL     '08'          FUNKEY
100200970526     C                   END
100300970526     C     KEY           IFEQ      F9
100400970526     C                   MOVEL     '09'          FUNKEY
100500970526     C                   END
100600970526     C     KEY           IFEQ      F10
100700970526     C                   MOVEL     '10'          FUNKEY
100800970526     C                   END
100900970526     C     KEY           IFEQ      F11
101000970526     C                   MOVEL     '11'          FUNKEY
101100970526     C                   END
101200970526     C     KEY           IFEQ      F12
101300970526     C                   MOVEL     '12'          FUNKEY
101400970526     C                   END
101500970526     C     KEY           IFEQ      F13
101600970526     C                   MOVEL     '13'          FUNKEY
101700970526     C                   END
101800970526     C     KEY           IFEQ      F14
101900970526     C                   MOVEL     '14'          FUNKEY
102000970526     C                   END
102100970526     C     KEY           IFEQ      F15
102200970526     C                   MOVEL     '15'          FUNKEY
102300970526     C                   END
102400970526     C     KEY           IFEQ      F16
102500970526     C                   MOVEL     '16'          FUNKEY
102600970526     C                   END
102700970526     C     KEY           IFEQ      F17
102800970526     C                   MOVEL     '17'          FUNKEY
102900970526     C                   END
103000970526     C     KEY           IFEQ      F18
103100970526     C                   MOVEL     '18'          FUNKEY
103200970526     C                   END
103300970526     C     KEY           IFEQ      F19
103400970526     C                   MOVEL     '19'          FUNKEY
103500970526     C                   END
103600970526     C     KEY           IFEQ      F20
103700970526     C                   MOVEL     '20'          FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F21
104000970526     C                   MOVEL     '21'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F22
104300970526     C                   MOVEL     '22'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F23
104600970526     C                   MOVEL     '23'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F24
104900970526     C                   MOVEL     '24'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      ROLLUP
105200970526     C                   MOVEL     'RU'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      HELP
105500970526     C                   MOVEL     'HP'          FUNKEY
105600970526     C                   END
105700970526     C*
105800970526     C                   ENDSR
105900970526     C*------------------------------------------------------------------------*
106000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
106100970526     C*------------------------------------------------------------------------*
106200970526     C     FUNGES        BEGSR
106300970526     C*
106400970526     C* PULISCO CAMPI DI LAVORO
106500970526     C                   Z-ADD     0             W                 5 0
106600970526     C*
106700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
106800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
106900970526     C     1             DO        24            I
107000970526     C                   MOVEL     FUD(I)        FUDRIG
107100970526     C*
107200970526     C* PULIZIA FUNZIONI PRECEDENTI
107300970526     C                   MOVEL     *BLANKS       FUV(I)
107400970526     C*
107500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
107600970526     C* (SALTA F23 E F24)
107700970526     C     FUDATT        IFEQ      'S'
107800970526     C     FUDFUN        ANDNE     '23'
107900970526     C     FUDFUN        ANDNE     '24'
108000970526     C                   ADD       1             W
108100970526     C                   MOVEL     FUDDES        FUV(W)
108200970526     C                   END
108300970526     C                   END                                                    DO WHILE
108400970526     C*
108500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
108600970526     C* ALTRIMENTI DISATTIVO F24
108700970526     C     1             DO        24            I
108800970526     C                   MOVEL     FUD(I)        FUDRIG
108900970526     C*
109000970526     C     FUDFUN        IFEQ      '24'
109100970526     C     W             IFGT      6
109200970526     C                   MOVEL     'S'           FUDATT
109300970526     C                   ELSE
109400970526     C                   MOVEL     *BLANKS       FUDATT
109500970526     C                   END                                                    W GT 8
109600970526     C                   MOVEL     FUDRIG        FUD(I)
109700970526     C                   END
109800970526     C                   END                                                    DO WHILE
109900970526     C*
110000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
110100970526     C     W             DIV       6             I
110200970526     C                   MVR                     Z
110300970526     C*
110400970526     C     Z             IFNE      0
110500970526     C     I             ADD       1             Z
110600970526     C                   ELSE
110700970526     C                   Z-ADD     I             Z
110800970526     C                   END
110900970526     C*
111000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
111100970526     C                   ADD       1             FUNGRU
111200970526     C     FUNGRU        IFGT      Z
111300970526     C     FUNGRU        ORLT      1
111400970526     C                   Z-ADD     1             FUNGRU
111500970526     C                   END
111600970526     C*
111700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
111800970526     C     FUNGRU        IFEQ      1
111900970526     C                   MOVEL     FUV(1)        FUN001
112000970526     C                   MOVEL     FUV(2)        FUN002
112100970526     C                   MOVEL     FUV(3)        FUN003
112200970526     C                   MOVEL     FUV(4)        FUN004
112300970526     C                   MOVEL     FUV(5)        FUN005
112400970526     C                   MOVEL     FUV(6)        FUN006
112500970526     C                   END
112600970526     C*
112700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
112800970526     C     FUNGRU        IFEQ      2
112900970526     C                   MOVEL     FUV(7)        FUN001
113000970526     C                   MOVEL     FUV(8)        FUN002
113100970526     C                   MOVEL     FUV(9)        FUN003
113200970526     C                   MOVEL     FUV(10)       FUN004
113300970526     C                   MOVEL     FUV(11)       FUN005
113400970526     C                   MOVEL     FUV(12)       FUN006
113500970526     C                   END
113600970526     C*
113700970526     C* TERZO GRUPPO DI 6 FUNZIONI
113800970526     C     FUNGRU        IFEQ      3
113900970526     C                   MOVEL     FUV(13)       FUN001
114000970526     C                   MOVEL     FUV(14)       FUN002
114100970526     C                   MOVEL     FUV(15)       FUN003
114200970526     C                   MOVEL     FUV(16)       FUN004
114300970526     C                   MOVEL     FUV(17)       FUN005
114400970526     C                   MOVEL     FUV(18)       FUN006
114500970526     C                   END
114600970526     C*
114700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
114800970526     C     FUNGRU        IFEQ      4
114900970526     C                   MOVEL     FUV(19)       FUN001
115000970526     C                   MOVEL     FUV(20)       FUN002
115100970526     C                   MOVEL     FUV(21)       FUN003
115200970526     C                   MOVEL     FUV(22)       FUN004
115300970526     C                   MOVEL     *BLANKS       FUN005
115400970526     C                   MOVEL     *BLANKS       FUN006
115500970526     C                   END
115600970526     C*
115700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
115800970526     C                   MOVEL     *BLANKS       FUN007
115900970526     C                   MOVEL     *BLANKS       FUN008
116000970526     C*
116100970526     C     1             DO        24            I
116200970526     C                   MOVEL     FUD(I)        FUDRIG
116300970526     C     FUDFUN        IFEQ      '23'
116400970526     C     FUDATT        ANDEQ     'S'
116500970526     C                   MOVEL     FUDDES        FUN007
116600970526     C                   END
116700970526     C     FUDFUN        IFEQ      '24'
116800970526     C     FUDATT        ANDEQ     'S'
116900970526     C                   MOVEL     FUDDES        FUN008
117000970526     C                   END
117100970526     C                   END                                                    DO
117200970526     C*
117300970526     C                   ENDSR
117400970526     C*------------------------------------------------------------------------*
117500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
117600970526     C*------------------------------------------------------------------------*
117700970526     C     OPZGES        BEGSR
117800970526     C*
117900970526     C* PULISCO CAMPI DI LAVORO
118000970526     C                   Z-ADD     0             W                 5 0
118100970526     C*
118200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
118300970526     C* E CARICO TABELLA OPZIONI ATTIVE
118400970526     C     1             DO        24            I
118500970526     C                   MOVEL     OPD(I)        OPDRIG
118600970526     C*
118700970526     C* PULIZIA OPZIONI PRECEDENTI
118800970526     C                   MOVEL     *BLANKS       OPV(I)
118900970526     C*
119000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
119100970526     C     OPDATT        IFEQ      'S'
119200970526     C                   ADD       1             W
119300970526     C                   MOVEL     OPDDES        OPV(W)
119400970526     C                   END
119500970526     C                   END                                                    DO WHILE
119600970526     C*
119700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
119800970526     C* ALTRIMENTI DISATTIVO F23
119900970526     C     1             DO        24            I
120000970526     C                   MOVEL     FUD(I)        FUDRIG
120100970526     C*
120200970526     C     FUDFUN        IFEQ      '23'
120300970526     C     W             IFGT      8
120400970526     C                   MOVEL     'S'           FUDATT
120500970526     C                   ELSE
120600970526     C                   MOVEL     *BLANKS       FUDATT
120700970526     C                   END                                                    W GT 8
120800970526     C                   MOVEL     FUDRIG        FUD(I)
120900970526     C                   END
121000970526     C                   END                                                    DO WHILE
121100970526     C*
121200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
121300970526     C     W             DIV       8             I
121400970526     C                   MVR                     Z                 5 0
121500970526     C*
121600970526     C     Z             IFNE      0
121700970526     C     I             ADD       1             Z
121800970526     C                   ELSE
121900970526     C                   Z-ADD     I             Z
122000970526     C                   END
122100970526     C*
122200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
122300970526     C                   ADD       1             OPZGRU
122400970526     C     OPZGRU        IFGT      Z
122500970526     C     OPZGRU        ORLT      1
122600970526     C                   Z-ADD     1             OPZGRU
122700970526     C                   END
122800970526     C*
122900970526     C* PRIMO GRUPPO DI 8 OPZIONI
123000970526     C     OPZGRU        IFEQ      1
123100970526     C                   MOVEL     OPV(1)        OPZ001
123200970526     C                   MOVEL     OPV(2)        OPZ002
123300970526     C                   MOVEL     OPV(3)        OPZ003
123400970526     C                   MOVEL     OPV(4)        OPZ004
123500970526     C                   MOVEL     OPV(5)        OPZ005
123600970526     C                   MOVEL     OPV(6)        OPZ006
123700970526     C                   MOVEL     OPV(7)        OPZ007
123800970526     C                   MOVEL     OPV(8)        OPZ008
123900970526     C                   END
124000970526     C*
124100970526     C* SECONDO GRUPPO DI 8 OPZIONI
124200970526     C     OPZGRU        IFEQ      2
124300970526     C                   MOVEL     OPV(9)        OPZ001
124400970526     C                   MOVEL     OPV(10)       OPZ002
124500970526     C                   MOVEL     OPV(11)       OPZ003
124600970526     C                   MOVEL     OPV(12)       OPZ004
124700970526     C                   MOVEL     OPV(13)       OPZ005
124800970526     C                   MOVEL     OPV(14)       OPZ006
124900970526     C                   MOVEL     OPV(15)       OPZ007
125000970526     C                   MOVEL     OPV(16)       OPZ008
125100970526     C                   END
125200970526     C*
125300970526     C* TERZO GRUPPO DI 8 OPZIONI
125400970526     C     OPZGRU        IFEQ      3
125500970526     C                   MOVEL     OPV(17)       OPZ001
125600970526     C                   MOVEL     OPV(18)       OPZ002
125700970526     C                   MOVEL     OPV(19)       OPZ003
125800970526     C                   MOVEL     OPV(20)       OPZ004
125900970526     C                   MOVEL     OPV(21)       OPZ005
126000970526     C                   MOVEL     OPV(22)       OPZ006
126100970526     C                   MOVEL     OPV(23)       OPZ007
126200970526     C                   MOVEL     OPV(24)       OPZ008
126300970526     C                   END
126400970526     C*
126500970526     C                   ENDSR
126600970526     C*------------------------------------------------------------------------*
126700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
126800970526     C*------------------------------------------------------------------------*
126900970526     C     OPZCON        BEGSR
127000970526     C*
127100970526     C* PULISCO CAMPI DI LAVORO
127200970526     C                   Z-ADD     0             I                 5 0
127300970526     C                   Z-ADD     0             W                 5 0
127400970526     C*
127500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
127600970526     C     1             DO        24            I
127700970526     C                   MOVEL     OPD(I)        OPDRIG
127800970526     C*
127900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
128000970526     C     OPZKEY        IFEQ      OPDOPZ
128100970526     C     OPZKEY        OREQ      OPDOP1
128200970526     C*
128300970526     C     OPDATT        IFEQ      'S'
128400970526     C                   MOVEL     OPDOPZ        OPZKEY
128500970526     C                   Z-ADD     I             W
128600970526     C                   END
128700970526     C                   END
128800970526     C*
128900970526     C                   END                                                    DO WHILE
129000970526     C*
129100970526     C* OPZIONE NON ATTIVA (ERRORE)
129200970526     C     W             IFEQ      0
129300970526     C*
129400970526     C* OPZIONE NON OK
129500970526     C                   Z-ADD     1             OPZOK
129600970526     C*
129700970526     C* EMISSIONE MESSAGGIO DI ERRORE
129800970526     C                   MOVEL     MSG(1)        DSMSMS
129900970526     C                   ELSE
130000970526     C*
130100970526     C* OPZIONE ATTIVA (PROCEDI ...)
130200970526     C*
130300030822     C     OPZKEY        IFEQ      ' 1'
130400030822     C                   EXSR      O01GES
130500030822     C                   END
130600970526     C     OPZKEY        IFEQ      ' 2'
130700970526     C                   EXSR      O02GES
130800970526     C                   END
130900991014     C     OPZKEY        IFEQ      ' 3'
131000991014     C                   EXSR      O03GES
131100991014     C                   END
131200040929     C     OPZKEY        IFEQ      ' 4'
131300040929     C                   EXSR      O04GES
131400040929     C                   END
131500970526     C     OPZKEY        IFEQ      ' 5'
131600970526     C                   EXSR      O05GES
131700970526     C                   END
131701040930     C     OPZKEY        IFEQ      ' 7'
131702040930     C                   EXSR      O07GES
131703040930     C                   END
131800970526     C                   END                                                    OPZ.NON ATTIVA
131900970526     C*
132000970526     C                   ENDSR
132100970526     C*------------------------------------------------------------------------*
132200970526     C* NOTE COSTRUZIONE SCHIERE
132300970526     C*------------------------------------------------------------------------*
132400970526     C*
132500970526     C* -MSG-MESSAGGI
132600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
132700970526     C* (NON TOCCARE MSG,1 E MSG,2)
132800970526     C*
132900970526     C* -CMD-COMANDI
133000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
133100970526     C*
133200970526     C* -OPD-OPZIONI
133300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
133400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
133500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
133600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
133700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
133800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
133900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
134000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
134100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
134200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
134300970526     C*
134400970526     C* -FUD-FUNZIONI
134500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
134600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
134700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
134800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
134900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
135000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
135100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
135200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
135300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
135400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
135500970526     O*------------------------------------------------------------------------*
135600970526** DEC - DECODIFICHE
135700970526    GESTIONE                                                     1
135800970526INTERROGAZIONE                                                   2
135900970530     SCELTA                                                      3
1360009705264
1361009705265
136200970526** MSG - MESSAGGI
136300030821TRTCM5R- Opzione non gestita                                                   1
136400030821TRTCM5R- Tasto funzionale non gestito                                          2
136500030821TRTCM5R- 3                                                                     3
136600030821TRTCM5R- Record annullato. Consentito solo il ripristino                       4
136700030821TRTCM5R- Record non annullato. Impossibile ripristinare                        5
136800030821TRTCM5R- 6                                                                     6
136900030821TRTCM5R- 7                                                                     7
137000030821TRTCM5R- 8                                                                     8
137100030821TRTCM5R- 9                                                                     9
137200030821TRTCM5R- 10                                                                   10
137300030821TRTCM5R- 11                                                                   11
137400030821TRTCM5R- Data errata                                                          12
137500030821TRTCM5R- 13                                                                   13
137600030821TRTCM5R- 14                                                                   14
137700030821TRTCM5R- 15                                                                   15
137800030821TRTCM5R- 16                                                                   16
137900030821TRTCM5R- 17                                                                   17
138000030821TRTCM5R- 18                                                                   18
138100030821TRTCM5R- 19                                                                   19
138200030821TRTCM5R- 20                                                                   20
138300970526** OPD - OPZIONI
138400030822 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
138500970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
138600991014 3=Copia           -3 -     (S/N)=N                        opz. 03¬
138700040929 4=Annulla         -4 -     (S/N)=N                        opz. 04¬
138800970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
138900040930                   -6 -     (S/N)=N                        opz. 06¬
139000040930 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
139100970527                   -8 -     (S/N)=N                        opz. 08¬
139200030821                   -9 -     (S/N)=N                        opz. 09¬
139300970527                   -10-     (S/N)=N                        opz. 10¬
139400970526                   -  -     (S/N)=N                        opz. 11¬
139500970526                   -  -     (S/N)=N                        opz. 12¬
139600970526                   -  -     (S/N)=N                        opz. 13¬
139700970526                   -  -     (S/N)=N                        opz. 14¬
139800970526                   -  -     (S/N)=N                        opz. 15¬
139900970526                   -  -     (S/N)=N                        opz. 16¬
140000970526                   -  -     (S/N)=N                        opz. 17¬
140100970526                   -  -     (S/N)=N                        opz. 18¬
140200970526                   -  -     (S/N)=N                        opz. 19¬
140300970526                   -  -     (S/N)=N                        opz. 20¬
140400970526                   -  -     (S/N)=N                        opz. 21¬
140500970526                   -  -     (S/N)=N                        opz. 22¬
140600970526                   -  -     (S/N)=N                        opz. 23¬
140700970526                   -  -     (S/N)=N                        opz. 24¬
140800970526** FUD - FUNZIONI
140900970526                   -        (S/N)=N                        funz.01¬
141000970526                   -        (S/N)=N                        funz.02¬
141100970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
141200970526                   -        (S/N)=N                        funz.04¬
141300970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
141400970526F06=Inserimento    -        (S/N)=N                        funz.06¬
141500970526                   -        (S/N)=N                        funz.07¬
141600970527                   -        (S/N)=N                        funz.08¬
141700970527                   -        (S/N)=N                        funz.09¬
141800970527                   -        (S/N)=N                        funz.10¬
141900970526                   -        (S/N)=N                        funz.11¬
142000970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
142100970526                   -        (S/N)=N                        funz.13¬
142200970526                   -        (S/N)=N                        funz.14¬
142300970526                   -        (S/N)=N                        funz.15¬
142400970526                   -        (S/N)=N                        funz.16¬
142500970526                   -        (S/N)=N                        funz.17¬
142600970526                   -        (S/N)=N                        funz.18¬
142700970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
142800970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
142900030821                   -        (S/N)=N                        funz.21¬
143000030821                   -        (S/N)=N                        funz.22¬
143100970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
143200970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
