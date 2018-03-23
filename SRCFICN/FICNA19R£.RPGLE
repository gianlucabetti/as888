000100100223     H DECEDIT('0,') DATEDIT(*DMY.)
000200090707     H* ficna19R*----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400090707     H*               GESTIONE storico TARIFFE aff/defl
000500000000     H*--------------------------------------------------------------*
000600090707     Fficna19D  CF   E             WORKSTN
000700090707     F                                     SFILE(video2:NRR1)
000800090707     Ffiadts0f  if   E           K DISK
000900021203     Ffiapd01l  if   E           K DISK
001000140117      ****-?
001100140117     D**** TRUL72DS      E DS
001200140117     D****  L72                           3s 0 DESCEND
001300140117     D****                                     DIM(60)
001400140117     D****                                     OVERLAY(O72Lia)
001500140117      ****-?Caricamento filiali contabili
001600140117     d TRUL31ds      e ds                  inz
001700140117     d $POg            s              3    inz   dim(250)
001800140117      ****-?
001900950113     D* SCHIERE PER CARICAMENTO L1 E L6
002000950113     D L6              S              3  0 DIM(30)                              FIL GESTITE
002100930923     D*
002200940919     D* DS PER TRUL06R - CARICAMENTO £1
002300940919     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002400940919     D  LIN                    1     90  0
002500940919     D                                     DIM(30)                              SKI COMODO
002600940930     D*
002700921102     D WLBDAT          DS
002800940906     D  G02DAT                 1      8  0
002900940906     D  G02INV                 9     16  0
003000940906     D  G02ERR                17     17
003100940906     D  G02TGI                18     22  0
003200940504     D                 DS
003300940906     D  AA                     1      4  0
003400940906     D  MM                     5      6  0
003500940906     D  GG                     7      8  0
003600940906     D  DATA                   1      8  0
003700000000     D KPJBA         E DS
003800940919     D  REM                    1      3
003900940919     D  REMFIL                 4      6
004000900320     D CNCR80        E DS
004100021203     D fnlv24ds      E DS
004200900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004300931115     D  TCU                  398    697
004400931115     D                                     DIM(50)                              TAB FLAG CNT
004500931115     D  KCU                  698    847P 0
004600931115     D                                     DIM(50)                              TAB CAPO CONTO
004700931115     D                                     PACKEVEN
004800931115     D  DTU                  848    895P 0
004900931115     D                                     DIM(12)                              DATE PARAM
005000931115     D                                     PACKEVEN
005100020129     d dataiso         s               d   datfmt(*iso)
005200020129     d dataeur         s               d   datfmt(*eur)
005300941005     D*
005400020130     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)            GG DEI MESI
005500930811     C*****************************************************************
005600930811     C*---------------------------------------------------------------*
005700930928     C* ESEGUO OPERAZIONI INIZIALI
005800930928     C                   EXSR      OPERAZ
005900090707     C                   EXSR      srvideo1
006000930903     C*
006100930820     C                   SETON                                        LR
006200930928     C*
006300930811     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
006400090707     C     srVIDEO1      BEGSR
006500011109     C*---------------------------------------------------------------*
006600930930     C*
006700020124     C                   EXSR      PULSF
006800930907     C*
006900930907     C     FOR01         TAG
007000090707     C                   EXFMT     video1
007100011127     C                   SETOFF                                       045386
007200930929     C*
007300930811     C** CMD3  - FINE LAVORO
007400931013     C   KC              GOTO      ENDVD1
007500931013     C*
007600930907     C                   EXSR      CONTR1
007700930907     C   90              GOTO      FOR01
007800020528     c                   exsr      carsf
007900020528     c                   do        *hival
008000090707     c                   if        nrr1 > 0
008100090707     C                   SETON                                        91
008200090707     c                   else
008300090707     c                   setoff                                       91
008400090707     c                   end
008500090707     c                   z-add     1             nrr1
008600090707     C                   WRITE     video2z
008700090707     C                   EXFMT     video2c
008800020528     C   kl              EXSR      PULSF
008900020528     c   kl              goto      for01
009000020528     c                   enddo
009100930811     C*
009200930907     C     ENDVD1        ENDSR
009300930811     C*
009400930811     C*--- CONTROLLI DELLA PRIMA VIDEATA -----------------------------*
009500930811     C     CONTR1        BEGSR
009600011109     C*---------------------------------------------------------------*
009700940920     C****  CODICE FILIALE  ****
009800950113     C* FILIALE OBBLIGATORIA
009900090707     c                   setoff                                       8340
010000090720     c                   clear                   msgv
010100950113     C     VIDFIL        IFEQ      0
010200950113     C                   SETON                                        8390
010300950113     C                   GOTO      ENDCT1
010400950113     C                   ENDIF
010500950113     C*
010600950113     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
010700950113     C     *IN89         IFEQ      *OFF
010800950113     C     VIDFIL        IFNE      SIMFEL
010900090424     C     simpou        andne     046
011000140117     C**** VIDFIL        LOOKUP    L72                                    35
011100140117     C                   move      vidfil        Fil03A            3
011200140117     C     Fil03A        LOOKUP    $POg                                   35
011300940920     C  N35              SETON                                        8390
011400940920     C  N35              GOTO      ENDCT1
011500940920     C                   ENDIF
011600950113     C*
011700950113     C                   ELSE
011800950113     C* 89 ON  - SONO UN "REMXXX" QUINDI CONTROLLO LA FILIALE NELLA L6
011900950113     C                   MOVEL     VIDFIL        WFIL              3
012000950113     C     WFIL          IFNE      REMFIL
012100950113     C     VIDFIL        LOOKUP    L6                                     35
012200950113     C  N35              SETON                                        8390
012300950113     C  N35              GOTO      ENDCT1
012400950113     C                   ENDIF
012500950113     C                   ENDIF
012600940920     C*
012700930811     C****  CODICE PADRONCINO  ****
012800930908     C     '?'           SCAN      VIDPDR                                 90
012900930811     C*
013000930811     C     *IN90         IFEQ      *ON
013100021203     C                   Z-ADD     VIDFIL        d24FIL
013200021203     C                   Z-ADD     0             d24pdr
013300021203     C                   MOVEL     'R'           d24FLG
013400090707     C                   MOVEL     'D'           d24tip
013500021203     C                   MOVEL     *BLANKS       d24rsc
013600021203     C                   MOVEL     '$$'          d24ann
013700021203     C                   MOVEL(p)  fnlv24ds      KPJBU
013800940926     C                   CALL      'FNLV24R'
013900930811     C                   PARM                    KPJBA
014000021203     C                   MOVEL     KPJBU         fnlv24ds
014100021203     C                   move      d24pdr        vidpdr
014200021203     C                   MOVEL     d24rsc        despdr
014300020528     C                   end
014400930811     C*
014500930921     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
014600930921     C                   TESTN                   VIDPDR               35
014700090720     C  N35              SETON                                        4090
014800090720     c  n35              eval      msgv= 'Autista annullato o inesistente'
014900930921     C  N35              GOTO      ENDCT1
015000931111     C*
015100940915     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
015200940920     C                   MOVEL     VIDFIL        COMPDR
015300940920     C                   MOVE      VIDPDR        COMPDR
015400021203     C     kapdc         CHAIN     fiapd000                           32
015500940914     C* CODICE PADRONCINO ERRATO
015600090720     c   32              eval      msgv= 'Autista annullato o inesistente'
015700940914     C   32              SETON                                        4090
015800940914     C   32              GOTO      ENDCT1
015900100223     C                   MOVEL     APDRSf        despdr
016000000620     C*
016100930928     C****  CONTROLLO SE ESISTONO TARIFFE PER QUEL PADRONCINO  ****
016200931012     C* IMPOSTO CAMPI DELLA CHIAVE
016300930928     C                   MOVEL     VIDFIL        COMPDR
016400930915     C                   MOVE      VIDPDR        COMPDR
016500020528     C                   MOVEL     ' '           COMSML            1
016600930908     C*
016700930811     C     ENDCT1        ENDSR
016800930811     C*
016900930811     C*
017000930812     C*--- PULIZIA SUBFILE -------------------------------------------*
017100930812     C     PULSF         BEGSR
017200011109     C*---------------------------------------------------------------*
017300090707     C                   SETON                                        90
017400090707     C                   WRITE     video2c
017500090707     C                   SETOFF                                       90
017600930812     C                   ENDSR
017700930812     C*
017800930812     C*--- CARICAMENTO SUBFILE ---------------------------------------*
017900930812     C     CARSF         BEGSR
018000011109     C*---------------------------------------------------------------*
018100090707     C                   Z-ADD     0             NRR1              4 0
018200930812     C*
018300090707     c     compdr        setll     fiadts00
018400020528     c                   do        *hival
018500090707     C     compdr        READE     fiadts00                               31
018600020528     C   31              leave
018700090707     c                   z-add     adtprg        vi2prg
018800090707      *data decorrenza
018900090707     c                   if        adtddt > 0
019000090707     c                   move      adtddt        dataiso
019100090707     c                   move      dataiso       dataeur
019200090707     c                   move      dataeur       vi2ddt
019300090707     c                   else
019400090707     c                   clear                   vi2ddt
019500090707     c                   endif
019600090707      *data scadenza
019700090707     c                   if        adtdst > 0
019800090707     c                   move      adtdst        dataiso
019900090707     c                   move      dataiso       dataeur
020000090707     c                   move      dataeur       vi2dst
020100090707     c                   else
020200090707     c                   clear                   vi2dst
020300090707     c                   endif
020400090707      *data scadenza
020500090707     c                   if        adtduv > 0
020600090707     c                   move      adtduv        dataiso
020700090707     c                   move      dataiso       dataeur
020800090707     c                   move      dataeur       vi2duv
020900090707     c                   else
021000090707     c                   clear                   vi2duv
021100090707     c                   endif
021200090707      *data scadenza
021300090707     c                   if        adtdcn > 0
021400090707     c                   move      adtdcn        dataiso
021500090707     c                   move      dataiso       dataeur
021600090707     c                   move      dataeur       vi2dcn
021700090707     c                   else
021800090707     c                   clear                   vi2dcn
021900090707     c                   endif
022000090707      *data scadenza
022100090707     c                   if        adtdts > 0
022200090707     c                   move      adtdts        dataiso
022300090707     c                   move      dataiso       dataeur
022400090707     c                   move      dataeur       vi2dts
022500090707     c                   else
022600090707     c                   clear                   vi2dts
022700090707     c                   endif
022800090707      *data scadenza
022900090707     c                   if        adtdrc > 0
023000090707     c                   move      adtdrc        dataiso
023100090707     c                   move      dataiso       dataeur
023200090707     c                   move      dataeur       vi2drc
023300090707     c                   else
023400090707     c                   clear                   vi2drc
023500090707     c                   endif
023600930812     C*
023700090707     C                   ADD       1             NRR1
023800090707     C                   WRITE     video2
023900020528
024000930812     C                   ENDDO
024100930820     C*
024200930812     C                   ENDSR
024300020125     C*--- OPERAZIONI INIZIALI ---------------------------------------*
024400020125     C     OPERAZ        BEGSR
024500020125     C*---------------------------------------------------------------*
024600020125     C***
024700020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
024800020125     C***
024900020125     C                   TIME                    W0120            14 0
025000020125     C                   MOVE      W0120         WDAT              8 0
025100020125     C*
025200020125     C                   Z-ADD     WDAT          G02DAT
025300020125     C                   MOVEL     *BLANK        G02ERR
025400020125     C                   CALL      'XSRDA8'
025500020125     C                   PARM                    WLBDAT
025600020125     C* UDATE A 8 IN AAAA/MM/GG
025700020125     C                   Z-ADD     G02INV        WUDA
025800020125     C* DATA DEL GIORNO IN GG/MM/AAAA
025900020125     C                   Z-ADD     G02DAT        UDATE8
026000020125     C*
026100020125     C***
026200020125     C* CARICAMENTO TABELLE
026300020125     C***
026400020125     C                   EXSR      CARTAB
026500020125     C*
026600020125     C* PULISCO CODICE PADRONCINO E DESCRIZIONE SOLO A INIZIO PGM
026700020125     C*   IN MODO CHE NELLA PRIMA VIDEATA RESTI IMPOSTATO L'ULTIMO
026800020125     C                   MOVEL     *BLANKS       VIDPDR
026900020125     C                   MOVE      '?'           VIDPDR
027000020528     C                   MOVEL     *BLANKS       despdr
027100020125     C                   ENDSR
027200020125     C*
027300020125     C*--- CARICAMENTO TABELLE ---------------------------------------*
027400020125     C     CARTAB        BEGSR
027500020125     C*---------------------------------------------------------------*
027600020125     C***
027700070307     C* CARICO TABELLA FILIALI contabili
027800020125     C***
027900140117     c********           clear                   trul72ds
028000140117     c********           eval      i72tla = 'L'
028100140117     c********           eval      I72POU = simpou
028200140117     c********           eval      kpjbu = trul72ds
028300140117     C********           CALL      'TRUL72R'
028400140117     C********           PARM                    KPJBA
028500140117     c********           movel     kpjbu         trul72ds
028600140117      *------
028700140117      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
028800140117     c                   clear                   TRUL31DS
028900140117     c                   eval      I31abi = 'AC'
029000140117     c                   eval      I31cpo = simpou
029100140117     c                   call      'TRUL31R'
029200140117     c                   parm                    KPJBA
029300140117     c                   parm                    TRUL31DS
029400140117     c                   if        o31pog > *zeros
029500140117     c                   movea     o31pog        $pog
029600140117     c                   endif
029700140117      *------
029800020125     C***
029900020125     C* VEDO SE SONO SIMFEL O REMOTO
030000020125     C***
030100020125     C     REM           IFEQ      'REM'
030200020125     C     REMFIL        ANDGT     *ZEROS
030300020125     C                   MOVEL     REMFIL        VIDFIL
030400070307     C                   MOVEL     REMFIL        VIDpog            3 0
030500020125     C                   SETON                                        89
030600020125     C***
030700020125     C* CARICO TABELLA FILIALI GESTITE £6
030800020125     C***
030900020125     C                   CLEAR                   DSUL06
031000020125     C                   MOVE      '£6'          D06COD
031100020125     C                   MOVEL     REMFIL        D06KEY
031200020125     C                   MOVEL     DSUL06        KPJBU
031300020125     C*
031400020125     C                   CALL      'TRUL06R'
031500020125     C                   PARM                    KPJBA
031600020125     C                   MOVEL     KPJBU         DSUL06
031700020125     C                   MOVEA     LIN           L6
031800020125     C*
031900020125     C                   ELSE
032000020125     C* SONO SIMFEL
032100030203     C                   MOVEL     SIMpou        VIDFIL
032200020125     C                   ENDIF
032300020125     C*
032400020125     C                   ENDSR
032500020125     C*
032600020125      *--------------------------------------------------
032700020125     c     *inzsr        begsr
032800020125      *--------------------------------------------------
032900020125
033000020125     C     *ENTRY        PLIST
033100020125     C                   PARM                    KPJBA
033200020125     C                   Z-ADD     1             CODUT
033300020125     C                   CALL      'X§PARUT'
033400020125     C                   PARM                    UT§DSE
033500020125     C                   MOVEL     RAGUT         RSUT             20
033600020125     C                   MOVEL     REC80         CNCR80
033700021203     C* ACCESSO fiapd
033800021203     C     Kapdc         KLIST
033900090707     C                   KFLD                    apdtip
034000021203     C                   KFLD                    COMpdr
034100090707     c                   eval      apdtip = 'D'
034200090707     C     *LIKE         DEFINE    adtDDT        WUDA                           *AAAA/MM/GG
034300090707     C     *LIKE         DEFINE    adtDDT        UDATE8                         *GG/MM/AAAA
034400020125     C*
034500090707     C     *LIKE         DEFINE    adtPDR        COMPDR
034600020125     C*
034700020125     c                   endsr
034800020130**   XDTF
034900020130312831303130313130313031
