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
001100160916     d tibs42ds      e ds
001200160916     d uteautds      e ds
001300940930     D*
001400921102     D WLBDAT          DS
001500940906     D  G02DAT                 1      8  0
001600940906     D  G02INV                 9     16  0
001700940906     D  G02ERR                17     17
001800940906     D  G02TGI                18     22  0
001900940504     D                 DS
002000940906     D  AA                     1      4  0
002100940906     D  MM                     5      6  0
002200940906     D  GG                     7      8  0
002300940906     D  DATA                   1      8  0
002400000000     D KPJBA         E DS
002500940919     D  REM                    1      3
002600940919     D  REMFIL                 4      6
002700900320     D CNCR80        E DS
002800021203     D fnlv24ds      E DS
002900900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003000931115     D  TCU                  398    697
003100931115     D                                     DIM(50)                              TAB FLAG CNT
003200931115     D  KCU                  698    847P 0
003300931115     D                                     DIM(50)                              TAB CAPO CONTO
003400931115     D                                     PACKEVEN
003500931115     D  DTU                  848    895P 0
003600931115     D                                     DIM(12)                              DATE PARAM
003700931115     D                                     PACKEVEN
003800020129     d dataiso         s               d   datfmt(*iso)
003900020129     d dataeur         s               d   datfmt(*eur)
004000941005     D*
004100020130     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)            GG DEI MESI
004200930811     C*****************************************************************
004300930811     C*---------------------------------------------------------------*
004400930928     C* ESEGUO OPERAZIONI INIZIALI
004500930928     C                   EXSR      OPERAZ
004600090707     C                   EXSR      srvideo1
004700930903     C*
004800930820     C                   SETON                                        LR
004900930928     C*
005000930811     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
005100090707     C     srVIDEO1      BEGSR
005200011109     C*---------------------------------------------------------------*
005300930930     C*
005400020124     C                   EXSR      PULSF
005500930907     C*
005600930907     C     FOR01         TAG
005700090707     C                   EXFMT     video1
005800011127     C                   SETOFF                                       045386
005900930929     C*
006000930811     C** CMD3  - FINE LAVORO
006100931013     C   KC              GOTO      ENDVD1
006200931013     C*
006300930907     C                   EXSR      CONTR1
006400930907     C   90              GOTO      FOR01
006500020528     c                   exsr      carsf
006600020528     c                   do        *hival
006700090707     c                   if        nrr1 > 0
006800090707     C                   SETON                                        91
006900090707     c                   else
007000090707     c                   setoff                                       91
007100090707     c                   end
007200090707     c                   z-add     1             nrr1
007300090707     C                   WRITE     video2z
007400090707     C                   EXFMT     video2c
007500020528     C   kl              EXSR      PULSF
007600020528     c   kl              goto      for01
007700020528     c                   enddo
007800930811     C*
007900930907     C     ENDVD1        ENDSR
008000930811     C*
008100930811     C*--- CONTROLLI DELLA PRIMA VIDEATA -----------------------------*
008200930811     C     CONTR1        BEGSR
008300011109     C*---------------------------------------------------------------*
008400940920     C****  CODICE FILIALE  ****
008500950113     C* FILIALE OBBLIGATORIA
008600090707     c                   setoff                                       8340
008700090720     c                   clear                   msgv
008800950113     C     VIDFIL        IFEQ      0
008900950113     C                   SETON                                        8390
009000950113     C                   GOTO      ENDCT1
009100950113     C                   ENDIF
009200950113     C*
009300160916     C* contyrollo autorizzazione
009400160916     c                   clear                   tibs42ds
009500160916     c                   movel     vidfil        i42pge
009600160916     c                   call      'TIBS42R'
009700160916     c                   parm                    tibs42ds
009800160916     c                   movel     o42uni        uteautds
009900160916     c                   if        o42err <> ' ' or §AffMTC <> 'S'
010000160916     c                   seton                                        8390
010100160916     c                   leavesr
010200160916     c                   endif
010300940920     C*
010400930811     C****  CODICE PADRONCINO  ****
010500930908     C     '?'           SCAN      VIDPDR                                 90
010600930811     C*
010700930811     C     *IN90         IFEQ      *ON
010800021203     C                   Z-ADD     VIDFIL        d24FIL
010900021203     C                   Z-ADD     0             d24pdr
011000021203     C                   MOVEL     'R'           d24FLG
011100090707     C                   MOVEL     'D'           d24tip
011200021203     C                   MOVEL     *BLANKS       d24rsc
011300021203     C                   MOVEL     '$$'          d24ann
011400021203     C                   MOVEL(p)  fnlv24ds      KPJBU
011500940926     C                   CALL      'FNLV24R'
011600930811     C                   PARM                    KPJBA
011700021203     C                   MOVEL     KPJBU         fnlv24ds
011800021203     C                   move      d24pdr        vidpdr
011900021203     C                   MOVEL     d24rsc        despdr
012000020528     C                   end
012100930811     C*
012200930921     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
012300930921     C                   TESTN                   VIDPDR               35
012400090720     C  N35              SETON                                        4090
012500090720     c  n35              eval      msgv= 'Autista annullato o inesistente'
012600930921     C  N35              GOTO      ENDCT1
012700931111     C*
012800940915     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
012900940920     C                   MOVEL     VIDFIL        COMPDR
013000940920     C                   MOVE      VIDPDR        COMPDR
013100021203     C     kapdc         CHAIN     fiapd000                           32
013200940914     C* CODICE PADRONCINO ERRATO
013300090720     c   32              eval      msgv= 'Autista annullato o inesistente'
013400940914     C   32              SETON                                        4090
013500940914     C   32              GOTO      ENDCT1
013600100223     C                   MOVEL     APDRSf        despdr
013700000620     C*
013800930928     C****  CONTROLLO SE ESISTONO TARIFFE PER QUEL PADRONCINO  ****
013900931012     C* IMPOSTO CAMPI DELLA CHIAVE
014000930928     C                   MOVEL     VIDFIL        COMPDR
014100930915     C                   MOVE      VIDPDR        COMPDR
014200020528     C                   MOVEL     ' '           COMSML            1
014300930908     C*
014400930811     C     ENDCT1        ENDSR
014500930811     C*
014600930811     C*
014700930812     C*--- PULIZIA SUBFILE -------------------------------------------*
014800930812     C     PULSF         BEGSR
014900011109     C*---------------------------------------------------------------*
015000090707     C                   SETON                                        90
015100090707     C                   WRITE     video2c
015200090707     C                   SETOFF                                       90
015300930812     C                   ENDSR
015400930812     C*
015500930812     C*--- CARICAMENTO SUBFILE ---------------------------------------*
015600930812     C     CARSF         BEGSR
015700011109     C*---------------------------------------------------------------*
015800090707     C                   Z-ADD     0             NRR1              4 0
015900930812     C*
016000090707     c     compdr        setll     fiadts00
016100020528     c                   do        *hival
016200090707     C     compdr        READE     fiadts00                               31
016300020528     C   31              leave
016400090707     c                   z-add     adtprg        vi2prg
016500090707      *data decorrenza
016600090707     c                   if        adtddt > 0
016700090707     c                   move      adtddt        dataiso
016800090707     c                   move      dataiso       dataeur
016900090707     c                   move      dataeur       vi2ddt
017000090707     c                   else
017100090707     c                   clear                   vi2ddt
017200090707     c                   endif
017300090707      *data scadenza
017400090707     c                   if        adtdst > 0
017500090707     c                   move      adtdst        dataiso
017600090707     c                   move      dataiso       dataeur
017700090707     c                   move      dataeur       vi2dst
017800090707     c                   else
017900090707     c                   clear                   vi2dst
018000090707     c                   endif
018100090707      *data scadenza
018200090707     c                   if        adtduv > 0
018300090707     c                   move      adtduv        dataiso
018400090707     c                   move      dataiso       dataeur
018500090707     c                   move      dataeur       vi2duv
018600090707     c                   else
018700090707     c                   clear                   vi2duv
018800090707     c                   endif
018900090707      *data scadenza
019000090707     c                   if        adtdcn > 0
019100090707     c                   move      adtdcn        dataiso
019200090707     c                   move      dataiso       dataeur
019300090707     c                   move      dataeur       vi2dcn
019400090707     c                   else
019500090707     c                   clear                   vi2dcn
019600090707     c                   endif
019700090707      *data scadenza
019800090707     c                   if        adtdts > 0
019900090707     c                   move      adtdts        dataiso
020000090707     c                   move      dataiso       dataeur
020100090707     c                   move      dataeur       vi2dts
020200090707     c                   else
020300090707     c                   clear                   vi2dts
020400090707     c                   endif
020500090707      *data scadenza
020600090707     c                   if        adtdrc > 0
020700090707     c                   move      adtdrc        dataiso
020800090707     c                   move      dataiso       dataeur
020900090707     c                   move      dataeur       vi2drc
021000090707     c                   else
021100090707     c                   clear                   vi2drc
021200090707     c                   endif
021300930812     C*
021400090707     C                   ADD       1             NRR1
021500090707     C                   WRITE     video2
021600020528
021700930812     C                   ENDDO
021800930820     C*
021900930812     C                   ENDSR
022000020125     C*--- OPERAZIONI INIZIALI ---------------------------------------*
022100020125     C     OPERAZ        BEGSR
022200020125     C*---------------------------------------------------------------*
022300020125     C***
022400020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
022500020125     C***
022600020125     C                   TIME                    W0120            14 0
022700020125     C                   MOVE      W0120         WDAT              8 0
022800020125     C*
022900020125     C                   Z-ADD     WDAT          G02DAT
023000020125     C                   MOVEL     *BLANK        G02ERR
023100020125     C                   CALL      'XSRDA8'
023200020125     C                   PARM                    WLBDAT
023300020125     C* UDATE A 8 IN AAAA/MM/GG
023400020125     C                   Z-ADD     G02INV        WUDA
023500020125     C* DATA DEL GIORNO IN GG/MM/AAAA
023600020125     C                   Z-ADD     G02DAT        UDATE8
023700020125     C*
023800020125     C***
023900020125     C* CARICAMENTO TABELLE
024000020125     C***
024100020125     C                   EXSR      CARTAB
024200020125     C*
024300020125     C* PULISCO CODICE PADRONCINO E DESCRIZIONE SOLO A INIZIO PGM
024400020125     C*   IN MODO CHE NELLA PRIMA VIDEATA RESTI IMPOSTATO L'ULTIMO
024500020125     C                   MOVEL     *BLANKS       VIDPDR
024600020125     C                   MOVE      '?'           VIDPDR
024700020528     C                   MOVEL     *BLANKS       despdr
024800020125     C                   ENDSR
024900020125     C*
025000020125     C*--- CARICAMENTO TABELLE ---------------------------------------*
025100020125     C     CARTAB        BEGSR
025200020125     C* SONO SIMFEL
025300030203     C                   MOVEL     SIMpou        VIDFIL
025400020125     C*
025500020125     C                   ENDSR
025600020125     C*
025700020125      *--------------------------------------------------
025800020125     c     *inzsr        begsr
025900020125      *--------------------------------------------------
026000020125
026100020125     C     *ENTRY        PLIST
026200020125     C                   PARM                    KPJBA
026300020125     C                   Z-ADD     1             CODUT
026400020125     C                   CALL      'X§PARUT'
026500020125     C                   PARM                    UT§DSE
026600020125     C                   MOVEL     RAGUT         RSUT             20
026700020125     C                   MOVEL     REC80         CNCR80
026800021203     C* ACCESSO fiapd
026900021203     C     Kapdc         KLIST
027000090707     C                   KFLD                    apdtip
027100021203     C                   KFLD                    COMpdr
027200090707     c                   eval      apdtip = 'D'
027300090707     C     *LIKE         DEFINE    adtDDT        WUDA                           *AAAA/MM/GG
027400090707     C     *LIKE         DEFINE    adtDDT        UDATE8                         *GG/MM/AAAA
027500020125     C*
027600090707     C     *LIKE         DEFINE    adtPDR        COMPDR
027700020125     C*
027800020125     c                   endsr
027900020130**   XDTF
028000020130312831303130313130313031
