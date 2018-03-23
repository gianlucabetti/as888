000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200160916     H* ficn19R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400020528     H*               GESTIONE storico TARIFFE PADRONCINI
000500000000     H*--------------------------------------------------------------*
000600020528     Fficn19D   CF   E             WORKSTN
000700020528     F                                     SFILE(CN19S02:NRR)
000800020528     Ffifgts0f  if   E           K DISK
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
004600020528     C                   EXSR      video1
004700930903     C*
004800930820     C                   SETON                                        LR
004900930928     C*
005000930811     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
005100930811     C     VIDEO1        BEGSR
005200011109     C*---------------------------------------------------------------*
005300930930     C*
005400020124     C                   EXSR      PULSF
005500930907     C*
005600930907     C     FOR01         TAG
005700020528     C                   EXFMT     CN19D01
005800011127     C                   SETOFF                                       045386
005900930929     C*
006000930811     C** CMD3  - FINE LAVORO
006100931013     C   KC              GOTO      ENDVD1
006200931013     C*
006300930907     C                   EXSR      CONTR1
006400930907     C   90              GOTO      FOR01
006500020528     c                   exsr      carsf
006600020528     c                   if        nrr > 0
006700020528     c                   do        *hival
006800020528     c                   z-add     1             rec
006900020528     C                   WRITE     CN19Z02
007000020528     C                   EXFMT     CN19C02
007100020528     C   kl              EXSR      PULSF
007200020528     c   kl              goto      for01
007300020528     c                   enddo
007400020528     c                   else
007500020528     c                   seton                                        55
007600020528     c                   goto      for01
007700020528     c                   endif
007800930811     C*
007900930907     C     ENDVD1        ENDSR
008000930811     C*
008100930811     C*--- CONTROLLI DELLA PRIMA VIDEATA -----------------------------*
008200930811     C     CONTR1        BEGSR
008300011109     C*---------------------------------------------------------------*
008400940920     C****  CODICE FILIALE  ****
008500950113     C* FILIALE OBBLIGATORIA
008600020528     c                   setoff                                       835540
008700950113     C     VIDFIL        IFEQ      0
008800950113     C                   SETON                                        8390
008900950113     C                   GOTO      ENDCT1
009000950113     C                   ENDIF
009100950113     C*
009200160916     C* contyrollo autorizzazione
009300160916     c                   clear                   tibs42ds
009400160916     c                   movel     vidfil        i42pge
009500160916     c                   call      'TIBS42R'
009600160916     c                   parm                    tibs42ds
009700160916     c                   movel     o42uni        uteautds
009800160916     c                   if        o42err <> ' ' or §AUTMTC <> 'S'
009900160916     c                   seton                                        8390
010000160916     c                   leavesr
010100160916     c                   endif
010200160916     C*
010300940920     C*
010400930811     C****  CODICE PADRONCINO  ****
010500930908     C     '?'           SCAN      VIDPDR                                 90
010600930811     C*
010700930811     C     *IN90         IFEQ      *ON
010800021203     C                   Z-ADD     VIDFIL        d24FIL
010900021203     C                   Z-ADD     0             d24pdr
011000021203     C                   MOVEL     'R'           d24FLG
011100030203     C                   MOVEL     tipo          d24tip
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
012400930921     C  N35              SETON                                        4090
012500930921     C  N35              GOTO      ENDCT1
012600931111     C*
012700940915     C* CONTROLLO SE ESISTE NELL'ANAGRAFICA PADRONCINI
012800940920     C                   MOVEL     VIDFIL        COMPDR
012900940920     C                   MOVE      VIDPDR        COMPDR
013000021203     C     kapdc         CHAIN     fiapd000                           32
013100940914     C* CODICE PADRONCINO ERRATO
013200940914     C   32              SETON                                        4090
013300940914     C   32              GOTO      ENDCT1
013400100223     C                   MOVEL     APDRSf        despdr
013500000620     C*
013600930928     C****  CONTROLLO SE ESISTONO TARIFFE PER QUEL PADRONCINO  ****
013700931012     C* IMPOSTO CAMPI DELLA CHIAVE
013800930928     C                   MOVEL     VIDFIL        COMPDR
013900930915     C                   MOVE      VIDPDR        COMPDR
014000020528     C                   MOVEL     ' '           COMSML            1
014100930908     C*
014200930811     C     ENDCT1        ENDSR
014300930811     C*
014400930811     C*
014500930812     C*--- PULIZIA SUBFILE -------------------------------------------*
014600930812     C     PULSF         BEGSR
014700011109     C*---------------------------------------------------------------*
014800930915     C                   SETON                                        87
014900020528     C                   WRITE     CN19C02
015000930915     C                   SETOFF                                       87
015100930812     C                   ENDSR
015200930812     C*
015300930812     C*--- CARICAMENTO SUBFILE ---------------------------------------*
015400930812     C     CARSF         BEGSR
015500011109     C*---------------------------------------------------------------*
015600930812     C                   Z-ADD     0             NRR               4 0
015700930812     C*
015800020528     c     kfgt          setll     fifgts00
015900020528     c                   do        *hival
016000020528     C     kfgt          READE     fifgts00                               31
016100020528     C   31              leave
016200090922     c* verifico il tipo servizio
016300090922     c                   if        tipo = 'A' and
016400090922     c                             (fgttsr ='X' or
016500090922     c                              fgttsr ='P' or
016600090922     c                              fgttsr ='T' )
016700090922     c                   iter
016800090922     c                   end
016900090922     c                   if        tipo = 'C' and
017000090922     c                             (fgttsr ='C' or
017100090922     c                              fgttsr ='R' or
017200090922     c                              fgttsr ='G' )
017300090922     c                   iter
017400090922     c                   end
017500930903     C*
017600930819     C* CODICE E PROGRESSIVO TARIFFA
017700930819     C                   MOVEL     FGTCTR        VI2CTR
017800930819     C                   MOVEL     FGTPRG        VI2PRG
017900011029     C                   MOVEL     FGTdiv        VI2div
018000930819     C*
018100930819     C* GIRO DATA DECORRENZA TARIFFA
018200940906     C                   Z-ADD     FGTDDT        DATA
018300940906     C                   Z-ADD     MM            GGMM              4 0
018400940906     C                   MOVEL     GG            GGMM
018500940906     C                   Z-ADD     AA            VI2DDT
018600940906     C                   MOVEL     GGMM          VI2DDT
018700930819     C* GIRO DATA SCADENZA TARIFFA
018800940906     C                   Z-ADD     FGTDST        DATA
018900940906     C                   Z-ADD     MM            GGMM
019000940906     C                   MOVEL     GG            GGMM
019100940906     C                   Z-ADD     AA            VI2DST
019200940906     C                   MOVEL     GGMM          VI2DST
019300930819     C* GIRO DATA ULTIMA VARIAZIONE
019400090424     c                   if        tipo = 'A'
019500090424     C                   Z-ADD     FGTDCN        DATA
019600090424     C                   Z-ADD     MM            GGMM
019700090424     C                   MOVEL     GG            GGMM
019800090424     C                   Z-ADD     AA            VI2DUV
019900090424     C                   MOVEL     GGMM          VI2DUV
020000090424     c                   else
020100940906     C                   Z-ADD     FGTDUV        DATA
020200940906     C                   Z-ADD     MM            GGMM
020300940906     C                   MOVEL     GG            GGMM
020400940906     C                   Z-ADD     AA            VI2DUV
020500940906     C                   MOVEL     GGMM          VI2DUV
020600090424     c                   end
020700930820     C*
020800930820     C* DESCRIZIONE TARIFFA
020900930819     C                   MOVEL     FGTDCV        VI2DCV
021000020528     c                   select
021100020528     c                   when      fgttsr = 'C'
021200030314     c                   eval      vi2tsr = 'Consegna    '
021300020528     c                   when      fgttsr = 'R'
021400030314     c                   eval      vi2tsr = 'Ritiro      '
021500020528     c                   when      fgttsr = 'G'
021600030314     c                   eval      vi2tsr = 'Giornata    '
021700030314     c                   when      fgttsr = 'X'
021800030314     c                   eval      vi2tsr = 'Mv.Magazzino'
021900030314     c                   when      fgttsr = 'P'
022000030314     c                   eval      vi2tsr = 'Serv.pulizie'
022100030314     c                   when      fgttsr = 'T'
022200030314     c                   eval      vi2tsr = 'M.Casse mob.'
022300020528     c                   endsl
022400930812     C*
022500930812     C                   ADD       1             NRR
022600020528     C                   WRITE     CN19S02
022700020528
022800930812     C                   ENDDO
022900930820     C*
023000930812     C                   ENDSR
023100020125     C*--- OPERAZIONI INIZIALI ---------------------------------------*
023200020125     C     OPERAZ        BEGSR
023300020125     C*---------------------------------------------------------------*
023400020125     C***
023500020125     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
023600020125     C***
023700020125     C                   TIME                    W0120            14 0
023800020125     C                   MOVE      W0120         WDAT              8 0
023900020125     C*
024000020125     C                   Z-ADD     WDAT          G02DAT
024100020125     C                   MOVEL     *BLANK        G02ERR
024200020125     C                   CALL      'XSRDA8'
024300020125     C                   PARM                    WLBDAT
024400020125     C* UDATE A 8 IN AAAA/MM/GG
024500020125     C                   Z-ADD     G02INV        WUDA
024600020125     C* DATA DEL GIORNO IN GG/MM/AAAA
024700020125     C                   Z-ADD     G02DAT        UDATE8
024800020125     C*
024900020125     C***
025000020125     C* CARICAMENTO TABELLE
025100020125     C***
025200020125     C                   EXSR      CARTAB
025300020125     C*
025400020125     C* PULISCO CODICE PADRONCINO E DESCRIZIONE SOLO A INIZIO PGM
025500020125     C*   IN MODO CHE NELLA PRIMA VIDEATA RESTI IMPOSTATO L'ULTIMO
025600020125     C                   MOVEL     *BLANKS       VIDPDR
025700020125     C                   MOVE      '?'           VIDPDR
025800020528     C                   MOVEL     *BLANKS       despdr
025900020125     C                   ENDSR
026000020125     C*
026100020125     C*--- CARICAMENTO TABELLE ---------------------------------------*
026200020125     C     CARTAB        BEGSR
026300020125     C*---------------------------------------------------------------*
026400030203     C                   MOVEL     SIMpou        VIDFIL
026500020125     C*
026600020125     C                   ENDSR
026700020125     C*
026800020125      *--------------------------------------------------
026900020125     c     *inzsr        begsr
027000020125      *--------------------------------------------------
027100020125
027200020125     C     *ENTRY        PLIST
027300020125     C                   PARM                    KPJBA
027400030203     C                   movel     kpjbu         tipo              1
027500090424     c                   if        tipo = 'A'
027600090424     c                   eval      vidtes= 'Copia Fir.'
027700090424     c                   else
027800090424     c                   eval      vidtes= 'Ult. Var.'
027900090424     c                   end
028000020125     C                   Z-ADD     1             CODUT
028100020125     C                   CALL      'X§PARUT'
028200020125     C                   PARM                    UT§DSE
028300020125     C                   MOVEL     RAGUT         RSUT             20
028400020125     C                   MOVEL     REC80         CNCR80
028500020125     C*
028600020125     C* ACCESSO fifgt
028700020125     C     KFGT          KLIST
028800020125     C                   KFLD                    COMPDR
028900020125     C                   KFLD                    COMSML
029000021203     C* ACCESSO fiapd
029100021203     C     Kapdc         KLIST
029200030203     C                   KFLD                    tipo
029300021203     C                   KFLD                    COMpdr
029400020125     C     *LIKE         DEFINE    FGTDDT        WUDA                           *AAAA/MM/GG
029500020125     C     *LIKE         DEFINE    FGTDDT        UDATE8                         *GG/MM/AAAA
029600020125     C*
029700020125     C     *LIKE         DEFINE    FGTPDR        COMPDR
029800020125     C*
029900020125     c                   endsr
030000020130**   XDTF
030100020130312831303130313130313031
