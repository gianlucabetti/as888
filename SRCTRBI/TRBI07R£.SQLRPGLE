000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300161108     Ftntbe01l  IF   E           K DISK
000400161110      *-
000500161110     FBIFDI00F  o  A E             DISK
000600040820     H**********************************************************************
000700161110     d KPJBA         E DS
000800161110     D trbi07ds      e DS                  inz
000900161110     D*
001000161110     d BITDI00F      E DS
001100161110     d FIDSF00F      E DS
001200161110      *
001300161110     d dDSF          E DS
001400161110      *
001500161110     D WrkSqlCmd       S           1024
001600161110     D  data_Oggi      s                   like(G02INV) INZ(0)
001700161110     D*
001800161110     D WLBDAT          DS
001900161110     D  G02DAT                 1      8  0
002000161110     D  G02INV                 9     16  0
002100161110     D  G02ERR                17     17
002200161110     D  G02TGI                18     22  0
002300040820      *
002400161110     D  pdrsav         s              7  0
002500161109     D  data_8         s              8s 0
002600040826     D  almeno_uno     s              1
002700161110     d tmp             s             17
002800040820      *
002900040820     D digits          C                   CONST('0123456789')
003000161108      *****************************************************************
003100161110     C                   SETON                                        LR
003200161108     c* compongo istruzione sql
003300161108     c                   exsr      istruz
003400161108      *
003500161108      *****************************************************************
003600161108      *  poi li rigenera
003700011210     C/EXEC SQL
003800011210     C+ PREPARE S1 FROM :WrkSqlCmd
003900011210     C/END-EXEC
004000011210
004100011210     C/EXEC SQL
004200011210     C+ DECLARE A1 CURSOR FOR S1
004300011210     C/END-EXEC
004400011210
004500011210     C/EXEC SQL
004600011210     C+ OPEN A1
004700011210     C/END-EXEC
004800040820     C                   DOU       SqlCod <> 0
004900040823     c*
005000011205     C/EXEC SQL
005100161110     C+ FETCH NEXT FROM A1 INTO :BITDI00F, :FIDSF00F
005200011205     C/END-EXEC
005300011210     C                   SELECT
005400041007     c*
005500041007     c* a fine file Totali x rotture
005600041007     C                   WHEN      SqlCod = 100
005700041007     c                   if        almeno_uno ='S'
005800161108     c                   exsr      TOTali
005900041007     c                   end
006000041007     c                   leave
006100041007     **
006200041007     C                   WHEN      SqlCod < 0
006300041007     C                   seton                                        H1
006400041007     c                   goto      fine
006500040820     c*
006600041007     C                   OTHER
006700040825     c* Totali x rotture
006800161108     c                   exsr      TOTali
006900040820     c* DETTAGLIO
007000040825     c                   eval      prima_volta = 'N'
007100040826     c                   move      'S'           almeno_uno
007200161110      *
007300161110     c                   clear                   bifdi000
007400161108     c                   exsr      dettaglio
007500161110     c                   write     bifdi000
007600040823      *
007700011210     C                   ENDSL
007800040820
007900011205     C                   ENDDO
008000011205     C/EXEC SQL
008100011205     C+ CLOSE A1
008200011205     C/END-EXEC
008300011205     c*
008400040820     C* totali generali
008500041007     c                   if        almeno_uno ='S'
008600041007     c                   end
008700040823      *
008800041007     c     fine          tag
008900161110     c                   return
009000161110      *
009100040820     C**************************************************************************
009200040820     C* INIZIALIZZAZIONE
009300040820     C**************************************************************************
009400040820     C     *inzsr        BEGSR
009500161108      **
009600161108     C     tbe01         kLIST
009700161108     c                   kFLD                    tbeCOD
009800161108     c                   kFLD                    tbeKE1
009900161108      **
010000161109     C     tbe02         kLIST
010100161109     c                   kFLD                    tbeCOD
010200161109     c                   kFLD                    tbeKE1
010300161109     c                   kFLD                    tbeKE2
010400161110      **-----
010500161110      **
010600040820     C     *ENTRY        PLIST
010700040820     C                   PARM                    KPJBA
010800161109      **
010900161110     c                   clear                   TRBI07DS
011000161110      **
011100161109      *   data passata e diversa da "00000000"
011200161110     c                   if        kpjbu = *blanks
011300161110     c                   movel     *zeros        data8alf          8
011400161110     c                   else
011500161109     c                   movel     kpjbu         data8alf          8
011600161109      ** controlla che vi siano solo dei numeri nel campo
011700161109     c     digits        check     data8alf
011800161109     c                   if        not %Found
011900161110     c                   movel     kpjbu         TRBI07DS
012000161109     c                   end
012100161109     c                   end
012200040820      *
012300040820      *  imposta UDATE di elaborazione
012400040820     C                   move      *date         G02DAT
012500040820     C                   MOVE      *ZEROS        G02INV
012600040820     C                   MOVE      *BLANKS       G02ERR
012700040820     C                   CALL      'XSRDA8'
012800040820     C                   PARM                    WLBDAT
012900040820     C                   Z-ADD     G02INV        data_oggi
013000161108     C                   move      data_oggi     day               8 0
013100040820     C                   TIME                    W0140            14 0
013200161108     C                   MOVEl     W0140         ore               6 0
013300040823      **
013400040825     c                   clear                   prima_volta       1
013500040826     c                   clear                   almeno_uno
013600040820     c*
013700040820     C                   ENDSR
013800040820     C**************************************************************************
013900040820     C* mi compongo l'istruzione sql
014000040820     C**************************************************************************
014100040820     C     istruz        BEGSR                                                  *
014200040820      **
014300040820     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
014400040820     C                   EVAL      WrkSqlCmd
014500040820     C                             =
014600161110     C                             'SELECT * FROM BITDI00F JOIN FIDSF00F ON '  +
014700161110     c                             'TDIFGS = DSFFGS and TDINFV = DSFNFV AND '  +
014800161110     c                             'DSFNPG = 4 '
014900161109      ***
015000161109      * Solo se deve rigenerare la data
015100170421     c                   if           i07datada <> 0
015200161109     C                   EVAL      WrkSqlCmd
015300161109     C                             =  %trim(WrkSqlCmd) + '  '                  +
015400170421     c                             'WHERE TDIDFV= ' + %trim(%char(i07datada))
015500161109     c                   end
015600161109      *
015700040820     C                   ENDSR
015800020326     C**************************************************************************
015900161108      *   esegue il dettaglio
016000040820     C**************************************************************************
016100161108     C     dettaglio     begSR
016200161109      *
016300161110     c                   z-add     tdiFGS        FDIfgs
016400161110     c                   z-add     tdiDFV        FDIdfv
016500161110     c                   z-add     tdiNFV        FDInfv
016600161110     c                   movel     dsfTRD        FDItrd
016700161110     c                   movel     dsfPDAAS4     FDIpdaas4
016800161110     c                   z-add     dsfDTORIN     FDIdtorin
016900161108     c                   eval      tmp= %char(%timestamp: *iso0)
017000161110     c                   eval      FDIDatora  =   tmp
017100161110      * decodifiche
017200161110     c                   if        dsfPDAAS4 = 'P'
017300161110     c                   movel     'PALMARE'     FDIpdad
017400161110     c                   else
017500161110     c                   movel     'AS400  '     FDIpdad
017600161110     c                   end
017700161110      *
017800161110     c                   movel     'DSF'         tbecod
017900161110     c                   movel(p)  dsfTRD        tbeke1
018000161110     c     tbe01         chain     tntbe01l
018100161110     c                   if        %Found(tntbe01l)
018200161110     c                   movel     tbeuni        dDSF
018300161110     c                   movel     D§DSFDES      FDItrdd
018400161110     c                   end
018500161109      *
018600040820     C                   ENDSR
018700040825     C**************************************************************************
018800040825      *
018900040825     C**************************************************************************
019000161108     C     TOTali        begSR
019100040826      *
019200161108     c* ROTTURA chiave
019300161110     c                   if        TDIpdr <> pdrsav or
019400040825     C                             SqlCod = 100
019500040831     c                   if        almeno_uno ='S'
019600161108     c                   exsr      rot_key
019700161108     c                   end
019800161110     c                   eval      pdrsav =  TDIpdr
019900040826     c                   end
020000040825      *
020100040825     C                   ENDSR
020200040820     C**************************************************************************
020300161108     C     rot_key       begSR
020400040823      *
020500040823     C* totale
020600040825     c                   if        prima_volta ='N'
020700040825     c                   end
020800040820      *
020900040820     C                   ENDSR
021000040820     C**************************************************************************
