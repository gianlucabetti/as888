000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300161108     Ftntbe01l  IF   E           K DISK
000400161110      *-
000500161110     FBIFDI00F  o  A E             DISK
000600040820     H**********************************************************************
000700161110     d KPJBA         E DS
000800161110     D*
000900161110     d BITDI00F      E DS
001000161110     d FIDSF00F      E DS
001100161110      *
001200161110     d dDSF          E DS
001300161110      *
001400161110     D WrkSqlCmd       S           1024
001500161110     D  data_Oggi      s                   like(G02INV) INZ(0)
001600161110     D*
001700161110     D WLBDAT          DS
001800161110     D  G02DAT                 1      8  0
001900161110     D  G02INV                 9     16  0
002000161110     D  G02ERR                17     17
002100161110     D  G02TGI                18     22  0
002200040820      *
002300161110     D  pdrsav         s              7  0
002400040826     D  almeno_uno     s              1
002500161110     d tmp             s             17
002600040820      *
002700040820     D digits          C                   CONST('0123456789')
002800161108      *****************************************************************
002900161110     C                   SETON                                        LR
003000161108     c* compongo istruzione sql
003100161108     c                   exsr      istruz
003200161108      *
003300161108      *****************************************************************
003400161108      *  poi li rigenera
003500011210     C/EXEC SQL
003600011210     C+ PREPARE S1 FROM :WrkSqlCmd
003700011210     C/END-EXEC
003800011210
003900011210     C/EXEC SQL
004000011210     C+ DECLARE A1 CURSOR FOR S1
004100011210     C/END-EXEC
004200011210
004300011210     C/EXEC SQL
004400011210     C+ OPEN A1
004500011210     C/END-EXEC
004600040820     C                   DOU       SqlCod <> 0
004700040823     c*
004800011205     C/EXEC SQL
004900161110     C+ FETCH NEXT FROM A1 INTO :BITDI00F, :FIDSF00F
005000011205     C/END-EXEC
005100011210     C                   SELECT
005200041007     c*
005300041007     c* a fine file Totali x rotture
005400041007     C                   WHEN      SqlCod = 100
005500041007     c                   if        almeno_uno ='S'
005600161108     c                   exsr      TOTali
005700041007     c                   end
005800041007     c                   leave
005900041007     **
006000041007     C                   WHEN      SqlCod < 0
006100041007     C                   seton                                        H1
006200041007     c                   goto      fine
006300040820     c*
006400041007     C                   OTHER
006500040825     c* Totali x rotture
006600161108     c                   exsr      TOTali
006700040820     c* DETTAGLIO
006800040825     c                   eval      prima_volta = 'N'
006900040826     c                   move      'S'           almeno_uno
007000161110      *
007100161110     c                   clear                   bifdi000
007200161108     c                   exsr      dettaglio
007300161110     c                   write     bifdi000
007400040823      *
007500011210     C                   ENDSL
007600040820
007700011205     C                   ENDDO
007800011205     C/EXEC SQL
007900011205     C+ CLOSE A1
008000011205     C/END-EXEC
008100040820     C* totali generali
008200041007     c                   if        almeno_uno ='S'
008300041007     c                   end
008400040823      *
008500041007     c     fine          tag
008600161110     c                   return
008700161110      *
008800040820     C**************************************************************************
008900040820     C* INIZIALIZZAZIONE
009000040820     C**************************************************************************
009100040820     C     *inzsr        BEGSR
009200161108      **
009300161108     C     tbe01         kLIST
009400161108     c                   kFLD                    tbeCOD
009500161108     c                   kFLD                    tbeKE1
009600161108      **
009700161109     C     tbe02         kLIST
009800161109     c                   kFLD                    tbeCOD
009900161109     c                   kFLD                    tbeKE1
010000161109     c                   kFLD                    tbeKE2
010100161110      **-----
010200161110      **
010300040820     C     *ENTRY        PLIST
010400040820     C                   PARM                    KPJBA
010500161110      **
010600040820      *  imposta UDATE di elaborazione
010700040820     C                   move      *date         G02DAT
010800040820     C                   MOVE      *ZEROS        G02INV
010900040820     C                   MOVE      *BLANKS       G02ERR
011000040820     C                   CALL      'XSRDA8'
011100040820     C                   PARM                    WLBDAT
011200040820     C                   Z-ADD     G02INV        data_oggi
011300161108     C                   move      data_oggi     day               8 0
011400040820     C                   TIME                    W0140            14 0
011500161108     C                   MOVEl     W0140         ore               6 0
011600040823      **
011700040825     c                   clear                   prima_volta       1
011800040826     c                   clear                   almeno_uno
011900040820     c*
012000040820     C                   ENDSR
012100040820     C**************************************************************************
012200040820     C* mi compongo l'istruzione sql
012300040820     C**************************************************************************
012400040820     C     istruz        BEGSR                                                  *
012500040820      **
012600040820     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
012700040820     C                   EVAL      WrkSqlCmd
012800040820     C                             =
012900161110     C                             'SELECT * FROM BITDI00F JOIN FIDSF00F ON '  +
013000161110     c                             'TDIFGS = DSFFGS and TDINFV = DSFNFV AND '  +
013100161110     c                             'DSFNPG = 4 '
013200161109      ***
013300040820     C                   ENDSR
013400020326     C**************************************************************************
013500161108      *   esegue il dettaglio
013600040820     C**************************************************************************
013700161108     C     dettaglio     begSR
013800161109      *
013900161110     c                   z-add     tdiFGS        FDIfgs
014000161110     c                   z-add     tdiDFV        FDIdfv
014100161110     c                   z-add     tdiNFV        FDInfv
014200161110     c                   movel     dsfTRD        FDItrd
014300161110     c                   movel     dsfPDAAS4     FDIpdaas4
014400161110     c                   z-add     dsfDTORIN     FDIdtorin
014500161108     c                   eval      tmp= %char(%timestamp: *iso0)
014600161110     c                   eval      FDIDatora  =   tmp
014700161110      * decodifiche
014800161110     c                   if        dsfPDAAS4 = 'P'
014900161110     c                   movel     'PALMARE'     FDIpdad
015000161110     c                   else
015100161110     c                   movel     'AS400  '     FDIpdad
015200161110     c                   end
015300161110      *
015400161110     c                   movel     'DSF'         tbecod
015500161110     c                   movel(p)  dsfTRD        tbeke1
015600161110     c     tbe01         chain     tntbe01l
015700161110     c                   if        %Found(tntbe01l)
015800161110     c                   movel     tbeuni        dDSF
015900161110     c                   movel     D§DSFDES      FDItrdd
016000161110     c                   end
016100161109      *
016200040820     C                   ENDSR
016300040825     C**************************************************************************
016400040825      *
016500040825     C**************************************************************************
016600161108     C     TOTali        begSR
016700040826      *
016800161108     c* ROTTURA chiave
016900161110     c                   if        TDIpdr <> pdrsav or
017000040825     C                             SqlCod = 100
017100040831     c                   if        almeno_uno ='S'
017200161108     c                   exsr      rot_key
017300161108     c                   end
017400161110     c                   eval      pdrsav =  TDIpdr
017500040826     c                   end
017600040825      *
017700040825     C                   ENDSR
017800040820     C**************************************************************************
017900161108     C     rot_key       begSR
018000040823      *
018100040823     C* totale
018200040825     c                   if        prima_volta ='N'
018300040825     c                   end
018400040820      *
018500040820     C                   ENDSR
018600170421      * ?------------------------------------------------------------------ */
018700170421      *?      X non bloccare in nessun caso il traduttore CLIENTI
018800170421      * ?------------------------------------------------------------------ */
018900170421     C     *pssr         BEGSR
019000170421     C
019100170421     c                   seton                                        LR
019200170421     c                   return
019300170421     C
019400170421     C                   ENDSR
019500170421      * ?------------------------------------------------------------------ */
