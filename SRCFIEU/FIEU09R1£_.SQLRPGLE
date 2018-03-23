000100140917      **-----------------------------------------------------------------***
000200140917      *  STAMPA il WorkFile
000300140917      **-----------------------------------------------------------------***
000400040820     H DECEDIT('0,') DATEDIT(*dmy.)
000500050510      **********************************************************************
000600140916-->  fTISE8CP   o    E             PRINTER OFLIND(*in66)
000700140916      *
000800050510      **-----------------------------------------------------------------***
000900040820     d KPJBA         E DS
001000050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001100110511     D CNCR42        E DS
001200040820     D*
001300140916-->  D DsPassaggio   e DS                  EXTNAME(TISE8BDS)
001400140917     D TISE8CF       e DS                  EXTNAME(WFTISE8CF)
001500040820     D*
001600040820     D WLBDAT          DS
001700040820     D  G02DAT                 1      8  0
001800040820     D  G02INV                 9     16  0
001900040820     D  G02ERR                17     17
002000040820     D  G02TGI                18     22  0
002100040820      *
002200040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002300040826     D  almeno_uno     s              1
002400050510      *
002500050511     D*-------------
002600050511     D* Reperimento nome PGM
002700050511     D STATUS         SDS           333
002800050511     D  DSPGM            *PROC
002900050511     D*-------------
003000040820     D digits          C                   CONST('0123456789')
003100050511      * ?------------------------------------------------------------------ */
003200140917      * ? Stampa quanto elaborato e memoprizzato sul workfile.
003300050511      * ?------------------------------------------------------------------ */
003400050510     C/EXEC SQL
003500050510     C+ Declare A1 Cursor for
003600140917     C+ select * from WFTISE8CF
003700140917     C+   order by  SE8CAGU, SE8CAGE, SE8CKSU, SE8CKSC, SE8CNZD
003800050510     C/END-EXEC
003900050510      *          apertura cursore
004000050510     C/EXEC SQL
004100050510     C+ OPEN A1
004200050510     C/END-EXEC
004300050510     c                   clear                   almeno_uno
004400050510      **
004500050510     C                   DOU       SqlCod <> 0
004600050510      **         lettura cursore
004700050510     C/EXEC SQL
004800140917     C+ FETCH NEXT FROM A1 INTO :TISE8CF
004900050510     C/END-EXEC
005000050510     C                   SELECT
005100050510     **
005200050510     C                   WHEN      SqlCod = 100
005300050510     c* a fine file
005400050510     c                   if        almeno_uno = 'S'
005500140916     c                   exsr      daFare_a_EoF
005600050510     c                   else
005700050510     c                   end
005800050510     c                   leave
005900050510     c*
006000050510     C                   WHEN      SqlCod < 0
006100050510     C                   seton                                        H1
006200050510     c                   goto      fine
006300050510     c*
006400050510     C                   OTHER
006500050510     **
006600050510     c* DETTAGLIO
006700050510     c* riga   dettaglio
006800140916     c                   exsr      ElabRig_det
006900050510     C                   ENDSL
007000050510
007100050510     C                   ENDDO
007200050510     C/EXEC SQL
007300050510     C+ Close A1
007400050510     C/END-EXEC
007500050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
007600041007     c     fine          tag
007700020326     C                   SETON                                        LR
007800050511     ?****************************************************************************
007900140916      *?   Elabora Riga File di Work
008000050511     ?****************************************************************************
008100140916     c     ElabRig_det   begsr
008200050510      *
008300050511     c                   move      'S'           almeno_uno
008400140917      *
008500140917     c                   eval      PDIST    =  SE8CDIV
008600140917     c                   eval      PAREA    =  SE8CARE
008700140917     c                   eval      PFIL     =  SE8CFIL
008800140917     c                   eval      PAGENT   =   SE8CAGE
008900140917     c                   eval      PAGEUNI  =   SE8CAGU
009000140917     c                   eval      PDAGEUNI =   SE8CAGUD
009100140917     c                   eval      PCLIUNI  =   SE8CKSU
009200140917     c                   eval      PDCLIUNI =   SE8CKSUD
009300140917     c                   eval      PCLIEN   =   SE8CKSC
009400140917     c                   eval      PCLV     =   SE8CCLV
009500140917     c                   eval      PDCLIEN  =   SE8CKSCD
009600140917     c                   eval      PNAZD    =   SE8CNZD
009700140917     c                   eval      PTARDPD  =   SE8CTDPD
009800140917     c                   eval      PTSPED1  =  SE8CTOT1
009900140917     c                   eval      PTSPED2  =  SE8CTOT2
010000140917     c                   eval      PTSPED3  =  SE8CTOT3
010100140917      *
010200140917     c                   exsr      PRT_rig_det
010300140916      *
010400140916     c                   endsr
010500140916     ?****************************************************************************
010600140916      *?   operazioni Finali dopo aver finito il ciclo di lettura SQL
010700140916     ?****************************************************************************
010800140916     c     daFare_a_EoF  begsr
010900140916      *
011000140917     c                   If         almeno_uno <> 'S'
011100140917     c                   exsr      PRT_NO_Dati
011200140917     c                   end
011300140916      *
011400140916     c                   endsr
011500050511     ?****************************************************************************
011600050511      *?   Stampa      File di Work Una riga x P.O.
011700050511     ?****************************************************************************
011800050511     c     PRT_rig_det   begsr
011900050511      *
012000050511     c                   if        *in66
012100050511      *                 * ------------------ *
012200050511     c                   write     testa
012300050511      *                 * ------------------ *
012400050511     c                   setoff                                       66
012500050511     c                   end
012600050511      *
012700050511      *                 * ------------------ *
012800050511     c                   write     dettagli
012900050511      *                 * ------------------ *
013000050511      *
013100050511     c                   endsr
013200050511     ?****************************************************************************
013300050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
013400050511     ?****************************************************************************
013500050511     c     PRT_NO_Dati   begsr
013600050511      *
013700050511     c                   if        *in66
013800050511      *                 * ------------------ *
013900050511     c                   write     testa
014000050511      *                 * ------------------ *
014100050511     c                   setoff                                       66
014200050511     c                   end
014300050511      *
014400050511      *                 * ------------------ *
014500050511     c                   write     nodati
014600050511      *                 * ------------------ *
014700050511      *
014800050511     c                   endsr
014900050511     ?****************************************************************************
015000050511      *?   INIZIALIZZAZIONE
015100050511     ?****************************************************************************
015200040820     C     *inzsr        BEGSR
015300140916     C*
015400040820     C     *ENTRY        PLIST
015500040820     C                   PARM                    KPJBA
015600050510     c                   movel     kpjbu         DsPassaggio
015700050511      **
015800050511     C                   Z-ADD     1             CODUT
015900050511     C                   CALL      'X§PARUT'
016000050511     C                   PARM                    UT§DSE
016100110511     C                   MOVEL     REC42         CNCR42
016200110511     C                   MOVEL     simRGS        RSUT             20
016300041103      *
016400040820      *  imposta UDATE di elaborazione
016500040820     C                   move      *date         G02DAT
016600040820     C                   MOVE      *ZEROS        G02INV
016700040820     C                   MOVE      *BLANKS       G02ERR
016800040820     C                   CALL      'XSRDA8'
016900040820     C                   PARM                    WLBDAT
017000140917      *
017100040820     C                   Z-ADD     G02INV        data_oggi
017200050511     C                   move      G02dat        day
017300040820     C                   TIME                    W0140            14 0
017400050511     C                   MOVEl     W0140         ore
017500140916      **
017600050510     c                   move      PASdti        dadata            8 0
017700050510     c                   move      PASdtf        adata             8 0
017800040823      **
017900050511     c                   seton                                        66
018000040826     c                   clear                   almeno_uno
018100050511     C                   MOVEL     DSPGM         NOMPGM
018200050511      **
018300050511     C                   move      *zeros        G02DAT
018400050511     C                   MOVE      dadata        G02INV
018500050511     C                   MOVE      '3'           G02ERR
018600050511     C                   CALL      'XSRDA8'
018700050511     C                   PARM                    WLBDAT
018800050511     C                   Z-ADD     G02Dat        PDadata
018900050511      **
019000050511     C                   move      *zeros        G02DAT
019100050511     C                   MOVE      adata         G02INV
019200050511     C                   MOVE      '3'           G02ERR
019300050511     C                   CALL      'XSRDA8'
019400050511     C                   PARM                    WLBDAT
019500050511     C                   Z-ADD     G02dat        PAdata
019600050510     c*
019700040820     C                   ENDSR
019800140916     ?****************************************************************************
