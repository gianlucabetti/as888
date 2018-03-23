000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300080730      * FIDG49r
000400950315      *---------*                                                    *
000500080730      *         - se esiti pda CET in + distinte                     *
000600000426      *                                                              *
000700950315      *--------------------------------------------------------------*
000800080730     FFidg49d   CF   E             WORKSTN SFILE(LR53S01:NRR3)
000900131204     ffipct01l  uf   e           k disk
001000080805     ffiapd01l  if   e           k disk
001100950320      *----------------------------------------------------------------*
001200950320      *    s c h i e r e                                               *
001300950320      *----------------------------------------------------------------*
001400950315     D KPJBA         E DS
001500080728     D fidg46ds      E DS
001501131106     D FIPCTCETDS    E DS
001502131106     d dsdataora       ds
001503131106     d   datains                      8s 0
001504131106     d   oramin                       4s 0
001505131106     d   second                       2s 0
001506131106     d   dataora               1     14s 0
001600950315      *---------------------------------------------------------------*
001700950315      * FLUSSO PRINCIPALE                                             *
001800950315      *---------------------------------------------------------------*
001900950320     C*
002000080728     C*  Caricamento subfile
002100080508     C                   EXSR      INZS01
002200080728     c*  Se il subfile è vuoto esco
002300080730     C                   IF        nrr3 = 0
002400080728     c                   seton                                        lr
002500080728     c                   return
002600080526     C                   END
002700080730     c* se solo un record lancio ed esco
002800080730     C                   IF        nrr3 = 1
002900080730     C                   Z-ADD     V3Cfgs        dg46fgsi
003000080730     C                   Z-ADD     V3Cndc        dg46ndci
003001131104     C                   move      V3Ctrd        dg46trdi
003100080807     C                   Z-ADD     V3Ccodaut     dg46codaut
003200080730     C                   MOVEL(P)  fidg46ds      KPJBU
003300080730     c                   call      'FIDG46R'
003400080730     c                   parm                    kpjba
003500110201     C                   eval      dg46erro ='2'
003600080730     c                   seton                                        lr
003700080730     c                   return
003800080730     C                   END
003900080701     c*
004000080728     C                   Z-ADD     Dg46aasi      v3Caas
004100080728     C                   Z-ADD     Dg46lnpi      v3Clnp
004200080728     C                   Z-ADD     Dg46nrsi      v3Cnrs
004201131104     C                   move      Dg46trdi      v3Ctrd
004300080728     C                   Z-ADD     Dg46nspi      v3Cnsp
004400080512     C                   Z-ADD     1             NRR3
004500080730     C                   WRITE     Z1
004600950315      *  Loop gestione subfile degli errori
004700080508     C                   DO        *hival
004800080508      *
004900080508     C                   EXFMT     LR53C01
005000080508      *  Fine Lavoro
005100080508     C     *INKC         IFEQ      '1'
005200080728     C                   MOVEL     '1'           dg46erro
005300080508     C                   leave
005400080508     C                   END
005500080508      *  Guida
005600080711     C     *INKL         IFEQ      '1'
005700080728     C                   MOVEL     '2'           dg46erro
005800080508     C                   leave
005900080508     C                   END
006000080508      *  Loop di controlli
006100080508     C                   EXSR      CTR01
006200080508     c
006300080508     C                   ENDdo
006400080508     c*
006500080728     c                   movel     fidg46ds      kpjbu
006600080728     c                   seton                                        lr
006700950317      *---------------------------------------------------------------*
006800950317      * Gestione videata subfile errori                               *
006900950317      *---------------------------------------------------------------*
007000080508     C     INZS01        BEGSR
007100080508     C                   SETOFF                                       202190
007200080508     C                   WRITE     LR53C01
007300950317     C                   SETON                                        2021
007400080512     C                   Z-ADD     0             NRR3              4 0
007500080728     c*
007600080730     c     kpct          setll     fipct01l
007700080710     c                   do        *hival
007800131204     c     kpct          reade(n)  fipct01l
007900080730     c                   if        %eof(fipct01l)
008000080710     c                   leave
008100080710     c                   end
008101131104     c                   if        pcttrd <> 'CET' and
008102131104     c                             pcttrd <> 'CEP'
008103131104     c                   iter
008104131104     c                   endif
008105131206     c                   if        dg46ndci <> 0 and
008106131206     c                             pctndc <> dg46ndci
008107131206     c                   iter
008108131206     c                   endif
008109131104      *
008200080730     C                   Z-ADD     pctfgs        v3Cfgs
008300080730     C                   Z-ADD     pctndc        v3Cndc
008301131104     C                   move      pcttrd        v3Ctrd
008400080805     C                   Z-ADD     pctcodaut     v3Ccodaut
008401131106     c                   z-add     pctdtorin     dataora
008402131106     c                   z-add     oramin        v3cora
008403131106     C                   movel     pctdati       FIPCTCETDS
008404131106     c                   if        §pctcmc <> *blank
008405131106     C                   movel     §pctcmc       v3ccmc
008406131106     c                   else
008407131107     c                   move      '  C'         v3ccmc
008408131106     c                   endif
008500080805     c     kapd          chain     fiapd01l
008600080805     c                   if        %found(fiapd01l)
008700080805     c                   eval      v3dcodaut = apdrsc
008800080805     c                   else
008900080805     c                   eval      v3dcodaut = *all'?'
009000080805     c                   end
009100080728     C                   ADD       1             NRR3
009200080728     C                   WRITE     LR53S01
009300080728     c                   enddo
009400080728     c*
009500080707     C                   ENDSR
009600080508      *---------------------------------------------------------------*
009700080508      *  Loop di errori  subfile errori                               *
009800080508      *---------------------------------------------------------------*
009900080509     C     CTR01         BEGSR
010000131204     c                   setoff                                       284041
010100950315      *
010200080509     C                   READC     LR53S01                                32
010300980325      *
010400950317     C     *IN32         DOWEQ     '0'
010500950320     C     *IN28         ANDEQ     '0'
010600080704     c*
010700080701     c                   select
010800080730      *  scelta distinta
010900080730     C                   when      v3csce = '1'
011000080730     C                   Z-ADD     V3Cfgs        dg46fgsi
011100080730     C                   Z-ADD     V3Cndc        dg46ndci
011200080807     C                   Z-ADD     V3Ccodaut     dg46codaut
011201131105     C                   move      V3Ctrd        dg46trdi
011300080730     C                   MOVEL(P)  fidg46ds      KPJBU
011400080730     c                   call      'FIDG46R'
011500080730     c                   parm                    kpjba
011600080730     c                   movel     kpjbu         fidg46ds
011800080730     c                   if        dg46erro =  '1'
011900080728     c                   seton                                        lr
012000080728     c                   return
012100080730     c                   endif
012200080730     c                   clear                   v3csce
012201131204      * cancellazione
012202131204     C                   when      v3csce = '4' and dg46canc ='S'
012203131209     C                             and *inkf
012204131204     c                   if        v3ctrd = 'CEP'
012205131204     c     kpctcep       chain     fipct01l
012206131204     c                   if        %found(fipct01l)
012207131204     c                   delete    fipct000
012208131206     c                   clear                   v3csce
012209131206     c                   move      'C'           dg46canc
012210131206     c                   movel     fidg46ds      kpjbu
012211131206     c                   exsr      inzs01
012212131206     c                   leavesr
012213131204     c                   endif
012214131204     c                   else
012215131204     c                   seton                                        4128
012216131204     c                   write     z1
012217131204     C                   endif
012221131204     c                   clear                   v3csce
012300080509     C                   ENDsl
012400980325      *  Aggiorno subfile
012500950320     C                   SETON                                        22
012600080707     C                   UPDATE    LR53s01
012700950320     C                   SETOFF                                       22
012800080711     C  N28              READC     LR53S01                                32
012900080509     C                   ENDdo
013000080715     c*
013100080715     c                   z-add     1             nrr3
013200950317     C*
013300950317     C                   ENDSR
013400080509     C*-----------------------------------------------------*
013500080509     C     *INZSR        BEGSR
013600080509     C*-----------------------------------------------------*
013700080508      *
013800080508     C     *ENTRY        PLIST
013900080728     C                   PARM                    kpjba
014000080728     c                   movel     kpjbu         fidg46ds
014001131204     c                   if        dg46canc  = 'S'
014002131204     c                   seton                                        42
014003131204     c                   endif
014100080730     c     kpct          klist
014200080728     c                   kfld                    dg46aasi
014300080728     c                   kfld                    dg46lnpi
014400080728     c                   kfld                    dg46nrsi
014500080728     c                   kfld                    dg46nspi
014501131204     c     kpctcep       klist
014502131204     c                   kfld                    v3Caas
014503131204     c                   kfld                    v3Clnp
014504131204     c                   kfld                    v3Cnrs
014506131204     c                   kfld                    v3Cnsp
014507131204     c                   kfld                    v3Ctrd
014700080805     c     kapd          klist
014800080805     c                   kfld                    apdtip
014900080805     c                   kfld                    v3ccodaut
015100080805     c                   eval      apdtip = 'A'
015200080515     c*
015300080509     C                   ENDSR
015400080515
