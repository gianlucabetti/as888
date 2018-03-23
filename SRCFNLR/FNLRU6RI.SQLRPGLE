000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000400160607      *  Nome programma:  FNlRU6RI
000500160720      *  Descrizione   :  Interfaccia per richiamo Int. Bolle unica OLD->NEW
000501160720      *                   DA USARE compilato come FNLR36R
000600160505      *  Data creazione:  05 MAG 2016
000700940712      *------------------------------------------------------------------------*
000800991105     H DECEDIT('0,') DATEDIT(*DMY/)
000900110929     h dftactgrp(*no) actgrp(*caller)
001000940712      *
001100160505     d kpjba         e ds
001200160505     d fidna1ds      e ds
001300160505     d fnlru6ds      e ds
001301161108     d Azuteds       e ds                  extname(Azute00f)
001302161108     d dDatiute      e ds
001303161108     D TIBS34DS      E DS
001400160505
001500940712     D PARAM1          DS
001600940714     D  V2CAAS                14     17  0
001700940712     D  COMLNP                18     20  0
001800940714     D  V2CNRS                21     22  0
001900940714     D  V2CNSP                23     29  0
002000940712     D  PA1F03                30     30
002100940713     D  PA1FLG                31     31
002200940713     D  PA1PGM                32     41
002300940714     D  PA1DSA                42     61
002400940809     D  PA1CML                62     62
002500940720     D  PA1DSP                63     70  0
002600940720     D  PA1RMN                71     85  0
002700940720     D  PA1LNA                86     88  0
002800940802     D  PA1LN1                89    108
002900940802     D  PA1RSD               109    143
003000941213     D* SE RICH DA PGM GIAC. = 1
003100941213     D  PA1GIA               144    144
003200941227     D  PA1FEL               145    147  0
003300950628     D  PA1TBO               148    148
003400110929     d  pa1rma               149    163
003500110929     d  pa1xco               164    164
003501160607
003502160607     d* parametri per fnlru6r1
003503160607     D PARAMU6ds1      DS
003504160607     d** INPUT
003505160607     D  V2CAAS_               14     17  0
003506160607     D  COMLNP_               18     20  0
003507160607     D  V2CNRS_               21     22  0
003508160607     D  V2CNSP_               23     29  0
003509160607     d** OUTPUT
003510160607     D  PA1F03_               30     30
003511160607     d** INPUT
003512160607     D  PA1FLG_               31     31
003513160607     D*    SE RICH DA PGM GIAC. = 1
003514160607     D  PA1GIA_              144    144
003515160607     d** OUTPUT
003516160607     d  pa1rma_              149    163                                         x F22=PerfectOrder
003517160607     d  pa1xco_              164    164                                         x F22=PerfectOrder
003518160607     d** INPUT
003519160607     d*   Tipo lancio: "C" -           CHIUSO CON LR
003520160607     d*                "L" - ELABORO E CHIUDO CON LR
003521160607     d*                " " - ELABORO E CHIUDO IN RETRN
003522160607     d  partla               165    165
003523160607     d  parflo               166    256
003600160505     d Status         sds
003700160505     d  SDSprm           *parms
003701160518
003702160518     d w_v2cnsp        s                   like(v2cnsp)
003703160607     d savkpjbu        s                   like(kpjbu)
003704160518
003800160505     C     *entry        plist
003900160505     C                   parm                    kpjba
004000160505     c                   parm                    fidna1ds
004100160505     c                   movel     kpjbu         param1
004101160607     c                   eval      savkpjbu=kpjbu
004102161108
004103161108     c     *dtaara       define    §azute        azuteds
004104161108     c     *dtaara       define    §datiute      ddatiute
004105161108     c                   in(E)     *dtaara
004106161108    1c                   If        %error  or RSUT = *blanks
004107161108     c                   Clear                   Tibs34ds
004108161108     c                   Call      'TIBS34R'
004109161108     c                   Parm                    Tibs34ds
004110161108     c                   In        *dtaara
004111161108    1c                   EndIf
004200160505
004300160505if  6c                   if        SDSprm  > 1
004400160505     c                   clear                   fnlru6ds
004500160505     c                   eval      ILRU6RA  ='S'
004600160505     c                   eval      ELRU6INS =IA1INS
004700160505     c                   eval      ILRU6ANT =IA1ANT
004800160505     c                   eval      ILRU6NUT =IA1NUT
004900160505     c                   eval      ILRU6PDT =IA1PDT
005000160505     c                   eval      OLRU6TOR =IA1TOR
005100160505     c                   eval      OLRU6OGG =IA1OGG
005200160505     c                   endif
005201160721
005202160518     c                   monitor
005203160518     c                   eval      w_v2cnsp=v2cnsp
005204160518     c                   on-error
005205160518     c                   clear                   w_v2cnsp
005206160518     c                   endmon
005207160721     c* INTERROGAZIONE DI UNA SPECIFICA BOLLA
005400160518     c                   if        w_v2cnsp>0
005401160607     c                   movel     param1        paramu6ds1
005402160607     c                   eval      partla='L'
005403160607     c                   clear                   parflo
005404160607     c                   eval      kpjbu=paramu6ds1
005500160505if  6c                   if        SDSprm  > 1
005600160505     c                   call      'FNLRU6R1'
005700160505     c                   parm                    kpjba
005800160505     c                   parm                    fnlru6ds
005900160505     c                   eval      IA1INS = ELRU6INS
006000160505     c                   eval      IA1TOR = OLRU6TOR
006100160505     c                   eval      IA1OGG = OLRU6OGG
006200160505     c                   else
006300160505     c                   call      'FNLRU6R1'
006400160505     c                   parm                    kpjba
006500160505     c                   endif
006501160607     c                   eval      paramu6ds1=kpjbu
006502160720     c****               eval      kpjbu=savkpjbu
006503160607     c                   eval      pa1f03=pa1f03_
006506160720     c                   movel     param1        kpjbu
006507160607     c
006600160505     c                   else
006601160721     c* INTERROGAZIONE CON SELEZIONE
006700161108if  6c                   if        SDSprm  > 1  or
006701161108     c                             (pa1flg='2' or pa1flg='M')
006702161108     c                   if        pa1flg='2' or pa1flg='M'
006703161108     c                   clear                   fnlru6ds
006704161108     c                   eval      ilru6lna=dutpou
006705161108     c                   endif
006800160505     c                   call      'FNLRU6R'
006900160505     c                   parm                    kpjba
007000160505     c                   parm                    fnlru6ds
007001161108     c                   if        sdsprm>1
007100160505     c                   eval      IA1INS = ELRU6INS
007200160505     c                   eval      IA1TOR = OLRU6TOR
007300160505     c                   eval      IA1OGG = OLRU6OGG
007301161108     c                   endif
007400160505     c                   else
007500160505     c                   call      'FNLRU6R'
007600160505     c                   parm                    kpjba
007700160505     c                   endif
007800160505     c                   endif
007900160505
008000160505     c                   seton                                        lr
