000100130314     /*PRM dbgview(*source)
000200130314     /*END
000300991105      *------------------------------------------------------------------------*
000400161028      *  Nome programma:  FNlRU6RI2
000500160720      *  Descrizione   :  Interfaccia per richiamo Int. Bolle unica OLD->NEW
000501161031      *                   Per l'interrogazione bolle partenze singola quando
000502161031      *                   richiamata
000503161028      *                   DA USARE compilato come FNLS05R
000504161031      *                   Per l'interrogazione bolle partenze quando richiamata
000505161031      *                   da menù (fnls04R) dovrà continuare a girare l'FNLS05R vero:
000507161031      *                   l'FNLS05R1 sarà l'oggetto corrispondente.
000600160505      *  Data creazione:  05 MAG 2016
000700940712      *------------------------------------------------------------------------*
000800991105     H DECEDIT('0,') DATEDIT(*DMY/)
000900110929     h dftactgrp(*no) actgrp(*caller)
001000940712      *
001100160505     d kpjba         e ds
001200160505     d fidna1ds      e ds
001300160505     d fnlru6ds      e ds
001400160505
001401161028     D PARAM0          DS
001402161028     D  v2caas                 1      4  0
001403161028     D  comlnp                 5      7  0
001404161028     D  v2cnrs                 8      9  0
001405161028     D  v2cnsp                10     16  0
001406161028     D  PA1F03                17     17
001407161028     D  PA1FLG                18     18
001408161028     D  PA0A9                 19     19
001409161028     D  PA0B5                 20     20
001410161028     D  PA075                 21     21
001411161028     D  PA098                 22     22
001412161028     D  PA0RSU                23     42
001413161028     D  PA1CML                43     43
001414161028     D  PA0SIM                44     46  0
001418161028      * PA1GIA indica se il pgm e' richiamato dalle giacenze
001419161028     D  PA1GIA                48     48
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
003524161028
003525161028      * - DS Parametri per ricerca Programmi chiamanti
003526161028     D DSStack         ds
003527161028     D  £Stack                       10    Dim(100)
003528161028
003600160505     d Status         sds
003700160505     d  SDSprm           *parms
003701160518
003702160518     d w_v2cnsp        s                   like(v2cnsp)
003703160607     d savkpjbu        s                   like(kpjbu)
003704160518
003800160505     C     *entry        plist
003900160505     C                   parm                    kpjba
004000160505     c                   parm                    fidna1ds
004001161028      * reperisco i programmi che hanno richiamato questo pgm
004002161028     c                   Call      'XRTVSTKR'
004003161028     C                   Parm                    PStack         1000
004004161028     C                   Parm                    PStartPoint       3 0
004005161028     C                   Parm                    PEntries          3 0
004006161028     c                   Movel     Pstack        DSstack
004007161028     c* Se pgm richiamato da ls04 richiamo ls05 "vero" ridenominato in fnls05r1
004008161115     c* Se però sono in manutenzione bolle partenze devo richiamare il nuovo
004009161115     c                   if        %lookup('FNLS04R':£Stack)>0 and kcdaz<>'LS0A'
004010161028if  1c                   if        SDSprm = 1
004011161028     C                   CALL      'FNLS05R1'
004012161028     C                   PARM                    KPJBA
004013161028x   1c                   else
004014161028     C                   call      'FNLS05R1'
004015161028     C                   parm                    KPJBA
004016161028     C                   parm                    FIDNA1ds
004017161028e   1c                   endif
004018161028     c                   else
004100161028     c                   movel     kpjbu         param0
004101160607     c                   eval      savkpjbu=kpjbu
004102161028
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
005202161028     c                   eval      v2caas_=v2caas
005203161028     c                   eval      comlnp_=comlnp
005204161028     c                   eval      v2cnrs_=v2cnrs
005205161028     c                   eval      v2cnsp_=v2cnsp
005206161028     c                   eval      pa1f03_=pa1f03
005207161028     c* pa1flg:
005208161028     c                   select
005209161028     c                   when      pa1flg='N' or pa1flg='R'
005210161028     c                   eval      pa1flg_='N'
005211161028     c                   when      pa1flg='C'
005212161028     c                   eval      pa1flg_=pa1flg
005213161028     c                   other
005214161028     c                   clear                   pa1flg_
005215161028     c                   endsl
005216161028     C                   EVAL      PA1GIA_=PA1GIA
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
006506161028     c                   movel     param0        kpjbu
006507160607     c
006508161028     c                   endif
007900160505
008000160505     c                   seton                                        lr
