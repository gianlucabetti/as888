000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200141027     h dftactgrp(*yes)
000300950315      *---------*----------------------------------------------------*
000400141021      *         - Gestione distinta dirottamenti          -          *
000500000426      *                                                              *
000600141020      *  Il programma prevede il caricamento a video delle spedizioni
000700141020      *  inserite nella distinta passata come parametro
000800101222      *  per la loro risoluzione                                     *
000900141020      *
001000950315      *--------------------------------------------------------------*
001100141021     FFNLRQ10d  CF   E             WORKSTN SFILE(LRq10s1:NRR3)
001200141023     ffnarb70l  if   e           k disk
001300110119     ffiapd01l  if   e           k disk
001400101223     fazorg01l  if   e           k disk
001500950320      *----------------------------------------------------------------*
001600950320      *    s c h i e r e                                               *
001700950320      *----------------------------------------------------------------*
001800950315     D KPJBA         E DS
001900140130      *
002000140130     d  svkpjbu        s                   like(kpjbu)
002100140130      *
002200140130     c*
002300141022     D fnlry09ds2    E DS
002400101223     D fnlr98ds      E DS
002500141021     D fnlrq9ds      E DS
002600141022     D fnlg82ds      E DS
002700141030     D fnlrj6ds      E DS
002800110119     D fnarbds       E DS                  extname(fnarb00f)
002900080707     d trul06ds      e ds
003000110103     d  lin£                   1     90  0 dim(30)
003100110118     D ddatiute      e ds
003200110118     D azuteds       e ds                  extname(AZUTE00F)
003300110118     D tibs34ds      E DS                  inz
003400141021      * DS per richiamo pgm FNLR36R interrogazione bolle
003500141021     D DSLR36          DS
003600141021     D  P36AAS                14     17  0
003700141021     D  P36LNP                18     20  0
003800141021     D  P36NRS                21     22  0
003900141021     D  P36NSP                23     29  0
004000141021     D  P36F03                30     30
004100141021     D  P36FLG                31     31
004200141023      *interrogazione spunte
004300141023     D ds_PARA04       DS
004400141023     D  p04NFV                 1      5  0
004500141023     D  p04LNP                 6      8  0
004600141023     D  p04LNA                 9     11  0
004700141023     D  p04NRS                12     13  0
004800141023     D  p04NCD                14     20  0
004900141023     D  p04NCA                21     27  0
005000141023     D  p04NPS                28     29  0
005100141023     D  p04NPG                30     30  0
005200141023     D  p04FLG                31     31
005300141023     D  p04PGM                32     38
005400141023     D  p04FGS                39     41  0
005500141023     d  p04bolp               42     44  0
005600141023     d  p04bors               45     46  0
005700141023     d  p04bosp               47     53  0
005800141023     d  p04boan               54     57  0
005900141023     D  PA4ERR                58     58
006000081117     D                SDS
006100081117     D  NOMPGM                 1     10
006200110117     D dtaiso          s               d
006300110117     D dtaeur          s               d   datfmt(*eur)
006400110117     D oggi            S              8  0
006500141028     d NotUsed         S             16A
006600141028     d ReturnSize      S              9B 0
006700141028     d SizeList        S              9B 0
006800141028     d stmrec1         S              4  0
006900141028     d*---
007000141028     d MaxKey          C                   2
007100141028     d Ascendente      C                   1
007200141028     d Discendente     C                   2
007300141028     d Carattere       C                   6
007400141028     d Put             C                   1
007500141028     d EndPut          C                   2
007600141028     d Get             C                   3
007700141028     d Numerico        C                   8
007800141028     d SflRcd          DS
007900141028     d  V3CNCLA
008000141028     d  V3HSCE
008100141028     d  V3HIND
008200141028     d  V3CSCE
008300141028     d  V3CAAS
008400141028     d  V3CLNP
008500141028     d  V3CNRS
008600141028     d  V3CNSP
008700141028     d  V3CRSD
008800141028     d  V3CVOL
008900141028     d  V3CPES
009000141028     d  V3CNCL
009001151125     d  V3Cdis
009100141028     d  Selected                      1A
009200141028
009300141028      /COPY QSYSINC/QRPGLESRC,QLGSORT
009400141028     d QLGKL                         16    DIM(MaxKey)
009500141028     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
009600141028     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
009700141028     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
009800141028     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
009900141028      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
010000141028      /COPY QSYSINC/QRPGLESRC,QUSEC
010100950315      *---------------------------------------------------------------*
010200950315      * FLUSSO PRINCIPALE                                             *
010300950315      *---------------------------------------------------------------*
010400080508     C*  Caricamento subfile errori
010500141028     c     inizio        tag
010600080508     C                   EXSR      INZS01
010700101220     c*  Se il subfile degli errori è vuoto forzo l'uscita dal pgm
010800080526     C     NRR3          IFEQ      0
010900141021     C                   MOVEL     '12'          rq9cmdr
011000141021     C                   MOVE      '1'           rq9erro
011100141021     c                   eval      rq9msgo = 'Nessuna spedizione presente'
011200101223     c                   exsr      srfine
011300080526     C                   END
011400080701     c*
011500141021     C                   Z-ADD     rq9fgsi       v3Clna
011600080710     c                   clear                   v3dlna
011700080710     c     v3clna        chain     azorg01l
011800080710     c                   if        %found(azorg01l)
011900080710     c                   eval      v3dlna = orgdes
012000080710     c                   end
012100141021     C                   Z-ADD     rq9ndci       v3Cnd1
012200141021     C                   Z-ADD     rq9pdr        v3Cpdr
012300101220     c                   clear                   v3dpdr
012400101220     c     kapd          chain     fiapd01l
012500101220     c                   if        %found(azorg01l)
012600101220     c                   eval      v3dpdr = apdrsf
012700101220     c                   end
012800141028     C                   Z-ADD     nrr3          NRRlast           4 0
012900141028     c                   exsr      sort
013000080512     C                   Z-ADD     1             NRR3
013100141023     c     emetti        tag
013200101223      *  gestione subfile degli errori
013300080508     C                   DO        *hival
013400141021     C                   WRITE     LRq10z1
013500141021     C                   EXFMT     LRq10c1
013600080508      *  Fine Lavoro
013700101220     c                   If        *inKC  = '1'
013800141021     C                   MOVEL     '03'          rq9cmdr
013900101222     C                   exsr      srfine
014000080508     C                   END
014100080508      *  Guida
014200101220     c                   If        *inKL  = '1'
014300141021     C                   MOVEL     '12'          rq9cmdr
014400101222     C                   exsr      srfine
014500080508     C                   END
014600141023      *  sel. tutte
014700141023     c                   If        *inKa  = '1'
014800141023     c                   clear                   nr                4 0
014900141023     c                   do        *hival        nr
015000141023     C     nr            chain     LRq10s1                            32
015100141023     c                   if        *in32
015200141023     c                   leave
015300141023     c                   end
015400141023     c                   if        v3hsce = *blank
015500141023     c                   move      '1'           v3csce
015600141023     c                   end
015700141030     c                   setoff                                       8788
015800141030     c                   if        v3hind = '1'
015900141030     c                   seton                                        88
016000141030     c                   endif
016100141030     c                   if        v3cncla > 0
016200141030     c                   seton                                        87
016300141030     c                   endif
016400141023     c                   update    lrq10s1
016500141023     c                   enddo
016600141023     c                   goto      emetti
016700141023     C                   END
016800080508      *  Loop di controlli
016900080508     C                   EXSR      CTR01
017000080701     c   28              iter
017100141023      * per conferme esegue opzioni di aggiornamento
017200141023     c                   if        *inkf
017300141021     c                   exsr      sraggio
017400141028     c                   goto      inizio
017500141023     c                   endif
017600080508     C                   ENDdo
017700141021      * lancia programmi di esecuzione per opzione
017800080508     c*
017900080508     c                   exsr      srfine
018000950317      *---------------------------------------------------------------*
018100950317      * Gestione videata subfile errori                               *
018200950317      *---------------------------------------------------------------*
018300080508     C     INZS01        BEGSR
018400080508     C                   SETOFF                                       202190
018500141021     C                   WRITE     LRq10c1
018600950317     C                   SETON                                        2021
018700080512     C                   Z-ADD     0             NRR3              4 0
018800101223     c*
018900101223     c                   exsr      srsfl
019000101223     c*
019100080523     c                   endsr
019200080523     c**********************************************************************
019300101223     c* memorizzo spedizione errata nel sfl
019400080523     c**********************************************************************
019500101223     c     srsfl         begsr
019600101223     c* se ho solo un'anomalia a livello di spedizione, la carico,
019700101223     c* altrimenti aggancio il dettaglio
019800141021     c     karb70        setll     fnarb70l
019900101223     c                   do        *hival
020000141021     c     karb70        reade     fnarb70l
020100141021     c                   if        %eof(fnarb70l)
020200101223     c                   leave
020300101223     c                   end
020400141021     C                   Z-ADD     arbaas        v3caas
020500141021     C                   Z-ADD     arblnp        v3clnp
020600141021     C                   Z-ADD     arbnrs        v3cnrs
020700141021     C                   Z-ADD     arbnsp        v3cnsp
020800141021     C                   movel(p)  arbrsd        v3crsd
020900141022     C                   Z-ADD     arbncl        v3cncl
021000141022      *peso
021100141022     c                   if        arbpkc > arbpkb
021200141022     c                   z-add     arbpkc        v3cpes
021300141022     c                   else
021400141022     c                   z-add     arbpkb        v3cpes
021500141022     c                   endif
021600141022      *volume
021700141022     c                   if        arbvlc > arbvlb
021800141022     c                   z-add     arbvlc        v3cvol
021900141022     c                   else
022000141022     c                   z-add     arbvlb        v3cvol
022100141022     c                   endif
022200141022      *
022300101223     c                   exsr      srdett
022400141021     c                   setoff                                       87
022500141022     c                   if        v3cncla > 0
022600141021     c                   seton                                        87
022700141021     c                   endif
022800080526     C                   ADD       1             NRR3
022900141021     C                   WRITE     LRq10s1
023000101223     c                   enddo
023100080526     c                   endsr
023200080508      *---------------------------------------------------------------*
023300101223      *  Loop di errori  per segnacollo                               *
023400080508      *---------------------------------------------------------------*
023500101223     C     srdett        BEGSR
023600141022     c* colli arrivati
023700141030     c                   clear                   fnlrj6ds
023800141030     c                   eval      ILRj6Tric = 'A'
023900141030     c                   eval      ILRj6Drf= oggi
024000141030     c                   eval      ILRj6AAS= v3cAAs
024100141030     c                   eval      ILRj6LNP= v3cLnp
024200141030     c                   eval      ILRj6NRS= v3cNRs
024300141030     c                   eval      ILRj6NSP= v3cNSP
024400141030     c                   eval      ILRj6lin= v3clna
024500141030     c                   call      'FNLRJ6R'
024600141030     c                   parm                    fnlrJ6ds
024700141030     c                   eval      v3cncla = OLRJ6NCL - olrj6cspu
024800141021      * verifica presenza disposizioni
024900141021     c                   setoff                                       88
025000141022     c                   clear                   fnlry09ds2
025100141021     c                   clear                   v3hind
025101151125     c                   clear                   v3cdis
025200141021     c                   eval      ilry09tch = 'C'
025300141023     c                   eval      ilry09tdis = 'D'
025400141021     c                   eval      ilry09fgs = arbifp
025500141027     c                   eval      ilry09aas = v3cAAs
025600141027     c                   eval      ilry09lnp = v3cLnp
025700141027     c                   eval      ilry09nrs = v3cNRs
025800141027     c                   eval      ilry09nsp = v3cNSP
025900141021     c                   exsr      verdispo
026000141023     c                   if        olry09esdi = '1'
026001151125     c                   if        olry09reso = '0'
026002151125     c                   move      'D'           v3cdis
026003151125     c                   else
026004151125     c                   move      'R'           v3cdis
026005151125     c                   endif
026100141021     c                   seton                                        88
026200141021     c                   move      '1'           v3hind
026300141021     c                   endif
026400140130      *
026500101223     c                   endsr
026600141021     c*-----------------------------------------------------------------
026700141021     c     verdispo      begsr
026800141021     c*-----------------------------------------------------------------
026900141021     c                   eval      ilry09fgs = arbifp
027000141027     c                   eval      ilry09df12 = 'S'
027100141027     c                   eval      ilry09df21 = 'S'
027200141027     c                   eval      ilry09df06 = 'S'
027300141027     c                   eval      ilry09ffr = 'N'
027400141027     C                   eval      ILRY09MDB = RQ9MOB
027500141027     C                   eval      ILRY09MDE = RQ9moe
027600141027     C                   eval      ILRY09PRB = RQ9prb
027700141027     C                   eval      ILRY09PRE = RQ9PRE
027800141027     C                   eval      ILRY09PSS = RQ9PSS
027900141027     C                   eval      ILRY09MDB4= RQ9MDB4
028000141027     C                   eval      ILRY09MDB5= RQ9MDB5
028100141027     C                   eval      ILRY09BSL = RQ9BSL
028200141027     C                   eval      ILRY09PSL = RQ9PSL
028300141027     C                   eval      ILRY09PRB4= RQ9PRB4
028400141027     C                   eval      ILRY09PRB5= RQ9PRB5
028500141022     c                   eval      kpjbu =  fnlry09ds2
028600141021     c                   call      'FNLRY09R'
028700141021     c                   parm                    kpjba
028800141022     c                   eval      fnlry09ds2 =  kpjbu
028900141021     C                   ENDSR
029000101223      *---------------------------------------------------------------*
029100101223      *  Loop di errori  subfile errori                               *
029200101223      *---------------------------------------------------------------*
029300101223     C     CTR01         BEGSR
029400110104     c                   setoff                                       284044
029500950315      *
029600110104     c                   clear                   nr                4 0
029700110104     c                   do        *hival        nr
029800141021     C     nr            chain     LRq10s1                            32
029900110104     c                   if        *in32
030000110104     c                   leave
030100110104     c                   end
030200141023     c                   select
030300141023      *  Interrogazione bolla
030400141023     C                   when      v3csce = '5'
030500141023     c                   clear                   v3csce
030600141023     C                   MOVEL     '1'           P36FLG
030700141023     C                   Z-ADD     V3CAAS        P36AAS
030800141023     C                   Z-ADD     V3CLNP        P36LNP
030900141023     C                   Z-ADD     V3CNSP        P36NSP
031000141023     C                   Z-ADD     V3CNRS        P36NRS
031100141023     C                   MOVEL(P)  DSLR36        KPJBU
031200141023     C                   CALL      'FNLR36R'
031300141023     c                   PARM                    KPJBA
031400141023     c* opzione gestione spunte
031500141023     c                   when      v3csce = 'S'
031600141023     c                   clear                   v3csce
031700141023     c                   clear                   ds_para04
031800141023     C                   MOVEL     'FNLRQ2R'     P04PGM
031900141023     c                   eval      P04NPS = 99
032000141023     c                   eval      P04NPG = 9
032100160517     c                   eval      P04FLG = '3'
032200141023     c                   eval      P04FGS = rq9fgsi
032300141023     c                   eval      P04NRS = v3cNRS
032400141023     c                   eval      p04bolp = V3cLNP
032500141023     c                   eval      p04bors = V3cNRS
032600141023     c                   eval      p04bosp = V3cNSP
032700141023     c                   eval      p04boan = V3cAAS
032800141023     c                   eval      kpjbu = ds_para04
032900141023     c                   call      'FNLR04R'
033000141023     c                   parm                    kpjba
033100141023     c                   endsl
033200141023     C*                  if        v3csce = '1' and v3hind <> *blank
033300141023     c*                  eval      $msg = 'Opzione errata ci sono -
033400141023     c*                            disposizioni di consegna per la spedizione'
033500141023     c*                  seton                                        2840
033600141023     c*                  endif
033700080704     c*
033800980325      *  Aggiorno subfile
033900141022     c     v3hind        comp      ' '                                88
034000141023     c     v3hsce        comp      ' '                                44
034100141028     c                   setoff                                       87
034200141028     c                   if        v3cncla > 0
034300141028     c                   seton                                        87
034400141028     c                   endif
034500141022     C                   UPDATE    LRq10s1
034600141022     c   28              leave
034700141022     C                   ENDdo
034800141022     c*
034900141022     c                   z-add     1             nrr3
035000950317     C*
035100950317     C                   ENDSR
035200080707      *---------------------------------------------------------------*
035300141027      * esegue opzioni con cmd di conferma
035400080707      *---------------------------------------------------------------*
035500110119     C     sraggio       BEGSR
035600140130      **
035700141021     c                   clear                   nr                4 0
035800141021     c                   do        *hival        nr
035900141021     C     nr            chain     LRq10s1                            32
036000141021     c                   if        *in32
036100141021     c                   leave
036200141021     c                   end
036300141118     c                   setoff                                       28
036400141021     c                   select
036500141023      *  Toglie da distinta
036600141023     C                   when      v3csce = '2'
036700141023     c                   exsr      liberaSPE
036800141023     c   28              goto      nosbloc
036900141027      *se devo solo sbloccare e non ho errori
037000141023     c                   seton                                        44
037100141023     c                   move      '1'           v3hsce
037200141023      *  Esegue disposizione o dirottamento
037300141021     C                   when      v3csce = '1'
037400141022     c                   exsr      liberaSPE
037500141022     c   28              goto      nosbloc
037600141023      * non ho disposizioni dirottamento diretto
037700141023     c                   if        v3hind = *blank
037800141113     c                   clear                   fnlg82ds
037900141022     c                   eval      D82ges  = 'A'
038000141022     c                   eval      D82gia  = 'N'
038100141022     c                   move      v3caas        d82AAS
038200141022     c                   move      v3clnp        d82LNP
038300141022     c                   move      v3cnrs        d82NRS
038400141022     c                   move      v3cnsp        d82NSP
038401151125     c                   if        v3cdis <> *blank
038500151125     c                   eval      D82dis  = v3cdis
038501151125     c                   else
038502151125     c                   eval      D82dis  = 'D'
038503151125     c                   endif
038600141022     c                   eval      D82tri  = 'C'
038700141022     C                   eval      d82MOB  =   RQ9MOB
038800141022     C                   eval      d82PRB  =   RQ9PRB
038900141022     C                   eval      d82MOE  =   RQ9MOE
039000141022     C                   eval      d82PRE  =   RQ9PRE
039100141022     C                   eval      d82PSS  =   RQ9PSS
039200141022     C                   eval      d82MDB4 =   RQ9MDB4
039300141022     C                   eval      d82MDB5 =   RQ9MDB5
039400141022     C                   eval      d82BSL  =   RQ9BSL
039500141022     C                   eval      d82PSL  =   RQ9PSL
039600141022     C                   eval      d82PRB4 =   RQ9PRB4
039700141022     C                   eval      d82PRB5 =   RQ9PRB5
039800141022     c                   movel(p)  fnlg82ds      kpjbu
039900141022     c                   call      'FNLG20R'
040000141022     c                   parm                    kpjba
040100141022     c                   movel(p)  kpjbu         fnlg82ds
040200141022     c                   if        d82f12 = '1'
040300141117     c                   clear                   v3hind
040400141027     c                   exsr      InserSpe
040500141027     c                   else
040600141028     c                   move      '1'           v3hsce
040700141028     c                   clear                   v3hind
040800141027     c                   endif
040900141023     c*
041000141023     c                   else
041100141022     c* opzione disposizioni consegna
041200141022     c                   clear                   fnlry09ds2
041300141021     c                   eval      ilry09tch = 'E'
041400141023     c                   eval      ilry09tdis = 'D'
041500141021     c                   eval      ilry09aas = v3cAAS
041600141021     c                   eval      ilry09lnp = v3cLNP
041700141021     c                   eval      ilry09nrs = v3cNRS
041800141021     c                   eval      ilry09nsp = v3cNSP
041900141021     c                   exsr      verdispo
042000141022     c                   if        olry09err = *blank
042100141023     c                   if        olry09esdi = '1'
042101151125     c                   if        olry09reso = '0'
042102151125     c                   move      'D'           v3cdis
042103151125     c                   else
042104151125     c                   move      'R'           v3cdis
042105151125     c                   endif
042200141022     c                   move      '1'           v3hind
042300141028     c                   exsr      InserSpe
042400141022     c                   else
042500141028     c                   move      '1'           v3hsce
042600141022     c                   clear                   v3hind
042700141022     c                   endif
042800141028      *errore
042900141028     c                   else
043000141028     C                   exsr      Inserspe
043100141028     c                   eval      $msg = 'Attenzione la disposizione -
043200141028     c                             di consegna non è stata applicata'
043300141028     c                   seton                                        2840
043400141022     c                   endif
043500141028      *
043600141022     c                   endif
043700141021     C                   ENDsl
043800141022     c     nosbloc       tag
043900141021      *  Aggiorno subfile
044000141021     c     v3hind        comp      ' '                                88
044100141027     c     v3hsce        comp      ' '                                44
044200141021     c                   clear                   v3csce
044300141028     c                   setoff                                       87
044400141028     c                   if        v3cncla > 0
044500141028     c                   seton                                        87
044600141028     c                   endif
044700141021     C                   UPDATE    LRq10s1
044800141023     c                   clear                   v3hsce
044900141028     c                   setoff                                       4440
045000110119     c                   enddo
045100140130      **
045200141021     c                   z-add     1             nrr3
045300110119     C                   ENDSR
045400141022      *_______________________________________________________
045500141022     c     LiberaSpe     begsr
045600141022      *_______________________________________________________
045700141022     c                   clear                   fnlr98ds
045800141022     c                   eval      i98aas = v3caas
045900141022     c                   eval      i98lnp = v3clnp
046000141022     c                   eval      i98nrs = v3cnrs
046100141022     c                   eval      i98nsp = v3cnsp
046200141022     c                   eval      I98VIDEO  = 'N'
046300141022     c                   eval      I98npg    = 4
046400141022     c                   eval      I98fgs    = rq9fgsi
046500141022     c                   eval      I98ndc    = rq9ndci
046600141022     c*
046700141023     c                   eval      I98ALCDST = 'N'
046800141023     c                   eval      I98CTLDST = 'N'
046900141027     c                   eval      I98STRCMT = ' '
047000141022     c                   eval      kpjbu = fnlr98ds
047100141027     c                   call      'FNLR98C'
047200141022     c                   parm                    kpjba
047300141022     c                   movel     kpjbu         fnlr98ds
047400141022     C                   if        o98esito = '1'
047500141022     c                   seton                                        2840
047600141023     C                   eval      $MSG = o98msgerr
047700141022     c                   end
047800141022     c                   endsr
047900141027      *_______________________________________________________
048000141027     c     InserSpe      begsr
048100141027      *_______________________________________________________
048200141027     c                   clear                   fnlr98ds
048300141027
048400141027     c                   eval      I98DELINS = 'M'
048500141027     c                   eval      i98aas = v3caas
048600141027     c                   eval      i98lnp = v3clnp
048700141027     c                   eval      i98nrs = v3cnrs
048800141027     c                   eval      i98nsp = v3cnsp
048900141027     c                   eval      I98VIDEO  = 'N'
049000141027     c                   eval      I98npg    = 4
049100141027     c                   eval      I98fgs    = rq9fgsi
049200141027     c                   eval      I98ndc    = rq9ndci
049300141027     c*
049400141027     c                   eval      I98ALCDST = 'N'
049500141027     c                   eval      I98CTLDST = 'N'
049600141027     c                   eval      I98STRCMT = ' '
049700141027     c                   eval      kpjbu = fnlr98ds
049800141027     c                   call      'FNLR98C'
049900141027     c                   parm                    kpjba
050000141027     c                   movel     kpjbu         fnlr98ds
050100141027     C                   if        o98esito = '1'
050200141027     c                   seton                                        2840
050300141027     C                   eval      $MSG = o98msgerr
050400141027     c                   end
050500141027     c                   endsr
050600110119      *---------------------------------------------------------------*
050700110119      * fine
050800110119      *---------------------------------------------------------------*
050900110119     C     srfine        BEGSR
051000080526     C*
051100080526     C                   SETON                                        LR
051200080710     c                   return
051300080707     c*
051400080707     C                   ENDSR
051500080509     C*-----------------------------------------------------*
051600080509     C     *INZSR        BEGSR
051700080509     C*-----------------------------------------------------*
051800080508      *
051900080508     C     *ENTRY        PLIST
052000101220     C                   PARM                    kpjba
052100141021     C                   PARM                    fnlrq9ds
052200140130      *
052300080515     C* Decodifica Elaboratore
052400080515     c     *dtaara       define    §azute        azuteds
052500080515     c     *dtaara       define    §datiute      ddatiute
052600080515     C                   in(E)     *dtaara
052700080515     C                   IF        %Error  or  RSUT = *blanks
052800080515     C                   call      'TIBS34R'
052900080515     C                   parm                    Tibs34Ds
053000080515     C                   in        *dtaara
053100080515     c                   ENDIF
053200110119     c     kapd          klist
053300110119     c                   kfld                    apdtip
053400141021     c                   kfld                    rq9pdr
053500141021     c                   eval      apdtip = 'A'
053600141021     c     karb70        klist
053700141021     c                   kfld                    ARBifp
053800141021     c                   kfld                    arbndc
053900141021     c                   kfld                    RQ9DFVI
054000141021     c                   z-add     RQ9FGSI       ARBifp
054100141021     c                   z-add     RQ9ndcI       ARBndc
054200110117     c* gg
054300141021     c                   if        rq9dfvi = 0
054400110117     c                   move      udate         dtaiso
054500110117     c                   else
054600141021     c                   move      rq9dfvi       dtaiso
054700110117     c                   end
054800110117     c                   move      dtaiso        oggi
054900080509     C                   ENDSR
055000141028      *-------------------------------------------------------------------------
055100141028      * Ordinamento SFL
055200141028      *-------------------------------------------------------------------------
055300141028     C     sort          BEGSR
055400141028      *
055500141028     C                   CLEAR                   QLGSCB
055600141028     C                   CLEAR                   QLGSCB00
055700141028      *
055800141028      *
055900141028     C                   EVAL      QLGNBRK  =  1
056000141028      *
056100141028     ** prima x PDA è a posizione 1, numerico, 1 byte, descending.
056200141028     C                   EVAL      QLGSP      = 1
056300141028     C                   EVAL      QLGSS      = %SIZE(v3cncla)
056400141028     C                   EVAL      QLGDT      = Numerico
056500141105     C                   EVAL      QLGSO      = discendente
056600141028      **
056700141028     C                   EVAL      QLGKL(1)   = QLGSKL
056800141028
056900141028      * Load other sort parameters.
057000141028     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
057100141028     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
057200141028     C                   EVAL      QLGRT     = 8
057300141028     C                   EVAL      QLGOKL    = 80
057400141028     C                   EVAL      QLGLKE    = 16
057500141028     C                   EVAL      QLGLSS    = 290
057600141028
057700141028      * Initialize Sort I/O API fields.
057800141028     C                   EVAL      QLGRL00  = QLGRL
057900141028     C                   EVAL      QLGRC00  = 1
058000141028     C                   CLEAR                   QUSEI
058100141028     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
058200141028
058300141028      * First step - Initialize the sort routine.
058400141028     C                   CALL      'QLGSORT'
058500141028     C                   PARM                    QLGSCB
058600141028     C                   PARM                    NotUsed
058700141028     C                   PARM                    NotUsed
058800141028     C                   PARM                    SizeList
058900141028     C                   PARM                    ReturnSize
059000141028     C                   PARM                    QUSEC
059100141028
059200141028      * Next step - Write records to I/O routine.
059300141028     C                   EVAL      QLGRT00 = Put
059400141028
059500141028     C     1             DO        nrrlast       x                 4 0
059600141028
059700141028     C     x             CHAIN     Lrq10s1
059800141028
059900141028     ** Solo le righe con Selected = 'Y' sono riordinate,
060000141028     ** quindi per fare un ordinamento di tutte le righe
060100141028     ** metto 'Y' sempre.
060200141028     C                   EVAL      Selected = 'Y'
060300141028
060400141028     C                   CLEAR                   QUSEI
060500141028     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
060600141028     C                   CALL      'QLGSRTIO'
060700141028     C                   PARM                    QLGSCB00
060800141028     C                   PARM                    SflRcd
060900141028     C                   PARM                    NotUsed
061000141028     C                   PARM                    SizeList
061100141028     C                   PARM                    NotUsed
061200141028     C                   PARM                    QUSEC
061300141028
061400141028     C                   ENDDO
061500141028
061600141028      * Next step - Signal end of input, clear subfile for reload.
061700141028     C                   EVAL      QLGRT00 = EndPut
061800141028     C                   CLEAR                   QUSEI
061900141028     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
062000141028
062100141028     C                   CALL      'QLGSRTIO'
062200141028     C                   PARM                    QLGSCB00
062300141028     C                   PARM                    SflRcd
062400141028     C                   PARM                    NotUsed
062500141028     C                   PARM                    SizeList
062600141028     C                   PARM                    NotUsed
062700141028     C                   PARM                    QUSEC
062800141028     C                   SETOFF                                       21
062900141028     C                   WRITE     LRq10c1
063000141028     C                   SETON                                        21
063100141028      * Final step - Write the records back to the subfile.
063200141028     C                   EVAL      QLGRT00 = Get
063300141028
063400141028     C     1             DO        nrrlast       x
063500141028     C                   CLEAR                   QUSEI
063600141028     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
063700141028     C                   CALL      'QLGSRTIO'
063800141028     C                   PARM                    QLGSCB00
063900141028     C                   PARM                    NotUsed
064000141028     C                   PARM                    SflRcd
064100141028     C                   PARM                    QLGRL00
064200141028     C                   PARM                    NotUsed
064300141028     C                   PARM                    QUSEC
064400141028      *
064500141028     c                   z-add     x             nrr3
064600141028     c     v3hind        comp      ' '                                88
064700141028     c     v3hsce        comp      ' '                                44
064800141028     c                   setoff                                       87
064900141028     c                   if        v3cncla > 0
065000141028     c                   seton                                        87
065100141028     c                   endif
065200141028     C                   WRITE     Lrq10s1
065300141028     C                   ENDDO
065400141028      *
065500141028     C                   ENDSR
