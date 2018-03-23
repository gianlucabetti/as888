000100080222     h decedit('0,') option(*nodebugio)
000200080222      *---------------------------------------------------------------*
000300080331      * non deve fare commit o rolback perchè lo fa il chiamante (TRTCPC0R)
000400080222      *---------------------------------------------------------------*
000500080226     ffiplg00f  o    e             disk    commit
000600080423     ffidsf02l  if   e           k disk
000700080331     d tmp             s             14
000800080222     d esito           s              1
000900080222     d prmfgs          s              3
001000080222     d prmnfv          s              6
001100080225     d prmaut          s              7
001200110606     d prmdfv          s              8
001300080222     d prmdoc          s             16
001400080222     d prmdatora       s             14
001500080222     d* output cod. errore
001600110606     d saverr          s              1
001700110606     d prmerr          s              1
001800080222     d prmmsg          s             30
001900080424     d*
002000080424     d parfgs          s                   like(prmfgs)
002100080428     d parspc          s                   like(§CRISPCCOD)
002200080424     d parndc          s                   like(prmnfv)
002300080424     d pards           s                   like(fiprdds)
002400121015
002500121015     d wDataOra        s             12
002600080222
002700080331     d cat4            s              1  0 inz(4)
002800080331     d fase            s              3    inz('CRI')
002900080222     d fiprdds       e ds                  extname(fiprd00r)
003000080331     d fiprdCRIds    e ds
003100080507     d
003200080423      * per sbmjob
003300080423     d profilo         s             10
003400080423     d nomejob         s             10
003500080512     d comman          S           1024                                         *comando CLP
003600080512     d lenght          S             15  5 INZ(1024)                            *lunghezza comando
003700080331      * ?_______________________________________________________________________
003800080222     c                   exsr      sr_prima
003900080222
004000080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
004100080225      * fine
004200080222     c                   exsr      sr_ctrdati
004300080227      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
004400080225     c                   exsr      sr_wrtlog
004500080424      * sottometto lavoro per generazione spunte 8 rientro e scrittura
004600080424      * fase CRI che permetta la chiusura distinta e aggiornamento file rientri con bancali
004700080423     c                   exsr      sr_sbmjob
004800080227     c* fine
004900080227     c                   exsr      sr_fine
005000080227     c*
005100080331      * ?_______________________________________________________________________
005200080222      * ROUTINE CONTROLLO DATI FIPRD
005300080331      * ?_______________________________________________________________________
005400080222     c     sr_ctrdati    begsr
005500080222     c                   clear                   fiplg000
005600080222     c                   clear                   prmerr
005700080222     c                   clear                   prmmsg
005800080226      * controllo se esiste la distinta e se dell'aut.
005900080222     c                   exsr      sr_ctrdst
006000080222     c                   if        prmerr <> ' '
006100080222     c                   eval      plgmsg = prmmsg
006200080222     c                   exsr      sr_wrtlog
006300080222     c                   end
006400080331     c                   move      §crifgs       plgfgs
006500080331     c                   move      §crindc       plgndc
006600080222      * data ora pda conferma
006700080222     c                   exsr      sr_ctrdop
006800110606     c                   if        prmerr =  'E'
006900080222     c                   eval      plgmsg = prmmsg
007000080222     c                   exsr      sr_wrtlog
007100080222     c                   end
007200110606     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
007300121015     c* 10 gg o superiore 10 gg. per cui forzo data distinta e ora ricezione
007400110606     c                   eval      saverr = prmerr
007500080331     c* verifico se fase CRI già presente sul file delle fasi
007600080331     c     kdsf          chain     fidsf02l
007700080331     c                   if        %found(fidsf02l)
007800080331     c                   eval      plgmsg = 'FASE CONTROLLO RIENTRO-
007900080331     c                              GIA PRESENTE'
008000080226     c                   exsr      sr_wrtlog
008100080226     c                   end
008200080222     c                   endsr
008300080222      *---------------------------------------------------------------------
008400080222      * controlla distinta
008500080222      *---------------------------------------------------------------------
008600080222     c     sr_ctrdst     begsr
008700080222     c                   eval      prmfgs = %subst(prddati:1:3)
008800080222     c                   eval      prmnfv = %subst(prddati:4:6)
008900080225     c                   eval      prmaut = %subst(prdprfc:3:7)
009000110606     c                   eval      prmdfv = *all'0'
009100080222     c                   call      'TRTCPDSTR'
009200080222     c                   parm                    prmfgs
009300080222     c                   parm                    prmnfv
009400080225     c                   parm                    prmaut
009500110606     c                   parm                    prmdfv
009600080222     c                   parm                    prmerr
009700080222     c                   parm                    prmmsg
009800080222     c                   endsr
009900080222      *---------------------------------------------------------------------
010000080222      * controlla data ora pda conferma
010100080222      *---------------------------------------------------------------------
010200080222     c     sr_ctrdop     begsr
010300080507     c                   eval      prmdatora = %subst(prddati:15:14)
010400080225     c                   call      'TRTCPDOPR'
010500080222     c                   parm                    prmdatora
010600080222     c                   parm                    prmerr
010700080222     c                   parm                    prmmsg
010800080222     c                   endsr
010900080222      *---------------------------------------------------------------------
011000080222      * scrive log
011100080222      *---------------------------------------------------------------------
011200080222     c     sr_wrtlog     begsr
011300080225     c* se sto scrivendo un errore imposto anche il flag
011400080225     c                   if        plgmsg <> ' '
011500080222     c                   eval      plgerr = 'E'
011600080225     c                   else
011700080225     c                   clear                   plgerr
011800080225     c                   end
011900121015     c* segnalo se forzata data ricezione errata perchè antecedente di 10 gg
012000121015      * o superiore a 10 gg.
012100110608     c                   if        saverr = 'F' and
012200110608     c                             plgmsg = ' '
012300121015     c                   eval      plgmsg = 'DATA < o > di 10 GG'
012400110607     c                   end
012500080222     c                   eval      plgtiporcd = prdtiporcd
012600080222     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
012700080222     c                   eval      plgtiporcd = prdtiporcd
012800080222     c                   eval      plgprfc = prdprfc
012900080222     c                   eval      plgiddisp = prdiddisp
013000080222     c                   eval      plgdatora = prddatora
013100080222     c                   eval      plgdati = prddati
013200080331     c                   eval      plgiddoc = *BLANK
013300080401     c                   eval      plgtipappl = 'C'
013400080222     c                   write     fiplg000
013500080225     c* se sto scrivendo un errore vado a fine
013600110608     c                   if        plgerr <> ' '
013700080222     c                   exsr      sr_fine
013800080225     c                   end
013900080227     c                   endsr
014000080227
014100080227      *----------------------------------------------------------------
014200080423      * sottomissione job per scrittura spunte 8 e fase CRI
014300080227      *----------------------------------------------------------------
014400080423     c     sr_sbmjob     begsr
014500080424     c                   move      prmfgs        parfgs
014600080424     c                   move      prmnfv        parndc
014700080424     c                   move      §CRISPCCOD    parspc
014800110606     c                   if        saverr = 'F'
014900121015     c                   eval      wDataOra = %subst(PRDdatora:1:12)
015000121015     c                   movel     wDataOra      §CRIdtorin
015100110606     c                   movel(p)  fiprdcrids    prddati
015200110608     c                   clear                   saverr
015300110606     c                   end
015400080423     c                   move      fiprdds       pards
015500080507     C                   eval      profilo = 'AU' + parfgs
015600080507     C                   eval      nomejob = 'AU' + parfgs+ 'CRI'
015700080423
015800080423     c                   eval      comman =
015900080424     c                             'sbmjob cmd(call pgm(TRTCP1C) PARM( -
016000080507     c                              '''+ parfgs +''' +
016100080507     c                              '''+ parndc +''' +
016200080507     c                              '''+ parspc +''' +
016300080512     c                              '''+ pards +''')) +
016400080512     c                              job(' + nomejob + ') -
016500080512     c                              JOBQ(aggpc) USER(' + profilo + ')'
016600080423
016700080423     c                   call      'QCMDEXC'
016800080423     c                   parm                    comman
016900080423     c                   parm                    lenght
017000080331
017100080222     c                   endsr
017200080222
017300080331      * ?_______________________________________________________________________
017400080222      * ROUTINE INIZIALE
017500080331      * ?_______________________________________________________________________
017600080222     c     sr_prima      begsr
017700080222     c* parametri
017800080222     c     *entry        plist
017900080222     c                   parm                    fiprdds
018000080509     c                   parm                    esito
018100080222     c                   clear                   esito
018200080331     c                   eval      fiprdcrids=prddati
018300080331     c     kdsf          klist
018400080331     c                   kfld                    cat4
018500080331     c                   kfld                    §criNDC
018600080331     c                   kfld                    §criFGS
018700080331     c                   kfld                    fase
018800080222
018900080222     c                   endsr
019000080222      *--------------------------------------------------------------
019100080222      * fine
019200080222      *--------------------------------------------------------------
019300080222     c     sr_fine       begsr
019400080222     c                   seton                                        rt
019500080222     c                   return
019600080222     c                   endsr
