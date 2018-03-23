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
001101110606     d prmdfv          s              8
001200080222     d prmdoc          s             16
001300080222     d prmdatora       s             14
001400080222     d* output cod. errore
001500110606     d saverr          s              1
001501110606     d prmerr          s              1
001600080222     d prmmsg          s             30
001700080424     d*
001800080424     d parfgs          s                   like(prmfgs)
001900080428     d parspc          s                   like(§CRISPCCOD)
002000080424     d parndc          s                   like(prmnfv)
002100080424     d pards           s                   like(fiprdds)
002200080222
002300080331     d cat4            s              1  0 inz(4)
002400080331     d fase            s              3    inz('CRI')
002500080222     d fiprdds       e ds                  extname(fiprd00r)
002600080331     d fiprdCRIds    e ds
002700080507     d
002800080423      * per sbmjob
002900080423     d profilo         s             10
003000080423     d nomejob         s             10
003100080512     d comman          S           1024                                         *comando CLP
003200080512     d lenght          S             15  5 INZ(1024)                            *lunghezza comando
003300080331      * ?_______________________________________________________________________
003400080222     c                   exsr      sr_prima
003500080222
003600080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
003700080225      * fine
003800080222     c                   exsr      sr_ctrdati
003900080227      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
004000080225     c                   exsr      sr_wrtlog
004100080424      * sottometto lavoro per generazione spunte 8 rientro e scrittura
004200080424      * fase CRI che permetta la chiusura distinta e aggiornamento file rientri con bancali
004300080423     c                   exsr      sr_sbmjob
004400080227     c* fine
004500080227     c                   exsr      sr_fine
004600080227     c*
004700080331      * ?_______________________________________________________________________
004800080222      * ROUTINE CONTROLLO DATI FIPRD
004900080331      * ?_______________________________________________________________________
005000080222     c     sr_ctrdati    begsr
005100080222     c                   clear                   fiplg000
005200080222     c                   clear                   prmerr
005300080222     c                   clear                   prmmsg
005400080226      * controllo se esiste la distinta e se dell'aut.
005500080222     c                   exsr      sr_ctrdst
005600080222     c                   if        prmerr <> ' '
005700080222     c                   eval      plgmsg = prmmsg
005800080222     c                   exsr      sr_wrtlog
005900080222     c                   end
006000080331     c                   move      §crifgs       plgfgs
006100080331     c                   move      §crindc       plgndc
006200080222      * data ora pda conferma
006300080222     c                   exsr      sr_ctrdop
006400110606     c                   if        prmerr =  'E'
006500080222     c                   eval      plgmsg = prmmsg
006600080222     c                   exsr      sr_wrtlog
006700080222     c                   end
006701110606     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
006702110606     c* 30 gg per cui forzo data distinta e ora ricezione
006703110606     c                   eval      saverr = prmerr
006800080331     c* verifico se fase CRI già presente sul file delle fasi
006900080331     c     kdsf          chain     fidsf02l
007000080331     c                   if        %found(fidsf02l)
007100080331     c                   eval      plgmsg = 'FASE CONTROLLO RIENTRO-
007200080331     c                              GIA PRESENTE'
007300080226     c                   exsr      sr_wrtlog
007400080226     c                   end
007500080222     c                   endsr
007600080222      *---------------------------------------------------------------------
007700080222      * controlla distinta
007800080222      *---------------------------------------------------------------------
007900080222     c     sr_ctrdst     begsr
008000080222     c                   eval      prmfgs = %subst(prddati:1:3)
008100080222     c                   eval      prmnfv = %subst(prddati:4:6)
008200080225     c                   eval      prmaut = %subst(prdprfc:3:7)
008201110606     c                   eval      prmdfv = *all'0'
008300080222     c                   call      'TRTCPDSTR'
008400080222     c                   parm                    prmfgs
008500080222     c                   parm                    prmnfv
008600080225     c                   parm                    prmaut
008601110606     c                   parm                    prmdfv
008700080222     c                   parm                    prmerr
008800080222     c                   parm                    prmmsg
008900080222     c                   endsr
009000080222      *---------------------------------------------------------------------
009100080222      * controlla data ora pda conferma
009200080222      *---------------------------------------------------------------------
009300080222     c     sr_ctrdop     begsr
009400080507     c                   eval      prmdatora = %subst(prddati:15:14)
009500080225     c                   call      'TRTCPDOPR'
009600080222     c                   parm                    prmdatora
009700080222     c                   parm                    prmerr
009800080222     c                   parm                    prmmsg
009900080222     c                   endsr
010000080222      *---------------------------------------------------------------------
010100080222      * scrive log
010200080222      *---------------------------------------------------------------------
010300080222     c     sr_wrtlog     begsr
010400080225     c* se sto scrivendo un errore imposto anche il flag
010500080225     c                   if        plgmsg <> ' '
010600080222     c                   eval      plgerr = 'E'
010700080225     c                   else
010800080225     c                   clear                   plgerr
010900080225     c                   end
010901110607     c* segnalo se forzata data ricezione errata perchè antecedente di 30 gg
010902110608     c                   if        saverr = 'F' and
010903110608     c                             plgmsg = ' '
010904110607     c                   move      'DATA < 30 GG'plgmsg
010905110607     c                   end
011000080222     c                   eval      plgtiporcd = prdtiporcd
011100080222     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
011200080222     c                   eval      plgtiporcd = prdtiporcd
011300080222     c                   eval      plgprfc = prdprfc
011400080222     c                   eval      plgiddisp = prdiddisp
011500080222     c                   eval      plgdatora = prddatora
011600080222     c                   eval      plgdati = prddati
011700080331     c                   eval      plgiddoc = *BLANK
011800080401     c                   eval      plgtipappl = 'C'
011900080222     c                   write     fiplg000
012000080225     c* se sto scrivendo un errore vado a fine
012100110608     c                   if        plgerr <> ' '
012200080222     c                   exsr      sr_fine
012300080225     c                   end
012400080227     c                   endsr
012500080227
012600080227      *----------------------------------------------------------------
012700080423      * sottomissione job per scrittura spunte 8 e fase CRI
012800080227      *----------------------------------------------------------------
012900080423     c     sr_sbmjob     begsr
013000080424     c                   move      prmfgs        parfgs
013100080424     c                   move      prmnfv        parndc
013200080424     c                   move      §CRISPCCOD    parspc
013201110606     c                   if        saverr = 'F'
013202110606     c                   movel     prddatora     §CRIDTORIN
013203110606     c                   movel(p)  fiprdcrids    prddati
013204110608     c                   clear                   saverr
013205110606     c                   end
013300080423     c                   move      fiprdds       pards
013400080507     C                   eval      profilo = 'AU' + parfgs
013500080507     C                   eval      nomejob = 'AU' + parfgs+ 'CRI'
013600080423
013700080423     c                   eval      comman =
013800080424     c                             'sbmjob cmd(call pgm(TRTCP1C) PARM( -
013900080507     c                              '''+ parfgs +''' +
014000080507     c                              '''+ parndc +''' +
014100080507     c                              '''+ parspc +''' +
014200080512     c                              '''+ pards +''')) +
014300080512     c                              job(' + nomejob + ') -
014400080512     c                              JOBQ(aggpc) USER(' + profilo + ')'
014500080423
014600080423     c                   call      'QCMDEXC'
014700080423     c                   parm                    comman
014800080423     c                   parm                    lenght
014900080331
015000080222     c                   endsr
015100080222
015200080331      * ?_______________________________________________________________________
015300080222      * ROUTINE INIZIALE
015400080331      * ?_______________________________________________________________________
015500080222     c     sr_prima      begsr
015600080222     c* parametri
015700080222     c     *entry        plist
015800080222     c                   parm                    fiprdds
015900080509     c                   parm                    esito
016000080222     c                   clear                   esito
016100080331     c                   eval      fiprdcrids=prddati
016200080331     c     kdsf          klist
016300080331     c                   kfld                    cat4
016400080331     c                   kfld                    §criNDC
016500080331     c                   kfld                    §criFGS
016600080331     c                   kfld                    fase
016700080222
016800080222     c                   endsr
016900080222      *--------------------------------------------------------------
017000080222      * fine
017100080222      *--------------------------------------------------------------
017200080222     c     sr_fine       begsr
017300080222     c                   seton                                        rt
017400080222     c                   return
017500080222     c                   endsr
