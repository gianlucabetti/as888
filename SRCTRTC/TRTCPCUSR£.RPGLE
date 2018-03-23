000100080222     h decedit('0,') option(*nodebugio)
000200080222      *---------------------------------------------------------------*
000300080331      * Da FIPRD00R scrive file FIDSF00F per inserimento fase CUS
000400080331      *             aggiorna FIQDT00F per bancali affidati
000500080331      * non deve fare commit o rolback perchè lo fa il chiamante (TRTCPC0R)
000600080222      *---------------------------------------------------------------*
000700080226     ffiplg00f  o    e             disk    commit
000800080624     ffidst01l  if   e           k disk
000900080331     ffidsf02l  uf a e           k disk    commit
001000080331     ffiqdt01l  uf   e           k disk    commit
001100080331     d tmp             s             14
001200080222     d esito           s              1
001300080222     d prmfgs          s              3
001400080222     d prmnfv          s              6
001500080225     d prmaut          s              7
001600110606     d prmdfv          s              8
001700080222     d prmdoc          s             16
001800080222     d prmdatora       s             14
001900080222     d* output cod. errore
002000110606     d saverr          s              1
002100080222     d prmerr          s              1
002200080222     d prmmsg          s             30
002300080222
002400080331     d cat4            s              1  0 inz(4)
002500080331     d fase            s              3    inz('CUS')
002600080222     d fiprdds       e ds                  extname(fiprd00r)
002700080331     d fiprdcusds    e ds
002800080728     d fidsfcusds    e ds
002900080624     d ddstflr       e ds
003000080728     d dpsp          e ds
003100080728     d tibs02ds      e ds
003200080728     d kpjba         e ds
003300080728     d parmpo          s              3
003400081211     d codspc          s              2    dim(22)
003500081211     d desspc          s             15    dim(22)
003600080331      * ?_______________________________________________________________________
003700080222     c                   exsr      sr_prima
003800080222
003900080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
004000080225      * fine
004100080222     c                   exsr      sr_ctrdati
004200080227      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
004300080225     c                   exsr      sr_wrtlog
004400080401      * scrivo file fasi e rientri
004500080227     c                   exsr      sr_write
004600080227     c* fine
004700080227     c                   exsr      sr_fine
004800080227     c*
004900080331      * ?_______________________________________________________________________
005000080222      * ROUTINE CONTROLLO DATI FIPRD
005100080331      * ?_______________________________________________________________________
005200080222     c     sr_ctrdati    begsr
005300080222     c                   clear                   fiplg000
005400080222     c                   clear                   prmerr
005500080222     c                   clear                   prmmsg
005600080226      * controllo se esiste la distinta e se dell'aut.
005700080222     c                   exsr      sr_ctrdst
005800080222     c                   if        prmerr <> ' '
005900080222     c                   eval      plgmsg = prmmsg
006000080222     c                   exsr      sr_wrtlog
006100080222     c                   end
006200080331     c                   move      §cusfgs       plgfgs
006300080331     c                   move      §cusndc       plgndc
006400080222      * data ora pda conferma
006500080222     c                   exsr      sr_ctrdop
006600110606     c                   if        prmerr =  'E'
006700080222     c                   eval      plgmsg = prmmsg
006800080222     c                   exsr      sr_wrtlog
006900080222     c                   end
007000110606     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
007100121016     c* 10 gg oppure superiore 10 gg. per cui forzo data distinta e ora ricezione
007200110606     c                   eval      saverr = prmerr
007300080331     c* verifico se fase CUS già presente sul file delle fasi
007400080429     c*    kdsf          chain     fidsf02l
007500080429     c*                  if        %found(fidsf02l)
007600080429     c*                  eval      plgmsg = 'FASE CONTROLLO USCITA GIA PRESENTE'
007700080429     c*                  exsr      sr_wrtlog
007800080429     c*                  end
007900080222     c                   endsr
008000080222      *---------------------------------------------------------------------
008100080222      * controlla distinta
008200080222      *---------------------------------------------------------------------
008300080222     c     sr_ctrdst     begsr
008400080222     c                   eval      prmfgs = %subst(prddati:1:3)
008500080222     c                   eval      prmnfv = %subst(prddati:4:6)
008600080225     c                   eval      prmaut = %subst(prdprfc:3:7)
008700110606     c                   eval      prmdfv = *all'0'
008800080222     c                   call      'TRTCPDSTR'
008900080222     c                   parm                    prmfgs
009000080222     c                   parm                    prmnfv
009100080225     c                   parm                    prmaut
009200110606     c                   parm                    prmdfv
009300080222     c                   parm                    prmerr
009400080222     c                   parm                    prmmsg
009500080222     c                   endsr
009600080222      *---------------------------------------------------------------------
009700080222      * controlla data ora pda conferma
009800080222      *---------------------------------------------------------------------
009900080222     c     sr_ctrdop     begsr
010000080331     c                   eval      prmdatora = %subst(prddati:35:14)
010100080225     c                   call      'TRTCPDOPR'
010200080222     c                   parm                    prmdatora
010300080222     c                   parm                    prmerr
010400080222     c                   parm                    prmmsg
010500080222     c                   endsr
010600080222      *---------------------------------------------------------------------
010700080222      * scrive log
010800080222      *---------------------------------------------------------------------
010900080222     c     sr_wrtlog     begsr
011000080225     c* se sto scrivendo un errore imposto anche il flag
011100080225     c                   if        plgmsg <> ' '
011200080222     c                   eval      plgerr = 'E'
011300080225     c                   else
011400080225     c                   clear                   plgerr
011500080225     c                   end
011600121016     c* segnalo se forzata data ricezione errata perchè antecedente di 10 gg
011700121016      * o superiore a 10 gg.
011800110607     c                   if        saverr = 'F'
011900110607     c                   if        plgmsg = ' '
012000121016     c                   eval      plgmsg = 'DATA < o > di 10 GG'
012100110607     c                   end
012200110607     c                   clear                   saverr
012300110607     c                   end
012400080222     c                   eval      plgtiporcd = prdtiporcd
012500080222     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
012600080222     c                   eval      plgtiporcd = prdtiporcd
012700080222     c                   eval      plgprfc = prdprfc
012800080222     c                   eval      plgiddisp = prdiddisp
012900080222     c                   eval      plgdatora = prddatora
013000080222     c                   eval      plgdati = prddati
013100080331     c                   eval      plgiddoc = *BLANK
013200080401     c                   eval      plgtipappl = 'C'
013300080222     c                   write     fiplg000
013400080225     c* se sto scrivendo un errore vado a fine
013500080225     c                   if        plgmsg <> ' '
013600080222     c                   exsr      sr_fine
013700080225     c                   end
013800080227     c                   endsr
013900080227
014000080227      *----------------------------------------------------------------
014100080331      * scrittura file fase distinta e rientri
014200080227      *----------------------------------------------------------------
014300080227     c     sr_write      begsr
014400080624      *scrittura aggiornamento fase
014500080624     c     kdsf          chain     fidsf02l
014600080331     c                   clear                   fidsf000
014700080728     c                   clear                   fidsfcusds
014800080331     c                   eval      DSFfgs    =   §CUSFGS
014900080331     c                   eval      DSFnpg    =   cat4
015000080331     c                   eval      DSFnfv    =   §CUSNDC
015100080331     c                   eval      DSFtrd    =   fase
015200080728     c                   eval      §DSFNRORM =   §CUSNRORM
015300080728     c                   eval      §DSFNRCON =   §CUSNRCON
015400080728     c                   eval      §DSFNRCOL =   §CUSNRCOL
015500080728     c                   eval      §DSFNRBARE=   §CUSNRBARE
015600080728     c                   eval      §DSFNRBAAF=   §CUSNRBAAF
015700080728     c                   eval      §DSFSPCCOD=   §CUSSPCCOD
015800080728     c* richiamo pgm per decodifica spc
015900080728     c                   exsr      decspc
016000080728      *
016100080728     c                   z-add     1             i                 2 0
016200080728     c     §CUSSPCCOD    lookup    codspc(i)                              66
016300080728     c   66              eval      §DSFSPCDES=   desspc(i)
016400080728     c                   movel     FIDSFCUSDS    DSFDATI
016500080728      *
016600080331     c                   eval      tmp= %char(%timestamp: *iso0)
016700080331     c                   move      tmp           DSFdtorin
016800080331     c                   eval      DSFPDAAS4 = 'P'
016900080624     c                   if        %found(fidsf02l)
017000080624     c                   update    fidsf000
017100080624     c                   else
017200080331     c                   write     fidsf000
017300080624     c                   endif
017400080624      *scrittura bancali per rientri solo se autista non in fase di test
017500080624     c                   clear                   ddstflr
017600080624     c     kdst          chain     fidst01l
017700080624     c                   if        %found(fidst01l)
017800080624     c                   movel     dstflr        ddstflr
017900080624     c                   endif
018000080624     c                   if        §DSTTSTPDA = *blank
018100080331     c     kqdt          chain     fiqdt01l
018200080331     c                   if        %found(fiqdt01l)
018300080331     c                   eval      QDTNBNAF  = §cusnrbaaf
018400080331     c                   update    fiqdt000
018500080331     c                   endif
018600080624     c                   endif
018700080331
018800080222     c                   endsr
018900080222
019000080728      * ?_______________________________________________________________________
019100080728      * decodifica SPC
019200080728      * ?_______________________________________________________________________
019300080728     c     decspc        begsr
019400080728      * recupero tabella codici SPC (PSP)
019500081211      * chiave 1 per le prime 11 password
019600080728     c                   clear                   tibs02ds
019700080728     c                   clear                   dpsp
019800080728     c                   eval      t02mod = 'C'
019900080728     c                   eval      t02sif = knsif
020000080728     c                   eval      t02cod = 'PSP'
020100080728     c                   movel     §CUSFGS       t02ke1
020200081211     c                   eval      t02ke2 = '1'
020300080728     c                   call      'TIBS02R'
020400080728     c                   parm                    kpjba
020500080728     c                   parm                    tibs02ds
020600080728     c                   if        t02err = *blanks
020700080728     c                   eval      dpsp = t02uni
020800080728     c                   endif
020900080728
021000080728     c                   if        dpsp <> *blanks
021100080728     c                   eval      codspc(01) = §pspcod01
021200080728     c                   eval      codspc(02) = §pspcod02
021300080728     c                   eval      codspc(03) = §pspcod03
021400080728     c                   eval      codspc(04) = §pspcod04
021500080728     c                   eval      codspc(05) = §pspcod05
021600080728     c                   eval      codspc(06) = §pspcod06
021700080728     c                   eval      codspc(07) = §pspcod07
021800080728     c                   eval      codspc(08) = §pspcod08
021900080728     c                   eval      codspc(09) = §pspcod09
022000080728     c                   eval      codspc(10) = §pspcod10
022100081211     c                   eval      codspc(11) = §pspcod11
022200080728     c                   eval      desspc(01) = §pspdes01
022300080728     c                   eval      desspc(02) = §pspdes02
022400080728     c                   eval      desspc(03) = §pspdes03
022500080728     c                   eval      desspc(04) = §pspdes04
022600080728     c                   eval      desspc(05) = §pspdes05
022700080728     c                   eval      desspc(06) = §pspdes06
022800080728     c                   eval      desspc(07) = §pspdes07
022900080728     c                   eval      desspc(08) = §pspdes08
023000080728     c                   eval      desspc(09) = §pspdes09
023100080728     c                   eval      desspc(10) = §pspdes10
023200081211     c                   eval      desspc(11) = §pspdes11
023300080728     c                   endif
023400081211      * chiave 2 per le altre 11 password
023500081211     c                   clear                   dpsp
023600081211     c                   eval      t02ke2 = '2'
023700081211     c                   call      'TIBS02R'
023800081211     c                   parm                    kpjba
023900081211     c                   parm                    tibs02ds
024000081211     c                   if        t02err = *blanks
024100081211     c                   eval      dpsp = t02uni
024200081211     c                   endif
024300081211
024400081211     c                   if        dpsp <> *blanks
024500081211     c                   eval      codspc(12) = §pspcod01
024600081211     c                   eval      desspc(12) = §pspdes01
024700081211     c                   eval      codspc(13) = §pspcod02
024800081211     c                   eval      desspc(13) = §pspdes02
024900081211     c                   eval      codspc(14) = §pspcod03
025000081211     c                   eval      desspc(14) = §pspdes03
025100081211     c                   eval      codspc(15) = §pspcod04
025200081211     c                   eval      desspc(15) = §pspdes04
025300081211     c                   eval      codspc(16) = §pspcod05
025400081211     c                   eval      desspc(16) = §pspdes05
025500081211     c                   eval      codspc(17) = §pspcod06
025600081211     c                   eval      desspc(17) = §pspdes06
025700081211     c                   eval      codspc(18) = §pspcod07
025800081211     c                   eval      desspc(18) = §pspdes07
025900081211     c                   eval      codspc(19) = §pspcod08
026000081211     c                   eval      desspc(19) = §pspdes08
026100081211     c                   eval      codspc(20) = §pspcod09
026200081211     c                   eval      desspc(20) = §pspdes09
026300081211     c                   eval      codspc(21) = §pspcod10
026400081211     c                   eval      desspc(21) = §pspdes10
026500081211     c                   eval      codspc(22) = §pspcod11
026600081211     c                   eval      desspc(22) = §pspdes11
026700081211     c                   endif
026800081211
026900080728     c                   endsr
027000080331      * ?_______________________________________________________________________
027100080222      * ROUTINE INIZIALE
027200080331      * ?_______________________________________________________________________
027300080222     c     sr_prima      begsr
027400080222     c* parametri
027500080222     c     *entry        plist
027600080222     c                   parm                    fiprdds
027700080222     c                   parm                    esito
027800080222     c                   clear                   esito
027900080331     c                   eval      fiprdcusds=prddati
028000080331     c     kdsf          klist
028100080331     c                   kfld                    cat4
028200080331     c                   kfld                    §CUSNDC
028300080331     c                   kfld                    §CUSFGS
028400080331     c                   kfld                    fase
028500080222
028600080624     c     kdst          klist
028700080624     c                   kfld                    cat4
028800080624     c                   kfld                    §CUSNDC
028900080624     c                   kfld                    §CUSFGS
029000080624
029100080331     c     kqdt          klist
029200080331     c                   kfld                    §CUSFGS
029300080331     c                   kfld                    §CUSNDC
029400080222     c                   endsr
029500080222      *--------------------------------------------------------------
029600080222      * fine
029700080222      *--------------------------------------------------------------
029800080222     c     sr_fine       begsr
029900080222     c                   seton                                        rt
030000080222     c                   return
030100080222     c                   endsr
