000100080222     h decedit('0,') option(*nodebugio)
000200080222      *---------------------------------------------------------------*
000300170526      * Da FIPRD00R scrive file FIDSF00F per inserimento fase PIN
000400080331      * non deve fare commit o rolback perchè lo fa il chiamante (TRTCPC0R)
000500080222      *---------------------------------------------------------------*
000600080226     ffiplg00f  o    e             disk    commit
000700080331     ffidsf02l  uf a e           k disk    commit
000800080331     d tmp             s             14
000900080222     d esito           s              1
001000080222     d prmfgs          s              3
001100080222     d prmnfv          s              6
001200080225     d prmaut          s              7
001300110606     d prmdfv          s              8
001400080222     d prmdoc          s             16
001500080222     d prmdatora       s             14
001600080222     d* output cod. errore
001700110606     d saverr          s              1
001800080222     d prmerr          s              1
001900080222     d prmmsg          s             30
002000080222
002100080331     d cat4            s              1  0 inz(4)
002200170526     d fase            s              3    inz('PIN')
002300080222     d fiprdds       e ds                  extname(fiprd00r)
002400170526     d fiprdpinds    e ds
002500170526     d fidsfpinds    e ds
002600080728     d dpsp          e ds
002700080728     d tibs02ds      e ds
002800080728     d kpjba         e ds
002900080728     d parmpo          s              3
003000081211     d codspc          s              2    dim(22)
003100081211     d desspc          s             15    dim(22)
003200080331      * ?_______________________________________________________________________
003300080222     c                   exsr      sr_prima
003400080222
003500080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
003600080225      * fine
003700080222     c                   exsr      sr_ctrdati
003800080227      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
003900080225     c                   exsr      sr_wrtlog
004000080401      * scrivo file fasi e rientri
004100080227     c                   exsr      sr_write
004200080227     c* fine
004300080227     c                   exsr      sr_fine
004400080227     c*
004500080331      * ?_______________________________________________________________________
004600080222      * ROUTINE CONTROLLO DATI FIPRD
004700080331      * ?_______________________________________________________________________
004800080222     c     sr_ctrdati    begsr
004900080222     c                   clear                   fiplg000
005000080222     c                   clear                   prmerr
005100080222     c                   clear                   prmmsg
005200080226      * controllo se esiste la distinta e se dell'aut.
005300080222     c                   exsr      sr_ctrdst
005400080222     c                   if        prmerr <> ' '
005500080222     c                   eval      plgmsg = prmmsg
005600080222     c                   exsr      sr_wrtlog
005700080222     c                   end
005800170526     c                   move      §pinfgs       plgfgs
005900170526     c                   move      §pinndc       plgndc
006000080222      * data ora pda conferma
006100080222     c                   exsr      sr_ctrdop
006200110606     c                   if        prmerr =  'E'
006300080222     c                   eval      plgmsg = prmmsg
006400080222     c                   exsr      sr_wrtlog
006500080222     c                   end
006600110606     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
006700121016     c* 10 gg oppure superiore 10 gg. per cui forzo data distinta e ora ricezione
006800110606     c                   eval      saverr = prmerr
006900170526     c* verifico se fase pin già presente sul file delle fasi
007000080429     c*    kdsf          chain     fidsf02l
007100080429     c*                  if        %found(fidsf02l)
007200080429     c*                  eval      plgmsg = 'FASE CONTROLLO USCITA GIA PRESENTE'
007300080429     c*                  exsr      sr_wrtlog
007400080429     c*                  end
007500080222     c                   endsr
007600080222      *---------------------------------------------------------------------
007700080222      * controlla distinta
007800080222      *---------------------------------------------------------------------
007900080222     c     sr_ctrdst     begsr
008000080222     c                   eval      prmfgs = %subst(prddati:1:3)
008100080222     c                   eval      prmnfv = %subst(prddati:4:6)
008200080225     c                   eval      prmaut = %subst(prdprfc:3:7)
008300110606     c                   eval      prmdfv = *all'0'
008400080222     c                   call      'TRTCPDSTR'
008500080222     c                   parm                    prmfgs
008600080222     c                   parm                    prmnfv
008700080225     c                   parm                    prmaut
008800110606     c                   parm                    prmdfv
008900080222     c                   parm                    prmerr
009000080222     c                   parm                    prmmsg
009100080222     c                   endsr
009200080222      *---------------------------------------------------------------------
009300080222      * controlla data ora pda conferma
009400080222      *---------------------------------------------------------------------
009500080222     c     sr_ctrdop     begsr
009600170720     c                   eval      prmdatora = %subst(prddati:10:14)
009700080225     c                   call      'TRTCPDOPR'
009800080222     c                   parm                    prmdatora
009900080222     c                   parm                    prmerr
010000080222     c                   parm                    prmmsg
010100080222     c                   endsr
010200080222      *---------------------------------------------------------------------
010300080222      * scrive log
010400080222      *---------------------------------------------------------------------
010500080222     c     sr_wrtlog     begsr
010600080225     c* se sto scrivendo un errore imposto anche il flag
010700080225     c                   if        plgmsg <> ' '
010800080222     c                   eval      plgerr = 'E'
010900080225     c                   else
011000080225     c                   clear                   plgerr
011100080225     c                   end
011200121016     c* segnalo se forzata data ricezione errata perchè antecedente di 10 gg
011300121016      * o superiore a 10 gg.
011400110607     c                   if        saverr = 'F'
011500110607     c                   if        plgmsg = ' '
011600121016     c                   eval      plgmsg = 'DATA < o > di 10 GG'
011700110607     c                   end
011800110607     c                   clear                   saverr
011900110607     c                   end
012000080222     c                   eval      plgtiporcd = prdtiporcd
012100080222     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
012200080222     c                   eval      plgtiporcd = prdtiporcd
012300080222     c                   eval      plgprfc = prdprfc
012400080222     c                   eval      plgiddisp = prdiddisp
012500080222     c                   eval      plgdatora = prddatora
012600080222     c                   eval      plgdati = prddati
012700080331     c                   eval      plgiddoc = *BLANK
012800080401     c                   eval      plgtipappl = 'C'
012900080222     c                   write     fiplg000
013000080225     c* se sto scrivendo un errore vado a fine
013100080225     c                   if        plgmsg <> ' '
013200080222     c                   exsr      sr_fine
013300080225     c                   end
013400080227     c                   endsr
013500080227
013600080227      *----------------------------------------------------------------
013700080331      * scrittura file fase distinta e rientri
013800080227      *----------------------------------------------------------------
013900080227     c     sr_write      begsr
014000080624      *scrittura aggiornamento fase
014100080624     c     kdsf          chain     fidsf02l
014200080331     c                   clear                   fidsf000
014300170526     c                   clear                   fidsfpinds
014400170526     c                   eval      DSFfgs    =   §pinFGS
014500080331     c                   eval      DSFnpg    =   cat4
014600170526     c                   eval      DSFnfv    =   §pinNDC
014700080331     c                   eval      DSFtrd    =   fase
014800170526     c                   eval      §DSFSPCCOD=   §pinSPCCOD
014900080728     c* richiamo pgm per decodifica spc
015000080728     c                   exsr      decspc
015100080728      *
015200080728     c                   z-add     1             i                 2 0
015300170526     c     §pinSPCCOD    lookup    codspc(i)                              66
015400080728     c   66              eval      §DSFSPCDES=   desspc(i)
015500170526     c                   movel     FIDSFpinDS    DSFDATI
015600080728      *
015700080331     c                   eval      tmp= %char(%timestamp: *iso0)
015800080331     c                   move      tmp           DSFdtorin
015900080331     c                   eval      DSFPDAAS4 = 'P'
016000080624     c                   if        %found(fidsf02l)
016100080624     c                   update    fidsf000
016200080624     c                   else
016300080331     c                   write     fidsf000
016400080624     c                   endif
016500080331
016600080222     c                   endsr
016700080222
016800080728      * ?_______________________________________________________________________
016900080728      * decodifica SPC
017000080728      * ?_______________________________________________________________________
017100080728     c     decspc        begsr
017200080728      * recupero tabella codici SPC (PSP)
017300081211      * chiave 1 per le prime 11 password
017400080728     c                   clear                   tibs02ds
017500080728     c                   clear                   dpsp
017600080728     c                   eval      t02mod = 'C'
017700080728     c                   eval      t02sif = knsif
017800080728     c                   eval      t02cod = 'PSP'
017900170526     c                   movel     §pinFGS       t02ke1
018000081211     c                   eval      t02ke2 = '1'
018100080728     c                   call      'TIBS02R'
018200080728     c                   parm                    kpjba
018300080728     c                   parm                    tibs02ds
018400080728     c                   if        t02err = *blanks
018500080728     c                   eval      dpsp = t02uni
018600080728     c                   endif
018700080728
018800080728     c                   if        dpsp <> *blanks
018900080728     c                   eval      codspc(01) = §pspcod01
019000080728     c                   eval      codspc(02) = §pspcod02
019100080728     c                   eval      codspc(03) = §pspcod03
019200080728     c                   eval      codspc(04) = §pspcod04
019300080728     c                   eval      codspc(05) = §pspcod05
019400080728     c                   eval      codspc(06) = §pspcod06
019500080728     c                   eval      codspc(07) = §pspcod07
019600080728     c                   eval      codspc(08) = §pspcod08
019700080728     c                   eval      codspc(09) = §pspcod09
019800080728     c                   eval      codspc(10) = §pspcod10
019900081211     c                   eval      codspc(11) = §pspcod11
020000080728     c                   eval      desspc(01) = §pspdes01
020100080728     c                   eval      desspc(02) = §pspdes02
020200080728     c                   eval      desspc(03) = §pspdes03
020300080728     c                   eval      desspc(04) = §pspdes04
020400080728     c                   eval      desspc(05) = §pspdes05
020500080728     c                   eval      desspc(06) = §pspdes06
020600080728     c                   eval      desspc(07) = §pspdes07
020700080728     c                   eval      desspc(08) = §pspdes08
020800080728     c                   eval      desspc(09) = §pspdes09
020900080728     c                   eval      desspc(10) = §pspdes10
021000081211     c                   eval      desspc(11) = §pspdes11
021100080728     c                   endif
021200081211      * chiave 2 per le altre 11 password
021300081211     c                   clear                   dpsp
021400081211     c                   eval      t02ke2 = '2'
021500081211     c                   call      'TIBS02R'
021600081211     c                   parm                    kpjba
021700081211     c                   parm                    tibs02ds
021800081211     c                   if        t02err = *blanks
021900081211     c                   eval      dpsp = t02uni
022000081211     c                   endif
022100081211
022200081211     c                   if        dpsp <> *blanks
022300081211     c                   eval      codspc(12) = §pspcod01
022400081211     c                   eval      desspc(12) = §pspdes01
022500081211     c                   eval      codspc(13) = §pspcod02
022600081211     c                   eval      desspc(13) = §pspdes02
022700081211     c                   eval      codspc(14) = §pspcod03
022800081211     c                   eval      desspc(14) = §pspdes03
022900081211     c                   eval      codspc(15) = §pspcod04
023000081211     c                   eval      desspc(15) = §pspdes04
023100081211     c                   eval      codspc(16) = §pspcod05
023200081211     c                   eval      desspc(16) = §pspdes05
023300081211     c                   eval      codspc(17) = §pspcod06
023400081211     c                   eval      desspc(17) = §pspdes06
023500081211     c                   eval      codspc(18) = §pspcod07
023600081211     c                   eval      desspc(18) = §pspdes07
023700081211     c                   eval      codspc(19) = §pspcod08
023800081211     c                   eval      desspc(19) = §pspdes08
023900081211     c                   eval      codspc(20) = §pspcod09
024000081211     c                   eval      desspc(20) = §pspdes09
024100081211     c                   eval      codspc(21) = §pspcod10
024200081211     c                   eval      desspc(21) = §pspdes10
024300081211     c                   eval      codspc(22) = §pspcod11
024400081211     c                   eval      desspc(22) = §pspdes11
024500081211     c                   endif
024600081211
024700080728     c                   endsr
024800080331      * ?_______________________________________________________________________
024900080222      * ROUTINE INIZIALE
025000080331      * ?_______________________________________________________________________
025100080222     c     sr_prima      begsr
025200080222     c* parametri
025300080222     c     *entry        plist
025400080222     c                   parm                    fiprdds
025500080222     c                   parm                    esito
025600080222     c                   clear                   esito
025700170526     c                   eval      fiprdpinds=prddati
025800080331     c     kdsf          klist
025900080331     c                   kfld                    cat4
026000170526     c                   kfld                    §pinNDC
026100170526     c                   kfld                    §pinFGS
026200080331     c                   kfld                    fase
026300080222     c                   endsr
026400080222      *--------------------------------------------------------------
026500080222      * fine
026600080222      *--------------------------------------------------------------
026700080222     c     sr_fine       begsr
026800080222     c                   seton                                        rt
026900080222     c                   return
027000080222     c                   endsr
