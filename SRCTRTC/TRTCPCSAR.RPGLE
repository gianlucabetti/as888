000100170523     h decedit('0,') option(*nodebugio)
000200080222      *---------------------------------------------------------------*
000300080401      * Da FIPRD00R scrive file FIPCD00F per chiusura distinta e log
000400080227      * non deve fare commit o rolback perchè lo fa il chianmate (TRTCPC0R)
000500080222      *---------------------------------------------------------------*
000600080331     ftabel00f  if   e           k disk
000700080709     ffnart27l  if   e           k disk
000800080226     ffiplg00f  o    e             disk    commit
000900080222     d esito           s              1
001000080222     d prmfgs          s              3
001100080222     d prmnfv          s              6
001200080225     d prmaut          s              7
001300110606     d prmdfv          s              8
001400080222     d prmdoc          s             16
001500170523     d prmdoc1         s              1
001600080222     d prmdatora       s             14
001700170523     d codaut          s              7  0
001800080222     d* output cod. errore
001900110606     d saverr          s              1
002000110606     d prmerr          s              1
002100080222     d prmmsg          s             30
002200121015
002300121015     d wDataOra        s             12
002400080222
002500080222     d fiprdds       e ds                  extname(fiprd00r)
002501170731     d fiprdds£      e ds                  extname(fiprd00r) prefix(£)
002600170523     d fiprdcsads    e ds
002700170607     d fiprdcedds    e ds
002800170523     d ds3e          e ds
002900080222     c                   exsr      sr_prima
003000080222
003100080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
003200080225      * fine
003300080222     c                   exsr      sr_ctrdati
003400170731      * scrivo file esiti CED per chiusura distinta solo se spunta
003401170731      * di rientro e di collo in distinta
003500170728     c                   if        §csafls <> 0 and §CSASPUAUT ='R'
003502170731     c                             and prmdoc1 <> '0'
003600170607     c                   exsr      sr_write
003700170607     C                   END
003800080416      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
003900080416     c                   exsr      sr_wrtlog
004000080227     c* fine
004100080227     c                   exsr      sr_fine
004200080227     c*
004300080222      *---------------------------------------------------------------------
004400080222      * ROUTINE CONTROLLO DATI FIPRD
004500080222      *---------------------------------------------------------------------
004600080222     c     sr_ctrdati    begsr
004700080222     c                   clear                   fiplg000
004800080222     c                   clear                   prmerr
004900080222     c                   clear                   prmmsg
005000080226      * controllo se esiste la distinta e se dell'aut.
005100080222     c                   exsr      sr_ctrdst
005200080222     c                   if        prmerr <> ' '
005300080222     c                   eval      plgmsg = prmmsg
005400080222     c                   exsr      sr_wrtlog
005500080222     c                   end
005600170523     c                   move      §csafgs       plgfgs
005700170523     c                   move      §csandc       plgndc
005800080226      * controllo spedizione e se in distinta
005900080222     c                   exsr      sr_ctrdoc
006000080222     c                   if        prmerr <> ' '
006100080222     c                   eval      plgmsg = prmmsg
006200080222     c                   exsr      sr_wrtlog
006300080222     c                   end
006400170523      * controllo segnacollo. può arrivare a zero se spunta un collo non in
006500170523      * distinta
006600170523     c                   if        §csafls <> 0
006700080709     c     kart          chain     fnart27l
006800080709     c                   if        not %found(fnart27l)
006900080408     c                   eval      plgmsg = 'SEGNACOLLO NON TROVATO IN FNART00F'
007000080401     c                   exsr      sr_wrtlog
007100080401     c                   end
007200170523     c                   end
007300080222      * data ora pda conferma
007400170720     c                   eval      prmdatora = %subst(prddati:43:14)
007500080222     c                   exsr      sr_ctrdop
007600110606     c                   if        prmerr =  'E'
007700080222     c                   eval      plgmsg = prmmsg
007800080331     c                   move      'PDA'         plgmsg
007900080222     c                   exsr      sr_wrtlog
008000080222     c                   end
008100170523     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
008200121015     c* o superiore 10 gg per cui forzo data distinta e ora ricezione
008300110606     c                   eval      saverr = prmerr
008400110609     c                   if        saverr = 'F'
008500121015     c                   eval      wDataOra = %subst(PRDdatora:1:12)
008600170523     c                   movel     wDataOra      §csadtorin
008700110609     c                   end
008800080331      * codice anomalia
008900170523     c                   if        §csacancon <> ' '
009000170523     c                   eval      tblkey = §csacancon
009100080401     c                   eval      tblcod = '3E'
009200080331     c     ktab          chain     tabel00f
009300080331     c                   if        not %found(tabel00f)
009400080408     c                   eval      plgmsg = 'ANOM.COLLO NO IN TABEL00F'
009500080331     c                   exsr      sr_wrtlog
009600080331     c                   end
009700080331     c                   end
009800080331     c*
009900080331     c                   endsr
010000080222      *---------------------------------------------------------------------
010100080222      * controlla distinta
010200080222      *---------------------------------------------------------------------
010300080222     c     sr_ctrdst     begsr
010400080222     c                   eval      prmfgs = %subst(prddati:1:3)
010500080222     c                   eval      prmnfv = %subst(prddati:4:6)
010600080225     c                   eval      prmaut = %subst(prdprfc:3:7)
010700110606     c                   eval      prmdfv = *all'0'
010800080222     c                   call      'TRTCPDSTR'
010900080222     c                   parm                    prmfgs
011000080222     c                   parm                    prmnfv
011100080225     c                   parm                    prmaut
011200110606     c                   parm                    prmdfv
011300080222     c                   parm                    prmerr
011400080222     c                   parm                    prmmsg
011500080222     c                   endsr
011600080222      *---------------------------------------------------------------------
011700080222      * controlla id documento
011800080222      *---------------------------------------------------------------------
011900080222     c     sr_ctrdoc     begsr
012000170731     c                   eval      prmdoc1= %subst(prddati:10:1)
012100170523     c                   eval      prmdoc = %subst(prddati:10:16)
012200080225     c                   eval      prmfgs = %subst(prddati:1:3)
012300080225     c                   eval      prmnfv = %subst(prddati:4:6)
012400170523     c* può arrivare una spunta di un collo che non appartiene alla distinta
012500170523     c* quindi tutto a 0
012600170523     c                   if        prmdoc1 <> ' ' and prmdoc1 <> '0'
012700080225     c                   call      'TRTCPDOCR'
012800080222     c                   parm                    prmdoc
012900080225     c                   parm                    prmfgs
013000080225     c                   parm                    prmnfv
013100080222     c                   parm                    prmerr
013200080222     c                   parm                    prmmsg
013300170523     c                   end
013400080222     c                   endsr
013500080222      *---------------------------------------------------------------------
013600080222      * controlla data ora pda conferma
013700080222      *---------------------------------------------------------------------
013800080222     c     sr_ctrdop     begsr
013900080225     c                   call      'TRTCPDOPR'
014000080222     c                   parm                    prmdatora
014100080222     c                   parm                    prmerr
014200080222     c                   parm                    prmmsg
014300080222     c                   endsr
014400080222      *---------------------------------------------------------------------
014500080222      * scrive log
014600080222      *---------------------------------------------------------------------
014700170607     c     sr_write      begsr
014800170607     c                   clear                   fiprdcedds
014900170607     c                   eval      §CEDFGS    = §CsaFGS
015000170607     c                   eval      §CEDNDC    = §CsaNDC
015100170607     c                   eval      §CEDAAS    = §CsaAAS
015200170607     c                   eval      §CEDLNP    = §CsaLNP
015300170607     c                   eval      §CEDNRS    = §CsaNRS
015400170607     c                   eval      §CEDNSP    = §CsaNSP
015500170607     c                   eval      §CEDFLS    = §CsaFLS
015600170607     c                   eval      §CEDLNA    = §CsaLNA
015700170607     c                   eval      §CEDNRSER  = §CsaNRSER
015800170607     c                   eval      §CEDNSC    = §CsaNSC
015900170607     c                   eval      §CEDSPUSPC = 'S'
016000170607     c                   eval      §CEDCANCON = §CsaCANCON
016100170607     c                   eval      §CEDDTORIN = §CsaDTORIN
016200170607     c                   eval      §CEDFLGSPC = 'S'
016300170731     c                   eval      £PRDTIPORCD= 'CED'
016301170731     c                   eval      £PRDPRFC   =PRDPRFC
016302170731     c                   eval      £PRDIDDISP =PRDIDDISP
016303170731     c                   eval      £PRDDATI   =fiprdcedds
016304170731     c                   eval      £PRDDATORA =PRDDATORA
016400170607     c                   CALL      'TRTCPCEDR'
016500170731     C                   PARM                    FIPRDDS£
016600170607     C                   PARM                    ESITO
016700170607     c                   endsr
016800170607      *---------------------------------------------------------------------
016900170607      * scrive log
017000170607      *---------------------------------------------------------------------
017100170607     c     sr_wrtlog     begsr
017200080225     c* se sto scrivendo un errore imposto anche il flag
017300080225     c                   if        plgmsg <> ' '
017400080222     c                   eval      plgerr = 'E'
017500080225     c                   else
017600080225     c                   clear                   plgerr
017700080225     c                   end
017800170523     c* segnalo se forzata data ricezione errata perchè antecsaente di 10 gg
017900121015      * o superiore di 10 gg.
018000110607     c                   if        saverr = 'F'
018100110607     c                   if        plgmsg = ' '
018200121015     c                   eval      PLGmsg  = 'DATA < o > di 10 GG'
018300110607     c                   end
018400110607     c                   clear                   saverr
018500110607     c                   end
018600080222     c                   eval      plgtiporcd = prdtiporcd
018700080408     c                   eval      PLGTIPAPPL = 'C'
018800080222     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
018900080222     c                   eval      plgprfc = prdprfc
019000080222     c                   eval      plgiddisp = prdiddisp
019100080222     c                   eval      plgdatora = prddatora
019200080222     c                   eval      plgdati = prddati
019300080225     c                   eval      plgiddoc = %subst(prddati:10:16)
019400080222     c                   write     fiplg000
019500080225     c* se sto scrivendo un errore vado a fine
019600080225     c                   if        plgmsg <> ' '
019700080222     c                   exsr      sr_fine
019800080225     c                   end
019900080227     c                   endsr
020000080227
020100170526     c*----------------------------------------------------------------
020200170526     c* ROUTINE INIZIALE
020300170526     c*----------------------------------------------------------------
020400170526     c     sr_prima      begsr
020500170526     c* parametri
020600080222     c     *entry        plist
020700080222     c                   parm                    fiprdds
020800080222     c                   parm                    esito
020900080222     c                   clear                   esito
020901170731     c                   eval      prmdoc1= %subst(prddati:10:1)
020902170731     c* inizializzo a 0 i riferimenti della spedizione.
020903170731     c* arrivano a blanks quando la spunta appartiene ad un collo non in
020904170731     c* distinta
020905170731     c                   if        prmdoc1 = ' '
020906170731     c                   eval      %subst(prddati:10:16)='0000000000000000'
020907170731     c                   end
021000170523     c                   eval      fiprdcsads=prddati
021100080805     c                   eval      codaut = %dec(%subst(prdprfc:3:7):7:0)
021200080331     c                   eval      tblkut = 1
021300080331     c     ktab          klist
021400080331     c                   kfld                    tblkut
021500080331     c                   kfld                    tblcod
021600080331     c                   kfld                    tblkey
021700080401     c     kart          klist
021800170523     c                   kfld                    §csafls
021900170523     c                   kfld                    §csalna
022000170731     c                   kfld                    §csaNRSER
022100170523     c                   kfld                    §csaNSc
022200080401     c     kpcd02        klist
022300170523     c                   kfld                    §csafgs
022400170523     c                   kfld                    §csandc
022500080805     c                   kfld                    codaut
022600080805     c                   kfld                    prdtiporcd
022700170523     c                   kfld                    §csaAAS
022800170523     c                   kfld                    §csaLNP
022900170523     c                   kfld                    §csaNRS
023000170523     c                   kfld                    §csaNSP
023100170523     c                   kfld                    §csaNSc
023200080222
023300080222     c                   endsr
023400080401      *--------------------------------------------------------------
023500080401      * fine
023600080401      *--------------------------------------------------------------
023700080401     c     sr_fine       begsr
023800080222     c                   seton                                        rt
023900080222     c                   return
024000080222     c                   endsr
