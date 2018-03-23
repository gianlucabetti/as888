000100080222     h decedit('0,') option(*nodebugio)
000200080222      *---------------------------------------------------------------*
000300080401      * Da FIPRD00R scrive file FIPCD00F per chiusura distinta e log
000400080227      * non deve fare commit o rolback perchè lo fa il chianmate (TRTCPC0R)
000500080222      *---------------------------------------------------------------*
000600080331     ftabel00f  if   e           k disk
000700080709     ffnart27l  if   e           k disk
000800080226     ffiplg00f  o    e             disk    commit
000900080805     ffipcd03l  uf a e           k disk    commit
001000080222     d esito           s              1
001100080222     d prmfgs          s              3
001200080222     d prmnfv          s              6
001300080225     d prmaut          s              7
001400110606     d prmdfv          s              8
001500080222     d prmdoc          s             16
001600080222     d prmdatora       s             14
001700080805     d codaut          s                   like(pcdcodaut)
001800080222     d* output cod. errore
001900110606     d saverr          s              1
002000110606     d prmerr          s              1
002100080222     d prmmsg          s             30
002200121015
002300121015     d wDataOra        s             12
002400080222
002500080222     d fiprdds       e ds                  extname(fiprd00r)
002600080331     d fiprdcedds    e ds
002700080331     d ds3e          e ds
002800080401     d fipcDcedds    e ds
002900080222     c                   exsr      sr_prima
003000080222
003100080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
003200080225      * fine
003300080222     c                   exsr      sr_ctrdati
003400080227      * scrivo file esiti per chiusura distinta
003500080227     c                   exsr      sr_write
003600080416      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
003700080416     c                   exsr      sr_wrtlog
003800080227     c* fine
003900080227     c                   exsr      sr_fine
004000080227     c*
004100080222      *---------------------------------------------------------------------
004200080222      * ROUTINE CONTROLLO DATI FIPRD
004300080222      *---------------------------------------------------------------------
004400080222     c     sr_ctrdati    begsr
004500080222     c                   clear                   fiplg000
004600080222     c                   clear                   prmerr
004700080222     c                   clear                   prmmsg
004800080226      * controllo se esiste la distinta e se dell'aut.
004900080222     c                   exsr      sr_ctrdst
005000080222     c                   if        prmerr <> ' '
005100080222     c                   eval      plgmsg = prmmsg
005200080222     c                   exsr      sr_wrtlog
005300080222     c                   end
005400080331     c                   move      §cedfgs       plgfgs
005500080331     c                   move      §cedndc       plgndc
005600080226      * controllo spedizione e se in distinta
005700080222     c                   exsr      sr_ctrdoc
005800080222     c                   if        prmerr <> ' '
005900080222     c                   eval      plgmsg = prmmsg
006000080222     c                   exsr      sr_wrtlog
006100080222     c                   end
006200080401      * controllo segnacollo
006300080709     c     kart          chain     fnart27l
006400080709     c                   if        not %found(fnart27l)
006500080408     c                   eval      plgmsg = 'SEGNACOLLO NON TROVATO IN FNART00F'
006600080401     c                   exsr      sr_wrtlog
006700080401     c                   end
006800080222      * data ora pda conferma
006900080331     c                   eval      prmdatora = %subst(prddati:44:14)
007000080222     c                   exsr      sr_ctrdop
007100110606     c                   if        prmerr =  'E'
007200080222     c                   eval      plgmsg = prmmsg
007300080331     c                   move      'PDA'         plgmsg
007400080222     c                   exsr      sr_wrtlog
007500080222     c                   end
007600110606     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
007700121015     c* o superiore 10 gg per cui forzo data distinta e ora ricezione
007800110606     c                   eval      saverr = prmerr
007900110609     c                   if        saverr = 'F'
008000121015     c                   eval      wDataOra = %subst(PRDdatora:1:12)
008100121015     c                   movel     wDataOra      §CEDdtorin
008200110609     c                   end
008300080331      * codice anomalia
008400080331     c                   if        §cedcancon <> ' '
008500080401     c                   eval      tblkey = §cedcancon
008600080401     c                   eval      tblcod = '3E'
008700080331     c     ktab          chain     tabel00f
008800080331     c                   if        not %found(tabel00f)
008900080408     c                   eval      plgmsg = 'ANOM.COLLO NO IN TABEL00F'
009000080331     c                   exsr      sr_wrtlog
009100080331     c                   end
009200080331     c                   end
009300080331     c*
009400080331     c                   endsr
009500080222      *---------------------------------------------------------------------
009600080222      * controlla distinta
009700080222      *---------------------------------------------------------------------
009800080222     c     sr_ctrdst     begsr
009900080222     c                   eval      prmfgs = %subst(prddati:1:3)
010000080222     c                   eval      prmnfv = %subst(prddati:4:6)
010100080225     c                   eval      prmaut = %subst(prdprfc:3:7)
010200110606     c                   eval      prmdfv = *all'0'
010300080222     c                   call      'TRTCPDSTR'
010400080222     c                   parm                    prmfgs
010500080222     c                   parm                    prmnfv
010600080225     c                   parm                    prmaut
010700110606     c                   parm                    prmdfv
010800080222     c                   parm                    prmerr
010900080222     c                   parm                    prmmsg
011000080222     c                   endsr
011100080222      *---------------------------------------------------------------------
011200080222      * controlla id documento
011300080222      *---------------------------------------------------------------------
011400080222     c     sr_ctrdoc     begsr
011500080222     c                   eval      prmdoc = %subst(prddati:10:16)
011600080225     c                   eval      prmfgs = %subst(prddati:1:3)
011700080225     c                   eval      prmnfv = %subst(prddati:4:6)
011800080225     c                   call      'TRTCPDOCR'
011900080222     c                   parm                    prmdoc
012000080225     c                   parm                    prmfgs
012100080225     c                   parm                    prmnfv
012200080222     c                   parm                    prmerr
012300080222     c                   parm                    prmmsg
012400080222     c                   endsr
012500080222      *---------------------------------------------------------------------
012600080222      * controlla data ora pda conferma
012700080222      *---------------------------------------------------------------------
012800080222     c     sr_ctrdop     begsr
012900080225     c                   call      'TRTCPDOPR'
013000080222     c                   parm                    prmdatora
013100080222     c                   parm                    prmerr
013200080222     c                   parm                    prmmsg
013300080222     c                   endsr
013400080222      *---------------------------------------------------------------------
013500080222      * scrive log
013600080222      *---------------------------------------------------------------------
013700080222     c     sr_wrtlog     begsr
013800080225     c* se sto scrivendo un errore imposto anche il flag
013900080225     c                   if        plgmsg <> ' '
014000080222     c                   eval      plgerr = 'E'
014100080225     c                   else
014200080225     c                   clear                   plgerr
014300080225     c                   end
014400121015     c* segnalo se forzata data ricezione errata perchè antecedente di 10 gg
014500121015      * o superiore di 10 gg.
014600110607     c                   if        saverr = 'F'
014700110607     c                   if        plgmsg = ' '
014800121015     c                   eval      PLGmsg  = 'DATA < o > di 10 GG'
014900110607     c                   end
015000110607     c                   clear                   saverr
015100110607     c                   end
015200080222     c                   eval      plgtiporcd = prdtiporcd
015300080408     c                   eval      PLGTIPAPPL = 'C'
015400080222     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
015500080222     c                   eval      plgprfc = prdprfc
015600080222     c                   eval      plgiddisp = prdiddisp
015700080222     c                   eval      plgdatora = prddatora
015800080222     c                   eval      plgdati = prddati
015900080225     c                   eval      plgiddoc = %subst(prddati:10:16)
016000080222     c                   write     fiplg000
016100080225     c* se sto scrivendo un errore vado a fine
016200080225     c                   if        plgmsg <> ' '
016300080222     c                   exsr      sr_fine
016400080225     c                   end
016500080227     c                   endsr
016600080227
016700080227      *----------------------------------------------------------------
016800080227      * scrittura file esiti per chiusura distinta
016900080227      *----------------------------------------------------------------
017000080227     c     sr_write      begsr
017100080401     c*
017200080401     c* verifico se ho già scritto il record
017300080401     c*
017400080807     c     kpcd02        setgt     fipcd03l
017500080807     c     kpcd02        readpe(e) fipcd03l
017600080401     c*
017700080401     c                   if        %error
017800080401     c                   eval      esito = 'E'
017900080401     c                   exsr      sr_fine
018000080401     c                   end
018100080519     c*
018200080807     c                   if        not %eof(fipcd03l)
018300080519     c* controllo sequenza data ora pda, nel caso non aggiorno scrivo log
018400080519     c* se quella trasmessa è > di quella che sto ricevendo ed esco
018500080519     c                   if        pcddtorin > §ceddtorin
018600080519     c                   eval      plgmsg = 'DATA/ORA FIPCD00F > DATA RICEV.'
018700080519     c                   exsr      sr_wrtlog
018800080519     c                   end
018900080519     c                   end
019000080401     c*
019100080401     c                   clear                   pcdatb
019200080401     c                   eval      pcdfgs = §cedfgs
019300080401     c                   eval      pcdndc = §cedndc
019400080805     c                   eval      pcdcodaut = %dec(%subst(prdprfc:3:7):7:0)
019500080401     c                   eval      pcdaas = §cedaas
019600080401     c                   eval      pcdlnp = §cedlnp
019700080401     c                   eval      pcdnrs = §cednrs
019800080401     c                   eval      pcdnsp = §cednsp
019900080401     c                   eval      PCDFLS  = §cedfls
020000080401     c                   eval      PCDLNA  = §cedlna
020100080401     c                   eval      PCDNRSER= §cednrser
020200080401     c                   eval      PCDNSC  = §cednsc
020300080401     c                   eval      pcdtrd = prdtiporcd
020400080304     c* flag di ricezione dati PDA
020500080401     c                   eval      PCdPDAAS4 = 'P'
020600080401     c                   eval      PCddtorin = §ceddtorin
020700110606     c                   if        saverr = 'F'
020800121015     c                   eval      wDataOra = %subst(PRDdatora:1:12)
020900121015     c                   movel     wDataOra      PCDdtorin
021000110606     c                   end
021100080430     c                   eval      §PCDSPUCON=§cedSPUCON
021200080430     c                   eval      §PCDSPUSPC=§cedSPUSPC
021300080430     c                   eval      §PCDCANCON=§cedCANCON
021400080430     c                   eval      §PCDFLGSPC=§cedFLGSPC
021500080430     c                   eval      PCddati = fipcdcedds
021600080227     c*
021700080807     c                   if        %eof(fipcd03l)
021800080401     c                   write     fipcd000
021900080227     c                   else
022000080401     c                   update    fipcd000
022100080227     c                   end
022200080222     c                   endsr
022300080222
022400080222      *----------------------------------------------------------------
022500080222      * ROUTINE INIZIALE
022600080222      *----------------------------------------------------------------
022700080222     c     sr_prima      begsr
022800080222     c* parametri
022900080222     c     *entry        plist
023000080222     c                   parm                    fiprdds
023100080222     c                   parm                    esito
023200080222     c                   clear                   esito
023300080401     c                   eval      fiprdcedds=prddati
023400080805     c                   eval      codaut = %dec(%subst(prdprfc:3:7):7:0)
023500080331     c                   eval      tblkut = 1
023600080331     c     ktab          klist
023700080331     c                   kfld                    tblkut
023800080331     c                   kfld                    tblcod
023900080331     c                   kfld                    tblkey
024000080401     c     kart          klist
024100080709     c                   kfld                    §cedfls
024200080709     c                   kfld                    §cedlna
024300080401     c                   kfld                    §cedNRS
024400080401     c                   kfld                    §cedNSc
024500080401     c     kpcd02        klist
024600080805     c                   kfld                    §cedfgs
024700080805     c                   kfld                    §cedndc
024800080805     c                   kfld                    codaut
024900080805     c                   kfld                    prdtiporcd
025000080401     c                   kfld                    §cedAAS
025100080401     c                   kfld                    §cedLNP
025200080401     c                   kfld                    §cedNRS
025300080401     c                   kfld                    §cedNSP
025400080401     c                   kfld                    §cedNSc
025500080222
025600080222     c                   endsr
025700080401      *--------------------------------------------------------------
025800080401      * fine
025900080401      *--------------------------------------------------------------
026000080401     c     sr_fine       begsr
026100080222     c                   seton                                        rt
026200080222     c                   return
026300080222     c                   endsr
