000100080222     h decedit('0,') option(*nodebugio)
000200080222      *---------------------------------------------------------------*
000300080401      * Da FIPRD00R scrive file FIPCD00F per chiusura distinta e log
000301080227      * non deve fare commit o rolback perchè lo fa il chianmate (TRTCPC0R)
000400080222      *---------------------------------------------------------------*
000501080331     ftabel00f  if   e           k disk
000502080709     ffnart27l  if   e           k disk
000503080226     ffiplg00f  o    e             disk    commit
000600080805     ffipcd03l  uf a e           k disk    commit
000700080222     d esito           s              1
000800080222     d prmfgs          s              3
000900080222     d prmnfv          s              6
000901080225     d prmaut          s              7
000902110606     d prmdfv          s              8
001000080222     d prmdoc          s             16
001100080222     d prmdatora       s             14
001101080805     d codaut          s                   like(pcdcodaut)
001200080222     d* output cod. errore
001300110606     d saverr          s              1
001301110606     d prmerr          s              1
001400080222     d prmmsg          s             30
001500080222
001600080222     d fiprdds       e ds                  extname(fiprd00r)
001601080331     d fiprdcedds    e ds
001603080331     d ds3e          e ds
001605080401     d fipcDcedds    e ds
001700080222     c                   exsr      sr_prima
001800080222
001900080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
001901080225      * fine
002100080222     c                   exsr      sr_ctrdati
002504080227      * scrivo file esiti per chiusura distinta
002505080227     c                   exsr      sr_write
002506080416      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
002507080416     c                   exsr      sr_wrtlog
002508080227     c* fine
002509080227     c                   exsr      sr_fine
002510080227     c*
002900080222      *---------------------------------------------------------------------
003100080222      * ROUTINE CONTROLLO DATI FIPRD
003200080222      *---------------------------------------------------------------------
003300080222     c     sr_ctrdati    begsr
003400080222     c                   clear                   fiplg000
003500080222     c                   clear                   prmerr
003600080222     c                   clear                   prmmsg
003700080226      * controllo se esiste la distinta e se dell'aut.
003800080222     c                   exsr      sr_ctrdst
003900080222     c                   if        prmerr <> ' '
004000080222     c                   eval      plgmsg = prmmsg
004100080222     c                   exsr      sr_wrtlog
004200080222     c                   end
004300080331     c                   move      §cedfgs       plgfgs
004400080331     c                   move      §cedndc       plgndc
004500080226      * controllo spedizione e se in distinta
004600080222     c                   exsr      sr_ctrdoc
004700080222     c                   if        prmerr <> ' '
004800080222     c                   eval      plgmsg = prmmsg
004900080222     c                   exsr      sr_wrtlog
005000080222     c                   end
005001080401      * controllo segnacollo
005003080709     c     kart          chain     fnart27l
005004080709     c                   if        not %found(fnart27l)
005005080408     c                   eval      plgmsg = 'SEGNACOLLO NON TROVATO IN FNART00F'
005006080401     c                   exsr      sr_wrtlog
005009080401     c                   end
005100080222      * data ora pda conferma
005101080331     c                   eval      prmdatora = %subst(prddati:44:14)
005200080222     c                   exsr      sr_ctrdop
005300110606     c                   if        prmerr =  'E'
005400080222     c                   eval      plgmsg = prmmsg
005401080331     c                   move      'PDA'         plgmsg
005500080222     c                   exsr      sr_wrtlog
005600080222     c                   end
005601110606     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
005602110606     c* 30 gg per cui forzo data distinta e ora ricezione
005603110606     c                   eval      saverr = prmerr
005604110609     c                   if        saverr = 'F'
005605110609     c                   movel     prddatora     §ceddtorin
005606110609     c                   end
005607080331      * codice anomalia
005608080331     c                   if        §cedcancon <> ' '
005609080401     c                   eval      tblkey = §cedcancon
005610080401     c                   eval      tblcod = '3E'
005611080331     c     ktab          chain     tabel00f
005612080331     c                   if        not %found(tabel00f)
005613080408     c                   eval      plgmsg = 'ANOM.COLLO NO IN TABEL00F'
005614080331     c                   exsr      sr_wrtlog
005615080331     c                   end
005616080331     c                   end
007661080331     c*
007662080331     c                   endsr
007700080222      *---------------------------------------------------------------------
007900080222      * controlla distinta
008000080222      *---------------------------------------------------------------------
008200080222     c     sr_ctrdst     begsr
008300080222     c                   eval      prmfgs = %subst(prddati:1:3)
008400080222     c                   eval      prmnfv = %subst(prddati:4:6)
008402080225     c                   eval      prmaut = %subst(prdprfc:3:7)
008403110606     c                   eval      prmdfv = *all'0'
008500080222     c                   call      'TRTCPDSTR'
008600080222     c                   parm                    prmfgs
008700080222     c                   parm                    prmnfv
008701080225     c                   parm                    prmaut
008702110606     c                   parm                    prmdfv
008800080222     c                   parm                    prmerr
008900080222     c                   parm                    prmmsg
009000080222     c                   endsr
009100080222      *---------------------------------------------------------------------
009200080222      * controlla id documento
009300080222      *---------------------------------------------------------------------
009400080222     c     sr_ctrdoc     begsr
009500080222     c                   eval      prmdoc = %subst(prddati:10:16)
009501080225     c                   eval      prmfgs = %subst(prddati:1:3)
009502080225     c                   eval      prmnfv = %subst(prddati:4:6)
009600080225     c                   call      'TRTCPDOCR'
009700080222     c                   parm                    prmdoc
009701080225     c                   parm                    prmfgs
009702080225     c                   parm                    prmnfv
009800080222     c                   parm                    prmerr
009900080222     c                   parm                    prmmsg
010000080222     c                   endsr
010100080222      *---------------------------------------------------------------------
010200080222      * controlla data ora pda conferma
010300080222      *---------------------------------------------------------------------
010400080222     c     sr_ctrdop     begsr
010700080225     c                   call      'TRTCPDOPR'
010800080222     c                   parm                    prmdatora
010900080222     c                   parm                    prmerr
011000080222     c                   parm                    prmmsg
011100080222     c                   endsr
011200080222      *---------------------------------------------------------------------
011300080222      * scrive log
011400080222      *---------------------------------------------------------------------
011500080222     c     sr_wrtlog     begsr
011501080225     c* se sto scrivendo un errore imposto anche il flag
011502080225     c                   if        plgmsg <> ' '
011600080222     c                   eval      plgerr = 'E'
011601080225     c                   else
011602080225     c                   clear                   plgerr
011603080225     c                   end
011604110607     c* segnalo se forzata data ricezione errata perchè antecedente di 30 gg
011605110607     c                   if        saverr = 'F'
011606110607     c                   if        plgmsg = ' '
011607110607     c                   move      'DATA < 30 GG'plgmsg
011608110607     c                   end
011609110607     c                   clear                   saverr
011610110607     c                   end
011700080222     c                   eval      plgtiporcd = prdtiporcd
011701080408     c                   eval      PLGTIPAPPL = 'C'
011800080222     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
012000080222     c                   eval      plgprfc = prdprfc
012100080222     c                   eval      plgiddisp = prdiddisp
012200080222     c                   eval      plgdatora = prddatora
012300080222     c                   eval      plgdati = prddati
012401080225     c                   eval      plgiddoc = %subst(prddati:10:16)
012500080222     c                   write     fiplg000
012501080225     c* se sto scrivendo un errore vado a fine
012502080225     c                   if        plgmsg <> ' '
012600080222     c                   exsr      sr_fine
012601080225     c                   end
012602080227     c                   endsr
012603080227
012604080227      *----------------------------------------------------------------
012605080227      * scrittura file esiti per chiusura distinta
012606080227      *----------------------------------------------------------------
012607080227     c     sr_write      begsr
012640080401     c*
012641080401     c* verifico se ho già scritto il record
012642080401     c*
012643080807     c     kpcd02        setgt     fipcd03l
012644080807     c     kpcd02        readpe(e) fipcd03l
012645080401     c*
012646080401     c                   if        %error
012647080401     c                   eval      esito = 'E'
012648080401     c                   exsr      sr_fine
012649080401     c                   end
012650080519     c*
012651080807     c                   if        not %eof(fipcd03l)
012652080519     c* controllo sequenza data ora pda, nel caso non aggiorno scrivo log
012653080519     c* se quella trasmessa è > di quella che sto ricevendo ed esco
012654080519     c                   if        pcddtorin > §ceddtorin
012655080519     c                   eval      plgmsg = 'DATA/ORA FIPCD00F > DATA RICEV.'
012656080519     c                   exsr      sr_wrtlog
012657080519     c                   end
012658080519     c                   end
012659080401     c*
012660080401     c                   clear                   pcdatb
012661080401     c                   eval      pcdfgs = §cedfgs
012662080401     c                   eval      pcdndc = §cedndc
012663080805     c                   eval      pcdcodaut = %dec(%subst(prdprfc:3:7):7:0)
012664080401     c                   eval      pcdaas = §cedaas
012665080401     c                   eval      pcdlnp = §cedlnp
012666080401     c                   eval      pcdnrs = §cednrs
012667080401     c                   eval      pcdnsp = §cednsp
012668080401     c                   eval      PCDFLS  = §cedfls
012669080401     c                   eval      PCDLNA  = §cedlna
012670080401     c                   eval      PCDNRSER= §cednrser
012671080401     c                   eval      PCDNSC  = §cednsc
012672080401     c                   eval      pcdtrd = prdtiporcd
012673080304     c* flag di ricezione dati PDA
012674080401     c                   eval      PCdPDAAS4 = 'P'
012675080401     c                   eval      PCddtorin = §ceddtorin
012676110606     c                   if        saverr = 'F'
012677110607     c                   movel     prddatora     pcdDtorin
012680110606     c                   end
012681080430     c                   eval      §PCDSPUCON=§cedSPUCON
012682080430     c                   eval      §PCDSPUSPC=§cedSPUSPC
012683080430     c                   eval      §PCDCANCON=§cedCANCON
012684080430     c                   eval      §PCDFLGSPC=§cedFLGSPC
012685080430     c                   eval      PCddati = fipcdcedds
012686080227     c*
012687080807     c                   if        %eof(fipcd03l)
012688080401     c                   write     fipcd000
012689080227     c                   else
012690080401     c                   update    fipcd000
012691080227     c                   end
012700080222     c                   endsr
012800080222
012900080222      *----------------------------------------------------------------
013000080222      * ROUTINE INIZIALE
013100080222      *----------------------------------------------------------------
013200080222     c     sr_prima      begsr
013300080222     c* parametri
013400080222     c     *entry        plist
013500080222     c                   parm                    fiprdds
013600080222     c                   parm                    esito
013700080222     c                   clear                   esito
013800080401     c                   eval      fiprdcedds=prddati
013801080805     c                   eval      codaut = %dec(%subst(prdprfc:3:7):7:0)
013802080331     c                   eval      tblkut = 1
013803080331     c     ktab          klist
013804080331     c                   kfld                    tblkut
013805080331     c                   kfld                    tblcod
013806080331     c                   kfld                    tblkey
013807080401     c     kart          klist
013809080709     c                   kfld                    §cedfls
013810080709     c                   kfld                    §cedlna
013811080401     c                   kfld                    §cedNRS
013813080401     c                   kfld                    §cedNSc
013814080401     c     kpcd02        klist
013815080805     c                   kfld                    §cedfgs
013816080805     c                   kfld                    §cedndc
013817080805     c                   kfld                    codaut
013818080805     c                   kfld                    prdtiporcd
013819080401     c                   kfld                    §cedAAS
013820080401     c                   kfld                    §cedLNP
013821080401     c                   kfld                    §cedNRS
013822080401     c                   kfld                    §cedNSP
013823080401     c                   kfld                    §cedNSc
013900080222
014000080222     c                   endsr
014402080401      *--------------------------------------------------------------
014403080401      * fine
014404080401      *--------------------------------------------------------------
014405080401     c     sr_fine       begsr
014500080222     c                   seton                                        rt
014600080222     c                   return
014700080222     c                   endsr
