000100080222     h decedit('0,') option(*nodebugio)
000200080222      *---------------------------------------------------------------*
000300080227      * Da FIPRD00R scrive file FIPCT00F per chiusura distinta e log
000400080227      * non deve fare commit o rolback perchè lo fa il chianmate (TRTCPC0R)
000500080222      *---------------------------------------------------------------*
000600080904     ffipdc03t  if   e           k disk
000700080505     ffipdc02t  if   e           k disk    rename(fipdc000:fipdc002)
000800080226     ffiplg00f  o    e             disk    commit
000900080805     ffipct02l  uf a e           k disk    commit
001000080222     d esito           s              1
001100080222     d prmfgs          s              3
001200080222     d prmnfv          s              6
001300080225     d prmaut          s              7
001400110606     d prmdfv          s              8
001500080222     d prmdoc          s             16
001600080222     d prmdatora       s             14
001700080805     d codaut          s                   like(pctcodaut)
001800080222     d* output cod. errore
001900110606     d saverr          s              1
002000110606     d prmerr          s              1
002100080222     d prmmsg          s             30
002200121015
002300121015     d wDataOra        s             12
002400080222
002500080225     d COM02           S                   LIKE(§COKIDROW)
002600080222     d fiprdds       e ds                  extname(fiprd00r)
002700080225     d fiprdcokds    e ds
002800080227     d fipctds       e ds                  extname(fipct00f)
002900080227     d fipctcokds    e ds
003000080222     c                   exsr      sr_prima
003100080222
003200080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
003300080225      * fine
003400080222     c                   exsr      sr_ctrdati
003500080227      * scrivo file esiti per chiusura distinta
003600080227     c                   exsr      sr_write
003700080416      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
003800080416     c                   exsr      sr_wrtlog
003900080227     c* fine
004000080227     c                   exsr      sr_fine
004100080227     c*
004200080222      *---------------------------------------------------------------------
004300080222      * ROUTINE CONTROLLO DATI FIPRD
004400080222      *---------------------------------------------------------------------
004500080222     c     sr_ctrdati    begsr
004600080222     c                   clear                   fiplg000
004700080222     c                   clear                   prmerr
004800080222     c                   clear                   prmmsg
004900080430      * tipo applicazione può essere C=consegne R=ritiri
005000080430     c                   if        §coktpiddo <> 'C'
005100080430     c                   eval      plgmsg = 'TIPO DOCUMENTO <> "C"'
005200080430     c                   exsr      sr_wrtlog
005300080430     c                   endif
005400080430     c                   eval      PLGTIPAPPL = §COKTPIDDO
005500080505      * identificativo riga
005600080505     c                   monitor
005700080513     c                   move      §COKIDROW     COM02
005800080505     c                   on-error
005900080505     c                   eval      plgmsg = 'IDROW FORMALMENTE ERRATO'
006000080505     c                   exsr      sr_wrtlog
006100080505     c                   endmon
006200080505      * controllo spedizione e se in distinta
006300080505     c                   exsr      sr_ctrdoc
006400080506     c* verifico se record annullato (prmerr = 1 errori formali)
006500080506     c                   if        prmerr <> '1'
006600080506     c     kpdc2         chain     fipdc02t
006700080506     c                   if        %found(fipdc02t) and pdcatb <> ' '
006800080506     c                   monitor
006900080513     c                   move      §cokfgs       plgfgs
007000080513     c                   move      §cokndc       plgndc
007100080506     c                   on-error
007200080506     c                   endmon
007300080506     c* attenzione non cambiare il msg perchè è testata nella sub. wrtlog
007400080506     c                   eval      plgmsg = 'CONFERMA ANNULLAMENTO'
007500080804     c*                  exsr      sr_wrtlog
007600080506     c                   end
007700080506     c                   end
007800080506     c*
007900080804     c                   if        prmerr <> ' ' and pdcatb = ' '
008000080505     c                   eval      plgmsg = prmmsg
008100080505     c                   exsr      sr_wrtlog
008200080505     c                   end
008300080226      * controllo se esiste la distinta e se dell'aut.
008400080222     c                   exsr      sr_ctrdst
008500080804     c                   if        prmerr = '1' or
008600080804     c                             (prmerr <> ' ' and pdcatb = ' ')
008700080222     c                   eval      plgmsg = prmmsg
008800080222     c                   exsr      sr_wrtlog
008900080222     c                   end
009000080225     c                   move      §cokfgs       plgfgs
009100080225     c                   move      §cokndc       plgndc
009200080222      * data ora pda conferma
009300080222     c                   exsr      sr_ctrdop
009400110601     c                   if        prmerr =  'E'
009500080222     c                   eval      plgmsg = prmmsg
009600080331     c                   move      'PDA'         plgmsg
009700080222     c                   exsr      sr_wrtlog
009800080222     c                   end
009900110601     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
010000121015     c* 10 gg o superiore per cui forzo data distinta e ora ricezione
010100110601     c                   eval      saverr = prmerr
010200080222      * flag richiesta assistenza
010300101122     c*                  if        §cokflgass<>'S' and §cokflgass<>*blanks
010400101122     c*                  eval      plgmsg = 'FLAG RICH. ASS. NON VALIDO'
010500101122     c*                  exsr      sr_wrtlog
010600101122     c*                  endif
010700080407     c* verifico se sono allineata con gli archivi di trasmissione
010800080407     c* spedizione o idrow
010900080904     c     kspe          setgt     fipdc03t
011000080904     c     kspe          readpe    fipdc03t
011100080904     c                   if        %eof(fipdc03t)
011200080902     c                   eval      plgmsg = 'SPEDIZ.NON TROVATA IN FIPDC00T'
011300080407     c                   exsr      sr_wrtlog
011400080407     c                   end
011500080222     c                   endsr
011600080222      *---------------------------------------------------------------------
011700080222      * controlla distinta
011800080222      *---------------------------------------------------------------------
011900080222     c     sr_ctrdst     begsr
012000080222     c                   eval      prmfgs = %subst(prddati:1:3)
012100080222     c                   eval      prmnfv = %subst(prddati:4:6)
012200080225     c                   eval      prmaut = %subst(prdprfc:3:7)
012300110606     c                   eval      prmdfv = *all'0'
012400080222     c                   call      'TRTCPDSTR'
012500080222     c                   parm                    prmfgs
012600080222     c                   parm                    prmnfv
012700080225     c                   parm                    prmaut
012800110606     c                   parm                    prmdfv
012900080222     c                   parm                    prmerr
013000080222     c                   parm                    prmmsg
013100080222     c                   endsr
013200080222      *---------------------------------------------------------------------
013300080222      * controlla id documento
013400080222      *---------------------------------------------------------------------
013500080222     c     sr_ctrdoc     begsr
013600080222     c                   eval      prmdoc = %subst(prddati:10:16)
013700080225     c                   eval      prmfgs = %subst(prddati:1:3)
013800080225     c                   eval      prmnfv = %subst(prddati:4:6)
013900080225     c                   call      'TRTCPDOCR'
014000080222     c                   parm                    prmdoc
014100080225     c                   parm                    prmfgs
014200080225     c                   parm                    prmnfv
014300080222     c                   parm                    prmerr
014400080222     c                   parm                    prmmsg
014500080222     c                   endsr
014600080222      *---------------------------------------------------------------------
014700080222      * controlla data ora pda conferma
014800080222      *---------------------------------------------------------------------
014900080222     c     sr_ctrdop     begsr
015000080222     c                   eval      prmdatora = %subst(prddati:26:14)
015100080225     c                   call      'TRTCPDOPR'
015200080222     c                   parm                    prmdatora
015300080222     c                   parm                    prmerr
015400080222     c                   parm                    prmmsg
015500080222     c                   endsr
015600080222      *---------------------------------------------------------------------
015700080222      * scrive log
015800080222      *---------------------------------------------------------------------
015900080222     c     sr_wrtlog     begsr
016000080225     c* se sto scrivendo un errore imposto anche il flag
016100080506     c                   if        plgmsg <> ' ' and
016200080506     c                             plgmsg <>'CONFERMA ANNULLAMENTO'
016300080222     c                   eval      plgerr = 'E'
016400080225     c                   else
016500080225     c                   clear                   plgerr
016600080225     c                   end
016700121015     c* segnalo se forzata data ricezione errata perchè antecedente di 10 gg
016800110607     c                   if        saverr = 'F'
016900110607     c                   if        plgmsg = ' '
017000121015     c                   eval      plgmsg = 'DATA < o > di 10 GG'
017100110607     c                   end
017200110607     c                   clear                   saverr
017300110607     c                   end
017400110607     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
017500080222     c                   eval      plgtiporcd = prdtiporcd
017600080222     c                   eval      plgprfc = prdprfc
017700080222     c                   eval      plgiddisp = prdiddisp
017800080222     c                   eval      plgdatora = prddatora
017900080222     c                   eval      plgdati = prddati
018000080225     c                   eval      plgiddoc = %subst(prddati:10:16)
018100080222     c                   write     fiplg000
018200080225     c* se sto scrivendo un errore vado a fine
018300080225     c                   if        plgmsg <> ' '
018400080222     c                   exsr      sr_fine
018500080225     c                   end
018600080227     c                   endsr
018700080227
018800080227      *----------------------------------------------------------------
018900080227      * scrittura file esiti per chiusura distinta
019000080227      *----------------------------------------------------------------
019100080227     c     sr_write      begsr
019200080227     c                   clear                   fipctcokds
019300080227     c                   clear                   fipctds
019400080805     c     kpct          setgt     fipct02l
019500080805     c     kpct          readpe(e) fipct02l
019600080227     c*
019700080227     c                   if        %error
019800080227     c                   eval      esito = 'E'
019900080227     c                   exsr      sr_fine
020000080227     c                   end
020100080227     c*
020200080805     c                   if        not %eof(fipct02l)
020300080227     c                   eval      fipctcokds = pctdati
020400080227     c* controllo sequenza data ora pda, nel caso non aggiorno scrivo log
020500080304     c* se quella trasmessa è > di quella che sto ricevendo
020600080227     c                   if        pctdtorin > §cokdatora
020700080407     c                   eval      plgmsg = 'DATA/ORA FIPCT00F > DATA RICEV.'
020800080227     c                   exsr      sr_wrtlog
020900080227     c                   end
021000080227     c* controllo sequenza riga
021100080304     c* se quello trasmesso è > di quello che sto ricevendo
021200080227     c                   if        §PCTIDROW > §cokIDROW
021300080407     c                   eval      plgmsg = 'IDROW FIPCT00F > IDROW RICEVUTO'
021400080227     c                   exsr      sr_wrtlog
021500080227     c                   end
021600080604     c                   end
021700080604     c*
021800080604     c                   clear                   pctatb
021900080604     c                   eval      pctfgs = §cokfgs
022000080604     c                   eval      pctndc = §cokndc
022100080227     c                   eval      pctaas = §cokaas
022200080227     c                   eval      pctlnp = §coklnp
022300080227     c                   eval      pctnrs = §coknrs
022400080227     c                   eval      pctnsp = §coknsp
022500080304     c                   eval      pcttrd = prdtiporcd
022600080304     c* flag di ricezione dati PDA
022700080304     c                   eval      PCTPDAAS4 = 'P'
022800080227     c                   eval      PCTdtorin = §cokdatora
022900110606     c                   if        saverr = 'F'
023000121015     c                   eval      wDataOra = %subst(PRDdatora:1:12)
023100121015     c                   movel     wDataOra      PCTdtorin
023200110606     c                   end
023300080227     c                   clear                   §PCTFLGVIS
023400080227     c                   eval      §PCTFLGASS = §cokFLGASS
023500080227     c                   eval      §PCTIDROW  = §cokIDROW
023600101103     c                   eval      §PCTnotass = §coknotass
023700080227     c                   eval      PCTdati = fipctcokds
023800080805     c                   eval      pctcodaut = %dec(%subst(prdprfc:3:7):7:0)
023900080227     c*
024000080805     c                   if        %eof(fipct02l)
024100080227     c                   write     fipct000
024200080227     c                   else
024300080227     c                   update    fipct000
024400080227     c                   end
024500080222     c                   endsr
024600080222
024700080222      *----------------------------------------------------------------
024800080222      * ROUTINE INIZIALE
024900080222      *----------------------------------------------------------------
025000080222     c     sr_prima      begsr
025100080222     c* parametri
025200080222     c     *entry        plist
025300080222     c                   parm                    fiprdds
025400080222     c                   parm                    esito
025500080222     c                   clear                   esito
025600080225     c                   eval      fiprdcokds=prddati
025700080805     c                   eval      codaut = %dec(%subst(prdprfc:3:7):7:0)
025800080226     c     kspe          klist
025900080227     c                   kfld                    §COKfgs
026000080227     c                   kfld                    §COKndc
026100080903     c                   kfld                    codaut
026200080227     c                   kfld                    §COKAAS
026300080227     c                   kfld                    §COKLNP
026400080226     c                   kfld                    §COKNRS
026500080226     c                   kfld                    §COKNSP
026600080227     c     kpct          klist
026700080805     c                   kfld                    §COKfgs
026800080805     c                   kfld                    §COKndc
026900080805     c                   kfld                    codaut
027000080805     c                   kfld                    prdtiporcd
027100080227     c                   kfld                    §COKAAS
027200080227     c                   kfld                    §COKLNP
027300080227     c                   kfld                    §COKNRS
027400080227     c                   kfld                    §COKNSP
027500080505     c     kpdc2         klist
027600080505     c                   kfld                    §COKAAS
027700080505     c                   kfld                    §COKLNP
027800080505     c                   kfld                    §COKNRS
027900080505     c                   kfld                    §COKNSP
028000080505     c                   kfld                    §COKIDROW
028100080222
028200080222     c                   endsr
028300080222      *--------------------------------------------------------------
028400080222      * fine
028500080222      *--------------------------------------------------------------
028600080222     c     sr_fine       begsr
028700080222     c                   seton                                        rt
028800080222     c                   return
028900080222     c                   endsr
