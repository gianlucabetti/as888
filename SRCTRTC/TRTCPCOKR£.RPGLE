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
001301110606     d prmdfv          s              8
001400080222     d prmdoc          s             16
001500080222     d prmdatora       s             14
001600080805     d codaut          s                   like(pctcodaut)
001700080222     d* output cod. errore
001800110606     d saverr          s              1
001801110606     d prmerr          s              1
001900080222     d prmmsg          s             30
002000080222
002100080225     d COM02           S                   LIKE(§COKIDROW)
002200080222     d fiprdds       e ds                  extname(fiprd00r)
002300080225     d fiprdcokds    e ds
002400080227     d fipctds       e ds                  extname(fipct00f)
002500080227     d fipctcokds    e ds
002600080222     c                   exsr      sr_prima
002700080222
002800080225      * controlla i dati ricevuti da PDA se errore scrivo il log e vado a
002900080225      * fine
003000080222     c                   exsr      sr_ctrdati
003100080227      * scrivo file esiti per chiusura distinta
003200080227     c                   exsr      sr_write
003300080416      * se arrivo qui vuol dire nessun errore quindi scrivo file di log
003400080416     c                   exsr      sr_wrtlog
003500080227     c* fine
003600080227     c                   exsr      sr_fine
003700080227     c*
003800080222      *---------------------------------------------------------------------
003900080222      * ROUTINE CONTROLLO DATI FIPRD
004000080222      *---------------------------------------------------------------------
004100080222     c     sr_ctrdati    begsr
004200080222     c                   clear                   fiplg000
004300080222     c                   clear                   prmerr
004400080222     c                   clear                   prmmsg
004500080430      * tipo applicazione può essere C=consegne R=ritiri
004600080430     c                   if        §coktpiddo <> 'C'
004700080430     c                   eval      plgmsg = 'TIPO DOCUMENTO <> "C"'
004800080430     c                   exsr      sr_wrtlog
004900080430     c                   endif
005000080430     c                   eval      PLGTIPAPPL = §COKTPIDDO
005100080505      * identificativo riga
005200080505     c                   monitor
005300080513     c                   move      §COKIDROW     COM02
005400080505     c                   on-error
005500080505     c                   eval      plgmsg = 'IDROW FORMALMENTE ERRATO'
005600080505     c                   exsr      sr_wrtlog
005700080505     c                   endmon
005800080505      * controllo spedizione e se in distinta
005900080505     c                   exsr      sr_ctrdoc
006000080506     c* verifico se record annullato (prmerr = 1 errori formali)
006100080506     c                   if        prmerr <> '1'
006200080506     c     kpdc2         chain     fipdc02t
006300080506     c                   if        %found(fipdc02t) and pdcatb <> ' '
006400080506     c                   monitor
006500080513     c                   move      §cokfgs       plgfgs
006600080513     c                   move      §cokndc       plgndc
006700080506     c                   on-error
006800080506     c                   endmon
006900080506     c* attenzione non cambiare il msg perchè è testata nella sub. wrtlog
007000080506     c                   eval      plgmsg = 'CONFERMA ANNULLAMENTO'
007100080804     c*                  exsr      sr_wrtlog
007200080506     c                   end
007300080506     c                   end
007400080506     c*
007500080804     c                   if        prmerr <> ' ' and pdcatb = ' '
007600080505     c                   eval      plgmsg = prmmsg
007700080505     c                   exsr      sr_wrtlog
007800080505     c                   end
007900080226      * controllo se esiste la distinta e se dell'aut.
008000080222     c                   exsr      sr_ctrdst
008100080804     c                   if        prmerr = '1' or
008200080804     c                             (prmerr <> ' ' and pdcatb = ' ')
008300080222     c                   eval      plgmsg = prmmsg
008400080222     c                   exsr      sr_wrtlog
008500080222     c                   end
008600080225     c                   move      §cokfgs       plgfgs
008700080225     c                   move      §cokndc       plgndc
008800080222      * data ora pda conferma
008900080222     c                   exsr      sr_ctrdop
009000110601     c                   if        prmerr =  'E'
009100080222     c                   eval      plgmsg = prmmsg
009200080331     c                   move      'PDA'         plgmsg
009300080222     c                   exsr      sr_wrtlog
009400080222     c                   end
009401110601     c* se errore F = forzabile vuol dire che ho ricvevuto una data anteced.
009402110601     c* 30 gg per cui forzo data distinta e ora ricezione
009405110601     c                   eval      saverr = prmerr
009500080222      * flag richiesta assistenza
009600101122     c*                  if        §cokflgass<>'S' and §cokflgass<>*blanks
009700101122     c*                  eval      plgmsg = 'FLAG RICH. ASS. NON VALIDO'
009800101122     c*                  exsr      sr_wrtlog
009900101122     c*                  endif
010000080407     c* verifico se sono allineata con gli archivi di trasmissione
010100080407     c* spedizione o idrow
010200080904     c     kspe          setgt     fipdc03t
010300080904     c     kspe          readpe    fipdc03t
010400080904     c                   if        %eof(fipdc03t)
010500080902     c                   eval      plgmsg = 'SPEDIZ.NON TROVATA IN FIPDC00T'
010600080407     c                   exsr      sr_wrtlog
010700080407     c                   end
010800080222     c                   endsr
010900080222      *---------------------------------------------------------------------
011000080222      * controlla distinta
011100080222      *---------------------------------------------------------------------
011200080222     c     sr_ctrdst     begsr
011300080222     c                   eval      prmfgs = %subst(prddati:1:3)
011400080222     c                   eval      prmnfv = %subst(prddati:4:6)
011500080225     c                   eval      prmaut = %subst(prdprfc:3:7)
011501110606     c                   eval      prmdfv = *all'0'
011600080222     c                   call      'TRTCPDSTR'
011700080222     c                   parm                    prmfgs
011800080222     c                   parm                    prmnfv
011900080225     c                   parm                    prmaut
011901110606     c                   parm                    prmdfv
012000080222     c                   parm                    prmerr
012100080222     c                   parm                    prmmsg
012200080222     c                   endsr
012300080222      *---------------------------------------------------------------------
012400080222      * controlla id documento
012500080222      *---------------------------------------------------------------------
012600080222     c     sr_ctrdoc     begsr
012700080222     c                   eval      prmdoc = %subst(prddati:10:16)
012800080225     c                   eval      prmfgs = %subst(prddati:1:3)
012900080225     c                   eval      prmnfv = %subst(prddati:4:6)
013000080225     c                   call      'TRTCPDOCR'
013100080222     c                   parm                    prmdoc
013200080225     c                   parm                    prmfgs
013300080225     c                   parm                    prmnfv
013400080222     c                   parm                    prmerr
013500080222     c                   parm                    prmmsg
013600080222     c                   endsr
013700080222      *---------------------------------------------------------------------
013800080222      * controlla data ora pda conferma
013900080222      *---------------------------------------------------------------------
014000080222     c     sr_ctrdop     begsr
014100080222     c                   eval      prmdatora = %subst(prddati:26:14)
014200080225     c                   call      'TRTCPDOPR'
014300080222     c                   parm                    prmdatora
014400080222     c                   parm                    prmerr
014500080222     c                   parm                    prmmsg
014600080222     c                   endsr
014700080222      *---------------------------------------------------------------------
014800080222      * scrive log
014900080222      *---------------------------------------------------------------------
015000080222     c     sr_wrtlog     begsr
015100080225     c* se sto scrivendo un errore imposto anche il flag
015200080506     c                   if        plgmsg <> ' ' and
015300080506     c                             plgmsg <>'CONFERMA ANNULLAMENTO'
015400080222     c                   eval      plgerr = 'E'
015500080225     c                   else
015600080225     c                   clear                   plgerr
015700080225     c                   end
015701110607     c* segnalo se forzata data ricezione errata perchè antecedente di 30 gg
015702110607     c                   if        saverr = 'F'
015703110607     c                   if        plgmsg = ' '
015704110607     c                   move      'DATA < 30 GG'plgmsg
015705110607     c                   end
015706110607     c                   clear                   saverr
015707110607     c                   end
015800110607     c                   eval      plgcodaut = %dec(%subst(prdprfc:3:7):7:0)
015900080222     c                   eval      plgtiporcd = prdtiporcd
016000080222     c                   eval      plgprfc = prdprfc
016100080222     c                   eval      plgiddisp = prdiddisp
016200080222     c                   eval      plgdatora = prddatora
016300080222     c                   eval      plgdati = prddati
016400080225     c                   eval      plgiddoc = %subst(prddati:10:16)
016500080222     c                   write     fiplg000
016600080225     c* se sto scrivendo un errore vado a fine
016700080225     c                   if        plgmsg <> ' '
016800080222     c                   exsr      sr_fine
016900080225     c                   end
017000080227     c                   endsr
017100080227
017200080227      *----------------------------------------------------------------
017300080227      * scrittura file esiti per chiusura distinta
017400080227      *----------------------------------------------------------------
017500080227     c     sr_write      begsr
017600080227     c                   clear                   fipctcokds
017700080227     c                   clear                   fipctds
017800080805     c     kpct          setgt     fipct02l
017900080805     c     kpct          readpe(e) fipct02l
018000080227     c*
018100080227     c                   if        %error
018200080227     c                   eval      esito = 'E'
018300080227     c                   exsr      sr_fine
018400080227     c                   end
018500080227     c*
018600080805     c                   if        not %eof(fipct02l)
018700080227     c                   eval      fipctcokds = pctdati
018800080227     c* controllo sequenza data ora pda, nel caso non aggiorno scrivo log
018900080304     c* se quella trasmessa è > di quella che sto ricevendo
019000080227     c                   if        pctdtorin > §cokdatora
019100080407     c                   eval      plgmsg = 'DATA/ORA FIPCT00F > DATA RICEV.'
019200080227     c                   exsr      sr_wrtlog
019300080227     c                   end
019400080227     c* controllo sequenza riga
019500080304     c* se quello trasmesso è > di quello che sto ricevendo
019600080227     c                   if        §PCTIDROW > §cokIDROW
019700080407     c                   eval      plgmsg = 'IDROW FIPCT00F > IDROW RICEVUTO'
019800080227     c                   exsr      sr_wrtlog
019900080227     c                   end
020000080604     c                   end
020100080604     c*
020200080604     c                   clear                   pctatb
020300080604     c                   eval      pctfgs = §cokfgs
020400080604     c                   eval      pctndc = §cokndc
020500080227     c                   eval      pctaas = §cokaas
020600080227     c                   eval      pctlnp = §coklnp
020700080227     c                   eval      pctnrs = §coknrs
020800080227     c                   eval      pctnsp = §coknsp
020900080304     c                   eval      pcttrd = prdtiporcd
021000080304     c* flag di ricezione dati PDA
021100080304     c                   eval      PCTPDAAS4 = 'P'
021200080227     c                   eval      PCTdtorin = §cokdatora
021201110606     c                   if        saverr = 'F'
021202110607     c                   movel     prddatora     pctdtorin
021205110606     c                   end
021300080227     c                   clear                   §PCTFLGVIS
021400080227     c                   eval      §PCTFLGASS = §cokFLGASS
021500080227     c                   eval      §PCTIDROW  = §cokIDROW
021600101103     c                   eval      §PCTnotass = §coknotass
021700080227     c                   eval      PCTdati = fipctcokds
021800080805     c                   eval      pctcodaut = %dec(%subst(prdprfc:3:7):7:0)
021900080227     c*
022000080805     c                   if        %eof(fipct02l)
022100080227     c                   write     fipct000
022200080227     c                   else
022300080227     c                   update    fipct000
022400080227     c                   end
022500080222     c                   endsr
022600080222
022700080222      *----------------------------------------------------------------
022800080222      * ROUTINE INIZIALE
022900080222      *----------------------------------------------------------------
023000080222     c     sr_prima      begsr
023100080222     c* parametri
023200080222     c     *entry        plist
023300080222     c                   parm                    fiprdds
023400080222     c                   parm                    esito
023500080222     c                   clear                   esito
023600080225     c                   eval      fiprdcokds=prddati
023700080805     c                   eval      codaut = %dec(%subst(prdprfc:3:7):7:0)
023800080226     c     kspe          klist
023900080227     c                   kfld                    §COKfgs
024000080227     c                   kfld                    §COKndc
024100080903     c                   kfld                    codaut
024200080227     c                   kfld                    §COKAAS
024300080227     c                   kfld                    §COKLNP
024400080226     c                   kfld                    §COKNRS
024500080226     c                   kfld                    §COKNSP
024600080227     c     kpct          klist
024700080805     c                   kfld                    §COKfgs
024800080805     c                   kfld                    §COKndc
024900080805     c                   kfld                    codaut
025000080805     c                   kfld                    prdtiporcd
025100080227     c                   kfld                    §COKAAS
025200080227     c                   kfld                    §COKLNP
025300080227     c                   kfld                    §COKNRS
025400080227     c                   kfld                    §COKNSP
025500080505     c     kpdc2         klist
025600080505     c                   kfld                    §COKAAS
025700080505     c                   kfld                    §COKLNP
025800080505     c                   kfld                    §COKNRS
025900080505     c                   kfld                    §COKNSP
026000080505     c                   kfld                    §COKIDROW
026100080222
026200080222     c                   endsr
026300080222      *--------------------------------------------------------------
026400080222      * fine
026500080222      *--------------------------------------------------------------
026600080222     c     sr_fine       begsr
026700080222     c                   seton                                        rt
026800080222     c                   return
026900080222     c                   endsr
