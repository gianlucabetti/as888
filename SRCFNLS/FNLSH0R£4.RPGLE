000100990504     H* ACTGRP=*CALLER
000200140915     H DFTACTGRP(*NO) ACTGRP(*caller)
000300990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000400100330      *clienti KSC a cui inviare da bollettazione
000500120511     f*fnblp65j  if   e           k disk
000600100330      *clienti CCM a cui inviare da bollettazione
000700120511     f*fnblp64j  if   e           k disk
000800120511     f*                                    rename(fnblp000:fnblpccm)
000900120511      *clienti a cui inviare da bollettazione
001000120511     ffnblp63j  if   e           k disk
001100040916      *clienti a cui inviare da orm commissionato
001200120511     f*fnblp66j  if   e           k disk
001300120511     f*                                    rename(fnblp000:fnblp)
001400120524     ffiar403l  if   e           k disk    rename(fiar4000:fiar403)
001500120524     f                                     prefix(t_)
001600120524     ffnblp01l  if   e           k disk    rename(fnblp000:fnblp01)
001700090311     FTIVGD00F  UF A E             DISK    USROPN
001800051114     ffIAR901l  if   e           k disk
001900060214     ffiar401l  uf   e           k disk    commit
002000021223     fFiar501l  if   e           k Disk
002100011005     fazorg01l  if   e           k disk
002200000517     ftabel00f  if   e           k disk
002300040825     ffnarb01l  if   e           k disk
002400000517      *--------------------------------------------------------------------
002500000522     D kpjba         e ds
002600000517     D ds3q          e ds
002700000518     D dsbl4w        e ds
002800010927     D dsbl4z        e ds
002900021223     d dar5ban       e ds
003000090311     D FNVABDS       E DS                  EXTNAME(FNVAB00T)
003100090316     d  VabTba                        2s 0 overlay(vabant:1)
003200090316     d  VabNba                        2s 0 overlay(vabant:3)
003300090316     d  VabTb2                        2s 0 overlay(vabant:5)
003400090316     d  VabNb2                        2s 0 overlay(vabant:7)
003500090311     D trul47ds      e ds
003600140915     d fnlvemds      e ds
003700000517     d  codut          s              1  0 inz(1)
003800000517     d  cod            s                   like(tblcod)
003900000517     d  key            s                   like(tblkey)
004000010927     d  kscj           s              7
004100100330     d  codice         s                   like(blpksc)
004200060214     d  ktrc           s                   like(ar4trc)
004300021223     d  KAr5Trd        s                   Like(Ar5Trd) Inz('BAN')
004400000518     d  woggi          s              8  0
004500000518     d  woggig         s              8  0
004600000518     d  dataora        s             14  0
004700000518     d  dataeur        s               d   datfmt(*eur)
004800000518     d  dataiso        s               d   datfmt(*iso)
004900000519     d  como7          s              7
005000120511     d  vuoto          s                   like(codice)
005100120524     d  kn30           s                   like(t_ar4n30)
005200120524     d  kn15           s             15
005300140915
005400140915     d* PROTOTIPI
005500140915     d fnblpds       e ds                  extname('FNBLP00F')
005600140915     d                                     prefix(p_)
005700140915     d fnlvemr         PR
005800140915     d                                     extpgm('FNLVEMR')
005900140915     d  kpjba                              likeds(kpjba)
006000140915     d  fnblpds                            likeds(fnblpds)
006100021223
006200000517     d
006300000517      *--------------------------------------------------------------------
006400090311     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrent
006500090311     C                   open      tivgd00f
006600090311     C                   clear                   trul47ds
006700090311     C                   eval      d47opz  = 'I'
006800090311     C                   eval      d47tip  = 'BT'
006900090311     C                   eval      d47lck  = 'N'
007000090311     C                   eval      d47chkj = 'N'
007100090311     C                   eval      d47pgm  = 'FNLSH0R'
007200090311     C                   call      'TRUL47R'
007300090311     C                   parm                    trul47ds
007400090311     C*
007500090311     C* Se elaborazione consentita => proseguo
007600090311     C                   if        d47sts <> 'A'
007700000517     c
007800090316     c     ktab1         setll     tabel
007900000517     c                   do        *hival
008000090316     c                   read      tabel                                  99
008100000517     C   99              leave
008200000529     c                   if        tblcod <> '3Q'
008300000529     c                   leave
008400000529     c                   end
008500000522     c                   if        tblflg <> *blank
008600000522     c                   iter
008700000522     c                   end
008800000517     c                   movel     tbluni        ds3q
008900100330     c                   movel     tblkey        kscj
009000100330     c                   movel     tblkey        codice
009100120524     c                   move      'Z'           ktrc
009200120524     c                   eval      kn30=kscj+'00000000'
009300120524     c                   eval      kn15=kscj+'00000000'
009400000517     c
009500000914     c
009600120511     c*    codice        chain     fnblp65j                           98
009700120511     c*  98codice        chain     fnblp64j                           98
009800120524     c*  98kscj          chain     fnblp66j                           98
009900120524     c     codice        chain     fnblp63j                           98
010000120524     c   98k63j          chain     fnblp63j                           98
010100120524     c   98kar43         setll     fiar403l
010200120524     c   98'Z'           reade     fiar403l
010300000914     c
010400120524     c                   if        %found(fnblp63j)
010500120524     c                             or (not %eof(fiar403l) and
010600120524     c                                 %subst(t_AR4N30:1:15)=kn15)
010700000518     c                   exsr      estrai
010800000518     c                   endif
010900000517     c                   enddo
011000090311     c                   endif
011100090311     c                   close     tivgd00f
011200090311     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
011300090311     C                   clear                   trul47ds
011400090311     C                   eval      d47opz  = 'F'
011500090311     C                   eval      d47tip  = 'BT'
011600090311     C                   call      'TRUL47R'
011700090311     C                   parm                    trul47ds
011800090311
011900990504     c                   seton                                        lr
012000000518?    C*-------------------------------------------------------------
012100000518?    C* estrai - estrae x file in qtemp x successiva scrittura membri
012200000518?    C*-------------------------------------------------------------
012300000518     c     estrai        begsr
012400010927      *loop di lettura per codice cliente mittente in KSC
012500000518      *
012600000518     c                   setoff                                       98
012700120511     c*    codice        setll     fnblp65j
012800120511     c     k63j          setll     fnblp63j
012900000518     c                   do        *hival
013000120511     c*    codice        reade     fnblp65j                               98
013100120511     c     k63j          reade     fnblp63j                               98
013200000518     c   98              leave
013300090316     c                   clear                   fnvabds
013400040916     c                   z-add     blpdse        blpdsew           8 0
013500040920      * prima di verificare il record da elaborare verifico che non sia
013600040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
013700040920      * se è così viene flaggato il BL4 per non trattare il record
013800040920      *
013900040920     c                   setoff                                       56
014000040920     c                   if        blpdbr > 0
014100040920     c                   exsr      aggbl4gg15
014200040920      * *in56 = on bypass il resto del loop
014300040920     c   56              iter
014400040920     c                   endif
014500000522      *aggiorno e spedisco solo le righe con data minore uguale a oggi
014600000623     c                   if        blpdse > woggig or blpft1 = *blank
014700000705     c                             or blpdse = 0
014800040825     c*
014900040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
015000040825     c*partenza a *zeros
015100040825     c                   exsr      chkdse
015200040825     c*
015300140915     c  N55              exsr      chk3Q
015400140915     c*
015500140915     c  N55              iter
015600000522     c                   end
015700000519     c                   exsr      datiagg
015800000518     c                   exsr      scrivi
015900000519     c                   exsr      aggbl4
016000000518     c                   enddo
016100100330      *loop di lettura con codice cliente in CCM
016200000518     c                   setoff                                       98
016300120511     c*    codice        setll     fnblp64j
016400120511     c     codice        setll     fnblp63j
016500000518     c                   do        *hival
016600120511     c*    codice        reade     fnblp64j                               98
016700120511     c     codice        reade     fnblp63j                               98
016800000518     c   98              leave
016900090316     c                   clear                   fnvabds
017000040916     c                   z-add     blpdse        blpdsew
017100040920      * prima di verificare il record da elaborare verifico che non sia
017200040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
017300040920      * se è così viene flaggato il BL4 per non trattare il record
017400040920      *
017500040920     c                   setoff                                       56
017600040920     c                   if        blpdbr > 0
017700040920     c                   exsr      aggbl4gg15
017800040920      * *in56 = on bypass il resto del loop
017900040920     c   56              iter
018000040920     c                   endif
018100000623     c                   if        blpdse > woggig or blpft1 = *blank
018200000705     c                             or blpdse = 0
018300040825     c*
018400040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
018500040825     c*partenza a *zeros
018600040825     c                   exsr      chkdse
018700140915     c*
018800140915     c  N55              exsr      chk3Q
018900040825     c*
019000040825     c  N55              iter
019100000522     c                   end
019200000518     c                   exsr      datiagg
019300000518     c                   exsr      scrivi
019400000518     c                   exsr      aggbl4
019500000518     c                   enddo
019600060214      *loop di lettura con codice cliente in AR4NOT (altra V.L.)
019700000518     c
019800120524     c                   move      'Z'           ktrc
019900010927     c                   setoff                                       98
020000120524     c*    kscj          setll     fnblp66j
020100120524     c     kar43         setll     fiar403l
020200011002     c                   seton                                        66
020300010927     c                   do        *hival
020400120524     c*    kscj          reade     fnblp66j                               98
020500120524     c*  98              leave
020600120524     c     'Z'           reade     fiar403l
020700120524     c                   if        %eof(fiar403l) or
020800120524     c                             %subst(t_ar4n30:1:15)>kn15
020900120524     c                   leave
021000120524     c                   endif
021100120524     c     kblp          chain     fnblp01l
021200120524     c                   if        %found(fnblp01l)
021300090316     c                   clear                   fnvabds
021400040916     c                   z-add     blpdse        blpdsew
021500040920      * prima di verificare il record da elaborare verifico che non sia
021600040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
021700040920      * se è così viene flaggato il BL4 per non trattare il record
021800040920      *
021900040920     c                   setoff                                       56
022000040920     c                   if        blpdbr > 0
022100040920     c                   exsr      aggbl4zgg15
022200040920      * *in56 = on bypass il resto del loop
022300040920     c   56              iter
022400040920     c                   endif
022500010927     c                   if        blpdse > woggig or blpft1 = *blank
022600010927     c                             or blpdse = 0
022700040825     c*
022800040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
022900040825     c*partenza a *zeros
023000040825     c                   exsr      chkdse
023100140915     c*
023200140915     c  N55              exsr      chk3Q
023300040825     c*
023400040825     c  N55              iter
023500010927     c                   end
023600010927     c                   exsr      datiagg
023700010927     c                   exsr      scrivi
023800010927     c                   exsr      aggbl4Z
023900120524     c                   endif
024000010927     c                   enddo
024100011002     c                   setoff                                       66
024200090316
024300090316?    C                   commit
024400010927
024500000518     c                   endsr
024600040825?    C*-------------------------------------------------------------
024700040825?    C* chkdse - verifiche aggiuntive in caso d data spunta entrata in part. = 0
024800040825?    C*-------------------------------------------------------------
024900040825     c     chkdse        begsr
025000040825     c*
025100040825     c*se in partenza data spunta entrata è *zeros chaino le bolle in arrivo
025200040825     c*x verificare se collo è stato poi cmq "spuntato" in arrivo
025300040825     c                   setoff                                       55
025400040825     c                   if        blpdse = 0
025500040825     c     karb1_c       chain     fnarb01l
025600040825     c                   if        %found(fnarb01l)
025700040916     c                   if        arbdam > 0 AND  arbcca <> '5' and
025800040825     c*paletto la condizione a partire dal 20040823 xchè è il giorno in cui
025900040825     c*mi sono accorto del problema (l'ha detto Luciano d fare così!)
026000040916     c                             blpdim > 20040823   OR
026100040916     c                             arbdam > 0 AND  arbcca = '5' and
026200040916     c                             arbdam <> arbdcm and blpdim > 20040823
026300040825     c                   seton                                        55
026400040916     c                   z-add     blpdrt        blpdsew
026500040825     c                   endif
026600040825     c                   endif
026700040825     c                   endif
026800040825     c*
026900040825     c                   endsr
027000000518?    C*-------------------------------------------------------------
027100140915?    C* controllo se si può proseguire nell'estrazione perché su
027200140915?    C* tab. 3Q è richiesto un invio anticipato
027300000518?    C*-------------------------------------------------------------
027400140915     c     Chk3Q         begsr
027500140915
027600140915      /free
027700140915       select;
027800140915         // se invio antipato dei dati su bollettazione
027900140915         // ma non per dati su spunta
028000140915         when §3QADB = 'S' and
028100140915              §3QADS = *blank;
028200140915           // il rcd è OK a prescindere
028300140915           *in55 = *on;
028400140915           leavesr;
028500140915         // se invio antipato dei dati su bollettazione per spunta
028600140915         when §3QADS = 'S';
028700140915           // controllo se spedizione è spuntata
028800140915           clear fnlvemds;
028900140915           clear fnblpds;
029000140915           iemAAS = blpAAS;
029100140915           iemLNP = blpLNP;
029200140915           iemNRS = blpNRS;
029300140915           iemNSP = blpNSP;
029400140915           kpjbu = fnlvemds;
029500140915           callP(e) FNLVEMR (kpjba : fnblpds);
029600140915           fnlvemds = kpjbu;
029700140915           // se spuntata, il rcd è OK
029800140915           if %error OR oemESITO <> '1';
029900140915           else;
030000140915             *in55 = *on;
030100140915             leavesr;
030200140915           endif;
030300140915       endsl;
030400140915
030500140915      /end-free
030600140915
030700140915     c*
030800140915     c                   endsr
030900140915?    C*-------------------------------------------------------------
031000140915?    C* datiagg- dati aggiuntivi da ar4 e AR9
031100140915?    C*-------------------------------------------------------------
031200140915     c     datiagg       begsr
031300140915      *
031400000518      *seconda ragione sociale cliente
031500000518     c                   move      'D'           ktrc
031600060519     c     kar4          chain(N)  fiar401l                           97
031700060214     c  n97              movel     ar4not        vabrd2
031800000605      *campo note bolla
031900000605     c                   move      '8'           ktrc
032000060519     c     kar4          chain(N)  fiar401l                           97
032100060214     c  n97              movel     ar4not        vabnot
032200000518      *campo note bolla 2
032300000518     c                   move      '9'           ktrc
032400060519     c     kar4          chain(N)  fiar401l                           97
032500060214     c  n97              movel     ar4not        vabnt2
032600000518      *dati relativi al contrassegno
032700051114     c     kAR9          chain     fIAR901l                           97
032800090226     c                   if        *in97='0'
032900051114     c                   z-add     AR9cas        vabcas
033000051114     c                   movel     AR9tic        vabtic
033100051114     c                   movel     AR9gca        vabgca
033200051114     c                   movel     AR9vca        vabvca
033300011005     c                   endif
033400021223      * Dati bancali
033500021223     c                   Clear                   VabAnt
033600030130     c                   If        %Subst(BlpGva:1:1) = 'B'
033700021223     c     KFiar5        Chain     Fiar501l
033800021223     c                   If        %Found(Fiar501l)
033900021223     c                   Movel     Ar5Uni        Dar5Ban
034000021223     c                   Movel     §Ar5Tba       VabTba
034100021223     c                   Movel     §Ar5Nba       VabNba
034200021223     c                   Movel     §Ar5Tb2       VabTb2
034300021223     c                   Movel     §Ar5Nb2       VabNb2
034400021223     c                   EndIf
034500021223     c                   EndIf
034600021223
034700000518     c                   endsr
034800000518?    C*-------------------------------------------------
034900000518?    C* scrivi - scrive   file work qtemp
035000000518?    C*-------------------------------------------------
035100000518     c     scrivi        begsr
035200000605     c                   move      *blank        vabscl
035300070222     c                   clear                   vabatb
035400000518     c                   movel     blpcbo        vabcbo
035500000518     c                   movel     blprsd        vabrsd
035600000518     c                   movel     blpind        vabind
035700000518     c                   movel     blpcad        vabcad
035800010927     c                   movel     blplod        vablod
035900000518     c                   movel     blpprd        vabprd
036000000518     c                   movel     blpnzd        vabnzd
036100000518     c                   movel     blpgc1        vabgc1
036200000518     c                   movel     blpgc2        vabgc2
036300000518     c                   movel     blptsp        vabtsp
036400000518     c                   movel     blpnas        vabnas
036500000518     c                   movel     blprma        vabrma
036600000518     c                   movel     blpctm        vabctm
036700000518     c                   movel     blpffd        vabffd
036800000518     c                   movel     blptcr        vabtcr
036900000518     c                   movel     blpcts        vabcts
037000000518     c                   movel     blpftm        vabftm
037100000518     c                   movel     blpvad        vabvad
037200000518     c                   movel     blpgma        vabgma
037300000518     c                   movel     blpgga        vabgga
037400000518     c                   movel     blpgva        vabgva
037500010111     c                   move      ' '           vabgva
037600000518     c                   movel     blptc1        vabtc1
037700000518     c                   movel     blptc2        vabtc2
037800000518     c                   movel     *blank        vabcmo
037900000518     c                   movel     *blank        vabnmo
038000000703     c                   move      '.'           vabnmo
038100060214      * se codice cliente contenuto nel campo note di FIAR4
038200011002     c                   if        *in66
038300120524     c****               move      ar4ksc        vabccm
038400120524     c                   eval      vabccm=%dec(%subst(t_ar4n30:1:7):7:0)
038500011002     c                   else
038600011002      * se codice cliente in FNBLP nel campo blpccm
038700000628     c                   if        blpccm > 0
038800000628     c                   z-add     blpccm        vabccm
038900000628     c                   else
039000011002      * se codice cliente in FNBLP nel campo blpksc
039100000628     c                   z-add     blpksc        vabccm
039200000628     c                   end
039300011002     c                   end
039400000518     c                   z-add     blplnp        vablnp
039500000518     c                   z-add     blpaas        vabaas
039600000518     c                   z-add     blpmgs        vabmgs
039700000518     c                   z-add     blpnrs        vabnrs
039800000518     c                   z-add     blpnsp        vabnsp
039900000518     c                   z-add     blplna        vablna
040000000518     c                   z-add     blpctr        vabctr
040100000518     c                   z-add     blpncl        vabncl
040200000703     c                   z-add     blppkf        vabpkb
040300000703     c                   z-add     blpvlf        vabvlb
040400000518     c                   z-add     blpqft        vabqft
040500000518     c                   z-add     blprmn        vabrmn
040600000518     c                   z-add     blpncd        vabncd
040700000518     c                   z-add     blpnca        vabnca
040800000518     c                   z-add     blpznc        vabznc
040900040916     c                   z-add     blpdsew       vabdcr
041000000621     c                   z-add     0             vabhcr
041100000518     c                   z-add     blpvmd        vabvmd
041200021223     c**!!!              z-add     0             vabant
041300001106      *
041400001106     c                   movel     blprmo        vabrmo
041500001106     c                   IF        VABRMO <> *blanks
041600001106     c                   movel     blpcam        vabcmo
041700001106     c                   movel     blpnzm        vabnmo
041800001106     c                   ELSE
041900001106     c                   clear                   vabcmo
042000001106     c                   clear                   vabnmo
042100001106     c                   ENDIF
042200001106     c                   move      '.'           vabnmo
042300090216     c                   z-add     blpias        vabias
042400090216     c                   movel     blpvas        vabvas
042500090220     C                   movel     blpXCO        vabXCO
042600001106      *
042700090316     C                   clear                   tivgd000
042800090316     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
042900090316     C                   eval      vgdTIP = 'BT'
043000090316     C                   movel     *all'0'       vgdKSU
043100090316     C                   move      §3qcks        vgdKSU
043200090316     C                   eval      vgdTSC = 'WW'
043300090316     C                   eval      vgdDAT = woggig
043400090316     C                   eval      vgdPGM = 'FNLSH0R'
043500090316     C*
043600090316     C                   write     tivgd000
043700001106      *
043800000518     c                   endsr
043900000518?    C*-------------------------------------------------------------
044000060214?    C* aggbl4 - aggiornamento tipo record W ar4 data presa in carico
044100000518?    C*-------------------------------------------------------------
044200000518     c     aggbl4        begsr
044300000518      *
044400000519      *aggiorna record con data presa in carico spedizione
044500000518     c                   move      'W'           ktrc
044600060214     c     kar4          chain     fiar401l                           97
044700000518     c                   if        not *in97
044800000518     c                   movel     woggig        §b4dat
044900060214     c                   movel     dsbl4w        ar4not
045000060214     c                   z-add     woggig        ar4duv
045100060214     c                   update    fiar4000
045200000518     c                   endif
045300000518     c                   endsr
045400010927?    C*-------------------------------------------------------------
045500060214?    C* aggbl4Z- aggiornamento tipo record Z ar4 cod.cli/datainvio
045600010927?    C*-------------------------------------------------------------
045700010927     c     aggbl4Z       begsr
045800010927      *
045900010927      *aggiorna record con data elaborazione
046000010927     c                   move      'Z'           ktrc
046100060214     c     kar4          chain     fiar401l                           97
046200010927     c                   if        not *in97
046300010927     c                   movel     woggig        §b4dtr
046400011010     c                   movel     kscj          §b4okc
046500060214     c                   movel     dsbl4z        ar4not
046600060214     c                   z-add     woggig        ar4duv
046700060214     c                   update    fiar4000
046800010927     c                   endif
046900010927     c                   endsr
047000040916?    C*-------------------------------------------------------------
047100060214?    C* aggbl4gg15 - aggiornamento tipo record W ar4 data ritiro + 15 giorni
047200040916?    C*-------------------------------------------------------------
047300040916     c     aggbl4gg15    begsr
047400040916      *
047500040920     c                   if        blpdbr < woggig15
047600040916      *aggiorna record con data presa in carico spedizione
047700040916     c                   move      'W'           ktrc
047800060214     c     kar4          chain     fiar401l                           97
047900040916     c                   if        not *in97
048000040916     c                   movel     20010101      §b4dat
048100060214     c                   movel     dsbl4w        ar4not
048200060214     c                   z-add     woggig        ar4duv
048300060214     c                   update    fiar4000
048400040920     c                   seton                                        56
048500040916     c                   endif
048600040916     c                   endif
048700040916     c                   endsr
048800040916?    C*-------------------------------------------------------------
048900040916?    C* aggbl4Zgg15- aggiornamento tipo record Z bl4 cod.cli/datainvio
049000040916?    C*-------------------------------------------------------------
049100040916     c     aggbl4Zgg15   begsr
049200040916      *
049300040920     c                   if        blpdbr < woggig15
049400040916      *aggiorna record con data elaborazione
049500040916     c                   move      'Z'           ktrc
049600060214     c     kar4          chain     fiar401l                           97
049700040916     c                   if        not *in97
049800040916     c                   movel     20010101      §b4dtr
049900040916     c                   movel     kscj          §b4okc
050000060214     c                   movel     dsbl4z        ar4not
050100060214     c                   z-add     woggig        ar4duv
050200040920     c                   seton                                        56
050300060214     c                   update    fiar4000
050400040916     c                   endif
050500040916     c                   endif
050600040916     c                   endsr
050700000517?    C*-------------------------------------------------
050800000517?    C* *INZSR - Settaggio iniziale programma STANDARD
050900000517?    C*-------------------------------------------------
051000000517     C     *inzsr        begsr
051100000517?    C*
051200000517     C     *entry        plist
051300000517     C                   parm                    kpjba
051400000517     c     ktab          klist
051500000517     c                   kfld                    codut
051600000517     c                   kfld                    cod
051700000517     c                   kfld                    key
051800000517      *
051900000517     c     ktab1         klist
052000000517     c                   kfld                    codut
052100000517     c                   kfld                    cod
052200100330      *
052300000517     c                   move      '3Q'          cod
052400120511     c*
052500120511     c     k63j          klist
052600120511     c                   kfld                    vuoto
052700120511     c                   kfld                    codice
052800120524     c*
052900120524     c     kar43         klist
053000120524     c                   kfld                    ktrc
053100120524     c                   kfld                    kn30
053200120524     c     kblp          klist
053300120524     c                   kfld                    t_ar4aas
053400120524     c                   kfld                    t_ar4lnp
053500120524     c                   kfld                    t_ar4nrs
053600120524     c                   kfld                    t_ar4nsp
053700100330      *
053800051114     c     kAR9          klist
053900000518     c                   kfld                    blpaas
054000000518     c                   kfld                    blplnp
054100000518     c                   kfld                    blpnrs
054200000518     c                   kfld                    blpnsp
054300060214     c     kar4          klist
054400000518     c                   kfld                    blpaas
054500000518     c                   kfld                    blplnp
054600000518     c                   kfld                    blpnrs
054700000518     c                   kfld                    blpnsp
054800000518     c                   kfld                    ktrc
054900021223     c     KFiar5        Klist
055000021223     c                   Kfld                    BlpAas
055100021223     c                   Kfld                    BlpLnp
055200021223     c                   Kfld                    BlpNrs
055300021223     c                   Kfld                    BlpNsp
055400021223     c                   Kfld                    KAr5Trd
055500040825     c     Karb1_c       Klist
055600040825     c                   Kfld                    BlpAas
055700040825     c                   Kfld                    BlpLnp
055800040825     c                   Kfld                    BlpNrs
055900040825     c                   Kfld                    BlpNsp
056000000518      *
056100000518     c                   time                    dataora
056200000518     c                   move      dataora       woggi
056300000518     c                   move      woggi         dataeur
056400000518     c                   move      dataeur       dataiso
056500000518     c                   move      dataiso       woggig
056600040916     c     dataiso       subdur    15:*d         dataiso
056700040916     c                   move      dataiso       woggig15          8 0
056800011005     C*---------------------------------------------------------------*
056900000517     c                   endsr
