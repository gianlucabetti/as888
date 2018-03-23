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
002200170301     f***tabel00f  if   e           k disk
002300040825     ffnarb01l  if   e           k disk
002400000517      *--------------------------------------------------------------------
002500000522     D kpjba         e ds
002600000517     D ds3q          e ds
002700170301     D tabel00f      e ds
002800000518     D dsbl4w        e ds
002900010927     D dsbl4z        e ds
003000021223     d dar5ban       e ds
003100090311     D FNVABDS       E DS                  EXTNAME(FNVAB00T)
003200090316     d  VabTba                        2s 0 overlay(vabant:1)
003300090316     d  VabNba                        2s 0 overlay(vabant:3)
003400090316     d  VabTb2                        2s 0 overlay(vabant:5)
003500090316     d  VabNb2                        2s 0 overlay(vabant:7)
003600090311     D trul47ds      e ds
003700140915     d fnlvemds      e ds
003800000517     d  codut          s              1  0 inz(1)
003900000517     d  cod            s                   like(tblcod)
004000000517     d  key            s                   like(tblkey)
004100010927     d  kscj           s              7
004200100330     d  codice         s                   like(blpksc)
004300060214     d  ktrc           s                   like(ar4trc)
004400021223     d  KAr5Trd        s                   Like(Ar5Trd) Inz('BAN')
004500000518     d  woggi          s              8  0
004600000518     d  woggig         s              8  0
004700000518     d  dataora        s             14  0
004800000518     d  dataeur        s               d   datfmt(*eur)
004900000518     d  dataiso        s               d   datfmt(*iso)
005000000519     d  como7          s              7
005100120511     d  vuoto          s                   like(codice)
005200120524     d  kn30           s                   like(t_ar4n30)
005300120524     d  kn15           s             15
005400170301     d wSQl            s           1024    inz  varying
005500140915
005600140915     d* PROTOTIPI
005700140915     d fnblpds       e ds                  extname('FNBLP00F')
005800140915     d                                     prefix(p_)
005900140915     d fnlvemr         PR
006000140915     d                                     extpgm('FNLVEMR')
006100140915     d  kpjba                              likeds(kpjba)
006200140915     d  fnblpds                            likeds(fnblpds)
006300160629
006400160629     d/copy gaitrasrc/srcProtoPi,UBCOL00R
006500021223
006600000517     d
006700000517      *--------------------------------------------------------------------
006800090311     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrent
006900090311     C                   open      tivgd00f
007000090311     C                   clear                   trul47ds
007100090311     C                   eval      d47opz  = 'I'
007200090311     C                   eval      d47tip  = 'BT'
007300090311     C                   eval      d47lck  = 'N'
007400090311     C                   eval      d47chkj = 'N'
007500090311     C                   eval      d47pgm  = 'FNLSH0R'
007600090311     C                   call      'TRUL47R'
007700090311     C                   parm                    trul47ds
007800090311     C*
007900090311     C* Se elaborazione consentita => proseguo
008000090311     C                   if        d47sts <> 'A'
008100000517     c
008200170301     c***  ktab1         setll     tabel
008300170301     c                   exsr      setll_sql
008400170301     c***                do        *hival
008500170301     c  N99              do        *hival
008600170301     c***                read      tabel                                  99
008700170301     c                   exsr      fetch_sql
008800000517     C   99              leave
008900000529     c                   if        tblcod <> '3Q'
009000000529     c                   leave
009100000529     c                   end
009200000522     c                   if        tblflg <> *blank
009300000522     c                   iter
009400000522     c                   end
009500000517     c                   movel     tbluni        ds3q
009600100330     c                   movel     tblkey        kscj
009700100330     c                   movel     tblkey        codice
009800120524     c                   move      'Z'           ktrc
009900120524     c                   eval      kn30=kscj+'00000000'
010000120524     c                   eval      kn15=kscj+'00000000'
010100000517     c
010200000914     c
010300120511     c*    codice        chain     fnblp65j                           98
010400120511     c*  98codice        chain     fnblp64j                           98
010500120524     c*  98kscj          chain     fnblp66j                           98
010600120524     c     codice        chain     fnblp63j                           98
010700120524     c   98k63j          chain     fnblp63j                           98
010800120524     c   98kar43         setll     fiar403l
010900120524     c   98'Z'           reade     fiar403l
011000000914     c
011100120524     c                   if        %found(fnblp63j)
011200120524     c                             or (not %eof(fiar403l) and
011300120524     c                                 %subst(t_AR4N30:1:15)=kn15)
011400000518     c                   exsr      estrai
011500000518     c                   endif
011600000517     c                   enddo
011700090311     c                   endif
011800090311     c                   close     tivgd00f
011900090311     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
012000090311     C                   clear                   trul47ds
012100090311     C                   eval      d47opz  = 'F'
012200090311     C                   eval      d47tip  = 'BT'
012300090311     C                   call      'TRUL47R'
012400090311     C                   parm                    trul47ds
012500170301
012600170301     c                   exsr      close_sql
012700090311
012800990504     c                   seton                                        lr
012900000518?    C*-------------------------------------------------------------
013000000518?    C* estrai - estrae x file in qtemp x successiva scrittura membri
013100000518?    C*-------------------------------------------------------------
013200000518     c     estrai        begsr
013300010927      *loop di lettura per codice cliente mittente in KSC
013400000518      *
013500000518     c                   setoff                                       98
013600120511     c*    codice        setll     fnblp65j
013700120511     c     k63j          setll     fnblp63j
013800000518     c                   do        *hival
013900120511     c*    codice        reade     fnblp65j                               98
014000120511     c     k63j          reade     fnblp63j                               98
014100000518     c   98              leave
014200090316     c                   clear                   fnvabds
014300040916     c                   z-add     blpdse        blpdsew           8 0
014400040920      * prima di verificare il record da elaborare verifico che non sia
014500040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
014600040920      * se è così viene flaggato il BL4 per non trattare il record
014700040920      *
014800040920     c                   setoff                                       56
014900040920     c                   if        blpdbr > 0
015000040920     c                   exsr      aggbl4gg15
015100040920      * *in56 = on bypass il resto del loop
015200040920     c   56              iter
015300040920     c                   endif
015400000522      *aggiorno e spedisco solo le righe con data minore uguale a oggi
015500000623     c                   if        blpdse > woggig or blpft1 = *blank
015600000705     c                             or blpdse = 0
015700040825     c*
015800040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
015900040825     c*partenza a *zeros
016000040825     c                   exsr      chkdse
016100040825     c*
016200140915     c  N55              exsr      chk3Q
016300140915     c*
016400140915     c  N55              iter
016500000522     c                   end
016600000519     c                   exsr      datiagg
016700000518     c                   exsr      scrivi
016800000519     c                   exsr      aggbl4
016900000518     c                   enddo
017000100330      *loop di lettura con codice cliente in CCM
017100000518     c                   setoff                                       98
017200120511     c*    codice        setll     fnblp64j
017300120511     c     codice        setll     fnblp63j
017400000518     c                   do        *hival
017500120511     c*    codice        reade     fnblp64j                               98
017600120511     c     codice        reade     fnblp63j                               98
017700000518     c   98              leave
017800090316     c                   clear                   fnvabds
017900040916     c                   z-add     blpdse        blpdsew
018000040920      * prima di verificare il record da elaborare verifico che non sia
018100040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
018200040920      * se è così viene flaggato il BL4 per non trattare il record
018300040920      *
018400040920     c                   setoff                                       56
018500040920     c                   if        blpdbr > 0
018600040920     c                   exsr      aggbl4gg15
018700040920      * *in56 = on bypass il resto del loop
018800040920     c   56              iter
018900040920     c                   endif
019000000623     c                   if        blpdse > woggig or blpft1 = *blank
019100000705     c                             or blpdse = 0
019200040825     c*
019300040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
019400040825     c*partenza a *zeros
019500040825     c                   exsr      chkdse
019600140915     c*
019700140915     c  N55              exsr      chk3Q
019800040825     c*
019900040825     c  N55              iter
020000000522     c                   end
020100000518     c                   exsr      datiagg
020200000518     c                   exsr      scrivi
020300000518     c                   exsr      aggbl4
020400000518     c                   enddo
020500060214      *loop di lettura con codice cliente in AR4NOT (altra V.L.)
020600000518     c
020700120524     c                   move      'Z'           ktrc
020800010927     c                   setoff                                       98
020900120524     c*    kscj          setll     fnblp66j
021000120524     c     kar43         setll     fiar403l
021100011002     c                   seton                                        66
021200010927     c                   do        *hival
021300120524     c*    kscj          reade     fnblp66j                               98
021400120524     c*  98              leave
021500120524     c     'Z'           reade     fiar403l
021600120524     c                   if        %eof(fiar403l) or
021700120524     c                             %subst(t_ar4n30:1:15)>kn15
021800120524     c                   leave
021900120524     c                   endif
022000120524     c     kblp          chain     fnblp01l
022100120524     c                   if        %found(fnblp01l)
022200090316     c                   clear                   fnvabds
022300040916     c                   z-add     blpdse        blpdsew
022400040920      * prima di verificare il record da elaborare verifico che non sia
022500040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
022600040920      * se è così viene flaggato il BL4 per non trattare il record
022700040920      *
022800040920     c                   setoff                                       56
022900040920     c                   if        blpdbr > 0
023000040920     c                   exsr      aggbl4zgg15
023100040920      * *in56 = on bypass il resto del loop
023200040920     c   56              iter
023300040920     c                   endif
023400010927     c                   if        blpdse > woggig or blpft1 = *blank
023500010927     c                             or blpdse = 0
023600040825     c*
023700040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
023800040825     c*partenza a *zeros
023900040825     c                   exsr      chkdse
024000140915     c*
024100140915     c  N55              exsr      chk3Q
024200040825     c*
024300040825     c  N55              iter
024400010927     c                   end
024500010927     c                   exsr      datiagg
024600010927     c                   exsr      scrivi
024700010927     c                   exsr      aggbl4Z
024800120524     c                   endif
024900010927     c                   enddo
025000011002     c                   setoff                                       66
025100090316
025200090316?    C                   commit
025300010927
025400000518     c                   endsr
025500040825?    C*-------------------------------------------------------------
025600040825?    C* chkdse - verifiche aggiuntive in caso d data spunta entrata in part. = 0
025700040825?    C*-------------------------------------------------------------
025800040825     c     chkdse        begsr
025900040825     c*
026000040825     c*se in partenza data spunta entrata è *zeros chaino le bolle in arrivo
026100040825     c*x verificare se collo è stato poi cmq "spuntato" in arrivo
026200040825     c                   setoff                                       55
026300040825     c                   if        blpdse = 0
026400040825     c     karb1_c       chain     fnarb01l
026500040825     c                   if        %found(fnarb01l)
026600040916     c                   if        arbdam > 0 AND  arbcca <> '5' and
026700040825     c*paletto la condizione a partire dal 20040823 xchè è il giorno in cui
026800040825     c*mi sono accorto del problema (l'ha detto Luciano d fare così!)
026900040916     c                             blpdim > 20040823   OR
027000040916     c                             arbdam > 0 AND  arbcca = '5' and
027100040916     c                             arbdam <> arbdcm and blpdim > 20040823
027200040825     c                   seton                                        55
027300040916     c                   z-add     blpdrt        blpdsew
027400040825     c                   endif
027500040825     c                   endif
027600040825     c                   endif
027700040825     c*
027800040825     c                   endsr
027900000518?    C*-------------------------------------------------------------
028000140915?    C* controllo se si può proseguire nell'estrazione perché su
028100140915?    C* tab. 3Q è richiesto un invio anticipato
028200000518?    C*-------------------------------------------------------------
028300140915     c     Chk3Q         begsr
028400140915
028500140915      /free
028600140915       select;
028700140915         // se invio antipato dei dati su bollettazione
028800140915         // ma non per dati su spunta
028900140915         when §3QADB = 'S' and
029000140915              §3QADS = *blank;
029100140915           // il rcd è OK a prescindere
029200140915           *in55 = *on;
029300140915           leavesr;
029400140915         // se invio antipato dei dati su bollettazione per spunta
029500140915         when §3QADS = 'S';
029600140915           // controllo se spedizione è spuntata
029700140915           clear fnlvemds;
029800140915           clear fnblpds;
029900140915           iemAAS = blpAAS;
030000140915           iemLNP = blpLNP;
030100140915           iemNRS = blpNRS;
030200140915           iemNSP = blpNSP;
030300140915           kpjbu = fnlvemds;
030400140915           callP(e) FNLVEMR (kpjba : fnblpds);
030500140915           fnlvemds = kpjbu;
030600160629           // testo se regolarmente spuntata
030700140915           if %error OR oemESITO <> '1';
030800160629             // se NON regolarmente spuntata, controllo se ha almeno una spunta
030900160629             // in partenza
031000160629             exsr CercoSpunta;
031100160629             // se c'è almeno una spunta (basta che il primo elemento della
031200160629             // schiera della data spunta sia valorizzato), rcd è OK
031300160629             if UBCOLSkDFS(1) <> 0;
031400160629               *in55 = *on;
031500160629               leavesr;
031600160629             endif;
031700140915           else;
031800140915             *in55 = *on;
031900140915             leavesr;
032000140915           endif;
032100140915       endsl;
032200140915
032300140915      /end-free
032400140915
032500140915     c*
032600140915     c                   endsr
032700140915?    C*-------------------------------------------------------------
032800160629?    C* cerco la spunta
032900140915?    C*-------------------------------------------------------------
033000160629     c     CercoSpunta   begsr
033100160629     C*
033200160629     c                   eval      UBCOLAAS =    blpAAS
033300160629     c                   eval      UBCOLLNP =    blpLNP
033400160629     c                   eval      UBCOLNRS =    blpNRS
033500160629     c                   eval      UBCOLNSP =    blpNSP
033600160629     C*
033700160629     C                   CALL      'UBCOL00R'
033800160705     c                   parm      'FV5'         UBCOLOpz
033900160629     c                   parm      *blank        UBCOLFlgOpe
034000160629     c                   parm      *blank        UBCOLTLa
034100160629     c                   parm                    UBCOLAAS
034200160629     c                   parm                    UBCOLLNP
034300160629     c                   parm                    UBCOLNRS
034400160629     c                   parm                    UBCOLNSP
034500160629     c                   parm                    UBCOLNCL
034600160629     c                   parm                    UBCOLTIP
034700160629     c                   parm                    UBCOLDCM
034800160629     C                   PARM                    UBCOLHMC
034900160629     C                   PARM                    UBCOLSkBRT
035000160629     C                   PARM                    UBCOLSkCLI
035100160629     c                   parm                    UBCOLSkDCM
035200160629     c                   parm                    UBCOLSkHMC
035300160629     c                   parm                    UBCOLSkDFS
035400160629     c                   parm                    UBCOLSkHMS
035500160629     c                   parm                    UBCOLSkVUC
035600160629     c                   parm                    UBCOLSkPUC
035700160629     c                   parm                    UBCOLErr
035800160629     C*
035900160629     c                   endsr
036000160629?    C*-------------------------------------------------------------
036100160629?    C* datiagg- dati aggiuntivi da ar4 e AR9
036200160629?    C*-------------------------------------------------------------
036300160629     c     datiagg       begsr
036400160629      *
036500000518      *seconda ragione sociale cliente
036600000518     c                   move      'D'           ktrc
036700060519     c     kar4          chain(N)  fiar401l                           97
036800060214     c  n97              movel     ar4not        vabrd2
036900000605      *campo note bolla
037000000605     c                   move      '8'           ktrc
037100060519     c     kar4          chain(N)  fiar401l                           97
037200060214     c  n97              movel     ar4not        vabnot
037300000518      *campo note bolla 2
037400000518     c                   move      '9'           ktrc
037500060519     c     kar4          chain(N)  fiar401l                           97
037600060214     c  n97              movel     ar4not        vabnt2
037700000518      *dati relativi al contrassegno
037800051114     c     kAR9          chain     fIAR901l                           97
037900090226     c                   if        *in97='0'
038000051114     c                   z-add     AR9cas        vabcas
038100051114     c                   movel     AR9tic        vabtic
038200051114     c                   movel     AR9gca        vabgca
038300051114     c                   movel     AR9vca        vabvca
038400011005     c                   endif
038500021223      * Dati bancali
038600021223     c                   Clear                   VabAnt
038700030130     c                   If        %Subst(BlpGva:1:1) = 'B'
038800021223     c     KFiar5        Chain     Fiar501l
038900021223     c                   If        %Found(Fiar501l)
039000021223     c                   Movel     Ar5Uni        Dar5Ban
039100021223     c                   Movel     §Ar5Tba       VabTba
039200021223     c                   Movel     §Ar5Nba       VabNba
039300021223     c                   Movel     §Ar5Tb2       VabTb2
039400021223     c                   Movel     §Ar5Nb2       VabNb2
039500021223     c                   EndIf
039600021223     c                   EndIf
039700021223
039800000518     c                   endsr
039900000518?    C*-------------------------------------------------
040000000518?    C* scrivi - scrive   file work qtemp
040100000518?    C*-------------------------------------------------
040200000518     c     scrivi        begsr
040300000605     c                   move      *blank        vabscl
040400070222     c                   clear                   vabatb
040500000518     c                   movel     blpcbo        vabcbo
040600000518     c                   movel     blprsd        vabrsd
040700000518     c                   movel     blpind        vabind
040800000518     c                   movel     blpcad        vabcad
040900010927     c                   movel     blplod        vablod
041000000518     c                   movel     blpprd        vabprd
041100000518     c                   movel     blpnzd        vabnzd
041200000518     c                   movel     blpgc1        vabgc1
041300000518     c                   movel     blpgc2        vabgc2
041400000518     c                   movel     blptsp        vabtsp
041500000518     c                   movel     blpnas        vabnas
041600000518     c                   movel     blprma        vabrma
041700000518     c                   movel     blpctm        vabctm
041800000518     c                   movel     blpffd        vabffd
041900000518     c                   movel     blptcr        vabtcr
042000000518     c                   movel     blpcts        vabcts
042100000518     c                   movel     blpftm        vabftm
042200000518     c                   movel     blpvad        vabvad
042300000518     c                   movel     blpgma        vabgma
042400000518     c                   movel     blpgga        vabgga
042500000518     c                   movel     blpgva        vabgva
042600010111     c                   move      ' '           vabgva
042700000518     c                   movel     blptc1        vabtc1
042800000518     c                   movel     blptc2        vabtc2
042900000518     c                   movel     *blank        vabcmo
043000000518     c                   movel     *blank        vabnmo
043100000703     c                   move      '.'           vabnmo
043200060214      * se codice cliente contenuto nel campo note di FIAR4
043300011002     c                   if        *in66
043400120524     c****               move      ar4ksc        vabccm
043500120524     c                   eval      vabccm=%dec(%subst(t_ar4n30:1:7):7:0)
043600011002     c                   else
043700011002      * se codice cliente in FNBLP nel campo blpccm
043800000628     c                   if        blpccm > 0
043900000628     c                   z-add     blpccm        vabccm
044000000628     c                   else
044100011002      * se codice cliente in FNBLP nel campo blpksc
044200000628     c                   z-add     blpksc        vabccm
044300000628     c                   end
044400011002     c                   end
044500000518     c                   z-add     blplnp        vablnp
044600000518     c                   z-add     blpaas        vabaas
044700000518     c                   z-add     blpmgs        vabmgs
044800000518     c                   z-add     blpnrs        vabnrs
044900000518     c                   z-add     blpnsp        vabnsp
045000000518     c                   z-add     blplna        vablna
045100000518     c                   z-add     blpctr        vabctr
045200000518     c                   z-add     blpncl        vabncl
045300000703     c                   z-add     blppkf        vabpkb
045400000703     c                   z-add     blpvlf        vabvlb
045500000518     c                   z-add     blpqft        vabqft
045600000518     c                   z-add     blprmn        vabrmn
045700000518     c                   z-add     blpncd        vabncd
045800000518     c                   z-add     blpnca        vabnca
045900000518     c                   z-add     blpznc        vabznc
046000040916     c                   z-add     blpdsew       vabdcr
046100000621     c                   z-add     0             vabhcr
046200000518     c                   z-add     blpvmd        vabvmd
046300021223     c**!!!              z-add     0             vabant
046400001106      *
046500001106     c                   movel     blprmo        vabrmo
046600001106     c                   IF        VABRMO <> *blanks
046700001106     c                   movel     blpcam        vabcmo
046800001106     c                   movel     blpnzm        vabnmo
046900001106     c                   ELSE
047000001106     c                   clear                   vabcmo
047100001106     c                   clear                   vabnmo
047200001106     c                   ENDIF
047300001106     c                   move      '.'           vabnmo
047400090216     c                   z-add     blpias        vabias
047500090216     c                   movel     blpvas        vabvas
047600090220     C                   movel     blpXCO        vabXCO
047700001106      *
047800090316     C                   clear                   tivgd000
047900090316     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
048000090316     C                   eval      vgdTIP = 'BT'
048100090316     C                   movel     *all'0'       vgdKSU
048200090316     C                   move      §3qcks        vgdKSU
048300090316     C                   eval      vgdTSC = 'WW'
048400090316     C                   eval      vgdDAT = woggig
048500090316     C                   eval      vgdPGM = 'FNLSH0R'
048600090316     C*
048700090316     C                   write     tivgd000
048800001106      *
048900000518     c                   endsr
049000000518?    C*-------------------------------------------------------------
049100060214?    C* aggbl4 - aggiornamento tipo record W ar4 data presa in carico
049200000518?    C*-------------------------------------------------------------
049300000518     c     aggbl4        begsr
049400000518      *
049500000519      *aggiorna record con data presa in carico spedizione
049600000518     c                   move      'W'           ktrc
049700060214     c     kar4          chain     fiar401l                           97
049800000518     c                   if        not *in97
049900000518     c                   movel     woggig        §b4dat
050000060214     c                   movel     dsbl4w        ar4not
050100060214     c                   z-add     woggig        ar4duv
050200060214     c                   update    fiar4000
050300000518     c                   endif
050400000518     c                   endsr
050500010927?    C*-------------------------------------------------------------
050600060214?    C* aggbl4Z- aggiornamento tipo record Z ar4 cod.cli/datainvio
050700010927?    C*-------------------------------------------------------------
050800010927     c     aggbl4Z       begsr
050900010927      *
051000010927      *aggiorna record con data elaborazione
051100010927     c                   move      'Z'           ktrc
051200060214     c     kar4          chain     fiar401l                           97
051300010927     c                   if        not *in97
051400010927     c                   movel     woggig        §b4dtr
051500011010     c                   movel     kscj          §b4okc
051600060214     c                   movel     dsbl4z        ar4not
051700060214     c                   z-add     woggig        ar4duv
051800060214     c                   update    fiar4000
051900010927     c                   endif
052000010927     c                   endsr
052100040916?    C*-------------------------------------------------------------
052200060214?    C* aggbl4gg15 - aggiornamento tipo record W ar4 data ritiro + 15 giorni
052300040916?    C*-------------------------------------------------------------
052400040916     c     aggbl4gg15    begsr
052500040916      *
052600040920     c                   if        blpdbr < woggig15
052700040916      *aggiorna record con data presa in carico spedizione
052800040916     c                   move      'W'           ktrc
052900060214     c     kar4          chain     fiar401l                           97
053000040916     c                   if        not *in97
053100040916     c                   movel     20010101      §b4dat
053200060214     c                   movel     dsbl4w        ar4not
053300060214     c                   z-add     woggig        ar4duv
053400060214     c                   update    fiar4000
053500040920     c                   seton                                        56
053600040916     c                   endif
053700040916     c                   endif
053800040916     c                   endsr
053900040916?    C*-------------------------------------------------------------
054000040916?    C* aggbl4Zgg15- aggiornamento tipo record Z bl4 cod.cli/datainvio
054100040916?    C*-------------------------------------------------------------
054200040916     c     aggbl4Zgg15   begsr
054300040916      *
054400040920     c                   if        blpdbr < woggig15
054500040916      *aggiorna record con data elaborazione
054600040916     c                   move      'Z'           ktrc
054700060214     c     kar4          chain     fiar401l                           97
054800040916     c                   if        not *in97
054900040916     c                   movel     20010101      §b4dtr
055000040916     c                   movel     kscj          §b4okc
055100060214     c                   movel     dsbl4z        ar4not
055200060214     c                   z-add     woggig        ar4duv
055300040920     c                   seton                                        56
055400060214     c                   update    fiar4000
055500040916     c                   endif
055600040916     c                   endif
055700040916     c                   endsr
055800170301       //--------------------------------------------------------------
055900170301       // setll_sql  - prepara sql su TABEL00F per scorrere tab. 3Q
056000170301       //--------------------------------------------------------------
056100170301       begsr  setll_sql;
056200170301
056300170301         wSQL = 'select * +
056400170301                 from tabel00f +
056500170301                 where +
056600170301                 tblkut = 1 and +
056700170301                 tblcod = ''3Q'' +
056800170301                 order by +
056900170301                 substr(TBLUNI , 13 , 7)';
057000170301
057100170301         // Dichiarazione cursore
057200170301         exec sql   prepare S1   from :wSQL;
057300170301         exec sql   declare C1  asensitive   cursor for S1;
057400170301
057500170301         // Apertura del cursore
057600170301         exec sql   open C1;
057700170301
057800170301         *in99 = *off;
057900170301         if SQLCode < 0;
058000170301           *in99 = *on;
058100170301         endif;
058200170301
058300170301       endsr;
058400170301       //--------------------------------------------------------------
058500170301       // fetch_sql  - legge il rcdset di TABEL00F
058600170301       //--------------------------------------------------------------
058700170301       begsr  fetch_sql;
058800170301
058900170301         exec sql  fetch next  from C1  into :TABEL00F;
059000170301
059100170301         *in99 = *off;
059200170301         if SQLCode = 100 or SQLCode < 0;
059300170301           *in99 = *on;
059400170301         endif;
059500170301
059600170301       endsr;
059700170301       //--------------------------------------------------------------
059800170301       // close_sql  - chiusura cursore
059900170301       //--------------------------------------------------------------
060000170301       begsr  close_sql;
060100170301
060200170301         // chiusura del cursore
060300170301         exec sql  close C1;
060400170301
060500170301       endsr;
060600000517?    C*-------------------------------------------------
060700000517?    C* *INZSR - Settaggio iniziale programma STANDARD
060800000517?    C*-------------------------------------------------
060900000517     C     *inzsr        begsr
061000000517?    C*
061100000517     C     *entry        plist
061200000517     C                   parm                    kpjba
061300000517     c     ktab          klist
061400000517     c                   kfld                    codut
061500000517     c                   kfld                    cod
061600000517     c                   kfld                    key
061700000517      *
061800000517     c     ktab1         klist
061900000517     c                   kfld                    codut
062000000517     c                   kfld                    cod
062100100330      *
062200000517     c                   move      '3Q'          cod
062300120511     c*
062400120511     c     k63j          klist
062500120511     c                   kfld                    vuoto
062600120511     c                   kfld                    codice
062700120524     c*
062800120524     c     kar43         klist
062900120524     c                   kfld                    ktrc
063000120524     c                   kfld                    kn30
063100120524     c     kblp          klist
063200120524     c                   kfld                    t_ar4aas
063300120524     c                   kfld                    t_ar4lnp
063400120524     c                   kfld                    t_ar4nrs
063500120524     c                   kfld                    t_ar4nsp
063600100330      *
063700051114     c     kAR9          klist
063800000518     c                   kfld                    blpaas
063900000518     c                   kfld                    blplnp
064000000518     c                   kfld                    blpnrs
064100000518     c                   kfld                    blpnsp
064200060214     c     kar4          klist
064300000518     c                   kfld                    blpaas
064400000518     c                   kfld                    blplnp
064500000518     c                   kfld                    blpnrs
064600000518     c                   kfld                    blpnsp
064700000518     c                   kfld                    ktrc
064800021223     c     KFiar5        Klist
064900021223     c                   Kfld                    BlpAas
065000021223     c                   Kfld                    BlpLnp
065100021223     c                   Kfld                    BlpNrs
065200021223     c                   Kfld                    BlpNsp
065300021223     c                   Kfld                    KAr5Trd
065400040825     c     Karb1_c       Klist
065500040825     c                   Kfld                    BlpAas
065600040825     c                   Kfld                    BlpLnp
065700040825     c                   Kfld                    BlpNrs
065800040825     c                   Kfld                    BlpNsp
065900000518      *
066000000518     c                   time                    dataora
066100000518     c                   move      dataora       woggi
066200000518     c                   move      woggi         dataeur
066300000518     c                   move      dataeur       dataiso
066400000518     c                   move      dataiso       woggig
066500040916     c     dataiso       subdur    15:*d         dataiso
066600040916     c                   move      dataiso       woggig15          8 0
066700011005     C*---------------------------------------------------------------*
066800000517     c                   endsr
