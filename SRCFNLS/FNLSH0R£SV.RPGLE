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
006100160629
006200160629     d/copy gaitrasrc/srcProtoPi,UBCOL00R
006300021223
006400000517     d
006500000517      *--------------------------------------------------------------------
006600090311     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrent
006700090311     C                   open      tivgd00f
006800090311     C                   clear                   trul47ds
006900090311     C                   eval      d47opz  = 'I'
007000090311     C                   eval      d47tip  = 'BT'
007100090311     C                   eval      d47lck  = 'N'
007200090311     C                   eval      d47chkj = 'N'
007300090311     C                   eval      d47pgm  = 'FNLSH0R'
007400090311     C                   call      'TRUL47R'
007500090311     C                   parm                    trul47ds
007600090311     C*
007700090311     C* Se elaborazione consentita => proseguo
007800090311     C                   if        d47sts <> 'A'
007900000517     c
008000090316     c     ktab1         setll     tabel
008100000517     c                   do        *hival
008200090316     c                   read      tabel                                  99
008300000517     C   99              leave
008400000529     c                   if        tblcod <> '3Q'
008500000529     c                   leave
008600000529     c                   end
008700000522     c                   if        tblflg <> *blank
008800000522     c                   iter
008900000522     c                   end
009000000517     c                   movel     tbluni        ds3q
009100100330     c                   movel     tblkey        kscj
009200100330     c                   movel     tblkey        codice
009300120524     c                   move      'Z'           ktrc
009400120524     c                   eval      kn30=kscj+'00000000'
009500120524     c                   eval      kn15=kscj+'00000000'
009600000517     c
009700000914     c
009800120511     c*    codice        chain     fnblp65j                           98
009900120511     c*  98codice        chain     fnblp64j                           98
010000120524     c*  98kscj          chain     fnblp66j                           98
010100120524     c     codice        chain     fnblp63j                           98
010200120524     c   98k63j          chain     fnblp63j                           98
010300120524     c   98kar43         setll     fiar403l
010400120524     c   98'Z'           reade     fiar403l
010500000914     c
010600120524     c                   if        %found(fnblp63j)
010700120524     c                             or (not %eof(fiar403l) and
010800120524     c                                 %subst(t_AR4N30:1:15)=kn15)
010900000518     c                   exsr      estrai
011000000518     c                   endif
011100000517     c                   enddo
011200090311     c                   endif
011300090311     c                   close     tivgd00f
011400090311     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
011500090311     C                   clear                   trul47ds
011600090311     C                   eval      d47opz  = 'F'
011700090311     C                   eval      d47tip  = 'BT'
011800090311     C                   call      'TRUL47R'
011900090311     C                   parm                    trul47ds
012000090311
012100990504     c                   seton                                        lr
012200000518?    C*-------------------------------------------------------------
012300000518?    C* estrai - estrae x file in qtemp x successiva scrittura membri
012400000518?    C*-------------------------------------------------------------
012500000518     c     estrai        begsr
012600010927      *loop di lettura per codice cliente mittente in KSC
012700000518      *
012800000518     c                   setoff                                       98
012900120511     c*    codice        setll     fnblp65j
013000120511     c     k63j          setll     fnblp63j
013100000518     c                   do        *hival
013200120511     c*    codice        reade     fnblp65j                               98
013300120511     c     k63j          reade     fnblp63j                               98
013400000518     c   98              leave
013500090316     c                   clear                   fnvabds
013600040916     c                   z-add     blpdse        blpdsew           8 0
013700040920      * prima di verificare il record da elaborare verifico che non sia
013800040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
013900040920      * se è così viene flaggato il BL4 per non trattare il record
014000040920      *
014100040920     c                   setoff                                       56
014200040920     c                   if        blpdbr > 0
014300040920     c                   exsr      aggbl4gg15
014400040920      * *in56 = on bypass il resto del loop
014500040920     c   56              iter
014600040920     c                   endif
014700000522      *aggiorno e spedisco solo le righe con data minore uguale a oggi
014800000623     c                   if        blpdse > woggig or blpft1 = *blank
014900000705     c                             or blpdse = 0
015000040825     c*
015100040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
015200040825     c*partenza a *zeros
015300040825     c                   exsr      chkdse
015400040825     c*
015500140915     c  N55              exsr      chk3Q
015600140915     c*
015700140915     c  N55              iter
015800000522     c                   end
015900000519     c                   exsr      datiagg
016000000518     c                   exsr      scrivi
016100000519     c                   exsr      aggbl4
016200000518     c                   enddo
016300100330      *loop di lettura con codice cliente in CCM
016400000518     c                   setoff                                       98
016500120511     c*    codice        setll     fnblp64j
016600120511     c     codice        setll     fnblp63j
016700000518     c                   do        *hival
016800120511     c*    codice        reade     fnblp64j                               98
016900120511     c     codice        reade     fnblp63j                               98
017000000518     c   98              leave
017100090316     c                   clear                   fnvabds
017200040916     c                   z-add     blpdse        blpdsew
017300040920      * prima di verificare il record da elaborare verifico che non sia
017400040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
017500040920      * se è così viene flaggato il BL4 per non trattare il record
017600040920      *
017700040920     c                   setoff                                       56
017800040920     c                   if        blpdbr > 0
017900040920     c                   exsr      aggbl4gg15
018000040920      * *in56 = on bypass il resto del loop
018100040920     c   56              iter
018200040920     c                   endif
018300000623     c                   if        blpdse > woggig or blpft1 = *blank
018400000705     c                             or blpdse = 0
018500040825     c*
018600040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
018700040825     c*partenza a *zeros
018800040825     c                   exsr      chkdse
018900140915     c*
019000140915     c  N55              exsr      chk3Q
019100040825     c*
019200040825     c  N55              iter
019300000522     c                   end
019400000518     c                   exsr      datiagg
019500000518     c                   exsr      scrivi
019600000518     c                   exsr      aggbl4
019700000518     c                   enddo
019800060214      *loop di lettura con codice cliente in AR4NOT (altra V.L.)
019900000518     c
020000120524     c                   move      'Z'           ktrc
020100010927     c                   setoff                                       98
020200120524     c*    kscj          setll     fnblp66j
020300120524     c     kar43         setll     fiar403l
020400011002     c                   seton                                        66
020500010927     c                   do        *hival
020600120524     c*    kscj          reade     fnblp66j                               98
020700120524     c*  98              leave
020800120524     c     'Z'           reade     fiar403l
020900120524     c                   if        %eof(fiar403l) or
021000120524     c                             %subst(t_ar4n30:1:15)>kn15
021100120524     c                   leave
021200120524     c                   endif
021300120524     c     kblp          chain     fnblp01l
021400120524     c                   if        %found(fnblp01l)
021500090316     c                   clear                   fnvabds
021600040916     c                   z-add     blpdse        blpdsew
021700040920      * prima di verificare il record da elaborare verifico che non sia
021800040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
021900040920      * se è così viene flaggato il BL4 per non trattare il record
022000040920      *
022100040920     c                   setoff                                       56
022200040920     c                   if        blpdbr > 0
022300040920     c                   exsr      aggbl4zgg15
022400040920      * *in56 = on bypass il resto del loop
022500040920     c   56              iter
022600040920     c                   endif
022700010927     c                   if        blpdse > woggig or blpft1 = *blank
022800010927     c                             or blpdse = 0
022900040825     c*
023000040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
023100040825     c*partenza a *zeros
023200040825     c                   exsr      chkdse
023300140915     c*
023400140915     c  N55              exsr      chk3Q
023500040825     c*
023600040825     c  N55              iter
023700010927     c                   end
023800010927     c                   exsr      datiagg
023900010927     c                   exsr      scrivi
024000010927     c                   exsr      aggbl4Z
024100120524     c                   endif
024200010927     c                   enddo
024300011002     c                   setoff                                       66
024400090316
024500090316?    C                   commit
024600010927
024700000518     c                   endsr
024800040825?    C*-------------------------------------------------------------
024900040825?    C* chkdse - verifiche aggiuntive in caso d data spunta entrata in part. = 0
025000040825?    C*-------------------------------------------------------------
025100040825     c     chkdse        begsr
025200040825     c*
025300040825     c*se in partenza data spunta entrata è *zeros chaino le bolle in arrivo
025400040825     c*x verificare se collo è stato poi cmq "spuntato" in arrivo
025500040825     c                   setoff                                       55
025600040825     c                   if        blpdse = 0
025700040825     c     karb1_c       chain     fnarb01l
025800040825     c                   if        %found(fnarb01l)
025900040916     c                   if        arbdam > 0 AND  arbcca <> '5' and
026000040825     c*paletto la condizione a partire dal 20040823 xchè è il giorno in cui
026100040825     c*mi sono accorto del problema (l'ha detto Luciano d fare così!)
026200040916     c                             blpdim > 20040823   OR
026300040916     c                             arbdam > 0 AND  arbcca = '5' and
026400040916     c                             arbdam <> arbdcm and blpdim > 20040823
026500040825     c                   seton                                        55
026600040916     c                   z-add     blpdrt        blpdsew
026700040825     c                   endif
026800040825     c                   endif
026900040825     c                   endif
027000040825     c*
027100040825     c                   endsr
027200000518?    C*-------------------------------------------------------------
027300140915?    C* controllo se si può proseguire nell'estrazione perché su
027400140915?    C* tab. 3Q è richiesto un invio anticipato
027500000518?    C*-------------------------------------------------------------
027600140915     c     Chk3Q         begsr
027700140915
027800140915      /free
027900140915       select;
028000140915         // se invio antipato dei dati su bollettazione
028100140915         // ma non per dati su spunta
028200140915         when §3QADB = 'S' and
028300140915              §3QADS = *blank;
028400140915           // il rcd è OK a prescindere
028500140915           *in55 = *on;
028600140915           leavesr;
028700140915         // se invio antipato dei dati su bollettazione per spunta
028800140915         when §3QADS = 'S';
028900140915           // controllo se spedizione è spuntata
029000140915           clear fnlvemds;
029100140915           clear fnblpds;
029200140915           iemAAS = blpAAS;
029300140915           iemLNP = blpLNP;
029400140915           iemNRS = blpNRS;
029500140915           iemNSP = blpNSP;
029600140915           kpjbu = fnlvemds;
029700140915           callP(e) FNLVEMR (kpjba : fnblpds);
029800140915           fnlvemds = kpjbu;
029900160629           // testo se regolarmente spuntata
030000140915           if %error OR oemESITO <> '1';
030100160629             // se NON regolarmente spuntata, controllo se ha almeno una spunta
030200160629             // in partenza
030300160629             exsr CercoSpunta;
030400160629             // se c'è almeno una spunta (basta che il primo elemento della
030500160629             // schiera della data spunta sia valorizzato), rcd è OK
030600160629             if UBCOLSkDFS(1) <> 0;
030700160629               *in55 = *on;
030800160629               leavesr;
030900160629             endif;
031000140915           else;
031100140915             *in55 = *on;
031200140915             leavesr;
031300140915           endif;
031400140915       endsl;
031500140915
031600140915      /end-free
031700140915
031800140915     c*
031900140915     c                   endsr
032000140915?    C*-------------------------------------------------------------
032100160629?    C* cerco la spunta
032200140915?    C*-------------------------------------------------------------
032300160629     c     CercoSpunta   begsr
032400160629     C*
032500160629     c                   eval      UBCOLAAS =    blpAAS
032600160629     c                   eval      UBCOLLNP =    blpLNP
032700160629     c                   eval      UBCOLNRS =    blpNRS
032800160629     c                   eval      UBCOLNSP =    blpNSP
032900160629     C*
033000160629     C                   CALL      'UBCOL00R'
033100160705     c                   parm      'FV5'         UBCOLOpz
033200160629     c                   parm      *blank        UBCOLFlgOpe
033300160629     c                   parm      *blank        UBCOLTLa
033400160629     c                   parm                    UBCOLAAS
033500160629     c                   parm                    UBCOLLNP
033600160629     c                   parm                    UBCOLNRS
033700160629     c                   parm                    UBCOLNSP
033800160629     c                   parm                    UBCOLNCL
033900160629     c                   parm                    UBCOLTIP
034000160629     c                   parm                    UBCOLDCM
034100160629     C                   PARM                    UBCOLHMC
034200160629     C                   PARM                    UBCOLSkBRT
034300160629     C                   PARM                    UBCOLSkCLI
034400160629     c                   parm                    UBCOLSkDCM
034500160629     c                   parm                    UBCOLSkHMC
034600160629     c                   parm                    UBCOLSkDFS
034700160629     c                   parm                    UBCOLSkHMS
034800160629     c                   parm                    UBCOLSkVUC
034900160629     c                   parm                    UBCOLSkPUC
035000160629     c                   parm                    UBCOLErr
035100160629     C*
035200160629     c                   endsr
035300160629?    C*-------------------------------------------------------------
035400160629?    C* datiagg- dati aggiuntivi da ar4 e AR9
035500160629?    C*-------------------------------------------------------------
035600160629     c     datiagg       begsr
035700160629      *
035800000518      *seconda ragione sociale cliente
035900000518     c                   move      'D'           ktrc
036000060519     c     kar4          chain(N)  fiar401l                           97
036100060214     c  n97              movel     ar4not        vabrd2
036200000605      *campo note bolla
036300000605     c                   move      '8'           ktrc
036400060519     c     kar4          chain(N)  fiar401l                           97
036500060214     c  n97              movel     ar4not        vabnot
036600000518      *campo note bolla 2
036700000518     c                   move      '9'           ktrc
036800060519     c     kar4          chain(N)  fiar401l                           97
036900060214     c  n97              movel     ar4not        vabnt2
037000000518      *dati relativi al contrassegno
037100051114     c     kAR9          chain     fIAR901l                           97
037200090226     c                   if        *in97='0'
037300051114     c                   z-add     AR9cas        vabcas
037400051114     c                   movel     AR9tic        vabtic
037500051114     c                   movel     AR9gca        vabgca
037600051114     c                   movel     AR9vca        vabvca
037700011005     c                   endif
037800021223      * Dati bancali
037900021223     c                   Clear                   VabAnt
038000030130     c                   If        %Subst(BlpGva:1:1) = 'B'
038100021223     c     KFiar5        Chain     Fiar501l
038200021223     c                   If        %Found(Fiar501l)
038300021223     c                   Movel     Ar5Uni        Dar5Ban
038400021223     c                   Movel     §Ar5Tba       VabTba
038500021223     c                   Movel     §Ar5Nba       VabNba
038600021223     c                   Movel     §Ar5Tb2       VabTb2
038700021223     c                   Movel     §Ar5Nb2       VabNb2
038800021223     c                   EndIf
038900021223     c                   EndIf
039000021223
039100000518     c                   endsr
039200000518?    C*-------------------------------------------------
039300000518?    C* scrivi - scrive   file work qtemp
039400000518?    C*-------------------------------------------------
039500000518     c     scrivi        begsr
039600000605     c                   move      *blank        vabscl
039700070222     c                   clear                   vabatb
039800000518     c                   movel     blpcbo        vabcbo
039900000518     c                   movel     blprsd        vabrsd
040000000518     c                   movel     blpind        vabind
040100000518     c                   movel     blpcad        vabcad
040200010927     c                   movel     blplod        vablod
040300000518     c                   movel     blpprd        vabprd
040400000518     c                   movel     blpnzd        vabnzd
040500000518     c                   movel     blpgc1        vabgc1
040600000518     c                   movel     blpgc2        vabgc2
040700000518     c                   movel     blptsp        vabtsp
040800000518     c                   movel     blpnas        vabnas
040900000518     c                   movel     blprma        vabrma
041000000518     c                   movel     blpctm        vabctm
041100000518     c                   movel     blpffd        vabffd
041200000518     c                   movel     blptcr        vabtcr
041300000518     c                   movel     blpcts        vabcts
041400000518     c                   movel     blpftm        vabftm
041500000518     c                   movel     blpvad        vabvad
041600000518     c                   movel     blpgma        vabgma
041700000518     c                   movel     blpgga        vabgga
041800000518     c                   movel     blpgva        vabgva
041900010111     c                   move      ' '           vabgva
042000000518     c                   movel     blptc1        vabtc1
042100000518     c                   movel     blptc2        vabtc2
042200000518     c                   movel     *blank        vabcmo
042300000518     c                   movel     *blank        vabnmo
042400000703     c                   move      '.'           vabnmo
042500060214      * se codice cliente contenuto nel campo note di FIAR4
042600011002     c                   if        *in66
042700120524     c****               move      ar4ksc        vabccm
042800120524     c                   eval      vabccm=%dec(%subst(t_ar4n30:1:7):7:0)
042900011002     c                   else
043000011002      * se codice cliente in FNBLP nel campo blpccm
043100000628     c                   if        blpccm > 0
043200000628     c                   z-add     blpccm        vabccm
043300000628     c                   else
043400011002      * se codice cliente in FNBLP nel campo blpksc
043500000628     c                   z-add     blpksc        vabccm
043600000628     c                   end
043700011002     c                   end
043800000518     c                   z-add     blplnp        vablnp
043900000518     c                   z-add     blpaas        vabaas
044000000518     c                   z-add     blpmgs        vabmgs
044100000518     c                   z-add     blpnrs        vabnrs
044200000518     c                   z-add     blpnsp        vabnsp
044300000518     c                   z-add     blplna        vablna
044400000518     c                   z-add     blpctr        vabctr
044500000518     c                   z-add     blpncl        vabncl
044600000703     c                   z-add     blppkf        vabpkb
044700000703     c                   z-add     blpvlf        vabvlb
044800000518     c                   z-add     blpqft        vabqft
044900000518     c                   z-add     blprmn        vabrmn
045000000518     c                   z-add     blpncd        vabncd
045100000518     c                   z-add     blpnca        vabnca
045200000518     c                   z-add     blpznc        vabznc
045300040916     c                   z-add     blpdsew       vabdcr
045400000621     c                   z-add     0             vabhcr
045500000518     c                   z-add     blpvmd        vabvmd
045600021223     c**!!!              z-add     0             vabant
045700001106      *
045800001106     c                   movel     blprmo        vabrmo
045900001106     c                   IF        VABRMO <> *blanks
046000001106     c                   movel     blpcam        vabcmo
046100001106     c                   movel     blpnzm        vabnmo
046200001106     c                   ELSE
046300001106     c                   clear                   vabcmo
046400001106     c                   clear                   vabnmo
046500001106     c                   ENDIF
046600001106     c                   move      '.'           vabnmo
046700090216     c                   z-add     blpias        vabias
046800090216     c                   movel     blpvas        vabvas
046900090220     C                   movel     blpXCO        vabXCO
047000001106      *
047100090316     C                   clear                   tivgd000
047200090316     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
047300090316     C                   eval      vgdTIP = 'BT'
047400090316     C                   movel     *all'0'       vgdKSU
047500090316     C                   move      §3qcks        vgdKSU
047600090316     C                   eval      vgdTSC = 'WW'
047700090316     C                   eval      vgdDAT = woggig
047800090316     C                   eval      vgdPGM = 'FNLSH0R'
047900090316     C*
048000090316     C                   write     tivgd000
048100001106      *
048200000518     c                   endsr
048300000518?    C*-------------------------------------------------------------
048400060214?    C* aggbl4 - aggiornamento tipo record W ar4 data presa in carico
048500000518?    C*-------------------------------------------------------------
048600000518     c     aggbl4        begsr
048700000518      *
048800000519      *aggiorna record con data presa in carico spedizione
048900000518     c                   move      'W'           ktrc
049000060214     c     kar4          chain     fiar401l                           97
049100000518     c                   if        not *in97
049200000518     c                   movel     woggig        §b4dat
049300060214     c                   movel     dsbl4w        ar4not
049400060214     c                   z-add     woggig        ar4duv
049500060214     c                   update    fiar4000
049600000518     c                   endif
049700000518     c                   endsr
049800010927?    C*-------------------------------------------------------------
049900060214?    C* aggbl4Z- aggiornamento tipo record Z ar4 cod.cli/datainvio
050000010927?    C*-------------------------------------------------------------
050100010927     c     aggbl4Z       begsr
050200010927      *
050300010927      *aggiorna record con data elaborazione
050400010927     c                   move      'Z'           ktrc
050500060214     c     kar4          chain     fiar401l                           97
050600010927     c                   if        not *in97
050700010927     c                   movel     woggig        §b4dtr
050800011010     c                   movel     kscj          §b4okc
050900060214     c                   movel     dsbl4z        ar4not
051000060214     c                   z-add     woggig        ar4duv
051100060214     c                   update    fiar4000
051200010927     c                   endif
051300010927     c                   endsr
051400040916?    C*-------------------------------------------------------------
051500060214?    C* aggbl4gg15 - aggiornamento tipo record W ar4 data ritiro + 15 giorni
051600040916?    C*-------------------------------------------------------------
051700040916     c     aggbl4gg15    begsr
051800040916      *
051900040920     c                   if        blpdbr < woggig15
052000040916      *aggiorna record con data presa in carico spedizione
052100040916     c                   move      'W'           ktrc
052200060214     c     kar4          chain     fiar401l                           97
052300040916     c                   if        not *in97
052400040916     c                   movel     20010101      §b4dat
052500060214     c                   movel     dsbl4w        ar4not
052600060214     c                   z-add     woggig        ar4duv
052700060214     c                   update    fiar4000
052800040920     c                   seton                                        56
052900040916     c                   endif
053000040916     c                   endif
053100040916     c                   endsr
053200040916?    C*-------------------------------------------------------------
053300040916?    C* aggbl4Zgg15- aggiornamento tipo record Z bl4 cod.cli/datainvio
053400040916?    C*-------------------------------------------------------------
053500040916     c     aggbl4Zgg15   begsr
053600040916      *
053700040920     c                   if        blpdbr < woggig15
053800040916      *aggiorna record con data elaborazione
053900040916     c                   move      'Z'           ktrc
054000060214     c     kar4          chain     fiar401l                           97
054100040916     c                   if        not *in97
054200040916     c                   movel     20010101      §b4dtr
054300040916     c                   movel     kscj          §b4okc
054400060214     c                   movel     dsbl4z        ar4not
054500060214     c                   z-add     woggig        ar4duv
054600040920     c                   seton                                        56
054700060214     c                   update    fiar4000
054800040916     c                   endif
054900040916     c                   endif
055000040916     c                   endsr
055100000517?    C*-------------------------------------------------
055200000517?    C* *INZSR - Settaggio iniziale programma STANDARD
055300000517?    C*-------------------------------------------------
055400000517     C     *inzsr        begsr
055500000517?    C*
055600000517     C     *entry        plist
055700000517     C                   parm                    kpjba
055800000517     c     ktab          klist
055900000517     c                   kfld                    codut
056000000517     c                   kfld                    cod
056100000517     c                   kfld                    key
056200000517      *
056300000517     c     ktab1         klist
056400000517     c                   kfld                    codut
056500000517     c                   kfld                    cod
056600100330      *
056700000517     c                   move      '3Q'          cod
056800120511     c*
056900120511     c     k63j          klist
057000120511     c                   kfld                    vuoto
057100120511     c                   kfld                    codice
057200120524     c*
057300120524     c     kar43         klist
057400120524     c                   kfld                    ktrc
057500120524     c                   kfld                    kn30
057600120524     c     kblp          klist
057700120524     c                   kfld                    t_ar4aas
057800120524     c                   kfld                    t_ar4lnp
057900120524     c                   kfld                    t_ar4nrs
058000120524     c                   kfld                    t_ar4nsp
058100100330      *
058200051114     c     kAR9          klist
058300000518     c                   kfld                    blpaas
058400000518     c                   kfld                    blplnp
058500000518     c                   kfld                    blpnrs
058600000518     c                   kfld                    blpnsp
058700060214     c     kar4          klist
058800000518     c                   kfld                    blpaas
058900000518     c                   kfld                    blplnp
059000000518     c                   kfld                    blpnrs
059100000518     c                   kfld                    blpnsp
059200000518     c                   kfld                    ktrc
059300021223     c     KFiar5        Klist
059400021223     c                   Kfld                    BlpAas
059500021223     c                   Kfld                    BlpLnp
059600021223     c                   Kfld                    BlpNrs
059700021223     c                   Kfld                    BlpNsp
059800021223     c                   Kfld                    KAr5Trd
059900040825     c     Karb1_c       Klist
060000040825     c                   Kfld                    BlpAas
060100040825     c                   Kfld                    BlpLnp
060200040825     c                   Kfld                    BlpNrs
060300040825     c                   Kfld                    BlpNsp
060400000518      *
060500000518     c                   time                    dataora
060600000518     c                   move      dataora       woggi
060700000518     c                   move      woggi         dataeur
060800000518     c                   move      dataeur       dataiso
060900000518     c                   move      dataiso       woggig
061000040916     c     dataiso       subdur    15:*d         dataiso
061100040916     c                   move      dataiso       woggig15          8 0
061200011005     C*---------------------------------------------------------------*
061300000517     c                   endsr
