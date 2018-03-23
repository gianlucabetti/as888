000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200170308     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300120511      *clienti a cui inviare da bollettazione
000400120511     ffnblp63j  if   e           k disk
000500120524     ffiar403l  if   e           k disk    rename(fiar4000:fiar403)
000600120524     f                                     prefix(t_)
000700120524     ffnblp01l  if   e           k disk    rename(fnblp000:fnblp01)
000800170309     fTIVGD00F  O    E             DISK    commit
000900051114     ffIAR901l  if   e           k disk
001000060214     ffiar401l  uf   e           k disk    commit
001100021223     fFiar501l  if   e           k Disk
001200011005     fazorg01l  if   e           k disk
001300040825     ffnarb01l  if   e           k disk
001400000517      *--------------------------------------------------------------------
001500170308     D psds           sds
001600170308     D  procname         *PROC
001700000522     D kpjba         e ds
001800000517     D ds3q          e ds
001900170301     D tabel00f      e ds
002000000518     D dsbl4w        e ds
002100010927     D dsbl4z        e ds
002200021223     d dar5ban       e ds
002300090311     D FNVABDS       E DS                  EXTNAME(FNVAB00T)
002400090316     d  VabTba                        2s 0 overlay(vabant:1)
002500090316     d  VabNba                        2s 0 overlay(vabant:3)
002600090316     d  VabTb2                        2s 0 overlay(vabant:5)
002700090316     d  VabNb2                        2s 0 overlay(vabant:7)
002800170315     D TIS7VASDS     E DS                  inz
002900140915     d fnlvemds      e ds
003000000517     d  codut          s              1  0 inz(1)
003100000517     d  cod            s                   like(tblcod)
003200000517     d  key            s                   like(tblkey)
003300010927     d  kscj           s              7
003400100330     d  codice         s                   like(blpksc)
003500060214     d  ktrc           s                   like(ar4trc)
003600021223     d  KAr5Trd        s                   Like(Ar5Trd) Inz('BAN')
003700000518     d  woggi          s              8  0
003800000518     d  woggig         s              8  0
003900000518     d  dataora        s             14  0
004000000518     d  dataeur        s               d   datfmt(*eur)
004100000518     d  dataiso        s               d   datfmt(*iso)
004200000519     d  como7          s              7
004300120511     d  vuoto          s                   like(codice)
004400120524     d  kn30           s                   like(t_ar4n30)
004500120524     d  kn15           s             15
004600170301     d wSQl            s           1024    inz  varying
004700170309     d s3QCKS          s                   like(§3QCKS) inz
004800170320     d s3QCSD          s                   like(§3QCSD) inz
004900170315     d s§7VASPRG       s                   like(o§7VASPRG) inz
005000140915
005100140915     d* PROTOTIPI
005200140915     d fnblpds       e ds                  extname('FNBLP00F')
005300140915     d                                     prefix(p_)
005400140915     d fnlvemr         PR
005500140915     d                                     extpgm('FNLVEMR')
005600140915     d  kpjba                              likeds(kpjba)
005700140915     d  fnblpds                            likeds(fnblpds)
005800160629
005900160629     d/copy gaitrasrc/srcProtoPi,UBCOL00R
006000021223
006100000517     d
006200000517      *--------------------------------------------------------------------
006300170308     C*
006400170308     C* Avvio il monitoring del processo
006500170308     C                   monitor
006600170309     c
006700170301     c                   exsr      setll_sql
006800170301     c  N99              do        *hival
006900170301     c                   exsr      fetch_sql
007000000517     C   99              leave
007100000529     c                   if        tblcod <> '3Q'
007200000529     c                   leave
007300170309     c                   endif
007400000522     c                   if        tblflg <> *blank
007500000522     c                   iter
007600170309     c                   endif
007700000517     c                   movel     tbluni        ds3q
007800100330     c                   movel     tblkey        kscj
007900100330     c                   movel     tblkey        codice
008000120524     c                   move      'Z'           ktrc
008100120524     c                   eval      kn30=kscj+'00000000'
008200120524     c                   eval      kn15=kscj+'00000000'
008300170309     c*
008400170309     c* Quindi estragggo i dati
008500120524     c     codice        chain     fnblp63j                           98
008600120524     c   98k63j          chain     fnblp63j                           98
008700120524     c   98kar43         setll     fiar403l
008800120524     c   98'Z'           reade     fiar403l
008900000914     c
009000120524     c                   if        %found(fnblp63j)
009100120524     c                             or (not %eof(fiar403l) and
009200120524     c                                 %subst(t_AR4N30:1:15)=kn15)
009300170309     c*
009400170309     c* Elabora i dati cliente unificante corrente
009500000518     c                   exsr      estrai
009600000518     c                   endif
009700170309     c*
009800170309     c* Proseguo ciclo
009900000517     c                   enddo
010000170308     c*
010100170301     c                   exsr      close_sql
010200170309     c*
010300170309     C* Finalizzo la transazione in sospeso
010400170309     c                   exsr      RLSPRG
010500170308     C*
010600170308     C* Sancisco il commit
010700170308     C                   commit(e)
010800170308     C*
010900170308     C* Su errore
011000170308     C                   on-error
011100170308     C                   exsr      exeerr
011200170308     C*
011300170308     C* Fine monitoring
011400170308     C                   endmon
011500170308     C*
011600990504     c                   seton                                        lr
011700170309
011800170309
011900170309
012000000518?    C*-------------------------------------------------------------
012100000518?    C* estrai - estrae x file in qtemp x successiva scrittura membri
012200000518?    C*-------------------------------------------------------------
012300000518     c     estrai        begsr
012400010927      *loop di lettura per codice cliente mittente in KSC
012500000518      *
012600000518     c                   setoff                                       98
012700120511     c     k63j          setll     fnblp63j
012800000518     c                   do        *hival
012900120511     c     k63j          reade     fnblp63j                               98
013000000518     c   98              leave
013100090316     c                   clear                   fnvabds
013200040916     c                   z-add     blpdse        blpdsew           8 0
013300040920      * prima di verificare il record da elaborare verifico che non sia
013400040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
013500040920      * se è così viene flaggato il BL4 per non trattare il record
013600040920      *
013700040920     c                   setoff                                       56
013800040920     c                   if        blpdbr > 0
013900040920     c                   exsr      aggbl4gg15
014000040920      * *in56 = on bypass il resto del loop
014100040920     c   56              iter
014200040920     c                   endif
014300000522      *aggiorno e spedisco solo le righe con data minore uguale a oggi
014400000623     c                   if        blpdse > woggig or blpft1 = *blank
014500000705     c                             or blpdse = 0
014600040825     c*
014700040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
014800040825     c*partenza a *zeros
014900040825     c                   exsr      chkdse
015000040825     c*
015100140915     c  N55              exsr      chk3Q
015200140915     c*
015300140915     c  N55              iter
015400000522     c                   end
015500000519     c                   exsr      datiagg
015600000518     c                   exsr      scrivi
015700000519     c                   exsr      aggbl4
015800000518     c                   enddo
015900100330      *loop di lettura con codice cliente in CCM
016000000518     c                   setoff                                       98
016100120511     c     codice        setll     fnblp63j
016200000518     c                   do        *hival
016300120511     c     codice        reade     fnblp63j                               98
016400000518     c   98              leave
016500090316     c                   clear                   fnvabds
016600040916     c                   z-add     blpdse        blpdsew
016700040920      * prima di verificare il record da elaborare verifico che non sia
016800040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
016900040920      * se è così viene flaggato il BL4 per non trattare il record
017000040920      *
017100040920     c                   setoff                                       56
017200040920     c                   if        blpdbr > 0
017300040920     c                   exsr      aggbl4gg15
017400040920      * *in56 = on bypass il resto del loop
017500040920     c   56              iter
017600040920     c                   endif
017700000623     c                   if        blpdse > woggig or blpft1 = *blank
017800000705     c                             or blpdse = 0
017900040825     c*
018000040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
018100040825     c*partenza a *zeros
018200040825     c                   exsr      chkdse
018300140915     c*
018400140915     c  N55              exsr      chk3Q
018500040825     c*
018600040825     c  N55              iter
018700000522     c                   end
018800000518     c                   exsr      datiagg
018900000518     c                   exsr      scrivi
019000000518     c                   exsr      aggbl4
019100000518     c                   enddo
019200060214      *loop di lettura con codice cliente in AR4NOT (altra V.L.)
019300000518     c
019400120524     c                   move      'Z'           ktrc
019500010927     c                   setoff                                       98
019600120524     c     kar43         setll     fiar403l
019700011002     c                   seton                                        66
019800010927     c                   do        *hival
019900120524     c     'Z'           reade     fiar403l
020000120524     c                   if        %eof(fiar403l) or
020100120524     c                             %subst(t_ar4n30:1:15)>kn15
020200120524     c                   leave
020300120524     c                   endif
020400120524     c     kblp          chain     fnblp01l
020500120524     c                   if        %found(fnblp01l)
020600090316     c                   clear                   fnvabds
020700040916     c                   z-add     blpdse        blpdsew
020800040920      * prima di verificare il record da elaborare verifico che non sia
020900040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
021000040920      * se è così viene flaggato il BL4 per non trattare il record
021100040920      *
021200040920     c                   setoff                                       56
021300040920     c                   if        blpdbr > 0
021400040920     c                   exsr      aggbl4zgg15
021500040920      * *in56 = on bypass il resto del loop
021600040920     c   56              iter
021700040920     c                   endif
021800010927     c                   if        blpdse > woggig or blpft1 = *blank
021900010927     c                             or blpdse = 0
022000040825     c*
022100040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
022200040825     c*partenza a *zeros
022300040825     c                   exsr      chkdse
022400140915     c*
022500140915     c  N55              exsr      chk3Q
022600040825     c*
022700040825     c  N55              iter
022800010927     c                   end
022900010927     c                   exsr      datiagg
023000010927     c                   exsr      scrivi
023100010927     c                   exsr      aggbl4Z
023200120524     c                   endif
023300010927     c                   enddo
023400011002     c                   setoff                                       66
023500170315     c*
023600000518     c                   endsr
023700040825?    C*-------------------------------------------------------------
023800040825?    C* chkdse - verifiche aggiuntive in caso d data spunta entrata in part. = 0
023900040825?    C*-------------------------------------------------------------
024000040825     c     chkdse        begsr
024100040825     c*
024200040825     c*se in partenza data spunta entrata è *zeros chaino le bolle in arrivo
024300040825     c*x verificare se collo è stato poi cmq "spuntato" in arrivo
024400040825     c                   setoff                                       55
024500040825     c                   if        blpdse = 0
024600040825     c     karb1_c       chain     fnarb01l
024700040825     c                   if        %found(fnarb01l)
024800040916     c                   if        arbdam > 0 AND  arbcca <> '5' and
024900040825     c*paletto la condizione a partire dal 20040823 xchè è il giorno in cui
025000040825     c*mi sono accorto del problema (l'ha detto Luciano d fare così!)
025100040916     c                             blpdim > 20040823   OR
025200040916     c                             arbdam > 0 AND  arbcca = '5' and
025300040916     c                             arbdam <> arbdcm and blpdim > 20040823
025400040825     c                   seton                                        55
025500040916     c                   z-add     blpdrt        blpdsew
025600040825     c                   endif
025700040825     c                   endif
025800040825     c                   endif
025900040825     c*
026000040825     c                   endsr
026100000518?    C*-------------------------------------------------------------
026200140915?    C* controllo se si può proseguire nell'estrazione perché su
026300140915?    C* tab. 3Q è richiesto un invio anticipato
026400000518?    C*-------------------------------------------------------------
026500140915     c     Chk3Q         begsr
026600140915
026700140915      /free
026800140915       select;
026900140915         // se invio antipato dei dati su bollettazione
027000140915         // ma non per dati su spunta
027100140915         when §3QADB = 'S' and
027200140915              §3QADS = *blank;
027300140915           // il rcd è OK a prescindere
027400140915           *in55 = *on;
027500140915           leavesr;
027600140915         // se invio antipato dei dati su bollettazione per spunta
027700140915         when §3QADS = 'S';
027800140915           // controllo se spedizione è spuntata
027900140915           clear fnlvemds;
028000140915           clear fnblpds;
028100140915           iemAAS = blpAAS;
028200140915           iemLNP = blpLNP;
028300140915           iemNRS = blpNRS;
028400140915           iemNSP = blpNSP;
028500140915           kpjbu = fnlvemds;
028600140915           callP(e) FNLVEMR (kpjba : fnblpds);
028700140915           fnlvemds = kpjbu;
028800160629           // testo se regolarmente spuntata
028900140915           if %error OR oemESITO <> '1';
029000160629             // se NON regolarmente spuntata, controllo se ha almeno una spunta
029100160629             // in partenza
029200160629             exsr CercoSpunta;
029300160629             // se c'è almeno una spunta (basta che il primo elemento della
029400160629             // schiera della data spunta sia valorizzato), rcd è OK
029500160629             if UBCOLSkDFS(1) <> 0;
029600160629               *in55 = *on;
029700160629               leavesr;
029800160629             endif;
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
031000160629?    C* cerco la spunta
031100140915?    C*-------------------------------------------------------------
031200160629     c     CercoSpunta   begsr
031300160629     C*
031400160629     c                   eval      UBCOLAAS =    blpAAS
031500160629     c                   eval      UBCOLLNP =    blpLNP
031600160629     c                   eval      UBCOLNRS =    blpNRS
031700160629     c                   eval      UBCOLNSP =    blpNSP
031800160629     C*
031900160629     C                   CALL      'UBCOL00R'
032000160705     c                   parm      'FV5'         UBCOLOpz
032100160629     c                   parm      *blank        UBCOLFlgOpe
032200160629     c                   parm      *blank        UBCOLTLa
032300160629     c                   parm                    UBCOLAAS
032400160629     c                   parm                    UBCOLLNP
032500160629     c                   parm                    UBCOLNRS
032600160629     c                   parm                    UBCOLNSP
032700160629     c                   parm                    UBCOLNCL
032800160629     c                   parm                    UBCOLTIP
032900160629     c                   parm                    UBCOLDCM
033000160629     C                   PARM                    UBCOLHMC
033100160629     C                   PARM                    UBCOLSkBRT
033200160629     C                   PARM                    UBCOLSkCLI
033300160629     c                   parm                    UBCOLSkDCM
033400160629     c                   parm                    UBCOLSkHMC
033500160629     c                   parm                    UBCOLSkDFS
033600160629     c                   parm                    UBCOLSkHMS
033700160629     c                   parm                    UBCOLSkVUC
033800160629     c                   parm                    UBCOLSkPUC
033900160629     c                   parm                    UBCOLErr
034000160629     C*
034100160629     c                   endsr
034200160629?    C*-------------------------------------------------------------
034300160629?    C* datiagg- dati aggiuntivi da ar4 e AR9
034400160629?    C*-------------------------------------------------------------
034500160629     c     datiagg       begsr
034600160629      *
034700000518      *seconda ragione sociale cliente
034800000518     c                   move      'D'           ktrc
034900060519     c     kar4          chain(N)  fiar401l                           97
035000060214     c  n97              movel     ar4not        vabrd2
035100000605      *campo note bolla
035200000605     c                   move      '8'           ktrc
035300060519     c     kar4          chain(N)  fiar401l                           97
035400060214     c  n97              movel     ar4not        vabnot
035500000518      *campo note bolla 2
035600000518     c                   move      '9'           ktrc
035700060519     c     kar4          chain(N)  fiar401l                           97
035800060214     c  n97              movel     ar4not        vabnt2
035900000518      *dati relativi al contrassegno
036000051114     c     kAR9          chain     fIAR901l                           97
036100090226     c                   if        *in97='0'
036200051114     c                   z-add     AR9cas        vabcas
036300051114     c                   movel     AR9tic        vabtic
036400051114     c                   movel     AR9gca        vabgca
036500051114     c                   movel     AR9vca        vabvca
036600011005     c                   endif
036700021223      * Dati bancali
036800021223     c                   Clear                   VabAnt
036900030130     c                   If        %Subst(BlpGva:1:1) = 'B'
037000021223     c     KFiar5        Chain     Fiar501l
037100021223     c                   If        %Found(Fiar501l)
037200021223     c                   Movel     Ar5Uni        Dar5Ban
037300021223     c                   Movel     §Ar5Tba       VabTba
037400021223     c                   Movel     §Ar5Nba       VabNba
037500021223     c                   Movel     §Ar5Tb2       VabTb2
037600021223     c                   Movel     §Ar5Nb2       VabNb2
037700021223     c                   EndIf
037800021223     c                   EndIf
037900021223
038000000518     c                   endsr
038100000518?    C*-------------------------------------------------
038200000518?    C* scrivi - scrive   file work qtemp
038300000518?    C*-------------------------------------------------
038400000518     c     scrivi        begsr
038500000605     c                   move      *blank        vabscl
038600070222     c                   clear                   vabatb
038700000518     c                   movel     blpcbo        vabcbo
038800000518     c                   movel     blprsd        vabrsd
038900000518     c                   movel     blpind        vabind
039000000518     c                   movel     blpcad        vabcad
039100010927     c                   movel     blplod        vablod
039200000518     c                   movel     blpprd        vabprd
039300000518     c                   movel     blpnzd        vabnzd
039400000518     c                   movel     blpgc1        vabgc1
039500000518     c                   movel     blpgc2        vabgc2
039600000518     c                   movel     blptsp        vabtsp
039700000518     c                   movel     blpnas        vabnas
039800000518     c                   movel     blprma        vabrma
039900000518     c                   movel     blpctm        vabctm
040000000518     c                   movel     blpffd        vabffd
040100000518     c                   movel     blptcr        vabtcr
040200000518     c                   movel     blpcts        vabcts
040300000518     c                   movel     blpftm        vabftm
040400000518     c                   movel     blpvad        vabvad
040500000518     c                   movel     blpgma        vabgma
040600000518     c                   movel     blpgga        vabgga
040700000518     c                   movel     blpgva        vabgva
040800010111     c                   move      ' '           vabgva
040900000518     c                   movel     blptc1        vabtc1
041000000518     c                   movel     blptc2        vabtc2
041100000518     c                   movel     *blank        vabcmo
041200000518     c                   movel     *blank        vabnmo
041300000703     c                   move      '.'           vabnmo
041400060214      * se codice cliente contenuto nel campo note di FIAR4
041500011002     c                   if        *in66
041600120524     c                   eval      vabccm=%dec(%subst(t_ar4n30:1:7):7:0)
041700011002     c                   else
041800011002      * se codice cliente in FNBLP nel campo blpccm
041900000628     c                   if        blpccm > 0
042000000628     c                   z-add     blpccm        vabccm
042100000628     c                   else
042200011002      * se codice cliente in FNBLP nel campo blpksc
042300000628     c                   z-add     blpksc        vabccm
042400000628     c                   end
042500011002     c                   end
042600000518     c                   z-add     blplnp        vablnp
042700000518     c                   z-add     blpaas        vabaas
042800000518     c                   z-add     blpmgs        vabmgs
042900000518     c                   z-add     blpnrs        vabnrs
043000000518     c                   z-add     blpnsp        vabnsp
043100000518     c                   z-add     blplna        vablna
043200000518     c                   z-add     blpctr        vabctr
043300000518     c                   z-add     blpncl        vabncl
043400000703     c                   z-add     blppkf        vabpkb
043500000703     c                   z-add     blpvlf        vabvlb
043600000518     c                   z-add     blpqft        vabqft
043700000518     c                   z-add     blprmn        vabrmn
043800000518     c                   z-add     blpncd        vabncd
043900000518     c                   z-add     blpnca        vabnca
044000000518     c                   z-add     blpznc        vabznc
044100040916     c                   z-add     blpdsew       vabdcr
044200000621     c                   z-add     0             vabhcr
044300000518     c                   z-add     blpvmd        vabvmd
044400021223     c**!!!              z-add     0             vabant
044500001106      *
044600001106     c                   movel     blprmo        vabrmo
044700001106     c                   IF        VABRMO <> *blanks
044800001106     c                   movel     blpcam        vabcmo
044900001106     c                   movel     blpnzm        vabnmo
045000001106     c                   ELSE
045100001106     c                   clear                   vabcmo
045200001106     c                   clear                   vabnmo
045300001106     c                   ENDIF
045400001106     c                   move      '.'           vabnmo
045500090216     c                   z-add     blpias        vabias
045600090216     c                   movel     blpvas        vabvas
045700090220     C                   movel     blpXCO        vabXCO
045800170315     C*
045900170315     C* Verifico rottura di codice per cliente unificante
046000170315     C                   if        §3QCKS <> s3QCKS
046100170315     C*
046200170315     C* Stacco progressivo univoco download
046300170315     C                   exsr      RTVPRG
046400170315     C*
046500170315     C* Se non primo giro => Finalizzo transazione precedente
046600170315     C                   if        s§7VASPRG <> *blanks
046700170315     C                   exsr      RLSPRG
046800170315     C                   endif
046900170315     C*
047000170320     C* Salvo l'unificante e tipo scarico dati corrente
047100170315     C                   eval      s3QCKS = §3QCKS
047200170320     C                   eval      s3QCSD = §3QCSD
047300170315     C*
047400170315     C* Salvo il progressivo univoco download corrente
047500170315     C                   eval      s§7VASPRG = o§7VASPRG
047600170315     C                   endif
047700170315     C*
047800170315     C* Valorizzo il buffer di download
047900090316     C                   clear                   tivgd000
048000090316     C                   eval      vgdDTA = %subst(FNVABDS:1:%size(FNVABDS))
048100090316     C                   eval      vgdTIP = 'BT'
048200170309     C                   eval      vgdKSU = '0'+%editc(§3QCKS:'X')
048300170320     C                   eval      vgdTSC = §3QCSD
048400170308     C                   eval      vgdDAT = woggig
048500170308     C                   eval      vgdPRG = o§7VASPRG
048600170308     C                   eval      vgdPGM = procname
048700170308     C                   eval      vgdSTO = '?'
048800090316     C*
048900090316     C                   write     tivgd000
049000170315     C*
049100170315     C                   endsr
049200000518?    C*-------------------------------------------------------------
049300060214?    C* aggbl4 - aggiornamento tipo record W ar4 data presa in carico
049400000518?    C*-------------------------------------------------------------
049500000518     c     aggbl4        begsr
049600000518      *
049700000519      *aggiorna record con data presa in carico spedizione
049800000518     c                   move      'W'           ktrc
049900060214     c     kar4          chain     fiar401l                           97
050000000518     c                   if        not *in97
050100000518     c                   movel     woggig        §b4dat
050200060214     c                   movel     dsbl4w        ar4not
050300060214     c                   z-add     woggig        ar4duv
050400060214     c                   update    fiar4000
050500000518     c                   endif
050600000518     c                   endsr
050700010927?    C*-------------------------------------------------------------
050800060214?    C* aggbl4Z- aggiornamento tipo record Z ar4 cod.cli/datainvio
050900010927?    C*-------------------------------------------------------------
051000010927     c     aggbl4Z       begsr
051100010927      *
051200010927      *aggiorna record con data elaborazione
051300010927     c                   move      'Z'           ktrc
051400060214     c     kar4          chain     fiar401l                           97
051500010927     c                   if        not *in97
051600010927     c                   movel     woggig        §b4dtr
051700011010     c                   movel     kscj          §b4okc
051800060214     c                   movel     dsbl4z        ar4not
051900060214     c                   z-add     woggig        ar4duv
052000060214     c                   update    fiar4000
052100010927     c                   endif
052200010927     c                   endsr
052300040916?    C*-------------------------------------------------------------
052400060214?    C* aggbl4gg15 - aggiornamento tipo record W ar4 data ritiro + 15 giorni
052500040916?    C*-------------------------------------------------------------
052600040916     c     aggbl4gg15    begsr
052700040916      *
052800040920     c                   if        blpdbr < woggig15
052900040916      *aggiorna record con data presa in carico spedizione
053000040916     c                   move      'W'           ktrc
053100060214     c     kar4          chain     fiar401l                           97
053200040916     c                   if        not *in97
053300040916     c                   movel     20010101      §b4dat
053400060214     c                   movel     dsbl4w        ar4not
053500060214     c                   z-add     woggig        ar4duv
053600060214     c                   update    fiar4000
053700040920     c                   seton                                        56
053800040916     c                   endif
053900040916     c                   endif
054000040916     c                   endsr
054100040916?    C*-------------------------------------------------------------
054200040916?    C* aggbl4Zgg15- aggiornamento tipo record Z bl4 cod.cli/datainvio
054300040916?    C*-------------------------------------------------------------
054400040916     c     aggbl4Zgg15   begsr
054500040916      *
054600040920     c                   if        blpdbr < woggig15
054700040916      *aggiorna record con data elaborazione
054800040916     c                   move      'Z'           ktrc
054900060214     c     kar4          chain     fiar401l                           97
055000040916     c                   if        not *in97
055100040916     c                   movel     20010101      §b4dtr
055200040916     c                   movel     kscj          §b4okc
055300060214     c                   movel     dsbl4z        ar4not
055400060214     c                   z-add     woggig        ar4duv
055500040920     c                   seton                                        56
055600060214     c                   update    fiar4000
055700040916     c                   endif
055800040916     c                   endif
055900040916     c                   endsr
056000170301       //--------------------------------------------------------------
056100170301       // setll_sql  - prepara sql su TABEL00F per scorrere tab. 3Q
056200170301       //--------------------------------------------------------------
056300170301       begsr  setll_sql;
056400170301
056500170301         wSQL = 'select * +
056600170301                 from tabel00f +
056700170301                 where +
056800170301                 tblkut = 1 and +
056900170301                 tblcod = ''3Q'' +
057000170301                 order by +
057100170301                 substr(TBLUNI , 13 , 7)';
057200170301
057300170301         // Dichiarazione cursore
057400170301         exec sql   prepare S1   from :wSQL;
057500170301         exec sql   declare C1  asensitive   cursor for S1;
057600170301
057700170301         // Apertura del cursore
057800170301         exec sql   open C1;
057900170301
058000170301         *in99 = *off;
058100170301         if SQLCode < 0;
058200170301           *in99 = *on;
058300170301         endif;
058400170301
058500170301       endsr;
058600170301       //--------------------------------------------------------------
058700170301       // fetch_sql  - legge il rcdset di TABEL00F
058800170301       //--------------------------------------------------------------
058900170301       begsr  fetch_sql;
059000170301
059100170301         exec sql  fetch next  from C1  into :TABEL00F;
059200170301
059300170301         *in99 = *off;
059400170301         if SQLCode = 100 or SQLCode < 0;
059500170301           *in99 = *on;
059600170301         endif;
059700170301
059800170301       endsr;
059900170301       //--------------------------------------------------------------
060000170301       // close_sql  - chiusura cursore
060100170301       //--------------------------------------------------------------
060200170301       begsr  close_sql;
060300170301
060400170301         // chiusura del cursore
060500170301         exec sql  close C1;
060600170301
060700170301       endsr;
060800170308
060900170308
061000170308
061100170308     C*------------------------------------------------------------------------*
061200170308     C* EXEERR - Routine di esecuzione azioni su Errore
061300170308     C*------------------------------------------------------------------------*
061400170308     C     EXEERR        BEGSR
061500170308     C*
061600170308     C                   dump(A)
061700170308     C                   rolbk
061800170308     C                   seton                                        lr
061900170308     C                   return
062000170308     C*
062100170308     C                   ENDSR
062200170308     C*------------------------------------------------------------------------*
062300170309
062400170309
062500170309
062600170309     C*------------------------------------------------------------------------*
062700170309     C* RTVPRG - Routine di reperimento progressivo univoco transazione
062800170309     C*------------------------------------------------------------------------*
062900170309     C     RTVPRG        BEGSR
063000170309     C*
063100170309     c                   CLEAR                   TIS7VASDS
063200170309     c                   EVAL      i§7VASOPZ = 'PRG'
063300170309     c                   CALL(e)   'TIS7VASR1'
063400170309     c                   PARM                    TIS7VASDS
063500170309     c*
063600170309     c                   if        %error OR
063700170309     c                             o§7VASOK = *off OR
063800170309     c                             o§7VASPRG = *blanks
063900170309     c                   exsr      EXEERR
064000170309     c                   endif
064100170309     C*
064200170309     C                   ENDSR
064300170309     C*------------------------------------------------------------------------*
064400170309
064500170309
064600170309
064700170309     C*------------------------------------------------------------------------*
064800170309     C* RLSPRG - Routine di finalizzazione transazione
064900170309     C*------------------------------------------------------------------------*
065000170309     C     RLSPRG        BEGSR
065100170309     C*
065200170309     C                   EVAL      i§7VASOPZ  = 'RLS'
065300170309     C                   EVAL      i§7VASTIP  = 'BT'
065400170309     C                   EVAL      i§7VASKSU  = '0'+%editc(s3QCKS:'X')
065500170320     C                   EVAL      i§7VASTSC  = s3QCSD
065600170309     C                   EVAL      i§7VASSTO  = '?'
065700170309     C                   EVAL      i§7VASSTTO = 'G'
065800170315     C                   EVAL      i§7VASPRG  = s§7VASPRG
065900170309     C                   CALL(e)   'TIS7VASR1'
066000170309     C                   PARM                    TIS7VASDS
066100170309     C*
066200170309     C* Verifico esito chiamata
066300170309     C                   if        %error OR o§7VASOK = *off
066400170309     C                   exsr      EXEERR
066500170309     C                   endif
066600170309     C*
066700170309     C                   ENDSR
066800170309     C*------------------------------------------------------------------------*
066900170308
067000170308
067100170308
067200000517?    C*-------------------------------------------------
067300000517?    C* *INZSR - Settaggio iniziale programma STANDARD
067400000517?    C*-------------------------------------------------
067500000517     C     *inzsr        begsr
067600000517?    C*
067700000517     C     *entry        plist
067800000517     C                   parm                    kpjba
067900000517     c     ktab          klist
068000000517     c                   kfld                    codut
068100000517     c                   kfld                    cod
068200000517     c                   kfld                    key
068300000517      *
068400000517     c     ktab1         klist
068500000517     c                   kfld                    codut
068600000517     c                   kfld                    cod
068700100330      *
068800000517     c                   move      '3Q'          cod
068900120511     c*
069000120511     c     k63j          klist
069100120511     c                   kfld                    vuoto
069200120511     c                   kfld                    codice
069300120524     c*
069400120524     c     kar43         klist
069500120524     c                   kfld                    ktrc
069600120524     c                   kfld                    kn30
069700120524     c     kblp          klist
069800120524     c                   kfld                    t_ar4aas
069900120524     c                   kfld                    t_ar4lnp
070000120524     c                   kfld                    t_ar4nrs
070100120524     c                   kfld                    t_ar4nsp
070200100330      *
070300051114     c     kAR9          klist
070400000518     c                   kfld                    blpaas
070500000518     c                   kfld                    blplnp
070600000518     c                   kfld                    blpnrs
070700000518     c                   kfld                    blpnsp
070800060214     c     kar4          klist
070900000518     c                   kfld                    blpaas
071000000518     c                   kfld                    blplnp
071100000518     c                   kfld                    blpnrs
071200000518     c                   kfld                    blpnsp
071300000518     c                   kfld                    ktrc
071400021223     c     KFiar5        Klist
071500021223     c                   Kfld                    BlpAas
071600021223     c                   Kfld                    BlpLnp
071700021223     c                   Kfld                    BlpNrs
071800021223     c                   Kfld                    BlpNsp
071900021223     c                   Kfld                    KAr5Trd
072000040825     c     Karb1_c       Klist
072100040825     c                   Kfld                    BlpAas
072200040825     c                   Kfld                    BlpLnp
072300040825     c                   Kfld                    BlpNrs
072400040825     c                   Kfld                    BlpNsp
072500000518      *
072600000518     c                   time                    dataora
072700000518     c                   move      dataora       woggi
072800000518     c                   move      woggi         dataeur
072900000518     c                   move      dataeur       dataiso
073000000518     c                   move      dataiso       woggig
073100040916     c     dataiso       subdur    15:*d         dataiso
073200040916     c                   move      dataiso       woggig15          8 0
073300011005     C*---------------------------------------------------------------*
073400000517     c                   endsr
