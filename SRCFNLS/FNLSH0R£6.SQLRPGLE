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
002800170308     D TIS7VASDS     E DS
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
004800140915
004900140915     d* PROTOTIPI
005000140915     d fnblpds       e ds                  extname('FNBLP00F')
005100140915     d                                     prefix(p_)
005200140915     d fnlvemr         PR
005300140915     d                                     extpgm('FNLVEMR')
005400140915     d  kpjba                              likeds(kpjba)
005500140915     d  fnblpds                            likeds(fnblpds)
005600160629
005700160629     d/copy gaitrasrc/srcProtoPi,UBCOL00R
005800021223
005900000517     d
006000000517      *--------------------------------------------------------------------
006100170308     C*
006200170308     C* Avvio il monitoring del processo
006300170308     C                   monitor
006400170309     c
006500170309     c                   setoff                                       31
006600170301     c                   exsr      setll_sql
006700170301     c  N99              do        *hival
006800170301     c                   exsr      fetch_sql
006900000517     C   99              leave
007000000529     c                   if        tblcod <> '3Q'
007100000529     c                   leave
007200170309     c                   endif
007300000522     c                   if        tblflg <> *blank
007400000522     c                   iter
007500170309     c                   endif
007600000517     c                   movel     tbluni        ds3q
007700100330     c                   movel     tblkey        kscj
007800100330     c                   movel     tblkey        codice
007900120524     c                   move      'Z'           ktrc
008000120524     c                   eval      kn30=kscj+'00000000'
008100120524     c                   eval      kn15=kscj+'00000000'
008200170309     c*
008300170309     c* Quindi estragggo i dati
008400120524     c     codice        chain     fnblp63j                           98
008500120524     c   98k63j          chain     fnblp63j                           98
008600120524     c   98kar43         setll     fiar403l
008700120524     c   98'Z'           reade     fiar403l
008800000914     c
008900120524     c                   if        %found(fnblp63j)
009000120524     c                             or (not %eof(fiar403l) and
009100120524     c                                 %subst(t_AR4N30:1:15)=kn15)
009200170309     c*
009300170309     c* Verifico rottura di codice per cliente unificante
009400170309     c                   if        §3QCKS <> s3QCKS
009500170309     c*
009600170309     c* Se NON primo giro => sancisco transazione precedente
009700170309     c                   if        *in31
009800170309     c*
009900170309     c* Finalizzo la transazione
010000170309     c                   exsr      RLSPRG
010100170309     c                   endif
010200170309     c*
010300170309     c* Stacco progressivo univoco download
010400170309     c                   exsr      RTVPRG
010500170309     c                   endif
010600170309     c*
010700170309     c* Elabora i dati cliente unificante corrente
010800000518     c                   exsr      estrai
010900170309     c                   seton                                        31
011000000518     c                   endif
011100170309     c*
011200170309     c* Salvo l'unificante corrente
011300170309     c                   eval      s3QCKS = §3QCKS
011400170309     c*
011500170309     c* Proseguo ciclo
011600000517     c                   enddo
011700170308     c*
011800170301     c                   exsr      close_sql
011900170309     c*
012000170309     C* Finalizzo la transazione in sospeso
012100170309     c                   exsr      RLSPRG
012200170308     C*
012300170308     C* Sancisco il commit
012400170308     C                   commit(e)
012500170308     C*
012600170308     C* Su errore
012700170308     C                   on-error
012800170308     C                   exsr      exeerr
012900170308     C*
013000170308     C* Fine monitoring
013100170308     C                   endmon
013200170308     C*
013300990504     c                   seton                                        lr
013400170309
013500170309
013600170309
013700000518?    C*-------------------------------------------------------------
013800000518?    C* estrai - estrae x file in qtemp x successiva scrittura membri
013900000518?    C*-------------------------------------------------------------
014000000518     c     estrai        begsr
014100010927      *loop di lettura per codice cliente mittente in KSC
014200000518      *
014300000518     c                   setoff                                       98
014400120511     c     k63j          setll     fnblp63j
014500000518     c                   do        *hival
014600120511     c     k63j          reade     fnblp63j                               98
014700000518     c   98              leave
014800090316     c                   clear                   fnvabds
014900040916     c                   z-add     blpdse        blpdsew           8 0
015000040920      * prima di verificare il record da elaborare verifico che non sia
015100040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
015200040920      * se è così viene flaggato il BL4 per non trattare il record
015300040920      *
015400040920     c                   setoff                                       56
015500040920     c                   if        blpdbr > 0
015600040920     c                   exsr      aggbl4gg15
015700040920      * *in56 = on bypass il resto del loop
015800040920     c   56              iter
015900040920     c                   endif
016000000522      *aggiorno e spedisco solo le righe con data minore uguale a oggi
016100000623     c                   if        blpdse > woggig or blpft1 = *blank
016200000705     c                             or blpdse = 0
016300040825     c*
016400040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
016500040825     c*partenza a *zeros
016600040825     c                   exsr      chkdse
016700040825     c*
016800140915     c  N55              exsr      chk3Q
016900140915     c*
017000140915     c  N55              iter
017100000522     c                   end
017200000519     c                   exsr      datiagg
017300000518     c                   exsr      scrivi
017400000519     c                   exsr      aggbl4
017500000518     c                   enddo
017600100330      *loop di lettura con codice cliente in CCM
017700000518     c                   setoff                                       98
017800120511     c     codice        setll     fnblp63j
017900000518     c                   do        *hival
018000120511     c     codice        reade     fnblp63j                               98
018100000518     c   98              leave
018200090316     c                   clear                   fnvabds
018300040916     c                   z-add     blpdse        blpdsew
018400040920      * prima di verificare il record da elaborare verifico che non sia
018500040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
018600040920      * se è così viene flaggato il BL4 per non trattare il record
018700040920      *
018800040920     c                   setoff                                       56
018900040920     c                   if        blpdbr > 0
019000040920     c                   exsr      aggbl4gg15
019100040920      * *in56 = on bypass il resto del loop
019200040920     c   56              iter
019300040920     c                   endif
019400000623     c                   if        blpdse > woggig or blpft1 = *blank
019500000705     c                             or blpdse = 0
019600040825     c*
019700040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
019800040825     c*partenza a *zeros
019900040825     c                   exsr      chkdse
020000140915     c*
020100140915     c  N55              exsr      chk3Q
020200040825     c*
020300040825     c  N55              iter
020400000522     c                   end
020500000518     c                   exsr      datiagg
020600000518     c                   exsr      scrivi
020700000518     c                   exsr      aggbl4
020800000518     c                   enddo
020900060214      *loop di lettura con codice cliente in AR4NOT (altra V.L.)
021000000518     c
021100120524     c                   move      'Z'           ktrc
021200010927     c                   setoff                                       98
021300120524     c     kar43         setll     fiar403l
021400011002     c                   seton                                        66
021500010927     c                   do        *hival
021600120524     c     'Z'           reade     fiar403l
021700120524     c                   if        %eof(fiar403l) or
021800120524     c                             %subst(t_ar4n30:1:15)>kn15
021900120524     c                   leave
022000120524     c                   endif
022100120524     c     kblp          chain     fnblp01l
022200120524     c                   if        %found(fnblp01l)
022300090316     c                   clear                   fnvabds
022400040916     c                   z-add     blpdse        blpdsew
022500040920      * prima di verificare il record da elaborare verifico che non sia
022600040920      * un record vecchio con data borderò < di 15 giorni rispetto ad oggi
022700040920      * se è così viene flaggato il BL4 per non trattare il record
022800040920      *
022900040920     c                   setoff                                       56
023000040920     c                   if        blpdbr > 0
023100040920     c                   exsr      aggbl4zgg15
023200040920      * *in56 = on bypass il resto del loop
023300040920     c   56              iter
023400040920     c                   endif
023500010927     c                   if        blpdse > woggig or blpft1 = *blank
023600010927     c                             or blpdse = 0
023700040825     c*
023800040825     c*effettuo alcune verifiche particolari in caso d data spunta entrata in
023900040825     c*partenza a *zeros
024000040825     c                   exsr      chkdse
024100140915     c*
024200140915     c  N55              exsr      chk3Q
024300040825     c*
024400040825     c  N55              iter
024500010927     c                   end
024600010927     c                   exsr      datiagg
024700010927     c                   exsr      scrivi
024800010927     c                   exsr      aggbl4Z
024900120524     c                   endif
025000010927     c                   enddo
025100011002     c                   setoff                                       66
025200090316
025300090316?    C                   commit
025400010927
025500000518     c                   endsr
025600040825?    C*-------------------------------------------------------------
025700040825?    C* chkdse - verifiche aggiuntive in caso d data spunta entrata in part. = 0
025800040825?    C*-------------------------------------------------------------
025900040825     c     chkdse        begsr
026000040825     c*
026100040825     c*se in partenza data spunta entrata è *zeros chaino le bolle in arrivo
026200040825     c*x verificare se collo è stato poi cmq "spuntato" in arrivo
026300040825     c                   setoff                                       55
026400040825     c                   if        blpdse = 0
026500040825     c     karb1_c       chain     fnarb01l
026600040825     c                   if        %found(fnarb01l)
026700040916     c                   if        arbdam > 0 AND  arbcca <> '5' and
026800040825     c*paletto la condizione a partire dal 20040823 xchè è il giorno in cui
026900040825     c*mi sono accorto del problema (l'ha detto Luciano d fare così!)
027000040916     c                             blpdim > 20040823   OR
027100040916     c                             arbdam > 0 AND  arbcca = '5' and
027200040916     c                             arbdam <> arbdcm and blpdim > 20040823
027300040825     c                   seton                                        55
027400040916     c                   z-add     blpdrt        blpdsew
027500040825     c                   endif
027600040825     c                   endif
027700040825     c                   endif
027800040825     c*
027900040825     c                   endsr
028000000518?    C*-------------------------------------------------------------
028100140915?    C* controllo se si può proseguire nell'estrazione perché su
028200140915?    C* tab. 3Q è richiesto un invio anticipato
028300000518?    C*-------------------------------------------------------------
028400140915     c     Chk3Q         begsr
028500140915
028600140915      /free
028700140915       select;
028800140915         // se invio antipato dei dati su bollettazione
028900140915         // ma non per dati su spunta
029000140915         when §3QADB = 'S' and
029100140915              §3QADS = *blank;
029200140915           // il rcd è OK a prescindere
029300140915           *in55 = *on;
029400140915           leavesr;
029500140915         // se invio antipato dei dati su bollettazione per spunta
029600140915         when §3QADS = 'S';
029700140915           // controllo se spedizione è spuntata
029800140915           clear fnlvemds;
029900140915           clear fnblpds;
030000140915           iemAAS = blpAAS;
030100140915           iemLNP = blpLNP;
030200140915           iemNRS = blpNRS;
030300140915           iemNSP = blpNSP;
030400140915           kpjbu = fnlvemds;
030500140915           callP(e) FNLVEMR (kpjba : fnblpds);
030600140915           fnlvemds = kpjbu;
030700160629           // testo se regolarmente spuntata
030800140915           if %error OR oemESITO <> '1';
030900160629             // se NON regolarmente spuntata, controllo se ha almeno una spunta
031000160629             // in partenza
031100160629             exsr CercoSpunta;
031200160629             // se c'è almeno una spunta (basta che il primo elemento della
031300160629             // schiera della data spunta sia valorizzato), rcd è OK
031400160629             if UBCOLSkDFS(1) <> 0;
031500160629               *in55 = *on;
031600160629               leavesr;
031700160629             endif;
031800140915           else;
031900140915             *in55 = *on;
032000140915             leavesr;
032100140915           endif;
032200140915       endsl;
032300140915
032400140915      /end-free
032500140915
032600140915     c*
032700140915     c                   endsr
032800140915?    C*-------------------------------------------------------------
032900160629?    C* cerco la spunta
033000140915?    C*-------------------------------------------------------------
033100160629     c     CercoSpunta   begsr
033200160629     C*
033300160629     c                   eval      UBCOLAAS =    blpAAS
033400160629     c                   eval      UBCOLLNP =    blpLNP
033500160629     c                   eval      UBCOLNRS =    blpNRS
033600160629     c                   eval      UBCOLNSP =    blpNSP
033700160629     C*
033800160629     C                   CALL      'UBCOL00R'
033900160705     c                   parm      'FV5'         UBCOLOpz
034000160629     c                   parm      *blank        UBCOLFlgOpe
034100160629     c                   parm      *blank        UBCOLTLa
034200160629     c                   parm                    UBCOLAAS
034300160629     c                   parm                    UBCOLLNP
034400160629     c                   parm                    UBCOLNRS
034500160629     c                   parm                    UBCOLNSP
034600160629     c                   parm                    UBCOLNCL
034700160629     c                   parm                    UBCOLTIP
034800160629     c                   parm                    UBCOLDCM
034900160629     C                   PARM                    UBCOLHMC
035000160629     C                   PARM                    UBCOLSkBRT
035100160629     C                   PARM                    UBCOLSkCLI
035200160629     c                   parm                    UBCOLSkDCM
035300160629     c                   parm                    UBCOLSkHMC
035400160629     c                   parm                    UBCOLSkDFS
035500160629     c                   parm                    UBCOLSkHMS
035600160629     c                   parm                    UBCOLSkVUC
035700160629     c                   parm                    UBCOLSkPUC
035800160629     c                   parm                    UBCOLErr
035900160629     C*
036000160629     c                   endsr
036100160629?    C*-------------------------------------------------------------
036200160629?    C* datiagg- dati aggiuntivi da ar4 e AR9
036300160629?    C*-------------------------------------------------------------
036400160629     c     datiagg       begsr
036500160629      *
036600000518      *seconda ragione sociale cliente
036700000518     c                   move      'D'           ktrc
036800060519     c     kar4          chain(N)  fiar401l                           97
036900060214     c  n97              movel     ar4not        vabrd2
037000000605      *campo note bolla
037100000605     c                   move      '8'           ktrc
037200060519     c     kar4          chain(N)  fiar401l                           97
037300060214     c  n97              movel     ar4not        vabnot
037400000518      *campo note bolla 2
037500000518     c                   move      '9'           ktrc
037600060519     c     kar4          chain(N)  fiar401l                           97
037700060214     c  n97              movel     ar4not        vabnt2
037800000518      *dati relativi al contrassegno
037900051114     c     kAR9          chain     fIAR901l                           97
038000090226     c                   if        *in97='0'
038100051114     c                   z-add     AR9cas        vabcas
038200051114     c                   movel     AR9tic        vabtic
038300051114     c                   movel     AR9gca        vabgca
038400051114     c                   movel     AR9vca        vabvca
038500011005     c                   endif
038600021223      * Dati bancali
038700021223     c                   Clear                   VabAnt
038800030130     c                   If        %Subst(BlpGva:1:1) = 'B'
038900021223     c     KFiar5        Chain     Fiar501l
039000021223     c                   If        %Found(Fiar501l)
039100021223     c                   Movel     Ar5Uni        Dar5Ban
039200021223     c                   Movel     §Ar5Tba       VabTba
039300021223     c                   Movel     §Ar5Nba       VabNba
039400021223     c                   Movel     §Ar5Tb2       VabTb2
039500021223     c                   Movel     §Ar5Nb2       VabNb2
039600021223     c                   EndIf
039700021223     c                   EndIf
039800021223
039900000518     c                   endsr
040000000518?    C*-------------------------------------------------
040100000518?    C* scrivi - scrive   file work qtemp
040200000518?    C*-------------------------------------------------
040300000518     c     scrivi        begsr
040400000605     c                   move      *blank        vabscl
040500070222     c                   clear                   vabatb
040600000518     c                   movel     blpcbo        vabcbo
040700000518     c                   movel     blprsd        vabrsd
040800000518     c                   movel     blpind        vabind
040900000518     c                   movel     blpcad        vabcad
041000010927     c                   movel     blplod        vablod
041100000518     c                   movel     blpprd        vabprd
041200000518     c                   movel     blpnzd        vabnzd
041300000518     c                   movel     blpgc1        vabgc1
041400000518     c                   movel     blpgc2        vabgc2
041500000518     c                   movel     blptsp        vabtsp
041600000518     c                   movel     blpnas        vabnas
041700000518     c                   movel     blprma        vabrma
041800000518     c                   movel     blpctm        vabctm
041900000518     c                   movel     blpffd        vabffd
042000000518     c                   movel     blptcr        vabtcr
042100000518     c                   movel     blpcts        vabcts
042200000518     c                   movel     blpftm        vabftm
042300000518     c                   movel     blpvad        vabvad
042400000518     c                   movel     blpgma        vabgma
042500000518     c                   movel     blpgga        vabgga
042600000518     c                   movel     blpgva        vabgva
042700010111     c                   move      ' '           vabgva
042800000518     c                   movel     blptc1        vabtc1
042900000518     c                   movel     blptc2        vabtc2
043000000518     c                   movel     *blank        vabcmo
043100000518     c                   movel     *blank        vabnmo
043200000703     c                   move      '.'           vabnmo
043300060214      * se codice cliente contenuto nel campo note di FIAR4
043400011002     c                   if        *in66
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
048100170309     C                   eval      vgdKSU = '0'+%editc(§3QCKS:'X')
048200090316     C                   eval      vgdTSC = 'WW'
048300170308     C                   eval      vgdDAT = woggig
048400170308     C                   eval      vgdPRG = o§7VASPRG
048500170308     C                   eval      vgdPGM = procname
048600170308     C                   eval      vgdSTO = '?'
048700090316     C*
048800090316     C                   write     tivgd000
048900001106      *
049000000518     c                   endsr
049100000518?    C*-------------------------------------------------------------
049200060214?    C* aggbl4 - aggiornamento tipo record W ar4 data presa in carico
049300000518?    C*-------------------------------------------------------------
049400000518     c     aggbl4        begsr
049500000518      *
049600000519      *aggiorna record con data presa in carico spedizione
049700000518     c                   move      'W'           ktrc
049800060214     c     kar4          chain     fiar401l                           97
049900000518     c                   if        not *in97
050000000518     c                   movel     woggig        §b4dat
050100060214     c                   movel     dsbl4w        ar4not
050200060214     c                   z-add     woggig        ar4duv
050300060214     c                   update    fiar4000
050400000518     c                   endif
050500000518     c                   endsr
050600010927?    C*-------------------------------------------------------------
050700060214?    C* aggbl4Z- aggiornamento tipo record Z ar4 cod.cli/datainvio
050800010927?    C*-------------------------------------------------------------
050900010927     c     aggbl4Z       begsr
051000010927      *
051100010927      *aggiorna record con data elaborazione
051200010927     c                   move      'Z'           ktrc
051300060214     c     kar4          chain     fiar401l                           97
051400010927     c                   if        not *in97
051500010927     c                   movel     woggig        §b4dtr
051600011010     c                   movel     kscj          §b4okc
051700060214     c                   movel     dsbl4z        ar4not
051800060214     c                   z-add     woggig        ar4duv
051900060214     c                   update    fiar4000
052000010927     c                   endif
052100010927     c                   endsr
052200040916?    C*-------------------------------------------------------------
052300060214?    C* aggbl4gg15 - aggiornamento tipo record W ar4 data ritiro + 15 giorni
052400040916?    C*-------------------------------------------------------------
052500040916     c     aggbl4gg15    begsr
052600040916      *
052700040920     c                   if        blpdbr < woggig15
052800040916      *aggiorna record con data presa in carico spedizione
052900040916     c                   move      'W'           ktrc
053000060214     c     kar4          chain     fiar401l                           97
053100040916     c                   if        not *in97
053200040916     c                   movel     20010101      §b4dat
053300060214     c                   movel     dsbl4w        ar4not
053400060214     c                   z-add     woggig        ar4duv
053500060214     c                   update    fiar4000
053600040920     c                   seton                                        56
053700040916     c                   endif
053800040916     c                   endif
053900040916     c                   endsr
054000040916?    C*-------------------------------------------------------------
054100040916?    C* aggbl4Zgg15- aggiornamento tipo record Z bl4 cod.cli/datainvio
054200040916?    C*-------------------------------------------------------------
054300040916     c     aggbl4Zgg15   begsr
054400040916      *
054500040920     c                   if        blpdbr < woggig15
054600040916      *aggiorna record con data elaborazione
054700040916     c                   move      'Z'           ktrc
054800060214     c     kar4          chain     fiar401l                           97
054900040916     c                   if        not *in97
055000040916     c                   movel     20010101      §b4dtr
055100040916     c                   movel     kscj          §b4okc
055200060214     c                   movel     dsbl4z        ar4not
055300060214     c                   z-add     woggig        ar4duv
055400040920     c                   seton                                        56
055500060214     c                   update    fiar4000
055600040916     c                   endif
055700040916     c                   endif
055800040916     c                   endsr
055900170301       //--------------------------------------------------------------
056000170301       // setll_sql  - prepara sql su TABEL00F per scorrere tab. 3Q
056100170301       //--------------------------------------------------------------
056200170301       begsr  setll_sql;
056300170301
056400170301         wSQL = 'select * +
056500170301                 from tabel00f +
056600170301                 where +
056700170301                 tblkut = 1 and +
056800170301                 tblcod = ''3Q'' +
056900170301                 order by +
057000170301                 substr(TBLUNI , 13 , 7)';
057100170301
057200170301         // Dichiarazione cursore
057300170301         exec sql   prepare S1   from :wSQL;
057400170301         exec sql   declare C1  asensitive   cursor for S1;
057500170301
057600170301         // Apertura del cursore
057700170301         exec sql   open C1;
057800170301
057900170301         *in99 = *off;
058000170301         if SQLCode < 0;
058100170301           *in99 = *on;
058200170301         endif;
058300170301
058400170301       endsr;
058500170301       //--------------------------------------------------------------
058600170301       // fetch_sql  - legge il rcdset di TABEL00F
058700170301       //--------------------------------------------------------------
058800170301       begsr  fetch_sql;
058900170301
059000170301         exec sql  fetch next  from C1  into :TABEL00F;
059100170301
059200170301         *in99 = *off;
059300170301         if SQLCode = 100 or SQLCode < 0;
059400170301           *in99 = *on;
059500170301         endif;
059600170301
059700170301       endsr;
059800170301       //--------------------------------------------------------------
059900170301       // close_sql  - chiusura cursore
060000170301       //--------------------------------------------------------------
060100170301       begsr  close_sql;
060200170301
060300170301         // chiusura del cursore
060400170301         exec sql  close C1;
060500170301
060600170301       endsr;
060700170308
060800170308
060900170308
061000170308     C*------------------------------------------------------------------------*
061100170308     C* EXEERR - Routine di esecuzione azioni su Errore
061200170308     C*------------------------------------------------------------------------*
061300170308     C     EXEERR        BEGSR
061400170308     C*
061500170308     C                   dump(A)
061600170308     C                   rolbk
061700170308     C                   seton                                        lr
061800170308     C                   return
061900170308     C*
062000170308     C                   ENDSR
062100170308     C*------------------------------------------------------------------------*
062200170309
062300170309
062400170309
062500170309     C*------------------------------------------------------------------------*
062600170309     C* RTVPRG - Routine di reperimento progressivo univoco transazione
062700170309     C*------------------------------------------------------------------------*
062800170309     C     RTVPRG        BEGSR
062900170309     C*
063000170309     c                   CLEAR                   TIS7VASDS
063100170309     c                   EVAL      i§7VASOPZ = 'PRG'
063200170309     c                   CALL(e)   'TIS7VASR1'
063300170309     c                   PARM                    TIS7VASDS
063400170309     c*
063500170309     c                   if        %error OR
063600170309     c                             o§7VASOK = *off OR
063700170309     c                             o§7VASPRG = *blanks
063800170309     c                   exsr      EXEERR
063900170309     c                   endif
064000170309     C*
064100170309     C                   ENDSR
064200170309     C*------------------------------------------------------------------------*
064300170309
064400170309
064500170309
064600170309     C*------------------------------------------------------------------------*
064700170309     C* RLSPRG - Routine di finalizzazione transazione
064800170309     C*------------------------------------------------------------------------*
064900170309     C     RLSPRG        BEGSR
065000170309     C*
065100170309     C                   EVAL      i§7VASOPZ  = 'RLS'
065200170309     C                   EVAL      i§7VASTIP  = 'BT'
065300170309     C                   EVAL      i§7VASKSU  = '0'+%editc(s3QCKS:'X')
065400170309     C                   EVAL      i§7VASTSC  = 'WW'
065500170309     C                   EVAL      i§7VASSTO  = '?'
065600170309     C                   EVAL      i§7VASSTTO = 'G'
065700170309     C                   EVAL      i§7VASPRG  = o§7VASPRG
065800170309     C                   CALL(e)   'TIS7VASR1'
065900170309     C                   PARM                    TIS7VASDS
066000170309     C*
066100170309     C* Verifico esito chiamata
066200170309     C                   if        %error OR o§7VASOK = *off
066300170309     C                   exsr      EXEERR
066400170309     C                   endif
066500170309     C*
066600170309     C                   ENDSR
066700170309     C*------------------------------------------------------------------------*
066800170308
066900170308
067000170308
067100000517?    C*-------------------------------------------------
067200000517?    C* *INZSR - Settaggio iniziale programma STANDARD
067300000517?    C*-------------------------------------------------
067400000517     C     *inzsr        begsr
067500000517?    C*
067600000517     C     *entry        plist
067700000517     C                   parm                    kpjba
067800000517     c     ktab          klist
067900000517     c                   kfld                    codut
068000000517     c                   kfld                    cod
068100000517     c                   kfld                    key
068200000517      *
068300000517     c     ktab1         klist
068400000517     c                   kfld                    codut
068500000517     c                   kfld                    cod
068600100330      *
068700000517     c                   move      '3Q'          cod
068800120511     c*
068900120511     c     k63j          klist
069000120511     c                   kfld                    vuoto
069100120511     c                   kfld                    codice
069200120524     c*
069300120524     c     kar43         klist
069400120524     c                   kfld                    ktrc
069500120524     c                   kfld                    kn30
069600120524     c     kblp          klist
069700120524     c                   kfld                    t_ar4aas
069800120524     c                   kfld                    t_ar4lnp
069900120524     c                   kfld                    t_ar4nrs
070000120524     c                   kfld                    t_ar4nsp
070100100330      *
070200051114     c     kAR9          klist
070300000518     c                   kfld                    blpaas
070400000518     c                   kfld                    blplnp
070500000518     c                   kfld                    blpnrs
070600000518     c                   kfld                    blpnsp
070700060214     c     kar4          klist
070800000518     c                   kfld                    blpaas
070900000518     c                   kfld                    blplnp
071000000518     c                   kfld                    blpnrs
071100000518     c                   kfld                    blpnsp
071200000518     c                   kfld                    ktrc
071300021223     c     KFiar5        Klist
071400021223     c                   Kfld                    BlpAas
071500021223     c                   Kfld                    BlpLnp
071600021223     c                   Kfld                    BlpNrs
071700021223     c                   Kfld                    BlpNsp
071800021223     c                   Kfld                    KAr5Trd
071900040825     c     Karb1_c       Klist
072000040825     c                   Kfld                    BlpAas
072100040825     c                   Kfld                    BlpLnp
072200040825     c                   Kfld                    BlpNrs
072300040825     c                   Kfld                    BlpNsp
072400000518      *
072500000518     c                   time                    dataora
072600000518     c                   move      dataora       woggi
072700000518     c                   move      woggi         dataeur
072800000518     c                   move      dataeur       dataiso
072900000518     c                   move      dataiso       woggig
073000040916     c     dataiso       subdur    15:*d         dataiso
073100040916     c                   move      dataiso       woggig15          8 0
073200011005     C*---------------------------------------------------------------*
073300000517     c                   endsr
