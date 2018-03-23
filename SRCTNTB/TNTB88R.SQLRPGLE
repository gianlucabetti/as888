000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200110708      * tntb88r *----------------------------------------------------*
000300110708      *         -Tabella "CXQ" clienti particolari per controllo in statistiche
000400110708      *                        ufficio qualità
000500940915      *--------------------------------------------------------------*
000600110708     Ftntb88D   CF   E             WORKSTN
000700110708     f                                     sfile(tb88s03 : nrr1)
000800110809     f                                     sfile(tb88s05 : nrr5)
000900110112     FTNTBE01L  uf a E           k DISK
001000110112     Ftabel00f  if   E           k DISK
001100121123     Fazcae05l  if   E           k DISK
001200110112     Fcnaco00f  if   E           k DISK
001300110112     Fcnclp00f  if   E           k DISK
001400121120     Fazorg01l  if   E           k DISK
001500110812     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600110812     f                                     FORMLEN(066)
001700110812     f                                     FORMOFL(063)
001800020805      *
001900020805      * DEFINIZIONE SCHIERE
002000960302     D*
002100121123     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
002200110812     D SK              S             66    DIM(5) CTDATA PERRCD(1)
002300110803     D ksc             S              7  0 DIM(900)
002400110810     D Unif            S              7  0 DIM(400)
002500110810     D UnifD           S             24    DIM(400)
002600110112     D*
002700110112     D KPJBA         E DS
002800110810     D dCXQ          E DS
002900121127     d* DS x controllo riempimento schiere
003000121127     d trul0sds      e ds
003100121123     d
003200121123     D fnlv55ds      E DS
003300110810     d
003400110708     D tibs10ds      E DS
003500110708     d  skc                   21   5520    dim(500)
003600110112     d TIBS02DS      E DS
003700110708     d xclirds       E DS
003800110708     D xKSC            S              7  0 DIM(30)
003900110708     D xKSA            S              4    DIM(30)                              *ALFANUMERICO
004000110708
004100110708     D                 DS
004200110708     D  DSKSN                  1      4P 0
004300110708     D  DSKSA                  1      4
004400110708     d
004500110112      * - Parametri per richiamo al pgm di controllo profilo utenti
004600110112     d TIBS34ds      e ds
004700110112      * - Ds di riferimento al file esterno AzUte00F
004800110112     d AZUTEds       e ds                  ExtName(AzUte00F)
004900110112      * - Ds per dati organigramma
005000110112     d dDatiUte      e ds
005100080529     d
005200080529     D WLBDAT          DS                  INZ
005300080529     D  G02DAT                 1      8  0
005400080529     D  G02INV                 9     16  0
005500080529     D  G02ERR                17     17
005600080529     D  G02TGI                18     22  0
005700080529     d
005800050103     d Dataiso         s               d   datfmt(*iso)
005900050103     d Dataeur         s               d   datfmt(*eur)
006000080529     d datadmy         s               d   datfmt(*dmy)
006100050103     d
006200080529     d nrr1            s              4  0 inz
006300110809     d nrr5            s              4  0 inz
006400130820     D kcodt           S                   LIKE(tblcod) inz('05')
006500110120     D kkey            S                   LIKE(tblkey)
006600110708     D kcod            S                   LIKE(tbecod) inz('CXQ')
006700110120     D kkey1           S                   LIKE(tbeke1)
006800110112     D kkey2           S                   LIKE(tbeke2)
006900110114     D kkut            S                   LIKE(acokut) inz(1)
007000110112     D kksc            S                   LIKE(acoksc)
007100121120     D savterm         S                   LIKE(v3cterm)
007200121123     D savhterm        S                   LIKE(v3cterm)
007300121120     D savksc          S                   LIKE(v3cksc)
007400110809     D savhksc         S                   LIKE(v3cksc)
007500110127     D savdksc         S                   LIKE(v3dksc)
007600110708     D savuni          S                   LIKE(v3cuni)
007700110708     D savduni         S                   LIKE(v3duni)
007800130819     D savcar          S                   LIKE(v3ccar)
007900130819     D savarea         S                   LIKE(v3carea)
008000110127     D wnrr            S                   LIKE(nrr1)
008100110127     D savcsr          S                   LIKE(c01csr)
008200110114     d c_SflPag        c                   const(17)
008300110112     d wpag            s              6  0
008400110112     d wresto          s              3  0
008500110708     d zz              s              3  0
008600110708     d yy              s              3  0
008700110708     d XX              s              3  0
008800110708     d Indx            s              3  0
008900110112     d $Fine           s              1
009000110810     d Wopz            s              1
009100110809     d Forzavar        s              1
009200110810     d wunif           s              7  0
009300130820     d P3DUNI          s             35
009400130820     d P3Dterm         s             20
009500130820     d P3Dare          s             15
009600041220     d
009700050103     C**********************************************************************
009800110112     c
009900110112     c                   exsr      Carsfl
010000110112     c                   exsr      Gessfl
010100080529     c
010200960129     C*
010300000000     C                   SETON                                        LR
010400080529      *-----------------------------------------------------***********
010500110112      * Caricamento sfl
010600080529      *-----------------------------------------------------***********
010700110112     C     CarSFL        BEGSR
010800080529     C* PULIZIA SFL
010900080529     C                   SETON                                        3031
011000110708     C                   WRITE     tb88C03
011100110810     C                   SETOFF                                       3031
011200080529     c                   clear                   nrr1
011300080529     c                   clear                   totnrr            5 0
011400110809     c                   clear                   forzavar
011500110112     c
011600110708     c     kcod          setll     tntbe01l
011700110708     c     kcod          reade(n)  tntbe01l
011800110112     c
011900110112    1c                   dow       not %eof(tntbe01l)
012000110112     c
012100110810     c                   clear                   v3hvaru
012200110810     c                   setoff                                       11
012300110708     c                   movel     tbeke2        v3cksc
012400110809     c                   movel     tbeke2        v3hksc
012500110112     c* Chain su cnaco per la ragione sociale
012600110112    2c                   if        v3cksc>*zeros
012700110708     c                   clear                   v3dksc
012800110708     c                   clear                   v3cuni
012900110810     c                   movel     tbeuni        dCXQ
013000110708     c                   exsr      Decodi
013100110810     c
013200110810     c* Se cambiato codice padre --> evidenzio in rosso
013300121120    3c                   if        %subst(tbeke1:1:7)>*zeros
013400110810     c                   eval      wunif=%int(%subst(tbeke1:1:7))
013500110810     c                   else
013600110810     c                   clear                   wunif
013700121120    3c                   endif
013800110810     c
013900110810    3c                   if        wunif  >0 and wunif  <>v3cuni
014000110810     c                   eval      v3hvaru='S'
014100130819     c                   eval      %subst(v3duni:13:10)=' old->'+
014200110810     c                                                   %editc(wunif:'X')
014300110810     c                   seton                                        11
014400110810    3c                   endif
014500121120     c*  Terminal se impostato
014600121120    3c                   if        §cxqterm>*zeros
014700121120     c                   movel     §cxqterm      v3cterm
014800130819   x3c                   else
014900121123     c* se campo vuoto riempo da cod unificante in terminal di partenza
015000121123     c                   clear                   fnlv55ds
015100121123     c                   eval      d55tpt='P'
015200121123     c                   movel     v3cuni        d55lin
015300121123     c                   eval      d55drf=dateu
015400121123     c                   call      'FNLV55R'
015500121123     c                   parm                    fnlv55ds
015600121123     c*
015700121123     c                   movel     d55tfp        v3cterm
015800121120    3c                   endif
015900130819     c
016000130819     c* Vedo se tutte le aree o solo quella del term di riferimento
016100130819    3c                   if        §cxqarea='T'
016200130819     c                   movel     §cxqarea      v3carea
016300130819     c                   movel     000           v3ccar
016400130819   x3c                   else
016500130819     c                   movel     ' '           v3carea
016600130819     c     v3cterm       chain     azorg01l
016700130819    4c                   if        %found(azorg01l)
016800130819     c                   movel     orgcar        v3ccar
016900130819     c                   else
017000130819     c                   clear                   v3ccar
017100130819    3c                   endif
017200130819    4c                   endif
017300130819    2c                   endif
017400121120     c
017500110112     c                   add       1             nrr1
017600110708     c                   write     tb88s03
017700110112     c
017800110708     c     kcod          reade(n)  tntbe01l
017900110708    1c                   enddo
018000110112     c
018100110112     c                   exsr      righevuote
018200110812     c* La prima volta mi posizione in prima videata
018300110812     c                   z-add     1             c01csr
018400110112     c
018500110112     c                   ENDSR
018600110112      *-----------------------------------------------------***********
018700110120     c*  Carico righe vuote del sfl
018800110112      *-----------------------------------------------------***********
018900110112     c     Righevuote    BEGSR
019000110810     c                   setoff                                       11
019100110112     c* Carico le righe vuote fino a riempimento di pagina
019200110112    1c                   if        nrr1>=c_sflpag
019300110112     c                   eval      wpag=%div(nrr1:c_sflPag)
019400110112     c                   eval      wresto=%rem(nrr1:c_sflPag)
019500110112    2c                   if        wresto>0
019600110112     c                   eval      c01csr=nrr1
019700110112     c                   else
019800110112     c                   eval      c01csr=(wpag*c_sflpag)+1
019900110112    2c                   endif
020000110112     c
020100110112   x1c                   else
020200110112     c                   clear                   wpag
020300110112     c                   eval      wresto=nrr1
020400110112     c                   eval      c01csr=nrr1
020500110112    2c                   if        c01csr=0
020600110112     c                   eval      c01csr=1
020700110112    2c                   endif
020800110112    1c                   endif
020900110112     C
021000110112     C                   DOW       wresto<c_sflpag
021100110112     c                   clear                   v3cksc
021200110809     c                   clear                   v3hksc
021300110112     c                   clear                   v3dksc
021400110708     c                   clear                   v3cuni
021500110708     c                   clear                   v3duni
021600110810     c                   clear                   v3hvarU
021700121120     c                   clear                   v3cterm
021800121123     c                   clear                   v3hterm
021900130819     c                   clear                   v3carea
022000130819     c                   clear                   v3ccar
022100110112     c*
022200110112     c                   eval      wresto=wresto+1
022300110112     c                   add       1             nrr1
022400110708     c                   write     tb88s03
022500110112     c                   enddo
022600110112     c
022700110112     c                   z-add     nrr1          totnrr
022800110112     c                   seton                                        33
022900110112     c                   ENDSR
023000110112      *-----------------------------------------------------***********
023100110112     c*  Gestione SFL
023200110112      *-----------------------------------------------------***********
023300110112     c     Gessfl        BEGSR
023400110112     c
023500110114     c                   setoff                                       28
023600110112     c
023700110112     c     for02         tag
023800110114     c                   eval      c01rcd=c01csr
023900110114     c
024000110708     c  n28              write     tb88z03
024100110708     c                   exfmt     tb88c03
024200110112     C                   SETOFF                                       289001
024300121120     C                   SETOFF                                       2941
024400110112     c                   clear                   v1cmsg
024500110112     C* F3 - FINE
024600110812    1c                   if        *inkc
024700110809     c                   exsr      ContrTAB
024800110812     c
024900110812     c* Errore --> modifiche wffettuate finestra di avviso
025000110812    2c                   if        *in90
025100110812     c                   setoff                                       kc
025200110812    3c                   dow       not *inkc and not *inkf
025300110812     c                   exfmt     tb88w07
025400110812    3c                   enddo
025500110812     c
025600110812    2c                   endif
025700110809     c
025800110812     c* Premuto o ripremuto f3
025900110812    2c                   if        *inkc
026000110809     C                   eval      $fine='1'
026100110809     c                   leavesr
026200110812    2c                   endif
026300110812    1c                   endif
026400110812     c
026500110708     C* F7 - ricerca per cliente unificante
026600110708     c                   if        *inkg
026700110708     c                   exsr      RicercaUnif
026800110127     c                   seton                                        90
026900110127     c                   goto      for02
027000110127     c                   endif
027100110708     c
027200110708     C* F10- Inserimento riga vuota
027300110708     c                   if        *inkj
027400110708     c                   exsr      InserRiga
027500110708     c                   seton                                        90
027600110708     c                   goto      for02
027700110708     c                   endif
027800110120     c
027900110120     c* ROLLUP, solo se ultima riga piena
028000110120     c                   if        *in37
028100110708     c     totnrr        chain     tb88s03
028200110120     c                   if        %found and  v3cksc>=*zeros
028300110120     c                   eval      nrr1=totnrr
028400110120     c                   exsr      righevuote
028500110120     c                   else
028600110120     c                   seton                                        289029
028700110120     c                   eval      v1cmsg=msg(4)
028800110120     c                   exsr      Aggiosfl
028900110120     c                   endif
029000110120     c                   goto      for02
029100110120     c                   endif
029200110112     c*
029300110112     c                   exsr      ctrd02
029400110112     c
029500110812     c                   if        *in90 or (not *inkf  and not *inkb and
029600110812     c                                       not *inkk)
029700110112     c                   goto      for02
029800110112     c                   endif
029900110810     c
030000110810     c* F2 - controlla unificanti
030100110810     c                   if        *inkb
030200110810     c                   exsr      Gessfl05
030300110810     c                   goto      for02
030400110810     c                   endif
030500110812     c* F6 o F11 - conferma
030600110812     c                   if        *inkf  or *inkk
030700110112     c                   exsr      ScriviTAB
030800110112     c                   endif
030900110812     c* F11 - Stampa
031000110812     c                   if        *inkk
031100110812     c                   exsr      StampaTab
031200110812     c                   endif
031300110112     c
031400110112     c                   ENDSR
031500110708      *-----------------------------------------------------***********
031600110708      * ricerca ed inserimento per cliente unificante
031700110708      *-----------------------------------------------------***********
031800110708     C     RicercaUnif   BEGSR
031900110708     c                   clear                   xclirds
032000110708     c                   eval      dxccap=dutkci
032100110708     c                   eval      dxcaut='AZ'
032200110708     c                   call      'XCLIR'
032300110708     c                   parm                    xclirds
032400110708     c
032500110708    1c                   if        dxcf03=*blanks and dxcerr=*blanks
032600110708
032700110708     C                   MOVEA     DXCKSC        xKSA                            *SCOMATTA CODICI
032800110708DO  2C     1             DO        30            xx
032900110708     C                   MOVE      xKSA(xx)      DSKSA                          *SCH ALF -> DS ALF
033000110708     C                   Z-ADD     DSKSN         xKSC(xx)                        *DS NUM -> SCH NUM
033100110708E   2C                   ENDDO
033200110708     c
033300110708    2c                   if        xksc(1)>*zeros
033400110708     c                   write     tb88z03
033500110708     c                   write     tb88c03
033600110708     c                   clear                   w4cconf
033700110803     c
033800110810     c                   dow       w4cconf='  '    and not *inkl
033900110708     c                   exfmt     tb88w04
034000110803     c                   enddo
034100110708     c
034200110810    3c                   if        w4cconf='SI'
034300110708     c* cerco la prima posizione vuota
034400110708     c                   eval      nrr1=totnrr+1
034500110708     c                   clear                   v3cksc
034600110812     c                   clear                   wPrimo            1
034700110708    4c                   dow       nrr1>0 and v3cksc<=*zeros
034800110708     c                   eval      nrr1=nrr1-1
034900110708     c     nrr1          chain     tb88s03
035000110708    4c                   enddo
035100110708     c
035200110708     c* trovato il primo record vuoto--> aggiungo da li in poi
035300110708     c                   eval      xx=1
035400110708    4c                   dow       xksc(xx)>*zeros
035500110708     c* Per ogni codice, carico e suoi figli
035600110708     c                   clear                   tibs10ds
035700110708     c                   eval      d10tle='ST'
035800110708     c                   eval      d10cod=xksc(xx)
035900110708     c                   eval      d10paf='F'
036000110708     c                   call      'TIBS10R'
036100110708     c                   parm                    tibs10ds
036200110708     c                   parm      'R'           d10ela            1
036300110708     c
036400110708     c                   if        d10err<>*blanks
036500110708     c* se non trovato cerco come Figlio
036600110708     c                   clear                   skc
036700110708     c                   eval      d10paf='P'
036800110708     c                   call      'TIBS10R'
036900110708     c                   parm                    tibs10ds
037000110708     c                   parm      'R'           d10ela            1
037100110708     c
037200110708     c
037300110708     c* se non ho trovato nulla --> imposto se stesso   padre=figlio
037400110708     c                   if        d10err<>*blanks
037500110708     c                   movel     d10cod        d10cop
037600110708     c                   movel     d10cod        skc(1)
037700110708     c                   endif
037800110708     c                   endif
037900110708     c
038000110708     c                   z-add     1             zz
038100110708    5c                   dow       skc(zz)>*zeros
038200110708     c                   eval      kksc=%int(%subst(skc(zz):5:7))
038300110708     c* carico solo se non già presente
038400110708     c                   eval      Indx=%lookup(kksc:ksc)
038500110708     c
038600110708   5ac                   if        Indx=0
038700110708     c                   eval      nrr1=nrr1+1
038800110708     c
038900110809     c* se tutte le righe scritte sono piene--> aggiungo una pagina
039000110708    6c                   if        nrr1>totnrr
039100110708     c                   eval      wnrr =nrr1
039200110708     c                   eval      nrr1=totnrr
039300110708     c
039400110708     c                   exsr      righevuote
039500110708     c                   eval      nrr1=wnrr
039600110708    6c                   endif
039700110708     c
039800110708     c     nrr1          chain     tb88s03
039900110708    6c                   if        %found
040000110708     c                   movel     kksc          v3cksc
040100110708     c                   clear                   v3dksc
040200110708     c                   seton                                        29
040300110708     c                   eval      v3dksc='--> INSERITO'
040400110708     c                   move      d10cop        v3cuni
040500121120     c                   if        v3cterm<=*zeros
040600121123     c                   clear                   fnlv55ds
040700121123     c                   eval      d55tpt='P'
040800121123     c                   eval      d55drf=dateu
040900121123     c                   movel     v3cuni        d55lin
041000121123     c                   call      'FNLV55R'
041100121123     c                   parm                    fnlv55ds
041200121123     c*
041300121123     c                   movel     d55tfp        v3cterm
041400121120     c                   endif
041500130819     c
041600130819     c     v3cterm       chain     azorg01l
041700130819     c                   if        %found(azorg01l)
041800130819     c                   movel     orgcar        v3ccar
041900130819     c                   else
042000130819     c                   clear                   v3ccar
042100130819    2c                   endif
042200130819     c                   clear                   v3carea
042300121120     c
042400110708     c                   exsr      Decodi
042500110708     c                   exsr      Aggiosfl
042600110812     c
042700110812     c* Posizionamento sul primo record inserito
042800110812     c                   if        Wprimo=' '
042900110812     c                   eval      c01csr=nrr1
043000110812     c                   eval      Wprimo='S'
043100110812     c                   endif
043200110812     c
043300110708    6c                   endif
043400110708   5ac                   endif
043500110708
043600110708     c                   eval      zz=zz+1
043700110708    5c                   enddo
043800110708
043900110708     c                   eval      xx=xx+1
044000110708    4c                   enddo
044100110708     c
044200110708    3c                   endif
044300110708    2c                   endif
044400110708    1c                   endif
044500110708     c*
044600110708     c                   ENDSR
044700110112      *-----------------------------------------------------***********
044800110127      * Inserimento riga vuota
044900110112      *-----------------------------------------------------***********
045000110127     C     InserRiga     BEGSR
045100110127     c
045200110127     c* Se ultima riga piena, aggiungo una pagina, poi sposto
045300110127     c                   eval      savcsr=c01csr
045400110127     c
045500110708     c     totnrr        chain     tb88s03
045600110127    1c                   if        %found and  v3cksc>=*zeros
045700110127     c                   eval      nrr1=totnrr
045800110127     c                   exsr      righevuote
045900110127    1c                   endif
046000110127
046100110809     c* sposto la penultima nell'ultima e così via, fino al posizionamento record
046200110127     c                   eval      nrr1=totnrr-1
046300110127     c
046400110127    1c                   dow       nrr1>savcsr
046500110127     c
046600110708     c     nrr1          chain     tb88s03
046700110127    2c                   if        %found
046800110809     c                   movel     v3hksc        savhksc
046900110809     c                   movel     v3cksc        savksc
047000110127     c                   movel     v3dksc        savdksc
047100110708     c                   movel     v3cuni        savuni
047200110708     c                   movel     v3duni        savduni
047300121120     c                   movel     v3cterm       savterm
047400121123     c                   movel     v3hterm       savhterm
047500130819     c                   movel     v3carea       savarea
047600130819     c                   movel     v3ccar        savcar
047700110127     c                   eval      wnrr=nrr1+1
047800110708     c     wnrr          chain     tb88s03
047900110809     c                   clear                   v3hksc
048000110809     c                   clear                   v3cksc
048100110127     c                   clear                   v3dksc
048200110708     c                   clear                   v3cuni
048300110708     c                   clear                   v3duni
048400121120     c                   clear                   v3cterm
048500121123     c                   clear                   v3hterm
048600130819     c                   clear                   v3carea
048700130819     c                   clear                   v3ccar
048800110809     c                   eval      v3hksc=savhksc
048900110809     c                   eval      v3cksc=savksc
049000110127     c                   eval      v3dksc=savdksc
049100110708     c                   eval      v3cuni=savuni
049200110708     c                   eval      v3duni=savduni
049300121120     c                   eval      v3cterm=savterm
049400121123     c                   eval      v3hterm=savhterm
049500130819     c                   eval      v3carea=savarea
049600130819     c                   eval      v3ccar =savcar
049700110127     c                   exsr      Aggiosfl
049800110127    2c                   endif
049900110127     c
050000110127     c                   eval      nrr1=nrr1-2
050100110127     c                   enddo
050200110127     c
050300110127     c* clearo la riga sotto al F10
050400110127     c                   eval      wnrr=nrr1+1
050500110708     c     wnrr          chain     tb88s03
050600110127     c                   clear                   v3cksc
050700110809     c                   clear                   v3hksc
050800110127     c                   clear                   v3dksc
050900110708     c                   clear                   v3cuni
051000110708     c                   clear                   v3duni
051100121120     c                   clear                   v3cterm
051200121123     c                   clear                   v3hterm
051300130819     c                   clear                   v3carea
051400130819     c                   clear                   v3ccar
051500110127     c                   exsr      Aggiosfl
051600110127     c
051700110127     c                   eval      c01csr=savcsr
051800110127     c                   ENDSR
051900110127     c
052000110127      *-----------------------------------------------------***********
052100110127      * controllo dati sfl
052200110127      *-----------------------------------------------------***********
052300110127     C     ctrd02        BEGSR
052400110809     c                   clear                   forzavar
052500110112     c*
052600110112     c                   eval      nrr1=1
052700110112     c                   z-add     1             yy
052800110809     c                   z-add     1             zz
052900110112     c                   clear                   ksc
053000110112     c*
053100110708    0c                   dow       nrr1<=totnrr
053200110120     c
053300110708     c     nrr1          chain     tb88s03
053400110810   0ac                   if        %found
053500110112     C*
053600110112     c     '?'           scan      v3cksc                                 90
053700110112    1c                   if        *in90
053800110112     c                   movel     rsut          paxdut           30
053900110112     c                   movel(p)  v3dksc        paxdmt           48
054000110112      * PAXSTA=9 ESCLUDO ANNULLATI
054100110112     C                   Z-ADD     9             PAXSTA
054200110112     c                   movel     dutkci        paxccc            4 0
054300110112     c                   clear                   paxdit
054400110112     C                   z-add     1             paxnum
054500110112     C                   CALL      'XALFA3BR'
054600110112     C                   PARM                    PAXDUT
054700110112     C                   PARM      1             CODUT             1 0
054800110112     C                   PARM                    PAXDMT
054900110112     C                   PARM                    PAXCCC
055000110112     C                   PARM                    PAXSTA            1 0
055100110112     C                   PARM                    PAXFLR           90
055200110112     C                   PARM                    PAXDIT            3
055300110112     C                   PARM                    PAXNUM            2 0
055400110112     C                   PARM                    PAXKCM           80
055500110112     C                   PARM                    PAXKSM          140
055600110112     C                   PARM                    PAXKDM           60
055700110112    2C     PAXSTA        IFGT      -1
055800110112     C                   MOVEL     paxksm        v3cksc
055900110112     C                   MOVEL     paxdmt        v3dksc
056000110803     c                   else
056100110803     c                   clear                   v3cksc
056200110112     c                   endif
056300110112    2c                   exsr      Aggiosfl
056400110112     c                   leavesr
056500110112    1c                   endif
056600110112     c
056700110112    1c                   if        v3cksc<>*blanks and v3cksc<>*zeros
056800110120     c                   testn                   v3cksc               35
056900110112     c
057000110120    2c                   if        not *in35  or %subst(v3cksc:7:1)<'0'
057100110114     c                   seton                                        29
057200110112     c                   eval      v1cmsg=msg(2)
057300110112     c                   exsr      Aggiosfl
057400110112     c                   leavesr
057500110112    2c                   endif
057600110112     c
057700110112     c                   movel     v3cksc        kksc
057800110112     c     kaco          chain     cnaco00f
057900110112    2c                   if        %found(cnaco00f)
058000110112     c                   movel     acorag        v3dksc
058100110112   x2c                   else
058200110120     c                   seton                                        29
058300110112     c                   eval      v1cmsg=msg(2)
058400110112     c                   exsr      Aggiosfl
058500110112     c                   leavesr
058600110112    2c                   endif
058700110112     c
058800110112     c*  Verifico se già presente
058900110114     c     kksc          lookup    ksc                                    29
059000110708    2c                   if        *in29
059100110112     c                   eval      v1cmsg=msg(3)
059200110112     c                   exsr      Aggiosfl
059300110112     c                   leavesr
059400110112     c                   else
059500110112     c                   eval      ksc(yy)=kksc
059600110112     c                   eval      yy=yy+1
059700110809     c
059800110809     c* Memorizzo tutti gli unificanti per eventuale controllo
059900110810     c     v3cuni        lookup    unif                                   35
060000110810    2c                   if        not *in35
060100110810     c                   eval      unif(zz) =v3cuni
060200110810     c                   eval      unifd(zz)=v3duni
060300110809     c                   eval      zz=zz+1
060400110809    2c                   endif
060500110809     c
060600110112    2c                   endif
060700110810     c
060800110810     c                   if        v3cksc<>v3hksc
060900110708     c                   clear                   v3cuni
061000110810     c                   clear                   v3hvaru
061100110708     c                   exsr      Decodi
061200110810     c*
061300110810     c                   else
061400121123     c* se uguale do msg forzabile per cambio codic unificante
061500110810     c                   if        v3hvaru='S'
061600110810     c                   seton                                        29
061700110810     c                   eval      v1cmsg=msg(8)
061800110810     c                   eval      v3hvaru='X'
061900110810     c                   exsr      Aggiosfl
062000110810     c                   leavesr
062100110810    2c                   endif
062200110810     c                   endif
062300121123     c
062400130819     c* controllo terminal   tutti 888= nessuno
062500121120     c     v3cterm       chain     azorg01l
062600130819    2c                   if        v3cterm<>888
062700121123    3c                   if        not %found(azorg01l) or (orgfag<>'A' and
062800121123     c                             orgfag<>'F')  or orgfva<>' '
062900121120     c                   seton                                        41
063000121120     c                   eval      v1cmsg=msg(9)
063100121120     c                   exsr      Aggiosfl
063200121120     c                   leavesr
063300121123    3c                   endif
063400121123     c
063500121123   3ac                   if        v3hterm<>v3cterm
063600121123     c                   clear                   wok               1
063700121123     c                   eval      kepa='P'
063800121123    3c                   do        2
063900121123     c     kcae          setll     azcae05l
064000121123     c     kcae          reade     azcae05l
064100121123    4c                   dow       not %eof(azcae05l)
064200121123    5c                   if        dateu >=caedde and dateu<=caedsc
064300121123     c                   eval      wok='S'
064400121123     c                   leave
064500121123    5c                   endif
064600121123     c
064700121123     c     kcae          reade     azcae05l
064800121123    4c                   enddo
064900121123    4c                   if        wok='S'
065000121123     c                   leave
065100121123     c                   else
065200121123     c                   eval      kepa='A'
065300121123    4c                   endif
065400121123    3c                   enddo
065500121123     c
065600121123     c                   eval      v3hterm=v3cterm
065700121123    3c                   if        wok=' '
065800121123     c                   seton                                        41
065900121123     c                   eval      v1cmsg=msg(10)
066000121123     c                   exsr      Aggiosfl
066100121123     c                   leavesr
066200121123    3c                   endif
066300121123   3ac                   endif
066400130819     c
066500130819     c* se impostato "T" tutte le aree, altrimenti quella del terminal
066600130819     c                   if        v3carea='T'
066700130819     c                   movel     000           v3ccar
066800130819     c                   else
066900130819     c                   movel     orgcar        v3ccar
067000130819     c                   endif
067100130819     c
067200121123     c
067300121123    2c                   endif
067400110114     c*
067500110114     c                   exsr      Aggiosfl
067600110112     c*
067700110708   x1c                   else
067800110112     c                   clear                   v3dksc
067900110708     c                   clear                   v3duni
068000110708     c                   clear                   v3cuni
068100130819     c                   clear                   v3hvaru
068200121120     c                   clear                   v3cterm
068300130819     c                   clear                   v3hterm
068400130819     c                   clear                   v3carea
068500130819     c                   clear                   v3ccar
068600110112     c                   exsr      Aggiosfl
068700110112    1c                   endif
068800110120     c
068900110708   0ac                   endif
069000110112     c
069100110112     c                   eval      nrr1=1+nrr1
069200110112     c*
069300110708    0c                   enddo
069400110120     c                   z-add     1             nrr1
069500110112     c
069600110112     c                   ENDSR
069700110112      *-----------------------------------------------------***********
069800110112      * Aggiornamento tabella
069900110112      *-----------------------------------------------------***********
070000110112     C     ScriviTAB     BEGSR
070100110112     c*
070200110112     c* Intanto cancello tutti i record
070300110112     c
070400110112     c/exec sql
070500110708     c+ delete from tntbe00f where tbecod='CXQ' and tbeke2<>'               '
070600110112     c/end-exec sql
070700110112     c
070800110112     c
070900110112     c                   eval      nrr1=1
071000110112     c                   z-add     1             yy
071100110112     c                   clear                   ksc
071200110112     c*
071300110708     c     nrr1          chain     tb88s03
071400110112    0c                   dow       %found
071500110112     c
071600110112    1c                   if        v3cksc>*zeros
071700110112     c                   clear                   tntbe000
071800110708     c                   eval      tbecod='CXQ'
071900110708     c                   eval      tbeapl='GE'
072000110810     c                   movel     v3cuni        tbeke1
072100110708     c                   eval      tbeke2=v3cksc
072200110810     c                   clear                   dcxq
072300110810     c                   movel     v3dksc        §cxqrsc
072400110810     c                   movel     v3duni        §cxqrsu
072500121120     c                   movel     v3cterm       §cxqterm
072600130819     c                   movel     v3carea       §cxqarea
072700110810     c                   movel     dcxq          tbeuni
072800110112     c                   write     tntbe000
072900110114     c                   eval      yy=yy+1
073000110112     c                   endif
073100110809     c
073200110809     c                   eval      v3hksc=v3cksc
073300110809     c                   exsr      AGGIOSFL
073400110112     c
073500110112     c                   eval      nrr1=nrr1+1
073600110708     c     nrr1          chain     tb88s03
073700110112     c                   enddo
073800121127     c
073900121127     c                   clear                   trul0sds
074000121127     c                   eval      i0stla='L'
074100121127     c                   eval      i0sski='CXQ'
074200121127     c                   eval      i0sele=1100
074300121127     c                   eval      i0spie=yy
074400121127     c                   eval      i0sfile='TNTBE00F'
074500121127     c                   eval      i0ssif=knsif
074600121127     c                   eval      i0spgm='TNSSL3R2'
074700121127     c                   movel     trul0sds      kpjbu
074800121127     c                   call      'TRUL0SR'
074900121127     c                   parm                    kpjba
075000121127
075100110112     c                   ENDSR
075200110809      *-----------------------------------------------------***********
075300110809      * controllo tutto il sfl per F3 per vedere se ci sono dei record
075400110809      *  variati che verranno persi
075500110809      *-----------------------------------------------------***********
075600110809     C     ContrTAB      BEGSR
075700110809     c
075800110809     c                   eval      nrr1=1
075900110809     c                   z-add     1             yy
076000110809     c*
076100110809     c     nrr1          chain     tb88s03
076200110809    0c                   dow       %found
076300110809     c
076400110809     c                   if        v3cksc=*zeros
076500110809     c                   clear                   v3cksc
076600110809     c                   endif
076700110809     c                   if        v3hksc=*zeros
076800110809     c                   clear                   v3hksc
076900110809     c                   endif
077000110809     c
077100110809     c                   if        v3cksc<>v3hksc
077200110812     c                   seton                                        90
077300110809     c                   eval      Forzavar='S'
077400110809     c                   leavesr
077500110809     c                   endif
077600110809     c
077700110809     c                   eval      nrr1=nrr1+1
077800110809     c     nrr1          chain     tb88s03
077900110809     c                   enddo
078000110809     c                   ENDSR
078100110810      *-----------------------------------------------------***********
078200110810     c*  Gestione SFL 05 - controlla unificanti
078300110810      *-----------------------------------------------------***********
078400110810     c     Gessfl05      BEGSR
078500110810     c                   exsr      contrUnif
078600110810     c
078700110810     c                   setoff                                       28
078800110810     c* se non ho caricato nemmeno un record videata vuota
078900110810     c                   if        nrr5=0
079000110810     c                   seton                                        30
079100110810     c                   write     tb88c06
079200110810     c                   exfmt     tb88c05
079300110810     c
079400110810     c                   setoff                                       30
079500110810     c
079600110810     c                   else
079700110810     c
079800110810     c     for05         tag
079900110810     c                   eval      c05rcd=c05csr
080000110810     c
080100110810     c  n28              write     tb88z05
080200110810     c*
080300110810     c                   exfmt     tb88c05
080400110810     C                   SETOFF                                       2890
080500110810     C                   SETOFF                                       40
080600110810     c                   clear                   v1cmsg
080700110810     C* F12- ritrono
080800110810     c                   if        *inkl
080900110810     c                   leavesr
081000110810     c                   endif
081100110810     c
081200110810     c                   exsr      ctrd05
081300110810     c
081400110810     c                   if        *in90 or not *inkf
081500110810     c                   goto      for05
081600110810     c                   endif
081700110810     c* F6 - conferma
081800110810     c                   if        *inkf
081900110810     c                   exsr      ScriviTAB05
082000110810     c                   endif
082100110810     c
082200110810     c                   endif
082300110810     c
082400110810     c                   ENDSR
082500110809      *-----------------------------------------------------***********
082600110809      * Controllo i codici unificanti se per caso sono variati i legami
082700110809      *  li evidenzio
082800110809      *-----------------------------------------------------***********
082900110809     C     ContrUNIF     BEGSR
083000110809     c* Elaboro la schiera degli unifficanti e controllo rispoetto a
083100110809     c*  quelli inseriti in tabella se ce ne sono in più o meno
083200110809     C* PULIZIA SFL
083300110809     C                   SETON                                        3031
083400110809     C                   WRITE     tb88C05
083500110809     C                   SETOFF                                       3031
083600110809     c                   clear                   nrr5
083700110809     c                   clear                   totnrr5           5 0
083800110809     c
083900110809     c                   z-add     1             xx
084000110809    1c                   dow       unif(xx)>0
084100110809     c                   clear                   tibs10ds
084200110809     c                   eval      d10tle='ST'
084300110809     c                   eval      d10cod=unif(xx)
084400110809     c                   eval      d10paf='F'
084500110809     c                   call      'TIBS10R'
084600110809     c                   parm                    tibs10ds
084700110809     c                   parm      'R'           d10ela            1
084800110809     c
084900110809    2c                   if        d10err<>*blanks
085000110809     c* se non trovato cerco come Figlio
085100110809     c                   clear                   skc
085200110809     c                   eval      d10paf='P'
085300110809     c                   call      'TIBS10R'
085400110809     c                   parm                    tibs10ds
085500110809     c                   parm      'R'           d10ela            1
085600110809     c
085700110809     c
085800110809     c* se non ho trovato nulla --> imposto se stesso   padre=figlio
085900110809    3c                   if        d10err<>*blanks
086000110809     c                   movel     d10cod        d10cop
086100110809     c                   movel     d10cod        skc(1)
086200110809    3c                   endif
086300110809    2c                   endif
086400110809     c
086500110809     c                   z-add     1             zz
086600110809    2c                   dow       skc(zz)>*zeros
086700110809     c                   eval      kksc=%int(%subst(skc(zz):5:7))
086800110809     c
086900110809     c* Verifico se già presente nella tabella
087000110810     c     kksc          lookup    ksc                                    35
087100110810    3c                   if        not *in35
087200110810     c                   setoff                                       10
087300110810     c                   eval      v5dmod='MANCANTE '
087400110809     c                   eval      v5cuni=unif(xx)
087500110810     c                   eval      v5duni=unifd(xx)
087600110809     c                   eval      v5cksc=kksc
087700110809     c     kaco          chain     cnaco00f
087800110809    4c                   if        %found(cnaco00f)
087900110809     c                   movel     acorag        v5dksc
088000130820     c                   else
088100130820     c                   movel     *all'?'       v5dksc
088200110809    4c                   endif
088300110809     c                   add       1             nrr5
088400110809     c                   write     tb88s05
088500110809    3c                   endif
088600110809     c
088700110809     C                   EVAL      zz=zz+1
088800110809    2c                   enddo
088900110809     c
089000110809     C                   EVAL      xx=xx+1
089100110809    1c                   enddo
089200110809     c
089300110810     c                   eval      c05csr=1
089400110810     c                   z-add     nrr5          totnrr5
089500110809     c                   ENDSR
089600110810      *-----------------------------------------------------***********
089700110810      * controllo dati sfl 05
089800110810      *-----------------------------------------------------***********
089900110810     C     ctrd05        BEGSR
090000110810     c*
090100110810     c                   eval      nrr5=1
090200110810     c                   z-add     1             yy
090300110810     c                   z-add     1             zz
090400110810     c                   clear                   wopz
090500110810     c*
090600110810    0c                   dow       nrr5<=totnrr5
090700110810     c
090800110810     c     nrr5          chain     tb88s05
090900110810    1c                   if        %found
091000110810     c* Per aggiunta solo "I"
091100110810    2c                   if        v5copz='I' and %subst(v5dmod:1:1)='E'
091200110810     c                   seton                                        40
091300110810     c                   eval      v1cmsg=msg(6)
091400110810     c                   exsr      Aggiosfl5
091500110810     c                   leavesr
091600110810    2c                   endif
091700110810     c* Per eliminaz. solo "A"
091800110810    2c                   if        v5copz='A' and %subst(v5dmod:1:1)='A'
091900110810     c                   seton                                        40
092000110810     c                   eval      v1cmsg=msg(1)
092100110810     c                   exsr      Aggiosfl5
092200110810     c                   leavesr
092300110810    2c                   endif
092400110810     c
092500110810     c* se premuto F6 ci deve essere almeno una "I" o una "A"
092600110810    2c                   if        v5copz<>*blanks
092700110810     c                   eval      wopz=v5copz
092800110810    2c                   endif
092900110810     c
093000110810    1c                   endif
093100110810     c
093200110810     c                   eval      nrr5=1+nrr5
093300110810     c*
093400110810    0c                   enddo
093500110810     c
093600110810     c* se premuto F6 ci deve essere almeno una "I" o una "A"
093700110810     c   kf              if        wopz=' '
093800110810     c                   z-add     1             nrr5
093900110810     c                   seton                                        40
094000110810     c                   eval      v1cmsg=msg(7)
094100110810     c                   exsr      Aggiosfl5
094200110810    2c                   endif
094300110810     c
094400110810     c                   ENDSR
094500110810      *-----------------------------------------------------***********
094600110810      * Aggiunta/elimiinazione recod da sfl
094700110810      *-----------------------------------------------------***********
094800110810     C     ScriviTAB05   BEGSR
094900110810     c
095000110810     c                   eval      nrr5=1
095100110810     c                   z-add     1             yy
095200110810     c                   clear                   ksc
095300110812     c                   clear                   wPrimo
095400110810     c*
095500110810     c     nrr5          chain     tb88s05
095600110810    0c                   dow       %found
095700110810     c
095800110810    1c                   if        v5copz='I'
095900110810     c
096000110810     c* cerco la prima posizione vuota
096100110810     c                   eval      nrr1=totnrr+1
096200110810     c                   clear                   v3cksc
096300110810    2c                   dow       nrr1>0 and v3cksc<=*zeros
096400110810     c                   eval      nrr1=nrr1-1
096500110810     c     nrr1          chain     tb88s03
096600110810    2c                   enddo
096700110810     c
096800110810     c                   eval      nrr1=nrr1+1
096900110810     c
097000110810     c* se tutte le righe scritte sono piene--> aggiungo una pagina
097100110810    2c                   if        nrr1>totnrr
097200110810     c                   eval      wnrr =nrr1
097300110810     c                   eval      nrr1=totnrr
097400110810     c
097500110810     c                   exsr      righevuote
097600110810     c                   eval      nrr1=wnrr
097700110810    2c                   endif
097800110810     c
097900110810     c     nrr1          chain     tb88s03
098000110810    2c                   if        %found
098100110810     c                   movel     v5cksc        v3cksc
098200110810     c                   movel     v5dksc        v3dksc
098300110810     c                   seton                                        29
098400110810     c                   eval      v3dksc='--> INSERITO'
098500110810     c                   move      v5cuni        v3cuni
098600130820     c                   movel     v5duni        v3duni
098700121120     c                   if        v3cterm<=*zeros
098800121123     c                   clear                   fnlv55ds
098900121123     c                   eval      d55tpt='P'
099000121123     c                   eval      d55drf=dateu
099100121123     c                   movel     v3cuni        d55lin
099200121123     c                   call      'FNLV55R'
099300121123     c                   parm                    fnlv55ds
099400121123     c*
099500121123     c                   movel     d55tfp        v3cterm
099600121120     c                   endif
099700121120     c
099800110810     c                   exsr      Aggiosfl
099900110812     c* Posizionamento sul primo record inserito
100000110812     c                   if        Wprimo=' '
100100110812     c                   eval      c01csr=nrr1
100200110812     c                   eval      Wprimo='S'
100300110812     c                   endif
100400110812     c
100500110810    2c                   endif
100600110810    1c                   endif
100700110810     c
100800110810     c                   eval      nrr5=nrr5+1
100900110810     c     nrr5          chain     tb88s05
101000110810     c                   enddo
101100110810     c
101200110810     c                   ENDSR
101300110112      *****************************************************************
101400110112     c* Aggioranemnto sfl
101500110112      *****************************************************************
101600110112     C     Aggiosfl      BEGSR
101700110112     c                   if        v1cmsg<>*blanks
101800110112     c                   seton                                        9028
101900110112     c                   eval      c01csr=nrr1
102000110112     c                   endif
102100110810     c*
102200110810     c                   if        v3hvaru<>*blanks
102300110810     c                   seton                                        11
102400110810     c                   else
102500110810     c                   setoff                                       11
102600110810     c                   endif
102700110112     c
102800110708     c                   update    tb88s03
102900110810     c                   setoff                                       29
103000110112     c
103100110112     c                   ENDSR
103200110810      *****************************************************************
103300110810     c* Aggioranemnto sfl5
103400110810      *****************************************************************
103500110810     C     Aggiosfl5     BEGSR
103600110810     c                   if        v1cmsg<>*blanks
103700110810     c                   seton                                        9028
103800110810     c                   eval      c05csr=nrr5
103900110810     c                   endif
104000110810     c                   if        %subst(v5dmod:1:1)='E'
104100110810     c                   seton                                        10
104200110810     c                   else
104300110810     c                   setoff                                       10
104400110810     c                   endif
104500110810     c
104600110810     c                   update    tb88s05
104700110810     c                   setoff                                       40
104800110810     c
104900110810     c                   ENDSR
105000110708      *****************************************************************
105100110708     c     Decodi        BEGSR
105200110708    1c                   if        v3dksc=*blanks
105300110708     c                   movel     v3cksc        kksc
105400110708     c     kaco          chain     cnaco00f
105500110708    2c                   if        %found(cnaco00f)
105600110708     c                   movel     acorag        v3dksc
105700110708    2c                   endif
105800110708    1c                   endif
105900110708     c
106000110708     c* Reperisco codice Padre
106100110708    1c                   if        v3cuni=0
106200110708     c                   clear                   tibs10ds
106300110708     c                   eval      d10tle='ST'
106400110708     c                   eval      d10cod=acoksc
106500110708     c                   eval      d10paf='P'
106600110708     c                   call      'TIBS10R'
106700110708     c                   parm                    tibs10ds
106800110708     c                   parm      'R'           d10ela            1
106900110708     c
107000110708     c* Passato tipo elaborazione per non eseguire la chiusura file tutte le volte
107100110708     c*  (li chiude per " " o "C" )
107200110708    2c                   if        d10err=*blanks and d10cop>*zeros
107300110708     c                   move      d10cop        v3cuni
107400110708     c                   else
107500110708     c                   movel     v3cksc        v3cuni
107600110708    2c                   endif
107700110708    1c                   endif
107800110708     c
107900110708    1c                   if        v3cuni>*zeros
108000110708     c                   movel     v3cuni        kksc
108100110708     c     kaco          chain     cnaco00f
108200110708    2c                   if        %found(cnaco00f)
108300110708     c                   movel     acorag        v3duni
108400110812     c                   movel     acorag        P3duni
108500110708    2c                   endif
108600110708    1c                   endif
108700121120     c*  se terminal non immesso, prendo al filiale del cliente unificante
108800121123    1c**                 if        v3cterm<=*zeros
108900121123     c**                 clear                   fnlv55ds
109000121123     c**                 eval      d55tpt='P'
109100121123     c**                 eval      d55drf=dateu
109200121123     c**                 movel     v3cuni        d55lin
109300121123     c**                 call      'FNLV55R'
109400121123     c**                 parm                    fnlv55ds
109500121123     c*
109600121123     c**                 movel     d55tfp        v3cterm
109700121123    1c**                 endif
109800110708     c
109900110708     c                   ENDSR
110000110812      *-----------------------------------------------------***********
110100110812      * Stampa tabella "CXQ"
110200110812      *-----------------------------------------------------***********
110300110812     C     StampaTab     BEGSR
110400110812     c                   seton                                        OF
110500110812     c
110600110812     c     kcod          setll     tntbe01l
110700110812     c     kcod          reade(n)  tntbe01l
110800110812     c
110900110812    1c                   dow       not %eof(tntbe01l)
111000110812     c
111100110812     c                   movel     tbeke2        v3cksc
111200110812     c* Chain su cnaco per la ragione sociale
111300110812    2c                   if        v3cksc>*zeros
111400110812     c                   clear                   V3dksc
111500110812     c                   clear                   v3cuni
111600110812     c                   movel     tbeuni        dCXQ
111700110812     c                   exsr      Decodi
111800110812     c
111900110812     c* Se cambiato codice padre
112000110812     c                   if        %subst(tbeke1:1:7)>*zeros
112100110812     c                   eval      wunif=%int(%subst(tbeke1:1:7))
112200110812     c                   else
112300110812     c                   clear                   wunif
112400110812     c                   endif
112500110812     c
112600110812    3c                   if        wunif  >0 and wunif  <>v3cuni
112700110812     c                   movel     wunif         kksc
112800110812     c     kaco          chain     cnaco00f
112900121120    4c                   if        not %found(cnaco00f)
113000110812     c                   clear                   acorag
113100121120    4c                   endif
113200110812     c
113300130820     c                   eval      %subst(P3duni:13:23)=' in tabella con-->'+
113400110812     c                                   %editc(wunif:'X')+ Acorag
113500110812     c                   seton                                        11
113600110812    3c                   endif
113700121120    2c                   endif
113800121120     c
113900121120     c* Terminal
114000121120    2c                   if        §cxqterm>*zeros
114100121120     c                   movel     §cxqterm      v3cterm
114200121120     c
114300121123     c                   select
114400121123    3c                   when      v3cterm =999
114500121120     c                   eval      p3dterm='T u t t i '
114600121123     c
114700121123    3c                   when      v3cterm =888
114800130820     c                   eval      p3dterm='Da NON elaborare'
114900121123     c                   other
115000121123     c
115100121120     c     v3cterm       chain     azorg01l
115200121120    4c                   if        %found(azorg01l)
115300121120     c                   movel     orgdes        p3dterm
115400121120     c                   else
115500121120     c                   eval      p3dterm=*all'?'
115600121120    4c                   endif
115700130820     c
115800130820     c                   if        §cxqarea='T'
115900130820     c                   eval      p3dare='Tutte'
116000130820     c                   else
116100130820     c                   eval      kkey=%editc(orgcar:'X')
116200130820     c     ktab          chain     tabel00f
116300130820     c                   if        %found(tabel00f)
116400130820     c                   eval      p3dare=%editc(orgcar:'X')+'-'+tbluni
116500130820     c                   else
116600130820     c                   eval      p3dare=%editc(orgcar:'X')+'-'+'?????????????'
116700130820     c                   endif
116800130820    2c                   endif
116900130820     c
117000130820    3c                   endsl
117100130820    2c                   endif
117200130820     c
117300110812     c*
117400110812     C                   IF        *INOF
117500110812     C                   EXCEPT    TESTA
117600110812     C                   SETOFF                                       OF
117700110812     C                   ENDIF
117800110812     C
117900110812     C                   EXCEPT    DET
118000110812     c
118100110812     c     kcod          reade(n)  tntbe01l
118200110812    1c                   enddo
118300110812     c
118400110812     c                   ENDSR
118500110112      *****************************************************************
118600110112     C     *INZSR        BEGSR
118700110112      *
118800110112     C     *ENTRY        PLIST
118900110112     C                   PARM                    KPJBA
119000110112      *
119100110112      *
119200110112      * accesso tntbe
119300110120     c     ktab          klist
119400110120     C                   kfld                    kkut
119500110120     C                   kfld                    kcodt
119600110120     C                   kfld                    kkey
119700110112     c     kaco          klist
119800110112     C                   kfld                    kkut
119900110112     C                   kfld                    dutkci
120000110112     C                   kfld                    kksc
120100110112     c
120200121123     c     kcae          klist
120300121123     C                   kfld                    kepa              1
120400121123     C                   kfld                    v3cterm
120500121123     c
120600110112     c                   exsr      sr_DatiJob
120700110812     c
120800110812     C                   TIME                    WTIME            14 0
120900110812     C                   MOVE      WTIME         WDATE             8 0
121000110812     C                   MOVEL     WTIME         UTIME             6 0
121100121123     c     *eur          movel     wdate         dataiso
121200121123     c                   movel     dataiso       dateu             8 0
121300110112      *
121400110112     C                   ENDSR
121500110112      *---------------------------------------------------------------*
121600110112      *?Reperimento dati del job (Utente/Operativi)                  ?*
121700110112      *---------------------------------------------------------------*
121800110112     c     sr_DatiJob    BEGSR
121900110112      *
122000110112     c     *dtaara       define    §azute        AZUTEds
122100110112     c     *dtaara       define    §datiute      dDatiUte
122200110112      *
122300110112     c                   in(E)     *dtaara
122400110112     c                   IF        %Error or RSUT = *blanks
122500110112     c                   clear                   TIBS34ds
122600110112     c                   call      'TIBS34R'
122700110112     c                   parm                    TIBS34ds
122800110112     c                   in        *dtaara
122900110112     c                   ENDIF
123000110112      *
123100110112     c                   ENDSR
123200110812     c*----------------------------------------------------------------------
123300110812     OQSYSPRT   E            TESTA            01
123400110812     O                       SK(1)               66
123500110812     O                       SK(2)              132
123600110812     O                       RSUT                20
123700110812     O                       WDATE              118 '  /  /    '
123800110812     O                       PAGE          Z    132
123900110812     O          E            TESTA            02
124000110812     O                       KNSIF               10
124100110812     O                       KNMUS               22
124200110812     O                       utimE              116 '  :  :  '
124300110812     O          E            TESTA            03
124400110812     O                       SK(5)               66
124500110812     O                       SK(5)              132
124600110812     O*
124700110812     O          E            TESTA            04
124800110812     O                       SK(3)               66
124900110812     O                       SK(4)              132
125000110812     O*
125100110812     O          E            TESTA            05
125200110812     O                       SK(5)               66
125300110812     O                       SK(5)              132
125400110812     O
125500110812     O          E            DET         1
125600110812     O                       V3CUNI
125700110812     O                       P3DUNI           +   2
125800110812     O                       V3CKSC           +   4
125900110812     O                       V3DKSC           +   2
126000121120     O                       V3cterm          +   4
126100121120     O                       p3dterm          +   2
126200130820     O                       p3dare           +   2
126300020805      **************************************************************************
126400960302** SCHIERA MSG - MESSAGGI DI ERRORE
126500110810Codice  AGGIUNTO  all'Unificante: possibile solo "I-inserimento"                1
126600110810Codice cliente errato o inesistente                                             2
126700110810Codice cliente già inserito                                                     3
126800110810Scorrimento non ammesso: ci sono ancora righe vuote nella pagina                4
126900110810ATTENZIONE alcune modifiche andranno perdute.F3 uscita senza conferma F6-Confer 5
127000110810Codice  ELIMINATO  dall'unificante: possibile solo "A-annullamento"             6
127100110810Uscire con F12 o effettuare almeno una scelta e premere F6                      7
127200110829VARIATO codice Unificante: verificare se eliminare il cod.cliente.ENTER forza   8
127300121120Terminal inesistente                                                            9
127400121123In data odierna la filiale indicata NON E' un terminal: enter forza            10
127500110812**
127600110812XXXXXXXXXXXXXXXXXXXX                     ** STAMPA TABELLA CLIENTI   1
127700110812 PARTICOLARI **               TNTB88R     XX.XX.XXXX     PAG. XXXX   2
127800130820Cliente Unificante attuale                      Codice Cliente       3
127900130820                        Terminal Appartenenza       Area             4
128000110812------------------------------------------------------------------   5
