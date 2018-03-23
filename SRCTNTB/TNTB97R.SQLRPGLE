000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200121217      * tntb97r *----------------------------------------------------*
000300121217      *         -Tabella "KPV" Gestione tabella raggruppamenti clienti
000400940915      *--------------------------------------------------------------*
000500121217     Ftntb97D   CF   E             WORKSTN
000600121217     f                                     sfile(tb97s03 : nrr1)
000700121217     f                                     sfile(tb97s05 : nrr5)
000800110112     FTNTBE01L  uf a E           k DISK
000900110112     Ftabel00f  if   E           k DISK
001000110112     Fcnaco00f  if   E           k DISK
001100110112     Fcnclp00f  if   E           k DISK
001200110812     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300110812     f                                     FORMLEN(066)
001400110812     f                                     FORMOFL(063)
001500020805      *
001600020805      * DEFINIZIONE SCHIERE
001700960302     D*
001800110810     D MSG             S             78    DIM(9) CTDATA PERRCD(1)
001900110812     D SK              S             66    DIM(5) CTDATA PERRCD(1)
002000110803     D ksc             S              7  0 DIM(900)
002100110810     D Unif            S              7  0 DIM(400)
002200110810     D UnifD           S             24    DIM(400)
002300110112     D*
002400110112     D KPJBA         E DS
002500121217     D dKPV          E DS
002600110810     d
002700110708     D tibs10ds      E DS
002800110708     d  skc                   21   5520    dim(500)
002900110112     d TIBS02DS      E DS
003000110708     d xclirds       E DS
003100110708     D xKSC            S              7  0 DIM(30)
003200110708     D xKSA            S              4    DIM(30)                              *ALFANUMERICO
003300110708
003400110708     D                 DS
003500110708     D  DSKSN                  1      4P 0
003600110708     D  DSKSA                  1      4
003700110708     d
003800110112      * - Parametri per richiamo al pgm di controllo profilo utenti
003900110112     d TIBS34ds      e ds
004000110112      * - Ds di riferimento al file esterno AzUte00F
004100110112     d AZUTEds       e ds                  ExtName(AzUte00F)
004200110112      * - Ds per dati organigramma
004300110112     d dDatiUte      e ds
004400080529     d
004500080529     D WLBDAT          DS                  INZ
004600080529     D  G02DAT                 1      8  0
004700080529     D  G02INV                 9     16  0
004800080529     D  G02ERR                17     17
004900080529     D  G02TGI                18     22  0
005000121217     d
005100121217     d TNTB97DS        ds                  inz
005200121217     d  B97ke1                 1      3
005300121217     d  B97ke2                 4     10  0
005400121217     d  B97descr              11     55
005500121217
005600080529     d
005700050103     d Dataiso         s               d   datfmt(*iso)
005800050103     d Dataeur         s               d   datfmt(*eur)
005900080529     d datadmy         s               d   datfmt(*dmy)
006000050103     d
006100080529     d nrr1            s              4  0 inz
006200110809     d nrr5            s              4  0 inz
006300110120     D kcodt           S                   LIKE(tblcod) inz('01')
006400110120     D kkey            S                   LIKE(tblkey)
006500121217     D kcod            S                   LIKE(tbecod) inz('KPV')
006600110120     D kkey1           S                   LIKE(tbeke1)
006700110112     D kkey2           S                   LIKE(tbeke2)
006800110114     D kkut            S                   LIKE(acokut) inz(1)
006900110112     D kksc            S                   LIKE(acoksc)
007000110127     D savksc          S                   LIKE(v3cksc)
007100110809     D savhksc         S                   LIKE(v3cksc)
007200110127     D savdksc         S                   LIKE(v3dksc)
007300110708     D savuni          S                   LIKE(v3cuni)
007400110708     D savduni         S                   LIKE(v3duni)
007500110127     D wnrr            S                   LIKE(nrr1)
007600110127     D savcsr          S                   LIKE(c01csr)
007700110114     d c_SflPag        c                   const(17)
007800110112     d wpag            s              6  0
007900110112     d wresto          s              3  0
008000110708     d zz              s              3  0
008100110708     d yy              s              3  0
008200110708     d XX              s              3  0
008300110708     d Indx            s              3  0
008400110112     d $Fine           s              1
008500110810     d Wopz            s              1
008600110809     d Forzavar        s              1
008700110810     d wunif           s              7  0
008800110812     d P3DUNI          s             45
008900041220     d
009000050103     C**********************************************************************
009100110112     c
009200110112     c                   exsr      Carsfl
009300110112     c                   exsr      Gessfl
009400080529     c
009500960129     C*
009600000000     C                   SETON                                        LR
009700080529      *-----------------------------------------------------***********
009800110112      * Caricamento sfl
009900080529      *-----------------------------------------------------***********
010000110112     C     CarSFL        BEGSR
010100080529     C* PULIZIA SFL
010200080529     C                   SETON                                        3031
010300121217     C                   WRITE     tb97C03
010400110810     C                   SETOFF                                       3031
010500080529     c                   clear                   nrr1
010600080529     c                   clear                   totnrr            5 0
010700110809     c                   clear                   forzavar
010800110112     c
010900121219     c                   eval      kkey1=b97ke1 +
011000121219     c                                   %editc(b97ke2:'X')
011100121219     c                   eval      v3ddescr=b97descr
011200121217     c
011300121217     c     ktbe          setll     tntbe01l
011400121217     c     ktbe          reade(n)  tntbe01l
011500110112     c
011600110112    1c                   dow       not %eof(tntbe01l)
011700121217     c                   movel     tbeuni        dKPV
011800121219     c                   setoff                                       11
011900110112     c
012000110810     c                   clear                   v3hvaru
012100121219     c                   movel     tbeke2        v3cksc
012200121219     c                   movel     tbeke2        v3hksc
012300110112     c* Chain su cnaco per la ragione sociale
012400110112    2c                   if        v3cksc>*zeros
012500110708     c                   clear                   v3dksc
012600110708     c                   clear                   v3cuni
012700110708     c                   exsr      Decodi
012800110810     c
012900110810     c* Se cambiato codice padre --> evidenzio in rosso
013000121219     c                   if        §kpvksu >*zeros
013100121219     c                   eval      wunif=%int(§kpvksu)
013200110810     c                   else
013300110810     c                   clear                   wunif
013400110810     c                   endif
013500110810     c
013600110810    3c                   if        wunif  >0 and wunif  <>v3cuni
013700110810     c                   eval      v3hvaru='S'
013800110810     c                   eval      %subst(v3duni:13:13)=' old->'+
013900110810     c                                                   %editc(wunif:'X')
014000110810     c                   seton                                        11
014100110810    3c                   endif
014200110810    2c                   endif
014300110112     c*
014400110112     c                   add       1             nrr1
014500121217     c                   write     tb97s03
014600110112     c
014700121217     c     ktbe          reade(n)  tntbe01l
014800110708    1c                   enddo
014900110112     c
015000110112     c                   exsr      righevuote
015100110812     c* La prima volta mi posizione in prima videata
015200110812     c                   z-add     1             c01csr
015300110112     c
015400110112     c                   ENDSR
015500110112      *-----------------------------------------------------***********
015600110120     c*  Carico righe vuote del sfl
015700110112      *-----------------------------------------------------***********
015800110112     c     Righevuote    BEGSR
015900110810     c                   setoff                                       11
016000110112     c* Carico le righe vuote fino a riempimento di pagina
016100110112    1c                   if        nrr1>=c_sflpag
016200110112     c                   eval      wpag=%div(nrr1:c_sflPag)
016300110112     c                   eval      wresto=%rem(nrr1:c_sflPag)
016400110112    2c                   if        wresto>0
016500110112     c                   eval      c01csr=nrr1
016600110112     c                   else
016700110112     c                   eval      c01csr=(wpag*c_sflpag)+1
016800110112    2c                   endif
016900110112     c
017000110112   x1c                   else
017100110112     c                   clear                   wpag
017200110112     c                   eval      wresto=nrr1
017300110112     c                   eval      c01csr=nrr1
017400110112    2c                   if        c01csr=0
017500110112     c                   eval      c01csr=1
017600110112    2c                   endif
017700110112    1c                   endif
017800110112     C
017900110112     C                   DOW       wresto<c_sflpag
018000110112     c                   clear                   v3cksc
018100110809     c                   clear                   v3hksc
018200110112     c                   clear                   v3dksc
018300110708     c                   clear                   v3cuni
018400110708     c                   clear                   v3duni
018500110810     c                   clear                   v3hvarU
018600110112     c*
018700110112     c                   eval      wresto=wresto+1
018800110112     c                   add       1             nrr1
018900121217     c                   write     tb97s03
019000110112     c                   enddo
019100110112     c
019200110112     c                   z-add     nrr1          totnrr
019300110112     c                   seton                                        33
019400110112     c                   ENDSR
019500110112      *-----------------------------------------------------***********
019600110112     c*  Gestione SFL
019700110112      *-----------------------------------------------------***********
019800110112     c     Gessfl        BEGSR
019900110112     c
020000110114     c                   setoff                                       28
020100110112     c
020200110112     c     for02         tag
020300110114     c                   eval      c01rcd=c01csr
020400110114     c
020500121217     c  n28              write     tb97z03
020600121217     c                   exfmt     tb97c03
020700110112     C                   SETOFF                                       289001
020800110112     C                   SETOFF                                       29
020900110112     c                   clear                   v1cmsg
021000110112     C* F3 - FINE
021100110812    1c                   if        *inkc
021200110809     c                   exsr      ContrTAB
021300110812     c
021400110812     c* Errore --> modifiche wffettuate finestra di avviso
021500110812    2c                   if        *in90
021600110812     c                   setoff                                       kc
021700110812    3c                   dow       not *inkc and not *inkf
021800121217     c                   exfmt     tb97w07
021900110812    3c                   enddo
022000110812     c
022100110812    2c                   endif
022200110809     c
022300110812     c* Premuto o ripremuto f3
022400110812    2c                   if        *inkc
022500110809     C                   eval      $fine='1'
022600110809     c                   leavesr
022700110812    2c                   endif
022800110812    1c                   endif
022900110812     c
023000110708     C* F7 - ricerca per cliente unificante
023100110708     c                   if        *inkg
023200110708     c                   exsr      RicercaUnif
023300110127     c                   seton                                        90
023400110127     c                   goto      for02
023500110127     c                   endif
023600110708     c
023700110708     C* F10- Inserimento riga vuota
023800110708     c                   if        *inkj
023900110708     c                   exsr      InserRiga
024000110708     c                   seton                                        90
024100110708     c                   goto      for02
024200110708     c                   endif
024300110120     c
024400110120     c* ROLLUP, solo se ultima riga piena
024500110120     c                   if        *in37
024600121217     c     totnrr        chain     tb97s03
024700110120     c                   if        %found and  v3cksc>=*zeros
024800110120     c                   eval      nrr1=totnrr
024900110120     c                   exsr      righevuote
025000110120     c                   else
025100110120     c                   seton                                        289029
025200110120     c                   eval      v1cmsg=msg(4)
025300110120     c                   exsr      Aggiosfl
025400110120     c                   endif
025500110120     c                   goto      for02
025600110120     c                   endif
025700110112     c*
025800110112     c                   exsr      ctrd02
025900110112     c
026000110812     c                   if        *in90 or (not *inkf  and not *inkb and
026100110812     c                                       not *inkk)
026200110112     c                   goto      for02
026300110112     c                   endif
026400110810     c
026500110810     c* F2 - controlla unificanti
026600110810     c                   if        *inkb
026700110810     c                   exsr      Gessfl05
026800110810     c                   goto      for02
026900110810     c                   endif
027000110812     c* F6 o F11 - conferma
027100110812     c                   if        *inkf  or *inkk
027200110112     c                   exsr      ScriviTAB
027300110112     c                   endif
027400110812     c* F11 - Stampa
027500110812     c                   if        *inkk
027600110812     c                   exsr      StampaTab
027700110812     c                   endif
027800110112     c
027900110112     c                   ENDSR
028000110708      *-----------------------------------------------------***********
028100110708      * ricerca ed inserimento per cliente unificante
028200110708      *-----------------------------------------------------***********
028300110708     C     RicercaUnif   BEGSR
028400110708     c                   clear                   xclirds
028500110708     c                   eval      dxccap=dutkci
028600110708     c                   eval      dxcaut='AZ'
028700110708     c                   call      'XCLIR'
028800110708     c                   parm                    xclirds
028900110708     c
029000110708    1c                   if        dxcf03=*blanks and dxcerr=*blanks
029100110708
029200110708     C                   MOVEA     DXCKSC        xKSA                            *SCOMATTA CODICI
029300110708DO  2C     1             DO        30            xx
029400110708     C                   MOVE      xKSA(xx)      DSKSA                          *SCH ALF -> DS ALF
029500110708     C                   Z-ADD     DSKSN         xKSC(xx)                        *DS NUM -> SCH NUM
029600110708E   2C                   ENDDO
029700110708     c
029800110708    2c                   if        xksc(1)>*zeros
029900121217     c                   write     tb97z03
030000121217     c                   write     tb97c03
030100110708     c                   clear                   w4cconf
030200110803     c
030300110810     c                   dow       w4cconf='  '    and not *inkl
030400121217     c                   exfmt     tb97w04
030500110803     c                   enddo
030600110708     c
030700110810    3c                   if        w4cconf='SI'
030800110708     c* cerco la prima posizione vuota
030900110708     c                   eval      nrr1=totnrr+1
031000110708     c                   clear                   v3cksc
031100110812     c                   clear                   wPrimo            1
031200110708    4c                   dow       nrr1>0 and v3cksc<=*zeros
031300110708     c                   eval      nrr1=nrr1-1
031400121217     c     nrr1          chain     tb97s03
031500110708    4c                   enddo
031600110708     c
031700110708     c* trovato il primo record vuoto--> aggiungo da li in poi
031800110708     c                   eval      xx=1
031900110708    4c                   dow       xksc(xx)>*zeros
032000110708     c* Per ogni codice, carico e suoi figli
032100110708     c                   clear                   tibs10ds
032200110708     c                   eval      d10tle='ST'
032300110708     c                   eval      d10cod=xksc(xx)
032400110708     c                   eval      d10paf='F'
032500110708     c                   call      'TIBS10R'
032600110708     c                   parm                    tibs10ds
032700110708     c                   parm      'R'           d10ela            1
032800110708     c
032900110708     c                   if        d10err<>*blanks
033000110708     c* se non trovato cerco come Figlio
033100110708     c                   clear                   skc
033200110708     c                   eval      d10paf='P'
033300110708     c                   call      'TIBS10R'
033400110708     c                   parm                    tibs10ds
033500110708     c                   parm      'R'           d10ela            1
033600110708     c
033700110708     c
033800110708     c* se non ho trovato nulla --> imposto se stesso   padre=figlio
033900110708     c                   if        d10err<>*blanks
034000110708     c                   movel     d10cod        d10cop
034100110708     c                   movel     d10cod        skc(1)
034200110708     c                   endif
034300110708     c                   endif
034400110708     c
034500110708     c                   z-add     1             zz
034600110708    5c                   dow       skc(zz)>*zeros
034700110708     c                   eval      kksc=%int(%subst(skc(zz):5:7))
034800110708     c* carico solo se non già presente
034900110708     c                   eval      Indx=%lookup(kksc:ksc)
035000110708     c
035100110708   5ac                   if        Indx=0
035200110708     c                   eval      nrr1=nrr1+1
035300110708     c
035400110809     c* se tutte le righe scritte sono piene--> aggiungo una pagina
035500110708    6c                   if        nrr1>totnrr
035600110708     c                   eval      wnrr =nrr1
035700110708     c                   eval      nrr1=totnrr
035800110708     c
035900110708     c                   exsr      righevuote
036000110708     c                   eval      nrr1=wnrr
036100110708    6c                   endif
036200110708     c
036300121217     c     nrr1          chain     tb97s03
036400110708    6c                   if        %found
036500110708     c                   movel     kksc          v3cksc
036600110708     c                   clear                   v3dksc
036700110708     c                   seton                                        29
036800110708     c                   eval      v3dksc='--> INSERITO'
036900110708     c                   move      d10cop        v3cuni
037000110708     c                   exsr      Decodi
037100110708     c                   exsr      Aggiosfl
037200110812     c
037300110812     c* Posizionamento sul primo record inserito
037400110812     c                   if        Wprimo=' '
037500110812     c                   eval      c01csr=nrr1
037600110812     c                   eval      Wprimo='S'
037700110812     c                   endif
037800110812     c
037900110708    6c                   endif
038000110708   5ac                   endif
038100110708
038200110708     c                   eval      zz=zz+1
038300110708    5c                   enddo
038400110708
038500110708     c                   eval      xx=xx+1
038600110708    4c                   enddo
038700110708     c
038800110708    3c                   endif
038900110708    2c                   endif
039000110708    1c                   endif
039100110708     c*
039200110708     c                   ENDSR
039300110112      *-----------------------------------------------------***********
039400110127      * Inserimento riga vuota
039500110112      *-----------------------------------------------------***********
039600110127     C     InserRiga     BEGSR
039700110127     c
039800110127     c* Se ultima riga piena, aggiungo una pagina, poi sposto
039900110127     c                   eval      savcsr=c01csr
040000110127     c
040100121217     c     totnrr        chain     tb97s03
040200110127    1c                   if        %found and  v3cksc>=*zeros
040300110127     c                   eval      nrr1=totnrr
040400110127     c                   exsr      righevuote
040500110127    1c                   endif
040600110127
040700110809     c* sposto la penultima nell'ultima e così via, fino al posizionamento record
040800110127     c                   eval      nrr1=totnrr-1
040900110127     c
041000110127    1c                   dow       nrr1>savcsr
041100110127     c
041200121217     c     nrr1          chain     tb97s03
041300110127    2c                   if        %found
041400110809     c                   movel     v3hksc        savhksc
041500110809     c                   movel     v3cksc        savksc
041600110127     c                   movel     v3dksc        savdksc
041700110708     c                   movel     v3cuni        savuni
041800110708     c                   movel     v3duni        savduni
041900110127     c                   eval      wnrr=nrr1+1
042000121217     c     wnrr          chain     tb97s03
042100110809     c                   clear                   v3hksc
042200110809     c                   clear                   v3cksc
042300110127     c                   clear                   v3dksc
042400110708     c                   clear                   v3cuni
042500110708     c                   clear                   v3duni
042600110809     c                   eval      v3hksc=savhksc
042700110809     c                   eval      v3cksc=savksc
042800110127     c                   eval      v3dksc=savdksc
042900110708     c                   eval      v3cuni=savuni
043000110708     c                   eval      v3duni=savduni
043100110127     c                   exsr      Aggiosfl
043200110127    2c                   endif
043300110127     c
043400110127     c                   eval      nrr1=nrr1-2
043500110127     c                   enddo
043600110127     c
043700110127     c* clearo la riga sotto al F10
043800110127     c                   eval      wnrr=nrr1+1
043900121217     c     wnrr          chain     tb97s03
044000110127     c                   clear                   v3cksc
044100110809     c                   clear                   v3hksc
044200110127     c                   clear                   v3dksc
044300110708     c                   clear                   v3cuni
044400110708     c                   clear                   v3duni
044500110127     c                   exsr      Aggiosfl
044600110127     c
044700110127     c                   eval      c01csr=savcsr
044800110127     c                   ENDSR
044900110127     c
045000110127      *-----------------------------------------------------***********
045100110127      * controllo dati sfl
045200110127      *-----------------------------------------------------***********
045300110127     C     ctrd02        BEGSR
045400110809     c                   clear                   forzavar
045500110112     c*
045600110112     c                   eval      nrr1=1
045700110112     c                   z-add     1             yy
045800110809     c                   z-add     1             zz
045900110112     c                   clear                   ksc
046000110112     c*
046100110708    0c                   dow       nrr1<=totnrr
046200110120     c
046300121217     c     nrr1          chain     tb97s03
046400110810   0ac                   if        %found
046500110112     C*
046600110112     c     '?'           scan      v3cksc                                 90
046700110112    1c                   if        *in90
046800110112     c                   movel     rsut          paxdut           30
046900110112     c                   movel(p)  v3dksc        paxdmt           48
047000110112      * PAXSTA=9 ESCLUDO ANNULLATI
047100110112     C                   Z-ADD     9             PAXSTA
047200110112     c                   movel     dutkci        paxccc            4 0
047300110112     c                   clear                   paxdit
047400110112     C                   z-add     1             paxnum
047500110112     C                   CALL      'XALFA3BR'
047600110112     C                   PARM                    PAXDUT
047700110112     C                   PARM      1             CODUT             1 0
047800110112     C                   PARM                    PAXDMT
047900110112     C                   PARM                    PAXCCC
048000110112     C                   PARM                    PAXSTA            1 0
048100110112     C                   PARM                    PAXFLR           90
048200110112     C                   PARM                    PAXDIT            3
048300110112     C                   PARM                    PAXNUM            2 0
048400110112     C                   PARM                    PAXKCM           80
048500110112     C                   PARM                    PAXKSM          140
048600110112     C                   PARM                    PAXKDM           60
048700110112    2C     PAXSTA        IFGT      -1
048800110112     C                   MOVEL     paxksm        v3cksc
048900110112     C                   MOVEL     paxdmt        v3dksc
049000110803     c                   else
049100110803     c                   clear                   v3cksc
049200110112     c                   endif
049300110112    2c                   exsr      Aggiosfl
049400110112     c                   leavesr
049500110112    1c                   endif
049600110112     c
049700110112    1c                   if        v3cksc<>*blanks and v3cksc<>*zeros
049800110120     c                   testn                   v3cksc               35
049900110112     c
050000110120    2c                   if        not *in35  or %subst(v3cksc:7:1)<'0'
050100110114     c                   seton                                        29
050200110112     c                   eval      v1cmsg=msg(2)
050300110112     c                   exsr      Aggiosfl
050400110112     c                   leavesr
050500110112    2c                   endif
050600110112     c
050700110112     c                   movel     v3cksc        kksc
050800110112     c     kaco          chain     cnaco00f
050900110112    2c                   if        %found(cnaco00f)
051000110112     c                   movel     acorag        v3dksc
051100110112   x2c                   else
051200110120     c                   seton                                        29
051300110112     c                   eval      v1cmsg=msg(2)
051400110112     c                   exsr      Aggiosfl
051500110112     c                   leavesr
051600110112    2c                   endif
051700110112     c
051800110112     c*  Verifico se già presente
051900110114     c     kksc          lookup    ksc                                    29
052000110708    2c                   if        *in29
052100110112     c                   eval      v1cmsg=msg(3)
052200110112     c                   exsr      Aggiosfl
052300110112     c                   leavesr
052400110112     c                   else
052500110112     c                   eval      ksc(yy)=kksc
052600110112     c                   eval      yy=yy+1
052700110809     c
052800110809     c* Memorizzo tutti gli unificanti per eventuale controllo
052900110810     c     v3cuni        lookup    unif                                   35
053000110810    2c                   if        not *in35
053100110810     c                   eval      unif(zz) =v3cuni
053200110810     c                   eval      unifd(zz)=v3duni
053300110809     c                   eval      zz=zz+1
053400110809    2c                   endif
053500110809     c
053600110112    2c                   endif
053700110810     c
053800110810     c                   if        v3cksc<>v3hksc
053900110708     c                   clear                   v3cuni
054000110810     c                   clear                   v3hvaru
054100110708     c                   exsr      Decodi
054200110810     c*
054300110810     c                   else
054400110810     c* se uguale do msg forzanbile per cambio codic unificante
054500110810     c                   if        v3hvaru='S'
054600110810     c                   seton                                        29
054700110810     c                   eval      v1cmsg=msg(8)
054800110810     c                   eval      v3hvaru='X'
054900110810     c                   exsr      Aggiosfl
055000110810     c                   leavesr
055100110810    2c                   endif
055200110810     c                   endif
055300110114     c*
055400110114     c                   exsr      Aggiosfl
055500110112     c*
055600110708   x1c                   else
055700110112     c                   clear                   v3dksc
055800110708     c                   clear                   v3duni
055900110708     c                   clear                   v3cuni
056000110112     c                   exsr      Aggiosfl
056100110112    1c                   endif
056200110120     c
056300110708   0ac                   endif
056400110112     c
056500110112     c                   eval      nrr1=1+nrr1
056600110112     c*
056700110708    0c                   enddo
056800110120     c                   z-add     1             nrr1
056900110112     c
057000110112     c                   ENDSR
057100110112      *-----------------------------------------------------***********
057200110112      * Aggiornamento tabella
057300110112      *-----------------------------------------------------***********
057400110112     C     ScriviTAB     BEGSR
057500110112     c*
057600110112     c* Intanto cancello tutti i record
057700110112     c
057800110112     c/exec sql
057900121219     c+ delete from tntbe00f where tbecod='KPV' and tbeke1= :kkey1
058000110112     c/end-exec sql
058100110112     c
058200110112     c
058300110112     c                   eval      nrr1=1
058400110112     c                   z-add     1             yy
058500110112     c                   clear                   ksc
058600110112     c*
058700121217     c     nrr1          chain     tb97s03
058800110112    0c                   dow       %found
058900110112     c
059000110112    1c                   if        v3cksc>*zeros
059100110112     c                   clear                   tntbe000
059200121217     c                   eval      tbecod='KPV'
059300121219     c                   eval      tbeapl='OP'
059400121219     c                   eval      tbeke1=kkey1
059500121219     c                   eval      tbeke2=v3cksc
059600121217     c                   clear                   dKPV
059700121217     c                   movel     v3dksc        §KPVrsc
059800121219     c                   movel     v3duni        §KPVRsu
059900121219     c                   movel     v3cuni        §KPVksu
060000121219     c                   movel     dKPV          tbeuni
060100110112     c                   write     tntbe000
060200110114     c                   eval      yy=yy+1
060300110112     c                   endif
060400110809     c
060500110809     c                   eval      v3hksc=v3cksc
060600110809     c                   exsr      AGGIOSFL
060700110112     c
060800110112     c                   eval      nrr1=nrr1+1
060900121217     c     nrr1          chain     tb97s03
061000110112     c                   enddo
061100110112     c                   ENDSR
061200110809      *-----------------------------------------------------***********
061300110809      * controllo tutto il sfl per F3 per vedere se ci sono dei record
061400110809      *  variati che verranno persi
061500110809      *-----------------------------------------------------***********
061600110809     C     ContrTAB      BEGSR
061700110809     c
061800110809     c                   eval      nrr1=1
061900110809     c                   z-add     1             yy
062000110809     c*
062100121217     c     nrr1          chain     tb97s03
062200110809    0c                   dow       %found
062300110809     c
062400110809     c                   if        v3cksc=*zeros
062500110809     c                   clear                   v3cksc
062600110809     c                   endif
062700110809     c                   if        v3hksc=*zeros
062800110809     c                   clear                   v3hksc
062900110809     c                   endif
063000110809     c
063100110809     c                   if        v3cksc<>v3hksc
063200110812     c                   seton                                        90
063300110809     c                   eval      Forzavar='S'
063400110809     c                   leavesr
063500110809     c                   endif
063600110809     c
063700110809     c                   eval      nrr1=nrr1+1
063800121217     c     nrr1          chain     tb97s03
063900110809     c                   enddo
064000110809     c                   ENDSR
064100110810      *-----------------------------------------------------***********
064200110810     c*  Gestione SFL 05 - controlla unificanti
064300110810      *-----------------------------------------------------***********
064400110810     c     Gessfl05      BEGSR
064500110810     c                   exsr      contrUnif
064600110810     c
064700110810     c                   setoff                                       28
064800110810     c* se non ho caricato nemmeno un record videata vuota
064900110810     c                   if        nrr5=0
065000110810     c                   seton                                        30
065100121217     c                   write     tb97c06
065200121217     c                   exfmt     tb97c05
065300110810     c
065400110810     c                   setoff                                       30
065500110810     c
065600110810     c                   else
065700110810     c
065800110810     c     for05         tag
065900110810     c                   eval      c05rcd=c05csr
066000110810     c
066100121217     c  n28              write     tb97z05
066200110810     c*
066300121217     c                   exfmt     tb97c05
066400110810     C                   SETOFF                                       2890
066500110810     C                   SETOFF                                       40
066600110810     c                   clear                   v1cmsg
066700110810     C* F12- ritrono
066800110810     c                   if        *inkl
066900110810     c                   leavesr
067000110810     c                   endif
067100110810     c
067200110810     c                   exsr      ctrd05
067300110810     c
067400110810     c                   if        *in90 or not *inkf
067500110810     c                   goto      for05
067600110810     c                   endif
067700110810     c* F6 - conferma
067800110810     c                   if        *inkf
067900110810     c                   exsr      ScriviTAB05
068000110810     c                   endif
068100110810     c
068200110810     c                   endif
068300110810     c
068400110810     c                   ENDSR
068500110809      *-----------------------------------------------------***********
068600110809      * Controllo i codici unificanti se per caso sono variati i legami
068700110809      *  li evidenzio
068800110809      *-----------------------------------------------------***********
068900110809     C     ContrUNIF     BEGSR
069000110809     c* Elaboro la schiera degli unifficanti e controllo rispoetto a
069100110809     c*  quelli inseriti in tabella se ce ne sono in più o meno
069200110809     C* PULIZIA SFL
069300110809     C                   SETON                                        3031
069400121217     C                   WRITE     tb97C05
069500110809     C                   SETOFF                                       3031
069600110809     c                   clear                   nrr5
069700110809     c                   clear                   totnrr5           5 0
069800110809     c
069900110809     c                   z-add     1             xx
070000110809    1c                   dow       unif(xx)>0
070100110809     c                   clear                   tibs10ds
070200110809     c                   eval      d10tle='ST'
070300110809     c                   eval      d10cod=unif(xx)
070400110809     c                   eval      d10paf='F'
070500110809     c                   call      'TIBS10R'
070600110809     c                   parm                    tibs10ds
070700110809     c                   parm      'R'           d10ela            1
070800110809     c
070900110809    2c                   if        d10err<>*blanks
071000110809     c* se non trovato cerco come Figlio
071100110809     c                   clear                   skc
071200110809     c                   eval      d10paf='P'
071300110809     c                   call      'TIBS10R'
071400110809     c                   parm                    tibs10ds
071500110809     c                   parm      'R'           d10ela            1
071600110809     c
071700110809     c
071800110809     c* se non ho trovato nulla --> imposto se stesso   padre=figlio
071900110809    3c                   if        d10err<>*blanks
072000110809     c                   movel     d10cod        d10cop
072100110809     c                   movel     d10cod        skc(1)
072200110809    3c                   endif
072300110809    2c                   endif
072400110809     c
072500110809     c                   z-add     1             zz
072600110809    2c                   dow       skc(zz)>*zeros
072700110809     c                   eval      kksc=%int(%subst(skc(zz):5:7))
072800110809     c
072900110809     c* Verifico se già presente nella tabella
073000110810     c     kksc          lookup    ksc                                    35
073100110810    3c                   if        not *in35
073200110810     c                   setoff                                       10
073300110810     c                   eval      v5dmod='MANCANTE '
073400110809     c                   eval      v5cuni=unif(xx)
073500110810     c                   eval      v5duni=unifd(xx)
073600110809     c                   eval      v5cksc=kksc
073700110809     c     kaco          chain     cnaco00f
073800110809    4c                   if        %found(cnaco00f)
073900110809     c                   movel     acorag        v5dksc
074000110809    4c                   endif
074100110809     c                   add       1             nrr5
074200121217     c                   write     tb97s05
074300110809    3c                   endif
074400110809     c
074500110809     C                   EVAL      zz=zz+1
074600110809    2c                   enddo
074700110809     c
074800110809     C                   EVAL      xx=xx+1
074900110809    1c                   enddo
075000110809     c
075100110810     c                   eval      c05csr=1
075200110810     c                   z-add     nrr5          totnrr5
075300110809     c                   ENDSR
075400110810      *-----------------------------------------------------***********
075500110810      * controllo dati sfl 05
075600110810      *-----------------------------------------------------***********
075700110810     C     ctrd05        BEGSR
075800110810     c*
075900110810     c                   eval      nrr5=1
076000110810     c                   z-add     1             yy
076100110810     c                   z-add     1             zz
076200110810     c                   clear                   wopz
076300110810     c*
076400110810    0c                   dow       nrr5<=totnrr5
076500110810     c
076600121217     c     nrr5          chain     tb97s05
076700110810    1c                   if        %found
076800110810     c* Per aggiunta solo "I"
076900110810    2c                   if        v5copz='I' and %subst(v5dmod:1:1)='E'
077000110810     c                   seton                                        40
077100110810     c                   eval      v1cmsg=msg(6)
077200110810     c                   exsr      Aggiosfl5
077300110810     c                   leavesr
077400110810    2c                   endif
077500110810     c* Per eliminaz. solo "A"
077600110810    2c                   if        v5copz='A' and %subst(v5dmod:1:1)='A'
077700110810     c                   seton                                        40
077800110810     c                   eval      v1cmsg=msg(1)
077900110810     c                   exsr      Aggiosfl5
078000110810     c                   leavesr
078100110810    2c                   endif
078200110810     c
078300110810     c* se premuto F6 ci deve essere almeno una "I" o una "A"
078400110810    2c                   if        v5copz<>*blanks
078500110810     c                   eval      wopz=v5copz
078600110810    2c                   endif
078700110810     c
078800110810    1c                   endif
078900110810     c
079000110810     c                   eval      nrr5=1+nrr5
079100110810     c*
079200110810    0c                   enddo
079300110810     c
079400110810     c* se premuto F6 ci deve essere almeno una "I" o una "A"
079500110810     c   kf              if        wopz=' '
079600110810     c                   z-add     1             nrr5
079700110810     c                   seton                                        40
079800110810     c                   eval      v1cmsg=msg(7)
079900110810     c                   exsr      Aggiosfl5
080000110810    2c                   endif
080100110810     c
080200110810     c                   ENDSR
080300110810      *-----------------------------------------------------***********
080400121219      * Aggiunta/eliminazione recod da sfl
080500110810      *-----------------------------------------------------***********
080600110810     C     ScriviTAB05   BEGSR
080700110810     c
080800110810     c                   eval      nrr5=1
080900110810     c                   z-add     1             yy
081000110810     c                   clear                   ksc
081100110812     c                   clear                   wPrimo
081200110810     c*
081300121217     c     nrr5          chain     tb97s05
081400110810    0c                   dow       %found
081500110810     c
081600110810    1c                   if        v5copz='I'
081700110810     c
081800110810     c* cerco la prima posizione vuota
081900110810     c                   eval      nrr1=totnrr+1
082000110810     c                   clear                   v3cksc
082100110810    2c                   dow       nrr1>0 and v3cksc<=*zeros
082200110810     c                   eval      nrr1=nrr1-1
082300121217     c     nrr1          chain     tb97s03
082400110810    2c                   enddo
082500110810     c
082600110810     c                   eval      nrr1=nrr1+1
082700110810     c
082800110810     c* se tutte le righe scritte sono piene--> aggiungo una pagina
082900110810    2c                   if        nrr1>totnrr
083000110810     c                   eval      wnrr =nrr1
083100110810     c                   eval      nrr1=totnrr
083200110810     c
083300110810     c                   exsr      righevuote
083400110810     c                   eval      nrr1=wnrr
083500110810    2c                   endif
083600110810     c
083700121217     c     nrr1          chain     tb97s03
083800110810    2c                   if        %found
083900110810     c                   movel     v5cksc        v3cksc
084000110810     c                   movel     v5dksc        v3dksc
084100110810     c                   seton                                        29
084200110810     c                   eval      v3dksc='--> INSERITO'
084300110810     c                   move      v5cuni        v3cuni
084400110810     c                   move      v5duni        v3duni
084500110810     c                   exsr      Aggiosfl
084600110812     c* Posizionamento sul primo record inserito
084700110812     c                   if        Wprimo=' '
084800110812     c                   eval      c01csr=nrr1
084900110812     c                   eval      Wprimo='S'
085000110812     c                   endif
085100110812     c
085200110810    2c                   endif
085300110810    1c                   endif
085400110810     c
085500110810     c                   eval      nrr5=nrr5+1
085600121217     c     nrr5          chain     tb97s05
085700110810     c                   enddo
085800110810     c
085900110810     c                   ENDSR
086000110112      *****************************************************************
086100110112     c* Aggioranemnto sfl
086200110112      *****************************************************************
086300110112     C     Aggiosfl      BEGSR
086400110112     c                   if        v1cmsg<>*blanks
086500110112     c                   seton                                        9028
086600110112     c                   eval      c01csr=nrr1
086700110112     c                   endif
086800110810     c*
086900110810     c                   if        v3hvaru<>*blanks
087000110810     c                   seton                                        11
087100110810     c                   else
087200110810     c                   setoff                                       11
087300110810     c                   endif
087400110112     c
087500121217     c                   update    tb97s03
087600110810     c                   setoff                                       29
087700110112     c
087800110112     c                   ENDSR
087900110810      *****************************************************************
088000110810     c* Aggioranemnto sfl5
088100110810      *****************************************************************
088200110810     C     Aggiosfl5     BEGSR
088300110810     c                   if        v1cmsg<>*blanks
088400110810     c                   seton                                        9028
088500110810     c                   eval      c05csr=nrr5
088600110810     c                   endif
088700110810     c                   if        %subst(v5dmod:1:1)='E'
088800110810     c                   seton                                        10
088900110810     c                   else
089000110810     c                   setoff                                       10
089100110810     c                   endif
089200110810     c
089300121217     c                   update    tb97s05
089400110810     c                   setoff                                       40
089500110810     c
089600110810     c                   ENDSR
089700110708      *****************************************************************
089800110708     c     Decodi        BEGSR
089900110708    1c                   if        v3dksc=*blanks
090000110708     c                   movel     v3cksc        kksc
090100110708     c     kaco          chain     cnaco00f
090200110708    2c                   if        %found(cnaco00f)
090300110708     c                   movel     acorag        v3dksc
090400110708    2c                   endif
090500110708    1c                   endif
090600110708     c
090700110708     c* Reperisco codice Padre
090800110708    1c                   if        v3cuni=0
090900110708     c                   clear                   tibs10ds
091000110708     c                   eval      d10tle='ST'
091100110708     c                   eval      d10cod=acoksc
091200110708     c                   eval      d10paf='P'
091300110708     c                   call      'TIBS10R'
091400110708     c                   parm                    tibs10ds
091500110708     c                   parm      'R'           d10ela            1
091600110708     c
091700110708     c* Passato tipo elaborazione per non eseguire la chiusura file tutte le volte
091800110708     c*  (li chiude per " " o "C" )
091900110708    2c                   if        d10err=*blanks and d10cop>*zeros
092000110708     c                   move      d10cop        v3cuni
092100110708     c                   else
092200110708     c                   movel     v3cksc        v3cuni
092300110708    2c                   endif
092400110708    1c                   endif
092500110708     c
092600110708    1c                   if        v3cuni>*zeros
092700110708     c                   movel     v3cuni        kksc
092800110708     c     kaco          chain     cnaco00f
092900110708    2c                   if        %found(cnaco00f)
093000110708     c                   movel     acorag        v3duni
093100110812     c                   movel     acorag        P3duni
093200110708    2c                   endif
093300110708    1c                   endif
093400110708     c
093500110708     c                   ENDSR
093600110812      *-----------------------------------------------------***********
093700121217      * Stampa tabella "KPV"
093800110812      *-----------------------------------------------------***********
093900110812     C     StampaTab     BEGSR
094000110812     c                   seton                                        OF
094100110812     c
094200121219     c     ktbe          setll     tntbe01l
094300121219     c     ktbe          reade(n)  tntbe01l
094400110812     c
094500110812    1c                   dow       not %eof(tntbe01l)
094600110812     c
094700121219     c                   movel     tbeke2        v3cksc
094800121219     c                   movel     tbeuni        DKPV
094900110812     c* Chain su cnaco per la ragione sociale
095000110812    2c                   if        v3cksc>*zeros
095100110812     c                   clear                   V3dksc
095200110812     c                   clear                   v3cuni
095300121217     c                   movel     tbeuni        dKPV
095400110812     c                   exsr      Decodi
095500110812     c
095600110812     c* Se cambiato codice padre
095700121219     c                   if        §kpvksu >*zeros
095800121219     c                   eval      wunif=%int(§kpvksu)
095900110812     c                   else
096000110812     c                   clear                   wunif
096100110812     c                   endif
096200110812     c
096300110812    3c                   if        wunif  >0 and wunif  <>v3cuni
096400110812     c                   movel     wunif         kksc
096500110812     c     kaco          chain     cnaco00f
096600110812    2c                   if        not %found(cnaco00f)
096700110812     c                   clear                   acorag
096800110812    2c                   endif
096900110812     c
097000110812     c                   eval      %subst(P3duni:13:33)=' in tabella con-->'+
097100110812     c                                   %editc(wunif:'X')+ Acorag
097200110812     c                   seton                                        11
097300110812    3c                   endif
097400110812    2c                   endif
097500110812     c*
097600110812     C                   IF        *INOF
097700110812     C                   EXCEPT    TESTA
097800110812     C                   SETOFF                                       OF
097900110812     C                   ENDIF
098000110812     C
098100110812     C                   EXCEPT    DET
098200110812     c
098300121219     c     ktbe          reade(n)  tntbe01l
098400110812    1c                   enddo
098500110812     c
098600110812     c                   ENDSR
098700110112      *****************************************************************
098800110112     C     *INZSR        BEGSR
098900110112      *
099000110112     C     *ENTRY        PLIST
099100110112     C                   PARM                    KPJBA
099200121217     c                   movel     kpjbu         tntb97ds
099300110112      *
099400110112      *
099500110112      * accesso tntbe
099600110120     c     ktab          klist
099700110120     C                   kfld                    kkut
099800110120     C                   kfld                    kcodt
099900110120     C                   kfld                    kkey
100000110112     c     kaco          klist
100100110112     C                   kfld                    kkut
100200110112     C                   kfld                    dutkci
100300110112     C                   kfld                    kksc
100400121217     c     kTBE          klist
100500121217     C                   kfld                    kcod
100600121217     C                   kfld                    kkey1
100700110112     c
100800110112     c                   exsr      sr_DatiJob
100900110812     c
101000110812     C                   TIME                    WTIME            14 0
101100110812     C                   MOVE      WTIME         WDATE             8 0
101200110812     C                   MOVEL     WTIME         UTIME             6 0
101300110112      *
101400110112     C                   ENDSR
101500110112      *---------------------------------------------------------------*
101600110112      *?Reperimento dati del job (Utente/Operativi)                  ?*
101700110112      *---------------------------------------------------------------*
101800110112     c     sr_DatiJob    BEGSR
101900110112      *
102000110112     c     *dtaara       define    §azute        AZUTEds
102100110112     c     *dtaara       define    §datiute      dDatiUte
102200110112      *
102300110112     c                   in(E)     *dtaara
102400110112     c                   IF        %Error or RSUT = *blanks
102500110112     c                   clear                   TIBS34ds
102600110112     c                   call      'TIBS34R'
102700110112     c                   parm                    TIBS34ds
102800110112     c                   in        *dtaara
102900110112     c                   ENDIF
103000110112      *
103100110112     c                   ENDSR
103200110812     c*----------------------------------------------------------------------
103300110812     OQSYSPRT   E            TESTA            01
103400110812     O                       SK(1)               66
103500110812     O                       SK(2)              132
103600110812     O                       RSUT                20
103700110812     O                       WDATE              118 '  /  /    '
103800110812     O                       PAGE          Z    132
103900110812     O          E            TESTA            02
104000110812     O                       KNSIF               10
104100110812     O                       KNMUS               22
104200110812     O                       utimE              116 '  :  :  '
104300121219     O          E            TESTA            04
104400121219     O                                         +  1 'Raggruppamento:'
104500121219     O                       b97ke1            +  2
104600121219     o                                         +  1 '/'
104700121219     O                       b97ke2            +  1
104800121219     O                       b97descr          +  2
104900121219     o
105000121219     O          E            TESTA            06
105100110812     O                       SK(5)               66
105200110812     O                       SK(5)              132
105300110812     O*
105400121219     O          E            TESTA            07
105500110812     O                       SK(3)               66
105600110812     O                       SK(4)              132
105700110812     O*
105800121219     O          E            TESTA            08
105900110812     O                       SK(5)               66
106000110812     O                       SK(5)              132
106100110812     O
106200110812     O          E            DET         1
106300110812     O                       V3CUNI
106400110812     O                       P3DUNI           +   2
106500110812     O                       V3CKSC           +   4
106600110812     O                       V3DKSC           +   2
106700020805      **************************************************************************
106800960302** SCHIERA MSG - MESSAGGI DI ERRORE
106900110810Codice  AGGIUNTO  all'Unificante: possibile solo "I-inserimento"                1
107000110810Codice cliente errato o inesistente                                             2
107100110810Codice cliente già inserito                                                     3
107200110810Scorrimento non ammesso: ci sono ancora righe vuote nella pagina                4
107300110810ATTENZIONE alcune modifiche andranno perdute.F3 uscita senza conferma F6-Confer 5
107400110810Codice  ELIMINATO  dall'unificante: possibile solo "A-annullamento"             6
107500110810Uscire con F12 o effettuare almeno una scelta e premere F6                      7
107600110829VARIATO codice Unificante: verificare se eliminare il cod.cliente.ENTER forza   8
107700110812**
107800130115XXXXXXXXXXXXXXXXXXXX                     ** Raggruppamenti clienti   1
107900130115 **                           TNRB97R     XX.XX.XXXX     PAG. XXXX   2
108000110812Cliente Unificante attuale                                 Codice    3
108100110812cliente                                                              4
108200110812------------------------------------------------------------------   5
