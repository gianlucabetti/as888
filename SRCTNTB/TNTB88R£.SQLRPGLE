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
006400110120     D kcodt           S                   LIKE(tblcod) inz('01')
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
007800110127     D wnrr            S                   LIKE(nrr1)
007900110127     D savcsr          S                   LIKE(c01csr)
008000110114     d c_SflPag        c                   const(17)
008100110112     d wpag            s              6  0
008200110112     d wresto          s              3  0
008300110708     d zz              s              3  0
008400110708     d yy              s              3  0
008500110708     d XX              s              3  0
008600110708     d Indx            s              3  0
008700110112     d $Fine           s              1
008800110810     d Wopz            s              1
008900110809     d Forzavar        s              1
009000110810     d wunif           s              7  0
009100110812     d P3DUNI          s             45
009200121120     d P3Dterm         s             25
009300041220     d
009400050103     C**********************************************************************
009500110112     c
009600110112     c                   exsr      Carsfl
009700110112     c                   exsr      Gessfl
009800080529     c
009900960129     C*
010000000000     C                   SETON                                        LR
010100080529      *-----------------------------------------------------***********
010200110112      * Caricamento sfl
010300080529      *-----------------------------------------------------***********
010400110112     C     CarSFL        BEGSR
010500080529     C* PULIZIA SFL
010600080529     C                   SETON                                        3031
010700110708     C                   WRITE     tb88C03
010800110810     C                   SETOFF                                       3031
010900080529     c                   clear                   nrr1
011000080529     c                   clear                   totnrr            5 0
011100110809     c                   clear                   forzavar
011200110112     c
011300110708     c     kcod          setll     tntbe01l
011400110708     c     kcod          reade(n)  tntbe01l
011500110112     c
011600110112    1c                   dow       not %eof(tntbe01l)
011700110112     c
011800110810     c                   clear                   v3hvaru
011900110810     c                   setoff                                       11
012000110708     c                   movel     tbeke2        v3cksc
012100110809     c                   movel     tbeke2        v3hksc
012200110112     c* Chain su cnaco per la ragione sociale
012300110112    2c                   if        v3cksc>*zeros
012400110708     c                   clear                   v3dksc
012500110708     c                   clear                   v3cuni
012600110810     c                   movel     tbeuni        dCXQ
012700110708     c                   exsr      Decodi
012800110810     c
012900110810     c* Se cambiato codice padre --> evidenzio in rosso
013000121120    3c                   if        %subst(tbeke1:1:7)>*zeros
013100110810     c                   eval      wunif=%int(%subst(tbeke1:1:7))
013200110810     c                   else
013300110810     c                   clear                   wunif
013400121120    3c                   endif
013500110810     c
013600110810    3c                   if        wunif  >0 and wunif  <>v3cuni
013700110810     c                   eval      v3hvaru='S'
013800110810     c                   eval      %subst(v3duni:13:13)=' old->'+
013900110810     c                                                   %editc(wunif:'X')
014000110810     c                   seton                                        11
014100110810    3c                   endif
014200121120     c*  Terminal se impostato
014300121120    3c                   if        §cxqterm>*zeros
014400121120     c                   movel     §cxqterm      v3cterm
014500121120     c                   else
014600121123     c* se campo vuoto riempo da cod unificante in terminal di partenza
014700121123     c                   clear                   fnlv55ds
014800121123     c                   eval      d55tpt='P'
014900121123     c                   movel     v3cuni        d55lin
015000121123     c                   eval      d55drf=dateu
015100121123     c                   call      'FNLV55R'
015200121123     c                   parm                    fnlv55ds
015300121123     c*
015400121123     c                   movel     d55tfp        v3cterm
015500121120    3c                   endif
015600121120    2c                   endif
015700121120     c
015800110112     c                   add       1             nrr1
015900110708     c                   write     tb88s03
016000110112     c
016100110708     c     kcod          reade(n)  tntbe01l
016200110708    1c                   enddo
016300110112     c
016400110112     c                   exsr      righevuote
016500110812     c* La prima volta mi posizione in prima videata
016600110812     c                   z-add     1             c01csr
016700110112     c
016800110112     c                   ENDSR
016900110112      *-----------------------------------------------------***********
017000110120     c*  Carico righe vuote del sfl
017100110112      *-----------------------------------------------------***********
017200110112     c     Righevuote    BEGSR
017300110810     c                   setoff                                       11
017400110112     c* Carico le righe vuote fino a riempimento di pagina
017500110112    1c                   if        nrr1>=c_sflpag
017600110112     c                   eval      wpag=%div(nrr1:c_sflPag)
017700110112     c                   eval      wresto=%rem(nrr1:c_sflPag)
017800110112    2c                   if        wresto>0
017900110112     c                   eval      c01csr=nrr1
018000110112     c                   else
018100110112     c                   eval      c01csr=(wpag*c_sflpag)+1
018200110112    2c                   endif
018300110112     c
018400110112   x1c                   else
018500110112     c                   clear                   wpag
018600110112     c                   eval      wresto=nrr1
018700110112     c                   eval      c01csr=nrr1
018800110112    2c                   if        c01csr=0
018900110112     c                   eval      c01csr=1
019000110112    2c                   endif
019100110112    1c                   endif
019200110112     C
019300110112     C                   DOW       wresto<c_sflpag
019400110112     c                   clear                   v3cksc
019500110809     c                   clear                   v3hksc
019600110112     c                   clear                   v3dksc
019700110708     c                   clear                   v3cuni
019800110708     c                   clear                   v3duni
019900110810     c                   clear                   v3hvarU
020000121120     c                   clear                   v3cterm
020100121123     c                   clear                   v3hterm
020200110112     c*
020300110112     c                   eval      wresto=wresto+1
020400110112     c                   add       1             nrr1
020500110708     c                   write     tb88s03
020600110112     c                   enddo
020700110112     c
020800110112     c                   z-add     nrr1          totnrr
020900110112     c                   seton                                        33
021000110112     c                   ENDSR
021100110112      *-----------------------------------------------------***********
021200110112     c*  Gestione SFL
021300110112      *-----------------------------------------------------***********
021400110112     c     Gessfl        BEGSR
021500110112     c
021600110114     c                   setoff                                       28
021700110112     c
021800110112     c     for02         tag
021900110114     c                   eval      c01rcd=c01csr
022000110114     c
022100110708     c  n28              write     tb88z03
022200110708     c                   exfmt     tb88c03
022300110112     C                   SETOFF                                       289001
022400121120     C                   SETOFF                                       2941
022500110112     c                   clear                   v1cmsg
022600110112     C* F3 - FINE
022700110812    1c                   if        *inkc
022800110809     c                   exsr      ContrTAB
022900110812     c
023000110812     c* Errore --> modifiche wffettuate finestra di avviso
023100110812    2c                   if        *in90
023200110812     c                   setoff                                       kc
023300110812    3c                   dow       not *inkc and not *inkf
023400110812     c                   exfmt     tb88w07
023500110812    3c                   enddo
023600110812     c
023700110812    2c                   endif
023800110809     c
023900110812     c* Premuto o ripremuto f3
024000110812    2c                   if        *inkc
024100110809     C                   eval      $fine='1'
024200110809     c                   leavesr
024300110812    2c                   endif
024400110812    1c                   endif
024500110812     c
024600110708     C* F7 - ricerca per cliente unificante
024700110708     c                   if        *inkg
024800110708     c                   exsr      RicercaUnif
024900110127     c                   seton                                        90
025000110127     c                   goto      for02
025100110127     c                   endif
025200110708     c
025300110708     C* F10- Inserimento riga vuota
025400110708     c                   if        *inkj
025500110708     c                   exsr      InserRiga
025600110708     c                   seton                                        90
025700110708     c                   goto      for02
025800110708     c                   endif
025900110120     c
026000110120     c* ROLLUP, solo se ultima riga piena
026100110120     c                   if        *in37
026200110708     c     totnrr        chain     tb88s03
026300110120     c                   if        %found and  v3cksc>=*zeros
026400110120     c                   eval      nrr1=totnrr
026500110120     c                   exsr      righevuote
026600110120     c                   else
026700110120     c                   seton                                        289029
026800110120     c                   eval      v1cmsg=msg(4)
026900110120     c                   exsr      Aggiosfl
027000110120     c                   endif
027100110120     c                   goto      for02
027200110120     c                   endif
027300110112     c*
027400110112     c                   exsr      ctrd02
027500110112     c
027600110812     c                   if        *in90 or (not *inkf  and not *inkb and
027700110812     c                                       not *inkk)
027800110112     c                   goto      for02
027900110112     c                   endif
028000110810     c
028100110810     c* F2 - controlla unificanti
028200110810     c                   if        *inkb
028300110810     c                   exsr      Gessfl05
028400110810     c                   goto      for02
028500110810     c                   endif
028600110812     c* F6 o F11 - conferma
028700110812     c                   if        *inkf  or *inkk
028800110112     c                   exsr      ScriviTAB
028900110112     c                   endif
029000110812     c* F11 - Stampa
029100110812     c                   if        *inkk
029200110812     c                   exsr      StampaTab
029300110812     c                   endif
029400110112     c
029500110112     c                   ENDSR
029600110708      *-----------------------------------------------------***********
029700110708      * ricerca ed inserimento per cliente unificante
029800110708      *-----------------------------------------------------***********
029900110708     C     RicercaUnif   BEGSR
030000110708     c                   clear                   xclirds
030100110708     c                   eval      dxccap=dutkci
030200110708     c                   eval      dxcaut='AZ'
030300110708     c                   call      'XCLIR'
030400110708     c                   parm                    xclirds
030500110708     c
030600110708    1c                   if        dxcf03=*blanks and dxcerr=*blanks
030700110708
030800110708     C                   MOVEA     DXCKSC        xKSA                            *SCOMATTA CODICI
030900110708DO  2C     1             DO        30            xx
031000110708     C                   MOVE      xKSA(xx)      DSKSA                          *SCH ALF -> DS ALF
031100110708     C                   Z-ADD     DSKSN         xKSC(xx)                        *DS NUM -> SCH NUM
031200110708E   2C                   ENDDO
031300110708     c
031400110708    2c                   if        xksc(1)>*zeros
031500110708     c                   write     tb88z03
031600110708     c                   write     tb88c03
031700110708     c                   clear                   w4cconf
031800110803     c
031900110810     c                   dow       w4cconf='  '    and not *inkl
032000110708     c                   exfmt     tb88w04
032100110803     c                   enddo
032200110708     c
032300110810    3c                   if        w4cconf='SI'
032400110708     c* cerco la prima posizione vuota
032500110708     c                   eval      nrr1=totnrr+1
032600110708     c                   clear                   v3cksc
032700110812     c                   clear                   wPrimo            1
032800110708    4c                   dow       nrr1>0 and v3cksc<=*zeros
032900110708     c                   eval      nrr1=nrr1-1
033000110708     c     nrr1          chain     tb88s03
033100110708    4c                   enddo
033200110708     c
033300110708     c* trovato il primo record vuoto--> aggiungo da li in poi
033400110708     c                   eval      xx=1
033500110708    4c                   dow       xksc(xx)>*zeros
033600110708     c* Per ogni codice, carico e suoi figli
033700110708     c                   clear                   tibs10ds
033800110708     c                   eval      d10tle='ST'
033900110708     c                   eval      d10cod=xksc(xx)
034000110708     c                   eval      d10paf='F'
034100110708     c                   call      'TIBS10R'
034200110708     c                   parm                    tibs10ds
034300110708     c                   parm      'R'           d10ela            1
034400110708     c
034500110708     c                   if        d10err<>*blanks
034600110708     c* se non trovato cerco come Figlio
034700110708     c                   clear                   skc
034800110708     c                   eval      d10paf='P'
034900110708     c                   call      'TIBS10R'
035000110708     c                   parm                    tibs10ds
035100110708     c                   parm      'R'           d10ela            1
035200110708     c
035300110708     c
035400110708     c* se non ho trovato nulla --> imposto se stesso   padre=figlio
035500110708     c                   if        d10err<>*blanks
035600110708     c                   movel     d10cod        d10cop
035700110708     c                   movel     d10cod        skc(1)
035800110708     c                   endif
035900110708     c                   endif
036000110708     c
036100110708     c                   z-add     1             zz
036200110708    5c                   dow       skc(zz)>*zeros
036300110708     c                   eval      kksc=%int(%subst(skc(zz):5:7))
036400110708     c* carico solo se non già presente
036500110708     c                   eval      Indx=%lookup(kksc:ksc)
036600110708     c
036700110708   5ac                   if        Indx=0
036800110708     c                   eval      nrr1=nrr1+1
036900110708     c
037000110809     c* se tutte le righe scritte sono piene--> aggiungo una pagina
037100110708    6c                   if        nrr1>totnrr
037200110708     c                   eval      wnrr =nrr1
037300110708     c                   eval      nrr1=totnrr
037400110708     c
037500110708     c                   exsr      righevuote
037600110708     c                   eval      nrr1=wnrr
037700110708    6c                   endif
037800110708     c
037900110708     c     nrr1          chain     tb88s03
038000110708    6c                   if        %found
038100110708     c                   movel     kksc          v3cksc
038200110708     c                   clear                   v3dksc
038300110708     c                   seton                                        29
038400110708     c                   eval      v3dksc='--> INSERITO'
038500110708     c                   move      d10cop        v3cuni
038600121120     c                   if        v3cterm<=*zeros
038700121123     c                   clear                   fnlv55ds
038800121123     c                   eval      d55tpt='P'
038900121123     c                   eval      d55drf=dateu
039000121123     c                   movel     v3cuni        d55lin
039100121123     c                   call      'FNLV55R'
039200121123     c                   parm                    fnlv55ds
039300121123     c*
039400121123     c                   movel     d55tfp        v3cterm
039500121120     c                   endif
039600121120     c
039700110708     c                   exsr      Decodi
039800110708     c                   exsr      Aggiosfl
039900110812     c
040000110812     c* Posizionamento sul primo record inserito
040100110812     c                   if        Wprimo=' '
040200110812     c                   eval      c01csr=nrr1
040300110812     c                   eval      Wprimo='S'
040400110812     c                   endif
040500110812     c
040600110708    6c                   endif
040700110708   5ac                   endif
040800110708
040900110708     c                   eval      zz=zz+1
041000110708    5c                   enddo
041100110708
041200110708     c                   eval      xx=xx+1
041300110708    4c                   enddo
041400110708     c
041500110708    3c                   endif
041600110708    2c                   endif
041700110708    1c                   endif
041800110708     c*
041900110708     c                   ENDSR
042000110112      *-----------------------------------------------------***********
042100110127      * Inserimento riga vuota
042200110112      *-----------------------------------------------------***********
042300110127     C     InserRiga     BEGSR
042400110127     c
042500110127     c* Se ultima riga piena, aggiungo una pagina, poi sposto
042600110127     c                   eval      savcsr=c01csr
042700110127     c
042800110708     c     totnrr        chain     tb88s03
042900110127    1c                   if        %found and  v3cksc>=*zeros
043000110127     c                   eval      nrr1=totnrr
043100110127     c                   exsr      righevuote
043200110127    1c                   endif
043300110127
043400110809     c* sposto la penultima nell'ultima e così via, fino al posizionamento record
043500110127     c                   eval      nrr1=totnrr-1
043600110127     c
043700110127    1c                   dow       nrr1>savcsr
043800110127     c
043900110708     c     nrr1          chain     tb88s03
044000110127    2c                   if        %found
044100110809     c                   movel     v3hksc        savhksc
044200110809     c                   movel     v3cksc        savksc
044300110127     c                   movel     v3dksc        savdksc
044400110708     c                   movel     v3cuni        savuni
044500110708     c                   movel     v3duni        savduni
044600121120     c                   movel     v3cterm       savterm
044700121123     c                   movel     v3hterm       savhterm
044800110127     c                   eval      wnrr=nrr1+1
044900110708     c     wnrr          chain     tb88s03
045000110809     c                   clear                   v3hksc
045100110809     c                   clear                   v3cksc
045200110127     c                   clear                   v3dksc
045300110708     c                   clear                   v3cuni
045400110708     c                   clear                   v3duni
045500121120     c                   clear                   v3cterm
045600121123     c                   clear                   v3hterm
045700110809     c                   eval      v3hksc=savhksc
045800110809     c                   eval      v3cksc=savksc
045900110127     c                   eval      v3dksc=savdksc
046000110708     c                   eval      v3cuni=savuni
046100110708     c                   eval      v3duni=savduni
046200121120     c                   eval      v3cterm=savterm
046300121123     c                   eval      v3hterm=savhterm
046400110127     c                   exsr      Aggiosfl
046500110127    2c                   endif
046600110127     c
046700110127     c                   eval      nrr1=nrr1-2
046800110127     c                   enddo
046900110127     c
047000110127     c* clearo la riga sotto al F10
047100110127     c                   eval      wnrr=nrr1+1
047200110708     c     wnrr          chain     tb88s03
047300110127     c                   clear                   v3cksc
047400110809     c                   clear                   v3hksc
047500110127     c                   clear                   v3dksc
047600110708     c                   clear                   v3cuni
047700110708     c                   clear                   v3duni
047800121120     c                   clear                   v3cterm
047900121123     c                   clear                   v3hterm
048000110127     c                   exsr      Aggiosfl
048100110127     c
048200110127     c                   eval      c01csr=savcsr
048300110127     c                   ENDSR
048400110127     c
048500110127      *-----------------------------------------------------***********
048600110127      * controllo dati sfl
048700110127      *-----------------------------------------------------***********
048800110127     C     ctrd02        BEGSR
048900110809     c                   clear                   forzavar
049000110112     c*
049100110112     c                   eval      nrr1=1
049200110112     c                   z-add     1             yy
049300110809     c                   z-add     1             zz
049400110112     c                   clear                   ksc
049500110112     c*
049600110708    0c                   dow       nrr1<=totnrr
049700110120     c
049800110708     c     nrr1          chain     tb88s03
049900110810   0ac                   if        %found
050000110112     C*
050100110112     c     '?'           scan      v3cksc                                 90
050200110112    1c                   if        *in90
050300110112     c                   movel     rsut          paxdut           30
050400110112     c                   movel(p)  v3dksc        paxdmt           48
050500110112      * PAXSTA=9 ESCLUDO ANNULLATI
050600110112     C                   Z-ADD     9             PAXSTA
050700110112     c                   movel     dutkci        paxccc            4 0
050800110112     c                   clear                   paxdit
050900110112     C                   z-add     1             paxnum
051000110112     C                   CALL      'XALFA3BR'
051100110112     C                   PARM                    PAXDUT
051200110112     C                   PARM      1             CODUT             1 0
051300110112     C                   PARM                    PAXDMT
051400110112     C                   PARM                    PAXCCC
051500110112     C                   PARM                    PAXSTA            1 0
051600110112     C                   PARM                    PAXFLR           90
051700110112     C                   PARM                    PAXDIT            3
051800110112     C                   PARM                    PAXNUM            2 0
051900110112     C                   PARM                    PAXKCM           80
052000110112     C                   PARM                    PAXKSM          140
052100110112     C                   PARM                    PAXKDM           60
052200110112    2C     PAXSTA        IFGT      -1
052300110112     C                   MOVEL     paxksm        v3cksc
052400110112     C                   MOVEL     paxdmt        v3dksc
052500110803     c                   else
052600110803     c                   clear                   v3cksc
052700110112     c                   endif
052800110112    2c                   exsr      Aggiosfl
052900110112     c                   leavesr
053000110112    1c                   endif
053100110112     c
053200110112    1c                   if        v3cksc<>*blanks and v3cksc<>*zeros
053300110120     c                   testn                   v3cksc               35
053400110112     c
053500110120    2c                   if        not *in35  or %subst(v3cksc:7:1)<'0'
053600110114     c                   seton                                        29
053700110112     c                   eval      v1cmsg=msg(2)
053800110112     c                   exsr      Aggiosfl
053900110112     c                   leavesr
054000110112    2c                   endif
054100110112     c
054200110112     c                   movel     v3cksc        kksc
054300110112     c     kaco          chain     cnaco00f
054400110112    2c                   if        %found(cnaco00f)
054500110112     c                   movel     acorag        v3dksc
054600110112   x2c                   else
054700110120     c                   seton                                        29
054800110112     c                   eval      v1cmsg=msg(2)
054900110112     c                   exsr      Aggiosfl
055000110112     c                   leavesr
055100110112    2c                   endif
055200110112     c
055300110112     c*  Verifico se già presente
055400110114     c     kksc          lookup    ksc                                    29
055500110708    2c                   if        *in29
055600110112     c                   eval      v1cmsg=msg(3)
055700110112     c                   exsr      Aggiosfl
055800110112     c                   leavesr
055900110112     c                   else
056000110112     c                   eval      ksc(yy)=kksc
056100110112     c                   eval      yy=yy+1
056200110809     c
056300110809     c* Memorizzo tutti gli unificanti per eventuale controllo
056400110810     c     v3cuni        lookup    unif                                   35
056500110810    2c                   if        not *in35
056600110810     c                   eval      unif(zz) =v3cuni
056700110810     c                   eval      unifd(zz)=v3duni
056800110809     c                   eval      zz=zz+1
056900110809    2c                   endif
057000110809     c
057100110112    2c                   endif
057200110810     c
057300110810     c                   if        v3cksc<>v3hksc
057400110708     c                   clear                   v3cuni
057500110810     c                   clear                   v3hvaru
057600110708     c                   exsr      Decodi
057700110810     c*
057800110810     c                   else
057900121123     c* se uguale do msg forzabile per cambio codic unificante
058000110810     c                   if        v3hvaru='S'
058100110810     c                   seton                                        29
058200110810     c                   eval      v1cmsg=msg(8)
058300110810     c                   eval      v3hvaru='X'
058400110810     c                   exsr      Aggiosfl
058500110810     c                   leavesr
058600110810    2c                   endif
058700110810     c                   endif
058800121123     c
058900121123     c* controllo terminal  b999 = tutti 888= nessuno
059000121120     c     v3cterm       chain     azorg01l
059100121123    2c                   if        v3cterm<> 999 and v3cterm<>888
059200121123    3c                   if        not %found(azorg01l) or (orgfag<>'A' and
059300121123     c                             orgfag<>'F')  or orgfva<>' '
059400121120     c                   seton                                        41
059500121120     c                   eval      v1cmsg=msg(9)
059600121120     c                   exsr      Aggiosfl
059700121120     c                   leavesr
059800121123    3c                   endif
059900121123     c
060000121123   3ac                   if        v3hterm<>v3cterm
060100121123     c                   clear                   wok               1
060200121123     c                   eval      kepa='P'
060300121123    3c                   do        2
060400121123     c     kcae          setll     azcae05l
060500121123     c     kcae          reade     azcae05l
060600121123    4c                   dow       not %eof(azcae05l)
060700121123    5c                   if        dateu >=caedde and dateu<=caedsc
060800121123     c                   eval      wok='S'
060900121123     c                   leave
061000121123    5c                   endif
061100121123     c
061200121123     c     kcae          reade     azcae05l
061300121123    4c                   enddo
061400121123    4c                   if        wok='S'
061500121123     c                   leave
061600121123     c                   else
061700121123     c                   eval      kepa='A'
061800121123    4c                   endif
061900121123    3c                   enddo
062000121123     c
062100121123     c                   eval      v3hterm=v3cterm
062200121123    3c                   if        wok=' '
062300121123     c                   seton                                        41
062400121123     c                   eval      v1cmsg=msg(10)
062500121123     c                   exsr      Aggiosfl
062600121123     c                   leavesr
062700121123    3c                   endif
062800121123   3ac                   endif
062900121123     c
063000121123    2c                   endif
063100110114     c*
063200110114     c                   exsr      Aggiosfl
063300110112     c*
063400110708   x1c                   else
063500110112     c                   clear                   v3dksc
063600110708     c                   clear                   v3duni
063700110708     c                   clear                   v3cuni
063800121120     c                   clear                   v3cterm
063900110112     c                   exsr      Aggiosfl
064000110112    1c                   endif
064100110120     c
064200110708   0ac                   endif
064300110112     c
064400110112     c                   eval      nrr1=1+nrr1
064500110112     c*
064600110708    0c                   enddo
064700110120     c                   z-add     1             nrr1
064800110112     c
064900110112     c                   ENDSR
065000110112      *-----------------------------------------------------***********
065100110112      * Aggiornamento tabella
065200110112      *-----------------------------------------------------***********
065300110112     C     ScriviTAB     BEGSR
065400110112     c*
065500110112     c* Intanto cancello tutti i record
065600110112     c
065700110112     c/exec sql
065800110708     c+ delete from tntbe00f where tbecod='CXQ' and tbeke2<>'               '
065900110112     c/end-exec sql
066000110112     c
066100110112     c
066200110112     c                   eval      nrr1=1
066300110112     c                   z-add     1             yy
066400110112     c                   clear                   ksc
066500110112     c*
066600110708     c     nrr1          chain     tb88s03
066700110112    0c                   dow       %found
066800110112     c
066900110112    1c                   if        v3cksc>*zeros
067000110112     c                   clear                   tntbe000
067100110708     c                   eval      tbecod='CXQ'
067200110708     c                   eval      tbeapl='GE'
067300110810     c                   movel     v3cuni        tbeke1
067400110708     c                   eval      tbeke2=v3cksc
067500110810     c                   clear                   dcxq
067600110810     c                   movel     v3dksc        §cxqrsc
067700110810     c                   movel     v3duni        §cxqrsu
067800121120     c                   movel     v3cterm       §cxqterm
067900110810     c                   movel     dcxq          tbeuni
068000110112     c                   write     tntbe000
068100110114     c                   eval      yy=yy+1
068200110112     c                   endif
068300110809     c
068400110809     c                   eval      v3hksc=v3cksc
068500110809     c                   exsr      AGGIOSFL
068600110112     c
068700110112     c                   eval      nrr1=nrr1+1
068800110708     c     nrr1          chain     tb88s03
068900110112     c                   enddo
069000121127     c
069100121127     c                   clear                   trul0sds
069200121127     c                   eval      i0stla='L'
069300121127     c                   eval      i0sski='CXQ'
069400121127     c                   eval      i0sele=1100
069500121127     c                   eval      i0spie=yy
069600121127     c                   eval      i0sfile='TNTBE00F'
069700121127     c                   eval      i0ssif=knsif
069800121127     c                   eval      i0spgm='TNSSL3R2'
069900121127     c                   movel     trul0sds      kpjbu
070000121127     c                   call      'TRUL0SR'
070100121127     c                   parm                    kpjba
070200121127
070300110112     c                   ENDSR
070400110809      *-----------------------------------------------------***********
070500110809      * controllo tutto il sfl per F3 per vedere se ci sono dei record
070600110809      *  variati che verranno persi
070700110809      *-----------------------------------------------------***********
070800110809     C     ContrTAB      BEGSR
070900110809     c
071000110809     c                   eval      nrr1=1
071100110809     c                   z-add     1             yy
071200110809     c*
071300110809     c     nrr1          chain     tb88s03
071400110809    0c                   dow       %found
071500110809     c
071600110809     c                   if        v3cksc=*zeros
071700110809     c                   clear                   v3cksc
071800110809     c                   endif
071900110809     c                   if        v3hksc=*zeros
072000110809     c                   clear                   v3hksc
072100110809     c                   endif
072200110809     c
072300110809     c                   if        v3cksc<>v3hksc
072400110812     c                   seton                                        90
072500110809     c                   eval      Forzavar='S'
072600110809     c                   leavesr
072700110809     c                   endif
072800110809     c
072900110809     c                   eval      nrr1=nrr1+1
073000110809     c     nrr1          chain     tb88s03
073100110809     c                   enddo
073200110809     c                   ENDSR
073300110810      *-----------------------------------------------------***********
073400110810     c*  Gestione SFL 05 - controlla unificanti
073500110810      *-----------------------------------------------------***********
073600110810     c     Gessfl05      BEGSR
073700110810     c                   exsr      contrUnif
073800110810     c
073900110810     c                   setoff                                       28
074000110810     c* se non ho caricato nemmeno un record videata vuota
074100110810     c                   if        nrr5=0
074200110810     c                   seton                                        30
074300110810     c                   write     tb88c06
074400110810     c                   exfmt     tb88c05
074500110810     c
074600110810     c                   setoff                                       30
074700110810     c
074800110810     c                   else
074900110810     c
075000110810     c     for05         tag
075100110810     c                   eval      c05rcd=c05csr
075200110810     c
075300110810     c  n28              write     tb88z05
075400110810     c*
075500110810     c                   exfmt     tb88c05
075600110810     C                   SETOFF                                       2890
075700110810     C                   SETOFF                                       40
075800110810     c                   clear                   v1cmsg
075900110810     C* F12- ritrono
076000110810     c                   if        *inkl
076100110810     c                   leavesr
076200110810     c                   endif
076300110810     c
076400110810     c                   exsr      ctrd05
076500110810     c
076600110810     c                   if        *in90 or not *inkf
076700110810     c                   goto      for05
076800110810     c                   endif
076900110810     c* F6 - conferma
077000110810     c                   if        *inkf
077100110810     c                   exsr      ScriviTAB05
077200110810     c                   endif
077300110810     c
077400110810     c                   endif
077500110810     c
077600110810     c                   ENDSR
077700110809      *-----------------------------------------------------***********
077800110809      * Controllo i codici unificanti se per caso sono variati i legami
077900110809      *  li evidenzio
078000110809      *-----------------------------------------------------***********
078100110809     C     ContrUNIF     BEGSR
078200110809     c* Elaboro la schiera degli unifficanti e controllo rispoetto a
078300110809     c*  quelli inseriti in tabella se ce ne sono in più o meno
078400110809     C* PULIZIA SFL
078500110809     C                   SETON                                        3031
078600110809     C                   WRITE     tb88C05
078700110809     C                   SETOFF                                       3031
078800110809     c                   clear                   nrr5
078900110809     c                   clear                   totnrr5           5 0
079000110809     c
079100110809     c                   z-add     1             xx
079200110809    1c                   dow       unif(xx)>0
079300110809     c                   clear                   tibs10ds
079400110809     c                   eval      d10tle='ST'
079500110809     c                   eval      d10cod=unif(xx)
079600110809     c                   eval      d10paf='F'
079700110809     c                   call      'TIBS10R'
079800110809     c                   parm                    tibs10ds
079900110809     c                   parm      'R'           d10ela            1
080000110809     c
080100110809    2c                   if        d10err<>*blanks
080200110809     c* se non trovato cerco come Figlio
080300110809     c                   clear                   skc
080400110809     c                   eval      d10paf='P'
080500110809     c                   call      'TIBS10R'
080600110809     c                   parm                    tibs10ds
080700110809     c                   parm      'R'           d10ela            1
080800110809     c
080900110809     c
081000110809     c* se non ho trovato nulla --> imposto se stesso   padre=figlio
081100110809    3c                   if        d10err<>*blanks
081200110809     c                   movel     d10cod        d10cop
081300110809     c                   movel     d10cod        skc(1)
081400110809    3c                   endif
081500110809    2c                   endif
081600110809     c
081700110809     c                   z-add     1             zz
081800110809    2c                   dow       skc(zz)>*zeros
081900110809     c                   eval      kksc=%int(%subst(skc(zz):5:7))
082000110809     c
082100110809     c* Verifico se già presente nella tabella
082200110810     c     kksc          lookup    ksc                                    35
082300110810    3c                   if        not *in35
082400110810     c                   setoff                                       10
082500110810     c                   eval      v5dmod='MANCANTE '
082600110809     c                   eval      v5cuni=unif(xx)
082700110810     c                   eval      v5duni=unifd(xx)
082800110809     c                   eval      v5cksc=kksc
082900110809     c     kaco          chain     cnaco00f
083000110809    4c                   if        %found(cnaco00f)
083100110809     c                   movel     acorag        v5dksc
083200110809    4c                   endif
083300110809     c                   add       1             nrr5
083400110809     c                   write     tb88s05
083500110809    3c                   endif
083600110809     c
083700110809     C                   EVAL      zz=zz+1
083800110809    2c                   enddo
083900110809     c
084000110809     C                   EVAL      xx=xx+1
084100110809    1c                   enddo
084200110809     c
084300110810     c                   eval      c05csr=1
084400110810     c                   z-add     nrr5          totnrr5
084500110809     c                   ENDSR
084600110810      *-----------------------------------------------------***********
084700110810      * controllo dati sfl 05
084800110810      *-----------------------------------------------------***********
084900110810     C     ctrd05        BEGSR
085000110810     c*
085100110810     c                   eval      nrr5=1
085200110810     c                   z-add     1             yy
085300110810     c                   z-add     1             zz
085400110810     c                   clear                   wopz
085500110810     c*
085600110810    0c                   dow       nrr5<=totnrr5
085700110810     c
085800110810     c     nrr5          chain     tb88s05
085900110810    1c                   if        %found
086000110810     c* Per aggiunta solo "I"
086100110810    2c                   if        v5copz='I' and %subst(v5dmod:1:1)='E'
086200110810     c                   seton                                        40
086300110810     c                   eval      v1cmsg=msg(6)
086400110810     c                   exsr      Aggiosfl5
086500110810     c                   leavesr
086600110810    2c                   endif
086700110810     c* Per eliminaz. solo "A"
086800110810    2c                   if        v5copz='A' and %subst(v5dmod:1:1)='A'
086900110810     c                   seton                                        40
087000110810     c                   eval      v1cmsg=msg(1)
087100110810     c                   exsr      Aggiosfl5
087200110810     c                   leavesr
087300110810    2c                   endif
087400110810     c
087500110810     c* se premuto F6 ci deve essere almeno una "I" o una "A"
087600110810    2c                   if        v5copz<>*blanks
087700110810     c                   eval      wopz=v5copz
087800110810    2c                   endif
087900110810     c
088000110810    1c                   endif
088100110810     c
088200110810     c                   eval      nrr5=1+nrr5
088300110810     c*
088400110810    0c                   enddo
088500110810     c
088600110810     c* se premuto F6 ci deve essere almeno una "I" o una "A"
088700110810     c   kf              if        wopz=' '
088800110810     c                   z-add     1             nrr5
088900110810     c                   seton                                        40
089000110810     c                   eval      v1cmsg=msg(7)
089100110810     c                   exsr      Aggiosfl5
089200110810    2c                   endif
089300110810     c
089400110810     c                   ENDSR
089500110810      *-----------------------------------------------------***********
089600110810      * Aggiunta/elimiinazione recod da sfl
089700110810      *-----------------------------------------------------***********
089800110810     C     ScriviTAB05   BEGSR
089900110810     c
090000110810     c                   eval      nrr5=1
090100110810     c                   z-add     1             yy
090200110810     c                   clear                   ksc
090300110812     c                   clear                   wPrimo
090400110810     c*
090500110810     c     nrr5          chain     tb88s05
090600110810    0c                   dow       %found
090700110810     c
090800110810    1c                   if        v5copz='I'
090900110810     c
091000110810     c* cerco la prima posizione vuota
091100110810     c                   eval      nrr1=totnrr+1
091200110810     c                   clear                   v3cksc
091300110810    2c                   dow       nrr1>0 and v3cksc<=*zeros
091400110810     c                   eval      nrr1=nrr1-1
091500110810     c     nrr1          chain     tb88s03
091600110810    2c                   enddo
091700110810     c
091800110810     c                   eval      nrr1=nrr1+1
091900110810     c
092000110810     c* se tutte le righe scritte sono piene--> aggiungo una pagina
092100110810    2c                   if        nrr1>totnrr
092200110810     c                   eval      wnrr =nrr1
092300110810     c                   eval      nrr1=totnrr
092400110810     c
092500110810     c                   exsr      righevuote
092600110810     c                   eval      nrr1=wnrr
092700110810    2c                   endif
092800110810     c
092900110810     c     nrr1          chain     tb88s03
093000110810    2c                   if        %found
093100110810     c                   movel     v5cksc        v3cksc
093200110810     c                   movel     v5dksc        v3dksc
093300110810     c                   seton                                        29
093400110810     c                   eval      v3dksc='--> INSERITO'
093500110810     c                   move      v5cuni        v3cuni
093600110810     c                   move      v5duni        v3duni
093700121120     c                   if        v3cterm<=*zeros
093800121123     c                   clear                   fnlv55ds
093900121123     c                   eval      d55tpt='P'
094000121123     c                   eval      d55drf=dateu
094100121123     c                   movel     v3cuni        d55lin
094200121123     c                   call      'FNLV55R'
094300121123     c                   parm                    fnlv55ds
094400121123     c*
094500121123     c                   movel     d55tfp        v3cterm
094600121120     c                   endif
094700121120     c
094800110810     c                   exsr      Aggiosfl
094900110812     c* Posizionamento sul primo record inserito
095000110812     c                   if        Wprimo=' '
095100110812     c                   eval      c01csr=nrr1
095200110812     c                   eval      Wprimo='S'
095300110812     c                   endif
095400110812     c
095500110810    2c                   endif
095600110810    1c                   endif
095700110810     c
095800110810     c                   eval      nrr5=nrr5+1
095900110810     c     nrr5          chain     tb88s05
096000110810     c                   enddo
096100110810     c
096200110810     c                   ENDSR
096300110112      *****************************************************************
096400110112     c* Aggioranemnto sfl
096500110112      *****************************************************************
096600110112     C     Aggiosfl      BEGSR
096700110112     c                   if        v1cmsg<>*blanks
096800110112     c                   seton                                        9028
096900110112     c                   eval      c01csr=nrr1
097000110112     c                   endif
097100110810     c*
097200110810     c                   if        v3hvaru<>*blanks
097300110810     c                   seton                                        11
097400110810     c                   else
097500110810     c                   setoff                                       11
097600110810     c                   endif
097700110112     c
097800110708     c                   update    tb88s03
097900110810     c                   setoff                                       29
098000110112     c
098100110112     c                   ENDSR
098200110810      *****************************************************************
098300110810     c* Aggioranemnto sfl5
098400110810      *****************************************************************
098500110810     C     Aggiosfl5     BEGSR
098600110810     c                   if        v1cmsg<>*blanks
098700110810     c                   seton                                        9028
098800110810     c                   eval      c05csr=nrr5
098900110810     c                   endif
099000110810     c                   if        %subst(v5dmod:1:1)='E'
099100110810     c                   seton                                        10
099200110810     c                   else
099300110810     c                   setoff                                       10
099400110810     c                   endif
099500110810     c
099600110810     c                   update    tb88s05
099700110810     c                   setoff                                       40
099800110810     c
099900110810     c                   ENDSR
100000110708      *****************************************************************
100100110708     c     Decodi        BEGSR
100200110708    1c                   if        v3dksc=*blanks
100300110708     c                   movel     v3cksc        kksc
100400110708     c     kaco          chain     cnaco00f
100500110708    2c                   if        %found(cnaco00f)
100600110708     c                   movel     acorag        v3dksc
100700110708    2c                   endif
100800110708    1c                   endif
100900110708     c
101000110708     c* Reperisco codice Padre
101100110708    1c                   if        v3cuni=0
101200110708     c                   clear                   tibs10ds
101300110708     c                   eval      d10tle='ST'
101400110708     c                   eval      d10cod=acoksc
101500110708     c                   eval      d10paf='P'
101600110708     c                   call      'TIBS10R'
101700110708     c                   parm                    tibs10ds
101800110708     c                   parm      'R'           d10ela            1
101900110708     c
102000110708     c* Passato tipo elaborazione per non eseguire la chiusura file tutte le volte
102100110708     c*  (li chiude per " " o "C" )
102200110708    2c                   if        d10err=*blanks and d10cop>*zeros
102300110708     c                   move      d10cop        v3cuni
102400110708     c                   else
102500110708     c                   movel     v3cksc        v3cuni
102600110708    2c                   endif
102700110708    1c                   endif
102800110708     c
102900110708    1c                   if        v3cuni>*zeros
103000110708     c                   movel     v3cuni        kksc
103100110708     c     kaco          chain     cnaco00f
103200110708    2c                   if        %found(cnaco00f)
103300110708     c                   movel     acorag        v3duni
103400110812     c                   movel     acorag        P3duni
103500110708    2c                   endif
103600110708    1c                   endif
103700121120     c*  se terminal non immesso, prendo al filiale del cliente unificante
103800121123    1c**                 if        v3cterm<=*zeros
103900121123     c**                 clear                   fnlv55ds
104000121123     c**                 eval      d55tpt='P'
104100121123     c**                 eval      d55drf=dateu
104200121123     c**                 movel     v3cuni        d55lin
104300121123     c**                 call      'FNLV55R'
104400121123     c**                 parm                    fnlv55ds
104500121123     c*
104600121123     c**                 movel     d55tfp        v3cterm
104700121123    1c**                 endif
104800110708     c
104900110708     c                   ENDSR
105000110812      *-----------------------------------------------------***********
105100110812      * Stampa tabella "CXQ"
105200110812      *-----------------------------------------------------***********
105300110812     C     StampaTab     BEGSR
105400110812     c                   seton                                        OF
105500110812     c
105600110812     c     kcod          setll     tntbe01l
105700110812     c     kcod          reade(n)  tntbe01l
105800110812     c
105900110812    1c                   dow       not %eof(tntbe01l)
106000110812     c
106100110812     c                   movel     tbeke2        v3cksc
106200110812     c* Chain su cnaco per la ragione sociale
106300110812    2c                   if        v3cksc>*zeros
106400110812     c                   clear                   V3dksc
106500110812     c                   clear                   v3cuni
106600110812     c                   movel     tbeuni        dCXQ
106700110812     c                   exsr      Decodi
106800110812     c
106900110812     c* Se cambiato codice padre
107000110812     c                   if        %subst(tbeke1:1:7)>*zeros
107100110812     c                   eval      wunif=%int(%subst(tbeke1:1:7))
107200110812     c                   else
107300110812     c                   clear                   wunif
107400110812     c                   endif
107500110812     c
107600110812    3c                   if        wunif  >0 and wunif  <>v3cuni
107700110812     c                   movel     wunif         kksc
107800110812     c     kaco          chain     cnaco00f
107900121120    4c                   if        not %found(cnaco00f)
108000110812     c                   clear                   acorag
108100121120    4c                   endif
108200110812     c
108300110812     c                   eval      %subst(P3duni:13:33)=' in tabella con-->'+
108400110812     c                                   %editc(wunif:'X')+ Acorag
108500110812     c                   seton                                        11
108600110812    3c                   endif
108700121120    2c                   endif
108800121120     c
108900121120     c* Terminal
109000121120    2c                   if        §cxqterm>*zeros
109100121120     c                   movel     §cxqterm      v3cterm
109200121120     c
109300121123     c                   select
109400121123    3c                   when      v3cterm =999
109500121120     c                   eval      p3dterm='T u t t i '
109600121123     c
109700121123    3c                   when      v3cterm =888
109800121123     c                   eval      p3dterm='N e s s u n o'
109900121123     c                   other
110000121123     c
110100121120     c     v3cterm       chain     azorg01l
110200121120    4c                   if        %found(azorg01l)
110300121120     c                   movel     orgdes        p3dterm
110400121120     c                   else
110500121120     c                   eval      p3dterm=*all'?'
110600121120    4c                   endif
110700121123    3c                   endsl
110800121123    2c                   endif
110900110812     c*
111000110812     C                   IF        *INOF
111100110812     C                   EXCEPT    TESTA
111200110812     C                   SETOFF                                       OF
111300110812     C                   ENDIF
111400110812     C
111500110812     C                   EXCEPT    DET
111600110812     c
111700110812     c     kcod          reade(n)  tntbe01l
111800110812    1c                   enddo
111900110812     c
112000110812     c                   ENDSR
112100110112      *****************************************************************
112200110112     C     *INZSR        BEGSR
112300110112      *
112400110112     C     *ENTRY        PLIST
112500110112     C                   PARM                    KPJBA
112600110112      *
112700110112      *
112800110112      * accesso tntbe
112900110120     c     ktab          klist
113000110120     C                   kfld                    kkut
113100110120     C                   kfld                    kcodt
113200110120     C                   kfld                    kkey
113300110112     c     kaco          klist
113400110112     C                   kfld                    kkut
113500110112     C                   kfld                    dutkci
113600110112     C                   kfld                    kksc
113700110112     c
113800121123     c     kcae          klist
113900121123     C                   kfld                    kepa              1
114000121123     C                   kfld                    v3cterm
114100121123     c
114200110112     c                   exsr      sr_DatiJob
114300110812     c
114400110812     C                   TIME                    WTIME            14 0
114500110812     C                   MOVE      WTIME         WDATE             8 0
114600110812     C                   MOVEL     WTIME         UTIME             6 0
114700121123     c     *eur          movel     wdate         dataiso
114800121123     c                   movel     dataiso       dateu             8 0
114900110112      *
115000110112     C                   ENDSR
115100110112      *---------------------------------------------------------------*
115200110112      *?Reperimento dati del job (Utente/Operativi)                  ?*
115300110112      *---------------------------------------------------------------*
115400110112     c     sr_DatiJob    BEGSR
115500110112      *
115600110112     c     *dtaara       define    §azute        AZUTEds
115700110112     c     *dtaara       define    §datiute      dDatiUte
115800110112      *
115900110112     c                   in(E)     *dtaara
116000110112     c                   IF        %Error or RSUT = *blanks
116100110112     c                   clear                   TIBS34ds
116200110112     c                   call      'TIBS34R'
116300110112     c                   parm                    TIBS34ds
116400110112     c                   in        *dtaara
116500110112     c                   ENDIF
116600110112      *
116700110112     c                   ENDSR
116800110812     c*----------------------------------------------------------------------
116900110812     OQSYSPRT   E            TESTA            01
117000110812     O                       SK(1)               66
117100110812     O                       SK(2)              132
117200110812     O                       RSUT                20
117300110812     O                       WDATE              118 '  /  /    '
117400110812     O                       PAGE          Z    132
117500110812     O          E            TESTA            02
117600110812     O                       KNSIF               10
117700110812     O                       KNMUS               22
117800110812     O                       utimE              116 '  :  :  '
117900110812     O          E            TESTA            03
118000110812     O                       SK(5)               66
118100110812     O                       SK(5)              132
118200110812     O*
118300110812     O          E            TESTA            04
118400110812     O                       SK(3)               66
118500110812     O                       SK(4)              132
118600110812     O*
118700110812     O          E            TESTA            05
118800110812     O                       SK(5)               66
118900110812     O                       SK(5)              132
119000110812     O
119100110812     O          E            DET         1
119200110812     O                       V3CUNI
119300110812     O                       P3DUNI           +   2
119400110812     O                       V3CKSC           +   4
119500110812     O                       V3DKSC           +   2
119600121120     O                       V3cterm          +   4
119700121120     O                       p3dterm          +   2
119800020805      **************************************************************************
119900960302** SCHIERA MSG - MESSAGGI DI ERRORE
120000110810Codice  AGGIUNTO  all'Unificante: possibile solo "I-inserimento"                1
120100110810Codice cliente errato o inesistente                                             2
120200110810Codice cliente già inserito                                                     3
120300110810Scorrimento non ammesso: ci sono ancora righe vuote nella pagina                4
120400110810ATTENZIONE alcune modifiche andranno perdute.F3 uscita senza conferma F6-Confer 5
120500110810Codice  ELIMINATO  dall'unificante: possibile solo "A-annullamento"             6
120600110810Uscire con F12 o effettuare almeno una scelta e premere F6                      7
120700110829VARIATO codice Unificante: verificare se eliminare il cod.cliente.ENTER forza   8
120800121120Terminal inesistente                                                            9
120900121123In data odierna la filiale indicata NON E' un terminal: enter forza            10
121000110812**
121100110812XXXXXXXXXXXXXXXXXXXX                     ** STAMPA TABELLA CLIENTI   1
121200110812 PARTICOLARI **               TNTB88R     XX.XX.XXXX     PAG. XXXX   2
121300110812Cliente Unificante attuale                                 Codice    3
121400121120cliente                             Terminal  Appartenenza           4
121500110812------------------------------------------------------------------   5
