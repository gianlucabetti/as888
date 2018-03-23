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
001100110112     Fcnaco00f  if   E           k DISK
001200110112     Fcnclp00f  if   E           k DISK
001300110812     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001400110812     f                                     FORMLEN(066)
001500110812     f                                     FORMOFL(063)
001600020805      *
001700020805      * DEFINIZIONE SCHIERE
001800960302     D*
001900110810     D MSG             S             78    DIM(9) CTDATA PERRCD(1)
002000110812     D SK              S             66    DIM(5) CTDATA PERRCD(1)
002100110803     D ksc             S              7  0 DIM(900)
002200110810     D Unif            S              7  0 DIM(400)
002300110810     D UnifD           S             24    DIM(400)
002400110112     D*
002500110112     D KPJBA         E DS
002600110810     D dCXQ          E DS
002700110810     d
002800110708     D tibs10ds      E DS
002900110708     d  skc                   21   5520    dim(500)
003000110112     d TIBS02DS      E DS
003100110708     d xclirds       E DS
003200110708     D xKSC            S              7  0 DIM(30)
003300110708     D xKSA            S              4    DIM(30)                              *ALFANUMERICO
003400110708
003500110708     D                 DS
003600110708     D  DSKSN                  1      4P 0
003700110708     D  DSKSA                  1      4
003800110708     d
003900110112      * - Parametri per richiamo al pgm di controllo profilo utenti
004000110112     d TIBS34ds      e ds
004100110112      * - Ds di riferimento al file esterno AzUte00F
004200110112     d AZUTEds       e ds                  ExtName(AzUte00F)
004300110112      * - Ds per dati organigramma
004400110112     d dDatiUte      e ds
004500080529     d
004600080529     D WLBDAT          DS                  INZ
004700080529     D  G02DAT                 1      8  0
004800080529     D  G02INV                 9     16  0
004900080529     D  G02ERR                17     17
005000080529     D  G02TGI                18     22  0
005100080529     d
005200050103     d Dataiso         s               d   datfmt(*iso)
005300050103     d Dataeur         s               d   datfmt(*eur)
005400080529     d datadmy         s               d   datfmt(*dmy)
005500050103     d
005600080529     d nrr1            s              4  0 inz
005700110809     d nrr5            s              4  0 inz
005800110120     D kcodt           S                   LIKE(tblcod) inz('01')
005900110120     D kkey            S                   LIKE(tblkey)
006000110708     D kcod            S                   LIKE(tbecod) inz('CXQ')
006100110120     D kkey1           S                   LIKE(tbeke1)
006200110112     D kkey2           S                   LIKE(tbeke2)
006300110114     D kkut            S                   LIKE(acokut) inz(1)
006400110112     D kksc            S                   LIKE(acoksc)
006500110127     D savksc          S                   LIKE(v3cksc)
006600110809     D savhksc         S                   LIKE(v3cksc)
006700110127     D savdksc         S                   LIKE(v3dksc)
006800110708     D savuni          S                   LIKE(v3cuni)
006900110708     D savduni         S                   LIKE(v3duni)
007000110127     D wnrr            S                   LIKE(nrr1)
007100110127     D savcsr          S                   LIKE(c01csr)
007200110114     d c_SflPag        c                   const(17)
007300110112     d wpag            s              6  0
007400110112     d wresto          s              3  0
007500110708     d zz              s              3  0
007600110708     d yy              s              3  0
007700110708     d XX              s              3  0
007800110708     d Indx            s              3  0
007900110112     d $Fine           s              1
008000110810     d Wopz            s              1
008100110809     d Forzavar        s              1
008200110810     d wunif           s              7  0
008300110812     d P3DUNI          s             45
008400041220     d
008500050103     C**********************************************************************
008600110112     c
008700110112     c                   exsr      Carsfl
008800110112     c                   exsr      Gessfl
008900080529     c
009000960129     C*
009100000000     C                   SETON                                        LR
009200080529      *-----------------------------------------------------***********
009300110112      * Caricamento sfl
009400080529      *-----------------------------------------------------***********
009500110112     C     CarSFL        BEGSR
009600080529     C* PULIZIA SFL
009700080529     C                   SETON                                        3031
009800110708     C                   WRITE     tb88C03
009900110810     C                   SETOFF                                       3031
010000080529     c                   clear                   nrr1
010100080529     c                   clear                   totnrr            5 0
010200110809     c                   clear                   forzavar
010300110112     c
010400110708     c     kcod          setll     tntbe01l
010500110708     c     kcod          reade(n)  tntbe01l
010600110112     c
010700110112    1c                   dow       not %eof(tntbe01l)
010800110112     c
010900110810     c                   clear                   v3hvaru
011000110810     c                   setoff                                       11
011100110708     c                   movel     tbeke2        v3cksc
011200110809     c                   movel     tbeke2        v3hksc
011300110112     c* Chain su cnaco per la ragione sociale
011400110112    2c                   if        v3cksc>*zeros
011500110708     c                   clear                   v3dksc
011600110708     c                   clear                   v3cuni
011700110810     c                   movel     tbeuni        dCXQ
011800110708     c                   exsr      Decodi
011900110810     c
012000110810     c* Se cambiato codice padre --> evidenzio in rosso
012100110810     c                   if        %subst(tbeke1:1:7)>*zeros
012200110810     c                   eval      wunif=%int(%subst(tbeke1:1:7))
012300110810     c                   else
012400110810     c                   clear                   wunif
012500110810     c                   endif
012600110810     c
012700110810    3c                   if        wunif  >0 and wunif  <>v3cuni
012800110810     c                   eval      v3hvaru='S'
012900110810     c                   eval      %subst(v3duni:13:13)=' old->'+
013000110810     c                                                   %editc(wunif:'X')
013100110810     c                   seton                                        11
013200110810    3c                   endif
013300110810    2c                   endif
013400110112     c*
013500110112     c                   add       1             nrr1
013600110708     c                   write     tb88s03
013700110112     c
013800110708     c     kcod          reade(n)  tntbe01l
013900110708    1c                   enddo
014000110112     c
014100110112     c                   exsr      righevuote
014200110812     c* La prima volta mi posizione in prima videata
014300110812     c                   z-add     1             c01csr
014400110112     c
014500110112     c                   ENDSR
014600110112      *-----------------------------------------------------***********
014700110120     c*  Carico righe vuote del sfl
014800110112      *-----------------------------------------------------***********
014900110112     c     Righevuote    BEGSR
015000110810     c                   setoff                                       11
015100110112     c* Carico le righe vuote fino a riempimento di pagina
015200110112    1c                   if        nrr1>=c_sflpag
015300110112     c                   eval      wpag=%div(nrr1:c_sflPag)
015400110112     c                   eval      wresto=%rem(nrr1:c_sflPag)
015500110112    2c                   if        wresto>0
015600110112     c                   eval      c01csr=nrr1
015700110112     c                   else
015800110112     c                   eval      c01csr=(wpag*c_sflpag)+1
015900110112    2c                   endif
016000110112     c
016100110112   x1c                   else
016200110112     c                   clear                   wpag
016300110112     c                   eval      wresto=nrr1
016400110112     c                   eval      c01csr=nrr1
016500110112    2c                   if        c01csr=0
016600110112     c                   eval      c01csr=1
016700110112    2c                   endif
016800110112    1c                   endif
016900110112     C
017000110112     C                   DOW       wresto<c_sflpag
017100110112     c                   clear                   v3cksc
017200110809     c                   clear                   v3hksc
017300110112     c                   clear                   v3dksc
017400110708     c                   clear                   v3cuni
017500110708     c                   clear                   v3duni
017600110810     c                   clear                   v3hvarU
017700110112     c*
017800110112     c                   eval      wresto=wresto+1
017900110112     c                   add       1             nrr1
018000110708     c                   write     tb88s03
018100110112     c                   enddo
018200110112     c
018300110112     c                   z-add     nrr1          totnrr
018400110112     c                   seton                                        33
018500110112     c                   ENDSR
018600110112      *-----------------------------------------------------***********
018700110112     c*  Gestione SFL
018800110112      *-----------------------------------------------------***********
018900110112     c     Gessfl        BEGSR
019000110112     c
019100110114     c                   setoff                                       28
019200110112     c
019300110112     c     for02         tag
019400110114     c                   eval      c01rcd=c01csr
019500110114     c
019600110708     c  n28              write     tb88z03
019700110708     c                   exfmt     tb88c03
019800110112     C                   SETOFF                                       289001
019900110112     C                   SETOFF                                       29
020000110112     c                   clear                   v1cmsg
020100110112     C* F3 - FINE
020200110812    1c                   if        *inkc
020300110809     c                   exsr      ContrTAB
020400110812     c
020500110812     c* Errore --> modifiche wffettuate finestra di avviso
020600110812    2c                   if        *in90
020700110812     c                   setoff                                       kc
020800110812    3c                   dow       not *inkc and not *inkf
020900110812     c                   exfmt     tb88w07
021000110812    3c                   enddo
021100110812     c
021200110812    2c                   endif
021300110809     c
021400110812     c* Premuto o ripremuto f3
021500110812    2c                   if        *inkc
021600110809     C                   eval      $fine='1'
021700110809     c                   leavesr
021800110812    2c                   endif
021900110812    1c                   endif
022000110812     c
022100110708     C* F7 - ricerca per cliente unificante
022200110708     c                   if        *inkg
022300110708     c                   exsr      RicercaUnif
022400110127     c                   seton                                        90
022500110127     c                   goto      for02
022600110127     c                   endif
022700110708     c
022800110708     C* F10- Inserimento riga vuota
022900110708     c                   if        *inkj
023000110708     c                   exsr      InserRiga
023100110708     c                   seton                                        90
023200110708     c                   goto      for02
023300110708     c                   endif
023400110120     c
023500110120     c* ROLLUP, solo se ultima riga piena
023600110120     c                   if        *in37
023700110708     c     totnrr        chain     tb88s03
023800110120     c                   if        %found and  v3cksc>=*zeros
023900110120     c                   eval      nrr1=totnrr
024000110120     c                   exsr      righevuote
024100110120     c                   else
024200110120     c                   seton                                        289029
024300110120     c                   eval      v1cmsg=msg(4)
024400110120     c                   exsr      Aggiosfl
024500110120     c                   endif
024600110120     c                   goto      for02
024700110120     c                   endif
024800110112     c*
024900110112     c                   exsr      ctrd02
025000110112     c
025100110812     c                   if        *in90 or (not *inkf  and not *inkb and
025200110812     c                                       not *inkk)
025300110112     c                   goto      for02
025400110112     c                   endif
025500110810     c
025600110810     c* F2 - controlla unificanti
025700110810     c                   if        *inkb
025800110810     c                   exsr      Gessfl05
025900110810     c                   goto      for02
026000110810     c                   endif
026100110812     c* F6 o F11 - conferma
026200110812     c                   if        *inkf  or *inkk
026300110112     c                   exsr      ScriviTAB
026400110112     c                   endif
026500110812     c* F11 - Stampa
026600110812     c                   if        *inkk
026700110812     c                   exsr      StampaTab
026800110812     c                   endif
026900110112     c
027000110112     c                   ENDSR
027100110708      *-----------------------------------------------------***********
027200110708      * ricerca ed inserimento per cliente unificante
027300110708      *-----------------------------------------------------***********
027400110708     C     RicercaUnif   BEGSR
027500110708     c                   clear                   xclirds
027600110708     c                   eval      dxccap=dutkci
027700110708     c                   eval      dxcaut='AZ'
027800110708     c                   call      'XCLIR'
027900110708     c                   parm                    xclirds
028000110708     c
028100110708    1c                   if        dxcf03=*blanks and dxcerr=*blanks
028200110708
028300110708     C                   MOVEA     DXCKSC        xKSA                            *SCOMATTA CODICI
028400110708DO  2C     1             DO        30            xx
028500110708     C                   MOVE      xKSA(xx)      DSKSA                          *SCH ALF -> DS ALF
028600110708     C                   Z-ADD     DSKSN         xKSC(xx)                        *DS NUM -> SCH NUM
028700110708E   2C                   ENDDO
028800110708     c
028900110708    2c                   if        xksc(1)>*zeros
029000110708     c                   write     tb88z03
029100110708     c                   write     tb88c03
029200110708     c                   clear                   w4cconf
029300110803     c
029400110810     c                   dow       w4cconf='  '    and not *inkl
029500110708     c                   exfmt     tb88w04
029600110803     c                   enddo
029700110708     c
029800110810    3c                   if        w4cconf='SI'
029900110708     c* cerco la prima posizione vuota
030000110708     c                   eval      nrr1=totnrr+1
030100110708     c                   clear                   v3cksc
030200110812     c                   clear                   wPrimo            1
030300110708    4c                   dow       nrr1>0 and v3cksc<=*zeros
030400110708     c                   eval      nrr1=nrr1-1
030500110708     c     nrr1          chain     tb88s03
030600110708    4c                   enddo
030700110708     c
030800110708     c* trovato il primo record vuoto--> aggiungo da li in poi
030900110708     c                   eval      xx=1
031000110708    4c                   dow       xksc(xx)>*zeros
031100110708     c* Per ogni codice, carico e suoi figli
031200110708     c                   clear                   tibs10ds
031300110708     c                   eval      d10tle='ST'
031400110708     c                   eval      d10cod=xksc(xx)
031500110708     c                   eval      d10paf='F'
031600110708     c                   call      'TIBS10R'
031700110708     c                   parm                    tibs10ds
031800110708     c                   parm      'R'           d10ela            1
031900110708     c
032000110708     c                   if        d10err<>*blanks
032100110708     c* se non trovato cerco come Figlio
032200110708     c                   clear                   skc
032300110708     c                   eval      d10paf='P'
032400110708     c                   call      'TIBS10R'
032500110708     c                   parm                    tibs10ds
032600110708     c                   parm      'R'           d10ela            1
032700110708     c
032800110708     c
032900110708     c* se non ho trovato nulla --> imposto se stesso   padre=figlio
033000110708     c                   if        d10err<>*blanks
033100110708     c                   movel     d10cod        d10cop
033200110708     c                   movel     d10cod        skc(1)
033300110708     c                   endif
033400110708     c                   endif
033500110708     c
033600110708     c                   z-add     1             zz
033700110708    5c                   dow       skc(zz)>*zeros
033800110708     c                   eval      kksc=%int(%subst(skc(zz):5:7))
033900110708     c* carico solo se non già presente
034000110708     c                   eval      Indx=%lookup(kksc:ksc)
034100110708     c
034200110708   5ac                   if        Indx=0
034300110708     c                   eval      nrr1=nrr1+1
034400110708     c
034500110809     c* se tutte le righe scritte sono piene--> aggiungo una pagina
034600110708    6c                   if        nrr1>totnrr
034700110708     c                   eval      wnrr =nrr1
034800110708     c                   eval      nrr1=totnrr
034900110708     c
035000110708     c                   exsr      righevuote
035100110708     c                   eval      nrr1=wnrr
035200110708    6c                   endif
035300110708     c
035400110708     c     nrr1          chain     tb88s03
035500110708    6c                   if        %found
035600110708     c                   movel     kksc          v3cksc
035700110708     c                   clear                   v3dksc
035800110708     c                   seton                                        29
035900110708     c                   eval      v3dksc='--> INSERITO'
036000110708     c                   move      d10cop        v3cuni
036100110708     c                   exsr      Decodi
036200110708     c                   exsr      Aggiosfl
036300110812     c
036400110812     c* Posizionamento sul primo record inserito
036500110812     c                   if        Wprimo=' '
036600110812     c                   eval      c01csr=nrr1
036700110812     c                   eval      Wprimo='S'
036800110812     c                   endif
036900110812     c
037000110708    6c                   endif
037100110708   5ac                   endif
037200110708
037300110708     c                   eval      zz=zz+1
037400110708    5c                   enddo
037500110708
037600110708     c                   eval      xx=xx+1
037700110708    4c                   enddo
037800110708     c
037900110708    3c                   endif
038000110708    2c                   endif
038100110708    1c                   endif
038200110708     c*
038300110708     c                   ENDSR
038400110112      *-----------------------------------------------------***********
038500110127      * Inserimento riga vuota
038600110112      *-----------------------------------------------------***********
038700110127     C     InserRiga     BEGSR
038800110127     c
038900110127     c* Se ultima riga piena, aggiungo una pagina, poi sposto
039000110127     c                   eval      savcsr=c01csr
039100110127     c
039200110708     c     totnrr        chain     tb88s03
039300110127    1c                   if        %found and  v3cksc>=*zeros
039400110127     c                   eval      nrr1=totnrr
039500110127     c                   exsr      righevuote
039600110127    1c                   endif
039700110127
039800110809     c* sposto la penultima nell'ultima e così via, fino al posizionamento record
039900110127     c                   eval      nrr1=totnrr-1
040000110127     c
040100110127    1c                   dow       nrr1>savcsr
040200110127     c
040300110708     c     nrr1          chain     tb88s03
040400110127    2c                   if        %found
040500110809     c                   movel     v3hksc        savhksc
040600110809     c                   movel     v3cksc        savksc
040700110127     c                   movel     v3dksc        savdksc
040800110708     c                   movel     v3cuni        savuni
040900110708     c                   movel     v3duni        savduni
041000110127     c                   eval      wnrr=nrr1+1
041100110708     c     wnrr          chain     tb88s03
041200110809     c                   clear                   v3hksc
041300110809     c                   clear                   v3cksc
041400110127     c                   clear                   v3dksc
041500110708     c                   clear                   v3cuni
041600110708     c                   clear                   v3duni
041700110809     c                   eval      v3hksc=savhksc
041800110809     c                   eval      v3cksc=savksc
041900110127     c                   eval      v3dksc=savdksc
042000110708     c                   eval      v3cuni=savuni
042100110708     c                   eval      v3duni=savduni
042200110127     c                   exsr      Aggiosfl
042300110127    2c                   endif
042400110127     c
042500110127     c                   eval      nrr1=nrr1-2
042600110127     c                   enddo
042700110127     c
042800110127     c* clearo la riga sotto al F10
042900110127     c                   eval      wnrr=nrr1+1
043000110708     c     wnrr          chain     tb88s03
043100110127     c                   clear                   v3cksc
043200110809     c                   clear                   v3hksc
043300110127     c                   clear                   v3dksc
043400110708     c                   clear                   v3cuni
043500110708     c                   clear                   v3duni
043600110127     c                   exsr      Aggiosfl
043700110127     c
043800110127     c                   eval      c01csr=savcsr
043900110127     c                   ENDSR
044000110127     c
044100110127      *-----------------------------------------------------***********
044200110127      * controllo dati sfl
044300110127      *-----------------------------------------------------***********
044400110127     C     ctrd02        BEGSR
044500110809     c                   clear                   forzavar
044600110112     c*
044700110112     c                   eval      nrr1=1
044800110112     c                   z-add     1             yy
044900110809     c                   z-add     1             zz
045000110112     c                   clear                   ksc
045100110112     c*
045200110708    0c                   dow       nrr1<=totnrr
045300110120     c
045400110708     c     nrr1          chain     tb88s03
045500110810   0ac                   if        %found
045600110112     C*
045700110112     c     '?'           scan      v3cksc                                 90
045800110112    1c                   if        *in90
045900110112     c                   movel     rsut          paxdut           30
046000110112     c                   movel(p)  v3dksc        paxdmt           48
046100110112      * PAXSTA=9 ESCLUDO ANNULLATI
046200110112     C                   Z-ADD     9             PAXSTA
046300110112     c                   movel     dutkci        paxccc            4 0
046400110112     c                   clear                   paxdit
046500110112     C                   z-add     1             paxnum
046600110112     C                   CALL      'XALFA3BR'
046700110112     C                   PARM                    PAXDUT
046800110112     C                   PARM      1             CODUT             1 0
046900110112     C                   PARM                    PAXDMT
047000110112     C                   PARM                    PAXCCC
047100110112     C                   PARM                    PAXSTA            1 0
047200110112     C                   PARM                    PAXFLR           90
047300110112     C                   PARM                    PAXDIT            3
047400110112     C                   PARM                    PAXNUM            2 0
047500110112     C                   PARM                    PAXKCM           80
047600110112     C                   PARM                    PAXKSM          140
047700110112     C                   PARM                    PAXKDM           60
047800110112    2C     PAXSTA        IFGT      -1
047900110112     C                   MOVEL     paxksm        v3cksc
048000110112     C                   MOVEL     paxdmt        v3dksc
048100110803     c                   else
048200110803     c                   clear                   v3cksc
048300110112     c                   endif
048400110112    2c                   exsr      Aggiosfl
048500110112     c                   leavesr
048600110112    1c                   endif
048700110112     c
048800110112    1c                   if        v3cksc<>*blanks and v3cksc<>*zeros
048900110120     c                   testn                   v3cksc               35
049000110112     c
049100110120    2c                   if        not *in35  or %subst(v3cksc:7:1)<'0'
049200110114     c                   seton                                        29
049300110112     c                   eval      v1cmsg=msg(2)
049400110112     c                   exsr      Aggiosfl
049500110112     c                   leavesr
049600110112    2c                   endif
049700110112     c
049800110112     c                   movel     v3cksc        kksc
049900110112     c     kaco          chain     cnaco00f
050000110112    2c                   if        %found(cnaco00f)
050100110112     c                   movel     acorag        v3dksc
050200110112   x2c                   else
050300110120     c                   seton                                        29
050400110112     c                   eval      v1cmsg=msg(2)
050500110112     c                   exsr      Aggiosfl
050600110112     c                   leavesr
050700110112    2c                   endif
050800110112     c
050900110112     c*  Verifico se già presente
051000110114     c     kksc          lookup    ksc                                    29
051100110708    2c                   if        *in29
051200110112     c                   eval      v1cmsg=msg(3)
051300110112     c                   exsr      Aggiosfl
051400110112     c                   leavesr
051500110112     c                   else
051600110112     c                   eval      ksc(yy)=kksc
051700110112     c                   eval      yy=yy+1
051800110809     c
051900110809     c* Memorizzo tutti gli unificanti per eventuale controllo
052000110810     c     v3cuni        lookup    unif                                   35
052100110810    2c                   if        not *in35
052200110810     c                   eval      unif(zz) =v3cuni
052300110810     c                   eval      unifd(zz)=v3duni
052400110809     c                   eval      zz=zz+1
052500110809    2c                   endif
052600110809     c
052700110112    2c                   endif
052800110810     c
052900110810     c                   if        v3cksc<>v3hksc
053000110708     c                   clear                   v3cuni
053100110810     c                   clear                   v3hvaru
053200110708     c                   exsr      Decodi
053300110810     c*
053400110810     c                   else
053500110810     c* se uguale do msg forzanbile per cambio codic unificante
053600110810     c                   if        v3hvaru='S'
053700110810     c                   seton                                        29
053800110810     c                   eval      v1cmsg=msg(8)
053900110810     c                   eval      v3hvaru='X'
054000110810     c                   exsr      Aggiosfl
054100110810     c                   leavesr
054200110810    2c                   endif
054300110810     c                   endif
054400110114     c*
054500110114     c                   exsr      Aggiosfl
054600110112     c*
054700110708   x1c                   else
054800110112     c                   clear                   v3dksc
054900110708     c                   clear                   v3duni
055000110708     c                   clear                   v3cuni
055100110112     c                   exsr      Aggiosfl
055200110112    1c                   endif
055300110120     c
055400110708   0ac                   endif
055500110112     c
055600110112     c                   eval      nrr1=1+nrr1
055700110112     c*
055800110708    0c                   enddo
055900110120     c                   z-add     1             nrr1
056000110112     c
056100110112     c                   ENDSR
056200110112      *-----------------------------------------------------***********
056300110112      * Aggiornamento tabella
056400110112      *-----------------------------------------------------***********
056500110112     C     ScriviTAB     BEGSR
056600110112     c*
056700110112     c* Intanto cancello tutti i record
056800110112     c
056900110112     c/exec sql
057000110708     c+ delete from tntbe00f where tbecod='CXQ' and tbeke2<>'               '
057100110112     c/end-exec sql
057200110112     c
057300110112     c
057400110112     c                   eval      nrr1=1
057500110112     c                   z-add     1             yy
057600110112     c                   clear                   ksc
057700110112     c*
057800110708     c     nrr1          chain     tb88s03
057900110112    0c                   dow       %found
058000110112     c
058100110112    1c                   if        v3cksc>*zeros
058200110112     c                   clear                   tntbe000
058300110708     c                   eval      tbecod='CXQ'
058400110708     c                   eval      tbeapl='GE'
058500110810     c                   movel     v3cuni        tbeke1
058600110708     c                   eval      tbeke2=v3cksc
058700110810     c                   clear                   dcxq
058800110810     c                   movel     v3dksc        §cxqrsc
058900110810     c                   movel     v3duni        §cxqrsu
059000110810     c                   movel     dcxq          tbeuni
059100110112     c                   write     tntbe000
059200110114     c                   eval      yy=yy+1
059300110112     c                   endif
059400110809     c
059500110809     c                   eval      v3hksc=v3cksc
059600110809     c                   exsr      AGGIOSFL
059700110112     c
059800110112     c                   eval      nrr1=nrr1+1
059900110708     c     nrr1          chain     tb88s03
060000110112     c                   enddo
060100110112     c                   ENDSR
060200110809      *-----------------------------------------------------***********
060300110809      * controllo tutto il sfl per F3 per vedere se ci sono dei record
060400110809      *  variati che verranno persi
060500110809      *-----------------------------------------------------***********
060600110809     C     ContrTAB      BEGSR
060700110809     c
060800110809     c                   eval      nrr1=1
060900110809     c                   z-add     1             yy
061000110809     c*
061100110809     c     nrr1          chain     tb88s03
061200110809    0c                   dow       %found
061300110809     c
061400110809     c                   if        v3cksc=*zeros
061500110809     c                   clear                   v3cksc
061600110809     c                   endif
061700110809     c                   if        v3hksc=*zeros
061800110809     c                   clear                   v3hksc
061900110809     c                   endif
062000110809     c
062100110809     c                   if        v3cksc<>v3hksc
062200110812     c                   seton                                        90
062300110809     c                   eval      Forzavar='S'
062400110809     c                   leavesr
062500110809     c                   endif
062600110809     c
062700110809     c                   eval      nrr1=nrr1+1
062800110809     c     nrr1          chain     tb88s03
062900110809     c                   enddo
063000110809     c                   ENDSR
063100110810      *-----------------------------------------------------***********
063200110810     c*  Gestione SFL 05 - controlla unificanti
063300110810      *-----------------------------------------------------***********
063400110810     c     Gessfl05      BEGSR
063500110810     c                   exsr      contrUnif
063600110810     c
063700110810     c                   setoff                                       28
063800110810     c* se non ho caricato nemmeno un record videata vuota
063900110810     c                   if        nrr5=0
064000110810     c                   seton                                        30
064100110810     c                   write     tb88c06
064200110810     c                   exfmt     tb88c05
064300110810     c
064400110810     c                   setoff                                       30
064500110810     c
064600110810     c                   else
064700110810     c
064800110810     c     for05         tag
064900110810     c                   eval      c05rcd=c05csr
065000110810     c
065100110810     c  n28              write     tb88z05
065200110810     c*
065300110810     c                   exfmt     tb88c05
065400110810     C                   SETOFF                                       2890
065500110810     C                   SETOFF                                       40
065600110810     c                   clear                   v1cmsg
065700110810     C* F12- ritrono
065800110810     c                   if        *inkl
065900110810     c                   leavesr
066000110810     c                   endif
066100110810     c
066200110810     c                   exsr      ctrd05
066300110810     c
066400110810     c                   if        *in90 or not *inkf
066500110810     c                   goto      for05
066600110810     c                   endif
066700110810     c* F6 - conferma
066800110810     c                   if        *inkf
066900110810     c                   exsr      ScriviTAB05
067000110810     c                   endif
067100110810     c
067200110810     c                   endif
067300110810     c
067400110810     c                   ENDSR
067500110809      *-----------------------------------------------------***********
067600110809      * Controllo i codici unificanti se per caso sono variati i legami
067700110809      *  li evidenzio
067800110809      *-----------------------------------------------------***********
067900110809     C     ContrUNIF     BEGSR
068000110809     c* Elaboro la schiera degli unifficanti e controllo rispoetto a
068100110809     c*  quelli inseriti in tabella se ce ne sono in più o meno
068200110809     C* PULIZIA SFL
068300110809     C                   SETON                                        3031
068400110809     C                   WRITE     tb88C05
068500110809     C                   SETOFF                                       3031
068600110809     c                   clear                   nrr5
068700110809     c                   clear                   totnrr5           5 0
068800110809     c
068900110809     c                   z-add     1             xx
069000110809    1c                   dow       unif(xx)>0
069100110809     c                   clear                   tibs10ds
069200110809     c                   eval      d10tle='ST'
069300110809     c                   eval      d10cod=unif(xx)
069400110809     c                   eval      d10paf='F'
069500110809     c                   call      'TIBS10R'
069600110809     c                   parm                    tibs10ds
069700110809     c                   parm      'R'           d10ela            1
069800110809     c
069900110809    2c                   if        d10err<>*blanks
070000110809     c* se non trovato cerco come Figlio
070100110809     c                   clear                   skc
070200110809     c                   eval      d10paf='P'
070300110809     c                   call      'TIBS10R'
070400110809     c                   parm                    tibs10ds
070500110809     c                   parm      'R'           d10ela            1
070600110809     c
070700110809     c
070800110809     c* se non ho trovato nulla --> imposto se stesso   padre=figlio
070900110809    3c                   if        d10err<>*blanks
071000110809     c                   movel     d10cod        d10cop
071100110809     c                   movel     d10cod        skc(1)
071200110809    3c                   endif
071300110809    2c                   endif
071400110809     c
071500110809     c                   z-add     1             zz
071600110809    2c                   dow       skc(zz)>*zeros
071700110809     c                   eval      kksc=%int(%subst(skc(zz):5:7))
071800110809     c
071900110809     c* Verifico se già presente nella tabella
072000110810     c     kksc          lookup    ksc                                    35
072100110810    3c                   if        not *in35
072200110810     c                   setoff                                       10
072300110810     c                   eval      v5dmod='MANCANTE '
072400110809     c                   eval      v5cuni=unif(xx)
072500110810     c                   eval      v5duni=unifd(xx)
072600110809     c                   eval      v5cksc=kksc
072700110809     c     kaco          chain     cnaco00f
072800110809    4c                   if        %found(cnaco00f)
072900110809     c                   movel     acorag        v5dksc
073000110809    4c                   endif
073100110809     c                   add       1             nrr5
073200110809     c                   write     tb88s05
073300110809    3c                   endif
073400110809     c
073500110809     C                   EVAL      zz=zz+1
073600110809    2c                   enddo
073700110809     c
073800110809     C                   EVAL      xx=xx+1
073900110809    1c                   enddo
074000110809     c
074100110810     c                   eval      c05csr=1
074200110810     c                   z-add     nrr5          totnrr5
074300110809     c                   ENDSR
074400110810      *-----------------------------------------------------***********
074500110810      * controllo dati sfl 05
074600110810      *-----------------------------------------------------***********
074700110810     C     ctrd05        BEGSR
074800110810     c*
074900110810     c                   eval      nrr5=1
075000110810     c                   z-add     1             yy
075100110810     c                   z-add     1             zz
075200110810     c                   clear                   wopz
075300110810     c*
075400110810    0c                   dow       nrr5<=totnrr5
075500110810     c
075600110810     c     nrr5          chain     tb88s05
075700110810    1c                   if        %found
075800110810     c* Per aggiunta solo "I"
075900110810    2c                   if        v5copz='I' and %subst(v5dmod:1:1)='E'
076000110810     c                   seton                                        40
076100110810     c                   eval      v1cmsg=msg(6)
076200110810     c                   exsr      Aggiosfl5
076300110810     c                   leavesr
076400110810    2c                   endif
076500110810     c* Per eliminaz. solo "A"
076600110810    2c                   if        v5copz='A' and %subst(v5dmod:1:1)='A'
076700110810     c                   seton                                        40
076800110810     c                   eval      v1cmsg=msg(1)
076900110810     c                   exsr      Aggiosfl5
077000110810     c                   leavesr
077100110810    2c                   endif
077200110810     c
077300110810     c* se premuto F6 ci deve essere almeno una "I" o una "A"
077400110810    2c                   if        v5copz<>*blanks
077500110810     c                   eval      wopz=v5copz
077600110810    2c                   endif
077700110810     c
077800110810    1c                   endif
077900110810     c
078000110810     c                   eval      nrr5=1+nrr5
078100110810     c*
078200110810    0c                   enddo
078300110810     c
078400110810     c* se premuto F6 ci deve essere almeno una "I" o una "A"
078500110810     c   kf              if        wopz=' '
078600110810     c                   z-add     1             nrr5
078700110810     c                   seton                                        40
078800110810     c                   eval      v1cmsg=msg(7)
078900110810     c                   exsr      Aggiosfl5
079000110810    2c                   endif
079100110810     c
079200110810     c                   ENDSR
079300110810      *-----------------------------------------------------***********
079400110810      * Aggiunta/elimiinazione recod da sfl
079500110810      *-----------------------------------------------------***********
079600110810     C     ScriviTAB05   BEGSR
079700110810     c
079800110810     c                   eval      nrr5=1
079900110810     c                   z-add     1             yy
080000110810     c                   clear                   ksc
080100110812     c                   clear                   wPrimo
080200110810     c*
080300110810     c     nrr5          chain     tb88s05
080400110810    0c                   dow       %found
080500110810     c
080600110810    1c                   if        v5copz='I'
080700110810     c
080800110810     c* cerco la prima posizione vuota
080900110810     c                   eval      nrr1=totnrr+1
081000110810     c                   clear                   v3cksc
081100110810    2c                   dow       nrr1>0 and v3cksc<=*zeros
081200110810     c                   eval      nrr1=nrr1-1
081300110810     c     nrr1          chain     tb88s03
081400110810    2c                   enddo
081500110810     c
081600110810     c                   eval      nrr1=nrr1+1
081700110810     c
081800110810     c* se tutte le righe scritte sono piene--> aggiungo una pagina
081900110810    2c                   if        nrr1>totnrr
082000110810     c                   eval      wnrr =nrr1
082100110810     c                   eval      nrr1=totnrr
082200110810     c
082300110810     c                   exsr      righevuote
082400110810     c                   eval      nrr1=wnrr
082500110810    2c                   endif
082600110810     c
082700110810     c     nrr1          chain     tb88s03
082800110810    2c                   if        %found
082900110810     c                   movel     v5cksc        v3cksc
083000110810     c                   movel     v5dksc        v3dksc
083100110810     c                   seton                                        29
083200110810     c                   eval      v3dksc='--> INSERITO'
083300110810     c                   move      v5cuni        v3cuni
083400110810     c                   move      v5duni        v3duni
083500110810     c                   exsr      Aggiosfl
083600110812     c* Posizionamento sul primo record inserito
083700110812     c                   if        Wprimo=' '
083800110812     c                   eval      c01csr=nrr1
083900110812     c                   eval      Wprimo='S'
084000110812     c                   endif
084100110812     c
084200110810    2c                   endif
084300110810    1c                   endif
084400110810     c
084500110810     c                   eval      nrr5=nrr5+1
084600110810     c     nrr5          chain     tb88s05
084700110810     c                   enddo
084800110810     c
084900110810     c                   ENDSR
085000110112      *****************************************************************
085100110112     c* Aggioranemnto sfl
085200110112      *****************************************************************
085300110112     C     Aggiosfl      BEGSR
085400110112     c                   if        v1cmsg<>*blanks
085500110112     c                   seton                                        9028
085600110112     c                   eval      c01csr=nrr1
085700110112     c                   endif
085800110810     c*
085900110810     c                   if        v3hvaru<>*blanks
086000110810     c                   seton                                        11
086100110810     c                   else
086200110810     c                   setoff                                       11
086300110810     c                   endif
086400110112     c
086500110708     c                   update    tb88s03
086600110810     c                   setoff                                       29
086700110112     c
086800110112     c                   ENDSR
086900110810      *****************************************************************
087000110810     c* Aggioranemnto sfl5
087100110810      *****************************************************************
087200110810     C     Aggiosfl5     BEGSR
087300110810     c                   if        v1cmsg<>*blanks
087400110810     c                   seton                                        9028
087500110810     c                   eval      c05csr=nrr5
087600110810     c                   endif
087700110810     c                   if        %subst(v5dmod:1:1)='E'
087800110810     c                   seton                                        10
087900110810     c                   else
088000110810     c                   setoff                                       10
088100110810     c                   endif
088200110810     c
088300110810     c                   update    tb88s05
088400110810     c                   setoff                                       40
088500110810     c
088600110810     c                   ENDSR
088700110708      *****************************************************************
088800110708     c     Decodi        BEGSR
088900110708    1c                   if        v3dksc=*blanks
089000110708     c                   movel     v3cksc        kksc
089100110708     c     kaco          chain     cnaco00f
089200110708    2c                   if        %found(cnaco00f)
089300110708     c                   movel     acorag        v3dksc
089400110708    2c                   endif
089500110708    1c                   endif
089600110708     c
089700110708     c* Reperisco codice Padre
089800110708    1c                   if        v3cuni=0
089900110708     c                   clear                   tibs10ds
090000110708     c                   eval      d10tle='ST'
090100110708     c                   eval      d10cod=acoksc
090200110708     c                   eval      d10paf='P'
090300110708     c                   call      'TIBS10R'
090400110708     c                   parm                    tibs10ds
090500110708     c                   parm      'R'           d10ela            1
090600110708     c
090700110708     c* Passato tipo elaborazione per non eseguire la chiusura file tutte le volte
090800110708     c*  (li chiude per " " o "C" )
090900110708    2c                   if        d10err=*blanks and d10cop>*zeros
091000110708     c                   move      d10cop        v3cuni
091100110708     c                   else
091200110708     c                   movel     v3cksc        v3cuni
091300110708    2c                   endif
091400110708    1c                   endif
091500110708     c
091600110708    1c                   if        v3cuni>*zeros
091700110708     c                   movel     v3cuni        kksc
091800110708     c     kaco          chain     cnaco00f
091900110708    2c                   if        %found(cnaco00f)
092000110708     c                   movel     acorag        v3duni
092100110812     c                   movel     acorag        P3duni
092200110708    2c                   endif
092300110708    1c                   endif
092400110708     c
092500110708     c                   ENDSR
092600110812      *-----------------------------------------------------***********
092700110812      * Stampa tabella "CXQ"
092800110812      *-----------------------------------------------------***********
092900110812     C     StampaTab     BEGSR
093000110812     c                   seton                                        OF
093100110812     c
093200110812     c     kcod          setll     tntbe01l
093300110812     c     kcod          reade(n)  tntbe01l
093400110812     c
093500110812    1c                   dow       not %eof(tntbe01l)
093600110812     c
093700110812     c                   movel     tbeke2        v3cksc
093800110812     c* Chain su cnaco per la ragione sociale
093900110812    2c                   if        v3cksc>*zeros
094000110812     c                   clear                   V3dksc
094100110812     c                   clear                   v3cuni
094200110812     c                   movel     tbeuni        dCXQ
094300110812     c                   exsr      Decodi
094400110812     c
094500110812     c* Se cambiato codice padre
094600110812     c                   if        %subst(tbeke1:1:7)>*zeros
094700110812     c                   eval      wunif=%int(%subst(tbeke1:1:7))
094800110812     c                   else
094900110812     c                   clear                   wunif
095000110812     c                   endif
095100110812     c
095200110812    3c                   if        wunif  >0 and wunif  <>v3cuni
095300110812     c                   movel     wunif         kksc
095400110812     c     kaco          chain     cnaco00f
095500110812    2c                   if        not %found(cnaco00f)
095600110812     c                   clear                   acorag
095700110812    2c                   endif
095800110812     c
095900110812     c                   eval      %subst(P3duni:13:33)=' in tabella con-->'+
096000110812     c                                   %editc(wunif:'X')+ Acorag
096100110812     c                   seton                                        11
096200110812    3c                   endif
096300110812    2c                   endif
096400110812     c*
096500110812     C                   IF        *INOF
096600110812     C                   EXCEPT    TESTA
096700110812     C                   SETOFF                                       OF
096800110812     C                   ENDIF
096900110812     C
097000110812     C                   EXCEPT    DET
097100110812     c
097200110812     c     kcod          reade(n)  tntbe01l
097300110812    1c                   enddo
097400110812     c
097500110812     c                   ENDSR
097600110112      *****************************************************************
097700110112     C     *INZSR        BEGSR
097800110112      *
097900110112     C     *ENTRY        PLIST
098000110112     C                   PARM                    KPJBA
098100110112      *
098200110112      *
098300110112      * accesso tntbe
098400110120     c     ktab          klist
098500110120     C                   kfld                    kkut
098600110120     C                   kfld                    kcodt
098700110120     C                   kfld                    kkey
098800110112     c     kaco          klist
098900110112     C                   kfld                    kkut
099000110112     C                   kfld                    dutkci
099100110112     C                   kfld                    kksc
099200110112     c
099300110112     c                   exsr      sr_DatiJob
099400110812     c
099500110812     C                   TIME                    WTIME            14 0
099600110812     C                   MOVE      WTIME         WDATE             8 0
099700110812     C                   MOVEL     WTIME         UTIME             6 0
099800110112      *
099900110112     C                   ENDSR
100000110112      *---------------------------------------------------------------*
100100110112      *?Reperimento dati del job (Utente/Operativi)                  ?*
100200110112      *---------------------------------------------------------------*
100300110112     c     sr_DatiJob    BEGSR
100400110112      *
100500110112     c     *dtaara       define    §azute        AZUTEds
100600110112     c     *dtaara       define    §datiute      dDatiUte
100700110112      *
100800110112     c                   in(E)     *dtaara
100900110112     c                   IF        %Error or RSUT = *blanks
101000110112     c                   clear                   TIBS34ds
101100110112     c                   call      'TIBS34R'
101200110112     c                   parm                    TIBS34ds
101300110112     c                   in        *dtaara
101400110112     c                   ENDIF
101500110112      *
101600110112     c                   ENDSR
101700110812     c*----------------------------------------------------------------------
101800110812     OQSYSPRT   E            TESTA            01
101900110812     O                       SK(1)               66
102000110812     O                       SK(2)              132
102100110812     O                       RSUT                20
102200110812     O                       WDATE              118 '  /  /    '
102300110812     O                       PAGE          Z    132
102400110812     O          E            TESTA            02
102500110812     O                       KNSIF               10
102600110812     O                       KNMUS               22
102700110812     O                       utimE              116 '  :  :  '
102800110812     O          E            TESTA            03
102900110812     O                       SK(5)               66
103000110812     O                       SK(5)              132
103100110812     O*
103200110812     O          E            TESTA            04
103300110812     O                       SK(3)               66
103400110812     O                       SK(4)              132
103500110812     O*
103600110812     O          E            TESTA            05
103700110812     O                       SK(5)               66
103800110812     O                       SK(5)              132
103900110812     O
104000110812     O          E            DET         1
104100110812     O                       V3CUNI
104200110812     O                       P3DUNI           +   2
104300110812     O                       V3CKSC           +   4
104400110812     O                       V3DKSC           +   2
104500020805      **************************************************************************
104600960302** SCHIERA MSG - MESSAGGI DI ERRORE
104700110810Codice  AGGIUNTO  all'Unificante: possibile solo "I-inserimento"                1
104800110810Codice cliente errato o inesistente                                             2
104900110810Codice cliente già inserito                                                     3
105000110810Scorrimento non ammesso: ci sono ancora righe vuote nella pagina                4
105100110810ATTENZIONE alcune modifiche andranno perdute.F3 uscita senza conferma F6-Confer 5
105200110810Codice  ELIMINATO  dall'unificante: possibile solo "A-annullamento"             6
105300110810Uscire con F12 o effettuare almeno una scelta e premere F6                      7
105400110829VARIATO codice Unificante: verificare se eliminare il cod.cliente.ENTER forza   8
105500110812**
105600110812XXXXXXXXXXXXXXXXXXXX                     ** STAMPA TABELLA CLIENTI   1
105700110812 PARTICOLARI **               TNTB88R     XX.XX.XXXX     PAG. XXXX   2
105800110812Cliente Unificante attuale                                 Codice    3
105900110812cliente                                                              4
106000110812------------------------------------------------------------------   5
