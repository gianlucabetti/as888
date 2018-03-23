000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200100713      * tibs93R *----------------------------------------------------*
000300100713      *         - cambio terminal di partenza/arrivo
000400940915      *--------------------------------------------------------------*
000500100713     FTIBS93D   CF   E             WORKSTN
000600100713     f                                     sfile(bs93s01 : nrr1)
000700951013     FAZORG01L  IF   E           K DISK
000800100714     FAZcae03L  uF a E           K DISK
000900100714     FAZcae05L  uF a E           K DISK    rename(azcae000:azcae005)
001000020805      *
001100020805      * DEFINIZIONE SCHIERE
001200960302     D*
001300100714     D MSG             S             78    DIM(9) CTDATA PERRCD(1)
001400040917      * DS per FNLV55R - Reperimento Terminal di Partenza e di Arrivo
001500040917     D FNLV55DS      E DS
001600040917     D KPJBA         E DS
001700040917     D CNCR80        E DS
001800040917     D UT§DSE0F      E DS
001900080529     d
002000080529     D WLBDAT          DS                  INZ
002100080529     D  G02DAT                 1      8  0
002200080529     D  G02INV                 9     16  0
002300080529     D  G02ERR                17     17
002400080529     D  G02TGI                18     22  0
002500080529     d
002600050103     d Dataiso         s               d   datfmt(*iso)
002700050103     d Dataeur         s               d   datfmt(*eur)
002800080529     d datadmy         s               d   datfmt(*dmy)
002900100713     d datasys         s               d   inz(*sys)
003000050103     d
003100100713     d codfil          s                   like(orgfil)
003200100713     d nrr1            s              4  0 inz
003300100714     d kepa            s                   like(caeepa)
003400100714     d ktfe            s                   like(caetfe)
003500100714     d ktfp            s                   like(caetfp)
003600100714     d ktfa            s                   like(caetfa)
003700041220     d
003800050103     C**********************************************************************
003900040917     C     *ENTRY        PLIST
004000040917     C                   PARM                    KPJBA
004100080529     c*
004200080529     C                   Z-ADD     1             CODUT
004300080529     C                   CALL      'X§PARUT'
004400080529     C                   PARM                    UT§DSE0F
004500080529     C                   MOVEL     REC80         CNCR80
004600080529     C                   MOVEL     RAGUT         VIDRSU
004700100714     c
004800100714     c     *iso          movel     datasys       dataoggi          8 0
004900100713     c
005000100713     c                   exsr      inzs01
005100080529     c
005200100713     c                   clear                   viddsca
005300100713     c                   clear                   vidddec
005400100714     c                   eval      vidrip='SI'
005500100713     c                   eval      nrr1=1
005600100713     c                   eval      c01rcd=1
005700100713     c                   eval      c01csr=1
005800080529     c
005900080529     c     inizio        tag
006000100713     c                   write     bs93t01
006100100713     c                   write     bs93p01
006200040917     c
006300040917     C     FOR01         TAG
006400000828      *
006500100713     C                   EXFMT     BS93c01
006600080529     C                   SETOFF                                       289001
006700100713     C                   SETOFF                                       014041
006800100713     C                   SETOFF                                       42
006900080529     c                   clear                   rivisual          1
007000040917     C* F3 - FINE
007100040917     C   KC              GOTO      FINE
007200080529     c
007300050104     C* F7 - manutenzione organigramma
007400050104     c                   if        *inkg
007500050110     C                   call      'TIBS84C'
007600050104     c                   parm                    kpjba
007700080529     c                   goto      inizio
007800050104     c                   endif
007900041220      *  CONTROLLI
008000040917     C                   EXSR      CTRD01
008100080529     c                   if        *in90
008200080529     c                   goto      for01
008300080529     c                   else
008400080529     c                   if        not *inkf or rivisual='S'
008500080529     c                   goto      inizio
008600080529     c                   endif
008700080529     c                   endif
008800080529     c
008900080529     c* F6 - conferma
009000080529     c                   if        *inkf
009100100714     c                   exsr      CambiaTer
009200080529     c                   endif
009300900515      *
009400000000     C     FINE          TAG
009500960129     C*
009600040917     c                   clear                   fnlv55ds
009700040917     C                   MOVEL     'C'           D55TLA
009800040917     C                   CALL      'FNLV55R'
009900040917     C                   PARM                    FNLV55DS
010000000000     C                   SETON                                        LR
010100100713      *-----------------------------------------------------***********
010200100713      * Inizializza SFL
010300100713      *-----------------------------------------------------***********
010400100713     C     Inzs01        BEGSR
010500100713     C* PULIZIA SFL
010600100713     C                   SETON                                        3031
010700100713     C                   WRITE     BS93C01
010800100713     C                   SETOFF                                       3031
010900100713     c                   clear                   nrr1
011000100713     c                   clear                   totnrr            5 0
011100100713     c                   clear                   s01fil
011200100713     c                   clear                   s01dfil
011300100713     c                   clear                   s01otfp
011400100713     c                   clear                   s01dotfp
011500100713     c                   clear                   s01otfa
011600100713     c                   clear                   s01dotfa
011700100713     c                   clear                   s01ntfp
011800100713     c                   clear                   s01dntfp
011900100713     c                   clear                   s01otfa
012000100713     c                   clear                   s01dntfa
012100100714     c                   dow       nrr1<38
012200100713     c                   eval      nrr1=nrr1+1
012300100713     c                   write     bs93s01
012400100713     c                   enddo
012500100713     c
012600100713     C                   ENDSR
012700080529      *-----------------------------------------------------***********
012800080529      * controllo data di elaborazione
012900080529      *-----------------------------------------------------***********
013000080529     C     Ctrd01        BEGSR
013100100713     c
013200100713     c* Periodo cambio : DAL
013300100713     C                   Z-ADD     vidddec       G02DAT
013400100713     C                   MOVEL     *BLANK        G02ERR
013500100713     C                   CALL      'XSRDA8'
013600100713     C                   PARM                    WLBDAT
013700100713     C* ERRATA
013800100713    1C     G02ERR        IFEQ      '1'
013900100713     c                   movel     msg(1)        vidmsg
014000100713     c                   seton                                        289002
014100100713     c                   leavesr
014200100713    1c                   endif
014300100713     c
014400100713     c                   movel     vidddec       w002a             2
014500100713    1c                   if        w002a='00'
014600100713     c     *dmy          move      vidddec       datadmy
014700100713     c                   move      datadmy       dataiso
014800100713     c                   else
014900100713     c     *eur          movel     vidddec       dataiso
015000100713    1c                   endif
015100100713     c                   movel     dataiso       amgddec           8 0
015200100713     c                   movel     dataiso       dataeur
015300100713     c                   movel     dataeur       vidddec
015400100714     c* sottraggo 1 alla data decorrenza
015500100714      * Calcolo decorrenza - 1 gg
015600100714     c                   subdur    1:*d          Dataiso
015700100714     c                   Move      Dataiso       wDECm1            8 0
015800100713     c
015900100713     c* La data decorrenza deve essere maggiore di oggi
016000100713    1c                   if        dataiso<datasys
016100100713     c                   movel     msg(2)        vidmsg
016200100713     c                   seton                                        289002
016300100713     c                   leavesr
016400100713     c                   endif
016500100713     c
016600100713     c* DATA SCADENZA
016700100713     C                   Z-ADD     viddsca       G02DAT
016800080529     C                   MOVEL     *BLANK        G02ERR
016900080529     C                   CALL      'XSRDA8'
017000080529     C                   PARM                    WLBDAT
017100080529     C* ERRATA
017200080529    1C     G02ERR        IFEQ      '1'
017300080529     c                   movel     msg(1)        vidmsg
017400080529     c                   seton                                        289001
017500100713     c                   leavesr
017600100713    1c                   endif
017700100713     c
017800100713     c                   movel     viddsca       w002a             2
017900100713    1c                   if        w002a='00'
018000100713     c     *dmy          move      viddsca       datadmy
018100080529     c                   move      datadmy       dataiso
018200080529     c                   else
018300100713     c     *eur          movel     viddsca       dataiso
018400100713    1c                   endif
018500100713     c                   movel     dataiso       amgdsca           8 0
018600080529     c                   movel     dataiso       dataeur
018700100713     c                   movel     dataeur       viddsca
018800100714      * Calcolo scadenza + 1 gg
018900100714     c                   adddur    1:*d          Dataiso
019000100714     c                   Move      Dataiso       wSCAp1            8 0
019100100713     c
019200100714     c* La data scadenza > data decorrenza
019300100713    1c                   if        amgdsca<amgddec
019400100713     c                   movel     msg(3)        vidmsg
019500100713     c                   seton                                        289001
019600100713     c                   leavesr
019700100713     c                   endif
019800100713     c
019900100713     c* controllo filiali SFL
020000100713     c                   eval      nrr1=1
020100100714    1c                   dow       nrr1<=38
020200100713     c     nrr1          chain     bs93s01
020300100713     c                   setoff                                       404142
020400100714     c                   setoff                                       1011
020500100713     c
020600100713     c* a cambio filiale pulisco i nuovi temrinal
020700100713    3c                   if        savfil<>s01fil
020800100713     c                   clear                   s01ntfp
020900100713     c                   clear                   s01dntfp
021000100713     c                   clear                   s01ntfa
021100100713     c                   clear                   s01dntfa
021200100713     c                   eval      savfil=s01fil
021300100714     c* posizionamento cursore sul 1° record dda scrivere il terminal
021400100714     c                   if        rivisual=' '
021500100714     c                   eval      rivisual='S'
021600100714     c                   if        s01fil>0
021700100714     c                   seton                                        41
021800100714    3c                   endif
021900100714    3c                   endif
022000100714     c* serve per non dare errore il primo giro di "manca nuovo terminal"
022100100714     c                   seton                                        11
022200100713    3c                   endif
022300100713     c
022400100713    2c                   if        s01fil>0
022500100713     c
022600100713     c                   movel     s01fil        codfil
022700100713     c                   exsr      ctrfil
022800100713    3c                   if        *in34
022900100713     c
023000100713     c                   movel     msg(4)        vidmsg
023100100713     c                   seton                                        289040
023200100713     c                   eval      c01rcd=nrr1
023300100713     c                   update    bs93s01
023400100713     c                   leavesr
023500100713     c                   else
023600100713     c                   movel     orgdes        s01dfil
023700100713    3c                   endif
023800100713     c
023900100713     c                   seton                                        10
024000100713     c* Propongo il suo terminal di partenza e di arrivo alla data di oggi
024100100713     c                   clear                   fnlv55ds
024200100713     c                   eval      d55lin=s01fil
024300100713     c     *iso          movel     datasys       d55drf
024400100713     c                   call      'FNLV55R'
024500100713     c                   parm                    fnlv55ds
024600100713     c
024700100713     c                   movel     d55tfp        s01otfp
024800100713     c                   movel     d55tfp        codfil
024900100713     c                   exsr      ctrfil
025000100713     c                   movel     orgdes        s01dotfp
025100100713     c
025200100713     c                   movel     d55tfa        s01otfa
025300100713     c                   movel     d55tfa        codfil
025400100713     c                   exsr      ctrfil
025500100713     c                   movel     orgdes        s01dotfa
025600100713     c
025700100713     c* Nuovo terminal di partenza
025800100714     c                   clear                   s01dntfp
025900100713    3c                   if        s01ntfp>0
026000100713     c                   movel     s01ntfp       codfil
026100100713     c                   exsr      ctrfil
026200100713    4c                   if        *in34
026300100713     c
026400100713     c                   movel     msg(5)        vidmsg
026500100713     c                   seton                                        289041
026600100713     c                   eval      c01rcd=nrr1
026700100713     c                   update    bs93s01
026800100713     c                   leavesr
026900100713     c                   else
027000100713     c                   movel     orgdes        s01dntfp
027100100713    4c                   endif
027200100713     c
027300100713     c* Verifico se è un terminal di partenza alla data decorrenza nuova
027400100713     c                   clear                   fnlv55ds
027500100713     c                   eval      d55tpt='P'
027600100713     c                   eval      d55lin=s01ntfp
027700100713     c                   eval      d55drf=amgddec
027800100713     c                   call      'FNLV55R'
027900100713     c                   parm                    fnlv55ds
028000100713    4c                   if        d55tfp<>d55lin or d55err<>' '
028100100714     c                   movel     msg(5)        vidmsg
028200100713     c                   seton                                        289041
028300100713     c                   update    bs93s01
028400100713     c                   leavesr
028500100713    4c                   endif
028600100714     c
028700100714     c* non deve essere uguale all'attuale
028800100714     c                   if        s01otfp=s01ntfp
028900100714     c                   movel     msg(8)        vidmsg
029000100714     c                   seton                                        289041
029100100714     c                   update    bs93s01
029200100714     c                   leavesr
029300100714    4c                   endif
029400100714     c
029500100713    3c                   endif
029600100713     c
029700100713     c* Nuovo terminal di arrivo
029800100714     c                   clear                   s01dntfa
029900100713    3c                   if        s01ntfa>0
030000100713     c                   movel     s01ntfa       codfil
030100100713     c                   exsr      ctrfil
030200100713    4c                   if        *in34
030300100713     c
030400100713     c                   movel     msg(6)        vidmsg
030500100713     c                   seton                                        289042
030600100713     c                   eval      c01rcd=nrr1
030700100713     c                   update    bs93s01
030800100713     c                   leavesr
030900100713     c                   else
031000100713     c                   movel     orgdes        s01dntfa
031100100713    4c                   endif
031200100713     c
031300100713     c* Verifico se è un terminal di partenza alla data decorrenza nuova
031400100713     c                   clear                   fnlv55ds
031500100713     c                   eval      d55tpt='A'
031600100713     c                   eval      d55lin=s01ntfa
031700100713     c                   eval      d55drf=amgddec
031800100713     c                   call      'FNLV55R'
031900100713     c                   parm                    fnlv55ds
032000100713    4c                   if        d55tfa<>d55lin or d55err<>' '
032100100713     c                   movel     msg(6)        vidmsg
032200100713     c                   seton                                        289042
032300100713     c                   eval      c01rcd=nrr1
032400100713     c                   update    bs93s01
032500100713     c                   leavesr
032600100713    4c                   endif
032700100714     c
032800100714     c* non deve essere uguale all'attuale
032900100714     c                   if        s01otfa=s01ntfa
033000100714     c                   movel     msg(9)        vidmsg
033100100714     c                   seton                                        289042
033200100714     c                   update    bs93s01
033300100714     c                   leavesr
033400100714    4c                   endif
033500100713    3c                   endif
033600100714     c
033700100714     c*   Uno dei due terminal obbligatori non al primo giro
033800100714    3c                   if        not *in11 and s01ntfp=0 and s01ntfa=0
033900100713     c                   movel     msg(7)        vidmsg
034000100713     c                   eval      c01rcd=nrr1
034100100713     c                   seton                                        289041
034200100713     c                   update    bs93s01
034300100713     c                   leavesr
034400100713    3c                   endif
034500100713     c
034600100714     c                   else
034700100714     c                   clear                   s01dfil
034800100713    2c                   endif
034900100713     c                   update    bs93s01
035000100713     c
035100100713     c                   add       1             nrr1
035200100713     c                   enddo
035300080529     c
035400080529     c                   ENDSR
035500951025     C**************************************************************************
035600951025     C*  CONTROLLO UNA FILIALE IN ORGANIGRAMMA
035700951025     C**************************************************************************
035800951025     C     CTRFIL        BEGSR
035900020213
036000951025     C     CODFIL        CHAIN     AZORG01L                           34
036100951025    1C  N34ORGFVA        IFNE      ' '
036200951025     C     ORGFAG        ORNE      'A'
036300951025     C     ORGFAG        ANDNE     'F'
036400951025     C                   SETON                                        34
036500951025    1C                   ENDIF
036600100713     c
036700100713     c                   if        *in34
036800100713     c                   clear                   orgdes
036900100713     c                   endif
037000951025     C*
037100951025     C                   ENDSR
037200100714      *-----------------------------------------------------***********
037300100714      * eseguo ikl cambio dei terminal
037400100714      *-----------------------------------------------------***********
037500100714     C     cambiaTer     BEGSR
037600100714     c
037700100714     c                   eval      nrr1=1
037800100714    1c                   dow       nrr1<=38
037900100714     c     nrr1          chain     bs93s01
038000100714     c
038100100714    2c                   if        s01fil>0
038200100714     c* Terminal di PARTENZA
038300100714    3c                   if        s01ntfp>0
038400100714     c* Leggo il terminal attuale e lo faccio scadere a data decorrenza-1
038500100714     c                   eval      kepa='P'
038600100714     c                   eval      ktfp=s01fil
038700100714     c                   eval      ktfe=s01otfp
038800100714     c                   eval      ktfa=000
038900100714     c     kcae          setll     azcae03l
039000100714     c     kcae          reade     azcae03l
039100100714    4c                   dow       not %eof(azcae03l)
039200100714    5c                   if        caedde<=dataoggi and caedsc>=dataoggi
039300100714     c                             and caeatb=' '
039400100714     c                   eval      caedsc=wdecM1
039500100714     c                   clear                   caeftr
039600100714     c                   update    azcae000
039700100714     c                   leave
039800100714     c                   else
039900100714    5c                   endif
040000100714     c     kcae          reade     azcae03l
040100100714    4c                   enddo
040200100714     c
040300100714     c* Scrittura nuovo terminal
040400100714     c                   clear                   caeatb
040500100714     c                   eval      caedde=amgddec
040600100714     c                   eval      caedsc=amgdsca
040700100714     c                   eval      caetfe=s01ntfp
040800100714     c                   eval      caedtr=dataoggi
040900100715     c                   write     azcae000                             99
041000100714     c*
041100100714     c* se devo ripristinare dopo il terminal attuale lo faccio
041200100714    4c                   if        vidrip='SI'
041300100714     c* Scrittura nuovo terminal
041400100714     c                   clear                   caeatb
041500100714     c                   eval      caedde=wscap1
041600100714     c                   eval      caedsc=20391231
041700100714     c                   eval      caetfe=s01otfp
041800100714     c                   eval      caedtr=dataoggi
041900100715     c                   write     azcae000                             99
042000100714    4c                   endif
042100100714     c
042200100714    3c                   endif
042300100714     c*
042400100714     c* Terminal di ARRIVO
042500100714    3c                   if        s01ntfa>0
042600100714     c* Leggo il terminal attuale e lo faccio scadere a data decorrenza-1
042700100714     c                   eval      kepa='A'
042800100714     c                   eval      ktfp=000
042900100714     c                   eval      ktfa=s01fil
043000100714     c                   eval      ktfe=s01otfa
043100100714     c     kcae5         setll     azcae05l
043200100714     c     kcae5         reade     azcae05l
043300100714    4c                   dow       not %eof(azcae05l)
043400100714    5c                   if        caedde<=dataoggi and caedsc>=dataoggi
043500100714     c                             and caeatb=' '
043600100714     c                   eval      caedsc=wdecM1
043700100714     c                   clear                   caeftr
043800100714     c                   update    azcae005
043900100714     c                   leave
044000100714     c                   else
044100100714    5c                   endif
044200100714     c     kcae5         reade     azcae05l
044300100714    4c                   enddo
044400100714     c
044500100714     c* Scrittura nuovo terminal
044600100714     c                   clear                   caeatb
044700100714     c                   eval      caedde=amgddec
044800100714     c                   eval      caedsc=amgdsca
044900100714     c                   eval      caetfe=s01ntfa
045000100714     c                   eval      caedtr=dataoggi
045100100715     c                   write     azcae005                             99
045200100714     c*
045300100714     c* se devo ripristinare dopo il terminal attuale lo faccio
045400100714    4c                   if        vidrip='SI'
045500100714     c* Scrittura nuovo terminal
045600100714     c                   clear                   caeatb
045700100714     c                   eval      caedde=wscap1
045800100714     c                   eval      caedsc=20391231
045900100714     c                   eval      caetfe=s01otfa
046000100714     c                   eval      caedtr=dataoggi
046100100715     c                   write     azcae005                             99
046200100714    4c                   endif
046300100714     c
046400100714    3c                   endif
046500100714    2c                   endif
046600100714     c
046700100714     c                   add       1             nrr1
046800100714    2c                   enddo
046900100714     C                   ENDSR
047000020805      **************************************************************************
047100100714     c     *inzsr        begsr
047200100714     c
047300100714     c     kcae          klist
047400100714     c                   kfld                    kepa
047500100714     c                   kfld                    ktfp
047600100714     c                   kfld                    ktfe
047700100714     c     kcae5         klist
047800100714     c                   kfld                    kepa
047900100714     c                   kfld                    ktfe
048000100714     c                   kfld                    ktfa
048100100714     c                   kfld                    ktfp
048200100714     C                   ENDSR
048300100714      **************************************************************************
048400960302** SCHIERA MSG - MESSAGGI DI ERRORE
048500100713Data errata                                                                   01
048600100713Data DECORRENZA deve essere MAGGIORE della data di oggi                       02
048700100713La data SCADENZA deve essere MAGGIORE  della data decorrenza                  03
048800100713Filiale errata                                                                04
048900100713La filiale indicata NON E' un terminal di Partenza                            05
049000100713La filiale indicata NON E' un terminal di Arrivo                              06
049100100713Indicare almeno uno dei due nuovi terminal che cambia                         07
049200100714Il nuovo terminal di Partenza non può essere uguale all'attuale               08
049300100714Il nuovo terminal di Arrrivo non può essere uguale all'attuale                09
