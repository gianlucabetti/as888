000100000000     H*------------------------------------------------------------------------*
000200140404     H* Statistica controllo orari srervizi  - Crea Work file
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500140508     Ffidst01l  IF   E           K DISK    extfile(wfdst) usropn
000600140409     Ffiar531c  IF   E           K DISK    extfile(fiar5) usropn
000700000000     FAZORG01L  IF   E           K DISK
000800140408     FAZORG02L  IF   E           K DISK    rename(azorg:azorg002)
000900000000     FTABEL00F  IF   E           K DISK
001000140424     Fazcpc01l  IF   E           K DISK
001100140424     Fazcpl01l  IF   E           K DISK
001200140404     FWFCCOS1l  UF A E           K DISK
001300140408     FTISTO4P   O    E             PRINTER OFLIND(*IN06)
001400140422     FTISTO4P1  O    E             PRINTER OFLIND(*IN07)
001500000000     D*------------------------------------------------------------------------*
001600000000     D* SCHIERE
001700000000     D*------------------------------------------------------------------------*
001800000000     D*-------------------
001900000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
002000000000     D*-------------------
002100000000     D FIL             S              3  0 DIM(500)                             *FILIALE
002200000000     D ARE             S              3  0 DIM(500)                             *AREA
002300000000     D DIV             S              1    DIM(500)                             *DIVISIONE
002400051109     D dfil            S             20    DIM(500)                             *FILIALE
002500140408     D dare            S             29    DIM(500)                             *AREA
002600140408     D** ddiv            S             25    DIM(500)                             *DIVISIONE
002700000000     D*-------------------
002800000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
002900051109     D CT              S              2    DIM(50)                              TIP0 BOLLA
003000140404     d
003100140404     D NTW             S              3    DIM(500)                             *NETWORK
003200140408     d
003300140408     D L6              S              3  0 DIM(30)
003400140422     d
003500140422     D or_dal          S              4  0 DIM(11)
003600000000     D*------------------------------------------------------------------------*
003700000000     D KPJBA         E DS
003800140408     d Azuteds       e ds                  Extname(Azute00f)
003900140408     d dDatiute      e ds
004000140408     d Tibs34ds      e ds                  Inz
004100140409     D azorgds       E DS                  EXTNAME(azorg00f)
004200140515     D titasds       E DS                  EXTNAME(titas00f)
004300000000     D*-------------------
004400000000     D DSTB          E DS
004500051109     D DS05          E DS
004600051109     D DS17          E DS
004700051109     D DS1b          E DS
004800040220     D*------------------
004900140404     D tisi95ds      E DS
005000140404     D dar5GEN       E DS
005100140404     D og143         E DS
005200140404     d
005300140404     D TRULORSDS     E DS
005400140404     D TRULOR2DS     E DS
005500140514     D TRULOR3DS     E DS
005600140408     d
005700140408     D* DS PER TRUL06R - CARICAMENTO £X
005800140408     D trul06ds      E DS
005900140408     D  LIN                    1     90  0
006000140408     D                                     DIM(30)
006100140411     D fnlv55ds      E DS
006200000000     D*-------------------
006300000000     D* DS PARAMETRI DI LANCIO PGM
006400000000     D*-------------------
006500000000     D PARAM           DS
006600140404     D  pardsd                 1      8  0
006700140404     D  pardsa                 9     16  0
006800140404     D  partser               17     17
006900140404     D  parfil                19     21  0
007000140411     D  parcar                22     24  0
007100140404     D  parperc               25     27  0
007200140408     D  parnum                28     30  0
007300140408     D  parorari              31     31
007400140408     D  parcor                32     32
007500140411     D  pardet                33     33
007600140415     d* dettaglio orari
007700140415     D  paror1                34     37  0
007800140415     D  paror2                38     41  0
007900140415     D  paror3                42     45  0
008000140415     D  paror4                46     49  0
008100140415     D  paror5                50     53  0
008200140415     D  paror6                54     57  0
008300140415     D  paror7                58     61  0
008400140415     D  paror8                62     65  0
008500140416     D  paror9                66     69  0
008600140415     D  paroltre              70     73  0
008700140408     d
008800140408     D WLBDAT          DS
008900140408     D  G02DAT                 1      8  0
009000140408     D  G02INV                 9     16  0
009100140408     D  G02ERR                17     17
009200140408     D  G02TGI                18     22  0
009300140404     d
009400060524     d
009500140404     d KAr5Trd         s                   Like(Ar5Trd) inz('GEN')
009600140404     D wNetwork        s              3    inz(*blanks)
009700140404     D kfil            s              3  0 inz
009800140404     D P1DTSER         s              8    inz
009900140404     D wperc_os        s              3  0 inz
010000140404     D wperc_om        s              3  0 inz
010100140404     D Y               s              3  0 inz
010200140408     D YY              s              3  0 inz
010300140423     D XX              s              3  0 inz
010400140408     D savlna          s              3  0 inz
010500140404     D kdcm            s              8  0 inz
010600140404     D wnorm           s              1    inz
010700140408     D wstampadet      s              1    inz
010800140404     D knfv            s                   like(dstnfv)
010900140404     D kloc            s                   like(o95loc)
011000140408     D wfuori_os       s                   like(wcostsOS)
011100140408     D wfuori_om       s                   like(wcostsOm)
011200140415     D kserv           s                   like(wcosserv)
011300140422     d Timeiso         s               t   timfmt(*iso)
011400140422     d w0060           s              6  0
011500140422     d wperc           s              3  0
011600140505     d wcosts          s              9  0
011700140505     d tot_ts          s              9  0
011800140423     d tot_tse         s              9  0
011900140423     d tot_tsos        s              9  0
012000140423     d tot_tsom        s              9  0
012100140404     d
012200140508     d WFDST           s             21    inz('FILTRA201 /FIDST01L')
012300140409     d fiar5           s             21    inz('GAITRAGRU /FIAR531C')
012400000000     C*------------------------------------------------------------------------*
012500000000     C* MAIN LINE
012600000000     C*------------------------------------------------------------------------*
012700140428     C     *ENTRY        PLIST
012800140428     C                   PARM                    KPJBA
012900140428     C                   MOVEL     KPJBU         PARAM
013000140409      /free
013100140409         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013200140409      /end-free
013300140428     C* CARICA TIPI BOLLA
013400140428     C                   EXSR      CARTBL
013500140408     c*
013600140408     c* se impostata l'area leggo le filiali dell'are altrimenti la singola filiale
013700140408     c*  altrimenti tutto
013800140408     c                   select
013900140408     c                   when      parfil>0
014000140408     c* carico la sua £6
014100140408     C                   CLEAR                   trul06ds
014200140408     C                   MOVE      '£6'          D06COD
014300140408     C                   movel     parfil        D06KEY
014400140408     c                   move      'S'           d06key
014500140409     c                   if        pardsa>0
014600140408     c                   movel     pardsa        d06drf
014700140409     c                   else
014800140409     c                   movel     pardsd        d06drf
014900140409     c                   endif
015000140408     C                   MOVEL     'L'           D06TLA
015100140408     C                   MOVEL     trul06ds      KPJBU
015200140408     C*
015300140408     C                   CALL      'TRUL06R'
015400140408     C                   PARM                    KPJBA
015500140408     C                   MOVEL     KPJBU         trul06ds
015600140408     C                   MOVEA     LIN           L6
015700140423     c                   z-add     1             XX
015800140408
015900140423     c                   dow       l6(XX)>0
016000140423     c                   eval      kfil=l6(XX)
016100140515     c                   exsr      Let_TAS
016200140423     c                   add       1             XX
016300140408     c                   enddo
016400140408     c* immesso cod area
016500140408     c                   when      parcar>0
016600140409     C/EXEC SQL
016700140409     C+ Declare A1 Cursor for
016800140409     c+ SELECT * FROM azorg00f
016900140409     c+ WHERE orgcar=  :parcar AND orgfva=' ' and (orgfag='F' or orgfag='A')
017000140409     c+ ORDER BY orgfil
017100140409     C/END-EXEC
017200140409      *          apertura cursore
017300140409     C/EXEC SQL
017400140409     C+ OPEN A1
017500140409     C/END-EXEC
017600140409     C                   DOU       SqlCod <> 0
017700140409     C/EXEC SQL
017800140409     C+ Fetch Next From A1 Into :azorgds
017900140409     C/END-EXEC
018000140409     c                   select
018100140409     C                   WHEN      SqlCod = 100
018200140409     c                   leave
018300140409      *
018400140409     C                   WHEN      SqlCod < 0
018500140409     c                   seton                                        H1
018600140409     c                   leave
018700140409      *
018800140409     C                   OTHER
018900140408     c                   eval      kfil=orgfil
019000140515     c                   exsr      Let_TAS
019100140409     C                   ENDSL
019200140409
019300140408     c                   enddo
019400140409     C/EXEC SQL
019500140409     C+ Close A1
019600140409     C/END-EXEC
019700140408     c
019800140408     c                   other
019900140409
020000140408     c     *loval        setll     azorg02l
020100140408     c                   read      azorg02l
020200140408     c                   dow       not %eof(azorg02l)
020300140408     c                   eval      kfil=orgfil
020400140515     c                   exsr      Let_TAS
020500140408     c                   read      azorg02l
020600140408     c                   enddo
020700140408     c
020800140408     c                   endsl
020900140404     c*
021000140404     c                   EXSR      Stampa
021100140408     c*
021200140408     c                   write     STO4FI
021300140422     c
021400140422     c                   if        pardet='S'
021500140422     c                   write     STO4RI_d
021600140422     c                   write     STO4FI_d
021700140422     c                   endif
021800000000     C*
021900140423     C                   CLEAR                   fnlv55ds
022000140423     C                   MOVEL     'C'           D55TLA
022100140423     C                   CALL      'FNLV55R'
022200140423     C                   PARM                    fnlv55ds
022300140423     c
022400000000     C                   SETON                                        LR
022500140408     C*------------------------------------------------------------------------*
022600140408     C* Ciclo di lettura su TITAS
022700140408     C*------------------------------------------------------------------------*
022800140408     c     LET_TAS       BEGSR
022900140515     C*
023000140423     c* carico fil gestore
023100140423     c                   clear                   fnlv55ds
023200140423     C                   EVAL      D55TPT='6'
023300140423     c                   eval      d55lin=kfil
023400140423     c                   if        pardsa>0
023500140423     c                   eval      d55drf=pardsa
023600140423     c                   else
023700140423     c                   eval      d55drf=pardsd
023800140423     c                   endif
023900140423     c
024000140423     c                   call      'FNLV55R'
024100140423     c                   parm                    fnlv55ds
024200140515     C*
024300140515     C/EXEC SQL
024400140515     C+ declare C1 cursor for
024500140515     C+ with
024600140515     C+ selTAS00 as (
024700140515     C+ select * from gaitragru/titas00f where
024800140515     C+ tasDCM >= :PARDSD and tasDCM <= :PARDSA and
024900140515     C+ tasLNA = :KFIL and tasNZD = ' ' and tasCCA = ' ' and
025000140515     C+ tasFFD = ' '),
025100140515     C+ selTAS10 as (
025200140515     C+ select * from gaitragru/titas10f where
025300140515     C+ tasDCM >= :PARDSD and tasDCM <= :PARDSA and
025400140515     C+ tasLNA = :KFIL and tasNZD = ' ' and tasCCA = ' ' and
025500140515     C+ tasFFD = ' ')
025600140515     C+ select * from seltas00
025700140515     C+ union all
025800140515     C+ select * from seltas10
025900140515     C+ for fetch only
026000140515     C/END-EXEC
026100140515     C
026200140515     C/EXEC SQL
026300140515     C+ open C1
026400140515     C/END-EXEC
026500140515     C
026600140515     C/EXEC SQL
026700140515     C+ Fetch C1 into :titasds
026800140515     C/END-EXEC
026900140515     c*
027000140515     C                   dow       sqlcod = *zeros
027100140515     C*
027200140515     C                   MOVEL     'N'           WRECOK            1
027300140515     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
027400140515     C                   IF        WRECOK <> 'N'
027500140515     C*
027600140515     C* CREA IL RECORD SUI FILE DI LAVORO
027700140515     C                   EXSR      CRErec
027800140515     C*
027900140515     C                   ENDIF
028000140515     C*
028100140515     C/EXEC SQL
028200140515     C+ Fetch C1 into :titasds
028300140515     C/END-EXEC
028400140515     C                   enddo                                                  *rottura linea
028500140515     C*
028600140515     C/EXEC SQL
028700140515     C+ close C1
028800140515     C/END-EXEC
028900140515     C*
029000140408     c                   ENDSR
029100000000     C*------------------------------------------------------------------------*
029200000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
029300000000     C*------------------------------------------------------------------------*
029400000000     C     CHKREC        BEGSR
029500000000     C*
029600000000     C                   MOVEL     'S'           WRECOK            1
029700040305     C*
029800140414     c*
029900051109     C* ESCLUDE LE BOLLE di recupero
030000051109     C                   Z-ADD     1             i
030100000000     C                   SETOFF                                           28
030200051109     C     TASTBL        LOOKUP    TB(i)                                  28
030300051109     C                   IF        *in28 = *on
030400000000     C                   MOVEL     'N'           WRECOK
030500000000     C                   ENDIF
030600011112     C*
030700000000     C                   ENDSR
030800140408     C*------------------------------------------------------------------------*
030900140408     C* controllo se si tratta di bolla da elaborare
031000140408     C*------------------------------------------------------------------------*
031100140408     C     NOELAB        BEGSR
031200140408     C*
031300140408     C                   MOVEL     'S'           Welab             1
031400140408     C*
031500140408     c* Cerco se si tratta di distinta PDA
031600140508     c* solo in AS888
031700140508    1c                   if        %subst(knsif:7:1)='P'
031800140508     c                   movel     taspdc        kfgs              3 0
031900140508     c                   movel     4             knpg              1 0
032000140508     c                   z-add     tasndc        knfv
032100140508     c     kdst          chain     fidst01l
032200140508    2c                   if        %found(fidst01l)
032300140508     c                             and (dstpda='C' or dstpda='E')
032400140508   x2c                   else
032500140508     C                   MOVEL     'N'           Welab
032600140508    2c                   endif
032700140508     c
032800140508   x1c                   else
032900140508   c
033000140508     c* Solo dall'aprile 2014 abbiamo le bolle con il flag impostato
033100140508     c                   if        tasaas<2014
033200140508     C                   MOVEL     'N'           Welab
033300140508     c                   endif
033400140508     c
033500140508     c                   if        tasaas=2014 and tasmgs<0401
033600140508     C                   MOVEL     'N'           Welab
033700140508     c                   endif
033800140408     c* chaino record GEN
033900140408     c     kar5          chain     fiar531c
034000140408     c                   if        %found(fiar531c)
034100140408     c                   eval      dar5gen=ar5uni
034200140408     c                   if        §AR5PDACO<>'  '
034300140408     C                   MOVEL     'N'           Welab
034400140408     c                   endif
034500140408     c                   endif
034600140508     c
034700140508    1C                   ENDIF
034800140408     c
034900140411     c* Non elaboro se impostata ora consegna richiesta
035000140411     c                   if        tashcr>0
035100140408     C                   MOVEL     'N'           Welab
035200140408     c                   endif
035300140408     c
035400140408     C                   ENDSR
035500000000     C*------------------------------------------------------------------------*
035600140408     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO
035700000000     C*------------------------------------------------------------------------*
035800140408     C     CRErec        BEGSR
035900140404     c                   clear                   kloc
036000140404     c
036100140404     c
036200140404     c* Richiamo pgm reperimento orari servizi
036300140404     c                   clear                   trulorsds
036400140404     c                   clear                   trulor2ds
036500140514     c                   clear                   trulor3ds
036600140404     c                   eval      ioredta=pardsd
036700140408     c                   eval      iorefil=taslna
036800140404     c                   eval      iorenar=tasnzd
036900140404     c                   eval      iorecap=tascad
037000140514     c                   eval      ioreloc=taslod
037100140414     c                   eval      ioreddc=tasddc
037200140414     c                   eval      iorendc=tasndc
037300140414     c                   eval      ioretfp=tastfp
037400140414     c                   eval      ioretfa=tastfa
037500140414     c                   eval      ioredti=tasdti
037600140428     c                   eval      iorehti=9999
037700140404     c                   eval      ioretser=partser
037800140515     c* devo avere sempre gli orari completi anche per H e E
037900140515     c                   eval      ioretsp ='C'
038000140404     c                   call      'TRULORSR'
038100140404     c                   parm                    kpjba
038200140404     c                   parm                    trulorsds
038300140404     c                   parm                    trulor2ds
038400140514     c                   parm                    trulor3ds
038500140514     c                   eval      wnorm=OOR3NORM
038600140514     c
038700140514     c                   if        oor3norm='S'
038800140514     c                   eval      kloc =OOR3LOC_N
038900140514     c                   endif
039000140404     c*
039100140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
039200140415     c                   if        oor2fcsgl='P'
039300140415     c                   eval      kserv='P '
039400140415     c                   else
039500140415     c                   clear                   kserv
039600140415     c                   endif
039700140415
039800140415     c     kccos         chain     wfccos1l
039900140404     c                   if        not %found(wfccos1l)
040000140404     c                   clear                   wfccos00
040100140404     c                   eval      wcosdcmd=pardsd
040200140404     c                   eval      wcosdcma=pardsa
040300140404     c                   eval      wcosver =p96ver
040400140411     c                   eval      wcoslna =d55tfa
040500140404     c                   eval      wcosnaz =tasnzd
040600140404     c                   eval      wcoscap =tascad
040700140404     c                   eval      wcosloc =kloc
040800140404     c                   eval      wcostser=partser
040900140404     c                   eval      wcosorif=OOR2ORIF
041000140415     c                   eval      wcosserv=kserv
041100140424     c
041200140424     c* obsoleto
041300140424     c     kcpc          chain     azcpc01l
041400140424     c                   if        %found(azcpc01l)
041500140424     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
041600140424     c                   endif
041700140424     c                   if        wcosloc<>*blanks
041800140424     c     kcpl          chain     azcpl01l
041900140424     c                   if        %found(azcpl01l)
042000140424     c                   eval      wcosloco=%subst(cplfi1:1:1)
042100140424     c                   endif
042200140424     c                   endif
042300140424     c* Imposto gli orari nel file
042400140424     c                   eval      WCOSOSTIS=OOR2STIS
042500140424     c                   eval      WCOSOSTfS=OOR2STfS
042600140424     c                   eval      WCOSOmiis=OOR2miis
042700140424     c                   eval      WCOSOmxfs=OOR2mxfs
042800140424     c
042900140415     c* imposto orari dettaglio se ci sono e richiestoc
043000140415     c                   if        pardet='S'
043100140422     c                   eval      wcosor1=or_dal(1)
043200140422     c                   eval      wcosor2=or_dal(2)
043300140422     c                   eval      wcosor3=or_dal(3)
043400140422     c                   eval      wcosor4=or_dal(4)
043500140422     c                   eval      wcosor5=or_dal(5)
043600140422     c                   eval      wcosor6=or_dal(6)
043700140422     c                   eval      wcosor7=or_dal(7)
043800140422     c                   eval      wcosor8=or_dal(8)
043900140422     c                   eval      wcosor9=or_dal(9)
044000140422     c                   eval      wcosorOl=paroltre
044100140404     c                   endif
044200140415     c                   endif
044300140404     c
044400140404     c                   add       1             wcosts
044500140404     c* Se loc non normalizzata
044600140514     c                   if        wnorm<>'S'
044700140404     c                   add       1             wcostsNN
044800140404     c                   endif
044900140408     c
045000140408     c* Controllo se spedizione da elaborare
045100140408     c                   exsr      Noelab
045200140408     c
045300140404     c* elaborate
045400140408     c                   if        welab='S'
045500140404     c                   add       1             wcostse
045600140404     c
045700140404     c* consegna negli orari std
045800140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
045900140404     c                   add       1             wcostsos
046000140404     c                   endif
046100140404     c* consegna negli orari min/max
046200140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
046300140404     c                   add       1             wcostsom
046400140404     c                   endif
046500140415     c* imposto campi dettaglio
046600140415     c                   exsr      ImpoDET
046700140408     c                   endif
046800140404     c
046900140404     c                   if        not %found(wfccos1l)
047000140404     c                   write     wfccos00
047100140404     c                   else
047200140404     c                   update    wfccos00
047300140404     c                   endif
047400000000     C*
047500000000     C                   ENDSR
047600140404     C*------------------------------------------------------------------------*
047700140404     C* Stampa - statistica
047800140404     C*------------------------------------------------------------------------*
047900140404     c     STAMPA        BEGSR
048000140404     c* Tpo servizio
048100140404     c                   if        partser='C'
048200140404     c                   eval      p1dtser='CONSEGNE'
048300140404     C                   ELSE
048400140408     c                   eval      p1dtser=' RITIRI '
048500140404     C                   ENDIF
048600140408     c* giro le date
048700140409     C                   MOVE      pardsd        G02inv
048800140408     C                   MOVEL     '3'           G02ERR
048900140408     C                   CALL      'XSRDA8'
049000140408     C                   PARM                    WLBDAT
049100140408     c                   z-add     g02dat        p1ddsd
049200140408     c*
049300140408     c                   if        pardsa>0
049400140409     C                   MOVE      pardsa        G02inv
049500140408     C                   MOVEL     '3'           G02ERR
049600140408     C                   CALL      'XSRDA8'
049700140408     C                   PARM                    WLBDAT
049800140408     c                   z-add     g02dat        p1ddsa
049900140408     c                   endif
050000140408     c* Orari per calcolo %
050100140408     c                   if        parorari='S'
050200140408     c                   eval      p1dorari='STANDARD'
050300140424     c                   eval      p1dtor_d='Standard'
050400140408     c                   else
050500140408     c                   eval      p1dorari='MINIMO/MASSIMO'
050600140424     c                   eval      p1dtor_d=' Min/Max'
050700140408     c                   endif
050800140408     c
050900140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
051000140408     c                   if        parcor='C'
051100140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
051200140411     c                   eval      p1dbolstd='   Nei limiti STD   '
051300140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
051400140408     C                   ELSE
051500140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
051600140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
051700140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
051800140408     C                   ENDIF
051900140423     c
052000140423     c                   write     STO4TE
052100140423
052200140423     c* Orari nel dettaglio
052300140423     c                   if        pardet='S'
052400140423     c                   exsr      TestaDET
052500140423     c                   write     STO4TE_d
052600140423     c                   endif
052700140404     c
052800140404     C
052900140424     c*
053000140404     c     *loval        setll     wfccos1l
053100140404     c                   read      wfccos1l
053200140404     c                   dow       not %eof(wfccos1l)
053300140404     c
053400140408     c* a cambio filiale stampo a pagina nuova
053500140408     c                   if        wcoslna<>savlna
053600140408
053700140408     c                   if        savlna>0
053800140423     c                   exsr      Totfil
053900140423     c
054000140408     c                   write     STO4TE
054100140422     c
054200140422     c                   if        pardet='S'
054300140422     c                   write     STO4RI_d
054400140422     c                   write     STO4TE_d
054500140408     c                   endif
054600140423     c
054700140423     c                   clear                   tot_ts
054800140423     c                   clear                   tot_tse
054900140423     c                   clear                   tot_tsos
055000140423     c                   clear                   tot_tsom
055100140422     c                   endif
055200140408
055300140408     c                   z-add     1             i
055400140408     C     wcoslna       lookup    fil(i)                                 55
055500140404     C                   if        %found
055600140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
055700140408     c                   eval      p1dare=dare(i)
055800140404     C                   else
055900140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
056000140408     c                   eval      p1dare=*all'?'
056100140404     C                   endif
056200140408
056300140408     c                   eval      savlna=wcoslna
056400140408
056500140408     c                   write     STO4TEF
056600140414     c                   write     STO4RI
056700140422     c                   if        pardet='S'
056800140422     c                   write     STO4TEF_d
056900140422     c                   write     STO4RI_d
057000140408     c                   endif
057100140422     c                   endif
057200140408
057300140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
057400140411     c                   if        wcostse>=parnum and wcostse>0
057500140408     c* voglio la % di correttezza
057600140408     c                   if        parcor='C'
057700140404     c* Calcolo % STD
057800140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
057900140408     c                   eval      p1dstsos=WCOSTSOs
058000140404     c* Calcolo % MIN/MAX
058100140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
058200140408     c                   eval      p1dstsom=WCOSTSOm
058300140408     c
058400140408     c                   else
058500140408     c* voglio la % delle fuori orario
058600140408     c                   eval      wfuori_os=wcostse-wcostsos
058700140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
058800140408     c                   eval      p1dstsos=Wfuori_os
058900140408
059000140408     c                   eval      wfuori_om=wcostse-wcostsom
059100140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
059200140408     c                   eval      p1dstsom=Wfuori_om
059300140408     c                   endif
059400140408     c
059500140407     c* stampo solo al di sotto della % richiesta
059600140408     c                   eval      wstampadet='S'
059700140408
059800140408     c                   select
059900140408     c                   when      parperc=0
060000140408
060100140408     c                   when      parorari='S'  and parcor='C' and
060200140408     c                             p1dperc_os>=parperc
060300140408     c                   eval      wstampadet='N'
060400140408     c
060500140408     c                   when      parorari='S'  and parcor='F' and
060600140408     c                             p1dperc_os< parperc
060700140408     c                   eval      wstampadet='N'
060800140408
060900140408     c                   when      parorari='M'  and parcor='C' and
061000140408     c                             p1dperc_om>=parperc
061100140408     c                   eval      wstampadet='N'
061200140408     c
061300140408     c                   when      parorari='M'  and parcor='F' and
061400140408     c                             p1dperc_om< parperc
061500140408     c                   eval      wstampadet='N'
061600140408     c                   endsl
061700140404     c*
061800140408     c                   if        wstampadet='S'
061900140424     c* POM
062000140424     c                   if        wcosserv='P '
062100140424     c                   eval      p1dpom='Pom'
062200140424     c                   else
062300140424     c                   eval      p1dpom='   '
062400140424     c                   endif
062500140411     c*
062600140411     c* Se la località è vuota imposto '??'
062700140411     c                   if        wcosloc=*blanks
062800140411     c                   eval      wcosloc='??'
062900140411     c                   endif
063000140408     c
063100140408     c                   if        *in06
063200140408     c                   write     sto4te
063300140408     c                   write     STO4TEF
063400140414     c                   write     STO4RI
063500140408     c                   setoff                                       06
063600140404     c                   endif
063700140408     c
063800140408     c* Richiamo pgm reperimento orari servizi
063900140424     c***                clear                   trulorsds
064000140424     c***                clear                   trulor2ds
064100140424     c***                eval      ioredta=pardsd
064200140424     c***                eval      iorefil=wcoslna
064300140424     c***                eval      iorenar=wcosnaz
064400140424     c***                eval      iorecap=wcoscap
064500140424     c***                eval      ioreloc=wcosloc
064600140424     c***                if        wcosloc=*blanks
064700140424     c***                eval      ioreloc='A '
064800140424     c***                endif
064900140424     c***                eval      ioretser=wcostser
065000140424     c***                eval      ioretsp ='C'
065100140424     c***                call      'TRULORSR'
065200140424     c***                parm                    kpjba
065300140424     c***                parm                    trulorsds
065400140424     c**+                parm                    trulor2ds
065500140408     c
065600140424     c                   eval      p1dstis=wcosoSTiS
065700140424     c                   eval      p1dstfs=wcosoSTFS
065800140424     c                   eval      p1dmiis=wcosomiis
065900140424     c                   eval      p1dmxfs=wcosomxfs
066000140404     c
066100140408     c                   write     STO4det
066200140422     c
066300140422     c* se devo stampare anche il dettaglio
066400140422     c                   if        pardet='S'
066500140422     c                   exsr      Stadet
066600140422     c                   endif
066700140408     c
066800140407     c                   endif
066900140408     c                   endif
067000140424     c* totali di filiale
067100140424     c                   add       WCOSTS        tot_ts
067200140424     c                   add       WCOSTSE       tot_tse
067300140424     c                   add       WCOSTSos      tot_tsos
067400140424     c                   add       WCOSTSom      tot_tsom
067500140407     c
067600140404     c                   read      wfccos1l
067700140404     c
067800140404     c                   enddo
067900140423     c
068000140423     c* totali finali
068100140423     c                   if        savlna>0
068200140423     c                   exsr      totfil
068300140423     c                   endif
068400140404     c
068500140404     c                   ENDSR
068600140423     C*------------------------------------------------------------------------*
068700140423     c* Imposta gli orari in testata dettaglioù
068800140423     C*------------------------------------------------------------------------*
068900140423     c     TestaDet      BEGSR
069000140423     c                   eval      p1dsot = paror1
069100140423     c                   eval      p1doltre= paroltre
069200140423     c                   eval      p1dda1  = or_dal(1)
069300140423
069400140423     c                   if        or_dal(2)>0
069500140423     c                   eval      p1dda2  = or_dal(2)
069600140423     c                   movel     or_dal(2)     w0060
069700140423     c                   move      w0060         timeiso
069800140423     c                   subdur    1:*mn         timeiso
069900140423     c                   eval      w0060     =%dec(timeiso)
070000140423     c                   movel     w0060         p1dal1
070100140423     c                   else
070200140423     c                   eval      p1dal1=paroltre
070300140423     c                   leavesr
070400140423     c                   endif
070500140423     c
070600140423     c                   if        or_dal(3)>0
070700140423     c                   eval      p1dda3  = or_dal(3)
070800140423     c                   movel     or_dal(3)     w0060
070900140423     c                   move      w0060         timeiso
071000140423     c                   subdur    1:*mn         timeiso
071100140423     c                   eval      w0060     =%dec(timeiso)
071200140423     c                   movel     w0060         p1dal2
071300140423     c                   else
071400140423     c                   eval      p1dal2=paroltre
071500140423     c                   leavesr
071600140423     c                   endif
071700140423
071800140423     c                   if        or_dal(4)>0
071900140423     c                   eval      p1dda4  = or_dal(4)
072000140423     c                   movel     or_dal(4)     w0060
072100140423     c                   move      w0060         timeiso
072200140423     c                   subdur    1:*mn         timeiso
072300140423     c                   eval      w0060     =%dec(timeiso)
072400140423     c                   movel     w0060         p1dal3
072500140423     c                   else
072600140423     c                   eval      p1dal3=paroltre
072700140423     c                   leavesr
072800140423     c                   endif
072900140423
073000140423     c                   if        or_dal(5)>0
073100140423     c                   eval      p1dda5  = or_dal(5)
073200140423     c                   movel     or_dal(5)     w0060
073300140423     c                   move      w0060         timeiso
073400140423     c                   subdur    1:*mn         timeiso
073500140423     c                   eval      w0060     =%dec(timeiso)
073600140423     c                   movel     w0060         p1dal4
073700140423     c                   else
073800140423     c                   eval      p1dal4=paroltre
073900140423     c                   leavesr
074000140423     c                   endif
074100140423
074200140423     c                   if        or_dal(6)>0
074300140423     c                   eval      p1dda6  = or_dal(6)
074400140423     c                   movel     or_dal(6)     w0060
074500140423     c                   move      w0060         timeiso
074600140423     c                   subdur    1:*mn         timeiso
074700140423     c                   eval      w0060     =%dec(timeiso)
074800140423     c                   movel     w0060         p1dal5
074900140423     c                   else
075000140423     c                   eval      p1dal5=paroltre
075100140423     c                   leavesr
075200140423     c                   endif
075300140423
075400140423     c                   if        or_dal(7)>0
075500140423     c                   eval      p1dda7  = or_dal(7)
075600140423     c                   movel     or_dal(7)     w0060
075700140423     c                   move      w0060         timeiso
075800140423     c                   subdur    1:*mn         timeiso
075900140423     c                   eval      w0060     =%dec(timeiso)
076000140423     c                   movel     w0060         p1dal6
076100140423     c                   else
076200140423     c                   eval      p1dal6=paroltre
076300140423     c                   leavesr
076400140423     c                   endif
076500140423
076600140423     c                   if        or_dal(8)>0
076700140423     c                   eval      p1dda8  = or_dal(8)
076800140423     c                   movel     or_dal(8)     w0060
076900140423     c                   move      w0060         timeiso
077000140423     c                   subdur    1:*mn         timeiso
077100140423     c                   eval      w0060     =%dec(timeiso)
077200140423     c                   movel     w0060         p1dal7
077300140423     c                   else
077400140423     c                   eval      p1dal7=paroltre
077500140423     c                   leavesr
077600140423     c                   endif
077700140423     c                   if        or_dal(9)>0
077800140423     c                   eval      p1dda9  = or_dal(9)
077900140423     c                   movel     or_dal(9)     w0060
078000140423     c                   move      w0060         timeiso
078100140423     c                   subdur    1:*mn         timeiso
078200140423     c                   eval      w0060     =%dec(timeiso)
078300140423     c                   movel     w0060         p1dal8
078400140423     c
078500140423     c                   eval      p1dal9=paroltre
078600140423     c                   else
078700140423     c                   eval      p1dal8=paroltre
078800140423     c                   leavesr
078900140423     c                   endif
079000140423     c
079100140423     c                   ENDSR
079200140422     C*------------------------------------------------------------------------*
079300140422     C* Stampa dettaglio
079400140422     C*------------------------------------------------------------------------*
079500140422     C     StaDET        BEGSR
079600140422     c                   if        *in07
079700140422     c                   write     sto4te_d
079800140422     c                   write     STO4TEF_d
079900140422     c                   write     STO4RI_d
080000140422     c                   setoff                                       07
080100140422     c                   endif
080200140424     c* Imposto gli orari
080300140424     c                   if        parorari='S'
080400140424     c                   eval      p1dor_dal=WCOSOSTIS
080500140424     c                   eval      p1dor_al =WCOSOSTfS
080600140424     c                   else
080700140424     c                   eval      p1dor_dal=WCOSOmiis
080800140424     c                   eval      p1dor_al =WCOSOmxfS
080900140424     c                   endif
081000140424     c                   setoff                                       111213
081100140424     c                   setoff                                       141516
081200140424     c                   setoff                                       171819
081300140505     c                   setoff                                       2021
081400140422     c* calcolo le %
081500140422     c* SOTTO
081600140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
081700140422     C                   IF        WPERC>0
081800140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
081900140505     c                   seton                                        20
082000140422     c                   else
082100140424     c                   eval      p1cp_sot='    '
082200140422     c                   endif
082300140422     c* OLTRE
082400140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
082500140422     C                   IF        WPERC>0
082600140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
082700140505     c                   seton                                        21
082800140422     c                   else
082900140424     c                   eval      p1cp_olt='    '
083000140422     c                   endif
083100140422     c* % 1
083200140505    1c                   if        wcosor1  >0
083300140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
083400140422     c
083500140505    2C                   IF        WPERC>0
083600140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
083700140505     c
083800140505    2c                   select
083900140505     c                   when      or_dal(1)<p1dor_dal
084000140505     c                   seton                                        11
084100140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
084200140505     c                   seton                                        11
084300140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
084400140505     c                   seton                                        11
084500140505    2c                   endsl
084600140505
084700140422     c                   else
084800140424     c                   eval      p1cp_o1 ='    '
084900140505    2c                   endif
085000140424     c
085100140505    1c                   endif
085200140422     c
085300140422     c* % 2
085400140422     c                   if        wcosor2  >0
085500140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
085600140422     c
085700140422     C                   IF        WPERC>0
085800140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
085900140505
086000140505     c                   select
086100140505     c                   when      or_dal(2)<p1dor_dal
086200140505     c                   seton                                        12
086300140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
086400140505     c                   seton                                        12
086500140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
086600140505     c                   seton                                        12
086700140505     c                   endsl
086800140505
086900140422     c                   else
087000140424     c                   eval      p1cp_o2 ='    '
087100140422     c                   endif
087200140422     c                   endif
087300140423     c* % 3
087400140423     c                   if        wcosor3  >0
087500140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
087600140423     c
087700140423     C                   IF        WPERC>0
087800140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
087900140505
088000140505     c                   select
088100140505     c                   when      or_dal(3)<p1dor_dal
088200140505     c                   seton                                        13
088300140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
088400140505     c                   seton                                        13
088500140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
088600140505     c                   seton                                        13
088700140505     c                   endsl
088800140423     c                   else
088900140424     c                   eval      p1cp_o3 ='    '
089000140423     c                   endif
089100140423     c                   endif
089200140423     c* % 4
089300140423     c                   if        wcosor4  >0
089400140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
089500140423     c
089600140423     C                   IF        WPERC>0
089700140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
089800140505
089900140505     c                   select
090000140505     c                   when      or_dal(4)<p1dor_dal
090100140505     c                   seton                                        14
090200140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
090300140505     c                   seton                                        14
090400140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
090500140505     c                   seton                                        14
090600140505     c                   endsl
090700140423     c                   else
090800140424     c                   eval      p1cp_o4 ='    '
090900140423     c                   endif
091000140423     c                   endif
091100140423     c* % 5
091200140423     c                   if        wcosor5  >0
091300140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
091400140423     c
091500140423     C                   IF        WPERC>0
091600140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
091700140505
091800140505     c                   select
091900140505     c                   when      or_dal(5)<p1dor_dal
092000140505     c                   seton                                        15
092100140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
092200140505     c                   seton                                        15
092300140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
092400140505     c                   seton                                        15
092500140505     c                   endsl
092600140423     c                   else
092700140424     c                   eval      p1cp_o5 ='    '
092800140423     c                   endif
092900140423     c                   endif
093000140423     c* % 6
093100140423     c                   if        wcosor6  >0
093200140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
093300140423     c
093400140423     C                   IF        WPERC>0
093500140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
093600140505
093700140505     c                   select
093800140505     c                   when      or_dal(6)<p1dor_dal
093900140505     c                   seton                                        16
094000140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
094100140505     c                   seton                                        16
094200140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
094300140505     c                   seton                                        16
094400140505     c                   endsl
094500140423     c                   else
094600140424     c                   eval      p1cp_o6 ='    '
094700140423     c                   endif
094800140423     c                   endif
094900140423     c* % 7
095000140423     c                   if        wcosor7  >0
095100140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
095200140423     c
095300140423     C                   IF        WPERC>0
095400140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
095500140505
095600140505     c                   select
095700140505     c                   when      or_dal(7)<p1dor_dal
095800140505     c                   seton                                        17
095900140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
096000140505     c                   seton                                        17
096100140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
096200140505     c                   seton                                        17
096300140505     c                   endsl
096400140423     c                   else
096500140424     c                   eval      p1cp_o7 ='    '
096600140423     c                   endif
096700140423     c                   endif
096800140423     c* % 8
096900140423     c                   if        wcosor8  >0
097000140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
097100140423     c
097200140423     C                   IF        WPERC>0
097300140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
097400140505
097500140505     c                   select
097600140505     c                   when      or_dal(8)<p1dor_dal
097700140505     c                   seton                                        18
097800140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
097900140505     c                   seton                                        18
098000140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
098100140505     c                   seton                                        18
098200140505     c                   endsl
098300140423     c                   else
098400140424     c                   eval      p1cp_o8 ='    '
098500140423     c                   endif
098600140423     c                   endif
098700140423     c* % 9
098800140423     c                   if        wcosor9  >0
098900140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
099000140423     c
099100140423     C                   IF        WPERC>0
099200140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
099300140505
099400140505     c                   select
099500140505     c                   when      or_dal(9)<p1dor_dal
099600140505     c                   seton                                        19
099700140505     c                   when      paroltre >p1dor_al
099800140505     c                   seton                                        19
099900140505     c                   endsl
100000140423     c                   else
100100140424     c                   eval      p1cp_o9 ='    '
100200140423     c                   endif
100300140423     c                   endif
100400140422     c
100500140422     c                   write     STO4det_d
100600140422     c                   ENDSR
100700140423     C*------------------------------------------------------------------------*
100800140423     c* Stampa totali di filiale
100900140423     C*------------------------------------------------------------------------*
101000140423     c     TOTfil        BEGSR
101100140423     c* voglio la % di correttezza
101200140423     c                   if        parcor='C'
101300140423     c* Calcolo % STD
101400140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
101500140423     c                   eval      p1dstsos=tot_TSOs
101600140423     c* Calcolo % MIN/MAX
101700140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
101800140423     c                   eval      p1dstsom=tot_TSOm
101900140423     c
102000140423     c                   else
102100140423     c* voglio la % delle fuori orario
102200140423     c                   eval      wfuori_os=tot_tse-tot_tsos
102300140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
102400140423     c                   eval      p1dstsos=Wfuori_os
102500140423
102600140423     c                   eval      wfuori_om=tot_tse-tot_tsom
102700140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
102800140423     c                   eval      p1dstsom=Wfuori_om
102900140423     c
103000140423     c                   endif
103100140423
103200140423     c                   if        *in06
103300140423     c                   write     sto4te
103400140423     c                   write     STO4TEF
103500140423     c                   write     STO4RI
103600140423     c                   setoff                                       06
103700140423     c                   else
103800140423     c                   write     STO4RI
103900140423     c                   write     STO4TOT
104000140423     c  n06              write     STO4RI
104100140423     c                   endif
104200140423     c
104300140423     c                   ENDSR
104400000000     C*------------------------------------------------------------------------*
104500000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
104600000000     C*------------------------------------------------------------------------*
104700000000     C     CARORG        BEGSR
104800000000     C*
104900140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
105000000000     C  N99              READ      AZORG01L                               99
105100000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
105200000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
105300000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
105400000000     C     ORGFAG        IFNE      'V'
105500051109     C                   ADD       1             I
105600000000     C                   Z-ADD     ORGFIL        FIL(I)
105700000000     C                   Z-ADD     ORGCAR        ARE(I)
105800000000     C                   MOVEL     ORGFL3        DIV(I)
105900140404     C* CALCOLA NETWORK
106000140404     C                   MOVEL     ORGDE3        OG143
106100140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
106200051109     c* riempio schiere decodifiche p.o., distretto e area
106300051109     c* distretto
106400140408     C***                Z-ADD     CODUT         KTBUTE
106500140408     C***                MOVEL     '17'          Ktbcod
106600140408     C***                MOVEL(P)  ORGFL3        KKEY
106700140408     C***  KTAB          CHAIN     TABEL00F                           30
106800140408    2C***  *IN30         IFEQ      *OFF
106900140408     C***  TBLFLG        ANDEQ     *BLANKS
107000140408     C***                MOVEL     TBLUNI        DS17
107100140408     C***                MOVEL     §17DES        ddiv(I)
107200140408     C***                ELSE
107300140408     C***                MOVE      *BLANKS       ddiv(I)
107400140409    2C***                END
107500051109     c* area
107600051109     C                   Z-ADD     CODUT         KTBUTE
107700051109     C                   MOVEL     '05'          Ktbcod
107800051109     C                   MOVEL(P)  ORGCAR        KKEY
107900051109     C     KTAB          CHAIN     TABEL00F                           30
108000051109    2C     *IN30         IFEQ      *OFF
108100051109     C     TBLFLG        ANDEQ     *BLANKS
108200051109     C                   MOVEL     TBLUNI        DS05
108300140408     C***                MOVEL     §05DES        dare(I)
108400140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
108500051109     C                   ELSE
108600140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
108700140408     C***                MOVE      *BLANKS       dare(I)
108800051109    2C                   END
108900051109     c* p.o.
109000051109     C                   MOVEl     orgdes        dfil(I)
109100051109     c
109200000000     C                   END
109300000000     C                   END                                                    <-- 2 ---
109400000000     C                   READ      AZORG01L                               99
109500000000     C                   END                                                    <-- 1 ---
109600000000     C*
109700000000     C                   ENDSR
109800000000     C*------------------------------------------------------------------------*
109900051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
110000000000     C*------------------------------------------------------------------------*
110100000000     C     CARTBL        BEGSR
110200000000     C*--------------------
110300051109     C* TIPI BOLLA di recupero
110400000000     C*--------------------
110500000000     C                   Z-ADD     0             I
110600000000     C                   Z-ADD     CODUT         KTBUTE
110700000000     C                   MOVEL     'TB'          KTBCOD
110800000000     C     KEYTAB        SETLL     TABEL00F
110900051109     C                   READe     TABEL00F                               99
111000000000     C     *IN99         DOWEQ     *OFF
111100000000     C     TBLFLG        IFEQ      *BLANKS
111200051109     C                   MOVEL     TBLUNI        DSTB
111300051109     c                   if        §tbrbl = 'R'
111400051109     C                   ADD       1             I
111500051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
111600051109     c                   endif
111700000000     C                   END
111800051109     C     keytab        READe     TABEL00F                               99
111900000000     C                   END
112000000000     C*
112100140428     c* recupero versione cappario alla data DAL
112200140428     C                   Z-ADD     pardsd        P96DRI            8 0
112300140428     C                   Z-ADD     *ZEROS        P96VER            5 0
112400140428     C                   CALL      'TISI96R'
112500140428     C                   PARM                    P96DRI
112600140428     C                   PARM                    P96VER
112700140428     c* carico gli orari in schiera
112800140428     c                   z-add     1             yy
112900140428     c                   if        paror1 >0
113000140428     c                   eval      or_dal(yy)=paror1
113100140428     c                   add       1             yy
113200140428     c                   endif
113300140428     c                   if        paror2 >0
113400140428     c                   eval      or_dal(yy)=paror2
113500140428     c                   add       1             yy
113600140428     c                   endif
113700140428     c                   if        paror3 >0
113800140428     c                   eval      or_dal(yy)=paror3
113900140428     c                   add       1             yy
114000140428     c                   endif
114100140428     c                   if        paror4 >0
114200140428     c                   eval      or_dal(yy)=paror4
114300140428     c                   add       1             yy
114400140428     c                   endif
114500140428     c                   if        paror5 >0
114600140428     c                   eval      or_dal(yy)=paror5
114700140428     c                   add       1             yy
114800140428     c                   endif
114900140428     c                   if        paror6 >0
115000140428     c                   eval      or_dal(yy)=paror6
115100140428     c                   add       1             yy
115200140428     c                   endif
115300140428     c                   if        paror7 >0
115400140428     c                   eval      or_dal(yy)=paror7
115500140428     c                   add       1             yy
115600140428     c                   endif
115700140428     c                   if        paror8 >0
115800140428     c                   eval      or_dal(yy)=paror8
115900140428     c                   add       1             yy
116000140428     c                   endif
116100140428     c                   if        paror9 >0
116200140428     c                   eval      or_dal(yy)=paror9
116300140428     c                   add       1             yy
116400140428     c                   endif
116500140428     C*
116600000000     C                   ENDSR
116700140415     C*------------------------------------------------------------------------*
116800140415     C* Imposto i campi di dettaglio in base ai range passati
116900140415     C*------------------------------------------------------------------------*
117000140415     C     ImpoDET       BEGSR
117100140415     c*
117200140422     c                   if        tashmc<or_dal(1)
117300140422     c                   add       1             wcosmint
117400140422     c                   leavesr
117500140422     c                   endif
117600140422     c                   if        tashmc> paroltre
117700140422     c                   add       1             wcosmaxt
117800140422     c                   leavesr
117900140422     c                   endif
118000140422
118100140422     c                   z-add     1             yy
118200140422     c                   dow       or_dal(yy)>0
118300140423     c                   if        tashmc>=or_dal(yy) and
118400140423     c
118500140423     c                             (or_dal(yy+1)>0 and
118600140423     c                              tashmc< or_dal(Yy+1)  or
118700140423     c                             (or_dal(yy+1)=0 and
118800140423     c                             tashmc<=paroltre))
118900140422     c                   select
119000140422     c                   when      yy=1
119100140415     c                   add       1             wcosor1t
119200140422     c                   when      yy=2
119300140422     c                   add       1             wcosor2t
119400140422     c                   when      yy=3
119500140422     c                   add       1             wcosor3t
119600140422     c                   when      yy=4
119700140422     c                   add       1             wcosor4t
119800140422     c                   when      yy=5
119900140422     c                   add       1             wcosor5t
120000140422     c                   when      yy=6
120100140422     c                   add       1             wcosor6t
120200140422     c                   when      yy=7
120300140422     c                   add       1             wcosor7t
120400140422     c                   when      yy=8
120500140422     c                   add       1             wcosor8t
120600140422     c                   when      yy=9
120700140422     c                   add       1             wcosor9t
120800140422     c                   endsl
120900140422     c
121000140415     c                   leavesr
121100140415     c                   endif
121200140422     c                   add       1             yy
121300140415     c
121400140422     c                   enddo
121500140415     c
121600140415     c                   ENDSR
121700000000     C*------------------------------------------------------------------------*
121800000000     C* *INZSR - ROUTINE INIZIALE
121900000000     C*------------------------------------------------------------------------*
122000000000     C     *INZSR        BEGSR
122100000000     C*--------------------
122200000000     C* RICEVIMENTO PARAMETRI
122300000000     C*--------------------
122400140408     c
122500140408     c     *dtaara       define    §azute        azuteds
122600140408     c     *dtaara       define    §datiute      ddatiute
122700140408     c                   in(E)     *dtaara
122800140408     c                   If        %error  or RSUT = *blanks
122900140408     c                   CLEAR                   tibs34ds
123000140408     c                   CALL      'TIBS34R'
123100140408     c                   PARM                    tibs34ds
123200140408     c                   in        *dtaara
123300140408     c                   EndIf
123400140408     c
123500140408     c                   eval      p1drsut=rsut
123600140408     C*
123700000000     C*--------------------
123800000000     C* CAMPI RIFERITI AL DATA BASE
123900000000     C*--------------------
124000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
124100000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
124200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
124300051109     C     *LIKE         DEFINE    TBLkey        Kkey
124400000000     C*--------------------
124500000000     C* CHIAVI DI LETTURA
124600000000     C*--------------------
124700000000     C*
124800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
124900000000     C     KEYTAB        KLIST
125000000000     C                   KFLD                    KTBUTE
125100000000     C                   KFLD                    KTBCOD
125200051109     C     KTAB          KLIST
125300051109     C                   KFLD                    ktbute
125400051109     C                   KFLD                    ktbcod
125500051109     C                   KFLD                    KKEY
125600000000     C*
125700060524     C     Kar5          KLIST
125800060524     C                   KFLD                    tasAAS
125900060524     C                   KFLD                    taslnp
126000060524     C                   KFLD                    tasnrs
126100060524     C                   KFLD                    tasnsp
126200060524     c                   kfld                    kar5trd
126300140404     C*
126400140404     C     Kdst          KLIST
126500140404     C                   KFLD                    Knpg
126600140404     C                   KFLD                    knFV
126700140404     C                   KFLD                    Kfgs
126800140404     c
126900140415     C     KCCOS         KLIST
127000140415     C                   KFLD                    partser
127100140423     C                   KFLD                    d55tfa
127200140404     C                   KFLD                    tasnzd
127300140404     C                   KFLD                    tascad
127400140404     C                   KFLD                    Kloc
127500140415     C                   KFLD                    kserv
127600140424     C     Kcpc          KLIST
127700140424     C                   KFLD                    p96ver
127800140424     C                   KFLD                    wcosnaz
127900140424     C                   KFLD                    wcoscap
128000140424     C     Kcpl          KLIST
128100140424     C                   KFLD                    p96ver
128200140424     C                   KFLD                    wcosnaz
128300140424     C                   KFLD                    wcoscap
128400140424     C                   KFLD                    wcosloc
128500000000     C*--------------------
128600000000     C                   Z-ADD     1             CODUT             1 0
128700000000     C*
128800000000     C***
128900000000     C* CARICA ORGANIGRAMMA
129000000000     C                   EXSR      CARORG
129100000000     C*
129200140404     C
129300140404     c* apertura file distinte di filiale
129400140508     C                   OPEN(e)   FIDST01L
129500140404     c
129600140508     c                   if        not %open(FIDST01L)
129700140508     c                   eval      %subst(WFDST:7:4)='PRD '
129800140508     c                   eval      %subst(FIAR5:7:4)='GRPS'
129900140508     C                   OPEN      FIDST01L
130000140508     c                   endif
130100140409
130200140409     C                   OPEN(e)   FIAR531C
130300140409     C
130400000000     C                   ENDSR
