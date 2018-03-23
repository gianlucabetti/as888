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
024700140515     C+ select * from titas00f where
024800140515     C+ tasDCM >= :PARDSD and tasDCM <= :PARDSA and
024900140515     C+ tasLNA = :KFIL and tasNZD = ' ' and tasCCA = ' ' and
025000140515     C+ tasFFD = ' '),
025100140515     C+ selTAS10 as (
025200140515     C+ select * from titas10f where
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
037800140514     c                   eval      ioretsp =tastsp
037900140404     c                   call      'TRULORSR'
038000140404     c                   parm                    kpjba
038100140404     c                   parm                    trulorsds
038200140404     c                   parm                    trulor2ds
038300140514     c                   parm                    trulor3ds
038400140514     c                   eval      wnorm=OOR3NORM
038500140514     c
038600140514     c                   if        oor3norm='S'
038700140514     c                   eval      kloc =OOR3LOC_N
038800140514     c                   endif
038900140404     c*
039000140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
039100140415     c                   if        oor2fcsgl='P'
039200140415     c                   eval      kserv='P '
039300140415     c                   else
039400140415     c                   clear                   kserv
039500140415     c                   endif
039600140415
039700140415     c     kccos         chain     wfccos1l
039800140404     c                   if        not %found(wfccos1l)
039900140404     c                   clear                   wfccos00
040000140404     c                   eval      wcosdcmd=pardsd
040100140404     c                   eval      wcosdcma=pardsa
040200140404     c                   eval      wcosver =p96ver
040300140411     c                   eval      wcoslna =d55tfa
040400140404     c                   eval      wcosnaz =tasnzd
040500140404     c                   eval      wcoscap =tascad
040600140404     c                   eval      wcosloc =kloc
040700140404     c                   eval      wcostser=partser
040800140404     c                   eval      wcosorif=OOR2ORIF
040900140415     c                   eval      wcosserv=kserv
041000140424     c
041100140424     c* obsoleto
041200140424     c     kcpc          chain     azcpc01l
041300140424     c                   if        %found(azcpc01l)
041400140424     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
041500140424     c                   endif
041600140424     c                   if        wcosloc<>*blanks
041700140424     c     kcpl          chain     azcpl01l
041800140424     c                   if        %found(azcpl01l)
041900140424     c                   eval      wcosloco=%subst(cplfi1:1:1)
042000140424     c                   endif
042100140424     c                   endif
042200140424     c* Imposto gli orari nel file
042300140424     c                   eval      WCOSOSTIS=OOR2STIS
042400140424     c                   eval      WCOSOSTfS=OOR2STfS
042500140424     c                   eval      WCOSOmiis=OOR2miis
042600140424     c                   eval      WCOSOmxfs=OOR2mxfs
042700140424     c
042800140415     c* imposto orari dettaglio se ci sono e richiestoc
042900140415     c                   if        pardet='S'
043000140422     c                   eval      wcosor1=or_dal(1)
043100140422     c                   eval      wcosor2=or_dal(2)
043200140422     c                   eval      wcosor3=or_dal(3)
043300140422     c                   eval      wcosor4=or_dal(4)
043400140422     c                   eval      wcosor5=or_dal(5)
043500140422     c                   eval      wcosor6=or_dal(6)
043600140422     c                   eval      wcosor7=or_dal(7)
043700140422     c                   eval      wcosor8=or_dal(8)
043800140422     c                   eval      wcosor9=or_dal(9)
043900140422     c                   eval      wcosorOl=paroltre
044000140404     c                   endif
044100140415     c                   endif
044200140404     c
044300140404     c                   add       1             wcosts
044400140404     c* Se loc non normalizzata
044500140514     c                   if        wnorm<>'S'
044600140404     c                   add       1             wcostsNN
044700140404     c                   endif
044800140408     c
044900140408     c* Controllo se spedizione da elaborare
045000140408     c                   exsr      Noelab
045100140408     c
045200140404     c* elaborate
045300140408     c                   if        welab='S'
045400140404     c                   add       1             wcostse
045500140404     c
045600140404     c* consegna negli orari std
045700140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
045800140404     c                   add       1             wcostsos
045900140404     c                   endif
046000140404     c* consegna negli orari min/max
046100140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
046200140404     c                   add       1             wcostsom
046300140404     c                   endif
046400140415     c* imposto campi dettaglio
046500140415     c                   exsr      ImpoDET
046600140408     c                   endif
046700140404     c
046800140404     c                   if        not %found(wfccos1l)
046900140404     c                   write     wfccos00
047000140404     c                   else
047100140404     c                   update    wfccos00
047200140404     c                   endif
047300000000     C*
047400000000     C                   ENDSR
047500140404     C*------------------------------------------------------------------------*
047600140404     C* Stampa - statistica
047700140404     C*------------------------------------------------------------------------*
047800140404     c     STAMPA        BEGSR
047900140404     c* Tpo servizio
048000140404     c                   if        partser='C'
048100140404     c                   eval      p1dtser='CONSEGNE'
048200140404     C                   ELSE
048300140408     c                   eval      p1dtser=' RITIRI '
048400140404     C                   ENDIF
048500140408     c* giro le date
048600140409     C                   MOVE      pardsd        G02inv
048700140408     C                   MOVEL     '3'           G02ERR
048800140408     C                   CALL      'XSRDA8'
048900140408     C                   PARM                    WLBDAT
049000140408     c                   z-add     g02dat        p1ddsd
049100140408     c*
049200140408     c                   if        pardsa>0
049300140409     C                   MOVE      pardsa        G02inv
049400140408     C                   MOVEL     '3'           G02ERR
049500140408     C                   CALL      'XSRDA8'
049600140408     C                   PARM                    WLBDAT
049700140408     c                   z-add     g02dat        p1ddsa
049800140408     c                   endif
049900140408     c* Orari per calcolo %
050000140408     c                   if        parorari='S'
050100140408     c                   eval      p1dorari='STANDARD'
050200140424     c                   eval      p1dtor_d='Standard'
050300140408     c                   else
050400140408     c                   eval      p1dorari='MINIMO/MASSIMO'
050500140424     c                   eval      p1dtor_d=' Min/Max'
050600140408     c                   endif
050700140408     c
050800140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
050900140408     c                   if        parcor='C'
051000140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
051100140411     c                   eval      p1dbolstd='   Nei limiti STD   '
051200140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
051300140408     C                   ELSE
051400140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
051500140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
051600140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
051700140408     C                   ENDIF
051800140423     c
051900140423     c                   write     STO4TE
052000140423
052100140423     c* Orari nel dettaglio
052200140423     c                   if        pardet='S'
052300140423     c                   exsr      TestaDET
052400140423     c                   write     STO4TE_d
052500140423     c                   endif
052600140404     c
052700140404     C
052800140424     c*
052900140404     c     *loval        setll     wfccos1l
053000140404     c                   read      wfccos1l
053100140404     c                   dow       not %eof(wfccos1l)
053200140404     c
053300140408     c* a cambio filiale stampo a pagina nuova
053400140408     c                   if        wcoslna<>savlna
053500140408
053600140408     c                   if        savlna>0
053700140423     c                   exsr      Totfil
053800140423     c
053900140408     c                   write     STO4TE
054000140422     c
054100140422     c                   if        pardet='S'
054200140422     c                   write     STO4RI_d
054300140422     c                   write     STO4TE_d
054400140408     c                   endif
054500140423     c
054600140423     c                   clear                   tot_ts
054700140423     c                   clear                   tot_tse
054800140423     c                   clear                   tot_tsos
054900140423     c                   clear                   tot_tsom
055000140422     c                   endif
055100140408
055200140408     c                   z-add     1             i
055300140408     C     wcoslna       lookup    fil(i)                                 55
055400140404     C                   if        %found
055500140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
055600140408     c                   eval      p1dare=dare(i)
055700140404     C                   else
055800140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
055900140408     c                   eval      p1dare=*all'?'
056000140404     C                   endif
056100140408
056200140408     c                   eval      savlna=wcoslna
056300140408
056400140408     c                   write     STO4TEF
056500140414     c                   write     STO4RI
056600140422     c                   if        pardet='S'
056700140422     c                   write     STO4TEF_d
056800140422     c                   write     STO4RI_d
056900140408     c                   endif
057000140422     c                   endif
057100140408
057200140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
057300140411     c                   if        wcostse>=parnum and wcostse>0
057400140408     c* voglio la % di correttezza
057500140408     c                   if        parcor='C'
057600140404     c* Calcolo % STD
057700140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
057800140408     c                   eval      p1dstsos=WCOSTSOs
057900140404     c* Calcolo % MIN/MAX
058000140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
058100140408     c                   eval      p1dstsom=WCOSTSOm
058200140408     c
058300140408     c                   else
058400140408     c* voglio la % delle fuori orario
058500140408     c                   eval      wfuori_os=wcostse-wcostsos
058600140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
058700140408     c                   eval      p1dstsos=Wfuori_os
058800140408
058900140408     c                   eval      wfuori_om=wcostse-wcostsom
059000140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
059100140408     c                   eval      p1dstsom=Wfuori_om
059200140408     c                   endif
059300140408     c
059400140407     c* stampo solo al di sotto della % richiesta
059500140408     c                   eval      wstampadet='S'
059600140408
059700140408     c                   select
059800140408     c                   when      parperc=0
059900140408
060000140408     c                   when      parorari='S'  and parcor='C' and
060100140408     c                             p1dperc_os>=parperc
060200140408     c                   eval      wstampadet='N'
060300140408     c
060400140408     c                   when      parorari='S'  and parcor='F' and
060500140408     c                             p1dperc_os< parperc
060600140408     c                   eval      wstampadet='N'
060700140408
060800140408     c                   when      parorari='M'  and parcor='C' and
060900140408     c                             p1dperc_om>=parperc
061000140408     c                   eval      wstampadet='N'
061100140408     c
061200140408     c                   when      parorari='M'  and parcor='F' and
061300140408     c                             p1dperc_om< parperc
061400140408     c                   eval      wstampadet='N'
061500140408     c                   endsl
061600140404     c*
061700140408     c                   if        wstampadet='S'
061800140424     c* POM
061900140424     c                   if        wcosserv='P '
062000140424     c                   eval      p1dpom='Pom'
062100140424     c                   else
062200140424     c                   eval      p1dpom='   '
062300140424     c                   endif
062400140411     c*
062500140411     c* Se la località è vuota imposto '??'
062600140411     c                   if        wcosloc=*blanks
062700140411     c                   eval      wcosloc='??'
062800140411     c                   endif
062900140408     c
063000140408     c                   if        *in06
063100140408     c                   write     sto4te
063200140408     c                   write     STO4TEF
063300140414     c                   write     STO4RI
063400140408     c                   setoff                                       06
063500140404     c                   endif
063600140408     c
063700140408     c* Richiamo pgm reperimento orari servizi
063800140424     c***                clear                   trulorsds
063900140424     c***                clear                   trulor2ds
064000140424     c***                eval      ioredta=pardsd
064100140424     c***                eval      iorefil=wcoslna
064200140424     c***                eval      iorenar=wcosnaz
064300140424     c***                eval      iorecap=wcoscap
064400140424     c***                eval      ioreloc=wcosloc
064500140424     c***                if        wcosloc=*blanks
064600140424     c***                eval      ioreloc='A '
064700140424     c***                endif
064800140424     c***                eval      ioretser=wcostser
064900140424     c***                eval      ioretsp ='C'
065000140424     c***                call      'TRULORSR'
065100140424     c***                parm                    kpjba
065200140424     c***                parm                    trulorsds
065300140424     c**+                parm                    trulor2ds
065400140408     c
065500140424     c                   eval      p1dstis=wcosoSTiS
065600140424     c                   eval      p1dstfs=wcosoSTFS
065700140424     c                   eval      p1dmiis=wcosomiis
065800140424     c                   eval      p1dmxfs=wcosomxfs
065900140404     c
066000140408     c                   write     STO4det
066100140422     c
066200140422     c* se devo stampare anche il dettaglio
066300140422     c                   if        pardet='S'
066400140422     c                   exsr      Stadet
066500140422     c                   endif
066600140408     c
066700140407     c                   endif
066800140408     c                   endif
066900140424     c* totali di filiale
067000140424     c                   add       WCOSTS        tot_ts
067100140424     c                   add       WCOSTSE       tot_tse
067200140424     c                   add       WCOSTSos      tot_tsos
067300140424     c                   add       WCOSTSom      tot_tsom
067400140407     c
067500140404     c                   read      wfccos1l
067600140404     c
067700140404     c                   enddo
067800140423     c
067900140423     c* totali finali
068000140423     c                   if        savlna>0
068100140423     c                   exsr      totfil
068200140423     c                   endif
068300140404     c
068400140404     c                   ENDSR
068500140423     C*------------------------------------------------------------------------*
068600140423     c* Imposta gli orari in testata dettaglioù
068700140423     C*------------------------------------------------------------------------*
068800140423     c     TestaDet      BEGSR
068900140423     c                   eval      p1dsot = paror1
069000140423     c                   eval      p1doltre= paroltre
069100140423     c                   eval      p1dda1  = or_dal(1)
069200140423
069300140423     c                   if        or_dal(2)>0
069400140423     c                   eval      p1dda2  = or_dal(2)
069500140423     c                   movel     or_dal(2)     w0060
069600140423     c                   move      w0060         timeiso
069700140423     c                   subdur    1:*mn         timeiso
069800140423     c                   eval      w0060     =%dec(timeiso)
069900140423     c                   movel     w0060         p1dal1
070000140423     c                   else
070100140423     c                   eval      p1dal1=paroltre
070200140423     c                   leavesr
070300140423     c                   endif
070400140423     c
070500140423     c                   if        or_dal(3)>0
070600140423     c                   eval      p1dda3  = or_dal(3)
070700140423     c                   movel     or_dal(3)     w0060
070800140423     c                   move      w0060         timeiso
070900140423     c                   subdur    1:*mn         timeiso
071000140423     c                   eval      w0060     =%dec(timeiso)
071100140423     c                   movel     w0060         p1dal2
071200140423     c                   else
071300140423     c                   eval      p1dal2=paroltre
071400140423     c                   leavesr
071500140423     c                   endif
071600140423
071700140423     c                   if        or_dal(4)>0
071800140423     c                   eval      p1dda4  = or_dal(4)
071900140423     c                   movel     or_dal(4)     w0060
072000140423     c                   move      w0060         timeiso
072100140423     c                   subdur    1:*mn         timeiso
072200140423     c                   eval      w0060     =%dec(timeiso)
072300140423     c                   movel     w0060         p1dal3
072400140423     c                   else
072500140423     c                   eval      p1dal3=paroltre
072600140423     c                   leavesr
072700140423     c                   endif
072800140423
072900140423     c                   if        or_dal(5)>0
073000140423     c                   eval      p1dda5  = or_dal(5)
073100140423     c                   movel     or_dal(5)     w0060
073200140423     c                   move      w0060         timeiso
073300140423     c                   subdur    1:*mn         timeiso
073400140423     c                   eval      w0060     =%dec(timeiso)
073500140423     c                   movel     w0060         p1dal4
073600140423     c                   else
073700140423     c                   eval      p1dal4=paroltre
073800140423     c                   leavesr
073900140423     c                   endif
074000140423
074100140423     c                   if        or_dal(6)>0
074200140423     c                   eval      p1dda6  = or_dal(6)
074300140423     c                   movel     or_dal(6)     w0060
074400140423     c                   move      w0060         timeiso
074500140423     c                   subdur    1:*mn         timeiso
074600140423     c                   eval      w0060     =%dec(timeiso)
074700140423     c                   movel     w0060         p1dal5
074800140423     c                   else
074900140423     c                   eval      p1dal5=paroltre
075000140423     c                   leavesr
075100140423     c                   endif
075200140423
075300140423     c                   if        or_dal(7)>0
075400140423     c                   eval      p1dda7  = or_dal(7)
075500140423     c                   movel     or_dal(7)     w0060
075600140423     c                   move      w0060         timeiso
075700140423     c                   subdur    1:*mn         timeiso
075800140423     c                   eval      w0060     =%dec(timeiso)
075900140423     c                   movel     w0060         p1dal6
076000140423     c                   else
076100140423     c                   eval      p1dal6=paroltre
076200140423     c                   leavesr
076300140423     c                   endif
076400140423
076500140423     c                   if        or_dal(8)>0
076600140423     c                   eval      p1dda8  = or_dal(8)
076700140423     c                   movel     or_dal(8)     w0060
076800140423     c                   move      w0060         timeiso
076900140423     c                   subdur    1:*mn         timeiso
077000140423     c                   eval      w0060     =%dec(timeiso)
077100140423     c                   movel     w0060         p1dal7
077200140423     c                   else
077300140423     c                   eval      p1dal7=paroltre
077400140423     c                   leavesr
077500140423     c                   endif
077600140423     c                   if        or_dal(9)>0
077700140423     c                   eval      p1dda9  = or_dal(9)
077800140423     c                   movel     or_dal(9)     w0060
077900140423     c                   move      w0060         timeiso
078000140423     c                   subdur    1:*mn         timeiso
078100140423     c                   eval      w0060     =%dec(timeiso)
078200140423     c                   movel     w0060         p1dal8
078300140423     c
078400140423     c                   eval      p1dal9=paroltre
078500140423     c                   else
078600140423     c                   eval      p1dal8=paroltre
078700140423     c                   leavesr
078800140423     c                   endif
078900140423     c
079000140423     c                   ENDSR
079100140422     C*------------------------------------------------------------------------*
079200140422     C* Stampa dettaglio
079300140422     C*------------------------------------------------------------------------*
079400140422     C     StaDET        BEGSR
079500140422     c                   if        *in07
079600140422     c                   write     sto4te_d
079700140422     c                   write     STO4TEF_d
079800140422     c                   write     STO4RI_d
079900140422     c                   setoff                                       07
080000140422     c                   endif
080100140424     c* Imposto gli orari
080200140424     c                   if        parorari='S'
080300140424     c                   eval      p1dor_dal=WCOSOSTIS
080400140424     c                   eval      p1dor_al =WCOSOSTfS
080500140424     c                   else
080600140424     c                   eval      p1dor_dal=WCOSOmiis
080700140424     c                   eval      p1dor_al =WCOSOmxfS
080800140424     c                   endif
080900140424     c                   setoff                                       111213
081000140424     c                   setoff                                       141516
081100140424     c                   setoff                                       171819
081200140505     c                   setoff                                       2021
081300140422     c* calcolo le %
081400140422     c* SOTTO
081500140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
081600140422     C                   IF        WPERC>0
081700140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
081800140505     c                   seton                                        20
081900140422     c                   else
082000140424     c                   eval      p1cp_sot='    '
082100140422     c                   endif
082200140422     c* OLTRE
082300140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
082400140422     C                   IF        WPERC>0
082500140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
082600140505     c                   seton                                        21
082700140422     c                   else
082800140424     c                   eval      p1cp_olt='    '
082900140422     c                   endif
083000140422     c* % 1
083100140505    1c                   if        wcosor1  >0
083200140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
083300140422     c
083400140505    2C                   IF        WPERC>0
083500140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
083600140505     c
083700140505    2c                   select
083800140505     c                   when      or_dal(1)<p1dor_dal
083900140505     c                   seton                                        11
084000140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
084100140505     c                   seton                                        11
084200140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
084300140505     c                   seton                                        11
084400140505    2c                   endsl
084500140505
084600140422     c                   else
084700140424     c                   eval      p1cp_o1 ='    '
084800140505    2c                   endif
084900140424     c
085000140505    1c                   endif
085100140422     c
085200140422     c* % 2
085300140422     c                   if        wcosor2  >0
085400140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
085500140422     c
085600140422     C                   IF        WPERC>0
085700140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
085800140505
085900140505     c                   select
086000140505     c                   when      or_dal(2)<p1dor_dal
086100140505     c                   seton                                        12
086200140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
086300140505     c                   seton                                        12
086400140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
086500140505     c                   seton                                        12
086600140505     c                   endsl
086700140505
086800140422     c                   else
086900140424     c                   eval      p1cp_o2 ='    '
087000140422     c                   endif
087100140422     c                   endif
087200140423     c* % 3
087300140423     c                   if        wcosor3  >0
087400140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
087500140423     c
087600140423     C                   IF        WPERC>0
087700140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
087800140505
087900140505     c                   select
088000140505     c                   when      or_dal(3)<p1dor_dal
088100140505     c                   seton                                        13
088200140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
088300140505     c                   seton                                        13
088400140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
088500140505     c                   seton                                        13
088600140505     c                   endsl
088700140423     c                   else
088800140424     c                   eval      p1cp_o3 ='    '
088900140423     c                   endif
089000140423     c                   endif
089100140423     c* % 4
089200140423     c                   if        wcosor4  >0
089300140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
089400140423     c
089500140423     C                   IF        WPERC>0
089600140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
089700140505
089800140505     c                   select
089900140505     c                   when      or_dal(4)<p1dor_dal
090000140505     c                   seton                                        14
090100140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
090200140505     c                   seton                                        14
090300140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
090400140505     c                   seton                                        14
090500140505     c                   endsl
090600140423     c                   else
090700140424     c                   eval      p1cp_o4 ='    '
090800140423     c                   endif
090900140423     c                   endif
091000140423     c* % 5
091100140423     c                   if        wcosor5  >0
091200140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
091300140423     c
091400140423     C                   IF        WPERC>0
091500140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
091600140505
091700140505     c                   select
091800140505     c                   when      or_dal(5)<p1dor_dal
091900140505     c                   seton                                        15
092000140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
092100140505     c                   seton                                        15
092200140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
092300140505     c                   seton                                        15
092400140505     c                   endsl
092500140423     c                   else
092600140424     c                   eval      p1cp_o5 ='    '
092700140423     c                   endif
092800140423     c                   endif
092900140423     c* % 6
093000140423     c                   if        wcosor6  >0
093100140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
093200140423     c
093300140423     C                   IF        WPERC>0
093400140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
093500140505
093600140505     c                   select
093700140505     c                   when      or_dal(6)<p1dor_dal
093800140505     c                   seton                                        16
093900140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
094000140505     c                   seton                                        16
094100140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
094200140505     c                   seton                                        16
094300140505     c                   endsl
094400140423     c                   else
094500140424     c                   eval      p1cp_o6 ='    '
094600140423     c                   endif
094700140423     c                   endif
094800140423     c* % 7
094900140423     c                   if        wcosor7  >0
095000140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
095100140423     c
095200140423     C                   IF        WPERC>0
095300140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
095400140505
095500140505     c                   select
095600140505     c                   when      or_dal(7)<p1dor_dal
095700140505     c                   seton                                        17
095800140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
095900140505     c                   seton                                        17
096000140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
096100140505     c                   seton                                        17
096200140505     c                   endsl
096300140423     c                   else
096400140424     c                   eval      p1cp_o7 ='    '
096500140423     c                   endif
096600140423     c                   endif
096700140423     c* % 8
096800140423     c                   if        wcosor8  >0
096900140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
097000140423     c
097100140423     C                   IF        WPERC>0
097200140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
097300140505
097400140505     c                   select
097500140505     c                   when      or_dal(8)<p1dor_dal
097600140505     c                   seton                                        18
097700140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
097800140505     c                   seton                                        18
097900140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
098000140505     c                   seton                                        18
098100140505     c                   endsl
098200140423     c                   else
098300140424     c                   eval      p1cp_o8 ='    '
098400140423     c                   endif
098500140423     c                   endif
098600140423     c* % 9
098700140423     c                   if        wcosor9  >0
098800140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
098900140423     c
099000140423     C                   IF        WPERC>0
099100140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
099200140505
099300140505     c                   select
099400140505     c                   when      or_dal(9)<p1dor_dal
099500140505     c                   seton                                        19
099600140505     c                   when      paroltre >p1dor_al
099700140505     c                   seton                                        19
099800140505     c                   endsl
099900140423     c                   else
100000140424     c                   eval      p1cp_o9 ='    '
100100140423     c                   endif
100200140423     c                   endif
100300140422     c
100400140422     c                   write     STO4det_d
100500140422     c                   ENDSR
100600140423     C*------------------------------------------------------------------------*
100700140423     c* Stampa totali di filiale
100800140423     C*------------------------------------------------------------------------*
100900140423     c     TOTfil        BEGSR
101000140423     c* voglio la % di correttezza
101100140423     c                   if        parcor='C'
101200140423     c* Calcolo % STD
101300140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
101400140423     c                   eval      p1dstsos=tot_TSOs
101500140423     c* Calcolo % MIN/MAX
101600140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
101700140423     c                   eval      p1dstsom=tot_TSOm
101800140423     c
101900140423     c                   else
102000140423     c* voglio la % delle fuori orario
102100140423     c                   eval      wfuori_os=tot_tse-tot_tsos
102200140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
102300140423     c                   eval      p1dstsos=Wfuori_os
102400140423
102500140423     c                   eval      wfuori_om=tot_tse-tot_tsom
102600140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
102700140423     c                   eval      p1dstsom=Wfuori_om
102800140423     c
102900140423     c                   endif
103000140423
103100140423     c                   if        *in06
103200140423     c                   write     sto4te
103300140423     c                   write     STO4TEF
103400140423     c                   write     STO4RI
103500140423     c                   setoff                                       06
103600140423     c                   else
103700140423     c                   write     STO4RI
103800140423     c                   write     STO4TOT
103900140423     c  n06              write     STO4RI
104000140423     c                   endif
104100140423     c
104200140423     c                   ENDSR
104300000000     C*------------------------------------------------------------------------*
104400000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
104500000000     C*------------------------------------------------------------------------*
104600000000     C     CARORG        BEGSR
104700000000     C*
104800140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
104900000000     C  N99              READ      AZORG01L                               99
105000000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
105100000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
105200000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
105300000000     C     ORGFAG        IFNE      'V'
105400051109     C                   ADD       1             I
105500000000     C                   Z-ADD     ORGFIL        FIL(I)
105600000000     C                   Z-ADD     ORGCAR        ARE(I)
105700000000     C                   MOVEL     ORGFL3        DIV(I)
105800140404     C* CALCOLA NETWORK
105900140404     C                   MOVEL     ORGDE3        OG143
106000140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
106100051109     c* riempio schiere decodifiche p.o., distretto e area
106200051109     c* distretto
106300140408     C***                Z-ADD     CODUT         KTBUTE
106400140408     C***                MOVEL     '17'          Ktbcod
106500140408     C***                MOVEL(P)  ORGFL3        KKEY
106600140408     C***  KTAB          CHAIN     TABEL00F                           30
106700140408    2C***  *IN30         IFEQ      *OFF
106800140408     C***  TBLFLG        ANDEQ     *BLANKS
106900140408     C***                MOVEL     TBLUNI        DS17
107000140408     C***                MOVEL     §17DES        ddiv(I)
107100140408     C***                ELSE
107200140408     C***                MOVE      *BLANKS       ddiv(I)
107300140409    2C***                END
107400051109     c* area
107500051109     C                   Z-ADD     CODUT         KTBUTE
107600051109     C                   MOVEL     '05'          Ktbcod
107700051109     C                   MOVEL(P)  ORGCAR        KKEY
107800051109     C     KTAB          CHAIN     TABEL00F                           30
107900051109    2C     *IN30         IFEQ      *OFF
108000051109     C     TBLFLG        ANDEQ     *BLANKS
108100051109     C                   MOVEL     TBLUNI        DS05
108200140408     C***                MOVEL     §05DES        dare(I)
108300140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
108400051109     C                   ELSE
108500140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
108600140408     C***                MOVE      *BLANKS       dare(I)
108700051109    2C                   END
108800051109     c* p.o.
108900051109     C                   MOVEl     orgdes        dfil(I)
109000051109     c
109100000000     C                   END
109200000000     C                   END                                                    <-- 2 ---
109300000000     C                   READ      AZORG01L                               99
109400000000     C                   END                                                    <-- 1 ---
109500000000     C*
109600000000     C                   ENDSR
109700000000     C*------------------------------------------------------------------------*
109800051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
109900000000     C*------------------------------------------------------------------------*
110000000000     C     CARTBL        BEGSR
110100000000     C*--------------------
110200051109     C* TIPI BOLLA di recupero
110300000000     C*--------------------
110400000000     C                   Z-ADD     0             I
110500000000     C                   Z-ADD     CODUT         KTBUTE
110600000000     C                   MOVEL     'TB'          KTBCOD
110700000000     C     KEYTAB        SETLL     TABEL00F
110800051109     C                   READe     TABEL00F                               99
110900000000     C     *IN99         DOWEQ     *OFF
111000000000     C     TBLFLG        IFEQ      *BLANKS
111100051109     C                   MOVEL     TBLUNI        DSTB
111200051109     c                   if        §tbrbl = 'R'
111300051109     C                   ADD       1             I
111400051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
111500051109     c                   endif
111600000000     C                   END
111700051109     C     keytab        READe     TABEL00F                               99
111800000000     C                   END
111900000000     C*
112000140428     c* recupero versione cappario alla data DAL
112100140428     C                   Z-ADD     pardsd        P96DRI            8 0
112200140428     C                   Z-ADD     *ZEROS        P96VER            5 0
112300140428     C                   CALL      'TISI96R'
112400140428     C                   PARM                    P96DRI
112500140428     C                   PARM                    P96VER
112600140428     c* carico gli orari in schiera
112700140428     c                   z-add     1             yy
112800140428     c                   if        paror1 >0
112900140428     c                   eval      or_dal(yy)=paror1
113000140428     c                   add       1             yy
113100140428     c                   endif
113200140428     c                   if        paror2 >0
113300140428     c                   eval      or_dal(yy)=paror2
113400140428     c                   add       1             yy
113500140428     c                   endif
113600140428     c                   if        paror3 >0
113700140428     c                   eval      or_dal(yy)=paror3
113800140428     c                   add       1             yy
113900140428     c                   endif
114000140428     c                   if        paror4 >0
114100140428     c                   eval      or_dal(yy)=paror4
114200140428     c                   add       1             yy
114300140428     c                   endif
114400140428     c                   if        paror5 >0
114500140428     c                   eval      or_dal(yy)=paror5
114600140428     c                   add       1             yy
114700140428     c                   endif
114800140428     c                   if        paror6 >0
114900140428     c                   eval      or_dal(yy)=paror6
115000140428     c                   add       1             yy
115100140428     c                   endif
115200140428     c                   if        paror7 >0
115300140428     c                   eval      or_dal(yy)=paror7
115400140428     c                   add       1             yy
115500140428     c                   endif
115600140428     c                   if        paror8 >0
115700140428     c                   eval      or_dal(yy)=paror8
115800140428     c                   add       1             yy
115900140428     c                   endif
116000140428     c                   if        paror9 >0
116100140428     c                   eval      or_dal(yy)=paror9
116200140428     c                   add       1             yy
116300140428     c                   endif
116400140428     C*
116500000000     C                   ENDSR
116600140415     C*------------------------------------------------------------------------*
116700140415     C* Imposto i campi di dettaglio in base ai range passati
116800140415     C*------------------------------------------------------------------------*
116900140415     C     ImpoDET       BEGSR
117000140415     c*
117100140422     c                   if        tashmc<or_dal(1)
117200140422     c                   add       1             wcosmint
117300140422     c                   leavesr
117400140422     c                   endif
117500140422     c                   if        tashmc> paroltre
117600140422     c                   add       1             wcosmaxt
117700140422     c                   leavesr
117800140422     c                   endif
117900140422
118000140422     c                   z-add     1             yy
118100140422     c                   dow       or_dal(yy)>0
118200140423     c                   if        tashmc>=or_dal(yy) and
118300140423     c
118400140423     c                             (or_dal(yy+1)>0 and
118500140423     c                              tashmc< or_dal(Yy+1)  or
118600140423     c                             (or_dal(yy+1)=0 and
118700140423     c                             tashmc<=paroltre))
118800140422     c                   select
118900140422     c                   when      yy=1
119000140415     c                   add       1             wcosor1t
119100140422     c                   when      yy=2
119200140422     c                   add       1             wcosor2t
119300140422     c                   when      yy=3
119400140422     c                   add       1             wcosor3t
119500140422     c                   when      yy=4
119600140422     c                   add       1             wcosor4t
119700140422     c                   when      yy=5
119800140422     c                   add       1             wcosor5t
119900140422     c                   when      yy=6
120000140422     c                   add       1             wcosor6t
120100140422     c                   when      yy=7
120200140422     c                   add       1             wcosor7t
120300140422     c                   when      yy=8
120400140422     c                   add       1             wcosor8t
120500140422     c                   when      yy=9
120600140422     c                   add       1             wcosor9t
120700140422     c                   endsl
120800140422     c
120900140415     c                   leavesr
121000140415     c                   endif
121100140422     c                   add       1             yy
121200140415     c
121300140422     c                   enddo
121400140415     c
121500140415     c                   ENDSR
121600000000     C*------------------------------------------------------------------------*
121700000000     C* *INZSR - ROUTINE INIZIALE
121800000000     C*------------------------------------------------------------------------*
121900000000     C     *INZSR        BEGSR
122000000000     C*--------------------
122100000000     C* RICEVIMENTO PARAMETRI
122200000000     C*--------------------
122300140408     c
122400140408     c     *dtaara       define    §azute        azuteds
122500140408     c     *dtaara       define    §datiute      ddatiute
122600140408     c                   in(E)     *dtaara
122700140408     c                   If        %error  or RSUT = *blanks
122800140408     c                   CLEAR                   tibs34ds
122900140408     c                   CALL      'TIBS34R'
123000140408     c                   PARM                    tibs34ds
123100140408     c                   in        *dtaara
123200140408     c                   EndIf
123300140408     c
123400140408     c                   eval      p1drsut=rsut
123500140408     C*
123600000000     C*--------------------
123700000000     C* CAMPI RIFERITI AL DATA BASE
123800000000     C*--------------------
123900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
124000000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
124100000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
124200051109     C     *LIKE         DEFINE    TBLkey        Kkey
124300000000     C*--------------------
124400000000     C* CHIAVI DI LETTURA
124500000000     C*--------------------
124600000000     C*
124700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
124800000000     C     KEYTAB        KLIST
124900000000     C                   KFLD                    KTBUTE
125000000000     C                   KFLD                    KTBCOD
125100051109     C     KTAB          KLIST
125200051109     C                   KFLD                    ktbute
125300051109     C                   KFLD                    ktbcod
125400051109     C                   KFLD                    KKEY
125500000000     C*
125600060524     C     Kar5          KLIST
125700060524     C                   KFLD                    tasAAS
125800060524     C                   KFLD                    taslnp
125900060524     C                   KFLD                    tasnrs
126000060524     C                   KFLD                    tasnsp
126100060524     c                   kfld                    kar5trd
126200140404     C*
126300140404     C     Kdst          KLIST
126400140404     C                   KFLD                    Knpg
126500140404     C                   KFLD                    knFV
126600140404     C                   KFLD                    Kfgs
126700140404     c
126800140415     C     KCCOS         KLIST
126900140415     C                   KFLD                    partser
127000140423     C                   KFLD                    d55tfa
127100140404     C                   KFLD                    tasnzd
127200140404     C                   KFLD                    tascad
127300140404     C                   KFLD                    Kloc
127400140415     C                   KFLD                    kserv
127500140424     C     Kcpc          KLIST
127600140424     C                   KFLD                    p96ver
127700140424     C                   KFLD                    wcosnaz
127800140424     C                   KFLD                    wcoscap
127900140424     C     Kcpl          KLIST
128000140424     C                   KFLD                    p96ver
128100140424     C                   KFLD                    wcosnaz
128200140424     C                   KFLD                    wcoscap
128300140424     C                   KFLD                    wcosloc
128400000000     C*--------------------
128500000000     C                   Z-ADD     1             CODUT             1 0
128600000000     C*
128700000000     C***
128800000000     C* CARICA ORGANIGRAMMA
128900000000     C                   EXSR      CARORG
129000000000     C*
129100140404     C
129200140404     c* apertura file distinte di filiale
129300140508     C                   OPEN(e)   FIDST01L
129400140404     c
129500140508     c                   if        not %open(FIDST01L)
129600140508     c                   eval      %subst(WFDST:7:4)='PRD '
129700140508     c                   eval      %subst(FIAR5:7:4)='GRPS'
129800140508     C                   OPEN      FIDST01L
129900140508     c                   endif
130000140409
130100140409     C                   OPEN(e)   FIAR531C
130200140409     C
130300000000     C                   ENDSR
