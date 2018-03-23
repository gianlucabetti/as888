000100000000     H*------------------------------------------------------------------------*
000200140404     H* Statistica controllo orari srervizi  - Crea Work file
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500140409     FTITAS26C  IF   E           K DISK    extfile(titas) usropn
000600140508     Ffidst01l  IF   E           K DISK    extfile(wfdst) usropn
000700140409     Ffiar531c  IF   E           K DISK    extfile(fiar5) usropn
000800000000     FAZORG01L  IF   E           K DISK
000900140408     FAZORG02L  IF   E           K DISK    rename(azorg:azorg002)
001000000000     FTABEL00F  IF   E           K DISK
001100140424     Fazcpc01l  IF   E           K DISK
001200140424     Fazcpl01l  IF   E           K DISK
001300140404     FWFCCOS1l  UF A E           K DISK
001400140408     FTISTO4P   O    E             PRINTER OFLIND(*IN06)
001500140422     FTISTO4P1  O    E             PRINTER OFLIND(*IN07)
001600000000     D*------------------------------------------------------------------------*
001700000000     D* SCHIERE
001800000000     D*------------------------------------------------------------------------*
001900000000     D*-------------------
002000000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
002100000000     D*-------------------
002200000000     D FIL             S              3  0 DIM(500)                             *FILIALE
002300000000     D ARE             S              3  0 DIM(500)                             *AREA
002400000000     D DIV             S              1    DIM(500)                             *DIVISIONE
002500051109     D dfil            S             20    DIM(500)                             *FILIALE
002600140408     D dare            S             29    DIM(500)                             *AREA
002700140408     D** ddiv            S             25    DIM(500)                             *DIVISIONE
002800000000     D*-------------------
002900000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
003000051109     D CT              S              2    DIM(50)                              TIP0 BOLLA
003100140404     d
003200140404     D NTW             S              3    DIM(500)                             *NETWORK
003300140408     d
003400140408     D L6              S              3  0 DIM(30)
003500140422     d
003600140422     D or_dal          S              4  0 DIM(11)
003700000000     D*------------------------------------------------------------------------*
003800000000     D KPJBA         E DS
003900140408     d Azuteds       e ds                  Extname(Azute00f)
004000140408     d dDatiute      e ds
004100140408     d Tibs34ds      e ds                  Inz
004200140409     D azorgds       E DS                  EXTNAME(azorg00f)
004300140512     D titasds       E DS                  EXTNAME(titas00f)
004400000000     D*-------------------
004500000000     D DSTB          E DS
004600051109     D DS05          E DS
004700051109     D DS17          E DS
004800051109     D DS1b          E DS
004900040220     D*------------------
005000140404     D tisi95ds      E DS
005100140404     D dar5GEN       E DS
005200140404     D og143         E DS
005300140404     d
005400140404     D TRULORSDS     E DS
005500140404     D TRULOR2DS     E DS
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
010800140512     D savcad          s                   like(tascad)
010900140512     D savlod          s                   like(taslod)
011000140512     D knfv            s                   like(dstnfv)
011100140404     D kloc            s                   like(o95loc)
011200140408     D wfuori_os       s                   like(wcostsOS)
011300140408     D wfuori_om       s                   like(wcostsOm)
011400140415     D kserv           s                   like(wcosserv)
011500140422     d Timeiso         s               t   timfmt(*iso)
011600140422     d w0060           s              6  0
011700140422     d wperc           s              3  0
011800140505     d wcosts          s              9  0
011900140505     d tot_ts          s              9  0
012000140423     d tot_tse         s              9  0
012100140423     d tot_tsos        s              9  0
012200140423     d tot_tsom        s              9  0
012300140404     d
012400140508     d WFDST           s             21    inz('FILTRA201 /FIDST01L')
012500140409     d titas           s             21    inz('GAITRAGRU /TITAS26C')
012600140409     d fiar5           s             21    inz('GAITRAGRU /FIAR531C')
012700000000     C*------------------------------------------------------------------------*
012800000000     C* MAIN LINE
012900000000     C*------------------------------------------------------------------------*
013000140428     C     *ENTRY        PLIST
013100140428     C                   PARM                    KPJBA
013200140428     C                   MOVEL     KPJBU         PARAM
013300140409      /free
013400140409         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013500140409      /end-free
013600140428     C* CARICA TIPI BOLLA
013700140428     C                   EXSR      CARTBL
013800140408     c*
013900140408     c* se impostata l'area leggo le filiali dell'are altrimenti la singola filiale
014000140408     c*  altrimenti tutto
014100140408     c                   select
014200140408     c                   when      parfil>0
014300140408     c* carico la sua £6
014400140408     C                   CLEAR                   trul06ds
014500140408     C                   MOVE      '£6'          D06COD
014600140408     C                   movel     parfil        D06KEY
014700140408     c                   move      'S'           d06key
014800140409     c                   if        pardsa>0
014900140408     c                   movel     pardsa        d06drf
015000140409     c                   else
015100140409     c                   movel     pardsd        d06drf
015200140409     c                   endif
015300140408     C                   MOVEL     'L'           D06TLA
015400140408     C                   MOVEL     trul06ds      KPJBU
015500140408     C*
015600140408     C                   CALL      'TRUL06R'
015700140408     C                   PARM                    KPJBA
015800140408     C                   MOVEL     KPJBU         trul06ds
015900140408     C                   MOVEA     LIN           L6
016000140423     c                   z-add     1             XX
016100140408
016200140423     c                   dow       l6(XX)>0
016300140423     c                   eval      kfil=l6(XX)
016400140408     c                   exsr      Let_TAS
016500140423     c                   add       1             XX
016600140408     c                   enddo
016700140408     c* immesso cod area
016800140408     c                   when      parcar>0
016900140409     C/EXEC SQL
017000140409     C+ Declare A1 Cursor for
017100140409     c+ SELECT * FROM azorg00f
017200140409     c+ WHERE orgcar=  :parcar AND orgfva=' ' and (orgfag='F' or orgfag='A')
017300140409     c+ ORDER BY orgfil
017400140409     C/END-EXEC
017500140409      *          apertura cursore
017600140409     C/EXEC SQL
017700140409     C+ OPEN A1
017800140409     C/END-EXEC
017900140409     C                   DOU       SqlCod <> 0
018000140409     C/EXEC SQL
018100140409     C+ Fetch Next From A1 Into :azorgds
018200140409     C/END-EXEC
018300140409     c                   select
018400140409     C                   WHEN      SqlCod = 100
018500140409     c                   leave
018600140409      *
018700140409     C                   WHEN      SqlCod < 0
018800140409     c                   seton                                        H1
018900140409     c                   leave
019000140409      *
019100140409     C                   OTHER
019200140408     c                   eval      kfil=orgfil
019300140408     c                   exsr      Let_TAS
019400140409     C                   ENDSL
019500140409
019600140408     c                   enddo
019700140409     C/EXEC SQL
019800140409     C+ Close A1
019900140409     C/END-EXEC
020000140408     c
020100140408     c                   other
020200140409
020300140408     c     *loval        setll     azorg02l
020400140408     c                   read      azorg02l
020500140408     c                   dow       not %eof(azorg02l)
020600140408     c                   eval      kfil=orgfil
020700140408     c                   exsr      Let_TAS
020800140408     c                   read      azorg02l
020900140408     c                   enddo
021000140408     c
021100140408     c                   endsl
021200140404     c*
021300140404     c                   EXSR      Stampa
021400140408     c*
021500140408     c                   write     STO4FI
021600140422     c
021700140422     c                   if        pardet='S'
021800140422     c                   write     STO4RI_d
021900140422     c                   write     STO4FI_d
022000140422     c                   endif
022100000000     C*
022200140423     C                   CLEAR                   fnlv55ds
022300140423     C                   MOVEL     'C'           D55TLA
022400140423     C                   CALL      'FNLV55R'
022500140423     C                   PARM                    fnlv55ds
022600140423     c
022700000000     C                   SETON                                        LR
022800140408     C*------------------------------------------------------------------------*
022900140408     C* Ciclo di lettura su TITAS
023000140408     C*------------------------------------------------------------------------*
023100140408     c     LET_TAS       BEGSR
023200140409     C                   MOVEL     '0'           FINL00            1
023300140423     c* carico fil gestore
023400140423     c                   clear                   fnlv55ds
023500140423     C                   EVAL      D55TPT='6'
023600140423     c                   eval      d55lin=kfil
023700140423     c                   if        pardsa>0
023800140423     c                   eval      d55drf=pardsa
023900140423     c                   else
024000140423     c                   eval      d55drf=pardsd
024100140423     c                   endif
024200140423     c
024300140423     c                   call      'FNLV55R'
024400140423     c                   parm                    fnlv55ds
024500140512     c                   exsr      setfil
024600140408     c
024700140408     c                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900000000     C* SETFIL - POSIZIONAMENTO SUL FILE
025000000000     C*------------------------------------------------------------------------*
025100000000     C     SETFIL        BEGSR
025200140512     C/EXEC SQL
025300140512     C+ Declare A2 Cursor for
025400140513     c+ SELECT * FROM GAITRAGRU/TITAS00F
025500140512     c+ WHERE taslna= :kfil and tasdcm between :pardsd and :pardsa
025600140512     c+ ORDER BY tasnzd, tascad, taslod
025700140512     C/END-EXEC
025800140512      *          apertura cursore
025900140512     C/EXEC SQL
026000140512     C+ OPEN A2
026100140512     C/END-EXEC
026200140512     C                   DOU       SqlCod <> 0
026300140512     C/EXEC SQL
026400140512     C+ Fetch Next From A2 Into :titasds
026500140512     C/END-EXEC
026600140512     c                   select
026700140512     C                   WHEN      SqlCod = 100
026800140512     c                   leave
026900140512      *
027000140512     C                   WHEN      SqlCod < 0
027100140512     c                   seton                                        H1
027200140512     c                   leave
027300140512      *
027400140512     C                   OTHER
027500140512     c                   exsr      chkrec
027600140512     c                   if        wrecok='S'
027700140512     c                   exsr      crerec
027800140512     c                   endif
027900140512     c
028000140512     C                   ENDSL
028100140512
028200140512     c                   enddo
028300140512     C/EXEC SQL
028400140512     C+ Close A2
028500140512     C/END-EXEC
028600000000     C*
028700140512     C/EXEC SQL
028800140512     C+ Declare A3 Cursor for
028900140513     c+ SELECT * FROM GAITRAGRU/TITAS10F
029000140512     c+ WHERE taslna= :kfil and tasdcm between :pardsd and :pardsa
029100140512     c+ ORDER BY  tasnzd, tascad, taslod
029200140512     C/END-EXEC
029300140512      *          apertura cursore
029400140512     C/EXEC SQL
029500140512     C+ OPEN A3
029600140512     C/END-EXEC
029700140512     C                   DOU       SqlCod <> 0
029800140512     C/EXEC SQL
029900140512     C+ Fetch Next From A3 Into :titasds
030000140512     C/END-EXEC
030100140512     c                   select
030200140512     C                   WHEN      SqlCod = 100
030300140512     c                   leave
030400140512      *
030500140512     C                   WHEN      SqlCod < 0
030600140512     c                   seton                                        H1
030700140512     c                   leave
030800140512      *
030900140512     C                   OTHER
031000140512     c                   exsr      chkrec
031100140512     c                   if        wrecok='S'
031200140512     c                   exsr      crerec
031300140512     c                   endif
031400140512     c
031500140512     C                   ENDSL
031600140512
031700140512     c                   enddo
031800140512     C/EXEC SQL
031900140512     C+ Close A3
032000140512     C/END-EXEC
032100000000     C                   ENDSR
032200000000     C*------------------------------------------------------------------------*
032300000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
032400000000     C*------------------------------------------------------------------------*
032500000000     C     CHKREC        BEGSR
032600000000     C*
032700000000     C                   MOVEL     'S'           WRECOK            1
032800040305     C*
032900140414     c*
033000051109     C* ESCLUDE LE BOLLE di recupero
033100051109     C                   Z-ADD     1             i
033200000000     C                   SETOFF                                           28
033300051109     C     TASTBL        LOOKUP    TB(i)                                  28
033400051109     C                   IF        *in28 = *on
033500000000     C                   MOVEL     'N'           WRECOK
033600000000     C                   ENDIF
033700140404     c
033800140414     c* Escludo bolle export e le bolle con consegna anomala
033900140414     c                   if        tasnzd<>*blanks or tascca<>' '
034000140408     C                   MOVEL     'N'           WRECOK
034100140408     c                   endif
034200140414     c
034300140411     c* Escludo le bolle in fermo deposito
034400140411     c                   if        tasffd='S'
034500140411     C                   MOVEL     'N'           WRECOK
034600140411     c                   endif
034700011112     C*
034800000000     C                   ENDSR
034900140408     C*------------------------------------------------------------------------*
035000140408     C* controllo se si tratta di bolla da elaborare
035100140408     C*------------------------------------------------------------------------*
035200140408     C     NOELAB        BEGSR
035300140408     C*
035400140408     C                   MOVEL     'S'           Welab             1
035500140408     C*
035600140408     c* Cerco se si tratta di distinta PDA
035700140508     c* solo in AS888
035800140508    1c                   if        %subst(knsif:7:1)='P'
035900140508     c                   movel     taspdc        kfgs              3 0
036000140508     c                   movel     4             knpg              1 0
036100140508     c                   z-add     tasndc        knfv
036200140508     c     kdst          chain     fidst01l
036300140508    2c                   if        %found(fidst01l)
036400140508     c                             and (dstpda='C' or dstpda='E')
036500140508   x2c                   else
036600140508     C                   MOVEL     'N'           Welab
036700140508    2c                   endif
036800140508     c
036900140508   x1c                   else
037000140508   c
037100140508     c* Solo dall'aprile 2014 abbiamo le bolle con il flag impostato
037200140508     c                   if        tasaas<2014
037300140508     C                   MOVEL     'N'           Welab
037400140508     c                   endif
037500140508     c
037600140508     c                   if        tasaas=2014 and tasmgs<0401
037700140508     C                   MOVEL     'N'           Welab
037800140508     c                   endif
037900140408     c* chaino record GEN
038000140408     c     kar5          chain     fiar531c
038100140408     c                   if        %found(fiar531c)
038200140408     c                   eval      dar5gen=ar5uni
038300140408     c                   if        §AR5PDACO<>'  '
038400140408     C                   MOVEL     'N'           Welab
038500140408     c                   endif
038600140408     c                   endif
038700140508     c
038800140508    1C                   ENDIF
038900140408     c
039000140411     c* Non elaboro se impostata ora consegna richiesta
039100140411     c                   if        tashcr>0
039200140408     C                   MOVEL     'N'           Welab
039300140408     c                   endif
039400140408     c
039500140408     C                   ENDSR
039600000000     C*------------------------------------------------------------------------*
039700140408     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO
039800000000     C*------------------------------------------------------------------------*
039900140408     C     CRErec        BEGSR
040000140404     c* recupero record azcos normalizzando prima la località
040100140512     c
040200140512     c                   if        tascad<>savcad or taslod<>savlod
040300140512     c                   exsr      normloc
040400140404     c                   clear                   kloc
040500140404     c
040600140404     c                   if        wnorm='S'
040700140512     c                   eval      kloc=o95loc
040800140404     c                   endif
040900140512     c
041000140512     c                   eval      savcad=tascad
041100140512     c                   eval      savlod=taslod
041200140512     c                   endif
041300140404     c
041400140404     c* Richiamo pgm reperimento orari servizi
041500140404     c                   clear                   trulorsds
041600140404     c                   clear                   trulor2ds
041700140404     c                   eval      ioredta=pardsd
041800140408     c                   eval      iorefil=taslna
041900140404     c                   eval      iorenar=tasnzd
042000140404     c                   eval      iorecap=tascad
042100140407     c                   eval      ioreloc=o95loc
042200140414     c                   eval      ioreddc=tasddc
042300140414     c                   eval      iorendc=tasndc
042400140414     c                   eval      ioretfp=tastfp
042500140414     c                   eval      ioretfa=tastfa
042600140414     c                   eval      ioredti=tasdti
042700140428     c                   eval      iorehti=9999
042800140404     c                   eval      ioretser=partser
042900140512     c                   eval      ioretsp =tastsp
043000140404     c                   call      'TRULORSR'
043100140404     c                   parm                    kpjba
043200140404     c                   parm                    trulorsds
043300140404     c                   parm                    trulor2ds
043400140404     c*
043500140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
043600140415     c                   if        oor2fcsgl='P'
043700140415     c                   eval      kserv='P '
043800140415     c                   else
043900140415     c                   clear                   kserv
044000140415     c                   endif
044100140415
044200140415     c     kccos         chain     wfccos1l
044300140404     c                   if        not %found(wfccos1l)
044400140404     c                   clear                   wfccos00
044500140404     c                   eval      wcosdcmd=pardsd
044600140404     c                   eval      wcosdcma=pardsa
044700140404     c                   eval      wcosver =p96ver
044800140411     c                   eval      wcoslna =d55tfa
044900140404     c                   eval      wcosnaz =tasnzd
045000140404     c                   eval      wcoscap =tascad
045100140404     c                   eval      wcosloc =kloc
045200140404     c                   eval      wcostser=partser
045300140404     c                   eval      wcosorif=OOR2ORIF
045400140415     c                   eval      wcosserv=kserv
045500140424     c
045600140424     c* obsoleto
045700140424     c     kcpc          chain     azcpc01l
045800140424     c                   if        %found(azcpc01l)
045900140424     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
046000140424     c                   endif
046100140424     c                   if        wcosloc<>*blanks
046200140424     c     kcpl          chain     azcpl01l
046300140424     c                   if        %found(azcpl01l)
046400140424     c                   eval      wcosloco=%subst(cplfi1:1:1)
046500140424     c                   endif
046600140424     c                   endif
046700140424     c* Imposto gli orari nel file
046800140424     c                   eval      WCOSOSTIS=OOR2STIS
046900140424     c                   eval      WCOSOSTfS=OOR2STfS
047000140424     c                   eval      WCOSOmiis=OOR2miis
047100140424     c                   eval      WCOSOmxfs=OOR2mxfs
047200140424     c
047300140415     c* imposto orari dettaglio se ci sono e richiestoc
047400140415     c                   if        pardet='S'
047500140422     c                   eval      wcosor1=or_dal(1)
047600140422     c                   eval      wcosor2=or_dal(2)
047700140422     c                   eval      wcosor3=or_dal(3)
047800140422     c                   eval      wcosor4=or_dal(4)
047900140422     c                   eval      wcosor5=or_dal(5)
048000140422     c                   eval      wcosor6=or_dal(6)
048100140422     c                   eval      wcosor7=or_dal(7)
048200140422     c                   eval      wcosor8=or_dal(8)
048300140422     c                   eval      wcosor9=or_dal(9)
048400140422     c                   eval      wcosorOl=paroltre
048500140404     c                   endif
048600140415     c                   endif
048700140404     c
048800140404     c                   add       1             wcosts
048900140404     c* Se loc non normalizzata
049000140404     c                   if        wnorm='N'
049100140404     c                   add       1             wcostsNN
049200140404     c                   endif
049300140408     c
049400140408     c* Controllo se spedizione da elaborare
049500140408     c                   exsr      Noelab
049600140408     c
049700140404     c* elaborate
049800140408     c                   if        welab='S'
049900140404     c                   add       1             wcostse
050000140404     c
050100140404     c* consegna negli orari std
050200140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
050300140404     c                   add       1             wcostsos
050400140404     c                   endif
050500140404     c* consegna negli orari min/max
050600140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
050700140404     c                   add       1             wcostsom
050800140404     c                   endif
050900140415     c* imposto campi dettaglio
051000140415     c                   exsr      ImpoDET
051100140408     c                   endif
051200140404     c
051300140404     c                   if        not %found(wfccos1l)
051400140404     c                   write     wfccos00
051500140404     c                   else
051600140404     c                   update    wfccos00
051700140404     c                   endif
051800000000     C*
051900000000     C                   ENDSR
052000140404     C*------------------------------------------------------------------------*
052100140404     C* NORMLOC - Normalizza le località mediante il TISI95
052200140404     C*------------------------------------------------------------------------*
052300140404     C     NORMLOC       BEGSR
052400140404     c                   eval      wnorm='N'
052500140404     c
052600140404     C* Determino il network della bolla corrente
052700140404     C                   z-add     1             i
052800140404     C     tasLNA        lookup    fil(i)                                 55
052900140404     C                   if        %found
053000140404     C                   movel     ntw(i)        wNetwork
053100140404     C                   else
053200140404     C                   movel     *all'X'       wNetwork
053300140404     C                   endif
053400140404     C*
053500140404     C                   clear                   tisi95ds                       *azzera i parametri
053600140404     C                   movel     *blanks       i95tla                         *tipo lancio
053700140404     C                   movel     '7'           i95tcn                         *tipo controllo
053800140404     C                   movel     tasTSP        i95tsp                         *tipo servizio
053900140404     C                   movel     tasNZD        i95nar                         *nazione dest
054000140404     C                   movel     tasCAD        i95cap                         *cap dest
054100140404     C                   movel     tasLOD        i95loc                         *località dest
054200140404     C                   movel     tasPRD        i95prv                         *provincia dest
054300140404     C                   z-add     tasPKF        i95lkg                         * peso da fatturare
054400140404     C                   z-add     tasVLF        i95lmc                         * volume da fattur
054500140404     C                   movel     tasFFD        i95ffd                         * fermo deposito
054600140404     C                   movel     tasFTC        i95fi2                         * 1° cons. partic.
054700140404     C                   move      tasTC2        i95fi2                         * 2° cons. partic.
054800140404     C*
054900140404     C                   eval      i95dat = tasAAS*10000+tasMGS                 *data di riferimento
055000140404     C*
055100140404     C* Se la LNA è filiale FEDEX imposto flag opportuno
055200140404     C                   if        wNetwork = 'FED'
055300140404     C                   eval      i95fi1 = 'S'
055400140404     C                   endif
055500140404     C*
055600140404     C* Se la LNA è filiale DPD imposto flag opportuno
055700140404     C                   if        wNetwork = 'DPD'
055800140404     C                   eval      i95fi1 = 'D'
055900140404     C                   endif
056000140404     C*
056100140404     C                   call      'TISI95R'
056200140404     C                   parm                    tisi95ds
056300140404     C*
056400140404     C* Se nn reperito instradamento considero i dati originali della bolla
056500140404     C                   if        o95lid = '3' or o95lid = '4'
056600140404     c                   eval      wnorm='S'
056700140404     C                   else
056800140404     C                   movel     tasLOD        o95loc
056900140404     C                   endif
057000140404     C*
057100000000     C                   ENDSR
057200140404     C*------------------------------------------------------------------------*
057300140404     C* Stampa - statistica
057400140404     C*------------------------------------------------------------------------*
057500140404     c     STAMPA        BEGSR
057600140404     c* Tpo servizio
057700140404     c                   if        partser='C'
057800140404     c                   eval      p1dtser='CONSEGNE'
057900140404     C                   ELSE
058000140408     c                   eval      p1dtser=' RITIRI '
058100140404     C                   ENDIF
058200140408     c* giro le date
058300140409     C                   MOVE      pardsd        G02inv
058400140408     C                   MOVEL     '3'           G02ERR
058500140408     C                   CALL      'XSRDA8'
058600140408     C                   PARM                    WLBDAT
058700140408     c                   z-add     g02dat        p1ddsd
058800140408     c*
058900140408     c                   if        pardsa>0
059000140409     C                   MOVE      pardsa        G02inv
059100140408     C                   MOVEL     '3'           G02ERR
059200140408     C                   CALL      'XSRDA8'
059300140408     C                   PARM                    WLBDAT
059400140408     c                   z-add     g02dat        p1ddsa
059500140408     c                   endif
059600140408     c* Orari per calcolo %
059700140408     c                   if        parorari='S'
059800140408     c                   eval      p1dorari='STANDARD'
059900140424     c                   eval      p1dtor_d='Standard'
060000140408     c                   else
060100140408     c                   eval      p1dorari='MINIMO/MASSIMO'
060200140424     c                   eval      p1dtor_d=' Min/Max'
060300140408     c                   endif
060400140408     c
060500140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
060600140408     c                   if        parcor='C'
060700140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
060800140411     c                   eval      p1dbolstd='   Nei limiti STD   '
060900140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
061000140408     C                   ELSE
061100140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
061200140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
061300140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
061400140408     C                   ENDIF
061500140423     c
061600140423     c                   write     STO4TE
061700140423
061800140423     c* Orari nel dettaglio
061900140423     c                   if        pardet='S'
062000140423     c                   exsr      TestaDET
062100140423     c                   write     STO4TE_d
062200140423     c                   endif
062300140404     c
062400140404     C
062500140424     c*
062600140404     c     *loval        setll     wfccos1l
062700140404     c                   read      wfccos1l
062800140404     c                   dow       not %eof(wfccos1l)
062900140404     c
063000140408     c* a cambio filiale stampo a pagina nuova
063100140408     c                   if        wcoslna<>savlna
063200140408
063300140408     c                   if        savlna>0
063400140423     c                   exsr      Totfil
063500140423     c
063600140408     c                   write     STO4TE
063700140422     c
063800140422     c                   if        pardet='S'
063900140422     c                   write     STO4RI_d
064000140422     c                   write     STO4TE_d
064100140408     c                   endif
064200140423     c
064300140423     c                   clear                   tot_ts
064400140423     c                   clear                   tot_tse
064500140423     c                   clear                   tot_tsos
064600140423     c                   clear                   tot_tsom
064700140422     c                   endif
064800140408
064900140408     c                   z-add     1             i
065000140408     C     wcoslna       lookup    fil(i)                                 55
065100140404     C                   if        %found
065200140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
065300140408     c                   eval      p1dare=dare(i)
065400140404     C                   else
065500140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
065600140408     c                   eval      p1dare=*all'?'
065700140404     C                   endif
065800140408
065900140408     c                   eval      savlna=wcoslna
066000140408
066100140408     c                   write     STO4TEF
066200140414     c                   write     STO4RI
066300140422     c                   if        pardet='S'
066400140422     c                   write     STO4TEF_d
066500140422     c                   write     STO4RI_d
066600140408     c                   endif
066700140422     c                   endif
066800140408
066900140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
067000140411     c                   if        wcostse>=parnum and wcostse>0
067100140408     c* voglio la % di correttezza
067200140408     c                   if        parcor='C'
067300140404     c* Calcolo % STD
067400140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
067500140408     c                   eval      p1dstsos=WCOSTSOs
067600140404     c* Calcolo % MIN/MAX
067700140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
067800140408     c                   eval      p1dstsom=WCOSTSOm
067900140408     c
068000140408     c                   else
068100140408     c* voglio la % delle fuori orario
068200140408     c                   eval      wfuori_os=wcostse-wcostsos
068300140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
068400140408     c                   eval      p1dstsos=Wfuori_os
068500140408
068600140408     c                   eval      wfuori_om=wcostse-wcostsom
068700140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
068800140408     c                   eval      p1dstsom=Wfuori_om
068900140408     c                   endif
069000140408     c
069100140407     c* stampo solo al di sotto della % richiesta
069200140408     c                   eval      wstampadet='S'
069300140408
069400140408     c                   select
069500140408     c                   when      parperc=0
069600140408
069700140408     c                   when      parorari='S'  and parcor='C' and
069800140408     c                             p1dperc_os>=parperc
069900140408     c                   eval      wstampadet='N'
070000140408     c
070100140408     c                   when      parorari='S'  and parcor='F' and
070200140408     c                             p1dperc_os< parperc
070300140408     c                   eval      wstampadet='N'
070400140408
070500140408     c                   when      parorari='M'  and parcor='C' and
070600140408     c                             p1dperc_om>=parperc
070700140408     c                   eval      wstampadet='N'
070800140408     c
070900140408     c                   when      parorari='M'  and parcor='F' and
071000140408     c                             p1dperc_om< parperc
071100140408     c                   eval      wstampadet='N'
071200140408     c                   endsl
071300140404     c*
071400140408     c                   if        wstampadet='S'
071500140424     c* POM
071600140424     c                   if        wcosserv='P '
071700140424     c                   eval      p1dpom='Pom'
071800140424     c                   else
071900140424     c                   eval      p1dpom='   '
072000140424     c                   endif
072100140411     c*
072200140411     c* Se la località è vuota imposto '??'
072300140411     c                   if        wcosloc=*blanks
072400140411     c                   eval      wcosloc='??'
072500140411     c                   endif
072600140408     c
072700140408     c                   if        *in06
072800140408     c                   write     sto4te
072900140408     c                   write     STO4TEF
073000140414     c                   write     STO4RI
073100140408     c                   setoff                                       06
073200140404     c                   endif
073300140408     c
073400140408     c* Richiamo pgm reperimento orari servizi
073500140424     c***                clear                   trulorsds
073600140424     c***                clear                   trulor2ds
073700140424     c***                eval      ioredta=pardsd
073800140424     c***                eval      iorefil=wcoslna
073900140424     c***                eval      iorenar=wcosnaz
074000140424     c***                eval      iorecap=wcoscap
074100140424     c***                eval      ioreloc=wcosloc
074200140424     c***                if        wcosloc=*blanks
074300140424     c***                eval      ioreloc='A '
074400140424     c***                endif
074500140424     c***                eval      ioretser=wcostser
074600140424     c***                eval      ioretsp ='C'
074700140424     c***                call      'TRULORSR'
074800140424     c***                parm                    kpjba
074900140424     c***                parm                    trulorsds
075000140424     c**+                parm                    trulor2ds
075100140408     c
075200140424     c                   eval      p1dstis=wcosoSTiS
075300140424     c                   eval      p1dstfs=wcosoSTFS
075400140424     c                   eval      p1dmiis=wcosomiis
075500140424     c                   eval      p1dmxfs=wcosomxfs
075600140404     c
075700140408     c                   write     STO4det
075800140422     c
075900140422     c* se devo stampare anche il dettaglio
076000140422     c                   if        pardet='S'
076100140422     c                   exsr      Stadet
076200140422     c                   endif
076300140408     c
076400140407     c                   endif
076500140408     c                   endif
076600140424     c* totali di filiale
076700140424     c                   add       WCOSTS        tot_ts
076800140424     c                   add       WCOSTSE       tot_tse
076900140424     c                   add       WCOSTSos      tot_tsos
077000140424     c                   add       WCOSTSom      tot_tsom
077100140407     c
077200140404     c                   read      wfccos1l
077300140404     c
077400140404     c                   enddo
077500140423     c
077600140423     c* totali finali
077700140423     c                   if        savlna>0
077800140423     c                   exsr      totfil
077900140423     c                   endif
078000140404     c
078100140404     c                   ENDSR
078200140423     C*------------------------------------------------------------------------*
078300140423     c* Imposta gli orari in testata dettaglioù
078400140423     C*------------------------------------------------------------------------*
078500140423     c     TestaDet      BEGSR
078600140423     c                   eval      p1dsot = paror1
078700140423     c                   eval      p1doltre= paroltre
078800140423     c                   eval      p1dda1  = or_dal(1)
078900140423
079000140423     c                   if        or_dal(2)>0
079100140423     c                   eval      p1dda2  = or_dal(2)
079200140423     c                   movel     or_dal(2)     w0060
079300140423     c                   move      w0060         timeiso
079400140423     c                   subdur    1:*mn         timeiso
079500140423     c                   eval      w0060     =%dec(timeiso)
079600140423     c                   movel     w0060         p1dal1
079700140423     c                   else
079800140423     c                   eval      p1dal1=paroltre
079900140423     c                   leavesr
080000140423     c                   endif
080100140423     c
080200140423     c                   if        or_dal(3)>0
080300140423     c                   eval      p1dda3  = or_dal(3)
080400140423     c                   movel     or_dal(3)     w0060
080500140423     c                   move      w0060         timeiso
080600140423     c                   subdur    1:*mn         timeiso
080700140423     c                   eval      w0060     =%dec(timeiso)
080800140423     c                   movel     w0060         p1dal2
080900140423     c                   else
081000140423     c                   eval      p1dal2=paroltre
081100140423     c                   leavesr
081200140423     c                   endif
081300140423
081400140423     c                   if        or_dal(4)>0
081500140423     c                   eval      p1dda4  = or_dal(4)
081600140423     c                   movel     or_dal(4)     w0060
081700140423     c                   move      w0060         timeiso
081800140423     c                   subdur    1:*mn         timeiso
081900140423     c                   eval      w0060     =%dec(timeiso)
082000140423     c                   movel     w0060         p1dal3
082100140423     c                   else
082200140423     c                   eval      p1dal3=paroltre
082300140423     c                   leavesr
082400140423     c                   endif
082500140423
082600140423     c                   if        or_dal(5)>0
082700140423     c                   eval      p1dda5  = or_dal(5)
082800140423     c                   movel     or_dal(5)     w0060
082900140423     c                   move      w0060         timeiso
083000140423     c                   subdur    1:*mn         timeiso
083100140423     c                   eval      w0060     =%dec(timeiso)
083200140423     c                   movel     w0060         p1dal4
083300140423     c                   else
083400140423     c                   eval      p1dal4=paroltre
083500140423     c                   leavesr
083600140423     c                   endif
083700140423
083800140423     c                   if        or_dal(6)>0
083900140423     c                   eval      p1dda6  = or_dal(6)
084000140423     c                   movel     or_dal(6)     w0060
084100140423     c                   move      w0060         timeiso
084200140423     c                   subdur    1:*mn         timeiso
084300140423     c                   eval      w0060     =%dec(timeiso)
084400140423     c                   movel     w0060         p1dal5
084500140423     c                   else
084600140423     c                   eval      p1dal5=paroltre
084700140423     c                   leavesr
084800140423     c                   endif
084900140423
085000140423     c                   if        or_dal(7)>0
085100140423     c                   eval      p1dda7  = or_dal(7)
085200140423     c                   movel     or_dal(7)     w0060
085300140423     c                   move      w0060         timeiso
085400140423     c                   subdur    1:*mn         timeiso
085500140423     c                   eval      w0060     =%dec(timeiso)
085600140423     c                   movel     w0060         p1dal6
085700140423     c                   else
085800140423     c                   eval      p1dal6=paroltre
085900140423     c                   leavesr
086000140423     c                   endif
086100140423
086200140423     c                   if        or_dal(8)>0
086300140423     c                   eval      p1dda8  = or_dal(8)
086400140423     c                   movel     or_dal(8)     w0060
086500140423     c                   move      w0060         timeiso
086600140423     c                   subdur    1:*mn         timeiso
086700140423     c                   eval      w0060     =%dec(timeiso)
086800140423     c                   movel     w0060         p1dal7
086900140423     c                   else
087000140423     c                   eval      p1dal7=paroltre
087100140423     c                   leavesr
087200140423     c                   endif
087300140423     c                   if        or_dal(9)>0
087400140423     c                   eval      p1dda9  = or_dal(9)
087500140423     c                   movel     or_dal(9)     w0060
087600140423     c                   move      w0060         timeiso
087700140423     c                   subdur    1:*mn         timeiso
087800140423     c                   eval      w0060     =%dec(timeiso)
087900140423     c                   movel     w0060         p1dal8
088000140423     c
088100140423     c                   eval      p1dal9=paroltre
088200140423     c                   else
088300140423     c                   eval      p1dal8=paroltre
088400140423     c                   leavesr
088500140423     c                   endif
088600140423     c
088700140423     c                   ENDSR
088800140422     C*------------------------------------------------------------------------*
088900140422     C* Stampa dettaglio
089000140422     C*------------------------------------------------------------------------*
089100140422     C     StaDET        BEGSR
089200140422     c                   if        *in07
089300140422     c                   write     sto4te_d
089400140422     c                   write     STO4TEF_d
089500140422     c                   write     STO4RI_d
089600140422     c                   setoff                                       07
089700140422     c                   endif
089800140424     c* Imposto gli orari
089900140424     c                   if        parorari='S'
090000140424     c                   eval      p1dor_dal=WCOSOSTIS
090100140424     c                   eval      p1dor_al =WCOSOSTfS
090200140424     c                   else
090300140424     c                   eval      p1dor_dal=WCOSOmiis
090400140424     c                   eval      p1dor_al =WCOSOmxfS
090500140424     c                   endif
090600140424     c                   setoff                                       111213
090700140424     c                   setoff                                       141516
090800140424     c                   setoff                                       171819
090900140505     c                   setoff                                       2021
091000140422     c* calcolo le %
091100140422     c* SOTTO
091200140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
091300140422     C                   IF        WPERC>0
091400140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
091500140505     c                   seton                                        20
091600140422     c                   else
091700140424     c                   eval      p1cp_sot='    '
091800140422     c                   endif
091900140422     c* OLTRE
092000140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
092100140422     C                   IF        WPERC>0
092200140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
092300140505     c                   seton                                        21
092400140422     c                   else
092500140424     c                   eval      p1cp_olt='    '
092600140422     c                   endif
092700140422     c* % 1
092800140505    1c                   if        wcosor1  >0
092900140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
093000140422     c
093100140505    2C                   IF        WPERC>0
093200140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
093300140505     c
093400140505    2c                   select
093500140505     c                   when      or_dal(1)<p1dor_dal
093600140505     c                   seton                                        11
093700140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
093800140505     c                   seton                                        11
093900140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
094000140505     c                   seton                                        11
094100140505    2c                   endsl
094200140505
094300140422     c                   else
094400140424     c                   eval      p1cp_o1 ='    '
094500140505    2c                   endif
094600140424     c
094700140505    1c                   endif
094800140422     c
094900140422     c* % 2
095000140422     c                   if        wcosor2  >0
095100140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
095200140422     c
095300140422     C                   IF        WPERC>0
095400140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
095500140505
095600140505     c                   select
095700140505     c                   when      or_dal(2)<p1dor_dal
095800140505     c                   seton                                        12
095900140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
096000140505     c                   seton                                        12
096100140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
096200140505     c                   seton                                        12
096300140505     c                   endsl
096400140505
096500140422     c                   else
096600140424     c                   eval      p1cp_o2 ='    '
096700140422     c                   endif
096800140422     c                   endif
096900140423     c* % 3
097000140423     c                   if        wcosor3  >0
097100140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
097200140423     c
097300140423     C                   IF        WPERC>0
097400140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
097500140505
097600140505     c                   select
097700140505     c                   when      or_dal(3)<p1dor_dal
097800140505     c                   seton                                        13
097900140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
098000140505     c                   seton                                        13
098100140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
098200140505     c                   seton                                        13
098300140505     c                   endsl
098400140423     c                   else
098500140424     c                   eval      p1cp_o3 ='    '
098600140423     c                   endif
098700140423     c                   endif
098800140423     c* % 4
098900140423     c                   if        wcosor4  >0
099000140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
099100140423     c
099200140423     C                   IF        WPERC>0
099300140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
099400140505
099500140505     c                   select
099600140505     c                   when      or_dal(4)<p1dor_dal
099700140505     c                   seton                                        14
099800140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
099900140505     c                   seton                                        14
100000140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
100100140505     c                   seton                                        14
100200140505     c                   endsl
100300140423     c                   else
100400140424     c                   eval      p1cp_o4 ='    '
100500140423     c                   endif
100600140423     c                   endif
100700140423     c* % 5
100800140423     c                   if        wcosor5  >0
100900140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
101000140423     c
101100140423     C                   IF        WPERC>0
101200140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
101300140505
101400140505     c                   select
101500140505     c                   when      or_dal(5)<p1dor_dal
101600140505     c                   seton                                        15
101700140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
101800140505     c                   seton                                        15
101900140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
102000140505     c                   seton                                        15
102100140505     c                   endsl
102200140423     c                   else
102300140424     c                   eval      p1cp_o5 ='    '
102400140423     c                   endif
102500140423     c                   endif
102600140423     c* % 6
102700140423     c                   if        wcosor6  >0
102800140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
102900140423     c
103000140423     C                   IF        WPERC>0
103100140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
103200140505
103300140505     c                   select
103400140505     c                   when      or_dal(6)<p1dor_dal
103500140505     c                   seton                                        16
103600140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
103700140505     c                   seton                                        16
103800140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
103900140505     c                   seton                                        16
104000140505     c                   endsl
104100140423     c                   else
104200140424     c                   eval      p1cp_o6 ='    '
104300140423     c                   endif
104400140423     c                   endif
104500140423     c* % 7
104600140423     c                   if        wcosor7  >0
104700140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
104800140423     c
104900140423     C                   IF        WPERC>0
105000140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
105100140505
105200140505     c                   select
105300140505     c                   when      or_dal(7)<p1dor_dal
105400140505     c                   seton                                        17
105500140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
105600140505     c                   seton                                        17
105700140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
105800140505     c                   seton                                        17
105900140505     c                   endsl
106000140423     c                   else
106100140424     c                   eval      p1cp_o7 ='    '
106200140423     c                   endif
106300140423     c                   endif
106400140423     c* % 8
106500140423     c                   if        wcosor8  >0
106600140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
106700140423     c
106800140423     C                   IF        WPERC>0
106900140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
107000140505
107100140505     c                   select
107200140505     c                   when      or_dal(8)<p1dor_dal
107300140505     c                   seton                                        18
107400140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
107500140505     c                   seton                                        18
107600140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
107700140505     c                   seton                                        18
107800140505     c                   endsl
107900140423     c                   else
108000140424     c                   eval      p1cp_o8 ='    '
108100140423     c                   endif
108200140423     c                   endif
108300140423     c* % 9
108400140423     c                   if        wcosor9  >0
108500140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
108600140423     c
108700140423     C                   IF        WPERC>0
108800140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
108900140505
109000140505     c                   select
109100140505     c                   when      or_dal(9)<p1dor_dal
109200140505     c                   seton                                        19
109300140505     c                   when      paroltre >p1dor_al
109400140505     c                   seton                                        19
109500140505     c                   endsl
109600140423     c                   else
109700140424     c                   eval      p1cp_o9 ='    '
109800140423     c                   endif
109900140423     c                   endif
110000140422     c
110100140422     c                   write     STO4det_d
110200140422     c                   ENDSR
110300140423     C*------------------------------------------------------------------------*
110400140423     c* Stampa totali di filiale
110500140423     C*------------------------------------------------------------------------*
110600140423     c     TOTfil        BEGSR
110700140423     c* voglio la % di correttezza
110800140423     c                   if        parcor='C'
110900140423     c* Calcolo % STD
111000140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
111100140423     c                   eval      p1dstsos=tot_TSOs
111200140423     c* Calcolo % MIN/MAX
111300140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
111400140423     c                   eval      p1dstsom=tot_TSOm
111500140423     c
111600140423     c                   else
111700140423     c* voglio la % delle fuori orario
111800140423     c                   eval      wfuori_os=tot_tse-tot_tsos
111900140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
112000140423     c                   eval      p1dstsos=Wfuori_os
112100140423
112200140423     c                   eval      wfuori_om=tot_tse-tot_tsom
112300140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
112400140423     c                   eval      p1dstsom=Wfuori_om
112500140423     c
112600140423     c                   endif
112700140423
112800140423     c                   if        *in06
112900140423     c                   write     sto4te
113000140423     c                   write     STO4TEF
113100140423     c                   write     STO4RI
113200140423     c                   setoff                                       06
113300140423     c                   else
113400140423     c                   write     STO4RI
113500140423     c                   write     STO4TOT
113600140423     c  n06              write     STO4RI
113700140423     c                   endif
113800140423     c
113900140423     c                   ENDSR
114000000000     C*------------------------------------------------------------------------*
114100000000     C* DIVARE - RECUPERA AREA E DIVISIONE
114200000000     C*------------------------------------------------------------------------*
114300000000     C     DIVARE        BEGSR
114400000000     C*
114500000000     C                   Z-ADD     1             I
114600051109     C     taslnp        LOOKUP    FIL(I)                                 99
114700000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
114800000000     C                   MOVEL     '*'           WDIV              1
114900000000     C                   Z-ADD     999           WARE              3 0
115000000000     C                   ELSE                                                   <-- 3 -->
115100000000     C                   MOVEL     DIV(I)        WDIV
115200000000     C                   Z-ADD     ARE(I)        WARE
115300000000     C                   END                                                    <-- 3 ---
115400000000     C*
115500000000     C                   ENDSR
115600000000     C*------------------------------------------------------------------------*
115700000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
115800000000     C*------------------------------------------------------------------------*
115900000000     C     CARORG        BEGSR
116000000000     C*
116100140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
116200000000     C  N99              READ      AZORG01L                               99
116300000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
116400000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
116500000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
116600000000     C     ORGFAG        IFNE      'V'
116700051109     C                   ADD       1             I
116800000000     C                   Z-ADD     ORGFIL        FIL(I)
116900000000     C                   Z-ADD     ORGCAR        ARE(I)
117000000000     C                   MOVEL     ORGFL3        DIV(I)
117100140404     C* CALCOLA NETWORK
117200140404     C                   MOVEL     ORGDE3        OG143
117300140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
117400051109     c* riempio schiere decodifiche p.o., distretto e area
117500051109     c* distretto
117600140408     C***                Z-ADD     CODUT         KTBUTE
117700140408     C***                MOVEL     '17'          Ktbcod
117800140408     C***                MOVEL(P)  ORGFL3        KKEY
117900140408     C***  KTAB          CHAIN     TABEL00F                           30
118000140408    2C***  *IN30         IFEQ      *OFF
118100140408     C***  TBLFLG        ANDEQ     *BLANKS
118200140408     C***                MOVEL     TBLUNI        DS17
118300140408     C***                MOVEL     §17DES        ddiv(I)
118400140408     C***                ELSE
118500140408     C***                MOVE      *BLANKS       ddiv(I)
118600140409    2C***                END
118700051109     c* area
118800051109     C                   Z-ADD     CODUT         KTBUTE
118900051109     C                   MOVEL     '05'          Ktbcod
119000051109     C                   MOVEL(P)  ORGCAR        KKEY
119100051109     C     KTAB          CHAIN     TABEL00F                           30
119200051109    2C     *IN30         IFEQ      *OFF
119300051109     C     TBLFLG        ANDEQ     *BLANKS
119400051109     C                   MOVEL     TBLUNI        DS05
119500140408     C***                MOVEL     §05DES        dare(I)
119600140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
119700051109     C                   ELSE
119800140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
119900140408     C***                MOVE      *BLANKS       dare(I)
120000051109    2C                   END
120100051109     c* p.o.
120200051109     C                   MOVEl     orgdes        dfil(I)
120300051109     c
120400000000     C                   END
120500000000     C                   END                                                    <-- 2 ---
120600000000     C                   READ      AZORG01L                               99
120700000000     C                   END                                                    <-- 1 ---
120800000000     C*
120900000000     C                   ENDSR
121000000000     C*------------------------------------------------------------------------*
121100051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
121200000000     C*------------------------------------------------------------------------*
121300000000     C     CARTBL        BEGSR
121400000000     C*--------------------
121500051109     C* TIPI BOLLA di recupero
121600000000     C*--------------------
121700000000     C                   Z-ADD     0             I
121800000000     C                   Z-ADD     CODUT         KTBUTE
121900000000     C                   MOVEL     'TB'          KTBCOD
122000000000     C     KEYTAB        SETLL     TABEL00F
122100051109     C                   READe     TABEL00F                               99
122200000000     C     *IN99         DOWEQ     *OFF
122300000000     C     TBLFLG        IFEQ      *BLANKS
122400051109     C                   MOVEL     TBLUNI        DSTB
122500051109     c                   if        §tbrbl = 'R'
122600051109     C                   ADD       1             I
122700051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
122800051109     c                   endif
122900000000     C                   END
123000051109     C     keytab        READe     TABEL00F                               99
123100000000     C                   END
123200000000     C*
123300140428     c* recupero versione cappario alla data DAL
123400140428     C                   Z-ADD     pardsd        P96DRI            8 0
123500140428     C                   Z-ADD     *ZEROS        P96VER            5 0
123600140428     C                   CALL      'TISI96R'
123700140428     C                   PARM                    P96DRI
123800140428     C                   PARM                    P96VER
123900140428     c* carico gli orari in schiera
124000140428     c                   z-add     1             yy
124100140428     c                   if        paror1 >0
124200140428     c                   eval      or_dal(yy)=paror1
124300140428     c                   add       1             yy
124400140428     c                   endif
124500140428     c                   if        paror2 >0
124600140428     c                   eval      or_dal(yy)=paror2
124700140428     c                   add       1             yy
124800140428     c                   endif
124900140428     c                   if        paror3 >0
125000140428     c                   eval      or_dal(yy)=paror3
125100140428     c                   add       1             yy
125200140428     c                   endif
125300140428     c                   if        paror4 >0
125400140428     c                   eval      or_dal(yy)=paror4
125500140428     c                   add       1             yy
125600140428     c                   endif
125700140428     c                   if        paror5 >0
125800140428     c                   eval      or_dal(yy)=paror5
125900140428     c                   add       1             yy
126000140428     c                   endif
126100140428     c                   if        paror6 >0
126200140428     c                   eval      or_dal(yy)=paror6
126300140428     c                   add       1             yy
126400140428     c                   endif
126500140428     c                   if        paror7 >0
126600140428     c                   eval      or_dal(yy)=paror7
126700140428     c                   add       1             yy
126800140428     c                   endif
126900140428     c                   if        paror8 >0
127000140428     c                   eval      or_dal(yy)=paror8
127100140428     c                   add       1             yy
127200140428     c                   endif
127300140428     c                   if        paror9 >0
127400140428     c                   eval      or_dal(yy)=paror9
127500140428     c                   add       1             yy
127600140428     c                   endif
127700140428     C*
127800000000     C                   ENDSR
127900140415     C*------------------------------------------------------------------------*
128000140415     C* Imposto i campi di dettaglio in base ai range passati
128100140415     C*------------------------------------------------------------------------*
128200140415     C     ImpoDET       BEGSR
128300140415     c*
128400140422     c                   if        tashmc<or_dal(1)
128500140422     c                   add       1             wcosmint
128600140422     c                   leavesr
128700140422     c                   endif
128800140422     c                   if        tashmc> paroltre
128900140422     c                   add       1             wcosmaxt
129000140422     c                   leavesr
129100140422     c                   endif
129200140422
129300140422     c                   z-add     1             yy
129400140422     c                   dow       or_dal(yy)>0
129500140423     c                   if        tashmc>=or_dal(yy) and
129600140423     c
129700140423     c                             (or_dal(yy+1)>0 and
129800140423     c                              tashmc< or_dal(Yy+1)  or
129900140423     c                             (or_dal(yy+1)=0 and
130000140423     c                             tashmc<=paroltre))
130100140422     c                   select
130200140422     c                   when      yy=1
130300140415     c                   add       1             wcosor1t
130400140422     c                   when      yy=2
130500140422     c                   add       1             wcosor2t
130600140422     c                   when      yy=3
130700140422     c                   add       1             wcosor3t
130800140422     c                   when      yy=4
130900140422     c                   add       1             wcosor4t
131000140422     c                   when      yy=5
131100140422     c                   add       1             wcosor5t
131200140422     c                   when      yy=6
131300140422     c                   add       1             wcosor6t
131400140422     c                   when      yy=7
131500140422     c                   add       1             wcosor7t
131600140422     c                   when      yy=8
131700140422     c                   add       1             wcosor8t
131800140422     c                   when      yy=9
131900140422     c                   add       1             wcosor9t
132000140422     c                   endsl
132100140422     c
132200140415     c                   leavesr
132300140415     c                   endif
132400140422     c                   add       1             yy
132500140415     c
132600140422     c                   enddo
132700140415     c
132800140415     c                   ENDSR
132900000000     C*------------------------------------------------------------------------*
133000000000     C* *INZSR - ROUTINE INIZIALE
133100000000     C*------------------------------------------------------------------------*
133200000000     C     *INZSR        BEGSR
133300000000     C*--------------------
133400000000     C* RICEVIMENTO PARAMETRI
133500000000     C*--------------------
133600140408     c
133700140408     c     *dtaara       define    §azute        azuteds
133800140408     c     *dtaara       define    §datiute      ddatiute
133900140408     c                   in(E)     *dtaara
134000140408     c                   If        %error  or RSUT = *blanks
134100140408     c                   CLEAR                   tibs34ds
134200140408     c                   CALL      'TIBS34R'
134300140408     c                   PARM                    tibs34ds
134400140408     c                   in        *dtaara
134500140408     c                   EndIf
134600140408     c
134700140408     c                   eval      p1drsut=rsut
134800140408     C*
134900000000     C*--------------------
135000000000     C* CAMPI RIFERITI AL DATA BASE
135100000000     C*--------------------
135200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
135300000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
135400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
135500051109     C     *LIKE         DEFINE    TBLkey        Kkey
135600000000     C*--------------------
135700000000     C* CHIAVI DI LETTURA
135800000000     C*--------------------
135900140404     C* CHIAVE LETTURA TITAS26C - COMPLETA
136000140404     C     Ktas          KLIST
136100140404     C                   KFLD                    Kdcm
136200140404     C                   KFLD                    Kfil
136300000000     C*
136400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
136500000000     C     KEYTAB        KLIST
136600000000     C                   KFLD                    KTBUTE
136700000000     C                   KFLD                    KTBCOD
136800051109     C     KTAB          KLIST
136900051109     C                   KFLD                    ktbute
137000051109     C                   KFLD                    ktbcod
137100051109     C                   KFLD                    KKEY
137200000000     C*
137300060524     C     Kar5          KLIST
137400060524     C                   KFLD                    tasAAS
137500060524     C                   KFLD                    taslnp
137600060524     C                   KFLD                    tasnrs
137700060524     C                   KFLD                    tasnsp
137800060524     c                   kfld                    kar5trd
137900140404     C*
138000140404     C     Kdst          KLIST
138100140404     C                   KFLD                    Knpg
138200140404     C                   KFLD                    knFV
138300140404     C                   KFLD                    Kfgs
138400140404     c
138500140415     C     KCCOS         KLIST
138600140415     C                   KFLD                    partser
138700140423     C                   KFLD                    d55tfa
138800140404     C                   KFLD                    tasnzd
138900140404     C                   KFLD                    tascad
139000140404     C                   KFLD                    Kloc
139100140415     C                   KFLD                    kserv
139200140424     C     Kcpc          KLIST
139300140424     C                   KFLD                    p96ver
139400140424     C                   KFLD                    wcosnaz
139500140424     C                   KFLD                    wcoscap
139600140424     C     Kcpl          KLIST
139700140424     C                   KFLD                    p96ver
139800140424     C                   KFLD                    wcosnaz
139900140424     C                   KFLD                    wcoscap
140000140424     C                   KFLD                    wcosloc
140100000000     C*--------------------
140200000000     C                   Z-ADD     1             CODUT             1 0
140300000000     C*
140400000000     C***
140500000000     C* CARICA ORGANIGRAMMA
140600000000     C                   EXSR      CARORG
140700000000     C*
140800140404     C
140900140404     c* apertura file distinte di filiale
141000140508     C                   OPEN(e)   FIDST01L
141100140404     c
141200140508     c                   if        not %open(FIDST01L)
141300140508     c                   eval      %subst(WFDST:7:4)='PRD '
141400140508     c                   eval      %subst(TITAS:7:4)='GRPS'
141500140508     c                   eval      %subst(FIAR5:7:4)='GRPS'
141600140508     C                   OPEN      FIDST01L
141700140508     c                   endif
141800140409
141900140409     C                   OPEN(e)   TITAS26C
142000140409     C                   OPEN(e)   FIAR531C
142100140409     C
142200000000     C                   ENDSR
