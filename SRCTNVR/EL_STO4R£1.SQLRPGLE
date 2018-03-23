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
012300140409     d titas           s             21    inz('GAITRAGRU /TITAS26C')
012400140409     d fiar5           s             21    inz('GAITRAGRU /FIAR531C')
012500000000     C*------------------------------------------------------------------------*
012600000000     C* MAIN LINE
012700000000     C*------------------------------------------------------------------------*
012800140428     C     *ENTRY        PLIST
012900140428     C                   PARM                    KPJBA
013000140428     C                   MOVEL     KPJBU         PARAM
013100140409      /free
013200140409         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013300140409      /end-free
013400140428     C* CARICA TIPI BOLLA
013500140428     C                   EXSR      CARTBL
013600140408     c*
013700140408     c* se impostata l'area leggo le filiali dell'are altrimenti la singola filiale
013800140408     c*  altrimenti tutto
013900140408     c                   select
014000140408     c                   when      parfil>0
014100140408     c* carico la sua £6
014200140408     C                   CLEAR                   trul06ds
014300140408     C                   MOVE      '£6'          D06COD
014400140408     C                   movel     parfil        D06KEY
014500140408     c                   move      'S'           d06key
014600140409     c                   if        pardsa>0
014700140408     c                   movel     pardsa        d06drf
014800140409     c                   else
014900140409     c                   movel     pardsd        d06drf
015000140409     c                   endif
015100140408     C                   MOVEL     'L'           D06TLA
015200140408     C                   MOVEL     trul06ds      KPJBU
015300140408     C*
015400140408     C                   CALL      'TRUL06R'
015500140408     C                   PARM                    KPJBA
015600140408     C                   MOVEL     KPJBU         trul06ds
015700140408     C                   MOVEA     LIN           L6
015800140423     c                   z-add     1             XX
015900140408
016000140423     c                   dow       l6(XX)>0
016100140423     c                   eval      kfil=l6(XX)
016200140408     c                   exsr      Let_TAS
016300140423     c                   add       1             XX
016400140408     c                   enddo
016500140408     c* immesso cod area
016600140408     c                   when      parcar>0
016700140409     C/EXEC SQL
016800140409     C+ Declare A1 Cursor for
016900140409     c+ SELECT * FROM azorg00f
017000140409     c+ WHERE orgcar=  :parcar AND orgfva=' ' and (orgfag='F' or orgfag='A')
017100140409     c+ ORDER BY orgfil
017200140409     C/END-EXEC
017300140409      *          apertura cursore
017400140409     C/EXEC SQL
017500140409     C+ OPEN A1
017600140409     C/END-EXEC
017700140409     C                   DOU       SqlCod <> 0
017800140409     C/EXEC SQL
017900140409     C+ Fetch Next From A1 Into :azorgds
018000140409     C/END-EXEC
018100140409     c                   select
018200140409     C                   WHEN      SqlCod = 100
018300140409     c                   leave
018400140409      *
018500140409     C                   WHEN      SqlCod < 0
018600140409     c                   seton                                        H1
018700140409     c                   leave
018800140409      *
018900140409     C                   OTHER
019000140408     c                   eval      kfil=orgfil
019100140408     c                   exsr      Let_TAS
019200140409     C                   ENDSL
019300140409
019400140408     c                   enddo
019500140409     C/EXEC SQL
019600140409     C+ Close A1
019700140409     C/END-EXEC
019800140408     c
019900140408     c                   other
020000140409
020100140408     c     *loval        setll     azorg02l
020200140408     c                   read      azorg02l
020300140408     c                   dow       not %eof(azorg02l)
020400140408     c                   eval      kfil=orgfil
020500140408     c                   exsr      Let_TAS
020600140408     c                   read      azorg02l
020700140408     c                   enddo
020800140408     c
020900140408     c                   endsl
021000140404     c*
021100140404     c                   EXSR      Stampa
021200140408     c*
021300140408     c                   write     STO4FI
021400140422     c
021500140422     c                   if        pardet='S'
021600140422     c                   write     STO4RI_d
021700140422     c                   write     STO4FI_d
021800140422     c                   endif
021900000000     C*
022000140423     C                   CLEAR                   fnlv55ds
022100140423     C                   MOVEL     'C'           D55TLA
022200140423     C                   CALL      'FNLV55R'
022300140423     C                   PARM                    fnlv55ds
022400140423     c
022500000000     C                   SETON                                        LR
022600140408     C*------------------------------------------------------------------------*
022700140408     C* Ciclo di lettura su TITAS
022800140408     C*------------------------------------------------------------------------*
022900140408     c     LET_TAS       BEGSR
023000140409     C                   MOVEL     '0'           FINL00            1
023100140423     c* carico fil gestore
023200140423     c                   clear                   fnlv55ds
023300140423     C                   EVAL      D55TPT='6'
023400140423     c                   eval      d55lin=kfil
023500140423     c                   if        pardsa>0
023600140423     c                   eval      d55drf=pardsa
023700140423     c                   else
023800140423     c                   eval      d55drf=pardsd
023900140423     c                   endif
024000140423     c
024100140423     c                   call      'FNLV55R'
024200140423     c                   parm                    fnlv55ds
024300140408     c
024400140408     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
024500140408     C     FINL00        IFEQ      '0'                                          --- 1 -->
024600140408     C*
024700140408     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
024800140408     C                   EXSR      SETFIL
024900140408     C*
025000140408     C* CICLO FINO A FINE FILE
025100140408     C     FINL00        DOWEQ     '0'                                          --- 2 -->
025200140408     C*
025300140408     C* CREA IL RECORD SUI FILE DI LAVORO
025400140408     C                   EXSR      CRErec
025500140408     C*
025600140408     C* LEGGE IL RECORD SUCCESSIVO
025700140408     C                   EXSR      LETFIL
025800140408     C                   ENDDO                                                  <-- 2 ---
025900140408     C*
026000140408     C                   ENDIF                                                  <-- 1 ---
026100140408     c                   ENDSR
026200000000     C*------------------------------------------------------------------------*
026300000000     C* SETFIL - POSIZIONAMENTO SUL FILE
026400000000     C*------------------------------------------------------------------------*
026500000000     C     SETFIL        BEGSR
026600000000     C*
026700000000     C* POSIZIONAMENTO E PRIMA LETTURA
026800000000     C*
026900140404     C                   movel     pardsd        Kdcm
027000991123     C*
027100140404     C     Ktas          SETLL     TITAS26C                           99        *FINE FILE
027200000000     C*
027300000000     C     *IN99         IFEQ      '1'
027400000000     C                   MOVEL     '1'           FINL00
027500000000     C                   ELSE
027600000000     C                   EXSR      LETFIL
027700000000     C                   END
027800000000     C*
027900000000     C                   ENDSR
028000140404     C*------------------------------------------------------------------------*
028100000000     C* LETFIL - LETTURA PROSSIMO RECORD
028200000000     C*------------------------------------------------------------------------*
028300000000     C     LETFIL        BEGSR
028400000000     C*
028500000000     C* LEGGO FINO A:
028600000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
028700000000     C*     - TROVATO RECORD VALIDO
028800000000     C                   MOVEL     'N'           WRECOK
028900140404    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
029000000000     C     WRECOK        ANDEQ     'N'
029100000000     C*
029200140404     C*
029300140404    2c                   if        kfil>0
029400140404     C     KTAS          READE     TITAS26C                               99
029500140404     c                   else
029600140404     C     Kdcm          READE     TITAS26C                               99
029700140404    2c                   endif
029800140404     c
029900140404    2C                   IF        *IN99
030000140404     C     KDCM          SETGT     TITAS26C
030100140404     C                   READ      TITAS26C                               99
030200140404    3C                   IF        *IN99  OR TASDCM>PARDSA
030300140404     C                   MOVEL     '1'           FINL00
030400140404     C                   ELSE
030500140404     C                   EVAL      KDCM=TASDCM
030600140404     C     KTAS          setll     TITAS26C
030700140404    3C                   ENDIF
030800140404
030900140404   x2C                   ELSE
031000140404     c
031100140404     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
031200140404    2C                   END                                                    <-- 1 ---
031300140404     C*
031400140404    1C                   ENDDO                                                  <-- 1 ---
031500000000     C*
031600000000     C                   ENDSR
031700000000     C*------------------------------------------------------------------------*
031800000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
031900000000     C*------------------------------------------------------------------------*
032000000000     C     CHKREC        BEGSR
032100000000     C*
032200000000     C                   MOVEL     'S'           WRECOK            1
032300040305     C*
032400140414     c*
032500051109     C* ESCLUDE LE BOLLE di recupero
032600051109     C                   Z-ADD     1             i
032700000000     C                   SETOFF                                           28
032800051109     C     TASTBL        LOOKUP    TB(i)                                  28
032900051109     C                   IF        *in28 = *on
033000000000     C                   MOVEL     'N'           WRECOK
033100000000     C                   ENDIF
033200140404     c
033300140414     c* Escludo bolle export e le bolle con consegna anomala
033400140414     c                   if        tasnzd<>*blanks or tascca<>' '
033500140408     C                   MOVEL     'N'           WRECOK
033600140408     c                   endif
033700140414     c
033800140411     c* Escludo le bolle in fermo deposito
033900140411     c                   if        tasffd='S'
034000140411     C                   MOVEL     'N'           WRECOK
034100140411     c                   endif
034200011112     C*
034300000000     C                   ENDSR
034400140408     C*------------------------------------------------------------------------*
034500140408     C* controllo se si tratta di bolla da elaborare
034600140408     C*------------------------------------------------------------------------*
034700140408     C     NOELAB        BEGSR
034800140408     C*
034900140408     C                   MOVEL     'S'           Welab             1
035000140408     C*
035100140408     c* Cerco se si tratta di distinta PDA
035200140508     c* solo in AS888
035300140508    1c                   if        %subst(knsif:7:1)='P'
035400140508     c                   movel     taspdc        kfgs              3 0
035500140508     c                   movel     4             knpg              1 0
035600140508     c                   z-add     tasndc        knfv
035700140508     c     kdst          chain     fidst01l
035800140508    2c                   if        %found(fidst01l)
035900140508     c                             and (dstpda='C' or dstpda='E')
036000140508   x2c                   else
036100140508     C                   MOVEL     'N'           Welab
036200140508    2c                   endif
036300140508     c
036400140508   x1c                   else
036500140508   c
036600140508     c* Solo dall'aprile 2014 abbiamo le bolle con il flag impostato
036700140508     c                   if        tasaas<2014
036800140508     C                   MOVEL     'N'           Welab
036900140508     c                   endif
037000140508     c
037100140508     c                   if        tasaas=2014 and tasmgs<0401
037200140508     C                   MOVEL     'N'           Welab
037300140508     c                   endif
037400140408     c* chaino record GEN
037500140408     c     kar5          chain     fiar531c
037600140408     c                   if        %found(fiar531c)
037700140408     c                   eval      dar5gen=ar5uni
037800140408     c                   if        §AR5PDACO<>'  '
037900140408     C                   MOVEL     'N'           Welab
038000140408     c                   endif
038100140408     c                   endif
038200140508     c
038300140508    1C                   ENDIF
038400140408     c
038500140411     c* Non elaboro se impostata ora consegna richiesta
038600140411     c                   if        tashcr>0
038700140408     C                   MOVEL     'N'           Welab
038800140408     c                   endif
038900140408     c
039000140408     C                   ENDSR
039100000000     C*------------------------------------------------------------------------*
039200140408     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO
039300000000     C*------------------------------------------------------------------------*
039400140408     C     CRErec        BEGSR
039500140404     c                   clear                   kloc
039600140404     c
039700140404     c
039800140404     c* Richiamo pgm reperimento orari servizi
039900140404     c                   clear                   trulorsds
040000140404     c                   clear                   trulor2ds
040100140514     c                   clear                   trulor3ds
040200140404     c                   eval      ioredta=pardsd
040300140408     c                   eval      iorefil=taslna
040400140404     c                   eval      iorenar=tasnzd
040500140404     c                   eval      iorecap=tascad
040600140514     c                   eval      ioreloc=taslod
040700140414     c                   eval      ioreddc=tasddc
040800140414     c                   eval      iorendc=tasndc
040900140414     c                   eval      ioretfp=tastfp
041000140414     c                   eval      ioretfa=tastfa
041100140414     c                   eval      ioredti=tasdti
041200140428     c                   eval      iorehti=9999
041300140404     c                   eval      ioretser=partser
041400140514     c                   eval      ioretsp =tastsp
041500140404     c                   call      'TRULORSR'
041600140404     c                   parm                    kpjba
041700140404     c                   parm                    trulorsds
041800140404     c                   parm                    trulor2ds
041900140514     c                   parm                    trulor3ds
042000140514     c                   eval      wnorm=OOR3NORM
042100140514     c
042200140514     c                   if        oor3norm='S'
042300140514     c                   eval      kloc =OOR3LOC_N
042400140514     c                   endif
042500140404     c*
042600140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
042700140415     c                   if        oor2fcsgl='P'
042800140415     c                   eval      kserv='P '
042900140415     c                   else
043000140415     c                   clear                   kserv
043100140415     c                   endif
043200140415
043300140415     c     kccos         chain     wfccos1l
043400140404     c                   if        not %found(wfccos1l)
043500140404     c                   clear                   wfccos00
043600140404     c                   eval      wcosdcmd=pardsd
043700140404     c                   eval      wcosdcma=pardsa
043800140404     c                   eval      wcosver =p96ver
043900140411     c                   eval      wcoslna =d55tfa
044000140404     c                   eval      wcosnaz =tasnzd
044100140404     c                   eval      wcoscap =tascad
044200140404     c                   eval      wcosloc =kloc
044300140404     c                   eval      wcostser=partser
044400140404     c                   eval      wcosorif=OOR2ORIF
044500140415     c                   eval      wcosserv=kserv
044600140424     c
044700140424     c* obsoleto
044800140424     c     kcpc          chain     azcpc01l
044900140424     c                   if        %found(azcpc01l)
045000140424     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
045100140424     c                   endif
045200140424     c                   if        wcosloc<>*blanks
045300140424     c     kcpl          chain     azcpl01l
045400140424     c                   if        %found(azcpl01l)
045500140424     c                   eval      wcosloco=%subst(cplfi1:1:1)
045600140424     c                   endif
045700140424     c                   endif
045800140424     c* Imposto gli orari nel file
045900140424     c                   eval      WCOSOSTIS=OOR2STIS
046000140424     c                   eval      WCOSOSTfS=OOR2STfS
046100140424     c                   eval      WCOSOmiis=OOR2miis
046200140424     c                   eval      WCOSOmxfs=OOR2mxfs
046300140424     c
046400140415     c* imposto orari dettaglio se ci sono e richiestoc
046500140415     c                   if        pardet='S'
046600140422     c                   eval      wcosor1=or_dal(1)
046700140422     c                   eval      wcosor2=or_dal(2)
046800140422     c                   eval      wcosor3=or_dal(3)
046900140422     c                   eval      wcosor4=or_dal(4)
047000140422     c                   eval      wcosor5=or_dal(5)
047100140422     c                   eval      wcosor6=or_dal(6)
047200140422     c                   eval      wcosor7=or_dal(7)
047300140422     c                   eval      wcosor8=or_dal(8)
047400140422     c                   eval      wcosor9=or_dal(9)
047500140422     c                   eval      wcosorOl=paroltre
047600140404     c                   endif
047700140415     c                   endif
047800140404     c
047900140404     c                   add       1             wcosts
048000140404     c* Se loc non normalizzata
048100140514     c                   if        wnorm<>'S'
048200140404     c                   add       1             wcostsNN
048300140404     c                   endif
048400140408     c
048500140408     c* Controllo se spedizione da elaborare
048600140408     c                   exsr      Noelab
048700140408     c
048800140404     c* elaborate
048900140408     c                   if        welab='S'
049000140404     c                   add       1             wcostse
049100140404     c
049200140404     c* consegna negli orari std
049300140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
049400140404     c                   add       1             wcostsos
049500140404     c                   endif
049600140404     c* consegna negli orari min/max
049700140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
049800140404     c                   add       1             wcostsom
049900140404     c                   endif
050000140415     c* imposto campi dettaglio
050100140415     c                   exsr      ImpoDET
050200140408     c                   endif
050300140404     c
050400140404     c                   if        not %found(wfccos1l)
050500140404     c                   write     wfccos00
050600140404     c                   else
050700140404     c                   update    wfccos00
050800140404     c                   endif
050900000000     C*
051000000000     C                   ENDSR
051100140404     C*------------------------------------------------------------------------*
051200140404     C* Stampa - statistica
051300140404     C*------------------------------------------------------------------------*
051400140404     c     STAMPA        BEGSR
051500140404     c* Tpo servizio
051600140404     c                   if        partser='C'
051700140404     c                   eval      p1dtser='CONSEGNE'
051800140404     C                   ELSE
051900140408     c                   eval      p1dtser=' RITIRI '
052000140404     C                   ENDIF
052100140408     c* giro le date
052200140409     C                   MOVE      pardsd        G02inv
052300140408     C                   MOVEL     '3'           G02ERR
052400140408     C                   CALL      'XSRDA8'
052500140408     C                   PARM                    WLBDAT
052600140408     c                   z-add     g02dat        p1ddsd
052700140408     c*
052800140408     c                   if        pardsa>0
052900140409     C                   MOVE      pardsa        G02inv
053000140408     C                   MOVEL     '3'           G02ERR
053100140408     C                   CALL      'XSRDA8'
053200140408     C                   PARM                    WLBDAT
053300140408     c                   z-add     g02dat        p1ddsa
053400140408     c                   endif
053500140408     c* Orari per calcolo %
053600140408     c                   if        parorari='S'
053700140408     c                   eval      p1dorari='STANDARD'
053800140424     c                   eval      p1dtor_d='Standard'
053900140408     c                   else
054000140408     c                   eval      p1dorari='MINIMO/MASSIMO'
054100140424     c                   eval      p1dtor_d=' Min/Max'
054200140408     c                   endif
054300140408     c
054400140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
054500140408     c                   if        parcor='C'
054600140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
054700140411     c                   eval      p1dbolstd='   Nei limiti STD   '
054800140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
054900140408     C                   ELSE
055000140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
055100140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
055200140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
055300140408     C                   ENDIF
055400140423     c
055500140423     c                   write     STO4TE
055600140423
055700140423     c* Orari nel dettaglio
055800140423     c                   if        pardet='S'
055900140423     c                   exsr      TestaDET
056000140423     c                   write     STO4TE_d
056100140423     c                   endif
056200140404     c
056300140404     C
056400140424     c*
056500140404     c     *loval        setll     wfccos1l
056600140404     c                   read      wfccos1l
056700140404     c                   dow       not %eof(wfccos1l)
056800140404     c
056900140408     c* a cambio filiale stampo a pagina nuova
057000140408     c                   if        wcoslna<>savlna
057100140408
057200140408     c                   if        savlna>0
057300140423     c                   exsr      Totfil
057400140423     c
057500140408     c                   write     STO4TE
057600140422     c
057700140422     c                   if        pardet='S'
057800140422     c                   write     STO4RI_d
057900140422     c                   write     STO4TE_d
058000140408     c                   endif
058100140423     c
058200140423     c                   clear                   tot_ts
058300140423     c                   clear                   tot_tse
058400140423     c                   clear                   tot_tsos
058500140423     c                   clear                   tot_tsom
058600140422     c                   endif
058700140408
058800140408     c                   z-add     1             i
058900140408     C     wcoslna       lookup    fil(i)                                 55
059000140404     C                   if        %found
059100140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
059200140408     c                   eval      p1dare=dare(i)
059300140404     C                   else
059400140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
059500140408     c                   eval      p1dare=*all'?'
059600140404     C                   endif
059700140408
059800140408     c                   eval      savlna=wcoslna
059900140408
060000140408     c                   write     STO4TEF
060100140414     c                   write     STO4RI
060200140422     c                   if        pardet='S'
060300140422     c                   write     STO4TEF_d
060400140422     c                   write     STO4RI_d
060500140408     c                   endif
060600140422     c                   endif
060700140408
060800140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
060900140411     c                   if        wcostse>=parnum and wcostse>0
061000140408     c* voglio la % di correttezza
061100140408     c                   if        parcor='C'
061200140404     c* Calcolo % STD
061300140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
061400140408     c                   eval      p1dstsos=WCOSTSOs
061500140404     c* Calcolo % MIN/MAX
061600140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
061700140408     c                   eval      p1dstsom=WCOSTSOm
061800140408     c
061900140408     c                   else
062000140408     c* voglio la % delle fuori orario
062100140408     c                   eval      wfuori_os=wcostse-wcostsos
062200140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
062300140408     c                   eval      p1dstsos=Wfuori_os
062400140408
062500140408     c                   eval      wfuori_om=wcostse-wcostsom
062600140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
062700140408     c                   eval      p1dstsom=Wfuori_om
062800140408     c                   endif
062900140408     c
063000140407     c* stampo solo al di sotto della % richiesta
063100140408     c                   eval      wstampadet='S'
063200140408
063300140408     c                   select
063400140408     c                   when      parperc=0
063500140408
063600140408     c                   when      parorari='S'  and parcor='C' and
063700140408     c                             p1dperc_os>=parperc
063800140408     c                   eval      wstampadet='N'
063900140408     c
064000140408     c                   when      parorari='S'  and parcor='F' and
064100140408     c                             p1dperc_os< parperc
064200140408     c                   eval      wstampadet='N'
064300140408
064400140408     c                   when      parorari='M'  and parcor='C' and
064500140408     c                             p1dperc_om>=parperc
064600140408     c                   eval      wstampadet='N'
064700140408     c
064800140408     c                   when      parorari='M'  and parcor='F' and
064900140408     c                             p1dperc_om< parperc
065000140408     c                   eval      wstampadet='N'
065100140408     c                   endsl
065200140404     c*
065300140408     c                   if        wstampadet='S'
065400140424     c* POM
065500140424     c                   if        wcosserv='P '
065600140424     c                   eval      p1dpom='Pom'
065700140424     c                   else
065800140424     c                   eval      p1dpom='   '
065900140424     c                   endif
066000140411     c*
066100140411     c* Se la località è vuota imposto '??'
066200140411     c                   if        wcosloc=*blanks
066300140411     c                   eval      wcosloc='??'
066400140411     c                   endif
066500140408     c
066600140408     c                   if        *in06
066700140408     c                   write     sto4te
066800140408     c                   write     STO4TEF
066900140414     c                   write     STO4RI
067000140408     c                   setoff                                       06
067100140404     c                   endif
067200140408     c
067300140408     c* Richiamo pgm reperimento orari servizi
067400140424     c***                clear                   trulorsds
067500140424     c***                clear                   trulor2ds
067600140424     c***                eval      ioredta=pardsd
067700140424     c***                eval      iorefil=wcoslna
067800140424     c***                eval      iorenar=wcosnaz
067900140424     c***                eval      iorecap=wcoscap
068000140424     c***                eval      ioreloc=wcosloc
068100140424     c***                if        wcosloc=*blanks
068200140424     c***                eval      ioreloc='A '
068300140424     c***                endif
068400140424     c***                eval      ioretser=wcostser
068500140424     c***                eval      ioretsp ='C'
068600140424     c***                call      'TRULORSR'
068700140424     c***                parm                    kpjba
068800140424     c***                parm                    trulorsds
068900140424     c**+                parm                    trulor2ds
069000140408     c
069100140424     c                   eval      p1dstis=wcosoSTiS
069200140424     c                   eval      p1dstfs=wcosoSTFS
069300140424     c                   eval      p1dmiis=wcosomiis
069400140424     c                   eval      p1dmxfs=wcosomxfs
069500140404     c
069600140408     c                   write     STO4det
069700140422     c
069800140422     c* se devo stampare anche il dettaglio
069900140422     c                   if        pardet='S'
070000140422     c                   exsr      Stadet
070100140422     c                   endif
070200140408     c
070300140407     c                   endif
070400140408     c                   endif
070500140424     c* totali di filiale
070600140424     c                   add       WCOSTS        tot_ts
070700140424     c                   add       WCOSTSE       tot_tse
070800140424     c                   add       WCOSTSos      tot_tsos
070900140424     c                   add       WCOSTSom      tot_tsom
071000140407     c
071100140404     c                   read      wfccos1l
071200140404     c
071300140404     c                   enddo
071400140423     c
071500140423     c* totali finali
071600140423     c                   if        savlna>0
071700140423     c                   exsr      totfil
071800140423     c                   endif
071900140404     c
072000140404     c                   ENDSR
072100140423     C*------------------------------------------------------------------------*
072200140423     c* Imposta gli orari in testata dettaglioù
072300140423     C*------------------------------------------------------------------------*
072400140423     c     TestaDet      BEGSR
072500140423     c                   eval      p1dsot = paror1
072600140423     c                   eval      p1doltre= paroltre
072700140423     c                   eval      p1dda1  = or_dal(1)
072800140423
072900140423     c                   if        or_dal(2)>0
073000140423     c                   eval      p1dda2  = or_dal(2)
073100140423     c                   movel     or_dal(2)     w0060
073200140423     c                   move      w0060         timeiso
073300140423     c                   subdur    1:*mn         timeiso
073400140423     c                   eval      w0060     =%dec(timeiso)
073500140423     c                   movel     w0060         p1dal1
073600140423     c                   else
073700140423     c                   eval      p1dal1=paroltre
073800140423     c                   leavesr
073900140423     c                   endif
074000140423     c
074100140423     c                   if        or_dal(3)>0
074200140423     c                   eval      p1dda3  = or_dal(3)
074300140423     c                   movel     or_dal(3)     w0060
074400140423     c                   move      w0060         timeiso
074500140423     c                   subdur    1:*mn         timeiso
074600140423     c                   eval      w0060     =%dec(timeiso)
074700140423     c                   movel     w0060         p1dal2
074800140423     c                   else
074900140423     c                   eval      p1dal2=paroltre
075000140423     c                   leavesr
075100140423     c                   endif
075200140423
075300140423     c                   if        or_dal(4)>0
075400140423     c                   eval      p1dda4  = or_dal(4)
075500140423     c                   movel     or_dal(4)     w0060
075600140423     c                   move      w0060         timeiso
075700140423     c                   subdur    1:*mn         timeiso
075800140423     c                   eval      w0060     =%dec(timeiso)
075900140423     c                   movel     w0060         p1dal3
076000140423     c                   else
076100140423     c                   eval      p1dal3=paroltre
076200140423     c                   leavesr
076300140423     c                   endif
076400140423
076500140423     c                   if        or_dal(5)>0
076600140423     c                   eval      p1dda5  = or_dal(5)
076700140423     c                   movel     or_dal(5)     w0060
076800140423     c                   move      w0060         timeiso
076900140423     c                   subdur    1:*mn         timeiso
077000140423     c                   eval      w0060     =%dec(timeiso)
077100140423     c                   movel     w0060         p1dal4
077200140423     c                   else
077300140423     c                   eval      p1dal4=paroltre
077400140423     c                   leavesr
077500140423     c                   endif
077600140423
077700140423     c                   if        or_dal(6)>0
077800140423     c                   eval      p1dda6  = or_dal(6)
077900140423     c                   movel     or_dal(6)     w0060
078000140423     c                   move      w0060         timeiso
078100140423     c                   subdur    1:*mn         timeiso
078200140423     c                   eval      w0060     =%dec(timeiso)
078300140423     c                   movel     w0060         p1dal5
078400140423     c                   else
078500140423     c                   eval      p1dal5=paroltre
078600140423     c                   leavesr
078700140423     c                   endif
078800140423
078900140423     c                   if        or_dal(7)>0
079000140423     c                   eval      p1dda7  = or_dal(7)
079100140423     c                   movel     or_dal(7)     w0060
079200140423     c                   move      w0060         timeiso
079300140423     c                   subdur    1:*mn         timeiso
079400140423     c                   eval      w0060     =%dec(timeiso)
079500140423     c                   movel     w0060         p1dal6
079600140423     c                   else
079700140423     c                   eval      p1dal6=paroltre
079800140423     c                   leavesr
079900140423     c                   endif
080000140423
080100140423     c                   if        or_dal(8)>0
080200140423     c                   eval      p1dda8  = or_dal(8)
080300140423     c                   movel     or_dal(8)     w0060
080400140423     c                   move      w0060         timeiso
080500140423     c                   subdur    1:*mn         timeiso
080600140423     c                   eval      w0060     =%dec(timeiso)
080700140423     c                   movel     w0060         p1dal7
080800140423     c                   else
080900140423     c                   eval      p1dal7=paroltre
081000140423     c                   leavesr
081100140423     c                   endif
081200140423     c                   if        or_dal(9)>0
081300140423     c                   eval      p1dda9  = or_dal(9)
081400140423     c                   movel     or_dal(9)     w0060
081500140423     c                   move      w0060         timeiso
081600140423     c                   subdur    1:*mn         timeiso
081700140423     c                   eval      w0060     =%dec(timeiso)
081800140423     c                   movel     w0060         p1dal8
081900140423     c
082000140423     c                   eval      p1dal9=paroltre
082100140423     c                   else
082200140423     c                   eval      p1dal8=paroltre
082300140423     c                   leavesr
082400140423     c                   endif
082500140423     c
082600140423     c                   ENDSR
082700140422     C*------------------------------------------------------------------------*
082800140422     C* Stampa dettaglio
082900140422     C*------------------------------------------------------------------------*
083000140422     C     StaDET        BEGSR
083100140422     c                   if        *in07
083200140422     c                   write     sto4te_d
083300140422     c                   write     STO4TEF_d
083400140422     c                   write     STO4RI_d
083500140422     c                   setoff                                       07
083600140422     c                   endif
083700140424     c* Imposto gli orari
083800140424     c                   if        parorari='S'
083900140424     c                   eval      p1dor_dal=WCOSOSTIS
084000140424     c                   eval      p1dor_al =WCOSOSTfS
084100140424     c                   else
084200140424     c                   eval      p1dor_dal=WCOSOmiis
084300140424     c                   eval      p1dor_al =WCOSOmxfS
084400140424     c                   endif
084500140424     c                   setoff                                       111213
084600140424     c                   setoff                                       141516
084700140424     c                   setoff                                       171819
084800140505     c                   setoff                                       2021
084900140422     c* calcolo le %
085000140422     c* SOTTO
085100140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
085200140422     C                   IF        WPERC>0
085300140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
085400140505     c                   seton                                        20
085500140422     c                   else
085600140424     c                   eval      p1cp_sot='    '
085700140422     c                   endif
085800140422     c* OLTRE
085900140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
086000140422     C                   IF        WPERC>0
086100140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
086200140505     c                   seton                                        21
086300140422     c                   else
086400140424     c                   eval      p1cp_olt='    '
086500140422     c                   endif
086600140422     c* % 1
086700140505    1c                   if        wcosor1  >0
086800140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
086900140422     c
087000140505    2C                   IF        WPERC>0
087100140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
087200140505     c
087300140505    2c                   select
087400140505     c                   when      or_dal(1)<p1dor_dal
087500140505     c                   seton                                        11
087600140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
087700140505     c                   seton                                        11
087800140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
087900140505     c                   seton                                        11
088000140505    2c                   endsl
088100140505
088200140422     c                   else
088300140424     c                   eval      p1cp_o1 ='    '
088400140505    2c                   endif
088500140424     c
088600140505    1c                   endif
088700140422     c
088800140422     c* % 2
088900140422     c                   if        wcosor2  >0
089000140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
089100140422     c
089200140422     C                   IF        WPERC>0
089300140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
089400140505
089500140505     c                   select
089600140505     c                   when      or_dal(2)<p1dor_dal
089700140505     c                   seton                                        12
089800140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
089900140505     c                   seton                                        12
090000140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
090100140505     c                   seton                                        12
090200140505     c                   endsl
090300140505
090400140422     c                   else
090500140424     c                   eval      p1cp_o2 ='    '
090600140422     c                   endif
090700140422     c                   endif
090800140423     c* % 3
090900140423     c                   if        wcosor3  >0
091000140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
091100140423     c
091200140423     C                   IF        WPERC>0
091300140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
091400140505
091500140505     c                   select
091600140505     c                   when      or_dal(3)<p1dor_dal
091700140505     c                   seton                                        13
091800140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
091900140505     c                   seton                                        13
092000140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
092100140505     c                   seton                                        13
092200140505     c                   endsl
092300140423     c                   else
092400140424     c                   eval      p1cp_o3 ='    '
092500140423     c                   endif
092600140423     c                   endif
092700140423     c* % 4
092800140423     c                   if        wcosor4  >0
092900140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
093000140423     c
093100140423     C                   IF        WPERC>0
093200140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
093300140505
093400140505     c                   select
093500140505     c                   when      or_dal(4)<p1dor_dal
093600140505     c                   seton                                        14
093700140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
093800140505     c                   seton                                        14
093900140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
094000140505     c                   seton                                        14
094100140505     c                   endsl
094200140423     c                   else
094300140424     c                   eval      p1cp_o4 ='    '
094400140423     c                   endif
094500140423     c                   endif
094600140423     c* % 5
094700140423     c                   if        wcosor5  >0
094800140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
094900140423     c
095000140423     C                   IF        WPERC>0
095100140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
095200140505
095300140505     c                   select
095400140505     c                   when      or_dal(5)<p1dor_dal
095500140505     c                   seton                                        15
095600140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
095700140505     c                   seton                                        15
095800140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
095900140505     c                   seton                                        15
096000140505     c                   endsl
096100140423     c                   else
096200140424     c                   eval      p1cp_o5 ='    '
096300140423     c                   endif
096400140423     c                   endif
096500140423     c* % 6
096600140423     c                   if        wcosor6  >0
096700140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
096800140423     c
096900140423     C                   IF        WPERC>0
097000140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
097100140505
097200140505     c                   select
097300140505     c                   when      or_dal(6)<p1dor_dal
097400140505     c                   seton                                        16
097500140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
097600140505     c                   seton                                        16
097700140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
097800140505     c                   seton                                        16
097900140505     c                   endsl
098000140423     c                   else
098100140424     c                   eval      p1cp_o6 ='    '
098200140423     c                   endif
098300140423     c                   endif
098400140423     c* % 7
098500140423     c                   if        wcosor7  >0
098600140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
098700140423     c
098800140423     C                   IF        WPERC>0
098900140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
099000140505
099100140505     c                   select
099200140505     c                   when      or_dal(7)<p1dor_dal
099300140505     c                   seton                                        17
099400140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
099500140505     c                   seton                                        17
099600140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
099700140505     c                   seton                                        17
099800140505     c                   endsl
099900140423     c                   else
100000140424     c                   eval      p1cp_o7 ='    '
100100140423     c                   endif
100200140423     c                   endif
100300140423     c* % 8
100400140423     c                   if        wcosor8  >0
100500140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
100600140423     c
100700140423     C                   IF        WPERC>0
100800140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
100900140505
101000140505     c                   select
101100140505     c                   when      or_dal(8)<p1dor_dal
101200140505     c                   seton                                        18
101300140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
101400140505     c                   seton                                        18
101500140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
101600140505     c                   seton                                        18
101700140505     c                   endsl
101800140423     c                   else
101900140424     c                   eval      p1cp_o8 ='    '
102000140423     c                   endif
102100140423     c                   endif
102200140423     c* % 9
102300140423     c                   if        wcosor9  >0
102400140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
102500140423     c
102600140423     C                   IF        WPERC>0
102700140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
102800140505
102900140505     c                   select
103000140505     c                   when      or_dal(9)<p1dor_dal
103100140505     c                   seton                                        19
103200140505     c                   when      paroltre >p1dor_al
103300140505     c                   seton                                        19
103400140505     c                   endsl
103500140423     c                   else
103600140424     c                   eval      p1cp_o9 ='    '
103700140423     c                   endif
103800140423     c                   endif
103900140422     c
104000140422     c                   write     STO4det_d
104100140422     c                   ENDSR
104200140423     C*------------------------------------------------------------------------*
104300140423     c* Stampa totali di filiale
104400140423     C*------------------------------------------------------------------------*
104500140423     c     TOTfil        BEGSR
104600140423     c* voglio la % di correttezza
104700140423     c                   if        parcor='C'
104800140423     c* Calcolo % STD
104900140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
105000140423     c                   eval      p1dstsos=tot_TSOs
105100140423     c* Calcolo % MIN/MAX
105200140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
105300140423     c                   eval      p1dstsom=tot_TSOm
105400140423     c
105500140423     c                   else
105600140423     c* voglio la % delle fuori orario
105700140423     c                   eval      wfuori_os=tot_tse-tot_tsos
105800140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
105900140423     c                   eval      p1dstsos=Wfuori_os
106000140423
106100140423     c                   eval      wfuori_om=tot_tse-tot_tsom
106200140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
106300140423     c                   eval      p1dstsom=Wfuori_om
106400140423     c
106500140423     c                   endif
106600140423
106700140423     c                   if        *in06
106800140423     c                   write     sto4te
106900140423     c                   write     STO4TEF
107000140423     c                   write     STO4RI
107100140423     c                   setoff                                       06
107200140423     c                   else
107300140423     c                   write     STO4RI
107400140423     c                   write     STO4TOT
107500140423     c  n06              write     STO4RI
107600140423     c                   endif
107700140423     c
107800140423     c                   ENDSR
107900000000     C*------------------------------------------------------------------------*
108000000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
108100000000     C*------------------------------------------------------------------------*
108200000000     C     CARORG        BEGSR
108300000000     C*
108400140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
108500000000     C  N99              READ      AZORG01L                               99
108600000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
108700000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
108800000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
108900000000     C     ORGFAG        IFNE      'V'
109000051109     C                   ADD       1             I
109100000000     C                   Z-ADD     ORGFIL        FIL(I)
109200000000     C                   Z-ADD     ORGCAR        ARE(I)
109300000000     C                   MOVEL     ORGFL3        DIV(I)
109400140404     C* CALCOLA NETWORK
109500140404     C                   MOVEL     ORGDE3        OG143
109600140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
109700051109     c* riempio schiere decodifiche p.o., distretto e area
109800051109     c* distretto
109900140408     C***                Z-ADD     CODUT         KTBUTE
110000140408     C***                MOVEL     '17'          Ktbcod
110100140408     C***                MOVEL(P)  ORGFL3        KKEY
110200140408     C***  KTAB          CHAIN     TABEL00F                           30
110300140408    2C***  *IN30         IFEQ      *OFF
110400140408     C***  TBLFLG        ANDEQ     *BLANKS
110500140408     C***                MOVEL     TBLUNI        DS17
110600140408     C***                MOVEL     §17DES        ddiv(I)
110700140408     C***                ELSE
110800140408     C***                MOVE      *BLANKS       ddiv(I)
110900140409    2C***                END
111000051109     c* area
111100051109     C                   Z-ADD     CODUT         KTBUTE
111200051109     C                   MOVEL     '05'          Ktbcod
111300051109     C                   MOVEL(P)  ORGCAR        KKEY
111400051109     C     KTAB          CHAIN     TABEL00F                           30
111500051109    2C     *IN30         IFEQ      *OFF
111600051109     C     TBLFLG        ANDEQ     *BLANKS
111700051109     C                   MOVEL     TBLUNI        DS05
111800140408     C***                MOVEL     §05DES        dare(I)
111900140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
112000051109     C                   ELSE
112100140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
112200140408     C***                MOVE      *BLANKS       dare(I)
112300051109    2C                   END
112400051109     c* p.o.
112500051109     C                   MOVEl     orgdes        dfil(I)
112600051109     c
112700000000     C                   END
112800000000     C                   END                                                    <-- 2 ---
112900000000     C                   READ      AZORG01L                               99
113000000000     C                   END                                                    <-- 1 ---
113100000000     C*
113200000000     C                   ENDSR
113300000000     C*------------------------------------------------------------------------*
113400051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
113500000000     C*------------------------------------------------------------------------*
113600000000     C     CARTBL        BEGSR
113700000000     C*--------------------
113800051109     C* TIPI BOLLA di recupero
113900000000     C*--------------------
114000000000     C                   Z-ADD     0             I
114100000000     C                   Z-ADD     CODUT         KTBUTE
114200000000     C                   MOVEL     'TB'          KTBCOD
114300000000     C     KEYTAB        SETLL     TABEL00F
114400051109     C                   READe     TABEL00F                               99
114500000000     C     *IN99         DOWEQ     *OFF
114600000000     C     TBLFLG        IFEQ      *BLANKS
114700051109     C                   MOVEL     TBLUNI        DSTB
114800051109     c                   if        §tbrbl = 'R'
114900051109     C                   ADD       1             I
115000051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
115100051109     c                   endif
115200000000     C                   END
115300051109     C     keytab        READe     TABEL00F                               99
115400000000     C                   END
115500000000     C*
115600140428     c* recupero versione cappario alla data DAL
115700140428     C                   Z-ADD     pardsd        P96DRI            8 0
115800140428     C                   Z-ADD     *ZEROS        P96VER            5 0
115900140428     C                   CALL      'TISI96R'
116000140428     C                   PARM                    P96DRI
116100140428     C                   PARM                    P96VER
116200140428     c* carico gli orari in schiera
116300140428     c                   z-add     1             yy
116400140428     c                   if        paror1 >0
116500140428     c                   eval      or_dal(yy)=paror1
116600140428     c                   add       1             yy
116700140428     c                   endif
116800140428     c                   if        paror2 >0
116900140428     c                   eval      or_dal(yy)=paror2
117000140428     c                   add       1             yy
117100140428     c                   endif
117200140428     c                   if        paror3 >0
117300140428     c                   eval      or_dal(yy)=paror3
117400140428     c                   add       1             yy
117500140428     c                   endif
117600140428     c                   if        paror4 >0
117700140428     c                   eval      or_dal(yy)=paror4
117800140428     c                   add       1             yy
117900140428     c                   endif
118000140428     c                   if        paror5 >0
118100140428     c                   eval      or_dal(yy)=paror5
118200140428     c                   add       1             yy
118300140428     c                   endif
118400140428     c                   if        paror6 >0
118500140428     c                   eval      or_dal(yy)=paror6
118600140428     c                   add       1             yy
118700140428     c                   endif
118800140428     c                   if        paror7 >0
118900140428     c                   eval      or_dal(yy)=paror7
119000140428     c                   add       1             yy
119100140428     c                   endif
119200140428     c                   if        paror8 >0
119300140428     c                   eval      or_dal(yy)=paror8
119400140428     c                   add       1             yy
119500140428     c                   endif
119600140428     c                   if        paror9 >0
119700140428     c                   eval      or_dal(yy)=paror9
119800140428     c                   add       1             yy
119900140428     c                   endif
120000140428     C*
120100000000     C                   ENDSR
120200140415     C*------------------------------------------------------------------------*
120300140415     C* Imposto i campi di dettaglio in base ai range passati
120400140415     C*------------------------------------------------------------------------*
120500140415     C     ImpoDET       BEGSR
120600140415     c*
120700140422     c                   if        tashmc<or_dal(1)
120800140422     c                   add       1             wcosmint
120900140422     c                   leavesr
121000140422     c                   endif
121100140422     c                   if        tashmc> paroltre
121200140422     c                   add       1             wcosmaxt
121300140422     c                   leavesr
121400140422     c                   endif
121500140422
121600140422     c                   z-add     1             yy
121700140422     c                   dow       or_dal(yy)>0
121800140423     c                   if        tashmc>=or_dal(yy) and
121900140423     c
122000140423     c                             (or_dal(yy+1)>0 and
122100140423     c                              tashmc< or_dal(Yy+1)  or
122200140423     c                             (or_dal(yy+1)=0 and
122300140423     c                             tashmc<=paroltre))
122400140422     c                   select
122500140422     c                   when      yy=1
122600140415     c                   add       1             wcosor1t
122700140422     c                   when      yy=2
122800140422     c                   add       1             wcosor2t
122900140422     c                   when      yy=3
123000140422     c                   add       1             wcosor3t
123100140422     c                   when      yy=4
123200140422     c                   add       1             wcosor4t
123300140422     c                   when      yy=5
123400140422     c                   add       1             wcosor5t
123500140422     c                   when      yy=6
123600140422     c                   add       1             wcosor6t
123700140422     c                   when      yy=7
123800140422     c                   add       1             wcosor7t
123900140422     c                   when      yy=8
124000140422     c                   add       1             wcosor8t
124100140422     c                   when      yy=9
124200140422     c                   add       1             wcosor9t
124300140422     c                   endsl
124400140422     c
124500140415     c                   leavesr
124600140415     c                   endif
124700140422     c                   add       1             yy
124800140415     c
124900140422     c                   enddo
125000140415     c
125100140415     c                   ENDSR
125200000000     C*------------------------------------------------------------------------*
125300000000     C* *INZSR - ROUTINE INIZIALE
125400000000     C*------------------------------------------------------------------------*
125500000000     C     *INZSR        BEGSR
125600000000     C*--------------------
125700000000     C* RICEVIMENTO PARAMETRI
125800000000     C*--------------------
125900140408     c
126000140408     c     *dtaara       define    §azute        azuteds
126100140408     c     *dtaara       define    §datiute      ddatiute
126200140408     c                   in(E)     *dtaara
126300140408     c                   If        %error  or RSUT = *blanks
126400140408     c                   CLEAR                   tibs34ds
126500140408     c                   CALL      'TIBS34R'
126600140408     c                   PARM                    tibs34ds
126700140408     c                   in        *dtaara
126800140408     c                   EndIf
126900140408     c
127000140408     c                   eval      p1drsut=rsut
127100140408     C*
127200000000     C*--------------------
127300000000     C* CAMPI RIFERITI AL DATA BASE
127400000000     C*--------------------
127500000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
127600000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
127700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
127800051109     C     *LIKE         DEFINE    TBLkey        Kkey
127900000000     C*--------------------
128000000000     C* CHIAVI DI LETTURA
128100000000     C*--------------------
128200140404     C* CHIAVE LETTURA TITAS26C - COMPLETA
128300140404     C     Ktas          KLIST
128400140404     C                   KFLD                    Kdcm
128500140404     C                   KFLD                    Kfil
128600000000     C*
128700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
128800000000     C     KEYTAB        KLIST
128900000000     C                   KFLD                    KTBUTE
129000000000     C                   KFLD                    KTBCOD
129100051109     C     KTAB          KLIST
129200051109     C                   KFLD                    ktbute
129300051109     C                   KFLD                    ktbcod
129400051109     C                   KFLD                    KKEY
129500000000     C*
129600060524     C     Kar5          KLIST
129700060524     C                   KFLD                    tasAAS
129800060524     C                   KFLD                    taslnp
129900060524     C                   KFLD                    tasnrs
130000060524     C                   KFLD                    tasnsp
130100060524     c                   kfld                    kar5trd
130200140404     C*
130300140404     C     Kdst          KLIST
130400140404     C                   KFLD                    Knpg
130500140404     C                   KFLD                    knFV
130600140404     C                   KFLD                    Kfgs
130700140404     c
130800140415     C     KCCOS         KLIST
130900140415     C                   KFLD                    partser
131000140423     C                   KFLD                    d55tfa
131100140404     C                   KFLD                    tasnzd
131200140404     C                   KFLD                    tascad
131300140404     C                   KFLD                    Kloc
131400140415     C                   KFLD                    kserv
131500140424     C     Kcpc          KLIST
131600140424     C                   KFLD                    p96ver
131700140424     C                   KFLD                    wcosnaz
131800140424     C                   KFLD                    wcoscap
131900140424     C     Kcpl          KLIST
132000140424     C                   KFLD                    p96ver
132100140424     C                   KFLD                    wcosnaz
132200140424     C                   KFLD                    wcoscap
132300140424     C                   KFLD                    wcosloc
132400000000     C*--------------------
132500000000     C                   Z-ADD     1             CODUT             1 0
132600000000     C*
132700000000     C***
132800000000     C* CARICA ORGANIGRAMMA
132900000000     C                   EXSR      CARORG
133000000000     C*
133100140404     C
133200140404     c* apertura file distinte di filiale
133300140508     C                   OPEN(e)   FIDST01L
133400140404     c
133500140508     c                   if        not %open(FIDST01L)
133600140508     c                   eval      %subst(WFDST:7:4)='PRD '
133700140508     c                   eval      %subst(TITAS:7:4)='GRPS'
133800140508     c                   eval      %subst(FIAR5:7:4)='GRPS'
133900140508     C                   OPEN      FIDST01L
134000140508     c                   endif
134100140409
134200140409     C                   OPEN(e)   TITAS26C
134300140409     C                   OPEN(e)   FIAR531C
134400140409     C
134500000000     C                   ENDSR
