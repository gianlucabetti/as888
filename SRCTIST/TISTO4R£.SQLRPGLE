000100000000     H*------------------------------------------------------------------------*
000200140404     H* Statistica controllo orari srervizi  - Crea Work file
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500140409     FTITAS26C  IF   E           K DISK    extfile(titas) usropn
000600140508     Ffidst01l  IF   E           K DISK    extfile(wfdst) usropn
000700140409     Ffiar531c  IF   E           K DISK    extfile(fiar5) usropn
000800150505     Ftita430c  IF   E           K DISK    extfile(tita4) usropn
000900140514     Ffnorf06l  IF   E           K DISK    usropn
001000140514     Ffnorm01l  IF   E           K DISK    usropn
001100000000     FAZORG01L  IF   E           K DISK
001200140408     FAZORG02L  IF   E           K DISK    rename(azorg:azorg002)
001300000000     FTABEL00F  IF   E           K DISK
001400140424     Fazcpc01l  IF   E           K DISK
001500140424     Fazcpl01l  IF   E           K DISK
001600140404     FWFCCOS1l  UF A E           K DISK
001700140514     FTISTO4P   O    E             PRINTER OFLIND(*IN06) usropn
001800140514     FTISTO4P1  O    E             PRINTER OFLIND(*IN07) usropn
001900140514     FTISTO4P2  O    E             PRINTER OFLIND(*IN08) usropn
002000140514     FTISTO4P3  O    E             PRINTER OFLIND(*IN09) usropn
002100000000     D*------------------------------------------------------------------------*
002200000000     D* SCHIERE
002300000000     D*------------------------------------------------------------------------*
002400000000     D*-------------------
002500140516     D* FILIALI CON RELATIVA AREA
002600000000     D*-------------------
002700000000     D FIL             S              3  0 DIM(500)                             *FILIALE
002800000000     D ARE             S              3  0 DIM(500)                             *AREA
002900051109     D dfil            S             20    DIM(500)                             *FILIALE
003000140408     D dare            S             29    DIM(500)                             *AREA
003100000000     D*-------------------
003200000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
003300051109     D CT              S              2    DIM(50)                              TIP0 BOLLA
003400140404     d
003500140404     D NTW             S              3    DIM(500)                             *NETWORK
003600140408     d
003700140408     D L6              S              3  0 DIM(30)
003800140422     d
003900140422     D or_dal          S              4  0 DIM(11)
004000000000     D*------------------------------------------------------------------------*
004100000000     D KPJBA         E DS
004200140408     d Azuteds       e ds                  Extname(Azute00f)
004300140408     d dDatiute      e ds
004400140408     d Tibs34ds      e ds                  Inz
004500140409     D azorgds       E DS                  EXTNAME(azorg00f)
004600000000     D*-------------------
004700000000     D DSTB          E DS
004800051109     D DS05          E DS
004900040220     D*------------------
005000140404     D dar5GEN       E DS
005100140404     D og143         E DS
005200140404     d
005300140404     D TRULORSDS     E DS
005400140404     D TRULOR2DS     E DS
005500140516     D TRULOR3DS     E DS
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
009600150505     d Ktrc            s                   Like(ta4trc) inz('X')
009700140404     D wNetwork        s              3    inz(*blanks)
009800140404     D kfil            s              3  0 inz
009900140404     D P1DTSER         s              8    inz
010000140404     D wperc_os        s              3  0 inz
010100140404     D wperc_om        s              3  0 inz
010200140404     D Y               s              3  0 inz
010300140408     D YY              s              3  0 inz
010400140423     D XX              s              3  0 inz
010500140408     D savlna          s              3  0 inz
010600140404     D kdcm            s              8  0 inz
010700140404     D wnorm           s              1    inz
010800140408     D wstampadet      s              1    inz
010900140404     D knfv            s                   like(dstnfv)
011000140516     D kloc            s                   like(ioreloc)
011100140408     D wfuori_os       s                   like(wcostsOS)
011200140408     D wfuori_om       s                   like(wcostsOm)
011300140415     D kserv           s                   like(wcosserv)
011400140422     d Timeiso         s               t   timfmt(*iso)
011500140422     d w0060           s              6  0
011600140422     d wperc           s              3  0
011700140505     d wcosts          s              9  0
011800140505     d tot_ts          s              9  0
011900140423     d tot_tse         s              9  0
012000140423     d tot_tsos        s              9  0
012100140423     d tot_tsom        s              9  0
012200140514     d wora            s                   like(tashmc)
012300140512     D kfar            s                   like(orffar) inz(420)
012400140512     D kdae            s                   like(orfdae) inz
012500140404     d
012600140508     d WFDST           s             21    inz('FILTRA201 /FIDST01L')
012700140409     d titas           s             21    inz('GAITRAGRU /TITAS26C')
012800150505     d tita4           s             21    inz('GAITRAGRU /TITA430C')
012900140409     d fiar5           s             21    inz('GAITRAGRU /FIAR531C')
013000000000     C*------------------------------------------------------------------------*
013100000000     C* MAIN LINE
013200000000     C*------------------------------------------------------------------------*
013300140428     C     *ENTRY        PLIST
013400140428     C                   PARM                    KPJBA
013500140428     C                   MOVEL     KPJBU         PARAM
013600140409      /free
013700140409         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013800140409      /end-free
013900140428     C* CARICA TIPI BOLLA
014000140428     C                   EXSR      CARTBL
014100140408     c*
014200140408     c* se impostata l'area leggo le filiali dell'are altrimenti la singola filiale
014300140408     c*  altrimenti tutto
014400140408     c                   select
014500140408     c                   when      parfil>0
014600140514     c* Consegne
014700140514     c                   if        partser='C'
014800140408     c* carico la sua £6
014900140408     C                   CLEAR                   trul06ds
015000140408     C                   MOVE      '£6'          D06COD
015100140408     C                   movel     parfil        D06KEY
015200140408     c                   move      'S'           d06key
015300140409     c                   if        pardsa>0
015400140408     c                   movel     pardsa        d06drf
015500140409     c                   else
015600140409     c                   movel     pardsd        d06drf
015700140409     c                   endif
015800140408     C                   MOVEL     'L'           D06TLA
015900140408     C                   MOVEL     trul06ds      KPJBU
016000140408     C*
016100140408     C                   CALL      'TRUL06R'
016200140408     C                   PARM                    KPJBA
016300140408     C                   MOVEL     KPJBU         trul06ds
016400140408     C                   MOVEA     LIN           L6
016500140423     c                   z-add     1             XX
016600140408
016700140423     c                   dow       l6(XX)>0
016800140423     c                   eval      kfil=l6(XX)
016900140408     c                   exsr      Let_TAS
017000140423     c                   add       1             XX
017100140408     c                   enddo
017200140514     c                   else
017300140514     c* Ritiri
017400140514     c                   eval      kfil=parfil
017500140514     c                   exsr      Let_ORF
017600140514     c                   endif
017700140408     c* immesso cod area
017800140408     c                   when      parcar>0
017900140409     C/EXEC SQL
018000140409     C+ Declare A1 Cursor for
018100140409     c+ SELECT * FROM azorg00f
018200140409     c+ WHERE orgcar=  :parcar AND orgfva=' ' and (orgfag='F' or orgfag='A')
018300140409     c+ ORDER BY orgfil
018400140409     C/END-EXEC
018500140409      *          apertura cursore
018600140409     C/EXEC SQL
018700140409     C+ OPEN A1
018800140409     C/END-EXEC
018900140409     C                   DOU       SqlCod <> 0
019000140409     C/EXEC SQL
019100140409     C+ Fetch Next From A1 Into :azorgds
019200140409     C/END-EXEC
019300140409     c                   select
019400140409     C                   WHEN      SqlCod = 100
019500140409     c                   leave
019600140409      *
019700140409     C                   WHEN      SqlCod < 0
019800140409     c                   seton                                        H1
019900140409     c                   leave
020000140409      *
020100140409     C                   OTHER
020200140408     c                   eval      kfil=orgfil
020300140514     c                   if        partser='C'
020400140408     c                   exsr      Let_TAS
020500140514     c                   else
020600140514     c                   exsr      Let_ORF
020700140514     c                   endif
020800140409     C                   ENDSL
020900140409
021000140408     c                   enddo
021100140409     C/EXEC SQL
021200140409     C+ Close A1
021300140409     C/END-EXEC
021400140408     c
021500140408     c                   other
021600140409
021700140408     c     *loval        setll     azorg02l
021800140408     c                   read      azorg02l
021900140408     c                   dow       not %eof(azorg02l)
022000140408     c                   eval      kfil=orgfil
022100140514     c                   if        partser='C'
022200140408     c                   exsr      Let_TAS
022300140514     c                   else
022400140514     c                   exsr      Let_ORF
022500140514     c                   endif
022600140408     c                   read      azorg02l
022700140408     c                   enddo
022800140408     c
022900140408     c                   endsl
023000140404     c*
023100140404     c                   EXSR      Stampa
023200140408     c*
023300140514     c                   if        partser='C'
023400140408     c                   write     STO4FI
023500140514     c                   else
023600140514     c                   write     STO4FI2
023700140514     c                   endif
023800140422     c
023900140422     c                   if        pardet='S'
024000140514     c                   if        partser='C'
024100140422     c                   write     STO4RI_d
024200140422     c                   write     STO4FI_d
024300140514     c                   else
024400140514     c                   write     STO4RI_d3
024500140514     c                   write     STO4FI_d3
024600140514     c                   endif
024700140422     c                   endif
024800000000     C*
024900140423     C                   CLEAR                   fnlv55ds
025000140423     C                   MOVEL     'C'           D55TLA
025100140423     C                   CALL      'FNLV55R'
025200140423     C                   PARM                    fnlv55ds
025300140423     c
025400000000     C                   SETON                                        LR
025500140408     C*------------------------------------------------------------------------*
025600140408     C* Ciclo di lettura su TITAS
025700140408     C*------------------------------------------------------------------------*
025800140408     c     LET_TAS       BEGSR
025900140409     C                   MOVEL     '0'           FINL00            1
026000140423     c* carico fil gestore
026100140423     c                   clear                   fnlv55ds
026200140423     C                   EVAL      D55TPT='6'
026300140423     c                   eval      d55lin=kfil
026400140423     c                   if        pardsa>0
026500140423     c                   eval      d55drf=pardsa
026600140423     c                   else
026700140423     c                   eval      d55drf=pardsd
026800140423     c                   endif
026900140423     c
027000140423     c                   call      'FNLV55R'
027100140423     c                   parm                    fnlv55ds
027200140408     c
027300140408     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
027400140408     C     FINL00        IFEQ      '0'                                          --- 1 -->
027500140408     C*
027600140408     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
027700140408     C                   EXSR      SETFIL
027800140408     C*
027900140408     C* CICLO FINO A FINE FILE
028000140408     C     FINL00        DOWEQ     '0'                                          --- 2 -->
028100140408     C*
028200140408     C* CREA IL RECORD SUI FILE DI LAVORO
028300140408     C                   EXSR      CRErec
028400140408     C*
028500140408     C* LEGGE IL RECORD SUCCESSIVO
028600140408     C                   EXSR      LETFIL
028700140408     C                   ENDDO                                                  <-- 2 ---
028800140408     C*
028900140408     C                   ENDIF                                                  <-- 1 ---
029000140408     c                   ENDSR
029100000000     C*------------------------------------------------------------------------*
029200000000     C* SETFIL - POSIZIONAMENTO SUL FILE
029300000000     C*------------------------------------------------------------------------*
029400000000     C     SETFIL        BEGSR
029500000000     C*
029600000000     C* POSIZIONAMENTO E PRIMA LETTURA
029700000000     C*
029800140404     C                   movel     pardsd        Kdcm
029900991123     C*
030000140404     C     Ktas          SETLL     TITAS26C                           99        *FINE FILE
030100000000     C*
030200000000     C     *IN99         IFEQ      '1'
030300000000     C                   MOVEL     '1'           FINL00
030400000000     C                   ELSE
030500000000     C                   EXSR      LETFIL
030600000000     C                   END
030700000000     C*
030800000000     C                   ENDSR
030900140404     C*------------------------------------------------------------------------*
031000000000     C* LETFIL - LETTURA PROSSIMO RECORD
031100000000     C*------------------------------------------------------------------------*
031200000000     C     LETFIL        BEGSR
031300000000     C*
031400000000     C* LEGGO FINO A:
031500000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
031600000000     C*     - TROVATO RECORD VALIDO
031700000000     C                   MOVEL     'N'           WRECOK
031800140404    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
031900000000     C     WRECOK        ANDEQ     'N'
032000000000     C*
032100140404     C*
032200140404    2c                   if        kfil>0
032300140404     C     KTAS          READE     TITAS26C                               99
032400140404     c                   else
032500140404     C     Kdcm          READE     TITAS26C                               99
032600140404    2c                   endif
032700140404     c
032800140404    2C                   IF        *IN99
032900140404     C     KDCM          SETGT     TITAS26C
033000140404     C                   READ      TITAS26C                               99
033100140404    3C                   IF        *IN99  OR TASDCM>PARDSA
033200140404     C                   MOVEL     '1'           FINL00
033300140404     C                   ELSE
033400140404     C                   EVAL      KDCM=TASDCM
033500140404     C     KTAS          setll     TITAS26C
033600140404    3C                   ENDIF
033700140404
033800140404   x2C                   ELSE
033900140404     c
034000140404     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
034100140404    2C                   END                                                    <-- 1 ---
034200140404     C*
034300140404    1C                   ENDDO                                                  <-- 1 ---
034400000000     C*
034500000000     C                   ENDSR
034600000000     C*------------------------------------------------------------------------*
034700000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
034800000000     C*------------------------------------------------------------------------*
034900000000     C     CHKREC        BEGSR
035000000000     C*
035100000000     C                   MOVEL     'S'           WRECOK            1
035200040305     C*
035300140414     c*
035400051109     C* ESCLUDE LE BOLLE di recupero
035500051109     C                   Z-ADD     1             i
035600000000     C                   SETOFF                                           28
035700051109     C     TASTBL        LOOKUP    TB(i)                                  28
035800051109     C                   IF        *in28 = *on
035900000000     C                   MOVEL     'N'           WRECOK
036000000000     C                   ENDIF
036100140404     c
036200140414     c* Escludo bolle export e le bolle con consegna anomala
036300140414     c                   if        tasnzd<>*blanks or tascca<>' '
036400140408     C                   MOVEL     'N'           WRECOK
036500140408     c                   endif
036600140414     c
036700140411     c* Escludo le bolle in fermo deposito
036800140411     c                   if        tasffd='S'
036900140411     C                   MOVEL     'N'           WRECOK
037000140411     c                   endif
037100011112     C*
037200000000     C                   ENDSR
037300140408     C*------------------------------------------------------------------------*
037400140408     C* controllo se si tratta di bolla da elaborare
037500140408     C*------------------------------------------------------------------------*
037600140408     C     NOELAB        BEGSR
037700140408     C*
037800140408     C                   MOVEL     'S'           Welab             1
037900140408     C*
038000140408     c* Cerco se si tratta di distinta PDA
038100140508     c* solo in AS888
038200140508    1c                   if        %subst(knsif:7:1)='P'
038300140508     c                   movel     taspdc        kfgs              3 0
038400140508     c                   movel     4             knpg              1 0
038500140508     c                   z-add     tasndc        knfv
038600140508     c     kdst          chain     fidst01l
038700140508    2c                   if        %found(fidst01l)
038800140508     c                             and (dstpda='C' or dstpda='E')
038900140508   x2c                   else
039000140508     C                   MOVEL     'N'           Welab
039100140508    2c                   endif
039200140508     c
039300140508   x1c                   else
039400140508   c
039500140508     c* Solo dall'aprile 2014 abbiamo le bolle con il flag impostato
039600140508     c                   if        tasaas<2014
039700140508     C                   MOVEL     'N'           Welab
039800140508     c                   endif
039900140508     c
040000140508     c                   if        tasaas=2014 and tasmgs<0401
040100140508     C                   MOVEL     'N'           Welab
040200140508     c                   endif
040300140408     c* chaino record GEN
040400140408     c     kar5          chain     fiar531c
040500140408     c                   if        %found(fiar531c)
040600140408     c                   eval      dar5gen=ar5uni
040700140408     c                   if        §AR5PDACO<>'  '
040800140408     C                   MOVEL     'N'           Welab
040900140408     c                   endif
041000140408     c                   endif
041100140508     c
041200140508    1C                   ENDIF
041300140408     c
041400140411     c* Non elaboro se impostata ora consegna richiesta
041500140411     c                   if        tashcr>0
041600140408     C                   MOVEL     'N'           Welab
041700140408     c                   endif
041800140408     c
041900140408     C                   ENDSR
042000000000     C*------------------------------------------------------------------------*
042100140408     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO
042200000000     C*------------------------------------------------------------------------*
042300140408     C     CRErec        BEGSR
042400140516     c
042500140404     c                   clear                   kloc
042600140404     c
042700140404     c* Richiamo pgm reperimento orari servizi
042800140404     c                   clear                   trulorsds
042900140404     c                   clear                   trulor2ds
043000140516     c                   clear                   trulor3ds
043100140404     c                   eval      ioredta=pardsd
043200140408     c                   eval      iorefil=taslna
043300140404     c                   eval      iorenar=tasnzd
043400140404     c                   eval      iorecap=tascad
043500140516     c                   eval      ioreloc=taslod
043600150505     c* Verifico se presente località normalizzata e la passo
043700150505     c     kta4          chain     tita430c
043800150505     c                   if        %found(tita430c)
043900150505     c                   eval      OOR3LOC_N=ta4not
044000150505     c                   eval      OOR3norm ='S'
044100150505     c                   else
044200150505     c                   eval      oor3loc_N=taslod
044300150505     c                   endif
044400140414     c                   eval      ioreddc=tasddc
044500140414     c                   eval      iorendc=tasndc
044600140414     c                   eval      ioretfp=tastfp
044700140414     c                   eval      ioretfa=tastfa
044800140414     c                   eval      ioredti=tasdti
044900140428     c                   eval      iorehti=9999
045000140404     c                   eval      ioretser=partser
045100140515     c* si forza sempre "C" perché per "H" e "E"  l'orario viene accorciato
045200140515     c* automaticamente e non è l'obiettivo della statistica
045300140515     c                   eval      ioretsp ='C'
045400140404     c                   call      'TRULORSR'
045500140404     c                   parm                    kpjba
045600140404     c                   parm                    trulorsds
045700140404     c                   parm                    trulor2ds
045800140516     c                   parm                    trulor3ds
045900140516     c                   eval      wnorm=OOR3NORM
046000140516     c
046100140516     c                   if        oor3norm='S'
046200140516     c                   eval      kloc =OOR3LOC_N
046300150717     c                   else
046400150717     c* verifico da CPL se era già normalizzata
046500150717     c     kcpl2         chain     azcpl01l
046600150717     c                   if        %found(azcpl01l)
046700150717     c                   eval      kloc =taslod
046800150717     c                   eval      OOR3norm ='S'
046900140516     c                   endif
047000150717     c                   endif
047100140516     c*
047200140404     c*
047300140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
047400140415     c                   if        oor2fcsgl='P'
047500140415     c                   eval      kserv='P '
047600140415     c                   else
047700140415     c                   clear                   kserv
047800140415     c                   endif
047900140415
048000140415     c     kccos         chain     wfccos1l
048100140404     c                   if        not %found(wfccos1l)
048200140404     c                   clear                   wfccos00
048300140404     c                   eval      wcosdcmd=pardsd
048400140404     c                   eval      wcosdcma=pardsa
048500140404     c                   eval      wcosver =p96ver
048600140411     c                   eval      wcoslna =d55tfa
048700140404     c                   eval      wcosnaz =tasnzd
048800140404     c                   eval      wcoscap =tascad
048900140404     c                   eval      wcosloc =kloc
049000140404     c                   eval      wcostser=partser
049100140404     c                   eval      wcosorif=OOR2ORIF
049200140415     c                   eval      wcosserv=kserv
049300140424     c
049400140424     c* obsoleto
049500140514     c                   exsr      sr_impobs
049600140424     c* Imposto gli orari nel file
049700140424     c                   eval      WCOSOSTIS=OOR2STIS
049800140424     c                   eval      WCOSOSTfS=OOR2STfS
049900140424     c                   eval      WCOSOmiis=OOR2miis
050000140424     c                   eval      WCOSOmxfs=OOR2mxfs
050100140424     c
050200140415     c* imposto orari dettaglio se ci sono e richiestoc
050300140415     c                   if        pardet='S'
050400140514     c                   exsr      sr_ImpOrDet
050500140404     c                   endif
050600140415     c                   endif
050700140404     c
050800140404     c                   add       1             wcosts
050900140404     c* Se loc non normalizzata
051000140516     c                   if        wnorm<>'S'
051100140404     c                   add       1             wcostsNN
051200140404     c                   endif
051300140408     c
051400140408     c* Controllo se spedizione da elaborare
051500140408     c                   exsr      Noelab
051600140408     c
051700140404     c* elaborate
051800140408     c                   if        welab='S'
051900140404     c                   add       1             wcostse
052000140404     c
052100140404     c* consegna negli orari std
052200140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
052300140404     c                   add       1             wcostsos
052400140404     c                   endif
052500140404     c* consegna negli orari min/max
052600140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
052700140404     c                   add       1             wcostsom
052800140404     c                   endif
052900140415     c* imposto campi dettaglio
053000140514     c                   z-add     tashmc        wora
053100140514     c                   exsr      ImpoDETUni
053200140408     c                   endif
053300140404     c
053400140404     c                   if        not %found(wfccos1l)
053500140404     c                   write     wfccos00
053600140404     c                   else
053700140404     c                   update    wfccos00
053800140404     c                   endif
053900000000     C*
054000000000     C                   ENDSR
054100140404     C*------------------------------------------------------------------------*
054200140404     C* Stampa - statistica
054300140404     C*------------------------------------------------------------------------*
054400140404     c     STAMPA        BEGSR
054500140404     c* Tpo servizio
054600140404     c                   if        partser='C'
054700140404     c                   eval      p1dtser='CONSEGNE'
054800140404     C                   ELSE
054900140408     c                   eval      p1dtser=' RITIRI '
055000140404     C                   ENDIF
055100140408     c* giro le date
055200140409     C                   MOVE      pardsd        G02inv
055300140408     C                   MOVEL     '3'           G02ERR
055400140408     C                   CALL      'XSRDA8'
055500140408     C                   PARM                    WLBDAT
055600140408     c                   z-add     g02dat        p1ddsd
055700140408     c*
055800140408     c                   if        pardsa>0
055900140409     C                   MOVE      pardsa        G02inv
056000140408     C                   MOVEL     '3'           G02ERR
056100140408     C                   CALL      'XSRDA8'
056200140408     C                   PARM                    WLBDAT
056300140408     c                   z-add     g02dat        p1ddsa
056400140408     c                   endif
056500140408     c* Orari per calcolo %
056600140408     c                   if        parorari='S'
056700140408     c                   eval      p1dorari='STANDARD'
056800140424     c                   eval      p1dtor_d='Standard'
056900140408     c                   else
057000140408     c                   eval      p1dorari='MINIMO/MASSIMO'
057100140424     c                   eval      p1dtor_d=' Min/Max'
057200140408     c                   endif
057300140408     c
057400140514     c                   if        partser='C'
057500140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
057600140408     c                   if        parcor='C'
057700140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
057800140411     c                   eval      p1dbolstd='   Nei limiti STD   '
057900140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
058000140408     C                   ELSE
058100140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
058200140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
058300140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
058400140408     C                   ENDIF
058500140514     c                   else
058600140514     c                   eval      p1dbolstd='  Fuori limiti STD  '
058700140514     c                   endif
058800140423     c
058900140514     c                   if        partser='C'
059000140423     c                   write     STO4TE
059100140514     c                   else
059200140514     c                   write     STO4TE2
059300140514     c                   endif
059400140423
059500140423     c* Orari nel dettaglio
059600140423     c                   if        pardet='S'
059700140423     c                   exsr      TestaDET
059800140514     c                   if        partser='C'
059900140423     c                   write     STO4TE_d
060000140514     c                   else
060100140514     c                   write     STO4TE_d3
060200140514     c                   endif
060300140423     c                   endif
060400140404     c
060500140404     C
060600140424     c*
060700140514     c     partser       setll     wfccos1l
060800140514     c     partser       reade     wfccos1l
060900140404     c                   dow       not %eof(wfccos1l)
061000140404     c
061100140408     c* a cambio filiale stampo a pagina nuova
061200140408     c                   if        wcoslna<>savlna
061300140408
061400140408     c                   if        savlna>0
061500140423     c                   exsr      Totfil
061600140423     c
061700140514     c                   if        partser='C'
061800140408     c                   write     STO4TE
061900140514     c                   else
062000140514     c                   write     STO4TE2
062100140514     c                   endif
062200140422     c
062300140422     c                   if        pardet='S'
062400140514     c                   if        partser='C'
062500140422     c                   write     STO4RI_d
062600140422     c                   write     STO4TE_d
062700140514     c                   else
062800140514     c                   write     STO4RI_d3
062900140514     c                   write     STO4TE_d3
063000140514     c                   endif
063100140408     c                   endif
063200140423     c
063300140423     c                   clear                   tot_ts
063400140423     c                   clear                   tot_tse
063500140423     c                   clear                   tot_tsos
063600140423     c                   clear                   tot_tsom
063700140422     c                   endif
063800140408
063900140408     c                   z-add     1             i
064000140408     C     wcoslna       lookup    fil(i)                                 55
064100140404     C                   if        %found
064200140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
064300140408     c                   eval      p1dare=dare(i)
064400140404     C                   else
064500140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
064600140408     c                   eval      p1dare=*all'?'
064700140404     C                   endif
064800140408
064900140408     c                   eval      savlna=wcoslna
065000140408
065100140514     c                   if        partser='C'
065200140408     c                   write     STO4TEF
065300140414     c                   write     STO4RI
065400140514     c                   else
065500140514     c                   write     STO4TEF2
065600140514     c                   write     STO4RI2
065700140514     c                   endif
065800140422     c                   if        pardet='S'
065900140514     c                   if        partser='C'
066000140422     c                   write     STO4TEF_d
066100140422     c                   write     STO4RI_d
066200140514     c                   else
066300140514     c                   write     STO4TEF_d3
066400140514     c                   write     STO4RI_d3
066500140514     c                   endif
066600140408     c                   endif
066700140422     c                   endif
066800140408
066900140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
067000140411     c                   if        wcostse>=parnum and wcostse>0
067100140408     c* voglio la % di correttezza
067200140408     c                   if        parcor='C'
067300140404     c* Calcolo % STD
067400140619     c                   if        wcostsos>0
067500140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
067600140408     c                   eval      p1dstsos=WCOSTSOs
067700140619     c                   else
067800140619     c                   clear                   p1dperc_os
067900140619     c                   clear                   p1dstsos
068000140619     c                   endif
068100140404     c* Calcolo % MIN/MAX
068200140619     c                   if        wcostsom>0
068300140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
068400140408     c                   eval      p1dstsom=WCOSTSOm
068500140619     c                   else
068600140619     c                   clear                   p1dperc_om
068700140619     c                   clear                   p1dstsom
068800140619     c                   endif
068900140408     c
069000140408     c                   else
069100140408     c* voglio la % delle fuori orario
069200140408     c                   eval      wfuori_os=wcostse-wcostsos
069300140619     c                   if        wfuori_OS>0
069400140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
069500140408     c                   eval      p1dstsos=Wfuori_os
069600140619     c                   else
069700140619     c                   clear                   p1dperc_os
069800140619     c                   clear                   p1dstsos
069900140619     c                   endif
070000140408
070100140408     c                   eval      wfuori_om=wcostse-wcostsom
070200140619     c                   if        wfuori_OM>0
070300140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
070400140408     c                   eval      p1dstsom=Wfuori_om
070500140619     c                   else
070600140619     c                   clear                   p1dperc_om
070700140619     c                   clear                   p1dstsom
070800140619     c                   endif
070900140408     c                   endif
071000140408     c
071100140407     c* stampo solo al di sotto della % richiesta
071200140408     c                   eval      wstampadet='S'
071300140408
071400140408     c                   select
071500140408     c                   when      parperc=0
071600140408
071700140408     c                   when      parorari='S'  and parcor='C' and
071800140408     c                             p1dperc_os>=parperc
071900140408     c                   eval      wstampadet='N'
072000140408     c
072100140408     c                   when      parorari='S'  and parcor='F' and
072200140408     c                             p1dperc_os< parperc
072300140408     c                   eval      wstampadet='N'
072400140408
072500140408     c                   when      parorari='M'  and parcor='C' and
072600140408     c                             p1dperc_om>=parperc
072700140408     c                   eval      wstampadet='N'
072800140408     c
072900140408     c                   when      parorari='M'  and parcor='F' and
073000140408     c                             p1dperc_om< parperc
073100140408     c                   eval      wstampadet='N'
073200140408     c                   endsl
073300140404     c*
073400140408     c                   if        wstampadet='S'
073500140424     c* POM
073600140424     c                   if        wcosserv='P '
073700140424     c                   eval      p1dpom='Pom'
073800140424     c                   else
073900140424     c                   eval      p1dpom='   '
074000140424     c                   endif
074100140411     c*
074200140411     c* Se la località è vuota imposto '??'
074300140411     c                   if        wcosloc=*blanks
074400140411     c                   eval      wcosloc='??'
074500140411     c                   endif
074600140408     c
074700140408     c                   if        *in06
074800140408     c                   write     sto4te
074900140408     c                   write     STO4TEF
075000140414     c                   write     STO4RI
075100140408     c                   setoff                                       06
075200140404     c                   endif
075300140514
075400140514     c                   if        *in08
075500140514     c                   write     sto4te2
075600140514     c                   write     STO4TEF2
075700140514     c                   write     STO4RI2
075800140514     c                   setoff                                       08
075900140514     c                   endif
076000140408     c
076100140424     c                   eval      p1dstis=wcosoSTiS
076200140424     c                   eval      p1dstfs=wcosoSTFS
076300140424     c                   eval      p1dmiis=wcosomiis
076400140424     c                   eval      p1dmxfs=wcosomxfs
076500140404     c
076600140514     c                   if        partser='C'
076700140408     c                   write     STO4det
076800140514     c                   else
076900140514     c                   write     STO4det2
077000140514     c                   endif
077100140422     c
077200140422     c* se devo stampare anche il dettaglio
077300140422     c                   if        pardet='S'
077400140422     c                   exsr      Stadet
077500140422     c                   endif
077600140408     c
077700140407     c                   endif
077800140408     c                   endif
077900140424     c* totali di filiale
078000140424     c                   add       WCOSTS        tot_ts
078100140424     c                   add       WCOSTSE       tot_tse
078200140424     c                   add       WCOSTSos      tot_tsos
078300140424     c                   add       WCOSTSom      tot_tsom
078400140407     c
078500140514     c     partser       reade     wfccos1l
078600140404     c
078700140404     c                   enddo
078800140423     c
078900140423     c* totali finali
079000140423     c                   if        savlna>0
079100140423     c                   exsr      totfil
079200140423     c                   endif
079300140404     c
079400140404     c                   ENDSR
079500140423     C*------------------------------------------------------------------------*
079600140423     c* Imposta gli orari in testata dettaglioù
079700140423     C*------------------------------------------------------------------------*
079800140423     c     TestaDet      BEGSR
079900140423     c                   eval      p1dsot = paror1
080000140423     c                   eval      p1doltre= paroltre
080100140423     c                   eval      p1dda1  = or_dal(1)
080200140423
080300140423     c                   if        or_dal(2)>0
080400140423     c                   eval      p1dda2  = or_dal(2)
080500140423     c                   movel     or_dal(2)     w0060
080600140423     c                   move      w0060         timeiso
080700140423     c                   subdur    1:*mn         timeiso
080800140423     c                   eval      w0060     =%dec(timeiso)
080900140423     c                   movel     w0060         p1dal1
081000140423     c                   else
081100140423     c                   eval      p1dal1=paroltre
081200140423     c                   leavesr
081300140423     c                   endif
081400140423     c
081500140423     c                   if        or_dal(3)>0
081600140423     c                   eval      p1dda3  = or_dal(3)
081700140423     c                   movel     or_dal(3)     w0060
081800140423     c                   move      w0060         timeiso
081900140423     c                   subdur    1:*mn         timeiso
082000140423     c                   eval      w0060     =%dec(timeiso)
082100140423     c                   movel     w0060         p1dal2
082200140423     c                   else
082300140423     c                   eval      p1dal2=paroltre
082400140423     c                   leavesr
082500140423     c                   endif
082600140423
082700140423     c                   if        or_dal(4)>0
082800140423     c                   eval      p1dda4  = or_dal(4)
082900140423     c                   movel     or_dal(4)     w0060
083000140423     c                   move      w0060         timeiso
083100140423     c                   subdur    1:*mn         timeiso
083200140423     c                   eval      w0060     =%dec(timeiso)
083300140423     c                   movel     w0060         p1dal3
083400140423     c                   else
083500140423     c                   eval      p1dal3=paroltre
083600140423     c                   leavesr
083700140423     c                   endif
083800140423
083900140423     c                   if        or_dal(5)>0
084000140423     c                   eval      p1dda5  = or_dal(5)
084100140423     c                   movel     or_dal(5)     w0060
084200140423     c                   move      w0060         timeiso
084300140423     c                   subdur    1:*mn         timeiso
084400140423     c                   eval      w0060     =%dec(timeiso)
084500140423     c                   movel     w0060         p1dal4
084600140423     c                   else
084700140423     c                   eval      p1dal4=paroltre
084800140423     c                   leavesr
084900140423     c                   endif
085000140423
085100140423     c                   if        or_dal(6)>0
085200140423     c                   eval      p1dda6  = or_dal(6)
085300140423     c                   movel     or_dal(6)     w0060
085400140423     c                   move      w0060         timeiso
085500140423     c                   subdur    1:*mn         timeiso
085600140423     c                   eval      w0060     =%dec(timeiso)
085700140423     c                   movel     w0060         p1dal5
085800140423     c                   else
085900140423     c                   eval      p1dal5=paroltre
086000140423     c                   leavesr
086100140423     c                   endif
086200140423
086300140423     c                   if        or_dal(7)>0
086400140423     c                   eval      p1dda7  = or_dal(7)
086500140423     c                   movel     or_dal(7)     w0060
086600140423     c                   move      w0060         timeiso
086700140423     c                   subdur    1:*mn         timeiso
086800140423     c                   eval      w0060     =%dec(timeiso)
086900140423     c                   movel     w0060         p1dal6
087000140423     c                   else
087100140423     c                   eval      p1dal6=paroltre
087200140423     c                   leavesr
087300140423     c                   endif
087400140423
087500140423     c                   if        or_dal(8)>0
087600140423     c                   eval      p1dda8  = or_dal(8)
087700140423     c                   movel     or_dal(8)     w0060
087800140423     c                   move      w0060         timeiso
087900140423     c                   subdur    1:*mn         timeiso
088000140423     c                   eval      w0060     =%dec(timeiso)
088100140423     c                   movel     w0060         p1dal7
088200140423     c                   else
088300140423     c                   eval      p1dal7=paroltre
088400140423     c                   leavesr
088500140423     c                   endif
088600140423     c                   if        or_dal(9)>0
088700140423     c                   eval      p1dda9  = or_dal(9)
088800140423     c                   movel     or_dal(9)     w0060
088900140423     c                   move      w0060         timeiso
089000140423     c                   subdur    1:*mn         timeiso
089100140423     c                   eval      w0060     =%dec(timeiso)
089200140423     c                   movel     w0060         p1dal8
089300140423     c
089400140423     c                   eval      p1dal9=paroltre
089500140423     c                   else
089600140423     c                   eval      p1dal8=paroltre
089700140423     c                   leavesr
089800140423     c                   endif
089900140423     c
090000140423     c                   ENDSR
090100140422     C*------------------------------------------------------------------------*
090200140422     C* Stampa dettaglio
090300140422     C*------------------------------------------------------------------------*
090400140422     C     StaDET        BEGSR
090500140422     c                   if        *in07
090600140422     c                   write     sto4te_d
090700140422     c                   write     STO4TEF_d
090800140422     c                   write     STO4RI_d
090900140422     c                   setoff                                       07
091000140422     c                   endif
091100140514
091200140514     c                   if        *in09
091300140514     c                   write     sto4te_d3
091400140514     c                   write     STO4TEF_d3
091500140514     c                   write     STO4RI_d3
091600140514     c                   setoff                                       09
091700140514     c                   endif
091800140424     c* Imposto gli orari
091900140424     c                   if        parorari='S'
092000140424     c                   eval      p1dor_dal=WCOSOSTIS
092100140424     c                   eval      p1dor_al =WCOSOSTfS
092200140424     c                   else
092300140424     c                   eval      p1dor_dal=WCOSOmiis
092400140424     c                   eval      p1dor_al =WCOSOmxfS
092500140424     c                   endif
092600140424     c                   setoff                                       111213
092700140424     c                   setoff                                       141516
092800140424     c                   setoff                                       171819
092900140505     c                   setoff                                       2021
093000140422     c* calcolo le %
093100140422     c* SOTTO
093200140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
093300140422     C                   IF        WPERC>0
093400140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
093500140505     c                   seton                                        20
093600140422     c                   else
093700140424     c                   eval      p1cp_sot='    '
093800140422     c                   endif
093900140422     c* OLTRE
094000140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
094100140422     C                   IF        WPERC>0
094200140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
094300140505     c                   seton                                        21
094400140422     c                   else
094500140424     c                   eval      p1cp_olt='    '
094600140422     c                   endif
094700140422     c* % 1
094800140505    1c                   if        wcosor1  >0
094900140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
095000140422     c
095100140505    2C                   IF        WPERC>0
095200140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
095300140505     c
095400140505    2c                   select
095500140505     c                   when      or_dal(1)<p1dor_dal
095600140505     c                   seton                                        11
095700140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
095800140505     c                   seton                                        11
095900140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
096000140505     c                   seton                                        11
096100140505    2c                   endsl
096200140505
096300140422     c                   else
096400140424     c                   eval      p1cp_o1 ='    '
096500140505    2c                   endif
096600140424     c
096700140505    1c                   endif
096800140422     c
096900140422     c* % 2
097000140422     c                   if        wcosor2  >0
097100140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
097200140422     c
097300140422     C                   IF        WPERC>0
097400140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
097500140505
097600140505     c                   select
097700140505     c                   when      or_dal(2)<p1dor_dal
097800140505     c                   seton                                        12
097900140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
098000140505     c                   seton                                        12
098100140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
098200140505     c                   seton                                        12
098300140505     c                   endsl
098400140505
098500140422     c                   else
098600140424     c                   eval      p1cp_o2 ='    '
098700140422     c                   endif
098800140422     c                   endif
098900140423     c* % 3
099000140423     c                   if        wcosor3  >0
099100140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
099200140423     c
099300140423     C                   IF        WPERC>0
099400140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
099500140505
099600140505     c                   select
099700140505     c                   when      or_dal(3)<p1dor_dal
099800140505     c                   seton                                        13
099900140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
100000140505     c                   seton                                        13
100100140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
100200140505     c                   seton                                        13
100300140505     c                   endsl
100400140423     c                   else
100500140424     c                   eval      p1cp_o3 ='    '
100600140423     c                   endif
100700140423     c                   endif
100800140423     c* % 4
100900140423     c                   if        wcosor4  >0
101000140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
101100140423     c
101200140423     C                   IF        WPERC>0
101300140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
101400140505
101500140505     c                   select
101600140505     c                   when      or_dal(4)<p1dor_dal
101700140505     c                   seton                                        14
101800140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
101900140505     c                   seton                                        14
102000140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
102100140505     c                   seton                                        14
102200140505     c                   endsl
102300140423     c                   else
102400140424     c                   eval      p1cp_o4 ='    '
102500140423     c                   endif
102600140423     c                   endif
102700140423     c* % 5
102800140423     c                   if        wcosor5  >0
102900140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
103000140423     c
103100140423     C                   IF        WPERC>0
103200140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
103300140505
103400140505     c                   select
103500140505     c                   when      or_dal(5)<p1dor_dal
103600140505     c                   seton                                        15
103700140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
103800140505     c                   seton                                        15
103900140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
104000140505     c                   seton                                        15
104100140505     c                   endsl
104200140423     c                   else
104300140424     c                   eval      p1cp_o5 ='    '
104400140423     c                   endif
104500140423     c                   endif
104600140423     c* % 6
104700140423     c                   if        wcosor6  >0
104800140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
104900140423     c
105000140423     C                   IF        WPERC>0
105100140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
105200140505
105300140505     c                   select
105400140505     c                   when      or_dal(6)<p1dor_dal
105500140505     c                   seton                                        16
105600140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
105700140505     c                   seton                                        16
105800140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
105900140505     c                   seton                                        16
106000140505     c                   endsl
106100140423     c                   else
106200140424     c                   eval      p1cp_o6 ='    '
106300140423     c                   endif
106400140423     c                   endif
106500140423     c* % 7
106600140423     c                   if        wcosor7  >0
106700140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
106800140423     c
106900140423     C                   IF        WPERC>0
107000140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
107100140505
107200140505     c                   select
107300140505     c                   when      or_dal(7)<p1dor_dal
107400140505     c                   seton                                        17
107500140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
107600140505     c                   seton                                        17
107700140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
107800140505     c                   seton                                        17
107900140505     c                   endsl
108000140423     c                   else
108100140424     c                   eval      p1cp_o7 ='    '
108200140423     c                   endif
108300140423     c                   endif
108400140423     c* % 8
108500140423     c                   if        wcosor8  >0
108600140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
108700140423     c
108800140423     C                   IF        WPERC>0
108900140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
109000140505
109100140505     c                   select
109200140505     c                   when      or_dal(8)<p1dor_dal
109300140505     c                   seton                                        18
109400140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
109500140505     c                   seton                                        18
109600140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
109700140505     c                   seton                                        18
109800140505     c                   endsl
109900140423     c                   else
110000140424     c                   eval      p1cp_o8 ='    '
110100140423     c                   endif
110200140423     c                   endif
110300140423     c* % 9
110400140423     c                   if        wcosor9  >0
110500140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
110600140423     c
110700140423     C                   IF        WPERC>0
110800140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
110900140505
111000140505     c                   select
111100140505     c                   when      or_dal(9)<p1dor_dal
111200140505     c                   seton                                        19
111300140505     c                   when      paroltre >p1dor_al
111400140505     c                   seton                                        19
111500140505     c                   endsl
111600140423     c                   else
111700140424     c                   eval      p1cp_o9 ='    '
111800140423     c                   endif
111900140423     c                   endif
112000140422     c
112100140514     c                   if        partser='C'
112200140422     c                   write     STO4det_d
112300140514     c                   else
112400140514     c                   write     STO4det_d3
112500140514     c                   endif
112600140422     c                   ENDSR
112700140423     C*------------------------------------------------------------------------*
112800140423     c* Stampa totali di filiale
112900140423     C*------------------------------------------------------------------------*
113000140423     c     TOTfil        BEGSR
113100140423     c* voglio la % di correttezza
113200140423     c                   if        parcor='C'
113300140423     c* Calcolo % STD
113400140619     c                   if        tot_tsos>0
113500140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
113600140423     c                   eval      p1dstsos=tot_TSOs
113700140619     c                   else
113800140619     c                   clear                   p1dperc_os
113900140619     c                   clear                   p1dstsos
114000140619     c                   endif
114100140423     c* Calcolo % MIN/MAX
114200140619     c                   if        tot_tsom>0
114300140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
114400140423     c                   eval      p1dstsom=tot_TSOm
114500140619     c                   else
114600140619     c                   clear                   p1dperc_om
114700140619     c                   clear                   p1dstsom
114800140619     c                   endif
114900140423     c
115000140423     c                   else
115100140423     c* voglio la % delle fuori orario
115200140423     c                   eval      wfuori_os=tot_tse-tot_tsos
115300140619     c                   if        wfuori_os>0
115400140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
115500140423     c                   eval      p1dstsos=Wfuori_os
115600140619     c                   else
115700140619     c                   clear                   p1dperc_os
115800140619     c                   clear                   p1dstsos
115900140619     c                   endif
116000140423
116100140423     c                   eval      wfuori_om=tot_tse-tot_tsom
116200140619     c                   if        wfuori_om>0
116300140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
116400140423     c                   eval      p1dstsom=Wfuori_om
116500140619     c                   else
116600140619     c                   clear                   p1dperc_om
116700140619     c                   clear                   p1dstsom
116800140619     c                   endif
116900140423     c
117000140423     c                   endif
117100140423
117200140514     c                   if        partser='C'
117300140423     c                   if        *in06
117400140423     c                   write     sto4te
117500140423     c                   write     STO4TEF
117600140423     c                   write     STO4RI
117700140423     c                   setoff                                       06
117800140423     c                   else
117900140423     c                   write     STO4RI
118000140930     c                   endif
118100140930     c
118200140423     c                   write     STO4TOT
118300140930     c                   write     STO4RI
118400140514     c                   else
118500140514     c                   if        *in08
118600140514     c                   write     sto4te2
118700140514     c                   write     STO4TEF2
118800140514     c                   write     STO4RI2
118900140514     c                   setoff                                       08
119000140514     c                   else
119100140514     c                   write     STO4RI2
119200140930     c                   endif
119300140514     c                   write     STO4TOT2
119400140930     c                   write     STO4RI2
119500140514     c                   endif
119600140423     c                   ENDSR
119700000000     C*------------------------------------------------------------------------*
119800000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
119900000000     C*------------------------------------------------------------------------*
120000000000     C     CARORG        BEGSR
120100000000     C*
120200140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
120300000000     C  N99              READ      AZORG01L                               99
120400000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
120500000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
120600000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
120700000000     C     ORGFAG        IFNE      'V'
120800051109     C                   ADD       1             I
120900000000     C                   Z-ADD     ORGFIL        FIL(I)
121000000000     C                   Z-ADD     ORGCAR        ARE(I)
121100140404     C* CALCOLA NETWORK
121200140404     C                   MOVEL     ORGDE3        OG143
121300140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
121400051109     c* area
121500051109     C                   Z-ADD     CODUT         KTBUTE
121600051109     C                   MOVEL     '05'          Ktbcod
121700051109     C                   MOVEL(P)  ORGCAR        KKEY
121800051109     C     KTAB          CHAIN     TABEL00F                           30
121900051109    2C     *IN30         IFEQ      *OFF
122000051109     C     TBLFLG        ANDEQ     *BLANKS
122100051109     C                   MOVEL     TBLUNI        DS05
122200140408     C***                MOVEL     §05DES        dare(I)
122300140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
122400051109     C                   ELSE
122500140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
122600140408     C***                MOVE      *BLANKS       dare(I)
122700051109    2C                   END
122800051109     c* p.o.
122900051109     C                   MOVEl     orgdes        dfil(I)
123000051109     c
123100000000     C                   END
123200000000     C                   END                                                    <-- 2 ---
123300000000     C                   READ      AZORG01L                               99
123400000000     C                   END                                                    <-- 1 ---
123500000000     C*
123600000000     C                   ENDSR
123700000000     C*------------------------------------------------------------------------*
123800051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
123900000000     C*------------------------------------------------------------------------*
124000000000     C     CARTBL        BEGSR
124100140516     C
124200140516     c* apertura file distinte di filiale
124300140516     c                   if        partser='C'
124400140516     C                   OPEN(e)   FIDST01L
124500140516     c
124600140516     c                   if        not %open(FIDST01L)
124700140516     c                   eval      %subst(WFDST:7:4)='PRD '
124800140516     c                   eval      %subst(TITAS:7:4)='GRPS'
124900150505     c                   eval      %subst(TITA4:7:4)='GRPS'
125000140516     c                   eval      %subst(FIAR5:7:4)='GRPS'
125100140516     C                   OPEN      FIDST01L
125200140516     c                   endif
125300140516
125400140516     C                   OPEN(e)   TITAS26C
125500150505     C                   OPEN(e)   TITA430C
125600140516     C                   OPEN(e)   FIAR531C
125700140516     c                   open      tisto4p
125800140516     c                   open      tisto4p1
125900140516     c                   else
126000140516     c                   open      fnorf06l
126100140516     c                   open      fnorm01l
126200140516     c                   open      tisto4p2
126300140516     c                   open      tisto4p3
126400140516     c                   endif
126500140516     C
126600000000     C*--------------------
126700051109     C* TIPI BOLLA di recupero
126800000000     C*--------------------
126900000000     C                   Z-ADD     0             I
127000000000     C                   Z-ADD     CODUT         KTBUTE
127100000000     C                   MOVEL     'TB'          KTBCOD
127200000000     C     KEYTAB        SETLL     TABEL00F
127300051109     C                   READe     TABEL00F                               99
127400000000     C     *IN99         DOWEQ     *OFF
127500000000     C     TBLFLG        IFEQ      *BLANKS
127600051109     C                   MOVEL     TBLUNI        DSTB
127700051109     c                   if        §tbrbl = 'R'
127800051109     C                   ADD       1             I
127900051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
128000051109     c                   endif
128100000000     C                   END
128200051109     C     keytab        READe     TABEL00F                               99
128300000000     C                   END
128400000000     C*
128500140428     c* recupero versione cappario alla data DAL
128600140428     C                   Z-ADD     pardsd        P96DRI            8 0
128700140428     C                   Z-ADD     *ZEROS        P96VER            5 0
128800140428     C                   CALL      'TISI96R'
128900140428     C                   PARM                    P96DRI
129000140428     C                   PARM                    P96VER
129100140428     c* carico gli orari in schiera
129200140428     c                   z-add     1             yy
129300140428     c                   if        paror1 >0
129400140428     c                   eval      or_dal(yy)=paror1
129500140428     c                   add       1             yy
129600140428     c                   endif
129700140428     c                   if        paror2 >0
129800140428     c                   eval      or_dal(yy)=paror2
129900140428     c                   add       1             yy
130000140428     c                   endif
130100140428     c                   if        paror3 >0
130200140428     c                   eval      or_dal(yy)=paror3
130300140428     c                   add       1             yy
130400140428     c                   endif
130500140428     c                   if        paror4 >0
130600140428     c                   eval      or_dal(yy)=paror4
130700140428     c                   add       1             yy
130800140428     c                   endif
130900140428     c                   if        paror5 >0
131000140428     c                   eval      or_dal(yy)=paror5
131100140428     c                   add       1             yy
131200140428     c                   endif
131300140428     c                   if        paror6 >0
131400140428     c                   eval      or_dal(yy)=paror6
131500140428     c                   add       1             yy
131600140428     c                   endif
131700140428     c                   if        paror7 >0
131800140428     c                   eval      or_dal(yy)=paror7
131900140428     c                   add       1             yy
132000140428     c                   endif
132100140428     c                   if        paror8 >0
132200140428     c                   eval      or_dal(yy)=paror8
132300140428     c                   add       1             yy
132400140428     c                   endif
132500140428     c                   if        paror9 >0
132600140428     c                   eval      or_dal(yy)=paror9
132700140428     c                   add       1             yy
132800140428     c                   endif
132900140428     C*
133000000000     C                   ENDSR
133100140512     C*------------------------------------------------------------------------*
133200140512     C* Ciclo di lettura su FNORF
133300140512     C*------------------------------------------------------------------------*
133400140512     c     LET_ORF       BEGSR
133500140512     C                   MOVEL     '0'           FINL00            1
133600140512     c
133700140512     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
133800140512     C     FINL00        IFEQ      '0'                                          --- 1 -->
133900140512     C*
134000140512     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
134100140512     C                   EXSR      SETOrf
134200140512     C*
134300140512     C* CICLO FINO A FINE FILE
134400140512     C     FINL00        DOWEQ     '0'                                          --- 2 -->
134500140512     C*
134600140512     C* CREA IL RECORD SUI FILE DI LAVORO
134700140512     C                   EXSR      CRErecR
134800140512     C*
134900140512     C* LEGGE IL RECORD SUCCESSIVO
135000140512     C                   EXSR      LETOrf
135100140512     C                   ENDDO                                                  <-- 2 ---
135200140512     C*
135300140512     C                   ENDIF                                                  <-- 1 ---
135400140512     c                   ENDSR
135500140512     C*------------------------------------------------------------------------*
135600140514     C* SETorf - POSIZIONAMENTO SUL FILE
135700140512     C*------------------------------------------------------------------------*
135800140512     C     SETOrf        BEGSR
135900140512     C*
136000140512     C* POSIZIONAMENTO E PRIMA LETTURA
136100140512     C*
136200140512     C                   movel     pardsd        Kdae
136300140512     C*
136400140512     C     Korf          SETLL     FNORF06L                           99        *FINE FILE
136500140512     C*
136600140512     C     *IN99         IFEQ      '1'
136700140512     C                   MOVEL     '1'           FINL00
136800140512     C                   ELSE
136900140512     C                   EXSR      LETOrf
137000140512     C                   END
137100140512     C*
137200140512     C                   ENDSR
137300140512     C*------------------------------------------------------------------------*
137400140512     C* LETFIL - LETTURA PROSSIMO RECORD
137500140512     C*------------------------------------------------------------------------*
137600140512     C     LETOrf        BEGSR
137700140512     C*
137800140512     C* LEGGO FINO A:
137900140512     C*     - FINE FILE O SUPERATO LIMITE FINALE
138000140512     C*     - TROVATO RECORD VALIDO
138100140512     C                   MOVEL     'N'           WRECOK
138200140512    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
138300140512     C     WRECOK        ANDEQ     'N'
138400140512     C*
138500140512     C*
138600140512     C     Korfp         READE     fnorf06l                               99
138700140512     c
138800140512    3C                   IF        *IN99  OR orfdae>PARDSA
138900140512     C                   MOVEL     '1'           FINL00
139000140514     c                   else
139100140512     c
139200140512     C                   EXSR      CHKREC_O                                     *CONTR.VALIDITA REC.
139300140512    2C                   END                                                    <-- 1 ---
139400140512     C*
139500140512    1C                   ENDDO                                                  <-- 1 ---
139600140512     C*
139700140512     C                   ENDSR
139800140512     C*------------------------------------------------------------------------*
139900140512     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD per Ritiri
140000140512     C*------------------------------------------------------------------------*
140100140512     C     CHKREC_O      BEGSR
140200140512     C*
140300140512     C                   MOVEL     'S'           WRECOK            1
140400140512     C*
140500140512     c* Escludo i ritiri contestuali alla consegna
140600140512     c                   if        orfnsr>0
140700140512     C                   MOVEL     'N'           WRECOK
140800140512     c                   endif
140900140512     c* Escludo gli ORM pronta merce: stabiliti col cliente fuori dagli orari std
141000140514     c     korm          chain     fnorm01l
141100140514     c                   if        %found(fnorm01l) and ormorr>0
141200140514     C****               MOVEL     'N'           WRECOK
141300140512     c                   endif
141400140512     C*
141500140512     C                   ENDSR
141600140512     C*------------------------------------------------------------------------*
141700140512     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO  per i ritiri
141800140512     C*------------------------------------------------------------------------*
141900140512     C     CRErecR       BEGSR
142000140515
142100140515     c                   movel     orfore        wora
142200140512     c                   clear                   kloc
142300140512     c
142400140512     c* Richiamo pgm reperimento orari servizi
142500140512     c                   clear                   trulorsds
142600140512     c                   clear                   trulor2ds
142700140516     c                   clear                   trulor3ds
142800140512     c                   eval      ioredta=ormdar
142900140512     c                   eval      iorefil=orfpog
143000140512     c                   eval      iorenar=ormnar
143100140512     c                   eval      iorecap=ormcar
143200140516     c                   eval      ioreloc=ormlor
143300140512     c                   eval      ioretser=partser
143400140516     c*****              eval      IOREgf2 = O95gf2
143500140514     c                   eval      ioretsp ='C'
143600140512     c                   call      'TRULORSR'
143700140512     c                   parm                    kpjba
143800140512     c                   parm                    trulorsds
143900140512     c                   parm                    trulor2ds
144000140516     c                   parm                    trulor3ds
144100140516     c                   eval      wnorm=OOR3NORM
144200140516     c
144300140516     c                   if        oor3norm='S'
144400140516     c                   eval      kloc =OOR3LOC_N
144500140516     c                   endif
144600140512     c*
144700140512     c                   clear                   kserv
144800140514     c* se l'orario di ritiro  non rientra negli orari standard verifico se orario
144900140514     c* concordato col cliente ed in questo caso scarto il record
145000140515     c                   if        (wora  <OOR2STIS  and
145100140514     c                              ormcra>0 and
145200140514     c                              ormorr<oor2stis)     or
145300140515     c                             (wora  >OOR2STFS  and
145400140514     c                              ormcra>0 and
145500140514     c                              ormorr>oor2stfs)
145600140514     c                   leavesr
145700140514     c                   endif
145800140512
145900140512     c     kccosR        chain     wfccos1l
146000140512     c                   if        not %found(wfccos1l)
146100140512     c                   clear                   wfccos00
146200140512     c                   eval      wcosdcmd=pardsd
146300140512     c                   eval      wcosdcma=pardsa
146400140512     c                   eval      wcosver =p96ver
146500140512     c                   eval      wcoslna =kfil
146600140512     c                   eval      wcosnaz =ormnar
146700140512     c                   eval      wcoscap =ormcar
146800140513     c                   eval      wcosloc =kloc
146900140512     c                   eval      wcostser=partser
147000140512     c                   eval      wcosorif=OOR2ORIF
147100140512     c                   eval      wcosserv=kserv
147200140512     c* imposta campi cap/località obsoleto
147300140512     c                   exsr      sr_impobs
147400140512     c* Imposto gli orari nel file
147500140512     c                   eval      WCOSOSTIS=OOR2STIS
147600140512     c                   eval      WCOSOSTfS=OOR2STfS
147700140512     c
147800140512     c* imposto orari dettaglio se ci sono e richiesto
147900140512     c                   if        pardet='S'
148000140512     c                   exsr      sr_ImpOrDet
148100140512     c                   endif
148200140512     c                   endif
148300140512     c
148400140512     c                   add       1             wcosts
148500140512     c* Se loc non normalizzata
148600140516     c                   if        wnorm<>'S'
148700140512     c                   add       1             wcostsNN
148800140512     c                   endif
148900140512     c
149000140512     c*** serve in questo caso? E' uguale a wcosts....
149100140512     c                   add       1             wcostse
149200140512     c
149300140512     c* ritiro negli orari std
149400140515     c                   if        wora  >=OOR2STIS  and wora  <=OOR2STFS
149500140512     c                   add       1             wcostsos
149600140512     c                   endif
149700140512     c* imposto campi dettaglio
149800140513     c                   exsr      ImpoDETuni
149900140512     c
150000140512     c                   if        not %found(wfccos1l)
150100140512     c                   write     wfccos00
150200140512     c                   else
150300140512     c                   update    wfccos00
150400140512     c                   endif
150500140512     C*
150600140512     C                   ENDSR
150700140513     C*------------------------------------------------------------------------*
150800140513     C* Imposto i campi di dettaglio in base ai range passati
150900140513     C*------------------------------------------------------------------------*
151000140513     C     ImpoDETUni    BEGSR
151100140513     c*
151200140513     c                   if        wora  <or_dal(1)
151300140513     c                   add       1             wcosmint
151400140513     c                   leavesr
151500140513     c                   endif
151600140513     c                   if        wora  > paroltre
151700140513     c                   add       1             wcosmaxt
151800140513     c                   leavesr
151900140513     c                   endif
152000140513
152100140513     c                   z-add     1             yy
152200140513     c                   dow       or_dal(yy)>0
152300140513     c                   if        wora  >=or_dal(yy) and
152400140513     c
152500140513     c                             (or_dal(yy+1)>0 and
152600140513     c                              wora  < or_dal(Yy+1)  or
152700140513     c                             (or_dal(yy+1)=0 and
152800140513     c                             wora  <=paroltre))
152900140513     c                   select
153000140513     c                   when      yy=1
153100140513     c                   add       1             wcosor1t
153200140513     c                   when      yy=2
153300140513     c                   add       1             wcosor2t
153400140513     c                   when      yy=3
153500140513     c                   add       1             wcosor3t
153600140513     c                   when      yy=4
153700140513     c                   add       1             wcosor4t
153800140513     c                   when      yy=5
153900140513     c                   add       1             wcosor5t
154000140513     c                   when      yy=6
154100140513     c                   add       1             wcosor6t
154200140513     c                   when      yy=7
154300140513     c                   add       1             wcosor7t
154400140513     c                   when      yy=8
154500140513     c                   add       1             wcosor8t
154600140513     c                   when      yy=9
154700140513     c                   add       1             wcosor9t
154800140513     c                   endsl
154900140513     c
155000140513     c                   leavesr
155100140513     c                   endif
155200140513     c                   add       1             yy
155300140513     c
155400140513     c                   enddo
155500140513     c
155600140513     c                   ENDSR
155700140512     C*------------------------------------------------------------------------*
155800140512     C* Imposta campi "obsloleto"
155900140512     C*------------------------------------------------------------------------*
156000140512     C     sr_impobs     BEGSR
156100140512     c* obsoleto
156200140512     c     kcpc          chain     azcpc01l
156300140512     c                   if        %found(azcpc01l)
156400140512     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
156500140512     c                   endif
156600140512     c                   if        wcosloc<>*blanks
156700140512     c     kcpl          chain     azcpl01l
156800140512     c                   if        %found(azcpl01l)
156900140512     c                   eval      wcosloco=%subst(cplfi1:1:1)
157000140512     c                   endif
157100140512     c                   endif
157200140512     c                   endsr
157300140512     C*------------------------------------------------------------------------*
157400140512     C* Imposta campi orari dettaglio
157500140512     C*------------------------------------------------------------------------*
157600140512     C     sr_ImpOrDet   BEGSR
157700140512     c                   eval      wcosor1=or_dal(1)
157800140512     c                   eval      wcosor2=or_dal(2)
157900140512     c                   eval      wcosor3=or_dal(3)
158000140512     c                   eval      wcosor4=or_dal(4)
158100140512     c                   eval      wcosor5=or_dal(5)
158200140512     c                   eval      wcosor6=or_dal(6)
158300140512     c                   eval      wcosor7=or_dal(7)
158400140512     c                   eval      wcosor8=or_dal(8)
158500140512     c                   eval      wcosor9=or_dal(9)
158600140512     c                   eval      wcosorOl=paroltre
158700140512     c                   endsr
158800000000     C*------------------------------------------------------------------------*
158900000000     C* *INZSR - ROUTINE INIZIALE
159000000000     C*------------------------------------------------------------------------*
159100000000     C     *INZSR        BEGSR
159200000000     C*--------------------
159300000000     C* RICEVIMENTO PARAMETRI
159400000000     C*--------------------
159500140408     c
159600140408     c     *dtaara       define    §azute        azuteds
159700140408     c     *dtaara       define    §datiute      ddatiute
159800140408     c                   in(E)     *dtaara
159900140408     c                   If        %error  or RSUT = *blanks
160000140408     c                   CLEAR                   tibs34ds
160100140408     c                   CALL      'TIBS34R'
160200140408     c                   PARM                    tibs34ds
160300140408     c                   in        *dtaara
160400140408     c                   EndIf
160500140408     c
160600140408     c                   eval      p1drsut=rsut
160700140408     C*
160800000000     C*--------------------
160900000000     C* CAMPI RIFERITI AL DATA BASE
161000000000     C*--------------------
161100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
161200000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
161300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
161400051109     C     *LIKE         DEFINE    TBLkey        Kkey
161500000000     C*--------------------
161600000000     C* CHIAVI DI LETTURA
161700000000     C*--------------------
161800140404     C* CHIAVE LETTURA TITAS26C - COMPLETA
161900140404     C     Ktas          KLIST
162000140404     C                   KFLD                    Kdcm
162100140404     C                   KFLD                    Kfil
162200140512     C* CHIAVE LETTURA FNORF06L - COMPLETA
162300140512     C     Korf          KLIST
162400140512     C                   KFLD                    Kfil
162500140512     C                   KFLD                    Kfar
162600140512     C                   KFLD                    Kdae
162700140514     C*
162800140514     C     Korfp         KLIST
162900140514     C                   KFLD                    Kfil
163000140514     C                   KFLD                    Kfar
163100140514     C     Korm          KLIST
163200140514     C                   KFLD                    orfpoe
163300140514     C                   KFLD                    orfnsr
163400140514     C                   KFLD                    orfnor
163500140514     C                   KFLD                    orfnrv
163600000000     C*
163700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
163800000000     C     KEYTAB        KLIST
163900000000     C                   KFLD                    KTBUTE
164000000000     C                   KFLD                    KTBCOD
164100051109     C     KTAB          KLIST
164200051109     C                   KFLD                    ktbute
164300051109     C                   KFLD                    ktbcod
164400051109     C                   KFLD                    KKEY
164500000000     C*
164600060524     C     Kar5          KLIST
164700060524     C                   KFLD                    tasAAS
164800060524     C                   KFLD                    taslnp
164900060524     C                   KFLD                    tasnrs
165000060524     C                   KFLD                    tasnsp
165100060524     c                   kfld                    kar5trd
165200150505     C     Kta4          KLIST
165300150505     C                   KFLD                    tasAAS
165400150505     C                   KFLD                    taslnp
165500150505     C                   KFLD                    tasnrs
165600150505     C                   KFLD                    tasnsp
165700150505     c                   kfld                    ktrc
165800140404     C*
165900140404     C     Kdst          KLIST
166000140404     C                   KFLD                    Knpg
166100140404     C                   KFLD                    knFV
166200140404     C                   KFLD                    Kfgs
166300140404     c
166400140415     C     KCCOS         KLIST
166500140415     C                   KFLD                    partser
166600140423     C                   KFLD                    d55tfa
166700140404     C                   KFLD                    tasnzd
166800140404     C                   KFLD                    tascad
166900140404     C                   KFLD                    Kloc
167000140415     C                   KFLD                    kserv
167100140514     C     KCCOSr        KLIST
167200140514     C                   KFLD                    partser
167300140514     C                   KFLD                    kfil
167400140514     C                   KFLD                    ormnar
167500140514     C                   KFLD                    ormcar
167600140514     C                   KFLD                    Kloc
167700140514     C                   KFLD                    kserv
167800140424     C     Kcpc          KLIST
167900140424     C                   KFLD                    p96ver
168000140424     C                   KFLD                    wcosnaz
168100140424     C                   KFLD                    wcoscap
168200140424     C     Kcpl          KLIST
168300140424     C                   KFLD                    p96ver
168400140424     C                   KFLD                    wcosnaz
168500140424     C                   KFLD                    wcoscap
168600140424     C                   KFLD                    wcosloc
168700150717     C     Kcpl2         KLIST
168800150717     C                   KFLD                    p96ver
168900150717     C                   KFLD                    tasnzd
169000150717     C                   KFLD                    tascad
169100150717     C                   KFLD                    taslod
169200000000     C*--------------------
169300000000     C                   Z-ADD     1             CODUT             1 0
169400000000     C*
169500000000     C***
169600000000     C* CARICA ORGANIGRAMMA
169700000000     C                   EXSR      CARORG
169800000000     C*
169900000000     C                   ENDSR
