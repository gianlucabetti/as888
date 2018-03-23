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
046300140516     c                   endif
046400140516     c*
046500140404     c*
046600140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
046700140415     c                   if        oor2fcsgl='P'
046800140415     c                   eval      kserv='P '
046900140415     c                   else
047000140415     c                   clear                   kserv
047100140415     c                   endif
047200140415
047300140415     c     kccos         chain     wfccos1l
047400140404     c                   if        not %found(wfccos1l)
047500140404     c                   clear                   wfccos00
047600140404     c                   eval      wcosdcmd=pardsd
047700140404     c                   eval      wcosdcma=pardsa
047800140404     c                   eval      wcosver =p96ver
047900140411     c                   eval      wcoslna =d55tfa
048000140404     c                   eval      wcosnaz =tasnzd
048100140404     c                   eval      wcoscap =tascad
048200140404     c                   eval      wcosloc =kloc
048300140404     c                   eval      wcostser=partser
048400140404     c                   eval      wcosorif=OOR2ORIF
048500140415     c                   eval      wcosserv=kserv
048600140424     c
048700140424     c* obsoleto
048800140514     c                   exsr      sr_impobs
048900140424     c* Imposto gli orari nel file
049000140424     c                   eval      WCOSOSTIS=OOR2STIS
049100140424     c                   eval      WCOSOSTfS=OOR2STfS
049200140424     c                   eval      WCOSOmiis=OOR2miis
049300140424     c                   eval      WCOSOmxfs=OOR2mxfs
049400140424     c
049500140415     c* imposto orari dettaglio se ci sono e richiestoc
049600140415     c                   if        pardet='S'
049700140514     c                   exsr      sr_ImpOrDet
049800140404     c                   endif
049900140415     c                   endif
050000140404     c
050100140404     c                   add       1             wcosts
050200140404     c* Se loc non normalizzata
050300140516     c                   if        wnorm<>'S'
050400140404     c                   add       1             wcostsNN
050500140404     c                   endif
050600140408     c
050700140408     c* Controllo se spedizione da elaborare
050800140408     c                   exsr      Noelab
050900140408     c
051000140404     c* elaborate
051100140408     c                   if        welab='S'
051200140404     c                   add       1             wcostse
051300140404     c
051400140404     c* consegna negli orari std
051500140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
051600140404     c                   add       1             wcostsos
051700140404     c                   endif
051800140404     c* consegna negli orari min/max
051900140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
052000140404     c                   add       1             wcostsom
052100140404     c                   endif
052200140415     c* imposto campi dettaglio
052300140514     c                   z-add     tashmc        wora
052400140514     c                   exsr      ImpoDETUni
052500140408     c                   endif
052600140404     c
052700140404     c                   if        not %found(wfccos1l)
052800140404     c                   write     wfccos00
052900140404     c                   else
053000140404     c                   update    wfccos00
053100140404     c                   endif
053200000000     C*
053300000000     C                   ENDSR
053400140404     C*------------------------------------------------------------------------*
053500140404     C* Stampa - statistica
053600140404     C*------------------------------------------------------------------------*
053700140404     c     STAMPA        BEGSR
053800140404     c* Tpo servizio
053900140404     c                   if        partser='C'
054000140404     c                   eval      p1dtser='CONSEGNE'
054100140404     C                   ELSE
054200140408     c                   eval      p1dtser=' RITIRI '
054300140404     C                   ENDIF
054400140408     c* giro le date
054500140409     C                   MOVE      pardsd        G02inv
054600140408     C                   MOVEL     '3'           G02ERR
054700140408     C                   CALL      'XSRDA8'
054800140408     C                   PARM                    WLBDAT
054900140408     c                   z-add     g02dat        p1ddsd
055000140408     c*
055100140408     c                   if        pardsa>0
055200140409     C                   MOVE      pardsa        G02inv
055300140408     C                   MOVEL     '3'           G02ERR
055400140408     C                   CALL      'XSRDA8'
055500140408     C                   PARM                    WLBDAT
055600140408     c                   z-add     g02dat        p1ddsa
055700140408     c                   endif
055800140408     c* Orari per calcolo %
055900140408     c                   if        parorari='S'
056000140408     c                   eval      p1dorari='STANDARD'
056100140424     c                   eval      p1dtor_d='Standard'
056200140408     c                   else
056300140408     c                   eval      p1dorari='MINIMO/MASSIMO'
056400140424     c                   eval      p1dtor_d=' Min/Max'
056500140408     c                   endif
056600140408     c
056700140514     c                   if        partser='C'
056800140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
056900140408     c                   if        parcor='C'
057000140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
057100140411     c                   eval      p1dbolstd='   Nei limiti STD   '
057200140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
057300140408     C                   ELSE
057400140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
057500140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
057600140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
057700140408     C                   ENDIF
057800140514     c                   else
057900140514     c                   eval      p1dbolstd='  Fuori limiti STD  '
058000140514     c                   endif
058100140423     c
058200140514     c                   if        partser='C'
058300140423     c                   write     STO4TE
058400140514     c                   else
058500140514     c                   write     STO4TE2
058600140514     c                   endif
058700140423
058800140423     c* Orari nel dettaglio
058900140423     c                   if        pardet='S'
059000140423     c                   exsr      TestaDET
059100140514     c                   if        partser='C'
059200140423     c                   write     STO4TE_d
059300140514     c                   else
059400140514     c                   write     STO4TE_d3
059500140514     c                   endif
059600140423     c                   endif
059700140404     c
059800140404     C
059900140424     c*
060000140514     c     partser       setll     wfccos1l
060100140514     c     partser       reade     wfccos1l
060200140404     c                   dow       not %eof(wfccos1l)
060300140404     c
060400140408     c* a cambio filiale stampo a pagina nuova
060500140408     c                   if        wcoslna<>savlna
060600140408
060700140408     c                   if        savlna>0
060800140423     c                   exsr      Totfil
060900140423     c
061000140514     c                   if        partser='C'
061100140408     c                   write     STO4TE
061200140514     c                   else
061300140514     c                   write     STO4TE2
061400140514     c                   endif
061500140422     c
061600140422     c                   if        pardet='S'
061700140514     c                   if        partser='C'
061800140422     c                   write     STO4RI_d
061900140422     c                   write     STO4TE_d
062000140514     c                   else
062100140514     c                   write     STO4RI_d3
062200140514     c                   write     STO4TE_d3
062300140514     c                   endif
062400140408     c                   endif
062500140423     c
062600140423     c                   clear                   tot_ts
062700140423     c                   clear                   tot_tse
062800140423     c                   clear                   tot_tsos
062900140423     c                   clear                   tot_tsom
063000140422     c                   endif
063100140408
063200140408     c                   z-add     1             i
063300140408     C     wcoslna       lookup    fil(i)                                 55
063400140404     C                   if        %found
063500140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
063600140408     c                   eval      p1dare=dare(i)
063700140404     C                   else
063800140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
063900140408     c                   eval      p1dare=*all'?'
064000140404     C                   endif
064100140408
064200140408     c                   eval      savlna=wcoslna
064300140408
064400140514     c                   if        partser='C'
064500140408     c                   write     STO4TEF
064600140414     c                   write     STO4RI
064700140514     c                   else
064800140514     c                   write     STO4TEF2
064900140514     c                   write     STO4RI2
065000140514     c                   endif
065100140422     c                   if        pardet='S'
065200140514     c                   if        partser='C'
065300140422     c                   write     STO4TEF_d
065400140422     c                   write     STO4RI_d
065500140514     c                   else
065600140514     c                   write     STO4TEF_d3
065700140514     c                   write     STO4RI_d3
065800140514     c                   endif
065900140408     c                   endif
066000140422     c                   endif
066100140408
066200140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
066300140411     c                   if        wcostse>=parnum and wcostse>0
066400140408     c* voglio la % di correttezza
066500140408     c                   if        parcor='C'
066600140404     c* Calcolo % STD
066700140619     c                   if        wcostsos>0
066800140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
066900140408     c                   eval      p1dstsos=WCOSTSOs
067000140619     c                   else
067100140619     c                   clear                   p1dperc_os
067200140619     c                   clear                   p1dstsos
067300140619     c                   endif
067400140404     c* Calcolo % MIN/MAX
067500140619     c                   if        wcostsom>0
067600140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
067700140408     c                   eval      p1dstsom=WCOSTSOm
067800140619     c                   else
067900140619     c                   clear                   p1dperc_om
068000140619     c                   clear                   p1dstsom
068100140619     c                   endif
068200140408     c
068300140408     c                   else
068400140408     c* voglio la % delle fuori orario
068500140408     c                   eval      wfuori_os=wcostse-wcostsos
068600140619     c                   if        wfuori_OS>0
068700140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
068800140408     c                   eval      p1dstsos=Wfuori_os
068900140619     c                   else
069000140619     c                   clear                   p1dperc_os
069100140619     c                   clear                   p1dstsos
069200140619     c                   endif
069300140408
069400140408     c                   eval      wfuori_om=wcostse-wcostsom
069500140619     c                   if        wfuori_OM>0
069600140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
069700140408     c                   eval      p1dstsom=Wfuori_om
069800140619     c                   else
069900140619     c                   clear                   p1dperc_om
070000140619     c                   clear                   p1dstsom
070100140619     c                   endif
070200140408     c                   endif
070300140408     c
070400140407     c* stampo solo al di sotto della % richiesta
070500140408     c                   eval      wstampadet='S'
070600140408
070700140408     c                   select
070800140408     c                   when      parperc=0
070900140408
071000140408     c                   when      parorari='S'  and parcor='C' and
071100140408     c                             p1dperc_os>=parperc
071200140408     c                   eval      wstampadet='N'
071300140408     c
071400140408     c                   when      parorari='S'  and parcor='F' and
071500140408     c                             p1dperc_os< parperc
071600140408     c                   eval      wstampadet='N'
071700140408
071800140408     c                   when      parorari='M'  and parcor='C' and
071900140408     c                             p1dperc_om>=parperc
072000140408     c                   eval      wstampadet='N'
072100140408     c
072200140408     c                   when      parorari='M'  and parcor='F' and
072300140408     c                             p1dperc_om< parperc
072400140408     c                   eval      wstampadet='N'
072500140408     c                   endsl
072600140404     c*
072700140408     c                   if        wstampadet='S'
072800140424     c* POM
072900140424     c                   if        wcosserv='P '
073000140424     c                   eval      p1dpom='Pom'
073100140424     c                   else
073200140424     c                   eval      p1dpom='   '
073300140424     c                   endif
073400140411     c*
073500140411     c* Se la località è vuota imposto '??'
073600140411     c                   if        wcosloc=*blanks
073700140411     c                   eval      wcosloc='??'
073800140411     c                   endif
073900140408     c
074000140408     c                   if        *in06
074100140408     c                   write     sto4te
074200140408     c                   write     STO4TEF
074300140414     c                   write     STO4RI
074400140408     c                   setoff                                       06
074500140404     c                   endif
074600140514
074700140514     c                   if        *in08
074800140514     c                   write     sto4te2
074900140514     c                   write     STO4TEF2
075000140514     c                   write     STO4RI2
075100140514     c                   setoff                                       08
075200140514     c                   endif
075300140408     c
075400140424     c                   eval      p1dstis=wcosoSTiS
075500140424     c                   eval      p1dstfs=wcosoSTFS
075600140424     c                   eval      p1dmiis=wcosomiis
075700140424     c                   eval      p1dmxfs=wcosomxfs
075800140404     c
075900140514     c                   if        partser='C'
076000140408     c                   write     STO4det
076100140514     c                   else
076200140514     c                   write     STO4det2
076300140514     c                   endif
076400140422     c
076500140422     c* se devo stampare anche il dettaglio
076600140422     c                   if        pardet='S'
076700140422     c                   exsr      Stadet
076800140422     c                   endif
076900140408     c
077000140407     c                   endif
077100140408     c                   endif
077200140424     c* totali di filiale
077300140424     c                   add       WCOSTS        tot_ts
077400140424     c                   add       WCOSTSE       tot_tse
077500140424     c                   add       WCOSTSos      tot_tsos
077600140424     c                   add       WCOSTSom      tot_tsom
077700140407     c
077800140514     c     partser       reade     wfccos1l
077900140404     c
078000140404     c                   enddo
078100140423     c
078200140423     c* totali finali
078300140423     c                   if        savlna>0
078400140423     c                   exsr      totfil
078500140423     c                   endif
078600140404     c
078700140404     c                   ENDSR
078800140423     C*------------------------------------------------------------------------*
078900140423     c* Imposta gli orari in testata dettaglioù
079000140423     C*------------------------------------------------------------------------*
079100140423     c     TestaDet      BEGSR
079200140423     c                   eval      p1dsot = paror1
079300140423     c                   eval      p1doltre= paroltre
079400140423     c                   eval      p1dda1  = or_dal(1)
079500140423
079600140423     c                   if        or_dal(2)>0
079700140423     c                   eval      p1dda2  = or_dal(2)
079800140423     c                   movel     or_dal(2)     w0060
079900140423     c                   move      w0060         timeiso
080000140423     c                   subdur    1:*mn         timeiso
080100140423     c                   eval      w0060     =%dec(timeiso)
080200140423     c                   movel     w0060         p1dal1
080300140423     c                   else
080400140423     c                   eval      p1dal1=paroltre
080500140423     c                   leavesr
080600140423     c                   endif
080700140423     c
080800140423     c                   if        or_dal(3)>0
080900140423     c                   eval      p1dda3  = or_dal(3)
081000140423     c                   movel     or_dal(3)     w0060
081100140423     c                   move      w0060         timeiso
081200140423     c                   subdur    1:*mn         timeiso
081300140423     c                   eval      w0060     =%dec(timeiso)
081400140423     c                   movel     w0060         p1dal2
081500140423     c                   else
081600140423     c                   eval      p1dal2=paroltre
081700140423     c                   leavesr
081800140423     c                   endif
081900140423
082000140423     c                   if        or_dal(4)>0
082100140423     c                   eval      p1dda4  = or_dal(4)
082200140423     c                   movel     or_dal(4)     w0060
082300140423     c                   move      w0060         timeiso
082400140423     c                   subdur    1:*mn         timeiso
082500140423     c                   eval      w0060     =%dec(timeiso)
082600140423     c                   movel     w0060         p1dal3
082700140423     c                   else
082800140423     c                   eval      p1dal3=paroltre
082900140423     c                   leavesr
083000140423     c                   endif
083100140423
083200140423     c                   if        or_dal(5)>0
083300140423     c                   eval      p1dda5  = or_dal(5)
083400140423     c                   movel     or_dal(5)     w0060
083500140423     c                   move      w0060         timeiso
083600140423     c                   subdur    1:*mn         timeiso
083700140423     c                   eval      w0060     =%dec(timeiso)
083800140423     c                   movel     w0060         p1dal4
083900140423     c                   else
084000140423     c                   eval      p1dal4=paroltre
084100140423     c                   leavesr
084200140423     c                   endif
084300140423
084400140423     c                   if        or_dal(6)>0
084500140423     c                   eval      p1dda6  = or_dal(6)
084600140423     c                   movel     or_dal(6)     w0060
084700140423     c                   move      w0060         timeiso
084800140423     c                   subdur    1:*mn         timeiso
084900140423     c                   eval      w0060     =%dec(timeiso)
085000140423     c                   movel     w0060         p1dal5
085100140423     c                   else
085200140423     c                   eval      p1dal5=paroltre
085300140423     c                   leavesr
085400140423     c                   endif
085500140423
085600140423     c                   if        or_dal(7)>0
085700140423     c                   eval      p1dda7  = or_dal(7)
085800140423     c                   movel     or_dal(7)     w0060
085900140423     c                   move      w0060         timeiso
086000140423     c                   subdur    1:*mn         timeiso
086100140423     c                   eval      w0060     =%dec(timeiso)
086200140423     c                   movel     w0060         p1dal6
086300140423     c                   else
086400140423     c                   eval      p1dal6=paroltre
086500140423     c                   leavesr
086600140423     c                   endif
086700140423
086800140423     c                   if        or_dal(8)>0
086900140423     c                   eval      p1dda8  = or_dal(8)
087000140423     c                   movel     or_dal(8)     w0060
087100140423     c                   move      w0060         timeiso
087200140423     c                   subdur    1:*mn         timeiso
087300140423     c                   eval      w0060     =%dec(timeiso)
087400140423     c                   movel     w0060         p1dal7
087500140423     c                   else
087600140423     c                   eval      p1dal7=paroltre
087700140423     c                   leavesr
087800140423     c                   endif
087900140423     c                   if        or_dal(9)>0
088000140423     c                   eval      p1dda9  = or_dal(9)
088100140423     c                   movel     or_dal(9)     w0060
088200140423     c                   move      w0060         timeiso
088300140423     c                   subdur    1:*mn         timeiso
088400140423     c                   eval      w0060     =%dec(timeiso)
088500140423     c                   movel     w0060         p1dal8
088600140423     c
088700140423     c                   eval      p1dal9=paroltre
088800140423     c                   else
088900140423     c                   eval      p1dal8=paroltre
089000140423     c                   leavesr
089100140423     c                   endif
089200140423     c
089300140423     c                   ENDSR
089400140422     C*------------------------------------------------------------------------*
089500140422     C* Stampa dettaglio
089600140422     C*------------------------------------------------------------------------*
089700140422     C     StaDET        BEGSR
089800140422     c                   if        *in07
089900140422     c                   write     sto4te_d
090000140422     c                   write     STO4TEF_d
090100140422     c                   write     STO4RI_d
090200140422     c                   setoff                                       07
090300140422     c                   endif
090400140514
090500140514     c                   if        *in09
090600140514     c                   write     sto4te_d3
090700140514     c                   write     STO4TEF_d3
090800140514     c                   write     STO4RI_d3
090900140514     c                   setoff                                       09
091000140514     c                   endif
091100140424     c* Imposto gli orari
091200140424     c                   if        parorari='S'
091300140424     c                   eval      p1dor_dal=WCOSOSTIS
091400140424     c                   eval      p1dor_al =WCOSOSTfS
091500140424     c                   else
091600140424     c                   eval      p1dor_dal=WCOSOmiis
091700140424     c                   eval      p1dor_al =WCOSOmxfS
091800140424     c                   endif
091900140424     c                   setoff                                       111213
092000140424     c                   setoff                                       141516
092100140424     c                   setoff                                       171819
092200140505     c                   setoff                                       2021
092300140422     c* calcolo le %
092400140422     c* SOTTO
092500140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
092600140422     C                   IF        WPERC>0
092700140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
092800140505     c                   seton                                        20
092900140422     c                   else
093000140424     c                   eval      p1cp_sot='    '
093100140422     c                   endif
093200140422     c* OLTRE
093300140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
093400140422     C                   IF        WPERC>0
093500140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
093600140505     c                   seton                                        21
093700140422     c                   else
093800140424     c                   eval      p1cp_olt='    '
093900140422     c                   endif
094000140422     c* % 1
094100140505    1c                   if        wcosor1  >0
094200140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
094300140422     c
094400140505    2C                   IF        WPERC>0
094500140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
094600140505     c
094700140505    2c                   select
094800140505     c                   when      or_dal(1)<p1dor_dal
094900140505     c                   seton                                        11
095000140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
095100140505     c                   seton                                        11
095200140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
095300140505     c                   seton                                        11
095400140505    2c                   endsl
095500140505
095600140422     c                   else
095700140424     c                   eval      p1cp_o1 ='    '
095800140505    2c                   endif
095900140424     c
096000140505    1c                   endif
096100140422     c
096200140422     c* % 2
096300140422     c                   if        wcosor2  >0
096400140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
096500140422     c
096600140422     C                   IF        WPERC>0
096700140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
096800140505
096900140505     c                   select
097000140505     c                   when      or_dal(2)<p1dor_dal
097100140505     c                   seton                                        12
097200140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
097300140505     c                   seton                                        12
097400140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
097500140505     c                   seton                                        12
097600140505     c                   endsl
097700140505
097800140422     c                   else
097900140424     c                   eval      p1cp_o2 ='    '
098000140422     c                   endif
098100140422     c                   endif
098200140423     c* % 3
098300140423     c                   if        wcosor3  >0
098400140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
098500140423     c
098600140423     C                   IF        WPERC>0
098700140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
098800140505
098900140505     c                   select
099000140505     c                   when      or_dal(3)<p1dor_dal
099100140505     c                   seton                                        13
099200140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
099300140505     c                   seton                                        13
099400140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
099500140505     c                   seton                                        13
099600140505     c                   endsl
099700140423     c                   else
099800140424     c                   eval      p1cp_o3 ='    '
099900140423     c                   endif
100000140423     c                   endif
100100140423     c* % 4
100200140423     c                   if        wcosor4  >0
100300140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
100400140423     c
100500140423     C                   IF        WPERC>0
100600140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
100700140505
100800140505     c                   select
100900140505     c                   when      or_dal(4)<p1dor_dal
101000140505     c                   seton                                        14
101100140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
101200140505     c                   seton                                        14
101300140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
101400140505     c                   seton                                        14
101500140505     c                   endsl
101600140423     c                   else
101700140424     c                   eval      p1cp_o4 ='    '
101800140423     c                   endif
101900140423     c                   endif
102000140423     c* % 5
102100140423     c                   if        wcosor5  >0
102200140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
102300140423     c
102400140423     C                   IF        WPERC>0
102500140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
102600140505
102700140505     c                   select
102800140505     c                   when      or_dal(5)<p1dor_dal
102900140505     c                   seton                                        15
103000140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
103100140505     c                   seton                                        15
103200140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
103300140505     c                   seton                                        15
103400140505     c                   endsl
103500140423     c                   else
103600140424     c                   eval      p1cp_o5 ='    '
103700140423     c                   endif
103800140423     c                   endif
103900140423     c* % 6
104000140423     c                   if        wcosor6  >0
104100140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
104200140423     c
104300140423     C                   IF        WPERC>0
104400140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
104500140505
104600140505     c                   select
104700140505     c                   when      or_dal(6)<p1dor_dal
104800140505     c                   seton                                        16
104900140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
105000140505     c                   seton                                        16
105100140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
105200140505     c                   seton                                        16
105300140505     c                   endsl
105400140423     c                   else
105500140424     c                   eval      p1cp_o6 ='    '
105600140423     c                   endif
105700140423     c                   endif
105800140423     c* % 7
105900140423     c                   if        wcosor7  >0
106000140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
106100140423     c
106200140423     C                   IF        WPERC>0
106300140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
106400140505
106500140505     c                   select
106600140505     c                   when      or_dal(7)<p1dor_dal
106700140505     c                   seton                                        17
106800140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
106900140505     c                   seton                                        17
107000140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
107100140505     c                   seton                                        17
107200140505     c                   endsl
107300140423     c                   else
107400140424     c                   eval      p1cp_o7 ='    '
107500140423     c                   endif
107600140423     c                   endif
107700140423     c* % 8
107800140423     c                   if        wcosor8  >0
107900140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
108000140423     c
108100140423     C                   IF        WPERC>0
108200140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
108300140505
108400140505     c                   select
108500140505     c                   when      or_dal(8)<p1dor_dal
108600140505     c                   seton                                        18
108700140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
108800140505     c                   seton                                        18
108900140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
109000140505     c                   seton                                        18
109100140505     c                   endsl
109200140423     c                   else
109300140424     c                   eval      p1cp_o8 ='    '
109400140423     c                   endif
109500140423     c                   endif
109600140423     c* % 9
109700140423     c                   if        wcosor9  >0
109800140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
109900140423     c
110000140423     C                   IF        WPERC>0
110100140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
110200140505
110300140505     c                   select
110400140505     c                   when      or_dal(9)<p1dor_dal
110500140505     c                   seton                                        19
110600140505     c                   when      paroltre >p1dor_al
110700140505     c                   seton                                        19
110800140505     c                   endsl
110900140423     c                   else
111000140424     c                   eval      p1cp_o9 ='    '
111100140423     c                   endif
111200140423     c                   endif
111300140422     c
111400140514     c                   if        partser='C'
111500140422     c                   write     STO4det_d
111600140514     c                   else
111700140514     c                   write     STO4det_d3
111800140514     c                   endif
111900140422     c                   ENDSR
112000140423     C*------------------------------------------------------------------------*
112100140423     c* Stampa totali di filiale
112200140423     C*------------------------------------------------------------------------*
112300140423     c     TOTfil        BEGSR
112400140423     c* voglio la % di correttezza
112500140423     c                   if        parcor='C'
112600140423     c* Calcolo % STD
112700140619     c                   if        tot_tsos>0
112800140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
112900140423     c                   eval      p1dstsos=tot_TSOs
113000140619     c                   else
113100140619     c                   clear                   p1dperc_os
113200140619     c                   clear                   p1dstsos
113300140619     c                   endif
113400140423     c* Calcolo % MIN/MAX
113500140619     c                   if        tot_tsom>0
113600140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
113700140423     c                   eval      p1dstsom=tot_TSOm
113800140619     c                   else
113900140619     c                   clear                   p1dperc_om
114000140619     c                   clear                   p1dstsom
114100140619     c                   endif
114200140423     c
114300140423     c                   else
114400140423     c* voglio la % delle fuori orario
114500140423     c                   eval      wfuori_os=tot_tse-tot_tsos
114600140619     c                   if        wfuori_os>0
114700140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
114800140423     c                   eval      p1dstsos=Wfuori_os
114900140619     c                   else
115000140619     c                   clear                   p1dperc_os
115100140619     c                   clear                   p1dstsos
115200140619     c                   endif
115300140423
115400140423     c                   eval      wfuori_om=tot_tse-tot_tsom
115500140619     c                   if        wfuori_om>0
115600140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
115700140423     c                   eval      p1dstsom=Wfuori_om
115800140619     c                   else
115900140619     c                   clear                   p1dperc_om
116000140619     c                   clear                   p1dstsom
116100140619     c                   endif
116200140423     c
116300140423     c                   endif
116400140423
116500140514     c                   if        partser='C'
116600140423     c                   if        *in06
116700140423     c                   write     sto4te
116800140423     c                   write     STO4TEF
116900140423     c                   write     STO4RI
117000140423     c                   setoff                                       06
117100140423     c                   else
117200140423     c                   write     STO4RI
117300140930     c                   endif
117400140930     c
117500140423     c                   write     STO4TOT
117600140930     c                   write     STO4RI
117700140514     c                   else
117800140514     c                   if        *in08
117900140514     c                   write     sto4te2
118000140514     c                   write     STO4TEF2
118100140514     c                   write     STO4RI2
118200140514     c                   setoff                                       08
118300140514     c                   else
118400140514     c                   write     STO4RI2
118500140930     c                   endif
118600140514     c                   write     STO4TOT2
118700140930     c                   write     STO4RI2
118800140514     c                   endif
118900140423     c                   ENDSR
119000000000     C*------------------------------------------------------------------------*
119100000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
119200000000     C*------------------------------------------------------------------------*
119300000000     C     CARORG        BEGSR
119400000000     C*
119500140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
119600000000     C  N99              READ      AZORG01L                               99
119700000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
119800000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
119900000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
120000000000     C     ORGFAG        IFNE      'V'
120100051109     C                   ADD       1             I
120200000000     C                   Z-ADD     ORGFIL        FIL(I)
120300000000     C                   Z-ADD     ORGCAR        ARE(I)
120400140404     C* CALCOLA NETWORK
120500140404     C                   MOVEL     ORGDE3        OG143
120600140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
120700051109     c* area
120800051109     C                   Z-ADD     CODUT         KTBUTE
120900051109     C                   MOVEL     '05'          Ktbcod
121000051109     C                   MOVEL(P)  ORGCAR        KKEY
121100051109     C     KTAB          CHAIN     TABEL00F                           30
121200051109    2C     *IN30         IFEQ      *OFF
121300051109     C     TBLFLG        ANDEQ     *BLANKS
121400051109     C                   MOVEL     TBLUNI        DS05
121500140408     C***                MOVEL     §05DES        dare(I)
121600140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
121700051109     C                   ELSE
121800140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
121900140408     C***                MOVE      *BLANKS       dare(I)
122000051109    2C                   END
122100051109     c* p.o.
122200051109     C                   MOVEl     orgdes        dfil(I)
122300051109     c
122400000000     C                   END
122500000000     C                   END                                                    <-- 2 ---
122600000000     C                   READ      AZORG01L                               99
122700000000     C                   END                                                    <-- 1 ---
122800000000     C*
122900000000     C                   ENDSR
123000000000     C*------------------------------------------------------------------------*
123100051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
123200000000     C*------------------------------------------------------------------------*
123300000000     C     CARTBL        BEGSR
123400140516     C
123500140516     c* apertura file distinte di filiale
123600140516     c                   if        partser='C'
123700140516     C                   OPEN(e)   FIDST01L
123800140516     c
123900140516     c                   if        not %open(FIDST01L)
124000140516     c                   eval      %subst(WFDST:7:4)='PRD '
124100140516     c                   eval      %subst(TITAS:7:4)='GRPS'
124200150505     c                   eval      %subst(TITA4:7:4)='GRPS'
124300140516     c                   eval      %subst(FIAR5:7:4)='GRPS'
124400140516     C                   OPEN      FIDST01L
124500140516     c                   endif
124600140516
124700140516     C                   OPEN(e)   TITAS26C
124800150505     C                   OPEN(e)   TITA430C
124900140516     C                   OPEN(e)   FIAR531C
125000140516     c                   open      tisto4p
125100140516     c                   open      tisto4p1
125200140516     c                   else
125300140516     c                   open      fnorf06l
125400140516     c                   open      fnorm01l
125500140516     c                   open      tisto4p2
125600140516     c                   open      tisto4p3
125700140516     c                   endif
125800140516     C
125900000000     C*--------------------
126000051109     C* TIPI BOLLA di recupero
126100000000     C*--------------------
126200000000     C                   Z-ADD     0             I
126300000000     C                   Z-ADD     CODUT         KTBUTE
126400000000     C                   MOVEL     'TB'          KTBCOD
126500000000     C     KEYTAB        SETLL     TABEL00F
126600051109     C                   READe     TABEL00F                               99
126700000000     C     *IN99         DOWEQ     *OFF
126800000000     C     TBLFLG        IFEQ      *BLANKS
126900051109     C                   MOVEL     TBLUNI        DSTB
127000051109     c                   if        §tbrbl = 'R'
127100051109     C                   ADD       1             I
127200051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
127300051109     c                   endif
127400000000     C                   END
127500051109     C     keytab        READe     TABEL00F                               99
127600000000     C                   END
127700000000     C*
127800140428     c* recupero versione cappario alla data DAL
127900140428     C                   Z-ADD     pardsd        P96DRI            8 0
128000140428     C                   Z-ADD     *ZEROS        P96VER            5 0
128100140428     C                   CALL      'TISI96R'
128200140428     C                   PARM                    P96DRI
128300140428     C                   PARM                    P96VER
128400140428     c* carico gli orari in schiera
128500140428     c                   z-add     1             yy
128600140428     c                   if        paror1 >0
128700140428     c                   eval      or_dal(yy)=paror1
128800140428     c                   add       1             yy
128900140428     c                   endif
129000140428     c                   if        paror2 >0
129100140428     c                   eval      or_dal(yy)=paror2
129200140428     c                   add       1             yy
129300140428     c                   endif
129400140428     c                   if        paror3 >0
129500140428     c                   eval      or_dal(yy)=paror3
129600140428     c                   add       1             yy
129700140428     c                   endif
129800140428     c                   if        paror4 >0
129900140428     c                   eval      or_dal(yy)=paror4
130000140428     c                   add       1             yy
130100140428     c                   endif
130200140428     c                   if        paror5 >0
130300140428     c                   eval      or_dal(yy)=paror5
130400140428     c                   add       1             yy
130500140428     c                   endif
130600140428     c                   if        paror6 >0
130700140428     c                   eval      or_dal(yy)=paror6
130800140428     c                   add       1             yy
130900140428     c                   endif
131000140428     c                   if        paror7 >0
131100140428     c                   eval      or_dal(yy)=paror7
131200140428     c                   add       1             yy
131300140428     c                   endif
131400140428     c                   if        paror8 >0
131500140428     c                   eval      or_dal(yy)=paror8
131600140428     c                   add       1             yy
131700140428     c                   endif
131800140428     c                   if        paror9 >0
131900140428     c                   eval      or_dal(yy)=paror9
132000140428     c                   add       1             yy
132100140428     c                   endif
132200140428     C*
132300000000     C                   ENDSR
132400140512     C*------------------------------------------------------------------------*
132500140512     C* Ciclo di lettura su FNORF
132600140512     C*------------------------------------------------------------------------*
132700140512     c     LET_ORF       BEGSR
132800140512     C                   MOVEL     '0'           FINL00            1
132900140512     c
133000140512     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
133100140512     C     FINL00        IFEQ      '0'                                          --- 1 -->
133200140512     C*
133300140512     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
133400140512     C                   EXSR      SETOrf
133500140512     C*
133600140512     C* CICLO FINO A FINE FILE
133700140512     C     FINL00        DOWEQ     '0'                                          --- 2 -->
133800140512     C*
133900140512     C* CREA IL RECORD SUI FILE DI LAVORO
134000140512     C                   EXSR      CRErecR
134100140512     C*
134200140512     C* LEGGE IL RECORD SUCCESSIVO
134300140512     C                   EXSR      LETOrf
134400140512     C                   ENDDO                                                  <-- 2 ---
134500140512     C*
134600140512     C                   ENDIF                                                  <-- 1 ---
134700140512     c                   ENDSR
134800140512     C*------------------------------------------------------------------------*
134900140514     C* SETorf - POSIZIONAMENTO SUL FILE
135000140512     C*------------------------------------------------------------------------*
135100140512     C     SETOrf        BEGSR
135200140512     C*
135300140512     C* POSIZIONAMENTO E PRIMA LETTURA
135400140512     C*
135500140512     C                   movel     pardsd        Kdae
135600140512     C*
135700140512     C     Korf          SETLL     FNORF06L                           99        *FINE FILE
135800140512     C*
135900140512     C     *IN99         IFEQ      '1'
136000140512     C                   MOVEL     '1'           FINL00
136100140512     C                   ELSE
136200140512     C                   EXSR      LETOrf
136300140512     C                   END
136400140512     C*
136500140512     C                   ENDSR
136600140512     C*------------------------------------------------------------------------*
136700140512     C* LETFIL - LETTURA PROSSIMO RECORD
136800140512     C*------------------------------------------------------------------------*
136900140512     C     LETOrf        BEGSR
137000140512     C*
137100140512     C* LEGGO FINO A:
137200140512     C*     - FINE FILE O SUPERATO LIMITE FINALE
137300140512     C*     - TROVATO RECORD VALIDO
137400140512     C                   MOVEL     'N'           WRECOK
137500140512    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
137600140512     C     WRECOK        ANDEQ     'N'
137700140512     C*
137800140512     C*
137900140512     C     Korfp         READE     fnorf06l                               99
138000140512     c
138100140512    3C                   IF        *IN99  OR orfdae>PARDSA
138200140512     C                   MOVEL     '1'           FINL00
138300140514     c                   else
138400140512     c
138500140512     C                   EXSR      CHKREC_O                                     *CONTR.VALIDITA REC.
138600140512    2C                   END                                                    <-- 1 ---
138700140512     C*
138800140512    1C                   ENDDO                                                  <-- 1 ---
138900140512     C*
139000140512     C                   ENDSR
139100140512     C*------------------------------------------------------------------------*
139200140512     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD per Ritiri
139300140512     C*------------------------------------------------------------------------*
139400140512     C     CHKREC_O      BEGSR
139500140512     C*
139600140512     C                   MOVEL     'S'           WRECOK            1
139700140512     C*
139800140512     c* Escludo i ritiri contestuali alla consegna
139900140512     c                   if        orfnsr>0
140000140512     C                   MOVEL     'N'           WRECOK
140100140512     c                   endif
140200140512     c* Escludo gli ORM pronta merce: stabiliti col cliente fuori dagli orari std
140300140514     c     korm          chain     fnorm01l
140400140514     c                   if        %found(fnorm01l) and ormorr>0
140500140514     C****               MOVEL     'N'           WRECOK
140600140512     c                   endif
140700140512     C*
140800140512     C                   ENDSR
140900140512     C*------------------------------------------------------------------------*
141000140512     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO  per i ritiri
141100140512     C*------------------------------------------------------------------------*
141200140512     C     CRErecR       BEGSR
141300140515
141400140515     c                   movel     orfore        wora
141500140512     c                   clear                   kloc
141600140512     c
141700140512     c* Richiamo pgm reperimento orari servizi
141800140512     c                   clear                   trulorsds
141900140512     c                   clear                   trulor2ds
142000140516     c                   clear                   trulor3ds
142100140512     c                   eval      ioredta=ormdar
142200140512     c                   eval      iorefil=orfpog
142300140512     c                   eval      iorenar=ormnar
142400140512     c                   eval      iorecap=ormcar
142500140516     c                   eval      ioreloc=ormlor
142600140512     c                   eval      ioretser=partser
142700140516     c*****              eval      IOREgf2 = O95gf2
142800140514     c                   eval      ioretsp ='C'
142900140512     c                   call      'TRULORSR'
143000140512     c                   parm                    kpjba
143100140512     c                   parm                    trulorsds
143200140512     c                   parm                    trulor2ds
143300140516     c                   parm                    trulor3ds
143400140516     c                   eval      wnorm=OOR3NORM
143500140516     c
143600140516     c                   if        oor3norm='S'
143700140516     c                   eval      kloc =OOR3LOC_N
143800140516     c                   endif
143900140512     c*
144000140512     c                   clear                   kserv
144100140514     c* se l'orario di ritiro  non rientra negli orari standard verifico se orario
144200140514     c* concordato col cliente ed in questo caso scarto il record
144300140515     c                   if        (wora  <OOR2STIS  and
144400140514     c                              ormcra>0 and
144500140514     c                              ormorr<oor2stis)     or
144600140515     c                             (wora  >OOR2STFS  and
144700140514     c                              ormcra>0 and
144800140514     c                              ormorr>oor2stfs)
144900140514     c                   leavesr
145000140514     c                   endif
145100140512
145200140512     c     kccosR        chain     wfccos1l
145300140512     c                   if        not %found(wfccos1l)
145400140512     c                   clear                   wfccos00
145500140512     c                   eval      wcosdcmd=pardsd
145600140512     c                   eval      wcosdcma=pardsa
145700140512     c                   eval      wcosver =p96ver
145800140512     c                   eval      wcoslna =kfil
145900140512     c                   eval      wcosnaz =ormnar
146000140512     c                   eval      wcoscap =ormcar
146100140513     c                   eval      wcosloc =kloc
146200140512     c                   eval      wcostser=partser
146300140512     c                   eval      wcosorif=OOR2ORIF
146400140512     c                   eval      wcosserv=kserv
146500140512     c* imposta campi cap/località obsoleto
146600140512     c                   exsr      sr_impobs
146700140512     c* Imposto gli orari nel file
146800140512     c                   eval      WCOSOSTIS=OOR2STIS
146900140512     c                   eval      WCOSOSTfS=OOR2STfS
147000140512     c
147100140512     c* imposto orari dettaglio se ci sono e richiesto
147200140512     c                   if        pardet='S'
147300140512     c                   exsr      sr_ImpOrDet
147400140512     c                   endif
147500140512     c                   endif
147600140512     c
147700140512     c                   add       1             wcosts
147800140512     c* Se loc non normalizzata
147900140516     c                   if        wnorm<>'S'
148000140512     c                   add       1             wcostsNN
148100140512     c                   endif
148200140512     c
148300140512     c*** serve in questo caso? E' uguale a wcosts....
148400140512     c                   add       1             wcostse
148500140512     c
148600140512     c* ritiro negli orari std
148700140515     c                   if        wora  >=OOR2STIS  and wora  <=OOR2STFS
148800140512     c                   add       1             wcostsos
148900140512     c                   endif
149000140512     c* imposto campi dettaglio
149100140513     c                   exsr      ImpoDETuni
149200140512     c
149300140512     c                   if        not %found(wfccos1l)
149400140512     c                   write     wfccos00
149500140512     c                   else
149600140512     c                   update    wfccos00
149700140512     c                   endif
149800140512     C*
149900140512     C                   ENDSR
150000140513     C*------------------------------------------------------------------------*
150100140513     C* Imposto i campi di dettaglio in base ai range passati
150200140513     C*------------------------------------------------------------------------*
150300140513     C     ImpoDETUni    BEGSR
150400140513     c*
150500140513     c                   if        wora  <or_dal(1)
150600140513     c                   add       1             wcosmint
150700140513     c                   leavesr
150800140513     c                   endif
150900140513     c                   if        wora  > paroltre
151000140513     c                   add       1             wcosmaxt
151100140513     c                   leavesr
151200140513     c                   endif
151300140513
151400140513     c                   z-add     1             yy
151500140513     c                   dow       or_dal(yy)>0
151600140513     c                   if        wora  >=or_dal(yy) and
151700140513     c
151800140513     c                             (or_dal(yy+1)>0 and
151900140513     c                              wora  < or_dal(Yy+1)  or
152000140513     c                             (or_dal(yy+1)=0 and
152100140513     c                             wora  <=paroltre))
152200140513     c                   select
152300140513     c                   when      yy=1
152400140513     c                   add       1             wcosor1t
152500140513     c                   when      yy=2
152600140513     c                   add       1             wcosor2t
152700140513     c                   when      yy=3
152800140513     c                   add       1             wcosor3t
152900140513     c                   when      yy=4
153000140513     c                   add       1             wcosor4t
153100140513     c                   when      yy=5
153200140513     c                   add       1             wcosor5t
153300140513     c                   when      yy=6
153400140513     c                   add       1             wcosor6t
153500140513     c                   when      yy=7
153600140513     c                   add       1             wcosor7t
153700140513     c                   when      yy=8
153800140513     c                   add       1             wcosor8t
153900140513     c                   when      yy=9
154000140513     c                   add       1             wcosor9t
154100140513     c                   endsl
154200140513     c
154300140513     c                   leavesr
154400140513     c                   endif
154500140513     c                   add       1             yy
154600140513     c
154700140513     c                   enddo
154800140513     c
154900140513     c                   ENDSR
155000140512     C*------------------------------------------------------------------------*
155100140512     C* Imposta campi "obsloleto"
155200140512     C*------------------------------------------------------------------------*
155300140512     C     sr_impobs     BEGSR
155400140512     c* obsoleto
155500140512     c     kcpc          chain     azcpc01l
155600140512     c                   if        %found(azcpc01l)
155700140512     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
155800140512     c                   endif
155900140512     c                   if        wcosloc<>*blanks
156000140512     c     kcpl          chain     azcpl01l
156100140512     c                   if        %found(azcpl01l)
156200140512     c                   eval      wcosloco=%subst(cplfi1:1:1)
156300140512     c                   endif
156400140512     c                   endif
156500140512     c                   endsr
156600140512     C*------------------------------------------------------------------------*
156700140512     C* Imposta campi orari dettaglio
156800140512     C*------------------------------------------------------------------------*
156900140512     C     sr_ImpOrDet   BEGSR
157000140512     c                   eval      wcosor1=or_dal(1)
157100140512     c                   eval      wcosor2=or_dal(2)
157200140512     c                   eval      wcosor3=or_dal(3)
157300140512     c                   eval      wcosor4=or_dal(4)
157400140512     c                   eval      wcosor5=or_dal(5)
157500140512     c                   eval      wcosor6=or_dal(6)
157600140512     c                   eval      wcosor7=or_dal(7)
157700140512     c                   eval      wcosor8=or_dal(8)
157800140512     c                   eval      wcosor9=or_dal(9)
157900140512     c                   eval      wcosorOl=paroltre
158000140512     c                   endsr
158100000000     C*------------------------------------------------------------------------*
158200000000     C* *INZSR - ROUTINE INIZIALE
158300000000     C*------------------------------------------------------------------------*
158400000000     C     *INZSR        BEGSR
158500000000     C*--------------------
158600000000     C* RICEVIMENTO PARAMETRI
158700000000     C*--------------------
158800140408     c
158900140408     c     *dtaara       define    §azute        azuteds
159000140408     c     *dtaara       define    §datiute      ddatiute
159100140408     c                   in(E)     *dtaara
159200140408     c                   If        %error  or RSUT = *blanks
159300140408     c                   CLEAR                   tibs34ds
159400140408     c                   CALL      'TIBS34R'
159500140408     c                   PARM                    tibs34ds
159600140408     c                   in        *dtaara
159700140408     c                   EndIf
159800140408     c
159900140408     c                   eval      p1drsut=rsut
160000140408     C*
160100000000     C*--------------------
160200000000     C* CAMPI RIFERITI AL DATA BASE
160300000000     C*--------------------
160400000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
160500000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
160600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
160700051109     C     *LIKE         DEFINE    TBLkey        Kkey
160800000000     C*--------------------
160900000000     C* CHIAVI DI LETTURA
161000000000     C*--------------------
161100140404     C* CHIAVE LETTURA TITAS26C - COMPLETA
161200140404     C     Ktas          KLIST
161300140404     C                   KFLD                    Kdcm
161400140404     C                   KFLD                    Kfil
161500140512     C* CHIAVE LETTURA FNORF06L - COMPLETA
161600140512     C     Korf          KLIST
161700140512     C                   KFLD                    Kfil
161800140512     C                   KFLD                    Kfar
161900140512     C                   KFLD                    Kdae
162000140514     C*
162100140514     C     Korfp         KLIST
162200140514     C                   KFLD                    Kfil
162300140514     C                   KFLD                    Kfar
162400140514     C     Korm          KLIST
162500140514     C                   KFLD                    orfpoe
162600140514     C                   KFLD                    orfnsr
162700140514     C                   KFLD                    orfnor
162800140514     C                   KFLD                    orfnrv
162900000000     C*
163000000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
163100000000     C     KEYTAB        KLIST
163200000000     C                   KFLD                    KTBUTE
163300000000     C                   KFLD                    KTBCOD
163400051109     C     KTAB          KLIST
163500051109     C                   KFLD                    ktbute
163600051109     C                   KFLD                    ktbcod
163700051109     C                   KFLD                    KKEY
163800000000     C*
163900060524     C     Kar5          KLIST
164000060524     C                   KFLD                    tasAAS
164100060524     C                   KFLD                    taslnp
164200060524     C                   KFLD                    tasnrs
164300060524     C                   KFLD                    tasnsp
164400060524     c                   kfld                    kar5trd
164500150505     C     Kta4          KLIST
164600150505     C                   KFLD                    tasAAS
164700150505     C                   KFLD                    taslnp
164800150505     C                   KFLD                    tasnrs
164900150505     C                   KFLD                    tasnsp
165000150505     c                   kfld                    ktrc
165100140404     C*
165200140404     C     Kdst          KLIST
165300140404     C                   KFLD                    Knpg
165400140404     C                   KFLD                    knFV
165500140404     C                   KFLD                    Kfgs
165600140404     c
165700140415     C     KCCOS         KLIST
165800140415     C                   KFLD                    partser
165900140423     C                   KFLD                    d55tfa
166000140404     C                   KFLD                    tasnzd
166100140404     C                   KFLD                    tascad
166200140404     C                   KFLD                    Kloc
166300140415     C                   KFLD                    kserv
166400140514     C     KCCOSr        KLIST
166500140514     C                   KFLD                    partser
166600140514     C                   KFLD                    kfil
166700140514     C                   KFLD                    ormnar
166800140514     C                   KFLD                    ormcar
166900140514     C                   KFLD                    Kloc
167000140514     C                   KFLD                    kserv
167100140424     C     Kcpc          KLIST
167200140424     C                   KFLD                    p96ver
167300140424     C                   KFLD                    wcosnaz
167400140424     C                   KFLD                    wcoscap
167500140424     C     Kcpl          KLIST
167600140424     C                   KFLD                    p96ver
167700140424     C                   KFLD                    wcosnaz
167800140424     C                   KFLD                    wcoscap
167900140424     C                   KFLD                    wcosloc
168000000000     C*--------------------
168100000000     C                   Z-ADD     1             CODUT             1 0
168200000000     C*
168300000000     C***
168400000000     C* CARICA ORGANIGRAMMA
168500000000     C                   EXSR      CARORG
168600000000     C*
168700000000     C                   ENDSR
