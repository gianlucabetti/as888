000100000000     H*------------------------------------------------------------------------*
000200140404     H* Statistica controllo orari srervizi  - Crea Work file
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500140409     FTITAS26C  IF   E           K DISK    extfile(titas) usropn
000600140508     Ffidst01l  IF   E           K DISK    extfile(wfdst) usropn
000700140409     Ffiar531c  IF   E           K DISK    extfile(fiar5) usropn
000800140514     Ffnorf06l  IF   E           K DISK    usropn
000900140514     Ffnorm01l  IF   E           K DISK    usropn
001000000000     FAZORG01L  IF   E           K DISK
001100140408     FAZORG02L  IF   E           K DISK    rename(azorg:azorg002)
001200000000     FTABEL00F  IF   E           K DISK
001300140424     Fazcpc01l  IF   E           K DISK
001400140424     Fazcpl01l  IF   E           K DISK
001500140404     FWFCCOS1l  UF A E           K DISK
001600140514     FTISTO4P   O    E             PRINTER OFLIND(*IN06) usropn
001700140514     FTISTO4P1  O    E             PRINTER OFLIND(*IN07) usropn
001800140514     FTISTO4P2  O    E             PRINTER OFLIND(*IN08) usropn
001900140514     FTISTO4P3  O    E             PRINTER OFLIND(*IN09) usropn
002000000000     D*------------------------------------------------------------------------*
002100000000     D* SCHIERE
002200000000     D*------------------------------------------------------------------------*
002300000000     D*-------------------
002400140516     D* FILIALI CON RELATIVA AREA
002500000000     D*-------------------
002600000000     D FIL             S              3  0 DIM(500)                             *FILIALE
002700000000     D ARE             S              3  0 DIM(500)                             *AREA
002800051109     D dfil            S             20    DIM(500)                             *FILIALE
002900140408     D dare            S             29    DIM(500)                             *AREA
003000000000     D*-------------------
003100000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
003200051109     D CT              S              2    DIM(50)                              TIP0 BOLLA
003300140404     d
003400140404     D NTW             S              3    DIM(500)                             *NETWORK
003500140408     d
003600140408     D L6              S              3  0 DIM(30)
003700140422     d
003800140422     D or_dal          S              4  0 DIM(11)
003900000000     D*------------------------------------------------------------------------*
004000000000     D KPJBA         E DS
004100140408     d Azuteds       e ds                  Extname(Azute00f)
004200140408     d dDatiute      e ds
004300140408     d Tibs34ds      e ds                  Inz
004400140409     D azorgds       E DS                  EXTNAME(azorg00f)
004500000000     D*-------------------
004600000000     D DSTB          E DS
004700051109     D DS05          E DS
004800040220     D*------------------
004900140404     D dar5GEN       E DS
005000140404     D og143         E DS
005100140404     d
005200140404     D TRULORSDS     E DS
005300140404     D TRULOR2DS     E DS
005400140516     D TRULOR3DS     E DS
005500140408     d
005600140408     D* DS PER TRUL06R - CARICAMENTO £X
005700140408     D trul06ds      E DS
005800140408     D  LIN                    1     90  0
005900140408     D                                     DIM(30)
006000140411     D fnlv55ds      E DS
006100000000     D*-------------------
006200000000     D* DS PARAMETRI DI LANCIO PGM
006300000000     D*-------------------
006400000000     D PARAM           DS
006500140404     D  pardsd                 1      8  0
006600140404     D  pardsa                 9     16  0
006700140404     D  partser               17     17
006800140404     D  parfil                19     21  0
006900140411     D  parcar                22     24  0
007000140404     D  parperc               25     27  0
007100140408     D  parnum                28     30  0
007200140408     D  parorari              31     31
007300140408     D  parcor                32     32
007400140411     D  pardet                33     33
007500140415     d* dettaglio orari
007600140415     D  paror1                34     37  0
007700140415     D  paror2                38     41  0
007800140415     D  paror3                42     45  0
007900140415     D  paror4                46     49  0
008000140415     D  paror5                50     53  0
008100140415     D  paror6                54     57  0
008200140415     D  paror7                58     61  0
008300140415     D  paror8                62     65  0
008400140416     D  paror9                66     69  0
008500140415     D  paroltre              70     73  0
008600140408     d
008700140408     D WLBDAT          DS
008800140408     D  G02DAT                 1      8  0
008900140408     D  G02INV                 9     16  0
009000140408     D  G02ERR                17     17
009100140408     D  G02TGI                18     22  0
009200140404     d
009300060524     d
009400140404     d KAr5Trd         s                   Like(Ar5Trd) inz('GEN')
009500140404     D wNetwork        s              3    inz(*blanks)
009600140404     D kfil            s              3  0 inz
009700140404     D P1DTSER         s              8    inz
009800140404     D wperc_os        s              3  0 inz
009900140404     D wperc_om        s              3  0 inz
010000140404     D Y               s              3  0 inz
010100140408     D YY              s              3  0 inz
010200140423     D XX              s              3  0 inz
010300140408     D savlna          s              3  0 inz
010400140404     D kdcm            s              8  0 inz
010500140404     D wnorm           s              1    inz
010600140408     D wstampadet      s              1    inz
010700140404     D knfv            s                   like(dstnfv)
010800140516     D kloc            s                   like(ioreloc)
010900140408     D wfuori_os       s                   like(wcostsOS)
011000140408     D wfuori_om       s                   like(wcostsOm)
011100140415     D kserv           s                   like(wcosserv)
011200140422     d Timeiso         s               t   timfmt(*iso)
011300140422     d w0060           s              6  0
011400140422     d wperc           s              3  0
011500140505     d wcosts          s              9  0
011600140505     d tot_ts          s              9  0
011700140423     d tot_tse         s              9  0
011800140423     d tot_tsos        s              9  0
011900140423     d tot_tsom        s              9  0
012000140514     d wora            s                   like(tashmc)
012100140512     D kfar            s                   like(orffar) inz(420)
012200140512     D kdae            s                   like(orfdae) inz
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
014300140514     c* Consegne
014400140514     c                   if        partser='C'
014500140408     c* carico la sua £6
014600140408     C                   CLEAR                   trul06ds
014700140408     C                   MOVE      '£6'          D06COD
014800140408     C                   movel     parfil        D06KEY
014900140408     c                   move      'S'           d06key
015000140409     c                   if        pardsa>0
015100140408     c                   movel     pardsa        d06drf
015200140409     c                   else
015300140409     c                   movel     pardsd        d06drf
015400140409     c                   endif
015500140408     C                   MOVEL     'L'           D06TLA
015600140408     C                   MOVEL     trul06ds      KPJBU
015700140408     C*
015800140408     C                   CALL      'TRUL06R'
015900140408     C                   PARM                    KPJBA
016000140408     C                   MOVEL     KPJBU         trul06ds
016100140408     C                   MOVEA     LIN           L6
016200140423     c                   z-add     1             XX
016300140408
016400140423     c                   dow       l6(XX)>0
016500140423     c                   eval      kfil=l6(XX)
016600140408     c                   exsr      Let_TAS
016700140423     c                   add       1             XX
016800140408     c                   enddo
016900140514     c                   else
017000140514     c* Ritiri
017100140514     c                   eval      kfil=parfil
017200140514     c                   exsr      Let_ORF
017300140514     c                   endif
017400140408     c* immesso cod area
017500140408     c                   when      parcar>0
017600140409     C/EXEC SQL
017700140409     C+ Declare A1 Cursor for
017800140409     c+ SELECT * FROM azorg00f
017900140409     c+ WHERE orgcar=  :parcar AND orgfva=' ' and (orgfag='F' or orgfag='A')
018000140409     c+ ORDER BY orgfil
018100140409     C/END-EXEC
018200140409      *          apertura cursore
018300140409     C/EXEC SQL
018400140409     C+ OPEN A1
018500140409     C/END-EXEC
018600140409     C                   DOU       SqlCod <> 0
018700140409     C/EXEC SQL
018800140409     C+ Fetch Next From A1 Into :azorgds
018900140409     C/END-EXEC
019000140409     c                   select
019100140409     C                   WHEN      SqlCod = 100
019200140409     c                   leave
019300140409      *
019400140409     C                   WHEN      SqlCod < 0
019500140409     c                   seton                                        H1
019600140409     c                   leave
019700140409      *
019800140409     C                   OTHER
019900140408     c                   eval      kfil=orgfil
020000140514     c                   if        partser='C'
020100140408     c                   exsr      Let_TAS
020200140514     c                   else
020300140514     c                   exsr      Let_ORF
020400140514     c                   endif
020500140409     C                   ENDSL
020600140409
020700140408     c                   enddo
020800140409     C/EXEC SQL
020900140409     C+ Close A1
021000140409     C/END-EXEC
021100140408     c
021200140408     c                   other
021300140409
021400140408     c     *loval        setll     azorg02l
021500140408     c                   read      azorg02l
021600140408     c                   dow       not %eof(azorg02l)
021700140408     c                   eval      kfil=orgfil
021800140514     c                   if        partser='C'
021900140408     c                   exsr      Let_TAS
022000140514     c                   else
022100140514     c                   exsr      Let_ORF
022200140514     c                   endif
022300140408     c                   read      azorg02l
022400140408     c                   enddo
022500140408     c
022600140408     c                   endsl
022700140404     c*
022800140404     c                   EXSR      Stampa
022900140408     c*
023000140514     c                   if        partser='C'
023100140408     c                   write     STO4FI
023200140514     c                   else
023300140514     c                   write     STO4FI2
023400140514     c                   endif
023500140422     c
023600140422     c                   if        pardet='S'
023700140514     c                   if        partser='C'
023800140422     c                   write     STO4RI_d
023900140422     c                   write     STO4FI_d
024000140514     c                   else
024100140514     c                   write     STO4RI_d3
024200140514     c                   write     STO4FI_d3
024300140514     c                   endif
024400140422     c                   endif
024500000000     C*
024600140423     C                   CLEAR                   fnlv55ds
024700140423     C                   MOVEL     'C'           D55TLA
024800140423     C                   CALL      'FNLV55R'
024900140423     C                   PARM                    fnlv55ds
025000140423     c
025100000000     C                   SETON                                        LR
025200140408     C*------------------------------------------------------------------------*
025300140408     C* Ciclo di lettura su TITAS
025400140408     C*------------------------------------------------------------------------*
025500140408     c     LET_TAS       BEGSR
025600140409     C                   MOVEL     '0'           FINL00            1
025700140423     c* carico fil gestore
025800140423     c                   clear                   fnlv55ds
025900140423     C                   EVAL      D55TPT='6'
026000140423     c                   eval      d55lin=kfil
026100140423     c                   if        pardsa>0
026200140423     c                   eval      d55drf=pardsa
026300140423     c                   else
026400140423     c                   eval      d55drf=pardsd
026500140423     c                   endif
026600140423     c
026700140423     c                   call      'FNLV55R'
026800140423     c                   parm                    fnlv55ds
026900140408     c
027000140408     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
027100140408     C     FINL00        IFEQ      '0'                                          --- 1 -->
027200140408     C*
027300140408     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
027400140408     C                   EXSR      SETFIL
027500140408     C*
027600140408     C* CICLO FINO A FINE FILE
027700140408     C     FINL00        DOWEQ     '0'                                          --- 2 -->
027800140408     C*
027900140408     C* CREA IL RECORD SUI FILE DI LAVORO
028000140408     C                   EXSR      CRErec
028100140408     C*
028200140408     C* LEGGE IL RECORD SUCCESSIVO
028300140408     C                   EXSR      LETFIL
028400140408     C                   ENDDO                                                  <-- 2 ---
028500140408     C*
028600140408     C                   ENDIF                                                  <-- 1 ---
028700140408     c                   ENDSR
028800000000     C*------------------------------------------------------------------------*
028900000000     C* SETFIL - POSIZIONAMENTO SUL FILE
029000000000     C*------------------------------------------------------------------------*
029100000000     C     SETFIL        BEGSR
029200000000     C*
029300000000     C* POSIZIONAMENTO E PRIMA LETTURA
029400000000     C*
029500140404     C                   movel     pardsd        Kdcm
029600991123     C*
029700140404     C     Ktas          SETLL     TITAS26C                           99        *FINE FILE
029800000000     C*
029900000000     C     *IN99         IFEQ      '1'
030000000000     C                   MOVEL     '1'           FINL00
030100000000     C                   ELSE
030200000000     C                   EXSR      LETFIL
030300000000     C                   END
030400000000     C*
030500000000     C                   ENDSR
030600140404     C*------------------------------------------------------------------------*
030700000000     C* LETFIL - LETTURA PROSSIMO RECORD
030800000000     C*------------------------------------------------------------------------*
030900000000     C     LETFIL        BEGSR
031000000000     C*
031100000000     C* LEGGO FINO A:
031200000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
031300000000     C*     - TROVATO RECORD VALIDO
031400000000     C                   MOVEL     'N'           WRECOK
031500140404    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
031600000000     C     WRECOK        ANDEQ     'N'
031700000000     C*
031800140404     C*
031900140404    2c                   if        kfil>0
032000140404     C     KTAS          READE     TITAS26C                               99
032100140404     c                   else
032200140404     C     Kdcm          READE     TITAS26C                               99
032300140404    2c                   endif
032400140404     c
032500140404    2C                   IF        *IN99
032600140404     C     KDCM          SETGT     TITAS26C
032700140404     C                   READ      TITAS26C                               99
032800140404    3C                   IF        *IN99  OR TASDCM>PARDSA
032900140404     C                   MOVEL     '1'           FINL00
033000140404     C                   ELSE
033100140404     C                   EVAL      KDCM=TASDCM
033200140404     C     KTAS          setll     TITAS26C
033300140404    3C                   ENDIF
033400140404
033500140404   x2C                   ELSE
033600140404     c
033700140404     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
033800140404    2C                   END                                                    <-- 1 ---
033900140404     C*
034000140404    1C                   ENDDO                                                  <-- 1 ---
034100000000     C*
034200000000     C                   ENDSR
034300000000     C*------------------------------------------------------------------------*
034400000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
034500000000     C*------------------------------------------------------------------------*
034600000000     C     CHKREC        BEGSR
034700000000     C*
034800000000     C                   MOVEL     'S'           WRECOK            1
034900040305     C*
035000140414     c*
035100051109     C* ESCLUDE LE BOLLE di recupero
035200051109     C                   Z-ADD     1             i
035300000000     C                   SETOFF                                           28
035400051109     C     TASTBL        LOOKUP    TB(i)                                  28
035500051109     C                   IF        *in28 = *on
035600000000     C                   MOVEL     'N'           WRECOK
035700000000     C                   ENDIF
035800140404     c
035900140414     c* Escludo bolle export e le bolle con consegna anomala
036000140414     c                   if        tasnzd<>*blanks or tascca<>' '
036100140408     C                   MOVEL     'N'           WRECOK
036200140408     c                   endif
036300140414     c
036400140411     c* Escludo le bolle in fermo deposito
036500140411     c                   if        tasffd='S'
036600140411     C                   MOVEL     'N'           WRECOK
036700140411     c                   endif
036800011112     C*
036900000000     C                   ENDSR
037000140408     C*------------------------------------------------------------------------*
037100140408     C* controllo se si tratta di bolla da elaborare
037200140408     C*------------------------------------------------------------------------*
037300140408     C     NOELAB        BEGSR
037400140408     C*
037500140408     C                   MOVEL     'S'           Welab             1
037600140408     C*
037700140408     c* Cerco se si tratta di distinta PDA
037800140508     c* solo in AS888
037900140508    1c                   if        %subst(knsif:7:1)='P'
038000140508     c                   movel     taspdc        kfgs              3 0
038100140508     c                   movel     4             knpg              1 0
038200140508     c                   z-add     tasndc        knfv
038300140508     c     kdst          chain     fidst01l
038400140508    2c                   if        %found(fidst01l)
038500140508     c                             and (dstpda='C' or dstpda='E')
038600140508   x2c                   else
038700140508     C                   MOVEL     'N'           Welab
038800140508    2c                   endif
038900140508     c
039000140508   x1c                   else
039100140508   c
039200140508     c* Solo dall'aprile 2014 abbiamo le bolle con il flag impostato
039300140508     c                   if        tasaas<2014
039400140508     C                   MOVEL     'N'           Welab
039500140508     c                   endif
039600140508     c
039700140508     c                   if        tasaas=2014 and tasmgs<0401
039800140508     C                   MOVEL     'N'           Welab
039900140508     c                   endif
040000140408     c* chaino record GEN
040100140408     c     kar5          chain     fiar531c
040200140408     c                   if        %found(fiar531c)
040300140408     c                   eval      dar5gen=ar5uni
040400140408     c                   if        §AR5PDACO<>'  '
040500140408     C                   MOVEL     'N'           Welab
040600140408     c                   endif
040700140408     c                   endif
040800140508     c
040900140508    1C                   ENDIF
041000140408     c
041100140411     c* Non elaboro se impostata ora consegna richiesta
041200140411     c                   if        tashcr>0
041300140408     C                   MOVEL     'N'           Welab
041400140408     c                   endif
041500140408     c
041600140408     C                   ENDSR
041700000000     C*------------------------------------------------------------------------*
041800140408     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO
041900000000     C*------------------------------------------------------------------------*
042000140408     C     CRErec        BEGSR
042100140516     c
042200140404     c                   clear                   kloc
042300140404     c
042400140404     c* Richiamo pgm reperimento orari servizi
042500140404     c                   clear                   trulorsds
042600140404     c                   clear                   trulor2ds
042700140516     c                   clear                   trulor3ds
042800140404     c                   eval      ioredta=pardsd
042900140408     c                   eval      iorefil=taslna
043000140404     c                   eval      iorenar=tasnzd
043100140404     c                   eval      iorecap=tascad
043200140516     c                   eval      ioreloc=taslod
043300140414     c                   eval      ioreddc=tasddc
043400140414     c                   eval      iorendc=tasndc
043500140414     c                   eval      ioretfp=tastfp
043600140414     c                   eval      ioretfa=tastfa
043700140414     c                   eval      ioredti=tasdti
043800140428     c                   eval      iorehti=9999
043900140404     c                   eval      ioretser=partser
044000140515     c* si forza sempre "C" perché per "H" e "E"  l'orario viene accorciato
044100140515     c* automaticamente e non è l'obiettivo della statistica
044200140515     c                   eval      ioretsp ='C'
044300140404     c                   call      'TRULORSR'
044400140404     c                   parm                    kpjba
044500140404     c                   parm                    trulorsds
044600140404     c                   parm                    trulor2ds
044700140516     c                   parm                    trulor3ds
044800140516     c                   eval      wnorm=OOR3NORM
044900140516     c
045000140516     c                   if        oor3norm='S'
045100140516     c                   eval      kloc =OOR3LOC_N
045200140516     c                   endif
045300140516     c*
045400140404     c*
045500140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
045600140415     c                   if        oor2fcsgl='P'
045700140415     c                   eval      kserv='P '
045800140415     c                   else
045900140415     c                   clear                   kserv
046000140415     c                   endif
046100140415
046200140415     c     kccos         chain     wfccos1l
046300140404     c                   if        not %found(wfccos1l)
046400140404     c                   clear                   wfccos00
046500140404     c                   eval      wcosdcmd=pardsd
046600140404     c                   eval      wcosdcma=pardsa
046700140404     c                   eval      wcosver =p96ver
046800140411     c                   eval      wcoslna =d55tfa
046900140404     c                   eval      wcosnaz =tasnzd
047000140404     c                   eval      wcoscap =tascad
047100140404     c                   eval      wcosloc =kloc
047200140404     c                   eval      wcostser=partser
047300140404     c                   eval      wcosorif=OOR2ORIF
047400140415     c                   eval      wcosserv=kserv
047500140424     c
047600140424     c* obsoleto
047700140514     c                   exsr      sr_impobs
047800140424     c* Imposto gli orari nel file
047900140424     c                   eval      WCOSOSTIS=OOR2STIS
048000140424     c                   eval      WCOSOSTfS=OOR2STfS
048100140424     c                   eval      WCOSOmiis=OOR2miis
048200140424     c                   eval      WCOSOmxfs=OOR2mxfs
048300140424     c
048400140415     c* imposto orari dettaglio se ci sono e richiestoc
048500140415     c                   if        pardet='S'
048600140514     c                   exsr      sr_ImpOrDet
048700140404     c                   endif
048800140415     c                   endif
048900140404     c
049000140404     c                   add       1             wcosts
049100140404     c* Se loc non normalizzata
049200140516     c                   if        wnorm<>'S'
049300140404     c                   add       1             wcostsNN
049400140404     c                   endif
049500140408     c
049600140408     c* Controllo se spedizione da elaborare
049700140408     c                   exsr      Noelab
049800140408     c
049900140404     c* elaborate
050000140408     c                   if        welab='S'
050100140404     c                   add       1             wcostse
050200140404     c
050300140404     c* consegna negli orari std
050400140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
050500140404     c                   add       1             wcostsos
050600140404     c                   endif
050700140404     c* consegna negli orari min/max
050800140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
050900140404     c                   add       1             wcostsom
051000140404     c                   endif
051100140415     c* imposto campi dettaglio
051200140514     c                   z-add     tashmc        wora
051300140514     c                   exsr      ImpoDETUni
051400140408     c                   endif
051500140404     c
051600140404     c                   if        not %found(wfccos1l)
051700140404     c                   write     wfccos00
051800140404     c                   else
051900140404     c                   update    wfccos00
052000140404     c                   endif
052100000000     C*
052200000000     C                   ENDSR
052300140404     C*------------------------------------------------------------------------*
052400140404     C* Stampa - statistica
052500140404     C*------------------------------------------------------------------------*
052600140404     c     STAMPA        BEGSR
052700140404     c* Tpo servizio
052800140404     c                   if        partser='C'
052900140404     c                   eval      p1dtser='CONSEGNE'
053000140404     C                   ELSE
053100140408     c                   eval      p1dtser=' RITIRI '
053200140404     C                   ENDIF
053300140408     c* giro le date
053400140409     C                   MOVE      pardsd        G02inv
053500140408     C                   MOVEL     '3'           G02ERR
053600140408     C                   CALL      'XSRDA8'
053700140408     C                   PARM                    WLBDAT
053800140408     c                   z-add     g02dat        p1ddsd
053900140408     c*
054000140408     c                   if        pardsa>0
054100140409     C                   MOVE      pardsa        G02inv
054200140408     C                   MOVEL     '3'           G02ERR
054300140408     C                   CALL      'XSRDA8'
054400140408     C                   PARM                    WLBDAT
054500140408     c                   z-add     g02dat        p1ddsa
054600140408     c                   endif
054700140408     c* Orari per calcolo %
054800140408     c                   if        parorari='S'
054900140408     c                   eval      p1dorari='STANDARD'
055000140424     c                   eval      p1dtor_d='Standard'
055100140408     c                   else
055200140408     c                   eval      p1dorari='MINIMO/MASSIMO'
055300140424     c                   eval      p1dtor_d=' Min/Max'
055400140408     c                   endif
055500140408     c
055600140514     c                   if        partser='C'
055700140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
055800140408     c                   if        parcor='C'
055900140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
056000140411     c                   eval      p1dbolstd='   Nei limiti STD   '
056100140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
056200140408     C                   ELSE
056300140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
056400140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
056500140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
056600140408     C                   ENDIF
056700140514     c                   else
056800140514     c                   eval      p1dbolstd='  Fuori limiti STD  '
056900140514     c                   endif
057000140423     c
057100140514     c                   if        partser='C'
057200140423     c                   write     STO4TE
057300140514     c                   else
057400140514     c                   write     STO4TE2
057500140514     c                   endif
057600140423
057700140423     c* Orari nel dettaglio
057800140423     c                   if        pardet='S'
057900140423     c                   exsr      TestaDET
058000140514     c                   if        partser='C'
058100140423     c                   write     STO4TE_d
058200140514     c                   else
058300140514     c                   write     STO4TE_d3
058400140514     c                   endif
058500140423     c                   endif
058600140404     c
058700140404     C
058800140424     c*
058900140514     c     partser       setll     wfccos1l
059000140514     c     partser       reade     wfccos1l
059100140404     c                   dow       not %eof(wfccos1l)
059200140404     c
059300140408     c* a cambio filiale stampo a pagina nuova
059400140408     c                   if        wcoslna<>savlna
059500140408
059600140408     c                   if        savlna>0
059700140423     c                   exsr      Totfil
059800140423     c
059900140514     c                   if        partser='C'
060000140408     c                   write     STO4TE
060100140514     c                   else
060200140514     c                   write     STO4TE2
060300140514     c                   endif
060400140422     c
060500140422     c                   if        pardet='S'
060600140514     c                   if        partser='C'
060700140422     c                   write     STO4RI_d
060800140422     c                   write     STO4TE_d
060900140514     c                   else
061000140514     c                   write     STO4RI_d3
061100140514     c                   write     STO4TE_d3
061200140514     c                   endif
061300140408     c                   endif
061400140423     c
061500140423     c                   clear                   tot_ts
061600140423     c                   clear                   tot_tse
061700140423     c                   clear                   tot_tsos
061800140423     c                   clear                   tot_tsom
061900140422     c                   endif
062000140408
062100140408     c                   z-add     1             i
062200140408     C     wcoslna       lookup    fil(i)                                 55
062300140404     C                   if        %found
062400140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
062500140408     c                   eval      p1dare=dare(i)
062600140404     C                   else
062700140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
062800140408     c                   eval      p1dare=*all'?'
062900140404     C                   endif
063000140408
063100140408     c                   eval      savlna=wcoslna
063200140408
063300140514     c                   if        partser='C'
063400140408     c                   write     STO4TEF
063500140414     c                   write     STO4RI
063600140514     c                   else
063700140514     c                   write     STO4TEF2
063800140514     c                   write     STO4RI2
063900140514     c                   endif
064000140422     c                   if        pardet='S'
064100140514     c                   if        partser='C'
064200140422     c                   write     STO4TEF_d
064300140422     c                   write     STO4RI_d
064400140514     c                   else
064500140514     c                   write     STO4TEF_d3
064600140514     c                   write     STO4RI_d3
064700140514     c                   endif
064800140408     c                   endif
064900140422     c                   endif
065000140408
065100140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
065200140411     c                   if        wcostse>=parnum and wcostse>0
065300140408     c* voglio la % di correttezza
065400140408     c                   if        parcor='C'
065500140404     c* Calcolo % STD
065600140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
065700140408     c                   eval      p1dstsos=WCOSTSOs
065800140404     c* Calcolo % MIN/MAX
065900140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
066000140408     c                   eval      p1dstsom=WCOSTSOm
066100140408     c
066200140408     c                   else
066300140408     c* voglio la % delle fuori orario
066400140408     c                   eval      wfuori_os=wcostse-wcostsos
066500140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
066600140408     c                   eval      p1dstsos=Wfuori_os
066700140408
066800140408     c                   eval      wfuori_om=wcostse-wcostsom
066900140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
067000140408     c                   eval      p1dstsom=Wfuori_om
067100140408     c                   endif
067200140408     c
067300140407     c* stampo solo al di sotto della % richiesta
067400140408     c                   eval      wstampadet='S'
067500140408
067600140408     c                   select
067700140408     c                   when      parperc=0
067800140408
067900140408     c                   when      parorari='S'  and parcor='C' and
068000140408     c                             p1dperc_os>=parperc
068100140408     c                   eval      wstampadet='N'
068200140408     c
068300140408     c                   when      parorari='S'  and parcor='F' and
068400140408     c                             p1dperc_os< parperc
068500140408     c                   eval      wstampadet='N'
068600140408
068700140408     c                   when      parorari='M'  and parcor='C' and
068800140408     c                             p1dperc_om>=parperc
068900140408     c                   eval      wstampadet='N'
069000140408     c
069100140408     c                   when      parorari='M'  and parcor='F' and
069200140408     c                             p1dperc_om< parperc
069300140408     c                   eval      wstampadet='N'
069400140408     c                   endsl
069500140404     c*
069600140408     c                   if        wstampadet='S'
069700140424     c* POM
069800140424     c                   if        wcosserv='P '
069900140424     c                   eval      p1dpom='Pom'
070000140424     c                   else
070100140424     c                   eval      p1dpom='   '
070200140424     c                   endif
070300140411     c*
070400140411     c* Se la località è vuota imposto '??'
070500140411     c                   if        wcosloc=*blanks
070600140411     c                   eval      wcosloc='??'
070700140411     c                   endif
070800140408     c
070900140408     c                   if        *in06
071000140408     c                   write     sto4te
071100140408     c                   write     STO4TEF
071200140414     c                   write     STO4RI
071300140408     c                   setoff                                       06
071400140404     c                   endif
071500140514
071600140514     c                   if        *in08
071700140514     c                   write     sto4te2
071800140514     c                   write     STO4TEF2
071900140514     c                   write     STO4RI2
072000140514     c                   setoff                                       08
072100140514     c                   endif
072200140408     c
072300140424     c                   eval      p1dstis=wcosoSTiS
072400140424     c                   eval      p1dstfs=wcosoSTFS
072500140424     c                   eval      p1dmiis=wcosomiis
072600140424     c                   eval      p1dmxfs=wcosomxfs
072700140404     c
072800140514     c                   if        partser='C'
072900140408     c                   write     STO4det
073000140514     c                   else
073100140514     c                   write     STO4det2
073200140514     c                   endif
073300140422     c
073400140422     c* se devo stampare anche il dettaglio
073500140422     c                   if        pardet='S'
073600140422     c                   exsr      Stadet
073700140422     c                   endif
073800140408     c
073900140407     c                   endif
074000140408     c                   endif
074100140424     c* totali di filiale
074200140424     c                   add       WCOSTS        tot_ts
074300140424     c                   add       WCOSTSE       tot_tse
074400140424     c                   add       WCOSTSos      tot_tsos
074500140424     c                   add       WCOSTSom      tot_tsom
074600140407     c
074700140514     c     partser       reade     wfccos1l
074800140404     c
074900140404     c                   enddo
075000140423     c
075100140423     c* totali finali
075200140423     c                   if        savlna>0
075300140423     c                   exsr      totfil
075400140423     c                   endif
075500140404     c
075600140404     c                   ENDSR
075700140423     C*------------------------------------------------------------------------*
075800140423     c* Imposta gli orari in testata dettaglioù
075900140423     C*------------------------------------------------------------------------*
076000140423     c     TestaDet      BEGSR
076100140423     c                   eval      p1dsot = paror1
076200140423     c                   eval      p1doltre= paroltre
076300140423     c                   eval      p1dda1  = or_dal(1)
076400140423
076500140423     c                   if        or_dal(2)>0
076600140423     c                   eval      p1dda2  = or_dal(2)
076700140423     c                   movel     or_dal(2)     w0060
076800140423     c                   move      w0060         timeiso
076900140423     c                   subdur    1:*mn         timeiso
077000140423     c                   eval      w0060     =%dec(timeiso)
077100140423     c                   movel     w0060         p1dal1
077200140423     c                   else
077300140423     c                   eval      p1dal1=paroltre
077400140423     c                   leavesr
077500140423     c                   endif
077600140423     c
077700140423     c                   if        or_dal(3)>0
077800140423     c                   eval      p1dda3  = or_dal(3)
077900140423     c                   movel     or_dal(3)     w0060
078000140423     c                   move      w0060         timeiso
078100140423     c                   subdur    1:*mn         timeiso
078200140423     c                   eval      w0060     =%dec(timeiso)
078300140423     c                   movel     w0060         p1dal2
078400140423     c                   else
078500140423     c                   eval      p1dal2=paroltre
078600140423     c                   leavesr
078700140423     c                   endif
078800140423
078900140423     c                   if        or_dal(4)>0
079000140423     c                   eval      p1dda4  = or_dal(4)
079100140423     c                   movel     or_dal(4)     w0060
079200140423     c                   move      w0060         timeiso
079300140423     c                   subdur    1:*mn         timeiso
079400140423     c                   eval      w0060     =%dec(timeiso)
079500140423     c                   movel     w0060         p1dal3
079600140423     c                   else
079700140423     c                   eval      p1dal3=paroltre
079800140423     c                   leavesr
079900140423     c                   endif
080000140423
080100140423     c                   if        or_dal(5)>0
080200140423     c                   eval      p1dda5  = or_dal(5)
080300140423     c                   movel     or_dal(5)     w0060
080400140423     c                   move      w0060         timeiso
080500140423     c                   subdur    1:*mn         timeiso
080600140423     c                   eval      w0060     =%dec(timeiso)
080700140423     c                   movel     w0060         p1dal4
080800140423     c                   else
080900140423     c                   eval      p1dal4=paroltre
081000140423     c                   leavesr
081100140423     c                   endif
081200140423
081300140423     c                   if        or_dal(6)>0
081400140423     c                   eval      p1dda6  = or_dal(6)
081500140423     c                   movel     or_dal(6)     w0060
081600140423     c                   move      w0060         timeiso
081700140423     c                   subdur    1:*mn         timeiso
081800140423     c                   eval      w0060     =%dec(timeiso)
081900140423     c                   movel     w0060         p1dal5
082000140423     c                   else
082100140423     c                   eval      p1dal5=paroltre
082200140423     c                   leavesr
082300140423     c                   endif
082400140423
082500140423     c                   if        or_dal(7)>0
082600140423     c                   eval      p1dda7  = or_dal(7)
082700140423     c                   movel     or_dal(7)     w0060
082800140423     c                   move      w0060         timeiso
082900140423     c                   subdur    1:*mn         timeiso
083000140423     c                   eval      w0060     =%dec(timeiso)
083100140423     c                   movel     w0060         p1dal6
083200140423     c                   else
083300140423     c                   eval      p1dal6=paroltre
083400140423     c                   leavesr
083500140423     c                   endif
083600140423
083700140423     c                   if        or_dal(8)>0
083800140423     c                   eval      p1dda8  = or_dal(8)
083900140423     c                   movel     or_dal(8)     w0060
084000140423     c                   move      w0060         timeiso
084100140423     c                   subdur    1:*mn         timeiso
084200140423     c                   eval      w0060     =%dec(timeiso)
084300140423     c                   movel     w0060         p1dal7
084400140423     c                   else
084500140423     c                   eval      p1dal7=paroltre
084600140423     c                   leavesr
084700140423     c                   endif
084800140423     c                   if        or_dal(9)>0
084900140423     c                   eval      p1dda9  = or_dal(9)
085000140423     c                   movel     or_dal(9)     w0060
085100140423     c                   move      w0060         timeiso
085200140423     c                   subdur    1:*mn         timeiso
085300140423     c                   eval      w0060     =%dec(timeiso)
085400140423     c                   movel     w0060         p1dal8
085500140423     c
085600140423     c                   eval      p1dal9=paroltre
085700140423     c                   else
085800140423     c                   eval      p1dal8=paroltre
085900140423     c                   leavesr
086000140423     c                   endif
086100140423     c
086200140423     c                   ENDSR
086300140422     C*------------------------------------------------------------------------*
086400140422     C* Stampa dettaglio
086500140422     C*------------------------------------------------------------------------*
086600140422     C     StaDET        BEGSR
086700140422     c                   if        *in07
086800140422     c                   write     sto4te_d
086900140422     c                   write     STO4TEF_d
087000140422     c                   write     STO4RI_d
087100140422     c                   setoff                                       07
087200140422     c                   endif
087300140514
087400140514     c                   if        *in09
087500140514     c                   write     sto4te_d3
087600140514     c                   write     STO4TEF_d3
087700140514     c                   write     STO4RI_d3
087800140514     c                   setoff                                       09
087900140514     c                   endif
088000140424     c* Imposto gli orari
088100140424     c                   if        parorari='S'
088200140424     c                   eval      p1dor_dal=WCOSOSTIS
088300140424     c                   eval      p1dor_al =WCOSOSTfS
088400140424     c                   else
088500140424     c                   eval      p1dor_dal=WCOSOmiis
088600140424     c                   eval      p1dor_al =WCOSOmxfS
088700140424     c                   endif
088800140424     c                   setoff                                       111213
088900140424     c                   setoff                                       141516
089000140424     c                   setoff                                       171819
089100140505     c                   setoff                                       2021
089200140422     c* calcolo le %
089300140422     c* SOTTO
089400140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
089500140422     C                   IF        WPERC>0
089600140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
089700140505     c                   seton                                        20
089800140422     c                   else
089900140424     c                   eval      p1cp_sot='    '
090000140422     c                   endif
090100140422     c* OLTRE
090200140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
090300140422     C                   IF        WPERC>0
090400140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
090500140505     c                   seton                                        21
090600140422     c                   else
090700140424     c                   eval      p1cp_olt='    '
090800140422     c                   endif
090900140422     c* % 1
091000140505    1c                   if        wcosor1  >0
091100140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
091200140422     c
091300140505    2C                   IF        WPERC>0
091400140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
091500140505     c
091600140505    2c                   select
091700140505     c                   when      or_dal(1)<p1dor_dal
091800140505     c                   seton                                        11
091900140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
092000140505     c                   seton                                        11
092100140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
092200140505     c                   seton                                        11
092300140505    2c                   endsl
092400140505
092500140422     c                   else
092600140424     c                   eval      p1cp_o1 ='    '
092700140505    2c                   endif
092800140424     c
092900140505    1c                   endif
093000140422     c
093100140422     c* % 2
093200140422     c                   if        wcosor2  >0
093300140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
093400140422     c
093500140422     C                   IF        WPERC>0
093600140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
093700140505
093800140505     c                   select
093900140505     c                   when      or_dal(2)<p1dor_dal
094000140505     c                   seton                                        12
094100140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
094200140505     c                   seton                                        12
094300140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
094400140505     c                   seton                                        12
094500140505     c                   endsl
094600140505
094700140422     c                   else
094800140424     c                   eval      p1cp_o2 ='    '
094900140422     c                   endif
095000140422     c                   endif
095100140423     c* % 3
095200140423     c                   if        wcosor3  >0
095300140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
095400140423     c
095500140423     C                   IF        WPERC>0
095600140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
095700140505
095800140505     c                   select
095900140505     c                   when      or_dal(3)<p1dor_dal
096000140505     c                   seton                                        13
096100140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
096200140505     c                   seton                                        13
096300140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
096400140505     c                   seton                                        13
096500140505     c                   endsl
096600140423     c                   else
096700140424     c                   eval      p1cp_o3 ='    '
096800140423     c                   endif
096900140423     c                   endif
097000140423     c* % 4
097100140423     c                   if        wcosor4  >0
097200140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
097300140423     c
097400140423     C                   IF        WPERC>0
097500140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
097600140505
097700140505     c                   select
097800140505     c                   when      or_dal(4)<p1dor_dal
097900140505     c                   seton                                        14
098000140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
098100140505     c                   seton                                        14
098200140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
098300140505     c                   seton                                        14
098400140505     c                   endsl
098500140423     c                   else
098600140424     c                   eval      p1cp_o4 ='    '
098700140423     c                   endif
098800140423     c                   endif
098900140423     c* % 5
099000140423     c                   if        wcosor5  >0
099100140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
099200140423     c
099300140423     C                   IF        WPERC>0
099400140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
099500140505
099600140505     c                   select
099700140505     c                   when      or_dal(5)<p1dor_dal
099800140505     c                   seton                                        15
099900140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
100000140505     c                   seton                                        15
100100140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
100200140505     c                   seton                                        15
100300140505     c                   endsl
100400140423     c                   else
100500140424     c                   eval      p1cp_o5 ='    '
100600140423     c                   endif
100700140423     c                   endif
100800140423     c* % 6
100900140423     c                   if        wcosor6  >0
101000140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
101100140423     c
101200140423     C                   IF        WPERC>0
101300140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
101400140505
101500140505     c                   select
101600140505     c                   when      or_dal(6)<p1dor_dal
101700140505     c                   seton                                        16
101800140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
101900140505     c                   seton                                        16
102000140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
102100140505     c                   seton                                        16
102200140505     c                   endsl
102300140423     c                   else
102400140424     c                   eval      p1cp_o6 ='    '
102500140423     c                   endif
102600140423     c                   endif
102700140423     c* % 7
102800140423     c                   if        wcosor7  >0
102900140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
103000140423     c
103100140423     C                   IF        WPERC>0
103200140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
103300140505
103400140505     c                   select
103500140505     c                   when      or_dal(7)<p1dor_dal
103600140505     c                   seton                                        17
103700140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
103800140505     c                   seton                                        17
103900140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
104000140505     c                   seton                                        17
104100140505     c                   endsl
104200140423     c                   else
104300140424     c                   eval      p1cp_o7 ='    '
104400140423     c                   endif
104500140423     c                   endif
104600140423     c* % 8
104700140423     c                   if        wcosor8  >0
104800140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
104900140423     c
105000140423     C                   IF        WPERC>0
105100140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
105200140505
105300140505     c                   select
105400140505     c                   when      or_dal(8)<p1dor_dal
105500140505     c                   seton                                        18
105600140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
105700140505     c                   seton                                        18
105800140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
105900140505     c                   seton                                        18
106000140505     c                   endsl
106100140423     c                   else
106200140424     c                   eval      p1cp_o8 ='    '
106300140423     c                   endif
106400140423     c                   endif
106500140423     c* % 9
106600140423     c                   if        wcosor9  >0
106700140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
106800140423     c
106900140423     C                   IF        WPERC>0
107000140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
107100140505
107200140505     c                   select
107300140505     c                   when      or_dal(9)<p1dor_dal
107400140505     c                   seton                                        19
107500140505     c                   when      paroltre >p1dor_al
107600140505     c                   seton                                        19
107700140505     c                   endsl
107800140423     c                   else
107900140424     c                   eval      p1cp_o9 ='    '
108000140423     c                   endif
108100140423     c                   endif
108200140422     c
108300140514     c                   if        partser='C'
108400140422     c                   write     STO4det_d
108500140514     c                   else
108600140514     c                   write     STO4det_d3
108700140514     c                   endif
108800140422     c                   ENDSR
108900140423     C*------------------------------------------------------------------------*
109000140423     c* Stampa totali di filiale
109100140423     C*------------------------------------------------------------------------*
109200140423     c     TOTfil        BEGSR
109300140423     c* voglio la % di correttezza
109400140423     c                   if        parcor='C'
109500140423     c* Calcolo % STD
109600140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
109700140423     c                   eval      p1dstsos=tot_TSOs
109800140423     c* Calcolo % MIN/MAX
109900140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
110000140423     c                   eval      p1dstsom=tot_TSOm
110100140423     c
110200140423     c                   else
110300140423     c* voglio la % delle fuori orario
110400140423     c                   eval      wfuori_os=tot_tse-tot_tsos
110500140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
110600140423     c                   eval      p1dstsos=Wfuori_os
110700140423
110800140423     c                   eval      wfuori_om=tot_tse-tot_tsom
110900140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
111000140423     c                   eval      p1dstsom=Wfuori_om
111100140423     c
111200140423     c                   endif
111300140423
111400140514     c                   if        partser='C'
111500140423     c                   if        *in06
111600140423     c                   write     sto4te
111700140423     c                   write     STO4TEF
111800140423     c                   write     STO4RI
111900140423     c                   setoff                                       06
112000140423     c                   else
112100140423     c                   write     STO4RI
112200140423     c                   write     STO4TOT
112300140423     c  n06              write     STO4RI
112400140423     c                   endif
112500140514     c                   else
112600140514     c                   if        *in08
112700140514     c                   write     sto4te2
112800140514     c                   write     STO4TEF2
112900140514     c                   write     STO4RI2
113000140514     c                   setoff                                       08
113100140514     c                   else
113200140514     c                   write     STO4RI2
113300140514     c                   write     STO4TOT2
113400140514     c  n08              write     STO4RI2
113500140514     c                   endif
113600140514     c                   endif
113700140423     c                   ENDSR
113800000000     C*------------------------------------------------------------------------*
113900000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
114000000000     C*------------------------------------------------------------------------*
114100000000     C     CARORG        BEGSR
114200000000     C*
114300140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
114400000000     C  N99              READ      AZORG01L                               99
114500000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
114600000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
114700000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
114800000000     C     ORGFAG        IFNE      'V'
114900051109     C                   ADD       1             I
115000000000     C                   Z-ADD     ORGFIL        FIL(I)
115100000000     C                   Z-ADD     ORGCAR        ARE(I)
115200140404     C* CALCOLA NETWORK
115300140404     C                   MOVEL     ORGDE3        OG143
115400140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
115500051109     c* area
115600051109     C                   Z-ADD     CODUT         KTBUTE
115700051109     C                   MOVEL     '05'          Ktbcod
115800051109     C                   MOVEL(P)  ORGCAR        KKEY
115900051109     C     KTAB          CHAIN     TABEL00F                           30
116000051109    2C     *IN30         IFEQ      *OFF
116100051109     C     TBLFLG        ANDEQ     *BLANKS
116200051109     C                   MOVEL     TBLUNI        DS05
116300140408     C***                MOVEL     §05DES        dare(I)
116400140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
116500051109     C                   ELSE
116600140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
116700140408     C***                MOVE      *BLANKS       dare(I)
116800051109    2C                   END
116900051109     c* p.o.
117000051109     C                   MOVEl     orgdes        dfil(I)
117100051109     c
117200000000     C                   END
117300000000     C                   END                                                    <-- 2 ---
117400000000     C                   READ      AZORG01L                               99
117500000000     C                   END                                                    <-- 1 ---
117600000000     C*
117700000000     C                   ENDSR
117800000000     C*------------------------------------------------------------------------*
117900051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
118000000000     C*------------------------------------------------------------------------*
118100000000     C     CARTBL        BEGSR
118200140516     C
118300140516     c* apertura file distinte di filiale
118400140516     c                   if        partser='C'
118500140516     C                   OPEN(e)   FIDST01L
118600140516     c
118700140516     c                   if        not %open(FIDST01L)
118800140516     c                   eval      %subst(WFDST:7:4)='PRD '
118900140516     c                   eval      %subst(TITAS:7:4)='GRPS'
119000140516     c                   eval      %subst(FIAR5:7:4)='GRPS'
119100140516     C                   OPEN      FIDST01L
119200140516     c                   endif
119300140516
119400140516     C                   OPEN(e)   TITAS26C
119500140516     C                   OPEN(e)   FIAR531C
119600140516     c                   open      tisto4p
119700140516     c                   open      tisto4p1
119800140516     c                   else
119900140516     c                   open      fnorf06l
120000140516     c                   open      fnorm01l
120100140516     c                   open      tisto4p2
120200140516     c                   open      tisto4p3
120300140516     c                   endif
120400140516     C
120500000000     C*--------------------
120600051109     C* TIPI BOLLA di recupero
120700000000     C*--------------------
120800000000     C                   Z-ADD     0             I
120900000000     C                   Z-ADD     CODUT         KTBUTE
121000000000     C                   MOVEL     'TB'          KTBCOD
121100000000     C     KEYTAB        SETLL     TABEL00F
121200051109     C                   READe     TABEL00F                               99
121300000000     C     *IN99         DOWEQ     *OFF
121400000000     C     TBLFLG        IFEQ      *BLANKS
121500051109     C                   MOVEL     TBLUNI        DSTB
121600051109     c                   if        §tbrbl = 'R'
121700051109     C                   ADD       1             I
121800051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
121900051109     c                   endif
122000000000     C                   END
122100051109     C     keytab        READe     TABEL00F                               99
122200000000     C                   END
122300000000     C*
122400140428     c* recupero versione cappario alla data DAL
122500140428     C                   Z-ADD     pardsd        P96DRI            8 0
122600140428     C                   Z-ADD     *ZEROS        P96VER            5 0
122700140428     C                   CALL      'TISI96R'
122800140428     C                   PARM                    P96DRI
122900140428     C                   PARM                    P96VER
123000140428     c* carico gli orari in schiera
123100140428     c                   z-add     1             yy
123200140428     c                   if        paror1 >0
123300140428     c                   eval      or_dal(yy)=paror1
123400140428     c                   add       1             yy
123500140428     c                   endif
123600140428     c                   if        paror2 >0
123700140428     c                   eval      or_dal(yy)=paror2
123800140428     c                   add       1             yy
123900140428     c                   endif
124000140428     c                   if        paror3 >0
124100140428     c                   eval      or_dal(yy)=paror3
124200140428     c                   add       1             yy
124300140428     c                   endif
124400140428     c                   if        paror4 >0
124500140428     c                   eval      or_dal(yy)=paror4
124600140428     c                   add       1             yy
124700140428     c                   endif
124800140428     c                   if        paror5 >0
124900140428     c                   eval      or_dal(yy)=paror5
125000140428     c                   add       1             yy
125100140428     c                   endif
125200140428     c                   if        paror6 >0
125300140428     c                   eval      or_dal(yy)=paror6
125400140428     c                   add       1             yy
125500140428     c                   endif
125600140428     c                   if        paror7 >0
125700140428     c                   eval      or_dal(yy)=paror7
125800140428     c                   add       1             yy
125900140428     c                   endif
126000140428     c                   if        paror8 >0
126100140428     c                   eval      or_dal(yy)=paror8
126200140428     c                   add       1             yy
126300140428     c                   endif
126400140428     c                   if        paror9 >0
126500140428     c                   eval      or_dal(yy)=paror9
126600140428     c                   add       1             yy
126700140428     c                   endif
126800140428     C*
126900000000     C                   ENDSR
127000140512     C*------------------------------------------------------------------------*
127100140512     C* Ciclo di lettura su FNORF
127200140512     C*------------------------------------------------------------------------*
127300140512     c     LET_ORF       BEGSR
127400140512     C                   MOVEL     '0'           FINL00            1
127500140512     c
127600140512     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
127700140512     C     FINL00        IFEQ      '0'                                          --- 1 -->
127800140512     C*
127900140512     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
128000140512     C                   EXSR      SETOrf
128100140512     C*
128200140512     C* CICLO FINO A FINE FILE
128300140512     C     FINL00        DOWEQ     '0'                                          --- 2 -->
128400140512     C*
128500140512     C* CREA IL RECORD SUI FILE DI LAVORO
128600140512     C                   EXSR      CRErecR
128700140512     C*
128800140512     C* LEGGE IL RECORD SUCCESSIVO
128900140512     C                   EXSR      LETOrf
129000140512     C                   ENDDO                                                  <-- 2 ---
129100140512     C*
129200140512     C                   ENDIF                                                  <-- 1 ---
129300140512     c                   ENDSR
129400140512     C*------------------------------------------------------------------------*
129500140514     C* SETorf - POSIZIONAMENTO SUL FILE
129600140512     C*------------------------------------------------------------------------*
129700140512     C     SETOrf        BEGSR
129800140512     C*
129900140512     C* POSIZIONAMENTO E PRIMA LETTURA
130000140512     C*
130100140512     C                   movel     pardsd        Kdae
130200140512     C*
130300140512     C     Korf          SETLL     FNORF06L                           99        *FINE FILE
130400140512     C*
130500140512     C     *IN99         IFEQ      '1'
130600140512     C                   MOVEL     '1'           FINL00
130700140512     C                   ELSE
130800140512     C                   EXSR      LETOrf
130900140512     C                   END
131000140512     C*
131100140512     C                   ENDSR
131200140512     C*------------------------------------------------------------------------*
131300140512     C* LETFIL - LETTURA PROSSIMO RECORD
131400140512     C*------------------------------------------------------------------------*
131500140512     C     LETOrf        BEGSR
131600140512     C*
131700140512     C* LEGGO FINO A:
131800140512     C*     - FINE FILE O SUPERATO LIMITE FINALE
131900140512     C*     - TROVATO RECORD VALIDO
132000140512     C                   MOVEL     'N'           WRECOK
132100140512    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
132200140512     C     WRECOK        ANDEQ     'N'
132300140512     C*
132400140512     C*
132500140512     C     Korfp         READE     fnorf06l                               99
132600140512     c
132700140512    3C                   IF        *IN99  OR orfdae>PARDSA
132800140512     C                   MOVEL     '1'           FINL00
132900140514     c                   else
133000140512     c
133100140512     C                   EXSR      CHKREC_O                                     *CONTR.VALIDITA REC.
133200140512    2C                   END                                                    <-- 1 ---
133300140512     C*
133400140512    1C                   ENDDO                                                  <-- 1 ---
133500140512     C*
133600140512     C                   ENDSR
133700140512     C*------------------------------------------------------------------------*
133800140512     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD per Ritiri
133900140512     C*------------------------------------------------------------------------*
134000140512     C     CHKREC_O      BEGSR
134100140512     C*
134200140512     C                   MOVEL     'S'           WRECOK            1
134300140512     C*
134400140512     c* Escludo i ritiri contestuali alla consegna
134500140512     c                   if        orfnsr>0
134600140512     C                   MOVEL     'N'           WRECOK
134700140512     c                   endif
134800140512     c* Escludo gli ORM pronta merce: stabiliti col cliente fuori dagli orari std
134900140514     c     korm          chain     fnorm01l
135000140514     c                   if        %found(fnorm01l) and ormorr>0
135100140514     C****               MOVEL     'N'           WRECOK
135200140512     c                   endif
135300140512     C*
135400140512     C                   ENDSR
135500140512     C*------------------------------------------------------------------------*
135600140512     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO  per i ritiri
135700140512     C*------------------------------------------------------------------------*
135800140512     C     CRErecR       BEGSR
135900140515
136000140515     c                   movel     orfore        wora
136100140512     c                   clear                   kloc
136200140512     c
136300140512     c* Richiamo pgm reperimento orari servizi
136400140512     c                   clear                   trulorsds
136500140512     c                   clear                   trulor2ds
136600140516     c                   clear                   trulor3ds
136700140512     c                   eval      ioredta=ormdar
136800140512     c                   eval      iorefil=orfpog
136900140512     c                   eval      iorenar=ormnar
137000140512     c                   eval      iorecap=ormcar
137100140516     c                   eval      ioreloc=ormlor
137200140512     c                   eval      ioretser=partser
137300140516     c*****              eval      IOREgf2 = O95gf2
137400140514     c                   eval      ioretsp ='C'
137500140512     c                   call      'TRULORSR'
137600140512     c                   parm                    kpjba
137700140512     c                   parm                    trulorsds
137800140512     c                   parm                    trulor2ds
137900140516     c                   parm                    trulor3ds
138000140516     c                   eval      wnorm=OOR3NORM
138100140516     c
138200140516     c                   if        oor3norm='S'
138300140516     c                   eval      kloc =OOR3LOC_N
138400140516     c                   endif
138500140512     c*
138600140512     c                   clear                   kserv
138700140514     c* se l'orario di ritiro  non rientra negli orari standard verifico se orario
138800140514     c* concordato col cliente ed in questo caso scarto il record
138900140515     c                   if        (wora  <OOR2STIS  and
139000140514     c                              ormcra>0 and
139100140514     c                              ormorr<oor2stis)     or
139200140515     c                             (wora  >OOR2STFS  and
139300140514     c                              ormcra>0 and
139400140514     c                              ormorr>oor2stfs)
139500140514     c                   leavesr
139600140514     c                   endif
139700140512
139800140512     c     kccosR        chain     wfccos1l
139900140512     c                   if        not %found(wfccos1l)
140000140512     c                   clear                   wfccos00
140100140512     c                   eval      wcosdcmd=pardsd
140200140512     c                   eval      wcosdcma=pardsa
140300140512     c                   eval      wcosver =p96ver
140400140512     c                   eval      wcoslna =kfil
140500140512     c                   eval      wcosnaz =ormnar
140600140512     c                   eval      wcoscap =ormcar
140700140513     c                   eval      wcosloc =kloc
140800140512     c                   eval      wcostser=partser
140900140512     c                   eval      wcosorif=OOR2ORIF
141000140512     c                   eval      wcosserv=kserv
141100140512     c* imposta campi cap/località obsoleto
141200140512     c                   exsr      sr_impobs
141300140512     c* Imposto gli orari nel file
141400140512     c                   eval      WCOSOSTIS=OOR2STIS
141500140512     c                   eval      WCOSOSTfS=OOR2STfS
141600140512     c
141700140512     c* imposto orari dettaglio se ci sono e richiesto
141800140512     c                   if        pardet='S'
141900140512     c                   exsr      sr_ImpOrDet
142000140512     c                   endif
142100140512     c                   endif
142200140512     c
142300140512     c                   add       1             wcosts
142400140512     c* Se loc non normalizzata
142500140516     c                   if        wnorm<>'S'
142600140512     c                   add       1             wcostsNN
142700140512     c                   endif
142800140512     c
142900140512     c*** serve in questo caso? E' uguale a wcosts....
143000140512     c                   add       1             wcostse
143100140512     c
143200140512     c* ritiro negli orari std
143300140515     c                   if        wora  >=OOR2STIS  and wora  <=OOR2STFS
143400140512     c                   add       1             wcostsos
143500140512     c                   endif
143600140512     c* imposto campi dettaglio
143700140513     c                   exsr      ImpoDETuni
143800140512     c
143900140512     c                   if        not %found(wfccos1l)
144000140512     c                   write     wfccos00
144100140512     c                   else
144200140512     c                   update    wfccos00
144300140512     c                   endif
144400140512     C*
144500140512     C                   ENDSR
144600140513     C*------------------------------------------------------------------------*
144700140513     C* Imposto i campi di dettaglio in base ai range passati
144800140513     C*------------------------------------------------------------------------*
144900140513     C     ImpoDETUni    BEGSR
145000140513     c*
145100140513     c                   if        wora  <or_dal(1)
145200140513     c                   add       1             wcosmint
145300140513     c                   leavesr
145400140513     c                   endif
145500140513     c                   if        wora  > paroltre
145600140513     c                   add       1             wcosmaxt
145700140513     c                   leavesr
145800140513     c                   endif
145900140513
146000140513     c                   z-add     1             yy
146100140513     c                   dow       or_dal(yy)>0
146200140513     c                   if        wora  >=or_dal(yy) and
146300140513     c
146400140513     c                             (or_dal(yy+1)>0 and
146500140513     c                              wora  < or_dal(Yy+1)  or
146600140513     c                             (or_dal(yy+1)=0 and
146700140513     c                             wora  <=paroltre))
146800140513     c                   select
146900140513     c                   when      yy=1
147000140513     c                   add       1             wcosor1t
147100140513     c                   when      yy=2
147200140513     c                   add       1             wcosor2t
147300140513     c                   when      yy=3
147400140513     c                   add       1             wcosor3t
147500140513     c                   when      yy=4
147600140513     c                   add       1             wcosor4t
147700140513     c                   when      yy=5
147800140513     c                   add       1             wcosor5t
147900140513     c                   when      yy=6
148000140513     c                   add       1             wcosor6t
148100140513     c                   when      yy=7
148200140513     c                   add       1             wcosor7t
148300140513     c                   when      yy=8
148400140513     c                   add       1             wcosor8t
148500140513     c                   when      yy=9
148600140513     c                   add       1             wcosor9t
148700140513     c                   endsl
148800140513     c
148900140513     c                   leavesr
149000140513     c                   endif
149100140513     c                   add       1             yy
149200140513     c
149300140513     c                   enddo
149400140513     c
149500140513     c                   ENDSR
149600140512     C*------------------------------------------------------------------------*
149700140512     C* Imposta campi "obsloleto"
149800140512     C*------------------------------------------------------------------------*
149900140512     C     sr_impobs     BEGSR
150000140512     c* obsoleto
150100140512     c     kcpc          chain     azcpc01l
150200140512     c                   if        %found(azcpc01l)
150300140512     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
150400140512     c                   endif
150500140512     c                   if        wcosloc<>*blanks
150600140512     c     kcpl          chain     azcpl01l
150700140512     c                   if        %found(azcpl01l)
150800140512     c                   eval      wcosloco=%subst(cplfi1:1:1)
150900140512     c                   endif
151000140512     c                   endif
151100140512     c                   endsr
151200140512     C*------------------------------------------------------------------------*
151300140512     C* Imposta campi orari dettaglio
151400140512     C*------------------------------------------------------------------------*
151500140512     C     sr_ImpOrDet   BEGSR
151600140512     c                   eval      wcosor1=or_dal(1)
151700140512     c                   eval      wcosor2=or_dal(2)
151800140512     c                   eval      wcosor3=or_dal(3)
151900140512     c                   eval      wcosor4=or_dal(4)
152000140512     c                   eval      wcosor5=or_dal(5)
152100140512     c                   eval      wcosor6=or_dal(6)
152200140512     c                   eval      wcosor7=or_dal(7)
152300140512     c                   eval      wcosor8=or_dal(8)
152400140512     c                   eval      wcosor9=or_dal(9)
152500140512     c                   eval      wcosorOl=paroltre
152600140512     c                   endsr
152700000000     C*------------------------------------------------------------------------*
152800000000     C* *INZSR - ROUTINE INIZIALE
152900000000     C*------------------------------------------------------------------------*
153000000000     C     *INZSR        BEGSR
153100000000     C*--------------------
153200000000     C* RICEVIMENTO PARAMETRI
153300000000     C*--------------------
153400140408     c
153500140408     c     *dtaara       define    §azute        azuteds
153600140408     c     *dtaara       define    §datiute      ddatiute
153700140408     c                   in(E)     *dtaara
153800140408     c                   If        %error  or RSUT = *blanks
153900140408     c                   CLEAR                   tibs34ds
154000140408     c                   CALL      'TIBS34R'
154100140408     c                   PARM                    tibs34ds
154200140408     c                   in        *dtaara
154300140408     c                   EndIf
154400140408     c
154500140408     c                   eval      p1drsut=rsut
154600140408     C*
154700000000     C*--------------------
154800000000     C* CAMPI RIFERITI AL DATA BASE
154900000000     C*--------------------
155000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
155100000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
155200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
155300051109     C     *LIKE         DEFINE    TBLkey        Kkey
155400000000     C*--------------------
155500000000     C* CHIAVI DI LETTURA
155600000000     C*--------------------
155700140404     C* CHIAVE LETTURA TITAS26C - COMPLETA
155800140404     C     Ktas          KLIST
155900140404     C                   KFLD                    Kdcm
156000140404     C                   KFLD                    Kfil
156100140512     C* CHIAVE LETTURA FNORF06L - COMPLETA
156200140512     C     Korf          KLIST
156300140512     C                   KFLD                    Kfil
156400140512     C                   KFLD                    Kfar
156500140512     C                   KFLD                    Kdae
156600140514     C*
156700140514     C     Korfp         KLIST
156800140514     C                   KFLD                    Kfil
156900140514     C                   KFLD                    Kfar
157000140514     C     Korm          KLIST
157100140514     C                   KFLD                    orfpoe
157200140514     C                   KFLD                    orfnsr
157300140514     C                   KFLD                    orfnor
157400140514     C                   KFLD                    orfnrv
157500000000     C*
157600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
157700000000     C     KEYTAB        KLIST
157800000000     C                   KFLD                    KTBUTE
157900000000     C                   KFLD                    KTBCOD
158000051109     C     KTAB          KLIST
158100051109     C                   KFLD                    ktbute
158200051109     C                   KFLD                    ktbcod
158300051109     C                   KFLD                    KKEY
158400000000     C*
158500060524     C     Kar5          KLIST
158600060524     C                   KFLD                    tasAAS
158700060524     C                   KFLD                    taslnp
158800060524     C                   KFLD                    tasnrs
158900060524     C                   KFLD                    tasnsp
159000060524     c                   kfld                    kar5trd
159100140404     C*
159200140404     C     Kdst          KLIST
159300140404     C                   KFLD                    Knpg
159400140404     C                   KFLD                    knFV
159500140404     C                   KFLD                    Kfgs
159600140404     c
159700140415     C     KCCOS         KLIST
159800140415     C                   KFLD                    partser
159900140423     C                   KFLD                    d55tfa
160000140404     C                   KFLD                    tasnzd
160100140404     C                   KFLD                    tascad
160200140404     C                   KFLD                    Kloc
160300140415     C                   KFLD                    kserv
160400140514     C     KCCOSr        KLIST
160500140514     C                   KFLD                    partser
160600140514     C                   KFLD                    kfil
160700140514     C                   KFLD                    ormnar
160800140514     C                   KFLD                    ormcar
160900140514     C                   KFLD                    Kloc
161000140514     C                   KFLD                    kserv
161100140424     C     Kcpc          KLIST
161200140424     C                   KFLD                    p96ver
161300140424     C                   KFLD                    wcosnaz
161400140424     C                   KFLD                    wcoscap
161500140424     C     Kcpl          KLIST
161600140424     C                   KFLD                    p96ver
161700140424     C                   KFLD                    wcosnaz
161800140424     C                   KFLD                    wcoscap
161900140424     C                   KFLD                    wcosloc
162000000000     C*--------------------
162100000000     C                   Z-ADD     1             CODUT             1 0
162200000000     C*
162300000000     C***
162400000000     C* CARICA ORGANIGRAMMA
162500000000     C                   EXSR      CARORG
162600000000     C*
162700000000     C                   ENDSR
