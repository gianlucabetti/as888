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
065600140619     c                   if        wcostsos>0
065700140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
065800140408     c                   eval      p1dstsos=WCOSTSOs
065900140619     c                   else
066000140619     c                   clear                   p1dperc_os
066100140619     c                   clear                   p1dstsos
066200140619     c                   endif
066300140404     c* Calcolo % MIN/MAX
066400140619     c                   if        wcostsom>0
066500140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
066600140408     c                   eval      p1dstsom=WCOSTSOm
066700140619     c                   else
066800140619     c                   clear                   p1dperc_om
066900140619     c                   clear                   p1dstsom
067000140619     c                   endif
067100140408     c
067200140408     c                   else
067300140408     c* voglio la % delle fuori orario
067400140408     c                   eval      wfuori_os=wcostse-wcostsos
067500140619     c                   if        wfuori_OS>0
067600140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
067700140408     c                   eval      p1dstsos=Wfuori_os
067800140619     c                   else
067900140619     c                   clear                   p1dperc_os
068000140619     c                   clear                   p1dstsos
068100140619     c                   endif
068200140408
068300140408     c                   eval      wfuori_om=wcostse-wcostsom
068400140619     c                   if        wfuori_OM>0
068500140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
068600140408     c                   eval      p1dstsom=Wfuori_om
068700140619     c                   else
068800140619     c                   clear                   p1dperc_om
068900140619     c                   clear                   p1dstsom
069000140619     c                   endif
069100140408     c                   endif
069200140408     c
069300140407     c* stampo solo al di sotto della % richiesta
069400140408     c                   eval      wstampadet='S'
069500140408
069600140408     c                   select
069700140408     c                   when      parperc=0
069800140408
069900140408     c                   when      parorari='S'  and parcor='C' and
070000140408     c                             p1dperc_os>=parperc
070100140408     c                   eval      wstampadet='N'
070200140408     c
070300140408     c                   when      parorari='S'  and parcor='F' and
070400140408     c                             p1dperc_os< parperc
070500140408     c                   eval      wstampadet='N'
070600140408
070700140408     c                   when      parorari='M'  and parcor='C' and
070800140408     c                             p1dperc_om>=parperc
070900140408     c                   eval      wstampadet='N'
071000140408     c
071100140408     c                   when      parorari='M'  and parcor='F' and
071200140408     c                             p1dperc_om< parperc
071300140408     c                   eval      wstampadet='N'
071400140408     c                   endsl
071500140404     c*
071600140408     c                   if        wstampadet='S'
071700140424     c* POM
071800140424     c                   if        wcosserv='P '
071900140424     c                   eval      p1dpom='Pom'
072000140424     c                   else
072100140424     c                   eval      p1dpom='   '
072200140424     c                   endif
072300140411     c*
072400140411     c* Se la località è vuota imposto '??'
072500140411     c                   if        wcosloc=*blanks
072600140411     c                   eval      wcosloc='??'
072700140411     c                   endif
072800140408     c
072900140408     c                   if        *in06
073000140408     c                   write     sto4te
073100140408     c                   write     STO4TEF
073200140414     c                   write     STO4RI
073300140408     c                   setoff                                       06
073400140404     c                   endif
073500140514
073600140514     c                   if        *in08
073700140514     c                   write     sto4te2
073800140514     c                   write     STO4TEF2
073900140514     c                   write     STO4RI2
074000140514     c                   setoff                                       08
074100140514     c                   endif
074200140408     c
074300140424     c                   eval      p1dstis=wcosoSTiS
074400140424     c                   eval      p1dstfs=wcosoSTFS
074500140424     c                   eval      p1dmiis=wcosomiis
074600140424     c                   eval      p1dmxfs=wcosomxfs
074700140404     c
074800140514     c                   if        partser='C'
074900140408     c                   write     STO4det
075000140514     c                   else
075100140514     c                   write     STO4det2
075200140514     c                   endif
075300140422     c
075400140422     c* se devo stampare anche il dettaglio
075500140422     c                   if        pardet='S'
075600140422     c                   exsr      Stadet
075700140422     c                   endif
075800140408     c
075900140407     c                   endif
076000140408     c                   endif
076100140424     c* totali di filiale
076200140424     c                   add       WCOSTS        tot_ts
076300140424     c                   add       WCOSTSE       tot_tse
076400140424     c                   add       WCOSTSos      tot_tsos
076500140424     c                   add       WCOSTSom      tot_tsom
076600140407     c
076700140514     c     partser       reade     wfccos1l
076800140404     c
076900140404     c                   enddo
077000140423     c
077100140423     c* totali finali
077200140423     c                   if        savlna>0
077300140423     c                   exsr      totfil
077400140423     c                   endif
077500140404     c
077600140404     c                   ENDSR
077700140423     C*------------------------------------------------------------------------*
077800140423     c* Imposta gli orari in testata dettaglioù
077900140423     C*------------------------------------------------------------------------*
078000140423     c     TestaDet      BEGSR
078100140423     c                   eval      p1dsot = paror1
078200140423     c                   eval      p1doltre= paroltre
078300140423     c                   eval      p1dda1  = or_dal(1)
078400140423
078500140423     c                   if        or_dal(2)>0
078600140423     c                   eval      p1dda2  = or_dal(2)
078700140423     c                   movel     or_dal(2)     w0060
078800140423     c                   move      w0060         timeiso
078900140423     c                   subdur    1:*mn         timeiso
079000140423     c                   eval      w0060     =%dec(timeiso)
079100140423     c                   movel     w0060         p1dal1
079200140423     c                   else
079300140423     c                   eval      p1dal1=paroltre
079400140423     c                   leavesr
079500140423     c                   endif
079600140423     c
079700140423     c                   if        or_dal(3)>0
079800140423     c                   eval      p1dda3  = or_dal(3)
079900140423     c                   movel     or_dal(3)     w0060
080000140423     c                   move      w0060         timeiso
080100140423     c                   subdur    1:*mn         timeiso
080200140423     c                   eval      w0060     =%dec(timeiso)
080300140423     c                   movel     w0060         p1dal2
080400140423     c                   else
080500140423     c                   eval      p1dal2=paroltre
080600140423     c                   leavesr
080700140423     c                   endif
080800140423
080900140423     c                   if        or_dal(4)>0
081000140423     c                   eval      p1dda4  = or_dal(4)
081100140423     c                   movel     or_dal(4)     w0060
081200140423     c                   move      w0060         timeiso
081300140423     c                   subdur    1:*mn         timeiso
081400140423     c                   eval      w0060     =%dec(timeiso)
081500140423     c                   movel     w0060         p1dal3
081600140423     c                   else
081700140423     c                   eval      p1dal3=paroltre
081800140423     c                   leavesr
081900140423     c                   endif
082000140423
082100140423     c                   if        or_dal(5)>0
082200140423     c                   eval      p1dda5  = or_dal(5)
082300140423     c                   movel     or_dal(5)     w0060
082400140423     c                   move      w0060         timeiso
082500140423     c                   subdur    1:*mn         timeiso
082600140423     c                   eval      w0060     =%dec(timeiso)
082700140423     c                   movel     w0060         p1dal4
082800140423     c                   else
082900140423     c                   eval      p1dal4=paroltre
083000140423     c                   leavesr
083100140423     c                   endif
083200140423
083300140423     c                   if        or_dal(6)>0
083400140423     c                   eval      p1dda6  = or_dal(6)
083500140423     c                   movel     or_dal(6)     w0060
083600140423     c                   move      w0060         timeiso
083700140423     c                   subdur    1:*mn         timeiso
083800140423     c                   eval      w0060     =%dec(timeiso)
083900140423     c                   movel     w0060         p1dal5
084000140423     c                   else
084100140423     c                   eval      p1dal5=paroltre
084200140423     c                   leavesr
084300140423     c                   endif
084400140423
084500140423     c                   if        or_dal(7)>0
084600140423     c                   eval      p1dda7  = or_dal(7)
084700140423     c                   movel     or_dal(7)     w0060
084800140423     c                   move      w0060         timeiso
084900140423     c                   subdur    1:*mn         timeiso
085000140423     c                   eval      w0060     =%dec(timeiso)
085100140423     c                   movel     w0060         p1dal6
085200140423     c                   else
085300140423     c                   eval      p1dal6=paroltre
085400140423     c                   leavesr
085500140423     c                   endif
085600140423
085700140423     c                   if        or_dal(8)>0
085800140423     c                   eval      p1dda8  = or_dal(8)
085900140423     c                   movel     or_dal(8)     w0060
086000140423     c                   move      w0060         timeiso
086100140423     c                   subdur    1:*mn         timeiso
086200140423     c                   eval      w0060     =%dec(timeiso)
086300140423     c                   movel     w0060         p1dal7
086400140423     c                   else
086500140423     c                   eval      p1dal7=paroltre
086600140423     c                   leavesr
086700140423     c                   endif
086800140423     c                   if        or_dal(9)>0
086900140423     c                   eval      p1dda9  = or_dal(9)
087000140423     c                   movel     or_dal(9)     w0060
087100140423     c                   move      w0060         timeiso
087200140423     c                   subdur    1:*mn         timeiso
087300140423     c                   eval      w0060     =%dec(timeiso)
087400140423     c                   movel     w0060         p1dal8
087500140423     c
087600140423     c                   eval      p1dal9=paroltre
087700140423     c                   else
087800140423     c                   eval      p1dal8=paroltre
087900140423     c                   leavesr
088000140423     c                   endif
088100140423     c
088200140423     c                   ENDSR
088300140422     C*------------------------------------------------------------------------*
088400140422     C* Stampa dettaglio
088500140422     C*------------------------------------------------------------------------*
088600140422     C     StaDET        BEGSR
088700140422     c                   if        *in07
088800140422     c                   write     sto4te_d
088900140422     c                   write     STO4TEF_d
089000140422     c                   write     STO4RI_d
089100140422     c                   setoff                                       07
089200140422     c                   endif
089300140514
089400140514     c                   if        *in09
089500140514     c                   write     sto4te_d3
089600140514     c                   write     STO4TEF_d3
089700140514     c                   write     STO4RI_d3
089800140514     c                   setoff                                       09
089900140514     c                   endif
090000140424     c* Imposto gli orari
090100140424     c                   if        parorari='S'
090200140424     c                   eval      p1dor_dal=WCOSOSTIS
090300140424     c                   eval      p1dor_al =WCOSOSTfS
090400140424     c                   else
090500140424     c                   eval      p1dor_dal=WCOSOmiis
090600140424     c                   eval      p1dor_al =WCOSOmxfS
090700140424     c                   endif
090800140424     c                   setoff                                       111213
090900140424     c                   setoff                                       141516
091000140424     c                   setoff                                       171819
091100140505     c                   setoff                                       2021
091200140422     c* calcolo le %
091300140422     c* SOTTO
091400140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
091500140422     C                   IF        WPERC>0
091600140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
091700140505     c                   seton                                        20
091800140422     c                   else
091900140424     c                   eval      p1cp_sot='    '
092000140422     c                   endif
092100140422     c* OLTRE
092200140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
092300140422     C                   IF        WPERC>0
092400140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
092500140505     c                   seton                                        21
092600140422     c                   else
092700140424     c                   eval      p1cp_olt='    '
092800140422     c                   endif
092900140422     c* % 1
093000140505    1c                   if        wcosor1  >0
093100140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
093200140422     c
093300140505    2C                   IF        WPERC>0
093400140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
093500140505     c
093600140505    2c                   select
093700140505     c                   when      or_dal(1)<p1dor_dal
093800140505     c                   seton                                        11
093900140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
094000140505     c                   seton                                        11
094100140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
094200140505     c                   seton                                        11
094300140505    2c                   endsl
094400140505
094500140422     c                   else
094600140424     c                   eval      p1cp_o1 ='    '
094700140505    2c                   endif
094800140424     c
094900140505    1c                   endif
095000140422     c
095100140422     c* % 2
095200140422     c                   if        wcosor2  >0
095300140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
095400140422     c
095500140422     C                   IF        WPERC>0
095600140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
095700140505
095800140505     c                   select
095900140505     c                   when      or_dal(2)<p1dor_dal
096000140505     c                   seton                                        12
096100140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
096200140505     c                   seton                                        12
096300140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
096400140505     c                   seton                                        12
096500140505     c                   endsl
096600140505
096700140422     c                   else
096800140424     c                   eval      p1cp_o2 ='    '
096900140422     c                   endif
097000140422     c                   endif
097100140423     c* % 3
097200140423     c                   if        wcosor3  >0
097300140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
097400140423     c
097500140423     C                   IF        WPERC>0
097600140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
097700140505
097800140505     c                   select
097900140505     c                   when      or_dal(3)<p1dor_dal
098000140505     c                   seton                                        13
098100140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
098200140505     c                   seton                                        13
098300140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
098400140505     c                   seton                                        13
098500140505     c                   endsl
098600140423     c                   else
098700140424     c                   eval      p1cp_o3 ='    '
098800140423     c                   endif
098900140423     c                   endif
099000140423     c* % 4
099100140423     c                   if        wcosor4  >0
099200140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
099300140423     c
099400140423     C                   IF        WPERC>0
099500140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
099600140505
099700140505     c                   select
099800140505     c                   when      or_dal(4)<p1dor_dal
099900140505     c                   seton                                        14
100000140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
100100140505     c                   seton                                        14
100200140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
100300140505     c                   seton                                        14
100400140505     c                   endsl
100500140423     c                   else
100600140424     c                   eval      p1cp_o4 ='    '
100700140423     c                   endif
100800140423     c                   endif
100900140423     c* % 5
101000140423     c                   if        wcosor5  >0
101100140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
101200140423     c
101300140423     C                   IF        WPERC>0
101400140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
101500140505
101600140505     c                   select
101700140505     c                   when      or_dal(5)<p1dor_dal
101800140505     c                   seton                                        15
101900140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
102000140505     c                   seton                                        15
102100140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
102200140505     c                   seton                                        15
102300140505     c                   endsl
102400140423     c                   else
102500140424     c                   eval      p1cp_o5 ='    '
102600140423     c                   endif
102700140423     c                   endif
102800140423     c* % 6
102900140423     c                   if        wcosor6  >0
103000140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
103100140423     c
103200140423     C                   IF        WPERC>0
103300140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
103400140505
103500140505     c                   select
103600140505     c                   when      or_dal(6)<p1dor_dal
103700140505     c                   seton                                        16
103800140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
103900140505     c                   seton                                        16
104000140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
104100140505     c                   seton                                        16
104200140505     c                   endsl
104300140423     c                   else
104400140424     c                   eval      p1cp_o6 ='    '
104500140423     c                   endif
104600140423     c                   endif
104700140423     c* % 7
104800140423     c                   if        wcosor7  >0
104900140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
105000140423     c
105100140423     C                   IF        WPERC>0
105200140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
105300140505
105400140505     c                   select
105500140505     c                   when      or_dal(7)<p1dor_dal
105600140505     c                   seton                                        17
105700140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
105800140505     c                   seton                                        17
105900140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
106000140505     c                   seton                                        17
106100140505     c                   endsl
106200140423     c                   else
106300140424     c                   eval      p1cp_o7 ='    '
106400140423     c                   endif
106500140423     c                   endif
106600140423     c* % 8
106700140423     c                   if        wcosor8  >0
106800140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
106900140423     c
107000140423     C                   IF        WPERC>0
107100140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
107200140505
107300140505     c                   select
107400140505     c                   when      or_dal(8)<p1dor_dal
107500140505     c                   seton                                        18
107600140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
107700140505     c                   seton                                        18
107800140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
107900140505     c                   seton                                        18
108000140505     c                   endsl
108100140423     c                   else
108200140424     c                   eval      p1cp_o8 ='    '
108300140423     c                   endif
108400140423     c                   endif
108500140423     c* % 9
108600140423     c                   if        wcosor9  >0
108700140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
108800140423     c
108900140423     C                   IF        WPERC>0
109000140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
109100140505
109200140505     c                   select
109300140505     c                   when      or_dal(9)<p1dor_dal
109400140505     c                   seton                                        19
109500140505     c                   when      paroltre >p1dor_al
109600140505     c                   seton                                        19
109700140505     c                   endsl
109800140423     c                   else
109900140424     c                   eval      p1cp_o9 ='    '
110000140423     c                   endif
110100140423     c                   endif
110200140422     c
110300140514     c                   if        partser='C'
110400140422     c                   write     STO4det_d
110500140514     c                   else
110600140514     c                   write     STO4det_d3
110700140514     c                   endif
110800140422     c                   ENDSR
110900140423     C*------------------------------------------------------------------------*
111000140423     c* Stampa totali di filiale
111100140423     C*------------------------------------------------------------------------*
111200140423     c     TOTfil        BEGSR
111300140423     c* voglio la % di correttezza
111400140423     c                   if        parcor='C'
111500140423     c* Calcolo % STD
111600140619     c                   if        tot_tsos>0
111700140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
111800140423     c                   eval      p1dstsos=tot_TSOs
111900140619     c                   else
112000140619     c                   clear                   p1dperc_os
112100140619     c                   clear                   p1dstsos
112200140619     c                   endif
112300140423     c* Calcolo % MIN/MAX
112400140619     c                   if        tot_tsom>0
112500140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
112600140423     c                   eval      p1dstsom=tot_TSOm
112700140619     c                   else
112800140619     c                   clear                   p1dperc_om
112900140619     c                   clear                   p1dstsom
113000140619     c                   endif
113100140423     c
113200140423     c                   else
113300140423     c* voglio la % delle fuori orario
113400140423     c                   eval      wfuori_os=tot_tse-tot_tsos
113500140619     c                   if        wfuori_os>0
113600140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
113700140423     c                   eval      p1dstsos=Wfuori_os
113800140619     c                   else
113900140619     c                   clear                   p1dperc_os
114000140619     c                   clear                   p1dstsos
114100140619     c                   endif
114200140423
114300140423     c                   eval      wfuori_om=tot_tse-tot_tsom
114400140619     c                   if        wfuori_om>0
114500140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
114600140423     c                   eval      p1dstsom=Wfuori_om
114700140619     c                   else
114800140619     c                   clear                   p1dperc_om
114900140619     c                   clear                   p1dstsom
115000140619     c                   endif
115100140423     c
115200140423     c                   endif
115300140423
115400140514     c                   if        partser='C'
115500140423     c                   if        *in06
115600140423     c                   write     sto4te
115700140423     c                   write     STO4TEF
115800140423     c                   write     STO4RI
115900140423     c                   setoff                                       06
116000140423     c                   else
116100140423     c                   write     STO4RI
116200140930     c                   endif
116300140930     c
116400140423     c                   write     STO4TOT
116500140930     c                   write     STO4RI
116600140514     c                   else
116700140514     c                   if        *in08
116800140514     c                   write     sto4te2
116900140514     c                   write     STO4TEF2
117000140514     c                   write     STO4RI2
117100140514     c                   setoff                                       08
117200140514     c                   else
117300140514     c                   write     STO4RI2
117400140930     c                   endif
117500140514     c                   write     STO4TOT2
117600140930     c                   write     STO4RI2
117700140514     c                   endif
117800140423     c                   ENDSR
117900000000     C*------------------------------------------------------------------------*
118000000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
118100000000     C*------------------------------------------------------------------------*
118200000000     C     CARORG        BEGSR
118300000000     C*
118400140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
118500000000     C  N99              READ      AZORG01L                               99
118600000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
118700000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
118800000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
118900000000     C     ORGFAG        IFNE      'V'
119000051109     C                   ADD       1             I
119100000000     C                   Z-ADD     ORGFIL        FIL(I)
119200000000     C                   Z-ADD     ORGCAR        ARE(I)
119300140404     C* CALCOLA NETWORK
119400140404     C                   MOVEL     ORGDE3        OG143
119500140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
119600051109     c* area
119700051109     C                   Z-ADD     CODUT         KTBUTE
119800051109     C                   MOVEL     '05'          Ktbcod
119900051109     C                   MOVEL(P)  ORGCAR        KKEY
120000051109     C     KTAB          CHAIN     TABEL00F                           30
120100051109    2C     *IN30         IFEQ      *OFF
120200051109     C     TBLFLG        ANDEQ     *BLANKS
120300051109     C                   MOVEL     TBLUNI        DS05
120400140408     C***                MOVEL     §05DES        dare(I)
120500140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
120600051109     C                   ELSE
120700140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
120800140408     C***                MOVE      *BLANKS       dare(I)
120900051109    2C                   END
121000051109     c* p.o.
121100051109     C                   MOVEl     orgdes        dfil(I)
121200051109     c
121300000000     C                   END
121400000000     C                   END                                                    <-- 2 ---
121500000000     C                   READ      AZORG01L                               99
121600000000     C                   END                                                    <-- 1 ---
121700000000     C*
121800000000     C                   ENDSR
121900000000     C*------------------------------------------------------------------------*
122000051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
122100000000     C*------------------------------------------------------------------------*
122200000000     C     CARTBL        BEGSR
122300140516     C
122400140516     c* apertura file distinte di filiale
122500140516     c                   if        partser='C'
122600140516     C                   OPEN(e)   FIDST01L
122700140516     c
122800140516     c                   if        not %open(FIDST01L)
122900140516     c                   eval      %subst(WFDST:7:4)='PRD '
123000140516     c                   eval      %subst(TITAS:7:4)='GRPS'
123100140516     c                   eval      %subst(FIAR5:7:4)='GRPS'
123200140516     C                   OPEN      FIDST01L
123300140516     c                   endif
123400140516
123500140516     C                   OPEN(e)   TITAS26C
123600140516     C                   OPEN(e)   FIAR531C
123700140516     c                   open      tisto4p
123800140516     c                   open      tisto4p1
123900140516     c                   else
124000140516     c                   open      fnorf06l
124100140516     c                   open      fnorm01l
124200140516     c                   open      tisto4p2
124300140516     c                   open      tisto4p3
124400140516     c                   endif
124500140516     C
124600000000     C*--------------------
124700051109     C* TIPI BOLLA di recupero
124800000000     C*--------------------
124900000000     C                   Z-ADD     0             I
125000000000     C                   Z-ADD     CODUT         KTBUTE
125100000000     C                   MOVEL     'TB'          KTBCOD
125200000000     C     KEYTAB        SETLL     TABEL00F
125300051109     C                   READe     TABEL00F                               99
125400000000     C     *IN99         DOWEQ     *OFF
125500000000     C     TBLFLG        IFEQ      *BLANKS
125600051109     C                   MOVEL     TBLUNI        DSTB
125700051109     c                   if        §tbrbl = 'R'
125800051109     C                   ADD       1             I
125900051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
126000051109     c                   endif
126100000000     C                   END
126200051109     C     keytab        READe     TABEL00F                               99
126300000000     C                   END
126400000000     C*
126500140428     c* recupero versione cappario alla data DAL
126600140428     C                   Z-ADD     pardsd        P96DRI            8 0
126700140428     C                   Z-ADD     *ZEROS        P96VER            5 0
126800140428     C                   CALL      'TISI96R'
126900140428     C                   PARM                    P96DRI
127000140428     C                   PARM                    P96VER
127100140428     c* carico gli orari in schiera
127200140428     c                   z-add     1             yy
127300140428     c                   if        paror1 >0
127400140428     c                   eval      or_dal(yy)=paror1
127500140428     c                   add       1             yy
127600140428     c                   endif
127700140428     c                   if        paror2 >0
127800140428     c                   eval      or_dal(yy)=paror2
127900140428     c                   add       1             yy
128000140428     c                   endif
128100140428     c                   if        paror3 >0
128200140428     c                   eval      or_dal(yy)=paror3
128300140428     c                   add       1             yy
128400140428     c                   endif
128500140428     c                   if        paror4 >0
128600140428     c                   eval      or_dal(yy)=paror4
128700140428     c                   add       1             yy
128800140428     c                   endif
128900140428     c                   if        paror5 >0
129000140428     c                   eval      or_dal(yy)=paror5
129100140428     c                   add       1             yy
129200140428     c                   endif
129300140428     c                   if        paror6 >0
129400140428     c                   eval      or_dal(yy)=paror6
129500140428     c                   add       1             yy
129600140428     c                   endif
129700140428     c                   if        paror7 >0
129800140428     c                   eval      or_dal(yy)=paror7
129900140428     c                   add       1             yy
130000140428     c                   endif
130100140428     c                   if        paror8 >0
130200140428     c                   eval      or_dal(yy)=paror8
130300140428     c                   add       1             yy
130400140428     c                   endif
130500140428     c                   if        paror9 >0
130600140428     c                   eval      or_dal(yy)=paror9
130700140428     c                   add       1             yy
130800140428     c                   endif
130900140428     C*
131000000000     C                   ENDSR
131100140512     C*------------------------------------------------------------------------*
131200140512     C* Ciclo di lettura su FNORF
131300140512     C*------------------------------------------------------------------------*
131400140512     c     LET_ORF       BEGSR
131500140512     C                   MOVEL     '0'           FINL00            1
131600140512     c
131700140512     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
131800140512     C     FINL00        IFEQ      '0'                                          --- 1 -->
131900140512     C*
132000140512     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
132100140512     C                   EXSR      SETOrf
132200140512     C*
132300140512     C* CICLO FINO A FINE FILE
132400140512     C     FINL00        DOWEQ     '0'                                          --- 2 -->
132500140512     C*
132600140512     C* CREA IL RECORD SUI FILE DI LAVORO
132700140512     C                   EXSR      CRErecR
132800140512     C*
132900140512     C* LEGGE IL RECORD SUCCESSIVO
133000140512     C                   EXSR      LETOrf
133100140512     C                   ENDDO                                                  <-- 2 ---
133200140512     C*
133300140512     C                   ENDIF                                                  <-- 1 ---
133400140512     c                   ENDSR
133500140512     C*------------------------------------------------------------------------*
133600140514     C* SETorf - POSIZIONAMENTO SUL FILE
133700140512     C*------------------------------------------------------------------------*
133800140512     C     SETOrf        BEGSR
133900140512     C*
134000140512     C* POSIZIONAMENTO E PRIMA LETTURA
134100140512     C*
134200140512     C                   movel     pardsd        Kdae
134300140512     C*
134400140512     C     Korf          SETLL     FNORF06L                           99        *FINE FILE
134500140512     C*
134600140512     C     *IN99         IFEQ      '1'
134700140512     C                   MOVEL     '1'           FINL00
134800140512     C                   ELSE
134900140512     C                   EXSR      LETOrf
135000140512     C                   END
135100140512     C*
135200140512     C                   ENDSR
135300140512     C*------------------------------------------------------------------------*
135400140512     C* LETFIL - LETTURA PROSSIMO RECORD
135500140512     C*------------------------------------------------------------------------*
135600140512     C     LETOrf        BEGSR
135700140512     C*
135800140512     C* LEGGO FINO A:
135900140512     C*     - FINE FILE O SUPERATO LIMITE FINALE
136000140512     C*     - TROVATO RECORD VALIDO
136100140512     C                   MOVEL     'N'           WRECOK
136200140512    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
136300140512     C     WRECOK        ANDEQ     'N'
136400140512     C*
136500140512     C*
136600140512     C     Korfp         READE     fnorf06l                               99
136700140512     c
136800140512    3C                   IF        *IN99  OR orfdae>PARDSA
136900140512     C                   MOVEL     '1'           FINL00
137000140514     c                   else
137100140512     c
137200140512     C                   EXSR      CHKREC_O                                     *CONTR.VALIDITA REC.
137300140512    2C                   END                                                    <-- 1 ---
137400140512     C*
137500140512    1C                   ENDDO                                                  <-- 1 ---
137600140512     C*
137700140512     C                   ENDSR
137800140512     C*------------------------------------------------------------------------*
137900140512     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD per Ritiri
138000140512     C*------------------------------------------------------------------------*
138100140512     C     CHKREC_O      BEGSR
138200140512     C*
138300140512     C                   MOVEL     'S'           WRECOK            1
138400140512     C*
138500140512     c* Escludo i ritiri contestuali alla consegna
138600140512     c                   if        orfnsr>0
138700140512     C                   MOVEL     'N'           WRECOK
138800140512     c                   endif
138900140512     c* Escludo gli ORM pronta merce: stabiliti col cliente fuori dagli orari std
139000140514     c     korm          chain     fnorm01l
139100140514     c                   if        %found(fnorm01l) and ormorr>0
139200140514     C****               MOVEL     'N'           WRECOK
139300140512     c                   endif
139400140512     C*
139500140512     C                   ENDSR
139600140512     C*------------------------------------------------------------------------*
139700140512     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO  per i ritiri
139800140512     C*------------------------------------------------------------------------*
139900140512     C     CRErecR       BEGSR
140000140515
140100140515     c                   movel     orfore        wora
140200140512     c                   clear                   kloc
140300140512     c
140400140512     c* Richiamo pgm reperimento orari servizi
140500140512     c                   clear                   trulorsds
140600140512     c                   clear                   trulor2ds
140700140516     c                   clear                   trulor3ds
140800140512     c                   eval      ioredta=ormdar
140900140512     c                   eval      iorefil=orfpog
141000140512     c                   eval      iorenar=ormnar
141100140512     c                   eval      iorecap=ormcar
141200140516     c                   eval      ioreloc=ormlor
141300140512     c                   eval      ioretser=partser
141400140516     c*****              eval      IOREgf2 = O95gf2
141500140514     c                   eval      ioretsp ='C'
141600140512     c                   call      'TRULORSR'
141700140512     c                   parm                    kpjba
141800140512     c                   parm                    trulorsds
141900140512     c                   parm                    trulor2ds
142000140516     c                   parm                    trulor3ds
142100140516     c                   eval      wnorm=OOR3NORM
142200140516     c
142300140516     c                   if        oor3norm='S'
142400140516     c                   eval      kloc =OOR3LOC_N
142500140516     c                   endif
142600140512     c*
142700140512     c                   clear                   kserv
142800140514     c* se l'orario di ritiro  non rientra negli orari standard verifico se orario
142900140514     c* concordato col cliente ed in questo caso scarto il record
143000140515     c                   if        (wora  <OOR2STIS  and
143100140514     c                              ormcra>0 and
143200140514     c                              ormorr<oor2stis)     or
143300140515     c                             (wora  >OOR2STFS  and
143400140514     c                              ormcra>0 and
143500140514     c                              ormorr>oor2stfs)
143600140514     c                   leavesr
143700140514     c                   endif
143800140512
143900140512     c     kccosR        chain     wfccos1l
144000140512     c                   if        not %found(wfccos1l)
144100140512     c                   clear                   wfccos00
144200140512     c                   eval      wcosdcmd=pardsd
144300140512     c                   eval      wcosdcma=pardsa
144400140512     c                   eval      wcosver =p96ver
144500140512     c                   eval      wcoslna =kfil
144600140512     c                   eval      wcosnaz =ormnar
144700140512     c                   eval      wcoscap =ormcar
144800140513     c                   eval      wcosloc =kloc
144900140512     c                   eval      wcostser=partser
145000140512     c                   eval      wcosorif=OOR2ORIF
145100140512     c                   eval      wcosserv=kserv
145200140512     c* imposta campi cap/località obsoleto
145300140512     c                   exsr      sr_impobs
145400140512     c* Imposto gli orari nel file
145500140512     c                   eval      WCOSOSTIS=OOR2STIS
145600140512     c                   eval      WCOSOSTfS=OOR2STfS
145700140512     c
145800140512     c* imposto orari dettaglio se ci sono e richiesto
145900140512     c                   if        pardet='S'
146000140512     c                   exsr      sr_ImpOrDet
146100140512     c                   endif
146200140512     c                   endif
146300140512     c
146400140512     c                   add       1             wcosts
146500140512     c* Se loc non normalizzata
146600140516     c                   if        wnorm<>'S'
146700140512     c                   add       1             wcostsNN
146800140512     c                   endif
146900140512     c
147000140512     c*** serve in questo caso? E' uguale a wcosts....
147100140512     c                   add       1             wcostse
147200140512     c
147300140512     c* ritiro negli orari std
147400140515     c                   if        wora  >=OOR2STIS  and wora  <=OOR2STFS
147500140512     c                   add       1             wcostsos
147600140512     c                   endif
147700140512     c* imposto campi dettaglio
147800140513     c                   exsr      ImpoDETuni
147900140512     c
148000140512     c                   if        not %found(wfccos1l)
148100140512     c                   write     wfccos00
148200140512     c                   else
148300140512     c                   update    wfccos00
148400140512     c                   endif
148500140512     C*
148600140512     C                   ENDSR
148700140513     C*------------------------------------------------------------------------*
148800140513     C* Imposto i campi di dettaglio in base ai range passati
148900140513     C*------------------------------------------------------------------------*
149000140513     C     ImpoDETUni    BEGSR
149100140513     c*
149200140513     c                   if        wora  <or_dal(1)
149300140513     c                   add       1             wcosmint
149400140513     c                   leavesr
149500140513     c                   endif
149600140513     c                   if        wora  > paroltre
149700140513     c                   add       1             wcosmaxt
149800140513     c                   leavesr
149900140513     c                   endif
150000140513
150100140513     c                   z-add     1             yy
150200140513     c                   dow       or_dal(yy)>0
150300140513     c                   if        wora  >=or_dal(yy) and
150400140513     c
150500140513     c                             (or_dal(yy+1)>0 and
150600140513     c                              wora  < or_dal(Yy+1)  or
150700140513     c                             (or_dal(yy+1)=0 and
150800140513     c                             wora  <=paroltre))
150900140513     c                   select
151000140513     c                   when      yy=1
151100140513     c                   add       1             wcosor1t
151200140513     c                   when      yy=2
151300140513     c                   add       1             wcosor2t
151400140513     c                   when      yy=3
151500140513     c                   add       1             wcosor3t
151600140513     c                   when      yy=4
151700140513     c                   add       1             wcosor4t
151800140513     c                   when      yy=5
151900140513     c                   add       1             wcosor5t
152000140513     c                   when      yy=6
152100140513     c                   add       1             wcosor6t
152200140513     c                   when      yy=7
152300140513     c                   add       1             wcosor7t
152400140513     c                   when      yy=8
152500140513     c                   add       1             wcosor8t
152600140513     c                   when      yy=9
152700140513     c                   add       1             wcosor9t
152800140513     c                   endsl
152900140513     c
153000140513     c                   leavesr
153100140513     c                   endif
153200140513     c                   add       1             yy
153300140513     c
153400140513     c                   enddo
153500140513     c
153600140513     c                   ENDSR
153700140512     C*------------------------------------------------------------------------*
153800140512     C* Imposta campi "obsloleto"
153900140512     C*------------------------------------------------------------------------*
154000140512     C     sr_impobs     BEGSR
154100140512     c* obsoleto
154200140512     c     kcpc          chain     azcpc01l
154300140512     c                   if        %found(azcpc01l)
154400140512     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
154500140512     c                   endif
154600140512     c                   if        wcosloc<>*blanks
154700140512     c     kcpl          chain     azcpl01l
154800140512     c                   if        %found(azcpl01l)
154900140512     c                   eval      wcosloco=%subst(cplfi1:1:1)
155000140512     c                   endif
155100140512     c                   endif
155200140512     c                   endsr
155300140512     C*------------------------------------------------------------------------*
155400140512     C* Imposta campi orari dettaglio
155500140512     C*------------------------------------------------------------------------*
155600140512     C     sr_ImpOrDet   BEGSR
155700140512     c                   eval      wcosor1=or_dal(1)
155800140512     c                   eval      wcosor2=or_dal(2)
155900140512     c                   eval      wcosor3=or_dal(3)
156000140512     c                   eval      wcosor4=or_dal(4)
156100140512     c                   eval      wcosor5=or_dal(5)
156200140512     c                   eval      wcosor6=or_dal(6)
156300140512     c                   eval      wcosor7=or_dal(7)
156400140512     c                   eval      wcosor8=or_dal(8)
156500140512     c                   eval      wcosor9=or_dal(9)
156600140512     c                   eval      wcosorOl=paroltre
156700140512     c                   endsr
156800000000     C*------------------------------------------------------------------------*
156900000000     C* *INZSR - ROUTINE INIZIALE
157000000000     C*------------------------------------------------------------------------*
157100000000     C     *INZSR        BEGSR
157200000000     C*--------------------
157300000000     C* RICEVIMENTO PARAMETRI
157400000000     C*--------------------
157500140408     c
157600140408     c     *dtaara       define    §azute        azuteds
157700140408     c     *dtaara       define    §datiute      ddatiute
157800140408     c                   in(E)     *dtaara
157900140408     c                   If        %error  or RSUT = *blanks
158000140408     c                   CLEAR                   tibs34ds
158100140408     c                   CALL      'TIBS34R'
158200140408     c                   PARM                    tibs34ds
158300140408     c                   in        *dtaara
158400140408     c                   EndIf
158500140408     c
158600140408     c                   eval      p1drsut=rsut
158700140408     C*
158800000000     C*--------------------
158900000000     C* CAMPI RIFERITI AL DATA BASE
159000000000     C*--------------------
159100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
159200000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
159300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
159400051109     C     *LIKE         DEFINE    TBLkey        Kkey
159500000000     C*--------------------
159600000000     C* CHIAVI DI LETTURA
159700000000     C*--------------------
159800140404     C* CHIAVE LETTURA TITAS26C - COMPLETA
159900140404     C     Ktas          KLIST
160000140404     C                   KFLD                    Kdcm
160100140404     C                   KFLD                    Kfil
160200140512     C* CHIAVE LETTURA FNORF06L - COMPLETA
160300140512     C     Korf          KLIST
160400140512     C                   KFLD                    Kfil
160500140512     C                   KFLD                    Kfar
160600140512     C                   KFLD                    Kdae
160700140514     C*
160800140514     C     Korfp         KLIST
160900140514     C                   KFLD                    Kfil
161000140514     C                   KFLD                    Kfar
161100140514     C     Korm          KLIST
161200140514     C                   KFLD                    orfpoe
161300140514     C                   KFLD                    orfnsr
161400140514     C                   KFLD                    orfnor
161500140514     C                   KFLD                    orfnrv
161600000000     C*
161700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
161800000000     C     KEYTAB        KLIST
161900000000     C                   KFLD                    KTBUTE
162000000000     C                   KFLD                    KTBCOD
162100051109     C     KTAB          KLIST
162200051109     C                   KFLD                    ktbute
162300051109     C                   KFLD                    ktbcod
162400051109     C                   KFLD                    KKEY
162500000000     C*
162600060524     C     Kar5          KLIST
162700060524     C                   KFLD                    tasAAS
162800060524     C                   KFLD                    taslnp
162900060524     C                   KFLD                    tasnrs
163000060524     C                   KFLD                    tasnsp
163100060524     c                   kfld                    kar5trd
163200140404     C*
163300140404     C     Kdst          KLIST
163400140404     C                   KFLD                    Knpg
163500140404     C                   KFLD                    knFV
163600140404     C                   KFLD                    Kfgs
163700140404     c
163800140415     C     KCCOS         KLIST
163900140415     C                   KFLD                    partser
164000140423     C                   KFLD                    d55tfa
164100140404     C                   KFLD                    tasnzd
164200140404     C                   KFLD                    tascad
164300140404     C                   KFLD                    Kloc
164400140415     C                   KFLD                    kserv
164500140514     C     KCCOSr        KLIST
164600140514     C                   KFLD                    partser
164700140514     C                   KFLD                    kfil
164800140514     C                   KFLD                    ormnar
164900140514     C                   KFLD                    ormcar
165000140514     C                   KFLD                    Kloc
165100140514     C                   KFLD                    kserv
165200140424     C     Kcpc          KLIST
165300140424     C                   KFLD                    p96ver
165400140424     C                   KFLD                    wcosnaz
165500140424     C                   KFLD                    wcoscap
165600140424     C     Kcpl          KLIST
165700140424     C                   KFLD                    p96ver
165800140424     C                   KFLD                    wcosnaz
165900140424     C                   KFLD                    wcoscap
166000140424     C                   KFLD                    wcosloc
166100000000     C*--------------------
166200000000     C                   Z-ADD     1             CODUT             1 0
166300000000     C*
166400000000     C***
166500000000     C* CARICA ORGANIGRAMMA
166600000000     C                   EXSR      CARORG
166700000000     C*
166800000000     C                   ENDSR
