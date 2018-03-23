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
004400160408     d Tibs02ds      e ds                  Inz
004500160408     d Tibs34ds      e ds                  Inz
004600140409     D azorgds       E DS                  EXTNAME(azorg00f)
004700000000     D*-------------------
004800000000     D DSTB          E DS
004900051109     D DS05          E DS
005000040220     D*------------------
005100140404     D dar5GEN       E DS
005200140404     D og143         E DS
005300140404     d
005400140404     D TRULORSDS     E DS
005500140404     D TRULOR2DS     E DS
005600140516     D TRULOR3DS     E DS
005700140408     d
005800140408     D* DS PER TRUL06R - CARICAMENTO £X
005900140408     D trul06ds      E DS
006000140408     D  LIN                    1     90  0
006100140408     D                                     DIM(30)
006200140411     D fnlv55ds      E DS
006300000000     D*-------------------
006400000000     D* DS PARAMETRI DI LANCIO PGM
006500000000     D*-------------------
006600000000     D PARAM           DS
006700140404     D  pardsd                 1      8  0
006800140404     D  pardsa                 9     16  0
006900140404     D  partser               17     17
007000140404     D  parfil                19     21  0
007100140411     D  parcar                22     24  0
007200140404     D  parperc               25     27  0
007300140408     D  parnum                28     30  0
007400140408     D  parorari              31     31
007500140408     D  parcor                32     32
007600140411     D  pardet                33     33
007700140415     d* dettaglio orari
007800140415     D  paror1                34     37  0
007900140415     D  paror2                38     41  0
008000140415     D  paror3                42     45  0
008100140415     D  paror4                46     49  0
008200140415     D  paror5                50     53  0
008300140415     D  paror6                54     57  0
008400140415     D  paror7                58     61  0
008500140415     D  paror8                62     65  0
008600140416     D  paror9                66     69  0
008700140415     D  paroltre              70     73  0
008800140408     d
008900140408     D WLBDAT          DS
009000140408     D  G02DAT                 1      8  0
009100140408     D  G02INV                 9     16  0
009200140408     D  G02ERR                17     17
009300140408     D  G02TGI                18     22  0
009400140404     d
009500060524     d
009600140404     d KAr5Trd         s                   Like(Ar5Trd) inz('GEN')
009700150505     d Ktrc            s                   Like(ta4trc) inz('X')
009800140404     D wNetwork        s              3    inz(*blanks)
009900140404     D kfil            s              3  0 inz
010000140404     D P1DTSER         s              8    inz
010100140404     D wperc_os        s              3  0 inz
010200140404     D wperc_om        s              3  0 inz
010300140404     D Y               s              3  0 inz
010400140408     D YY              s              3  0 inz
010500140423     D XX              s              3  0 inz
010600140408     D savlna          s              3  0 inz
010700140404     D kdcm            s              8  0 inz
010800140404     D wnorm           s              1    inz
010900140408     D wstampadet      s              1    inz
011000140404     D knfv            s                   like(dstnfv)
011100140516     D kloc            s                   like(ioreloc)
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
012300140514     d wora            s                   like(tashmc)
012400140512     D kfar            s                   like(orffar) inz(420)
012500140512     D kdae            s                   like(orfdae) inz
012600140404     d
012700140508     d WFDST           s             21    inz('FILTRA201 /FIDST01L')
012800140409     d titas           s             21    inz('GAITRAGRU /TITAS26C')
012900150505     d tita4           s             21    inz('GAITRAGRU /TITA430C')
013000140409     d fiar5           s             21    inz('GAITRAGRU /FIAR531C')
013100000000     C*------------------------------------------------------------------------*
013200000000     C* MAIN LINE
013300000000     C*------------------------------------------------------------------------*
013400140428     C     *ENTRY        PLIST
013500140428     C                   PARM                    KPJBA
013600140428     C                   MOVEL     KPJBU         PARAM
013700140409      /free
013800140409         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013900140409      /end-free
014000140428     C* CARICA TIPI BOLLA
014100140428     C                   EXSR      CARTBL
014200140408     c*
014300140408     c* se impostata l'area leggo le filiali dell'are altrimenti la singola filiale
014400140408     c*  altrimenti tutto
014500140408     c                   select
014600140408     c                   when      parfil>0
014700140514     c* Consegne
014800140514     c                   if        partser='C'
014900140408     c* carico la sua £6
015000140408     C                   CLEAR                   trul06ds
015100140408     C                   MOVE      '£6'          D06COD
015200140408     C                   movel     parfil        D06KEY
015300140408     c                   move      'S'           d06key
015400140409     c                   if        pardsa>0
015500140408     c                   movel     pardsa        d06drf
015600140409     c                   else
015700140409     c                   movel     pardsd        d06drf
015800140409     c                   endif
015900140408     C                   MOVEL     'L'           D06TLA
016000140408     C                   MOVEL     trul06ds      KPJBU
016100140408     C*
016200140408     C                   CALL      'TRUL06R'
016300140408     C                   PARM                    KPJBA
016400140408     C                   MOVEL     KPJBU         trul06ds
016500140408     C                   MOVEA     LIN           L6
016600140423     c                   z-add     1             XX
016700160408     c
016800140423     c                   dow       l6(XX)>0
016900140423     c                   eval      kfil=l6(XX)
017000140408     c                   exsr      Let_TAS
017100140423     c                   add       1             XX
017200140408     c                   enddo
017300140514     c                   else
017400140514     c* Ritiri
017500140514     c                   eval      kfil=parfil
017600140514     c                   exsr      Let_ORF
017700140514     c                   endif
017800140408     c* immesso cod area
017900140408     c                   when      parcar>0
018000140409     C/EXEC SQL
018100140409     C+ Declare A1 Cursor for
018200140409     c+ SELECT * FROM azorg00f
018300140409     c+ WHERE orgcar=  :parcar AND orgfva=' ' and (orgfag='F' or orgfag='A')
018400140409     c+ ORDER BY orgfil
018500140409     C/END-EXEC
018600140409      *          apertura cursore
018700140409     C/EXEC SQL
018800140409     C+ OPEN A1
018900140409     C/END-EXEC
019000140409     C                   DOU       SqlCod <> 0
019100140409     C/EXEC SQL
019200140409     C+ Fetch Next From A1 Into :azorgds
019300140409     C/END-EXEC
019400140409     c                   select
019500140409     C                   WHEN      SqlCod = 100
019600140409     c                   leave
019700140409      *
019800140409     C                   WHEN      SqlCod < 0
019900140409     c                   seton                                        H1
020000140409     c                   leave
020100140409      *
020200140409     C                   OTHER
020300140408     c                   eval      kfil=orgfil
020400140514     c                   if        partser='C'
020500140408     c                   exsr      Let_TAS
020600140514     c                   else
020700140514     c                   exsr      Let_ORF
020800140514     c                   endif
020900140409     C                   ENDSL
021000140409
021100140408     c                   enddo
021200140409     C/EXEC SQL
021300140409     C+ Close A1
021400140409     C/END-EXEC
021500140408     c
021600140408     c                   other
021700140409
021800140408     c     *loval        setll     azorg02l
021900140408     c                   read      azorg02l
022000140408     c                   dow       not %eof(azorg02l)
022100140408     c                   eval      kfil=orgfil
022200140514     c                   if        partser='C'
022300140408     c                   exsr      Let_TAS
022400140514     c                   else
022500140514     c                   exsr      Let_ORF
022600140514     c                   endif
022700140408     c                   read      azorg02l
022800140408     c                   enddo
022900140408     c
023000140408     c                   endsl
023100140404     c*
023200140404     c                   EXSR      Stampa
023300140408     c*
023400140514     c                   if        partser='C'
023500140408     c                   write     STO4FI
023600140514     c                   else
023700140514     c                   write     STO4FI2
023800140514     c                   endif
023900140422     c
024000140422     c                   if        pardet='S'
024100140514     c                   if        partser='C'
024200140422     c                   write     STO4RI_d
024300140422     c                   write     STO4FI_d
024400140514     c                   else
024500140514     c                   write     STO4RI_d3
024600140514     c                   write     STO4FI_d3
024700140514     c                   endif
024800140422     c                   endif
024900000000     C*
025000140423     C                   CLEAR                   fnlv55ds
025100140423     C                   MOVEL     'C'           D55TLA
025200140423     C                   CALL      'FNLV55R'
025300140423     C                   PARM                    fnlv55ds
025400140423     c
025500000000     C                   SETON                                        LR
025600140408     C*------------------------------------------------------------------------*
025700140408     C* Ciclo di lettura su TITAS
025800140408     C*------------------------------------------------------------------------*
025900140408     c     LET_TAS       BEGSR
026000140409     C                   MOVEL     '0'           FINL00            1
026100140423     c* carico fil gestore
026200140423     c                   clear                   fnlv55ds
026300140423     C                   EVAL      D55TPT='6'
026400140423     c                   eval      d55lin=kfil
026500140423     c                   if        pardsa>0
026600140423     c                   eval      d55drf=pardsa
026700140423     c                   else
026800140423     c                   eval      d55drf=pardsd
026900140423     c                   endif
027000140423     c
027100140423     c                   call      'FNLV55R'
027200140423     c                   parm                    fnlv55ds
027300140408     c
027400140408     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
027500140408     C     FINL00        IFEQ      '0'                                          --- 1 -->
027600140408     C*
027700140408     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
027800140408     C                   EXSR      SETFIL
027900140408     C*
028000140408     C* CICLO FINO A FINE FILE
028100140408     C     FINL00        DOWEQ     '0'                                          --- 2 -->
028200140408     C*
028300140408     C* CREA IL RECORD SUI FILE DI LAVORO
028400140408     C                   EXSR      CRErec
028500140408     C*
028600140408     C* LEGGE IL RECORD SUCCESSIVO
028700140408     C                   EXSR      LETFIL
028800140408     C                   ENDDO                                                  <-- 2 ---
028900140408     C*
029000140408     C                   ENDIF                                                  <-- 1 ---
029100140408     c                   ENDSR
029200000000     C*------------------------------------------------------------------------*
029300000000     C* SETFIL - POSIZIONAMENTO SUL FILE
029400000000     C*------------------------------------------------------------------------*
029500000000     C     SETFIL        BEGSR
029600000000     C*
029700000000     C* POSIZIONAMENTO E PRIMA LETTURA
029800000000     C*
029900140404     C                   movel     pardsd        Kdcm
030000991123     C*
030100140404     C     Ktas          SETLL     TITAS26C                           99        *FINE FILE
030200000000     C*
030300000000     C     *IN99         IFEQ      '1'
030400000000     C                   MOVEL     '1'           FINL00
030500000000     C                   ELSE
030600000000     C                   EXSR      LETFIL
030700000000     C                   END
030800000000     C*
030900000000     C                   ENDSR
031000140404     C*------------------------------------------------------------------------*
031100000000     C* LETFIL - LETTURA PROSSIMO RECORD
031200000000     C*------------------------------------------------------------------------*
031300000000     C     LETFIL        BEGSR
031400000000     C*
031500000000     C* LEGGO FINO A:
031600000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
031700000000     C*     - TROVATO RECORD VALIDO
031800000000     C                   MOVEL     'N'           WRECOK
031900140404    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
032000000000     C     WRECOK        ANDEQ     'N'
032100000000     C*
032200140404     C*
032300140404    2c                   if        kfil>0
032400140404     C     KTAS          READE     TITAS26C                               99
032500140404     c                   else
032600140404     C     Kdcm          READE     TITAS26C                               99
032700140404    2c                   endif
032800140404     c
032900140404    2C                   IF        *IN99
033000140404     C     KDCM          SETGT     TITAS26C
033100140404     C                   READ      TITAS26C                               99
033200140404    3C                   IF        *IN99  OR TASDCM>PARDSA
033300140404     C                   MOVEL     '1'           FINL00
033400140404     C                   ELSE
033500140404     C                   EVAL      KDCM=TASDCM
033600140404     C     KTAS          setll     TITAS26C
033700140404    3C                   ENDIF
033800140404
033900140404   x2C                   ELSE
034000140404     c
034100140404     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
034200140404    2C                   END                                                    <-- 1 ---
034300140404     C*
034400140404    1C                   ENDDO                                                  <-- 1 ---
034500000000     C*
034600000000     C                   ENDSR
034700000000     C*------------------------------------------------------------------------*
034800000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
034900000000     C*------------------------------------------------------------------------*
035000000000     C     CHKREC        BEGSR
035100000000     C*
035200000000     C                   MOVEL     'S'           WRECOK            1
035300040305     C*
035400140414     c*
035500051109     C* ESCLUDE LE BOLLE di recupero
035600051109     C                   Z-ADD     1             i
035700000000     C                   SETOFF                                           28
035800051109     C     TASTBL        LOOKUP    TB(i)                                  28
035900051109     C                   IF        *in28 = *on
036000000000     C                   MOVEL     'N'           WRECOK
036100000000     C                   ENDIF
036200140404     c
036300140414     c* Escludo bolle export e le bolle con consegna anomala
036400140414     c                   if        tasnzd<>*blanks or tascca<>' '
036500140408     C                   MOVEL     'N'           WRECOK
036600140408     c                   endif
036700140414     c
036800140411     c* Escludo le bolle in fermo deposito
036900140411     c                   if        tasffd='S'
037000140411     C                   MOVEL     'N'           WRECOK
037100140411     c                   endif
037200160226     c* Escludo i prepagati annullati
037300160226     c                   if        tasctm='4 ' or tasctm='4F'
037400160226     C                   MOVEL     'N'           WRECOK
037500160226     c                   endif
037600011112     C*
037700000000     C                   ENDSR
037800140408     C*------------------------------------------------------------------------*
037900140408     C* controllo se si tratta di bolla da elaborare
038000140408     C*------------------------------------------------------------------------*
038100140408     C     NOELAB        BEGSR
038200140408     C*
038300140408     C                   MOVEL     'S'           Welab             1
038400140408     C*
038500140408     c* Cerco se si tratta di distinta PDA
038600140508     c* solo in AS888
038700140508    1c                   if        %subst(knsif:7:1)='P'
038800140508     c                   movel     taspdc        kfgs              3 0
038900140508     c                   movel     4             knpg              1 0
039000140508     c                   z-add     tasndc        knfv
039100140508     c     kdst          chain     fidst01l
039200140508    2c                   if        %found(fidst01l)
039300140508     c                             and (dstpda='C' or dstpda='E')
039400140508   x2c                   else
039500140508     C                   MOVEL     'N'           Welab
039600140508    2c                   endif
039700140508     c
039800140508   x1c                   else
039900140508   c
040000140508     c* Solo dall'aprile 2014 abbiamo le bolle con il flag impostato
040100140508     c                   if        tasaas<2014
040200140508     C                   MOVEL     'N'           Welab
040300140508     c                   endif
040400140508     c
040500140508     c                   if        tasaas=2014 and tasmgs<0401
040600140508     C                   MOVEL     'N'           Welab
040700140508     c                   endif
040800140408     c* chaino record GEN
040900140408     c     kar5          chain     fiar531c
041000140408     c                   if        %found(fiar531c)
041100140408     c                   eval      dar5gen=ar5uni
041200140408     c                   if        §AR5PDACO<>'  '
041300140408     C                   MOVEL     'N'           Welab
041400140408     c                   endif
041500140408     c                   endif
041600140508     c
041700140508    1C                   ENDIF
041800140408     c
041900140411     c* Non elaboro se impostata ora consegna richiesta
042000140411     c                   if        tashcr>0
042100140408     C                   MOVEL     'N'           Welab
042200140408     c                   endif
042300140408     c
042400140408     C                   ENDSR
042500000000     C*------------------------------------------------------------------------*
042600140408     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO
042700000000     C*------------------------------------------------------------------------*
042800140408     C     CRErec        BEGSR
042900140516     c
043000140404     c                   clear                   kloc
043100140404     c
043200140404     c* Richiamo pgm reperimento orari servizi
043300140404     c                   clear                   trulorsds
043400140404     c                   clear                   trulor2ds
043500140516     c                   clear                   trulor3ds
043600140404     c                   eval      ioredta=pardsd
043700140408     c                   eval      iorefil=taslna
043800140404     c                   eval      iorenar=tasnzd
043900140404     c                   eval      iorecap=tascad
044000140516     c                   eval      ioreloc=taslod
044100150505     c* Verifico se presente località normalizzata e la passo
044200150505     c     kta4          chain     tita430c
044300150505     c                   if        %found(tita430c)
044400150505     c                   eval      OOR3LOC_N=ta4not
044500150505     c                   eval      OOR3norm ='S'
044600150505     c                   else
044700150505     c                   eval      oor3loc_N=taslod
044800150505     c                   endif
044900140414     c                   eval      ioreddc=tasddc
045000140414     c                   eval      iorendc=tasndc
045100140414     c                   eval      ioretfp=tastfp
045200140414     c                   eval      ioretfa=tastfa
045300140414     c                   eval      ioredti=tasdti
045400140428     c                   eval      iorehti=9999
045500140404     c                   eval      ioretser=partser
045600140515     c* si forza sempre "C" perché per "H" e "E"  l'orario viene accorciato
045700140515     c* automaticamente e non è l'obiettivo della statistica
045800140515     c                   eval      ioretsp ='C'
045900140404     c                   call      'TRULORSR'
046000140404     c                   parm                    kpjba
046100140404     c                   parm                    trulorsds
046200140404     c                   parm                    trulor2ds
046300140516     c                   parm                    trulor3ds
046400140516     c                   eval      wnorm=OOR3NORM
046500140516     c
046600140516     c                   if        oor3norm='S'
046700140516     c                   eval      kloc =OOR3LOC_N
046800150717     c                   else
046900150717     c* verifico da CPL se era già normalizzata
047000150717     c     kcpl2         chain     azcpl01l
047100150717     c                   if        %found(azcpl01l)
047200150717     c                   eval      kloc =taslod
047300150717     c                   eval      OOR3norm ='S'
047400140516     c                   endif
047500150717     c                   endif
047600140516     c*
047700140404     c*
047800140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
047900140415     c                   if        oor2fcsgl='P'
048000140415     c                   eval      kserv='P '
048100140415     c                   else
048200140415     c                   clear                   kserv
048300140415     c                   endif
048400140415
048500140415     c     kccos         chain     wfccos1l
048600140404     c                   if        not %found(wfccos1l)
048700140404     c                   clear                   wfccos00
048800140404     c                   eval      wcosdcmd=pardsd
048900140404     c                   eval      wcosdcma=pardsa
049000140404     c                   eval      wcosver =p96ver
049100140411     c                   eval      wcoslna =d55tfa
049200140404     c                   eval      wcosnaz =tasnzd
049300140404     c                   eval      wcoscap =tascad
049400140404     c                   eval      wcosloc =kloc
049500140404     c                   eval      wcostser=partser
049600140404     c                   eval      wcosorif=OOR2ORIF
049700140415     c                   eval      wcosserv=kserv
049800140424     c
049900140424     c* obsoleto
050000140514     c                   exsr      sr_impobs
050100140424     c* Imposto gli orari nel file
050200140424     c                   eval      WCOSOSTIS=OOR2STIS
050300140424     c                   eval      WCOSOSTfS=OOR2STfS
050400140424     c                   eval      WCOSOmiis=OOR2miis
050500140424     c                   eval      WCOSOmxfs=OOR2mxfs
050600140424     c
050700140415     c* imposto orari dettaglio se ci sono e richiestoc
050800140415     c                   if        pardet='S'
050900140514     c                   exsr      sr_ImpOrDet
051000140404     c                   endif
051100140415     c                   endif
051200140404     c
051300140404     c                   add       1             wcosts
051400140404     c* Se loc non normalizzata
051500140516     c                   if        wnorm<>'S'
051600140404     c                   add       1             wcostsNN
051700140404     c                   endif
051800140408     c
051900140408     c* Controllo se spedizione da elaborare
052000140408     c                   exsr      Noelab
052100140408     c
052200140404     c* elaborate
052300140408     c                   if        welab='S'
052400140404     c                   add       1             wcostse
052500140404     c
052600140404     c* consegna negli orari std
052700140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
052800140404     c                   add       1             wcostsos
052900140404     c                   endif
053000140404     c* consegna negli orari min/max
053100140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
053200140404     c                   add       1             wcostsom
053300140404     c                   endif
053400140415     c* imposto campi dettaglio
053500140514     c                   z-add     tashmc        wora
053600140514     c                   exsr      ImpoDETUni
053700140408     c                   endif
053800140404     c
053900140404     c                   if        not %found(wfccos1l)
054000140404     c                   write     wfccos00
054100140404     c                   else
054200140404     c                   update    wfccos00
054300140404     c                   endif
054400000000     C*
054500000000     C                   ENDSR
054600140404     C*------------------------------------------------------------------------*
054700140404     C* Stampa - statistica
054800140404     C*------------------------------------------------------------------------*
054900140404     c     STAMPA        BEGSR
055000140404     c* Tpo servizio
055100140404     c                   if        partser='C'
055200140404     c                   eval      p1dtser='CONSEGNE'
055300140404     C                   ELSE
055400140408     c                   eval      p1dtser=' RITIRI '
055500140404     C                   ENDIF
055600140408     c* giro le date
055700140409     C                   MOVE      pardsd        G02inv
055800140408     C                   MOVEL     '3'           G02ERR
055900140408     C                   CALL      'XSRDA8'
056000140408     C                   PARM                    WLBDAT
056100140408     c                   z-add     g02dat        p1ddsd
056200140408     c*
056300140408     c                   if        pardsa>0
056400140409     C                   MOVE      pardsa        G02inv
056500140408     C                   MOVEL     '3'           G02ERR
056600140408     C                   CALL      'XSRDA8'
056700140408     C                   PARM                    WLBDAT
056800140408     c                   z-add     g02dat        p1ddsa
056900140408     c                   endif
057000140408     c* Orari per calcolo %
057100140408     c                   if        parorari='S'
057200140408     c                   eval      p1dorari='STANDARD'
057300140424     c                   eval      p1dtor_d='Standard'
057400140408     c                   else
057500140408     c                   eval      p1dorari='MINIMO/MASSIMO'
057600140424     c                   eval      p1dtor_d=' Min/Max'
057700140408     c                   endif
057800140408     c
057900140514     c                   if        partser='C'
058000140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
058100140408     c                   if        parcor='C'
058200140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
058300140411     c                   eval      p1dbolstd='   Nei limiti STD   '
058400140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
058500140408     C                   ELSE
058600140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
058700140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
058800140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
058900140408     C                   ENDIF
059000140514     c                   else
059100140514     c                   eval      p1dbolstd='  Fuori limiti STD  '
059200140514     c                   endif
059300140423     c
059400140514     c                   if        partser='C'
059500140423     c                   write     STO4TE
059600140514     c                   else
059700140514     c                   write     STO4TE2
059800140514     c                   endif
059900140423
060000140423     c* Orari nel dettaglio
060100140423     c                   if        pardet='S'
060200140423     c                   exsr      TestaDET
060300140514     c                   if        partser='C'
060400140423     c                   write     STO4TE_d
060500140514     c                   else
060600140514     c                   write     STO4TE_d3
060700140514     c                   endif
060800140423     c                   endif
060900140404     c
061000140404     C
061100140424     c*
061200140514     c     partser       setll     wfccos1l
061300140514     c     partser       reade     wfccos1l
061400140404     c                   dow       not %eof(wfccos1l)
061500140404     c
061600140408     c* a cambio filiale stampo a pagina nuova
061700140408     c                   if        wcoslna<>savlna
061800140408
061900140408     c                   if        savlna>0
062000140423     c                   exsr      Totfil
062100140423     c
062200140514     c                   if        partser='C'
062300140408     c                   write     STO4TE
062400140514     c                   else
062500140514     c                   write     STO4TE2
062600140514     c                   endif
062700140422     c
062800140422     c                   if        pardet='S'
062900140514     c                   if        partser='C'
063000140422     c                   write     STO4RI_d
063100140422     c                   write     STO4TE_d
063200140514     c                   else
063300140514     c                   write     STO4RI_d3
063400140514     c                   write     STO4TE_d3
063500140514     c                   endif
063600140408     c                   endif
063700140423     c
063800140423     c                   clear                   tot_ts
063900140423     c                   clear                   tot_tse
064000140423     c                   clear                   tot_tsos
064100140423     c                   clear                   tot_tsom
064200140422     c                   endif
064300140408
064400140408     c                   z-add     1             i
064500140408     C     wcoslna       lookup    fil(i)                                 55
064600140404     C                   if        %found
064700140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
064800140408     c                   eval      p1dare=dare(i)
064900140404     C                   else
065000140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
065100140408     c                   eval      p1dare=*all'?'
065200140404     C                   endif
065300140408
065400140408     c                   eval      savlna=wcoslna
065500160408     c                   clear                   wdoppiag          1
065600160408     c* verifico se invece che la soglia POM ha la seconda FASCIA
065700160408     c                   clear                   tibs02ds
065800160408     c                   eval      t02tla='L'
065900160408     c                   eval      t02mod='C'
066000160408     c                   eval      t02cod='FLO'
066100160408     c                   movel(p)  wcoslna       t02ke1
066200160408     c                   call      'TIBS02R'
066300160408     c                   parm                    kpjba
066400160408     c                   parm                    tibs02ds
066500160408     c                   if        t02err=' '   and t02atb=' '
066600160408     c                   eval      wdoppiag='S'
066700160408     c                   endif
066800140408
066900140514     c                   if        partser='C'
067000140408     c                   write     STO4TEF
067100140414     c                   write     STO4RI
067200140514     c                   else
067300140514     c                   write     STO4TEF2
067400140514     c                   write     STO4RI2
067500140514     c                   endif
067600140422     c                   if        pardet='S'
067700140514     c                   if        partser='C'
067800140422     c                   write     STO4TEF_d
067900140422     c                   write     STO4RI_d
068000140514     c                   else
068100140514     c                   write     STO4TEF_d3
068200140514     c                   write     STO4RI_d3
068300140514     c                   endif
068400140408     c                   endif
068500140422     c                   endif
068600140408
068700140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
068800140411     c                   if        wcostse>=parnum and wcostse>0
068900140408     c* voglio la % di correttezza
069000140408     c                   if        parcor='C'
069100140404     c* Calcolo % STD
069200140619     c                   if        wcostsos>0
069300140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
069400140408     c                   eval      p1dstsos=WCOSTSOs
069500140619     c                   else
069600140619     c                   clear                   p1dperc_os
069700140619     c                   clear                   p1dstsos
069800140619     c                   endif
069900140404     c* Calcolo % MIN/MAX
070000140619     c                   if        wcostsom>0
070100140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
070200140408     c                   eval      p1dstsom=WCOSTSOm
070300140619     c                   else
070400140619     c                   clear                   p1dperc_om
070500140619     c                   clear                   p1dstsom
070600140619     c                   endif
070700140408     c
070800140408     c                   else
070900140408     c* voglio la % delle fuori orario
071000140408     c                   eval      wfuori_os=wcostse-wcostsos
071100140619     c                   if        wfuori_OS>0
071200140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
071300140408     c                   eval      p1dstsos=Wfuori_os
071400140619     c                   else
071500140619     c                   clear                   p1dperc_os
071600140619     c                   clear                   p1dstsos
071700140619     c                   endif
071800140408
071900140408     c                   eval      wfuori_om=wcostse-wcostsom
072000140619     c                   if        wfuori_OM>0
072100140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
072200140408     c                   eval      p1dstsom=Wfuori_om
072300140619     c                   else
072400140619     c                   clear                   p1dperc_om
072500140619     c                   clear                   p1dstsom
072600140619     c                   endif
072700140408     c                   endif
072800140408     c
072900140407     c* stampo solo al di sotto della % richiesta
073000140408     c                   eval      wstampadet='S'
073100140408
073200140408     c                   select
073300140408     c                   when      parperc=0
073400140408
073500140408     c                   when      parorari='S'  and parcor='C' and
073600140408     c                             p1dperc_os>=parperc
073700140408     c                   eval      wstampadet='N'
073800140408     c
073900140408     c                   when      parorari='S'  and parcor='F' and
074000140408     c                             p1dperc_os< parperc
074100140408     c                   eval      wstampadet='N'
074200140408
074300140408     c                   when      parorari='M'  and parcor='C' and
074400140408     c                             p1dperc_om>=parperc
074500140408     c                   eval      wstampadet='N'
074600140408     c
074700140408     c                   when      parorari='M'  and parcor='F' and
074800140408     c                             p1dperc_om< parperc
074900140408     c                   eval      wstampadet='N'
075000140408     c                   endsl
075100140404     c*
075200140408     c                   if        wstampadet='S'
075300140424     c* POM
075400140424     c                   if        wcosserv='P '
075500160408     c                   if        wdoppiag='S'
075600160408     c                   eval      p1dpom='2Fa'
075700160408     c                   else
075800140424     c                   eval      p1dpom='Pom'
075900160408     c                   endif
076000160408     c
076100140424     c                   else
076200140424     c                   eval      p1dpom='   '
076300140424     c                   endif
076400140411     c*
076500140411     c* Se la località è vuota imposto '??'
076600140411     c                   if        wcosloc=*blanks
076700140411     c                   eval      wcosloc='??'
076800140411     c                   endif
076900140408     c
077000140408     c                   if        *in06
077100140408     c                   write     sto4te
077200140408     c                   write     STO4TEF
077300140414     c                   write     STO4RI
077400140408     c                   setoff                                       06
077500140404     c                   endif
077600140514
077700140514     c                   if        *in08
077800140514     c                   write     sto4te2
077900140514     c                   write     STO4TEF2
078000140514     c                   write     STO4RI2
078100140514     c                   setoff                                       08
078200140514     c                   endif
078300140408     c
078400140424     c                   eval      p1dstis=wcosoSTiS
078500140424     c                   eval      p1dstfs=wcosoSTFS
078600140424     c                   eval      p1dmiis=wcosomiis
078700140424     c                   eval      p1dmxfs=wcosomxfs
078800140404     c
078900140514     c                   if        partser='C'
079000140408     c                   write     STO4det
079100140514     c                   else
079200140514     c                   write     STO4det2
079300140514     c                   endif
079400140422     c
079500140422     c* se devo stampare anche il dettaglio
079600140422     c                   if        pardet='S'
079700140422     c                   exsr      Stadet
079800140422     c                   endif
079900140408     c
080000140407     c                   endif
080100140408     c                   endif
080200140424     c* totali di filiale
080300140424     c                   add       WCOSTS        tot_ts
080400140424     c                   add       WCOSTSE       tot_tse
080500140424     c                   add       WCOSTSos      tot_tsos
080600140424     c                   add       WCOSTSom      tot_tsom
080700140407     c
080800140514     c     partser       reade     wfccos1l
080900140404     c
081000140404     c                   enddo
081100140423     c
081200140423     c* totali finali
081300140423     c                   if        savlna>0
081400140423     c                   exsr      totfil
081500140423     c                   endif
081600140404     c
081700140404     c                   ENDSR
081800140423     C*------------------------------------------------------------------------*
081900140423     c* Imposta gli orari in testata dettaglioù
082000140423     C*------------------------------------------------------------------------*
082100140423     c     TestaDet      BEGSR
082200140423     c                   eval      p1dsot = paror1
082300140423     c                   eval      p1doltre= paroltre
082400140423     c                   eval      p1dda1  = or_dal(1)
082500140423
082600140423     c                   if        or_dal(2)>0
082700140423     c                   eval      p1dda2  = or_dal(2)
082800140423     c                   movel     or_dal(2)     w0060
082900140423     c                   move      w0060         timeiso
083000140423     c                   subdur    1:*mn         timeiso
083100140423     c                   eval      w0060     =%dec(timeiso)
083200140423     c                   movel     w0060         p1dal1
083300140423     c                   else
083400140423     c                   eval      p1dal1=paroltre
083500140423     c                   leavesr
083600140423     c                   endif
083700140423     c
083800140423     c                   if        or_dal(3)>0
083900140423     c                   eval      p1dda3  = or_dal(3)
084000140423     c                   movel     or_dal(3)     w0060
084100140423     c                   move      w0060         timeiso
084200140423     c                   subdur    1:*mn         timeiso
084300140423     c                   eval      w0060     =%dec(timeiso)
084400140423     c                   movel     w0060         p1dal2
084500140423     c                   else
084600140423     c                   eval      p1dal2=paroltre
084700140423     c                   leavesr
084800140423     c                   endif
084900140423
085000140423     c                   if        or_dal(4)>0
085100140423     c                   eval      p1dda4  = or_dal(4)
085200140423     c                   movel     or_dal(4)     w0060
085300140423     c                   move      w0060         timeiso
085400140423     c                   subdur    1:*mn         timeiso
085500140423     c                   eval      w0060     =%dec(timeiso)
085600140423     c                   movel     w0060         p1dal3
085700140423     c                   else
085800140423     c                   eval      p1dal3=paroltre
085900140423     c                   leavesr
086000140423     c                   endif
086100140423
086200140423     c                   if        or_dal(5)>0
086300140423     c                   eval      p1dda5  = or_dal(5)
086400140423     c                   movel     or_dal(5)     w0060
086500140423     c                   move      w0060         timeiso
086600140423     c                   subdur    1:*mn         timeiso
086700140423     c                   eval      w0060     =%dec(timeiso)
086800140423     c                   movel     w0060         p1dal4
086900140423     c                   else
087000140423     c                   eval      p1dal4=paroltre
087100140423     c                   leavesr
087200140423     c                   endif
087300140423
087400140423     c                   if        or_dal(6)>0
087500140423     c                   eval      p1dda6  = or_dal(6)
087600140423     c                   movel     or_dal(6)     w0060
087700140423     c                   move      w0060         timeiso
087800140423     c                   subdur    1:*mn         timeiso
087900140423     c                   eval      w0060     =%dec(timeiso)
088000140423     c                   movel     w0060         p1dal5
088100140423     c                   else
088200140423     c                   eval      p1dal5=paroltre
088300140423     c                   leavesr
088400140423     c                   endif
088500140423
088600140423     c                   if        or_dal(7)>0
088700140423     c                   eval      p1dda7  = or_dal(7)
088800140423     c                   movel     or_dal(7)     w0060
088900140423     c                   move      w0060         timeiso
089000140423     c                   subdur    1:*mn         timeiso
089100140423     c                   eval      w0060     =%dec(timeiso)
089200140423     c                   movel     w0060         p1dal6
089300140423     c                   else
089400140423     c                   eval      p1dal6=paroltre
089500140423     c                   leavesr
089600140423     c                   endif
089700140423
089800140423     c                   if        or_dal(8)>0
089900140423     c                   eval      p1dda8  = or_dal(8)
090000140423     c                   movel     or_dal(8)     w0060
090100140423     c                   move      w0060         timeiso
090200140423     c                   subdur    1:*mn         timeiso
090300140423     c                   eval      w0060     =%dec(timeiso)
090400140423     c                   movel     w0060         p1dal7
090500140423     c                   else
090600140423     c                   eval      p1dal7=paroltre
090700140423     c                   leavesr
090800140423     c                   endif
090900140423     c                   if        or_dal(9)>0
091000140423     c                   eval      p1dda9  = or_dal(9)
091100140423     c                   movel     or_dal(9)     w0060
091200140423     c                   move      w0060         timeiso
091300140423     c                   subdur    1:*mn         timeiso
091400140423     c                   eval      w0060     =%dec(timeiso)
091500140423     c                   movel     w0060         p1dal8
091600140423     c
091700140423     c                   eval      p1dal9=paroltre
091800140423     c                   else
091900140423     c                   eval      p1dal8=paroltre
092000140423     c                   leavesr
092100140423     c                   endif
092200140423     c
092300140423     c                   ENDSR
092400140422     C*------------------------------------------------------------------------*
092500140422     C* Stampa dettaglio
092600140422     C*------------------------------------------------------------------------*
092700140422     C     StaDET        BEGSR
092800140422     c                   if        *in07
092900140422     c                   write     sto4te_d
093000140422     c                   write     STO4TEF_d
093100140422     c                   write     STO4RI_d
093200140422     c                   setoff                                       07
093300140422     c                   endif
093400140514
093500140514     c                   if        *in09
093600140514     c                   write     sto4te_d3
093700140514     c                   write     STO4TEF_d3
093800140514     c                   write     STO4RI_d3
093900140514     c                   setoff                                       09
094000140514     c                   endif
094100140424     c* Imposto gli orari
094200140424     c                   if        parorari='S'
094300140424     c                   eval      p1dor_dal=WCOSOSTIS
094400140424     c                   eval      p1dor_al =WCOSOSTfS
094500140424     c                   else
094600140424     c                   eval      p1dor_dal=WCOSOmiis
094700140424     c                   eval      p1dor_al =WCOSOmxfS
094800140424     c                   endif
094900140424     c                   setoff                                       111213
095000140424     c                   setoff                                       141516
095100140424     c                   setoff                                       171819
095200140505     c                   setoff                                       2021
095300140422     c* calcolo le %
095400140422     c* SOTTO
095500140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
095600140422     C                   IF        WPERC>0
095700140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
095800140505     c                   seton                                        20
095900140422     c                   else
096000140424     c                   eval      p1cp_sot='    '
096100140422     c                   endif
096200140422     c* OLTRE
096300140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
096400140422     C                   IF        WPERC>0
096500140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
096600140505     c                   seton                                        21
096700140422     c                   else
096800140424     c                   eval      p1cp_olt='    '
096900140422     c                   endif
097000140422     c* % 1
097100140505    1c                   if        wcosor1  >0
097200140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
097300140422     c
097400140505    2C                   IF        WPERC>0
097500140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
097600140505     c
097700140505    2c                   select
097800140505     c                   when      or_dal(1)<p1dor_dal
097900140505     c                   seton                                        11
098000140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
098100140505     c                   seton                                        11
098200140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
098300140505     c                   seton                                        11
098400140505    2c                   endsl
098500140505
098600140422     c                   else
098700140424     c                   eval      p1cp_o1 ='    '
098800140505    2c                   endif
098900140424     c
099000140505    1c                   endif
099100140422     c
099200140422     c* % 2
099300140422     c                   if        wcosor2  >0
099400140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
099500140422     c
099600140422     C                   IF        WPERC>0
099700140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
099800140505
099900140505     c                   select
100000140505     c                   when      or_dal(2)<p1dor_dal
100100140505     c                   seton                                        12
100200140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
100300140505     c                   seton                                        12
100400140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
100500140505     c                   seton                                        12
100600140505     c                   endsl
100700140505
100800140422     c                   else
100900140424     c                   eval      p1cp_o2 ='    '
101000140422     c                   endif
101100140422     c                   endif
101200140423     c* % 3
101300140423     c                   if        wcosor3  >0
101400140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
101500140423     c
101600140423     C                   IF        WPERC>0
101700140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
101800140505
101900140505     c                   select
102000140505     c                   when      or_dal(3)<p1dor_dal
102100140505     c                   seton                                        13
102200140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
102300140505     c                   seton                                        13
102400140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
102500140505     c                   seton                                        13
102600140505     c                   endsl
102700140423     c                   else
102800140424     c                   eval      p1cp_o3 ='    '
102900140423     c                   endif
103000140423     c                   endif
103100140423     c* % 4
103200140423     c                   if        wcosor4  >0
103300140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
103400140423     c
103500140423     C                   IF        WPERC>0
103600140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
103700140505
103800140505     c                   select
103900140505     c                   when      or_dal(4)<p1dor_dal
104000140505     c                   seton                                        14
104100140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
104200140505     c                   seton                                        14
104300140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
104400140505     c                   seton                                        14
104500140505     c                   endsl
104600140423     c                   else
104700140424     c                   eval      p1cp_o4 ='    '
104800140423     c                   endif
104900140423     c                   endif
105000140423     c* % 5
105100140423     c                   if        wcosor5  >0
105200140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
105300140423     c
105400140423     C                   IF        WPERC>0
105500140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
105600140505
105700140505     c                   select
105800140505     c                   when      or_dal(5)<p1dor_dal
105900140505     c                   seton                                        15
106000140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
106100140505     c                   seton                                        15
106200140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
106300140505     c                   seton                                        15
106400140505     c                   endsl
106500140423     c                   else
106600140424     c                   eval      p1cp_o5 ='    '
106700140423     c                   endif
106800140423     c                   endif
106900140423     c* % 6
107000140423     c                   if        wcosor6  >0
107100140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
107200140423     c
107300140423     C                   IF        WPERC>0
107400140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
107500140505
107600140505     c                   select
107700140505     c                   when      or_dal(6)<p1dor_dal
107800140505     c                   seton                                        16
107900140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
108000140505     c                   seton                                        16
108100140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
108200140505     c                   seton                                        16
108300140505     c                   endsl
108400140423     c                   else
108500140424     c                   eval      p1cp_o6 ='    '
108600140423     c                   endif
108700140423     c                   endif
108800140423     c* % 7
108900140423     c                   if        wcosor7  >0
109000140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
109100140423     c
109200140423     C                   IF        WPERC>0
109300140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
109400140505
109500140505     c                   select
109600140505     c                   when      or_dal(7)<p1dor_dal
109700140505     c                   seton                                        17
109800140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
109900140505     c                   seton                                        17
110000140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
110100140505     c                   seton                                        17
110200140505     c                   endsl
110300140423     c                   else
110400140424     c                   eval      p1cp_o7 ='    '
110500140423     c                   endif
110600140423     c                   endif
110700140423     c* % 8
110800140423     c                   if        wcosor8  >0
110900140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
111000140423     c
111100140423     C                   IF        WPERC>0
111200140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
111300140505
111400140505     c                   select
111500140505     c                   when      or_dal(8)<p1dor_dal
111600140505     c                   seton                                        18
111700140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
111800140505     c                   seton                                        18
111900140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
112000140505     c                   seton                                        18
112100140505     c                   endsl
112200140423     c                   else
112300140424     c                   eval      p1cp_o8 ='    '
112400140423     c                   endif
112500140423     c                   endif
112600140423     c* % 9
112700140423     c                   if        wcosor9  >0
112800140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
112900140423     c
113000140423     C                   IF        WPERC>0
113100140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
113200140505
113300140505     c                   select
113400140505     c                   when      or_dal(9)<p1dor_dal
113500140505     c                   seton                                        19
113600140505     c                   when      paroltre >p1dor_al
113700140505     c                   seton                                        19
113800140505     c                   endsl
113900140423     c                   else
114000140424     c                   eval      p1cp_o9 ='    '
114100140423     c                   endif
114200140423     c                   endif
114300140422     c
114400140514     c                   if        partser='C'
114500140422     c                   write     STO4det_d
114600140514     c                   else
114700140514     c                   write     STO4det_d3
114800140514     c                   endif
114900140422     c                   ENDSR
115000140423     C*------------------------------------------------------------------------*
115100140423     c* Stampa totali di filiale
115200140423     C*------------------------------------------------------------------------*
115300140423     c     TOTfil        BEGSR
115400140423     c* voglio la % di correttezza
115500140423     c                   if        parcor='C'
115600140423     c* Calcolo % STD
115700140619     c                   if        tot_tsos>0
115800140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
115900140423     c                   eval      p1dstsos=tot_TSOs
116000140619     c                   else
116100140619     c                   clear                   p1dperc_os
116200140619     c                   clear                   p1dstsos
116300140619     c                   endif
116400140423     c* Calcolo % MIN/MAX
116500140619     c                   if        tot_tsom>0
116600140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
116700140423     c                   eval      p1dstsom=tot_TSOm
116800140619     c                   else
116900140619     c                   clear                   p1dperc_om
117000140619     c                   clear                   p1dstsom
117100140619     c                   endif
117200140423     c
117300140423     c                   else
117400140423     c* voglio la % delle fuori orario
117500140423     c                   eval      wfuori_os=tot_tse-tot_tsos
117600140619     c                   if        wfuori_os>0
117700140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
117800140423     c                   eval      p1dstsos=Wfuori_os
117900140619     c                   else
118000140619     c                   clear                   p1dperc_os
118100140619     c                   clear                   p1dstsos
118200140619     c                   endif
118300140423
118400140423     c                   eval      wfuori_om=tot_tse-tot_tsom
118500140619     c                   if        wfuori_om>0
118600140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
118700140423     c                   eval      p1dstsom=Wfuori_om
118800140619     c                   else
118900140619     c                   clear                   p1dperc_om
119000140619     c                   clear                   p1dstsom
119100140619     c                   endif
119200140423     c
119300140423     c                   endif
119400140423
119500140514     c                   if        partser='C'
119600140423     c                   if        *in06
119700140423     c                   write     sto4te
119800140423     c                   write     STO4TEF
119900140423     c                   write     STO4RI
120000140423     c                   setoff                                       06
120100140423     c                   else
120200140423     c                   write     STO4RI
120300140930     c                   endif
120400140930     c
120500140423     c                   write     STO4TOT
120600140930     c                   write     STO4RI
120700140514     c                   else
120800140514     c                   if        *in08
120900140514     c                   write     sto4te2
121000140514     c                   write     STO4TEF2
121100140514     c                   write     STO4RI2
121200140514     c                   setoff                                       08
121300140514     c                   else
121400140514     c                   write     STO4RI2
121500140930     c                   endif
121600140514     c                   write     STO4TOT2
121700140930     c                   write     STO4RI2
121800140514     c                   endif
121900140423     c                   ENDSR
122000000000     C*------------------------------------------------------------------------*
122100000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
122200000000     C*------------------------------------------------------------------------*
122300000000     C     CARORG        BEGSR
122400000000     C*
122500140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
122600000000     C  N99              READ      AZORG01L                               99
122700000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
122800000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
122900000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
123000000000     C     ORGFAG        IFNE      'V'
123100051109     C                   ADD       1             I
123200000000     C                   Z-ADD     ORGFIL        FIL(I)
123300000000     C                   Z-ADD     ORGCAR        ARE(I)
123400140404     C* CALCOLA NETWORK
123500140404     C                   MOVEL     ORGDE3        OG143
123600140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
123700051109     c* area
123800051109     C                   Z-ADD     CODUT         KTBUTE
123900051109     C                   MOVEL     '05'          Ktbcod
124000051109     C                   MOVEL(P)  ORGCAR        KKEY
124100051109     C     KTAB          CHAIN     TABEL00F                           30
124200051109    2C     *IN30         IFEQ      *OFF
124300051109     C     TBLFLG        ANDEQ     *BLANKS
124400051109     C                   MOVEL     TBLUNI        DS05
124500140408     C***                MOVEL     §05DES        dare(I)
124600140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
124700051109     C                   ELSE
124800140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
124900140408     C***                MOVE      *BLANKS       dare(I)
125000051109    2C                   END
125100051109     c* p.o.
125200051109     C                   MOVEl     orgdes        dfil(I)
125300051109     c
125400000000     C                   END
125500000000     C                   END                                                    <-- 2 ---
125600000000     C                   READ      AZORG01L                               99
125700000000     C                   END                                                    <-- 1 ---
125800000000     C*
125900000000     C                   ENDSR
126000000000     C*------------------------------------------------------------------------*
126100051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
126200000000     C*------------------------------------------------------------------------*
126300000000     C     CARTBL        BEGSR
126400160408     c                   clear                   wdoppiag          1
126500140516     C
126600140516     c* apertura file distinte di filiale
126700140516     c                   if        partser='C'
126800140516     C                   OPEN(e)   FIDST01L
126900140516     c
127000140516     c                   if        not %open(FIDST01L)
127100140516     c                   eval      %subst(WFDST:7:4)='PRD '
127200140516     c                   eval      %subst(TITAS:7:4)='GRPS'
127300150505     c                   eval      %subst(TITA4:7:4)='GRPS'
127400140516     c                   eval      %subst(FIAR5:7:4)='GRPS'
127500140516     C                   OPEN      FIDST01L
127600140516     c                   endif
127700140516
127800140516     C                   OPEN(e)   TITAS26C
127900150505     C                   OPEN(e)   TITA430C
128000140516     C                   OPEN(e)   FIAR531C
128100140516     c                   open      tisto4p
128200140516     c                   open      tisto4p1
128300140516     c                   else
128400140516     c                   open      fnorf06l
128500140516     c                   open      fnorm01l
128600140516     c                   open      tisto4p2
128700140516     c                   open      tisto4p3
128800140516     c                   endif
128900140516     C
129000000000     C*--------------------
129100051109     C* TIPI BOLLA di recupero
129200000000     C*--------------------
129300000000     C                   Z-ADD     0             I
129400000000     C                   Z-ADD     CODUT         KTBUTE
129500000000     C                   MOVEL     'TB'          KTBCOD
129600000000     C     KEYTAB        SETLL     TABEL00F
129700051109     C                   READe     TABEL00F                               99
129800000000     C     *IN99         DOWEQ     *OFF
129900000000     C     TBLFLG        IFEQ      *BLANKS
130000051109     C                   MOVEL     TBLUNI        DSTB
130100051109     c                   if        §tbrbl = 'R'
130200051109     C                   ADD       1             I
130300051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
130400051109     c                   endif
130500000000     C                   END
130600051109     C     keytab        READe     TABEL00F                               99
130700000000     C                   END
130800000000     C*
130900140428     c* recupero versione cappario alla data DAL
131000140428     C                   Z-ADD     pardsd        P96DRI            8 0
131100140428     C                   Z-ADD     *ZEROS        P96VER            5 0
131200140428     C                   CALL      'TISI96R'
131300140428     C                   PARM                    P96DRI
131400140428     C                   PARM                    P96VER
131500140428     c* carico gli orari in schiera
131600140428     c                   z-add     1             yy
131700140428     c                   if        paror1 >0
131800140428     c                   eval      or_dal(yy)=paror1
131900140428     c                   add       1             yy
132000140428     c                   endif
132100140428     c                   if        paror2 >0
132200140428     c                   eval      or_dal(yy)=paror2
132300140428     c                   add       1             yy
132400140428     c                   endif
132500140428     c                   if        paror3 >0
132600140428     c                   eval      or_dal(yy)=paror3
132700140428     c                   add       1             yy
132800140428     c                   endif
132900140428     c                   if        paror4 >0
133000140428     c                   eval      or_dal(yy)=paror4
133100140428     c                   add       1             yy
133200140428     c                   endif
133300140428     c                   if        paror5 >0
133400140428     c                   eval      or_dal(yy)=paror5
133500140428     c                   add       1             yy
133600140428     c                   endif
133700140428     c                   if        paror6 >0
133800140428     c                   eval      or_dal(yy)=paror6
133900140428     c                   add       1             yy
134000140428     c                   endif
134100140428     c                   if        paror7 >0
134200140428     c                   eval      or_dal(yy)=paror7
134300140428     c                   add       1             yy
134400140428     c                   endif
134500140428     c                   if        paror8 >0
134600140428     c                   eval      or_dal(yy)=paror8
134700140428     c                   add       1             yy
134800140428     c                   endif
134900140428     c                   if        paror9 >0
135000140428     c                   eval      or_dal(yy)=paror9
135100140428     c                   add       1             yy
135200140428     c                   endif
135300140428     C*
135400000000     C                   ENDSR
135500140512     C*------------------------------------------------------------------------*
135600140512     C* Ciclo di lettura su FNORF
135700140512     C*------------------------------------------------------------------------*
135800140512     c     LET_ORF       BEGSR
135900140512     C                   MOVEL     '0'           FINL00            1
136000140512     c
136100140512     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
136200140512     C     FINL00        IFEQ      '0'                                          --- 1 -->
136300140512     C*
136400140512     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
136500140512     C                   EXSR      SETOrf
136600140512     C*
136700140512     C* CICLO FINO A FINE FILE
136800140512     C     FINL00        DOWEQ     '0'                                          --- 2 -->
136900140512     C*
137000140512     C* CREA IL RECORD SUI FILE DI LAVORO
137100140512     C                   EXSR      CRErecR
137200140512     C*
137300140512     C* LEGGE IL RECORD SUCCESSIVO
137400140512     C                   EXSR      LETOrf
137500140512     C                   ENDDO                                                  <-- 2 ---
137600140512     C*
137700140512     C                   ENDIF                                                  <-- 1 ---
137800140512     c                   ENDSR
137900140512     C*------------------------------------------------------------------------*
138000140514     C* SETorf - POSIZIONAMENTO SUL FILE
138100140512     C*------------------------------------------------------------------------*
138200140512     C     SETOrf        BEGSR
138300140512     C*
138400140512     C* POSIZIONAMENTO E PRIMA LETTURA
138500140512     C*
138600140512     C                   movel     pardsd        Kdae
138700140512     C*
138800140512     C     Korf          SETLL     FNORF06L                           99        *FINE FILE
138900140512     C*
139000140512     C     *IN99         IFEQ      '1'
139100140512     C                   MOVEL     '1'           FINL00
139200140512     C                   ELSE
139300140512     C                   EXSR      LETOrf
139400140512     C                   END
139500140512     C*
139600140512     C                   ENDSR
139700140512     C*------------------------------------------------------------------------*
139800140512     C* LETFIL - LETTURA PROSSIMO RECORD
139900140512     C*------------------------------------------------------------------------*
140000140512     C     LETOrf        BEGSR
140100140512     C*
140200140512     C* LEGGO FINO A:
140300140512     C*     - FINE FILE O SUPERATO LIMITE FINALE
140400140512     C*     - TROVATO RECORD VALIDO
140500140512     C                   MOVEL     'N'           WRECOK
140600140512    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
140700140512     C     WRECOK        ANDEQ     'N'
140800140512     C*
140900140512     C*
141000140512     C     Korfp         READE     fnorf06l                               99
141100140512     c
141200140512    3C                   IF        *IN99  OR orfdae>PARDSA
141300140512     C                   MOVEL     '1'           FINL00
141400140514     c                   else
141500140512     c
141600140512     C                   EXSR      CHKREC_O                                     *CONTR.VALIDITA REC.
141700140512    2C                   END                                                    <-- 1 ---
141800140512     C*
141900140512    1C                   ENDDO                                                  <-- 1 ---
142000140512     C*
142100140512     C                   ENDSR
142200140512     C*------------------------------------------------------------------------*
142300140512     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD per Ritiri
142400140512     C*------------------------------------------------------------------------*
142500140512     C     CHKREC_O      BEGSR
142600140512     C*
142700140512     C                   MOVEL     'S'           WRECOK            1
142800140512     C*
142900140512     c* Escludo i ritiri contestuali alla consegna
143000140512     c                   if        orfnsr>0
143100140512     C                   MOVEL     'N'           WRECOK
143200140512     c                   endif
143300140512     c* Escludo gli ORM pronta merce: stabiliti col cliente fuori dagli orari std
143400140514     c     korm          chain     fnorm01l
143500140514     c                   if        %found(fnorm01l) and ormorr>0
143600140514     C****               MOVEL     'N'           WRECOK
143700140512     c                   endif
143800140512     C*
143900140512     C                   ENDSR
144000140512     C*------------------------------------------------------------------------*
144100140512     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO  per i ritiri
144200140512     C*------------------------------------------------------------------------*
144300140512     C     CRErecR       BEGSR
144400140515
144500140515     c                   movel     orfore        wora
144600140512     c                   clear                   kloc
144700140512     c
144800140512     c* Richiamo pgm reperimento orari servizi
144900140512     c                   clear                   trulorsds
145000140512     c                   clear                   trulor2ds
145100140516     c                   clear                   trulor3ds
145200140512     c                   eval      ioredta=ormdar
145300140512     c                   eval      iorefil=orfpog
145400140512     c                   eval      iorenar=ormnar
145500140512     c                   eval      iorecap=ormcar
145600140516     c                   eval      ioreloc=ormlor
145700140512     c                   eval      ioretser=partser
145800140516     c*****              eval      IOREgf2 = O95gf2
145900140514     c                   eval      ioretsp ='C'
146000140512     c                   call      'TRULORSR'
146100140512     c                   parm                    kpjba
146200140512     c                   parm                    trulorsds
146300140512     c                   parm                    trulor2ds
146400140516     c                   parm                    trulor3ds
146500140516     c                   eval      wnorm=OOR3NORM
146600140516     c
146700140516     c                   if        oor3norm='S'
146800140516     c                   eval      kloc =OOR3LOC_N
146900140516     c                   endif
147000140512     c*
147100140512     c                   clear                   kserv
147200140514     c* se l'orario di ritiro  non rientra negli orari standard verifico se orario
147300140514     c* concordato col cliente ed in questo caso scarto il record
147400140515     c                   if        (wora  <OOR2STIS  and
147500140514     c                              ormcra>0 and
147600140514     c                              ormorr<oor2stis)     or
147700140515     c                             (wora  >OOR2STFS  and
147800140514     c                              ormcra>0 and
147900140514     c                              ormorr>oor2stfs)
148000140514     c                   leavesr
148100140514     c                   endif
148200140512
148300140512     c     kccosR        chain     wfccos1l
148400140512     c                   if        not %found(wfccos1l)
148500140512     c                   clear                   wfccos00
148600140512     c                   eval      wcosdcmd=pardsd
148700140512     c                   eval      wcosdcma=pardsa
148800140512     c                   eval      wcosver =p96ver
148900140512     c                   eval      wcoslna =kfil
149000140512     c                   eval      wcosnaz =ormnar
149100140512     c                   eval      wcoscap =ormcar
149200140513     c                   eval      wcosloc =kloc
149300140512     c                   eval      wcostser=partser
149400140512     c                   eval      wcosorif=OOR2ORIF
149500140512     c                   eval      wcosserv=kserv
149600140512     c* imposta campi cap/località obsoleto
149700140512     c                   exsr      sr_impobs
149800140512     c* Imposto gli orari nel file
149900140512     c                   eval      WCOSOSTIS=OOR2STIS
150000140512     c                   eval      WCOSOSTfS=OOR2STfS
150100140512     c
150200140512     c* imposto orari dettaglio se ci sono e richiesto
150300140512     c                   if        pardet='S'
150400140512     c                   exsr      sr_ImpOrDet
150500140512     c                   endif
150600140512     c                   endif
150700140512     c
150800140512     c                   add       1             wcosts
150900140512     c* Se loc non normalizzata
151000140516     c                   if        wnorm<>'S'
151100140512     c                   add       1             wcostsNN
151200140512     c                   endif
151300140512     c
151400140512     c*** serve in questo caso? E' uguale a wcosts....
151500140512     c                   add       1             wcostse
151600140512     c
151700140512     c* ritiro negli orari std
151800140515     c                   if        wora  >=OOR2STIS  and wora  <=OOR2STFS
151900140512     c                   add       1             wcostsos
152000140512     c                   endif
152100140512     c* imposto campi dettaglio
152200140513     c                   exsr      ImpoDETuni
152300140512     c
152400140512     c                   if        not %found(wfccos1l)
152500140512     c                   write     wfccos00
152600140512     c                   else
152700140512     c                   update    wfccos00
152800140512     c                   endif
152900140512     C*
153000140512     C                   ENDSR
153100140513     C*------------------------------------------------------------------------*
153200140513     C* Imposto i campi di dettaglio in base ai range passati
153300140513     C*------------------------------------------------------------------------*
153400140513     C     ImpoDETUni    BEGSR
153500140513     c*
153600140513     c                   if        wora  <or_dal(1)
153700140513     c                   add       1             wcosmint
153800140513     c                   leavesr
153900140513     c                   endif
154000140513     c                   if        wora  > paroltre
154100140513     c                   add       1             wcosmaxt
154200140513     c                   leavesr
154300140513     c                   endif
154400140513
154500140513     c                   z-add     1             yy
154600140513     c                   dow       or_dal(yy)>0
154700140513     c                   if        wora  >=or_dal(yy) and
154800140513     c
154900140513     c                             (or_dal(yy+1)>0 and
155000140513     c                              wora  < or_dal(Yy+1)  or
155100140513     c                             (or_dal(yy+1)=0 and
155200140513     c                             wora  <=paroltre))
155300140513     c                   select
155400140513     c                   when      yy=1
155500140513     c                   add       1             wcosor1t
155600140513     c                   when      yy=2
155700140513     c                   add       1             wcosor2t
155800140513     c                   when      yy=3
155900140513     c                   add       1             wcosor3t
156000140513     c                   when      yy=4
156100140513     c                   add       1             wcosor4t
156200140513     c                   when      yy=5
156300140513     c                   add       1             wcosor5t
156400140513     c                   when      yy=6
156500140513     c                   add       1             wcosor6t
156600140513     c                   when      yy=7
156700140513     c                   add       1             wcosor7t
156800140513     c                   when      yy=8
156900140513     c                   add       1             wcosor8t
157000140513     c                   when      yy=9
157100140513     c                   add       1             wcosor9t
157200140513     c                   endsl
157300140513     c
157400140513     c                   leavesr
157500140513     c                   endif
157600140513     c                   add       1             yy
157700140513     c
157800140513     c                   enddo
157900140513     c
158000140513     c                   ENDSR
158100140512     C*------------------------------------------------------------------------*
158200140512     C* Imposta campi "obsloleto"
158300140512     C*------------------------------------------------------------------------*
158400140512     C     sr_impobs     BEGSR
158500140512     c* obsoleto
158600140512     c     kcpc          chain     azcpc01l
158700140512     c                   if        %found(azcpc01l)
158800140512     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
158900140512     c                   endif
159000140512     c                   if        wcosloc<>*blanks
159100140512     c     kcpl          chain     azcpl01l
159200140512     c                   if        %found(azcpl01l)
159300140512     c                   eval      wcosloco=%subst(cplfi1:1:1)
159400140512     c                   endif
159500140512     c                   endif
159600140512     c                   endsr
159700140512     C*------------------------------------------------------------------------*
159800140512     C* Imposta campi orari dettaglio
159900140512     C*------------------------------------------------------------------------*
160000140512     C     sr_ImpOrDet   BEGSR
160100140512     c                   eval      wcosor1=or_dal(1)
160200140512     c                   eval      wcosor2=or_dal(2)
160300140512     c                   eval      wcosor3=or_dal(3)
160400140512     c                   eval      wcosor4=or_dal(4)
160500140512     c                   eval      wcosor5=or_dal(5)
160600140512     c                   eval      wcosor6=or_dal(6)
160700140512     c                   eval      wcosor7=or_dal(7)
160800140512     c                   eval      wcosor8=or_dal(8)
160900140512     c                   eval      wcosor9=or_dal(9)
161000140512     c                   eval      wcosorOl=paroltre
161100140512     c                   endsr
161200000000     C*------------------------------------------------------------------------*
161300000000     C* *INZSR - ROUTINE INIZIALE
161400000000     C*------------------------------------------------------------------------*
161500000000     C     *INZSR        BEGSR
161600000000     C*--------------------
161700000000     C* RICEVIMENTO PARAMETRI
161800000000     C*--------------------
161900140408     c
162000140408     c     *dtaara       define    §azute        azuteds
162100140408     c     *dtaara       define    §datiute      ddatiute
162200140408     c                   in(E)     *dtaara
162300140408     c                   If        %error  or RSUT = *blanks
162400140408     c                   CLEAR                   tibs34ds
162500140408     c                   CALL      'TIBS34R'
162600140408     c                   PARM                    tibs34ds
162700140408     c                   in        *dtaara
162800140408     c                   EndIf
162900140408     c
163000140408     c                   eval      p1drsut=rsut
163100140408     C*
163200000000     C*--------------------
163300000000     C* CAMPI RIFERITI AL DATA BASE
163400000000     C*--------------------
163500000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
163600000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
163700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
163800051109     C     *LIKE         DEFINE    TBLkey        Kkey
163900000000     C*--------------------
164000000000     C* CHIAVI DI LETTURA
164100000000     C*--------------------
164200140404     C* CHIAVE LETTURA TITAS26C - COMPLETA
164300140404     C     Ktas          KLIST
164400140404     C                   KFLD                    Kdcm
164500140404     C                   KFLD                    Kfil
164600140512     C* CHIAVE LETTURA FNORF06L - COMPLETA
164700140512     C     Korf          KLIST
164800140512     C                   KFLD                    Kfil
164900140512     C                   KFLD                    Kfar
165000140512     C                   KFLD                    Kdae
165100140514     C*
165200140514     C     Korfp         KLIST
165300140514     C                   KFLD                    Kfil
165400140514     C                   KFLD                    Kfar
165500140514     C     Korm          KLIST
165600140514     C                   KFLD                    orfpoe
165700140514     C                   KFLD                    orfnsr
165800140514     C                   KFLD                    orfnor
165900140514     C                   KFLD                    orfnrv
166000000000     C*
166100000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
166200000000     C     KEYTAB        KLIST
166300000000     C                   KFLD                    KTBUTE
166400000000     C                   KFLD                    KTBCOD
166500051109     C     KTAB          KLIST
166600051109     C                   KFLD                    ktbute
166700051109     C                   KFLD                    ktbcod
166800051109     C                   KFLD                    KKEY
166900000000     C*
167000060524     C     Kar5          KLIST
167100060524     C                   KFLD                    tasAAS
167200060524     C                   KFLD                    taslnp
167300060524     C                   KFLD                    tasnrs
167400060524     C                   KFLD                    tasnsp
167500060524     c                   kfld                    kar5trd
167600150505     C     Kta4          KLIST
167700150505     C                   KFLD                    tasAAS
167800150505     C                   KFLD                    taslnp
167900150505     C                   KFLD                    tasnrs
168000150505     C                   KFLD                    tasnsp
168100150505     c                   kfld                    ktrc
168200140404     C*
168300140404     C     Kdst          KLIST
168400140404     C                   KFLD                    Knpg
168500140404     C                   KFLD                    knFV
168600140404     C                   KFLD                    Kfgs
168700140404     c
168800140415     C     KCCOS         KLIST
168900140415     C                   KFLD                    partser
169000140423     C                   KFLD                    d55tfa
169100140404     C                   KFLD                    tasnzd
169200140404     C                   KFLD                    tascad
169300140404     C                   KFLD                    Kloc
169400140415     C                   KFLD                    kserv
169500140514     C     KCCOSr        KLIST
169600140514     C                   KFLD                    partser
169700140514     C                   KFLD                    kfil
169800140514     C                   KFLD                    ormnar
169900140514     C                   KFLD                    ormcar
170000140514     C                   KFLD                    Kloc
170100140514     C                   KFLD                    kserv
170200140424     C     Kcpc          KLIST
170300140424     C                   KFLD                    p96ver
170400140424     C                   KFLD                    wcosnaz
170500140424     C                   KFLD                    wcoscap
170600140424     C     Kcpl          KLIST
170700140424     C                   KFLD                    p96ver
170800140424     C                   KFLD                    wcosnaz
170900140424     C                   KFLD                    wcoscap
171000140424     C                   KFLD                    wcosloc
171100150717     C     Kcpl2         KLIST
171200150717     C                   KFLD                    p96ver
171300150717     C                   KFLD                    tasnzd
171400150717     C                   KFLD                    tascad
171500150717     C                   KFLD                    taslod
171600000000     C*--------------------
171700000000     C                   Z-ADD     1             CODUT             1 0
171800000000     C*
171900000000     C***
172000000000     C* CARICA ORGANIGRAMMA
172100000000     C                   EXSR      CARORG
172200000000     C*
172300000000     C                   ENDSR
