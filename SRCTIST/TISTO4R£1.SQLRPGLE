000100000000     H*------------------------------------------------------------------------*
000200140404     H* Statistica controllo orari srervizi  - Crea Work file
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500140409     FTITAS26C  IF   E           K DISK    extfile(titas) usropn
000600140508     Ffidst01l  IF   E           K DISK    extfile(wfdst) usropn
000700140409     Ffiar531c  IF   E           K DISK    extfile(fiar5) usropn
000701140514     Ffnorf06l  IF   E           K DISK    usropn
000702140514     Ffnorm01l  IF   E           K DISK    usropn
000800000000     FAZORG01L  IF   E           K DISK
000900140408     FAZORG02L  IF   E           K DISK    rename(azorg:azorg002)
001000000000     FTABEL00F  IF   E           K DISK
001100140424     Fazcpc01l  IF   E           K DISK
001200140424     Fazcpl01l  IF   E           K DISK
001300140404     FWFCCOS1l  UF A E           K DISK
001400140514     FTISTO4P   O    E             PRINTER OFLIND(*IN06) usropn
001500140514     FTISTO4P1  O    E             PRINTER OFLIND(*IN07) usropn
001501140514     FTISTO4P2  O    E             PRINTER OFLIND(*IN08) usropn
001502140514     FTISTO4P3  O    E             PRINTER OFLIND(*IN09) usropn
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
010800140404     D kloc            s                   like(o95loc)
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
011901140514     d wora            s                   like(tashmc)
011902140512     D kfar            s                   like(orffar) inz(420)
011903140512     D kdae            s                   like(orfdae) inz
012000140404     d
012100140508     d WFDST           s             21    inz('FILTRA201 /FIDST01L')
012200140409     d titas           s             21    inz('GAITRAGRU /TITAS26C')
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
013901140514     c* Consegne
013902140514     c                   if        partser='C'
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
016100140408     c                   exsr      Let_TAS
016200140423     c                   add       1             XX
016300140408     c                   enddo
016301140514     c                   else
016302140514     c* Ritiri
016303140514     c                   eval      kfil=parfil
016304140514     c                   exsr      Let_ORF
016305140514     c                   endif
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
018901140514     c                   if        partser='C'
019000140408     c                   exsr      Let_TAS
019001140514     c                   else
019002140514     c                   exsr      Let_ORF
019003140514     c                   endif
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
020301140514     c                   if        partser='C'
020400140408     c                   exsr      Let_TAS
020401140514     c                   else
020402140514     c                   exsr      Let_ORF
020403140514     c                   endif
020500140408     c                   read      azorg02l
020600140408     c                   enddo
020700140408     c
020800140408     c                   endsl
020900140404     c*
021000140404     c                   EXSR      Stampa
021100140408     c*
021101140514     c                   if        partser='C'
021200140408     c                   write     STO4FI
021201140514     c                   else
021202140514     c                   write     STO4FI2
021203140514     c                   endif
021300140422     c
021400140422     c                   if        pardet='S'
021401140514     c                   if        partser='C'
021500140422     c                   write     STO4RI_d
021600140422     c                   write     STO4FI_d
021601140514     c                   else
021602140514     c                   write     STO4RI_d3
021603140514     c                   write     STO4FI_d3
021604140514     c                   endif
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
022900140409     C                   MOVEL     '0'           FINL00            1
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
024200140408     c
024300140408     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
024400140408     C     FINL00        IFEQ      '0'                                          --- 1 -->
024500140408     C*
024600140408     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
024700140408     C                   EXSR      SETFIL
024800140408     C*
024900140408     C* CICLO FINO A FINE FILE
025000140408     C     FINL00        DOWEQ     '0'                                          --- 2 -->
025100140408     C*
025200140408     C* CREA IL RECORD SUI FILE DI LAVORO
025300140408     C                   EXSR      CRErec
025400140408     C*
025500140408     C* LEGGE IL RECORD SUCCESSIVO
025600140408     C                   EXSR      LETFIL
025700140408     C                   ENDDO                                                  <-- 2 ---
025800140408     C*
025900140408     C                   ENDIF                                                  <-- 1 ---
026000140408     c                   ENDSR
026100000000     C*------------------------------------------------------------------------*
026200000000     C* SETFIL - POSIZIONAMENTO SUL FILE
026300000000     C*------------------------------------------------------------------------*
026400000000     C     SETFIL        BEGSR
026500000000     C*
026600000000     C* POSIZIONAMENTO E PRIMA LETTURA
026700000000     C*
026800140404     C                   movel     pardsd        Kdcm
026900991123     C*
027000140404     C     Ktas          SETLL     TITAS26C                           99        *FINE FILE
027100000000     C*
027200000000     C     *IN99         IFEQ      '1'
027300000000     C                   MOVEL     '1'           FINL00
027400000000     C                   ELSE
027500000000     C                   EXSR      LETFIL
027600000000     C                   END
027700000000     C*
027800000000     C                   ENDSR
027900140404     C*------------------------------------------------------------------------*
028000000000     C* LETFIL - LETTURA PROSSIMO RECORD
028100000000     C*------------------------------------------------------------------------*
028200000000     C     LETFIL        BEGSR
028300000000     C*
028400000000     C* LEGGO FINO A:
028500000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
028600000000     C*     - TROVATO RECORD VALIDO
028700000000     C                   MOVEL     'N'           WRECOK
028800140404    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
028900000000     C     WRECOK        ANDEQ     'N'
029000000000     C*
029100140404     C*
029200140404    2c                   if        kfil>0
029300140404     C     KTAS          READE     TITAS26C                               99
029400140404     c                   else
029500140404     C     Kdcm          READE     TITAS26C                               99
029600140404    2c                   endif
029700140404     c
029800140404    2C                   IF        *IN99
029900140404     C     KDCM          SETGT     TITAS26C
030000140404     C                   READ      TITAS26C                               99
030100140404    3C                   IF        *IN99  OR TASDCM>PARDSA
030200140404     C                   MOVEL     '1'           FINL00
030300140404     C                   ELSE
030400140404     C                   EVAL      KDCM=TASDCM
030500140404     C     KTAS          setll     TITAS26C
030600140404    3C                   ENDIF
030700140404
030800140404   x2C                   ELSE
030900140404     c
031000140404     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
031100140404    2C                   END                                                    <-- 1 ---
031200140404     C*
031300140404    1C                   ENDDO                                                  <-- 1 ---
031400000000     C*
031500000000     C                   ENDSR
031600000000     C*------------------------------------------------------------------------*
031700000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
031800000000     C*------------------------------------------------------------------------*
031900000000     C     CHKREC        BEGSR
032000000000     C*
032100000000     C                   MOVEL     'S'           WRECOK            1
032200040305     C*
032300140414     c*
032400051109     C* ESCLUDE LE BOLLE di recupero
032500051109     C                   Z-ADD     1             i
032600000000     C                   SETOFF                                           28
032700051109     C     TASTBL        LOOKUP    TB(i)                                  28
032800051109     C                   IF        *in28 = *on
032900000000     C                   MOVEL     'N'           WRECOK
033000000000     C                   ENDIF
033100140404     c
033200140414     c* Escludo bolle export e le bolle con consegna anomala
033300140414     c                   if        tasnzd<>*blanks or tascca<>' '
033400140408     C                   MOVEL     'N'           WRECOK
033500140408     c                   endif
033600140414     c
033700140411     c* Escludo le bolle in fermo deposito
033800140411     c                   if        tasffd='S'
033900140411     C                   MOVEL     'N'           WRECOK
034000140411     c                   endif
034100011112     C*
034200000000     C                   ENDSR
034300140408     C*------------------------------------------------------------------------*
034400140408     C* controllo se si tratta di bolla da elaborare
034500140408     C*------------------------------------------------------------------------*
034600140408     C     NOELAB        BEGSR
034700140408     C*
034800140408     C                   MOVEL     'S'           Welab             1
034900140408     C*
035000140408     c* Cerco se si tratta di distinta PDA
035100140508     c* solo in AS888
035200140508    1c                   if        %subst(knsif:7:1)='P'
035300140508     c                   movel     taspdc        kfgs              3 0
035400140508     c                   movel     4             knpg              1 0
035500140508     c                   z-add     tasndc        knfv
035600140508     c     kdst          chain     fidst01l
035700140508    2c                   if        %found(fidst01l)
035800140508     c                             and (dstpda='C' or dstpda='E')
035900140508   x2c                   else
036000140508     C                   MOVEL     'N'           Welab
036100140508    2c                   endif
036200140508     c
036300140508   x1c                   else
036400140508   c
036500140508     c* Solo dall'aprile 2014 abbiamo le bolle con il flag impostato
036600140508     c                   if        tasaas<2014
036700140508     C                   MOVEL     'N'           Welab
036800140508     c                   endif
036900140508     c
037000140508     c                   if        tasaas=2014 and tasmgs<0401
037100140508     C                   MOVEL     'N'           Welab
037200140508     c                   endif
037300140408     c* chaino record GEN
037400140408     c     kar5          chain     fiar531c
037500140408     c                   if        %found(fiar531c)
037600140408     c                   eval      dar5gen=ar5uni
037700140408     c                   if        §AR5PDACO<>'  '
037800140408     C                   MOVEL     'N'           Welab
037900140408     c                   endif
038000140408     c                   endif
038100140508     c
038200140508    1C                   ENDIF
038300140408     c
038400140411     c* Non elaboro se impostata ora consegna richiesta
038500140411     c                   if        tashcr>0
038600140408     C                   MOVEL     'N'           Welab
038700140408     c                   endif
038800140408     c
038900140408     C                   ENDSR
039000000000     C*------------------------------------------------------------------------*
039100140408     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO
039200000000     C*------------------------------------------------------------------------*
039300140408     C     CRErec        BEGSR
039400140404     c* recupero record azcos normalizzando prima la località
039500140404     c                   exsr      normloc
039600140404     c                   clear                   kloc
039700140404     c
039800140404     c                   if        wnorm='S'
039900140404     c                   eval      kloc=o95loc
040000140404     c                   endif
040100140404     c
040200140404     c* Richiamo pgm reperimento orari servizi
040300140404     c                   clear                   trulorsds
040400140404     c                   clear                   trulor2ds
040500140404     c                   eval      ioredta=pardsd
040600140408     c                   eval      iorefil=taslna
040700140404     c                   eval      iorenar=tasnzd
040800140404     c                   eval      iorecap=tascad
040900140407     c                   eval      ioreloc=o95loc
041000140414     c                   eval      ioreddc=tasddc
041100140414     c                   eval      iorendc=tasndc
041200140414     c                   eval      ioretfp=tastfp
041300140414     c                   eval      ioretfa=tastfa
041400140414     c                   eval      ioredti=tasdti
041500140428     c                   eval      iorehti=9999
041600140404     c                   eval      ioretser=partser
041601140515     c* si forza sempre "C" perché per "H" e "E"  l'orario viene accorciato
041602140515     c* automaticamente e non è l'obiettivo della statistica
041700140515     c                   eval      ioretsp ='C'
041800140404     c                   call      'TRULORSR'
041900140404     c                   parm                    kpjba
042000140404     c                   parm                    trulorsds
042100140404     c                   parm                    trulor2ds
042200140404     c*
042300140415     c* se per reperire gli orari ho usato il campo soglia, lo uso in chain
042400140415     c                   if        oor2fcsgl='P'
042500140415     c                   eval      kserv='P '
042600140415     c                   else
042700140415     c                   clear                   kserv
042800140415     c                   endif
042900140415
043000140415     c     kccos         chain     wfccos1l
043100140404     c                   if        not %found(wfccos1l)
043200140404     c                   clear                   wfccos00
043300140404     c                   eval      wcosdcmd=pardsd
043400140404     c                   eval      wcosdcma=pardsa
043500140404     c                   eval      wcosver =p96ver
043600140411     c                   eval      wcoslna =d55tfa
043700140404     c                   eval      wcosnaz =tasnzd
043800140404     c                   eval      wcoscap =tascad
043900140404     c                   eval      wcosloc =kloc
044000140404     c                   eval      wcostser=partser
044100140404     c                   eval      wcosorif=OOR2ORIF
044200140415     c                   eval      wcosserv=kserv
044300140424     c
044400140424     c* obsoleto
044401140514     c                   exsr      sr_impobs
045500140424     c* Imposto gli orari nel file
045600140424     c                   eval      WCOSOSTIS=OOR2STIS
045700140424     c                   eval      WCOSOSTfS=OOR2STfS
045800140424     c                   eval      WCOSOmiis=OOR2miis
045900140424     c                   eval      WCOSOmxfs=OOR2mxfs
046000140424     c
046100140415     c* imposto orari dettaglio se ci sono e richiestoc
046200140415     c                   if        pardet='S'
046201140514     c                   exsr      sr_ImpOrDet
047300140404     c                   endif
047400140415     c                   endif
047500140404     c
047600140404     c                   add       1             wcosts
047700140404     c* Se loc non normalizzata
047800140404     c                   if        wnorm='N'
047900140404     c                   add       1             wcostsNN
048000140404     c                   endif
048100140408     c
048200140408     c* Controllo se spedizione da elaborare
048300140408     c                   exsr      Noelab
048400140408     c
048500140404     c* elaborate
048600140408     c                   if        welab='S'
048700140404     c                   add       1             wcostse
048800140404     c
048900140404     c* consegna negli orari std
049000140404     c                   if        tashmc>=OOR2STIS  and tashmc<=OOR2STFS
049100140404     c                   add       1             wcostsos
049200140404     c                   endif
049300140404     c* consegna negli orari min/max
049400140404     c                   if        tashmc>=OOR2MIIS  and tashmc<=OOR2MXFS
049500140404     c                   add       1             wcostsom
049600140404     c                   endif
049700140415     c* imposto campi dettaglio
049701140514     c                   z-add     tashmc        wora
049800140514     c                   exsr      ImpoDETUni
049900140408     c                   endif
050000140404     c
050100140404     c                   if        not %found(wfccos1l)
050200140404     c                   write     wfccos00
050300140404     c                   else
050400140404     c                   update    wfccos00
050500140404     c                   endif
050600000000     C*
050700000000     C                   ENDSR
050800140404     C*------------------------------------------------------------------------*
050900140404     C* NORMLOC - Normalizza le località mediante il TISI95
051000140404     C*------------------------------------------------------------------------*
051100140404     C     NORMLOC       BEGSR
051200140404     c                   eval      wnorm='N'
051300140404     c
051400140404     C* Determino il network della bolla corrente
051500140404     C                   z-add     1             i
051600140404     C     tasLNA        lookup    fil(i)                                 55
051700140404     C                   if        %found
051800140404     C                   movel     ntw(i)        wNetwork
051900140404     C                   else
052000140404     C                   movel     *all'X'       wNetwork
052100140404     C                   endif
052200140404     C*
052300140404     C                   clear                   tisi95ds                       *azzera i parametri
052400140404     C                   movel     *blanks       i95tla                         *tipo lancio
052500140404     C                   movel     '7'           i95tcn                         *tipo controllo
052600140404     C                   movel     tasTSP        i95tsp                         *tipo servizio
052700140404     C                   movel     tasNZD        i95nar                         *nazione dest
052800140404     C                   movel     tasCAD        i95cap                         *cap dest
052900140404     C                   movel     tasLOD        i95loc                         *località dest
053000140404     C                   movel     tasPRD        i95prv                         *provincia dest
053100140404     C                   z-add     tasPKF        i95lkg                         * peso da fatturare
053200140404     C                   z-add     tasVLF        i95lmc                         * volume da fattur
053300140404     C                   movel     tasFFD        i95ffd                         * fermo deposito
053400140404     C                   movel     tasFTC        i95fi2                         * 1° cons. partic.
053500140404     C                   move      tasTC2        i95fi2                         * 2° cons. partic.
053600140404     C*
053700140404     C                   eval      i95dat = tasAAS*10000+tasMGS                 *data di riferimento
053800140404     C*
053900140404     C* Se la LNA è filiale FEDEX imposto flag opportuno
054000140404     C                   if        wNetwork = 'FED'
054100140404     C                   eval      i95fi1 = 'S'
054200140404     C                   endif
054300140404     C*
054400140404     C* Se la LNA è filiale DPD imposto flag opportuno
054500140404     C                   if        wNetwork = 'DPD'
054600140404     C                   eval      i95fi1 = 'D'
054700140404     C                   endif
054800140404     C*
054900140404     C                   call      'TISI95R'
055000140404     C                   parm                    tisi95ds
055100140404     C*
055200140404     C* Se nn reperito instradamento considero i dati originali della bolla
055300140404     C                   if        o95lid = '3' or o95lid = '4'
055400140404     c                   eval      wnorm='S'
055500140404     C                   else
055600140404     C                   movel     tasLOD        o95loc
055700140404     C                   endif
055800140404     C*
055900000000     C                   ENDSR
056000140404     C*------------------------------------------------------------------------*
056100140404     C* Stampa - statistica
056200140404     C*------------------------------------------------------------------------*
056300140404     c     STAMPA        BEGSR
056400140404     c* Tpo servizio
056500140404     c                   if        partser='C'
056600140404     c                   eval      p1dtser='CONSEGNE'
056700140404     C                   ELSE
056800140408     c                   eval      p1dtser=' RITIRI '
056900140404     C                   ENDIF
057000140408     c* giro le date
057100140409     C                   MOVE      pardsd        G02inv
057200140408     C                   MOVEL     '3'           G02ERR
057300140408     C                   CALL      'XSRDA8'
057400140408     C                   PARM                    WLBDAT
057500140408     c                   z-add     g02dat        p1ddsd
057600140408     c*
057700140408     c                   if        pardsa>0
057800140409     C                   MOVE      pardsa        G02inv
057900140408     C                   MOVEL     '3'           G02ERR
058000140408     C                   CALL      'XSRDA8'
058100140408     C                   PARM                    WLBDAT
058200140408     c                   z-add     g02dat        p1ddsa
058300140408     c                   endif
058400140408     c* Orari per calcolo %
058500140408     c                   if        parorari='S'
058600140408     c                   eval      p1dorari='STANDARD'
058700140424     c                   eval      p1dtor_d='Standard'
058800140408     c                   else
058900140408     c                   eval      p1dorari='MINIMO/MASSIMO'
059000140424     c                   eval      p1dtor_d=' Min/Max'
059100140408     c                   endif
059200140408     c
059201140514     c                   if        partser='C'
059300140408     c                   eval      p1dorari=%trim(p1dorari)+' per spedizioni'
059400140408     c                   if        parcor='C'
059500140408     c                   eval      p1dorari=%trim(p1dorari)+' CORRETTE'
059600140411     c                   eval      p1dbolstd='   Nei limiti STD   '
059700140411     c                   eval      p1dbolmin='Nei limiti MIN/MAX  '
059800140408     C                   ELSE
059900140409     c                   eval      p1dorari=%trim(p1dorari)+' FUORI ORARIO'
060000140411     c                   eval      p1dbolstd='  Fuori limiti STD  '
060100140411     c                   eval      p1dbolmin='Fuori limiti MIN/MAX'
060200140408     C                   ENDIF
060201140514     c                   else
060202140514     c                   eval      p1dbolstd='  Fuori limiti STD  '
060203140514     c                   endif
060300140423     c
060301140514     c                   if        partser='C'
060400140423     c                   write     STO4TE
060401140514     c                   else
060402140514     c                   write     STO4TE2
060403140514     c                   endif
060500140423
060600140423     c* Orari nel dettaglio
060700140423     c                   if        pardet='S'
060800140423     c                   exsr      TestaDET
060801140514     c                   if        partser='C'
060900140423     c                   write     STO4TE_d
060901140514     c                   else
060902140514     c                   write     STO4TE_d3
060903140514     c                   endif
061000140423     c                   endif
061100140404     c
061200140404     C
061300140424     c*
061400140514     c     partser       setll     wfccos1l
061500140514     c     partser       reade     wfccos1l
061600140404     c                   dow       not %eof(wfccos1l)
061700140404     c
061800140408     c* a cambio filiale stampo a pagina nuova
061900140408     c                   if        wcoslna<>savlna
062000140408
062100140408     c                   if        savlna>0
062200140423     c                   exsr      Totfil
062300140423     c
062301140514     c                   if        partser='C'
062400140408     c                   write     STO4TE
062401140514     c                   else
062402140514     c                   write     STO4TE2
062403140514     c                   endif
062500140422     c
062600140422     c                   if        pardet='S'
062601140514     c                   if        partser='C'
062700140422     c                   write     STO4RI_d
062800140422     c                   write     STO4TE_d
062801140514     c                   else
062802140514     c                   write     STO4RI_d3
062803140514     c                   write     STO4TE_d3
062804140514     c                   endif
062900140408     c                   endif
063000140423     c
063100140423     c                   clear                   tot_ts
063200140423     c                   clear                   tot_tse
063300140423     c                   clear                   tot_tsos
063400140423     c                   clear                   tot_tsom
063500140422     c                   endif
063600140408
063700140408     c                   z-add     1             i
063800140408     C     wcoslna       lookup    fil(i)                                 55
063900140404     C                   if        %found
064000140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' '+dfil(i)
064100140408     c                   eval      p1dare=dare(i)
064200140404     C                   else
064300140409     c                   eval      p1dfil=%editc(wcoslna:'X')+' XXXXXXXXXXXXXXX'
064400140408     c                   eval      p1dare=*all'?'
064500140404     C                   endif
064600140408
064700140408     c                   eval      savlna=wcoslna
064800140408
064801140514     c                   if        partser='C'
064900140408     c                   write     STO4TEF
065000140414     c                   write     STO4RI
065001140514     c                   else
065002140514     c                   write     STO4TEF2
065003140514     c                   write     STO4RI2
065004140514     c                   endif
065100140422     c                   if        pardet='S'
065101140514     c                   if        partser='C'
065200140422     c                   write     STO4TEF_d
065300140422     c                   write     STO4RI_d
065301140514     c                   else
065302140514     c                   write     STO4TEF_d3
065303140514     c                   write     STO4RI_d3
065304140514     c                   endif
065400140408     c                   endif
065500140422     c                   endif
065600140408
065700140408     c* Stampo solo le località con minimo spe elaborate >= alla richiesta
065800140411     c                   if        wcostse>=parnum and wcostse>0
065900140408     c* voglio la % di correttezza
066000140408     c                   if        parcor='C'
066100140404     c* Calcolo % STD
066200140408     c                   eval(H)   p1dperc_os=(100*WCOSTSOS)/WCOSTSE
066300140408     c                   eval      p1dstsos=WCOSTSOs
066400140404     c* Calcolo % MIN/MAX
066500140408     c                   eval(H)   p1dperc_om=(100*WCOSTSOm)/WCOSTSE
066600140408     c                   eval      p1dstsom=WCOSTSOm
066700140408     c
066800140408     c                   else
066900140408     c* voglio la % delle fuori orario
067000140408     c                   eval      wfuori_os=wcostse-wcostsos
067100140408     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/WCOSTSE
067200140408     c                   eval      p1dstsos=Wfuori_os
067300140408
067400140408     c                   eval      wfuori_om=wcostse-wcostsom
067500140408     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/WCOSTSE
067600140408     c                   eval      p1dstsom=Wfuori_om
067700140408     c                   endif
067800140408     c
067900140407     c* stampo solo al di sotto della % richiesta
068000140408     c                   eval      wstampadet='S'
068100140408
068200140408     c                   select
068300140408     c                   when      parperc=0
068400140408
068500140408     c                   when      parorari='S'  and parcor='C' and
068600140408     c                             p1dperc_os>=parperc
068700140408     c                   eval      wstampadet='N'
068800140408     c
068900140408     c                   when      parorari='S'  and parcor='F' and
069000140408     c                             p1dperc_os< parperc
069100140408     c                   eval      wstampadet='N'
069200140408
069300140408     c                   when      parorari='M'  and parcor='C' and
069400140408     c                             p1dperc_om>=parperc
069500140408     c                   eval      wstampadet='N'
069600140408     c
069700140408     c                   when      parorari='M'  and parcor='F' and
069800140408     c                             p1dperc_om< parperc
069900140408     c                   eval      wstampadet='N'
070000140408     c                   endsl
070100140404     c*
070200140408     c                   if        wstampadet='S'
070300140424     c* POM
070400140424     c                   if        wcosserv='P '
070500140424     c                   eval      p1dpom='Pom'
070600140424     c                   else
070700140424     c                   eval      p1dpom='   '
070800140424     c                   endif
070900140411     c*
071000140411     c* Se la località è vuota imposto '??'
071100140411     c                   if        wcosloc=*blanks
071200140411     c                   eval      wcosloc='??'
071300140411     c                   endif
071400140408     c
071500140408     c                   if        *in06
071600140408     c                   write     sto4te
071700140408     c                   write     STO4TEF
071800140414     c                   write     STO4RI
071900140408     c                   setoff                                       06
072000140404     c                   endif
072001140514
072002140514     c                   if        *in08
072003140514     c                   write     sto4te2
072004140514     c                   write     STO4TEF2
072005140514     c                   write     STO4RI2
072006140514     c                   setoff                                       08
072007140514     c                   endif
073900140408     c
074000140424     c                   eval      p1dstis=wcosoSTiS
074100140424     c                   eval      p1dstfs=wcosoSTFS
074200140424     c                   eval      p1dmiis=wcosomiis
074300140424     c                   eval      p1dmxfs=wcosomxfs
074400140404     c
074401140514     c                   if        partser='C'
074500140408     c                   write     STO4det
074501140514     c                   else
074502140514     c                   write     STO4det2
074503140514     c                   endif
074600140422     c
074700140422     c* se devo stampare anche il dettaglio
074800140422     c                   if        pardet='S'
074900140422     c                   exsr      Stadet
075000140422     c                   endif
075100140408     c
075200140407     c                   endif
075300140408     c                   endif
075400140424     c* totali di filiale
075500140424     c                   add       WCOSTS        tot_ts
075600140424     c                   add       WCOSTSE       tot_tse
075700140424     c                   add       WCOSTSos      tot_tsos
075800140424     c                   add       WCOSTSom      tot_tsom
075900140407     c
076000140514     c     partser       reade     wfccos1l
076100140404     c
076200140404     c                   enddo
076300140423     c
076400140423     c* totali finali
076500140423     c                   if        savlna>0
076600140423     c                   exsr      totfil
076700140423     c                   endif
076800140404     c
076900140404     c                   ENDSR
077000140423     C*------------------------------------------------------------------------*
077100140423     c* Imposta gli orari in testata dettaglioù
077200140423     C*------------------------------------------------------------------------*
077300140423     c     TestaDet      BEGSR
077400140423     c                   eval      p1dsot = paror1
077500140423     c                   eval      p1doltre= paroltre
077600140423     c                   eval      p1dda1  = or_dal(1)
077700140423
077800140423     c                   if        or_dal(2)>0
077900140423     c                   eval      p1dda2  = or_dal(2)
078000140423     c                   movel     or_dal(2)     w0060
078100140423     c                   move      w0060         timeiso
078200140423     c                   subdur    1:*mn         timeiso
078300140423     c                   eval      w0060     =%dec(timeiso)
078400140423     c                   movel     w0060         p1dal1
078500140423     c                   else
078600140423     c                   eval      p1dal1=paroltre
078700140423     c                   leavesr
078800140423     c                   endif
078900140423     c
079000140423     c                   if        or_dal(3)>0
079100140423     c                   eval      p1dda3  = or_dal(3)
079200140423     c                   movel     or_dal(3)     w0060
079300140423     c                   move      w0060         timeiso
079400140423     c                   subdur    1:*mn         timeiso
079500140423     c                   eval      w0060     =%dec(timeiso)
079600140423     c                   movel     w0060         p1dal2
079700140423     c                   else
079800140423     c                   eval      p1dal2=paroltre
079900140423     c                   leavesr
080000140423     c                   endif
080100140423
080200140423     c                   if        or_dal(4)>0
080300140423     c                   eval      p1dda4  = or_dal(4)
080400140423     c                   movel     or_dal(4)     w0060
080500140423     c                   move      w0060         timeiso
080600140423     c                   subdur    1:*mn         timeiso
080700140423     c                   eval      w0060     =%dec(timeiso)
080800140423     c                   movel     w0060         p1dal3
080900140423     c                   else
081000140423     c                   eval      p1dal3=paroltre
081100140423     c                   leavesr
081200140423     c                   endif
081300140423
081400140423     c                   if        or_dal(5)>0
081500140423     c                   eval      p1dda5  = or_dal(5)
081600140423     c                   movel     or_dal(5)     w0060
081700140423     c                   move      w0060         timeiso
081800140423     c                   subdur    1:*mn         timeiso
081900140423     c                   eval      w0060     =%dec(timeiso)
082000140423     c                   movel     w0060         p1dal4
082100140423     c                   else
082200140423     c                   eval      p1dal4=paroltre
082300140423     c                   leavesr
082400140423     c                   endif
082500140423
082600140423     c                   if        or_dal(6)>0
082700140423     c                   eval      p1dda6  = or_dal(6)
082800140423     c                   movel     or_dal(6)     w0060
082900140423     c                   move      w0060         timeiso
083000140423     c                   subdur    1:*mn         timeiso
083100140423     c                   eval      w0060     =%dec(timeiso)
083200140423     c                   movel     w0060         p1dal5
083300140423     c                   else
083400140423     c                   eval      p1dal5=paroltre
083500140423     c                   leavesr
083600140423     c                   endif
083700140423
083800140423     c                   if        or_dal(7)>0
083900140423     c                   eval      p1dda7  = or_dal(7)
084000140423     c                   movel     or_dal(7)     w0060
084100140423     c                   move      w0060         timeiso
084200140423     c                   subdur    1:*mn         timeiso
084300140423     c                   eval      w0060     =%dec(timeiso)
084400140423     c                   movel     w0060         p1dal6
084500140423     c                   else
084600140423     c                   eval      p1dal6=paroltre
084700140423     c                   leavesr
084800140423     c                   endif
084900140423
085000140423     c                   if        or_dal(8)>0
085100140423     c                   eval      p1dda8  = or_dal(8)
085200140423     c                   movel     or_dal(8)     w0060
085300140423     c                   move      w0060         timeiso
085400140423     c                   subdur    1:*mn         timeiso
085500140423     c                   eval      w0060     =%dec(timeiso)
085600140423     c                   movel     w0060         p1dal7
085700140423     c                   else
085800140423     c                   eval      p1dal7=paroltre
085900140423     c                   leavesr
086000140423     c                   endif
086100140423     c                   if        or_dal(9)>0
086200140423     c                   eval      p1dda9  = or_dal(9)
086300140423     c                   movel     or_dal(9)     w0060
086400140423     c                   move      w0060         timeiso
086500140423     c                   subdur    1:*mn         timeiso
086600140423     c                   eval      w0060     =%dec(timeiso)
086700140423     c                   movel     w0060         p1dal8
086800140423     c
086900140423     c                   eval      p1dal9=paroltre
087000140423     c                   else
087100140423     c                   eval      p1dal8=paroltre
087200140423     c                   leavesr
087300140423     c                   endif
087400140423     c
087500140423     c                   ENDSR
087600140422     C*------------------------------------------------------------------------*
087700140422     C* Stampa dettaglio
087800140422     C*------------------------------------------------------------------------*
087900140422     C     StaDET        BEGSR
088000140422     c                   if        *in07
088100140422     c                   write     sto4te_d
088200140422     c                   write     STO4TEF_d
088300140422     c                   write     STO4RI_d
088400140422     c                   setoff                                       07
088500140422     c                   endif
088501140514
088502140514     c                   if        *in09
088503140514     c                   write     sto4te_d3
088504140514     c                   write     STO4TEF_d3
088505140514     c                   write     STO4RI_d3
088506140514     c                   setoff                                       09
088507140514     c                   endif
088600140424     c* Imposto gli orari
088700140424     c                   if        parorari='S'
088800140424     c                   eval      p1dor_dal=WCOSOSTIS
088900140424     c                   eval      p1dor_al =WCOSOSTfS
089000140424     c                   else
089100140424     c                   eval      p1dor_dal=WCOSOmiis
089200140424     c                   eval      p1dor_al =WCOSOmxfS
089300140424     c                   endif
089400140424     c                   setoff                                       111213
089500140424     c                   setoff                                       141516
089600140424     c                   setoff                                       171819
089700140505     c                   setoff                                       2021
089800140422     c* calcolo le %
089900140422     c* SOTTO
090000140422     c                   eval(H)   wperc  =(100*WCOSMINT)/WCOSTSE
090100140422     C                   IF        WPERC>0
090200140424     C                   EVAL      P1CP_SOT=%editc(wperc:'Z')+' '
090300140505     c                   seton                                        20
090400140422     c                   else
090500140424     c                   eval      p1cp_sot='    '
090600140422     c                   endif
090700140422     c* OLTRE
090800140422     c                   eval(H)   wperc  =(100*WCOSMAXT)/WCOSTSE
090900140422     C                   IF        WPERC>0
091000140424     C                   EVAL      P1CP_olt=%editc(wperc:'Z')+' '
091100140505     c                   seton                                        21
091200140422     c                   else
091300140424     c                   eval      p1cp_olt='    '
091400140422     c                   endif
091500140422     c* % 1
091600140505    1c                   if        wcosor1  >0
091700140422     c                   eval(H)   wperc  =(100*WCOSor1T)/WCOSTSE
091800140422     c
091900140505    2C                   IF        WPERC>0
092000140424     C                   EVAL      P1CP_o1 =%editc(wperc:'Z')+' '
092100140505     c
092200140505    2c                   select
092300140505     c                   when      or_dal(1)<p1dor_dal
092400140505     c                   seton                                        11
092500140505     c                   when      or_dal(2)>0 and or_dal(2)>p1dor_al
092600140505     c                   seton                                        11
092700140505     c                   when      or_dal(2)=0 and paroltre >p1dor_al
092800140505     c                   seton                                        11
092900140505    2c                   endsl
093000140505
093100140422     c                   else
093200140424     c                   eval      p1cp_o1 ='    '
093300140505    2c                   endif
093400140424     c
093500140505    1c                   endif
093600140422     c
093700140422     c* % 2
093800140422     c                   if        wcosor2  >0
093900140422     c                   eval(H)   wperc  =(100*WCOSor2T)/WCOSTSE
094000140422     c
094100140422     C                   IF        WPERC>0
094200140424     C                   EVAL      P1CP_o2 =%editc(wperc:'Z')+' '
094300140505
094400140505     c                   select
094500140505     c                   when      or_dal(2)<p1dor_dal
094600140505     c                   seton                                        12
094700140505     c                   when      or_dal(3)>0 and or_dal(3)>p1dor_al
094800140505     c                   seton                                        12
094900140505     c                   when      or_dal(3)=0 and paroltre >p1dor_al
095000140505     c                   seton                                        12
095100140505     c                   endsl
095200140505
095300140422     c                   else
095400140424     c                   eval      p1cp_o2 ='    '
095500140422     c                   endif
095600140422     c                   endif
095700140423     c* % 3
095800140423     c                   if        wcosor3  >0
095900140423     c                   eval(H)   wperc  =(100*WCOSor3T)/WCOSTSE
096000140423     c
096100140423     C                   IF        WPERC>0
096200140424     C                   EVAL      P1CP_o3 =%editc(wperc:'Z')+' '
096300140505
096400140505     c                   select
096500140505     c                   when      or_dal(3)<p1dor_dal
096600140505     c                   seton                                        13
096700140505     c                   when      or_dal(4)>0 and or_dal(4)>p1dor_al
096800140505     c                   seton                                        13
096900140505     c                   when      or_dal(4)=0 and paroltre >p1dor_al
097000140505     c                   seton                                        13
097100140505     c                   endsl
097200140423     c                   else
097300140424     c                   eval      p1cp_o3 ='    '
097400140423     c                   endif
097500140423     c                   endif
097600140423     c* % 4
097700140423     c                   if        wcosor4  >0
097800140423     c                   eval(H)   wperc  =(100*WCOSor4T)/WCOSTSE
097900140423     c
098000140423     C                   IF        WPERC>0
098100140424     C                   EVAL      P1CP_o4 =%editc(wperc:'Z')+' '
098200140505
098300140505     c                   select
098400140505     c                   when      or_dal(4)<p1dor_dal
098500140505     c                   seton                                        14
098600140505     c                   when      or_dal(5)>0 and or_dal(5)>p1dor_al
098700140505     c                   seton                                        14
098800140505     c                   when      or_dal(5)=0 and paroltre >p1dor_al
098900140505     c                   seton                                        14
099000140505     c                   endsl
099100140423     c                   else
099200140424     c                   eval      p1cp_o4 ='    '
099300140423     c                   endif
099400140423     c                   endif
099500140423     c* % 5
099600140423     c                   if        wcosor5  >0
099700140423     c                   eval(H)   wperc  =(100*WCOSor5T)/WCOSTSE
099800140423     c
099900140423     C                   IF        WPERC>0
100000140424     C                   EVAL      P1CP_o5 =%editc(wperc:'Z')+' '
100100140505
100200140505     c                   select
100300140505     c                   when      or_dal(5)<p1dor_dal
100400140505     c                   seton                                        15
100500140505     c                   when      or_dal(6)>0 and or_dal(6)>p1dor_al
100600140505     c                   seton                                        15
100700140505     c                   when      or_dal(6)=0 and paroltre >p1dor_al
100800140505     c                   seton                                        15
100900140505     c                   endsl
101000140423     c                   else
101100140424     c                   eval      p1cp_o5 ='    '
101200140423     c                   endif
101300140423     c                   endif
101400140423     c* % 6
101500140423     c                   if        wcosor6  >0
101600140423     c                   eval(H)   wperc  =(100*WCOSor6T)/WCOSTSE
101700140423     c
101800140423     C                   IF        WPERC>0
101900140424     C                   EVAL      P1CP_o6 =%editc(wperc:'Z')+' '
102000140505
102100140505     c                   select
102200140505     c                   when      or_dal(6)<p1dor_dal
102300140505     c                   seton                                        16
102400140505     c                   when      or_dal(7)>0 and or_dal(7)>p1dor_al
102500140505     c                   seton                                        16
102600140505     c                   when      or_dal(7)=0 and paroltre >p1dor_al
102700140505     c                   seton                                        16
102800140505     c                   endsl
102900140423     c                   else
103000140424     c                   eval      p1cp_o6 ='    '
103100140423     c                   endif
103200140423     c                   endif
103300140423     c* % 7
103400140423     c                   if        wcosor7  >0
103500140423     c                   eval(H)   wperc  =(100*WCOSor7T)/WCOSTSE
103600140423     c
103700140423     C                   IF        WPERC>0
103800140424     C                   EVAL      P1CP_o7 =%editc(wperc:'Z')+' '
103900140505
104000140505     c                   select
104100140505     c                   when      or_dal(7)<p1dor_dal
104200140505     c                   seton                                        17
104300140505     c                   when      or_dal(8)>0 and or_dal(8)>p1dor_al
104400140505     c                   seton                                        17
104500140505     c                   when      or_dal(8)=0 and paroltre >p1dor_al
104600140505     c                   seton                                        17
104700140505     c                   endsl
104800140423     c                   else
104900140424     c                   eval      p1cp_o7 ='    '
105000140423     c                   endif
105100140423     c                   endif
105200140423     c* % 8
105300140423     c                   if        wcosor8  >0
105400140423     c                   eval(H)   wperc  =(100*WCOSor8T)/WCOSTSE
105500140423     c
105600140423     C                   IF        WPERC>0
105700140424     C                   EVAL      P1CP_o8 =%editc(wperc:'Z')+' '
105800140505
105900140505     c                   select
106000140505     c                   when      or_dal(8)<p1dor_dal
106100140505     c                   seton                                        18
106200140505     c                   when      or_dal(9)>0 and or_dal(9)>p1dor_al
106300140505     c                   seton                                        18
106400140505     c                   when      or_dal(9)=0 and paroltre >p1dor_al
106500140505     c                   seton                                        18
106600140505     c                   endsl
106700140423     c                   else
106800140424     c                   eval      p1cp_o8 ='    '
106900140423     c                   endif
107000140423     c                   endif
107100140423     c* % 9
107200140423     c                   if        wcosor9  >0
107300140423     c                   eval(H)   wperc  =(100*WCOSor9T)/WCOSTSE
107400140423     c
107500140423     C                   IF        WPERC>0
107600140424     C                   EVAL      P1CP_o9 =%editc(wperc:'Z')+' '
107700140505
107800140505     c                   select
107900140505     c                   when      or_dal(9)<p1dor_dal
108000140505     c                   seton                                        19
108100140505     c                   when      paroltre >p1dor_al
108200140505     c                   seton                                        19
108300140505     c                   endsl
108400140423     c                   else
108500140424     c                   eval      p1cp_o9 ='    '
108600140423     c                   endif
108700140423     c                   endif
108800140422     c
108801140514     c                   if        partser='C'
108900140422     c                   write     STO4det_d
108901140514     c                   else
108902140514     c                   write     STO4det_d3
108903140514     c                   endif
109000140422     c                   ENDSR
109100140423     C*------------------------------------------------------------------------*
109200140423     c* Stampa totali di filiale
109300140423     C*------------------------------------------------------------------------*
109400140423     c     TOTfil        BEGSR
109500140423     c* voglio la % di correttezza
109600140423     c                   if        parcor='C'
109700140423     c* Calcolo % STD
109800140423     c                   eval(H)   p1dperc_os=(100*tot_TSOS)/tot_TSE
109900140423     c                   eval      p1dstsos=tot_TSOs
110000140423     c* Calcolo % MIN/MAX
110100140423     c                   eval(H)   p1dperc_om=(100*tot_TSOm)/tot_TSE
110200140423     c                   eval      p1dstsom=tot_TSOm
110300140423     c
110400140423     c                   else
110500140423     c* voglio la % delle fuori orario
110600140423     c                   eval      wfuori_os=tot_tse-tot_tsos
110700140423     c                   eval(H)   p1dperc_os=(100*wfuori_OS)/tot_TSE
110800140423     c                   eval      p1dstsos=Wfuori_os
110900140423
111000140423     c                   eval      wfuori_om=tot_tse-tot_tsom
111100140423     c                   eval(H)   p1dperc_om=(100*wfuori_Om)/tot_TSE
111200140423     c                   eval      p1dstsom=Wfuori_om
111300140423     c
111400140423     c                   endif
111500140423
111501140514     c                   if        partser='C'
111600140423     c                   if        *in06
111700140423     c                   write     sto4te
111800140423     c                   write     STO4TEF
111900140423     c                   write     STO4RI
112000140423     c                   setoff                                       06
112100140423     c                   else
112200140423     c                   write     STO4RI
112300140423     c                   write     STO4TOT
112400140423     c  n06              write     STO4RI
112500140423     c                   endif
112601140514     c                   else
112602140514     c                   if        *in08
112603140514     c                   write     sto4te2
112604140514     c                   write     STO4TEF2
112605140514     c                   write     STO4RI2
112606140514     c                   setoff                                       08
112608140514     c                   else
112609140514     c                   write     STO4RI2
112610140514     c                   write     STO4TOT2
112611140514     c  n08              write     STO4RI2
112612140514     c                   endif
112613140514     c                   endif
112700140423     c                   ENDSR
112800000000     C*------------------------------------------------------------------------*
112900000000     C* DIVARE - RECUPERA AREA E DIVISIONE
113000000000     C*------------------------------------------------------------------------*
113100000000     C     DIVARE        BEGSR
113200000000     C*
113300000000     C                   Z-ADD     1             I
113400051109     C     taslnp        LOOKUP    FIL(I)                                 99
113500000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
113600000000     C                   MOVEL     '*'           WDIV              1
113700000000     C                   Z-ADD     999           WARE              3 0
113800000000     C                   ELSE                                                   <-- 3 -->
113900000000     C                   MOVEL     DIV(I)        WDIV
114000000000     C                   Z-ADD     ARE(I)        WARE
114100000000     C                   END                                                    <-- 3 ---
114200000000     C*
114300000000     C                   ENDSR
114400000000     C*------------------------------------------------------------------------*
114500000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
114600000000     C*------------------------------------------------------------------------*
114700000000     C     CARORG        BEGSR
114800000000     C*
114900140404     C     *loval        SETLL     AZORG01L                           99        *FINE-ARCHIV
115000000000     C  N99              READ      AZORG01L                               99
115100000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
115200000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
115300000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
115400000000     C     ORGFAG        IFNE      'V'
115500051109     C                   ADD       1             I
115600000000     C                   Z-ADD     ORGFIL        FIL(I)
115700000000     C                   Z-ADD     ORGCAR        ARE(I)
115800000000     C                   MOVEL     ORGFL3        DIV(I)
115900140404     C* CALCOLA NETWORK
116000140404     C                   MOVEL     ORGDE3        OG143
116100140404     C                   MOVEL     §OGNTW        NTW(I)                         *NETWORK
116200051109     c* riempio schiere decodifiche p.o., distretto e area
116300051109     c* distretto
116400140408     C***                Z-ADD     CODUT         KTBUTE
116500140408     C***                MOVEL     '17'          Ktbcod
116600140408     C***                MOVEL(P)  ORGFL3        KKEY
116700140408     C***  KTAB          CHAIN     TABEL00F                           30
116800140408    2C***  *IN30         IFEQ      *OFF
116900140408     C***  TBLFLG        ANDEQ     *BLANKS
117000140408     C***                MOVEL     TBLUNI        DS17
117100140408     C***                MOVEL     §17DES        ddiv(I)
117200140408     C***                ELSE
117300140408     C***                MOVE      *BLANKS       ddiv(I)
117400140409    2C***                END
117500051109     c* area
117600051109     C                   Z-ADD     CODUT         KTBUTE
117700051109     C                   MOVEL     '05'          Ktbcod
117800051109     C                   MOVEL(P)  ORGCAR        KKEY
117900051109     C     KTAB          CHAIN     TABEL00F                           30
118000051109    2C     *IN30         IFEQ      *OFF
118100051109     C     TBLFLG        ANDEQ     *BLANKS
118200051109     C                   MOVEL     TBLUNI        DS05
118300140408     C***                MOVEL     §05DES        dare(I)
118400140409     c                   eval      dare(I)=%editc(orgcar:'X')+ ' '+§05DES
118500051109     C                   ELSE
118600140409     c                   eval      dare(I)=%editc(orgcar:'X')+' ???????????????'
118700140408     C***                MOVE      *BLANKS       dare(I)
118800051109    2C                   END
118900051109     c* p.o.
119000051109     C                   MOVEl     orgdes        dfil(I)
119100051109     c
119200000000     C                   END
119300000000     C                   END                                                    <-- 2 ---
119400000000     C                   READ      AZORG01L                               99
119500000000     C                   END                                                    <-- 1 ---
119600000000     C*
119700000000     C                   ENDSR
119800000000     C*------------------------------------------------------------------------*
119900051109     C* CARTBL - CARICA I TIPI BOLLA di recupero
120000000000     C*------------------------------------------------------------------------*
120100000000     C     CARTBL        BEGSR
120200000000     C*--------------------
120300051109     C* TIPI BOLLA di recupero
120400000000     C*--------------------
120500000000     C                   Z-ADD     0             I
120600000000     C                   Z-ADD     CODUT         KTBUTE
120700000000     C                   MOVEL     'TB'          KTBCOD
120800000000     C     KEYTAB        SETLL     TABEL00F
120900051109     C                   READe     TABEL00F                               99
121000000000     C     *IN99         DOWEQ     *OFF
121100000000     C     TBLFLG        IFEQ      *BLANKS
121200051109     C                   MOVEL     TBLUNI        DSTB
121300051109     c                   if        §tbrbl = 'R'
121400051109     C                   ADD       1             I
121500051109     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
121600051109     c                   endif
121700000000     C                   END
121800051109     C     keytab        READe     TABEL00F                               99
121900000000     C                   END
122000000000     C*
122100140428     c* recupero versione cappario alla data DAL
122200140428     C                   Z-ADD     pardsd        P96DRI            8 0
122300140428     C                   Z-ADD     *ZEROS        P96VER            5 0
122400140428     C                   CALL      'TISI96R'
122500140428     C                   PARM                    P96DRI
122600140428     C                   PARM                    P96VER
122700140428     c* carico gli orari in schiera
122800140428     c                   z-add     1             yy
122900140428     c                   if        paror1 >0
123000140428     c                   eval      or_dal(yy)=paror1
123100140428     c                   add       1             yy
123200140428     c                   endif
123300140428     c                   if        paror2 >0
123400140428     c                   eval      or_dal(yy)=paror2
123500140428     c                   add       1             yy
123600140428     c                   endif
123700140428     c                   if        paror3 >0
123800140428     c                   eval      or_dal(yy)=paror3
123900140428     c                   add       1             yy
124000140428     c                   endif
124100140428     c                   if        paror4 >0
124200140428     c                   eval      or_dal(yy)=paror4
124300140428     c                   add       1             yy
124400140428     c                   endif
124500140428     c                   if        paror5 >0
124600140428     c                   eval      or_dal(yy)=paror5
124700140428     c                   add       1             yy
124800140428     c                   endif
124900140428     c                   if        paror6 >0
125000140428     c                   eval      or_dal(yy)=paror6
125100140428     c                   add       1             yy
125200140428     c                   endif
125300140428     c                   if        paror7 >0
125400140428     c                   eval      or_dal(yy)=paror7
125500140428     c                   add       1             yy
125600140428     c                   endif
125700140428     c                   if        paror8 >0
125800140428     c                   eval      or_dal(yy)=paror8
125900140428     c                   add       1             yy
126000140428     c                   endif
126100140428     c                   if        paror9 >0
126200140428     c                   eval      or_dal(yy)=paror9
126300140428     c                   add       1             yy
126400140428     c                   endif
126500140428     C*
126600000000     C                   ENDSR
131601140512     C*------------------------------------------------------------------------*
131602140512     C* Ciclo di lettura su FNORF
131603140512     C*------------------------------------------------------------------------*
131604140512     c     LET_ORF       BEGSR
131605140512     C                   MOVEL     '0'           FINL00            1
131618140512     c
131619140512     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
131620140512     C     FINL00        IFEQ      '0'                                          --- 1 -->
131621140512     C*
131622140512     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
131623140512     C                   EXSR      SETOrf
131624140512     C*
131625140512     C* CICLO FINO A FINE FILE
131626140512     C     FINL00        DOWEQ     '0'                                          --- 2 -->
131627140512     C*
131628140512     C* CREA IL RECORD SUI FILE DI LAVORO
131629140512     C                   EXSR      CRErecR
131630140512     C*
131631140512     C* LEGGE IL RECORD SUCCESSIVO
131632140512     C                   EXSR      LETOrf
131633140512     C                   ENDDO                                                  <-- 2 ---
131634140512     C*
131635140512     C                   ENDIF                                                  <-- 1 ---
131636140512     c                   ENDSR
131637140512     C*------------------------------------------------------------------------*
131638140514     C* SETorf - POSIZIONAMENTO SUL FILE
131639140512     C*------------------------------------------------------------------------*
131640140512     C     SETOrf        BEGSR
131641140512     C*
131642140512     C* POSIZIONAMENTO E PRIMA LETTURA
131643140512     C*
131644140512     C                   movel     pardsd        Kdae
131645140512     C*
131647140512     C     Korf          SETLL     FNORF06L                           99        *FINE FILE
131652140512     C*
131653140512     C     *IN99         IFEQ      '1'
131654140512     C                   MOVEL     '1'           FINL00
131655140512     C                   ELSE
131656140512     C                   EXSR      LETOrf
131657140512     C                   END
131658140512     C*
131659140512     C                   ENDSR
131660140512     C*------------------------------------------------------------------------*
131661140512     C* LETFIL - LETTURA PROSSIMO RECORD
131662140512     C*------------------------------------------------------------------------*
131663140512     C     LETOrf        BEGSR
131664140512     C*
131665140512     C* LEGGO FINO A:
131666140512     C*     - FINE FILE O SUPERATO LIMITE FINALE
131667140512     C*     - TROVATO RECORD VALIDO
131668140512     C                   MOVEL     'N'           WRECOK
131669140512    1C     FINL00        DOWEQ     '0'                                          --- 1 -->
131670140512     C     WRECOK        ANDEQ     'N'
131671140512     C*
131672140512     C*
131674140512     C     Korfp         READE     fnorf06l                               99
131678140512     c
131683140512    3C                   IF        *IN99  OR orfdae>PARDSA
131684140512     C                   MOVEL     '1'           FINL00
131689140514     c                   else
131691140512     c
131692140512     C                   EXSR      CHKREC_O                                     *CONTR.VALIDITA REC.
131693140512    2C                   END                                                    <-- 1 ---
131694140512     C*
131695140512    1C                   ENDDO                                                  <-- 1 ---
131696140512     C*
131697140512     C                   ENDSR
131698140512     C*------------------------------------------------------------------------*
131699140512     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD per Ritiri
131700140512     C*------------------------------------------------------------------------*
131701140512     C     CHKREC_O      BEGSR
131702140512     C*
131703140512     C                   MOVEL     'S'           WRECOK            1
131704140512     C*
131705140512     c* Escludo i ritiri contestuali alla consegna
131706140512     c                   if        orfnsr>0
131707140512     C                   MOVEL     'N'           WRECOK
131708140512     c                   endif
131709140512     c* Escludo gli ORM pronta merce: stabiliti col cliente fuori dagli orari std
131710140514     c     korm          chain     fnorm01l
131711140514     c                   if        %found(fnorm01l) and ormorr>0
131712140514     C****               MOVEL     'N'           WRECOK
131713140512     c                   endif
131723140512     C*
131724140512     C                   ENDSR
131725140512     C*------------------------------------------------------------------------*
131726140512     C* CRErec - CREA IL RECORD SUL FILE DI LAVORO  per i ritiri
131727140512     C*------------------------------------------------------------------------*
131728140512     C     CRErecR       BEGSR
131729140515
131730140515     c                   movel     orfore        wora
131731140512     c* recupero record azcos normalizzando prima la località
131732140512     c                   exsr      normlocR
131733140512     c                   clear                   kloc
131734140512     c
131735140512     c                   if        wnorm='S'
131736140512     c                   eval      kloc=o95loc
131737140512     c                   endif
131738140512     c
131739140512     c* Richiamo pgm reperimento orari servizi
131740140512     c                   clear                   trulorsds
131741140512     c                   clear                   trulor2ds
131744140512     c                   eval      ioredta=ormdar
131745140512     c                   eval      iorefil=orfpog
131746140512     c                   eval      iorenar=ormnar
131747140512     c                   eval      iorecap=ormcar
131748140512     c                   eval      ioreloc=o95loc
131749140512     c                   eval      ioretser=partser
131752140512     c                   eval      IOREgf2 = O95gf2
131753140514     c                   eval      ioretsp ='C'
131769140512     c                   call      'TRULORSR'
131770140512     c                   parm                    kpjba
131771140512     c                   parm                    trulorsds
131772140512     c                   parm                    trulor2ds
131773140512     c*
131779140512     c                   clear                   kserv
131780140514     c* se l'orario di ritiro  non rientra negli orari standard verifico se orario
131781140514     c* concordato col cliente ed in questo caso scarto il record
131783140515     c                   if        (wora  <OOR2STIS  and
131784140514     c                              ormcra>0 and
131785140514     c                              ormorr<oor2stis)     or
131786140515     c                             (wora  >OOR2STFS  and
131787140514     c                              ormcra>0 and
131788140514     c                              ormorr>oor2stfs)
131789140514     c                   leavesr
131790140514     c                   endif
131791140512
131792140512     c     kccosR        chain     wfccos1l
131793140512     c                   if        not %found(wfccos1l)
131794140512     c                   clear                   wfccos00
131795140512     c                   eval      wcosdcmd=pardsd
131796140512     c                   eval      wcosdcma=pardsa
131797140512     c                   eval      wcosver =p96ver
131798140512     c                   eval      wcoslna =kfil
131799140512     c                   eval      wcosnaz =ormnar
131800140512     c                   eval      wcoscap =ormcar
131801140513     c                   eval      wcosloc =kloc
131802140512     c                   eval      wcostser=partser
131803140512     c                   eval      wcosorif=OOR2ORIF
131804140512     c                   eval      wcosserv=kserv
131805140512     c* imposta campi cap/località obsoleto
131806140512     c                   exsr      sr_impobs
131818140512     c* Imposto gli orari nel file
131819140512     c                   eval      WCOSOSTIS=OOR2STIS
131820140512     c                   eval      WCOSOSTfS=OOR2STfS
131825140512     c
131826140512     c* imposto orari dettaglio se ci sono e richiesto
131827140512     c                   if        pardet='S'
131828140512     c                   exsr      sr_ImpOrDet
131829140512     c                   endif
131840140512     c                   endif
131841140512     c
131842140512     c                   add       1             wcosts
131843140512     c* Se loc non normalizzata
131844140512     c                   if        wnorm='N'
131845140512     c                   add       1             wcostsNN
131846140512     c                   endif
131847140512     c
131852140512     c*** serve in questo caso? E' uguale a wcosts....
131853140512     c                   add       1             wcostse
131854140512     c
131855140512     c* ritiro negli orari std
131856140515     c                   if        wora  >=OOR2STIS  and wora  <=OOR2STFS
131857140512     c                   add       1             wcostsos
131858140512     c                   endif
131859140512     c* imposto campi dettaglio
131861140513     c                   exsr      ImpoDETuni
131862140512     c
131863140512     c                   if        not %found(wfccos1l)
131864140512     c                   write     wfccos00
131865140512     c                   else
131866140512     c                   update    wfccos00
131867140512     c                   endif
131868140512     C*
131869140512     C                   ENDSR
131870140512     C*------------------------------------------------------------------------*
131871140512     C* NORMLOCR - Normalizza le località mediante il TISI95 - RITIRI
131872140512     C*------------------------------------------------------------------------*
131873140512     C     NORMLOCR      BEGSR
131874140512     c                   eval      wnorm='N'
131875140512     c
131884140512     C*
131885140512     C                   clear                   tisi95ds                       *azzera i parametri
131886140512     C                   movel     *blanks       i95tla                         *tipo lancio
131887140512     C                   movel     '7'           i95tcn                         *tipo controllo
131888140514     C                   movel     'C'           i95tsp                         *tipo servizio
131889140512     C                   movel     ormnar        i95nar                         *nazione dest
131890140512     C                   movel     ormcar        i95cap                         *cap dest
131891140512     C                   movel     ormlor        i95loc                         *località dest
131892140512     C                   movel     ormprr        i95prv                         *provincia dest
131898140512     C*
131899140513     C                   eval      i95dat = orfdae                              *data di riferimento
131910140512     C*
131911140512     C                   call      'TISI95R'
131912140512     C                   parm                    tisi95ds
131913140512     C*
131914140512     C* Se nn reperito instradamento considero i dati originali della bolla
131915140512     C                   if        o95lid = '3' or o95lid = '4'
131916140512     c                   eval      wnorm='S'
131917140512     C                   else
131918140514     C                   movel     ormlor        o95loc
131919140512     C                   endif
131920140512     C*
131921140512     C                   ENDSR
131922140513     C*------------------------------------------------------------------------*
131923140513     C* Imposto i campi di dettaglio in base ai range passati
131924140513     C*------------------------------------------------------------------------*
131925140513     C     ImpoDETUni    BEGSR
131926140513     c*
131927140513     c                   if        wora  <or_dal(1)
131928140513     c                   add       1             wcosmint
131929140513     c                   leavesr
131930140513     c                   endif
131931140513     c                   if        wora  > paroltre
131932140513     c                   add       1             wcosmaxt
131933140513     c                   leavesr
131934140513     c                   endif
131935140513
131936140513     c                   z-add     1             yy
131937140513     c                   dow       or_dal(yy)>0
131938140513     c                   if        wora  >=or_dal(yy) and
131939140513     c
131940140513     c                             (or_dal(yy+1)>0 and
131941140513     c                              wora  < or_dal(Yy+1)  or
131942140513     c                             (or_dal(yy+1)=0 and
131943140513     c                             wora  <=paroltre))
131944140513     c                   select
131945140513     c                   when      yy=1
131946140513     c                   add       1             wcosor1t
131947140513     c                   when      yy=2
131948140513     c                   add       1             wcosor2t
131949140513     c                   when      yy=3
131950140513     c                   add       1             wcosor3t
131951140513     c                   when      yy=4
131952140513     c                   add       1             wcosor4t
131953140513     c                   when      yy=5
131954140513     c                   add       1             wcosor5t
131955140513     c                   when      yy=6
131956140513     c                   add       1             wcosor6t
131957140513     c                   when      yy=7
131958140513     c                   add       1             wcosor7t
131959140513     c                   when      yy=8
131960140513     c                   add       1             wcosor8t
131961140513     c                   when      yy=9
131962140513     c                   add       1             wcosor9t
131963140513     c                   endsl
131964140513     c
131965140513     c                   leavesr
131966140513     c                   endif
131967140513     c                   add       1             yy
131968140513     c
131969140513     c                   enddo
131970140513     c
131971140513     c                   ENDSR
131972140512     C*------------------------------------------------------------------------*
131973140512     C* Imposta campi "obsloleto"
131974140512     C*------------------------------------------------------------------------*
131975140512     C     sr_impobs     BEGSR
131976140512     c* obsoleto
131977140512     c     kcpc          chain     azcpc01l
131978140512     c                   if        %found(azcpc01l)
131979140512     c                   eval      wcoscapo=%subst(cpcfi1:1:1)
131980140512     c                   endif
131981140512     c                   if        wcosloc<>*blanks
131982140512     c     kcpl          chain     azcpl01l
131983140512     c                   if        %found(azcpl01l)
131984140512     c                   eval      wcosloco=%subst(cplfi1:1:1)
131985140512     c                   endif
131986140512     c                   endif
131987140512     c                   endsr
131988140512     C*------------------------------------------------------------------------*
131989140512     C* Imposta campi orari dettaglio
131990140512     C*------------------------------------------------------------------------*
131991140512     C     sr_ImpOrDet   BEGSR
131992140512     c                   eval      wcosor1=or_dal(1)
131993140512     c                   eval      wcosor2=or_dal(2)
131994140512     c                   eval      wcosor3=or_dal(3)
131995140512     c                   eval      wcosor4=or_dal(4)
131996140512     c                   eval      wcosor5=or_dal(5)
131997140512     c                   eval      wcosor6=or_dal(6)
131998140512     c                   eval      wcosor7=or_dal(7)
131999140512     c                   eval      wcosor8=or_dal(8)
132000140512     c                   eval      wcosor9=or_dal(9)
132001140512     c                   eval      wcosorOl=paroltre
132003140512     c                   endsr
132004000000     C*------------------------------------------------------------------------*
132005000000     C* *INZSR - ROUTINE INIZIALE
132006000000     C*------------------------------------------------------------------------*
132007000000     C     *INZSR        BEGSR
132100000000     C*--------------------
132200000000     C* RICEVIMENTO PARAMETRI
132300000000     C*--------------------
132400140408     c
132500140408     c     *dtaara       define    §azute        azuteds
132600140408     c     *dtaara       define    §datiute      ddatiute
132700140408     c                   in(E)     *dtaara
132800140408     c                   If        %error  or RSUT = *blanks
132900140408     c                   CLEAR                   tibs34ds
133000140408     c                   CALL      'TIBS34R'
133100140408     c                   PARM                    tibs34ds
133200140408     c                   in        *dtaara
133300140408     c                   EndIf
133400140408     c
133500140408     c                   eval      p1drsut=rsut
133600140408     C*
133700000000     C*--------------------
133800000000     C* CAMPI RIFERITI AL DATA BASE
133900000000     C*--------------------
134000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
134100000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
134200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
134300051109     C     *LIKE         DEFINE    TBLkey        Kkey
134400000000     C*--------------------
134500000000     C* CHIAVI DI LETTURA
134600000000     C*--------------------
134700140404     C* CHIAVE LETTURA TITAS26C - COMPLETA
134800140404     C     Ktas          KLIST
134900140404     C                   KFLD                    Kdcm
135000140404     C                   KFLD                    Kfil
135001140512     C* CHIAVE LETTURA FNORF06L - COMPLETA
135002140512     C     Korf          KLIST
135004140512     C                   KFLD                    Kfil
135005140512     C                   KFLD                    Kfar
135006140512     C                   KFLD                    Kdae
135007140514     C*
135008140514     C     Korfp         KLIST
135009140514     C                   KFLD                    Kfil
135010140514     C                   KFLD                    Kfar
135011140514     C     Korm          KLIST
135012140514     C                   KFLD                    orfpoe
135013140514     C                   KFLD                    orfnsr
135014140514     C                   KFLD                    orfnor
135015140514     C                   KFLD                    orfnrv
135100000000     C*
135200000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
135300000000     C     KEYTAB        KLIST
135400000000     C                   KFLD                    KTBUTE
135500000000     C                   KFLD                    KTBCOD
135600051109     C     KTAB          KLIST
135700051109     C                   KFLD                    ktbute
135800051109     C                   KFLD                    ktbcod
135900051109     C                   KFLD                    KKEY
136000000000     C*
136100060524     C     Kar5          KLIST
136200060524     C                   KFLD                    tasAAS
136300060524     C                   KFLD                    taslnp
136400060524     C                   KFLD                    tasnrs
136500060524     C                   KFLD                    tasnsp
136600060524     c                   kfld                    kar5trd
136700140404     C*
136800140404     C     Kdst          KLIST
136900140404     C                   KFLD                    Knpg
137000140404     C                   KFLD                    knFV
137100140404     C                   KFLD                    Kfgs
137200140404     c
137300140415     C     KCCOS         KLIST
137400140415     C                   KFLD                    partser
137500140423     C                   KFLD                    d55tfa
137600140404     C                   KFLD                    tasnzd
137700140404     C                   KFLD                    tascad
137800140404     C                   KFLD                    Kloc
137900140415     C                   KFLD                    kserv
137901140514     C     KCCOSr        KLIST
137902140514     C                   KFLD                    partser
137903140514     C                   KFLD                    kfil
137904140514     C                   KFLD                    ormnar
137905140514     C                   KFLD                    ormcar
137906140514     C                   KFLD                    Kloc
137907140514     C                   KFLD                    kserv
138000140424     C     Kcpc          KLIST
138100140424     C                   KFLD                    p96ver
138200140424     C                   KFLD                    wcosnaz
138300140424     C                   KFLD                    wcoscap
138400140424     C     Kcpl          KLIST
138500140424     C                   KFLD                    p96ver
138600140424     C                   KFLD                    wcosnaz
138700140424     C                   KFLD                    wcoscap
138800140424     C                   KFLD                    wcosloc
138900000000     C*--------------------
139000000000     C                   Z-ADD     1             CODUT             1 0
139100000000     C*
139200000000     C***
139300000000     C* CARICA ORGANIGRAMMA
139400000000     C                   EXSR      CARORG
139500000000     C*
139600140404     C
139700140404     c* apertura file distinte di filiale
139701140514     c                   if        partser='C'
139800140508     C                   OPEN(e)   FIDST01L
139900140404     c
140000140508     c                   if        not %open(FIDST01L)
140100140508     c                   eval      %subst(WFDST:7:4)='PRD '
140200140508     c                   eval      %subst(TITAS:7:4)='GRPS'
140300140508     c                   eval      %subst(FIAR5:7:4)='GRPS'
140400140508     C                   OPEN      FIDST01L
140500140508     c                   endif
140600140409
140700140409     C                   OPEN(e)   TITAS26C
140800140409     C                   OPEN(e)   FIAR531C
140801140514     c                   open      tisto4p
140802140514     c                   open      tisto4p1
140803140514     c                   else
140804140514     c                   open      fnorf06l
140805140514     c                   open      fnorm01l
140806140514     c                   open      tisto4p2
140807140514     c                   open      tisto4p3
140808140514     c                   endif
140900140409     C
141000000000     C                   ENDSR
