000100941109     H*------------------------------------------------------------------------*
000200081210     H* Ritorno dati a cliente  - dettaglio c.A. da tabella 3K                 *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600070124     ftabel00f  if   e           k disk
000700060911     fTNTBE01L  IF   E           K DISK
000800080926     ffndct24l  IF   E           K DISK
000900080926     ffndck01l  IF   E           K DISK
001000080926     ffndfa01l  IF   E           K DISK
001100080926     ffndcf01l  IF   E           K DISK
001200080926     ffndcl01l  IF   E           K DISK
001300080926     ffnblp01l  IF   E           K DISK
001400080926     f                                     prefix(ARB:3)
001500080926     ffnarb01l  IF   E           K DISK
001600080926     ffiar601l  IF   E           K DISK
001700060829     FTIVGD00F  o    E             DISK
001800070124     d
001900080926     D clica           s              7  0 dim(5000)
002000080926     D cliun           S              7    DIM(5000)
002100080926     D cliela          S              1    DIM(5000)
002200060828     d
002300060828     D KPJBA         E DS
002400070129     D tibs02ds      E DS
002500070305     D DVGDFLO       E DS
002600080926     D Dcanom_I      E DS
002700080926     D DDCT01        E DS
002800080926     d
002900080926     D Ds3K          E DS
003000080926     D Ds3A          E DS
003100080926     D DTAD          E DS
003200080926     D DCCH          E DS
003300080926     d
003400080926     D                 DS
003500080926     D  arbaas
003600080926     D  arbmgs
003700080926     D  arbdsp                 1      8  0
003800060828      *
003900060829     d Dataiso         s               d   datfmt(*iso)
004000060829     d Dataeur         s               d   datfmt(*eur)
004100070124     d ktbkut          s                   like(tblkut) inz(1)
004200080926     D Kcod            S                   LIKE(tblcod)
004300080926     D Kkey            S                   LIKE(tblkey)
004400080926
004500080926     d Indx            s              3  0
004600080926     d wporto          s              1
004700080926     d wbol            s              1
004800080926     d spedata         s              8  0
004900080926     d speccm          s                   like(arbksc)
005000080926     d speccd          s                   like(arbksc)
005100080926     d spersd          s                   like(arbrsd)
005200080926     d spermn          s                   like(arbrmn)
005300080926     d sperma          s                   like(arbrma)
005400080926     d wBksc           s                   like(arbksc)
005500080926     D KTRC            S                   LIKE(ar6trc)
005600941109     C*------------------------------------------------------------------------*
005700060424     c     *entry        plist
005800060424     c                   parm                    kpjba
005900080926     c
006000080926     c     *loval        setll     fndct24l
006100080926     c                   read      fndct24l
006200080926     c
006300080926    1c                   dow       not %eof(fndct24l)
006400080926     c* Escludo le c.a. annullate o con fase < 100
006500090109     c* elaboro solo le c.a. dal 2008 in poi
006600090109    2c                   if        dctatb=' ' and dctfca>=100  and
006700090109     c                             dctaac>=2008
006800090109     c                   clear                   dcffca
006900081001     c* Se la c.a. + chiusa, verifico se c'e' stata almeno la fase 100
007000081001    3c                   if        dctfca=900
007100081001     c     kdct2         setgt     fndcf01l
007200081001     c     kdct2         readpe    fndcf01l
007300081001    4c                   dow       not  %eof(fndcf01l)
007400081001    5c                   if        dcffca<900
007500081001     c                   leave
007600081001     c                   else
007700081001     c     kdct2         readpe    fndcf01l
007800081001    5c                   endif
007900081001    4c                   enddo
008000081001    3c                   endif
008100081001     c
008200081001   3ac                   if        (dctfca>=100 and dctfca<900) or dcffca>=100
008300080926
008400080926     c                   movel     dctflo        ddct01
008500080926     c
008600080926     c* Verifico se il beneficiario  è tra quelli che prevede la
008700080926     c*  trasmissione
008800080926     c                   exsr      BENEFIC
008900080926     c
009000080926    3c                   if        wBksc>0
009100080926     c                   eval      Indx=%lookup(wbksc:clica)
009200080926    4c                   if        Indx>0
009300080926     c* Scrivo record di TIVGD
009400080926     c                   exsr      ELABORA
009500080926    4c                   endif
009600080926    3c                   endif
009700081001   3ac                   endif
009800080926    2c                   endif
009900080926     c
010000080926     c                   read      fndct24l
010100080926    1c                   enddo
010200070919     c
010300000000     C                   SETON                                        LR
010400060828     c*------------------------------------------------------------------
010500060828     c     *INZSR        BEGSR
010600080926     c
010700080926     C     kdct2         KLIST                                                  *
010800080926     C                   KFLD                    dctaac                         *
010900080926     C                   KFLD                    dctfil                         *
011000080926     C                   KFLD                    dctnca                         *
011100080926     C     kbolla        KLIST                                                  *
011200080926     C                   KFLD                    dctaas                         *
011300080926     C                   KFLD                    dctlnp                         *
011400080926     C                   KFLD                    dctnrs                         *
011500080926     C                   KFLD                    dctnsp                         *
011600080926     C     kbolladck     KLIST                                                  *
011700080926     C                   KFLD                    dckaas                         *
011800080926     C                   KFLD                    dcklnp                         *
011900080926     C                   KFLD                    dcknrs                         *
012000080926     C                   KFLD                    dcknsp                         *
012100080926     c     kBOLtrc       klist
012200080926     c                   kfld                    DCTaas
012300080926     c                   kfld                    DCTlnp
012400080926     c                   kfld                    DCTnrs
012500080926     c                   kfld                    DCTnsp
012600080926     c                   kfld                    Ktrc
012700070124     C     KE2TAB        KLIST
012800070124     C                   KFLD                    KTBKUT
012900080926     C                   KFLD                    KCOD
013000080926     C     ktab          KLIST                                                  *
013100080926     C                   KFLD                    KTBKUT
013200080926     C                   KFLD                    kcod                           *
013300080926     C                   KFLD                    kkey                           *
013400060829     c* Data di elaborazione
013500060829     c                   time                    w0140            14 0
013600060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
013700060829     c     *eur          movel     w0080         dataeur
013800060829     c                   movel     dataeur       dataiso
013900060829     c     *iso          movel     dataiso       datcor            8 0
014000060911     c
014100080926     c* Carico in tabella tutti i codici clienti che vogliono la trasmission
014200080926     c*  completa delle c.a.
014300080926     c
014400080926     C                   CLEAR                   IZ                4 0
014500080926     c                   eval      kcod='3K'
014600080926
014700080926     C     KE2TAB        SETLL     TABEL00F
014800080926     C     KE2TAB        READE     TABEL00F                               98
014900080926     C     *IN98         DOWEQ     *OFF
015000080926     c                   if        tblflg=' '
015100080926     C                   MOVEL     TBLUNI        DS3k
015200080926     c                   if        §3kca='S'
015300080926     C                   ADD       1             IZ
015400080926     C                   MOVEL     TBLKEY        clica(IZ)
015500080926     C                   MOVEL     §3kgks        cliun(IZ)
015600080926     C                   ENDIF
015700080926     C                   ENDIF
015800080926     c
015900080926     C     KE2TAB        READE     TABEL00F                               98
016000080926     C                   ENDDO
016100080926     c                   clear                   cliela
016200060829     c
016300060828     c                   ENDSR
016400080926     c*------------------------------------------------------------------
016500080926     c* Cerco il beneficiario della c.a.
016600080926     c*------------------------------------------------------------------
016700080926     c     BENEFIC       BEGSR
016800080926     c                   clear                   wbksc
016900080926     c
017000080926     c* solo se flag già impostato
017100080926    1c                   if        dctptr<>' '    or dctksc>0
017200080926     c
017300080926     c                   exsr      CERBOL
017400080926
017500080926    2c                   if        dctksc>0
017600080926     c                   eval      wbksc=dctksc
017700080926   x2c                   else
017800080926    3c                   if        dctptr='M'
017900080926     c                   movel     speccm        wBksc
018000080926   x3c                   else
018100080926     c                   movel     speccd        wBksc
018200080926    3c                   endif
018300080926     c
018400080926    2c                   endif
018500080926    1c                   endif
018600080926     c
018700080926     c                   ENDSR
018800080926      *------------------------------------------------------------------------*
018900080926      * CERCO LA BOLLA DELLA C.A.
019000080926      *------------------------------------------------------------------------*
019100080926     c     CERBOL        BEGSR
019200080926     c                   clear                   WPORTO
019300080926     c                   clear                   spedata
019400080926     c                   clear                   speccm
019500080926     c                   clear                   speccd
019600080926     c                   clear                   spersd
019700080926     c                   clear                   spermn
019800080926     c                   clear                   sperma
019900080926     c                   clear                   dcknsp
020000080926     c* Cerco la bolla
020100080926      * se non sono in sede verifico se CA con legami e se sono in arrivo rispetto la CA
020200080926    1c                   if        §dctlega = 'S'
020300080926     c     Kdct2         chain     fndck01l
020400080926     c
020500080926    2c                   if        not %found(fndck01l)
020600080926     c                   clear                   dckaas
020700080926     c                   clear                   dcklnp
020800080926     c                   clear                   dcknrs
020900080926     c                   clear                   dcknsp
021000080926    2c                   endif
021100080926    1c                   endif
021200080926     c
021300080926     c* Per bolla legata prendo mittente e data spedizione da mamma
021400080926     c*  destinatario da figlia
021500080926    1c                   if        dcknsp>0
021600080926     c     kbolla        chain     fnblp01l
021700080926    2c                   if        %found(FNBLP01L)
021800080926     c                   eval      WBOL='P'
021900080926     c                   exsr      CARDATIBOlla
022000080926    2c                   endif
022100080926     c
022200080926     c     kbolladck     chain     fnarb01l
022300080926    2c                   if        %found(FNARB01L)
022400080926     c                   eval      WBOL='A'
022500080926     c
022600080926     c* Se non trovato blp della mamma, riempo i campi del mittente e
022700080926     c*  DSP da fnarb della figlia
022800080926    3c                   if        not %found(FNBLP01L)
022900080926     c                   exsr      CARDatibolla
023000080926    3c                   endif
023100080926     c
023200080926     c                   movel     arbrsd        spersd
023300080926    3c                   if        speccd=0 and wporto='A'
023400080926     c                   movel     arbksc        speccd
023500080926    3c                   endif
023600080926     c                   else
023700080926     c* destinatario da bolla BLP se non trovo ARB
023800080926     c                   movel     arbrsd        spersd
023900080926    2c                   endif
024000080926     c
024100080926   x1c                   else
024200080926      *
024300080926     c* Se non c'e' legame bolla, cerco prima in fnarb e poi in fnblp
024400080926     c     kbolla        chain     fnarb01l
024500080926    2c                   if        %found(FNARB01L)
024600080926     c                   eval      WBOL='A'
024700080926   x2c                   else
024800080926     c     kbolla        chain     fnblp01l
024900080926    3c                   if        %found(FNBLP01L)
025000080926     c                   eval      WBOL='P'
025100080926    3c                   endif
025200080926    2c                   endif
025300080926     c
025400080926     c* Verifico se la bolla e' franco o assegnato
025500080926    2c                   if        wbol<> ' '
025600080926     c                   exsr      CARDAtibolla
025700080926     c                   movel     arbrsd        spersd
025800080926     c
025900080926    3c                   if        wbol='A' and wporto='A'
026000080926     c                   movel     arbksc        speccd
026100080926    3c                   endif
026200080926    2c                   endif
026300080926    1c                   endif
026400080926     c
026500080926     c                   ENDSR
026600080926      *------------------------------------------------------------------------*
026700080926      * CARICO dati dalla bolla "principale" trovata
026800080926      *------------------------------------------------------------------------*
026900080926     c     CARDATiBOLLA  BEGSR
027000080926     c                   movel     arbdsp        spedata
027100080926     c                   movel     arbrmn        spermn
027200080926     c                   movel     arbrma        sperma
027300080926     c
027400080926     c                   exsr      CERTBO
027500080926     c
027600080926    1c                   if        wporto='F'
027700080926     c                   movel     arbksc        speccm
027800080926    1c                   else
027900080926     c                   movel     arbccm        speccm
028000080926     c
028100080926     c* chain in fiar6 per prendere codice di tassazione
028200080926    2c                   if        WBOL='P'
028300080926     c                   eval      Ktrc     = '1'
028400080926     c     kBOLtrc       chain     FIAR6000
028500080926    3c                   if        %found(FIAR601L)
028600080926     c                   move      ar6ksc        w004a             4
028700080926    4c                   if        w004a<>'9999'
028800080926     c                   movel     ar6ksc        speccd
028900080926    4c                   endif
029000080926    3c                   endif
029100080926    2c                   endif
029200080926    1c                   endif
029300080926     c                   ENDSR
029400080926      *------------------------------------------------------------------------*
029500080926      * CERCO il tipo bolla della spedizione
029600080926      *------------------------------------------------------------------------*
029700080926     c     CERTBO        BEGSR
029800080926     C                   movel     '3A'          kCOD
029900080926     C                   movel(P)  ARBCBO        kKEY
030000080926     C     ktab          CHAIN     TABEL                              31
030100080926     C  n31              movel     TBLUNI        DS3A
030200080926     C  n31              movel     §3ATB1        WPORTO            1
030300080926     c   31              movel     'F'           WPORTO
030400080926     c                   endsr
030500060828     c*------------------------------------------------------------------
030600060829     c* Elaborazione per scrittura file TIVGD00F
030700060828     c*------------------------------------------------------------------
030800060828     c     ELABORA       BEGSR
030900060829     c
031000060829     c* Preparo la scrittura del file TIVGD
031100060829     c                   clear                   tivgd000
031200080926     c                   eval      vgdtip='CA'
031300080930     c                   eval      vgdksu='0'+cliun(indx)
031400060829     c                   eval      vgdtsc='WW'
031500060829     c
031600060829     c                   eval      vgddaT=datcor
031700081210     c                   eval      vgdpgm='TITV1K9R'
031800070305     c                   clear                   DVGDFLO
031900080930     c*****              movel     'P'           §VGDFELA
032000080930     c*****              movel     DVGDFLO       vgdflo
032100060829     c
032200080926     c* Imposto i dati in DS
032300080926     c                   clear                   DCANOM_I
032400080926     c                   eval      §CAIAAS=%editc(dctaas:'Z')
032500080926     c                   eval      §CAIlnp=%editc(dctlnp:'Z')
032600080926     c                   eval      §CAInrs=%Editc(dctnrs:'Z')
032700080926     c                   eval      §CAInsp=%Editc(dctnsp:'Z')
032800080926     c                   move      spedata       §CAImgs
032900080926     c                   eval      §CAIrmn=%Editc(spermn:'Z')
033000080926     c                   eval      §CAIrma=sperma
033100080926     c                   eval      §CAIrsd=spersd
033200080926     c* Tipo anomalia danni
033300080926     c                   eval      §CAItad=dcttad
033400080926     c                   clear                   DTAD
033500080926     c                   clear                   TIBS02DS
033600080926     C                   MOVEL     'C'           t02mod
033700080926     C                   MOVEL     knsif         t02sif
033800080926     C                   MOVEL     'TAD'         t02cod
033900080926     C                   MOVEL(P)  dcttad        t02ke1
034000080926     C                   CALL      'TIBS02R'
034100080926     C                   PARM                    KPJBA
034200080926     C                   PARM                    TIBS02DS
034300080926      *
034400080926     C                   if        t02err = *BLANKS
034500080926     C                   MOVEL     T02UNI        DTAD
034600080926     C                   endif
034700080926     c                   eval      §caitadd=§taddesc
034800080926      *
034900080926     c                   eval      §CAIAAc=%editc(dctaac:'Z')
035000080926     c                   movel     dctmgc        §caimgc
035100080926     c                   eval      §CAIfil=%editc(dctfil:'Z')
035200080926     c                   eval      §CAInca=%editc(dctnca:'Z')
035300080926     c                   movel     dctdch        §CAIdch
035400080926     c* Causale chiusura C.A.
035500080926     c                   movel     dctcch        §CAIcch
035600080926     c                   clear                   DCCH
035700081001     c                   if        dctcch<>*blanks
035800080926     c                   clear                   TIBS02DS
035900080926     C                   MOVEL     'C'           t02mod
036000080926     C                   MOVEL     knsif         t02sif
036100080926     C                   MOVEL     'CCH'         t02cod
036200080926     C                   MOVEL(P)  dctcch        t02ke1
036300080926     C                   CALL      'TIBS02R'
036400080926     C                   PARM                    KPJBA
036500080926     C                   PARM                    TIBS02DS
036600080926      *
036700080926     C                   if        t02err = *BLANKS
036800080926     C                   MOVEL     T02UNI        DCCH
036900080926     C                   endif
037000080926     c                   eval      §caicchd=§cchdesc
037100081001     c                   endif
037200080926
037300080926     c                   eval      §CAIfca=%editc(dctfca:'Z')
037400080926     c     dctfca        chain     fndfa01l
037500080926     c                   if        %found(fndfa01l)
037600080926     c                   movel     dfades        §CAIfcad
037700080926     c                   endif
037800080926     c
037900080926     c* Data fase da fndcf
038000080926     c     kdct2         setgt     fndcf01l
038100080926     c     kdct2         readpe    fndcf01l
038200080926     c                   dow       not  %eof(fndcf01l)
038300080926     c                   if        dcffca=dctfca
038400080926     c                   movel     dcfdfc        §CAIdfc
038500080926     c                   leave
038600080926     c                   endif
038700080926     c
038800080926     c     kdct2         readpe    fndcf01l
038900080926     c                   enddo
039000080926     c
039100080926     c                   eval      §CAIipv=%editc(dctipv:'4')
039200080926     c                   movel     dctvpv        §CAIvpv
039300080926     c
039400080926     c     kdct2         chain     fndcl01l
039500080926     c                   if        %found(fndcl01l)
039600080926     c                   eval      §CAIipr=%editc(dclipr:'4')
039700080926     c                   movel     dclvpr        §CAIvpr
039800081001     c
039900081001     c* importo liquidato solo se causale chiusura che prevede pagament
040000081001     c                   if        §cchpaga='S'
040100080926     c                   eval      §CAIipl=%editc(dclipl:'4')
040200080926     c                   movel     dclvpl        §CAIvpl
040300080926     c
040400080926     c                   movel     dclasn        §CAIasn
040500080926     c                   if        dcldea>0
040600080926     c                   movel     dcldea        §CAIdea
040700080926     c                   endif
040800080926     c                   movel     dclabi        §CAIabi
040900080926     c                   movel     dclcab        §CAIcab
041000080926     c                   endif
041100081001     c                   endif
041200080926     c
041300080926     c                   eval      VGDDTA =%subst(Dcanom_I:1:%size(Dcanom_I))
041400070315     c*
041500060829     c                   write     tivgd000
041600080926     c
041700080926     c                   eval      cliela(Indx)='S'
041800070315     c*
041900060828     c                   ENDSR
