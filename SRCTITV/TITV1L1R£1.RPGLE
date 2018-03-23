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
006500080926    2c                   if        dctatb=' ' and dctfca>=100
006600081001     c* Se la c.a. + chiusa, verifico se c'e' stata almeno la fase 100
006700081001    3c                   if        dctfca=900
006800081001     c     kdct2         setgt     fndcf01l
006900081001     c     kdct2         readpe    fndcf01l
007000081001    4c                   dow       not  %eof(fndcf01l)
007100081001    5c                   if        dcffca<900
007200081001     c                   leave
007300081001     c                   else
007400081001     c     kdct2         readpe    fndcf01l
007500081001    5c                   endif
007600081001    4c                   enddo
007700081001    3c                   endif
007800081001     c
007900081001   3ac                   if        (dctfca>=100 and dctfca<900) or dcffca>=100
008000080926
008100080926     c                   movel     dctflo        ddct01
008200080926     c
008300080926     c* Verifico se il beneficiario  è tra quelli che prevede la
008400080926     c*  trasmissione
008500080926     c                   exsr      BENEFIC
008600080926     c
008700080926    3c                   if        wBksc>0
008800080926     c                   eval      Indx=%lookup(wbksc:clica)
008900080926    4c                   if        Indx>0
009000080926     c* Scrivo record di TIVGD
009100080926     c                   exsr      ELABORA
009200080926    4c                   endif
009300080926    3c                   endif
009400081001   3ac                   endif
009500080926    2c                   endif
009600080926     c
009700080926     c                   read      fndct24l
009800080926    1c                   enddo
009900070919     c
010000000000     C                   SETON                                        LR
010100060828     c*------------------------------------------------------------------
010200060828     c     *INZSR        BEGSR
010300080926     c
010400080926     C     kdct2         KLIST                                                  *
010500080926     C                   KFLD                    dctaac                         *
010600080926     C                   KFLD                    dctfil                         *
010700080926     C                   KFLD                    dctnca                         *
010800080926     C     kbolla        KLIST                                                  *
010900080926     C                   KFLD                    dctaas                         *
011000080926     C                   KFLD                    dctlnp                         *
011100080926     C                   KFLD                    dctnrs                         *
011200080926     C                   KFLD                    dctnsp                         *
011300080926     C     kbolladck     KLIST                                                  *
011400080926     C                   KFLD                    dckaas                         *
011500080926     C                   KFLD                    dcklnp                         *
011600080926     C                   KFLD                    dcknrs                         *
011700080926     C                   KFLD                    dcknsp                         *
011800080926     c     kBOLtrc       klist
011900080926     c                   kfld                    DCTaas
012000080926     c                   kfld                    DCTlnp
012100080926     c                   kfld                    DCTnrs
012200080926     c                   kfld                    DCTnsp
012300080926     c                   kfld                    Ktrc
012400070124     C     KE2TAB        KLIST
012500070124     C                   KFLD                    KTBKUT
012600080926     C                   KFLD                    KCOD
012700080926     C     ktab          KLIST                                                  *
012800080926     C                   KFLD                    KTBKUT
012900080926     C                   KFLD                    kcod                           *
013000080926     C                   KFLD                    kkey                           *
013100060829     c* Data di elaborazione
013200060829     c                   time                    w0140            14 0
013300060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
013400060829     c     *eur          movel     w0080         dataeur
013500060829     c                   movel     dataeur       dataiso
013600060829     c     *iso          movel     dataiso       datcor            8 0
013700060911     c
013800080926     c* Carico in tabella tutti i codici clienti che vogliono la trasmission
013900080926     c*  completa delle c.a.
014000080926     c
014100080926     C                   CLEAR                   IZ                4 0
014200080926     c                   eval      kcod='3K'
014300080926
014400080926     C     KE2TAB        SETLL     TABEL00F
014500080926     C     KE2TAB        READE     TABEL00F                               98
014600080926     C     *IN98         DOWEQ     *OFF
014700080926     c                   if        tblflg=' '
014800080926     C                   MOVEL     TBLUNI        DS3k
014900080926     c                   if        §3kca='S'
015000080926     C                   ADD       1             IZ
015100080926     C                   MOVEL     TBLKEY        clica(IZ)
015200080926     C                   MOVEL     §3kgks        cliun(IZ)
015300080926     C                   ENDIF
015400080926     C                   ENDIF
015500080926     c
015600080926     C     KE2TAB        READE     TABEL00F                               98
015700080926     C                   ENDDO
015800080926     c                   clear                   cliela
015900060829     c
016000060828     c                   ENDSR
016100080926     c*------------------------------------------------------------------
016200080926     c* Cerco il beneficiario della c.a.
016300080926     c*------------------------------------------------------------------
016400080926     c     BENEFIC       BEGSR
016500080926     c                   clear                   wbksc
016600080926     c
016700080926     c* solo se flag già impostato
016800080926    1c                   if        dctptr<>' '    or dctksc>0
016900080926     c
017000080926     c                   exsr      CERBOL
017100080926
017200080926    2c                   if        dctksc>0
017300080926     c                   eval      wbksc=dctksc
017400080926   x2c                   else
017500080926    3c                   if        dctptr='M'
017600080926     c                   movel     speccm        wBksc
017700080926   x3c                   else
017800080926     c                   movel     speccd        wBksc
017900080926    3c                   endif
018000080926     c
018100080926    2c                   endif
018200080926    1c                   endif
018300080926     c
018400080926     c                   ENDSR
018500080926      *------------------------------------------------------------------------*
018600080926      * CERCO LA BOLLA DELLA C.A.
018700080926      *------------------------------------------------------------------------*
018800080926     c     CERBOL        BEGSR
018900080926     c                   clear                   WPORTO
019000080926     c                   clear                   spedata
019100080926     c                   clear                   speccm
019200080926     c                   clear                   speccd
019300080926     c                   clear                   spersd
019400080926     c                   clear                   spermn
019500080926     c                   clear                   sperma
019600080926     c                   clear                   dcknsp
019700080926     c* Cerco la bolla
019800080926      * se non sono in sede verifico se CA con legami e se sono in arrivo rispetto la CA
019900080926    1c                   if        §dctlega = 'S'
020000080926     c     Kdct2         chain     fndck01l
020100080926     c
020200080926    2c                   if        not %found(fndck01l)
020300080926     c                   clear                   dckaas
020400080926     c                   clear                   dcklnp
020500080926     c                   clear                   dcknrs
020600080926     c                   clear                   dcknsp
020700080926    2c                   endif
020800080926    1c                   endif
020900080926     c
021000080926     c* Per bolla legata prendo mittente e data spedizione da mamma
021100080926     c*  destinatario da figlia
021200080926    1c                   if        dcknsp>0
021300080926     c     kbolla        chain     fnblp01l
021400080926    2c                   if        %found(FNBLP01L)
021500080926     c                   eval      WBOL='P'
021600080926     c                   exsr      CARDATIBOlla
021700080926    2c                   endif
021800080926     c
021900080926     c     kbolladck     chain     fnarb01l
022000080926    2c                   if        %found(FNARB01L)
022100080926     c                   eval      WBOL='A'
022200080926     c
022300080926     c* Se non trovato blp della mamma, riempo i campi del mittente e
022400080926     c*  DSP da fnarb della figlia
022500080926    3c                   if        not %found(FNBLP01L)
022600080926     c                   exsr      CARDatibolla
022700080926    3c                   endif
022800080926     c
022900080926     c                   movel     arbrsd        spersd
023000080926    3c                   if        speccd=0 and wporto='A'
023100080926     c                   movel     arbksc        speccd
023200080926    3c                   endif
023300080926     c                   else
023400080926     c* destinatario da bolla BLP se non trovo ARB
023500080926     c                   movel     arbrsd        spersd
023600080926    2c                   endif
023700080926     c
023800080926   x1c                   else
023900080926      *
024000080926     c* Se non c'e' legame bolla, cerco prima in fnarb e poi in fnblp
024100080926     c     kbolla        chain     fnarb01l
024200080926    2c                   if        %found(FNARB01L)
024300080926     c                   eval      WBOL='A'
024400080926   x2c                   else
024500080926     c     kbolla        chain     fnblp01l
024600080926    3c                   if        %found(FNBLP01L)
024700080926     c                   eval      WBOL='P'
024800080926    3c                   endif
024900080926    2c                   endif
025000080926     c
025100080926     c* Verifico se la bolla e' franco o assegnato
025200080926    2c                   if        wbol<> ' '
025300080926     c                   exsr      CARDAtibolla
025400080926     c                   movel     arbrsd        spersd
025500080926     c
025600080926    3c                   if        wbol='A' and wporto='A'
025700080926     c                   movel     arbksc        speccd
025800080926    3c                   endif
025900080926    2c                   endif
026000080926    1c                   endif
026100080926     c
026200080926     c                   ENDSR
026300080926      *------------------------------------------------------------------------*
026400080926      * CARICO dati dalla bolla "principale" trovata
026500080926      *------------------------------------------------------------------------*
026600080926     c     CARDATiBOLLA  BEGSR
026700080926     c                   movel     arbdsp        spedata
026800080926     c                   movel     arbrmn        spermn
026900080926     c                   movel     arbrma        sperma
027000080926     c
027100080926     c                   exsr      CERTBO
027200080926     c
027300080926    1c                   if        wporto='F'
027400080926     c                   movel     arbksc        speccm
027500080926    1c                   else
027600080926     c                   movel     arbccm        speccm
027700080926     c
027800080926     c* chain in fiar6 per prendere codice di tassazione
027900080926    2c                   if        WBOL='P'
028000080926     c                   eval      Ktrc     = '1'
028100080926     c     kBOLtrc       chain     FIAR6000
028200080926    3c                   if        %found(FIAR601L)
028300080926     c                   move      ar6ksc        w004a             4
028400080926    4c                   if        w004a<>'9999'
028500080926     c                   movel     ar6ksc        speccd
028600080926    4c                   endif
028700080926    3c                   endif
028800080926    2c                   endif
028900080926    1c                   endif
029000080926     c                   ENDSR
029100080926      *------------------------------------------------------------------------*
029200080926      * CERCO il tipo bolla della spedizione
029300080926      *------------------------------------------------------------------------*
029400080926     c     CERTBO        BEGSR
029500080926     C                   movel     '3A'          kCOD
029600080926     C                   movel(P)  ARBCBO        kKEY
029700080926     C     ktab          CHAIN     TABEL                              31
029800080926     C  n31              movel     TBLUNI        DS3A
029900080926     C  n31              movel     §3ATB1        WPORTO            1
030000080926     c   31              movel     'F'           WPORTO
030100080926     c                   endsr
030200060828     c*------------------------------------------------------------------
030300060829     c* Elaborazione per scrittura file TIVGD00F
030400060828     c*------------------------------------------------------------------
030500060828     c     ELABORA       BEGSR
030600060829     c
030700060829     c* Preparo la scrittura del file TIVGD
030800060829     c                   clear                   tivgd000
030900080926     c                   eval      vgdtip='CA'
031000080930     c                   eval      vgdksu='0'+cliun(indx)
031100060829     c                   eval      vgdtsc='WW'
031200060829     c
031300060829     c                   eval      vgddaT=datcor
031400081210     c                   eval      vgdpgm='TITV1K9R'
031500070305     c                   clear                   DVGDFLO
031600080930     c*****              movel     'P'           §VGDFELA
031700080930     c*****              movel     DVGDFLO       vgdflo
031800060829     c
031900080926     c* Imposto i dati in DS
032000080926     c                   clear                   DCANOM_I
032100080926     c                   eval      §CAIAAS=%editc(dctaas:'Z')
032200080926     c                   eval      §CAIlnp=%editc(dctlnp:'Z')
032300080926     c                   eval      §CAInrs=%Editc(dctnrs:'Z')
032400080926     c                   eval      §CAInsp=%Editc(dctnsp:'Z')
032500080926     c                   move      spedata       §CAImgs
032600080926     c                   eval      §CAIrmn=%Editc(spermn:'Z')
032700080926     c                   eval      §CAIrma=sperma
032800080926     c                   eval      §CAIrsd=spersd
032900080926     c* Tipo anomalia danni
033000080926     c                   eval      §CAItad=dcttad
033100080926     c                   clear                   DTAD
033200080926     c                   clear                   TIBS02DS
033300080926     C                   MOVEL     'C'           t02mod
033400080926     C                   MOVEL     knsif         t02sif
033500080926     C                   MOVEL     'TAD'         t02cod
033600080926     C                   MOVEL(P)  dcttad        t02ke1
033700080926     C                   CALL      'TIBS02R'
033800080926     C                   PARM                    KPJBA
033900080926     C                   PARM                    TIBS02DS
034000080926      *
034100080926     C                   if        t02err = *BLANKS
034200080926     C                   MOVEL     T02UNI        DTAD
034300080926     C                   endif
034400080926     c                   eval      §caitadd=§taddesc
034500080926      *
034600080926     c                   eval      §CAIAAc=%editc(dctaac:'Z')
034700080926     c                   movel     dctmgc        §caimgc
034800080926     c                   eval      §CAIfil=%editc(dctfil:'Z')
034900080926     c                   eval      §CAInca=%editc(dctnca:'Z')
035000080926     c                   movel     dctdch        §CAIdch
035100080926     c* Causale chiusura C.A.
035200080926     c                   movel     dctcch        §CAIcch
035300080926     c                   clear                   DCCH
035400081001     c                   if        dctcch<>*blanks
035500080926     c                   clear                   TIBS02DS
035600080926     C                   MOVEL     'C'           t02mod
035700080926     C                   MOVEL     knsif         t02sif
035800080926     C                   MOVEL     'CCH'         t02cod
035900080926     C                   MOVEL(P)  dctcch        t02ke1
036000080926     C                   CALL      'TIBS02R'
036100080926     C                   PARM                    KPJBA
036200080926     C                   PARM                    TIBS02DS
036300080926      *
036400080926     C                   if        t02err = *BLANKS
036500080926     C                   MOVEL     T02UNI        DCCH
036600080926     C                   endif
036700080926     c                   eval      §caicchd=§cchdesc
036800081001     c                   endif
036900080926
037000080926     c                   eval      §CAIfca=%editc(dctfca:'Z')
037100080926     c     dctfca        chain     fndfa01l
037200080926     c                   if        %found(fndfa01l)
037300080926     c                   movel     dfades        §CAIfcad
037400080926     c                   endif
037500080926     c
037600080926     c* Data fase da fndcf
037700080926     c     kdct2         setgt     fndcf01l
037800080926     c     kdct2         readpe    fndcf01l
037900080926     c                   dow       not  %eof(fndcf01l)
038000080926     c                   if        dcffca=dctfca
038100080926     c                   movel     dcfdfc        §CAIdfc
038200080926     c                   leave
038300080926     c                   endif
038400080926     c
038500080926     c     kdct2         readpe    fndcf01l
038600080926     c                   enddo
038700080926     c
038800080926     c                   eval      §CAIipv=%editc(dctipv:'4')
038900080926     c                   movel     dctvpv        §CAIvpv
039000080926     c
039100080926     c     kdct2         chain     fndcl01l
039200080926     c                   if        %found(fndcl01l)
039300080926     c                   eval      §CAIipr=%editc(dclipr:'4')
039400080926     c                   movel     dclvpr        §CAIvpr
039500081001     c
039600081001     c* importo liquidato solo se causale chiusura che prevede pagament
039700081001     c                   if        §cchpaga='S'
039800080926     c                   eval      §CAIipl=%editc(dclipl:'4')
039900080926     c                   movel     dclvpl        §CAIvpl
040000080926     c
040100080926     c                   movel     dclasn        §CAIasn
040200080926     c                   if        dcldea>0
040300080926     c                   movel     dcldea        §CAIdea
040400080926     c                   endif
040500080926     c                   movel     dclabi        §CAIabi
040600080926     c                   movel     dclcab        §CAIcab
040700080926     c                   endif
040800081001     c                   endif
040900080926     c
041000080926     c                   eval      VGDDTA =%subst(Dcanom_I:1:%size(Dcanom_I))
041100070315     c*
041200060829     c                   write     tivgd000
041300080926     c
041400080926     c                   eval      cliela(Indx)='S'
041500070315     c*
041600060828     c                   ENDSR
