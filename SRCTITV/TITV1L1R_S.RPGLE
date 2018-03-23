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
003300090126     D DSCH          E DS
003400080926     d
003500080926     D                 DS
003600080926     D  arbaas
003700080926     D  arbmgs
003800080926     D  arbdsp                 1      8  0
003900060828      *
004000090126     d Dataiso         s               d   datfmt(*iso)
004100090126     d Dataiso2        s               d   datfmt(*iso)
004200060829     d Dataeur         s               d   datfmt(*eur)
004300070124     d ktbkut          s                   like(tblkut) inz(1)
004400080926     D Kcod            S                   LIKE(tblcod)
004500080926     D Kkey            S                   LIKE(tblkey)
004600080926
004700080926     d Indx            s              3  0
004800080926     d wporto          s              1
004900080926     d wbol            s              1
005000080926     d spedata         s              8  0
005100090126     d ggela           s              2  0
005200080926     d speccm          s                   like(arbksc)
005300080926     d speccd          s                   like(arbksc)
005400080926     d spersd          s                   like(arbrsd)
005500080926     d spermn          s                   like(arbrmn)
005600080926     d sperma          s                   like(arbrma)
005700080926     d wBksc           s                   like(arbksc)
005800080926     D KTRC            S                   LIKE(ar6trc)
005900090126     D Kecod           S                   LIKE(tbecod)
006000090126     D Keke1           S                   LIKE(tbeke1)
006100090126     D Keke2           S                   LIKE(tbeke2)
006200090126     D Kelin           S                   LIKE(tbelin) inz(' ')
006300090126     D Kesif           S                   LIKE(tbesif)
006400941109     C*------------------------------------------------------------------------*
006500060424     c     *entry        plist
006600060424     c                   parm                    kpjba
006700080926     c
006800080926     c     *loval        setll     fndct24l
006900080926     c                   read      fndct24l
007000080926     c
007100080926    1c                   dow       not %eof(fndct24l)
007200080926     c* Escludo le c.a. annullate o con fase < 100
007300090109     c* elaboro solo le c.a. dal 2008 in poi
007400090109    2c                   if        dctatb=' ' and dctfca>=100  and
007500090109     c                             dctaac>=2008
007600090109     c                   clear                   dcffca
007700090126     c* la c.a. deve essere stata chiusa nell'ultimo mese di elaborazione
007800090421   2ac                   if        dctfca<900 or dctfca=900 and dctdch>=
007900090421     c                             wchiusaDA and dctdch<=wchiusaA
008000090421     c
008100090421   2ac**                 if        dctfca<900 or dctfca=900 and dctdch>=
008200090421     c***                          20090306  and dctdch<=20090419
008300090126
008400081001     c* Se la c.a. + chiusa, verifico se c'e' stata almeno la fase 100
008500081001    3c                   if        dctfca=900
008600081001     c     kdct2         setgt     fndcf01l
008700081001     c     kdct2         readpe    fndcf01l
008800081001    4c                   dow       not  %eof(fndcf01l)
008900081001    5c                   if        dcffca<900
009000081001     c                   leave
009100081001     c                   else
009200081001     c     kdct2         readpe    fndcf01l
009300081001    5c                   endif
009400081001    4c                   enddo
009500090126     c
009600081001    3c                   endif
009700081001     c
009800081001   3ac                   if        (dctfca>=100 and dctfca<900) or dcffca>=100
009900080926
010000080926     c                   movel     dctflo        ddct01
010100080926     c
010200080926     c* Verifico se il beneficiario  è tra quelli che prevede la
010300080926     c*  trasmissione
010400080926     c                   exsr      BENEFIC
010500080926     c
010600080926    3c                   if        wBksc>0
010700080926     c                   eval      Indx=%lookup(wbksc:clica)
010800080926    4c                   if        Indx>0
010900080926     c* Scrivo record di TIVGD
011000080926     c                   exsr      ELABORA
011100080926    4c                   endif
011200080926    3c                   endif
011300081001   3ac                   endif
011400090126   2ac                   endif
011500090126    2c                   endif
011600080926     c
011700080926     c                   read      fndct24l
011800080926    1c                   enddo
011900070919     c
012000000000     C                   SETON                                        LR
012100060828     c*------------------------------------------------------------------
012200060828     c     *INZSR        BEGSR
012300080926     c
012400080926     C     kdct2         KLIST                                                  *
012500080926     C                   KFLD                    dctaac                         *
012600080926     C                   KFLD                    dctfil                         *
012700080926     C                   KFLD                    dctnca                         *
012800080926     C     kbolla        KLIST                                                  *
012900080926     C                   KFLD                    dctaas                         *
013000080926     C                   KFLD                    dctlnp                         *
013100080926     C                   KFLD                    dctnrs                         *
013200080926     C                   KFLD                    dctnsp                         *
013300080926     C     kbolladck     KLIST                                                  *
013400080926     C                   KFLD                    dckaas                         *
013500080926     C                   KFLD                    dcklnp                         *
013600080926     C                   KFLD                    dcknrs                         *
013700080926     C                   KFLD                    dcknsp                         *
013800080926     c     kBOLtrc       klist
013900080926     c                   kfld                    DCTaas
014000080926     c                   kfld                    DCTlnp
014100080926     c                   kfld                    DCTnrs
014200080926     c                   kfld                    DCTnsp
014300080926     c                   kfld                    Ktrc
014400070124     C     KE2TAB        KLIST
014500070124     C                   KFLD                    KTBKUT
014600080926     C                   KFLD                    KCOD
014700080926     C     ktab          KLIST                                                  *
014800080926     C                   KFLD                    KTBKUT
014900080926     C                   KFLD                    kcod                           *
015000080926     C                   KFLD                    kkey                           *
015100090126     C     ktbe          KLIST                                                  *
015200090126     C                   KFLD                    kecod
015300090126     C                   KFLD                    keke1                          *
015400090126     C                   KFLD                    keke2                          *
015500090126     C                   KFLD                    kelin                          *
015600090126     C                   KFLD                    kesif                          *
015700060829     c* Data di elaborazione
015800060829     c                   time                    w0140            14 0
015900060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
016000060829     c     *eur          movel     w0080         dataeur
016100060829     c                   movel     dataeur       dataiso
016200060829     c     *iso          movel     dataiso       datcor            8 0
016300060911     c
016400080926     c* Carico in tabella tutti i codici clienti che vogliono la trasmission
016500080926     c*  completa delle c.a.
016600080926     c
016700080926     C                   CLEAR                   IZ                4 0
016800080926     c                   eval      kcod='3K'
016900080926
017000080926     C     KE2TAB        SETLL     TABEL00F
017100080926     C     KE2TAB        READE     TABEL00F                               98
017200080926     C     *IN98         DOWEQ     *OFF
017300080926     c                   if        tblflg=' '
017400080926     C                   MOVEL     TBLUNI        DS3k
017500080926     c                   if        §3kca='S'
017600080926     C                   ADD       1             IZ
017700080926     C                   MOVEL     TBLKEY        clica(IZ)
017800080926     C                   MOVEL     §3kgks        cliun(IZ)
017900080926     C                   ENDIF
018000080926     C                   ENDIF
018100080926     c
018200080926     C     KE2TAB        READE     TABEL00F                               98
018300080926     C                   ENDDO
018400080926     c                   clear                   cliela
018500090126     c* le c.a. chiuse vengono date  dalla data ultima elaborazione
018600090126     c*  il giorno lo prendo dalla tabella SCH
018700090126     c                   eval      kecod='SCH'
018800090126     c                   eval      kesif=knsif
018900090126     c                   eval      keke1='TV1L           '
019000090126     c                   clear                   keke2
019100090126     c     ktbe          chain     tntbe01l
019200090126     c                   if        %found(tntbe01l)
019300090126     c                   eval      dsch=tbeuni
019400090126     c                   eval      ggela=d§schggm
019500090126     c                   else
019600090126     c                   eval      ggela=20
019700090126     c                   endif
019800121008     C*
019900121008     C* se l'azione è sospesa esco senza fare nulla
020000121008     C                   IF        D§SCHSosp = 'S'
020100121008     C                   SETON                                        LR
020200121008     C                   RETURN
020300121008     C                   ENDIF
020400121008     C*
020500090126     c* oggi - 1 giorno -> data chiusura AL
020600090126     c     dataiso       subdur    1:*d          dataiso2
020700090126     c     *iso          movel     dataiso2      wchiusaA          8 0
020800090126     c* oggi - 1 mese  --> data chiusura DAL   al gionro di elab.preced
020900090126     c     dataiso       subdur    1:*m          dataiso2
021000090126     c     *iso          movel     dataiso2      wchiusaDA         8 0
021100090126     c                   move      ggela         wchiusaDA
021200060829     c
021300060828     c                   ENDSR
021400080926     c*------------------------------------------------------------------
021500080926     c* Cerco il beneficiario della c.a.
021600080926     c*------------------------------------------------------------------
021700080926     c     BENEFIC       BEGSR
021800080926     c                   clear                   wbksc
021900080926     c
022000080926     c* solo se flag già impostato
022100080926    1c                   if        dctptr<>' '    or dctksc>0
022200080926     c
022300080926     c                   exsr      CERBOL
022400080926
022500080926    2c                   if        dctksc>0
022600080926     c                   eval      wbksc=dctksc
022700080926   x2c                   else
022800080926    3c                   if        dctptr='M'
022900080926     c                   movel     speccm        wBksc
023000080926   x3c                   else
023100080926     c                   movel     speccd        wBksc
023200080926    3c                   endif
023300080926     c
023400080926    2c                   endif
023500080926    1c                   endif
023600080926     c
023700080926     c                   ENDSR
023800080926      *------------------------------------------------------------------------*
023900080926      * CERCO LA BOLLA DELLA C.A.
024000080926      *------------------------------------------------------------------------*
024100080926     c     CERBOL        BEGSR
024200080926     c                   clear                   WPORTO
024300080926     c                   clear                   spedata
024400080926     c                   clear                   speccm
024500080926     c                   clear                   speccd
024600080926     c                   clear                   spersd
024700080926     c                   clear                   spermn
024800080926     c                   clear                   sperma
024900080926     c                   clear                   dcknsp
025000080926     c* Cerco la bolla
025100080926      * se non sono in sede verifico se CA con legami e se sono in arrivo rispetto la CA
025200080926    1c                   if        §dctlega = 'S'
025300080926     c     Kdct2         chain     fndck01l
025400080926     c
025500080926    2c                   if        not %found(fndck01l)
025600080926     c                   clear                   dckaas
025700080926     c                   clear                   dcklnp
025800080926     c                   clear                   dcknrs
025900080926     c                   clear                   dcknsp
026000080926    2c                   endif
026100080926    1c                   endif
026200080926     c
026300080926     c* Per bolla legata prendo mittente e data spedizione da mamma
026400080926     c*  destinatario da figlia
026500080926    1c                   if        dcknsp>0
026600080926     c     kbolla        chain     fnblp01l
026700080926    2c                   if        %found(FNBLP01L)
026800080926     c                   eval      WBOL='P'
026900080926     c                   exsr      CARDATIBOlla
027000080926    2c                   endif
027100080926     c
027200080926     c     kbolladck     chain     fnarb01l
027300080926    2c                   if        %found(FNARB01L)
027400080926     c                   eval      WBOL='A'
027500080926     c
027600080926     c* Se non trovato blp della mamma, riempo i campi del mittente e
027700080926     c*  DSP da fnarb della figlia
027800080926    3c                   if        not %found(FNBLP01L)
027900080926     c                   exsr      CARDatibolla
028000080926    3c                   endif
028100080926     c
028200080926     c                   movel     arbrsd        spersd
028300080926    3c                   if        speccd=0 and wporto='A'
028400080926     c                   movel     arbksc        speccd
028500080926    3c                   endif
028600080926     c                   else
028700080926     c* destinatario da bolla BLP se non trovo ARB
028800080926     c                   movel     arbrsd        spersd
028900080926    2c                   endif
029000080926     c
029100080926   x1c                   else
029200080926      *
029300080926     c* Se non c'e' legame bolla, cerco prima in fnarb e poi in fnblp
029400080926     c     kbolla        chain     fnarb01l
029500080926    2c                   if        %found(FNARB01L)
029600080926     c                   eval      WBOL='A'
029700080926   x2c                   else
029800080926     c     kbolla        chain     fnblp01l
029900080926    3c                   if        %found(FNBLP01L)
030000080926     c                   eval      WBOL='P'
030100080926    3c                   endif
030200080926    2c                   endif
030300080926     c
030400080926     c* Verifico se la bolla e' franco o assegnato
030500080926    2c                   if        wbol<> ' '
030600080926     c                   exsr      CARDAtibolla
030700080926     c                   movel     arbrsd        spersd
030800080926     c
030900080926    3c                   if        wbol='A' and wporto='A'
031000080926     c                   movel     arbksc        speccd
031100080926    3c                   endif
031200080926    2c                   endif
031300080926    1c                   endif
031400080926     c
031500080926     c                   ENDSR
031600080926      *------------------------------------------------------------------------*
031700080926      * CARICO dati dalla bolla "principale" trovata
031800080926      *------------------------------------------------------------------------*
031900080926     c     CARDATiBOLLA  BEGSR
032000080926     c                   movel     arbdsp        spedata
032100080926     c                   movel     arbrmn        spermn
032200080926     c                   movel     arbrma        sperma
032300080926     c
032400080926     c                   exsr      CERTBO
032500080926     c
032600080926    1c                   if        wporto='F'
032700080926     c                   movel     arbksc        speccm
032800080926    1c                   else
032900080926     c                   movel     arbccm        speccm
033000080926     c
033100080926     c* chain in fiar6 per prendere codice di tassazione
033200080926    2c                   if        WBOL='P'
033300080926     c                   eval      Ktrc     = '1'
033400080926     c     kBOLtrc       chain     FIAR6000
033500080926    3c                   if        %found(FIAR601L)
033600080926     c                   move      ar6ksc        w004a             4
033700080926    4c                   if        w004a<>'9999'
033800080926     c                   movel     ar6ksc        speccd
033900080926    4c                   endif
034000080926    3c                   endif
034100080926    2c                   endif
034200080926    1c                   endif
034300080926     c                   ENDSR
034400080926      *------------------------------------------------------------------------*
034500080926      * CERCO il tipo bolla della spedizione
034600080926      *------------------------------------------------------------------------*
034700080926     c     CERTBO        BEGSR
034800080926     C                   movel     '3A'          kCOD
034900080926     C                   movel(P)  ARBCBO        kKEY
035000080926     C     ktab          CHAIN     TABEL                              31
035100080926     C  n31              movel     TBLUNI        DS3A
035200080926     C  n31              movel     §3ATB1        WPORTO            1
035300080926     c   31              movel     'F'           WPORTO
035400080926     c                   endsr
035500060828     c*------------------------------------------------------------------
035600060829     c* Elaborazione per scrittura file TIVGD00F
035700060828     c*------------------------------------------------------------------
035800060828     c     ELABORA       BEGSR
035900060829     c
036000060829     c* Preparo la scrittura del file TIVGD
036100060829     c                   clear                   tivgd000
036200080926     c                   eval      vgdtip='CA'
036300080930     c                   eval      vgdksu='0'+cliun(indx)
036400060829     c                   eval      vgdtsc='WW'
036500060829     c
036600060829     c                   eval      vgddaT=datcor
036700090126     c                   eval      vgdpgm='TITV1L1R'
036800070305     c                   clear                   DVGDFLO
036900080930     c*****              movel     'P'           §VGDFELA
037000080930     c*****              movel     DVGDFLO       vgdflo
037100060829     c
037200080926     c* Imposto i dati in DS
037300080926     c                   clear                   DCANOM_I
037400080926     c                   eval      §CAIAAS=%editc(dctaas:'Z')
037500080926     c                   eval      §CAIlnp=%editc(dctlnp:'Z')
037600080926     c                   eval      §CAInrs=%Editc(dctnrs:'Z')
037700080926     c                   eval      §CAInsp=%Editc(dctnsp:'Z')
037800080926     c                   move      spedata       §CAImgs
037900080926     c                   eval      §CAIrmn=%Editc(spermn:'Z')
038000080926     c                   eval      §CAIrma=sperma
038100080926     c                   eval      §CAIrsd=spersd
038200080926     c* Tipo anomalia danni
038300080926     c                   eval      §CAItad=dcttad
038400080926     c                   clear                   DTAD
038500080926     c                   clear                   TIBS02DS
038600080926     C                   MOVEL     'C'           t02mod
038700080926     C                   MOVEL     knsif         t02sif
038800080926     C                   MOVEL     'TAD'         t02cod
038900080926     C                   MOVEL(P)  dcttad        t02ke1
039000080926     C                   CALL      'TIBS02R'
039100080926     C                   PARM                    KPJBA
039200080926     C                   PARM                    TIBS02DS
039300080926      *
039400080926     C                   if        t02err = *BLANKS
039500080926     C                   MOVEL     T02UNI        DTAD
039600080926     C                   endif
039700080926     c                   eval      §caitadd=§taddesc
039800080926      *
039900080926     c                   eval      §CAIAAc=%editc(dctaac:'Z')
040000080926     c                   movel     dctmgc        §caimgc
040100080926     c                   eval      §CAIfil=%editc(dctfil:'Z')
040200080926     c                   eval      §CAInca=%editc(dctnca:'Z')
040300080926     c                   movel     dctdch        §CAIdch
040400080926     c* Causale chiusura C.A.
040500080926     c                   movel     dctcch        §CAIcch
040600080926     c                   clear                   DCCH
040700081001     c                   if        dctcch<>*blanks
040800080926     c                   clear                   TIBS02DS
040900080926     C                   MOVEL     'C'           t02mod
041000080926     C                   MOVEL     knsif         t02sif
041100080926     C                   MOVEL     'CCH'         t02cod
041200080926     C                   MOVEL(P)  dctcch        t02ke1
041300080926     C                   CALL      'TIBS02R'
041400080926     C                   PARM                    KPJBA
041500080926     C                   PARM                    TIBS02DS
041600080926      *
041700080926     C                   if        t02err = *BLANKS
041800080926     C                   MOVEL     T02UNI        DCCH
041900080926     C                   endif
042000080926     c                   eval      §caicchd=§cchdesc
042100081001     c                   endif
042200080926
042300080926     c                   eval      §CAIfca=%editc(dctfca:'Z')
042400080926     c     dctfca        chain     fndfa01l
042500080926     c                   if        %found(fndfa01l)
042600080926     c                   movel     dfades        §CAIfcad
042700080926     c                   endif
042800080926     c
042900080926     c* Data fase da fndcf
043000080926     c     kdct2         setgt     fndcf01l
043100080926     c     kdct2         readpe    fndcf01l
043200080926     c                   dow       not  %eof(fndcf01l)
043300080926     c                   if        dcffca=dctfca
043400080926     c                   movel     dcfdfc        §CAIdfc
043500080926     c                   leave
043600080926     c                   endif
043700080926     c
043800080926     c     kdct2         readpe    fndcf01l
043900080926     c                   enddo
044000080926     c
044100080926     c                   eval      §CAIipv=%editc(dctipv:'4')
044200080926     c                   movel     dctvpv        §CAIvpv
044300080926     c
044400080926     c     kdct2         chain     fndcl01l
044500080926     c                   if        %found(fndcl01l)
044600080926     c                   eval      §CAIipr=%editc(dclipr:'4')
044700080926     c                   movel     dclvpr        §CAIvpr
044800081001     c
044900081001     c* importo liquidato solo se causale chiusura che prevede pagament
045000081001     c                   if        §cchpaga='S'
045100080926     c                   eval      §CAIipl=%editc(dclipl:'4')
045200080926     c                   movel     dclvpl        §CAIvpl
045300080926     c
045400080926     c                   movel     dclasn        §CAIasn
045500080926     c                   if        dcldea>0
045600080926     c                   movel     dcldea        §CAIdea
045700080926     c                   endif
045800080926     c                   movel     dclabi        §CAIabi
045900080926     c                   movel     dclcab        §CAIcab
046000080926     c                   endif
046100081001     c                   endif
046200080926     c
046300080926     c                   eval      VGDDTA =%subst(Dcanom_I:1:%size(Dcanom_I))
046400070315     c*
046500060829     c                   write     tivgd000
046600080926     c
046700080926     c                   eval      cliela(Indx)='S'
046800070315     c*
046900060828     c                   ENDSR
