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
019800090126     c* oggi - 1 giorno -> data chiusura AL
019900090126     c     dataiso       subdur    1:*d          dataiso2
020000090126     c     *iso          movel     dataiso2      wchiusaA          8 0
020100090126     c* oggi - 1 mese  --> data chiusura DAL   al gionro di elab.preced
020200090126     c     dataiso       subdur    1:*m          dataiso2
020300090126     c     *iso          movel     dataiso2      wchiusaDA         8 0
020400090126     c                   move      ggela         wchiusaDA
020500060829     c
020600060828     c                   ENDSR
020700080926     c*------------------------------------------------------------------
020800080926     c* Cerco il beneficiario della c.a.
020900080926     c*------------------------------------------------------------------
021000080926     c     BENEFIC       BEGSR
021100080926     c                   clear                   wbksc
021200080926     c
021300080926     c* solo se flag già impostato
021400080926    1c                   if        dctptr<>' '    or dctksc>0
021500080926     c
021600080926     c                   exsr      CERBOL
021700080926
021800080926    2c                   if        dctksc>0
021900080926     c                   eval      wbksc=dctksc
022000080926   x2c                   else
022100080926    3c                   if        dctptr='M'
022200080926     c                   movel     speccm        wBksc
022300080926   x3c                   else
022400080926     c                   movel     speccd        wBksc
022500080926    3c                   endif
022600080926     c
022700080926    2c                   endif
022800080926    1c                   endif
022900080926     c
023000080926     c                   ENDSR
023100080926      *------------------------------------------------------------------------*
023200080926      * CERCO LA BOLLA DELLA C.A.
023300080926      *------------------------------------------------------------------------*
023400080926     c     CERBOL        BEGSR
023500080926     c                   clear                   WPORTO
023600080926     c                   clear                   spedata
023700080926     c                   clear                   speccm
023800080926     c                   clear                   speccd
023900080926     c                   clear                   spersd
024000080926     c                   clear                   spermn
024100080926     c                   clear                   sperma
024200080926     c                   clear                   dcknsp
024300080926     c* Cerco la bolla
024400080926      * se non sono in sede verifico se CA con legami e se sono in arrivo rispetto la CA
024500080926    1c                   if        §dctlega = 'S'
024600080926     c     Kdct2         chain     fndck01l
024700080926     c
024800080926    2c                   if        not %found(fndck01l)
024900080926     c                   clear                   dckaas
025000080926     c                   clear                   dcklnp
025100080926     c                   clear                   dcknrs
025200080926     c                   clear                   dcknsp
025300080926    2c                   endif
025400080926    1c                   endif
025500080926     c
025600080926     c* Per bolla legata prendo mittente e data spedizione da mamma
025700080926     c*  destinatario da figlia
025800080926    1c                   if        dcknsp>0
025900080926     c     kbolla        chain     fnblp01l
026000080926    2c                   if        %found(FNBLP01L)
026100080926     c                   eval      WBOL='P'
026200080926     c                   exsr      CARDATIBOlla
026300080926    2c                   endif
026400080926     c
026500080926     c     kbolladck     chain     fnarb01l
026600080926    2c                   if        %found(FNARB01L)
026700080926     c                   eval      WBOL='A'
026800080926     c
026900080926     c* Se non trovato blp della mamma, riempo i campi del mittente e
027000080926     c*  DSP da fnarb della figlia
027100080926    3c                   if        not %found(FNBLP01L)
027200080926     c                   exsr      CARDatibolla
027300080926    3c                   endif
027400080926     c
027500080926     c                   movel     arbrsd        spersd
027600080926    3c                   if        speccd=0 and wporto='A'
027700080926     c                   movel     arbksc        speccd
027800080926    3c                   endif
027900080926     c                   else
028000080926     c* destinatario da bolla BLP se non trovo ARB
028100080926     c                   movel     arbrsd        spersd
028200080926    2c                   endif
028300080926     c
028400080926   x1c                   else
028500080926      *
028600080926     c* Se non c'e' legame bolla, cerco prima in fnarb e poi in fnblp
028700080926     c     kbolla        chain     fnarb01l
028800080926    2c                   if        %found(FNARB01L)
028900080926     c                   eval      WBOL='A'
029000080926   x2c                   else
029100080926     c     kbolla        chain     fnblp01l
029200080926    3c                   if        %found(FNBLP01L)
029300080926     c                   eval      WBOL='P'
029400080926    3c                   endif
029500080926    2c                   endif
029600080926     c
029700080926     c* Verifico se la bolla e' franco o assegnato
029800080926    2c                   if        wbol<> ' '
029900080926     c                   exsr      CARDAtibolla
030000080926     c                   movel     arbrsd        spersd
030100080926     c
030200080926    3c                   if        wbol='A' and wporto='A'
030300080926     c                   movel     arbksc        speccd
030400080926    3c                   endif
030500080926    2c                   endif
030600080926    1c                   endif
030700080926     c
030800080926     c                   ENDSR
030900080926      *------------------------------------------------------------------------*
031000080926      * CARICO dati dalla bolla "principale" trovata
031100080926      *------------------------------------------------------------------------*
031200080926     c     CARDATiBOLLA  BEGSR
031300080926     c                   movel     arbdsp        spedata
031400080926     c                   movel     arbrmn        spermn
031500080926     c                   movel     arbrma        sperma
031600080926     c
031700080926     c                   exsr      CERTBO
031800080926     c
031900080926    1c                   if        wporto='F'
032000080926     c                   movel     arbksc        speccm
032100080926    1c                   else
032200080926     c                   movel     arbccm        speccm
032300080926     c
032400080926     c* chain in fiar6 per prendere codice di tassazione
032500080926    2c                   if        WBOL='P'
032600080926     c                   eval      Ktrc     = '1'
032700080926     c     kBOLtrc       chain     FIAR6000
032800080926    3c                   if        %found(FIAR601L)
032900080926     c                   move      ar6ksc        w004a             4
033000080926    4c                   if        w004a<>'9999'
033100080926     c                   movel     ar6ksc        speccd
033200080926    4c                   endif
033300080926    3c                   endif
033400080926    2c                   endif
033500080926    1c                   endif
033600080926     c                   ENDSR
033700080926      *------------------------------------------------------------------------*
033800080926      * CERCO il tipo bolla della spedizione
033900080926      *------------------------------------------------------------------------*
034000080926     c     CERTBO        BEGSR
034100080926     C                   movel     '3A'          kCOD
034200080926     C                   movel(P)  ARBCBO        kKEY
034300080926     C     ktab          CHAIN     TABEL                              31
034400080926     C  n31              movel     TBLUNI        DS3A
034500080926     C  n31              movel     §3ATB1        WPORTO            1
034600080926     c   31              movel     'F'           WPORTO
034700080926     c                   endsr
034800060828     c*------------------------------------------------------------------
034900060829     c* Elaborazione per scrittura file TIVGD00F
035000060828     c*------------------------------------------------------------------
035100060828     c     ELABORA       BEGSR
035200060829     c
035300060829     c* Preparo la scrittura del file TIVGD
035400060829     c                   clear                   tivgd000
035500080926     c                   eval      vgdtip='CA'
035600080930     c                   eval      vgdksu='0'+cliun(indx)
035700060829     c                   eval      vgdtsc='WW'
035800060829     c
035900060829     c                   eval      vgddaT=datcor
036000090126     c                   eval      vgdpgm='TITV1L1R'
036100070305     c                   clear                   DVGDFLO
036200080930     c*****              movel     'P'           §VGDFELA
036300080930     c*****              movel     DVGDFLO       vgdflo
036400060829     c
036500080926     c* Imposto i dati in DS
036600080926     c                   clear                   DCANOM_I
036700080926     c                   eval      §CAIAAS=%editc(dctaas:'Z')
036800080926     c                   eval      §CAIlnp=%editc(dctlnp:'Z')
036900080926     c                   eval      §CAInrs=%Editc(dctnrs:'Z')
037000080926     c                   eval      §CAInsp=%Editc(dctnsp:'Z')
037100080926     c                   move      spedata       §CAImgs
037200080926     c                   eval      §CAIrmn=%Editc(spermn:'Z')
037300080926     c                   eval      §CAIrma=sperma
037400080926     c                   eval      §CAIrsd=spersd
037500080926     c* Tipo anomalia danni
037600080926     c                   eval      §CAItad=dcttad
037700080926     c                   clear                   DTAD
037800080926     c                   clear                   TIBS02DS
037900080926     C                   MOVEL     'C'           t02mod
038000080926     C                   MOVEL     knsif         t02sif
038100080926     C                   MOVEL     'TAD'         t02cod
038200080926     C                   MOVEL(P)  dcttad        t02ke1
038300080926     C                   CALL      'TIBS02R'
038400080926     C                   PARM                    KPJBA
038500080926     C                   PARM                    TIBS02DS
038600080926      *
038700080926     C                   if        t02err = *BLANKS
038800080926     C                   MOVEL     T02UNI        DTAD
038900080926     C                   endif
039000080926     c                   eval      §caitadd=§taddesc
039100080926      *
039200080926     c                   eval      §CAIAAc=%editc(dctaac:'Z')
039300080926     c                   movel     dctmgc        §caimgc
039400080926     c                   eval      §CAIfil=%editc(dctfil:'Z')
039500080926     c                   eval      §CAInca=%editc(dctnca:'Z')
039600080926     c                   movel     dctdch        §CAIdch
039700080926     c* Causale chiusura C.A.
039800080926     c                   movel     dctcch        §CAIcch
039900080926     c                   clear                   DCCH
040000081001     c                   if        dctcch<>*blanks
040100080926     c                   clear                   TIBS02DS
040200080926     C                   MOVEL     'C'           t02mod
040300080926     C                   MOVEL     knsif         t02sif
040400080926     C                   MOVEL     'CCH'         t02cod
040500080926     C                   MOVEL(P)  dctcch        t02ke1
040600080926     C                   CALL      'TIBS02R'
040700080926     C                   PARM                    KPJBA
040800080926     C                   PARM                    TIBS02DS
040900080926      *
041000080926     C                   if        t02err = *BLANKS
041100080926     C                   MOVEL     T02UNI        DCCH
041200080926     C                   endif
041300080926     c                   eval      §caicchd=§cchdesc
041400081001     c                   endif
041500080926
041600080926     c                   eval      §CAIfca=%editc(dctfca:'Z')
041700080926     c     dctfca        chain     fndfa01l
041800080926     c                   if        %found(fndfa01l)
041900080926     c                   movel     dfades        §CAIfcad
042000080926     c                   endif
042100080926     c
042200080926     c* Data fase da fndcf
042300080926     c     kdct2         setgt     fndcf01l
042400080926     c     kdct2         readpe    fndcf01l
042500080926     c                   dow       not  %eof(fndcf01l)
042600080926     c                   if        dcffca=dctfca
042700080926     c                   movel     dcfdfc        §CAIdfc
042800080926     c                   leave
042900080926     c                   endif
043000080926     c
043100080926     c     kdct2         readpe    fndcf01l
043200080926     c                   enddo
043300080926     c
043400080926     c                   eval      §CAIipv=%editc(dctipv:'4')
043500080926     c                   movel     dctvpv        §CAIvpv
043600080926     c
043700080926     c     kdct2         chain     fndcl01l
043800080926     c                   if        %found(fndcl01l)
043900080926     c                   eval      §CAIipr=%editc(dclipr:'4')
044000080926     c                   movel     dclvpr        §CAIvpr
044100081001     c
044200081001     c* importo liquidato solo se causale chiusura che prevede pagament
044300081001     c                   if        §cchpaga='S'
044400080926     c                   eval      §CAIipl=%editc(dclipl:'4')
044500080926     c                   movel     dclvpl        §CAIvpl
044600080926     c
044700080926     c                   movel     dclasn        §CAIasn
044800080926     c                   if        dcldea>0
044900080926     c                   movel     dcldea        §CAIdea
045000080926     c                   endif
045100080926     c                   movel     dclabi        §CAIabi
045200080926     c                   movel     dclcab        §CAIcab
045300080926     c                   endif
045400081001     c                   endif
045500080926     c
045600080926     c                   eval      VGDDTA =%subst(Dcanom_I:1:%size(Dcanom_I))
045700070315     c*
045800060829     c                   write     tivgd000
045900080926     c
046000080926     c                   eval      cliela(Indx)='S'
046100070315     c*
046200060828     c                   ENDSR
