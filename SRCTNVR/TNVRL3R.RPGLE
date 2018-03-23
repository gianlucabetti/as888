000100941109     H*------------------------------------------------------------------------*
000200081210     H* Ritorno dati a cliente  - dettaglio c.A. da tabella 3K                 *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600130801     fmyocod    if   e             disk
000700130801     ftabel00f  if   e           k disk
000800060911     fTNTBE01L  IF   E           K DISK
000900130802     ffndct04l  IF   E           K DISK
001000080926     ffndck01l  IF   E           K DISK
001100080926     ffndfa01l  IF   E           K DISK
001200080926     ffndcf01l  IF   E           K DISK
001300080926     ffndcl01l  IF   E           K DISK
001400080926     ffnblp01l  IF   E           K DISK
001500080926     f                                     prefix(ARB:3)
001600080926     ffnarb01l  IF   E           K DISK
001700130801     ftitas30c  IF   E           K DISK
001800130801     FTNTMD01L  IF   E           K DISK
001900130801     Ftita430c  IF   E           K DISK
002000080926     ffiar601l  IF   E           K DISK
002100060829     FTIVGD00F  o    E             DISK
002200130801     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002300070124     d
002400080926     D clica           s              7  0 dim(5000)
002500080926     D cliun           S              7    DIM(5000)
002600060828     d
002700060828     D KPJBA         E DS
002800070129     D tibs02ds      E DS
002900070305     D DVGDFLO       E DS
003000080926     D Dcanom_I      E DS
003100080926     D DDCT01        E DS
003200080926     d
003300080926     D Ds3K          E DS
003400080926     D Ds3A          E DS
003500080926     D DTAD          E DS
003600080926     D DCCH          E DS
003700090126     D DSCH          E DS
003800080926     d
003900080926     D                 DS
004000080926     D  arbaas
004100080926     D  arbmgs
004200080926     D  arbdsp                 1      8  0
004300060828      *
004400090126     d Dataiso         s               d   datfmt(*iso)
004500090126     d Dataiso2        s               d   datfmt(*iso)
004600060829     d Dataeur         s               d   datfmt(*eur)
004700070124     d ktbkut          s                   like(tblkut) inz(1)
004800080926     D Kcod            S                   LIKE(tblcod)
004900080926     D Kkey            S                   LIKE(tblkey)
005000080926
005100080926     d Indx            s              3  0
005200080926     d wporto          s              1
005300080926     d wbol            s              1
005400080926     d spedata         s              8  0
005500090126     d ggela           s              2  0
005600080926     d speccm          s                   like(arbksc)
005700080926     d speccd          s                   like(arbksc)
005800080926     d spersd          s                   like(arbrsd)
005900080926     d spermn          s                   like(arbrmn)
006000080926     d sperma          s                   like(arbrma)
006100080926     d wBksc           s                   like(arbksc)
006200080926     D KTRC            S                   LIKE(ar6trc)
006300090126     D Kecod           S                   LIKE(tbecod)
006400090126     D Keke1           S                   LIKE(tbeke1)
006500090126     D Keke2           S                   LIKE(tbeke2)
006600090126     D Kelin           S                   LIKE(tbelin) inz(' ')
006700090126     D Kesif           S                   LIKE(tbesif)
006800130801     ITITAS000
006900130801     I              TASAAS                      ARBAAS
007000130801     I              TASLNP                      ARBLNP
007100130801     I              TASNRS                      ARBNRS
007200130801     I              TASNSP                      ARBNSP
007300130801     I              TASMGS                      ARBMGS
007400130801     I              TASNCL                      ARBNCL
007500130801     I              TASKSC                      ARBKSC
007600130801     I              TASCTR                      ARBCTR
007700130801     I              TASTSP                      ARBTSP
007800130801     I              TASCCM                      ARBCCM
007900130801     I              TASLNA                      ARBLNA
008000130801     I              TASIAS                      ARBIAS
008100130801     I              TASCCA                      ARBCCA
008200130801     I              TASRMN                      ARBRMN
008300130801     I              TASpkf                      ARBpkf
008400130801     ITITAS010
008500130801     I              TASAAS                      ARBAAS
008600130801     I              TASLNP                      ARBLNP
008700130801     I              TASNRS                      ARBNRS
008800130801     I              TASNSP                      ARBNSP
008900130801     I              TASMGS                      ARBMGS
009000130801     I              TASNCL                      ARBNCL
009100130801     I              TASKSC                      ARBKSC
009200130801     I              TASCTR                      ARBCTR
009300130801     I              TASTSP                      ARBTSP
009400130801     I              TASCCM                      ARBCCM
009500130801     I              TASLNA                      ARBLNA
009600130801     I              TASIAS                      ARBIAS
009700130801     I              TASCCA                      ARBCCA
009800130801     I              TASRMN                      ARBRMN
009900130801     I              TASpkf                      ARBpkf
010000130801     ITITASP00
010100130801     I              TASAAS                      ARBAAS
010200130801     I              TASLNP                      ARBLNP
010300130801     I              TASNRS                      ARBNRS
010400130801     I              TASNSP                      ARBNSP
010500130801     I              TASMGS                      ARBMGS
010600130801     I              TASNCL                      ARBNCL
010700130801     I              TASKSC                      ARBKSC
010800130801     I              TASCTR                      ARBCTR
010900130801     I              TASTSP                      ARBTSP
011000130801     I              TASCCM                      ARBCCM
011100130801     I              TASLNA                      ARBLNA
011200130801     I              TASIAS                      ARBIAS
011300130801     I              TASCCA                      ARBCCA
011400130801     I              TASRMN                      ARBRMN
011500130801     I              TASpkf                      ARBpkf
011600941109     C*------------------------------------------------------------------------*
011700060424     c     *entry        plist
011800130802     c                   parm                    kpjba
011900130801     c                   movel     kpjbu         writevgd          1
012000080926     c
012100130801     c
012200130802     c                   z-add     2012          kanno             4 0
012300130802     c     kanno         setll     fndct04l
012400130802     c     kanno         reade     fndct04l
012500080926     c
012600130802    1c                   dow       not %eof(fndct04l)
012700080926     c* Escludo le c.a. annullate o con fase < 100
012800130801     c* elaboro solo le c.a. dal 2011 al 30/06/13
012900090109    2c                   if        dctatb=' ' and dctfca>=100  and
013000130801     c                             dctaac>=2011
013100090109     c                   clear                   dcffca
013200130801     c
013300130802     c* elaboro tutte le c.a. aperte del2011
013400130802     c**                 if        dctaac<2013 or (dctmgc<=0630  and
013500130802     c**                           dctaac=2013)
013600130802     c
013700130802     c                   if        dctaac=2012
013800130802     c
013900081001     c* Se la c.a. + chiusa, verifico se c'e' stata almeno la fase 100
014000081001    3c                   if        dctfca=900
014100081001     c     kdct2         setgt     fndcf01l
014200081001     c     kdct2         readpe    fndcf01l
014300081001    4c                   dow       not  %eof(fndcf01l)
014400081001    5c                   if        dcffca<900
014500081001     c                   leave
014600081001     c                   else
014700081001     c     kdct2         readpe    fndcf01l
014800081001    5c                   endif
014900081001    4c                   enddo
015000090126     c
015100081001    3c                   endif
015200081001     c
015300081001   3ac                   if        (dctfca>=100 and dctfca<900) or dcffca>=100
015400080926
015500080926     c                   movel     dctflo        ddct01
015600080926     c
015700080926     c* Verifico se il beneficiario  è tra quelli che prevede la
015800080926     c*  trasmissione
015900080926     c                   exsr      BENEFIC
016000080926     c
016100080926    3c                   if        wBksc>0
016200080926     c                   eval      Indx=%lookup(wbksc:clica)
016300080926    4c                   if        Indx>0
016400080926     c* Scrivo record di TIVGD
016500080926     c                   exsr      ELABORA
016600080926    4c                   endif
016700080926    3c                   endif
016800081001   3ac                   endif
016900090126   2ac                   endif
017000090126    2c                   endif
017100080926     c
017200130802     c     kanno         reade     fndct04l
017300080926    1c                   enddo
017400070919     c
017500130801     c                   except    totale
017600000000     C                   SETON                                        LR
017700060828     c*------------------------------------------------------------------
017800060828     c     *INZSR        BEGSR
017900080926     c
018000080926     C     kdct2         KLIST                                                  *
018100080926     C                   KFLD                    dctaac                         *
018200080926     C                   KFLD                    dctfil                         *
018300080926     C                   KFLD                    dctnca                         *
018400080926     C     kbolla        KLIST                                                  *
018500080926     C                   KFLD                    dctaas                         *
018600080926     C                   KFLD                    dctlnp                         *
018700080926     C                   KFLD                    dctnrs                         *
018800080926     C                   KFLD                    dctnsp                         *
018900080926     C     kbolladck     KLIST                                                  *
019000080926     C                   KFLD                    dckaas                         *
019100080926     C                   KFLD                    dcklnp                         *
019200080926     C                   KFLD                    dcknrs                         *
019300080926     C                   KFLD                    dcknsp                         *
019400080926     c     kBOLtrc       klist
019500080926     c                   kfld                    DCTaas
019600080926     c                   kfld                    DCTlnp
019700080926     c                   kfld                    DCTnrs
019800080926     c                   kfld                    DCTnsp
019900080926     c                   kfld                    Ktrc
020000070124     C     KE2TAB        KLIST
020100070124     C                   KFLD                    KTBKUT
020200080926     C                   KFLD                    KCOD
020300080926     C     ktab          KLIST                                                  *
020400080926     C                   KFLD                    KTBKUT
020500080926     C                   KFLD                    kcod                           *
020600080926     C                   KFLD                    kkey                           *
020700090126     C     ktbe          KLIST                                                  *
020800090126     C                   KFLD                    kecod
020900090126     C                   KFLD                    keke1                          *
021000090126     C                   KFLD                    keke2                          *
021100090126     C                   KFLD                    kelin                          *
021200090126     C                   KFLD                    kesif                          *
021300060829     c* Data di elaborazione
021400060829     c                   time                    w0140            14 0
021500060829     C                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
021600060829     c     *eur          movel     w0080         dataeur
021700060829     c                   movel     dataeur       dataiso
021800060829     c     *iso          movel     dataiso       datcor            8 0
021900060911     c
022000130801     c* Carico il file dei cod clienti della MYO
022100080926     c
022200080926     C                   CLEAR                   IZ                4 0
022300130801     c                   read      myocod
022400130801     c
022500130801     c                   dow       not %eof(myocod)
022600080926     C                   ADD       1             IZ
022700130801     C                   MOVEL     ccm           clica(IZ)
022800130801     C                   MOVEL     1020001       cliun(IZ)
022900080926     c
023000130801     c                   read      myocod
023100080926     C                   ENDDO
023200130801     c
023300090126     c* le c.a. chiuse vengono date  dalla data ultima elaborazione
023400090126     c*  il giorno lo prendo dalla tabella SCH
023500090126     c                   eval      kecod='SCH'
023600090126     c                   eval      kesif=knsif
023700090126     c                   eval      keke1='TV1L           '
023800090126     c                   clear                   keke2
023900090126     c     ktbe          chain     tntbe01l
024000090126     c                   if        %found(tntbe01l)
024100090126     c                   eval      dsch=tbeuni
024200090126     c                   eval      ggela=d§schggm
024300090126     c                   else
024400090126     c                   eval      ggela=20
024500090126     c                   endif
024600090126     c* oggi - 1 giorno -> data chiusura AL
024700090126     c     dataiso       subdur    1:*d          dataiso2
024800090126     c     *iso          movel     dataiso2      wchiusaA          8 0
024900090126     c* oggi - 1 mese  --> data chiusura DAL   al gionro di elab.preced
025000090126     c     dataiso       subdur    1:*m          dataiso2
025100090126     c     *iso          movel     dataiso2      wchiusaDA         8 0
025200090126     c                   move      ggela         wchiusaDA
025300060829     c
025400060828     c                   ENDSR
025500080926     c*------------------------------------------------------------------
025600080926     c* Cerco il beneficiario della c.a.
025700080926     c*------------------------------------------------------------------
025800080926     c     BENEFIC       BEGSR
025900080926     c                   clear                   wbksc
026000080926     c
026100080926     c* solo se flag già impostato
026200080926    1c                   if        dctptr<>' '    or dctksc>0
026300080926     c
026400080926     c                   exsr      CERBOL
026500080926
026600080926    2c                   if        dctksc>0
026700080926     c                   eval      wbksc=dctksc
026800080926   x2c                   else
026900080926    3c                   if        dctptr='M'
027000080926     c                   movel     speccm        wBksc
027100080926   x3c                   else
027200080926     c                   movel     speccd        wBksc
027300080926    3c                   endif
027400080926     c
027500080926    2c                   endif
027600080926    1c                   endif
027700080926     c
027800080926     c                   ENDSR
027900080926      *------------------------------------------------------------------------*
028000080926      * CERCO LA BOLLA DELLA C.A.
028100080926      *------------------------------------------------------------------------*
028200080926     c     CERBOL        BEGSR
028300080926     c                   clear                   WPORTO
028400080926     c                   clear                   spedata
028500080926     c                   clear                   speccm
028600080926     c                   clear                   speccd
028700080926     c                   clear                   spersd
028800080926     c                   clear                   spermn
028900080926     c                   clear                   sperma
029000080926     c                   clear                   dcknsp
029100130801     c* SEDE
029200130801     c     kbolla        chain     TITAS30c                           32
029300130801     c     kbolla        chain     tntmd01l                           32
029400130801    3c                   if        not *in32
029500130801     c                   eval      WBOL='D'
029600130801     c                   movel     tastbl        WPORTO
029700130801     c                   move      arbdsp        spedata
029800130801     c                   movel     arbrsd        spersd
029900130801     c                   movel     arbrmn        spermn
030000130801     c
030100130801    4c                   if        wporto='F'
030200130801     c                   movel     arbksc        speccm
030300130801   x4c                   else
030400130801     c                   movel     arbccm        speccm
030500130801     c                   movel     arbksc        speccd
030600130801    4c                   endif
030700130801    3c                   endif
030800130801     c
030900130801     c                   eval      Ktrc     = 'A'
031000130801     c     kboltrc       chain     tita430c
031100130801     c                   if        %found(tita430c)
031200130801     c                   movel     ta4not        sperma
031300130801     c                   else
031400130801     c                   clear                   sperma
031500130801     c                   endif
031600130801     c
031700130801     c                   leavesr
031800080926     c* Cerco la bolla
031900080926      * se non sono in sede verifico se CA con legami e se sono in arrivo rispetto la CA
032000130801    1c                   if        §dctlega = 'S'
032100130801     c     Kdct2         chain     fndck01l
032200130801     c
032300130801    2c                   if        not %found(fndck01l)
032400130801     c                   clear                   dckaas
032500130801     c                   clear                   dcklnp
032600130801     c                   clear                   dcknrs
032700130801     c                   clear                   dcknsp
032800080926    2c                   endif
032900080926    1c                   endif
033000080926     c
033100080926     c* Per bolla legata prendo mittente e data spedizione da mamma
033200080926     c*  destinatario da figlia
033300080926    1c                   if        dcknsp>0
033400080926     c     kbolla        chain     fnblp01l
033500080926    2c                   if        %found(FNBLP01L)
033600080926     c                   eval      WBOL='P'
033700080926     c                   exsr      CARDATIBOlla
033800080926    2c                   endif
033900080926     c
034000080926     c     kbolladck     chain     fnarb01l
034100080926    2c                   if        %found(FNARB01L)
034200080926     c                   eval      WBOL='A'
034300080926     c
034400080926     c* Se non trovato blp della mamma, riempo i campi del mittente e
034500080926     c*  DSP da fnarb della figlia
034600080926    3c                   if        not %found(FNBLP01L)
034700080926     c                   exsr      CARDatibolla
034800080926    3c                   endif
034900080926     c
035000080926     c                   movel     arbrsd        spersd
035100080926    3c                   if        speccd=0 and wporto='A'
035200080926     c                   movel     arbksc        speccd
035300080926    3c                   endif
035400080926     c                   else
035500080926     c* destinatario da bolla BLP se non trovo ARB
035600080926     c                   movel     arbrsd        spersd
035700080926    2c                   endif
035800080926     c
035900080926   x1c                   else
036000080926      *
036100080926     c* Se non c'e' legame bolla, cerco prima in fnarb e poi in fnblp
036200080926     c     kbolla        chain     fnarb01l
036300080926    2c                   if        %found(FNARB01L)
036400080926     c                   eval      WBOL='A'
036500080926   x2c                   else
036600080926     c     kbolla        chain     fnblp01l
036700080926    3c                   if        %found(FNBLP01L)
036800080926     c                   eval      WBOL='P'
036900080926    3c                   endif
037000080926    2c                   endif
037100080926     c
037200080926     c* Verifico se la bolla e' franco o assegnato
037300080926    2c                   if        wbol<> ' '
037400080926     c                   exsr      CARDAtibolla
037500080926     c                   movel     arbrsd        spersd
037600080926     c
037700080926    3c                   if        wbol='A' and wporto='A'
037800080926     c                   movel     arbksc        speccd
037900080926    3c                   endif
038000080926    2c                   endif
038100080926    1c                   endif
038200080926     c
038300080926     c                   ENDSR
038400080926      *------------------------------------------------------------------------*
038500080926      * CARICO dati dalla bolla "principale" trovata
038600080926      *------------------------------------------------------------------------*
038700080926     c     CARDATiBOLLA  BEGSR
038800080926     c                   movel     arbdsp        spedata
038900080926     c                   movel     arbrmn        spermn
039000080926     c                   movel     arbrma        sperma
039100080926     c
039200080926     c                   exsr      CERTBO
039300080926     c
039400080926    1c                   if        wporto='F'
039500080926     c                   movel     arbksc        speccm
039600080926    1c                   else
039700080926     c                   movel     arbccm        speccm
039800080926     c
039900080926     c* chain in fiar6 per prendere codice di tassazione
040000080926    2c                   if        WBOL='P'
040100080926     c                   eval      Ktrc     = '1'
040200080926     c     kBOLtrc       chain     FIAR6000
040300080926    3c                   if        %found(FIAR601L)
040400080926     c                   move      ar6ksc        w004a             4
040500080926    4c                   if        w004a<>'9999'
040600080926     c                   movel     ar6ksc        speccd
040700080926    4c                   endif
040800080926    3c                   endif
040900080926    2c                   endif
041000080926    1c                   endif
041100080926     c                   ENDSR
041200080926      *------------------------------------------------------------------------*
041300080926      * CERCO il tipo bolla della spedizione
041400080926      *------------------------------------------------------------------------*
041500080926     c     CERTBO        BEGSR
041600080926     C                   movel     '3A'          kCOD
041700080926     C                   movel(P)  ARBCBO        kKEY
041800080926     C     ktab          CHAIN     TABEL                              31
041900080926     C  n31              movel     TBLUNI        DS3A
042000080926     C  n31              movel     §3ATB1        WPORTO            1
042100080926     c   31              movel     'F'           WPORTO
042200080926     c                   endsr
042300060828     c*------------------------------------------------------------------
042400060829     c* Elaborazione per scrittura file TIVGD00F
042500060828     c*------------------------------------------------------------------
042600060828     c     ELABORA       BEGSR
042700060829     c
042800060829     c* Preparo la scrittura del file TIVGD
042900060829     c                   clear                   tivgd000
043000080926     c                   eval      vgdtip='CA'
043100080930     c                   eval      vgdksu='0'+cliun(indx)
043200060829     c                   eval      vgdtsc='WW'
043300060829     c
043400060829     c                   eval      vgddaT=datcor
043500130801     c                   eval      vgdpgm='TNVRL1R'
043600070305     c                   clear                   DVGDFLO
043700080930     c*****              movel     'P'           §VGDFELA
043800080930     c*****              movel     DVGDFLO       vgdflo
043900060829     c
044000080926     c* Imposto i dati in DS
044100080926     c                   clear                   DCANOM_I
044200080926     c                   eval      §CAIAAS=%editc(dctaas:'Z')
044300080926     c                   eval      §CAIlnp=%editc(dctlnp:'Z')
044400080926     c                   eval      §CAInrs=%Editc(dctnrs:'Z')
044500080926     c                   eval      §CAInsp=%Editc(dctnsp:'Z')
044600080926     c                   move      spedata       §CAImgs
044700080926     c                   eval      §CAIrmn=%Editc(spermn:'Z')
044800080926     c                   eval      §CAIrma=sperma
044900080926     c                   eval      §CAIrsd=spersd
045000080926     c* Tipo anomalia danni
045100080926     c                   eval      §CAItad=dcttad
045200080926     c                   clear                   DTAD
045300080926     c                   clear                   TIBS02DS
045400080926     C                   MOVEL     'C'           t02mod
045500080926     C                   MOVEL     knsif         t02sif
045600080926     C                   MOVEL     'TAD'         t02cod
045700080926     C                   MOVEL(P)  dcttad        t02ke1
045800080926     C                   CALL      'TIBS02R'
045900080926     C                   PARM                    KPJBA
046000080926     C                   PARM                    TIBS02DS
046100080926      *
046200080926     C                   if        t02err = *BLANKS
046300080926     C                   MOVEL     T02UNI        DTAD
046400080926     C                   endif
046500080926     c                   eval      §caitadd=§taddesc
046600080926      *
046700080926     c                   eval      §CAIAAc=%editc(dctaac:'Z')
046800080926     c                   movel     dctmgc        §caimgc
046900080926     c                   eval      §CAIfil=%editc(dctfil:'Z')
047000080926     c                   eval      §CAInca=%editc(dctnca:'Z')
047100080926     c                   movel     dctdch        §CAIdch
047200080926     c* Causale chiusura C.A.
047300080926     c                   movel     dctcch        §CAIcch
047400080926     c                   clear                   DCCH
047500081001     c                   if        dctcch<>*blanks
047600080926     c                   clear                   TIBS02DS
047700080926     C                   MOVEL     'C'           t02mod
047800080926     C                   MOVEL     knsif         t02sif
047900080926     C                   MOVEL     'CCH'         t02cod
048000080926     C                   MOVEL(P)  dctcch        t02ke1
048100080926     C                   CALL      'TIBS02R'
048200080926     C                   PARM                    KPJBA
048300080926     C                   PARM                    TIBS02DS
048400080926      *
048500080926     C                   if        t02err = *BLANKS
048600080926     C                   MOVEL     T02UNI        DCCH
048700080926     C                   endif
048800080926     c                   eval      §caicchd=§cchdesc
048900081001     c                   endif
049000080926
049100080926     c                   eval      §CAIfca=%editc(dctfca:'Z')
049200080926     c     dctfca        chain     fndfa01l
049300080926     c                   if        %found(fndfa01l)
049400080926     c                   movel     dfades        §CAIfcad
049500080926     c                   endif
049600080926     c
049700080926     c* Data fase da fndcf
049800080926     c     kdct2         setgt     fndcf01l
049900080926     c     kdct2         readpe    fndcf01l
050000080926     c                   dow       not  %eof(fndcf01l)
050100080926     c                   if        dcffca=dctfca
050200080926     c                   movel     dcfdfc        §CAIdfc
050300080926     c                   leave
050400080926     c                   endif
050500080926     c
050600080926     c     kdct2         readpe    fndcf01l
050700080926     c                   enddo
050800080926     c
050900080926     c                   eval      §CAIipv=%editc(dctipv:'4')
051000080926     c                   movel     dctvpv        §CAIvpv
051100080926     c
051200080926     c     kdct2         chain     fndcl01l
051300080926     c                   if        %found(fndcl01l)
051400080926     c                   eval      §CAIipr=%editc(dclipr:'4')
051500080926     c                   movel     dclvpr        §CAIvpr
051600081001     c
051700081001     c* importo liquidato solo se causale chiusura che prevede pagament
051800081001     c                   if        §cchpaga='S'
051900080926     c                   eval      §CAIipl=%editc(dclipl:'4')
052000080926     c                   movel     dclvpl        §CAIvpl
052100080926     c
052200080926     c                   movel     dclasn        §CAIasn
052300080926     c                   if        dcldea>0
052400080926     c                   movel     dcldea        §CAIdea
052500080926     c                   endif
052600080926     c                   movel     dclabi        §CAIabi
052700080926     c                   movel     dclcab        §CAIcab
052800080926     c                   endif
052900081001     c                   endif
053000080926     c
053100080926     c                   eval      VGDDTA =%subst(Dcanom_I:1:%size(Dcanom_I))
053200070315     c*
053300130801     c                   if        writevgd= 'S'
053400060829     c                   write     tivgd000
053500130801     c                   endif
053600130801     c
053700130801     c* Elaboro i totali
053800130801     c*  1) totale c.a.
053900130801     c                   add       1             totca            13 0
054000130801     c
054100130801     c*  3) totale c.a.  ancora aperte
054200130801     c                   if        dctdch=0
054300130801     c                   add       1             totcaape         13 0
054400130801     c*  4) importo complessivo c.a. ancora aperte
054500130801     c* importo liquidato solo se causale chiusura che prevede pagament
054600130801     c                   if        %found(fndcl01l)
054700130801     c                   add       dclipr        totiplape        17 3
054800130801     c                   else
054900130801     c                   add       dctipv        totiplape        17 3
055000130801     c                   endif
055100130801     c* transattiva
055200130801     c                   if        dctfpr='T'
055300130801     c                   add       1             tottransape      13 0
055400130801     c                   else
055500130801     c* pratica
055600130801     c                   add       1             totpraticaape    13 0
055700130801     c                   endif
055800130801
055900130801     c                   endif
056000130801     c
056100130801     c*  5) totale c.a. chiuse senza liquidazione
056200130801     c                   if        dctdch>0 and §cchpaga=' '
056300130801     c                   add       1             totNOliq         13 0
056400130801     c                   endif
056500130801     c
056600130801     c*  6) totale c.a. chiuse con   liquidazione
056700130801     c                   if        dctdch>0 and §cchpaga='S'
056800130801     c                   add       1             totSIliq         13 0
056900130801     c
057000130801     c*   ) importo complessivo c.a. chiuse
057100130801     c* importo liquidato solo se causale chiusura che prevede pagament
057200130801     c                   if        §cchpaga='S'   and %found(fndcl01l)
057300130801     c                   add       dclipl        totipl           17 3
057400130801     c                   else
057500130801     c                   except    err
057600130801     c                   endif
057700130801     c* transattiva
057800130801     c                   if        dctfpr='T'
057900130801     c                   add       1             tottrans         13 0
058000130801     c                   if        §cchpaga='S'   and %found(fndcl01l)
058100130801     c                   add       dclipl        totiplT          17 3
058200130801     c                   endif
058300130801     c                   else
058400130801     c* pratica
058500130801     c                   add       1             totpratica       13 0
058600130801     c                   if        §cchpaga='S'   and %found(fndcl01l)
058700130801     c                   add       dclipl        totiplP          17 3
058800130801     c                   endif
058900130801     c                   endif
059000130801     c                   endif
059100070315     c*
059200060828     c                   ENDSR
059300130801     oQsysprt   e            totale      2
059400130801     o                                         + 20 'Totali C.A. cliente MYO ap'
059500130802     o                                         +  0 'erte dal 01/01/2012 al '
059600130802     o                                         +  0 '31/12/2012  '
059700130801     oQsysprt   e            totale      3
059800130801     o                                         +  0 'Totale complessivo: C.A. '
059900130801     o                       totca         2   +  0
060000130801     oQsysprt   e            totale      3
060100130801     o                                         +  0 'Totale c.a. ancora aperte'
060200130801     o                       totcaape      2   +  0
060300130801     o                                         +  5 'Massimo importo da liquida'
060400130801     o                                         +  0 're'
060500130801     o                       totiplape     2   +  0
060600130801     oQsysprt   e            totale      2
060700130801     o                                         +  7 'di cui LIQUIDAZ.TRANSATTI'
060800130801     o                                         +  0 'VA'
060900130801     o                       tottransape   2   +  0
061000130801     o                                         +  5 'di cui PRATICA ASSICURATI'
061100130801     o                                         +  0 'VA'
061200130801     o                       totpraticaape 2   +  0
061300130801     o
061400130801     oQsysprt   e            totale      3
061500130801     o                                         +  0 'Totale c.a. chiuse SENZA '
061600130801     o                                         +  0 'rimborso'
061700130801     o                       totnoliq      2   +  0
061800130801     oQsysprt   e            totale      3
061900130801     o                                         +  0 'Totale c.a. chiuse CON   '
062000130801     o                                         +  0 'rimborso'
062100130801     o                       totSIliq      2   +  0
062200130801     o                                         +  5 'importo liquidato:'
062300130801     o                       totipl        2   +  0
062400130801     oQsysprt   e            totale      2
062500130801     o                                         +  7 'di cui LIQUIDAZ.TRANSATTI'
062600130801     o                                         +  0 'VA'
062700130801     o                       tottrans      2   +  0
062800130801     o                                         +  5 'importo liquidato:'
062900130801     o                       totiplT       2   +  0
063000130801     oQsysprt   e            totale      2
063100130801     o                                         +  7 'di cui PRATICA ASSICURATI'
063200130801     o                                         +  0 'VA'
063300130801     o                       totpratica    2   +  0
063400130801     o                                         +  5 'importo liquidato:'
063500130801     o                       totiplP       2   +  0
063600130801     oQsysprt   e            ERR         2
063700130801     o                                         +  5 'ERRORE c.a.liquidata'
063800130801     o                                         +  0 ' manca importo'
063900130801     o                       dctaac        2   +  0
064000130801     o                       dctfil        2   +  0
064100130801     o                       dctnca        2   +  0
064200130801     o
