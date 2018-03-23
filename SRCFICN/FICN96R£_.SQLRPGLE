000100020422     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020422     H*PARMS ACTGRP(QILE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400900606?     *--------------------------------------------------------------*
000500031202      * ficn96R   stampa autofattura extra                           *
000600900606?     *--------------------------------------------------------------*
000700020422     FanRCO98J  iF   E           K DISK
000800020419     Fanfrn01L  IF   E           K DISK
000900031205     Fanope01L  IF   E           K DISK
001000031127     Fantxt01L  IF   E           K DISK
001100020502     FanCBA01L  IF   E           K DISK
001200020422     FndIVA00F  IF   E           K DISK
001300021203     Ffiapd01L  IF   E           K DISK
001400031124     Ftntbe01l  IF   E           K DISK
001500020528     Ftntlz01L  IF   E           K DISK
001600031124     Ffiott06L  uF   E           K DISK
001601070606     ftnanf01L  uF a E           K DISK
001700031124     Fficn96p   O    e             PRINTER OFLIND(*IN23)
001800020422     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
001900900515      *
002000020606     D KPJBA         E DS
002001071120     D ficn99ds      E DS
002100020422?     *  Tabella Y4Z
002200020422     D angy4z        E DS                  extname(angy4zds)
002201070606     D danf0         e DS
002202070606     D danf1         e DS
002300031124     D dcaf          e DS
002400031124     D dtlz01        e DS
002500020528     D xatbds        e DS
002600020422     D soc001        E DS                  EXTNAME(xsoc001ds)
002700020422     D xsocds          DS          1000
002800020502     D*ABI/CAB
002900020502     D X59ABIDS      E DS                  INZ
003000020502  "  D* DS per input condizione di pagamento
003100020502  "  DDVCDPDS        E DS                  INZ
003200020502  "  D* Ds per output
003300020502  "  DDVOCDPDS       E DS                  INZ
003400030827?     *  Tab
003500020422     DTABG48         E DS                  EXTNAME(ANGG48DS)
003600020422     D X06DS         E DS                  EXTNAME(X06G48DS)
003700020422     D  X06SOC       E                     EXTFLD(X06SOCIETA)
003800020422     D  X06CIV       E                     EXTFLD(X06CDIVA)
003900020422     D  X06DAT       E                     EXTFLD(X06DATA)
004000020422     D  X06LIN       E                     EXTFLD(X06LINGUA)
004100900516      *
004200020419     D Ficn54ds      E DS
004201070606     D codfac          S                   like(RCOkscfact)
004300020422     d dataiso         s               d
004400020422     d dataeur         s               d   datfmt(*eur)
004500020422     D conta           S              3  0 inz
004600020422     D comodo          S              9  2 inz
004700020422     D salnra          S                   like(nrass)
004800020527     D ctgans          S                   like(rcoctgan02)
004900020422     D salpdr          S              7  0 inz
005000020422     D salfor          S              8    inz
005100020614     D salfors         S                   like(salfor)
005200020502     D kcc             S                   inz like(rcokcc)
005201071221     D comdta          S                   like(ivadtdoc) inz(d'2007-12-31')
005300031124     D fiott           DS
005400020419     d pdrs                           7  0
005500020419     d ddcs                           8  0
005600020419     d tots                          12  3
005700030827     d nrass                          9  0
005800020422     d cdfs                           8
005900031127     d nots                          35
006000011031     ***********************************************************************
006100910521     C                   EXSR      DEFCAM
006200031124     c* Lettura fiott06l ordinato per fornitore padroncino data conteg.
006300031124     c* i record scelti sono in base ai limiti scelti a
006400020419     c* video per fornitore e data protocollo, numero fattura <> 0
006500031124     c*
006600031124     c* l'indicatore 80 mi serve per evitare la doppia ribattitura
006700031124     c* nella stampa della copia IVA
006800031124     c*
006900040217     c* se stampa per libro IVA non controllo la data di stampa
007000040217     c* altrimenti deve essere a 0
007100031124     c                   if        f54uni = '999     '
007200031124     c                   seton                                        80
007300020419     C/EXEC SQL
007400031124     C+ DECLARE A1 CURSOR FOR SELECT ottpdr, OTTDDC, sum(OTTTIM), OTTnra,
007500031127     C+ OTTcdf, OTTNOT FROM fiott06l WHERE OTTSOC = :f54soc and ottcdf
007600031127     C+ between :f54pd1 and :f54pd2 and ottdft between :f54dti and
007700031127     C+ :f54dtf and ottnff <> 0 and otttip = :f54tip and otttab = :f54cau
007800040217     C+ GROUP BY ottcdf, ottnra, ottpdr, ottddc, ottnot ORDER BY ottcdf,
007900040217     C+ ottnra, ottpdr, ottddc, ottnot
008000020419     C/END-EXEC
008100031124     c                   else
008200031124     c                   setoff                                       80
008300031124     C/EXEC SQL
008400031127     C+ DECLARE B1 CURSOR FOR SELECT ottpdr, OTTDDC, sum(OTTTIM), OTTnra,
008500031127     C+ OTTcdf, OTTNOT FROM fiott06l WHERE OTTSOC = :f54soc and ottcdf
008600031127     C+ between :f54pd1 and :f54pd2 and ottdft between :f54dti and
008700031127     C+ :f54dtf and ottnff <> 0 and otttip = :f54tip and otttab = :f54cau
008800031127     C+ and ottdcn = 0 GROUP BY ottcdf, ottnra, ottpdr, ottddc, ottnot
008900031127     C+ ORDER BY ottcdf, ottnra, ottpdr, ottddc, ottnot
009000031124     C/END-EXEC
009100031124     c                   end
009200020419
009300031124     c                   if        f54uni = '999     '
009400020419     C/EXEC SQL
009500020419     C+ OPEN A1
009600020419     C/END-EXEC
009700031124     c                   else
009800031124     C/EXEC SQL
009900031124     C+ OPEN b1
010000031124     C/END-EXEC
010100031124     c                   end
010200020419
010300020419     C                   DOU       SqlCod <> 0
010400031124     c                   if        f54uni = '999     '
010500020419     C/EXEC SQL
010600031124     C+ FETCH NEXT FROM A1 INTO :fiott
010700020419     C/END-EXEC
010800031124     c                   else
010900031124     C/EXEC SQL
011000031124     C+ FETCH NEXT FROM b1 INTO :fiott
011100031124     C/END-EXEC
011200031124     c                   end
011300020419     c*
011400020419     C                   SELECT
011500020419     C                   WHEN      SqlCod = 0
011600020614     c* se nr.registrazione = * blanks aggiorno data stampa ed esco
011700030827     c                   if        nrass = 0
011800020614     c                   movel     salfor        salfors
011900020614     c                   movel     cdfs          salfor
012000020614     c                   exsr      sragg
012100020614     c                   movel     salfors       salfor
012200020531     c                   iter
012300020531     c                   end
012400020419     c* rottura fornitore
012500020422     c                   if        cdfs <> salfor or nrass <> salnra
012600020419     c                   exsr      srrotfor
012700020423     c* se la fattura è = 0 leggo un altro fornitore
012800020423     c                   if        impfat = 0
012900020423     c                   z-add     0             num               3 0
013000020423     c                   z-add     0             imppdr
013100020423     c                   iter
013200020423     c                   end
013300020419     c                   end
013400020422     c* rottura padroncino
013500020419     c                   if        pdrs <> salpdr
013600020419     c                   exsr      srrotpdr
013700020419     c                   end
013800020422     c* dettaglio campi
013900020422     C                   EXSR      srcampi
014000020701     c* stampo la riga solo se il totale è <> 0
014100020701     c                   if        imptg <> 0
014200020701     C                   ADD       1             NUM
014300020422     c                   add       1             conta
014400020422     c                   seton                                        01
014500020422     c                   exsr      lista
014600020701     c                   end
014700020422     c*
014800020419     C                   WHEN      SqlCod = 100
014900020419     C                   WHEN      SqlCod <> 0
015000020419      * Forzo la stampa del JOBLOG.
015100020419     C                   CALL      'X66CHGJOB'
015200020419     C                   ENDSL
015300020419     C                   ENDDO
015400031124     c                   if        f54uni = '999     '
015500020419     C/EXEC SQL
015600020419     C+ CLOSE A1
015700020419     C/END-EXEC
015800031124     c                   else
015900031124     C/EXEC SQL
016000031124     C+ CLOSE b1
016100031124     C/END-EXEC
016200031124     c                   end
016300020422     C* gestisco gli ultimi totali
016400020423     c                   if        impfat <> 0
016500020422     c                   exsr      srrotfor
016600020423     c                   end
016700020422     c*
016800910521     C                   SETON                                        LR
016900020419     C**************************************************************************
017000020419     C* Rottura fornitore
017100020419     C**************************************************************************
017200020419     C     Srrotfor      BEGSR                                                  *
017300020419     c* rotture precedenti
017400020419     c                   exsr      srrotpdr
017500020419     c* stampa la riga di totale
017600020419     c                   if        impfat <> 0
017700020509     c                   seton                                        05
017800020423     c                   else
017900020423     c                   z-add     0             conta
018000020419     c                   end
018100020419     c*
018200020701     c                   if        conta <> 60 and conta <> 0
018300020701     c* salto pagina per avere il totale fattura tutto insieme
018400040330     c                   if        conta > 24
018500020701     c                   seton                                        23
018600020701     c                   end
018700020701     c                   exsr      lista
018800020502     c* aggiorno la data di stampa
018900020614     c                   exsr      sragg
019000020701     c                   end
019100020422     c*
019200020419     c                   z-add     60            conta
019300020509     c                   setoff                                       23
019400020422     c* azzera il conteggio per fornitore
019500020502     c                   z-add     0             page
019600020502     c                   z-add     0             impon
019700020422     c                   z-add     0             impiva
019800020422     c                   z-add     0             impfat
019900020419     c* decodifico fornitore
020000020419     c                   exsr      srfor
020100020423     c* aggancio registrazione solo se fornitore ok
020200020423     c                   if        not *in20
020300020422     c                   move      nrass         ivanrasreg
020400020422     c     kiva          chain     ndiva00f
020500020422     c                   if        %found(ndiva00f)
020600020422     c                   move      ivadtdoc      dataeur
020700020422     c                   move      dataeur       dtfat
020800020422     c                   move      ivanrdoc      nrfat
020900020422     c                   move      ivadtreg      dataeur
021000020422     c                   move      dataeur       dtprt
021100020422     c                   move      ivanrreg      nrprt
021200020422     c                   Z-ADD     ivaimporto    impiva
021300020422     c                   Z-ADD     ivaimponib    impon
021400020422     c     impon         add       impiva        impfat
021500020422     c                   end
021600020423     c                   end
021700020531     c                   if        impfat <> 0
021800020531     c                   move      cdfs          salfor
021900020531     c                   move      nrass         salnra
022000020531     c                   end
022100020419     c*
022200020419     c                   ENDSR
022300020419     C**************************************************************************
022400031124     C* aggiorna data di stampa in FIOTT00F
022500020419     C**************************************************************************
022600020614     C     sragg         BEGSR                                                  *
022700020614     c*
022800031124     c     kott          setll     fiott06l
022900020614     c                   do        *hival
023000031124     c     kott          reade     fiott06l
023100031124     c                   if        %eof(fiott06l)
023200020614     c                   leave
023300020614     c                   end
023400020614     c* escludo quelli non fatturati oppure già stampati
023500031124     c                   if        ottnff = 0 or ottdcn <> 0
023600020614     c                   iter
023700020614     c                   end
023800020614      * controllo numero registrazione
023900031124     c                   if        ottnra <> salnra
024000020614     c                   iter
024100020614     c                   end
024200020614      *
024300020614     c                   move      *date         dataiso
024400031124     c                   move      dataiso       ottdcn
024500020614     c                   except    aggio
024600020614     c                   enddo
024601071120     c* stampo la lettera x fusione solo se data fattura = 31/12/2007
024602071221     c                   if        ivadtdoc = comdta and
024603071221     c                             f54uni <> '999     '
024604071120     c                   eval      f99socn = f54soc
024605071120     c                   eval      f99kscn = salfor
024606071120     c                   eval      f99tip = 'O'
024607071120     c                   movel(p)  ficn99ds      kpjbu
024608071120     c                   call      'FICN99R'
024609071120     c                   parm                    kpjba
024610071221     c                   end
024700020614     c*
024800020614     c                   ENDSR
024900020614     C**************************************************************************
025000020614     C* Rottura padroncino
025100020614     C**************************************************************************
025200020614     C     Srrotpdr      BEGSR                                                  *
025300020419     c                   seton                                        07
025400020419     c* decodifica padroncino
025500021203     C     kapd          CHAIN     fiapd01L
025600021203     c                   if        %found(fiapd01l)
025700070822     c                   movel(p)  apdpdr        despdr
025800020419     c                   else
025900020419     c                   movel     *blanks       despdr
026000020419    3C                   ENDIF
026100020422     c* stampa la riga di totale padroncino solo se ha più righe
026200020422     c                   eval(h)   comodo = imppdr
026300020422     c                   if        num > 1  and comodo <> impon
026400020419     c                   seton                                        03
026500020419     c                   add       1             conta
026600020419     c                   exsr      lista
026700020419     c                   end
026800020419     c* azzera il conteggio per padroncino
026900020419     c                   z-add     0             num               3 0
027000020419     c                   z-add     0             imppdr
027100020419     c                   move      pdrs          salpdr
027200020419     c*
027300020419     c                   endsr
027400020419?     *--------------------------------------------------------------*
027500020419?     *  dati in testata                                             *
027600020419?     *--------------------------------------------------------------*
027700020419     C     srfor         BEGSR
027800020419      *
027900020502     c                   eval      rcoksc = cdfs
028000020422     C     Kfrn          CHAIN     anfrn01l                           20
028100020422     C  N20Krco          CHAIN     anrco98j                           20
028200020423     c* controllo se il fornitore appartiene all'unità di reg. scelta
028300020423     c* se 999 tutte le unità
028400020423     c                   if        not *in20 and f54uni <> '999     '
028500020423     c                             and rcounita <> f54uni
028600020423     c                   seton                                        20
028700020423     c                   end
028800020528     c* se 999 tutte le unità (vuol dire che sono in sede) controllo
028900020528     c* che il fornitore sia in autofatturazione
028901090707     C                   CLEAR                   DTLZ01
029000020531     c                   if        not *in20
029100050125     c                   select
029200050125     c                   when      f54tip='A'
029300050125     c                   movel(p)  'P'           tlztip
029400050125     c                   when      f54tip='C'
029500050125     c                   movel(p)  'C'           tlztip
029600050125     c                   when      f54tip='D'
029700050125     c                   movel(p)  'D'           tlztip
029800050125     c                   endsl
029900020528     c                   move      cdfs          tlzpdr
030000020528     c     ktlz          chain     tntlz01l
030100020528     c                   if        %found(tntlz01l)
030200020528     c                   movel     tlzflo        dtlz01
030300020531     c                   if        §tlzfl1 = 'S'
030400020531     c                             and f54uni = '999     '
030500020528     c                   seton                                        20
030600020528     c                   end
030700020528     c                   end
030800020528     c                   end
030900020423     c*
031000020423     c                   if        not *in20
031001091109     C                   if        %subst(indtelex: 1: 8) = 'CAP.SOC.'
031002091109     c                   eval      csemi = indtelex
031003091109     c                   else
031005091201     c* per l'ottico non può essere blanks
031006091201     c                   eval      csemi ='.'
031007091109     c                   end
031100020419     C                   MOVEL     sogdes        desemi
031200020422     C                   eval      indemi = indindriz
031300020422     c                   if        indprov <> ' '
031400020422     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
031500020422     c                   else
031600020422     C                   eval      locemi = indlocalit
031700020422     c                   end
031800020422     C                   eval      capemi = indcap
031801091119     c*
031802091119     C                   movel     sogpartiva    piemi            20
031803091119     C                   MOVEL     indprov       premi             2
031804091119     C                   movel     sogcdfisc     cfemi            20
031805091119     c*
031806091119     C                   if        cfemi <> ' '
031807091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
031808091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
031809091119     C                             ': ' + %trimr(cfemi)
031810091119     c                   else
031811091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
031812091119     c                   end
032000031205     c* aggancio la tabella Y4Z con tipo servizio
032100031205     c                   exsr      sry4z
032200020419     c* se il fornitore ha un codice IVA
032300031205     c                   if        frncdiva <> *blanks
032400031205     c                   exsr      srese
032500031205     c                   end
032600031205     c* decodifico aliquota IVA/esenzione
032700031205     c                   clear                   comiva            5
032800031205     c                   clear                   comiva1           5
032900031205     c                   clear                   desiva
033000031205     c* se NON presente in anagrafica fornitore
033100031205     c                   if        frncdiva = *blanks
033200031205     c                   if        §4ZIVA <>0
033300031205     c                   move      §4ziva        comiva
033400020419     c                   else
033500031205     c                   EXSR      SRESE
033600031205     c                   eval      desiva = desg48
033700020419     c                   end
033800031205     c                   else
033900031205     c* se presente in anagrafica fornitore
034000031205     c                   if        ALIG48 <>0
034100031205     c                   move      alig48        comiva
034200031205     c                   else
034300031205     c                   eval      desiva = desg48
034400031205     c                   end
034500031205     c                   end
034600031205     c*
034700031205     c                   if        comiva <> ' '
034800020422     c                   eval      comiva1 = %subst(comiva: 2: 2)+ ',' +
034900020422     c                             %subst(comiva: 4: 2)
035000020422     c                   eval      desiva = 'Aliquota ' + comiva1 + '%'
035100020422     C                   end
035200020502     c* condizioni di pagamento
035300020502  "  C                   reset                   DVCDPDS
035400020502  "  C                   eval      DCPRIC='1'                                   chain
035500020502  "  C                   eval      DCPSOC=XSCSOC
035600020502  "  C                   eval      DCPUNI=frnunita
035700020502  "  C                   eval      DCPVIS='1'                                   lf principale
035800020502  "  C                   eval      DCPALC=*OFF                                  no allocazione
035900020502  "  C                   eval      DCPNRK=1                                     numero chiavi
036000020502  "  C                   eval      DCPLIN=xsclin
036100020502  "  C                   eval      DCPCOD=frncondpag
036200020502  "  C                   eval      DCPERR=*OFF
036300020502  "  C                   CALL      'DVCDP'
036400020502  "  C                   PARM                    DVCDPDS
036500020502  "  C                   PARM                    DVOCDPDS
036600020502  "  C                   if        DCPERR=*OFF
036700020502  "  C                   eval      Descon = CDPdesbrev
036800020502     c                   else
036900020502     c                   clear                   descon
037000020502     C                   end
037100020502     c* banca d'appoggio  INDICATORE 11 x FACTOR
037200020502     c                   if        rcokscfact <> *blanks
037300020502     c                   seton                                        11
037400020502     c                   eval      rcoksc = RCOkscfact
037500020502     C     Krco          CHAIN     anrco98j
037600020502     c                   if        %found(anrco98j)
037700020502     C                   MOVEL     sogdes        desfac
037701070606     C                   MOVEL     rcoksc        codfac
037800020502     c                   end
037900020502     c                   else
038000020502     c                   setoff                                       11
038100020502     c*banca d'appoggio in alternativa al factor
038200020502     C     KCBA          CHAIN     ANCBA01L
038300020502     c                   if        %found(anCBA01L) AND
038400020502    >C                             CBAABI <> *Blank AND
038500020502    >C                             CBACAB <> *Blank
038600020502    >C                   Move      CBAABI        X59ABI
038700020502    >C                   Move      CBACAB        X59CAB
038800020502A0692C                   Move      XSCSOC        X59SOC
038900020502    >C                   Call      'X59ABI'
039000020502    >C                   Parm                    X59ABIDS
039100020502     C*
039200020502    >C                   If        X59Err = '1'
039300090928    >C                   CLEAR                   iban
039301090928    >C                   CLEAR                   ABI
039400020502    >C                   CLEAR                   CAB
039500020503    >C                   CLEAR                   nrcc
039600020502    >C                   CLEAR                   DESIST
039700020502    >C                   CLEAR                   DESAGE
039800020502    >C                   Else
039900020502     C                   Movel     X59DesIst     DESIST
040000020502     C                   Movel     X59DesAge     DESAGE
040100090928     C                   Movel     CBAiban       iban
040101090928     C                   Movel     CBAABI        ABI               5
040200090928     C                   Movel     CBACAB        CAB               5
040300090928     C                   Movel     CBACcB        nrcc             18
040400020502    >C                   End
040500020502     c                   end
040600020502     c                   end
040700020502     C*
040800020423     C                   end
040900020419      *
041000020419     C                   ENDSR
041001070606      *-----------------------------------------------------***********
041002070606      * memorizzo storico testata anagrafico fornitore solo se no copia iva
041003070606      *-----------------------------------------------------***********
041004070606     C     SRanf0        BEGSR
041011070606     c                   movel(p)  f54tip        ANFTIP
041014070607     c                   eval      ANFTsr = 'O'
041015070607     c                   eval      ANFTrc = '0'
041016070606     c                   eval      ANFSOC = f54soc
041017070905     c                   eval      ANFCDF = salfor
041018070606     c                   eval      ANFNFF = ivanrdoc
041019070606     c                   move      ivadtdoc      ANFDFT
041020070606     c                   clear                   anfpdr
041021070606     c                   clear                   danf0
041022070606     c     kanf0         chain     tnanf01l
041023070606     c                   if        f54uni <>'999     '
041024091111     c                   eval      §ANF0telex = csemi
041025091111     c                   eval      §ANF0prov  = premi
041026070606     c                   eval      §ANF0DES = desemi
041027070606     c                   eval      §ANF0IND = indemi
041028070606     c                   eval      §ANF0CAP = capemi
041029070606     c                   eval      §ANF0LOC  = locemi
041030070606     c                   eval      §ANF0PI   = piemi
041031070606     c                   eval      §ANF0CF   = cfemi
041032070606     c                   eval      §ANF0CDP  = frncondpag
041033070606     c                   eval      §ANF0DCP  = descon
041034070606     c                   if        *in11
041035070606     c                   eval      §ANF0FAC  = codfac
041036070606     c                   eval      §ANF0DFAC = desfac
041037070606     c                   else
041038090928     c                   eval      §ANF0iban = iban
041039070606     c                   eval      §ANF0ABI  = abi
041040070606     c                   eval      §ANF0CAB  = cab
041041070606     c                   eval      §ANF0NRC  = nrcc
041042070606     c                   eval      §ANF0IST  = desist
041043070606     c                   eval      §ANF0AGE  = desage
041044070606     c                   end
041045070606     c                   eval      §ANF0NRP  = nrprt
041046070606     c                   move      ivadtreg      §ANF0DRP
041047070606     c                   eval      anfuni = danf0
041048070606     c                   if        %found(tnanf01l)
041049070606     c                   update    tnanf000
041050070606     c                   else
041051070606     c                   write     tnanf000
041052070606     c                   end
041053070606     c                   else
041054070606     c* imposto i dati di stampa come memorizzati
041055070606     c                   if        %found(tnanf01l)
041056070606     c                   eval      danf0 = anfuni
041057091111     c                   eval      csemi = §ANF0telex
041059070606     c                   eval      desemi      = §ANF0DES
041060070606     c                   eval      indemi      = §ANF0IND
041061070606     c                   eval      capemi      = §ANF0CAP
041062070606     c                   eval      locemi     = §ANF0LOC
041063091119     c                   eval      premi = §ANF0prov
041064070606     c                   eval      piemi      = §ANF0PI
041065070606     c                   eval      cfemi      = §ANF0CF
041066091119     C                   if        cfemi <> ' '
041067091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
041068091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
041069091119     C                             ': ' + %trimr(cfemi)
041070091119     c                   else
041071091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
041072091119     c                   end
041073070606     c                   eval      descon     = §ANF0DCP
041074090928     c                   eval      iban       = §ANF0iban
041075070606     c                   eval      abi        = §ANF0ABI
041076070606     c                   eval      cab        = §ANF0CAB
041077070606     c                   eval      nrcc       = §ANF0NRC
041078070606     c                   eval      desist     = §ANF0IST
041079070606     c                   eval      desage     = §ANF0AGE
041080070606     c                   eval      desfac    = §ANF0DFAC
041081070606     c                   end
041082070606     c                   end
041083070606     C                   ENDSR
041084070606      *-----------------------------------------------------***********
041085070606      * memorizzo storico righe anagrafico fornitore solo se no copia iva
041086070606      *-----------------------------------------------------***********
041087070606     C     SRanf1        BEGSR
041088070606     c                   eval      ANFTIP = f54tip
041089070607     c                   eval      ANFTsr = 'O'
041090070607     c                   eval      ANFTrc = '1'
041091070606     c                   eval      ANFSOC = f54soc
041092070606     c                   eval      ANFCDF = cdfs
041093070606     c                   eval      ANFNFF = ivanrdoc
041094070606     c                   move      ivadtdoc      ANFDFT
041095070606     c                   eval      ANFpdr = pdrs
041096070606     c                   clear                   danf1
041097070606     c     kanf0         chain     tnanf01l
041098070606     c                   if        f54uni <>'999     '
041099070606     c                   eval      §ANF1DES = despdr
041100070606     c                   eval      anfuni = danf1
041101070606     c                   if        %found(tnanf01l)
041102070606     c                   update    tnanf000
041103070606     c                   else
041104070606     c                   write     tnanf000
041105070606     c                   end
041106070606     c                   else
041107070606     c* imposto i dati di stampa come memorizzati
041108070606     c                   if        %found(tnanf01l)
041109070606     c                   eval      danf1 = anfuni
041110070606     c                   eval      despdr      = §ANF1DES
041111070606     c                   end
041112070606     c                   end
041113070606     C                   ENDSR
041114020419      *-----------------------------------------------------***********
041200031205      * decodifico esenzione
041300020419      *-----------------------------------------------------***********
041400031205     C     srese         BEGSR
041500031205     C                   CLEAR                   tabg48
041600031205     C                   CLEAR                   X06DS
041700031205     c                   if        frncdiva <> *blanks
041800031205     C                   MOVE      frncdiva      X06CIV
041900031205     c                   else
042000031205     c                   if        §4zivc <> ' '
042100031205     C                   MOVE      §4zivc        X06CIV
042200031205     c                   end
042300031205     c                   end
042400031205     C                   MOVE      xscsoc        X06SOC
042500031205     C                   MOVE      XSCLIN        X06LIN
042600031205     C                   MOVE      f54dti        X06DAT
042700031205     C                   CALL      'X06G48'
042800031205     C                   PARM                    X06DS
042900031205     C     X06ERR        IFEQ      '0'
043000031205     C                   MOVEL     X06UNI        TABG48
043100031205     C                   end
043200031205      *
043300031205     C                   ENDSR
043400031205      *-----------------------------------------------------***********
043500031205      * Campi per stampa
043600031205      *-----------------------------------------------------***********
043700031205     C     SRCAMPI       BEGSR
043800030827     C                   Z-ADD     Tots          imptg
043900020422     C                   add       imptg         imppdr
044000020419     c                   move      ddcs          dataiso
044100020419     c                   move      dataiso       dataeur
044200020422     c                   move      dataeur       DTDDC
044300031127     c                   movel     nots          note
044400020419     C                   ENDSR
044500030827     C**************************************************************************
044600030827     C* Gestione stampa
044700030827     C**************************************************************************
044800030827     C     LISTA         BEGSR                                                  *
044900020509     c                   if        conta>60 or *in23
045000020422     c                   seton                                        3007
045100020422     c                   z-add     1             conta
045200020422     c                   end
045201070606     c* memorizzo nello storico anagrafiche
045202070606     c   30              exsr      sranf0
045300020422     c* testata fattura
045400020422     c   30              write     testa
045500020509     c                   setoff                                       3023
045600020508     c* testata lista fatture emesse
045700020508     c                   if        f54lis = 'S' and *inof
045800020508     c                   except    teste
045900020508     c                   setoff                                       of
046000020508     c                   end
046100020422     c* totale padroncino
046200020422     c   03              write     totpdr
046300020509     c* totale FATTURA FORNITORE
046400020422     c   05              write     totGEN
046500020509     c* dati finali
046600020701     c*  06              write     totGE1
046700020531     c* riga dettaglio fatture emesse solo per fornitori in autofatturazione
046800020531     c                   if        f54lis = 'S' and *in05 and
046900020531     c                             §tlzfl1 = ' '
047000020422     c                   except    det
047100020422     c                   end
047101070606     c* memorizzo storico righe anagrafico x autofat.
047102070606     c                   if        *in01 and despdr <> ' '
047103070606     c                   exsr      sranf1
047104070606     c                   end
047200020422     c* dettaglio
047300020422     c   01              write     riga
047400020422     C   01              MOVEL     *BLANKS       DESPDR
047500020422     C   01              setoff                                       07
047600020422     c                   movea     '000000'      *in(01)
047700020422     c                   ENDSR
047800020527?     *--------------------------------------------------------------*
047900020527?     *  lettura tabella Y4Z                                         *
048000020527?     *--------------------------------------------------------------*
048100020527     C     sry4z         BEGSR
048200020527     c*  lettura tabella Y4Z
048300020527     c                   clear                   xatbds
048400031124     c                   movel     §caftab       xtbkey
048500020527     c                   move      '1'           xtbric
048600020527     c                   move      'Y4Z'         xtbcod
048700020527     c                   call      'XATB'
048800020527     c                   parm                    xatbds
048900020527     c                   if        xtberr = '0'
049000020527     c                   movel     xtbuni        angy4z
049100020527     c                   else
049200020527     c                   clear                   angy4z
049300020527     c                   end
049400031205     c     kope          chain     anope01l
049500031205     c                   if        %found(anope01l) and opetpreg = '1'
049600031205     c                   select
049700031205     c                   when      opetpdoci = '0'
049800031205     c                   eval      ptpdoc = 'FATTURA'
049900031205     c                   when      opetpdoci = '1'
050000031205     c                   eval      ptpdoc = 'NOTA CREDITO'
050100031205     c                   endsl
050200031205     c                   end
050300020527     C*
050400020527     C                   ENDSR
050500011031     C**********************************************************************
050600910521     C     DEFCAM        BEGSR
050700011031     C**********************************************************************
050800000000     C     *ENTRY        PLIST
050900000000     C                   PARM                    KPJBA
051000020419     C                   MOVEL     KPJBU         ficn54ds
051100031124     c* aggancio la tabella della causale
051200031127     c                   eval      txtlingua = '1'
051300031127     c                   eval      txtposrig = 'T'
051400031124     c                   movel     'CAF'         tbecod
051500031124     c                   movel     f54cau        tbeke1
051600031124     c     ktbe          chain     tntbe01l
051700031124     c                   if        %found(tntbe01l)
051800031124     c                   movel     tbeuni        dcaf
051900031127     c     ktxt          setll     antxt01l
052000031127     c                   do        3             x                 2 0
052100031127     c     ktxt          reade     antxt01l
052200031127     c                   if        %eof(antxt01l)
052300031127     c                   leave
052400031127     c                   end
052500031127     c                   select
052600031127     c                   when      x = 1
052700031127     c                   eval      testo1 = txttesrig
052800031127     c                   when      x = 2
052900031127     c                   eval      testo2 = txttesrig
053000031127     c                   when      x = 3
053100031127     c                   eval      testo3 = txttesrig
053200031127     c                   endsl
053300031127     c                   enddo
053400031124     c                   else
053500031124     c                   clear                   dcaf
053600031124     c                   end
053700020419     C*---------- RICERCA DITTA :
053800020419     C                   MOVEL     'SOC001'      TIPXSC
053900020422     C                   MOVEL     f54soc        SOCXSC
054000020422     c     f54lis        comp      'S'                                    of
054100020419     C                   EXSR      REPSOC
054200020419     C     RTNXSC        IFNE      '1'
054300020419     C                   MOVEL     XSOCDS        SOC001
054400020422     C                   MOVEL     xscrgs        RSUT             20
054500020422     c                   eval      desric = xscrgs
054600020422     c                   eval      indric = xscvia
054700020422     c                   eval      capric = xsccap
054800031112     c                   eval      piric = xsciva
054801061121     c                   eval      cfric = XSCCDF
054802070608     c     cfric         comp      ' '                                8989
054900020422     c                   if        xscprv = ' '
055000020422     C                   eval      locric = xsccit
055100020422     C                   ELSE
055200020422     C                   eval      locric = %trimr(xsccit)+' ('+xscprv+')'
055300020419     c                   end
055400020422     c                   end
055500020419      *  Definizione chiave di accesso
055501070606     c     kanf0         klist
055502070606     c                   KFLD                    ANFTIP
055503070607     c                   KFLD                    ANFTSR
055504070606     c                   KFLD                    ANFTrc
055505070606     c                   KFLD                    ANFSOC
055506070606     c                   KFLD                    ANFCDF
055507070606     c                   KFLD                    anfdft
055508070606     c                   KFLD                    ANFNFF
055509070606     c                   KFLD                    ANFpdr
055600031124     C     Kapd          KLIST
055700031124     C                   KFLD                    f54tip
055800031124     C                   KFLD                    pdrs
055900031127     C     Ktxt          klist
056000031127     C                   kfld                    §cafsoc
056100031127     C                   kfld                    txtunita
056200031127     C                   kfld                    §cafCDtes
056300031127     C                   kfld                    txtlingua
056400031127     C                   kfld                    txtposrig
056500031127     C     Ktbe          KLIST
056600031127     C                   KFLD                    tbecod
056700031127     C                   KFLD                    tbeke1
056800021203     C     Ktlz          KLIST
056900021203     C                   KFLD                    tlztip
057000021203     C                   KFLD                    tlzpdr
057100021203     C                   KFLD                    f54soc
057200031124     C     Kott          KLIST
057300020502     C                   KFLD                    f54soc
057400020502     C                   KFLD                    salfor
057500031124     C                   KFLD                    f54tip
057600020502     C     Kfrn          KLIST
057700020502     C                   KFLD                    f54soc
057800020502     C                   KFLD                    cdfs
057900020419     C     Krco          KLIST
058000020422     C                   KFLD                    f54soc
058100020422     C                   KFLD                    rcosnatura
058200020502     C                   KFLD                    rcoksc
058300020422     c                   eval      rcosnatura = 'F'
058400020422     C     Kiva          KLIST
058500020422     C                   KFLD                    ivasys
058600020422     C                   KFLD                    ivanrasreg
058700020502     c                   eval      ivasys = 0
058800020502    >C     KCBA          KLIST
058900020502    >C                   KFLD                    XSCSOC
059000020502    >C                   KFLD                    CBACLIFOR
059100020502    >C                   KFLD                    KCC
059200020502    >C                   KFLD                    rcoKSC
059300020502    >C                   MOVEL     'F'           CBACLIFOR
059400031205     C     Kope          KLIST
059500031205     C                   KFLD                    xscsoc
059600031205     C                   KFLD                    §4zcrca
059700020419     C                   ENDSR
059800020419     C*----------------------------------------------------*
059900020419     C* Reperimento dati società
060000020419     C*----------------------------------------------------*
060100020419     C     REPSOC        BEGSR
060200020419     C*
060300020419     C                   CALLB     'XSOC'
060400020419     C                   PARM                    TIPXSC            6
060500020419     C                   PARM                    SOCXSC            3
060600020419     C                   PARM                    CDSXSC            9 0
060700020419     C                   PARM                    MODXSC            3
060800020419     C                   PARM      *blanks       RTNXSC            1
060900020419     C                   PARM                    XSOCDS
061000020419     C                   PARM                    KPJBA
061100020419     C*
061200020419     C                   ENDSR
061300031124     Ofiott000  E            aggio
061400031124     o                       ottdcn
061500020502     OQSYSPRT   E            TESTE          2 02
061600020422     O                       RSUT                20
061700031205     O                                         + 25 'LISTA DOCUMENTI EMESSI X '
061800031205     O                                         +  0 'PRESTAZIONI RESIDUALI'
061900031124     O                                          113 'FICN96R'
062000020422     O                       UDATE              127 '  /  /  '
062100020422     O                       PAGE1         Z    132
062200020422     O          E            teste       1  2
062300020503     o                                         +  0 'Fornitore'
062400021010     o                                         + 46 'Fattura'
062500020503     o                                         +  1 'Data'
062600020503     o                                         +  8 'Firma'
062700021010     o                                         + 37 'Data consegna'
062800020503     O          E            DET         3
062900020503     O                       salfor            +  0
063000020422     O                       desemi            +  1
063100020422     O                       nrfat         4   +  1
063200020503     O                       dtfat             +  1 '  /  /    '
063300020503     O                                         +  2 '__________________________'
063400021010     O                                         +  0 '______________'
063500021010     O                                         +  2 '_______________'
