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
001700070606     ftnanf01L  uF a E           K DISK
001800031124     Fficn96p   O    e             PRINTER OFLIND(*IN23)
001900020422     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
002000900515      *
002100020606     D KPJBA         E DS
002200071120     D ficn99ds      E DS
002300020422?     *  Tabella Y4Z
002400020422     D angy4z        E DS                  extname(angy4zds)
002500070606     D danf0         e DS
002600070606     D danf1         e DS
002700031124     D dcaf          e DS
002800031124     D dtlz01        e DS
002900020528     D xatbds        e DS
003000020422     D soc001        E DS                  EXTNAME(xsoc001ds)
003100020422     D xsocds          DS          1000
003200020502     D*ABI/CAB
003300020502     D X59ABIDS      E DS                  INZ
003400020502  "  D* DS per input condizione di pagamento
003500020502  "  DDVCDPDS        E DS                  INZ
003600020502  "  D* Ds per output
003700020502  "  DDVOCDPDS       E DS                  INZ
003800030827?     *  Tab
003900020422     DTABG48         E DS                  EXTNAME(ANGG48DS)
004000020422     D X06DS         E DS                  EXTNAME(X06G48DS)
004100020422     D  X06SOC       E                     EXTFLD(X06SOCIETA)
004200020422     D  X06CIV       E                     EXTFLD(X06CDIVA)
004300020422     D  X06DAT       E                     EXTFLD(X06DATA)
004400020422     D  X06LIN       E                     EXTFLD(X06LINGUA)
004500900516      *
004600020419     D Ficn54ds      E DS
004700070606     D codfac          S                   like(RCOkscfact)
004800020422     d dataiso         s               d
004900020422     d dataeur         s               d   datfmt(*eur)
005000020422     D conta           S              3  0 inz
005100020422     D comodo          S              9  2 inz
005200020422     D salnra          S                   like(nrass)
005300020527     D ctgans          S                   like(rcoctgan02)
005400020422     D salpdr          S              7  0 inz
005500020422     D salfor          S              8    inz
005600020614     D salfors         S                   like(salfor)
005700020502     D kcc             S                   inz like(rcokcc)
005800071221     D comdta          S                   like(ivadtdoc) inz(d'2007-12-31')
005900031124     D fiott           DS
006000020419     d pdrs                           7  0
006100020419     d ddcs                           8  0
006200020419     d tots                          12  3
006300030827     d nrass                          9  0
006400020422     d cdfs                           8
006500031127     d nots                          35
006600110517     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
006700110517     D ESITO_OK...
006800110517     D                 C                   0
006900110517     ***************************************************************************
007000110517     **
007100110517     ** Dichiarazione prototipi procedure esterne.
007200110517     **
007300110517     ***************************************************************************
007400110517      /DEFINE DFTACTGRP_YES
007500110517     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
007600110517      /UNDEFINE DFTACTGRP_YES
007700110517     ***************************************************************************
007800110517     **
007900110517     ** Definizione strutture dati.
008000110517     **
008100110517      **************************************************************************
008200110517     D tibsSocI0...
008300110517     D               E DS                  prefix(i)
008400110517     D                                     INZ
008500110517     D tibsSocO0...
008600110517     D               E DS                  prefix(o)
008700110517     D                                     INZ
008800110517     ***************************************************************************
008900110517     **
009000110517     ** Definizione variabili modulo/programma.
009100110517     **
009200110517     ***************************************************************************
009300110517     D prmRqsOpCode...
009400110517     D                 S             10A
009500110517     D prmRpyOpCode...
009600110517     D                 S             10A
009700110517     D prmRpyIdMsg...
009800110517     D                 S             10I 0
009900110517     D prmRqsFormato...
010000110517     D                 S             10A
010100110517     D prmRqsDataSize...
010200110517     D                 S             10I 0
010300110517     D prmRpyFormato...
010400110517     D                 S             10A
010500110517     D prmRpyDataSize...
010600110517     D                 S             10I 0
010700011031     ***********************************************************************
010800910521     C                   EXSR      DEFCAM
010900031124     c* Lettura fiott06l ordinato per fornitore padroncino data conteg.
011000031124     c* i record scelti sono in base ai limiti scelti a
011100020419     c* video per fornitore e data protocollo, numero fattura <> 0
011200031124     c*
011300031124     c* l'indicatore 80 mi serve per evitare la doppia ribattitura
011400031124     c* nella stampa della copia IVA
011500031124     c*
011600040217     c* se stampa per libro IVA non controllo la data di stampa
011700040217     c* altrimenti deve essere a 0
011800031124     c                   if        f54uni = '999     '
011900031124     c                   seton                                        80
012000020419     C/EXEC SQL
012100031124     C+ DECLARE A1 CURSOR FOR SELECT ottpdr, OTTDDC, sum(OTTTIM), OTTnra,
012200031127     C+ OTTcdf, OTTNOT FROM fiott06l WHERE OTTSOC = :f54soc and ottcdf
012300031127     C+ between :f54pd1 and :f54pd2 and ottdft between :f54dti and
012400031127     C+ :f54dtf and ottnff <> 0 and otttip = :f54tip and otttab = :f54cau
012500040217     C+ GROUP BY ottcdf, ottnra, ottpdr, ottddc, ottnot ORDER BY ottcdf,
012600040217     C+ ottnra, ottpdr, ottddc, ottnot
012700020419     C/END-EXEC
012800031124     c                   else
012900031124     c                   setoff                                       80
013000031124     C/EXEC SQL
013100031127     C+ DECLARE B1 CURSOR FOR SELECT ottpdr, OTTDDC, sum(OTTTIM), OTTnra,
013200031127     C+ OTTcdf, OTTNOT FROM fiott06l WHERE OTTSOC = :f54soc and ottcdf
013300031127     C+ between :f54pd1 and :f54pd2 and ottdft between :f54dti and
013400031127     C+ :f54dtf and ottnff <> 0 and otttip = :f54tip and otttab = :f54cau
013500031127     C+ and ottdcn = 0 GROUP BY ottcdf, ottnra, ottpdr, ottddc, ottnot
013600031127     C+ ORDER BY ottcdf, ottnra, ottpdr, ottddc, ottnot
013700031124     C/END-EXEC
013800031124     c                   end
013900020419
014000031124     c                   if        f54uni = '999     '
014100020419     C/EXEC SQL
014200020419     C+ OPEN A1
014300020419     C/END-EXEC
014400031124     c                   else
014500031124     C/EXEC SQL
014600031124     C+ OPEN b1
014700031124     C/END-EXEC
014800031124     c                   end
014900020419
015000020419     C                   DOU       SqlCod <> 0
015100031124     c                   if        f54uni = '999     '
015200020419     C/EXEC SQL
015300031124     C+ FETCH NEXT FROM A1 INTO :fiott
015400020419     C/END-EXEC
015500031124     c                   else
015600031124     C/EXEC SQL
015700031124     C+ FETCH NEXT FROM b1 INTO :fiott
015800031124     C/END-EXEC
015900031124     c                   end
016000020419     c*
016100020419     C                   SELECT
016200020419     C                   WHEN      SqlCod = 0
016300020614     c* se nr.registrazione = * blanks aggiorno data stampa ed esco
016400030827     c                   if        nrass = 0
016500020614     c                   movel     salfor        salfors
016600020614     c                   movel     cdfs          salfor
016700020614     c                   exsr      sragg
016800020614     c                   movel     salfors       salfor
016900020531     c                   iter
017000020531     c                   end
017100020419     c* rottura fornitore
017200020422     c                   if        cdfs <> salfor or nrass <> salnra
017300020419     c                   exsr      srrotfor
017400020423     c* se la fattura è = 0 leggo un altro fornitore
017500020423     c                   if        impfat = 0
017600020423     c                   z-add     0             num               3 0
017700020423     c                   z-add     0             imppdr
017800020423     c                   iter
017900020423     c                   end
018000020419     c                   end
018100020422     c* rottura padroncino
018200020419     c                   if        pdrs <> salpdr
018300020419     c                   exsr      srrotpdr
018400020419     c                   end
018500020422     c* dettaglio campi
018600020422     C                   EXSR      srcampi
018700020701     c* stampo la riga solo se il totale è <> 0
018800020701     c                   if        imptg <> 0
018900020701     C                   ADD       1             NUM
019000020422     c                   add       1             conta
019100020422     c                   seton                                        01
019200020422     c                   exsr      lista
019300020701     c                   end
019400020422     c*
019500020419     C                   WHEN      SqlCod = 100
019600020419     C                   WHEN      SqlCod <> 0
019700020419      * Forzo la stampa del JOBLOG.
019800020419     C                   CALL      'X66CHGJOB'
019900020419     C                   ENDSL
020000020419     C                   ENDDO
020100031124     c                   if        f54uni = '999     '
020200020419     C/EXEC SQL
020300020419     C+ CLOSE A1
020400020419     C/END-EXEC
020500031124     c                   else
020600031124     C/EXEC SQL
020700031124     C+ CLOSE b1
020800031124     C/END-EXEC
020900031124     c                   end
021000020422     C* gestisco gli ultimi totali
021100020423     c                   if        impfat <> 0
021200020422     c                   exsr      srrotfor
021300020423     c                   end
021400110517     ** Chiudo il programma.
021500110517     C                   CALL      'TIBSSOCR'
021600110517     C                   PARM      'FINALIZE'    prmRqsOpCode
021700110517     C                   PARM                    prmRpyOpCode
021800110517     C                   PARM                    prmRpyIdMsg
021900110517     c*
022000020422     c*
022100910521     C                   SETON                                        LR
022200020419     C**************************************************************************
022300020419     C* Rottura fornitore
022400020419     C**************************************************************************
022500020419     C     Srrotfor      BEGSR                                                  *
022600020419     c* rotture precedenti
022700020419     c                   exsr      srrotpdr
022800020419     c* stampa la riga di totale
022900020419     c                   if        impfat <> 0
023000020509     c                   seton                                        05
023100020423     c                   else
023200020423     c                   z-add     0             conta
023300020419     c                   end
023400020419     c*
023500020701     c                   if        conta <> 60 and conta <> 0
023600020701     c* salto pagina per avere il totale fattura tutto insieme
023700040330     c                   if        conta > 24
023800020701     c                   seton                                        23
023900020701     c                   end
024000020701     c                   exsr      lista
024100020502     c* aggiorno la data di stampa
024200020614     c                   exsr      sragg
024300020701     c                   end
024400020422     c*
024500020419     c                   z-add     60            conta
024600020509     c                   setoff                                       23
024700020422     c* azzera il conteggio per fornitore
024800020502     c                   z-add     0             page
024900020502     c                   z-add     0             impon
025000020422     c                   z-add     0             impiva
025100020422     c                   z-add     0             impfat
025200020419     c* decodifico fornitore
025300020419     c                   exsr      srfor
025400020423     c* aggancio registrazione solo se fornitore ok
025500020423     c                   if        not *in20
025600020422     c                   move      nrass         ivanrasreg
025700020422     c     kiva          chain     ndiva00f
025800020422     c                   if        %found(ndiva00f)
025900020422     c                   move      ivadtdoc      dataeur
026000020422     c                   move      dataeur       dtfat
026100020422     c                   move      ivanrdoc      nrfat
026200020422     c                   move      ivadtreg      dataeur
026300020422     c                   move      dataeur       dtprt
026400020422     c                   move      ivanrreg      nrprt
026500161116     c                   move      ivalibro      ivalib
026600161116     c                   Z-ADD(h)  ivaimporto    impiva
026700161116     c                   Z-ADD(h)  ivaimponib    impon
026800020422     c     impon         add       impiva        impfat
026900020422     c                   end
027000020423     c                   end
027100020531     c                   if        impfat <> 0
027200020531     c                   move      cdfs          salfor
027300020531     c                   move      nrass         salnra
027400020531     c                   end
027500020419     c*
027600020419     c                   ENDSR
027700020419     C**************************************************************************
027800031124     C* aggiorna data di stampa in FIOTT00F
027900020419     C**************************************************************************
028000020614     C     sragg         BEGSR                                                  *
028100020614     c*
028200031124     c     kott          setll     fiott06l
028300020614     c                   do        *hival
028400031124     c     kott          reade     fiott06l
028500031124     c                   if        %eof(fiott06l)
028600020614     c                   leave
028700020614     c                   end
028800020614     c* escludo quelli non fatturati oppure già stampati
028900031124     c                   if        ottnff = 0 or ottdcn <> 0
029000020614     c                   iter
029100020614     c                   end
029200020614      * controllo numero registrazione
029300031124     c                   if        ottnra <> salnra
029400020614     c                   iter
029500020614     c                   end
029600020614      *
029700020614     c                   move      *date         dataiso
029800031124     c                   move      dataiso       ottdcn
029900020614     c                   except    aggio
030000020614     c                   enddo
030100071120     c* stampo la lettera x fusione solo se data fattura = 31/12/2007
030200071221     c                   if        ivadtdoc = comdta and
030300071221     c                             f54uni <> '999     '
030400071120     c                   eval      f99socn = f54soc
030500071120     c                   eval      f99kscn = salfor
030600071120     c                   eval      f99tip = 'O'
030700071120     c                   movel(p)  ficn99ds      kpjbu
030800071120     c                   call      'FICN99R'
030900071120     c                   parm                    kpjba
031000071221     c                   end
031100020614     c*
031200020614     c                   ENDSR
031300020614     C**************************************************************************
031400020614     C* Rottura padroncino
031500020614     C**************************************************************************
031600020614     C     Srrotpdr      BEGSR                                                  *
031700020419     c                   seton                                        07
031800020419     c* decodifica padroncino
031900021203     C     kapd          CHAIN     fiapd01L
032000021203     c                   if        %found(fiapd01l)
032100070822     c                   movel(p)  apdpdr        despdr
032200020419     c                   else
032300020419     c                   movel     *blanks       despdr
032400020419    3C                   ENDIF
032500020422     c* stampa la riga di totale padroncino solo se ha più righe
032600020422     c                   eval(h)   comodo = imppdr
032700020422     c                   if        num > 1  and comodo <> impon
032800020419     c                   seton                                        03
032900020419     c                   add       1             conta
033000020419     c                   exsr      lista
033100020419     c                   end
033200020419     c* azzera il conteggio per padroncino
033300020419     c                   z-add     0             num               3 0
033400020419     c                   z-add     0             imppdr
033500020419     c                   move      pdrs          salpdr
033600020419     c*
033700020419     c                   endsr
033800020419?     *--------------------------------------------------------------*
033900020419?     *  dati in testata                                             *
034000020419?     *--------------------------------------------------------------*
034100020419     C     srfor         BEGSR
034200020419      *
034300020502     c                   eval      rcoksc = cdfs
034400020422     C     Kfrn          CHAIN     anfrn01l                           20
034500020422     C  N20Krco          CHAIN     anrco98j                           20
034600020423     c* controllo se il fornitore appartiene all'unità di reg. scelta
034700020423     c* se 999 tutte le unità
034800020423     c                   if        not *in20 and f54uni <> '999     '
034900020423     c                             and rcounita <> f54uni
035000020423     c                   seton                                        20
035100020423     c                   end
035200020528     c* se 999 tutte le unità (vuol dire che sono in sede) controllo
035300020528     c* che il fornitore sia in autofatturazione
035400090707     C                   CLEAR                   DTLZ01
035500020531     c                   if        not *in20
035600050125     c                   select
035700050125     c                   when      f54tip='A'
035800050125     c                   movel(p)  'P'           tlztip
035900050125     c                   when      f54tip='C'
036000050125     c                   movel(p)  'C'           tlztip
036100050125     c                   when      f54tip='D'
036200050125     c                   movel(p)  'D'           tlztip
036300050125     c                   endsl
036400020528     c                   move      cdfs          tlzpdr
036500020528     c     ktlz          chain     tntlz01l
036600020528     c                   if        %found(tntlz01l)
036700020528     c                   movel     tlzflo        dtlz01
036800020531     c                   if        §tlzfl1 = 'S'
036900020531     c                             and f54uni = '999     '
037000020528     c                   seton                                        20
037100020528     c                   end
037200020528     c                   end
037300020528     c                   end
037400020423     c*
037500020423     c                   if        not *in20
037600091109     C                   if        %subst(indtelex: 1: 8) = 'CAP.SOC.'
037700091109     c                   eval      csemi = indtelex
037800091109     c                   else
037900091201     c* per l'ottico non può essere blanks
038000091201     c                   eval      csemi ='.'
038100091109     c                   end
038200020419     C                   MOVEL     sogdes        desemi
038300020422     C                   eval      indemi = indindriz
038400020422     c                   if        indprov <> ' '
038500020422     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
038600020422     c                   else
038700020422     C                   eval      locemi = indlocalit
038800020422     c                   end
038900020422     C                   eval      capemi = indcap
039000091119     c*
039100091119     C                   movel     sogpartiva    piemi            20
039200091119     C                   MOVEL     indprov       premi             2
039300091119     C                   movel     sogcdfisc     cfemi            20
039400161122     c*
039500161122     c                   exsr      Retrieve_PEC
039600091119     c*
039700091119     C                   if        cfemi <> ' '
039800091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
039900091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
040000091119     C                             ': ' + %trimr(cfemi)
040100091119     c                   else
040200091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
040300091119     c                   end
040400031205     c* aggancio la tabella Y4Z con tipo servizio
040500031205     c                   exsr      sry4z
040600020419     c* se il fornitore ha un codice IVA
040700031205     c                   if        frncdiva <> *blanks
040800031205     c                   exsr      srese
040900031205     c                   end
041000031205     c* decodifico aliquota IVA/esenzione
041100031205     c                   clear                   comiva            5
041200031205     c                   clear                   comiva1           5
041300031205     c                   clear                   desiva
041400031205     c* se NON presente in anagrafica fornitore
041500031205     c                   if        frncdiva = *blanks
041600031205     c                   if        §4ZIVA <>0
041700031205     c                   move      §4ziva        comiva
041800020419     c                   else
041900031205     c                   EXSR      SRESE
042000031205     c                   eval      desiva = desg48
042100020419     c                   end
042200031205     c                   else
042300031205     c* se presente in anagrafica fornitore
042400031205     c                   if        ALIG48 <>0
042500031205     c                   move      alig48        comiva
042600031205     c                   else
042700031205     c                   eval      desiva = desg48
042800031205     c                   end
042900031205     c                   end
043000031205     c*
043100031205     c                   if        comiva <> ' '
043200020422     c                   eval      comiva1 = %subst(comiva: 2: 2)+ ',' +
043300020422     c                             %subst(comiva: 4: 2)
043400020422     c                   eval      desiva = 'Aliquota ' + comiva1 + '%'
043500020422     C                   end
043600020502     c* condizioni di pagamento
043700020502  "  C                   reset                   DVCDPDS
043800020502  "  C                   eval      DCPRIC='1'                                   chain
043900020502  "  C                   eval      DCPSOC=XSCSOC
044000020502  "  C                   eval      DCPUNI=frnunita
044100020502  "  C                   eval      DCPVIS='1'                                   lf principale
044200020502  "  C                   eval      DCPALC=*OFF                                  no allocazione
044300020502  "  C                   eval      DCPNRK=1                                     numero chiavi
044400020502  "  C                   eval      DCPLIN=xsclin
044500020502  "  C                   eval      DCPCOD=frncondpag
044600020502  "  C                   eval      DCPERR=*OFF
044700020502  "  C                   CALL      'DVCDP'
044800020502  "  C                   PARM                    DVCDPDS
044900020502  "  C                   PARM                    DVOCDPDS
045000020502  "  C                   if        DCPERR=*OFF
045100020502  "  C                   eval      Descon = CDPdesbrev
045200020502     c                   else
045300020502     c                   clear                   descon
045400020502     C                   end
045500020502     c* banca d'appoggio  INDICATORE 11 x FACTOR
045600020502     c                   if        rcokscfact <> *blanks
045700020502     c                   seton                                        11
045800020502     c                   eval      rcoksc = RCOkscfact
045900020502     C     Krco          CHAIN     anrco98j
046000020502     c                   if        %found(anrco98j)
046100020502     C                   MOVEL     sogdes        desfac
046200070606     C                   MOVEL     rcoksc        codfac
046300020502     c                   end
046400020502     c                   else
046500020502     c                   setoff                                       11
046600020502     c*banca d'appoggio in alternativa al factor
046700020502     C     KCBA          CHAIN     ANCBA01L
046800020502     c                   if        %found(anCBA01L) AND
046900020502    >C                             CBAABI <> *Blank AND
047000020502    >C                             CBACAB <> *Blank
047100020502    >C                   Move      CBAABI        X59ABI
047200020502    >C                   Move      CBACAB        X59CAB
047300020502A0692C                   Move      XSCSOC        X59SOC
047400020502    >C                   Call      'X59ABI'
047500020502    >C                   Parm                    X59ABIDS
047600020502     C*
047700020502    >C                   If        X59Err = '1'
047800090928    >C                   CLEAR                   iban
047900090928    >C                   CLEAR                   ABI
048000020502    >C                   CLEAR                   CAB
048100020503    >C                   CLEAR                   nrcc
048200020502    >C                   CLEAR                   DESIST
048300020502    >C                   CLEAR                   DESAGE
048400020502    >C                   Else
048500020502     C                   Movel     X59DesIst     DESIST
048600020502     C                   Movel     X59DesAge     DESAGE
048700090928     C                   Movel     CBAiban       iban
048800090928     C                   Movel     CBAABI        ABI               5
048900090928     C                   Movel     CBACAB        CAB               5
049000090928     C                   Movel     CBACcB        nrcc             18
049100020502    >C                   End
049200020502     c                   end
049300020502     c                   end
049400020502     C*
049500020423     C                   end
049600020419      *
049700020419     C                   ENDSR
049800070606      *-----------------------------------------------------***********
049900070606      * memorizzo storico testata anagrafico fornitore solo se no copia iva
050000070606      *-----------------------------------------------------***********
050100070606     C     SRanf0        BEGSR
050200070606     c                   movel(p)  f54tip        ANFTIP
050300070607     c                   eval      ANFTsr = 'O'
050400070607     c                   eval      ANFTrc = '0'
050500070606     c                   eval      ANFSOC = f54soc
050600070905     c                   eval      ANFCDF = salfor
050700070606     c                   eval      ANFNFF = ivanrdoc
050800070606     c                   move      ivadtdoc      ANFDFT
050900070606     c                   clear                   anfpdr
051000070606     c                   clear                   danf0
051100070606     c     kanf0         chain     tnanf01l
051200070606     c                   if        f54uni <>'999     '
051300091111     c                   eval      §ANF0telex = csemi
051400091111     c                   eval      §ANF0prov  = premi
051500070606     c                   eval      §ANF0DES = desemi
051600070606     c                   eval      §ANF0IND = indemi
051700070606     c                   eval      §ANF0CAP = capemi
051800070606     c                   eval      §ANF0LOC  = locemi
051900070606     c                   eval      §ANF0PI   = piemi
052000070606     c                   eval      §ANF0CF   = cfemi
052100161122     c                   eval      §ANF0pec  = peceml
052200161122     c                   eval      §ANF0lIVA = ivaLIB
052300070606     c                   eval      §ANF0CDP  = frncondpag
052400070606     c                   eval      §ANF0DCP  = descon
052500070606     c                   if        *in11
052600070606     c                   eval      §ANF0FAC  = codfac
052700070606     c                   eval      §ANF0DFAC = desfac
052800070606     c                   else
052900090928     c                   eval      §ANF0iban = iban
053000070606     c                   eval      §ANF0ABI  = abi
053100070606     c                   eval      §ANF0CAB  = cab
053200070606     c                   eval      §ANF0NRC  = nrcc
053300070606     c                   eval      §ANF0IST  = desist
053400070606     c                   eval      §ANF0AGE  = desage
053500070606     c                   end
053600070606     c                   eval      §ANF0NRP  = nrprt
053700070606     c                   move      ivadtreg      §ANF0DRP
053800070606     c                   eval      anfuni = danf0
053900070606     c                   if        %found(tnanf01l)
054000070606     c                   update    tnanf000
054100070606     c                   else
054200070606     c                   write     tnanf000
054300070606     c                   end
054400070606     c                   else
054500070606     c* imposto i dati di stampa come memorizzati
054600070606     c                   if        %found(tnanf01l)
054700070606     c                   eval      danf0 = anfuni
054800091111     c                   eval      csemi = §ANF0telex
054900070606     c                   eval      desemi      = §ANF0DES
055000070606     c                   eval      indemi      = §ANF0IND
055100070606     c                   eval      capemi      = §ANF0CAP
055200070606     c                   eval      locemi     = §ANF0LOC
055300091119     c                   eval      premi = §ANF0prov
055400070606     c                   eval      piemi      = §ANF0PI
055500070606     c                   eval      cfemi      = §ANF0CF
055600091119     C                   if        cfemi <> ' '
055700091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
055800091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
055900091119     C                             ': ' + %trimr(cfemi)
056000091119     c                   else
056100091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
056200091119     c                   end
056300070606     c                   eval      descon     = §ANF0DCP
056400090928     c                   eval      iban       = §ANF0iban
056500070606     c                   eval      abi        = §ANF0ABI
056600070606     c                   eval      cab        = §ANF0CAB
056700070606     c                   eval      nrcc       = §ANF0NRC
056800161122     c                   eval      peceml     = §ANF0pec
056900070606     c                   eval      desist     = §ANF0IST
057000070606     c                   eval      desage     = §ANF0AGE
057100070606     c                   eval      desfac    = §ANF0DFAC
057200070606     c                   end
057300070606     c                   end
057400070606     C                   ENDSR
057500161116      *----------------------------------------------------------------------
057600161116      *   reperisce la PEC se c'è
057700161116      *----------------------------------------------------------------------
057800161116     C     RETRIEVE_PEC  BEGSR
057900161116      *
058000161116     c                   eval        PECEML = 'PEC:'
058100161117     C                   if        cfemi <> ' '
058200161117     C                   eval       prmI_CFEMI = cFEMI
058300161116     C                   clear                   prmO_PECEML
058400161116     C                   CALL      'FICNPECR'
058500161117     C                   PARM                    prmI_CFEMI       20
058600161116     C                   PARM                    prmO_PECEML     100
058700161116      *
058800161116     C                   eval       pecEML = %trim(pecEML) + %trim(prmO_PECEML)
058900161116     c                   end
059000161116      *
059100161116     C                   ENDSR
059200161116      *----------------------------------------------------------------------
059300070606      *-----------------------------------------------------***********
059400070606      * memorizzo storico righe anagrafico fornitore solo se no copia iva
059500070606      *-----------------------------------------------------***********
059600070606     C     SRanf1        BEGSR
059700070606     c                   eval      ANFTIP = f54tip
059800070607     c                   eval      ANFTsr = 'O'
059900070607     c                   eval      ANFTrc = '1'
060000070606     c                   eval      ANFSOC = f54soc
060100070606     c                   eval      ANFCDF = cdfs
060200070606     c                   eval      ANFNFF = ivanrdoc
060300070606     c                   move      ivadtdoc      ANFDFT
060400070606     c                   eval      ANFpdr = pdrs
060500070606     c                   clear                   danf1
060600070606     c     kanf0         chain     tnanf01l
060700070606     c                   if        f54uni <>'999     '
060800070606     c                   eval      §ANF1DES = despdr
060900070606     c                   eval      anfuni = danf1
061000070606     c                   if        %found(tnanf01l)
061100070606     c                   update    tnanf000
061200070606     c                   else
061300070606     c                   write     tnanf000
061400070606     c                   end
061500070606     c                   else
061600070606     c* imposto i dati di stampa come memorizzati
061700070606     c                   if        %found(tnanf01l)
061800070606     c                   eval      danf1 = anfuni
061900070606     c                   eval      despdr      = §ANF1DES
062000070606     c                   end
062100070606     c                   end
062200070606     C                   ENDSR
062300020419      *-----------------------------------------------------***********
062400031205      * decodifico esenzione
062500020419      *-----------------------------------------------------***********
062600031205     C     srese         BEGSR
062700031205     C                   CLEAR                   tabg48
062800031205     C                   CLEAR                   X06DS
062900031205     c                   if        frncdiva <> *blanks
063000031205     C                   MOVE      frncdiva      X06CIV
063100031205     c                   else
063200031205     c                   if        §4zivc <> ' '
063300031205     C                   MOVE      §4zivc        X06CIV
063400031205     c                   end
063500031205     c                   end
063600031205     C                   MOVE      xscsoc        X06SOC
063700031205     C                   MOVE      XSCLIN        X06LIN
063800031205     C                   MOVE      f54dti        X06DAT
063900031205     C                   CALL      'X06G48'
064000031205     C                   PARM                    X06DS
064100031205     C     X06ERR        IFEQ      '0'
064200031205     C                   MOVEL     X06UNI        TABG48
064300031205     C                   end
064400031205      *
064500031205     C                   ENDSR
064600031205      *-----------------------------------------------------***********
064700031205      * Campi per stampa
064800031205      *-----------------------------------------------------***********
064900031205     C     SRCAMPI       BEGSR
065000030827     C                   Z-ADD     Tots          imptg
065100020422     C                   add       imptg         imppdr
065200020419     c                   move      ddcs          dataiso
065300020419     c                   move      dataiso       dataeur
065400020422     c                   move      dataeur       DTDDC
065500031127     c                   movel     nots          note
065600020419     C                   ENDSR
065700030827     C**************************************************************************
065800030827     C* Gestione stampa
065900030827     C**************************************************************************
066000030827     C     LISTA         BEGSR                                                  *
066100020509     c                   if        conta>60 or *in23
066200020422     c                   seton                                        3007
066300020422     c                   z-add     1             conta
066400020422     c                   end
066500070606     c* memorizzo nello storico anagrafiche
066600070606     c   30              exsr      sranf0
066700020422     c* testata fattura
066800020422     c   30              write     testa
066900020509     c                   setoff                                       3023
067000020508     c* testata lista fatture emesse
067100020508     c                   if        f54lis = 'S' and *inof
067200020508     c                   except    teste
067300020508     c                   setoff                                       of
067400020508     c                   end
067500020422     c* totale padroncino
067600020422     c   03              write     totpdr
067700020509     c* totale FATTURA FORNITORE
067800020422     c   05              write     totGEN
067900020509     c* dati finali
068000020701     c*  06              write     totGE1
068100020531     c* riga dettaglio fatture emesse solo per fornitori in autofatturazione
068200020531     c                   if        f54lis = 'S' and *in05 and
068300020531     c                             §tlzfl1 = ' '
068400020422     c                   except    det
068500020422     c                   end
068600070606     c* memorizzo storico righe anagrafico x autofat.
068700070606     c                   if        *in01 and despdr <> ' '
068800070606     c                   exsr      sranf1
068900070606     c                   end
069000020422     c* dettaglio
069100020422     c   01              write     riga
069200020422     C   01              MOVEL     *BLANKS       DESPDR
069300020422     C   01              setoff                                       07
069400020422     c                   movea     '000000'      *in(01)
069500020422     c                   ENDSR
069600020527?     *--------------------------------------------------------------*
069700020527?     *  lettura tabella Y4Z                                         *
069800020527?     *--------------------------------------------------------------*
069900020527     C     sry4z         BEGSR
070000020527     c*  lettura tabella Y4Z
070100020527     c                   clear                   xatbds
070200031124     c                   movel     §caftab       xtbkey
070300020527     c                   move      '1'           xtbric
070400020527     c                   move      'Y4Z'         xtbcod
070500020527     c                   call      'XATB'
070600020527     c                   parm                    xatbds
070700020527     c                   if        xtberr = '0'
070800020527     c                   movel     xtbuni        angy4z
070900020527     c                   else
071000020527     c                   clear                   angy4z
071100020527     c                   end
071200031205     c     kope          chain     anope01l
071300031205     c                   if        %found(anope01l) and opetpreg = '1'
071400031205     c                   select
071500031205     c                   when      opetpdoci = '0'
071600031205     c                   eval      ptpdoc = 'FATTURA'
071700031205     c                   when      opetpdoci = '1'
071800031205     c                   eval      ptpdoc = 'NOTA CREDITO'
071900031205     c                   endsl
072000031205     c                   end
072100020527     C*
072200020527     C                   ENDSR
072300011031     C**********************************************************************
072400910521     C     DEFCAM        BEGSR
072500011031     C**********************************************************************
072600000000     C     *ENTRY        PLIST
072700000000     C                   PARM                    KPJBA
072800020419     C                   MOVEL     KPJBU         ficn54ds
072900031124     c* aggancio la tabella della causale
073000031127     c                   eval      txtlingua = '1'
073100031127     c                   eval      txtposrig = 'T'
073200031124     c                   movel     'CAF'         tbecod
073300031124     c                   movel     f54cau        tbeke1
073400031124     c     ktbe          chain     tntbe01l
073500031124     c                   if        %found(tntbe01l)
073600031124     c                   movel     tbeuni        dcaf
073700031127     c     ktxt          setll     antxt01l
073800031127     c                   do        3             x                 2 0
073900031127     c     ktxt          reade     antxt01l
074000031127     c                   if        %eof(antxt01l)
074100031127     c                   leave
074200031127     c                   end
074300031127     c                   select
074400031127     c                   when      x = 1
074500031127     c                   eval      testo1 = txttesrig
074600031127     c                   when      x = 2
074700031127     c                   eval      testo2 = txttesrig
074800031127     c                   when      x = 3
074900031127     c                   eval      testo3 = txttesrig
075000031127     c                   endsl
075100031127     c                   enddo
075200031124     c                   else
075300031124     c                   clear                   dcaf
075400031124     c                   end
075500020419     C*---------- RICERCA DITTA :
075600020419     C                   MOVEL     'SOC001'      TIPXSC
075700020422     C                   MOVEL     f54soc        SOCXSC
075800020422     c     f54lis        comp      'S'                                    of
075900020419     C                   EXSR      REPSOC
076000020419     C     RTNXSC        IFNE      '1'
076100020419     C                   MOVEL     XSOCDS        SOC001
076200110517     c* decodifico i dati della società
076300110517     c                   exsr      Decod_societa
076400110517     c                   if         PRMRPYIDMSG >= 0
076500110517     c                              and oIDSOCIETA <> *blank
076600110517     c*
076700110517     C                   MOVEL     oRAGSOCIALE   RSUT             20
076800110517     c                   movel     oRAGSOCIALE   desric
076900110517     c                   movel     oSEDLEGIND    indric
077000110517     c                   movel     oSEDLEGCAP    capric
077100110517     c                   movel     oPARTITAIVA   piric
077200110517     c                   movel     oCODFISCALE   cfric
077300070608     c     cfric         comp      ' '                                8989
077400110517     c                   if        oSEDLEGPRO = ' '
077500110517     C                   movel     oSEDLEGLOC    locric
077600110517     C                   ELSE
077700110517     C                   eval      locric = %trimr(osedlegloc)+
077800110517     C                             ' ('+osedlegpro+')'
077900110517     c                   end
078000020422     c                   end
078100110517     c                   end
078200020419      *  Definizione chiave di accesso
078300070606     c     kanf0         klist
078400070606     c                   KFLD                    ANFTIP
078500070607     c                   KFLD                    ANFTSR
078600070606     c                   KFLD                    ANFTrc
078700070606     c                   KFLD                    ANFSOC
078800070606     c                   KFLD                    ANFCDF
078900070606     c                   KFLD                    anfdft
079000070606     c                   KFLD                    ANFNFF
079100070606     c                   KFLD                    ANFpdr
079200031124     C     Kapd          KLIST
079300031124     C                   KFLD                    f54tip
079400031124     C                   KFLD                    pdrs
079500031127     C     Ktxt          klist
079600031127     C                   kfld                    §cafsoc
079700031127     C                   kfld                    txtunita
079800031127     C                   kfld                    §cafCDtes
079900031127     C                   kfld                    txtlingua
080000031127     C                   kfld                    txtposrig
080100031127     C     Ktbe          KLIST
080200031127     C                   KFLD                    tbecod
080300031127     C                   KFLD                    tbeke1
080400021203     C     Ktlz          KLIST
080500021203     C                   KFLD                    tlztip
080600021203     C                   KFLD                    tlzpdr
080700021203     C                   KFLD                    f54soc
080800031124     C     Kott          KLIST
080900020502     C                   KFLD                    f54soc
081000020502     C                   KFLD                    salfor
081100031124     C                   KFLD                    f54tip
081200020502     C     Kfrn          KLIST
081300020502     C                   KFLD                    f54soc
081400020502     C                   KFLD                    cdfs
081500020419     C     Krco          KLIST
081600020422     C                   KFLD                    f54soc
081700020422     C                   KFLD                    rcosnatura
081800020502     C                   KFLD                    rcoksc
081900020422     c                   eval      rcosnatura = 'F'
082000020422     C     Kiva          KLIST
082100020422     C                   KFLD                    ivasys
082200020422     C                   KFLD                    ivanrasreg
082300020502     c                   eval      ivasys = 0
082400020502    >C     KCBA          KLIST
082500020502    >C                   KFLD                    XSCSOC
082600020502    >C                   KFLD                    CBACLIFOR
082700020502    >C                   KFLD                    KCC
082800020502    >C                   KFLD                    rcoKSC
082900020502    >C                   MOVEL     'F'           CBACLIFOR
083000031205     C     Kope          KLIST
083100031205     C                   KFLD                    xscsoc
083200031205     C                   KFLD                    §4zcrca
083300020419     C                   ENDSR
083400020419     C*----------------------------------------------------*
083500020419     C* Reperimento dati società
083600020419     C*----------------------------------------------------*
083700020419     C     REPSOC        BEGSR
083800020419     C*
083900020419     C                   CALLB     'XSOC'
084000020419     C                   PARM                    TIPXSC            6
084100020419     C                   PARM                    SOCXSC            3
084200020419     C                   PARM                    CDSXSC            9 0
084300020419     C                   PARM                    MODXSC            3
084400020419     C                   PARM      *blanks       RTNXSC            1
084500020419     C                   PARM                    XSOCDS
084600020419     C                   PARM                    KPJBA
084700020419     C*
084800020419     C                   ENDSR
084900110517     C**************************************************************************
085000110517     C* decodifica la società
085100110517     C************************************************************
085200110517     C     Decod_societa begSR
085300110517     ** Inizializzo il programma.
085400110517     C                   CALL      'TIBSSOCR'
085500110517     C                   PARM      'INIT'        prmRqsOpCode
085600110517     C                   PARM                    prmRpyOpCode
085700110517     C                   PARM                    prmRpyIdMsg
085800110517      *
085900110517     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
086000110517     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
086100110517     c* forzo società 201
086200110517     C                   EVAL      IidSocieta =f54soc
086300110517     C                   move      F54DTI        IDTVALIDITA
086400110517     C
086500110517     C                   CALL      'TIBSSOCR'
086600110517     C                   PARM      'GETANAGRAF'  prmRqsOpCode
086700110517     C                   PARM      *BLANK        prmRpyOpCode
086800110517     C                   PARM      *ZERO         prmRpyIdMsg
086900110517     C                   PARM      'TIBSSOCI0'   prmRqsFormato
087000110517     C                   PARM                    tibsSocI0
087100110517     C                   PARM                    prmRqsDataSize
087200110517     C                   PARM      'TIBSSOCO0'   prmRpyFormato
087300110517     C                   PARM                    tibsSocO0
087400110517     C                   PARM                    prmRpyDataSize
087500110517      *
087600110517     C                   ENDSR
087700031124     Ofiott000  E            aggio
087800031124     o                       ottdcn
087900020502     OQSYSPRT   E            TESTE          2 02
088000020422     O                       RSUT                20
088100031205     O                                         + 25 'LISTA DOCUMENTI EMESSI X '
088200031205     O                                         +  0 'PRESTAZIONI RESIDUALI'
088300031124     O                                          113 'FICN96R'
088400020422     O                       UDATE              127 '  /  /  '
088500020422     O                       PAGE1         Z    132
088600020422     O          E            teste       1  2
088700020503     o                                         +  0 'Fornitore'
088800021010     o                                         + 46 'Fattura'
088900020503     o                                         +  1 'Data'
089000020503     o                                         +  8 'Firma'
089100021010     o                                         + 37 'Data consegna'
089200020503     O          E            DET         3
089300020503     O                       salfor            +  0
089400020422     O                       desemi            +  1
089500020422     O                       nrfat         4   +  1
089600020503     O                       dtfat             +  1 '  /  /    '
089700020503     O                                         +  2 '__________________________'
089800021010     O                                         +  0 '______________'
089900021010     O                                         +  2 '_______________'
