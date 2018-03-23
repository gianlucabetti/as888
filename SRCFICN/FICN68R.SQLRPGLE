000100020422     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020422     H*PARMS ACTGRP(QILE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400900606?     *--------------------------------------------------------------*
000500030827      * ficn68R   stampa autofattura conteggi aff/defl               *
000600900606?     *--------------------------------------------------------------*
000700020422     FanRCO98J  iF   E           K DISK
000800020419     Fanfrn01L  IF   E           K DISK
000900020502     FanCBA01L  IF   E           K DISK
001000020422     FndIVA00F  IF   E           K DISK
001100021203     Ffiapd01L  IF   E           K DISK
001200020528     Ftntlz01L  IF   E           K DISK
001300030827     Fazorg01l  IF   E           K DISK
001400030827     Ffiatt06L  uF   E           K DISK
001500070606     ftnanf01L  uF a E           K DISK
001600030827     Fficn68p   O    e             PRINTER OFLIND(*IN23)
001700020422     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
001800900515      *
001900020606     D KPJBA         E DS
002000071120     D ficn99ds      E DS
002100020422?     *  Tabella Y4Z
002200020422     D angy4z        E DS                  extname(angy4zds)
002300020528     D dtlz01        e DS
002400020528     D xatbds        e DS
002500020422     D soc001        E DS                  EXTNAME(xsoc001ds)
002600070606     D danf0         e DS
002700070606     D danf1         e DS
002800080922     D DanF2         e DS
002900020422     D xsocds          DS          1000
003000020502     D*ABI/CAB
003100020502     D X59ABIDS      E DS                  INZ
003200020502  "  D* DS per input condizione di pagamento
003300020502  "  DDVCDPDS        E DS                  INZ
003400020502  "  D* Ds per output
003500020502  "  DDVOCDPDS       E DS                  INZ
003600030827?     *  Tab
003700020422     DTABG48         E DS                  EXTNAME(ANGG48DS)
003800020422     D X06DS         E DS                  EXTNAME(X06G48DS)
003900020422     D  X06SOC       E                     EXTFLD(X06SOCIETA)
004000020422     D  X06CIV       E                     EXTFLD(X06CDIVA)
004100020422     D  X06DAT       E                     EXTFLD(X06DATA)
004200020422     D  X06LIN       E                     EXTFLD(X06LINGUA)
004300900516      *
004400020419     D Ficn54ds      E DS
004500070606     D codfac          S                   like(RCOkscfact)
004600020422     d dataiso         s               d
004700020422     d dataeur         s               d   datfmt(*eur)
004800020422     D conta           S              3  0 inz
004900020422     D comodo          S              9  2 inz
005000020422     D salnra          S                   like(nrass)
005100020527     D ctgans          S                   like(rcoctgan02)
005200020422     D salpdr          S              7  0 inz
005300020422     D salfor          S              8    inz
005400020614     D salfors         S                   like(salfor)
005500020502     D kcc             S                   inz like(rcokcc)
005600020419     c* SQL
005700020419     D cmd             s             70    ctdata dim(10)                       FIL. COMODO
005800071221     D comdta          S                   like(ivadtdoc) inz(d'2007-12-31')
005900020419     D WrkSqlCmd       S           1024
006000030827     D fiatt           DS
006100020419     d pdrs                           7  0
006200020419     d ddcs                           8  0
006300090629     d totv                          12  3
006400090629     d tots                          12  3
006500030827     d nrass                          9  0
006600020422     d cdfs                           8
006700030827     d podas                          3  0
006800030827     d poas                           3  0
006900110502     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
007000110502     D ESITO_OK...
007100110502     D                 C                   0
007200110502     ***************************************************************************
007300110502     **
007400110502     ** Dichiarazione prototipi procedure esterne.
007500110502     **
007600110502     ***************************************************************************
007700110502      /DEFINE DFTACTGRP_YES
007800110502     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
007900110502      /UNDEFINE DFTACTGRP_YES
008000110502     ***************************************************************************
008100110502     **
008200110502     ** Definizione strutture dati.
008300110502     **
008400110502      **************************************************************************
008500110502     D tibsSocI0...
008600110502     D               E DS                  prefix(i)
008700110502     D                                     INZ
008800110502     D tibsSocO0...
008900110502     D               E DS                  prefix(o)
009000110502     D                                     INZ
009100110502     ***************************************************************************
009200110502     **
009300110502     ** Definizione variabili modulo/programma.
009400110502     **
009500110502     ***************************************************************************
009600110502     D prmRqsOpCode...
009700110502     D                 S             10A
009800110502     D prmRpyOpCode...
009900110502     D                 S             10A
010000110502     D prmRpyIdMsg...
010100110502     D                 S             10I 0
010200110502     D prmRqsFormato...
010300110502     D                 S             10A
010400110502     D prmRqsDataSize...
010500110502     D                 S             10I 0
010600110502     D prmRpyFormato...
010700110502     D                 S             10A
010800110502     D prmRpyDataSize...
010900110502     D                 S             10I 0
011000110502
011100011031     ***********************************************************************
011200910521     C                   EXSR      DEFCAM
011300030827     c* Lettura fiatt06l ordinato per fornitore padroncino data conteg.
011400030827     c* i record scelti sono: attdco<> 0 limiti scelti a
011500020419     c* video per fornitore e data protocollo, numero fattura <> 0
011600020419     c*
011700020419     C/EXEC SQL
011800020419     C+ PREPARE S1 FROM :WrkSqlCmd
011900020419     C/END-EXEC
012000020419
012100020419     C/EXEC SQL
012200020419     C+ DECLARE A1 CURSOR FOR S1
012300020419     C/END-EXEC
012400020419
012500020419     C/EXEC SQL
012600020419     C+ OPEN A1
012700020419     C/END-EXEC
012800020419
012900020419     C                   DOU       SqlCod <> 0
013000020419     C/EXEC SQL
013100030827     C+ FETCH NEXT FROM A1 INTO :fiatt
013200020419     C/END-EXEC
013300020419     c*
013400020419     C                   SELECT
013500020419     C                   WHEN      SqlCod = 0
013600020614     c* se nr.registrazione = * blanks aggiorno data stampa ed esco
013700030827     c                   if        nrass = 0
013800020614     c                   movel     salfor        salfors
013900020614     c                   movel     cdfs          salfor
014000020614     c                   exsr      sragg
014100020614     c                   movel     salfors       salfor
014200020531     c                   iter
014300020531     c                   end
014400020419     c* rottura fornitore
014500020422     c                   if        cdfs <> salfor or nrass <> salnra
014600020419     c                   exsr      srrotfor
014700020423     c* se la fattura è = 0 leggo un altro fornitore
014800020423     c                   if        impfat = 0
014900020423     c                   z-add     0             num               3 0
015000020423     c                   z-add     0             imppdr
015100020423     c                   iter
015200020423     c                   end
015300020419     c                   end
015400020422     c* rottura padroncino
015500020419     c                   if        pdrs <> salpdr
015600020419     c                   exsr      srrotpdr
015700020419     c                   end
015800020422     c* dettaglio campi
015900020422     C                   EXSR      srcampi
016000020701     c* stampo la riga solo se il totale è <> 0
016100020701     c                   if        imptg <> 0
016200020701     C                   ADD       1             NUM
016300081003     c*******            add       1             conta
016400020422     c                   seton                                        01
016500020422     c                   exsr      lista
016600020701     c                   end
016700020422     c*
016800020419     C                   WHEN      SqlCod = 100
016900020419     C                   WHEN      SqlCod <> 0
017000020419      * Forzo la stampa del JOBLOG.
017100020419     C                   CALL      'X66CHGJOB'
017200020419     C                   ENDSL
017300020419     C                   ENDDO
017400020419     C/EXEC SQL
017500020419     C+ CLOSE A1
017600020419     C/END-EXEC
017700020422     C* gestisco gli ultimi totali
017800020423     c                   if        impfat <> 0
017900081001     c                   move      'N'           stampa_Corr       1
018000020422     c                   exsr      srrotfor
018100020423     c                   end
018200110502     ** Chiudo il programma.
018300110502     C                   CALL      'TIBSSOCR'
018400110502     C                   PARM      'FINALIZE'    prmRqsOpCode
018500110502     C                   PARM                    prmRpyOpCode
018600110502     C                   PARM                    prmRpyIdMsg
018700020422     c*
018800910521     C                   SETON                                        LR
018900020419     C**************************************************************************
019000020419     C* Rottura fornitore
019100020419     C**************************************************************************
019200020419     C     Srrotfor      BEGSR                                                  *
019300081001      *
019400081001      * emissione dicitura : Corrispettivi......
019500081001     c****               if        stampa_Corr <> 'N'
019600081001     c****               setoff                                       23
019700081001     c****               end
019800020419     c* rotture precedenti
019900020419     c                   exsr      srrotpdr
020000081002      *
020100020419     c* stampa la riga di totale
020200020419     c                   if        impfat <> 0
020300020509     c                   seton                                        05
020400020423     c                   else
020500020423     c                   z-add     0             conta
020600020419     c                   end
020700020419     c*
020800020701     c                   if        conta <> 60 and conta <> 0
020900020701     c* salto pagina per avere il totale fattura tutto insieme
021000081003     c*****              if        conta > 24
021100081003     c                   if        conta > 29
021200081001     c                   seton                                          24
021300081001     c******             seton                                        2324
021400020701     c                   end
021500081002     c*
021600020701     c                   exsr      lista
021700020502     c* aggiorno la data di stampa
021800020614     c                   exsr      sragg
021900020701     c                   end
022000020422     c*
022100020419     c                   z-add     60            conta
022200081001     c******             setoff                                       23
022300081001     c                   setoff                                       24
022400081002     c*
022500020422     c* azzera il conteggio per fornitore
022600020502     c                   z-add     0             page
022700020502     c                   z-add     0             impon
022800020422     c                   z-add     0             impiva
022900020422     c                   z-add     0             impfat
023000020419     c* decodifico fornitore
023100020419     c                   exsr      srfor
023200081001     c                   setoff                                       23
023300020423     c* aggancio registrazione solo se fornitore ok
023400020423     c                   if        not *in20
023500020422     c                   move      nrass         ivanrasreg
023600020422     c     kiva          chain     ndiva00f
023700020422     c                   if        %found(ndiva00f)
023800020422     c                   move      ivadtdoc      dataeur
023900020422     c                   move      dataeur       dtfat
024000020422     c                   move      ivanrdoc      nrfat
024100020422     c                   move      ivadtreg      dataeur
024200020422     c                   move      dataeur       dtprt
024300020422     c                   move      ivanrreg      nrprt
024400161116     c                   move      ivalibro      ivalib
024500161116     c                   Z-ADD(h)  ivaimporto    impiva
024600161116     c                   Z-ADD(h)  ivaimponib    impon
024700020422     c     impon         add       impiva        impfat
024800020422     c                   end
024900020423     c                   end
025000020531     c                   if        impfat <> 0
025100020531     c                   move      cdfs          salfor
025200020531     c                   move      nrass         salnra
025300020531     c                   end
025400020419     c*
025500020419     c                   ENDSR
025600020419     C**************************************************************************
025700030827     C* aggiorna data di stampa in FIATT00F
025800020419     C**************************************************************************
025900020614     C     sragg         BEGSR                                                  *
026000020614     c*
026100030828     c                   eval      attflg = 'C'
026200030827     c     katt          setll     fiatt06l
026300020614     c                   do        *hival
026400030828     c     katt          reade     fiatt06l
026500030827     c                   if        %eof(fiatt06l)
026600020614     c                   leave
026700020614     c                   end
026800020614     c* escludo quelli non fatturati oppure già stampati
026900030827     c                   if        attnff = 0 or attdcn <> 0
027000020614     c                   iter
027100020614     c                   end
027200020614      * controllo numero registrazione
027300030827     c                   if        attnra <> salnra
027400020614     c                   iter
027500020614     c                   end
027600020614      *
027700020614     c                   move      *date         dataiso
027800030827     c                   move      dataiso       attdcn
027900020614     c                   except    aggio
028000020614     c                   enddo
028100071120     c* stampo la lettera x fusione solo se data fattura = 31/12/2007
028200071221     c                   if        ivadtdoc = comdta and
028300071221     c                             f54uni <> '999     '
028400071120     c                   eval      f99socn = f54soc
028500071120     c                   eval      f99kscn = salfor
028600071120     c                   eval      f99tip = 'D'
028700071120     c                   movel(p)  ficn99ds      kpjbu
028800071120     c                   call      'FICN99R'
028900071120     c                   parm                    kpjba
029000071221     c                   end
029100020614     c*
029200020614     c                   ENDSR
029300020614     C**************************************************************************
029400020614     C* Rottura padroncino
029500020614     C**************************************************************************
029600020614     C     Srrotpdr      BEGSR                                                  *
029700020419     c                   seton                                        07
029800020419     c* decodifica padroncino
029900021203     C     kapd          CHAIN     fiapd01L
030000021203     c                   if        %found(fiapd01l)
030100070822     c                   movel(p)  apdpdr        despdr
030200020419     c                   else
030300020419     c                   movel     *blanks       despdr
030400020419    3C                   ENDIF
030500020422     c* stampa la riga di totale padroncino solo se ha più righe
030600020422     c                   eval(h)   comodo = imppdr
030700080926     c******  Prima saltava il totale Autotrasp. se era 1 solo
030800080926     c******   adesso con la stampa dell'importo carburante legge 133/08
030900080926     c******   deve sempre stampare il totale Autotrasp.
031000080926     c******             if        num > 1  and comodo <> impon
031100081006     c                   if        num>=1 and comodo <> 0
031200020419     c                   seton                                        03
031300081002     c******             add       1             conta
031400020419     c                   exsr      lista
031500080926     c                   end
031600020419     c* azzera il conteggio per padroncino
031700020419     c                   z-add     0             num               3 0
031800020419     c                   z-add     0             imppdr
031900020419     c                   move      pdrs          salpdr
032000080926     c*
032100020419     c                   endsr
032200020419?     *--------------------------------------------------------------*
032300020419?     *  dati in testata                                             *
032400020419?     *--------------------------------------------------------------*
032500020419     C     srfor         BEGSR
032600020419      *
032700020502     c                   eval      rcoksc = cdfs
032800020422     C     Kfrn          CHAIN     anfrn01l                           20
032900020422     C  N20Krco          CHAIN     anrco98j                           20
033000020423     c* controllo se il fornitore appartiene all'unità di reg. scelta
033100020423     c* se 999 tutte le unità
033200020423     c                   if        not *in20 and f54uni <> '999     '
033300020423     c                             and rcounita <> f54uni
033400020423     c                   seton                                        20
033500020423     c                   end
033600020528     c* se 999 tutte le unità (vuol dire che sono in sede) controllo
033700020528     c* che il fornitore sia in autofatturazione
033800090707     c                   clear                   dtlz01
033900020531     c                   if        not *in20
034000020528     c                   move      cdfs          tlzpdr
034100020528     c     ktlz          chain     tntlz01l
034200020528     c                   if        %found(tntlz01l)
034300020528     c                   movel     tlzflo        dtlz01
034400020531     c                   if        §tlzfl1 = 'S'
034500020531     c                             and f54uni = '999     '
034600020528     c                   seton                                        20
034700020528     c                   end
034800020528     c                   end
034900020528     c                   end
035000020423     c*
035100020423     c                   if        not *in20
035200091109     C                   if        %subst(indtelex: 1: 8) = 'CAP.SOC.'
035300091109     c                   eval      csemi = indtelex
035400091109     c                   else
035500091201     c* per l'ottico non può essere blanks
035600091201     c                   eval      csemi ='.'
035700091109     c                   end
035800020419     C                   MOVEL     sogdes        desemi
035900020422     C                   eval      indemi = indindriz
036000020422     c                   if        indprov <> ' '
036100020422     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
036200020422     c                   else
036300020422     C                   eval      locemi = indlocalit
036400020422     c                   end
036500020422     C                   eval      capemi = indcap
036600091119     c*
036700091119     C                   movel     sogpartiva    piemi            20
036800091119     C                   MOVEL     indprov       premi             2
036900091119     C                   movel     sogcdfisc     cfemi            20
037000161122     c*
037100161122     c                   exsr      Retrieve_PEC
037200091119     c*
037300091119     C                   if        cfemi <> ' '
037400091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
037500091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
037600091119     C                             ': ' + %trimr(cfemi)
037700091119     c                   else
037800091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
037900091119     c                   end
038000020419     c* se il fornitore ha un codice IVA
038100020419     C                   CLEAR                   tabg48
038200020419     c                   if        frncdiva <> *blanks
038300020419     C                   CLEAR                   X06DS
038400020419     C                   MOVE      xscsoc        X06SOC
038500020419     C                   MOVE      XSCLIN        X06LIN
038600020419     C                   MOVE      frncdiva      X06CIV
038700020422     C                   MOVE      f54dti        X06DAT
038800020419     C                   CALL      'X06G48'
038900020419     C                   PARM                    X06DS
039000020419     C     X06ERR        IFEQ      '0'
039100020419     C                   MOVEL     X06UNI        TABG48
039200020419     C                   end
039300020419     C                   end
039400030827     c* aggancio la tabella Y4Z con tipo servizio D
039500030827     c                   exsr      sry4z
039600020419     c* aliquota IVA
039700020419     c                   if        frncdiva = *blanks or alig48 <> 0
039800020419     c                   if        alig48 = 0
039900020422     c                   move      §4ziva        comiva            5
040000020419     c                   else
040100020422     c                   move      alig48        comiva
040200020419     c                   end
040300020422     c                   clear                   comiva1           5
040400020422     c                   eval      comiva1 = %subst(comiva: 2: 2)+ ',' +
040500020422     c                             %subst(comiva: 4: 2)
040600020422     c                   eval      desiva = 'Aliquota ' + comiva1 + '%'
040700020422     c                   else
040800020422     c                   eval      desiva = desg48
040900020422     C                   end
041000020502     c* condizioni di pagamento
041100020502  "  C                   reset                   DVCDPDS
041200020502  "  C                   eval      DCPRIC='1'                                   chain
041300020502  "  C                   eval      DCPSOC=XSCSOC
041400020502  "  C                   eval      DCPUNI=frnunita
041500020502  "  C                   eval      DCPVIS='1'                                   lf principale
041600020502  "  C                   eval      DCPALC=*OFF                                  no allocazione
041700020502  "  C                   eval      DCPNRK=1                                     numero chiavi
041800020502  "  C                   eval      DCPLIN=xsclin
041900020502  "  C                   eval      DCPCOD=frncondpag
042000020502  "  C                   eval      DCPERR=*OFF
042100020502  "  C                   CALL      'DVCDP'
042200020502  "  C                   PARM                    DVCDPDS
042300020502  "  C                   PARM                    DVOCDPDS
042400020502  "  C                   if        DCPERR=*OFF
042500020502  "  C                   eval      Descon = CDPdesbrev
042600020502     c                   else
042700020502     c                   clear                   descon
042800020502     C                   end
042900020502     c* banca d'appoggio  INDICATORE 11 x FACTOR
043000020502     c                   if        rcokscfact <> *blanks
043100020502     c                   seton                                        11
043200020502     c                   eval      rcoksc = RCOkscfact
043300020502     C     Krco          CHAIN     anrco98j
043400020502     c                   if        %found(anrco98j)
043500020502     C                   MOVEL     sogdes        desfac
043600070606     C                   MOVEL     rcoksc        codfac
043700020502     c                   end
043800020502     c                   else
043900020502     c                   setoff                                       11
044000020502     c*banca d'appoggio in alternativa al factor
044100020502     C     KCBA          CHAIN     ANCBA01L
044200020502     c                   if        %found(anCBA01L) AND
044300020502    >C                             CBAABI <> *Blank AND
044400020502    >C                             CBACAB <> *Blank
044500020502    >C                   Move      CBAABI        X59ABI
044600020502    >C                   Move      CBACAB        X59CAB
044700020502A0692C                   Move      XSCSOC        X59SOC
044800020502    >C                   Call      'X59ABI'
044900020502    >C                   Parm                    X59ABIDS
045000020502     C*
045100020502    >C                   If        X59Err = '1'
045200090928    >C                   CLEAR                   iban
045300090928    >C                   CLEAR                   ABI
045400020502    >C                   CLEAR                   CAB
045500020503    >C                   CLEAR                   nrcc
045600020502    >C                   CLEAR                   DESIST
045700020502    >C                   CLEAR                   DESAGE
045800020502    >C                   Else
045900020502     C                   Movel     X59DesIst     DESIST
046000020502     C                   Movel     X59DesAge     DESAGE
046100090928     C                   Movel     CBAiban       iban
046200090928     C                   Movel     CBAABI        ABI               5
046300090928     C                   Movel     CBACAB        CAB               5
046400090928     C                   Movel     CBACcB        nrcc             18
046500020502    >C                   End
046600020502     c                   end
046700020502     c                   end
046800020502     C*
046900020423     C                   end
047000020419      *
047100020419     C                   ENDSR
047200020419      *-----------------------------------------------------***********
047300020419      * Campi per stampa
047400020419      *-----------------------------------------------------***********
047500020419     C     SRCAMPI       BEGSR
047600090629     C                   Z-ADD     Totv          imptv
047700090629     C                   Z-ADD     Tots          imptg
047800020422     C                   add       imptg         imppdr
047900020419     c                   move      ddcs          dataiso
048000020419     c                   move      dataiso       dataeur
048100020422     c                   move      dataeur       DTDDC
048200030827     c* decodifico po da
048300030827     c                   move      podas         orgfil
048400030827     c                   exsr      srorg
048500030827     c                   movel(p)  orgdes        poda
048600030827     c* decodifico po a
048700030827     c                   move      poas          orgfil
048800030827     c                   exsr      srorg
048900030827     c                   movel(p)  orgdes        poa
049000020419     C                   ENDSR
049100020422     C**************************************************************************
049200030827     C* decodifico po
049300020422     C**************************************************************************
049400030827     C     srorg         BEGSR                                                  *
049500030827     c                   clear                   orgdes
049600030827     c     orgfil        chain     azorg01l
049700030827     C                   ENDSR
049800030827     C**************************************************************************
049900030827     C* Gestione stampa
050000030827     C**************************************************************************
050100030827     C     LISTA         BEGSR                                                  *
050200081002      *
050300081001     c******             if        conta>60 or *in23  or
050400081001     c                   if        conta>60 or *IN24  OR
050500081003     c******                       (conta>34 and *in03) or
050600081003     c                             conta > 35
050700020422     c                   seton                                        3007
050800020422     c                   z-add     1             conta
050900081001      * se stampata anche dicitura : Corrispettivi......
051000081001     c                   if        *in23=*off
051100081002     c                   add       2             conta
051200081001     c                   end
051300020422     c                   end
051400081002      *
051500070606     c* memorizzo nello storico anagrafiche
051600070606     c   30              exsr      sranf0
051700020422     c* testata fattura
051800020422     c   30              write     testa
051900081001     c                   setoff                                       30  24
052000081001     c                   seton                                          23
052100081002      *
052200020508     c* testata lista fatture emesse
052300020508     c                   if        f54lis = 'S' and *inof
052400020508     c                   except    teste
052500020508     c                   setoff                                       of
052600020508     c                   end
052700081002      *
052800081002      * --------------------------------------
052900081002      *  Stampa il totale Autotrasportatore:  --> 03 ON
053000081002      * --------------------------------------
053100081002     c                   if        *in03
053200081002      *
053300080922     c* decodifica dati Carburante Legge 133/08
053400081002     c                   exsr      sranf2
053500020422     c* totale padroncino
053600081002     c                   write     totpdr
053700081002     c                   add       1             conta
053800111125      * stampo solo se ho trovato il record 2 per adeguamento carburante
053900111125     c                   if        %found(tnanf01l) and
054000111125     c                             impGPI <> 0
054100081002      * Controllo se OVRFLOW
054200081003     c                   z-add     34            maxrighe          3 0
054300081002     c                   exsr      chk_OvrFLW
054400081002     c* 1°riga riepilogo carburante
054500081002     c                   write     carbu1
054600081002     c                   add       1             conta
054700081002      *
054800081002      * Controllo se OVRFLOW
054900081002     c                   exsr      chk_OvrFLW
055000081002     c* 2°riga riepilogo carburante
055100081002     c                   write     carbu2
055200081002     c                   add       1             conta
055300081002      *
055400081002      * Controllo se OVRFLOW
055500081002     c                   exsr      chk_OvrFLW
055600081002     c* 3°riga riepilogo carburante
055700081002     c                   write     carbu3
055800081002     c                   add       1             conta
055900081002      *
056000081002      * Controllo se OVRFLOW
056100081002     c                   exsr      chk_OvrFLW
056200081002     c* 4°riga riepilogo carburante
056300081002     c                   write     carbu4
056400081002     c                   add       1             conta
056500081002      *
056600081002      * Controllo se OVRFLOW
056700081003     c                   z-add     31            maxrighe          3 0
056800081002     c                   exsr      chk_OvrFLW
056900081002     c* ultime righe riepilogo carburante
057000081002     c                   write     carbu5
057100081002     c                   add       4             conta
057200081002      *
057300111125     c                   end
057400081002     c                   end
057500081002      *---------------------------------------
057600081002      *
057700020509     c* totale FATTURA FORNITORE
057800020422     c   05              write     totGEN
057900020509     c* dati finali
058000020701     c*  06              write     totGE1
058100020531     c* riga dettaglio fatture emesse solo per fornitori in autofatturazione
058200020531     c                   if        f54lis = 'S' and *in05 and
058300020531     c                             §tlzfl1 = ' '
058400020422     c                   except    det
058500020422     c                   end
058600070606     c* memorizzo storico righe anagrafico x autofat.
058700070606     c                   if        *in01 and despdr <> ' '
058800070606     c                   exsr      sranf1
058900070606     c                   end
059000020422     c* dettaglio
059100020422     c   01              write     riga
059200081003     c   01              add       1             conta
059300020422     C   01              MOVEL     *BLANKS       DESPDR
059400020422     C   01              setoff                                       07
059500020422     c                   movea     '000000'      *in(01)
059600020422     c                   ENDSR
059700020527?     *--------------------------------------------------------------*
059800020527?     *  lettura tabella Y4Z                                         *
059900020527?     *--------------------------------------------------------------*
060000020527     C     sry4z         BEGSR
060100020527     c*  lettura tabella Y4Z
060200020527     c                   clear                   xatbds
060300020527     c                   move      '1'           xtbric
060400020527     c                   move      *blank        xtbkey
060500020527     c                   move      'Y4Z'         xtbcod
060600030827     c                   movel     'D'           xtbkey
060700020527     c                   call      'XATB'
060800020527     c                   parm                    xatbds
060900020527     c                   if        xtberr = '0'
061000020527     c                   movel     xtbuni        angy4z
061100020527     c                   else
061200020527     c                   clear                   angy4z
061300020527     c                   end
061400020527     C*
061500020527     C                   ENDSR
061600070606      *-----------------------------------------------------***********
061700070606      * memorizzo storico testata anagrafico fornitore solo se no copia iva
061800070606      *-----------------------------------------------------***********
061900070606     C     SRanf0        BEGSR
062000070606     c                   eval      ANFTIP = 'D'
062100070607     c                   eval      ANFTsr = 'D'
062200070606     c                   eval      ANFTrc = '0'
062300070606     c                   eval      ANFSOC = f54soc
062400070905     c                   eval      ANFCDF = salfor
062500070606     c                   eval      ANFNFF = ivanrdoc
062600070606     c                   move      ivadtdoc      ANFDFT
062700070606     c                   clear                   anfpdr
062800070606     c                   clear                   danf0
062900070606     c     kanf0         chain     tnanf01l
063000070606     c                   if        f54uni <>'999     '
063100091111     c                   eval      §ANF0telex = csemi
063200091111     c                   eval      §ANF0prov  = premi
063300070606     c                   eval      §ANF0DES = desemi
063400070606     c                   eval      §ANF0IND = indemi
063500070606     c                   eval      §ANF0CAP = capemi
063600070606     c                   eval      §ANF0LOC  = locemi
063700070606     c                   eval      §ANF0PI   = piemi
063800070606     c                   eval      §ANF0CF   = cfemi
063900161122     c                   eval      §ANF0pec  = peceml
064000161122     c                   eval      §ANF0lIVA = ivaLIB
064100070606     c                   eval      §ANF0CDP  = frncondpag
064200070606     c                   eval      §ANF0DCP  = descon
064300070606     c                   if        *in11
064400070606     c                   eval      §ANF0FAC  = codfac
064500070606     c                   eval      §ANF0DFAC = desfac
064600070606     c                   else
064700090928     c                   eval      §ANF0iban = iban
064800090928     c                   eval      §ANF0ABI  = abi
064900070606     c                   eval      §ANF0CAB  = cab
065000070606     c                   eval      §ANF0NRC  = nrcc
065100070606     c                   eval      §ANF0IST  = desist
065200070606     c                   eval      §ANF0AGE  = desage
065300070606     c                   end
065400070606     c                   eval      §ANF0NRP  = nrprt
065500070606     c                   move      ivadtreg      §ANF0DRP
065600070606     c                   eval      anfuni = danf0
065700070606     c                   if        %found(tnanf01l)
065800070606     c                   update    tnanf000
065900070606     c                   else
066000070606     c                   write     tnanf000
066100070606     c                   end
066200070606     c                   else
066300070606     c* imposto i dati di stampa come memorizzati
066400070606     c                   if        %found(tnanf01l)
066500070606     c                   eval      danf0 = anfuni
066600091111     c                   eval      csemi = §ANF0telex
066700070606     c                   eval      desemi      = §ANF0DES
066800070606     c                   eval      indemi      = §ANF0IND
066900070606     c                   eval      capemi      = §ANF0CAP
067000070606     c                   eval      locemi     = §ANF0LOC
067100091119     c                   eval      premi = §ANF0prov
067200070606     c                   eval      piemi      = §ANF0PI
067300070606     c                   eval      cfemi      = §ANF0CF
067400091119     C                   if        cfemi <> ' '
067500091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
067600091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
067700091119     C                             ': ' + %trimr(cfemi)
067800091119     c                   else
067900091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
068000091119     c                   end
068100070606     c                   eval      descon     = §ANF0DCP
068200090928     c                   eval      iban       = §ANF0iban
068300070606     c                   eval      abi        = §ANF0ABI
068400070606     c                   eval      cab        = §ANF0CAB
068500070606     c                   eval      nrcc       = §ANF0NRC
068600161122     c                   eval      peceml     = §ANF0pec
068700070606     c                   eval      desist     = §ANF0IST
068800070606     c                   eval      desage     = §ANF0AGE
068900070606     c                   eval      desfac    = §ANF0DFAC
069000070606     c                   end
069100070606     c                   end
069200070606     C                   ENDSR
069300161116      *----------------------------------------------------------------------
069400161116      *   reperisce la PEC se c'è
069500161116      *----------------------------------------------------------------------
069600161116     C     RETRIEVE_PEC  BEGSR
069700161116      *
069800161116     c                   eval        PECEML = 'PEC:'
069900161117     C                   if        cfemi <> ' '
070000161117     C                   eval       prmI_CFEMI = cFEMI
070100161116     C                   clear                   prmO_PECEML
070200161116     C                   CALL      'FICNPECR'
070300161117     C                   PARM                    prmI_CFEMI       20
070400161116     C                   PARM                    prmO_PECEML     100
070500161116      *
070600161116     C                   eval       pecEML = %trim(pecEML) + %trim(prmO_PECEML)
070700161116     c                   end
070800161116      *
070900161116     C                   ENDSR
071000161116      *----------------------------------------------------------------------
071100070606      *-----------------------------------------------------***********
071200070606      * memorizzo storico righe anagrafico fornitore solo se no copia iva
071300070606      *-----------------------------------------------------***********
071400070606     C     SRanf1        BEGSR
071500070606     c                   eval      ANFTIP = 'D'
071600070607     c                   eval      ANFTsr = 'D'
071700070606     c                   eval      ANFTrc = '1'
071800070606     c                   eval      ANFSOC = f54soc
071900070606     c                   eval      ANFCDF = cdfs
072000070606     c                   eval      ANFNFF = ivanrdoc
072100070606     c                   move      ivadtdoc      ANFDFT
072200070606     c                   eval      ANFpdr = pdrs
072300070606     c                   clear                   danf1
072400070606     c     kanf0         chain     tnanf01l
072500070606     c                   if        f54uni <>'999     '
072600070606     c                   eval      §ANF1DES = despdr
072700070606     c                   eval      anfuni = danf1
072800070606     c                   if        %found(tnanf01l)
072900070606     c                   update    tnanf000
073000070606     c                   else
073100070606     c                   write     tnanf000
073200070606     c                   end
073300070606     c                   else
073400070606     c* imposto i dati di stampa come memorizzati
073500070606     c                   if        %found(tnanf01l)
073600070606     c                   eval      danf1 = anfuni
073700070606     c                   eval      despdr      = §ANF1DES
073800070606     c                   end
073900070606     c                   end
074000070606     C                   ENDSR
074100080922      *-----------------------------------------------------***********
074200080922      * Decodifico dati x Carburante legge 133/2008
074300080922      *-----------------------------------------------------***********
074400080922     C     SRanf2        BEGSR
074500080922     c                   eval      ANFTIP = 'D'
074600080922     c                   eval      ANFTSR = 'D'
074700080922     c                   eval      ANFTrc = '2'
074800080922     c                   eval      ANFSOC = f54soc
074900080925     c                   eval      ANFCDF = salFOR
075000080922     c                   eval      ANFNFF = ivanrdoc
075100080922     c                   move      ivadtdoc      ANFDFT
075200080925     c                   eval      ANFpdr = salPDR
075300080922     c                   clear                   danf2
075400111128     c*                  clear                   impPDR
075500081003     c                   clear                   impTGC
075600081003     c                   clear                   impGPI
075700081003     c                   clear                   impVAR
075800081003     c                   clear                   impADGc
075900080922     c     kanf0         chain     tnanf01l
076000080922     c* imposto i dati di stampa come memorizzati
076100080922     c                   if        %found(tnanf01l)
076200080922     c                   eval      danf2 = anfuni
076300080922     c                   eval      impPDR  = §ANF2COR
076400161116     c                   eval(h)   impTGC  = §ANF2TGC
076500080922     c                   eval      impGPI  = §ANF2GPI
076600080922     c                   eval      impVAR  = §ANF2VAR
076700080922     c                   eval      impADGc = §ANF2ADG
076800080922     c                   end
076900080922     C                   ENDSR
077000011031     C**********************************************************************
077100910521     C     DEFCAM        BEGSR
077200011031     C**********************************************************************
077300000000     C     *ENTRY        PLIST
077400000000     C                   PARM                    KPJBA
077500020419     C                   MOVEL     KPJBU         ficn54ds
077600020419     C*---------- RICERCA DITTA :
077700020419     C                   MOVEL     'SOC001'      TIPXSC
077800020422     C                   MOVEL     f54soc        SOCXSC
077900020422     c     f54lis        comp      'S'                                    of
078000020419     C                   EXSR      REPSOC
078100020419      *  Definizione chiave di accesso
078200070606     c     kanf0         klist
078300070606     c                   KFLD                    ANFTIP
078400070607     c                   KFLD                    ANFTsr
078500070606     c                   KFLD                    ANFTrc
078600070606     c                   KFLD                    ANFSOC
078700070606     c                   KFLD                    ANFCDF
078800070606     c                   KFLD                    anfdft
078900070606     c                   KFLD                    ANFNFF
079000070606     c                   KFLD                    ANFpdr
079100021203     C     Kapd          KLIST
079200021203     C                   KFLD                    apdtip
079300021203     C                   KFLD                    pdrs
079400030827     c                   movel     'D'           apdtip
079500021203     C     Ktlz          KLIST
079600021203     C                   KFLD                    tlztip
079700021203     C                   KFLD                    tlzpdr
079800021203     C                   KFLD                    f54soc
079900030827     c                   movel     'D'           tlztip
080000030827     C     Katt          KLIST
080100020502     C                   KFLD                    f54soc
080200020502     C                   KFLD                    salfor
080300030828     C                   KFLD                    attflg
080400030827     C     Katt1         KLIST
080500030827     C                   KFLD                    f54soc
080600030827     C                   KFLD                    salfor
080700020502     C     Kfrn          KLIST
080800020502     C                   KFLD                    f54soc
080900020502     C                   KFLD                    cdfs
081000020419     C     Krco          KLIST
081100020422     C                   KFLD                    f54soc
081200020422     C                   KFLD                    rcosnatura
081300020502     C                   KFLD                    rcoksc
081400020422     c                   eval      rcosnatura = 'F'
081500020422     C     Kiva          KLIST
081600020422     C                   KFLD                    ivasys
081700020422     C                   KFLD                    ivanrasreg
081800020502     c                   eval      ivasys = 0
081900020502    >C     KCBA          KLIST
082000020502    >C                   KFLD                    XSCSOC
082100020502    >C                   KFLD                    CBACLIFOR
082200020502    >C                   KFLD                    KCC
082300020502    >C                   KFLD                    rcoKSC
082400020502    >C                   MOVEL     'F'           CBACLIFOR
082500020502     c* società
082600020422     c                   eval      %subst(cmd(3): 31: 3) = F54SOC
082700020502     c* autotrasportatore inizio
082800020422     c                   eval      %subst(cmd(3): 50: 8) = F54pd1
082900020502     c* autotrasportatore fine
083000020422     c                   eval      %subst(cmd(4): 12: 8) = F54pd2
083100020502     c* data inizio
083200020422     c                   move      f54dti        com8              8
083300020419     c                   eval      %subst(cmd(5): 11: 8) = com8
083400020502     c* data fine
083500020422     c                   move      f54dtf        com8
083600020419     c                   eval      %subst(cmd(5): 34: 8) = com8
083700020419     c* MI COMPONGO L'ISTRUZIONE SQL
083800020604     c* l'indicatore 80 mi serve per evitare la doppia ribattitura
083900020604     c* nella stampa della copia IVA
084000020503     c*
084100040217     c* se stampa per libro IVA non controllo data di stampa fattura
084200040217     c* altrimenti deve essere a 0 (CMD,8)
084300020503     c                   if        f54uni = '999     '
084400020604     c                   seton                                        80
084500020419     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
084600020503     C                             +CMD(4)+CMD(5)+CMD(9)+CMD(6)+CMD(7)
084700020503     c                   else
084800020604     c                   setoff                                       80
084900020503     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
085000020503     C                             +CMD(4)+CMD(5)+CMD(8)+CMD(6)+CMD(7)
085100020503     c                   end
085200020419     C                   ENDSR
085300020419     C*----------------------------------------------------*
085400020419     C* Reperimento dati società
085500020419     C*----------------------------------------------------*
085600020419     C     REPSOC        BEGSR
085700020419     C*
085800020419     C                   CALLB     'XSOC'
085900020419     C                   PARM                    TIPXSC            6
086000020419     C                   PARM                    SOCXSC            3
086100020419     C                   PARM                    CDSXSC            9 0
086200020419     C                   PARM                    MODXSC            3
086300020419     C                   PARM      *blanks       RTNXSC            1
086400020419     C                   PARM                    XSOCDS
086500020419     C                   PARM                    KPJBA
086600110502     C     RTNXSC        IFNE      '1'
086700110502     C                   MOVEL     XSOCDS        SOC001
086800110502     c* decodifico i dati della società
086900110502     c                   exsr      Decod_societa
087000110502     c                   if         PRMRPYIDMSG >= 0
087100110502     c                              and oIDSOCIETA <> *blank
087200110502     c*
087300110502     C                   MOVEL     oRAGSOCIALE   RSUT             20
087400110502     c                   movel     oRAGSOCIALE   desric
087500110502     c                   movel     oSEDLEGIND    indric
087600110502     c                   movel     oSEDLEGCAP    capric
087700110502     c                   movel     oPARTITAIVA   piric
087800110502     c                   movel     oCODFISCALE   cfric
087900110502     c     cfric         comp      ' '                                8989
088000110502     c                   if        oSEDLEGPRO = ' '
088100110502     C                   movel     oSEDLEGLOC    locric
088200110502     C                   ELSE
088300110502     C                   eval      locric = %trimr(osedlegloc)+
088400110502     C                             ' ('+osedlegpro+')'
088500110502     c                   end
088600110502     c                   end
088700110502     c                   end
088800020419     C*
088900020419     C                   ENDSR
089000081002     C*----------------------------------------------------*
089100081002     C*   Controlla se deve andare alla pagina seguente
089200081002     C*----------------------------------------------------*
089300081002     C     chk_OvrFLW    BegSR
089400081002     C*
089500081002     C* se superato il limite si emette la testata
089600081002     c                   if        conta > maxrighe
089700081002     c                   seton                                        3007
089800081002     c                   z-add     1             conta
089900081002     c                   end
090000081002      *
090100081002     c* memorizzo nello storico anagrafiche
090200081002     c   30              exsr      sranf0
090300081002     c* testata fattura
090400081002     c   30              write     testa
090500081002     c                   setoff                                       30  24
090600081002     c                   seton                                          23
090700081002     C*
090800081002     C                   ENDSR
090900110502     C**************************************************************************
091000110502     C* decodifica la società
091100110502     C************************************************************
091200110502     C     Decod_societa begSR
091300110502     ** Inizializzo il programma.
091400110502     C                   CALL      'TIBSSOCR'
091500110502     C                   PARM      'INIT'        prmRqsOpCode
091600110502     C                   PARM                    prmRpyOpCode
091700110502     C                   PARM                    prmRpyIdMsg
091800110502      *
091900110502     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
092000110502     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
092100110502     c* forzo società 201
092200110502     C                   EVAL      IidSocieta =f54soc
092300110502     C                   move      F54DTI        IDTVALIDITA
092400110502     C
092500110502     C                   CALL      'TIBSSOCR'
092600110502     C                   PARM      'GETANAGRAF'  prmRqsOpCode
092700110502     C                   PARM      *BLANK        prmRpyOpCode
092800110502     C                   PARM      *ZERO         prmRpyIdMsg
092900110502     C                   PARM      'TIBSSOCI0'   prmRqsFormato
093000110502     C                   PARM                    tibsSocI0
093100110502     C                   PARM                    prmRqsDataSize
093200110502     C                   PARM      'TIBSSOCO0'   prmRpyFormato
093300110502     C                   PARM                    tibsSocO0
093400110502     C                   PARM                    prmRpyDataSize
093500110502      *
093600110502     C                   ENDSR
093700081002     C*----------------------------------------------------*
093800081002     C*
093900030827     Ofiatt000  E            aggio
094000030827     o                       attdcn
094100020502     OQSYSPRT   E            TESTE          2 02
094200020422     O                       RSUT                20
094300030827     O                                         + 25 'LISTA FATTURE EMESSE X AFF'
094400030827     O                                         +  0 'LUENZE/DEFLUENZE'
094500030827     O                                          113 'FICN68R'
094600020422     O                       UDATE              127 '  /  /  '
094700020422     O                       PAGE1         Z    132
094800020422     O          E            teste       1  2
094900020503     o                                         +  0 'Fornitore'
095000021010     o                                         + 46 'Fattura'
095100020503     o                                         +  1 'Data'
095200020503     o                                         +  8 'Firma'
095300021010     o                                         + 37 'Data consegna'
095400020503     O          E            DET         3
095500020503     O                       salfor            +  0
095600020422     O                       desemi            +  1
095700020422     O                       nrfat         4   +  1
095800020503     O                       dtfat             +  1 '  /  /    '
095900020503     O                                         +  2 '__________________________'
096000021010     O                                         +  0 '______________'
096100021010     O                                         +  2 '_______________'
096200020419**
096300090629SELECT attpdr, attddc, sum(attimpp-attimp),                           1
096400030827sum(attimpp), attnra, attcdf, attfgp, attfga                          2
096500030827from fiatt06l WHERE attsoc = '000' and attcdf >='00000000' and        3
096600030827attcdf <= '00000000' and attdco <> 0 and                              4
096700030827attdft >= 00000000 and attdft <= 00000000 and attnff <> 0             5
096800030827GROUP BY attcdf, attnra, attpdr, attddc, attfgp, attfga               6
096900030827ORDER BY attcdf, attnra, attpdr, attddc, attfgp, attfga               7
097000030827and attdcn = 0                                                        8
097100040217                                                                      9
