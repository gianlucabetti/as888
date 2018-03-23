000100150112     H OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR('PJXBND' : 'PJCBND')
000200150112     H ACTGRP('QILE')
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400900606?     *--------------------------------------------------------------*
000500030207      * ficn65R   stampa autofattura conteggi cooperative            *
000600900606?     *--------------------------------------------------------------*
000700020422     FanRCO98J  iF   E           K DISK
000800020419     Fanfrn01L  IF   E           K DISK
000900020502     FanCBA01L  IF   E           K DISK
001000030207     FndIVA00f  IF   E           K DISK
001100030207     Fndreg00F  IF   E           K DISK
001200021203     Ffiapd01L  IF   E           K DISK
001300030207     Fazorg01L  IF   E           K DISK
001400030207     Ftntlz01l  IF   E           K DISK
001500030207     Ftabel00f  IF   E           K DISK
001600030207     Ffictt06L  uF   E           K DISK
001700070606     ftnanf01L  uF a E           K DISK
001800030207     Fficn65p   O    e             PRINTER OFLIND(*IN23)
001900020422     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
002000900515      *
002100020606     D KPJBA         E DS
002200071120     D ficn99ds      E DS
002300020422?     *  Tabella Y4Z
002400020422     D angy4z        E DS                  extname(angy4zds)
002500020528     D dtlz01        e DS
002600020528     D xatbds        e DS
002700020422     D soc001        E DS                  EXTNAME(xsoc001ds)
002800070606     D danf0         e DS
002900020422     D xsocds          DS          1000
003000020502     D*ABI/CAB
003100020502     D X59ABIDS      E DS                  INZ
003200020502  "  D* DS per input condizione di pagamento
003300020502  "  DDVCDPDS        E DS                  INZ
003400020502  "  D* Ds per output
003500030207  "  DDVOCDPDS       E ds                  inz
003600030207     c*
003700030207  "  Dds1z           E DS                  INZ
003800020422?     *  Tabella IVA
003900020422     DTABG48         E DS                  EXTNAME(ANGG48DS)
004000020422     D X06DS         E DS                  EXTNAME(X06G48DS)
004100020422     D  X06SOC       E                     EXTFLD(X06SOCIETA)
004200020422     D  X06CIV       E                     EXTFLD(X06CDIVA)
004300020422     D  X06DAT       E                     EXTFLD(X06DATA)
004400020422     D  X06LIN       E                     EXTFLD(X06LINGUA)
004500900516      *
004600020419     D Ficn54ds      E DS
004700030212     D Ficn20ds      E DS
004800070606     D codfac          S                   like(RCOkscfact)
004900020422     d dataiso         s               d
005000020422     d dataeur         s               d   datfmt(*eur)
005100020422     D conta           S              3  0 inz
005200020422     D comodo          S              9  2 inz
005300020422     D salnra          S                   like(nrass)
005400150121     D savSQLCOD       S                   like(SQLcod)
005500030207     D com03           S              3    inz
005600020422     D salfor          S              8    inz
005700020614     D salfors         S                   like(salfor)
005800071221     D comdta          S                   like(ivadtdoc) inz(d'2007-12-31')
005900020502     D kcc             S                   inz like(rcokcc)
006000020419     c* SQL
006100020419     D cmd             s             70    ctdata dim(10)                       FIL. COMODO
006200030623     D decm            s             10    ctdata dim(12)                       FIL. COMODO
006300020419     D WrkSqlCmd       S           1024
006400030207     D minddc          S                   like(cttddc)
006500030207     D maxddc          S                   like(cttddc)
006600030212     D minpdr          S                   like(cttpdr)
006700030212     D maxpdr          S                   like(cttpdr)
006800150120     d in_nra          S                   like(cttnra)
006900150120     d in_cdf          S                   like(cttcdf)
007000150120      **
007100030207     D fictt           DS
007200030207     d tsrs                                like(ctttsr)
007300030207     d tots                                like(cttitc)
007400030207     d nrass                               like(cttnra)
007500030207     d cdfs                                like(cttcdf)
007600110502     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
007700110502     D ESITO_OK...
007800110502     D                 C                   0
007900110502     ***************************************************************************
008000110502     **
008100110502     ** Dichiarazione prototipi procedure esterne.
008200110502     **
008300110502     ***************************************************************************
008400110502      /DEFINE DFTACTGRP_YES
008500110502     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
008600110502      /UNDEFINE DFTACTGRP_YES
008700110502     ***************************************************************************
008800110502     **
008900110502     ** Definizione strutture dati.
009000110502     **
009100110502      **************************************************************************
009200110502     D tibsSocI0...
009300110502     D               E DS                  prefix(i)
009400110502     D                                     INZ
009500110502     D tibsSocO0...
009600110502     D               E DS                  prefix(o)
009700110502     D                                     INZ
009800110502     ***************************************************************************
009900110502     **
010000110502     ** Definizione variabili modulo/programma.
010100110502     **
010200110502     ***************************************************************************
010300110502     D prmRqsOpCode...
010400110502     D                 S             10A
010500110502     D prmRpyOpCode...
010600110502     D                 S             10A
010700110502     D prmRpyIdMsg...
010800110502     D                 S             10I 0
010900110502     D prmRqsFormato...
011000110502     D                 S             10A
011100110502     D prmRqsDataSize...
011200110502     D                 S             10I 0
011300110502     D prmRpyFormato...
011400110502     D                 S             10A
011500110502     D prmRpyDataSize...
011600110502     D                 S             10I 0
011700110502
011800011031     ***********************************************************************
011900910521     C                   EXSR      DEFCAM
012000030207     c* Lettura fictt06l ordinato per fornitore tipo servizio
012100030207     c* i record scelti sono: FTTFVL='C', limiti scelti a
012200020419     c* video per fornitore e data protocollo, numero fattura <> 0
012300020419     c*
012400020419     C/EXEC SQL
012500020419     C+ PREPARE S1 FROM :WrkSqlCmd
012600020419     C/END-EXEC
012700020419
012800020419     C/EXEC SQL
012900020419     C+ DECLARE A1 CURSOR FOR S1
013000020419     C/END-EXEC
013100020419
013200020419     C/EXEC SQL
013300020419     C+ OPEN A1
013400020419     C/END-EXEC
013500020419
013600020419     C                   DOU       SqlCod <> 0
013700020419     C/EXEC SQL
013800030207     C+ FETCH NEXT FROM A1 INTO :fictt
013900020419     C/END-EXEC
014000020419     c*
014100020419     C                   SELECT
014200020419     C                   WHEN      SqlCod = 0
014300030207     c* se nr.registrazione = 0 aggiorno data stampa ed esco
014400030207     c                   if        nrass = 0
014500020614     c                   movel     salfor        salfors
014600020614     c                   movel     cdfs          salfor
014700020614     c                   exsr      sragg
014800020614     c                   movel     salfors       salfor
014900020531     c                   iter
015000020531     c                   end
015100020419     c* rottura fornitore
015200020422     c                   if        cdfs <> salfor or nrass <> salnra
015300020419     c                   exsr      srrotfor
015400020419     c                   end
015500030723     c* se la fattura è = 0 leggo un altro fornitore
015600030723     c                   if        impfat = 0
015700030723     c                   iter
015800030723     c                   end
015900020422     c* dettaglio campi
016000020422     C                   EXSR      srcampi
016100020701     c* stampo la riga solo se il totale è <> 0
016200020701     c                   if        imptg <> 0
016300020422     c                   add       1             conta
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
017900020422     c                   exsr      srrotfor
018000020423     c                   end
018100030212     c* chiudo pgm FICN22R
018200030212     c                   clear                   ficn20ds
018300030212     c                   eval      f20ela = 'C'
018400030212     c                   movel(p)  ficn20ds      kpjbu
018500030212     c                   call      'FICN22R'
018600030212     c                   parm                    kpjba
018700030509     c* rimposto la kpjbu iniziale
018800030509     c                   eval      kpjbu = ficn54ds
018900110502     ** Chiudo il programma.
019000110502     C                   CALL      'TIBSSOCR'
019100110502     C                   PARM      'FINALIZE'    prmRqsOpCode
019200110502     C                   PARM                    prmRpyOpCode
019300110502     C                   PARM                    prmRpyIdMsg
019400110502     c*
019500020422     c*
019600910521     C                   SETON                                        LR
019700020419     C**************************************************************************
019800020419     C* Rottura fornitore
019900020419     C**************************************************************************
020000020419     C     Srrotfor      BEGSR                                                  *
020100020419     c* stampa la riga di totale
020200020419     c                   if        impfat <> 0
020300030207     c                   seton                                        02
020400020423     c                   else
020500020423     c                   z-add     0             conta
020600020419     c                   end
020700020419     c*
020800020701     c                   if        conta <> 60 and conta <> 0
020900020701     c* salto pagina per avere il totale fattura tutto insieme
021000040330     c                   if        conta > 24
021100020701     c                   seton                                        23
021200020701     c                   end
021300020701     c                   exsr      lista
021400020502     c* aggiorno la data di stampa
021500020614     c                   exsr      sragg
021600020701     c                   end
021700020422     c*
021800020419     c                   z-add     60            conta
021900020509     c                   setoff                                       23
022000020422     c* azzera il conteggio per fornitore
022100020502     c                   z-add     0             page
022200020502     c                   z-add     0             impon
022300020422     c                   z-add     0             impiva
022400020422     c                   z-add     0             impfat
022500020419     c* decodifico fornitore
022600020419     c                   exsr      srfor
022700020423     c* aggancio registrazione solo se fornitore ok
022800020423     c                   if        not *in20
022900030207     c                   z-add     nrass         ivanrasreg
023000030207     c     kiva          chain     ndiva00f
023100030207     c                   if        %found(ndiva00f)
023200020422     c                   move      ivadtdoc      dataeur
023300020422     c                   move      dataeur       dtfat
023400020422     c                   move      ivanrdoc      nrfat
023500020422     c                   move      ivadtreg      dataeur
023600020422     c                   move      dataeur       dtprt
023700020422     c                   move      ivanrreg      nrprt
023800020422     c                   Z-ADD     ivaimporto    impiva
023900020422     c                   Z-ADD     ivaimponib    impon
024000150109      **
024100150112     c                   clear                   fatpul
024200150112     c                   clear                   ivapul
024300150112     c                   clear                   imppul
024400150112     c                   clear                   aliiva
024500150120     c                   clear                   fatt_Pulizie      1
024600150109      **  per solo le Pulizie NON deve comprendere l'IVA
024700150109      **  il Totale equivale all'imponibile
024800150109     c                   if        Tsrs = 'P'
024900150120     c                   move      'S'           fatt_Pulizie
025000150109     c                   Z-ADD     impon         impfat
025100150109     c     impon         add       impiva        fatpul
025200150109     c                   z-add     impiva        ivapul
025300150109     c                   z-add     impon         imppul
025400150112     c                   movel     desIVA        aliIVA
025500150109     c                   else
025600150112      *
025700150112      * normalmente invece
025800020422     c     impon         add       impiva        impfat
025900150112      *
026000150109     c                   end
026100150109      **
026200020422     c                   end
026300030207     c* condizioni di pagamento: aggancio registrazione xchè la cond. pag.
026400030207     c* non è detto che sia quella del fornitore visto che è in I/O nella
026500030207     c* contabilizzazione
026600030207     c     kiva          chain     ndreg00f
026700030207     c                   if        %found(ndreg00f)
026800030207  "  C                   reset                   DVCDPDS
026900030207  "  C                   eval      DCPRIC='1'                                   chain
027000030207  "  C                   eval      DCPSOC=XSCSOC
027100030207  "  C                   eval      DCPUNI=frnunita
027200030207  "  C                   eval      DCPVIS='1'                                   lf principale
027300030207  "  C                   eval      DCPALC=*OFF                                  no allocazione
027400030207  "  C                   eval      DCPNRK=1                                     numero chiavi
027500030207  "  C                   eval      DCPLIN=xsclin
027600030207  "  C                   eval      DCPCOD=regcondpag
027700030207  "  C                   eval      DCPERR=*OFF
027800030207  "  C                   CALL      'DVCDP'
027900030207  "  C                   PARM                    DVCDPDS
028000030207  "  C                   PARM                    DVOCDPDS
028100030207  "  C                   if        DCPERR=*OFF
028200030207  "  C                   eval      Descon = CDPdesbrev
028300030207     c                   else
028400030207     c                   clear                   descon
028500030207     C                   end
028600030207     C                   end
028700030207     C                   end
028800030207     c*
028900030212     c                   if        impfat <> 0 and
029000030212     c                             (cdfs <> salfor or nrass <> salnra)
029100030207     c                   movel     cdfs          salfor
029200030207     c                   z-add     nrass         salnra
029300030212     c* stampo valorizzazione conteggi cooperative
029400030212     c                   exsr      srficn22
029500020531     c                   end
029600030527     c* se escluso da autofatturazione (ind. 21 acceso) azzero l'importo
029700030527     c* della fattura così passo al prossimo fornitore
029800030527     c   21              clear                   impfat
029900030527     c*
030000020419     c                   ENDSR
030100020419     C**************************************************************************
030200030207     C* aggiorna data di stampa in FICTT00F
030300020419     C**************************************************************************
030400020614     C     sragg         BEGSR                                                  *
030500020614     c*
030600030207     c     kctt          setll     fictt06l
030700020614     c                   do        *hival
030800030207     c     kctt          reade     fictt06l
030900030207     c                   if        %eof(fictt06l)
031000020614     c                   leave
031100020614     c                   end
031200020614     c* escludo quelli non fatturati oppure già stampati
031300030207     c                   if        cttnff = 0 or cttdcn <> 0
031400020614     c                   iter
031500020614     c                   end
031600020614      * controllo numero registrazione
031700030207     c                   if        cttnra = salnra
031800020614     c                   move      *date         dataiso
031900030207     c                   move      dataiso       cttdcn
032000020614     c                   except    aggio
032100030207     c                   end
032200020614     c                   enddo
032300071120     c* stampo la lettera x fusione solo se data fattura = 31/12/2007
032400071221     c                   if        ivadtdoc = comdta and
032500071221     c                             f54uni <> '999     '
032600071120     c                   eval      f99socn = f54soc
032700071120     c                   eval      f99kscn = salfor
032800071120     c                   eval      f99tip = 'C'
032900071120     c                   movel(p)  ficn99ds      kpjbu
033000071120     c                   call      'FICN99R'
033100071120     c                   parm                    kpjba
033200071221     c                   end
033300020614     c*
033400020614     c                   ENDSR
033500020419?     *--------------------------------------------------------------*
033600020419?     *  dati in testata                                             *
033700020419?     *--------------------------------------------------------------*
033800020419     C     srfor         BEGSR
033900020419      *
034000030527     c                   setoff                                       21
034100020502     c                   eval      rcoksc = cdfs
034200020422     C     Kfrn          CHAIN     anfrn01l                           20
034300020422     C  N20Krco          CHAIN     anrco98j                           20
034400020423     c* controllo se il fornitore appartiene all'unità di reg. scelta
034500020423     c* se 999 tutte le unità
034600020423     c                   if        not *in20 and f54uni <> '999     '
034700020423     c                             and rcounita <> f54uni
034800020423     c                   seton                                        20
034900020423     c                   end
035000020528     c* se 999 tutte le unità (vuol dire che sono in sede) controllo
035100020528     c* che il fornitore sia in autofatturazione
035200090707     C                   CLEAR                   DTLZ01
035300020531     c                   if        not *in20
035400020528     c                   move      cdfs          tlzpdr
035500020528     c     ktlz          chain     tntlz01l
035600020528     c                   if        %found(tntlz01l)
035700020528     c                   movel     tlzflo        dtlz01
035800020531     c                   if        §tlzfl1 = 'S'
035900020531     c                             and f54uni = '999     '
036000030527     c                   seton                                        21
036100020528     c                   end
036200020528     c                   end
036300020528     c                   end
036400020423     c*
036500020423     c                   if        not *in20
036600091109     C                   if        %subst(indtelex: 1: 8) = 'CAP.SOC.'
036700091109     c                   eval      csemi = indtelex
036800091109     c                   else
036900091201     c* per l'ottico non può essere blanks
037000091201     c                   eval      csemi ='.'
037100091109     c                   end
037200020419     C                   MOVEL     sogdes        desemi
037300020422     C                   eval      indemi = indindriz
037400020422     c                   if        indprov <> ' '
037500020422     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
037600020422     c                   else
037700020422     C                   eval      locemi = indlocalit
037800020422     c                   end
037900020422     C                   eval      capemi = indcap
038000091119     c*
038100091119     C                   movel     sogpartiva    piemi            20
038200091119     C                   MOVEL     indprov       premi             2
038300091119     C                   movel     sogcdfisc     cfemi            20
038400091119     c*
038500091119     C                   if        cfemi <> ' '
038600091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
038700091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
038800091119     C                             ': ' + %trimr(cfemi)
038900091119     c                   else
039000091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
039100091119     c                   end
039200030523     c* se il fornitore ha un codice IVA
039300030523     C                   CLEAR                   tabg48
039400030523     c                   if        frncdiva <> *blanks
039500030523     C                   CLEAR                   X06DS
039600030523     C                   MOVE      xscsoc        X06SOC
039700030523     C                   MOVE      XSCLIN        X06LIN
039800030523     C                   MOVE      frncdiva      X06CIV
039900030523     C                   MOVE      f54dti        X06DAT
040000030523     C                   CALL      'X06G48'
040100030523     C                   PARM                    X06DS
040200030523     C     X06ERR        IFEQ      '0'
040300030523     C                   MOVEL     X06UNI        TABG48
040400030523     C                   end
040500030523     C                   end
040600030523     c* aggancio la tabella Y4Z con tipo servizio X
040700030523     c                   exsr      sry4z
040800030523     c* aliquota IVA
040900030523     c                   if        frncdiva = *blanks or alig48 <> 0
041000030523     c                   if        alig48 = 0
041100030523     c                   move      §4ziva        comiva            5
041200030523     c                   else
041300030523     c                   move      alig48        comiva
041400030523     c                   end
041500030523     c                   clear                   comiva1           5
041600030523     c                   eval      comiva1 = %subst(comiva: 2: 2)+ ',' +
041700030523     c                             %subst(comiva: 4: 2)
041800030523     c                   eval      desiva = 'Aliquota ' + comiva1 + '%'
041900030523     c                   else
042000030523     c                   eval      desiva = desg48
042100030523     C                   end
042200020502     c* banca d'appoggio  INDICATORE 11 x FACTOR
042300020502     c                   if        rcokscfact <> *blanks
042400020502     c                   seton                                        11
042500020502     c                   eval      rcoksc = RCOkscfact
042600020502     C     Krco          CHAIN     anrco98j
042700020502     c                   if        %found(anrco98j)
042800020502     C                   MOVEL     sogdes        desfac
042900070606     C                   MOVEL     rcoksc        codfac
043000020502     c                   end
043100020502     c                   else
043200020502     c*banca d'appoggio in alternativa al factor
043300030207     c                   setoff                                       11
043400020502     C     KCBA          CHAIN     ANCBA01L
043500020502     c                   if        %found(anCBA01L) AND
043600020502    >C                             CBAABI <> *Blank AND
043700020502    >C                             CBACAB <> *Blank
043800020502    >C                   Move      CBAABI        X59ABI
043900020502    >C                   Move      CBACAB        X59CAB
044000020502A0692C                   Move      XSCSOC        X59SOC
044100020502    >C                   Call      'X59ABI'
044200020502    >C                   Parm                    X59ABIDS
044300020502     C*
044400020502    >C                   If        X59Err = '1'
044500090928    >C                   CLEAR                   iban
044600090928    >C                   CLEAR                   ABI
044700020502    >C                   CLEAR                   CAB
044800020503    >C                   CLEAR                   nrcc
044900020502    >C                   CLEAR                   DESIST
045000020502    >C                   CLEAR                   DESAGE
045100020502    >C                   Else
045200020502     C                   Movel     X59DesIst     DESIST
045300020502     C                   Movel     X59DesAge     DESAGE
045400090928     C                   Movel     CBAiban       iban
045500090928     C                   Movel     CBAABI        ABI               5
045600090928     C                   Movel     CBACAB        CAB               5
045700090928     C                   Movel     CBACcB        nrcc             18
045800020502    >C                   End
045900020502     c                   end
046000020502     c                   end
046100030207     c* decodifica po
046200030207     c                   eval      com03 = %subst(cdfs: 2: 3)
046300030207     c                   move      com03         orgfil
046400030207     c     orgfil        chain     azorg01l
046500030207     c                   if        %found(azorg01l)
046600030207     C                   eval      despo = %trimr(orgdes) + ' ' +
046700030207     C                             %trimr(orgind) + ' ' +
046800030207     C                             %trimr(orgloc)
046900030207     c                   end
047000150121      *-
047100150121      ** salva sqlCOD dell'SQL principale
047200150121     c                   eval       savSQLCOD = SQLcod
047300150121      *-
047400030207     c* reperisco la data minima e la data massima della prestazione fattur.
047500030207     c                   if        f54uni = '999     '
047600030207     C/EXEC SQL
047700030212     C+ SELECT min(cttddc), max(cttddc), min(cttpdr), max(cttpdr),
047800150121     C+ cttcdf, cttnra INTO :minddc, :maxddc, :minpdr, :maxpdr FROM
047900030212     C+ fictt06l WHERE cttcdf = :cdfs and cttnra = :nrass and cttfvl =
048000040217     C+ 'C' and cttdft between :f54dti and :f54dtf and cttnff <> 0 GROUP
048100040217     C+ BY cttcdf, cttnra ORDER BY cttcdf, cttnra
048200030207     C/END-EXEC
048300030207     c                   else
048400030207     c* data di stampa = 0
048500030207     C/EXEC SQL
048600030212     C+ SELECT min(cttddc), max(cttddc), min(cttpdr), max(cttpdr),
048700150121     C+ cttcdf, cttnra INTO :minddc, :maxddc, :minpdr, :maxpdr FROM
048800030212     C+ fictt06l WHERE cttcdf = :cdfs and cttnra = :nrass and cttfvl =
048900030212     C+ 'C' and cttdcn = 0 and cttdft between :f54dti and :f54dtf and
049000030212     C+ cttnff <> 0 GROUP BY cttcdf, cttnra ORDER BY cttcdf, cttnra
049100030207     C/END-EXEC
049200020423     C                   end
049300150121      *
049400150121      *- reimposta SQLcod del SQL Principale
049500150121     c                   eval       SQLcod = savSQLCOD
049600150121      *-
049700030623     c* decodifico il mese
049800030623     c                   move      maxddc        dataiso
049900030623     c                   extrct    dataiso:*m    mm                2 0
050000030623     c                   movel(p)  decm(mm)      pmese
050100150123     c                   extrct    dataiso:*y    yyyy              4 0
050200150123     c                   movel     yyyy          PANNO
050300030207      *
050400030207     C                   end
050500020419      *
050600020419     C                   ENDSR
050700070606      *-----------------------------------------------------***********
050800070606      * memorizzo storico testata anagrafico fornitore solo se no copia iva
050900070606      *-----------------------------------------------------***********
051000070606     C     SRanf0        BEGSR
051100070606     c                   eval      ANFTIP = 'C'
051200070607     c                   eval      ANFTsr = 'C'
051300070606     c                   eval      ANFTrc = '0'
051400070606     c                   eval      ANFSOC = f54soc
051500070905     c                   eval      ANFCDF = salfor
051600070606     c                   eval      ANFNFF = ivanrdoc
051700070606     c                   move      ivadtdoc      ANFDFT
051800070606     c                   clear                   anfpdr
051900070606     c                   clear                   danf0
052000070606     c     kanf0         chain     tnanf01l
052100070606     c                   if        f54uni <>'999     '
052200091111     c                   eval      §ANF0telex = csemi
052300091111     c                   eval      §ANF0prov  = premi
052400070606     c                   eval      §ANF0DES = desemi
052500070606     c                   eval      §ANF0IND = indemi
052600070606     c                   eval      §ANF0CAP = capemi
052700070606     c                   eval      §ANF0LOC  = locemi
052800070606     c                   eval      §ANF0PI   = piemi
052900070606     c                   eval      §ANF0CF   = cfemi
053000070606     c                   eval      §ANF0CDP  = frncondpag
053100070606     c                   eval      §ANF0DCP  = descon
053200070606     c                   if        *in11
053300070606     c                   eval      §ANF0FAC  = codfac
053400070606     c                   eval      §ANF0DFAC = desfac
053500070606     c                   else
053600090928     c                   eval      §ANF0iban = iban
053700070606     c                   eval      §ANF0ABI  = abi
053800070606     c                   eval      §ANF0CAB  = cab
053900070606     c                   eval      §ANF0NRC  = nrcc
054000070606     c                   eval      §ANF0IST  = desist
054100070606     c                   eval      §ANF0AGE  = desage
054200070606     c                   end
054300070606     c                   eval      §ANF0NRP  = nrprt
054400070606     c                   move      ivadtreg      §ANF0DRP
054500070606     c                   eval      anfuni = danf0
054600070606     c                   if        %found(tnanf01l)
054700070606     c                   update    tnanf000
054800070606     c                   else
054900070606     c                   write     tnanf000
055000070606     c                   end
055100070606     c                   else
055200070606     c* imposto i dati di stampa come memorizzati
055300070606     c                   if        %found(tnanf01l)
055400070606     c                   eval      danf0 = anfuni
055500091111     c                   eval      csemi = §ANF0telex
055600070606     c                   eval      desemi      = §ANF0DES
055700070606     c                   eval      indemi      = §ANF0IND
055800070606     c                   eval      capemi      = §ANF0CAP
055900070606     c                   eval      locemi     = §ANF0LOC
056000091119     c                   eval      premi = §ANF0prov
056100070606     c                   eval      piemi      = §ANF0PI
056200070606     c                   eval      cfemi      = §ANF0CF
056300091119     C                   if        cfemi <> ' '
056400091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
056500091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
056600091119     C                             ': ' + %trimr(cfemi)
056700091119     c                   else
056800091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
056900091119     c                   end
057000070606     c                   eval      descon     = §ANF0DCP
057100090928     c                   eval      iban       = §ANF0iban
057200070606     c                   eval      abi        = §ANF0ABI
057300070606     c                   eval      cab        = §ANF0CAB
057400070606     c                   eval      nrcc       = §ANF0NRC
057500070606     c                   eval      desist     = §ANF0IST
057600070606     c                   eval      desage     = §ANF0AGE
057700070606     c                   eval      desfac    = §ANF0DFAC
057800070606     c                   end
057900070606     c                   end
058000070606     C                   ENDSR
058100030523?     *--------------------------------------------------------------*
058200030523?     *  lettura tabella Y4Z tipo aservizio X                        *
058300030523?     *--------------------------------------------------------------*
058400030523     C     sry4z         BEGSR
058500030523     c*  lettura tabella Y4Z
058600030523     c                   clear                   xatbds
058700030523     c                   move      '1'           xtbric
058800030523     c                   move      *blank        xtbkey
058900030523     c                   move      'Y4Z'         xtbcod
059000030523     c                   movel     'X'           xtbkey
059100030523     c                   call      'XATB'
059200030523     c                   parm                    xatbds
059300030523     c                   if        xtberr = '0'
059400030523     c                   movel     xtbuni        angy4z
059500030523     c                   else
059600030523     c                   clear                   angy4z
059700030523     c                   end
059800030523     C*
059900030523     C                   ENDSR
060000020419      *-----------------------------------------------------***********
060100020419      * Campi per stampa
060200020419      *-----------------------------------------------------***********
060300020419     C     SRCAMPI       BEGSR
060400030207     C                   Z-ADD     Tots          imptg
060500030207     c* decodifico il tipo servizio
060600030207     c                   eval      tblkey = tsrs
060700030207     c     ktab          chain     tabel00f
060800030207     c                   if        %found(tabel00f)
060900030207     c                   movel     tbluni        ds1z
061000030207     c                   movel     §1zdes        destsr
061100030207     c                   end
061200030416     c* descrizione aggiuntiva solo per tsr = X e P
061300030416     c                   if        tsrs = 'X' or tsrs = 'P' or tsrs = 'T'
061400030207     c                   eval      desagg = 'come da allegata valorizzazione'
061500030207     c                   else
061600030207     c                   clear                   desagg
061700030207     c                   end
061800020419     C                   ENDSR
061900020422     C**************************************************************************
062000020422     C* Gestione stampa
062100020422     C**************************************************************************
062200020422     C     LISTA         BEGSR                                                  *
062300020509     c                   if        conta>60 or *in23
062400030207     c                   seton                                        30
062500020422     c                   z-add     1             conta
062600020422     c                   end
062700070606     c* memorizzo nello storico anagrafiche
062800070606     c   30              exsr      sranf0
062900020422     c* testata fattura
063000020422     c   30              write     testa
063100020509     c                   setoff                                       3023
063200020508     c* testata lista fatture emesse
063300020508     c                   if        f54lis = 'S' and *inof
063400020508     c                   except    teste
063500020508     c                   setoff                                       of
063600020508     c                   end
063700150120      *
063800150120      *   fare REVERSE CHARGE
063900150120      *  se si tratta di una Fattura Pulizie e c'è un importo IVA (NON ESENTE)
064000150120     c                   if        fatt_Pulizie = 'S'
064100150120     c                             and ivapul > 0
064200150120      *
064300150120      *  x le pulizie delle COOP testa il campo di valore
064400150120      * preimpostato se si stava trattando la COOP x le PULIZIE
064500150120     c   02              write     totPUL
064600150120      *
064700150120     c                   else
064800150120      * altrimenti   normale emissione di
064900020509     c* totale FATTURA FORNITORE
065000150120     c   02              write     totGEN
065100150120     c                   end
065200150120      *
065300020531     c* riga dettaglio fatture emesse solo per fornitori in autofatturazione
065400030207     c                   if        f54lis = 'S' and *in02 and
065500020531     c                             §tlzfl1 = ' '
065600020422     c                   except    det
065700020422     c                   end
065800020422     c* dettaglio
065900020422     c   01              write     riga
066000030207     c                   movea     '00'          *in(01)
066100020422     c                   ENDSR
066200011031     C**********************************************************************
066300910521     C     DEFCAM        BEGSR
066400011031     C**********************************************************************
066500000000     C     *ENTRY        PLIST
066600000000     C                   PARM                    KPJBA
066700020419     C                   MOVEL     KPJBU         ficn54ds
066800020419     C*---------- RICERCA DITTA :
066900020419     C                   MOVEL     'SOC001'      TIPXSC
067000020422     C                   MOVEL     f54soc        SOCXSC
067100020422     c     f54lis        comp      'S'                                    of
067200020419     C                   EXSR      REPSOC
067300020419      *  Definizione chiave di accesso
067400021203     C     Ktlz          KLIST
067500021203     C                   KFLD                    tlztip
067600021203     C                   KFLD                    tlzpdr
067700021203     C                   KFLD                    f54soc
067800030207     c                   movel     'C'           tlztip
067900030207     C     Kctt          KLIST
068000020502     C                   KFLD                    f54soc
068100020502     C                   KFLD                    salfor
068200030207     C                   KFLD                    cttfvl
068300030207     c                   movel     'C'           cttfvl
068400020502     C     Kfrn          KLIST
068500020502     C                   KFLD                    f54soc
068600020502     C                   KFLD                    cdfs
068700020419     C     Krco          KLIST
068800020422     C                   KFLD                    f54soc
068900020422     C                   KFLD                    rcosnatura
069000020502     C                   KFLD                    rcoksc
069100020422     c                   eval      rcosnatura = 'F'
069200020422     C     Kiva          KLIST
069300020422     C                   KFLD                    ivasys
069400020422     C                   KFLD                    ivanrasreg
069500020502     c                   eval      ivasys = 0
069600020502    >C     KCBA          KLIST
069700020502    >C                   KFLD                    XSCSOC
069800020502    >C                   KFLD                    CBACLIFOR
069900020502    >C                   KFLD                    KCC
070000020502    >C                   KFLD                    rcoKSC
070100020502    >C                   MOVEL     'F'           CBACLIFOR
070200030207     C*
070300030207     C     KTAB          KLIST
070400030207     C                   KFLD                    tblkut
070500030207     C                   KFLD                    tblcod
070600030207     C                   KFLD                    tblkey
070700030207     C                   Z-ADD     1             tblkut
070800030207     C                   eval      tblcod = '1Z'
070900070606      *  Definizione chiave di accesso
071000070606     c     kanf0         klist
071100070606     c                   KFLD                    ANFTIP
071200070607     c                   KFLD                    ANFTsr
071300070606     c                   KFLD                    ANFTrc
071400070606     c                   KFLD                    ANFSOC
071500070606     c                   KFLD                    ANFCDF
071600070606     c                   KFLD                    anfdft
071700070606     c                   KFLD                    ANFNFF
071800070606     c                   KFLD                    ANFpdr
071900020502     c* società
072000030207     c                   eval      %subst(cmd(2): 31: 3) = F54SOC
072100030207     c* coop inizio
072200030207     c                   eval      %subst(cmd(2): 50: 8) = F54pd1
072300030207     c* coop fine
072400030207     c                   eval      %subst(cmd(3): 12: 8) = F54pd2
072500020502     c* data inizio
072600020422     c                   move      f54dti        com8              8
072700030207     c                   eval      %subst(cmd(4): 11: 8) = com8
072800020502     c* data fine
072900020422     c                   move      f54dtf        com8
073000030207     c                   eval      %subst(cmd(4): 34: 8) = com8
073100020419     c* MI COMPONGO L'ISTRUZIONE SQL
073200020604     c* l'indicatore 80 mi serve per evitare la doppia ribattitura
073300020604     c* nella stampa della copia IVA
073400020503     c*
073500040217     c* se stampa per libro IVA non controllo data di stampa           ssere
073600040217     c* altrimenti deve essere a 0 (CMD,8)
073700020503     c                   if        f54uni = '999     '
073800020604     c                   seton                                        80
073900020419     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
074000030207     C                             +CMD(4)+CMD(8)+CMD(5)+CMD(6)
074100020503     c                   else
074200020604     c                   setoff                                       80
074300020503     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
074400030207     C                             +CMD(4)+CMD(7)+CMD(5)+CMD(6)
074500020503     c                   end
074600020419     C                   ENDSR
074700020419     C*----------------------------------------------------*
074800030212     C* valorizzazione giornaliera cooperative ficn22r
074900020419     C*----------------------------------------------------*
075000030212     C     srficn22      BEGSR
075100030212     c                   clear                   ficn20ds
075200030212     c                   eval      f20ela = 'R'
075300030212     c                   eval      f20cda = minpdr
075400030212     c                   eval      f20ca  = maxpdr
075500030212     c                   eval      f20dti = minddc
075600030212     c                   eval      f20dtf = maxddc
075700030212     c                   eval      f20ore = 'N'
075800030212     c                   eval      f20com = 'N'
075900030212     c                   eval      f20det = 'S'
076000030212     c                   eval      f20rie = 'N'
076100030212     c                   eval      f20soc = xscsoc
076200030212     c                   eval      f20rsc = desric
076300030212     c                   eval      f20for = cdfs
076400030212     c                   eval      f20rgs = desemi
076500030212     c                   eval      f20nff = nrfat
076600030212     c                   move      dtfat         dataeur
076700030212     c                   move      dataeur       dataiso
076800030212     c                   move      dataiso       f20dft
076900030212     c                   movel(p)  ficn20ds      kpjbu
077000030212     c                   call      'FICN22R'
077100030212     c                   parm                    kpjba
077200030212     C                   ENDSR
077300030212     C*----------------------------------------------------*
077400030212     C* Reperimento dati società
077500030212     C*----------------------------------------------------*
077600030212     C     REPSOC        BEGSR
077700020419     C*
077800020419     C                   CALLB     'XSOC'
077900020419     C                   PARM                    TIPXSC            6
078000020419     C                   PARM                    SOCXSC            3
078100020419     C                   PARM                    CDSXSC            9 0
078200020419     C                   PARM                    MODXSC            3
078300020419     C                   PARM      *blanks       RTNXSC            1
078400020419     C                   PARM                    XSOCDS
078500020419     C                   PARM                    KPJBA
078600110502     C     RTNXSC        IFNE      '1'
078700110502     C                   MOVEL     XSOCDS        SOC001
078800110502     c* decodifico i dati della società
078900110502     c                   exsr      Decod_societa
079000110502     c                   if         PRMRPYIDMSG >= 0
079100110502     c                              and oIDSOCIETA <> *blank
079200110502     c*
079300110502     C                   MOVEL     oRAGSOCIALE   RSUT             20
079400110502     c                   movel     oRAGSOCIALE   desric
079500110502     c                   movel     oSEDLEGIND    indric
079600110502     c                   movel     oSEDLEGCAP    capric
079700110502     c                   movel     oPARTITAIVA   piric
079800110502     c                   movel     oCODFISCALE   cfric
079900110502     c     cfric         comp      ' '                                8989
080000110502     c                   if        oSEDLEGPRO = ' '
080100110502     C                   movel     oSEDLEGLOC    locric
080200110502     C                   ELSE
080300110502     C                   eval      locric = %trimr(osedlegloc)+
080400110502     C                             ' ('+osedlegpro+')'
080500110502     c                   end
080600110502     c                   end
080700110502     c                   end
080800020419     C*
080900020419     C                   ENDSR
081000110502     C**************************************************************************
081100110502     C* decodifica la società
081200110502     C************************************************************
081300110502     C     Decod_societa begSR
081400110502     ** Inizializzo il programma.
081500110502     C                   CALL      'TIBSSOCR'
081600110502     C                   PARM      'INIT'        prmRqsOpCode
081700110502     C                   PARM                    prmRpyOpCode
081800110502     C                   PARM                    prmRpyIdMsg
081900110502      *
082000110502     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
082100110502     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
082200110502     c* forzo società 201
082300110502     C                   EVAL      IidSocieta =f54soc
082400110502     C                   move      F54DTI        IDTVALIDITA
082500110502     C
082600110502     C                   CALL      'TIBSSOCR'
082700110502     C                   PARM      'GETANAGRAF'  prmRqsOpCode
082800110502     C                   PARM      *BLANK        prmRpyOpCode
082900110502     C                   PARM      *ZERO         prmRpyIdMsg
083000110502     C                   PARM      'TIBSSOCI0'   prmRqsFormato
083100110502     C                   PARM                    tibsSocI0
083200110502     C                   PARM                    prmRqsDataSize
083300110502     C                   PARM      'TIBSSOCO0'   prmRpyFormato
083400110502     C                   PARM                    tibsSocO0
083500110502     C                   PARM                    prmRpyDataSize
083600110502      *
083700110502     C                   ENDSR
083800030207     Ofictt000  E            aggio
083900030207     o                       cttdcn
084000020502     OQSYSPRT   E            TESTE          2 02
084100020422     O                       RSUT                20
084200030616     O                                         + 25 'LISTA FATTURE EMESSE PER C'
084300030616     O                                           +0 'OOPERATIVE'
084400030616     O                                          113 'FICN65R'
084500020422     O                       UDATE              127 '  /  /  '
084600020422     O                       PAGE1         Z    132
084700020422     O          E            teste       1  2
084800020503     o                                         +  0 'Fornitore'
084900021010     o                                         + 46 'Fattura'
085000020503     o                                         +  1 'Data'
085100020503     o                                         +  8 'Firma'
085200021010     o                                         + 37 'Data consegna'
085300020503     O          E            DET         3
085400020503     O                       salfor            +  0
085500020422     O                       desemi            +  1
085600020422     O                       nrfat         4   +  1
085700020503     O                       dtfat             +  1 '  /  /    '
085800020503     O                                         +  2 '__________________________'
085900021010     O                                         +  0 '______________'
086000021010     O                                         +  2 '_______________'
086100020419**
086200030207SELECT ctttsr, sum(cttitc+cttitma+cttitmp), cttnra, cttcdf            1
086300030207from fictt06l WHERE cttsoc = '000' and cttcdf >='00000000' and        2
086400030207cttcdf <= '00000000' and cttfvl = 'C' and                             3
086500030207cttdft >= 00000000 and cttdft <= 00000000 and cttnff <> 0             4
086600030207GROUP BY cttcdf, cttnra, ctttsr                                       5
086700030207ORDER BY cttcdf, cttnra, ctttsr                                       6
086800030207and cttdcn = 0                                                        7
086900040217                                                                      8
087000030623** decm
087100030623gennaio
087200030623febbraio
087300030623marzo
087400030623aprile
087500030623maggio
087600030623giugno
087700030623luglio
087800030623agosto
087900030623settembre
088000030623ottobre
088100030623novembre
088200030623dicembre
