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
001500160121     Ftntbe01L  IF   E           K DISK
001600030207     Ftabel00f  IF   E           K DISK
001700030207     Ffictt06L  uF   E           K DISK
001800160119     Ftnsre03L  uF   E           K DISK
001900070606     ftnanf01L  uF a E           K DISK
002000030207     Fficn65p   O    e             PRINTER OFLIND(*IN23)
002100020422     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
002200900515      *
002300020606     D KPJBA         E DS
002400071120     D ficn99ds      E DS
002500020422?     *  Tabella Y4Z
002600020422     D angy4z        E DS                  extname(angy4zds)
002700020528     D dtlz01        e DS
002800020528     D xatbds        e DS
002900020422     D soc001        E DS                  EXTNAME(xsoc001ds)
003000070606     D danf0         e DS
003100160119     D dsrexaf       e DS
003200160121     D dsre          e DS
003300020422     D xsocds          DS          1000
003400020502     D*ABI/CAB
003500020502     D X59ABIDS      E DS                  INZ
003600020502  "  D* DS per input condizione di pagamento
003700020502  "  DDVCDPDS        E DS                  INZ
003800020502  "  D* Ds per output
003900030207  "  DDVOCDPDS       E ds                  inz
004000030207     c*
004100030207  "  Dds1z           E DS                  INZ
004200020422?     *  Tabella IVA
004300020422     DTABG48         E DS                  EXTNAME(ANGG48DS)
004400020422     D X06DS         E DS                  EXTNAME(X06G48DS)
004500020422     D  X06SOC       E                     EXTFLD(X06SOCIETA)
004600020422     D  X06CIV       E                     EXTFLD(X06CDIVA)
004700020422     D  X06DAT       E                     EXTFLD(X06DATA)
004800020422     D  X06LIN       E                     EXTFLD(X06LINGUA)
004900900516      *
005000020419     D Ficn54ds      E DS
005100030212     D Ficn20ds      E DS
005200070606     D codfac          S                   like(RCOkscfact)
005300020422     d dataiso         s               d
005400020422     d dataeur         s               d   datfmt(*eur)
005500020422     D conta           S              3  0 inz
005600020422     D comodo          S              9  2 inz
005700020422     D salnra          S                   like(nrass)
005800150121     D savSQLCOD       S                   like(SQLcod)
005900030207     D com03           S              3    inz
006000020422     D salfor          S              8    inz
006100020614     D salfors         S                   like(salfor)
006200071221     D comdta          S                   like(ivadtdoc) inz(d'2007-12-31')
006300020502     D kcc             S                   inz like(rcokcc)
006400160119     D dtaiso          s               d   inz
006500160119     D dtaeur          s               d   datfmt(*eur) inz
006600160118     d  desdocds       ds
006700160119     d   cost1                        8    inz('Fattura ')
006800160119     d   nfat                         9
006900160119     d   cost2                        5    inz(' del ')
007000160119     d   dfat                        10
007100020419     c* SQL
007200020419     D cmd             s             70    ctdata dim(10)                       FIL. COMODO
007300030623     D decm            s             10    ctdata dim(12)                       FIL. COMODO
007400020419     D WrkSqlCmd       S           1024
007500030207     D minddc          S                   like(cttddc)
007600030207     D maxddc          S                   like(cttddc)
007700030212     D minpdr          S                   like(cttpdr)
007800030212     D maxpdr          S                   like(cttpdr)
007900150120     d in_nra          S                   like(cttnra)
008000150120     d in_cdf          S                   like(cttcdf)
008100150120      **
008200030207     D fictt           DS
008300030207     d tsrs                                like(ctttsr)
008400030207     d tots                                like(cttitc)
008500030207     d nrass                               like(cttnra)
008600030207     d cdfs                                like(cttcdf)
008700110502     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
008800110502     D ESITO_OK...
008900110502     D                 C                   0
009000110502     ***************************************************************************
009100110502     **
009200110502     ** Dichiarazione prototipi procedure esterne.
009300110502     **
009400110502     ***************************************************************************
009500110502      /DEFINE DFTACTGRP_YES
009600110502     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
009700110502      /UNDEFINE DFTACTGRP_YES
009800110502     ***************************************************************************
009900110502     **
010000110502     ** Definizione strutture dati.
010100110502     **
010200110502      **************************************************************************
010300110502     D tibsSocI0...
010400110502     D               E DS                  prefix(i)
010500110502     D                                     INZ
010600110502     D tibsSocO0...
010700110502     D               E DS                  prefix(o)
010800110502     D                                     INZ
010900110502     ***************************************************************************
011000110502     **
011100110502     ** Definizione variabili modulo/programma.
011200110502     **
011300110502     ***************************************************************************
011400110502     D prmRqsOpCode...
011500110502     D                 S             10A
011600110502     D prmRpyOpCode...
011700110502     D                 S             10A
011800110502     D prmRpyIdMsg...
011900110502     D                 S             10I 0
012000110502     D prmRqsFormato...
012100110502     D                 S             10A
012200110502     D prmRqsDataSize...
012300110502     D                 S             10I 0
012400110502     D prmRpyFormato...
012500110502     D                 S             10A
012600110502     D prmRpyDataSize...
012700110502     D                 S             10I 0
012800110502
012900011031     ***********************************************************************
013000910521     C                   EXSR      DEFCAM
013100030207     c* Lettura fictt06l ordinato per fornitore tipo servizio
013200030207     c* i record scelti sono: FTTFVL='C', limiti scelti a
013300020419     c* video per fornitore e data protocollo, numero fattura <> 0
013400020419     c*
013500020419     C/EXEC SQL
013600020419     C+ PREPARE S1 FROM :WrkSqlCmd
013700020419     C/END-EXEC
013800020419
013900020419     C/EXEC SQL
014000020419     C+ DECLARE A1 CURSOR FOR S1
014100020419     C/END-EXEC
014200020419
014300020419     C/EXEC SQL
014400020419     C+ OPEN A1
014500020419     C/END-EXEC
014600020419
014700020419     C                   DOU       SqlCod <> 0
014800020419     C/EXEC SQL
014900030207     C+ FETCH NEXT FROM A1 INTO :fictt
015000020419     C/END-EXEC
015100020419     c*
015200020419     C                   SELECT
015300020419     C                   WHEN      SqlCod = 0
015400030207     c* se nr.registrazione = 0 aggiorno data stampa ed esco
015500030207     c                   if        nrass = 0
015600020614     c                   movel     salfor        salfors
015700020614     c                   movel     cdfs          salfor
015800020614     c                   exsr      sragg
015900020614     c                   movel     salfors       salfor
016000020531     c                   iter
016100020531     c                   end
016200020419     c* rottura fornitore
016300020422     c                   if        cdfs <> salfor or nrass <> salnra
016400020419     c                   exsr      srrotfor
016500020419     c                   end
016600030723     c* se la fattura è = 0 leggo un altro fornitore
016700030723     c                   if        impfat = 0
016800030723     c                   iter
016900030723     c                   end
017000020422     c* dettaglio campi
017100020422     C                   EXSR      srcampi
017200020701     c* stampo la riga solo se il totale è <> 0
017300020701     c                   if        imptg <> 0
017400020422     c                   add       1             conta
017500020422     c                   seton                                        01
017600020422     c                   exsr      lista
017700020701     c                   end
017800020422     c*
017900020419     C                   WHEN      SqlCod = 100
018000020419     C                   WHEN      SqlCod <> 0
018100020419      * Forzo la stampa del JOBLOG.
018200020419     C                   CALL      'X66CHGJOB'
018300020419     C                   ENDSL
018400020419     C                   ENDDO
018500020419     C/EXEC SQL
018600020419     C+ CLOSE A1
018700020419     C/END-EXEC
018800020422     C* gestisco gli ultimi totali
018900020423     c                   if        impfat <> 0
019000020422     c                   exsr      srrotfor
019100020423     c                   end
019200030212     c* chiudo pgm FICN22R
019300030212     c                   clear                   ficn20ds
019400030212     c                   eval      f20ela = 'C'
019500030212     c                   movel(p)  ficn20ds      kpjbu
019600030212     c                   call      'FICN22R'
019700030212     c                   parm                    kpjba
019800030509     c* rimposto la kpjbu iniziale
019900030509     c                   eval      kpjbu = ficn54ds
020000110502     ** Chiudo il programma.
020100110502     C                   CALL      'TIBSSOCR'
020200110502     C                   PARM      'FINALIZE'    prmRqsOpCode
020300110502     C                   PARM                    prmRpyOpCode
020400110502     C                   PARM                    prmRpyIdMsg
020500110502     c*
020600020422     c*
020700910521     C                   SETON                                        LR
020800020419     C**************************************************************************
020900020419     C* Rottura fornitore
021000020419     C**************************************************************************
021100020419     C     Srrotfor      BEGSR                                                  *
021200020419     c* stampa la riga di totale
021300020419     c                   if        impfat <> 0
021400030207     c                   seton                                        02
021500020423     c                   else
021600020423     c                   z-add     0             conta
021700020419     c                   end
021800020419     c*
021900020701     c                   if        conta <> 60 and conta <> 0
022000020701     c* salto pagina per avere il totale fattura tutto insieme
022100040330     c                   if        conta > 24
022200020701     c                   seton                                        23
022300020701     c                   end
022400020701     c                   exsr      lista
022500020502     c* aggiorno la data di stampa
022600020614     c                   exsr      sragg
022700020701     c                   end
022800020422     c*
022900020419     c                   z-add     60            conta
023000020509     c                   setoff                                       23
023100020422     c* azzera il conteggio per fornitore
023200020502     c                   z-add     0             page
023300020502     c                   z-add     0             impon
023400020422     c                   z-add     0             impiva
023500020422     c                   z-add     0             impfat
023600020419     c* decodifico fornitore
023700020419     c                   exsr      srfor
023800020423     c* aggancio registrazione solo se fornitore ok
023900020423     c                   if        not *in20
024000030207     c                   z-add     nrass         ivanrasreg
024100030207     c     kiva          chain     ndiva00f
024200030207     c                   if        %found(ndiva00f)
024300020422     c                   move      ivadtdoc      dataeur
024400020422     c                   move      dataeur       dtfat
024500020422     c                   move      ivanrdoc      nrfat
024600020422     c                   move      ivadtreg      dataeur
024700020422     c                   move      dataeur       dtprt
024800020422     c                   move      ivanrreg      nrprt
024900161116     c                   move      ivalibro      ivalib
025000161115     c                   Z-ADD(h)  ivaimporto    impiva
025100161115     c                   Z-ADD(h)  ivaimponib    impon
025200150109      **
025300150112     c                   clear                   fatpul
025400150112     c                   clear                   ivapul
025500150112     c                   clear                   imppul
025600150112     c                   clear                   aliiva
025700150120     c                   clear                   fatt_Pulizie      1
025800150109      **  per solo le Pulizie NON deve comprendere l'IVA
025900150109      **  il Totale equivale all'imponibile
026000150109     c                   if        Tsrs = 'P'
026100150120     c                   move      'S'           fatt_Pulizie
026200150109     c                   Z-ADD     impon         impfat
026300150109     c     impon         add       impiva        fatpul
026400150109     c                   z-add     impiva        ivapul
026500150109     c                   z-add     impon         imppul
026600150112     c                   movel     desIVA        aliIVA
026700150109     c                   else
026800150112      *
026900150112      * normalmente invece
027000020422     c     impon         add       impiva        impfat
027100150112      *
027200150109     c                   end
027300150109      **
027400020422     c                   end
027500030207     c* condizioni di pagamento: aggancio registrazione xchè la cond. pag.
027600030207     c* non è detto che sia quella del fornitore visto che è in I/O nella
027700030207     c* contabilizzazione
027800030207     c     kiva          chain     ndreg00f
027900030207     c                   if        %found(ndreg00f)
028000030207  "  C                   reset                   DVCDPDS
028100030207  "  C                   eval      DCPRIC='1'                                   chain
028200030207  "  C                   eval      DCPSOC=XSCSOC
028300030207  "  C                   eval      DCPUNI=frnunita
028400030207  "  C                   eval      DCPVIS='1'                                   lf principale
028500030207  "  C                   eval      DCPALC=*OFF                                  no allocazione
028600030207  "  C                   eval      DCPNRK=1                                     numero chiavi
028700030207  "  C                   eval      DCPLIN=xsclin
028800030207  "  C                   eval      DCPCOD=regcondpag
028900030207  "  C                   eval      DCPERR=*OFF
029000030207  "  C                   CALL      'DVCDP'
029100030207  "  C                   PARM                    DVCDPDS
029200030207  "  C                   PARM                    DVOCDPDS
029300030207  "  C                   if        DCPERR=*OFF
029400030207  "  C                   eval      Descon = CDPdesbrev
029500030207     c                   else
029600030207     c                   clear                   descon
029700030207     C                   end
029800030207     C                   end
029900030207     C                   end
030000030207     c*
030100030212     c                   if        impfat <> 0 and
030200030212     c                             (cdfs <> salfor or nrass <> salnra)
030300030207     c                   movel     cdfs          salfor
030400030207     c                   z-add     nrass         salnra
030500030212     c* stampo valorizzazione conteggi cooperative
030600030212     c                   exsr      srficn22
030700020531     c                   end
030800030527     c* se escluso da autofatturazione (ind. 21 acceso) azzero l'importo
030900030527     c* della fattura così passo al prossimo fornitore
031000030527     c   21              clear                   impfat
031100030527     c*
031200020419     c                   ENDSR
031300020419     C**************************************************************************
031400030207     C* aggiorna data di stampa in FICTT00F
031500020419     C**************************************************************************
031600020614     C     sragg         BEGSR                                                  *
031700020614     c*
031800030207     c     kctt          setll     fictt06l
031900020614     c                   do        *hival
032000030207     c     kctt          reade     fictt06l
032100030207     c                   if        %eof(fictt06l)
032200020614     c                   leave
032300020614     c                   end
032400020614     c* escludo quelli non fatturati oppure già stampati
032500030207     c                   if        cttnff = 0 or cttdcn <> 0
032600020614     c                   iter
032700020614     c                   end
032800020614      * controllo numero registrazione
032900030207     c                   if        cttnra = salnra
033000020614     c                   move      *date         dataiso
033100030207     c                   move      dataiso       cttdcn
033200020614     c                   except    aggio
033300030207     c                   end
033400020614     c                   enddo
033500071120     c* stampo la lettera x fusione solo se data fattura = 31/12/2007
033600071221     c                   if        ivadtdoc = comdta and
033700071221     c                             f54uni <> '999     '
033800071120     c                   eval      f99socn = f54soc
033900071120     c                   eval      f99kscn = salfor
034000071120     c                   eval      f99tip = 'C'
034100071120     c                   movel(p)  ficn99ds      kpjbu
034200071120     c                   call      'FICN99R'
034300071120     c                   parm                    kpjba
034400071221     c                   end
034500020614     c*
034600020614     c                   ENDSR
034700020419?     *--------------------------------------------------------------*
034800020419?     *  dati in testata                                             *
034900020419?     *--------------------------------------------------------------*
035000020419     C     srfor         BEGSR
035100020419      *
035200030527     c                   setoff                                       21
035300020502     c                   eval      rcoksc = cdfs
035400020422     C     Kfrn          CHAIN     anfrn01l                           20
035500020422     C  N20Krco          CHAIN     anrco98j                           20
035600020423     c* controllo se il fornitore appartiene all'unità di reg. scelta
035700020423     c* se 999 tutte le unità
035800020423     c                   if        not *in20 and f54uni <> '999     '
035900020423     c                             and rcounita <> f54uni
036000020423     c                   seton                                        20
036100020423     c                   end
036200020528     c* se 999 tutte le unità (vuol dire che sono in sede) controllo
036300020528     c* che il fornitore sia in autofatturazione
036400090707     C                   CLEAR                   DTLZ01
036500020531     c                   if        not *in20
036600020528     c                   move      cdfs          tlzpdr
036700020528     c     ktlz          chain     tntlz01l
036800020528     c                   if        %found(tntlz01l)
036900020528     c                   movel     tlzflo        dtlz01
037000020531     c                   if        §tlzfl1 = 'S'
037100020531     c                             and f54uni = '999     '
037200030527     c                   seton                                        21
037300020528     c                   end
037400020528     c                   end
037500020528     c                   end
037600020423     c*
037700020423     c                   if        not *in20
037800091109     C                   if        %subst(indtelex: 1: 8) = 'CAP.SOC.'
037900091109     c                   eval      csemi = indtelex
038000091109     c                   else
038100091201     c* per l'ottico non può essere blanks
038200091201     c                   eval      csemi ='.'
038300091109     c                   end
038400020419     C                   MOVEL     sogdes        desemi
038500020422     C                   eval      indemi = indindriz
038600020422     c                   if        indprov <> ' '
038700020422     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
038800020422     c                   else
038900020422     C                   eval      locemi = indlocalit
039000020422     c                   end
039100020422     C                   eval      capemi = indcap
039200091119     c*
039300091119     C                   movel     sogpartiva    piemi            20
039400091119     C                   MOVEL     indprov       premi             2
039500091119     C                   movel     sogcdfisc     cfemi            20
039600161122      *-
039700161122     c                   exsr      Retrieve_PEC
039800161122      *-
039900091119     C                   if        cfemi <> ' '
040000091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
040100091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
040200091119     C                             ': ' + %trimr(cfemi)
040300091119     c                   else
040400091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
040500091119     c                   end
040600030523     c* se il fornitore ha un codice IVA
040700030523     C                   CLEAR                   tabg48
040800030523     c                   if        frncdiva <> *blanks
040900030523     C                   CLEAR                   X06DS
041000030523     C                   MOVE      xscsoc        X06SOC
041100030523     C                   MOVE      XSCLIN        X06LIN
041200030523     C                   MOVE      frncdiva      X06CIV
041300030523     C                   MOVE      f54dti        X06DAT
041400030523     C                   CALL      'X06G48'
041500030523     C                   PARM                    X06DS
041600030523     C     X06ERR        IFEQ      '0'
041700030523     C                   MOVEL     X06UNI        TABG48
041800030523     C                   end
041900030523     C                   end
042000030523     c* aggancio la tabella Y4Z con tipo servizio X
042100030523     c                   exsr      sry4z
042200030523     c* aliquota IVA
042300030523     c                   if        frncdiva = *blanks or alig48 <> 0
042400030523     c                   if        alig48 = 0
042500030523     c                   move      §4ziva        comiva            5
042600030523     c                   else
042700030523     c                   move      alig48        comiva
042800030523     c                   end
042900030523     c                   clear                   comiva1           5
043000030523     c                   eval      comiva1 = %subst(comiva: 2: 2)+ ',' +
043100030523     c                             %subst(comiva: 4: 2)
043200030523     c                   eval      desiva = 'Aliquota ' + comiva1 + '%'
043300161121     c                   evalR     desiva = %trim(desiva)
043400030523     c                   else
043500030523     c                   eval      desiva = desg48
043600030523     C                   end
043700020502     c* banca d'appoggio  INDICATORE 11 x FACTOR
043800020502     c                   if        rcokscfact <> *blanks
043900020502     c                   seton                                        11
044000020502     c                   eval      rcoksc = RCOkscfact
044100020502     C     Krco          CHAIN     anrco98j
044200020502     c                   if        %found(anrco98j)
044300020502     C                   MOVEL     sogdes        desfac
044400070606     C                   MOVEL     rcoksc        codfac
044500020502     c                   end
044600020502     c                   else
044700020502     c*banca d'appoggio in alternativa al factor
044800030207     c                   setoff                                       11
044900020502     C     KCBA          CHAIN     ANCBA01L
045000020502     c                   if        %found(anCBA01L) AND
045100020502    >C                             CBAABI <> *Blank AND
045200020502    >C                             CBACAB <> *Blank
045300020502    >C                   Move      CBAABI        X59ABI
045400020502    >C                   Move      CBACAB        X59CAB
045500020502A0692C                   Move      XSCSOC        X59SOC
045600020502    >C                   Call      'X59ABI'
045700020502    >C                   Parm                    X59ABIDS
045800020502     C*
045900020502    >C                   If        X59Err = '1'
046000090928    >C                   CLEAR                   iban
046100090928    >C                   CLEAR                   ABI
046200020502    >C                   CLEAR                   CAB
046300020503    >C                   CLEAR                   nrcc
046400020502    >C                   CLEAR                   DESIST
046500020502    >C                   CLEAR                   DESAGE
046600020502    >C                   Else
046700020502     C                   Movel     X59DesIst     DESIST
046800020502     C                   Movel     X59DesAge     DESAGE
046900090928     C                   Movel     CBAiban       iban
047000090928     C                   Movel     CBAABI        ABI               5
047100090928     C                   Movel     CBACAB        CAB               5
047200090928     C                   Movel     CBACcB        nrcc             18
047300020502    >C                   End
047400020502     c                   end
047500020502     c                   end
047600030207     c* decodifica po
047700030207     c                   eval      com03 = %subst(cdfs: 2: 3)
047800030207     c                   move      com03         orgfil
047900030207     c     orgfil        chain     azorg01l
048000030207     c                   if        %found(azorg01l)
048100030207     C                   eval      despo = %trimr(orgdes) + ' ' +
048200030207     C                             %trimr(orgind) + ' ' +
048300030207     C                             %trimr(orgloc)
048400030207     c                   end
048500150121      *-
048600150121      ** salva sqlCOD dell'SQL principale
048700150121     c                   eval       savSQLCOD = SQLcod
048800150121      *-
048900030207     c* reperisco la data minima e la data massima della prestazione fattur.
049000030207     c                   if        f54uni = '999     '
049100030207     C/EXEC SQL
049200030212     C+ SELECT min(cttddc), max(cttddc), min(cttpdr), max(cttpdr),
049300150121     C+ cttcdf, cttnra INTO :minddc, :maxddc, :minpdr, :maxpdr FROM
049400030212     C+ fictt06l WHERE cttcdf = :cdfs and cttnra = :nrass and cttfvl =
049500040217     C+ 'C' and cttdft between :f54dti and :f54dtf and cttnff <> 0 GROUP
049600040217     C+ BY cttcdf, cttnra ORDER BY cttcdf, cttnra
049700030207     C/END-EXEC
049800030207     c                   else
049900030207     c* data di stampa = 0
050000030207     C/EXEC SQL
050100030212     C+ SELECT min(cttddc), max(cttddc), min(cttpdr), max(cttpdr),
050200150121     C+ cttcdf, cttnra INTO :minddc, :maxddc, :minpdr, :maxpdr FROM
050300030212     C+ fictt06l WHERE cttcdf = :cdfs and cttnra = :nrass and cttfvl =
050400030212     C+ 'C' and cttdcn = 0 and cttdft between :f54dti and :f54dtf and
050500030212     C+ cttnff <> 0 GROUP BY cttcdf, cttnra ORDER BY cttcdf, cttnra
050600030207     C/END-EXEC
050700020423     C                   end
050800150121      *
050900150121      *- reimposta SQLcod del SQL Principale
051000150121     c                   eval       SQLcod = savSQLCOD
051100150121      *-
051200030623     c* decodifico il mese
051300030623     c                   move      maxddc        dataiso
051400030623     c                   extrct    dataiso:*m    mm                2 0
051500030623     c                   movel(p)  decm(mm)      pmese
051600150123     c                   extrct    dataiso:*y    yyyy              4 0
051700150123     c                   movel     yyyy          PANNO
051800030207      *
051900030207     C                   end
052000020419      *
052100020419     C                   ENDSR
052200070606      *-----------------------------------------------------***********
052300070606      * memorizzo storico testata anagrafico fornitore solo se no copia iva
052400070606      *-----------------------------------------------------***********
052500070606     C     SRanf0        BEGSR
052600070606     c                   eval      ANFTIP = 'C'
052700070607     c                   eval      ANFTsr = 'C'
052800070606     c                   eval      ANFTrc = '0'
052900070606     c                   eval      ANFSOC = f54soc
053000070905     c                   eval      ANFCDF = salfor
053100070606     c                   eval      ANFNFF = ivanrdoc
053200070606     c                   move      ivadtdoc      ANFDFT
053300070606     c                   clear                   anfpdr
053400070606     c                   clear                   danf0
053500070606     c     kanf0         chain     tnanf01l
053600070606     c                   if        f54uni <>'999     '
053700091111     c                   eval      §ANF0telex = csemi
053800091111     c                   eval      §ANF0prov  = premi
053900070606     c                   eval      §ANF0DES = desemi
054000070606     c                   eval      §ANF0IND = indemi
054100070606     c                   eval      §ANF0CAP = capemi
054200070606     c                   eval      §ANF0LOC  = locemi
054300070606     c                   eval      §ANF0PI   = piemi
054400070606     c                   eval      §ANF0CF   = cfemi
054500161122     c                   eval      §ANF0pec  = peceml
054600161122     c                   eval      §ANF0lIVA = ivaLIB
054700070606     c                   eval      §ANF0CDP  = frncondpag
054800070606     c                   eval      §ANF0DCP  = descon
054900070606     c                   if        *in11
055000070606     c                   eval      §ANF0FAC  = codfac
055100070606     c                   eval      §ANF0DFAC = desfac
055200070606     c                   else
055300090928     c                   eval      §ANF0iban = iban
055400070606     c                   eval      §ANF0ABI  = abi
055500070606     c                   eval      §ANF0CAB  = cab
055600070606     c                   eval      §ANF0NRC  = nrcc
055700070606     c                   eval      §ANF0IST  = desist
055800070606     c                   eval      §ANF0AGE  = desage
055900070606     c                   end
056000070606     c                   eval      §ANF0NRP  = nrprt
056100070606     c                   move      ivadtreg      §ANF0DRP
056200070606     c                   eval      anfuni = danf0
056300070606     c                   if        %found(tnanf01l)
056400070606     c                   update    tnanf000
056500070606     c                   else
056600070606     c                   write     tnanf000
056700070606     c                   end
056800070606     c                   else
056900070606     c* imposto i dati di stampa come memorizzati
057000070606     c                   if        %found(tnanf01l)
057100070606     c                   eval      danf0 = anfuni
057200091111     c                   eval      csemi = §ANF0telex
057300070606     c                   eval      desemi      = §ANF0DES
057400070606     c                   eval      indemi      = §ANF0IND
057500070606     c                   eval      capemi      = §ANF0CAP
057600070606     c                   eval      locemi     = §ANF0LOC
057700091119     c                   eval      premi = §ANF0prov
057800070606     c                   eval      piemi      = §ANF0PI
057900070606     c                   eval      cfemi      = §ANF0CF
058000091119     C                   if        cfemi <> ' '
058100091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
058200091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
058300091119     C                             ': ' + %trimr(cfemi)
058400091119     c                   else
058500091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
058600091119     c                   end
058700070606     c                   eval      descon     = §ANF0DCP
058800090928     c                   eval      iban       = §ANF0iban
058900070606     c                   eval      abi        = §ANF0ABI
059000070606     c                   eval      cab        = §ANF0CAB
059100070606     c                   eval      nrcc       = §ANF0NRC
059200161122     c                   eval      peceml     = §ANF0pec
059300070606     c                   eval      desist     = §ANF0IST
059400070606     c                   eval      desage     = §ANF0AGE
059500070606     c                   eval      desfac    = §ANF0DFAC
059600070606     c                   end
059700070606     c                   end
059800070606     C                   ENDSR
059900161116      *----------------------------------------------------------------------
060000161116      *   reperisce la PEC se c'è
060100161116      *----------------------------------------------------------------------
060200161116     C     RETRIEVE_PEC  BEGSR
060300161116      *
060400161116     c                   eval        PECEML = 'PEC:'
060500161116     C                   if        cfemi <> ' '
060600161116     C                   eval       prmI_CFEMI = cFEMI
060700161116     C                   clear                   prmO_PECEML
060800161116     C                   CALL      'FICNPECR'
060900161116     C                   PARM                    prmI_CFEMI       20
061000161116     C                   PARM                    prmO_PECEML     100
061100161116      *
061200161116     C                   eval       pecEML = %trim(pecEML) + %trim(prmO_PECEML)
061300161116     c                   end
061400161116      *
061500161116     C                   ENDSR
061600030523?     *--------------------------------------------------------------*
061700030523?     *  lettura tabella Y4Z tipo aservizio X                        *
061800030523?     *--------------------------------------------------------------*
061900030523     C     sry4z         BEGSR
062000030523     c*  lettura tabella Y4Z
062100030523     c                   clear                   xatbds
062200030523     c                   move      '1'           xtbric
062300030523     c                   move      *blank        xtbkey
062400030523     c                   move      'Y4Z'         xtbcod
062500030523     c                   movel     'X'           xtbkey
062600030523     c                   call      'XATB'
062700030523     c                   parm                    xatbds
062800030523     c                   if        xtberr = '0'
062900030523     c                   movel     xtbuni        angy4z
063000030523     c                   else
063100030523     c                   clear                   angy4z
063200030523     c                   end
063300030523     C*
063400030523     C                   ENDSR
063500020419      *-----------------------------------------------------***********
063600020419      * Campi per stampa
063700020419      *-----------------------------------------------------***********
063800020419     C     SRCAMPI       BEGSR
063900030207     C                   Z-ADD     Tots          imptg
064000030207     c* decodifico il tipo servizio
064100030207     c                   eval      tblkey = tsrs
064200030207     c     ktab          chain     tabel00f
064300030207     c                   if        %found(tabel00f)
064400030207     c                   movel     tbluni        ds1z
064500030207     c                   movel     §1zdes        destsr
064600030207     c                   end
064700030416     c* descrizione aggiuntiva solo per tsr = X e P
064800030416     c                   if        tsrs = 'X' or tsrs = 'P' or tsrs = 'T'
064900030207     c                   eval      desagg = 'come da allegata valorizzazione'
065000030207     c                   else
065100030207     c                   clear                   desagg
065200030207     c                   end
065300020419     C                   ENDSR
065400020422     C**************************************************************************
065500020422     C* Gestione stampa
065600020422     C**************************************************************************
065700020422     C     LISTA         BEGSR                                                  *
065800020509     c                   if        conta>60 or *in23
065900030207     c                   seton                                        30
066000020422     c                   z-add     1             conta
066100020422     c                   end
066200070606     c* memorizzo nello storico anagrafiche
066300070606     c   30              exsr      sranf0
066400020422     c* testata fattura
066500020422     c   30              write     testa
066600020509     c                   setoff                                       3023
066700020508     c* testata lista fatture emesse
066800020508     c                   if        f54lis = 'S' and *inof
066900020508     c                   except    teste
067000020508     c                   setoff                                       of
067100020508     c                   end
067200150120      *
067300160119     c   02              exsr      rettifiche
067400150120      *   fare REVERSE CHARGE
067500150120      *  se si tratta di una Fattura Pulizie e c'è un importo IVA (NON ESENTE)
067600150120     c                   if        fatt_Pulizie = 'S'
067700150120     c                             and ivapul > 0
067800150120      *
067900150120      *  x le pulizie delle COOP testa il campo di valore
068000150120      * preimpostato se si stava trattando la COOP x le PULIZIE
068100150120     c   02              write     totPUL
068200150120      *
068300150120     c                   else
068400150120      * altrimenti   normale emissione di
068500020509     c* totale FATTURA FORNITORE
068600150120     c   02              write     totGEN
068700150120     c                   end
068800150120      *
068900020531     c* riga dettaglio fatture emesse solo per fornitori in autofatturazione
069000030207     c                   if        f54lis = 'S' and *in02 and
069100020531     c                             §tlzfl1 = ' '
069200020422     c                   except    det
069300020422     c                   end
069400020422     c* dettaglio
069500020422     c   01              write     riga
069600030207     c                   movea     '00'          *in(01)
069700020422     c                   ENDSR
069800160118      *-----------------------------------------------------***********
069900160118      * rettifiche fatture fornitori
070000160118      *-----------------------------------------------------***********
070100160118     C     rettifiche    BEGSR
070200160119     c                   move      dtfat         dtaeur
070300160119     c                   move      dtaeur        dtaiso
070400160119     c                   move      dtaiso        dtfatymd          8 0
070500160119     c                   z-add     nrfat         nrfat9            9 0
070600160119     C/EXEC SQL
070700160119     C+ DECLARE R1 CURSOR FOR SELECT SRESRE, SREUNI, sum( SRETIM )
070800160119     C+    FROM tnsre00f WHERE
070900160119     C+    sRESOC = :f54soc and sretip = 'C'
071000160119     C+    and SRECDF = :salfor and SREDFT = :dtfatymd and SRENFF = :nrfat
071100160119     C+    GROUP BY SRESRE, SREUNI
071200160119     C/END-EXEC
071300160119      *
071400160119     C/EXEC SQL
071500160119     C+ OPEN R1
071600160119     C/END-EXEC
071700160119      *
071800160119     c                   do        *hival
071900160119     C/EXEC SQL
072000160119     C+ FETCH  next FROM R1 INTO :sresre, :sreuni, :sretim
072100160119     C/END-EXEC
072200160119     C                   if        SqlCod = 100
072300160119     c                   Leave
072400160119      *  x errori
072500160119     C                   elseif    SqlCod < 0
072600160119     c                   dump(a)
072700160119     c                   leave
072800160119     c                   endif
072900160119     c                   if        SQLCOD = 0
073000160119     c                   movel     sreuni        dsrexaf
073100160119     c                   eval      nfat = %editc(§srexafnf:'Z')
073200160119     c                   if        §srexafdf > 0
073300160119     c                   move      §SREXAFdf     dtaiso
073400160119     c                   move      dtaiso        dtaeur
073500160119     c                   move      dtaeur        dfat
073600160119     c                   endif
073700160121     c                   movel     sresre        tbeke1
073800160121     c                   movel     'SRE'         tbecod
073900160121     c     ktbe          chain     tntbe01l
074000160121     c                   if        %found(tntbe01l)
074100160121     c                   movel     tbeuni        dsre
074200160121     c                   eval      desret = §sredes
074300160121     c                   endif
074400160119     c                   movel     desdocds      desdoc
074500160119     c                   z-add     sretim        impret
074600160119     c                   write     rigaret
074700160119     c                   endif
074800160119     c                   enddo
074900160119     C/EXEC SQL
075000160119     C+ CLOSE R1
075100160119     C/END-EXEC
075200160118     c* decodifico il tipo servizio
075300160118     c     ksre          setll     tnsre03l
075400160118     c                   do        *hival
075500160118     c     ksre          reade     tnsre03l
075600160119     c                   if        %eof(tnsre03l)
075700160119     c                   leave
075800160118     c                   endif
075900160119     c                   move      *date         dataiso
076000160119     c                   move      dataiso       sredcn
076100160119     c                   update    tnsre000
076200160118     c                   enddo
076300160118     C                   ENDSR
076400011031     C**********************************************************************
076500910521     C     DEFCAM        BEGSR
076600011031     C**********************************************************************
076700000000     C     *ENTRY        PLIST
076800000000     C                   PARM                    KPJBA
076900020419     C                   MOVEL     KPJBU         ficn54ds
077000020419     C*---------- RICERCA DITTA :
077100020419     C                   MOVEL     'SOC001'      TIPXSC
077200020422     C                   MOVEL     f54soc        SOCXSC
077300020422     c     f54lis        comp      'S'                                    of
077400020419     C                   EXSR      REPSOC
077500160121      *
077600160121     c     ktbe          klist
077700160121     c                   kfld                    tbecod
077800160121     c                   kfld                    tbeke1
077900020419      *  Definizione chiave di accesso
078000021203     C     Ktlz          KLIST
078100021203     C                   KFLD                    tlztip
078200021203     C                   KFLD                    tlzpdr
078300021203     C                   KFLD                    f54soc
078400030207     c                   movel     'C'           tlztip
078500160118     C     Ksre          KLIST
078600160119     C                   KFLD                    f54soc
078700160119     C                   KFLD                    salfor
078800160119     C                   KFLD                    dtfatymd
078900160119     c                   kfld                    nrfat9
079000160118      *
079100030207     C     Kctt          KLIST
079200020502     C                   KFLD                    f54soc
079300020502     C                   KFLD                    salfor
079400030207     C                   KFLD                    cttfvl
079500030207     c                   movel     'C'           cttfvl
079600020502     C     Kfrn          KLIST
079700020502     C                   KFLD                    f54soc
079800020502     C                   KFLD                    cdfs
079900020419     C     Krco          KLIST
080000020422     C                   KFLD                    f54soc
080100020422     C                   KFLD                    rcosnatura
080200020502     C                   KFLD                    rcoksc
080300020422     c                   eval      rcosnatura = 'F'
080400020422     C     Kiva          KLIST
080500020422     C                   KFLD                    ivasys
080600020422     C                   KFLD                    ivanrasreg
080700020502     c                   eval      ivasys = 0
080800020502    >C     KCBA          KLIST
080900020502    >C                   KFLD                    XSCSOC
081000020502    >C                   KFLD                    CBACLIFOR
081100020502    >C                   KFLD                    KCC
081200020502    >C                   KFLD                    rcoKSC
081300020502    >C                   MOVEL     'F'           CBACLIFOR
081400030207     C*
081500030207     C     KTAB          KLIST
081600030207     C                   KFLD                    tblkut
081700030207     C                   KFLD                    tblcod
081800030207     C                   KFLD                    tblkey
081900030207     C                   Z-ADD     1             tblkut
082000030207     C                   eval      tblcod = '1Z'
082100070606      *  Definizione chiave di accesso
082200070606     c     kanf0         klist
082300070606     c                   KFLD                    ANFTIP
082400070607     c                   KFLD                    ANFTsr
082500070606     c                   KFLD                    ANFTrc
082600070606     c                   KFLD                    ANFSOC
082700070606     c                   KFLD                    ANFCDF
082800070606     c                   KFLD                    anfdft
082900070606     c                   KFLD                    ANFNFF
083000070606     c                   KFLD                    ANFpdr
083100020502     c* società
083200030207     c                   eval      %subst(cmd(2): 31: 3) = F54SOC
083300030207     c* coop inizio
083400030207     c                   eval      %subst(cmd(2): 50: 8) = F54pd1
083500030207     c* coop fine
083600030207     c                   eval      %subst(cmd(3): 12: 8) = F54pd2
083700020502     c* data inizio
083800020422     c                   move      f54dti        com8              8
083900030207     c                   eval      %subst(cmd(4): 11: 8) = com8
084000020502     c* data fine
084100020422     c                   move      f54dtf        com8
084200030207     c                   eval      %subst(cmd(4): 34: 8) = com8
084300020419     c* MI COMPONGO L'ISTRUZIONE SQL
084400020604     c* l'indicatore 80 mi serve per evitare la doppia ribattitura
084500020604     c* nella stampa della copia IVA
084600020503     c*
084700040217     c* se stampa per libro IVA non controllo data di stampa           ssere
084800040217     c* altrimenti deve essere a 0 (CMD,8)
084900020503     c                   if        f54uni = '999     '
085000020604     c                   seton                                        80
085100020419     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
085200030207     C                             +CMD(4)+CMD(8)+CMD(5)+CMD(6)
085300020503     c                   else
085400020604     c                   setoff                                       80
085500020503     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
085600030207     C                             +CMD(4)+CMD(7)+CMD(5)+CMD(6)
085700020503     c                   end
085800020419     C                   ENDSR
085900020419     C*----------------------------------------------------*
086000030212     C* valorizzazione giornaliera cooperative ficn22r
086100020419     C*----------------------------------------------------*
086200030212     C     srficn22      BEGSR
086300030212     c                   clear                   ficn20ds
086400030212     c                   eval      f20ela = 'R'
086500030212     c                   eval      f20cda = minpdr
086600030212     c                   eval      f20ca  = maxpdr
086700030212     c                   eval      f20dti = minddc
086800030212     c                   eval      f20dtf = maxddc
086900030212     c                   eval      f20ore = 'N'
087000030212     c                   eval      f20com = 'N'
087100030212     c                   eval      f20det = 'S'
087200030212     c                   eval      f20rie = 'N'
087300030212     c                   eval      f20soc = xscsoc
087400030212     c                   eval      f20rsc = desric
087500030212     c                   eval      f20for = cdfs
087600030212     c                   eval      f20rgs = desemi
087700030212     c                   eval      f20nff = nrfat
087800030212     c                   move      dtfat         dataeur
087900030212     c                   move      dataeur       dataiso
088000030212     c                   move      dataiso       f20dft
088100030212     c                   movel(p)  ficn20ds      kpjbu
088200030212     c                   call      'FICN22R'
088300030212     c                   parm                    kpjba
088400030212     C                   ENDSR
088500030212     C*----------------------------------------------------*
088600030212     C* Reperimento dati società
088700030212     C*----------------------------------------------------*
088800030212     C     REPSOC        BEGSR
088900020419     C*
089000020419     C                   CALLB     'XSOC'
089100020419     C                   PARM                    TIPXSC            6
089200020419     C                   PARM                    SOCXSC            3
089300020419     C                   PARM                    CDSXSC            9 0
089400020419     C                   PARM                    MODXSC            3
089500020419     C                   PARM      *blanks       RTNXSC            1
089600020419     C                   PARM                    XSOCDS
089700020419     C                   PARM                    KPJBA
089800110502     C     RTNXSC        IFNE      '1'
089900110502     C                   MOVEL     XSOCDS        SOC001
090000110502     c* decodifico i dati della società
090100110502     c                   exsr      Decod_societa
090200110502     c                   if         PRMRPYIDMSG >= 0
090300110502     c                              and oIDSOCIETA <> *blank
090400110502     c*
090500110502     C                   MOVEL     oRAGSOCIALE   RSUT             20
090600110502     c                   movel     oRAGSOCIALE   desric
090700110502     c                   movel     oSEDLEGIND    indric
090800110502     c                   movel     oSEDLEGCAP    capric
090900110502     c                   movel     oPARTITAIVA   piric
091000110502     c                   movel     oCODFISCALE   cfric
091100110502     c     cfric         comp      ' '                                8989
091200110502     c                   if        oSEDLEGPRO = ' '
091300110502     C                   movel     oSEDLEGLOC    locric
091400110502     C                   ELSE
091500110502     C                   eval      locric = %trimr(osedlegloc)+
091600110502     C                             ' ('+osedlegpro+')'
091700110502     c                   end
091800110502     c                   end
091900110502     c                   end
092000020419     C*
092100020419     C                   ENDSR
092200110502     C**************************************************************************
092300110502     C* decodifica la società
092400110502     C************************************************************
092500110502     C     Decod_societa begSR
092600110502     ** Inizializzo il programma.
092700110502     C                   CALL      'TIBSSOCR'
092800110502     C                   PARM      'INIT'        prmRqsOpCode
092900110502     C                   PARM                    prmRpyOpCode
093000110502     C                   PARM                    prmRpyIdMsg
093100110502      *
093200110502     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
093300110502     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
093400110502     c* forzo società 201
093500110502     C                   EVAL      IidSocieta =f54soc
093600110502     C                   move      F54DTI        IDTVALIDITA
093700110502     C
093800110502     C                   CALL      'TIBSSOCR'
093900110502     C                   PARM      'GETANAGRAF'  prmRqsOpCode
094000110502     C                   PARM      *BLANK        prmRpyOpCode
094100110502     C                   PARM      *ZERO         prmRpyIdMsg
094200110502     C                   PARM      'TIBSSOCI0'   prmRqsFormato
094300110502     C                   PARM                    tibsSocI0
094400110502     C                   PARM                    prmRqsDataSize
094500110502     C                   PARM      'TIBSSOCO0'   prmRpyFormato
094600110502     C                   PARM                    tibsSocO0
094700110502     C                   PARM                    prmRpyDataSize
094800110502      *
094900110502     C                   ENDSR
095000161115      *----------------------------------------------------------------------
095100030207     Ofictt000  E            aggio
095200030207     o                       cttdcn
095300020502     OQSYSPRT   E            TESTE          2 02
095400020422     O                       RSUT                20
095500030616     O                                         + 25 'LISTA FATTURE EMESSE PER C'
095600030616     O                                           +0 'OOPERATIVE'
095700030616     O                                          113 'FICN65R'
095800020422     O                       UDATE              127 '  /  /  '
095900020422     O                       PAGE1         Z    132
096000020422     O          E            teste       1  2
096100020503     o                                         +  0 'Fornitore'
096200021010     o                                         + 46 'Fattura'
096300020503     o                                         +  1 'Data'
096400020503     o                                         +  8 'Firma'
096500021010     o                                         + 37 'Data consegna'
096600020503     O          E            DET         3
096700020503     O                       salfor            +  0
096800020422     O                       desemi            +  1
096900020422     O                       nrfat         4   +  1
097000020503     O                       dtfat             +  1 '  /  /    '
097100020503     O                                         +  2 '__________________________'
097200021010     O                                         +  0 '______________'
097300021010     O                                         +  2 '_______________'
097400020419**
097500030207SELECT ctttsr, sum(cttitc+cttitma+cttitmp), cttnra, cttcdf            1
097600030207from fictt06l WHERE cttsoc = '000' and cttcdf >='00000000' and        2
097700030207cttcdf <= '00000000' and cttfvl = 'C' and                             3
097800030207cttdft >= 00000000 and cttdft <= 00000000 and cttnff <> 0             4
097900030207GROUP BY cttcdf, cttnra, ctttsr                                       5
098000030207ORDER BY cttcdf, cttnra, ctttsr                                       6
098100030207and cttdcn = 0                                                        7
098200040217                                                                      8
098300030623** decm
098400030623gennaio
098500030623febbraio
098600030623marzo
098700030623aprile
098800030623maggio
098900030623giugno
099000030623luglio
099100030623agosto
099200030623settembre
099300030623ottobre
099400030623novembre
099500030623dicembre
