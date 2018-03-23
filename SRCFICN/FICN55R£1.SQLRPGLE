000100020422     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020422     H*PARMS ACTGRP(QILE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400900606?     *--------------------------------------------------------------*
000500020422      * ficn55R   stampa autofattura conteggi autotrasp.             *
000600900606?     *--------------------------------------------------------------*
000700020422     FanRCO98J  iF   E           K DISK
000800020419     Fanfrn01L  IF   E           K DISK
000900020502     FanCBA01L  IF   E           K DISK
001000020422     FndIVA00F  IF   E           K DISK
001100021203     Ffiapd01L  IF   E           K DISK
001200020528     Ftntlz01L  IF   E           K DISK
001201160121     Ftntbe01L  IF   E           K DISK
001300061121     ffiftt06L  uF   E           K DISK
001301160120     Ftnsre03L  uF   E           K DISK
001400061121     ftnanf01L  uF a E           K DISK
001500061121     Fficn55p   O    e             PRINTER OFLIND(*IN23)
001600020422     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
001700900515      *
001800020606     D dftt01        E DS
001900071120     D ficn99ds      E DS
002000020606     D KPJBA         E DS
002100020422?     *  Tabella Y4Z
002200020422     D angy4z        E DS                  extname(angy4zds)
002300020528     D dtlz01        e DS
002301160120     D dsrexaf       e DS
002302160121     D dsre          e DS
002400061121     D danf0         e DS
002500061121     D danf1         e DS
002600080919     D DanF2         e DS
002700020528     D xatbds        e DS
002800020422     D soc001        E DS                  EXTNAME(xsoc001ds)
002900020422     D xsocds          DS          1000
003000020502     D*ABI/CAB
003100020502     D X59ABIDS      E DS                  INZ
003200020502  "  D* DS per input condizione di pagamento
003300020502  "  DDVCDPDS        E DS                  INZ
003400020502  "  D* Ds per output
003500020502  "  DDVOCDPDS       E DS                  INZ
003600020422?     *  Tabella IVA
003700020422     DTABG48         E DS                  EXTNAME(ANGG48DS)
003800020422     D X06DS         E DS                  EXTNAME(X06G48DS)
003900020422     D  X06SOC       E                     EXTFLD(X06SOCIETA)
004000020422     D  X06CIV       E                     EXTFLD(X06CDIVA)
004100020422     D  X06DAT       E                     EXTFLD(X06DATA)
004200020422     D  X06LIN       E                     EXTFLD(X06LINGUA)
004300900516      *
004400020419     D Ficn54ds      E DS
004500020422     d dataiso         s               d
004600020422     d dataeur         s               d   datfmt(*eur)
004700020422     D conta           S              3  0 inz
004800020422     D comodo          S              9  2 inz
004900020422     D salnra          S                   like(nrass)
005000020527     D ctgans          S                   like(rcoctgan02)
005100020422     D salpdr          S              7  0 inz
005200020422     D salfor          S              8    inz
005300020614     D salfors         S                   like(salfor)
005400020502     D kcc             S                   inz like(rcokcc)
005500061123     D codfac          S                   like(RCOkscfact)
005600071221     D comdta          S                   like(ivadtdoc) inz(d'2007-12-31')
005601160120     D dtaiso          s               d   inz
005602160120     D dtaeur          s               d   datfmt(*eur) inz
005603160120     d  desdocds       ds
005604160120     d   cost1                        8    inz('Fattura ')
005605160120     d   nfat                         9
005606160120     d   cost2                        5    inz(' del ')
005607160120     d   dfat                        10
005700020419     c* SQL
005800020419     D cmd             s             70    ctdata dim(10)                       FIL. COMODO
005900020419     D WrkSqlCmd       S           1024
006000020419     D fiftt           DS
006100020419     d pdrs                           7  0
006200020419     d ddcs                           8  0
006300020419     d tots                          12  3
006400020422     d exts                          10  3
006500020422     d nrass                          9
006600020422     d cdfs                           8
006700110429     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
006800110429     D ESITO_OK...
006900110429     D                 C                   0
007000110429     ***************************************************************************
007100110429     **
007200110429     ** Dichiarazione prototipi procedure esterne.
007300110429     **
007400110429     ***************************************************************************
007500110429      /DEFINE DFTACTGRP_YES
007600110429     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
007700110429      /UNDEFINE DFTACTGRP_YES
007800110429     ***************************************************************************
007900110429     **
008000110429     ** Definizione strutture dati.
008100110429     **
008200110429      **************************************************************************
008300110429     D tibsSocI0...
008400110502     D               E DS                  prefix(i)
008500110429     D                                     INZ
008600110429     D tibsSocO0...
008700110502     D               E DS                  prefix(o)
008800110429     D                                     INZ
008900110429     ***************************************************************************
009000110429     **
009100110429     ** Definizione variabili modulo/programma.
009200110429     **
009300110429     ***************************************************************************
009400110429     D prmRqsOpCode...
009500110429     D                 S             10A
009600110429     D prmRpyOpCode...
009700110429     D                 S             10A
009800110429     D prmRpyIdMsg...
009900110429     D                 S             10I 0
010000110429     D prmRqsFormato...
010100110429     D                 S             10A
010200110429     D prmRqsDataSize...
010300110429     D                 S             10I 0
010400110429     D prmRpyFormato...
010500110429     D                 S             10A
010600110429     D prmRpyDataSize...
010700110429     D                 S             10I 0
010800110429
010900011031     ***********************************************************************
011000910521     C                   EXSR      DEFCAM
011100020419     c* Lettura fiftt06l ordinato per fornitore padroncino data conteg.
011200020419     c* i record scelti sono: FTTFVL='C', FTTTSR=' ', limiti scelti a
011300020419     c* video per fornitore e data protocollo, numero fattura <> 0
011400020419     c*
011500020419     C/EXEC SQL
011600020419     C+ PREPARE S1 FROM :WrkSqlCmd
011700020419     C/END-EXEC
011800020419
011900020419     C/EXEC SQL
012000020419     C+ DECLARE A1 CURSOR FOR S1
012100020419     C/END-EXEC
012200020419
012300020419     C/EXEC SQL
012400020419     C+ OPEN A1
012500020419     C/END-EXEC
012600020419
012700020419     C                   DOU       SqlCod <> 0
012800020419     C/EXEC SQL
012900020419     C+ FETCH NEXT FROM A1 INTO :fiftt
013000020419     C/END-EXEC
013100020419     c*
013200020419     C                   SELECT
013300020419     C                   WHEN      SqlCod = 0
013400020614     c* se nr.registrazione = * blanks aggiorno data stampa ed esco
013500020531     c                   if        nrass = *blanks
013600020614     c                   movel     salfor        salfors
013700020614     c                   movel     cdfs          salfor
013800020614     c                   exsr      sragg
013900020614     c                   movel     salfors       salfor
014000020531     c                   iter
014100020531     c                   end
014200020419     c* rottura fornitore
014300020422     c                   if        cdfs <> salfor or nrass <> salnra
014400020419     c                   exsr      srrotfor
014500020423     c* se la fattura è = 0 leggo un altro fornitore
014600020423     c                   if        impfat = 0
014700020423     c                   z-add     0             num               3 0
014800020423     c                   z-add     0             imppdr
014900020423     c                   iter
015000020423     c                   end
015100020419     c                   end
015200020422     c* rottura padroncino
015300020419     c                   if        pdrs <> salpdr
015400020419     c                   exsr      srrotpdr
015500020419     c                   end
015600020422     c* dettaglio campi
015700020422     C                   EXSR      srcampi
015800020701     c* stampo la riga solo se il totale è <> 0
015900020701     c                   if        imptg <> 0
016000020701     C                   ADD       1             NUM
016100081003     c*******            add       1             conta
016200020422     c                   seton                                        01
016300020422     c                   exsr      lista
016400020701     c                   end
016500020422     c*
016600020419     C                   WHEN      SqlCod = 100
016700020419     C                   WHEN      SqlCod <> 0
016800020419      * Forzo la stampa del JOBLOG.
016900020419     C                   CALL      'X66CHGJOB'
017000020419     C                   ENDSL
017100020419     C                   ENDDO
017200020419     C/EXEC SQL
017300020419     C+ CLOSE A1
017400020419     C/END-EXEC
017500020422     C* gestisco gli ultimi totali
017600020423     c                   if        impfat <> 0
017700081001     c                   move      'N'           stampa_Corr       1
017800020422     c                   exsr      srrotfor
017900020423     c                   end
018000110429     ** Chiudo il programma.
018100110429     C                   CALL      'TIBSSOCR'
018200110429     C                   PARM      'FINALIZE'    prmRqsOpCode
018300110429     C                   PARM                    prmRpyOpCode
018400110429     C                   PARM                    prmRpyIdMsg
018500020422     c*
018600910521     C                   SETON                                        LR
018700020419     C**************************************************************************
018800020419     C* Rottura fornitore
018900020419     C**************************************************************************
019000020419     C     Srrotfor      BEGSR                                                  *
019100081001      *
019200081001      * emissione dicitura : Corrispettivi......
019300081001     c******             if        stampa_Corr <> 'N'
019400081001     c******             setoff                                       23
019500081001     c******             end
019600020419     c* rotture precedenti
019700020419     c                   exsr      srrotpdr
019800020419     c* stampa la riga di totale
019900020419     c                   if        impfat <> 0
020000020509     c                   seton                                        05
020100020423     c                   else
020200020423     c                   z-add     0             conta
020300020419     c                   end
020400020419     c*
020500020701     c                   if        conta <> 60 and conta <> 0
020600020701     c* salto pagina per avere il totale fattura tutto insieme
020700081003     c*****              if        conta > 24
020800081003     c                   if        conta > 29
020900081001     c                   seton                                          24
021000081001     c******             seton                                        2324
021100020701     c                   end
021200020701     c                   exsr      lista
021300020502     c* aggiorno la data di stampa
021400020614     c                   exsr      sragg
021500071120     c                   end
021600020422     c*
021700020419     c                   z-add     60            conta
021800081001     c******             setoff                                       23
021900081001     c                   setoff                                       24
022000020422     c* azzera il conteggio per fornitore
022100020502     c                   z-add     0             page
022200020502     c                   z-add     0             impon
022300020422     c                   z-add     0             impiva
022400020422     c                   z-add     0             impfat
022500020419     c* decodifico fornitore
022600020419     c                   exsr      srfor
022700081001     c                   setoff                                       23
022800020423     c* aggancio registrazione solo se fornitore ok
022900020423     c                   if        not *in20
023000020422     c                   move      nrass         ivanrasreg
023100020422     c     kiva          chain     ndiva00f
023200020422     c                   if        %found(ndiva00f)
023300020422     c                   move      ivadtdoc      dataeur
023400020422     c                   move      dataeur       dtfat
023500020422     c                   move      ivanrdoc      nrfat
023600020422     c                   move      ivadtreg      dataeur
023700020422     c                   move      dataeur       dtprt
023800020422     c                   move      ivanrreg      nrprt
023900020422     c                   Z-ADD     ivaimporto    impiva
024000020422     c                   Z-ADD     ivaimponib    impon
024100020422     c     impon         add       impiva        impfat
024200020422     c                   end
024300020423     c                   end
024400020531     c                   if        impfat <> 0
024500020531     c                   move      cdfs          salfor
024600020531     c                   move      nrass         salnra
024700020531     c                   end
024800020419     c*
024900020419     c                   ENDSR
025000020419     C**************************************************************************
025100020614     C* aggiorna data di stampa in FIFTT00F
025200020419     C**************************************************************************
025300020614     C     sragg         BEGSR                                                  *
025400020614     c*
025500020614     c                   clear                   ftttsr
025600020614     c     kftt          setll     fiftt06l
025700020614     c                   do        *hival
025800020614     c     kftt          reade     fiftt06l
025900020614     c                   if        %eof(fiftt06l)
026000020614     c                   leave
026100020614     c                   end
026200020614     c* escludo quelli non fatturati oppure già stampati
026300020614     c                   if        fttnff = 0 or fttdcn <> 0
026400020614     c                   iter
026500020614     c                   end
026600020614      * controllo numero registrazione
026700020614     c                   if        %subst(fttflr:2:9) <> *blank
026800020614     c                   movel     fttflr        dftt01
026900020614     c                   move      §fttnrg       com09             9
027000020614     c                   if        com09 <> salnra
027100020614     c                   iter
027200020614     c                   end
027300020614     c                   end
027400020614      *
027500020614     c                   move      *date         dataiso
027600020614     c                   move      dataiso       fttdcn
027700020614     c                   except    aggio
027800020614     c                   enddo
027900071120     c* stampo la lettera x fusione solo se data fattura = 31/12/2007
028000071221     c                   if        ivadtdoc = comdta and
028100071221     c                             f54uni <> '999     '
028200071120     c                   eval      f99socn = f54soc
028300071120     c                   eval      f99kscn = salfor
028400071120     c                   eval      f99tip = 'A'
028500071120     c                   movel(p)  ficn99ds      kpjbu
028600071120     c                   call      'FICN99R'
028700071120     c                   parm                    kpjba
028800071221     c                   end
028900020614     c*
029000020614     c                   ENDSR
029100020614     C**************************************************************************
029200020614     C* Rottura padroncino
029300020614     C**************************************************************************
029400020614     C     Srrotpdr      BEGSR                                                  *
029500020419     c                   seton                                        07
029600020419     c* decodifica padroncino
029700021203     C     kapd          CHAIN     fiapd01L
029800021203     c                   if        %found(fiapd01l)
029900070822     c                   movel(p)  apdpdr        despdr
030000020419     c                   else
030100020419     c                   movel     *blanks       despdr
030200020419    3C                   ENDIF
030300020422     c* stampa la riga di totale padroncino solo se ha più righe
030400020422     c                   eval(h)   comodo = imppdr
030500080926     c******  Prima saltava il totale Autotrasp. se era 1 solo
030600080926     c******   adesso con la stampa dell'importo carburante legge 133/08
030700080926     c******   deve sempre stampare il totale Autotrasp.
030800081006     c******             if        num > 1  and comodo <> impon
030900081006     c                   if        num>=1 and comodo <> 0
031000020419     c                   seton                                        03
031100081002     c********           add       1             conta
031200020419     c                   exsr      lista
031300080926     c                   end
031400020419     c* azzera il conteggio per padroncino
031500020419     c                   z-add     0             num               3 0
031600020419     c                   z-add     0             imppdr
031700020419     c                   move      pdrs          salpdr
031800020419     c*
031900080926     c*
032000020419     c                   endsr
032100020419?     *--------------------------------------------------------------*
032200020419?     *  dati in testata                                             *
032300020419?     *--------------------------------------------------------------*
032400020419     C     srfor         BEGSR
032500020419      *
032600020502     c                   eval      rcoksc = cdfs
032700020422     C     Kfrn          CHAIN     anfrn01l                           20
032800020422     C  N20Krco          CHAIN     anrco98j                           20
032900020423     c* controllo se il fornitore appartiene all'unità di reg. scelta
033000020423     c* se 999 tutte le unità
033100020423     c                   if        not *in20 and f54uni <> '999     '
033200020423     c                             and rcounita <> f54uni
033300020423     c                   seton                                        20
033400020423     c                   end
033500020528     c* se 999 tutte le unità (vuol dire che sono in sede) controllo
033600020528     c* che il fornitore sia in autofatturazione
033700090707     c                   clear                   dtlz01
033800020531     c                   if        not *in20
033900020528     c                   move      cdfs          tlzpdr
034000020528     c     ktlz          chain     tntlz01l
034100020528     c                   if        %found(tntlz01l)
034200020528     c                   movel     tlzflo        dtlz01
034300020531     c                   if        §tlzfl1 = 'S'
034400020531     c                             and f54uni = '999     '
034500020528     c                   seton                                        20
034600020528     c                   end
034700020528     c                   end
034800020528     c                   end
034900020423     c*
035000020423     c                   if        not *in20
035100091109     C                   if        %subst(indtelex: 1: 8) = 'CAP.SOC.'
035200091109     c                   eval      csemi = indtelex
035300091109     c                   else
035400091201     c* per l'ottico non può essere blanks
035500091201     c                   eval      csemi ='.'
035600091109     c                   end
035700020419     C                   MOVEL     sogdes        desemi
035800020422     C                   eval      indemi = indindriz
035900020422     c                   if        indprov <> ' '
036000020422     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
036100020422     c                   else
036200020422     C                   eval      locemi = indlocalit
036300020422     c                   end
036400020422     C                   eval      capemi = indcap
036500091112     c*
036600091112     C                   movel     sogpartiva    piemi            20
036700091112     C                   MOVEL     indprov       premi             2
036800091112     C                   movel     sogcdfisc     cfemi            20
036900091112     c*
037000091112     C                   if        cfemi <> ' '
037100091112     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
037200091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
037300091119     C                             ': ' + %trimr(cfemi)
037400091112     c                   else
037500091112     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
037600091112     c                   end
037700020419     c* se il fornitore ha un codice IVA
037800020419     C                   CLEAR                   tabg48
037900020419     c                   if        frncdiva <> *blanks
038000020419     C                   CLEAR                   X06DS
038100020419     C                   MOVE      xscsoc        X06SOC
038200020419     C                   MOVE      XSCLIN        X06LIN
038300020419     C                   MOVE      frncdiva      X06CIV
038400020422     C                   MOVE      f54dti        X06DAT
038500020419     C                   CALL      'X06G48'
038600020419     C                   PARM                    X06DS
038700020419     C     X06ERR        IFEQ      '0'
038800020419     C                   MOVEL     X06UNI        TABG48
038900020419     C                   end
039000020419     C                   end
039100020527     c* aggancio la tabella Y4Z per tipologia fornitore
039200020527     c                   if        rcoctgan02 <> ctgans
039300020527     c                   exsr      sry4z
039400020527     c                   movel     rcoctgan02    ctgans
039500020527     c                   end
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
041800061123  "  C                   eval      DCPLIN=xsclin
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
043600061123     C                   MOVEL     rcoksc        codfac
043700020701     C*                  eval      indfac = indindriz
043800020701     c*                  if        indprov <> ' '
043900020701     C*                  eval      locfac = %trimr(indlocalit)+' ('+indprov+')'
044000020701     c*                  else
044100020701     C*                  eval      locfac = indlocalit
044200020701     c*                  end
044300020701     C*                  eval      capfac = indcap
044400020701     C*                  eval      telfac = indtelefon
044500020502     c                   end
044600020502     c                   else
044700020502     c                   setoff                                       11
044800020502     c*banca d'appoggio in alternativa al factor
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
046000090925    >C                   CLEAR                   iban
046100090925    >C                   CLEAR                   ABI
046200020502    >C                   CLEAR                   CAB
046300020503    >C                   CLEAR                   nrcc
046400020502    >C                   CLEAR                   DESIST
046500020502    >C                   CLEAR                   DESAGE
046600020502    >C                   Else
046700020502     C                   Movel     X59DesIst     DESIST
046800020502     C                   Movel     X59DesAge     DESAGE
046900090925     C                   Movel     CBAiban       iban
047000090925     C                   Movel     CBAABI        ABI               5
047100090925     C                   Movel     CBACAB        CAB               5
047200090925     C                   Movel     CBACcB        nrcc             18
047300020502    >C                   End
047400020502     c                   end
047500020502     c                   end
047600020502     C*
047700020423     C                   end
047800020419      *
047900020419     C                   ENDSR
048000020419      *-----------------------------------------------------***********
048100061121      * memorizzo storico testata anagrafico fornitore solo se no copia iva
048200020419      *-----------------------------------------------------***********
048300061121     C     SRanf0        BEGSR
048400061121     c                   eval      ANFTIP = 'A'
048500070607     c                   eval      ANFTSR = 'A'
048600061121     c                   eval      ANFTrc = '0'
048700061121     c                   eval      ANFSOC = f54soc
048800070905     c                   eval      ANFCDF = salfor
048900061121     c                   eval      ANFNFF = ivanrdoc
049000061121     c                   move      ivadtdoc      ANFDFT
049100061121     c                   clear                   anfpdr
049200061121     c                   clear                   danf0
049300061121     c     kanf0         chain     tnanf01l
049400061121     c                   if        f54uni <>'999     '
049500091111     c                   eval      §ANF0telex = csemi
049600091111     c                   eval      §ANF0prov  = premi
049700091111     c                   eval      §ANF0DES = desemi
049800061121     c                   eval      §ANF0IND = indemi
049900061121     c                   eval      §ANF0CAP = capemi
050000061121     c                   eval      §ANF0LOC  = locemi
050100061121     c                   eval      §ANF0PI   = piemi
050200061121     c                   eval      §ANF0CF   = cfemi
050300061121     c                   eval      §ANF0CDP  = frncondpag
050400061121     c                   eval      §ANF0DCP  = descon
050500061121     c                   if        *in11
050600061123     c                   eval      §ANF0FAC  = codfac
050700061121     c                   eval      §ANF0DFAC = desfac
050800061121     c                   else
050900090925     c                   eval      §ANF0iban = iban
051000090925     c                   eval      §ANF0ABI  = abi
051100061121     c                   eval      §ANF0CAB  = cab
051200061121     c                   eval      §ANF0NRC  = nrcc
051300061121     c                   eval      §ANF0IST  = desist
051400061121     c                   eval      §ANF0AGE  = desage
051500061121     c                   end
051600061121     c                   eval      §ANF0NRP  = nrprt
051700061121     c                   move      ivadtreg      §ANF0DRP
051800061121     c                   eval      anfuni = danf0
051900061121     c                   if        %found(tnanf01l)
052000061121     c                   update    tnanf000
052100061121     c                   else
052200061121     c                   write     tnanf000
052300061121     c                   end
052400061121     c                   else
052500061121     c* imposto i dati di stampa come memorizzati
052600061121     c                   if        %found(tnanf01l)
052700061121     c                   eval      danf0 = anfuni
052800091111     c                   eval      csemi = §ANF0telex
052900061121     c                   eval      desemi      = §ANF0DES
053000061121     c                   eval      indemi      = §ANF0IND
053100061121     c                   eval      capemi      = §ANF0CAP
053200061121     c                   eval      locemi     = §ANF0LOC
053300091112     c                   eval      premi = §ANF0prov
053400061121     c                   eval      piemi      = §ANF0PI
053500061121     c                   eval      cfemi      = §ANF0CF
053600091112     C                   if        cfemi <> ' '
053700091112     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
053800091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
053900091119     C                             ': ' + %trimr(cfemi)
054000091112     c                   else
054100091112     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
054200091112     c                   end
054300061121     c                   eval      descon     = §ANF0DCP
054400061121     c                   eval      abi        = §ANF0ABI
054500090925     c                   eval      iban       = §ANF0iban
054600061121     c                   eval      cab        = §ANF0CAB
054700061121     c                   eval      nrcc       = §ANF0NRC
054800061121     c                   eval      desist     = §ANF0IST
054900061121     c                   eval      desage     = §ANF0AGE
055000061121     c                   eval      desfac    = §ANF0DFAC
055100061121     c                   end
055200061121     c                   end
055300061121     C                   ENDSR
055400061121      *-----------------------------------------------------***********
055500061121      * memorizzo storico righe anagrafico fornitore solo se no copia iva
055600061121      *-----------------------------------------------------***********
055700061121     C     SRanf1        BEGSR
055800061121     c                   eval      ANFTIP = 'A'
055900070607     c                   eval      ANFTSR = 'A'
056000061121     c                   eval      ANFTrc = '1'
056100061121     c                   eval      ANFSOC = f54soc
056200061121     c                   eval      ANFCDF = cdfs
056300061121     c                   eval      ANFNFF = ivanrdoc
056400061121     c                   move      ivadtdoc      ANFDFT
056500061121     c                   eval      ANFpdr = pdrs
056600061121     c                   clear                   danf1
056700061121     c     kanf0         chain     tnanf01l
056800061121     c                   if        f54uni <>'999     '
056900061121     c                   eval      §ANF1DES = despdr
057000061121     c                   eval      anfuni = danf1
057100061121     c                   if        %found(tnanf01l)
057200061121     c                   update    tnanf000
057300061121     c                   else
057400061121     c                   write     tnanf000
057500061121     c                   end
057600061121     c                   else
057700061121     c* imposto i dati di stampa come memorizzati
057800061121     c                   if        %found(tnanf01l)
057900061121     c                   eval      danf1 = anfuni
058000061121     c                   eval      despdr      = §ANF1DES
058100061121     c                   end
058200061121     c                   end
058300061121     C                   ENDSR
058400080919      *-----------------------------------------------------***********
058500080919      * Decodifico dati x Carburante legge 133/2008
058600080919      *-----------------------------------------------------***********
058700080919     C     SRanf2        BEGSR
058800080919     c                   eval      ANFTIP = 'A'
058900080919     c                   eval      ANFTSR = 'A'
059000080919     c                   eval      ANFTrc = '2'
059100080919     c                   eval      ANFSOC = f54soc
059200080925     c                   eval      ANFCDF = salFOR
059300080919     c                   eval      ANFNFF = ivanrdoc
059400080919     c                   move      ivadtdoc      ANFDFT
059500080925     c                   eval      ANFpdr = salPDR
059600080919     c                   clear                   danf2
059700111128     c*                  clear                   impPDR
059800081003     c                   clear                   impTGC
059900081003     c                   clear                   impGPI
060000081003     c                   clear                   impVAR
060100081003     c                   clear                   impADGc
060200080919     c     kanf0         chain     tnanf01l
060300080919     c* imposto i dati di stampa come memorizzati
060400080919     c                   if        %found(tnanf01l)
060500080919     c                   eval      danf2 = anfuni
060600080919     c                   eval      impPDR  = §ANF2COR
060700080919     c                   eval      impTGC  = §ANF2TGC
060800080919     c                   eval      impGPI  = §ANF2GPI
060900080919     c                   eval      impVAR  = §ANF2VAR
061000080919     c                   eval      impADGc = §ANF2ADG
061100080919     c                   end
061200080919     C                   ENDSR
061300061121      *-----------------------------------------------------***********
061400061121      * Campi per stampa
061500061121      *-----------------------------------------------------***********
061600061121     C     SRCAMPI       BEGSR
061700020419     C                   Z-ADD     Tots          impcom
061800020419     C                   Z-ADD     exts          impext
061900020422     C     impcom        add       impext        imptg
062000020422     C                   add       imptg         imppdr
062100020419     c                   move      ddcs          dataiso
062200020419     c                   move      dataiso       dataeur
062300020422     c                   move      dataeur       DTDDC
062400020419     C                   ENDSR
062500020422     C**************************************************************************
062600020422     C* Gestione stampa
062700020422     C**************************************************************************
062800020422     C     LISTA         BEGSR                                                  *
062900081002      *
063000081001     c******             if        conta>60 or *in23  or
063100081001     c                   if        conta>60 or *IN24  OR
063200081003     c******                       (conta>34 and *in03) or
063300081003     c                             conta > 35
063400020422     c                   seton                                        3007
063500020422     c                   z-add     1             conta
063600081001      * se stampata anche dicitura : Corrispettivi......
063700081001     c                   if        *in23=*off
063800081001     c                   add       5             conta
063900081001     c                   end
064000020422     c                   end
064100081002      *
064200061121     c* memorizzo nello storico anagrafiche
064300061121     c   30              exsr      sranf0
064400061121     c* testata fattura
064500020422     c   30              write     testa
064600081001     c                   setoff                                       30  24
064700081001     c                   seton                                          23
064800081002      *
064900020508     c* testata lista fatture emesse
065000020508     c                   if        f54lis = 'S' and *inof
065100020508     c                   except    teste
065200020508     c                   setoff                                       of
065300020508     c                   end
065400081002      *
065500081002      * --------------------------------------
065600081002      *  Stampa il totale Autotrasportatore:  --> 03 ON
065700081002      * --------------------------------------
065800081002     c                   if        *in03
065900081002      *
066000080919     c* decodifica dati Carburante Legge 133/08
066100081002     c                   exsr      sranf2
066200020422     c* totale padroncino
066300081002     c                   write     totpdr
066400081002     c                   add       1             conta
066500111125      * stampo solo se ho trovato il record 2 per adeguamento carburante
066600111125     c                   if        %found(tnanf01l) and
066700111125     c                             impGPI <> 0
066800081002      * Controllo se OVRFLOW
066900081003     c                   z-add     34            maxrighe          3 0
067000081002     c                   exsr      chk_OvrFLW
067100081002     c* 1°riga riepilogo carburante
067200081002     c                   write     carbu1
067300081002     c                   add       1             conta
067400081002      *
067500081002      * Controllo se OVRFLOW
067600081002     c                   exsr      chk_OvrFLW
067700081002     c* 2°riga riepilogo carburante
067800081002     c                   write     carbu2
067900081002     c                   add       1             conta
068000081003      *
068100081002      * Controllo se OVRFLOW
068200081002     c                   exsr      chk_OvrFLW
068300081002     c* 3°riga riepilogo carburante
068400081002     c                   write     carbu3
068500081002     c                   add       1             conta
068600081003      *
068700081002      * Controllo se OVRFLOW
068800081002     c                   exsr      chk_OvrFLW
068900081002     c* 4°riga riepilogo carburante
069000081002     c                   write     carbu4
069100081002     c                   add       1             conta
069200081002      *
069300081002      * Controllo se OVRFLOW
069400081003     c                   z-add     31            maxrighe          3 0
069500081002     c                   exsr      chk_OvrFLW
069600081002     c* ultime righe riepilogo carburante
069700081002     c                   write     carbu5
069800081002     c                   add       4             conta
069900081002      *
070000081002     c                   end
070100111125     c                   end
070200081002      *---------------------------------------
070300160120      * rettifiche di sede
070301160120     c   05              exsr      rettifiche
070400020509     c* totale FATTURA FORNITORE
070500020422     c   05              write     totGEN
070600020509     c* dati finali
070700020701     c*  06              write     totGE1
070800020531     c* riga dettaglio fatture emesse solo per fornitori in autofatturazione
070900020531     c                   if        f54lis = 'S' and *in05 and
071000020531     c                             §tlzfl1 = ' '
071100020422     c                   except    det
071200020422     c                   end
071300061121     c* memorizzo storico righe anagrafico x autofat.
071400061121     c                   if        *in01 and despdr <> ' '
071500061121     c                   exsr      sranf1
071600061121     c                   end
071700061121     c* dettaglio
071800020422     c   01              write     riga
071900081003     c   01              add       1             conta
072000020422     C   01              MOVEL     *BLANKS       DESPDR
072100020422     C   01              setoff                                       07
072200020422     c                   movea     '000000'      *in(01)
072300020422     c                   ENDSR
072301160120      *-----------------------------------------------------***********
072302160120      * rettifiche fatture fornitori
072303160120      *-----------------------------------------------------***********
072304160120     C     rettifiche    BEGSR
072305160120     c                   move      dtfat         dtaeur
072306160120     c                   move      dtaeur        dtaiso
072307160120     c                   move      dtaiso        dtfatymd          8 0
072308160120     c                   z-add     nrfat         nrfat9            9 0
072309160120     C/EXEC SQL
072310160120     C+ DECLARE R1 CURSOR FOR SELECT SRESRE, SREUNI, sum( SRETIM )
072311160120     C+    FROM tnsre00f WHERE
072312160120     C+    sRESOC = :f54soc and sretip = 'A'
072313160120     C+    and SRECDF = :salfor and SREDFT = :dtfatymd and SRENFF = :nrfat
072314160120     C+    GROUP BY SRESRE, SREUNI
072315160120     C/END-EXEC
072316160120      *
072317160120     C/EXEC SQL
072318160120     C+ OPEN R1
072319160120     C/END-EXEC
072320160120      *
072321160120     c                   do        *hival
072322160120     C/EXEC SQL
072323160120     C+ FETCH  next FROM R1 INTO :sresre, :sreuni, :sretim
072324160120     C/END-EXEC
072325160120     C                   if        SqlCod = 100
072326160120     c                   Leave
072327160120      *  x errori
072328160120     C                   elseif    SqlCod < 0
072329160120     c                   dump(a)
072330160120     c                   leave
072331160120     c                   endif
072332160120     c                   if        SQLCOD = 0
072333160120     c                   movel     sreuni        dsrexaf
072334160120     c                   eval      nfat = %editc(§srexafnf:'Z')
072335160120     c                   if        §srexafdf > 0
072336160120     c                   move      §SREXAFdf     dtaiso
072337160120     c                   move      dtaiso        dtaeur
072338160120     c                   move      dtaeur        dfat
072339160120     c                   endif
072340160121     c                   movel     sresre        tbeke1
072341160121     c                   movel     'SRE'         tbecod
072342160121     c     ktbe          chain     tntbe01l
072343160121     c                   if        %found(tntbe01l)
072344160121     c                   movel     tbeuni        dsre
072345160121     c                   eval      desret = §sredes
072346160121     c                   endif
072347160120     c                   movel     desdocds      desdoc
072348160120     c                   z-add     sretim        impret
072349160120     c                   write     rigaret
072350160121     c                   add       2             conta
072351160120     c                   endif
072352160120     c                   enddo
072353160120     C/EXEC SQL
072354160120     C+ CLOSE R1
072355160120     C/END-EXEC
072356160120     c* decodifico il tipo servizio
072357160120     c     ksre          setll     tnsre03l
072358160120     c                   do        *hival
072359160120     c     ksre          reade     tnsre03l
072360160120     c                   if        %eof(tnsre03l)
072361160120     c                   leave
072362160120     c                   endif
072363160120     c                   move      *date         dataiso
072364160120     c                   move      dataiso       sredcn
072365160120     c                   update    tnsre000
072366160120     c                   enddo
072367160120     C                   ENDSR
072400020527?     *--------------------------------------------------------------*
072500020527?     *  lettura tabella Y4Z                                         *
072600020527?     *--------------------------------------------------------------*
072700020527     C     sry4z         BEGSR
072800020527     c*  lettura tabella Y4Z
072900020527     c                   clear                   xatbds
073000020527     c                   move      '1'           xtbric
073100020527     c                   move      *blank        xtbkey
073200020527     c                   move      'Y4Z'         xtbcod
073300020527     c                   movel     rcoctgan02    xtbkey
073400020527     c                   call      'XATB'
073500020527     c                   parm                    xatbds
073600020527     c                   if        xtberr = '0'
073700020527     c                   movel     xtbuni        angy4z
073800020527     c                   else
073900020527     c                   clear                   angy4z
074000020527     c                   end
074100020527     C*
074200020527     C                   ENDSR
074300011031     C**********************************************************************
074400910521     C     DEFCAM        BEGSR
074500011031     C**********************************************************************
074600000000     C     *ENTRY        PLIST
074700000000     C                   PARM                    KPJBA
074800020419     C                   MOVEL     KPJBU         ficn54ds
074900020419     C*---------- RICERCA DITTA :
075000020419     C                   MOVEL     'SOC001'      TIPXSC
075100020422     C                   MOVEL     f54soc        SOCXSC
075200020422     c     f54lis        comp      'S'                                    of
075300020419     C                   EXSR      REPSOC
075400020419      *  Definizione chiave di accesso
075500061121     c     kanf0         klist
075600061121     c                   KFLD                    ANFTIP
075700070607     c                   KFLD                    ANFTSR
075800061121     c                   KFLD                    ANFTrc
075900061121     c                   KFLD                    ANFSOC
076000061121     c                   KFLD                    ANFCDF
076100061121     c                   KFLD                    anfdft
076200061121     c                   KFLD                    ANFNFF
076300061121     c                   KFLD                    ANFpdr
076400021203     C     Kapd          KLIST
076500021203     C                   KFLD                    apdtip
076600021203     C                   KFLD                    pdrs
076700021203     c                   movel     'A'           apdtip
076701160121      *
076702160121     c     ktbe          klist
076703160121     c                   kfld                    tbecod
076704160121     c                   kfld                    tbeke1
076800021203     C     Ktlz          KLIST
076900021203     C                   KFLD                    tlztip
077000021203     C                   KFLD                    tlzpdr
077100021203     C                   KFLD                    f54soc
077200021203     c                   movel     'P'           tlztip
077201160120     C     Ksre          KLIST
077202160120     C                   KFLD                    f54soc
077203160120     C                   KFLD                    salfor
077204160120     C                   KFLD                    dtfatymd
077205160120     c                   kfld                    nrfat9
077300020502     C     Kftt          KLIST
077400020502     C                   KFLD                    f54soc
077500020502     C                   KFLD                    salfor
077600020502     C                   KFLD                    ftttsr
077700020502     C     Kfrn          KLIST
077800020502     C                   KFLD                    f54soc
077900020502     C                   KFLD                    cdfs
078000020419     C     Krco          KLIST
078100020422     C                   KFLD                    f54soc
078200020422     C                   KFLD                    rcosnatura
078300020502     C                   KFLD                    rcoksc
078400020422     c                   eval      rcosnatura = 'F'
078500020422     C     Kiva          KLIST
078600020422     C                   KFLD                    ivasys
078700020422     C                   KFLD                    ivanrasreg
078800020502     c                   eval      ivasys = 0
078900020502    >C     KCBA          KLIST
079000020502    >C                   KFLD                    XSCSOC
079100020502    >C                   KFLD                    CBACLIFOR
079200020502    >C                   KFLD                    KCC
079300020502    >C                   KFLD                    rcoKSC
079400020502    >C                   MOVEL     'F'           CBACLIFOR
079500020502     c* società
079600020422     c                   eval      %subst(cmd(3): 31: 3) = F54SOC
079700020502     c* autotrasportatore inizio
079800020422     c                   eval      %subst(cmd(3): 50: 8) = F54pd1
079900020502     c* autotrasportatore fine
080000020422     c                   eval      %subst(cmd(4): 12: 8) = F54pd2
080100020502     c* data inizio
080200020422     c                   move      f54dti        com8              8
080300020419     c                   eval      %subst(cmd(5): 11: 8) = com8
080400020502     c* data fine
080500020422     c                   move      f54dtf        com8
080600020419     c                   eval      %subst(cmd(5): 34: 8) = com8
080700020419     c* MI COMPONGO L'ISTRUZIONE SQL
080800020604     c* l'indicatore 80 mi serve per evitare la doppia ribattitura
080900020604     c* nella stampa della copia IVA
081000020503     c*
081100040217     c* se stampa per libro IVA non controllo data di stampa fattura
081200040217     c* altrimenti deve essere a 0 (CMD,8)
081300020503     c                   if        f54uni = '999     '
081400020604     c                   seton                                        80
081500020419     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
081600020503     C                             +CMD(4)+CMD(5)+CMD(9)+CMD(6)+CMD(7)
081700020503     c                   else
081800020604     c                   setoff                                       80
081900020503     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
082000020503     C                             +CMD(4)+CMD(5)+CMD(8)+CMD(6)+CMD(7)
082100020503     c                   end
082200020419     C                   ENDSR
082300020419     C*----------------------------------------------------*
082400020419     C* Reperimento dati società
082500020419     C*----------------------------------------------------*
082600020419     C     REPSOC        BEGSR
082700020419     C*
082800020419     C                   CALLB     'XSOC'
082900020419     C                   PARM                    TIPXSC            6
083000020419     C                   PARM                    SOCXSC            3
083100020419     C                   PARM                    CDSXSC            9 0
083200020419     C                   PARM                    MODXSC            3
083300020419     C                   PARM      *blanks       RTNXSC            1
083400020419     C                   PARM                    XSOCDS
083500020419     C                   PARM                    KPJBA
083600110429     C     RTNXSC        IFNE      '1'
083700110429     C                   MOVEL     XSOCDS        SOC001
083800110429     c* decodifico i dati della società
083900110429     c                   exsr      Decod_societa
084000110502     c                   if         PRMRPYIDMSG >= 0
084100110502     c                              and oIDSOCIETA <> *blank
084200110429     c*
084300110502     C                   MOVEL     oRAGSOCIALE   RSUT             20
084400110502     c                   movel     oRAGSOCIALE   desric
084500110502     c                   movel     oSEDLEGIND    indric
084600110502     c                   movel     oSEDLEGCAP    capric
084700110502     c                   movel     oPARTITAIVA   piric
084800110502     c                   movel     oCODFISCALE   cfric
084900110429     c     cfric         comp      ' '                                8989
085000110502     c                   if        oSEDLEGPRO = ' '
085100110502     C                   movel     oSEDLEGLOC    locric
085200110429     C                   ELSE
085300110502     C                   eval      locric = %trimr(osedlegloc)+
085400110502     C                             ' ('+osedlegpro+')'
085500110502     c                   end
085600110429     c                   end
085700110429     c                   end
085800020419     C*
085900020419     C                   ENDSR
086000081002     C*----------------------------------------------------*
086100081002     C*   Controlla se deve andare alla pagina seguente
086200081002     C*----------------------------------------------------*
086300081002     C     chk_OvrFLW    BegSR
086400081002     C*
086500081002     C* se superato il limite si emette la testata
086600081002     c                   if        conta > maxrighe
086700081002     c                   seton                                        3007
086800081002     c                   z-add     1             conta
086900081002     c                   end
087000081002      *
087100081002     c* memorizzo nello storico anagrafiche
087200081002     c   30              exsr      sranf0
087300081002     c* testata fattura
087400081002     c   30              write     testa
087500081002     c                   setoff                                       30  24
087600081002     c                   seton                                          23
087700081002     C*
087800081002     C                   ENDSR
087900110429     C**************************************************************************
088000110429     C* decodifica la società
088100110429     C************************************************************
088200110429     C     Decod_societa begSR
088300110429     ** Inizializzo il programma.
088400110429     C                   CALL      'TIBSSOCR'
088500110429     C                   PARM      'INIT'        prmRqsOpCode
088600110429     C                   PARM                    prmRpyOpCode
088700110429     C                   PARM                    prmRpyIdMsg
088800110429      *
088900110429     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
089000110429     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
089100110429     c* forzo società 201
089200110502     C                   EVAL      IidSocieta =f54soc
089300110502     C                   move      F54DTI        IDTVALIDITA
089400110429     C
089500110429     C                   CALL      'TIBSSOCR'
089600110429     C                   PARM      'GETANAGRAF'  prmRqsOpCode
089700110429     C                   PARM      *BLANK        prmRpyOpCode
089800110429     C                   PARM      *ZERO         prmRpyIdMsg
089900110429     C                   PARM      'TIBSSOCI0'   prmRqsFormato
090000110429     C                   PARM                    tibsSocI0
090100110429     C                   PARM                    prmRqsDataSize
090200110429     C                   PARM      'TIBSSOCO0'   prmRpyFormato
090300110429     C                   PARM                    tibsSocO0
090400110429     C                   PARM                    prmRpyDataSize
090500110429      *
090600110429     C                   ENDSR
090700081002     C*----------------------------------------------------*
090800081002      *
090900020502     Ofiftt000  E            aggio
091000020502     o                       fttdcn
091100020502     OQSYSPRT   E            TESTE          2 02
091200020422     O                       RSUT                20
091300030616     O                                         + 25 'LISTA FATTURE EMESSE X AUT'
091400030616     O                                         +  0 'OTRASPORTATORI'
091500030616     O                                          113 'FICN55R'
091600020422     O                       UDATE              127 '  /  /  '
091700020422     O                       PAGE1         Z    132
091800020422     O          E            teste       1  2
091900020503     o                                         +  0 'Fornitore'
092000021010     o                                         + 46 'Fattura'
092100020503     o                                         +  1 'Data'
092200020503     o                                         +  8 'Firma'
092300021010     o                                         + 37 'Data consegna'
092400020503     O          E            DET         3
092500020503     O                       salfor            +  0
092600020422     O                       desemi            +  1
092700020422     O                       nrfat         4   +  1
092800020503     O                       dtfat             +  1 '  /  /    '
092900020503     O                                         +  2 '__________________________'
093000021010     O                                         +  0 '______________'
093100021010     O                                         +  2 '_______________'
093200020419**
093300020419SELECT fttpdr, fttddc, sum(fttitt+fttita+ftttpe+ftttbn+fttmnt),       1
093400020422sum(ftttim), substr(fttflr, 2, 9) as nrass, fttcdf                    2
093500020419from fiftt06l WHERE fttsoc = '000' and fttcdf >='00000000' and        3
093600020503fttcdf <= '00000000' and ftttsr = ' ' and fttfvl = 'C' and            4
093700020419fttdft >= 00000000 and fttdft <= 00000000 and fttnff <> 0             5
093800020528GROUP BY fttcdf, substr(fttflr, 2, 9), fttpdr, fttddc                 6
093900020528ORDER BY fttcdf, nrass, fttpdr, fttddc                                7
094000020503and fttdcn = 0                                                        8
094100040217                                                                      9
