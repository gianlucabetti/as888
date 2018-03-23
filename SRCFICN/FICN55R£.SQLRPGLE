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
001300160121     Ftntbe01L  IF   E           K DISK
001400061121     ffiftt06L  uF   E           K DISK
001500160120     Ftnsre03L  uF   E           K DISK
001600061121     ftnanf01L  uF a E           K DISK
001700061121     Fficn55p   O    e             PRINTER OFLIND(*IN23)
001800020422     Fqsysprt   O    f  132        PRINTER OFLIND(*INof)
001900900515      *
002000020606     D dftt01        E DS
002100071120     D ficn99ds      E DS
002200020606     D KPJBA         E DS
002300020422?     *  Tabella Y4Z
002400020422     D angy4z        E DS                  extname(angy4zds)
002500020528     D dtlz01        e DS
002600160120     D dsrexaf       e DS
002700160121     D dsre          e DS
002800061121     D danf0         e DS
002900061121     D danf1         e DS
003000080919     D DanF2         e DS
003100020528     D xatbds        e DS
003200020422     D soc001        E DS                  EXTNAME(xsoc001ds)
003300020422     D xsocds          DS          1000
003400020502     D*ABI/CAB
003500020502     D X59ABIDS      E DS                  INZ
003600020502  "  D* DS per input condizione di pagamento
003700020502  "  DDVCDPDS        E DS                  INZ
003800020502  "  D* Ds per output
003900020502  "  DDVOCDPDS       E DS                  INZ
004000020422?     *  Tabella IVA
004100020422     DTABG48         E DS                  EXTNAME(ANGG48DS)
004200020422     D X06DS         E DS                  EXTNAME(X06G48DS)
004300020422     D  X06SOC       E                     EXTFLD(X06SOCIETA)
004400020422     D  X06CIV       E                     EXTFLD(X06CDIVA)
004500020422     D  X06DAT       E                     EXTFLD(X06DATA)
004600020422     D  X06LIN       E                     EXTFLD(X06LINGUA)
004700900516      *
004800020419     D Ficn54ds      E DS
004900020422     d dataiso         s               d
005000020422     d dataeur         s               d   datfmt(*eur)
005100020422     D conta           S              3  0 inz
005200020422     D comodo          S              9  2 inz
005300020422     D salnra          S                   like(nrass)
005400020527     D ctgans          S                   like(rcoctgan02)
005500020422     D salpdr          S              7  0 inz
005600020422     D salfor          S              8    inz
005700020614     D salfors         S                   like(salfor)
005800020502     D kcc             S                   inz like(rcokcc)
005900061123     D codfac          S                   like(RCOkscfact)
006000071221     D comdta          S                   like(ivadtdoc) inz(d'2007-12-31')
006100160120     D dtaiso          s               d   inz
006200160120     D dtaeur          s               d   datfmt(*eur) inz
006300160120     d  desdocds       ds
006400160120     d   cost1                        8    inz('Fattura ')
006500160120     d   nfat                         9
006600160120     d   cost2                        5    inz(' del ')
006700160120     d   dfat                        10
006800020419     c* SQL
006900020419     D cmd             s             70    ctdata dim(10)                       FIL. COMODO
007000020419     D WrkSqlCmd       S           1024
007100020419     D fiftt           DS
007200020419     d pdrs                           7  0
007300020419     d ddcs                           8  0
007400020419     d tots                          12  3
007500020422     d exts                          10  3
007600020422     d nrass                          9
007700020422     d cdfs                           8
007800110429     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
007900110429     D ESITO_OK...
008000110429     D                 C                   0
008100110429     ***************************************************************************
008200110429     **
008300110429     ** Dichiarazione prototipi procedure esterne.
008400110429     **
008500110429     ***************************************************************************
008600110429      /DEFINE DFTACTGRP_YES
008700110429     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
008800110429      /UNDEFINE DFTACTGRP_YES
008900110429     ***************************************************************************
009000110429     **
009100110429     ** Definizione strutture dati.
009200110429     **
009300110429      **************************************************************************
009400110429     D tibsSocI0...
009500110502     D               E DS                  prefix(i)
009600110429     D                                     INZ
009700110429     D tibsSocO0...
009800110502     D               E DS                  prefix(o)
009900110429     D                                     INZ
010000110429     ***************************************************************************
010100110429     **
010200110429     ** Definizione variabili modulo/programma.
010300110429     **
010400110429     ***************************************************************************
010500110429     D prmRqsOpCode...
010600110429     D                 S             10A
010700110429     D prmRpyOpCode...
010800110429     D                 S             10A
010900110429     D prmRpyIdMsg...
011000110429     D                 S             10I 0
011100110429     D prmRqsFormato...
011200110429     D                 S             10A
011300110429     D prmRqsDataSize...
011400110429     D                 S             10I 0
011500110429     D prmRpyFormato...
011600110429     D                 S             10A
011700110429     D prmRpyDataSize...
011800110429     D                 S             10I 0
011900110429
012000011031     ***********************************************************************
012100910521     C                   EXSR      DEFCAM
012200020419     c* Lettura fiftt06l ordinato per fornitore padroncino data conteg.
012300020419     c* i record scelti sono: FTTFVL='C', FTTTSR=' ', limiti scelti a
012400020419     c* video per fornitore e data protocollo, numero fattura <> 0
012500020419     c*
012600020419     C/EXEC SQL
012700020419     C+ PREPARE S1 FROM :WrkSqlCmd
012800020419     C/END-EXEC
012900020419
013000020419     C/EXEC SQL
013100020419     C+ DECLARE A1 CURSOR FOR S1
013200020419     C/END-EXEC
013300020419
013400020419     C/EXEC SQL
013500020419     C+ OPEN A1
013600020419     C/END-EXEC
013700020419
013800020419     C                   DOU       SqlCod <> 0
013900020419     C/EXEC SQL
014000020419     C+ FETCH NEXT FROM A1 INTO :fiftt
014100020419     C/END-EXEC
014200020419     c*
014300020419     C                   SELECT
014400020419     C                   WHEN      SqlCod = 0
014500020614     c* se nr.registrazione = * blanks aggiorno data stampa ed esco
014600020531     c                   if        nrass = *blanks
014700020614     c                   movel     salfor        salfors
014800020614     c                   movel     cdfs          salfor
014900020614     c                   exsr      sragg
015000020614     c                   movel     salfors       salfor
015100020531     c                   iter
015200020531     c                   end
015300020419     c* rottura fornitore
015400020422     c                   if        cdfs <> salfor or nrass <> salnra
015500020419     c                   exsr      srrotfor
015600020423     c* se la fattura è = 0 leggo un altro fornitore
015700020423     c                   if        impfat = 0
015800020423     c                   z-add     0             num               3 0
015900020423     c                   z-add     0             imppdr
016000020423     c                   iter
016100020423     c                   end
016200020419     c                   end
016300020422     c* rottura padroncino
016400020419     c                   if        pdrs <> salpdr
016500020419     c                   exsr      srrotpdr
016600020419     c                   end
016700020422     c* dettaglio campi
016800020422     C                   EXSR      srcampi
016900020701     c* stampo la riga solo se il totale è <> 0
017000020701     c                   if        imptg <> 0
017100020701     C                   ADD       1             NUM
017200081003     c*******            add       1             conta
017300020422     c                   seton                                        01
017400020422     c                   exsr      lista
017500020701     c                   end
017600020422     c*
017700020419     C                   WHEN      SqlCod = 100
017800020419     C                   WHEN      SqlCod <> 0
017900020419      * Forzo la stampa del JOBLOG.
018000020419     C                   CALL      'X66CHGJOB'
018100020419     C                   ENDSL
018200020419     C                   ENDDO
018300020419     C/EXEC SQL
018400020419     C+ CLOSE A1
018500020419     C/END-EXEC
018600020422     C* gestisco gli ultimi totali
018700020423     c                   if        impfat <> 0
018800081001     c                   move      'N'           stampa_Corr       1
018900020422     c                   exsr      srrotfor
019000020423     c                   end
019100110429     ** Chiudo il programma.
019200110429     C                   CALL      'TIBSSOCR'
019300110429     C                   PARM      'FINALIZE'    prmRqsOpCode
019400110429     C                   PARM                    prmRpyOpCode
019500110429     C                   PARM                    prmRpyIdMsg
019600020422     c*
019700910521     C                   SETON                                        LR
019800020419     C**************************************************************************
019900020419     C* Rottura fornitore
020000020419     C**************************************************************************
020100020419     C     Srrotfor      BEGSR                                                  *
020200081001      *
020300081001      * emissione dicitura : Corrispettivi......
020400081001     c******             if        stampa_Corr <> 'N'
020500081001     c******             setoff                                       23
020600081001     c******             end
020700020419     c* rotture precedenti
020800020419     c                   exsr      srrotpdr
020900020419     c* stampa la riga di totale
021000020419     c                   if        impfat <> 0
021100020509     c                   seton                                        05
021200020423     c                   else
021300020423     c                   z-add     0             conta
021400020419     c                   end
021500020419     c*
021600020701     c                   if        conta <> 60 and conta <> 0
021700020701     c* salto pagina per avere il totale fattura tutto insieme
021800081003     c*****              if        conta > 24
021900081003     c                   if        conta > 29
022000081001     c                   seton                                          24
022100081001     c******             seton                                        2324
022200020701     c                   end
022300020701     c                   exsr      lista
022400020502     c* aggiorno la data di stampa
022500020614     c                   exsr      sragg
022600071120     c                   end
022700020422     c*
022800020419     c                   z-add     60            conta
022900081001     c******             setoff                                       23
023000081001     c                   setoff                                       24
023100020422     c* azzera il conteggio per fornitore
023200020502     c                   z-add     0             page
023300020502     c                   z-add     0             impon
023400020422     c                   z-add     0             impiva
023500020422     c                   z-add     0             impfat
023600020419     c* decodifico fornitore
023700020419     c                   exsr      srfor
023800081001     c                   setoff                                       23
023900020423     c* aggancio registrazione solo se fornitore ok
024000020423     c                   if        not *in20
024100020422     c                   move      nrass         ivanrasreg
024200020422     c     kiva          chain     ndiva00f
024300020422     c                   if        %found(ndiva00f)
024400020422     c                   move      ivadtdoc      dataeur
024500020422     c                   move      dataeur       dtfat
024600020422     c                   move      ivanrdoc      nrfat
024700020422     c                   move      ivadtreg      dataeur
024800020422     c                   move      dataeur       dtprt
024900020422     c                   move      ivanrreg      nrprt
025000161116     c                   move      ivalibro      ivalib
025100161116     c                   Z-ADD(h)  ivaimporto    impiva
025200161116     c                   Z-ADD(h)  ivaimponib    impon
025300020422     c     impon         add       impiva        impfat
025400020422     c                   end
025500020423     c                   end
025600020531     c                   if        impfat <> 0
025700020531     c                   move      cdfs          salfor
025800020531     c                   move      nrass         salnra
025900020531     c                   end
026000020419     c*
026100020419     c                   ENDSR
026200020419     C**************************************************************************
026300020614     C* aggiorna data di stampa in FIFTT00F
026400020419     C**************************************************************************
026500020614     C     sragg         BEGSR                                                  *
026600020614     c*
026700020614     c                   clear                   ftttsr
026800020614     c     kftt          setll     fiftt06l
026900020614     c                   do        *hival
027000020614     c     kftt          reade     fiftt06l
027100020614     c                   if        %eof(fiftt06l)
027200020614     c                   leave
027300020614     c                   end
027400020614     c* escludo quelli non fatturati oppure già stampati
027500020614     c                   if        fttnff = 0 or fttdcn <> 0
027600020614     c                   iter
027700020614     c                   end
027800020614      * controllo numero registrazione
027900020614     c                   if        %subst(fttflr:2:9) <> *blank
028000020614     c                   movel     fttflr        dftt01
028100020614     c                   move      §fttnrg       com09             9
028200020614     c                   if        com09 <> salnra
028300020614     c                   iter
028400020614     c                   end
028500020614     c                   end
028600020614      *
028700020614     c                   move      *date         dataiso
028800020614     c                   move      dataiso       fttdcn
028900020614     c                   except    aggio
029000020614     c                   enddo
029100071120     c* stampo la lettera x fusione solo se data fattura = 31/12/2007
029200071221     c                   if        ivadtdoc = comdta and
029300071221     c                             f54uni <> '999     '
029400071120     c                   eval      f99socn = f54soc
029500071120     c                   eval      f99kscn = salfor
029600071120     c                   eval      f99tip = 'A'
029700071120     c                   movel(p)  ficn99ds      kpjbu
029800071120     c                   call      'FICN99R'
029900071120     c                   parm                    kpjba
030000071221     c                   end
030100020614     c*
030200020614     c                   ENDSR
030300020614     C**************************************************************************
030400020614     C* Rottura padroncino
030500020614     C**************************************************************************
030600020614     C     Srrotpdr      BEGSR                                                  *
030700020419     c                   seton                                        07
030800020419     c* decodifica padroncino
030900021203     C     kapd          CHAIN     fiapd01L
031000021203     c                   if        %found(fiapd01l)
031100070822     c                   movel(p)  apdpdr        despdr
031200020419     c                   else
031300020419     c                   movel     *blanks       despdr
031400020419    3C                   ENDIF
031500020422     c* stampa la riga di totale padroncino solo se ha più righe
031600020422     c                   eval(h)   comodo = imppdr
031700080926     c******  Prima saltava il totale Autotrasp. se era 1 solo
031800080926     c******   adesso con la stampa dell'importo carburante legge 133/08
031900080926     c******   deve sempre stampare il totale Autotrasp.
032000081006     c******             if        num > 1  and comodo <> impon
032100081006     c                   if        num>=1 and comodo <> 0
032200020419     c                   seton                                        03
032300081002     c********           add       1             conta
032400020419     c                   exsr      lista
032500080926     c                   end
032600020419     c* azzera il conteggio per padroncino
032700020419     c                   z-add     0             num               3 0
032800020419     c                   z-add     0             imppdr
032900020419     c                   move      pdrs          salpdr
033000020419     c*
033100080926     c*
033200020419     c                   endsr
033300020419?     *--------------------------------------------------------------*
033400020419?     *  dati in testata                                             *
033500020419?     *--------------------------------------------------------------*
033600020419     C     srfor         BEGSR
033700020419      *
033800020502     c                   eval      rcoksc = cdfs
033900020422     C     Kfrn          CHAIN     anfrn01l                           20
034000020422     C  N20Krco          CHAIN     anrco98j                           20
034100020423     c* controllo se il fornitore appartiene all'unità di reg. scelta
034200020423     c* se 999 tutte le unità
034300020423     c                   if        not *in20 and f54uni <> '999     '
034400020423     c                             and rcounita <> f54uni
034500020423     c                   seton                                        20
034600020423     c                   end
034700020528     c* se 999 tutte le unità (vuol dire che sono in sede) controllo
034800020528     c* che il fornitore sia in autofatturazione
034900090707     c                   clear                   dtlz01
035000020531     c                   if        not *in20
035100020528     c                   move      cdfs          tlzpdr
035200020528     c     ktlz          chain     tntlz01l
035300020528     c                   if        %found(tntlz01l)
035400020528     c                   movel     tlzflo        dtlz01
035500020531     c                   if        §tlzfl1 = 'S'
035600020531     c                             and f54uni = '999     '
035700020528     c                   seton                                        20
035800020528     c                   end
035900020528     c                   end
036000020528     c                   end
036100020423     c*
036200020423     c                   if        not *in20
036300091109     C                   if        %subst(indtelex: 1: 8) = 'CAP.SOC.'
036400091109     c                   eval      csemi = indtelex
036500091109     c                   else
036600091201     c* per l'ottico non può essere blanks
036700091201     c                   eval      csemi ='.'
036800091109     c                   end
036900020419     C                   MOVEL     sogdes        desemi
037000020422     C                   eval      indemi = indindriz
037100020422     c                   if        indprov <> ' '
037200020422     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
037300020422     c                   else
037400020422     C                   eval      locemi = indlocalit
037500020422     c                   end
037600020422     C                   eval      capemi = indcap
037700091112     c*
037800091112     C                   movel     sogpartiva    piemi            20
037900091112     C                   MOVEL     indprov       premi             2
038000091112     C                   movel     sogcdfisc     cfemi            20
038100161122      **
038200161122     c                   exsr      Retrieve_PEC
038300161122      **
038400091112     C                   if        cfemi <> ' '
038500091112     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
038600091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
038700091119     C                             ': ' + %trimr(cfemi)
038800091112     c                   else
038900091112     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
039000091112     c                   end
039100020419     c* se il fornitore ha un codice IVA
039200020419     C                   CLEAR                   tabg48
039300020419     c                   if        frncdiva <> *blanks
039400020419     C                   CLEAR                   X06DS
039500020419     C                   MOVE      xscsoc        X06SOC
039600020419     C                   MOVE      XSCLIN        X06LIN
039700020419     C                   MOVE      frncdiva      X06CIV
039800020422     C                   MOVE      f54dti        X06DAT
039900020419     C                   CALL      'X06G48'
040000020419     C                   PARM                    X06DS
040100020419     C     X06ERR        IFEQ      '0'
040200020419     C                   MOVEL     X06UNI        TABG48
040300020419     C                   end
040400020419     C                   end
040500020527     c* aggancio la tabella Y4Z per tipologia fornitore
040600020527     c                   if        rcoctgan02 <> ctgans
040700020527     c                   exsr      sry4z
040800020527     c                   movel     rcoctgan02    ctgans
040900020527     c                   end
041000020419     c* aliquota IVA
041100020419     c                   if        frncdiva = *blanks or alig48 <> 0
041200020419     c                   if        alig48 = 0
041300020422     c                   move      §4ziva        comiva            5
041400020419     c                   else
041500020422     c                   move      alig48        comiva
041600020419     c                   end
041700020422     c                   clear                   comiva1           5
041800020422     c                   eval      comiva1 = %subst(comiva: 2: 2)+ ',' +
041900020422     c                             %subst(comiva: 4: 2)
042000020422     c                   eval      desiva = 'Aliquota ' + comiva1 + '%'
042100020422     c                   else
042200020422     c                   eval      desiva = desg48
042300020422     C                   end
042400020502     c* condizioni di pagamento
042500020502  "  C                   reset                   DVCDPDS
042600020502  "  C                   eval      DCPRIC='1'                                   chain
042700020502  "  C                   eval      DCPSOC=XSCSOC
042800020502  "  C                   eval      DCPUNI=frnunita
042900020502  "  C                   eval      DCPVIS='1'                                   lf principale
043000020502  "  C                   eval      DCPALC=*OFF                                  no allocazione
043100020502  "  C                   eval      DCPNRK=1                                     numero chiavi
043200061123  "  C                   eval      DCPLIN=xsclin
043300020502  "  C                   eval      DCPCOD=frncondpag
043400020502  "  C                   eval      DCPERR=*OFF
043500020502  "  C                   CALL      'DVCDP'
043600020502  "  C                   PARM                    DVCDPDS
043700020502  "  C                   PARM                    DVOCDPDS
043800020502  "  C                   if        DCPERR=*OFF
043900020502  "  C                   eval      Descon = CDPdesbrev
044000020502     c                   else
044100020502     c                   clear                   descon
044200020502     C                   end
044300020502     c* banca d'appoggio  INDICATORE 11 x FACTOR
044400020502     c                   if        rcokscfact <> *blanks
044500020502     c                   seton                                        11
044600020502     c                   eval      rcoksc = RCOkscfact
044700020502     C     Krco          CHAIN     anrco98j
044800020502     c                   if        %found(anrco98j)
044900020502     C                   MOVEL     sogdes        desfac
045000061123     C                   MOVEL     rcoksc        codfac
045100020701     C*                  eval      indfac = indindriz
045200020701     c*                  if        indprov <> ' '
045300020701     C*                  eval      locfac = %trimr(indlocalit)+' ('+indprov+')'
045400020701     c*                  else
045500020701     C*                  eval      locfac = indlocalit
045600020701     c*                  end
045700020701     C*                  eval      capfac = indcap
045800020701     C*                  eval      telfac = indtelefon
045900020502     c                   end
046000020502     c                   else
046100020502     c                   setoff                                       11
046200020502     c*banca d'appoggio in alternativa al factor
046300020502     C     KCBA          CHAIN     ANCBA01L
046400020502     c                   if        %found(anCBA01L) AND
046500020502    >C                             CBAABI <> *Blank AND
046600020502    >C                             CBACAB <> *Blank
046700020502    >C                   Move      CBAABI        X59ABI
046800020502    >C                   Move      CBACAB        X59CAB
046900020502A0692C                   Move      XSCSOC        X59SOC
047000020502    >C                   Call      'X59ABI'
047100020502    >C                   Parm                    X59ABIDS
047200020502     C*
047300020502    >C                   If        X59Err = '1'
047400090925    >C                   CLEAR                   iban
047500090925    >C                   CLEAR                   ABI
047600020502    >C                   CLEAR                   CAB
047700020503    >C                   CLEAR                   nrcc
047800020502    >C                   CLEAR                   DESIST
047900020502    >C                   CLEAR                   DESAGE
048000020502    >C                   Else
048100020502     C                   Movel     X59DesIst     DESIST
048200020502     C                   Movel     X59DesAge     DESAGE
048300090925     C                   Movel     CBAiban       iban
048400090925     C                   Movel     CBAABI        ABI               5
048500090925     C                   Movel     CBACAB        CAB               5
048600090925     C                   Movel     CBACcB        nrcc             18
048700020502    >C                   End
048800020502     c                   end
048900020502     c                   end
049000020502     C*
049100020423     C                   end
049200020419      *
049300020419     C                   ENDSR
049400020419      *-----------------------------------------------------***********
049500061121      * memorizzo storico testata anagrafico fornitore solo se no copia iva
049600020419      *-----------------------------------------------------***********
049700061121     C     SRanf0        BEGSR
049800061121     c                   eval      ANFTIP = 'A'
049900070607     c                   eval      ANFTSR = 'A'
050000061121     c                   eval      ANFTrc = '0'
050100061121     c                   eval      ANFSOC = f54soc
050200070905     c                   eval      ANFCDF = salfor
050300061121     c                   eval      ANFNFF = ivanrdoc
050400061121     c                   move      ivadtdoc      ANFDFT
050500061121     c                   clear                   anfpdr
050600061121     c                   clear                   danf0
050700061121     c     kanf0         chain     tnanf01l
050800061121     c                   if        f54uni <>'999     '
050900091111     c                   eval      §ANF0telex = csemi
051000091111     c                   eval      §ANF0prov  = premi
051100091111     c                   eval      §ANF0DES = desemi
051200061121     c                   eval      §ANF0IND = indemi
051300061121     c                   eval      §ANF0CAP = capemi
051400061121     c                   eval      §ANF0LOC  = locemi
051500061121     c                   eval      §ANF0PI   = piemi
051600061121     c                   eval      §ANF0CF   = cfemi
051700161122     c                   eval      §ANF0pec  = peceml
051800161122     c                   eval      §ANF0lIVA = ivaLIB
051900061121     c                   eval      §ANF0CDP  = frncondpag
052000061121     c                   eval      §ANF0DCP  = descon
052100061121     c                   if        *in11
052200061123     c                   eval      §ANF0FAC  = codfac
052300061121     c                   eval      §ANF0DFAC = desfac
052400061121     c                   else
052500090925     c                   eval      §ANF0iban = iban
052600090925     c                   eval      §ANF0ABI  = abi
052700061121     c                   eval      §ANF0CAB  = cab
052800061121     c                   eval      §ANF0NRC  = nrcc
052900061121     c                   eval      §ANF0IST  = desist
053000061121     c                   eval      §ANF0AGE  = desage
053100061121     c                   end
053200061121     c                   eval      §ANF0NRP  = nrprt
053300061121     c                   move      ivadtreg      §ANF0DRP
053400061121     c                   eval      anfuni = danf0
053500061121     c                   if        %found(tnanf01l)
053600061121     c                   update    tnanf000
053700061121     c                   else
053800061121     c                   write     tnanf000
053900061121     c                   end
054000061121     c                   else
054100061121     c* imposto i dati di stampa come memorizzati
054200061121     c                   if        %found(tnanf01l)
054300061121     c                   eval      danf0 = anfuni
054400091111     c                   eval      csemi = §ANF0telex
054500061121     c                   eval      desemi      = §ANF0DES
054600061121     c                   eval      indemi      = §ANF0IND
054700061121     c                   eval      capemi      = §ANF0CAP
054800061121     c                   eval      locemi     = §ANF0LOC
054900091112     c                   eval      premi = §ANF0prov
055000161122     c                   eval      peceml     = §ANF0pec
055100061121     c                   eval      piemi      = §ANF0PI
055200061121     c                   eval      cfemi      = §ANF0CF
055300091112     C                   if        cfemi <> ' '
055400091112     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
055500091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
055600091119     C                             ': ' + %trimr(cfemi)
055700091112     c                   else
055800091112     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
055900091112     c                   end
056000061121     c                   eval      descon     = §ANF0DCP
056100061121     c                   eval      abi        = §ANF0ABI
056200090925     c                   eval      iban       = §ANF0iban
056300061121     c                   eval      cab        = §ANF0CAB
056400061121     c                   eval      nrcc       = §ANF0NRC
056500061121     c                   eval      desist     = §ANF0IST
056600061121     c                   eval      desage     = §ANF0AGE
056700061121     c                   eval      desfac    = §ANF0DFAC
056800061121     c                   end
056900061121     c                   end
057000061121     C                   ENDSR
057100161116      *----------------------------------------------------------------------
057200161116      *   reperisce la PEC se c'è
057300161116      *----------------------------------------------------------------------
057400161116     C     RETRIEVE_PEC  BEGSR
057500161116      *
057600161116     c                   eval        PECEML = 'PEC:'
057700161117     C                   if        cfemi <> ' '
057800161117     C                   eval       prmI_CFEMI = cfEMI
057900161116     C                   clear                   prmO_PECEML
058000161116     C                   CALL      'FICNPECR'
058100161117     C                   PARM                    prmI_CFEMI       20
058200161116     C                   PARM                    prmO_PECEML     100
058300161116      *
058400161116     C                   eval       pecEML = %trim(pecEML) + %trim(prmO_PECEML)
058500161116     c                   end
058600161116      *
058700161116     C                   ENDSR
058800161116      *----------------------------------------------------------------------
058900061121      *-----------------------------------------------------***********
059000061121      * memorizzo storico righe anagrafico fornitore solo se no copia iva
059100061121      *-----------------------------------------------------***********
059200061121     C     SRanf1        BEGSR
059300061121     c                   eval      ANFTIP = 'A'
059400070607     c                   eval      ANFTSR = 'A'
059500061121     c                   eval      ANFTrc = '1'
059600061121     c                   eval      ANFSOC = f54soc
059700061121     c                   eval      ANFCDF = cdfs
059800061121     c                   eval      ANFNFF = ivanrdoc
059900061121     c                   move      ivadtdoc      ANFDFT
060000061121     c                   eval      ANFpdr = pdrs
060100061121     c                   clear                   danf1
060200061121     c     kanf0         chain     tnanf01l
060300061121     c                   if        f54uni <>'999     '
060400061121     c                   eval      §ANF1DES = despdr
060500061121     c                   eval      anfuni = danf1
060600061121     c                   if        %found(tnanf01l)
060700061121     c                   update    tnanf000
060800061121     c                   else
060900061121     c                   write     tnanf000
061000061121     c                   end
061100061121     c                   else
061200061121     c* imposto i dati di stampa come memorizzati
061300061121     c                   if        %found(tnanf01l)
061400061121     c                   eval      danf1 = anfuni
061500061121     c                   eval      despdr      = §ANF1DES
061600061121     c                   end
061700061121     c                   end
061800061121     C                   ENDSR
061900080919      *-----------------------------------------------------***********
062000080919      * Decodifico dati x Carburante legge 133/2008
062100080919      *-----------------------------------------------------***********
062200080919     C     SRanf2        BEGSR
062300080919     c                   eval      ANFTIP = 'A'
062400080919     c                   eval      ANFTSR = 'A'
062500080919     c                   eval      ANFTrc = '2'
062600080919     c                   eval      ANFSOC = f54soc
062700080925     c                   eval      ANFCDF = salFOR
062800080919     c                   eval      ANFNFF = ivanrdoc
062900080919     c                   move      ivadtdoc      ANFDFT
063000080925     c                   eval      ANFpdr = salPDR
063100080919     c                   clear                   danf2
063200111128     c*                  clear                   impPDR
063300081003     c                   clear                   impTGC
063400081003     c                   clear                   impGPI
063500081003     c                   clear                   impVAR
063600081003     c                   clear                   impADGc
063700080919     c     kanf0         chain     tnanf01l
063800080919     c* imposto i dati di stampa come memorizzati
063900080919     c                   if        %found(tnanf01l)
064000080919     c                   eval      danf2 = anfuni
064100080919     c                   eval      impPDR  = §ANF2COR
064200161116     c                   eval(h)   impTGC  = §ANF2TGC
064300080919     c                   eval      impGPI  = §ANF2GPI
064400080919     c                   eval      impVAR  = §ANF2VAR
064500080919     c                   eval      impADGc = §ANF2ADG
064600080919     c                   end
064700080919     C                   ENDSR
064800061121      *-----------------------------------------------------***********
064900061121      * Campi per stampa
065000061121      *-----------------------------------------------------***********
065100061121     C     SRCAMPI       BEGSR
065200020419     C                   Z-ADD     Tots          impcom
065300020419     C                   Z-ADD     exts          impext
065400020422     C     impcom        add       impext        imptg
065500020422     C                   add       imptg         imppdr
065600020419     c                   move      ddcs          dataiso
065700020419     c                   move      dataiso       dataeur
065800020422     c                   move      dataeur       DTDDC
065900020419     C                   ENDSR
066000020422     C**************************************************************************
066100020422     C* Gestione stampa
066200020422     C**************************************************************************
066300020422     C     LISTA         BEGSR                                                  *
066400081002      *
066500081001     c******             if        conta>60 or *in23  or
066600081001     c                   if        conta>60 or *IN24  OR
066700081003     c******                       (conta>34 and *in03) or
066800081003     c                             conta > 35
066900020422     c                   seton                                        3007
067000020422     c                   z-add     1             conta
067100081001      * se stampata anche dicitura : Corrispettivi......
067200081001     c                   if        *in23=*off
067300081001     c                   add       5             conta
067400081001     c                   end
067500020422     c                   end
067600081002      *
067700061121     c* memorizzo nello storico anagrafiche
067800061121     c   30              exsr      sranf0
067900061121     c* testata fattura
068000020422     c   30              write     testa
068100081001     c                   setoff                                       30  24
068200081001     c                   seton                                          23
068300081002      *
068400020508     c* testata lista fatture emesse
068500020508     c                   if        f54lis = 'S' and *inof
068600020508     c                   except    teste
068700020508     c                   setoff                                       of
068800020508     c                   end
068900081002      *
069000081002      * --------------------------------------
069100081002      *  Stampa il totale Autotrasportatore:  --> 03 ON
069200081002      * --------------------------------------
069300081002     c                   if        *in03
069400081002      *
069500080919     c* decodifica dati Carburante Legge 133/08
069600081002     c                   exsr      sranf2
069700020422     c* totale padroncino
069800081002     c                   write     totpdr
069900081002     c                   add       1             conta
070000111125      * stampo solo se ho trovato il record 2 per adeguamento carburante
070100111125     c                   if        %found(tnanf01l) and
070200111125     c                             impGPI <> 0
070300081002      * Controllo se OVRFLOW
070400081003     c                   z-add     34            maxrighe          3 0
070500081002     c                   exsr      chk_OvrFLW
070600081002     c* 1°riga riepilogo carburante
070700081002     c                   write     carbu1
070800081002     c                   add       1             conta
070900081002      *
071000081002      * Controllo se OVRFLOW
071100081002     c                   exsr      chk_OvrFLW
071200081002     c* 2°riga riepilogo carburante
071300081002     c                   write     carbu2
071400081002     c                   add       1             conta
071500081003      *
071600081002      * Controllo se OVRFLOW
071700081002     c                   exsr      chk_OvrFLW
071800081002     c* 3°riga riepilogo carburante
071900081002     c                   write     carbu3
072000081002     c                   add       1             conta
072100081003      *
072200081002      * Controllo se OVRFLOW
072300081002     c                   exsr      chk_OvrFLW
072400081002     c* 4°riga riepilogo carburante
072500081002     c                   write     carbu4
072600081002     c                   add       1             conta
072700081002      *
072800081002      * Controllo se OVRFLOW
072900081003     c                   z-add     31            maxrighe          3 0
073000081002     c                   exsr      chk_OvrFLW
073100081002     c* ultime righe riepilogo carburante
073200081002     c                   write     carbu5
073300081002     c                   add       4             conta
073400081002      *
073500081002     c                   end
073600111125     c                   end
073700081002      *---------------------------------------
073800160120      * rettifiche di sede
073900160120     c   05              exsr      rettifiche
074000020509     c* totale FATTURA FORNITORE
074100020422     c   05              write     totGEN
074200020509     c* dati finali
074300020701     c*  06              write     totGE1
074400020531     c* riga dettaglio fatture emesse solo per fornitori in autofatturazione
074500020531     c                   if        f54lis = 'S' and *in05 and
074600020531     c                             §tlzfl1 = ' '
074700020422     c                   except    det
074800020422     c                   end
074900061121     c* memorizzo storico righe anagrafico x autofat.
075000061121     c                   if        *in01 and despdr <> ' '
075100061121     c                   exsr      sranf1
075200061121     c                   end
075300061121     c* dettaglio
075400020422     c   01              write     riga
075500081003     c   01              add       1             conta
075600020422     C   01              MOVEL     *BLANKS       DESPDR
075700020422     C   01              setoff                                       07
075800020422     c                   movea     '000000'      *in(01)
075900020422     c                   ENDSR
076000160120      *-----------------------------------------------------***********
076100160120      * rettifiche fatture fornitori
076200160120      *-----------------------------------------------------***********
076300160120     C     rettifiche    BEGSR
076400160120     c                   move      dtfat         dtaeur
076500160120     c                   move      dtaeur        dtaiso
076600160120     c                   move      dtaiso        dtfatymd          8 0
076700160120     c                   z-add     nrfat         nrfat9            9 0
076800160120     C/EXEC SQL
076900160120     C+ DECLARE R1 CURSOR FOR SELECT SRESRE, SREUNI, sum( SRETIM )
077000160120     C+    FROM tnsre00f WHERE
077100160120     C+    sRESOC = :f54soc and sretip = 'A'
077200160120     C+    and SRECDF = :salfor and SREDFT = :dtfatymd and SRENFF = :nrfat
077300160120     C+    GROUP BY SRESRE, SREUNI
077400160120     C/END-EXEC
077500160120      *
077600160120     C/EXEC SQL
077700160120     C+ OPEN R1
077800160120     C/END-EXEC
077900160120      *
078000160120     c                   do        *hival
078100160120     C/EXEC SQL
078200160120     C+ FETCH  next FROM R1 INTO :sresre, :sreuni, :sretim
078300160120     C/END-EXEC
078400160120     C                   if        SqlCod = 100
078500160120     c                   Leave
078600160120      *  x errori
078700160120     C                   elseif    SqlCod < 0
078800160120     c                   dump(a)
078900160120     c                   leave
079000160120     c                   endif
079100160120     c                   if        SQLCOD = 0
079200160120     c                   movel     sreuni        dsrexaf
079300160120     c                   eval      nfat = %editc(§srexafnf:'Z')
079400160120     c                   if        §srexafdf > 0
079500160120     c                   move      §SREXAFdf     dtaiso
079600160120     c                   move      dtaiso        dtaeur
079700160120     c                   move      dtaeur        dfat
079800160120     c                   endif
079900160121     c                   movel     sresre        tbeke1
080000160121     c                   movel     'SRE'         tbecod
080100160121     c     ktbe          chain     tntbe01l
080200160121     c                   if        %found(tntbe01l)
080300160121     c                   movel     tbeuni        dsre
080400160121     c                   eval      desret = §sredes
080500160121     c                   endif
080600160120     c                   movel     desdocds      desdoc
080700160120     c                   z-add     sretim        impret
080800160120     c                   write     rigaret
080900160121     c                   add       2             conta
081000160120     c                   endif
081100160120     c                   enddo
081200160120     C/EXEC SQL
081300160120     C+ CLOSE R1
081400160120     C/END-EXEC
081500160120     c* decodifico il tipo servizio
081600160120     c     ksre          setll     tnsre03l
081700160120     c                   do        *hival
081800160120     c     ksre          reade     tnsre03l
081900160120     c                   if        %eof(tnsre03l)
082000160120     c                   leave
082100160120     c                   endif
082200160120     c                   move      *date         dataiso
082300160120     c                   move      dataiso       sredcn
082400160120     c                   update    tnsre000
082500160120     c                   enddo
082600160120     C                   ENDSR
082700020527?     *--------------------------------------------------------------*
082800020527?     *  lettura tabella Y4Z                                         *
082900020527?     *--------------------------------------------------------------*
083000020527     C     sry4z         BEGSR
083100020527     c*  lettura tabella Y4Z
083200020527     c                   clear                   xatbds
083300020527     c                   move      '1'           xtbric
083400020527     c                   move      *blank        xtbkey
083500020527     c                   move      'Y4Z'         xtbcod
083600020527     c                   movel     rcoctgan02    xtbkey
083700020527     c                   call      'XATB'
083800020527     c                   parm                    xatbds
083900020527     c                   if        xtberr = '0'
084000020527     c                   movel     xtbuni        angy4z
084100020527     c                   else
084200020527     c                   clear                   angy4z
084300020527     c                   end
084400020527     C*
084500020527     C                   ENDSR
084600011031     C**********************************************************************
084700910521     C     DEFCAM        BEGSR
084800011031     C**********************************************************************
084900000000     C     *ENTRY        PLIST
085000000000     C                   PARM                    KPJBA
085100020419     C                   MOVEL     KPJBU         ficn54ds
085200020419     C*---------- RICERCA DITTA :
085300020419     C                   MOVEL     'SOC001'      TIPXSC
085400020422     C                   MOVEL     f54soc        SOCXSC
085500020422     c     f54lis        comp      'S'                                    of
085600020419     C                   EXSR      REPSOC
085700020419      *  Definizione chiave di accesso
085800061121     c     kanf0         klist
085900061121     c                   KFLD                    ANFTIP
086000070607     c                   KFLD                    ANFTSR
086100061121     c                   KFLD                    ANFTrc
086200061121     c                   KFLD                    ANFSOC
086300061121     c                   KFLD                    ANFCDF
086400061121     c                   KFLD                    anfdft
086500061121     c                   KFLD                    ANFNFF
086600061121     c                   KFLD                    ANFpdr
086700021203     C     Kapd          KLIST
086800021203     C                   KFLD                    apdtip
086900021203     C                   KFLD                    pdrs
087000021203     c                   movel     'A'           apdtip
087100160121      *
087200160121     c     ktbe          klist
087300160121     c                   kfld                    tbecod
087400160121     c                   kfld                    tbeke1
087500021203     C     Ktlz          KLIST
087600021203     C                   KFLD                    tlztip
087700021203     C                   KFLD                    tlzpdr
087800021203     C                   KFLD                    f54soc
087900021203     c                   movel     'P'           tlztip
088000160120     C     Ksre          KLIST
088100160120     C                   KFLD                    f54soc
088200160120     C                   KFLD                    salfor
088300160120     C                   KFLD                    dtfatymd
088400160120     c                   kfld                    nrfat9
088500020502     C     Kftt          KLIST
088600020502     C                   KFLD                    f54soc
088700020502     C                   KFLD                    salfor
088800020502     C                   KFLD                    ftttsr
088900020502     C     Kfrn          KLIST
089000020502     C                   KFLD                    f54soc
089100020502     C                   KFLD                    cdfs
089200020419     C     Krco          KLIST
089300020422     C                   KFLD                    f54soc
089400020422     C                   KFLD                    rcosnatura
089500020502     C                   KFLD                    rcoksc
089600020422     c                   eval      rcosnatura = 'F'
089700020422     C     Kiva          KLIST
089800020422     C                   KFLD                    ivasys
089900020422     C                   KFLD                    ivanrasreg
090000020502     c                   eval      ivasys = 0
090100020502    >C     KCBA          KLIST
090200020502    >C                   KFLD                    XSCSOC
090300020502    >C                   KFLD                    CBACLIFOR
090400020502    >C                   KFLD                    KCC
090500020502    >C                   KFLD                    rcoKSC
090600020502    >C                   MOVEL     'F'           CBACLIFOR
090700020502     c* società
090800020422     c                   eval      %subst(cmd(3): 31: 3) = F54SOC
090900020502     c* autotrasportatore inizio
091000020422     c                   eval      %subst(cmd(3): 50: 8) = F54pd1
091100020502     c* autotrasportatore fine
091200020422     c                   eval      %subst(cmd(4): 12: 8) = F54pd2
091300020502     c* data inizio
091400020422     c                   move      f54dti        com8              8
091500020419     c                   eval      %subst(cmd(5): 11: 8) = com8
091600020502     c* data fine
091700020422     c                   move      f54dtf        com8
091800020419     c                   eval      %subst(cmd(5): 34: 8) = com8
091900020419     c* MI COMPONGO L'ISTRUZIONE SQL
092000020604     c* l'indicatore 80 mi serve per evitare la doppia ribattitura
092100020604     c* nella stampa della copia IVA
092200020503     c*
092300040217     c* se stampa per libro IVA non controllo data di stampa fattura
092400040217     c* altrimenti deve essere a 0 (CMD,8)
092500020503     c                   if        f54uni = '999     '
092600020604     c                   seton                                        80
092700020419     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
092800020503     C                             +CMD(4)+CMD(5)+CMD(9)+CMD(6)+CMD(7)
092900020503     c                   else
093000020604     c                   setoff                                       80
093100020503     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
093200020503     C                             +CMD(4)+CMD(5)+CMD(8)+CMD(6)+CMD(7)
093300020503     c                   end
093400020419     C                   ENDSR
093500020419     C*----------------------------------------------------*
093600020419     C* Reperimento dati società
093700020419     C*----------------------------------------------------*
093800020419     C     REPSOC        BEGSR
093900020419     C*
094000020419     C                   CALLB     'XSOC'
094100020419     C                   PARM                    TIPXSC            6
094200020419     C                   PARM                    SOCXSC            3
094300020419     C                   PARM                    CDSXSC            9 0
094400020419     C                   PARM                    MODXSC            3
094500020419     C                   PARM      *blanks       RTNXSC            1
094600020419     C                   PARM                    XSOCDS
094700020419     C                   PARM                    KPJBA
094800110429     C     RTNXSC        IFNE      '1'
094900110429     C                   MOVEL     XSOCDS        SOC001
095000110429     c* decodifico i dati della società
095100110429     c                   exsr      Decod_societa
095200110502     c                   if         PRMRPYIDMSG >= 0
095300110502     c                              and oIDSOCIETA <> *blank
095400110429     c*
095500110502     C                   MOVEL     oRAGSOCIALE   RSUT             20
095600110502     c                   movel     oRAGSOCIALE   desric
095700110502     c                   movel     oSEDLEGIND    indric
095800110502     c                   movel     oSEDLEGCAP    capric
095900110502     c                   movel     oPARTITAIVA   piric
096000110502     c                   movel     oCODFISCALE   cfric
096100110429     c     cfric         comp      ' '                                8989
096200110502     c                   if        oSEDLEGPRO = ' '
096300110502     C                   movel     oSEDLEGLOC    locric
096400110429     C                   ELSE
096500110502     C                   eval      locric = %trimr(osedlegloc)+
096600110502     C                             ' ('+osedlegpro+')'
096700110502     c                   end
096800110429     c                   end
096900110429     c                   end
097000020419     C*
097100020419     C                   ENDSR
097200081002     C*----------------------------------------------------*
097300081002     C*   Controlla se deve andare alla pagina seguente
097400081002     C*----------------------------------------------------*
097500081002     C     chk_OvrFLW    BegSR
097600081002     C*
097700081002     C* se superato il limite si emette la testata
097800081002     c                   if        conta > maxrighe
097900081002     c                   seton                                        3007
098000081002     c                   z-add     1             conta
098100081002     c                   end
098200081002      *
098300081002     c* memorizzo nello storico anagrafiche
098400081002     c   30              exsr      sranf0
098500081002     c* testata fattura
098600081002     c   30              write     testa
098700081002     c                   setoff                                       30  24
098800081002     c                   seton                                          23
098900081002     C*
099000081002     C                   ENDSR
099100110429     C**************************************************************************
099200110429     C* decodifica la società
099300110429     C************************************************************
099400110429     C     Decod_societa begSR
099500110429     ** Inizializzo il programma.
099600110429     C                   CALL      'TIBSSOCR'
099700110429     C                   PARM      'INIT'        prmRqsOpCode
099800110429     C                   PARM                    prmRpyOpCode
099900110429     C                   PARM                    prmRpyIdMsg
100000110429      *
100100110429     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
100200110429     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
100300110429     c* forzo società 201
100400110502     C                   EVAL      IidSocieta =f54soc
100500110502     C                   move      F54DTI        IDTVALIDITA
100600110429     C
100700110429     C                   CALL      'TIBSSOCR'
100800110429     C                   PARM      'GETANAGRAF'  prmRqsOpCode
100900110429     C                   PARM      *BLANK        prmRpyOpCode
101000110429     C                   PARM      *ZERO         prmRpyIdMsg
101100110429     C                   PARM      'TIBSSOCI0'   prmRqsFormato
101200110429     C                   PARM                    tibsSocI0
101300110429     C                   PARM                    prmRqsDataSize
101400110429     C                   PARM      'TIBSSOCO0'   prmRpyFormato
101500110429     C                   PARM                    tibsSocO0
101600110429     C                   PARM                    prmRpyDataSize
101700110429      *
101800110429     C                   ENDSR
101900081002     C*----------------------------------------------------*
102000081002      *
102100020502     Ofiftt000  E            aggio
102200020502     o                       fttdcn
102300020502     OQSYSPRT   E            TESTE          2 02
102400020422     O                       RSUT                20
102500030616     O                                         + 25 'LISTA FATTURE EMESSE X AUT'
102600030616     O                                         +  0 'OTRASPORTATORI'
102700030616     O                                          113 'FICN55R'
102800020422     O                       UDATE              127 '  /  /  '
102900020422     O                       PAGE1         Z    132
103000020422     O          E            teste       1  2
103100020503     o                                         +  0 'Fornitore'
103200021010     o                                         + 46 'Fattura'
103300020503     o                                         +  1 'Data'
103400020503     o                                         +  8 'Firma'
103500021010     o                                         + 37 'Data consegna'
103600020503     O          E            DET         3
103700020503     O                       salfor            +  0
103800020422     O                       desemi            +  1
103900020422     O                       nrfat         4   +  1
104000020503     O                       dtfat             +  1 '  /  /    '
104100020503     O                                         +  2 '__________________________'
104200021010     O                                         +  0 '______________'
104300021010     O                                         +  2 '_______________'
104400020419**
104500020419SELECT fttpdr, fttddc, sum(fttitt+fttita+ftttpe+ftttbn+fttmnt),       1
104600020422sum(ftttim), substr(fttflr, 2, 9) as nrass, fttcdf                    2
104700020419from fiftt06l WHERE fttsoc = '000' and fttcdf >='00000000' and        3
104800020503fttcdf <= '00000000' and ftttsr = ' ' and fttfvl = 'C' and            4
104900020419fttdft >= 00000000 and fttdft <= 00000000 and fttnff <> 0             5
105000020528GROUP BY fttcdf, substr(fttflr, 2, 9), fttpdr, fttddc                 6
105100020528ORDER BY fttcdf, nrass, fttpdr, fttddc                                7
105200020503and fttdcn = 0                                                        8
105300040217                                                                      9
