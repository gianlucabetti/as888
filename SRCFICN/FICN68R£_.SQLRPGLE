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
006301090629     d tots                          12  3
006400030827     d nrass                          9  0
006500020422     d cdfs                           8
006600030827     d podas                          3  0
006700030827     d poas                           3  0
006701110502     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
006702110502     D ESITO_OK...
006703110502     D                 C                   0
006704110502     ***************************************************************************
006705110502     **
006706110502     ** Dichiarazione prototipi procedure esterne.
006707110502     **
006708110502     ***************************************************************************
006709110502      /DEFINE DFTACTGRP_YES
006710110502     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
006711110502      /UNDEFINE DFTACTGRP_YES
006712110502     ***************************************************************************
006713110502     **
006714110502     ** Definizione strutture dati.
006715110502     **
006716110502      **************************************************************************
006717110502     D tibsSocI0...
006718110502     D               E DS                  prefix(i)
006719110502     D                                     INZ
006720110502     D tibsSocO0...
006721110502     D               E DS                  prefix(o)
006722110502     D                                     INZ
006723110502     ***************************************************************************
006724110502     **
006725110502     ** Definizione variabili modulo/programma.
006726110502     **
006727110502     ***************************************************************************
006728110502     D prmRqsOpCode...
006729110502     D                 S             10A
006730110502     D prmRpyOpCode...
006731110502     D                 S             10A
006732110502     D prmRpyIdMsg...
006733110502     D                 S             10I 0
006734110502     D prmRqsFormato...
006735110502     D                 S             10A
006736110502     D prmRqsDataSize...
006737110502     D                 S             10I 0
006738110502     D prmRpyFormato...
006739110502     D                 S             10A
006740110502     D prmRpyDataSize...
006741110502     D                 S             10I 0
006742110502
006800011031     ***********************************************************************
006900910521     C                   EXSR      DEFCAM
007000030827     c* Lettura fiatt06l ordinato per fornitore padroncino data conteg.
007100030827     c* i record scelti sono: attdco<> 0 limiti scelti a
007200020419     c* video per fornitore e data protocollo, numero fattura <> 0
007300020419     c*
007400020419     C/EXEC SQL
007500020419     C+ PREPARE S1 FROM :WrkSqlCmd
007600020419     C/END-EXEC
007700020419
007800020419     C/EXEC SQL
007900020419     C+ DECLARE A1 CURSOR FOR S1
008000020419     C/END-EXEC
008100020419
008200020419     C/EXEC SQL
008300020419     C+ OPEN A1
008400020419     C/END-EXEC
008500020419
008600020419     C                   DOU       SqlCod <> 0
008700020419     C/EXEC SQL
008800030827     C+ FETCH NEXT FROM A1 INTO :fiatt
008900020419     C/END-EXEC
009000020419     c*
009100020419     C                   SELECT
009200020419     C                   WHEN      SqlCod = 0
009300020614     c* se nr.registrazione = * blanks aggiorno data stampa ed esco
009400030827     c                   if        nrass = 0
009500020614     c                   movel     salfor        salfors
009600020614     c                   movel     cdfs          salfor
009700020614     c                   exsr      sragg
009800020614     c                   movel     salfors       salfor
009900020531     c                   iter
010000020531     c                   end
010100020419     c* rottura fornitore
010200020422     c                   if        cdfs <> salfor or nrass <> salnra
010300020419     c                   exsr      srrotfor
010400020423     c* se la fattura è = 0 leggo un altro fornitore
010500020423     c                   if        impfat = 0
010600020423     c                   z-add     0             num               3 0
010700020423     c                   z-add     0             imppdr
010800020423     c                   iter
010900020423     c                   end
011000020419     c                   end
011100020422     c* rottura padroncino
011200020419     c                   if        pdrs <> salpdr
011300020419     c                   exsr      srrotpdr
011400020419     c                   end
011500020422     c* dettaglio campi
011600020422     C                   EXSR      srcampi
011700020701     c* stampo la riga solo se il totale è <> 0
011800020701     c                   if        imptg <> 0
011900020701     C                   ADD       1             NUM
012000081003     c*******            add       1             conta
012100020422     c                   seton                                        01
012200020422     c                   exsr      lista
012300020701     c                   end
012400020422     c*
012500020419     C                   WHEN      SqlCod = 100
012600020419     C                   WHEN      SqlCod <> 0
012700020419      * Forzo la stampa del JOBLOG.
012800020419     C                   CALL      'X66CHGJOB'
012900020419     C                   ENDSL
013000020419     C                   ENDDO
013100020419     C/EXEC SQL
013200020419     C+ CLOSE A1
013300020419     C/END-EXEC
013400020422     C* gestisco gli ultimi totali
013500020423     c                   if        impfat <> 0
013600081001     c                   move      'N'           stampa_Corr       1
013700020422     c                   exsr      srrotfor
013800020423     c                   end
013801110502     ** Chiudo il programma.
013802110502     C                   CALL      'TIBSSOCR'
013803110502     C                   PARM      'FINALIZE'    prmRqsOpCode
013804110502     C                   PARM                    prmRpyOpCode
013805110502     C                   PARM                    prmRpyIdMsg
013900020422     c*
014000910521     C                   SETON                                        LR
014100020419     C**************************************************************************
014200020419     C* Rottura fornitore
014300020419     C**************************************************************************
014400020419     C     Srrotfor      BEGSR                                                  *
014500081001      *
014600081001      * emissione dicitura : Corrispettivi......
014700081001     c****               if        stampa_Corr <> 'N'
014800081001     c****               setoff                                       23
014900081001     c****               end
015000020419     c* rotture precedenti
015100020419     c                   exsr      srrotpdr
015200081002      *
015300020419     c* stampa la riga di totale
015400020419     c                   if        impfat <> 0
015500020509     c                   seton                                        05
015600020423     c                   else
015700020423     c                   z-add     0             conta
015800020419     c                   end
015900020419     c*
016000020701     c                   if        conta <> 60 and conta <> 0
016100020701     c* salto pagina per avere il totale fattura tutto insieme
016200081003     c*****              if        conta > 24
016300081003     c                   if        conta > 29
016400081001     c                   seton                                          24
016500081001     c******             seton                                        2324
016600020701     c                   end
016700081002     c*
016800020701     c                   exsr      lista
016900020502     c* aggiorno la data di stampa
017000020614     c                   exsr      sragg
017100020701     c                   end
017200020422     c*
017300020419     c                   z-add     60            conta
017400081001     c******             setoff                                       23
017500081001     c                   setoff                                       24
017600081002     c*
017700020422     c* azzera il conteggio per fornitore
017800020502     c                   z-add     0             page
017900020502     c                   z-add     0             impon
018000020422     c                   z-add     0             impiva
018100020422     c                   z-add     0             impfat
018200020419     c* decodifico fornitore
018300020419     c                   exsr      srfor
018400081001     c                   setoff                                       23
018500020423     c* aggancio registrazione solo se fornitore ok
018600020423     c                   if        not *in20
018700020422     c                   move      nrass         ivanrasreg
018800020422     c     kiva          chain     ndiva00f
018900020422     c                   if        %found(ndiva00f)
019000020422     c                   move      ivadtdoc      dataeur
019100020422     c                   move      dataeur       dtfat
019200020422     c                   move      ivanrdoc      nrfat
019300020422     c                   move      ivadtreg      dataeur
019400020422     c                   move      dataeur       dtprt
019500020422     c                   move      ivanrreg      nrprt
019600020422     c                   Z-ADD     ivaimporto    impiva
019700020422     c                   Z-ADD     ivaimponib    impon
019800020422     c     impon         add       impiva        impfat
019900020422     c                   end
020000020423     c                   end
020100020531     c                   if        impfat <> 0
020200020531     c                   move      cdfs          salfor
020300020531     c                   move      nrass         salnra
020400020531     c                   end
020500020419     c*
020600020419     c                   ENDSR
020700020419     C**************************************************************************
020800030827     C* aggiorna data di stampa in FIATT00F
020900020419     C**************************************************************************
021000020614     C     sragg         BEGSR                                                  *
021100020614     c*
021200030828     c                   eval      attflg = 'C'
021300030827     c     katt          setll     fiatt06l
021400020614     c                   do        *hival
021500030828     c     katt          reade     fiatt06l
021600030827     c                   if        %eof(fiatt06l)
021700020614     c                   leave
021800020614     c                   end
021900020614     c* escludo quelli non fatturati oppure già stampati
022000030827     c                   if        attnff = 0 or attdcn <> 0
022100020614     c                   iter
022200020614     c                   end
022300020614      * controllo numero registrazione
022400030827     c                   if        attnra <> salnra
022500020614     c                   iter
022600020614     c                   end
022700020614      *
022800020614     c                   move      *date         dataiso
022900030827     c                   move      dataiso       attdcn
023000020614     c                   except    aggio
023100020614     c                   enddo
023200071120     c* stampo la lettera x fusione solo se data fattura = 31/12/2007
023300071221     c                   if        ivadtdoc = comdta and
023400071221     c                             f54uni <> '999     '
023500071120     c                   eval      f99socn = f54soc
023600071120     c                   eval      f99kscn = salfor
023700071120     c                   eval      f99tip = 'D'
023800071120     c                   movel(p)  ficn99ds      kpjbu
023900071120     c                   call      'FICN99R'
024000071120     c                   parm                    kpjba
024100071221     c                   end
024200020614     c*
024300020614     c                   ENDSR
024400020614     C**************************************************************************
024500020614     C* Rottura padroncino
024600020614     C**************************************************************************
024700020614     C     Srrotpdr      BEGSR                                                  *
024800020419     c                   seton                                        07
024900020419     c* decodifica padroncino
025000021203     C     kapd          CHAIN     fiapd01L
025100021203     c                   if        %found(fiapd01l)
025200070822     c                   movel(p)  apdpdr        despdr
025300020419     c                   else
025400020419     c                   movel     *blanks       despdr
025500020419    3C                   ENDIF
025600020422     c* stampa la riga di totale padroncino solo se ha più righe
025700020422     c                   eval(h)   comodo = imppdr
025800080926     c******  Prima saltava il totale Autotrasp. se era 1 solo
025900080926     c******   adesso con la stampa dell'importo carburante legge 133/08
026000080926     c******   deve sempre stampare il totale Autotrasp.
026100080926     c******             if        num > 1  and comodo <> impon
026200081006     c                   if        num>=1 and comodo <> 0
026300020419     c                   seton                                        03
026400081002     c******             add       1             conta
026500020419     c                   exsr      lista
026600080926     c                   end
026700020419     c* azzera il conteggio per padroncino
026800020419     c                   z-add     0             num               3 0
026900020419     c                   z-add     0             imppdr
027000020419     c                   move      pdrs          salpdr
027100080926     c*
027200020419     c                   endsr
027300020419?     *--------------------------------------------------------------*
027400020419?     *  dati in testata                                             *
027500020419?     *--------------------------------------------------------------*
027600020419     C     srfor         BEGSR
027700020419      *
027800020502     c                   eval      rcoksc = cdfs
027900020422     C     Kfrn          CHAIN     anfrn01l                           20
028000020422     C  N20Krco          CHAIN     anrco98j                           20
028100020423     c* controllo se il fornitore appartiene all'unità di reg. scelta
028200020423     c* se 999 tutte le unità
028300020423     c                   if        not *in20 and f54uni <> '999     '
028400020423     c                             and rcounita <> f54uni
028500020423     c                   seton                                        20
028600020423     c                   end
028700020528     c* se 999 tutte le unità (vuol dire che sono in sede) controllo
028800020528     c* che il fornitore sia in autofatturazione
028801090707     c                   clear                   dtlz01
028900020531     c                   if        not *in20
029000020528     c                   move      cdfs          tlzpdr
029100020528     c     ktlz          chain     tntlz01l
029200020528     c                   if        %found(tntlz01l)
029300020528     c                   movel     tlzflo        dtlz01
029400020531     c                   if        §tlzfl1 = 'S'
029500020531     c                             and f54uni = '999     '
029600020528     c                   seton                                        20
029700020528     c                   end
029800020528     c                   end
029900020528     c                   end
030000020423     c*
030100020423     c                   if        not *in20
030101091109     C                   if        %subst(indtelex: 1: 8) = 'CAP.SOC.'
030102091109     c                   eval      csemi = indtelex
030103091109     c                   else
030105091201     c* per l'ottico non può essere blanks
030106091201     c                   eval      csemi ='.'
030107091109     c                   end
030200020419     C                   MOVEL     sogdes        desemi
030300020422     C                   eval      indemi = indindriz
030400020422     c                   if        indprov <> ' '
030500020422     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
030600020422     c                   else
030700020422     C                   eval      locemi = indlocalit
030800020422     c                   end
030900020422     C                   eval      capemi = indcap
030901091119     c*
030902091119     C                   movel     sogpartiva    piemi            20
030903091119     C                   MOVEL     indprov       premi             2
030904091119     C                   movel     sogcdfisc     cfemi            20
030905091119     c*
030906091119     C                   if        cfemi <> ' '
030907091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
030908091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
030909091119     C                             ': ' + %trimr(cfemi)
030910091119     c                   else
030911091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
030912091119     c                   end
031300020419     c* se il fornitore ha un codice IVA
031400020419     C                   CLEAR                   tabg48
031500020419     c                   if        frncdiva <> *blanks
031600020419     C                   CLEAR                   X06DS
031700020419     C                   MOVE      xscsoc        X06SOC
031800020419     C                   MOVE      XSCLIN        X06LIN
031900020419     C                   MOVE      frncdiva      X06CIV
032000020422     C                   MOVE      f54dti        X06DAT
032100020419     C                   CALL      'X06G48'
032200020419     C                   PARM                    X06DS
032300020419     C     X06ERR        IFEQ      '0'
032400020419     C                   MOVEL     X06UNI        TABG48
032500020419     C                   end
032600020419     C                   end
032700030827     c* aggancio la tabella Y4Z con tipo servizio D
032800030827     c                   exsr      sry4z
032900020419     c* aliquota IVA
033000020419     c                   if        frncdiva = *blanks or alig48 <> 0
033100020419     c                   if        alig48 = 0
033200020422     c                   move      §4ziva        comiva            5
033300020419     c                   else
033400020422     c                   move      alig48        comiva
033500020419     c                   end
033600020422     c                   clear                   comiva1           5
033700020422     c                   eval      comiva1 = %subst(comiva: 2: 2)+ ',' +
033800020422     c                             %subst(comiva: 4: 2)
033900020422     c                   eval      desiva = 'Aliquota ' + comiva1 + '%'
034000020422     c                   else
034100020422     c                   eval      desiva = desg48
034200020422     C                   end
034300020502     c* condizioni di pagamento
034400020502  "  C                   reset                   DVCDPDS
034500020502  "  C                   eval      DCPRIC='1'                                   chain
034600020502  "  C                   eval      DCPSOC=XSCSOC
034700020502  "  C                   eval      DCPUNI=frnunita
034800020502  "  C                   eval      DCPVIS='1'                                   lf principale
034900020502  "  C                   eval      DCPALC=*OFF                                  no allocazione
035000020502  "  C                   eval      DCPNRK=1                                     numero chiavi
035100020502  "  C                   eval      DCPLIN=xsclin
035200020502  "  C                   eval      DCPCOD=frncondpag
035300020502  "  C                   eval      DCPERR=*OFF
035400020502  "  C                   CALL      'DVCDP'
035500020502  "  C                   PARM                    DVCDPDS
035600020502  "  C                   PARM                    DVOCDPDS
035700020502  "  C                   if        DCPERR=*OFF
035800020502  "  C                   eval      Descon = CDPdesbrev
035900020502     c                   else
036000020502     c                   clear                   descon
036100020502     C                   end
036200020502     c* banca d'appoggio  INDICATORE 11 x FACTOR
036300020502     c                   if        rcokscfact <> *blanks
036400020502     c                   seton                                        11
036500020502     c                   eval      rcoksc = RCOkscfact
036600020502     C     Krco          CHAIN     anrco98j
036700020502     c                   if        %found(anrco98j)
036800020502     C                   MOVEL     sogdes        desfac
036900070606     C                   MOVEL     rcoksc        codfac
037000020502     c                   end
037100020502     c                   else
037200020502     c                   setoff                                       11
037300020502     c*banca d'appoggio in alternativa al factor
037400020502     C     KCBA          CHAIN     ANCBA01L
037500020502     c                   if        %found(anCBA01L) AND
037600020502    >C                             CBAABI <> *Blank AND
037700020502    >C                             CBACAB <> *Blank
037800020502    >C                   Move      CBAABI        X59ABI
037900020502    >C                   Move      CBACAB        X59CAB
038000020502A0692C                   Move      XSCSOC        X59SOC
038100020502    >C                   Call      'X59ABI'
038200020502    >C                   Parm                    X59ABIDS
038300020502     C*
038400020502    >C                   If        X59Err = '1'
038500090928    >C                   CLEAR                   iban
038501090928    >C                   CLEAR                   ABI
038600020502    >C                   CLEAR                   CAB
038700020503    >C                   CLEAR                   nrcc
038800020502    >C                   CLEAR                   DESIST
038900020502    >C                   CLEAR                   DESAGE
039000020502    >C                   Else
039100020502     C                   Movel     X59DesIst     DESIST
039200020502     C                   Movel     X59DesAge     DESAGE
039300090928     C                   Movel     CBAiban       iban
039301090928     C                   Movel     CBAABI        ABI               5
039400090928     C                   Movel     CBACAB        CAB               5
039500090928     C                   Movel     CBACcB        nrcc             18
039600020502    >C                   End
039700020502     c                   end
039800020502     c                   end
039900020502     C*
040000020423     C                   end
040100020419      *
040200020419     C                   ENDSR
040300020419      *-----------------------------------------------------***********
040400020419      * Campi per stampa
040500020419      *-----------------------------------------------------***********
040600020419     C     SRCAMPI       BEGSR
040700090629     C                   Z-ADD     Totv          imptv
040701090629     C                   Z-ADD     Tots          imptg
040800020422     C                   add       imptg         imppdr
040900020419     c                   move      ddcs          dataiso
041000020419     c                   move      dataiso       dataeur
041100020422     c                   move      dataeur       DTDDC
041200030827     c* decodifico po da
041300030827     c                   move      podas         orgfil
041400030827     c                   exsr      srorg
041500030827     c                   movel(p)  orgdes        poda
041600030827     c* decodifico po a
041700030827     c                   move      poas          orgfil
041800030827     c                   exsr      srorg
041900030827     c                   movel(p)  orgdes        poa
042000020419     C                   ENDSR
042100020422     C**************************************************************************
042200030827     C* decodifico po
042300020422     C**************************************************************************
042400030827     C     srorg         BEGSR                                                  *
042500030827     c                   clear                   orgdes
042600030827     c     orgfil        chain     azorg01l
042700030827     C                   ENDSR
042800030827     C**************************************************************************
042900030827     C* Gestione stampa
043000030827     C**************************************************************************
043100030827     C     LISTA         BEGSR                                                  *
043200081002      *
043300081001     c******             if        conta>60 or *in23  or
043400081001     c                   if        conta>60 or *IN24  OR
043500081003     c******                       (conta>34 and *in03) or
043600081003     c                             conta > 35
043700020422     c                   seton                                        3007
043800020422     c                   z-add     1             conta
043900081001      * se stampata anche dicitura : Corrispettivi......
044000081001     c                   if        *in23=*off
044100081002     c                   add       2             conta
044200081001     c                   end
044300020422     c                   end
044400081002      *
044500070606     c* memorizzo nello storico anagrafiche
044600070606     c   30              exsr      sranf0
044700020422     c* testata fattura
044800020422     c   30              write     testa
044900081001     c                   setoff                                       30  24
045000081001     c                   seton                                          23
045100081002      *
045200020508     c* testata lista fatture emesse
045300020508     c                   if        f54lis = 'S' and *inof
045400020508     c                   except    teste
045500020508     c                   setoff                                       of
045600020508     c                   end
045700081002      *
045800081002      * --------------------------------------
045900081002      *  Stampa il totale Autotrasportatore:  --> 03 ON
046000081002      * --------------------------------------
046100081002     c                   if        *in03
046200081002      *
046300080922     c* decodifica dati Carburante Legge 133/08
046400081002     c                   exsr      sranf2
046500020422     c* totale padroncino
046600081002     c                   write     totpdr
046700081002     c                   add       1             conta
046800081002      *
046900081002      * Controllo se OVRFLOW
047000081003     c                   z-add     34            maxrighe          3 0
047100081002     c                   exsr      chk_OvrFLW
047200081002     c* 1°riga riepilogo carburante
047300081002     c                   write     carbu1
047400081002     c                   add       1             conta
047500081002      *
047600081002      * Controllo se OVRFLOW
047700081002     c                   exsr      chk_OvrFLW
047800081002     c* 2°riga riepilogo carburante
047900081002     c                   write     carbu2
048000081002     c                   add       1             conta
048100081002      *
048200081002      * Controllo se OVRFLOW
048300081002     c                   exsr      chk_OvrFLW
048400081002     c* 3°riga riepilogo carburante
048500081002     c                   write     carbu3
048600081002     c                   add       1             conta
048700081002      *
048800081002      * Controllo se OVRFLOW
048900081002     c                   exsr      chk_OvrFLW
049000081002     c* 4°riga riepilogo carburante
049100081002     c                   write     carbu4
049200081002     c                   add       1             conta
049300081002      *
049400081002      * Controllo se OVRFLOW
049500081003     c                   z-add     31            maxrighe          3 0
049600081002     c                   exsr      chk_OvrFLW
049700081002     c* ultime righe riepilogo carburante
049800081002     c                   write     carbu5
049900081002     c                   add       4             conta
050000081002      *
050100081002     c                   end
050200081002      *---------------------------------------
050300081002      *
050400020509     c* totale FATTURA FORNITORE
050500020422     c   05              write     totGEN
050600020509     c* dati finali
050700020701     c*  06              write     totGE1
050800020531     c* riga dettaglio fatture emesse solo per fornitori in autofatturazione
050900020531     c                   if        f54lis = 'S' and *in05 and
051000020531     c                             §tlzfl1 = ' '
051100020422     c                   except    det
051200020422     c                   end
051300070606     c* memorizzo storico righe anagrafico x autofat.
051400070606     c                   if        *in01 and despdr <> ' '
051500070606     c                   exsr      sranf1
051600070606     c                   end
051700020422     c* dettaglio
051800020422     c   01              write     riga
051900081003     c   01              add       1             conta
052000020422     C   01              MOVEL     *BLANKS       DESPDR
052100020422     C   01              setoff                                       07
052200020422     c                   movea     '000000'      *in(01)
052300020422     c                   ENDSR
052400020527?     *--------------------------------------------------------------*
052500020527?     *  lettura tabella Y4Z                                         *
052600020527?     *--------------------------------------------------------------*
052700020527     C     sry4z         BEGSR
052800020527     c*  lettura tabella Y4Z
052900020527     c                   clear                   xatbds
053000020527     c                   move      '1'           xtbric
053100020527     c                   move      *blank        xtbkey
053200020527     c                   move      'Y4Z'         xtbcod
053300030827     c                   movel     'D'           xtbkey
053400020527     c                   call      'XATB'
053500020527     c                   parm                    xatbds
053600020527     c                   if        xtberr = '0'
053700020527     c                   movel     xtbuni        angy4z
053800020527     c                   else
053900020527     c                   clear                   angy4z
054000020527     c                   end
054100020527     C*
054200020527     C                   ENDSR
054300070606      *-----------------------------------------------------***********
054400070606      * memorizzo storico testata anagrafico fornitore solo se no copia iva
054500070606      *-----------------------------------------------------***********
054600070606     C     SRanf0        BEGSR
054700070606     c                   eval      ANFTIP = 'D'
054800070607     c                   eval      ANFTsr = 'D'
054900070606     c                   eval      ANFTrc = '0'
055000070606     c                   eval      ANFSOC = f54soc
055100070905     c                   eval      ANFCDF = salfor
055200070606     c                   eval      ANFNFF = ivanrdoc
055300070606     c                   move      ivadtdoc      ANFDFT
055400070606     c                   clear                   anfpdr
055500070606     c                   clear                   danf0
055600070606     c     kanf0         chain     tnanf01l
055700070606     c                   if        f54uni <>'999     '
055701091111     c                   eval      §ANF0telex = csemi
055702091111     c                   eval      §ANF0prov  = premi
055800070606     c                   eval      §ANF0DES = desemi
055900070606     c                   eval      §ANF0IND = indemi
056000070606     c                   eval      §ANF0CAP = capemi
056100070606     c                   eval      §ANF0LOC  = locemi
056200070606     c                   eval      §ANF0PI   = piemi
056300070606     c                   eval      §ANF0CF   = cfemi
056400070606     c                   eval      §ANF0CDP  = frncondpag
056500070606     c                   eval      §ANF0DCP  = descon
056600070606     c                   if        *in11
056700070606     c                   eval      §ANF0FAC  = codfac
056800070606     c                   eval      §ANF0DFAC = desfac
056900070606     c                   else
056901090928     c                   eval      §ANF0iban = iban
057001090928     c                   eval      §ANF0ABI  = abi
057100070606     c                   eval      §ANF0CAB  = cab
057200070606     c                   eval      §ANF0NRC  = nrcc
057300070606     c                   eval      §ANF0IST  = desist
057400070606     c                   eval      §ANF0AGE  = desage
057500070606     c                   end
057600070606     c                   eval      §ANF0NRP  = nrprt
057700070606     c                   move      ivadtreg      §ANF0DRP
057800070606     c                   eval      anfuni = danf0
057900070606     c                   if        %found(tnanf01l)
058000070606     c                   update    tnanf000
058100070606     c                   else
058200070606     c                   write     tnanf000
058300070606     c                   end
058400070606     c                   else
058500070606     c* imposto i dati di stampa come memorizzati
058600070606     c                   if        %found(tnanf01l)
058700070606     c                   eval      danf0 = anfuni
058701091111     c                   eval      csemi = §ANF0telex
058800070606     c                   eval      desemi      = §ANF0DES
058900070606     c                   eval      indemi      = §ANF0IND
059000070606     c                   eval      capemi      = §ANF0CAP
059100070606     c                   eval      locemi     = §ANF0LOC
059101091119     c                   eval      premi = §ANF0prov
059200070606     c                   eval      piemi      = §ANF0PI
059300070606     c                   eval      cfemi      = §ANF0CF
059301091119     C                   if        cfemi <> ' '
059302091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi) +
059303091119     c                             ' C.F.-R.IMPRESE ' + %trimr(premi) +
059304091119     C                             ': ' + %trimr(cfemi)
059305091119     c                   else
059306091119     C                   eval      picfemi =  'P.IVA: ' + %trimr(piemi)
059307091119     c                   end
059400070606     c                   eval      descon     = §ANF0DCP
059401090928     c                   eval      iban       = §ANF0iban
059500070606     c                   eval      abi        = §ANF0ABI
059600070606     c                   eval      cab        = §ANF0CAB
059700070606     c                   eval      nrcc       = §ANF0NRC
059800070606     c                   eval      desist     = §ANF0IST
059900070606     c                   eval      desage     = §ANF0AGE
060000070606     c                   eval      desfac    = §ANF0DFAC
060100070606     c                   end
060200070606     c                   end
060300070606     C                   ENDSR
060400070606      *-----------------------------------------------------***********
060500070606      * memorizzo storico righe anagrafico fornitore solo se no copia iva
060600070606      *-----------------------------------------------------***********
060700070606     C     SRanf1        BEGSR
060800070606     c                   eval      ANFTIP = 'D'
060900070607     c                   eval      ANFTsr = 'D'
061000070606     c                   eval      ANFTrc = '1'
061100070606     c                   eval      ANFSOC = f54soc
061200070606     c                   eval      ANFCDF = cdfs
061300070606     c                   eval      ANFNFF = ivanrdoc
061400070606     c                   move      ivadtdoc      ANFDFT
061500070606     c                   eval      ANFpdr = pdrs
061600070606     c                   clear                   danf1
061700070606     c     kanf0         chain     tnanf01l
061800070606     c                   if        f54uni <>'999     '
061900070606     c                   eval      §ANF1DES = despdr
062000070606     c                   eval      anfuni = danf1
062100070606     c                   if        %found(tnanf01l)
062200070606     c                   update    tnanf000
062300070606     c                   else
062400070606     c                   write     tnanf000
062500070606     c                   end
062600070606     c                   else
062700070606     c* imposto i dati di stampa come memorizzati
062800070606     c                   if        %found(tnanf01l)
062900070606     c                   eval      danf1 = anfuni
063000070606     c                   eval      despdr      = §ANF1DES
063100070606     c                   end
063200070606     c                   end
063300070606     C                   ENDSR
063400080922      *-----------------------------------------------------***********
063500080922      * Decodifico dati x Carburante legge 133/2008
063600080922      *-----------------------------------------------------***********
063700080922     C     SRanf2        BEGSR
063800080922     c                   eval      ANFTIP = 'D'
063900080922     c                   eval      ANFTSR = 'D'
064000080922     c                   eval      ANFTrc = '2'
064100080922     c                   eval      ANFSOC = f54soc
064200080925     c                   eval      ANFCDF = salFOR
064300080922     c                   eval      ANFNFF = ivanrdoc
064400080922     c                   move      ivadtdoc      ANFDFT
064500080925     c                   eval      ANFpdr = salPDR
064600080922     c                   clear                   danf2
064700081003     c                   clear                   impPDR
064800081003     c                   clear                   impTGC
064900081003     c                   clear                   impGPI
065000081003     c                   clear                   impVAR
065100081003     c                   clear                   impADGc
065200080922     c     kanf0         chain     tnanf01l
065300080922     c* imposto i dati di stampa come memorizzati
065400080922     c                   if        %found(tnanf01l)
065500080922     c                   eval      danf2 = anfuni
065600080922     c                   eval      impPDR  = §ANF2COR
065700080922     c                   eval      impTGC  = §ANF2TGC
065800080922     c                   eval      impGPI  = §ANF2GPI
065900080922     c                   eval      impVAR  = §ANF2VAR
066000080922     c                   eval      impADGc = §ANF2ADG
066100080922     c                   end
066200080922     C                   ENDSR
066300011031     C**********************************************************************
066400910521     C     DEFCAM        BEGSR
066500011031     C**********************************************************************
066600000000     C     *ENTRY        PLIST
066700000000     C                   PARM                    KPJBA
066800020419     C                   MOVEL     KPJBU         ficn54ds
066900020419     C*---------- RICERCA DITTA :
067000020419     C                   MOVEL     'SOC001'      TIPXSC
067100020422     C                   MOVEL     f54soc        SOCXSC
067200020422     c     f54lis        comp      'S'                                    of
067300020419     C                   EXSR      REPSOC
068900020419      *  Definizione chiave di accesso
069000070606     c     kanf0         klist
069100070606     c                   KFLD                    ANFTIP
069200070607     c                   KFLD                    ANFTsr
069300070606     c                   KFLD                    ANFTrc
069400070606     c                   KFLD                    ANFSOC
069500070606     c                   KFLD                    ANFCDF
069600070606     c                   KFLD                    anfdft
069700070606     c                   KFLD                    ANFNFF
069800070606     c                   KFLD                    ANFpdr
069900021203     C     Kapd          KLIST
070000021203     C                   KFLD                    apdtip
070100021203     C                   KFLD                    pdrs
070200030827     c                   movel     'D'           apdtip
070300021203     C     Ktlz          KLIST
070400021203     C                   KFLD                    tlztip
070500021203     C                   KFLD                    tlzpdr
070600021203     C                   KFLD                    f54soc
070700030827     c                   movel     'D'           tlztip
070800030827     C     Katt          KLIST
070900020502     C                   KFLD                    f54soc
071000020502     C                   KFLD                    salfor
071100030828     C                   KFLD                    attflg
071200030827     C     Katt1         KLIST
071300030827     C                   KFLD                    f54soc
071400030827     C                   KFLD                    salfor
071500020502     C     Kfrn          KLIST
071600020502     C                   KFLD                    f54soc
071700020502     C                   KFLD                    cdfs
071800020419     C     Krco          KLIST
071900020422     C                   KFLD                    f54soc
072000020422     C                   KFLD                    rcosnatura
072100020502     C                   KFLD                    rcoksc
072200020422     c                   eval      rcosnatura = 'F'
072300020422     C     Kiva          KLIST
072400020422     C                   KFLD                    ivasys
072500020422     C                   KFLD                    ivanrasreg
072600020502     c                   eval      ivasys = 0
072700020502    >C     KCBA          KLIST
072800020502    >C                   KFLD                    XSCSOC
072900020502    >C                   KFLD                    CBACLIFOR
073000020502    >C                   KFLD                    KCC
073100020502    >C                   KFLD                    rcoKSC
073200020502    >C                   MOVEL     'F'           CBACLIFOR
073300020502     c* società
073400020422     c                   eval      %subst(cmd(3): 31: 3) = F54SOC
073500020502     c* autotrasportatore inizio
073600020422     c                   eval      %subst(cmd(3): 50: 8) = F54pd1
073700020502     c* autotrasportatore fine
073800020422     c                   eval      %subst(cmd(4): 12: 8) = F54pd2
073900020502     c* data inizio
074000020422     c                   move      f54dti        com8              8
074100020419     c                   eval      %subst(cmd(5): 11: 8) = com8
074200020502     c* data fine
074300020422     c                   move      f54dtf        com8
074400020419     c                   eval      %subst(cmd(5): 34: 8) = com8
074500020419     c* MI COMPONGO L'ISTRUZIONE SQL
074600020604     c* l'indicatore 80 mi serve per evitare la doppia ribattitura
074700020604     c* nella stampa della copia IVA
074800020503     c*
074900040217     c* se stampa per libro IVA non controllo data di stampa fattura
075000040217     c* altrimenti deve essere a 0 (CMD,8)
075100020503     c                   if        f54uni = '999     '
075200020604     c                   seton                                        80
075300020419     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
075400020503     C                             +CMD(4)+CMD(5)+CMD(9)+CMD(6)+CMD(7)
075500020503     c                   else
075600020604     c                   setoff                                       80
075700020503     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)
075800020503     C                             +CMD(4)+CMD(5)+CMD(8)+CMD(6)+CMD(7)
075900020503     c                   end
076000020419     C                   ENDSR
076100020419     C*----------------------------------------------------*
076200020419     C* Reperimento dati società
076300020419     C*----------------------------------------------------*
076400020419     C     REPSOC        BEGSR
076500020419     C*
076600020419     C                   CALLB     'XSOC'
076700020419     C                   PARM                    TIPXSC            6
076800020419     C                   PARM                    SOCXSC            3
076900020419     C                   PARM                    CDSXSC            9 0
077000020419     C                   PARM                    MODXSC            3
077100020419     C                   PARM      *blanks       RTNXSC            1
077200020419     C                   PARM                    XSOCDS
077300020419     C                   PARM                    KPJBA
077301110502     C     RTNXSC        IFNE      '1'
077302110502     C                   MOVEL     XSOCDS        SOC001
077303110502     c* decodifico i dati della società
077304110502     c                   exsr      Decod_societa
077305110502     c                   if         PRMRPYIDMSG >= 0
077306110502     c                              and oIDSOCIETA <> *blank
077307110502     c*
077308110502     C                   MOVEL     oRAGSOCIALE   RSUT             20
077309110502     c                   movel     oRAGSOCIALE   desric
077310110502     c                   movel     oSEDLEGIND    indric
077311110502     c                   movel     oSEDLEGCAP    capric
077312110502     c                   movel     oPARTITAIVA   piric
077313110502     c                   movel     oCODFISCALE   cfric
077314110502     c     cfric         comp      ' '                                8989
077315110502     c                   if        oSEDLEGPRO = ' '
077316110502     C                   movel     oSEDLEGLOC    locric
077317110502     C                   ELSE
077318110502     C                   eval      locric = %trimr(osedlegloc)+
077319110502     C                             ' ('+osedlegpro+')'
077320110502     c                   end
077321110502     c                   end
077322110502     c                   end
077400020419     C*
077500020419     C                   ENDSR
077600081002     C*----------------------------------------------------*
077700081002     C*   Controlla se deve andare alla pagina seguente
077800081002     C*----------------------------------------------------*
077900081002     C     chk_OvrFLW    BegSR
078000081002     C*
078100081002     C* se superato il limite si emette la testata
078200081002     c                   if        conta > maxrighe
078300081002     c                   seton                                        3007
078400081002     c                   z-add     1             conta
078500081002     c                   end
078600081002      *
078700081002     c* memorizzo nello storico anagrafiche
078800081002     c   30              exsr      sranf0
078900081002     c* testata fattura
079000081002     c   30              write     testa
079100081002     c                   setoff                                       30  24
079200081002     c                   seton                                          23
079300081002     C*
079400081002     C                   ENDSR
079401110502     C**************************************************************************
079402110502     C* decodifica la società
079403110502     C************************************************************
079404110502     C     Decod_societa begSR
079405110502     ** Inizializzo il programma.
079406110502     C                   CALL      'TIBSSOCR'
079407110502     C                   PARM      'INIT'        prmRqsOpCode
079408110502     C                   PARM                    prmRpyOpCode
079409110502     C                   PARM                    prmRpyIdMsg
079410110502      *
079411110502     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
079412110502     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
079413110502     c* forzo società 201
079414110502     C                   EVAL      IidSocieta =f54soc
079415110502     C                   move      F54DTI        IDTVALIDITA
079416110502     C
079417110502     C                   CALL      'TIBSSOCR'
079418110502     C                   PARM      'GETANAGRAF'  prmRqsOpCode
079419110502     C                   PARM      *BLANK        prmRpyOpCode
079420110502     C                   PARM      *ZERO         prmRpyIdMsg
079421110502     C                   PARM      'TIBSSOCI0'   prmRqsFormato
079422110502     C                   PARM                    tibsSocI0
079423110502     C                   PARM                    prmRqsDataSize
079424110502     C                   PARM      'TIBSSOCO0'   prmRpyFormato
079425110502     C                   PARM                    tibsSocO0
079426110502     C                   PARM                    prmRpyDataSize
079427110502      *
079428110502     C                   ENDSR
079500081002     C*----------------------------------------------------*
079600081002     C*
079700030827     Ofiatt000  E            aggio
079800030827     o                       attdcn
079900020502     OQSYSPRT   E            TESTE          2 02
080000020422     O                       RSUT                20
080100030827     O                                         + 25 'LISTA FATTURE EMESSE X AFF'
080200030827     O                                         +  0 'LUENZE/DEFLUENZE'
080300030827     O                                          113 'FICN68R'
080400020422     O                       UDATE              127 '  /  /  '
080500020422     O                       PAGE1         Z    132
080600020422     O          E            teste       1  2
080700020503     o                                         +  0 'Fornitore'
080800021010     o                                         + 46 'Fattura'
080900020503     o                                         +  1 'Data'
081000020503     o                                         +  8 'Firma'
081100021010     o                                         + 37 'Data consegna'
081200020503     O          E            DET         3
081300020503     O                       salfor            +  0
081400020422     O                       desemi            +  1
081500020422     O                       nrfat         4   +  1
081600020503     O                       dtfat             +  1 '  /  /    '
081700020503     O                                         +  2 '__________________________'
081800021010     O                                         +  0 '______________'
081900021010     O                                         +  2 '_______________'
082000020419**
082100090629SELECT attpdr, attddc, sum(attimpp-attimp),                           1
082200030827sum(attimpp), attnra, attcdf, attfgp, attfga                          2
082300030827from fiatt06l WHERE attsoc = '000' and attcdf >='00000000' and        3
082400030827attcdf <= '00000000' and attdco <> 0 and                              4
082500030827attdft >= 00000000 and attdft <= 00000000 and attnff <> 0             5
082600030827GROUP BY attcdf, attnra, attpdr, attddc, attfgp, attfga               6
082700030827ORDER BY attcdf, attnra, attpdr, attddc, attfgp, attfga               7
082800030827and attdcn = 0                                                        8
082900040217                                                                      9
