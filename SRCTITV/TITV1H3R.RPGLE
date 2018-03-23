000100080805     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000200080624     H DECEDIT('0,') DATEDIT(*DMY.)
000300080624
000400080623     FTITAS33C  IF   E           K DISK
000500080804     F*TITA430C  IF   E           K DISK
000600080130     FTIVGD00F  UF A E             DISK    commit
000700030924
000800041015     D*-----------
000900041015     D* VARIABILI D WRK
001000041015     D*-----------
001100080804     D prmlistacli     S             70A
001200080804     D prmesito        S              1A
001300080624     D wrkesito        S                   like(prmesito)
001400041015
001500041015     D*-----------
001600041015     D* DEFINIZIONE DS ESTERNE
001700041015     D*-----------
001800080213     D trul47ds      E DS
001900080804     D* dta4a         E DS
002000031103
002100030924     D*-----------
002200030924     D* RIDEFINIZIONE CHIAVE BOLLA IN FORMATO ALFA
002300030924     D*-----------
002400030924     D                 DS
002500080623     D   TASAAS                       4S 0
002600080624     D   Sep01                        1A   INZ('-')
002700080623     D   TASLNP                       3S 0
002800080624     D   Sep02                        1A   INZ('-')
002900080623     D   TASNRS                       2S 0
003000080624     D   Sep03                        1A   INZ('-')
003100080623     D   TASNSP                       7S 0
003200080624     D wDS_KEYBOLLA            1     19
003300031103
003400031103     D*------------------
003500031103     D* DS "XSRDA8" - CONTROLLA DATA (8)
003600031103     D*------------------
003700031103     D WLBDA8          DS                  INZ
003800031103     D  G08DAT                 1      8  0
003900031103     D  G08INV                 9     16  0
004000031103     D  G08ERR                17     17
004100031103     D  G08TGI                18     22  0
004200080624
004300080624     D*------------------
004400080624     D* DS "XSRLAV8" - DIFFERENZA GG LAVORATIVI TRA 2 DATE
004500080624     D*------------------
004600080624     D WDAT8           DS
004700080624     D  DATADA                        8  0
004800080624     D  DATAA                         8  0
004900080624     D  GGLAV                         5  0
005000080624
005100080624     D*------------------
005200080624     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
005300080624     D*------------------
005400081120     D InAnno          S              4S 0
005500081120     D InLineaPar      S              3S 0
005600081120     D InSerie         S              2S 0
005700081120     D InNumSped       S              7S 0
005800081120     D OutLblTyp       S              1A
005900081120     D OutAnnoBO       S              4S 0
006000081120     D OutLineaParBO   S              3S 0
006100081120     D OutSerieBO      S              2S 0
006200081120     D OutNumSpedBO    S              7S 0
006300081120     D OutDcmBO        S              8S 0
006400081120     D OutCcaBO        S              1A
006500081120     D OutRblBO        S              1A
006600081120     D OutAnnoFI       S              4S 0
006700081120     D OutLineaParFI   S              3S 0
006800081120     D OutSerieFI      S              2S 0
006900081120     D OutNumSpedFI    S              7S 0
007000081120     D OutDcmFI        S              8S 0
007100081120     D OutCcaFI        S              1A
007200080624     D*------------------
007300080624     D* LINKING A DEFINIZIONI ESTERNE
007400080624     D*------------------
007500080624     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007600080623
007700080623     D*------------------
007800080623     D* VARIABILI DI WRK
007900080623     D*------------------
008000080624     D wDataISO        s               D   DATFMT(*ISO) INZ(D'1999-01-01')
008100080624     D wMeseCor01      s              8  0 INZ
008200080624     D wMesePre01      s              8  0 INZ
008300080624     D wMesePre31      s              8  0 INZ
008400080624     D wMesePre01Inv   s              8  0 INZ
008500080624     D wMesePre31Inv   s              8  0 INZ
008600080624     D dataIn          s              8  0 INZ
008700080624     D dataOut         s              8  0 INZ
008800080624     D wBufferOut      s          10000    VARYING INZ
008900080624     D i               s              3  0 INZ
009000080804     D skCLI           s              7A   DIM(10) INZ
009100080804     D skCLIelem       s              7S 0 INZ
009200080624     D wConsPart       s              1    INZ
009300080624
009400080624     D*------------
009500080624     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
009600080624     D*------------
009700080624     D iHtml           s             10    inz('<html>')
009800080624     D fHtml           s             10    inz('</html>')
009900080624     D iBody           s             10    inz('<body>')
010000080624     D fBody           s             10    inz('</body>')
010100080624     D iTable          s             20    inz('<table border="1">')
010200080624     D fTable          s             10    inz('</table>')
010300080624     D iThead          s             10    inz('<thead>')
010400080624     D fThead          s             10    inz('</thead>')
010500080624     D iRow            s              5    inz('<tr>')
010600080624     D fRow            s              5    inz('</tr>')
010700080624     D iCol            s              5    inz('<td>')
010800080624     D fCol            s              5    inz('</td>')
010900080624     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
011000080624     D fColBold        s              5    inz('</th>')
011100080130
011200030924
011300030325
011400030924     C* MAIN/
011500080623     C*
011600080623     C* Calcolo le date necessarie
011700080623     C                   exsr      calDATE
011800080623     C*
011900080623     C                   movel     'N'           wProcedi          1
012000080213     C*
012100080624     C* Avvio il blocco elaborazione TIVGD x tipo file: 'EC'
012200080213     C                   clear                   trul47ds
012300080213     C                   eval      d47opz  = 'I'
012400080624     C                   eval      d47tip  = 'EC'
012500080213     C                   eval      d47lck  = 'N'
012600080213     C                   eval      d47chkj = 'N'
012700080624     C                   eval      d47pgm  = 'TITV1H3C'
012800080213     C                   call      'TRUL47R'
012900080213     C                   parm                    trul47ds
013000080213     C*
013100080213     C* Se elaborazione consentita => proseguo
013200080213     C                   if        d47sts <> 'A'
013300080623     C                   movel     'S'           wProcedi
013400080213     C                   endif
013500080213     C*
013600080213     C* Se ok a procedere => elaboro
013700080623     C                   if        wProcedi = 'S'
013800080624     C*
013900080624     C* Header iniziali
014000080624     C                   exsr      wri_MARES_I
014100041015     C*
014200041015     C* Esegue traduzione
014300080624     C                   for       i = 1 to %elem(skCLI)
014400080804     C                   if        skCLI(i) <> *blanks
014500080624     C                   exsr      traduci
014600080624     C                   endif
014700080624     C                   endfor
014800080624     C*
014900080624     C* Closer finali
015000080624     C                   exsr      wri_MARES_F
015100080128     C*
015200080128     C* Se nn già diversamente valorizzato imposto qui l'esito a OK
015300080624     C                   if        wrkesito = *blanks
015400080624     C                   eval      wrkesito = '1'
015500080624     C                   endif
015600080213     C*
015700080213     C                   endif
015800080128     C*
015900080213     C* Infine eliminazione "blocco TRUL47"
016000080624     C                   exsr      endTRUL47
016100080213     C*
016200921023     C                   SETON                                        LR
016300080213
016400080213
016500080213
016600080213     C     endTRUL47     BEGSR
016700080213     C*
016800080623     C                   if        wProcedi = 'S'
016900080213     C*
017000080624     C* Elimino il blocco elaborazione TIVGD x tipo file: 'EC'
017100080213     C                   clear                   trul47ds
017200080213     C                   eval      d47opz  = 'F'
017300080624     C                   eval      d47tip  = 'EC'
017400080213     C                   call      'TRUL47R'
017500080213     C                   parm                    trul47ds
017600080213     C*
017700080213     C                   endif
017800080213     C*
017900080213     C                   ENDSR
018000991027
018100030924
018200030924
018300991027     C     traduci       BEGSR
018400080624     C*
018500080624     C* Posizionamento sulla bolle del cliente del "mese scorso"
018600080804     C                   move(P)   skCLI(i)      skCLIelem
018700080804     C                   eval      tasCCM = skCLIelem
018800080624     C                   movel     wMesePre01    tasAAS
018900080624     C                   move      wMesePre01    tasMGS
019000080624     C     KEYtas33_C    setll     titas33c
019100080624     C                   if        %found(titas33c)
019200080624     C*
019300080624     C* Prima lettura fuori ciclo
019400080624     C     tasCCM        reade     titas33c
019500080624     C*
019600080624     C* Scorro tutte le bolle del cliente del periodo richiesto
019700080624     C                   dow       not %eof(titas33c)
019800080624     C                   if        tasAAS*10000+tasMGS > wMesePre31
019900080624     C                   leave
020000080624     C                   else
020100080624     C*
020200080624     C                   setoff                                       31
020300080624     C*
020400080624     C* Chiamata metodo GetLblTyp
020500080624     C                   if        UBLBLSPE_GetLblTyp(tasAAS
020600080624     C                                               :tasLNP
020700080624     C                                               :tasNRS
020800080624     C                                               :tasNSP
020900080624     C                                               :OutLblTyp
021000080624     C                                               :OutAnnoBO
021100080624     C                                               :OutLineaParBO
021200080624     C                                               :OutSerieBO
021300080624     C                                               :OutNumSpedBO
021400080624     C                                               :OutDcmBO
021500080806     C                                               :OutCcaBO
021600080806     C                                               :OutRblBO) = 0
021700080806     C                   if        OutLblTyp <> 'O' OR
021800080806     C                             OutRblBO   = 'R'
021900080624     C                   seton                                        31
022000080624     C                   endif
022100080624     C                   endif
022200080624     C*
022300080624     C* Restituisco solamente le bolle originali
022400080624     C                   if        *in31 = *off
022500080624     C*
022600080624     C* Eseguo routine reperimento dati
022700080624     C                   exsr      rtvDati
022800080624     C*
022900080624     C* Eseguo routine scarico buffer file testata cliente
023000080624     C                   exsr      wri_MARES_EC
023100080624     C                   endif
023200080624     C                   endif
023300080624     C*
023400080624     C* Proseguo con la lettura
023500080624     C     tasCCM        reade     titas33c
023600080624     C                   enddo
023700080624     C                   endif
023800991027     C*
023900080624     C* Al termine sancisco le transazioni in sospeso (effettuo 1 solo commit complessivo x
024000080624     C* evitare eventuali duplicazioni d dati al cliente in caso d interruzione imprevista)
024100080624     C                   commit
024200080624     C*
024300910830     C                   ENDSR
024400080130
024500080130
024600080130
024700080130     C     rtvDati       BEGSR
024800080130     C*
024900080624     C* Quindi aggancio l'estensione riferimenti bolla x RMA
025000080804     C*                   clear                   dta4a
025100080804     C*                   eval      ta4TRC = 'A'
025200080804     C*     KEYta430_C    chain     tita430c
025300080804     C*                   if        %found(tita430c)
025400080804     C*                   eval      dta4a = ta4NOT
025500080804     C*                   endif
025600080624     C*
025700080624     C* Verifico se spedizione a Supermercato o con Appuntamento
025800080624     C                   reset                   wConsPart
025900080624     C                   if        TASFTC = 'S' or TASTC2 = 'S'
026000080624     C                   eval      wConsPart = 'S'
026100080624     C                   else
026200080624     C                   if        TASFTC = 'A' or TASTC2 = 'A'
026300080624     C                   eval      wConsPart = 'A'
026400080624     C                   endif
026500080624     C                   endif
026600080624     C*
026700080624     C* Reperisco il numero d giorni lavorativi intercorsi x la consegna
026800080624     C*
026900080624     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
027000080624     C                   clear                   WDAT8
027100080624     C                   z-add     tasDRT        DATADA
027200080624     C                   z-add     tasDCM        DATAA
027300080624     C                   call      'XSRLAV8'
027400080624     C                   parm                    WDAT8
027500080130     C*
027600080130     C                   ENDSR
027700080130     C***
027800080130
027900080624
028000080624
028100031103
028200080624     C     wri_MARES_I   BEGSR
028300080624     C*
028400080624     C* Scrivo gli header iniziali
028500080624     C                   eval      wBufferout = iHtml + iBody + iTable
028600080624     C                   exsr      wri_VGD
028700080624     C*
028800080624     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
028900080624     C                   eval      wBufferOut = iThead + iRow
029000080624     C                             +iColBold+'Nazione destinazione'+fColBold
029100080624     C                             +iColBold+'Riferimenti Mittente'+fColBold
029200080624     C                             +iColBold+'Data ritiro'+fColBold
029300080624     C                             +iColBold+'Data consegna'+fColBold
029400080624     C                             +iColBold+'Località destinazione'+fColBold
029500080624     C                             +iColBold+'Provincia destinazione'+fColBold
029600080624     C                             +iColBold+'Giorni delivery'+fColBold
029700080624     C                             +iColBold+'Supermerc/Appuntam'+fColBold
029800110524     C                             +iColBold+'Num. Sped. BRT'+fColBold
029900080624     C                   exsr      wri_VGD
030000080624     C*
030100080624     C                   ENDSR
030200080624     C***
030300080624
030400080624
030500080624
030600080624
030700080624     C     wri_MARES_F   BEGSR
030800080624     C*
030900080624     C* Scrivo i closer finali
031000080624     C                   eval      wBufferOut = fTable + fBody + fHtml
031100080624     C                   exsr      wri_VGD
031200080624     C*
031300080624     C                   ENDSR
031400080624     C***
031500080624
031600080624
031700080624
031800080624
031900080624     C     wri_MARES_EC  BEGSR
032000080624     C*
032100080624     C* Valorizzo il buffer d output
032200080624     C                   EVAL      wBufferOut = iRow
032300080624     C                             +iCol+%trim(tasNZD)+fCol
032400080804     C***                          +iCol+%trim(§TA4ARMA)+fCol
032500080804     C                             +iCol+%editc(tasRMN:'3')+fCol
032600080624     C                             +iCol+%editc(tasDRT:'X')+fCol
032700080624     C                             +iCol+%editc(tasDCM:'X')+fCol
032800080624     C                             +iCol+%trim(tasLOD)+fCol
032900080624     C                             +iCol+tasPRD+fCol
033000080624     C                             +iCol+%editc(GGLAV:'X')+fCol
033100080624     C                             +iCol+%trim(wConsPart)+fCol
033200080624     C                             +iCol+wDS_KEYBOLLA+fCol
033300080129     C*
033400080624     C                   exsr      wri_VGD
033500080129     C*
033600080129     C                   ENDSR
033700080624
033800080624
033900080624
034000080624
034100080624     C     wri_VGD       BEGSR
034200080624     C*
034300080624     C* Preparo la scrittura del file TIVGD
034400080624     C                   clear                   tivgd000
034500080624     C                   eval      vgdtip = 'EC'
034600080624     C                   eval      vgdksu = *all'0'
034700080624     C                   move      skCLI(1)      vgdksu
034800080624     C                   eval      vgdtsc = 'WW'
034900080624     C                   eval      vgddat = datcor
035000080624     C                   eval      vgdpgm = 'TITV1H3C'
035100080624     C*
035200080624     C                   eval      vgddta = %trim(wBufferOut)
035300080624     C*
035400080624     C                   write     tivgd000
035500080624     C*
035600080624     C                   ENDSR
035700080128
035800080128
035900080128
036000080128
036100080128     C     *pssr         BEGSR
036200080128     C*
036300080130     C                   eval      wrkesito = '2'
036400080128     C*
036500080128     C                   seton                                        LR
036600080128     C*
036700080128     C                   ENDSR     '*CANCL'
036800080128     C***
036900080623
037000080623
037100080623
037200080623
037300080623     C     calDATE       BEGSR
037400080623     C*
037500080623     C* Dalla data corrente ottengo il primo del mese corrente
037600080623     C                   movel(P)  wAAAAMMcor    wMeseCor01
037700080623     C                   move      '01'          wMeseCor01
037800080623     C*
037900080623     C* Dal primo del mese corrente ottengo l'ultimo del mese precedente
038000080623     C     *ISO          move      wMeseCor01    wDataISO
038100080623     C     wDataISO      subdur    1:*D          wDataISO
038200080623     C     *ISO          move      wDataISO      wMesePre31
038300080623     C* Inverto la data finale elaborazione
038400080623     C                   z-add     wMesePre31    dataIn
038500080623     C                   exsr      invDATE
038600080623     C                   z-add     dataOut       wMesePre31Inv
038700080623     C*
038800080623     C* Dal ultimo del mese precedente ottengo il primo del mese precedente
038900080623     C                   movel(P)  wMesePre31    wMesePre01
039000080623     C                   move      '01'          wMesePre01
039100080624     C*
039200080623     C* Inverto la data iniziale elaborazione
039300080623     C                   z-add     wMesePre01    dataIn
039400080623     C                   exsr      invDATE
039500080623     C                   z-add     dataOut       wMesePre01Inv
039600080623     C*
039700080623     C                   ENDSR
039800080623
039900080623
040000080623
040100080623
040200080623     C     invDATE       BEGSR
040300080623     C*
040400080623     C                   z-add     dataIn        g08inv
040500080623     C                   z-add     *zeros        g08dat
040600080623     C                   movel     '3'           g08err
040700080623     C                   call      'XSRDA8'
040800080623     C                   parm                    wlbda8
040900080623     C                   z-add     g08dat        dataOut                        *DATA CORRENTE GGMMA
041000080623     C*
041100080623     C                   ENDSR
041200080623
041300031103
041400031103
041500991027
041600991027      /TITLE Operazioni iniziali.
041700991027     C     *inzsr        BEGSR
041800991027     C*
041900991027     C     *ENTRY        PLIST
042000080804     C                   parm                    prmlistacli
042100080624     C     wrkesito      parm      wrkesito      prmesito
042200080804     C*
042300080804     C                   movea     prmlistacli   skCLI
042400031103     C*
042500031103     C* Calcola la data corrente
042600031103     C                   time                    wn14             14 0
042700031103     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
042800031103     C                   z-add     wn8           g08dat
042900031103     C                   z-add     *zeros        g08inv
043000031103     C                   movel     '0'           g08err
043100031103     C                   call      'XSRDA8'
043200031103     C                   parm                    wlbda8
043300031103     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
043400080623     C                   movel     datcor        wAAAAMMcor        6 0
043500030709     C*
043600030709     C* Definizione chiavi
043700031103     C*
043800080623     C* Chiave su TITAS33C - Completa
043900080623     C     KEYtas33_C    KLIST
044000080623     C                   KFLD                    tasCCM
044100080623     C                   KFLD                    tasAAS
044200080623     C                   KFLD                    tasMGS
044300080129     C*
044400080623     C* Chiave su TITA430C - Completa
044500080804     C*     KEYta430_C    KLIST
044600080804     C*                   KFLD                    tasAAS
044700080804     C*                   KFLD                    tasLNP
044800080804     C*                   KFLD                    tasNRS
044900080804     C*                   KFLD                    tasNSP
045000080804     C*                   KFLD                    ta4TRC
045100991027     C*
045200991027     C                   ENDSR
