000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200111221     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600100517     FTITAS30C  IF   E           K DISK
000700100517     F                                     rename(titas000:titas0)
000800100517     F                                     rename(titas010:titas1)
000900100517     F                                     rename(titasP00:titasP)
001000030924
001100080924     D fnvac00t      e ds
001200071121
001300071121     D*------------
001400071121     D* DECODIFICHE COSTANTI
001500071121     D*------------
001600071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001700071121
001800071121
001900071121     D*------------
002000071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002100071121     D*------------
002200071121     D iHtml           s             10    inz('<html>')
002300071121     D fHtml           s             10    inz('</html>')
002400071121     D iBody           s             10    inz('<body>')
002500071121     D fBody           s             10    inz('</body>')
002600071121     D iTable          s             20    inz('<table border="1">')
002700071121     D fTable          s             10    inz('</table>')
002800071121     D iThead          s             10    inz('<thead>')
002900071121     D fThead          s             10    inz('</thead>')
003000071121     D iRow            s              5    inz('<tr>')
003100071121     D fRow            s              5    inz('</tr>')
003200071121     D iCol            s              5    inz('<td>')
003300071121     D fCol            s              5    inz('</td>')
003400071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003500071121     D fColBold        s              5    inz('</th>')
003600071122     D cApi            s              1    inz('''')
003700071121
003800071121
003900071121     D*------------
004000071121     D* VARIABILI D WRK
004100071121     D*------------
004200030924     D prmppt          S             50
004300030924     D prmesito        S              1
004400030924     D wrkesito        S                   like(prmesito)
004500071122     D wBufferOut      s          10000    varying inz
004600100910     D wDataSts        S              8  0 inz(*zeros)
004700100907     D wConsOkSiNo     S              1    inz(*blanks)
004800100907     D wConsOkSiNoDes  S            300    inz(*blanks)
004900100907     D wLinkDir        S            300    inz(*blanks)
005000100517     D wCAD            s              9    inz(*blanks)
005100071210     D wLOD            s             35    inz(*blanks)
005200071211     D*
005300100910
005400100910
005500100910     D*------------------
005600100910     D* LINKING A DEFINIZIONI ESTERNE
005700100910     D*------------------
005800100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005900100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006000100910
006100030924
006200030325
006300030924     C* MAIN/
006400071121     C*
006500071121     C* Verifico se già presenti dati nel file d output
006600071121     C                   EXSR      chkRECFILMBR
006700071121     C*
006800071121     C* Effettuo elaborazione
006900071121     C                   EXSR      traduci
007000071121     C*
007100921023     C                   SETON                                        LR
007200991027
007300071121
007400071121
007500071121     C     chkRECFILMBR  BEGSR
007600071121     C*
007700071121     C                   READ      TIVAWWWT                               55
007800071121     C*
007900071121     C                   ENDSR
008000071121
008100071121
008200030924
008300991027     C     traduci       BEGSR
008400071121     C*
008500071121     C* Head iniziali della struttura HTML
008600100908     C                   IF        %subst(prmppt:2:1) = 'H'
008700071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
008800071210     C   55              WRITE     TIVAW000
008900100908     C                   ENDIF
009000071121     C*
009100071121     C* Inizializzazone formato e variabili x output
009200071121     C                   clear                   TIVAW000
009300071121     C                   clear                   wBufferOut
009400100907     C                   clear                   wLinkDir
009500100907     C                   clear                   wConsOkSiNo
009600100907     C                   clear                   wConsOkSiNoDes
009700071121     C*
009800071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009900071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010000100908     C*
010100100908     C                   SELECT
010200100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
010300071121     C                   EVAL      wBufferOut = iThead + iRow
010400100910     C                             +iColBold+ 'CUSTOMER CODE'          +fColBold
010500111221     C                             +iColBold+ 'SHIPMENT DATE'          +fColBold
010600160902     C                             +iColBold+ 'CARRIER SHIPMENT ID'    +fColBold
010700100907     C                             +iColBold+ 'DESTINATION NAME'       +fColBold
010800100907     C                             +iColBold+ 'ZIP CODE'               +fColBold
010900100907     C                             +iColBold+ 'CITY'                   +fColBold
011000100907     C                             +iColBold+ 'SENDER REF. ID'         +fColBold
011100111221     C                             +iColBold+ 'SENDER REF. ID ALPHA'   +fColBold
011200100907     C                             +iColBold+ 'STATUS'                 +fColBold
011300100910     C                             +iColBold+ 'STATUS DATE'            +fColBold
011400160902     C***                          +iColBold+ 'LINK'                   +fColBold
011500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
011600100908     C                   EVAL      wBufferOut =
011700100910     C                              '"'+'CUSTOMER CODE'          +'";'
011800160902     C                             +'"'+'SHIPMENT DATE'          +'";'
011900160902     C                             +'"'+'CARRIER SHIPMENT ID'    +'";'
012000100910     C                             +'"'+'DESTINATION NAME'       +'";'
012100100908     C                             +'"'+'ZIP CODE'               +'";'
012200100908     C                             +'"'+'CITY'                   +'";'
012300100908     C                             +'"'+'SENDER REF. ID'         +'";'
012400111221     C                             +'"'+'SENDER REF. ID ALPHA'   +'";'
012500100908     C                             +'"'+'STATUS'                 +'";'
012600100910     C                             +'"'+'STATUS DATE'            +'";'
012700160902     C***                          +'"'+'LINK'                   +'"'
012800100908     C                   ENDSL
012900100908     C*
013000071121     C                   EVAL      VAWDTA = wBufferOut
013100071121     C                   WRITE     TIVAW000
013200071121     C                   ENDIF
013300991027     C*
013400080924     C                   READ      TIVGDTMP
013500060119     C*
013600080924     C                   DOW       not %eof(TIVGDTMP)
013700080924     C*
013800080924     C                   eval      fnvac00t = vgdDTA
013900071121     C*
014000071121     C                   clear                   TIVAW000
014100071121     C                   clear                   wBufferOut
014200100907     C                   clear                   wLinkDir
014300100907     C                   clear                   wConsOkSiNo
014400100907     C                   clear                   wConsOkSiNoDes
014500100910     C*
014600100910     C* Solo bolle originali e valide ai fini della spedizione
014700100910     C                   movel     *blanks       wEsito1           1
014800111223     C                   movel     *blanks       wEsito2           1
014900111223     C                   z-add     *zeros        wrkDCM            8 0
015000111223     C                   movel     *blanks       wrkCCA            1
015100100910     C*
015200100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
015300100910     C                                                vacAAS
015400100910     C                                               :vacLNP
015500100910     C                                               :vacNRS
015600100910     C                                               :vacNSP
015700100910     C                                               :pOutLblTyp
015800100910     C                                               :pOutAnnoBO
015900100910     C                                               :pOutLineaParBO
016000100910     C                                               :pOutSerieBO
016100100910     C                                               :pOutNumSpedBO
016200100910     C                                               :pOutDcmBO
016300100910     C                                               :pOutCcaBO
016400100910     C                                               :pOutRblBO))
016500100910     C*
016600100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
016700111223     C*
016800111223     C* Chiamata metodo GetLastChild
016900111223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
017000111223     C                                                pOutAnnoBO
017100111223     C                                               :pOutLineaParBO
017200111223     C                                               :pOutSerieBO
017300111223     C                                               :pOutNumSpedBO
017400111223     C                                               :pOutAnnoFI
017500111223     C                                               :pOutLineaParFI
017600111223     C                                               :pOutSerieFI
017700111223     C                                               :pOutNumSpedFI
017800111223     C                                               :pOutDcmFI
017900111223     C                                               :pOutCcaFI))
018000111223     C*
018100111223     C                   if        wEsito2 = '0'
018200111223     C                   eval      wrkDCM = pOutDcmFI
018300111223     C                   eval      wrkCCA = pOutCcaFI
018400111223     C                   else
018500111223     C                   eval      wrkDCM = pOutDcmBO
018600111223     C                   eval      wrkCCA = pOutCcaBO
018700111223     C                   endif
018800071210     C*
018900071210     C* Inizializzo campi d wrk
019000100517     C                   EVAL      wCAD = *blanks
019100071213     C                   EVAL      wLOD = *blanks
019200071210     C*
019300071210     C* Aggancio la bolla relativa x il completamento dei dati
019400071210     C     KEYtas30      chain     titas30c
019500071210     C                   if        %found(titas30c)
019600071210     C*
019700071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
019800100517     C                   EVAL      wCAD = tasCAD
019900071213     C                   EVAL      wLOD = tasLOD
020000071213     C*
020100071213     C                   endif
020200071121     C*
020300071121     C* Compondo la stringa dati output relativa al record corrente
020400071121     C                   EXSR      wrifile
020500100910     C*
020600100910     C                   endif
020700040719     C*
020800040719     C* In ogni caso storicizzo e cancello l'input sempre
020900080924     C                   DELETE    TIVGD000
021000040719     C*
021100040719     C* Proseguo con la lettura dell'input
021200080924     C                   READ      TIVGDTMP
021300060119     C*
021400030325     C                   ENDDO
021500071121     C*
021600071121     C* Head finali della struttura HTML
021700071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
021800071210     C***                WRITE     TIVAW000
021900991027     C*
022000030325     C                   EVAL      wrkesito = '0'
022100991027     C*
022200910830     C                   ENDSR
022300100907
022400100907
022500100907
022600100907     C     calSTS        BEGSR
022700100910     C*
022800111223     C                   EVAL      wDataSts       = wrkDCM
022900100907     C*
023000100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
023100111223     C                   IF        wrkDCM > *zeros AND
023200111223     C                             (wrkCCA = *blanks OR wrkCCA = '1')
023300100907     C                   EVAL      wConsOkSiNo    = 'S'
023400100907     C                   EVAL      wConsOkSiNoDes = 'Delivered'
023500100907     C                   ELSE
023600100907     C                   EVAL      wConsOkSiNo    = 'N'
023700100907     C*
023800100907     C* A questo punto stabilisco la descrizione
023900100910     C                   SELECT
024000111223     C                   WHEN      wrkCCA = '1'
024100100907     C                   EVAL      wConsOkSiNoDes = 'Diverted'
024200111223     C                   WHEN      wrkCCA = '2'
024300100907     C                   EVAL      wConsOkSiNoDes = 'Returned'
024400100907     C                   EVAL      wConsOkSiNo    = 'R'
024500111223     C                   WHEN      wrkCCA = '3'
024600100907     C                   EVAL      wConsOkSiNoDes = 'Goods Stolen'
024700111223     C                   WHEN      wrkCCA = '4'
024800100907     C                   EVAL      wConsOkSiNoDes = 'Payment Stolen'
024900111223     C                   WHEN      wrkCCA = '5'
025000100907     C                   EVAL      wConsOkSiNoDes = 'Closed on hold report'
025100111223     C                   WHEN      wrkCCA = '6'
025200100907     C                   EVAL      wConsOkSiNoDes = 'Damage and Returned'
025300111228     C                   WHEN      wrkCCA = '7'
025400111228     C                   EVAL      wConsOkSiNoDes = 'Never entrusted goods'
025500111223     C                   WHEN      wrkCCA = 'A'
025600100907     C                   EVAL      wConsOkSiNoDes = 'New on hold report'
025700111223     C                   WHEN      wrkCCA = 'C'
025800100907     C                   EVAL      wConsOkSiNoDes = 'In delivery'
025900111223     C                   WHEN      wrkCCA = 'R'
026000100907     C                   EVAL      wConsOkSiNoDes = 'Reset shipment'
026100111223     C                   WHEN      wrkCCA = 'S'
026200100907     C                   EVAL      wConsOkSiNoDes = 'Destroyed Goods'
026300111228     C                   OTHER
026400111228     C                   EVAL      wConsOkSiNoDes = '*****'
026500100910     C                   ENDSL
026600100907     C*
026700111223     C                   IF        (vacDLA > wrkDCM AND vacDLA >  vacDAG)
026800100910     C                   EVAL      wDataSts       = vacDLA
026900100907     C                   EVAL      wConsOkSiNoDes = 'Left Notice'
027000100907     C                   ENDIF
027100111223     C                   IF        (vacDAG > wrkDCM AND vacDAG >= vacDLA)
027200100910     C                   EVAL      wDataSts       = vacDAG
027300100907     C                   EVAL      wConsOkSiNoDes = 'On-Hold'
027400100907     C                   ENDIF
027500111223     C                   IF        wrkDCM = *zeros AND
027600100910     C                             vacDLA = *zeros AND
027700100910     C                             vacDAG = *zeros AND
027800111223     C                             wrkCCA = *blanks
027900100910     C                   EVAL      wDataSts       = datcor
028000100910     C*
028100100910     C* Verifico la data consegna richiesta
028200100910     C                   IF        vacDCR > datcor
028300100910     C                   EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
028400100910     C                                              'on ' + %editc(vacDCR:'X')
028500100910     C                   ELSE
028600100910     C                   EVAL      wConsOkSiNoDes = 'In progress'
028700100910     C                   ENDIF
028800100910     C                   ENDIF
028900100907     C*
029000100907     C                   ENDIF
029100100907     C*
029200100907     C                   ENDSR
029300100907
029400100907
029500100907
029600100907     C     makLNK        BEGSR
029700100907     C*
029800160927     C                   EVAL      wLinkDir = 'http://vas.brt.it/vas/' +
029900100907     C                                        'sped_ricdocmit_load.hsm?docmit='+
030000100907     C                                        %trim(%char(vacRMN))             +
030100111221     C                                        '&ksu=' + %editc(vacCCM:'X')     +
030200160902     C                                        '&lang=en'
030300100907     C*
030400100907     C                   ENDSR
030500100907
030600071121
030700071121
030800071121
030900071121     C     wrifile       BEGSR
031000071121     C*
031100100907     C* Determino lo status della spedizione
031200100907     C                   EXSR      calSTS
031300100907     C*
031400111221     C* Costruisco il link diretto (solo x bolle in porto franco)
031500111227     C***                IF        vacCBO = '1'
031600100907     C                   EXSR      makLNK
031700111227     C***                ENDIF
031800100908     C*
031900100908     C                   SELECT
032000100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
032100071121     C                   EVAL      wBufferOut = iRow +
032200111223     C                              iCol+%trim(%editc(vacCCM:'X'))        +fCol+
032300160902     C                              iCol+%trim(%char(vacAAS*10000+vacMGS))+fCol+
032400160902     C                              iCol+%trim(%editc(vacLNP:'X'))
032500160902     C                                  +%trim(%editc(vacNRS:'X'))
032600160902     C                                  +%trim(%editc(vacNSP:'X'))        +fCol+
032700111223     C                              iCol+%trim(vacRSD)                    +fCol+
032800111223     C                              iCol+%trim(wCAD)                      +fCol+
032900111223     C                              iCol+%trim(wLOD)                      +fCol+
033000111223     C                              iCol+%trim(%editc(vacRMN:'Q'))        +fCol+
033100111223     C                              iCol+%trim(vacRMA)                    +fCol+
033200111223     C                              iCol+%trim(wConsOkSiNoDes)            +fCol+
033300111223     C                              iCol+%trim(%editc(wDataSts:'X'))      +fCol+
033400111223     C                              iCol+%trim(wLinkDir)                  +fCol
033500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
033600100908     C                   EVAL      wBufferOut =
033700100910     C                               '"'+%trim(%editc(vacCCM:'X'))        +'";'
033800111223     C                              +'"'+%trim(%char(vacAAS*10000+vacMGS))+'";'
033900160902     C                              +'"'+%trim(%editc(vacLNP:'X'))
034000160902     C                                  +%trim(%editc(vacNRS:'X'))
034100160902     C                                  +%trim(%editc(vacNSP:'X'))        +'";'
034200100910     C                              +'"'+%trim(vacRSD)                    +'";'
034300100910     C                              +'"'+%trim(wCAD)                      +'";'
034400100910     C                              +'"'+%trim(wLOD)                      +'";'
034500100910     C                              +'"'+%trim(%editc(vacRMN:'Q'))        +'";'
034600111221     C                              +'"'+%trim(vacRMA)                    +'";'
034700100910     C                              +'"'+%trim(wConsOkSiNoDes)            +'";'
034800100910     C                              +'"'+%trim(%editc(wDataSts:'X'))      +'";'
034900160902     C***                           +'"'+%trim(wLinkDir)                  +'"'
035000100908     C                   ENDSL
035100071121     C*
035200071121     C                   EVAL      VAWDTA = wBufferOut
035300071121     C                   WRITE     TIVAW000
035400071121     C*
035500071121     C                   ENDSR
035600071121
035700071121
035800071121
035900991027
036000991027      /TITLE Operazioni iniziali.
036100991027     C     *inzsr        BEGSR
036200991027     C*
036300991027     C     *ENTRY        PLIST
036400991027     C                   parm                    prmppt
036500991027     C     wrkesito      parm      wrkesito      prmesito
036600100910     C*
036700100910     C* CALCOLA LA DATA CORRENTE
036800100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
036900100910     C                   eval      datcor = %dec(%date() : *ISO)
037000071210     C*
037100071213     C* TITAS30C - Parziale
037200071210     C     KEYtas30      KLIST
037300071210     C                   KFLD                    vacAAS
037400071210     C                   KFLD                    vacLNP
037500071210     C                   KFLD                    vacNRS
037600071210     C                   KFLD                    vacNSP
037700991027     C*
037800991027     C                   ENDSR
037900071121     C*
038000071121** DEC - COSTANTI
038100071121<?xml version="1.0"?>
038200071121<?mso-application progid="Excel.Sheet"?>
038300071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
038400071121xmlns:o="urn:schemas-microsoft-com:office:office"
038500071121xmlns:x="urn:schemas-microsoft-com:office:excel"
038600071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
038700071121xmlns:html="http://www.w3.org/TR/REC-html40">
038800071121</Workbook>
038900071121<Worksheet ss:Name="PAG1">
039000071121</Worksheet>
039100071121<Table border="1">
039200071121</Table>
039300071121<Row>
039400071121</Row>
039500071121<Cell><Data ss:Type="String">
039600071121<Cell><Data ss:Type="Number">
039700071121</Data></Cell>
039800071121
039900071121
040000071121
