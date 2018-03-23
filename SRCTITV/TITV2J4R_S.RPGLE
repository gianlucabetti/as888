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
010600100907     C                             +iColBold+ 'DESTINATION NAME'       +fColBold
010700100907     C                             +iColBold+ 'ZIP CODE'               +fColBold
010800100907     C                             +iColBold+ 'CITY'                   +fColBold
010900100907     C                             +iColBold+ 'SENDER REF. ID'         +fColBold
011000111221     C                             +iColBold+ 'SENDER REF. ID ALPHA'   +fColBold
011100100907     C                             +iColBold+ 'STATUS'                 +fColBold
011200100910     C                             +iColBold+ 'STATUS DATE'            +fColBold
011300100907     C                             +iColBold+ 'LINK'                   +fColBold
011400100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
011500100908     C                   EVAL      wBufferOut =
011600100910     C                              '"'+'CUSTOMER CODE'          +'";'
011700111221     C                             +'"'+'SHIPMENT DATE'          +'";'
011800100910     C                             +'"'+'DESTINATION NAME'       +'";'
011900100908     C                             +'"'+'ZIP CODE'               +'";'
012000100908     C                             +'"'+'CITY'                   +'";'
012100100908     C                             +'"'+'SENDER REF. ID'         +'";'
012200111221     C                             +'"'+'SENDER REF. ID ALPHA'   +'";'
012300100908     C                             +'"'+'STATUS'                 +'";'
012400100910     C                             +'"'+'STATUS DATE'            +'";'
012500100908     C                             +'"'+'LINK'                   +'"'
012600100908     C                   ENDSL
012700100908     C*
012800071121     C                   EVAL      VAWDTA = wBufferOut
012900071121     C                   WRITE     TIVAW000
013000071121     C                   ENDIF
013100991027     C*
013200080924     C                   READ      TIVGDTMP
013300060119     C*
013400080924     C                   DOW       not %eof(TIVGDTMP)
013500080924     C*
013600080924     C                   eval      fnvac00t = vgdDTA
013700071121     C*
013800071121     C                   clear                   TIVAW000
013900071121     C                   clear                   wBufferOut
014000100907     C                   clear                   wLinkDir
014100100907     C                   clear                   wConsOkSiNo
014200100907     C                   clear                   wConsOkSiNoDes
014300100910     C*
014400100910     C* Solo bolle originali e valide ai fini della spedizione
014500100910     C                   movel     *blanks       wEsito1           1
014600111223     C                   movel     *blanks       wEsito2           1
014700111223     C                   z-add     *zeros        wrkDCM            8 0
014800111223     C                   movel     *blanks       wrkCCA            1
014900100910     C*
015000100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
015100100910     C                                                vacAAS
015200100910     C                                               :vacLNP
015300100910     C                                               :vacNRS
015400100910     C                                               :vacNSP
015500100910     C                                               :pOutLblTyp
015600100910     C                                               :pOutAnnoBO
015700100910     C                                               :pOutLineaParBO
015800100910     C                                               :pOutSerieBO
015900100910     C                                               :pOutNumSpedBO
016000100910     C                                               :pOutDcmBO
016100100910     C                                               :pOutCcaBO
016200100910     C                                               :pOutRblBO))
016300100910     C*
016400100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
016500111223     C*
016600111223     C* Chiamata metodo GetLastChild
016700111223     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
016800111223     C                                                pOutAnnoBO
016900111223     C                                               :pOutLineaParBO
017000111223     C                                               :pOutSerieBO
017100111223     C                                               :pOutNumSpedBO
017200111223     C                                               :pOutAnnoFI
017300111223     C                                               :pOutLineaParFI
017400111223     C                                               :pOutSerieFI
017500111223     C                                               :pOutNumSpedFI
017600111223     C                                               :pOutDcmFI
017700111223     C                                               :pOutCcaFI))
017800111223     C*
017900111223     C                   if        wEsito2 = '0'
018000111223     C                   eval      wrkDCM = pOutDcmFI
018100111223     C                   eval      wrkCCA = pOutCcaFI
018200111223     C                   else
018300111223     C                   eval      wrkDCM = pOutDcmBO
018400111223     C                   eval      wrkCCA = pOutCcaBO
018500111223     C                   endif
018600071210     C*
018700071210     C* Inizializzo campi d wrk
018800100517     C                   EVAL      wCAD = *blanks
018900071213     C                   EVAL      wLOD = *blanks
019000071210     C*
019100071210     C* Aggancio la bolla relativa x il completamento dei dati
019200071210     C     KEYtas30      chain     titas30c
019300071210     C                   if        %found(titas30c)
019400071210     C*
019500071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
019600100517     C                   EVAL      wCAD = tasCAD
019700071213     C                   EVAL      wLOD = tasLOD
019800071213     C*
019900071213     C                   endif
020000071121     C*
020100071121     C* Compondo la stringa dati output relativa al record corrente
020200071121     C                   EXSR      wrifile
020300100910     C*
020400100910     C                   endif
020500040719     C*
020600040719     C* In ogni caso storicizzo e cancello l'input sempre
020700080924     C                   DELETE    TIVGD000
020800040719     C*
020900040719     C* Proseguo con la lettura dell'input
021000080924     C                   READ      TIVGDTMP
021100060119     C*
021200030325     C                   ENDDO
021300071121     C*
021400071121     C* Head finali della struttura HTML
021500071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
021600071210     C***                WRITE     TIVAW000
021700991027     C*
021800030325     C                   EVAL      wrkesito = '0'
021900991027     C*
022000910830     C                   ENDSR
022100100907
022200100907
022300100907
022400100907     C     calSTS        BEGSR
022500100910     C*
022600111223     C                   EVAL      wDataSts       = wrkDCM
022700100907     C*
022800100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
022900111223     C                   IF        wrkDCM > *zeros AND
023000111223     C                             (wrkCCA = *blanks OR wrkCCA = '1')
023100100907     C                   EVAL      wConsOkSiNo    = 'S'
023200100907     C                   EVAL      wConsOkSiNoDes = 'Delivered'
023300100907     C                   ELSE
023400100907     C                   EVAL      wConsOkSiNo    = 'N'
023500100907     C*
023600100907     C* A questo punto stabilisco la descrizione
023700100910     C                   SELECT
023800111223     C                   WHEN      wrkCCA = '1'
023900100907     C                   EVAL      wConsOkSiNoDes = 'Diverted'
024000111223     C                   WHEN      wrkCCA = '2'
024100100907     C                   EVAL      wConsOkSiNoDes = 'Returned'
024200100907     C                   EVAL      wConsOkSiNo    = 'R'
024300111223     C                   WHEN      wrkCCA = '3'
024400100907     C                   EVAL      wConsOkSiNoDes = 'Goods Stolen'
024500111223     C                   WHEN      wrkCCA = '4'
024600100907     C                   EVAL      wConsOkSiNoDes = 'Payment Stolen'
024700111223     C                   WHEN      wrkCCA = '5'
024800100907     C                   EVAL      wConsOkSiNoDes = 'Closed on hold report'
024900111223     C                   WHEN      wrkCCA = '6'
025000100907     C                   EVAL      wConsOkSiNoDes = 'Damage and Returned'
025100111228     C                   WHEN      wrkCCA = '7'
025200111228     C                   EVAL      wConsOkSiNoDes = 'Never entrusted goods'
025300111223     C                   WHEN      wrkCCA = 'A'
025400100907     C                   EVAL      wConsOkSiNoDes = 'New on hold report'
025500111223     C                   WHEN      wrkCCA = 'C'
025600100907     C                   EVAL      wConsOkSiNoDes = 'In delivery'
025700111223     C                   WHEN      wrkCCA = 'R'
025800100907     C                   EVAL      wConsOkSiNoDes = 'Reset shipment'
025900111223     C                   WHEN      wrkCCA = 'S'
026000100907     C                   EVAL      wConsOkSiNoDes = 'Destroyed Goods'
026100111228     C                   OTHER
026200111228     C                   EVAL      wConsOkSiNoDes = '*****'
026300100910     C                   ENDSL
026400100907     C*
026500111223     C                   IF        (vacDLA > wrkDCM AND vacDLA >  vacDAG)
026600100910     C                   EVAL      wDataSts       = vacDLA
026700100907     C                   EVAL      wConsOkSiNoDes = 'Left Notice'
026800100907     C                   ENDIF
026900111223     C                   IF        (vacDAG > wrkDCM AND vacDAG >= vacDLA)
027000100910     C                   EVAL      wDataSts       = vacDAG
027100100907     C                   EVAL      wConsOkSiNoDes = 'On-Hold'
027200100907     C                   ENDIF
027300111223     C                   IF        wrkDCM = *zeros AND
027400100910     C                             vacDLA = *zeros AND
027500100910     C                             vacDAG = *zeros AND
027600111223     C                             wrkCCA = *blanks
027700100910     C                   EVAL      wDataSts       = datcor
027800100910     C*
027900100910     C* Verifico la data consegna richiesta
028000100910     C                   IF        vacDCR > datcor
028100100910     C                   EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
028200100910     C                                              'on ' + %editc(vacDCR:'X')
028300100910     C                   ELSE
028400100910     C                   EVAL      wConsOkSiNoDes = 'In progress'
028500100910     C                   ENDIF
028600100910     C                   ENDIF
028700100907     C*
028800100907     C                   ENDIF
028900100907     C*
029000100907     C                   ENDSR
029100100907
029200100907
029300100907
029400100907     C     makLNK        BEGSR
029500100907     C*
029600160927     C                   EVAL      wLinkDir = 'http://vas.brt.it/vas/' +
029700100907     C                                        'sped_ricdocmit_load.hsm?docmit='+
029800100907     C                                        %trim(%char(vacRMN))             +
029900111221     C                                        '&ksu=' + %editc(vacCCM:'X')     +
030000111221     C                                        '&lang=it'
030100100907     C*
030200100907     C                   ENDSR
030300100907
030400071121
030500071121
030600071121
030700071121     C     wrifile       BEGSR
030800071121     C*
030900100907     C* Determino lo status della spedizione
031000100907     C                   EXSR      calSTS
031100100907     C*
031200111221     C* Costruisco il link diretto (solo x bolle in porto franco)
031300111227     C***                IF        vacCBO = '1'
031400100907     C                   EXSR      makLNK
031500111227     C***                ENDIF
031600100908     C*
031700100908     C                   SELECT
031800100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
031900071121     C                   EVAL      wBufferOut = iRow +
032000111223     C                              iCol+%trim(%editc(vacCCM:'X'))        +fCol+
032100111223     C                              iCol+%trim(%char(vacAAS*10000+vacMGS))+fCol+
032200111223     C                              iCol+%trim(vacRSD)                    +fCol+
032300111223     C                              iCol+%trim(wCAD)                      +fCol+
032400111223     C                              iCol+%trim(wLOD)                      +fCol+
032500111223     C                              iCol+%trim(%editc(vacRMN:'Q'))        +fCol+
032600111223     C                              iCol+%trim(vacRMA)                    +fCol+
032700111223     C                              iCol+%trim(wConsOkSiNoDes)            +fCol+
032800111223     C                              iCol+%trim(%editc(wDataSts:'X'))      +fCol+
032900111223     C                              iCol+%trim(wLinkDir)                  +fCol
033000100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
033100100908     C                   EVAL      wBufferOut =
033200100910     C                               '"'+%trim(%editc(vacCCM:'X'))        +'";'
033300111223     C                              +'"'+%trim(%char(vacAAS*10000+vacMGS))+'";'
033400100910     C                              +'"'+%trim(vacRSD)                    +'";'
033500100910     C                              +'"'+%trim(wCAD)                      +'";'
033600100910     C                              +'"'+%trim(wLOD)                      +'";'
033700100910     C                              +'"'+%trim(%editc(vacRMN:'Q'))        +'";'
033800111221     C                              +'"'+%trim(vacRMA)                    +'";'
033900100910     C                              +'"'+%trim(wConsOkSiNoDes)            +'";'
034000100910     C                              +'"'+%trim(%editc(wDataSts:'X'))      +'";'
034100100910     C                              +'"'+%trim(wLinkDir)                  +'"'
034200100908     C                   ENDSL
034300071121     C*
034400071121     C                   EVAL      VAWDTA = wBufferOut
034500071121     C                   WRITE     TIVAW000
034600071121     C*
034700071121     C                   ENDSR
034800071121
034900071121
035000071121
035100991027
035200991027      /TITLE Operazioni iniziali.
035300991027     C     *inzsr        BEGSR
035400991027     C*
035500991027     C     *ENTRY        PLIST
035600991027     C                   parm                    prmppt
035700991027     C     wrkesito      parm      wrkesito      prmesito
035800100910     C*
035900100910     C* CALCOLA LA DATA CORRENTE
036000100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
036100100910     C                   eval      datcor = %dec(%date() : *ISO)
036200071210     C*
036300071213     C* TITAS30C - Parziale
036400071210     C     KEYtas30      KLIST
036500071210     C                   KFLD                    vacAAS
036600071210     C                   KFLD                    vacLNP
036700071210     C                   KFLD                    vacNRS
036800071210     C                   KFLD                    vacNSP
036900991027     C*
037000991027     C                   ENDSR
037100071121     C*
037200071121** DEC - COSTANTI
037300071121<?xml version="1.0"?>
037400071121<?mso-application progid="Excel.Sheet"?>
037500071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
037600071121xmlns:o="urn:schemas-microsoft-com:office:office"
037700071121xmlns:x="urn:schemas-microsoft-com:office:excel"
037800071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
037900071121xmlns:html="http://www.w3.org/TR/REC-html40">
038000071121</Workbook>
038100071121<Worksheet ss:Name="PAG1">
038200071121</Worksheet>
038300071121<Table border="1">
038400071121</Table>
038500071121<Row>
038600071121</Row>
038700071121<Cell><Data ss:Type="String">
038800071121<Cell><Data ss:Type="Number">
038900071121</Data></Cell>
039000071121
039100071121
039200071121
