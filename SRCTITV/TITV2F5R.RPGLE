000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200100910     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600100517     FTITAS30C  IF   E           K DISK
000700100517     F                                     rename(titas000:titas0)
000800100517     F                                     rename(titas010:titas1)
000900100517     F                                     rename(titasP00:titasP)
001000110519     FTITA430C  IF   E           K DISK
001100030924
001200080924     D fnvac00t      e ds
001300071121
001400071121     D*------------
001500071121     D* DECODIFICHE COSTANTI
001600071121     D*------------
001700071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001800071121
001900071121
002000071121     D*------------
002100071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002200071121     D*------------
002300071121     D iHtml           s             10    inz('<html>')
002400071121     D fHtml           s             10    inz('</html>')
002500071121     D iBody           s             10    inz('<body>')
002600071121     D fBody           s             10    inz('</body>')
002700071121     D iTable          s             20    inz('<table border="1">')
002800071121     D fTable          s             10    inz('</table>')
002900071121     D iThead          s             10    inz('<thead>')
003000071121     D fThead          s             10    inz('</thead>')
003100071121     D iRow            s              5    inz('<tr>')
003200071121     D fRow            s              5    inz('</tr>')
003300071121     D iCol            s              5    inz('<td>')
003400071121     D fCol            s              5    inz('</td>')
003500071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003600071121     D fColBold        s              5    inz('</th>')
003700071122     D cApi            s              1    inz('''')
003800071121
003900071121
004000071121     D*------------
004100071121     D* VARIABILI D WRK
004200071121     D*------------
004300030924     D prmppt          S             50
004400030924     D prmesito        S              1
004500030924     D wrkesito        S                   like(prmesito)
004600071122     D wBufferOut      s          10000    varying inz
004700100910     D wDataSts        S              8  0 inz(*zeros)
004800100907     D wConsOkSiNo     S              1    inz(*blanks)
004900100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005000100907     D wLinkDir        S            300    inz(*blanks)
005100110519     D wDSP            s             10    inz(*blanks)
005200110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
005300110519     D wIND            s                   inz(*blanks) like(tasIND)
005400110519     D wCAD            s                   inz(*blanks) like(tasIND)
005500110519     D wLOD            s                   inz(*blanks) like(tasLOD)
005600110519     D wPRD            s                   inz(*blanks) like(tasPRD)
005700071211     D*
005800100910
005900100910
006000100910     D*------------------
006100100910     D* LINKING A DEFINIZIONI ESTERNE
006200100910     D*------------------
006300100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006400100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006500100910
006600030924
006700030325
006800030924     C* MAIN/
006900071121     C*
007000071121     C* Verifico se già presenti dati nel file d output
007100071121     C                   EXSR      chkRECFILMBR
007200071121     C*
007300071121     C* Effettuo elaborazione
007400071121     C                   EXSR      traduci
007500071121     C*
007600921023     C                   SETON                                        LR
007700991027
007800071121
007900071121
008000071121     C     chkRECFILMBR  BEGSR
008100071121     C*
008200071121     C                   READ      TIVAWWWT                               55
008300071121     C*
008400071121     C                   ENDSR
008500071121
008600071121
008700030924
008800991027     C     traduci       BEGSR
008900071121     C*
009000071121     C* Head iniziali della struttura HTML
009100100908     C                   IF        %subst(prmppt:2:1) = 'H'
009200071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
009300071210     C   55              WRITE     TIVAW000
009400100908     C                   ENDIF
009500071121     C*
009600071121     C* Inizializzazone formato e variabili x output
009700071121     C                   clear                   TIVAW000
009800071121     C                   clear                   wBufferOut
009900100907     C                   clear                   wLinkDir
010000100907     C                   clear                   wConsOkSiNo
010100100907     C                   clear                   wConsOkSiNoDes
010200071121     C*
010300071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010400071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010500100908     C*
010600100908     C                   SELECT
010700100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
010800071121     C                   EVAL      wBufferOut = iThead + iRow
010900110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
011000110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
011100110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
011200110519     C                             +iColBold+ 'RAG. SOC. DESTINATARIO' +fColBold
011300110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
011400110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
011500110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
011600110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
011700110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
011800110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
011900110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
012000110519     C                             +iColBold+ 'STATO'                  +fColBold
012100110519     C                             +iColBold+ 'DATA STATO'             +fColBold
012200100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
012300100908     C                   EVAL      wBufferOut =
012400110519     C                              '"'+      'CODICE CLIENTE'         +'";'
012500110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
012600110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
012700110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
012800110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
012900110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
013000110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
013100110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
013200110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
013300110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
013400110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
013500110519     C                             +'"'+      'STATO'                  +'";'
013600110519     C                             +'"'+      'DATA STATO'             +'";'
013700100908     C                   ENDSL
013800100908     C*
013900071121     C                   EVAL      VAWDTA = wBufferOut
014000071121     C                   WRITE     TIVAW000
014100071121     C                   ENDIF
014200991027     C*
014300080924     C                   READ      TIVGDTMP
014400060119     C*
014500080924     C                   DOW       not %eof(TIVGDTMP)
014600080924     C*
014700080924     C                   eval      fnvac00t = vgdDTA
014800071121     C*
014900071121     C                   clear                   TIVAW000
015000071121     C                   clear                   wBufferOut
015100100907     C                   clear                   wLinkDir
015200100907     C                   clear                   wConsOkSiNo
015300100907     C                   clear                   wConsOkSiNoDes
015400100910     C*
015500100910     C* Solo bolle originali e valide ai fini della spedizione
015600100910     C                   movel     *blanks       wEsito1           1
015700100910     C*
015800100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
015900100910     C                                                vacAAS
016000100910     C                                               :vacLNP
016100100910     C                                               :vacNRS
016200100910     C                                               :vacNSP
016300100910     C                                               :pOutLblTyp
016400100910     C                                               :pOutAnnoBO
016500100910     C                                               :pOutLineaParBO
016600100910     C                                               :pOutSerieBO
016700100910     C                                               :pOutNumSpedBO
016800100910     C                                               :pOutDcmBO
016900100910     C                                               :pOutCcaBO
017000100910     C                                               :pOutRblBO))
017100100910     C*
017200100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
017300071210     C*
017400071210     C* Inizializzo campi d wrk
017500110519     C                   EVAL      wDSP = *blanks
017600110519     C                   EVAL      wRD2 = *blanks
017700110519     C                   EVAL      wIND = *blanks
017800110519     C                   EVAL      wCAD = *blanks
017900110519     C                   EVAL      wLOD = *blanks
018000110519     C                   EVAL      wPRD = *blanks
018100071210     C*
018200071210     C* Aggancio la bolla relativa x il completamento dei dati
018300071210     C     KEYtas30      chain     titas30c
018400071210     C                   if        %found(titas30c)
018500110519     C*
018600110519     C                   eval      ta4TRC = 'D'
018700110519     C     KEYta430      chain     tita430c
018800110519     C                   if        %found(tita430c)
018900110519     C                   eval      wRD2 = '.' + ta4NOT
019000110519     C                   endif
019100071210     C*
019200071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
019300110519     C                   eval      wDSP = %editc((tasAAS*10000+tasMGS):'X')
019400110519     C                   eval      wIND = tasIND
019500110519     C                   eval      wCAD = tasCAD
019600110519     C                   eval      wLOD = tasLOD
019700110519     C                   eval      wPRD = tasPRD
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
022600120926     C                   MOVEL     *blanks       wMIC              1
022700121114     C                   MOVEL     *blanks       wGIAC             1
022800100910     C                   EVAL      wDataSts       = vacDCM
022900100907     C*
023000100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
023100100907     C                   IF        VACDCM > *zeros AND
023200100907     C                             (VACCCA = *blanks OR VACCCA = '1')
023300100907     C                   EVAL      wConsOkSiNo    = 'S'
023400110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
023500100907     C                   ELSE
023600100907     C                   EVAL      wConsOkSiNo    = 'N'
023700100907     C*
023800100907     C* A questo punto stabilisco la descrizione
023900100910     C                   SELECT
024000100910     C                   WHEN      vacCCA = '1'
024100110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
024200100910     C                   WHEN      vacCCA = '2'
024300110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
024400100907     C                   EVAL      wConsOkSiNo    = 'R'
024500100910     C                   WHEN      vacCCA = '3'
024600110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
024700110519     C                   EVAL      wConsOkSiNo    = 'R'
024800100910     C                   WHEN      vacCCA = '4'
024900110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
025000110519     C                   EVAL      wConsOkSiNo    = 'R'
025100100910     C                   WHEN      vacCCA = '5'
025200110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
025300110519     C                   EVAL      wConsOkSiNo    = 'R'
025400100910     C                   WHEN      vacCCA = '6'
025500110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
025600110519     C                   EVAL      wConsOkSiNo    = 'R'
025700100910     C                   WHEN      vacCCA = 'A'
025800110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
025900110519     C                   EVAL      wConsOkSiNo    = 'R'
026000100910     C                   WHEN      vacCCA = 'C'
026100110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
026200120926     C                   EVAL      wMIC = 'S'
026300100910     C                   WHEN      vacCCA = 'R'
026400110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
026500100910     C                   WHEN      vacCCA = 'S'
026600110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
026700110519     C                   EVAL      wConsOkSiNo    = 'R'
026800100910     C                   ENDSL
026900100907     C*
027000100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
027100100910     C                   EVAL      wDataSts       = vacDLA
027200110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
027300100907     C                   ENDIF
027400100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
027500100910     C                   EVAL      wDataSts       = vacDAG
027600110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
027700121114     C                   EVAL      wGIAC = 'S'
027800100907     C                   ENDIF
027900100910     C                   IF        vacDCM = *zeros AND
028000100910     C                             vacDLA = *zeros AND
028100100910     C                             vacDAG = *zeros AND
028200100910     C                             vacCCA = *blanks
028300100910     C                   EVAL      wDataSts       = datcor
028400100910     C*
028500100910     C* Verifico la data consegna richiesta
028600100910     C                   IF        vacDCR > datcor
028700110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
028800110519     C                                              'data ' + %editc(vacDCR:'X')
028900100910     C                   ELSE
029000110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
029100100910     C                   ENDIF
029200100910     C                   ENDIF
029300100907     C*
029400100907     C                   ENDIF
029500100907     C*
029600100907     C                   ENDSR
029700100907
029800100907
029900100907
030000100907     C     makLNK        BEGSR
030100100907     C*
030200160927     C                   EVAL      wLinkDir = 'http://vas.brt.it/vas/' +
030300100907     C                                        'sped_ricdocmit_load.hsm?docmit='+
030400100907     C                                        %trim(%char(vacRMN))             +
030500100907     C                                        '&ksu=0895741&lang=fr'
030600100907     C*
030700100907     C                   ENDSR
030800100907
030900071121
031000071121
031100071121
031200071121     C     wrifile       BEGSR
031300120926     C*
031400120926     C                   SETOFF                                       31
031500071121     C*
031600100907     C* Determino lo status della spedizione
031700100907     C                   EXSR      calSTS
031800100907     C*
031900100907     C* Costruisco il link diretto
032000110519     C***                EXSR      makLNK
032100100908     C*
032200100908     C                   SELECT
032300100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
032400071121     C                   EVAL      wBufferOut = iRow +
032500100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
032600110519     C                                  iCol+%trim(wDSP)                  +fCol+
032700110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
032800100907     C                                  iCol+%trim(vacRSD)                +fCol+
032900110519     C                                  iCol+%trim(wRD2)                  +fCol+
033000110519     C                                  iCol+%trim(wIND)                  +fCol+
033100100517     C                                  iCol+%trim(wCAD)                  +fCol+
033200100517     C                                  iCol+%trim(wLOD)                  +fCol+
033300110519     C                                  iCol+%trim(wPRD)                  +fCol+
033400100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
033500110519     C                                  iCol+%trim(vacRMA)                +fCol+
033600100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
033700110519     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol
033800100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
033900100908     C                   EVAL      wBufferOut =
034000110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
034100110519     C                              +'"'+   %trim(wDSP)                   +'";'
034200110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
034300110519     C                              +'"'+   %trim(vacRSD)                 +'";'
034400110519     C                              +'"'+   %trim(wRD2)                   +'";'
034500110519     C                              +'"'+   %trim(wIND)                   +'";'
034600110519     C                              +'"'+   %trim(wCAD)                   +'";'
034700110519     C                              +'"'+   %trim(wLOD)                   +'";'
034800110519     C                              +'"'+   %trim(wPRD)                   +'";'
034900110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
035000110519     C                              +'"'+   %trim(vacRMA)                 +'";'
035100110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
035200110519     C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
035300100908     C                   ENDSL
035400071121     C*
035500120926     C* Se nei parametri è richiesto no MIC => gestisco
035600120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
035700120926     C                   SETON                                        31
035800120926     C                   ENDIF
035900121114     C*
036000121114     C* Se nei parametri è richiesto solo GIACIENZE => gestisco
036100121114     C                   IF        %subst(prmppt:4:1) = 'S' and wGIAC <> 'S'
036200121114     C                   SETON                                        31
036300121114     C                   ENDIF
036400120926     C*
036500071121     C                   EVAL      VAWDTA = wBufferOut
036600110519     C*
036700110519     C* Restituisco solo le spedizioni "pendenti"
036800110713     C***                IF        wConsOkSiNo = 'N'
036900120926     C  N31              WRITE     TIVAW000
037000110713     C***                ENDIF
037100071121     C*
037200071121     C                   ENDSR
037300071121
037400071121
037500071121
037600991027
037700991027      /TITLE Operazioni iniziali.
037800991027     C     *inzsr        BEGSR
037900991027     C*
038000991027     C     *ENTRY        PLIST
038100991027     C                   parm                    prmppt
038200991027     C     wrkesito      parm      wrkesito      prmesito
038300100910     C*
038400100910     C* CALCOLA LA DATA CORRENTE
038500100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
038600100910     C                   eval      datcor = %dec(%date() : *ISO)
038700071210     C*
038800071213     C* TITAS30C - Parziale
038900071210     C     KEYtas30      KLIST
039000071210     C                   KFLD                    vacAAS
039100071210     C                   KFLD                    vacLNP
039200071210     C                   KFLD                    vacNRS
039300071210     C                   KFLD                    vacNSP
039400110519     C*
039500110519     C* TITA430C - Completa
039600110519     C     KEYta430      KLIST
039700110519     C                   KFLD                    vacAAS
039800110519     C                   KFLD                    vacLNP
039900110519     C                   KFLD                    vacNRS
040000110519     C                   KFLD                    vacNSP
040100110519     C                   KFLD                    ta4TRC
040200991027     C*
040300991027     C                   ENDSR
040400071121     C*
040500071121** DEC - COSTANTI
040600071121<?xml version="1.0"?>
040700071121<?mso-application progid="Excel.Sheet"?>
040800071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
040900071121xmlns:o="urn:schemas-microsoft-com:office:office"
041000071121xmlns:x="urn:schemas-microsoft-com:office:excel"
041100071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
041200071121xmlns:html="http://www.w3.org/TR/REC-html40">
041300071121</Workbook>
041400071121<Worksheet ss:Name="PAG1">
041500071121</Worksheet>
041600071121<Table border="1">
041700071121</Table>
041800071121<Row>
041900071121</Row>
042000071121<Cell><Data ss:Type="String">
042100071121<Cell><Data ss:Type="Number">
042200071121</Data></Cell>
042300071121
042400071121
042500071121
