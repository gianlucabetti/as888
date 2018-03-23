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
004800141128     D wOraSts         S              4  0 inz(*zeros)
004900100907     D wConsOkSiNo     S              1    inz(*blanks)
005000100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005100100907     D wLinkDir        S            300    inz(*blanks)
005200110519     D wDSP            s             10    inz(*blanks)
005300110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
005400110519     D wIND            s                   inz(*blanks) like(tasIND)
005500110519     D wCAD            s                   inz(*blanks) like(tasIND)
005600110519     D wLOD            s                   inz(*blanks) like(tasLOD)
005700110519     D wPRD            s                   inz(*blanks) like(tasPRD)
005800071211     D*
005900100910
006000100910
006100100910     D*------------------
006200100910     D* LINKING A DEFINIZIONI ESTERNE
006300100910     D*------------------
006400100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006500100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006600100910
006700030924
006800030325
006900030924     C* MAIN/
007000071121     C*
007100071121     C* Verifico se già presenti dati nel file d output
007200071121     C                   EXSR      chkRECFILMBR
007300071121     C*
007400071121     C* Effettuo elaborazione
007500071121     C                   EXSR      traduci
007600071121     C*
007700921023     C                   SETON                                        LR
007800991027
007900071121
008000071121
008100071121     C     chkRECFILMBR  BEGSR
008200071121     C*
008300071121     C                   READ      TIVAWWWT                               55
008400071121     C*
008500071121     C                   ENDSR
008600071121
008700071121
008800030924
008900991027     C     traduci       BEGSR
009000071121     C*
009100071121     C* Head iniziali della struttura HTML
009200100908     C                   IF        %subst(prmppt:2:1) = 'H'
009300071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
009400071210     C   55              WRITE     TIVAW000
009500100908     C                   ENDIF
009600071121     C*
009700071121     C* Inizializzazone formato e variabili x output
009800071121     C                   clear                   TIVAW000
009900071121     C                   clear                   wBufferOut
010000100907     C                   clear                   wLinkDir
010100100907     C                   clear                   wConsOkSiNo
010200100907     C                   clear                   wConsOkSiNoDes
010300071121     C*
010400071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010500071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010600100908     C*
010700100908     C                   SELECT
010800100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
010900071121     C                   EVAL      wBufferOut = iThead + iRow
011000110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
011100110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
011200110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
011300110519     C                             +iColBold+ 'RAG. SOC. DESTINATARIO' +fColBold
011400110519     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
011500110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
011600110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
011700110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
011800110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
011900110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
012000110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
012100131015     C                             +iColBold+ 'TOT. COLLI'             +fColBold
012200110519     C                             +iColBold+ 'STATO'                  +fColBold
012300110519     C                             +iColBold+ 'DATA STATO'             +fColBold
012400141128     C                             +iColBold+ 'ORA STATO'              +fColBold
012500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
012600100908     C                   EVAL      wBufferOut =
012700110519     C                              '"'+      'CODICE CLIENTE'         +'";'
012800110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
012900110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
013000110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
013100110519     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
013200110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
013300110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
013400110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
013500110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
013600110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
013700110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
013800131015     C                             +'"'+      'TOT. COLLI'             +'";'
013900110519     C                             +'"'+      'STATO'                  +'";'
014000110519     C                             +'"'+      'DATA STATO'             +'";'
014100141128     C                             +'"'+      'ORA STATO'              +'";'
014200100908     C                   ENDSL
014300100908     C*
014400071121     C                   EVAL      VAWDTA = wBufferOut
014500071121     C                   WRITE     TIVAW000
014600071121     C                   ENDIF
014700991027     C*
014800080924     C                   READ      TIVGDTMP
014900060119     C*
015000080924     C                   DOW       not %eof(TIVGDTMP)
015100080924     C*
015200080924     C                   eval      fnvac00t = vgdDTA
015300071121     C*
015400071121     C                   clear                   TIVAW000
015500071121     C                   clear                   wBufferOut
015600100907     C                   clear                   wLinkDir
015700100907     C                   clear                   wConsOkSiNo
015800100907     C                   clear                   wConsOkSiNoDes
015900100910     C*
016000100910     C* Solo bolle originali e valide ai fini della spedizione
016100100910     C                   movel     *blanks       wEsito1           1
016200100910     C*
016300100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
016400100910     C                                                vacAAS
016500100910     C                                               :vacLNP
016600100910     C                                               :vacNRS
016700100910     C                                               :vacNSP
016800100910     C                                               :pOutLblTyp
016900100910     C                                               :pOutAnnoBO
017000100910     C                                               :pOutLineaParBO
017100100910     C                                               :pOutSerieBO
017200100910     C                                               :pOutNumSpedBO
017300100910     C                                               :pOutDcmBO
017400100910     C                                               :pOutCcaBO
017500100910     C                                               :pOutRblBO))
017600100910     C*
017700100910     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
017800071210     C*
017900071210     C* Inizializzo campi d wrk
018000110519     C                   EVAL      wDSP = *blanks
018100110519     C                   EVAL      wRD2 = *blanks
018200110519     C                   EVAL      wIND = *blanks
018300110519     C                   EVAL      wCAD = *blanks
018400110519     C                   EVAL      wLOD = *blanks
018500110519     C                   EVAL      wPRD = *blanks
018600071210     C*
018700071210     C* Aggancio la bolla relativa x il completamento dei dati
018800071210     C     KEYtas30      chain     titas30c
018900071210     C                   if        %found(titas30c)
019000110519     C*
019100110519     C                   eval      ta4TRC = 'D'
019200110519     C     KEYta430      chain     tita430c
019300110519     C                   if        %found(tita430c)
019400110519     C                   eval      wRD2 = '.' + ta4NOT
019500110519     C                   endif
019600071210     C*
019700071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
019800110519     C                   eval      wDSP = %editc((tasAAS*10000+tasMGS):'X')
019900110519     C                   eval      wIND = tasIND
020000110519     C                   eval      wCAD = tasCAD
020100110519     C                   eval      wLOD = tasLOD
020200110519     C                   eval      wPRD = tasPRD
020300071213     C*
020400071213     C                   endif
020500071121     C*
020600071121     C* Compondo la stringa dati output relativa al record corrente
020700071121     C                   EXSR      wrifile
020800100910     C*
020900100910     C                   endif
021000040719     C*
021100040719     C* In ogni caso storicizzo e cancello l'input sempre
021200080924     C                   DELETE    TIVGD000
021300040719     C*
021400040719     C* Proseguo con la lettura dell'input
021500080924     C                   READ      TIVGDTMP
021600060119     C*
021700030325     C                   ENDDO
021800071121     C*
021900071121     C* Head finali della struttura HTML
022000071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
022100071210     C***                WRITE     TIVAW000
022200991027     C*
022300030325     C                   EVAL      wrkesito = '0'
022400991027     C*
022500910830     C                   ENDSR
022600100907
022700100907
022800100907
022900100907     C     calSTS        BEGSR
023000100910     C*
023100120926     C                   MOVEL     *blanks       wMIC              1
023200121114     C                   MOVEL     *blanks       wGIAC             1
023300100910     C                   EVAL      wDataSts       = vacDCM
023400141128     C                   EVAL      wOraSts        = vacHMC
023500100907     C*
023600100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
023700100907     C                   IF        VACDCM > *zeros AND
023800100907     C                             (VACCCA = *blanks OR VACCCA = '1')
023900100907     C                   EVAL      wConsOkSiNo    = 'S'
024000110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
024100100907     C                   ELSE
024200100907     C                   EVAL      wConsOkSiNo    = 'N'
024300100907     C*
024400100907     C* A questo punto stabilisco la descrizione
024500100910     C                   SELECT
024600100910     C                   WHEN      vacCCA = '1'
024700110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
024800100910     C                   WHEN      vacCCA = '2'
024900110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
025000100907     C                   EVAL      wConsOkSiNo    = 'R'
025100100910     C                   WHEN      vacCCA = '3'
025200110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
025300110519     C                   EVAL      wConsOkSiNo    = 'R'
025400100910     C                   WHEN      vacCCA = '4'
025500110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
025600110519     C                   EVAL      wConsOkSiNo    = 'R'
025700100910     C                   WHEN      vacCCA = '5'
025800110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
025900110519     C                   EVAL      wConsOkSiNo    = 'R'
026000100910     C                   WHEN      vacCCA = '6'
026100110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
026200110519     C                   EVAL      wConsOkSiNo    = 'R'
026300100910     C                   WHEN      vacCCA = 'A'
026400110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
026500110519     C                   EVAL      wConsOkSiNo    = 'R'
026600100910     C                   WHEN      vacCCA = 'C'
026700110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
026800120926     C                   EVAL      wMIC = 'S'
026900100910     C                   WHEN      vacCCA = 'R'
027000110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
027100100910     C                   WHEN      vacCCA = 'S'
027200110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
027300110519     C                   EVAL      wConsOkSiNo    = 'R'
027400100910     C                   ENDSL
027500100907     C*
027600100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
027700100910     C                   EVAL      wDataSts       = vacDLA
027800110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
027900100907     C                   ENDIF
028000100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
028100100910     C                   EVAL      wDataSts       = vacDAG
028200110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
028300121114     C                   EVAL      wGIAC = 'S'
028400100907     C                   ENDIF
028500100910     C                   IF        vacDCM = *zeros AND
028600100910     C                             vacDLA = *zeros AND
028700100910     C                             vacDAG = *zeros AND
028800100910     C                             vacCCA = *blanks
028900100910     C                   EVAL      wDataSts       = datcor
029000100910     C*
029100100910     C* Verifico la data consegna richiesta
029200100910     C                   IF        vacDCR > datcor
029300110519     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
029400110519     C                                              'data ' + %editc(vacDCR:'X')
029500100910     C                   ELSE
029600110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
029700100910     C                   ENDIF
029800100910     C                   ENDIF
029900100907     C*
030000100907     C                   ENDIF
030100100907     C*
030200100907     C                   ENDSR
030300100907
030400100907
030500100907
030600100907     C     makLNK        BEGSR
030700100907     C*
030800160927     C                   EVAL      wLinkDir = 'http://vas.brt.it/vas/' +
030900100907     C                                        'sped_ricdocmit_load.hsm?docmit='+
031000100907     C                                        %trim(%char(vacRMN))             +
031100100907     C                                        '&ksu=0895741&lang=fr'
031200100907     C*
031300100907     C                   ENDSR
031400100907
031500071121
031600071121
031700071121
031800071121     C     wrifile       BEGSR
031900120926     C*
032000120926     C                   SETOFF                                       31
032100071121     C*
032200100907     C* Determino lo status della spedizione
032300100907     C                   EXSR      calSTS
032400100907     C*
032500100907     C* Costruisco il link diretto
032600110519     C***                EXSR      makLNK
032700100908     C*
032800100908     C                   SELECT
032900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
033000071121     C                   EVAL      wBufferOut = iRow +
033100100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
033200110519     C                                  iCol+%trim(wDSP)                  +fCol+
033300110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
033400100907     C                                  iCol+%trim(vacRSD)                +fCol+
033500110519     C                                  iCol+%trim(wRD2)                  +fCol+
033600110519     C                                  iCol+%trim(wIND)                  +fCol+
033700100517     C                                  iCol+%trim(wCAD)                  +fCol+
033800100517     C                                  iCol+%trim(wLOD)                  +fCol+
033900110519     C                                  iCol+%trim(wPRD)                  +fCol+
034000100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
034100110519     C                                  iCol+%trim(vacRMA)                +fCol+
034200131015     C                                  iCol+%trim(%editc(vacNCL:'X'))    +fCol+
034300100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
034400141128     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
034500141128     C                                  iCol+%trim(%editc(wOraSts:'X'))   +fCol
034600100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
034700100908     C                   EVAL      wBufferOut =
034800110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
034900110519     C                              +'"'+   %trim(wDSP)                   +'";'
035000110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
035100110519     C                              +'"'+   %trim(vacRSD)                 +'";'
035200110519     C                              +'"'+   %trim(wRD2)                   +'";'
035300110519     C                              +'"'+   %trim(wIND)                   +'";'
035400110519     C                              +'"'+   %trim(wCAD)                   +'";'
035500110519     C                              +'"'+   %trim(wLOD)                   +'";'
035600110519     C                              +'"'+   %trim(wPRD)                   +'";'
035700110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
035800110519     C                              +'"'+   %trim(vacRMA)                 +'";'
035900131015     C                              +'"'+   %trim(%editc(vacNCL:'X'))     +'";'
036000110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
036100110519     C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
036200141128     C                              +'"'+   %trim(%editc(wOraSts:'X'))    +'";'
036300100908     C                   ENDSL
036400071121     C*
036500120926     C* Se nei parametri è richiesto no MIC => gestisco
036600120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
036700120926     C                   SETON                                        31
036800120926     C                   ENDIF
036900121114     C*
037000121114     C* Se nei parametri è richiesto solo GIACIENZE => gestisco
037100121114     C                   IF        %subst(prmppt:4:1) = 'S' and wGIAC <> 'S'
037200121114     C                   SETON                                        31
037300121114     C                   ENDIF
037400120926     C*
037500071121     C                   EVAL      VAWDTA = wBufferOut
037600110519     C*
037700110519     C* Restituisco solo le spedizioni "pendenti"
037800110713     C***                IF        wConsOkSiNo = 'N'
037900120926     C  N31              WRITE     TIVAW000
038000110713     C***                ENDIF
038100071121     C*
038200071121     C                   ENDSR
038300071121
038400071121
038500071121
038600991027
038700991027      /TITLE Operazioni iniziali.
038800991027     C     *inzsr        BEGSR
038900991027     C*
039000991027     C     *ENTRY        PLIST
039100991027     C                   parm                    prmppt
039200991027     C     wrkesito      parm      wrkesito      prmesito
039300100910     C*
039400100910     C* CALCOLA LA DATA CORRENTE
039500100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
039600100910     C                   eval      datcor = %dec(%date() : *ISO)
039700071210     C*
039800071213     C* TITAS30C - Parziale
039900071210     C     KEYtas30      KLIST
040000071210     C                   KFLD                    vacAAS
040100071210     C                   KFLD                    vacLNP
040200071210     C                   KFLD                    vacNRS
040300071210     C                   KFLD                    vacNSP
040400110519     C*
040500110519     C* TITA430C - Completa
040600110519     C     KEYta430      KLIST
040700110519     C                   KFLD                    vacAAS
040800110519     C                   KFLD                    vacLNP
040900110519     C                   KFLD                    vacNRS
041000110519     C                   KFLD                    vacNSP
041100110519     C                   KFLD                    ta4TRC
041200991027     C*
041300991027     C                   ENDSR
041400071121     C*
041500071121** DEC - COSTANTI
041600071121<?xml version="1.0"?>
041700071121<?mso-application progid="Excel.Sheet"?>
041800071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
041900071121xmlns:o="urn:schemas-microsoft-com:office:office"
042000071121xmlns:x="urn:schemas-microsoft-com:office:excel"
042100071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
042200071121xmlns:html="http://www.w3.org/TR/REC-html40">
042300071121</Workbook>
042400071121<Worksheet ss:Name="PAG1">
042500071121</Worksheet>
042600071121<Table border="1">
042700071121</Table>
042800071121<Row>
042900071121</Row>
043000071121<Cell><Data ss:Type="String">
043100071121<Cell><Data ss:Type="Number">
043200071121</Data></Cell>
043300071121
043400071121
043500071121
