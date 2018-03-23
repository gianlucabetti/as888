000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
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
005200120713     D wKSC            s              7  0 inz(*zeros)
005300071211     D*
005400100910
005500100910
005600100910     D*------------------
005700100910     D* LINKING A DEFINIZIONI ESTERNE
005800100910     D*------------------
005900100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006000100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006100100910
006200120427     D*-------------------
006300120427     D* COSTANTI
006400120427     D*-------------------
006500120427     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006600120427     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006700120427
006800030924
006900030325
007000030924     C* MAIN/
007100071121     C*
007200071121     C* Verifico se già presenti dati nel file d output
007300071121     C                   EXSR      chkRECFILMBR
007400071121     C*
007500071121     C* Effettuo elaborazione
007600071121     C                   EXSR      traduci
007700071121     C*
007800921023     C                   SETON                                        LR
007900991027
008000071121
008100071121
008200071121     C     chkRECFILMBR  BEGSR
008300071121     C*
008400071121     C                   READ      TIVAWWWT                               55
008500071121     C*
008600071121     C                   ENDSR
008700071121
008800071121
008900030924
009000991027     C     traduci       BEGSR
009100071121     C*
009200071121     C* Head iniziali della struttura HTML
009300100908     C                   IF        %subst(prmppt:2:1) = 'H'
009400071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
009500071210     C   55              WRITE     TIVAW000
009600100908     C                   ENDIF
009700071121     C*
009800071121     C* Inizializzazone formato e variabili x output
009900071121     C                   clear                   TIVAW000
010000071121     C                   clear                   wBufferOut
010100100907     C                   clear                   wLinkDir
010200100907     C                   clear                   wConsOkSiNo
010300100907     C                   clear                   wConsOkSiNoDes
010400071121     C*
010500071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010600071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
010700100908     C*
010800100908     C                   SELECT
010900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
011000071121     C                   EVAL      wBufferOut = iThead + iRow
011100100910     C                             +iColBold+ 'CUSTOMER CODE'          +fColBold
011200100907     C                             +iColBold+ 'DESTINATION NAME'       +fColBold
011300100907     C                             +iColBold+ 'ZIP CODE'               +fColBold
011400100907     C                             +iColBold+ 'CITY'                   +fColBold
011500100907     C                             +iColBold+ 'SENDER REF. ID'         +fColBold
011600100907     C                             +iColBold+ 'STATUS'                 +fColBold
011700100910     C                             +iColBold+ 'STATUS DATE'            +fColBold
011800100907     C                             +iColBold+ 'LINK'                   +fColBold
011900100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
012000100908     C                   EVAL      wBufferOut =
012100100910     C                              '"'+'CUSTOMER CODE'          +'";'
012200100910     C                             +'"'+'DESTINATION NAME'       +'";'
012300100908     C                             +'"'+'ZIP CODE'               +'";'
012400100908     C                             +'"'+'CITY'                   +'";'
012500100908     C                             +'"'+'SENDER REF. ID'         +'";'
012600100908     C                             +'"'+'STATUS'                 +'";'
012700100910     C                             +'"'+'STATUS DATE'            +'";'
012800100908     C                             +'"'+'LINK'                   +'"'
012900100908     C                   ENDSL
013000100908     C*
013100071121     C                   EVAL      VAWDTA = wBufferOut
013200071121     C                   WRITE     TIVAW000
013300071121     C                   ENDIF
013400991027     C*
013500080924     C                   READ      TIVGDTMP
013600060119     C*
013700080924     C                   DOW       not %eof(TIVGDTMP)
013800080924     C*
013900080924     C                   eval      fnvac00t = vgdDTA
014000071121     C*
014100071121     C                   clear                   TIVAW000
014200071121     C                   clear                   wBufferOut
014300100907     C                   clear                   wLinkDir
014400100907     C                   clear                   wConsOkSiNo
014500100907     C                   clear                   wConsOkSiNoDes
014600100910     C*
014700120713     C* Solo bolle originali (oppure se diversamente richiesto nei parametri)
014800120713     C* e valide ai fini della spedizione
014900100910     C                   movel     *blanks       wEsito1           1
015000100910     C*
015100100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
015200100910     C                                                vacAAS
015300100910     C                                               :vacLNP
015400100910     C                                               :vacNRS
015500100910     C                                               :vacNSP
015600100910     C                                               :pOutLblTyp
015700100910     C                                               :pOutAnnoBO
015800100910     C                                               :pOutLineaParBO
015900100910     C                                               :pOutSerieBO
016000100910     C                                               :pOutNumSpedBO
016100100910     C                                               :pOutDcmBO
016200100910     C                                               :pOutCcaBO
016300100910     C                                               :pOutRblBO))
016400100910     C*
016500120713     C                   if         pOutRblBO ='N' AND
016600120713     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
016700071210     C*
016800071210     C* Inizializzo campi d wrk
016900100517     C                   EVAL      wCAD = *blanks
017000071213     C                   EVAL      wLOD = *blanks
017100120713     C                   EVAL      wKSC = *zeros
017200071210     C*
017300071210     C* Aggancio la bolla relativa x il completamento dei dati
017400071210     C     KEYtas30      chain     titas30c
017500071210     C                   if        %found(titas30c)
017600071210     C*
017700071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
017800100517     C                   EVAL      wCAD = tasCAD
017900071213     C                   EVAL      wLOD = tasLOD
018000120713     C                   EVAL      wKSC = tasKSC
018100071213     C*
018200071213     C                   endif
018300071121     C*
018400071121     C* Compondo la stringa dati output relativa al record corrente
018500071121     C                   EXSR      wrifile
018600100910     C*
018700100910     C                   endif
018800040719     C*
018900040719     C* In ogni caso storicizzo e cancello l'input sempre
019000080924     C                   DELETE    TIVGD000
019100040719     C*
019200040719     C* Proseguo con la lettura dell'input
019300080924     C                   READ      TIVGDTMP
019400060119     C*
019500030325     C                   ENDDO
019600071121     C*
019700071121     C* Head finali della struttura HTML
019800071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
019900071210     C***                WRITE     TIVAW000
020000991027     C*
020100030325     C                   EVAL      wrkesito = '0'
020200991027     C*
020300910830     C                   ENDSR
020400100907
020500100907
020600100907
020700100907     C     calSTS        BEGSR
020800100910     C*
020900100910     C                   EVAL      wDataSts       = vacDCM
021000100907     C*
021100100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
021200100907     C                   IF        VACDCM > *zeros AND
021300100907     C                             (VACCCA = *blanks OR VACCCA = '1')
021400100907     C                   EVAL      wConsOkSiNo    = 'S'
021500100907     C                   EVAL      wConsOkSiNoDes = 'Delivered'
021600100907     C                   ELSE
021700100907     C                   EVAL      wConsOkSiNo    = 'N'
021800100907     C*
021900100907     C* A questo punto stabilisco la descrizione
022000100910     C                   SELECT
022100100910     C                   WHEN      vacCCA = '1'
022200100907     C                   EVAL      wConsOkSiNoDes = 'Diverted'
022300100910     C                   WHEN      vacCCA = '2'
022400100907     C                   EVAL      wConsOkSiNoDes = 'Returned'
022500100907     C                   EVAL      wConsOkSiNo    = 'R'
022600100910     C                   WHEN      vacCCA = '3'
022700100907     C                   EVAL      wConsOkSiNoDes = 'Goods Stolen'
022800100910     C                   WHEN      vacCCA = '4'
022900100907     C                   EVAL      wConsOkSiNoDes = 'Payment Stolen'
023000100910     C                   WHEN      vacCCA = '5'
023100100907     C                   EVAL      wConsOkSiNoDes = 'Closed on hold report'
023200100910     C                   WHEN      vacCCA = '6'
023300100907     C                   EVAL      wConsOkSiNoDes = 'Damage and Returned'
023400120427     C                   WHEN      vacCCA = '7'
023500120427     C                   EVAL      wConsOkSiNoDes = 'Goods never entrusted us'
023600100910     C                   WHEN      vacCCA = 'A'
023700100907     C                   EVAL      wConsOkSiNoDes = 'New on hold report'
023800100910     C                   WHEN      vacCCA = 'C'
023900100907     C                   EVAL      wConsOkSiNoDes = 'In delivery'
024000100910     C                   WHEN      vacCCA = 'R'
024100100907     C                   EVAL      wConsOkSiNoDes = 'Reset shipment'
024200100910     C                   WHEN      vacCCA = 'S'
024300100907     C                   EVAL      wConsOkSiNoDes = 'Destroyed Goods'
024400100910     C                   ENDSL
024500100907     C*
024600100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
024700100910     C                   EVAL      wDataSts       = vacDLA
024800120427     C                   EVAL      wConsOkSiNoDes = 'Delivery attempt failed' +
024900120427     C                                              ' - Left Notice'
025000100907     C                   ENDIF
025100100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
025200100910     C                   EVAL      wDataSts       = vacDAG
025300100907     C                   EVAL      wConsOkSiNoDes = 'On-Hold'
025400100907     C                   ENDIF
025500100910     C                   IF        vacDCM = *zeros AND
025600100910     C                             vacDLA = *zeros AND
025700100910     C                             vacDAG = *zeros AND
025800100910     C                             vacCCA = *blanks
025900100910     C                   EVAL      wDataSts       = datcor
026000100910     C*
026100100910     C* Verifico la data consegna richiesta
026200100910     C                   IF        vacDCR > datcor
026300100910     C                   EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
026400100910     C                                              'on ' + %editc(vacDCR:'X')
026500100910     C                   ELSE
026600100910     C                   EVAL      wConsOkSiNoDes = 'In progress'
026700100910     C                   ENDIF
026800100910     C                   ENDIF
026900100907     C*
027000100907     C                   ENDIF
027100120427     C*
027200120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
027300120427     C                   IF        vacCCA = '1'  AND
027400120427     C                             wDataSts = *zeros
027500120427     C                   EXSR      REPDIR
027600120427     C                   ENDIF
027700100907     C*
027800100907     C                   ENDSR
027900120427
028000120427
028100120427
028200120427     C*------------------------------------------------------------------------*
028300120427     C* REPDIR - Reperimento data DIROTTAMENTO
028400120427     C*------------------------------------------------------------------------*
028500120427     C     REPDIR        BEGSR
028600120427     C*
028700120427     C                   movel     *blanks       wEsito1           1
028800120427     C*
028900120427     C* Chiamata metodo GetLblTyp
029000120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
029100120427     C                                                vacAAS
029200120427     C                                               :vacLNP
029300120427     C                                               :vacNRS
029400120427     C                                               :vacNSP
029500120427     C                                               :pOutLblTyp
029600120427     C                                               :pOutAnnoBO
029700120427     C                                               :pOutLineaParBO
029800120427     C                                               :pOutSerieBO
029900120427     C                                               :pOutNumSpedBO
030000120427     C                                               :pOutDcmBO
030100120427     C                                               :pOutCcaBO
030200120427     C                                               :pOutRblBO))
030300120427     C*
030400120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
030500120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
030600120427     C                   eval      wDataSts = pOutDcmBO
030700120427     C                   endif
030800120427     C*
030900120427     C                   ENDSR
031000120427     C*------------------------------------------------------------------------*
031100100907
031200100907
031300100907
031400100907     C     makLNK        BEGSR
031500130109     C*
031600130109     C* Se richiesta inclusione link T&T
031700130109     C                   if        %subst(prmppt:6:7) <> *blanks
031800120427     C*
031900120427     C* Porto in minuscolo il parametro relativo alla lingua del link
032000120427     C                   movel     *blanks       parLang           2
032100120427     C                   eval      parLang = %subst(prmppt:13:2)
032200120427     C     maiu:minu     xlate     parLang       parLang
032300100907     C*
032400160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
032500100907     C                                        'sped_ricdocmit_load.hsm?docmit='+
032600100907     C                                        %trim(%char(vacRMN))             +
032700120427     C                                        '&ksu='+%subst(prmppt:6:7)+
032800120427     C                                        '&lang='+parLang
032900130109     C*
033000130109     C                   endif
033100100907     C*
033200100907     C                   ENDSR
033300100907
033400071121
033500071121
033600071121
033700071121     C     wrifile       BEGSR
033800071121     C*
033900100907     C* Determino lo status della spedizione
034000100907     C                   EXSR      calSTS
034100100907     C*
034200100907     C* Costruisco il link diretto
034300100907     C                   EXSR      makLNK
034400100908     C*
034500120713     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
034600120713     C                   IF        %subst(prmppt:15:1) = 'P'
034700120713     C                   MOVE      vacCCM        CCM               4 0
034800120713     C                   MOVE      wKSC          KSC               4 0
034900120713     C                   IF        CCM =  8888 or  CCM =  9999
035000120713     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
035100120713     C                   EVAL      vacCCM = wKSC
035200120713     C                   ENDIF
035300120713     C                   ENDIF
035400120713     C                   ENDIF
035500120713     C*
035600100908     C                   SELECT
035700100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
035800071121     C                   EVAL      wBufferOut = iRow +
035900100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
036000100907     C                                  iCol+%trim(vacRSD)                +fCol+
036100100517     C                                  iCol+%trim(wCAD)                  +fCol+
036200100517     C                                  iCol+%trim(wLOD)                  +fCol+
036300100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
036400100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
036500100910     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
036600100907     C                                  iCol+%trim(wLinkDir)              +fCol
036700100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
036800100908     C                   EVAL      wBufferOut =
036900100910     C                               '"'+%trim(%editc(vacCCM:'X'))        +'";'
037000100910     C                              +'"'+%trim(vacRSD)                    +'";'
037100100910     C                              +'"'+%trim(wCAD)                      +'";'
037200100910     C                              +'"'+%trim(wLOD)                      +'";'
037300100910     C                              +'"'+%trim(%editc(vacRMN:'Q'))        +'";'
037400100910     C                              +'"'+%trim(wConsOkSiNoDes)            +'";'
037500100910     C                              +'"'+%trim(%editc(wDataSts:'X'))      +'";'
037600100910     C                              +'"'+%trim(wLinkDir)                  +'"'
037700100908     C                   ENDSL
037800071121     C*
037900071121     C                   EVAL      VAWDTA = wBufferOut
038000071121     C                   WRITE     TIVAW000
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
040400991027     C*
040500991027     C                   ENDSR
040600071121     C*
040700071121** DEC - COSTANTI
040800071121<?xml version="1.0"?>
040900071121<?mso-application progid="Excel.Sheet"?>
041000071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
041100071121xmlns:o="urn:schemas-microsoft-com:office:office"
041200071121xmlns:x="urn:schemas-microsoft-com:office:excel"
041300071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
041400071121xmlns:html="http://www.w3.org/TR/REC-html40">
041500071121</Workbook>
041600071121<Worksheet ss:Name="PAG1">
041700071121</Worksheet>
041800071121<Table border="1">
041900071121</Table>
042000071121<Row>
042100071121</Row>
042200071121<Cell><Data ss:Type="String">
042300071121<Cell><Data ss:Type="Number">
042400071121</Data></Cell>
042500071121
042600071121
042700071121
