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
023100130409     C                   EVAL      wConsOkSiNoDes = 'Goods never entrusted us'
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
031900120427     C                   movel     *blanks       parLang           2
032000130126     C                   movel     *blanks       parKSU            7
032100130126     C*
032200130126     C                   eval      parKSU  = %subst(prmppt:6:7)
032300130126     C                   if        parKSU  = *blanks
032400130126     C                   eval      parKSU  = %editc(vacCCM:'X')
032500130126     C                   endif
032600130126     C*
032700120427     C                   eval      parLang = %subst(prmppt:13:2)
032800120427     C     maiu:minu     xlate     parLang       parLang
032900100907     C*
033000130409     C                   eval      wLinkDir =
033100160927     C                                        'http://vas.brt.it/vas/'         +
033200130126     C                                        'sped_ricdocmit_load.hsm?rma='   +
033300130126     C                                        %trim(vacRMA)                    +
033400130409     C                                        '&ksu='+parKSU+'&lang='+parLang
033500130109     C*
033600130109     C                   endif
033700100907     C*
033800100907     C                   ENDSR
033900100907
034000071121
034100071121
034200071121
034300071121     C     wrifile       BEGSR
034400071121     C*
034500100907     C* Determino lo status della spedizione
034600100907     C                   EXSR      calSTS
034700100907     C*
034800100907     C* Costruisco il link diretto
034900100907     C                   EXSR      makLNK
035000100908     C*
035100120713     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
035200120713     C                   IF        %subst(prmppt:15:1) = 'P'
035300120713     C                   MOVE      vacCCM        CCM               4 0
035400120713     C                   MOVE      wKSC          KSC               4 0
035500120713     C                   IF        CCM =  8888 or  CCM =  9999
035600120713     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
035700120713     C                   EVAL      vacCCM = wKSC
035800120713     C                   ENDIF
035900120713     C                   ENDIF
036000120713     C                   ENDIF
036100120713     C*
036200100908     C                   SELECT
036300100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
036400071121     C                   EVAL      wBufferOut = iRow +
036500100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
036600100907     C                                  iCol+%trim(vacRSD)                +fCol+
036700100517     C                                  iCol+%trim(wCAD)                  +fCol+
036800100517     C                                  iCol+%trim(wLOD)                  +fCol+
036900130126     C                                  iCol+%trim(vacRMA)                +fCol+
037000130126     C***                               iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
037100100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
037200100910     C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
037300100907     C                                  iCol+%trim(wLinkDir)              +fCol
037400100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
037500100908     C                   EVAL      wBufferOut =
037600100910     C                               '"'+%trim(%editc(vacCCM:'X'))        +'";'
037700100910     C                              +'"'+%trim(vacRSD)                    +'";'
037800100910     C                              +'"'+%trim(wCAD)                      +'";'
037900100910     C                              +'"'+%trim(wLOD)                      +'";'
038000130126     C                              +'"'+%trim(vacRMA)                    +'";'
038100130126     C***                           +'"'+%trim(%editc(vacRMN:'Q'))        +'";'
038200100910     C                              +'"'+%trim(wConsOkSiNoDes)            +'";'
038300100910     C                              +'"'+%trim(%editc(wDataSts:'X'))      +'";'
038400100910     C                              +'"'+%trim(wLinkDir)                  +'"'
038500100908     C                   ENDSL
038600071121     C*
038700071121     C                   EVAL      VAWDTA = wBufferOut
038800071121     C                   WRITE     TIVAW000
038900071121     C*
039000071121     C                   ENDSR
039100071121
039200071121
039300071121
039400991027
039500991027      /TITLE Operazioni iniziali.
039600991027     C     *inzsr        BEGSR
039700991027     C*
039800991027     C     *ENTRY        PLIST
039900991027     C                   parm                    prmppt
040000991027     C     wrkesito      parm      wrkesito      prmesito
040100100910     C*
040200100910     C* CALCOLA LA DATA CORRENTE
040300100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
040400100910     C                   eval      datcor = %dec(%date() : *ISO)
040500071210     C*
040600071213     C* TITAS30C - Parziale
040700071210     C     KEYtas30      KLIST
040800071210     C                   KFLD                    vacAAS
040900071210     C                   KFLD                    vacLNP
041000071210     C                   KFLD                    vacNRS
041100071210     C                   KFLD                    vacNSP
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
