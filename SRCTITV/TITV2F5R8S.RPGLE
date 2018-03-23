000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200140127     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     FTIVGDTMP  UF   E             DISK
000500040719     FTIVAWWWT  UF A E             DISK
000600100517     FTITAS30C  IF   E           K DISK
000700100517     F                                     rename(titas000:titas0)
000800100517     F                                     rename(titas010:titas1)
000900100517     F                                     rename(titasP00:titasP)
001000110519     FTITA430C  IF   E           K DISK
001100121207     FTITAA30C  IF   E           K DISK
001200130913     FTIGCP51L  IF   E           K DISK
001300140308     FFIAR531C  IF   E           K DISK
001400140308     FTIGNP01L  IF   E           K DISK
001500140311     FTABEL00F  IF   E           K DISK
001600030924
001700080924     D fnvac00t      e ds
001800140308     D dar5gen       e ds
001900140308     D dar5emd       e ds
002000140311     D ds2A          e ds                  inz
002100071121
002200071121     D*------------
002300071121     D* DECODIFICHE COSTANTI
002400071121     D*------------
002500071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
002600071121
002700071121
002800071121     D*------------
002900071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
003000071121     D*------------
003100071121     D iHtml           s             10    inz('<html>')
003200071121     D fHtml           s             10    inz('</html>')
003300071121     D iBody           s             10    inz('<body>')
003400071121     D fBody           s             10    inz('</body>')
003500071121     D iTable          s             20    inz('<table border="1">')
003600071121     D fTable          s             10    inz('</table>')
003700071121     D iThead          s             10    inz('<thead>')
003800071121     D fThead          s             10    inz('</thead>')
003900071121     D iRow            s              5    inz('<tr>')
004000071121     D fRow            s              5    inz('</tr>')
004100071121     D iCol            s              5    inz('<td>')
004200071121     D fCol            s              5    inz('</td>')
004300071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
004400071121     D fColBold        s              5    inz('</th>')
004500071122     D cApi            s              1    inz('''')
004600071121
004700071121
004800071121     D*------------
004900071121     D* VARIABILI D WRK
005000071121     D*------------
005100030924     D prmppt          S             50
005200030924     D prmesito        S              1
005300030924     D wrkesito        S                   like(prmesito)
005400071122     D wBufferOut      s          10000    varying inz
005500100910     D wDataSts        S              8  0 inz(*zeros)
005600140308     D wDataStsAlfa    S             10    inz(*blanks)
005700100907     D wConsOkSiNo     S              1    inz(*blanks)
005800100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005900140613     D wConsOkSiNoCod  S              3    inz(*blanks)
006000100907     D wLinkDir        S            300    inz(*blanks)
006100110519     D wDSP            s             10    inz(*blanks)
006200110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
006300110519     D wIND            s                   inz(*blanks) like(tasIND)
006400110519     D wCAD            s                   inz(*blanks) like(tasIND)
006500110519     D wLOD            s                   inz(*blanks) like(tasLOD)
006600110519     D wPRD            s                   inz(*blanks) like(tasPRD)
006700140127     D wNCD            s                   inz(*blanks) like(tasNCD)
006800140127     D wNCA            s                   inz(*blanks) like(tasNCA)
006900140127     D wSPEDBRT        s             12  0 inz
007000121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
007100121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
007200121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
007300121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
007400121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
007500140129     D wFIRMATAR       s                   inz(*blanks) like(ta4NOT)
007600130426     D wKSC            s              7  0 inz(*zeros)
007700140308     D wNoteGiac       s            300    inz(*blanks)
007800140311     D wGCPCMC         s                   inz(*blanks) like(gcpCMC)
007900140311     D wGiacCau        s                   inz(*blanks) like(§2ADES)
008000071211     D*
008100100910
008200100910
008300100910     D*------------------
008400100910     D* LINKING A DEFINIZIONI ESTERNE
008500100910     D*------------------
008600100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008700100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008800140308     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008900140308     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
009000130426
009100130426
009200130426     D*-------------------
009300130426     D* COSTANTI
009400130426     D*-------------------
009500130426     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009600130426     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009700100910
009800030924
009900030325
010000030924     C* MAIN/
010100140308     C*
010200071121     C* Verifico se già presenti dati nel file d output
010300071121     C                   EXSR      chkRECFILMBR
010400071121     C*
010500071121     C* Effettuo elaborazione
010600071121     C                   EXSR      traduci
010700071121     C*
010800921023     C                   SETON                                        LR
010900991027
011000071121
011100071121
011200071121     C     chkRECFILMBR  BEGSR
011300071121     C*
011400071121     C                   READ      TIVAWWWT                               55
011500071121     C*
011600071121     C                   ENDSR
011700071121
011800071121
011900030924
012000991027     C     traduci       BEGSR
012100071121     C*
012200071121     C* Head iniziali della struttura HTML
012300100908     C                   IF        %subst(prmppt:2:1) = 'H'
012400071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
012500071210     C   55              WRITE     TIVAW000
012600100908     C                   ENDIF
012700071121     C*
012800071121     C* Inizializzazone formato e variabili x output
012900071121     C                   clear                   TIVAW000
013000071121     C                   clear                   wBufferOut
013100100907     C                   clear                   wLinkDir
013200100907     C                   clear                   wConsOkSiNo
013300100907     C                   clear                   wConsOkSiNoDes
013400071121     C*
013500071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013600071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
013700100908     C*
013800100908     C                   SELECT
013900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
014000071121     C                   EVAL      wBufferOut = iThead + iRow
014100141002     C                             +iColBold+ 'LDV BRT'                +fColBold
014200141002     C                             +iColBold+ 'SHIPMENT DATE'          +fColBold
014300141002     C                             +iColBold+ 'CONSIGNEE NAME'         +fColBold
014400141002     C                             +iColBold+ 'CONSIGNEE ADDRESS'      +fColBold
014500141002     C                             +iColBold+ 'CONSIGNEE ZIP CODE'     +fColBold
014600141002     C                             +iColBold+ 'CONSIGNEE CITY'         +fColBold
014700141002     C                             +iColBold+ 'CONSIGNEE PROVINCE CODE'+fColBold
014800141002     C                             +iColBold+ 'SHIPMENT NUM   REF.ID'  +fColBold
014900141002     C                             +iColBold+ 'SHIPMENT ALPHA REF.ID'  +fColBold
015000141002     C                             +iColBold+ 'STATUS DATE'            +fColBold
015100141002     C                             +iColBold+ 'STATUS'                 +fColBold
015200141002     C                             +iColBold+ 'SIGNER'                 +fColBold
015300100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
015400100908     C                   EVAL      wBufferOut =
015500141002     C                              '"'+      'LDV BRT'                +'";'
015600141002     C                             +'"'+      'SHIPMENT DATE'          +'";'
015700141002     C                             +'"'+      'CONSIGNEE NAME'         +'";'
015800141002     C                             +'"'+      'CONSIGNEE ADDRESS'      +'";'
015900141002     C                             +'"'+      'CONSIGNEE ZIP CODE'     +'";'
016000141002     C                             +'"'+      'CONSIGNEE CITY'         +'";'
016100141002     C                             +'"'+      'CONSIGNEE PROVINCE CODE'+'";'
016200141002     C                             +'"'+      'SHIPMENT NUM   REF.ID'  +'";'
016300141002     C                             +'"'+      'SHIPMENT ALPHA REF.ID'  +'";'
016400141002     C                             +'"'+      'STATUS DATE'            +'";'
016500141002     C                             +'"'+      'STATUS'                 +'";'
016600141002     C                             +'"'+      'SIGNER'                 +'";'
016700140127     C                   ENDSL
016800100908     C*
016900071121     C                   EVAL      VAWDTA = wBufferOut
017000071121     C                   WRITE     TIVAW000
017100071121     C                   ENDIF
017200991027     C*
017300080924     C                   READ      TIVGDTMP
017400060119     C*
017500080924     C                   DOW       not %eof(TIVGDTMP)
017600080924     C*
017700080924     C                   eval      fnvac00t = vgdDTA
017800071121     C*
017900071121     C                   clear                   TIVAW000
018000071121     C                   clear                   wBufferOut
018100100907     C                   clear                   wLinkDir
018200100907     C                   clear                   wConsOkSiNo
018300100907     C                   clear                   wConsOkSiNoDes
018400140308     C                   clear                   dar5gen
018500140308     C                   clear                   dar5emd
018600100910     C*
018700100910     C* Solo bolle originali e valide ai fini della spedizione
018800100910     C                   movel     *blanks       wEsito1           1
018900100910     C*
019000100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019100100910     C                                                vacAAS
019200100910     C                                               :vacLNP
019300100910     C                                               :vacNRS
019400100910     C                                               :vacNSP
019500100910     C                                               :pOutLblTyp
019600100910     C                                               :pOutAnnoBO
019700100910     C                                               :pOutLineaParBO
019800100910     C                                               :pOutSerieBO
019900100910     C                                               :pOutNumSpedBO
020000100910     C                                               :pOutDcmBO
020100100910     C                                               :pOutCcaBO
020200100910     C                                               :pOutRblBO))
020300100910     C*
020400130426     C                   if         pOutRblBO ='N' AND
020500130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
020600071210     C*
020700071210     C* Inizializzo campi d wrk
020800140127     C                   EVAL      wDSP       = *blanks
020900140127     C                   EVAL      wRD2       = *blanks
021000140127     C                   EVAL      wIND       = *blanks
021100140127     C                   EVAL      wCAD       = *blanks
021200140127     C                   EVAL      wLOD       = *blanks
021300140127     C                   EVAL      wPRD       = *blanks
021400140127     C                   EVAL      wNCD       = *zeros
021500140127     C                   EVAL      wNCA       = *zeros
021600140127     C                   EVAL      wSPEDBRT   = *zeros
021700140127     C                   EVAL      wRSCMORI   = *blanks
021800140127     C                   EVAL      wINDMORI   = *blanks
021900140127     C                   EVAL      wCAPMORI   = *blanks
022000140127     C                   EVAL      wLOCMORI   = *blanks
022100140127     C                   EVAL      wPRVMORI   = *blanks
022200140129     C                   EVAL      wFIRMATAR  = *blanks
022300140308     C                   EVAL      wNoteGiac  = *blanks
022400140311     C                   EVAL      wGCPCMC    = *blanks
022500140311     C                   EVAL      wGiacCau   = *blanks
022600140308     C                   setoff                                       70
022700121207     C*
022800071210     C* Aggancio la bolla relativa x il completamento dei dati
022900071210     C     KEYtas30      chain     titas30c
023000071210     C                   if        %found(titas30c)
023100110519     C*
023200121207     C* Reperisco l'estensione ragione sociale destinatario
023300110519     C                   eval      ta4TRC = 'D'
023400140129     C     KEYta430C     chain     tita430c
023500110519     C                   if        %found(tita430c)
023600110519     C                   eval      wRD2 = '.' + ta4NOT
023700110519     C                   endif
023800140127     C*
023900140127     C* Reperisco, se c'è, la firma del consegnatario
024000140129     C                   eval      ta4TRC = '1'
024100140129     C     KEYta430C     chain     tita430c
024200140129     C                   if        %found(tita430c)
024300140129     C                   eval      wFIRMATAR = ta4NOT
024400140127     C                   endif
024500140308     C*
024600140308     C* Reperisco, se c'è, il referente consegna ed il suo telefono
024700140308     C                   eval      ar5TRD = 'GEN'
024800140308     C     KEYar531_1P   chain     fiar531C
024900140308     C                   if        %found(FIAR531C)
025000140308     C                   eval      dar5gen = ar5UNI
025100140308     C                   endif
025200140308     C*
025300140308     C* Reperisco, se c'è, il cellulare del destinatario
025400140308     C                   eval      ar5TRD = 'EMD'
025500140308     C     KEYar531_1P   chain     fiar531C
025600140308     C                   if        %found(FIAR531C)
025700140308     C                   eval      dar5emd = ar5UNI
025800140308     C                   endif
025900071210     C*
026000071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
026100140308     C                   eval      wDSP     =
026200140308     C                              UBFMTDATE_Convert(
026300140308     C                                  %editc((tasAAS*10000+tasMGS):'4') :
026400140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
026500140127     C                   eval      wIND     = tasIND
026600140127     C                   eval      wCAD     = tasCAD
026700140127     C                   eval      wLOD     = tasLOD
026800140127     C                   eval      wPRD     = tasPRD
026900140127     C                   eval      wNCD     = tasNCD
027000140127     C                   eval      wNCA     = tasNCA
027100140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
027200140127     C                                        tasNRS*10000000+
027300140127     C                                        tasNSP
027400121207     C*
027500121207     C* Reperisco i dati del mittente originale
027600140308     C***                eval      taaTRC = 'O'
027700140308     C***      aa30      chain     titaa30c
027800140308     C***                if        %found(titaa30c)
027900140308     C***                eval      wRSCMORI = taaRSC
028000140308     C***                eval      wINDMORI = taaIND
028100140308     C***                eval      wCAPMORI = taaCAP
028200140308     C***                eval      wLOCMORI = taaLOC
028300140308     C***                eval      wPRVMORI = taaPRV
028400140308     C***                endif
028500130913     C*
028600130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
028700130913     C* (costruisco la chiave sul file TIGCP51L)
028800130913     C*
028900140310     C                   EVAL      gcpDUR = *zeros
029000130913     C                   EVAL      gcpFRG = *zeros
029100130913     C     KEYgcp51      SETLL     tigcp51l
029200130913     C                   IF        %equal(tigcp51l)
029300130913     C     KEYgcp51      READE     tigcp51l
029400130913     C                   DOW       not %eof(tigcp51l)
029500130913     C*
029600130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
029700130913     C                   IF        gcpDUR > *zeros AND
029800130913     C                             gcpDDM = *zeros AND
029900130913     C                             gcpFAS < 030
030000130913     C                   seton                                        70
030100130913     C                   ENDIF
030200140311     C*
030300140311     C* Reperisco la causale di ultima apertura giacenza
030400140311     C                   EVAL      wGCPCMC = gcpCMC
030500130913     C*
030600130913     C* Proseguo la lettura delle giacenze x la bolla corrente
030700130913     C     KEYgcp51      READE     tigcp51l
030800130913     C                   ENDDO
030900140308     C*
031000140308     C* Se in giacenza reperisco le note di giacenza
031100140308     C                   EXSR      rtvNOTEGIAC
031200140308     C*
031300140308     C                   ENDIF
031400071213     C*
031500071213     C                   endif
031600071121     C*
031700071121     C* Compondo la stringa dati output relativa al record corrente
031800071121     C                   EXSR      wrifile
031900100910     C*
032000100910     C                   endif
032100040719     C*
032200040719     C* In ogni caso storicizzo e cancello l'input sempre
032300080924     C                   DELETE    TIVGD000
032400040719     C*
032500040719     C* Proseguo con la lettura dell'input
032600080924     C                   READ      TIVGDTMP
032700060119     C*
032800030325     C                   ENDDO
032900071121     C*
033000071121     C* Head finali della struttura HTML
033100071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
033200071210     C***                WRITE     TIVAW000
033300991027     C*
033400030325     C                   EVAL      wrkesito = '0'
033500991027     C*
033600910830     C                   ENDSR
033700100907
033800100907
033900100907
034000100907     C     calSTS        BEGSR
034100100910     C*
034200120926     C                   MOVEL     *blanks       wMIC              1
034300121114     C                   MOVEL     *blanks       wGIAC             1
034400121207     C                   MOVEL     *blanks       wLAVV             1
034500140129     C                   MOVEL     *blanks       wPEND             1
034600100910     C                   EVAL      wDataSts       = vacDCM
034700140308     C*
034800140308     C* Verifico lo stato delle disposizioni di giacenza
034900140308     C                   MOVEL     *blanks       wGiacDisp        10
035000140308     C   70              EVAL      wGiacDisp = 'IN ATTESA'
035100100907     C*
035200100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
035300100907     C                   IF        VACDCM > *zeros AND
035400100907     C                             (VACCCA = *blanks OR VACCCA = '1')
035500100907     C                   EVAL      wConsOkSiNo    = 'S'
035600140613     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
035700140613     C                   EVAL      wConsOkSiNoCod = '000'
035800140308     C                   EVAL      wGiacDisp      = *blanks
035900140308     C                   EVAL      wNoteGiac      = *blanks
036000100907     C                   ELSE
036100100907     C                   EVAL      wConsOkSiNo    = 'N'
036200140311     C                   EVAL      wFIRMATAR      = *blanks
036300100907     C*
036400100907     C* A questo punto stabilisco la descrizione
036500100910     C                   SELECT
036600100910     C                   WHEN      vacCCA = '1'
036700140613     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
036800140613     C                   EVAL      wConsOkSiNoCod = '002'
036900100910     C                   WHEN      vacCCA = '2'
037000140613     C                   EVAL      wConsOkSiNoDes = 'Resa'
037100140613     C                   EVAL      wConsOkSiNoCod = '003'
037200100907     C                   EVAL      wConsOkSiNo    = 'R'
037300100910     C                   WHEN      vacCCA = '3'
037400110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
037500140613     C                   EVAL      wConsOkSiNoCod = '009'
037600110519     C                   EVAL      wConsOkSiNo    = 'R'
037700100910     C                   WHEN      vacCCA = '4'
037800110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
037900110519     C                   EVAL      wConsOkSiNo    = 'R'
038000100910     C                   WHEN      vacCCA = '5'
038100110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
038200140613     C                   EVAL      wConsOkSiNoCod = '010'
038300110519     C                   EVAL      wConsOkSiNo    = 'R'
038400100910     C                   WHEN      vacCCA = '6'
038500110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
038600140613     C                   EVAL      wConsOkSiNoCod = '008'
038700110519     C                   EVAL      wConsOkSiNo    = 'R'
038800100910     C                   WHEN      vacCCA = 'A'
038900110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
039000140613     C                   EVAL      wConsOkSiNoCod = '011'
039100110519     C                   EVAL      wConsOkSiNo    = 'R'
039200100910     C                   WHEN      vacCCA = 'C'
039300140613     C                   EVAL      wConsOkSiNoDes = 'In consegna'
039400140613     C                   EVAL      wConsOkSiNoCod = '001'
039500120926     C                   EVAL      wMIC = 'S'
039600100910     C                   WHEN      vacCCA = 'R'
039700110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
039800140613     C                   EVAL      wConsOkSiNoCod = '013'
039900100910     C                   WHEN      vacCCA = 'S'
040000110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
040100140613     C                   EVAL      wConsOkSiNoCod = '012'
040200110519     C                   EVAL      wConsOkSiNo    = 'R'
040300100910     C                   ENDSL
040400100907     C*
040500140308     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
040600100910     C                   EVAL      wDataSts       = vacDLA
040700140613     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
040800140613     C                   EVAL      wConsOkSiNoCod = '004'
040900121207     C                   EVAL      wLAVV = 'S'
041000100907     C                   ENDIF
041100140308     C                   IF        (gcpDUR > vacDCM AND gcpDUR >= vacDLA)
041200140308     C                   EVAL      wDataSts       = gcpDUR
041300140613     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
041400140613     C                   EVAL      wConsOkSiNoCod = '005'
041500140311     C                   EXSR      rtvCAU_GIAC
041600121114     C                   EVAL      wGIAC = 'S'
041700140308     C                   ELSE
041800140308     C                   EVAL      wGiacDisp      = *blanks
041900140308     C                   EVAL      wNoteGiac      = *blanks
042000100907     C                   ENDIF
042100100910     C                   IF        vacDCM = *zeros AND
042200100910     C                             vacDLA = *zeros AND
042300140308     C                             gcpDUR = *zeros AND
042400100910     C                             vacCCA = *blanks
042500100910     C                   EVAL      wDataSts       = datcor
042600100910     C*
042700100910     C* Verifico la data consegna richiesta
042800100910     C                   IF        vacDCR > datcor
042900140308     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
043000140308     C                              UBFMTDATE_Convert(
043100140308     C                                  %editc(vacDCR:'4') :
043200140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
043300140613     C                   EVAL      wConsOkSiNoCod = '006'
043400100910     C                   ELSE
043500110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
043600140613     C                   EVAL      wConsOkSiNoCod = '007'
043700140129     C                   EVAL      wPEND          = 'S'
043800100910     C                   ENDIF
043900100910     C                   ENDIF
044000100907     C*
044100100907     C                   ENDIF
044200100907     C*
044300100907     C                   ENDSR
044400140311
044500140311
044600140311
044700140311     C     rtvCAU_GIAC   BEGSR
044800140311     C*
044900140311     C* Rererisco la descrizioe della causale di giacenza
045000140311     C                   MOVEL     '1'           tblKUT
045100140311     C                   MOVEL     '2A'          tblCOD
045200140311     C                   MOVEL(P)  wGCPCMC       tblKEY
045300140311     C     KEYtab00_C    CHAIN     TABEL00F                           30
045400140311     C                   IF        %found(tabel00f)
045500140311     C                   MOVEL     TBLUNI        ds2A
045600140311     C                   MOVEL(P)  §2ADES        wGiacCau
045700140311     C                   ENDIF
045800140311     C*
045900140311     C                   ENDSR
046000140311     C*------------------------------------------------------------------------*
046100140311
046200140308
046300140308
046400140308     C     rtvNOTEGIAC   BEGSR
046500140308     C*
046600140308     C* Scorro tutte le note giacenza relative alla giacebza corrente
046700140308     C     KEYgnp01_P    setll     tignp01l
046800140308     C                   if        %equal(tignp01l)
046900140308     C                   eval      wNoteGiac = ''''
047000140308     C     KEYgnp01_P    reade     tignp01l
047100140308     C                   dow       not %eof(tignp01l)
047200140308     C*** concateno note giacenza
047300140308     C                   eval      wNoteGiac =
047400140308     C                             %trim(wNoteGiac) + ' ' + gnpDMC
047500140308     C     KEYgnp01_P    reade     tignp01l
047600140308     C                   enddo
047700140308     C                   endif
047800140308     C*
047900140308     C                   ENDSR
048000100907
048100100907
048200100907
048300100907     C     makLNK        BEGSR
048400130426     C*
048500130426     C* Se richiesta inclusione link T&T
048600130426     C                   if        %subst(prmppt:6:7) <> *blanks
048700130426     C*
048800130426     C* Porto in minuscolo il parametro relativo alla lingua del link
048900130426     C                   movel     *blanks       parLang           2
049000130426     C                   eval      parLang = %subst(prmppt:13:2)
049100130426     C     maiu:minu     xlate     parLang       parLang
049200130426     C*
049300160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
049400130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
049500130426     C                                        %trim(%char(vacRMN))             +
049600130426     C                                        '&ksu='+%subst(prmppt:6:7)+
049700130426     C                                        '&lang='+parLang
049800130426     C*
049900130426     C                   endif
050000100907     C*
050100100907     C                   ENDSR
050200100907
050300071121
050400071121
050500071121
050600071121     C     wrifile       BEGSR
050700120926     C*
050800120926     C                   SETOFF                                       31
050900071121     C*
051000100907     C* Determino lo status della spedizione
051100100907     C                   EXSR      calSTS
051200100907     C*
051300100907     C* Costruisco il link diretto
051400130426     C                   EXSR      makLNK
051500130426     C*
051600130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
051700130426     C                   IF        %subst(prmppt:15:1) = 'P'
051800130426     C                   MOVE      vacCCM        CCM               4 0
051900130426     C                   MOVE      wKSC          KSC               4 0
052000130426     C                   IF        CCM =  8888 or  CCM =  9999
052100130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
052200130426     C                   EVAL      vacCCM = wKSC
052300130426     C                   ENDIF
052400130426     C                   ENDIF
052500130426     C                   ENDIF
052600100908     C*
052700140308     C                   EVAL      wDataStsAlfa =
052800140308     C                              UBFMTDATE_Convert(
052900140308     C                                  %editc(wDataSts:'4') :
053000140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
053100140308     C*
053200100908     C                   SELECT
053300100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
053400071121     C                   EVAL      wBufferOut = iRow +
053500141002     C                                  iCol+%trim(%editc(wSPEDBRT:'X'))  +fCol+
053600140308     C                                  iCol+%trim(wDSP)                  +fCol+
053700100907     C                                  iCol+%trim(vacRSD)                +fCol+
053800110519     C                                  iCol+%trim(wIND)                  +fCol+
053900100517     C                                  iCol+%trim(wCAD)                  +fCol+
054000100517     C                                  iCol+%trim(wLOD)                  +fCol+
054100110519     C                                  iCol+%trim(wPRD)                  +fCol+
054200100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
054300110519     C                                  iCol+%trim(vacRMA)                +fCol+
054400140308     C                                  iCol+%trim(wDataStsAlfa)          +fCol+
054500100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
054600140129     C                                  iCol+%trim(wFIRMATAR)             +fCol
054700100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
054800100908     C                   EVAL      wBufferOut =
054900141002     C                               '"'+   %trim(%editc(wSPEDBRT:'X'))   +'";'
055000140308     C                              +'"'+   %trim(wDSP)                   +'";'
055100110519     C                              +'"'+   %trim(vacRSD)                 +'";'
055200110519     C                              +'"'+   %trim(wIND)                   +'";'
055300110519     C                              +'"'+   %trim(wCAD)                   +'";'
055400110519     C                              +'"'+   %trim(wLOD)                   +'";'
055500110519     C                              +'"'+   %trim(wPRD)                   +'";'
055600110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
055700110519     C                              +'"'+   %trim(vacRMA)                 +'";'
055800140308     C                              +'"'+   %trim(wDataStsAlfa)           +'";'
055900110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
056000140129     C                              +'"'+   %trim(wFIRMATAR)              +'";'
056100100908     C                   ENDSL
056200071121     C*
056300120926     C* Se nei parametri è richiesto no MIC => gestisco
056400120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
056500120926     C                   SETON                                        31
056600120926     C                   ENDIF
056700121114     C*
056800121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
056900121207     C                   IF        %subst(prmppt:4:1) = 'S' and
057000121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
057100121114     C                   SETON                                        31
057200121114     C                   ENDIF
057300140124     C*
057400140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
057500140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
057600140124     C                   SETON                                        31
057700140124     C                   ENDIF
057800120926     C*
057900071121     C                   EVAL      VAWDTA = wBufferOut
058000110519     C*
058100110519     C* Restituisco solo le spedizioni "pendenti"
058200110713     C***                IF        wConsOkSiNo = 'N'
058300120926     C  N31              WRITE     TIVAW000
058400110713     C***                ENDIF
058500071121     C*
058600071121     C                   ENDSR
058700071121
058800071121
058900071121
059000991027
059100991027      /TITLE Operazioni iniziali.
059200991027     C     *inzsr        BEGSR
059300991027     C*
059400991027     C     *ENTRY        PLIST
059500991027     C                   parm                    prmppt
059600991027     C     wrkesito      parm      wrkesito      prmesito
059700100910     C*
059800100910     C* CALCOLA LA DATA CORRENTE
059900100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
060000100910     C                   eval      datcor = %dec(%date() : *ISO)
060100071210     C*
060200071213     C* TITAS30C - Parziale
060300071210     C     KEYtas30      KLIST
060400071210     C                   KFLD                    vacAAS
060500071210     C                   KFLD                    vacLNP
060600071210     C                   KFLD                    vacNRS
060700071210     C                   KFLD                    vacNSP
060800110519     C*
060900110519     C* TITA430C - Completa
061000140129     C     KEYta430C     KLIST
061100110519     C                   KFLD                    vacAAS
061200110519     C                   KFLD                    vacLNP
061300110519     C                   KFLD                    vacNRS
061400110519     C                   KFLD                    vacNSP
061500110519     C                   KFLD                    ta4TRC
061600121207     C*
061700121207     C* TITAA30C - Completa
061800121207     C     KEYtaa30      KLIST
061900121207     C                   KFLD                    vacAAS
062000121207     C                   KFLD                    vacLNP
062100121207     C                   KFLD                    vacNRS
062200121207     C                   KFLD                    vacNSP
062300121207     C                   KFLD                    taaTRC
062400130913     C*
062500130913     C* TIGCP51L - Completa
062600130913     C     KEYgcp51      KLIST
062700130913     C                   KFLD                    vacAAS
062800130913     C                   KFLD                    vacLNP
062900130913     C                   KFLD                    vacNRS
063000130913     C                   KFLD                    vacNSP
063100130913     C                   KFLD                    gcpFRG
063200140308     C*
063300140308     C* Chiave su FIAR531C - Parziale
063400140308     C     KEYar531_1P   KLIST
063500140308     C                   KFLD                    vacAAS
063600140308     C                   KFLD                    vacLNP
063700140308     C                   KFLD                    vacNRS
063800140308     C                   KFLD                    vacNSP
063900140308     C                   KFLD                    ar5TRD
064000140308     C*
064100140308     C* Chiave su TIGNP01L - Parziale
064200140308     C     KEYgnp01_P    KLIST
064300140308     C                   KFLD                    gcpAGC
064400140308     C                   KFLD                    gcpFGC
064500140308     C                   KFLD                    gcpNGC
064600140308     C                   KFLD                    gcpFRG
064700140311     C*
064800140311     C* TABEL00F - Completa
064900140311     C     KEYtab00_C    KLIST
065000140311     C                   KFLD                    tblKUT
065100140311     C                   KFLD                    tblCOD
065200140311     C                   KFLD                    tblKEY
065300991027     C*
065400991027     C                   ENDSR
065500071121     C*
065600071121** DEC - COSTANTI
065700071121<?xml version="1.0"?>
065800071121<?mso-application progid="Excel.Sheet"?>
065900071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
066000071121xmlns:o="urn:schemas-microsoft-com:office:office"
066100071121xmlns:x="urn:schemas-microsoft-com:office:excel"
066200071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
066300071121xmlns:html="http://www.w3.org/TR/REC-html40">
066400071121</Workbook>
066500071121<Worksheet ss:Name="PAG1">
066600071121</Worksheet>
066700071121<Table border="1">
066800071121</Table>
066900071121<Row>
067000071121</Row>
067100071121<Cell><Data ss:Type="String">
067200071121<Cell><Data ss:Type="Number">
067300071121</Data></Cell>
067400071121
067500071121
067600071121
