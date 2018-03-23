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
006200160926     D wDRT            s             10    inz(*blanks)
006300110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
006400110519     D wIND            s                   inz(*blanks) like(tasIND)
006500110519     D wCAD            s                   inz(*blanks) like(tasIND)
006600110519     D wLOD            s                   inz(*blanks) like(tasLOD)
006700110519     D wPRD            s                   inz(*blanks) like(tasPRD)
006800140127     D wNCD            s                   inz(*blanks) like(tasNCD)
006900140127     D wNCA            s                   inz(*blanks) like(tasNCA)
007000140127     D wSPEDBRT        s             12  0 inz
007100121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
007200121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
007300121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
007400121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
007500121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
007600140129     D wFIRMATAR       s                   inz(*blanks) like(ta4NOT)
007700130426     D wKSC            s              7  0 inz(*zeros)
007800140308     D wNoteGiac       s            300    inz(*blanks)
007900140311     D wGCPCMC         s                   inz(*blanks) like(gcpCMC)
008000140311     D wGiacCau        s                   inz(*blanks) like(§2ADES)
008100071211     D*
008200100910
008300100910
008400100910     D*------------------
008500100910     D* LINKING A DEFINIZIONI ESTERNE
008600100910     D*------------------
008700100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008800100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008900140308     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
009000140308     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
009100130426
009200130426
009300130426     D*-------------------
009400130426     D* COSTANTI
009500130426     D*-------------------
009600130426     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009700130426     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009800100910
009900030924
010000030325
010100030924     C* MAIN/
010200140308     C*
010300071121     C* Verifico se già presenti dati nel file d output
010400071121     C                   EXSR      chkRECFILMBR
010500071121     C*
010600071121     C* Effettuo elaborazione
010700071121     C                   EXSR      traduci
010800071121     C*
010900921023     C                   SETON                                        LR
011000991027
011100071121
011200071121
011300071121     C     chkRECFILMBR  BEGSR
011400071121     C*
011500071121     C                   READ      TIVAWWWT                               55
011600071121     C*
011700071121     C                   ENDSR
011800071121
011900071121
012000030924
012100991027     C     traduci       BEGSR
012200071121     C*
012300071121     C* Head iniziali della struttura HTML
012400100908     C                   IF        %subst(prmppt:2:1) = 'H'
012500071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
012600071210     C   55              WRITE     TIVAW000
012700100908     C                   ENDIF
012800071121     C*
012900071121     C* Inizializzazone formato e variabili x output
013000071121     C                   clear                   TIVAW000
013100071121     C                   clear                   wBufferOut
013200100907     C                   clear                   wLinkDir
013300100907     C                   clear                   wConsOkSiNo
013400100907     C                   clear                   wConsOkSiNoDes
013500071121     C*
013600071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013700071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
013800100908     C*
013900100908     C                   SELECT
014000100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
014100071121     C                   EVAL      wBufferOut = iThead + iRow
014200141002     C                             +iColBold+ 'LDV BRT'                +fColBold
014300141002     C                             +iColBold+ 'SHIPMENT DATE'          +fColBold
014400141002     C                             +iColBold+ 'CONSIGNEE NAME'         +fColBold
014500141002     C                             +iColBold+ 'CONSIGNEE ADDRESS'      +fColBold
014600141002     C                             +iColBold+ 'CONSIGNEE ZIP CODE'     +fColBold
014700141002     C                             +iColBold+ 'CONSIGNEE CITY'         +fColBold
014800141002     C                             +iColBold+ 'CONSIGNEE PROVINCE CODE'+fColBold
014900141002     C                             +iColBold+ 'SHIPMENT NUM   REF.ID'  +fColBold
015000141002     C                             +iColBold+ 'SHIPMENT ALPHA REF.ID'  +fColBold
015100141002     C                             +iColBold+ 'STATUS DATE'            +fColBold
015200141002     C                             +iColBold+ 'STATUS'                 +fColBold
015300141002     C                             +iColBold+ 'SIGNER'                 +fColBold
015400160926     C                             +iColBold+ 'ENTRUSTED DATE'         +fColBold
015500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
015600100908     C                   EVAL      wBufferOut =
015700141002     C                              '"'+      'LDV BRT'                +'";'
015800141002     C                             +'"'+      'SHIPMENT DATE'          +'";'
015900141002     C                             +'"'+      'CONSIGNEE NAME'         +'";'
016000141002     C                             +'"'+      'CONSIGNEE ADDRESS'      +'";'
016100141002     C                             +'"'+      'CONSIGNEE ZIP CODE'     +'";'
016200141002     C                             +'"'+      'CONSIGNEE CITY'         +'";'
016300141002     C                             +'"'+      'CONSIGNEE PROVINCE CODE'+'";'
016400141002     C                             +'"'+      'SHIPMENT NUM   REF.ID'  +'";'
016500141002     C                             +'"'+      'SHIPMENT ALPHA REF.ID'  +'";'
016600141002     C                             +'"'+      'STATUS DATE'            +'";'
016700141002     C                             +'"'+      'STATUS'                 +'";'
016800141002     C                             +'"'+      'SIGNER'                 +'";'
016900160926     C                             +'"'+      'ENTRUSTED DATE'         +'";'
017000140127     C                   ENDSL
017100100908     C*
017200071121     C                   EVAL      VAWDTA = wBufferOut
017300071121     C                   WRITE     TIVAW000
017400071121     C                   ENDIF
017500991027     C*
017600080924     C                   READ      TIVGDTMP
017700060119     C*
017800080924     C                   DOW       not %eof(TIVGDTMP)
017900080924     C*
018000080924     C                   eval      fnvac00t = vgdDTA
018100071121     C*
018200071121     C                   clear                   TIVAW000
018300071121     C                   clear                   wBufferOut
018400100907     C                   clear                   wLinkDir
018500100907     C                   clear                   wConsOkSiNo
018600100907     C                   clear                   wConsOkSiNoDes
018700140308     C                   clear                   dar5gen
018800140308     C                   clear                   dar5emd
018900100910     C*
019000100910     C* Solo bolle originali e valide ai fini della spedizione
019100100910     C                   movel     *blanks       wEsito1           1
019200100910     C*
019300100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019400100910     C                                                vacAAS
019500100910     C                                               :vacLNP
019600100910     C                                               :vacNRS
019700100910     C                                               :vacNSP
019800100910     C                                               :pOutLblTyp
019900100910     C                                               :pOutAnnoBO
020000100910     C                                               :pOutLineaParBO
020100100910     C                                               :pOutSerieBO
020200100910     C                                               :pOutNumSpedBO
020300100910     C                                               :pOutDcmBO
020400100910     C                                               :pOutCcaBO
020500100910     C                                               :pOutRblBO))
020600100910     C*
020700130426     C                   if         pOutRblBO ='N' AND
020800130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
020900071210     C*
021000071210     C* Inizializzo campi d wrk
021100140127     C                   EVAL      wDSP       = *blanks
021200160926     C                   EVAL      wDRT       = *blanks
021300140127     C                   EVAL      wRD2       = *blanks
021400140127     C                   EVAL      wIND       = *blanks
021500140127     C                   EVAL      wCAD       = *blanks
021600140127     C                   EVAL      wLOD       = *blanks
021700140127     C                   EVAL      wPRD       = *blanks
021800140127     C                   EVAL      wNCD       = *zeros
021900140127     C                   EVAL      wNCA       = *zeros
022000140127     C                   EVAL      wSPEDBRT   = *zeros
022100140127     C                   EVAL      wRSCMORI   = *blanks
022200140127     C                   EVAL      wINDMORI   = *blanks
022300140127     C                   EVAL      wCAPMORI   = *blanks
022400140127     C                   EVAL      wLOCMORI   = *blanks
022500140127     C                   EVAL      wPRVMORI   = *blanks
022600140129     C                   EVAL      wFIRMATAR  = *blanks
022700140308     C                   EVAL      wNoteGiac  = *blanks
022800140311     C                   EVAL      wGCPCMC    = *blanks
022900140311     C                   EVAL      wGiacCau   = *blanks
023000140308     C                   setoff                                       70
023100121207     C*
023200071210     C* Aggancio la bolla relativa x il completamento dei dati
023300071210     C     KEYtas30      chain     titas30c
023400071210     C                   if        %found(titas30c)
023500110519     C*
023600121207     C* Reperisco l'estensione ragione sociale destinatario
023700110519     C                   eval      ta4TRC = 'D'
023800140129     C     KEYta430C     chain     tita430c
023900110519     C                   if        %found(tita430c)
024000110519     C                   eval      wRD2 = '.' + ta4NOT
024100110519     C                   endif
024200140127     C*
024300140127     C* Reperisco, se c'è, la firma del consegnatario
024400140129     C                   eval      ta4TRC = '1'
024500140129     C     KEYta430C     chain     tita430c
024600140129     C                   if        %found(tita430c)
024700140129     C                   eval      wFIRMATAR = ta4NOT
024800140127     C                   endif
024900140308     C*
025000140308     C* Reperisco, se c'è, il referente consegna ed il suo telefono
025100140308     C                   eval      ar5TRD = 'GEN'
025200140308     C     KEYar531_1P   chain     fiar531C
025300140308     C                   if        %found(FIAR531C)
025400140308     C                   eval      dar5gen = ar5UNI
025500140308     C                   endif
025600140308     C*
025700140308     C* Reperisco, se c'è, il cellulare del destinatario
025800140308     C                   eval      ar5TRD = 'EMD'
025900140308     C     KEYar531_1P   chain     fiar531C
026000140308     C                   if        %found(FIAR531C)
026100140308     C                   eval      dar5emd = ar5UNI
026200140308     C                   endif
026300071210     C*
026400071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
026500140308     C                   eval      wDSP     =
026600140308     C                              UBFMTDATE_Convert(
026700140308     C                                  %editc((tasAAS*10000+tasMGS):'4') :
026800140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
026900160926     C                   eval      wDRT     =
027000160926     C                              UBFMTDATE_Convert(
027100160926     C                                  %editc((tasDRT):'4') :
027200160926     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
027300140127     C                   eval      wIND     = tasIND
027400140127     C                   eval      wCAD     = tasCAD
027500140127     C                   eval      wLOD     = tasLOD
027600140127     C                   eval      wPRD     = tasPRD
027700140127     C                   eval      wNCD     = tasNCD
027800140127     C                   eval      wNCA     = tasNCA
027900140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
028000140127     C                                        tasNRS*10000000+
028100140127     C                                        tasNSP
028200121207     C*
028300121207     C* Reperisco i dati del mittente originale
028400140308     C***                eval      taaTRC = 'O'
028500140308     C***      aa30      chain     titaa30c
028600140308     C***                if        %found(titaa30c)
028700140308     C***                eval      wRSCMORI = taaRSC
028800140308     C***                eval      wINDMORI = taaIND
028900140308     C***                eval      wCAPMORI = taaCAP
029000140308     C***                eval      wLOCMORI = taaLOC
029100140308     C***                eval      wPRVMORI = taaPRV
029200140308     C***                endif
029300130913     C*
029400130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
029500130913     C* (costruisco la chiave sul file TIGCP51L)
029600130913     C*
029700140310     C                   EVAL      gcpDUR = *zeros
029800130913     C                   EVAL      gcpFRG = *zeros
029900130913     C     KEYgcp51      SETLL     tigcp51l
030000130913     C                   IF        %equal(tigcp51l)
030100130913     C     KEYgcp51      READE     tigcp51l
030200130913     C                   DOW       not %eof(tigcp51l)
030300130913     C*
030400130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
030500130913     C                   IF        gcpDUR > *zeros AND
030600130913     C                             gcpDDM = *zeros AND
030700130913     C                             gcpFAS < 030
030800130913     C                   seton                                        70
030900130913     C                   ENDIF
031000140311     C*
031100140311     C* Reperisco la causale di ultima apertura giacenza
031200140311     C                   EVAL      wGCPCMC = gcpCMC
031300130913     C*
031400130913     C* Proseguo la lettura delle giacenze x la bolla corrente
031500130913     C     KEYgcp51      READE     tigcp51l
031600130913     C                   ENDDO
031700140308     C*
031800140308     C* Se in giacenza reperisco le note di giacenza
031900140308     C                   EXSR      rtvNOTEGIAC
032000140308     C*
032100140308     C                   ENDIF
032200071213     C*
032300071213     C                   endif
032400071121     C*
032500071121     C* Compondo la stringa dati output relativa al record corrente
032600071121     C                   EXSR      wrifile
032700100910     C*
032800100910     C                   endif
032900040719     C*
033000040719     C* In ogni caso storicizzo e cancello l'input sempre
033100080924     C                   DELETE    TIVGD000
033200040719     C*
033300040719     C* Proseguo con la lettura dell'input
033400080924     C                   READ      TIVGDTMP
033500060119     C*
033600030325     C                   ENDDO
033700071121     C*
033800071121     C* Head finali della struttura HTML
033900071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
034000071210     C***                WRITE     TIVAW000
034100991027     C*
034200030325     C                   EVAL      wrkesito = '0'
034300991027     C*
034400910830     C                   ENDSR
034500100907
034600100907
034700100907
034800100907     C     calSTS        BEGSR
034900100910     C*
035000120926     C                   MOVEL     *blanks       wMIC              1
035100121114     C                   MOVEL     *blanks       wGIAC             1
035200121207     C                   MOVEL     *blanks       wLAVV             1
035300140129     C                   MOVEL     *blanks       wPEND             1
035400100910     C                   EVAL      wDataSts       = vacDCM
035500140308     C*
035600140308     C* Verifico lo stato delle disposizioni di giacenza
035700140308     C                   MOVEL     *blanks       wGiacDisp        10
035800140308     C   70              EVAL      wGiacDisp = 'IN ATTESA'
035900100907     C*
036000100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
036100100907     C                   IF        VACDCM > *zeros AND
036200100907     C                             (VACCCA = *blanks OR VACCCA = '1')
036300100907     C                   EVAL      wConsOkSiNo    = 'S'
036400140613     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
036500140613     C                   EVAL      wConsOkSiNoCod = '000'
036600140308     C                   EVAL      wGiacDisp      = *blanks
036700140308     C                   EVAL      wNoteGiac      = *blanks
036800100907     C                   ELSE
036900100907     C                   EVAL      wConsOkSiNo    = 'N'
037000140311     C                   EVAL      wFIRMATAR      = *blanks
037100100907     C*
037200100907     C* A questo punto stabilisco la descrizione
037300100910     C                   SELECT
037400100910     C                   WHEN      vacCCA = '1'
037500140613     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
037600140613     C                   EVAL      wConsOkSiNoCod = '002'
037700100910     C                   WHEN      vacCCA = '2'
037800140613     C                   EVAL      wConsOkSiNoDes = 'Resa'
037900140613     C                   EVAL      wConsOkSiNoCod = '003'
038000100907     C                   EVAL      wConsOkSiNo    = 'R'
038100100910     C                   WHEN      vacCCA = '3'
038200110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
038300140613     C                   EVAL      wConsOkSiNoCod = '009'
038400110519     C                   EVAL      wConsOkSiNo    = 'R'
038500100910     C                   WHEN      vacCCA = '4'
038600110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
038700110519     C                   EVAL      wConsOkSiNo    = 'R'
038800100910     C                   WHEN      vacCCA = '5'
038900110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
039000140613     C                   EVAL      wConsOkSiNoCod = '010'
039100110519     C                   EVAL      wConsOkSiNo    = 'R'
039200100910     C                   WHEN      vacCCA = '6'
039300110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
039400140613     C                   EVAL      wConsOkSiNoCod = '008'
039500110519     C                   EVAL      wConsOkSiNo    = 'R'
039600100910     C                   WHEN      vacCCA = 'A'
039700110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
039800140613     C                   EVAL      wConsOkSiNoCod = '011'
039900110519     C                   EVAL      wConsOkSiNo    = 'R'
040000100910     C                   WHEN      vacCCA = 'C'
040100140613     C                   EVAL      wConsOkSiNoDes = 'In consegna'
040200140613     C                   EVAL      wConsOkSiNoCod = '001'
040300120926     C                   EVAL      wMIC = 'S'
040400100910     C                   WHEN      vacCCA = 'R'
040500110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
040600140613     C                   EVAL      wConsOkSiNoCod = '013'
040700100910     C                   WHEN      vacCCA = 'S'
040800110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
040900140613     C                   EVAL      wConsOkSiNoCod = '012'
041000110519     C                   EVAL      wConsOkSiNo    = 'R'
041100100910     C                   ENDSL
041200100907     C*
041300140308     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
041400100910     C                   EVAL      wDataSts       = vacDLA
041500140613     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
041600140613     C                   EVAL      wConsOkSiNoCod = '004'
041700121207     C                   EVAL      wLAVV = 'S'
041800100907     C                   ENDIF
041900140308     C                   IF        (gcpDUR > vacDCM AND gcpDUR >= vacDLA)
042000140308     C                   EVAL      wDataSts       = gcpDUR
042100140613     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
042200140613     C                   EVAL      wConsOkSiNoCod = '005'
042300140311     C                   EXSR      rtvCAU_GIAC
042400121114     C                   EVAL      wGIAC = 'S'
042500140308     C                   ELSE
042600140308     C                   EVAL      wGiacDisp      = *blanks
042700140308     C                   EVAL      wNoteGiac      = *blanks
042800100907     C                   ENDIF
042900100910     C                   IF        vacDCM = *zeros AND
043000100910     C                             vacDLA = *zeros AND
043100140308     C                             gcpDUR = *zeros AND
043200100910     C                             vacCCA = *blanks
043300100910     C                   EVAL      wDataSts       = datcor
043400100910     C*
043500100910     C* Verifico la data consegna richiesta
043600100910     C                   IF        vacDCR > datcor
043700140308     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
043800140308     C                              UBFMTDATE_Convert(
043900140308     C                                  %editc(vacDCR:'4') :
044000140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
044100140613     C                   EVAL      wConsOkSiNoCod = '006'
044200100910     C                   ELSE
044300110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
044400140613     C                   EVAL      wConsOkSiNoCod = '007'
044500140129     C                   EVAL      wPEND          = 'S'
044600100910     C                   ENDIF
044700100910     C                   ENDIF
044800100907     C*
044900100907     C                   ENDIF
045000100907     C*
045100100907     C                   ENDSR
045200140311
045300140311
045400140311
045500140311     C     rtvCAU_GIAC   BEGSR
045600140311     C*
045700140311     C* Rererisco la descrizioe della causale di giacenza
045800140311     C                   MOVEL     '1'           tblKUT
045900140311     C                   MOVEL     '2A'          tblCOD
046000140311     C                   MOVEL(P)  wGCPCMC       tblKEY
046100140311     C     KEYtab00_C    CHAIN     TABEL00F                           30
046200140311     C                   IF        %found(tabel00f)
046300140311     C                   MOVEL     TBLUNI        ds2A
046400140311     C                   MOVEL(P)  §2ADES        wGiacCau
046500140311     C                   ENDIF
046600140311     C*
046700140311     C                   ENDSR
046800140311     C*------------------------------------------------------------------------*
046900140311
047000140308
047100140308
047200140308     C     rtvNOTEGIAC   BEGSR
047300140308     C*
047400140308     C* Scorro tutte le note giacenza relative alla giacebza corrente
047500140308     C     KEYgnp01_P    setll     tignp01l
047600140308     C                   if        %equal(tignp01l)
047700140308     C                   eval      wNoteGiac = ''''
047800140308     C     KEYgnp01_P    reade     tignp01l
047900140308     C                   dow       not %eof(tignp01l)
048000140308     C*** concateno note giacenza
048100140308     C                   eval      wNoteGiac =
048200140308     C                             %trim(wNoteGiac) + ' ' + gnpDMC
048300140308     C     KEYgnp01_P    reade     tignp01l
048400140308     C                   enddo
048500140308     C                   endif
048600140308     C*
048700140308     C                   ENDSR
048800100907
048900100907
049000100907
049100100907     C     makLNK        BEGSR
049200130426     C*
049300130426     C* Se richiesta inclusione link T&T
049400130426     C                   if        %subst(prmppt:6:7) <> *blanks
049500130426     C*
049600130426     C* Porto in minuscolo il parametro relativo alla lingua del link
049700130426     C                   movel     *blanks       parLang           2
049800130426     C                   eval      parLang = %subst(prmppt:13:2)
049900130426     C     maiu:minu     xlate     parLang       parLang
050000130426     C*
050100160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
050200130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
050300130426     C                                        %trim(%char(vacRMN))             +
050400130426     C                                        '&ksu='+%subst(prmppt:6:7)+
050500130426     C                                        '&lang='+parLang
050600130426     C*
050700130426     C                   endif
050800100907     C*
050900100907     C                   ENDSR
051000100907
051100071121
051200071121
051300071121
051400071121     C     wrifile       BEGSR
051500120926     C*
051600120926     C                   SETOFF                                       31
051700071121     C*
051800100907     C* Determino lo status della spedizione
051900100907     C                   EXSR      calSTS
052000100907     C*
052100100907     C* Costruisco il link diretto
052200130426     C                   EXSR      makLNK
052300130426     C*
052400130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
052500130426     C                   IF        %subst(prmppt:15:1) = 'P'
052600130426     C                   MOVE      vacCCM        CCM               4 0
052700130426     C                   MOVE      wKSC          KSC               4 0
052800130426     C                   IF        CCM =  8888 or  CCM =  9999
052900130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
053000130426     C                   EVAL      vacCCM = wKSC
053100130426     C                   ENDIF
053200130426     C                   ENDIF
053300130426     C                   ENDIF
053400100908     C*
053500140308     C                   EVAL      wDataStsAlfa =
053600140308     C                              UBFMTDATE_Convert(
053700140308     C                                  %editc(wDataSts:'4') :
053800140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
053900140308     C*
054000100908     C                   SELECT
054100100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
054200071121     C                   EVAL      wBufferOut = iRow +
054300141002     C                                  iCol+%trim(%editc(wSPEDBRT:'X'))  +fCol+
054400140308     C                                  iCol+%trim(wDSP)                  +fCol+
054500100907     C                                  iCol+%trim(vacRSD)                +fCol+
054600110519     C                                  iCol+%trim(wIND)                  +fCol+
054700100517     C                                  iCol+%trim(wCAD)                  +fCol+
054800100517     C                                  iCol+%trim(wLOD)                  +fCol+
054900110519     C                                  iCol+%trim(wPRD)                  +fCol+
055000100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
055100110519     C                                  iCol+%trim(vacRMA)                +fCol+
055200140308     C                                  iCol+%trim(wDataStsAlfa)          +fCol+
055300100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
055400160926     C                                  iCol+%trim(wFIRMATAR)             +fCol+
055500160926     C                                  iCol+%trim(wDRT)                  +fCol
055600100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
055700100908     C                   EVAL      wBufferOut =
055800141002     C                               '"'+   %trim(%editc(wSPEDBRT:'X'))   +'";'
055900140308     C                              +'"'+   %trim(wDSP)                   +'";'
056000110519     C                              +'"'+   %trim(vacRSD)                 +'";'
056100110519     C                              +'"'+   %trim(wIND)                   +'";'
056200110519     C                              +'"'+   %trim(wCAD)                   +'";'
056300110519     C                              +'"'+   %trim(wLOD)                   +'";'
056400110519     C                              +'"'+   %trim(wPRD)                   +'";'
056500110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
056600110519     C                              +'"'+   %trim(vacRMA)                 +'";'
056700140308     C                              +'"'+   %trim(wDataStsAlfa)           +'";'
056800110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
056900140129     C                              +'"'+   %trim(wFIRMATAR)              +'";'
057000160926     C                              +'"'+   %trim(wDRT)                   +'";'
057100100908     C                   ENDSL
057200071121     C*
057300120926     C* Se nei parametri è richiesto no MIC => gestisco
057400120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
057500120926     C                   SETON                                        31
057600120926     C                   ENDIF
057700121114     C*
057800121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
057900121207     C                   IF        %subst(prmppt:4:1) = 'S' and
058000121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
058100121114     C                   SETON                                        31
058200121114     C                   ENDIF
058300140124     C*
058400140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
058500140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
058600140124     C                   SETON                                        31
058700140124     C                   ENDIF
058800120926     C*
058900071121     C                   EVAL      VAWDTA = wBufferOut
059000110519     C*
059100110519     C* Restituisco solo le spedizioni "pendenti"
059200110713     C***                IF        wConsOkSiNo = 'N'
059300120926     C  N31              WRITE     TIVAW000
059400110713     C***                ENDIF
059500071121     C*
059600071121     C                   ENDSR
059700071121
059800071121
059900071121
060000991027
060100991027      /TITLE Operazioni iniziali.
060200991027     C     *inzsr        BEGSR
060300991027     C*
060400991027     C     *ENTRY        PLIST
060500991027     C                   parm                    prmppt
060600991027     C     wrkesito      parm      wrkesito      prmesito
060700100910     C*
060800100910     C* CALCOLA LA DATA CORRENTE
060900100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061000100910     C                   eval      datcor = %dec(%date() : *ISO)
061100071210     C*
061200071213     C* TITAS30C - Parziale
061300071210     C     KEYtas30      KLIST
061400071210     C                   KFLD                    vacAAS
061500071210     C                   KFLD                    vacLNP
061600071210     C                   KFLD                    vacNRS
061700071210     C                   KFLD                    vacNSP
061800110519     C*
061900110519     C* TITA430C - Completa
062000140129     C     KEYta430C     KLIST
062100110519     C                   KFLD                    vacAAS
062200110519     C                   KFLD                    vacLNP
062300110519     C                   KFLD                    vacNRS
062400110519     C                   KFLD                    vacNSP
062500110519     C                   KFLD                    ta4TRC
062600121207     C*
062700121207     C* TITAA30C - Completa
062800121207     C     KEYtaa30      KLIST
062900121207     C                   KFLD                    vacAAS
063000121207     C                   KFLD                    vacLNP
063100121207     C                   KFLD                    vacNRS
063200121207     C                   KFLD                    vacNSP
063300121207     C                   KFLD                    taaTRC
063400130913     C*
063500130913     C* TIGCP51L - Completa
063600130913     C     KEYgcp51      KLIST
063700130913     C                   KFLD                    vacAAS
063800130913     C                   KFLD                    vacLNP
063900130913     C                   KFLD                    vacNRS
064000130913     C                   KFLD                    vacNSP
064100130913     C                   KFLD                    gcpFRG
064200140308     C*
064300140308     C* Chiave su FIAR531C - Parziale
064400140308     C     KEYar531_1P   KLIST
064500140308     C                   KFLD                    vacAAS
064600140308     C                   KFLD                    vacLNP
064700140308     C                   KFLD                    vacNRS
064800140308     C                   KFLD                    vacNSP
064900140308     C                   KFLD                    ar5TRD
065000140308     C*
065100140308     C* Chiave su TIGNP01L - Parziale
065200140308     C     KEYgnp01_P    KLIST
065300140308     C                   KFLD                    gcpAGC
065400140308     C                   KFLD                    gcpFGC
065500140308     C                   KFLD                    gcpNGC
065600140308     C                   KFLD                    gcpFRG
065700140311     C*
065800140311     C* TABEL00F - Completa
065900140311     C     KEYtab00_C    KLIST
066000140311     C                   KFLD                    tblKUT
066100140311     C                   KFLD                    tblCOD
066200140311     C                   KFLD                    tblKEY
066300991027     C*
066400991027     C                   ENDSR
066500071121     C*
066600071121** DEC - COSTANTI
066700071121<?xml version="1.0"?>
066800071121<?mso-application progid="Excel.Sheet"?>
066900071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
067000071121xmlns:o="urn:schemas-microsoft-com:office:office"
067100071121xmlns:x="urn:schemas-microsoft-com:office:excel"
067200071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
067300071121xmlns:html="http://www.w3.org/TR/REC-html40">
067400071121</Workbook>
067500071121<Worksheet ss:Name="PAG1">
067600071121</Worksheet>
067700071121<Table border="1">
067800071121</Table>
067900071121<Row>
068000071121</Row>
068100071121<Cell><Data ss:Type="String">
068200071121<Cell><Data ss:Type="Number">
068300071121</Data></Cell>
068400071121
068500071121
068600071121
