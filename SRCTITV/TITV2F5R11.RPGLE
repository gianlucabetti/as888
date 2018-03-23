000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200140127     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400170222     FTIVGDTMP  IF   E             DISK
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
013400170222     C*
013500170222     C* Se richiesto descrizioni status in INGLESE => gestisco
013600170222     C                   IF        %subst(prmppt:17:1) = 'E'
013700170222     C                   SETON                                        56
013800170222     C                   ELSE
013900170222     C                   SETOFF                                       56
014000170222     C                   ENDIF
014100071121     C*
014200071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
014300071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
014400100908     C*
014500100908     C                   SELECT
014600100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
014700071121     C                   EVAL      wBufferOut = iThead + iRow
014800170222     C                             +iColBold+ 'CUSTOMER CODE'          +fColBold
014900170222     C                             +iColBold+ 'SHIPMENT DATE'          +fColBold
015000170222     C                             +iColBold+ 'ARRIVAL DEPOT'          +fColBold
015100170222     C                             +iColBold+ 'CONSIGNEE NAME'         +fColBold
015200170222     C                             +iColBold+ 'CONSIGNEE PHONE'        +fColBold
015300170222     C                             +iColBold+ 'DELIVERY CONTACT'       +fColBold
015400170222     C                             +iColBold+ 'CONSIGNEE ADDRESS'      +fColBold
015500170222     C                             +iColBold+ 'CONSIGNEE ZIP CODE'     +fColBold
015600170222     C                             +iColBold+ 'CONSIGNEE CITY'         +fColBold
015700170222     C                             +iColBold+ 'CONSIGNEE AREA CODE'    +fColBold
015800170222     C                             +iColBold+ 'SHIPMENT NUM. REF. ID'  +fColBold
015900170222     C                             +iColBold+ 'SHIPMENT ALPHA REF. ID' +fColBold
016000170222     C                             +iColBold+ 'STATUS DATE'            +fColBold
016100170222     C                             +iColBold+ 'STATUS'                 +fColBold
016200170222     C                             +iColBold+ 'STATUS CODE'            +fColBold
016300170222     C                             +iColBold+ 'ON-HOLF DISP.'          +fColBold
016400170222     C                             +iColBold+ 'ON-HOLD CAUSE'          +fColBold
016500170222     C                             +iColBold+ 'STATUS NOTES'           +fColBold
016600170222     C                             +iColBold+ 'BRT SHIPMENT ID'        +fColBold
016700170222     C                             +iColBold+ 'DELIVERY SIGN'          +fColBold
016800100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
016900100908     C                   EVAL      wBufferOut =
017000170222     C                              '"'+      'CUSTOMER CODE'          +'";'
017100170222     C                             +'"'+      'SHIPMENT DATE'          +'";'
017200170222     C                             +'"'+      'ARRIVAL DEPOT'          +'";'
017300170222     C                             +'"'+      'CONSIGNEE NAME'         +'";'
017400170222     C                             +'"'+      'CONSIGNEE PHONE'        +'";'
017500170222     C                             +'"'+      'DELIVERY CONTACT'       +'";'
017600170222     C                             +'"'+      'CONSIGNEE ADDRESS'      +'";'
017700170222     C                             +'"'+      'CONSIGNEE ZIP CODE'     +'";'
017800170222     C                             +'"'+      'CONSIGNEE CITY'         +'";'
017900170222     C                             +'"'+      'CONSIGNEE AREA CODE'    +'";'
018000170222     C                             +'"'+      'SHIPMENT NUM. REF. ID'  +'";'
018100170222     C                             +'"'+      'SHIPMENT ALPHA REF. ID' +'";'
018200170222     C                             +'"'+      'STATUS DATE'            +'";'
018300170222     C                             +'"'+      'STATUS'                 +'";'
018400170222     C                             +'"'+      'STATUS CODE'            +'";'
018500170222     C                             +'"'+      'ON-HOLF DISP.'          +'";'
018600170222     C                             +'"'+      'ON-HOLD CAUSE'          +'";'
018700170222     C                             +'"'+      'STATUS NOTES'           +'";'
018800170222     C                             +'"'+      'BRT SHIPMENT ID'        +'";'
018900170222     C                             +'"'+      'DELIVERY SIGN'          +'";'
019000140127     C                   ENDSL
019100100908     C*
019200071121     C                   EVAL      VAWDTA = wBufferOut
019300071121     C                   WRITE     TIVAW000
019400071121     C                   ENDIF
019500991027     C*
019600080924     C                   READ      TIVGDTMP
019700060119     C*
019800080924     C                   DOW       not %eof(TIVGDTMP)
019900080924     C*
020000080924     C                   eval      fnvac00t = vgdDTA
020100071121     C*
020200071121     C                   clear                   TIVAW000
020300071121     C                   clear                   wBufferOut
020400100907     C                   clear                   wLinkDir
020500100907     C                   clear                   wConsOkSiNo
020600100907     C                   clear                   wConsOkSiNoDes
020700140308     C                   clear                   dar5gen
020800140308     C                   clear                   dar5emd
020900100910     C*
021000100910     C* Solo bolle originali e valide ai fini della spedizione
021100100910     C                   movel     *blanks       wEsito1           1
021200100910     C*
021300100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
021400100910     C                                                vacAAS
021500100910     C                                               :vacLNP
021600100910     C                                               :vacNRS
021700100910     C                                               :vacNSP
021800100910     C                                               :pOutLblTyp
021900100910     C                                               :pOutAnnoBO
022000100910     C                                               :pOutLineaParBO
022100100910     C                                               :pOutSerieBO
022200100910     C                                               :pOutNumSpedBO
022300100910     C                                               :pOutDcmBO
022400100910     C                                               :pOutCcaBO
022500100910     C                                               :pOutRblBO))
022600100910     C*
022700130426     C                   if         pOutRblBO ='N' AND
022800130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
022900071210     C*
023000071210     C* Inizializzo campi d wrk
023100140127     C                   EVAL      wDSP       = *blanks
023200140127     C                   EVAL      wRD2       = *blanks
023300140127     C                   EVAL      wIND       = *blanks
023400140127     C                   EVAL      wCAD       = *blanks
023500140127     C                   EVAL      wLOD       = *blanks
023600140127     C                   EVAL      wPRD       = *blanks
023700140127     C                   EVAL      wNCD       = *zeros
023800140127     C                   EVAL      wNCA       = *zeros
023900140127     C                   EVAL      wSPEDBRT   = *zeros
024000140127     C                   EVAL      wRSCMORI   = *blanks
024100140127     C                   EVAL      wINDMORI   = *blanks
024200140127     C                   EVAL      wCAPMORI   = *blanks
024300140127     C                   EVAL      wLOCMORI   = *blanks
024400140127     C                   EVAL      wPRVMORI   = *blanks
024500140129     C                   EVAL      wFIRMATAR  = *blanks
024600140308     C                   EVAL      wNoteGiac  = *blanks
024700140311     C                   EVAL      wGCPCMC    = *blanks
024800140311     C                   EVAL      wGiacCau   = *blanks
024900140308     C                   setoff                                       70
025000121207     C*
025100071210     C* Aggancio la bolla relativa x il completamento dei dati
025200071210     C     KEYtas30      chain     titas30c
025300071210     C                   if        %found(titas30c)
025400110519     C*
025500121207     C* Reperisco l'estensione ragione sociale destinatario
025600110519     C                   eval      ta4TRC = 'D'
025700140129     C     KEYta430C     chain     tita430c
025800110519     C                   if        %found(tita430c)
025900110519     C                   eval      wRD2 = '.' + ta4NOT
026000110519     C                   endif
026100140127     C*
026200140127     C* Reperisco, se c'è, la firma del consegnatario
026300140129     C                   eval      ta4TRC = '1'
026400140129     C     KEYta430C     chain     tita430c
026500140129     C                   if        %found(tita430c)
026600140129     C                   eval      wFIRMATAR = ta4NOT
026700140127     C                   endif
026800140308     C*
026900140308     C* Reperisco, se c'è, il referente consegna ed il suo telefono
027000140308     C                   eval      ar5TRD = 'GEN'
027100140308     C     KEYar531_1P   chain     fiar531C
027200140308     C                   if        %found(FIAR531C)
027300140308     C                   eval      dar5gen = ar5UNI
027400140308     C                   endif
027500140308     C*
027600140308     C* Reperisco, se c'è, il cellulare del destinatario
027700140308     C                   eval      ar5TRD = 'EMD'
027800140308     C     KEYar531_1P   chain     fiar531C
027900140308     C                   if        %found(FIAR531C)
028000140308     C                   eval      dar5emd = ar5UNI
028100140308     C                   endif
028200071210     C*
028300071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
028400140308     C                   eval      wDSP     =
028500140308     C                              UBFMTDATE_Convert(
028600140308     C                                  %editc((tasAAS*10000+tasMGS):'4') :
028700140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
028800140127     C                   eval      wIND     = tasIND
028900140127     C                   eval      wCAD     = tasCAD
029000140127     C                   eval      wLOD     = tasLOD
029100140127     C                   eval      wPRD     = tasPRD
029200140127     C                   eval      wNCD     = tasNCD
029300140127     C                   eval      wNCA     = tasNCA
029400140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
029500140127     C                                        tasNRS*10000000+
029600140127     C                                        tasNSP
029700121207     C*
029800121207     C* Reperisco i dati del mittente originale
029900140308     C***                eval      taaTRC = 'O'
030000140308     C***      aa30      chain     titaa30c
030100140308     C***                if        %found(titaa30c)
030200140308     C***                eval      wRSCMORI = taaRSC
030300140308     C***                eval      wINDMORI = taaIND
030400140308     C***                eval      wCAPMORI = taaCAP
030500140308     C***                eval      wLOCMORI = taaLOC
030600140308     C***                eval      wPRVMORI = taaPRV
030700140308     C***                endif
030800130913     C*
030900130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
031000130913     C* (costruisco la chiave sul file TIGCP51L)
031100130913     C*
031200140310     C                   EVAL      gcpDUR = *zeros
031300130913     C                   EVAL      gcpFRG = *zeros
031400130913     C     KEYgcp51      SETLL     tigcp51l
031500130913     C                   IF        %equal(tigcp51l)
031600130913     C     KEYgcp51      READE     tigcp51l
031700130913     C                   DOW       not %eof(tigcp51l)
031800130913     C*
031900130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
032000130913     C                   IF        gcpDUR > *zeros AND
032100130913     C                             gcpDDM = *zeros AND
032200130913     C                             gcpFAS < 030
032300130913     C                   seton                                        70
032400130913     C                   ENDIF
032500140311     C*
032600140311     C* Reperisco la causale di ultima apertura giacenza
032700140311     C                   EVAL      wGCPCMC = gcpCMC
032800130913     C*
032900130913     C* Proseguo la lettura delle giacenze x la bolla corrente
033000130913     C     KEYgcp51      READE     tigcp51l
033100130913     C                   ENDDO
033200140308     C*
033300140308     C* Se in giacenza reperisco le note di giacenza
033400140308     C                   EXSR      rtvNOTEGIAC
033500140308     C*
033600140308     C                   ENDIF
033700071213     C*
033800071213     C                   endif
033900071121     C*
034000071121     C* Compondo la stringa dati output relativa al record corrente
034100071121     C                   EXSR      wrifile
034200100910     C*
034300100910     C                   endif
034400040719     C*
034500040719     C* Proseguo con la lettura dell'input
034600080924     C                   READ      TIVGDTMP
034700060119     C*
034800030325     C                   ENDDO
034900071121     C*
035000071121     C* Head finali della struttura HTML
035100071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
035200071210     C***                WRITE     TIVAW000
035300991027     C*
035400030325     C                   EVAL      wrkesito = '0'
035500991027     C*
035600910830     C                   ENDSR
035700100907
035800100907
035900100907
036000100907     C     calSTS        BEGSR
036100100910     C*
036200120926     C                   MOVEL     *blanks       wMIC              1
036300121114     C                   MOVEL     *blanks       wGIAC             1
036400170222     C                   MOVEL     *blanks       wATTESA           1
036500121207     C                   MOVEL     *blanks       wLAVV             1
036600140129     C                   MOVEL     *blanks       wPEND             1
036700100910     C                   EVAL      wDataSts       = vacDCM
036800140308     C*
036900140308     C* Verifico lo stato delle disposizioni di giacenza
037000140308     C                   MOVEL     *blanks       wGiacDisp        10
037100140308     C   70              EVAL      wGiacDisp = 'IN ATTESA'
037200170222     C   70              EVAL      wATTESA   = 'S'
037300100907     C*
037400100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
037500100907     C                   IF        VACDCM > *zeros AND
037600100907     C                             (VACCCA = *blanks OR VACCCA = '1')
037700100907     C                   EVAL      wConsOkSiNo    = 'S'
037800170222     C  N56              EVAL      wConsOkSiNoDes = 'Consegnata'
037900170222     C   56              EVAL      wConsOkSiNoDes = 'Delivered'
038000140613     C                   EVAL      wConsOkSiNoCod = '000'
038100140308     C                   EVAL      wGiacDisp      = *blanks
038200140308     C                   EVAL      wNoteGiac      = *blanks
038300100907     C                   ELSE
038400100907     C                   EVAL      wConsOkSiNo    = 'N'
038500140311     C                   EVAL      wFIRMATAR      = *blanks
038600100907     C*
038700100907     C* A questo punto stabilisco la descrizione
038800100910     C                   SELECT
038900100910     C                   WHEN      vacCCA = '1'
039000170222     C  N56              EVAL      wConsOkSiNoDes = 'Dirottata'
039100170222     C   56              EVAL      wConsOkSiNoDes = 'Diverted'
039200140613     C                   EVAL      wConsOkSiNoCod = '002'
039300100910     C                   WHEN      vacCCA = '2'
039400170222     C  N56              EVAL      wConsOkSiNoDes = 'Resa'
039500170222     C   56              EVAL      wConsOkSiNoDes = 'Returned'
039600140613     C                   EVAL      wConsOkSiNoCod = '003'
039700100907     C                   EVAL      wConsOkSiNo    = 'R'
039800100910     C                   WHEN      vacCCA = '3'
039900170222     C  N56              EVAL      wConsOkSiNoDes = 'Rapinata'
040000170222     C   56              EVAL      wConsOkSiNoDes = 'Stolen (goods)'
040100140613     C                   EVAL      wConsOkSiNoCod = '009'
040200110519     C                   EVAL      wConsOkSiNo    = 'R'
040300100910     C                   WHEN      vacCCA = '4'
040400170222     C  N56              EVAL      wConsOkSiNoDes = 'Rapinato incasso'
040500170222     C   56              EVAL      wConsOkSiNoDes = 'Stolen (money)'
040600110519     C                   EVAL      wConsOkSiNo    = 'R'
040700100910     C                   WHEN      vacCCA = '5'
040800170222     C  N56              EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
040900170222     C   56              EVAL      wConsOkSiNoDes = 'Dossier closed'
041000140613     C                   EVAL      wConsOkSiNoCod = '010'
041100110519     C                   EVAL      wConsOkSiNo    = 'R'
041200100910     C                   WHEN      vacCCA = '6'
041300170222     C  N56              EVAL      wConsOkSiNoDes = 'Resa da danno'
041400170222     C   56              EVAL      wConsOkSiNoDes = 'Returned (after damage)'
041500140613     C                   EVAL      wConsOkSiNoCod = '008'
041600110519     C                   EVAL      wConsOkSiNo    = 'R'
041700100910     C                   WHEN      vacCCA = 'A'
041800170222     C  N56              EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
041900170222     C   56              EVAL      wConsOkSiNoDes = 'Dossier opened'
042000140613     C                   EVAL      wConsOkSiNoCod = '011'
042100110519     C                   EVAL      wConsOkSiNo    = 'R'
042200100910     C                   WHEN      vacCCA = 'C'
042300170222     C  N56              EVAL      wConsOkSiNoDes = 'In consegna'
042400170222     C   56              EVAL      wConsOkSiNoDes = 'In delivery'
042500140613     C                   EVAL      wConsOkSiNoCod = '001'
042600120926     C                   EVAL      wMIC = 'S'
042700100910     C                   WHEN      vacCCA = 'R'
042800170222     C  N56              EVAL      wConsOkSiNoDes = 'Ripristinata'
042900170222     C   56              EVAL      wConsOkSiNoDes = 'Restored'
043000140613     C                   EVAL      wConsOkSiNoCod = '013'
043100100910     C                   WHEN      vacCCA = 'S'
043200170222     C  N56              EVAL      wConsOkSiNoDes = 'Merce distrutta'
043300170222     C   56              EVAL      wConsOkSiNoDes = 'Goods destroyed'
043400140613     C                   EVAL      wConsOkSiNoCod = '012'
043500110519     C                   EVAL      wConsOkSiNo    = 'R'
043600100910     C                   ENDSL
043700100907     C*
043800140308     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
043900100910     C                   EVAL      wDataSts       = vacDLA
044000170222     C  N56              EVAL      wConsOkSiNoDes = 'Lasciato avviso'
044100170222     C   56              EVAL      wConsOkSiNoDes = 'Deliv. attemped notice'
044200140613     C                   EVAL      wConsOkSiNoCod = '004'
044300121207     C                   EVAL      wLAVV = 'S'
044400100907     C                   ENDIF
044500140308     C                   IF        (gcpDUR > vacDCM AND gcpDUR >= vacDLA)
044600140308     C                   EVAL      wDataSts       = gcpDUR
044700170222     C  N56              EVAL      wConsOkSiNoDes = 'In giacenza'
044800170222     C   56              EVAL      wConsOkSiNoDes = 'On-Hold'
044900140613     C                   EVAL      wConsOkSiNoCod = '005'
045000140311     C                   EXSR      rtvCAU_GIAC
045100121114     C                   EVAL      wGIAC = 'S'
045200140308     C                   ELSE
045300140308     C                   EVAL      wGiacDisp      = *blanks
045400140308     C                   EVAL      wNoteGiac      = *blanks
045500100907     C                   ENDIF
045600100910     C                   IF        vacDCM = *zeros AND
045700100910     C                             vacDLA = *zeros AND
045800140308     C                             gcpDUR = *zeros AND
045900100910     C                             vacCCA = *blanks
046000100910     C                   EVAL      wDataSts       = datcor
046100100910     C*
046200100910     C* Verifico la data consegna richiesta
046300100910     C                   IF        vacDCR > datcor
046400170222     C  N56              EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
046500140308     C                              UBFMTDATE_Convert(
046600140308     C                                  %editc(vacDCR:'4') :
046700140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
046800170222     C   56              EVAL      wConsOkSiNoDes = 'Delivery requested on ' +
046900170222     C                              UBFMTDATE_Convert(
047000170222     C                                  %editc(vacDCR:'4') :
047100170222     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
047200140613     C                   EVAL      wConsOkSiNoCod = '006'
047300100910     C                   ELSE
047400170222     C  N56              EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
047500170222     C   56              EVAL      wConsOkSiNoDes = 'In progress'
047600140613     C                   EVAL      wConsOkSiNoCod = '007'
047700140129     C                   EVAL      wPEND          = 'S'
047800100910     C                   ENDIF
047900100910     C                   ENDIF
048000100907     C*
048100100907     C                   ENDIF
048200100907     C*
048300100907     C                   ENDSR
048400140311
048500140311
048600140311
048700140311     C     rtvCAU_GIAC   BEGSR
048800140311     C*
048900140311     C* Rererisco la descrizioe della causale di giacenza
049000140311     C                   MOVEL     '1'           tblKUT
049100140311     C                   MOVEL     '2A'          tblCOD
049200140311     C                   MOVEL(P)  wGCPCMC       tblKEY
049300140311     C     KEYtab00_C    CHAIN     TABEL00F                           30
049400140311     C                   IF        %found(tabel00f)
049500140311     C                   MOVEL     TBLUNI        ds2A
049600140311     C                   MOVEL(P)  §2ADES        wGiacCau
049700140311     C                   ENDIF
049800140311     C*
049900140311     C                   ENDSR
050000140311     C*------------------------------------------------------------------------*
050100140311
050200140308
050300140308
050400140308     C     rtvNOTEGIAC   BEGSR
050500140308     C*
050600140308     C* Scorro tutte le note giacenza relative alla giacebza corrente
050700140308     C     KEYgnp01_P    setll     tignp01l
050800140308     C                   if        %equal(tignp01l)
050900140308     C                   eval      wNoteGiac = ''''
051000140308     C     KEYgnp01_P    reade     tignp01l
051100140308     C                   dow       not %eof(tignp01l)
051200140308     C*** concateno note giacenza
051300140308     C                   eval      wNoteGiac =
051400140308     C                             %trim(wNoteGiac) + ' ' + gnpDMC
051500140308     C     KEYgnp01_P    reade     tignp01l
051600140308     C                   enddo
051700140308     C                   endif
051800140308     C*
051900140308     C                   ENDSR
052000100907
052100100907
052200100907
052300100907     C     makLNK        BEGSR
052400130426     C*
052500130426     C* Se richiesta inclusione link T&T
052600130426     C                   if        %subst(prmppt:6:7) <> *blanks
052700130426     C*
052800130426     C* Porto in minuscolo il parametro relativo alla lingua del link
052900130426     C                   movel     *blanks       parLang           2
053000130426     C                   eval      parLang = %subst(prmppt:13:2)
053100130426     C     maiu:minu     xlate     parLang       parLang
053200130426     C*
053300160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
053400130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
053500130426     C                                        %trim(%char(vacRMN))             +
053600130426     C                                        '&ksu='+%subst(prmppt:6:7)+
053700130426     C                                        '&lang='+parLang
053800130426     C*
053900130426     C                   endif
054000100907     C*
054100100907     C                   ENDSR
054200100907
054300071121
054400071121
054500071121
054600071121     C     wrifile       BEGSR
054700120926     C*
054800120926     C                   SETOFF                                       31
054900071121     C*
055000100907     C* Determino lo status della spedizione
055100100907     C                   EXSR      calSTS
055200100907     C*
055300100907     C* Costruisco il link diretto
055400130426     C                   EXSR      makLNK
055500130426     C*
055600130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
055700130426     C                   IF        %subst(prmppt:15:1) = 'P'
055800130426     C                   MOVE      vacCCM        CCM               4 0
055900130426     C                   MOVE      wKSC          KSC               4 0
056000130426     C                   IF        CCM =  8888 or  CCM =  9999
056100130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
056200130426     C                   EVAL      vacCCM = wKSC
056300130426     C                   ENDIF
056400130426     C                   ENDIF
056500130426     C                   ENDIF
056600100908     C*
056700140308     C                   EVAL      wDataStsAlfa =
056800140308     C                              UBFMTDATE_Convert(
056900140308     C                                  %editc(wDataSts:'4') :
057000140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
057100140308     C*
057200100908     C                   SELECT
057300100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
057400071121     C                   EVAL      wBufferOut = iRow +
057500100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
057600140308     C                                  iCol+%trim(wDSP)                  +fCol+
057700110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
057800100907     C                                  iCol+%trim(vacRSD)                +fCol+
057900140308     C                                  iCol+%trim(§AR5TEL)               +fCol+
058000140308     C                                  iCol+%trim(§AR5REF)               +fCol+
058100110519     C                                  iCol+%trim(wIND)                  +fCol+
058200100517     C                                  iCol+%trim(wCAD)                  +fCol+
058300100517     C                                  iCol+%trim(wLOD)                  +fCol+
058400110519     C                                  iCol+%trim(wPRD)                  +fCol+
058500100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
058600110519     C                                  iCol+%trim(vacRMA)                +fCol+
058700140308     C                                  iCol+%trim(wDataStsAlfa)          +fCol+
058800100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
058900140613     C                                  iCol+%trim(wConsOkSiNoCod)        +fCol+
059000130913     C                                  iCol+%trim(wGiacDisp)             +fCol+
059100140311     C                                  iCol+%trim(wGiacCau)              +fCol+
059200140308     C                                  iCol+%trim(wNoteGiac)             +fCol+
059300140127     C                                  iCol+%trim(%editc(wSPEDBRT:'X'))  +fCol+
059400140129     C                                  iCol+%trim(wFIRMATAR)             +fCol
059500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
059600100908     C                   EVAL      wBufferOut =
059700110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
059800140308     C                              +'"'+   %trim(wDSP)                   +'";'
059900110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
060000110519     C                              +'"'+   %trim(vacRSD)                 +'";'
060100140308     C                              +'"'+   %trim(§AR5TEL)                +'";'
060200140308     C                              +'"'+   %trim(§AR5REF)                +'";'
060300110519     C                              +'"'+   %trim(wIND)                   +'";'
060400110519     C                              +'"'+   %trim(wCAD)                   +'";'
060500110519     C                              +'"'+   %trim(wLOD)                   +'";'
060600110519     C                              +'"'+   %trim(wPRD)                   +'";'
060700110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
060800110519     C                              +'"'+   %trim(vacRMA)                 +'";'
060900140308     C                              +'"'+   %trim(wDataStsAlfa)           +'";'
061000110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
061100140613     C                              +'"'+   %trim(wConsOkSiNoCod)         +'";'
061200130913     C                              +'"'+   %trim(wGiacDisp)              +'";'
061300140311     C                              +'"'+   %trim(wGiacCau)               +'";'
061400140308     C                              +'"'+   %trim(wNoteGiac)              +'";'
061500140127     C                              +'"'+   %trim(%editc(wSPEDBRT:'X'))   +'";'
061600140129     C                              +'"'+   %trim(wFIRMATAR)              +'";'
061700100908     C                   ENDSL
061800071121     C*
061900120926     C* Se nei parametri è richiesto no MIC => gestisco
062000120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
062100120926     C                   SETON                                        31
062200120926     C                   ENDIF
062300121114     C*
062400121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
062500121207     C                   IF        %subst(prmppt:4:1) = 'S' and
062600121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
062700121114     C                   SETON                                        31
062800121114     C                   ENDIF
062900170222     C*
063000170222     C* Se nei parametri è richiesto solo GIACIENZE in attesa di disposizioni => gestisco
063100170222     C                   IF        %subst(prmppt:4:1) = 'A' and
063200170222     C                             wATTESA <> 'S'
063300170222     C                   SETON                                        31
063400170222     C                   ENDIF
063500140124     C*
063600140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
063700140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
063800140124     C                   SETON                                        31
063900140124     C                   ENDIF
064000120926     C*
064100071121     C                   EVAL      VAWDTA = wBufferOut
064200110519     C*
064300110519     C* Restituisco solo le spedizioni "pendenti"
064400110713     C***                IF        wConsOkSiNo = 'N'
064500120926     C  N31              WRITE     TIVAW000
064600110713     C***                ENDIF
064700071121     C*
064800071121     C                   ENDSR
064900071121
065000071121
065100071121
065200991027
065300991027      /TITLE Operazioni iniziali.
065400991027     C     *inzsr        BEGSR
065500991027     C*
065600991027     C     *ENTRY        PLIST
065700991027     C                   parm                    prmppt
065800991027     C     wrkesito      parm      wrkesito      prmesito
065900100910     C*
066000100910     C* CALCOLA LA DATA CORRENTE
066100100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
066200100910     C                   eval      datcor = %dec(%date() : *ISO)
066300071210     C*
066400071213     C* TITAS30C - Parziale
066500071210     C     KEYtas30      KLIST
066600071210     C                   KFLD                    vacAAS
066700071210     C                   KFLD                    vacLNP
066800071210     C                   KFLD                    vacNRS
066900071210     C                   KFLD                    vacNSP
067000110519     C*
067100110519     C* TITA430C - Completa
067200140129     C     KEYta430C     KLIST
067300110519     C                   KFLD                    vacAAS
067400110519     C                   KFLD                    vacLNP
067500110519     C                   KFLD                    vacNRS
067600110519     C                   KFLD                    vacNSP
067700110519     C                   KFLD                    ta4TRC
067800121207     C*
067900121207     C* TITAA30C - Completa
068000121207     C     KEYtaa30      KLIST
068100121207     C                   KFLD                    vacAAS
068200121207     C                   KFLD                    vacLNP
068300121207     C                   KFLD                    vacNRS
068400121207     C                   KFLD                    vacNSP
068500121207     C                   KFLD                    taaTRC
068600130913     C*
068700130913     C* TIGCP51L - Completa
068800130913     C     KEYgcp51      KLIST
068900130913     C                   KFLD                    vacAAS
069000130913     C                   KFLD                    vacLNP
069100130913     C                   KFLD                    vacNRS
069200130913     C                   KFLD                    vacNSP
069300130913     C                   KFLD                    gcpFRG
069400140308     C*
069500140308     C* Chiave su FIAR531C - Parziale
069600140308     C     KEYar531_1P   KLIST
069700140308     C                   KFLD                    vacAAS
069800140308     C                   KFLD                    vacLNP
069900140308     C                   KFLD                    vacNRS
070000140308     C                   KFLD                    vacNSP
070100140308     C                   KFLD                    ar5TRD
070200140308     C*
070300140308     C* Chiave su TIGNP01L - Parziale
070400140308     C     KEYgnp01_P    KLIST
070500140308     C                   KFLD                    gcpAGC
070600140308     C                   KFLD                    gcpFGC
070700140308     C                   KFLD                    gcpNGC
070800140308     C                   KFLD                    gcpFRG
070900140311     C*
071000140311     C* TABEL00F - Completa
071100140311     C     KEYtab00_C    KLIST
071200140311     C                   KFLD                    tblKUT
071300140311     C                   KFLD                    tblCOD
071400140311     C                   KFLD                    tblKEY
071500991027     C*
071600991027     C                   ENDSR
071700071121     C*
071800071121** DEC - COSTANTI
071900071121<?xml version="1.0"?>
072000071121<?mso-application progid="Excel.Sheet"?>
072100071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
072200071121xmlns:o="urn:schemas-microsoft-com:office:office"
072300071121xmlns:x="urn:schemas-microsoft-com:office:excel"
072400071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
072500071121xmlns:html="http://www.w3.org/TR/REC-html40">
072600071121</Workbook>
072700071121<Worksheet ss:Name="PAG1">
072800071121</Worksheet>
072900071121<Table border="1">
073000071121</Table>
073100071121<Row>
073200071121</Row>
073300071121<Cell><Data ss:Type="String">
073400071121<Cell><Data ss:Type="Number">
073500071121</Data></Cell>
073600071121
073700071121
073800071121
