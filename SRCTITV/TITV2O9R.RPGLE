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
001100121207     FTITAA30C  IF   E           K DISK
001200030924
001300080924     D fnvac00t      e ds
001400071121
001500071121     D*------------
001600071121     D* DECODIFICHE COSTANTI
001700071121     D*------------
001800071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
001900071121
002000071121
002100071121     D*------------
002200071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
002300071121     D*------------
002400071121     D iHtml           s             10    inz('<html>')
002500071121     D fHtml           s             10    inz('</html>')
002600071121     D iBody           s             10    inz('<body>')
002700071121     D fBody           s             10    inz('</body>')
002800071121     D iTable          s             20    inz('<table border="1">')
002900071121     D fTable          s             10    inz('</table>')
003000071121     D iThead          s             10    inz('<thead>')
003100071121     D fThead          s             10    inz('</thead>')
003200071121     D iRow            s              5    inz('<tr>')
003300071121     D fRow            s              5    inz('</tr>')
003400071121     D iCol            s              5    inz('<td>')
003500071121     D fCol            s              5    inz('</td>')
003600071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
003700071121     D fColBold        s              5    inz('</th>')
003800071122     D cApi            s              1    inz('''')
003900071121
004000071121
004100071121     D*------------
004200071121     D* VARIABILI D WRK
004300071121     D*------------
004400030924     D prmppt          S             50
004500030924     D prmesito        S              1
004600030924     D wrkesito        S                   like(prmesito)
004700071122     D wBufferOut      s          10000    varying inz
004800100910     D wDataSts        S              8  0 inz(*zeros)
004900100907     D wConsOkSiNo     S              1    inz(*blanks)
005000100907     D wConsOkSiNoDes  S            300    inz(*blanks)
005100100907     D wLinkDir        S            300    inz(*blanks)
005200110519     D wDSP            s             10    inz(*blanks)
005300110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
005400110519     D wIND            s                   inz(*blanks) like(tasIND)
005500110519     D wCAD            s                   inz(*blanks) like(tasIND)
005600110519     D wLOD            s                   inz(*blanks) like(tasLOD)
005700110519     D wPRD            s                   inz(*blanks) like(tasPRD)
005800121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
005900121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
006000121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
006100121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
006200121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
006300130409     D wKSC            s              7  0 inz(*zeros)
006400071211     D*
006500100910
006600100910
006700100910     D*------------------
006800100910     D* LINKING A DEFINIZIONI ESTERNE
006900100910     D*------------------
007000100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007100100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007200100910
007300130409
007400130409     D*-------------------
007500130409     D* COSTANTI
007600130409     D*-------------------
007700130409     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007800130409     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007900130409
008000030924
008100030325
008200030924     C* MAIN/
008300071121     C*
008400071121     C* Verifico se già presenti dati nel file d output
008500071121     C                   EXSR      chkRECFILMBR
008600071121     C*
008700071121     C* Effettuo elaborazione
008800071121     C                   EXSR      traduci
008900071121     C*
009000921023     C                   SETON                                        LR
009100991027
009200071121
009300071121
009400071121     C     chkRECFILMBR  BEGSR
009500071121     C*
009600071121     C                   READ      TIVAWWWT                               55
009700071121     C*
009800071121     C                   ENDSR
009900071121
010000071121
010100030924
010200991027     C     traduci       BEGSR
010300071121     C*
010400071121     C* Head iniziali della struttura HTML
010500100908     C                   IF        %subst(prmppt:2:1) = 'H'
010600071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
010700071210     C   55              WRITE     TIVAW000
010800100908     C                   ENDIF
010900071121     C*
011000071121     C* Inizializzazone formato e variabili x output
011100071121     C                   clear                   TIVAW000
011200071121     C                   clear                   wBufferOut
011300100907     C                   clear                   wLinkDir
011400100907     C                   clear                   wConsOkSiNo
011500100907     C                   clear                   wConsOkSiNoDes
011600130409     C*
011700130409     C* Verifico la lingua richiesta
011800130409     C                   setoff                                       505152
011900130409     C                   select
012000130409     C                   when      %subst(prmppt:1:1) = *blanks
012100130409     C                   setoff                                       50
012200130409     C                   when      %subst(prmppt:1:1) = 'I'
012300130409     C                   seton                                        50
012400130409     C                   seton                                        51
012500130409     C                   when      %subst(prmppt:1:1) = 'E'
012600130409     C                   seton                                        50
012700130409     C                   seton                                        52
012800130409     C                   endsl
012900071121     C*
013000071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013100130409     C                   IF        *in50 AND *IN55 = *ON
013200100908     C*
013300100908     C                   SELECT
013400100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
013500130409     C   51              EVAL      wBufferOut = iThead + iRow
013600130409     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
013700130409 xxx C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
013800130409     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
013900130409     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
014000130409     C                             +iColBold+ 'STATO'                  +fColBold
014100130409 xxx C                             +iColBold+ 'DATA STATO'             +fColBold
014200130409     C                             +iColBold+ 'LINEA PARTENZA'         +fColBold
014300110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
014400121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
014500130409 xxx C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
014600110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
014700110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
014800110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
014900110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
015000121207     C                             +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
015100121207     C                             +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
015200121207     C                             +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
015300121207     C                             +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
015400121207     C                             +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
015500130409     C                             +iColBold+ 'LINK'                   +fColBold
015600130409     C   52              EVAL      wBufferOut = iThead + iRow
015700130409     C                             +iColBold+ 'CUSTOMER CODE'          +fColBold
015800130409 xxx C                             +iColBold+ 'SHIPMENT DATE'          +fColBold
015900130409     C                             +iColBold+ '1 SHIPMENT REFERENCE'   +fColBold
016000130409     C                             +iColBold+ '2 SHIPMENT REFERENCE'   +fColBold
016100130409     C                             +iColBold+ 'STATUS'                 +fColBold
016200130409 xxx C                             +iColBold+ 'STATUS DATE'            +fColBold
016300130409     C                             +iColBold+ 'DEPARTURE DEPOT'        +fColBold
016400130409     C                             +iColBold+ 'ARRIVAL DEPOT'          +fColBold
016500130409     C                             +iColBold+ 'CONSIGNEE NAME'         +fColBold
016600130409 xxx C                             +iColBold+ 'CONSIGNEE PHONE'        +fColBold
016700130409     C                             +iColBold+ 'CONSIGNEE ADDRESS'      +fColBold
016800130409     C                             +iColBold+ 'CONSIGNEE ZIPCODE'      +fColBold
016900130409     C                             +iColBold+ 'CONSIGNEE CITY'         +fColBold
017000130409     C                             +iColBold+ 'CONSIGNEE AREA'         +fColBold
017100130409     C                             +iColBold+ 'ORIGINAL SENDER NAME'   +fColBold
017200130409     C                             +iColBold+ 'ORIGINAL SENDER ADDRESS'+fColBold
017300130409     C                             +iColBold+ 'ORIGINAL SENDER ZIPCODE'+fColBold
017400130409     C                             +iColBold+ 'ORIGINAL SENDER CITY'   +fColBold
017500130409     C                             +iColBold+ 'ORIGINAL SENDER AREA'   +fColBold
017600130409     C                             +iColBold+ 'LINK'                   +fColBold
017700100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
017800130409     C   51              EVAL      wBufferOut =
017900130409     C                              '"'+      'CODICE CLIENTE'         +'";'
018000130409 xxx C                             +'"'+      'DATA SPEDIZIONE'        +'";'
018100130409     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
018200130409     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
018300130409     C                             +'"'+      'STATO'                  +'";'
018400130409 xxx C                             +'"'+      'DATA STATO'             +'";'
018500130409     C                             +'"'+      'LINEA PARTENZA'         +'";'
018600110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
018700110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
018800130409 xxx C                             +'"'+      'TEL. DESTINATARIO'      +'";'
018900110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
019000110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
019100110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
019200110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
019300121207     C                             +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
019400121207     C                             +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
019500121207     C                             +'"'+      'CAP MITTENTE ORIG.'     +'";'
019600121207     C                             +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
019700121207     C                             +'"'+      'PROV. MITTENTE ORIG.'   +'";'
019800130409     C                             +'"'+      'LINK'                   +'";'
019900130409     C   52              EVAL      wBufferOut =
020000130409     C                              '"'+      'CUSTOMER CODE'          +'";'
020100130409 xxx C                             +'"'+      'SHIPMENT DATE'          +'";'
020200130409     C                             +'"'+      '1 SHIPMENT REFERENCE'   +'";'
020300130409     C                             +'"'+      '2 SHIPMENT REFERENCE'   +'";'
020400130409     C                             +'"'+      'STATUS'                 +'";'
020500130409 xxx C                             +'"'+      'STATUS DATE'            +'";'
020600130409     C                             +'"'+      'DEPARTURE DEPOT'        +'";'
020700130409     C                             +'"'+      'ARRIVAL DEPOT'          +'";'
020800130409     C                             +'"'+      'CONSIGNEE NAME'         +'";'
020900130409 xxx C                             +'"'+      'CONSIGNEE PHONE'        +'";'
021000130409     C                             +'"'+      'CONSIGNEE ADDRESS'      +'";'
021100130409     C                             +'"'+      'CONSIGNEE ZIPCODE'      +'";'
021200130409     C                             +'"'+      'CONSIGNEE CITY'         +'";'
021300130409     C                             +'"'+      'CONSIGNEE AREA'         +'";'
021400130409     C                             +'"'+      'ORIGINAL SENDER NAME'   +'";'
021500130409     C                             +'"'+      'ORIGINAL SENDER ADDRESS'+'";'
021600130409     C                             +'"'+      'ORIGINAL SENDER ZIPCODE'+'";'
021700130409     C                             +'"'+      'ORIGINAL SENDER CITY'   +'";'
021800130409     C                             +'"'+      'ORIGINAL SENDER AREA'   +'";'
021900130409     C                             +'"'+      'LINK'                   +'";'
022000100908     C                   ENDSL
022100100908     C*
022200071121     C                   EVAL      VAWDTA = wBufferOut
022300071121     C                   WRITE     TIVAW000
022400071121     C                   ENDIF
022500991027     C*
022600080924     C                   READ      TIVGDTMP
022700060119     C*
022800080924     C                   DOW       not %eof(TIVGDTMP)
022900080924     C*
023000080924     C                   eval      fnvac00t = vgdDTA
023100071121     C*
023200071121     C                   clear                   TIVAW000
023300071121     C                   clear                   wBufferOut
023400100907     C                   clear                   wLinkDir
023500100907     C                   clear                   wConsOkSiNo
023600100907     C                   clear                   wConsOkSiNoDes
023700100910     C*
023800100910     C* Solo bolle originali e valide ai fini della spedizione
023900100910     C                   movel     *blanks       wEsito1           1
024000100910     C*
024100100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
024200100910     C                                                vacAAS
024300100910     C                                               :vacLNP
024400100910     C                                               :vacNRS
024500100910     C                                               :vacNSP
024600100910     C                                               :pOutLblTyp
024700100910     C                                               :pOutAnnoBO
024800100910     C                                               :pOutLineaParBO
024900100910     C                                               :pOutSerieBO
025000100910     C                                               :pOutNumSpedBO
025100100910     C                                               :pOutDcmBO
025200100910     C                                               :pOutCcaBO
025300100910     C                                               :pOutRblBO))
025400100910     C*
025500130409     C                   if         pOutRblBO ='N' AND
025600130409     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
025700071210     C*
025800071210     C* Inizializzo campi d wrk
025900121207     C                   EVAL      wDSP     = *blanks
026000121207     C                   EVAL      wRD2     = *blanks
026100121207     C                   EVAL      wIND     = *blanks
026200121207     C                   EVAL      wCAD     = *blanks
026300121207     C                   EVAL      wLOD     = *blanks
026400121207     C                   EVAL      wPRD     = *blanks
026500121207     C                   EVAL      wRSCMORI = *blanks
026600121207     C                   EVAL      wINDMORI = *blanks
026700121207     C                   EVAL      wCAPMORI = *blanks
026800121207     C                   EVAL      wLOCMORI = *blanks
026900121207     C                   EVAL      wPRVMORI = *blanks
027000121207     C*
027100071210     C* Aggancio la bolla relativa x il completamento dei dati
027200071210     C     KEYtas30      chain     titas30c
027300071210     C                   if        %found(titas30c)
027400110519     C*
027500121207     C* Reperisco l'estensione ragione sociale destinatario
027600110519     C                   eval      ta4TRC = 'D'
027700110519     C     KEYta430      chain     tita430c
027800110519     C                   if        %found(tita430c)
027900130409     C                   eval      wRD2 = ta4NOT
028000110519     C                   endif
028100071210     C*
028200071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
028300110519     C                   eval      wDSP = %editc((tasAAS*10000+tasMGS):'X')
028400110519     C                   eval      wIND = tasIND
028500110519     C                   eval      wCAD = tasCAD
028600110519     C                   eval      wLOD = tasLOD
028700110519     C                   eval      wPRD = tasPRD
028800121207     C*
028900121207     C* Reperisco i dati del mittente originale
029000121207     C                   eval      taaTRC = 'O'
029100121207     C     KEYtaa30      chain     titaa30c
029200121207     C                   if        %found(titaa30c)
029300121207     C                   eval      wRSCMORI = taaRSC
029400121207     C                   eval      wINDMORI = taaIND
029500121207     C                   eval      wCAPMORI = taaCAP
029600121207     C                   eval      wLOCMORI = taaLOC
029700121207     C                   eval      wPRVMORI = taaPRV
029800121207     C                   endif
029900071213     C*
030000071213     C                   endif
030100071121     C*
030200071121     C* Compondo la stringa dati output relativa al record corrente
030300071121     C                   EXSR      wrifile
030400100910     C*
030500100910     C                   endif
030600040719     C*
030700040719     C* In ogni caso storicizzo e cancello l'input sempre
030800080924     C                   DELETE    TIVGD000
030900040719     C*
031000040719     C* Proseguo con la lettura dell'input
031100080924     C                   READ      TIVGDTMP
031200060119     C*
031300030325     C                   ENDDO
031400071121     C*
031500071121     C* Head finali della struttura HTML
031600071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
031700071210     C***                WRITE     TIVAW000
031800991027     C*
031900030325     C                   EVAL      wrkesito = '0'
032000991027     C*
032100910830     C                   ENDSR
032200100907
032300100907
032400100907
032500100907     C     calSTS        BEGSR
032600100910     C*
032700120926     C                   MOVEL     *blanks       wMIC              1
032800121114     C                   MOVEL     *blanks       wGIAC             1
032900121207     C                   MOVEL     *blanks       wLAVV             1
033000130409     C                   MOVEL     *blanks       wCONS             1
033100100910     C                   EVAL      wDataSts       = vacDCM
033200100907     C*
033300100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
033400100907     C                   IF        VACDCM > *zeros AND
033500100907     C                             (VACCCA = *blanks OR VACCCA = '1')
033600100907     C                   EVAL      wConsOkSiNo    = 'S'
033700130409     C   51              EVAL      wConsOkSiNoDes = 'Consegnata'
033800130409     C   52              EVAL      wConsOkSiNoDes = 'Delivered'
033900130409     C                   EVAL      wCONS = 'S'
034000100907     C                   ELSE
034100100907     C                   EVAL      wConsOkSiNo    = 'N'
034200100907     C*
034300100907     C* A questo punto stabilisco la descrizione
034400100910     C                   SELECT
034500100910     C                   WHEN      vacCCA = '1'
034600130409     C   51              EVAL      wConsOkSiNoDes = 'Dirottata'
034700130409     C   52              EVAL      wConsOkSiNoDes = 'Diverted'
034800100910     C                   WHEN      vacCCA = '2'
034900130409     C   51              EVAL      wConsOkSiNoDes = 'Resa'
035000130409     C   52              EVAL      wConsOkSiNoDes = 'Returned'
035100100907     C                   EVAL      wConsOkSiNo    = 'R'
035200100910     C                   WHEN      vacCCA = '3'
035300130409     C   51              EVAL      wConsOkSiNoDes = 'Rapinata'
035400130409     C   52              EVAL      wConsOkSiNoDes = 'Goods Stolen'
035500110519     C                   EVAL      wConsOkSiNo    = 'R'
035600100910     C                   WHEN      vacCCA = '4'
035700130409     C   51              EVAL      wConsOkSiNoDes = 'Rapinato incasso'
035800130409     C   52              EVAL      wConsOkSiNoDes = 'Payment Stolen'
035900110519     C                   EVAL      wConsOkSiNo    = 'R'
036000100910     C                   WHEN      vacCCA = '5'
036100130409     C   51              EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
036200130409     C   52              EVAL      wConsOkSiNoDes = 'Goods never entrusted us'
036300110519     C                   EVAL      wConsOkSiNo    = 'R'
036400100910     C                   WHEN      vacCCA = '6'
036500130409     C   51              EVAL      wConsOkSiNoDes = 'Resa da danno'
036600130409     C   52              EVAL      wConsOkSiNoDes = 'Damage and Returned'
036700110519     C                   EVAL      wConsOkSiNo    = 'R'
036800130409     C                   WHEN      vacCCA = '7'
036900130409     C   51              EVAL      wConsOkSiNoDes = 'Merce mai affidata'
037000130409     C   52              EVAL      wConsOkSiNoDes = 'Goods never entrusted us'
037100130409     C                   EVAL      wConsOkSiNo    = 'R'
037200100910     C                   WHEN      vacCCA = 'A'
037300130409     C   51              EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
037400130409     C   52              EVAL      wConsOkSiNoDes = 'New on hold report'
037500110519     C                   EVAL      wConsOkSiNo    = 'R'
037600100910     C                   WHEN      vacCCA = 'C'
037700130409     C   51              EVAL      wConsOkSiNoDes = 'In consegna'
037800130409     C   52              EVAL      wConsOkSiNoDes = 'In delivery'
037900120926     C                   EVAL      wMIC = 'S'
038000100910     C                   WHEN      vacCCA = 'R'
038100130409     C   51              EVAL      wConsOkSiNoDes = 'Ripristinata'
038200130409     C   52              EVAL      wConsOkSiNoDes = 'Reset shipment'
038300100910     C                   WHEN      vacCCA = 'S'
038400130409     C   51              EVAL      wConsOkSiNoDes = 'Merce distrutta'
038500130409     C   52              EVAL      wConsOkSiNoDes = 'Destroyed Goods'
038600110519     C                   EVAL      wConsOkSiNo    = 'R'
038700100910     C                   ENDSL
038800100907     C*
038900100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
039000100910     C                   EVAL      wDataSts       = vacDLA
039100130409     C   51              EVAL      wConsOkSiNoDes = 'Lasciato avviso'
039200130409     C   52              EVAL      wConsOkSiNoDes = 'Delivery attempt failed' +
039300130409     C                                              ' - Left Notice'
039400121207     C                   EVAL      wLAVV = 'S'
039500100907     C                   ENDIF
039600100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
039700100910     C                   EVAL      wDataSts       = vacDAG
039800130409     C   51              EVAL      wConsOkSiNoDes = 'In giacenza'
039900130409     C   52              EVAL      wConsOkSiNoDes = 'On-Hold in stock'
040000121114     C                   EVAL      wGIAC = 'S'
040100100907     C                   ENDIF
040200100910     C                   IF        vacDCM = *zeros AND
040300100910     C                             vacDLA = *zeros AND
040400100910     C                             vacDAG = *zeros AND
040500100910     C                             vacCCA = *blanks
040600100910     C                   EVAL      wDataSts       = datcor
040700100910     C*
040800100910     C* Verifico la data consegna richiesta
040900100910     C                   IF        vacDCR > datcor
041000130409     C   51              EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
041100110519     C                                              'data ' + %editc(vacDCR:'X')
041200130409     C   52              EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
041300130409     C                                              'on ' + %editc(vacDCR:'X')
041400100910     C                   ELSE
041500130409     C   51              EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
041600130409     C   52              EVAL      wConsOkSiNoDes = 'In progress'
041700100910     C                   ENDIF
041800100910     C                   ENDIF
041900100907     C*
042000100907     C                   ENDIF
042100100907     C*
042200100907     C                   ENDSR
042300130409
042400130409
042500130409
042600130409     C     makLNK        BEGSR
042700130409     C*
042800130409     C* Se richiesta inclusione link T&T
042900130409     C                   if        %subst(prmppt:6:7) <> *blanks
043000130409     C*
043100130409     C* Porto in minuscolo il parametro relativo alla lingua del link
043200130409     C                   movel     *blanks       parLang           2
043300130409     C                   eval      parLang = %subst(prmppt:13:2)
043400130409     C     maiu:minu     xlate     parLang       parLang
043500130409     C*
043600160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
043700130409     C                                        'sped_ricdocmit_load.hsm?docmit='+
043800130409     C                                        %trim(%char(vacRMN))             +
043900130409     C                                        '&ksu='+%subst(prmppt:6:7)+
044000130409     C                                        '&lang='+parLang
044100130409     C*
044200130409     C                   endif
044300130409     C*
044400130409     C                   ENDSR
044500100907
044600071121
044700071121
044800071121
044900071121     C     wrifile       BEGSR
045000120926     C*
045100120926     C                   SETOFF                                       31
045200071121     C*
045300100907     C* Determino lo status della spedizione
045400100907     C                   EXSR      calSTS
045500100907     C*
045600100907     C* Costruisco il link diretto
045700130409     C                   EXSR      makLNK
045800130409     C*
045900130409     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
046000130409     C                   IF        %subst(prmppt:15:1) = 'P'
046100130409     C                   MOVE      vacCCM        CCM               4 0
046200130409     C                   MOVE      wKSC          KSC               4 0
046300130409     C                   IF        CCM =  8888 or  CCM =  9999
046400130409     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
046500130409     C                   EVAL      vacCCM = wKSC
046600130409     C                   ENDIF
046700130409     C                   ENDIF
046800130409     C                   ENDIF
046900100908     C*
047000100908     C                   SELECT
047100100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
047200071121     C                   EVAL      wBufferOut = iRow +
047300100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
047400130409 xxx C                                  iCol+%trim(wDSP)                  +fCol+
047500130409     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
047600130409     C                                  iCol+%trim(vacRMA)                +fCol+
047700130409     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
047800130409 xxx C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
047900130409     C                                  iCol+%trim(%editc(vacLNP:'X'))    +fCol+
048000110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
048100130409     C                                  iCol+%trim(vacRSD)+' '+%trim(wRD2)+fCol+
048200130409 xxx C                                  iCol+%trim(' ')                   +fCol+
048300110519     C                                  iCol+%trim(wIND)                  +fCol+
048400100517     C                                  iCol+%trim(wCAD)                  +fCol+
048500100517     C                                  iCol+%trim(wLOD)                  +fCol+
048600110519     C                                  iCol+%trim(wPRD)                  +fCol+
048700121207     C                                  iCol+%trim(wRSCMORI)              +fCol+
048800121207     C                                  iCol+%trim(wINDMORI)              +fCol+
048900121207     C                                  iCol+%trim(wCAPMORI)              +fCol+
049000121207     C                                  iCol+%trim(wLOCMORI)              +fCol+
049100121207     C                                  iCol+%trim(wPRVMORI)              +fCol+
049200130409     C                                  iCol+%trim(wLinkDir)              +fCol
049300100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
049400100908     C                   EVAL      wBufferOut =
049500130409     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
049600130409 xxx C                              +'"'+   %trim(wDSP)                   +'";'
049700130409     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
049800130409     C                              +'"'+   %trim(vacRMA)                 +'";'
049900130409     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
050000130409 xxx C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
050100130409     C                              +'"'+   %trim(%editc(vacLNP:'X'))     +'";'
050200110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
050300130409     C                              +'"'+   %trim(vacRSD)+' '+%trim(wRD2) +'";'
050400130409 xxx C                              +'"'+   %trim(' ')                    +'";'
050500110519     C                              +'"'+   %trim(wIND)                   +'";'
050600110519     C                              +'"'+   %trim(wCAD)                   +'";'
050700110519     C                              +'"'+   %trim(wLOD)                   +'";'
050800110519     C                              +'"'+   %trim(wPRD)                   +'";'
050900121207     C                              +'"'+   %trim(wRSCMORI)               +'";'
051000121207     C                              +'"'+   %trim(wINDMORI)               +'";'
051100121207     C                              +'"'+   %trim(wCAPMORI)               +'";'
051200121207     C                              +'"'+   %trim(wLOCMORI)               +'";'
051300121207     C                              +'"'+   %trim(wPRVMORI)               +'";'
051400130409     C                              +'"'+   %trim(wLinkDir)               +'";'
051500100908     C                   ENDSL
051600071121     C*
051700120926     C* Se nei parametri è richiesto no MIC => gestisco
051800120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
051900120926     C                   SETON                                        31
052000120926     C                   ENDIF
052100121114     C*
052200121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
052300121207     C                   IF        %subst(prmppt:4:1) = 'S' and
052400121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
052500121114     C                   SETON                                        31
052600121114     C                   ENDIF
052700130409     C*
052800130409     C* Se nei parametri è richiesto solo CONSEGNATE o solo NON CONSEGNATE => gestisco
052900130409     C                   IF        %subst(prmppt:5:1) = 'N' and wCONS =  'S'
053000130409     C                   SETON                                        31
053100130409     C                   ENDIF
053200130409     C                   IF        %subst(prmppt:5:1) = 'S' and wCONS <> 'S'
053300130409     C                   SETON                                        31
053400130409     C                   ENDIF
053500120926     C*
053600071121     C                   EVAL      VAWDTA = wBufferOut
053700110519     C*
053800120926     C  N31              WRITE     TIVAW000
053900071121     C*
054000071121     C                   ENDSR
054100071121
054200071121
054300071121
054400991027
054500991027      /TITLE Operazioni iniziali.
054600991027     C     *inzsr        BEGSR
054700991027     C*
054800991027     C     *ENTRY        PLIST
054900991027     C                   parm                    prmppt
055000991027     C     wrkesito      parm      wrkesito      prmesito
055100100910     C*
055200100910     C* CALCOLA LA DATA CORRENTE
055300100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
055400100910     C                   eval      datcor = %dec(%date() : *ISO)
055500071210     C*
055600071213     C* TITAS30C - Parziale
055700071210     C     KEYtas30      KLIST
055800071210     C                   KFLD                    vacAAS
055900071210     C                   KFLD                    vacLNP
056000071210     C                   KFLD                    vacNRS
056100071210     C                   KFLD                    vacNSP
056200110519     C*
056300110519     C* TITA430C - Completa
056400110519     C     KEYta430      KLIST
056500110519     C                   KFLD                    vacAAS
056600110519     C                   KFLD                    vacLNP
056700110519     C                   KFLD                    vacNRS
056800110519     C                   KFLD                    vacNSP
056900110519     C                   KFLD                    ta4TRC
057000121207     C*
057100121207     C* TITAA30C - Completa
057200121207     C     KEYtaa30      KLIST
057300121207     C                   KFLD                    vacAAS
057400121207     C                   KFLD                    vacLNP
057500121207     C                   KFLD                    vacNRS
057600121207     C                   KFLD                    vacNSP
057700121207     C                   KFLD                    taaTRC
057800991027     C*
057900991027     C                   ENDSR
058000071121     C*
058100071121** DEC - COSTANTI
058200071121<?xml version="1.0"?>
058300071121<?mso-application progid="Excel.Sheet"?>
058400071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
058500071121xmlns:o="urn:schemas-microsoft-com:office:office"
058600071121xmlns:x="urn:schemas-microsoft-com:office:excel"
058700071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
058800071121xmlns:html="http://www.w3.org/TR/REC-html40">
058900071121</Workbook>
059000071121<Worksheet ss:Name="PAG1">
059100071121</Worksheet>
059200071121<Table border="1">
059300071121</Table>
059400071121<Row>
059500071121</Row>
059600071121<Cell><Data ss:Type="String">
059700071121<Cell><Data ss:Type="Number">
059800071121</Data></Cell>
059900071121
060000071121
060100071121
