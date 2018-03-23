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
005800130516     D wRSCMITT        s                   inz(*blanks) like(taaRSC)
005900130516     D wINDMITT        s                   inz(*blanks) like(taaIND)
006000130516     D wCAPMITT        s                   inz(*blanks) like(taaCAP)
006100130516     D wLOCMITT        s                   inz(*blanks) like(taaLOC)
006200130516     D wPRVMITT        s                   inz(*blanks) like(taaPRV)
006300121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
006400121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
006500121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
006600121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
006700121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
006800130409     D wKSC            s              7  0 inz(*zeros)
006900071211     D*
007000100910
007100100910
007200100910     D*------------------
007300100910     D* LINKING A DEFINIZIONI ESTERNE
007400100910     D*------------------
007500100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007600100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007700100910
007800130409
007900130409     D*-------------------
008000130409     D* COSTANTI
008100130409     D*-------------------
008200130409     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300130409     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400130409
008500030924
008600030325
008700030924     C* MAIN/
008800071121     C*
008900071121     C* Verifico se già presenti dati nel file d output
009000071121     C                   EXSR      chkRECFILMBR
009100071121     C*
009200071121     C* Effettuo elaborazione
009300071121     C                   EXSR      traduci
009400071121     C*
009500921023     C                   SETON                                        LR
009600991027
009700071121
009800071121
009900071121     C     chkRECFILMBR  BEGSR
010000071121     C*
010100071121     C                   READ      TIVAWWWT                               55
010200071121     C*
010300071121     C                   ENDSR
010400071121
010500071121
010600030924
010700991027     C     traduci       BEGSR
010800071121     C*
010900071121     C* Head iniziali della struttura HTML
011000100908     C                   IF        %subst(prmppt:2:1) = 'H'
011100071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
011200071210     C   55              WRITE     TIVAW000
011300100908     C                   ENDIF
011400071121     C*
011500071121     C* Inizializzazone formato e variabili x output
011600071121     C                   clear                   TIVAW000
011700071121     C                   clear                   wBufferOut
011800100907     C                   clear                   wLinkDir
011900100907     C                   clear                   wConsOkSiNo
012000100907     C                   clear                   wConsOkSiNoDes
012100130409     C*
012200130409     C* Verifico la lingua richiesta
012300130409     C                   setoff                                       505152
012400130409     C                   select
012500130409     C                   when      %subst(prmppt:1:1) = *blanks
012600130409     C                   setoff                                       50
012700130409     C                   when      %subst(prmppt:1:1) = 'I'
012800130409     C                   seton                                        50
012900130409     C                   seton                                        51
013000130409     C                   when      %subst(prmppt:1:1) = 'E'
013100130409     C                   seton                                        50
013200130409     C                   seton                                        52
013300130409     C                   endsl
013400071121     C*
013500071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013600130409     C                   IF        *in50 AND *IN55 = *ON
013700100908     C*
013800100908     C                   SELECT
013900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
014000130409     C   51              EVAL      wBufferOut = iThead + iRow
014100130409     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
014200130409 xxx C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
014300130409     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
014400130409     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
014500130409     C                             +iColBold+ 'STATO'                  +fColBold
014600130409 xxx C                             +iColBold+ 'DATA STATO'             +fColBold
014700130409     C                             +iColBold+ 'LINEA PARTENZA'         +fColBold
014800110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
014900121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
015000130409 xxx C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
015100110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
015200110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
015300110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
015400110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
015500130516     C                             +iColBold+ 'RAG.SOC. MITTENTE'      +fColBold
015600130516     C                             +iColBold+ 'INDIRIZ. MITTENTE'      +fColBold
015700130516     C                             +iColBold+ 'CAP MITTENTE'           +fColBold
015800130516     C                             +iColBold+ 'LOCALITA MITTENTE'      +fColBold
015900130516     C                             +iColBold+ 'PROV. MITTENTE'         +fColBold
016000121207     C                             +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
016100121207     C                             +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
016200121207     C                             +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
016300121207     C                             +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
016400121207     C                             +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
016500130409     C                             +iColBold+ 'LINK'                   +fColBold
016600130409     C   52              EVAL      wBufferOut = iThead + iRow
016700130409     C                             +iColBold+ 'CUSTOMER CODE'          +fColBold
016800130409 xxx C                             +iColBold+ 'SHIPMENT DATE'          +fColBold
016900130409     C                             +iColBold+ '1 SHIPMENT REFERENCE'   +fColBold
017000130409     C                             +iColBold+ '2 SHIPMENT REFERENCE'   +fColBold
017100130409     C                             +iColBold+ 'STATUS'                 +fColBold
017200130409 xxx C                             +iColBold+ 'STATUS DATE'            +fColBold
017300130409     C                             +iColBold+ 'DEPARTURE DEPOT'        +fColBold
017400130409     C                             +iColBold+ 'ARRIVAL DEPOT'          +fColBold
017500130409     C                             +iColBold+ 'CONSIGNEE NAME'         +fColBold
017600130409 xxx C                             +iColBold+ 'CONSIGNEE PHONE'        +fColBold
017700130409     C                             +iColBold+ 'CONSIGNEE ADDRESS'      +fColBold
017800130409     C                             +iColBold+ 'CONSIGNEE ZIPCODE'      +fColBold
017900130409     C                             +iColBold+ 'CONSIGNEE CITY'         +fColBold
018000130409     C                             +iColBold+ 'CONSIGNEE AREA'         +fColBold
018100130516     C                             +iColBold+ 'SENDER NAME'            +fColBold
018200130516     C                             +iColBold+ 'SENDER ADDRESS'         +fColBold
018300130516     C                             +iColBold+ 'SENDER ZIPCODE'         +fColBold
018400130516     C                             +iColBold+ 'SENDER CITY'            +fColBold
018500130516     C                             +iColBold+ 'SENDER AREA'            +fColBold
018600130409     C                             +iColBold+ 'ORIGINAL SENDER NAME'   +fColBold
018700130409     C                             +iColBold+ 'ORIGINAL SENDER ADDRESS'+fColBold
018800130409     C                             +iColBold+ 'ORIGINAL SENDER ZIPCODE'+fColBold
018900130409     C                             +iColBold+ 'ORIGINAL SENDER CITY'   +fColBold
019000130409     C                             +iColBold+ 'ORIGINAL SENDER AREA'   +fColBold
019100130409     C                             +iColBold+ 'LINK'                   +fColBold
019200100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
019300130409     C   51              EVAL      wBufferOut =
019400130409     C                              '"'+      'CODICE CLIENTE'         +'";'
019500130409 xxx C                             +'"'+      'DATA SPEDIZIONE'        +'";'
019600130409     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
019700130409     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
019800130409     C                             +'"'+      'STATO'                  +'";'
019900130409 xxx C                             +'"'+      'DATA STATO'             +'";'
020000130409     C                             +'"'+      'LINEA PARTENZA'         +'";'
020100110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
020200110519     C                             +'"'+      'RAG. SOC. DESTINATARIO' +'";'
020300130409 xxx C                             +'"'+      'TEL. DESTINATARIO'      +'";'
020400110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
020500110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
020600110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
020700110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
020800130516     C                             +'"'+      'RAG.SOC. MITTENTE'      +'";'
020900130516     C                             +'"'+      'INDIRIZ. MITTENTE'      +'";'
021000130516     C                             +'"'+      'CAP MITTENTE'           +'";'
021100130516     C                             +'"'+      'LOCALITA MITTENTE'      +'";'
021200130516     C                             +'"'+      'PROV. MITTENTE'         +'";'
021300121207     C                             +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
021400121207     C                             +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
021500121207     C                             +'"'+      'CAP MITTENTE ORIG.'     +'";'
021600121207     C                             +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
021700121207     C                             +'"'+      'PROV. MITTENTE ORIG.'   +'";'
021800130409     C                             +'"'+      'LINK'                   +'";'
021900130409     C   52              EVAL      wBufferOut =
022000130409     C                              '"'+      'CUSTOMER CODE'          +'";'
022100130409 xxx C                             +'"'+      'SHIPMENT DATE'          +'";'
022200130409     C                             +'"'+      '1 SHIPMENT REFERENCE'   +'";'
022300130409     C                             +'"'+      '2 SHIPMENT REFERENCE'   +'";'
022400130409     C                             +'"'+      'STATUS'                 +'";'
022500130409 xxx C                             +'"'+      'STATUS DATE'            +'";'
022600130409     C                             +'"'+      'DEPARTURE DEPOT'        +'";'
022700130409     C                             +'"'+      'ARRIVAL DEPOT'          +'";'
022800130409     C                             +'"'+      'CONSIGNEE NAME'         +'";'
022900130409 xxx C                             +'"'+      'CONSIGNEE PHONE'        +'";'
023000130409     C                             +'"'+      'CONSIGNEE ADDRESS'      +'";'
023100130409     C                             +'"'+      'CONSIGNEE ZIPCODE'      +'";'
023200130409     C                             +'"'+      'CONSIGNEE CITY'         +'";'
023300130409     C                             +'"'+      'CONSIGNEE AREA'         +'";'
023400130516     C                             +'"'+      'SENDER NAME'            +'";'
023500130516     C                             +'"'+      'SENDER ADDRESS'         +'";'
023600130516     C                             +'"'+      'SENDER ZIPCODE'         +'";'
023700130516     C                             +'"'+      'SENDER CITY'            +'";'
023800130516     C                             +'"'+      'SENDER AREA'            +'";'
023900130409     C                             +'"'+      'ORIGINAL SENDER NAME'   +'";'
024000130409     C                             +'"'+      'ORIGINAL SENDER ADDRESS'+'";'
024100130409     C                             +'"'+      'ORIGINAL SENDER ZIPCODE'+'";'
024200130409     C                             +'"'+      'ORIGINAL SENDER CITY'   +'";'
024300130409     C                             +'"'+      'ORIGINAL SENDER AREA'   +'";'
024400130409     C                             +'"'+      'LINK'                   +'";'
024500100908     C                   ENDSL
024600100908     C*
024700071121     C                   EVAL      VAWDTA = wBufferOut
024800071121     C                   WRITE     TIVAW000
024900071121     C                   ENDIF
025000991027     C*
025100080924     C                   READ      TIVGDTMP
025200060119     C*
025300080924     C                   DOW       not %eof(TIVGDTMP)
025400080924     C*
025500080924     C                   eval      fnvac00t = vgdDTA
025600071121     C*
025700071121     C                   clear                   TIVAW000
025800071121     C                   clear                   wBufferOut
025900100907     C                   clear                   wLinkDir
026000100907     C                   clear                   wConsOkSiNo
026100100907     C                   clear                   wConsOkSiNoDes
026200100910     C*
026300100910     C* Solo bolle originali e valide ai fini della spedizione
026400100910     C                   movel     *blanks       wEsito1           1
026500100910     C*
026600100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
026700100910     C                                                vacAAS
026800100910     C                                               :vacLNP
026900100910     C                                               :vacNRS
027000100910     C                                               :vacNSP
027100100910     C                                               :pOutLblTyp
027200100910     C                                               :pOutAnnoBO
027300100910     C                                               :pOutLineaParBO
027400100910     C                                               :pOutSerieBO
027500100910     C                                               :pOutNumSpedBO
027600100910     C                                               :pOutDcmBO
027700100910     C                                               :pOutCcaBO
027800100910     C                                               :pOutRblBO))
027900100910     C*
028000130409     C                   if         pOutRblBO ='N' AND
028100130409     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
028200071210     C*
028300071210     C* Inizializzo campi d wrk
028400121207     C                   EVAL      wDSP     = *blanks
028500121207     C                   EVAL      wRD2     = *blanks
028600121207     C                   EVAL      wIND     = *blanks
028700121207     C                   EVAL      wCAD     = *blanks
028800121207     C                   EVAL      wLOD     = *blanks
028900121207     C                   EVAL      wPRD     = *blanks
029000130516     C                   EVAL      wRSCMITT = *blanks
029100130516     C                   EVAL      wINDMITT = *blanks
029200130516     C                   EVAL      wCAPMITT = *blanks
029300130516     C                   EVAL      wLOCMITT = *blanks
029400130516     C                   EVAL      wPRVMITT = *blanks
029500121207     C                   EVAL      wRSCMORI = *blanks
029600121207     C                   EVAL      wINDMORI = *blanks
029700121207     C                   EVAL      wCAPMORI = *blanks
029800121207     C                   EVAL      wLOCMORI = *blanks
029900121207     C                   EVAL      wPRVMORI = *blanks
030000121207     C*
030100071210     C* Aggancio la bolla relativa x il completamento dei dati
030200071210     C     KEYtas30      chain     titas30c
030300071210     C                   if        %found(titas30c)
030400110519     C*
030500121207     C* Reperisco l'estensione ragione sociale destinatario
030600110519     C                   eval      ta4TRC = 'D'
030700110519     C     KEYta430      chain     tita430c
030800110519     C                   if        %found(tita430c)
030900130409     C                   eval      wRD2 = ta4NOT
031000110519     C                   endif
031100071210     C*
031200071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
031300110519     C                   eval      wDSP = %editc((tasAAS*10000+tasMGS):'X')
031400110519     C                   eval      wIND = tasIND
031500110519     C                   eval      wCAD = tasCAD
031600110519     C                   eval      wLOD = tasLOD
031700110519     C                   eval      wPRD = tasPRD
031800121207     C*
031900121207     C* Reperisco i dati del mittente originale
032000121207     C                   eval      taaTRC = 'O'
032100121207     C     KEYtaa30      chain     titaa30c
032200121207     C                   if        %found(titaa30c)
032300121207     C                   eval      wRSCMORI = taaRSC
032400121207     C                   eval      wINDMORI = taaIND
032500121207     C                   eval      wCAPMORI = taaCAP
032600121207     C                   eval      wLOCMORI = taaLOC
032700121207     C                   eval      wPRVMORI = taaPRV
032800121207     C                   endif
032900130516     C*
033000130516     C* Reperisco i dati del mittente originale
033100130516     C                   eval      taaTRC = 'M'
033200130516     C     KEYtaa30      chain     titaa30c
033300130516     C                   if        %found(titaa30c)
033400130516     C                   eval      wRSCMITT = taaRSC
033500130516     C                   eval      wINDMITT = taaIND
033600130516     C                   eval      wCAPMITT = taaCAP
033700130516     C                   eval      wLOCMITT = taaLOC
033800130516     C                   eval      wPRVMITT = taaPRV
033900130516     C                   endif
034000071213     C*
034100071213     C                   endif
034200071121     C*
034300071121     C* Compondo la stringa dati output relativa al record corrente
034400071121     C                   EXSR      wrifile
034500100910     C*
034600100910     C                   endif
034700040719     C*
034800040719     C* In ogni caso storicizzo e cancello l'input sempre
034900080924     C                   DELETE    TIVGD000
035000040719     C*
035100040719     C* Proseguo con la lettura dell'input
035200080924     C                   READ      TIVGDTMP
035300060119     C*
035400030325     C                   ENDDO
035500071121     C*
035600071121     C* Head finali della struttura HTML
035700071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
035800071210     C***                WRITE     TIVAW000
035900991027     C*
036000030325     C                   EVAL      wrkesito = '0'
036100991027     C*
036200910830     C                   ENDSR
036300100907
036400100907
036500100907
036600100907     C     calSTS        BEGSR
036700100910     C*
036800120926     C                   MOVEL     *blanks       wMIC              1
036900121114     C                   MOVEL     *blanks       wGIAC             1
037000121207     C                   MOVEL     *blanks       wLAVV             1
037100130409     C                   MOVEL     *blanks       wCONS             1
037200100910     C                   EVAL      wDataSts       = vacDCM
037300100907     C*
037400100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
037500100907     C                   IF        VACDCM > *zeros AND
037600100907     C                             (VACCCA = *blanks OR VACCCA = '1')
037700100907     C                   EVAL      wConsOkSiNo    = 'S'
037800130409     C   51              EVAL      wConsOkSiNoDes = 'Consegnata'
037900130409     C   52              EVAL      wConsOkSiNoDes = 'Delivered'
038000130409     C                   EVAL      wCONS = 'S'
038100100907     C                   ELSE
038200100907     C                   EVAL      wConsOkSiNo    = 'N'
038300100907     C*
038400100907     C* A questo punto stabilisco la descrizione
038500100910     C                   SELECT
038600100910     C                   WHEN      vacCCA = '1'
038700130409     C   51              EVAL      wConsOkSiNoDes = 'Dirottata'
038800130409     C   52              EVAL      wConsOkSiNoDes = 'Diverted'
038900100910     C                   WHEN      vacCCA = '2'
039000130409     C   51              EVAL      wConsOkSiNoDes = 'Resa'
039100130409     C   52              EVAL      wConsOkSiNoDes = 'Returned'
039200100907     C                   EVAL      wConsOkSiNo    = 'R'
039300100910     C                   WHEN      vacCCA = '3'
039400130409     C   51              EVAL      wConsOkSiNoDes = 'Rapinata'
039500130409     C   52              EVAL      wConsOkSiNoDes = 'Goods Stolen'
039600110519     C                   EVAL      wConsOkSiNo    = 'R'
039700100910     C                   WHEN      vacCCA = '4'
039800130409     C   51              EVAL      wConsOkSiNoDes = 'Rapinato incasso'
039900130409     C   52              EVAL      wConsOkSiNoDes = 'Payment Stolen'
040000110519     C                   EVAL      wConsOkSiNo    = 'R'
040100100910     C                   WHEN      vacCCA = '5'
040200130409     C   51              EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
040300130409     C   52              EVAL      wConsOkSiNoDes = 'Goods never entrusted us'
040400110519     C                   EVAL      wConsOkSiNo    = 'R'
040500100910     C                   WHEN      vacCCA = '6'
040600130409     C   51              EVAL      wConsOkSiNoDes = 'Resa da danno'
040700130409     C   52              EVAL      wConsOkSiNoDes = 'Damage and Returned'
040800110519     C                   EVAL      wConsOkSiNo    = 'R'
040900130409     C                   WHEN      vacCCA = '7'
041000130409     C   51              EVAL      wConsOkSiNoDes = 'Merce mai affidata'
041100130409     C   52              EVAL      wConsOkSiNoDes = 'Goods never entrusted us'
041200130409     C                   EVAL      wConsOkSiNo    = 'R'
041300100910     C                   WHEN      vacCCA = 'A'
041400130409     C   51              EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
041500130409     C   52              EVAL      wConsOkSiNoDes = 'New on hold report'
041600110519     C                   EVAL      wConsOkSiNo    = 'R'
041700100910     C                   WHEN      vacCCA = 'C'
041800130409     C   51              EVAL      wConsOkSiNoDes = 'In consegna'
041900130409     C   52              EVAL      wConsOkSiNoDes = 'In delivery'
042000120926     C                   EVAL      wMIC = 'S'
042100100910     C                   WHEN      vacCCA = 'R'
042200130409     C   51              EVAL      wConsOkSiNoDes = 'Ripristinata'
042300130409     C   52              EVAL      wConsOkSiNoDes = 'Reset shipment'
042400100910     C                   WHEN      vacCCA = 'S'
042500130409     C   51              EVAL      wConsOkSiNoDes = 'Merce distrutta'
042600130409     C   52              EVAL      wConsOkSiNoDes = 'Destroyed Goods'
042700110519     C                   EVAL      wConsOkSiNo    = 'R'
042800100910     C                   ENDSL
042900100907     C*
043000100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
043100100910     C                   EVAL      wDataSts       = vacDLA
043200130409     C   51              EVAL      wConsOkSiNoDes = 'Lasciato avviso'
043300130409     C   52              EVAL      wConsOkSiNoDes = 'Delivery attempt failed' +
043400130409     C                                              ' - Left Notice'
043500121207     C                   EVAL      wLAVV = 'S'
043600100907     C                   ENDIF
043700100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
043800100910     C                   EVAL      wDataSts       = vacDAG
043900130409     C   51              EVAL      wConsOkSiNoDes = 'In giacenza'
044000130409     C   52              EVAL      wConsOkSiNoDes = 'On-Hold in stock'
044100121114     C                   EVAL      wGIAC = 'S'
044200100907     C                   ENDIF
044300100910     C                   IF        vacDCM = *zeros AND
044400100910     C                             vacDLA = *zeros AND
044500100910     C                             vacDAG = *zeros AND
044600100910     C                             vacCCA = *blanks
044700100910     C                   EVAL      wDataSts       = datcor
044800100910     C*
044900100910     C* Verifico la data consegna richiesta
045000100910     C                   IF        vacDCR > datcor
045100130409     C   51              EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
045200110519     C                                              'data ' + %editc(vacDCR:'X')
045300130409     C   52              EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
045400130409     C                                              'on ' + %editc(vacDCR:'X')
045500100910     C                   ELSE
045600130409     C   51              EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
045700130409     C   52              EVAL      wConsOkSiNoDes = 'In progress'
045800100910     C                   ENDIF
045900100910     C                   ENDIF
046000100907     C*
046100100907     C                   ENDIF
046200100907     C*
046300100907     C                   ENDSR
046400130409
046500130409
046600130409
046700130409     C     makLNK        BEGSR
046800130409     C*
046900130409     C* Se richiesta inclusione link T&T
047000130409     C                   if        %subst(prmppt:6:7) <> *blanks
047100130409     C*
047200130409     C* Porto in minuscolo il parametro relativo alla lingua del link
047300130409     C                   movel     *blanks       parLang           2
047400130409     C                   eval      parLang = %subst(prmppt:13:2)
047500130409     C     maiu:minu     xlate     parLang       parLang
047600130409     C*
047700160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
047800130409     C                                        'sped_ricdocmit_load.hsm?docmit='+
047900130409     C                                        %trim(%char(vacRMN))             +
048000130409     C                                        '&ksu='+%subst(prmppt:6:7)+
048100130409     C                                        '&lang='+parLang
048200130409     C*
048300130409     C                   endif
048400130409     C*
048500130409     C                   ENDSR
048600100907
048700071121
048800071121
048900071121
049000071121     C     wrifile       BEGSR
049100120926     C*
049200120926     C                   SETOFF                                       31
049300071121     C*
049400100907     C* Determino lo status della spedizione
049500100907     C                   EXSR      calSTS
049600100907     C*
049700100907     C* Costruisco il link diretto
049800130409     C                   EXSR      makLNK
049900130409     C*
050000130409     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
050100130409     C                   IF        %subst(prmppt:15:1) = 'P'
050200130409     C                   MOVE      vacCCM        CCM               4 0
050300130409     C                   MOVE      wKSC          KSC               4 0
050400130409     C                   IF        CCM =  8888 or  CCM =  9999
050500130409     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
050600130409     C                   EVAL      vacCCM = wKSC
050700130409     C                   ENDIF
050800130409     C                   ENDIF
050900130409     C                   ENDIF
051000100908     C*
051100100908     C                   SELECT
051200100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
051300071121     C                   EVAL      wBufferOut = iRow +
051400100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
051500130409 xxx C                                  iCol+%trim(wDSP)                  +fCol+
051600130409     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
051700130409     C                                  iCol+%trim(vacRMA)                +fCol+
051800130409     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
051900130409 xxx C                                  iCol+%trim(%editc(wDataSts:'X'))  +fCol+
052000130409     C                                  iCol+%trim(%editc(vacLNP:'X'))    +fCol+
052100110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
052200130409     C                                  iCol+%trim(vacRSD)+' '+%trim(wRD2)+fCol+
052300130409 xxx C                                  iCol+%trim(' ')                   +fCol+
052400110519     C                                  iCol+%trim(wIND)                  +fCol+
052500100517     C                                  iCol+%trim(wCAD)                  +fCol+
052600100517     C                                  iCol+%trim(wLOD)                  +fCol+
052700110519     C                                  iCol+%trim(wPRD)                  +fCol+
052800130516     C                                  iCol+%trim(wRSCMITT)              +fCol+
052900130516     C                                  iCol+%trim(wINDMITT)              +fCol+
053000130516     C                                  iCol+%trim(wCAPMITT)              +fCol+
053100130516     C                                  iCol+%trim(wLOCMITT)              +fCol+
053200130516     C                                  iCol+%trim(wPRVMITT)              +fCol+
053300121207     C                                  iCol+%trim(wRSCMORI)              +fCol+
053400121207     C                                  iCol+%trim(wINDMORI)              +fCol+
053500121207     C                                  iCol+%trim(wCAPMORI)              +fCol+
053600121207     C                                  iCol+%trim(wLOCMORI)              +fCol+
053700121207     C                                  iCol+%trim(wPRVMORI)              +fCol+
053800130409     C                                  iCol+%trim(wLinkDir)              +fCol
053900100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
054000100908     C                   EVAL      wBufferOut =
054100130409     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
054200130409 xxx C                              +'"'+   %trim(wDSP)                   +'";'
054300130409     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
054400130409     C                              +'"'+   %trim(vacRMA)                 +'";'
054500130409     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
054600130409 xxx C                              +'"'+   %trim(%editc(wDataSts:'X'))   +'";'
054700130409     C                              +'"'+   %trim(%editc(vacLNP:'X'))     +'";'
054800110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
054900130409     C                              +'"'+   %trim(vacRSD)+' '+%trim(wRD2) +'";'
055000130409 xxx C                              +'"'+   %trim(' ')                    +'";'
055100110519     C                              +'"'+   %trim(wIND)                   +'";'
055200110519     C                              +'"'+   %trim(wCAD)                   +'";'
055300110519     C                              +'"'+   %trim(wLOD)                   +'";'
055400110519     C                              +'"'+   %trim(wPRD)                   +'";'
055500130516     C                              +'"'+   %trim(wRSCMITT)               +'";'
055600130516     C                              +'"'+   %trim(wINDMITT)               +'";'
055700130516     C                              +'"'+   %trim(wCAPMITT)               +'";'
055800130516     C                              +'"'+   %trim(wLOCMITT)               +'";'
055900130516     C                              +'"'+   %trim(wPRVMITT)               +'";'
056000121207     C                              +'"'+   %trim(wRSCMORI)               +'";'
056100121207     C                              +'"'+   %trim(wINDMORI)               +'";'
056200121207     C                              +'"'+   %trim(wCAPMORI)               +'";'
056300121207     C                              +'"'+   %trim(wLOCMORI)               +'";'
056400121207     C                              +'"'+   %trim(wPRVMORI)               +'";'
056500130409     C                              +'"'+   %trim(wLinkDir)               +'";'
056600100908     C                   ENDSL
056700071121     C*
056800120926     C* Se nei parametri è richiesto no MIC => gestisco
056900120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
057000120926     C                   SETON                                        31
057100120926     C                   ENDIF
057200121114     C*
057300121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
057400121207     C                   IF        %subst(prmppt:4:1) = 'S' and
057500121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
057600121114     C                   SETON                                        31
057700121114     C                   ENDIF
057800130409     C*
057900130409     C* Se nei parametri è richiesto solo CONSEGNATE o solo NON CONSEGNATE => gestisco
058000130409     C                   IF        %subst(prmppt:5:1) = 'N' and wCONS =  'S'
058100130409     C                   SETON                                        31
058200130409     C                   ENDIF
058300130409     C                   IF        %subst(prmppt:5:1) = 'S' and wCONS <> 'S'
058400130409     C                   SETON                                        31
058500130409     C                   ENDIF
058600120926     C*
058700071121     C                   EVAL      VAWDTA = wBufferOut
058800110519     C*
058900120926     C  N31              WRITE     TIVAW000
059000071121     C*
059100071121     C                   ENDSR
059200071121
059300071121
059400071121
059500991027
059600991027      /TITLE Operazioni iniziali.
059700991027     C     *inzsr        BEGSR
059800991027     C*
059900991027     C     *ENTRY        PLIST
060000991027     C                   parm                    prmppt
060100991027     C     wrkesito      parm      wrkesito      prmesito
060200100910     C*
060300100910     C* CALCOLA LA DATA CORRENTE
060400100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
060500100910     C                   eval      datcor = %dec(%date() : *ISO)
060600071210     C*
060700071213     C* TITAS30C - Parziale
060800071210     C     KEYtas30      KLIST
060900071210     C                   KFLD                    vacAAS
061000071210     C                   KFLD                    vacLNP
061100071210     C                   KFLD                    vacNRS
061200071210     C                   KFLD                    vacNSP
061300110519     C*
061400110519     C* TITA430C - Completa
061500110519     C     KEYta430      KLIST
061600110519     C                   KFLD                    vacAAS
061700110519     C                   KFLD                    vacLNP
061800110519     C                   KFLD                    vacNRS
061900110519     C                   KFLD                    vacNSP
062000110519     C                   KFLD                    ta4TRC
062100121207     C*
062200121207     C* TITAA30C - Completa
062300121207     C     KEYtaa30      KLIST
062400121207     C                   KFLD                    vacAAS
062500121207     C                   KFLD                    vacLNP
062600121207     C                   KFLD                    vacNRS
062700121207     C                   KFLD                    vacNSP
062800121207     C                   KFLD                    taaTRC
062900991027     C*
063000991027     C                   ENDSR
063100071121     C*
063200071121** DEC - COSTANTI
063300071121<?xml version="1.0"?>
063400071121<?mso-application progid="Excel.Sheet"?>
063500071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
063600071121xmlns:o="urn:schemas-microsoft-com:office:office"
063700071121xmlns:x="urn:schemas-microsoft-com:office:excel"
063800071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
063900071121xmlns:html="http://www.w3.org/TR/REC-html40">
064000071121</Workbook>
064100071121<Worksheet ss:Name="PAG1">
064200071121</Worksheet>
064300071121<Table border="1">
064400071121</Table>
064500071121<Row>
064600071121</Row>
064700071121<Cell><Data ss:Type="String">
064800071121<Cell><Data ss:Type="Number">
064900071121</Data></Cell>
065000071121
065100071121
065200071121
