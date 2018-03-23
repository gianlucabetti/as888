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
005900100907     D wLinkDir        S            300    inz(*blanks)
006000110519     D wDSP            s             10    inz(*blanks)
006100110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
006200110519     D wIND            s                   inz(*blanks) like(tasIND)
006300110519     D wCAD            s                   inz(*blanks) like(tasIND)
006400110519     D wLOD            s                   inz(*blanks) like(tasLOD)
006500110519     D wPRD            s                   inz(*blanks) like(tasPRD)
006600140127     D wNCD            s                   inz(*blanks) like(tasNCD)
006700140127     D wNCA            s                   inz(*blanks) like(tasNCA)
006800140127     D wSPEDBRT        s             12  0 inz
006900121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
007000121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
007100121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
007200121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
007300121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
007400140129     D wFIRMATAR       s                   inz(*blanks) like(ta4NOT)
007500130426     D wKSC            s              7  0 inz(*zeros)
007600140308     D wNoteGiac       s            300    inz(*blanks)
007700140311     D wGCPCMC         s                   inz(*blanks) like(gcpCMC)
007800140311     D wGiacCau        s                   inz(*blanks) like(§2ADES)
007900071211     D*
008000100910
008100100910
008200100910     D*------------------
008300100910     D* LINKING A DEFINIZIONI ESTERNE
008400100910     D*------------------
008500100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008600100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008700140308     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008800140308     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008900130426
009000130426
009100130426     D*-------------------
009200130426     D* COSTANTI
009300130426     D*-------------------
009400130426     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009500130426     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009600100910
009700030924
009800030325
009900030924     C* MAIN/
010000140308     C*
010100071121     C* Verifico se già presenti dati nel file d output
010200071121     C                   EXSR      chkRECFILMBR
010300071121     C*
010400071121     C* Effettuo elaborazione
010500071121     C                   EXSR      traduci
010600071121     C*
010700921023     C                   SETON                                        LR
010800991027
010900071121
011000071121
011100071121     C     chkRECFILMBR  BEGSR
011200071121     C*
011300071121     C                   READ      TIVAWWWT                               55
011400071121     C*
011500071121     C                   ENDSR
011600071121
011700071121
011800030924
011900991027     C     traduci       BEGSR
012000071121     C*
012100071121     C* Head iniziali della struttura HTML
012200100908     C                   IF        %subst(prmppt:2:1) = 'H'
012300071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
012400071210     C   55              WRITE     TIVAW000
012500100908     C                   ENDIF
012600071121     C*
012700071121     C* Inizializzazone formato e variabili x output
012800071121     C                   clear                   TIVAW000
012900071121     C                   clear                   wBufferOut
013000100907     C                   clear                   wLinkDir
013100100907     C                   clear                   wConsOkSiNo
013200100907     C                   clear                   wConsOkSiNoDes
013300071121     C*
013400071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
013500071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
013600100908     C*
013700100908     C                   SELECT
013800100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
013900071121     C                   EVAL      wBufferOut = iThead + iRow
014000110519     C                             +iColBold+ 'CODICE CLIENTE'         +fColBold
014100110519     C                             +iColBold+ 'DATA SPEDIZIONE'        +fColBold
014200121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
014300110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
014400110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
014500110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
014600110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
014700141023     C                             +iColBold+ 'NATURA MERCE'           +fColBold
014800110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
014900140308     C                             +iColBold+ 'DATA STATO'             +fColBold
015000110519     C                             +iColBold+ 'STATO'                  +fColBold
015100140308     C                             +iColBold+ 'LDV BRT'                +fColBold
015200140127     C                             +iColBold+ 'FIRMA CONSEGNA'         +fColBold
015300100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
015400100908     C                   EVAL      wBufferOut =
015500110519     C                              '"'+      'CODICE CLIENTE'         +'";'
015600110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
015700140308     C                             +'"'+      'RAG.SOC. DESTINATARIO'  +'";'
015800110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
015900110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
016000110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
016100110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
016200141023     C                             +'"'+      'NATURA MERCE'           +'";'
016300110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
016400140308     C                             +'"'+      'DATA STATO'             +'";'
016500110519     C                             +'"'+      'STATO'                  +'";'
016600140308     C                             +'"'+      'LDV BRT'                +'";'
016700140127     C                             +'"'+      'FIRMA CONSEGNA'         +'";'
016800140127     C                   ENDSL
016900100908     C*
017000071121     C                   EVAL      VAWDTA = wBufferOut
017100071121     C                   WRITE     TIVAW000
017200071121     C                   ENDIF
017300991027     C*
017400080924     C                   READ      TIVGDTMP
017500060119     C*
017600080924     C                   DOW       not %eof(TIVGDTMP)
017700080924     C*
017800080924     C                   eval      fnvac00t = vgdDTA
017900071121     C*
018000071121     C                   clear                   TIVAW000
018100071121     C                   clear                   wBufferOut
018200100907     C                   clear                   wLinkDir
018300100907     C                   clear                   wConsOkSiNo
018400100907     C                   clear                   wConsOkSiNoDes
018500140308     C                   clear                   dar5gen
018600140308     C                   clear                   dar5emd
018700100910     C*
018800100910     C* Solo bolle originali e valide ai fini della spedizione
018900100910     C                   movel     *blanks       wEsito1           1
019000100910     C*
019100100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019200100910     C                                                vacAAS
019300100910     C                                               :vacLNP
019400100910     C                                               :vacNRS
019500100910     C                                               :vacNSP
019600100910     C                                               :pOutLblTyp
019700100910     C                                               :pOutAnnoBO
019800100910     C                                               :pOutLineaParBO
019900100910     C                                               :pOutSerieBO
020000100910     C                                               :pOutNumSpedBO
020100100910     C                                               :pOutDcmBO
020200100910     C                                               :pOutCcaBO
020300100910     C                                               :pOutRblBO))
020400100910     C*
020500130426     C                   if         pOutRblBO ='N' AND
020600130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
020700071210     C*
020800071210     C* Inizializzo campi d wrk
020900140127     C                   EVAL      wDSP       = *blanks
021000140127     C                   EVAL      wRD2       = *blanks
021100140127     C                   EVAL      wIND       = *blanks
021200140127     C                   EVAL      wCAD       = *blanks
021300140127     C                   EVAL      wLOD       = *blanks
021400140127     C                   EVAL      wPRD       = *blanks
021500140127     C                   EVAL      wNCD       = *zeros
021600140127     C                   EVAL      wNCA       = *zeros
021700140127     C                   EVAL      wSPEDBRT   = *zeros
021800140127     C                   EVAL      wRSCMORI   = *blanks
021900140127     C                   EVAL      wINDMORI   = *blanks
022000140127     C                   EVAL      wCAPMORI   = *blanks
022100140127     C                   EVAL      wLOCMORI   = *blanks
022200140127     C                   EVAL      wPRVMORI   = *blanks
022300140129     C                   EVAL      wFIRMATAR  = *blanks
022400140308     C                   EVAL      wNoteGiac  = *blanks
022500140311     C                   EVAL      wGCPCMC    = *blanks
022600140311     C                   EVAL      wGiacCau   = *blanks
022700140308     C                   setoff                                       70
022800121207     C*
022900071210     C* Aggancio la bolla relativa x il completamento dei dati
023000071210     C     KEYtas30      chain     titas30c
023100071210     C                   if        %found(titas30c)
023200110519     C*
023300121207     C* Reperisco l'estensione ragione sociale destinatario
023400110519     C                   eval      ta4TRC = 'D'
023500140129     C     KEYta430C     chain     tita430c
023600110519     C                   if        %found(tita430c)
023700110519     C                   eval      wRD2 = '.' + ta4NOT
023800110519     C                   endif
023900140127     C*
024000140127     C* Reperisco, se c'è, la firma del consegnatario
024100140129     C                   eval      ta4TRC = '1'
024200140129     C     KEYta430C     chain     tita430c
024300140129     C                   if        %found(tita430c)
024400140129     C                   eval      wFIRMATAR = ta4NOT
024500140127     C                   endif
024600140308     C*
024700140308     C* Reperisco, se c'è, il referente consegna ed il suo telefono
024800140308     C                   eval      ar5TRD = 'GEN'
024900140308     C     KEYar531_1P   chain     fiar531C
025000140308     C                   if        %found(FIAR531C)
025100140308     C                   eval      dar5gen = ar5UNI
025200140308     C                   endif
025300140308     C*
025400140308     C* Reperisco, se c'è, il cellulare del destinatario
025500140308     C                   eval      ar5TRD = 'EMD'
025600140308     C     KEYar531_1P   chain     fiar531C
025700140308     C                   if        %found(FIAR531C)
025800140308     C                   eval      dar5emd = ar5UNI
025900140308     C                   endif
026000071210     C*
026100071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
026200140308     C                   eval      wDSP     =
026300140308     C                              UBFMTDATE_Convert(
026400140308     C                                  %editc((tasAAS*10000+tasMGS):'4') :
026500140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
026600140127     C                   eval      wIND     = tasIND
026700140127     C                   eval      wCAD     = tasCAD
026800140127     C                   eval      wLOD     = tasLOD
026900140127     C                   eval      wPRD     = tasPRD
027000140127     C                   eval      wNCD     = tasNCD
027100140127     C                   eval      wNCA     = tasNCA
027200140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
027300140127     C                                        tasNRS*10000000+
027400140127     C                                        tasNSP
027500121207     C*
027600121207     C* Reperisco i dati del mittente originale
027700140308     C***                eval      taaTRC = 'O'
027800140308     C***      aa30      chain     titaa30c
027900140308     C***                if        %found(titaa30c)
028000140308     C***                eval      wRSCMORI = taaRSC
028100140308     C***                eval      wINDMORI = taaIND
028200140308     C***                eval      wCAPMORI = taaCAP
028300140308     C***                eval      wLOCMORI = taaLOC
028400140308     C***                eval      wPRVMORI = taaPRV
028500140308     C***                endif
028600130913     C*
028700130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
028800130913     C* (costruisco la chiave sul file TIGCP51L)
028900130913     C*
029000140310     C                   EVAL      gcpDUR = *zeros
029100130913     C                   EVAL      gcpFRG = *zeros
029200130913     C     KEYgcp51      SETLL     tigcp51l
029300130913     C                   IF        %equal(tigcp51l)
029400130913     C     KEYgcp51      READE     tigcp51l
029500130913     C                   DOW       not %eof(tigcp51l)
029600130913     C*
029700130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
029800130913     C                   IF        gcpDUR > *zeros AND
029900130913     C                             gcpDDM = *zeros AND
030000130913     C                             gcpFAS < 030
030100130913     C                   seton                                        70
030200130913     C                   ENDIF
030300140311     C*
030400140311     C* Reperisco la causale di ultima apertura giacenza
030500140311     C                   EVAL      wGCPCMC = gcpCMC
030600130913     C*
030700130913     C* Proseguo la lettura delle giacenze x la bolla corrente
030800130913     C     KEYgcp51      READE     tigcp51l
030900130913     C                   ENDDO
031000140308     C*
031100140308     C* Se in giacenza reperisco le note di giacenza
031200140308     C                   EXSR      rtvNOTEGIAC
031300140308     C*
031400140308     C                   ENDIF
031500071213     C*
031600071213     C                   endif
031700071121     C*
031800071121     C* Compondo la stringa dati output relativa al record corrente
031900071121     C                   EXSR      wrifile
032000100910     C*
032100100910     C                   endif
032200040719     C*
032300040719     C* In ogni caso storicizzo e cancello l'input sempre
032400080924     C                   DELETE    TIVGD000
032500040719     C*
032600040719     C* Proseguo con la lettura dell'input
032700080924     C                   READ      TIVGDTMP
032800060119     C*
032900030325     C                   ENDDO
033000071121     C*
033100071121     C* Head finali della struttura HTML
033200071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
033300071210     C***                WRITE     TIVAW000
033400991027     C*
033500030325     C                   EVAL      wrkesito = '0'
033600991027     C*
033700910830     C                   ENDSR
033800100907
033900100907
034000100907
034100100907     C     calSTS        BEGSR
034200100910     C*
034300120926     C                   MOVEL     *blanks       wMIC              1
034400121114     C                   MOVEL     *blanks       wGIAC             1
034500121207     C                   MOVEL     *blanks       wLAVV             1
034600140129     C                   MOVEL     *blanks       wPEND             1
034700100910     C                   EVAL      wDataSts       = vacDCM
034800140308     C*
034900140308     C* Verifico lo stato delle disposizioni di giacenza
035000140308     C                   MOVEL     *blanks       wGiacDisp        10
035100140308     C   70              EVAL      wGiacDisp = 'IN ATTESA'
035200100907     C*
035300100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
035400100907     C                   IF        VACDCM > *zeros AND
035500100907     C                             (VACCCA = *blanks OR VACCCA = '1')
035600100907     C                   EVAL      wConsOkSiNo    = 'S'
035700110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
035800140308     C                   EVAL      wGiacDisp      = *blanks
035900140308     C                   EVAL      wNoteGiac      = *blanks
036000100907     C                   ELSE
036100100907     C                   EVAL      wConsOkSiNo    = 'N'
036200140311     C                   EVAL      wFIRMATAR      = *blanks
036300100907     C*
036400100907     C* A questo punto stabilisco la descrizione
036500100910     C                   SELECT
036600100910     C                   WHEN      vacCCA = '1'
036700110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
036800100910     C                   WHEN      vacCCA = '2'
036900110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
037000100907     C                   EVAL      wConsOkSiNo    = 'R'
037100100910     C                   WHEN      vacCCA = '3'
037200110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
037300110519     C                   EVAL      wConsOkSiNo    = 'R'
037400100910     C                   WHEN      vacCCA = '4'
037500110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
037600110519     C                   EVAL      wConsOkSiNo    = 'R'
037700100910     C                   WHEN      vacCCA = '5'
037800110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
037900110519     C                   EVAL      wConsOkSiNo    = 'R'
038000100910     C                   WHEN      vacCCA = '6'
038100110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
038200110519     C                   EVAL      wConsOkSiNo    = 'R'
038300160302     C                   WHEN      vacCCA = '7'
038400160302     C                   EVAL      wConsOkSiNoDes = 'Merce mai affidata'
038500160302     C                   EVAL      wConsOkSiNo    = 'R'
038600100910     C                   WHEN      vacCCA = 'A'
038700110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
038800110519     C                   EVAL      wConsOkSiNo    = 'R'
038900100910     C                   WHEN      vacCCA = 'C'
039000110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
039100120926     C                   EVAL      wMIC = 'S'
039200100910     C                   WHEN      vacCCA = 'R'
039300110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
039400100910     C                   WHEN      vacCCA = 'S'
039500110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
039600110519     C                   EVAL      wConsOkSiNo    = 'R'
039700100910     C                   ENDSL
039800100907     C*
039900140308     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
040000100910     C                   EVAL      wDataSts       = vacDLA
040100110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
040200121207     C                   EVAL      wLAVV = 'S'
040300100907     C                   ENDIF
040400140308     C                   IF        (gcpDUR > vacDCM AND gcpDUR >= vacDLA)
040500140308     C                   EVAL      wDataSts       = gcpDUR
040600110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
040700140311     C                   EXSR      rtvCAU_GIAC
040800121114     C                   EVAL      wGIAC = 'S'
040900140308     C                   ELSE
041000140308     C                   EVAL      wGiacDisp      = *blanks
041100140308     C                   EVAL      wNoteGiac      = *blanks
041200100907     C                   ENDIF
041300100910     C                   IF        vacDCM = *zeros AND
041400100910     C                             vacDLA = *zeros AND
041500140308     C                             gcpDUR = *zeros AND
041600100910     C                             vacCCA = *blanks
041700100910     C                   EVAL      wDataSts       = datcor
041800100910     C*
041900100910     C* Verifico la data consegna richiesta
042000100910     C                   IF        vacDCR > datcor
042100140308     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
042200140308     C                              UBFMTDATE_Convert(
042300140308     C                                  %editc(vacDCR:'4') :
042400140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
042500100910     C                   ELSE
042600110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
042700140129     C                   EVAL      wPEND          = 'S'
042800100910     C                   ENDIF
042900100910     C                   ENDIF
043000100907     C*
043100100907     C                   ENDIF
043200100907     C*
043300100907     C                   ENDSR
043400140311
043500140311
043600140311
043700140311     C     rtvCAU_GIAC   BEGSR
043800140311     C*
043900140311     C* Rererisco la descrizioe della causale di giacenza
044000140311     C                   MOVEL     '1'           tblKUT
044100140311     C                   MOVEL     '2A'          tblCOD
044200140311     C                   MOVEL(P)  wGCPCMC       tblKEY
044300140311     C     KEYtab00_C    CHAIN     TABEL00F                           30
044400140311     C                   IF        %found(tabel00f)
044500140311     C                   MOVEL     TBLUNI        ds2A
044600140311     C                   MOVEL(P)  §2ADES        wGiacCau
044700140311     C                   ENDIF
044800140311     C*
044900140311     C                   ENDSR
045000140311     C*------------------------------------------------------------------------*
045100140311
045200140308
045300140308
045400140308     C     rtvNOTEGIAC   BEGSR
045500140308     C*
045600140308     C* Scorro tutte le note giacenza relative alla giacebza corrente
045700140308     C     KEYgnp01_P    setll     tignp01l
045800140308     C                   if        %equal(tignp01l)
045900140308     C                   eval      wNoteGiac = ''''
046000140308     C     KEYgnp01_P    reade     tignp01l
046100140308     C                   dow       not %eof(tignp01l)
046200140308     C*** concateno note giacenza
046300140308     C                   eval      wNoteGiac =
046400140308     C                             %trim(wNoteGiac) + ' ' + gnpDMC
046500140308     C     KEYgnp01_P    reade     tignp01l
046600140308     C                   enddo
046700140308     C                   endif
046800140308     C*
046900140308     C                   ENDSR
047000100907
047100100907
047200100907
047300100907     C     makLNK        BEGSR
047400130426     C*
047500130426     C* Se richiesta inclusione link T&T
047600130426     C                   if        %subst(prmppt:6:7) <> *blanks
047700130426     C*
047800130426     C* Porto in minuscolo il parametro relativo alla lingua del link
047900130426     C                   movel     *blanks       parLang           2
048000130426     C                   eval      parLang = %subst(prmppt:13:2)
048100130426     C     maiu:minu     xlate     parLang       parLang
048200130426     C*
048300160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
048400130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
048500130426     C                                        %trim(%char(vacRMN))             +
048600130426     C                                        '&ksu='+%subst(prmppt:6:7)+
048700130426     C                                        '&lang='+parLang
048800130426     C*
048900130426     C                   endif
049000100907     C*
049100100907     C                   ENDSR
049200100907
049300071121
049400071121
049500071121
049600071121     C     wrifile       BEGSR
049700120926     C*
049800120926     C                   SETOFF                                       31
049900071121     C*
050000100907     C* Determino lo status della spedizione
050100100907     C                   EXSR      calSTS
050200100907     C*
050300100907     C* Costruisco il link diretto
050400130426     C                   EXSR      makLNK
050500130426     C*
050600130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
050700130426     C                   IF        %subst(prmppt:15:1) = 'P'
050800130426     C                   MOVE      vacCCM        CCM               4 0
050900130426     C                   MOVE      wKSC          KSC               4 0
051000130426     C                   IF        CCM =  8888 or  CCM =  9999
051100130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
051200130426     C                   EVAL      vacCCM = wKSC
051300130426     C                   ENDIF
051400130426     C                   ENDIF
051500130426     C                   ENDIF
051600100908     C*
051700140308     C                   EVAL      wDataStsAlfa =
051800140308     C                              UBFMTDATE_Convert(
051900140308     C                                  %editc(wDataSts:'4') :
052000140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
052100140308     C*
052200100908     C                   SELECT
052300100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
052400071121     C                   EVAL      wBufferOut = iRow +
052500100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
052600140308     C                                  iCol+%trim(wDSP)                  +fCol+
052700100907     C                                  iCol+%trim(vacRSD)                +fCol+
052800110519     C                                  iCol+%trim(wIND)                  +fCol+
052900100517     C                                  iCol+%trim(wCAD)                  +fCol+
053000100517     C                                  iCol+%trim(wLOD)                  +fCol+
053100110519     C                                  iCol+%trim(wPRD)                  +fCol+
053200141023     C                                  iCol+%trim(vacNAS)                +fCol+
053300100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
053400140308     C                                  iCol+%trim(wDataStsAlfa)          +fCol+
053500100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
053600140127     C                                  iCol+%trim(%editc(wSPEDBRT:'X'))  +fCol+
053700140129     C                                  iCol+%trim(wFIRMATAR)             +fCol
053800100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
053900100908     C                   EVAL      wBufferOut =
054000110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
054100140308     C                              +'"'+   %trim(wDSP)                   +'";'
054200110519     C                              +'"'+   %trim(vacRSD)                 +'";'
054300110519     C                              +'"'+   %trim(wIND)                   +'";'
054400110519     C                              +'"'+   %trim(wCAD)                   +'";'
054500110519     C                              +'"'+   %trim(wLOD)                   +'";'
054600110519     C                              +'"'+   %trim(wPRD)                   +'";'
054700141023     C                              +'"'+   %trim(vacNAS)                 +'";'
054800110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
054900140308     C                              +'"'+   %trim(wDataStsAlfa)           +'";'
055000110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
055100140127     C                              +'"'+   %trim(%editc(wSPEDBRT:'X'))   +'";'
055200140129     C                              +'"'+   %trim(wFIRMATAR)              +'";'
055300100908     C                   ENDSL
055400071121     C*
055500120926     C* Se nei parametri è richiesto no MIC => gestisco
055600120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
055700120926     C                   SETON                                        31
055800120926     C                   ENDIF
055900121114     C*
056000121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
056100121207     C                   IF        %subst(prmppt:4:1) = 'S' and
056200121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
056300121114     C                   SETON                                        31
056400121114     C                   ENDIF
056500140124     C*
056600140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
056700140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
056800140124     C                   SETON                                        31
056900140124     C                   ENDIF
057000120926     C*
057100071121     C                   EVAL      VAWDTA = wBufferOut
057200110519     C*
057300110519     C* Restituisco solo le spedizioni "pendenti"
057400110713     C***                IF        wConsOkSiNo = 'N'
057500120926     C  N31              WRITE     TIVAW000
057600110713     C***                ENDIF
057700071121     C*
057800071121     C                   ENDSR
057900071121
058000071121
058100071121
058200991027
058300991027      /TITLE Operazioni iniziali.
058400991027     C     *inzsr        BEGSR
058500991027     C*
058600991027     C     *ENTRY        PLIST
058700991027     C                   parm                    prmppt
058800991027     C     wrkesito      parm      wrkesito      prmesito
058900100910     C*
059000100910     C* CALCOLA LA DATA CORRENTE
059100100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
059200100910     C                   eval      datcor = %dec(%date() : *ISO)
059300071210     C*
059400071213     C* TITAS30C - Parziale
059500071210     C     KEYtas30      KLIST
059600071210     C                   KFLD                    vacAAS
059700071210     C                   KFLD                    vacLNP
059800071210     C                   KFLD                    vacNRS
059900071210     C                   KFLD                    vacNSP
060000110519     C*
060100110519     C* TITA430C - Completa
060200140129     C     KEYta430C     KLIST
060300110519     C                   KFLD                    vacAAS
060400110519     C                   KFLD                    vacLNP
060500110519     C                   KFLD                    vacNRS
060600110519     C                   KFLD                    vacNSP
060700110519     C                   KFLD                    ta4TRC
060800121207     C*
060900121207     C* TITAA30C - Completa
061000121207     C     KEYtaa30      KLIST
061100121207     C                   KFLD                    vacAAS
061200121207     C                   KFLD                    vacLNP
061300121207     C                   KFLD                    vacNRS
061400121207     C                   KFLD                    vacNSP
061500121207     C                   KFLD                    taaTRC
061600130913     C*
061700130913     C* TIGCP51L - Completa
061800130913     C     KEYgcp51      KLIST
061900130913     C                   KFLD                    vacAAS
062000130913     C                   KFLD                    vacLNP
062100130913     C                   KFLD                    vacNRS
062200130913     C                   KFLD                    vacNSP
062300130913     C                   KFLD                    gcpFRG
062400140308     C*
062500140308     C* Chiave su FIAR531C - Parziale
062600140308     C     KEYar531_1P   KLIST
062700140308     C                   KFLD                    vacAAS
062800140308     C                   KFLD                    vacLNP
062900140308     C                   KFLD                    vacNRS
063000140308     C                   KFLD                    vacNSP
063100140308     C                   KFLD                    ar5TRD
063200140308     C*
063300140308     C* Chiave su TIGNP01L - Parziale
063400140308     C     KEYgnp01_P    KLIST
063500140308     C                   KFLD                    gcpAGC
063600140308     C                   KFLD                    gcpFGC
063700140308     C                   KFLD                    gcpNGC
063800140308     C                   KFLD                    gcpFRG
063900140311     C*
064000140311     C* TABEL00F - Completa
064100140311     C     KEYtab00_C    KLIST
064200140311     C                   KFLD                    tblKUT
064300140311     C                   KFLD                    tblCOD
064400140311     C                   KFLD                    tblKEY
064500991027     C*
064600991027     C                   ENDSR
064700071121     C*
064800071121** DEC - COSTANTI
064900071121<?xml version="1.0"?>
065000071121<?mso-application progid="Excel.Sheet"?>
065100071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
065200071121xmlns:o="urn:schemas-microsoft-com:office:office"
065300071121xmlns:x="urn:schemas-microsoft-com:office:excel"
065400071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
065500071121xmlns:html="http://www.w3.org/TR/REC-html40">
065600071121</Workbook>
065700071121<Worksheet ss:Name="PAG1">
065800071121</Worksheet>
065900071121<Table border="1">
066000071121</Table>
066100071121<Row>
066200071121</Row>
066300071121<Cell><Data ss:Type="String">
066400071121<Cell><Data ss:Type="Number">
066500071121</Data></Cell>
066600071121
066700071121
066800071121
