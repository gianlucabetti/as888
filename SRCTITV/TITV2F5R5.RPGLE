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
014200110519     C                             +iColBold+ 'LINEA ARRIVO'           +fColBold
014300121207     C                             +iColBold+ 'RAG.SOC. DESTINATARIO'  +fColBold
014400140308     C                             +iColBold+ 'TEL. DESTINATARIO'      +fColBold
014500140308     C                             +iColBold+ 'REFERENTE CONSEGNA'     +fColBold
014600140308     C                             +iColBold+ 'TEL. REFERENTE'         +fColBold
014700110519     C                             +iColBold+ 'INDIRIZZO DESTINATARIO' +fColBold
014800110519     C                             +iColBold+ 'CAP DESTINATARIO'       +fColBold
014900110519     C                             +iColBold+ 'LOCALITA DESTINATARIO'  +fColBold
015000110519     C                             +iColBold+ 'PROVINCIA DESTINATARIO' +fColBold
015100110519     C                             +iColBold+ 'RIFERIMENTO SPED. NUM'  +fColBold
015200110519     C                             +iColBold+ 'RIFERIMENTO SPED. ALFA' +fColBold
015300140308     C***                          +iColBold+ 'RAG.SOC. MITTENTE ORIG.'+fColBold
015400140308     C***                          +iColBold+ 'INDIRIZ. MITTENTE ORIG.'+fColBold
015500140308     C***                          +iColBold+ 'CAP MITTENTE ORIG.'     +fColBold
015600140308     C***                          +iColBold+ 'LOCALITA MITTENTE ORIG.'+fColBold
015700140308     C***                          +iColBold+ 'PROV. MITTENTE ORIG.'   +fColBold
015800140308     C                             +iColBold+ 'DATA STATO'             +fColBold
015900110519     C                             +iColBold+ 'STATO'                  +fColBold
016000130913     C                             +iColBold+ 'DISPOSIZIONI GIAC.'     +fColBold
016100140311     C                             +iColBold+ 'CAUSALE GIAC.'          +fColBold
016200140308     C                             +iColBold+ 'NOTE STATO'             +fColBold
016300140308     C***                          +iColBold+ 'SEGNACOLLO DA'          +fColBold
016400140308     C***                          +iColBold+ 'SEGNACOLLO A'           +fColBold
016500140308     C                             +iColBold+ 'LDV BRT'                +fColBold
016600140127     C                             +iColBold+ 'FIRMA CONSEGNA'         +fColBold
016700100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
016800100908     C                   EVAL      wBufferOut =
016900110519     C                              '"'+      'CODICE CLIENTE'         +'";'
017000110519     C                             +'"'+      'DATA SPEDIZIONE'        +'";'
017100110519     C                             +'"'+      'LINEA ARRIVO'           +'";'
017200140308     C                             +'"'+      'RAG.SOC. DESTINATARIO'  +'";'
017300140308     C                             +'"'+      'TEL. DESTINATARIO'      +'";'
017400140308     C                             +'"'+      'REFERENTE CONSEGNA'     +'";'
017500140308     C                             +'"'+      'TEL. REFERENTE'         +'";'
017600110519     C                             +'"'+      'INDIRIZZO DESTINATARIO' +'";'
017700110519     C                             +'"'+      'CAP DESTINATARIO'       +'";'
017800110519     C                             +'"'+      'LOCALITA DESTINATARIO'  +'";'
017900110519     C                             +'"'+      'PROVINCIA DESTINATARIO' +'";'
018000110519     C                             +'"'+      'RIFERIMENTO SPED. NUM'  +'";'
018100110519     C                             +'"'+      'RIFERIMENTO SPED. ALFA' +'";'
018200140308     C***                          +'"'+      'RAG.SOC. MITTENTE ORIG.'+'";'
018300140308     C***                          +'"'+      'INDIRIZ. MITTENTE ORIG.'+'";'
018400140308     C***                          +'"'+      'CAP MITTENTE ORIG.'     +'";'
018500140308     C***                          +'"'+      'LOCALITA MITTENTE ORIG.'+'";'
018600140308     C***                          +'"'+      'PROV. MITTENTE ORIG.'   +'";'
018700140308     C                             +'"'+      'DATA STATO'             +'";'
018800110519     C                             +'"'+      'STATO'                  +'";'
018900130913     C                             +'"'+      'DISPOSIZIONI GIAC.'     +'";'
019000140311     C                             +'"'+      'CAUSALE GIAC.'          +'";'
019100140308     C                             +'"'+      'NOTE STATO'             +'";'
019200140308     C***                          +'"'+      'SEGNACOLLO DA'          +'";'
019300140308     C***                          +'"'+      'SEGNACOLLO A'           +'";'
019400140308     C                             +'"'+      'LDV BRT'                +'";'
019500140127     C                             +'"'+      'FIRMA CONSEGNA'         +'";'
019600140127     C                   ENDSL
019700100908     C*
019800071121     C                   EVAL      VAWDTA = wBufferOut
019900071121     C                   WRITE     TIVAW000
020000071121     C                   ENDIF
020100991027     C*
020200080924     C                   READ      TIVGDTMP
020300060119     C*
020400080924     C                   DOW       not %eof(TIVGDTMP)
020500080924     C*
020600080924     C                   eval      fnvac00t = vgdDTA
020700071121     C*
020800071121     C                   clear                   TIVAW000
020900071121     C                   clear                   wBufferOut
021000100907     C                   clear                   wLinkDir
021100100907     C                   clear                   wConsOkSiNo
021200100907     C                   clear                   wConsOkSiNoDes
021300140308     C                   clear                   dar5gen
021400140308     C                   clear                   dar5emd
021500100910     C*
021600100910     C* Solo bolle originali e valide ai fini della spedizione
021700100910     C                   movel     *blanks       wEsito1           1
021800100910     C*
021900100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
022000100910     C                                                vacAAS
022100100910     C                                               :vacLNP
022200100910     C                                               :vacNRS
022300100910     C                                               :vacNSP
022400100910     C                                               :pOutLblTyp
022500100910     C                                               :pOutAnnoBO
022600100910     C                                               :pOutLineaParBO
022700100910     C                                               :pOutSerieBO
022800100910     C                                               :pOutNumSpedBO
022900100910     C                                               :pOutDcmBO
023000100910     C                                               :pOutCcaBO
023100100910     C                                               :pOutRblBO))
023200100910     C*
023300130426     C                   if         pOutRblBO ='N' AND
023400130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
023500071210     C*
023600071210     C* Inizializzo campi d wrk
023700140127     C                   EVAL      wDSP       = *blanks
023800140127     C                   EVAL      wRD2       = *blanks
023900140127     C                   EVAL      wIND       = *blanks
024000140127     C                   EVAL      wCAD       = *blanks
024100140127     C                   EVAL      wLOD       = *blanks
024200140127     C                   EVAL      wPRD       = *blanks
024300140127     C                   EVAL      wNCD       = *zeros
024400140127     C                   EVAL      wNCA       = *zeros
024500140127     C                   EVAL      wSPEDBRT   = *zeros
024600140127     C                   EVAL      wRSCMORI   = *blanks
024700140127     C                   EVAL      wINDMORI   = *blanks
024800140127     C                   EVAL      wCAPMORI   = *blanks
024900140127     C                   EVAL      wLOCMORI   = *blanks
025000140127     C                   EVAL      wPRVMORI   = *blanks
025100140129     C                   EVAL      wFIRMATAR  = *blanks
025200140308     C                   EVAL      wNoteGiac  = *blanks
025300140311     C                   EVAL      wGCPCMC    = *blanks
025400140311     C                   EVAL      wGiacCau   = *blanks
025500140308     C                   setoff                                       70
025600121207     C*
025700071210     C* Aggancio la bolla relativa x il completamento dei dati
025800071210     C     KEYtas30      chain     titas30c
025900071210     C                   if        %found(titas30c)
026000110519     C*
026100121207     C* Reperisco l'estensione ragione sociale destinatario
026200110519     C                   eval      ta4TRC = 'D'
026300140129     C     KEYta430C     chain     tita430c
026400110519     C                   if        %found(tita430c)
026500110519     C                   eval      wRD2 = '.' + ta4NOT
026600110519     C                   endif
026700140127     C*
026800140127     C* Reperisco, se c'è, la firma del consegnatario
026900140129     C                   eval      ta4TRC = '1'
027000140129     C     KEYta430C     chain     tita430c
027100140129     C                   if        %found(tita430c)
027200140129     C                   eval      wFIRMATAR = ta4NOT
027300140127     C                   endif
027400140308     C*
027500140308     C* Reperisco, se c'è, il referente consegna ed il suo telefono
027600140308     C                   eval      ar5TRD = 'GEN'
027700140308     C     KEYar531_1P   chain     fiar531C
027800140308     C                   if        %found(FIAR531C)
027900140308     C                   eval      dar5gen = ar5UNI
028000140308     C                   endif
028100140308     C*
028200140308     C* Reperisco, se c'è, il cellulare del destinatario
028300140308     C                   eval      ar5TRD = 'EMD'
028400140308     C     KEYar531_1P   chain     fiar531C
028500140308     C                   if        %found(FIAR531C)
028600140308     C                   eval      dar5emd = ar5UNI
028700140308     C                   endif
028800071210     C*
028900071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
029000140308     C                   eval      wDSP     =
029100140308     C                              UBFMTDATE_Convert(
029200140308     C                                  %editc((tasAAS*10000+tasMGS):'4') :
029300140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
029400140127     C                   eval      wIND     = tasIND
029500140127     C                   eval      wCAD     = tasCAD
029600140127     C                   eval      wLOD     = tasLOD
029700140127     C                   eval      wPRD     = tasPRD
029800140127     C                   eval      wNCD     = tasNCD
029900140127     C                   eval      wNCA     = tasNCA
030000140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
030100140127     C                                        tasNRS*10000000+
030200140127     C                                        tasNSP
030300121207     C*
030400121207     C* Reperisco i dati del mittente originale
030500140308     C***                eval      taaTRC = 'O'
030600140308     C***      aa30      chain     titaa30c
030700140308     C***                if        %found(titaa30c)
030800140308     C***                eval      wRSCMORI = taaRSC
030900140308     C***                eval      wINDMORI = taaIND
031000140308     C***                eval      wCAPMORI = taaCAP
031100140308     C***                eval      wLOCMORI = taaLOC
031200140308     C***                eval      wPRVMORI = taaPRV
031300140308     C***                endif
031400130913     C*
031500130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
031600130913     C* (costruisco la chiave sul file TIGCP51L)
031700130913     C*
031800140310     C                   EVAL      gcpDUR = *zeros
031900130913     C                   EVAL      gcpFRG = *zeros
032000130913     C     KEYgcp51      SETLL     tigcp51l
032100130913     C                   IF        %equal(tigcp51l)
032200130913     C     KEYgcp51      READE     tigcp51l
032300130913     C                   DOW       not %eof(tigcp51l)
032400130913     C*
032500130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
032600130913     C                   IF        gcpDUR > *zeros AND
032700130913     C                             gcpDDM = *zeros AND
032800130913     C                             gcpFAS < 030
032900130913     C                   seton                                        70
033000130913     C                   ENDIF
033100140311     C*
033200140311     C* Reperisco la causale di ultima apertura giacenza
033300140311     C                   EVAL      wGCPCMC = gcpCMC
033400130913     C*
033500130913     C* Proseguo la lettura delle giacenze x la bolla corrente
033600130913     C     KEYgcp51      READE     tigcp51l
033700130913     C                   ENDDO
033800140308     C*
033900140308     C* Se in giacenza reperisco le note di giacenza
034000140308     C                   EXSR      rtvNOTEGIAC
034100140308     C*
034200140308     C                   ENDIF
034300071213     C*
034400071213     C                   endif
034500071121     C*
034600071121     C* Compondo la stringa dati output relativa al record corrente
034700071121     C                   EXSR      wrifile
034800100910     C*
034900100910     C                   endif
035000040719     C*
035100040719     C* In ogni caso storicizzo e cancello l'input sempre
035200080924     C                   DELETE    TIVGD000
035300040719     C*
035400040719     C* Proseguo con la lettura dell'input
035500080924     C                   READ      TIVGDTMP
035600060119     C*
035700030325     C                   ENDDO
035800071121     C*
035900071121     C* Head finali della struttura HTML
036000071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
036100071210     C***                WRITE     TIVAW000
036200991027     C*
036300030325     C                   EVAL      wrkesito = '0'
036400991027     C*
036500910830     C                   ENDSR
036600100907
036700100907
036800100907
036900100907     C     calSTS        BEGSR
037000100910     C*
037100120926     C                   MOVEL     *blanks       wMIC              1
037200121114     C                   MOVEL     *blanks       wGIAC             1
037300121207     C                   MOVEL     *blanks       wLAVV             1
037400140129     C                   MOVEL     *blanks       wPEND             1
037500100910     C                   EVAL      wDataSts       = vacDCM
037600140308     C*
037700140308     C* Verifico lo stato delle disposizioni di giacenza
037800140308     C                   MOVEL     *blanks       wGiacDisp        10
037900140308     C   70              EVAL      wGiacDisp = 'IN ATTESA'
038000100907     C*
038100100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
038200100907     C                   IF        VACDCM > *zeros AND
038300100907     C                             (VACCCA = *blanks OR VACCCA = '1')
038400100907     C                   EVAL      wConsOkSiNo    = 'S'
038500110519     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
038600140308     C                   EVAL      wGiacDisp      = *blanks
038700140308     C                   EVAL      wNoteGiac      = *blanks
038800100907     C                   ELSE
038900100907     C                   EVAL      wConsOkSiNo    = 'N'
039000140311     C                   EVAL      wFIRMATAR      = *blanks
039100100907     C*
039200100907     C* A questo punto stabilisco la descrizione
039300100910     C                   SELECT
039400100910     C                   WHEN      vacCCA = '1'
039500110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
039600100910     C                   WHEN      vacCCA = '2'
039700110519     C                   EVAL      wConsOkSiNoDes = 'Resa'
039800100907     C                   EVAL      wConsOkSiNo    = 'R'
039900100910     C                   WHEN      vacCCA = '3'
040000110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
040100110519     C                   EVAL      wConsOkSiNo    = 'R'
040200100910     C                   WHEN      vacCCA = '4'
040300110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
040400110519     C                   EVAL      wConsOkSiNo    = 'R'
040500100910     C                   WHEN      vacCCA = '5'
040600110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
040700110519     C                   EVAL      wConsOkSiNo    = 'R'
040800100910     C                   WHEN      vacCCA = '6'
040900110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
041000110519     C                   EVAL      wConsOkSiNo    = 'R'
041100100910     C                   WHEN      vacCCA = 'A'
041200110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
041300110519     C                   EVAL      wConsOkSiNo    = 'R'
041400100910     C                   WHEN      vacCCA = 'C'
041500110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
041600120926     C                   EVAL      wMIC = 'S'
041700100910     C                   WHEN      vacCCA = 'R'
041800110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
041900100910     C                   WHEN      vacCCA = 'S'
042000110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
042100110519     C                   EVAL      wConsOkSiNo    = 'R'
042200100910     C                   ENDSL
042300100907     C*
042400140308     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
042500100910     C                   EVAL      wDataSts       = vacDLA
042600110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
042700121207     C                   EVAL      wLAVV = 'S'
042800100907     C                   ENDIF
042900140308     C                   IF        (gcpDUR > vacDCM AND gcpDUR >= vacDLA)
043000140308     C                   EVAL      wDataSts       = gcpDUR
043100110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
043200140311     C                   EXSR      rtvCAU_GIAC
043300121114     C                   EVAL      wGIAC = 'S'
043400140308     C                   ELSE
043500140308     C                   EVAL      wGiacDisp      = *blanks
043600140308     C                   EVAL      wNoteGiac      = *blanks
043700100907     C                   ENDIF
043800100910     C                   IF        vacDCM = *zeros AND
043900100910     C                             vacDLA = *zeros AND
044000140308     C                             gcpDUR = *zeros AND
044100100910     C                             vacCCA = *blanks
044200100910     C                   EVAL      wDataSts       = datcor
044300100910     C*
044400100910     C* Verifico la data consegna richiesta
044500100910     C                   IF        vacDCR > datcor
044600140308     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
044700140308     C                              UBFMTDATE_Convert(
044800140308     C                                  %editc(vacDCR:'4') :
044900140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
045000100910     C                   ELSE
045100110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
045200140129     C                   EVAL      wPEND          = 'S'
045300100910     C                   ENDIF
045400100910     C                   ENDIF
045500100907     C*
045600100907     C                   ENDIF
045700100907     C*
045800100907     C                   ENDSR
045900140311
046000140311
046100140311
046200140311     C     rtvCAU_GIAC   BEGSR
046300140311     C*
046400140311     C* Rererisco la descrizioe della causale di giacenza
046500140311     C                   MOVEL     '1'           tblKUT
046600140311     C                   MOVEL     '2A'          tblCOD
046700140311     C                   MOVEL(P)  wGCPCMC       tblKEY
046800140311     C     KEYtab00_C    CHAIN     TABEL00F                           30
046900140311     C                   IF        %found(tabel00f)
047000140311     C                   MOVEL     TBLUNI        ds2A
047100140311     C                   MOVEL(P)  §2ADES        wGiacCau
047200140311     C                   ENDIF
047300140311     C*
047400140311     C                   ENDSR
047500140311     C*------------------------------------------------------------------------*
047600140311
047700140308
047800140308
047900140308     C     rtvNOTEGIAC   BEGSR
048000140308     C*
048100140308     C* Scorro tutte le note giacenza relative alla giacebza corrente
048200140308     C     KEYgnp01_P    setll     tignp01l
048300140308     C                   if        %equal(tignp01l)
048400140308     C                   eval      wNoteGiac = ''''
048500140308     C     KEYgnp01_P    reade     tignp01l
048600140308     C                   dow       not %eof(tignp01l)
048700140308     C*** concateno note giacenza
048800140308     C                   eval      wNoteGiac =
048900140308     C                             %trim(wNoteGiac) + ' ' + gnpDMC
049000140308     C     KEYgnp01_P    reade     tignp01l
049100140308     C                   enddo
049200140308     C                   endif
049300140308     C*
049400140308     C                   ENDSR
049500100907
049600100907
049700100907
049800100907     C     makLNK        BEGSR
049900130426     C*
050000130426     C* Se richiesta inclusione link T&T
050100130426     C                   if        %subst(prmppt:6:7) <> *blanks
050200130426     C*
050300130426     C* Porto in minuscolo il parametro relativo alla lingua del link
050400130426     C                   movel     *blanks       parLang           2
050500130426     C                   eval      parLang = %subst(prmppt:13:2)
050600130426     C     maiu:minu     xlate     parLang       parLang
050700130426     C*
050800160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
050900130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
051000130426     C                                        %trim(%char(vacRMN))             +
051100130426     C                                        '&ksu='+%subst(prmppt:6:7)+
051200130426     C                                        '&lang='+parLang
051300130426     C*
051400130426     C                   endif
051500100907     C*
051600100907     C                   ENDSR
051700100907
051800071121
051900071121
052000071121
052100071121     C     wrifile       BEGSR
052200120926     C*
052300120926     C                   SETOFF                                       31
052400071121     C*
052500100907     C* Determino lo status della spedizione
052600100907     C                   EXSR      calSTS
052700100907     C*
052800100907     C* Costruisco il link diretto
052900130426     C                   EXSR      makLNK
053000130426     C*
053100130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
053200130426     C                   IF        %subst(prmppt:15:1) = 'P'
053300130426     C                   MOVE      vacCCM        CCM               4 0
053400130426     C                   MOVE      wKSC          KSC               4 0
053500130426     C                   IF        CCM =  8888 or  CCM =  9999
053600130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
053700130426     C                   EVAL      vacCCM = wKSC
053800130426     C                   ENDIF
053900130426     C                   ENDIF
054000130426     C                   ENDIF
054100100908     C*
054200140308     C                   EVAL      wDataStsAlfa =
054300140308     C                              UBFMTDATE_Convert(
054400140308     C                                  %editc(wDataSts:'4') :
054500140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
054600140308     C*
054700100908     C                   SELECT
054800100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
054900071121     C                   EVAL      wBufferOut = iRow +
055000100910     C                                  iCol+%trim(%editc(vacCCM:'X'))    +fCol+
055100140308     C                                  iCol+%trim(wDSP)                  +fCol+
055200110519     C                                  iCol+%trim(%editc(vacLNA:'X'))    +fCol+
055300100907     C                                  iCol+%trim(vacRSD)                +fCol+
055400140308     C                                  iCol+%trim(§AR5TEL)               +fCol+
055500140308     C                                  iCol+%trim(§AR5REF)               +fCol+
055600140308     C                                  iCol+%trim(§AR5TELD)              +fCol+
055700110519     C                                  iCol+%trim(wIND)                  +fCol+
055800100517     C                                  iCol+%trim(wCAD)                  +fCol+
055900100517     C                                  iCol+%trim(wLOD)                  +fCol+
056000110519     C                                  iCol+%trim(wPRD)                  +fCol+
056100100907     C                                  iCol+%trim(%editc(vacRMN:'Q'))    +fCol+
056200110519     C                                  iCol+%trim(vacRMA)                +fCol+
056300140308     C***                               iCol+%trim(wRSCMORI)              +fCol+
056400140308     C***                               iCol+%trim(wINDMORI)              +fCol+
056500140308     C***                               iCol+%trim(wCAPMORI)              +fCol+
056600140308     C***                               iCol+%trim(wLOCMORI)              +fCol+
056700140308     C***                               iCol+%trim(wPRVMORI)              +fCol+
056800140308     C                                  iCol+%trim(wDataStsAlfa)          +fCol+
056900100907     C                                  iCol+%trim(wConsOkSiNoDes)        +fCol+
057000130913     C                                  iCol+%trim(wGiacDisp)             +fCol+
057100140311     C                                  iCol+%trim(wGiacCau)              +fCol+
057200140308     C                                  iCol+%trim(wNoteGiac)             +fCol+
057300140308     C***                               iCol+%trim(%editc(wNCD:'X'))      +fCol+
057400140308     C***                               iCol+%trim(%editc(wNCA:'X'))      +fCol+
057500140127     C                                  iCol+%trim(%editc(wSPEDBRT:'X'))  +fCol+
057600140129     C                                  iCol+%trim(wFIRMATAR)             +fCol
057700100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
057800100908     C                   EVAL      wBufferOut =
057900110519     C                               '"'+   %trim(%editc(vacCCM:'X'))     +'";'
058000140308     C                              +'"'+   %trim(wDSP)                   +'";'
058100110519     C                              +'"'+   %trim(%editc(vacLNA:'X'))     +'";'
058200110519     C                              +'"'+   %trim(vacRSD)                 +'";'
058300140308     C                              +'"'+   %trim(§AR5TEL)                +'";'
058400140308     C                              +'"'+   %trim(§AR5REF)                +'";'
058500140308     C                              +'"'+   %trim(§AR5TELD)               +'";'
058600110519     C                              +'"'+   %trim(wIND)                   +'";'
058700110519     C                              +'"'+   %trim(wCAD)                   +'";'
058800110519     C                              +'"'+   %trim(wLOD)                   +'";'
058900110519     C                              +'"'+   %trim(wPRD)                   +'";'
059000110519     C                              +'"'+   %trim(%editc(vacRMN:'Q'))     +'";'
059100110519     C                              +'"'+   %trim(vacRMA)                 +'";'
059200140308     C***                           +'"'+   %trim(wRSCMORI)               +'";'
059300140308     C***                           +'"'+   %trim(wINDMORI)               +'";'
059400140308     C***                           +'"'+   %trim(wCAPMORI)               +'";'
059500140308     C***                           +'"'+   %trim(wLOCMORI)               +'";'
059600140308     C***                           +'"'+   %trim(wPRVMORI)               +'";'
059700140308     C                              +'"'+   %trim(wDataStsAlfa)           +'";'
059800110519     C                              +'"'+   %trim(wConsOkSiNoDes)         +'";'
059900130913     C                              +'"'+   %trim(wGiacDisp)              +'";'
060000140311     C                              +'"'+   %trim(wGiacCau)               +'";'
060100140308     C                              +'"'+   %trim(wNoteGiac)              +'";'
060200140308     C***                           +'"'+   %trim(%editc(wNCD:'X'))       +'";'
060300140308     C***                           +'"'+   %trim(%editc(wNCA:'X'))       +'";'
060400140127     C                              +'"'+   %trim(%editc(wSPEDBRT:'X'))   +'";'
060500140129     C                              +'"'+   %trim(wFIRMATAR)              +'";'
060600100908     C                   ENDSL
060700071121     C*
060800120926     C* Se nei parametri è richiesto no MIC => gestisco
060900120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
061000120926     C                   SETON                                        31
061100120926     C                   ENDIF
061200121114     C*
061300121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
061400121207     C                   IF        %subst(prmppt:4:1) = 'S' and
061500121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
061600121114     C                   SETON                                        31
061700121114     C                   ENDIF
061800140124     C*
061900140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
062000140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
062100140124     C                   SETON                                        31
062200140124     C                   ENDIF
062300120926     C*
062400071121     C                   EVAL      VAWDTA = wBufferOut
062500110519     C*
062600110519     C* Restituisco solo le spedizioni "pendenti"
062700110713     C***                IF        wConsOkSiNo = 'N'
062800120926     C  N31              WRITE     TIVAW000
062900110713     C***                ENDIF
063000071121     C*
063100071121     C                   ENDSR
063200071121
063300071121
063400071121
063500991027
063600991027      /TITLE Operazioni iniziali.
063700991027     C     *inzsr        BEGSR
063800991027     C*
063900991027     C     *ENTRY        PLIST
064000991027     C                   parm                    prmppt
064100991027     C     wrkesito      parm      wrkesito      prmesito
064200100910     C*
064300100910     C* CALCOLA LA DATA CORRENTE
064400100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
064500100910     C                   eval      datcor = %dec(%date() : *ISO)
064600071210     C*
064700071213     C* TITAS30C - Parziale
064800071210     C     KEYtas30      KLIST
064900071210     C                   KFLD                    vacAAS
065000071210     C                   KFLD                    vacLNP
065100071210     C                   KFLD                    vacNRS
065200071210     C                   KFLD                    vacNSP
065300110519     C*
065400110519     C* TITA430C - Completa
065500140129     C     KEYta430C     KLIST
065600110519     C                   KFLD                    vacAAS
065700110519     C                   KFLD                    vacLNP
065800110519     C                   KFLD                    vacNRS
065900110519     C                   KFLD                    vacNSP
066000110519     C                   KFLD                    ta4TRC
066100121207     C*
066200121207     C* TITAA30C - Completa
066300121207     C     KEYtaa30      KLIST
066400121207     C                   KFLD                    vacAAS
066500121207     C                   KFLD                    vacLNP
066600121207     C                   KFLD                    vacNRS
066700121207     C                   KFLD                    vacNSP
066800121207     C                   KFLD                    taaTRC
066900130913     C*
067000130913     C* TIGCP51L - Completa
067100130913     C     KEYgcp51      KLIST
067200130913     C                   KFLD                    vacAAS
067300130913     C                   KFLD                    vacLNP
067400130913     C                   KFLD                    vacNRS
067500130913     C                   KFLD                    vacNSP
067600130913     C                   KFLD                    gcpFRG
067700140308     C*
067800140308     C* Chiave su FIAR531C - Parziale
067900140308     C     KEYar531_1P   KLIST
068000140308     C                   KFLD                    vacAAS
068100140308     C                   KFLD                    vacLNP
068200140308     C                   KFLD                    vacNRS
068300140308     C                   KFLD                    vacNSP
068400140308     C                   KFLD                    ar5TRD
068500140308     C*
068600140308     C* Chiave su TIGNP01L - Parziale
068700140308     C     KEYgnp01_P    KLIST
068800140308     C                   KFLD                    gcpAGC
068900140308     C                   KFLD                    gcpFGC
069000140308     C                   KFLD                    gcpNGC
069100140308     C                   KFLD                    gcpFRG
069200140311     C*
069300140311     C* TABEL00F - Completa
069400140311     C     KEYtab00_C    KLIST
069500140311     C                   KFLD                    tblKUT
069600140311     C                   KFLD                    tblCOD
069700140311     C                   KFLD                    tblKEY
069800991027     C*
069900991027     C                   ENDSR
070000071121     C*
070100071121** DEC - COSTANTI
070200071121<?xml version="1.0"?>
070300071121<?mso-application progid="Excel.Sheet"?>
070400071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
070500071121xmlns:o="urn:schemas-microsoft-com:office:office"
070600071121xmlns:x="urn:schemas-microsoft-com:office:excel"
070700071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
070800071121xmlns:html="http://www.w3.org/TR/REC-html40">
070900071121</Workbook>
071000071121<Worksheet ss:Name="PAG1">
071100071121</Worksheet>
071200071121<Table border="1">
071300071121</Table>
071400071121<Row>
071500071121</Row>
071600071121<Cell><Data ss:Type="String">
071700071121<Cell><Data ss:Type="Number">
071800071121</Data></Cell>
071900071121
072000071121
072100071121
