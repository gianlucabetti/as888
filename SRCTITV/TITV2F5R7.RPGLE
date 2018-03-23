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
001600140822     FAZORG01L  IF   E           K DISK
001700030924
001800080924     D fnvac00t      e ds
001900140308     D dar5gen       e ds
002000140308     D dar5emd       e ds
002100140311     D ds2A          e ds                  inz
002200071121
002300071121     D*------------
002400071121     D* DECODIFICHE COSTANTI
002500071121     D*------------
002600071121     D DEC             S             80    DIM(20) CTDATA PERRCD(1)
002700071121
002800071121
002900071121     D*------------
003000071121     D* COSTANTI X IMPLEMENTAZIONE FORMATO HTML/XLS
003100071121     D*------------
003200071121     D iHtml           s             10    inz('<html>')
003300071121     D fHtml           s             10    inz('</html>')
003400071121     D iBody           s             10    inz('<body>')
003500071121     D fBody           s             10    inz('</body>')
003600071121     D iTable          s             20    inz('<table border="1">')
003700071121     D fTable          s             10    inz('</table>')
003800071121     D iThead          s             10    inz('<thead>')
003900071121     D fThead          s             10    inz('</thead>')
004000071121     D iRow            s              5    inz('<tr>')
004100071121     D fRow            s              5    inz('</tr>')
004200071121     D iCol            s              5    inz('<td>')
004300071121     D fCol            s              5    inz('</td>')
004400071121     D iColBold        s             25    inz('<th bgcolor="#FFFF00">')
004500071121     D fColBold        s              5    inz('</th>')
004600071122     D cApi            s              1    inz('''')
004700071121
004800071121
004900071121     D*------------
005000071121     D* VARIABILI D WRK
005100071121     D*------------
005200030924     D prmppt          S             50
005300030924     D prmesito        S              1
005400030924     D wrkesito        S                   like(prmesito)
005500071122     D wBufferOut      s          10000    varying inz
005600100910     D wDataSts        S              8  0 inz(*zeros)
005700140308     D wDataStsAlfa    S             10    inz(*blanks)
005800100907     D wConsOkSiNo     S              1    inz(*blanks)
005900100907     D wConsOkSiNoDes  S            300    inz(*blanks)
006000140908     D wDescEvento     S             50    inz(*blanks)
006100100907     D wLinkDir        S            300    inz(*blanks)
006200110519     D wDSP            s             10    inz(*blanks)
006300110519     D wRD2            s                   inz(*blanks) like(ta4NOT)
006400110519     D wIND            s                   inz(*blanks) like(tasIND)
006500110519     D wCAD            s                   inz(*blanks) like(tasIND)
006600110519     D wLOD            s                   inz(*blanks) like(tasLOD)
006700110519     D wPRD            s                   inz(*blanks) like(tasPRD)
006800140127     D wNCD            s                   inz(*blanks) like(tasNCD)
006900140127     D wNCA            s                   inz(*blanks) like(tasNCA)
007000140822     D wLNA            s                   inz(*blanks) like(tasLNA)
007100140822     D wLNA_Full       s             25    inz(*blanks)
007200140127     D wSPEDBRT        s             12  0 inz
007300121207     D wRSCMORI        s                   inz(*blanks) like(taaRSC)
007400121207     D wINDMORI        s                   inz(*blanks) like(taaIND)
007500121207     D wCAPMORI        s                   inz(*blanks) like(taaCAP)
007600121207     D wLOCMORI        s                   inz(*blanks) like(taaLOC)
007700121207     D wPRVMORI        s                   inz(*blanks) like(taaPRV)
007800140129     D wFIRMATAR       s                   inz(*blanks) like(ta4NOT)
007900130426     D wKSC            s              7  0 inz(*zeros)
008000140308     D wNoteGiac       s            300    inz(*blanks)
008100140311     D wGCPCMC         s                   inz(*blanks) like(gcpCMC)
008200140311     D wGiacCau        s                   inz(*blanks) like(§2ADES)
008300071211     D*
008400100910
008500100910
008600100910     D*------------------
008700100910     D* LINKING A DEFINIZIONI ESTERNE
008800100910     D*------------------
008900100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
009000100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
009100140308     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
009200140308     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
009300130426
009400130426
009500130426     D*-------------------
009600130426     D* COSTANTI
009700130426     D*-------------------
009800130426     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009900130426     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010000100910
010100030924
010200030325
010300030924     C* MAIN/
010400140308     C*
010500071121     C* Verifico se già presenti dati nel file d output
010600071121     C                   EXSR      chkRECFILMBR
010700071121     C*
010800071121     C* Effettuo elaborazione
010900071121     C                   EXSR      traduci
011000071121     C*
011100921023     C                   SETON                                        LR
011200991027
011300071121
011400071121
011500071121     C     chkRECFILMBR  BEGSR
011600071121     C*
011700071121     C                   READ      TIVAWWWT                               55
011800071121     C*
011900071121     C                   ENDSR
012000071121
012100071121
012200030924
012300991027     C     traduci       BEGSR
012400071121     C*
012500071121     C* Head iniziali della struttura HTML
012600100908     C                   IF        %subst(prmppt:2:1) = 'H'
012700071210     C   55              EVAL      VAWDTA = iHtml + iBody + iTable
012800071210     C   55              WRITE     TIVAW000
012900100908     C                   ENDIF
013000071121     C*
013100071121     C* Inizializzazone formato e variabili x output
013200071121     C                   clear                   TIVAW000
013300071121     C                   clear                   wBufferOut
013400100907     C                   clear                   wLinkDir
013500100907     C                   clear                   wConsOkSiNo
013600100907     C                   clear                   wConsOkSiNoDes
013700140822     C                   z-add     *zeros        wCnt              6 0
013800071121     C*
013900071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
014000071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
014100100908     C*
014200100908     C                   SELECT
014300100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
014400071121     C                   EVAL      wBufferOut = iThead + iRow
014500140908     C                             +iColBold+ 'N'                      +fColBold
014600140908     C                             +iColBold+ 'Numero rif.spedizione'  +fColBold
014700140908     C                             +iColBold+ 'Lettera di vettura'     +fColBold
014800140908     C                             +iColBold+ 'Data ultimo status'     +fColBold
014900140908     C                             +iColBold+ 'Filiale ultimo status'  +fColBold
015000140908     C                             +iColBold+ 'Ultimo status'          +fColBold
015100140908     C                             +iColBold+ 'Riferimento segnacollo' +fColBold
015200140908     C                             +iColBold+ 'Destinatario'           +fColBold
015300140908     C                             +iColBold+ 'Destinazione'           +fColBold
015400140908     C                             +iColBold+ 'Awb'                    +fColBold
015500140822     C                             +iColBold+ 'ADR'                    +fColBold
015600100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
015700100908     C                   EVAL      wBufferOut =
015800140908     C                              '"'+      'N'                      +'";'
015900140908     C                             +'"'+      'Numero rif.spedizione'  +'";'
016000140908     C                             +'"'+      'Lettera di vettura'     +'";'
016100140908     C                             +'"'+      'Data ultimo status'     +'";'
016200140908     C                             +'"'+      'Filiale ultimo status'  +'";'
016300140908     C                             +'"'+      'Ultimo status'          +'";'
016400140908     C                             +'"'+      'Riferimento segnacollo' +'";'
016500140908     C                             +'"'+      'Destinatario'           +'";'
016600140908     C                             +'"'+      'Destinazione'           +'";'
016700140908     C                             +'"'+      'Awb'                    +'";'
016800140822     C                             +'"'+      'ADR'                    +'";'
016900140127     C                   ENDSL
017000100908     C*
017100071121     C                   EVAL      VAWDTA = wBufferOut
017200071121     C                   WRITE     TIVAW000
017300071121     C                   ENDIF
017400991027     C*
017500080924     C                   READ      TIVGDTMP
017600060119     C*
017700080924     C                   DOW       not %eof(TIVGDTMP)
017800140822     C*
017900080924     C                   eval      fnvac00t = vgdDTA
018000071121     C*
018100071121     C                   clear                   TIVAW000
018200071121     C                   clear                   wBufferOut
018300100907     C                   clear                   wLinkDir
018400100907     C                   clear                   wConsOkSiNo
018500100907     C                   clear                   wConsOkSiNoDes
018600140308     C                   clear                   dar5gen
018700140308     C                   clear                   dar5emd
018800100910     C*
018900100910     C* Solo bolle originali e valide ai fini della spedizione
019000100910     C                   movel     *blanks       wEsito1           1
019100100910     C*
019200100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019300100910     C                                                vacAAS
019400100910     C                                               :vacLNP
019500100910     C                                               :vacNRS
019600100910     C                                               :vacNSP
019700100910     C                                               :pOutLblTyp
019800100910     C                                               :pOutAnnoBO
019900100910     C                                               :pOutLineaParBO
020000100910     C                                               :pOutSerieBO
020100100910     C                                               :pOutNumSpedBO
020200100910     C                                               :pOutDcmBO
020300100910     C                                               :pOutCcaBO
020400100910     C                                               :pOutRblBO))
020500100910     C*
020600130426     C                   if         pOutRblBO ='N' AND
020700130426     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
020800071210     C*
020900071210     C* Inizializzo campi d wrk
021000140127     C                   EVAL      wDSP       = *blanks
021100140127     C                   EVAL      wRD2       = *blanks
021200140127     C                   EVAL      wIND       = *blanks
021300140127     C                   EVAL      wCAD       = *blanks
021400140127     C                   EVAL      wLOD       = *blanks
021500140127     C                   EVAL      wPRD       = *blanks
021600140127     C                   EVAL      wNCD       = *zeros
021700140127     C                   EVAL      wNCA       = *zeros
021800140822     C                   EVAL      wLNA       = *zeros
021900140822     C                   EVAL      wLNA_Full  = *blanks
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
023100140822     C*
023200140822     C* Compongo la chiave per agganciare la bolla in sede
023300140822     C                   eval      tasAAS = vacAAS
023400140822     C                   eval      tasLNP = vacLNP
023500140822     C                   eval      tasNRS = vacNRS
023600140822     C                   eval      tasNSP = vacNSP
023700140822     C*
023800140822     C* Chiamata metodo GetLastChild
023900140822     C                   movel     *blanks       wEsito2           1
024000140822     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
024100140822     C                                                vacAAS
024200140822     C                                               :vacLNP
024300140822     C                                               :vacNRS
024400140822     C                                               :vacNSP
024500140822     C                                               :pOutAnnoFI
024600140822     C                                               :pOutLineaParFI
024700140822     C                                               :pOutSerieFI
024800140822     C                                               :pOutNumSpedFI
024900140822     C                                               :pOutDcmFI
025000140822     C                                               :pOutCcaFI))
025100140822     C* Se ultima figlia esiste => overraido i dati correnti
025200140822     C                   if        wEsito2 = '0'
025300140822     C                   eval      tasAAS = pOutAnnoFI
025400140822     C                   eval      tasLNP = pOutLineaParFI
025500140822     C                   eval      tasNRS = pOutSerieFI
025600140822     C                   eval      tasNSP = pOutNumSpedFI
025700140822     C                   endif
025800121207     C*
025900071210     C* Aggancio la bolla relativa x il completamento dei dati
026000071210     C     KEYtas30      chain     titas30c
026100071210     C                   if        %found(titas30c)
026200110519     C*
026300121207     C* Reperisco l'estensione ragione sociale destinatario
026400110519     C                   eval      ta4TRC = 'D'
026500140129     C     KEYta430C     chain     tita430c
026600110519     C                   if        %found(tita430c)
026700110519     C                   eval      wRD2 = '.' + ta4NOT
026800110519     C                   endif
026900140127     C*
027000140127     C* Reperisco, se c'è, la firma del consegnatario
027100140129     C                   eval      ta4TRC = '1'
027200140129     C     KEYta430C     chain     tita430c
027300140129     C                   if        %found(tita430c)
027400140129     C                   eval      wFIRMATAR = ta4NOT
027500140127     C                   endif
027600140308     C*
027700140308     C* Reperisco, se c'è, il referente consegna ed il suo telefono
027800140308     C                   eval      ar5TRD = 'GEN'
027900140308     C     KEYar531_1P   chain     fiar531C
028000140308     C                   if        %found(FIAR531C)
028100140308     C                   eval      dar5gen = ar5UNI
028200140308     C                   endif
028300140308     C*
028400140308     C* Reperisco, se c'è, il cellulare del destinatario
028500140308     C                   eval      ar5TRD = 'EMD'
028600140308     C     KEYar531_1P   chain     fiar531C
028700140308     C                   if        %found(FIAR531C)
028800140308     C                   eval      dar5emd = ar5UNI
028900140308     C                   endif
029000071210     C*
029100071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
029200140308     C                   eval      wDSP     =
029300140308     C                              UBFMTDATE_Convert(
029400140308     C                                  %editc((tasAAS*10000+tasMGS):'4') :
029500140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
029600140127     C                   eval      wIND     = tasIND
029700140127     C                   eval      wCAD     = tasCAD
029800140127     C                   eval      wLOD     = tasLOD
029900140127     C                   eval      wPRD     = tasPRD
030000140127     C                   eval      wNCD     = tasNCD
030100140127     C                   eval      wNCA     = tasNCA
030200140127     C                   eval      wSPEDBRT = tasLNP*1000000000+
030300140127     C                                        tasNRS*10000000+
030400140127     C                                        tasNSP
030500140822     C                   eval      wLNA     = tasLNA
030600140822     C*
030700140822     C* Reperisco la descrizione della filiale di arrivo
030800140822     C                   eval      wLNA_Full = %editc(wLNA:'X')
030900140822     C     wLNA          chain     AZORG01L
031000140822     C                   if        %found(azorg01l)
031100140822     C                   eval      wLNA_Full = %trim(wLNA_Full) + ' ' +
031200140822     C                                         %trim(orgDES)
031300140822     C                   endif
031400121207     C*
031500121207     C* Reperisco i dati del mittente originale
031600140308     C***                eval      taaTRC = 'O'
031700140308     C***      aa30      chain     titaa30c
031800140308     C***                if        %found(titaa30c)
031900140308     C***                eval      wRSCMORI = taaRSC
032000140308     C***                eval      wINDMORI = taaIND
032100140308     C***                eval      wCAPMORI = taaCAP
032200140308     C***                eval      wLOCMORI = taaLOC
032300140308     C***                eval      wPRVMORI = taaPRV
032400140308     C***                endif
032500130913     C*
032600130913     C* Effettuo check sulle eventuali giacenze sulla spedizione corrente
032700130913     C* (costruisco la chiave sul file TIGCP51L)
032800130913     C*
032900140310     C                   EVAL      gcpDUR = *zeros
033000130913     C                   EVAL      gcpFRG = *zeros
033100130913     C     KEYgcp51      SETLL     tigcp51l
033200130913     C                   IF        %equal(tigcp51l)
033300130913     C     KEYgcp51      READE     tigcp51l
033400130913     C                   DOW       not %eof(tigcp51l)
033500130913     C*
033600130913     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
033700130913     C                   IF        gcpDUR > *zeros AND
033800130913     C                             gcpDDM = *zeros AND
033900130913     C                             gcpFAS < 030
034000130913     C                   seton                                        70
034100130913     C                   ENDIF
034200140311     C*
034300140311     C* Reperisco la causale di ultima apertura giacenza
034400140311     C                   EVAL      wGCPCMC = gcpCMC
034500130913     C*
034600130913     C* Proseguo la lettura delle giacenze x la bolla corrente
034700130913     C     KEYgcp51      READE     tigcp51l
034800130913     C                   ENDDO
034900140308     C*
035000140308     C* Se in giacenza reperisco le note di giacenza
035100140308     C                   EXSR      rtvNOTEGIAC
035200140308     C*
035300140308     C                   ENDIF
035400071213     C*
035500071213     C                   endif
035600071121     C*
035700071121     C* Compondo la stringa dati output relativa al record corrente
035800071121     C                   EXSR      wrifile
035900100910     C*
036000100910     C                   endif
036100040719     C*
036200040719     C* In ogni caso storicizzo e cancello l'input sempre
036300080924     C                   DELETE    TIVGD000
036400040719     C*
036500040719     C* Proseguo con la lettura dell'input
036600080924     C                   READ      TIVGDTMP
036700060119     C*
036800030325     C                   ENDDO
036900071121     C*
037000071121     C* Head finali della struttura HTML
037100071210     C***                EVAL      VAWDTA = fTable + fBody + fHtml
037200071210     C***                WRITE     TIVAW000
037300991027     C*
037400030325     C                   EVAL      wrkesito = '0'
037500991027     C*
037600910830     C                   ENDSR
037700100907
037800100907
037900100907
038000100907     C     calSTS        BEGSR
038100100910     C*
038200120926     C                   MOVEL     *blanks       wMIC              1
038300121114     C                   MOVEL     *blanks       wGIAC             1
038400121207     C                   MOVEL     *blanks       wLAVV             1
038500140129     C                   MOVEL     *blanks       wPEND             1
038600100910     C                   EVAL      wDataSts       = vacDCM
038700140308     C*
038800140308     C* Verifico lo stato delle disposizioni di giacenza
038900140308     C                   MOVEL     *blanks       wGiacDisp        10
039000140308     C   70              EVAL      wGiacDisp = 'IN ATTESA'
039100100907     C*
039200100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
039300100907     C                   IF        VACDCM > *zeros AND
039400100907     C                             (VACCCA = *blanks OR VACCCA = '1')
039500100907     C                   EVAL      wConsOkSiNo    = 'S'
039600140908     C                   EVAL      wConsOkSiNoDes =
039700140908     C                                    'LA SPEDIZIONE E´ STATA CONSEGNATA'
039800140308     C                   EVAL      wGiacDisp      = *blanks
039900140308     C                   EVAL      wNoteGiac      = *blanks
040000100907     C                   ELSE
040100100907     C                   EVAL      wConsOkSiNo    = 'N'
040200140311     C                   EVAL      wFIRMATAR      = *blanks
040300100907     C*
040400100907     C* A questo punto stabilisco la descrizione
040500100910     C                   SELECT
040600100910     C                   WHEN      vacCCA = '1'
040700110519     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
040800100910     C                   WHEN      vacCCA = '2'
040900140908     C                   EVAL      wConsOkSiNoDes =
041000140908     C                                   'RIFIUTATA - TORNATA AL MITTENTE'
041100100907     C                   EVAL      wConsOkSiNo    = 'R'
041200100910     C                   WHEN      vacCCA = '3'
041300110519     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
041400110519     C                   EVAL      wConsOkSiNo    = 'R'
041500100910     C                   WHEN      vacCCA = '4'
041600110519     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
041700110519     C                   EVAL      wConsOkSiNo    = 'R'
041800100910     C                   WHEN      vacCCA = '5'
041900110519     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
042000110519     C                   EVAL      wConsOkSiNo    = 'R'
042100100910     C                   WHEN      vacCCA = '6'
042200110519     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
042300110519     C                   EVAL      wConsOkSiNo    = 'R'
042400100910     C                   WHEN      vacCCA = 'A'
042500110519     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
042600110519     C                   EVAL      wConsOkSiNo    = 'R'
042700100910     C                   WHEN      vacCCA = 'C'
042800110519     C                   EVAL      wConsOkSiNoDes = 'In consegna'
042900120926     C                   EVAL      wMIC = 'S'
043000100910     C                   WHEN      vacCCA = 'R'
043100110519     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
043200100910     C                   WHEN      vacCCA = 'S'
043300110519     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
043400110519     C                   EVAL      wConsOkSiNo    = 'R'
043500100910     C                   ENDSL
043600100907     C*
043700140308     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
043800100910     C                   EVAL      wDataSts       = vacDLA
043900110519     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
044000121207     C                   EVAL      wLAVV = 'S'
044100100907     C                   ENDIF
044200140308     C                   IF        (gcpDUR > vacDCM AND gcpDUR >= vacDLA)
044300140308     C                   EVAL      wDataSts       = gcpDUR
044400110519     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
044500140311     C                   EXSR      rtvCAU_GIAC
044600121114     C                   EVAL      wGIAC = 'S'
044700140308     C                   ELSE
044800140308     C                   EVAL      wGiacDisp      = *blanks
044900140308     C                   EVAL      wNoteGiac      = *blanks
045000100907     C                   ENDIF
045100100910     C                   IF        vacDCM = *zeros AND
045200100910     C                             vacDLA = *zeros AND
045300140308     C                             gcpDUR = *zeros AND
045400100910     C                             vacCCA = *blanks
045500100910     C                   EVAL      wDataSts       = datcor
045600100910     C*
045700100910     C* Verifico la data consegna richiesta
045800100910     C                   IF        vacDCR > datcor
045900140308     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
046000140308     C                              UBFMTDATE_Convert(
046100140308     C                                  %editc(vacDCR:'4') :
046200140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
046300100910     C                   ELSE
046400110519     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
046500140129     C                   EVAL      wPEND          = 'S'
046600100910     C                   ENDIF
046700100910     C                   ENDIF
046800100907     C*
046900100907     C                   ENDIF
047000100907     C*
047100100907     C                   ENDSR
047200140311
047300140311
047400140311
047500140311     C     rtvCAU_GIAC   BEGSR
047600140311     C*
047700140311     C* Rererisco la descrizioe della causale di giacenza
047800140311     C                   MOVEL     '1'           tblKUT
047900140311     C                   MOVEL     '2A'          tblCOD
048000140311     C                   MOVEL(P)  wGCPCMC       tblKEY
048100140311     C     KEYtab00_C    CHAIN     TABEL00F                           30
048200140311     C                   IF        %found(tabel00f)
048300140311     C                   MOVEL     TBLUNI        ds2A
048400140311     C                   MOVEL(P)  §2ADES        wGiacCau
048500140311     C                   ENDIF
048600140311     C*
048700140311     C                   ENDSR
048800140311     C*------------------------------------------------------------------------*
048900140311
049000140308
049100140308
049200140308     C     rtvNOTEGIAC   BEGSR
049300140308     C*
049400140308     C* Scorro tutte le note giacenza relative alla giacebza corrente
049500140308     C     KEYgnp01_P    setll     tignp01l
049600140308     C                   if        %equal(tignp01l)
049700140308     C                   eval      wNoteGiac = ''''
049800140308     C     KEYgnp01_P    reade     tignp01l
049900140308     C                   dow       not %eof(tignp01l)
050000140308     C*** concateno note giacenza
050100140308     C                   eval      wNoteGiac =
050200140308     C                             %trim(wNoteGiac) + ' ' + gnpDMC
050300140308     C     KEYgnp01_P    reade     tignp01l
050400140308     C                   enddo
050500140308     C                   endif
050600140308     C*
050700140308     C                   ENDSR
050800100907
050900100907
051000100907
051100100907     C     makLNK        BEGSR
051200130426     C*
051300130426     C* Se richiesta inclusione link T&T
051400130426     C                   if        %subst(prmppt:6:7) <> *blanks
051500130426     C*
051600130426     C* Porto in minuscolo il parametro relativo alla lingua del link
051700130426     C                   movel     *blanks       parLang           2
051800130426     C                   eval      parLang = %subst(prmppt:13:2)
051900130426     C     maiu:minu     xlate     parLang       parLang
052000130426     C*
052100160927     C                   eval      wLinkDir = 'http://vas.brt.it/vas/' +
052200130426     C                                        'sped_ricdocmit_load.hsm?docmit='+
052300130426     C                                        %trim(%char(vacRMN))             +
052400130426     C                                        '&ksu='+%subst(prmppt:6:7)+
052500130426     C                                        '&lang='+parLang
052600130426     C*
052700130426     C                   endif
052800100907     C*
052900100907     C                   ENDSR
053000100907
053100071121
053200071121
053300071121
053400071121     C     wrifile       BEGSR
053500120926     C*
053600120926     C                   SETOFF                                       31
053700071121     C*
053800100907     C* Determino lo status della spedizione
053900100907     C                   EXSR      calSTS
054000100907     C*
054100100907     C* Costruisco il link diretto
054200130426     C                   EXSR      makLNK
054300130426     C*
054400130426     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
054500130426     C                   IF        %subst(prmppt:15:1) = 'P'
054600130426     C                   MOVE      vacCCM        CCM               4 0
054700130426     C                   MOVE      wKSC          KSC               4 0
054800130426     C                   IF        CCM =  8888 or  CCM =  9999
054900130426     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
055000130426     C                   EVAL      vacCCM = wKSC
055100130426     C                   ENDIF
055200130426     C                   ENDIF
055300130426     C                   ENDIF
055400140822     C*
055500140822     C                   add       1             wCnt
055600100908     C*
055700140308     C                   EVAL      wDataStsAlfa =
055800140308     C                              UBFMTDATE_Convert(
055900140308     C                                  %editc(wDataSts:'4') :
056000140308     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
056100140308     C*
056200140908     C                   EVAL      wDescEvento =
056300140908     C                                       %trim(wConsOkSiNoDes)        +' '+
056400140908     C***                                    %trim(wGiacDisp)             +' '+
056500140908     C                                       %trim(wGiacCau)              +' '+
056600140908     C                                       %trim(wNoteGiac)
056700140908     C*
056800100908     C                   SELECT
056900100908     C                   WHEN      %subst(prmppt:2:1) = 'H'
057000140822     C                   EVAL      wBufferOut = iRow +
057100140822     C                                  iCol+%trim(%editc(wCnt:'4'))      +fCol+
057200140919     C                             iCol+''''+%trim(vacRMA)                +fCol+
057300140909     C                             iCol+''''+%trim(%editc(wSPEDBRT:'X'))  +fCol+
057400140822     C                                  iCol+%trim(wDataStsAlfa)          +fCol+
057500140822     C                                  iCol+%trim(wLNA_Full)             +fCol+
057600140908     C                                  iCol+%trim(wDescEvento)           +fCol+
057700140909     C                                  iCol+''''                         +fCol+
057800100907     C                                  iCol+%trim(vacRSD)                +fCol+
057900140822     C                                  iCol+%trim(wLOD)                  +fCol+
058000140822     C***                               iCol+%trim(wFIRMATAR)             +fCol+
058100140822     C                                  iCol+''                           +fCol+
058200140822     C                                  iCol+''                           +fCol
058300100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
058400100908     C                   EVAL      wBufferOut =
058500140822     C                               '"'+   %trim(%editc(wCnt:'4'))       +'";'
058600140919     C                              +'"'''+ %trim(vacRMA)                 +'";'
058700140909     C                              +'"'''+ %trim(%editc(wSPEDBRT:'X'))   +'";'
058800140822     C                              +'"'+   %trim(wDataStsAlfa)           +'";'
058900140822     C                              +'"'+   %trim(wLNA_Full)              +'";'
059000140908     C                              +'"'+   %trim(wDescEvento)            +'";'
059100140909     C                              +'"'+   ''''                          +'";'
059200140822     C                              +'"'+   %trim(vacRSD)                 +'";'
059300140822     C                              +'"'+   %trim(wLOD)                   +'";'
059400140822     C***                           +'"'+   %trim(wFIRMATAR)              +'";'
059500140822     C                              +'"'+   ''                            +'";'
059600140822     C                              +'"'+   ''                            +'";'
059700100908     C                   ENDSL
059800071121     C*
059900120926     C* Se nei parametri è richiesto no MIC => gestisco
060000120926     C                   IF        %subst(prmppt:3:1) = 'N' and wMIC = 'S'
060100120926     C                   SETON                                        31
060200120926     C                   ENDIF
060300121114     C*
060400121207     C* Se nei parametri è richiesto solo GIACIENZE e L/AVV => gestisco
060500121207     C                   IF        %subst(prmppt:4:1) = 'S' and
060600121207     C                             wGIAC <> 'S' and wLAVV <> 'S'
060700121114     C                   SETON                                        31
060800121114     C                   ENDIF
060900140124     C*
061000140129     C* Se nei parametri è richiesto solo NO CONS "pending" => gestisco
061100140129     C                   IF        %subst(prmppt:5:1) = 'S' and wPEND <> 'S'
061200140124     C                   SETON                                        31
061300140124     C                   ENDIF
061400120926     C*
061500071121     C                   EVAL      VAWDTA = wBufferOut
061600110519     C*
061700110519     C* Restituisco solo le spedizioni "pendenti"
061800110713     C***                IF        wConsOkSiNo = 'N'
061900120926     C  N31              WRITE     TIVAW000
062000110713     C***                ENDIF
062100071121     C*
062200071121     C                   ENDSR
062300071121
062400071121
062500071121
062600991027
062700991027      /TITLE Operazioni iniziali.
062800991027     C     *inzsr        BEGSR
062900991027     C*
063000991027     C     *ENTRY        PLIST
063100991027     C                   parm                    prmppt
063200991027     C     wrkesito      parm      wrkesito      prmesito
063300100910     C*
063400100910     C* CALCOLA LA DATA CORRENTE
063500100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
063600100910     C                   eval      datcor = %dec(%date() : *ISO)
063700071210     C*
063800071213     C* TITAS30C - Parziale
063900071210     C     KEYtas30      KLIST
064000140822     C                   KFLD                    tasAAS
064100140822     C                   KFLD                    tasLNP
064200140822     C                   KFLD                    tasNRS
064300140822     C                   KFLD                    tasNSP
064400110519     C*
064500110519     C* TITA430C - Completa
064600140129     C     KEYta430C     KLIST
064700140822     C                   KFLD                    tasAAS
064800140822     C                   KFLD                    tasLNP
064900140822     C                   KFLD                    tasNRS
065000140822     C                   KFLD                    tasNSP
065100110519     C                   KFLD                    ta4TRC
065200121207     C*
065300121207     C* TITAA30C - Completa
065400121207     C     KEYtaa30      KLIST
065500140822     C                   KFLD                    tasAAS
065600140822     C                   KFLD                    tasLNP
065700140822     C                   KFLD                    tasNRS
065800140822     C                   KFLD                    tasNSP
065900121207     C                   KFLD                    taaTRC
066000130913     C*
066100130913     C* TIGCP51L - Completa
066200130913     C     KEYgcp51      KLIST
066300140822     C                   KFLD                    tasAAS
066400140822     C                   KFLD                    tasLNP
066500140822     C                   KFLD                    tasNRS
066600140822     C                   KFLD                    tasNSP
066700130913     C                   KFLD                    gcpFRG
066800140308     C*
066900140308     C* Chiave su FIAR531C - Parziale
067000140308     C     KEYar531_1P   KLIST
067100140822     C                   KFLD                    tasAAS
067200140822     C                   KFLD                    tasLNP
067300140822     C                   KFLD                    tasNRS
067400140822     C                   KFLD                    tasNSP
067500140308     C                   KFLD                    ar5TRD
067600140308     C*
067700140308     C* Chiave su TIGNP01L - Parziale
067800140308     C     KEYgnp01_P    KLIST
067900140308     C                   KFLD                    gcpAGC
068000140308     C                   KFLD                    gcpFGC
068100140308     C                   KFLD                    gcpNGC
068200140308     C                   KFLD                    gcpFRG
068300140311     C*
068400140311     C* TABEL00F - Completa
068500140311     C     KEYtab00_C    KLIST
068600140311     C                   KFLD                    tblKUT
068700140311     C                   KFLD                    tblCOD
068800140311     C                   KFLD                    tblKEY
068900991027     C*
069000991027     C                   ENDSR
069100071121     C*
069200071121** DEC - COSTANTI
069300071121<?xml version="1.0"?>
069400071121<?mso-application progid="Excel.Sheet"?>
069500071121<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"
069600071121xmlns:o="urn:schemas-microsoft-com:office:office"
069700071121xmlns:x="urn:schemas-microsoft-com:office:excel"
069800071121xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"
069900071121xmlns:html="http://www.w3.org/TR/REC-html40">
070000071121</Workbook>
070100071121<Worksheet ss:Name="PAG1">
070200071121</Worksheet>
070300071121<Table border="1">
070400071121</Table>
070500071121<Row>
070600071121</Row>
070700071121<Cell><Data ss:Type="String">
070800071121<Cell><Data ss:Type="Number">
070900071121</Data></Cell>
071000071121
071100071121
071200071121
