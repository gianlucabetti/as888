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
001300071121
001400071121     D*------------
001500071121     D* VARIABILI D WRK
001600071121     D*------------
001700030924     D prmppt          S             50
001800030924     D prmesito        S              1
001900030924     D wrkesito        S                   like(prmesito)
002000071122     D wBufferOut      s          10000    varying inz
002100100910     D wDataSts        S              8  0 inz(*zeros)
002200100907     D wConsOkSiNo     S              1    inz(*blanks)
002300100907     D wConsOkSiNoDes  S            300    inz(*blanks)
002400130129     D wConsOkSiNoCod  S             15    inz(*blanks)
002500100517     D wCAD            s              9    inz(*blanks)
002600071210     D wLOD            s             35    inz(*blanks)
002700120713     D wKSC            s              7  0 inz(*zeros)
002800071211     D*
002900100910
003000100910
003100100910     D*------------------
003200100910     D* LINKING A DEFINIZIONI ESTERNE
003300100910     D*------------------
003400100910     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003500100910     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003600100910
003700120427     D*-------------------
003800120427     D* COSTANTI
003900120427     D*-------------------
004000120427     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004100120427     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004200120427
004300030924
004400030325
004500030924     C* MAIN/
004600071121     C*
004700071121     C* Verifico se già presenti dati nel file d output
004800071121     C                   EXSR      chkRECFILMBR
004900071121     C*
005000071121     C* Effettuo elaborazione
005100071121     C                   EXSR      traduci
005200071121     C*
005300921023     C                   SETON                                        LR
005400991027
005500071121
005600071121
005700071121     C     chkRECFILMBR  BEGSR
005800071121     C*
005900071121     C                   READ      TIVAWWWT                               55
006000071121     C*
006100071121     C                   ENDSR
006200071121
006300071121
006400030924
006500991027     C     traduci       BEGSR
006600071121     C*
006700071121     C* Inizializzazone formato e variabili x output
006800071121     C                   clear                   TIVAW000
006900071121     C                   clear                   wBufferOut
007000100907     C                   clear                   wConsOkSiNo
007100100907     C                   clear                   wConsOkSiNoDes
007200130129     C                   clear                   wConsOkSiNoCod
007300071121     C*
007400071121     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007500071121     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
007600100908     C*
007700100908     C                   SELECT
007800100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
007900100908     C                   EVAL      wBufferOut =
008000100910     C                              '"'+'CUSTOMER CODE'          +'";'
008100100910     C                             +'"'+'DESTINATION NAME'       +'";'
008200100908     C                             +'"'+'ZIP CODE'               +'";'
008300100908     C                             +'"'+'CITY'                   +'";'
008400100908     C                             +'"'+'SENDER REF. ID'         +'";'
008500100908     C                             +'"'+'STATUS'                 +'";'
008600130212     C                             +'"'+'STATUS DATE'            +'"'
008700100908     C                   ENDSL
008800100908     C*
008900071121     C                   EVAL      VAWDTA = wBufferOut
009000071121     C                   WRITE     TIVAW000
009100071121     C                   ENDIF
009200991027     C*
009300080924     C                   READ      TIVGDTMP
009400060119     C*
009500080924     C                   DOW       not %eof(TIVGDTMP)
009600080924     C*
009700080924     C                   eval      fnvac00t = vgdDTA
009800071121     C*
009900071121     C                   clear                   TIVAW000
010000071121     C                   clear                   wBufferOut
010100100907     C                   clear                   wConsOkSiNo
010200100907     C                   clear                   wConsOkSiNoDes
010300130129     C                   clear                   wConsOkSiNoCod
010400100910     C*
010500120713     C* Solo bolle originali (oppure se diversamente richiesto nei parametri)
010600120713     C* e valide ai fini della spedizione
010700100910     C                   movel     *blanks       wEsito1           1
010800100910     C*
010900100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
011000100910     C                                                vacAAS
011100100910     C                                               :vacLNP
011200100910     C                                               :vacNRS
011300100910     C                                               :vacNSP
011400100910     C                                               :pOutLblTyp
011500100910     C                                               :pOutAnnoBO
011600100910     C                                               :pOutLineaParBO
011700100910     C                                               :pOutSerieBO
011800100910     C                                               :pOutNumSpedBO
011900100910     C                                               :pOutDcmBO
012000100910     C                                               :pOutCcaBO
012100100910     C                                               :pOutRblBO))
012200100910     C*
012300120713     C                   if         pOutRblBO ='N' AND
012400120713     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
012500071210     C*
012600071210     C* Inizializzo campi d wrk
012700100517     C                   EVAL      wCAD = *blanks
012800071213     C                   EVAL      wLOD = *blanks
012900120713     C                   EVAL      wKSC = *zeros
013000071210     C*
013100071210     C* Aggancio la bolla relativa x il completamento dei dati
013200071210     C     KEYtas30      chain     titas30c
013300071210     C                   if        %found(titas30c)
013400071210     C*
013500071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
013600100517     C                   EVAL      wCAD = tasCAD
013700071213     C                   EVAL      wLOD = tasLOD
013800120713     C                   EVAL      wKSC = tasKSC
013900071213     C*
014000071213     C                   endif
014100071121     C*
014200071121     C* Compondo la stringa dati output relativa al record corrente
014300071121     C                   EXSR      wrifile
014400100910     C*
014500100910     C                   endif
014600040719     C*
014700040719     C* In ogni caso storicizzo e cancello l'input sempre
014800080924     C                   DELETE    TIVGD000
014900040719     C*
015000040719     C* Proseguo con la lettura dell'input
015100080924     C                   READ      TIVGDTMP
015200060119     C*
015300030325     C                   ENDDO
015400991027     C*
015500030325     C                   EVAL      wrkesito = '0'
015600991027     C*
015700910830     C                   ENDSR
015800100907
015900100907
016000100907
016100100907     C     calSTS        BEGSR
016200100910     C*
016300100910     C                   EVAL      wDataSts       = vacDCM
016400100907     C*
016500100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
016600100907     C                   IF        VACDCM > *zeros AND
016700100907     C                             (VACCCA = *blanks OR VACCCA = '1')
016800100907     C                   EVAL      wConsOkSiNo    = 'S'
016900100907     C                   EVAL      wConsOkSiNoDes = 'Delivered'
017000130129     C                   EVAL      wConsOkSiNoCod = '100'
017100100907     C                   ELSE
017200100907     C                   EVAL      wConsOkSiNo    = 'N'
017300100907     C*
017400100907     C* A questo punto stabilisco la descrizione
017500100910     C                   SELECT
017600100910     C                   WHEN      vacCCA = '1'
017700100907     C                   EVAL      wConsOkSiNoDes = 'Diverted'
017800130129     C                   EVAL      wConsOkSiNoCod = '301'
017900100910     C                   WHEN      vacCCA = '2'
018000100907     C                   EVAL      wConsOkSiNoDes = 'Returned'
018100130129     C                   EVAL      wConsOkSiNoCod = '302'
018200100907     C                   EVAL      wConsOkSiNo    = 'R'
018300100910     C                   WHEN      vacCCA = '3'
018400100907     C                   EVAL      wConsOkSiNoDes = 'Goods Stolen'
018500130129     C                   EVAL      wConsOkSiNoCod = '303'
018600100910     C                   WHEN      vacCCA = '4'
018700100907     C                   EVAL      wConsOkSiNoDes = 'Payment Stolen'
018800130129     C                   EVAL      wConsOkSiNoCod = '304'
018900100910     C                   WHEN      vacCCA = '5'
019000100907     C                   EVAL      wConsOkSiNoDes = 'Closed on hold report'
019100130129     C                   EVAL      wConsOkSiNoCod = '305'
019200100910     C                   WHEN      vacCCA = '6'
019300100907     C                   EVAL      wConsOkSiNoDes = 'Damage and Returned'
019400130129     C                   EVAL      wConsOkSiNoCod = '306'
019500120427     C                   WHEN      vacCCA = '7'
019600120427     C                   EVAL      wConsOkSiNoDes = 'Goods never entrusted us'
019700130129     C                   EVAL      wConsOkSiNoCod = '307'
019800100910     C                   WHEN      vacCCA = 'A'
019900100907     C                   EVAL      wConsOkSiNoDes = 'New on hold report'
020000130129     C                   EVAL      wConsOkSiNoCod = '308'
020100100910     C                   WHEN      vacCCA = 'C'
020200100907     C                   EVAL      wConsOkSiNoDes = 'In delivery'
020300130129     C                   EVAL      wConsOkSiNoCod = '201'
020400100910     C                   WHEN      vacCCA = 'R'
020500100907     C                   EVAL      wConsOkSiNoDes = 'Reset shipment'
020600130129     C                   EVAL      wConsOkSiNoCod = '310'
020700100910     C                   WHEN      vacCCA = 'S'
020800100907     C                   EVAL      wConsOkSiNoDes = 'Destroyed Goods'
020900130129     C                   EVAL      wConsOkSiNoCod = '311'
021000100910     C                   ENDSL
021100100907     C*
021200100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
021300100910     C                   EVAL      wDataSts       = vacDLA
021400120427     C                   EVAL      wConsOkSiNoDes = 'Delivery attempt failed' +
021500120427     C                                              ' - Left Notice'
021600130129     C                   EVAL      wConsOkSiNoCod = '312'
021700100907     C                   ENDIF
021800100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
021900100910     C                   EVAL      wDataSts       = vacDAG
022000100907     C                   EVAL      wConsOkSiNoDes = 'On-Hold'
022100130129     C                   EVAL      wConsOkSiNoCod = '313'
022200100907     C                   ENDIF
022300100910     C                   IF        vacDCM = *zeros AND
022400100910     C                             vacDLA = *zeros AND
022500100910     C                             vacDAG = *zeros AND
022600100910     C                             vacCCA = *blanks
022700100910     C                   EVAL      wDataSts       = datcor
022800100910     C*
022900100910     C* Verifico la data consegna richiesta
023000100910     C                   IF        vacDCR > datcor
023100100910     C                   EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
023200100910     C                                              'on ' + %editc(vacDCR:'X')
023300130129     C                   EVAL      wConsOkSiNoCod = '314'
023400100910     C                   ELSE
023500100910     C                   EVAL      wConsOkSiNoDes = 'In progress'
023600130129     C                   EVAL      wConsOkSiNoCod = '202'
023700100910     C                   ENDIF
023800100910     C                   ENDIF
023900100907     C*
024000100907     C                   ENDIF
024100120427     C*
024200120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
024300120427     C                   IF        vacCCA = '1'  AND
024400120427     C                             wDataSts = *zeros
024500120427     C                   EXSR      REPDIR
024600120427     C                   ENDIF
024700100907     C*
024800100907     C                   ENDSR
024900120427
025000120427
025100120427
025200120427     C*------------------------------------------------------------------------*
025300120427     C* REPDIR - Reperimento data DIROTTAMENTO
025400120427     C*------------------------------------------------------------------------*
025500120427     C     REPDIR        BEGSR
025600120427     C*
025700120427     C                   movel     *blanks       wEsito1           1
025800120427     C*
025900120427     C* Chiamata metodo GetLblTyp
026000120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
026100120427     C                                                vacAAS
026200120427     C                                               :vacLNP
026300120427     C                                               :vacNRS
026400120427     C                                               :vacNSP
026500120427     C                                               :pOutLblTyp
026600120427     C                                               :pOutAnnoBO
026700120427     C                                               :pOutLineaParBO
026800120427     C                                               :pOutSerieBO
026900120427     C                                               :pOutNumSpedBO
027000120427     C                                               :pOutDcmBO
027100120427     C                                               :pOutCcaBO
027200120427     C                                               :pOutRblBO))
027300120427     C*
027400120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
027500120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
027600120427     C                   eval      wDataSts = pOutDcmBO
027700120427     C                   endif
027800120427     C*
027900120427     C                   ENDSR
028000120427     C*------------------------------------------------------------------------*
028100100907
028200100907
028300100907
028400100907
028500071121
028600071121
028700071121
028800071121     C     wrifile       BEGSR
028900071121     C*
029000100907     C* Determino lo status della spedizione
029100100907     C                   EXSR      calSTS
029200100908     C*
029300120713     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
029400120713     C                   IF        %subst(prmppt:15:1) = 'P'
029500120713     C                   MOVE      vacCCM        CCM               4 0
029600120713     C                   MOVE      wKSC          KSC               4 0
029700120713     C                   IF        CCM =  8888 or  CCM =  9999
029800120713     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
029900120713     C                   EVAL      vacCCM = wKSC
030000120713     C                   ENDIF
030100120713     C                   ENDIF
030200120713     C                   ENDIF
030300120713     C*
030400100908     C                   SELECT
030500100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
030600100908     C                   EVAL      wBufferOut =
030700100910     C                               '"'+%trim(%editc(vacCCM:'X'))        +'";'
030800100910     C                              +'"'+%trim(vacRSD)                    +'";'
030900100910     C                              +'"'+%trim(wCAD)                      +'";'
031000100910     C                              +'"'+%trim(wLOD)                      +'";'
031100130126     C                              +'"'+%trim(vacRMA)                    +'";'
031200130129     C                              +'"'+%trim(wConsOkSiNoCod)            +'";'
031300130212     C                              +'"'+%trim(%editc(wDataSts:'X'))      +'"'
031400100908     C                   ENDSL
031500071121     C*
031600071121     C                   EVAL      VAWDTA = wBufferOut
031700071121     C                   WRITE     TIVAW000
031800071121     C*
031900071121     C                   ENDSR
032000071121
032100071121
032200071121
032300991027
032400991027      /TITLE Operazioni iniziali.
032500991027     C     *inzsr        BEGSR
032600991027     C*
032700991027     C     *ENTRY        PLIST
032800991027     C                   parm                    prmppt
032900991027     C     wrkesito      parm      wrkesito      prmesito
033000100910     C*
033100100910     C* CALCOLA LA DATA CORRENTE
033200100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
033300100910     C                   eval      datcor = %dec(%date() : *ISO)
033400071210     C*
033500071213     C* TITAS30C - Parziale
033600071210     C     KEYtas30      KLIST
033700071210     C                   KFLD                    vacAAS
033800071210     C                   KFLD                    vacLNP
033900071210     C                   KFLD                    vacNRS
034000071210     C                   KFLD                    vacNSP
034100991027     C*
034200991027     C                   ENDSR
034300071121     C*
