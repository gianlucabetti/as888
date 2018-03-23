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
008100130312     C                             +'"'+'CARRIER SHIPMENT ID'    +'";'
008200100910     C                             +'"'+'DESTINATION NAME'       +'";'
008300100908     C                             +'"'+'ZIP CODE'               +'";'
008400100908     C                             +'"'+'CITY'                   +'";'
008500100908     C                             +'"'+'SENDER REF. ID'         +'";'
008600100908     C                             +'"'+'STATUS'                 +'";'
008700130312     C                             +'"'+'STATUS DESCRIPTION'     +'";'
008800130312     C                             +'"'+'STATUS DATE'            +'";'
008900130312     C                             +'"'+'UPDATE TIMESTAMP'       +'"'
009000100908     C                   ENDSL
009100100908     C*
009200071121     C                   EVAL      VAWDTA = wBufferOut
009300071121     C                   WRITE     TIVAW000
009400071121     C                   ENDIF
009500991027     C*
009600080924     C                   READ      TIVGDTMP
009700060119     C*
009800080924     C                   DOW       not %eof(TIVGDTMP)
009900080924     C*
010000080924     C                   eval      fnvac00t = vgdDTA
010100071121     C*
010200071121     C                   clear                   TIVAW000
010300071121     C                   clear                   wBufferOut
010400100907     C                   clear                   wConsOkSiNo
010500100907     C                   clear                   wConsOkSiNoDes
010600130129     C                   clear                   wConsOkSiNoCod
010700100910     C*
010800120713     C* Solo bolle originali (oppure se diversamente richiesto nei parametri)
010900120713     C* e valide ai fini della spedizione
011000100910     C                   movel     *blanks       wEsito1           1
011100100910     C*
011200100910     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
011300100910     C                                                vacAAS
011400100910     C                                               :vacLNP
011500100910     C                                               :vacNRS
011600100910     C                                               :vacNSP
011700100910     C                                               :pOutLblTyp
011800100910     C                                               :pOutAnnoBO
011900100910     C                                               :pOutLineaParBO
012000100910     C                                               :pOutSerieBO
012100100910     C                                               :pOutNumSpedBO
012200100910     C                                               :pOutDcmBO
012300100910     C                                               :pOutCcaBO
012400100910     C                                               :pOutRblBO))
012500100910     C*
012600120713     C                   if         pOutRblBO ='N' AND
012700120713     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
012800071210     C*
012900071210     C* Inizializzo campi d wrk
013000100517     C                   EVAL      wCAD = *blanks
013100071213     C                   EVAL      wLOD = *blanks
013200120713     C                   EVAL      wKSC = *zeros
013300071210     C*
013400071210     C* Aggancio la bolla relativa x il completamento dei dati
013500071210     C     KEYtas30      chain     titas30c
013600071210     C                   if        %found(titas30c)
013700071210     C*
013800071210     C* Reperisco i dati del destinatario nn già presenti sul FNVAC
013900100517     C                   EVAL      wCAD = tasCAD
014000071213     C                   EVAL      wLOD = tasLOD
014100120713     C                   EVAL      wKSC = tasKSC
014200071213     C*
014300071213     C                   endif
014400071121     C*
014500071121     C* Compondo la stringa dati output relativa al record corrente
014600071121     C                   EXSR      wrifile
014700100910     C*
014800100910     C                   endif
014900040719     C*
015000040719     C* In ogni caso storicizzo e cancello l'input sempre
015100080924     C                   DELETE    TIVGD000
015200040719     C*
015300040719     C* Proseguo con la lettura dell'input
015400080924     C                   READ      TIVGDTMP
015500060119     C*
015600030325     C                   ENDDO
015700991027     C*
015800030325     C                   EVAL      wrkesito = '0'
015900991027     C*
016000910830     C                   ENDSR
016100100907
016200100907
016300100907
016400100907     C     calSTS        BEGSR
016500100910     C*
016600100910     C                   EVAL      wDataSts       = vacDCM
016700100907     C*
016800100907     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
016900100907     C                   IF        VACDCM > *zeros AND
017000100907     C                             (VACCCA = *blanks OR VACCCA = '1')
017100100907     C                   EVAL      wConsOkSiNo    = 'S'
017200100907     C                   EVAL      wConsOkSiNoDes = 'Delivered'
017300130129     C                   EVAL      wConsOkSiNoCod = '100'
017400100907     C                   ELSE
017500100907     C                   EVAL      wConsOkSiNo    = 'N'
017600100907     C*
017700100907     C* A questo punto stabilisco la descrizione
017800100910     C                   SELECT
017900100910     C                   WHEN      vacCCA = '1'
018000100907     C                   EVAL      wConsOkSiNoDes = 'Diverted'
018100130129     C                   EVAL      wConsOkSiNoCod = '301'
018200100910     C                   WHEN      vacCCA = '2'
018300100907     C                   EVAL      wConsOkSiNoDes = 'Returned'
018400130129     C                   EVAL      wConsOkSiNoCod = '302'
018500100907     C                   EVAL      wConsOkSiNo    = 'R'
018600100910     C                   WHEN      vacCCA = '3'
018700100907     C                   EVAL      wConsOkSiNoDes = 'Goods Stolen'
018800130129     C                   EVAL      wConsOkSiNoCod = '303'
018900100910     C                   WHEN      vacCCA = '4'
019000100907     C                   EVAL      wConsOkSiNoDes = 'Payment Stolen'
019100130129     C                   EVAL      wConsOkSiNoCod = '304'
019200100910     C                   WHEN      vacCCA = '5'
019300100907     C                   EVAL      wConsOkSiNoDes = 'Closed on hold report'
019400130129     C                   EVAL      wConsOkSiNoCod = '305'
019500100910     C                   WHEN      vacCCA = '6'
019600100907     C                   EVAL      wConsOkSiNoDes = 'Damage and Returned'
019700130129     C                   EVAL      wConsOkSiNoCod = '306'
019800120427     C                   WHEN      vacCCA = '7'
019900120427     C                   EVAL      wConsOkSiNoDes = 'Goods never entrusted us'
020000130129     C                   EVAL      wConsOkSiNoCod = '307'
020100100910     C                   WHEN      vacCCA = 'A'
020200100907     C                   EVAL      wConsOkSiNoDes = 'New on hold report'
020300130129     C                   EVAL      wConsOkSiNoCod = '308'
020400100910     C                   WHEN      vacCCA = 'C'
020500100907     C                   EVAL      wConsOkSiNoDes = 'In delivery'
020600130129     C                   EVAL      wConsOkSiNoCod = '201'
020700100910     C                   WHEN      vacCCA = 'R'
020800100907     C                   EVAL      wConsOkSiNoDes = 'Reset shipment'
020900130129     C                   EVAL      wConsOkSiNoCod = '310'
021000100910     C                   WHEN      vacCCA = 'S'
021100100907     C                   EVAL      wConsOkSiNoDes = 'Destroyed Goods'
021200130129     C                   EVAL      wConsOkSiNoCod = '311'
021300100910     C                   ENDSL
021400100907     C*
021500100907     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
021600100910     C                   EVAL      wDataSts       = vacDLA
021700120427     C                   EVAL      wConsOkSiNoDes = 'Delivery attempt failed' +
021800120427     C                                              ' - Left Notice'
021900130129     C                   EVAL      wConsOkSiNoCod = '312'
022000100907     C                   ENDIF
022100100907     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
022200100910     C                   EVAL      wDataSts       = vacDAG
022300100907     C                   EVAL      wConsOkSiNoDes = 'On-Hold'
022400130129     C                   EVAL      wConsOkSiNoCod = '313'
022500100907     C                   ENDIF
022600100910     C                   IF        vacDCM = *zeros AND
022700100910     C                             vacDLA = *zeros AND
022800100910     C                             vacDAG = *zeros AND
022900100910     C                             vacCCA = *blanks
023000100910     C                   EVAL      wDataSts       = datcor
023100100910     C*
023200100910     C* Verifico la data consegna richiesta
023300100910     C                   IF        vacDCR > datcor
023400100910     C                   EVAL      wConsOkSiNoDes = 'Requested delivery date ' +
023500100910     C                                              'on ' + %editc(vacDCR:'X')
023600130129     C                   EVAL      wConsOkSiNoCod = '314'
023700100910     C                   ELSE
023800100910     C                   EVAL      wConsOkSiNoDes = 'In progress'
023900130129     C                   EVAL      wConsOkSiNoCod = '202'
024000100910     C                   ENDIF
024100100910     C                   ENDIF
024200100907     C*
024300100907     C                   ENDIF
024400120427     C*
024500120427     C* Effettuo particolari operazioni in caso d DIROTTAMENTO
024600120427     C                   IF        vacCCA = '1'  AND
024700120427     C                             wDataSts = *zeros
024800120427     C                   EXSR      REPDIR
024900120427     C                   ENDIF
025000100907     C*
025100100907     C                   ENDSR
025200120427
025300120427
025400120427
025500120427     C*------------------------------------------------------------------------*
025600120427     C* REPDIR - Reperimento data DIROTTAMENTO
025700120427     C*------------------------------------------------------------------------*
025800120427     C     REPDIR        BEGSR
025900120427     C*
026000120427     C                   movel     *blanks       wEsito1           1
026100120427     C*
026200120427     C* Chiamata metodo GetLblTyp
026300120427     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
026400120427     C                                                vacAAS
026500120427     C                                               :vacLNP
026600120427     C                                               :vacNRS
026700120427     C                                               :vacNSP
026800120427     C                                               :pOutLblTyp
026900120427     C                                               :pOutAnnoBO
027000120427     C                                               :pOutLineaParBO
027100120427     C                                               :pOutSerieBO
027200120427     C                                               :pOutNumSpedBO
027300120427     C                                               :pOutDcmBO
027400120427     C                                               :pOutCcaBO
027500120427     C                                               :pOutRblBO))
027600120427     C*
027700120427     C* Se reperimento effettuato e trattasi d dirottamento => recupero la data
027800120427     C                   if        wEsito1 = '0' and pOutCcaBO = '1'
027900120427     C                   eval      wDataSts = pOutDcmBO
028000120427     C                   endif
028100120427     C*
028200120427     C                   ENDSR
028300120427     C*------------------------------------------------------------------------*
028400100907
028500100907
028600100907
028700100907
028800071121
028900071121
029000071121
029100071121     C     wrifile       BEGSR
029200071121     C*
029300100907     C* Determino lo status della spedizione
029400100907     C                   EXSR      calSTS
029500100908     C*
029600120713     C* Se richiesto nei parametri verifico il porto (franco / assegnato)
029700120713     C                   IF        %subst(prmppt:15:1) = 'P'
029800120713     C                   MOVE      vacCCM        CCM               4 0
029900120713     C                   MOVE      wKSC          KSC               4 0
030000120713     C                   IF        CCM =  8888 or  CCM =  9999
030100120713     C                   IF        KSC <> 0000 and KSC <> 8888 and KSC <> 9999
030200120713     C                   EVAL      vacCCM = wKSC
030300120713     C                   ENDIF
030400120713     C                   ENDIF
030500120713     C                   ENDIF
030600120713     C*
030700100908     C                   SELECT
030800100908     C                   WHEN      %subst(prmppt:2:1) = 'C'
030900100908     C                   EVAL      wBufferOut =
031000130312     C                               '"'+%trim(%editc(vacCCM:'X'))         +'";'
031100130308     C                              +'"'+%trim(%editc(vacLNP:'X'))
031200130308     C                                  +%trim(%editc(vacNRS:'X'))
031300130312     C                                  +%trim(%editc(vacNSP:'X'))         +'";'
031400130312     C                              +'"'+%trim(vacRSD)                     +'";'
031500130312     C                              +'"'+%trim(wCAD)                       +'";'
031600130312     C                              +'"'+%trim(wLOD)                       +'";'
031700130312     C                              +'"'+%trim(vacRMA)                     +'";'
031800130312     C                              +'"'+%trim(wConsOkSiNoCod)             +'";'
031900130312     C                              +'"'+%trim(wConsOkSiNoDes)             +'";'
032000130312     C                              +'"'+%trim(%editc(wDataSts:'X'))       +'";'
032100130312     C                              +'"'+%trim(%char(%dec(%timestamp:*ISO)))+'"'
032200100908     C                   ENDSL
032300071121     C*
032400071121     C                   EVAL      VAWDTA = wBufferOut
032500071121     C                   WRITE     TIVAW000
032600071121     C*
032700071121     C                   ENDSR
032800071121
032900071121
033000071121
033100991027
033200991027      /TITLE Operazioni iniziali.
033300991027     C     *inzsr        BEGSR
033400991027     C*
033500991027     C     *ENTRY        PLIST
033600991027     C                   parm                    prmppt
033700991027     C     wrkesito      parm      wrkesito      prmesito
033800100910     C*
033900100910     C* CALCOLA LA DATA CORRENTE
034000100910     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
034100100910     C                   eval      datcor = %dec(%date() : *ISO)
034200071210     C*
034300071213     C* TITAS30C - Parziale
034400071210     C     KEYtas30      KLIST
034500071210     C                   KFLD                    vacAAS
034600071210     C                   KFLD                    vacLNP
034700071210     C                   KFLD                    vacNRS
034800071210     C                   KFLD                    vacNSP
034900991027     C*
035000991027     C                   ENDSR
035100071121     C*
