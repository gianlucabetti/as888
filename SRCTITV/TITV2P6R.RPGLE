000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200130523     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400130520     FTITAA30C  IF   E           K DISK
000500130521     FTITAS30C  IF   E           K DISK
000600050908     FTIVGDTMP  UF   E             DISK
000700101018     FTIVAWWWT  UF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200130518
001300130518     D fnvacds       e ds                  extname(fnvac00t) inz
001400130518     D fnvabds       e ds                  extname(fnvab00t) inz
001500130518     D titv2p6d0     e ds                  inz
001600130518     D titv2p6d1     e ds                  inz
001700130518     D titv2p6d2     e ds                  inz
001800130520     D titv2p6d9     e ds                  inz
001900130520     D wTotRec         s             10S 0 inz
002000130520     D wRec1           s             10S 0 inz
002100130520     D wRec2           s             10S 0 inz
002200130520     D pTIP            s              2A   inz
002300130518     D pKSC            s              7A   inz
002400130518     D xAAS            s                   like(vabAAS) inz
002500130518     D xLNP            s                   like(vabLNP) inz
002600130518     D xNRS            s                   like(vabNRS) inz
002700130520     D xNSP            s                   like(vabNSP) inz
002800130520     D wRSCMITT        s                   inz(*blanks) like(taaRSC)
002900130520     D wINDMITT        s                   inz(*blanks) like(taaIND)
003000130520     D wCAPMITT        s                   inz(*blanks) like(taaCAP)
003100130520     D wLOCMITT        s                   inz(*blanks) like(taaLOC)
003200130520     D wPRVMITT        s                   inz(*blanks) like(taaPRV)
003300130520     D wDataSts        S              8  0 inz(*zeros)
003400030325
003500130521
003600130521     D*------------------
003700130521     D* DS REPERIMENTO ATTRIBUTI CODICE CLIENTE
003800130521     D*------------
003900130521     D BS69DS        E DS                  extname(TIBS69DS)
004000130521     D ACODS         E DS                  extname(CNACO00F)
004100130521     D INDDS         E DS                  extname(CNIND00F)
004200130521     D CLPDS         E DS                  extname(CNCLP00F)
004300130521     D CLSDS         E DS                  extname(FNCLS00F)
004400130523
004500130523
004600130523     D*------------------
004700130523     D* LINKING A DEFINIZIONI ESTERNE
004800130523     D*------------------
004900130523     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005000130523     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005100130523
005200101018
005300101018
005400130520     C*
005500130520     C* Inizializzo i contatori generali
005600130520     C                   clear                   wTotRec
005700130520     C                   clear                   wRec1
005800130520     C                   clear                   wRec2
005900130520     C*
006000030908     C                   EXSR      chkRECFILMBR
006100130520     C                   EXSR      wriRec0
006200991027     C                   EXSR      traduci
006300130520     C                   EXSR      wriRec9
006400130518     C*
006500921023     C                   SETON                                        LR
006600101018
006700130518
006800101018
006900030908     C     chkRECFILMBR  BEGSR
007000030908     C*
007100130518     C     *start        SETLL     TIVAWWWT
007200130518     C                   DOW       not %eof(TIVAWWWT)
007300101018     C                   READ      TIVAW000                               55
007400130518     C  N55              ADD       1             wTotRec
007500130518     C                   ENDDO
007600030908     C*
007700030908     C                   ENDSR
007800130518
007900130518
008000130518
008100130520     C     wriRec0       BEGSR
008200130518     C*
008300130518     C                   clear                   titv2p6d0
008400130518     C                   eval      D0TPRCD    = '0'
008500130518     C                   eval      D0NMFILE   = 'BASI'
008600130518     C                   eval      D0DTFILE   = %editc(datcor:'X')
008700130520     C                   Z-Add     *zeros        wGiorni           3 0
008800130520     C                   eval      wGiorni    =  %diff(%date(datcor):
008900130520     C                                           %date(datYTD):*DAYS)
009000130520     C                   eval      D0NRGIORNI = %editc(wGiorni:'X')
009100130518     C                   eval      D0TMGEN    = %editc(oracor:'X')
009200130520     C*
009300130520     C                   eval      vawDTA = titv2p6d0
009400130607     C                   write     TIVAW000
009500130518     C                   add       1             wTotRec
009600130518     C*
009700130518     C                   ENDSR
009800101018
009900130518
010000991027
010100991027     C     traduci       BEGSR
010200080918     C*
010300130518     C* Ridefinisco i parametri in ingresso
010400130518     C                   eval      pTIP = %subst(prmppt:1:2)
010500130518     C                   eval      pKSC = %subst(prmppt:3:7)
010600991027     C*
010700050908     C                   READ      TIVGDTMP
010800050908     C                   DOW       not %eof(TIVGDTMP)
010900130520     C*
011000130520     C* In relazione al tipo file ricevuto nei parametri => ridefinisco il dato
011100130520     C                   clear                   fnvabds
011200130520     C                   clear                   fnvacds
011300130607     C                   setoff                                       31
011400130520     C
011500130520     C                   select
011600130520     C*
011700130520     C                   when      pTIP = 'BT'
011800130520     C                   eval      fnvabds = vgdDTA
011900130520     C                   eval      xAAS    = vabAAS
012000130520     C                   eval      xLNP    = vabLNP
012100130520     C                   eval      xNRS    = vabNRS
012200130520     C                   eval      xNSP    = vabNSP
012300130521     C                   exsr      rtvBoll
012400130520     C                   exsr      rtvMitt
012500130520     C                   exsr      wriRec1
012600130520     C*
012700130520     C                   when      pTIP = 'VC'
012800130520     C                   eval      fnvacds = vgdDTA
012900130520     C                   eval      xAAS    = vacAAS
013000130520     C                   eval      xLNP    = vacLNP
013100130520     C                   eval      xNRS    = vacNRS
013200130520     C                   eval      xNSP    = vacNSP
013300130520     C                   exsr      wriRec2
013400130520     C*
013500130520     C                   endsl
013600030325     C*
013700050908     C                   DELETE    TIVGD000
013800991027     C*
013900050908     C                   READ      TIVGDTMP
014000030325     C                   ENDDO
014100991027     C*
014200030325     C                   EVAL      wrkesito = '0'
014300991027     C*
014400910830     C                   ENDSR
014500130521
014600130521
014700130521
014800130521     C     rtvBoll       BEGSR
014900130521     C*
015000130521     C* Reperisco i dati della bolla
015100130521     C                   setoff                                       40
015200130521     C     KEYtas30      chain     titas30c
015300130521     C                   if        %found(titas30c)
015400130521     C                   seton                                        40
015500130521     C                   endif
015600130521     C*
015700130521     C                   ENDSR
015800130520
015900130520
016000130520
016100130520     C     rtvMitt       BEGSR
016200130520     C*
016300130520     C* Inizializzo variabili di work
016400130520     C                   clear                   wRSCMITT
016500130520     C                   clear                   wINDMITT
016600130520     C                   clear                   wCAPMITT
016700130520     C                   clear                   wLOCMITT
016800130520     C                   clear                   wPRVMITT
016900130520     C*
017000130520     C* Reperisco i dati del mittente
017100130520     C                   eval      taaTRC = 'M'
017200130520     C     KEYtaa30      chain     titaa30c
017300130520     C                   if        %found(titaa30c)
017400130520     C                   eval      wRSCMITT = taaRSC
017500130520     C                   eval      wINDMITT = taaIND
017600130520     C                   eval      wCAPMITT = taaCAP
017700130520     C                   eval      wLOCMITT = taaLOC
017800130520     C                   eval      wPRVMITT = taaPRV
017900130521     C*
018000130521     C                   else
018100130521     C*
018200130521     C* Se bolla trovata => reperisco il dato
018300130521     C                   clear                   BS69DS
018400130521     C                   clear                   ACODS
018500130521     C                   clear                   INDDS
018600130521     C                   clear                   CLPDS
018700130521     C                   clear                   CLSDS
018800130521     C                   movel     'GAITRA201'   I69SIF
018900130521     C                   z-add     tasKSC        I69KAC
019000130521     C                   z-add     tasKSC        I69KIN
019100130521     C                   call      'TIBS69R'
019200130521     C                   parm                    BS69DS
019300130521     C                   parm                    ACODS
019400130521     C                   parm                    INDDS
019500130521     C                   parm                    CLPDS
019600130521     C                   parm                    CLSDS
019700130521     C     O69ERR        ifne      '1'
019800130521     C                   eval      wRSCMITT = acoRAG
019900130521     C                   eval      wINDMITT = indVIA
020000130521     C                   eval      wCAPMITT = %char(indCAP)
020100130521     C                   eval      wLOCMITT = indCIT
020200130521     C                   eval      wPRVMITT = indPRV
020300130521     C                   endif
020400130521     C*
020500130520     C                   endif
020600130520     C*
020700130520     C                   ENDSR
020800130518
020900130518
021000130518
021100130520     C     wriRec1       BEGSR
021200130518     C*
021300130518     C                   clear                   titv2p6d1
021400130518     C                   eval      D1TPRCD    = '1'
021500130518     C                   eval      D1DTESTR   = %editc(datcor:'X')
021600130520     C                   eval      D1DTPART   = %editc(vabAAS:'X')+
021700130520     C                                          %editc(vabMGS:'X')
021800130520     C                   eval      D1LDVTNT   = %editc(vabNRS:'X')+
021900130520     C                                          %editc(vabNSP:'X')
022000130518     C                   eval      D1FILDA    = %editc(vabLNP:'X')
022100130518     C                   eval      D1FILA     = %editc(vabLNA:'X')
022200130518     C                   eval      D1RSD      = vabRSD
022300130518     C                   eval      D1IND      = vabIND
022400130518     C                   eval      D1LOD      = vabLOD
022500130518     C                   eval      D1CAD      = vabCAD
022600130518     C                   eval      D1PRD      = vabPRD
022700130520     C                   eval      D1RSM      = wRSCMITT
022800130520     C                   eval      D1INM      = wINDMITT
022900130520     C                   eval      D1LOM      = wLOCMITT
023000130520     C                   eval      D1CAM      = wCAPMITT
023100130520     C                   eval      D1PRM      = wPRVMITT
023200130518     C                   eval      D1NCL      = %editc(vabNCL:'X')
023300130520     C                   eval      D1PKD      = %subst(
023400130607     C                                          %editc(vabPKB:'X')+'00':3:7)
023500130607     C                   eval      D1PKR      = *zeros
023600130607     C                   eval      D1VLM      = '000'+%subst(
023700130607     C                                          %editc(vabVLB:'X'):1:4)
023800130520     C                   eval      D1DTCONS   = *all'0'
023900130518     C                   eval      D1CCMTNT   = %editc(vabCCM:'X')
024000130518     C                   eval      D1TS       = vabTSP
024100130518     C                   eval      D1VCA      = vabVCA
024200130607     C                   eval      D1CAS      = %subst(%editc(vabCAS:'X'):2:11)
024300130705     C                   eval      D1RIFMIT   = %trim(%editc(vabRMN:'4'))
024400130518     C                   eval      D1NOTE     = vabNOT + ' ' + vabNT2
024500130518     C                   eval      D1FERMODEP = vabFFD
024600130708     C                   eval      D1SASEST   = 'PC'
024700130708     C                   evalr     D1LDVAEST  = %subst(%editc(vabAAS:'X'):3:2)+
024800130607     C                                          %editc(vabLNP:'X')+
024900130607     C                                          %editc(vabNRS:'X')+
025000130607     C                                          %editc(vabNSP:'X')
025100130518     C*
025200130520     C                   eval      vawDTA = titv2p6d1
025300130523     C                   exsr      wriBuffer
025400130607     C  N31              add       1             wTotRec
025500130607     C  N31              add       1             wRec1
025600130518     C*
025700130518     C                   ENDSR
025800130518
025900130518
026000130518
026100130520     C     wriRec2       BEGSR
026200130520     C*
026300130520     C* Stabilisco lo status corrente della spedizione
026400130520     C                   exsr      calSTS
026500130518     C*
026600130520     C* Se trattasi di status da ritornare al cliente => procedo
026700130520     C                   if        *in53
026800130518     C                   clear                   titv2p6d2
026900130520     C                   eval      D2TPRCD    = '2'
027000130520     C                   eval      D2DTESTR   = %editc(datcor:'X')
027100130520     C                   eval      D2CCM      = %editc(vacCCM:'X')
027200130520     C                   eval      D2DTMODSAS = %editc(wDataSts:'X')
027300130607     C                   eval      D2TMMODSAS = D2TMCONS
027400130709     C                   eval      D2RIFMIT   = %trim(%editc(vacRMN:'4'))
027500130607     C                   eval      D2DTINC    = *zeros
027600130607     C                   eval      D2PKR      = *zeros
027700130607     C                   eval      D2VLM      = *zeros
027800130607     C                   eval      D2NMRICEV  = *blanks
027900130607     C*
028000130520     C                   if        wConsOkSiNoDes = 'OK'
028100130520     C                   eval      D2DTCONS   = %editc(wDataSts:'X')
028200130520     C                   endif
028300130607     C                   if        wConsOkSiNoDes = 'RI'
028400130607     C                   eval      D2DTCONS   = %editc(wDataSts:'X')
028500130607     C                   endif
028600130520     C                   if        wConsOkSiNoDes = 'OK'
028700130520     C                   eval      D2TMCONS   = %editc(vacHMC:'X')+'00'
028800130520     C                   endif
028900130607     C                   if        wConsOkSiNoDes = 'GI'
029000130607     C                   eval      D2DTGIA    = %editc(wDataSts:'X')
029100130607     C                   endif
029200130607     C*
029300130520     C                   eval      D2FILLER   = 'XX'
029400130705     C                   eval      D2SASEST   = wConsOkSiNoDes
029500130708     C                   evalr     D2LDVEST   = %subst(%editc(vacAAS:'X'):3:2)+
029600130607     C                                          %editc(vacLNP:'X')+
029700130607     C                                          %editc(vacNRS:'X')+
029800130520     C                                          %editc(vacNSP:'X')
029900130520     C*
030000130520     C                   eval      vawDTA = titv2p6d2
030100130523     C                   exsr      wriBuffer
030200130607     C  N31              add       1             wTotRec
030300130607     C  N31              add       1             wRec2
030400130520     C*
030500130520     C                   endif
030600130518     C*
030700130518     C                   ENDSR
030800130520
030900130520
031000130520
031100130520     C     calSTS        BEGSR
031200130520     C*
031300130520     C                   SETOFF                                       53
031400130520     C                   MOVEL     *blanks       wConsOkSiNoDes    2
031500130520     C                   EVAL      wDataSts       = vacDCM
031600130520     C*
031700130520     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
031800130520     C                   IF        vacDCM > *zeros AND
031900130520     C                             (vacCCA = *blanks OR vacCCA = '1')
032000130520     C                   EVAL      wConsOkSiNoDes = 'OK'
032100130520     C                   SETON                                        53
032200130520     C                   ELSE
032300130520     C*
032400130520     C* A questo punto stabilisco la descrizione
032500130520     C                   SELECT
032600130520     C                   WHEN      vacCCA = '1'
032700130520     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
032800130520     C                   SETOFF                                       53
032900130520     C                   WHEN      vacCCA = '2'
033000130520     C                   EVAL      wConsOkSiNoDes = 'KO'
033100130520     C                   SETON                                        53
033200130520     C                   WHEN      vacCCA = '3'
033300130520     C                   EVAL      wConsOkSiNoDes = 'KO'
033400130520     C                   SETON                                        53
033500130520     C                   WHEN      vacCCA = '4'
033600130520     C                   EVAL      wConsOkSiNoDes = 'KO'
033700130520     C                   SETON                                        53
033800130520     C                   WHEN      vacCCA = '5'
033900130520     C                   EVAL      wConsOkSiNoDes = 'KO'
034000130520     C                   SETON                                        53
034100130520     C                   WHEN      vacCCA = '6'
034200130520     C                   EVAL      wConsOkSiNoDes = 'KO'
034300130520     C                   SETON                                        53
034400130520     C                   WHEN      vacCCA = '7'
034500130520     C                   EVAL      wConsOkSiNoDes = 'KO'
034600130520     C                   SETON                                        53
034700130520     C                   WHEN      vacCCA = 'A'
034800130520     C                   EVAL      wConsOkSiNoDes = 'KO'
034900130520     C                   SETON                                        53
035000130520     C                   WHEN      vacCCA = 'C'
035100130607     C                   EVAL      wConsOkSiNoDes = 'PC'
035200130607     C                   SETON                                        53
035300130520     C                   WHEN      vacCCA = 'R'
035400130607     C                   EVAL      wConsOkSiNoDes = 'RI'
035500130520     C                   SETON                                        53
035600130520     C                   WHEN      vacCCA = 'S'
035700130520     C                   EVAL      wConsOkSiNoDes = 'KO'
035800130520     C                   SETON                                        53
035900130520     C                   ENDSL
036000130520     C*
036100130520     C                   IF        (vacDLA > vacDCM AND vacDLA >  vacDAG)
036200130607     C                   EVAL      wDataSts       = vacDLA                      * lasciato avviso
036300130607     C                   EVAL      wConsOkSiNoDes = 'PC'
036400130607     C                   SETON                                        53
036500130520     C                   ENDIF
036600130520     C                   IF        (vacDAG > vacDCM AND vacDAG >= vacDLA)
036700130520     C                   EVAL      wDataSts       = vacDAG
036800130607     C                   EVAL      wConsOkSiNoDes = 'GI'                        * giacenza
036900130520     C                   SETON                                        53
037000130520     C                   ENDIF
037100130520     C                   IF        vacDCM = *zeros AND
037200130520     C                             vacDLA = *zeros AND
037300130520     C                             vacDAG = *zeros AND
037400130520     C                             vacCCA = *blanks
037500130520     C                   EVAL      wDataSts       = datcor
037600130520     C*
037700130520     C* Verifico la data consegna richiesta
037800130520     C                   IF        vacDCR > datcor
037900130520     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna in ' +
038000130520     C                                              'data ' + %editc(vacDCR:'X')
038100130520     C                   SETOFF                                       53
038200130520     C                   ELSE
038300130520     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
038400130520     C                   SETOFF                                       53
038500130520     C                   ENDIF
038600130520     C                   ENDIF
038700130520     C*
038800130520     C                   ENDIF
038900130520     C*
039000130520     C                   ENDSR
039100130520
039200130520
039300130520
039400130520     C     wriRec9       BEGSR
039500130520     C*
039600130520     C                   clear                   titv2p6d9
039700130520     C                   add       1             wTotRec
039800130520     C                   eval      D9TPRCD    = '9'
039900130520     C                   eval      D9FISSO_P  = 'P'
040000130520     C                   eval      D9NRRCDTP1 = wRec1
040100130520     C                   eval      D9FISSO_A  = 'A'
040200130520     C                   eval      D9NRRCDTP2 = wRec2
040300130520     C                   eval      D9NRRCD    = wTotRec
040400130520     C*
040500130520     C                   eval      vawDTA = titv2p6d9
040600130607     C                   write     TIVAW000
040700130520     C*
040800130520     C                   ENDSR
040900130523
041000130523
041100130523
041200130523     C     wriBuffer     BEGSR
041300130523     C*
041400130523     C* Ritorno solamente le bolle originali
041500130523     C*
041600130523     C* Inizializzo i campi d wrk
041700130523     C                   setoff                                       31
041800130523     C                   movel     *blanks       wEsito1           1
041900130523     C*
042000130523     C* Chiamata metodo GetLblTyp
042100130523     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
042200130523     C                                                xAAS
042300130523     C                                               :xLNP
042400130523     C                                               :xNRS
042500130523     C                                               :xNSP
042600130523     C                                               :pOutLblTyp
042700130523     C                                               :pOutAnnoBO
042800130523     C                                               :pOutLineaParBO
042900130523     C                                               :pOutSerieBO
043000130523     C                                               :pOutNumSpedBO
043100130523     C                                               :pOutDcmBO
043200130523     C                                               :pOutCcaBO
043300130523     C                                               :pOutRblBO))
043400130523     C* Se trattasi d bolla originale proseguo con le verifiche
043500130523     C                   if        pOutLblTyp <> 'O'
043600130523     C                   seton                                        31
043700130523     C                   endif
043800130523     C*
043900130523     C* Scarico il buffer di output
044000130523     C  N31              write     TIVAW000
044100130523     C*
044200130523     C                   ENDSR
044300991027
044400130518
044500130518
044600101018
044700991027      /TITLE Operazioni iniziali.
044800991027     C     *inzsr        BEGSR
044900991027     C*
045000991027     C     *ENTRY        PLIST
045100040628     C                   PARM                    prmppt
045200040628     C     wrkesito      PARM      wrkesito      prmesito
045300130518     C*
045400130518     C* Reperimento ora corrente
045500130518     C                   Z-ADD     *zeros        datcor            8 0
045600130518     C                   Z-ADD     *zeros        datYTD            8 0
045700130518     C                   EVAL      datcor = %dec(%date() : *ISO)
045800130518     C                   EVAL      datYTD = datcor
045900130518     C                   MOVE      '0101'        datYTD
046000130518     C                   time                    wn14             14 0
046100130518     C                   movel     wn14          oracor            6 0
046200991027     C*
046300040628     C* DEFINIZIONE CHIAVI
046400130520     C*
046500130520     C* TITAA30C - Completa
046600130520     C     KEYtaa30      KLIST
046700130520     C                   KFLD                    xAAS
046800130520     C                   KFLD                    xLNP
046900130520     C                   KFLD                    xNRS
047000130520     C                   KFLD                    xNSP
047100130520     C                   KFLD                    taaTRC
047200130521     C*
047300130521     C* TITAS30C - Completa
047400130521     C     KEYtas30      KLIST
047500130521     C                   KFLD                    xAAS
047600130521     C                   KFLD                    xLNP
047700130521     C                   KFLD                    xNRS
047800130521     C                   KFLD                    xNSP
047900130521     C*
048000130521     C* CNACO00F - Completa
048100130521     C     KEYaco00      KLIST
048200130521     C                   KFLD                    acoKUT
048300130521     C                   KFLD                    acoKCC
048400130521     C                   KFLD                    acoKSC
048500040628     C*
048600991027     C                   ENDSR
