000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400090325     Ftivgdtmp  uF   E             DISK
000500100209     FTIVAWWWT  UF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000110913     D fnvacds       e ds                  extname(fnvac00t)
001100111220     D fnevbds       e ds                  extname(fnevb00f)
001200111216     D sepFld          s              1    inz(';')
001300111216
001400111216
001500111221     D pInRichiesta    S              3A
001600111220     D pOutTITAS_DS  E DS                  extname(TITAS00F)
001700111216     D pOutFNARB_DS  E DS                  extname(FNARB00F)
001800111216     D pOutFNBLP_DS  E DS                  extname(FNBLP00F)
001900111221     D pOutEsitoSPE    S              3A
002000111222
002100111222     D pInTpChiamDCT   S              2S 0
002200111222     D pOutFNDCT_DS  E DS                  extname(FNDCT02L)
002300111222     D pOutEsitoDCT    S              1A
002400111220
002500111220     D pInNumProg      S              2S 0
002600111220     D pOutTIGCP_DS  E DS                  extname(TIGCP00F)
002700111220     D pOutEsitoGIA    S              1A
002800111220
002900111221     D pInCausale      S              3A
003000111221     D pInDtEvento     S              8S 0
003100111220     D* tipo chiamata:
003200111220     D*  0=prima chiamata
003300111220     D*  1=successive
003400111222     D pInTpChiamEVB   S              1S 0
003500111220     D pOutFNEVB_DS    DS                  occurs(50)
003600111221     D  pOutCausale                   3A
003700111221     D  pOutDtEvento                  8S 0
003800111221     D  pOutOraEvento                 4S 0
003900111221     D  pOutDtImmEv                   8S 0
004000111221     D  pOutOraImmEv                  6S 0
004100111221     D  pOutFilImmEv                  3S 0
004200111220     D* esito reperimento:
004300111220     D*  0=non reperito
004400111220     D*  1=reperito
004500111220     D pOutEsitoEVB    S              1A
004600111220     D pOutNrOccurs    S              3S 0
004700111220     D pOutEnd         S              1A
004800111220
004900110810
005000111216
005100111216     D DS_Cliente      DS
005200111216     D   c_NumBolla                   7
005300111216     D   c_DataBolla                 10
005400111216     D   c_RagSoc                    35
005500111216     D   c_Via                       35
005600111227     D   c_Localita                  20
005700111216     D   c_Prov                       5
005800111216     D   c_Colli                      6
005900111216     D   c_NoteCons                  39
006000111216     D   c_DataUscita                10
006100111216     D   c_DataArrCen                10
006200111216     D   c_DataArrLoc                10
006300111216     D   c_DataCarico                10
006400111216     D   c_DataCons                  10
006500111216     D   c_DataPrenot                10
006600111216     D   c_DataGiac                  10
006700111216     D   c_CodGiac                   13
006800111222     D   c_PraticaDan                10
006900111216     D   c_Riprenotaz                10
007000111221     D   c_CCA                        1
007100111221     D   c_DataCCA                   10
007200110913
007300110913
007400110913     D*------------------
007500110913     D* LINKING A DEFINIZIONI ESTERNE
007600110913     D*------------------
007700110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007800110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007900111222     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008000111222     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008100110913
008200110810
008300991027
008400030908     C                   EXSR      chkRECFILMBR
008500991027     C                   EXSR      traduci
008600921023     C                   SETON                                        LR
008700030908
008800110810
008900110810
009000030908     C     chkRECFILMBR  BEGSR
009100030908     C*
009200100209     C                   READ      TIVAWWWT                               55
009300030908     C*
009400030908     C                   ENDSR
009500110810
009600110810
009700991027
009800991027     C     traduci       BEGSR
009900991027     C*
010000090325     C                   READ      tivgdtmp
010100090325     C                   DOW       not %eof
010200110914     c                   movel     vgddta        fnvacds
010300041220     C*
010400111216     C* Inizializzioni
010500111216     C                   CLEAR                   DS_Cliente
010600110913     C*
010700110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
010800110913     C                   MOVEL     *blanks       wEsito1           1
010900110913     C                   MOVEL     *blanks       wEsito2           1
011000111220     C                   Z-ADD     *zeros        wDataCons         8 0
011100111221     C                   Z-ADD     *zeros        wDataCCA          8 0
011200111220     C                   MOVEL     *blanks       wConsAnom         1
011300110913     C*
011400110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
011500110913     C                                                vacAAS
011600110913     C                                               :vacLNP
011700110913     C                                               :vacNRS
011800110913     C                                               :vacNSP
011900110913     C                                               :pOutLblTyp
012000110913     C                                               :pOutAnnoBO
012100110913     C                                               :pOutLineaParBO
012200110913     C                                               :pOutSerieBO
012300110913     C                                               :pOutNumSpedBO
012400110913     C                                               :pOutDcmBO
012500110913     C                                               :pOutCcaBO
012600110913     C                                               :pOutRblBO))
012700111109     C*
012800111109     C* Ritorno solamente le bolle valide come spedizione
012900111223     C                   IF        pOutLblTyp = 'O' AND pOutRblBO = 'N'
013000110913     C*
013100110913     C* Chiamata metodo GetLastChild
013200110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
013300110913     C                                                pOutAnnoBO
013400110913     C                                               :pOutLineaParBO
013500110913     C                                               :pOutSerieBO
013600110913     C                                               :pOutNumSpedBO
013700110913     C                                               :pOutAnnoFI
013800110913     C                                               :pOutLineaParFI
013900110913     C                                               :pOutSerieFI
014000110913     C                                               :pOutNumSpedFI
014100110913     C                                               :pOutDcmFI
014200110913     C                                               :pOutCcaFI))
014300110914     C*
014400110914     C                   IF        wEsito2 = '0'
014500110914     C                   EVAL      wDataCons = pOutDcmFI
014600110914     C                   EVAL      wConsAnom = pOutCcaFI
014700110914     C                   ELSE
014800110914     C                   EVAL      wDataCons = pOutDcmBO
014900110914     C                   EVAL      wConsAnom = pOutCcaBO
015000110914     C                   ENDIF
015100111221     C*
015200111221     C                   IF        wConsAnom <> *blanks
015300111221     C                   EVAL      wDataCCA  = wDataCons
015400111221     C                   EVAL      wDataCons = *zeros
015500111221     C                   ENDIF
015600111221     C*
015700111221     C* Ritorno solamente le bolle originali
015800111221     C                   IF        pOutLblTyp = 'O'
015900111220     C*
016000111220     C* Reperisco i dati della spedizione
016100111220     C                   EXSR      rtvDatiBolla
016200110913     C*
016300111216     C* Compongo il buffer d out
016400110810     C                   EVAL      VAWDTA  =
016500111216     C                             c_NumBolla                           +sepFld+
016600111216     C                             c_DataBolla                          +sepFld+
016700111216     C                             c_RagSoc                             +sepFld+
016800111216     C                             c_Via                                +sepFld+
016900111216     C                             c_Localita                           +sepFld+
017000111216     C                             c_Prov                               +sepFld+
017100111216     C                             c_Colli                              +sepFld+
017200111216     C                             c_NoteCons                           +sepFld+
017300111216     C                             c_DataUscita                         +sepFld+
017400111216     C                             c_DataArrCen                         +sepFld+
017500111216     C                             c_DataArrLoc                         +sepFld+
017600111216     C                             c_DataCarico                         +sepFld+
017700111216     C                             c_DataCons                           +sepFld+
017800111216     C                             c_DataPrenot                         +sepFld+
017900111216     C                             c_DataGiac                           +sepFld+
018000111216     C                             c_CodGiac                            +sepFld+
018100111216     C                             c_PraticaDan                         +sepFld+
018200111221     C                             c_Riprenotaz                         +sepFld+
018300111221     C                             c_CCA                                +sepFld+
018400111227     C                             c_DataCCA                            +X'00'
018500930409     C*
018600100209     C                   WRITE     TIVAW000
018700111109     C*
018800111221     C                   ENDIF
018900111109     C                   ENDIF
019000110810     C*
019100090325     C                   delete    tivgd000
019200991027     C*
019300090325     C                   READ      tivgdtmp
019400030325     C                   ENDDO
019500991027     C*
019600030325     C                   EVAL      wrkesito = '0'
019700991027     C*
019800910830     C                   ENDSR
019900111216
020000111216
020100111216
020200111216     C     rtvDatiBolla  BEGSR
020300111220     C*
020400111220     C* Reperisco gli eventi 'MIC'
020500111228     C                   z-add     *zeros        pInTpChiamEVB
020600111228     C                   dow       pOutEnd <> '1' AND pInTpChiamEVB < 50
020700111228     C*
020800111220     C                   call      'UBRTVEVB'
020900111220     C                   parm      vacAAS        pInAnno
021000111220     C                   parm      vacLNP        pInLineaPar
021100111220     C                   parm      vacNRS        pInSerie
021200111220     C                   parm      vacNSP        pInNumSped
021300111220     C                   parm      'MIC'         pInCausale
021400111220     C                   parm      *zeros        pInDtEvento
021500111228     C                   parm                    pInTpChiamEVB
021600111220     C                   parm                    pOutFNEVB_DS
021700111220     C                   parm                    pOutEsitoEVB
021800111220     C                   parm                    pOutNrOccurs
021900111220     C                   parm                    pOutEnd
022000111228     C*
022100111228     C* Reperisco i dati del buffer FNDCT
022200111228     C                   if        pOutNrOccurs > *zeros
022300111228     C     pOutNrOccurs  occur     pOutFNEVB_DS
022400111228     C                   endif
022500111220     C*
022600111228     C* Proseguo con le successive chiamate ad driver
022700111228     C                   add       1             pInTpChiamEVB
022800111228     C                   enddo
022900111222     C*
023000111222     C* Reperisco l'ultima giacenza
023100111222     C                   call      'UBRTVGIAC'
023200111222     C                   parm      vacAAS        pInAnno
023300111222     C                   parm      vacLNP        pInLineaPar
023400111222     C                   parm      vacNRS        pInSerie
023500111222     C                   parm      vacNSP        pInNumSped
023600111222     C                   parm      *zeros        pInNumProg
023700111222     C                   parm                    pOutTIGCP_DS
023800111222     C                   parm                    pOutEsitoGIA
023900111222     C*
024000111222     C* Reperisco l'ultimo danno ("valido ai fini internet")
024100111222     C                   call      'UBRTVDCT'
024200111222     C                   parm      vacAAS        pInAnno
024300111222     C                   parm      vacLNP        pInLineaPar
024400111222     C                   parm      vacNRS        pInSerie
024500111222     C                   parm      vacNSP        pInNumSped
024600111222     C                   parm      01            pInTpChiamDCT
024700111222     C                   parm                    pOutFNDCT_DS
024800111222     C                   parm                    pOutEsitoDCT
024900111220     C*
025000111220     C* Reperisco i dati di testata
025100111220     C                   call      'UBRTVSPE'
025200111221     C                   parm      'APS'         pInRichiesta
025300111220     C                   parm      vacAAS        pInAnno
025400111220     C                   parm      vacLNP        pInLineaPar
025500111220     C                   parm      vacNRS        pInSerie
025600111220     C                   parm      vacNSP        pInNumSped
025700111220     C                   parm                    pOutFNARB_DS
025800111220     C                   parm                    pOutFNBLP_DS
025900111221     C                   parm                    pOutTITAS_DS
026000111221     C                   parm                    pOutEsitoSPE
026100111220     C*
026200111221     C* Valorizzo il buffer d outout
026300111221     C                   select
026400111221     C*
026500111221     C                   when      %subst(pOutEsitoSPE:1:2) = 'AP'
026600111221     C                   eval      c_Via        = %trim(arbIND)
026700111221     C                   eval      c_Localita   = %trim(arbLOD)
026800111222     C*
026900111222     C                   eval      c_DataUscita =
027000111222     C                              UBFMTDATE_Convert(
027100111227     C                                  %editc(blpDRT:'4') :
027200111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
027300111222     C*
027400111222     C                   eval      c_DataArrCen =
027500111222     C                              UBFMTDATE_Convert(
027600111227     C                                  %editc(arbAAS*10000+arbMGS:'4') :
027700111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
027800111222     C*
027900111222     C                   eval      c_DataArrLoc =
028000111222     C                              UBFMTDATE_Convert(
028100111227     C                                  %editc(arbDTI:'4') :
028200111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
028300111222     C*
028400111222     C                   eval      c_DataPrenot =
028500111222     C                              UBFMTDATE_Convert(
028600111227     C                                  %editc(arbDTI:'4') :
028700111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
028800111222     C*
028900111222     C                   eval      c_Riprenotaz =
029000111222     C                              UBFMTDATE_Convert(
029100111227     C                                  %editc(arbDCR:'4') :
029200111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
029300111221     C*
029400111221     C                   when      %subst(pOutEsitoSPE:1:2) = 'A '
029500111221     C                   eval      c_Via        = %trim(arbIND)
029600111221     C                   eval      c_Localita   = %trim(arbLOD)
029700111222     C*
029800111222     C                   eval      c_DataUscita =
029900111222     C                              UBFMTDATE_Convert(
030000111227     C                                  %editc(arbDTI:'4') :
030100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
030200111222     C*
030300111222     C                   eval      c_DataArrCen =
030400111222     C                              UBFMTDATE_Convert(
030500111227     C                                  %editc(arbAAS*10000+arbMGS:'4') :
030600111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
030700111222     C*
030800111222     C                   eval      c_DataArrLoc =
030900111222     C                              UBFMTDATE_Convert(
031000111227     C                                  %editc(arbDTI:'4') :
031100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
031200111222     C*
031300111222     C                   eval      c_DataPrenot =
031400111222     C                              UBFMTDATE_Convert(
031500111227     C                                  %editc(arbDTI:'4') :
031600111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
031700111222     C*
031800111222     C                   eval      c_Riprenotaz =
031900111222     C                              UBFMTDATE_Convert(
032000111227     C                                  %editc(arbDCR:'4') :
032100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
032200111221     C*
032300111221     C                   when      %subst(pOutEsitoSPE:1:2) = ' P'
032400111221     C                   eval      c_Via        = %trim(blpIND)
032500111221     C                   eval      c_Localita   = %trim(blpLOD)
032600111222     C*
032700111222     C                   eval      c_DataUscita =
032800111222     C                              UBFMTDATE_Convert(
032900111227     C                                  %editc(blpDRT:'4') :
033000111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
033100111222     C*
033200111222     C                   eval      c_DataArrCen =
033300111222     C                              UBFMTDATE_Convert(
033400111227     C                                  %editc(blpAAS*10000+blpMGS:'4') :
033500111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
033600111222     C*
033700111222     C                   eval      c_DataArrLoc =
033800111222     C                              UBFMTDATE_Convert(
033900111227     C                                  %editc(blpDTI:'4') :
034000111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
034100111222     C*
034200111222     C                   eval      c_DataPrenot =
034300111222     C                              UBFMTDATE_Convert(
034400111227     C                                  %editc(blpDTI:'4') :
034500111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
034600111222     C*
034700111222     C                   eval      c_Riprenotaz =
034800111222     C                              UBFMTDATE_Convert(
034900111227     C                                  %editc(blpDCR:'4') :
035000111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
035100111221     C*
035200111221     C                   when      %subst(pOutEsitoSPE:3:1) = 'S'
035300111221     C                   eval      c_Via        = %trim(tasIND)
035400111221     C                   eval      c_Localita   = %trim(tasLOD)
035500111222     C*
035600111222     C                   eval      c_DataUscita =
035700111222     C                              UBFMTDATE_Convert(
035800111227     C                                  %editc(tasDRT:'4') :
035900111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
036000111222     C*
036100111222     C                   eval      c_DataArrCen =
036200111222     C                              UBFMTDATE_Convert(
036300111227     C                                  %editc(tasAAS*10000+tasMGS:'4') :
036400111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
036500111222     C*
036600111222     C                   eval      c_DataArrLoc =
036700111222     C                              UBFMTDATE_Convert(
036800111227     C                                  %editc(tasDTI:'4') :
036900111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
037000111222     C*
037100111222     C                   eval      c_DataPrenot =
037200111222     C                              UBFMTDATE_Convert(
037300111227     C                                  %editc(tasDTI:'4') :
037400111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
037500111222     C*
037600111222     C                   eval      c_Riprenotaz =
037700111222     C                              UBFMTDATE_Convert(
037800111227     C                                  %editc(tasDCR:'4') :
037900111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
038000111221     C                   endsl
038100111216     C*
038200111221     C                   exsr      valBuffer
038300111221     C*
038400111216     C                   ENDSR
038500111216
038600111216
038700111216
038800111221     C     valBuffer     BEGSR
038900111216     C*
039000111221     C                   eval      c_NumBolla   = %trim(%editc(vacRMN:'Q'))
039100111221     C                   eval      c_DataBolla  = %trim(vacRMA)
039200111221     C                   eval      c_RagSoc     = %trim(vacRSD)
039300111221     C                   eval      c_Prov       = vacPRD
039400111221     C                   eval      c_Colli      = %char(vacNCL)
039500111216     C                   eval      c_NoteCons   = *blanks
039600111222     C*
039700111222     C                   eval      c_DataCarico =
039800111222     C                              UBFMTDATE_Convert(
039900111227     C                                  %editc(pOutDtEvento:'4') :
040000111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
040100111222     C*
040200111222     C                   eval      c_DataCons   =
040300111222     C                              UBFMTDATE_Convert(
040400111227     C                                  %editc(wDataCons:'4') :
040500111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
040600111222     C*
040700111222     C                   eval      c_DataGiac   =
040800111222     C                              UBFMTDATE_Convert(
040900111227     C                                  %editc(gcpAGC*10000+gcpMGC:'4') :
041000111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
041100111222     C*
041200111220     C                   eval      c_CodGiac    = gcpCMC
041300111222     C                   eval      c_PraticaDan = dctTAD
041400111221     C                   eval      c_CCA        = wConsAnom
041500111222     C*
041600111222     C                   eval      c_DataCCA    =
041700111222     C                              UBFMTDATE_Convert(
041800111227     C                                  %editc(wDataCCA:'4') :
041900111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
042000111216     C*
042100111216     C                   ENDSR
042200991027
042300110810
042400110810
042500991027      /TITLE Operazioni iniziali.
042600991027     C     *inzsr        BEGSR
042700991027     C*
042800991027     C     *ENTRY        PLIST
042900991027     C                   parm                    prmppt
043000991027     C     wrkesito      parm      wrkesito      prmesito
043100030709     C*
043200991027     C                   ENDSR
