000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400090325     Ftivgdtmp  uF   E             DISK
000500100209     FTIVAWWWT  UF A E             DISK    commit
000600120112     Fwfwdec01i if   e           k disk
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100110913     D fnvacds       e ds                  extname(fnvac00t)
001200111220     D fnevbds       e ds                  extname(fnevb00f)
001300111216     D sepFld          s              1    inz(';')
001400111216
001500111216
001600111221     D pInRichiesta    S              3A
001700111220     D pOutTITAS_DS  E DS                  extname(TITAS00F)
001800111216     D pOutFNARB_DS  E DS                  extname(FNARB00F)
001900111216     D pOutFNBLP_DS  E DS                  extname(FNBLP00F)
002000111221     D pOutEsitoSPE    S              3A
002100111222
002200111222     D pInTpChiamDCT   S              2S 0
002300111222     D pOutFNDCT_DS  E DS                  extname(FNDCT02L)
002400111222     D pOutEsitoDCT    S              1A
002500111220
002600111220     D pInNumProg      S              2S 0
002700111220     D pOutTIGCP_DS  E DS                  extname(TIGCP00F)
002800111220     D pOutEsitoGIA    S              1A
002900111220
003000111221     D pInCausale      S              3A
003100111221     D pInDtEvento     S              8S 0
003200111220     D* tipo chiamata:
003300111220     D*  0=prima chiamata
003400111220     D*  1=successive
003500111222     D pInTpChiamEVB   S              1S 0
003600111220     D pOutFNEVB_DS    DS                  occurs(50)
003700111221     D  pOutCausale                   3A
003800111221     D  pOutDtEvento                  8S 0
003900111221     D  pOutOraEvento                 4S 0
004000111221     D  pOutDtImmEv                   8S 0
004100111221     D  pOutOraImmEv                  6S 0
004200111221     D  pOutFilImmEv                  3S 0
004300111220     D* esito reperimento:
004400111220     D*  0=non reperito
004500111220     D*  1=reperito
004600111220     D pOutEsitoEVB    S              1A
004700111220     D pOutNrOccurs    S              3S 0
004800111220     D pOutEnd         S              1A
004900111220
005000110810
005100111216
005200111216     D DS_Cliente      DS
005300111216     D   c_NumBolla                   7
005400111216     D   c_DataBolla                 10
005500111216     D   c_RagSoc                    35
005600111216     D   c_Via                       35
005700111227     D   c_Localita                  20
005800111216     D   c_Prov                       5
005900111216     D   c_Colli                      6
006000111216     D   c_NoteCons                  39
006100111216     D   c_DataUscita                10
006200111216     D   c_DataArrCen                10
006300111216     D   c_DataArrLoc                10
006400111216     D   c_DataCarico                10
006500111216     D   c_DataCons                  10
006600111216     D   c_DataPrenot                10
006700111216     D   c_DataGiac                  10
006800111216     D   c_CodGiac                   13
006900111222     D   c_PraticaDan                10
007000111216     D   c_Riprenotaz                10
007100111221     D   c_CCA                        1
007200111221     D   c_DataCCA                   10
007300110913
007400110913
007500110913     D*------------------
007600110913     D* LINKING A DEFINIZIONI ESTERNE
007700110913     D*------------------
007800110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
007900110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008000111222     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008100111222     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008200110913
008300110810
008400991027
008500030908     C                   EXSR      chkRECFILMBR
008600991027     C                   EXSR      traduci
008700921023     C                   SETON                                        LR
008800030908
008900110810
009000110810
009100030908     C     chkRECFILMBR  BEGSR
009200030908     C*
009300100209     C                   READ      TIVAWWWT                               55
009400030908     C*
009500030908     C                   ENDSR
009600110810
009700110810
009800991027
009900991027     C     traduci       BEGSR
010000991027     C*
010100090325     C                   READ      tivgdtmp
010200090325     C                   DOW       not %eof
010300110914     c                   movel     vgddta        fnvacds
010400041220     C*
010500111216     C* Inizializzioni
010600111216     C                   CLEAR                   DS_Cliente
010700110913     C*
010800110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
010900110913     C                   MOVEL     *blanks       wEsito1           1
011000110913     C                   MOVEL     *blanks       wEsito2           1
011100111220     C                   Z-ADD     *zeros        wDataCons         8 0
011200111221     C                   Z-ADD     *zeros        wDataCCA          8 0
011300111220     C                   MOVEL     *blanks       wConsAnom         1
011400110913     C*
011500110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
011600110913     C                                                vacAAS
011700110913     C                                               :vacLNP
011800110913     C                                               :vacNRS
011900110913     C                                               :vacNSP
012000110913     C                                               :pOutLblTyp
012100110913     C                                               :pOutAnnoBO
012200110913     C                                               :pOutLineaParBO
012300110913     C                                               :pOutSerieBO
012400110913     C                                               :pOutNumSpedBO
012500110913     C                                               :pOutDcmBO
012600110913     C                                               :pOutCcaBO
012700110913     C                                               :pOutRblBO))
012800111109     C*
012900111109     C* Ritorno solamente le bolle valide come spedizione
013000111223     C                   IF        pOutLblTyp = 'O' AND pOutRblBO = 'N'
013100110913     C*
013200110913     C* Chiamata metodo GetLastChild
013300110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
013400110913     C                                                pOutAnnoBO
013500110913     C                                               :pOutLineaParBO
013600110913     C                                               :pOutSerieBO
013700110913     C                                               :pOutNumSpedBO
013800110913     C                                               :pOutAnnoFI
013900110913     C                                               :pOutLineaParFI
014000110913     C                                               :pOutSerieFI
014100110913     C                                               :pOutNumSpedFI
014200110913     C                                               :pOutDcmFI
014300110913     C                                               :pOutCcaFI))
014400110914     C*
014500110914     C                   IF        wEsito2 = '0'
014600110914     C                   EVAL      wDataCons = pOutDcmFI
014700110914     C                   EVAL      wConsAnom = pOutCcaFI
014800110914     C                   ELSE
014900110914     C                   EVAL      wDataCons = pOutDcmBO
015000110914     C                   EVAL      wConsAnom = pOutCcaBO
015100110914     C                   ENDIF
015200111221     C*
015300111221     C                   IF        wConsAnom <> *blanks
015400111221     C                   EVAL      wDataCCA  = wDataCons
015500111221     C                   EVAL      wDataCons = *zeros
015600111221     C                   ENDIF
015700111221     C*
015800111221     C* Ritorno solamente le bolle originali
015900111221     C                   IF        pOutLblTyp = 'O'
016000111220     C*
016100111220     C* Reperisco i dati della spedizione
016200111220     C                   EXSR      rtvDatiBolla
016300110913     C*
016400111216     C* Compongo il buffer d out
016500110810     C                   EVAL      VAWDTA  =
016600111216     C                             c_NumBolla                           +sepFld+
016700111216     C                             c_DataBolla                          +sepFld+
016800111216     C                             c_RagSoc                             +sepFld+
016900111216     C                             c_Via                                +sepFld+
017000111216     C                             c_Localita                           +sepFld+
017100111216     C                             c_Prov                               +sepFld+
017200111216     C                             c_Colli                              +sepFld+
017300111216     C                             c_NoteCons                           +sepFld+
017400111216     C                             c_DataUscita                         +sepFld+
017500111216     C                             c_DataArrCen                         +sepFld+
017600111216     C                             c_DataArrLoc                         +sepFld+
017700111216     C                             c_DataCarico                         +sepFld+
017800111216     C                             c_DataCons                           +sepFld+
017900111216     C                             c_DataPrenot                         +sepFld+
018000111216     C                             c_DataGiac                           +sepFld+
018100111216     C                             c_CodGiac                            +sepFld+
018200111216     C                             c_PraticaDan                         +sepFld+
018300111221     C                             c_Riprenotaz                         +sepFld+
018400111221     C                             c_CCA                                +sepFld+
018500111227     C                             c_DataCCA                            +X'00'
018600930409     C*
018700120221     C* Scivo solo a "determinate" condizioni
018800120221     C                   IF        %len(%trim(c_NumBolla))  <  7 OR
018900120221     C                             %len(%trim(c_DataBolla)) < 10
019000120221     C                   ELSE
019100100209     C                   WRITE     TIVAW000
019200120221     C                   ENDIF
019300111109     C*
019400111221     C                   ENDIF
019500111109     C                   ENDIF
019600110810     C*
019700090325     C                   delete    tivgd000
019800991027     C*
019900090325     C                   READ      tivgdtmp
020000030325     C                   ENDDO
020100991027     C*
020200030325     C                   EVAL      wrkesito = '0'
020300991027     C*
020400910830     C                   ENDSR
020500111216
020600111216
020700111216
020800111216     C     rtvDatiBolla  BEGSR
020900111220     C*
021000111220     C* Reperisco gli eventi 'MIC'
021100111228     C                   z-add     *zeros        pInTpChiamEVB
021200111228     C                   dow       pOutEnd <> '1' AND pInTpChiamEVB < 50
021300111228     C*
021400111220     C                   call      'UBRTVEVB'
021500111220     C                   parm      vacAAS        pInAnno
021600111220     C                   parm      vacLNP        pInLineaPar
021700111220     C                   parm      vacNRS        pInSerie
021800111220     C                   parm      vacNSP        pInNumSped
021900111220     C                   parm      'MIC'         pInCausale
022000111220     C                   parm      *zeros        pInDtEvento
022100111228     C                   parm                    pInTpChiamEVB
022200111220     C                   parm                    pOutFNEVB_DS
022300111220     C                   parm                    pOutEsitoEVB
022400111220     C                   parm                    pOutNrOccurs
022500111220     C                   parm                    pOutEnd
022600111228     C*
022700111228     C* Reperisco i dati del buffer FNDCT
022800111228     C                   if        pOutNrOccurs > *zeros
022900111228     C     pOutNrOccurs  occur     pOutFNEVB_DS
023000111228     C                   endif
023100111220     C*
023200111228     C* Proseguo con le successive chiamate ad driver
023300111228     C                   add       1             pInTpChiamEVB
023400111228     C                   enddo
023500111222     C*
023600111222     C* Reperisco l'ultima giacenza
023700111222     C                   call      'UBRTVGIAC'
023800111222     C                   parm      vacAAS        pInAnno
023900111222     C                   parm      vacLNP        pInLineaPar
024000111222     C                   parm      vacNRS        pInSerie
024100111222     C                   parm      vacNSP        pInNumSped
024200111222     C                   parm      *zeros        pInNumProg
024300111222     C                   parm                    pOutTIGCP_DS
024400111222     C                   parm                    pOutEsitoGIA
024500111222     C*
024600111222     C* Reperisco l'ultimo danno ("valido ai fini internet")
024700111222     C                   call      'UBRTVDCT'
024800111222     C                   parm      vacAAS        pInAnno
024900111222     C                   parm      vacLNP        pInLineaPar
025000111222     C                   parm      vacNRS        pInSerie
025100111222     C                   parm      vacNSP        pInNumSped
025200111222     C                   parm      01            pInTpChiamDCT
025300111222     C                   parm                    pOutFNDCT_DS
025400111222     C                   parm                    pOutEsitoDCT
025500111220     C*
025600111220     C* Reperisco i dati di testata
025700111220     C                   call      'UBRTVSPE'
025800111221     C                   parm      'APS'         pInRichiesta
025900111220     C                   parm      vacAAS        pInAnno
026000111220     C                   parm      vacLNP        pInLineaPar
026100111220     C                   parm      vacNRS        pInSerie
026200111220     C                   parm      vacNSP        pInNumSped
026300111220     C                   parm                    pOutFNARB_DS
026400111220     C                   parm                    pOutFNBLP_DS
026500111221     C                   parm                    pOutTITAS_DS
026600111221     C                   parm                    pOutEsitoSPE
026700111220     C*
026800111221     C* Valorizzo il buffer d outout
026900111221     C                   select
027000111221     C*
027100111221     C                   when      %subst(pOutEsitoSPE:1:2) = 'AP'
027200111221     C                   eval      c_Via        = %trim(arbIND)
027300111221     C                   eval      c_Localita   = %trim(arbLOD)
027400111222     C*
027500111222     C                   eval      c_DataUscita =
027600111222     C                              UBFMTDATE_Convert(
027700111227     C                                  %editc(blpDRT:'4') :
027800111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
027900111222     C*
028000111222     C                   eval      c_DataArrCen =
028100111222     C                              UBFMTDATE_Convert(
028200111227     C                                  %editc(arbAAS*10000+arbMGS:'4') :
028300111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
028400111222     C*
028500111222     C                   eval      c_DataArrLoc =
028600111222     C                              UBFMTDATE_Convert(
028700111227     C                                  %editc(arbDTI:'4') :
028800111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
028900111222     C*
029000120214     C                   eval      c_DataPrenot = *blanks
029100120214     C***                           UBFMTDATE_Convert(
029200120214     C***                               %editc(arbDTI:'4') :
029300120214     C***                               'YYYYMMDD' : 'DD/MM/YYYY')
029400111222     C*
029500111222     C                   eval      c_Riprenotaz =
029600111222     C                              UBFMTDATE_Convert(
029700111227     C                                  %editc(arbDCR:'4') :
029800111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
029900111221     C*
030000111221     C                   when      %subst(pOutEsitoSPE:1:2) = 'A '
030100111221     C                   eval      c_Via        = %trim(arbIND)
030200111221     C                   eval      c_Localita   = %trim(arbLOD)
030300111222     C*
030400111222     C                   eval      c_DataUscita =
030500111222     C                              UBFMTDATE_Convert(
030600111227     C                                  %editc(arbDTI:'4') :
030700111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
030800111222     C*
030900111222     C                   eval      c_DataArrCen =
031000111222     C                              UBFMTDATE_Convert(
031100111227     C                                  %editc(arbAAS*10000+arbMGS:'4') :
031200111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
031300111222     C*
031400111222     C                   eval      c_DataArrLoc =
031500111222     C                              UBFMTDATE_Convert(
031600111227     C                                  %editc(arbDTI:'4') :
031700111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
031800111222     C*
031900120214     C                   eval      c_DataPrenot = *blanks
032000120214     C***                           UBFMTDATE_Convert(
032100120214     C***                               %editc(arbDTI:'4') :
032200120214     C***                               'YYYYMMDD' : 'DD/MM/YYYY')
032300111222     C*
032400111222     C                   eval      c_Riprenotaz =
032500111222     C                              UBFMTDATE_Convert(
032600111227     C                                  %editc(arbDCR:'4') :
032700111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
032800111221     C*
032900111221     C                   when      %subst(pOutEsitoSPE:1:2) = ' P'
033000111221     C                   eval      c_Via        = %trim(blpIND)
033100111221     C                   eval      c_Localita   = %trim(blpLOD)
033200111222     C*
033300111222     C                   eval      c_DataUscita =
033400111222     C                              UBFMTDATE_Convert(
033500111227     C                                  %editc(blpDRT:'4') :
033600111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
033700111222     C*
033800111222     C                   eval      c_DataArrCen =
033900111222     C                              UBFMTDATE_Convert(
034000111227     C                                  %editc(blpAAS*10000+blpMGS:'4') :
034100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
034200111222     C*
034300111222     C                   eval      c_DataArrLoc =
034400111222     C                              UBFMTDATE_Convert(
034500111227     C                                  %editc(blpDTI:'4') :
034600111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
034700111222     C*
034800120214     C                   eval      c_DataPrenot = *blanks
034900120214     C***                           UBFMTDATE_Convert(
035000120214     C***                               %editc(blpDTI:'4') :
035100120214     C***                               'YYYYMMDD' : 'DD/MM/YYYY')
035200111222     C*
035300111222     C                   eval      c_Riprenotaz =
035400111222     C                              UBFMTDATE_Convert(
035500111227     C                                  %editc(blpDCR:'4') :
035600111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
035700111221     C*
035800111221     C                   when      %subst(pOutEsitoSPE:3:1) = 'S'
035900111221     C                   eval      c_Via        = %trim(tasIND)
036000111221     C                   eval      c_Localita   = %trim(tasLOD)
036100111222     C*
036200111222     C                   eval      c_DataUscita =
036300111222     C                              UBFMTDATE_Convert(
036400111227     C                                  %editc(tasDRT:'4') :
036500111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
036600111222     C*
036700111222     C                   eval      c_DataArrCen =
036800111222     C                              UBFMTDATE_Convert(
036900111227     C                                  %editc(tasAAS*10000+tasMGS:'4') :
037000111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
037100111222     C*
037200111222     C                   eval      c_DataArrLoc =
037300111222     C                              UBFMTDATE_Convert(
037400111227     C                                  %editc(tasDTI:'4') :
037500111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
037600111222     C*
037700120214     C                   eval      c_DataPrenot = *blanks
037800120214     C***                           UBFMTDATE_Convert(
037900120214     C***                               %editc(tasDTI:'4') :
038000120214     C***                               'YYYYMMDD' : 'DD/MM/YYYY')
038100111222     C*
038200111222     C                   eval      c_Riprenotaz =
038300111222     C                              UBFMTDATE_Convert(
038400111227     C                                  %editc(tasDCR:'4') :
038500111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
038600111221     C                   endsl
038700111216     C*
038800111221     C                   exsr      valBuffer
038900111221     C*
039000111216     C                   ENDSR
039100111216
039200111216
039300111216
039400111221     C     valBuffer     BEGSR
039500120112     C*
039600120112     C* Forzatura dati x sistemazione casino del cliente con logistica
039700120112     C                   z-add     vacRMN        wRMN              9 0
039800120112     C                   if        vacRMN < 1000000
039900120112     C* 1° tentativo x 2011
040000120112     C                   add       11000000      wRMN
040100120112     C     wRMN          chain     wfwdec01i
040200120112     C                   if        %found(wfwdec01i)
040300120112     C                   eval      vacRMN = SEL0001 - 10000000
040400120221     C                   if        %trim(vacRMA) = *all'*'
040500120217     C                   eval      vacRMA = *blanks
040600120217     C                   else
040700120112     C                   eval      vacRMA =
040800120112     C                              UBFMTDATE_Convert(
040900120112     C                                  %editc(%dec(C1XOL:*ISO):'4'):
041000120112     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
041100120217     C                   endif
041200120217     C*
041300120112     C                   else
041400120112     C* 2° tentativo x 2012
041500120112     C                   add       12000000      wRMN
041600120112     C     wRMN          chain     wfwdec01i
041700120112     C                   if        %found(wfwdec01i)
041800120112     C                   eval      vacRMN = SEL0001 - 10000000
041900120221     C                   if        %trim(vacRMA) = *all'*'
042000120217     C                   eval      vacRMA = *blanks
042100120217     C                   else
042200120112     C                   eval      vacRMA =
042300120112     C                              UBFMTDATE_Convert(
042400120112     C                                  %editc(%dec(C1XOL:*ISO):'4'):
042500120112     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
042600120217     C                   endif
042700120112     C                   endif
042800120112     C                   endif
042900120112     C                   endif
043000111216     C*
043100120112     C                   eval      c_NumBolla   = %trim(%editc(vacRMN:'Q'))
043200120112     C                   eval      c_DataBolla  = %trim(vacRMA)
043300111221     C                   eval      c_RagSoc     = %trim(vacRSD)
043400111221     C                   eval      c_Prov       = vacPRD
043500111221     C                   eval      c_Colli      = %char(vacNCL)
043600111216     C                   eval      c_NoteCons   = *blanks
043700111222     C*
043800111222     C                   eval      c_DataCarico =
043900111222     C                              UBFMTDATE_Convert(
044000111227     C                                  %editc(pOutDtEvento:'4') :
044100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
044200111222     C*
044300111222     C                   eval      c_DataCons   =
044400111222     C                              UBFMTDATE_Convert(
044500111227     C                                  %editc(wDataCons:'4') :
044600111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
044700111222     C*
044800111222     C                   eval      c_DataGiac   =
044900111222     C                              UBFMTDATE_Convert(
045000111227     C                                  %editc(gcpAGC*10000+gcpMGC:'4') :
045100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
045200111222     C*
045300111220     C                   eval      c_CodGiac    = gcpCMC
045400111222     C                   eval      c_PraticaDan = dctTAD
045500111221     C                   eval      c_CCA        = wConsAnom
045600111222     C*
045700111222     C                   eval      c_DataCCA    =
045800111222     C                              UBFMTDATE_Convert(
045900111227     C                                  %editc(wDataCCA:'4') :
046000111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
046100111216     C*
046200111216     C                   ENDSR
046300991027
046400110810
046500110810
046600991027      /TITLE Operazioni iniziali.
046700991027     C     *inzsr        BEGSR
046800991027     C*
046900991027     C     *ENTRY        PLIST
047000991027     C                   parm                    prmppt
047100991027     C     wrkesito      parm      wrkesito      prmesito
047200030709     C*
047300991027     C                   ENDSR
