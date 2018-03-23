000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300180109     h*
000400180109     h* EDPES -  EDPLC
000500180109     h* il pgm è da sistemare per il richiamo a UBRTVSPE i cui campi sono stati
000600180109     h* trasformati da signed a packed  attenzione che i 4 campi sono anche in
000700180109     h* prototipo UBLBLSPE definiti signed che servono ad altri pgm
000800180109     h* l'oggetto è stato cancellato
000900991027
001000090325     Ftivgdtmp  uF   E             DISK
001100100209     FTIVAWWWT  UF A E             DISK    commit
001200120112     Fwfwdec01i if   e           k disk
001300991027
001400991027     D prmppt          s             50
001500991027     D prmesito        s              1
001600991027     D wrkesito        s                   like(prmesito)
001700110913     D fnvacds       e ds                  extname(fnvac00t)
001800111220     D fnevbds       e ds                  extname(fnevb00f)
001900111216     D sepFld          s              1    inz(';')
002000111216
002100111216
002200111221     D pInRichiesta    S              3A
002300111220     D pOutTITAS_DS  E DS                  extname(TITAS00F)
002400111216     D pOutFNARB_DS  E DS                  extname(FNARB00F)
002500111216     D pOutFNBLP_DS  E DS                  extname(FNBLP00F)
002600111221     D pOutEsitoSPE    S              3A
002700111222
002800111222     D pInTpChiamDCT   S              2S 0
002900111222     D pOutFNDCT_DS  E DS                  extname(FNDCT02L)
003000111222     D pOutEsitoDCT    S              1A
003100111220
003200111220     D pInNumProg      S              2S 0
003300111220     D pOutTIGCP_DS  E DS                  extname(TIGCP00F)
003400111220     D pOutEsitoGIA    S              1A
003500111220
003600111221     D pInCausale      S              3A
003700111221     D pInDtEvento     S              8S 0
003800111220     D* tipo chiamata:
003900111220     D*  0=prima chiamata
004000111220     D*  1=successive
004100111222     D pInTpChiamEVB   S              1S 0
004200111220     D pOutFNEVB_DS    DS                  occurs(50)
004300111221     D  pOutCausale                   3A
004400111221     D  pOutDtEvento                  8S 0
004500111221     D  pOutOraEvento                 4S 0
004600111221     D  pOutDtImmEv                   8S 0
004700111221     D  pOutOraImmEv                  6S 0
004800111221     D  pOutFilImmEv                  3S 0
004900111220     D* esito reperimento:
005000111220     D*  0=non reperito
005100111220     D*  1=reperito
005200111220     D pOutEsitoEVB    S              1A
005300111220     D pOutNrOccurs    S              3S 0
005400111220     D pOutEnd         S              1A
005500111220
005600110810
005700111216
005800111216     D DS_Cliente      DS
005900111216     D   c_NumBolla                   7
006000111216     D   c_DataBolla                 10
006100111216     D   c_RagSoc                    35
006200111216     D   c_Via                       35
006300111227     D   c_Localita                  20
006400111216     D   c_Prov                       5
006500111216     D   c_Colli                      6
006600111216     D   c_NoteCons                  39
006700111216     D   c_DataUscita                10
006800111216     D   c_DataArrCen                10
006900111216     D   c_DataArrLoc                10
007000111216     D   c_DataCarico                10
007100111216     D   c_DataCons                  10
007200111216     D   c_DataPrenot                10
007300111216     D   c_DataGiac                  10
007400111216     D   c_CodGiac                   13
007500111222     D   c_PraticaDan                10
007600111216     D   c_Riprenotaz                10
007700111221     D   c_CCA                        1
007800111221     D   c_DataCCA                   10
007900110913
008000110913
008100110913     D*------------------
008200110913     D* LINKING A DEFINIZIONI ESTERNE
008300110913     D*------------------
008400110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
008500110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
008600111222     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008700111222     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008800110913
008900110810
009000991027
009100030908     C                   EXSR      chkRECFILMBR
009200991027     C                   EXSR      traduci
009300921023     C                   SETON                                        LR
009400030908
009500110810
009600110810
009700030908     C     chkRECFILMBR  BEGSR
009800030908     C*
009900100209     C                   READ      TIVAWWWT                               55
010000030908     C*
010100030908     C                   ENDSR
010200110810
010300110810
010400991027
010500991027     C     traduci       BEGSR
010600991027     C*
010700090325     C                   READ      tivgdtmp
010800090325     C                   DOW       not %eof
010900110914     c                   movel     vgddta        fnvacds
011000041220     C*
011100111216     C* Inizializzioni
011200111216     C                   CLEAR                   DS_Cliente
011300110913     C*
011400110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
011500110913     C                   MOVEL     *blanks       wEsito1           1
011600110913     C                   MOVEL     *blanks       wEsito2           1
011700111220     C                   Z-ADD     *zeros        wDataCons         8 0
011800111221     C                   Z-ADD     *zeros        wDataCCA          8 0
011900111220     C                   MOVEL     *blanks       wConsAnom         1
012000110913     C*
012100110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
012200110913     C                                                vacAAS
012300110913     C                                               :vacLNP
012400110913     C                                               :vacNRS
012500110913     C                                               :vacNSP
012600110913     C                                               :pOutLblTyp
012700110913     C                                               :pOutAnnoBO
012800110913     C                                               :pOutLineaParBO
012900110913     C                                               :pOutSerieBO
013000110913     C                                               :pOutNumSpedBO
013100110913     C                                               :pOutDcmBO
013200110913     C                                               :pOutCcaBO
013300110913     C                                               :pOutRblBO))
013400111109     C*
013500111109     C* Ritorno solamente le bolle valide come spedizione
013600111223     C                   IF        pOutLblTyp = 'O' AND pOutRblBO = 'N'
013700110913     C*
013800110913     C* Chiamata metodo GetLastChild
013900110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
014000110913     C                                                pOutAnnoBO
014100110913     C                                               :pOutLineaParBO
014200110913     C                                               :pOutSerieBO
014300110913     C                                               :pOutNumSpedBO
014400110913     C                                               :pOutAnnoFI
014500110913     C                                               :pOutLineaParFI
014600110913     C                                               :pOutSerieFI
014700110913     C                                               :pOutNumSpedFI
014800110913     C                                               :pOutDcmFI
014900110913     C                                               :pOutCcaFI))
015000110914     C*
015100110914     C                   IF        wEsito2 = '0'
015200110914     C                   EVAL      wDataCons = pOutDcmFI
015300110914     C                   EVAL      wConsAnom = pOutCcaFI
015400110914     C                   ELSE
015500110914     C                   EVAL      wDataCons = pOutDcmBO
015600110914     C                   EVAL      wConsAnom = pOutCcaBO
015700110914     C                   ENDIF
015800111221     C*
015900111221     C                   IF        wConsAnom <> *blanks
016000111221     C                   EVAL      wDataCCA  = wDataCons
016100111221     C                   EVAL      wDataCons = *zeros
016200111221     C                   ENDIF
016300111221     C*
016400111221     C* Ritorno solamente le bolle originali
016500111221     C                   IF        pOutLblTyp = 'O'
016600111220     C*
016700111220     C* Reperisco i dati della spedizione
016800111220     C                   EXSR      rtvDatiBolla
016900110913     C*
017000111216     C* Compongo il buffer d out
017100110810     C                   EVAL      VAWDTA  =
017200111216     C                             c_NumBolla                           +sepFld+
017300111216     C                             c_DataBolla                          +sepFld+
017400111216     C                             c_RagSoc                             +sepFld+
017500111216     C                             c_Via                                +sepFld+
017600111216     C                             c_Localita                           +sepFld+
017700111216     C                             c_Prov                               +sepFld+
017800111216     C                             c_Colli                              +sepFld+
017900111216     C                             c_NoteCons                           +sepFld+
018000111216     C                             c_DataUscita                         +sepFld+
018100111216     C                             c_DataArrCen                         +sepFld+
018200111216     C                             c_DataArrLoc                         +sepFld+
018300111216     C                             c_DataCarico                         +sepFld+
018400111216     C                             c_DataCons                           +sepFld+
018500111216     C                             c_DataPrenot                         +sepFld+
018600111216     C                             c_DataGiac                           +sepFld+
018700111216     C                             c_CodGiac                            +sepFld+
018800111216     C                             c_PraticaDan                         +sepFld+
018900111221     C                             c_Riprenotaz                         +sepFld+
019000111221     C                             c_CCA                                +sepFld+
019100111227     C                             c_DataCCA                            +X'00'
019200930409     C*
019300120221     C* Scivo solo a "determinate" condizioni
019400120221     C                   IF        %len(%trim(c_NumBolla))  <  7 OR
019500120221     C                             %len(%trim(c_DataBolla)) < 10
019600120221     C                   ELSE
019700100209     C                   WRITE     TIVAW000
019800120221     C                   ENDIF
019900111109     C*
020000111221     C                   ENDIF
020100111109     C                   ENDIF
020200110810     C*
020300090325     C                   delete    tivgd000
020400991027     C*
020500090325     C                   READ      tivgdtmp
020600030325     C                   ENDDO
020700991027     C*
020800030325     C                   EVAL      wrkesito = '0'
020900991027     C*
021000910830     C                   ENDSR
021100111216
021200111216
021300111216
021400111216     C     rtvDatiBolla  BEGSR
021500111220     C*
021600111220     C* Reperisco gli eventi 'MIC'
021700111228     C                   z-add     *zeros        pInTpChiamEVB
021800111228     C                   dow       pOutEnd <> '1' AND pInTpChiamEVB < 50
021900111228     C*
022000111220     C                   call      'UBRTVEVB'
022100111220     C                   parm      vacAAS        pInAnno
022200111220     C                   parm      vacLNP        pInLineaPar
022300111220     C                   parm      vacNRS        pInSerie
022400111220     C                   parm      vacNSP        pInNumSped
022500111220     C                   parm      'MIC'         pInCausale
022600111220     C                   parm      *zeros        pInDtEvento
022700111228     C                   parm                    pInTpChiamEVB
022800111220     C                   parm                    pOutFNEVB_DS
022900111220     C                   parm                    pOutEsitoEVB
023000111220     C                   parm                    pOutNrOccurs
023100111220     C                   parm                    pOutEnd
023200111228     C*
023300111228     C* Reperisco i dati del buffer FNDCT
023400111228     C                   if        pOutNrOccurs > *zeros
023500111228     C     pOutNrOccurs  occur     pOutFNEVB_DS
023600111228     C                   endif
023700111220     C*
023800111228     C* Proseguo con le successive chiamate ad driver
023900111228     C                   add       1             pInTpChiamEVB
024000111228     C                   enddo
024100111222     C*
024200111222     C* Reperisco l'ultima giacenza
024300111222     C                   call      'UBRTVGIAC'
024400111222     C                   parm      vacAAS        pInAnno
024500111222     C                   parm      vacLNP        pInLineaPar
024600111222     C                   parm      vacNRS        pInSerie
024700111222     C                   parm      vacNSP        pInNumSped
024800111222     C                   parm      *zeros        pInNumProg
024900111222     C                   parm                    pOutTIGCP_DS
025000111222     C                   parm                    pOutEsitoGIA
025100111222     C*
025200111222     C* Reperisco l'ultimo danno ("valido ai fini internet")
025300111222     C                   call      'UBRTVDCT'
025400111222     C                   parm      vacAAS        pInAnno
025500111222     C                   parm      vacLNP        pInLineaPar
025600111222     C                   parm      vacNRS        pInSerie
025700111222     C                   parm      vacNSP        pInNumSped
025800111222     C                   parm      01            pInTpChiamDCT
025900111222     C                   parm                    pOutFNDCT_DS
026000111222     C                   parm                    pOutEsitoDCT
026100111220     C*
026200111220     C* Reperisco i dati di testata
026300111220     C                   call      'UBRTVSPE'
026400111221     C                   parm      'APS'         pInRichiesta
026500111220     C                   parm      vacAAS        pInAnno
026600111220     C                   parm      vacLNP        pInLineaPar
026700111220     C                   parm      vacNRS        pInSerie
026800111220     C                   parm      vacNSP        pInNumSped
026900111220     C                   parm                    pOutFNARB_DS
027000111220     C                   parm                    pOutFNBLP_DS
027100111221     C                   parm                    pOutTITAS_DS
027200111221     C                   parm                    pOutEsitoSPE
027300111220     C*
027400111221     C* Valorizzo il buffer d outout
027500111221     C                   select
027600111221     C*
027700111221     C                   when      %subst(pOutEsitoSPE:1:2) = 'AP'
027800111221     C                   eval      c_Via        = %trim(arbIND)
027900111221     C                   eval      c_Localita   = %trim(arbLOD)
028000111222     C*
028100111222     C                   eval      c_DataUscita =
028200111222     C                              UBFMTDATE_Convert(
028300111227     C                                  %editc(blpDRT:'4') :
028400111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
028500111222     C*
028600111222     C                   eval      c_DataArrCen =
028700111222     C                              UBFMTDATE_Convert(
028800111227     C                                  %editc(arbAAS*10000+arbMGS:'4') :
028900111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
029000111222     C*
029100111222     C                   eval      c_DataArrLoc =
029200111222     C                              UBFMTDATE_Convert(
029300111227     C                                  %editc(arbDTI:'4') :
029400111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
029500111222     C*
029600120214     C                   eval      c_DataPrenot = *blanks
029700120214     C***                           UBFMTDATE_Convert(
029800120214     C***                               %editc(arbDTI:'4') :
029900120214     C***                               'YYYYMMDD' : 'DD/MM/YYYY')
030000111222     C*
030100111222     C                   eval      c_Riprenotaz =
030200111222     C                              UBFMTDATE_Convert(
030300111227     C                                  %editc(arbDCR:'4') :
030400111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
030500111221     C*
030600111221     C                   when      %subst(pOutEsitoSPE:1:2) = 'A '
030700111221     C                   eval      c_Via        = %trim(arbIND)
030800111221     C                   eval      c_Localita   = %trim(arbLOD)
030900111222     C*
031000111222     C                   eval      c_DataUscita =
031100111222     C                              UBFMTDATE_Convert(
031200111227     C                                  %editc(arbDTI:'4') :
031300111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
031400111222     C*
031500111222     C                   eval      c_DataArrCen =
031600111222     C                              UBFMTDATE_Convert(
031700111227     C                                  %editc(arbAAS*10000+arbMGS:'4') :
031800111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
031900111222     C*
032000111222     C                   eval      c_DataArrLoc =
032100111222     C                              UBFMTDATE_Convert(
032200111227     C                                  %editc(arbDTI:'4') :
032300111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
032400111222     C*
032500120214     C                   eval      c_DataPrenot = *blanks
032600120214     C***                           UBFMTDATE_Convert(
032700120214     C***                               %editc(arbDTI:'4') :
032800120214     C***                               'YYYYMMDD' : 'DD/MM/YYYY')
032900111222     C*
033000111222     C                   eval      c_Riprenotaz =
033100111222     C                              UBFMTDATE_Convert(
033200111227     C                                  %editc(arbDCR:'4') :
033300111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
033400111221     C*
033500111221     C                   when      %subst(pOutEsitoSPE:1:2) = ' P'
033600111221     C                   eval      c_Via        = %trim(blpIND)
033700111221     C                   eval      c_Localita   = %trim(blpLOD)
033800111222     C*
033900111222     C                   eval      c_DataUscita =
034000111222     C                              UBFMTDATE_Convert(
034100111227     C                                  %editc(blpDRT:'4') :
034200111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
034300111222     C*
034400111222     C                   eval      c_DataArrCen =
034500111222     C                              UBFMTDATE_Convert(
034600111227     C                                  %editc(blpAAS*10000+blpMGS:'4') :
034700111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
034800111222     C*
034900111222     C                   eval      c_DataArrLoc =
035000111222     C                              UBFMTDATE_Convert(
035100111227     C                                  %editc(blpDTI:'4') :
035200111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
035300111222     C*
035400120214     C                   eval      c_DataPrenot = *blanks
035500120214     C***                           UBFMTDATE_Convert(
035600120214     C***                               %editc(blpDTI:'4') :
035700120214     C***                               'YYYYMMDD' : 'DD/MM/YYYY')
035800111222     C*
035900111222     C                   eval      c_Riprenotaz =
036000111222     C                              UBFMTDATE_Convert(
036100111227     C                                  %editc(blpDCR:'4') :
036200111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
036300111221     C*
036400111221     C                   when      %subst(pOutEsitoSPE:3:1) = 'S'
036500111221     C                   eval      c_Via        = %trim(tasIND)
036600111221     C                   eval      c_Localita   = %trim(tasLOD)
036700111222     C*
036800111222     C                   eval      c_DataUscita =
036900111222     C                              UBFMTDATE_Convert(
037000111227     C                                  %editc(tasDRT:'4') :
037100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
037200111222     C*
037300111222     C                   eval      c_DataArrCen =
037400111222     C                              UBFMTDATE_Convert(
037500111227     C                                  %editc(tasAAS*10000+tasMGS:'4') :
037600111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
037700111222     C*
037800111222     C                   eval      c_DataArrLoc =
037900111222     C                              UBFMTDATE_Convert(
038000111227     C                                  %editc(tasDTI:'4') :
038100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
038200111222     C*
038300120214     C                   eval      c_DataPrenot = *blanks
038400120214     C***                           UBFMTDATE_Convert(
038500120214     C***                               %editc(tasDTI:'4') :
038600120214     C***                               'YYYYMMDD' : 'DD/MM/YYYY')
038700111222     C*
038800111222     C                   eval      c_Riprenotaz =
038900111222     C                              UBFMTDATE_Convert(
039000111227     C                                  %editc(tasDCR:'4') :
039100111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
039200111221     C                   endsl
039300111216     C*
039400111221     C                   exsr      valBuffer
039500111221     C*
039600111216     C                   ENDSR
039700111216
039800111216
039900111216
040000111221     C     valBuffer     BEGSR
040100120112     C*
040200120112     C* Forzatura dati x sistemazione casino del cliente con logistica
040300120112     C                   z-add     vacRMN        wRMN              9 0
040400120112     C                   if        vacRMN < 1000000
040500120112     C* 1° tentativo x 2011
040600120112     C                   add       11000000      wRMN
040700120112     C     wRMN          chain     wfwdec01i
040800120112     C                   if        %found(wfwdec01i)
040900120112     C                   eval      vacRMN = SEL0001 - 10000000
041000120221     C                   if        %trim(vacRMA) = *all'*'
041100120217     C                   eval      vacRMA = *blanks
041200120217     C                   else
041300120112     C                   eval      vacRMA =
041400120112     C                              UBFMTDATE_Convert(
041500120112     C                                  %editc(%dec(C1XOL:*ISO):'4'):
041600120112     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
041700120217     C                   endif
041800120217     C*
041900120112     C                   else
042000120112     C* 2° tentativo x 2012
042100120112     C                   add       12000000      wRMN
042200120112     C     wRMN          chain     wfwdec01i
042300120112     C                   if        %found(wfwdec01i)
042400120112     C                   eval      vacRMN = SEL0001 - 10000000
042500120221     C                   if        %trim(vacRMA) = *all'*'
042600120217     C                   eval      vacRMA = *blanks
042700120217     C                   else
042800120112     C                   eval      vacRMA =
042900120112     C                              UBFMTDATE_Convert(
043000120112     C                                  %editc(%dec(C1XOL:*ISO):'4'):
043100120112     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
043200120217     C                   endif
043300120112     C                   endif
043400120112     C                   endif
043500120112     C                   endif
043600111216     C*
043700120112     C                   eval      c_NumBolla   = %trim(%editc(vacRMN:'Q'))
043800120112     C                   eval      c_DataBolla  = %trim(vacRMA)
043900111221     C                   eval      c_RagSoc     = %trim(vacRSD)
044000111221     C                   eval      c_Prov       = vacPRD
044100111221     C                   eval      c_Colli      = %char(vacNCL)
044200111216     C                   eval      c_NoteCons   = *blanks
044300111222     C*
044400111222     C                   eval      c_DataCarico =
044500111222     C                              UBFMTDATE_Convert(
044600111227     C                                  %editc(pOutDtEvento:'4') :
044700111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
044800111222     C*
044900111222     C                   eval      c_DataCons   =
045000111222     C                              UBFMTDATE_Convert(
045100111227     C                                  %editc(wDataCons:'4') :
045200111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
045300111222     C*
045400111222     C                   eval      c_DataGiac   =
045500111222     C                              UBFMTDATE_Convert(
045600111227     C                                  %editc(gcpAGC*10000+gcpMGC:'4') :
045700111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
045800111222     C*
045900111220     C                   eval      c_CodGiac    = gcpCMC
046000111222     C                   eval      c_PraticaDan = dctTAD
046100111221     C                   eval      c_CCA        = wConsAnom
046200111222     C*
046300111222     C                   eval      c_DataCCA    =
046400111222     C                              UBFMTDATE_Convert(
046500111227     C                                  %editc(wDataCCA:'4') :
046600111227     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
046700111216     C*
046800111216     C                   ENDSR
046900991027
047000110810
047100110810
047200991027      /TITLE Operazioni iniziali.
047300991027     C     *inzsr        BEGSR
047400991027     C*
047500991027     C     *ENTRY        PLIST
047600991027     C                   parm                    prmppt
047700991027     C     wrkesito      parm      wrkesito      prmesito
047800030709     C*
047900991027     C                   ENDSR
