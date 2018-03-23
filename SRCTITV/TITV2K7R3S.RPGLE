000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120427
000400110103     FAZORG01L  IF   E           K DISK
000500120427     FTNTBE11L  IF   E           K DISK
000600110927     FTABEL00F  IF   E           K DISK
000700031103     FTITAS30C  IF   E           K DISK
000800120427     FTITAH30C  IF   E           K DISK
000900141119     FTITA430C  IF   E           K DISK
001000110103     FTISTB03L  UF   E           K DISK    commit
001100090323     FTIVAWWWT  UF A E             DISK    commit
001200031103
001300031103     D*-----------
001400120427     D* SCHIERE
001500031103     D*-----------
001600120427     D skCAUBAR        S              3    DIM(999)
001700120427     D skCAUDPD        S              9    DIM(999)
001800120427     D skCAUDES        S             30    DIM(999)
001900120427     D jCAU            S              3  0 INZ(*zeros)
002000120427     D skPOBAR         S              3  0 DIM(999)
002100120427     D skPODPD         S             25    DIM(999)
002200120427     D skPODES         S             20    DIM(999)
002300120427     D jPO             S              3  0 INZ(*zeros)
002400120427
002500030924
002600041015     D*-----------
002700041015     D* VARIABILI D WRK
002800041015     D*-----------
002900031103     D prmppi          S             50
003000030924     D prmesito        S              1
003100030924     D wrkesito        S                   like(prmesito)
003200141119     D wFIRMATAR       S                   inz(*blanks) like(ta4NOT)
003300110929
003400041015
003500041015     D*-----------
003600041015     D* DEFINIZIONE DS ESTERNE
003700041015     D*-----------
003800111116     D DS2Z          E DS
003900120427     D DICE          E DS
004000120427     D OG143         E DS
004100031103
004200031103     D*-----------
004300031103     D* VARIABILI RIFERITE AL DATA-BASE
004400031103     D*-----------
004500080618     D wKstbKSU        S                   like(stbKSU)
004600080618     D wKstbFTR        S                   like(stbFTR)
004700030924
004800110103
004900031103
005000031103     D*-----------
005100031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005200031103     D*-----------
005300031103     D DSPPI           DS
005400080618     D  w§PPIKSU                      7  0
005500120427     D  w§PPILIN                      1
005600130710     D  w§PPIINT                      1
005700031103
005800110928
005900110928     D*------------------
006000110928     D* LINKING A DEFINIZIONI ESTERNE
006100110928     D*------------------
006200110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006300110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006400110928
006500030924
006600030325
006700030924     C* MAIN/
006800041015     C*
006900041015     C* Carica i dati tabellati
007000041015     C                   EXSR      cartab
007100130710     C*
007200130710     C* Verifico se già presenti dati nel file d output
007300130710     C                   EXSR      chkRECFILMBR
007400041015     C*
007500041015     C* Esegue traduzione
007600991027     C                   EXSR      traduci
007700110929     C*
007800921023     C                   SETON                                        LR
007900111121
008000111121
008100111121
008200111121     C     cartab        BEGSR
008300111121     C*
008400120427     C* Carico causali evento 2Z e descrizioni in lingua (ICE)
008500120427     C                   clear                   skCAUBAR
008600120427     C                   clear                   skCAUDPD
008700120427     C                   clear                   skCAUDES
008800120427     C                   z-add     *zeros        jCAU
008900111121     C                   eval      tblKUT = 1
009000120427     C                   eval      tblCOD = '2Z'
009100111121     C     KEYtab00P     setll     tabel00f
009200111121     C     KEYtab00P     reade     tabel00f
009300111121     C                   dow       not %eof(tabel00f)
009400120427     C                   add       1             jCAU
009500120427     C                   movel(p)  tblUNI        DS2Z
009600120427     C                   eval      skCAUBAR(jCAU) = tblKEY
009700120427     C                   eval      skCAUDPD(jCAU) = §2ZEVD+§2ZCED
009800120427     C*
009900120427     C                   eval      tbeLIN = w§PPILIN
010000120427     C                   eval      tbeCOD = 'ICE'
010100120427     C                   eval      tbeKE1 = tblKEY
010200120427     C     KEYtbe11P     chain     tntbe11l
010300120427     C                   if        %found(tntbe11l)
010400120427     C                   eval      DICE = tbeUNI
010500120427     C                   eval      skCAUDES(jCAU) = §ICEDEP
010600120427     C                   else
010700120427     C                   eval      skCAUDES(jCAU) = '-'
010800120427     C                   endif
010900120427     C*
011000111121     C     KEYtab00P     reade     tabel00f
011100111121     C                   enddo
011200120427     C*
011300120427     C* Caricamento organigramma BAR e DPD
011400120427     C                   clear                   skPOBAR
011500120427     C                   clear                   skPODPD
011600120427     C                   clear                   skPODES
011700120427     C                   z-add     *zeros        jPO
011800120427     C     *loval        setll     azorg01l
011900120427     C                   read      azorg01l
012000120427     C                   dow       not %eof(azorg01l)
012100120427     C                   if        orgDE3 <> *blanks
012200120427     C*
012300120427     C                   add       1             jPO
012400120427     C*
012500120427     C                   movel     orgFIL        skPOBAR(jPO)
012600120427     C                   movel     orgDE3        skPODPD(jPO)
012700120427     C                   movel     orgDES        skPODES(jPO)
012800120427     C*
012900120427     C                   endif
013000111121     C*
013100120427     C                   read      azorg01l
013200120427     C                   enddo
013300120427     C*
013400111121     C                   ENDSR
013500991027
013600130710
013700130710
013800130710     C     chkRECFILMBR  BEGSR
013900130710     C*
014000130710     C                   READ      TIVAWWWT                               55
014100130710     C*
014200130710     C                   ENDSR
014300130710
014400030924
014500030924
014600991027     C     traduci       BEGSR
014700130710     C*
014800130710     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
014900130710     C                   IF        w§PPIINT = 'I' AND *IN55 = *ON
015000130710     C                   EVAL      vawDTA =
015100130710     C                             'PARCELNO;SCAN_CODE;DEPOT_CODE;DEPOTNAME;'+
015200130710     C                             'EVENT_DATE_TIME;ROUTE;TOUR;PCODE;SERVICE;'+
015300130710     C                             'CONSIGNEE_COUNTRY_CODE;CONSIGNEE_ZIP;'+
015400130710     C                             'ADD_SERVICE_1;ADD_SERVICE_2;ADD_SERVICE_3;'+
015500130710     C                             'WEIGHT;CUSTOMER_REFERENCE;POD_IMAGE_REF;'+
015600130710     C                             'RECEIVER_NAME;INFO_TEXT;LOCATION;'
015700130710     C                   WRITE     TIVAW000
015800130710     C                   ENDIF
015900031103     C*
016000031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
016100031103     C* di traduzione (trattasi d "post-traduzione")
016200120427     C                   eval      wKstbKSU = w§PPIKSU
016300120427     C                   eval      wKstbFTR = *blanks
016400120427     C     KEYstb03P     setll     tistb03l
016500120427     C                   if        %equal(tistb03l)
016600120427     C     KEYstb03P     reade     tistb03l
016700991027     C*
016800031103     C* Ciclo x tuttii record del cliente da trasmettere
016900120427     C                   dow       not %eof(tistb03l)
017000080618     C*
017100120427     C                   seton                                        50
017200031103     C*
017300031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
017400031103     C* deve restituire al cliente nel file tradotto
017500120427     C                   if        stbFTR = *blanks                             * ridondante
017600110103     C*
017700120427     C                   setoff                                       50
017800031103     C*
017900031103     C* Innanzitutto chaino la bolla direttamente su TITAS
018000120427     C     KEYtas30P     chain     titas30c
018100120427     C                   if        %found(titas30c)
018200141119     C*
018300141119     C* Reperisco, se c'è, la firma del consegnatario
018400141119     C                   eval      ta4TRC = '1'
018500141119     C     KEYta430C     chain     tita430c
018600141119     C                   if        %found(tita430c)
018700141119     C                   eval      wFIRMATAR = ta4NOT
018800141119     C                   else
018900141119     C                   eval      wFIRMATAR = *blanks
019000141119     C                   endif
019100110103     C*
019200120427     C* Quindi aggancio l'estensione dettagli segnacolli
019300120427     C                   clear                   tahNOT
019400120427     C                   eval      tahTRC = 'C'
019500120427     C     KEYtah30P     chain     titah30c
019600110927     C*
019700120427     C                   z-add     *zeros        wPO               3 0
019800120427     C*
019900120427     C                   movel     *blanks       oParcelNbr      256
020000120427     C                   movel     *blanks       oScanEvent      256
020100130710     C                   movel     *blanks       oException      256
020200120427     C                   movel     *blanks       oScanEventDes   256
020300120427     C                   movel     *blanks       oScanDptCode    256
020400120427     C                   movel     *blanks       oScanDptName    256
020500120427     C                   movel     *blanks       oEventDatTim    256
020600130710     C                   movel     *blanks       oConsignName    256
020700130710     C                   movel     *blanks       oConsignStrt    256
020800130710     C                   movel     *blanks       oRoute          256
020900130710     C                   movel     *blanks       oTour           256
021000170607     C                   movel     '00'          oPcode          256
021100130710     C                   movel     *blanks       oService        256
021200120427     C                   movel     *blanks       oConsignCntry   256
021300120427     C                   movel     *blanks       oConsignZip     256
021400130710     C                   movel     *blanks       oConsignCity    256
021500120427     C                   movel     *blanks       oAddService01   256
021600120427     C                   movel     *blanks       oAddService02   256
021700120427     C                   movel     *blanks       oAddService03   256
021800120427     C                   movel     *blanks       oWeight         256
021900120620     C                   movel     *zeros        oSenderRefID     14
022000141119     C                   movel     *zeros        oBRTspedID       12
022100120427     C                   movel     *blanks       oPOD_Image      256
022200120427     C                   movel     *blanks       oReceivrName    256
022300130710     C                   movel     *blanks       oInfo_Text      256
022400130710     C                   movel     *blanks       oLocation       256
022500141119     C                   movel     *blanks       oFirmaCons      256
022600120427     C*
022700120427     C* Innanzitutto valorizzo i dati "correnti"
022800120427     C                   eval      oParcelNbr    = tahNOT
022900120427     C                   eval      oEventDatTim  = %trim(%editc(stbDAS:'X'))+
023000130712     C                                             %trim(%editc(stbORS:'X'))+
023100130712     C                                             '00'
023200120427     C                   eval      oConsignName  = tasRSD
023300120427     C                   eval      oConsignStrt  = tasIND
023400120427     C                   eval      oConsignCntry = '380'
023500120427     C                   eval      oConsignZip   = tasCAD
023600120427     C                   eval      oConsignCity  = tasLOD
023700120427     C                   eval      oWeight       = %editw(tasPKB:'     0 , ')
023800120620     C                   evalr     oSenderRefID  = %trim(%editc(tasRMN:'X'))
023900141119     C                   eval      oBRTspedID    = %editc(stbLNP:'X')+
024000141119     C                                             %editc(stbNRS:'X')+
024100141119     C                                             %editc(stbNSP:'X')
024200130710     C                   eval      oService      = '101'
024300141119     C                   eval      oFirmaCons    = %trim(wFIRMATAR)
024400120427     C*
024500120427     C                   select
024600120427     C*
024700120427     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
024800120427     C                   eval      oScanEvent = '01'
024900120427     C                   eval      wPO = tasLNP
025000120427     C*
025100120427     C                   when      %subst(stbCOS:1:9) = 'ARRPRICOL'
025200120427     C                   eval      oScanEvent = '02'
025300120427     C                   eval      wPO = tasLNA
025400120427     C*
025500120427     C                   when      %subst(stbCOS:1:4) = 'RESO'
025600120427     C                   eval      oScanEvent = '06'
025700120427     C                   eval      wPO = tasLNA
025800120427     C*
025900120620     C                   when      %trim(stbCOS) = 'CONSOK'
026000120427     C                   eval      oScanEvent = '13'
026100120427     C                   eval      wPO = tasLNA
026200120427     C*
026300120427     C                   other
026400120427     C                   movel     stbCOS        wCOS              3
026500120427     C                   z-add     1             jCAU
026600120427     C     wCOS          lookup    skCAUBAR(jCAU)                         55
026700120427     C                   if        %found and skCAUDPD(jCAU) <> *blanks
026800120427     C                   eval      oScanEvent    = %subst(skCAUDPD(jCAU):1:3)
026900120427     C                   eval      oException    = %subst(skCAUDPD(jCAU):4:6)
027000120427     C                   eval      oScanEventDes = skCAUDES(jCAU)
027100120427     C                   eval      wPO = tasLNA
027200120427     C                   else
027300120427     C                   seton                                        50
027400110927     C                   endif
027500120427     C*
027600120427     C                   endsl
027700120427     C*
027800120427     C                   clear                   OG143
027900120427     C                   z-add     1             jPO
028000120427     C     wPO           lookup    skPOBAR(jPO)                           55
028100120427     C                   if        %found
028200120427     C                   movel     skPODPD(jPO)  OG143
028300120427     C                   eval      oScanDptCode  = §OGDP1
028400120427     C                   eval      oScanDptName  = skPODES(jPO)
028500120427     C                   endif
028600110927     C*
028700120427     C                   else
028800120427     C                   seton                                        50
028900120427     C                   endif
029000120427     C                   endif
029100080618     C*
029200080618     C* Scarico il buffer d output
029300120427     C  N50              exsr      WRIREC
029400031103     C*
029500031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
029600120427     C                   eval      stbFTR = 'T'
029700120427     C                   update    tistb000
029800991027     C*
029900120427     C     KEYstb03P     reade     tistb03l
030000120427     C                   setoff                                       50
030100120427     C                   enddo
030200120427     C                   endif
030300991027     C*
030400120427     C                   eval      wrkesito = '0'
030500991027     C*
030600910830     C                   ENDSR
030700031103
030800031103
030900031103
031000110103     C     wrirec        BEGSR
031100110928     C*
031200120427     C* Considero solamente le bolle originali
031300111103     C                   movel     *blanks       wEsito1           1
031400111103     C*
031500111103     C* Chiamata metodo GetLblTyp
031600111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
031700111103     C                                                stbAAS
031800111103     C                                               :stbLNP
031900111103     C                                               :stbNRS
032000111103     C                                               :stbNSP
032100111103     C                                               :pOutLblTyp
032200111103     C                                               :pOutAnnoBO
032300111103     C                                               :pOutLineaParBO
032400111103     C                                               :pOutSerieBO
032500111103     C                                               :pOutNumSpedBO
032600111103     C                                               :pOutDcmBO
032700111103     C                                               :pOutCcaBO
032800111103     C                                               :pOutRblBO))
032900111103     C*
033000120427     C                   if        pOutLblTyp = 'O' and
033100120427     C                             not *IN50                                    * ridondante
033200141119     C*
033300141119     C* Forzatura su data/ora evento => se non pervenuta escludo il record
033400141204     C                   if        %trim(oEventDatTim) <> *zeros AND
033500141204     C                             %trim(oEventDatTim) <> *blanks
033600141119     C*
033700031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
033800120427     C                   eval      VAWDTA =
033900131030     C***                                 %trim(oParcelNbr)
034000131030     C                                    %trim(oSenderRefID)
034100120427     C                               +';'+%trim(oScanEvent)
034200130710     C***                            +';'+%trim(oScanEventDes)
034300130710     C                               +';'+%trim(oScanDptCode)
034400120427     C                               +';'+%trim(oScanDptName)
034500120427     C                               +';'+%trim(oEventDatTim)
034600130710     C                               +';'+%trim(oRoute)
034700130710     C                               +';'+%trim(oTour)
034800130710     C                               +';'+%trim(oPcode)
034900130710     C                               +';'+%trim(oService)
035000130710     C***                            +';'+%trim(oConsignName)
035100130710     C***                            +';'+%trim(oConsignStrt)
035200120427     C                               +';'+%trim(oConsignCntry)
035300120427     C                               +';'+%trim(oConsignZip)
035400130710     C***                            +';'+%trim(oConsignCity)
035500141120     C***                            +';'+%trim(oAddService01)
035600141120     C                               +';'+%trim(oException)
035700120427     C                               +';'+%trim(oAddService02)
035800120427     C                               +';'+%trim(oAddService03)
035900120427     C                               +';'+%trim(oWeight)
036000141119     C***                            +';'+%trim(oSenderRefID)
036100141119     C                               +';'+%trim(oBRTspedID)
036200120427     C                               +';'+%trim(oPOD_Image)
036300141119     C***                            +';'+%trim(oReceivrName)
036400141119     C                               +';'+%trim(oFirmaCons)
036500130710     C                               +';'+%trim(oInfo_Text)
036600130710     C                               +';'+%trim(oLocation)
036700120427     C                               +';'
036800110927     C*
036900120427     C                   write     TIVAW000
037000120427     C                   endif
037100141119     C*
037200141119     C                   endif
037300031103     C*
037400031103     C                   ENDSR
037500031103
037600031103
037700991027
037800991027      /TITLE Operazioni iniziali.
037900991027     C     *inzsr        BEGSR
038000991027     C*
038100991027     C     *ENTRY        PLIST
038200031103     C                   parm                    prmppi
038300991027     C     wrkesito      parm      wrkesito      prmesito
038400031103     C*
038500031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
038600031103     C                   EVAL      DSPPI = prmppi
038700030709     C*
038800030709     C* Definizione chiavi
038900110927     C*
039000110927     C* Chiave su TABEL00F - Parziale
039100110927     C     KEYtab00P     KLIST
039200110927     C                   KFLD                    tblKUT
039300110927     C                   KFLD                    tblCOD
039400120427     C*
039500120427     C* Chiave su TNTBE00F - Parziale
039600120427     C     KEYtbe11P     KLIST
039700120427     C                   KFLD                    tbeLIN
039800120427     C                   KFLD                    tbeCOD
039900120427     C                   KFLD                    tbeKE1
040000030924     C*
040100110103     C* Chiave su TISTB03L - Parziale
040200110103     C     KEYstb03P     KLIST
040300031104     C                   KFLD                    wKstbKSU
040400031103     C                   KFLD                    wKstbFTR
040500031103     C*
040600031103     C* Chiave su TITAS30C - Parziale
040700031103     C     KEYtas30P     KLIST
040800031103     C                   KFLD                    stbAAS
040900031103     C                   KFLD                    stbLNP
041000031103     C                   KFLD                    stbNRS
041100031103     C                   KFLD                    stbNSP
041200120427     C*
041300120427     C* Chiave su TITAH30C - Parziale
041400120427     C     KEYtah30P     KLIST
041500120427     C                   KFLD                    stbAAS
041600120427     C                   KFLD                    stbLNP
041700120427     C                   KFLD                    stbNRS
041800120427     C                   KFLD                    stbNSP
041900120427     C                   KFLD                    tahTRC
042000141119     C*
042100141119     C* Chiave su TITA430C - Completa
042200141119     C     KEYta430C     KLIST
042300141119     C                   KFLD                    stbAAS
042400141119     C                   KFLD                    stbLNP
042500141119     C                   KFLD                    stbNRS
042600141119     C                   KFLD                    stbNSP
042700141119     C                   KFLD                    ta4TRC
042800991027     C*
042900991027     C                   ENDSR
