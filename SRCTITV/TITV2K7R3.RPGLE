000100170719     H DECEDIT('0.') DATEDIT(*DMY.)
000200120427     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120427
000400110103     FAZORG01L  IF   E           K DISK
000500120427     FTNTBE11L  IF   E           K DISK
000600110927     FTABEL00F  IF   E           K DISK
000700031103     FTITAS30C  IF   E           K DISK
000800120427     FTITAH30C  IF   E           K DISK
000900141119     FTITA430C  IF   E           K DISK
001000171003     FTISTB03L  UF   E           K DISK    commit
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
003300170712     D sVAWDTA         S                   like(VAWDTA) inz
003400110929
003500041015
003600041015     D*-----------
003700041015     D* DEFINIZIONE DS ESTERNE
003800041015     D*-----------
003900111116     D DS2Z          E DS
004000120427     D DICE          E DS
004100120427     D OG143         E DS
004200031103
004300031103     D*-----------
004400031103     D* VARIABILI RIFERITE AL DATA-BASE
004500031103     D*-----------
004600080618     D wKstbKSU        S                   like(stbKSU)
004700080618     D wKstbFTR        S                   like(stbFTR)
004800030924
004900110103
005000031103
005100031103     D*-----------
005200031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005300031103     D*-----------
005400031103     D DSPPI           DS
005500080618     D  w§PPIKSU                      7  0
005600120427     D  w§PPILIN                      1
005700130710     D  w§PPIINT                      1
005800031103
005900110928
006000110928     D*------------------
006100110928     D* LINKING A DEFINIZIONI ESTERNE
006200110928     D*------------------
006300110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006400110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006500110928
006600030924
006700030325
006800030924     C* MAIN/
006900041015     C*
007000041015     C* Carica i dati tabellati
007100041015     C                   EXSR      cartab
007200130710     C*
007300130710     C* Verifico se già presenti dati nel file d output
007400130710     C                   EXSR      chkRECFILMBR
007500041015     C*
007600041015     C* Esegue traduzione
007700991027     C                   EXSR      traduci
007800110929     C*
007900921023     C                   SETON                                        LR
008000111121
008100111121
008200111121
008300111121     C     cartab        BEGSR
008400111121     C*
008500120427     C* Carico causali evento 2Z e descrizioni in lingua (ICE)
008600120427     C                   clear                   skCAUBAR
008700120427     C                   clear                   skCAUDPD
008800120427     C                   clear                   skCAUDES
008900120427     C                   z-add     *zeros        jCAU
009000111121     C                   eval      tblKUT = 1
009100120427     C                   eval      tblCOD = '2Z'
009200111121     C     KEYtab00P     setll     tabel00f
009300111121     C     KEYtab00P     reade     tabel00f
009400111121     C                   dow       not %eof(tabel00f)
009500120427     C                   add       1             jCAU
009600120427     C                   movel(p)  tblUNI        DS2Z
009700120427     C                   eval      skCAUBAR(jCAU) = tblKEY
009800120427     C                   eval      skCAUDPD(jCAU) = §2ZEVD+§2ZCED
009900120427     C*
010000120427     C                   eval      tbeLIN = w§PPILIN
010100120427     C                   eval      tbeCOD = 'ICE'
010200120427     C                   eval      tbeKE1 = tblKEY
010300120427     C     KEYtbe11P     chain     tntbe11l
010400120427     C                   if        %found(tntbe11l)
010500120427     C                   eval      DICE = tbeUNI
010600120427     C                   eval      skCAUDES(jCAU) = §ICEDEP
010700120427     C                   else
010800120427     C                   eval      skCAUDES(jCAU) = '-'
010900120427     C                   endif
011000120427     C*
011100111121     C     KEYtab00P     reade     tabel00f
011200111121     C                   enddo
011300120427     C*
011400120427     C* Caricamento organigramma BAR e DPD
011500120427     C                   clear                   skPOBAR
011600120427     C                   clear                   skPODPD
011700120427     C                   clear                   skPODES
011800120427     C                   z-add     *zeros        jPO
011900120427     C     *loval        setll     azorg01l
012000120427     C                   read      azorg01l
012100120427     C                   dow       not %eof(azorg01l)
012200120427     C                   if        orgDE3 <> *blanks
012300120427     C*
012400120427     C                   add       1             jPO
012500120427     C*
012600120427     C                   movel     orgFIL        skPOBAR(jPO)
012700120427     C                   movel     orgDE3        skPODPD(jPO)
012800120427     C                   movel     orgDES        skPODES(jPO)
012900120427     C*
013000120427     C                   endif
013100111121     C*
013200120427     C                   read      azorg01l
013300120427     C                   enddo
013400120427     C*
013500111121     C                   ENDSR
013600991027
013700130710
013800130710
013900130710     C     chkRECFILMBR  BEGSR
014000130710     C*
014100130710     C                   READ      TIVAWWWT                               55
014200130710     C*
014300130710     C                   ENDSR
014400130710
014500030924
014600030924
014700991027     C     traduci       BEGSR
014800170712     C*
014900170712     C                   SETOFF                                       54
015000130710     C*
015100130710     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
015200130710     C                   IF        w§PPIINT = 'I' AND *IN55 = *ON
015300170712     C                   SETON                                        54
015400130710     C                   ENDIF
015500031103     C*
015600031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
015700031103     C* di traduzione (trattasi d "post-traduzione")
015800120427     C                   eval      wKstbKSU = w§PPIKSU
015900120427     C                   eval      wKstbFTR = *blanks
016000120427     C     KEYstb03P     setll     tistb03l
016100120427     C                   if        %equal(tistb03l)
016200120427     C     KEYstb03P     reade     tistb03l
016300991027     C*
016400031103     C* Ciclo x tuttii record del cliente da trasmettere
016500120427     C                   dow       not %eof(tistb03l)
016600080618     C*
016700120427     C                   seton                                        50
016800031103     C*
016900031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
017000031103     C* deve restituire al cliente nel file tradotto
017100120427     C                   if        stbFTR = *blanks                             * ridondante
017200110103     C*
017300120427     C                   setoff                                       50
017400031103     C*
017500031103     C* Innanzitutto chaino la bolla direttamente su TITAS
017600120427     C     KEYtas30P     chain     titas30c
017700120427     C                   if        %found(titas30c)
017800141119     C*
017900141119     C* Reperisco, se c'è, la firma del consegnatario
018000141119     C                   eval      ta4TRC = '1'
018100141119     C     KEYta430C     chain     tita430c
018200141119     C                   if        %found(tita430c)
018300141119     C                   eval      wFIRMATAR = ta4NOT
018400141119     C                   else
018500141119     C                   eval      wFIRMATAR = *blanks
018600141119     C                   endif
018700110103     C*
018800120427     C* Quindi aggancio l'estensione dettagli segnacolli
018900120427     C                   clear                   tahNOT
019000120427     C                   eval      tahTRC = 'C'
019100120427     C     KEYtah30P     chain     titah30c
019200110927     C*
019300120427     C                   z-add     *zeros        wPO               3 0
019400120427     C*
019500120427     C                   movel     *blanks       oParcelNbr      256
019600120427     C                   movel     *blanks       oScanEvent      256
019700130710     C                   movel     *blanks       oException      256
019800120427     C                   movel     *blanks       oScanEventDes   256
019900120427     C                   movel     *blanks       oScanDptCode    256
020000120427     C                   movel     *blanks       oScanDptName    256
020100120427     C                   movel     *blanks       oEventDatTim    256
020200130710     C                   movel     *blanks       oConsignName    256
020300130710     C                   movel     *blanks       oConsignStrt    256
020400130710     C                   movel     *blanks       oRoute          256
020500130710     C                   movel     *blanks       oTour           256
020600170607     C                   movel     '00'          oPcode          256
020700130710     C                   movel     *blanks       oService        256
020800120427     C                   movel     *blanks       oConsignCntry   256
020900120427     C                   movel     *blanks       oConsignZip     256
021000130710     C                   movel     *blanks       oConsignCity    256
021100120427     C                   movel     *blanks       oAddService01   256
021200120427     C                   movel     *blanks       oAddService02   256
021300120427     C                   movel     *blanks       oAddService03   256
021400120427     C                   movel     *blanks       oWeight         256
021500120620     C                   movel     *zeros        oSenderRefID     14
021600141119     C                   movel     *zeros        oBRTspedID       12
021700120427     C                   movel     *blanks       oPOD_Image      256
021800120427     C                   movel     *blanks       oReceivrName    256
021900130710     C                   movel     *blanks       oInfo_Text      256
022000130710     C                   movel     *blanks       oLocation       256
022100141119     C                   movel     *blanks       oFirmaCons      256
022200120427     C*
022300120427     C* Innanzitutto valorizzo i dati "correnti"
022400120427     C                   eval      oParcelNbr    = tahNOT
022500120427     C                   eval      oEventDatTim  = %trim(%editc(stbDAS:'X'))+
022600130712     C                                             %trim(%editc(stbORS:'X'))+
022700130712     C                                             '00'
022800120427     C                   eval      oConsignName  = tasRSD
022900120427     C                   eval      oConsignStrt  = tasIND
023000120427     C                   eval      oConsignCntry = '380'
023100120427     C                   eval      oConsignZip   = tasCAD
023200120427     C                   eval      oConsignCity  = tasLOD
023300170719     C                   eval      oWeight       = %editw(tasPKB:'     0 . ')
023400170720     C***                evalr     oSenderRefID  = %trim(%editc(tasRMN:'X'))
023500170720     C                   evalr     oSenderRefID  = %trim(stbRMA)
023600141119     C                   eval      oBRTspedID    = %editc(stbLNP:'X')+
023700141119     C                                             %editc(stbNRS:'X')+
023800141119     C                                             %editc(stbNSP:'X')
023900130710     C                   eval      oService      = '101'
024000141119     C                   eval      oFirmaCons    = %trim(wFIRMATAR)
024100120427     C*
024200120427     C                   select
024300120427     C*
024400120427     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
024500120427     C                   eval      oScanEvent = '01'
024600120427     C                   eval      wPO = tasLNP
024700120427     C*
024800120427     C                   when      %subst(stbCOS:1:9) = 'ARRPRICOL'
024900120427     C                   eval      oScanEvent = '02'
025000120427     C                   eval      wPO = tasLNA
025100120427     C*
025200120427     C                   when      %subst(stbCOS:1:4) = 'RESO'
025300120427     C                   eval      oScanEvent = '06'
025400120427     C                   eval      wPO = tasLNA
025500120427     C*
025600120620     C                   when      %trim(stbCOS) = 'CONSOK'
025700120427     C                   eval      oScanEvent = '13'
025800120427     C                   eval      wPO = tasLNA
025900120427     C*
026000120427     C                   other
026100120427     C                   movel     stbCOS        wCOS              3
026200120427     C                   z-add     1             jCAU
026300120427     C     wCOS          lookup    skCAUBAR(jCAU)                         55
026400120427     C                   if        %found and skCAUDPD(jCAU) <> *blanks
026500120427     C                   eval      oScanEvent    = %subst(skCAUDPD(jCAU):1:3)
026600120427     C                   eval      oException    = %subst(skCAUDPD(jCAU):4:6)
026700120427     C                   eval      oScanEventDes = skCAUDES(jCAU)
026800120427     C                   eval      wPO = tasLNA
026900120427     C                   else
027000120427     C                   seton                                        50
027100110927     C                   endif
027200120427     C*
027300120427     C                   endsl
027400120427     C*
027500120427     C                   clear                   OG143
027600120427     C                   z-add     1             jPO
027700120427     C     wPO           lookup    skPOBAR(jPO)                           55
027800120427     C                   if        %found
027900120427     C                   movel     skPODPD(jPO)  OG143
028000120427     C                   eval      oScanDptCode  = §OGDP1
028100120427     C                   eval      oScanDptName  = skPODES(jPO)
028200120427     C                   endif
028300110927     C*
028400120427     C                   else
028500120427     C                   seton                                        50
028600120427     C                   endif
028700120427     C                   endif
028800080618     C*
028900080618     C* Scarico il buffer d output
029000120427     C  N50              exsr      WRIREC
029100031103     C*
029200031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
029300120427     C                   eval      stbFTR = 'T'
029400120427     C                   update    tistb000
029500991027     C*
029600120427     C     KEYstb03P     reade     tistb03l
029700120427     C                   setoff                                       50
029800120427     C                   enddo
029900120427     C                   endif
030000991027     C*
030100120427     C                   eval      wrkesito = '0'
030200991027     C*
030300910830     C                   ENDSR
030400031103
030500031103
030600031103
030700110103     C     wrirec        BEGSR
030800110928     C*
030900120427     C* Considero solamente le bolle originali
031000111103     C                   movel     *blanks       wEsito1           1
031100111103     C*
031200111103     C* Chiamata metodo GetLblTyp
031300111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
031400111103     C                                                stbAAS
031500111103     C                                               :stbLNP
031600111103     C                                               :stbNRS
031700111103     C                                               :stbNSP
031800111103     C                                               :pOutLblTyp
031900111103     C                                               :pOutAnnoBO
032000111103     C                                               :pOutLineaParBO
032100111103     C                                               :pOutSerieBO
032200111103     C                                               :pOutNumSpedBO
032300111103     C                                               :pOutDcmBO
032400111103     C                                               :pOutCcaBO
032500111103     C                                               :pOutRblBO))
032600111103     C*
032700120427     C                   if        pOutLblTyp = 'O' and
032800120427     C                             not *IN50                                    * ridondante
032900141119     C*
033000141119     C* Forzatura su data/ora evento => se non pervenuta escludo il record
033100141204     C                   if        %trim(oEventDatTim) <> *zeros AND
033200141204     C                             %trim(oEventDatTim) <> *blanks
033300141119     C*
033400031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
033500120427     C                   eval      VAWDTA =
033600170725     C***                                 %trim(oParcelNbr)
033700170725     C                                    %trim(oBRTspedID)
033800170725     C***                                 %trim(oSenderRefID)
033900120427     C                               +';'+%trim(oScanEvent)
034000130710     C***                            +';'+%trim(oScanEventDes)
034100130710     C                               +';'+%trim(oScanDptCode)
034200120427     C                               +';'+%trim(oScanDptName)
034300120427     C                               +';'+%trim(oEventDatTim)
034400130710     C                               +';'+%trim(oRoute)
034500130710     C                               +';'+%trim(oTour)
034600130710     C                               +';'+%trim(oPcode)
034700130710     C                               +';'+%trim(oService)
034800130710     C***                            +';'+%trim(oConsignName)
034900130710     C***                            +';'+%trim(oConsignStrt)
035000120427     C                               +';'+%trim(oConsignCntry)
035100120427     C                               +';'+%trim(oConsignZip)
035200130710     C***                            +';'+%trim(oConsignCity)
035300141120     C***                            +';'+%trim(oAddService01)
035400141120     C                               +';'+%trim(oException)
035500120427     C                               +';'+%trim(oAddService02)
035600120427     C                               +';'+%trim(oAddService03)
035700120427     C                               +';'+%trim(oWeight)
035800170725     C                               +';'+%trim(oSenderRefID)
035900170725     C***                            +';'+%trim(oBRTspedID)
036000120427     C                               +';'+%trim(oPOD_Image)
036100141119     C***                            +';'+%trim(oReceivrName)
036200141119     C                               +';'+%trim(oFirmaCons)
036300130710     C                               +';'+%trim(oInfo_Text)
036400130710     C                               +';'+%trim(oLocation)
036500120427     C                               +';'
036600110927     C*
036700170712     C   54              exsr      wriHead
036800120427     C                   write     TIVAW000
036900120427     C                   endif
037000141119     C*
037100141119     C                   endif
037200170712     C*
037300170712     C                   ENDSR
037400170712
037500170712
037600170712
037700170712     C     wriHead       BEGSR
037800170712     C*
037900170712     C* Salvo il buffer corrente
038000170712     C                   EVAL      sVAWDTA = VAWDTA
038100170712     C*
038200170712     C* Scarico la riga di intestazione colonne
038300170712     C                   EVAL      VAWDTA =
038400170712     C                             'PARCELNO;SCAN_CODE;DEPOT_CODE;DEPOTNAME;'+
038500170712     C                             'EVENT_DATE_TIME;ROUTE;TOUR;PCODE;SERVICE;'+
038600170712     C                             'CONSIGNEE_COUNTRY_CODE;CONSIGNEE_ZIP;'+
038700170712     C                             'ADD_SERVICE_1;ADD_SERVICE_2;ADD_SERVICE_3;'+
038800170712     C                             'WEIGHT;CUSTOMER_REFERENCE;POD_IMAGE_REF;'+
038900170712     C                             'RECEIVER_NAME;INFO_TEXT;LOCATION;'
039000170712     C                   WRITE     TIVAW000
039100170712     C                   SETOFF                                       54
039200170712     C*
039300170712     C* Ripristino il buffer corrente
039400170712     C                   EVAL      VAWDTA = sVAWDTA
039500031103     C*
039600031103     C                   ENDSR
039700031103
039800031103
039900991027
040000991027      /TITLE Operazioni iniziali.
040100991027     C     *inzsr        BEGSR
040200991027     C*
040300991027     C     *ENTRY        PLIST
040400031103     C                   parm                    prmppi
040500991027     C     wrkesito      parm      wrkesito      prmesito
040600031103     C*
040700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
040800031103     C                   EVAL      DSPPI = prmppi
040900030709     C*
041000030709     C* Definizione chiavi
041100110927     C*
041200110927     C* Chiave su TABEL00F - Parziale
041300110927     C     KEYtab00P     KLIST
041400110927     C                   KFLD                    tblKUT
041500110927     C                   KFLD                    tblCOD
041600120427     C*
041700120427     C* Chiave su TNTBE00F - Parziale
041800120427     C     KEYtbe11P     KLIST
041900120427     C                   KFLD                    tbeLIN
042000120427     C                   KFLD                    tbeCOD
042100120427     C                   KFLD                    tbeKE1
042200030924     C*
042300110103     C* Chiave su TISTB03L - Parziale
042400110103     C     KEYstb03P     KLIST
042500031104     C                   KFLD                    wKstbKSU
042600031103     C                   KFLD                    wKstbFTR
042700031103     C*
042800031103     C* Chiave su TITAS30C - Parziale
042900031103     C     KEYtas30P     KLIST
043000031103     C                   KFLD                    stbAAS
043100031103     C                   KFLD                    stbLNP
043200031103     C                   KFLD                    stbNRS
043300031103     C                   KFLD                    stbNSP
043400120427     C*
043500120427     C* Chiave su TITAH30C - Parziale
043600120427     C     KEYtah30P     KLIST
043700120427     C                   KFLD                    stbAAS
043800120427     C                   KFLD                    stbLNP
043900120427     C                   KFLD                    stbNRS
044000120427     C                   KFLD                    stbNSP
044100120427     C                   KFLD                    tahTRC
044200141119     C*
044300141119     C* Chiave su TITA430C - Completa
044400141119     C     KEYta430C     KLIST
044500141119     C                   KFLD                    stbAAS
044600141119     C                   KFLD                    stbLNP
044700141119     C                   KFLD                    stbNRS
044800141119     C                   KFLD                    stbNSP
044900141119     C                   KFLD                    ta4TRC
045000991027     C*
045100991027     C                   ENDSR
