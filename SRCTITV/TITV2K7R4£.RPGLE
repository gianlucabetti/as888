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
001100170818     FTISTBD2I  UF   E           K DISK    commit
001200090323     FTIVAWWWT  UF A E             DISK    commit
001300031103
001400031103     D*-----------
001500120427     D* SCHIERE
001600031103     D*-----------
001700120427     D skCAUBAR        S              3    DIM(999)
001800120427     D skCAUDPD        S              9    DIM(999)
001900120427     D skCAUDES        S             30    DIM(999)
002000120427     D jCAU            S              3  0 INZ(*zeros)
002100120427     D skPOBAR         S              3  0 DIM(999)
002200120427     D skPODPD         S             25    DIM(999)
002300120427     D skPODES         S             20    DIM(999)
002400120427     D jPO             S              3  0 INZ(*zeros)
002500120427
002600030924
002700041015     D*-----------
002800041015     D* VARIABILI D WRK
002900041015     D*-----------
003000031103     D prmppi          S             50
003100030924     D prmesito        S              1
003200030924     D wrkesito        S                   like(prmesito)
003300141119     D wFIRMATAR       S                   inz(*blanks) like(ta4NOT)
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
005800170818     D  w§PPICOLLI                    1
005900031103
006000110928
006100110928     D*------------------
006200110928     D* LINKING A DEFINIZIONI ESTERNE
006300110928     D*------------------
006400110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
006500110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
006600110928
006700030924
006800030325
006900030924     C* MAIN/
007000041015     C*
007100041015     C* Carica i dati tabellati
007200041015     C                   EXSR      cartab
007300130710     C*
007400130710     C* Verifico se già presenti dati nel file d output
007500130710     C                   EXSR      chkRECFILMBR
007600041015     C*
007700041015     C* Esegue traduzione
007800991027     C                   EXSR      traduci
007900110929     C*
008000921023     C                   SETON                                        LR
008100111121
008200111121
008300111121
008400111121     C     cartab        BEGSR
008500111121     C*
008600120427     C* Carico causali evento 2Z e descrizioni in lingua (ICE)
008700120427     C                   clear                   skCAUBAR
008800120427     C                   clear                   skCAUDPD
008900120427     C                   clear                   skCAUDES
009000120427     C                   z-add     *zeros        jCAU
009100111121     C                   eval      tblKUT = 1
009200120427     C                   eval      tblCOD = '2Z'
009300111121     C     KEYtab00P     setll     tabel00f
009400111121     C     KEYtab00P     reade     tabel00f
009500111121     C                   dow       not %eof(tabel00f)
009600120427     C                   add       1             jCAU
009700120427     C                   movel(p)  tblUNI        DS2Z
009800120427     C                   eval      skCAUBAR(jCAU) = tblKEY
009900120427     C                   eval      skCAUDPD(jCAU) = §2ZEVD+§2ZCED
010000120427     C*
010100120427     C                   eval      tbeLIN = w§PPILIN
010200120427     C                   eval      tbeCOD = 'ICE'
010300120427     C                   eval      tbeKE1 = tblKEY
010400120427     C     KEYtbe11P     chain     tntbe11l
010500120427     C                   if        %found(tntbe11l)
010600120427     C                   eval      DICE = tbeUNI
010700120427     C                   eval      skCAUDES(jCAU) = §ICEDEP
010800120427     C                   else
010900120427     C                   eval      skCAUDES(jCAU) = '-'
011000120427     C                   endif
011100120427     C*
011200111121     C     KEYtab00P     reade     tabel00f
011300111121     C                   enddo
011400120427     C*
011500120427     C* Caricamento organigramma BAR e DPD
011600120427     C                   clear                   skPOBAR
011700120427     C                   clear                   skPODPD
011800120427     C                   clear                   skPODES
011900120427     C                   z-add     *zeros        jPO
012000120427     C     *loval        setll     azorg01l
012100120427     C                   read      azorg01l
012200120427     C                   dow       not %eof(azorg01l)
012300120427     C                   if        orgDE3 <> *blanks
012400120427     C*
012500120427     C                   add       1             jPO
012600120427     C*
012700120427     C                   movel     orgFIL        skPOBAR(jPO)
012800120427     C                   movel     orgDE3        skPODPD(jPO)
012900120427     C                   movel     orgDES        skPODES(jPO)
013000120427     C*
013100120427     C                   endif
013200111121     C*
013300120427     C                   read      azorg01l
013400120427     C                   enddo
013500120427     C*
013600111121     C                   ENDSR
013700991027
013800130710
013900130710
014000130710     C     chkRECFILMBR  BEGSR
014100130710     C*
014200130710     C                   READ      TIVAWWWT                               55
014300130710     C*
014400130710     C                   ENDSR
014500130710
014600030924
014700030924
014800991027     C     traduci       BEGSR
014900130710     C*
015000130710     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
015100130710     C                   IF        w§PPIINT = 'I' AND *IN55 = *ON
015200130710     C                   EVAL      vawDTA =
015300130710     C                             'PARCELNO;SCAN_CODE;DEPOT_CODE;DEPOTNAME;'+
015400130710     C                             'EVENT_DATE_TIME;ROUTE;TOUR;PCODE;SERVICE;'+
015500130710     C                             'CONSIGNEE_COUNTRY_CODE;CONSIGNEE_ZIP;'+
015600130710     C                             'ADD_SERVICE_1;ADD_SERVICE_2;ADD_SERVICE_3;'+
015700130710     C                             'WEIGHT;CUSTOMER_REFERENCE;POD_IMAGE_REF;'+
015800130710     C                             'RECEIVER_NAME;INFO_TEXT;LOCATION;'
015900130710     C                   WRITE     TIVAW000
016000130710     C                   ENDIF
016100031103     C*
016200031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
016300031103     C* di traduzione (trattasi d "post-traduzione")
016400120427     C                   eval      wKstbKSU = w§PPIKSU
016500120427     C                   eval      wKstbFTR = *blanks
016600120427     C     KEYstb03P     setll     tistb03l
016700120427     C                   if        %equal(tistb03l)
016800120427     C     KEYstb03P     reade     tistb03l
016900991027     C*
017000031103     C* Ciclo x tuttii record del cliente da trasmettere
017100120427     C                   dow       not %eof(tistb03l)
017200080618     C*
017300120427     C                   seton                                        50
017400031103     C*
017500031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
017600031103     C* deve restituire al cliente nel file tradotto
017700120427     C                   if        stbFTR = *blanks                             * ridondante
017800170818     C*
017900170818     C* Ignoro completamente STATI di tipo '9' con provenineza '4'
018000170818     C                   if        stbTIS = '9' and
018100170818     C                             stbPRS = '4'
018200170818     C                   else
018300110103     C*
018400120427     C                   setoff                                       50
018500031103     C*
018600031103     C* Innanzitutto chaino la bolla direttamente su TITAS
018700120427     C     KEYtas30P     chain     titas30c
018800120427     C                   if        %found(titas30c)
018900141119     C*
019000141119     C* Reperisco, se c'è, la firma del consegnatario
019100141119     C                   eval      ta4TRC = '1'
019200141119     C     KEYta430C     chain     tita430c
019300141119     C                   if        %found(tita430c)
019400141119     C                   eval      wFIRMATAR = ta4NOT
019500141119     C                   else
019600141119     C                   eval      wFIRMATAR = *blanks
019700141119     C                   endif
019800110103     C*
019900120427     C* Quindi aggancio l'estensione dettagli segnacolli
020000120427     C                   clear                   tahNOT
020100120427     C                   eval      tahTRC = 'C'
020200120427     C     KEYtah30P     chain     titah30c
020300110927     C*
020400120427     C                   z-add     *zeros        wPO               3 0
020500120427     C*
020600120427     C                   movel     *blanks       oParcelNbr      256
020700120427     C                   movel     *blanks       oScanEvent      256
020800130710     C                   movel     *blanks       oException      256
020900120427     C                   movel     *blanks       oScanEventDes   256
021000120427     C                   movel     *blanks       oScanDptCode    256
021100120427     C                   movel     *blanks       oScanDptName    256
021200120427     C                   movel     *blanks       oEventDatTim    256
021300130710     C                   movel     *blanks       oConsignName    256
021400130710     C                   movel     *blanks       oConsignStrt    256
021500130710     C                   movel     *blanks       oRoute          256
021600130710     C                   movel     *blanks       oTour           256
021700130710     C                   movel     *blanks       oPcode          256
021800130710     C                   movel     *blanks       oService        256
021900120427     C                   movel     *blanks       oConsignCntry   256
022000120427     C                   movel     *blanks       oConsignZip     256
022100130710     C                   movel     *blanks       oConsignCity    256
022200120427     C                   movel     *blanks       oAddService01   256
022300120427     C                   movel     *blanks       oAddService02   256
022400120427     C                   movel     *blanks       oAddService03   256
022500120427     C                   movel     *blanks       oWeight         256
022600120620     C                   movel     *zeros        oSenderRefID     14
022700141119     C                   movel     *zeros        oBRTspedID       12
022800120427     C                   movel     *blanks       oPOD_Image      256
022900120427     C                   movel     *blanks       oReceivrName    256
023000130710     C                   movel     *blanks       oInfo_Text      256
023100130710     C                   movel     *blanks       oLocation       256
023200141119     C                   movel     *blanks       oFirmaCons      256
023300120427     C*
023400120427     C* Innanzitutto valorizzo i dati "correnti"
023500170818     C***                eval      oParcelNbr    = tahNOT
023600120427     C                   eval      oEventDatTim  = %trim(%editc(stbDAS:'X'))+
023700130712     C                                             %trim(%editc(stbORS:'X'))+
023800130712     C                                             '00'
023900120427     C                   eval      oConsignName  = tasRSD
024000120427     C                   eval      oConsignStrt  = tasIND
024100120427     C                   eval      oConsignCntry = '380'
024200120427     C                   eval      oConsignZip   = tasCAD
024300120427     C                   eval      oConsignCity  = tasLOD
024400120427     C                   eval      oWeight       = %editw(tasPKB:'     0 , ')
024500170818     C                   evalr     oSenderRefID  = %trim(%editc(tasRMN:'X'))
024600170818     C                   eval      oBRTspedID    = %editc(stbLNP:'X')+
024700141119     C                                             %editc(stbNRS:'X')+
024800141119     C                                             %editc(stbNSP:'X')
024900130710     C                   eval      oService      = '101'
025000141119     C                   eval      oFirmaCons    = %trim(wFIRMATAR)
025100120427     C*
025200170824     C                   setoff                                       80
025300120427     C                   select
025400120427     C*
025500120427     C                   when      %subst(stbCOS:1:8) = 'INCARICO'
025600120427     C                   eval      oScanEvent = '01'
025700120427     C                   eval      wPO = tasLNP
025800120427     C*
025900120427     C                   when      %subst(stbCOS:1:9) = 'ARRPRICOL'
026000170818     C                   eval      oScanEvent = '02'
026100120427     C                   eval      wPO = tasLNA
026200120427     C*
026300120427     C                   when      %subst(stbCOS:1:4) = 'RESO'
026400120427     C                   eval      oScanEvent = '06'
026500120427     C                   eval      wPO = tasLNA
026600120427     C*
026700120620     C                   when      %trim(stbCOS) = 'CONSOK'
026800120427     C                   eval      oScanEvent = '13'
026900120427     C                   eval      wPO = tasLNA
027000170818     C*
027100170824     C                   when      %trim(stbCOS) = 'SPUNTNPG5' and *in80 and
027200170818     C                                             w§PPICOLLI = 'S'
027300170818     C                   eval      oScanEvent = '99'
027400170818     C                   eval      wPO = tasLNP
027500170818     C                   setoff                                       50
027600170818     C     KEYstbd2_P    setll     tistbd2i
027700170818     C                   if        %equal(tistbd2i)
027800170818     C     KEYstbd2_P    reade     tistbd2i
027900170818     C*
028000170818     C* Ciclo x tutti i record colli cliente
028100170818     C                   dow       not %eof(tistbd2i)
028200170818     C                   if        stbdCOS = 'SPUNTNPG5' and
028300170818     C                             stbdDAD > *zeros      and
028400170818     C                             %len(%trim(stbdDTA)) >= 22
028500170818     C                   eval      oParcelNbr = %subst(stbdDTA:9:14)
028600170818     C*
028700170818     C* Scarico il buffer d output per ogni collo con spunta fisica partenza
028800170818     C                   exsr      WRIREC
028900170818     C                   endif
029000170818     C     KEYstbd2_P    reade     tistbd2i
029100170818     C                   enddo
029200170818     C                   endif
029300170818     C                   seton                                        50
029400120427     C*
029500120427     C                   other
029600120427     C                   movel     stbCOS        wCOS              3
029700120427     C                   z-add     1             jCAU
029800120427     C     wCOS          lookup    skCAUBAR(jCAU)                         55
029900120427     C                   if        %found and skCAUDPD(jCAU) <> *blanks
030000120427     C                   eval      oScanEvent    = %subst(skCAUDPD(jCAU):1:3)
030100120427     C                   eval      oException    = %subst(skCAUDPD(jCAU):4:6)
030200120427     C                   eval      oScanEventDes = skCAUDES(jCAU)
030300120427     C                   eval      wPO = tasLNA
030400120427     C                   else
030500120427     C                   seton                                        50
030600110927     C                   endif
030700120427     C*
030800120427     C                   endsl
030900110927     C*
031000120427     C                   else
031100120427     C                   seton                                        50
031200120427     C                   endif
031300120427     C                   endif
031400170818     C                   endif
031500080618     C*
031600080618     C* Scarico il buffer d output
031700120427     C  N50              exsr      WRIREC
031800031103     C*
031900031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
032000120427     C                   eval      stbFTR = 'T'
032100120427     C                   update    tistb000
032200991027     C*
032300120427     C     KEYstb03P     reade     tistb03l
032400120427     C                   setoff                                       50
032500120427     C                   enddo
032600120427     C                   endif
032700991027     C*
032800120427     C                   eval      wrkesito = '0'
032900991027     C*
033000910830     C                   ENDSR
033100031103
033200031103
033300031103
033400110103     C     wrirec        BEGSR
033500110928     C*
033600120427     C* Considero solamente le bolle originali
033700111103     C                   movel     *blanks       wEsito1           1
033800111103     C*
033900111103     C* Chiamata metodo GetLblTyp
034000111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
034100111103     C                                                stbAAS
034200111103     C                                               :stbLNP
034300111103     C                                               :stbNRS
034400111103     C                                               :stbNSP
034500111103     C                                               :pOutLblTyp
034600111103     C                                               :pOutAnnoBO
034700111103     C                                               :pOutLineaParBO
034800111103     C                                               :pOutSerieBO
034900111103     C                                               :pOutNumSpedBO
035000111103     C                                               :pOutDcmBO
035100111103     C                                               :pOutCcaBO
035200111103     C                                               :pOutRblBO))
035300111103     C*
035400120427     C                   if        pOutLblTyp = 'O' and
035500120427     C                             not *IN50                                    * ridondante
035600141119     C*
035700141119     C* Forzatura su data/ora evento => se non pervenuta escludo il record
035800141204     C                   if        %trim(oEventDatTim) <> *zeros AND
035900141204     C                             %trim(oEventDatTim) <> *blanks
036000170818     C*
036100170818     C                   clear                   OG143
036200170818     C                   z-add     1             jPO
036300170818     C     wPO           lookup    skPOBAR(jPO)                           55
036400170818     C                   if        %found
036500170818     C                   movel     skPODPD(jPO)  OG143
036600170818     C                   eval      oScanDptCode  = §OGDP1
036700170818     C                   eval      oScanDptName  = skPODES(jPO)
036800170818     C                   endif
036900141119     C*
037000031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
037100120427     C                   eval      VAWDTA =
037200170824     C***                                 %trim(oParcelNbr)
037300170824     C                                    %trim(oSenderRefID)
037400120427     C                               +';'+%trim(oScanEvent)
037500130710     C***                            +';'+%trim(oScanEventDes)
037600130710     C                               +';'+%trim(oScanDptCode)
037700120427     C                               +';'+%trim(oScanDptName)
037800120427     C                               +';'+%trim(oEventDatTim)
037900130710     C                               +';'+%trim(oRoute)
038000130710     C                               +';'+%trim(oTour)
038100130710     C                               +';'+%trim(oPcode)
038200130710     C                               +';'+%trim(oService)
038300130710     C***                            +';'+%trim(oConsignName)
038400130710     C***                            +';'+%trim(oConsignStrt)
038500120427     C                               +';'+%trim(oConsignCntry)
038600120427     C                               +';'+%trim(oConsignZip)
038700130710     C***                            +';'+%trim(oConsignCity)
038800141120     C***                            +';'+%trim(oAddService01)
038900141120     C                               +';'+%trim(oException)
039000120427     C                               +';'+%trim(oAddService02)
039100120427     C                               +';'+%trim(oAddService03)
039200120427     C                               +';'+%trim(oWeight)
039300170824     C                               +';'+%trim(oSenderRefID)
039400170818     C***                            +';'+%trim(oBRTspedID)
039500170824     C***                            +';'+%trim(oParcelNbr)
039600120427     C                               +';'+%trim(oPOD_Image)
039700141119     C***                            +';'+%trim(oReceivrName)
039800141119     C                               +';'+%trim(oFirmaCons)
039900130710     C                               +';'+%trim(oInfo_Text)
040000130710     C                               +';'+%trim(oLocation)
040100120427     C                               +';'
040200110927     C*
040300120427     C                   write     TIVAW000
040400120427     C                   endif
040500141119     C*
040600141119     C                   endif
040700031103     C*
040800031103     C                   ENDSR
040900031103
041000031103
041100991027
041200991027      /TITLE Operazioni iniziali.
041300991027     C     *inzsr        BEGSR
041400991027     C*
041500991027     C     *ENTRY        PLIST
041600031103     C                   parm                    prmppi
041700991027     C     wrkesito      parm      wrkesito      prmesito
041800031103     C*
041900031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
042000031103     C                   EVAL      DSPPI = prmppi
042100030709     C*
042200030709     C* Definizione chiavi
042300110927     C*
042400110927     C* Chiave su TABEL00F - Parziale
042500110927     C     KEYtab00P     KLIST
042600110927     C                   KFLD                    tblKUT
042700110927     C                   KFLD                    tblCOD
042800120427     C*
042900120427     C* Chiave su TNTBE00F - Parziale
043000120427     C     KEYtbe11P     KLIST
043100120427     C                   KFLD                    tbeLIN
043200120427     C                   KFLD                    tbeCOD
043300120427     C                   KFLD                    tbeKE1
043400030924     C*
043500110103     C* Chiave su TISTB03L - Parziale
043600110103     C     KEYstb03P     KLIST
043700031104     C                   KFLD                    wKstbKSU
043800031103     C                   KFLD                    wKstbFTR
043900031103     C*
044000031103     C* Chiave su TITAS30C - Parziale
044100031103     C     KEYtas30P     KLIST
044200031103     C                   KFLD                    stbAAS
044300031103     C                   KFLD                    stbLNP
044400031103     C                   KFLD                    stbNRS
044500031103     C                   KFLD                    stbNSP
044600120427     C*
044700120427     C* Chiave su TITAH30C - Parziale
044800120427     C     KEYtah30P     KLIST
044900120427     C                   KFLD                    stbAAS
045000120427     C                   KFLD                    stbLNP
045100120427     C                   KFLD                    stbNRS
045200120427     C                   KFLD                    stbNSP
045300120427     C                   KFLD                    tahTRC
045400141119     C*
045500141119     C* Chiave su TITA430C - Completa
045600141119     C     KEYta430C     KLIST
045700141119     C                   KFLD                    stbAAS
045800141119     C                   KFLD                    stbLNP
045900141119     C                   KFLD                    stbNRS
046000141119     C                   KFLD                    stbNSP
046100141119     C                   KFLD                    ta4TRC
046200170818     C*
046300170818     C* Chiave su TISTBD2I - Parziale
046400170818     C     KEYstbd2_P    KLIST
046500170818     C                   KFLD                    stbKSU
046600170818     C                   KFLD                    stbAAS
046700170818     C                   KFLD                    stbLNP
046800170818     C                   KFLD                    stbNRS
046900170818     C                   KFLD                    stbNSP
047000991027     C*
047100991027     C                   ENDSR
