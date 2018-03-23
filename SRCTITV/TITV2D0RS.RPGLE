000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110131     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500070426     FTITAS30C  IF   E           K DISK
000600090326     FFNVACXLS  uF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100030325
001200030331     D dsvac         e ds                  extname(fnvac00t)
001300051007     D wIAS            s             14
001400051007     D wPKB            s              8
001500051007     D wVLB            s              6
001600051007     D wQFT            s             14
001700051007     D wCAS            s             14
001800110131
001900110131     D*------------------
002000110131     D* LINKING A DEFINIZIONI ESTERNE
002100110131     D*------------------
002200110131     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002300110131     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002400991027
002500030908     C                   EXSR      chkRECFILMBR
002600991027     C                   EXSR      traduci
002700921023     C                   SETON                                        LR
002800030908
002900030908     C     chkRECFILMBR  BEGSR
003000030908     C*
003100030908     C                   READ      FNVACXLS                               55
003200030908     C*
003300030908     C                   ENDSR
003400991027
003500991027     C     traduci       BEGSR
003600051007     C*
003700051007     C* Verifico se richiesto un particolare carattere d separatore campi
003800051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003900100409     C                   EVAL      SepFld = %subst(prmppt:3:1)
004000051007     C                   ELSE
004100100409     C                   MOVEL     ','           SepFld            1
004200051007     C                   ENDIF
004300051007     C*
004400051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004500051007     C                   SETON                                        60
004600051007     C                   IF        %subst(prmppt:4:1) = ','
004700051007     C                   SETOFF                                       60
004800051007     C                   ENDIF
004900030325     C*
005000030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005100030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005200100409     C                   EVAL      VACXDTA =
005300100409     C                             '"VACAAS"'                           +SepFld+
005400100409     C                             '"VACLNP"'                           +SepFld+
005500100409     C                             '"VACNRS"'                           +SepFld+
005600100409     C                             '"VACNSP"'                           +SepFld+
005700100409     C                             '"VACMGS"'                           +SepFld+
005800100409     C                             '"VACCBO"'                           +SepFld+
005900100409     C                             '"VACLNA"'                           +SepFld+
006000100409     C                             '"VACRSD"'                           +SepFld+
006100100409     C                             '"VACPRD"'                           +SepFld+
006200100409     C                             '"VACGC1"'                           +SepFld+
006300100409     C                             '"VACGC2"'                           +SepFld+
006400100409     C                             '"VACCTR"'                           +SepFld+
006500100409     C                             '"VACCTS"'                           +SepFld+
006600100409     C                             '"VACFTM"'                           +SepFld+
006700100409     C                             '"VACFIN"'                           +SepFld+
006800100409     C                             '"VACFAP"'                           +SepFld+
006900100409     C                             '"VACTSP"'                           +SepFld+
007000100409     C                             '"VACIAS"'                           +SepFld+
007100100409     C                             '"VACVAS"'                           +SepFld+
007200100409     C                             '"VACNAS"'                           +SepFld+
007300100409     C                             '"VACNCL"'                           +SepFld+
007400100409     C                             '"VACPKB"'                           +SepFld+
007500100409     C                             '"VACVLB"'                           +SepFld+
007600100409     C                             '"VACQFT"'                           +SepFld+
007700100409     C                             '"VACTIC"'                           +SepFld+
007800100409     C                             '"VACCAS"'                           +SepFld+
007900100409     C                             '"VACVCA"'                           +SepFld+
008000100409     C                             '"VACCCM"'                           +SepFld+
008100100409     C                             '"VACRMN"'                           +SepFld+
008200100409     C                             '"VACRMA"'                           +SepFld+
008300100409     C                             '"VACRMO"'                           +SepFld+
008400100409     C                             '"VACFFD"'                           +SepFld+
008500100409     C                             '"VACTCR"'                           +SepFld+
008600100409     C                             '"VACDCR"'                           +SepFld+
008700100409     C                             '"VACHCR"'                           +SepFld+
008800100409     C                             '"VACDCM"'                           +SepFld+
008900100409     C                             '"VACHMC"'                           +SepFld+
009000100409     C                             '"VACTC1"'                           +SepFld+
009100100409     C                             '"VACTC2"'                           +SepFld+
009200100409     C                             '"VACCCA"'                           +SepFld+
009300100409     C                             '"VACDLA"'                           +SepFld+
009400100409     C                             '"VACDAG"'
009500100409     C*
009600100409     C                   IF        %subst(prmppt:5:1) = 'I'
009700100409     C                   EVAL      VACXDTA = %trim(VACXDTA)             +SepFld+
009800100409     C                             '"VACIND"'                           +SepFld+
009900100409     C                             '"VACCAD"'                           +SepFld+
010000100409     C                             '"VACLOD"'
010100100409     C                   ENDIF
010200100409     C*
010300100409     C                   WRITE     FNVACX00
010400100409     C                   ENDIF
010500100409     C*
010600100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010700100409     C* (descrizione campi)
010800100409     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
010900100409     C                   EVAL      VACXDTA =
011000100409     C                             '"ANNO SPEDIZIONE"'                  +SepFld+
011100100409     C                             '"FILIALE PARTENZA"'                 +SepFld+
011200100409     C                             '"NUMERO SERIE"'                     +SepFld+
011300100409     C                             '"NUMERO SPEDIZIONE"'                +SepFld+
011400100409     C                             '"MESE/GIORNO SPEDIZIONE"'           +SepFld+
011500100409     C                             '"CODICE BOLLA"'                     +SepFld+
011600100409     C                             '"FILIALE ARRIVO"'                   +SepFld+
011700100409     C                             '"RAGIONE SOCIALE DESTINATARIO"'     +SepFld+
011800100409     C                             '"PROVINCIA DESTINATARIO"'           +SepFld+
011900100409     C                             '"1° GG.CHIUSURA DESTINATARIO"'      +SepFld+
012000100409     C                             '"2° GG.CHIUSURA DESTINATARIO"'      +SepFld+
012100100409     C                             '"CODICE TARIFFA"'                   +SepFld+
012200100409     C                             '"CODICE TASSAZIONE"'                +SepFld+
012300100409     C                             '"FLAG TASSAZ C/TARIFFA MITTENTE"'   +SepFld+
012400100409     C                             '"FLAG INOLTRO"'                     +SepFld+
012500100409     C                             '"FLAG ANTEPORTO"'                   +SepFld+
012600100409     C                             '"TIPO SERVIZIO BOLLE"'              +SepFld+
012700100409     C                             '"IMPORTO DA ASSICURARE"'            +SepFld+
012800100409     C                             '"DIVISA IMPORTO DA ASSICURARE"'     +SepFld+
012900100409     C                             '"NATURA MERCE MITTENTE"'            +SepFld+
013000100409     C                             '"NUMERO COLLI"'                     +SepFld+
013100100409     C                             '"PESO KG BOLLETTATO"'               +SepFld+
013200100409     C                             '"VOLUME BOLLETTATO"'                +SepFld+
013300100409     C                             '"QUANTITA DA FATTURARE"'            +SepFld+
013400100409     C                             '"TIPO INCASSO CONTRASSEGNA"'        +SepFld+
013500100409     C                             '"CONTRASSEGNO"'                     +SepFld+
013600100409     C                             '"DIVISA IMPORTO C/ASSEGNO"'         +SepFld+
013700100409     C                             '"COD.CLIENTE MITTENTE"'             +SepFld+
013800100409     C                             '"RIFERIMENTO MITTENTE NUMERICO"'    +SepFld+
013900100409     C                             '"RIFERIMENTO MITTENTE ALFABETICO"'  +SepFld+
014000100409     C                             '"RAG.SOC.MITTENTE ORIGINALE"'       +SepFld+
014100100409     C                             '"S=FERMO DEPOSITO"'                 +SepFld+
014200100409     C                             '"TIPO DT.CONSEGNA:IL/PRIMA/DOPO"'   +SepFld+
014300100409     C                             '"DATA CONSEGNA RICHIESTA"'          +SepFld+
014400100409     C                             '"ORA CONSEGNA RICHIESTA"'           +SepFld+
014500100409     C                             '"DATA CONSEGNA MERCE REALE"'        +SepFld+
014600100409     C                             '"ORA CONSEGNA REALE (HHMM)"'        +SepFld+
014700100409     C                             '"1 CONSEGNA PARTICOLARE"'           +SepFld+
014800100409     C                             '"2 CONSEGNA PARTICOLARE"'           +SepFld+
014900100409     C                             '"CODICE CONSEGNA ANOMALA"'          +SepFld+
015000100409     C                             '"DATA LASCIATO AVVISO"'             +SepFld+
015100100409     C                             '"DATA APERTURA GIACENZA"'
015200100409     C*
015300100409     C                   IF        %subst(prmppt:5:1) = 'I'
015400100409     C                   EVAL      VACXDTA = %trim(VACXDTA)             +SepFld+
015500100409     C                             '"INDIRIZZO DESTINATARIO"'           +SepFld+
015600100409     C                             '"CAP DESTINATARIO"'                 +SepFld+
015700100409     C                             '"LOCALITA DESTINATARIO"'
015800100409     C                   ENDIF
015900030325     C*
016000030331     C                   WRITE     FNVACX00
016100030325     C                   ENDIF
016200100409     C*
016300100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
016400100409     C* (descrizione campi in inglese)
016500100409     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
016600100409     C                   EVAL      VACXDTA =
016700100409     C                             '"SHIPMENT YEAR"'                    +SepFld+
016800100409     C                             '"OPERATIVE DEPARTURE DEPOT"'        +SepFld+
016900100409     C                             '"SERIAL NUMBER"'                    +SepFld+
017000100409     C                             '"SHIPMENT NUMBER"'                  +SepFld+
017100100409     C                             '"SHIPMENT MONTH/DAY"'               +SepFld+
017200100409     C                             '"NOTE CODE"'                        +SepFld+
017300100409     C                             '"OPERATIVE DESTINATION DEPOT"'      +SepFld+
017400100409     C                             '"DESTINATION NAME"'                 +SepFld+
017500100409     C                             '"DESTINATION PROVINCE"'             +SepFld+
017600100409     C                             '"1ST DESTINATION CLOSURE DAY"'      +SepFld+
017700100409     C                             '"2ND DESTINATION CLOSURE DAY"'      +SepFld+
017800100409     C                             '"TARIFF CODE"'                      +SepFld+
017900100409     C                             '"TAXING CODE"'                      +SepFld+
018000100409     C                             '"SENDER TARIFF/ TAXING CODE FLAG"'  +SepFld+
018100100409     C                             '"COLLECTION PLACE"'                 +SepFld+
018200100409     C                             '"DELIVERY PLACE"'                   +SepFld+
018300100409     C                             '"DATA ENTRY TYPE"'                  +SepFld+
018400100409     C                             '"SUM TO INSURE"'                    +SepFld+
018500100409     C                             '"CURRENCY OF SUM TO INSURE"'        +SepFld+
018600100409     C                             '"SENDER GOODS TYPE"'                +SepFld+
018700100409     C                             '"NUMBER OF PACKS"'                  +SepFld+
018800100409     C                             '"WEIGHT KG LABELLED"'               +SepFld+
018900100409     C                             '"VOLUME LABELLED"'                  +SepFld+
019000100409     C                             '"QUANTITY TO INVOICE"'              +SepFld+
019100100409     C                             '"TYPE OF C.O.D. PAYMENT"'           +SepFld+
019200100409     C                             '"C.O.D."'                           +SepFld+
019300100409     C                             '"C.O.D.SUM CURRENCY"'               +SepFld+
019400100409     C                             '"SENDER CLIENT CODE"'               +SepFld+
019500100409     C                             '"NUMERICAL SENDER REFERENCE"'       +SepFld+
019600100409     C                             '"ALPHABETICAL SENDER REFERENCE"'    +SepFld+
019700100409     C                             '"ORIGINAL SENDER NAME"'             +SepFld+
019800100409     C                             '"S=TO BE HOLD AT DEPOT"'            +SepFld+
019900100409     C                             '"DELIVERY TYPE: AT/BEFORE/AFTER"'   +SepFld+
020000100409     C                             '"REQUESTED DELIVERY DATE"'          +SepFld+
020100100409     C                             '"REQUESTED DELIVERY TIME"'          +SepFld+
020200100409     C                             '"EFFECTIVE GOODS DELIVERY DATE"'    +SepFld+
020300100409     C                             '"EFFECTIVE GOODS DELIVERY TIME"'    +SepFld+
020400100409     C                             '"1ST SPECIAL DELIVERY"'             +SepFld+
020500100409     C                             '"2ND SPECIAL DELIVERY"'             +SepFld+
020600100409     C                             '"FAULTY DELIVERY CODE"'             +SepFld+
020700100409     C                             '"DATE OF NOTIFICATION"'             +SepFld+
020800100409     C                             '"STOCK OPENING DATE"'
020900100409     C*
021000100409     C                   IF        %subst(prmppt:5:1) = 'I'
021100100409     C                   EVAL      VACXDTA = %trim(VACXDTA)             +SepFld+
021200100409     C                             '"RECIPIENT ADDRESS"'                +SepFld+
021300100409     C                             '"RECIPIENT POSTAL CODE"'            +SepFld+
021400100409     C                             '"RECIPIENT AREA"'
021500100409     C                   ENDIF
021600100409     C*
021700100409     C                   WRITE     FNVACX00
021800100409     C                   ENDIF
021900991027     C*
022000080924     C                   READ      tivgdtmp
022100080924     C                   DOW       not %eof(tivgdtmp)
022200080924     c                   movel     vgddta        dsvac
022300991027     C*
022400030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
022500060411     C***  '"':''''      XLATE     dsvac         dsvac
022600110131     C*
022700110131     C                   SETOFF                                       3132
022800110131     C*
022900110131     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
023000110131     C                   if        %subst(prmppt:7:1) = 'S'
023100110131     C                   if        VACNRS = *zeros
023200110131     C                   seton                                        31
023300110131     C                   endif
023400110131     C                   endif
023500110131     C*
023600110131     C* Se richiesto no ritorno CCA x consegna dirottamenti
023700110131     C                   if        %subst(prmppt:8:1) = 'D'
023800110131     C                   seton                                        32
023900110131     C                   endif
024000110131     C*
024100110131     C* Se tutto ok => procedo
024200110131     C                   if        *in31 = *off
024300110131     C*
024400110131     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
024500110131     C                   IF        %subst(prmppt:9:1) = 'O'
024600110131     C*
024700110131     C* Inizializzo i campi d wrk
024800110131     C                   movel     *blanks       wEsito1           1
024900110131     C                   movel     *blanks       wEsito2           1
025000110131     C*
025100110131     C* 1 => no messe in consegna
025200110331     C***                IF        *IN31 = *off
025300110331     C***                IF        VACCCA = 'C'
025400110331     C***                SETON                                        31
025500110331     C***                ENDIF
025600110331     C***                ENDIF
025700110131     C*
025800110131     C* 2 => no bolle figlie
025900110131     C                   IF        *IN31 = *off
026000110131     C* Chiamata metodo GetLblTyp
026100110131     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
026200110131     C                                                VACAAS
026300110131     C                                               :VACLNP
026400110131     C                                               :VACNRS
026500110131     C                                               :VACNSP
026600110131     C                                               :pOutLblTyp
026700110131     C                                               :pOutAnnoBO
026800110131     C                                               :pOutLineaParBO
026900110131     C                                               :pOutSerieBO
027000110131     C                                               :pOutNumSpedBO
027100110131     C                                               :pOutDcmBO
027200110131     C                                               :pOutCcaBO
027300110131     C                                               :pOutRblBO))
027400110131     C* Se trattasi d bolla originale proseguo con le verifiche
027500110131     C                   IF        pOutLblTyp <> 'O'
027600110131     C                   SETON                                        31
027700110131     C                   ENDIF
027800110131     C                   ENDIF
027900110131     C*
028000110131     C* 3 => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
028100110331     C***                IF        *IN31 = *off
028200110331     C***                IF        VACCCA = '1' AND
028300110331     C***                          VACDCM = *zeros
028400110331     C***                SETON                                        31
028500110331     C***                ENDIF
028600110331     C***                ENDIF
028700110131     C*
028800110131     C* 4 => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
028900110131     C*      consegnata ultima bolla figlia
029000110131     C                   IF        *IN31 = *off
029100110131     C                   IF        VACDCM > *zeros
029200110331     C***                IF        VACCCA = '1' OR
029300110331     C***                          VACCCA = '2' OR
029400110331     C***                          VACCCA = '9'
029500110331     C                   IF        VACCCA = '2'
029600110131     C* Inizializzo i campi d output
029700110131     C                   move      *blanks       VACCCA
029800110131     C                   move(p)   '00000000'    VACDCM
029900110131     C* Chiamata metodo GetLastChild
030000110131     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
030100110131     C                                                VACAAS
030200110131     C                                               :VACLNP
030300110131     C                                               :VACNRS
030400110131     C                                               :VACNSP
030500110131     C                                               :pOutAnnoFI
030600110131     C                                               :pOutLineaParFI
030700110131     C                                               :pOutSerieFI
030800110131     C                                               :pOutNumSpedFI
030900110131     C                                               :pOutDcmFI
031000110131     C                                               :pOutCcaFI))
031100110131     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
031200110131     C                   if        pOutDcmFI > *zeros
031300110131     C                   move      pOutCcaFI     VACCCA
031400110131     C                   move      pOutDcmFI     VACDCM
031500110131     C                   else
031600110131     C                   SETON                                        31
031700110131     C                   endif
031800110131     C*
031900110131     C                   if        wEsito1 <> *zeros OR
032000110131     C                             wEsito2 <> *zeros
032100110131 xxx C***                SETON                                        31
032200110131     C                   endif
032300110131     C                   ENDIF
032400110131     C                   ENDIF
032500110131     C                   ENDIF
032600110131     C*
032700110131     C                   ENDIF
032800110131     C*
032900110131     C                   if        *in32
033000110131     C                   if        VACCCA = '1'
033100110131     C                   eval      VACCCA = *blanks
033200110131     C                   endif
033300110131     C                   endif
033400110131     C*
033500110131     C                   endif
033600110131     C*
033700110131     C* Se tutto ok => procedo
033800110131     C                   if        *in31 = *off
033900051007     C*
034000051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
034100051007     C  N60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 ,   '))
034200051007     C   60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 .   '))
034300051007     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
034400051007     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
034500051007     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
034600051007     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
034700051007     C  N60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 ,   '))
034800051007     C   60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 .   '))
034900051007     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
035000051007     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
035100110310     C*
035200110310     C* Forzatura x LEVIS => RMN sempre lungo 6 digits
035300110310     C                   MOVE(p)   VACRMN        wRMN              6
035400051007     C*
035500051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
035600040206     C                   IF        %subst(prmppt:2:1) = 'D'
035700100409     C                   EVAL      VACXDTA =%trim(%editc(VACAAS:'Q'))+SepFld+
035800100409     C                                      %trim(%editc(VACLNP:'Q'))+SepFld+
035900100409     C                                      %trim(%editc(VACNRS:'Q'))+SepFld+
036000100409     C                                      %trim(%editc(VACNSP:'Q'))+SepFld+
036100100409     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
036200100409     C                                         '"'+%trim(VACCBO)+'"' +SepFld+
036300100409     C                                      %trim(%editc(VACLNA:'Q'))+SepFld+
036400100409     C                                         '"'+%trim(VACRSD)+'"' +SepFld+
036500100409     C                                         '"'+%trim(VACPRD)+'"' +SepFld+
036600100409     C                                         '"'+%trim(VACGC1)+'"' +SepFld+
036700100409     C                                         '"'+%trim(VACGC2)+'"' +SepFld+
036800100409     C                                      %trim(%editc(VACCTR:'Q'))+SepFld+
036900100409     C                                         '"'+%trim(VACCTS)+'"' +SepFld+
037000100409     C                                         '"'+%trim(VACFTM)+'"' +SepFld+
037100100409     C                                         '"'+%trim(VACFIN)+'"' +SepFld+
037200100409     C                                         '"'+%trim(VACFAP)+'"' +SepFld+
037300100409     C                                         '"'+%trim(VACTSP)+'"' +SepFld+
037400100409     C                                                    %trim(wIAS)+SepFld+
037500100409     C                                         '"'+%trim(VACVAS)+'"' +SepFld+
037600100409     C                                         '"'+%trim(VACNAS)+'"' +SepFld+
037700100409     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
037800100409     C                                                    %trim(wPKB)+SepFld+
037900100409     C                                                    %trim(wVLB)+SepFld+
038000100409     C                                                    %trim(wQFT)+SepFld+
038100100409     C                                         '"'+%trim(VACTIC)+'"' +SepFld+
038200100409     C                                                    %trim(wCAS)+SepFld+
038300100409     C                                         '"'+%trim(VACVCA)+'"' +SepFld+
038400100409     C                                      %trim(%editc(VACCCM:'Q'))+SepFld+
038500110310     C***                                   %trim(%editc(VACRMN:'Q'))+SepFld+
038600110310     C                                         '"'+%trim(wRMN)  +'"' +SepFld+
038700100409     C                                         '"'+%trim(VACRMA)+'"' +SepFld+
038800100409     C                                         '"'+%trim(VACRMO)+'"' +SepFld+
038900100409     C                                         '"'+%trim(VACFFD)+'"' +SepFld+
039000100409     C                                         '"'+%trim(VACTCR)+'"' +SepFld+
039100100409     C                             %trim(%editw(VACDCR:'    /  /  '))+SepFld+
039200100409     C                                      %trim(%editc(VACHCR:'Q'))+SepFld+
039300100409     C                             %trim(%editw(VACDCM:'    /  /  '))+SepFld+
039400100409     C                                      %trim(%editc(VACHMC:'Q'))+SepFld+
039500100409     C                                         '"'+%trim(VACTC1)+'"' +SepFld+
039600100409     C                                         '"'+%trim(VACTC2)+'"' +SepFld+
039700100409     C                                         '"'+%trim(VACCCA)+'"' +SepFld+
039800100409     C                             %trim(%editw(VACDLA:'    /  /  '))+SepFld+
039900051007     C                             %trim(%editw(VACDAG:'    /  /  '))
040000040206     C                   ELSE
040100040206     C*
040200100409     C                   EVAL      VACXDTA =%trim(%editc(VACAAS:'Q'))+SepFld+
040300100409     C                                      %trim(%editc(VACLNP:'Q'))+SepFld+
040400100409     C                                      %trim(%editc(VACNRS:'Q'))+SepFld+
040500100409     C                                      %trim(%editc(VACNSP:'Q'))+SepFld+
040600100409     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
040700100409     C                                          '"'+%trim(VACCBO)+'"'+SepFld+
040800100409     C                                      %trim(%editc(VACLNA:'Q'))+SepFld+
040900100409     C                                          '"'+%trim(VACRSD)+'"'+SepFld+
041000100409     C                                          '"'+%trim(VACPRD)+'"'+SepFld+
041100100409     C                                          '"'+%trim(VACGC1)+'"'+SepFld+
041200100409     C                                          '"'+%trim(VACGC2)+'"'+SepFld+
041300100409     C                                      %trim(%editc(VACCTR:'Q'))+SepFld+
041400100409     C                                          '"'+%trim(VACCTS)+'"'+SepFld+
041500100409     C                                          '"'+%trim(VACFTM)+'"'+SepFld+
041600100409     C                                          '"'+%trim(VACFIN)+'"'+SepFld+
041700100409     C                                          '"'+%trim(VACFAP)+'"'+SepFld+
041800100409     C                                          '"'+%trim(VACTSP)+'"'+SepFld+
041900100409     C                                                    %trim(wIAS)+SepFld+
042000100409     C                                          '"'+%trim(VACVAS)+'"'+SepFld+
042100100409     C                                          '"'+%trim(VACNAS)+'"'+SepFld+
042200100409     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
042300100409     C                                                    %trim(wPKB)+SepFld+
042400100409     C                                                    %trim(wVLB)+SepFld+
042500100409     C                                                    %trim(wQFT)+SepFld+
042600100409     C                                          '"'+%trim(VACTIC)+'"'+SepFld+
042700100409     C                                                    %trim(wCAS)+SepFld+
042800100409     C                                          '"'+%trim(VACVCA)+'"'+SepFld+
042900100409     C                                      %trim(%editc(VACCCM:'Q'))+SepFld+
043000110310     C***                                   %trim(%editc(VACRMN:'Q'))+SepFld+
043100110310     C                                         '"'+%trim(wRMN)  +'"' +SepFld+
043200100409     C                                          '"'+%trim(VACRMA)+'"'+SepFld+
043300100409     C                                          '"'+%trim(VACRMO)+'"'+SepFld+
043400100409     C                                          '"'+%trim(VACFFD)+'"'+SepFld+
043500100409     C                                          '"'+%trim(VACTCR)+'"'+SepFld+
043600100409     C                                      %trim(%editc(VACDCR:'Q'))+SepFld+
043700100409     C                                      %trim(%editc(VACHCR:'Q'))+SepFld+
043800100409     C                                      %trim(%editc(VACDCM:'Q'))+SepFld+
043900100409     C                                      %trim(%editc(VACHMC:'Q'))+SepFld+
044000100409     C                                          '"'+%trim(VACTC1)+'"'+SepFld+
044100100409     C                                          '"'+%trim(VACTC2)+'"'+SepFld+
044200100409     C                                          '"'+%trim(VACCCA)+'"'+SepFld+
044300100409     C                                      %trim(%editc(VACDLA:'Q'))+SepFld+
044400051007     C                                      %trim(%editc(VACDAG:'Q'))
044500040206     C                   ENDIF
044600070426     C*
044700070426     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
044800070426     C                   IF        %subst(prmppt:5:1) = 'I'
044900070426     C*
045000070426     C* Inizializzo i campi "estensione"
045100070426     C                   EVAL      TASIND = *blanks
045200070426     C                   EVAL      TASCAD = *blanks
045300070426     C                   EVAL      TASLOD = *blanks
045400070426     C*
045500070426     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
045600070426     C     KEYtas30C     CHAIN     titas30c
045700070426     C                   IF        %found(titas30c)
045800070426     C*
045900070426     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
046000100409     C                   EVAL      VACXDTA = %trim(VACXDTA) + SepFld+
046100100409     C                                          '"'+%trim(TASIND)+'"'+SepFld+
046200100409     C                                          '"'+%trim(TASCAD)+'"'+SepFld+
046300070426     C                                          '"'+%trim(TASLOD)+'"'
046400070426     C                   ENDIF
046500070426     C                   ENDIF
046600930409     C*
046700110131     C  N31              WRITE     FNVACX00
046800110131     C                   endif
046900110131     C*
047000080924     C                   DELETE    tivgd000
047100991027     C*
047200080924     C                   READ      tivgdtmp
047300030325     C                   ENDDO
047400991027     C*
047500030325     C                   EVAL      wrkesito = '0'
047600991027     C*
047700910830     C                   ENDSR
047800991027
047900991027      /TITLE Operazioni iniziali.
048000991027     C     *inzsr        BEGSR
048100991027     C*
048200991027     C     *ENTRY        PLIST
048300991027     C                   parm                    prmppt
048400991027     C     wrkesito      parm      wrkesito      prmesito
048500070426     C*
048600070426     C* Definizione chiavi
048700070426     C*
048800070426     C* Chiave su TITAS30C - Completa
048900070426     C     KEYtas30C     KLIST
049000070426     C                   KFLD                    vacAAS
049100070426     C                   KFLD                    vacLNP
049200070426     C                   KFLD                    vacNRS
049300070426     C                   KFLD                    vacNSP
049400991027     C*
049500991027     C                   ENDSR
