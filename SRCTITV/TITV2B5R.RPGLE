000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
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
001800101125     D wRMN            s             15
001900101125     D wRMA            s             15
002000991027
002100030908     C                   EXSR      chkRECFILMBR
002200991027     C                   EXSR      traduci
002300921023     C                   SETON                                        LR
002400030908
002500030908     C     chkRECFILMBR  BEGSR
002600030908     C*
002700030908     C                   READ      FNVACXLS                               55
002800030908     C*
002900030908     C                   ENDSR
003000991027
003100991027     C     traduci       BEGSR
003200051007     C*
003300051007     C* Verifico se richiesto un particolare carattere d separatore campi
003400051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003500100409     C                   EVAL      SepFld = %subst(prmppt:3:1)
003600051007     C                   ELSE
003700100409     C                   MOVEL     ','           SepFld            1
003800051007     C                   ENDIF
003900051007     C*
004000051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004100051007     C                   SETON                                        60
004200051007     C                   IF        %subst(prmppt:4:1) = ','
004300051007     C                   SETOFF                                       60
004400051007     C                   ENDIF
004500030325     C*
004600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004800100409     C                   EVAL      VACXDTA =
004900100409     C                             '"VACAAS"'                           +SepFld+
005000100409     C                             '"VACLNP"'                           +SepFld+
005100100409     C                             '"VACNRS"'                           +SepFld+
005200100409     C                             '"VACNSP"'                           +SepFld+
005300100409     C                             '"VACMGS"'                           +SepFld+
005400100409     C                             '"VACCBO"'                           +SepFld+
005500100409     C                             '"VACLNA"'                           +SepFld+
005600100409     C                             '"VACRSD"'                           +SepFld+
005700100409     C                             '"VACPRD"'                           +SepFld+
005800100409     C                             '"VACGC1"'                           +SepFld+
005900100409     C                             '"VACGC2"'                           +SepFld+
006000100409     C                             '"VACCTR"'                           +SepFld+
006100100409     C                             '"VACCTS"'                           +SepFld+
006200100409     C                             '"VACFTM"'                           +SepFld+
006300100409     C                             '"VACFIN"'                           +SepFld+
006400100409     C                             '"VACFAP"'                           +SepFld+
006500100409     C                             '"VACTSP"'                           +SepFld+
006600100409     C                             '"VACIAS"'                           +SepFld+
006700100409     C                             '"VACVAS"'                           +SepFld+
006800100409     C                             '"VACNAS"'                           +SepFld+
006900100409     C                             '"VACNCL"'                           +SepFld+
007000100409     C                             '"VACPKB"'                           +SepFld+
007100100409     C                             '"VACVLB"'                           +SepFld+
007200100409     C                             '"VACQFT"'                           +SepFld+
007300100409     C                             '"VACTIC"'                           +SepFld+
007400100409     C                             '"VACCAS"'                           +SepFld+
007500100409     C                             '"VACVCA"'                           +SepFld+
007600100409     C                             '"VACCCM"'                           +SepFld+
007700100409     C                             '"VACRMN"'                           +SepFld+
007800100409     C                             '"VACRMA"'                           +SepFld+
007900100409     C                             '"VACRMO"'                           +SepFld+
008000100409     C                             '"VACFFD"'                           +SepFld+
008100100409     C                             '"VACTCR"'                           +SepFld+
008200100409     C                             '"VACDCR"'                           +SepFld+
008300100409     C                             '"VACHCR"'                           +SepFld+
008400100409     C                             '"VACDCM"'                           +SepFld+
008500100409     C                             '"VACHMC"'                           +SepFld+
008600100409     C                             '"VACTC1"'                           +SepFld+
008700100409     C                             '"VACTC2"'                           +SepFld+
008800100409     C                             '"VACCCA"'                           +SepFld+
008900100409     C                             '"VACDLA"'                           +SepFld+
009000100409     C                             '"VACDAG"'
009100100409     C*
009200100409     C                   IF        %subst(prmppt:5:1) = 'I'
009300100409     C                   EVAL      VACXDTA = %trim(VACXDTA)             +SepFld+
009400100409     C                             '"VACIND"'                           +SepFld+
009500100409     C                             '"VACCAD"'                           +SepFld+
009600100409     C                             '"VACLOD"'
009700100409     C                   ENDIF
009800100409     C*
009900100409     C                   WRITE     FNVACX00
010000100409     C                   ENDIF
010100100409     C*
010200100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010300100409     C* (descrizione campi)
010400100409     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
010500100409     C                   EVAL      VACXDTA =
010600100409     C                             '"ANNO SPEDIZIONE"'                  +SepFld+
010700100409     C                             '"FILIALE PARTENZA"'                 +SepFld+
010800100409     C                             '"NUMERO SERIE"'                     +SepFld+
010900100409     C                             '"NUMERO SPEDIZIONE"'                +SepFld+
011000100409     C                             '"MESE/GIORNO SPEDIZIONE"'           +SepFld+
011100100409     C                             '"CODICE BOLLA"'                     +SepFld+
011200100409     C                             '"FILIALE ARRIVO"'                   +SepFld+
011300100409     C                             '"RAGIONE SOCIALE DESTINATARIO"'     +SepFld+
011400100409     C                             '"PROVINCIA DESTINATARIO"'           +SepFld+
011500100409     C                             '"1° GG.CHIUSURA DESTINATARIO"'      +SepFld+
011600100409     C                             '"2° GG.CHIUSURA DESTINATARIO"'      +SepFld+
011700100409     C                             '"CODICE TARIFFA"'                   +SepFld+
011800100409     C                             '"CODICE TASSAZIONE"'                +SepFld+
011900100409     C                             '"FLAG TASSAZ C/TARIFFA MITTENTE"'   +SepFld+
012000100409     C                             '"FLAG INOLTRO"'                     +SepFld+
012100100409     C                             '"FLAG ANTEPORTO"'                   +SepFld+
012200100409     C                             '"TIPO SERVIZIO BOLLE"'              +SepFld+
012300100409     C                             '"IMPORTO DA ASSICURARE"'            +SepFld+
012400100409     C                             '"DIVISA IMPORTO DA ASSICURARE"'     +SepFld+
012500100409     C                             '"NATURA MERCE MITTENTE"'            +SepFld+
012600100409     C                             '"NUMERO COLLI"'                     +SepFld+
012700100409     C                             '"PESO KG BOLLETTATO"'               +SepFld+
012800100409     C                             '"VOLUME BOLLETTATO"'                +SepFld+
012900100409     C                             '"QUANTITA DA FATTURARE"'            +SepFld+
013000100409     C                             '"TIPO INCASSO CONTRASSEGNA"'        +SepFld+
013100100409     C                             '"CONTRASSEGNO"'                     +SepFld+
013200100409     C                             '"DIVISA IMPORTO C/ASSEGNO"'         +SepFld+
013300100409     C                             '"COD.CLIENTE MITTENTE"'             +SepFld+
013400100409     C                             '"RIFERIMENTO MITTENTE NUMERICO"'    +SepFld+
013500100409     C                             '"RIFERIMENTO MITTENTE ALFABETICO"'  +SepFld+
013600100409     C                             '"RAG.SOC.MITTENTE ORIGINALE"'       +SepFld+
013700100409     C                             '"S=FERMO DEPOSITO"'                 +SepFld+
013800100409     C                             '"TIPO DT.CONSEGNA:IL/PRIMA/DOPO"'   +SepFld+
013900100409     C                             '"DATA CONSEGNA RICHIESTA"'          +SepFld+
014000100409     C                             '"ORA CONSEGNA RICHIESTA"'           +SepFld+
014100100409     C                             '"DATA CONSEGNA MERCE REALE"'        +SepFld+
014200100409     C                             '"ORA CONSEGNA REALE (HHMM)"'        +SepFld+
014300100409     C                             '"1 CONSEGNA PARTICOLARE"'           +SepFld+
014400100409     C                             '"2 CONSEGNA PARTICOLARE"'           +SepFld+
014500100409     C                             '"CODICE CONSEGNA ANOMALA"'          +SepFld+
014600100409     C                             '"DATA LASCIATO AVVISO"'             +SepFld+
014700100409     C                             '"DATA APERTURA GIACENZA"'
014800100409     C*
014900100409     C                   IF        %subst(prmppt:5:1) = 'I'
015000100409     C                   EVAL      VACXDTA = %trim(VACXDTA)             +SepFld+
015100100409     C                             '"INDIRIZZO DESTINATARIO"'           +SepFld+
015200100409     C                             '"CAP DESTINATARIO"'                 +SepFld+
015300100409     C                             '"LOCALITA DESTINATARIO"'
015400100409     C                   ENDIF
015500030325     C*
015600030331     C                   WRITE     FNVACX00
015700030325     C                   ENDIF
015800100409     C*
015900100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
016000100409     C* (descrizione campi in inglese)
016100100409     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
016200100409     C                   EVAL      VACXDTA =
016300100409     C                             '"SHIPMENT YEAR"'                    +SepFld+
016400100409     C                             '"OPERATIVE DEPARTURE DEPOT"'        +SepFld+
016500100409     C                             '"SERIAL NUMBER"'                    +SepFld+
016600100409     C                             '"SHIPMENT NUMBER"'                  +SepFld+
016700100409     C                             '"SHIPMENT MONTH/DAY"'               +SepFld+
016800100409     C                             '"NOTE CODE"'                        +SepFld+
016900100409     C                             '"OPERATIVE DESTINATION DEPOT"'      +SepFld+
017000100409     C                             '"DESTINATION NAME"'                 +SepFld+
017100100409     C                             '"DESTINATION PROVINCE"'             +SepFld+
017200100409     C                             '"1ST DESTINATION CLOSURE DAY"'      +SepFld+
017300100409     C                             '"2ND DESTINATION CLOSURE DAY"'      +SepFld+
017400100409     C                             '"TARIFF CODE"'                      +SepFld+
017500100409     C                             '"TAXING CODE"'                      +SepFld+
017600100409     C                             '"SENDER TARIFF/ TAXING CODE FLAG"'  +SepFld+
017700100409     C                             '"COLLECTION PLACE"'                 +SepFld+
017800100409     C                             '"DELIVERY PLACE"'                   +SepFld+
017900100409     C                             '"DATA ENTRY TYPE"'                  +SepFld+
018000100409     C                             '"SUM TO INSURE"'                    +SepFld+
018100100409     C                             '"CURRENCY OF SUM TO INSURE"'        +SepFld+
018200100409     C                             '"SENDER GOODS TYPE"'                +SepFld+
018300100409     C                             '"NUMBER OF PACKS"'                  +SepFld+
018400100409     C                             '"WEIGHT KG LABELLED"'               +SepFld+
018500100409     C                             '"VOLUME LABELLED"'                  +SepFld+
018600100409     C                             '"QUANTITY TO INVOICE"'              +SepFld+
018700100409     C                             '"TYPE OF C.O.D. PAYMENT"'           +SepFld+
018800100409     C                             '"C.O.D."'                           +SepFld+
018900100409     C                             '"C.O.D.SUM CURRENCY"'               +SepFld+
019000100409     C                             '"SENDER CLIENT CODE"'               +SepFld+
019100100409     C                             '"NUMERICAL SENDER REFERENCE"'       +SepFld+
019200100409     C                             '"ALPHABETICAL SENDER REFERENCE"'    +SepFld+
019300100409     C                             '"ORIGINAL SENDER NAME"'             +SepFld+
019400100409     C                             '"S=TO BE HOLD AT DEPOT"'            +SepFld+
019500100409     C                             '"DELIVERY TYPE: AT/BEFORE/AFTER"'   +SepFld+
019600100409     C                             '"REQUESTED DELIVERY DATE"'          +SepFld+
019700100409     C                             '"REQUESTED DELIVERY TIME"'          +SepFld+
019800100409     C                             '"EFFECTIVE GOODS DELIVERY DATE"'    +SepFld+
019900100409     C                             '"EFFECTIVE GOODS DELIVERY TIME"'    +SepFld+
020000100409     C                             '"1ST SPECIAL DELIVERY"'             +SepFld+
020100100409     C                             '"2ND SPECIAL DELIVERY"'             +SepFld+
020200100409     C                             '"FAULTY DELIVERY CODE"'             +SepFld+
020300100409     C                             '"DATE OF NOTIFICATION"'             +SepFld+
020400100409     C                             '"STOCK OPENING DATE"'
020500100409     C*
020600100409     C                   IF        %subst(prmppt:5:1) = 'I'
020700100409     C                   EVAL      VACXDTA = %trim(VACXDTA)             +SepFld+
020800100409     C                             '"RECIPIENT ADDRESS"'                +SepFld+
020900100409     C                             '"RECIPIENT POSTAL CODE"'            +SepFld+
021000100409     C                             '"RECIPIENT AREA"'
021100100409     C                   ENDIF
021200100409     C*
021300100409     C                   WRITE     FNVACX00
021400100409     C                   ENDIF
021500991027     C*
021600080924     C                   READ      tivgdtmp
021700080924     C                   DOW       not %eof(tivgdtmp)
021800080924     c                   movel     vgddta        dsvac
021900991027     C*
022000030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
022100060411     C***  '"':''''      XLATE     dsvac         dsvac
022200051007     C*
022300051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
022400051007     C  N60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 ,   '))
022500051007     C   60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 .   '))
022600051007     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
022700051007     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
022800051007     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
022900051007     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
023000051007     C  N60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 ,   '))
023100051007     C   60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 .   '))
023200051007     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
023300051007     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
023400101125     C*
023500101125     C                   EVAL      wRMN = %trim(%editc(VACRMN:'Q'))
023600101125     C                   EVAL      wRMA = %trim(VACRMA)
023700101125     C*
023800101125     C* Inizializzo i campi "estensione"
023900101125     C                   EVAL      TASIND = *blanks
024000101125     C                   EVAL      TASCAD = *blanks
024100101125     C                   EVAL      TASLOD = *blanks
024200101125     C*
024300101125     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
024400101125     C     KEYtas30C     CHAIN     titas30c
024500101125     C                   IF        %found(titas30c)
024600101125     C*
024700101125     C* Forzatura x sistemazione casino fatto da Bovisa
024800101125     C                   IF        TASAAS*10000+TASMGS >= 20101119 and
024900101125     C                             TASAAS*10000+TASMGS <= 20101124
025000101125     C* Inverto i riferimenti
025100101125     C                   EVAL      wRMN = %trim(VACRMA)
025200101125     C                   EVAL      wRMA = %trim(%editc(VACRMN:'Q'))
025300101125     C                   ENDIF
025400051007     C*
025500051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
025600040206     C                   IF        %subst(prmppt:2:1) = 'D'
025700100409     C                   EVAL      VACXDTA =%trim(%editc(VACAAS:'Q'))+SepFld+
025800100409     C                                      %trim(%editc(VACLNP:'Q'))+SepFld+
025900100409     C                                      %trim(%editc(VACNRS:'Q'))+SepFld+
026000100409     C                                      %trim(%editc(VACNSP:'Q'))+SepFld+
026100100409     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
026200100409     C                                         '"'+%trim(VACCBO)+'"' +SepFld+
026300100409     C                                      %trim(%editc(VACLNA:'Q'))+SepFld+
026400100409     C                                         '"'+%trim(VACRSD)+'"' +SepFld+
026500100409     C                                         '"'+%trim(VACPRD)+'"' +SepFld+
026600100409     C                                         '"'+%trim(VACGC1)+'"' +SepFld+
026700100409     C                                         '"'+%trim(VACGC2)+'"' +SepFld+
026800100409     C                                      %trim(%editc(VACCTR:'Q'))+SepFld+
026900100409     C                                         '"'+%trim(VACCTS)+'"' +SepFld+
027000100409     C                                         '"'+%trim(VACFTM)+'"' +SepFld+
027100100409     C                                         '"'+%trim(VACFIN)+'"' +SepFld+
027200100409     C                                         '"'+%trim(VACFAP)+'"' +SepFld+
027300100409     C                                         '"'+%trim(VACTSP)+'"' +SepFld+
027400100409     C                                                    %trim(wIAS)+SepFld+
027500100409     C                                         '"'+%trim(VACVAS)+'"' +SepFld+
027600100409     C                                         '"'+%trim(VACNAS)+'"' +SepFld+
027700100409     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
027800100409     C                                                    %trim(wPKB)+SepFld+
027900100409     C                                                    %trim(wVLB)+SepFld+
028000100409     C                                                    %trim(wQFT)+SepFld+
028100100409     C                                         '"'+%trim(VACTIC)+'"' +SepFld+
028200100409     C                                                    %trim(wCAS)+SepFld+
028300100409     C                                         '"'+%trim(VACVCA)+'"' +SepFld+
028400100409     C                                      %trim(%editc(VACCCM:'Q'))+SepFld+
028500101125     C                                             %trim(wRMN)       +SepFld+
028600101125     C                                         '"'+%trim(wRMA)  +'"' +SepFld+
028700100409     C                                         '"'+%trim(VACRMO)+'"' +SepFld+
028800100409     C                                         '"'+%trim(VACFFD)+'"' +SepFld+
028900100409     C                                         '"'+%trim(VACTCR)+'"' +SepFld+
029000100409     C                             %trim(%editw(VACDCR:'    /  /  '))+SepFld+
029100100409     C                                      %trim(%editc(VACHCR:'Q'))+SepFld+
029200100409     C                             %trim(%editw(VACDCM:'    /  /  '))+SepFld+
029300100409     C                                      %trim(%editc(VACHMC:'Q'))+SepFld+
029400100409     C                                         '"'+%trim(VACTC1)+'"' +SepFld+
029500100409     C                                         '"'+%trim(VACTC2)+'"' +SepFld+
029600100409     C                                         '"'+%trim(VACCCA)+'"' +SepFld+
029700100409     C                             %trim(%editw(VACDLA:'    /  /  '))+SepFld+
029800051007     C                             %trim(%editw(VACDAG:'    /  /  '))
029900040206     C                   ELSE
030000040206     C*
030100100409     C                   EVAL      VACXDTA =%trim(%editc(VACAAS:'Q'))+SepFld+
030200100409     C                                      %trim(%editc(VACLNP:'Q'))+SepFld+
030300100409     C                                      %trim(%editc(VACNRS:'Q'))+SepFld+
030400100409     C                                      %trim(%editc(VACNSP:'Q'))+SepFld+
030500100409     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
030600100409     C                                          '"'+%trim(VACCBO)+'"'+SepFld+
030700100409     C                                      %trim(%editc(VACLNA:'Q'))+SepFld+
030800100409     C                                          '"'+%trim(VACRSD)+'"'+SepFld+
030900100409     C                                          '"'+%trim(VACPRD)+'"'+SepFld+
031000100409     C                                          '"'+%trim(VACGC1)+'"'+SepFld+
031100100409     C                                          '"'+%trim(VACGC2)+'"'+SepFld+
031200100409     C                                      %trim(%editc(VACCTR:'Q'))+SepFld+
031300100409     C                                          '"'+%trim(VACCTS)+'"'+SepFld+
031400100409     C                                          '"'+%trim(VACFTM)+'"'+SepFld+
031500100409     C                                          '"'+%trim(VACFIN)+'"'+SepFld+
031600100409     C                                          '"'+%trim(VACFAP)+'"'+SepFld+
031700100409     C                                          '"'+%trim(VACTSP)+'"'+SepFld+
031800100409     C                                                    %trim(wIAS)+SepFld+
031900100409     C                                          '"'+%trim(VACVAS)+'"'+SepFld+
032000100409     C                                          '"'+%trim(VACNAS)+'"'+SepFld+
032100100409     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
032200100409     C                                                    %trim(wPKB)+SepFld+
032300100409     C                                                    %trim(wVLB)+SepFld+
032400100409     C                                                    %trim(wQFT)+SepFld+
032500100409     C                                          '"'+%trim(VACTIC)+'"'+SepFld+
032600100409     C                                                    %trim(wCAS)+SepFld+
032700100409     C                                          '"'+%trim(VACVCA)+'"'+SepFld+
032800100409     C                                      %trim(%editc(VACCCM:'Q'))+SepFld+
032900101125     C                                              %trim(wRMN)      +SepFld+
033000101125     C                                          '"'+%trim(wRMA)  +'"'+SepFld+
033100100409     C                                          '"'+%trim(VACRMO)+'"'+SepFld+
033200100409     C                                          '"'+%trim(VACFFD)+'"'+SepFld+
033300100409     C                                          '"'+%trim(VACTCR)+'"'+SepFld+
033400100409     C                                      %trim(%editc(VACDCR:'Q'))+SepFld+
033500100409     C                                      %trim(%editc(VACHCR:'Q'))+SepFld+
033600100409     C                                      %trim(%editc(VACDCM:'Q'))+SepFld+
033700100409     C                                      %trim(%editc(VACHMC:'Q'))+SepFld+
033800100409     C                                          '"'+%trim(VACTC1)+'"'+SepFld+
033900100409     C                                          '"'+%trim(VACTC2)+'"'+SepFld+
034000100409     C                                          '"'+%trim(VACCCA)+'"'+SepFld+
034100100409     C                                      %trim(%editc(VACDLA:'Q'))+SepFld+
034200051007     C                                      %trim(%editc(VACDAG:'Q'))
034300040206     C                   ENDIF
034400101125     C*
034500101125     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
034600101125     C                   IF        %subst(prmppt:5:1) = 'I'
034700070426     C*
034800070426     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
034900100409     C                   EVAL      VACXDTA = %trim(VACXDTA) + SepFld+
035000100409     C                                          '"'+%trim(TASIND)+'"'+SepFld+
035100100409     C                                          '"'+%trim(TASCAD)+'"'+SepFld+
035200070426     C                                          '"'+%trim(TASLOD)+'"'
035300070426     C                   ENDIF
035400070426     C                   ENDIF
035500930409     C*
035600030331     C                   WRITE     FNVACX00
035700080924     C                   DELETE    tivgd000
035800991027     C*
035900080924     C                   READ      tivgdtmp
036000030325     C                   ENDDO
036100991027     C*
036200030325     C                   EVAL      wrkesito = '0'
036300991027     C*
036400910830     C                   ENDSR
036500991027
036600991027      /TITLE Operazioni iniziali.
036700991027     C     *inzsr        BEGSR
036800991027     C*
036900991027     C     *ENTRY        PLIST
037000991027     C                   parm                    prmppt
037100991027     C     wrkesito      parm      wrkesito      prmesito
037200070426     C*
037300070426     C* Definizione chiavi
037400070426     C*
037500070426     C* Chiave su TITAS30C - Completa
037600070426     C     KEYtas30C     KLIST
037700070426     C                   KFLD                    vacAAS
037800070426     C                   KFLD                    vacLNP
037900070426     C                   KFLD                    vacNRS
038000070426     C                   KFLD                    vacNSP
038100991027     C*
038200991027     C                   ENDSR
