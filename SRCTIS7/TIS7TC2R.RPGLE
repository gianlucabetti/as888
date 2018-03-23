000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200120426     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500070426     FTITAS30C  IF   E           K DISK
000600140715     FFIAR401L  if   e           k disk    usropn
000700140715     F                                     extfile(LibFileAR4)
000800140715     Ftita430c  if   E           k DISK
000900110314     FTIVAWWWT  uF A E             DISK    commit
001000991027
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400030325
001500030331     D dsvac         e ds                  extname(fnvac00t)
001600051007     D wIAS            s             14
001700051007     D wPKB            s              8
001800051007     D wVLB            s              6
001900051007     D wQFT            s             14
002000051007     D wCAS            s             14
002100140715
002200140715
002300140715     D LibFileAR4      s             21A   inz
002400140715     D currSysNeta     s              8A   inz('*NULL')
002500140715
002600140715     D*------------------
002700140715     D* LINKING A DEFINIZIONI ESTERNE
002800140715     D*------------------
002900140715     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
003000140715     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
003100140715
003200110131
003300110131     D*------------------
003400110131     D* LINKING A DEFINIZIONI ESTERNE
003500110131     D*------------------
003600110131     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003700110131     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003800991027
003900140715
004000140715
004100140715     C*
004200140715     C* Reperisco il sistema AS/400 corrente
004300140715     C                   callp     UBRTVNETA_Rtv(currSysNeta)
004400140715     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
004500140715     C                   eval      LibFileAR4 = 'FILTRA201/FIAR401L'
004600140715     C                   else
004700140715     C                   eval      LibFileAR4 = 'FILTRAPRD/FIAR401L'
004800140715     C                   endif
004900140715     C*
005000140715     C                   if        not %open(FIAR401L)
005100140715     C                   open      FIAR401L
005200140715     C                   endif
005300140715     C*
005400030908     C                   EXSR      chkRECFILMBR
005500991027     C                   EXSR      traduci
005600140715     C*
005700140715     C                   if        %open(FIAR401L)
005800140715     C                   close     FIAR401L
005900140715     C                   endif
006000140715     C*
006100921023     C                   SETON                                        LR
006200140715
006300140715
006400030908
006500030908     C     chkRECFILMBR  BEGSR
006600030908     C*
006700110314     C                   READ      TIVAW000                               55
006800030908     C*
006900030908     C                   ENDSR
007000991027
007100991027     C     traduci       BEGSR
007200051007     C*
007300051007     C* Verifico se richiesto un particolare carattere d separatore campi
007400051007     C                   IF        %subst(prmppt:3:1) <> *blanks
007500100409     C                   EVAL      SepFld = %subst(prmppt:3:1)
007600051007     C                   ELSE
007700100409     C                   MOVEL     ','           SepFld            1
007800051007     C                   ENDIF
007900051007     C*
008000051007     C* Verifico se richiesto un particolare carattere d separatore decimale
008100051007     C                   SETON                                        60
008200051007     C                   IF        %subst(prmppt:4:1) = ','
008300051007     C                   SETOFF                                       60
008400051007     C                   ENDIF
008500030325     C*
008600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
008800110314     C                   EVAL      VAWDTA =
008900100409     C                             '"VACAAS"'                           +SepFld+
009000100409     C                             '"VACLNP"'                           +SepFld+
009100100409     C                             '"VACNRS"'                           +SepFld+
009200100409     C                             '"VACNSP"'                           +SepFld+
009300100409     C                             '"VACMGS"'                           +SepFld+
009400100409     C                             '"VACCBO"'                           +SepFld+
009500100409     C                             '"VACLNA"'                           +SepFld+
009600100409     C                             '"VACRSD"'                           +SepFld+
009700100409     C                             '"VACPRD"'                           +SepFld+
009800100409     C                             '"VACGC1"'                           +SepFld+
009900100409     C                             '"VACGC2"'                           +SepFld+
010000100409     C                             '"VACCTR"'                           +SepFld+
010100100409     C                             '"VACCTS"'                           +SepFld+
010200100409     C                             '"VACFTM"'                           +SepFld+
010300100409     C                             '"VACFIN"'                           +SepFld+
010400100409     C                             '"VACFAP"'                           +SepFld+
010500100409     C                             '"VACTSP"'                           +SepFld+
010600100409     C                             '"VACIAS"'                           +SepFld+
010700100409     C                             '"VACVAS"'                           +SepFld+
010800100409     C                             '"VACNAS"'                           +SepFld+
010900100409     C                             '"VACNCL"'                           +SepFld+
011000100409     C                             '"VACPKB"'                           +SepFld+
011100100409     C                             '"VACVLB"'                           +SepFld+
011200100409     C                             '"VACQFT"'                           +SepFld+
011300100409     C                             '"VACTIC"'                           +SepFld+
011400100409     C                             '"VACCAS"'                           +SepFld+
011500100409     C                             '"VACVCA"'                           +SepFld+
011600100409     C                             '"VACCCM"'                           +SepFld+
011700100409     C                             '"VACRMN"'                           +SepFld+
011800100409     C                             '"VACRMA"'                           +SepFld+
011900100409     C                             '"VACRMO"'                           +SepFld+
012000100409     C                             '"VACFFD"'                           +SepFld+
012100100409     C                             '"VACTCR"'                           +SepFld+
012200100409     C                             '"VACDCR"'                           +SepFld+
012300100409     C                             '"VACHCR"'                           +SepFld+
012400100409     C                             '"VACDCM"'                           +SepFld+
012500100409     C                             '"VACHMC"'                           +SepFld+
012600100409     C                             '"VACTC1"'                           +SepFld+
012700100409     C                             '"VACTC2"'                           +SepFld+
012800100409     C                             '"VACCCA"'                           +SepFld+
012900100409     C                             '"VACDLA"'                           +SepFld+
013000100409     C                             '"VACDAG"'
013100100409     C*
013200100409     C                   IF        %subst(prmppt:5:1) = 'I'
013300110314     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
013400100409     C                             '"VACIND"'                           +SepFld+
013500100409     C                             '"VACCAD"'                           +SepFld+
013600180309     C                             '"VACLOD"'                           +SepFld+
013601180309     C                             '"VACNZD"'
013700100409     C                   ENDIF
013800140715     C*
013900140715     C                   IF        %subst(prmppt:11:1) = 'S'
014000140715     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
014100140715     C                             '"VACFIRMA"'
014200140715     C                   ENDIF
014300100409     C*
014400110314     C                   WRITE     TIVAW000
014500100409     C                   ENDIF
014600100409     C*
014700100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
014800100409     C* (descrizione campi)
014900100409     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
015000110314     C                   EVAL      VAWDTA =
015100100409     C                             '"ANNO SPEDIZIONE"'                  +SepFld+
015200100409     C                             '"FILIALE PARTENZA"'                 +SepFld+
015300100409     C                             '"NUMERO SERIE"'                     +SepFld+
015400100409     C                             '"NUMERO SPEDIZIONE"'                +SepFld+
015500100409     C                             '"MESE/GIORNO SPEDIZIONE"'           +SepFld+
015600100409     C                             '"CODICE BOLLA"'                     +SepFld+
015700100409     C                             '"FILIALE ARRIVO"'                   +SepFld+
015800100409     C                             '"RAGIONE SOCIALE DESTINATARIO"'     +SepFld+
015900100409     C                             '"PROVINCIA DESTINATARIO"'           +SepFld+
016000100409     C                             '"1° GG.CHIUSURA DESTINATARIO"'      +SepFld+
016100100409     C                             '"2° GG.CHIUSURA DESTINATARIO"'      +SepFld+
016200100409     C                             '"CODICE TARIFFA"'                   +SepFld+
016300100409     C                             '"CODICE TASSAZIONE"'                +SepFld+
016400100409     C                             '"FLAG TASSAZ C/TARIFFA MITTENTE"'   +SepFld+
016500100409     C                             '"FLAG INOLTRO"'                     +SepFld+
016600100409     C                             '"FLAG ANTEPORTO"'                   +SepFld+
016700100409     C                             '"TIPO SERVIZIO BOLLE"'              +SepFld+
016800100409     C                             '"IMPORTO DA ASSICURARE"'            +SepFld+
016900100409     C                             '"DIVISA IMPORTO DA ASSICURARE"'     +SepFld+
017000100409     C                             '"NATURA MERCE MITTENTE"'            +SepFld+
017100100409     C                             '"NUMERO COLLI"'                     +SepFld+
017200100409     C                             '"PESO KG BOLLETTATO"'               +SepFld+
017300100409     C                             '"VOLUME BOLLETTATO"'                +SepFld+
017400100409     C                             '"QUANTITA DA FATTURARE"'            +SepFld+
017500100409     C                             '"TIPO INCASSO CONTRASSEGNA"'        +SepFld+
017600100409     C                             '"CONTRASSEGNO"'                     +SepFld+
017700100409     C                             '"DIVISA IMPORTO C/ASSEGNO"'         +SepFld+
017800100409     C                             '"COD.CLIENTE MITTENTE"'             +SepFld+
017900100409     C                             '"RIFERIMENTO MITTENTE NUMERICO"'    +SepFld+
018000100409     C                             '"RIFERIMENTO MITTENTE ALFABETICO"'  +SepFld+
018100100409     C                             '"RAG.SOC.MITTENTE ORIGINALE"'       +SepFld+
018200100409     C                             '"S=FERMO DEPOSITO"'                 +SepFld+
018300100409     C                             '"TIPO DT.CONSEGNA:IL/PRIMA/DOPO"'   +SepFld+
018400100409     C                             '"DATA CONSEGNA RICHIESTA"'          +SepFld+
018500100409     C                             '"ORA CONSEGNA RICHIESTA"'           +SepFld+
018600100409     C                             '"DATA CONSEGNA MERCE REALE"'        +SepFld+
018700100409     C                             '"ORA CONSEGNA REALE (HHMM)"'        +SepFld+
018800100409     C                             '"1 CONSEGNA PARTICOLARE"'           +SepFld+
018900100409     C                             '"2 CONSEGNA PARTICOLARE"'           +SepFld+
019000100409     C                             '"CODICE CONSEGNA ANOMALA"'          +SepFld+
019100100409     C                             '"DATA LASCIATO AVVISO"'             +SepFld+
019200100409     C                             '"DATA APERTURA GIACENZA"'
019300100409     C*
019400100409     C                   IF        %subst(prmppt:5:1) = 'I'
019500110314     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
019600100409     C                             '"INDIRIZZO DESTINATARIO"'           +SepFld+
019700100409     C                             '"CAP DESTINATARIO"'                 +SepFld+
019800180309     C                             '"LOCALITA DESTINATARIO"'            +SepFld+
019801180309     C                             '"NAZIONE DESTINATARIO"'
019900100409     C                   ENDIF
020000140715     C*
020100140715     C                   IF        %subst(prmppt:11:1) = 'S'
020200140715     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
020300140715     C                             '"FIRMA"'
020400140715     C                   ENDIF
020500030325     C*
020600110314     C                   WRITE     TIVAW000
020700030325     C                   ENDIF
020800100409     C*
020900100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
021000100409     C* (descrizione campi in inglese)
021100100409     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
021200110314     C                   EVAL      VAWDTA =
021300100409     C                             '"SHIPMENT YEAR"'                    +SepFld+
021400100409     C                             '"OPERATIVE DEPARTURE DEPOT"'        +SepFld+
021500100409     C                             '"SERIAL NUMBER"'                    +SepFld+
021600100409     C                             '"SHIPMENT NUMBER"'                  +SepFld+
021700100409     C                             '"SHIPMENT MONTH/DAY"'               +SepFld+
021800100409     C                             '"NOTE CODE"'                        +SepFld+
021900100409     C                             '"OPERATIVE DESTINATION DEPOT"'      +SepFld+
022000100409     C                             '"DESTINATION NAME"'                 +SepFld+
022100100409     C                             '"DESTINATION PROVINCE"'             +SepFld+
022200100409     C                             '"1ST DESTINATION CLOSURE DAY"'      +SepFld+
022300100409     C                             '"2ND DESTINATION CLOSURE DAY"'      +SepFld+
022400100409     C                             '"TARIFF CODE"'                      +SepFld+
022500100409     C                             '"TAXING CODE"'                      +SepFld+
022600100409     C                             '"SENDER TARIFF/ TAXING CODE FLAG"'  +SepFld+
022700100409     C                             '"COLLECTION PLACE"'                 +SepFld+
022800100409     C                             '"DELIVERY PLACE"'                   +SepFld+
022900100409     C                             '"DATA ENTRY TYPE"'                  +SepFld+
023000100409     C                             '"SUM TO INSURE"'                    +SepFld+
023100100409     C                             '"CURRENCY OF SUM TO INSURE"'        +SepFld+
023200100409     C                             '"SENDER GOODS TYPE"'                +SepFld+
023300100409     C                             '"NUMBER OF PACKS"'                  +SepFld+
023400100409     C                             '"WEIGHT KG LABELLED"'               +SepFld+
023500100409     C                             '"VOLUME LABELLED"'                  +SepFld+
023600100409     C                             '"QUANTITY TO INVOICE"'              +SepFld+
023700100409     C                             '"TYPE OF C.O.D. PAYMENT"'           +SepFld+
023800100409     C                             '"C.O.D."'                           +SepFld+
023900100409     C                             '"C.O.D.SUM CURRENCY"'               +SepFld+
024000100409     C                             '"SENDER CLIENT CODE"'               +SepFld+
024100100409     C                             '"NUMERICAL SENDER REFERENCE"'       +SepFld+
024200100409     C                             '"ALPHABETICAL SENDER REFERENCE"'    +SepFld+
024300100409     C                             '"ORIGINAL SENDER NAME"'             +SepFld+
024400100409     C                             '"S=TO BE HOLD AT DEPOT"'            +SepFld+
024500100409     C                             '"DELIVERY TYPE: AT/BEFORE/AFTER"'   +SepFld+
024600100409     C                             '"REQUESTED DELIVERY DATE"'          +SepFld+
024700100409     C                             '"REQUESTED DELIVERY TIME"'          +SepFld+
024800100409     C                             '"EFFECTIVE GOODS DELIVERY DATE"'    +SepFld+
024900100409     C                             '"EFFECTIVE GOODS DELIVERY TIME"'    +SepFld+
025000100409     C                             '"1ST SPECIAL DELIVERY"'             +SepFld+
025100100409     C                             '"2ND SPECIAL DELIVERY"'             +SepFld+
025200100409     C                             '"FAULTY DELIVERY CODE"'             +SepFld+
025300100409     C                             '"DATE OF NOTIFICATION"'             +SepFld+
025400100409     C                             '"STOCK OPENING DATE"'
025500100409     C*
025600100409     C                   IF        %subst(prmppt:5:1) = 'I'
025700110314     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
025800100409     C                             '"RECIPIENT ADDRESS"'                +SepFld+
025900100409     C                             '"RECIPIENT POSTAL CODE"'            +SepFld+
026000180309     C                             '"RECIPIENT AREA"'                   +SepFld+
026001180309     C                             '"RECIPIENT COUNTRY"'
026100100409     C                   ENDIF
026200140715     C*
026300140715     C                   IF        %subst(prmppt:11:1) = 'S'
026400140715     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
026500140715     C                             '"SIGNATURE"'
026600140715     C                   ENDIF
026700100409     C*
026800110314     C                   WRITE     TIVAW000
026900100409     C                   ENDIF
027000991027     C*
027100080924     C                   READ      tivgdtmp
027200080924     C                   DOW       not %eof(tivgdtmp)
027300080924     c                   movel     vgddta        dsvac
027400991027     C*
027500030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
027600060411     C***  '"':''''      XLATE     dsvac         dsvac
027700110131     C*
027800110131     C                   SETOFF                                       3132
027900110131     C*
028000110131     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
028100110131     C                   if        %subst(prmppt:7:1) = 'S'
028200110131     C                   if        VACNRS = *zeros
028300110131     C                   seton                                        31
028400110131     C                   endif
028500110131     C                   endif
028600110131     C*
028700110131     C* Se richiesto no ritorno CCA x consegna dirottamenti
028800110131     C                   if        %subst(prmppt:8:1) = 'D'
028900110131     C                   seton                                        32
029000110131     C                   endif
029100110131     C*
029200110131     C* Se tutto ok => procedo
029300110131     C                   if        *in31 = *off
029400120426     C*
029500120426     C* Se richiesto nei parametri traduzione solo spedizione valide ai fini della spedizione =>
029600120426     C                   IF        %subst(prmppt:10:1) <> *blanks
029700120426     C*
029800120426     C* Inizializzo i campi d wrk
029900120426     C                   movel     *blanks       wEsito1           1
030000120426     C*
030100120426     C* Chiamata metodo GetLblTyp
030200120426     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
030300120426     C                                                VACAAS
030400120426     C                                               :VACLNP
030500120426     C                                               :VACNRS
030600120426     C                                               :VACNSP
030700120426     C                                               :pOutLblTyp
030800120426     C                                               :pOutAnnoBO
030900120426     C                                               :pOutLineaParBO
031000120426     C                                               :pOutSerieBO
031100120426     C                                               :pOutNumSpedBO
031200120426     C                                               :pOutDcmBO
031300120426     C                                               :pOutCcaBO
031400120426     C                                               :pOutRblBO))
031500120426     C* Se no errore => considero solo se trattasi del raggruppamento bolla richiesto
031600120426     C                   if        wEsito1  = *zeros
031700120426     C                   IF        pOutRblBO <> %subst(prmppt:10:1)
031800120426     C                   SETON                                        31
031900120426     C                   ENDIF
032000120426     C                   endif
032100120426     C                   ENDIF
032200110131     C*
032300110131     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
032400110131     C                   IF        %subst(prmppt:9:1) = 'O'
032500110131     C*
032600110131     C* Inizializzo i campi d wrk
032700110131     C                   movel     *blanks       wEsito1           1
032800110131     C                   movel     *blanks       wEsito2           1
032900110131     C*
033000110131     C* 1 => no messe in consegna
033100110131     C                   IF        *IN31 = *off
033200110131     C                   IF        VACCCA = 'C'
033300110131     C                   SETON                                        31
033400110131     C                   ENDIF
033500110131     C                   ENDIF
033600110131     C*
033700110131     C* 2 => no bolle figlie
033800110131     C                   IF        *IN31 = *off
033900110131     C* Chiamata metodo GetLblTyp
034000110131     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
034100110131     C                                                VACAAS
034200110131     C                                               :VACLNP
034300110131     C                                               :VACNRS
034400110131     C                                               :VACNSP
034500110131     C                                               :pOutLblTyp
034600110131     C                                               :pOutAnnoBO
034700110131     C                                               :pOutLineaParBO
034800110131     C                                               :pOutSerieBO
034900110131     C                                               :pOutNumSpedBO
035000110131     C                                               :pOutDcmBO
035100110131     C                                               :pOutCcaBO
035200110131     C                                               :pOutRblBO))
035300110131     C* Se trattasi d bolla originale proseguo con le verifiche
035400110131     C                   IF        pOutLblTyp <> 'O'
035500110131     C                   SETON                                        31
035600110131     C                   ENDIF
035700110131     C                   ENDIF
035800110131     C*
035900110131     C* 3 => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
036000110131     C                   IF        *IN31 = *off
036100110131     C                   IF        VACCCA = '1' AND
036200110131     C                             VACDCM = *zeros
036300110131     C                   SETON                                        31
036400110131     C                   ENDIF
036500110131     C                   ENDIF
036600110131     C*
036700110131     C* 4 => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
036800110131     C*      consegnata ultima bolla figlia
036900110131     C                   IF        *IN31 = *off
037000110131     C                   IF        VACDCM > *zeros
037100110131     C                   IF        VACCCA = '1' OR
037200110131     C                             VACCCA = '2' OR
037300110131     C                             VACCCA = '9'
037400110131     C* Inizializzo i campi d output
037500110131     C                   move      *blanks       VACCCA
037600110131     C                   move(p)   '00000000'    VACDCM
037700110131     C* Chiamata metodo GetLastChild
037800110131     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
037900110131     C                                                VACAAS
038000110131     C                                               :VACLNP
038100110131     C                                               :VACNRS
038200110131     C                                               :VACNSP
038300110131     C                                               :pOutAnnoFI
038400110131     C                                               :pOutLineaParFI
038500110131     C                                               :pOutSerieFI
038600110131     C                                               :pOutNumSpedFI
038700110131     C                                               :pOutDcmFI
038800110131     C                                               :pOutCcaFI))
038900110131     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
039000110131     C                   if        pOutDcmFI > *zeros
039100110131     C                   move      pOutCcaFI     VACCCA
039200110131     C                   move      pOutDcmFI     VACDCM
039300110131     C                   else
039400110131     C                   SETON                                        31
039500110131     C                   endif
039600110131     C*
039700110131     C                   if        wEsito1 <> *zeros OR
039800110131     C                             wEsito2 <> *zeros
039900110131 xxx C***                SETON                                        31
040000110131     C                   endif
040100110131     C                   ENDIF
040200110131     C                   ENDIF
040300110131     C                   ENDIF
040400110131     C*
040500110131     C                   ENDIF
040600110131     C*
040700110131     C                   if        *in32
040800110131     C                   if        VACCCA = '1'
040900110131     C                   eval      VACCCA = *blanks
041000110131     C                   endif
041100110131     C                   endif
041200110131     C*
041300110131     C                   endif
041400110131     C*
041500110131     C* Se tutto ok => procedo
041600110131     C                   if        *in31 = *off
041700051007     C*
041800051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
041900051007     C  N60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 ,   '))
042000051007     C   60              EVAL      wIAS = %trim(%editw(VACIAS:'        0 .   '))
042100051007     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
042200051007     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
042300051007     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
042400051007     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
042500051007     C  N60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 ,   '))
042600051007     C   60              EVAL      wQFT = %trim(%editw(VACQFT:'        0 .   '))
042700051007     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
042800051007     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
042900051007     C*
043000051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
043100040206     C                   IF        %subst(prmppt:2:1) = 'D'
043200110314     C                   EVAL      VAWDTA = %trim(%editc(VACAAS:'Q'))+SepFld+
043300100409     C                                      %trim(%editc(VACLNP:'Q'))+SepFld+
043400100409     C                                      %trim(%editc(VACNRS:'Q'))+SepFld+
043500100409     C                                      %trim(%editc(VACNSP:'Q'))+SepFld+
043600100409     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
043700100409     C                                         '"'+%trim(VACCBO)+'"' +SepFld+
043800100409     C                                      %trim(%editc(VACLNA:'Q'))+SepFld+
043900100409     C                                         '"'+%trim(VACRSD)+'"' +SepFld+
044000100409     C                                         '"'+%trim(VACPRD)+'"' +SepFld+
044100100409     C                                         '"'+%trim(VACGC1)+'"' +SepFld+
044200100409     C                                         '"'+%trim(VACGC2)+'"' +SepFld+
044300100409     C                                      %trim(%editc(VACCTR:'Q'))+SepFld+
044400100409     C                                         '"'+%trim(VACCTS)+'"' +SepFld+
044500100409     C                                         '"'+%trim(VACFTM)+'"' +SepFld+
044600100409     C                                         '"'+%trim(VACFIN)+'"' +SepFld+
044700100409     C                                         '"'+%trim(VACFAP)+'"' +SepFld+
044800100409     C                                         '"'+%trim(VACTSP)+'"' +SepFld+
044900100409     C                                                    %trim(wIAS)+SepFld+
045000100409     C                                         '"'+%trim(VACVAS)+'"' +SepFld+
045100100409     C                                         '"'+%trim(VACNAS)+'"' +SepFld+
045200100409     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
045300100409     C                                                    %trim(wPKB)+SepFld+
045400100409     C                                                    %trim(wVLB)+SepFld+
045500100409     C                                                    %trim(wQFT)+SepFld+
045600100409     C                                         '"'+%trim(VACTIC)+'"' +SepFld+
045700100409     C                                                    %trim(wCAS)+SepFld+
045800100409     C                                         '"'+%trim(VACVCA)+'"' +SepFld+
045900100409     C                                      %trim(%editc(VACCCM:'Q'))+SepFld+
046000100409     C                                      %trim(%editc(VACRMN:'Q'))+SepFld+
046100100409     C                                         '"'+%trim(VACRMA)+'"' +SepFld+
046200100409     C                                         '"'+%trim(VACRMO)+'"' +SepFld+
046300100409     C                                         '"'+%trim(VACFFD)+'"' +SepFld+
046400100409     C                                         '"'+%trim(VACTCR)+'"' +SepFld+
046500100409     C                             %trim(%editw(VACDCR:'    /  /  '))+SepFld+
046600100409     C                                      %trim(%editc(VACHCR:'Q'))+SepFld+
046700100409     C                             %trim(%editw(VACDCM:'    /  /  '))+SepFld+
046800100409     C                                      %trim(%editc(VACHMC:'Q'))+SepFld+
046900100409     C                                         '"'+%trim(VACTC1)+'"' +SepFld+
047000100409     C                                         '"'+%trim(VACTC2)+'"' +SepFld+
047100100409     C                                         '"'+%trim(VACCCA)+'"' +SepFld+
047200100409     C                             %trim(%editw(VACDLA:'    /  /  '))+SepFld+
047300051007     C                             %trim(%editw(VACDAG:'    /  /  '))
047400040206     C                   ELSE
047500040206     C*
047600110314     C                   EVAL      VAWDTA = %trim(%editc(VACAAS:'Q'))+SepFld+
047700100409     C                                      %trim(%editc(VACLNP:'Q'))+SepFld+
047800100409     C                                      %trim(%editc(VACNRS:'Q'))+SepFld+
047900100409     C                                      %trim(%editc(VACNSP:'Q'))+SepFld+
048000100409     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
048100100409     C                                          '"'+%trim(VACCBO)+'"'+SepFld+
048200100409     C                                      %trim(%editc(VACLNA:'Q'))+SepFld+
048300100409     C                                          '"'+%trim(VACRSD)+'"'+SepFld+
048400100409     C                                          '"'+%trim(VACPRD)+'"'+SepFld+
048500100409     C                                          '"'+%trim(VACGC1)+'"'+SepFld+
048600100409     C                                          '"'+%trim(VACGC2)+'"'+SepFld+
048700100409     C                                      %trim(%editc(VACCTR:'Q'))+SepFld+
048800100409     C                                          '"'+%trim(VACCTS)+'"'+SepFld+
048900100409     C                                          '"'+%trim(VACFTM)+'"'+SepFld+
049000100409     C                                          '"'+%trim(VACFIN)+'"'+SepFld+
049100100409     C                                          '"'+%trim(VACFAP)+'"'+SepFld+
049200100409     C                                          '"'+%trim(VACTSP)+'"'+SepFld+
049300100409     C                                                    %trim(wIAS)+SepFld+
049400100409     C                                          '"'+%trim(VACVAS)+'"'+SepFld+
049500100409     C                                          '"'+%trim(VACNAS)+'"'+SepFld+
049600100409     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
049700100409     C                                                    %trim(wPKB)+SepFld+
049800100409     C                                                    %trim(wVLB)+SepFld+
049900100409     C                                                    %trim(wQFT)+SepFld+
050000100409     C                                          '"'+%trim(VACTIC)+'"'+SepFld+
050100100409     C                                                    %trim(wCAS)+SepFld+
050200100409     C                                          '"'+%trim(VACVCA)+'"'+SepFld+
050300100409     C                                      %trim(%editc(VACCCM:'Q'))+SepFld+
050400100409     C                                      %trim(%editc(VACRMN:'Q'))+SepFld+
050500100409     C                                          '"'+%trim(VACRMA)+'"'+SepFld+
050600100409     C                                          '"'+%trim(VACRMO)+'"'+SepFld+
050700100409     C                                          '"'+%trim(VACFFD)+'"'+SepFld+
050800100409     C                                          '"'+%trim(VACTCR)+'"'+SepFld+
050900100409     C                                      %trim(%editc(VACDCR:'Q'))+SepFld+
051000100409     C                                      %trim(%editc(VACHCR:'Q'))+SepFld+
051100100409     C                                      %trim(%editc(VACDCM:'Q'))+SepFld+
051200100409     C                                      %trim(%editc(VACHMC:'Q'))+SepFld+
051300100409     C                                          '"'+%trim(VACTC1)+'"'+SepFld+
051400100409     C                                          '"'+%trim(VACTC2)+'"'+SepFld+
051500100409     C                                          '"'+%trim(VACCCA)+'"'+SepFld+
051600100409     C                                      %trim(%editc(VACDLA:'Q'))+SepFld+
051700051007     C                                      %trim(%editc(VACDAG:'Q'))
051800040206     C                   ENDIF
051900070426     C*
052000070426     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
052100070426     C                   IF        %subst(prmppt:5:1) = 'I'
052200070426     C*
052300070426     C* Inizializzo i campi "estensione"
052400070426     C                   EVAL      TASIND = *blanks
052500070426     C                   EVAL      TASCAD = *blanks
052600070426     C                   EVAL      TASLOD = *blanks
052601180309     C                   EVAL      TASNZD = *blanks
052700070426     C*
052800070426     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
052900070426     C     KEYtas30C     CHAIN     titas30c
053000070426     C                   IF        %found(titas30c)
053100070426     C*
053200070426     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
053300110314     C                   EVAL      VAWDTA  = %trim(VAWDTA) + SepFld+
053400100409     C                                          '"'+%trim(TASIND)+'"'+SepFld+
053500100409     C                                          '"'+%trim(TASCAD)+'"'+SepFld+
053600180309     C                                          '"'+%trim(TASLOD)+'"'+SepFld+
053601180309     C                                          '"'+%trim(TASNZD)+'"'
053700070426     C                   ENDIF
053800070426     C                   ENDIF
053900140715     C*
054000140715     C* Se richiesto nei parametri d input reperisco la firma di consegna merce
054100140715     C                   IF        %subst(prmppt:11:1) = 'S'
054200140715     C*
054300140715     C                   movel     *blanks       wNOTE_1          35
054400140715     C*
054500140715     C                   eval      ar4trc = '1'
054600140715     C     KEYar401_C    chain     fiar401l
054700140715     C                   if        %found(fiar401l)
054800140715     C                   eval      wNOTE_1 = ar4NOT
054900140715     C                   else
055000140715     C     KEYar401_C    chain     tita430c
055100140715     C                   if        %found(tita430c)
055200140715     C                   eval      wNOTE_1 = ta4NOT
055300140715     C                   endif
055400140715     C                   endif
055500140715     C*
055600140715     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
055700140715     C                   EVAL      VAWDTA  = %trim(VAWDTA) + SepFld+
055800140715     C                                          '"'+%trim(wNOTE_1)+'"'
055900140715     C*
056000140715     C                   ENDIF
056100930409     C*
056200110314     C  N31              WRITE     TIVAW000
056300110131     C                   endif
056400110131     C*
056500080924     C                   DELETE    tivgd000
056600991027     C*
056700080924     C                   READ      tivgdtmp
056800030325     C                   ENDDO
056900991027     C*
057000030325     C                   EVAL      wrkesito = '0'
057100991027     C*
057200910830     C                   ENDSR
057300991027
057400991027      /TITLE Operazioni iniziali.
057500991027     C     *inzsr        BEGSR
057600991027     C*
057700991027     C     *ENTRY        PLIST
057800991027     C                   parm                    prmppt
057900991027     C     wrkesito      parm      wrkesito      prmesito
058000070426     C*
058100070426     C* Definizione chiavi
058200070426     C*
058300070426     C* Chiave su TITAS30C - Completa
058400070426     C     KEYtas30C     KLIST
058500070426     C                   KFLD                    vacAAS
058600070426     C                   KFLD                    vacLNP
058700070426     C                   KFLD                    vacNRS
058800070426     C                   KFLD                    vacNSP
058900140715     C*
059000140715     C* Chiave su FIAR401L / TITA430C - Completa
059100140715     C     KEYar401_C    KLIST
059200140715     C                   KFLD                    vacAAS
059300140715     C                   KFLD                    vacLNP
059400140715     C                   KFLD                    vacNRS
059500140715     C                   KFLD                    vacNSP
059600140715     C                   KFLD                    ar4trc
059700991027     C*
059800991027     C                   ENDSR
