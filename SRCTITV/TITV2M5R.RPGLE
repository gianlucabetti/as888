000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110131     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500070426     FTITAS30C  IF   E           K DISK
000600121011     FTNCSB03L  IF   E           K DISK
000700121011     FTIVAWWWT  uF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300030331     D dsvac         e ds                  extname(fnvac00t)
001400051007     D wPKB            s              8
001500051007     D wVLB            s              6
001600051007     D wCAS            s             14
001700121011     D wDDP            s              8  0
001800110131
001900110131     D*------------------
002000110131     D* LINKING A DEFINIZIONI ESTERNE
002100110131     D*------------------
002200110131     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002300110131     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002400991027
002500121011
002600030908     C                   EXSR      chkRECFILMBR
002700991027     C                   EXSR      traduci
002800921023     C                   SETON                                        LR
002900121011
003000030908
003100030908     C     chkRECFILMBR  BEGSR
003200030908     C*
003300121011     C                   READ      TIVAWWWT                               55
003400030908     C*
003500030908     C                   ENDSR
003600991027
003700121011
003800991027     C     traduci       BEGSR
003900051007     C*
004000051007     C* Verifico se richiesto un particolare carattere d separatore campi
004100051007     C                   IF        %subst(prmppt:3:1) <> *blanks
004200100409     C                   EVAL      SepFld = %subst(prmppt:3:1)
004300051007     C                   ELSE
004400100409     C                   MOVEL     ','           SepFld            1
004500051007     C                   ENDIF
004600051007     C*
004700051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004800051007     C                   SETON                                        60
004900051007     C                   IF        %subst(prmppt:4:1) = ','
005000051007     C                   SETOFF                                       60
005100051007     C                   ENDIF
005200030325     C*
005300030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005400030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005500121011     C                   EVAL      VAWDTA =
005600100409     C                             '"VACAAS"'                           +SepFld+
005700100409     C                             '"VACLNP"'                           +SepFld+
005800121011     C                             '"VACRMN"'                           +SepFld+
005900100409     C                             '"VACMGS"'                           +SepFld+
006000100409     C                             '"VACLNA"'                           +SepFld+
006100100409     C                             '"VACRSD"'                           +SepFld+
006200100409     C                             '"VACPRD"'                           +SepFld+
006300121011     C                             '"VACFIN"'                           +SepFld+
006400121011     C                             '"VACNCL"'                           +SepFld+
006500121011     C                             '"VACPKB"'                           +SepFld+
006600121011     C                             '"VACVLB"'                           +SepFld+
006700121011     C                             '"VACCAS"'                           +SepFld+
006800121011     C                             '"VACDCM"'                           +SepFld+
006900121011     C                             '"VACFFD"'                           +SepFld+
007000121011     C                             '"VACTC1"'                           +SepFld+
007100121011     C                             '"VACTC2"'                           +SepFld+
007200121011     C                             '"VACCCA"'                           +SepFld+
007300121011     C                             '"VACDLA"'                           +SepFld+
007400121011     C                             '"VACDAG"'                           +SepFld+
007500121011     C                             '"CSBDDP"'
007600100409     C*
007700100409     C                   IF        %subst(prmppt:5:1) = 'I'
007800121011     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
007900100409     C                             '"VACIND"'                           +SepFld+
008000100409     C                             '"VACCAD"'                           +SepFld+
008100100409     C                             '"VACLOD"'
008200100409     C                   ENDIF
008300100409     C*
008400121011     C                   WRITE     TIVAW000
008500100409     C                   ENDIF
008600100409     C*
008700100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
008800100409     C* (descrizione campi)
008900100409     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
009000121011     C                   EVAL      VAWDTA =
009100100409     C                             '"ANNO SPEDIZIONE"'                  +SepFld+
009200100409     C                             '"FILIALE PARTENZA"'                 +SepFld+
009300121011     C                             '"RIFERIMENTO MITTENTE NUMERICO"'    +SepFld+
009400100409     C                             '"MESE/GIORNO SPEDIZIONE"'           +SepFld+
009500100409     C                             '"FILIALE ARRIVO"'                   +SepFld+
009600100409     C                             '"RAGIONE SOCIALE DESTINATARIO"'     +SepFld+
009700100409     C                             '"PROVINCIA DESTINATARIO"'           +SepFld+
009800121011     C                             '"FLAG INOLTRO"'                     +SepFld+
009900121011     C                             '"NUMERO COLLI"'                     +SepFld+
010000121011     C                             '"PESO KG BOLLETTATO"'               +SepFld+
010100121011     C                             '"VOLUME BOLLETTATO"'                +SepFld+
010200121011     C                             '"CONTRASSEGNO"'                     +SepFld+
010300121011     C                             '"DATA CONSEGNA MERCE REALE"'        +SepFld+
010400121011     C                             '"S=FERMO DEPOSITO"'                 +SepFld+
010500121011     C                             '"1 CONSEGNA PARTICOLARE"'           +SepFld+
010600121011     C                             '"2 CONSEGNA PARTICOLARE"'           +SepFld+
010700121011     C                             '"CODICE CONSEGNA ANOMALA"'          +SepFld+
010800121011     C                             '"DATA LASCIATO AVVISO"'             +SepFld+
010900121011     C                             '"DATA APERTURA GIACENZA"'           +SepFld+
011000121011     C                             '"DATA PAGAMENTO C/ASSEGNO"'
011100100409     C*
011200121011     C                   IF        %subst(prmppt:5:1) = 'D'
011300121011     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
011400100409     C                             '"INDIRIZZO DESTINATARIO"'           +SepFld+
011500100409     C                             '"CAP DESTINATARIO"'                 +SepFld+
011600100409     C                             '"LOCALITA DESTINATARIO"'
011700100409     C                   ENDIF
011800030325     C*
011900121011     C                   WRITE     TIVAW000
012000030325     C                   ENDIF
012100100409     C*
012200100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
012300100409     C* (descrizione campi in inglese)
012400100409     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
012500121011     C                   EVAL      VAWDTA =
012600100409     C                             '"SHIPMENT YEAR"'                    +SepFld+
012700100409     C                             '"OPERATIVE DEPARTURE DEPOT"'        +SepFld+
012800121011     C                             '"NUMERICAL SENDER REFERENCE"'       +SepFld+
012900100409     C                             '"SHIPMENT MONTH/DAY"'               +SepFld+
013000100409     C                             '"OPERATIVE DESTINATION DEPOT"'      +SepFld+
013100100409     C                             '"DESTINATION NAME"'                 +SepFld+
013200100409     C                             '"DESTINATION PROVINCE"'             +SepFld+
013300121011     C                             '"COLLECTION PLACE"'                 +SepFld+
013400121011     C                             '"NUMBER OF PACKS"'                  +SepFld+
013500121011     C                             '"WEIGHT KG LABELLED"'               +SepFld+
013600121011     C                             '"VOLUME LABELLED"'                  +SepFld+
013700121011     C                             '"C.O.D."'                           +SepFld+
013800121011     C                             '"EFFECTIVE GOODS DELIVERY DATE"'    +SepFld+
013900121011     C                             '"S=TO BE HOLD AT DEPOT"'            +SepFld+
014000121011     C                             '"1ST SPECIAL DELIVERY"'             +SepFld+
014100121011     C                             '"2ND SPECIAL DELIVERY"'             +SepFld+
014200121011     C                             '"FAULTY DELIVERY CODE"'             +SepFld+
014300121011     C                             '"DATE OF NOTIFICATION"'             +SepFld+
014400121011     C                             '"STOCK OPENING DATE"'               +SepFld+
014500121011     C                             '"C.O.D. PAYMENT DATE"'
014600100409     C*
014700121011     C                   IF        %subst(prmppt:5:1) = 'E'
014800121011     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
014900100409     C                             '"RECIPIENT ADDRESS"'                +SepFld+
015000100409     C                             '"RECIPIENT POSTAL CODE"'            +SepFld+
015100100409     C                             '"RECIPIENT AREA"'
015200100409     C                   ENDIF
015300100409     C*
015400121011     C                   WRITE     TIVAW000
015500100409     C                   ENDIF
015600991027     C*
015700080924     C                   READ      tivgdtmp
015800080924     C                   DOW       not %eof(tivgdtmp)
015900121011     C*
016000121011     C                   clear                   wPKB
016100121011     C                   clear                   wVLB
016200121011     C                   clear                   wCAS
016300121011     C                   clear                   wDDP
016400121011     C*
016500121011     C                   movel     vgddta        dsvac
016600991027     C*
016700030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
016800060411     C***  '"':''''      XLATE     dsvac         dsvac
016900110131     C*
017000110131     C                   SETOFF                                       3132
017100110131     C*
017200110131     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
017300110131     C                   if        %subst(prmppt:7:1) = 'S'
017400110131     C                   if        VACNRS = *zeros
017500110131     C                   seton                                        31
017600110131     C                   endif
017700110131     C                   endif
017800110131     C*
017900110131     C* Se richiesto no ritorno CCA x consegna dirottamenti
018000110131     C                   if        %subst(prmppt:8:1) = 'D'
018100110131     C                   seton                                        32
018200110131     C                   endif
018300121011     C*
018400121011     C* Se richiesto nei parametri solo/escluso una specifica filiale:
018500121011     C* - "solo"
018600121011     C                   if        %subst(prmppt:11:3) <> *blanks AND
018700121011     C                             %subst(prmppt:10:1) =  'S'
018800121011     C                   if        %editc(vacLNP:'X') <> %subst(prmppt:11:3)
018900121011     C                   seton                                        31
019000121011     C                   endif
019100121011     C                   endif
019200121011     C* - "escluso"
019300121011     C                   if        %subst(prmppt:11:3) <> *blanks AND
019400121011     C                             %subst(prmppt:10:1) =  'E'
019500121011     C                   if        %editc(vacLNP:'X')  = %subst(prmppt:11:3)
019600121011     C                   seton                                        31
019700121011     C                   endif
019800121011     C                   endif
019900110131     C*
020000110131     C* Se tutto ok => procedo
020100110131     C                   if        *in31 = *off
020200110131     C*
020300110131     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
020400110131     C                   IF        %subst(prmppt:9:1) = 'O'
020500110131     C*
020600110131     C* Inizializzo i campi d wrk
020700110131     C                   movel     *blanks       wEsito1           1
020800110131     C                   movel     *blanks       wEsito2           1
020900110131     C*
021000110131     C* 1 => no messe in consegna
021100110331     C***                IF        *IN31 = *off
021200110331     C***                IF        VACCCA = 'C'
021300110331     C***                SETON                                        31
021400110331     C***                ENDIF
021500110331     C***                ENDIF
021600110131     C*
021700110131     C* 2 => no bolle figlie
021800110131     C                   IF        *IN31 = *off
021900110131     C* Chiamata metodo GetLblTyp
022000110131     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
022100110131     C                                                VACAAS
022200110131     C                                               :VACLNP
022300110131     C                                               :VACNRS
022400110131     C                                               :VACNSP
022500110131     C                                               :pOutLblTyp
022600110131     C                                               :pOutAnnoBO
022700110131     C                                               :pOutLineaParBO
022800110131     C                                               :pOutSerieBO
022900110131     C                                               :pOutNumSpedBO
023000110131     C                                               :pOutDcmBO
023100110131     C                                               :pOutCcaBO
023200110131     C                                               :pOutRblBO))
023300110131     C* Se trattasi d bolla originale proseguo con le verifiche
023400110131     C                   IF        pOutLblTyp <> 'O'
023500110131     C                   SETON                                        31
023600110131     C                   ENDIF
023700110131     C                   ENDIF
023800110131     C*
023900110131     C* 3 => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
024000110331     C***                IF        *IN31 = *off
024100110331     C***                IF        VACCCA = '1' AND
024200110331     C***                          VACDCM = *zeros
024300110331     C***                SETON                                        31
024400110331     C***                ENDIF
024500110331     C***                ENDIF
024600110131     C*
024700110131     C* 4 => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
024800110131     C*      consegnata ultima bolla figlia
024900110131     C                   IF        *IN31 = *off
025000110131     C                   IF        VACDCM > *zeros
025100110331     C***                IF        VACCCA = '1' OR
025200110331     C***                          VACCCA = '2' OR
025300110331     C***                          VACCCA = '9'
025400110331     C                   IF        VACCCA = '2'
025500110131     C* Inizializzo i campi d output
025600110131     C                   move      *blanks       VACCCA
025700110131     C                   move(p)   '00000000'    VACDCM
025800110131     C* Chiamata metodo GetLastChild
025900110131     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
026000110131     C                                                VACAAS
026100110131     C                                               :VACLNP
026200110131     C                                               :VACNRS
026300110131     C                                               :VACNSP
026400110131     C                                               :pOutAnnoFI
026500110131     C                                               :pOutLineaParFI
026600110131     C                                               :pOutSerieFI
026700110131     C                                               :pOutNumSpedFI
026800110131     C                                               :pOutDcmFI
026900110131     C                                               :pOutCcaFI))
027000110131     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
027100110131     C                   if        pOutDcmFI > *zeros
027200110131     C                   move      pOutCcaFI     VACCCA
027300110131     C                   move      pOutDcmFI     VACDCM
027400110131     C                   else
027500110131     C                   SETON                                        31
027600110131     C                   endif
027700110131     C*
027800110131     C                   if        wEsito1 <> *zeros OR
027900110131     C                             wEsito2 <> *zeros
028000110131 xxx C***                SETON                                        31
028100110131     C                   endif
028200110131     C                   ENDIF
028300110131     C                   ENDIF
028400110131     C                   ENDIF
028500110131     C*
028600110131     C                   ENDIF
028700110131     C*
028800110131     C                   if        *in32
028900110131     C                   if        VACCCA = '1'
029000110131     C                   eval      VACCCA = *blanks
029100110131     C                   endif
029200110131     C                   endif
029300110131     C*
029400110131     C                   endif
029500110131     C*
029600110131     C* Se tutto ok => procedo
029700110131     C                   if        *in31 = *off
029800051007     C*
029900051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
030000051007     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
030100051007     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
030200051007     C  N60              EVAL      wVLB = %trim(%editw(VACVLB:'0 ,   '))
030300051007     C   60              EVAL      wVLB = %trim(%editw(VACVLB:'0 .   '))
030400051007     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
030500051007     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
030600121011     C*
030700121011     C* Reperisco i dati del contrassegno dal TNCSB03L
030800121011     C     KEYcsb03C     CHAIN     tncsb03l
030900121011     C                   IF        %found(tncsb03l)
031000121011     C                   eval      wDDP = csbDDP
031100121011     C                   ENDIF
031200051007     C*
031300051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
031400040206     C                   IF        %subst(prmppt:2:1) = 'D'
031500121011     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+SepFld+
031600100409     C                                      %trim(%editc(VACLNP:'Q'))+SepFld+
031700121011     C                                      %trim(%editc(VACRMN:'Q'))+SepFld+
031800121011     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
031900121011     C                                      %trim(%editc(VACLNA:'Q'))+SepFld+
032000121011     C                                         '"'+%trim(VACRSD)+'"' +SepFld+
032100121011     C                                         '"'+%trim(VACPRD)+'"' +SepFld+
032200121011     C                                         '"'+%trim(VACFIN)+'"' +SepFld+
032300121011     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
032400121011     C                                                    %trim(wPKB)+SepFld+
032500121011     C                                                    %trim(wVLB)+SepFld+
032600121011     C                                                    %trim(wCAS)+SepFld+
032700121011     C                             %trim(%editw(VACDCM:'    /  /  '))+SepFld+
032800121011     C                                         '"'+%trim(VACFFD)+'"' +SepFld+
032900121011     C                                         '"'+%trim(VACTC1)+'"' +SepFld+
033000121011     C                                         '"'+%trim(VACTC2)+'"' +SepFld+
033100121011     C                                         '"'+%trim(VACCCA)+'"' +SepFld+
033200121011     C                             %trim(%editw(VACDLA:'    /  /  '))+SepFld+
033300121011     C                             %trim(%editw(VACDAG:'    /  /  '))+SepFld+
033400121011     C                             %trim(%editw(CSBDDP:'    /  /  '))
033500121011     C*
033600040206     C                   ELSE
033700121011     C*
033800121011     C                   EVAL      VAWDTA  =%trim(%editc(VACAAS:'Q'))+SepFld+
033900121011     C                                      %trim(%editc(VACLNP:'Q'))+SepFld+
034000121011     C                                      %trim(%editc(VACRMN:'Q'))+SepFld+
034100121011     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
034200121011     C                                      %trim(%editc(VACLNA:'Q'))+SepFld+
034300121011     C                                         '"'+%trim(VACRSD)+'"' +SepFld+
034400121011     C                                         '"'+%trim(VACPRD)+'"' +SepFld+
034500121011     C                                         '"'+%trim(VACFIN)+'"' +SepFld+
034600121011     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
034700121011     C                                                    %trim(wPKB)+SepFld+
034800121011     C                                                    %trim(wVLB)+SepFld+
034900121011     C                                                    %trim(wCAS)+SepFld+
035000121011     C                                      %trim(%editc(VACDCM:'Q'))+SepFld+
035100121011     C                                         '"'+%trim(VACFFD)+'"' +SepFld+
035200121011     C                                         '"'+%trim(VACTC1)+'"' +SepFld+
035300121011     C                                         '"'+%trim(VACTC2)+'"' +SepFld+
035400121011     C                                         '"'+%trim(VACCCA)+'"' +SepFld+
035500121011     C                                      %trim(%editc(VACDLA:'Q'))+SepFld+
035600121011     C                                      %trim(%editc(VACDAG:'Q'))+SepFld+
035700121011     C                                      %trim(%editc(CSBDDP:'Q'))
035800040206     C*
035900040206     C                   ENDIF
036000070426     C*
036100070426     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
036200070426     C                   IF        %subst(prmppt:5:1) = 'I'
036300070426     C*
036400070426     C* Inizializzo i campi "estensione"
036500070426     C                   EVAL      TASIND = *blanks
036600070426     C                   EVAL      TASCAD = *blanks
036700070426     C                   EVAL      TASLOD = *blanks
036800070426     C*
036900070426     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
037000070426     C     KEYtas30C     CHAIN     titas30c
037100070426     C                   IF        %found(titas30c)
037200070426     C*
037300070426     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
037400121011     C                   EVAL      VAWDTA =         %trim(VAWDTA)   + SepFld+
037500100409     C                                          '"'+%trim(TASIND)+'"'+SepFld+
037600100409     C                                          '"'+%trim(TASCAD)+'"'+SepFld+
037700070426     C                                          '"'+%trim(TASLOD)+'"'
037800070426     C                   ENDIF
037900070426     C                   ENDIF
038000930409     C*
038100121011     C  N31              WRITE     TIVAW000
038200110131     C                   endif
038300110131     C*
038400080924     C                   DELETE    tivgd000
038500991027     C*
038600080924     C                   READ      tivgdtmp
038700030325     C                   ENDDO
038800991027     C*
038900030325     C                   EVAL      wrkesito = '0'
039000991027     C*
039100910830     C                   ENDSR
039200991027
039300121011
039400121011
039500991027      /TITLE Operazioni iniziali.
039600991027     C     *inzsr        BEGSR
039700991027     C*
039800991027     C     *ENTRY        PLIST
039900991027     C                   parm                    prmppt
040000991027     C     wrkesito      parm      wrkesito      prmesito
040100070426     C*
040200070426     C* Definizione chiavi
040300070426     C*
040400070426     C* Chiave su TITAS30C - Completa
040500070426     C     KEYtas30C     KLIST
040600070426     C                   KFLD                    vacAAS
040700070426     C                   KFLD                    vacLNP
040800070426     C                   KFLD                    vacNRS
040900070426     C                   KFLD                    vacNSP
041000121011     C*
041100121011     C* Chiave su TNCSB03L - Completa
041200121011     C     KEYcsb03C     KLIST
041300121011     C                   KFLD                    vacAAS
041400121011     C                   KFLD                    vacLNP
041500121011     C                   KFLD                    vacNRS
041600121011     C                   KFLD                    vacNSP
041700991027     C*
041800991027     C                   ENDSR
