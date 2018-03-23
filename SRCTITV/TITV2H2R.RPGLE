000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110131     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500070426     FTITAS30C  IF   E           K DISK
000600110314     FTIVAWWWT  uF A E             DISK    commit
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
003100110314     C                   READ      TIVAW000                               55
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
005200110314     C                   EVAL      VAWDTA =
005300100409     C                             '"VACMGS"'                           +SepFld+
005400100409     C                             '"VACRSD"'                           +SepFld+
005500100409     C                             '"VACPRD"'                           +SepFld+
005600100409     C                             '"VACNCL"'                           +SepFld+
005700100409     C                             '"VACCCM"'                           +SepFld+
005800100409     C                             '"VACRMA"'                           +SepFld+
005900100409     C                             '"VACDCM"'                           +SepFld+
006000100409     C                             '"VACCCA"'                           +SepFld+
006100100409     C                             '"VACDLA"'                           +SepFld+
006200100409     C                             '"VACDAG"'
006300100409     C*
006400100409     C                   IF        %subst(prmppt:5:1) = 'I'
006500110314     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
006600100409     C                             '"VACIND"'                           +SepFld+
006700100409     C                             '"VACCAD"'                           +SepFld+
006800100409     C                             '"VACLOD"'
006900100409     C                   ENDIF
007000100409     C*
007100110314     C                   WRITE     TIVAW000
007200100409     C                   ENDIF
007300100409     C*
007400100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007500100409     C* (descrizione campi)
007600100409     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
007700110314     C                   EVAL      VAWDTA =
007800100409     C                             '"MESE/GIORNO SPEDIZIONE"'           +SepFld+
007900100409     C                             '"RAGIONE SOCIALE DESTINATARIO"'     +SepFld+
008000100409     C                             '"PROVINCIA DESTINATARIO"'           +SepFld+
008100100409     C                             '"NUMERO COLLI"'                     +SepFld+
008200100409     C                             '"PESO KG BOLLETTATO"'               +SepFld+
008300100409     C                             '"COD.CLIENTE MITTENTE"'             +SepFld+
008400100409     C                             '"RIFERIMENTO MITTENTE ALFABETICO"'  +SepFld+
008500100409     C                             '"DATA CONSEGNA MERCE REALE"'        +SepFld+
008600100409     C                             '"CODICE CONSEGNA ANOMALA"'          +SepFld+
008700100409     C                             '"DATA LASCIATO AVVISO"'             +SepFld+
008800100409     C                             '"DATA APERTURA GIACENZA"'
008900100409     C*
009000100409     C                   IF        %subst(prmppt:5:1) = 'I'
009100110314     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
009200100409     C                             '"INDIRIZZO DESTINATARIO"'           +SepFld+
009300100409     C                             '"CAP DESTINATARIO"'                 +SepFld+
009400100409     C                             '"LOCALITA DESTINATARIO"'
009500100409     C                   ENDIF
009600030325     C*
009700110314     C                   WRITE     TIVAW000
009800030325     C                   ENDIF
009900100409     C*
010000100409     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
010100100409     C* (descrizione campi in inglese)
010200100409     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
010300110314     C                   EVAL      VAWDTA =
010400100409     C                             '"SHIPMENT MONTH/DAY"'               +SepFld+
010500110721     C                             '"RECIPIENT NAME"'                   +SepFld+
010600110721     C                             '"RECIPIENT PROVINCE"'               +SepFld+
010700100409     C                             '"NUMBER OF PACKS"'                  +SepFld+
010800100409     C                             '"WEIGHT KG LABELLED"'               +SepFld+
010900100409     C                             '"SENDER CLIENT CODE"'               +SepFld+
011000100409     C                             '"ALPHABETICAL SENDER REFERENCE"'    +SepFld+
011100100409     C                             '"EFFECTIVE GOODS DELIVERY DATE"'    +SepFld+
011200100409     C                             '"FAULTY DELIVERY CODE"'             +SepFld+
011300100409     C                             '"DATE OF NOTIFICATION"'             +SepFld+
011400100409     C                             '"STOCK OPENING DATE"'
011500100409     C*
011600100409     C                   IF        %subst(prmppt:5:1) = 'I'
011700110314     C                   EVAL      VAWDTA = %trim(VAWDTA)               +SepFld+
011800100409     C                             '"RECIPIENT ADDRESS"'                +SepFld+
011900100409     C                             '"RECIPIENT POSTAL CODE"'            +SepFld+
012000100409     C                             '"RECIPIENT AREA"'
012100100409     C                   ENDIF
012200100409     C*
012300110314     C                   WRITE     TIVAW000
012400100409     C                   ENDIF
012500991027     C*
012600080924     C                   READ      tivgdtmp
012700080924     C                   DOW       not %eof(tivgdtmp)
012800080924     c                   movel     vgddta        dsvac
012900991027     C*
013000030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
013100060411     C***  '"':''''      XLATE     dsvac         dsvac
013200110131     C*
013300110131     C                   SETOFF                                       3132
013400110131     C*
013500110131     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
013600110131     C                   if        %subst(prmppt:7:1) = 'S'
013700110131     C                   if        VACNRS = *zeros
013800110131     C                   seton                                        31
013900110131     C                   endif
014000110131     C                   endif
014100110131     C*
014200110131     C* Se richiesto no ritorno CCA x consegna dirottamenti
014300110131     C                   if        %subst(prmppt:8:1) = 'D'
014400110131     C                   seton                                        32
014500110131     C                   endif
014600110131     C*
014700110131     C* Se tutto ok => procedo
014800110131     C                   if        *in31 = *off
014900110131     C*
015000110131     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
015100110131     C                   IF        %subst(prmppt:9:1) = 'O'
015200110131     C*
015300110131     C* Inizializzo i campi d wrk
015400110131     C                   movel     *blanks       wEsito1           1
015500110131     C                   movel     *blanks       wEsito2           1
015600110131     C*
015700110131     C* 1 => no messe in consegna
015800110131     C                   IF        *IN31 = *off
015900110131     C                   IF        VACCCA = 'C'
016000110131     C                   SETON                                        31
016100110131     C                   ENDIF
016200110131     C                   ENDIF
016300110131     C*
016400110131     C* 2 => no bolle figlie
016500110131     C                   IF        *IN31 = *off
016600110131     C* Chiamata metodo GetLblTyp
016700110131     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
016800110131     C                                                VACAAS
016900110131     C                                               :VACLNP
017000110131     C                                               :VACNRS
017100110131     C                                               :VACNSP
017200110131     C                                               :pOutLblTyp
017300110131     C                                               :pOutAnnoBO
017400110131     C                                               :pOutLineaParBO
017500110131     C                                               :pOutSerieBO
017600110131     C                                               :pOutNumSpedBO
017700110131     C                                               :pOutDcmBO
017800110131     C                                               :pOutCcaBO
017900110131     C                                               :pOutRblBO))
018000110131     C* Se trattasi d bolla originale proseguo con le verifiche
018100110131     C                   IF        pOutLblTyp <> 'O'
018200110131     C                   SETON                                        31
018300110131     C                   ENDIF
018400110131     C                   ENDIF
018500110131     C*
018600110131     C* 3 => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
018700110131     C                   IF        *IN31 = *off
018800110131     C                   IF        VACCCA = '1' AND
018900110131     C                             VACDCM = *zeros
019000110131     C                   SETON                                        31
019100110131     C                   ENDIF
019200110131     C                   ENDIF
019300110131     C*
019400110131     C* 4 => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
019500110131     C*      consegnata ultima bolla figlia
019600110131     C                   IF        *IN31 = *off
019700110131     C                   IF        VACDCM > *zeros
019800110131     C                   IF        VACCCA = '1' OR
019900110131     C                             VACCCA = '2' OR
020000110131     C                             VACCCA = '9'
020100110131     C* Inizializzo i campi d output
020200110131     C                   move      *blanks       VACCCA
020300110131     C                   move(p)   '00000000'    VACDCM
020400110131     C* Chiamata metodo GetLastChild
020500110131     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
020600110131     C                                                VACAAS
020700110131     C                                               :VACLNP
020800110131     C                                               :VACNRS
020900110131     C                                               :VACNSP
021000110131     C                                               :pOutAnnoFI
021100110131     C                                               :pOutLineaParFI
021200110131     C                                               :pOutSerieFI
021300110131     C                                               :pOutNumSpedFI
021400110131     C                                               :pOutDcmFI
021500110131     C                                               :pOutCcaFI))
021600110131     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
021700110131     C                   if        pOutDcmFI > *zeros
021800110131     C                   move      pOutCcaFI     VACCCA
021900110131     C                   move      pOutDcmFI     VACDCM
022000110131     C                   else
022100110131     C                   SETON                                        31
022200110131     C                   endif
022300110131     C*
022400110131     C                   if        wEsito1 <> *zeros OR
022500110131     C                             wEsito2 <> *zeros
022600110131 xxx C***                SETON                                        31
022700110131     C                   endif
022800110131     C                   ENDIF
022900110131     C                   ENDIF
023000110131     C                   ENDIF
023100110131     C*
023200110131     C                   ENDIF
023300110131     C*
023400110131     C                   if        *in32
023500110131     C                   if        VACCCA = '1'
023600110131     C                   eval      VACCCA = *blanks
023700110131     C                   endif
023800110131     C                   endif
023900110131     C*
024000110131     C                   endif
024100110131     C*
024200110131     C* Se tutto ok => procedo
024300110131     C                   if        *in31 = *off
024400051007     C*
024500051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
024600051007     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
024700051007     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
024800051007     C*
024900051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
025000040206     C                   IF        %subst(prmppt:2:1) = 'D'
025100110721     C                   EVAL      VAWDTA =
025200100409     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
025300100409     C                                         '"'+%trim(VACRSD)+'"' +SepFld+
025400100409     C                                         '"'+%trim(VACPRD)+'"' +SepFld+
025500100409     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
025600100409     C                                                    %trim(wPKB)+SepFld+
025700100409     C                                      %trim(%editc(VACCCM:'Q'))+SepFld+
025800100409     C                                         '"'+%trim(VACRMA)+'"' +SepFld+
025900100409     C                             %trim(%editw(VACDCM:'    /  /  '))+SepFld+
026000100409     C                                         '"'+%trim(VACCCA)+'"' +SepFld+
026100100409     C                             %trim(%editw(VACDLA:'    /  /  '))+SepFld+
026200051007     C                             %trim(%editw(VACDAG:'    /  /  '))
026300040206     C                   ELSE
026400040206     C*
026500110721     C                   EVAL      VAWDTA =
026600100409     C                                      %trim(%editc(VACMGS:'Q'))+SepFld+
026700100409     C                                          '"'+%trim(VACRSD)+'"'+SepFld+
026800100409     C                                          '"'+%trim(VACPRD)+'"'+SepFld+
026900100409     C                                      %trim(%editc(VACNCL:'Q'))+SepFld+
027000100409     C                                                    %trim(wPKB)+SepFld+
027100100409     C                                      %trim(%editc(VACCCM:'Q'))+SepFld+
027200100409     C                                          '"'+%trim(VACRMA)+'"'+SepFld+
027300100409     C                                      %trim(%editc(VACDCM:'Q'))+SepFld+
027400100409     C                                          '"'+%trim(VACCCA)+'"'+SepFld+
027500100409     C                                      %trim(%editc(VACDLA:'Q'))+SepFld+
027600051007     C                                      %trim(%editc(VACDAG:'Q'))
027700040206     C                   ENDIF
027800070426     C*
027900070426     C* Se richiesto nei parametri d input reperisco i dati mancanti del destinatario
028000070426     C                   IF        %subst(prmppt:5:1) = 'I'
028100070426     C*
028200070426     C* Inizializzo i campi "estensione"
028300070426     C                   EVAL      TASIND = *blanks
028400070426     C                   EVAL      TASCAD = *blanks
028500070426     C                   EVAL      TASLOD = *blanks
028600070426     C*
028700070426     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
028800070426     C     KEYtas30C     CHAIN     titas30c
028900070426     C                   IF        %found(titas30c)
029000070426     C*
029100070426     C* Valorizzo il buffer d output con i dati aggiuntivi richiesti in input
029200110314     C                   EVAL      VAWDTA  = %trim(VAWDTA) + SepFld+
029300100409     C                                          '"'+%trim(TASIND)+'"'+SepFld+
029400100409     C                                          '"'+%trim(TASCAD)+'"'+SepFld+
029500070426     C                                          '"'+%trim(TASLOD)+'"'
029600070426     C                   ENDIF
029700070426     C                   ENDIF
029800930409     C*
029900110314     C  N31              WRITE     TIVAW000
030000110131     C                   endif
030100110131     C*
030200080924     C                   DELETE    tivgd000
030300991027     C*
030400080924     C                   READ      tivgdtmp
030500030325     C                   ENDDO
030600991027     C*
030700030325     C                   EVAL      wrkesito = '0'
030800991027     C*
030900910830     C                   ENDSR
031000991027
031100991027      /TITLE Operazioni iniziali.
031200991027     C     *inzsr        BEGSR
031300991027     C*
031400991027     C     *ENTRY        PLIST
031500991027     C                   parm                    prmppt
031600991027     C     wrkesito      parm      wrkesito      prmesito
031700070426     C*
031800070426     C* Definizione chiavi
031900070426     C*
032000070426     C* Chiave su TITAS30C - Completa
032100070426     C     KEYtas30C     KLIST
032200070426     C                   KFLD                    vacAAS
032300070426     C                   KFLD                    vacLNP
032400070426     C                   KFLD                    vacNRS
032500070426     C                   KFLD                    vacNSP
032600991027     C*
032700991027     C                   ENDSR
