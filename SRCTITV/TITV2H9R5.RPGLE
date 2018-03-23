000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400111111     FTABEL00F  IF   E           K DISK
000500030709     FTITAS30C  IF   E           K DISK
000600110913     FTIGCP51L  IF   E           K DISK
000700111111     FFNEVB01L  IF   E           K DISK
000800090325     Ftivgdtmp  uF   E             DISK
000900100209     FTIVAWWWT  UF A E             DISK    commit
001000991027
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400111111     D fnvacds       e ds                  extname(fnvac00t) inz
001500111111     D ds2A          e ds                  inz
001600070316     D sepFld          s              1
001700110810
001800110810
001900110913     D wTLT            s              5S 1 inz
002000110913     D wGGS            s                   inz like(gcpGGS)
002100110913     D wDCMC           s                   inz like(vacDCM)
002200110913     D wDCM2           s                   inz like(vacDCM)
002300111111     D wDLA            s                   inz like(vacDLA)
002400110913     D wNRC            s              6    inz
002500120615     D wPKB            s              8    inz
002600141222     D wCAS            s             14    inz
002700110913
002800110913
002900110913     D*------------------
003000110913     D* LINKING A DEFINIZIONI ESTERNE
003100110913     D*------------------
003200110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003300110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003400110913
003500110810
003600991027
003700030908     C                   EXSR      chkRECFILMBR
003800991027     C                   EXSR      traduci
003900921023     C                   SETON                                        LR
004000030908
004100110810
004200110810
004300030908     C     chkRECFILMBR  BEGSR
004400030908     C*
004500100209     C                   READ      TIVAWWWT                               55
004600030908     C*
004700030908     C                   ENDSR
004800110810
004900110810
005000991027
005100991027     C     traduci       BEGSR
005200030325     C*
005300090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005400140423     C*
005500140423     C* (descrizione campi in italiano)
005600111111     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005700100209     C                   EVAL      VAWDTA =
005800111111     C                             '"Data ritiro"'                      +sepFld+
005900111111     C                             '"Data partenza"'                    +sepFld+
006000111111     C                             '"Rif. mittente numerico"'           +sepFld+
006100111111     C                             '"Rif. mittente alfabetico"'         +sepFld+
006200111111     C                             '"Destinatario"'                     +sepFld+
006300111111     C                             '"CAP destinatario"'                 +sepFld+
006400111111     C                             '"Località destinatario"'            +sepFld+
006500141222     C                             '"Provincia destinatario"'           +sepFld+
006600120615     C                             '"Peso Kg."'                         +sepFld+
006700141222     C                             '"Importo contrassegno"'             +sepFld+
006800120307     C                             '"Num. sped. BRT"'                   +sepFld+
006900111111     C                             '"Data arrivo"'                      +sepFld+
007000111111     C                             '"Data lasciato avviso"'             +sepFld+
007100111111     C                             '"Data consegna"'                    +sepFld+
007200111111     C                             '"Data reso"'                        +sepFld+
007300111111     C                             '"Giorni sosta"'                     +sepFld+
007400111111     C                             '"Affidabilità"'                     +sepFld
007500090427     C*
007600100209     C                   WRITE     TIVAW000
007700090427     C                   ENDIF
007800140423     C*
007900140423     C* (descrizione campi in inglese)
008000140423     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
008100140423     C                   EVAL      VAWDTA =
008200140423     C                             '"Date of Arrival Departure depot"'  +sepFld+
008300140423     C                             '"Date of Departure"'                +sepFld+
008400140423     C                             '"1st Shipment Ref. ID"'             +sepFld+
008500140423     C                             '"2nd Shipment Ref. ID"'             +sepFld+
008600140423     C                             '"Consignee name"'                   +sepFld+
008700140423     C                             '"Consignee ZIP code"'               +sepFld+
008800140423     C                             '"Consignee city"'                   +sepFld+
008900141222     C                             '"Consignee province code"'          +sepFld+
009000140423     C                             '"Shipment weight (kg)"'             +sepFld+
009100141222     C                             '"COD amount"'                       +sepFld+
009200140423     C                             '"BRT shipment code"'                +sepFld+
009300140423     C                             '"Date of Arrival Arrival depot"'    +sepFld+
009400140423     C                             '"Date of Last Delivery attempt"'    +sepFld+
009500140423     C                             '"Date of Delivery"'                 +sepFld+
009600140423     C                             '"Date of Return"'                   +sepFld+
009700140423     C                             '"Days of holding goods"'            +sepFld+
009800140423     C                             '"Transport lead time (days)"'       +sepFld
009900140423     C*
010000140423     C                   WRITE     TIVAW000
010100140423     C                   ENDIF
010200991027     C*
010300090325     C                   READ      tivgdtmp
010400090325     C                   DOW       not %eof
010500110914     c                   movel     vgddta        fnvacds
010600041220     C*
010700110810     C* Inizializzo i campi d wrk
010800110913     C                   Z-ADD     *zeros        wTLT
010900110913     C                   Z-ADD     *zeros        wGGS
011000110913     C                   Z-ADD     *zeros        wDCMC
011100110913     C                   Z-ADD     *zeros        wDCM2
011200111111     C                   Z-ADD     *zeros        wDLA
011300030709     C*
011400110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
011500110810     C     KEYtas30C_C   CHAIN     titas30c
011600030709     C                   IF        %found(titas30c)
011700110913     C*
011800110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
011900110913     C                   MOVEL     *blanks       wEsito1           1
012000110913     C                   MOVEL     *blanks       wEsito2           1
012100110914     C                   Z-ADD     *zeros        wDataCons         8 0
012200110914     C                   MOVEL     *blanks       wConsAnom         1
012300110913     C*
012400110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
012500110913     C                                                vacAAS
012600110913     C                                               :vacLNP
012700110913     C                                               :vacNRS
012800110913     C                                               :vacNSP
012900110913     C                                               :pOutLblTyp
013000110913     C                                               :pOutAnnoBO
013100110913     C                                               :pOutLineaParBO
013200110913     C                                               :pOutSerieBO
013300110913     C                                               :pOutNumSpedBO
013400110913     C                                               :pOutDcmBO
013500110913     C                                               :pOutCcaBO
013600110913     C                                               :pOutRblBO))
013700111109     C*
013800111109     C* Ritorno solamente le bolle valide come spedizione
013900111109     C                   IF        pOutRblBO = 'N'
014000110913     C*
014100110913     C* Chiamata metodo GetLastChild
014200110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
014300110913     C                                                pOutAnnoBO
014400110913     C                                               :pOutLineaParBO
014500110913     C                                               :pOutSerieBO
014600110913     C                                               :pOutNumSpedBO
014700110913     C                                               :pOutAnnoFI
014800110913     C                                               :pOutLineaParFI
014900110913     C                                               :pOutSerieFI
015000110913     C                                               :pOutNumSpedFI
015100110913     C                                               :pOutDcmFI
015200110913     C                                               :pOutCcaFI))
015300110914     C*
015400110914     C                   IF        wEsito2 = '0'
015500110914     C                   EVAL      wDataCons = pOutDcmFI
015600110914     C                   EVAL      wConsAnom = pOutCcaFI
015700110914     C                   ELSE
015800110914     C                   EVAL      wDataCons = pOutDcmBO
015900110914     C                   EVAL      wConsAnom = pOutCcaBO
016000110914     C                   ENDIF
016100111111     C*
016200111111     C* Quindi aggancio l'ultima giacenza
016300111111     C                   EVAL      gcpFRG = *zeros
016400111111     C     KEYgcp51_C    CHAIN     tigcp51l
016500111111     C                   IF        %found(tigcp51l)
016600111111     C*
016700111111     C* Se giacenza NN annullata la considero
016800111111     C                   IF        gcpATB = *blanks
016900111111     C                   EVAL      wGGS = gcpGGS
017000111111     C                   ENDIF
017100111111     C                   ENDIF
017200111111     C*
017300111111     C* Quindi reperisco l'ultimo lasciato avviso
017400111111     C                   EXSR      REPLAV
017500111111     C*****
017600111111     C* Determino stato attuale della spedizione
017700111111     C*****
017800110913     C                   SELECT
017900110913     C* Consegna OK
018000110914     C                   WHEN      wDataCons > *zeros AND
018100110914     C                             wConsAnom = *blanks
018200110914     C                   EVAL      wDCMC = wDataCons
018300110914     C                   EVAL      wTLT  = tasNRC / 24
018400110913     C* Reso
018500110914     C                   WHEN      wDataCons > *zeros AND
018600110914     C                             wConsAnom = '2'
018700110914     C                   EVAL      wDCM2 = wDataCons
018800110914     C*
018900110913     C                   ENDSL
019000071205     C*
019100110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
019200110914     C  N60              EVAL      wNRC = %trim(%editw(wTLT:'   0 , -'))
019300110914     C   60              EVAL      wNRC = %trim(%editw(wTLT:'   0 . -'))
019400120615     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
019500120615     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
019600141222     C  N60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 ,   '))
019700141222     C   60              EVAL      wCAS = %trim(%editw(VACCAS:'        0 .   '))
019800110810     C*
019900110810     C                   EVAL      VAWDTA  =
020000110913     C                             %trim(%editc(TASDRT:'Q'))            +sepFld+
020100110913     C                             %trim(%editc(TASDUC:'Q'))            +sepFld+
020200110914     C                             %trim(%editc(VACRMN:'Q'))            +sepFld+
020300111223     C                             %trim(vacRMA)                        +sepFld+
020400110810     C                             %trim(tasRSD)                        +sepFld+
020500110810     C                             %trim(tasCAD)                        +sepFld+
020600110810     C                             %trim(tasLOD)                        +sepFld+
020700141222     C                             %trim(tasPRD)                        +sepFld+
020800120615     C                             %trim(wPKB)                          +sepFld+
020900141222     C                             %trim(wCAS)                          +sepFld+
021000110913     C                             %trim(%char(tasAAS))+'/'+
021100110913     C                             %trim(%char(tasLNP))+'/'+
021200110913     C                             %trim(%char(tasNRS))+'/'+
021300110913     C                             %trim(%char(tasNSP))                 +sepFld+
021400110913     C                             %trim(%editc(tasDTI:'Q'))            +sepFld+
021500111111     C                             %trim(%editc(wDLA:'Q'))              +sepFld+
021600110914     C                             %trim(%editc(wDCMC:'Q'))             +sepFld+
021700110914     C                             %trim(%editc(wDCM2:'Q'))             +sepFld+
021800110913     C                             %trim(%editc(wGGS:'Q'))              +sepFld+
021900110913     C                             %trim(wNRC)                          +sepFld
022000930409     C*
022100100209     C                   WRITE     TIVAW000
022200110810     C                   ENDIF
022300111109     C*
022400111109     C                   ENDIF
022500110810     C*
022600090325     C                   delete    tivgd000
022700991027     C*
022800090325     C                   READ      tivgdtmp
022900030325     C                   ENDDO
023000991027     C*
023100030325     C                   EVAL      wrkesito = '0'
023200991027     C*
023300910830     C                   ENDSR
023400111111
023500111111
023600111111
023700111111     C*------------------------------------------------------------------------*
023800111111     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
023900111111     C*------------------------------------------------------------------------*
024000111111     C     REPLAV        BEGSR
024100111111     C*
024200111111     C* Pererisco l'ultimo evento Lasciato Avviso
024300111111     C                   MOVEL     '1'           tblKUT
024400111111     C                   MOVEL     '2A'          tblCOD
024500111111     C     KEYtas30C_C   SETGT     FNEVB01L
024600111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
024700111111     C*
024800111111     C     *IN34         DOWEQ     *OFF
024900111111     C                   MOVEL(P)  EVBCEV        tblKEY
025000111111     C     KEYtab00_C    CHAIN     TABEL00F                           30
025100111111     C                   IF        %found(tabel00f)
025200111111     C                   MOVEL     TBLUNI        DS2A
025300111111     C     §2AFTC        IFEQ      'A'
025400111111     C                   Z-ADD     EVBDEV        wDLA
025500111111     C                   LEAVE
025600111111     C                   ENDIF
025700111111     C                   ENDIF
025800111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
025900111111     C                   ENDDO
026000111111     C*
026100111111     C                   ENDSR
026200111111     C*------------------------------------------------------------------------*
026300111111
026400110810
026500110810
026600991027      /TITLE Operazioni iniziali.
026700991027     C     *inzsr        BEGSR
026800991027     C*
026900991027     C     *ENTRY        PLIST
027000991027     C                   parm                    prmppt
027100991027     C     wrkesito      parm      wrkesito      prmesito
027200070316     C*
027300070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
027400070316     C                   eval      sepFld = %subst(prmppt:2:1)
027500070316     C                   if        sepFld = *blanks
027600070316     C                   eval      sepFld = ','
027700070316     C                   endif
027800071205     C*
027900071205     C* Verifico se richiesto un particolare carattere d separatore decimale
028000071205     C                   SETON                                        60
028100071205     C                   IF        %subst(prmppt:3:1) = ','
028200071205     C                   SETOFF                                       60
028300071205     C                   ENDIF
028400030709     C*
028500030709     C* Definizione chiavi
028600030709     C*
028700030709     C* Chiave su TITAS30C - Completa
028800110810     C     KEYtas30C_C   KLIST
028900110913     C                   KFLD                    vacAAS
029000110913     C                   KFLD                    vacLNP
029100110913     C                   KFLD                    vacNRS
029200110913     C                   KFLD                    vacNSP
029300110913     C*
029400110913     C* Chiave su TIGCP51L - Completa
029500110913     C     KEYgcp51_C    KLIST
029600110913     C                   KFLD                    vacAAS
029700110913     C                   KFLD                    vacLNP
029800110913     C                   KFLD                    vacNRS
029900110913     C                   KFLD                    vacNSP
030000110913     C                   KFLD                    gcpFRG
030100111111     C*
030200111111     C     KEYtab00_C    KLIST
030300111111     C                   KFLD                    tblKUT
030400111111     C                   KFLD                    tblCOD
030500111111     C                   KFLD                    tblKEY
030600110810     C*
030700991027     C                   ENDSR
