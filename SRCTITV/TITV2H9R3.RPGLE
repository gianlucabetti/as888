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
002600110913
002700110913
002800110913     D*------------------
002900110913     D* LINKING A DEFINIZIONI ESTERNE
003000110913     D*------------------
003100110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003200110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003300110913
003400110810
003500991027
003600030908     C                   EXSR      chkRECFILMBR
003700991027     C                   EXSR      traduci
003800921023     C                   SETON                                        LR
003900030908
004000110810
004100110810
004200030908     C     chkRECFILMBR  BEGSR
004300030908     C*
004400100209     C                   READ      TIVAWWWT                               55
004500030908     C*
004600030908     C                   ENDSR
004700110810
004800110810
004900991027
005000991027     C     traduci       BEGSR
005100030325     C*
005200090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005300140423     C*
005400140423     C* (descrizione campi in italiano)
005500111111     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005600100209     C                   EVAL      VAWDTA =
005700111111     C                             '"Data ritiro"'                      +sepFld+
005800111111     C                             '"Data partenza"'                    +sepFld+
005900111111     C                             '"Rif. mittente numerico"'           +sepFld+
006000111111     C                             '"Rif. mittente alfabetico"'         +sepFld+
006100111111     C                             '"Destinatario"'                     +sepFld+
006200111111     C                             '"CAP destinatario"'                 +sepFld+
006300111111     C                             '"Località destinatario"'            +sepFld+
006400120615     C                             '"Peso Kg."'                         +sepFld+
006500120307     C                             '"Num. sped. BRT"'                   +sepFld+
006600111111     C                             '"Data arrivo"'                      +sepFld+
006700111111     C                             '"Data lasciato avviso"'             +sepFld+
006800111111     C                             '"Data consegna"'                    +sepFld+
006900111111     C                             '"Data reso"'                        +sepFld+
007000111111     C                             '"Giorni sosta"'                     +sepFld+
007100111111     C                             '"Affidabilità"'                     +sepFld
007200090427     C*
007300100209     C                   WRITE     TIVAW000
007400090427     C                   ENDIF
007500140423     C*
007600140423     C* (descrizione campi in inglese)
007700140423     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
007800140423     C                   EVAL      VAWDTA =
007900140423     C                             '"Date of Arrival Departure depot"'  +sepFld+
008000140423     C                             '"Date of Departure"'                +sepFld+
008100140423     C                             '"1st Shipment Ref. ID"'             +sepFld+
008200140423     C                             '"2nd Shipment Ref. ID"'             +sepFld+
008300140423     C                             '"Consignee name"'                   +sepFld+
008400140423     C                             '"Consignee ZIP code"'               +sepFld+
008500140423     C                             '"Consignee city"'                   +sepFld+
008600140423     C                             '"Shipment weight (kg)"'             +sepFld+
008700140423     C                             '"BRT shipment code"'                +sepFld+
008800140423     C                             '"Date of Arrival Arrival depot"'    +sepFld+
008900140423     C                             '"Date of Last Delivery attempt"'    +sepFld+
009000140423     C                             '"Date of Delivery"'                 +sepFld+
009100140423     C                             '"Date of Return"'                   +sepFld+
009200140423     C                             '"Days of holding goods"'            +sepFld+
009300140423     C                             '"Transport lead time (days)"'       +sepFld
009400140423     C*
009500140423     C                   WRITE     TIVAW000
009600140423     C                   ENDIF
009700991027     C*
009800090325     C                   READ      tivgdtmp
009900090325     C                   DOW       not %eof
010000110914     c                   movel     vgddta        fnvacds
010100041220     C*
010200110810     C* Inizializzo i campi d wrk
010300110913     C                   Z-ADD     *zeros        wTLT
010400110913     C                   Z-ADD     *zeros        wGGS
010500110913     C                   Z-ADD     *zeros        wDCMC
010600110913     C                   Z-ADD     *zeros        wDCM2
010700111111     C                   Z-ADD     *zeros        wDLA
010800030709     C*
010900110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
011000110810     C     KEYtas30C_C   CHAIN     titas30c
011100030709     C                   IF        %found(titas30c)
011200110913     C*
011300110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
011400110913     C                   MOVEL     *blanks       wEsito1           1
011500110913     C                   MOVEL     *blanks       wEsito2           1
011600110914     C                   Z-ADD     *zeros        wDataCons         8 0
011700110914     C                   MOVEL     *blanks       wConsAnom         1
011800110913     C*
011900110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
012000110913     C                                                vacAAS
012100110913     C                                               :vacLNP
012200110913     C                                               :vacNRS
012300110913     C                                               :vacNSP
012400110913     C                                               :pOutLblTyp
012500110913     C                                               :pOutAnnoBO
012600110913     C                                               :pOutLineaParBO
012700110913     C                                               :pOutSerieBO
012800110913     C                                               :pOutNumSpedBO
012900110913     C                                               :pOutDcmBO
013000110913     C                                               :pOutCcaBO
013100110913     C                                               :pOutRblBO))
013200111109     C*
013300111109     C* Ritorno solamente le bolle valide come spedizione
013400111109     C                   IF        pOutRblBO = 'N'
013500110913     C*
013600110913     C* Chiamata metodo GetLastChild
013700110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
013800110913     C                                                pOutAnnoBO
013900110913     C                                               :pOutLineaParBO
014000110913     C                                               :pOutSerieBO
014100110913     C                                               :pOutNumSpedBO
014200110913     C                                               :pOutAnnoFI
014300110913     C                                               :pOutLineaParFI
014400110913     C                                               :pOutSerieFI
014500110913     C                                               :pOutNumSpedFI
014600110913     C                                               :pOutDcmFI
014700110913     C                                               :pOutCcaFI))
014800110914     C*
014900110914     C                   IF        wEsito2 = '0'
015000110914     C                   EVAL      wDataCons = pOutDcmFI
015100110914     C                   EVAL      wConsAnom = pOutCcaFI
015200110914     C                   ELSE
015300110914     C                   EVAL      wDataCons = pOutDcmBO
015400110914     C                   EVAL      wConsAnom = pOutCcaBO
015500110914     C                   ENDIF
015600111111     C*
015700111111     C* Quindi aggancio l'ultima giacenza
015800111111     C                   EVAL      gcpFRG = *zeros
015900111111     C     KEYgcp51_C    CHAIN     tigcp51l
016000111111     C                   IF        %found(tigcp51l)
016100111111     C*
016200111111     C* Se giacenza NN annullata la considero
016300111111     C                   IF        gcpATB = *blanks
016400111111     C                   EVAL      wGGS = gcpGGS
016500111111     C                   ENDIF
016600111111     C                   ENDIF
016700111111     C*
016800111111     C* Quindi reperisco l'ultimo lasciato avviso
016900111111     C                   EXSR      REPLAV
017000111111     C*****
017100111111     C* Determino stato attuale della spedizione
017200111111     C*****
017300110913     C                   SELECT
017400110913     C* Consegna OK
017500110914     C                   WHEN      wDataCons > *zeros AND
017600110914     C                             wConsAnom = *blanks
017700110914     C                   EVAL      wDCMC = wDataCons
017800110914     C                   EVAL      wTLT  = tasNRC / 24
017900110913     C* Reso
018000110914     C                   WHEN      wDataCons > *zeros AND
018100110914     C                             wConsAnom = '2'
018200110914     C                   EVAL      wDCM2 = wDataCons
018300110914     C*
018400110913     C                   ENDSL
018500071205     C*
018600110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
018700110914     C  N60              EVAL      wNRC = %trim(%editw(wTLT:'   0 , -'))
018800110914     C   60              EVAL      wNRC = %trim(%editw(wTLT:'   0 . -'))
018900120615     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
019000120615     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
019100110810     C*
019200110810     C                   EVAL      VAWDTA  =
019300110913     C                             %trim(%editc(TASDRT:'Q'))            +sepFld+
019400110913     C                             %trim(%editc(TASDUC:'Q'))            +sepFld+
019500110914     C                             %trim(%editc(VACRMN:'Q'))            +sepFld+
019600111223     C                             %trim(vacRMA)                        +sepFld+
019700110810     C                             %trim(tasRSD)                        +sepFld+
019800110810     C                             %trim(tasCAD)                        +sepFld+
019900110810     C                             %trim(tasLOD)                        +sepFld+
020000120615     C                             %trim(wPKB)                          +sepFld+
020100110913     C                             %trim(%char(tasAAS))+'/'+
020200110913     C                             %trim(%char(tasLNP))+'/'+
020300110913     C                             %trim(%char(tasNRS))+'/'+
020400110913     C                             %trim(%char(tasNSP))                 +sepFld+
020500110913     C                             %trim(%editc(tasDTI:'Q'))            +sepFld+
020600111111     C                             %trim(%editc(wDLA:'Q'))              +sepFld+
020700110914     C                             %trim(%editc(wDCMC:'Q'))             +sepFld+
020800110914     C                             %trim(%editc(wDCM2:'Q'))             +sepFld+
020900110913     C                             %trim(%editc(wGGS:'Q'))              +sepFld+
021000110913     C                             %trim(wNRC)                          +sepFld
021100930409     C*
021200100209     C                   WRITE     TIVAW000
021300110810     C                   ENDIF
021400111109     C*
021500111109     C                   ENDIF
021600110810     C*
021700090325     C                   delete    tivgd000
021800991027     C*
021900090325     C                   READ      tivgdtmp
022000030325     C                   ENDDO
022100991027     C*
022200030325     C                   EVAL      wrkesito = '0'
022300991027     C*
022400910830     C                   ENDSR
022500111111
022600111111
022700111111
022800111111     C*------------------------------------------------------------------------*
022900111111     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
023000111111     C*------------------------------------------------------------------------*
023100111111     C     REPLAV        BEGSR
023200111111     C*
023300111111     C* Pererisco l'ultimo evento Lasciato Avviso
023400111111     C                   MOVEL     '1'           tblKUT
023500111111     C                   MOVEL     '2A'          tblCOD
023600111111     C     KEYtas30C_C   SETGT     FNEVB01L
023700111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
023800111111     C*
023900111111     C     *IN34         DOWEQ     *OFF
024000111111     C                   MOVEL(P)  EVBCEV        tblKEY
024100111111     C     KEYtab00_C    CHAIN     TABEL00F                           30
024200111111     C                   IF        %found(tabel00f)
024300111111     C                   MOVEL     TBLUNI        DS2A
024400111111     C     §2AFTC        IFEQ      'A'
024500111111     C                   Z-ADD     EVBDEV        wDLA
024600111111     C                   LEAVE
024700111111     C                   ENDIF
024800111111     C                   ENDIF
024900111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
025000111111     C                   ENDDO
025100111111     C*
025200111111     C                   ENDSR
025300111111     C*------------------------------------------------------------------------*
025400111111
025500110810
025600110810
025700991027      /TITLE Operazioni iniziali.
025800991027     C     *inzsr        BEGSR
025900991027     C*
026000991027     C     *ENTRY        PLIST
026100991027     C                   parm                    prmppt
026200991027     C     wrkesito      parm      wrkesito      prmesito
026300070316     C*
026400070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
026500070316     C                   eval      sepFld = %subst(prmppt:2:1)
026600070316     C                   if        sepFld = *blanks
026700070316     C                   eval      sepFld = ','
026800070316     C                   endif
026900071205     C*
027000071205     C* Verifico se richiesto un particolare carattere d separatore decimale
027100071205     C                   SETON                                        60
027200071205     C                   IF        %subst(prmppt:3:1) = ','
027300071205     C                   SETOFF                                       60
027400071205     C                   ENDIF
027500030709     C*
027600030709     C* Definizione chiavi
027700030709     C*
027800030709     C* Chiave su TITAS30C - Completa
027900110810     C     KEYtas30C_C   KLIST
028000110913     C                   KFLD                    vacAAS
028100110913     C                   KFLD                    vacLNP
028200110913     C                   KFLD                    vacNRS
028300110913     C                   KFLD                    vacNSP
028400110913     C*
028500110913     C* Chiave su TIGCP51L - Completa
028600110913     C     KEYgcp51_C    KLIST
028700110913     C                   KFLD                    vacAAS
028800110913     C                   KFLD                    vacLNP
028900110913     C                   KFLD                    vacNRS
029000110913     C                   KFLD                    vacNSP
029100110913     C                   KFLD                    gcpFRG
029200111111     C*
029300111111     C     KEYtab00_C    KLIST
029400111111     C                   KFLD                    tblKUT
029500111111     C                   KFLD                    tblCOD
029600111111     C                   KFLD                    tblKEY
029700110810     C*
029800991027     C                   ENDSR
