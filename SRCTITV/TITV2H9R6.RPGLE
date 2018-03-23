000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400111111     FTABEL00F  IF   E           K DISK
000500030709     FTITAS30C  IF   E           K DISK
000600160311     FTITAA30C  IF   E           K DISK
000700110913     FTIGCP51L  IF   E           K DISK
000800111111     FFNEVB01L  IF   E           K DISK
000900090325     Ftivgdtmp  uF   E             DISK
001000100209     FTIVAWWWT  UF A E             DISK    commit
001100991027
001200991027     D prmppt          s             50
001300991027     D prmesito        s              1
001400991027     D wrkesito        s                   like(prmesito)
001500111111     D fnvacds       e ds                  extname(fnvac00t) inz
001600111111     D ds2A          e ds                  inz
001700070316     D sepFld          s              1
001800110810
001900110810
002000110913     D wTLT            s              5S 1 inz
002100110913     D wGGS            s                   inz like(gcpGGS)
002200110913     D wDCMC           s                   inz like(vacDCM)
002300110913     D wDCM2           s                   inz like(vacDCM)
002400111111     D wDLA            s                   inz like(vacDLA)
002500110913     D wNRC            s              6    inz
002600120615     D wPKB            s              8    inz
002700160311     D wRSC            s                   inz like(taaRSC)
002800110913
002900110913
003000110913     D*------------------
003100110913     D* LINKING A DEFINIZIONI ESTERNE
003200110913     D*------------------
003300110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003400110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003500110913
003600110810
003700991027
003800030908     C                   EXSR      chkRECFILMBR
003900991027     C                   EXSR      traduci
004000921023     C                   SETON                                        LR
004100030908
004200110810
004300110810
004400030908     C     chkRECFILMBR  BEGSR
004500030908     C*
004600100209     C                   READ      TIVAWWWT                               55
004700030908     C*
004800030908     C                   ENDSR
004900110810
005000110810
005100991027
005200991027     C     traduci       BEGSR
005300030325     C*
005400090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005500140423     C*
005600140423     C* (descrizione campi in italiano)
005700111111     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005800100209     C                   EVAL      VAWDTA =
005900111111     C                             '"Data ritiro"'                      +sepFld+
006000111111     C                             '"Data partenza"'                    +sepFld+
006100160311     C                             '"Data consegan richiesta"'          +sepFld+
006200111111     C                             '"Rif. mittente numerico"'           +sepFld+
006300111111     C                             '"Rif. mittente alfabetico"'         +sepFld+
006400160311     C                             '"Tipo Porto F=Franco A=Assegnato"'  +sepFld+
006500160311     C                             '"Mittente originale"'               +sepFld+
006600111111     C                             '"Destinatario"'                     +sepFld+
006700111111     C                             '"CAP destinatario"'                 +sepFld+
006800111111     C                             '"Località destinatario"'            +sepFld+
006900120615     C                             '"Peso Kg."'                         +sepFld+
007000120307     C                             '"Num. sped. BRT"'                   +sepFld+
007100111111     C                             '"Data arrivo"'                      +sepFld+
007200111111     C                             '"Data lasciato avviso"'             +sepFld+
007300111111     C                             '"Data consegna"'                    +sepFld+
007400111111     C                             '"Data reso"'                        +sepFld+
007500111111     C                             '"Giorni sosta"'                     +sepFld+
007600111111     C                             '"Affidabilità"'                     +sepFld
007700090427     C*
007800100209     C                   WRITE     TIVAW000
007900090427     C                   ENDIF
008000140423     C*
008100140423     C* (descrizione campi in inglese)
008200140423     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
008300140423     C                   EVAL      VAWDTA =
008400140423     C                             '"Date of Arrival Departure depot"'  +sepFld+
008500140423     C                             '"Date of Departure"'                +sepFld+
008600160311     C                             '"Requested Delivery Date"'          +sepFld+
008700140423     C                             '"1st Shipment Ref. ID"'             +sepFld+
008800140423     C                             '"2nd Shipment Ref. ID"'             +sepFld+
008900160311     C                             '"Incoterms F=Free-Domicile A=Ex-Works"'
009000160311     C                                                                  +sepFld+
009100160311     C                             '"Original Sender"'                  +sepFld+
009200140423     C                             '"Consignee name"'                   +sepFld+
009300140423     C                             '"Consignee ZIP code"'               +sepFld+
009400140423     C                             '"Consignee city"'                   +sepFld+
009500140423     C                             '"Shipment weight (kg)"'             +sepFld+
009600140423     C                             '"BRT shipment code"'                +sepFld+
009700140423     C                             '"Date of Arrival Arrival depot"'    +sepFld+
009800140423     C                             '"Date of Last Delivery attempt"'    +sepFld+
009900140423     C                             '"Date of Delivery"'                 +sepFld+
010000140423     C                             '"Date of Return"'                   +sepFld+
010100140423     C                             '"Days of holding goods"'            +sepFld+
010200140423     C                             '"Transport lead time (days)"'       +sepFld
010300140423     C*
010400140423     C                   WRITE     TIVAW000
010500140423     C                   ENDIF
010600991027     C*
010700090325     C                   READ      tivgdtmp
010800090325     C                   DOW       not %eof
010900110914     c                   movel     vgddta        fnvacds
011000041220     C*
011100110810     C* Inizializzo i campi d wrk
011200110913     C                   Z-ADD     *zeros        wTLT
011300110913     C                   Z-ADD     *zeros        wGGS
011400110913     C                   Z-ADD     *zeros        wDCMC
011500110913     C                   Z-ADD     *zeros        wDCM2
011600111111     C                   Z-ADD     *zeros        wDLA
011700160311     C                   CLEAR                   wRSC
011800030709     C*
011900110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
012000110810     C     KEYtas30C_C   CHAIN     titas30c
012100030709     C                   IF        %found(titas30c)
012200110913     C*
012300110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
012400110913     C                   MOVEL     *blanks       wEsito1           1
012500110913     C                   MOVEL     *blanks       wEsito2           1
012600110914     C                   Z-ADD     *zeros        wDataCons         8 0
012700110914     C                   MOVEL     *blanks       wConsAnom         1
012800110913     C*
012900110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
013000110913     C                                                vacAAS
013100110913     C                                               :vacLNP
013200110913     C                                               :vacNRS
013300110913     C                                               :vacNSP
013400110913     C                                               :pOutLblTyp
013500110913     C                                               :pOutAnnoBO
013600110913     C                                               :pOutLineaParBO
013700110913     C                                               :pOutSerieBO
013800110913     C                                               :pOutNumSpedBO
013900110913     C                                               :pOutDcmBO
014000110913     C                                               :pOutCcaBO
014100110913     C                                               :pOutRblBO))
014200111109     C*
014300111109     C* Ritorno solamente le bolle valide come spedizione
014400111109     C                   IF        pOutRblBO = 'N'
014500110913     C*
014600110913     C* Chiamata metodo GetLastChild
014700110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
014800110913     C                                                pOutAnnoBO
014900110913     C                                               :pOutLineaParBO
015000110913     C                                               :pOutSerieBO
015100110913     C                                               :pOutNumSpedBO
015200110913     C                                               :pOutAnnoFI
015300110913     C                                               :pOutLineaParFI
015400110913     C                                               :pOutSerieFI
015500110913     C                                               :pOutNumSpedFI
015600110913     C                                               :pOutDcmFI
015700110913     C                                               :pOutCcaFI))
015800110914     C*
015900110914     C                   IF        wEsito2 = '0'
016000110914     C                   EVAL      wDataCons = pOutDcmFI
016100110914     C                   EVAL      wConsAnom = pOutCcaFI
016200110914     C                   ELSE
016300110914     C                   EVAL      wDataCons = pOutDcmBO
016400110914     C                   EVAL      wConsAnom = pOutCcaBO
016500110914     C                   ENDIF
016600111111     C*
016700111111     C* Quindi aggancio l'ultima giacenza
016800111111     C                   EVAL      gcpFRG = *zeros
016900111111     C     KEYgcp51_C    CHAIN     tigcp51l
017000111111     C                   IF        %found(tigcp51l)
017100111111     C*
017200111111     C* Se giacenza NN annullata la considero
017300111111     C                   IF        gcpATB = *blanks
017400111111     C                   EVAL      wGGS = gcpGGS
017500111111     C                   ENDIF
017600111111     C                   ENDIF
017700160311     C*
017800160311     C* X reperire i dati del mittente originale aggancio sempre il TITAA con tipo record = 'O'
017900160311     C                   EVAL      taaTRC = 'O'
018000160311     C     KEYtaa30C_C   CHAIN     titaa30c
018100160311     C                   IF        %found(titaa30c)
018200160311     C                   EVAL      wRSC = taaRSC
018300160311     C                   ENDIF
018400111111     C*
018500111111     C* Quindi reperisco l'ultimo lasciato avviso
018600160311     C                   EXSR      REPLAV
018700111111     C*****
018800111111     C* Determino stato attuale della spedizione
018900111111     C*****
019000110913     C                   SELECT
019100110913     C* Consegna OK
019200110914     C                   WHEN      wDataCons > *zeros AND
019300110914     C                             wConsAnom = *blanks
019400110914     C                   EVAL      wDCMC = wDataCons
019500110914     C                   EVAL      wTLT  = tasNRC / 24
019600110913     C* Reso
019700110914     C                   WHEN      wDataCons > *zeros AND
019800110914     C                             wConsAnom = '2'
019900110914     C                   EVAL      wDCM2 = wDataCons
020000110914     C*
020100110913     C                   ENDSL
020200071205     C*
020300110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
020400110914     C  N60              EVAL      wNRC = %trim(%editw(wTLT:'   0 , -'))
020500110914     C   60              EVAL      wNRC = %trim(%editw(wTLT:'   0 . -'))
020600120615     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
020700120615     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
020800110810     C*
020900110810     C                   EVAL      VAWDTA  =
021000110913     C                             %trim(%editc(TASDRT:'Q'))            +sepFld+
021100110913     C                             %trim(%editc(TASDUC:'Q'))            +sepFld+
021200160311     C                             %trim(%editc(TASDCR:'Q'))            +sepFld+
021300110914     C                             %trim(%editc(VACRMN:'Q'))            +sepFld+
021400111223     C                             %trim(vacRMA)                        +sepFld+
021500160311     C                             %trim(%subst(TASTBL:1:1))            +sepFld+
021600160311     C                             %trim(wRSC)                          +sepFld+
021700110810     C                             %trim(tasRSD)                        +sepFld+
021800110810     C                             %trim(tasCAD)                        +sepFld+
021900110810     C                             %trim(tasLOD)                        +sepFld+
022000120615     C                             %trim(wPKB)                          +sepFld+
022100110913     C                             %trim(%char(tasAAS))+'/'+
022200110913     C                             %trim(%char(tasLNP))+'/'+
022300110913     C                             %trim(%char(tasNRS))+'/'+
022400110913     C                             %trim(%char(tasNSP))                 +sepFld+
022500110913     C                             %trim(%editc(tasDTI:'Q'))            +sepFld+
022600111111     C                             %trim(%editc(wDLA:'Q'))              +sepFld+
022700110914     C                             %trim(%editc(wDCMC:'Q'))             +sepFld+
022800110914     C                             %trim(%editc(wDCM2:'Q'))             +sepFld+
022900110913     C                             %trim(%editc(wGGS:'Q'))              +sepFld+
023000110913     C                             %trim(wNRC)                          +sepFld
023100930409     C*
023200100209     C                   WRITE     TIVAW000
023300110810     C                   ENDIF
023400111109     C*
023500111109     C                   ENDIF
023600110810     C*
023700090325     C                   delete    tivgd000
023800991027     C*
023900090325     C                   READ      tivgdtmp
024000030325     C                   ENDDO
024100991027     C*
024200030325     C                   EVAL      wrkesito = '0'
024300991027     C*
024400910830     C                   ENDSR
024500111111
024600111111
024700111111
024800111111     C*------------------------------------------------------------------------*
024900111111     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
025000111111     C*------------------------------------------------------------------------*
025100111111     C     REPLAV        BEGSR
025200111111     C*
025300111111     C* Pererisco l'ultimo evento Lasciato Avviso
025400111111     C                   MOVEL     '1'           tblKUT
025500111111     C                   MOVEL     '2A'          tblCOD
025600111111     C     KEYtas30C_C   SETGT     FNEVB01L
025700111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
025800111111     C*
025900111111     C     *IN34         DOWEQ     *OFF
026000111111     C                   MOVEL(P)  EVBCEV        tblKEY
026100111111     C     KEYtab00_C    CHAIN     TABEL00F                           30
026200111111     C                   IF        %found(tabel00f)
026300111111     C                   MOVEL     TBLUNI        DS2A
026400111111     C     §2AFTC        IFEQ      'A'
026500111111     C                   Z-ADD     EVBDEV        wDLA
026600111111     C                   LEAVE
026700111111     C                   ENDIF
026800111111     C                   ENDIF
026900111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
027000111111     C                   ENDDO
027100111111     C*
027200111111     C                   ENDSR
027300111111     C*------------------------------------------------------------------------*
027400111111
027500110810
027600110810
027700991027      /TITLE Operazioni iniziali.
027800991027     C     *inzsr        BEGSR
027900991027     C*
028000991027     C     *ENTRY        PLIST
028100991027     C                   parm                    prmppt
028200991027     C     wrkesito      parm      wrkesito      prmesito
028300070316     C*
028400070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
028500070316     C                   eval      sepFld = %subst(prmppt:2:1)
028600070316     C                   if        sepFld = *blanks
028700070316     C                   eval      sepFld = ','
028800070316     C                   endif
028900071205     C*
029000071205     C* Verifico se richiesto un particolare carattere d separatore decimale
029100071205     C                   SETON                                        60
029200071205     C                   IF        %subst(prmppt:3:1) = ','
029300071205     C                   SETOFF                                       60
029400071205     C                   ENDIF
029500030709     C*
029600030709     C* Definizione chiavi
029700030709     C*
029800030709     C* Chiave su TITAS30C - Completa
029900110810     C     KEYtas30C_C   KLIST
030000110913     C                   KFLD                    vacAAS
030100110913     C                   KFLD                    vacLNP
030200110913     C                   KFLD                    vacNRS
030300110913     C                   KFLD                    vacNSP
030400160311     C*
030500160311     C* Chiave su TITAA30C - Completa
030600160311     C     KEYtaa30C_C   KLIST
030700160311     C                   KFLD                    vacAAS
030800160311     C                   KFLD                    vacLNP
030900160311     C                   KFLD                    vacNRS
031000160311     C                   KFLD                    vacNSP
031100160311     C                   KFLD                    taaTRC
031200110913     C*
031300110913     C* Chiave su TIGCP51L - Completa
031400110913     C     KEYgcp51_C    KLIST
031500110913     C                   KFLD                    vacAAS
031600110913     C                   KFLD                    vacLNP
031700110913     C                   KFLD                    vacNRS
031800110913     C                   KFLD                    vacNSP
031900110913     C                   KFLD                    gcpFRG
032000111111     C*
032100111111     C     KEYtab00_C    KLIST
032200111111     C                   KFLD                    tblKUT
032300111111     C                   KFLD                    tblCOD
032400111111     C                   KFLD                    tblKEY
032500110810     C*
032600991027     C                   ENDSR
