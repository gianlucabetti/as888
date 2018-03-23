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
002600140508     D wDataSts        S              8  0 inz(*zeros)
002700140508     D wConsOkSiNo     S              1    inz(*blanks)
002800140508     D wConsOkSiNoDes  S            300    inz(*blanks)
002900140508     D wNoteGiac       s            300    inz(*blanks)
003000140508     D wFIRMATAR       s             35    inz(*blanks)
003100110913
003200110913
003300110913     D*------------------
003400110913     D* LINKING A DEFINIZIONI ESTERNE
003500110913     D*------------------
003600110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003700110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003800140508     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003900140508     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
004000110913
004100110810
004200991027
004300030908     C                   EXSR      chkRECFILMBR
004400991027     C                   EXSR      traduci
004500921023     C                   SETON                                        LR
004600030908
004700110810
004800110810
004900030908     C     chkRECFILMBR  BEGSR
005000030908     C*
005100100209     C                   READ      TIVAWWWT                               55
005200030908     C*
005300030908     C                   ENDSR
005400110810
005500110810
005600991027
005700991027     C     traduci       BEGSR
005800030325     C*
005900090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
006000140423     C*
006100140423     C* (descrizione campi in italiano)
006200111111     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
006300100209     C                   EVAL      VAWDTA =
006400111111     C                             '"Data ritiro"'                      +sepFld+
006500111111     C                             '"Data partenza"'                    +sepFld+
006600111111     C                             '"Rif. mittente numerico"'           +sepFld+
006700111111     C                             '"Rif. mittente alfabetico"'         +sepFld+
006800111111     C                             '"Destinatario"'                     +sepFld+
006900111111     C                             '"CAP destinatario"'                 +sepFld+
007000111111     C                             '"Località destinatario"'            +sepFld+
007100140509     C                             '"Colli"'                            +sepFld+
007200120615     C                             '"Peso Kg."'                         +sepFld+
007300120307     C                             '"Num. sped. BRT"'                   +sepFld+
007400111111     C                             '"Data arrivo"'                      +sepFld+
007500111111     C                             '"Data lasciato avviso"'             +sepFld+
007600140509     C                             '"Stato corrente"'                   +sepFld+
007700140509     C                             '"Data stato"'                       +sepFld+
007800140509 xxx C***                          '"Data reso"'                        +sepFld+
007900140508     C                             '"Giorni sosta"'                     +sepFld
008000140508 xxx C***                          '"Affidabilità"'                     +sepFld
008100090427     C*
008200100209     C                   WRITE     TIVAW000
008300090427     C                   ENDIF
008400140423     C*
008500140423     C* (descrizione campi in inglese)
008600140423     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
008700140423     C                   EVAL      VAWDTA =
008800140423     C                             '"Date of Arrival Departure depot"'  +sepFld+
008900140423     C                             '"Date of Departure"'                +sepFld+
009000140423     C                             '"1st Shipment Ref. ID"'             +sepFld+
009100140423     C                             '"2nd Shipment Ref. ID"'             +sepFld+
009200140423     C                             '"Consignee name"'                   +sepFld+
009300140423     C                             '"Consignee ZIP code"'               +sepFld+
009400140423     C                             '"Consignee city"'                   +sepFld+
009500140509     C                             '"Shipment Parcels"'                 +sepFld+
009600140509     C                             '"Shipment Weight (kg)"'             +sepFld+
009700140423     C                             '"BRT shipment code"'                +sepFld+
009800140423     C                             '"Date of Arrival Arrival depot"'    +sepFld+
009900140423     C                             '"Date of Last Delivery attempt"'    +sepFld+
010000140509     C                             '"Status"'                           +sepFld+
010100140509     C                             '"Date of Status"'                   +sepFld+
010200140509 xxx C***                          '"Date of Return"'                   +sepFld+
010300140509     C                             '"Days of Holding goods"'            +sepFld
010400140508 xxx C***                          '"Transport lead time (days)"'       +sepFld
010500140423     C*
010600140423     C                   WRITE     TIVAW000
010700140423     C                   ENDIF
010800991027     C*
010900090325     C                   READ      tivgdtmp
011000090325     C                   DOW       not %eof
011100110914     c                   movel     vgddta        fnvacds
011200041220     C*
011300110810     C* Inizializzo i campi d wrk
011400110913     C                   Z-ADD     *zeros        wTLT
011500110913     C                   Z-ADD     *zeros        wGGS
011600110913     C                   Z-ADD     *zeros        wDCMC
011700110913     C                   Z-ADD     *zeros        wDCM2
011800111111     C                   Z-ADD     *zeros        wDLA
011900140508     C                   setoff                                       70
012000030709     C*
012100110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
012200110810     C     KEYtas30C_C   CHAIN     titas30c
012300030709     C                   IF        %found(titas30c)
012400110913     C*
012500110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
012600110913     C                   MOVEL     *blanks       wEsito1           1
012700110913     C*
012800110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
012900110913     C                                                vacAAS
013000110913     C                                               :vacLNP
013100110913     C                                               :vacNRS
013200110913     C                                               :vacNSP
013300110913     C                                               :pOutLblTyp
013400110913     C                                               :pOutAnnoBO
013500110913     C                                               :pOutLineaParBO
013600110913     C                                               :pOutSerieBO
013700110913     C                                               :pOutNumSpedBO
013800110913     C                                               :pOutDcmBO
013900110913     C                                               :pOutCcaBO
014000110913     C                                               :pOutRblBO))
014100140508     C*
014200140508     C* Solo bolle originali e valide ai fini della spedizione
014300140508     C                   if         pOutRblBO ='N' AND
014400140508     C                             (pOutLblTyp='O' OR %subst(prmppt:16:1)='F')
014500111111     C*
014600111111     C* Quindi aggancio l'ultima giacenza
014700140509     C                   EVAL      gcpDUR = *zeros
014800140509     C                   EVAL      gcpFRG = *zeros
014900140509     C     KEYgcp51_C    SETLL     tigcp51l
015000140509     C                   IF        %equal(tigcp51l)
015100140509     C     KEYgcp51_C    READE     tigcp51l
015200140509     C                   DOW       not %eof(tigcp51l)
015300140509     C*
015400140509     C* Se giacenza NN annullata la considero
015500140509     C                   IF        gcpATB = *blanks
015600140509     C                   EVAL      wGGS = gcpGGS
015700140509     C*
015800140509     C* Verifico le condizione che soddisfano lo stato GIACNODIS1
015900140509     C                   IF        gcpDUR > *zeros AND
016000140509     C                             gcpDDM = *zeros AND
016100140509     C                             gcpFAS < 030
016200140509     C                   seton                                        70
016300140509     C                   ENDIF
016400140509     C*
016500140509     C* Reperisco la causale di ultima apertura giacenza
016600140509 xxx C***                EVAL      wGCPCMC = gcpCMC
016700140509     C                   ENDIF
016800140509     C*
016900140509     C* Proseguo la lettura delle giacenze x la bolla corrente
017000140509     C     KEYgcp51_C    READE     tigcp51l
017100140509     C                   ENDDO
017200140509     C                   ENDIF
017300111111     C*
017400111111     C* Quindi reperisco l'ultimo lasciato avviso
017500111111     C                   EXSR      REPLAV
017600140508     C
017700111111     C* Determino stato attuale della spedizione
017800140508     C                   EXSR      CALSTS
017900111111     C*****
018000110913     C                   SELECT
018100110913     C* Consegna OK
018200140508     C                   WHEN      wConsOkSiNoDes = 'Consegnata'
018300140508     C                   EVAL      wDCMC = wDataSts
018400110914     C                   EVAL      wTLT  = tasNRC / 24
018500110913     C* Reso
018600140508     C                   WHEN      wConsOkSiNoDes = 'Resa'
018700140508     C                   EVAL      wDCM2 = wDataSts
018800110914     C*
018900110913     C                   ENDSL
019000071205     C*
019100110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
019200110914     C  N60              EVAL      wNRC = %trim(%editw(wTLT:'   0 , -'))
019300110914     C   60              EVAL      wNRC = %trim(%editw(wTLT:'   0 . -'))
019400120615     C  N60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 , '))
019500120615     C   60              EVAL      wPKB = %trim(%editw(VACPKB:'    0 . '))
019600110810     C*
019700110810     C                   EVAL      VAWDTA  =
019800140509     C                             %trim(%editc(TASDRT:'4'))            +sepFld+
019900140509     C                             %trim(%editc(TASDUC:'4'))            +sepFld+
020000140509     C                             %trim(%editc(VACRMN:'4'))            +sepFld+
020100111223     C                             %trim(vacRMA)                        +sepFld+
020200110810     C                             %trim(tasRSD)                        +sepFld+
020300110810     C                             %trim(tasCAD)                        +sepFld+
020400110810     C                             %trim(tasLOD)                        +sepFld+
020500140509     C                             %trim(%editc(tasNCL:'4'))            +sepFld+
020600120615     C                             %trim(wPKB)                          +sepFld+
020700110913     C                             %trim(%char(tasAAS))+'/'+
020800110913     C                             %trim(%char(tasLNP))+'/'+
020900110913     C                             %trim(%char(tasNRS))+'/'+
021000110913     C                             %trim(%char(tasNSP))                 +sepFld+
021100140509     C                             %trim(%editc(tasDTI:'4'))            +sepFld+
021200140509     C                             %trim(%editc(wDLA:'4'))              +sepFld+
021300140509     C                             %trim(wConsOkSiNoDes)                +sepFld+
021400140509     C                             %trim(%editc(wDataSts:'4'))          +sepFld+
021500140509     C***                          %trim(%editc(wDCM2:'4'))             +sepFld+
021600140509     C                             %trim(%editc(wGGS:'Q'))              +sepFld
021700140508     C***                          %trim(wNRC)                          +sepFld
021800930409     C*
021900100209     C                   WRITE     TIVAW000
022000110810     C                   ENDIF
022100111109     C*
022200111109     C                   ENDIF
022300110810     C*
022400090325     C                   delete    tivgd000
022500991027     C*
022600090325     C                   READ      tivgdtmp
022700030325     C                   ENDDO
022800991027     C*
022900030325     C                   EVAL      wrkesito = '0'
023000991027     C*
023100910830     C                   ENDSR
023200111111
023300111111
023400111111
023500111111     C*------------------------------------------------------------------------*
023600111111     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
023700111111     C*------------------------------------------------------------------------*
023800111111     C     REPLAV        BEGSR
023900111111     C*
024000111111     C* Pererisco l'ultimo evento Lasciato Avviso
024100111111     C                   MOVEL     '1'           tblKUT
024200111111     C                   MOVEL     '2A'          tblCOD
024300111111     C     KEYtas30C_C   SETGT     FNEVB01L
024400111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
024500111111     C*
024600111111     C     *IN34         DOWEQ     *OFF
024700111111     C                   MOVEL(P)  EVBCEV        tblKEY
024800111111     C     KEYtab00_C    CHAIN     TABEL00F                           30
024900111111     C                   IF        %found(tabel00f)
025000111111     C                   MOVEL     TBLUNI        DS2A
025100111111     C     §2AFTC        IFEQ      'A'
025200111111     C                   Z-ADD     EVBDEV        wDLA
025300111111     C                   LEAVE
025400111111     C                   ENDIF
025500111111     C                   ENDIF
025600111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
025700111111     C                   ENDDO
025800111111     C*
025900111111     C                   ENDSR
026000111111     C*------------------------------------------------------------------------*
026100140508
026200140508
026300140508
026400140508     C     calSTS        BEGSR
026500140508     C*
026600140508     C                   MOVEL     *blanks       wMIC              1
026700140508     C                   MOVEL     *blanks       wGIAC             1
026800140508     C                   MOVEL     *blanks       wLAVV             1
026900140508     C                   MOVEL     *blanks       wPEND             1
027000140508     C                   EVAL      wDataSts       = vacDCM
027100140508     C*
027200140508     C* Verifico lo stato delle disposizioni di giacenza
027300140508     C                   MOVEL     *blanks       wGiacDisp        10
027400140508     C   70              EVAL      wGiacDisp = 'IN ATTESA'
027500140508     C*
027600140508     C* Determino subito lo stato corrente della spedizione (e relativa descrizione)
027700140508     C                   IF        VACDCM > *zeros AND
027800140508     C                             (VACCCA = *blanks OR VACCCA = '1')
027900140508     C                   EVAL      wConsOkSiNo    = 'S'
028000140508     C                   EVAL      wConsOkSiNoDes = 'Consegnata'
028100140508     C                   EVAL      wGiacDisp      = *blanks
028200140508     C                   EVAL      wNoteGiac      = *blanks
028300140508     C                   ELSE
028400140508     C                   EVAL      wConsOkSiNo    = 'N'
028500140508     C                   EVAL      wFIRMATAR      = *blanks
028600140508     C*
028700140508     C* A questo punto stabilisco la descrizione
028800140508     C                   SELECT
028900140508     C                   WHEN      vacCCA = '1'
029000140508     C                   EVAL      wConsOkSiNoDes = 'Dirottata'
029100140508     C                   WHEN      vacCCA = '2'
029200140508     C                   EVAL      wConsOkSiNoDes = 'Resa'
029300140508     C                   EVAL      wConsOkSiNo    = 'R'
029400140508     C                   WHEN      vacCCA = '3'
029500140508     C                   EVAL      wConsOkSiNoDes = 'Rapinata'
029600140508     C                   EVAL      wConsOkSiNo    = 'R'
029700140508     C                   WHEN      vacCCA = '4'
029800140508     C                   EVAL      wConsOkSiNoDes = 'Rapinato incasso'
029900140508     C                   EVAL      wConsOkSiNo    = 'R'
030000140508     C                   WHEN      vacCCA = '5'
030100140508     C                   EVAL      wConsOkSiNoDes = 'Chiusa pratica anomalia'
030200140508     C                   EVAL      wConsOkSiNo    = 'R'
030300140508     C                   WHEN      vacCCA = '6'
030400140508     C                   EVAL      wConsOkSiNoDes = 'Resa da danno'
030500140508     C                   EVAL      wConsOkSiNo    = 'R'
030600140508     C                   WHEN      vacCCA = 'A'
030700140508     C                   EVAL      wConsOkSiNoDes = 'Aperta pratica anomalia'
030800140508     C                   EVAL      wConsOkSiNo    = 'R'
030900140508     C                   WHEN      vacCCA = 'C'
031000140508     C                   EVAL      wConsOkSiNoDes = 'In consegna'
031100140508     C                   EVAL      wMIC = 'S'
031200140508     C                   WHEN      vacCCA = 'R'
031300140508     C                   EVAL      wConsOkSiNoDes = 'Ripristinata'
031400140508     C                   WHEN      vacCCA = 'S'
031500140508     C                   EVAL      wConsOkSiNoDes = 'Merce distrutta'
031600140508     C                   EVAL      wConsOkSiNo    = 'R'
031700140508     C                   ENDSL
031800140508     C*
031900140508     C                   IF        (vacDLA > vacDCM AND vacDLA >  gcpDUR)
032000140508     C                   EVAL      wDataSts       = vacDLA
032100140508     C                   EVAL      wConsOkSiNoDes = 'Lasciato avviso'
032200140508     C                   EVAL      wLAVV = 'S'
032300140508     C                   ENDIF
032400140508     C                   IF        (gcpDUR > vacDCM AND gcpDUR >= vacDLA)
032500140508     C                   EVAL      wDataSts       = gcpDUR
032600140508     C                   EVAL      wConsOkSiNoDes = 'In giacenza'
032700140508     C                   EVAL      wGIAC = 'S'
032800140508     C                   ELSE
032900140508     C                   EVAL      wGiacDisp      = *blanks
033000140508     C                   EVAL      wNoteGiac      = *blanks
033100140508     C                   ENDIF
033200140508     C                   IF        vacDCM = *zeros AND
033300140508     C                             vacDLA = *zeros AND
033400140508     C                             gcpDUR = *zeros AND
033500140508     C                             vacCCA = *blanks
033600140508     C                   EVAL      wDataSts       = datcor
033700140508     C*
033800140508     C* Verifico la data consegna richiesta
033900140508     C                   IF        vacDCR > datcor
034000140508     C                   EVAL      wConsOkSiNoDes = 'Richiesta consegna il ' +
034100140508     C                              UBFMTDATE_Convert(
034200140508     C                                  %editc(vacDCR:'4') :
034300140508     C                                  'YYYYMMDD' : 'DD/MM/YYYY')
034400140508     C                   ELSE
034500140508     C                   EVAL      wConsOkSiNoDes = 'In lavorazione/attesa'
034600140508     C                   EVAL      wPEND          = 'S'
034700140508     C                   ENDIF
034800140508     C                   ENDIF
034900140508     C*
035000140508     C                   ENDIF
035100140508     C*
035200140508     C                   ENDSR
035300111111
035400110810
035500110810
035600991027      /TITLE Operazioni iniziali.
035700991027     C     *inzsr        BEGSR
035800991027     C*
035900991027     C     *ENTRY        PLIST
036000991027     C                   parm                    prmppt
036100991027     C     wrkesito      parm      wrkesito      prmesito
036200140508     C*
036300140508     C* CALCOLA LA DATA CORRENTE
036400140508     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
036500140508     C                   eval      datcor = %dec(%date() : *ISO)
036600070316     C*
036700070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
036800070316     C                   eval      sepFld = %subst(prmppt:2:1)
036900070316     C                   if        sepFld = *blanks
037000070316     C                   eval      sepFld = ','
037100070316     C                   endif
037200071205     C*
037300071205     C* Verifico se richiesto un particolare carattere d separatore decimale
037400071205     C                   SETON                                        60
037500071205     C                   IF        %subst(prmppt:3:1) = ','
037600071205     C                   SETOFF                                       60
037700071205     C                   ENDIF
037800030709     C*
037900030709     C* Definizione chiavi
038000030709     C*
038100030709     C* Chiave su TITAS30C - Completa
038200110810     C     KEYtas30C_C   KLIST
038300110913     C                   KFLD                    vacAAS
038400110913     C                   KFLD                    vacLNP
038500110913     C                   KFLD                    vacNRS
038600110913     C                   KFLD                    vacNSP
038700110913     C*
038800110913     C* Chiave su TIGCP51L - Completa
038900110913     C     KEYgcp51_C    KLIST
039000110913     C                   KFLD                    vacAAS
039100110913     C                   KFLD                    vacLNP
039200110913     C                   KFLD                    vacNRS
039300110913     C                   KFLD                    vacNSP
039400110913     C                   KFLD                    gcpFRG
039500111111     C*
039600111111     C     KEYtab00_C    KLIST
039700111111     C                   KFLD                    tblKUT
039800111111     C                   KFLD                    tblCOD
039900111111     C                   KFLD                    tblKEY
040000110810     C*
040100991027     C                   ENDSR
