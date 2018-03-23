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
002500110913
002600110913
002700110913     D*------------------
002800110913     D* LINKING A DEFINIZIONI ESTERNE
002900110913     D*------------------
003000110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003100110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003200110913
003300110810
003400991027
003500030908     C                   EXSR      chkRECFILMBR
003600991027     C                   EXSR      traduci
003700921023     C                   SETON                                        LR
003800030908
003900110810
004000110810
004100030908     C     chkRECFILMBR  BEGSR
004200030908     C*
004300100209     C                   READ      TIVAWWWT                               55
004400030908     C*
004500030908     C                   ENDSR
004600110810
004700110810
004800991027
004900991027     C     traduci       BEGSR
005000030325     C*
005100090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005200090427     C* (descrizione campi in inglese)
005300111111     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005400100209     C                   EVAL      VAWDTA =
005500111111     C                             '"Data ritiro"'                      +sepFld+
005600111111     C                             '"Data partenza"'                    +sepFld+
005700111111     C                             '"Rif. mittente numerico"'           +sepFld+
005800111111     C                             '"Rif. mittente alfabetico"'         +sepFld+
005900111111     C                             '"Destinatario"'                     +sepFld+
006000111111     C                             '"CAP destinatario"'                 +sepFld+
006100111111     C                             '"Località destinatario"'            +sepFld+
006200120307     C                             '"Num. sped. BRT"'                   +sepFld+
006300111111     C                             '"Data arrivo"'                      +sepFld+
006400111111     C                             '"Data lasciato avviso"'             +sepFld+
006500111111     C                             '"Data consegna"'                    +sepFld+
006600111111     C                             '"Data reso"'                        +sepFld+
006700111111     C                             '"Giorni sosta"'                     +sepFld+
006800111111     C                             '"Affidabilità"'                     +sepFld
006900090427     C*
007000100209     C                   WRITE     TIVAW000
007100090427     C                   ENDIF
007200991027     C*
007300090325     C                   READ      tivgdtmp
007400090325     C                   DOW       not %eof
007500110914     c                   movel     vgddta        fnvacds
007600041220     C*
007700110810     C* Inizializzo i campi d wrk
007800110913     C                   Z-ADD     *zeros        wTLT
007900110913     C                   Z-ADD     *zeros        wGGS
008000110913     C                   Z-ADD     *zeros        wDCMC
008100110913     C                   Z-ADD     *zeros        wDCM2
008200111111     C                   Z-ADD     *zeros        wDLA
008300030709     C*
008400110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
008500110810     C     KEYtas30C_C   CHAIN     titas30c
008600030709     C                   IF        %found(titas30c)
008700110913     C*
008800110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
008900110913     C                   MOVEL     *blanks       wEsito1           1
009000110913     C                   MOVEL     *blanks       wEsito2           1
009100110914     C                   Z-ADD     *zeros        wDataCons         8 0
009200110914     C                   MOVEL     *blanks       wConsAnom         1
009300110913     C*
009400110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
009500110913     C                                                vacAAS
009600110913     C                                               :vacLNP
009700110913     C                                               :vacNRS
009800110913     C                                               :vacNSP
009900110913     C                                               :pOutLblTyp
010000110913     C                                               :pOutAnnoBO
010100110913     C                                               :pOutLineaParBO
010200110913     C                                               :pOutSerieBO
010300110913     C                                               :pOutNumSpedBO
010400110913     C                                               :pOutDcmBO
010500110913     C                                               :pOutCcaBO
010600110913     C                                               :pOutRblBO))
010700111109     C*
010800111109     C* Ritorno solamente le bolle valide come spedizione
010900111109     C                   IF        pOutRblBO = 'N'
011000110913     C*
011100110913     C* Chiamata metodo GetLastChild
011200110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
011300110913     C                                                pOutAnnoBO
011400110913     C                                               :pOutLineaParBO
011500110913     C                                               :pOutSerieBO
011600110913     C                                               :pOutNumSpedBO
011700110913     C                                               :pOutAnnoFI
011800110913     C                                               :pOutLineaParFI
011900110913     C                                               :pOutSerieFI
012000110913     C                                               :pOutNumSpedFI
012100110913     C                                               :pOutDcmFI
012200110913     C                                               :pOutCcaFI))
012300110914     C*
012400110914     C                   IF        wEsito2 = '0'
012500110914     C                   EVAL      wDataCons = pOutDcmFI
012600110914     C                   EVAL      wConsAnom = pOutCcaFI
012700110914     C                   ELSE
012800110914     C                   EVAL      wDataCons = pOutDcmBO
012900110914     C                   EVAL      wConsAnom = pOutCcaBO
013000110914     C                   ENDIF
013100111111     C*
013200111111     C* Quindi aggancio l'ultima giacenza
013300111111     C                   EVAL      gcpFRG = *zeros
013400111111     C     KEYgcp51_C    CHAIN     tigcp51l
013500111111     C                   IF        %found(tigcp51l)
013600111111     C*
013700111111     C* Se giacenza NN annullata la considero
013800111111     C                   IF        gcpATB = *blanks
013900111111     C                   EVAL      wGGS = gcpGGS
014000111111     C                   ENDIF
014100111111     C                   ENDIF
014200111111     C*
014300111111     C* Quindi reperisco l'ultimo lasciato avviso
014400111111     C                   EXSR      REPLAV
014500111111     C*****
014600111111     C* Determino stato attuale della spedizione
014700111111     C*****
014800110913     C                   SELECT
014900110913     C* Consegna OK
015000110914     C                   WHEN      wDataCons > *zeros AND
015100110914     C                             wConsAnom = *blanks
015200110914     C                   EVAL      wDCMC = wDataCons
015300110914     C                   EVAL      wTLT  = tasNRC / 24
015400110913     C* Reso
015500110914     C                   WHEN      wDataCons > *zeros AND
015600110914     C                             wConsAnom = '2'
015700110914     C                   EVAL      wDCM2 = wDataCons
015800110914     C*
015900110913     C                   ENDSL
016000071205     C*
016100110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
016200110914     C  N60              EVAL      wNRC = %trim(%editw(wTLT:'   0 , -'))
016300110914     C   60              EVAL      wNRC = %trim(%editw(wTLT:'   0 . -'))
016400110810     C*
016500110810     C                   EVAL      VAWDTA  =
016600110913     C                             %trim(%editc(TASDRT:'Q'))            +sepFld+
016700110913     C                             %trim(%editc(TASDUC:'Q'))            +sepFld+
016800110914     C                             %trim(%editc(VACRMN:'Q'))            +sepFld+
016900111223     C                             %trim(vacRMA)                        +sepFld+
017000110810     C                             %trim(tasRSD)                        +sepFld+
017100110810     C                             %trim(tasCAD)                        +sepFld+
017200110810     C                             %trim(tasLOD)                        +sepFld+
017300110913     C                             %trim(%char(tasAAS))+'/'+
017400110913     C                             %trim(%char(tasLNP))+'/'+
017500110913     C                             %trim(%char(tasNRS))+'/'+
017600110913     C                             %trim(%char(tasNSP))                 +sepFld+
017700110913     C                             %trim(%editc(tasDTI:'Q'))            +sepFld+
017800111111     C                             %trim(%editc(wDLA:'Q'))              +sepFld+
017900110914     C                             %trim(%editc(wDCMC:'Q'))             +sepFld+
018000110914     C                             %trim(%editc(wDCM2:'Q'))             +sepFld+
018100110913     C                             %trim(%editc(wGGS:'Q'))              +sepFld+
018200110913     C                             %trim(wNRC)                          +sepFld
018300930409     C*
018400100209     C                   WRITE     TIVAW000
018500110810     C                   ENDIF
018600111109     C*
018700111109     C                   ENDIF
018800110810     C*
018900090325     C                   delete    tivgd000
019000991027     C*
019100090325     C                   READ      tivgdtmp
019200030325     C                   ENDDO
019300991027     C*
019400030325     C                   EVAL      wrkesito = '0'
019500991027     C*
019600910830     C                   ENDSR
019700111111
019800111111
019900111111
020000111111     C*------------------------------------------------------------------------*
020100111111     C* REPLAV - Routine di reperimento dati relativi al lasciato avviso
020200111111     C*------------------------------------------------------------------------*
020300111111     C     REPLAV        BEGSR
020400111111     C*
020500111111     C* Pererisco l'ultimo evento Lasciato Avviso
020600111111     C                   MOVEL     '1'           tblKUT
020700111111     C                   MOVEL     '2A'          tblCOD
020800111111     C     KEYtas30C_C   SETGT     FNEVB01L
020900111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
021000111111     C*
021100111111     C     *IN34         DOWEQ     *OFF
021200111111     C                   MOVEL(P)  EVBCEV        tblKEY
021300111111     C     KEYtab00_C    CHAIN     TABEL00F                           30
021400111111     C                   IF        %found(tabel00f)
021500111111     C                   MOVEL     TBLUNI        DS2A
021600111111     C     §2AFTC        IFEQ      'A'
021700111111     C                   Z-ADD     EVBDEV        wDLA
021800111111     C                   LEAVE
021900111111     C                   ENDIF
022000111111     C                   ENDIF
022100111111     C     KEYtas30C_C   READPE    FNEVB01L                               34
022200111111     C                   ENDDO
022300111111     C*
022400111111     C                   ENDSR
022500111111     C*------------------------------------------------------------------------*
022600111111
022700110810
022800110810
022900991027      /TITLE Operazioni iniziali.
023000991027     C     *inzsr        BEGSR
023100991027     C*
023200991027     C     *ENTRY        PLIST
023300991027     C                   parm                    prmppt
023400991027     C     wrkesito      parm      wrkesito      prmesito
023500070316     C*
023600070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
023700070316     C                   eval      sepFld = %subst(prmppt:2:1)
023800070316     C                   if        sepFld = *blanks
023900070316     C                   eval      sepFld = ','
024000070316     C                   endif
024100071205     C*
024200071205     C* Verifico se richiesto un particolare carattere d separatore decimale
024300071205     C                   SETON                                        60
024400071205     C                   IF        %subst(prmppt:3:1) = ','
024500071205     C                   SETOFF                                       60
024600071205     C                   ENDIF
024700030709     C*
024800030709     C* Definizione chiavi
024900030709     C*
025000030709     C* Chiave su TITAS30C - Completa
025100110810     C     KEYtas30C_C   KLIST
025200110913     C                   KFLD                    vacAAS
025300110913     C                   KFLD                    vacLNP
025400110913     C                   KFLD                    vacNRS
025500110913     C                   KFLD                    vacNSP
025600110913     C*
025700110913     C* Chiave su TIGCP51L - Completa
025800110913     C     KEYgcp51_C    KLIST
025900110913     C                   KFLD                    vacAAS
026000110913     C                   KFLD                    vacLNP
026100110913     C                   KFLD                    vacNRS
026200110913     C                   KFLD                    vacNSP
026300110913     C                   KFLD                    gcpFRG
026400111111     C*
026500111111     C     KEYtab00_C    KLIST
026600111111     C                   KFLD                    tblKUT
026700111111     C                   KFLD                    tblCOD
026800111111     C                   KFLD                    tblKEY
026900110810     C*
027000991027     C                   ENDSR
