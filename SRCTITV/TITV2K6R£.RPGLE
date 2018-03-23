000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400120214     FAZORG01L  IF   E           K DISK
000500030709     FTITAS30C  IF   E           K DISK
000600090325     Ftivgdtmp  uF   E             DISK
000700100209     FTIVAWWWT  UF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200110913     D fnvacds       e ds                  extname(fnvac00t)
001300070316     D sepFld          s              1
001400110810
001500110810
001600110913     D wTLT            s              5S 1 inz
001700110913     D wDCMC           s                   inz like(vacDCM)
001800110913     D wDCM2           s                   inz like(vacDCM)
001900120214     D wNRC            s             10    inz
002000120214     D wPKB            s             10    inz
002100120214     D wVLB            s             10    inz
002200120214     D wLNPdes         s             35    inz
002300120214     D wLNAdes         s             35    inz
002400110913
002500110913
002600110913     D*------------------
002700110913     D* LINKING A DEFINIZIONI ESTERNE
002800110913     D*------------------
002900110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003000110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003100120117     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003200120117     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
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
005300090427     C* (descrizione campi in inglese)
005400090427     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
005500100209     C                   EVAL      VAWDTA =
005600120117     C                             '"Customer code"'                    +sepFld+
005700120117     C                             '"Shipment reference ID"'            +sepFld+
005800110913     C                             '"Consignee name"'                   +sepFld+
005900110913     C                             '"Consignee zip code"'               +sepFld+
006000110913     C                             '"Consignee city"'                   +sepFld+
006100120214     C                             '"Consignee province code"'          +sepFld+
006200120307     C                             '"BRT shipment code"'                +sepFld+
006300120214     C                             '"Parcels"'                          +sepFld+
006400120214     C                             '"Weight (Kg)"'                      +sepFld+
006500120214     C                             '"Volume (m3)"'                      +sepFld+
006600120117     C                             '"Date of arrival - Departure dpt"'  +sepFld+
006700120117     C                             '"Date of departure"'                +sepFld+
006800120117     C                             '"Date of arrival - Arrival dpt"'    +sepFld+
006900120117     C                             '"Date of delivery"'                 +sepFld+
007000120117     C                             '"Date of return"'                   +sepFld+
007100120214     C                             '"Requested delivery date"'          +sepFld+
007200120224     C                             '"Departure depot"'                  +sepFld+
007300120224     C                             '"Arrival depot"'                    +sepFld+
007400120214     C                             '"Special delivery flags"'           +sepFld+
007500120117     C                             '"SLA - Transport lead time"'        +sepFld
007600120117     C*
007700120117     C                   WRITE     TIVAW000
007800120117     C                   ENDIF
007900991027     C*
008000090325     C                   READ      tivgdtmp
008100090325     C                   DOW       not %eof
008200110914     c                   movel     vgddta        fnvacds
008300041220     C*
008400110810     C* Inizializzo i campi d wrk
008500110913     C                   Z-ADD     *zeros        wTLT
008600110913     C                   Z-ADD     *zeros        wDCMC
008700110913     C                   Z-ADD     *zeros        wDCM2
008800030709     C*
008900110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
009000110810     C     KEYtas30C_C   CHAIN     titas30c
009100030709     C                   IF        %found(titas30c)
009200110913     C*
009300110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
009400110913     C                   MOVEL     *blanks       wEsito1           1
009500110913     C                   MOVEL     *blanks       wEsito2           1
009600110914     C                   Z-ADD     *zeros        wDataCons         8 0
009700110914     C                   MOVEL     *blanks       wConsAnom         1
009800110913     C*
009900110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
010000110913     C                                                vacAAS
010100110913     C                                               :vacLNP
010200110913     C                                               :vacNRS
010300110913     C                                               :vacNSP
010400110913     C                                               :pOutLblTyp
010500110913     C                                               :pOutAnnoBO
010600110913     C                                               :pOutLineaParBO
010700110913     C                                               :pOutSerieBO
010800110913     C                                               :pOutNumSpedBO
010900110913     C                                               :pOutDcmBO
011000110913     C                                               :pOutCcaBO
011100110913     C                                               :pOutRblBO))
011200111109     C*
011300111109     C* Ritorno solamente le bolle valide come spedizione
011400111109     C                   IF        pOutRblBO = 'N'
011500110913     C*
011600110913     C* Chiamata metodo GetLastChild
011700110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
011800110913     C                                                pOutAnnoBO
011900110913     C                                               :pOutLineaParBO
012000110913     C                                               :pOutSerieBO
012100110913     C                                               :pOutNumSpedBO
012200110913     C                                               :pOutAnnoFI
012300110913     C                                               :pOutLineaParFI
012400110913     C                                               :pOutSerieFI
012500110913     C                                               :pOutNumSpedFI
012600110913     C                                               :pOutDcmFI
012700110913     C                                               :pOutCcaFI))
012800110914     C*
012900110914     C                   IF        wEsito2 = '0'
013000110914     C                   EVAL      wDataCons = pOutDcmFI
013100110914     C                   EVAL      wConsAnom = pOutCcaFI
013200110914     C                   ELSE
013300110914     C                   EVAL      wDataCons = pOutDcmBO
013400110914     C                   EVAL      wConsAnom = pOutCcaBO
013500110914     C                   ENDIF
013600110913     C*
013700110913     C                   SELECT
013800110913     C* Consegna OK
013900110914     C                   WHEN      wDataCons > *zeros AND
014000110914     C                             wConsAnom = *blanks
014100110914     C                   EVAL      wDCMC = wDataCons
014200110914     C                   EVAL      wTLT  = tasNRC / 24
014300110913     C* Reso
014400110914     C                   WHEN      wDataCons > *zeros AND
014500110914     C                             wConsAnom = '2'
014600110914     C                   EVAL      wDCM2 = wDataCons
014700110914     C*
014800110913     C                   ENDSL
014900071205     C*
015000110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
015100120214     C                   MOVEL     *blanks       wSegno            1
015200120214     C                   IF        wTLT < *zeros
015300120214     C                   EVAL      wSegno = '-'
015400120214     C                   ENDIF
015500120214     C*
015600120214     C  N60              EVAL      wNRC = %trim(wSegno +
015700120214     C                                    %trim(%editw(wTLT:'   0 , ')))
015800120214     C   60              EVAL      wNRC = %trim(wSegno +
015900120214     C                                    %trim(%editw(wTLT:'   0 . ')))
016000120214     C*
016100120214     C  N60              EVAL      wPKB = %trim(%editw(tasPKB:'     0 , '))
016200120214     C   60              EVAL      wPKB = %trim(%editw(tasPKB:'     0 . '))
016300120214     C  N60              EVAL      wVLB = %trim(%editw(tasVLB:'   0 ,   '))
016400120214     C   60              EVAL      wVLB = %trim(%editw(tasVLB:'   0 .   '))
016500120214     C*
016600120214     C* Forzatura in caso d spedizione ancora nn "chiusa"
016700120214     C                   IF        wDCMC = *zeros AND wDCM2 = *zeros
016800120214     C                   EVAL      wNRC = 'PENDING'
016900120214     C                   ENDIF
017000120214     C*
017100120214     C* Reperisco le descrizioni delle filiali partenza/arrivo
017200120214     C                   EVAL      wLNPdes = %trim(%editc(tasLNP:'X'))
017300120214     C                   EVAL      wLNAdes = %trim(%editc(tasLNA:'X'))
017400120214     C*
017500120214     C     tasLNP        CHAIN     azorg01l
017600120214     C                   IF        %found(azorg01l)
017700120214     C                   EVAL      wLNPdes = %trim(orgDES)+' - '+%trim(wLNPdes)
017800120214     C                   ENDIF
017900120214     C*
018000120214     C     tasLNA        CHAIN     azorg01l
018100120214     C                   IF        %found(azorg01l)
018200120214     C                   EVAL      wLNAdes = %trim(orgDES)+' - '+%trim(wLNAdes)
018300120214     C                   ENDIF
018400120214     C
018500110810     C*
018600110810     C                   EVAL      VAWDTA  =
018700120117     C                             %trim(%editc(vacCCM:'X'))            +sepFld+
018800120117     C                             %trim(%editc(vacRMN:'Q'))            +sepFld+
018900110810     C                             %trim(tasRSD)                        +sepFld+
019000110810     C                             %trim(tasCAD)                        +sepFld+
019100110810     C                             %trim(tasLOD)                        +sepFld+
019200120214     C                             %trim(tasPRD)                        +sepFld+
019300120117     C                             %trim(%editc(tasAAS:'X'))+'/'+
019400120117     C                             %trim(%editc(tasLNP:'X'))+'/'+
019500120117     C                             %trim(%editc(tasNRS:'X'))+'/'+
019600120117     C                             %trim(%editc(tasNSP:'X'))            +sepFld+
019700120214     C                             %trim(%editc(tasNCL:'1'))            +sepFld+
019800120214     C                             %trim(wPKB)                          +sepFld+
019900120214     C                             %trim(wVLB)                          +sepFld+
020000120117     C                             %trim(UBFMTDATE_Convert(
020100120117     C                                  %editc(tasDRT:'4') :
020200120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
020300120117     C                             %trim(UBFMTDATE_Convert(
020400120117     C                                  %editc(tasDUC:'4') :
020500120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
020600120117     C                             %trim(UBFMTDATE_Convert(
020700120117     C                                  %editc(tasDTI:'4') :
020800120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
020900120117     C                             %trim(UBFMTDATE_Convert(
021000120117     C                                  %editc(wDCMC:'4') :
021100120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
021200120117     C                             %trim(UBFMTDATE_Convert(
021300120117     C                                  %editc(wDCM2:'4') :
021400120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
021500120214     C                             %trim(UBFMTDATE_Convert(
021600120214     C                                  %editc(tasDCR:'4') :
021700120214     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
021800120214     C                             %trim(wLNPdes)                       +sepFld+
021900120214     C                             %trim(wLNAdes)                       +sepFld+
022000120214     C                             tasFTC + tasTC2                      +sepFld+
022100110913     C                             %trim(wNRC)                          +sepFld
022200120117     C*
022300120426     C* Restituisco solo i porti franco e solo in ritardo
022400120426     C                   IF        %trim(vacCBO) = '1' AND
022500120426     C                             wTLT > *zeros
022600120117     C                   WRITE     TIVAW000
022700120117     C                   ENDIF
022800120117     C*
022900110810     C                   ENDIF
023000111109     C*
023100111109     C                   ENDIF
023200110810     C*
023300090325     C                   delete    tivgd000
023400991027     C*
023500090325     C                   READ      tivgdtmp
023600030325     C                   ENDDO
023700991027     C*
023800030325     C                   EVAL      wrkesito = '0'
023900991027     C*
024000910830     C                   ENDSR
024100991027
024200110810
024300110810
024400991027      /TITLE Operazioni iniziali.
024500991027     C     *inzsr        BEGSR
024600991027     C*
024700991027     C     *ENTRY        PLIST
024800991027     C                   parm                    prmppt
024900991027     C     wrkesito      parm      wrkesito      prmesito
025000070316     C*
025100070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
025200070316     C                   eval      sepFld = %subst(prmppt:2:1)
025300070316     C                   if        sepFld = *blanks
025400070316     C                   eval      sepFld = ','
025500070316     C                   endif
025600071205     C*
025700071205     C* Verifico se richiesto un particolare carattere d separatore decimale
025800071205     C                   SETON                                        60
025900071205     C                   IF        %subst(prmppt:3:1) = ','
026000071205     C                   SETOFF                                       60
026100071205     C                   ENDIF
026200030709     C*
026300030709     C* Definizione chiavi
026400030709     C*
026500030709     C* Chiave su TITAS30C - Completa
026600110810     C     KEYtas30C_C   KLIST
026700110913     C                   KFLD                    vacAAS
026800110913     C                   KFLD                    vacLNP
026900110913     C                   KFLD                    vacNRS
027000110913     C                   KFLD                    vacNSP
027100110810     C*
027200991027     C                   ENDSR
