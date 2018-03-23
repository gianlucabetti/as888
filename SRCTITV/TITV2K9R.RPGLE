000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400120214     FAZORG01L  IF   E           K DISK
000500030709     FTITAS30C  IF   E           K DISK
000600120224     FWADLY05L  IF   E           K DISK
000700090325     Ftivgdtmp  uF   E             DISK
000800100209     FTIVAWWWT  UF A E             DISK    commit
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300110913     D fnvacds       e ds                  extname(fnvac00t)
001400070316     D sepFld          s              1
001500110810
001600110810
001700110913     D wTLT            s              5S 1 inz
001800110913     D wDCMC           s                   inz like(vacDCM)
001900110913     D wDCM2           s                   inz like(vacDCM)
002000120214     D wNRC            s             10    inz
002100120214     D wPKB            s             10    inz
002200120214     D wVLB            s             10    inz
002300120214     D wLNPdes         s             35    inz
002400120214     D wLNAdes         s             35    inz
002500110913
002600110913
002700110913     D*------------------
002800110913     D* LINKING A DEFINIZIONI ESTERNE
002900110913     D*------------------
003000110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003100110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003200120117     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003300120117     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
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
005400090427     C* (descrizione campi in inglese)
005500090427     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
005600100209     C                   EVAL      VAWDTA =
005700120117     C                             '"Customer code"'                    +sepFld+
005800120117     C                             '"Shipment reference ID"'            +sepFld+
005900110913     C                             '"Consignee name"'                   +sepFld+
006000110913     C                             '"Consignee zip code"'               +sepFld+
006100110913     C                             '"Consignee city"'                   +sepFld+
006200120214     C                             '"Consignee province code"'          +sepFld+
006300120307     C                             '"BRT shipment code"'                +sepFld+
006400120214     C                             '"Parcels"'                          +sepFld+
006500120214     C                             '"Weight (Kg)"'                      +sepFld+
006600120214     C                             '"Volume (m3)"'                      +sepFld+
006700120117     C                             '"Date of arrival - Departure dpt"'  +sepFld+
006800120117     C                             '"Date of departure"'                +sepFld+
006900120117     C                             '"Date of arrival - Arrival dpt"'    +sepFld+
007000120117     C                             '"Date of delivery"'                 +sepFld+
007100120117     C                             '"Date of return"'                   +sepFld+
007200120214     C                             '"Requested delivery date"'          +sepFld+
007300120224     C                             '"Departure depot"'                  +sepFld+
007400120224     C                             '"Arrival depot"'                    +sepFld+
007500120214     C                             '"Special delivery flags"'           +sepFld+
007600120224     C                             '"Delivery time (standard)"'         +sepFld+
007700120117     C                             '"SLA - Transport lead time"'        +sepFld
007800120117     C*
007900120117     C                   WRITE     TIVAW000
008000120117     C                   ENDIF
008100991027     C*
008200090325     C                   READ      tivgdtmp
008300090325     C                   DOW       not %eof
008400110914     c                   movel     vgddta        fnvacds
008500041220     C*
008600110810     C* Inizializzo i campi d wrk
008700110913     C                   Z-ADD     *zeros        wTLT
008800110913     C                   Z-ADD     *zeros        wDCMC
008900110913     C                   Z-ADD     *zeros        wDCM2
009000030709     C*
009100110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
009200110810     C     KEYtas30C_C   CHAIN     titas30c
009300030709     C                   IF        %found(titas30c)
009400120224     C*
009500120224     C                   Z-ADD     *zeros        wTRC              3 0
009600120227     C*
009700120227     C* Se tipo servizio 'D' => forzo cmq 'C'
009800120227     C                   IF        tasTSp = 'D'
009900120227     C                   EVAL      tasTSP = 'C'
010000120227     C                   ENDIF
010100120224     C     KEYdly05L_P   CHAIN     wadly05l
010200120224     C                   IF        %found(wadly05l)
010300120224     C                   EVAL      wTRC = dlyTRC
010400120224     C                   ENDIF
010500110913     C*
010600110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
010700110913     C                   MOVEL     *blanks       wEsito1           1
010800110913     C                   MOVEL     *blanks       wEsito2           1
010900110914     C                   Z-ADD     *zeros        wDataCons         8 0
011000110914     C                   MOVEL     *blanks       wConsAnom         1
011100110913     C*
011200110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
011300110913     C                                                vacAAS
011400110913     C                                               :vacLNP
011500110913     C                                               :vacNRS
011600110913     C                                               :vacNSP
011700110913     C                                               :pOutLblTyp
011800110913     C                                               :pOutAnnoBO
011900110913     C                                               :pOutLineaParBO
012000110913     C                                               :pOutSerieBO
012100110913     C                                               :pOutNumSpedBO
012200110913     C                                               :pOutDcmBO
012300110913     C                                               :pOutCcaBO
012400110913     C                                               :pOutRblBO))
012500111109     C*
012600111109     C* Ritorno solamente le bolle valide come spedizione
012700111109     C                   IF        pOutRblBO = 'N'
012800110913     C*
012900110913     C* Chiamata metodo GetLastChild
013000110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
013100110913     C                                                pOutAnnoBO
013200110913     C                                               :pOutLineaParBO
013300110913     C                                               :pOutSerieBO
013400110913     C                                               :pOutNumSpedBO
013500110913     C                                               :pOutAnnoFI
013600110913     C                                               :pOutLineaParFI
013700110913     C                                               :pOutSerieFI
013800110913     C                                               :pOutNumSpedFI
013900110913     C                                               :pOutDcmFI
014000110913     C                                               :pOutCcaFI))
014100110914     C*
014200110914     C                   IF        wEsito2 = '0'
014300110914     C                   EVAL      wDataCons = pOutDcmFI
014400110914     C                   EVAL      wConsAnom = pOutCcaFI
014500110914     C                   ELSE
014600110914     C                   EVAL      wDataCons = pOutDcmBO
014700110914     C                   EVAL      wConsAnom = pOutCcaBO
014800110914     C                   ENDIF
014900110913     C*
015000110913     C                   SELECT
015100110913     C* Consegna OK
015200110914     C                   WHEN      wDataCons > *zeros AND
015300110914     C                             wConsAnom = *blanks
015400110914     C                   EVAL      wDCMC = wDataCons
015500110914     C                   EVAL      wTLT  = tasNRC / 24
015600110913     C* Reso
015700110914     C                   WHEN      wDataCons > *zeros AND
015800110914     C                             wConsAnom = '2'
015900110914     C                   EVAL      wDCM2 = wDataCons
016000110914     C*
016100110913     C                   ENDSL
016200071205     C*
016300110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
016400120214     C                   MOVEL     *blanks       wSegno            1
016500120214     C                   IF        wTLT < *zeros
016600120214     C                   EVAL      wSegno = '-'
016700120214     C                   ENDIF
016800120214     C*
016900120214     C  N60              EVAL      wNRC = %trim(wSegno +
017000120214     C                                    %trim(%editw(wTLT:'   0 , ')))
017100120214     C   60              EVAL      wNRC = %trim(wSegno +
017200120214     C                                    %trim(%editw(wTLT:'   0 . ')))
017300120214     C*
017400120214     C  N60              EVAL      wPKB = %trim(%editw(tasPKB:'     0 , '))
017500120214     C   60              EVAL      wPKB = %trim(%editw(tasPKB:'     0 . '))
017600120214     C  N60              EVAL      wVLB = %trim(%editw(tasVLB:'   0 ,   '))
017700120214     C   60              EVAL      wVLB = %trim(%editw(tasVLB:'   0 .   '))
017800120214     C*
017900120214     C* Forzatura in caso d spedizione ancora nn "chiusa"
018000120214     C                   IF        wDCMC = *zeros AND wDCM2 = *zeros
018100120214     C                   EVAL      wNRC = 'PENDING'
018200120214     C                   ENDIF
018300120214     C*
018400120214     C* Reperisco le descrizioni delle filiali partenza/arrivo
018500120214     C                   EVAL      wLNPdes = %trim(%editc(tasLNP:'X'))
018600120214     C                   EVAL      wLNAdes = %trim(%editc(tasLNA:'X'))
018700120214     C*
018800120214     C     tasLNP        CHAIN     azorg01l
018900120214     C                   IF        %found(azorg01l)
019000120214     C                   EVAL      wLNPdes = %trim(orgDES)+' - '+%trim(wLNPdes)
019100120214     C                   ENDIF
019200120214     C*
019300120214     C     tasLNA        CHAIN     azorg01l
019400120214     C                   IF        %found(azorg01l)
019500120214     C                   EVAL      wLNAdes = %trim(orgDES)+' - '+%trim(wLNAdes)
019600120214     C                   ENDIF
019700120214     C
019800110810     C*
019900110810     C                   EVAL      VAWDTA  =
020000120117     C                             %trim(%editc(vacCCM:'X'))            +sepFld+
020100120117     C                             %trim(%editc(vacRMN:'Q'))            +sepFld+
020200110810     C                             %trim(tasRSD)                        +sepFld+
020300110810     C                             %trim(tasCAD)                        +sepFld+
020400110810     C                             %trim(tasLOD)                        +sepFld+
020500120214     C                             %trim(tasPRD)                        +sepFld+
020600120117     C                             %trim(%editc(tasAAS:'X'))+'/'+
020700120117     C                             %trim(%editc(tasLNP:'X'))+'/'+
020800120117     C                             %trim(%editc(tasNRS:'X'))+'/'+
020900120117     C                             %trim(%editc(tasNSP:'X'))            +sepFld+
021000120214     C                             %trim(%editc(tasNCL:'1'))            +sepFld+
021100120214     C                             %trim(wPKB)                          +sepFld+
021200120214     C                             %trim(wVLB)                          +sepFld+
021300120117     C                             %trim(UBFMTDATE_Convert(
021400120117     C                                  %editc(tasDRT:'4') :
021500120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
021600120117     C                             %trim(UBFMTDATE_Convert(
021700120117     C                                  %editc(tasDUC:'4') :
021800120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
021900120117     C                             %trim(UBFMTDATE_Convert(
022000120117     C                                  %editc(tasDTI:'4') :
022100120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
022200120117     C                             %trim(UBFMTDATE_Convert(
022300120117     C                                  %editc(wDCMC:'4') :
022400120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
022500120117     C                             %trim(UBFMTDATE_Convert(
022600120117     C                                  %editc(wDCM2:'4') :
022700120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
022800120214     C                             %trim(UBFMTDATE_Convert(
022900120214     C                                  %editc(tasDCR:'4') :
023000120214     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
023100120214     C                             %trim(wLNPdes)                       +sepFld+
023200120214     C                             %trim(wLNAdes)                       +sepFld+
023300120214     C                             tasFTC + tasTC2                      +sepFld+
023400120224     C                             %char(wTRC)                          +sepFld+
023500110913     C                             %trim(wNRC)                          +sepFld
023600120117     C*
023700120117     C* Restituisco solo i porti franco
023800120117     C                   IF        %trim(vacCBO) = '1' AND
023900120430     C                             vacCCM = 0054790
024000120117     C                   WRITE     TIVAW000
024100120117     C                   ENDIF
024200120117     C*
024300110810     C                   ENDIF
024400111109     C*
024500111109     C                   ENDIF
024600110810     C*
024700090325     C                   delete    tivgd000
024800991027     C*
024900090325     C                   READ      tivgdtmp
025000030325     C                   ENDDO
025100991027     C*
025200030325     C                   EVAL      wrkesito = '0'
025300991027     C*
025400910830     C                   ENDSR
025500991027
025600110810
025700110810
025800991027      /TITLE Operazioni iniziali.
025900991027     C     *inzsr        BEGSR
026000991027     C*
026100991027     C     *ENTRY        PLIST
026200991027     C                   parm                    prmppt
026300991027     C     wrkesito      parm      wrkesito      prmesito
026400070316     C*
026500070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
026600070316     C                   eval      sepFld = %subst(prmppt:2:1)
026700070316     C                   if        sepFld = *blanks
026800070316     C                   eval      sepFld = ','
026900070316     C                   endif
027000071205     C*
027100071205     C* Verifico se richiesto un particolare carattere d separatore decimale
027200071205     C                   SETON                                        60
027300071205     C                   IF        %subst(prmppt:3:1) = ','
027400071205     C                   SETOFF                                       60
027500071205     C                   ENDIF
027600030709     C*
027700030709     C* Definizione chiavi
027800030709     C*
027900030709     C* Chiave su TITAS30C - Completa
028000110810     C     KEYtas30C_C   KLIST
028100110913     C                   KFLD                    vacAAS
028200110913     C                   KFLD                    vacLNP
028300110913     C                   KFLD                    vacNRS
028400110913     C                   KFLD                    vacNSP
028500120224     C*
028600120224     C* Chiave su WADLY05L - Parziale
028700120224     C     KEYdly05L_P   KLIST
028800120224     C                   KFLD                    tasLNP
028900120224     C                   KFLD                    tasTSP
029000120224     C                   KFLD                    tasNZD
029100120224     C                   KFLD                    tasCAD
029200110810     C*
029300991027     C                   ENDSR
