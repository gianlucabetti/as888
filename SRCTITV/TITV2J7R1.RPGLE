000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200150527     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
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
002000150526     D wHMCC           s                   inz like(vacHMC)
002100120214     D wNRC            s             10    inz
002200120214     D wPKB            s             10    inz
002300120214     D wVLB            s             10    inz
002400120214     D wLNPdes         s             35    inz
002500120214     D wLNAdes         s             35    inz
002600110913
002700110913
002800110913     D*------------------
002900110913     D* LINKING A DEFINIZIONI ESTERNE
003000110913     D*------------------
003100110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003200110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003300120117     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003400120117     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
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
005500090427     C* (descrizione campi in inglese)
005600090427     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
005700100209     C                   EVAL      VAWDTA =
005800120117     C                             '"Customer code"'                    +sepFld+
005900120117     C                             '"Shipment reference ID"'            +sepFld+
006000110913     C                             '"Consignee name"'                   +sepFld+
006100110913     C                             '"Consignee zip code"'               +sepFld+
006200110913     C                             '"Consignee city"'                   +sepFld+
006300120214     C                             '"Consignee province code"'          +sepFld+
006400120307     C                             '"BRT shipment code"'                +sepFld+
006500120214     C                             '"Parcels"'                          +sepFld+
006600120214     C                             '"Weight (Kg)"'                      +sepFld+
006700120214     C                             '"Volume (m3)"'                      +sepFld+
006800120117     C                             '"Date of arrival - Departure dpt"'  +sepFld+
006900120117     C                             '"Date of departure"'                +sepFld+
007000120117     C                             '"Date of arrival - Arrival dpt"'    +sepFld+
007100150526     C                             '"Date of delivery"'                 +sepFld+
007200150526     C                             '"Hour of delivery"'                 +sepFld+
007300120117     C                             '"Date of return"'                   +sepFld+
007400120214     C                             '"Requested delivery date"'          +sepFld+
007500120224     C                             '"Departure depot"'                  +sepFld+
007600120224     C                             '"Arrival depot"'                    +sepFld+
007700120214     C                             '"Special delivery flags"'           +sepFld+
007800120224     C                             '"Delivery time (standard)"'         +sepFld+
007900120117     C                             '"SLA - Transport lead time"'        +sepFld
008000120117     C*
008100120117     C                   WRITE     TIVAW000
008200120117     C                   ENDIF
008300991027     C*
008400090325     C                   READ      tivgdtmp
008500090325     C                   DOW       not %eof
008600110914     c                   movel     vgddta        fnvacds
008700041220     C*
008800110810     C* Inizializzo i campi d wrk
008900110913     C                   Z-ADD     *zeros        wTLT
009000110913     C                   Z-ADD     *zeros        wDCMC
009100110913     C                   Z-ADD     *zeros        wDCM2
009200150526     C                   Z-ADD     *zeros        wHMCC
009300030709     C*
009400110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
009500110810     C     KEYtas30C_C   CHAIN     titas30c
009600030709     C                   IF        %found(titas30c)
009700120224     C*
009800120224     C                   Z-ADD     *zeros        wTRC              3 0
009900120227     C*
010000120227     C* Se tipo servizio 'D' => forzo cmq 'C'
010100120227     C                   IF        tasTSp = 'D'
010200120227     C                   EVAL      tasTSP = 'C'
010300120227     C                   ENDIF
010400120224     C     KEYdly05L_P   CHAIN     wadly05l
010500120224     C                   IF        %found(wadly05l)
010600120224     C                   EVAL      wTRC = dlyTRC
010700120224     C                   ENDIF
010800110913     C*
010900110913     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
011000110913     C                   MOVEL     *blanks       wEsito1           1
011100110913     C                   MOVEL     *blanks       wEsito2           1
011200110914     C                   Z-ADD     *zeros        wDataCons         8 0
011300110914     C                   MOVEL     *blanks       wConsAnom         1
011400110913     C*
011500110913     C                   EVAL      wEsito1 = %char(UBLBLSPE_GetLblTyp(
011600110913     C                                                vacAAS
011700110913     C                                               :vacLNP
011800110913     C                                               :vacNRS
011900110913     C                                               :vacNSP
012000110913     C                                               :pOutLblTyp
012100110913     C                                               :pOutAnnoBO
012200110913     C                                               :pOutLineaParBO
012300110913     C                                               :pOutSerieBO
012400110913     C                                               :pOutNumSpedBO
012500110913     C                                               :pOutDcmBO
012600110913     C                                               :pOutCcaBO
012700110913     C                                               :pOutRblBO))
012800111109     C*
012900111109     C* Ritorno solamente le bolle valide come spedizione
013000111109     C                   IF        pOutRblBO = 'N'
013100110913     C*
013200110913     C* Chiamata metodo GetLastChild
013300110913     C                   EVAL      wEsito2 = %char(UBLBLSPE_GetLastChild(
013400110913     C                                                pOutAnnoBO
013500110913     C                                               :pOutLineaParBO
013600110913     C                                               :pOutSerieBO
013700110913     C                                               :pOutNumSpedBO
013800110913     C                                               :pOutAnnoFI
013900110913     C                                               :pOutLineaParFI
014000110913     C                                               :pOutSerieFI
014100110913     C                                               :pOutNumSpedFI
014200110913     C                                               :pOutDcmFI
014300110913     C                                               :pOutCcaFI))
014400110914     C*
014500110914     C                   IF        wEsito2 = '0'
014600110914     C                   EVAL      wDataCons = pOutDcmFI
014700110914     C                   EVAL      wConsAnom = pOutCcaFI
014800110914     C                   ELSE
014900110914     C                   EVAL      wDataCons = pOutDcmBO
015000110914     C                   EVAL      wConsAnom = pOutCcaBO
015100110914     C                   ENDIF
015200110913     C*
015300110913     C                   SELECT
015400110913     C* Consegna OK
015500110914     C                   WHEN      wDataCons > *zeros AND
015600110914     C                             wConsAnom = *blanks
015700110914     C                   EVAL      wDCMC = wDataCons
015800150526     C                   EVAL      wHMCC = tasHMC
015900110914     C                   EVAL      wTLT  = tasNRC / 24
016000110913     C* Reso
016100110914     C                   WHEN      wDataCons > *zeros AND
016200110914     C                             wConsAnom = '2'
016300110914     C                   EVAL      wDCM2 = wDataCons
016400110914     C*
016500110913     C                   ENDSL
016600071205     C*
016700110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
016800120214     C                   MOVEL     *blanks       wSegno            1
016900120214     C                   IF        wTLT < *zeros
017000120214     C                   EVAL      wSegno = '-'
017100120214     C                   ENDIF
017200120214     C*
017300120214     C  N60              EVAL      wNRC = %trim(wSegno +
017400120214     C                                    %trim(%editw(wTLT:'   0 , ')))
017500120214     C   60              EVAL      wNRC = %trim(wSegno +
017600120214     C                                    %trim(%editw(wTLT:'   0 . ')))
017700120214     C*
017800120214     C  N60              EVAL      wPKB = %trim(%editw(tasPKB:'     0 , '))
017900120214     C   60              EVAL      wPKB = %trim(%editw(tasPKB:'     0 . '))
018000120214     C  N60              EVAL      wVLB = %trim(%editw(tasVLB:'   0 ,   '))
018100120214     C   60              EVAL      wVLB = %trim(%editw(tasVLB:'   0 .   '))
018200120214     C*
018300120214     C* Forzatura in caso d spedizione ancora nn "chiusa"
018400120214     C                   IF        wDCMC = *zeros AND wDCM2 = *zeros
018500120214     C                   EVAL      wNRC = 'PENDING'
018600120214     C                   ENDIF
018700120214     C*
018800120214     C* Reperisco le descrizioni delle filiali partenza/arrivo
018900120214     C                   EVAL      wLNPdes = %trim(%editc(tasLNP:'X'))
019000120214     C                   EVAL      wLNAdes = %trim(%editc(tasLNA:'X'))
019100120214     C*
019200120214     C     tasLNP        CHAIN     azorg01l
019300120214     C                   IF        %found(azorg01l)
019400120214     C                   EVAL      wLNPdes = %trim(orgDES)+' - '+%trim(wLNPdes)
019500120214     C                   ENDIF
019600120214     C*
019700120214     C     tasLNA        CHAIN     azorg01l
019800120214     C                   IF        %found(azorg01l)
019900120214     C                   EVAL      wLNAdes = %trim(orgDES)+' - '+%trim(wLNAdes)
020000120214     C                   ENDIF
020100120214     C
020200110810     C*
020300110810     C                   EVAL      VAWDTA  =
020400120117     C                             %trim(%editc(vacCCM:'X'))            +sepFld+
020500120117     C                             %trim(%editc(vacRMN:'Q'))            +sepFld+
020600110810     C                             %trim(tasRSD)                        +sepFld+
020700110810     C                             %trim(tasCAD)                        +sepFld+
020800110810     C                             %trim(tasLOD)                        +sepFld+
020900120214     C                             %trim(tasPRD)                        +sepFld+
021000120117     C                             %trim(%editc(tasAAS:'X'))+'/'+
021100120117     C                             %trim(%editc(tasLNP:'X'))+'/'+
021200120117     C                             %trim(%editc(tasNRS:'X'))+'/'+
021300120117     C                             %trim(%editc(tasNSP:'X'))            +sepFld+
021400120214     C                             %trim(%editc(tasNCL:'1'))            +sepFld+
021500120214     C                             %trim(wPKB)                          +sepFld+
021600120214     C                             %trim(wVLB)                          +sepFld+
021700120117     C                             %trim(UBFMTDATE_Convert(
021800120117     C                                  %editc(tasDRT:'4') :
021900120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
022000120117     C                             %trim(UBFMTDATE_Convert(
022100120117     C                                  %editc(tasDUC:'4') :
022200120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
022300120117     C                             %trim(UBFMTDATE_Convert(
022400120117     C                                  %editc(tasDTI:'4') :
022500120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
022600120117     C                             %trim(UBFMTDATE_Convert(
022700120117     C                                  %editc(wDCMC:'4') :
022800120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
022900150526     C                             %trim(%editw(wHMCC:' 0 :  '))        +sepFld+
023000120117     C                             %trim(UBFMTDATE_Convert(
023100120117     C                                  %editc(wDCM2:'4') :
023200120117     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
023300120214     C                             %trim(UBFMTDATE_Convert(
023400120214     C                                  %editc(tasDCR:'4') :
023500120214     C                                  'YYYYMMDD' : 'DD/MM/YYYY'))     +sepFld+
023600120214     C                             %trim(wLNPdes)                       +sepFld+
023700120214     C                             %trim(wLNAdes)                       +sepFld+
023800120214     C                             tasFTC + tasTC2                      +sepFld+
023900120224     C                             %char(wTRC)                          +sepFld+
024000110913     C                             %trim(wNRC)                          +sepFld
024100120117     C*
024200150525     C* Se richiesto nei parametri => restituisco solo i porti franco
024300150525     C                   IF        %subst(prmppt:4:1) = 'S'
024400150525     C                   IF        %subst(tasTBL:1:1)='F'
024500120117     C                   WRITE     TIVAW000
024600120117     C                   ENDIF
024700150525     C                   ELSE
024800150525     C                   WRITE     TIVAW000
024900150525     C                   ENDIF
025000120117     C*
025100110810     C                   ENDIF
025200111109     C*
025300111109     C                   ENDIF
025400110810     C*
025500090325     C                   delete    tivgd000
025600991027     C*
025700090325     C                   READ      tivgdtmp
025800030325     C                   ENDDO
025900991027     C*
026000030325     C                   EVAL      wrkesito = '0'
026100991027     C*
026200910830     C                   ENDSR
026300991027
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
029300120224     C*
029400120224     C* Chiave su WADLY05L - Parziale
029500120224     C     KEYdly05L_P   KLIST
029600120224     C                   KFLD                    tasLNP
029700120224     C                   KFLD                    tasTSP
029800120224     C                   KFLD                    tasNZD
029900120224     C                   KFLD                    tasCAD
030000110810     C*
030100991027     C                   ENDSR
