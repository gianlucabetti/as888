000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400030709     FTITAS30C  IF   E           K DISK
000500110913     FTIGCP51L  IF   E           K DISK
000600090325     Ftivgdtmp  uF   E             DISK
000700100209     FTIVAWWWT  UF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200110913     D fnvacds       e ds                  extname(fnvac00t)
001300160208     D DTA4A         E DS
001400070316     D sepFld          s              1
001500110810
001600110810
001700110913     D wTLT            s              5S 1 inz
001800110913     D wGGS            s                   inz like(gcpGGS)
001900110913     D wDCMC           s                   inz like(vacDCM)
002000110913     D wDCM2           s                   inz like(vacDCM)
002100110913     D wNRC            s              6    inz
002200110913
002300110913
002400110913     D*------------------
002500110913     D* LINKING A DEFINIZIONI ESTERNE
002600110913     D*------------------
002700160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
002800110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002900110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003000110913
003100110810
003200991027
003300030908     C                   EXSR      chkRECFILMBR
003400991027     C                   EXSR      traduci
003500160208     C*
003600160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
003700160208     C                   CALL      'UBTA400R'
003800160208     C                   PARM                    UBTA4IOPZ
003900160208     C                   PARM      'C'           UBTA4ITLA
004000160208     C                   PARM                    UBTA4IAAS
004100160208     C                   PARM                    UBTA4ILNP
004200160208     C                   PARM                    UBTA4INRS
004300160208     C                   PARM                    UBTA4INSP
004400160208     C                   PARM                    UBTA4ITRC
004500160208     C                   PARM                    UBTA4OERR
004600160208     C                   PARM                    UBTA4ODS
004700160208     C                   PARM                    UBTA4OLEN
004800160208     C                   PARM                    UBTA4ODATI
004900160208     C*
005000921023     C                   SETON                                        LR
005100030908
005200110810
005300110810
005400030908     C     chkRECFILMBR  BEGSR
005500030908     C*
005600100209     C                   READ      TIVAWWWT                               55
005700030908     C*
005800030908     C                   ENDSR
005900110810
006000110810
006100991027
006200991027     C     traduci       BEGSR
006300030325     C*
006400090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
006500090427     C* (descrizione campi in inglese)
006600090427     C                   IF        %subst(prmppt:1:1) = 'E' AND *IN55 = *ON
006700100209     C                   EVAL      VAWDTA =
006800110913     C                             '"Date of Arrival departure dpt"'    +sepFld+
006900110913     C                             '"Date of Departure"'                +sepFld+
007000110913     C                             '"Delivery note nbr"'                +sepFld+
007100110913     C                             '"Casio Division"'                   +sepFld+
007200110913     C                             '"Consignee name"'                   +sepFld+
007300110913     C                             '"Consignee zip code"'               +sepFld+
007400110913     C                             '"Consignee city"'                   +sepFld+
007500120307     C                             '"BRT shipment code"'                +sepFld+
007600110913     C                             '"Date of Arrival arrival dpt"'      +sepFld+
007700110913     C                             '"Date of delivery"'                 +sepFld+
007800110913     C                             '"Date of return"'                   +sepFld+
007900110913     C                             '"Days of holding goods"'            +sepFld+
008000110913     C                             '"Transport lead time"'              +sepFld
008100090427     C*
008200100209     C                   WRITE     TIVAW000
008300090427     C                   ENDIF
008400991027     C*
008500090325     C                   READ      tivgdtmp
008600090325     C                   DOW       not %eof
008700110914     c                   movel     vgddta        fnvacds
008800041220     C*
008900110810     C* Inizializzo i campi d wrk
009000110913     C                   Z-ADD     *zeros        wTLT
009100110913     C                   Z-ADD     *zeros        wGGS
009200110913     C                   Z-ADD     *zeros        wDCMC
009300110913     C                   Z-ADD     *zeros        wDCM2
009400030709     C*
009500110913     C* Reperisco i dati NON presenti sul FNVAC00T dal TITAS30C
009600110810     C     KEYtas30C_C   CHAIN     titas30c
009700030709     C                   IF        %found(titas30c)
009800160208     C*
009900160208     C* Reperisco NATURA MERCE
010000160208     C                   EXSR      RTVNAS
010100110913     C*
010200110913     C* Quindi aggancio l'ultima giacenza
010300110913     C                   EVAL      gcpFRG = *zeros
010400110913     C     KEYgcp51_C    CHAIN     tigcp51l
010500110913     C                   IF        %found(tigcp51l)
010600110913     C*
010700110913     C* Se giacenza NN annullata la considero
010800110913     C                   IF        gcpATB = *blanks
010900110913     C                   EVAL      wGGS = gcpGGS
011000110913     C                   ENDIF
011100110913     C                   ENDIF
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
015600110913     C*
015700110913     C                   SELECT
015800110913     C* Consegna OK
015900110914     C                   WHEN      wDataCons > *zeros AND
016000110914     C                             wConsAnom = *blanks
016100110914     C                   EVAL      wDCMC = wDataCons
016200110914     C                   EVAL      wTLT  = tasNRC / 24
016300110913     C* Reso
016400110914     C                   WHEN      wDataCons > *zeros AND
016500110914     C                             wConsAnom = '2'
016600110914     C                   EVAL      wDCM2 = wDataCons
016700110914     C*
016800110913     C                   ENDSL
016900071205     C*
017000110810     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
017100110914     C  N60              EVAL      wNRC = %trim(%editw(wTLT:'   0 , -'))
017200110914     C   60              EVAL      wNRC = %trim(%editw(wTLT:'   0 . -'))
017300110810     C*
017400110810     C                   EVAL      VAWDTA  =
017500110913     C                             %trim(%editc(TASDRT:'Q'))            +sepFld+
017600110913     C                             %trim(%editc(TASDUC:'Q'))            +sepFld+
017700110914     C                             %trim(%editc(VACRMN:'Q'))            +sepFld+
017800160208     C                             %trim(§TA4ANAS)                      +sepFld+
017900110810     C                             %trim(tasRSD)                        +sepFld+
018000110810     C                             %trim(tasCAD)                        +sepFld+
018100110810     C                             %trim(tasLOD)                        +sepFld+
018200110913     C                             %trim(%char(tasAAS))+'/'+
018300110913     C                             %trim(%char(tasLNP))+'/'+
018400110913     C                             %trim(%char(tasNRS))+'/'+
018500110913     C                             %trim(%char(tasNSP))                 +sepFld+
018600110913     C                             %trim(%editc(tasDTI:'Q'))            +sepFld+
018700110914     C                             %trim(%editc(wDCMC:'Q'))             +sepFld+
018800110914     C                             %trim(%editc(wDCM2:'Q'))             +sepFld+
018900110913     C                             %trim(%editc(wGGS:'Q'))              +sepFld+
019000110913     C                             %trim(wNRC)                          +sepFld
019100930409     C*
019200100209     C                   WRITE     TIVAW000
019300110810     C                   ENDIF
019400111109     C*
019500111109     C                   ENDIF
019600110810     C*
019700090325     C                   delete    tivgd000
019800991027     C*
019900090325     C                   READ      tivgdtmp
020000030325     C                   ENDDO
020100991027     C*
020200030325     C                   EVAL      wrkesito = '0'
020300991027     C*
020400910830     C                   ENDSR
020500160208
020600160208
020700160208
020800160208     C     RTVNAS        BEGSR
020900160208     C*
021000160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
021100160208     C                   CLEAR                   DTA4A
021200160208     C*
021300160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
021400160208     C                   CALL      'UBTA400R'
021500160208     C                   PARM      *blanks       UBTA4IOPZ
021600160208     C                   PARM      *blanks       UBTA4ITLA
021700160208     C                   PARM      tasAAS        UBTA4IAAS
021800160208     C                   PARM      tasLNP        UBTA4ILNP
021900160208     C                   PARM      tasNRS        UBTA4INRS
022000160208     C                   PARM      tasNSP        UBTA4INSP
022100160208     C                   PARM      'A'           UBTA4ITRC
022200160208     C                   PARM                    UBTA4OERR
022300160208     C                   PARM                    UBTA4ODS
022400160208     C                   PARM                    UBTA4OLEN
022500160208     C                   PARM                    UBTA4ODATI
022600160208     C*
022700160208     C                   IF        UBTA4OERR = *zeros
022800160208     C                   SELECT
022900160208     C* Gestione output tipo record 'A'
023000160208     C                   WHEN      UBTA4ODS = 'DTA4A'
023100160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
023200160208     C                   ENDSL
023300160208     C*
023400160208     C                   ENDIF
023500160208     C*
023600160208     C                   ENDSR
023700991027
023800110810
023900110810
024000991027      /TITLE Operazioni iniziali.
024100991027     C     *inzsr        BEGSR
024200991027     C*
024300991027     C     *ENTRY        PLIST
024400991027     C                   parm                    prmppt
024500991027     C     wrkesito      parm      wrkesito      prmesito
024600070316     C*
024700070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
024800070316     C                   eval      sepFld = %subst(prmppt:2:1)
024900070316     C                   if        sepFld = *blanks
025000070316     C                   eval      sepFld = ','
025100070316     C                   endif
025200071205     C*
025300071205     C* Verifico se richiesto un particolare carattere d separatore decimale
025400071205     C                   SETON                                        60
025500071205     C                   IF        %subst(prmppt:3:1) = ','
025600071205     C                   SETOFF                                       60
025700071205     C                   ENDIF
025800030709     C*
025900030709     C* Definizione chiavi
026000030709     C*
026100030709     C* Chiave su TITAS30C - Completa
026200110810     C     KEYtas30C_C   KLIST
026300110913     C                   KFLD                    vacAAS
026400110913     C                   KFLD                    vacLNP
026500110913     C                   KFLD                    vacNRS
026600110913     C                   KFLD                    vacNSP
026700110913     C*
026800110913     C* Chiave su TIGCP51L - Completa
026900110913     C     KEYgcp51_C    KLIST
027000110913     C                   KFLD                    vacAAS
027100110913     C                   KFLD                    vacLNP
027200110913     C                   KFLD                    vacNRS
027300110913     C                   KFLD                    vacNSP
027400110913     C                   KFLD                    gcpFRG
027500110810     C*
027600991027     C                   ENDSR
