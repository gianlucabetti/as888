000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200120424     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400120424     Fcnaco00f  IF   e           K DISK
000500120424     Ftitas30c  IF   E           K DISK
000600090325     Ftivgdtmp  uF   E             DISK
000700100209     FTIVAWWWT  UF A E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090325     D tivafds       e ds                  extname(tivaf00t)
001300070316     D sepFld          s              1
001400071205
001500120424     D wIMV            s             14    inz
001600120424     D wDataFatt       s             10    inz
001700120424     D wDataSped       s             10    inz
001800120424
001900120424
002000120424     D*------------------
002100120424     D* LINKING A DEFINIZIONI ESTERNE
002200120424     D*------------------
002300120424     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
002400120424     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
002500991027
002600100224
002700100224
002800030908     C                   EXSR      chkRECFILMBR
002900991027     C                   EXSR      traduci
003000921023     C                   SETON                                        LR
003100030908
003200100224
003300030908     C     chkRECFILMBR  BEGSR
003400030908     C*
003500100209     C                   READ      TIVAWWWT                               55
003600030908     C*
003700030908     C                   ENDSR
003800100224
003900991027
004000991027     C     traduci       BEGSR
004100030325     C*
004200030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004300090427     C* (nomi campi)
004400030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004500100209     C                   EVAL      VAWDTA =
004600120424     C                             '"VAFNFT"'                           +sepFld+
004700120424     C                             '"VAFDFT"'                           +sepFld+
004800120424     C                             '"SHPMNT_NBR"'                       +sepFld+
004900120424     C                             '"SHPMNT_DATE"'                      +sepFld+
005000120424     C                             '"MITTENTE_CODICE"'                  +sepFld+
005100120424     C                             '"MITTENTE_DESCR."'                  +sepFld+
005200120424     C                             '"VAFRSD"'                           +sepFld+
005300120424     C                             '"VAFIND"'                           +sepFld+
005400120424     C                             '"VAFCAD"'                           +sepFld+
005500120424     C                             '"VAFLOD"'                           +sepFld+
005600120424     C                             '"VAFPRD"'                           +sepFld+
005700120424     C                             '"VAFIMV"'                           +sepFld+
005800120424     C                             '"VAFKSC"'                           +sepFld+
005900120424     C                             '"VAFRMA"'                           +sepFld+
006000120424     C                             '"VAFRMN"'                           +sepFld
006100030325     C*
006200100209     C                   WRITE     TIVAW000
006300030325     C                   ENDIF
006400090427     C*
006500090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
006600090427     C* (descrizione campi)
006700090427     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
006800100209     C                   EVAL      VAWDTA =
006900120424     C                             '"NUMERO FATTURA"'                   +sepFld+
007000120424     C                             '"DATA FATTURA"'                     +sepFld+
007100100209     C                             '"NUMERO SPED."'                     +sepFld+
007200120424     C                             '"DATA SPED."'                       +sepFld+
007300120424     C                             '"MITTENTE CODICE"'                  +sepFld+
007400120424     C                             '"MITTENTE DESCR."'                  +sepFld+
007500120424     C                             '"DESTINATARIO"'                     +sepFld+
007600120424     C                             '"INDIRIZZO DEST."'                  +sepFld+
007700120424     C                             '"CAP DEST."'                        +sepFld+
007800120424     C                             '"LOCALITA DEST."'                   +sepFld+
007900120424     C                             '"PROVINCIA DEST."'                  +sepFld+
008000120424     C                             '"IMPONIBILE"'                       +sepFld+
008100120424     C                             '"UTENTE"'                           +sepFld+
008200120424     C                             '"REPARTO"'                          +sepFld+
008300120424     C                             '"RIFERIMENTO SPED."'                +sepFld
008400090427     C*
008500100209     C                   WRITE     TIVAW000
008600090427     C                   ENDIF
008700991027     C*
008800090325     C                   READ      tivgdtmp
008900090325     C                   DOW       not %eof
009000100224     C                   movel     vgddta        tivafds
009100100224     C*
009200120424     C                   clear                   wIMV
009300030709     C*
009400100224     C* Reperisco i dati NON presenti sul TIVAF00T
009500120424     C     KEYTAS30_1P   CHAIN     titas30c
009600120424     C                   IF        %found(titas30c)
009700120424     C*
009800120424     C* Chain su cnaco00f per reperire ragione sociale mittente
009900120424     C     KEYACO00_1C   CHAIN     cnaco00f
010000120424     C                   IF        %found(cnaco00f)
010100071205     C*
010200071205     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
010300071205     C  N60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 ,   '))
010400071205     C   60              EVAL      wIMV = %trim(%editw(VAFIMV:'        0 .   '))
010500100224     C*
010600120424     C                   EVAL      wDataFatt =  %trim(UBFMTDATE_Convert(
010700120424     C                              %editc(vafDFT:'4'):'YYYYMMDD':'DD/MM/YYYY'))
010800120424     C*
010900120424     C                   EVAL      wDataSped =  %trim(UBFMTDATE_Convert(
011000120424     C                              %editc(vafAAS*10000+vafMGS:'4'):
011100120424     C                                     'YYYYMMDD':'DD/MM/YYYY'))
011200120424     C*
011300100224     C                   EVAL      VAWDTA  =
011400120424     C                                       %trim(%editc(vafNFT:'X'))+sepFld+
011500120424     C                                                     wDataFatt  +sepFld+
011600120424     C                                       %trim(%editc(vafAAS:'X'))+'/'+
011700120424     C                                       %trim(%editc(vafLNP:'X'))+'/'+
011800120424     C                                       %trim(%editc(vafNRS:'X'))+'/'+
011900120424     C                                       %trim(%editc(vafNSP:'X'))+sepFld+
012000120424     C                                                     wDataSped  +sepFld+
012100120424     C                                       %trim(%editc(tasCCM:'X'))+sepFld+
012200120424     C                                              %trim(acoRAG)     +sepFld+
012300120424     C                                              %trim(tasRSD)     +sepFld+
012400120424     C                                              %trim(tasIND)     +sepFld+
012500120424     C                                              %trim(tasCAD)     +sepFld+
012600120424     C                                              %trim(tasLOD)     +sepFld+
012700120424     C                                              %trim(tasPRD)     +sepFld+
012800120424     C                                              %trim(wIMV)       +sepFld+
012900120424     C                                       %trim(%editc(tasKSC:'X'))+sepFld+
013000120424     C                                              %trim(vafRMA)     +sepFld+
013100120424     C                                       %trim(%editc(vafRMN:'P'))+sepFld
013200930409     C*
013300100209     C                   WRITE     TIVAW000
013400090325     C                   delete    tivgd000
013500120424     C*
013600120424     C                   ENDIF
013700120424     C                   ENDIF
013800991027     C*
013900090325     C                   READ      tivgdtmp
014000030325     C                   ENDDO
014100991027     C*
014200030325     C                   EVAL      wrkesito = '0'
014300991027     C*
014400910830     C                   ENDSR
014500991027
014600120424
014700120424
014800991027      /TITLE Operazioni iniziali.
014900991027     C     *inzsr        BEGSR
015000991027     C*
015100991027     C     *ENTRY        PLIST
015200991027     C                   parm                    prmppt
015300991027     C     wrkesito      parm      wrkesito      prmesito
015400070316     C*
015500070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
015600070316     C                   eval      sepFld = %subst(prmppt:2:1)
015700070316     C                   if        sepFld = *blanks
015800070316     C                   eval      sepFld = ','
015900070316     C                   endif
016000071205     C*
016100071205     C* Verifico se richiesto un particolare carattere d separatore decimale
016200071205     C                   SETON                                        60
016300071205     C                   IF        %subst(prmppt:3:1) = ','
016400071205     C                   SETOFF                                       60
016500071205     C                   ENDIF
016600120424     C*
016700120424     C* Inizializzazioni
016800120424     C                   EVAL      acoKUT = 1
016900120424     C                   EVAL      acoKCC = 151
017000120424     C*
017100120424     C* DEFINIZIONE CHIAVI
017200120424     C*
017300120424     C* Chiave su TITAS30C - Parziale
017400120424     C     KEYTAS30_1P   KLIST
017500120424     C                   KFLD                    vafAAS
017600120424     C                   KFLD                    vafLNP
017700120424     C                   KFLD                    vafNRS
017800120424     C                   KFLD                    vafNSP
017900120424     C*
018000120424     C* Chiave su CNACO00F - Completa
018100120424     C     KEYACO00_1C   KLIST
018200120424     C                   KFLD                    acoKUT
018300120424     C                   KFLD                    acoKCC
018400120424     C                   KFLD                    tasCCM
018500120424     C*
018600991027     C                   ENDSR
