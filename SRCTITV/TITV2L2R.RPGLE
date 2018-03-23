000100120508     H DECEDIT('0.') DATEDIT(*DMY.)
000200120424     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400120508     Ftitas30c  IF   E           K DISK
000500090325     Ftivgdtmp  uF   E             DISK
000600100209     FTIVAWWWT  UF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100120508     D fnvacds       e ds                  extname(fnvac00t)
001200070316     D sepFld          s              1
001300071205
001400120508     D wPKB            s              8
001500120509     D wDataSped       s             10    inz
001600120508     D wDataCons       s             10    inz
001700120508
001800120508
001900120508     D*------------------
002000120508     D* LINKING A DEFINIZIONI ESTERNE
002100120508     D*------------------
002200120508     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
002300120508     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
002400120508
002500100224
002600100224
002700030908     C                   EXSR      chkRECFILMBR
002800991027     C                   EXSR      traduci
002900921023     C                   SETON                                        LR
003000030908
003100100224
003200030908     C     chkRECFILMBR  BEGSR
003300030908     C*
003400100209     C                   READ      TIVAWWWT                               55
003500030908     C*
003600030908     C                   ENDSR
003700100224
003800991027
003900991027     C     traduci       BEGSR
004000030325     C*
004100030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004200090427     C* (nomi campi)
004300030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004400100209     C                   EVAL      VAWDTA =
004500120508     C                             '"VACRSD"'                           +sepFld+
004600120508     C                             '"VACLOD"'                           +sepFld+
004700120508     C                             '"VACPRD"'                           +sepFld+
004800120508     C                             '"VACNAS"'                           +sepFld+
004900120508     C                             '"VACRMN"'                           +sepFld+
005000120508     C                             '"VACMGS"'                           +sepFld+
005100120508     C                             '"VACDCM"'                           +sepFld+
005200120508     C                             '"VACHMC"'                           +sepFld+
005300120508     C                             '"VACNCL"'                           +sepFld+
005400120508     C                             '"VACPKB"'                           +sepFld
005500030325     C*
005600100209     C                   WRITE     TIVAW000
005700030325     C                   ENDIF
005800090427     C*
005900090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
006000090427     C* (descrizione campi)
006100090427     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
006200100209     C                   EVAL      VAWDTA =
006300120508     C                             '"DESTINATARIO"'                     +sepFld+
006400120508     C                             '"LOCALITA DEST."'                   +sepFld+
006500120508     C                             '"PROVINCIA DEST."'                  +sepFld+
006600120508     C                             '"NATURA MERCE"'                     +sepFld+
006700120508     C                             '"RIF.MITTENTE SPED."'               +sepFld+
006800120508     C                             '"MESE/GIORNO SPED."'                +sepFld+
006900120508     C                             '"DATA CONSEGNA"'                    +sepFld+
007000120508     C                             '"ORA CONSEGNA"'                     +sepFld+
007100120508     C                             '"NUMERO COLLI"'                     +sepFld+
007200120508     C                             '"PESO"'                             +sepFld
007300090427     C*
007400100209     C                   WRITE     TIVAW000
007500090427     C                   ENDIF
007600991027     C*
007700090325     C                   READ      tivgdtmp
007800090325     C                   DOW       not %eof
007900120508     C                   movel     vgddta        fnvacds
008000120508     C*
008100120508     C* Ritorno solamente le spedizioni consegnate OK
008200120508     C                   IF        vacDCM > *zeros AND vacCCA = *blanks
008300030709     C*
008400100224     C* Reperisco i dati NON presenti sul TIVAF00T
008500120424     C     KEYTAS30_1P   CHAIN     titas30c
008600120424     C                   IF        %found(titas30c)
008700120508     C*
008800120508     C  N60              EVAL      wPKB = %trim(%editw(vacPKB:'    0 , '))
008900120508     C   60              EVAL      wPKB = %trim(%editw(vacPKB:'    0 . '))
009000120508     C*
009100120509     C                   EVAL      wDataSped =  %trim(UBFMTDATE_Convert(
009200120509     C                              %editc(vacAAS*10000+vacMGS:'4'):'YYYYMMDD':
009300120509     C                              'DD/MM/YYYY'))
009400120509     C*
009500120508     C                   EVAL      wDataCons =  %trim(UBFMTDATE_Convert(
009600120508     C                              %editc(vacDCM:'4'):'YYYYMMDD':'DD/MM/YYYY'))
009700120424     C*
009800100224     C                   EVAL      VAWDTA  =
009900120508     C                                       %trim(vacRSD)            +sepFld+
010000120508     C                                       %trim(tasLOD)            +sepFld+
010100120508     C                                       %trim(vacPRD)            +sepFld+
010200120508     C                                       %trim(vacNAS)            +sepFld+
010300120508     C                                       %trim(%editc(vacRMN:'P'))+sepFld+
010400120509     C                                                     wDataSped  +sepFld+
010500120508     C                                                     wDataCons  +sepFld+
010600120508     C                                       %trim(%editc(vacHMC:'X'))+sepFld+
010700120508     C                                       %trim(%editc(vacNCL:'X'))+sepFld+
010800120508     C                                                    %trim(wPKB) +sepFld
010900930409     C*
011000100209     C                   WRITE     TIVAW000
011100090325     C                   delete    tivgd000
011200120424     C*
011300120424     C                   ENDIF
011400120424     C                   ENDIF
011500991027     C*
011600090325     C                   READ      tivgdtmp
011700030325     C                   ENDDO
011800991027     C*
011900030325     C                   EVAL      wrkesito = '0'
012000991027     C*
012100910830     C                   ENDSR
012200991027
012300120424
012400120424
012500991027      /TITLE Operazioni iniziali.
012600991027     C     *inzsr        BEGSR
012700991027     C*
012800991027     C     *ENTRY        PLIST
012900991027     C                   parm                    prmppt
013000991027     C     wrkesito      parm      wrkesito      prmesito
013100070316     C*
013200070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
013300070316     C                   eval      sepFld = %subst(prmppt:2:1)
013400070316     C                   if        sepFld = *blanks
013500120508     C                   eval      sepFld = ';'
013600070316     C                   endif
013700120508     C*
013800120508     C* Verifico se richiesto un particolare carattere d separatore decimale
013900120508     C                   SETON                                        60
014000120508     C                   IF        %subst(prmppt:3:1) = ','
014100120508     C                   SETOFF                                       60
014200120508     C                   ENDIF
014300120424     C*
014400120424     C* DEFINIZIONE CHIAVI
014500120424     C*
014600120424     C* Chiave su TITAS30C - Parziale
014700120424     C     KEYTAS30_1P   KLIST
014800120508     C                   KFLD                    vacAAS
014900120508     C                   KFLD                    vacLNP
015000120508     C                   KFLD                    vacNRS
015100120508     C                   KFLD                    vacNSP
015200120424     C*
015300991027     C                   ENDSR
