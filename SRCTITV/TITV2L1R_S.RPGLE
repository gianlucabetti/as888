000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200120424     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400120502     Fazorg01l  IF   e           K DISK
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
001500120502     D wVLF            s             14    inz
001600120502     D wPKB            s             14    inz
001700120503     D wPVL            s             14    inz
001800120502     D wPOR            s             14    inz
001900120502     D wIMV            s             14    inz
002000120503     D wDataFatt       s             10    inz
002100120424
002200120424
002300120424     D*------------------
002400120424     D* LINKING A DEFINIZIONI ESTERNE
002500120424     D*------------------
002600120424     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
002700120424     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
002800991027
002900100224
003000100224
003100030908     C                   EXSR      chkRECFILMBR
003200991027     C                   EXSR      traduci
003300921023     C                   SETON                                        LR
003400030908
003500100224
003600030908     C     chkRECFILMBR  BEGSR
003700030908     C*
003800100209     C                   READ      TIVAWWWT                               55
003900030908     C*
004000030908     C                   ENDSR
004100100224
004200120502
004300991027
004400991027     C     traduci       BEGSR
004500030325     C*
004600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004700090427     C* (nomi campi)
004800030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004900100209     C                   EVAL      VAWDTA =
005000120502     C                             '"VAFDFT"'                           +sepFld+
005100120424     C                             '"VAFNFT"'                           +sepFld+
005200120502     C                             '"VAFLNP"'                           +sepFld+
005300120502     C                             '"VAFNRS"'                           +sepFld+
005400120502     C                             '"VAFNSP"'                           +sepFld+
005500120502     C                             '"VAFMGS"'                           +sepFld+
005600120502     C                             '"VAFRMN"'                           +sepFld+
005700120502     C                             '"VAFPRO_LNP"'                       +sepFld+
005800120502     C                             '"VAFPRD"'                           +sepFld+
005900120424     C                             '"VAFRSD"'                           +sepFld+
006000120424     C                             '"VAFCAD"'                           +sepFld+
006100120502     C                             '"VAFNCL"'                           +sepFld+
006200120502     C                             '"VAFVLF"'                           +sepFld+
006300120502     C                             '"VAFPKB"'                           +sepFld+
006400120503     C                             '"VAFPVL"'                           +sepFld+
006500120502     C                             '"VAFPOR"'                           +sepFld+
006600120502     C                             '"VAFIMV"'                           +sepFld
006700030325     C*
006800100209     C                   WRITE     TIVAW000
006900030325     C                   ENDIF
007000090427     C*
007100090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
007200090427     C* (descrizione campi)
007300090427     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
007400100209     C                   EVAL      VAWDTA =
007500120502     C                             '"DATA FATTURA"'                     +sepFld+
007600120424     C                             '"NUMERO FATTURA"'                   +sepFld+
007700120502     C                             '"FILIALE PARTENZA SPED."'           +sepFld+
007800120502     C                             '"SERIE SPED."'                      +sepFld+
007900120502     C                             '"NUMERO SPED."'                     +sepFld+
008000120502     C                             '"DATA SPED."'                       +sepFld+
008100120502     C                             '"RIFERIMENTO MITTENTE"'             +sepFld+
008200120502     C                             '"PROV. PARTENZA"'                   +sepFld+
008300120502     C                             '"PROV. ARRIVO"'                     +sepFld+
008400120424     C                             '"DESTINATARIO"'                     +sepFld+
008500120502     C                             '"CAP DESTINATARIO"'                 +sepFld+
008600120502     C                             '"COLLI"'                            +sepFld+
008700120502     C                             '"VOLUME"'                           +sepFld+
008800120502     C                             '"PESO"'                             +sepFld+
008900120503     C                             '"PESO TASSABILE"'                   +sepFld+
009000120502     C                             '"TRASPORTO"'                        +sepFld+
009100120502     C                             '"IMPONIBILE"'                       +sepFld
009200090427     C*
009300100209     C                   WRITE     TIVAW000
009400090427     C                   ENDIF
009500991027     C*
009600090325     C                   READ      tivgdtmp
009700090325     C                   DOW       not %eof
009800100224     C                   movel     vgddta        tivafds
009900100224     C*
010000120502     C                   clear                   wDataFatt
010100120502     C                   clear                   wVLF
010200120502     C                   clear                   wPKB
010300120503     C                   clear                   wPVL
010400120502     C                   clear                   wPOR
010500120502     C                   clear                   wIMV
010600030709     C*
010700100224     C* Reperisco i dati NON presenti sul TIVAF00T
010800120424     C     KEYTAS30_1P   CHAIN     titas30c
010900120424     C                   IF        %found(titas30c)
011000120424     C*
011100120502     C* Chain su azorg01l per reperire la provincia della filiale di partenza
011200120502     C     vafLNP        CHAIN     azorg01l
011300120502     C                   IF        %found(azorg01l)
011400071205     C*
011500071205     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
011600120502     C  N60              EVAL      wVLF = %trim(%editw(vafVLF:'0 ,   '))
011700120502     C   60              EVAL      wVLF = %trim(%editw(vafVLF:'0 .   '))
011800120502     C  N60              EVAL      wPKB = %trim(%editw(vafPKB:'    0 , '))
011900120502     C   60              EVAL      wPKB = %trim(%editw(vafPKB:'    0 . '))
012000120503     C  N60              EVAL      wPVL = %trim(%editw(tasPVL:'    0 , '))
012100120503     C   60              EVAL      wPVL = %trim(%editw(tasPVL:'    0 . '))
012200120502     C  N60              EVAL      wPOR = %trim(%editw(vafPOR:'      0 ,   '))
012300120502     C   60              EVAL      wPOR = %trim(%editw(vafPOR:'      0 .   '))
012400120502     C  N60              EVAL      wIMV = %trim(%editw(vafIMV:'        0 ,   '))
012500120502     C   60              EVAL      wIMV = %trim(%editw(vafIMV:'        0 .   '))
012600100224     C*
012700120424     C                   EVAL      wDataFatt =  %trim(UBFMTDATE_Convert(
012800120424     C                              %editc(vafDFT:'4'):'YYYYMMDD':'DD/MM/YYYY'))
012900120424     C*
013000100224     C                   EVAL      VAWDTA  =
013100120502     C                                                     wDataFatt  +sepFld+
013200120424     C                                       %trim(%editc(vafNFT:'X'))+sepFld+
013300120502     C                                       %trim(%editc(vafLNP:'X'))+sepFld+
013400120502     C                                       %trim(%editc(vafNRS:'X'))+sepFld+
013500120424     C                                       %trim(%editc(vafNSP:'X'))+sepFld+
013600120502     C                                       %trim(%editc(vafMGS:'X'))+sepFld+
013700120502     C                                       %trim(%editc(vafRMN:'P'))+sepFld+
013800120502     C                                              %trim(orgPRO)     +sepFld+
013900120502     C                                              %trim(tasPRD)     +sepFld+
014000120502     C                                              %trim(tasRSD)     +sepFld+
014100120502     C                                              %trim(tasCAD)     +sepFld+
014200120502     C                                       %trim(%editc(vafNCL:'X'))+sepFld+
014300120502     C                                              %trim(wVLF)       +sepFld+
014400120502     C                                              %trim(wPKB)       +sepFld+
014500120503     C                                              %trim(wPVL)       +sepFld+
014600120502     C                                              %trim(wPOR)       +sepFld+
014700120502     C                                              %trim(wIMV)       +sepFld
014800930409     C*
014900100209     C                   WRITE     TIVAW000
015000090325     C                   delete    tivgd000
015100120424     C*
015200120424     C                   ENDIF
015300120424     C                   ENDIF
015400991027     C*
015500090325     C                   READ      tivgdtmp
015600030325     C                   ENDDO
015700991027     C*
015800030325     C                   EVAL      wrkesito = '0'
015900991027     C*
016000910830     C                   ENDSR
016100991027
016200120424
016300120424
016400991027      /TITLE Operazioni iniziali.
016500991027     C     *inzsr        BEGSR
016600991027     C*
016700991027     C     *ENTRY        PLIST
016800991027     C                   parm                    prmppt
016900991027     C     wrkesito      parm      wrkesito      prmesito
017000070316     C*
017100070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
017200070316     C                   eval      sepFld = %subst(prmppt:2:1)
017300070316     C                   if        sepFld = *blanks
017400070316     C                   eval      sepFld = ','
017500070316     C                   endif
017600071205     C*
017700071205     C* Verifico se richiesto un particolare carattere d separatore decimale
017800071205     C                   SETON                                        60
017900071205     C                   IF        %subst(prmppt:3:1) = ','
018000071205     C                   SETOFF                                       60
018100071205     C                   ENDIF
018200120424     C*
018300120424     C* DEFINIZIONE CHIAVI
018400120424     C*
018500120424     C* Chiave su TITAS30C - Parziale
018600120424     C     KEYTAS30_1P   KLIST
018700120424     C                   KFLD                    vafAAS
018800120424     C                   KFLD                    vafLNP
018900120424     C                   KFLD                    vafNRS
019000120424     C                   KFLD                    vafNSP
019100120424     C*
019200991027     C                   ENDSR
