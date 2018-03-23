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
002000121011     D wVA1            s             12    inz
002100121011     D wVA2            s             12    inz
002200121011     D wVA3            s             12    inz
002300121011     D wVA4            s             12    inz
002400121011     D wVA5            s             12    inz
002500121011     D wVA6            s             12    inz
002600121011     D wVA7            s             12    inz
002700121011     D wVA8            s             12    inz
002800120503     D wDataFatt       s             10    inz
002900120424
003000120424
003100120424     D*------------------
003200120424     D* LINKING A DEFINIZIONI ESTERNE
003300120424     D*------------------
003400120424     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
003500120424     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
003600991027
003700100224
003800100224
003900030908     C                   EXSR      chkRECFILMBR
004000991027     C                   EXSR      traduci
004100921023     C                   SETON                                        LR
004200030908
004300100224
004400030908     C     chkRECFILMBR  BEGSR
004500030908     C*
004600100209     C                   READ      TIVAWWWT                               55
004700030908     C*
004800030908     C                   ENDSR
004900100224
005000120502
005100991027
005200991027     C     traduci       BEGSR
005300030325     C*
005400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005500090427     C* (nomi campi)
005600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005700100209     C                   EVAL      VAWDTA =
005800120502     C                             '"VAFDFT"'                           +sepFld+
005900120424     C                             '"VAFNFT"'                           +sepFld+
006000120502     C                             '"VAFLNP"'                           +sepFld+
006100120502     C                             '"VAFNRS"'                           +sepFld+
006200120502     C                             '"VAFNSP"'                           +sepFld+
006300120502     C                             '"VAFMGS"'                           +sepFld+
006400120502     C                             '"VAFRMN"'                           +sepFld+
006500120502     C                             '"VAFPRO_LNP"'                       +sepFld+
006600120502     C                             '"VAFPRD"'                           +sepFld+
006700120424     C                             '"VAFRSD"'                           +sepFld+
006800120424     C                             '"VAFCAD"'                           +sepFld+
006900120502     C                             '"VAFNCL"'                           +sepFld+
007000120502     C                             '"VAFVLF"'                           +sepFld+
007100120502     C                             '"VAFPKB"'                           +sepFld+
007200120503     C                             '"VAFPVL"'                           +sepFld+
007300120502     C                             '"VAFPOR"'                           +sepFld+
007400121011     C                             '"VAFSV1"'                           +sepFld+
007500121011     C                             '"VAFVA1"'                           +sepFld+
007600121011     C                             '"VAFSV2"'                           +sepFld+
007700121011     C                             '"VAFVA2"'                           +sepFld+
007800121011     C                             '"VAFSV3"'                           +sepFld+
007900121011     C                             '"VAFVA3"'                           +sepFld+
008000121011     C                             '"VAFSV4"'                           +sepFld+
008100121011     C                             '"VAFVA4"'                           +sepFld+
008200121011     C                             '"VAFSV5"'                           +sepFld+
008300121011     C                             '"VAFVA5"'                           +sepFld+
008400121011     C                             '"VAFSV6"'                           +sepFld+
008500121011     C                             '"VAFVA6"'                           +sepFld+
008600121011     C                             '"VAFSV7"'                           +sepFld+
008700121011     C                             '"VAFVA7"'                           +sepFld+
008800121011     C                             '"VAFSV8"'                           +sepFld+
008900121011     C                             '"VAFVA8"'                           +sepFld+
009000120502     C                             '"VAFIMV"'                           +sepFld
009100030325     C*
009200100209     C                   WRITE     TIVAW000
009300030325     C                   ENDIF
009400090427     C*
009500090427     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
009600090427     C* (descrizione campi)
009700090427     C                   IF        %subst(prmppt:1:1) = 'D' AND *IN55 = *ON
009800100209     C                   EVAL      VAWDTA =
009900120502     C                             '"DATA FATTURA"'                     +sepFld+
010000120424     C                             '"NUMERO FATTURA"'                   +sepFld+
010100120502     C                             '"FILIALE PARTENZA SPED."'           +sepFld+
010200120502     C                             '"SERIE SPED."'                      +sepFld+
010300120502     C                             '"NUMERO SPED."'                     +sepFld+
010400120502     C                             '"DATA SPED."'                       +sepFld+
010500120502     C                             '"RIFERIMENTO MITTENTE"'             +sepFld+
010600120502     C                             '"PROV. PARTENZA"'                   +sepFld+
010700120502     C                             '"PROV. ARRIVO"'                     +sepFld+
010800120424     C                             '"DESTINATARIO"'                     +sepFld+
010900120502     C                             '"CAP DESTINATARIO"'                 +sepFld+
011000120502     C                             '"COLLI"'                            +sepFld+
011100120502     C                             '"VOLUME"'                           +sepFld+
011200120502     C                             '"PESO"'                             +sepFld+
011300120503     C                             '"PESO TASSABILE"'                   +sepFld+
011400120502     C                             '"TRASPORTO"'                        +sepFld+
011500121011     C                             '"SIGLA VARIA 1"'                    +sepFld+
011600121011     C                             '"IMPORTO VARIA 1"'                  +sepFld+
011700121011     C                             '"SIGLA VARIA 2"'                    +sepFld+
011800121011     C                             '"IMPORTO VARIA 2"'                  +sepFld+
011900121011     C                             '"SIGLA VARIA 3"'                    +sepFld+
012000121011     C                             '"IMPORTO VARIA 3"'                  +sepFld+
012100121011     C                             '"SIGLA VARIA 4"'                    +sepFld+
012200121011     C                             '"IMPORTO VARIA 4"'                  +sepFld+
012300121011     C                             '"SIGLA VARIA 5"'                    +sepFld+
012400121011     C                             '"IMPORTO VARIA 5"'                  +sepFld+
012500121011     C                             '"SIGLA VARIA 6"'                    +sepFld+
012600121011     C                             '"IMPORTO VARIA 6"'                  +sepFld+
012700121011     C                             '"SIGLA VARIA 7"'                    +sepFld+
012800121011     C                             '"IMPORTO VARIA 7"'                  +sepFld+
012900121011     C                             '"SIGLA VARIA 8"'                    +sepFld+
013000121011     C                             '"IMPORTO VARIA 8"'                  +sepFld+
013100120502     C                             '"IMPONIBILE"'                       +sepFld
013200090427     C*
013300100209     C                   WRITE     TIVAW000
013400090427     C                   ENDIF
013500991027     C*
013600090325     C                   READ      tivgdtmp
013700090325     C                   DOW       not %eof
013800100224     C                   movel     vgddta        tivafds
013900100224     C*
014000120502     C                   clear                   wDataFatt
014100120502     C                   clear                   wVLF
014200120502     C                   clear                   wPKB
014300120503     C                   clear                   wPVL
014400120502     C                   clear                   wPOR
014500120502     C                   clear                   wIMV
014600121011     C                   clear                   wVA1
014700121011     C                   clear                   wVA2
014800121011     C                   clear                   wVA3
014900121011     C                   clear                   wVA4
015000121011     C                   clear                   wVA5
015100121011     C                   clear                   wVA6
015200121011     C                   clear                   wVA7
015300121011     C                   clear                   wVA8
015400030709     C*
015500100224     C* Reperisco i dati NON presenti sul TIVAF00T
015600120424     C     KEYTAS30_1P   CHAIN     titas30c
015700120424     C                   IF        %found(titas30c)
015800120424     C*
015900120502     C* Chain su azorg01l per reperire la provincia della filiale di partenza
016000120502     C     vafLNP        CHAIN     azorg01l
016100120502     C                   IF        %found(azorg01l)
016200071205     C*
016300071205     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
016400120502     C  N60              EVAL      wVLF = %trim(%editw(vafVLF:'0 ,   '))
016500120502     C   60              EVAL      wVLF = %trim(%editw(vafVLF:'0 .   '))
016600120502     C  N60              EVAL      wPKB = %trim(%editw(vafPKB:'    0 , '))
016700120502     C   60              EVAL      wPKB = %trim(%editw(vafPKB:'    0 . '))
016800120503     C  N60              EVAL      wPVL = %trim(%editw(tasPVL:'    0 , '))
016900120503     C   60              EVAL      wPVL = %trim(%editw(tasPVL:'    0 . '))
017000120502     C  N60              EVAL      wPOR = %trim(%editw(vafPOR:'      0 ,   '))
017100120502     C   60              EVAL      wPOR = %trim(%editw(vafPOR:'      0 .   '))
017200120502     C  N60              EVAL      wIMV = %trim(%editw(vafIMV:'        0 ,   '))
017300120502     C   60              EVAL      wIMV = %trim(%editw(vafIMV:'        0 .   '))
017400121011     C  N60              EVAL      wVA1 = %trim(%editw(vafVA1:'        0 ,   '))
017500121011     C   60              EVAL      wVA1 = %trim(%editw(vafVA1:'        0 .   '))
017600121011     C  N60              EVAL      wVA2 = %trim(%editw(vafVA2:'        0 ,   '))
017700121011     C   60              EVAL      wVA2 = %trim(%editw(vafVA2:'        0 .   '))
017800121011     C  N60              EVAL      wVA3 = %trim(%editw(vafVA3:'        0 ,   '))
017900121011     C   60              EVAL      wVA3 = %trim(%editw(vafVA3:'        0 .   '))
018000121011     C  N60              EVAL      wVA4 = %trim(%editw(vafVA4:'        0 ,   '))
018100121011     C   60              EVAL      wVA4 = %trim(%editw(vafVA4:'        0 .   '))
018200121011     C  N60              EVAL      wVA5 = %trim(%editw(vafVA5:'        0 ,   '))
018300121011     C   60              EVAL      wVA5 = %trim(%editw(vafVA5:'        0 .   '))
018400121011     C  N60              EVAL      wVA6 = %trim(%editw(vafVA6:'        0 ,   '))
018500121011     C   60              EVAL      wVA6 = %trim(%editw(vafVA6:'        0 .   '))
018600121011     C  N60              EVAL      wVA7 = %trim(%editw(vafVA7:'        0 ,   '))
018700121011     C   60              EVAL      wVA7 = %trim(%editw(vafVA7:'        0 .   '))
018800121011     C  N60              EVAL      wVA8 = %trim(%editw(vafVA8:'        0 ,   '))
018900121011     C   60              EVAL      wVA8 = %trim(%editw(vafVA8:'        0 .   '))
019000100224     C*
019100120424     C                   EVAL      wDataFatt =  %trim(UBFMTDATE_Convert(
019200120424     C                              %editc(vafDFT:'4'):'YYYYMMDD':'DD/MM/YYYY'))
019300120424     C*
019400100224     C                   EVAL      VAWDTA  =
019500120502     C                                                     wDataFatt  +sepFld+
019600120424     C                                       %trim(%editc(vafNFT:'X'))+sepFld+
019700120502     C                                       %trim(%editc(vafLNP:'X'))+sepFld+
019800120502     C                                       %trim(%editc(vafNRS:'X'))+sepFld+
019900120424     C                                       %trim(%editc(vafNSP:'X'))+sepFld+
020000120502     C                                       %trim(%editc(vafMGS:'X'))+sepFld+
020100120502     C                                       %trim(%editc(vafRMN:'P'))+sepFld+
020200120502     C                                              %trim(orgPRO)     +sepFld+
020300120502     C                                              %trim(tasPRD)     +sepFld+
020400120502     C                                              %trim(tasRSD)     +sepFld+
020500120502     C                                              %trim(tasCAD)     +sepFld+
020600120502     C                                       %trim(%editc(vafNCL:'X'))+sepFld+
020700120502     C                                              %trim(wVLF)       +sepFld+
020800120502     C                                              %trim(wPKB)       +sepFld+
020900120503     C                                              %trim(wPVL)       +sepFld+
021000120502     C                                              %trim(wPOR)       +sepFld+
021100121011     C                                              %trim(vafSV1)     +sepFld+
021200121011     C                                              %trim(wVA1)       +sepFld+
021300121011     C                                              %trim(vafSV2)     +sepFld+
021400121011     C                                              %trim(wVA2)       +sepFld+
021500121011     C                                              %trim(vafSV3)     +sepFld+
021600121011     C                                              %trim(wVA3)       +sepFld+
021700121011     C                                              %trim(vafSV4)     +sepFld+
021800121011     C                                              %trim(wVA4)       +sepFld+
021900121011     C                                              %trim(vafSV5)     +sepFld+
022000121011     C                                              %trim(wVA5)       +sepFld+
022100121011     C                                              %trim(vafSV6)     +sepFld+
022200121011     C                                              %trim(wVA6)       +sepFld+
022300121011     C                                              %trim(vafSV7)     +sepFld+
022400121011     C                                              %trim(wVA7)       +sepFld+
022500121011     C                                              %trim(vafSV8)     +sepFld+
022600121011     C                                              %trim(wVA8)       +sepFld+
022700120502     C                                              %trim(wIMV)       +sepFld
022800930409     C*
022900100209     C                   WRITE     TIVAW000
023000090325     C                   delete    tivgd000
023100120424     C*
023200120424     C                   ENDIF
023300120424     C                   ENDIF
023400991027     C*
023500090325     C                   READ      tivgdtmp
023600030325     C                   ENDDO
023700991027     C*
023800030325     C                   EVAL      wrkesito = '0'
023900991027     C*
024000910830     C                   ENDSR
024100991027
024200120424
024300120424
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
026200120424     C*
026300120424     C* DEFINIZIONE CHIAVI
026400120424     C*
026500120424     C* Chiave su TITAS30C - Parziale
026600120424     C     KEYTAS30_1P   KLIST
026700120424     C                   KFLD                    vafAAS
026800120424     C                   KFLD                    vafLNP
026900120424     C                   KFLD                    vafNRS
027000120424     C                   KFLD                    vafNSP
027100120424     C*
027200991027     C                   ENDSR
