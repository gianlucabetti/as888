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
010500180308     C                             '"COD. CLIENTE"'                     +sepFld+
010600180308     C                             '"COD. BOLLA"'                       +sepFld+
010700120502     C                             '"RIFERIMENTO MITTENTE"'             +sepFld+
010800120502     C                             '"PROV. PARTENZA"'                   +sepFld+
010900120502     C                             '"PROV. ARRIVO"'                     +sepFld+
011000120424     C                             '"DESTINATARIO"'                     +sepFld+
011100120502     C                             '"CAP DESTINATARIO"'                 +sepFld+
011200120502     C                             '"COLLI"'                            +sepFld+
011300120502     C                             '"VOLUME"'                           +sepFld+
011400120502     C                             '"PESO"'                             +sepFld+
011500120503     C                             '"PESO TASSABILE"'                   +sepFld+
011600120502     C                             '"TRASPORTO"'                        +sepFld+
011700121011     C                             '"SIGLA VARIA 1"'                    +sepFld+
011800121011     C                             '"IMPORTO VARIA 1"'                  +sepFld+
011900121011     C                             '"SIGLA VARIA 2"'                    +sepFld+
012000121011     C                             '"IMPORTO VARIA 2"'                  +sepFld+
012100121011     C                             '"SIGLA VARIA 3"'                    +sepFld+
012200121011     C                             '"IMPORTO VARIA 3"'                  +sepFld+
012300121011     C                             '"SIGLA VARIA 4"'                    +sepFld+
012400121011     C                             '"IMPORTO VARIA 4"'                  +sepFld+
012500121011     C                             '"SIGLA VARIA 5"'                    +sepFld+
012600121011     C                             '"IMPORTO VARIA 5"'                  +sepFld+
012700121011     C                             '"SIGLA VARIA 6"'                    +sepFld+
012800121011     C                             '"IMPORTO VARIA 6"'                  +sepFld+
012900121011     C                             '"SIGLA VARIA 7"'                    +sepFld+
013000121011     C                             '"IMPORTO VARIA 7"'                  +sepFld+
013100121011     C                             '"SIGLA VARIA 8"'                    +sepFld+
013200121011     C                             '"IMPORTO VARIA 8"'                  +sepFld+
013300120502     C                             '"IMPONIBILE"'                       +sepFld
013400090427     C*
013500100209     C                   WRITE     TIVAW000
013600090427     C                   ENDIF
013700991027     C*
013800090325     C                   READ      tivgdtmp
013900090325     C                   DOW       not %eof
014000100224     C                   movel     vgddta        tivafds
014100100224     C*
014200120502     C                   clear                   wDataFatt
014300120502     C                   clear                   wVLF
014400120502     C                   clear                   wPKB
014500120503     C                   clear                   wPVL
014600120502     C                   clear                   wPOR
014700120502     C                   clear                   wIMV
014800121011     C                   clear                   wVA1
014900121011     C                   clear                   wVA2
015000121011     C                   clear                   wVA3
015100121011     C                   clear                   wVA4
015200121011     C                   clear                   wVA5
015300121011     C                   clear                   wVA6
015400121011     C                   clear                   wVA7
015500121011     C                   clear                   wVA8
015600030709     C*
015700100224     C* Reperisco i dati NON presenti sul TIVAF00T
015800120424     C     KEYTAS30_1P   CHAIN     titas30c
015900120424     C                   IF        %found(titas30c)
016000120424     C*
016100120502     C* Chain su azorg01l per reperire la provincia della filiale di partenza
016200120502     C     vafLNP        CHAIN     azorg01l
016300120502     C                   IF        %found(azorg01l)
016400071205     C*
016500071205     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
016600120502     C  N60              EVAL      wVLF = %trim(%editw(vafVLF:'0 ,   '))
016700120502     C   60              EVAL      wVLF = %trim(%editw(vafVLF:'0 .   '))
016800120502     C  N60              EVAL      wPKB = %trim(%editw(vafPKB:'    0 , '))
016900120502     C   60              EVAL      wPKB = %trim(%editw(vafPKB:'    0 . '))
017000120503     C  N60              EVAL      wPVL = %trim(%editw(tasPVL:'    0 , '))
017100120503     C   60              EVAL      wPVL = %trim(%editw(tasPVL:'    0 . '))
017200120502     C  N60              EVAL      wPOR = %trim(%editw(vafPOR:'      0 ,   '))
017300120502     C   60              EVAL      wPOR = %trim(%editw(vafPOR:'      0 .   '))
017400120502     C  N60              EVAL      wIMV = %trim(%editw(vafIMV:'        0 ,   '))
017500120502     C   60              EVAL      wIMV = %trim(%editw(vafIMV:'        0 .   '))
017600121011     C  N60              EVAL      wVA1 = %trim(%editw(vafVA1:'        0 ,   '))
017700121011     C   60              EVAL      wVA1 = %trim(%editw(vafVA1:'        0 .   '))
017800121011     C  N60              EVAL      wVA2 = %trim(%editw(vafVA2:'        0 ,   '))
017900121011     C   60              EVAL      wVA2 = %trim(%editw(vafVA2:'        0 .   '))
018000121011     C  N60              EVAL      wVA3 = %trim(%editw(vafVA3:'        0 ,   '))
018100121011     C   60              EVAL      wVA3 = %trim(%editw(vafVA3:'        0 .   '))
018200121011     C  N60              EVAL      wVA4 = %trim(%editw(vafVA4:'        0 ,   '))
018300121011     C   60              EVAL      wVA4 = %trim(%editw(vafVA4:'        0 .   '))
018400121011     C  N60              EVAL      wVA5 = %trim(%editw(vafVA5:'        0 ,   '))
018500121011     C   60              EVAL      wVA5 = %trim(%editw(vafVA5:'        0 .   '))
018600121011     C  N60              EVAL      wVA6 = %trim(%editw(vafVA6:'        0 ,   '))
018700121011     C   60              EVAL      wVA6 = %trim(%editw(vafVA6:'        0 .   '))
018800121011     C  N60              EVAL      wVA7 = %trim(%editw(vafVA7:'        0 ,   '))
018900121011     C   60              EVAL      wVA7 = %trim(%editw(vafVA7:'        0 .   '))
019000121011     C  N60              EVAL      wVA8 = %trim(%editw(vafVA8:'        0 ,   '))
019100121011     C   60              EVAL      wVA8 = %trim(%editw(vafVA8:'        0 .   '))
019200100224     C*
019300120424     C                   EVAL      wDataFatt =  %trim(UBFMTDATE_Convert(
019400120424     C                              %editc(vafDFT:'4'):'YYYYMMDD':'DD/MM/YYYY'))
019500120424     C*
019600100224     C                   EVAL      VAWDTA  =
019700120502     C                                                     wDataFatt  +sepFld+
019800120424     C                                       %trim(%editc(vafNFT:'X'))+sepFld+
019900120502     C                                       %trim(%editc(vafLNP:'X'))+sepFld+
020000120502     C                                       %trim(%editc(vafNRS:'X'))+sepFld+
020100120424     C                                       %trim(%editc(vafNSP:'X'))+sepFld+
020200120502     C                                       %trim(%editc(vafMGS:'X'))+sepFld+
020300180308     C                                       %trim(%editc(vafKSC:'X'))+sepFld+
020400180227     C                                              %trim(tasCBO)     +sepFld+
020500120502     C                                       %trim(%editc(vafRMN:'P'))+sepFld+
020600120502     C                                              %trim(orgPRO)     +sepFld+
020700120502     C                                              %trim(tasPRD)     +sepFld+
020800120502     C                                              %trim(tasRSD)     +sepFld+
020900120502     C                                              %trim(tasCAD)     +sepFld+
021000120502     C                                       %trim(%editc(vafNCL:'X'))+sepFld+
021100120502     C                                              %trim(wVLF)       +sepFld+
021200120502     C                                              %trim(wPKB)       +sepFld+
021300120503     C                                              %trim(wPVL)       +sepFld+
021400120502     C                                              %trim(wPOR)       +sepFld+
021500121011     C                                              %trim(vafSV1)     +sepFld+
021600121011     C                                              %trim(wVA1)       +sepFld+
021700121011     C                                              %trim(vafSV2)     +sepFld+
021800121011     C                                              %trim(wVA2)       +sepFld+
021900121011     C                                              %trim(vafSV3)     +sepFld+
022000121011     C                                              %trim(wVA3)       +sepFld+
022100121011     C                                              %trim(vafSV4)     +sepFld+
022200121011     C                                              %trim(wVA4)       +sepFld+
022300121011     C                                              %trim(vafSV5)     +sepFld+
022400121011     C                                              %trim(wVA5)       +sepFld+
022500121011     C                                              %trim(vafSV6)     +sepFld+
022600121011     C                                              %trim(wVA6)       +sepFld+
022700121011     C                                              %trim(vafSV7)     +sepFld+
022800121011     C                                              %trim(wVA7)       +sepFld+
022900121011     C                                              %trim(vafSV8)     +sepFld+
023000121011     C                                              %trim(wVA8)       +sepFld+
023100120502     C                                              %trim(wIMV)       +sepFld
023200930409     C*
023300100209     C                   WRITE     TIVAW000
023400090325     C                   delete    tivgd000
023500120424     C*
023600120424     C                   ENDIF
023700120424     C                   ENDIF
023800991027     C*
023900090325     C                   READ      tivgdtmp
024000030325     C                   ENDDO
024100991027     C*
024200030325     C                   EVAL      wrkesito = '0'
024300991027     C*
024400910830     C                   ENDSR
024500991027
024600120424
024700120424
024800991027      /TITLE Operazioni iniziali.
024900991027     C     *inzsr        BEGSR
025000991027     C*
025100991027     C     *ENTRY        PLIST
025200991027     C                   parm                    prmppt
025300991027     C     wrkesito      parm      wrkesito      prmesito
025400070316     C*
025500070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
025600070316     C                   eval      sepFld = %subst(prmppt:2:1)
025700070316     C                   if        sepFld = *blanks
025800070316     C                   eval      sepFld = ','
025900070316     C                   endif
026000071205     C*
026100071205     C* Verifico se richiesto un particolare carattere d separatore decimale
026200071205     C                   SETON                                        60
026300071205     C                   IF        %subst(prmppt:3:1) = ','
026400071205     C                   SETOFF                                       60
026500071205     C                   ENDIF
026600120424     C*
026700120424     C* DEFINIZIONE CHIAVI
026800120424     C*
026900120424     C* Chiave su TITAS30C - Parziale
027000120424     C     KEYTAS30_1P   KLIST
027100120424     C                   KFLD                    vafAAS
027200120424     C                   KFLD                    vafLNP
027300120424     C                   KFLD                    vafNRS
027400120424     C                   KFLD                    vafNSP
027500120424     C*
027600991027     C                   ENDSR
