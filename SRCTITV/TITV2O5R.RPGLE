000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400030709     FTITAS30C  IF   E           K DISK
000500090325     Ftivgdtmp  uF   E             DISK
000600100209     FTIVAWWWT  UF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100090325     D tivafds       e ds                  extname(tivaf00t)
001200070316     D sepFld          s              1
001300991027
001400030908     C                   EXSR      chkRECFILMBR
001500991027     C                   EXSR      traduci
001600921023     C                   SETON                                        LR
001700030908
001800130314
001900030908     C     chkRECFILMBR  BEGSR
002000030908     C*
002100100209     C                   READ      TIVAWWWT                               55
002200030908     C*
002300030908     C                   ENDSR
002400991027
002500130314
002600991027     C     traduci       BEGSR
002700030325     C*
002800030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
002900030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
003000100209     C                   EVAL      VAWDTA  = '"VAFAAS"'+sepFld+
003100070316     C                                       '"VAFLNP"'+sepFld+
003200070316     C                                       '"VAFNRS"'+sepFld+
003300070316     C                                       '"VAFNSP"'+sepFld+
003400070316     C                                       '"VAFTBL"'+sepFld+
003500070316     C                                       '"VAFMGS"'+sepFld+
003600070316     C                                       '"VAFKSC"'+sepFld+
003700070316     C                                       '"VAFLNA"'+sepFld+
003800070316     C                                       '"VAFNCL"'+sepFld+
003900070316     C                                       '"VAFPKB"'+sepFld+
004000070316     C                                       '"VAFVLF"'+sepFld+
004100070316     C                                       '"VAFQFT"'+sepFld+
004200070316     C                                       '"VAFPOR"'+sepFld+
004300070316     C                                       '"VAFSV1"'+sepFld+
004400070316     C                                       '"VAFVA1"'+sepFld+
004500070316     C                                       '"VAFSV2"'+sepFld+
004600070316     C                                       '"VAFVA2"'+sepFld+
004700070316     C                                       '"VAFSV3"'+sepFld+
004800070316     C                                       '"VAFVA3"'+sepFld+
004900070316     C                                       '"VAFSV4"'+sepFld+
005000070316     C                                       '"VAFVA4"'+sepFld+
005100070316     C                                       '"VAFSV5"'+sepFld+
005200070316     C                                       '"VAFVA5"'+sepFld+
005300070316     C                                       '"VAFSV6"'+sepFld+
005400070316     C                                       '"VAFVA6"'+sepFld+
005500070316     C                                       '"VAFSV7"'+sepFld+
005600070316     C                                       '"VAFVA7"'+sepFld+
005700070316     C                                       '"VAFSV8"'+sepFld+
005800070316     C                                       '"VAFVA8"'+sepFld+
005900070316     C                                       '"VAFVAX"'+sepFld+
006000070316     C                                       '"VAFIMV"'+sepFld+
006100070316     C                                       '"VAFDFT"'+sepFld+
006200070316     C                                       '"VAFNFT"'+sepFld+
006300070316     C                                       '"VAFFIV"'+sepFld+
006400070316     C                                       '"VAFDIV"'+sepFld+
006500070316     C                                       '"VAFDRT"'+sepFld+
006600070316     C                                       '"VAFRMN"'+sepFld+
006700070316     C                                       '"VAFRMA"'+sepFld+
006800070316     C                                       '"VAFRMX"'+sepFld+
006900070316     C                                       '"VAFFTC"'+sepFld+
007000070316     C                                       '"VAFTC2"'+sepFld+
007100070316     C                                       '"VAFTSP"'+sepFld+
007200070316     C                                       '"VAFFAP"'+sepFld+
007300070316     C                                       '"VAFFIN"'+sepFld+
007400070316     C                                       '"VAFCTR"'+sepFld+
007500070316     C                                       '"VAFNAS"'+sepFld+
007600070316     C                                       '"VAFCTS"'+sepFld+
007700070316     C                                       '"VAFCAD"'+sepFld+
007800070316     C                                       '"VAFLOD"'+sepFld+
007900070316     C                                       '"VAFPRD"'+sepFld+
008000070316     C                                       '"VAFNZD"'+sepFld+
008100070316     C                                       '"VAFVAS"'+sepFld+
008200070316     C                                       '"VAFIAS"'+sepFld+
008300070316     C                                       '"VAFFPC"'+sepFld+
008400070316     C                                       '"VAFPKC"'+sepFld+
008500070316     C                                       '"VAFFVC"'+sepFld+
008600130314     C                                       '"VAFVLC"'+sepFld+
008700130314     C                                       '"TASDCM"'+sepFld+
008800130314     C                                       '"TASCCA"'
008900030325     C*
009000100209     C                   WRITE     TIVAW000
009100030325     C                   ENDIF
009200991027     C*
009300090325     C                   READ      tivgdtmp
009400090325     C                   DOW       not %eof
009500130314     C                   movel     vgddta        tivafds
009600130314     C*
009700130314     C                   Z-ADD     *zeros        tasDCM
009800130314     C                   MOVEL     *blanks       tasCCA
009900030709     C*
010000030709     C* Reperisco i dati NON presenti sul TIVAF00T dal TITAS30C
010100030709     C     KEYtas30C     CHAIN     titas30c
010200030325     C*
010300100209     C                   EVAL      VAWDTA  = %trim(%editc(VAFAAS:'Q'))+sepFld+
010400070316     C                                       %trim(%editc(VAFLNP:'Q'))+sepFld+
010500070316     C                                       %trim(%editc(VAFNRS:'Q'))+sepFld+
010600070316     C                                       %trim(%editc(VAFNSP:'Q'))+sepFld+
010700070316     C                                          '"'+%trim(VAFTBL)+'"'+sepFld+
010800070316     C                                       %trim(%editc(VAFMGS:'Q'))+sepFld+
010900070316     C                                       %trim(%editc(VAFKSC:'Q'))+sepFld+
011000070316     C                                       %trim(%editc(VAFLNA:'Q'))+sepFld+
011100070316     C                                       %trim(%editc(VAFNCL:'Q'))+sepFld+
011200070316     C                                       %trim(%editc(VAFPKB:'Q'))+sepFld+
011300070316     C                                       %trim(%editc(VAFVLF:'Q'))+sepFld+
011400070316     C                                       %trim(%editc(VAFQFT:'Q'))+sepFld+
011500070316     C                                       %trim(%editc(VAFPOR:'Q'))+sepFld+
011600070316     C                                          '"'+%trim(VAFSV1)+'"'+sepFld+
011700070316     C                                       %trim(%editc(VAFVA1:'Q'))+sepFld+
011800070316     C                                          '"'+%trim(VAFSV2)+'"'+sepFld+
011900070316     C                                       %trim(%editc(VAFVA2:'Q'))+sepFld+
012000070316     C                                          '"'+%trim(VAFSV3)+'"'+sepFld+
012100070316     C                                       %trim(%editc(VAFVA3:'Q'))+sepFld+
012200070316     C                                          '"'+%trim(VAFSV4)+'"'+sepFld+
012300070316     C                                       %trim(%editc(VAFVA4:'Q'))+sepFld+
012400070316     C                                          '"'+%trim(VAFSV5)+'"'+sepFld+
012500070316     C                                       %trim(%editc(VAFVA5:'Q'))+sepFld+
012600070316     C                                          '"'+%trim(VAFSV6)+'"'+sepFld+
012700070316     C                                       %trim(%editc(VAFVA6:'Q'))+sepFld+
012800070316     C                                          '"'+%trim(VAFSV7)+'"'+sepFld+
012900070316     C                                       %trim(%editc(VAFVA7:'Q'))+sepFld+
013000070316     C                                          '"'+%trim(VAFSV8)+'"'+sepFld+
013100070316     C                                       %trim(%editc(VAFVA8:'Q'))+sepFld+
013200070316     C                                       %trim(%editc(VAFVAX:'Q'))+sepFld+
013300070316     C                                       %trim(%editc(VAFIMV:'Q'))+sepFld+
013400070316     C                                       %trim(%editc(VAFDFT:'Q'))+sepFld+
013500070316     C                                       %trim(%editc(VAFNFT:'Q'))+sepFld+
013600070316     C                                       %trim(%editc(VAFFIV:'Q'))+sepFld+
013700070316     C                                          '"'+%trim(VAFDIV)+'"'+sepFld+
013800070316     C                                       %trim(%editc(VAFDRT:'Q'))+sepFld+
013900070316     C                                       %trim(%editc(VAFRMN:'Q'))+sepFld+
014000070316     C                                          '"'+%trim(VAFRMA)+'"'+sepFld+
014100070316     C                                          '"'+%trim(VAFRMX)+'"'+sepFld+
014200070316     C                                          '"'+%trim(VAFFTC)+'"'+sepFld+
014300070316     C                                          '"'+%trim(VAFTC2)+'"'+sepFld+
014400070316     C                                          '"'+%trim(VAFTSP)+'"'+sepFld+
014500070316     C                                          '"'+%trim(VAFFAP)+'"'+sepFld+
014600070316     C                                          '"'+%trim(VAFFIN)+'"'+sepFld+
014700070316     C                                       %trim(%editc(VAFCTR:'Q'))+sepFld+
014800070316     C                                          '"'+%trim(VAFNAS)+'"'+sepFld+
014900070316     C                                          '"'+%trim(VAFCTS)+'"'+sepFld+
015000070316     C                                          '"'+%trim(VAFCAD)+'"'+sepFld+
015100070316     C                                          '"'+%trim(VAFLOD)+'"'+sepFld+
015200070316     C                                          '"'+%trim(VAFPRD)+'"'+sepFld+
015300070316     C                                          '"'+%trim(VAFNZD)+'"'+sepFld+
015400070316     C                                          '"'+%trim(VAFVAS)+'"'+sepFld+
015500070316     C                                       %trim(%editc(VAFIAS:'Q'))+sepFld+
015600070316     C                                          '"'+%trim(VAFFPC)+'"'+sepFld+
015700070316     C                                       %trim(%editc(VAFPKC:'Q'))+sepFld+
015800070316     C                                          '"'+%trim(VAFFVC)+'"'+sepFld+
015900070316     C                                       %trim(%editc(VAFVLC:'Q'))+sepFld+
016000130314     C                                       %trim(%editc(tasDCM:'X'))+sepFld+
016100130314     C                                          '"'+%trim(tasCCA)+'"'
016200930409     C*
016300100209     C                   WRITE     TIVAW000
016400090325     C                   delete    tivgd000
016500991027     C*
016600090325     C                   READ      tivgdtmp
016700030325     C                   ENDDO
016800991027     C*
016900030325     C                   EVAL      wrkesito = '0'
017000991027     C*
017100910830     C                   ENDSR
017200991027
017300130314
017400991027      /TITLE Operazioni iniziali.
017500991027     C     *inzsr        BEGSR
017600991027     C*
017700991027     C     *ENTRY        PLIST
017800991027     C                   parm                    prmppt
017900991027     C     wrkesito      parm      wrkesito      prmesito
018000070316     C*
018100070316     C* Verifico il carattere separatore campo indicato nei parametri del cliente/tipo file
018200070316     C                   eval      sepFld = %subst(prmppt:2:1)
018300070316     C                   if        sepFld = *blanks
018400070316     C                   eval      sepFld = ','
018500070316     C                   endif
018600030709     C*
018700030709     C* Definizione chiavi
018800030709     C*
018900030709     C* Chiave su TITAS30C - Completa
019000030709     C     KEYtas30C     KLIST
019100030709     C                   KFLD                    vafAAS
019200030709     C                   KFLD                    vafLNP
019300030709     C                   KFLD                    vafNRS
019400030709     C                   KFLD                    vafNSP
019500030709     C                   KFLD                    vafTBL
019600991027     C*
019700991027     C                   ENDSR
