000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400991027     FTNVAS00T  uF   E             DISK
000500030325     FTNVASXLS  O    E             DISK
000600991027     FTNVAS00S  O    E             DISK    rename(tnvas000:tnvassss)
000700030414     FTITAS30C  IF   E           K DISK
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300030325     D dsvas         e ds                  extname(tnvas00t)
001400991027
001500991027     C                   EXSR      traduci
001600921023     C                   SETON                                        LR
001700991027
001800991027     C     traduci       BEGSR
001900030325     C*
002000030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
002100030325     C                   IF        %subst(prmppt:1:1) = 'I'
002200030325     C                   EVAL      VASXDTA = '"VASLNP"'+','+
002300030325     C                                       '"VASLNA"'+','+
002400030325     C                                       '"VASAAS"'+','+
002500030325     C                                       '"VASNRS"'+','+
002600030325     C                                       '"VASNSP"'+','+
002700030325     C                                       '"VASRMN"'+','+
002800030325     C                                       '"VASCAS"'+','+
002900030325     C                                       '"VASVCA"'+','+
003000030325     C                                       '"VASNRA"'+','+
003100030325     C                                       '"VASDTE"'+','+
003200030325     C                                       '"VASTPI"'+','+
003300030325     C                                       '"VASABI"'+','+
003400030325     C                                       '"VASCAI"'+','+
003500030325     C                                       '"VASNDP"'+','+
003600030325     C                                       '"VASDDP"'+','+
003700030325     C                                       '"VASCCB"'+','+
003800030325     C                                       '"VASABP"'+','+
003900030325     C                                       '"VASCAP"'+','+
004000030325     C                                       '"VASFPC"'+','+
004100030325     C                                       '"VASDVA"'+','+
004200030325     C                                       '"VASKSC"'+','+
004300030414     C                                       '"VASRMA"'+','+
004400030414     C                                       '"TASRSD"'+','+
004500030414     C                                       '"TASLOD"'+','+
004600030414     C                                       '"TASPRD"'
004700030325     C*
004800030325     C                   WRITE     TNVASX00
004900030325     C                   ENDIF
005000991027     C*
005100991027     C                   READ      TNVAS00T
005200030325     C                   DOW       not %eof(TNVAS00T)
005300030414     C*
005400030414     C* "Chaino" la bolla x reperire i dati mancanti
005500030414     C     KEYtas30cP    CHAIN     TITAS30C
005600030414     C                   IF        %found(TITAS30C)
005700991027     C*
005800030325     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
005900030325     C     '"':''''      XLATE     dsvas         dsvas
006000030325     C*
006100030325     C                   EVAL      VASXDTA = %trim(%editc(vasLNP:'Q'))+','+
006200030325     C                                       %trim(%editc(vasLNA:'Q'))+','+
006300030325     C                                       %trim(%editc(vasAAS:'Q'))+','+
006400030325     C                                       %trim(%editc(vasNRS:'Q'))+','+
006500030325     C                                       %trim(%editc(vasNSP:'Q'))+','+
006600030325     C                                       %trim(%editc(vasRMN:'Q'))+','+
006700030325     C                                       %trim(%editc(vasCAS:'Q'))+','+
006800030325     C                                       '"'+%trim(vasVCA)+'"'+','+
006900030325     C                                       '"'+%trim(vasNRA)+'"'+','+
007000030325     C                                       %trim(%editc(vasDTE:'Q'))+','+
007100030325     C                                       '"'+%trim(vasTPI)+'"'+','+
007200030325     C                                       %trim(%editc(vasABI:'Q'))+','+
007300030325     C                                       %trim(%editc(vasCAI:'Q'))+','+
007400030325     C                                       %trim(%editc(vasNDP:'Q'))+','+
007500030325     C                                       %trim(%editc(vasDDP:'Q'))+','+
007600030325     C                                       '"'+%trim(vasCCB)+'"'+','+
007700030325     C                                       %trim(%editc(vasABP:'Q'))+','+
007800030325     C                                       %trim(%editc(vasCAP:'Q'))+','+
007900030325     C                                       '"'+%trim(vasFPC)+'"'+','+
008000030325     C                                       %trim(%editc(vasDVA:'Q'))+','+
008100030325     C                                       %trim(%editc(vasKSC:'Q'))+','+
008200030414     C                                       '"'+%trim(vasRMA)+'"'+','+
008300030414     C                                       '"'+%trim(tasRSD)+'"'+','+
008400030414     C                                       '"'+%trim(tasLOD)+'"'+','+
008500030414     C                                       '"'+%trim(tasPRD)+'"'
008600930409     C*
008700030325     C                   WRITE     TNVASX00
008800030325     C                   WRITE     TNVASsss
008900030325     C                   DELETE    TNVAS000
009000030414     C*
009100030414     C                   ENDIF
009200991027     C*
009300991027     C                   READ      TNVAS00T
009400030325     C                   ENDDO
009500991027     C*
009600030325     C                   EVAL      wrkesito = '0'
009700991027     C*
009800910830     C                   ENDSR
009900991027
010000991027      /TITLE Operazioni iniziali.
010100991027     C     *inzsr        BEGSR
010200991027     C*
010300991027     C     *ENTRY        PLIST
010400991027     C                   parm                    prmppt
010500991027     C     wrkesito      parm      wrkesito      prmesito
010600030414     C****
010700030414     C* Definizione chiavi
010800030414     C****
010900030414     C* Chiave su TITAS30C - parziale
011000030414     C     KEYtas30cP    KLIST
011100030414     C                   KFLD                    vasAAS
011200030414     C                   KFLD                    vasLNP
011300030414     C                   KFLD                    vasNRS
011400030414     C                   KFLD                    vasNSP
011500991027     C*
011600991027     C                   ENDSR
