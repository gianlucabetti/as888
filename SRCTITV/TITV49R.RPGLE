000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400090122     Ftivgdtmp  uF   E             DISK
000500030325     FTNVASXLS  O    E             DISK
000600030414     FTITAS30C  IF   E           K DISK
000700991027
000800090122     D tnvasds       e ds                  extname(tnvas00t)
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200991027
001300090122
001400991027     C                   EXSR      traduci
001500921023     C                   SETON                                        LR
001600991027
001700991027     C     traduci       BEGSR
001800030325     C*
001900030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
002000031028     C*                  IF        %subst(prmppt:1:1) = 'I'
002100031028     C*                  EVAL      VASXDTA = '"VASLNP"'+','+
002200031028     C*                                      '"VASLNA"'+','+
002300031028     C*                                      '"VASAAS"'+','+
002400031028     C*                                      '"VASNRS"'+','+
002500031028     C*                                      '"VASNSP"'+','+
002600031028     C*                                      '"VASRMN"'+','+
002700031028     C*                                      '"VASCAS"'+','+
002800031028     C*                                      '"VASVCA"'+','+
002900031028     C*                                      '"VASNRA"'+','+
003000031028     C*                                      '"VASDTE"'+','+
003100031028     C*                                      '"VASTPI"'+','+
003200031028     C*                                      '"VASABI"'+','+
003300031028     C*                                      '"VASCAI"'+','+
003400031028     C*                                      '"VASNDP"'+','+
003500031028     C*                                      '"VASDDP"'+','+
003600031028     C*                                      '"VASCCB"'+','+
003700031028     C*                                      '"VASABP"'+','+
003800031028     C*                                      '"VASCAP"'+','+
003900031028     C*                                      '"VASFPC"'+','+
004000031028     C*                                      '"VASDVA"'+','+
004100031028     C*                                      '"VASKSC"'+','+
004200031028     C*                                      '"VASRMA"'+','+
004300031028     C*                                      '"TASRSD"'+','+
004400031028     C*                                      '"TASLOD"'+','+
004500031028     C*                                      '"TASPRD"'
004600030325     C*
004700031028     C*                  WRITE     TNVASX00
004800031028     C*                  ENDIF
004900991027     C*
005000090122     C                   READ      tivgdtmp
005100090122     C                   DOW       not %eof(tivgdtmp)
005200090122     C                   movel     vgddta        tnvasds
005300030414     C*
005400030414     C* "Chaino" la bolla x reperire i dati mancanti
005500030414     C     KEYtas30cP    CHAIN     TITAS30C
005600030414     C                   IF        %found(TITAS30C)
005700030325     C*
005800031028     C                   EVAL      VASXDTA = %editc(vasLNP:'P')+
005900031028     C                                       %editc(vasLNA:'P')+
006000031028     C                                       %editc(vasAAS:'P')+
006100031028     C                                       %editc(vasNRS:'P')+
006200031028     C                                       %editc(vasNSP:'P')+
006300031028     C                                       %editc(vasRMN:'P')+
006400031028     C                                       %editc(vasCAS:'P')+
006500031028     C                                       vasVCA+
006600031028     C                                       vasNRA+
006700031028     C                                       %editc(vasDTE:'P')+
006800031028     C                                       vasTPI+
006900031028     C                                       %editc(vasABI:'P')+
007000031028     C                                       %editc(vasCAI:'P')+
007100031028     C                                       %editc(vasNDP:'P')+
007200031028     C                                       %editc(vasDDP:'P')+
007300031028     C                                       vasCCB+
007400031028     C                                       %editc(vasABP:'P')+
007500031028     C                                       %editc(vasCAP:'P')+
007600031028     C                                       vasFPC+
007700031028     C                                       %editc(vasDVA:'P')+
007800031028     C                                       %editc(vasKSC:'P')+
007900031028     C                                       vasRMA+
008000031028     C                                       tasRSD+
008100031028     C                                       tasLOD+
008200031028     C                                       tasPRD+'.'
008300930409     C*
008400030325     C                   WRITE     TNVASX00
008500090122     C                   DELETE    tivgd000
008600030414     C*
008700030414     C                   ENDIF
008800991027     C*
008900090122     C                   READ      tivgdtmp
009000030325     C                   ENDDO
009100991027     C*
009200030325     C                   EVAL      wrkesito = '0'
009300991027     C*
009400910830     C                   ENDSR
009500991027
009600991027      /TITLE Operazioni iniziali.
009700991027     C     *inzsr        BEGSR
009800991027     C*
009900991027     C     *ENTRY        PLIST
010000991027     C                   parm                    prmppt
010100991027     C     wrkesito      parm      wrkesito      prmesito
010200030414     C****
010300030414     C* Definizione chiavi
010400030414     C****
010500030414     C* Chiave su TITAS30C - parziale
010600030414     C     KEYtas30cP    KLIST
010700030414     C                   KFLD                    vasAAS
010800030414     C                   KFLD                    vasLNP
010900030414     C                   KFLD                    vasNRS
011000030414     C                   KFLD                    vasNSP
011100991027     C*
011200991027     C                   ENDSR
