000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500090225     Ffirab01l  IF   E           K DISK
000600081007     Ftivawwwt  O    E             DISK
000700991027
000800080924     D fnvac00t      e ds
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200081007
001300081007
001400081007     D ds_out_cli      ds
001500081007     D  dout_NumDDT                  10
001600081007     D  dout_RifSAP                  10
001700081007     D  dout_DataCons                 8
001800081007     D  dout_Causale                  2
001900140131     D  dout_SubCaus                  3
002000140131     D  dout_IdBRT                   15
002100021007
002200021007
002300991027     C                   EXSR      traduci
002400921023     C                   SETON                                        LR
002500991027
002600081007
002700081007
002800991027     C     traduci       BEGSR
002900991027     C*
003000080924     C                   READ      TIVGDTMP
003100080924     C                   DOW       not %eof(TIVGDTMP)
003200080924     C*
003300080924     C                   EVAL      fnvac00t = vgdDTA
003400140129     C                   clear                   ds_out_cli
003500080613     C*
003600140129     C                   SELECT
003700140129     C                   WHEN      vacDCM > *zeros AND
003800081007     C                             vacCCA = *blanks
003900140129     C                   movel     '00'          dout_Causale
004000140129     C                   EXSR      WRIFILE
004100140129     C*
004200140129     C                   WHEN      vacDCM > *zeros AND
004300140129     C                             vacCCA = '2'
004400140129     C                   movel     '07'          dout_Causale
004500140129     C                   EXSR      WRIFILE
004600140129     C*
004700140129     C                   ENDSL
004800080613     C*
004900081007     C                   DELETE    TIVGD000
005000991027     C*
005100080924     C                   READ      TIVGDTMP
005200081007     C                   ENDDO
005300991027     C*
005400991027     C                   eval      wrkesito = '0'
005500991027     C*
005600910830     C                   ENDSR
005700140129
005800140129
005900140129
006000140129     C     WRIFILE       BEGSR
006100140129     C*
006200140129     C* Scorro il file raccordo accorpamento bolle x verticalizzare gli esiti
006300140129     C* anche delle bolle accorpate
006400140129     C*
006500140129     C     KEYrab01_C    SETLL     firab01l
006600140129     C                   IF        %equal(firab01l)
006700140129     C     KEYrab01_C    READE     firab01l
006800140129     C                   DOW       not %eof(firab01l)
006900140129     C                   move      rabRMNR       dout_NumDDT
007000140129     C                   EXSR      WRIOUT
007100140129     C     KEYrab01_C    READE     firab01l
007200140129     C                   ENDDO
007300140129     C*
007400140129     C                   ELSE
007500140129     C                   move      vacRMN        dout_NumDDT
007600140129     C                   EXSR      WRIOUT
007700140129     C                   ENDIF
007800140129     C*
007900140129     C                   ENDSR
008000090225
008100090225
008200090225
008300090225     C     WRIOUT        BEGSR
008400090225     C*
008500090225     C***                movel     vacRMA        dout_RifSAP
008600090225     C                   move      vacDCM        dout_DataCons
008700140131     C                   eval      dout_IdBRT   = %editc(vacLNP:'X') +
008800140131     C                                            %editc(vacNRS:'X') +
008900140131     C                                            %editc(vacNSP:'X') + '/' +
009000140131     C                                            %subst(%editc(vacAAS:'X'):3:2)
009100090225     C*
009200090225     C                   eval      vawDTA = ds_out_cli
009300090225     C*
009400090225     C                   WRITE     TIVAW000
009500090225     C*
009600090225     C                   ENDSR
009700991027
009800081007
009900081007
010000991027      /TITLE Operazioni iniziali.
010100991027     C     *inzsr        BEGSR
010200991027     C*
010300991027     C     *ENTRY        PLIST
010400991027     C                   parm                    prmppt
010500991027     C     wrkesito      parm      wrkesito      prmesito
010600090225     C*
010700090225     C* Definizione chiavi
010800090225     C*
010900090225     C* Chiave su FIRAB01L - Completa
011000090225     C     KEYrab01_C    KLIST
011100090225     C                   KFLD                    vacAAS
011200090225     C                   KFLD                    vacLNP
011300090225     C                   KFLD                    vacNRS
011400090225     C                   KFLD                    vacNSP
011500050622     C*
011600991027     C                   ENDSR
