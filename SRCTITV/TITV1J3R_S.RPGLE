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
001900021007
002000021007
002100991027     C                   EXSR      traduci
002200921023     C                   SETON                                        LR
002300991027
002400081007
002500081007
002600991027     C     traduci       BEGSR
002700991027     C*
002800080924     C                   READ      TIVGDTMP
002900080924     C                   DOW       not %eof(TIVGDTMP)
003000080924     C*
003100080924     C                   EVAL      fnvac00t = vgdDTA
003200080613     C*
003300081007     C* Occorre tornare al cliente le sole consegnate ok
003400081007     C                   IF        vacDCM > *zeros AND
003500081007     C                             vacCCA = *blanks
003600090225     C*
003700090225     C* Scorro il file raccordo accorpamento bolle x verticalizzare gli esiti
003800090225     C* anche delle bolle accorpate
003900090225     C*
004000090225     C     KEYrab01_C    SETLL     firab01l
004100090225     C                   IF        %equal(firab01l)
004200090225     C     KEYrab01_C    READE     firab01l
004300090225     C                   DOW       not %eof(firab01l)
004400090225     C                   clear                   ds_out_cli
004500090225     C                   move      rabRMNR       dout_NumDDT
004600090225     C                   EXSR      WRIOUT
004700090225     C     KEYrab01_C    READE     firab01l
004800090225     C                   ENDDO
004900090225     C*
005000090225     C                   ELSE
005100090225     C                   clear                   ds_out_cli
005200090225     C                   move      vacRMN        dout_NumDDT
005300090225     C                   EXSR      WRIOUT
005400090225     C                   ENDIF
005500081007     C                   ENDIF
005600080613     C*
005700081007     C                   DELETE    TIVGD000
005800991027     C*
005900080924     C                   READ      TIVGDTMP
006000081007     C                   ENDDO
006100991027     C*
006200991027     C                   eval      wrkesito = '0'
006300991027     C*
006400910830     C                   ENDSR
006500090225
006600090225
006700090225
006800090225     C     WRIOUT        BEGSR
006900090225     C*
007000090225     C***                movel     vacRMA        dout_RifSAP
007100090225     C                   move      vacDCM        dout_DataCons
007200090225     C                   movel     '00'          dout_Causale
007300090225     C*
007400090225     C                   eval      vawDTA = ds_out_cli
007500090225     C*
007600090225     C                   WRITE     TIVAW000
007700090225     C*
007800090225     C                   ENDSR
007900991027
008000081007
008100081007
008200991027      /TITLE Operazioni iniziali.
008300991027     C     *inzsr        BEGSR
008400991027     C*
008500991027     C     *ENTRY        PLIST
008600991027     C                   parm                    prmppt
008700991027     C     wrkesito      parm      wrkesito      prmesito
008800090225     C*
008900090225     C* Definizione chiavi
009000090225     C*
009100090225     C* Chiave su FIRAB01L - Completa
009200090225     C     KEYrab01_C    KLIST
009300090225     C                   KFLD                    vacAAS
009400090225     C                   KFLD                    vacLNP
009500090225     C                   KFLD                    vacNRS
009600090225     C                   KFLD                    vacNSP
009700050622     C*
009800991027     C                   ENDSR
