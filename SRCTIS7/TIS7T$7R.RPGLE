000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500060410     Ftivawwwt  uF A E             DISK
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000030325
001800991027     C                   EXSR      traduci
001900921023     C                   SETON                                        LR
002000030908
002600991027
002700991027     C     traduci       BEGSR
011200991027     C*
011300060410     C                   READ      TIVGDTMP
011400060410     C                   DOW       not %eof(TIVGDTMP)
011500060411     C*
011600060613     C* Valorizzo il buffer dal campo flat di output
011700060613     C                   EVAL      vawDTA  = vgdDTA
026800930409     C*
026900060410     C                   WRITE     TIVAW000
027000060410     C                   DELETE    TIVGD000
027100991027     C*
027200060410     C                   READ      TIVGDTMP
027300030325     C                   ENDDO
027400991027     C*
027500030325     C                   EVAL      wrkesito = '0'
027600991027     C*
027700910830     C                   ENDSR
027800991027
027900991027      /TITLE Operazioni iniziali.
028000991027     C     *inzsr        BEGSR
028100991027     C*
028200991027     C     *ENTRY        PLIST
028300991027     C                   parm                    prmppt
028400991027     C     wrkesito      parm      wrkesito      prmesito
028500991027     C*
028600991027     C                   ENDSR
