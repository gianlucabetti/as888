000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700070329     D dtllgo        e ds
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001600991027
001700060804     C                   exsr      traduci
001800060804     C                   seton                                        LR
001900991027
002000060804
002100991027     C     traduci       BEGSR
002200991027     C*
002300060804     C                   read      TIVGDTMP
002400991027     C                   dow       not %eof
002500070329     C                   eval      dtllgo = vgdDTA
002600050121     C*
002700050121     C* Inizializzazione campi e variabili d out
002800060804     C                   clear                   tivaw000
006100060804     C*
006200060804     C* Valorizzo il buffer del file d output
006300070329     C                   eval      vawdta = dtllgo
006400930409     C*
006500050121     C                   WRITE     TIVAW000
006600050909     C                   DELETE    TIVGD000
006700991027     C*
006800050909     C                   READ      TIVGDTMP
006900070329     C                   enddo
007000991027     C*
007100991027     C                   eval      wrkesito = '0'
007200991027     C*
007300910830     C                   ENDSR
007400060804
007500991027
007600991027      /TITLE Operazioni iniziali.
007700991027     C     *inzsr        BEGSR
007800991027     C*
007900991027     C     *ENTRY        PLIST
008000991027     C                   parm                    prmppt
008100991027     C     wrkesito      parm      wrkesito      prmesito
008200991027     C*
008300991027     C                   ENDSR
