000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000991027
001100060804     C                   exsr      traduci
001200060804     C                   seton                                        LR
001300991027
001400060804
001500991027     C     traduci       BEGSR
001600991027     C*
001700060804     C                   read      TIVGDTMP
001800991027     C                   dow       not %eof
001900121023     C*
002000121023     C* Se nei parametri non è richiesta "elaborazione fittizia" procedo
002100121023     C                   if        %subst(prmppt:1:1) <> 'K'
002200050121     C*
002300050121     C* Inizializzazione campi e variabili d out
002400060804     C                   clear                   tivaw000
002500060804     C*
002600060804     C* Valorizzo il buffer del file d output
002700080130     C                   eval      vawDTA = vgdDTA
002800930409     C*
002900050121     C                   WRITE     TIVAW000
003000121023     C*
003100121023     C                   endif
003200121023     C*
003300050909     C                   DELETE    TIVGD000
003400050909     C                   READ      TIVGDTMP
003500991027     C                   ENDdo
003600991027     C*
003700991027     C                   eval      wrkesito = '0'
003800991027     C*
003900910830     C                   ENDSR
004000060804
004100991027
004200991027      /TITLE Operazioni iniziali.
004300991027     C     *inzsr        BEGSR
004400991027     C*
004500991027     C     *ENTRY        PLIST
004600991027     C                   parm                    prmppt
004700991027     C     wrkesito      parm      wrkesito      prmesito
004800991027     C*
004900991027     C                   ENDSR
