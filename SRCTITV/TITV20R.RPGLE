000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500020729     FfnvacglpwtO    E             DISK
000600991027
000700080924     D fnvac00t      e ds
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100020920     D wrkDAT          s              8
001200020729
001300991027
001400991027     C                   EXSR      traduci
001500921023     C                   SETON                                        LR
001600991027
001700991027     C     traduci       BEGSR
001800991027     C*
001900080924     C                   READ      TIVGDTMP
002000080924     C                   dow       not %eof(TIVGDTMP)
002100080924     C*
002200080924     C                   EVAL      fnvac00t = vgdDTA
002300020508     C*
002400020508     C* Innanzitutto inizializzo il formato record del file tradotto
002500020729     C                   CLEAR                   FNVACGLP
002600020508     C*
002700020729     C* Quindi effettuo la traduzione vera e propria
002800020729     C                   EVAL      GLPFLG4 = vacRMA
002900020729     C                   EVAL      GLPCMEZ = %subst(vacRMO:1:3)
003000020729     C                   EVAL      GLPKMEZ = %subst(vacRMO:4:7)
003100020729     C                   EVAL      GLPRMEZ = %subst(vacRMO:11:3)
003200020729     C*
003300020729     C* Effettuo considerazioni particolari relative all'esito della spedizione
003400020729     C*
003500020729     C* LASCIATO AVVISO...
003600020729     C                   IF        vacDLA > *zeros and
003700020729     C                             vacDAG = *zeros and
003800020729     C                             vacDCM = *zeros
003900020729     C                   EVAL      GLPESIT = 'A'
004000020920     C                   MOVEL(P)  vacDLA        wrkDAT
004100020920     C                   EVAL      GLPDCCE = %subst(wrkDAT:1:4) + '-' +
004200020920     C                                       %subst(wrkDAT:5:2) + '-' +
004300020920     C                                       %subst(wrkDAT:7:2)
004400020729     C                   ENDIF
004500020729     C*
004600020729     C* GIACENZA...
004700020729     C                   IF        vacDAG > *zeros and
004800020729     C                             vacDCM = *zeros
004900020729     C                   EVAL      GLPESIT = 'G'
005000020920     C                   MOVEL(P)  vacDAG        wrkDAT
005100020920     C                   EVAL      GLPDCCE = %subst(wrkDAT:1:4) + '-' +
005200020920     C                                       %subst(wrkDAT:5:2) + '-' +
005300020920     C                                       %subst(wrkDAT:7:2)
005400020729     C                   ENDIF
005500020729     C*
005600020729     C* CONSEGNATA OK...
005700020729     C                   IF        vacDCM > *zeros
005800020729     C                   IF        vacCCA = 'C' OR
005900020729     C                             vacCCA = 'A'
006000020729     C                   ELSE
006100020729     C                   IF        vacCCA = *blanks
006200020729     C                   EVAL      GLPESIT = 'C'
006300020920     C                   MOVEL(P)  vacDCM        wrkDAT
006400020920     C                   EVAL      GLPDCCE = %subst(wrkDAT:1:4) + '-' +
006500020920     C                                       %subst(wrkDAT:5:2) + '-' +
006600020920     C                                       %subst(wrkDAT:7:2)
006700020729     C                   ELSE
006800020729     C*
006900020729     C* CHIUSURA OK CON DIROTTAMENTO...
007000020729     C                   IF        vacCCA = '1'
007100020729     C                   EVAL      GLPESIT = 'C'
007200020920     C                   MOVEL(P)  vacDCM        wrkDAT
007300020920     C                   EVAL      GLPDCCE = %subst(wrkDAT:1:4) + '-' +
007400020920     C                                       %subst(wrkDAT:5:2) + '-' +
007500020920     C                                       %subst(wrkDAT:7:2)
007600020729     C                   ELSE
007700020729     C*
007800020729     C* CHIUSURA CON RESO...
007900020729     C                   IF        vacCCA = '2'
008000020729     C                   EVAL      GLPESIT = 'R'
008100020920     C                   MOVEL(P)  vacDCM        wrkDAT
008200020920     C                   EVAL      GLPDCCE = %subst(wrkDAT:1:4) + '-' +
008300020920     C                                       %subst(wrkDAT:5:2) + '-' +
008400020920     C                                       %subst(wrkDAT:7:2)
008500020729     C                   ELSE
008600020729     C*
008700020729     C* CHIUSURA CON ANOMALIA...
008800020729     C                   EVAL      GLPESIT = 'D'
008900021108     C                   MOVEL(P)  vacDCM        wrkDAT
009000021108     C                   EVAL      GLPDCCE = %subst(wrkDAT:1:4) + '-' +
009100021108     C                                       %subst(wrkDAT:5:2) + '-' +
009200021108     C                                       %subst(wrkDAT:7:2)
009300020729     C                   ENDIF
009400020729     C                   ENDIF
009500020729     C                   ENDIF
009600020729     C                   ENDIF
009700020729     C                   ENDIF
009800020508     C*
009900020729     C                   IF        GLPESIT <> *blanks
010000020729     C                   WRITE     FNVACGLP
010100020729     C                   ENDIF
010200080924     C                   DELETE    TIVGD000
010300020508     C*
010400080924     C                   READ      TIVGDTMP
010500991027     C                   ENDdo
010600991027     C*
010700991027     C                   eval      wrkesito = '0'
010800991027     C*
010900910830     C                   ENDSR
011000020508
011100020508
011200020508
011300991027
011400991027      /TITLE Operazioni iniziali.
011500991027     C     *inzsr        BEGSR
011600991027     C*
011700991027     C     *ENTRY        PLIST
011800991027     C                   parm                    prmppt
011900991027     C     wrkesito      parm      wrkesito      prmesito
012000991027     C*
012100991027     C                   ENDSR
