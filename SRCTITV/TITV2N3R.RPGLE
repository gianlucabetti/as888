000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400070301     FFIRAB01L  iF   E           K DISK
000500090320     Ftivgdtmp  uF   E             DISK
000600090320     FTIVAWWWT  uF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100090320     D fnvabds       e ds                  extname(fnvab00t)
001200030325
001300070227     D wVABRMNR        s                   like(vabRMN)
001400070227     D wVABRMAR        s                   like(vabRMA)
001500991027
001600121206
001700030908     C                   EXSR      chkRECFILMBR
001800991027     C                   EXSR      traduci
001900921023     C                   SETON                                        LR
002000121206
002100030908
002200030908     C     chkRECFILMBR  BEGSR
002300030908     C*
002400070227     C                   READ      TIVAWWWT                               55
002500030908     C*
002600030908     C                   ENDSR
002700070312
002800070312
002900991027
003000991027     C     traduci       BEGSR
003100030325     C*
003200090320     C                   READ      tivgdtmp
003300090320     C                   DOW       not %eof(tivgdtmp)
003400090320     c                   movel     vgddta        fnvabds
003500070228     C*
003600121206     C                   clear                   wVABRMNR
003700121206     C                   clear                   wVABRMAR
003800070312     C*
003900070301     C     KEYrab01_C    SETLL     firab01l
004000070301     C                   IF        %equal(firab01l)
004100070301     C     KEYrab01_C    READE     firab01l
004200070301     C                   DOW       not %eof(firab01l)
004300070227     C                   EVAL      wVABRMNR = rabRMNR
004400070227     C                   EVAL      wVABRMAR = rabRMAR
004500070228     C                   EXSR      WRIOUT
004600070301     C     KEYrab01_C    READE     firab01l
004700070228     C                   ENDDO
004800070228     C                   ELSE
004900070228     C                   EXSR      WRIOUT
005000070227     C                   ENDIF
005100070227     C*
005200090320     C                   delete    tivgd000
005300991027     C*
005400090320     C                   READ      tivgdtmp
005500030325     C                   ENDDO
005600991027     C*
005700030325     C                   EVAL      wrkesito = '0'
005800991027     C*
005900910830     C                   ENDSR
006000070228
006100070228
006200070228
006300070228     C     wriOUT        BEGSR
006400070228     C*
006500121206     C                   EVAL      VAWDTA  = %trim(%editc(VABAAS  :'X'))+
006600121206     C                                       %trim(%editc(VABLNP  :'X'))+
006700121206     C                                       %trim(%editc(VABNRS  :'X'))+
006800121206     C                                       %trim(%editc(VABNSP  :'X'))+
006900121206     C                                       %trim(%editc(VABRMN  :'X'))+
007000121206     C                                                    VABRMA        +
007100121206     C                                       %trim(%editc(wVABRMNR:'X'))+
007200121206     C                                                    wVABRMAR
007300070228     C*
007400070228     C                   WRITE     TIVAW000
007500070228     C*
007600070228     C                   ENDSR
007700070228
007800070228
007900070228
008000991027
008100991027      /TITLE Operazioni iniziali.
008200991027     C     *inzsr        BEGSR
008300991027     C*
008400991027     C     *ENTRY        PLIST
008500991027     C                   parm                    prmppt
008600991027     C     wrkesito      parm      wrkesito      prmesito
008700070227     C*
008800070227     C* Definizione chiavi
008900070227     C*
009000070301     C* Chiave su FIRAB01L - Completa
009100070227     C     KEYrab01_C    KLIST
009200070227     C                   KFLD                    vabAAS
009300070227     C                   KFLD                    vabLNP
009400070227     C                   KFLD                    vabNRS
009500070227     C                   KFLD                    vabNSP
009600991027     C*
009700991027     C                   ENDSR
