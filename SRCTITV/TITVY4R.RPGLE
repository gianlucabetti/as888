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
001300070227     D iFLD            s              1    inz
001400070906     D iTXT            s             10    inz varying
001500070227     D wVABRMNR        s                   like(vabRMN)
001600070227     D wVABRMAR        s                   like(vabRMA)
001700991027
001800030908     C                   EXSR      chkRECFILMBR
001900991027     C                   EXSR      traduci
002000921023     C                   SETON                                        LR
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
003200030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
003300030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
003400070906     C                   EVAL      VAWDTA  = iTXT+'VABAAS' +iTXT+iFLD+
003500070906     C                                       iTXT+'VABMGS' +iTXT+iFLD+
003600070906     C                                       iTXT+'VABRSD' +iTXT+iFLD+
003700070906     C                                       iTXT+'VABIND' +iTXT+iFLD+
003800070906     C                                       iTXT+'VABCAD' +iTXT+iFLD+
003900070906     C                                       iTXT+'VABLOD' +iTXT+iFLD+
004000070906     C                                       iTXT+'VABPRD' +iTXT+iFLD+
004100070906     C                                       iTXT+'VABNZD' +iTXT+iFLD+
004200070906     C                                       iTXT+'VABNCL' +iTXT+iFLD+
004300070906     C                                       iTXT+'VABRMN' +iTXT+iFLD+
004400070906     C                                       iTXT+'VABRMA' +iTXT+iFLD+
004500070906     C                                       iTXT+'VABRMNR'
004600030325     C*
004700070227     C                   WRITE     TIVAW000
004800030325     C                   ENDIF
004900991027     C*
005000090320     C                   READ      tivgdtmp
005100090320     C                   DOW       not %eof(tivgdtmp)
005200090320     c                   movel     vgddta        fnvabds
005300070228     C*
005400070228     C                   EVAL      wVABRMNR = *zeros
005500070228     C                   EVAL      wVABRMAR = *blanks
005600070312     C*
005700070301     C     KEYrab01_C    SETLL     firab01l
005800070301     C                   IF        %equal(firab01l)
005900070301     C     KEYrab01_C    READE     firab01l
006000070301     C                   DOW       not %eof(firab01l)
006100070227     C                   EVAL      wVABRMNR = rabRMNR
006200070227     C                   EVAL      wVABRMAR = rabRMAR
006300070228     C                   EXSR      WRIOUT
006400070301     C     KEYrab01_C    READE     firab01l
006500070228     C                   ENDDO
006600070228     C                   ELSE
006700070228     C                   EXSR      WRIOUT
006800070227     C                   ENDIF
006900070227     C*
007000090320     C                   delete    tivgd000
007100991027     C*
007200090320     C                   READ      tivgdtmp
007300030325     C                   ENDDO
007400991027     C*
007500030325     C                   EVAL      wrkesito = '0'
007600991027     C*
007700910830     C                   ENDSR
007800070228
007900070228
008000070228
008100070228     C     wriOUT        BEGSR
008200070228     C*
008300070312     C                   EVAL      VAWDTA  = %trim(%editc(VABAAS:'Q'))+iFLD+
008400070828     C                                       %trim(%editw(VABMGS:'0    '))+iFLD+
008500070906     C                                         iTXT+%trim(VABRSD)+iTXT+iFLD+
008600070906     C                                         iTXT+%trim(VABIND)+iTXT+iFLD+
008700070906     C                                         iTXT+%trim(VABCAD)+iTXT+iFLD+
008800070906     C                                         iTXT+%trim(VABLOD)+iTXT+iFLD+
008900070906     C                                         iTXT+%trim(VABPRD)+iTXT+iFLD+
009000070906     C                                         iTXT+%trim(VABNZD)+iTXT+iFLD+
009100070228     C                                       %trim(%editc(VABNCL:'Q'))+iFLD+
009200070228     C                                       %trim(%editc(VABRMN:'Q'))+iFLD+
009300070906     C                                         iTXT+%trim(VABRMA)+iTXT+iFLD+
009400070312     C                                       %trim(%editc(wVABRMNR:'Q'))
009500070228     C*
009600070228     C                   WRITE     TIVAW000
009700070228     C*
009800070228     C                   ENDSR
009900070228
010000070228
010100070228
010200991027
010300991027      /TITLE Operazioni iniziali.
010400991027     C     *inzsr        BEGSR
010500991027     C*
010600991027     C     *ENTRY        PLIST
010700991027     C                   parm                    prmppt
010800991027     C     wrkesito      parm      wrkesito      prmesito
010900070227     C*
011000070227     C* Definizione chiavi
011100070227     C*
011200070301     C* Chiave su FIRAB01L - Completa
011300070227     C     KEYrab01_C    KLIST
011400070227     C                   KFLD                    vabAAS
011500070227     C                   KFLD                    vabLNP
011600070227     C                   KFLD                    vabNRS
011700070227     C                   KFLD                    vabNSP
011800070227     C*
011900070227     C* Verifico il separatore campo richiesto
012000070227     C                   EVAL      iFLD = %subst(prmppt:2:1)
012100070906     C                   EVAL      iTXT = %trim(%subst(prmppt:3:1))
012200070227     C*
012300070227     C* Verifico correttezza parametri indicati
012400070227     C                   IF        %subst(prmppt:2:1) = *blanks
012500070227     C                   EVAL      iFLD=','
012600070227     C                   ENDIF
012700991027     C*
012800991027     C                   ENDSR
