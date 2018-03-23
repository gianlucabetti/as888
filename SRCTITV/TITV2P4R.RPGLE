000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090323     Ftivawwwt  uF A E             DISK    commit
000600130503     FTITA430C  IF   E           K DISK
000700130503     FFNORM01L  IF   E           K DISK
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200030325
001300130429     D wfdsc1ds      e ds                  extname(wfdsc00f1)
001400130503     D dta4m         e ds
001500060410     D wPKF            s              8
001600060410     D wVLF            s              6
001700051007     D wCAS            s             14
001800060410     D wIFT            s             14
001900991027
002000030908     C                   EXSR      chkRECFILMBR
002100991027     C                   EXSR      traduci
002200921023     C                   SETON                                        LR
002300030908
002400030908     C     chkRECFILMBR  BEGSR
002500030908     C*
002600060410     C                   READ      TIVAWWWT                               55
002700030908     C*
002800030908     C                   ENDSR
002900991027
003000991027     C     traduci       BEGSR
003100051007     C*
003200051007     C* Verifico se richiesto un particolare carattere d separatore campi
003300051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003400051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003500051007     C                   ELSE
003600051007     C                   MOVEL     ','           wSepCampi         1
003700051007     C                   ENDIF
003800051007     C*
003900051007     C* Verifico se richiesto un particolare carattere d separatore decimale
004000051007     C                   SETON                                        60
004100051007     C                   IF        %subst(prmppt:4:1) = ','
004200051007     C                   SETOFF                                       60
004300051007     C                   ENDIF
004400030325     C*
004500030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004600030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004700130506     C                   EVAL      VAWDTA  = '"DSCLNP"'+wSepCampi+
004800060410     C                                       '"DSCNSP"'+wSepCampi+
004900060410     C                                       '"DSCRSM"'+wSepCampi+
005000060410     C                                       '"DSCFBR"'+wSepCampi+
005100060410     C                                       '"DSCNCL"'+wSepCampi+
005200060410     C                                       '"DSCNCD"'+wSepCampi+
005300060410     C                                       '"DSCNCA"'+wSepCampi+
005400130503     C                                       '"DSCNO1"'+wSepCampi+
005500130503     C                                       '"DSCNO2"'+wSepCampi+
005600140730     C                                       '"DSCNAM"'+wSepCampi+
005700140730     C                                       '"DSCRMN"'+wSepCampi+
005800140730     C                                       '"DSCRMA"'+wSepCampi
005900130503     C*
006000060410     C                   WRITE     TIVAW000
006100030325     C                   ENDIF
006200991027     C*
006300060410     C                   READ      TIVGDTMP
006400060410     C                   DOW       not %eof(TIVGDTMP)
006500060411     C*
006600060411     C* Valorizzo il buffer dal campo flat di inout
006700130429     C                   EVAL      wfdsc1ds = vgdDTA
006800130503     C*
006900130503     C                   clear                   dscNO1
007000130503     C                   clear                   dscNO2
007100130503     C                   clear                   dscNAM
007200130503     C*
007300130503     C* Reperisco le note immesse sul ORM da internet
007400130503     C                   eval      ta4TRC = 'M'
007500130503     C     KEYta430_C    chain     tita430c
007600130503     C                   if        %found(tita430c)
007700130503     C                   eval      dta4m = ta4NOT
007800130503     C*
007900130503     C* Quindi aggancio l'orm legato
008000130503     C     KEYorm01_C    chain     fnorm01l
008100130503     C                   if        %found(fnorm01l)
008200130503     C                   eval      dscNO1 = ormNO1
008300130503     C                   eval      dscNO2 = ormNO2
008400130503     C                   eval      dscNAM = ormNAM
008500130503     C                   endif
008600130503     C                   endif
008700991027     C*
008800060410     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
008900130429     C***  '"':''''      XLATE     wfdsc1ds       wfdsc1ds
009000051007     C*
009100051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
009200060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
009300060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
009400060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
009500060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
009600060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
009700060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
009800060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
009900060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
010000051007     C*
010100130506     C                   EVAL      VAWDTA = %trim(%editc(DSCLNP:'Q'))+wSepCampi+
010200060411     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
010300060411     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
010400060411     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
010500060411     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
010600060411     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
010700060411     C                                      %trim(%editc(DSCNCA:'Q'))+wSepCampi+
010800130429     C                                         '"'+%trim(DSCNO1)+'"' +wSepCampi+
010900130429     C                                         '"'+%trim(DSCNO2)+'"' +wSepCampi+
011000140730     C                                         '"'+%trim(DSCNAM)+'"' +wSepCampi+
011100140730     C                                      %trim(%editc(DSCRMN:'X'))+wSepCampi+
011200140730     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi
011300930409     C*
011400060410     C                   WRITE     TIVAW000
011500060410     C                   DELETE    TIVGD000
011600991027     C*
011700060410     C                   READ      TIVGDTMP
011800030325     C                   ENDDO
011900991027     C*
012000030325     C                   EVAL      wrkesito = '0'
012100991027     C*
012200910830     C                   ENDSR
012300991027
012400991027      /TITLE Operazioni iniziali.
012500991027     C     *inzsr        BEGSR
012600991027     C*
012700991027     C     *ENTRY        PLIST
012800991027     C                   parm                    prmppt
012900991027     C     wrkesito      parm      wrkesito      prmesito
013000130503     C*
013100130503     C* Chiave su TITA430C - Completa
013200130503     C     KEYta430_C    KLIST
013300130503     C                   KFLD                    dscAAS
013400130503     C                   KFLD                    dscLNP
013500130503     C                   KFLD                    dscNRS
013600130503     C                   KFLD                    dscNSP
013700130503     C                   KFLD                    ta4TRC
013800130503     C*
013900130503     C* Chiave su FNORM01L - Completa
014000130503     C     KEYorm01_C    KLIST
014100130503     C                   KFLD                    §TA4MPOE
014200130503     C                   KFLD                    §TA4MNSR
014300130503     C                   KFLD                    §TA4MNOR
014400130503     C                   KFLD                    §TA4MNRV
014500991027     C*
014600991027     C                   ENDSR
