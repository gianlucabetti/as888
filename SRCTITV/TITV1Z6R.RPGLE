000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090323     FTIVGDTMP  uF   E             DISK
000500100803     FTIVAWWWT  uF A E             DISK    commit
000600100803     Ffnorm01l  iF   E           K DISK
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100090323     D fnvapds       e ds                  extname(fnvap00t)
001200100803
001300991027
001400100803
001500030908     C                   EXSR      chkRECFILMBR
001600991027     C                   EXSR      traduci
001700921023     C                   SETON                                        LR
001800100803
001900100803
002000030908
002100030908     C     chkRECFILMBR  BEGSR
002200030908     C*
002300100803     C                   READ      TIVAWWWT                               55
002400030908     C*
002500030908     C                   ENDSR
002600100803
002700100803
002800991027
002900991027     C     traduci       BEGSR
003000051007     C*
003100051007     C* Verifico se richiesto un particolare carattere d separatore campi
003200051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003300051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003400051007     C                   ELSE
003500051007     C                   MOVEL     ','           wSepCampi         1
003600051007     C                   ENDIF
003700051007     C*
003800051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003900051007     C                   SETON                                        60
004000051007     C                   IF        %subst(prmppt:4:1) = ','
004100051007     C                   SETOFF                                       60
004200051007     C                   ENDIF
004300030325     C*
004400030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004500030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004600100803     C                   EVAL      VAWDTA  = '"VAPIDC"'+wSepCampi+
004700060719     C                                       '"VAPPOE"'+wSepCampi+
004800060719     C                                       '"VAPDPE"'+wSepCampi+
004900060719     C                                       '"VAPNSR"'+wSepCampi+
005000060719     C                                       '"VAPNOR"'+wSepCampi+
005100060719     C                                       '"VAPNRV"'+wSepCampi+
005200060719     C                                       '"VAPRFA"'+wSepCampi+
005300060719     C                                       '"VAPPOG"'+wSepCampi+
005400060719     C                                       '"VAPDPG"'+wSepCampi+
005500060719     C                                       '"VAPDAE"'+wSepCampi+
005600060719     C                                       '"VAPORE"'+wSepCampi+
005700060719     C                                       '"VAPFAR"'+wSepCampi+
005800060719     C                                       '"VAPDFA"'+wSepCampi+
005900060719     C                                       '"VAPCAR"'+wSepCampi+
006000060719     C                                       '"VAPDCA"'+wSepCampi+
006100100803     C                                       '"VAPNOT"'+wSepCampi+
006200100803     C                                       '"VAPNCL"'+wSepCampi
006300030325     C*
006400100803     C                   WRITE     TIVAW000
006500030325     C                   ENDIF
006600991027     C*
006700090323     C                   READ      TIVGDTMP
006800090323     C                   DOW       not %eof(TIVGDTMP)
006900090323     C                   eval      fnvapds = vgdDTA
007000100803     C*
007100100803     C* Reperisco alcuni dati direttamente dal file orm
007200100803     C                   exsr      repORM
007300051007     C*
007400051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
007500040206     C                   IF        %subst(prmppt:2:1) = 'D'
007600100803     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
007700060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
007800060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
007900060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
008000060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
008100060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
008200060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
008300060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
008400060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
008500060719     C                             %trim(%editw(VAPDAE:'    /  /  '))+wSepCampi+
008600060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
008700060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
008800060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
008900060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
009000060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
009100100803     C                                         '"'+%trim(VAPNOT)+'"' +wSepCampi+
009200100803     C                                      %trim(%editc(ORMNCL:'X'))+wSepCampi
009300040206     C                   ELSE
009400060719     C*
009500100803     C                   EVAL      VAWDTA  =   '"'+%trim(VAPIDC)+'"' +wSepCampi+
009600060719     C                                      %trim(%editc(VAPPOE:'Q'))+wSepCampi+
009700060719     C                                         '"'+%trim(VAPDPE)+'"' +wSepCampi+
009800060719     C                                      %trim(%editc(VAPNSR:'Q'))+wSepCampi+
009900060719     C                                      %trim(%editc(VAPNOR:'Q'))+wSepCampi+
010000060719     C                                      %trim(%editc(VAPNRV:'Q'))+wSepCampi+
010100060719     C                                         '"'+%trim(VAPRFA)+'"' +wSepCampi+
010200060719     C                                      %trim(%editc(VAPPOG:'Q'))+wSepCampi+
010300060719     C                                         '"'+%trim(VAPDPG)+'"' +wSepCampi+
010400060719     C                                      %trim(%editc(VAPDAE:'Q'))+wSepCampi+
010500060719     C                                      %trim(%editc(VAPORE:'Q'))+wSepCampi+
010600060719     C                                      %trim(%editc(VAPFAR:'Q'))+wSepCampi+
010700060719     C                                         '"'+%trim(VAPDFA)+'"' +wSepCampi+
010800060719     C                                         '"'+%trim(VAPCAR)+'"' +wSepCampi+
010900060719     C                                         '"'+%trim(VAPDCA)+'"' +wSepCampi+
011000100803     C                                         '"'+%trim(VAPNOT)+'"' +wSepCampi+
011100100803     C                                      %trim(%editc(ORMNCL:'X'))+wSepCampi
011200040206     C*
011300040206     C                   ENDIF
011400930409     C*
011500100803     C                   WRITE     TIVAW000
011600090323     C                   DELETE    TIVGD000
011700991027     C*
011800090323     C                   READ      TIVGDTMP
011900030325     C                   ENDDO
012000991027     C*
012100030325     C                   EVAL      wrkesito = '0'
012200991027     C*
012300910830     C                   ENDSR
012400100803
012500100803
012600100803
012700100803      /TITLE Reperimento dati orm
012800100803     C     REPORM        BEGSR
012900100803     C*
013000100803     C                   clear                   ormNCL
013100100803     C*
013200100803     C* Aggancio lìorm relativo alla fase corrente
013300100803     C     KEYorm01_C    CHAIN     fnorm01l
013400100803     C*
013500100803     C                   ENDSR
013600100803
013700100803
013800100803
013900991027
014000991027      /TITLE Operazioni iniziali.
014100991027     C     *inzsr        BEGSR
014200991027     C*
014300991027     C     *ENTRY        PLIST
014400991027     C                   parm                    prmppt
014500991027     C     wrkesito      parm      wrkesito      prmesito
014600100803     C*
014700100803     C     KEYorm01_C    KLIST
014800100803     C                   KFLD                    vapPOE
014900100803     C                   KFLD                    vapNSR
015000100803     C                   KFLD                    vapNOR
015100100803     C                   KFLD                    vapNRV
015200100803     C*
015300991027     C                   ENDSR
