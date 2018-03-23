000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090323     Ftivawwwt  uF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000030325
001100060504     D wfdscds       e ds                  extname(wfdsc00f)
001200060410     D wPKF            s              8
001300060410     D wVLF            s              6
001400051007     D wCAS            s             14
001500060410     D wIFT            s             14
001600991027
001700030908     C                   EXSR      chkRECFILMBR
001800991027     C                   EXSR      traduci
001900921023     C                   SETON                                        LR
002000030908
002100030908     C     chkRECFILMBR  BEGSR
002200030908     C*
002300060410     C                   READ      TIVAWWWT                               55
002400030908     C*
002500030908     C                   ENDSR
002600991027
002700991027     C     traduci       BEGSR
002800051007     C*
002900051007     C* Verifico se richiesto un particolare carattere d separatore campi
003000051007     C                   IF        %subst(prmppt:3:1) <> *blanks
003100051007     C                   EVAL      wSepCampi = %subst(prmppt:3:1)
003200051007     C                   ELSE
003300051007     C                   MOVEL     ','           wSepCampi         1
003400051007     C                   ENDIF
003500051007     C*
003600051007     C* Verifico se richiesto un particolare carattere d separatore decimale
003700051007     C                   SETON                                        60
003800051007     C                   IF        %subst(prmppt:4:1) = ','
003900051007     C                   SETOFF                                       60
004000051007     C                   ENDIF
004100030325     C*
004200030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
004300030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
004400090420     C                   EVAL      VAWDTA  =
004500090421     C                             '"ANNO SPED"'                     +wSepCampi+
004600090421     C                             '"P.O. PART"'                     +wSepCampi+
004700090421     C                             '"NUMERO SPED"'                   +wSepCampi+
004800090421     C                             '"CUSTOMER"'                      +wSepCampi+
004900090421     C                             '"CUSTOMER ADDRESS"'              +wSepCampi+
005000090421     C                             '"ZIP CODE"'                      +wSepCampi+
005100090421     C                             '"CITY"'                          +wSepCampi+
005200090421     C                             '"PROV MITT"'                     +wSepCampi+
005300090420     C                             '"RAGIONE SOCIALE DESTINATARIO"'  +wSepCampi+
005400090421     C                             '"GIAC"'                          +wSepCampi+
005500090421     C                             '"BOX"'                           +wSepCampi+
005600090421     C                             '"RIF MIT N"'                     +wSepCampi+
005700090421     C                             '"RIF MIT A(RA)"'                 +wSepCampi+
005800090421     C                             '"NR PALLET"'                     +wSepCampi+
005900090421     C                             '"DATA PALLET"'                   +wSepCampi+
006000090421     C                             '"ID BOX FROM"'                   +wSepCampi+
006100090421     C                             '"ID BOX TO"'
006200030325     C*
006300060410     C                   WRITE     TIVAW000
006400030325     C                   ENDIF
006500991027     C*
006600060410     C                   READ      TIVGDTMP
006700060410     C                   DOW       not %eof(TIVGDTMP)
006800060411     C*
006900060411     C* Valorizzo il buffer dal campo flat di inout
007000060411     C                   EVAL      wfdscds = vgdDTA
007100991027     C*
007200060410     C* Verifico che nn vi siano nei campi alfa caratteri <"> che altrimenti sostituisco con <'>
007300060411     C***  '"':''''      XLATE     wfdscds       wfdscds
007400051007     C*
007500051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
007600060410     C  N60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 , '))
007700060410     C   60              EVAL      wPKF = %trim(%editw(DSCPKF:'    0 . '))
007800060410     C  N60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 ,   '))
007900060410     C   60              EVAL      wVLF = %trim(%editw(DSCVLF:'0 .   '))
008000060410     C  N60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 ,   '))
008100060410     C   60              EVAL      wCAS = %trim(%editw(DSCCAS:'        0 .   '))
008200060410     C  N60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 ,   '))
008300060410     C   60              EVAL      wIFT = %trim(%editw(DSCIFT:'        0 .   '))
008400051007     C*
008500051007     C* Se richiesto nei parametri l'editazione dei campi data => gestisco
008600040206     C                   IF        %subst(prmppt:2:1) = 'D'
008700060411     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
008800060411     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
008900060411     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
009000060411     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
009100060411     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
009200060411     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
009300060411     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
009400060411     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
009500060411     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
009600060411     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
009700060411     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
009800060411     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
009900060411     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
010000060411     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
010100060411     C                             %trim(%editw(DSCDDC:'    /  /  '))+wSepCampi+
010200060411     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
010300090421     C                                      %trim(%editc(DSCNCA:'Q'))
010400060411     C                   ELSE
010500090420     C*
010600090421     C                   EVAL      VAWDTA = %trim(%editc(DSCAAS:'Q'))+wSepCampi+
010700090421     C                                      %trim(%editc(DSCLNP:'Q'))+wSepCampi+
010800090421     C                                      %trim(%editc(DSCNSP:'Q'))+wSepCampi+
010900090421     C                                         '"'+%trim(DSCRSM)+'"' +wSepCampi+
011000090421     C                                         '"'+%trim(DSCINM)+'"' +wSepCampi+
011100090421     C                                         '"'+%trim(DSCCAM)+'"' +wSepCampi+
011200090421     C                                         '"'+%trim(DSCLOM)+'"' +wSepCampi+
011300090421     C                                         '"'+%trim(DSCPRM)+'"' +wSepCampi+
011400090421     C                                         '"'+%trim(DSCRSD)+'"' +wSepCampi+
011500090421     C                                         '"'+%trim(DSCFBR)+'"' +wSepCampi+
011600090421     C                                      %trim(%editc(DSCNCL:'Q'))+wSepCampi+
011700090421     C                                      %trim(%editc(DSCRMN:'Q'))+wSepCampi+
011800090421     C                                         '"'+%trim(DSCRMA)+'"' +wSepCampi+
011900090421     C                                      %trim(%editc(DSCNDC:'Q'))+wSepCampi+
012000090421     C                                      %trim(%editc(DSCDDC:'Q'))+wSepCampi+
012100090421     C                                      %trim(%editc(DSCNCD:'Q'))+wSepCampi+
012200090421     C                                      %trim(%editc(DSCNCA:'Q'))
012300040206     C*
012400040206     C                   ENDIF
012500930409     C*
012600060410     C                   WRITE     TIVAW000
012700060410     C                   DELETE    TIVGD000
012800991027     C*
012900060410     C                   READ      TIVGDTMP
013000030325     C                   ENDDO
013100991027     C*
013200030325     C                   EVAL      wrkesito = '0'
013300991027     C*
013400910830     C                   ENDSR
013500991027
013600991027      /TITLE Operazioni iniziali.
013700991027     C     *inzsr        BEGSR
013800991027     C*
013900991027     C     *ENTRY        PLIST
014000991027     C                   parm                    prmppt
014100991027     C     wrkesito      parm      wrkesito      prmesito
014200991027     C*
014300991027     C                   ENDSR
