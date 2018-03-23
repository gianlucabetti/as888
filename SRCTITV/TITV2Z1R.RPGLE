000100090323     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000200090709     H DECEDIT('0,') DATEDIT(*DMY.)
000300991027
000400170215     Ftivgdtmp  iF   E             DISK
000500090708     FTIVAWWWT  uF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000090708     D fnvabds       e ds                  extname(fnvab00t)
001100080613
001200080613     D*------------------
001300080613     D* LINKING A DEFINIZIONI ESTERNE
001400080613     D*------------------
001500080613     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
001600130516     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
001700130516
001800130516
001900991027
002000030908     C                   EXSR      chkRECFILMBR
002100991027     C                   EXSR      traduci
002200921023     C                   SETON                                        LR
002300130516
002400130516
002500030908
002600030908     C     chkRECFILMBR  BEGSR
002700030908     C*
002800090708     C                   READ      TIVAWWWT                               55
002900030908     C*
003000030908     C                   ENDSR
003100130516
003200130516
003300991027
003400991027     C     traduci       BEGSR
003500030325     C*
003600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
003700030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
003800130516     C                   EVAL      VAWDTA =
003900170215     C                                       'CARRIER'      +','+
004000170215     C                                       'NUMRIFINT'    +','+
004100170215     C                                       'LDV'
004200030325     C*
004300090708     C                   WRITE     TIVAW000
004400030325     C                   ENDIF
004500991027     C*
004600090320     C                   READ      tivgdtmp
004700090320     C                   DOW       not %eof(tivgdtmp)
004800090320     c                   movel     vgddta        fnvabds
004900080613     C*
005000080613     C                   setoff                                       31
005100080613     C*
005200080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
005300080613     C                   if        %subst(prmppt:6:1) = 'O'
005400080613     C*
005500080613     C* Chiamata metodo GetLblTyp
005600080613     C                   if        UBLBLSPE_GetLblTyp(VABAAS
005700080613     C                                               :VABLNP
005800080613     C                                               :VABNRS
005900080613     C                                               :VABNSP
006000130516     C                                               :pOutLblTyp
006100130516     C                                               :pOutAnnoBO
006200130516     C                                               :pOutLineaParBO
006300130516     C                                               :pOutSerieBO
006400130516     C                                               :pOutNumSpedBO
006500130516     C                                               :pOutDcmBO
006600130516     C                                               :pOutCcaBO
006700130516     C                                               :pOutRblBO) = 0
006800140930     C                   if        pOutLblTyp <> 'O' OR
006900140930     C                             pOutRblBO  <> 'N'
007000080613     C                   seton                                        31
007100080613     C                   endif
007200080613     C                   endif
007300080613     C                   endif
007400080618     C*
007500080618     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
007600080618     C                   if        %subst(prmppt:7:1) = 'S'
007700080618     C                   if        VABNRS = *zeros
007800080618     C                   seton                                        31
007900080618     C                   endif
008000080618     C                   endif
008100080613     C*
008200080613     C* Se tutto ok => procedo
008300080613     C                   if        *in31 = *off
008400030325     C*
008500130516     C                   EVAL      VAWDTA =
008600170215     C                                       'BRT'                         +','+
008700170215     C                                       %trim(%editc(VABRMN:'4'))     +','+
008800170215     C                                       %trim(%editc(VABLNP:'X'))     +
008900170215     C                                       %trim(%editc(VABNRS:'X'))     +
009000170215     C                                       %trim(%editc(VABNSP:'X'))
009100930409     C*
009200090708     C                   WRITE     TIVAW000
009300080613     C                   endif
009400991027     C*
009500090320     C                   READ      tivgdtmp
009600030325     C                   ENDDO
009700991027     C*
009800030325     C                   EVAL      wrkesito = '0'
009900991027     C*
010000910830     C                   ENDSR
010100080527
010200080527
010300991027
010400991027      /TITLE Operazioni iniziali.
010500991027     C     *inzsr        BEGSR
010600991027     C*
010700991027     C     *ENTRY        PLIST
010800991027     C                   parm                    prmppt
010900991027     C     wrkesito      parm      wrkesito      prmesito
011000080527     C*
011100991027     C                   ENDSR
