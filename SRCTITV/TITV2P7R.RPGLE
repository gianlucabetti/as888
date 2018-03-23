000100090323     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000200090709     H DECEDIT('0,') DATEDIT(*DMY.)
000300991027
000400090320     Ftivgdtmp  uF   E             DISK
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
003900130516     C                                       '"VABAAS"'+';'+
004000130516     C                                       '"VABMGS"'+';'+
004100130516     C                                       '"VABRMA"'+';'+
004200130516     C                                       '"VABSPED"'+';'+
004300130516     C                                       '"VABRSD"'+';'+
004400130516     C                                       '"VABIND"'+';'+
004500130516     C                                       '"VABCAD"'+';'+
004600130516     C                                       '"VABLOD"'+';'+
004700130516     C                                       '"VABPRD"'+';'
004800030325     C*
004900090708     C                   WRITE     TIVAW000
005000030325     C                   ENDIF
005100991027     C*
005200090320     C                   READ      tivgdtmp
005300090320     C                   DOW       not %eof(tivgdtmp)
005400090320     c                   movel     vgddta        fnvabds
005500080613     C*
005600080613     C                   setoff                                       31
005700080613     C*
005800080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
005900080613     C                   if        %subst(prmppt:6:1) = 'O'
006000080613     C*
006100080613     C* Chiamata metodo GetLblTyp
006200080613     C                   if        UBLBLSPE_GetLblTyp(VABAAS
006300080613     C                                               :VABLNP
006400080613     C                                               :VABNRS
006500080613     C                                               :VABNSP
006600130516     C                                               :pOutLblTyp
006700130516     C                                               :pOutAnnoBO
006800130516     C                                               :pOutLineaParBO
006900130516     C                                               :pOutSerieBO
007000130516     C                                               :pOutNumSpedBO
007100130516     C                                               :pOutDcmBO
007200130516     C                                               :pOutCcaBO
007300130516     C                                               :pOutRblBO) = 0
007400140930     C                   if        pOutLblTyp <> 'O' OR
007500140930     C                             pOutRblBO  <> 'N'
007600080613     C                   seton                                        31
007700080613     C                   endif
007800080613     C                   endif
007900080613     C                   endif
008000080618     C*
008100080618     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
008200080618     C                   if        %subst(prmppt:7:1) = 'S'
008300080618     C                   if        VABNRS = *zeros
008400080618     C                   seton                                        31
008500080618     C                   endif
008600080618     C                   endif
008700080613     C*
008800080613     C* Se tutto ok => procedo
008900080613     C                   if        *in31 = *off
009000030325     C*
009100130516     C                   EVAL      VAWDTA =
009200130516     C                                       %trim(%editc(VABAAS:'X'))+';'+
009300130516     C                                       %trim(%editc(VABMGS:'X'))+';'+
009400130516     C                                          '"'+%trim(VABRMA)+'"'+';'+
009500130516     C                                       %trim(%editc(VABLNP:'X'))+
009600130516     C                                       %trim(%editc(VABNRS:'X'))+
009700130516     C                                       %trim(%editc(VABNSP:'X'))+';'+
009800090709     C                                          '"'+%trim(VABRSD)+'"'+';'+
009900090709     C                                          '"'+%trim(VABIND)+'"'+';'+
010000130516     C                                          '"'+%trim(VABCAD)+'"'+';'+
010100090709     C                                          '"'+%trim(VABLOD)+'"'+';'+
010200130516     C                                          '"'+%trim(VABPRD)+'"'+';'
010300930409     C*
010400090708     C                   WRITE     TIVAW000
010500080613     C                   endif
010600080613     C*
010700090320     C                   delete    tivgd000
010800991027     C*
010900090320     C                   READ      tivgdtmp
011000030325     C                   ENDDO
011100991027     C*
011200030325     C                   EVAL      wrkesito = '0'
011300991027     C*
011400910830     C                   ENDSR
011500080527
011600080527
011700991027
011800991027      /TITLE Operazioni iniziali.
011900991027     C     *inzsr        BEGSR
012000991027     C*
012100991027     C     *ENTRY        PLIST
012200991027     C                   parm                    prmppt
012300991027     C     wrkesito      parm      wrkesito      prmesito
012400080527     C*
012500991027     C                   ENDSR
