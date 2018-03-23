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
004200130521     C                                       '"VABRMN"'+';'+
004300130516     C                                       '"VABSPED"'+';'+
004400130516     C                                       '"VABRSD"'+';'+
004500130516     C                                       '"VABIND"'+';'+
004600130516     C                                       '"VABCAD"'+';'+
004700130516     C                                       '"VABLOD"'+';'+
004800130516     C                                       '"VABPRD"'+';'
004900030325     C*
005000090708     C                   WRITE     TIVAW000
005100030325     C                   ENDIF
005200991027     C*
005300090320     C                   READ      tivgdtmp
005400090320     C                   DOW       not %eof(tivgdtmp)
005500090320     c                   movel     vgddta        fnvabds
005600080613     C*
005700080613     C                   setoff                                       31
005800080613     C*
005900080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
006000080613     C                   if        %subst(prmppt:6:1) = 'O'
006100080613     C*
006200080613     C* Chiamata metodo GetLblTyp
006300080613     C                   if        UBLBLSPE_GetLblTyp(VABAAS
006400080613     C                                               :VABLNP
006500080613     C                                               :VABNRS
006600080613     C                                               :VABNSP
006700130516     C                                               :pOutLblTyp
006800130516     C                                               :pOutAnnoBO
006900130516     C                                               :pOutLineaParBO
007000130516     C                                               :pOutSerieBO
007100130516     C                                               :pOutNumSpedBO
007200130516     C                                               :pOutDcmBO
007300130516     C                                               :pOutCcaBO
007400130516     C                                               :pOutRblBO) = 0
007500130516     C                   if        pOutLblTyp <> 'O'
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
009500130521     C                                       %trim(%editc(VABRMN:'4'))+';'+
009600130516     C                                       %trim(%editc(VABLNP:'X'))+
009700130516     C                                       %trim(%editc(VABNRS:'X'))+
009800130516     C                                       %trim(%editc(VABNSP:'X'))+';'+
009900090709     C                                          '"'+%trim(VABRSD)+'"'+';'+
010000090709     C                                          '"'+%trim(VABIND)+'"'+';'+
010100130516     C                                          '"'+%trim(VABCAD)+'"'+';'+
010200090709     C                                          '"'+%trim(VABLOD)+'"'+';'+
010300130516     C                                          '"'+%trim(VABPRD)+'"'+';'
010400930409     C*
010500090708     C                   WRITE     TIVAW000
010600080613     C                   endif
010700080613     C*
010800090320     C                   delete    tivgd000
010900991027     C*
011000090320     C                   READ      tivgdtmp
011100030325     C                   ENDDO
011200991027     C*
011300030325     C                   EVAL      wrkesito = '0'
011400991027     C*
011500910830     C                   ENDSR
011600080527
011700080527
011800991027
011900991027      /TITLE Operazioni iniziali.
012000991027     C     *inzsr        BEGSR
012100991027     C*
012200991027     C     *ENTRY        PLIST
012300991027     C                   parm                    prmppt
012400991027     C     wrkesito      parm      wrkesito      prmesito
012500080527     C*
012600991027     C                   ENDSR
