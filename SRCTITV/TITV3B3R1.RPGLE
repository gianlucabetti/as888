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
001100180123     D* formato del file di outout del cliente
001200180208     D TKINP00F        ds
001300180208     D  vuoto01                       3
001400180208     D  TIMHAW                       30
001500180208     D  TIHAWB                       30
001600180208     D  vuoto04                       3
001700180208     D  vuoto05                       3
001800180208     D  vuoto06                       5
001900180208     D  vuoto07                       8
002000180208     D  vuoto08                      30
002100180208     D  vuoto09                      30
002200180208     D  vuoto10                       8
002300180208     D  TISTEP                       15
002400180208     D  TISTDD                       40
002500180208     D  TIDTST                        8
002600180208     D  TIHHST                        6
002700180208     D  vuoto15                       8
002800180208     D  vuoto16                       6
002900180208     D  TISTFI                        1
003000180208     D  vuoto18                       1
003100080613
003200080613     D*------------------
003300080613     D* LINKING A DEFINIZIONI ESTERNE
003400080613     D*------------------
003500080613     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
003600130516     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
003700130516
003800130516
003900991027
004000030908     C                   EXSR      chkRECFILMBR
004100991027     C                   EXSR      traduci
004200921023     C                   SETON                                        LR
004300130516
004400130516
004500030908
004600030908     C     chkRECFILMBR  BEGSR
004700030908     C*
004800090708     C                   READ      TIVAWWWT                               55
004900030908     C*
005000030908     C                   ENDSR
005100130516
005200130516
005300991027
005400991027     C     traduci       BEGSR
005500030325     C*
005600030325     C* Se richiesto nei parametri scrivo una prima riga con l'intestazione dei campi
005700180124     C* da documentazione non dovrebbe richiederla, per cui non la so
005800030908     C                   IF        %subst(prmppt:1:1) = 'I' AND *IN55 = *ON
005900180124     C                   EVAL      VAWDTA = *blank
006000030325     C*
006100090708     C                   WRITE     TIVAW000
006200030325     C                   ENDIF
006300991027     C*
006400090320     C                   READ      tivgdtmp
006500090320     C                   DOW       not %eof(tivgdtmp)
006600090320     c                   movel     vgddta        fnvabds
006700080613     C*
006800080613     C                   setoff                                       31
006900080613     C*
007000180123     C* Se richiesto nei parametri d'ingresso ritorno solamente le bolle originali
007100080613     C                   if        %subst(prmppt:6:1) = 'O'
007200080613     C*
007300180124     C* Mediante il metodo GetLblTyp di UBLBLSPE scopro se
007400180124     C* - la bolla è Originale o Figlia
007500180124     C* - quale tipologia di raggruppamento bolle ha il tipo bolla in fattura
007600080613     C                   if        UBLBLSPE_GetLblTyp(VABAAS
007700080613     C                                               :VABLNP
007800080613     C                                               :VABNRS
007900080613     C                                               :VABNSP
008000130516     C                                               :pOutLblTyp
008100130516     C                                               :pOutAnnoBO
008200130516     C                                               :pOutLineaParBO
008300130516     C                                               :pOutSerieBO
008400130516     C                                               :pOutNumSpedBO
008500130516     C                                               :pOutDcmBO
008600130516     C                                               :pOutCcaBO
008700130516     C                                               :pOutRblBO) = 0
008800180124     C* prendo in condiderazione solo se la bolla è Originale e il suo raggruppamento è Normale
008900180124     C* - quale tipologiadi raggruppamento bolle ha il tipo bolla in fattura
009000140930     C                   if        pOutLblTyp <> 'O' OR
009100140930     C                             pOutRblBO  <> 'N'
009200080613     C                   seton                                        31
009300080613     C                   endif
009400080613     C                   endif
009500080613     C                   endif
009600080618     C*
009700180123     C* Se richiesto nei parametri d'ingresso ritorno solamente le bolle originali
009800080618     C                   if        %subst(prmppt:7:1) = 'S'
009900080618     C                   if        VABNRS = *zeros
010000080618     C                   seton                                        31
010100080618     C                   endif
010200080618     C                   endif
010300080613     C*
010400080613     C* Se tutto ok => procedo
010500180123     C                   if        *in31 = *off
010600030325     C*
010700180123     C                   clear                   TKINP00F
010800180123     C                   eval      TIMHAW = %trim(%editc(VABRMN:'4'))
010900180123     C                   eval      TIHAWB = %trim(%editc(VABRMN:'4'))
011000180123     C                   eval      TISTDD = 'PRESA IN CONSEGNA'
011100180208     C                   eval      TISTEP = '000000000000010'
011200180123     C                   eval      TIDTST =  %trim(%editc(VABDCR:'4'))
011300180124     C                   eval      TIHHST =  %trim(%editc(VABHCR:'4'))
011400180123     C*
011500180124     C                   EVAL      VAWDTA = TKINP00F
011600930409     C*
011700090708     C                   WRITE     TIVAW000
011800080613     C                   endif
011900991027     C*
012000090320     C                   READ      tivgdtmp
012100030325     C                   ENDDO
012200991027     C*
012300030325     C                   EVAL      wrkesito = '0'
012400991027     C*
012500910830     C                   ENDSR
012600080527
012700080527
012800991027
012900991027      /TITLE Operazioni iniziali.
013000991027     C     *inzsr        BEGSR
013100991027     C*
013200991027     C     *ENTRY        PLIST
013300991027     C                   parm                    prmppt
013400991027     C     wrkesito      parm      wrkesito      prmesito
013500080527     C*
013600991027     C                   ENDSR
