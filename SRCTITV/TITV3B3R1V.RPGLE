000100090323     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000200090709     H DECEDIT('0,') DATEDIT(*DMY.)
000300991027
000400180130     FFNVAB00T  iF   E             DISK
000500090708     FTIVAWWWT  uF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000180130     D*fnvabds       e ds                  extname(fnvab00t)
001100180123     D* formato del file di outout del cliente
001200180123     D TKINP00F        ds
001300180130     D  vuoto01                       3
001400180130     D  TIMHAW                       30
001500180130     D  TIHAWB                       30
001600180130     D  vuoto04                       3
001700180130     D  vuoto05                       3
001800180130     D  vuoto06                       5
001900180130     D  vuoto07                       8
002000180130     D  vuoto08                      30
002100180130     D  vuoto09                      30
002200180130     D  vuoto10                       8
002300180130     D  vuoto11                      15
002400180130     D  TISTDD                       40
002500180130     D  TIDTST                        8
002600180130     D  TIHHST                        6
002700180130     D  vuoto15                       8
002800180130     D  vuoto16                       6
002900180130     D  vuoto17                       1
003000180130     D  vuoto18                       1
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
006400180130     C                   READ      FNVAB00T
006500180130     C                   DOW       not %eof(FNVAB00T)
006600180130     c***                movel     vgddta        fnvabds
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
011100180123     C                   eval      TIDTST =  %trim(%editc(VABDCR:'4'))
011200180124     C                   eval      TIHHST =  %trim(%editc(VABHCR:'4'))
011300180123     C*
011400180124     C                   EVAL      VAWDTA = TKINP00F
011500930409     C*
011600090708     C                   WRITE     TIVAW000
011700080613     C                   endif
011800991027     C*
011900180130     C                   READ      FNVAB00T
012000030325     C                   ENDDO
012100991027     C*
012200030325     C                   EVAL      wrkesito = '0'
012300991027     C*
012400910830     C                   ENDSR
012500080527
012600080527
012700991027
012800991027      /TITLE Operazioni iniziali.
012900991027     C     *inzsr        BEGSR
013000991027     C*
013100991027     C     *ENTRY        PLIST
013200991027     C                   parm                    prmppt
013300991027     C     wrkesito      parm      wrkesito      prmesito
013400080527     C*
013500991027     C                   ENDSR
