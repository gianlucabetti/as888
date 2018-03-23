000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200110914     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300991027
000400090325     Ftivgdtmp  uF   E             DISK
000500100209     FTIVAWWWT  UF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000110913     D fnvacds       e ds                  extname(fnvac00t)
001100130227     D sepFld          s              1    inz(';')
001200111216
001300111216
001400111221     D pInRichiesta    S              3A
001500111220     D pOutTITAS_DS  E DS                  extname(TITAS00F)
001600111216     D pOutFNARB_DS  E DS                  extname(FNARB00F)
001700111216     D pOutFNBLP_DS  E DS                  extname(FNBLP00F)
001800111221     D pOutEsitoSPE    S              3A
001900111216
002000110913
002100110913
002200110913     D*------------------
002300110913     D* LINKING A DEFINIZIONI ESTERNE
002400110913     D*------------------
002500110913     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002600110913     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002700110913
002800110810
002900991027
003000030908     C                   EXSR      chkRECFILMBR
003100991027     C                   EXSR      traduci
003200921023     C                   SETON                                        LR
003300030908
003400110810
003500110810
003600030908     C     chkRECFILMBR  BEGSR
003700030908     C*
003800100209     C                   READ      TIVAWWWT                               55
003900030908     C*
004000030908     C                   ENDSR
004100110810
004200110810
004300991027
004400991027     C     traduci       BEGSR
004500991027     C*
004600130227     C                   read      tivgdtmp
004700130227     C                   dow       not %eof
004800130227     C                   movel     vgddta        fnvacds
004900130227     C*
005000130227     C* Reprisco i legami bolla x considerazioni sullo stato attuale della bolla
005100130227     C                   movel     *blanks       wEsito1           1
005200130227     C*
005300130227     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
005400130227     C                                                vacAAS
005500130227     C                                               :vacLNP
005600130227     C                                               :vacNRS
005700130227     C                                               :vacNSP
005800130227     C                                               :pOutLblTyp
005900130227     C                                               :pOutAnnoBO
006000130227     C                                               :pOutLineaParBO
006100130227     C                                               :pOutSerieBO
006200130227     C                                               :pOutNumSpedBO
006300130227     C                                               :pOutDcmBO
006400130227     C                                               :pOutCcaBO
006500130227     C                                               :pOutRblBO))
006600130227     C*
006700130227     C* Ritorno solamente le bolle valide come spedizione
006800130227     C                   if        pOutLblTyp = 'O' AND pOutRblBO = 'N'
006900111220     C*
007000111220     C* Reperisco i dati della spedizione
007100130227     C                   exsr      rtvDatiBolla
007200130227     C*
007300130227     C* Se la spedizione risulta consegna in arrivo la ritorno
007400130227     C                   if        %subst(pOutEsitoSPE:1:2) = 'A '
007500130227     C                   if        arbDCM > *zeros and
007600130227     C                             (arbCCA = '1' or arbCCA = *blanks)
007700130227     C*
007800130227     C* Compongo il buffer d out
007900130227     C                   eval      vawDTA = %trim(%editc(arbRMN:'X'))
008000930409     C*
008100130227     C                   write     TIVAW000
008200090325     C                   delete    tivgd000
008300130227     C                   read      tivgdtmp
008400130227     C*
008500130227     C                   endif
008600130227     C                   endif
008700130227     C                   endif
008800130227     C*
008900130227     C                   enddo
009000991027     C*
009100030325     C                   EVAL      wrkesito = '0'
009200991027     C*
009300910830     C                   ENDSR
009400111216
009500111216
009600111216
009700111216     C     rtvDatiBolla  BEGSR
009800130227     C*
009900130227     C                   clear                   pInRichiesta
010000130227     C                   clear                   pOutTITAS_DS
010100130227     C                   clear                   pOutFNARB_DS
010200130227     C                   clear                   pOutFNBLP_DS
010300130227     C                   clear                   pOutEsitoSPE
010400111220     C*
010500111220     C* Reperisco i dati di testata
010600111220     C                   call      'UBRTVSPE'
010700130227     C                   parm      'A  '         pInRichiesta
010800111220     C                   parm      vacAAS        pInAnno
010900111220     C                   parm      vacLNP        pInLineaPar
011000111220     C                   parm      vacNRS        pInSerie
011100111220     C                   parm      vacNSP        pInNumSped
011200111220     C                   parm                    pOutFNARB_DS
011300111220     C                   parm                    pOutFNBLP_DS
011400111221     C                   parm                    pOutTITAS_DS
011500111221     C                   parm                    pOutEsitoSPE
011600111221     C*
011700111216     C                   ENDSR
011800111216
011900991027
012000110810
012100110810
012200991027      /TITLE Operazioni iniziali.
012300991027     C     *inzsr        BEGSR
012400991027     C*
012500991027     C     *ENTRY        PLIST
012600991027     C                   parm                    prmppt
012700991027     C     wrkesito      parm      wrkesito      prmesito
012800030709     C*
012900991027     C                   ENDSR
