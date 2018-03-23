000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090513     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300090323
000400050908     FTIVGDTMP  UF   E             DISK
000500090323     Ffnvagwwt  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000050908     D fnvagds       e ds                  extname(fnvag00t)
001100941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200941221     D* 10+3
001300941221     D                 DS
001400941221     D  WS15                   1      1
001500941221     D  WI15                   2     11
001600991125     D  WV15                  12     12    inz(',')
001700941221     D  WD15                  13     15
001800941221     D  WASC15                 1     15
001900090513
002000090513     D*------------------
002100090513     D* LINKING A DEFINIZIONI ESTERNE
002200090513     D*------------------
002300090513     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
002400090513     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
002500090513
002600991027
002700991027     C                   exsr      traduci
002800991027     C                   seton                                        lr
002900991027
003000991027     C     traduci       BEGSR
003100991027     C*
003200050908     C                   READ      TIVGDTMP
003300991027     C                   dow       not %eof
003400050908     C                   eval      fnvagds = vgdDTA
003500991027     C*
003600941229     C                   MOVE      VAGLNP        VHGLNP
003700941229     C                   MOVE      VAGAAS        VHGAAS
003800941229     C                   MOVE      VAGNRS        VHGNRS
003900941229     C                   MOVE      VAGNSP        VHGNSP
004000941229     C                   MOVE      VAGLNA        VHGLNA
004100941229     C                   MOVE      VAGAGC        VHGAGC
004200941229     C                   MOVE      VAGMGC        VHGMGC
004300941229     C                   MOVE      VAGFGC        VHGFGC
004400941229     C                   MOVE      VAGNGC        VHGNGC
004500941229     C                   MOVE      VAGFRG        VHGFRG
004600941229     C                   MOVE      VAGGGA        VHGGGA
004700941229     C                   MOVE      VAGCMC        VHGCMC
004800941229     C                   MOVE      VAGDMC        VHGDMC
004900941229     C                   MOVE      VAGNOT        VHGNOT
005000941229     C                   MOVE      VAGRMN        VHGRMN
005100941229     C                   MOVE      VAGRMA        VHGRMA
005200941229     C                   MOVE      VAGSCM        VHGSCM
005300941229     C                   MOVE      VAGDMM        VHGDMM
005400941229     C                   MOVE      VAGVCS        VHGVCS
005500941221     C*
005600941229     C                   MOVE      VAGCAS        WD15
005700941229     C                   MOVEL     VAGCAS        WI15
005800941229     C                   MOVE      WASC15        VHGCAS
005900941221     C*
006000941229     C                   MOVE      VAGVCA        VHGVCA
006100941229     C                   MOVE      VAGDDM        VHGDDM
006200941229     C                   MOVE      VAGDED        VHGDED
006300941229     C                   MOVE      VAGDUR        VHGDUR
006400941229     C                   MOVE      VAGDCG        VHGDCG
006500941229     C                   MOVE      VAGCFG        VHGCFG
006600941229     C                   MOVE      VAGSGS        VHGSGS
006700941229     C                   MOVE      VAGSGR        VHGSGR
006800941229     C                   MOVE      VAGSGD        VHGSGD
006900941229     C                   MOVE      VAGDSD        VHGDSD
007000941229     C                   MOVE      VAGDSA        VHGDSA
007100941229     C                   MOVE      VAGGGS        VHGGGS
007200941229     C                   MOVE      VAGSGF        VHGSGF
007300941229     C                   MOVE      VAGSGA        VHGSGA
007400941229     C                   MOVE      VAGTFT        VHGTFT
007500941229     C                   MOVE      VAGDBG        VHGDBG
007600941229     C                   MOVE      VAGLPW        VHGLPW
007700941229     C                   MOVE      VAGASW        VHGASW
007800941229     C                   MOVE      VAGNSW        VHGNSW
007900941229     C                   MOVE      VAGNBW        VHGNBW
008000941229     C                   MOVE      VAGKSC        VHGKSC
008100090513     C*
008200090513     C                   SETOFF                                       31
008300090513     C*
008400090513     C*
008500090513     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
008600090513     C                   IF        %subst(prmppt:1:1) = 'O'
008700090513     C*
008800090513     C* Inizializzo i campi d wrk
008900090513     C                   movel     *blanks       wEsito1           1
009000090513     C                   movel     *blanks       wEsito2           1
009100090513     C*
009200090513     C* => No bolle figlie
009300090513     C*
009400090513     C* Chiamata metodo GetLblTyp
009500090513     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
009600090513     C                                                VAGAAS
009700090513     C                                               :VAGLNP
009800090513     C                                               :VAGNRS
009900090513     C                                               :VAGNSP
010000090513     C                                               :pOutLblTyp
010100090513     C                                               :pOutAnnoBO
010200090513     C                                               :pOutLineaParBO
010300090513     C                                               :pOutSerieBO
010400090513     C                                               :pOutNumSpedBO
010500090513     C                                               :pOutDcmBO
010600090513     C                                               :pOutCcaBO
010700090513     C                                               :pOutRblBO))
010800090513     C* Se trattasi d bolla originale proseguo con le verifiche
010900090513     C                   IF        pOutLblTyp <> 'O'
011000090513     C                   SETON                                        31
011100090513     C                   ENDIF
011200090513     C                   ENDIF
011300930409     C*
011400090513     C  N31              WRITE     FNVHGAA
011500050908     C                   DELETE    TIVGD000
011600991027     C*
011700050908     C                   READ      TIVGDTMP
011800991027     C                   ENDdo
011900991027     C*
012000991027     C                   eval      wrkesito = '0'
012100991027     C*
012200910830     C                   ENDSR
012300991027
012400991027      /TITLE Operazioni iniziali.
012500991027     C     *inzsr        BEGSR
012600991027     C*
012700991027     C     *ENTRY        PLIST
012800991027     C                   parm                    prmppt
012900991027     C     wrkesito      parm      wrkesito      prmesito
013000991027     C*
013100991027     C                   ENDSR
