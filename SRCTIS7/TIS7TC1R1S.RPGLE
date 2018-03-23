000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500090326     Ffnvac1wt  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000080924     D fnvacds       e ds                  extname(fnvac00t)
001100021008     D*-------------------
001200021008     D* COSTANTI
001300021008     D*-------------------
001400021008     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
001500021008     D                                     abcdefghijklmnopqrstuvwxyz1234567-
001600021008     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
001700021008     D*-------------------
001800021008     D* SCHIERE DI WRK
001900021008     D*-------------------
002000021008     D SKcharOK        S              1    DIM(92)
002100991027
002200081016
002300081016     D*------------------
002400081016     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
002500081016     D*------------------
002600081016     D InAnno          S              4S 0
002700081016     D InLineaPar      S              3S 0
002800081016     D InSerie         S              2S 0
002900081016     D InNumSped       S              7S 0
003000081016     D OutLblTyp       S              1A
003100081016     D OutAnnoBO       S              4S 0
003200081016     D OutLineaParBO   S              3S 0
003300081016     D OutSerieBO      S              2S 0
003400081016     D OutNumSpedBO    S              7S 0
003500081016     D OutDcmBO        S              8S 0
003600081016     D OutCcaBO        S              1A
003700081016     D OutRblBO        S              1A
003800081016     D OutAnnoFI       S              4S 0
003900081016     D OutLineaParFI   S              3S 0
004000081016     D OutSerieFI      S              2S 0
004100081016     D OutNumSpedFI    S              7S 0
004200081016     D OutDcmFI        S              8S 0
004300081016     D OutCcaFI        S              1A
004400081016     D*------------------
004500081016     D* LINKING A DEFINIZIONI ESTERNE
004600081016     D*------------------
004700081016     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
004800021008
004900081016
005000021008
005100021008     C                   MOVEA     charOK        SKcharOK
005200991027     C                   EXSR      traduci
005300921023     C                   SETON                                        LR
005400991027
005500081016
005600081016
005700991027     C     traduci       BEGSR
005800991027     C*
005900080924     C                   READ      tivgdtmp
006000991027     C                   dow       not %eof
006100080924     c                   movel     vgddta        fnvacds
006200060529     C*
006300060529     C                   SETOFF                                       31
006400991027     C*
006500941221     C                   MOVE      VACAAS        VHCAAS
006600941221     C                   MOVE      VACLNP        VHCLNP
006700941221     C                   MOVE      VACNRS        VHCNRS
006800941221     C                   MOVE      VACNSP        VHCNSP
006900941221     C                   MOVE      VACMGS        VHCMGS
007000941221     C                   MOVE      VACCBO        VHCCBO
007100941221     C                   MOVE      VACLNA        VHCLNA
007200941221     C                   MOVE      VACRSD        VHCRSD
007300021008     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
007400021008     C     1             DO        35            x                 2 0
007500021008     C                   MOVEL     *blanks       wrkCHAR           1
007600021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
007700021008     C                   IF        wrkCHAR <> *blanks AND
007800021008     C                             wrkCHAR <> ''''
007900021008     C     wrkCHAR       LOOKUP    SKcharOK                               20
008000021008     C                   IF        *in20 = *off
008100021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
008200021008     C                   ENDIF
008300021008     C                   ENDIF
008400021008     C                   ENDDO
008500021008     C*
008600941221     C                   MOVE      VACPRD        VHCPRD
008700941221     C                   MOVE      VACGC1        VHCGC1
008800941221     C                   MOVE      VACGC2        VHCGC2
008900941221     C                   MOVE      VACCTR        VHCCTR
009000941221     C                   MOVE      VACCTS        VHCCTS
009100941221     C                   MOVE      VACFTM        VHCFTM
009200941221     C                   MOVE      VACFIN        VHCFIN
009300941221     C                   MOVE      VACFAP        VHCFAP
009400941221     C                   MOVE      VACTSP        VHCTSP
009500020603     C                   MOVE      VACIAS        VHCIAS
009600941221     C                   MOVE      VACVAS        VHCVAS
009700941221     C                   MOVE      VACNAS        VHCNAS
009800941221     C                   MOVE      VACNCL        VHCNCL
009900020603     C                   MOVE      VACPKB        VHCPKB
010000020603     C                   MOVE      VACVLB        VHCVLB
010100020603     C                   MOVE      VACQFT        VHCQFT
010200941221     C                   MOVE      VACTIC        VHCTIC
010300020603     C                   MOVE      VACCAS        VHCCAS
010400941221     C                   MOVE      VACVCA        VHCVCA
010500941221     C                   MOVE      VACCCM        VHCCCM
010600941221     C                   MOVE      VACRMN        VHCRMN
010700941221     C                   MOVE      VACRMA        VHCRMA
010800941221     C                   MOVE      VACRMO        VHCRMO
010900941221     C                   MOVE      VACFFD        VHCFFD
011000941221     C                   MOVE      VACTCR        VHCTCR
011100941221     C                   MOVE      VACDCR        VHCDCR
011200941221     C                   MOVE      VACHCR        VHCHCR
011300941221     C                   MOVE      VACDCM        VHCDCM
011400941221     C                   MOVE      VACHMC        VHCHMC
011500941221     C                   MOVE      VACTC1        VHCTC1
011600941221     C                   MOVE      VACTC2        VHCTC2
011700941221     C                   MOVE      VACCCA        VHCCCA
011800081016     C                   MOVE      VACDLA        VHCDLA
011900081016     C                   MOVE      VACDAG        VHCDAG
012000081016     C*
012100081016     C                   SETOFF                                       31
012200081016     C*
012300060529     C*
012400060529     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
012500081020     C                   IF        %subst(prmppt:1:1) = 'O'
012600081020     C*
012700081020     C* Inizializzo i campi d wrk
012800081020     C                   movel     *blanks       wEsito1           1
012900081020     C                   movel     *blanks       wEsito2           1
013000081016     C*
013100081020     C* 1 => no messe in consegna
013200081020     C                   IF        *IN31 = *off
013300081016     C                   IF        VACCCA = 'C'
013400081016     C                   SETON                                        31
013500081016     C                   ENDIF
013600081020     C                   ENDIF
013700081020     C*
013800081020     C* 2 => no bolle figlie
013900081020     C                   IF        *IN31 = *off
014000081020     C* Chiamata metodo GetLblTyp
014100081020     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
014200081020     C                                                VACAAS
014300081020     C                                               :VACLNP
014400081020     C                                               :VACNRS
014500081020     C                                               :VACNSP
014600081020     C                                               :OutLblTyp
014700081020     C                                               :OutAnnoBO
014800081020     C                                               :OutLineaParBO
014900081020     C                                               :OutSerieBO
015000081020     C                                               :OutNumSpedBO
015100081020     C                                               :OutDcmBO
015200081020     C                                               :OutCcaBO
015300081020     C                                               :OutRblBO))
015400081020     C* Se trattasi d bolla originale proseguo con le verifiche
015500081020     C                   IF        OutLblTyp <> 'O'
015600081020     C                   SETON                                        31
015700081020     C                   ENDIF
015800081020     C                   ENDIF
015900081020     C*
016000081020     C* 3 => x i dirottamenti senza data consegna inizializzo il codice consegna anomala
016100081020     C                   IF        *IN31 = *off
016200081017     C                   IF        VACCCA = '1' AND
016300081016     C                             VACDCM = *zeros
016400081020     C* Inizializzo i campi d output
016500081020     C                   move      *blanks       VHCCCA
016600081016     C                   ENDIF
016700081020     C                   ENDIF
016800081016     C*
016900081020     C* 4 => se presente data consegna merce + codice anomalia ritorno solo se consegnata
017000081020     C*      ultima bolla figlia
017100081020     C                   IF        *IN31 = *off
017200081020     C                   IF        VACDCM > *zeros
017300081020     C                   IF        VACCCA = '1' OR
017400081020     C                             VACCCA = '2' OR
017500081020     C                             VACCCA = '9'
017600081016     C* Inizializzo i campi d output
017700081016     C                   move      *blanks       VHCCCA
017800081016     C                   move      *zeros        VHCDCM
017900081016     C* Chiamata metodo GetLblTyp
018000081016     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
018100081016     C                                                VACAAS
018200081016     C                                               :VACLNP
018300081016     C                                               :VACNRS
018400081016     C                                               :VACNSP
018500081016     C                                               :OutLblTyp
018600081016     C                                               :OutAnnoBO
018700081016     C                                               :OutLineaParBO
018800081016     C                                               :OutSerieBO
018900081016     C                                               :OutNumSpedBO
019000081016     C                                               :OutDcmBO
019100081016     C                                               :OutCcaBO
019200081016     C                                               :OutRblBO))
019300081016     C* Se trattasi d bolla originale proseguo con le verifiche
019400081016     C                   if        OutLblTyp = 'O'
019500081016     C* Chiamata metodo GetLastChild
019600081016     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
019700081016     C                                                VACAAS
019800081016     C                                               :VACLNP
019900081016     C                                               :VACNRS
020000081016     C                                               :VACNSP
020100081016     C                                               :OutAnnoFI
020200081016     C                                               :OutLineaParFI
020300081016     C                                               :OutSerieFI
020400081016     C                                               :OutNumSpedFI
020500081016     C                                               :OutDcmFI
020600081016     C                                               :OutCcaFI))
020700081020     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
020800081016     C                   if        OutDcmFI > *zeros
020900081016     C                   move      OutCcaFI      VHCCCA
021000081016     C                   move      OutDcmFI      VHCDCM
021100081016     C                   endif
021200081016     C                   endif
021300081016     C*
021400081016     C                   if        wEsito1 <> *zeros OR
021500081016     C                             wEsito2 <> *zeros
021600081020 xxx C***                SETON                                        31
021700081016     C                   endif
021800060529     C                   ENDIF
021900081020     C                   ENDIF
022000081020     C                   ENDIF
022100081020     C*
022200060529     C                   ENDIF
022300930409     C*
022400060529     C  N31              WRITE     FNVACAA
022500080924     C                   delete    tivgd000
022600991027     C*
022700080924     C                   READ      tivgdtmp
022800991027     C                   ENDdo
022900991027     C*
023000991027     C                   eval      wrkesito = '0'
023100991027     C*
023200910830     C                   ENDSR
023300991027
023400081016
023500081016
023600991027      /TITLE Operazioni iniziali.
023700991027     C     *inzsr        BEGSR
023800991027     C*
023900991027     C     *ENTRY        PLIST
024000991027     C                   parm                    prmppt
024100991027     C     wrkesito      parm      wrkesito      prmesito
024200991027     C*
024300991027     C                   ENDSR
