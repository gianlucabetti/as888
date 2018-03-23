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
005800100408     C*
005900100408     C* Inizializzo indicatori d wrk
006000100408     C                   SETOFF                                       30
006100100408     C*
006200100408     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
006300100408     C                   IF        %subst(prmppt:2:1) = 'A'
006400100408     C                   SETON                                        30
006500100408     C                   ENDIF
006600991027     C*
006700080924     C                   READ      tivgdtmp
006800991027     C                   dow       not %eof
006900080924     c                   movel     vgddta        fnvacds
007000060529     C*
007100060529     C                   SETOFF                                       31
007200991027     C*
007300941221     C                   MOVE      VACAAS        VHCAAS
007400941221     C                   MOVE      VACLNP        VHCLNP
007500941221     C                   MOVE      VACNRS        VHCNRS
007600941221     C                   MOVE      VACNSP        VHCNSP
007700941221     C                   MOVE      VACMGS        VHCMGS
007800941221     C                   MOVE      VACCBO        VHCCBO
007900941221     C                   MOVE      VACLNA        VHCLNA
008000941221     C                   MOVE      VACRSD        VHCRSD
008100021008     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
008200021008     C     1             DO        35            x                 2 0
008300021008     C                   MOVEL     *blanks       wrkCHAR           1
008400021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
008500021008     C                   IF        wrkCHAR <> *blanks AND
008600021008     C                             wrkCHAR <> ''''
008700021008     C     wrkCHAR       LOOKUP    SKcharOK                               20
008800021008     C                   IF        *in20 = *off
008900021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
009000021008     C                   ENDIF
009100021008     C                   ENDIF
009200021008     C                   ENDDO
009300021008     C*
009400941221     C                   MOVE      VACPRD        VHCPRD
009500941221     C                   MOVE      VACGC1        VHCGC1
009600941221     C                   MOVE      VACGC2        VHCGC2
009700941221     C                   MOVE      VACCTR        VHCCTR
009800941221     C                   MOVE      VACCTS        VHCCTS
009900941221     C                   MOVE      VACFTM        VHCFTM
010000941221     C                   MOVE      VACFIN        VHCFIN
010100941221     C                   MOVE      VACFAP        VHCFAP
010200941221     C                   MOVE      VACTSP        VHCTSP
010300020603     C                   MOVE      VACIAS        VHCIAS
010400941221     C                   MOVE      VACVAS        VHCVAS
010500941221     C                   MOVE      VACNAS        VHCNAS
010600941221     C                   MOVE      VACNCL        VHCNCL
010700020603     C                   MOVE      VACPKB        VHCPKB
010800020603     C                   MOVE      VACVLB        VHCVLB
010900020603     C                   MOVE      VACQFT        VHCQFT
011000941221     C                   MOVE      VACTIC        VHCTIC
011100020603     C                   MOVE      VACCAS        VHCCAS
011200941221     C                   MOVE      VACVCA        VHCVCA
011300941221     C                   MOVE      VACCCM        VHCCCM
011400941221     C                   MOVE      VACRMN        VHCRMN
011500100408     C  N30              MOVE      VACRMA        VHCRMA
011600100408     C   30              EVALR     VHCRMA = %trim(VACRMA)
011700941221     C                   MOVE      VACRMO        VHCRMO
011800941221     C                   MOVE      VACFFD        VHCFFD
011900941221     C                   MOVE      VACTCR        VHCTCR
012000941221     C                   MOVE      VACDCR        VHCDCR
012100941221     C                   MOVE      VACHCR        VHCHCR
012200941221     C                   MOVE      VACDCM        VHCDCM
012300941221     C                   MOVE      VACHMC        VHCHMC
012400941221     C                   MOVE      VACTC1        VHCTC1
012500941221     C                   MOVE      VACTC2        VHCTC2
012600941221     C                   MOVE      VACCCA        VHCCCA
012700081016     C                   MOVE      VACDLA        VHCDLA
012800081016     C                   MOVE      VACDAG        VHCDAG
012900081016     C*
013000081016     C                   SETOFF                                       31
013100081016     C*
013200060529     C*
013300060529     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
013400081020     C                   IF        %subst(prmppt:1:1) = 'O'
013500081020     C*
013600081020     C* Inizializzo i campi d wrk
013700081020     C                   movel     *blanks       wEsito1           1
013800081020     C                   movel     *blanks       wEsito2           1
013900081016     C*
014000081020     C* 1 => no messe in consegna
014100081020     C                   IF        *IN31 = *off
014200081016     C                   IF        VACCCA = 'C'
014300081016     C                   SETON                                        31
014400081016     C                   ENDIF
014500081020     C                   ENDIF
014600081020     C*
014700081020     C* 2 => no bolle figlie
014800081020     C                   IF        *IN31 = *off
014900081020     C* Chiamata metodo GetLblTyp
015000081020     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
015100081020     C                                                VACAAS
015200081020     C                                               :VACLNP
015300081020     C                                               :VACNRS
015400081020     C                                               :VACNSP
015500081020     C                                               :OutLblTyp
015600081020     C                                               :OutAnnoBO
015700081020     C                                               :OutLineaParBO
015800081020     C                                               :OutSerieBO
015900081020     C                                               :OutNumSpedBO
016000081020     C                                               :OutDcmBO
016100081020     C                                               :OutCcaBO
016200081020     C                                               :OutRblBO))
016300081020     C* Se trattasi d bolla originale proseguo con le verifiche
016400081020     C                   IF        OutLblTyp <> 'O'
016500081020     C                   SETON                                        31
016600081020     C                   ENDIF
016700081020     C                   ENDIF
016800081020     C*
016900090904 xxx C* 3 => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
017000081020     C                   IF        *IN31 = *off
017100081017     C                   IF        VACCCA = '1' AND
017200081016     C                             VACDCM = *zeros
017300090904     C                   SETON                                        31
017400081016     C                   ENDIF
017500081020     C                   ENDIF
017600081016     C*
017700090904     C* 4 => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
017800090904     C*      consegnata ultima bolla figlia
017900081020     C                   IF        *IN31 = *off
018000081020     C                   IF        VACDCM > *zeros
018100081020     C                   IF        VACCCA = '1' OR
018200081020     C                             VACCCA = '2' OR
018300081020     C                             VACCCA = '9'
018400081016     C* Inizializzo i campi d output
018500081016     C                   move      *blanks       VHCCCA
018600081016     C                   move      *zeros        VHCDCM
018700081016     C* Chiamata metodo GetLastChild
018800081016     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
018900081016     C                                                VACAAS
019000081016     C                                               :VACLNP
019100081016     C                                               :VACNRS
019200081016     C                                               :VACNSP
019300081016     C                                               :OutAnnoFI
019400081016     C                                               :OutLineaParFI
019500081016     C                                               :OutSerieFI
019600081016     C                                               :OutNumSpedFI
019700081016     C                                               :OutDcmFI
019800081016     C                                               :OutCcaFI))
019900081020     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
020000081016     C                   if        OutDcmFI > *zeros
020100081016     C                   move      OutCcaFI      VHCCCA
020200081016     C                   move      OutDcmFI      VHCDCM
020300090904     C                   else
020400090904     C                   SETON                                        31
020500081016     C                   endif
020600081016     C*
020700081016     C                   if        wEsito1 <> *zeros OR
020800081016     C                             wEsito2 <> *zeros
020900081020 xxx C***                SETON                                        31
021000081016     C                   endif
021100060529     C                   ENDIF
021200081020     C                   ENDIF
021300081020     C                   ENDIF
021400081020     C*
021500060529     C                   ENDIF
021600930409     C*
021700060529     C  N31              WRITE     FNVACAA
021800080924     C                   delete    tivgd000
021900991027     C*
022000080924     C                   READ      tivgdtmp
022100991027     C                   ENDdo
022200991027     C*
022300991027     C                   eval      wrkesito = '0'
022400991027     C*
022500910830     C                   ENDSR
022600991027
022700081016
022800081016
022900991027      /TITLE Operazioni iniziali.
023000991027     C     *inzsr        BEGSR
023100991027     C*
023200991027     C     *ENTRY        PLIST
023300991027     C                   parm                    prmppt
023400991027     C     wrkesito      parm      wrkesito      prmesito
023500991027     C*
023600991027     C                   ENDSR
