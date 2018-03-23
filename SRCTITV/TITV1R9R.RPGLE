000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500090922     Ftivawwwt  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000080924     D fnvacds       e ds                  extname(fnvac00t)
001100090922     D fnvac1wds     e ds                  extname(fnvac1wt)
001200021008     D*-------------------
001300021008     D* COSTANTI
001400021008     D*-------------------
001500021008     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
001600021008     D                                     abcdefghijklmnopqrstuvwxyz1234567-
001700021008     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
001800021008     D*-------------------
001900021008     D* SCHIERE DI WRK
002000021008     D*-------------------
002100021008     D SKcharOK        S              1    DIM(92)
002200991027
002300081016
002400081016     D*------------------
002500081016     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
002600081016     D*------------------
002700081016     D InAnno          S              4S 0
002800081016     D InLineaPar      S              3S 0
002900081016     D InSerie         S              2S 0
003000081016     D InNumSped       S              7S 0
003100081016     D OutLblTyp       S              1A
003200081016     D OutAnnoBO       S              4S 0
003300081016     D OutLineaParBO   S              3S 0
003400081016     D OutSerieBO      S              2S 0
003500081016     D OutNumSpedBO    S              7S 0
003600081016     D OutDcmBO        S              8S 0
003700081016     D OutCcaBO        S              1A
003800081016     D OutRblBO        S              1A
003900081016     D OutAnnoFI       S              4S 0
004000081016     D OutLineaParFI   S              3S 0
004100081016     D OutSerieFI      S              2S 0
004200081016     D OutNumSpedFI    S              7S 0
004300081016     D OutDcmFI        S              8S 0
004400081016     D OutCcaFI        S              1A
004500081016     D*------------------
004600081016     D* LINKING A DEFINIZIONI ESTERNE
004700081016     D*------------------
004800081016     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
004900021008
005000081016
005100021008
005200021008     C                   MOVEA     charOK        SKcharOK
005300991027     C                   EXSR      traduci
005400921023     C                   SETON                                        LR
005500991027
005600081016
005700081016
005800991027     C     traduci       BEGSR
005900991027     C*
006000080924     C                   READ      tivgdtmp
006100991027     C                   dow       not %eof
006200080924     c                   movel     vgddta        fnvacds
006300060529     C*
006400060529     C                   SETOFF                                       31
006500991027     C*
006600941221     C                   MOVE      VACAAS        VHCAAS
006700941221     C                   MOVE      VACLNP        VHCLNP
006800941221     C                   MOVE      VACNRS        VHCNRS
006900941221     C                   MOVE      VACNSP        VHCNSP
007000941221     C                   MOVE      VACMGS        VHCMGS
007100941221     C                   MOVE      VACCBO        VHCCBO
007200941221     C                   MOVE      VACLNA        VHCLNA
007300941221     C                   MOVE      VACRSD        VHCRSD
007400021008     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
007500021008     C     1             DO        35            x                 2 0
007600021008     C                   MOVEL     *blanks       wrkCHAR           1
007700021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
007800021008     C                   IF        wrkCHAR <> *blanks AND
007900021008     C                             wrkCHAR <> ''''
008000021008     C     wrkCHAR       LOOKUP    SKcharOK                               20
008100021008     C                   IF        *in20 = *off
008200021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
008300021008     C                   ENDIF
008400021008     C                   ENDIF
008500021008     C                   ENDDO
008600021008     C*
008700941221     C                   MOVE      VACPRD        VHCPRD
008800941221     C                   MOVE      VACGC1        VHCGC1
008900941221     C                   MOVE      VACGC2        VHCGC2
009000941221     C                   MOVE      VACCTR        VHCCTR
009100941221     C                   MOVE      VACCTS        VHCCTS
009200941221     C                   MOVE      VACFTM        VHCFTM
009300941221     C                   MOVE      VACFIN        VHCFIN
009400941221     C                   MOVE      VACFAP        VHCFAP
009500941221     C                   MOVE      VACTSP        VHCTSP
009600020603     C                   MOVE      VACIAS        VHCIAS
009700941221     C                   MOVE      VACVAS        VHCVAS
009800941221     C                   MOVE      VACNAS        VHCNAS
009900941221     C                   MOVE      VACNCL        VHCNCL
010000020603     C                   MOVE      VACPKB        VHCPKB
010100020603     C                   MOVE      VACVLB        VHCVLB
010200020603     C                   MOVE      VACQFT        VHCQFT
010300941221     C                   MOVE      VACTIC        VHCTIC
010400020603     C                   MOVE      VACCAS        VHCCAS
010500941221     C                   MOVE      VACVCA        VHCVCA
010600941221     C                   MOVE      VACCCM        VHCCCM
010700941221     C                   MOVE      VACRMN        VHCRMN
010800941221     C                   MOVE      VACRMA        VHCRMA
010900941221     C                   MOVE      VACRMO        VHCRMO
011000941221     C                   MOVE      VACFFD        VHCFFD
011100941221     C                   MOVE      VACTCR        VHCTCR
011200941221     C                   MOVE      VACDCR        VHCDCR
011300941221     C                   MOVE      VACHCR        VHCHCR
011400941221     C                   MOVE      VACDCM        VHCDCM
011500941221     C                   MOVE      VACHMC        VHCHMC
011600941221     C                   MOVE      VACTC1        VHCTC1
011700941221     C                   MOVE      VACTC2        VHCTC2
011800941221     C                   MOVE      VACCCA        VHCCCA
011900090922     C                   MOVE      *zeros        VHCDLA
012000090922     C                   MOVE      *zeros        VHCDAG
012100081016     C*
012200081016     C                   SETOFF                                       31
012300081016     C*
012400060529     C*
012500060529     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
012600081020     C                   IF        %subst(prmppt:1:1) = 'O'
012700081020     C*
012800081020     C* Inizializzo i campi d wrk
012900081020     C                   movel     *blanks       wEsito1           1
013000081020     C                   movel     *blanks       wEsito2           1
013100081016     C*
013200081020     C* 1 => no messe in consegna
013300081020     C                   IF        *IN31 = *off
013400081016     C                   IF        VACCCA = 'C'
013500081016     C                   SETON                                        31
013600081016     C                   ENDIF
013700081020     C                   ENDIF
013800081020     C*
013900081020     C* 2 => no bolle figlie
014000081020     C                   IF        *IN31 = *off
014100081020     C* Chiamata metodo GetLblTyp
014200081020     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
014300081020     C                                                VACAAS
014400081020     C                                               :VACLNP
014500081020     C                                               :VACNRS
014600081020     C                                               :VACNSP
014700081020     C                                               :OutLblTyp
014800081020     C                                               :OutAnnoBO
014900081020     C                                               :OutLineaParBO
015000081020     C                                               :OutSerieBO
015100081020     C                                               :OutNumSpedBO
015200081020     C                                               :OutDcmBO
015300081020     C                                               :OutCcaBO
015400081020     C                                               :OutRblBO))
015500081020     C* Se trattasi d bolla originale proseguo con le verifiche
015600081020     C                   IF        OutLblTyp <> 'O'
015700081020     C                   SETON                                        31
015800081020     C                   ENDIF
015900081020     C                   ENDIF
016000081020     C*
016100090922     C* 3 => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
016200081020     C                   IF        *IN31 = *off
016300081017     C                   IF        VACCCA = '1' AND
016400081016     C                             VACDCM = *zeros
016500090904     C                   SETON                                        31
016600081016     C                   ENDIF
016700081020     C                   ENDIF
016800081016     C*
016900090904     C* 4 => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
017000090904     C*      consegnata ultima bolla figlia
017100081020     C                   IF        *IN31 = *off
017200081020     C                   IF        VACDCM > *zeros
017300081020     C                   IF        VACCCA = '1' OR
017400081020     C                             VACCCA = '2' OR
017500081020     C                             VACCCA = '9'
017600081016     C* Inizializzo i campi d output
017700081016     C                   move      *blanks       VHCCCA
017800081016     C                   move      *zeros        VHCDCM
017900081016     C* Chiamata metodo GetLastChild
018000081016     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
018100081016     C                                                VACAAS
018200081016     C                                               :VACLNP
018300081016     C                                               :VACNRS
018400081016     C                                               :VACNSP
018500081016     C                                               :OutAnnoFI
018600081016     C                                               :OutLineaParFI
018700081016     C                                               :OutSerieFI
018800081016     C                                               :OutNumSpedFI
018900081016     C                                               :OutDcmFI
019000081016     C                                               :OutCcaFI))
019100081020     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
019200081016     C                   if        OutDcmFI > *zeros
019300081016     C                   move      OutCcaFI      VHCCCA
019400081016     C                   move      OutDcmFI      VHCDCM
019500090904     C                   else
019600090904     C                   SETON                                        31
019700081016     C                   endif
019800081016     C*
019900081016     C                   if        wEsito1 <> *zeros OR
020000081016     C                             wEsito2 <> *zeros
020100081020 xxx C***                SETON                                        31
020200081016     C                   endif
020300060529     C                   ENDIF
020400081020     C                   ENDIF
020500081020     C                   ENDIF
020600090922     C*
020700090922     C* In ultima istanza nn ritorno gli esiti senza data consegna valorizzata
020800090922     C                   if        VHCDCM <> *zeros AND
020900090922     C                             VHCDCM <> *blanks
021000091026     C                   else
021100090922     C                   SETON                                        31
021200090922     C                   endif
021300081020     C*
021400060529     C                   ENDIF
021500930409     C*
021600090922     C                   EVAL      vawDTA = fnvac1wds
021700090922     C  N31              WRITE     TIVAW000
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
