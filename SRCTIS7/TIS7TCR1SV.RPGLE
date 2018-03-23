000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500090326     Ffnvacwwt  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000080924     D fnvacds       e ds                  extname(fnvac00t)
001100941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200941221     D* 10+3
001300941221     D                 DS
001400941221     D  WS15                   1      1
001500941221     D  WI15                   2     11
001600991125     D  WV15                  12     12    inz(',')
001700941221     D  WD15                  13     15
001800941221     D  WASC15                 1     15
001900941221     D*  6+1
002000941221     D                 DS
002100941221     D  WS09                   1      1
002200941221     D  WI09                   2      7
002300991125     D  WV09                   8      8    inz(',')
002400941221     D  WD09                   9      9
002500941221     D  WASC09                 1      9
002600941221     D*  2+3
002700941221     D                 DS
002800941221     D  WS07                   1      1
002900941221     D  WI07                   2      3
003000991125     D  WV07                   4      4    inz(',')
003100941221     D  WD07                   5      7
003200941221     D  WASC07                 1      7
003300021007     D*-------------------
003400021007     D* COSTANTI
003500021007     D*-------------------
003600021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
003700021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
003800021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
003900021007     D*-------------------
004000021007     D* SCHIERE DI WRK
004100021007     D*-------------------
004200021007     D SKcharOK        S              1    DIM(92)
004300081017
004400081017
004500081017     D*------------------
004600081017     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
004700081017     D*------------------
004800081017     D InAnno          S              4S 0
004900081017     D InLineaPar      S              3S 0
005000081017     D InSerie         S              2S 0
005100081017     D InNumSped       S              7S 0
005200081017     D OutLblTyp       S              1A
005300081017     D OutAnnoBO       S              4S 0
005400081017     D OutLineaParBO   S              3S 0
005500081017     D OutSerieBO      S              2S 0
005600081017     D OutNumSpedBO    S              7S 0
005700081017     D OutDcmBO        S              8S 0
005800081017     D OutCcaBO        S              1A
005900081017     D OutRblBO        S              1A
006000081017     D OutAnnoFI       S              4S 0
006100081017     D OutLineaParFI   S              3S 0
006200081017     D OutSerieFI      S              2S 0
006300081017     D OutNumSpedFI    S              7S 0
006400081017     D OutDcmFI        S              8S 0
006500081017     D OutCcaFI        S              1A
006600081017     D*------------------
006700081017     D* LINKING A DEFINIZIONI ESTERNE
006800081017     D*------------------
006900081017     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007000021007
007100021007
007200021008     C                   MOVEA     charOK        SKcharOK
007300991027     C                   EXSR      traduci
007400921023     C                   SETON                                        LR
007500991027
007600081017
007700081017
007800991027     C     traduci       BEGSR
007900991027     C*
008000080924     C                   READ      tivgdtmp
008100050622     C                   DOW       not %eof
008200080924     c                   movel     vgddta        fnvacds
008300050622     C*
008400050622     C                   SETOFF                                       31
008500080613     C*
008600080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
008700080613     C                   if        %subst(prmppt:7:1) = 'S'
008800080613     C                   if        VACNRS = *zeros
008900080613     C                   seton                                        31
009000080613     C                   endif
009100080613     C                   endif
009200080613     C*
009300080613     C* Se tutto ok => procedo
009400080613     C                   if        *in31 = *off
009500991027     C*
009600941221     C                   MOVE      VACAAS        VHCAAS
009700941221     C                   MOVE      VACLNP        VHCLNP
009800941221     C                   MOVE      VACNRS        VHCNRS
009900941221     C                   MOVE      VACNSP        VHCNSP
010000941221     C                   MOVE      VACMGS        VHCMGS
010100941221     C                   MOVE      VACCBO        VHCCBO
010200941221     C                   MOVE      VACLNA        VHCLNA
010300021008     C                   MOVE      VACRSD        VHCRSD
010400021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
010500021007     C     1             DO        35            x                 2 0
010600021007     C                   MOVEL     *blanks       wrkCHAR           1
010700021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
010800021008     C                   IF        wrkCHAR <> *blanks AND
010900021008     C                             wrkCHAR <> ''''
011000021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
011100021007     C                   IF        *in20 = *off
011200021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
011300021007     C                   ENDIF
011400021007     C                   ENDIF
011500021007     C                   ENDDO
011600021007     C*
011700941221     C                   MOVE      VACPRD        VHCPRD
011800941221     C                   MOVE      VACGC1        VHCGC1
011900941221     C                   MOVE      VACGC2        VHCGC2
012000941221     C                   MOVE      VACCTR        VHCCTR
012100941221     C                   MOVE      VACCTS        VHCCTS
012200941221     C                   MOVE      VACFTM        VHCFTM
012300941221     C                   MOVE      VACFIN        VHCFIN
012400941221     C                   MOVE      VACFAP        VHCFAP
012500941221     C                   MOVE      VACTSP        VHCTSP
012600941221     C*
012700941221     C                   MOVE      VACIAS        WD15
012800941221     C                   MOVEL     VACIAS        WI15
012900941221     C                   MOVE      WASC15        VHCIAS
013000941221     C*
013100941221     C                   MOVE      VACVAS        VHCVAS
013200941221     C                   MOVE      VACNAS        VHCNAS
013300941221     C                   MOVE      VACNCL        VHCNCL
013400941221     C*
013500941221     C                   MOVE      VACPKB        WD09
013600941221     C                   MOVEL     VACPKB        WI09
013700941221     C                   MOVE      WASC09        VHCPKB
013800941221     C*
013900941221     C                   MOVE      VACVLB        WD07
014000941221     C                   MOVEL     VACVLB        WI07
014100941221     C                   MOVE      WASC07        VHCVLB
014200941221     C*
014300941221     C                   MOVE      VACQFT        WD15
014400941221     C                   MOVEL     VACQFT        WI15
014500941221     C                   MOVE      WASC15        VHCQFT
014600941221     C*
014700941221     C                   MOVE      VACTIC        VHCTIC
014800941221     C*
014900941221     C                   MOVE      VACCAS        WD15
015000941221     C                   MOVEL     VACCAS        WI15
015100941221     C                   MOVE      WASC15        VHCCAS
015200941221     C*
015300941221     C                   MOVE      VACVCA        VHCVCA
015400941221     C                   MOVE      VACCCM        VHCCCM
015500941221     C                   MOVE      VACRMN        VHCRMN
015600941221     C                   MOVE      VACRMA        VHCRMA
015700941221     C                   MOVE      VACRMO        VHCRMO
015800941221     C                   MOVE      VACFFD        VHCFFD
015900941221     C                   MOVE      VACTCR        VHCTCR
016000941221     C                   MOVE      VACDCR        VHCDCR
016100941221     C                   MOVE      VACHCR        VHCHCR
016200941221     C                   MOVE      VACDCM        VHCDCM
016300941221     C                   MOVE      VACHMC        VHCHMC
016400941221     C                   MOVE      VACTC1        VHCTC1
016500941221     C                   MOVE      VACTC2        VHCTC2
016600941221     C                   MOVE      VACCCA        VHCCCA
016700081017 xxx C                   MOVE      VACDLA        VHCDLA
016800081017     C                   MOVE      VACDAG        VHCDAG
016900081017     C*
017000081017     C                   SETOFF                                       31
017100081020     C*
017200081020     C*
017300081020     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
017400081020     C                   IF        %subst(prmppt:1:1) = 'O'
017500081020     C*
017600081020     C* Inizializzo i campi d wrk
017700081020     C                   movel     *blanks       wEsito1           1
017800081020     C                   movel     *blanks       wEsito2           1
017900081020     C*
018000081020     C* 1 => no messe in consegna
018100081020     C                   IF        *IN31 = *off
018200081020     C                   IF        VACCCA = 'C'
018300081020     C                   SETON                                        31
018400081020     C                   ENDIF
018500081020     C                   ENDIF
018600081020     C*
018700081020     C* 2 => no bolle figlie
018800081020     C                   IF        *IN31 = *off
018900081020     C* Chiamata metodo GetLblTyp
019000081020     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019100081020     C                                                VACAAS
019200081020     C                                               :VACLNP
019300081020     C                                               :VACNRS
019400081020     C                                               :VACNSP
019500081020     C                                               :OutLblTyp
019600081020     C                                               :OutAnnoBO
019700081020     C                                               :OutLineaParBO
019800081020     C                                               :OutSerieBO
019900081020     C                                               :OutNumSpedBO
020000081020     C                                               :OutDcmBO
020100081020     C                                               :OutCcaBO
020200081020     C                                               :OutRblBO))
020300081020     C* Se trattasi d bolla originale proseguo con le verifiche
020400081020     C                   IF        OutLblTyp <> 'O'
020500081020     C                   SETON                                        31
020600081020     C                   ENDIF
020700081020     C                   ENDIF
020800081020     C*
020900081020     C* 3 => x i dirottamenti senza data consegna inizializzo il codice consegna anomala
021000081020     C                   IF        *IN31 = *off
021100081020     C                   IF        VACCCA = '1' AND
021200081020     C                             VACDCM = *zeros
021300081020     C* Inizializzo i campi d output
021400081020     C                   move      *blanks       VHCCCA
021500081020     C                   ENDIF
021600081020     C                   ENDIF
021700081020     C*
021800081020     C* 4 => se presente data consegna merce + codice anomalia ritorno solo se consegnata
021900081020     C*      ultima bolla figlia
022000081020     C                   IF        *IN31 = *off
022100081020     C                   IF        VACDCM > *zeros
022200081020     C                   IF        VACCCA = '1' OR
022300081020     C                             VACCCA = '2' OR
022400081020     C                             VACCCA = '9'
022500081020     C* Inizializzo i campi d output
022600081020     C                   move      *blanks       VHCCCA
022700081020     C                   move      *zeros        VHCDCM
022800081020     C* Chiamata metodo GetLblTyp
022900081020     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
023000081020     C                                                VACAAS
023100081020     C                                               :VACLNP
023200081020     C                                               :VACNRS
023300081020     C                                               :VACNSP
023400081020     C                                               :OutLblTyp
023500081020     C                                               :OutAnnoBO
023600081020     C                                               :OutLineaParBO
023700081020     C                                               :OutSerieBO
023800081020     C                                               :OutNumSpedBO
023900081020     C                                               :OutDcmBO
024000081020     C                                               :OutCcaBO
024100081020     C                                               :OutRblBO))
024200081020     C* Se trattasi d bolla originale proseguo con le verifiche
024300081020     C                   if        OutLblTyp = 'O'
024400081020     C* Chiamata metodo GetLastChild
024500081020     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
024600081020     C                                                VACAAS
024700081020     C                                               :VACLNP
024800081020     C                                               :VACNRS
024900081020     C                                               :VACNSP
025000081020     C                                               :OutAnnoFI
025100081020     C                                               :OutLineaParFI
025200081020     C                                               :OutSerieFI
025300081020     C                                               :OutNumSpedFI
025400081020     C                                               :OutDcmFI
025500081020     C                                               :OutCcaFI))
025600081020     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
025700081020     C                   if        OutDcmFI > *zeros
025800081020     C                   move      OutCcaFI      VHCCCA
025900081020     C                   move      OutDcmFI      VHCDCM
026000081020     C                   endif
026100081020     C                   endif
026200081020     C*
026300081020     C                   if        wEsito1 <> *zeros OR
026400081020     C                             wEsito2 <> *zeros
026500081020 xxx C***                SETON                                        31
026600081020     C                   endif
026700081020     C                   ENDIF
026800081020     C                   ENDIF
026900081020     C                   ENDIF
027000081020     C*
027100081020     C                   ENDIF
027200930409     C*
027300050622     C  N31              WRITE     FNVACAA
027400080613     C                   endif
027500080613     C*
027600080924     c                   delete    tivgd000
027700991027     C*
027800080924     C                   READ      tivgdtmp
027900991027     C                   ENDdo
028000991027     C*
028100991027     C                   eval      wrkesito = '0'
028200991027     C*
028300910830     C                   ENDSR
028400991027
028500081017
028600081017
028700991027      /TITLE Operazioni iniziali.
028800991027     C     *inzsr        BEGSR
028900991027     C*
029000991027     C     *ENTRY        PLIST
029100991027     C                   parm                    prmppt
029200991027     C     wrkesito      parm      wrkesito      prmesito
029300050622     C*
029400991027     C                   ENDSR
