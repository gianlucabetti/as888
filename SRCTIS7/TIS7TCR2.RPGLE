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
008400100211     C                   SETOFF                                       3132
008500080613     C*
008600080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
008700080613     C                   if        %subst(prmppt:7:1) = 'S'
008800080613     C                   if        VACNRS = *zeros
008900080613     C                   seton                                        31
009000080613     C                   endif
009100080613     C                   endif
009200121112     C*
009300121112     C* Se richiesto nei parametri d ingresso di non ritornare le messe in consegna
009400121112     C                   if        %subst(prmppt:6:1) = 'N'
009500121112     C                   if        VACCCA = 'C'
009600121112     C                   seton                                        31
009700121112     C                   endif
009800121112     C                   endif
009900100211     C*
010000100211     C* Se richiesto no ritorno CCA x consegna dirottamenti
010100100211     C                   if        %subst(prmppt:8:1) = 'D'
010200100211     C                   seton                                        32
010300100211     C                   endif
010400080613     C*
010500080613     C* Se tutto ok => procedo
010600080613     C                   if        *in31 = *off
010700991027     C*
010800941221     C                   MOVE      VACAAS        VHCAAS
010900941221     C                   MOVE      VACLNP        VHCLNP
011000941221     C                   MOVE      VACNRS        VHCNRS
011100941221     C                   MOVE      VACNSP        VHCNSP
011200941221     C                   MOVE      VACMGS        VHCMGS
011300941221     C                   MOVE      VACCBO        VHCCBO
011400941221     C                   MOVE      VACLNA        VHCLNA
011500021008     C                   MOVE      VACRSD        VHCRSD
011600021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
011700021007     C     1             DO        35            x                 2 0
011800021007     C                   MOVEL     *blanks       wrkCHAR           1
011900021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
012000021008     C                   IF        wrkCHAR <> *blanks AND
012100021008     C                             wrkCHAR <> ''''
012200021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
012300021007     C                   IF        *in20 = *off
012400021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
012500021007     C                   ENDIF
012600021007     C                   ENDIF
012700021007     C                   ENDDO
012800021007     C*
012900941221     C                   MOVE      VACPRD        VHCPRD
013000941221     C                   MOVE      VACGC1        VHCGC1
013100941221     C                   MOVE      VACGC2        VHCGC2
013200941221     C                   MOVE      VACCTR        VHCCTR
013300941221     C                   MOVE      VACCTS        VHCCTS
013400941221     C                   MOVE      VACFTM        VHCFTM
013500941221     C                   MOVE      VACFIN        VHCFIN
013600941221     C                   MOVE      VACFAP        VHCFAP
013700941221     C                   MOVE      VACTSP        VHCTSP
013800941221     C*
013900941221     C                   MOVE      VACIAS        WD15
014000941221     C                   MOVEL     VACIAS        WI15
014100941221     C                   MOVE      WASC15        VHCIAS
014200941221     C*
014300941221     C                   MOVE      VACVAS        VHCVAS
014400941221     C                   MOVE      VACNAS        VHCNAS
014500941221     C                   MOVE      VACNCL        VHCNCL
014600941221     C*
014700941221     C                   MOVE      VACPKB        WD09
014800941221     C                   MOVEL     VACPKB        WI09
014900941221     C                   MOVE      WASC09        VHCPKB
015000941221     C*
015100941221     C                   MOVE      VACVLB        WD07
015200941221     C                   MOVEL     VACVLB        WI07
015300941221     C                   MOVE      WASC07        VHCVLB
015400941221     C*
015500941221     C                   MOVE      VACQFT        WD15
015600941221     C                   MOVEL     VACQFT        WI15
015700941221     C                   MOVE      WASC15        VHCQFT
015800941221     C*
015900941221     C                   MOVE      VACTIC        VHCTIC
016000941221     C*
016100941221     C                   MOVE      VACCAS        WD15
016200941221     C                   MOVEL     VACCAS        WI15
016300941221     C                   MOVE      WASC15        VHCCAS
016400941221     C*
016500941221     C                   MOVE      VACVCA        VHCVCA
016600941221     C                   MOVE      VACCCM        VHCCCM
016700941221     C                   MOVE      VACRMN        VHCRMN
016800941221     C                   MOVE      VACRMA        VHCRMA
016900941221     C                   MOVE      VACRMO        VHCRMO
017000941221     C                   MOVE      VACFFD        VHCFFD
017100941221     C                   MOVE      VACTCR        VHCTCR
017200941221     C                   MOVE      VACDCR        VHCDCR
017300941221     C                   MOVE      VACHCR        VHCHCR
017400941221     C                   MOVE      VACDCM        VHCDCM
017500941221     C                   MOVE      VACHMC        VHCHMC
017600941221     C                   MOVE      VACTC1        VHCTC1
017700941221     C                   MOVE      VACTC2        VHCTC2
017800941221     C                   MOVE      VACCCA        VHCCCA
017900081017 xxx C                   MOVE      VACDLA        VHCDLA
018000081017     C                   MOVE      VACDAG        VHCDAG
018100081017     C*
018200081017     C                   SETOFF                                       31
018300081020     C*
018400081020     C*
018500081020     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
018600081020     C                   IF        %subst(prmppt:1:1) = 'O'
018700081020     C*
018800081020     C* Inizializzo i campi d wrk
018900081020     C                   movel     *blanks       wEsito1           1
019000081020     C                   movel     *blanks       wEsito2           1
019100081020     C*
019200121112     C* => no bolle figlie
019300081020     C                   IF        *IN31 = *off
019400081020     C* Chiamata metodo GetLblTyp
019500081020     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019600081020     C                                                VACAAS
019700081020     C                                               :VACLNP
019800081020     C                                               :VACNRS
019900081020     C                                               :VACNSP
020000081020     C                                               :OutLblTyp
020100081020     C                                               :OutAnnoBO
020200081020     C                                               :OutLineaParBO
020300081020     C                                               :OutSerieBO
020400081020     C                                               :OutNumSpedBO
020500081020     C                                               :OutDcmBO
020600081020     C                                               :OutCcaBO
020700081020     C                                               :OutRblBO))
020800081020     C* Se trattasi d bolla originale proseguo con le verifiche
020900081020     C                   IF        OutLblTyp <> 'O'
021000081020     C                   SETON                                        31
021100081020     C                   ENDIF
021200081020     C                   ENDIF
021300081020     C*
021400121112 xxx C* => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
021500081020     C                   IF        *IN31 = *off
021600081020     C                   IF        VACCCA = '1' AND
021700081020     C                             VACDCM = *zeros
021800090904     C                   SETON                                        31
021900081020     C                   ENDIF
022000081020     C                   ENDIF
022100081020     C*
022200121112     C* => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
022300090904     C*      consegnata ultima bolla figlia
022400081020     C                   IF        *IN31 = *off
022500081020     C                   IF        VACDCM > *zeros
022600081020     C                   IF        VACCCA = '1' OR
022700081020     C                             VACCCA = '2' OR
022800081020     C                             VACCCA = '9'
022900081020     C* Inizializzo i campi d output
023000081020     C                   move      *blanks       VHCCCA
023100101028     C                   move(p)   '00000000'    VHCDCM
023200081020     C* Chiamata metodo GetLastChild
023300081020     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
023400081020     C                                                VACAAS
023500081020     C                                               :VACLNP
023600081020     C                                               :VACNRS
023700081020     C                                               :VACNSP
023800081020     C                                               :OutAnnoFI
023900081020     C                                               :OutLineaParFI
024000081020     C                                               :OutSerieFI
024100081020     C                                               :OutNumSpedFI
024200081020     C                                               :OutDcmFI
024300081020     C                                               :OutCcaFI))
024400081020     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
024500081020     C                   if        OutDcmFI > *zeros
024600081020     C                   move      OutCcaFI      VHCCCA
024700081020     C                   move      OutDcmFI      VHCDCM
024800090904     C                   else
024900090904     C                   SETON                                        31
025000081020     C                   endif
025100081020     C*
025200081020     C                   if        wEsito1 <> *zeros OR
025300081020     C                             wEsito2 <> *zeros
025400081020 xxx C***                SETON                                        31
025500081020     C                   endif
025600081020     C                   ENDIF
025700081020     C                   ENDIF
025800081020     C                   ENDIF
025900081020     C*
026000081020     C                   ENDIF
026100100211     C*
026200100211     C                   if        *in32
026300100211     C                   if        VHCCCA = '1'
026400100211     C                   eval      VHCCCA = *blanks
026500100211     C                   endif
026600100211     C                   endif
026700930409     C*
026800050622     C  N31              WRITE     FNVACAA
026900080613     C                   endif
027000080613     C*
027100080924     c                   delete    tivgd000
027200991027     C*
027300080924     C                   READ      tivgdtmp
027400991027     C                   ENDdo
027500991027     C*
027600991027     C                   eval      wrkesito = '0'
027700991027     C*
027800910830     C                   ENDSR
027900991027
028000081017
028100081017
028200991027      /TITLE Operazioni iniziali.
028300991027     C     *inzsr        BEGSR
028400991027     C*
028500991027     C     *ENTRY        PLIST
028600991027     C                   parm                    prmppt
028700991027     C     wrkesito      parm      wrkesito      prmesito
028800050622     C*
028900991027     C                   ENDSR
