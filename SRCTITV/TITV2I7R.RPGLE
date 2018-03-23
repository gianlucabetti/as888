000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500111020     FFNEVB01L  IF   E           K DISK
000600090326     Ffnvacwwt  O    E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100080924     D fnvacds       e ds                  extname(fnvac00t)
001200941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001300941221     D* 10+3
001400941221     D                 DS
001500941221     D  WS15                   1      1
001600941221     D  WI15                   2     11
001700991125     D  WV15                  12     12    inz(',')
001800941221     D  WD15                  13     15
001900941221     D  WASC15                 1     15
002000941221     D*  6+1
002100941221     D                 DS
002200941221     D  WS09                   1      1
002300941221     D  WI09                   2      7
002400991125     D  WV09                   8      8    inz(',')
002500941221     D  WD09                   9      9
002600941221     D  WASC09                 1      9
002700941221     D*  2+3
002800941221     D                 DS
002900941221     D  WS07                   1      1
003000941221     D  WI07                   2      3
003100991125     D  WV07                   4      4    inz(',')
003200941221     D  WD07                   5      7
003300941221     D  WASC07                 1      7
003400021007     D*-------------------
003500021007     D* COSTANTI
003600021007     D*-------------------
003700021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
003800021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
003900021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
004000021007     D*-------------------
004100021007     D* SCHIERE DI WRK
004200021007     D*-------------------
004300021007     D SKcharOK        S              1    DIM(92)
004400081017
004500081017
004600081017     D*------------------
004700081017     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
004800081017     D*------------------
004900081017     D InAnno          S              4S 0
005000081017     D InLineaPar      S              3S 0
005100081017     D InSerie         S              2S 0
005200081017     D InNumSped       S              7S 0
005300081017     D OutLblTyp       S              1A
005400081017     D OutAnnoBO       S              4S 0
005500081017     D OutLineaParBO   S              3S 0
005600081017     D OutSerieBO      S              2S 0
005700081017     D OutNumSpedBO    S              7S 0
005800081017     D OutDcmBO        S              8S 0
005900081017     D OutCcaBO        S              1A
006000081017     D OutRblBO        S              1A
006100081017     D OutAnnoFI       S              4S 0
006200081017     D OutLineaParFI   S              3S 0
006300081017     D OutSerieFI      S              2S 0
006400081017     D OutNumSpedFI    S              7S 0
006500081017     D OutDcmFI        S              8S 0
006600081017     D OutCcaFI        S              1A
006700081017     D*------------------
006800081017     D* LINKING A DEFINIZIONI ESTERNE
006900081017     D*------------------
007000081017     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
007100021007
007200021007
007300021008     C                   MOVEA     charOK        SKcharOK
007400991027     C                   EXSR      traduci
007500921023     C                   SETON                                        LR
007600991027
007700081017
007800081017
007900991027     C     traduci       BEGSR
008000991027     C*
008100080924     C                   READ      tivgdtmp
008200050622     C                   DOW       not %eof
008300080924     c                   movel     vgddta        fnvacds
008400050622     C*
008500100211     C                   SETOFF                                       3132
008600080613     C*
008700080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
008800080613     C                   if        %subst(prmppt:7:1) = 'S'
008900080613     C                   if        VACNRS = *zeros
009000080613     C                   seton                                        31
009100080613     C                   endif
009200080613     C                   endif
009300100211     C*
009400100211     C* Se richiesto no ritorno CCA x consegna dirottamenti
009500100211     C                   if        %subst(prmppt:8:1) = 'D'
009600100211     C                   seton                                        32
009700100211     C                   endif
009800080613     C*
009900080613     C* Se tutto ok => procedo
010000080613     C                   if        *in31 = *off
010100991027     C*
010200941221     C                   MOVE      VACAAS        VHCAAS
010300941221     C                   MOVE      VACLNP        VHCLNP
010400941221     C                   MOVE      VACNRS        VHCNRS
010500941221     C                   MOVE      VACNSP        VHCNSP
010600941221     C                   MOVE      VACMGS        VHCMGS
010700941221     C                   MOVE      VACCBO        VHCCBO
010800941221     C                   MOVE      VACLNA        VHCLNA
010900021008     C                   MOVE      VACRSD        VHCRSD
011000021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
011100021007     C     1             DO        35            x                 2 0
011200021007     C                   MOVEL     *blanks       wrkCHAR           1
011300021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
011400021008     C                   IF        wrkCHAR <> *blanks AND
011500021008     C                             wrkCHAR <> ''''
011600021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
011700021007     C                   IF        *in20 = *off
011800021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
011900021007     C                   ENDIF
012000021007     C                   ENDIF
012100021007     C                   ENDDO
012200021007     C*
012300941221     C                   MOVE      VACPRD        VHCPRD
012400941221     C                   MOVE      VACGC1        VHCGC1
012500941221     C                   MOVE      VACGC2        VHCGC2
012600941221     C                   MOVE      VACCTR        VHCCTR
012700941221     C                   MOVE      VACCTS        VHCCTS
012800941221     C                   MOVE      VACFTM        VHCFTM
012900941221     C                   MOVE      VACFIN        VHCFIN
013000941221     C                   MOVE      VACFAP        VHCFAP
013100941221     C                   MOVE      VACTSP        VHCTSP
013200941221     C*
013300941221     C                   MOVE      VACIAS        WD15
013400941221     C                   MOVEL     VACIAS        WI15
013500941221     C                   MOVE      WASC15        VHCIAS
013600941221     C*
013700941221     C                   MOVE      VACVAS        VHCVAS
013800941221     C                   MOVE      VACNAS        VHCNAS
013900941221     C                   MOVE      VACNCL        VHCNCL
014000941221     C*
014100941221     C                   MOVE      VACPKB        WD09
014200941221     C                   MOVEL     VACPKB        WI09
014300941221     C                   MOVE      WASC09        VHCPKB
014400941221     C*
014500941221     C                   MOVE      VACVLB        WD07
014600941221     C                   MOVEL     VACVLB        WI07
014700941221     C                   MOVE      WASC07        VHCVLB
014800941221     C*
014900941221     C                   MOVE      VACQFT        WD15
015000941221     C                   MOVEL     VACQFT        WI15
015100941221     C                   MOVE      WASC15        VHCQFT
015200941221     C*
015300941221     C                   MOVE      VACTIC        VHCTIC
015400941221     C*
015500941221     C                   MOVE      VACCAS        WD15
015600941221     C                   MOVEL     VACCAS        WI15
015700941221     C                   MOVE      WASC15        VHCCAS
015800941221     C*
015900941221     C                   MOVE      VACVCA        VHCVCA
016000941221     C                   MOVE      VACCCM        VHCCCM
016100941221     C                   MOVE      VACRMN        VHCRMN
016200941221     C                   MOVE      VACRMA        VHCRMA
016300941221     C                   MOVE      VACRMO        VHCRMO
016400941221     C                   MOVE      VACFFD        VHCFFD
016500941221     C                   MOVE      VACTCR        VHCTCR
016600941221     C                   MOVE      VACDCR        VHCDCR
016700941221     C                   MOVE      VACHCR        VHCHCR
016800941221     C                   MOVE      VACDCM        VHCDCM
016900941221     C                   MOVE      VACHMC        VHCHMC
017000941221     C                   MOVE      VACTC1        VHCTC1
017100941221     C                   MOVE      VACTC2        VHCTC2
017200941221     C                   MOVE      VACCCA        VHCCCA
017300081017 xxx C                   MOVE      VACDLA        VHCDLA
017400081017     C                   MOVE      VACDAG        VHCDAG
017500081017     C*
017600081017     C                   SETOFF                                       31
017700081020     C*
017800081020     C*
017900081020     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
018000081020     C                   IF        %subst(prmppt:1:1) = 'O'
018100081020     C*
018200081020     C* Inizializzo i campi d wrk
018300081020     C                   movel     *blanks       wEsito1           1
018400081020     C                   movel     *blanks       wEsito2           1
018500081020     C*
018600081020     C* 1 => no messe in consegna
018700081020     C                   IF        *IN31 = *off
018800081020     C                   IF        VACCCA = 'C'
018900081020     C                   SETON                                        31
019000081020     C                   ENDIF
019100081020     C                   ENDIF
019200081020     C*
019300081020     C* 2 => no bolle figlie
019400081020     C                   IF        *IN31 = *off
019500081020     C* Chiamata metodo GetLblTyp
019600081020     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
019700081020     C                                                VACAAS
019800081020     C                                               :VACLNP
019900081020     C                                               :VACNRS
020000081020     C                                               :VACNSP
020100081020     C                                               :OutLblTyp
020200081020     C                                               :OutAnnoBO
020300081020     C                                               :OutLineaParBO
020400081020     C                                               :OutSerieBO
020500081020     C                                               :OutNumSpedBO
020600081020     C                                               :OutDcmBO
020700081020     C                                               :OutCcaBO
020800081020     C                                               :OutRblBO))
020900081020     C* Se trattasi d bolla originale proseguo con le verifiche
021000081020     C                   IF        OutLblTyp <> 'O'
021100081020     C                   SETON                                        31
021200081020     C                   ENDIF
021300081020     C                   ENDIF
021400081020     C*
021500090904 xxx C* 3 => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
021600081020     C                   IF        *IN31 = *off
021700081020     C                   IF        VACCCA = '1' AND
021800081020     C                             VACDCM = *zeros
021900090904     C                   SETON                                        31
022000081020     C                   ENDIF
022100081020     C                   ENDIF
022200081020     C*
022300090904     C* 4 => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
022400090904     C*      consegnata ultima bolla figlia
022500081020     C                   IF        *IN31 = *off
022600081020     C                   IF        VACDCM > *zeros
022700081020     C                   IF        VACCCA = '1' OR
022800081020     C                             VACCCA = '2' OR
022900081020     C                             VACCCA = '9'
023000081020     C* Inizializzo i campi d output
023100081020     C                   move      *blanks       VHCCCA
023200101028     C                   move(p)   '00000000'    VHCDCM
023300081020     C* Chiamata metodo GetLastChild
023400081020     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
023500081020     C                                                VACAAS
023600081020     C                                               :VACLNP
023700081020     C                                               :VACNRS
023800081020     C                                               :VACNSP
023900081020     C                                               :OutAnnoFI
024000081020     C                                               :OutLineaParFI
024100081020     C                                               :OutSerieFI
024200081020     C                                               :OutNumSpedFI
024300081020     C                                               :OutDcmFI
024400081020     C                                               :OutCcaFI))
024500081020     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
024600081020     C                   if        OutDcmFI > *zeros
024700081020     C                   move      OutCcaFI      VHCCCA
024800081020     C                   move      OutDcmFI      VHCDCM
024900090904     C                   else
025000090904     C                   SETON                                        31
025100081020     C                   endif
025200081020     C*
025300081020     C                   if        wEsito1 <> *zeros OR
025400081020     C                             wEsito2 <> *zeros
025500081020 xxx C***                SETON                                        31
025600081020     C                   endif
025700081020     C                   ENDIF
025800081020     C                   ENDIF
025900081020     C                   ENDIF
026000081020     C*
026100081020     C                   ENDIF
026200100211     C*
026300100211     C                   if        *in32
026400100211     C                   if        VHCCCA = '1'
026500100211     C                   eval      VHCCCA = *blanks
026600100211     C                   endif
026700100211     C                   endif
026800111020     C*
026900111020     C* In caso d messa in consegna verifico se occorso evento 'N'
027000111020     C                   IF        VACCCA = 'C'
027100111020     C                   MOVEL     *blanks       wCEV              3
027200111020     C                   Z-ADD     *zeros        wDEV              8 0
027300111020     C                   MOVEL     vacCCA        savCCA            1
027400111020     C                   Z-ADD     vacDCM        savDCM            8 0
027500111020     C                   SETOFF                                       7071
027600111020     C*
027700111020     C* Reperisco gli eventi relativi alla bolla corrente
027800111020     C     KEYevb01P     SETLL     fnevb01l
027900111020     C                   IF        %equal(fnevb01l)
028000111020     C     KEYevb01P     READE     fnevb01l
028100111020     C                   DOW       not %eof(fnevb01l)
028200111020     C*
028300111020     C* Se evento NN annullato lo considero
028400111020     C                   IF        evbATB = *blanks
028500111020     C*
028600111020     C                   SELECT
028700111020     C                   WHEN      evbCEV = 'N'
028800111020     C                   EVAL      wDEV = evbDEV
028900111020     C                   SETON                                        70
029000111020     C                   WHEN      evbCEV = 'MIC'  and
029100111020     C                             evbDEV = vacDCM and
029200111020     C                             *in70  = *on
029300111020     C                   SETON                                        71
029400111020     C                   OTHER
029500111020     C                   SETOFF                                       70
029600111020     C                   ENDSL
029700111020     C*
029800111020     C                   ENDIF
029900111020     C*
030000111020     C* Proseguo la lettura degli eventi x la bolla corrente
030100111020     C     KEYevb01P     READE     fnevb01l
030200111020     C                   ENDDO
030300111020     C                   ENDIF
030400111020     C*
030500111020     C* Se condizioni verificate => scrivo anche un record x l'evento 'N'
030600111020     C                   IF        *IN70 and *IN71
030700111020     C                   EVAL      vacCCA = 'N'
030800111020     C                   EVAL      vacDCM = wDEV
030900111020     C  N31              WRITE     FNVACAA
031000111020     C                   EVAL      vacCCA = savCCA
031100111020     C                   EVAL      vacDCM = savDCM
031200111020     C                   ENDIF
031300111020     C*
031400111020     C                   ENDIF
031500930409     C*
031600050622     C  N31              WRITE     FNVACAA
031700080613     C                   endif
031800080613     C*
031900080924     c                   delete    tivgd000
032000991027     C*
032100080924     C                   READ      tivgdtmp
032200991027     C                   ENDdo
032300991027     C*
032400991027     C                   eval      wrkesito = '0'
032500991027     C*
032600910830     C                   ENDSR
032700991027
032800081017
032900081017
033000991027      /TITLE Operazioni iniziali.
033100991027     C     *inzsr        BEGSR
033200991027     C*
033300991027     C     *ENTRY        PLIST
033400991027     C                   parm                    prmppt
033500991027     C     wrkesito      parm      wrkesito      prmesito
033600111020     C*
033700111020     C* Chiave su FNEVB01L - Parziale
033800111020     C     KEYevb01P     KLIST
033900111020     C                   KFLD                    vacAAS
034000111020     C                   KFLD                    vacLNP
034100111020     C                   KFLD                    vacNRS
034200111020     C                   KFLD                    vacNSP
034300050622     C*
034400991027     C                   ENDSR
