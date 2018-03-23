000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300991027
000400120813     FTITA430C  IF   E           K DISK
000500080924     Ftivgdtmp  uF   E             DISK
000600120813     Ftivawwwt  O    E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100080924     D fnvacds       e ds                  extname(fnvac00t)
001200120813     D fnvac1wt      e ds
001300021008     D*-------------------
001400021008     D* COSTANTI
001500021008     D*-------------------
001600021008     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
001700021008     D                                     abcdefghijklmnopqrstuvwxyz1234567-
001800021008     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
001900021008     D*-------------------
002000021008     D* SCHIERE DI WRK
002100021008     D*-------------------
002200021008     D SKcharOK        S              1    DIM(92)
002300991027
002400081016
002500081016     D*------------------
002600081016     D* PASSAGGIO PARAMETRI A PROCEDURE UBLBLSPE
002700081016     D*------------------
002800081016     D InAnno          S              4S 0
002900081016     D InLineaPar      S              3S 0
003000081016     D InSerie         S              2S 0
003100081016     D InNumSped       S              7S 0
003200081016     D OutLblTyp       S              1A
003300081016     D OutAnnoBO       S              4S 0
003400081016     D OutLineaParBO   S              3S 0
003500081016     D OutSerieBO      S              2S 0
003600081016     D OutNumSpedBO    S              7S 0
003700081016     D OutDcmBO        S              8S 0
003800081016     D OutCcaBO        S              1A
003900081016     D OutRblBO        S              1A
004000081016     D OutAnnoFI       S              4S 0
004100081016     D OutLineaParFI   S              3S 0
004200081016     D OutSerieFI      S              2S 0
004300081016     D OutNumSpedFI    S              7S 0
004400081016     D OutDcmFI        S              8S 0
004500081016     D OutCcaFI        S              1A
004600081016     D*------------------
004700081016     D* LINKING A DEFINIZIONI ESTERNE
004800081016     D*------------------
004900081016     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005000021008
005100081016
005200021008
005300021008     C                   MOVEA     charOK        SKcharOK
005400991027     C                   EXSR      traduci
005500921023     C                   SETON                                        LR
005600991027
005700081016
005800081016
005900991027     C     traduci       BEGSR
006000100408     C*
006100100408     C* Inizializzo indicatori d wrk
006200100408     C                   SETOFF                                       30
006300100408     C*
006400100408     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
006500100408     C                   IF        %subst(prmppt:2:1) = 'A'
006600100408     C                   SETON                                        30
006700100408     C                   ENDIF
006800991027     C*
006900080924     C                   READ      tivgdtmp
007000991027     C                   dow       not %eof
007100120813     C                   movel     vgddta        fnvacds
007200120813     C                   clear                   TIVAW000
007300060529     C*
007400060529     C                   SETOFF                                       31
007500991027     C*
007600941221     C                   MOVE      VACAAS        VHCAAS
007700941221     C                   MOVE      VACLNP        VHCLNP
007800941221     C                   MOVE      VACNRS        VHCNRS
007900941221     C                   MOVE      VACNSP        VHCNSP
008000941221     C                   MOVE      VACMGS        VHCMGS
008100941221     C                   MOVE      VACCBO        VHCCBO
008200941221     C                   MOVE      VACLNA        VHCLNA
008300941221     C                   MOVE      VACRSD        VHCRSD
008400021008     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
008500021008     C     1             DO        35            x                 2 0
008600021008     C                   MOVEL     *blanks       wrkCHAR           1
008700021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
008800021008     C                   IF        wrkCHAR <> *blanks AND
008900021008     C                             wrkCHAR <> ''''
009000021008     C     wrkCHAR       LOOKUP    SKcharOK                               20
009100021008     C                   IF        *in20 = *off
009200021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
009300021008     C                   ENDIF
009400021008     C                   ENDIF
009500021008     C                   ENDDO
009600021008     C*
009700941221     C                   MOVE      VACPRD        VHCPRD
009800941221     C                   MOVE      VACGC1        VHCGC1
009900941221     C                   MOVE      VACGC2        VHCGC2
010000941221     C                   MOVE      VACCTR        VHCCTR
010100941221     C                   MOVE      VACCTS        VHCCTS
010200941221     C                   MOVE      VACFTM        VHCFTM
010300941221     C                   MOVE      VACFIN        VHCFIN
010400941221     C                   MOVE      VACFAP        VHCFAP
010500941221     C                   MOVE      VACTSP        VHCTSP
010600020603     C                   MOVE      VACIAS        VHCIAS
010700941221     C                   MOVE      VACVAS        VHCVAS
010800941221     C                   MOVE      VACNAS        VHCNAS
010900941221     C                   MOVE      VACNCL        VHCNCL
011000020603     C                   MOVE      VACPKB        VHCPKB
011100020603     C                   MOVE      VACVLB        VHCVLB
011200020603     C                   MOVE      VACQFT        VHCQFT
011300941221     C                   MOVE      VACTIC        VHCTIC
011400020603     C                   MOVE      VACCAS        VHCCAS
011500941221     C                   MOVE      VACVCA        VHCVCA
011600941221     C                   MOVE      VACCCM        VHCCCM
011700941221     C                   MOVE      VACRMN        VHCRMN
011800100408     C  N30              MOVE      VACRMA        VHCRMA
011900100408     C   30              EVALR     VHCRMA = %trim(VACRMA)
012000941221     C                   MOVE      VACRMO        VHCRMO
012100941221     C                   MOVE      VACFFD        VHCFFD
012200941221     C                   MOVE      VACTCR        VHCTCR
012300941221     C                   MOVE      VACDCR        VHCDCR
012400941221     C                   MOVE      VACHCR        VHCHCR
012500941221     C                   MOVE      VACDCM        VHCDCM
012600941221     C                   MOVE      VACHMC        VHCHMC
012700941221     C                   MOVE      VACTC1        VHCTC1
012800941221     C                   MOVE      VACTC2        VHCTC2
012900941221     C                   MOVE      VACCCA        VHCCCA
013000081016     C                   MOVE      VACDLA        VHCDLA
013100081016     C                   MOVE      VACDAG        VHCDAG
013200081016     C*
013300081016     C                   SETOFF                                       31
013400081016     C*
013500060529     C*
013600060529     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
013700081020     C                   IF        %subst(prmppt:1:1) = 'O'
013800081020     C*
013900081020     C* Inizializzo i campi d wrk
014000081020     C                   movel     *blanks       wEsito1           1
014100081020     C                   movel     *blanks       wEsito2           1
014200081016     C*
014300081020     C* 1 => no messe in consegna
014400081020     C                   IF        *IN31 = *off
014500081016     C                   IF        VACCCA = 'C'
014600081016     C                   SETON                                        31
014700081016     C                   ENDIF
014800081020     C                   ENDIF
014900081020     C*
015000081020     C* 2 => no bolle figlie
015100081020     C                   IF        *IN31 = *off
015200081020     C* Chiamata metodo GetLblTyp
015300081020     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
015400081020     C                                                VACAAS
015500081020     C                                               :VACLNP
015600081020     C                                               :VACNRS
015700081020     C                                               :VACNSP
015800081020     C                                               :OutLblTyp
015900081020     C                                               :OutAnnoBO
016000081020     C                                               :OutLineaParBO
016100081020     C                                               :OutSerieBO
016200081020     C                                               :OutNumSpedBO
016300081020     C                                               :OutDcmBO
016400081020     C                                               :OutCcaBO
016500081020     C                                               :OutRblBO))
016600081020     C* Se trattasi d bolla originale proseguo con le verifiche
016700081020     C                   IF        OutLblTyp <> 'O'
016800081020     C                   SETON                                        31
016900081020     C                   ENDIF
017000081020     C                   ENDIF
017100081020     C*
017200090904 xxx C* 3 => se bolla orig. ma dirottamento senza data consegna => nn restituisco il record
017300081020     C                   IF        *IN31 = *off
017400081017     C                   IF        VACCCA = '1' AND
017500081016     C                             VACDCM = *zeros
017600090904     C                   SETON                                        31
017700081016     C                   ENDIF
017800081020     C                   ENDIF
017900081016     C*
018000090904     C* 4 => se bolla orig. e presente data consegna merce + codice anomalia ritorno solo se
018100090904     C*      consegnata ultima bolla figlia
018200081020     C                   IF        *IN31 = *off
018300081020     C                   IF        VACDCM > *zeros
018400081020     C                   IF        VACCCA = '1' OR
018500081020     C                             VACCCA = '2' OR
018600081020     C                             VACCCA = '9'
018700081016     C* Inizializzo i campi d output
018800081016     C                   move      *blanks       VHCCCA
018900081016     C                   move      *zeros        VHCDCM
019000081016     C* Chiamata metodo GetLastChild
019100081016     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
019200081016     C                                                VACAAS
019300081016     C                                               :VACLNP
019400081016     C                                               :VACNRS
019500081016     C                                               :VACNSP
019600081016     C                                               :OutAnnoFI
019700081016     C                                               :OutLineaParFI
019800081016     C                                               :OutSerieFI
019900081016     C                                               :OutNumSpedFI
020000081016     C                                               :OutDcmFI
020100081016     C                                               :OutCcaFI))
020200081020     C* Se ultima figlia consegnata => restituisco la bolla corrente (ovvero originale)
020300081016     C                   if        OutDcmFI > *zeros
020400081016     C                   move      OutCcaFI      VHCCCA
020500081016     C                   move      OutDcmFI      VHCDCM
020600090904     C                   else
020700090904     C                   SETON                                        31
020800081016     C                   endif
020900081016     C*
021000081016     C                   if        wEsito1 <> *zeros OR
021100081016     C                             wEsito2 <> *zeros
021200081020 xxx C***                SETON                                        31
021300081016     C                   endif
021400060529     C                   ENDIF
021500081020     C                   ENDIF
021600081020     C                   ENDIF
021700081020     C*
021800060529     C                   ENDIF
021900120813     C*
022000120813     C* Valorizzo sempre e cmq il buffer d outrput
022100120813     C                   eval      vawDTA = fnvac1wt
022200120813     C*
022300120813     C* Se bolla da ritornare => se presente ritorno anche il firmatario della consegna
022400120813     C                   if        not *in31
022500120813     C*
022600120813     C* Reperisco la chiave del ORM legato alla bolla corrente
022700120813     C                   eval      ta4TRC = '1'
022800120813     C     KEYta430C     chain     tita430c
022900120813     C                   if        %found(tita430C)
023000120813     C                   if        ta4NOT <> *blanks
023100120813     C                   eval      vawDTA = fnvac1wt + ta4NOT
023200120813     C                   endif
023300120813     C                   endif
023400120813     C                   endif
023500930409     C*
023600120813     C  N31              WRITE     TIVAW000
023700080924     C                   delete    tivgd000
023800991027     C*
023900080924     C                   READ      tivgdtmp
024000991027     C                   ENDdo
024100991027     C*
024200991027     C                   eval      wrkesito = '0'
024300991027     C*
024400910830     C                   ENDSR
024500991027
024600081016
024700081016
024800991027      /TITLE Operazioni iniziali.
024900991027     C     *inzsr        BEGSR
025000991027     C*
025100991027     C     *ENTRY        PLIST
025200991027     C                   parm                    prmppt
025300991027     C     wrkesito      parm      wrkesito      prmesito
025400120813     C*
025500120813     C* Definizione chiavi
025600120813     C*
025700120813     C* Chiave su TITA430C - Completa
025800120813     C     KEYta430C     KLIST
025900120813     C                   KFLD                    vacAAS
026000120813     C                   KFLD                    vacLNP
026100120813     C                   KFLD                    vacNRS
026200120813     C                   KFLD                    vacNSP
026300120813     C                   KFLD                    ta4TRC
026400991027     C*
026500991027     C                   ENDSR
