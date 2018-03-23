000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000501180313     Ftivawwwt  O    E             DISK    commit
000600050714     FFNLBL01L  IF   E           K DISK
000700050622     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002)
000800050622     FTITAS30C  IF   E           K DISK
000801180313     Ftita430c  if   e           k disk
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300080924     D fnvacds       e ds                  extname(fnvac00t)
001302180313     d fnvacwwds     e ds                  extname(fnvacwwt)
001303180313     D fnvacds1      e ds
001400941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001500941221     D* 10+3
001600941221     D                 DS
001700941221     D  WS15                   1      1
001800941221     D  WI15                   2     11
001900991125     D  WV15                  12     12    inz(',')
002000941221     D  WD15                  13     15
002100941221     D  WASC15                 1     15
002200941221     D*  6+1
002300941221     D                 DS
002400941221     D  WS09                   1      1
002500941221     D  WI09                   2      7
002600991125     D  WV09                   8      8    inz(',')
002700941221     D  WD09                   9      9
002800941221     D  WASC09                 1      9
002900941221     D*  2+3
003000941221     D                 DS
003100941221     D  WS07                   1      1
003200941221     D  WI07                   2      3
003300991125     D  WV07                   4      4    inz(',')
003400941221     D  WD07                   5      7
003500941221     D  WASC07                 1      7
003600021007     D*-------------------
003700021007     D* COSTANTI
003800021007     D*-------------------
003900021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
004000021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
004100021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
004200021007     D*-------------------
004300021007     D* SCHIERE DI WRK
004400021007     D*-------------------
004500021007     D SKcharOK        S              1    DIM(92)
004600021007
004700021007
004800021008     C                   MOVEA     charOK        SKcharOK
004900991027     C                   EXSR      traduci
005000921023     C                   SETON                                        LR
005100991027
005200991027     C     traduci       BEGSR
005300991027     C*
005400080924     C                   READ      tivgdtmp
005500050622     C                   DOW       not %eof
005600080924     c                   movel     vgddta        fnvacds
005601180313     C                   clear                   TIVAW000
005602180313     C                   clear                   fnvacds1
005700050622     C*
005800050622     C                   SETOFF                                       31
005900080613     C*
006000080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
006100080613     C                   if        %subst(prmppt:7:1) = 'S'
006200080613     C                   if        VACNRS = *zeros
006300080613     C                   seton                                        31
006400080613     C                   endif
006500080613     C                   endif
006600080613     C*
006700080613     C* Se tutto ok => procedo
006800080613     C                   if        *in31 = *off
006900991027     C*
007000941221     C                   MOVE      VACAAS        VHCAAS
007100941221     C                   MOVE      VACLNP        VHCLNP
007200941221     C                   MOVE      VACNRS        VHCNRS
007300941221     C                   MOVE      VACNSP        VHCNSP
007400941221     C                   MOVE      VACMGS        VHCMGS
007500941221     C                   MOVE      VACCBO        VHCCBO
007600941221     C                   MOVE      VACLNA        VHCLNA
007700021008     C                   MOVE      VACRSD        VHCRSD
007800021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
007900021007     C     1             DO        35            x                 2 0
008000021007     C                   MOVEL     *blanks       wrkCHAR           1
008100021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
008200021008     C                   IF        wrkCHAR <> *blanks AND
008300021008     C                             wrkCHAR <> ''''
008400021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
008500021007     C                   IF        *in20 = *off
008600021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
008700021007     C                   ENDIF
008800021007     C                   ENDIF
008900021007     C                   ENDDO
009000021007     C*
009100941221     C                   MOVE      VACPRD        VHCPRD
009200941221     C                   MOVE      VACGC1        VHCGC1
009300941221     C                   MOVE      VACGC2        VHCGC2
009400941221     C                   MOVE      VACCTR        VHCCTR
009500941221     C                   MOVE      VACCTS        VHCCTS
009600941221     C                   MOVE      VACFTM        VHCFTM
009700941221     C                   MOVE      VACFIN        VHCFIN
009800941221     C                   MOVE      VACFAP        VHCFAP
009900941221     C                   MOVE      VACTSP        VHCTSP
010000941221     C*
010100941221     C                   MOVE      VACIAS        WD15
010200941221     C                   MOVEL     VACIAS        WI15
010300941221     C                   MOVE      WASC15        VHCIAS
010400941221     C*
010500941221     C                   MOVE      VACVAS        VHCVAS
010600941221     C                   MOVE      VACNAS        VHCNAS
010700941221     C                   MOVE      VACNCL        VHCNCL
010800941221     C*
010900941221     C                   MOVE      VACPKB        WD09
011000941221     C                   MOVEL     VACPKB        WI09
011100941221     C                   MOVE      WASC09        VHCPKB
011200941221     C*
011300941221     C                   MOVE      VACVLB        WD07
011400941221     C                   MOVEL     VACVLB        WI07
011500941221     C                   MOVE      WASC07        VHCVLB
011600941221     C*
011700941221     C                   MOVE      VACQFT        WD15
011800941221     C                   MOVEL     VACQFT        WI15
011900941221     C                   MOVE      WASC15        VHCQFT
012000941221     C*
012100941221     C                   MOVE      VACTIC        VHCTIC
012200941221     C*
012300941221     C                   MOVE      VACCAS        WD15
012400941221     C                   MOVEL     VACCAS        WI15
012500941221     C                   MOVE      WASC15        VHCCAS
012600941221     C*
012700941221     C                   MOVE      VACVCA        VHCVCA
012800941221     C                   MOVE      VACCCM        VHCCCM
012900941221     C                   MOVE      VACRMN        VHCRMN
013000941221     C                   MOVE      VACRMA        VHCRMA
013100941221     C                   MOVE      VACRMO        VHCRMO
013200941221     C                   MOVE      VACFFD        VHCFFD
013300941221     C                   MOVE      VACTCR        VHCTCR
013400941221     C                   MOVE      VACDCR        VHCDCR
013500941221     C                   MOVE      VACHCR        VHCHCR
013600941221     C                   MOVE      VACDCM        VHCDCM
013700941221     C                   MOVE      VACHMC        VHCHMC
013800941221     C                   MOVE      VACTC1        VHCTC1
013900941221     C                   MOVE      VACTC2        VHCTC2
014000941221     C                   MOVE      VACCCA        VHCCCA
014100140418     C*
014200140418     C* Se richiesto in caso di sped. 8888/9999 sostituisco con il KSC
014300140418     C                   if        %subst(prmppt:8:1) = 'S'
014400140418     C     KEYlbl01C     CHAIN     TITAS30C
014500140418     C                   if        %found(TITAS30C)
014600140418     C                   move      VACCCM        w8888             4
014700140418     C                   if        w8888 = '8888' or w8888 = '9999'
014800140418     C                   MOVE      tasKSC        VHCCCM
014900140418     C                   endif
015000140418     C                   endif
015100140418     C                   endif
015200050622     C*
015300050622     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
015400050622     C* - no bolle figlie
015500050622     C* - no bolle mamme se CCA=1 e bolle figlie legate NN CONS. o NN RESI
015600060619     C* - no bolle mamme se CCA=2 e bolle figlie legate NN CONS. o NN RESI
015700060619     C                   MOVEL     *blanks       wTip              1
015800060619     C                   EVAL      wTip = %subst(prmppt:1:1)
015900060619     C                   IF        wTip <> *blanks
016000060619     C* Se CCA=2 e richiesta forzatura relativa nei parametri azzero la data consegna merce
016100060619     C                   IF        VHCCCA='2' and (wTip='2' or wTip='3')
016200060619     C                   MOVE      *zeros        VHCDCM
016300060619     C                   ENDIF
016400050622     C* Verifico se trattasi d bolla "figlia"
016500050622     C     KEYlbl01C     CHAIN     FNLBL01L
016600050622     C                   IF        %found(FNLBL01L)
016700050622     C                   SETON                                        31
016800050622     C                   ELSE
016900060619     C* Se CCA=1 o CCA=2 => verifico stato attuale dell'ultima bolla figlia legata
017000060619     C                   IF        (VACCCA='1' and wTip='1') or
017100060619     C                             (VACCCA='2' and wTip='2') or
017200060619     C                             ((VACCCA='1' or VACCCA='2') and wTip='3')
017300050622     C     KEYlbl01C     CHAIN     FNLBL02L
017400050622     C                   IF        %found(FNLBL02L)
017500050714     C     KEYlbl02P     SETLL     FNLBL02L
017600050714     C                   IF        %equal(FNLBL02L)
017700050714     C     KEYlbl02P     READE     FNLBL02L
017800050714     C                   DOW       not %eof(FNLBL02L)
017900050714     C     KEYlbl02P     READE     FNLBL02L
018000050622     C                   ENDDO
018100050714     C                   ENDIF
018200050622     C* A questo punto con la chiave dell'ultima bolla figlia letta verifico il suo stato attuale
018300050622     C     KEYlbl02P     CHAIN     TITAS30C
018400050622     C                   IF        %found(TITAS30C)
018500050622     C                   IF        tasDCM > *zeros AND
018600050622     C                             (tasCCA = *blanks OR tasCCA = '2')
018700050623     C* Se ultima bolla figlia chiusa con consegna OK o reso => forzo lo stato d quest'ultima
018800050623     C* sulla bolla originale
018900050623     C                   EVAL      VHCCCA = tasCCA
019000050622     C                   ELSE
019100050622     C                   SETON                                        31
019200050622     C                   ENDIF
019300050622     C                   ENDIF
019400050622     C                   ENDIF
019500050622     C                   ENDIF
019600050622     C                   ENDIF
019700050622     C                   ENDIF
019800050622     C*
019900941221     C                   MOVE      VACDLA        VHCDLA
020000941221     C                   MOVE      VACDAG        VHCDAG
020001180313     C
020100930409     C*
020101180312     C* Se bolla da ritornare => se presente ritorno anche il firmatario della consegna
020103180312     C                   IF        not *in31 and %subst(prmppt:11:1) = 'S'
020104180312     C*
020105180312     C* Reperisco la chiave del ORM legato alla bolla corrente
020106180312     C                   eval      ta4TRC = '1'
020107180312     C     KEYta430C     chain     tita430c
020108180312     C                   if        %found(tita430C)
020109180312     C                   if        ta4NOT <> *blanks
020110180313     C                   eval      vac1fir = ta4NOT
020111180312     C                   endif
020112180312     C                   endif
020113180312     C                   endif
020114180313     C* Se bolla da ritornare => se richiesto ritorno anche dati aggiuntivi destinatario
020115180312     C                   IF        not *in31 and %subst(prmppt:5:1) = 'I'
020116180313     C     KEYlbl01C     CHAIN     TITAS30C
020117180313     C                   if        %found(TITAS30C)
020118180313     C                   eval      vac1ind = tasind
020119180313     C                   eval      vac1cad = tascad
020120180313     C                   eval      vac1lod = taslod
020121180313     C                   eval      vac1nzd = tasnzd
020122180313     C                   endif
020124180312     C                   ENDIF
020125180313     C
020126180313     C* Valorizzo il buffer di output
020127180313     C                   eval      vawDTA = fnvacwwds + fnvacds1
020201180313     C  N31              WRITE     TIVAW000
020300080613     C                   endif
020400080613     C*
020500080924     c                   delete    tivgd000
020600991027     C*
020700080924     C                   READ      tivgdtmp
020800991027     C                   ENDdo
020900991027     C*
021000991027     C                   eval      wrkesito = '0'
021100991027     C*
021200910830     C                   ENDSR
021300991027
021400991027      /TITLE Operazioni iniziali.
021500991027     C     *inzsr        BEGSR
021600991027     C*
021700991027     C     *ENTRY        PLIST
021800991027     C                   parm                    prmppt
021900991027     C     wrkesito      parm      wrkesito      prmesito
022000050622     C*
022100050622     C* Chiave su FNLBL01L - completa
022200050622     C     KEYlbl01C     KLIST
022300050622     C                   KFLD                    vacAAS
022400050622     C                   KFLD                    vacLNP
022500050622     C                   KFLD                    vacNRS
022600050622     C                   KFLD                    vacNSP
022700050622     C*
022800050622     C     KEYlbl02P     KLIST
022900050622     C                   KFLD                    lblAAN
023000050622     C                   KFLD                    lblLPN
023100050622     C                   KFLD                    lblNRN
023200050622     C                   KFLD                    lblNSN
023201180313     C
023202180313     C* Chiave su TITA430C - Completa
023203180313     C     KEYta430C     KLIST
023204180313     C                   KFLD                    vacAAS
023205180313     C                   KFLD                    vacLNP
023206180313     C                   KFLD                    vacNRS
023207180313     C                   KFLD                    vacNSP
023208180313     C                   KFLD                    ta4TRC
023300050622     C*
023400991027     C                   ENDSR
