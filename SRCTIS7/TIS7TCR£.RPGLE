000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500090326     Ffnvacwwt  O    E             DISK    commit
000600050714     FFNLBL01L  IF   E           K DISK
000700050622     FFNLBL02L  IF   E           K DISK    rename(fnlbl000:fnlbl002)
000800050622     FTITAS30C  IF   E           K DISK
000900991027
001000991027     D prmppt          s             50
001100991027     D prmesito        s              1
001200991027     D wrkesito        s                   like(prmesito)
001300080924     D fnvacds       e ds                  extname(fnvac00t)
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
014100050622     C*
014200050622     C* Se richiesto nei parametri traduzione effettuo le seguenti considerazioni:
014300050622     C* - no bolle figlie
014400050622     C* - no bolle mamme se CCA=1 e bolle figlie legate NN CONS. o NN RESI
014500060619     C* - no bolle mamme se CCA=2 e bolle figlie legate NN CONS. o NN RESI
014600060619     C                   MOVEL     *blanks       wTip              1
014700060619     C                   EVAL      wTip = %subst(prmppt:1:1)
014800060619     C                   IF        wTip <> *blanks
014900060619     C* Se CCA=2 e richiesta forzatura relativa nei parametri azzero la data consegna merce
015000060619     C                   IF        VHCCCA='2' and (wTip='2' or wTip='3')
015100060619     C                   MOVE      *zeros        VHCDCM
015200060619     C                   ENDIF
015300050622     C* Verifico se trattasi d bolla "figlia"
015400050622     C     KEYlbl01C     CHAIN     FNLBL01L
015500050622     C                   IF        %found(FNLBL01L)
015600050622     C                   SETON                                        31
015700050622     C                   ELSE
015800060619     C* Se CCA=1 o CCA=2 => verifico stato attuale dell'ultima bolla figlia legata
015900060619     C                   IF        (VACCCA='1' and wTip='1') or
016000060619     C                             (VACCCA='2' and wTip='2') or
016100060619     C                             ((VACCCA='1' or VACCCA='2') and wTip='3')
016200050622     C     KEYlbl01C     CHAIN     FNLBL02L
016300050622     C                   IF        %found(FNLBL02L)
016400050714     C     KEYlbl02P     SETLL     FNLBL02L
016500050714     C                   IF        %equal(FNLBL02L)
016600050714     C     KEYlbl02P     READE     FNLBL02L
016700050714     C                   DOW       not %eof(FNLBL02L)
016800050714     C     KEYlbl02P     READE     FNLBL02L
016900050622     C                   ENDDO
017000050714     C                   ENDIF
017100050622     C* A questo punto con la chiave dell'ultima bolla figlia letta verifico il suo stato attuale
017200050622     C     KEYlbl02P     CHAIN     TITAS30C
017300050622     C                   IF        %found(TITAS30C)
017400140418     C*
017500140418     C                   if        %subst(prmppt:8:1) = 'S'                     *KSC in CCM se 8888
017600140418     C                   move      VACCCM        w8888             4
017700140418     C                   if        w8888 = '8888' or w8888 = '9999'
017800140418     C                   MOVE      tasKSC        VHCCCM
017900140418     C                   endif
018000140418     C                   endif
018100140418     C*
018200050622     C                   IF        tasDCM > *zeros AND
018300050622     C                             (tasCCA = *blanks OR tasCCA = '2')
018400050623     C* Se ultima bolla figlia chiusa con consegna OK o reso => forzo lo stato d quest'ultima
018500050623     C* sulla bolla originale
018600050623     C                   EVAL      VHCCCA = tasCCA
018700050622     C                   ELSE
018800050622     C                   SETON                                        31
018900050622     C                   ENDIF
019000050622     C                   ENDIF
019100050622     C                   ENDIF
019200050622     C                   ENDIF
019300050622     C                   ENDIF
019400050622     C                   ENDIF
019500050622     C*
019600941221     C                   MOVE      VACDLA        VHCDLA
019700941221     C                   MOVE      VACDAG        VHCDAG
019800930409     C*
019900050622     C  N31              WRITE     FNVACAA
020000080613     C                   endif
020100080613     C*
020200080924     c                   delete    tivgd000
020300991027     C*
020400080924     C                   READ      tivgdtmp
020500991027     C                   ENDdo
020600991027     C*
020700991027     C                   eval      wrkesito = '0'
020800991027     C*
020900910830     C                   ENDSR
021000991027
021100991027      /TITLE Operazioni iniziali.
021200991027     C     *inzsr        BEGSR
021300991027     C*
021400991027     C     *ENTRY        PLIST
021500991027     C                   parm                    prmppt
021600991027     C     wrkesito      parm      wrkesito      prmesito
021700050622     C*
021800050622     C* Chiave su FNLBL01L - completa
021900050622     C     KEYlbl01C     KLIST
022000050622     C                   KFLD                    vacAAS
022100050622     C                   KFLD                    vacLNP
022200050622     C                   KFLD                    vacNRS
022300050622     C                   KFLD                    vacNSP
022400050622     C*
022500050622     C     KEYlbl02P     KLIST
022600050622     C                   KFLD                    lblAAN
022700050622     C                   KFLD                    lblLPN
022800050622     C                   KFLD                    lblNRN
022900050622     C                   KFLD                    lblNSN
023000050622     C*
023100991027     C                   ENDSR
