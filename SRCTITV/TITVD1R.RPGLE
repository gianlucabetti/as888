000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400090320     Ftivgdtmp  uF   E             DISK
000500090320     FFNVACWWT  O    E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000090320     D fnvabds       e ds                  extname(fnvab00t)
001100040706     D FNVAC00T      E DS
001200991027
001300040706     D*-------------------
001400040706     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001500040706     D*-------------------
001600040706     D* 10+3
001700040706     D                 DS
001800040706     D  WS15                   1      1
001900040706     D  WI15                   2     11
002000040706     D  WV15                  12     12    inz(',')
002100040706     D  WD15                  13     15
002200040706     D  WASC15                 1     15
002300040706     D*  6+1
002400040706     D                 DS
002500040706     D  WS09                   1      1
002600040706     D  WI09                   2      7
002700040706     D  WV09                   8      8    inz(',')
002800040706     D  WD09                   9      9
002900040706     D  WASC09                 1      9
003000040706     D*  2+3
003100040706     D                 DS
003200040706     D  WS07                   1      1
003300040706     D  WI07                   2      3
003400040706     D  WV07                   4      4    inz(',')
003500040706     D  WD07                   5      7
003600040706     D  WASC07                 1      7
003700040706     D*-------------------
003800040706     D* COSTANTI
003900040706     D*-------------------
004000040706     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
004100040706     D                                     abcdefghijklmnopqrstuvwxyz1234567-
004200040706     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
004300040706     D*-------------------
004400040706     D* SCHIERE DI WRK
004500040706     D*-------------------
004600040706     D SKcharOK        S              1    DIM(92)
004700040706
004800040706
004900040706
005000040706     C                   MOVEA     charOK        SKcharOK
005100991027     C                   EXSR      traduci
005200921023     C                   SETON                                        LR
005300040706
005400040706
005500991027
005600991027     C     traduci       BEGSR
005700040706     C*
005800040706     C* Leggo tutto il file d input (FNVAB)
005900090320     C     *START        SETLL     tivgdtmp
006000090320     C                   READ      tivgdtmp
006100090320     C                   DOW       not %eof(tivgdtmp)
006200090320     c                   movel     vgddta        fnvabds
006300040706     C*
006400040706     C* Inizializzo il buffer del file d output (FNVAC)
006500040706     C                   CLEAR                   FNVACAA
006600040706     C*
006700040706     C* Inizializzo la DS del formato VAC standard x valorizzare in modo sterile anche i campi del
006800040706     C* VAC mancanti nel VAB
006900040706     C                   CLEAR                   FNVAC00T
007000040706     C                   MOVE      VACCCA        VHCCCA
007100040706     C                   MOVE      VACDAG        VHCDAG
007200040706     C                   MOVE      VACDCM        VHCDCM
007300040706     C                   MOVE      VACHMC        VHCHMC
007400040706     C                   MOVE      VACDLA        VHCDLA
007500040706     C                   MOVE      VACFIN        VHCFIN
007600040706     C                   MOVE      VACFAP        VHCFAP
007700040706     C*
007800040706     C* Valorizzo il buffer del file d output (FNVAC)
007900040706     C                   MOVE      VABAAS        VHCAAS
008000040706     C                   MOVE      VABLNP        VHCLNP
008100040706     C                   MOVE      VABNRS        VHCNRS
008200040706     C                   MOVE      VABNSP        VHCNSP
008300040706     C                   MOVE      VABMGS        VHCMGS
008400040706     C                   MOVE      VABCBO        VHCCBO
008500040706     C                   MOVE      VABLNA        VHCLNA
008600040706     C                   MOVE      VABRSD        VHCRSD
008700040706     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
008800040706     C     1             DO        35            x                 2 0
008900040706     C                   MOVEL     *blanks       wrkCHAR           1
009000040706     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
009100040706     C                   IF        wrkCHAR <> *blanks AND
009200040706     C                             wrkCHAR <> ''''
009300040706     C     wrkCHAR       LOOKUP    SKcharOK                               20
009400040706     C                   IF        *in20 = *off
009500040706     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
009600040706     C                   ENDIF
009700040706     C                   ENDIF
009800040706     C                   ENDDO
009900040706     C*
010000040706     C                   MOVE      VABPRD        VHCPRD
010100040706     C                   MOVE      VABGC1        VHCGC1
010200040706     C                   MOVE      VABGC2        VHCGC2
010300040706     C                   MOVE      VABCTR        VHCCTR
010400040706     C                   MOVE      VABCTS        VHCCTS
010500040706     C                   MOVE      VABFTM        VHCFTM
010600040706     C                   MOVE      VABTSP        VHCTSP
010700040706     C*
010800040706     C                   MOVE      VABIAS        WD15
010900040706     C                   MOVEL     VABIAS        WI15
011000040706     C                   MOVE      WASC15        VHCIAS
011100040706     C*
011200040706     C                   MOVE      VABVAS        VHCVAS
011300040706     C                   MOVE      VABNAS        VHCNAS
011400040706     C                   MOVE      VABNCL        VHCNCL
011500040706     C*
011600040706     C                   MOVE      VABPKB        WD09
011700040706     C                   MOVEL     VABPKB        WI09
011800040706     C                   MOVE      WASC09        VHCPKB
011900040706     C*
012000040706     C                   MOVE      VABVLB        WD07
012100040706     C                   MOVEL     VABVLB        WI07
012200040706     C                   MOVE      WASC07        VHCVLB
012300040706     C*
012400040706     C                   MOVE      VABQFT        WD15
012500040706     C                   MOVEL     VABQFT        WI15
012600040706     C                   MOVE      WASC15        VHCQFT
012700040706     C*
012800040706     C                   MOVE      VABTIC        VHCTIC
012900040706     C*
013000040706     C                   MOVE      VABCAS        WD15
013100040706     C                   MOVEL     VABCAS        WI15
013200040706     C                   MOVE      WASC15        VHCCAS
013300040706     C*
013400040706     C                   MOVE      VABVCA        VHCVCA
013500040706     C                   MOVE      VABCCM        VHCCCM
013600040706     C                   MOVE      VABRMN        VHCRMN
013700040706     C                   MOVE      VABRMA        VHCRMA
013800040706     C                   MOVE      VABRMO        VHCRMO
013900040706     C                   MOVE      VABFFD        VHCFFD
014000040706     C                   MOVE      VABTCR        VHCTCR
014100040706     C                   MOVE      VABDCR        VHCDCR
014200040706     C                   MOVE      VABHCR        VHCHCR
014300040706     C                   MOVE      VABTC1        VHCTC1
014400040706     C                   MOVE      VABTC2        VHCTC2
014500040706     C*
014600040706     C* Forzatura x cliente => codice consegna anomala = '9'
014700040706     C                   MOVEL     '9'           VHCCCA
014800040706     C*
014900040706     C* Scarico il buffer del file d output (FNVAC)
015000040706     C                   WRITE     FNVACAA
015100090320     C                   delete    tivgd000
015200040706     C*
015300040706     C* Quindi proseguo con la lettura del file d input (FNVAB)
015400090320     C                   READ      tivgdtmp
015500040706     C                   ENDDO
015600991027     C*
015700030325     C                   EVAL      wrkesito = '0'
015800991027     C*
015900910830     C                   ENDSR
016000040706
016100040706
016200040706
016300991027
016400991027      /TITLE Operazioni iniziali.
016500991027     C     *inzsr        BEGSR
016600991027     C*
016700991027     C     *ENTRY        PLIST
016800991027     C                   parm                    prmppt
016900991027     C     wrkesito      parm      wrkesito      prmesito
017000991027     C*
017100991027     C                   ENDSR
