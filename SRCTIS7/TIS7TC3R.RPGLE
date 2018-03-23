000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500050720     FFNBLT01L  IF   E           K DISK
000600030924     FTITAH30C  IF   E           K DISK
000700090326     Ffnvacdwt  O    E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200080924     D fnvacds       e ds                  extname(fnvac00t)
001300941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001400941221     D* 10+3
001500941221     D                 DS
001600941221     D  WS15                   1      1
001700941221     D  WI15                   2     11
001800991125     D  WV15                  12     12    inz(',')
001900941221     D  WD15                  13     15
002000941221     D  WASC15                 1     15
002100941221     D*  6+1
002200941221     D                 DS
002300941221     D  WS09                   1      1
002400941221     D  WI09                   2      7
002500991125     D  WV09                   8      8    inz(',')
002600941221     D  WD09                   9      9
002700941221     D  WASC09                 1      9
002800941221     D*  2+3
002900941221     D                 DS
003000941221     D  WS07                   1      1
003100941221     D  WI07                   2      3
003200991125     D  WV07                   4      4    inz(',')
003300941221     D  WD07                   5      7
003400941221     D  WASC07                 1      7
003500021007     D*-------------------
003600021007     D* COSTANTI
003700021007     D*-------------------
003800021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
003900021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
004000021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
004100021007     D*-------------------
004200021007     D* SCHIERE DI WRK
004300021007     D*-------------------
004400021007     D SKcharOK        S              1    DIM(92)
004500021007
004600021007
004700021008     C                   MOVEA     charOK        SKcharOK
004800991027     C                   EXSR      traduci
004900921023     C                   SETON                                        LR
005000991027
005100991027     C     traduci       BEGSR
005200991027     C*
005300080924     C                   READ      tivgdtmp
005400991027     C                   dow       not %eof
005500080924     c                   movel     vgddta        fnvacds
005600991027     C*
005700941221     C                   MOVE      VACAAS        VHCAAS
005800941221     C                   MOVE      VACLNP        VHCLNP
005900941221     C                   MOVE      VACNRS        VHCNRS
006000941221     C                   MOVE      VACNSP        VHCNSP
006100941221     C                   MOVE      VACMGS        VHCMGS
006200941221     C                   MOVE      VACCBO        VHCCBO
006300941221     C                   MOVE      VACLNA        VHCLNA
006400021008     C                   MOVE      VACRSD        VHCRSD
006500021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
006600021007     C     1             DO        35            x                 2 0
006700021007     C                   MOVEL     *blanks       wrkCHAR           1
006800021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
006900021008     C                   IF        wrkCHAR <> *blanks AND
007000021008     C                             wrkCHAR <> ''''
007100021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
007200021007     C                   IF        *in20 = *off
007300021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
007400021007     C                   ENDIF
007500021007     C                   ENDIF
007600021007     C                   ENDDO
007700021007     C*
007800941221     C                   MOVE      VACPRD        VHCPRD
007900941221     C                   MOVE      VACGC1        VHCGC1
008000941221     C                   MOVE      VACGC2        VHCGC2
008100941221     C                   MOVE      VACCTR        VHCCTR
008200941221     C                   MOVE      VACCTS        VHCCTS
008300941221     C                   MOVE      VACFTM        VHCFTM
008400941221     C                   MOVE      VACFIN        VHCFIN
008500941221     C                   MOVE      VACFAP        VHCFAP
008600941221     C                   MOVE      VACTSP        VHCTSP
008700941221     C*
008800941221     C                   MOVE      VACIAS        WD15
008900941221     C                   MOVEL     VACIAS        WI15
009000941221     C                   MOVE      WASC15        VHCIAS
009100941221     C*
009200941221     C                   MOVE      VACVAS        VHCVAS
009300941221     C                   MOVE      VACNAS        VHCNAS
009400941221     C                   MOVE      VACNCL        VHCNCL
009500941221     C*
009600941221     C                   MOVE      VACPKB        WD09
009700941221     C                   MOVEL     VACPKB        WI09
009800941221     C                   MOVE      WASC09        VHCPKB
009900941221     C*
010000941221     C                   MOVE      VACVLB        WD07
010100941221     C                   MOVEL     VACVLB        WI07
010200941221     C                   MOVE      WASC07        VHCVLB
010300941221     C*
010400941221     C                   MOVE      VACQFT        WD15
010500941221     C                   MOVEL     VACQFT        WI15
010600941221     C                   MOVE      WASC15        VHCQFT
010700941221     C*
010800941221     C                   MOVE      VACTIC        VHCTIC
010900941221     C*
011000941221     C                   MOVE      VACCAS        WD15
011100941221     C                   MOVEL     VACCAS        WI15
011200941221     C                   MOVE      WASC15        VHCCAS
011300941221     C*
011400941221     C                   MOVE      VACVCA        VHCVCA
011500941221     C                   MOVE      VACCCM        VHCCCM
011600941221     C                   MOVE      VACRMN        VHCRMN
011700941221     C                   MOVE      VACRMA        VHCRMA
011800941221     C                   MOVE      VACRMO        VHCRMO
011900941221     C                   MOVE      VACFFD        VHCFFD
012000941221     C                   MOVE      VACTCR        VHCTCR
012100941221     C                   MOVE      VACDCR        VHCDCR
012200941221     C                   MOVE      VACHCR        VHCHCR
012300941221     C                   MOVE      VACDCM        VHCDCM
012400941221     C                   MOVE      VACHMC        VHCHMC
012500941221     C                   MOVE      VACTC1        VHCTC1
012600941221     C                   MOVE      VACTC2        VHCTC2
012700941221     C                   MOVE      VACCCA        VHCCCA
012800941221     C                   MOVE      VACDLA        VHCDLA
012900941221     C                   MOVE      VACDAG        VHCDAG
013000030925     C                   MOVEL     '.'           VHCNUL
013100030924     C*
013200030924     C* Quindi aggancio il dettaglio colli
013300030924     C                   EVAL      tahTRC = 'C'
013400030924     C     KEYtahC       SETLL     titah30c
013500030924     C                   IF        %equal(titah30c)
013600030924     C     KEYtahC       READE     titah30c
013700030924     C                   DOW       not %eof(titah30c)
013800030924     C                   EVAL      VHCNNC = tahNOT
013900050801     C*
014000050801     C* X ogni segnacollo d default imposto la data consegna merce presente in testata (VAC)
014100050801     C                   MOVE      VACDCM        VHCDCM
014200050720     C*
014300050720     C* Se riesco considero la data consegna merce d ogni singolo collo
014400050720     C                   EVAL      bltNSC = tahNSC
014500050720     C     KEYbltC       CHAIN     fnblt01l
014600050720     C                   IF        %found(fnblt01l)
014700050801     C                   IF        bltDCM > *zeros
014800050720     C                   MOVE      bltDCM        VHCDCM
014900050801     C                   ENDIF
015000050720     C                   ENDIF
015100050720     C*
015200030924     C                   WRITE     FNVACAA
015300030924     C     KEYtahC       READE     titah30c
015400030924     C                   ENDDO
015500030924     C                   ELSE
015600030924     C                   WRITE     FNVACAA
015700030924     C                   ENDIF
015800930409     C*
015900080924     C                   delete    tivgd000
016000991027     C*
016100080924     C                   READ      tivgdtmp
016200991027     C                   ENDdo
016300991027     C*
016400991027     C                   eval      wrkesito = '0'
016500991027     C*
016600910830     C                   ENDSR
016700991027
016800991027      /TITLE Operazioni iniziali.
016900991027     C     *inzsr        BEGSR
017000991027     C*
017100991027     C     *ENTRY        PLIST
017200991027     C                   parm                    prmppt
017300991027     C     wrkesito      parm      wrkesito      prmesito
017400030924     C*
017500030924     C* Chiave su TITAH30C - completa
017600030924     C     KEYtahC       KLIST
017700030924     C                   KFLD                    vacAAS
017800030924     C                   KFLD                    vacLNP
017900030924     C                   KFLD                    vacNRS
018000030924     C                   KFLD                    vacNSP
018100030924     C                   KFLD                    tahTRC
018200050720     C*
018300050720     C* Chiave su FNBLT01L - completa
018400050720     C     KEYbltC       KLIST
018500050720     C                   KFLD                    vacAAS
018600050720     C                   KFLD                    vacLNP
018700050720     C                   KFLD                    vacNRS
018800050720     C                   KFLD                    vacNSP
018900050720     C                   KFLD                    bltNSC
019000991027     C*
019100991027     C                   ENDSR
