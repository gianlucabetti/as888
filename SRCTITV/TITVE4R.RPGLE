000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400080924     FTIVGDTMP  uF   E             DISK
000500991027     Ffnvacwwt  O    E             DISK
000600991027
000700080924     D fnvac00t      e ds
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
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
004300021007
004400021007
004500021008     C                   MOVEA     charOK        SKcharOK
004600991027     C                   EXSR      traduci
004700921023     C                   SETON                                        LR
004800991027
004900991027     C     traduci       BEGSR
005000991027     C*
005100080924     C                   READ      TIVGDTMP
005200080924     C                   dow       not %eof(TIVGDTMP)
005300080924     C*
005400080924     C                   EVAL      fnvac00t = vgdDTA
005500991027     C*
005600040805     C* NN restituisco al cliente i codici anomalia '9'
005700040805     C                   IF        VACCCA <> '9'
005800941221     C                   MOVE      VACAAS        VHCAAS
005900941221     C                   MOVE      VACLNP        VHCLNP
006000941221     C                   MOVE      VACNRS        VHCNRS
006100941221     C                   MOVE      VACNSP        VHCNSP
006200941221     C                   MOVE      VACMGS        VHCMGS
006300941221     C                   MOVE      VACCBO        VHCCBO
006400941221     C                   MOVE      VACLNA        VHCLNA
006500021008     C                   MOVE      VACRSD        VHCRSD
006600021007     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
006700021007     C     1             DO        35            x                 2 0
006800021007     C                   MOVEL     *blanks       wrkCHAR           1
006900021008     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
007000021008     C                   IF        wrkCHAR <> *blanks AND
007100021008     C                             wrkCHAR <> ''''
007200021007     C     wrkCHAR       LOOKUP    SKcharOK                               20
007300021007     C                   IF        *in20 = *off
007400021008     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
007500021007     C                   ENDIF
007600021007     C                   ENDIF
007700021007     C                   ENDDO
007800021007     C*
007900941221     C                   MOVE      VACPRD        VHCPRD
008000941221     C                   MOVE      VACGC1        VHCGC1
008100941221     C                   MOVE      VACGC2        VHCGC2
008200941221     C                   MOVE      VACCTR        VHCCTR
008300941221     C                   MOVE      VACCTS        VHCCTS
008400941221     C                   MOVE      VACFTM        VHCFTM
008500941221     C                   MOVE      VACFIN        VHCFIN
008600941221     C                   MOVE      VACFAP        VHCFAP
008700941221     C                   MOVE      VACTSP        VHCTSP
008800941221     C*
008900941221     C                   MOVE      VACIAS        WD15
009000941221     C                   MOVEL     VACIAS        WI15
009100941221     C                   MOVE      WASC15        VHCIAS
009200941221     C*
009300941221     C                   MOVE      VACVAS        VHCVAS
009400941221     C                   MOVE      VACNAS        VHCNAS
009500941221     C                   MOVE      VACNCL        VHCNCL
009600941221     C*
009700941221     C                   MOVE      VACPKB        WD09
009800941221     C                   MOVEL     VACPKB        WI09
009900941221     C                   MOVE      WASC09        VHCPKB
010000941221     C*
010100941221     C                   MOVE      VACVLB        WD07
010200941221     C                   MOVEL     VACVLB        WI07
010300941221     C                   MOVE      WASC07        VHCVLB
010400941221     C*
010500941221     C                   MOVE      VACQFT        WD15
010600941221     C                   MOVEL     VACQFT        WI15
010700941221     C                   MOVE      WASC15        VHCQFT
010800941221     C*
010900941221     C                   MOVE      VACTIC        VHCTIC
011000941221     C*
011100941221     C                   MOVE      VACCAS        WD15
011200941221     C                   MOVEL     VACCAS        WI15
011300941221     C                   MOVE      WASC15        VHCCAS
011400941221     C*
011500941221     C                   MOVE      VACVCA        VHCVCA
011600941221     C                   MOVE      VACCCM        VHCCCM
011700941221     C                   MOVE      VACRMN        VHCRMN
011800941221     C                   MOVE      VACRMA        VHCRMA
011900941221     C                   MOVE      VACRMO        VHCRMO
012000941221     C                   MOVE      VACFFD        VHCFFD
012100941221     C                   MOVE      VACTCR        VHCTCR
012200941221     C                   MOVE      VACDCR        VHCDCR
012300941221     C                   MOVE      VACHCR        VHCHCR
012400941221     C                   MOVE      VACDCM        VHCDCM
012500941221     C                   MOVE      VACHMC        VHCHMC
012600941221     C                   MOVE      VACTC1        VHCTC1
012700941221     C                   MOVE      VACTC2        VHCTC2
012800941221     C                   MOVE      VACCCA        VHCCCA
012900941221     C                   MOVE      VACDLA        VHCDLA
013000941221     C                   MOVE      VACDAG        VHCDAG
013100930409     C*
013200040805     C* Se data consegna merce = *zeros e codice consegna anomala = *blanks imposto il valore
013300040805     C* '<' al codice consegna anomala (questo xchè il cliente NN riesce a distinguere il
013400040805     C* *blanks x le consegne OK da quelle NN OK).
013500040805     C                   IF        VACDCM = *zeros AND
013600040805     C                             VACCCA = *blanks
013700040805     C                   EVAL      VACCCA = '<'
013800040805     C                   MOVE      VACCCA        VHCCCA
013900040805     C                   ENDIF
014000040805     C*
014100941221     C                   WRITE     FNVACAA
014200040805     C                   ENDIF
014300040805     C*
014400080924     C                   delete    TIVGD000
014500991027     C*
014600080924     C                   READ      TIVGDTMP
014700991027     C                   ENDdo
014800991027     C*
014900991027     C                   eval      wrkesito = '0'
015000991027     C*
015100910830     C                   ENDSR
015200991027
015300991027      /TITLE Operazioni iniziali.
015400991027     C     *inzsr        BEGSR
015500991027     C*
015600991027     C     *ENTRY        PLIST
015700991027     C                   parm                    prmppt
015800991027     C     wrkesito      parm      wrkesito      prmesito
015900991027     C*
016000991027     C                   ENDSR
