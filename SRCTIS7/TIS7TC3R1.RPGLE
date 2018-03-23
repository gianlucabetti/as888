000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090326     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400080924     Ftivgdtmp  uF   E             DISK
000500050720     FFNBLT01L  IF   E           K DISK
000600030924     FTITAH30C  IF   E           K DISK
000700090326     FtivawWWT  O    E             DISK    commit
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200080924     D fnvacds       e ds                  extname(fnvac00t)
001300071115     D*
001400071115     D*
001500071115     D FNVAC1WT      E DS
001600071115     D*
001700071115     D FNVAC1WTDS      DS
001800071115     D  wFNVAC1WT                          like(FNVAC1WT) inz(*blanks)
001900071115     D  wNNC                         35    inz(*blanks)
002000071115     D  wNUL                          1    inz('.')
002100021007     D*-------------------
002200021007     D* COSTANTI
002300021007     D*-------------------
002400021007     D charOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
002500021007     D                                     abcdefghijklmnopqrstuvwxyz1234567-
002600021007     D                                     890,;.:-_òçà°ù§+*èé!"£$%&/()=?ì\|')
002700021007     D*-------------------
002800021007     D* SCHIERE DI WRK
002900021007     D*-------------------
003000021007     D SKcharOK        S              1    DIM(92)
003100021007
003200021007
003300021008     C                   MOVEA     charOK        SKcharOK
003400991027     C                   EXSR      traduci
003500921023     C                   SETON                                        LR
003600071115
003700071115
003800071115     C     traduci       BEGSR
003900071115     C*
004000080924     C                   READ      tivgdtmp
004100071115     C                   dow       not %eof
004200080924     c                   movel     vgddta        fnvacds
004300071115     C*
004400071115     C                   SETOFF                                       31
004500071115     C*
004600071115     C                   MOVE      VACAAS        VHCAAS
004700071115     C                   MOVE      VACLNP        VHCLNP
004800071115     C                   MOVE      VACNRS        VHCNRS
004900071115     C                   MOVE      VACNSP        VHCNSP
005000071115     C                   MOVE      VACMGS        VHCMGS
005100071115     C                   MOVE      VACCBO        VHCCBO
005200071115     C                   MOVE      VACLNA        VHCLNA
005300071115     C                   MOVE      VACRSD        VHCRSD
005400071115     C* Effettuo considerazioni sui caratteri della Ragione Sociale Destinatario
005500071115     C     1             DO        35            x                 2 0
005600071115     C                   MOVEL     *blanks       wrkCHAR           1
005700071115     C                   EVAL      wrkCHAR = %subst(VHCRSD:x:1)
005800071115     C                   IF        wrkCHAR <> *blanks AND
005900071115     C                             wrkCHAR <> ''''
006000071115     C     wrkCHAR       LOOKUP    SKcharOK                               20
006100071115     C                   IF        *in20 = *off
006200071115     C                   EVAL      %subst(VHCRSD:x:1) = *blanks
006300071115     C                   ENDIF
006400071115     C                   ENDIF
006500071115     C                   ENDDO
006600071115     C*
006700071115     C                   MOVE      VACPRD        VHCPRD
006800071115     C                   MOVE      VACGC1        VHCGC1
006900071115     C                   MOVE      VACGC2        VHCGC2
007000071115     C                   MOVE      VACCTR        VHCCTR
007100071115     C                   MOVE      VACCTS        VHCCTS
007200071115     C                   MOVE      VACFTM        VHCFTM
007300071115     C                   MOVE      VACFIN        VHCFIN
007400071115     C                   MOVE      VACFAP        VHCFAP
007500071115     C                   MOVE      VACTSP        VHCTSP
007600071115     C                   MOVE      VACIAS        VHCIAS
007700071115     C                   MOVE      VACVAS        VHCVAS
007800071115     C                   MOVE      VACNAS        VHCNAS
007900071115     C                   MOVE      VACNCL        VHCNCL
008000071115     C                   MOVE      VACPKB        VHCPKB
008100071115     C                   MOVE      VACVLB        VHCVLB
008200071115     C                   MOVE      VACQFT        VHCQFT
008300071115     C                   MOVE      VACTIC        VHCTIC
008400071115     C                   MOVE      VACCAS        VHCCAS
008500071115     C                   MOVE      VACVCA        VHCVCA
008600071115     C                   MOVE      VACCCM        VHCCCM
008700071115     C                   MOVE      VACRMN        VHCRMN
008800071115     C                   MOVE      VACRMA        VHCRMA
008900071115     C                   MOVE      VACRMO        VHCRMO
009000071115     C                   MOVE      VACFFD        VHCFFD
009100071115     C                   MOVE      VACTCR        VHCTCR
009200071115     C                   MOVE      VACDCR        VHCDCR
009300071115     C                   MOVE      VACHCR        VHCHCR
009400071115     C                   MOVE      VACDCM        VHCDCM
009500071115     C                   MOVE      VACHMC        VHCHMC
009600071115     C                   MOVE      VACTC1        VHCTC1
009700071115     C                   MOVE      VACTC2        VHCTC2
009800071115     C                   MOVE      VACCCA        VHCCCA
009900071115     C                   MOVE      VACDLA        VHCDLA
010000071115     C                   MOVE      VACDAG        VHCDAG
010100071115     C                   CLEAR                   wNNC
010200071115     C                   MOVEL     '.'           wNUL
010300071115     C*
010400071115     C* Quindi aggancio il dettaglio colli
010500071115     C                   EVAL      tahTRC = 'C'
010600071115     C     KEYtahC       SETLL     titah30c
010700071115     C                   IF        %equal(titah30c)
010800071115     C     KEYtahC       READE     titah30c
010900071115     C                   DOW       not %eof(titah30c)
011000071115     C                   EVAL      wNNC = tahNOT
011100071115     C*
011200071115     C* X ogni segnacollo d default imposto la data consegna merce presente in testata (VAC)
011300071115     C                   MOVE      VACDCM        VHCDCM
011400071115     C*
011500071115     C* Se riesco considero la data consegna merce d ogni singolo collo
011600071115     C                   EVAL      bltNSC = tahNSC
011700071115     C     KEYbltC       CHAIN     fnblt01l
011800071115     C                   IF        %found(fnblt01l)
011900071115     C                   IF        bltDCM > *zeros
012000071115     C                   MOVE      bltDCM        VHCDCM
012100071115     C                   ENDIF
012200071115     C                   ENDIF
012300071115     C*
012400071115     C                   MOVEL(P)  FNVAC1WT      wFNVAC1WT
012500071115     C                   EVAL      vawDTA = FNVAC1WTDS
012600071115     C*
012700071115     C                   WRITE     TIVAW000
012800071115     C     KEYtahC       READE     titah30c
012900071115     C                   ENDDO
013000071115     C                   ELSE
013100071115     C*
013200071115     C                   MOVEL(P)  FNVAC1WT      wFNVAC1WT
013300071115     C                   EVAL      vawDTA = FNVAC1WTDS
013400071115     C*
013500071115     C                   WRITE     TIVAW000
013600071115     C                   ENDIF
013700071115     C*
013800080924     C                   delete    tivgd000
013900071115     C*
014000080924     C                   READ      tivgdtmp
014100071115     C                   ENDdo
014200071115     C*
014300071115     C                   eval      wrkesito = '0'
014400071115     C*
014500071115     C                   ENDSR
014600991027
014700991027
014800991027      /TITLE Operazioni iniziali.
014900991027     C     *inzsr        BEGSR
015000991027     C*
015100991027     C     *ENTRY        PLIST
015200991027     C                   parm                    prmppt
015300991027     C     wrkesito      parm      wrkesito      prmesito
015400030924     C*
015500030924     C* Chiave su TITAH30C - completa
015600030924     C     KEYtahC       KLIST
015700030924     C                   KFLD                    vacAAS
015800030924     C                   KFLD                    vacLNP
015900030924     C                   KFLD                    vacNRS
016000030924     C                   KFLD                    vacNSP
016100030924     C                   KFLD                    tahTRC
016200050720     C*
016300050720     C* Chiave su FNBLT01L - completa
016400050720     C     KEYbltC       KLIST
016500050720     C                   KFLD                    vacAAS
016600050720     C                   KFLD                    vacLNP
016700050720     C                   KFLD                    vacNRS
016800050720     C                   KFLD                    vacNSP
016900050720     C                   KFLD                    bltNSC
017000991027     C*
017100991027     C                   ENDSR
