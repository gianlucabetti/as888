000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400060410     FTIVGDTMP  UF   E             DISK
000500090323     Ftivawwwt  uF A E             DISK    commit
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000030325
001100060505     D wfdscds_p     e ds                  extname(wfdsc00f) prefix(p_)
001200060505     D wfdscds_s     e ds                  extname(wfdscsst) prefix(s_)
001300060505     D wfdscds       e ds                  extname(wfdscaat)
001400060505     D wPKF            s              9
001500060505     D wVLF            s              7
001600060505     D wCAS            s             15
001700060505     D wIFT            s             15
001800991027
001900991027     C                   EXSR      traduci
002000921023     C                   SETON                                        LR
002100060505
002200991027
002300991027     C     traduci       BEGSR
002400030325     C*
002500060410     C                   READ      TIVGDTMP
002600060410     C                   DOW       not %eof(TIVGDTMP)
002700060411     C*
002800060411     C* Valorizzo il buffer dal campo flat di inout
002900060505     C                   EVAL      wfdscds_p = vgdDTA
003000051007     C*
003100051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
003200060505     C                   EVALR     wPKF = %editw(p_DSCPKF:'    0 , ')
003300060505     C                   EVALR     wVLF = %editw(p_DSCVLF:'0 ,   ')
003400060505     C                   EVALR     wCAS = %editw(p_DSCCAS:'        0 ,   ')
003500060505     C                   EVALR     wIFT = %editw(p_DSCIFT:'        0 ,   ')
003600060505     C*
003700060505     C* Passo da formato misto numerico/alfanumerico a formato posizionale ASCII editato
003800060505     C                   EXSR      NumToAlfa
003900060411     C*
004000060505     C                   EVAL      VAWDTA = DSCAAS+
004100060505     C                                      DSCLNP+
004200060505     C                                      DSCNRS+
004300060505     C                                      DSCNSP+
004400060505     C                                      DSCFNS+
004500060505     C                                      DSCLNA+
004600060505     C                                      DSCZNC+
004700060505     C                                      DSCRSM+
004800060505     C                                      DSCINM+
004900060505     C                                      DSCCAM+
005000060505     C                                      DSCLOM+
005100060505     C                                      DSCPRM+
005200060505     C                                      DSCFAP+
005300060505     C                                      DSCNZM+
005400060505     C                                      DSCRSD+
005500060505     C                                      DSCIND+
005600060505     C                                      DSCCAD+
005700060505     C                                      DSCLOD+
005800060505     C                                      DSCPRD+
005900060505     C                                      DSCNZD+
006000060505     C                                      DSCFIN+
006100060505     C                                      DSCTSP+
006200060505     C                                      DSCFBR+
006300060505     C                                      DSCNCL+
006400060505     C                                      DSCPKF+
006500060505     C                                      DSCVLF+
006600060505     C                                      DSCRMN+
006700060505     C                                      DSCRMA+
006800060505     C                                      DSCTCR+
006900060505     C                                      DSCDCR+
007000060505     C                                      DSCHCR+
007100060505     C                                      DSCPDC+
007200060505     C                                      DSCFGS+
007300060505     C                                      DSCNDC+
007400060505     C                                      DSCDDC+
007500060505     C                                      DSCTC1+
007600060505     C                                      DSCTC2+
007700060505     C                                      DSCGGA+
007800060505     C                                      DSCGMA+
007900060505     C                                      DSCGVA+
008000060505     C                                      DSCNGD+
008100060505     C                                      DSCNSS+
008200060505     C                                      DSCTIC+
008300060505     C                                      DSCCAS+
008400060505     C                                      DSCVCA+
008500060505     C                                      DSCFIV+
008600060505     C                                      DSCNFT+
008700060505     C                                      DSCDFT+
008800060505     C                                      DSCIFT+
008900060505     C                                      DSCDIV+
009000060505     C                                      DSCFLS+
009100060505     C                                      DSCNCD+
009200060505     C                                      DSCNCA+
009300060505     C                                      DSCS01+
009400060505     C                                      DSCS02+
009500060505     C                                      DSCS03+
009600060505     C                                      DSCS04+
009700060505     C                                      DSCS05+
009800060505     C                                      DSCS06+
009900060505     C                                      DSCS07+
010000060505     C                                      DSCS08+
010100060505     C                                      DSCS09+
010200060505     C                                      DSCS10+
010300060505     C                                      DSCDGE+
010400060505     C                                      DSCHMC
010500930409     C*
010600060410     C                   WRITE     TIVAW000
010700060410     C                   DELETE    TIVGD000
010800991027     C*
010900060410     C                   READ      TIVGDTMP
011000030325     C                   ENDDO
011100991027     C*
011200030325     C                   EVAL      wrkesito = '0'
011300991027     C*
011400910830     C                   ENDSR
011500060505
011600060505
011700060505     C     NumToAlfa     BEGSR
011800060505     C*
011900060505     C* Da packed a signed
012000060505     C                   EVAL      s_DSCAAS = p_DSCAAS
012100060505     C                   EVAL      s_DSCLNP = p_DSCLNP
012200060505     C                   EVAL      s_DSCNRS = p_DSCNRS
012300060505     C                   EVAL      s_DSCNSP = p_DSCNSP
012400060505     C                   EVAL      s_DSCFNS = p_DSCFNS
012500060505     C                   EVAL      s_DSCLNA = p_DSCLNA
012600060505     C                   EVAL      s_DSCZNC = p_DSCZNC
012700060505     C                   EVAL      s_DSCRSM = p_DSCRSM
012800060505     C                   EVAL      s_DSCINM = p_DSCINM
012900060505     C                   EVAL      s_DSCCAM = p_DSCCAM
013000060505     C                   EVAL      s_DSCLOM = p_DSCLOM
013100060505     C                   EVAL      s_DSCPRM = p_DSCPRM
013200060505     C                   EVAL      s_DSCFAP = p_DSCFAP
013300060505     C                   EVAL      s_DSCNZM = p_DSCNZM
013400060505     C                   EVAL      s_DSCRSD = p_DSCRSD
013500060505     C                   EVAL      s_DSCIND = p_DSCIND
013600060505     C                   EVAL      s_DSCCAD = p_DSCCAD
013700060505     C                   EVAL      s_DSCLOD = p_DSCLOD
013800060505     C                   EVAL      s_DSCPRD = p_DSCPRD
013900060505     C                   EVAL      s_DSCNZD = p_DSCNZD
014000060505     C                   EVAL      s_DSCFIN = p_DSCFIN
014100060505     C                   EVAL      s_DSCTSP = p_DSCTSP
014200060505     C                   EVAL      s_DSCFBR = p_DSCFBR
014300060505     C                   EVAL      s_DSCNCL = p_DSCNCL
014400060505     C                   EVAL      s_DSCRMN = p_DSCRMN
014500060505     C                   EVAL      s_DSCRMA = p_DSCRMA
014600060505     C                   EVAL      s_DSCTCR = p_DSCTCR
014700060505     C                   EVAL      s_DSCDCR = p_DSCDCR
014800060505     C                   EVAL      s_DSCHCR = p_DSCHCR
014900060505     C                   EVAL      s_DSCPDC = p_DSCPDC
015000060505     C                   EVAL      s_DSCFGS = p_DSCFGS
015100060505     C                   EVAL      s_DSCNDC = p_DSCNDC
015200060505     C                   EVAL      s_DSCDDC = p_DSCDDC
015300060505     C                   EVAL      s_DSCTC1 = p_DSCTC1
015400060505     C                   EVAL      s_DSCTC2 = p_DSCTC2
015500060505     C                   EVAL      s_DSCGGA = p_DSCGGA
015600060505     C                   EVAL      s_DSCGMA = p_DSCGMA
015700060505     C                   EVAL      s_DSCGVA = p_DSCGVA
015800060505     C                   EVAL      s_DSCNGD = p_DSCNGD
015900060505     C                   EVAL      s_DSCNSS = p_DSCNSS
016000060505     C                   EVAL      s_DSCTIC = p_DSCTIC
016100060505     C                   EVAL      s_DSCVCA = p_DSCVCA
016200060505     C                   EVAL      s_DSCFIV = p_DSCFIV
016300060505     C                   EVAL      s_DSCNFT = p_DSCNFT
016400060505     C                   EVAL      s_DSCDFT = p_DSCDFT
016500060505     C                   EVAL      s_DSCDIV = p_DSCDIV
016600060505     C                   EVAL      s_DSCFLS = p_DSCFLS
016700060505     C                   EVAL      s_DSCNCD = p_DSCNCD
016800060505     C                   EVAL      s_DSCNCA = p_DSCNCA
016900060505     C                   EVAL      s_DSCS01 = p_DSCS01
017000060505     C                   EVAL      s_DSCS02 = p_DSCS02
017100060505     C                   EVAL      s_DSCS03 = p_DSCS03
017200060505     C                   EVAL      s_DSCS04 = p_DSCS04
017300060505     C                   EVAL      s_DSCS05 = p_DSCS05
017400060505     C                   EVAL      s_DSCS06 = p_DSCS06
017500060505     C                   EVAL      s_DSCS07 = p_DSCS07
017600060505     C                   EVAL      s_DSCS08 = p_DSCS08
017700060505     C                   EVAL      s_DSCS09 = p_DSCS09
017800060505     C                   EVAL      s_DSCS10 = p_DSCS10
017900060505     C                   EVAL      s_DSCDGE = p_DSCDGE
018000060505     C                   EVAL      s_DSCHMC = p_DSCHMC
018100060505     C*
018200060505     C* Da signed ad alfa
018300060505     C                   MOVE(P)   s_DSCAAS      DSCAAS
018400060505     C                   MOVE(P)   s_DSCLNP      DSCLNP
018500060505     C                   MOVE(P)   s_DSCNRS      DSCNRS
018600060505     C                   MOVE(P)   s_DSCNSP      DSCNSP
018700060505     C                   MOVE(P)   s_DSCFNS      DSCFNS
018800060505     C                   MOVE(P)   s_DSCLNA      DSCLNA
018900060505     C                   MOVE(P)   s_DSCZNC      DSCZNC
019000060505     C                   MOVE(P)   s_DSCRSM      DSCRSM
019100060505     C                   MOVE(P)   s_DSCINM      DSCINM
019200060505     C                   MOVE(P)   s_DSCCAM      DSCCAM
019300060505     C                   MOVE(P)   s_DSCLOM      DSCLOM
019400060505     C                   MOVE(P)   s_DSCPRM      DSCPRM
019500060505     C                   MOVE(P)   s_DSCFAP      DSCFAP
019600060505     C                   MOVE(P)   s_DSCNZM      DSCNZM
019700060505     C                   MOVE(P)   s_DSCRSD      DSCRSD
019800060505     C                   MOVE(P)   s_DSCIND      DSCIND
019900060505     C                   MOVE(P)   s_DSCCAD      DSCCAD
020000060505     C                   MOVE(P)   s_DSCLOD      DSCLOD
020100060505     C                   MOVE(P)   s_DSCPRD      DSCPRD
020200060505     C                   MOVE(P)   s_DSCNZD      DSCNZD
020300060505     C                   MOVE(P)   s_DSCFIN      DSCFIN
020400060505     C                   MOVE(P)   s_DSCTSP      DSCTSP
020500060505     C                   MOVE(P)   s_DSCFBR      DSCFBR
020600060505     C                   MOVE(P)   s_DSCNCL      DSCNCL
020700060505     C                   MOVE(P)   s_DSCRMN      DSCRMN
020800060505     C                   MOVE(P)   s_DSCRMA      DSCRMA
020900060505     C                   MOVE(P)   s_DSCTCR      DSCTCR
021000060505     C                   MOVE(P)   s_DSCDCR      DSCDCR
021100060505     C                   MOVE(P)   s_DSCHCR      DSCHCR
021200060505     C                   MOVE(P)   s_DSCPDC      DSCPDC
021300060505     C                   MOVE(P)   s_DSCFGS      DSCFGS
021400060505     C                   MOVE(P)   s_DSCNDC      DSCNDC
021500060505     C                   MOVE(P)   s_DSCDDC      DSCDDC
021600060505     C                   MOVE(P)   s_DSCTC1      DSCTC1
021700060505     C                   MOVE(P)   s_DSCTC2      DSCTC2
021800060505     C                   MOVE(P)   s_DSCGGA      DSCGGA
021900060505     C                   MOVE(P)   s_DSCGMA      DSCGMA
022000060505     C                   MOVE(P)   s_DSCGVA      DSCGVA
022100060505     C                   MOVE(P)   s_DSCNGD      DSCNGD
022200060505     C                   MOVE(P)   s_DSCNSS      DSCNSS
022300060505     C                   MOVE(P)   s_DSCTIC      DSCTIC
022400060505     C                   MOVE(P)   s_DSCVCA      DSCVCA
022500060505     C                   MOVE(P)   s_DSCFIV      DSCFIV
022600060505     C                   MOVE(P)   s_DSCNFT      DSCNFT
022700060505     C                   MOVE(P)   s_DSCDFT      DSCDFT
022800060505     C                   MOVE(P)   s_DSCDIV      DSCDIV
022900060505     C                   MOVE(P)   s_DSCFLS      DSCFLS
023000060505     C                   MOVE(P)   s_DSCNCD      DSCNCD
023100060505     C                   MOVE(P)   s_DSCNCA      DSCNCA
023200060505     C                   MOVE(P)   s_DSCS01      DSCS01
023300060505     C                   MOVE(P)   s_DSCS02      DSCS02
023400060505     C                   MOVE(P)   s_DSCS03      DSCS03
023500060505     C                   MOVE(P)   s_DSCS04      DSCS04
023600060505     C                   MOVE(P)   s_DSCS05      DSCS05
023700060505     C                   MOVE(P)   s_DSCS06      DSCS06
023800060505     C                   MOVE(P)   s_DSCS07      DSCS07
023900060505     C                   MOVE(P)   s_DSCS08      DSCS08
024000060505     C                   MOVE(P)   s_DSCS09      DSCS09
024100060505     C                   MOVE(P)   s_DSCS10      DSCS10
024200060505     C                   MOVE(P)   s_DSCDGE      DSCDGE
024300060505     C                   MOVE(P)   s_DSCHMC      DSCHMC
024400060505     C                   MOVE(P)   wPKF          DSCPKF
024500060505     C                   MOVE(P)   wVLF          DSCVLF
024600060505     C                   MOVE(P)   wCAS          DSCCAS
024700060505     C                   MOVE(P)   wIFT          DSCIFT
024800060531     C*
024900060531     C* Forzatura a *blanks dei byte "in +" nei numerici (alcuni clienti erroneamente gestivano
025000060531     C* come valore del campo anche tali byte)
025100060531     C                   EVAL      %subst(DSCAAS:1:2) = *blanks
025200060531     C                   EVAL      %subst(DSCLNP:1:1) = *blanks
025300060531     C                   EVAL      %subst(DSCNRS:1:2) = *blanks
025400060531     C                   EVAL      %subst(DSCNSP:1:1) = *blanks
025500060531     C                   EVAL      %subst(DSCLNA:1:1) = *blanks
025600060531     C                   EVAL      %subst(DSCZNC:1:2) = *blanks
025700060531     C                   EVAL      %subst(DSCNCL:1:1) = *blanks
025800060531     C                   EVAL      %subst(DSCPKF:1:1) = *blanks
025900060531     C                   EVAL      %subst(DSCVLF:1:1) = *blanks
026000060531     C                   EVAL      %subst(DSCRMN:1:1) = *blanks
026100060531     C                   EVAL      %subst(DSCDCR:1:2) = *blanks
026200060531     C                   EVAL      %subst(DSCHCR:1:2) = *blanks
026300060531     C                   EVAL      %subst(DSCPDC:1:1) = *blanks
026400060531     C                   EVAL      %subst(DSCFGS:1:1) = *blanks
026500060531     C                   EVAL      %subst(DSCNDC:1:1) = *blanks
026600060531     C                   EVAL      %subst(DSCDDC:1:2) = *blanks
026700060531     C                   EVAL      %subst(DSCNGD:1:1) = *blanks
026800060531     C                   EVAL      %subst(DSCNSS:1:1) = *blanks
026900060531     C                   EVAL      %subst(DSCCAS:1:1) = *blanks
027000060531     C                   EVAL      %subst(DSCFIV:1:1) = *blanks
027100060531     C                   EVAL      %subst(DSCNFT:1:2) = *blanks
027200060531     C                   EVAL      %subst(DSCDFT:1:2) = *blanks
027300060531     C                   EVAL      %subst(DSCIFT:1:1) = *blanks
027400060531     C                   EVAL      %subst(DSCFLS:1:1) = *blanks
027500060531     C                   EVAL      %subst(DSCNCD:1:1) = *blanks
027600060531     C                   EVAL      %subst(DSCNCA:1:1) = *blanks
027700060531     C                   EVAL      %subst(DSCS01:1:1) = *blanks
027800060531     C                   EVAL      %subst(DSCS02:1:1) = *blanks
027900060531     C                   EVAL      %subst(DSCS03:1:1) = *blanks
028000060531     C                   EVAL      %subst(DSCS04:1:1) = *blanks
028100060531     C                   EVAL      %subst(DSCS05:1:1) = *blanks
028200060531     C                   EVAL      %subst(DSCS06:1:1) = *blanks
028300060531     C                   EVAL      %subst(DSCS07:1:1) = *blanks
028400060531     C                   EVAL      %subst(DSCS08:1:1) = *blanks
028500060531     C                   EVAL      %subst(DSCS09:1:1) = *blanks
028600060531     C                   EVAL      %subst(DSCS10:1:1) = *blanks
028700060531     C                   EVAL      %subst(DSCDGE:1:2) = *blanks
028800060531     C                   EVAL      %subst(DSCHMC:1:2) = *blanks
028900060505     C*
029000060505     C                   ENDSR
029100060505
029200991027
029300991027      /TITLE Operazioni iniziali.
029400991027     C     *inzsr        BEGSR
029500991027     C*
029600991027     C     *ENTRY        PLIST
029700991027     C                   parm                    prmppt
029800991027     C     wrkesito      parm      wrkesito      prmesito
029900991027     C*
030000991027     C                   ENDSR
