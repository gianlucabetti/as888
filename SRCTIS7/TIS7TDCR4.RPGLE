000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400070903     Ftitas30c  if   e           k disk
000500060410     FTIVGDTMP  UF   E             DISK
000600090323     Ftivawwwt  uF A E             DISK    commit
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100030325
001200060505     D wfdscds_p     e ds                  extname(wfdsc00f) prefix(p_)
001300060505     D wfdscds_s     e ds                  extname(wfdscsst) prefix(s_)
001400060505     D wfdscds       e ds                  extname(wfdscaat)
001500060505     D wPKF            s              9
001600060505     D wVLF            s              7
001700060505     D wCAS            s             15
001800060505     D wIFT            s             15
001900070903     D*
002000070903     D wDDT            s              1    inz(*blanks)
002100070903     D*------------
002200991027
002300991027     C                   EXSR      traduci
002400921023     C                   SETON                                        LR
002500070903
002600070903
002700070903
002800070903     C     rtvDDT        BEGSR
002900070903     C*
003000070903     C                   movel     *blanks       wDDT
003100070903     C*
003200070903     C     KeyTAS30C     chain     titas30c
003300070903     C                   if        %found(titas30c)
003400070903     C*
003500070903     C* I flag che indicano il DDT sono i seguenti:
003600070903     C                   if        tasLL1 = 'S' or
003700070903     C                             tasLL1 = 'P' or
003800070903     C                             tasLL1 = 'C' or
003900070903     C                             tasLL1 = 'Q'
004000070903     C                   eval      wDDT = 'S'
004100070903     C                   endif
004200070903     C                   endif
004300070903     C*
004400070903     C                   ENDSR
004500060505
004600070903
004700070903
004800991027
004900991027     C     traduci       BEGSR
005000030325     C*
005100060410     C                   READ      TIVGDTMP
005200060410     C                   DOW       not %eof(TIVGDTMP)
005300060411     C*
005400060411     C* Valorizzo il buffer dal campo flat di inout
005500060505     C                   EVAL      wfdscds_p = vgdDTA
005600070903     C*
005700070903     C* Reperisco se trattasi d bolla con DDT SI
005800070903     C                   EXSR      rtvDDT
005900051007     C*
006000051007     C* A seconda del separatore decimale richiesto => gestisco maschere editazione differenti
006100060505     C                   EVALR     wPKF = %editw(p_DSCPKF:'    0 , ')
006200060505     C                   EVALR     wVLF = %editw(p_DSCVLF:'0 ,   ')
006300060505     C                   EVALR     wCAS = %editw(p_DSCCAS:'        0 ,   ')
006400060505     C                   EVALR     wIFT = %editw(p_DSCIFT:'        0 ,   ')
006500060505     C*
006600060505     C* Passo da formato misto numerico/alfanumerico a formato posizionale ASCII editato
006700060505     C                   EXSR      NumToAlfa
006800060411     C*
006900060505     C                   EVAL      VAWDTA = DSCAAS+
007000060505     C                                      DSCLNP+
007100060505     C                                      DSCNRS+
007200060505     C                                      DSCNSP+
007300060505     C                                      DSCFNS+
007400060505     C                                      DSCLNA+
007500060505     C                                      DSCZNC+
007600060505     C                                      DSCRSM+
007700060505     C                                      DSCINM+
007800060505     C                                      DSCCAM+
007900060505     C                                      DSCLOM+
008000060505     C                                      DSCPRM+
008100060505     C                                      DSCFAP+
008200060505     C                                      DSCNZM+
008300060505     C                                      DSCRSD+
008400060505     C                                      DSCIND+
008500060505     C                                      DSCCAD+
008600060505     C                                      DSCLOD+
008700060505     C                                      DSCPRD+
008800060505     C                                      DSCNZD+
008900060505     C                                      DSCFIN+
009000060505     C                                      DSCTSP+
009100060505     C                                      DSCFBR+
009200060505     C                                      DSCNCL+
009300060505     C                                      DSCPKF+
009400060505     C                                      DSCVLF+
009500060505     C                                      DSCRMN+
009600060505     C                                      DSCRMA+
009700060505     C                                      DSCTCR+
009800060505     C                                      DSCDCR+
009900060505     C                                      DSCHCR+
010000060505     C                                      DSCPDC+
010100060505     C                                      DSCFGS+
010200060505     C                                      DSCNDC+
010300060505     C                                      DSCDDC+
010400060505     C                                      DSCTC1+
010500060505     C                                      DSCTC2+
010600060505     C                                      DSCGGA+
010700060505     C                                      DSCGMA+
010800060505     C                                      DSCGVA+
010900060505     C                                      DSCNGD+
011000060505     C                                      DSCNSS+
011100060505     C                                      DSCTIC+
011200060505     C                                      DSCCAS+
011300060505     C                                      DSCVCA+
011400060505     C                                      DSCFIV+
011500060505     C                                      DSCNFT+
011600060505     C                                      DSCDFT+
011700060505     C                                      DSCIFT+
011800060505     C                                      DSCDIV+
011900060505     C                                      DSCFLS+
012000060505     C                                      DSCNCD+
012100060505     C                                      DSCNCA+
012200060505     C                                      DSCS01+
012300060505     C                                      DSCS02+
012400060505     C                                      DSCS03+
012500060505     C                                      DSCS04+
012600060505     C                                      DSCS05+
012700060505     C                                      DSCS06+
012800060505     C                                      DSCS07+
012900060505     C                                      DSCS08+
013000060505     C                                      DSCS09+
013100060505     C                                      DSCS10+
013200060505     C                                      DSCDGE+
013300070903     C                                      DSCHMC+
013400070903     C                                      wDDT
013500930409     C*
013600060410     C                   WRITE     TIVAW000
013700060410     C                   DELETE    TIVGD000
013800991027     C*
013900060410     C                   READ      TIVGDTMP
014000030325     C                   ENDDO
014100991027     C*
014200030325     C                   EVAL      wrkesito = '0'
014300991027     C*
014400910830     C                   ENDSR
014500060505
014600060505
014700060505     C     NumToAlfa     BEGSR
014800060505     C*
014900060505     C* Da packed a signed
015000060505     C                   EVAL      s_DSCAAS = p_DSCAAS
015100060505     C                   EVAL      s_DSCLNP = p_DSCLNP
015200060505     C                   EVAL      s_DSCNRS = p_DSCNRS
015300060505     C                   EVAL      s_DSCNSP = p_DSCNSP
015400060505     C                   EVAL      s_DSCFNS = p_DSCFNS
015500060505     C                   EVAL      s_DSCLNA = p_DSCLNA
015600060505     C                   EVAL      s_DSCZNC = p_DSCZNC
015700060505     C                   EVAL      s_DSCRSM = p_DSCRSM
015800060505     C                   EVAL      s_DSCINM = p_DSCINM
015900060505     C                   EVAL      s_DSCCAM = p_DSCCAM
016000060505     C                   EVAL      s_DSCLOM = p_DSCLOM
016100060505     C                   EVAL      s_DSCPRM = p_DSCPRM
016200060505     C                   EVAL      s_DSCFAP = p_DSCFAP
016300060505     C                   EVAL      s_DSCNZM = p_DSCNZM
016400060505     C                   EVAL      s_DSCRSD = p_DSCRSD
016500060505     C                   EVAL      s_DSCIND = p_DSCIND
016600060505     C                   EVAL      s_DSCCAD = p_DSCCAD
016700060505     C                   EVAL      s_DSCLOD = p_DSCLOD
016800060505     C                   EVAL      s_DSCPRD = p_DSCPRD
016900060505     C                   EVAL      s_DSCNZD = p_DSCNZD
017000060505     C                   EVAL      s_DSCFIN = p_DSCFIN
017100060505     C                   EVAL      s_DSCTSP = p_DSCTSP
017200060505     C                   EVAL      s_DSCFBR = p_DSCFBR
017300060505     C                   EVAL      s_DSCNCL = p_DSCNCL
017400060505     C                   EVAL      s_DSCRMN = p_DSCRMN
017500060505     C                   EVAL      s_DSCRMA = p_DSCRMA
017600060505     C                   EVAL      s_DSCTCR = p_DSCTCR
017700060505     C                   EVAL      s_DSCDCR = p_DSCDCR
017800060505     C                   EVAL      s_DSCHCR = p_DSCHCR
017900060505     C                   EVAL      s_DSCPDC = p_DSCPDC
018000060505     C                   EVAL      s_DSCFGS = p_DSCFGS
018100060505     C                   EVAL      s_DSCNDC = p_DSCNDC
018200060505     C                   EVAL      s_DSCDDC = p_DSCDDC
018300060505     C                   EVAL      s_DSCTC1 = p_DSCTC1
018400060505     C                   EVAL      s_DSCTC2 = p_DSCTC2
018500060505     C                   EVAL      s_DSCGGA = p_DSCGGA
018600060505     C                   EVAL      s_DSCGMA = p_DSCGMA
018700060505     C                   EVAL      s_DSCGVA = p_DSCGVA
018800060505     C                   EVAL      s_DSCNGD = p_DSCNGD
018900060505     C                   EVAL      s_DSCNSS = p_DSCNSS
019000060505     C                   EVAL      s_DSCTIC = p_DSCTIC
019100060505     C                   EVAL      s_DSCVCA = p_DSCVCA
019200060505     C                   EVAL      s_DSCFIV = p_DSCFIV
019300060505     C                   EVAL      s_DSCNFT = p_DSCNFT
019400060505     C                   EVAL      s_DSCDFT = p_DSCDFT
019500060505     C                   EVAL      s_DSCDIV = p_DSCDIV
019600060505     C                   EVAL      s_DSCFLS = p_DSCFLS
019700060505     C                   EVAL      s_DSCNCD = p_DSCNCD
019800060505     C                   EVAL      s_DSCNCA = p_DSCNCA
019900060505     C                   EVAL      s_DSCS01 = p_DSCS01
020000060505     C                   EVAL      s_DSCS02 = p_DSCS02
020100060505     C                   EVAL      s_DSCS03 = p_DSCS03
020200060505     C                   EVAL      s_DSCS04 = p_DSCS04
020300060505     C                   EVAL      s_DSCS05 = p_DSCS05
020400060505     C                   EVAL      s_DSCS06 = p_DSCS06
020500060505     C                   EVAL      s_DSCS07 = p_DSCS07
020600060505     C                   EVAL      s_DSCS08 = p_DSCS08
020700060505     C                   EVAL      s_DSCS09 = p_DSCS09
020800060505     C                   EVAL      s_DSCS10 = p_DSCS10
020900060505     C                   EVAL      s_DSCDGE = p_DSCDGE
021000060505     C                   EVAL      s_DSCHMC = p_DSCHMC
021100060505     C*
021200060505     C* Da signed ad alfa
021300060505     C                   MOVE(P)   s_DSCAAS      DSCAAS
021400060505     C                   MOVE(P)   s_DSCLNP      DSCLNP
021500060505     C                   MOVE(P)   s_DSCNRS      DSCNRS
021600060505     C                   MOVE(P)   s_DSCNSP      DSCNSP
021700060505     C                   MOVE(P)   s_DSCFNS      DSCFNS
021800060505     C                   MOVE(P)   s_DSCLNA      DSCLNA
021900060505     C                   MOVE(P)   s_DSCZNC      DSCZNC
022000060505     C                   MOVE(P)   s_DSCRSM      DSCRSM
022100060505     C                   MOVE(P)   s_DSCINM      DSCINM
022200060505     C                   MOVE(P)   s_DSCCAM      DSCCAM
022300060505     C                   MOVE(P)   s_DSCLOM      DSCLOM
022400060505     C                   MOVE(P)   s_DSCPRM      DSCPRM
022500060505     C                   MOVE(P)   s_DSCFAP      DSCFAP
022600060505     C                   MOVE(P)   s_DSCNZM      DSCNZM
022700060505     C                   MOVE(P)   s_DSCRSD      DSCRSD
022800060505     C                   MOVE(P)   s_DSCIND      DSCIND
022900060505     C                   MOVE(P)   s_DSCCAD      DSCCAD
023000060505     C                   MOVE(P)   s_DSCLOD      DSCLOD
023100060505     C                   MOVE(P)   s_DSCPRD      DSCPRD
023200060505     C                   MOVE(P)   s_DSCNZD      DSCNZD
023300060505     C                   MOVE(P)   s_DSCFIN      DSCFIN
023400060505     C                   MOVE(P)   s_DSCTSP      DSCTSP
023500060505     C                   MOVE(P)   s_DSCFBR      DSCFBR
023600060505     C                   MOVE(P)   s_DSCNCL      DSCNCL
023700060505     C                   MOVE(P)   s_DSCRMN      DSCRMN
023800060505     C                   MOVE(P)   s_DSCRMA      DSCRMA
023900060505     C                   MOVE(P)   s_DSCTCR      DSCTCR
024000060505     C                   MOVE(P)   s_DSCDCR      DSCDCR
024100060505     C                   MOVE(P)   s_DSCHCR      DSCHCR
024200060505     C                   MOVE(P)   s_DSCPDC      DSCPDC
024300060505     C                   MOVE(P)   s_DSCFGS      DSCFGS
024400060505     C                   MOVE(P)   s_DSCNDC      DSCNDC
024500060505     C                   MOVE(P)   s_DSCDDC      DSCDDC
024600060505     C                   MOVE(P)   s_DSCTC1      DSCTC1
024700060505     C                   MOVE(P)   s_DSCTC2      DSCTC2
024800060505     C                   MOVE(P)   s_DSCGGA      DSCGGA
024900060505     C                   MOVE(P)   s_DSCGMA      DSCGMA
025000060505     C                   MOVE(P)   s_DSCGVA      DSCGVA
025100060505     C                   MOVE(P)   s_DSCNGD      DSCNGD
025200060505     C                   MOVE(P)   s_DSCNSS      DSCNSS
025300060505     C                   MOVE(P)   s_DSCTIC      DSCTIC
025400060505     C                   MOVE(P)   s_DSCVCA      DSCVCA
025500060505     C                   MOVE(P)   s_DSCFIV      DSCFIV
025600060505     C                   MOVE(P)   s_DSCNFT      DSCNFT
025700060505     C                   MOVE(P)   s_DSCDFT      DSCDFT
025800060505     C                   MOVE(P)   s_DSCDIV      DSCDIV
025900060505     C                   MOVE(P)   s_DSCFLS      DSCFLS
026000060505     C                   MOVE(P)   s_DSCNCD      DSCNCD
026100060505     C                   MOVE(P)   s_DSCNCA      DSCNCA
026200060505     C                   MOVE(P)   s_DSCS01      DSCS01
026300060505     C                   MOVE(P)   s_DSCS02      DSCS02
026400060505     C                   MOVE(P)   s_DSCS03      DSCS03
026500060505     C                   MOVE(P)   s_DSCS04      DSCS04
026600060505     C                   MOVE(P)   s_DSCS05      DSCS05
026700060505     C                   MOVE(P)   s_DSCS06      DSCS06
026800060505     C                   MOVE(P)   s_DSCS07      DSCS07
026900060505     C                   MOVE(P)   s_DSCS08      DSCS08
027000060505     C                   MOVE(P)   s_DSCS09      DSCS09
027100060505     C                   MOVE(P)   s_DSCS10      DSCS10
027200060505     C                   MOVE(P)   s_DSCDGE      DSCDGE
027300060505     C                   MOVE(P)   s_DSCHMC      DSCHMC
027400060505     C                   MOVE(P)   wPKF          DSCPKF
027500060505     C                   MOVE(P)   wVLF          DSCVLF
027600060505     C                   MOVE(P)   wCAS          DSCCAS
027700060505     C                   MOVE(P)   wIFT          DSCIFT
027800060531     C*
027900060531     C* Forzatura a *blanks dei byte "in +" nei numerici (alcuni clienti erroneamente gestivano
028000060531     C* come valore del campo anche tali byte)
028100060531     C                   EVAL      %subst(DSCAAS:1:2) = *blanks
028200060531     C                   EVAL      %subst(DSCLNP:1:1) = *blanks
028300060531     C                   EVAL      %subst(DSCNRS:1:2) = *blanks
028400060531     C                   EVAL      %subst(DSCNSP:1:1) = *blanks
028500060531     C                   EVAL      %subst(DSCLNA:1:1) = *blanks
028600060531     C                   EVAL      %subst(DSCZNC:1:2) = *blanks
028700060531     C                   EVAL      %subst(DSCNCL:1:1) = *blanks
028800060531     C                   EVAL      %subst(DSCPKF:1:1) = *blanks
028900060531     C                   EVAL      %subst(DSCVLF:1:1) = *blanks
029000060531     C                   EVAL      %subst(DSCRMN:1:1) = *blanks
029100060531     C                   EVAL      %subst(DSCDCR:1:2) = *blanks
029200060531     C                   EVAL      %subst(DSCHCR:1:2) = *blanks
029300060531     C                   EVAL      %subst(DSCPDC:1:1) = *blanks
029400060531     C                   EVAL      %subst(DSCFGS:1:1) = *blanks
029500060531     C                   EVAL      %subst(DSCNDC:1:1) = *blanks
029600060531     C                   EVAL      %subst(DSCDDC:1:2) = *blanks
029700060531     C                   EVAL      %subst(DSCNGD:1:1) = *blanks
029800060531     C                   EVAL      %subst(DSCNSS:1:1) = *blanks
029900060531     C                   EVAL      %subst(DSCCAS:1:1) = *blanks
030000060531     C                   EVAL      %subst(DSCFIV:1:1) = *blanks
030100060531     C                   EVAL      %subst(DSCNFT:1:2) = *blanks
030200060531     C                   EVAL      %subst(DSCDFT:1:2) = *blanks
030300060531     C                   EVAL      %subst(DSCIFT:1:1) = *blanks
030400060531     C                   EVAL      %subst(DSCFLS:1:1) = *blanks
030500060531     C                   EVAL      %subst(DSCNCD:1:1) = *blanks
030600060531     C                   EVAL      %subst(DSCNCA:1:1) = *blanks
030700060531     C                   EVAL      %subst(DSCS01:1:1) = *blanks
030800060531     C                   EVAL      %subst(DSCS02:1:1) = *blanks
030900060531     C                   EVAL      %subst(DSCS03:1:1) = *blanks
031000060531     C                   EVAL      %subst(DSCS04:1:1) = *blanks
031100060531     C                   EVAL      %subst(DSCS05:1:1) = *blanks
031200060531     C                   EVAL      %subst(DSCS06:1:1) = *blanks
031300060531     C                   EVAL      %subst(DSCS07:1:1) = *blanks
031400060531     C                   EVAL      %subst(DSCS08:1:1) = *blanks
031500060531     C                   EVAL      %subst(DSCS09:1:1) = *blanks
031600060531     C                   EVAL      %subst(DSCS10:1:1) = *blanks
031700060531     C                   EVAL      %subst(DSCDGE:1:2) = *blanks
031800060531     C                   EVAL      %subst(DSCHMC:1:2) = *blanks
031900060505     C*
032000060505     C                   ENDSR
032100060505
032200991027
032300991027      /TITLE Operazioni iniziali.
032400991027     C     *inzsr        BEGSR
032500991027     C*
032600991027     C     *ENTRY        PLIST
032700991027     C                   parm                    prmppt
032800991027     C     wrkesito      parm      wrkesito      prmesito
032900070903     C*
033000070903     C* Chiave su TITAS30C - Completa
033100070903     C     KeyTAS30C     klist
033200070903     C                   kfld                    p_dscAAS
033300070903     C                   kfld                    p_dscLNP
033400070903     C                   kfld                    p_dscNRS
033500070903     C                   kfld                    p_dscNSP
033600991027     C*
033700991027     C                   ENDSR
