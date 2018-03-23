000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000629
000400090320     Ftivgdtmp  uF   E             DISK
000500110504     Ftivawwwt  O    E             DISK    commit
000600010131     Ftita430c  if   E           k DISK
000700110504     Ffnorm01l  if   E           k DISK
000800991027
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100991027     D wrkesito        s                   like(prmesito)
001200090320     D fnvabds       e ds                  extname(fnvab00t)
001300110503     D fnvabww       e ds                  extname(fnvab1wt)
001400010528     D wrklire         s             10  0
001500010531     D w_vabvmd        s                   like(vabvmd)
001600010531     D w_vabias        s                   like(vabias)
001700010531     D w_vabqft        s                   like(vabqft)
001800010531     D w_vabcas        s                   like(vabcas)
001900010531     D w_vabvas        s                   like(vabvas)
002000010531     D w_vabvca        s                   like(vabvca)
002100010531     D w_vabxco        s                   like(vabxco)
002200010131     D*-------------------
002300110503     D* DS ESTENSIONE BOLLE TIPO RECORD "M" E "A"
002400010131     D*-------------------
002500110503     D DTA4M         E DS
002600110503     D DTA4A         E DS
002700110704     D*-------------------
002800110704     D* DS RIDEFINIZIONE CHIAVE ESTERNA ORM=>BOLLA
002900110704     D*-------------------
003000110704     D DS_ORMBOL       DS
003100110704     D  D_OB_B                        1
003200110704     D  D_OB_AA                       2  0
003300110704     D  D_OB_LNP                      3  0
003400110704     D  D_OB_NRS                      2  0
003500110704     D  D_OB_NSP                      7  0
003600991027
003700010131
003800010131
003900991027     C                   exsr      traduci
004000991027     C                   seton                                        lr
004100000629
004200110503
004300000629
004400000629     C     traduci       begsr
004500000629     C*
004600090320     C                   READ      tivgdtmp
004700000629     C                   dow       not %eof
004800110503     C*
004900110503     C                   setoff                                       31
005000100729     C*
005100110503     C                   clear                   fnvabww
005200110503     C                   clear                   dta4m
005300110503     C                   clear                   dta4a
005400100729     C                   movel     vgddta        fnvabds
005500010528     C*
005600010528     C* Eseguo considerazioni su importi se divisa è EUR per bolle POSTE
005700010528     C                   exsr      repimp
005800110503     C*
005900110503     C                   MOVE      VABAAS        VHBAAS
006000110503     C                   MOVE      VABLNP        VHBLNP
006100110503     C                   MOVE      VABNRS        VHBNRS
006200110503     C                   MOVE      VABNSP        VHBNSP
006300110503     C                   MOVE      VABMGS        VHBMGS
006400110503     C                   MOVE      VABCBO        VHBCBO
006500110503     C                   MOVE      VABLNA        VHBLNA
006600110503     C                   MOVE      VABRSD        VHBRSD
006700110503     C                   MOVE      VABPRD        VHBPRD
006800110503     C                   MOVE      VABGC1        VHBGC1
006900110503     C                   MOVE      VABGC2        VHBGC2
007000110503     C                   MOVE      VABCTR        VHBCTR
007100110503     C                   MOVE      VABCTS        VHBCTS
007200110503     C                   MOVE      VABFTM        VHBFTM
007300110503     C                   MOVE      VABTSP        VHBTSP
007400110503     C*
007500110503     C                   MOVE      W_VABIAS      VHBIAS
007600110503     C                   MOVE      W_VABVAS      VHBVAS
007700110503     C                   MOVE      VABNAS        VHBNAS
007800110503     C                   MOVE      VABNCL        VHBNCL
007900110503     C                   MOVE      VABPKB        VHBPKB
008000110503     C                   MOVE      VABVLB        VHBVLB
008100110503     C                   MOVE      W_VABQFT      VHBQFT
008200110503     C                   MOVE      W_VABVMD      VHBVMD
008300110503     C                   MOVE      VABTIC        VHBTIC
008400110503     C                   MOVE      W_VABCAS      VHBCAS
008500110503     C                   MOVE      W_VABVCA      VHBVCA
008600110503     C                   MOVE      VABCCM        VHBCCM
008700110503     C                   MOVE      VABRMN        VHBRMN
008800110503     C                   MOVE      VABRMA        VHBRMA
008900110503     C                   MOVE      VABRMO        VHBRMO
009000110503     C                   MOVE      VABFFD        VHBFFD
009100110503     C                   MOVE      VABTCR        VHBTCR
009200110503     C                   MOVE      VABDCR        VHBDCR
009300110503     C                   MOVE      VABHCR        VHBHCR
009400110503     C                   MOVE      VABTC1        VHBTC1
009500110503     C                   MOVE      VABTC2        VHBTC2
009600110503     C                   MOVE      VABATB        VHBATB
009700110503     C                   MOVE      VABRD2        VHBRD2
009800110503     C                   MOVE      VABIND        VHBIND
009900110503     C                   MOVE      VABCAD        VHBCAD
010000110503     C                   MOVE      VABLOD        VHBLOD
010100110503     C                   MOVE      VABNZD        VHBNZD
010200110503     C                   MOVE      VABGCA        VHBGCA
010300110503     C                   MOVE      VABNCD        VHBNCD
010400110503     C                   MOVE      VABNCA        VHBNCA
010500110503     C                   MOVE      W_VABXCO      VHBXCO
010600110503     C                   MOVE      VABNOT        VHBNOT
010700110503     C                   MOVE      VABNT2        VHBNT2
010800110503     C                   MOVE      VABZNC        VHBZNC
010900110503     C                   MOVE      VABCTM        VHBCTM
011000110503     C                   MOVE      VABVAD        VHBVAD
011100110503     C                   MOVE      VABGMA        VHBGMA
011200110503     C                   MOVE      VABGGA        VHBGGA
011300110503     C                   MOVE      VABGVA        VHBGVA
011400110503     C                   MOVE      VABSCL        VHBSCL
011500110503     C                   MOVE      VABANT        VHBANT
011600110503     C                   MOVE      VABCMO        VHBCMO
011700110523     C                   MOVE      *blanks       VHBNMO
011800010131     C*
011900110503     C* Reperisco estensioni "M" e "A"
012000110503     C                   exsr      reptaM
012100110704     C*
012200110704     C* Dal ORM reperisco l'RMA della bolla originale
012300110704     C                   if        *in31 and ormRFA <> *blanks
012400110704     C                   eval      DS_ORMBOL = ormRFA
012500110704     C                   z-add     *zeros        wAnno             4 0
012600110704     C                   eval      wAnno = 2000 + D_OB_AA
012700110704     C                   exsr      reptaA
012800110704     C                   else
012900110704     C                   setoff                                       31
013000110704     C                   endif
013100000629     C*
013200110503     C                   eval      vawDTA = fnvabww + §TA4ARMA
013300110503     C*
013400110504     C   31              WRITE     TIVAW000
013500090320     C                   delete    tivgd000
013600000629     C*
013700090320     C                   READ      tivgdtmp
013800000629     C                   ENDdo
013900000629     C*
014000000629     C                   eval      wrkesito = '0'
014100000629     C*
014200000629     C                   ENDSR
014300010131
014400010131
014500010131
014600010131     C*
014700110503     C     reptaM        BEGSR
014800010131     C*
014900010131     C* Inizializzo i campi "fissi"
015000110503     C                   movel     'M'           kt4trc
015100010131     C* Posizionamento diretto
015200110504     C     KEYTA430_C    chain     tita430c
015300010131     C                   if        %found(tita430c)
015400110504     C                   movel     ta4not        DTA4M
015500110503     C*
015600110503     C* Se trovato => reperisco l'ORM relativo
015700110504     C     KEYORM01_C    chain     fnorm01l
015800110504     C                   if        %found(fnorm01l)
015900110504     C                   seton                                        31
016000010131     C                   endif
016100110504     C                   endif
016200010131     C*
016300010131     C                   ENDSR
016400110503
016500110503
016600110503
016700110503     C*
016800110503     C     reptaA        BEGSR
016900110503     C*
017000110503     C* Inizializzo i campi "fissi"
017100110503     C                   movel     'A'           kt4trc
017200110503     C* Posizionamento diretto
017300110704     C     KEYTA430_C_2  chain     tita430c
017400110503     C                   if        %found(tita430c)
017500110503     C                   movel     ta4not        DTA4A
017600110503     C                   endif
017700110503     C*
017800110503     C                   ENDSR
017900010131
018000010131
018100010528
018200010608     C*
018300010608     C     repimp        BEGSR
018400010608     C*
018500010608     C* Inizializzo le variabili di wrk
018600010608     C                   eval      w_vabxco = *blanks
018700010608     C                   eval      w_vabvas = *blanks
018800010608     C                   eval      w_vabvca = *blanks
018900010608     C                   eval      w_vabias = *zeros
019000010608     C                   eval      w_vabcas = *zeros
019100010608     C                   eval      w_vabvmd = *zeros
019200010608     C                   eval      w_vabqft = *zeros
019300010608     C*
019400010608     C* Come prima cosa verifico se trattasi di bolla POSTE
019500010608     C                   if        vabtsp = 'P'
019600010608     C* ... se divisa è EUR allora reperisco il controvalore in ITL e sdoppio i valori nei campi
019700010608     C                   if        vabvas = 'EUR'
019800010608     C                   eval      w_vabvmd = vabias
019900010608     C                   eval(h)   wrklire = w_vabvmd * 1936,27
020000010608     C                   eval      w_vabias = wrklire
020100010608     C                   eval      w_vabvas = *blanks
020200010608     C                   else
020300010608     C                   eval      w_vabias = vabias
020400010608     C                   eval      w_vabvas = vabvas
020500010608     C                   endif
020600010608     C*
020700010608     C                   if        vabvca = 'EUR'
020800010608     C                   eval      w_vabqft = vabcas
020900010608     C                   eval(h)   wrklire = w_vabqft * 1936,27
021000010608     C                   eval      w_vabcas = wrklire
021100010608     C                   eval      w_vabvca = *blanks
021200010608     C                   else
021300010608     C                   eval      w_vabcas = vabcas
021400010608     C                   eval      w_vabvca = vabvca
021500010608     C                   endif
021600010608     C*
021700010608     C                   eval      w_vabxco = *blanks
021800010608     C                   else
021900010608     C                   eval      w_vabcas = vabcas
022000010608     C                   eval      w_vabvca = vabvca
022100010608     C                   eval      w_vabias = vabias
022200010608     C                   eval      w_vabvas = vabvas
022300010608     C                   eval      w_vabxco = vabxco
022400010608     C                   endif
022500010608     C*
022600010608     C                   ENDSR
022700010528
022800000629
022900991027
023000991027      /TITLE Operazioni iniziali.
023100991027     C     *inzsr        BEGSR
023200991027     C*
023300991027     C     *ENTRY        PLIST
023400991027     C                   parm                    prmppt
023500991027     C     wrkesito      parm      wrkesito      prmesito
023600010131     C*
023700010131     C* Campi riferiti al data base
023800010131     C     *LIKE         DEFINE    ta4trc        kt4trc
023900010131     C*
024000110504     C     KEYTA430_C    KLIST
024100110503     C                   KFLD                    vabaas
024200110503     C                   KFLD                    vablnp
024300110503     C                   KFLD                    vabnrs
024400110503     C                   KFLD                    vabnsp
024500010131     C                   KFLD                    kt4trc
024600110704     C*
024700110704     C     KEYTA430_C_2  KLIST
024800110704     C                   KFLD                    wAnno
024900110704     C                   KFLD                    D_OB_LNP
025000110704     C                   KFLD                    D_OB_NRS
025100110704     C                   KFLD                    D_OB_NSP
025200110704     C                   KFLD                    kt4trc
025300110504     C*
025400110504     C     KEYORM01_C    KLIST
025500110504     C                   KFLD                    §TA4MPOE
025600110504     C                   KFLD                    §TA4MNSR
025700110504     C                   KFLD                    §TA4MNOR
025800110504     C                   KFLD                    §TA4MNRV
025900991027     C*
026000991027     C                   ENDSR
