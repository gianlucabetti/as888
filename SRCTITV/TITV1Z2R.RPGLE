000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200110921     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300000629
000400090320     Ftivgdtmp  uF   E             DISK
000500130109     Ftivawwwt  O    E             DISK    commit
000600010131     Ftita430c  if   E           k DISK
000700080527     Ftitas30c  if   E           k DISK
000800110921     FFIAR401L  if   e           k disk    usropn
000900110921     F                                     extfile(LibFileAR4)
001000991027
001100991027     D prmppt          s             50
001200991027     D prmesito        s              1
001300991027     D wrkesito        s                   like(prmesito)
001400090320     D fnvabds       e ds                  extname(fnvab00t)
001500130109     D fnvabwwt      e ds                  inz
001600010528     D wrklire         s             10  0
001700010531     D w_vabvmd        s                   like(vabvmd)
001800010531     D w_vabias        s                   like(vabias)
001900010531     D w_vabqft        s                   like(vabqft)
002000010531     D w_vabcas        s                   like(vabcas)
002100010531     D w_vabvas        s                   like(vabvas)
002200010531     D w_vabvca        s                   like(vabvca)
002300010531     D w_vabxco        s                   like(vabxco)
002400000629     D*----------
002500000629     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002600000629     D* 10+3
002700000629     D                 DS
002800000629     D  WS15                   1      1
002900000629     D  WI15                   2     11
003000000629     D  WV15                  12     12
003100000629     D  WD15                  13     15
003200000629     D  WASC15                 1     15
003300000629     D*  6+1
003400000629     D                 DS
003500000629     D  WS09                   1      1
003600000629     D  WI09                   2      7
003700000629     D  WV09                   8      8
003800000629     D  WD09                   9      9
003900000629     D  WASC09                 1      9
004000000629     D*  2+3
004100000629     D                 DS
004200000629     D  WS07                   1      1
004300000629     D  WI07                   2      3
004400000629     D  WV07                   4      4
004500000629     D  WD07                   5      7
004600000629     D  WASC07                 1      7
004700010131     D*-------------------
004800010131     D* DS ESTENSIONE BOLLE TIPO RECORD "J" (X BOLE INGOMBRANTI NON DICHIARATE)
004900010131     D*-------------------
005000010131     D DSBL4J        E DS
005100110921
005200110921
005300110921     D LibFileAR4      s             21A   inz
005400110921     D currSysNeta     s              8A   inz('*NULL')
005500110921
005600110921     D*------------------
005700110921     D* LINKING A DEFINIZIONI ESTERNE
005800110921     D*------------------
005900110921     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
006000110921     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
006100010131
006200010131
006300110921
006400110921     C*
006500110921     C* Reperisco il sistema AS/400 corrente
006600110921     C                   callp     UBRTVNETA_Rtv(currSysNeta)
006700110921     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
006800110921     C                   eval      LibFileAR4 = 'FILTRA201/FIAR401L'
006900110921     C                   else
007000110921     C                   eval      LibFileAR4 = 'FILTRAPRD/FIAR401L'
007100110921     C                   endif
007200110921     C*
007300110921     C                   if        not %open(FIAR401L)
007400110921     C                   open      FIAR401L
007500110921     C                   endif
007600110921     C*
007700000629     C                   exsr      defcam
007800991027     C                   exsr      traduci
007900991027     C                   seton                                        lr
008000991027
008100110921
008200110921
008300000629     C     defcam        begsr
008400000629     C*
008500000629     C                   MOVE      *BLANK        WS15
008600000629     C                   MOVE      ','           WV15
008700000629     C                   MOVE      *BLANK        WS07
008800000629     C                   MOVE      ','           WV07
008900000629     C                   MOVE      *BLANK        WS09
009000000629     C                   MOVE      ','           WV09
009100000629     C*
009200000629     C                   ENDSR
009300000629
009400110921
009500000629
009600000629     C     traduci       begsr
009700000629     C*
009800090320     C                   READ      tivgdtmp
009900000629     C                   dow       not %eof
010000100729     C*
010100100729     C* Includo solo i record scritti dal *pgm 'TIS7VBR'
010200100729     C                   if        vgdpgm = 'TIS7VBR'
010300100729     C*
010400100729     C                   movel     vgddta        fnvabds
010500080613     C*
010600080613     C                   setoff                                       31
010700080613     C*
010800080613     C* Se richiesto nei parametri d ingresso ritorno solamente le bolle originali
010900080613     C                   if        %subst(prmppt:7:1) = 'S'
011000080613     C                   if        VABNRS = *zeros
011100080613     C                   seton                                        31
011200080613     C                   endif
011300080613     C                   endif
011400080613     C*
011500080613     C* Se tutto ok => procedo
011600080613     C                   if        *in31 = *off
011700010528     C*
011800010528     C* Eseguo considerazioni su importi se divisa è EUR per bolle POSTE
011900010528     C                   exsr      repimp
012000000904     C*
012100000629     C                   MOVE      VABAAS        VHBAAS
012200000629     C                   MOVE      VABLNP        VHBLNP
012300000629     C                   MOVE      VABNRS        VHBNRS
012400000629     C                   MOVE      VABNSP        VHBNSP
012500000629     C                   MOVE      VABMGS        VHBMGS
012600000629     C                   MOVE      VABCBO        VHBCBO
012700000629     C                   MOVE      VABLNA        VHBLNA
012800000629     C                   MOVE      VABRSD        VHBRSD
012900000629     C                   MOVE      VABPRD        VHBPRD
013000000629     C                   MOVE      VABGC1        VHBGC1
013100000629     C                   MOVE      VABGC2        VHBGC2
013200000629     C                   MOVE      VABCTR        VHBCTR
013300000629     C                   MOVE      VABCTS        VHBCTS
013400000629     C                   MOVE      VABFTM        VHBFTM
013500000629     C                   MOVE      VABTSP        VHBTSP
013600000629     C*
013700010531     C                   MOVE      W_VABIAS      WD15
013800010531     C                   MOVEL     W_VABIAS      WI15
013900000629     C                   MOVE      WASC15        VHBIAS
014000000629     C*
014100010531     C                   MOVE      W_VABVAS      VHBVAS
014200010531     C                   MOVE      VABNAS        VHBNAS
014300000629     C                   MOVE      VABNCL        VHBNCL
014400000629     C*
014500000629     C                   MOVE      VABPKB        WD09
014600000629     C                   MOVEL     VABPKB        WI09
014700000629     C                   MOVE      WASC09        VHBPKB
014800000629     C*
014900000629     C                   MOVE      VABVLB        WD07
015000000629     C                   MOVEL     VABVLB        WI07
015100000629     C                   MOVE      WASC07        VHBVLB
015200000629     C*
015300010531     C                   MOVE      W_VABQFT      WD15
015400010531     C                   MOVEL     W_VABQFT      WI15
015500000629     C                   MOVE      WASC15        VHBQFT
015600000629     C*
015700010531     C                   MOVE      W_VABVMD      WD15
015800010531     C                   MOVEL     W_VABVMD      WI15
015900010530     C                   MOVE      WASC15        VHBVMD
016000010530     C*
016100000629     C                   MOVE      VABTIC        VHBTIC
016200000629     C*
016300010531     C                   MOVE      W_VABCAS      WD15
016400010531     C                   MOVEL     W_VABCAS      WI15
016500000629     C                   MOVE      WASC15        VHBCAS
016600000629     C*
016700010531     C                   MOVE      W_VABVCA      VHBVCA
016800000629     C                   MOVE      VABCCM        VHBCCM
016900000629     C                   MOVE      VABRMN        VHBRMN
017000000629     C                   MOVE      VABRMA        VHBRMA
017100000629     C                   MOVE      VABRMO        VHBRMO
017200000629     C                   MOVE      VABFFD        VHBFFD
017300000629     C                   MOVE      VABTCR        VHBTCR
017400000629     C                   MOVE      VABDCR        VHBDCR
017500000629     C                   MOVE      VABHCR        VHBHCR
017600000629     C                   MOVE      VABTC1        VHBTC1
017700000629     C                   MOVE      VABTC2        VHBTC2
017800000629     C                   MOVE      VABATB        VHBATB
017900000629     C                   MOVE      VABRD2        VHBRD2
018000000629     C                   MOVE      VABIND        VHBIND
018100000629     C                   MOVE      VABCAD        VHBCAD
018200000629     C                   MOVE      VABLOD        VHBLOD
018300000629     C                   MOVE      VABNZD        VHBNZD
018400000629     C                   MOVE      VABGCA        VHBGCA
018500000629     C                   MOVE      VABNCD        VHBNCD
018600000629     C                   MOVE      VABNCA        VHBNCA
018700010531     C                   MOVE      W_VABXCO      VHBXCO
018800000629     C                   MOVE      VABZNC        VHBZNC
018900000629     C                   MOVE      VABCTM        VHBCTM
019000000629     C                   MOVE      VABVAD        VHBVAD
019100000629     C                   MOVE      VABGMA        VHBGMA
019200000629     C                   MOVE      VABGGA        VHBGGA
019300000629     C                   MOVE      VABGVA        VHBGVA
019400000629     C                   MOVE      VABSCL        VHBSCL
019500000629     C                   MOVE      VABANT        VHBANT
019600000629     C                   MOVE      VABCMO        VHBCMO
019700000629     C                   MOVE      VABNMO        VHBNMO
019800010131     C*
019900010131     C* Eseguo considerazioni su pacchi ingombranti aggiunti da Bartolini
020000110921     C                   exsr      repTA4
020100110921     C*
020200110921     C* Reperimento note consegna
020300110921     C                   exsr      repNOT
020400110921     C*
020500110921     C                   MOVE      wNOTE_8       VHBNOT
020600110921     C                   MOVE      wNOTE_9       VHBNT2
020700080527     C*
020800080527     C* Se richiesto nei parametri d ingresso valorizzo da data consegna richiesta con il
020900080527     C* proprio valore
021000080527     C                   if        %subst(prmppt:5:1) = 'R'
021100080527     C                   exsr      reptas
021200080527     C                   endif
021300130109     C*
021400130109     C* Scarico il buffer di output
021500130109     C                   eval      VAWDTA = fnvabwwt
021600000629     C*
021700130109     C                   WRITE     TIVAW000
021800080613     C                   endif
021900100729     C*
022000100729     C                   endif
022100080613     C*
022200090320     C                   delete    tivgd000
022300000629     C*
022400090320     C                   READ      tivgdtmp
022500000629     C                   ENDdo
022600000629     C*
022700000629     C                   eval      wrkesito = '0'
022800000629     C*
022900000629     C                   ENDSR
023000010131
023100010131
023200010131
023300010131
023400010131     C*
023500010131     C     repta4        BEGSR
023600010131     C*
023700010131     C* Inizializzo i campi "fissi"
023800010131     C                   movel     'J'           kt4trc
023900010131     C* Compongo la chiave della bolla
024000010131     C                   z-add     vabaas        kt4aas
024100010131     C                   z-add     vablnp        kt4lnp
024200010131     C                   z-add     vabnrs        kt4nrs
024300010131     C                   z-add     vabnsp        kt4nsp
024400010131     C* Posizionamento diretto
024500110921     C     KEYta430_C    chain     tita430c
024600010131     C                   if        %found(tita430c)
024700010131     C                   clear                   DSBL4J
024800010131     C                   movel     ta4not        DSBL4J
024900010131     C                   if        §B4BAI = 'S'
025000010131     C                   movel     'S'           VHBFTM
025100010131     C                   endif
025200010131     C                   endif
025300010131     C*
025400010131     C                   ENDSR
025500080527
025600080527
025700080527
025800080527
025900080527     C*
026000080527     C     reptas        BEGSR
026100080527     C*
026200080527     C* Posizionamento diretto
026300080527     C     KEYtas30_P    chain     titas30c
026400080527     C                   if        %found(titas30c)
026500080527     C                   move      tasDCR        VHBDCR
026600080527     C                   endif
026700080527     C*
026800080527     C                   ENDSR
026900010131
027000010131
027100010528
027200010528
027300010608     C*
027400010608     C     repimp        BEGSR
027500010608     C*
027600010608     C* Inizializzo le variabili di wrk
027700010608     C                   eval      w_vabxco = *blanks
027800010608     C                   eval      w_vabvas = *blanks
027900010608     C                   eval      w_vabvca = *blanks
028000010608     C                   eval      w_vabias = *zeros
028100010608     C                   eval      w_vabcas = *zeros
028200010608     C                   eval      w_vabvmd = *zeros
028300010608     C                   eval      w_vabqft = *zeros
028400010608     C*
028500010608     C* Come prima cosa verifico se trattasi di bolla POSTE
028600010608     C                   if        vabtsp = 'P'
028700010608     C* ... se divisa è EUR allora reperisco il controvalore in ITL e sdoppio i valori nei campi
028800010608     C                   if        vabvas = 'EUR'
028900010608     C                   eval      w_vabvmd = vabias
029000010608     C                   eval(h)   wrklire = w_vabvmd * 1936,27
029100010608     C                   eval      w_vabias = wrklire
029200010608     C                   eval      w_vabvas = *blanks
029300010608     C                   else
029400010608     C                   eval      w_vabias = vabias
029500010608     C                   eval      w_vabvas = vabvas
029600010608     C                   endif
029700010608     C*
029800010608     C                   if        vabvca = 'EUR'
029900010608     C                   eval      w_vabqft = vabcas
030000010608     C                   eval(h)   wrklire = w_vabqft * 1936,27
030100010608     C                   eval      w_vabcas = wrklire
030200010608     C                   eval      w_vabvca = *blanks
030300010608     C                   else
030400010608     C                   eval      w_vabcas = vabcas
030500010608     C                   eval      w_vabvca = vabvca
030600010608     C                   endif
030700010608     C*
030800010608     C                   eval      w_vabxco = *blanks
030900010608     C                   else
031000010608     C                   eval      w_vabcas = vabcas
031100010608     C                   eval      w_vabvca = vabvca
031200010608     C                   eval      w_vabias = vabias
031300010608     C                   eval      w_vabvas = vabvas
031400010608     C                   eval      w_vabxco = vabxco
031500010608     C                   endif
031600010608     C*
031700010608     C                   ENDSR
031800110921
031900110921
032000110921
032100110921     C     repNOT        BEGSR
032200110921     C*
032300110921     C                   movel     *blanks       wNOTE_8          35
032400110921     C                   movel     *blanks       wNOTE_9          35
032500110921     C*
032600110921     C                   eval      ar4trc = '8'
032700110921     C     KeyAR401_C    chain     fiar401l
032800110921     C                   if        %found(fiar401l)
032900110921     C                   eval      wNOTE_8 = ar4NOT
033000110921     C                   else
033100110921     C                   eval      ta4trc = '8'
033200110921     C     KEYta430_C    chain     tita430c
033300110921     C                   if        %found(tita430c)
033400110921     C                   eval      wNOTE_8 = ta4NOT
033500110921     C                   endif
033600110921     C                   endif
033700110921     C*
033800110921     C                   eval      ar4TRC = '9'
033900110921     C     KeyAR401_C    chain     fiar401l
034000110921     C                   if        %found(fiar401l)
034100110921     C                   eval      wNOTE_9 = ar4NOT
034200110921     C                   else
034300110921     C                   eval      ta4trc = '9'
034400110921     C     KEYta430_C    chain     tita430c
034500110921     C                   if        %found(tita430c)
034600110921     C                   eval      wNOTE_9 = ta4NOT
034700110921     C                   endif
034800110921     C                   endif
034900110921     C*
035000110921     C                   ENDSR
035100010528
035200010528
035300000629
035400991027
035500991027      /TITLE Operazioni iniziali.
035600991027     C     *inzsr        BEGSR
035700991027     C*
035800991027     C     *ENTRY        PLIST
035900991027     C                   parm                    prmppt
036000991027     C     wrkesito      parm      wrkesito      prmesito
036100010131     C*
036200010131     C* Campi riferiti al data base
036300010131     C     *LIKE         DEFINE    ta4aas        kt4aas
036400010131     C     *LIKE         DEFINE    ta4lnp        kt4lnp
036500010131     C     *LIKE         DEFINE    ta4nrs        kt4nrs
036600010131     C     *LIKE         DEFINE    ta4nsp        kt4nsp
036700010131     C     *LIKE         DEFINE    ta4trc        kt4trc
036800010131     C*
036900110921     C     KEYta430_C    KLIST
037000010131     C                   KFLD                    kt4aas
037100010131     C                   KFLD                    kt4lnp
037200010131     C                   KFLD                    kt4nrs
037300010131     C                   KFLD                    kt4nsp
037400010131     C                   KFLD                    kt4trc
037500080527     C*
037600080527     C     KEYtas30_P    KLIST
037700080527     C                   KFLD                    vabaas
037800080527     C                   KFLD                    vablnp
037900080527     C                   KFLD                    vabnrs
038000080527     C                   KFLD                    vabnsp
038100110921     C*
038200110921     C* Chiave su FIAR401L - Completa
038300110921     C     KeyAR401_C    klist
038400110921     C                   kfld                    vabaas
038500110921     C                   kfld                    vablnp
038600110921     C                   kfld                    vabnrs
038700110921     C                   kfld                    vabnsp
038800110921     C                   kfld                    ar4trc
038900991027     C*
039000991027     C                   ENDSR
