000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------*
000300010509     H*         - RITORNO DATI AL CLIENTE (ACCETTAZIONE BOLLE/ESISTI CONSEGNA)
000400010509     H*-------------------------------------------------------------------------*
000500030505     FQUADRAVAB IF   E             DISK    rename(quadravab:quadravb0)
000600010509     F                                     prefix(c_)
000700030505     FFNVABPTX  UF A E             DISK
000800030505     FTITAS30C  IF   E           K DISK
000900010131     F                                     rename(titas000:titasx00)
001000010131     F                                     rename(titas010:titasx10)
001100010131     F                                     rename(titasP00:titasxP0)
001200001218     FTITA430C  IF   E           K DISK
001300001218     FTITAA30C  IF   E           K DISK
001400001221     FTNCSB03L  IF   E           K DISK
001500001218     D*--------------------
001600001218     D* DS ESTERNE
001700001218     D*--------------------
001800000526     D DSBL4J        E DS
001900030505     D TIS7NCDSVB    E DS
002000010606     C*---------------------------------------------------------------*
002100001218
002200001218
002300001218
002400920812     C*---------------------------------------------------------------*
002500001218     C* MAIN
002600001218     C*---------------------------------------------------------------*
002700001218     C                   exsr      procedi
002800001218     C*
002900001218     C                   seton                                        LR
003000010509     C*---------------------------------------------------------------*
003100001218
003200001218
003300001218
003400001218     C*------------------------------------------------------------------------*
003500001218     C* PROCEDI - Routine principale
003600001218     C*------------------------------------------------------------------------*
003700001218     C     PROCEDI       BEGSR
003800030505     C*
003900030505     C* Ciclo sul file di input VAB POSTE (CTRLSDA/QUADRAVAB)
004000030505     C                   read      QUADRAVAB
004100030505     C                   dow       not %eof(QUADRAVAB)
004200030505     C*
004300030505     C* Ridefinisco l'input
004400030505     C                   eval      TIS7NCDSVB = c_QUADRAVAB
004500030505     C*
004600030505     C                   move(p)   VB0AAS        tasAAS
004700030505     C                   move(p)   VB0LNP        tasLNP
004800030505     C                   move(p)   VB0NRS        tasNRS
004900030505     C                   move(p)   VB0NSP        tasNSP
005000030505     C*
005100030505     C     K30C          chain     titas30c                           55
005200030505     C                   if        %found(titas30c)
005300030505     C                   exsr      inzfil
005400030505     C                   exsr      exeVAB
005500001221     C                   exsr      scriviVAB
005600030505     C                   else
005700030505     C                   exsr      inzfil
005800030505     C                   eval      vabAAS = tasAAS
005900030505     C                   eval      vabLNP = tasLNP
006000030505     C                   eval      vabNRS = tasNRS
006100030505     C                   eval      vabNSP = tasNSP
006200030505     C                   write     FNVAB000
006300030505     C                   endif
006400010509     C*
006500030505     C                   read      QUADRAVAB
006600001218     C                   enddo
006700001218     C*
006800001218     C                   ENDSR
006900001218     C*------------------------------------------------------------------------*
007000001218
007100001218
007200010509
007300010627     C*------------------------------------------------------------------------*
007400010627     C* INZFIL - Routine di inizializzazione formati record file di OUTPUT
007500010627     C*------------------------------------------------------------------------*
007600010627     C     INZFIL        BEGSR
007700010627     C*
007800010627     C                   clear                   fnvab000
007900010627     C*
008000010627     C                   ENDSR
008100010627     C*------------------------------------------------------------------------*
008200010627
008300010627
008400010627
008500010509     C*------------------------------------------------------------------------*
008600030505     C* EXEVAB - Routine di inizializzazione e reperimento dati VAB
008700010509     C*------------------------------------------------------------------------*
008800030505     C     EXEVAB        BEGSR
008900010509     C*
009000010509     C* Effettuo considerazioni particolare per bolla "POSTE"
009100010509     C     tasTSP        ifeq      'P'                                          BOLLA POSTE
009200010509     C                   exsr      REPPTIAS
009300010509     C                   else
009400010509     C                   movel     tasIAS        vabIAS
009500010509     C                   movel     tasVAS        vabVAS
009600010509     C                   movel     tasXCO        vabXCO
009700010509     C                   endif
009800010509     C*
009900010509     C* Reperisco i dati relativi al contrassegno
010000010509     C                   exsr      REPCSB
010100010509     C*
010200010509     C* Reperisco i dati relativi al riferimento mittente alfabetico
010300010509     C                   exsr      REPRMA
010400010509     C*
010500010509     C* Reperisco dall'estensione anagrafica bolle di SEDE i dati del Destinatario
010600010509     C                   exsr      REPDEST
010700010509     C*
010800010509     C                   ENDSR
010900010509     C*------------------------------------------------------------------------*
011000010131
011100001218
011200001218
011300001218     C*------------------------------------------------------------------------*
011400001221     C* SCRIVIVAB - Routine di scrittura file esiti di consegna (FNVAB)
011500001218     C*------------------------------------------------------------------------*
011600001221     C     SCRIVIVAB     BEGSR
011700001218     C*
011800001221     C                   movel     tascbo        vabcbo
011900001221     C                   movel     tasrsd        vabrsd
012000001221     C                   movel     tasind        vabind
012100001221     C                   movel     tascad        vabcad
012200001221     C                   movel     taslod        vablod
012300001221     C                   movel     tasprd        vabprd
012400001221     C                   movel     tasnzd        vabnzd
012500001221     C                   movel     tasgc1        vabgc1
012600001221     C                   movel     tasgc2        vabgc2
012700001221     C                   movel     tastsp        vabtsp
012800001221     C                   movel     tasctm        vabctm
012900001221     C                   movel     tasffd        vabffd
013000001221     C                   movel     tastcr        vabtcr
013100001221     C                   movel     tascts        vabcts
013200001221     C                   movel     tasftm        vabftm
013300001221     C                   movel     tasgma        vabgma
013400001221     C                   movel     tasgga        vabgga
013500001221     C                   movel     tasgva        vabgva
013600010111     C                   move      ' '           vabgva
013700001221     C                   if        tasccm > 0
013800001221     C                   z-add     tasccm        vabccm
013900001221     C                   else
014000001221     C                   z-add     tasksc        vabccm
014100001221     C                   end
014200001221     C                   z-add     taslnp        vablnp
014300001221     C                   z-add     tasaas        vabaas
014400001221     C                   z-add     tasmgs        vabmgs
014500001221     C                   z-add     tasnrs        vabnrs
014600001221     C                   z-add     tasnsp        vabnsp
014700001221     C                   z-add     taslna        vablna
014800001221     C                   z-add     tasctr        vabctr
014900001221     C                   z-add     tasncl        vabncl
015000001221     C                   z-add     taspkf        vabpkb
015100001221     C                   z-add     tasvlf        vabvlb
015200001221     C                   z-add     tasqft        vabqft
015300001221     C                   z-add     tasrmn        vabrmn
015400001221     C                   z-add     tasncd        vabncd
015500001221     C                   z-add     tasnca        vabnca
015600001221     C                   z-add     tasznc        vabznc
015700010108     C                   z-add     tasdbr        vabdcr
015800001227     C* Considerazioni per bolle "autogenerate"
015900001227     C                   if        tasSOP = '&' and
016000001227     C                             vabSCL = *blanks
016100001227     C                   movel     'A'           vabSCL
016200001227     C                   endif
016300001218     C*
016400001227     C                   write     FNVAB000
016500001218     C*
016600001218     C                   ENDSR
016700001221     C*------------------------------------------------------------------------*
016800010626
016900010626
017000010626
017100001218     C*------------------------------------------------------------------------*
017200001218     C* REPPTIAS - Routine di reperimento dati relativi all'importo/divisa da assicurare x POSTE
017300010131     C*          - e dei pacchi ingombranti non dichiarati
017400001218     C*------------------------------------------------------------------------*
017500001218     C     REPPTIAS      BEGSR
017600001218     C*
017700060215     D* Prendo importo da assicurare da TITA4 t.r.=J
017800001218     C                   movel     'J'           KTRC
017900001218     C     KTA4          chain     tita430c
018000001218     C                   if        %found(tita430c)
018100001218     C                   movel     ta4NOT        DSBL4J
018200010509     C* => istruzioni x VAB
018300001221     C                   z-add     §B4IAS        vabIAS
018400001221     C                   movel     §B4VAS        vabVAS
018500001221     C                   movel     §B4XCO        vabXCO
018600001227     C                   movel     §B4BAU        vabSCL
018700010131     C                   if        §B4BAI = 'S'
018800010131     C                   movel     §B4BAI        vabFTM
018900010131     C                   endif
019000001218     C                   endif
019100001218     C*
019200001218     C                   ENDSR
019300001218     C*------------------------------------------------------------------------*
019400001218
019500001218
019600001218
019700001218
019800001218     C*------------------------------------------------------------------------*
019900001218     C* REPRMA - Routine di reperimento dati relativi al riferimento mittente alfabetico
020000001218     C*------------------------------------------------------------------------*
020100001218     C     REPRMA        BEGSR
020200001218     C*
020300060215     D* Prendo importo da assicurare da TITA4 t.r.=A
020400001218     C                   movel     'A'           KTRC
020500001218     C     KTA4          chain     tita430c
020600001218     C                   if        %found(tita430c)
020700001221     C                   eval      vabRMA = %subst(ta4NOT:1:15)
020800001218     C                   endif
020900001218     C*
021000001218     C                   ENDSR
021100001218     C*------------------------------------------------------------------------*
021200001218
021300001218
021400001218
021500001218
021600001218     C*------------------------------------------------------------------------*
021700001218     C* REPDEST - Routine di reperimento dati relativi al destinatario della bolla (TITAA)
021800001218     C*           ... e ai dati del mittente originale
021900001218     C*------------------------------------------------------------------------*
022000001218     C     REPDEST       BEGSR
022100001218     C*
022200001218     D* Prendo il tipo anagrafica = D
022300001218     C                   movel     'D'           KTRC
022400010131     C     KTAA          chain     titaa30c
022500001218     C                   if        %found(titaa30c)
022600001221     C                   movel     taaRSC        vabRD2
022700001218     C                   endif
022800001218     C*
022900010509     C* Prendo il tipo anagrafica = O
023000001218     C                   movel     'O'           KTRC
023100001218     C     KTAA          chain     titaa30c
023200001218     C                   if        %found(titaa30c)
023300001221     C                   movel     taaRSC        vabRMO
023400001221     C                   movel     taaCAP        vabCMO
023500001221     C                   movel     taaNAZ        vabNMO
023600001218     C                   endif
023700010509     C*
023800001222     C                   if        vabNMO = *blanks
023900001222     C                   move      '.'           vabNMO
024000001222     C                   endif
024100001218     C*
024200001218     C                   ENDSR
024300001218     C*------------------------------------------------------------------------*
024400001218
024500001218
024600001218
024700001218
024800001218     C*------------------------------------------------------------------------*
024900001218     C* REPCSB - Routine di reperimento dati relativi al contrassegno
025000001218     C*------------------------------------------------------------------------*
025100001218     C     REPCSB        BEGSR
025200001218     C*
025300001219     C     KCSB          chain     tncsb03l
025400001219     C                   if        %found(tncsb03l)
025500001218     C                   if        csbSTA = 9 and tasTSP <> 'P'
025600001218     C                   else
025700001221     C                   movel     csbTPA        vabTIC
025800001221     C                   move      csbTPI        vabTIC
025900001221     C                   z-add     csbCAS        vabCAS
026000001221     C                   movel     csbVCA        vabVCA
026100001221     C                   movel     csbGCA        vabGCA
026200001218     C                   endif
026300001218     C                   endif
026400001218     C*
026500001218     C                   ENDSR
026600001218     C*------------------------------------------------------------------------*
026700021028
026800021028
026900021028
027000001218
027100001218     C*------------------------------------------------------------------------*
027200001218     C* *INZSR - ROUTINE INIZIALE
027300001218     C*------------------------------------------------------------------------*
027400001218     C     *INZSR        BEGSR
027500001218     C*
027600001218     C* Definizioni chiavi
027700030505     C     K30C          KLIST                                                  TITAS30C - Parziale
027800030505     C                   KFLD                    tasAAS
027900030505     C                   KFLD                    tasLNP
028000030505     C                   KFLD                    tasNRS
028100030505     C                   KFLD                    tasNSP
028200010131     C*
028300010509     C     KTA4          KLIST                                                  TITA430C - Completa
028400001218     C                   KFLD                    tasAAS
028500001218     C                   KFLD                    tasLNP
028600001218     C                   KFLD                    tasNRS
028700001218     C                   KFLD                    tasNSP
028800001218     C                   KFLD                    KTRC              1
028900001218     C*
029000010509     C     KTAA          KLIST                                                  TITAA30C - Completa
029100001218     C                   KFLD                    tasAAS
029200001218     C                   KFLD                    tasLNP
029300001218     C                   KFLD                    tasNRS
029400001218     C                   KFLD                    tasNSP
029500001218     C                   KFLD                    KTRC
029600001218     C*
029700010509     C     KCSB          KLIST                                                  TNCSB03L - Completa
029800001218     C                   KFLD                    tasAAS
029900001218     C                   KFLD                    tasLNP
030000001218     C                   KFLD                    tasNRS
030100001218     C                   KFLD                    tasNSP
030200001218     C*
030300001218     C                   ENDSR
