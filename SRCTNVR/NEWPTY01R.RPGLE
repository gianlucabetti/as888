000100060801      *===============================================================*
000200061127     H decedit('0,') datedit(*dmy/)
000300061127     F*------------------------------------------------------------------------*
000400061127     F* DATA BASE
000500061127     F*------------------------------------------------------------------------*
000600090608     Fazcpc01l  IF   E           K DISK
000700061204     Fazcpl01l  UF   E           K DISK
000800090608     Fxxpty01l  IF   E           K DISK
000900061127     FPRTF198   O    F  220        PRINTER usropn
001000090608     FQSYSPRT   O    F  132        PRINTER usropn
001100090608     D*------------------------------------------------------------------------*
001200090608     D* DS
001300090608     D*------------------------------------------------------------------------*
001400090608     D tisi81ds      e ds
001500061127     C*------------------------------------------------------------------------*
001600061127     C* MAIN LINE
001700061127     C*------------------------------------------------------------------------*
001800061127     C*
001900061127     C     *Entry        plist
002000090608     C                   parm                    IN_VER            5
002100090608     C                   parm                    IN_ELA            1
002200061204     C*
002300061204     C* Imposto la versione cappario da trattare
002400090608     C                   move(p)   IN_VER        wVERS             5 0
002500061204     C*
002600061204     C* Verifico se lancio in aggiornamento
002700090608     C                   if        IN_ELA = 'A'
002800061204     C                   seton                                        55
002900061204     C                   else
003000061204     C                   setoff                                       55
003100061204     C                   endif
003200061204     C*
003300061204     C* Definizione chiavi di lettura
003400061204     C*
003500061204     C* AZCPC01L - Completa
003600061204     C     KEYcpc01_C    KLIST
003700061204     C                   KFLD                    cplVER
003800061204     C                   KFLD                    cplNAR
003900061204     C                   KFLD                    cplCAP
004000090608     C*
004100090608     C* XXPTY01L - Completa
004200090608     C     KEYpty01_C    KLIST
004300090608     C                   KFLD                    cplCAP
004400090608     C                   KFLD                    cplLOC
004500061204     C*
004600061204     C* Elabora
004700061127     C                   exsr      Procedi
004800061127     C*
004900061127     C* Fine
005000061127     C                   eval      *INLR = *on
005100061127     C*--------------------------------------------------------------------------------------------*
005200061127     C* Procedi - Routine iter di elaborazinoe
005300061127     C*--------------------------------------------------------------------------------------------*
005400061127     C     Procedi       BEGSR
005500061127     C*
005600090608     C* Imposta nuovi codici tempo sulle località
005700061204     C                   exsr      exeLoc
005800061127     C*
005900061127     C                   ENDSR
006000061127     C*--------------------------------------------------------------------------------------------*
006100090608     C* exeLoc - Imposta nuovi codici tempo sulle località
006200061127     C*--------------------------------------------------------------------------------------------*
006300061204     C     exeLoc        BEGSR
006400061127     C*
006500061127     C* Apro il printer file
006600061127     C                   open      PRTF198
006700090608     C                   open      QSYSPRT
006800090610     C*
006900090610     C                   if        IN_ELA = 'A'
007000090610     C                   eval      wRigaPrt='* Elaborazione in AGGIORNAMENTO *'
007100090610     C                   else
007200090610     C                   eval      wRigaPrt='* Elaborazione in SIMULAZIONE *'
007300090610     C                   endif
007400090610     C                   except    staLista
007500090610     C                   except    staAgg
007600090610     C                   eval      wRigaPrt=''
007700090610     C                   except    staLista
007800090610     C                   except    staAgg
007900061127     C*
008000061204     C* Scorro tutto il file AZCPL01L (solo versione interessata)
008100061204     C     wVERS         setll     azcpl01l
008200061204     C     wVERS         reade     azcpl01l
008300061204     C                   dow       not %eof(azcpl01l)
008400090608     C*
008500090608     C* Inizializzo riga d stampa
008600090608     C                   movel     *blanks       wRigaPrt        120
008700090608     C*
008800090608     C* Se necessario aggancio subito il cap
008900090608     C                   if        cplLIV <> 'L'
009000090608     C     KEYcpc01_C    chain     AZCPC01L
009100090608     C                   if        %found(azcpc01l)
009200090608     C                   z-add     cpcTTC        wTTC              5 0
009300090608     C                   else
009400090608     C                   z-add     *zeros        wTTC
009500090608     C                   endif
009600090608     C                   else
009700090608     C                   z-add     cplTTC        wTTC
009800090608     C                   endif
009900090608     C*
010000090608     C* Salvo il codice tipo tempo attuale
010100090608     C                   move(p)   wTTC          wSavTTC           5
010200090608     C*
010300090608     C* Aggancio subito il "PTY" relativo
010400090608     C     KEYpty01_C    chain     xxpty01l
010500090608     C                   if        not %found(xxpty01l)
010600090608     C                   eval      wRigaPrt = 'ERR - Località ' + cplCAP +
010700090608     C                                        ' - '       + cplLOC +
010800090608     C                                  ' non trovata in file "NewPriority"'
010900090608     C                   except    staLista
011000090608     C                   else
011100090608     C                   select
011200090608     C* ...da 1 => 4
011300090608     C                   when      wTTC = 1 and xptyNEW = 'SI'
011400090608     C                   if        cplLIV = 'L'
011500090608     C                   eval      cplTTC = 4
011600090608     C                   else
011700090608     C                   z-add     4             wTTC              5 0
011800090608     C                   exsr      impCPC
011900090608     C                   endif
012000090608     C                   exsr      updCPL
012100090608     C                   eval      wRigaPrt = 'UDP - Località ' + cplCAP +
012200090608     C                                        ' - '             + cplLOC +
012300090608     C                                  ' modificato tipo tempo da '     +
012400090608     C                                  wSavTTC + ' a 4'
012500090608     C                   except    staAgg
012600090608     C* ...da 8 => 9
012700090608     C                   when      wTTC = 8 and xptyNEW = 'SI'
012800090608     C                   if        cplLIV = 'L'
012900090608     C                   eval      cplTTC = 9
013000090608     C                   else
013100090608     C                   z-add     9             wTTC              5 0
013200090608     C                   exsr      impCPC
013300090608     C                   endif
013400090608     C                   exsr      updCPL
013500090608     C                   eval      wRigaPrt = 'UDP - Località ' + cplCAP +
013600090608     C                                        ' - '             + cplLOC +
013700090608     C                                  ' modificato tipo tempo da '     +
013800090608     C                                  wSavTTC + ' a 9'
013900090608     C                   except    staAgg
014000090608     C* ...da 3 => 1
014100090608     C                   when      wTTC = 3 and xptyNEW = 'NO'
014200090608     C                   if        cplLIV = 'L'
014300090608     C                   eval      cplTTC = 1
014400090608     C                   else
014500090608     C                   z-add     1             wTTC              5 0
014600090608     C                   exsr      impCPC
014700090608     C                   endif
014800090608     C                   exsr      updCPL
014900090608     C                   eval      wRigaPrt = 'UDP - Località ' + cplCAP +
015000090608     C                                        ' - '             + cplLOC +
015100090608     C                                  ' modificato tipo tempo da '     +
015200090608     C                                  wSavTTC + ' a 1'
015300090608     C                   except    staAgg
015400090608     C* ...da 4 => 1
015500090608     C                   when      wTTC = 4 and xptyNEW = 'NO'
015600090608     C                   if        cplLIV = 'L'
015700090608     C                   eval      cplTTC = 1
015800090608     C                   else
015900090608     C                   z-add     1             wTTC              5 0
016000090608     C                   exsr      impCPC
016100090608     C                   endif
016200090608     C                   exsr      updCPL
016300090608     C                   eval      wRigaPrt = 'UDP - Località ' + cplCAP +
016400090608     C                                        ' - '             + cplLOC +
016500090608     C                                  ' modificato tipo tempo da '     +
016600090608     C                                  wSavTTC + ' a 1'
016700090608     C                   except    staAgg
016800090608     C*
016900090608     C* ...altri codici tipo tempo => listo (solo differenze)
017000090608     C                   other
017100090608     C                   if        (wTTC = 3 or wTTC = 4 or wTTC = 5 or
017200090608     C                              wTTC = 6 or wTTC = 13) and
017300090608     C                             xptyNEW = 'NO'
017400090608     C                   eval      wRigaPrt = 'DIF - Località ' + cplCAP +
017500090608     C                                        ' - '             + cplLOC +
017600090608     C                                  ' tipo tempo "SI/NO"'
017700090608     C                   except    staLista
017800090608     C                   endif
017900090608     C                   if        (wTTC <> 3 and wTTC <> 4 and wTTC <> 5
018000090608     C                              and wTTC <> 6 and wTTC <> 13) and
018100090608     C                             xptyNEW = 'SI'
018200090608     C                   eval      wRigaPrt = 'DIF - Località ' + cplCAP +
018300090608     C                                        ' - '             + cplLOC +
018400090608     C                             ' tipo tempo (' + wSavTTC + ') "NO/SI"'
018500090608     C                   except    staLista
018600090608     C                   endif
018700090608     C*
018800090608     C                   endsl
018900090608     C*
019000090608     C                   endif
019100061127     C*
019200061204     C* Proseguo con la lettura del file AZCPL (solo versione interessata)
019300061204     C     wVERS         reade     azcpl01l
019400061127     C                   enddo
019500061127     C*
019600061127     C* Chiudo il printer file
019700061127     C                   close     PRTF198
019800090608     C                   close     QSYSPRT
019900061127     C*
020000061127     C                   ENDSR
020100090608     C*------------------------------------------------------------------------*
020200090608     C* IMPCPC - IMPORTA I DATI DEL CAPPARIO
020300090608     C*------------------------------------------------------------------------*
020400090608     C     IMPCPC        BEGSR
020500090608     C*
020600090608     C                   MOVEL     'L'           cplLIV
020700090608     C                   MOVEL     CPCLNA        cplLNA
020800090608     C                   MOVEL     CPCLOL        cplLOL
020900090608     C                   MOVEL     CPCLO2        cplLO2
021000090608     C                   MOVEL     CPCLOS        cplLOS
021100090608     C                   MOVEL     CPCZNC        cplZNC
021200090608     C                   MOVEL     CPCZOL        cplZOL
021300090608     C                   MOVEL     CPCZO2        cplZO2
021400090608     C                   MOVEL     CPCZOS        cplZOS
021500090608     C                   MOVEL     CPCZFD        cplZFD
021600090608     C                   Z-ADD     CPCLKG        cplLKG
021700090608     C                   Z-ADD     CPCLMC        cplLMC
021800090608     C                   Z-ADD     CPCLKS        cplLKS
021900090608     C                   Z-ADD     CPCLMS        cplLMS
022000090608     C                   Z-ADD     CPCLK2        cplLK2
022100090608     C                   Z-ADD     CPCLM2        cplLM2
022200090608     C                   MOVEL     CPCCTS        cplCTS
022300090608     C                   MOVEL     CPCCTE        cplCTE
022400090608     C                   MOVEL     CPCISO        cplISO
022500090608     C                   MOVEL     CPCPII        cplPII
022600090608     C                   MOVEL     CPCPID        cplPID
022700090608     C                   MOVEL     CPCPIS        cplPIS
022800090608     C                   MOVEL     CPCGDD        cplGDD
022900090608     C                   MOVEL     wTTC          cplTTC
023000090622     C                   MOVE      CPCFI1        cplFI1
023100090622     C                   MOVE      CPCFI2        cplFI2
023200090608     C*
023300090608     C***                eval      wRigaPrt = 'UDP - Località ' + cplCAP +
023400090608     C***                                     ' - '             + cplLOC +
023500090608     C***                               ' promossa a livello L'
023600090608     C***                except    staLista
023700090608     C*
023800090608     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
023900090608     C                   Z-ADD     wVERS         D81VER
024000090608     C                   MOVEL     cplNAR        D81NAR
024100090608     C                   MOVEL     cplCAP        D81CAP
024200090608     C                   MOVEL     'L'           D81CHK
024300090608     C   55              CALL      'TISI81R'
024400090608     C                   PARM                    tisi81ds
024500090608     C*
024600090608     C                   ENDSR
024700090608     C*------------------------------------------------------------------------*
024800090608     C* UPDCPL - AGGIORNAMENTO LOCALITA'
024900090608     C*------------------------------------------------------------------------*
025000090608     C     UPDCPL        BEGSR
025100090608     C*
025200090608     C                   eval      cplFTR = *blanks
025300090608     C                   eval      cplDTR = 11111111
025400090608     C   55              update    azcpl000
025500090608     C*
025600090608     C                   ENDSR
025700061127     C*------------------------------------------------------------------------*
025800090608     OPRTF198   E            staLista    1
025900090608     O                       wRigaPrt            +1
026000090608     OQSYSPRT   E            staAgg      1
026100090608     O                       wRigaPrt            +1
