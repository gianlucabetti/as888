000100060801      *===============================================================*
000200061127     H decedit('0,') datedit(*dmy/)
000300061127     F*------------------------------------------------------------------------*
000400061127     F* DATA BASE
000500061127     F*------------------------------------------------------------------------*
000600090608     Fazcpc01l  IF   E           K DISK
000700061204     Fazcpl01l  UF   E           K DISK
000800091204     FH1030     IF   E             DISK    PREFIX(F_)
000900091204     F                                     RENAME(H1030:H103000)
001000091204     FPRTF198   O    F  198        PRINTER usropn
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
003700091204     C                   KFLD                    wVERS
003800091204     C                   KFLD                    cplNAR
003900091204     C                   KFLD                    cplCAP
004000091204     C*
004100091204     C* AZCPL01L - Completa
004200091204     C     KEYcpl01_C    KLIST
004300091204     C                   KFLD                    wVERS
004400091204     C                   KFLD                    cplNAR
004500091204     C                   KFLD                    cplCAP
004600091204     C                   KFLD                    cplLOC
004700061204     C*
004800061204     C* Elabora
004900061127     C                   exsr      Procedi
005000061127     C*
005100061127     C* Fine
005200061127     C                   eval      *INLR = *on
005300061127     C*--------------------------------------------------------------------------------------------*
005400061127     C* Procedi - Routine iter di elaborazinoe
005500061127     C*--------------------------------------------------------------------------------------------*
005600061127     C     Procedi       BEGSR
005700061127     C*
005800090608     C* Imposta nuovi codici tempo sulle località
005900061204     C                   exsr      exeLoc
006000061127     C*
006100061127     C                   ENDSR
006200061127     C*--------------------------------------------------------------------------------------------*
006300090608     C* exeLoc - Imposta nuovi codici tempo sulle località
006400061127     C*--------------------------------------------------------------------------------------------*
006500061204     C     exeLoc        BEGSR
006600091204     C*
006700091204     C* Inizializzazioi contatori d wrk
006800091204     C                   z-add     *zeros        wContaUPD         6 0
006900091204     C                   z-add     *zeros        wContaERR         6 0
007000091204     C                   z-add     *zeros        wContaTOT         6 0
007100061127     C*
007200061127     C* Apro il printer file
007300061127     C                   open      PRTF198
007400090610     C*
007500090610     C                   if        IN_ELA = 'A'
007600090610     C                   eval      wRigaPrt='* Elaborazione in AGGIORNAMENTO *'
007700090610     C                   else
007800090610     C                   eval      wRigaPrt='* Elaborazione in SIMULAZIONE *'
007900090610     C                   endif
008000091204     C                   except    staLista
008100090610     C                   eval      wRigaPrt=''
008200090610     C                   except    staLista
008300091204     C*
008400091204     C* Scorro tutto il file H1030
008500091204     C     *start        setll     H1030
008600091204     C                   read      H1030
008700091204     C                   dow       not %eof(H1030)
008800091204     C*
008900091204     C* Elaboro solo le località con servizio H1030 previsto
009000091204     C                   if        %subst(F_H1030:53:2) = 'SI'
009100091204     C                   add       1             wContaTOT
009200091204     C*
009300091204     C* Inizializzo riga d stampa
009400091204     C                   movel     *blanks       wRigaPrt        150
009500091204     C*
009600091204     C                   eval      cplNAR = *blanks
009700091204     C                   eval      cplCAP = %subst(F_H1030:5:5)
009800091204     C                   eval      cplLOC = %subst(F_H1030:11:35)
009900061127     C*
010000091204     C* Agggancio il file località
010100091204     C     KEYcpl01_C    chain     azcpl01l
010200091204     C                   if        not %found(azcpl01l)
010300091204     C*
010400091204     C* Se località inesistente => listo anomalia
010500091204     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' ' +
010600091204     C                               'non trovata su file AZCPL vers. '   +
010700091204     C                             IN_VER
010800091204     C                   except    staLista
010900091204     C                   add       1             wContaERR
011000091204     C*
011100091204     C                   else
011200090608     C*
011300091204     C* Verifico il livello d gestione
011400091204     C                   if        cplLIV =  'L'
011500091204     C*
011600091204     C                   select
011700091204     C                   when      cplTTC = 3
011800091204     C                   eval      cplTTC = 1003
011900091204     C                   add       1             wContaUPD
012000091204     C                   when      cplTTC = 4
012100091204     C                   eval      cplTTC = 1004
012200091204     C                   add       1             wContaUPD
012300091204     C                   when      cplTTC = 6
012400091204     C                   eval      cplTTC = 1006
012500091204     C                   add       1             wContaUPD
012600091204     C                   other
012700091204     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' '   +
012800091204     C                               'codice tipo tempo <> 3/4/6 - (Liv L)' +
012900091204     C                               %editc(cplTTC:'X')
013000091204     C                   except    staLista
013100091204     C                   add       1             wContaERR
013200091204     C                   endsl
013300091204     C*
013400091204     C                   else
013500091204     C*
013600090608     C     KEYcpc01_C    chain     AZCPC01L
013700090608     C                   if        %found(azcpc01l)
013800091204     C*
013900091204     C                   select
014000091204     C                   when      cpcTTC = 3
014100091204     C                   eval      cplTTC = 1003
014200091204     C                   exsr      impcpc
014300091204     C                   add       1             wContaUPD
014400091204     C                   when      cpcTTC = 4
014500091204     C                   eval      cplTTC = 1004
014600091204     C                   exsr      impcpc
014700091204     C                   add       1             wContaUPD
014800091204     C                   when      cpcTTC = 6
014900091204     C                   eval      cplTTC = 1006
015000091204     C                   exsr      impcpc
015100091204     C                   add       1             wContaUPD
015200091204     C                   other
015300091204     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' '  +
015400091204     C                               'codice tipo tempo <> 3/4/6 - (Liv C)'+
015500091204     C                               %editc(cpcTTC:'X')
015600091204     C                   except    staLista
015700091204     C                   add       1             wContaERR
015800091204     C                   endsl
015900091204     C*
016000091204     C                   endif
016100091204     C                   endif
016200091204     C                   endif
016300091204     C*
016400091204     C* Quindi aggiorno i dati
016500091204     C                   exsr      updcpl
016600091204     C*
016700091204     C                   endif
016800061127     C*
016900091204     C* Proseguo con la lettura del file H1030
017000091204     C                   read      H1030
017100061127     C                   enddo
017200091204     C*
017300091204     C* Stampa totali finali
017400091204     C                   eval      wRigaPrt = *blanks
017500091204     C                   except    staLista
017600091204     C                   eval      wRigaPrt = 'TOTALI LOCALITA AGGIORNATE: '+
017700091204     C                               %editc(wContaUPD:'X')
017800091204     C                   except    staLista
017900091204     C                   eval      wRigaPrt = 'TOTALI ERRORI             : '+
018000091204     C                               %editc(wContaERR:'X')
018100091204     C                   except    staLista
018200091204     C                   eval      wRigaPrt = 'TOTALI LOCALITA "SI H1030": '+
018300091204     C                               %editc(wContaTOT:'X')
018400091204     C                   except    staLista
018500061127     C*
018600061127     C* Chiudo il printer file
018700061127     C                   close     PRTF198
018800061127     C*
018900061127     C                   ENDSR
019000090608     C*------------------------------------------------------------------------*
019100090608     C* IMPCPC - IMPORTA I DATI DEL CAPPARIO
019200090608     C*------------------------------------------------------------------------*
019300090608     C     IMPCPC        BEGSR
019400090608     C*
019500090608     C                   MOVEL     'L'           cplLIV
019600090608     C                   MOVEL     CPCLNA        cplLNA
019700090608     C                   MOVEL     CPCLOL        cplLOL
019800090608     C                   MOVEL     CPCLO2        cplLO2
019900090608     C                   MOVEL     CPCLOS        cplLOS
020000090608     C                   MOVEL     CPCZNC        cplZNC
020100090608     C                   MOVEL     CPCZOL        cplZOL
020200090608     C                   MOVEL     CPCZO2        cplZO2
020300090608     C                   MOVEL     CPCZOS        cplZOS
020400090608     C                   MOVEL     CPCZFD        cplZFD
020500090608     C                   Z-ADD     CPCLKG        cplLKG
020600090608     C                   Z-ADD     CPCLMC        cplLMC
020700090608     C                   Z-ADD     CPCLKS        cplLKS
020800090608     C                   Z-ADD     CPCLMS        cplLMS
020900090608     C                   Z-ADD     CPCLK2        cplLK2
021000090608     C                   Z-ADD     CPCLM2        cplLM2
021100090608     C                   MOVEL     CPCCTS        cplCTS
021200090608     C                   MOVEL     CPCCTE        cplCTE
021300090608     C                   MOVEL     CPCISO        cplISO
021400090608     C                   MOVEL     CPCPII        cplPII
021500090608     C                   MOVEL     CPCPID        cplPID
021600090608     C                   MOVEL     CPCPIS        cplPIS
021700090608     C                   MOVEL     CPCGDD        cplGDD
021800090622     C                   MOVE      CPCFI1        cplFI1
021900090622     C                   MOVE      CPCFI2        cplFI2
022000090608     C*
022100091204     C***                eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' ' +
022200091204     C***                            'promossa a livello L'
022300091204     C***                except    staLista
022400090608     C*
022500090608     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
022600090608     C                   Z-ADD     wVERS         D81VER
022700090608     C                   MOVEL     cplNAR        D81NAR
022800090608     C                   MOVEL     cplCAP        D81CAP
022900090608     C                   MOVEL     'L'           D81CHK
023000090608     C   55              CALL      'TISI81R'
023100090608     C                   PARM                    tisi81ds
023200090608     C*
023300090608     C                   ENDSR
023400090608     C*------------------------------------------------------------------------*
023500090608     C* UPDCPL - AGGIORNAMENTO LOCALITA'
023600090608     C*------------------------------------------------------------------------*
023700090608     C     UPDCPL        BEGSR
023800090608     C*
023900090608     C                   eval      cplFTR = *blanks
024000091204     C                   eval      cplDTR = 22222222
024100090608     C   55              update    azcpl000
024200090608     C*
024300090608     C                   ENDSR
024400061127     C*------------------------------------------------------------------------*
024500090608     OPRTF198   E            staLista    1
024600090608     O                       wRigaPrt            +1
