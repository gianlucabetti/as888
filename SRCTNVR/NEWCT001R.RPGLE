000100060801      *===============================================================*
000200061127     H decedit('0,') datedit(*dmy/)
000300061127     F*------------------------------------------------------------------------*
000400061127     F* DATA BASE
000500061127     F*------------------------------------------------------------------------*
000600090608     Fazcpc01l  IF   E           K DISK
000700061204     Fazcpl01l  UF   E           K DISK
000800100422     FCT20100422IF   E             DISK    PREFIX(F_)
000900100422     F                                     RENAME(CT20100422:CT)
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
005800100422     C* Imposta nuovi codici tempo sulle località - TSP "E"
005900100422     C                   exsr      exeLoc_E
006000100422     C*
006100100422     C* Imposta nuovi codici tempo sulle località - TSP "H"
006200100422     C                   exsr      exeLoc_H
006300061127     C*
006400061127     C                   ENDSR
006500061127     C*--------------------------------------------------------------------------------------------*
006600100422     C* exeLoc_E - Imposta nuovi codici tempo sulle località => TSP "E"
006700061127     C*--------------------------------------------------------------------------------------------*
006800100422     C     exeLoc_E      BEGSR
006900091204     C*
007000091204     C* Inizializzazioi contatori d wrk
007100091204     C                   z-add     *zeros        wContaUPD         6 0
007200091204     C                   z-add     *zeros        wContaERR         6 0
007300091204     C                   z-add     *zeros        wContaTOT         6 0
007400061127     C*
007500061127     C* Apro il printer file
007600061127     C                   open      PRTF198
007700090610     C*
007800090610     C                   if        IN_ELA = 'A'
007900100422     C                   eval      wRigaPrt='* Elaborazione in AGGIORNAMENTO *'+
008000100422     C                                      ' - TSP "E"'
008100090610     C                   else
008200100422     C                   eval      wRigaPrt='* Elaborazione in SIMULAZIONE *'  +
008300100422     C                                      ' - TSP "E"'
008400090610     C                   endif
008500091204     C                   except    staLista
008600090610     C                   eval      wRigaPrt=''
008700090610     C                   except    staLista
008800091204     C*
008900100422     C* Scorro tutto il file CT20100422
009000100422     C     *start        setll     CT20100422
009100100422     C                   read      CT20100422
009200100422     C                   dow       not %eof(CT20100422)
009300091204     C*
009400100422     C* Elaboro solo le località con servizio PRIORITY = SI
009500100422     C                   if        F_PTY = 'SI'
009600091204     C                   add       1             wContaTOT
009700091204     C*
009800091204     C* Inizializzo riga d stampa
009900091204     C                   movel     *blanks       wRigaPrt        150
010000091204     C*
010100091204     C                   eval      cplNAR = *blanks
010200100422     C                   eval      cplCAP = F_CAP
010300100422     C                   eval      cplLOC = F_LOC
010400061127     C*
010500091204     C* Agggancio il file località
010600091204     C     KEYcpl01_C    chain     azcpl01l
010700091204     C                   if        not %found(azcpl01l)
010800091204     C*
010900091204     C* Se località inesistente => listo anomalia
011000091204     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' ' +
011100091204     C                               'non trovata su file AZCPL vers. '   +
011200091204     C                             IN_VER
011300091204     C                   except    staLista
011400091204     C                   add       1             wContaERR
011500091204     C*
011600091204     C                   else
011700090608     C*
011800091204     C* Verifico il livello d gestione
011900091204     C                   if        cplLIV =  'L'
012000091204     C*
012100091204     C                   select
012200100422     C                   when      cplTTC = 1
012300100422     C                   eval      cplTTC = 4
012400091204     C                   add       1             wContaUPD
012500100422     C                   when      cplTTC = 11
012600100422     C                   eval      cplTTC = 3
012700091204     C                   add       1             wContaUPD
012800091204     C                   other
012900091204     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' '   +
013000100422     C                               'codice tipo tempo <> 1/11 - (Liv L)' +
013100091204     C                               %editc(cplTTC:'X')
013200091204     C                   except    staLista
013300091204     C                   add       1             wContaERR
013400091204     C                   endsl
013500091204     C*
013600091204     C                   else
013700091204     C*
013800090608     C     KEYcpc01_C    chain     AZCPC01L
013900090608     C                   if        %found(azcpc01l)
014000091204     C*
014100091204     C                   select
014200100422     C                   when      cpcTTC = 1
014300100422     C                   eval      cplTTC = 4
014400091204     C                   exsr      impcpc
014500091204     C                   add       1             wContaUPD
014600100422     C                   when      cpcTTC = 11
014700100422     C                   eval      cplTTC = 3
014800091204     C                   exsr      impcpc
014900091204     C                   add       1             wContaUPD
015000091204     C                   other
015100091204     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' '  +
015200100422     C                               'codice tipo tempo <> 1/11 - (Liv C)'+
015300091204     C                               %editc(cpcTTC:'X')
015400091204     C                   except    staLista
015500091204     C                   add       1             wContaERR
015600091204     C                   endsl
015700091204     C*
015800091204     C                   endif
015900091204     C                   endif
016000091204     C                   endif
016100091204     C*
016200091204     C* Quindi aggiorno i dati
016300091204     C                   exsr      updcpl
016400091204     C*
016500091204     C                   endif
016600061127     C*
016700100422     C* Proseguo con la lettura del file CT20100422
016800100422     C                   read      CT20100422
016900061127     C                   enddo
017000091204     C*
017100091204     C* Stampa totali finali
017200091204     C                   eval      wRigaPrt = *blanks
017300091204     C                   except    staLista
017400100422     C                   eval      wRigaPrt = 'TOTALI LOCALITA AGGIORNATE   : '+
017500091204     C                               %editc(wContaUPD:'X')
017600091204     C                   except    staLista
017700100422     C                   eval      wRigaPrt = 'TOTALI ERRORI                : '+
017800091204     C                               %editc(wContaERR:'X')
017900091204     C                   except    staLista
018000100422     C                   eval      wRigaPrt = 'TOTALI LOCALITA "SI PRIORITY": '+
018100091204     C                               %editc(wContaTOT:'X')
018200091204     C                   except    staLista
018300061127     C*
018400061127     C* Chiudo il printer file
018500061127     C                   close     PRTF198
018600061127     C*
018700061127     C                   ENDSR
018800100422     C*--------------------------------------------------------------------------------------------*
018900100422     C* exeLoc_H - Imposta nuovi codici tempo sulle località => TSP "H"
019000100422     C*--------------------------------------------------------------------------------------------*
019100100422     C     exeLoc_H      BEGSR
019200100422     C*
019300100422     C* Inizializzazioi contatori d wrk
019400100422     C                   z-add     *zeros        wContaUPD         6 0
019500100422     C                   z-add     *zeros        wContaERR         6 0
019600100422     C                   z-add     *zeros        wContaTOT         6 0
019700100422     C*
019800100422     C* Apro il printer file
019900100422     C                   open      PRTF198
020000100422     C*
020100100422     C                   if        IN_ELA = 'A'
020200100422     C                   eval      wRigaPrt='* Elaborazione in AGGIORNAMENTO *'+
020300100422     C                                      ' - TSP "H"'
020400100422     C                   else
020500100422     C                   eval      wRigaPrt='* Elaborazione in SIMULAZIONE *'  +
020600100422     C                                      ' - TSP "H"'
020700100422     C                   endif
020800100422     C                   except    staLista
020900100422     C                   eval      wRigaPrt=''
021000100422     C                   except    staLista
021100100422     C*
021200100422     C* Scorro tutto il file CT20100422
021300100422     C     *start        setll     CT20100422
021400100422     C                   read      CT20100422
021500100422     C                   dow       not %eof(CT20100422)
021600100422     C*
021700100422     C* Elaboro solo le località con servizio H1030 = SI
021800100422     C                   if        F_H1030 = 'SI'
021900100422     C                   add       1             wContaTOT
022000100422     C*
022100100422     C* Inizializzo riga d stampa
022200100422     C                   movel     *blanks       wRigaPrt        150
022300100422     C*
022400100422     C                   eval      cplNAR = *blanks
022500100422     C                   eval      cplCAP = F_CAP
022600100422     C                   eval      cplLOC = F_LOC
022700100422     C*
022800100422     C* Agggancio il file località
022900100422     C     KEYcpl01_C    chain     azcpl01l
023000100422     C                   if        not %found(azcpl01l)
023100100422     C*
023200100422     C* Se località inesistente => listo anomalia
023300100422     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' ' +
023400100422     C                               'non trovata su file AZCPL vers. '   +
023500100422     C                             IN_VER
023600100422     C                   except    staLista
023700100422     C                   add       1             wContaERR
023800100422     C*
023900100422     C                   else
024000100422     C*
024100100422     C* Verifico il livello d gestione
024200100422     C                   if        cplLIV =  'L'
024300100422     C*
024400100422     C                   select
024500100422     C                   when      cplTTC = 4
024600100422     C                   eval      cplTTC = 1004
024700100422     C                   add       1             wContaUPD
024800100422     C                   when      cplTTC = 3
024900100422     C                   eval      cplTTC = 1003
025000100422     C                   add       1             wContaUPD
025100100422     C                   other
025200100422     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' '   +
025300100423     C                               'codice tipo tempo <> 3/4 - (Liv L)' +
025400100422     C                               %editc(cplTTC:'X')
025500100422     C                   except    staLista
025600100422     C                   add       1             wContaERR
025700100422     C                   endsl
025800100422     C*
025900100422     C                   else
026000100422     C*
026100100422     C     KEYcpc01_C    chain     AZCPC01L
026200100422     C                   if        %found(azcpc01l)
026300100422     C*
026400100422     C                   select
026500100422     C                   when      cpcTTC = 4
026600100422     C                   eval      cplTTC = 1004
026700100422     C                   exsr      impcpc
026800100422     C                   add       1             wContaUPD
026900100422     C                   when      cpcTTC = 3
027000100422     C                   eval      cplTTC = 1003
027100100422     C                   exsr      impcpc
027200100422     C                   add       1             wContaUPD
027300100422     C                   other
027400100422     C                   eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' '  +
027500100423     C                               'codice tipo tempo <> 3/4 - (Liv C)'+
027600100422     C                               %editc(cpcTTC:'X')
027700100422     C                   except    staLista
027800100422     C                   add       1             wContaERR
027900100422     C                   endsl
028000100422     C*
028100100422     C                   endif
028200100422     C                   endif
028300100422     C                   endif
028400100422     C*
028500100422     C* Quindi aggiorno i dati
028600100422     C                   exsr      updcpl
028700100422     C*
028800100422     C                   endif
028900100422     C*
029000100422     C* Proseguo con la lettura del file CT20100422
029100100422     C                   read      CT20100422
029200100422     C                   enddo
029300100422     C*
029400100422     C* Stampa totali finali
029500100422     C                   eval      wRigaPrt = *blanks
029600100422     C                   except    staLista
029700100422     C                   eval      wRigaPrt = 'TOTALI LOCALITA AGGIORNATE   : '+
029800100422     C                               %editc(wContaUPD:'X')
029900100422     C                   except    staLista
030000100422     C                   eval      wRigaPrt = 'TOTALI ERRORI                : '+
030100100422     C                               %editc(wContaERR:'X')
030200100422     C                   except    staLista
030300100422     C                   eval      wRigaPrt = 'TOTALI LOCALITA "SI H1030"   : '+
030400100422     C                               %editc(wContaTOT:'X')
030500100422     C                   except    staLista
030600100422     C*
030700100422     C* Chiudo il printer file
030800100422     C                   close     PRTF198
030900100422     C*
031000100422     C                   ENDSR
031100090608     C*------------------------------------------------------------------------*
031200090608     C* IMPCPC - IMPORTA I DATI DEL CAPPARIO
031300090608     C*------------------------------------------------------------------------*
031400090608     C     IMPCPC        BEGSR
031500090608     C*
031600090608     C                   MOVEL     'L'           cplLIV
031700090608     C                   MOVEL     CPCLNA        cplLNA
031800090608     C                   MOVEL     CPCLOL        cplLOL
031900090608     C                   MOVEL     CPCLO2        cplLO2
032000090608     C                   MOVEL     CPCLOS        cplLOS
032100090608     C                   MOVEL     CPCZNC        cplZNC
032200090608     C                   MOVEL     CPCZOL        cplZOL
032300090608     C                   MOVEL     CPCZO2        cplZO2
032400090608     C                   MOVEL     CPCZOS        cplZOS
032500090608     C                   MOVEL     CPCZFD        cplZFD
032600090608     C                   Z-ADD     CPCLKG        cplLKG
032700090608     C                   Z-ADD     CPCLMC        cplLMC
032800090608     C                   Z-ADD     CPCLKS        cplLKS
032900090608     C                   Z-ADD     CPCLMS        cplLMS
033000090608     C                   Z-ADD     CPCLK2        cplLK2
033100090608     C                   Z-ADD     CPCLM2        cplLM2
033200090608     C                   MOVEL     CPCCTS        cplCTS
033300090608     C                   MOVEL     CPCCTE        cplCTE
033400090608     C                   MOVEL     CPCISO        cplISO
033500090608     C                   MOVEL     CPCPII        cplPII
033600090608     C                   MOVEL     CPCPID        cplPID
033700090608     C                   MOVEL     CPCPIS        cplPIS
033800090608     C                   MOVEL     CPCGDD        cplGDD
033900090622     C                   MOVE      CPCFI1        cplFI1
034000090622     C                   MOVE      CPCFI2        cplFI2
034100090608     C*
034200091204     C***                eval      wRigaPrt = cplCAP + ' ' + cplLOC + ' ' +
034300091204     C***                            'promossa a livello L'
034400091204     C***                except    staLista
034500090608     C*
034600090608     C* AGGIORNA IL FLAG "ESISTENZA LOCALITA'" SUL CAP
034700090608     C                   Z-ADD     wVERS         D81VER
034800090608     C                   MOVEL     cplNAR        D81NAR
034900090608     C                   MOVEL     cplCAP        D81CAP
035000090608     C                   MOVEL     'L'           D81CHK
035100090608     C   55              CALL      'TISI81R'
035200090608     C                   PARM                    tisi81ds
035300090608     C*
035400090608     C                   ENDSR
035500090608     C*------------------------------------------------------------------------*
035600090608     C* UPDCPL - AGGIORNAMENTO LOCALITA'
035700090608     C*------------------------------------------------------------------------*
035800090608     C     UPDCPL        BEGSR
035900090608     C*
036000090608     C                   eval      cplFTR = *blanks
036100091204     C                   eval      cplDTR = 22222222
036200090608     C   55              update    azcpl000
036300090608     C*
036400090608     C                   ENDSR
036500061127     C*------------------------------------------------------------------------*
036600090608     OPRTF198   E            staLista    1
036700090608     O                       wRigaPrt            +1
