000100060801      *===============================================================*
000200061127     H decedit('0,') datedit(*dmy/)
000300061127     F*------------------------------------------------------------------------*
000400061127     F* DATA BASE
000500061127     F*------------------------------------------------------------------------*
000600080331     Fazcpl01l  IF   E           K DISK
000700061127     FPRTF198   O    F  220        PRINTER usropn
000800061127     D*-------------------
000900080331     D* VARIABILI D WRK
001000061127     D*-------------------
001100080331     D P_In_VerAlfa    s              5
001200080331     D P_In_Simula     s              1
001300080331     D W_VerNum        s              5s 0 inz
001400080331     D W_CplNar        s              3    inz
001500080331     D W_CplCap        s              9    inz
001600080331     D W_CplLoc        s             35    inz
001700080331     D W_CplLiv        s              1    inz
001800080331     D W_CplIso        s              1    inz
001900080331     D W_CplEss        s              1    inz
002000080331     D W_CpsNar        s              3    inz
002100080331     D W_CpsCap        s              9    inz
002200080331     D W_CpsLoc        s             35    inz
002300080331     D W_CpsSin        s             35    inz
002400080331     D azcpl_ds      e ds                  extname(azcpl00f)
002500061127     C*------------------------------------------------------------------------*
002600061127     C* MAIN LINE
002700061127     C*------------------------------------------------------------------------*
002800080331     C*
002900080331     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
003000080331     C
003100080331     C/EXEC SQL
003200080331     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
003300080331     C/END-EXEC
003400061127     C*
003500061127     C     *Entry        plist
003600080331     C                   parm                    P_In_VerAlfa
003700080331     C                   parm                    P_In_Simula
003800061204     C*
003900080331     C                   move(p)   P_In_VerAlfa  W_VerNum
004000061204     C*
004100061204     C* Verifico se lancio in aggiornamento
004200080331     C                   if        P_In_Simula = 'A'
004300061204     C                   seton                                        55
004400061204     C                   else
004500061204     C                   setoff                                       55
004600061204     C                   endif
004700080331     C*
004800080331     C* Definizione chiavi di lettura
004900080331     C*
005000080331     C* AZCPC01L - Completa
005100080331     C     KEYcpc01_C    KLIST
005200080331     C                   KFLD                    W_VerNum
005300080331     C                   KFLD                    W_CpsNar
005400080331     C                   KFLD                    W_CpsCap
005500080331     C                   KFLD                    W_CpsLoc
005600061204     C*
005700061204     C* Elabora
005800061127     C                   exsr      Procedi
005900061127     C*
006000061127     C* Fine
006100061127     C                   eval      *INLR = *on
006200061127     C*--------------------------------------------------------------------------------------------*
006300061127     C* Procedi - Routine iter di elaborazinoe
006400061127     C*--------------------------------------------------------------------------------------------*
006500061127     C     Procedi       BEGSR
006600061127     C*
006700061204     C* Sistema tipo inoltro e codici tempo sulle località
006800061204     C                   exsr      exeLoc
006900061127     C*
007000061127     C                   ENDSR
007100061127     C*--------------------------------------------------------------------------------------------*
007200080331     C* exeLoc - Scorre tutto il file località della versione richiesta nei parametri in ingresso
007300061127     C*--------------------------------------------------------------------------------------------*
007400061204     C     exeLoc        BEGSR
007500080331     C*
007600080331     C* Apro il printer file
007700080331     C                   open      PRTF198
007800080331     C*
007900080331     C*
008000080331     C/EXEC SQL
008100080331     C+ declare  C_Loc cursor for
008200080331     C+ select   cplNAR, cplCAP, cplLOC, cplLIV, cplISO, cplESS
008300080331     C+ from     azcpl01l
008400080331     C+ where    cplVER = :W_VerNum and
008500080331     C+          cplNAR = '   '     and
008600080331     C+          cplFI1 = 'O'
008700080331     C+ order by cplCAP, cplLOC
008800080331     C+ for update of cplATB, cplDTR
008900080331     C/END-EXEC
009000080331     C
009100080331     C/EXEC SQL
009200080331     C+ open C_Loc
009300080331     C/END-EXEC
009400080331     C*
009500080331     C* Scorro tutto il result-set
009600080331     C                   dou       sqlcod = 100
009700080331     C
009800080331     C/EXEC SQL
009900080331     C+ Fetch next from C_Loc into :W_CplNar, :W_CplCap, :W_CplLoc,
010000080331     C+                            :W_CplLiv, :W_CplIso, :W_CplEss
010100080331     C/END-EXEC
010200080331     C                   if        sqlcod > *zeros
010300080331     C                   leave
010400080331     C                   else
010500080331     C                   if        sqlcod = *zeros
010600080331     C*
010700080331     C* Lancio SQL x verifica esistenza localitaà corrente come identico sinonimo x stesso cap
010800080331     C                   setoff                                       60
010900080331     C                   exsr      repSin
011000080331     C                   if        *in60 = *on
011100080331     C*
011200080331     C* reperisco i dati della località del sinonimo identico alla località obsoleta corrente
011300080331     C                   exsr      repLoc
011400080331     C*
011500080331     C* Listo x verifica
011600080331     C                   except    staLista
011700080331     C*
011800080331     C* Se richeisto esplicitamente anche aggiornamento => eseguo
011900080331     C                   if        *in55 = *on
012000080331     C*
012100080331     C* Se la località obsoleta ha sinonimi => nn aggiorno
012200080331     C                   if        W_CplEss <> 'S'
012300080331     C
012400080331     C/EXEC SQL
012500080331     C+     update azcpl01l
012600080331     C+     set    cplATB = 'A', cplDTR = 77777777
012700080331     C+     where  CURRENT  OF  C_Loc
012800080331     C/END-EXEC
012900080331     C*
013000080331     C                   endif
013100080331     C                   endif
013200080331     C*
013300080331     C                   endif
013400080331     C                   endif
013500080331     C                   endif
013600080331     C*
013700080331     C                   enddo
013800080331     C*
013900080331     C/EXEC SQL
014000080331     C+ close C_Loc
014100080331     C/END-EXEC
014200080331     C*
014300061127     C*
014400061127     C* Chiudo il printer file
014500061127     C                   close     PRTF198
014600061127     C*
014700061127     C                   ENDSR
014800080331     C*--------------------------------------------------------------------------------------------*
014900080331     C* repSin - gestione sinonimi
015000080331     C*--------------------------------------------------------------------------------------------*
015100080331     C     repSin        BEGSR
015200080331     C*
015300080331     C*
015400080331     C/EXEC SQL
015500080331     C+ declare C_Sin cursor for
015600080331     C+ select cpsNAR, cpsCAP, cpsLOC, cpsSIN from azcps01l
015700080331     C+ where cpsVER = :W_verNum and
015800080331     C+       cpsNAR = :W_CplNar and
015900080331     C+       cpsCAP = :W_CplCap and
016000080331     C+       cpsSIN = :W_CplLoc and
016100080331     C+       cpsATB = ' '
016200080331     C/END-EXEC
016300080331     C
016400080331     C/EXEC SQL
016500080331     C+ open C_Sin
016600080331     C/END-EXEC
016700080331     C
016800080331     C/EXEC SQL
016900080331     C+ Fetch next from C_Sin into :W_CpsNar, :W_CpsCap, :W_CpsLoc, :W_CpsSin
017000080331     C/END-EXEC
017100080331     C*
017200080331     C* Verifico se SQL ok e verifico se reperito qualcosa
017300080331     C                   if        sqlcod = *zeros
017400080331     C                   seton                                        60
017500080331     C                   endif
017600080331     C*
017700080331     C/EXEC SQL
017800080331     C+ close C_Sin
017900080331     C/END-EXEC
018000080331     C*
018100080331     C*
018200080331     C                   ENDSR
018300080331     C*--------------------------------------------------------------------------------------------*
018400080331     C* repLoc - Reperisce i dati della località del sinonimo corrente
018500080331     C*--------------------------------------------------------------------------------------------*
018600080331     C     repLoc        BEGSR
018700080331     C*
018800080331     C                   reset                   azcpl_ds
018900080331     C     KEYcpc01_C    chain     azcpl01l
019000080331     C*
019100080331     C                   ENDSR
019200061127     C*------------------------------------------------------------------------*
019300080331     OPRTF198   E            staLista    1
019400080331     O                                           +2 'Loc Obs:'
019500080331     O                       W_CplCap            +2
019600080331     O                       W_CplLoc            +2
019700080331     O                                          +10 'Sin Loc:'
019800080331     O                       W_CpsCap            +2
019900080331     O                       W_CpsLoc            +2
020000080331     O                       W_CpsSin            +2
020100080331     O                                           +2 'Ino:'
020200080331     O                       W_CplIso            +1
020300080331     O                                           +1 '/'
020400080331     O                       cplISO              +1
020500080331     O                                           +2 'Liv:'
020600080331     O                       W_CplLiv            +1
020700080331     O                                           +1 '/'
020800080331     O                       cplLIV              +1
020900080331     O                       W_CplEss            +5
