000100060801      *===============================================================*
000200061127     H decedit('0,') datedit(*dmy/)
000300061127     F*------------------------------------------------------------------------*
000400061127     F* DATA BASE
000500061127     F*------------------------------------------------------------------------*
000600090609     Fazcpc01l  UF   E           K DISK
000700090609     Fazcpl01l  IF   E           K DISK
000800090609     FPRTF198   O    F  220        PRINTER usropn
000900090608     D*------------------------------------------------------------------------*
001000090609     D DS_cpl        e ds                  extname(azcpl00f) prefix(s_)
001100061127     C*------------------------------------------------------------------------*
001200061127     C* MAIN LINE
001300061127     C*------------------------------------------------------------------------*
001400061127     C*
001500061127     C     *Entry        plist
001600090608     C                   parm                    IN_VER            5
001700090609     C                   parm                    IN_ELA            1
001800061204     C*
001900061204     C* Imposto la versione cappario da trattare
002000090608     C                   move(p)   IN_VER        wVERS             5 0
002100090609     C*
002200090609     C* Verifico se lancio in aggiornamento
002300090609     C                   if        IN_ELA = 'A'
002400090609     C                   seton                                        55
002500090609     C                   else
002600090609     C                   setoff                                       55
002700090609     C                   endif
002800061204     C*
002900061204     C* Definizione chiavi di lettura
003000061204     C*
003100090608     C* AZCPC01L - Parziale
003200090608     C     KEYcpc01_P    KLIST
003300090608     C                   KFLD                    cpcVER
003400090608     C                   KFLD                    cpcNAR
003500090609     C*
003600090609     C* AZCPL01L - Parziale
003700090609     C     KEYcpl01_P    KLIST
003800090609     C                   KFLD                    cpcVER
003900090609     C                   KFLD                    cpcNAR
004000090609     C                   KFLD                    cpcCAP
004100061204     C*
004200061204     C* Elabora
004300061127     C                   exsr      Procedi
004400061127     C*
004500061127     C* Fine
004600061127     C                   eval      *INLR = *on
004700061127     C*--------------------------------------------------------------------------------------------*
004800061127     C* Procedi - Routine iter di elaborazinoe
004900061127     C*--------------------------------------------------------------------------------------------*
005000061127     C     Procedi       BEGSR
005100061127     C*
005200090608     C                   exsr      exeCap
005300061127     C*
005400061127     C                   ENDSR
005500061127     C*--------------------------------------------------------------------------------------------*
005600090608     C* exeCap
005700061127     C*--------------------------------------------------------------------------------------------*
005800090608     C     exeCap        BEGSR
005900090609     C*
006000090609     C* Apro il printer file
006100090609     C                   open      PRTF198
006200090610     C*
006300090610     C                   if        IN_ELA = 'A'
006400090610     C                   eval      wRigaPrt='* Elaborazione in AGGIORNAMENTO *'
006500090610     C                   else
006600090610     C                   eval      wRigaPrt='* Elaborazione in SIMULAZIONE *'
006700090610     C                   endif
006800090610     C                   except    staLista
006900090610     C                   eval      wRigaPrt=''
007000090610     C                   except    staLista
007100061127     C*
007200090608     C* Scorro tutto il file AZCPC01L (solo versione interessata)
007300090608     C                   eval      cpcVER = wVERS
007400090608     C                   eval      cpcNAR = *blanks
007500090609     C*
007600090608     C     KEYcpc01_P    setll     azcpc01l
007700090608     C     KEYcpc01_P    reade     azcpc01l
007800090608     C                   dow       not %eof(azcpc01l)
007900090609     C*
008000090609     C* Inizializzazioni d wrk
008100090609     C                   movel     *blanks       wRigaPrt        120
008200090609     C                   clear                   DS_cpl
008300090609     C*
008400090609     C* Effettuo verifica località relative al cap corrente
008500090609     C                   exsr      letcpl
008600061127     C*
008700090608     C* Proseguo con la lettura del file AZCPC (solo versione interessata)
008800090608     C     KEYcpc01_P    reade     azcpc01l
008900061127     C                   enddo
009000090609     C*
009100090609     C* Chiudo il printer file
009200090609     C                   close     PRTF198
009300061127     C*
009400061127     C                   ENDSR
009500090609     C*--------------------------------------------------------------------------------------------*
009600090609     C* letcpl - legge le località del cap
009700090609     C*--------------------------------------------------------------------------------------------*
009800090609     C     letcpl        BEGSR
009900090609     c*
010000090609     c* imposta a NO flag località tutte 'L' e tutte UGUALI
010100090609     c                   MOVEL     'N'           wFlag             1
010200090609     c*
010300090609     c                   Z-ADD     0             wConta            7 0
010400090609     c*
010500090609     c     KEYcpl01_P    SETLL     azcpl01l
010600090609     c                   IF        %equal(azcpl01l)
010700090609     c                   MOVEL     'S'           wFlag
010800090609     c                   ENDIF
010900090609     c*
011000090609     c     KEYcpl01_P    READE     azcpl01l
011100090609     c                   DOW       NOT %eof(azcpl01l)
011200090609     c                   EVAL      wConta = wConta + 1
011300090609     c*
011400090609     c* ignoro annullate
011500090609     c                   IF        cplatb=*blanks
011600090609     c*
011700090609     c* solo se livello 'L'
011800090609     c                   IF        cplliv<>'L'
011900090609     c                   MOVEL     'N'           wFlag
012000090609     c                   LEAVE
012100090609     c                   ELSE
012200090609     c*
012300090609     c* solo se tutte uguali
012400090609     c                   IF        wConta = 1
012500090609     c                   EVAL      s_CPLTTC = CPLTTC
012600090609     c                   ELSE
012700090610     c                   IF        s_CPLTTC <> CPLTTC
012800090609     c                   MOVEL     'N'           wFlag
012900090609     c                   LEAVE
013000090609     c                   ENDIF
013100090609     c                   ENDIF
013200090609     c                   ENDIF
013300090609     c*
013400090609     c                   ENDIF
013500090609     c     KEYcpl01_P    READE     azcpl01l
013600090609     c                   ENDDO
013700090609     c*
013800090610     c                   IF        wFlag = 'S'
013900090610     c                   IF        cpcTTC <> s_cplTTC
014000090609     c                   EXSR      IMPCPL
014100090609     c                   EXSR      UPDCPC
014200090610     c                   ENDIF
014300090609     c                   ENDIF
014400090609     c*
014500090609     C                   ENDSR
014600090609     C*------------------------------------------------------------------------*
014700090609     C* IMPCPL - IMPORTA I DATI DELLE LOCALITA' SUL CAP
014800090609     C*------------------------------------------------------------------------*
014900090609     C     IMPCPL        BEGSR
015000090609     C*
015100090609     C                   MOVEL     CPLTTC        cpcTTC
015200090609     C*
015300090609     C                   eval      wRigaPrt = 'UDP - CAP ' + cpcCAP +
015400090610     C                             ' cod. tempo aggiornato da località'
015500090609     C                   except    staLista
015600090609     C*
015700090609     C                   ENDSR
015800090609     C*------------------------------------------------------------------------*
015900090609     C* UPDCPC - AGGIORNAMENTO CAP
016000090609     C*------------------------------------------------------------------------*
016100090609     C     UPDCPC        BEGSR
016200090609     C*
016300090609     C                   eval      cpcFTR = *blanks
016400090609     C                   eval      cpcDTR = 11111111
016500090609     C   55              update    azcpc000
016600090609     C*
016700090609     C                   ENDSR
016800090609     C*------------------------------------------------------------------------*
016900090609     OPRTF198   E            staLista    1
017000090609     O                       wRigaPrt            +1
