000100060801      *===============================================================*
000200061127     H decedit('0,') datedit(*dmy/)
000300061127     F*------------------------------------------------------------------------*
000400061127     F* DATA BASE
000500061127     F*------------------------------------------------------------------------*
000600061204     Fazcpl01l  UF   E           K DISK
000700061204     Fazcpc01l  IF   E           K DISK
000800061127     FPRTF198   O    F  220        PRINTER usropn
000900061127     D*-------------------
001000061127     D* DS ARCHITETTURA
001100061127     D*-------------------
001200061127     D KPJBA         e ds
001300061127     C*------------------------------------------------------------------------*
001400061127     C* MAIN LINE
001500061127     C*------------------------------------------------------------------------*
001600061127     C*
001700061127     C     *Entry        plist
001800061127     C                   parm                    KPJBA
001900061204     C*
002000061204     C* Imposto la versione cappario da trattare
002100061204     C                   z-add     26            wVERS             5 0
002200061204     C*
002300061204     C* Verifico se lancio in aggiornamento
002400061204     C                   if        %subst(KPJBU:1:1) = 'A'
002500061204     C                   seton                                        55
002600061204     C                   else
002700061204     C                   setoff                                       55
002800061204     C                   endif
002900061204     C*
003000061204     C* Definizione chiavi di lettura
003100061204     C*
003200061204     C* AZCPC01L - Completa
003300061204     C     KEYcpc01_C    KLIST
003400061204     C                   KFLD                    cplVER
003500061204     C                   KFLD                    cplNAR
003600061204     C                   KFLD                    cplCAP
003700061204     C*
003800061204     C* Elabora
003900061127     C                   exsr      Procedi
004000061127     C*
004100061127     C* Fine
004200061127     C                   eval      *INLR = *on
004300061127     C*--------------------------------------------------------------------------------------------*
004400061127     C* Procedi - Routine iter di elaborazinoe
004500061127     C*--------------------------------------------------------------------------------------------*
004600061127     C     Procedi       BEGSR
004700061127     C*
004800061204     C* Sistema tipo inoltro e codici tempo sulle località
004900061204     C                   exsr      exeLoc
005000061127     C*
005100061127     C                   ENDSR
005200061127     C*--------------------------------------------------------------------------------------------*
005300061204     C* exeLoc - Sistema tipi inoltro e codici tempo sulle località
005400061127     C*--------------------------------------------------------------------------------------------*
005500061204     C     exeLoc        BEGSR
005600061127     C*
005700061127     C* Apro il printer file
005800061127     C                   open      PRTF198
005900061127     C*
006000061204     C* Scorro tutto il file AZCPL01L (solo versione interessata)
006100061204     C     wVERS         setll     azcpl01l
006200061204     C     wVERS         reade     azcpl01l
006300061204     C                   dow       not %eof(azcpl01l)
006400061127     C*
006500061204     C* Tratto solo le località con tipo inoltro 'C' o 'P'
006600061204     C                   if        cplISO = 'C' or
006700061204     C                             cplISO = 'P'
006800061204     C*
006900061204     C* Aggancio il cap relativo
007000061204     C     KEYcpc01_C    chain     azcpc01l
007100061204     C                   if        %found(azcpc01l)
007200061204     C*
007300061204     C* Tratto solo le cap con tipo inoltro 'C' o 'P'
007400061204     C                   if        cpcISO = 'C' or
007500061204     C                             cpcISO = 'P'
007600061127     C*
007700061204     C* Verifico se presenti differenze tra località e cap relativo
007800061204     C                   setoff                                       56
007900061204     C                   if        cplISO <> cpcISO OR
008000061204     C                             cplTTC <> cpcTTC
008100061204     C                   seton                                        56
008200061204     C                   endif
008300061204     C*
008400061204     C                   if        *in56 = *on
008500061204     C*
008600061204     C* Listo x verifica
008700061204     C                   except    staUpdLoc
008800061204     C*
008900061204     C* Porto i dati dal cap alla località
009000061204     C                   eval      cplISO  = cpcISO
009100061204     C                   eval      cplTTC  = cpcTTC
009200061204     C                   eval      cplFTR  = *blanks
009300061204     C*
009400061204     C                   if        *in55     = *on
009500061204     C                   update    azcpl000
009600061127     C                   endif
009700061204     C                   endif
009800061127     C*
009900061127     C                   endif
010000061204     C*
010100061204     C                   endif
010200061204     C*
010300061204     C                   endif
010400061127     C*
010500061204     C* Proseguo con la lettura del file AZCPL (solo versione interessata)
010600061204     C     wVERS         reade     azcpl01l
010700061127     C                   enddo
010800061127     C*
010900061127     C* Chiudo il printer file
011000061127     C                   close     PRTF198
011100061127     C*
011200061127     C                   ENDSR
011300061127     C*------------------------------------------------------------------------*
011400061204     OPRTF198   E            staUpdLoc   1
011500061204     O                       cpcPRV              +1
011600061204     O                       cplCAP              +1
011700061204     O                       cplLOC              +1
011800061204     O                       cplISO              +1
011900061204     O                       cplTTC              +1
012000061204     O                       cpcISO              +5
012100061204     O                       cpcTTC              +1
