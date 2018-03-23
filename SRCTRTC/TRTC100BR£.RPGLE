000100960530     H DECEDIT('0,') DATEDIT(*DMY/)
000200050901     F*                                                                     *
000300050901     C*? PARAMETRI:
000400050901     F*       obbligatorio INPUT-->  sndrcv   = S(snd) o R(rcv)             *
000500050901     F*           Snd Rcv--Out       tiporcv  = identifica SUM/STA          *
000600050901     F*               Snd--Out       clisnd   = codice cliente              *
000700050901     F*               Snd--Out       sigsnd   = sigla cliente               *
000800050901     C*?
000900050901     F*---------------------------------------------------------------------*
001000050831     Fwfrcv00w  if   e             disk    usropn
001100050831     Fwfsnd00w  if   e             disk    usropn
001200050831     fedtab01l  if   e           k disk
001300960530     D*---------------------------------------------------------------------*
001400050831     D EDTT00DS      E DS
001500050829     D WDAT            DS          1950
001600050829     D  SUMPRG                 1      4  0
001700050829     D  SUMTPR                 5      8
001800050831
001900050831      *parametri
002000050831     D  sndrcv         s              1
002100050831     D  tiporcv        s              4
002200050831     D  clisnd         s              7
002300050831     D  sigsnd         s              2
002400050831     D CPT             S             35    DIM(100)
002500050831     D CPTparz         S             35    DIM(100)
002600050831     D DPT             S             90    DIM(100)
002700050831     D DPU             S             90    DIM(100)
002800050831     D EDIDSPT       E DS
002900050831     D EDIDSPU       E DS
003000050831     D W035A           s             35
003100050831      *---------------------------------------------------------------------------
003200050831     C*? Seleziona il Flat File
003300050831     c                   if        sndrcv = 'S'
003400050901     c                   open      wfsnd00w
003500050831     c                   exsr      filesnd
003600050831     c                   else
003700050901     c                   open      wfrcv00w
003800050831     c                   exsr      filercv
003900050831     c                   endif
004000050831
004100050831     C                   SETON                                        LR
004200050831     C*------------------------------------------------------------
004300050831     c     filesnd       begsr
004400050831     C*------------------------------------------------------------
004500050831     C                   READ      wfsnd00w
004600050831     C                   If        not %EOF(wfsnd00w)
004700050901     c                   movel     flatrec       wdat
004800050831     c                   movel     flatrec       edtt00ds
004900050831     c                   exsr      repcli
005000050831     c                   endif
005100050901     c                   move      sumtpr        tiporcv
005200050831     c                   endsr
005300050831     C*
005400050831     C*------------------------------------------------------------
005500050831     c     filercv       begsr
005600050831     C*------------------------------------------------------------
005700050829     C                   READ      wfrcv00w
005800050829     C                   If        not %EOF(wfrcv00w)
005900050829     c                   movel     flatrec       wdat
006000050829     c                   endif
006100050901     c                   move      sumtpr        tiporcv
006200050831     c                   endsr
006300050831     C*------------------------------------------------------*
006400050831      * Cerca il codice del Cliente/Partner da Tab PT
006500050831     C*------------------------------------------------------*
006600050831     C     repcli        BEGSR
006700050831      *
006800050831      * Prova con il codice + qualificatore
006900050831     C                   MOVEL     TA0010        WCOD             35
007000050831     C                   MOVE      TAA007        WCOD
007100050831     C                   Z-ADD     1             XX                3 0
007200050831     C     WCOD          LOOKUP    CPT(XX)                                32
007300050831      *
007400050831      * se non l'ha trovato prova parzialmente senza il qualificatore
007500050831     C                   If        *IN32 = *off
007600050831     C                   MOVEL     ta0010        WCOD
007700050831     C                   eval      XX = 1
007800050831     C                   clear                   W035A
007900050831     C                   eval      W035A = %subst(WCod:1:31)
008000050831     C     W035A         lookup    CPTparz(XX)                            32
008100050831     C                   Endif
008200050831      *
008300050831      * Se ha trovato la tabella prende il codice del cliente e lo passa
008400050831      *  nel campo generico di INET
008500050831     C                   If        *IN32 = *on
008600050831     c                   movel     DPT(XX)       edidspt
008700050831     c                   movel     DPu(XX)       edidspu
008800050831     c                   move      *zeros        Clisnd
008900050831     c                   move      §PtKSC        Clisnd
009000050831     c                   move      §Pusig        sigsnd
009100050831     C                   Endif
009200050831      *
009300050831     C                   ENDSR
009400960530     C*
009500960530     C*----------------------------------------------------------------
009600960530     C*  *INZSR - OPERAZIONI INIZIALI
009700960530     C*----------------------------------------------------------------
009800960530     C     *INZSR        BEGSR
009900960530     C*
010000960530     C     *ENTRY        PLIST
010100050831     C                   PARM                    sndrcv
010200050831     C                   PARM                    tiporcv
010300050901     C                   PARM                    clisnd
010400050901     C                   PARM                    sigsnd
010500050901     c                   clear                   tiporcv
010600050901     c                   clear                   clisnd
010700050901     c                   clear                   sigsnd
010800050831      * ----------------
010900050831      * Caricamento Tabella Partner esteri
011000050831     C                   Z-ADD     0             X                 3 0
011100050831     C                   MOVEL     'PT'          TABCOD
011200050831     C     TABCOD        CHAIN     EDTAB01L                           30
011300050831     C     *IN30         DOWEQ     '0'
011400050831     C     TABFLG        ifeq      *BLANKS
011500050831     C                   ADD       1             X
011600050831     C                   MOVEL     TABKEY        CPT(X)
011700050831     C                   eval      CPTparz(X) = %subst(TABKEY:1:30)
011800050831     C                   MOVEL     TABUNI        DPT(X)
011900050831     C                   end
012000050831     C     TABCOD        READE     EDTAB01L                               30
012100050831     C                   END
012200050901      * tab/PU
012300050901     C                   Z-ADD     0             X                 3 0
012400050901     C                   MOVEL     'PU'          TABCOD
012500050901     C     TABCOD        CHAIN     EDTAB01L                           30
012600050901     C     *IN30         DOWEQ     '0'
012700050901     C     TABFLG        ifeq      *BLANKS
012800050901     C                   ADD       1             X
012900050901     C                   MOVEL     TABUNI        DPu(X)
013000050901     C                   end
013100050901     C     TABCOD        READE     EDTAB01L                               30
013200050901     C                   END
013300050901      *
013400960530     C                   ENDSR
