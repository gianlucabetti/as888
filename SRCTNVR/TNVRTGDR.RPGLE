000100020722     H/TITLE  tarscodifica tabella TABEL00F a tabella TNTBE00F
000200020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300951009?     *--------------------------------------------------------------*
000400951009      * KC - FINE LAVORO                                             *
000500951009      * KF - CONFERMA                                                *
000600951009?     *--------------------------------------------------------------*
000700161122     Ftnvrtgdd  CF   E             WORKSTN
000800161122     d
000900161122     D param           DS
001000161122     D  datada                 1      8  0
001100161122     D  dataa                  9     16  0
001200161122     D  fil                   17     19  0
001300161122     D  priv                  20     20
001400161122     D  alert                 21     21
001500161122     d* mic="S"  al posto del primo tentativo consegna controllo evento MIC con la r.a.
001600161122     d* MIC= Y   elaboro solo r.a. preima del primo MIC
001700161122     D  MIC                   22     22
001800161122     D  fasciasn              23     23
001900161122     d
002000020411     D*---------------------------------------------------------------*
002100020411     D* CAMPI INTERNI
002200020411     D*---------------------------------------------------------------*
002300951009     D KPJBA         E DS
002400951009?     *--------------------------------------------------------------*
002500020405      *
002600161122     c                   clear                   param
002700161122     c     leggi         tag
002800161122     C                   EXFMT     VRTGDD01
002900020415      *  Fine Lavoro
003000020415     C     *INKC         IFEQ      '1'
003100161122     c                   goto      fine
003200020415     C                   END
003300951009      *  Controlli
003400951009     C                   EXSR      CTR01
003500161122     c  nKF
003600161122     Cor 90              GOTO      leggi
003700161122     c
003800020408      *  CONFERMA
003900951013     C     *INKF         IFEQ      '1'
004000161122     c
004100161122     c                   if        sel1<>' ' or sel2<>' ' or sel3<>' '
004200161122     c                   select
004300161122     c                   when      sel1='S'
004400161122     c                   eval      mic='S'
004500161122     c                   when      sel2='S'
004600161122     c                   eval      mic='Y'
004700161122     c                   other
004800161122     c                   eval      mic=' '
004900161122     c                   endsl
005000161122     c
005100161122     C                   MOVEL(p)  param         KPJBU
005200161122     c                   call      'TNVRTGD4R'
005300020419     c                   parm                    kpjba
005400161122     c                   endif
005500161122     c
005600161122     c                   if        elaavv='S'
005700161122     C                   MOVEL(p)  param         KPJBU
005800161122     c                   call      'TNVRTGD5R'
005900161122     c                   parm                    kpjba
006000161122     c                   endif
006100161124     c                   if        elaric='S'
006200161124     C                   MOVEL(p)  param         KPJBU
006300161124     c                   call      'TNVRTGD6R'
006400161124     c                   parm                    kpjba
006500161124     c                   endif
006600161122     c                   endif
006700951009      *
006800161122     C     FINe          tag
006900161122     c                   seton                                        lr
007000951009?     *--------------------------------------------------------------*
007100951009?     *  CTR01: Controlli prima videata                              *
007200951009?     *--------------------------------------------------------------*
007300951009     C     CTR01         BEGSR
007400161122     c                   setoff                                       90
007500951010      *
007600020419     C                   ENDSR
007700951011?     *--------------------------------------------------------------*
007800951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
007900951011?     *--------------------------------------------------------------*
008000951011     C     *INZSR        BEGSR
008100951011      *
008200951011     C     *ENTRY        PLIST
008300951011     C                   PARM                    KPJBA
008400951012     C                   ENDSR
