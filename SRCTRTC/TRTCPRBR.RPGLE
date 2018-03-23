000100080219     h decedit('0,') option(*nodebugio)
000200080219      *--------------------------------------------------------------------------------------------*
000300080219      * Pgm autostartjob che per ogni p.o. valido sottomette                                       *
000400080219      * un JOB per la ricezione dati da PDA - ritiri                                               *
000500080219      *--------------------------------------------------------------------------------------------*
000600080219     fazorg01l  if   e           k disk
000700080219     fazute01l  if   e           k disk
000800080219
000900120521     d
001000120521     d tibs40ds      e ds
001100120521     D wfil                    9   1808  0 DIM(600)
001200120521     D wlib                 1809   7808    DIM(600)
001300120521
001400120514      *
001500120514     d XX              s              3  0
001600080219     d parpo           s              3
001700080219     d profilo         s             10
001800080219     d nomejob         s             10
001900080219     d WCurlib         s             10
002000080219     d WCurlibDFT      s             10    inz('*CRTDFT')
002100080219     d comman          S            100                                         *comando CLP
002200080219     d lenght          S             15  5 INZ(100)                             *lunghezza comando
002300120514
002400120514     d kpjba         e ds
002500120110     D dataiso         s               d   datfmt(*iso)
002600120110     D dateu           s              8  0
002700080219
002800080219      *--------------------------------------------------------------------------------------------*
002900120110     c                   time                    dataiso
003000120110     C                   move      dataiso       dateu
003100120514
003200120514     c                   exsr      routinz
003300080219
003400080219      * elimino eventuale libreria corrente
003500080219     c                   eval      Wcurlib = Wcurlibdft
003600080219     C                   exsr      VarCurLib
003700080219     c
003800080219
003900080219     c     *loval        setll     azorg01l
004000080219     c                   do        *hival
004100080219     c                   read      azorg01l
004200080219
004300080219      * fine file
004400080219    1c                   if        %eof(azorg01l)
004500080219     c                   leave
004600080219     c                   endif
004700080219
004800080219      * escludo gli annullati e se non è Filiale o Agenzia
004900080219     c                   if        orgfva <> *blanks or
005000080219     c                             (orgfag <> 'F' and orgfag <> 'A')
005100080219     c                   iter
005200080219     c                   endif
005300080219
005400080219     c                   move      orgfil        parpo
005500080219
005600080219      * verifico esistenza profilo
005700080219     C                   eval      profilo = 'AU' + parpo
005800080219     C                   eval      nomejob = 'AU' + parpo + 'RIT'
005900080219     c     profilo       chain     azute000
006000120110     c                   if        %found and dateu<=utedsc
006100080219
006200080219      * verifico se impostare libreria personalizzazioni
006300120514     c**                 clear                   dcur
006400120514     c**                 movel(p)  parpo         kke1
006500120514     c**   Ktbe          chain     TNTBE000
006600120514if  1c**                 if             %found(TNTBE01L)
006700120514     c**                           and  TBEatb = *blanks
006800120514     c**                 movel     TBEuni        dcur
006900080219      *
007000080219      *  imposto libreria personalizzazioni
007100120514     c**                 if        §curlib <> *blanks
007200120514     c**                 eval      Wcurlib = §curlib
007300120514     C**                 exsr      VarCurLib
007400120514     c**                 endif
007500120514     c**                 endif
007600120514     c
007700120514     c* ES --> cambiato uso della tabella CUR caricate le filiali in skiera
007800120514     c                   clear                   xx
007900120514     c                   eval      XX  =%lookup(orgfil:wfil)
008000120514     c                   if        xx  >0
008100120514     c                   eval      Wcurlib = wlib(XX)
008200120514     C                   exsr      VarCurLib
008300120514     c                   endif
008400080219
008500080219      * sottometto JOB
008600080219     c                   eval      comman =
008700080226     c                             'sbmjob cmd(call pgm(TRTCPR1C) PARM('''
008800080219     c                              + parpo + ''')) job(' + nomejob + ') -
008900080219     c                              JOBQ(WIFIBARRIT) USER(' + profilo + ')'
009000080219
009100080219     c                   call      'QCMDEXC'
009200080219     c                   parm                    comman
009300080219     c                   parm                    lenght
009400080219
009500080219     c                   endif
009600080219
009700080219      * REimposto "NESSUNA" libreria corrente
009800080219     c                   if        Wcurlib <> Wcurlibdft
009900080219     c                   eval      Wcurlib = Wcurlibdft
010000080219     C                   exsr      VarCurLib
010100080219     c                   endif
010200080219
010300080219     c                   enddo
010400080219
010500080219     c                   eval      *inlr = *on
010600080219      *--------------------------------------------------------------------------------------------*
010700080219      * elimino eventuale libreria corrente
010800080219      *--------------------------------------------------------------------------------------------*
010900080219     C     VarCurLib     begsr
011000080219      *
011100120514     c                   eval      lenght=100
011200080219     c                   eval      comman =
011300080219     c                             'chgcurlib curlib(' + Wcurlib + ')'
011400080219
011500080219     c                   call      'QCMDEXC'
011600080219     c                   parm                    comman
011700080219     c                   parm                    lenght
011800080219
011900080219     c                   endsr
012000120514      *--------------------------------------------------------------------------------------------*
012100120514     c     RoutInz       BEGSR
012200120514     c
012300120514     c* Carico tabella CUR per sapere le filiali che hanno la CUR in lista
012400120521     c                   clear                   tibs40ds
012500120521     c                   call      'TIBS40R'
012600120521     c                   parm                    kpjba
012700120521     c                   parm                    tibs40ds
012800120521     c
012900120514     c                   ENDSR
