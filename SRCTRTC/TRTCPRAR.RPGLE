000100080219     h decedit('0,') option(*nodebugio)
000200080219      *--------------------------------------------------------------------------------------------*
000300080219      * Pgm autostartjob che per ogni p.o. sottomette                                              *
000400080219      * un JOB per la ricezione dati da pistola radio                                              *
000500080219      *--------------------------------------------------------------------------------------------*
000600080219     fazorg01l  if   e           k disk
000700080219     fazute01l  if   e           k disk
000800120521     d
000900120521     d tibs40ds      e ds
001000120521     D wfil                    9   1808  0 DIM(600)
001100120521     D wlib                 1809   7808    DIM(600)
001200120521
001300120514      *
001400120514     d XX              s              3  0
001500080219     d parpo           s              3
001600080219     d profilo         s             10
001700080219     d nomejob         s             10
001800080219     d WCurlib         s             10
001900080219     d WCurlibDFT      s             10    inz('*CRTDFT')
002000080219     d comman          S            100                                         *comando CLP
002100080219     d lenght          S             15  5 INZ(100)                             *lunghezza comando
002200120514
002300120514     d kpjba         e ds
002400120110     D dataiso         s               d   datfmt(*iso)
002500120110     D dateu           s              8  0
002600080219
002700080219      *--------------------------------------------------------------------------------------------*
002800120110     c                   time                    dataiso
002900120110     C                   move      dataiso       dateu
003000080219
003100120514     c                   exsr      routinz
003200120514
003300080219      * elimino eventuale libreria corrente
003400080219     c                   eval      Wcurlib = Wcurlibdft
003500080219     C                   exsr      VarCurLib
003600080219     c
003700080219
003800080219     c     *loval        setll     azorg01l
003900080219     c                   do        *hival
004000080219     c                   read      azorg01l
004100080219
004200080219      * fine file
004300080219    1c                   if        %eof(azorg01l)
004400080219     c                   leave
004500080219     c                   endif
004600080219
004700080219      * escludo gli annullati e se non è Filiale o Agenzia
004800080219     c                   if        orgfva <> *blanks or
004900080219     c                             (orgfag <> 'F' and orgfag <> 'A')
005000080219     c                   iter
005100080219     c                   endif
005200080219
005300080219     c                   move      orgfil        parpo
005400080219
005500080219      * verifico esistenza profilo
005600080219     C                   eval      profilo = 'PR' + parpo
005700080219     c     profilo       chain     azute000
005800120110     c                   if        %found and dateu<=utedsc
005900080219
006000080219      * verifico se impostare libreria personalizzazioni
006100120514     c**                 clear                   dcur
006200120514     c**                 movel(p)  parpo         kke1
006300120514     c**   Ktbe          chain     TNTBE000
006400120514if  1c**                 if             %found(TNTBE01L)
006500120514     c**                           and  TBEatb = *blanks
006600120514     c**                 movel     TBEuni        dcur
006700080219      *
006800080219      *  imposto libreria personalizzazioni
006900120514     c**                 if        §curlib <> *blanks
007000120514     c**                 eval      Wcurlib = §curlib
007100120514     C**                 exsr      VarCurLib
007200120514     c**                 endif
007300120514     c**                 endif
007400120514     c
007500120514     c* ES --> cambiato uso della tabella CUR caricate le filiali in skiera
007600120514     c                   clear                   xx
007700120514     c                   eval      XX  =%lookup(orgfil:wfil)
007800120514     c                   if        xx  >0
007900120514     c                   eval      Wcurlib = wlib(XX)
008000120514     C                   exsr      VarCurLib
008100120514     c                   endif
008200120514
008300080219      * sottometto JOB
008400080219     c                   eval      comman =
008500080219     c                             'sbmjob cmd(call pgm(TRTCP8C) PARM('''
008600080219     c                              + parpo + ''')) job(' + profilo + ') -
008700080219     c                              JOBQ(WIFIPR) USER(' + profilo + ')'
008800080219
008900080219     c                   call      'QCMDEXC'
009000080219     c                   parm                    comman
009100080219     c                   parm                    lenght
009200080219
009300080219     c                   endif
009400080219
009500080219      * REimposto "NESSUNA" libreria corrente
009600080219     c                   if        Wcurlib <> Wcurlibdft
009700080219     c                   eval      Wcurlib = Wcurlibdft
009800080219     C                   exsr      VarCurLib
009900080219     c                   endif
010000080219
010100080219     c                   enddo
010200080219
010300080219     c                   eval      *inlr = *on
010400080219      *--------------------------------------------------------------------------------------------*
010500080219      * modifico libreria corrente
010600080219      *--------------------------------------------------------------------------------------------*
010700080219     C     VarCurLib     begsr
010800080219      *
010900120514     c                   eval      lenght=100
011000080219     c                   eval      comman =
011100080219     c                             'chgcurlib curlib(' + Wcurlib + ')'
011200080219
011300080219     c                   call      'QCMDEXC'
011400080219     c                   parm                    comman
011500080219     c                   parm                    lenght
011600080219
011700080219     c                   endsr
011800120514      *--------------------------------------------------------------------------------------------*
011900120514     c     RoutInz       BEGSR
012000120514     c
012100120514     c* Carico tabella CUR per sapere le filiali che hanno la CUR in lista
012200120521     c                   clear                   tibs40ds
012300120521     c                   call      'TIBS40R'
012400120521     c                   parm                    kpjba
012500120521     c                   parm                    tibs40ds
012600120521     c
012700120521     c
012800120514     c                   ENDSR
