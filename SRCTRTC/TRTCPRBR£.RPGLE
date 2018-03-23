000100080219     h decedit('0,') option(*nodebugio)
000200080219      *--------------------------------------------------------------------------------------------*
000300080219      * Pgm autostartjob che per ogni p.o. valido sottomette                                       *
000400080219      * un JOB per la ricezione dati da PDA - ritiri                                               *
000500080219      *--------------------------------------------------------------------------------------------*
000600080219     fazorg01l  if   e           k disk
000700080219     fazute01l  if   e           k disk
000800080219     ftntbe01l  if   e           k disk
000900080219
001000080219     d parpo           s              3
001100080219     d profilo         s             10
001200080219     d nomejob         s             10
001300080219     d WCurlib         s             10
001400080219     d WCurlibDFT      s             10    inz('*CRTDFT')
001500080219     d comman          S            100                                         *comando CLP
001600080219     d lenght          S             15  5 INZ(100)                             *lunghezza comando
001700080219     d kcod            s              3    inz('CUR')
001800080219     d kke1            s             15
001900080219     d dcur          e ds
001901120110     D dataiso         s               d   datfmt(*iso)
001902120110     D dateu           s              8  0
002000080219
002100080219      *--------------------------------------------------------------------------------------------*
002200080219     c     Ktbe          klist                                                  *tntbe01l
002300080219     c                   kfld                    kcod                            -tabella
002400080219     c                   kfld                    kke1                            -chiave uno
002500080219      *--------------------------------------------------------------------------------------------*
002501120110     c                   time                    dataiso
002502120110     C                   move      dataiso       dateu
002600080219
002700080219      * elimino eventuale libreria corrente
002800080219     c                   eval      Wcurlib = Wcurlibdft
002900080219     C                   exsr      VarCurLib
003000080219     c
003100080219
003200080219     c     *loval        setll     azorg01l
003300080219     c                   do        *hival
003400080219     c                   read      azorg01l
003500080219
003600080219      * fine file
003700080219    1c                   if        %eof(azorg01l)
003800080219     c                   leave
003900080219     c                   endif
004000080219
004100080219      * escludo gli annullati e se non è Filiale o Agenzia
004200080219     c                   if        orgfva <> *blanks or
004300080219     c                             (orgfag <> 'F' and orgfag <> 'A')
004400080219     c                   iter
004500080219     c                   endif
004600080219
004700080219     c                   move      orgfil        parpo
004800080219
004900080219      * verifico esistenza profilo
005000080219     C                   eval      profilo = 'AU' + parpo
005100080219     C                   eval      nomejob = 'AU' + parpo + 'RIT'
005200080219     c     profilo       chain     azute000
005300120110     c                   if        %found and dateu<=utedsc
005400080219
005500080219      * verifico se impostare libreria personalizzazioni
005600080219     c                   clear                   dcur
005700080219     c                   movel(p)  parpo         kke1
005800080219     c     Ktbe          chain     TNTBE000
005900080219if  1c                   if             %found(TNTBE01L)
006000080219     c                             and  TBEatb = *blanks
006100080219     c                   movel     TBEuni        dcur
006200080219      *
006300080219      *  imposto libreria personalizzazioni
006400080219     c                   if        §curlib <> *blanks
006500080219     c                   eval      Wcurlib = §curlib
006600080219     C                   exsr      VarCurLib
006700080219     c                   endif
006800080219     c                   endif
006900080219
007000080219      * sottometto JOB
007100080219     c                   eval      comman =
007200080226     c                             'sbmjob cmd(call pgm(TRTCPR1C) PARM('''
007300080219     c                              + parpo + ''')) job(' + nomejob + ') -
007400080219     c                              JOBQ(WIFIBARRIT) USER(' + profilo + ')'
007500080219
007600080219     c                   call      'QCMDEXC'
007700080219     c                   parm                    comman
007800080219     c                   parm                    lenght
007900080219
008000080219     c                   endif
008100080219
008200080219      * REimposto "NESSUNA" libreria corrente
008300080219     c                   if        Wcurlib <> Wcurlibdft
008400080219     c                   eval      Wcurlib = Wcurlibdft
008500080219     C                   exsr      VarCurLib
008600080219     c                   endif
008700080219
008800080219     c                   enddo
008900080219
009000080219     c                   eval      *inlr = *on
009100080219      *--------------------------------------------------------------------------------------------*
009200080219      * elimino eventuale libreria corrente
009300080219      *--------------------------------------------------------------------------------------------*
009400080219     C     VarCurLib     begsr
009500080219      *
009600080219     c                   eval      comman =
009700080219     c                             'chgcurlib curlib(' + Wcurlib + ')'
009800080219
009900080219     c                   call      'QCMDEXC'
010000080219     c                   parm                    comman
010100080219     c                   parm                    lenght
010200080219
010300080219     c                   endsr
