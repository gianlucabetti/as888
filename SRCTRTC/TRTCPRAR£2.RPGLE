000600080219     h decedit('0,') option(*nodebugio)
000700080219      *--------------------------------------------------------------------------------------------*
000701080219      * Pgm autostartjob che per ogni p.o. sottomette                                              *
000702080219      * un JOB per la ricezione dati da pistola radio                                              *
001000080219      *--------------------------------------------------------------------------------------------*
001100080219     fazorg01l  if   e           k disk
001200080219     fazute01l  if   e           k disk
001300080219     ftntbe01l  if   e           k disk
001400080219
001500080219     d parpo           s              3
001600080219     d profilo         s             10
001700080219     d nomejob         s             10
001800080219     d WCurlib         s             10
001900080219     d WCurlibDFT      s             10    inz('*CRTDFT')
002000080219     d comman          S            100                                         *comando CLP
002100080219     d lenght          S             15  5 INZ(100)                             *lunghezza comando
002200080219     d kcod            s              3    inz('CUR')
002300080219     d kke1            s             15
002400080219     d dcur          e ds
002500080219
002600080219      *--------------------------------------------------------------------------------------------*
002700080219     c     Ktbe          klist                                                  *tntbe01l
002800080219     c                   kfld                    kcod                            -tabella
002900080219     c                   kfld                    kke1                            -chiave uno
003000080219      *--------------------------------------------------------------------------------------------*
003100080219
003200080219      * elimino eventuale libreria corrente
003300080219     c                   eval      Wcurlib = Wcurlibdft
003400080219     C                   exsr      VarCurLib
003500080219     c
003600080219
003700080219     c     *loval        setll     azorg01l
003800080219     c                   do        *hival
003900080219     c                   read      azorg01l
004000080219
004100080219      * fine file
004200080219    1c                   if        %eof(azorg01l)
004300080219     c                   leave
004400080219     c                   endif
004500080219
004600080219      * escludo gli annullati e se non è Filiale o Agenzia
004700080219     c                   if        orgfva <> *blanks or
004800080219     c                             (orgfag <> 'F' and orgfag <> 'A')
004900080219     c                   iter
005000080219     c                   endif
005100080219
005200080219     c                   move      orgfil        parpo
005300080219
005400080219      * verifico esistenza profilo
005500080219     C                   eval      profilo = 'PR' + parpo
005700080219     c     profilo       chain     azute000
005800080219     c                   if        %found
005900080219
006000080219      * verifico se impostare libreria personalizzazioni
006100080219     c                   clear                   dcur
006200080219     c                   movel(p)  parpo         kke1
006300080219     c     Ktbe          chain     TNTBE000
006400080219if  1c                   if             %found(TNTBE01L)
006500080219     c                             and  TBEatb = *blanks
006600080219     c                   movel     TBEuni        dcur
006700080219      *
006800080219      *  imposto libreria personalizzazioni
006900080219     c                   if        §curlib <> *blanks
007000080219     c                   eval      Wcurlib = §curlib
007100080219     C                   exsr      VarCurLib
007200080219     c                   endif
007300080219     c                   endif
007400080219
007500080219      * sottometto JOB
007501080219     c                   eval      comman =
007502080219     c                             'sbmjob cmd(call pgm(TRTCP8C) PARM('''
007503080219     c                              + parpo + ''')) job(' + profilo + ') -
007504080219     c                              JOBQ(WIFIPR) USER(' + profilo + ')'
008000080219
008100080219     c                   call      'QCMDEXC'
008200080219     c                   parm                    comman
008300080219     c                   parm                    lenght
008400080219
008500080219     c                   endif
008600080219
008700080219      * REimposto "NESSUNA" libreria corrente
008800080219     c                   if        Wcurlib <> Wcurlibdft
008900080219     c                   eval      Wcurlib = Wcurlibdft
009000080219     C                   exsr      VarCurLib
009100080219     c                   endif
009200080219
009300080219     c                   enddo
009400080219
009500080219     c                   eval      *inlr = *on
009600080219      *--------------------------------------------------------------------------------------------*
009700080219      * modifico libreria corrente
009800080219      *--------------------------------------------------------------------------------------------*
009900080219     C     VarCurLib     begsr
010000080219      *
010100080219     c                   eval      comman =
010200080219     c                             'chgcurlib curlib(' + Wcurlib + ')'
010300080219
010400080219     c                   call      'QCMDEXC'
010500080219     c                   parm                    comman
010600080219     c                   parm                    lenght
010700080219
010800080219     c                   endsr
