000100080219     h decedit('0,') option(*nodebugio)
000200080219      *--------------------------------------------------------------------------------------------*
000300080219      * Pgm autostartjob che per ogni p.o. valido sottomette                                       *
000400080220      * un JOB per la ricezione dati da PDA - consegne                                             *
000500080219      *--------------------------------------------------------------------------------------------*
000600080219     fazorg01l  if   e           k disk
000700080219     fazute01l  if   e           k disk
000701080219     ftntbe01l  if   e           k disk
000800080219
000900080219     d parpo           s              3
001000080219     d profilo         s             10
001001080219     d nomejob         s             10
001002080219     d WCurlib         s             10
001003080219     d WCurlibDFT      s             10    inz('*CRTDFT')
001004080219     d comman          S            100                                         *comando CLP
001005080219     d lenght          S             15  5 INZ(100)                             *lunghezza comando
001006080219     d kcod            s              3    inz('CUR')
001007080219     d kke1            s             15
001009080219     d dcur          e ds
001010120110     D dataiso         s               d   datfmt(*iso)
001011120110     D dateu           s              8  0
001100080219
001200080219      *--------------------------------------------------------------------------------------------*
001209080219     c     Ktbe          klist                                                  *tntbe01l
001210080219     c                   kfld                    kcod                            -tabella
001211080219     c                   kfld                    kke1                            -chiave uno
001212080219      *--------------------------------------------------------------------------------------------*
001213120110     c                   time                    dataiso
001214120110     C                   move      dataiso       dateu
001311080219
001312080219      * elimino eventuale libreria corrente
001313080219     c                   eval      Wcurlib = Wcurlibdft
001314080219     C                   exsr      VarCurLib
001315080219     c
001316080219
001400080219     c     *loval        setll     azorg01l
001500080219     c                   do        *hival
001600080219     c                   read      azorg01l
001700080219
001800080219      * fine file
001900080219    1c                   if        %eof(azorg01l)
002000080219     c                   leave
002100080219     c                   endif
002200080219
002300080219      * escludo gli annullati e se non è Filiale o Agenzia
002400080219     c                   if        orgfva <> *blanks or
002500080219     c                             (orgfag <> 'F' and orgfag <> 'A')
002600080219     c                   iter
002700080219     c                   endif
002800080219
002900080219     c                   move      orgfil        parpo
003000080219
003100080219      * verifico esistenza profilo
003200080219     C                   eval      profilo = 'AU' + parpo
003201080220     C                   eval      nomejob = 'AU' + parpo + 'CON'
003300080219     c     profilo       chain     azute000
003500120110     c                   if        %found and dateu<=utedsc
003501080219
003600080219      * verifico se impostare libreria personalizzazioni
003602080219     c                   clear                   dcur
003603080219     c                   movel(p)  parpo         kke1
003604080219     c     Ktbe          chain     TNTBE000
003605080219if  1c                   if             %found(TNTBE01L)
003606080219     c                             and  TBEatb = *blanks
003607080219     c                   movel     TBEuni        dcur
003613080219      *
003614080219      *  imposto libreria personalizzazioni
003615080219     c                   if        §curlib <> *blanks
003618080219     c                   eval      Wcurlib = §curlib
003619080219     C                   exsr      VarCurLib
003620080219     c                   endif
003627080219     c                   endif
003628080219
003629080219      * sottometto JOB
003630080219     c                   eval      comman =
003631080222     c                             'sbmjob cmd(call pgm(TRTCPC0C) PARM('''
003632080219     c                              + parpo + ''')) job(' + nomejob + ') -
003633080220     c                              JOBQ(WIFIBARCON) USER(' + profilo + ')'
003634080219
003635080219     c                   call      'QCMDEXC'
003636080219     c                   parm                    comman
003637080219     c                   parm                    lenght
003638080219
003900080219     c                   endif
003901080219
003902080219      * REimposto "NESSUNA" libreria corrente
003903080219     c                   if        Wcurlib <> Wcurlibdft
003904080219     c                   eval      Wcurlib = Wcurlibdft
003905080219     C                   exsr      VarCurLib
003906080219     c                   endif
004000080219
004100080219     c                   enddo
004200080219
004300080219     c                   eval      *inlr = *on
004400080219      *--------------------------------------------------------------------------------------------*
004401080219      * elimino eventuale libreria corrente
004402080219      *--------------------------------------------------------------------------------------------*
004500080219     C     VarCurLib     begsr
004600080219      *
004700080219     c                   eval      comman =
004800080219     c                             'chgcurlib curlib(' + Wcurlib + ')'
005100080219
005200080219     c                   call      'QCMDEXC'
005300080219     c                   parm                    comman
005400080219     c                   parm                    lenght
005500080219
005600080219     c                   endsr
