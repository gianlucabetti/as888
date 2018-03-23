000100030115
000200051223      /TITLE legge fifre in filiale settimanalmente
000300051129     ftntud01l  uf a e           k disk
000400051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500051129     dkpjba          e ds
000600051229     dtntudtads      e ds
000700051129     dtntu05ds       e ds
000800051223     dfifreds        e ds                  extname(fifre00f)
000900051214     D wrkGetLista     S           4096
001000051214     D                                     VARYING
001100051130     d dtaiso          s               d   inz
001200051223     d dta             s                   like(freddc) inz
001300051223     d fgs             s              3
001400051223     d comfgs          s                   like(tudfil) inz
001500051129     d conta           s                   like(tudqta)
001600051214     d mult            s                   like(tudmult)
001700030115
001800060322     **?calcolo nr. variazioni coop
001900051223     c                   if        t05mult = ' ' or t05mult = '330'
002000051223     c                   exsr      srdatadist
002100051219     c                   end
002200051229     c* aggiorno data ultima elaborazione nella tabella TUP
002300051229     c                   clear                   tntudtads
002400051229     c                   eval      §DTAazn = kcoaz
002500051229     c                   eval      §DTAdta = t05dtf
002600051229     c                   call      'TNTUDTAR'
002700051229     c                   parm                    tntudtads
002800051214     c*
002900051214     c                   seton                                        lr
003000051222     C**************************************************************************
003100051222     C* compongo istruzione sql
003200051222     C**************************************************************************
003300051222     C     istruzione    BEGSR
003400051222     **?Imposto la parte fissa dell'istruzione.
003500051222     C                   EVAL      wrkGetLista =
003600051223     C                             'SELECT substr(digits(frepdr), 1, 3), -
003700051223     C                             freddc, count(*) -
003800060110     C                             FROM fifre02l -
003900060322     C                             where  -
004000060322     C                             fretip =''C'' -
004100060322     c                             AND fretsr in (''X'', ''P'')  -
004200060206     c                             AND freddc between ' +
004300060123     C                             %EDITC(t05dti:'X') + ' AND ' +
004400060123     C                             %EDITC(t05dtf:'X') + ' and   +
004500060123     c                             frepno = 1 '
004600051222     **?Se passato p.o.
004700051222     C                   if        t05fil <> 0
004800051222     C                   EVAL      wrkGetLista = wrkGetLista +
004900051223     C                             ' AND substr(digits(frepdr), 1, 3) = ' +
005000051223     C                             %TRIML(%EDITC(t05fil:'Z'))
005100051222     C                   END
005200051222     **?Ordinamento.
005300051222     C                   EVAL      wrkGetLista = wrkGetLista +
005400051223     C                             ' GROUP BY substr(digits(frepdr), 1, 3), -
005500051223     C                             freddc'
005600051222     **?Solo lettura.
005700051222     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
005800051222     C
005900051222     C/EXEC SQL
006000051222     C+ PREPARE s0 FROM :wrkGetLista
006100051222     C/END-EXEC
006200051222     C
006300051222     C/EXEC SQL
006400051222     C+ DECLARE a0 CURSOR FOR S0
006500051222     C/END-EXEC
006600051222     C
006700051222     C/EXEC SQL
006800051222     C+ OPEN a0
006900051222     C/END-EXEC
007000051222     c                   endsr
007100051222     c**********************************************************************
007200051223     c     srdatadist    begsr
007300051222     c**********************************************************************
007400051222     c                   exsr      istruzione
007500051222     c                   clear                   conta
007600051222
007700051222     C                   DO        *hival
007800051222     C/EXEC SQL
007900051223     C+ FETCH NEXT FROM a0 INTO :fgs, :dta, :conta
008000051222     C/END-EXEC
008100051222     C                   SELECT
008200051222     c*
008300051222     C                   WHEN      SqlCod = 100
008400051222     c                   leave
008500051222     C                   WHEN      SqlCod < 0
008600051222     C                   seton                                        H1
008700051222     c                   exsr      srfine
008800051222     c                   other
008900051223     **?calcolo nr. rettifiche aut/coop
009000051223     c                   eval      mult = '330'
009100051223     c                   move      dta           dtaiso
009200051223     c                   move      fgs           comfgs
009300051223     c                   exsr      aggio
009400051222     C                   ENDSL
009500051222      *
009600051222     C                   ENDDO
009700051222     C/EXEC SQL
009800051222     C+ CLOSE a0
009900051222     C/END-EXEC
010000051222     c                   endsr
010100051214     c**********************************************************************
010200051214     c     aggio         begsr
010300051214     c**********************************************************************
010400051129     c     keytud        chain     tntud01l
010500051129     c                   eval      tudqta = conta
010600051129     c                   if        %found(tntud01l)
010700051229     c                   exsr      srstampa
010800051129     c                   update    tntud000
010900051129     c                   else
011000051214     c                   eval      tudmult = mult
011100051130     c                   eval      tuddta  = dtaiso
011200051219     c                   eval      tudfil  = comfgs
011300051129     c                   clear                   tudcli
011400051129     c                   write     tntud000
011500051129     c                   end
011600051129     c                   endsr
011700051129     c**********************************************************************
011800051129     c     *inzsr        begsr
011900051129     c**********************************************************************
012000051129     C     *ENTRY        PLIST
012100051129     C                   PARM                    kpjba
012200051129     c                   movel     kpjbu         tntu05ds
012300051129     c     keytud        klist
012400051214     c                   kfld                    mult
012500051130     c                   kfld                    dtaiso
012600051219     c                   kfld                    comfgs
012700051229     c                   seton                                        of
012800051129     c                   endsr
012900051129     C**************************************************************************
013000051129     C* esco con codice d'errore
013100051129     C**************************************************************************
013200051129     C     srfine        BEGSR
013300051129     c                   seton                                        lr
013400051129     c                   return
013500051129     c*
013600051129     c                   endsr
013700051229     C**************************************************************************
013800051229     C* stampo anomalia
013900051229     C**************************************************************************
014000051229     C     srstampa      BEGSR
014100051229     c*
014200051229     C   OF              EXCEPT    TESTA
014300051229     C   OF              SETOFF                                       OF
014400051229     C                   EXCEPT    anomalia
014500051229     c*
014600051229     c                   endsr
014700051229     OQSYSPRT   E            TESTA            02
014800051229     O                                           41 'Anomalie nel pgm TNTU13R'
014900051229     O                       PAGE          Z    132
015000051229     O          E            anomalia    1
015100051229     O                                           +0 'Si è eseguito un UPDATE pe-
015200051229     O                                              r il moltiplicatore'
015300051229     O                       tudmult             +1
015400051229     O                                           +1 'P.O.'
015500051229     O                       tudfil              +1
015600051229     O                                           +1 'Data'
015700051229     O                       tuddta              +1
