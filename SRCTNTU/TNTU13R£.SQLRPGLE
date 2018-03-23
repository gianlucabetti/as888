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
001800051223     **?calcolo nr. variazioni aut/coop
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
003900060206     C                             where ((fretip =''A'' -
004000060206     c                             AND fretsr in (''C'', ''R'')) or -
004100060206     C                             (fretip =''C'' -
004200060206     c                             AND fretsr in (''X'', ''P''))) -
004300060206     c                             AND freddc between ' +
004400060123     C                             %EDITC(t05dti:'X') + ' AND ' +
004500060123     C                             %EDITC(t05dtf:'X') + ' and   +
004600060123     c                             frepno = 1 '
004700051222     **?Se passato p.o.
004800051222     C                   if        t05fil <> 0
004900051222     C                   EVAL      wrkGetLista = wrkGetLista +
005000051223     C                             ' AND substr(digits(frepdr), 1, 3) = ' +
005100051223     C                             %TRIML(%EDITC(t05fil:'Z'))
005200051222     C                   END
005300051222     **?Ordinamento.
005400051222     C                   EVAL      wrkGetLista = wrkGetLista +
005500051223     C                             ' GROUP BY substr(digits(frepdr), 1, 3), -
005600051223     C                             freddc'
005700051222     **?Solo lettura.
005800051222     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
005900051222     C
006000051222     C/EXEC SQL
006100051222     C+ PREPARE s0 FROM :wrkGetLista
006200051222     C/END-EXEC
006300051222     C
006400051222     C/EXEC SQL
006500051222     C+ DECLARE a0 CURSOR FOR S0
006600051222     C/END-EXEC
006700051222     C
006800051222     C/EXEC SQL
006900051222     C+ OPEN a0
007000051222     C/END-EXEC
007100051222     c                   endsr
007200051222     c**********************************************************************
007300051223     c     srdatadist    begsr
007400051222     c**********************************************************************
007500051222     c                   exsr      istruzione
007600051222     c                   clear                   conta
007700051222
007800051222     C                   DO        *hival
007900051222     C/EXEC SQL
008000051223     C+ FETCH NEXT FROM a0 INTO :fgs, :dta, :conta
008100051222     C/END-EXEC
008200051222     C                   SELECT
008300051222     c*
008400051222     C                   WHEN      SqlCod = 100
008500051222     c                   leave
008600051222     C                   WHEN      SqlCod < 0
008700051222     C                   seton                                        H1
008800051222     c                   exsr      srfine
008900051222     c                   other
009000051223     **?calcolo nr. rettifiche aut/coop
009100051223     c                   eval      mult = '330'
009200051223     c                   move      dta           dtaiso
009300051223     c                   move      fgs           comfgs
009400051223     c                   exsr      aggio
009500051222     C                   ENDSL
009600051222      *
009700051222     C                   ENDDO
009800051222     C/EXEC SQL
009900051222     C+ CLOSE a0
010000051222     C/END-EXEC
010100051222     c                   endsr
010200051214     c**********************************************************************
010300051214     c     aggio         begsr
010400051214     c**********************************************************************
010500051129     c     keytud        chain     tntud01l
010600051129     c                   eval      tudqta = conta
010700051129     c                   if        %found(tntud01l)
010800051229     c                   exsr      srstampa
010900051129     c                   update    tntud000
011000051129     c                   else
011100051214     c                   eval      tudmult = mult
011200051130     c                   eval      tuddta  = dtaiso
011300051219     c                   eval      tudfil  = comfgs
011400051129     c                   clear                   tudcli
011500051129     c                   write     tntud000
011600051129     c                   end
011700051129     c                   endsr
011800051129     c**********************************************************************
011900051129     c     *inzsr        begsr
012000051129     c**********************************************************************
012100051129     C     *ENTRY        PLIST
012200051129     C                   PARM                    kpjba
012300051129     c                   movel     kpjbu         tntu05ds
012400051129     c     keytud        klist
012500051214     c                   kfld                    mult
012600051130     c                   kfld                    dtaiso
012700051219     c                   kfld                    comfgs
012800051229     c                   seton                                        of
012900051129     c                   endsr
013000051129     C**************************************************************************
013100051129     C* esco con codice d'errore
013200051129     C**************************************************************************
013300051129     C     srfine        BEGSR
013400051129     c                   seton                                        lr
013500051129     c                   return
013600051129     c*
013700051129     c                   endsr
013800051229     C**************************************************************************
013900051229     C* stampo anomalia
014000051229     C**************************************************************************
014100051229     C     srstampa      BEGSR
014200051229     c*
014300051229     C   OF              EXCEPT    TESTA
014400051229     C   OF              SETOFF                                       OF
014500051229     C                   EXCEPT    anomalia
014600051229     c*
014700051229     c                   endsr
014800051229     OQSYSPRT   E            TESTA            02
014900051229     O                                           41 'Anomalie nel pgm TNTU13R'
015000051229     O                       PAGE          Z    132
015100051229     O          E            anomalia    1
015200051229     O                                           +0 'Si è eseguito un UPDATE pe-
015300051229     O                                              r il moltiplicatore'
015400051229     O                       tudmult             +1
015500051229     O                                           +1 'P.O.'
015600051229     O                       tudfil              +1
015700051229     O                                           +1 'Data'
015800051229     O                       tuddta              +1
