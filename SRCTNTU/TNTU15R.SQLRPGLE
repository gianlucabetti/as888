000100030115
000200090427      /TITLE legge fnrig settimanalmente
000300051129     ftntud01l  uf a e           k disk
000400051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500051129     dkpjba          e ds
000600051229     dtntudtads      e ds
000700051129     dtntu05ds       e ds
000800090427     dfnrigds        e ds                  extname(fnrig00f)
000900051214     D wrkGetLista     S           4096
001000051214     D                                     VARYING
001100051130     d dtaiso          s               d   inz
001200090427     d dta             s                   like(rigdata) inz
001300051223     d fgs             s                   like(riglnp)
001400051223     d comfgs          s                   like(tudfil) inz
001500051129     d conta           s                   like(tudqta)
001600051214     d mult            s                   like(tudmult)
001700030115
001800060113     **?calcolo nr. c/a omessi
001900051223     c                   if        t05mult = ' ' or t05mult = '230'
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
003600090427     C                             'SELECT riglnp, rigdata, count(*) -
003700090427     C                             FROM fnrig00f where -
003801121024     C                             rigcau in (''8'', ''3'', ''C'') and -
003900090427     C                             rigdata between ' +
004000051223     C                             %EDITC(t05dti:'X') + ' AND ' +
004100121023     C                             %EDITC(t05dtf:'X')
004200051222     **?Se passato p.o.
004300051222     C                   if        t05fil <> 0
004400051222     C                   EVAL      wrkGetLista = wrkGetLista +
004500051223     C                             ' AND riglnp = ' +
004600051223     C                             %TRIML(%EDITC(t05fil:'Z'))
004700051222     C                   END
004800051222     **?Ordinamento.
004900051222     C                   EVAL      wrkGetLista = wrkGetLista +
005000090427     C                             ' GROUP BY riglnp, rigdata '
005100051222     **?Solo lettura.
005200051222     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
005300051222     C
005400051222     C/EXEC SQL
005500051222     C+ PREPARE s0 FROM :wrkGetLista
005600051222     C/END-EXEC
005700051222     C
005800051222     C/EXEC SQL
005900051222     C+ DECLARE a0 CURSOR FOR S0
006000051222     C/END-EXEC
006100051222     C
006200051222     C/EXEC SQL
006300051222     C+ OPEN a0
006400051222     C/END-EXEC
006500051222     c                   endsr
006600051222     c**********************************************************************
006700051223     c     srdatadist    begsr
006800051222     c**********************************************************************
006900051222     c                   exsr      istruzione
007000051222     c                   clear                   conta
007100051222
007200051222     C                   DO        *hival
007300051222     C/EXEC SQL
007400051223     C+ FETCH NEXT FROM a0 INTO :fgs, :dta, :conta
007500051222     C/END-EXEC
007600051222     C                   SELECT
007700051222     c*
007800051222     C                   WHEN      SqlCod = 100
007900051222     c                   leave
008000051222     C                   WHEN      SqlCod < 0
008100051222     C                   seton                                        H1
008200051222     c                   exsr      srfine
008300051222     c                   other
008400051223     **?calcolo nr. rettifiche aut/coop
008500051223     c                   eval      mult = '230'
008600051223     c                   move      dta           dtaiso
008700051223     c                   move      fgs           comfgs
008800051223     c                   exsr      aggio
008900051222     C                   ENDSL
009000051222      *
009100051222     C                   ENDDO
009200051222     C/EXEC SQL
009300051222     C+ CLOSE a0
009400051222     C/END-EXEC
009500051222     c                   endsr
009600051214     c**********************************************************************
009700051214     c     aggio         begsr
009800051214     c**********************************************************************
009900051129     c     keytud        chain     tntud01l
010000051129     c                   eval      tudqta = conta
010100051129     c                   if        %found(tntud01l)
010200051229     c                   exsr      srstampa
010300051129     c                   update    tntud000
010400051129     c                   else
010500051214     c                   eval      tudmult = mult
010600051130     c                   eval      tuddta  = dtaiso
010700051219     c                   eval      tudfil  = comfgs
010800051129     c                   clear                   tudcli
010900051129     c                   write     tntud000
011000051129     c                   end
011100051129     c                   endsr
011200051129     c**********************************************************************
011300051129     c     *inzsr        begsr
011400051129     c**********************************************************************
011500051129     C     *ENTRY        PLIST
011600051129     C                   PARM                    kpjba
011700051129     c                   movel     kpjbu         tntu05ds
011800051129     c     keytud        klist
011900051214     c                   kfld                    mult
012000051130     c                   kfld                    dtaiso
012100051219     c                   kfld                    comfgs
012200051229     c                   seton                                        of
012300051129     c                   endsr
012400051129     C**************************************************************************
012500051129     C* esco con codice d'errore
012600051129     C**************************************************************************
012700051129     C     srfine        BEGSR
012800051129     c                   seton                                        lr
012900051129     c                   return
013000051129     c*
013100051129     c                   endsr
013200051229     C**************************************************************************
013300051229     C* stampo anomalia
013400051229     C**************************************************************************
013500051229     C     srstampa      BEGSR
013600051229     c*
013700051229     C   OF              EXCEPT    TESTA
013800051229     C   OF              SETOFF                                       OF
013900051229     C                   EXCEPT    anomalia
014000051229     c*
014100051229     c                   endsr
014200051229     OQSYSPRT   E            TESTA            02
014300051229     O                                           41 'Anomalie nel pgm TNTU15R'
014400051229     O                       PAGE          Z    132
014500051229     O          E            anomalia    1
014600051229     O                                           +0 'Si è eseguito un UPDATE pe-
014700051229     O                                              r il moltiplicatore'
014800051229     O                       tudmult             +1
014900051229     O                                           +1 'P.O.'
015000051229     O                       tudfil              +1
015100051229     O                                           +1 'Data'
015200051229     O                       tuddta              +1
