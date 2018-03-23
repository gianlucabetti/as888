000400030115
000500051222      /TITLE legge fiftt/fiatt/fictt UNA VOLTA AL MESE IN SEDE
000600051129     ftntud01l  uf a e           k disk
000601051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000602051129     dkpjba          e ds
000603051229     dtntudtads      e ds
000604051129     dtntu05ds       e ds
000605051219     dfifttds        e ds                  extname(fiftt00f)
000606051214     D wrkGetLista     S           4096
000607051214     D                                     VARYING
000608051130     d dtaiso          s               d   inz
000609051222     d comdti          s             10
000610051222     d comdtf          s             10
000611051222     d dtai            s                   like(dtaiso)
000612051222     d reg             s              9  0
000613051222     d dta             s                   like(fttddc) inz
000614051219     d fgs             s                   like(fttfgs) inz
000615051219     d nff             s                   like(fttnff) inz
000616051222     d comfgs          s                   like(fttfgs) inz
000617051129     d conta           s                   like(tudqta)
000618051214     d mult            s                   like(tudmult)
000619051222     d rotto           s             13
000620051219     d rottos          s                   like(rotto)
002000030115
002001051222     c                   move      t05dti        dtaiso
002002051222     c                   move      dtaiso        comdti
002003051222     c                   move      t05dtf        dtaiso
002004051222     c                   move      dtaiso        comdtf
002041051222     **?calcolo nr. fatture autotrasp.
002042051219     c                   if        t05mult = ' ' or t05mult = '290'
002043060119     c* cancello la view
002044060119     C/EXEC SQL
002045060119     C+ DROP VIEW QTEMP/Wnrasregv CASCADE
002046060119     C/END-EXEC
002047060119     c* creo la view per reperire numero assoluto
002048060119     C/EXEC SQL
002049060119     C+ CREATE VIEW QTEMP/wnrasregv AS SELECT 0 as sys, dec(substr(fttflr,
002050060119     C+ 2, 9), 9, 0) as nrasreg FROM fiftt00f WHERE substr(fttflr, 2, 9) <>
002051060119     C+ ' ' and fttnff <> 0 and fttnff <> 999999999 and ftttsr = ' ' GROUP
002052060119     C+ BY 0, dec(substr(fttflr, 2, 9), 9, 0)
002060060119     C/END-EXEC
002061060119     c                   eval      mult = '290'
002062060119     c                   exsr      srdatafat
002063051219     c                   end
002064051222     **?calcolo nr. fatture aff/defl
002065051222     c                   if        t05mult = ' ' or t05mult = '300'
002069060119     c* cancello la view
002070060119     C/EXEC SQL
002071060119     C+ DROP VIEW QTEMP/Wnrasregv CASCADE
002072060119     C/END-EXEC
002073060119     c* creo la view per reperire numero assoluto
002074060119     C/EXEC SQL
002075060119     C+ CREATE VIEW QTEMP/wnrasregv AS SELECT 0 as sys, attnra
002076060119     C+ as nrasreg FROM fiatt00f WHERE attnra  <> 0
002077060119     C+ GROUP by 0, attnra
002079060119     C/END-EXEC
002080060119     c                   eval      mult = '300'
002081060119     c                   exsr      srdatafat
002082051222     c                   end
002083051222     **?calcolo nr. fatture coop
002084051222     c                   if        t05mult = ' ' or t05mult = '310'
002085060119     C/EXEC SQL
002086060119     C+ DROP VIEW QTEMP/Wnrasregv CASCADE
002087060119     C/END-EXEC
002088060119     C/EXEC SQL
002089060119     C+ CREATE VIEW QTEMP/wnrasregv AS SELECT 0 as sys, cttnra
002090060119     C+ as nrasreg FROM fictt00f WHERE cttnra  <> 0
002091060119     C+ GROUP by 0, cttnra
002092060119     C/END-EXEC
002093051222     c                   eval      mult = '310'
002094060119     c                   exsr      srdatafat
002095051222     c                   end
002096051229     c* aggiorno data ultima elaborazione nella tabella TUP
002097051229     c                   clear                   tntudtads
002098051229     c                   eval      §DTAazn = kcoaz
002099051229     c                   eval      §DTAdta = t05dtf
002100051229     c                   call      'TNTUDTAR'
002101051229     c                   parm                    tntudtads
002102051214     c*
002103051214     c                   seton                                        lr
004115051219     C**************************************************************************
004116051219     C* compongo istruzione sql
004117051219     C**************************************************************************
004118051219     C     istruzione2   BEGSR
004123051219     **?Imposto la parte fissa dell'istruzione.
004124051219     C                   EVAL      wrkGetLista =
004125060119     C                             'SELECT utefil, REGDTIMM, count(*) -
004126060119     C                             FROM wnrasregv join ndreg00f on  -
004127060119     C                             sys = regsys and nrasreg = REGNRASREG -
004128060119     C                             join anutn00f on reguteimm = -
004129060119     C                             utncut join azute00f on utnprf -
004130060119     C                             = uteute WHERE REGDTIMM between '        +
004131051222     C                             '''' + comdti + ''''
004132051222     C                             +
004133051222     C                             ' AND '
004134051222     C                             +
004135051222     C                             '''' + comdtf + ''''
004136051219     **?Se passato p.o.
004137051219     C                   if        t05fil <> 0
004138051219     C                   EVAL      wrkGetLista = wrkGetLista +
004139060119     C                             ' AND utefil = ' + %TRIML(%EDITC(t05fil:'Z'))
004140051219     C                   END
004141051219     **?Ordinamento.
004142051219     C                   EVAL      wrkGetLista = wrkGetLista +
004143060119     C                             ' GROUP BY utefil, REGDTIMM'
004144051219     **?Solo lettura.
004145051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004146051219     C
004147051219     C/EXEC SQL
004148051222     C+ PREPARE s2 FROM :wrkGetLista
004149051219     C/END-EXEC
004150051219     C
004151051219     C/EXEC SQL
004152051222     C+ DECLARE a2 CURSOR FOR S2
004153051219     C/END-EXEC
004154051219     C
004155051219     C/EXEC SQL
004156051222     C+ OPEN a2
004157051219     C/END-EXEC
004158051219     c                   endsr
004159051219     c**********************************************************************
004160060119     c     srdatafat     begsr
004161051219     c**********************************************************************
004162051219     c                   exsr      istruzione2
004164051219
004165051219     C                   DO        *hival
004166051219     C/EXEC SQL
004167060119     C+ FETCH NEXT FROM a2 INTO :fgs, :dtai, :conta
004168051219     C/END-EXEC
004169051219     C                   SELECT
004170051219     c*
004171051219     C                   WHEN      SqlCod = 100
004175051219     c                   leave
004176051219     C                   WHEN      SqlCod < 0
004177051219     C                   seton                                        H1
004178051219     c                   exsr      srfine
004179051219     c                   other
004181060119     c                   move      dtai          dtaiso
004182060119     c                   move      fgs           comfgs
004183060119     c                   exsr      aggio
004184051219     C                   ENDSL
004185051219      *
004186051219     C                   ENDDO
004187051219     C/EXEC SQL
004188051222     C+ CLOSE a2
004189051219     C/END-EXEC
004190051219     c                   endsr
004191051222     C**************************************************************************
004192051222     C* compongo istruzione sql
004193051222     C**************************************************************************
004194051222     C     istruzione1   BEGSR
004197051222     **?Imposto la parte fissa dell'istruzione.
004198060119     C                   EVAL      wrkGetLista =
004199060119     C                             'SELECT utefil, REGDTIMM, count(*) -
004200060119     C                             FROM wnrasregv join ndreg00f on  -
004201060119     C                             sys = regsys and nrasreg = REGNRASREG -
004202060119     C                             join anutn00f on reguteimm = -           -
004203060119     C                             utncut join azute00f on utnprf -          -
004204060119     C                             = uteute WHERE REGDTIMM between '        +
004205060119     C                             '''' + comdti + ''''
004206060119     C                             +
004207060119     C                             ' AND '
004208060119     C                             +
004209060119     C                             '''' + comdtf + ''''
004210060119     **?Se passato p.o.
004211060119     C                   if        t05fil <> 0
004212060119     C                   EVAL      wrkGetLista = wrkGetLista +
004213060119     C                             ' AND utefil = ' + %TRIML(%EDITC(t05fil:'Z'))
004214060119     C                   END
004215060119     **?Ordinamento.
004216060119     C                   EVAL      wrkGetLista = wrkGetLista +
004217060119     C                             ' GROUP BY utefil, REGDTIMM'
004218060119     **?Solo lettura.
004219060119     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
004220051222     C
004221051222     C/EXEC SQL
004222051222     C+ PREPARE s1 FROM :wrkGetLista
004223051222     C/END-EXEC
004224051222     C
004225051222     C/EXEC SQL
004226051222     C+ DECLARE a1 CURSOR FOR S1
004227051222     C/END-EXEC
004228051222     C
004229051222     C/EXEC SQL
004230051222     C+ OPEN a1
004231051222     C/END-EXEC
004232051222     c                   endsr
004972051214     c**********************************************************************
004973051214     c     aggio         begsr
004974051214     c**********************************************************************
004977051129     c     keytud        chain     tntud01l
004978051129     c                   eval      tudqta = conta
004979051129     c                   if        %found(tntud01l)
004980051229     c                   exsr      srstampa
004981051129     c                   update    tntud000
004982051129     c                   else
004983051214     c                   eval      tudmult = mult
004984051130     c                   eval      tuddta  = dtaiso
004985051219     c                   eval      tudfil  = comfgs
004986051129     c                   clear                   tudcli
004987051129     c                   write     tntud000
004988051129     c                   end
005000051129     c                   endsr
005001051129     c**********************************************************************
005002051129     c     *inzsr        begsr
005003051129     c**********************************************************************
005100051129     C     *ENTRY        PLIST
005200051129     C                   PARM                    kpjba
005300051129     c                   movel     kpjbu         tntu05ds
005340051129     c     keytud        klist
005341051214     c                   kfld                    mult
005342051130     c                   kfld                    dtaiso
005343051219     c                   kfld                    comfgs
005344051229     c                   seton                                        of
005400051129     c                   endsr
005500051129     C**************************************************************************
005600051129     C* esco con codice d'errore
005700051129     C**************************************************************************
005800051129     C     srfine        BEGSR
007100051129     c                   seton                                        lr
007200051129     c                   return
007300051129     c*
007400051129     c                   endsr
007500051229     C**************************************************************************
007600051229     C* stampo anomalia
007700051229     C**************************************************************************
007800051229     C     srstampa      BEGSR
007900051229     c*
008000051229     C   OF              EXCEPT    TESTA
008100051229     C   OF              SETOFF                                       OF
008200051229     C                   EXCEPT    anomalia
008300051229     c*
008400051229     c                   endsr
008500051229     OQSYSPRT   E            TESTA            02
008600051229     O                                           41 'Anomalie nel pgm TNTU12R'
008700051229     O                       PAGE          Z    132
008800051229     O          E            anomalia    1
008900051229     O                                           +0 'Si è eseguito un UPDATE pe-
009000051229     O                                              r il moltiplicatore'
009100051229     O                       tudmult             +1
009200051229     O                                           +1 'P.O.'
009300051229     O                       tudfil              +1
009400051229     O                                           +1 'Data'
009500051229     O                       tuddta              +1
