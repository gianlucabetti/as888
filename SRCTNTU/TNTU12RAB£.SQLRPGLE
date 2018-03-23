000100051222      /TITLE legge fiftt/fiatt/fictt UNA VOLTA AL MESE IN SEDE
000200170426
000300051129     ftntud01l  uf a e           k disk
000400051229     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000500051129     dkpjba          e ds
000600051229     dtntudtads      e ds
000700051129     dtntu05ds       e ds
000800051219     dfifttds        e ds                  extname(fiftt00f)
000900051214     D wrkGetLista     S           4096
001000051214     D                                     VARYING
001100170426     D wrkFile         S           1024
001200170426     D                                     VARYING
001300051130     d dtaiso          s               d   inz
001400051222     d comdti          s             10
001500051222     d comdtf          s             10
001600051222     d dtai            s                   like(dtaiso)
001700170427      **
001800051222     d reg             s              9  0
001900051222     d dta             s                   like(fttddc) inz
002000051219     d fgs             s                   like(fttfgs) inz
002100051219     d nff             s                   like(fttnff) inz
002200051222     d comfgs          s                   like(fttfgs) inz
002300051129     d conta           s                   like(tudqta)
002400051214     d mult            s                   like(tudmult)
002500051222     d rotto           s             13
002600051219     d rottos          s                   like(rotto)
002700170427      *
002800170427     d dta_iso         s               d   datfmt(*iso)
002900170427     d dtada_iso       s               d   datfmt(*iso)
003000170427     d dtaa_iso        s               d   datfmt(*iso)
003100030115
003200051222     c                   move      t05dti        dtaiso
003300051222     c                   move      dtaiso        comdti
003400170427      *
003500051222     c                   move      t05dtf        dtaiso
003600051222     c                   move      dtaiso        comdtf
003700170427      *
003800170427      *  Da data a Data in formato *ISO
003900170427     c     *iso          move      t05dti        dtada_iso
004000170427     c     *iso          move      t05dtf        dtaa_iso
004100170427      *
004200051222     **?calcolo nr. fatture autotrasp.
004300051219     c                   if        t05mult = ' ' or t05mult = '290'
004400170426      **
004500170427     c                   eval      wrkFile = 'WITH WFILE as (SELECT -
004600170427     c                             INT(substr(fttflr, 2, 9)) as nrasreg -
004700170428     c                             FROM fiftt00f WHERE ftttsr = '' ''  and  -
004800170428     c                             substr(fttflr, 2, 9) <> '' '' and  -
004900170428     c                             fttnff <> 0 and fttnff <> 999999999 -
005000170428     c                             GROUP BY INT(substr(fttflr, 2, 9)) -
005100170427     c                             ), '
005200170426      **
005300060119     c                   eval      mult = '290'
005400060119     c                   exsr      srdatafat
005500051219     c                   end
005600170427      **
005700051222     **?calcolo nr. fatture aff/defl
005800051222     c                   if        t05mult = ' ' or t05mult = '300'
005900170426      **
006000170427     c                   eval      wrkFile = 'WITH WFILE as (SELECT -
006100170427     c                             attnra as nrasreg FROM fiatt00f WHERE -
006200170427     c                             attnra <> 0 GROUP by attnra -
006300170427     c                             ), '
006400170426      **
006500060119     c                   eval      mult = '300'
006600060119     c                   exsr      srdatafat
006700051222     c                   end
006800170427      **
006900051222     **?calcolo nr. fatture coop
007000051222     c                   if        t05mult = ' ' or t05mult = '310'
007100170426      **
007200170427     c                   eval      wrkFile = 'WITH WFILE as (SELECT -
007300170427     c                             cttnra as nrasreg FROM fictt00f WHERE -
007400170428     c                             cttnra <> 0 GROUP BY cttnra -
007500170427     c                             ), '
007600170426      **
007700051222     c                   eval      mult = '310'
007800060119     c                   exsr      srdatafat
007900051222     c                   end
008000170426      ***
008100051229     c* aggiorno data ultima elaborazione nella tabella TUP
008200051229     c                   clear                   tntudtads
008300051229     c                   eval      §DTAazn = kcoaz
008400051229     c                   eval      §DTAdta = t05dtf
008500051229     c                   call      'TNTUDTAR'
008600051229     c                   parm                    tntudtads
008700051214     c*
008800051214     c                   seton                                        lr
008900051219     C**************************************************************************
009000051219     C* compongo istruzione sql
009100051219     C**************************************************************************
009200051219     C     istruzione2   BEGSR
009300170427      *
009400051219     **?Imposto la parte fissa dell'istruzione.
009500170427     C                   EVAL      wrkGetLista =  %trim(wrkfile) +
009600170427     C                             ' CONTABIL as ( SELECT * from WFILE -
009700170427     C                             join ndreg00f on regsys=0 and -
009800170427     C                             nrasreg = REGNRASREG join -
009900170427     C                             anutn00f on reguteimm = utncut join -
010000170427     C                             azute00f on utnprf = uteute -
010100170427     C                             WHERE REGDTIMM between '        +
010200170427     C                             '''' + comdti + ''''
010300170427     C                             +
010400170427     C                             ' AND '
010500170427     C                             +
010600170427     C                             '''' + comdtf + '''' +
010700170427     C                             '), -
010800170427     C                             RISULTA as ( SELECT utefil, REGDTIMM, -
010900170427     C                             count(*) as NUM FROM CONTABIL -
011000170427     C                             GROUP BY utefil, REGDTIMM) -
011100170427     C                             SELECT * FROM RISULTA '
011200170427     **?Se passato p.o.
011300170427     C                   if        t05fil <> 0
011400170427     C                   EVAL      wrkGetLista = wrkGetLista +
011500170427     C                             ' WHERE utefil = ' +
011600170427     C                             %TRIML(%EDITC(t05fil:'Z'))
011700170427     C                   END
011800170427     **?Clausola di sola lettura
011900170427     C                   EVAL      wrkGetLista = wrkGetLista +
012000170427     C                             ' FOR READ ONLY '
012100170427      *
012200051219     C/EXEC SQL
012300051222     C+ PREPARE s2 FROM :wrkGetLista
012400051219     C/END-EXEC
012500051219     C
012600051219     C/EXEC SQL
012700051222     C+ DECLARE a2 CURSOR FOR S2
012800051219     C/END-EXEC
012900051219     C
013000051219     C/EXEC SQL
013100051222     C+ OPEN a2
013200051219     C/END-EXEC
013300051219     c                   endsr
013400051219     c**********************************************************************
013500060119     c     srdatafat     begsr
013600051219     c**********************************************************************
013700051219     c                   exsr      istruzione2
013800051219
013900051219     C                   DO        *hival
014000051219     C/EXEC SQL
014100170427     C+ FETCH NEXT FROM a2 INTO :fgs, :dta_iso, :conta
014200051219     C/END-EXEC
014300051219     C                   SELECT
014400051219     c*
014500051219     C                   WHEN      SqlCod = 100
014600051219     c                   leave
014700170427      **
014800051219     C                   WHEN      SqlCod < 0
014900051219     C                   seton                                        H1
015000051219     c                   exsr      srfine
015100170427      **
015200051219     c                   other
015300170427      **
015400170427     **?Se passato p.o. o la DATA è fuori dal Range
015500170427     C**********         if        t05fil <> 0 and fgs <> t05fil
015600170427     c**********                     or
015700170427     c**********                   dta_iso < dtada_iso
015800170427     c**********                     or
015900170427     c**********                   dta_iso > dtaa_iso
016000170427     c**********         iter
016100170427     c**********         end
016200170427      **
016300170427     c                   move      dta_iso       dtaiso
016400060119     c                   move      fgs           comfgs
016500170427     **?Aggiorna e stampa
016600060119     c                   exsr      aggio
016700170427      **
016800051219     C                   ENDSL
016900051219      *
017000051219     C                   ENDDO
017100051219     C/EXEC SQL
017200051222     C+ CLOSE a2
017300051219     C/END-EXEC
017400051219     c                   endsr
017500051222     C**************************************************************************
017600051222     C* compongo istruzione sql
017700051222     C**************************************************************************
017800051222     C     istruzione1   BEGSR
017900170426      *
018000051222     **?Imposto la parte fissa dell'istruzione.
018100060119     C                   EVAL      wrkGetLista =
018200060119     C                             'SELECT utefil, REGDTIMM, count(*) -
018300060119     C                             FROM wnrasregv join ndreg00f on  -
018400060119     C                             sys = regsys and nrasreg = REGNRASREG -
018500060119     C                             join anutn00f on reguteimm = -           -
018600060119     C                             utncut join azute00f on utnprf -          -
018700060119     C                             = uteute WHERE REGDTIMM between '        +
018800060119     C                             '''' + comdti + ''''
018900060119     C                             +
019000060119     C                             ' AND '
019100060119     C                             +
019200060119     C                             '''' + comdtf + ''''
019300060119     **?Se passato p.o.
019400060119     C                   if        t05fil <> 0
019500060119     C                   EVAL      wrkGetLista = wrkGetLista +
019600060119     C                             ' AND utefil = ' + %TRIML(%EDITC(t05fil:'Z'))
019700060119     C                   END
019800060119     **?Ordinamento.
019900060119     C                   EVAL      wrkGetLista = wrkGetLista +
020000060119     C                             ' GROUP BY utefil, REGDTIMM'
020100060119     **?Solo lettura.
020200060119     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
020300051222     C
020400051222     C/EXEC SQL
020500051222     C+ PREPARE s1 FROM :wrkGetLista
020600051222     C/END-EXEC
020700051222     C
020800051222     C/EXEC SQL
020900051222     C+ DECLARE a1 CURSOR FOR S1
021000051222     C/END-EXEC
021100051222     C
021200051222     C/EXEC SQL
021300051222     C+ OPEN a1
021400051222     C/END-EXEC
021500051222     c                   endsr
021600051214     c**********************************************************************
021700051214     c     aggio         begsr
021800051214     c**********************************************************************
021900051129     c     keytud        chain     tntud01l
022000170427      **
022100051129     c                   eval      tudqta = conta
022200051129     c                   if        %found(tntud01l)
022300170427      **
022400051229     c                   exsr      srstampa
022500170427      **
022600051129     c                   update    tntud000
022700170427      **
022800051129     c                   else
022900051214     c                   eval      tudmult = mult
023000051130     c                   eval      tuddta  = dtaiso
023100051219     c                   eval      tudfil  = comfgs
023200051129     c                   clear                   tudcli
023300051129     c                   write     tntud000
023400051129     c                   end
023500170427      **
023600051129     c                   endsr
023700051129     c**********************************************************************
023800051129     c     *inzsr        begsr
023900051129     c**********************************************************************
024000051129     C     *ENTRY        PLIST
024100051129     C                   PARM                    kpjba
024200170427      ***
024300051129     c                   movel     kpjbu         tntu05ds
024400170427      ***
024500051129     c     keytud        klist
024600051214     c                   kfld                    mult
024700051130     c                   kfld                    dtaiso
024800051219     c                   kfld                    comfgs
024900170427      ***
025000051229     c                   seton                                        of
025100170427      ***
025200051129     c                   endsr
025300051129     C**************************************************************************
025400051129     C* esco con codice d'errore
025500051129     C**************************************************************************
025600051129     C     srfine        BEGSR
025700170427      ***
025800051129     c                   seton                                        lr
025900051129     c                   return
026000051129     c*
026100051129     c                   endsr
026200051229     C**************************************************************************
026300051229     C* stampo anomalia
026400051229     C**************************************************************************
026500051229     C     srstampa      BEGSR
026600051229     c*
026700051229     C   OF              EXCEPT    TESTA
026800051229     C   OF              SETOFF                                       OF
026900051229     C                   EXCEPT    anomalia
027000051229     c*
027100051229     c                   endsr
027200170427     C**************************************************************************
027300051229     OQSYSPRT   E            TESTA            02
027400051229     O                                           41 'Anomalie nel pgm TNTU12R'
027500051229     O                       PAGE          Z    132
027600051229     O          E            anomalia    1
027700051229     O                                           +0 'Si è eseguito un UPDATE pe-
027800051229     O                                              r il moltiplicatore'
027900051229     O                       tudmult             +1
028000051229     O                                           +1 'P.O.'
028100051229     O                       tudfil              +1
028200051229     O                                           +1 'Data'
028300051229     O                       tuddta              +1
