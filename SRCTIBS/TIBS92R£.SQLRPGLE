000100060102     H DECEDIT('0,') DATEDIT(*dMy.)
000200090116      * TIBS92R  *---------------------------------------------------*
000300090116      *  - diplica dati in FISGN per etichettare in altro terminal
000400940915      *--------------------------------------------------------------*
000500090116     Ffisgn00F  iF a E             DISK    rename(fisgn000:fisgnfis)
000600060102     Fazorg01l  iF   E           K DISK
000700090116     Ffisgn01l  uF   E           K DISK    prefix(O_)
000800050103     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF) USROPN
000900090116     d
001000090116     D OVRP            S             70    DIM(1) CTDATA PERRCD(1)
001100090116     d
001200090116     d wrkgetlista     s           4096    varying
001300090116     d fnsgnds       e ds                  extname(fisgn00f)
001400090116
001500040917     C**********************************************************************
001600040917     C     *ENTRY        PLIST
001700090116     C                   PARM                    parpoDA           3 0
001800090116     C                   PARM                    parpoa            3 0
001900090119     C                   PARM                    partfp            3 0
002000090116     c
002100090116     c                   eval      comman=ovrp(1)
002200090116     C                   CALL      'QCMDEXC'
002300090116     C                   PARM                    COMMAN           80
002400090116     C                   PARM      80            LUNG             15 5
002500090116     c
002600090116     c                   open      qsysprt
002700090116     c*
002800090116     c     parpoda       chain     azorg01l
002900090116     c                   movel     orgdes        despoda          20
003000090116     c     parpoa        chain     azorg01l
003100090116     c                   movel     orgdes        despoa           20
003200090116
003300090116     c                   except    testata
003400090116     C
003500090116     C     KSGN01        KLIST
003600090116     C                   KFLD                    SGNTNP
003700090116     C                   KFLD                    SGNWHO
003800090116     C**********************************************************************
003900090116
004000090116      *?Imposto la parte fissa dell'istruzione sql
004100090116     c                   eval      wrkgetlista =
004200090116     c                             'select * from fisgn00f where ' +
004300090116     c                             'sgnlnp = ' +
004400090119     c                             %editc(parpoda:'X') +  ' and sgntnp= ' +
004500090119     c                             %editc(partfp:'X') +
004600090116     c                             ' AND sgnatb='' '''
004700090116     C/EXEC SQL
004800090116     C+ PREPARE s1 FROM :wrkgetlista
004900090116     C/END-EXEC
005000090116     C
005100090116     C/EXEC SQL
005200090119     C+ DECLARE A1 INSENSITIVE CURSOR FOR S1
005300090116     C/END-EXEC
005400090116     C
005500090116     C/EXEC SQL
005600090116     C+ OPEN a1
005700090116     C/END-EXEC
005800090116
005900090116      *?Leggo il file
006000090116     c                   do        *hival
006100090116     C/EXEC SQL
006200090116     C+ FETCH NEXT FROM a1 INTO :fnsgnds
006300090116     C/END-EXEC
006400090116
006500090116     c                   select
006600090116
006700090116     c                   when      sqlcod = 100
006800090116     c                   leave
006900090116     c                   when      sqlcod < 0
007000090116     c                   seton                                        H1
007100090116     c                   eval      *InLr = *On
007200090116     c                   return
007300090116     c
007400090116     c                   other
007500090116     c                   exsr      Duplica
007600090116     c                   endsl
007700090116
007800090116     c                   enddo
007900090116
008000090116     C/EXEC SQL
008100090116     C+ CLOSE a1
008200090116     C/END-EXEC
008300090116     c
008400090116     c* Stampo totale fisgn aggiunti
008500090116     c                   except    totale
008600090116     c
008700090116     c                   seton                                        lr
008800090116     c****************************************************************
008900090116     C     DUPLICA       BEGSR
009000090116     c****************************************************************
009100090116     c                   clear                   elabora           1
009200050104     c* aggiorno campo sgntnp
009300090116     c                   eval      sgntnp=parPOA
009400090116     C* VERIFICO CHE NON SIA PRESENTE UN RECORD CON LO STESSO CHI SONO
009500090116     C     KSGN01        CHAIN     FISGN01L
009600090116     c* se trovasto verifico che non sia lo stesso
009700090116     c*  in questo caso vado oltre e non duplico
009800090116    1c                   if        %found(fisgn01l)
009900090116     c                   select
010000090116    2c                   when      O_sgnatb='*'
010100090116     c                   delete    fisgn000
010200090116     c                   eval      elabora='S'
010300090116     c
010400090116    2c                   when      O_sgnatb='A'
010500090116     c                   except    stampaerr
010600090116     c                   add       1             contaerr          7 0
010700090116     c                   unlock    fisgn01l
010800090116     c                   other
010900090116     c
011000090116    3c                   if        o_sgnlnp=sgnlnp and
011100090116     c                             o_sgnnsp=sgnnsp and
011200090116     c                             o_sgnnrs=sgnnrs and
011300090116     c                             o_sgnaas=sgnaas
011400090116     c                   add       1             contaesi          7 0
011500090126     c* se già esiste aggiorno solo alcuni campi
011600090126     c                   eval      o_sgnksc=sgnksc
011700090126     c                   eval      o_sgncdp=sgncdp
011800090126     c                   eval      o_sgnncd=sgnncd
011900090126     c                   eval      o_sgnlna=sgnlna
012000090126     c                   eval      o_sgndla=sgndla
012100090126     c                   eval      o_sgntna=sgntna
012200090126     c                   eval      o_sgntsp=sgntsp
012300090126     c                   eval      o_sgnzsc=sgnzsc
012400090126     c                   eval      o_sgnnc1=sgnnc1
012500090126     c                   eval      o_sgnnc2=sgnnc2
012600090126     c                   eval      o_sgnrsm=sgnrsm
012700090126     c                   eval      o_sgnprm=sgnprm
012800090126     c                   eval      o_sgnrsd=sgnrsd
012900090126     c                   eval      o_sgncad=sgncad
013000090126     c                   eval      o_sgnind=sgnind
013100090126     c                   eval      o_sgnlod=sgnlod
013200090126     c                   eval      o_sgnprd=sgnprd
013300090126     c                   eval      o_sgncpd=sgncpd
013400090126     c                   eval      o_sgnvol=sgnvol
013500090126     c                   eval      o_sgnpkg=sgnpkg
013600090126     c                   eval      o_sgnfvr=sgnfvr
013700090126     c                   eval      o_sgntst=sgntst
013800090126     c                   eval      o_sgnfca=sgnfca
013900090126     c                   update    fisgn000
014000090116   x3c                   else
014100090116     c                   except    stampaerr
014200090116     c                   add       1             contaerr          7 0
014300090116    3c                   endif
014400090116    2c                   endsl
014500090116     c
014600090116     c                   else
014700090116     c                   eval      elabora='S'
014800090116    1c                   endif
014900090116     c
015000090116    1c                   if        elabora='S'
015100090116     c                   clear                   sgnt6a
015200090116     c                   clear                   sgnt6b
015300090116     c                   clear                   sgnt6c
015400090116     c                   clear                   sgnt6d
015500090116     c                   clear                   sgnt6e
015600090116     c                   clear                   sgnt6f
015700090116     c
015800090116     c                   eval      sgndatora = %char(%timestamp:*iso0)
015900090116     c                   WRITE     fisgnfis
016000050104     c                   add       1             contasgn          7 0
016100090116    1c                   endif
016200050104    c
016300090116     C                   ENDSR
016400090116     oQsysprt   e            testata        1  2
016500090116     o                                         +  0 'DUPLICA DATI FISGN00F dall'
016600090116     o                                         +  0 'a filiale '
016700090116     o                       parpoda           +  1
016800090116     o                       despoda           +  2
016900090116     o                                         +  2 'al terminal '
017000090116     o                       parpoa            +  1
017100090116     o                       despoa            +  2
017200090116     o                       *date              130 '  /  /    '
017300090116     o
017400090116     oQsysprt   e            totale      2
017500090116     o                                         +  0 'Totale FISGN00F aggiunti'
017600050104     o                       CONTAsgn      2   +  2
017700090116     o                                         +  2 'Totale errori '
017800090116     o                       CONTAerr      2   +  2
017900090116     o                                         +  2 'Già esistenti '
018000090116     o                       CONTAesi      2   +  2
018100090116     oQsysprt   e            stampaerr   1
018200090116     o                                         +  0 'Errori CHI SONO già pr'
018300090116     o                                         +  0 'esente.Rec non aggiunto'
018400090116     o                       sgnATB            +  2
018500090116     o                       sgntnp            +  1
018600090116     o                       sgnwho            +  1
018700090116     o                       sgnaas            +  1
018800090116     o                       sgnlnp            +  1
018900090116     o                       sgnnrs        z   +  1
019000090116     o                       sgnnsp        z   +  1
019100090116     o                                         +  1 'NSC'
019200090116     o                       sgnfls            +  1
019300090116     o                       sgnlna            +  1
019400090116     o                       sgnncd        z   +  1
019500050103** ovrp
019600090116OVRPRTF FILE(QSYSPRT) TOFILE(QSYSPRT) HOLD(*YES) USRDTA('CpyfFISGN')
