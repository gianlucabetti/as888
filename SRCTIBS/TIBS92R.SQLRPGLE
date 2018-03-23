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
001301130109     d                 ds
001302130109     D campoksc                      35
001303130109     D ksc                            7    dim(5) overlay(campoksc)
001304130109
001305130109     d ii              s              3  0 inz
001306130110     d l               s              2  0 inz
001307130109     d wprimo          s              1
001308130110      *
001309130110     D err             s              7  0 dim(5)
001310130110     D esi             s              7  0 dim(5)
001311130110     D sgn             s              7  0 dim(5)
001312130110     D w007a           s              7
001400090116
001500040917     C**********************************************************************
001600040917     C     *ENTRY        PLIST
001700090116     C                   PARM                    parpoDA           3 0
001800090116     C                   PARM                    parpoa            3 0
001900090119     C                   PARM                    partfp            3 0
001901130108     C                   PARM                    parksc           35
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
003301130109     c                   if        %parms > 3
003302130109     c                   eval      campoksc=parksc
003303130109     c                   except    testatcli
003304130109     c                   endif
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
004601130109     c* se ricevuto anche il IV° parametro parzializzo per i codici clienti ricevuti
004602130109    1c                   if        %parms > 3
004603130109     c                   eval      wprimo='S'
004605130109     c                   eval      wrkgetlista=%trim(wrkgetlista) +
004606130109     c                             ' and sgnksc in ('
004607130109    2c                   do        5             ii
004608130109    3c                   if        ksc(ii) <>*blanks
004609130109    4c                   if        wprimo<>'S'
004610130109     c                   eval      wrkgetlista=wrkgetlista+ ','
004611130109    4c                   endif
004612130109     c                   eval      wrkgetlista=wrkgetlista + ' ' + ksc(ii)
004613130109     c                   eval      wprimo=' '
004614130109    3c                   endif
004615130109    2c                   enddo
004616130109     c                   eval      wrkgetlista=wrkgetlista + ')'
004618130109    1c                   endif
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
008401130110     c                   if        %parms<=3
008500090116     c                   except    totale
008501130110
008502130110     c                   else
008503130110      *
008504130110     c                   do        5             l
008505130110     c                   if        ksc(l)<>*blanks
008506130110     c                   except    totalecli
008507130110     c                   endif
008508130110     c                   enddo
008509130110
008510130110     c                   endif
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
010601130110     c                   if        %parms>3
010602130110     c                   move      sgnksc        w007a
010603130110     c                   eval      l=%lookup(w007a:ksc)
010604130110     c                   add       1             err(l)
010605130110     c                   endif
010700090116     c                   unlock    fisgn01l
010800090116     c                   other
010900090116     c
011000090116    3c                   if        o_sgnlnp=sgnlnp and
011100090116     c                             o_sgnnsp=sgnnsp and
011200090116     c                             o_sgnnrs=sgnnrs and
011300090116     c                             o_sgnaas=sgnaas
011400090116     c                   add       1             contaesi          7 0
011401130110     c                   if        %parms>3
011402130110     c                   move      sgnksc        w007a
011403130110     c                   eval      l=%lookup(w007a:ksc)
011404130110     c                   add       1             esi(l)
011405130110     c                   endif
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
014201130110     c                   if        %parms>3
014202130110     c                   move      sgnksc        w007a
014203130110     c                   eval      l=%lookup(w007a:ksc)
014204130110     c                   add       1             err(l)
014205130110     c                   endif
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
016001130110     c                   if        %parms>3
016002130110     c                   move      sgnksc        w007a
016003130110     c                   eval      l=%lookup(w007a:ksc)
016004130110     c                   add       1             sgn(l)
016005130110     c                   endif
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
017201130109     o          e            testatcli      1
017202130109     o                                         + 22 'Clienti:'
017203130109     o                       ksc(1)            +  1
017204130109     o                       ksc(2)            +  2
017205130109     o                       ksc(3)            +  2
017206130109     o                       ksc(4)            +  2
017207130109     o                       ksc(5)            +  2
017300090116     o
017400090116     oQsysprt   e            totale      2
017500090116     o                                         +  0 'Totale FISGN00F aggiunti'
017600050104     o                       CONTAsgn      2   +  2
017700090116     o                                         +  2 'Totale errori '
017800090116     o                       CONTAerr      2   +  2
017900090116     o                                         +  2 'Già esistenti '
018000090116     o                       CONTAesi      2   +  2
018001130110     o          e            totalecli   2  1
018002130110     o                                         +  0 'Totale Cliente'
018003130110     o                       ksc(l)            +  2
018004130110     o                                         +  2 'FISGN00F aggiunti'
018005130110     o                       sgn(l)        2   +  2
018006130110     o                                         +  2 'Totale errori '
018007130110     o                       err(l)        2   +  2
018008130110     o                                         +  2 'Già esistenti '
018009130110     o                       esi(l)        2   +  2
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
