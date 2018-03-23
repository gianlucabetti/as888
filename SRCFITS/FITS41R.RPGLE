000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300060207      * FITS41R  *----------------------------------------------------*
000400060207      *  BOLLE sede  dai file delle bolle scaricate
000500021010      *---------------------------------------------------------------* c
000600060210     ftitas0nw  If a e           k disk
000700060210     f                                     rename(titas000:tas0new)
000800060210     ftitas1nw  If a e           k disk
000900060210     f                                     rename(titas010:tas1new)
001000060210     ftitas30c  If   e           k disk
001100021007      *
001200060207     ftitaaNEW  Uf A e           k disk    rename(titaa000:taanew)
001300060207     ftitaa30c  If   e           k disk
001400060207     FtitahNEW  Uf A e           k disk    rename(titah000:tahnew)
001500060207     ftitah30c  If   e           k disk
001600060207     FtitaSVNW  Uf A e           k disk    rename(titasv00:tasvnw)
001700060207     ftitasv1l  If   e           k disk
001800060207     FtitatNEW  Uf A e           k disk    rename(titat000:tatnew)
001900060207     ftitat30c  If   e           k disk
002000060207     Ftita4NEW  Uf A e           k disk    rename(tita4000:ta4new)
002100060207     ftita430c  If   e           k disk
002200060207     ftita7NEW  Uf A e           k disk    rename(tita7000:ta7new)
002300060207     ftita730c  If   e           k disk
002400060207     Ftita7VNW  Uf A e           k disk    rename(tita7v00:ta7vnw)
002500060207     ftita7v1l  If   e           k disk
002600060207     ftntmdNEW  Uf A e           k disk    rename(tntmd000:tmdnew)
002700060207     ftntmd01l  If   e           k disk
002800060207     ftncsbNEW  Uf A e           k disk    rename(tncsb000:csbnew)
002900060207     ftncsb03l  If   e           k disk
003000060207     ftncsaNEW  Uf A e           k disk    rename(tncsa000:csanew)
003100060207     ftncsa01l  If   e           k disk
003200060207     ftncsgNEW  Uf A e           k disk    rename(tncsg000:csgnew)
003300060207     ftncsg01l  If   e           k disk
003400060207     ftncsvNEW  Uf A e           k disk    rename(tncsv000:csvnew)
003500060207     ftncsv01l  If   e           k disk
003600070131     ftitaiNEW  Uf A e           k disk    rename(titai000:tainew)
003700070131     ftitai01l  If   e           k disk
003800090327     ffiar5NEW  Uf A e           k disk    rename(fiar5000:ar5new)
003900100211     ffiar531c  If   e           k disk
004000060207     f
004100060207     ffnblp01l  If   e           k disk
004200060207     ffnarb01l  If   e           k disk
004300060207     f                                     prefix(blp)
004400040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
004500060207      *
004600060207     d KPJBA         E DS
004700060207      *
004800060207     i
004900060207     ITITAS000      01
005000060207     ITITAS010      02
005100060207     ITITASP00      03
005200021008      *****************************************************************
005300021010     c     *ENTRY        PLIST
005400021010     c                   PARM                    KPJBA
005500040805     c
005600060207     c     Kbolla        KLIST
005700060207     c                   KFLD                    blpAAS
005800060207     c                   KFLD                    blpLNP
005900060207     c                   KFLD                    blpNRS
006000060207     c                   KFLD                    blpNSP
006100021009      *
006200021010      *---------------------------------------------------------------*
006300060207     c                   READ      Fnblp01l                               22
006400021010     c     *IN22         DOWEQ     *OFF
006500060207     c                   exsr      scrivi
006600040805     c
006700060207     c                   READ      fnblp01l                               22
006800021010     c                   ENDDO
006900060207     c
007000060210     c
007100060207     c                   READ      Fnarb01l                               22
007200060210     c
007300060207     c     *IN22         DOWEQ     *OFF
007400060210     c
007500060210     c* solo se non scaricata con blp
007600060210     c     kbolla        setll     fnblp01l                               35
007700060210     c  n35              exsr      scrivi
007800060207     c
007900060207     c                   READ      fnarb01l                               22
008000060207     c                   ENDDO
008100021010      *
008200040805     c                   EXCEPT    RINUM1
008300021011     c                   seton                                        LR
008400021009      *
008500060207      *---------------------------------------------------------------*
008600060207     c     SCRIVI        BEGSR
008700060207     c                   setoff                                       010203
008800060210     C     Kbolla        setll     titas30c
008900060210     C     Kbolla        reade     titas30c                               36
009000060210     c                   dow       not %eof(titas30c)
009100060207     c   01              write     tas0nEW                              61
009200060207     c   02              write     tas1nEW                              61
009300060207     c   03              write     tas1nEW                              61
009400060210     c  n61              add       1             $Tottas           7 0
009500060210     c   61              add       1             $Toterr           7 0
009600060210     C     Kbolla        reade     titas30c                               36
009700060210     c                   enddo
009800060210     c
009900060210     c                   if        not *in61
010000060207      *
010100060207     C     Kbolla        setll     titaa30c
010200060207     C     Kbolla        reade     titaa30c                               36
010300060207     c     *in36         doweq     *off
010400060207     c                   write     taaNEW                               61
010500060207     c                   add       1             $Tottaa           7 0
010600060207     C     Kbolla        reade     titaa30c                               36
010700060207     c                   enddo
010800060207      *
010900060207     C     Kbolla        setll     titah30c
011000060207     C     Kbolla        reade     titah30c                               36
011100060207     c     *in36         doweq     *off
011200060207     c                   write     tahNEW                               61
011300060207     c                   add       1             $Tottah           7 0
011400060207     C     Kbolla        reade     titah30c                               36
011500060207     c                   enddo
011600060207      *
011700060207     C     Kbolla        setll     titasv1l
011800060207     C     Kbolla        reade     titasv1l                               36
011900060207     c     *in36         doweq     *off
012000060207     c                   write     tasvnW                               61
012100060207     c                   add       1             $Tottasv          7 0
012200060207     C     Kbolla        reade     titasv1l                               36
012300060207     c                   enddo
012400060207      *
012500060207     C     Kbolla        setll     titat30c
012600060207     C     Kbolla        reade     titat30c                               36
012700060207     c     *in36         doweq     *off
012800060207     c                   write     tatNEW                               61
012900060207     c                   add       1             $Tottat           7 0
013000060207     C     Kbolla        reade     titat30c                               36
013100060207     c                   enddo
013200060207      *
013300060207     C     Kbolla        setll     tita430c
013400060207     C     Kbolla        reade     tita430c                               36
013500060207     c     *in36         doweq     *off
013600060207     c                   write     ta4NEW                               61
013700060207     c                   add       1             $Totta4           7 0
013800060207     C     Kbolla        reade     tita430c                               36
013900060207     c                   enddo
014000060207      *
014100060207     C     Kbolla        setll     tita730c
014200060207     C     Kbolla        reade     tita730c                               36
014300060207     c     *in36         doweq     *off
014400060207     c                   write     ta7NEW                               61
014500060207     c                   add       1             $Totta7           7 0
014600060207     C     Kbolla        reade     tita730c                               36
014700060207     c                   enddo
014800060207      *
014900060207     C     Kbolla        setll     tita7v1l
015000060207     C     Kbolla        reade     tita7v1l                               36
015100060207     c     *in36         doweq     *off
015200060207     c                   write     ta7vnW                               61
015300060207     c                   add       1             $Totta7v          7 0
015400060207     C     Kbolla        reade     tita7v1l                               36
015500060207     c                   enddo
015600060207      *
015700060207     C     Kbolla        setll     tntmd01l
015800060207     C     Kbolla        reade     tntmd01l                               36
015900060207     c     *in36         doweq     *off
016000060207     c                   write     tmdNEW                               61
016100060207     c                   add       1             $Tottmd           7 0
016200060207     C     Kbolla        reade     tntmd01l                               36
016300060207     c                   enddo
016400060207      *
016500060207     C     Kbolla        setll     tncsb03l
016600060207     C     Kbolla        reade     tncsb03l                               36
016700060207     c     *in36         doweq     *off
016800060207     c                   write     csbNEW                               61
016900060207     c                   add       1             $Totcsb           7 0
017000060207     C     Kbolla        reade     tncsb03l                               36
017100060207     c                   enddo
017200060207      *
017300060207     C     Kbolla        setll     tncsa01l
017400060207     C     Kbolla        reade     tncsa01l                               36
017500060207     c     *in36         doweq     *off
017600060207     c                   write     csaNEW                               61
017700060207     c                   add       1             $Totcsa           7 0
017800060207     C     Kbolla        reade     tncsa01l                               36
017900060207     c                   enddo
018000060207      *
018100060207     C     Kbolla        setll     tncsg01l
018200060207     C     Kbolla        reade     tncsg01l                               36
018300060207     c     *in36         doweq     *off
018400060207     c                   write     csgNEW                               61
018500060207     c                   add       1             $Totcsg           7 0
018600060207     C     Kbolla        reade     tncsg01l                               36
018700060207     c                   enddo
018800060207      *
018900060207     C     Kbolla        setll     tncsv01l
019000060207     C     Kbolla        reade     tncsv01l                               36
019100060207     c     *in36         doweq     *off
019200060207     c                   write     csvNEW                               61
019300060207     c                   add       1             $Totcsv           7 0
019400060207     C     Kbolla        reade     tncsv01l                               36
019500060207     c                   enddo
019600070131      *
019700070131     C     Kbolla        setll     titai01l
019800070131     C     Kbolla        reade     titai01l                               36
019900070131     c     *in36         doweq     *off
020000070131     c                   write     taiNEW                               61
020100070131     c                   add       1             $Tottai           7 0
020200070131     C     Kbolla        reade     titai01l                               36
020300070131     c                   enddo
020400090327      *
020500100211     C     Kbolla        setll     fiar531c
020600100211     C     Kbolla        reade     fiar531c                               36
020700090327     c     *in36         doweq     *off
020800090327     c                   write     ar5NEW                               61
020900090327     c                   add       1             $Totar5           7 0
021000100211     C     Kbolla        reade     fiar531c                               36
021100090327     c                   enddo
021200060207     c
021300060207     c                   endif
021400060207     c                   ENDSR
021500021010      *---------------------------------------------------------------*
021600040805     oQSYSPRT   E            RINUM1         2 02
021700060207     o                                         +  5 'BOLLE SEDE'
021800060207     o                                         +  5 'Fits41R'
021900021010     o                       UDATE             +  2 '  /  /  '
022000021010     o          E            RINUM1      1
022100060207     o                                         +  3 'TOT.Testata titas0 e 10:'
022200060207     o                       $TOTtas       2   +  2
022300060207     o                                         +  3 'TOT. TITAA:'
022400060207     o                       $TOTtaa       2   +  2
022500060207     o                                         +  3 'TOT. TITAH:'
022600060207     o                       $TOTtah       2   +  2
022700060207     o                                         +  3 'TOT. TITASV:'
022800060207     o                       $TOTtasv      2   +  2
022900060207     o          E            RINUM1      2
023000060207     o                                         +  3 'TOT. TITAT:'
023100060207     o                       $TOTtat       2   +  2
023200060207     o                                         +  3 'TOT. TITA4:'
023300060207     o                       $TOTta4       2   +  2
023400060207     o                                         +  3 'TOT. TITA7:'
023500060207     o                       $TOTta7       2   +  2
023600060207     o                                         +  3 'TOT. TITA7V:'
023700060207     o                       $TOTta7v      2   +  2
023800060207     o                                         +  3 'TOT. TNTMD:'
023900060207     o                       $TOTtmd       2   +  2
024000060207     o          E            RINUM1      2
024100060207     o                                         +  3 'TOT. TNCSB:'
024200060207     o                       $TOTcsb       2   +  2
024300060207     o                                         +  3 'TOT. TNCSA:'
024400060207     o                       $TOTcsa       2   +  2
024500060207     o                                         +  3 'TOT. TNCSG:'
024600060207     o                       $TOTcsg       2   +  2
024700060207     o                                         +  3 'TOT. TNCSV:'
024800060207     o                       $TOTcsv       2   +  2
024900070131     o                                         +  3 'TOT. TITAI:'
025000070131     o                       $TOTtai       2   +  2
025100090327     o          E            RINUM1      2
025200090327     o                                         +  3 'TOT. FIAR5:'
025300090327     o                       $TOTar5       2   +  2
025400060210     o          E            RINUM1      2
025500060210     o                                         +  3 'TOT. ERRORI:'
025600060210     o                       $TOTerr       2   +  2
