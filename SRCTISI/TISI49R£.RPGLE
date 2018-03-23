000100060208     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200060208
000300060208      *------------------------------------------------------------------------*
000400060208      *                                                                        *
000500060208      *                 ALLINEA TABELLE X ELOS2005         ?                   *
000600060208      *                                                                        *
000700060208      *------------------------------------------------------------------------*
000800060208
000900060208     ftntbe01l  if   e           k disk
001000060208     ftabel00f  if   e           k disk
001100060208     ftntbepcv  o    e             disk    rename(tntbepcv:tntbepc) prefix(v_)
001200060321     f                                     usropn
001300060208     ftisi49d   cf   e             workstn sfile(si49s01:nrr)
001400060208
001500060208      *------------------------------------------------------------------------*
001600060208      *  RIEPILOGO INDICATORI
001700060208      *------------------------------------------------------------------------*
001800060208      * 20 - gestione sfl
001900060208      * 21 - gestione sfl
002000060208      * 31 - fine sfl
002100060208      *------------------------------------------------------------------------*
002200060208
002300060208      *   V A R I A B I L I
002400060321     d lengh           s             15  5 inz(80)
002500060208     d nrr             s              4  0
002600060208     d xx              s              3  0
002700060208     d w002a           s              2
002800060208
002900060208      *   S C H I E R E
003000060321     d cmd             s             80    dim(1) ctdata perrcd(1)
003100060208     d sktab           s              3    dim(99) ctdata perrcd(1)
003200060208
003300060208      *   D S   I N T E R N E / E S T E R N E
003400060208
003500060208     d                sds
003600060208     d  vtcpgm                 1     10
003700060208
003800060208     d azuteds       e ds                  extname(azute00f)
003900060208     d ddatiute      e ds
004000060208     d dscv          e ds
004100060208     d dstr          e ds
004200060208     d ds03          e ds
004300060208     d ds1a          e ds
004400060208     d ds1b          e ds
004500060208     d ds1p          e ds
004600060208     d ds15          e ds
004700060208     d ds3a          e ds
004800060208     d ds5e          e ds
004900060208     d dtba          e ds
005000060208     d kpjba         e ds
005100060208     d tibs34ds      e ds
005200060208
005300060208      *   C O S T A N T I
005400060208      * titolo videata (lunghezza massima 35)
005500060214     d vtctit          c                   const('** ESTRAZIONE TABELLE X ELOS-
005600060214     d                                     2005 **')
005700060208
005800060208      *------------------------------------------------------------------------*
005900060208
006000060208      * pulisco subfile
006100060208     c                   exsr      sr_pulsfl
006200060208
006300060208      * carico il subfile
006400060208     c                   exsr      sr_carsfl
006500060208
006600060208     c     emisfl        tag
006700060208
006800060208      * emetto il Subfile
006900060208     c                   exsr      sr_emisfl
007000060208      * F3=Fine
007100060208     c                   if        *inkc
007200060208     c                   goto      fine
007300060208     c                   endif
007400060208
007500060208      * conferma
007600060321     c                   if        *inkf
007700060321      * clrpfm del file prima di scriverlo
007800060321     c                   call(e)   'QCMDEXC'
007900060321     c                   parm                    cmd(1)
008000060321     c                   parm                    lengh
008100060321      * se riuscito comando vado avanti con l'elaborazione
008200060321     c                   if        not %error
008300060321     c                   open      tntbepcv
008400060321     c                   exsr      sr_conferma
008500060321     c                   endif
008600060321     c                   endif
008700060208
008800060208     c                   if        not *inkc and not *inkf
008900060208     c                   goto      emisfl
009000060208     c                   endif
009100060208
009200060208     c     fine          tag
009300060208
009400060208     c                   eval      *Inlr = *on
009500060208
009600060208      *------------------------------------------------------------------------*
009700060208      * PULISCO IL SUBFILE
009800060208      *------------------------------------------------------------------------*
009900060208     c     sr_pulsfl     begsr
010000060208
010100060208     c                   clear                   nrr
010200060208     c                   eval      *in20 = *off
010300060208     c                   eval      *in21 = *off
010400060208     c                   write     si49c01
010500060208     c                   eval      *in20 = *on
010600060208     c                   eval      *in21 = *on
010700060208
010800060208     c                   endsr
010900060208
011000060208      *------------------------------------------------------------------------*
011100060208      * CARICO IL SUBFILE
011200060208      *------------------------------------------------------------------------*
011300060208     c     sr_carsfl     begsr
011400060208
011500060208      * carico solo le tabelle che devo passare a pc vas
011600060208     c                   eval      xx = 1
011700060208     c                   for       xx by 1 to 99
011800060208     c                   if        sktab(xx) = *blanks
011900060208     c                   leave
012000060208     c                   endif
012100060208      * carico da tabel
012200060208     c                   if        %subst(sktab(xx):3:1) = *blanks
012300060208     c                   eval      tblkut = 1
012400060208     c                   clear                   tblcod
012500060208     c                   move      *all'0'       tblkey
012600060208     c                   eval      w002a = %subst(sktab(xx):1:2)
012700060208     c                   move      w002a         tblkey
012800060208     c     ktabel        chain     tabel00f
012900060208     c                   if        %found(tabel00f)
013000060208     c                   Eval      v1scod = sktab(xx)
013100060208     c                   Eval      v1sdes = tbluni
013200060208      * scrivo sbfl
013300060208     c                   add       1             nrr
013400060208     c                   write     si49s01
013500060208     c                   endif
013600060208      * carico da tntbe
013700060208     c                   else
013800060208     c                   clear                   tbecod
013900060208     c                   move      *all'0'       tbeke1
014000060208     c                   move      sktab(xx)     tbeke1
014100060208     c     ktntbe        chain     tntbe01l
014200060208     c                   if        %found(tntbe01l)
014300060208     c                   Eval      v1scod = sktab(xx)
014400060208     c                   Eval      v1sdes = tbeuni
014500060208      * scrivo sbfl
014600060208     c                   add       1             nrr
014700060208     c                   write     si49s01
014800060208     c                   endif
014900060208     c                   endif
015000060208     c                   endfor
015100060208
015200060208     c                   eval      *in31 = *on
015300060208
015400060208     c                   endsr
015500060208
015600060208      *------------------------------------------------------------------------*
015700060208      * EMETTO IL SUBFILE
015800060208      *------------------------------------------------------------------------*
015900060208     c     sr_emisfl     begsr
016000060208
016100060208     c                   write     si49t01
016200060208     c                   write     si49z01
016300060208     c                   exfmt     si49c01
016400060208
016500060208     c                   endsr
016600060208
016700060208      *------------------------------------------------------------------------*
016800060208      * F6 - Conferma
016900060208      *------------------------------------------------------------------------*
017000060208     c     sr_conferma   begsr
017100060208
017200060209     c                   clear                   nrr
017300060208      * leggo dal subfile quello che devo caricare
017400060208     c                   do        *hival
017500060208     c                   eval      nrr = nrr +1
017600060208     c     nrr           chain     si49s01                            30
017700060208     c                   if        *in30
017800060208     c                   leave
017900060208     c                   endif
018000060208      * carico le tabelle nel file per il pc vas
018100060208     c                   select
018200060208      * tabella CV
018300060208     c                   when      v1scod = 'CV'
018400060208     c                   eval      tblcod = v1scod
018500060208     c     ktabel1       setll     tabel00f
018600060208     c                   do        *hival
018700060208     c     ktabel1       reade     tabel00f
018800060208     c                   if        %eof(tabel00f)
018900060208     c                   leave
019000060208     c                   endif
019100060209     c                   if        tblflg <> *blanks
019200060209     c                   iter
019300060209     c                   endif
019400060208     c                   eval      dscv = tbluni
019500060208     c                   if        §cvvas <> 'S'
019600060208     c                   iter
019700060208     c                   endif
019800060208      * scrivo il file pr pc vas
019900060208     c                   exsr      sr_scrivi1
020000060208     c                   enddo
020100060208      * tabella TR
020200060208     c                   when      v1scod = 'TR'
020300060208     c                   eval      tblcod = v1scod
020400060208     c     ktabel1       setll     tabel00f
020500060208     c                   do        *hival
020600060208     c     ktabel1       reade     tabel00f
020700060208     c                   if        %eof(tabel00f)
020800060208     c                   leave
020900060208     c                   endif
021000060209     c                   if        tblflg <> *blanks
021100060209     c                   iter
021200060209     c                   endif
021300060208     c                   eval      dstr = tbluni
021400060208     c                   if        §trvas <> 'S'
021500060208     c                   iter
021600060208     c                   endif
021700060208      * scrivo il file pr pc vas
021800060208     c                   exsr      sr_scrivi1
021900060208     c                   enddo
022000060208      * tabella 03
022100060208     c                   when      v1scod = '03'
022200060208     c                   eval      tblcod = v1scod
022300060208     c     ktabel1       setll     tabel00f
022400060208     c                   do        *hival
022500060208     c     ktabel1       reade     tabel00f
022600060208     c                   if        %eof(tabel00f)
022700060208     c                   leave
022800060208     c                   endif
022900060209     c                   if        tblflg <> *blanks
023000060209     c                   iter
023100060209     c                   endif
023200060208     c                   eval      ds03 = tbluni
023300060310      * tutte
023400060310     c**!!!              if        §03vas <> 'S'
023500060310     c**!!!              iter
023600060310     c**!!!              endif
023700060208      * scrivo il file pr pc vas
023800060208     c                   exsr      sr_scrivi1
023900060208     c                   enddo
024000060208      * tabella 1A
024100060208     c                   when      v1scod = '1A'
024200060208     c                   eval      tblcod = v1scod
024300060208     c     ktabel1       setll     tabel00f
024400060208     c                   do        *hival
024500060208     c     ktabel1       reade     tabel00f
024600060208     c                   if        %eof(tabel00f)
024700060208     c                   leave
024800060208     c                   endif
024900060209     c                   if        tblflg <> *blanks
025000060209     c                   iter
025100060209     c                   endif
025200060208     c                   eval      ds1a = tbluni
025300060208     c                   if        §1avas <> 'S'
025400060208     c                   iter
025500060208     c                   endif
025600060208      * scrivo il file pr pc vas
025700060208     c                   exsr      sr_scrivi1
025800060208     c                   enddo
025900060208      * tabella 1B
026000060208     c                   when      v1scod = '1B'
026100060208     c                   eval      tblcod = v1scod
026200060208     c     ktabel1       setll     tabel00f
026300060208     c                   do        *hival
026400060208     c     ktabel1       reade     tabel00f
026500060208     c                   if        %eof(tabel00f)
026600060208     c                   leave
026700060208     c                   endif
026800060209     c                   if        tblflg <> *blanks
026900060209     c                   iter
027000060209     c                   endif
027100060208     c                   eval      ds1b = tbluni
027200060208     c                   if        §1bvas <> 'S'
027300060208     c                   iter
027400060208     c                   endif
027500060208      * scrivo il file pr pc vas
027600060208     c                   exsr      sr_scrivi1
027700060208     c                   enddo
027800060208      * tabella 1P
027900060208     c                   when      v1scod = '1P'
028000060208     c                   eval      tblcod = v1scod
028100060208     c     ktabel1       setll     tabel00f
028200060208     c                   do        *hival
028300060208     c     ktabel1       reade     tabel00f
028400060208     c                   if        %eof(tabel00f)
028500060208     c                   leave
028600060208     c                   endif
028700060209     c                   if        tblflg <> *blanks
028800060209     c                   iter
028900060209     c                   endif
029000060208     c                   eval      ds1p = tbluni
029100060208     c                   if        §1pvas <> 'S'
029200060208     c                   iter
029300060208     c                   endif
029400060208      * scrivo il file pr pc vas
029500060208     c                   exsr      sr_scrivi1
029600060208     c                   enddo
029700060208      * tabella 15
029800060208     c                   when      v1scod = '15'
029900060208     c                   eval      tblcod = v1scod
030000060208     c     ktabel1       setll     tabel00f
030100060208     c                   do        *hival
030200060208     c     ktabel1       reade     tabel00f
030300060208     c                   if        %eof(tabel00f)
030400060208     c                   leave
030500060208     c                   endif
030600060209     c                   if        tblflg <> *blanks
030700060209     c                   iter
030800060209     c                   endif
030900060208     c                   eval      ds15 = tbluni
031000060208     c                   if        §15vas <> 'S'
031100060208     c                   iter
031200060208     c                   endif
031300060208      * scrivo il file pr pc vas
031400060208     c                   exsr      sr_scrivi1
031500060208     c                   enddo
031600060208      * tabella 3A
031700060208     c                   when      v1scod = '3A'
031800060208     c                   eval      tblcod = v1scod
031900060208     c     ktabel1       setll     tabel00f
032000060208     c                   do        *hival
032100060208     c     ktabel1       reade     tabel00f
032200060208     c                   if        %eof(tabel00f)
032300060208     c                   leave
032400060208     c                   endif
032500060209     c                   if        tblflg <> *blanks
032600060209     c                   iter
032700060209     c                   endif
032800060208     c                   eval      ds3a = tbluni
032900060208     c                   if        §3avas <> 'S'
033000060208     c                   iter
033100060208     c                   endif
033200060208      * scrivo il file pr pc vas
033300060208     c                   exsr      sr_scrivi1
033400060208     c                   enddo
033500060208      * tabella 5E
033600060208     c                   when      v1scod = '5E'
033700060208     c                   eval      tblcod = v1scod
033800060208     c     ktabel1       setll     tabel00f
033900060208     c                   do        *hival
034000060208     c     ktabel1       reade     tabel00f
034100060208     c                   if        %eof(tabel00f)
034200060208     c                   leave
034300060208     c                   endif
034400060209     c                   if        tblflg <> *blanks
034500060209     c                   iter
034600060209     c                   endif
034700060208     c                   eval      ds5e = tbluni
034800060208     c                   if        §5evas <> 'S'
034900060208     c                   iter
035000060208     c                   endif
035100060208      * scrivo il file pr pc vas
035200060208     c                   exsr      sr_scrivi1
035300060208     c                   enddo
035400060208      * tabella TBA
035500060208     c                   when      v1scod = 'TBA'
035600060208     c                   eval      tbecod = v1scod
035700060208     c     tbecod        setll     tntbe01l
035800060208     c                   do        *hival
035900060208     c     tbecod        reade     tntbe01l
036000060208     c                   if        %eof(tntbe01l)
036100060208     c                   leave
036200060208     c                   endif
036300060209     c                   if        tbeatb <> *blanks
036400060209     c                   iter
036500060209     c                   endif
036600060208     c                   eval      dtba = tbeuni
036700060208     c                   if        §tbavas <> 'S'
036800060208     c                   iter
036900060208     c                   endif
037000060208      * scrivo il file pr pc vas
037100060208     c                   exsr      sr_scrivi2
037200060208     c                   enddo
037300060208     c                   endsl
037400060208
037500060208     c                   enddo
037600060208
037700060208     c                   endsr
037800060208
037900060208      *------------------------------------------------------------------------*
038000060208      * SCRIVO TNTBEPCV DA TABEL
038100060208      *------------------------------------------------------------------------*
038200060208     c     sr_scrivi1    begsr
038300060208
038400060208     c                   clear                   tntbepc
038500060208     c                   eval      v_tbelin = '1'
038600060208     c                   eval      v_tbecod = tblcod
038700060208     c                   eval      v_tbeke1 = tblkey
038800060208     c                   eval      v_tbeuni = tbluni
038900060208     c                   write     tntbepc
039000060208
039100060208     c                   endsr
039200060208
039300060208      *------------------------------------------------------------------------*
039400060208      * SCRIVO TNTBEPCV DA TNTBE
039500060208      *------------------------------------------------------------------------*
039600060208     c     sr_scrivi2    begsr
039700060208
039800060208     c                   clear                   tntbepc
039900060208     c                   eval      v_tbelin = '1'
040000060208     c                   eval      v_tbecod = tbecod
040100060208     c                   eval      v_tbeke1 = tbeke1
040200060208     c                   eval      v_tbeke2 = tbeke2
040300060208     c                   eval      v_tbeuni = tbeuni
040400060208     c                   write     tntbepc
040500060208
040600060208     c                   endsr
040700060208
040800060208      *------------------------------------------------------------------------*
040900060208      * ROUTINE INIZIALE
041000060208      *------------------------------------------------------------------------*
041100060208     c     *inzsr        begsr
041200060208
041300060208     c     *entry        plist
041400060208     c                   parm                    kpjba
041500060208
041600060208     c     *dtaara       define    §azute        azuteds
041700060208     c     *dtaara       define    §datiute      ddatiute
041800060208     c                   in(e)     *dtaara
041900060208     c                   if        %error or rsut = *blanks
042000060208     c                   clear                   tibs34ds
042100060208     c                   call      'TIBS34R'
042200060208     c                   parm                    tibs34ds
042300060208     c                   in        *dtaara
042400060208     c                   endif
042500060208
042600060208      * klist
042700060208     c     ktabel        klist
042800060208     c                   kfld                    tblkut
042900060208     c                   kfld                    tblcod
043000060208     c                   kfld                    tblkey
043100060208     c     ktabel1       klist
043200060208     c                   kfld                    tblkut
043300060208     c                   kfld                    tblcod
043400060208
043500060208     c     ktntbe        klist
043600060208     c                   kfld                    tbecod
043700060208     c                   kfld                    tbeke1
043800060208
043900060208     c                   endsr
044000060208      *------------------------------------------------------------------------*
044100060321** cmd
044200060321CLRPFM FILE(TNTBEPCV)
044300060208** SKTAB  Lungh. 03                                                            *
044400060208CV                                                                            01
044500060208TR                                                                            02
04460006020803                                                                            03
0447000602081A                                                                            04
0448000602081B                                                                            05
0449000602081P                                                                            06
04500006020815                                                                            07
0451000602083A                                                                            08
0452000602085E                                                                            09
045300060208TBA                                                                           09
