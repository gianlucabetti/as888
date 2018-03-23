000100060208     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200060208
000300060208      *------------------------------------------------------------------------*
000400060208      *                                                                        *
000500110527      *                 ALLINEA TABELLE X EasySped         ?                   *
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
004200110527     d*// ds03          e ds
004300060208     d ds1a          e ds
004400060208     d ds1b          e ds
004500060208     d ds1p          e ds
004600060208     d ds15          e ds
004700060208     d ds3a          e ds
004800060208     d ds5e          e ds
004900060208     d dtba          e ds
005000100111     d dlts          e ds
005100060208     d kpjba         e ds
005200060208     d tibs34ds      e ds
005300060208
005400060208      *   C O S T A N T I
005500060208      * titolo videata (lunghezza massima 35)
005600110527     d vtctit          c                   const('** ESTRAZIONE TABELLE X EASY-
005700110527     d                                     SPED **')
005800060208
005900060208      *------------------------------------------------------------------------*
006000060208
006100060208      * pulisco subfile
006200060208     c                   exsr      sr_pulsfl
006300060208
006400060208      * carico il subfile
006500060208     c                   exsr      sr_carsfl
006600060208
006700060208     c     emisfl        tag
006800060208
006900060208      * emetto il Subfile
007000060208     c                   exsr      sr_emisfl
007100060208      * F3=Fine
007200060208     c                   if        *inkc
007300060208     c                   goto      fine
007400060208     c                   endif
007500060208
007600060208      * conferma
007700060321     c                   if        *inkf
007800060321      * clrpfm del file prima di scriverlo
007900060321     c                   call(e)   'QCMDEXC'
008000060321     c                   parm                    cmd(1)
008100060321     c                   parm                    lengh
008200060321      * se riuscito comando vado avanti con l'elaborazione
008300060321     c                   if        not %error
008400060321     c                   open      tntbepcv
008500060321     c                   exsr      sr_conferma
008600060321     c                   endif
008700060321     c                   endif
008800060208
008900060208     c                   if        not *inkc and not *inkf
009000060208     c                   goto      emisfl
009100060208     c                   endif
009200060208
009300060208     c     fine          tag
009400060208
009500060208     c                   eval      *Inlr = *on
009600060208
009700060208      *------------------------------------------------------------------------*
009800060208      * PULISCO IL SUBFILE
009900060208      *------------------------------------------------------------------------*
010000060208     c     sr_pulsfl     begsr
010100060208
010200060208     c                   clear                   nrr
010300060208     c                   eval      *in20 = *off
010400060208     c                   eval      *in21 = *off
010500060208     c                   write     si49c01
010600060208     c                   eval      *in20 = *on
010700060208     c                   eval      *in21 = *on
010800060208
010900060208     c                   endsr
011000060208
011100060208      *------------------------------------------------------------------------*
011200060208      * CARICO IL SUBFILE
011300060208      *------------------------------------------------------------------------*
011400060208     c     sr_carsfl     begsr
011500060208
011600060208      * carico solo le tabelle che devo passare a pc vas
011700060208     c                   eval      xx = 1
011800060208     c                   for       xx by 1 to 99
011900060208     c                   if        sktab(xx) = *blanks
012000060208     c                   leave
012100060208     c                   endif
012200060208      * carico da tabel
012300060208     c                   if        %subst(sktab(xx):3:1) = *blanks
012400060208     c                   eval      tblkut = 1
012500060208     c                   clear                   tblcod
012600060208     c                   move      *all'0'       tblkey
012700060208     c                   eval      w002a = %subst(sktab(xx):1:2)
012800060208     c                   move      w002a         tblkey
012900060208     c     ktabel        chain     tabel00f
013000060208     c                   if        %found(tabel00f)
013100060208     c                   Eval      v1scod = sktab(xx)
013200060208     c                   Eval      v1sdes = tbluni
013300060208      * scrivo sbfl
013400060208     c                   add       1             nrr
013500060208     c                   write     si49s01
013600060208     c                   endif
013700060208      * carico da tntbe
013800060208     c                   else
013900060208     c                   clear                   tbecod
014000060208     c                   move      *all'0'       tbeke1
014100060208     c                   move      sktab(xx)     tbeke1
014200060208     c     ktntbe        chain     tntbe01l
014300060208     c                   if        %found(tntbe01l)
014400060208     c                   Eval      v1scod = sktab(xx)
014500060208     c                   Eval      v1sdes = tbeuni
014600060208      * scrivo sbfl
014700060208     c                   add       1             nrr
014800060208     c                   write     si49s01
014900060208     c                   endif
015000060208     c                   endif
015100060208     c                   endfor
015200060208
015300060208     c                   eval      *in31 = *on
015400060208
015500060208     c                   endsr
015600060208
015700060208      *------------------------------------------------------------------------*
015800060208      * EMETTO IL SUBFILE
015900060208      *------------------------------------------------------------------------*
016000060208     c     sr_emisfl     begsr
016100060208
016200060208     c                   write     si49t01
016300060208     c                   write     si49z01
016400060208     c                   exfmt     si49c01
016500060208
016600060208     c                   endsr
016700060208
016800060208      *------------------------------------------------------------------------*
016900060208      * F6 - Conferma
017000060208      *------------------------------------------------------------------------*
017100060208     c     sr_conferma   begsr
017200060208
017300060209     c                   clear                   nrr
017400110527      *
017500060208      * leggo dal subfile quello che devo caricare
017600110527do  1c                   DO        *hival
017700110527      *
017800060208     c                   eval      nrr = nrr +1
017900060208     c     nrr           chain     si49s01                            30
018000060208     c                   if        *in30
018100060208     c                   leave
018200060208     c                   endif
018300110527      *
018400060208      * carico le tabelle nel file per il pc vas
018500060208     c                   select
018600110527      *
018700060208      * tabella CV
018800060208     c                   when      v1scod = 'CV'
018900060208     c                   eval      tblcod = v1scod
019000060208     c     ktabel1       setll     tabel00f
019100060208     c                   do        *hival
019200060208     c     ktabel1       reade     tabel00f
019300060208     c                   if        %eof(tabel00f)
019400060208     c                   leave
019500060208     c                   endif
019600060209     c                   if        tblflg <> *blanks
019700060209     c                   iter
019800060209     c                   endif
019900060208     c                   eval      dscv = tbluni
020000060208     c                   if        §cvvas <> 'S'
020100060208     c                   iter
020200060208     c                   endif
020300060208      * scrivo il file pr pc vas
020400060208     c                   exsr      sr_scrivi1
020500060208     c                   enddo
020600110527      *
020700060208      * tabella TR
020800060208     c                   when      v1scod = 'TR'
020900060208     c                   eval      tblcod = v1scod
021000060208     c     ktabel1       setll     tabel00f
021100060208     c                   do        *hival
021200060208     c     ktabel1       reade     tabel00f
021300060208     c                   if        %eof(tabel00f)
021400060208     c                   leave
021500060208     c                   endif
021600060209     c                   if        tblflg <> *blanks
021700060209     c                   iter
021800060209     c                   endif
021900060208     c                   eval      dstr = tbluni
022000060208     c                   if        §trvas <> 'S'
022100060208     c                   iter
022200060208     c                   endif
022300060208      * scrivo il file pr pc vas
022400060208     c                   exsr      sr_scrivi1
022500060208     c                   enddo
022600110527      *
022700060208      * tabella 03
022800060208     c                   when      v1scod = '03'
022900060208     c                   eval      tblcod = v1scod
023000060208     c     ktabel1       setll     tabel00f
023100060208     c                   do        *hival
023200060208     c     ktabel1       reade     tabel00f
023300060208     c                   if        %eof(tabel00f)
023400060208     c                   leave
023500060208     c                   endif
023600060209     c                   if        tblflg <> *blanks
023700060209     c                   iter
023800060209     c                   endif
023900060208      * scrivo il file pr pc vas
024000060208     c                   exsr      sr_scrivi1
024100060208     c                   enddo
024200110527      *
024300060208      * tabella 1A
024400060208     c                   when      v1scod = '1A'
024500060208     c                   eval      tblcod = v1scod
024600060208     c     ktabel1       setll     tabel00f
024700060208     c                   do        *hival
024800060208     c     ktabel1       reade     tabel00f
024900060208     c                   if        %eof(tabel00f)
025000060208     c                   leave
025100060208     c                   endif
025200060209     c                   if        tblflg <> *blanks
025300060209     c                   iter
025400060209     c                   endif
025500060208     c                   eval      ds1a = tbluni
025600060208     c                   if        §1avas <> 'S'
025700060208     c                   iter
025800060208     c                   endif
025900060208      * scrivo il file pr pc vas
026000060208     c                   exsr      sr_scrivi1
026100060208     c                   enddo
026200110527      *
026300060208      * tabella 1B
026400060208     c                   when      v1scod = '1B'
026500060208     c                   eval      tblcod = v1scod
026600060208     c     ktabel1       setll     tabel00f
026700060208     c                   do        *hival
026800060208     c     ktabel1       reade     tabel00f
026900060208     c                   if        %eof(tabel00f)
027000060208     c                   leave
027100060208     c                   endif
027200060209     c                   if        tblflg <> *blanks
027300060209     c                   iter
027400060209     c                   endif
027500060208     c                   eval      ds1b = tbluni
027600060208     c                   if        §1bvas <> 'S'
027700060208     c                   iter
027800060208     c                   endif
027900060208      * scrivo il file pr pc vas
028000060208     c                   exsr      sr_scrivi1
028100060208     c                   enddo
028200110527      *
028300060208      * tabella 1P
028400060208     c                   when      v1scod = '1P'
028500060208     c                   eval      tblcod = v1scod
028600060208     c     ktabel1       setll     tabel00f
028700060208     c                   do        *hival
028800060208     c     ktabel1       reade     tabel00f
028900060208     c                   if        %eof(tabel00f)
029000060208     c                   leave
029100060208     c                   endif
029200060209     c                   if        tblflg <> *blanks
029300060209     c                   iter
029400060209     c                   endif
029500060208     c                   eval      ds1p = tbluni
029600060208     c                   if        §1pvas <> 'S'
029700060208     c                   iter
029800060208     c                   endif
029900060208      * scrivo il file pr pc vas
030000060208     c                   exsr      sr_scrivi1
030100060208     c                   enddo
030200110527      *
030300060208      * tabella 15
030400060208     c                   when      v1scod = '15'
030500060208     c                   eval      tblcod = v1scod
030600060208     c     ktabel1       setll     tabel00f
030700060208     c                   do        *hival
030800060208     c     ktabel1       reade     tabel00f
030900060208     c                   if        %eof(tabel00f)
031000060208     c                   leave
031100060208     c                   endif
031200060209     c                   if        tblflg <> *blanks
031300060209     c                   iter
031400060209     c                   endif
031500060208     c                   eval      ds15 = tbluni
031600060208     c                   if        §15vas <> 'S'
031700060208     c                   iter
031800060208     c                   endif
031900060208      * scrivo il file pr pc vas
032000060208     c                   exsr      sr_scrivi1
032100060208     c                   enddo
032200110527      *
032300060208      * tabella 3A
032400060208     c                   when      v1scod = '3A'
032500060208     c                   eval      tblcod = v1scod
032600060208     c     ktabel1       setll     tabel00f
032700060208     c                   do        *hival
032800060208     c     ktabel1       reade     tabel00f
032900060208     c                   if        %eof(tabel00f)
033000060208     c                   leave
033100060208     c                   endif
033200060209     c                   if        tblflg <> *blanks
033300060209     c                   iter
033400060209     c                   endif
033500060208     c                   eval      ds3a = tbluni
033600060208     c                   if        §3avas <> 'S'
033700060208     c                   iter
033800060208     c                   endif
033900060208      * scrivo il file pr pc vas
034000060208     c                   exsr      sr_scrivi1
034100060208     c                   enddo
034200110527      *
034300060208      * tabella 5E
034400060208     c                   when      v1scod = '5E'
034500060208     c                   eval      tblcod = v1scod
034600060208     c     ktabel1       setll     tabel00f
034700060208     c                   do        *hival
034800060208     c     ktabel1       reade     tabel00f
034900060208     c                   if        %eof(tabel00f)
035000060208     c                   leave
035100060208     c                   endif
035200060209     c                   if        tblflg <> *blanks
035300060209     c                   iter
035400060209     c                   endif
035500060208     c                   eval      ds5e = tbluni
035600060208     c                   if        §5evas <> 'S'
035700060208     c                   iter
035800060208     c                   endif
035900060208      * scrivo il file pr pc vas
036000060208     c                   exsr      sr_scrivi1
036100060208     c                   enddo
036200110527      *
036300060208      * tabella TBA
036400060208     c                   when      v1scod = 'TBA'
036500060208     c                   eval      tbecod = v1scod
036600060208     c     tbecod        setll     tntbe01l
036700060208     c                   do        *hival
036800060208     c     tbecod        reade     tntbe01l
036900060208     c                   if        %eof(tntbe01l)
037000060208     c                   leave
037100060208     c                   endif
037200060209     c                   if        tbeatb <> *blanks
037300060209     c                   iter
037400060209     c                   endif
037500060208     c                   eval      dtba = tbeuni
037600060208     c                   if        §tbavas <> 'S'
037700060208     c                   iter
037800060208     c                   endif
037900060208      * scrivo il file pr pc vas
038000060208     c                   exsr      sr_scrivi2
038100060208     c                   enddo
038200110527      *
038300100111      * tabella LTS
038400100111     c                   when      v1scod = 'LTS'
038500100111     c                   eval      tbecod = v1scod
038600100111     c     tbecod        setll     tntbe01l
038700100111     c                   do        *hival
038800100111     c     tbecod        reade     tntbe01l
038900100111     c                   if        %eof(tntbe01l)
039000100111     c                   leave
039100100111     c                   endif
039200100111     c                   if        tbeatb <> *blanks
039300100111     c                   iter
039400100111     c                   endif
039500100111     c                   eval      dlts = tbeuni
039600100111     c                   if        §ltsvas <> 'S'
039700100111     c                   iter
039800100111     c                   endif
039900100111      * scrivo il file pr pc vas
040000100111     c                   exsr      sr_scrivi2
040100100111     c                   enddo
040200110527      *
040300110527      * tabella STA
040400110527     c                   when      v1scod = 'STA'
040500110527     c                   eval      tbecod = v1scod
040600110527     c     tbecod        setll     tntbe01l
040700110527     c                   do        *hival
040800110527     c     tbecod        reade     tntbe01l
040900110527     c                   if        %eof(tntbe01l)
041000110527     c                   leave
041100110527     c                   endif
041200110527     c                   if        tbeatb <> *blanks
041300110527     c                   iter
041400110527     c                   endif
041500110527      * scrivo il file pr pc vas
041600110527     c                   exsr      sr_scrivi2
041700110527     c                   enddo
041800110527      *
041900060208     c                   endsl
042000110527      *
042100110527e   1c                   ENDDO
042200060208
042300060208     c                   endsr
042400060208
042500060208      *------------------------------------------------------------------------*
042600060208      * SCRIVO TNTBEPCV DA TABEL
042700060208      *------------------------------------------------------------------------*
042800060208     c     sr_scrivi1    begsr
042900060208
043000060208     c                   clear                   tntbepc
043100060208     c                   eval      v_tbelin = '1'
043200060208     c                   eval      v_tbecod = tblcod
043300060208     c                   eval      v_tbeke1 = tblkey
043400060208     c                   eval      v_tbeuni = tbluni
043500060208     c                   write     tntbepc
043600060208
043700060208     c                   endsr
043800060208
043900060208      *------------------------------------------------------------------------*
044000060208      * SCRIVO TNTBEPCV DA TNTBE
044100060208      *------------------------------------------------------------------------*
044200060208     c     sr_scrivi2    begsr
044300060208
044400060208     c                   clear                   tntbepc
044500060208     c                   eval      v_tbelin = '1'
044600060208     c                   eval      v_tbecod = tbecod
044700060208     c                   eval      v_tbeke1 = tbeke1
044800060208     c                   eval      v_tbeke2 = tbeke2
044900060208     c                   eval      v_tbeuni = tbeuni
045000060208     c                   write     tntbepc
045100060208
045200060208     c                   endsr
045300060208
045400060208      *------------------------------------------------------------------------*
045500060208      * ROUTINE INIZIALE
045600060208      *------------------------------------------------------------------------*
045700060208     c     *inzsr        begsr
045800060208
045900060208     c     *entry        plist
046000060208     c                   parm                    kpjba
046100060208
046200060208     c     *dtaara       define    §azute        azuteds
046300060208     c     *dtaara       define    §datiute      ddatiute
046400060208     c                   in(e)     *dtaara
046500060208     c                   if        %error or rsut = *blanks
046600060208     c                   clear                   tibs34ds
046700060208     c                   call      'TIBS34R'
046800060208     c                   parm                    tibs34ds
046900060208     c                   in        *dtaara
047000060208     c                   endif
047100060208
047200060208      * klist
047300060208     c     ktabel        klist
047400060208     c                   kfld                    tblkut
047500060208     c                   kfld                    tblcod
047600060208     c                   kfld                    tblkey
047700060208     c     ktabel1       klist
047800060208     c                   kfld                    tblkut
047900060208     c                   kfld                    tblcod
048000060208
048100060208     c     ktntbe        klist
048200060208     c                   kfld                    tbecod
048300060208     c                   kfld                    tbeke1
048400060208
048500060208     c                   endsr
048600060208      *------------------------------------------------------------------------*
048700060321** cmd
048800060321CLRPFM FILE(TNTBEPCV)
048900060208** SKTAB  Lungh. 03                                                            *
049000060208CV                                                                            01
049100060208TR                                                                            02
04920006020803                                                                            03
0493000602081A                                                                            04
0494000602081B                                                                            05
0495000602081P                                                                            06
04960006020815                                                                            07
0497000602083A                                                                            08
0498000602085E                                                                            09
049900060208TBA                                                                           09
050000100111LTS                                                                           10
050100110527STA                                                                           11
