000100010925      *PARMS DYNUSRPRF(*OWNER)
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000502160223     Fwfexportl UF A E           k DISK
000503160223     D rrn             s              9  0
000504160223     D compd           s             19  5
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000801160223     C     Key           KLIST
000802160223     C                   Kfld                    waas
000803160223     C                   Kfld                    wlnp
000804160223     C                   Kfld                    wnrs
000805160223     C                   Kfld                    wnsp
000806160223     C                   Kfld                    wtbl
000807160223     C/EXEC SQL
000808160223     C+ delete from gaitraazm/wfexportf
000809160223     C/END-EXEC
000810160223     C/EXEC SQL
000811160223     C+ delete from gaitraazm/wfexptotf
000812160223     C/END-EXEC
001210160222     c* estrae spedizioni export conteggiando competenze ricavi e ricarichi
001211050408     C/EXEC SQL
001212160223     C+ insert into gaitraazm/wfexportf
001213160223     C+ (waas, wlnp, wnrs, wnsp, wtbl, wlna , wricavo, wcompe, wricarico)
001214160223     C+ SELECT TASAAS, TASLNP, TASNRS, TASNSP, tastbl, taslna,
001215160223     C+ TASIMV,
001217160223     C+ (CETPAR +
001218160222     C+ CETPAM +
001219160222     C+ CETPAU +
001220160222     C+ CETPAV +
001221160222     C+ CETARD +
001222160222     C+ CETARM +
001223160222     C+ CETARU +
001224160222     C+ CETARV +
001225160222     C+ CETTRL +
001226160222     C+ CETPAG +
001228160222     C+ CETSEC +
001229160222     C+ CETRCV +
001230160222     C+ CETMDE +
001231160223     C+ CETTDE +
001232160223     C+ CETMAF +
001233160223     C+ CETTAF +
001234160223     C+ CETSES )
001235160222     C+ , cetsea
009100160222     C+ FROM titas10f join eccet00f on
009101160222     C+ tasaas = cetaas and
009102160222     C+ taslnp = cetlnp and
009103160222     C+ tasnrs = cetnrs and
009104160222     C+ tasnsp = cetnsp and
009105160222     C+ tastbl = cettbl
009106160222     C+ WHERE tasaas = 2016 and tasmgs between 0101
009200160222     C+ and 0131 and tastbl = 'F1' and (taslna between 190 and 199 or
009201160223     C+ taslna > 290) and tascca = ' '
009301050408     C/END-EXEC
009302160222
009303160222     c* aggiorna le competenze se presenti altre nel dettaglio
009304160222     C/EXEC SQL
009305160223     C+ DECLARE C1 CURSOR FOR
009307160223     C+ SELECT WFEXPORTF.WAAS, WFEXPORTF.WLNP, WFEXPORTF.WNRS,
009308160223     C+ WFEXPORTF.WNSP, WFEXPORTF.WTBL , sum(cedimp) FROM wfexportf join
009309160223     C+ ecced00f on waas = cedaas and wlnp = cedlnp and wnrs = cednrs and
009310160223     C+ wnsp = cednsp and wtbl = cedtbl GROUP BY WFEXPORTF.WAAS,
009311160223     C+ WFEXPORTF.WLNP, WFEXPORTF.WNRS, WFEXPORTF.WNSP, WFEXPORTF.WTBL
009319160222     C/END-EXEC
009320050408     c*
009321050408     C/EXEC SQL
009322050408     C+ OPEN C1
009323050408     C/END-EXEC
009324050408
009325050408     C                   DO        *HIVAL
009326050408
009327050408     C/EXEC SQL
009328160223     C+ FETCH NEXT FROM C1 INTO :waas, :wlnp, :wnrs, :wnsp, :wtbl, :compd
009329050408     C/END-EXEC
009330050408
009331160223     C                   IF        SQLCod = 100 or sqlcod < 0
009332050408     C                   LEAVE
009333050408     C                   ENDIF
009334050408
009335160223     c     key           chain     wfexportl
009336160223     c                   if        %found(wfexportl)
009337160223     c                   add       compd         wcompe
009338160223     c                   update    wfexport0
009339160222     c                   end
009340050408
009341050408     C                   ENDDO
009342050408
009343050408     C/EXEC SQL
009344050408     C+ CLOSE C1
009345050408     C/END-EXEC
009346160223     f* file di totale
009347160223     C/EXEC SQL
009348160223     C+ INSERT INTO GAITRAAZM/WFEXPTOTF (WLNA, WDES, WRICAVO, WCOMPE,
009349160223     C+ WRICARICO) SELECT wlna, orgdes, sum(wricavo), sum(wcompe),
009350160223     C+ sum(wricarico) FROM wfexportf join azorg00f on wlna=orgfil GROUP
009351160223     C+ BY wlna, orgdes order by wlna
009352160223     C/END-EXEC
009383050408     c                   seton                                        lr
