000100921203     H DECEDIT('0,') DATEDIT(*DMY.)
000200051024     Faffidabp  iF   E             DISK    prefix(£)
000300051024     f                                     rename(affidabp:affidab1)
000400051024     Faffidabd  iF   E             DISK    prefix(£)
000500051024     f                                     rename(affidabd:affidab2)
000600051024     d tipo            s              1
000700051024     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
000800051024     c* nei file affidab* ci sono sole le spedizioni con variazioni CD
000900051024     c* sono stati generati con questi criteri: ultima variazione fatta =
001000051024     c* CD, data consegna richiesta invariata e flag tipo consegna variato
001100051024     c* da P/D a blanks
001200051024     c*
001300051024     c* SELECT a.ARBAAS, a.ARBLNP, A.ARBNRS, A.ARBNSP, c.ARBdcm, c.arbdcr
001400051024     c* FROM edpba/affidab a join filtra201/fnarbg0f b on a.arbaas =
001500051024     c* b.arbaas and a.arblnp = b.arblnp and a.arbnrs = b.arbnrs and
001600051024     c* a.arbnsp = b.arbnsp and substr(digits(sel0005), 7, 8)=
001700051024     c* digits(b.arbdtv) and substr(digits(sel0005), 15, 6)=
001800051024     c* digits(b.arborv) join filtra201/fnarb01l c on a.arbaas = c.arbaas
001900051024     c* and a.arblnp = c.arblnp and a.arbnrs = c.arbnrs and a.arbnsp =
002000051024     c* c.arbnsp WHERE b.arbtcr='D' and b.arbcvb = 'CD' and c.arbtcr = ' '
002100051024     c* and b.arbdcr=c.arbdcr ORDER BY A.ARBAAS, A.ARBLNP, A.ARBNRS,
002200051024     c* A.ARBNSP
002300051024     c*
002400051024     c                   eval      tipo= 'P'
002500051024     c                   do        *hival
002600051024     c                   read      affidabp
002700051024     c                   if        %eof(affidabp)
002800051024     c                   leave
002900051024     c                   end
003000051024     c                   if        £arbdcm <> 0
003100051024     c* arb
003200051024     c                   exsr      srarb
003300051024     c* blp
003400051024     c                   exsr      srblp
003500051024     c* tas
003600051024     c                   exsr      srtas
003700051024     c* ar5 sede
003800051024     c                   exsr      srar5s
003900051024     c* ar5 filiale
004000051024     c                   exsr      srar5f
004100051024     c                   end
004200051024     c                   enddo
004300051024     c*
004400051024     c                   eval      tipo= 'D'
004500051024     c                   do        *hival
004600051024     c                   read      affidabd
004700051024     c                   if        %eof(affidabd)
004800051024     c                   leave
004900051024     c                   end
005000051024     c                   if        £arbdcm <>0
005100051024     c* arb
005200051024     c                   exsr      srarb
005300051024     c* blp
005400051024     c                   exsr      srblp
005500051024     c* tas
005600051024     c                   exsr      srtas
005700051024     c* ar5 sede
005800051024     c                   exsr      srar5s
005900051024     c* ar5 filiale
006000051024     c                   exsr      srar5f
006100051024     c                   end
006200051024     c                   enddo
006300051024     c*
006400040115
006500000000     C                   SETON                                        LR
006600040115
006700040115     C**---------------------------------------------------------
006800051024     c     srarb         begsr
006900040115     C**---------------------------------------------------------
007000051024     C* chiamo pgm x delivery prima con sped. poi senza per fare ricalcolare
007100051024     c                   clear                   tnsd99
007200051024     C                   MOVEL     ' '           D98TLA
007300051024     C                   MOVEL     'A'           D98TBO
007400051024     C                   MOVE      £ARBAAS       D98AAS
007500051024     C                   MOVE      £ARBLNP       D98LNP
007600051024     C                   MOVE      £ARBNRS       D98NRS
007700051024     C                   MOVE      £ARBNSP       D98NSP
007800051024     C                   CALL      'TNSD99R'
007900051024     C                   PARM                    TNSD99
008000051024     C                   clear                   D98NRS
008100051024     C                   clear                   D98NSP
008200051024     c                   eval      d98tcr = tipo
008300051024     C                   CALL      'TNSD99R'
008400051024     C                   PARM                    TNSD99
008500051024     C                   Z-ADD     D98NCI        ARBNCI                         NON CONF.CONS. INT.
008600051024     C                   Z-ADD     D98NPI        ARBNPI                         NON CONF.PART. INT.
008700051024     C                   Z-ADD     D98DCE        ARBDCE                         DATA CONS. PREV.CLI.
008800051024     C                   Z-ADD     D98NRC        ARBNRC                         NON CONF. RESA CLI.
008900050412     c*
009000051024     C/EXEC SQL
009100051024     C+ UPDATE FILTRA201/FNARB00F SET ARBTCR = :tipo, ARBDCE = :d98dce,
009200051024     C+ ARBNPI = :d98npi, ARBNCI = :d98nci, ARBNRC = :d98nrc WHERE arbaas
009300051024     C+ = :£arbaas and arblnp=:£arblnp and arbnrs = :£arbnrs and arbnsp =
009400051024     C+ :£arbnsp and arbtcr = ' '
009500051024     C/END-EXEC
009600051024     c*
009700040115     c                   endsr
009800051024     C**---------------------------------------------------------
009900051024     c     srblp         begsr
010000051024     C**---------------------------------------------------------
010100051024     c*
010200051024     C/EXEC SQL
010300051024     C+ UPDATE FILTRA201/FNBLP00F SET BLPTCR = :tipo, BLPDCE = :d98dce,
010400051024     C+ BLPNPI = :d98npi, BLPNCI = :d98nci, BLPNRC = :d98nrc WHERE blpaas
010500051024     C+ = :£arbaas and blplnp=:£arblnp and blpnrs = :£arbnrs and blpnsp =
010600051024     C+ :£arbnsp and blptcr = ' '
010700051024     C/END-EXEC
010800051024     c*
010900051024     c                   endsr
011000051024     C**---------------------------------------------------------
011100051024     c     srtas         begsr
011200051024     C**---------------------------------------------------------
011300051024     c*
011400051024     C/EXEC SQL
011500051024     C+ UPDATE GAITRAGRU/TITAS00F SET TASTCR = :tipo, TASDCE = :d98dce,
011600051024     C+ TASHCE = :d98hce, TASNPI = :d98npi, TASNCI = :d98nci, TASNRC =
011700051024     C+ :d98nrc WHERE tasaas = :£arbaas and taslnp=:£arblnp and tasnrs =
011800051024     C+ :£arbnrs and tasnsp = :£arbnsp and tastcr = ' '
011900051024     C/END-EXEC
012000051024     c*
012100051024     c                   endsr
012200051024     C**---------------------------------------------------------
012300051024     c     srar5s        begsr
012400051024     C**---------------------------------------------------------
012500051024     c*
012600051024     C/EXEC SQL
012700051024     C+ UPDATE GAITRAGRU/FIAR500F SET AR5UNI = substr(ar5uni, 1, 93)
012800051024     C+ concat :tipo concat substr(ar5uni, 95) WHERE ar5aas =:£arbaas and
012900051024     C+ ar5lnp = :£arblnp and ar5nrs = :£arbnrs and ar5nsp = :£arbnsp and
013000051024     C+ ar5trd = 'GEN' and substr(ar5uni, 94, 1) = ' '
013100051024     C/END-EXEC
013200051024     c*
013300051024     c                   endsr
013400051024     C**---------------------------------------------------------
013500051024     c     srar5f        begsr
013600051024     C**---------------------------------------------------------
013700051024     c*
013800051024     C/EXEC SQL
013900051024     C+ UPDATE FILTRA201/FIAR500F SET AR5UNI = substr(ar5uni, 1,
014000051024     C+ 93)concat :tipo concat
014100051024     C+           substr(ar5uni, 95) WHERE ar5aas =:£arbaas and ar5lnp =
014200051024     C+ :£arblnp and ar5nrs = :£arbnrs and ar5nsp = :£arbnsp and ar5trd =
014300051024     C+ 'GEN' and substr(ar5uni, 94, 1) = ' '
014400051024     C/END-EXEC
014500051024     c*
014600051024     c                   endsr
