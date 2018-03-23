000100921203     H DECEDIT('0,') DATEDIT(*DMY.)
000200051024     Faffidabp  iF   E             DISK    prefix(£)
000300051024     f                                     rename(affidabp:affidab1)
000400051024     Faffidabd  iF   E             DISK    prefix(£)
000500051024     f                                     rename(affidabd:affidab2)
000600051024     d tipo            s              1
000700051025     d savnci          s                   like(d98nci)
000800051025     d savnpi          s                   like(d98npi)
000900051025     d savdce          s                   like(d98dce)
001000051025     d savnrc          s                   like(d98nrc)
001100051025     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
001200051024     c* nei file affidab* ci sono sole le spedizioni con variazioni CD
001300051024     c* sono stati generati con questi criteri: ultima variazione fatta =
001400051024     c* CD, data consegna richiesta invariata e flag tipo consegna variato
001500051024     c* da P/D a blanks
001600051024     c*
001700051024     c* SELECT a.ARBAAS, a.ARBLNP, A.ARBNRS, A.ARBNSP, c.ARBdcm, c.arbdcr
001800051024     c* FROM edpba/affidab a join filtra201/fnarbg0f b on a.arbaas =
001900051024     c* b.arbaas and a.arblnp = b.arblnp and a.arbnrs = b.arbnrs and
002000051024     c* a.arbnsp = b.arbnsp and substr(digits(sel0005), 7, 8)=
002100051024     c* digits(b.arbdtv) and substr(digits(sel0005), 15, 6)=
002200051024     c* digits(b.arborv) join filtra201/fnarb01l c on a.arbaas = c.arbaas
002300051024     c* and a.arblnp = c.arblnp and a.arbnrs = c.arbnrs and a.arbnsp =
002400051024     c* c.arbnsp WHERE b.arbtcr='D' and b.arbcvb = 'CD' and c.arbtcr = ' '
002500051024     c* and b.arbdcr=c.arbdcr ORDER BY A.ARBAAS, A.ARBLNP, A.ARBNRS,
002600051024     c* A.ARBNSP
002700051024     c*
002800051024     c                   eval      tipo= 'P'
002900051024     c                   do        *hival
003000051024     c                   read      affidabp
003100051024     c                   if        %eof(affidabp)
003200051024     c                   leave
003300051024     c                   end
003400051024     c                   if        £arbdcm <> 0
003500051025     c* affidabilità
003600051025     c                   exsr      sraffida
003700051025     c* se data consegna reale = data consegna richiesta non aggiorno
003800051025     c* perchè toglierei affidabilità ma in realtà il p.o. ha agito bene
003900051025     c                   if        d98dcr = £arbdcm
004000051025     c                   iter
004100051025     c                   end
004200051025     c* se torno con l'errore non aggiorno
004300051025     c                   if        d98err <> '0'
004400051025     c                   iter
004500051025     c                   end
004600051025     c* se torno che i tempi non sono cambiati non aggiorno
004700051025     c                   if        savnci = d98nci and
004800051025     c                             savnpi = d98npi and
004900051025     c                             savdce = d98dce and
005000051025     c                             savnrc = d98nrc
005100051025     c                   iter
005200051025     c                   end
005300051025     c* arb
005400051025     c                   exsr      srarb
005500051024     c* blp
005600051024     c                   exsr      srblp
005700051024     c* tas
005800051024     c                   exsr      srtas
005900051024     c* ar5 sede
006000051024     c                   exsr      srar5s
006100051024     c* ar5 filiale
006200051024     c                   exsr      srar5f
006300051024     c                   end
006400051024     c                   enddo
006500051024     c*
006600051024     c                   eval      tipo= 'D'
006700051024     c                   do        *hival
006800051024     c                   read      affidabd
006900051024     c                   if        %eof(affidabd)
007000051024     c                   leave
007100051024     c                   end
007200051024     c                   if        £arbdcm <>0
007300051025     c* affidabilità
007400051025     c                   exsr      sraffida
007500051025     c* se data consegna reale = data consegna richiesta non aggiorno
007600051025     c* perchè toglierei affidabilità ma in realtà il p.o. ha agito bene
007700051025     c                   if        d98dcr = £arbdcm
007800051025     c                   iter
007900051025     c                   end
008000051025     c* se torno con l'errore non aggiorno
008100051025     c                   if        d98err <> '0'
008200051025     c                   iter
008300051025     c                   end
008400051025     c* se torno che i tempi non sono cambiati non aggiorno
008500051025     c                   if        savnci = d98nci and
008600051025     c                             savnpi = d98npi and
008700051025     c                             savdce = d98dce and
008800051025     c                             savnrc = d98nrc
008900051025     c                   iter
009000051025     c                   end
009100051024     c* arb
009200051024     c                   exsr      srarb
009300051024     c* blp
009400051024     c                   exsr      srblp
009500051024     c* tas
009600051024     c                   exsr      srtas
009700051024     c* ar5 sede
009800051024     c                   exsr      srar5s
009900051024     c* ar5 filiale
010000051024     c                   exsr      srar5f
010100051024     c                   end
010200051024     c                   enddo
010300051024     c*
010400040115
010500000000     C                   SETON                                        LR
010600040115
010700040115     C**---------------------------------------------------------
010800051025     c     sraffida      begsr
010900040115     C**---------------------------------------------------------
011000051024     C* chiamo pgm x delivery prima con sped. poi senza per fare ricalcolare
011100051024     c                   clear                   tnsd99
011200051024     C                   MOVEL     ' '           D98TLA
011300051024     C                   MOVEL     'A'           D98TBO
011400051024     C                   MOVE      £ARBAAS       D98AAS
011500051024     C                   MOVE      £ARBLNP       D98LNP
011600051024     C                   MOVE      £ARBNRS       D98NRS
011700051024     C                   MOVE      £ARBNSP       D98NSP
011800051024     C                   CALL      'TNSD99R'
011900051024     C                   PARM                    TNSD99
012000051025     c                   move      D98NCI        savnci
012100051025     c                   move      D98NPI        savnpi
012200051025     c                   move      D98DCE        savdce
012300051025     c                   move      D98NRC        savnrc
012400051025     c                   if        d98dcr<> £arbdcm
012500051025     c                   clear                   tnsd99
012600051025     C                   MOVEL     ' '           D98TLA
012700051025     C                   MOVEL     'A'           D98TBO
012800051025     C                   MOVE      £ARBAAS       D98AAS
012900051025     C                   MOVE      £ARBLNP       D98LNP
013000051025     C                   MOVE      £ARBNRS       D98NRS
013100051025     C                   MOVE      £ARBNSP       D98NSP
013200051025     c                   if        tipo = 'P'
013300051025     C                   CALL      'TNSD99R_P'
013400051025     C                   PARM                    TNSD99
013500051025     c                   else
013600051025     C                   CALL      'TNSD99R_D'
013700051025     C                   PARM                    TNSD99
013800051025     c                   end
013900051025     c                   end
014000051025     c                   endsr
014100051025     C**---------------------------------------------------------
014200051025     c     srarb         begsr
014300051025     C**---------------------------------------------------------
014400050412     c*
014500051024     C/EXEC SQL
014600051024     C+ UPDATE FILTRA201/FNARB00F SET ARBTCR = :tipo, ARBDCE = :d98dce,
014700051024     C+ ARBNPI = :d98npi, ARBNCI = :d98nci, ARBNRC = :d98nrc WHERE arbaas
014800051024     C+ = :£arbaas and arblnp=:£arblnp and arbnrs = :£arbnrs and arbnsp =
014900051024     C+ :£arbnsp and arbtcr = ' '
015000051024     C/END-EXEC
015100051024     c*
015200040115     c                   endsr
015300051024     C**---------------------------------------------------------
015400051024     c     srblp         begsr
015500051024     C**---------------------------------------------------------
015600051024     c*
015700051024     C/EXEC SQL
015800051024     C+ UPDATE FILTRA201/FNBLP00F SET BLPTCR = :tipo, BLPDCE = :d98dce,
015900051024     C+ BLPNPI = :d98npi, BLPNCI = :d98nci, BLPNRC = :d98nrc WHERE blpaas
016000051024     C+ = :£arbaas and blplnp=:£arblnp and blpnrs = :£arbnrs and blpnsp =
016100051024     C+ :£arbnsp and blptcr = ' '
016200051024     C/END-EXEC
016300051024     c*
016400051024     c                   endsr
016500051024     C**---------------------------------------------------------
016600051024     c     srtas         begsr
016700051024     C**---------------------------------------------------------
016800051024     c*
016900051024     C/EXEC SQL
017000051024     C+ UPDATE GAITRAGRU/TITAS00F SET TASTCR = :tipo, TASDCE = :d98dce,
017100051024     C+ TASHCE = :d98hce, TASNPI = :d98npi, TASNCI = :d98nci, TASNRC =
017200051024     C+ :d98nrc WHERE tasaas = :£arbaas and taslnp=:£arblnp and tasnrs =
017300051024     C+ :£arbnrs and tasnsp = :£arbnsp and tastcr = ' '
017400051024     C/END-EXEC
017500051024     c*
017600051024     c                   endsr
017700051024     C**---------------------------------------------------------
017800051024     c     srar5s        begsr
017900051024     C**---------------------------------------------------------
018000051024     c*
018100051024     C/EXEC SQL
018200051024     C+ UPDATE GAITRAGRU/FIAR500F SET AR5UNI = substr(ar5uni, 1, 93)
018300051024     C+ concat :tipo concat substr(ar5uni, 95) WHERE ar5aas =:£arbaas and
018400051024     C+ ar5lnp = :£arblnp and ar5nrs = :£arbnrs and ar5nsp = :£arbnsp and
018500051024     C+ ar5trd = 'GEN' and substr(ar5uni, 94, 1) = ' '
018600051024     C/END-EXEC
018700051024     c*
018800051024     c                   endsr
018900051024     C**---------------------------------------------------------
019000051024     c     srar5f        begsr
019100051024     C**---------------------------------------------------------
019200051024     c*
019300051024     C/EXEC SQL
019400051024     C+ UPDATE FILTRA201/FIAR500F SET AR5UNI = substr(ar5uni, 1,
019500051024     C+ 93)concat :tipo concat
019600051024     C+           substr(ar5uni, 95) WHERE ar5aas =:£arbaas and ar5lnp =
019700051024     C+ :£arblnp and ar5nrs = :£arbnrs and ar5nsp = :£arbnsp and ar5trd =
019800051024     C+ 'GEN' and substr(ar5uni, 94, 1) = ' '
019900051024     C/END-EXEC
020000051024     c*
020100051024     c                   endsr
