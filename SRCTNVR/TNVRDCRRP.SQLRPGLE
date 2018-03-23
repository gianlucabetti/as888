000100921203     H DECEDIT('0,') DATEDIT(*DMY.)
000200051024     Faffidabp  iF   E             DISK    prefix(£)
000300051024     f                                     rename(affidabp:affidab1)
000400051024     Faffidabd  iF   E             DISK    prefix(£)
000500051024     f                                     rename(affidabd:affidab2)
000600051024     fqsysprt   o    f  132        printer
000700051024     d tipo            s              1
000800051025     d savnci          s                   like(d98nci)
000900051025     d savnpi          s                   like(d98npi)
001000051025     d savdce          s                   like(d98dce)
001100051025     d savnrc          s                   like(d98nrc)
001200051024     D TNSD99        E DS                  EXTNAME(TNSD99DS) INZ
001300051024     c* nei file affidab* ci sono sole le spedizioni con variazioni CD
001400051024     c* sono stati generati con questi criteri: ultima variazione fatta =
001500051024     c* CD, data consegna richiesta invariata e flag tipo consegna variato
001600051024     c* da P/D a blanks
001700051024     c*
001800051024     c* SELECT a.ARBAAS, a.ARBLNP, A.ARBNRS, A.ARBNSP, c.ARBdcm, c.arbdcr
001900051024     c* FROM edpba/affidab a join filtra201/fnarbg0f b on a.arbaas =
002000051024     c* b.arbaas and a.arblnp = b.arblnp and a.arbnrs = b.arbnrs and
002100051024     c* a.arbnsp = b.arbnsp and substr(digits(sel0005), 7, 8)=
002200051024     c* digits(b.arbdtv) and substr(digits(sel0005), 15, 6)=
002300051024     c* digits(b.arborv) join filtra201/fnarb01l c on a.arbaas = c.arbaas
002400051024     c* and a.arblnp = c.arblnp and a.arbnrs = c.arbnrs and a.arbnsp =
002500051024     c* c.arbnsp WHERE b.arbtcr='D' and b.arbcvb = 'CD' and c.arbtcr = ' '
002600051024     c* and b.arbdcr=c.arbdcr ORDER BY A.ARBAAS, A.ARBLNP, A.ARBNRS,
002700051024     c* A.ARBNSP
002800051024     c*
002900051024     c                   eval      tipo= 'P'
003000051024     c                   do        *hival
003100051024     c                   read      affidabp
003200051024     c                   if        %eof(affidabp)
003300051024     c                   leave
003400051024     c                   end
003500051024     c                   if        £arbdcm <> 0
003600051024     c                   exsr      srarb
003700051024     c                   end
003800051024     c                   enddo
003900051024     c*
004000051024     c                   eval      tipo= 'D'
004100051024     c                   do        *hival
004200051024     c                   read      affidabd
004300051024     c                   if        %eof(affidabd)
004400051024     c                   leave
004500051024     c                   end
004600051024     c                   if        £arbdcm <>0
004700051024     c                   exsr      srarb
004800051024     c                   end
004900051024     c                   enddo
005000051024     c*
005100040115
005200000000     C                   SETON                                        LR
005300040115
005400040115     C**---------------------------------------------------------
005500051024     c     srarb         begsr
005600040115     C**---------------------------------------------------------
005700051024     C* chiamo pgm x delivery prima con sped. poi senza per fare ricalcolare
005800051024     c                   clear                   tnsd99
005900051024     C                   MOVEL     ' '           D98TLA
006000051024     C                   MOVEL     'A'           D98TBO
006100051024     C                   MOVE      £ARBAAS       D98AAS
006200051024     C                   MOVE      £ARBLNP       D98LNP
006300051024     C                   MOVE      £ARBNRS       D98NRS
006400051024     C                   MOVE      £ARBNSP       D98NSP
006500051025     C                   CALL      'TNSD99R'
006600051025     C                   PARM                    TNSD99
006700051025     c                   move      D98NCI        savnci
006800051025     c                   move      D98NPI        savnpi
006900051025     c                   move      D98DCE        savdce
007000051025     c                   move      D98NRC        savnrc
007100051025     c                   if        d98dcr<> £arbdcm
007200051025     c*
007300051024     c                   clear                   tnsd99
007400051024     C                   MOVEL     ' '           D98TLA
007500051024     C                   MOVEL     'A'           D98TBO
007600051024     C                   MOVE      £ARBAAS       D98AAS
007700051024     C                   MOVE      £ARBLNP       D98LNP
007800051024     C                   MOVE      £ARBNRS       D98NRS
007900051024     C                   MOVE      £ARBNSP       D98NSP
008000051025     c                   if        tipo = 'P'
008100051025     C                   CALL      'TNSD99R_P'
008200051025     C                   PARM                    TNSD99
008300051025     c                   else
008400051025     C                   CALL      'TNSD99R_D'
008500051025     C                   PARM                    TNSD99
008600051025     c                   end
008700051025     c                   setoff                                       0102
008800051025     c                   if        d98err <> '0'
008900051025     c                   seton                                        02
009000051025     c                   end
009100051025     c                   if        (tipo = 'P' and
009200051025     c                             (savnci < d98nci or
009300051025     c                             savnpi < d98npi or
009400051025     c                             savnrc < d98nrc )) or
009500051025     c                             (tipo = 'D' and
009600051025     c                             (savnci > d98nci or
009700051025     c                             savnpi  >d98npi or
009800051025     c                             savnrc > d98nrc ))
009900051025     c                   seton                                        01
010000051025     c                   end
010100051025     c                   if        savnci <> d98nci or
010200051025     c                             savnpi <> d98npi or
010300051025     c                             savdce <> d98dce or
010400051025     c                             savnrc <> d98nrc
010500051025     c                   except    riga
010600051024     c                   except    rigad
010700051025     c                   end
010800051025     c                   end
010900051024     c                   endsr
011000051024     OQsysPrt   E            riga        1
011100051024     O                                           +0 'Sped.'
011200051024     O                       £arbaas             +1
011300051024     O                       £arblnp             +1
011400051024     O                       £arbnrs       z     +1
011500051024     O                       £arbnsp       z     +1
011600051024     O                                           +1 'NCI'
011700051025     O                       savNCI        k     +1
011800051024     O                                           +1 'NPI'
011900051025     O                       savNPI        k     +1
012000051024     O                                           +1 'DCE'
012100051025     O                       savDCE              +1
012200051024     O                                           +1 'NRC'
012300051025     O                       savNRC        k     +1
012400051024     O          E            rigad       1
012500051024     o                       tipo                +0
012600051024     O                                           29 'NCI'
012700051024     O                       D98NCI        k     +1
012800051024     O                                           +1 'NPI'
012900051024     O                       D98NPI        k     +1
013000051024     O                                           +1 'DCE'
013100051024     O                       D98DCE              +1
013200051024     O                                           +1 'NRC'
013300051024     O                       D98NRC        k     +1
013400051025     O               01                          +1 '+/-'
013500051025     O          E    02      rigad       1
013600051025     O                       D98msg              +0
