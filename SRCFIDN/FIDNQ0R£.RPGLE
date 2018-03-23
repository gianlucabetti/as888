000100000606     H DECEDIT('0,') DATEDIT(*DMY.)
000200090327     H* FIDNQ0R *----------------------------------------------------*
000300090327     H*  elaborazioni giorniere ufficio qualità su c.a.
000400000000     H*--------------------------------------------------------------*
000500090401     FTABEL00F  IF   E           K DISK
000600090401     fAzcln01l  if   e           k Disk
000700090327     Ffidnq0d   CF   E             WORKSTN
000800941223     D*
000900110125     D CHKO            s             40    dim(1) ctdata  perrcd(1)
001000110125     D*
001100920925     D WLBDAT          DS
001200941223     D  G02DAT                 1      8  0
001300941223     D  G02INV                 9     16  0
001400941223     D  G02ERR                17     17
001500941223     D  G02TGI                18     22  0
001600090401     d
001700090401     d Ds1y          e ds                  inz
001800090327     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900900517     D KPJBA         E DS
002000090327     D fidn43ds      E DS
002100110125     D TNSSL0DS        DS                  INZ
002200110125     D  PARDTI                 1      8  0
002300110125     D  PARDTF                 9     16  0
002400110125     D  PAROPZ                17     17
002500110125     D  PARtipo               18     18
002600110125
002700090327     d Dataiso         s               d   datfmt(*iso)
002800110125     d
002900090401     d ktfp            s                   like(clntfp) inz
003000090401     d ktfa            s                   like(clntfa) inz
003100090401     d kann            s                   like(clnann) inz
003200090401     d kmes            s                   like(clnmes) inz
003300090401     d Wdata           s               d   Datfmt(*iso) inz
003400090401     d Wgiorno         s              2  0 inz
003500090401     d
003600090401     d COD             s                   like(TBLCOD) inz
003700090401     d KEY             s                   like(TBLKEY) inz
003800090401     d
003900090401     d ClnPom          ds
004000090401     d  Pom                    1     31    Dim(31)
004100090401
004200900511     C*---------------------------------------------------------------*
004300090327     c
004400900509     C     FOR01         TAG
004500090327     C                   EXFMT     dnq0d01
004600941223     C*
004700090327     C** f3 - FINE LAVORO
004800900509     C   KC              GOTO      FINE
004900900524     C* CONTROLLO
005000900524     C                   EXSR      CONTR
005100941223     C  NKF
005200900524     COR 90              GOTO      FOR01
005300090327     C** F6 - ELABORAZIONE
005400090327     c                   clear                   fidn43ds
005500110125     c
005600110125     c* Statistica Spedizioni
005700110125     c                   if        v1cspe='S'
005800110125
005900110125     c                   eval      pardtf   =comela
006000110125     c                   eval      pardti   =comela
006100110125     c                   move      01            pardti
006200120912     c                   eval      paropz ='4'
006300110125     c                   eval      partipo='R'
006400110125     c
006500110125     c                   movel     tnssl0ds      kpjbu
006600110125     c                   eval      kcoaz='SSLR'
006700110125     c                   call(e)   'BCH10'
006800110125     c                   parm                    kpjba
006900110125     c
007000110125     c                   endif
007100110328
007200110328     c                   eval      comman=chko(1)
007300110328     c                   call(e)   'QCMDEXC'
007400110328     c                   parm                    comman           80
007500110328     c                   parm      40            lung             15 5
007600110328     c                   if        not %error
007700110328     c                   eval      kqieb='K018SDINO'
007800110328     c                   endif
007900110125     c
008000090327     c* Giornaliero
008100090327     c                   if        v1cgio='S'
008200090327     c
008300090327     c                   eval      i43crp='S'
008400090327     c                   eval      i43ord='2'
008500090327     c                   eval      i43dcd=v1cela
008600090327     c                   eval      i43amgdcd=comela
008700121219     c* stampa congelata
008800121219     c                   eval      i43psn ='H'
008900090327     c                   movel     fidn43ds      kpjbu
009000090327     c                   eval      kcoaz='DN47'
009100090327     c                   call      'BCH10'
009200090327     c                   parm                    kpjba
009300090327     c                   endif
009400090327     c
009500090327     c* Riepilogativo
009600090327     c                   if        v1criep='S'
009700090327     c                   clear                   fidn43ds
009800090327     c
009900090327     c                   eval      i43crp='S'
010000090327     c                   eval      i43ord='2'
010100090327     c* data elaborazione AL --> data a video
010200090327     c                   eval      i43dca=v1cela
010300090327     c                   eval      i43amgdca=comela
010400090327     c* data elaborazione DAL --> inizio mese
010500090327     c                   eval      i43dcd=i43dca
010600090327     c                   movel     01            i43dcd
010700090327     c                   eval      i43amgdcd=i43amgdca
010800090327     c                   move      01            i43amgdcd
010900121219     c* stampa congelata
011000121219     c                   eval      i43psn ='H'
011100090327     c
011200090327     c                   movel     fidn43ds      kpjbu
011300090327     c                   eval      kcoaz='DN47'
011400090327     c                   call      'BCH10'
011500090327     c                   parm                    kpjba
011600090403     c
011700090403     c* stampa anche dei totali AM e V  riepilogativi
011800090403     c                   if        v1ctotr='S'
011900090403     c
012000090403     c                   eval      i43crp='N'
012100090403     c                   eval      i43rgr='A'
012200090403     c                   eval      i43rg2='M'
012300090403     c                   eval      i43usi='S'
012400090403     c                   eval      i43toi='S'
012500121219     c* stampa congelata
012600121219     c                   eval      i43psn ='H'
012700090403     c                   movel     fidn43ds      kpjbu
012800090403     c                   eval      kcoaz='DN47'
012900090403     c                   call      'BCH10'
013000090403     c                   parm                    kpjba
013100090403     c
013200090403     c                   eval      i43rgr='V'
013300090403     c                   eval      i43rg2=' '
013400121219     c* stampa congelata
013500121219     c                   eval      i43psn ='H'
013600090403     c                   movel     fidn43ds      kpjbu
013700090403     c                   eval      kcoaz='DN47'
013800090403     c                   call      'BCH10'
013900090403     c                   parm                    kpjba
014000090403     c
014100090327     c                   endif
014200090403     c                   endif
014300090327     c
014400090327     c* Ammanchi / Mancanze
014500090327     c                   if        v1csta ='S'
014600090327     c                   clear                   fidn43ds
014700090327     c
014800090327     c                   eval      i43crp='N'
014900090327     c                   eval      i43usi='S'
015000090327     c                   eval      i43ord='2'
015100090327     c                   eval      i43dcd=v1cela
015200090327     c                   eval      i43amgdcd=comela
015300090327     c                   eval      i43rgr='A'
015400090327     c                   eval      i43rg2='M'
015500121219     c* stampa congelata
015600121219     c                   eval      i43psn ='H'
015700090327     c
015800090327     c                   movel     fidn43ds      kpjbu
015900090327     c                   eval      kcoaz='DN47'
016000090327     c                   call      'BCH10'
016100090327     c                   parm                    kpjba
016200090327     c                   endif
016300120221     c* Avarie   solo totali
016400090327     c                   if        v1cstv ='S'
016500090327     c                   clear                   fidn43ds
016600090327     c
016700090327     c                   eval      i43crp='N'
016800090327     c                   eval      i43usi='S'
016900120221     c                   eval      i43toi='S'
017000090327     c                   eval      i43ord='2'
017100090327     c                   eval      i43dcd=v1cela
017200090327     c                   eval      i43amgdcd=comela
017300090327     c                   eval      i43rgr='V'
017400121219     c* stampa congelata
017500121219     c                   eval      i43psn ='H'
017600090327     c
017700090327     c                   movel     fidn43ds      kpjbu
017800090327     c                   eval      kcoaz='DN47'
017900090327     c                   call      'BCH10'
018000090327     c                   parm                    kpjba
018100090327     c                   endif
018200090327     c
018300920925     C     FINE          TAG
018400000000     C                   SETON                                        LR
018500900524     C*
018600900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
018700900524     C     CONTR         BEGSR
018800900524     C                   SETOFF                                       90
018900090327     C* DATA elaborazione
019000090327     C                   MOVE      V1cela        G02DAT
019100920925     C                   MOVEL     *BLANK        G02ERR
019200941223     C                   CALL      'XSRDA8'
019300920925     C                   PARM                    WLBDAT
019400920925     C     G02ERR        IFEQ      '1'
019500920925     C                   SETON                                        40  90
019600920925     C                   GOTO      ENDCTR
019700920925     C                   END
019800090327     C                   Z-ADD     G02INV        comela            8 0
019900090327     C                   Z-ADD     G02DAT        V1cela
020000941223     C*
020100900524     C     ENDCTR        ENDSR
020200920925     C*
020300090327     C*--- operazioni iniziali ---------------------------------------*
020400090327     C     *INZSR        BEGSR
020500090327     c
020600090327     C     *ENTRY        PLIST
020700090327     C                   PARM                    KPJBA
020800090401     c
020900090401     c                   z-add     1             codut
021000090327     C                   CALL      'X§PARUT'
021100090327     C                   PARM                    UT§DSE
021200090327     C                   MOVEL     RAGUT         RSUT             20
021300090327     c
021400090327     c                   time                    w0140            14 0
021500090327     c                   move      w0140         v1cela
021600090401     c     *eur          movel     v1cela        wdata
021700090401     c                   Movel(p)  '1Y'          Cod
021800090401     c
021900090401     c     Festav        Tag
022000090401     c                   Subdur    1:*D          Wdata
022100090401     c                   Extrct    Wdata:*y      Kann
022200090401     c                   Extrct    Wdata:*m      Kmes
022300090401     c                   Extrct    Wdata:*d      Wgiorno
022400090401     c     Kazcln        Chain     Azcln01l
022500090401If  2c                   If        %Found(Azcln01l)
022600090401If  3c                   If        Pom(Wgiorno) <> *blanks
022700090401     c                   Movel     Pom(Wgiorno)  Key
022800090401     c     Ktab          Chain     Tabel00f
022900090401     c                   clear                   ds1y
023000090401If  4c                   If        %Found(Tabel00f)
023100090401     c                   Movel     Tbluni        Ds1y
023200090401E   4c                   EndIf
023300090401If  4c                   If        §1yfei = 'S'
023400090401     c                   Goto      Festav
023500090401E   4c                   EndIf
023600090401E   3c                   EndIf
023700090401E   2c                   EndIf
023800090401
023900090401     c     *eur          Move      Wdata         v1cela
024000090327     c                   eval      v1cgio='S'
024100090327     c                   eval      v1criep='S'
024200090403     c                   eval      v1ctotr='S'
024300090327     c                   eval      v1csta ='S'
024400090327     c                   eval      v1cstv ='S'
024500110125     c                   eval      v1cspe ='S'
024600090401     c
024700090401     c     Kazcln        Klist
024800090401     c                   Kfld                    Ktfp
024900090401     c                   Kfld                    Ktfa
025000090401     c                   Kfld                    Kann
025100090401     c                   Kfld                    Kmes
025200090401     C     KTAB          KLIST
025300090401     C                   KFLD                    CODUT
025400090401     C                   KFLD                    COD
025500090401     C                   KFLD                    KEY
025600090401     C*
025700920925     C                   ENDSR
025800110125**
025900110328CHKOBJ OBJ(K018SDINO) OBJTYPE(*JOBQ)
