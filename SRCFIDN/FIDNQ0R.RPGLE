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
005100130129
005200130129     c                   select
005300130129     c                   when      *in90
005400130129     c                   goto      for01
005500130129
005600130129     c* F9 - skedulatore
005700130129     c                   when      *inki=*on
005800130129     c                   exsr      F09GES
005900130129     c                   goto      for01
006000130129     c
006100130129     c                   when      not *inkf
006200130129     C                   GOTO      FOR01
006300130129     c                   endsl
006400130129     c
006500090327     C** F6 - ELABORAZIONE
006600090327     c                   clear                   fidn43ds
006700110125     c
006800110125     c* Statistica Spedizioni
006900110125     c                   if        v1cspe='S'
007000110125
007100110125     c                   eval      pardtf   =comela
007200110125     c                   eval      pardti   =comela
007300110125     c                   move      01            pardti
007400120912     c                   eval      paropz ='4'
007500110125     c                   eval      partipo='R'
007600110125     c
007700110125     c                   movel     tnssl0ds      kpjbu
007800110125     c                   eval      kcoaz='SSLR'
007900110125     c                   call(e)   'BCH10'
008000110125     c                   parm                    kpjba
008100110125     c
008200110125     c                   endif
008300110328
008400110328     c                   eval      comman=chko(1)
008500110328     c                   call(e)   'QCMDEXC'
008600110328     c                   parm                    comman           80
008700110328     c                   parm      40            lung             15 5
008800110328     c                   if        not %error
008900110328     c                   eval      kqieb='K018SDINO'
009000110328     c                   endif
009100110125     c
009200090327     c* Giornaliero
009300090327     c                   if        v1cgio='S'
009400090327     c
009500090327     c                   eval      i43crp='S'
009600090327     c                   eval      i43ord='2'
009700090327     c                   eval      i43dcd=v1cela
009800090327     c                   eval      i43amgdcd=comela
009900121219     c* stampa congelata
010000121219     c                   eval      i43psn ='H'
010100090327     c                   movel     fidn43ds      kpjbu
010200090327     c                   eval      kcoaz='DN47'
010300090327     c                   call      'BCH10'
010400090327     c                   parm                    kpjba
010500090327     c                   endif
010600090327     c
010700090327     c* Riepilogativo
010800090327     c                   if        v1criep='S'
010900090327     c                   clear                   fidn43ds
011000090327     c
011100090327     c                   eval      i43crp='S'
011200090327     c                   eval      i43ord='2'
011300090327     c* data elaborazione AL --> data a video
011400090327     c                   eval      i43dca=v1cela
011500090327     c                   eval      i43amgdca=comela
011600090327     c* data elaborazione DAL --> inizio mese
011700090327     c                   eval      i43dcd=i43dca
011800090327     c                   movel     01            i43dcd
011900090327     c                   eval      i43amgdcd=i43amgdca
012000090327     c                   move      01            i43amgdcd
012100121219     c* stampa congelata
012200121219     c                   eval      i43psn ='H'
012300090327     c
012400090327     c                   movel     fidn43ds      kpjbu
012500090327     c                   eval      kcoaz='DN47'
012600090327     c                   call      'BCH10'
012700090327     c                   parm                    kpjba
012800090403     c
012900090403     c* stampa anche dei totali AM e V  riepilogativi
013000090403     c                   if        v1ctotr='S'
013100090403     c
013200090403     c                   eval      i43crp='N'
013300090403     c                   eval      i43rgr='A'
013400090403     c                   eval      i43rg2='M'
013500090403     c                   eval      i43usi='S'
013600090403     c                   eval      i43toi='S'
013700121219     c* stampa congelata
013800121219     c                   eval      i43psn ='H'
013900090403     c                   movel     fidn43ds      kpjbu
014000090403     c                   eval      kcoaz='DN47'
014100090403     c                   call      'BCH10'
014200090403     c                   parm                    kpjba
014300090403     c
014400090403     c                   eval      i43rgr='V'
014500090403     c                   eval      i43rg2=' '
014600121219     c* stampa congelata
014700121219     c                   eval      i43psn ='H'
014800090403     c                   movel     fidn43ds      kpjbu
014900090403     c                   eval      kcoaz='DN47'
015000090403     c                   call      'BCH10'
015100090403     c                   parm                    kpjba
015200090403     c
015300090327     c                   endif
015400090403     c                   endif
015500090327     c
015600090327     c* Ammanchi / Mancanze
015700090327     c                   if        v1csta ='S'
015800090327     c                   clear                   fidn43ds
015900090327     c
016000090327     c                   eval      i43crp='N'
016100090327     c                   eval      i43usi='S'
016200090327     c                   eval      i43ord='2'
016300090327     c                   eval      i43dcd=v1cela
016400090327     c                   eval      i43amgdcd=comela
016500090327     c                   eval      i43rgr='A'
016600090327     c                   eval      i43rg2='M'
016700121219     c* stampa congelata
016800121219     c                   eval      i43psn ='H'
016900090327     c
017000090327     c                   movel     fidn43ds      kpjbu
017100090327     c                   eval      kcoaz='DN47'
017200090327     c                   call      'BCH10'
017300090327     c                   parm                    kpjba
017400090327     c                   endif
017500120221     c* Avarie   solo totali
017600090327     c                   if        v1cstv ='S'
017700090327     c                   clear                   fidn43ds
017800090327     c
017900090327     c                   eval      i43crp='N'
018000090327     c                   eval      i43usi='S'
018100120221     c                   eval      i43toi='S'
018200090327     c                   eval      i43ord='2'
018300090327     c                   eval      i43dcd=v1cela
018400090327     c                   eval      i43amgdcd=comela
018500090327     c                   eval      i43rgr='V'
018600121219     c* stampa congelata
018700121219     c                   eval      i43psn ='H'
018800090327     c
018900090327     c                   movel     fidn43ds      kpjbu
019000090327     c                   eval      kcoaz='DN47'
019100090327     c                   call      'BCH10'
019200090327     c                   parm                    kpjba
019300090327     c                   endif
019400090327     c
019500920925     C     FINE          TAG
019600000000     C                   SETON                                        LR
019700900524     C*
019800900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
019900900524     C     CONTR         BEGSR
020000900524     C                   SETOFF                                       90
020100090327     C* DATA elaborazione
020200090327     C                   MOVE      V1cela        G02DAT
020300920925     C                   MOVEL     *BLANK        G02ERR
020400941223     C                   CALL      'XSRDA8'
020500920925     C                   PARM                    WLBDAT
020600920925     C     G02ERR        IFEQ      '1'
020700920925     C                   SETON                                        40  90
020800920925     C                   GOTO      ENDCTR
020900920925     C                   END
021000090327     C                   Z-ADD     G02INV        comela            8 0
021100090327     C                   Z-ADD     G02DAT        V1cela
021200941223     C*
021300900524     C     ENDCTR        ENDSR
021400130129     C*------------------------------------------------------------------------*
021500130129     C* F09GEs - GEST. TASTO FUNZ. F09          ==> skedulazione
021600130129     C*------------------------------------------------------------------------*
021700130129     C     F09GES        BEGSR
021800130129     C*
021900130129     C                   MOVE      '1'           KRITB
022000130129     c
022100130129     c                   call      'BCH09'
022200130129     c                   parm                    kpjba
022300130129     C*
022400130129     C                   ENDSR
022500920925     C*
022600090327     C*--- operazioni iniziali ---------------------------------------*
022700090327     C     *INZSR        BEGSR
022800090327     c
022900090327     C     *ENTRY        PLIST
023000090327     C                   PARM                    KPJBA
023100090401     c
023200090401     c                   z-add     1             codut
023300090327     C                   CALL      'X§PARUT'
023400090327     C                   PARM                    UT§DSE
023500090327     C                   MOVEL     RAGUT         RSUT             20
023600090327     c
023700090327     c                   time                    w0140            14 0
023800090327     c                   move      w0140         v1cela
023900090401     c     *eur          movel     v1cela        wdata
024000090401     c                   Movel(p)  '1Y'          Cod
024100090401     c
024200090401     c     Festav        Tag
024300090401     c                   Subdur    1:*D          Wdata
024400090401     c                   Extrct    Wdata:*y      Kann
024500090401     c                   Extrct    Wdata:*m      Kmes
024600090401     c                   Extrct    Wdata:*d      Wgiorno
024700090401     c     Kazcln        Chain     Azcln01l
024800090401If  2c                   If        %Found(Azcln01l)
024900090401If  3c                   If        Pom(Wgiorno) <> *blanks
025000090401     c                   Movel     Pom(Wgiorno)  Key
025100090401     c     Ktab          Chain     Tabel00f
025200090401     c                   clear                   ds1y
025300090401If  4c                   If        %Found(Tabel00f)
025400090401     c                   Movel     Tbluni        Ds1y
025500090401E   4c                   EndIf
025600090401If  4c                   If        §1yfei = 'S'
025700090401     c                   Goto      Festav
025800090401E   4c                   EndIf
025900090401E   3c                   EndIf
026000090401E   2c                   EndIf
026100090401
026200090401     c     *eur          Move      Wdata         v1cela
026300090327     c                   eval      v1cgio='S'
026400090327     c                   eval      v1criep='S'
026500090403     c                   eval      v1ctotr='S'
026600090327     c                   eval      v1csta ='S'
026700090327     c                   eval      v1cstv ='S'
026800110125     c                   eval      v1cspe ='S'
026900090401     c
027000090401     c     Kazcln        Klist
027100090401     c                   Kfld                    Ktfp
027200090401     c                   Kfld                    Ktfa
027300090401     c                   Kfld                    Kann
027400090401     c                   Kfld                    Kmes
027500090401     C     KTAB          KLIST
027600090401     C                   KFLD                    CODUT
027700090401     C                   KFLD                    COD
027800090401     C                   KFLD                    KEY
027900090401     C*
028000920925     C                   ENDSR
028100110125**
028200110328CHKOBJ OBJ(K018SDINO) OBJTYPE(*JOBQ)
