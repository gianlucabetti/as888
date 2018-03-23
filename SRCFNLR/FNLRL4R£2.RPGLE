000100941109     H*------------------------------------------------------------------------*
000200070329     H* Scambio dati da Bartolini a Tellus => anagrafico giri                  *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600070329     FFIGRG04L  IF   E           K DISK
000700070329     FTIVGD00F  O    E             DISK
000800070329     D*
000900070329     D DTLLGO        E DS
001000070329     D DGRGFLO       E DS
001100060829     D TIS781DS      E DS
001200070207     D TIS781DFLO    E DS
001300070305     D DVGDFLO       E DS
001400070329     D TRUL33DS      E DS
001500070329     D KPJBA         E DS
001600070329     D*
001700070329     D wstacca         s              1    inz
001800070329     D widb            s              7  0 inz
001900070329     D Escbolla        s              1    inz
002000070329     D Dataiso         s               d   datfmt(*iso)
002100070329     D Dataeur         s               d   datfmt(*eur)
002200070330     D In_TST          s              1
002300070330     D In_FGS          s              3S 0
002400070330     D In_DDE          s              8S 0
002500070330     D In_VER          s              8
002600941109     C*------------------------------------------------------------------------*
002700060424     c     *entry        plist
002800070330     c                   parm                    In_TST
002900070330     c                   parm                    In_FGS
003000070330     c                   parm                    In_DDE
003100070330     c                   parm                    In_VER
003200070329     c*
003300070329     c     KEYgrg04c     setll     figrg04l
003400070329     c     KEYgrg04c     reade     figrg04l
003500070329     c                   dow       not %eof(figrg04l)
003600070329     c*
003700060828     c                   exsr      ESCLUDI
003800070329     c*
003900070329     c                   if        Escbolla=' '
004000060828     c                   exsr      ELABORA
004100070329     c                   endif
004200070329     c*
004300070329     c     KEYgrg04c     reade     figrg04l
004400070329     c                   enddo
004500930128     C*
004600060829     c* Finita l'elaborazione chiamo pgm per invio dati a TELLUS
004700060829     c* Imposto la DS
004800060829     c                   clear                   tis781ds
004900070329     c                   eval      §781tip='GO'
005000070329     c                   if        In_TST='S'
005100070207     c                   eval      §781ksu='00TELTST'
005200070207     c                   else
005300070207     c                   eval      §781ksu='00TELLUS'
005400070207     c                   endif
005500060829     c                   eval      §781tsc='WW'
005600060829     c                   eval      §781daT=datcor
005700060831     c                   eval      §781pgm='TLL' + %editc(widb:'X')
005800070207     c* Costruisco il nome da dare al file d output finale
005900070329     c                   eval      §781out='G'+%editc(In_FGS:'X')+
006000070301     c                             %subst(%editc(widb:'X'):2:6)
006100060906     c* Imposto la "S" per il cambio del S.I.
006200070207     c                   clear                   TIS781DFLO
006300070207     c                   movel     'S'           §781flocsi
006400070207     c                   movel     TIS781DFLO    §781flo
006500060831     c
006600060829     c* Finita l'elaborazione chiamo pgm
006700060904     c                   CALL      'TIS781C1'
006800060904     c                   parm      *blanks       ESITO             1
006900060904     c                   parm                    tis781ds
007000060904     c                   parm      *blanks       vgdprg           10
007100070329     c*
007200070329     c                   SETON                                        LR
007300060828     c*------------------------------------------------------------------
007400060828     c     *INZSR        BEGSR
007500070329     c*
007600070329     c     KEYgrg04c     klist
007700070329     c                   kfld                    In_FGS
007800070329     c                   kfld                    In_DDE
007900070329     c*
008000070329     c* Data corrente
008100060829     c                   time                    w0140            14 0
008200070329     c                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
008300060829     c     *eur          movel     w0080         dataeur
008400060829     c                   movel     dataeur       dataiso
008500060829     c     *iso          movel     dataiso       datcor            8 0
008600070329     c*
008700060828     c                   ENDSR
008800060828     c*------------------------------------------------------------------
008900070329     c* Considerazioni x esclusione giri
009000060828     c*------------------------------------------------------------------
009100060828     c     ESCLUDI       BEGSR
009200070329     c*
009300060828     c                   clear                   Escbolla
009400070329     c*
009500070329     c* Escluso dall'esportazione a TELLUS i giri fittizi
009600070329     c                   eval      dgrgflo = grgFLO
009700070329     c                   if        §GRGFIT = 'S'
009800070329     c                   eval      Escbolla = 'S'
009900070329     c                   endif
010000070709     c* Escluso dall'esportazione a TELLUS i giri tipo utilizzo ritiro
010100070709     c                   if        §GRGTUG = 'R'
010200070709     c                   eval      Escbolla = 'S'
010300070709     c                   endif
010400070329     c*
010500070329     c                   ENDSR
010600060828     c*------------------------------------------------------------------
010700060829     c* Elaborazione per scrittura file TIVGD00F
010800060828     c*------------------------------------------------------------------
010900060828     c     ELABORA       BEGSR
011000070329     c*
011100060829     c* Stacco l'ID elaboratore una volta sola
011200060829     c                   if        wstacca=' '
011300060829     c                   clear                   trul33ds
011400060829     c                   movel     'L'           i33tla
011500060829     c                   z-add     60            i33cnu
011600060829     c                   z-add     1             i33num
011700060829     c                   movel     trul33ds      kpjbu
011800060829     c                   call      'TRUL33R'
011900060829     c                   PARM                    kpjba
012000060829     c                   movel     kpjbu         trul33ds
012100070329     c*
012200070118     c                   if        o33err<>*zeros
012300060829     c                   z-add     1             o33nri
012400060829     c                   endif
012500070329     c*
012600070329     c* Imposto il numero staccato in un campo lungo 7
012700060831     c                   z-add     o33nri        widb
012800060829     c                   eval      wstacca='S'
012900060829     c                   endif
013000070329     c*
013100060829     c* Preparo la scrittura del file TIVGD
013200060829     c                   clear                   tivgd000
013300070329     c                   eval      vgdtip='GO'
013400070329     c*
013500070329     c                   if        In_TST='S'
013600070207     c                   eval      vgdksu='00TELTST'
013700070207     c                   else
013800070207     c                   eval      vgdksu='00TELLUS'
013900070207     c                   endif
014000070329     c*
014100060829     c                   eval      vgdtsc='WW'
014200070329     c                   eval      vgddat=datcor
014300060831     c                   eval      vgdpgm='TLL' + %editc(widb:'X')
014400070329     c*
014500070305     c                   clear                   DVGDFLO
014600070305     c                   movel     'P'           §VGDFELA
014700070305     c                   movel     DVGDFLO       vgdflo
014800060829     c
014900060829     c* Imposto i dati della spedizione
015000070329     c                   clear                   DTLLGO
015100070423     c                   eval      §TLLGOATB = 'S'
015200070423     c*
015300070423     c                   if        grgATB   = 'A'
015400070423     c                   eval      §TLLGOATB='N'
015500070423     c                   else
015600070423     c                   if        §grgFIT = *blanks AND
015700070423     c                             §grgEDP = 'S'
015800070423     c     *iso          movel     grgDSCAD      dataiso
015900070423     c                   adddur    60:*D         dataiso
016000070423     c     *iso          movel     dataiso       wDscadFut         8 0
016100070423     c                   if        wDscadFut <  datcor
016200070423     c                   eval      §TLLGOATB='N'
016300070423     c                   endif
016400070423     c                   else
016500070423     c                   if        grgDSCAD <  datcor
016600070330     c                   eval      §TLLGOATB='N'
016700070423     c                   endif
016800070423     c                   endif
016900070330     c                   endif
017000070423     c*
017100070329     c                   move(p)   grgFGS        §TLLGOFGS
017200070329     c                   eval      §TLLGOCGI=grgCGI
017300070329     c                   eval      §TLLGOAIN=grgAIN
017400070329     c                   eval      §TLLGODES=grgDES
017500070329     c                   eval      §TLLGOVER=In_VER
017600070330     c                   move(p)   '.'           §TLLGOFLO
017700070315     c*
017800080404     c                   eval      VGDDTA = %subst(DTLLGO:1:%size(DTLLGO))
017900070315     c*
018000060829     c                   write     tivgd000
018100070315     c*
018200060828     c                   ENDSR
