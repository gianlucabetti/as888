000100941109     H*------------------------------------------------------------------------*
000200070329     H* Scambio dati da Bartolini a Tellus => anagrafico giri                  *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500111205     H DFTACTGRP(*NO) ACTGRP('TELLUS')
000600941109     F*------------------------------------------------------------------------*
000700070329     FFIGRG04L  IF   E           K DISK
000800070329     FTIVGD00F  O    E             DISK
000900070329     D*
001000070329     D DTLLGO        E DS
001100070329     D DGRGFLO       E DS
001200060829     D TIS781DS      E DS
001300070207     D TIS781DFLO    E DS
001400070305     D DVGDFLO       E DS
001500070329     D TRUL33DS      E DS
001600070329     D KPJBA         E DS
001700070329     D*
001800070329     D wstacca         s              1    inz
001900070329     D widb            s              7  0 inz
002000070329     D Escbolla        s              1    inz
002100070329     D Dataiso         s               d   datfmt(*iso)
002200070329     D Dataeur         s               d   datfmt(*eur)
002300070330     D In_TST          s              1
002400070330     D In_FGS          s              3S 0
002500070330     D In_DDE          s              8S 0
002600070330     D In_VER          s              8
002700941109     C*------------------------------------------------------------------------*
002800060424     c     *entry        plist
002900070330     c                   parm                    In_TST
003000070330     c                   parm                    In_FGS
003100070330     c                   parm                    In_DDE
003200070330     c                   parm                    In_VER
003300070329     c*
003400070329     c     KEYgrg04c     setll     figrg04l
003500070329     c     KEYgrg04c     reade     figrg04l
003600070329     c                   dow       not %eof(figrg04l)
003700070329     c*
003800060828     c                   exsr      ESCLUDI
003900070329     c*
004000070329     c                   if        Escbolla=' '
004100060828     c                   exsr      ELABORA
004200070329     c                   endif
004300070329     c*
004400070329     c     KEYgrg04c     reade     figrg04l
004500070329     c                   enddo
004600930128     C*
004700060829     c* Finita l'elaborazione chiamo pgm per invio dati a TELLUS
004800060829     c* Imposto la DS
004900060829     c                   clear                   tis781ds
005000070329     c                   eval      §781tip='GO'
005100070329     c                   if        In_TST='S'
005200070207     c                   eval      §781ksu='00TELTST'
005300070207     c                   else
005400070207     c                   eval      §781ksu='00TELLUS'
005500070207     c                   endif
005600060829     c                   eval      §781tsc='WW'
005700060829     c                   eval      §781daT=datcor
005800060831     c                   eval      §781pgm='TLL' + %editc(widb:'X')
005900070207     c* Costruisco il nome da dare al file d output finale
006000070329     c                   eval      §781out='G'+%editc(In_FGS:'X')+
006100070301     c                             %subst(%editc(widb:'X'):2:6)
006200060906     c* Imposto la "S" per il cambio del S.I.
006300070207     c                   clear                   TIS781DFLO
006400070207     c                   movel     'S'           §781flocsi
006500080905     c                   movel     'P'           §781floela
006600070207     c                   movel     TIS781DFLO    §781flo
006700060831     c
006800060829     c* Finita l'elaborazione chiamo pgm
006900060904     c                   CALL      'TIS781C1'
007000060904     c                   parm      *blanks       ESITO             1
007100060904     c                   parm                    tis781ds
007200060904     c                   parm      *blanks       vgdprg           10
007300070329     c*
007400070329     c                   SETON                                        LR
007500060828     c*------------------------------------------------------------------
007600060828     c     *INZSR        BEGSR
007700070329     c*
007800070329     c     KEYgrg04c     klist
007900070329     c                   kfld                    In_FGS
008000070329     c                   kfld                    In_DDE
008100070329     c*
008200070329     c* Data corrente
008300060829     c                   time                    w0140            14 0
008400070329     c                   MOVE      w0140         w0080             8 0          *DATA (8) IN G/M/AA
008500060829     c     *eur          movel     w0080         dataeur
008600060829     c                   movel     dataeur       dataiso
008700060829     c     *iso          movel     dataiso       datcor            8 0
008800070329     c*
008900060828     c                   ENDSR
009000060828     c*------------------------------------------------------------------
009100070329     c* Considerazioni x esclusione giri
009200060828     c*------------------------------------------------------------------
009300060828     c     ESCLUDI       BEGSR
009400070329     c*
009500060828     c                   clear                   Escbolla
009600070329     c*
009700070329     c* Escluso dall'esportazione a TELLUS i giri fittizi
009800070329     c                   eval      dgrgflo = grgFLO
009900070329     c                   if        §GRGFIT = 'S'
010000070329     c                   eval      Escbolla = 'S'
010100070329     c                   endif
010200070709     c* Escluso dall'esportazione a TELLUS i giri tipo utilizzo ritiro
010300070709     c                   if        §GRGTUG = 'R'
010400070709     c                   eval      Escbolla = 'S'
010500070709     c                   endif
010600070329     c*
010700070329     c                   ENDSR
010800060828     c*------------------------------------------------------------------
010900060829     c* Elaborazione per scrittura file TIVGD00F
011000060828     c*------------------------------------------------------------------
011100060828     c     ELABORA       BEGSR
011200070329     c*
011300060829     c* Stacco l'ID elaboratore una volta sola
011400060829     c                   if        wstacca=' '
011500060829     c                   clear                   trul33ds
011600060829     c                   movel     'L'           i33tla
011700060829     c                   z-add     60            i33cnu
011800060829     c                   z-add     1             i33num
011900060829     c                   movel     trul33ds      kpjbu
012000060829     c                   call      'TRUL33R'
012100060829     c                   PARM                    kpjba
012200060829     c                   movel     kpjbu         trul33ds
012300070329     c*
012400070118     c                   if        o33err<>*zeros
012500060829     c                   z-add     1             o33nri
012600060829     c                   endif
012700070329     c*
012800070329     c* Imposto il numero staccato in un campo lungo 7
012900060831     c                   z-add     o33nri        widb
013000060829     c                   eval      wstacca='S'
013100060829     c                   endif
013200070329     c*
013300060829     c* Preparo la scrittura del file TIVGD
013400060829     c                   clear                   tivgd000
013500070329     c                   eval      vgdtip='GO'
013600070329     c*
013700070329     c                   if        In_TST='S'
013800070207     c                   eval      vgdksu='00TELTST'
013900070207     c                   else
014000070207     c                   eval      vgdksu='00TELLUS'
014100070207     c                   endif
014200070329     c*
014300060829     c                   eval      vgdtsc='WW'
014400070329     c                   eval      vgddat=datcor
014500060831     c                   eval      vgdpgm='TLL' + %editc(widb:'X')
014600070329     c*
014700070305     c                   clear                   DVGDFLO
014800070305     c                   movel     'P'           §VGDFELA
014900070305     c                   movel     DVGDFLO       vgdflo
015000060829     c
015100060829     c* Imposto i dati della spedizione
015200070329     c                   clear                   DTLLGO
015300070423     c                   eval      §TLLGOATB = 'S'
015400070423     c*
015500070423     c                   if        grgATB   = 'A'
015600070423     c                   eval      §TLLGOATB='N'
015700070423     c                   else
015800070423     c                   if        §grgFIT = *blanks AND
015900070423     c                             §grgEDP = 'S'
016000070423     c     *iso          movel     grgDSCAD      dataiso
016100070423     c                   adddur    60:*D         dataiso
016200070423     c     *iso          movel     dataiso       wDscadFut         8 0
016300070423     c                   if        wDscadFut <  datcor
016400070423     c                   eval      §TLLGOATB='N'
016500070423     c                   endif
016600070423     c                   else
016700070423     c                   if        grgDSCAD <  datcor
016800070330     c                   eval      §TLLGOATB='N'
016900070423     c                   endif
017000070423     c                   endif
017100070330     c                   endif
017200070423     c*
017300070329     c                   move(p)   grgFGS        §TLLGOFGS
017400070329     c                   eval      §TLLGOCGI=grgCGI
017500070329     c                   eval      §TLLGOAIN=grgAIN
017600070329     c                   eval      §TLLGODES=grgDES
017700070329     c                   eval      §TLLGOVER=In_VER
017800070330     c                   move(p)   '.'           §TLLGOFLO
017900070315     c*
018000080404     c                   eval      VGDDTA = %subst(DTLLGO:1:%size(DTLLGO))
018100070315     c*
018200060829     c                   write     tivgd000
018300070315     c*
018400060828     c                   ENDSR
