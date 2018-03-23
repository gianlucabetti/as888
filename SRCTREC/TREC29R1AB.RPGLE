000100000000     H*------------------------------------------------------------------------*
000200000000     H DATEDIT(*YMD)
000300000000     F*------------------------------------------------------------------------*
000400170113     F* DATA BASE
000500000000     F*------------------------------------------------------------------------*
000600161216     Ftabel00f  iF   E           K DISK
000700000000     D*------------------
000800000000     D* DS PARAMETRI DI LANCIO
000900000000     D*------------------
001000170117     D PARAM         E DS                  EXTNAME(TREC28DS)
001100161216     D*-------------------
001200161216     d ds3i          E DS
001300000000     D*-------------------
001400000000     D* ARCHITETTURA
001500000000     D*-------------------
001600000000     D KPJBA         E DS
001700170116     C*------------------------------------------------------------------------*
001800170116     C*   Sottomette il o i lavori
001900170116     C*------------------------------------------------------------------------*
002000000000     C                   SETON                                        LR
002100000000     C*
002200170116     c                   if        partip = *blank
002300161216     c                   z-add     3             volte             3 0
002400161216     c                   else
002500161216     c                   z-add     1             volte             3 0
002600161216     c                   end
002700170116      *
002800000000     C* LANCIA IL PROGRAMMA
002900161216     c                   do        volte         nr                3 0
003000170116      **
003100170116     c                   if        volte<>1
003200161216     c                   if        nr =1
003300161216     C                   MOVEL     'T'           PARtip
003400161216     c                   elseif    nr =2
003500161216     C                   MOVEL     'A'           PARtip
003600161216     c                   elseif    nr =3
003700161216     C                   MOVEL     'P'           PARtip
003800161216     c                   end
003900161216     c                   end
004000170116      **
004100161216     c                   if        PARtip = 'T'
004200161216     C                   MOVEL     soctraini     PARsoc
004300161216     c                   else
004400161216     C                   MOVEL     socBRT        PARsoc
004500161216     c                   end
004600161216     C                   MOVEL     'EC28'        KCOAZ
004700000000     C                   MOVEL     PARAM         KPJBU
004800170227     c                   if        %subst(KNMUS:1:3)='EDP'
004900180205     C                   CALL      'BCH09'
005000180205     C                   PARM                    KPJBA
005100170227     c                   end
005200000000     C                   CALL      'BCH10'
005300000000     C                   PARM                    KPJBA
005400161216     c                   end
005500000000     C*
005600170116     C                   RETURN
005700000000     C*------------------------------------------------------------------------*
005800000000     C* *INZSR - OPERAZIONI INIZIALI
005900000000     C*------------------------------------------------------------------------*
006000000000     C     *INZSR        BEGSR
006100000000     C*---
006200000000     C* RICEVIMENTO PARAMETRI
006300000000     C*---
006400000000     C     *ENTRY        PLIST
006500000000     C                   PARM                    KPJBA
006600170116     C                   movel     KPJBU         PARAM
006700170116     C*---
006800000000     C* VARIABILI NON RIFERITE AL DATABASE
006900161216     C*---
007000161216     C     Ktab          KLIST
007100161216     C                   KFLD                    tblkut
007200161216     C                   KFLD                    tblcod
007300161216     C                   KFLD                    tblkey
007400161216      *
007500161216     c                   clear                   soctraini         3
007600161216     c                   z-add     1             tblkut
007700161216     c                   move      '3I'          tblcod
007800161216     c                   movel(p)  '1'           tblkey
007900161216     c     ktab          chain     tabel00f
008000161216     c                   if        %found(tabel00f)
008100161216     c                   movel     tbluni        ds3i
008200161216     c                   eval      soctraini = %subst(§3IBST: 7: 3)
008300161216     c                   endif
008400161216     c                   move      '201'         socBRT            3
008500000000     C*
008600170117      * se richiamato dal SUPERLANCIO i parametri erano solo i primi 3
008700170117      *  ANNO/MESE/SIMUL   tipo/soc/stampa sono stati implementati con
008800170117      *   il nuovo video.  Quindi il superlancio passerà dei blanks che
008900170117      *    vanno bene qui, a parte la stampa S/N dove nel superlancio
009000170117      *     deve valere sempre "N"--> di conseguenza se riceve BLANK imposta "N"
009100170117     c                   if        parstp = *blank
009200170117     c                   eval      parstp = 'N'
009300170117     c                   end
009400170117      *
009500000000     C                   ENDSR
009600000000     O*------------------------------------------------------------------------*
