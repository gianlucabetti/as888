000100000000     H*------------------------------------------------------------------------*
000200051103     H* STAMPA DELIVERY TIME - PARAMETRI (Da stampa Testi Lettera)
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800070312     FTNTAM01L  IF   E           K DISK    usropn
000900070312     FTNOFM01L  IF   E           K DISK    usropn
001000070312     F                                     RENAME(TNTAM000:TNOFM000)
001100051103     D FNLV45DS      E DS
001200000000     D*---------------------
001300000000     D* DS PARAMETRI DI LANCIO
001400000000     D*------------------
001500000000     D PARAM           DS                  INZ
001600000000     D  PARLNP                 1      3
001700000000     D  PARTEM                 4      4
001800000000     D  PARTSP                 5      5
001900000000     D  PARISO                 6      6
002000980210     D  PARDIS                 7      7
002100980210     D  PARSSA                 8      8
002200980210     D  PARND1                 9     83
002300980210     D  PARND2                84    158
002400980210     D  PARND3               159    233
002500060612     d  parlin               234    234
002600980210     D*---------------------
002700980210     D* ARCHITETTURA
002800980210     D*---------------------
002900980210     D KPJBA         E DS
003000060217     d
003100060217     d K_i45ksc        s                   Like(i45ksc)
003200000000     C*------------------------------------------------------------------------*
003300000000     C* MAIN LINE
003400000000     C*------------------------------------------------------------------------*
003500000000     C*
003600051103     c     *entry        plist
003700051103     C                   PARM                    KPJBA
003800051103     C                   PARM                    fnlv45ds
003900980210     C*
004000051103     c* in base alla tariffa reperisco il tipo servizio da passare
004100060217     c                   if        i45ast = 'C'
004200060217     c                   z-add     i45ksc        k_i45ksc
004300070312     c                   open      tntam01l
004400070312     c     ktam          chain     tntam01l
004500070312     c                   endif
004600060217     c                   if        i45ast = 'V'
004700060217     c                   z-add     i45nrv        k_i45ksc
004800070312     c                   open      tnofm01l
004900070312     c     ktam          chain     tnofm01l
005000060217     c                   endif
005100060217     c*
005200051103     c
005300051103     C                   MOVEL     i45app        PARLNP                         *LINEA DI PARTENZA
005400051103     C                   MOVEL     'C'           PARTEM                         *TEMPI DELIVERY
005500051103     C                   MOVEL     tamtsp        PARTSP                         *TIPO SERVIZIO
005600051103     C                   MOVEL     'N'           PARISO                         *CONSIDERA ISOLE
005700051103     C                   MOVEL     'N'           PARDIS                         *CONSIDERA L.DISAGG.
005800051103     C                   MOVEL     'S'           PARSSA                         *SINTETICA/DETTAGLIO
005900051103     C                   clear                   PARND1                         *NOTA DELIVERY N°1
006000051103     C                   clear                   PARND2                         *NOTA DELIVERY N°2
006100051103     C                   clear                   PARND3                         *NOTA DELIVERY N°3
006200060612     c                   eval      parlin = i45lin
006300000000     C*
006400000000     C                   MOVEL     PARAM         KPJBU
006500051103     c                   call      'TISI62R'
006600000000     C                   PARM                    KPJBA
006700000000     C*
006800051103     C                   SETON                                        LR
006900000000     C*------------------------------------------------------------------------*
007000000000     C* *INZSR - ROUTINE INIZIALE
007100000000     C*------------------------------------------------------------------------*
007200000000     C     *INZSR        BEGSR
007300980210     C*---
007400000000     C* CHIAVI DI LETTURA
007500980210     C*---
007600051103     C*
007700060217     C     KTAM          KLIST
007800060217     C                   KFLD                    k_i45ksc
007900051103     C                   KFLD                    i45ctr
008000051103     C                   KFLD                    i45prg
008100000000     C*
008200000000     C                   ENDSR
