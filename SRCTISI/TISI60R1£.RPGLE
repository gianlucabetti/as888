000100000000     H*------------------------------------------------------------------------*
000200051103     H* STAMPA DELIVERY TIME - PARAMETRI (Da stampa Testi Lettera)
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800051103     FTNTAM01L  IF   E           K DISK
000900060217     F****TNOFM01L  IF   E           K DISK    RENAME(TNTAM000:TNOFM000)
001000051103     D FNLV45DS      E DS
001100000000     D*---------------------
001200000000     D* DS PARAMETRI DI LANCIO
001300000000     D*------------------
001400000000     D PARAM           DS                  INZ
001500000000     D  PARLNP                 1      3
001600000000     D  PARTEM                 4      4
001700000000     D  PARTSP                 5      5
001800000000     D  PARISO                 6      6
001900980210     D  PARDIS                 7      7
002000980210     D  PARSSA                 8      8
002100980210     D  PARND1                 9     83
002200980210     D  PARND2                84    158
002300980210     D  PARND3               159    233
002400060612     d  parlin               234    234
002500980210     D*---------------------
002600980210     D* ARCHITETTURA
002700980210     D*---------------------
002800980210     D KPJBA         E DS
002900060217     d
003000060217     d K_i45ksc        s                   Like(i45ksc)
003100000000     C*------------------------------------------------------------------------*
003200000000     C* MAIN LINE
003300000000     C*------------------------------------------------------------------------*
003400000000     C*
003500051103     c     *entry        plist
003600051103     C                   PARM                    KPJBA
003700051103     C                   PARM                    fnlv45ds
003800980210     C*
003900051103     c* in base alla tariffa reperisco il tipo servizio da passare
004000060217     c                   if        i45ast = 'C'
004100060217     c                   z-add     i45ksc        k_i45ksc
004200060217     c                   else
004300060217     c                   if        i45ast = 'V'
004400060217     c                   z-add     i45nrv        k_i45ksc
004500060217     c                   endif
004600060217     c                   endif
004700060217     c*
004800060217     c     ktam          chain     tntam01l
004900051103     c
005000051103     C                   MOVEL     i45app        PARLNP                         *LINEA DI PARTENZA
005100051103     C                   MOVEL     'C'           PARTEM                         *TEMPI DELIVERY
005200051103     C                   MOVEL     tamtsp        PARTSP                         *TIPO SERVIZIO
005300051103     C                   MOVEL     'N'           PARISO                         *CONSIDERA ISOLE
005400051103     C                   MOVEL     'N'           PARDIS                         *CONSIDERA L.DISAGG.
005500051103     C                   MOVEL     'S'           PARSSA                         *SINTETICA/DETTAGLIO
005600051103     C                   clear                   PARND1                         *NOTA DELIVERY N°1
005700051103     C                   clear                   PARND2                         *NOTA DELIVERY N°2
005800051103     C                   clear                   PARND3                         *NOTA DELIVERY N°3
005900060612     c                   eval      parlin = i45lin
006000000000     C*
006100000000     C                   MOVEL     PARAM         KPJBU
006200051103     c                   call      'TISI62R'
006300000000     C                   PARM                    KPJBA
006400000000     C*
006500051103     C                   SETON                                        LR
006600000000     C*------------------------------------------------------------------------*
006700000000     C* *INZSR - ROUTINE INIZIALE
006800000000     C*------------------------------------------------------------------------*
006900000000     C     *INZSR        BEGSR
007000980210     C*---
007100000000     C* CHIAVI DI LETTURA
007200980210     C*---
007300051103     C*
007400060217     C     KTAM          KLIST
007500060217     C                   KFLD                    k_i45ksc
007600051103     C                   KFLD                    i45ctr
007700051103     C                   KFLD                    i45prg
007800000000     C*
007900000000     C                   ENDSR
