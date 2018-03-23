000300990407     k*--------------------------------------------------------------------------------------------*
000400990407     h DECEDIT('0,') DATEDIT(*DMY.)
000500990407     f*--------------------------------------------------------------------------------------------*
000600990407     f* Data base
000700990407     f*--------------------------------------------------------------------------------------------*
000800110520     fecfim30C  uF   E           K DISK                                         *Metricubi  INFRA
000900110520     ftitas30c  iF   E           K DISK                                         *Bolle
000901110516     fqsysprt   o    f  132        printer
001800990407     d* Variabili riferite al data base
001900990407     d*---
002000110516     d kasaas          S                   LIKE(tasaas)
002100990407     d kaslnp          S                   LIKE(taslnp)
002200990407     d kasnrs          S                   LIKE(tasnrs)
002300990407     d kasnsp          S                   LIKE(tasnsp)
002301110518     d kastbl          S                   LIKE(tastbl)
002500990407     i*--------------------------------------------------------------------------------------------*
002600990407     i* Input
002700990407     i*--------------------------------------------------------------------------------------------*
002800990407     i*---
002900990407     i* Indicatori lettura formati record ECFIM
003000990407     i*---
003100990407     iecfim000      20
003200990407     iecfimp00      21
003300990407     c*--------------------------------------------------------------------------------------------*
003400990407     c* Main lines
003500990407     c*--------------------------------------------------------------------------------------------*
003600990407     c*---
003700990407     c* ECFIM -metricubi-
003800990407     c*---
003900990407     C                   SETOFF                                       2021      *lettura formati
004000990407     c                   READ      ecfim30c                               99
004100990407do  1c                   DOW       NOT *in99
004300990407     c* legge la bolla
004400990407     c                   Z-ADD     fimaas        kasaas
004500990407     c                   Z-ADD     fimlnp        kaslnp
004600990407     c                   Z-ADD     fimnrs        kasnrs
004700990407     c                   Z-ADD     fimnsp        kasnsp
004701110518     c                   movel     fimtbl        kastbl
004900110516     c     keytas        CHAIN     titas30c                           98
005000110516if  3c                   IF        *in98                                        *bolla trovata
005200990407     c* aggiorna
005300990408if  4c                   IF        *in20                                        *lettura ecfim000
005401110518     c                   except    stampa
005402110520     c                   delete    ecfim000                                     *aggiorna
005500990408e   4c                   ENDIF
005600990408if  4c                   IF        *in21                                        *lettura ecfim000
005701110518     c                   except    stampa
005702110520     c                   delete    ecfimp00
005800990408e   4c                   ENDIF
005900990408e   3c                   ENDIF
006100990407     c* lettura successiva
006200990407     C                   SETOFF                                       2021      *lettura formati
006300990407     c                   READ      ecfim30c                               99
006400990407e   1c                   ENDDO
006500990407     c*
006600990407     c                   SETON                                            lr
006700990407     c*--------------------------------------------------------------------------------------------*
006800990407     C* *inzsr - operazioni iniziali
006900990407     c*--------------------------------------------------------------------------------------------*
007000990407     C     *inzsr        BEGSR
007100990407     c*
007200990407     c* chiavi di lettura
007300110516     c     keytas        KLIST
007400990407     c                   KFLD                    kasaas                          -anno
007500990407     c                   KFLD                    kaslnp                          -linea partenza
007600990407     c                   KFLD                    kasnrs                          -serie
007700990407     c                   KFLD                    kasnsp                          -spedizione
007701110520     c*                  KFLD                    kastbl                          -spedizione
007900990407     c*
008000990407     c                   ENDSR
008001110518     oqsysprt   e            stampa      1
008200110518     o                       fimaas              +1
008400110518     o                       fimlnp              +1
008500110518     o                       fimnrs              +1
008600110518     o                       fimnsp              +1
008700110518     o                       fimtbl              +1
