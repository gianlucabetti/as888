000100950428     H DECEDIT('0,') DATEDIT(*DMY.)
000200991227
000300000126     FANCLN01L  IP   E           K DISK
000400000127     FFNSPE03L  IF   E           k DISK
000500000127     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600000126      *
000700000126     D SPWFLS          S                   LIKE(SPEFLS) INZ('L')
000800000126     D SPWCOD          S                   LIKE(SPECOD) INZ('001')
000900000126     D WRKKSC          S                   LIKE(SPEcli)
001000000126     C*---------------------------------------------------------------*
001100991227
001200000126     C*---------------------------------------------------------------*
001300000126     C     KSPE          KLIST
001400000126     C                   KFLD                    SPWFLS
001500000126     C                   KFLD                    WRKKSC
001600000126     C                   KFLD                    SPWCOD
001700000126     C*
001800000126     C*---------------------------------------------------------------*
001900000126    4C* per ogni angrafica controllo se il codice di fatturazione del cliente è diverso da
002000000126     c* dal codice cliente
002100000127     c                   if        CLNksc <> CLNclifatt
002200000126     c* verifico se esiste il codice luogo 001 per il codice cliente
002300000126     c* e se si annullo il record
002400000126     c                   move      clnksc        wrkksc
002500000126     c*
002600000126     c     kspe          chain     fnspe03l                           30
002700000127     C                   IF        NOT *IN30
002800000127     c                   eval      speflg = 'A'
002900000127     c                   clear                   speftr
003000000127     c                   clear                   spedtr
003100000127     c                   update    fnspe000
003200000127     c   of
003300000127     corn90              except    testa
003400000127     c                   except    riga
003500000127     c   of              setoff                                       of
003600000127     c  n90              seton                                        90
003700000127     C                   ENDIF
003800000126     c*
003900000127     c                   endif
004000991227    3 *
004100000127     oqsysprt   E            TESTA            02
004200000127     O                                              'Bollettaz.'
004300000127     o                                        +   1 'Fatturaz.'
004400000127     O          E            RIGA        1
004500000127     O                       CLNKSC           +   1
004600000127     O                       CLNCLIFATT       +   1
