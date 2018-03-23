000100031002      * DS Esterna x gestione allocazione ed invio messaggi
000101040315     FTIVIR00F  UF   E             DISK    infds(tivirinf)
000102040315
000104040315     D tivirinf        ds
000105040315     D  tivirnrri            397    400i 0
000107040315
000301040315     C*
000302040315     C* Leggo il 1° record del file TIVIR00F
000303040315     C     1             CHAIN     TIVIR00F
002100031002     C*
002200031002     C                   SETON                                        LR
