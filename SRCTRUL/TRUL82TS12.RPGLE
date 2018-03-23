000100031002      * DS Esterna x gestione allocazione ed invio messaggi
000101040315     FTIVIR00F  UF   E             DISK    infds(tivirinf)
000102040315
000104040315     D tivirinf        ds
000105040315     D  tivirnrri            397    400i 0
000106040315
000107040315     D TRUL82DS      E DS
000108040315     D STATUS         SDS
000112040315     D EXCP_DATA              91    170                                         * Error Message
000114040315
000115040315     C*
000116040315     C* Leggo il 2° record del file TIVIR00F
000117040315     C     2             CHAIN     TIVIR00F
000301040315     C*
000302040315     C* Leggo il 1° record del file TIVIR00F
000303040315     C     1             CHAIN(e)  TIVIR00F
000304040315     C*
000305040315     C* Se errore chiamo il *pgm d utilità d controllo allocazioni record
000306040315     C                   IF        %error
000307040315     C* Valorizzo la DS di procedura
000308040315     C                   CLEAR                   TRUL82DS
000309040315     C                   EVAL      UL82§FIL = 'TIVIR00F'
000310040315     C***                EVAL      UL82§WIN = 'S'
000311040315     C                   EVAL      UL82§WIN = 'N'
000312040315     C***                EVAL      UL82§MSS = 'Babbeo muoviti ad uscire da '+
000313040315     C***                                     'questa videata!!!'
000314040315     C                   EVAL      UL82§F5  = 'S'
000315040315     C***                EVAL      UL82§F7  = 'S'
000316040315     C*
000317040315     C***                EVAL      UL82§NUM = 0
000318040315     C***                EVAL      UL82§ATT = 0
000319040315     C*
000320040315     C* Effettuo la chiamata al *PGM d utilità
000321040315     C                   CALL      'TRUL82R'
000322040315     C                   PARM                    TRUL82DS
000323040315     C*
000324040315     C                   ENDIF
002100031002     C*
002200031002     C                   SETON                                        LR
