000100110729     ***************************************************************************
000200110729     **
000300110729     ** Questo programma riceve una data e un numero di giorni lavorativi e
000400110729     ** restituisce un elenco di date dove � possibile chiedere ritiri.
000500110729     **
000600110729     ***************************************************************************
000700110801     H DFTACTGRP(*NO) ACTGRP(*NEW)
000800110729
000900110729     ***************************************************************************
001000110729     **
001100110729     ** Costanti.
001200110729     **
001300110729     ***************************************************************************
001400110801     D ESITO_OK...
001500110801     D                 C                   0
001600110729     D ESITO_ERRORE...
001700110729     D                 C                   -1
001800110729     D ESITO_ERRORE_GIORNI_LAVORATIVI...
001900110729     D                 C                   -2
002000110729     D ESITO_ERRORE_DATA_DECORRENZA...
002100110729     D                 C                   -3
002200110729     D LINGUA_FRANCESE...
002300110729     D                 C                   'fr'
002400110729     D LINGUA_INGLESE...
002500110729     D                 C                   'en'
002600110729     D LINGUA_ITALIANO...
002700110729     D                 C                   'it'
002800110729     D LINGUA_TEDESCO...
002900110729     D                 C                   'de'
003000110729
003100110729     ***************************************************************************
003200110729     **
003300110729     ** Strutture dati.
003400110729     **
003500110729     ***************************************************************************
003600110729     D tio10800i     E DS                  QUALIFIED INZ(*EXTDFT)
003700110729     D tio10800o     E DS                  QUALIFIED INZ(*EXTDFT)
003800110729
003900110729     ***************************************************************************
004000110729     **
004100110729     ** Campi.
004200110729     **
004300110729     ***************************************************************************
004400110801     D rqsDataFormat   S             10A   INZ('TIO10800I')
004500110801     D rqsDataSize     S             10I 0 INZ(%SIZE(tio10800i))
004600110729     D rpyEsito        S             10I 0
004700110801     D rpyDataFormat   S             10A   INZ('TIO10800O')
004800110801     D rpyDataSize     S             10I 0 INZ(%SIZE(tio10800o))
004900110729
005000110729     ***************************************************************************
005100110729     **
005200110729     ** Prototipi.
005300110729     **
005400110729     ***************************************************************************
005500110801     D tio70800r       PR                  EXTPGM('TIO70800R')
005600110801     D  rqsDataFormat                10A
005700110801     D  rqsDataSize                  10I 0
005800110801     D  rqsData                            LIKEDS(tio10800i)
005900110801     D  rpyEsito                     10I 0
006000110801     D  rpyDataFormat                10A
006100110801     D  rpyDataSize                  10I 0
006200110801     D  rpyData                            LIKEDS(tio10800o)
006300110729
006400110729
006500110729     ***************************************************************************
006600110729     **
006700110729     ** Main.
006800110729     **
006900110729     ***************************************************************************
007000110729
007100110729      /FREE
007200110729
007300110801       *INLR = *ON;
007400110801
007500110801       tio70800r( rqsDataFormat : rqsDataSize : tio10800i
007600110801                : rpyEsito
007700110801                : rpyDataFormat : rpyDataSize : tio10800o
007800110801                );
007900110801
008000110801       tio10800i.lingua = LINGUA_INGLESE;
008100110801
008200110801       tio70800r( rqsDataFormat : rqsDataSize : tio10800i
008300110801                : rpyEsito
008400110801                : rpyDataFormat : rpyDataSize : tio10800o
008500110801                );
008600110801
008700110801       tio10800i.lingua = LINGUA_FRANCESE;
008800110801
008900110801       tio70800r( rqsDataFormat : rqsDataSize : tio10800i
009000110801                : rpyEsito
009100110801                : rpyDataFormat : rpyDataSize : tio10800o
009200110801                );
009300110801
009400110801       tio10800i.lingua = LINGUA_TEDESCO;
009500110801
009600110801       tio70800r( rqsDataFormat : rqsDataSize : tio10800i
009700110801                : rpyEsito
009800110801                : rpyDataFormat : rpyDataSize : tio10800o
009900110801                );
010000110729
010100110729      /END-FREE
