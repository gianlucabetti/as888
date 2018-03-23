000100941109     H*------------------------------------------------------------------------*
000200110504     H* STAMPA Anagrafica punti di scambio traini   - esegue                   *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600941109     F* DATA BASE
000700941109     F*------------------------------------------------------------------------*
000800930129     FAZORG01L  IF   E           K DISK
000900110504     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
001000000000     F                                     FORMLEN(66)
001100000000     F                                     FORMOFL(60)
001200941109     D*------------------------------------------------------------------------*
001300110505     D TES             S             66    DIM(8) CTDATA PERRCD(1)
001400941109     D*-------------------
001500930210     D* DS PARAMETRI DI LANCIO PGM
001600941109     D*-------------------
001700970619     D PARAM           DS
001800110504     D  PARdri                 1      8  0
001900110504     D  PARdrigma              9     16  0
002000110504     D  PARord                17     17
002100110504     d
002200941109     D*-------------------
002300941109     D* DS CONTROLLO DATE
002400941109     D*-------------------
002500930129     D G02PAR          DS
002600941109     D  G02DAT                 1      8  0
002700941109     D  G02INV                 9     16  0
002800941109     D  G02ERR                17     17
002900941109     D  G02TGI                18     22  0
003000941109     D*-------------------
003100000000     D KPJBA         E DS
003200020906     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003300070207     D* DS PER TISI95R - CONTROLLO DI CAP
003400070207     D tisi95ds      E DS
003500060421     d
003600110504     d PADTOR          s             16
003700110505     d p1ddde          s              8  0
003800110505     d p1ddsc          s              8  0
003900110504     d
004000110505     D tnpscds       E DS                  EXTNAME(TNPSC00F)
004100110505     d
004200110505     d wrkgetlista     s           4096    varying
004300941109     C*------------------------------------------------------------------------*
004400930128     C*
004500930128     C* STAMPA INTESTAZIONE STAMPA
004600930128     C                   EXCEPT    TESTA
004700000000     C*
004800110504     C* Elaboro in base all'ordinamento
004900110504     c                   exsr      carSQL
005000000000     C*
005100930128     C* STAMPA RIGA DI FINE STAMPA
005200000000     C                   EXCEPT    STAFIN
005300000000     C*
005400000000     C                   SETON                                        LR
005500110504     C*------------------------------------------------------------------------*
005600110504     C*? carico SQL per i dati dal file TNPSC
005700110504     C*------------------------------------------------------------------------*
005800110504     C     CARSQL        BEGSR
005900110504     c                   eval      wrkgetlista =
006000110504     c                             'select * from tnpsc00f where  -
006100110504     c                                pscatb='' ''  order by '
006200110504     c*
006300110504     c                   select
006400110504     c                   when      parord='S'
006500110504     c                   eval      wrkgetlista=%trim(wrkgetlista)+ ' PSCPSC'
006600110504     c                   when      parord='P'
006700110504     c                   eval      wrkgetlista=%trim(wrkgetlista)+ ' PSCPRV, -
006800110504     c                             PSCCAP'
006900110504     c                   other
007000110504     c                   eval      wrkgetlista=%trim(wrkgetlista)+ ' PSCCAP '
007100110504     c                   endsl
007200110504
007300110504     C/EXEC SQL
007400110504     C+ PREPARE s1 FROM :wrkgetlista
007500110504     C/END-EXEC
007600110504     C
007700110504     C/EXEC SQL
007800110504     C+ DECLARE A1 CURSOR FOR S1
007900110504     C/END-EXEC
008000110504     C
008100110504     C/EXEC SQL
008200110504     C+ OPEN a1
008300110504     C/END-EXEC
008400110504
008500110504      *?Leggo il file
008600110504     c                   do        *hival
008700110504     C/EXEC SQL
008800110505     C+ FETCH NEXT FROM a1 INTO : tnpSCds
008900110504     C/END-EXEC
009000110504
009100110504     c                   select
009200110504
009300110504     c                   when      sqlcod = 100
009400110504     c                   leave
009500110504     c                   when      sqlcod < 0
009600110504     c                   seton                                        H1
009700110504     c                   leave
009800110504     c                   other
009900110504     c
010000110504     c                   exsr      chkrec
010100110504     c
010200110504     C* STAMPA RIGA DI DETTAGLIO TEMPI
010300110504     C     WSALTA        IFEQ      'N'
010400110504     C                   EXSR      STARIG
010500110504     C                   END
010600110504     c
010700110504     c                   endsl
010800110504
010900110504     c                   enddo
011000110504
011100110504     C/EXEC SQL
011200110504     C+ CLOSE a1
011300110504     C/END-EXEC
011400110504
011500110504     c                   ENDSR
011600941109     C*------------------------------------------------------------------------*
011700930210     C*? CHKREC - CONTROLLA SE IL RECORD E' DA CONSIDERARE O NO
011800941109     C*------------------------------------------------------------------------*
011900930210     C     CHKREC        BEGSR
012000930210     C*
012100930210     C* IMPOSTA A NO IL FLAG PER LA GESTIONE DEL RECORD
012200930210     C                   MOVEL     'N'           WSALTA            1
012300930210     C*
012400110506     C* Verifico data rif,  se impostata
012500110506     C                   if        pardri>0  and pscdsc>0
012600110504     c
012700110506     c                   if         pscdsc>=pardri
012800110504     c                   else
012900060421     C                   MOVEL     'S'           WSALTA
013000060421     C                   END
013100110504     C                   END
013200941109     C*
013300930210     C                   ENDSR
013400941109     C*------------------------------------------------------------------------*
013500941109     C*? STARIG - STAMPA UNA RIGA
013600941109     C*------------------------------------------------------------------------*
013700930129     C     STARIG        BEGSR
013800070207     c                   setoff                                       10
013900070207     c* chiamo tisi95
014000070207     C*
014100070207     C                   CLEAR                   TISI95DS
014200070207     C                   EVAL      I95TCN = '7'
014300110504     C                   EVAL      I95DAT = pardri
014400110505     C                   EVAL      I95NAR = pscnar
014500110505     C                   EVAL      I95PRV = pscprv
014600110505     C                   EVAL      I95CAP = psccap
014700110505     C                   EVAL      I95LOC = pscloc
014800070207     C                   CALL      'TISI95R'
014900070207     C                   PARM                    TISI95DS
015000110504     c* Se c'e' la località, memorizzo se obsoleta
015100110504    2c                   if        o95lf1='O'
015200110504     c                   move      'OBS'         psccap
015300070207     c                   seton                                        10
015400070207     c                   endif
015500930129     C*
015600930129     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
015700930129     C     *INOF         IFEQ      '1'
015800930129     C                   EXCEPT    TESTA
015900930129     C                   SETOFF                                       OF
016000930129     C                   END
016100930129     C*
016200930129     C* INVERTO LA DATA DI DECORRENZA
016300110506     C**                 Z-ADD     pscdde        G02INV
016400110506     C**                 Z-ADD     *ZEROS        G02DAT
016500110506     C**                 MOVEL     '3'           G02ERR
016600110506     C**                 CALL      'XSRDA8'
016700110506     C**                 PARM                    G02PAR
016800110506     C**                 Z-ADD     G02DAT        p1ddde
016900110504
017000110506     c                   clear                   p1ddsc
017100110506     c                   if        pscdsc>0
017200110505     C                   Z-ADD     pscdsc        G02INV
017300110504     C                   Z-ADD     *ZEROS        G02DAT
017400110504     C                   MOVEL     '3'           G02ERR
017500110504     C                   CALL      'XSRDA8'
017600110504     C                   PARM                    G02PAR
017700110504     C                   Z-ADD     G02DAT        p1ddsc
017800110506     c                   endif
017900110506     c
018000930129     C*
018100930129     C* STAMPA RIGA DI DETTAGLIO TEMPI
018200930129     C                   EXCEPT    RIGA
018300930129     C*
018400930129     C                   ENDSR
018500941109     C*------------------------------------------------------------------------*
018600941109     C*?*INZSR -  ROUTINE INIZIALE
018700941109     C*------------------------------------------------------------------------*
018800941109     C     *INZSR        BEGSR
018900000000     C*
019000930129     C* PARAMETRI IN INPUT AL PGM
019100000000     C     *ENTRY        PLIST
019200000000     C                   PARM                    KPJBA
019300970619     C                   MOVEL     KPJBU         PARAM
019400060421     c
019500110505     c                   z-add     1             codut             1 0
019600110505     c
019700930129     C* RECUPERO IL CODICE DELL'AZIENDA
019800930129     C                   Z-ADD     1             CODUT
019900020830     C                   CALL      'X§PARUT'
020000930129     C                   PARM                    UTEDSE
020100930129     C                   MOVEL     RAGUT         RSUT             25
020200000000     C*
020300930129     C* RECUPERO LA DATA E L'ORA
020400060421     c                   time                    w0140            14 0
020500060421     C                   MOVE      w0140         wdata             8 0          *DATA (8) IN G/M/AA
020600930129     C                   TIME                    ORA               6 0
020700110504     c* Dati testata stampa
020800110504     c                   clear                   padtor
020900110504     c                   select
021000110505     c                   when      parord='S'
021100110505     c                   eval      padtor='PUNTO di SCAMBIO'
021200110505     c                   when      parord='P'
021300110505     c                   eval      padtor='PROVINCIA/CAP'
021400110504     c                   other
021500110504     c                   eval      padtor='C.A.P.      '
021600110505     c                   endsl
021700000000     C*
021800000000     C                   ENDSR
021900941109     O*------------------------------------------------------------------------*
022000941109     O*                 *** STAMPA TEMPI DI TRAZIONE  ***
022100941109     O*------------------------------------------------------------------------*
022200930128     OQSYSPRT   E            TESTA          1 02
022300060421     O                       RSUT                25
022400110504     O                                           65 '** STAMPA ANAGRAFICA PUNTI'
022500110504     O                                         +  0 ' DI SCAMBIO **'
022600110504     O                                           95 'TNTLP3R'
022700060421     O                                          106 'Data'
022800060421     O                       wdata              117 '  /  /    '
022900060424     O                                          128 'Pagina'
023000930128     O                       PAGE          Z    132
023100941109     O*-------
023200060421     O          E            TESTA       1  1
023300060421     O                                          106 'Ora'
023400060421     O                       ORA                115 '  :  :  '
023500060421     O*-------
023600070207     O          E            TESTA       1  1
023700060421     O                       TES(1)              66
023800110504     O                       padtor              34
023900110505     O                       pardrigma           64 '  /  /    '
024000941109     O*-------
024100070207     O          E            TESTA       1
024200930129     O                       TES(3)              66
024300110504     O                       TES(5)             132
024400941109     O*-------
024500070207     O          E            TESTA       1
024600110504     O                       TES(4)              66
024700110504     O                       TES(6)             132
024800070207     O          E            TESTA       1
024900060421     O                       TES(2)              66
025000060421     O                       TES(2)             132
025100941109     O*------------------------
025200070207     O          E            RIGA        1
025300110505     O                       pscpsc           +   2
025400110505     O                       pscind           +   4
025500110504     O                       psccap           +   2
025600110504     O                       pscloc           +   2
025700110504     O                       pscprv           +   2
025800110504     O                       pscnar           +   1
025900110504     O                       p1ddsc           +   3 '  /  /    '
026000110504     O          E    10      RIGA        0  0
026100110505     O                       psccap           +  53
026200110505     O                       pscloc           +   2
026300110505     O                       pscprv           +   2
026400110505     O                       pscnar           +   1
026500110504
026600941109     O*------------------------
026700000000     O          E            STAFIN           60
026800941109     O                                           89 ' F I N E   S T A M P A '
026900000000     O*----------------------------------------------------------------
027000941109**
027100110504     Ordinamento: xxxxxxxxxxxxxxxx     In decorrenza: xx/xx/xxxx  1
027200060421------------------------------------------------------------------2
027300110505    Codice      Indirizzo                            C.A.P.     Lo3
027400110504Punto di Scambio                                                  4
027500110506calità                                        Data
027600110506                                  Prv Naz   Scadenza
027700110504  xxxxxxxxxx    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxxxxxxxx  xx
027800110506xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xx xxx  xx/xx/xxxx
