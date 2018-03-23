000100960903     H*------------------------------------------------------------------------*
000200960903     H* IMPORTAZIONE BUDGET COMMERCIALE DA PC - ESEGUE                         *
000300960903     H*------------------------------------------------------------------------*
000400960903     H DECEDIT('0,') DATEDIT(*DMY.)
000500960903     F*------------------------------------------------------------------------*
000600960903     F* DATA BASE                                                              *
000700960903     F*------------------------------------------------------------------------*
000800020114     FTEBDP00F  IF   E           K DISK
000900020114     FTEBDC01L  UF A E           K DISK
001000960903     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100960903     D*------------------------------------------------------------------------*
001200960903     D* SCHIERE                                                                *
001300960903     D*------------------------------------------------------------------------*
001400960903     D*------------------
001500960903     D* RIGHE DI STAMPA
001600960903     D*------------------
001700960903     D RIG             S             66    DIM(7) CTDATA PERRCD(1)
001800960903     D*------------------
001900960903     D* RIGHE DECODIFICA
002000960903     D*------------------
002100960903     D DPAR            S             40    DIM(3) CTDATA PERRCD(1)
002200960903     D*------------------------------------------------------------------------*
002300960903     D* INPUT                                                                  *
002400960903     D*------------------------------------------------------------------------*
002500960903     D*------------------
002600960903     D* ARCHITETTURA
002700960903     D*------------------
002800960903     D KPJBA         E DS
002900960903     D*------------------
003000020916     D* DS REPERIMENTO DATI UTENTE
003100020916     D*-------------------
003200020916     D TIBS34DS      E DS                                                       *Profili utente
003300020916     D DDATIUTE      E DS                                                       *Dati utente
003400020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003500960903     D*------------------
003600960903     D* PARAMETRI DI LANCIO
003700960903     D*------------------
003800960903     D PARAM           DS
003900960903     D  PARANN                 1      4  0
004000960903     D  PARAGG                 5      5
004100960903     C*------------------------------------------------------------------------*
004200960903     C* MAIN LINE
004300960903     C*------------------------------------------------------------------------*
004400960903     C*
004500960903     C* STAMPA PARAMETRI DI LANCIO
004600960903     C                   EXSR      STAPAR
004700960903     C*
004800960903     C* PRIMA LETTURA BUDGET PC
004900960903     C                   EXSR      LETFIL
005000960903     C*
005100960903     C* CICLO FINO A FINE FILE
005200960903DO  1C     $FINE         DOWEQ     '0'
005300960903     C*
005400960903     C* SCRIVE BUDGET AS/400
005500960903     C                   EXSR      SCRIVE
005600960903     C*
005700960903     C* LETTURA SUCCESSIVO BUDGET PC
005800960903     C                   EXSR      LETFIL
005900960903E   1C                   ENDDO
006000960903     C*
006100960903     C* STAMPA RIGA RIEPILOGO
006200960903     C                   EXCEPT    TES
006300960903     C                   EXCEPT    RIE
006400960903     C*
006500960903     C* STAMPA RIGA FINE PROSPETTO
006600960903     C                   EXCEPT    FIN
006700960903     C*
006800960903     C                   SETON                                        LR
006900960903     C*------------------------------------------------------------------------*
007000960903     C* LETFIL - LETTURA PROSSIMO RECORD
007100960903     C*------------------------------------------------------------------------*
007200960903     C     LETFIL        BEGSR
007300960903     C*
007400960903     C* LEGGO FINO A:
007500960903     C*     - FINE FILE
007600960903     C*     - TROVATO RECORD VALIDO
007700960903     C                   MOVEL     'N'           WRECOK
007800960903DO  1C     $FINE         DOWEQ     '0'
007900960903     C     WRECOK        ANDEQ     'N'
008000960903     C*
008100020114     C                   READ      TEBDP00F                               99
008200960903     C*
008300960903IF  2C     *IN99         IFEQ      *ON
008400960903     C                   MOVEL     '1'           $FINE                          *FINE ARCHIVIO
008500960903X   2C                   ELSE
008600960903     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
008700960903IF  3C     WRECOK        IFEQ      'S'                                          *NO ERRORE
008800960903     C                   ADD       1             RECLET                         *INCREMENTA CONTATORE
008900960903E   3C                   ENDIF
009000960903E   2C                   ENDIF
009100960903     C*
009200960903E   1C                   ENDDO
009300960903     C*
009400960903     C                   ENDSR
009500960903     C*------------------------------------------------------------------------*
009600960903     C* CHKREC - CONTROLLA VALIDITA' RECORD
009700960903     C*------------------------------------------------------------------------*
009800960903     C     CHKREC        BEGSR
009900960903     C*
010000960903     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
010100960903     C*
010200960903     C                   ENDSR
010300960903     C*------------------------------------------------------------------------*
010400960903     C* SCRIVE - SCRIVE BUDGET AS/400
010500960903     C*------------------------------------------------------------------------*
010600960903     C     SCRIVE        BEGSR
010700960903     C*---
010800960903     C* PER OGNI RECORD LETTO DAL BUDGET PC DEVE CREARE 12 RECORD DEL BUDGET AS.
010900960903     C* CONTROLLA PERCIO', MESE PER MESE, L' ESISTENZA DEL RECORD SUL BUDGET AS.
011000960903     C* SE LO TROVA VIENE GUIDATO DA FLAG DI AGGIOR.TO, SE NON LO TROVA AGGIUNGE
011100960903     C*---
011200960903     C                   Z-ADD     TBPAGE        KBDAGE                         *AGENTE
011300960903     C                   Z-ADD     PARANN        KBDANN                         *ANNO
011400960903     C                   MOVEL     TBPIOE        KBDIOE                         *ITALIA/ESTERO
011500960903     C*--------------------
011600960903     C* 1°  M E S E
011700960903     C*--------------------
011800960903     C                   Z-ADD     01            KBDMES                         *MESE
011900020114     C     KEYBDC        CHAIN     TEBDC01L                           99
012000960903     C*
012100960903     C* VALORIZZA I CAMPI DEL BUDGET AS
012200960903     C                   Z-ADD     TBPSP1        TBCNSP                         *SPEDIZIONI
012300020114     C**         TBPRB1    MULT 1000      TBCRIC           *RICAVI NETTI
012400020114     C                   Z-ADD     TBPRB1        TBCRIC                         *RICAVI NETTI
012500960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
012600960903     C*---
012700960903     C* AGGIUNTA
012800960903     C*---
012900960903IF  1C     *IN99         IFEQ      *ON
013000960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
013100960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
013200960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
013300960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
013400020114     C                   WRITE     TEBDC000                             97
013500960903IF  2C     *IN97         IFEQ      *OFF
013600960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
013700960903E   2C                   ENDIF
013800960903X   1C                   ELSE
013900960903     C*---
014000960903     C* AGGIORNAMENTO
014100960903     C*---
014200960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
014300020114     C                   UPDATE    TEBDC000                             97
014400960903IF  3C     *IN97         IFEQ      *OFF
014500960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
014600960903E   3C                   ENDIF
014700960903E   2C                   ENDIF
014800960903E   1C                   ENDIF
014900960903     C*--------------------
015000960903     C* 2°  M E S E
015100960903     C*--------------------
015200960903     C                   Z-ADD     02            KBDMES                         *MESE
015300020114     C     KEYBDC        CHAIN     TEBDC01L                           99
015400960903     C*
015500960903     C* VALORIZZA I CAMPI DEL BUDGET AS
015600960903     C                   Z-ADD     TBPSP2        TBCNSP                         *SPEDIZIONI
015700020114     C**         TBPRB2    MULT 1000      TBCRIC           *RICAVI NETTI
015800020114     C                   Z-ADD     TBPRB2        TBCRIC                         *RICAVI NETTI
015900960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
016000960903     C*---
016100960903     C* AGGIUNTA
016200960903     C*---
016300960903IF  1C     *IN99         IFEQ      *ON
016400960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
016500960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
016600960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
016700960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
016800020114     C                   WRITE     TEBDC000                             97
016900960903IF  2C     *IN97         IFEQ      *OFF
017000960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
017100960903E   2C                   ENDIF
017200960903X   1C                   ELSE
017300960903     C*---
017400960903     C* AGGIORNAMENTO
017500960903     C*---
017600960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
017700020114     C                   UPDATE    TEBDC000                             97
017800960903IF  3C     *IN97         IFEQ      *OFF
017900960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
018000960903E   3C                   ENDIF
018100960903E   2C                   ENDIF
018200960903E   1C                   ENDIF
018300960903     C*--------------------
018400960903     C* 3°  M E S E
018500960903     C*--------------------
018600960903     C                   Z-ADD     03            KBDMES                         *MESE
018700020114     C     KEYBDC        CHAIN     TEBDC01L                           99
018800960903     C*
018900960903     C* VALORIZZA I CAMPI DEL BUDGET AS
019000960903     C                   Z-ADD     TBPSP3        TBCNSP                         *SPEDIZIONI
019100020114     C**         TBPRB3    MULT 1000      TBCRIC           *RICAVI NETTI
019200020114     C                   Z-ADD     TBPRB3        TBCRIC                         *RICAVI NETTI
019300960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
019400960903     C*---
019500960903     C* AGGIUNTA
019600960903     C*---
019700960903IF  1C     *IN99         IFEQ      *ON
019800960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
019900960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
020000960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
020100960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
020200020114     C                   WRITE     TEBDC000                             97
020300960903IF  2C     *IN97         IFEQ      *OFF
020400960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
020500960903E   2C                   ENDIF
020600960903X   1C                   ELSE
020700960903     C*---
020800960903     C* AGGIORNAMENTO
020900960903     C*---
021000960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
021100020114     C                   UPDATE    TEBDC000                             97
021200960903IF  3C     *IN97         IFEQ      *OFF
021300960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
021400960903E   3C                   ENDIF
021500960903E   2C                   ENDIF
021600960903E   1C                   ENDIF
021700960903     C*--------------------
021800960903     C* 4°  M E S E
021900960903     C*--------------------
022000960903     C                   Z-ADD     04            KBDMES                         *MESE
022100020114     C     KEYBDC        CHAIN     TEBDC01L                           99
022200960903     C*
022300960903     C* VALORIZZA I CAMPI DEL BUDGET AS
022400960903     C                   Z-ADD     TBPSP4        TBCNSP                         *SPEDIZIONI
022500020114     C**         TBPRB4    MULT 1000      TBCRIC           *RICAVI NETTI
022600020114     C                   Z-ADD     TBPRB4        TBCRIC                         *RICAVI NETTI
022700960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
022800960903     C*---
022900960903     C* AGGIUNTA
023000960903     C*---
023100960903IF  1C     *IN99         IFEQ      *ON
023200960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
023300960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
023400960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
023500960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
023600020114     C                   WRITE     TEBDC000                             97
023700960903IF  2C     *IN97         IFEQ      *OFF
023800960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
023900960903E   2C                   ENDIF
024000960903X   1C                   ELSE
024100960903     C*---
024200960903     C* AGGIORNAMENTO
024300960903     C*---
024400960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
024500020114     C                   UPDATE    TEBDC000                             97
024600960903IF  3C     *IN97         IFEQ      *OFF
024700960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
024800960903E   3C                   ENDIF
024900960903E   2C                   ENDIF
025000960903E   1C                   ENDIF
025100960903     C*--------------------
025200960903     C* 5°  M E S E
025300960903     C*--------------------
025400960903     C                   Z-ADD     05            KBDMES                         *MESE
025500020114     C     KEYBDC        CHAIN     TEBDC01L                           99
025600960903     C*
025700960903     C* VALORIZZA I CAMPI DEL BUDGET AS
025800960903     C                   Z-ADD     TBPSP5        TBCNSP                         *SPEDIZIONI
025900020114     C**         TBPRB5    MULT 1000      TBCRIC           *RICAVI NETTI
026000020114     C                   Z-ADD     TBPRB5        TBCRIC                         *RICAVI NETTI
026100960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
026200960903     C*---
026300960903     C* AGGIUNTA
026400960903     C*---
026500960903IF  1C     *IN99         IFEQ      *ON
026600960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
026700960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
026800960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
026900960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
027000020114     C                   WRITE     TEBDC000                             97
027100960903IF  2C     *IN97         IFEQ      *OFF
027200960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
027300960903E   2C                   ENDIF
027400960903X   1C                   ELSE
027500960903     C*---
027600960903     C* AGGIORNAMENTO
027700960903     C*---
027800960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
027900020114     C                   UPDATE    TEBDC000                             97
028000960903IF  3C     *IN97         IFEQ      *OFF
028100960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
028200960903E   3C                   ENDIF
028300960903E   2C                   ENDIF
028400960903E   1C                   ENDIF
028500960903     C*--------------------
028600960903     C* 6°  M E S E
028700960903     C*--------------------
028800960903     C                   Z-ADD     06            KBDMES                         *MESE
028900020114     C     KEYBDC        CHAIN     TEBDC01L                           99
029000960903     C*
029100960903     C* VALORIZZA I CAMPI DEL BUDGET AS
029200960903     C                   Z-ADD     TBPSP6        TBCNSP                         *SPEDIZIONI
029300020114     C**         TBPRB6    MULT 1000      TBCRIC           *RICAVI NETTI
029400020114     C                   Z-ADD     TBPRB6        TBCRIC                         *RICAVI NETTI
029500960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
029600960903     C*---
029700960903     C* AGGIUNTA
029800960903     C*---
029900960903IF  1C     *IN99         IFEQ      *ON
030000960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
030100960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
030200960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
030300960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
030400020114     C                   WRITE     TEBDC000                             97
030500960903IF  2C     *IN97         IFEQ      *OFF
030600960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
030700960903E   2C                   ENDIF
030800960903X   1C                   ELSE
030900960903     C*---
031000960903     C* AGGIORNAMENTO
031100960903     C*---
031200960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
031300020114     C                   UPDATE    TEBDC000                             97
031400960903IF  3C     *IN97         IFEQ      *OFF
031500960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
031600960903E   3C                   ENDIF
031700960903E   2C                   ENDIF
031800960903E   1C                   ENDIF
031900960903     C*--------------------
032000960903     C* 7°  M E S E
032100960903     C*--------------------
032200960903     C                   Z-ADD     07            KBDMES                         *MESE
032300020114     C     KEYBDC        CHAIN     TEBDC01L                           99
032400960903     C*
032500960903     C* VALORIZZA I CAMPI DEL BUDGET AS
032600960903     C                   Z-ADD     TBPSP7        TBCNSP                         *SPEDIZIONI
032700020114     C**         TBPRB7    MULT 1000      TBCRIC           *RICAVI NETTI
032800020114     C                   Z-ADD     TBPRB7        TBCRIC                         *RICAVI NETTI
032900960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
033000960903     C*---
033100960903     C* AGGIUNTA
033200960903     C*---
033300960903IF  1C     *IN99         IFEQ      *ON
033400960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
033500960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
033600960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
033700960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
033800020114     C                   WRITE     TEBDC000                             97
033900960903IF  2C     *IN97         IFEQ      *OFF
034000960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
034100960903E   2C                   ENDIF
034200960903X   1C                   ELSE
034300960903     C*---
034400960903     C* AGGIORNAMENTO
034500960903     C*---
034600960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
034700020114     C                   UPDATE    TEBDC000                             97
034800960903IF  3C     *IN97         IFEQ      *OFF
034900960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
035000960903E   3C                   ENDIF
035100960903E   2C                   ENDIF
035200960903E   1C                   ENDIF
035300960903     C*--------------------
035400960903     C* 8°  M E S E
035500960903     C*--------------------
035600960903     C                   Z-ADD     08            KBDMES                         *MESE
035700020114     C     KEYBDC        CHAIN     TEBDC01L                           99
035800960903     C*
035900960903     C* VALORIZZA I CAMPI DEL BUDGET AS
036000960903     C                   Z-ADD     TBPSP8        TBCNSP                         *SPEDIZIONI
036100020114     C**         TBPRB8    MULT 1000      TBCRIC           *RICAVI NETTI
036200020114     C                   Z-ADD     TBPRB8        TBCRIC                         *RICAVI NETTI
036300960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
036400960903     C*---
036500960903     C* AGGIUNTA
036600960903     C*---
036700960903IF  1C     *IN99         IFEQ      *ON
036800960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
036900960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
037000960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
037100960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
037200020114     C                   WRITE     TEBDC000                             97
037300960903IF  2C     *IN97         IFEQ      *OFF
037400960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
037500960903E   2C                   ENDIF
037600960903X   1C                   ELSE
037700960903     C*---
037800960903     C* AGGIORNAMENTO
037900960903     C*---
038000960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
038100020114     C                   UPDATE    TEBDC000                             97
038200960903IF  3C     *IN97         IFEQ      *OFF
038300960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
038400960903E   3C                   ENDIF
038500960903E   2C                   ENDIF
038600960903E   1C                   ENDIF
038700960903     C*--------------------
038800960903     C* 9°  M E S E
038900960903     C*--------------------
039000960903     C                   Z-ADD     09            KBDMES                         *MESE
039100020114     C     KEYBDC        CHAIN     TEBDC01L                           99
039200960903     C*
039300960903     C* VALORIZZA I CAMPI DEL BUDGET AS
039400960903     C                   Z-ADD     TBPSP9        TBCNSP                         *SPEDIZIONI
039500020114     C**         TBPRB9    MULT 1000      TBCRIC           *RICAVI NETTI
039600020114     C                   Z-ADD     TBPRB9        TBCRIC                         *RICAVI NETTI
039700960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
039800960903     C*---
039900960903     C* AGGIUNTA
040000960903     C*---
040100960903IF  1C     *IN99         IFEQ      *ON
040200960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
040300960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
040400960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
040500960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
040600020114     C                   WRITE     TEBDC000                             97
040700960903IF  2C     *IN97         IFEQ      *OFF
040800960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
040900960903E   2C                   ENDIF
041000960903X   1C                   ELSE
041100960903     C*---
041200960903     C* AGGIORNAMENTO
041300960903     C*---
041400960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
041500020114     C                   UPDATE    TEBDC000                             97
041600960903IF  3C     *IN97         IFEQ      *OFF
041700960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
041800960903E   3C                   ENDIF
041900960903E   2C                   ENDIF
042000960903E   1C                   ENDIF
042100960903     C*--------------------
042200960903     C* 10° M E S E
042300960903     C*--------------------
042400960903     C                   Z-ADD     10            KBDMES                         *MESE
042500020114     C     KEYBDC        CHAIN     TEBDC01L                           99
042600960903     C*
042700960903     C* VALORIZZA I CAMPI DEL BUDGET AS
042800960903     C                   Z-ADD     TBPSP0        TBCNSP                         *SPEDIZIONI
042900020114     C**         TBPRB0    MULT 1000      TBCRIC           *RICAVI NETTI
043000020114     C                   Z-ADD     TBPRB0        TBCRIC                         *RICAVI NETTI
043100960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
043200960903     C*---
043300960903     C* AGGIUNTA
043400960903     C*---
043500960903IF  1C     *IN99         IFEQ      *ON
043600960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
043700960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
043800960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
043900960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
044000020114     C                   WRITE     TEBDC000                             97
044100960903IF  2C     *IN97         IFEQ      *OFF
044200960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
044300960903E   2C                   ENDIF
044400960903X   1C                   ELSE
044500960903     C*---
044600960903     C* AGGIORNAMENTO
044700960903     C*---
044800960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
044900020114     C                   UPDATE    TEBDC000                             97
045000960903IF  3C     *IN97         IFEQ      *OFF
045100960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
045200960903E   3C                   ENDIF
045300960903E   2C                   ENDIF
045400960903E   1C                   ENDIF
045500960903     C*--------------------
045600960903     C* 11° M E S E
045700960903     C*--------------------
045800960903     C                   Z-ADD     11            KBDMES                         *MESE
045900020114     C     KEYBDC        CHAIN     TEBDC01L                           99
046000960903     C*
046100960903     C* VALORIZZA I CAMPI DEL BUDGET AS
046200960903     C                   Z-ADD     TBPSPA        TBCNSP                         *SPEDIZIONI
046300020114     C**         TBPRBA    MULT 1000      TBCRIC           *RICAVI NETTI
046400020114     C                   Z-ADD     TBPRBA        TBCRIC                         *RICAVI NETTI
046500960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
046600960903     C*---
046700960903     C* AGGIUNTA
046800960903     C*---
046900960903IF  1C     *IN99         IFEQ      *ON
047000960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
047100960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
047200960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
047300960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
047400020114     C                   WRITE     TEBDC000                             97
047500960903IF  2C     *IN97         IFEQ      *OFF
047600960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
047700960903E   2C                   ENDIF
047800960903X   1C                   ELSE
047900960903     C*---
048000960903     C* AGGIORNAMENTO
048100960903     C*---
048200960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
048300020114     C                   UPDATE    TEBDC000                             97
048400960903IF  3C     *IN97         IFEQ      *OFF
048500960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
048600960903E   3C                   ENDIF
048700960903E   2C                   ENDIF
048800960903E   1C                   ENDIF
048900960903     C*--------------------
049000960903     C* 12° M E S E
049100960903     C*--------------------
049200960903     C                   Z-ADD     12            KBDMES                         *MESE
049300020114     C     KEYBDC        CHAIN     TEBDC01L                           99
049400960903     C*
049500960903     C* VALORIZZA I CAMPI DEL BUDGET AS
049600960903     C                   Z-ADD     TBPSPB        TBCNSP                         *SPEDIZIONI
049700020114     C*+         TBPRBB    MULT 1000      TBCRIC           *RICAVI NETTI
049800020114     C                   Z-ADD     TBPRBB        TBCRIC                         *RICAVI NETTI
049900960903     C                   MOVEL     *BLANKS       TBCATB                         *RECORD VALIDO
050000960903     C*---
050100960903     C* AGGIUNTA
050200960903     C*---
050300960903IF  1C     *IN99         IFEQ      *ON
050400960903     C                   Z-ADD     KBDAGE        TBCAGE                         *AGENTE
050500960903     C                   Z-ADD     KBDANN        TBCANN                         *ANNO
050600960903     C                   Z-ADD     KBDMES        TBCMES                         *MESE
050700960903     C                   MOVEL     KBDIOE        TBCIOE                         *ITA/EST
050800020114     C                   WRITE     TEBDC000                             97
050900960903IF  2C     *IN97         IFEQ      *OFF
051000960903     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
051100960903E   2C                   ENDIF
051200960903X   1C                   ELSE
051300960903     C*---
051400960903     C* AGGIORNAMENTO
051500960903     C*---
051600960903IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
051700020114     C                   UPDATE    TEBDC000                             97
051800960903IF  3C     *IN97         IFEQ      *OFF
051900960903     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
052000960903E   3C                   ENDIF
052100960903E   2C                   ENDIF
052200960903E   1C                   ENDIF
052300960903     C*
052400960903     C                   ENDSR
052500960903     C*------------------------------------------------------------------------*
052600960903     C* STAPAR - STAMPA PAGINA PARAMETRI
052700960903     C*------------------------------------------------------------------------*
052800960903     C     STAPAR        BEGSR
052900960903     C*
053000960903     C* FINE PAGINA, STAMPA TESTATA
053100960903IF  1C     *INOF         IFEQ      *ON
053200960903     C                   EXCEPT    TES                                          *TESTATA
053300960903     C                   SETOFF                                       OF
053400960903E   1C                   ENDIF
053500960903     C*
053600960903     C* DECODIFICHE PARAMETRI
053700960903     C                   Z-ADD     PARANN        PACANN            4 0          *ANNO DA CARICARE
053800960903IF  1C     PARAGG        IFEQ      'S'
053900960903     C                   MOVEL     'SI'          PADAGG            2            *AGGIORNMENTO 'SI'
054000960903     C                   ELSE
054100960903     C                   MOVEL     'NO'          PADAGG                         *AGGIORNMENTO 'NO'
054200960903E   1C                   ENDIF
054300960903     C* STAMPA PARAMETRI
054400960903     C                   EXCEPT    PAR
054500960903     C*
054600960903     C                   ENDSR
054700020916     C*--------------------------------------------------------------------------------------------*
054800020916     C* REPERISCE I DATI UTENTE
054900020916     C*--------------------------------------------------------------------------------------------*
055000020916     C     REPDATIUTE    BEGSR
055100020916     C*
055200020916     C* INIZIALIZZA VARIABILI DI WRK
055300020916     C                   CLEAR                   TIBS34DS
055400020916     C                   CLEAR                   AZUTEDS
055500020916     C                   CLEAR                   DDATIUTE
055600020916     C*
055700020916     C     *DTAARA       DEFINE    §azute        azuteds
055800020916     C     *DTAARA       DEFINE    §datiute      ddatiute
055900020916     C                   IN(E)     *DTAARA
056000020916if  1C                   IF        %Error
056100020916     c                   EVAL      I34Tla = 'L'
056200020916     C                   CALL      'TIBS34R'
056300020916     C                   PARM                    Tibs34Ds
056400020916     C                   IN        *DTAARA
056500020916e   1C                   ENDIF
056600020916     C*
056700020916     C                   ENDSR
056800960903     C*------------------------------------------------------------------------*
056900960903     C* *INZSR - OPERAZIONI INIZIALI
057000960903     C*------------------------------------------------------------------------*
057100960903     C     *INZSR        BEGSR
057200960903     C*--------------------
057300960903     C* RICEVIMENTO PARAMETRI
057400960903     C*--------------------
057500960903     C     *ENTRY        PLIST
057600960903     C                   PARM                    KPJBA
057700960903     C                   MOVEL     KPJBU         PARAM
057800960903     C*--------------------
057900960903     C* VARIABILI RIFERITE AL DATABASE
058000960903     C*--------------------
058100020114     C     *LIKE         DEFINE    TBCAGE        KBDAGE                         *TEBDC00F
058200960903     C     *LIKE         DEFINE    TBCANN        KBDANN
058300960903     C     *LIKE         DEFINE    TBCMES        KBDMES
058400960903     C     *LIKE         DEFINE    TBCIOE        KBDIOE
058500960903     C*--------------------
058600960903     C* CHIAVI DI LETTURA
058700960903     C*--------------------
058800020114     C* LETTURA TEBDC01L - COMPLETA
058900960903     C     KEYBDC        KLIST
059000960903     C                   KFLD                    KBDAGE                         *AGENTE
059100960903     C                   KFLD                    KBDANN                         *ANNO
059200960903     C                   KFLD                    KBDMES                         *MESE
059300960903     C                   KFLD                    KBDIOE                         *ITA/EST
059400960903     C*--------------------
059500960903     C* VARIABILI DI CONTROLLO
059600960903     C*--------------------
059700960903     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
059800960903     C                   MOVEL     '0'           $FINE             1            *FINE PGM
059900960903     C                   MOVEL     '1'           *INOF                          *OVERFLOW
060000960903     C                   Z-ADD     *ZEROS        RECLET            6 0          *N° RECORD LETTI
060100960903     C                   Z-ADD     *ZEROS        RECAGG            6 0          *N° RECORD AGGIORNATI
060200960903     C                   Z-ADD     *ZEROS        RECSCR            6 0          *N° RECORD SCRITTI
060300960903     C*
060400960903     C* REPERIMENTO DATI UTENTE
060500020916     C                   EXSR      REPDATIUTE
060600960903     C*
060700960903     C                   ENDSR
060800960903     O*------------------------------------------------------------------------*
060900960903     O* STAMPA DI CONTROLLO
061000960903     O*------------------------------------------------------------------------*
061100960903     OQSYSPRT   E            TES              02
061200020114     O                                            8 'TISEC9R '
061300020916     O                       RSUT                40
061400960903     O                                           60 '***  IMPORTAZIONE BUDGET'
061500960903     O                                           84 'COMMERCIALE DA PC  ***'
061600960903     O                                           92 'Data:'
061700960903     O                       UDATE              101 '  /  /  '
061800960903     O                                          127 'Pag.'
061900960903     O                       PAGE          Z    132
062000960903     O*------------------------------------------------------------------------*
062100960903     O          E            PAR              04
062200960903     O                       DPAR(1)             78
062300960903     O*------------------------
062400960903     O          E            PAR              06
062500960903     O                       DPAR(2)             78
062600960903     O                       PACANN             115
062700960903     O*------------------------
062800960903     O          E            PAR              07
062900960903     O                       DPAR(3)             78
063000960903     O                       PADAGG             115
063100960903     O*------------------------------------------------------------------------*
063200960903     O          E            RIE              04
063300960903     O                       RIG(2)              66
063400960903     O                       RIG(2)             132
063500960903     O*------------------------
063600960903     O          E            RIE              05
063700960903     O                       RIG(5)              99
063800960903     O*------------------------
063900960903     O          E            RIE              06
064000960903     O                       RIG(2)              66
064100960903     O                       RIG(2)             132
064200960903     O*------------------------
064300960903     O          E            RIE              07
064400960903     O                       RIG(6)              66
064500960903     O*------------------------
064600960903     O          E            RIE              08
064700960903     O                       RECLET        1     10
064800960903     O                       RECSCR        1     27
064900960903     O                       RECAGG        1     46
065000960903     O*------------------------------------------------------------------------*
065100960903     O          E            FIN              62
065200960903     O                                           60 ' * * *    F I N E    P R'
065300960903     O                                           84 ' O S P E T T O    * * * '
065400960903     O*------------------------------------------------------------------------*
065500960903** RIG
0656009609031
065700960903------------------------------------------------------------------2
0658009609033
0659009609034
066000960903  * * *   R I E P I L O G O    E L A B O R A Z I O N E   * * *    5
066100960903 N° REC LETTI   N° REC AGGIUNTI   N° REC AGGIORNATI               6
066200960903   999.999          999.999            999.999                    7
066300960903** PAR
066400960903***  PARAMETRI LANCIO ELABORAZIONE  ***              1
066500960903ANNO DA CARICARE ......................:             2
066600960903AGGIORNAMENTO SE DATI GIA' PRESENTI ...:             3
