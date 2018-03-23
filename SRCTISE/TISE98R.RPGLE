000100000000     H*------------------------------------------------------------------------*
000200960822     H* IMPORTAZIONE BUDGET DI FILIALE DA PC - ESEGUE                          *
000300000000     H*------------------------------------------------------------------------*
000400000000     H* INPUT  : -------- - ----------------------------------------------------
000500020115     H*          PARAM    - PARAMETRI PASSATI DAL FILTRO 'TISE97R'
000600020325     H*                     PARANN  -  ANNO DA CARICARE
000700000000     H*                     PARAGG  -  AGGIORNAMENTO     (S/N)
000800000000     H* OUTPUT : -------- - ----------------------------------------------------
000900000000     H*          QSYSPRT  - STAMPA RECORD NON VALIDI/LETTI/AGGIORNATI/AGGIUNTI
001000000000     H*------------------------------------------------------------------------*
001100000000     H* INDICATORI USATI                                                       *
001200000000     H*------------------------------------------------------------------------*
001300000000     H* 21/27 - RICERCA CAPICONTO CONTABILITA'
001400000000     H* 97    - OPERAZIONE WRITE O UPDAT NON COMPLETATA CON SUCCESSO
001500000000     H* 98    - LOKUP
001600000000     H* 99    - DI COMODO
001700000000     F*------------------------------------------------------------------------*
001800000000     F* DATA BASE                                                              *
001900000000     F*------------------------------------------------------------------------*
002000000000     H DECEDIT('0,') DATEDIT(*DMY.)
002100000000     FTABEL00F  IF   E           K DISK
002200020115     FTEBPC00F  IF   E           K DISK
002300020115     FTEBDG01L  UF A E           K DISK
002400000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002500000000     F                                     FORMLEN(66)
002600000000     F                                     FORMOFL(55)
002700000000     L*------------------------------------------------------------------------*
002800000000     L* CONTINUAZIONE
002900000000     L*------------------------------------------------------------------------*
003000000000     D*------------------------------------------------------------------------*
003100000000     D* SCHIERE                                                                *
003200000000     D*------------------------------------------------------------------------*
003300000000     D*------------------
003400000000     D* RICHE DI STAMPA
003500000000     D*------------------
003600000000     D STA             S             66    DIM(7) CTDATA PERRCD(1)
003700000000     D*------------------
003800000000     D* DECODIFICA PARAMETRI FILTRO
003900000000     D*------------------
004000000000     D PAR             S             40    DIM(3) CTDATA PERRCD(1)
004100000000     D*------------------
004200000000     D* FASCE DI PESO
004300000000     D*------------------
004400000000     D FPE             S              2    DIM(12)
004500000000     D*------------------------------------------------------------------------*
004600000000     D* INPUT                                                                  *
004700000000     D*------------------------------------------------------------------------*
004800000000     D*------------------
004900000000     D* ARCHITETTURA
005000000000     D*------------------
005100000000     D KPJBA         E DS
005200000000     D  PROFIL                 1     10                                         UTENTE
005300000000     D*------------------
005400020115     D* PARAMETRI FILTRO -TISE97R-
005500000000     D*------------------
005600000000     D PARAM           DS
005700960108     D  PARANN                 1      4  0                                      ANNO
005800960108     D  PARAGG                 5      5                                         AGGIOR
005900000000     D*------------------
006000020916     D* DS REPERIMENTO DATI UTENTE
006100020916     D*-------------------
006200020916     D TIBS34DS      E DS                                                       *Profili utente
006300020916     D DDATIUTE      E DS                                                       *Dati utente
006400020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006500000000     C*------------------------------------------------------------------------*
006600000000     C*?MAIN LINE
006700000000     C*------------------------------------------------------------------------*
006800000000     C*
006900000000     C* STAMPA PARAMETRI FILTRO
007000000000     C                   EXSR      STAPAR
007100000000     C*
007200000000     C* POSIZIONAMENTO SUL FILE BUDGET PC E PRIMA LETTURA
007300000000     C                   EXSR      LETFIL
007400000000     C*
007500000000     C* CICLO FINO A FINE FILE
007600000000DO  1C     $FINE         DOWEQ     '0'
007700000000     C*
007800000000     C* SCRIVE FILE BUDGET AS
007900000000     C                   EXSR      SCRIVE
008000000000     C*
008100000000     C* LEGGE IL RECORD SUCCESSIVO
008200000000     C                   EXSR      LETFIL
008300000000     C*
008400000000E   1C                   ENDDO
008500000000     C*
008600000000     C* STAMPA RIEPILOGATIVA RECORD LETTI/SCRITTI/DOPPI
008700000000     C                   EXCEPT    TESTA                                        *TESTATA
008800000000     C                   EXCEPT    RIEP                                         *RIEPILOGO
008900000000     C*
009000000000     C* STAMPA RIGA DI FINE PROSPETTO
009100000000     C                   EXCEPT    FINE
009200000000     C*
009300000000     C                   SETON                                        LR
009400000000     C*------------------------------------------------------------------------*
009500000000     C*? LETFIL - LETTURA PROSSIMO RECORD IN INPUT
009600000000     C*------------------------------------------------------------------------*
009700000000     C     LETFIL        BEGSR
009800000000     C*
009900000000     C* LEGGO FINO A:
010000000000     C*     - FINE FILE
010100000000     C*     - TROVATO RECORD VALIDO
010200000000     C                   MOVEL     'N'           WRECOK
010300000000DO  1C     $FINE         DOWEQ     '0'
010400000000     C     WRECOK        ANDEQ     'N'
010500000000     C*
010600020115     C                   READ      TEBPC00F                               99
010700000000     C*
010800000000IF  2C     *IN99         IFEQ      '1'
010900000000     C                   MOVEL     '1'           $FINE                          *FINE ARCHIVIO
011000000000X   2C                   ELSE
011100000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
011200000000IF  3C     WRECOK        IFEQ      'N'
011300000000     C                   EXSR      STAERR                                       STAMPA ERRORE IN ELAB
011400000000X   3C                   ELSE
011500000000     C                   ADD       1             RECLET
011600000000E   3C                   ENDIF
011700000000E   2C                   ENDIF
011800000000     C*
011900000000E   1C                   ENDDO
012000000000     C*
012100000000     C                   ENDSR
012200000000     C*------------------------------------------------------------------------*
012300000000     C*?CHKREC - CONTROLLO VALIDITA' RECORD BUDGET PC
012400000000     C*------------------------------------------------------------------------*
012500000000     C     CHKREC        BEGSR
012600000000     C*
012700000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
012800000000     C*
012900000000     C* FASCIA INESISTENTE, ESCLUDO
013000000000     C     TBPFPE        LOOKUP    FPE(1)                                 98
013100000000IF  1C     *IN98         IFEQ      *OFF
013200000000     C                   MOVEL     'N'           WRECOK                         *FASCIA INESISTENTE
013300000000E   1C                   ENDIF
013400000000     C*
013500000000     C                   ENDSR
013600000000     C*------------------------------------------------------------------------*
013700000000     C*? SCRIVE - SCRIVE SUL FILE BUDGET AS/400
013800000000     C*------------------------------------------------------------------------*
013900000000     C     SCRIVE        BEGSR
014000000000     C*
014100000000     C* PER OGNI RECORD LETTO DAL BUDGET PC DEVO CREARE 12 RECORD DEL BUDGET AS.
014200000000     C* CONTROLLO PERCIO', MESE PER MESE, L' ESISTENZA DEL RECORD SUL BUDGET AS:
014300000000     C* SE LO TROVO VENGO GUIDATA DA FLAG DI AGGIORNAM, SE NON LO TROVO AGGIUNGO
014400000000     C*
014500000000     C* TRASFORMO I QUINTALI DEL BUDGET PC IN KILOGRAMMI PER IL BUDGET AS (X100)
014600000000     C* MOLTIPLICO I RICAVI DEL BUDGET PC PER 1000 PER IL BUDGET AS
014700000000     C*
014800000000     C* IMPOSTO LA CHIAVE
014900000000     C                   Z-ADD     TBPFIL        KBDFIL                         *FILIALE
015000960108     C                   Z-ADD     PARANN        KBDANN                         *ANNO RICHIESTO
015100960115     C                   MOVEL     TBPIOE        KBDIOE                         *ITA/IMP/EXP
015200000000     C                   MOVEL     TBPFPE        KBDFPE                         *FASCIA DI PESO
015300000000     C*--------------------
015400000000     C* 1°  M E S E
015500000000     C*--------------------
015600000000     C                   Z-ADD     01            KBDMES                         *MESE
015700020115     C     KEYBDG        CHAIN     TEBDG01L                           99
015800000000     C*
015900000000     C* VALORIZZO I CAMPI DEL BUDGET AS
016000000000     C                   Z-ADD     TBPSP1        TBGNSP                         *SPEDIZIONI PARTENZA
016100000000     C                   Z-ADD     TBPSA1        TBGNSA                         *SPEDIZIONI ARRIVO
016200000000     C     TBPPP1        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
016300000000     C     TBPPA1        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
016400000000     C                   Z-ADD     TBPCP1        TBGNCP                         *COLLI PARTENZA
016500000000     C                   Z-ADD     TBPCA1        TBGNCA                         *COLLI ARRIVO
016600000000     C                   Z-ADD     TBPVP1        TBGVOP                         *VOLUME PARTENZA
016700000000     C                   Z-ADD     TBPVA1        TBGVOA                         *VOLUME ARRIVO
016800000000     C     TBPRB1        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
016900000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
017000000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
017100000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
017200000000     C*
017300000000IF  1C     *IN99         IFEQ      *ON
017400000000     C*--------------------
017500000000     C* AGGIUNTA
017600000000     C*--------------------
017700000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
017800000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
017900000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
018000960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
018100000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
018200020115     C                   WRITE     TEBDG                                97
018300000000IF  2C     *IN97         IFEQ      *OFF
018400000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
018500000000E   2C                   ENDIF
018600000000X   1C                   ELSE
018700000000     C*--------------------
018800000000     C* AGGIORNAMENTO
018900000000     C*--------------------
019000000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
019100020115     C                   UPDATE    TEBDG                                97
019200000000IF  3C     *IN97         IFEQ      *OFF
019300000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
019400000000E   3C                   ENDIF
019500000000E   2C                   ENDIF
019600000000E   1C                   ENDIF
019700000000     C*--------------------
019800000000     C* 2°  M E S E
019900000000     C*--------------------
020000000000     C                   Z-ADD     02            KBDMES                         *MESE
020100020115     C     KEYBDG        CHAIN     TEBDG01L                           99
020200000000     C*
020300000000     C* VALORIZZO I CAMPI DEL BUDGET AS
020400000000     C                   Z-ADD     TBPSP2        TBGNSP                         *SPEDIZIONI PARTENZA
020500000000     C                   Z-ADD     TBPSA2        TBGNSA                         *SPEDIZIONI ARRIVO
020600000000     C     TBPPP2        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
020700000000     C     TBPPA2        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
020800000000     C                   Z-ADD     TBPCP2        TBGNCP                         *COLLI PARTENZA
020900000000     C                   Z-ADD     TBPCA2        TBGNCA                         *COLLI ARRIVO
021000000000     C                   Z-ADD     TBPVP2        TBGVOP                         *VOLUME PARTENZA
021100000000     C                   Z-ADD     TBPVA2        TBGVOA                         *VOLUME ARRIVO
021200000000     C     TBPRB2        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
021300000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
021400000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
021500000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
021600000000     C*
021700000000IF  1C     *IN99         IFEQ      *ON
021800000000     C*--------------------
021900000000     C* AGGIUNTA
022000000000     C*--------------------
022100000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
022200000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
022300000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
022400960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
022500000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
022600020115     C                   WRITE     TEBDG                                97
022700000000IF  2C     *IN97         IFEQ      *OFF
022800000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
022900000000E   2C                   ENDIF
023000000000X   1C                   ELSE
023100000000     C*--------------------
023200000000     C* AGGIORNAMENTO
023300000000     C*--------------------
023400000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
023500020115     C                   UPDATE    TEBDG                                97
023600000000IF  3C     *IN97         IFEQ      *OFF
023700000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
023800000000E   3C                   ENDIF
023900000000E   2C                   ENDIF
024000000000E   1C                   ENDIF
024100000000     C*
024200000000     C*--------------------
024300000000     C* 3°  M E S E
024400000000     C*--------------------
024500000000     C                   Z-ADD     03            KBDMES                         *MESE
024600020115     C     KEYBDG        CHAIN     TEBDG01L                           99
024700000000     C*
024800000000     C* VALORIZZO I CAMPI DEL BUDGET AS
024900000000     C                   Z-ADD     TBPSP3        TBGNSP                         *SPEDIZIONI PARTENZA
025000000000     C                   Z-ADD     TBPSA3        TBGNSA                         *SPEDIZIONI ARRIVO
025100000000     C     TBPPP3        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
025200000000     C     TBPPA3        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
025300000000     C                   Z-ADD     TBPCP3        TBGNCP                         *COLLI PARTENZA
025400000000     C                   Z-ADD     TBPCA3        TBGNCA                         *COLLI ARRIVO
025500000000     C                   Z-ADD     TBPVP3        TBGVOP                         *VOLUME PARTENZA
025600000000     C                   Z-ADD     TBPVA3        TBGVOA                         *VOLUME ARRIVO
025700000000     C     TBPRB3        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
025800000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
025900000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
026000000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
026100000000     C*
026200000000IF  1C     *IN99         IFEQ      *ON
026300000000     C*--------------------
026400000000     C* AGGIUNTA
026500000000     C*--------------------
026600000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
026700000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
026800000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
026900960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
027000000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
027100020115     C                   WRITE     TEBDG                                97
027200000000IF  2C     *IN97         IFEQ      *OFF
027300000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
027400000000E   2C                   ENDIF
027500000000X   1C                   ELSE
027600000000     C*--------------------
027700000000     C* AGGIORNAMENTO
027800000000     C*--------------------
027900000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
028000020115     C                   UPDATE    TEBDG                                97
028100000000IF  3C     *IN97         IFEQ      *OFF
028200000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
028300000000E   3C                   ENDIF
028400000000E   2C                   ENDIF
028500000000E   1C                   ENDIF
028600000000     C*
028700000000     C*--------------------
028800000000     C* 4°  M E S E
028900000000     C*--------------------
029000000000     C                   Z-ADD     04            KBDMES                         *MESE
029100020115     C     KEYBDG        CHAIN     TEBDG01L                           99
029200000000     C*
029300000000     C* VALORIZZO I CAMPI DEL BUDGET AS
029400000000     C                   Z-ADD     TBPSP4        TBGNSP                         *SPEDIZIONI PARTENZA
029500000000     C                   Z-ADD     TBPSA4        TBGNSA                         *SPEDIZIONI ARRIVO
029600000000     C     TBPPP4        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
029700000000     C     TBPPA4        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
029800000000     C                   Z-ADD     TBPCP4        TBGNCP                         *COLLI PARTENZA
029900000000     C                   Z-ADD     TBPCA4        TBGNCA                         *COLLI ARRIVO
030000000000     C                   Z-ADD     TBPVP4        TBGVOP                         *VOLUME PARTENZA
030100000000     C                   Z-ADD     TBPVA4        TBGVOA                         *VOLUME ARRIVO
030200000000     C     TBPRB4        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
030300000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
030400000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
030500000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
030600000000     C*
030700000000IF  1C     *IN99         IFEQ      *ON
030800000000     C*--------------------
030900000000     C* AGGIUNTA
031000000000     C*--------------------
031100000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
031200000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
031300000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
031400960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
031500000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
031600020115     C                   WRITE     TEBDG                                97
031700000000IF  2C     *IN97         IFEQ      *OFF
031800000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
031900000000E   2C                   ENDIF
032000000000X   1C                   ELSE
032100000000     C*--------------------
032200000000     C* AGGIORNAMENTO
032300000000     C*--------------------
032400000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
032500020115     C                   UPDATE    TEBDG                                97
032600000000IF  3C     *IN97         IFEQ      *OFF
032700000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
032800000000E   3C                   ENDIF
032900000000E   2C                   ENDIF
033000000000E   1C                   ENDIF
033100000000     C*
033200000000     C*--------------------
033300000000     C* 5°  M E S E
033400000000     C*--------------------
033500000000     C                   Z-ADD     05            KBDMES                         *MESE
033600020115     C     KEYBDG        CHAIN     TEBDG01L                           99
033700000000     C*
033800000000     C* VALORIZZO I CAMPI DEL BUDGET AS
033900000000     C                   Z-ADD     TBPSP5        TBGNSP                         *SPEDIZIONI PARTENZA
034000000000     C                   Z-ADD     TBPSA5        TBGNSA                         *SPEDIZIONI ARRIVO
034100000000     C     TBPPP5        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
034200000000     C     TBPPA5        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
034300000000     C                   Z-ADD     TBPCP5        TBGNCP                         *COLLI PARTENZA
034400000000     C                   Z-ADD     TBPCA5        TBGNCA                         *COLLI ARRIVO
034500000000     C                   Z-ADD     TBPVP5        TBGVOP                         *VOLUME PARTENZA
034600000000     C                   Z-ADD     TBPVA5        TBGVOA                         *VOLUME ARRIVO
034700000000     C     TBPRB5        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
034800000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
034900000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
035000000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
035100000000     C*
035200000000IF  1C     *IN99         IFEQ      *ON
035300000000     C*--------------------
035400000000     C* AGGIUNTA
035500000000     C*--------------------
035600000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
035700000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
035800000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
035900960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
036000000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
036100020115     C                   WRITE     TEBDG                                97
036200000000IF  2C     *IN97         IFEQ      *OFF
036300000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
036400000000E   2C                   ENDIF
036500000000X   1C                   ELSE
036600000000     C*--------------------
036700000000     C* AGGIORNAMENTO
036800000000     C*--------------------
036900000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
037000020115     C                   UPDATE    TEBDG                                97
037100000000IF  3C     *IN97         IFEQ      *OFF
037200000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
037300000000E   3C                   ENDIF
037400000000E   2C                   ENDIF
037500000000E   1C                   ENDIF
037600000000     C*
037700000000     C*--------------------
037800000000     C* 6°  M E S E
037900000000     C*--------------------
038000000000     C                   Z-ADD     06            KBDMES                         *MESE
038100020115     C     KEYBDG        CHAIN     TEBDG01L                           99
038200000000     C*
038300000000     C* VALORIZZO I CAMPI DEL BUDGET AS
038400000000     C                   Z-ADD     TBPSP6        TBGNSP                         *SPEDIZIONI PARTENZA
038500000000     C                   Z-ADD     TBPSA6        TBGNSA                         *SPEDIZIONI ARRIVO
038600000000     C     TBPPP6        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
038700000000     C     TBPPA6        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
038800000000     C                   Z-ADD     TBPCP6        TBGNCP                         *COLLI PARTENZA
038900000000     C                   Z-ADD     TBPCA6        TBGNCA                         *COLLI ARRIVO
039000000000     C                   Z-ADD     TBPVP6        TBGVOP                         *VOLUME PARTENZA
039100000000     C                   Z-ADD     TBPVA6        TBGVOA                         *VOLUME ARRIVO
039200000000     C     TBPRB6        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
039300000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
039400000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
039500000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
039600000000     C*
039700000000IF  1C     *IN99         IFEQ      *ON
039800000000     C*--------------------
039900000000     C* AGGIUNTA
040000000000     C*--------------------
040100000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
040200000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
040300000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
040400960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
040500000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
040600020115     C                   WRITE     TEBDG                                97
040700000000IF  2C     *IN97         IFEQ      *OFF
040800000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
040900000000E   2C                   ENDIF
041000000000X   1C                   ELSE
041100000000     C*--------------------
041200000000     C* AGGIORNAMENTO
041300000000     C*--------------------
041400000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
041500020115     C                   UPDATE    TEBDG                                97
041600000000IF  3C     *IN97         IFEQ      *OFF
041700000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
041800000000E   3C                   ENDIF
041900000000E   2C                   ENDIF
042000000000E   1C                   ENDIF
042100000000     C*
042200000000     C*--------------------
042300000000     C* 7°  M E S E
042400000000     C*--------------------
042500000000     C                   Z-ADD     07            KBDMES                         *MESE
042600020115     C     KEYBDG        CHAIN     TEBDG01L                           99
042700000000     C*
042800000000     C* VALORIZZO I CAMPI DEL BUDGET AS
042900000000     C                   Z-ADD     TBPSP7        TBGNSP                         *SPEDIZIONI PARTENZA
043000000000     C                   Z-ADD     TBPSA7        TBGNSA                         *SPEDIZIONI ARRIVO
043100000000     C     TBPPP7        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
043200000000     C     TBPPA7        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
043300000000     C                   Z-ADD     TBPCP7        TBGNCP                         *COLLI PARTENZA
043400000000     C                   Z-ADD     TBPCA7        TBGNCA                         *COLLI ARRIVO
043500000000     C                   Z-ADD     TBPVP7        TBGVOP                         *VOLUME PARTENZA
043600000000     C                   Z-ADD     TBPVA7        TBGVOA                         *VOLUME ARRIVO
043700000000     C     TBPRB7        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
043800000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
043900000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
044000000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
044100000000     C*
044200000000IF  1C     *IN99         IFEQ      *ON
044300000000     C*--------------------
044400000000     C* AGGIUNTA
044500000000     C*--------------------
044600000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
044700000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
044800000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
044900960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
045000000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
045100020115     C                   WRITE     TEBDG                                97
045200000000IF  2C     *IN97         IFEQ      *OFF
045300000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
045400000000E   2C                   ENDIF
045500000000X   1C                   ELSE
045600000000     C*--------------------
045700000000     C* AGGIORNAMENTO
045800000000     C*--------------------
045900000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
046000020115     C                   UPDATE    TEBDG                                97
046100000000IF  3C     *IN97         IFEQ      *OFF
046200000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
046300000000E   3C                   ENDIF
046400000000E   2C                   ENDIF
046500000000E   1C                   ENDIF
046600000000     C*
046700000000     C*--------------------
046800000000     C* 8°  M E S E
046900000000     C*--------------------
047000000000     C                   Z-ADD     08            KBDMES                         *MESE
047100020115     C     KEYBDG        CHAIN     TEBDG01L                           99
047200000000     C*
047300000000     C* VALORIZZO I CAMPI DEL BUDGET AS
047400000000     C                   Z-ADD     TBPSP8        TBGNSP                         *SPEDIZIONI PARTENZA
047500000000     C                   Z-ADD     TBPSA8        TBGNSA                         *SPEDIZIONI ARRIVO
047600000000     C     TBPPP8        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
047700000000     C     TBPPA8        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
047800000000     C                   Z-ADD     TBPCP8        TBGNCP                         *COLLI PARTENZA
047900000000     C                   Z-ADD     TBPCA8        TBGNCA                         *COLLI ARRIVO
048000000000     C                   Z-ADD     TBPVP8        TBGVOP                         *VOLUME PARTENZA
048100000000     C                   Z-ADD     TBPVA8        TBGVOA                         *VOLUME ARRIVO
048200000000     C     TBPRB8        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
048300000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
048400000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
048500000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
048600000000     C*
048700000000IF  1C     *IN99         IFEQ      *ON
048800000000     C*--------------------
048900000000     C* AGGIUNTA
049000000000     C*--------------------
049100000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
049200000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
049300000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
049400960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
049500000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
049600020115     C                   WRITE     TEBDG                                97
049700000000IF  2C     *IN97         IFEQ      *OFF
049800000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
049900000000E   2C                   ENDIF
050000000000X   1C                   ELSE
050100000000     C*--------------------
050200000000     C* AGGIORNAMENTO
050300000000     C*--------------------
050400000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
050500020115     C                   UPDATE    TEBDG                                97
050600000000IF  3C     *IN97         IFEQ      *OFF
050700000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
050800000000E   3C                   ENDIF
050900000000E   2C                   ENDIF
051000000000E   1C                   ENDIF
051100000000     C*
051200000000     C*--------------------
051300000000     C* 9°  M E S E
051400000000     C*--------------------
051500000000     C                   Z-ADD     09            KBDMES                         *MESE
051600020115     C     KEYBDG        CHAIN     TEBDG01L                           99
051700000000     C*
051800000000     C* VALORIZZO I CAMPI DEL BUDGET AS
051900000000     C                   Z-ADD     TBPSP9        TBGNSP                         *SPEDIZIONI PARTENZA
052000000000     C                   Z-ADD     TBPSA9        TBGNSA                         *SPEDIZIONI ARRIVO
052100000000     C     TBPPP9        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
052200000000     C     TBPPA9        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
052300000000     C                   Z-ADD     TBPCP9        TBGNCP                         *COLLI PARTENZA
052400000000     C                   Z-ADD     TBPCA9        TBGNCA                         *COLLI ARRIVO
052500000000     C                   Z-ADD     TBPVP9        TBGVOP                         *VOLUME PARTENZA
052600000000     C                   Z-ADD     TBPVA9        TBGVOA                         *VOLUME ARRIVO
052700000000     C     TBPRB9        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
052800000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
052900000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
053000000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
053100000000     C*
053200000000IF  1C     *IN99         IFEQ      *ON
053300000000     C*--------------------
053400000000     C* AGGIUNTA
053500000000     C*--------------------
053600000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
053700000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
053800000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
053900960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
054000000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
054100020115     C                   WRITE     TEBDG                                97
054200000000IF  2C     *IN97         IFEQ      *OFF
054300000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
054400000000E   2C                   ENDIF
054500000000X   1C                   ELSE
054600000000     C*--------------------
054700000000     C* AGGIORNAMENTO
054800000000     C*--------------------
054900000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
055000020115     C                   UPDATE    TEBDG                                97
055100000000IF  3C     *IN97         IFEQ      *OFF
055200000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
055300000000E   3C                   ENDIF
055400000000E   2C                   ENDIF
055500000000E   1C                   ENDIF
055600000000     C*
055700000000     C*--------------------
055800000000     C* 10° M E S E
055900000000     C*--------------------
056000000000     C                   Z-ADD     10            KBDMES                         *MESE
056100020115     C     KEYBDG        CHAIN     TEBDG01L                           99
056200000000     C*
056300000000     C* VALORIZZO I CAMPI DEL BUDGET AS
056400000000     C                   Z-ADD     TBPSP0        TBGNSP                         *SPEDIZIONI PARTENZA
056500000000     C                   Z-ADD     TBPSA0        TBGNSA                         *SPEDIZIONI ARRIVO
056600000000     C     TBPPP0        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
056700000000     C     TBPPA0        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
056800000000     C                   Z-ADD     TBPCP0        TBGNCP                         *COLLI PARTENZA
056900000000     C                   Z-ADD     TBPCA0        TBGNCA                         *COLLI ARRIVO
057000000000     C                   Z-ADD     TBPVP0        TBGVOP                         *VOLUME PARTENZA
057100000000     C                   Z-ADD     TBPVA0        TBGVOA                         *VOLUME ARRIVO
057200000000     C     TBPRB0        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
057300000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
057400000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
057500000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
057600000000     C*
057700000000IF  1C     *IN99         IFEQ      *ON
057800000000     C*--------------------
057900000000     C* AGGIUNTA
058000000000     C*--------------------
058100000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
058200000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
058300000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
058400960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
058500000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
058600020115     C                   WRITE     TEBDG                                97
058700000000IF  2C     *IN97         IFEQ      *OFF
058800000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
058900000000E   2C                   ENDIF
059000000000X   1C                   ELSE
059100000000     C*--------------------
059200000000     C* AGGIORNAMENTO
059300000000     C*--------------------
059400000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
059500020115     C                   UPDATE    TEBDG                                97
059600000000IF  3C     *IN97         IFEQ      *OFF
059700000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
059800000000E   3C                   ENDIF
059900000000E   2C                   ENDIF
060000000000E   1C                   ENDIF
060100000000     C*
060200000000     C*--------------------
060300000000     C* 11° M E S E
060400000000     C*--------------------
060500000000     C                   Z-ADD     11            KBDMES                         *MESE
060600020115     C     KEYBDG        CHAIN     TEBDG01L                           99
060700000000     C*
060800000000     C* VALORIZZO I CAMPI DEL BUDGET AS
060900000000     C                   Z-ADD     TBPSPA        TBGNSP                         *SPEDIZIONI PARTENZA
061000000000     C                   Z-ADD     TBPSAA        TBGNSA                         *SPEDIZIONI ARRIVO
061100000000     C     TBPPPA        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
061200000000     C     TBPPAA        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
061300000000     C                   Z-ADD     TBPCPA        TBGNCP                         *COLLI PARTENZA
061400000000     C                   Z-ADD     TBPCAA        TBGNCA                         *COLLI ARRIVO
061500000000     C                   Z-ADD     TBPVPA        TBGVOP                         *VOLUME PARTENZA
061600000000     C                   Z-ADD     TBPVAA        TBGVOA                         *VOLUME ARRIVO
061700000000     C     TBPRBA        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
061800000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
061900000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
062000000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
062100000000     C*
062200000000IF  1C     *IN99         IFEQ      *ON
062300000000     C*--------------------
062400000000     C* AGGIUNTA
062500000000     C*--------------------
062600000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
062700000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
062800000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
062900960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
063000000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
063100020115     C                   WRITE     TEBDG                                97
063200000000IF  2C     *IN97         IFEQ      *OFF
063300000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
063400000000E   2C                   ENDIF
063500000000X   1C                   ELSE
063600000000     C*--------------------
063700000000     C* AGGIORNAMENTO
063800000000     C*--------------------
063900000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
064000020115     C                   UPDATE    TEBDG                                97
064100000000IF  3C     *IN97         IFEQ      *OFF
064200000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
064300000000E   3C                   ENDIF
064400000000E   2C                   ENDIF
064500000000E   1C                   ENDIF
064600000000     C*
064700000000     C*--------------------
064800000000     C* 12° M E S E
064900000000     C*--------------------
065000000000     C                   Z-ADD     12            KBDMES                         *MESE
065100020115     C     KEYBDG        CHAIN     TEBDG01L                           99
065200000000     C*
065300000000     C* VALORIZZO I CAMPI DEL BUDGET AS
065400000000     C                   Z-ADD     TBPSPB        TBGNSP                         *SPEDIZIONI PARTENZA
065500000000     C                   Z-ADD     TBPSAB        TBGNSA                         *SPEDIZIONI ARRIVO
065600000000     C     TBPPPB        MULT      100           TBGKGP                         *KILOGRAMMI PARTENZA
065700000000     C     TBPPAB        MULT      100           TBGKGA                         *KILOGRAMMI ARRIVO
065800000000     C                   Z-ADD     TBPCPB        TBGNCP                         *COLLI PARTENZA
065900000000     C                   Z-ADD     TBPCAB        TBGNCA                         *COLLI ARRIVO
066000000000     C                   Z-ADD     TBPVPB        TBGVOP                         *VOLUME PARTENZA
066100000000     C                   Z-ADD     TBPVAB        TBGVOA                         *VOLUME ARRIVO
066200000000     C     TBPRBB        MULT      1000          TBGIRB                         *RICAVI DA BOLLE
066300000000     C                   Z-ADD     *ZEROS        TBGINC                         *NOTE DI CREDITO
066400000000     C                   Z-ADD     *ZEROS        TBGIRR                         *RETTIFICHE AI RICAVI
066500000000     C                   MOVEL     *BLANKS       TBGATB                         *RECORD VALIDO
066600000000     C*
066700000000IF  1C     *IN99         IFEQ      *ON
066800000000     C*--------------------
066900000000     C* AGGIUNTA
067000000000     C*--------------------
067100000000     C                   Z-ADD     KBDFIL        TBGFIL                         *FILIALE
067200000000     C                   Z-ADD     KBDANN        TBGANN                         *ANNO
067300000000     C                   Z-ADD     KBDMES        TBGMES                         *MESE
067400960115     C                   MOVEL     KBDIOE        TBGIOE                         *ITA/IMP/EXP
067500000000     C                   MOVEL     KBDFPE        TBGFPE                         *FASCIA DI PESO
067600020115     C                   WRITE     TEBDG                                97
067700000000IF  2C     *IN97         IFEQ      *OFF
067800000000     C                   ADD       1             RECSCR                         CONTATORE REC SCRITTI
067900000000E   2C                   ENDIF
068000000000X   1C                   ELSE
068100000000     C*--------------------
068200000000     C* AGGIORNAMENTO
068300000000     C*--------------------
068400000000IF  2C     PARAGG        IFEQ      'S'                                          *CHIESTO AGGIORNAM.TO
068500020115     C                   UPDATE    TEBDG                                97
068600000000IF  3C     *IN97         IFEQ      *OFF
068700000000     C                   ADD       1             RECAGG                         CONTATORE REC AGGIORN
068800000000E   3C                   ENDIF
068900000000E   2C                   ENDIF
069000000000E   1C                   ENDIF
069100000000     C*
069200000000     C                   ENDSR
069300000000     C*------------------------------------------------------------------------*
069400000000     C*? STAERR - STAMPA ERRORE IN ELABORAZIONE
069500000000     C*------------------------------------------------------------------------*
069600000000     C     STAERR        BEGSR
069700000000     C*
069800000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
069900000000IF  1C     *INOF         IFEQ      '1'
070000000000     C                   EXCEPT    TESTA                                        *TESTATA
070100000000     C                   EXCEPT    INTES                                        *INTESTAZIONE
070200000000     C                   SETOFF                                       OF
070300000000E   1C                   ENDIF
070400000000     C*
070500000000     C* STAMPA RIGA DI ERRORE
070600000000     C                   EXCEPT    ERROR
070700000000     C*
070800000000     C                   ENDSR
070900000000     C*------------------------------------------------------------------------*
071000000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
071100000000     C*------------------------------------------------------------------------*
071200000000     C     STAPAR        BEGSR
071300000000     C*
071400000000     C* RECUPERO LA DATA E L'ORA
071500000000     C                   Z-ADD     UDATE         DATA              6 0
071600000000     C                   TIME                    ORA               6 0
071700000000     C*
071800000000     C                   EXCEPT    TESTA
071900000000     C*
072000000000     C* DECODIFICHE PARAMETRI FILTRO
072100960108     C                   Z-ADD     PARANN        PACANN            4 0          *ANNO DA CARICARE
072200000000IF  1C     PARAGG        IFEQ      'S'
072300000000     C                   MOVEL     'SI'          PADAGG            2            *AGGIORNMENTO 'SI'
072400000000     C                   ELSE
072500000000     C                   MOVEL     'NO'          PADAGG                         *AGGIORNMENTO 'NO'
072600000000E   1C                   ENDIF
072700000000     C*
072800000000     C* STAMPA PARAMETRI
072900000000     C                   EXCEPT    PARM
073000000000     C*
073100000000     C                   ENDSR
073200000000     C*------------------------------------------------------------------------*
073300000000     C*?CARTAB - CARICA LE FASCE DI PESO
073400000000     C*           INDICI J(4,0)
073500000000     C*------------------------------------------------------------------------*
073600000000     C     CARTAB        BEGSR
073700990217     C*--------------------
073800990217     C* FASCE DI PESO
073900990217     C*--------------------
074000990217IF  1C     PARANN        IFLE      1998                                         *FASCE FINO AL '98
074100990217     C                   MOVEL     'SDR'         WSX               3
074200990217X   1C                   ELSE                                                   *FASCE DAL '99
074300020325     C                   MOVEL     'S9R'         WSX               3
074400020325E   1C                   ENDIF
074500000000     C                   Z-ADD     1             J                 4 0
074600000000     C                   Z-ADD     CODUT         KTBKUT
074700000000     C                   MOVEL     '2L'          KTBCOD
074800000000     C     KEYTA2        SETLL     TABEL00F
074900000000     C     KEYTA2        READE     TABEL00F                               99
075000000000DO  1C     *IN99         DOWEQ     *OFF
075100000000     C                   MOVEL     TBLKEY        ALF5              5
075200000000     C                   MOVEL     TBLKEY        FILEST            3            *FILE STATISTICA
075300990217IF  2C     FILEST        IFEQ      WSX
075400000000     C                   MOVE      ALF5          FPE(J)
075500000000     C                   ADD       1             J
075600000000E   2C                   ENDIF
075700000000     C     KEYTA2        READE     TABEL00F                               99
075800000000E   1C                   ENDDO
075900000000     C*
076000000000     C                   ENDSR
076100020916     C*--------------------------------------------------------------------------------------------*
076200020916     C* REPERISCE I DATI UTENTE
076300020916     C*--------------------------------------------------------------------------------------------*
076400020916     C     REPDATIUTE    BEGSR
076500020916     C*
076600020916     C* INIZIALIZZA VARIABILI DI WRK
076700020916     C                   CLEAR                   TIBS34DS
076800020916     C                   CLEAR                   AZUTEDS
076900020916     C                   CLEAR                   DDATIUTE
077000020916     C*
077100020916     C     *DTAARA       DEFINE    §azute        azuteds
077200020916     C     *DTAARA       DEFINE    §datiute      ddatiute
077300020916     C                   IN(E)     *DTAARA
077400020916if  1C                   IF        %Error
077500020916     c                   EVAL      I34Tla = 'L'
077600020916     C                   CALL      'TIBS34R'
077700020916     C                   PARM                    Tibs34Ds
077800020916     C                   IN        *DTAARA
077900020916e   1C                   ENDIF
078000020916     C*
078100020916     C                   ENDSR
078200000000     C*------------------------------------------------------------------------*
078300000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
078400000000     C*------------------------------------------------------------------------*
078500000000     C     *INZSR        BEGSR
078600000000     C*
078700000000     C* PARAMETRI IN INPUT AL PGM
078800000000     C     *ENTRY        PLIST
078900000000     C                   PARM                    KPJBA
079000000000     C                   MOVEL     KPJBU         PARAM
079100000000     C*
079200000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
079300000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
079400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
079500020115     C     *LIKE         DEFINE    TBGFIL        KBDFIL                         *TEBDG00F
079600000000     C     *LIKE         DEFINE    TBGANN        KBDANN
079700000000     C     *LIKE         DEFINE    TBGMES        KBDMES
079800960108     C     *LIKE         DEFINE    TBGIOE        KBDIOE
079900000000     C     *LIKE         DEFINE    TBGFPE        KBDFPE
080000000000     C*
080100020115     C* CHIAVE LETTURA TEBDG01L - COMPLETA
080200000000     C     KEYBDG        KLIST
080300000000     C                   KFLD                    KBDFIL                         *FILIALE
080400000000     C                   KFLD                    KBDANN                         *ANNO
080500000000     C                   KFLD                    KBDMES                         *MESE
080600960115     C                   KFLD                    KBDIOE                         *NAZ/IMP/EXP
080700000000     C                   KFLD                    KBDFPE                         *FASCIA DI PESO
080800000000     C*
080900000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
081000000000     C     KEYTA2        KLIST
081100000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
081200000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
081300000000     C*
081400000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
081500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
081600000000     C*
081700000000     C* AZZERO I CONTATORI
081800000000     C                   Z-ADD     *ZEROS        RECLET            6 0
081900000000     C                   Z-ADD     *ZEROS        RECAGG            6 0
082000000000     C                   Z-ADD     *ZEROS        RECSCR            6 0
082100000000     C*
082200000000     C* ACCENDO INDICATORE DI FINE PAGINA
082300000000     C                   SETON                                        OF
082400000000     C*
082500000000     C* RECUPERO LA RAGIONE SOCIALE DELL'UTENTE
082600020916     C                   EXSR      REPDATIUTE
082700020916     C                   Z-ADD     1             CODUT             1 0
082800000000     C*
082900000000     C* CARICA LE FASCE DI PESO
083000000000     C                   EXSR      CARTAB
083100000000     C*
083200000000     C                   ENDSR
083300000000     O*------------------------------------------------------------------------*
083400000000     O* STAMPA DEI RECORD LETTI/AGGIORNATI/AGGIUNTI
083500000000     O*------------------------------------------------------------------------*
083600000000     OQSYSPRT   E            TESTA            02
083700020115     O                                            8 'TISE98R '
083800000000     O                       RSUT                40
083900960822     O                                           60 '*** IMPORTAZIONE BUDGET '
084000960822     O                                           84 'DI FILIALE DA PC ***'
084100000000     O                                           92 'Data:'
084200000000     O                       DATA               101 '  /  /  '
084300000000     O                                          110 'Ora:'
084400000000     O                       ORA                119 '  :  :  '
084500000000     O                                          128 'Pagina:'
084600000000     O                       PAGE          Z    132
084700000000     O*------------------------------------------------------------------------*
084800000000     O          E            PARM             04
084900000000     O                       PAR(1)              78
085000000000     O*------------------------
085100000000     O          E            PARM             06
085200000000     O                       PAR(2)              78
085300960108     O                       PACANN             115
085400000000     O*------------------------
085500000000     O          E            PARM             07
085600000000     O                       PAR(3)              78
085700960108     O                       PADAGG             115
085800000000     O*------------------------------------------------------------------------*
085900000000     O          E            INTES            04
086000000000     O                       STA(1)              99
086100000000     O*------------------------
086200000000     O          E            INTES            05
086300000000     O                       STA(2)              66
086400000000     O                       STA(2)             132
086500000000     O*------------------------
086600000000     O          E            INTES            06
086700000000     O                       STA(3)              66
086800000000     O*------------------------
086900000000     O          E            INTES          1 07
087000000000     O                       STA(2)              66
087100000000     O                       STA(2)             132
087200000000     O*------------------------------------------------------------------------*
087300000000     O          E            ERROR       1
087400000000     O                       TBPFIL               4
087500000000     O                       TBPFPE              16
087600000000     O                                           50 'FASCIA DI PESO INESIST.'
087700960108     O                       TBPIOE              57
087800000000     O*------------------------------------------------------------------------*
087900000000     O          E            RIEP             04
088000000000     O                       STA(2)              66
088100000000     O                       STA(2)             132
088200000000     O*------------------------
088300000000     O          E            RIEP             05
088400000000     O                       STA(5)              99
088500000000     O*------------------------
088600000000     O          E            RIEP             06
088700000000     O                       STA(2)              66
088800000000     O                       STA(2)             132
088900000000     O*------------------------
089000000000     O          E            RIEP             07
089100000000     O                       STA(6)              66
089200000000     O*------------------------
089300000000     O          E            RIEP             08
089400000000     O                       RECLET        1     10
089500000000     O                       RECSCR        1     27
089600000000     O                       RECAGG        1     46
089700000000     O*------------------------------------------------------------------------*
089800000000     O          E            FINE             62
089900000000     O                                           60 ' * * *    F I N E    P R'
090000000000     O                                           84 ' O S P E T T O    * * * '
090100000000     O*------------------------------------------------------------------------*
090200000000** STA
090300000000  ***  RECORD DEL BUDGET PC ERRATI E QUINDI NON ELABORATI  ***    1
090400000000------------------------------------------------------------------2
090500960115FILIALE   FASCIA DI PESO   ***   E R R O R E   ***    I/M/X       3
090600960108 999          XX           FASCIA DI PESO INESIST.      X         4
090700000000  * * *   R I E P I L O G O    E L A B O R A Z I O N E   * * *    5
090800000000 N° REC LETTI   N° REC AGGIUNTI   N° REC AGGIORNATI               6
090900000000   999.999          999.999            999.999                    7
091000000000** PAR
091100000000***  PARAMETRI LANCIO ELABORAZIONE  ***              1
091200960108ANNO DA CARICARE ......................:             2
091300000000AGGIORNAMENTO SE DATI GIA' PRESENTI ...:             3
