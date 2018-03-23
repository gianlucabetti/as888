000100020716     H DECEDIT('0,') DATEDIT(*DMY.)
000200960521     H*------------------------------------------------------------------------*
000300960521     H* FORZATURA SCADENZA TRAINI - ESEGUE                                     *
000400960521     H* NOTE PER COMPILARE IL PROGRAMMA:                                       *
000500960731     H* - ESEGUIRE LE OVRDBF DEL PROGRAMMA TNTL54C                             *
000600960521     H* - COMPILARE IL PROGRAMMA IN INTERATTIVO                                *
000700960521     H*------------------------------------------------------------------------*
000800960521     F*------------------------------------------------------------------------*
000900960521     F* DATA BASE                                                              *
001000960521     F*------------------------------------------------------------------------*
001100960521     FTNTL54P   O    E             PRINTER OFLIND(*IN01)
001200960521     FTNTLT01L  UF   E           K DISK
001300960521     FTNTLT11L  IF   E           K DISK
001400960521     F                                     RENAME(TNTLT000:TNTLTD)
001500960521     D*------------------------------------------------------------------------*
001600960521     D* SCHIERE                                                                *
001700960521     D*------------------------------------------------------------------------*
001800960521     D*-------------------
001900960521     D* MESSAGGI DI ERRORE
002000960521     D*-------------------
002100960521     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
002200960521     D*---------------------
002300960521     D* ARCHITETTURA
002400960521     D*---------------------
002500960521     D KPJBA         E DS
002600960521     D  PROFIL                 1     10                                         UTENTE
002700960521     D*---------------------
002800960521     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
002900960521     D*---------------------
003000960521     D DSMSG         E DS
003100020716
003200020716     d DdatiUte      e ds
003300020716     d AzuteDs       e ds                  ExtName(Azute00f)
003400020716     d Tibs34Ds      e ds                  Inz
003500020716
003600960521     D*---------------------
003700960521     D* DS PARAMETRI DI LANCIO
003800960521     D*---------------------
003900960521     D PARAM           DS
004000960521     D  PARTR1                 1      7  0
004100960521     D  PARTR2                 8     14  0
004200960521     D  PARDDE                15     22  0
004300960521     D  PARDSC                23     30  0
004400960521     D  PARDS2                31     38  0
004500960521     D  PARSIM                39     39
004600980605     D  PARTE1                40     46  0
004700980605     D  PARTE2                47     53  0
004800980605     D  PARTE3                54     60  0
004900980605     D  PARTE4                61     67  0
005000980605     D  PARTE5                68     74  0
005100980605     D  PARTE6                75     81  0
005200980605     D  PARTE7                82     88  0
005300980605     D  PARTE8                89     95  0
005400980605     D  PARTE9                96    102  0
005500980605     D  PARTE0               103    109  0
005600980605     D  PARTEA               110    116  0
005700980605     D  PARTEB               117    123  0
005800960521     D*---------------------
005900960521     D* DS CONTROLLO DATA (8)
006000960521     D*------------------
006100960521     D WLBDA8          DS                  INZ
006200960521     D  G08DAT                 1      8  0
006300960521     D  G08INV                 9     16  0
006400960521     D  G08ERR                17     17
006500960521     D  G08TGI                18     22  0
006600960521     D*------------------
006700960521     D* DEFINIZIONE COSTANTI
006800960521     D*------------------
006900960521     D DESSIM          C                   CONST('*** SIMULAZIONE ***')
007000960521     I*------------------------------------------------------------------------*
007100960521     I* INPUT                                                                  *
007200960521     I*------------------------------------------------------------------------*
007300960521     I*-------------
007400960521     I* RIDENOMINAZIONE CAMPI SECONDO FILE TRAINI
007500960521     I*-------------
007600960521     ITNTLTD
007700960521     I              TLTTRN                      DLTTRN
007800960521     I              TLTDDE                      DLTDDE
007900960521     I              TLTDSC                      DLTDSC
008000960521     I              TLTTFP                      DLTTFP
008100960521     I              TLTTFA                      DLTTFA
008200960521     I              TLTTTR                      DLTTTR
008300960521     I              TLTGSE                      DLTGSE
008400960521     I              TLTBIS                      DLTBIS
008500960521     I              TLTTMZ                      DLTTMZ
008600960521     I              TLTFES                      DLTFES
008700960521     I              TLTDIR                      DLTDIR
008800960521     I              TLTDUV                      DLTDUV
008900960521     I              TLTFTR                      DLTFTR
009000960521     I              TLTDTR                      DLTDTR
009100960521     I              TLTATB                      DLTATB
009200960521     C*------------------------------------------------------------------------*
009300960521     C* MAIN LINE
009400960521     C*------------------------------------------------------------------------*
009500960521     C*
009600960521     C* STAMPA PARAMETRI DI LANCIO
009700960521     C                   EXSR      STAPAR
009800960521     C*
009900960521     C* POSIZIONAMENTO E PRIMA LETTURA TRAINI
010000960521     C                   EXSR      SETFIL
010100960521     C*
010200960521     C* FINO A FINE TRAINI
010300960521DO  1C     $FINE         DOWEQ     '0'
010400960521     C*
010500960521     C* MODIFICA I TRAINI E STAMPA L'ELENCO
010600960521     C                   EXSR      MODSTM
010700960521     C*
010800960521     C* LETTURA SUCESSIVA TRAINI
010900960521     C                   EXSR      LETFIL
011000960521E   1C                   ENDDO
011100960521     C*
011200960521     C* STAMPA RIGA DI FINE PROSPETTO
011300960521     C                   WRITE     TL54FI
011400960521     C*
011500960521     C                   SETON                                        LR
011600960521     C*------------------------------------------------------------------------*
011700960521     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA TRAINI
011800960521     C*------------------------------------------------------------------------*
011900960521     C     SETFIL        BEGSR
012000960521     C*
012100960521     C* POSIZIONAMENTO
012200960521     C     PARTR1        SETLL     TNTLT01L                           99        *FINE FILE
012300960521     C* LETTURA
012400960521IF  1C     *IN99         IFEQ      *OFF
012500960521     C                   EXSR      LETFIL
012600960521X   1C                   ELSE
012700960521     C                   MOVEL     '1'           $FINE
012800960521E   1C                   ENDIF
012900960521     C*
013000960521     C                   ENDSR
013100960521     C*------------------------------------------------------------------------*
013200960521     C* LETFIL - LETTURA SUCESSIVA TRAINI
013300960521     C*------------------------------------------------------------------------*
013400960521     C     LETFIL        BEGSR
013500960521     C*
013600960521     C                   MOVEL     'N'           WRECOK
013700960521     C*
013800960521     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
013900960521DO  1C     $FINE         DOWEQ     '0'
014000960521     C     WRECOK        ANDEQ     'N'
014100960521     C                   READ      TNTLT01L                               99
014200960521     C*
014300960521IF  2C     *IN99         IFEQ      *ON
014400960521     C                   MOVEL     '1'           $FINE
014500960521X   2C                   ELSE
014600960521     C                   EXSR      CHKREC                                       *CONTROLLA RECROD
014700960521E   2C                   ENDIF
014800960521E   1C                   ENDDO
014900960521     C*
015000960521     C                   ENDSR
015100960521     C*------------------------------------------------------------------------*
015200960521     C* CHKREC - CONTROLLA VALIDITA' TRAINI
015300960521     C*------------------------------------------------------------------------*
015400960521     C     CHKREC        BEGSR
015500960521     C*
015600960521     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
015700960521     C*
015800960521     C* TRAINO > DELL'ULTIMO RICHIESTO, ESCE DAL CICLO
015900960521IF  1C     TLTTRN        IFGT      PARTR2                                       ULTIMO TRAINO CHIEST
016000960521     C                   MOVEL     '1'           $FINE                          *FINE LETTURA
016100960521E   1C                   ENDIF
016200960521     C*
016300960521     C* TRAINO ANNULLATO, ESCLUDE
016400960521IF  1C     TLTATB        IFNE      *BLANKS
016500960521     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
016600960521E   1C                   ENDIF
016700960521     C*
016800960521     C* TRAINO CON NUMERO > 9.000.000, ESCLUDE
016900960521IF  1C     TLTTRN        IFGT      9000000
017000960521     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
017100960521E   1C                   ENDIF
017200960521     C*
017300960521     C* TRAINO <> DA UNO DI QUELLI RICHIESTI, ESCLUDE
017400960521IF  1C     TLTTRN        IFGE      PARTR1                                       PRIMO  TRAINO CHIEST
017500960521     C     TLTTRN        ANDLE     PARTR2                                       ULTIMO TRAINO CHIEST
017600960521X   1C                   ELSE
017700960521     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
017800960521E   1C                   ENDIF
017900980605     C*
018000980605     C* TRAINO RICHIESTO DA ESCLUDERE, ESCLUDE
018100980605IF  1C     TLTTRN        IFEQ      PARTE1
018200980605     C     TLTTRN        OREQ      PARTE2
018300980605     C     TLTTRN        OREQ      PARTE3
018400980605     C     TLTTRN        OREQ      PARTE4
018500980605     C     TLTTRN        OREQ      PARTE5
018600980605     C     TLTTRN        OREQ      PARTE6
018700980605     C     TLTTRN        OREQ      PARTE7
018800980605     C     TLTTRN        OREQ      PARTE8
018900980605     C     TLTTRN        OREQ      PARTE9
019000980605     C     TLTTRN        OREQ      PARTE0
019100980605     C     TLTTRN        OREQ      PARTEA
019200980605     C     TLTTRN        OREQ      PARTEB
019300980605     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
019400980605E   1C                   ENDIF
019500960521     C*
019600960521     C* DECORRENZA <> DA QUELLA RICHIESTA, ESCLUDE
019700960521IF  1C     PARDDE        IFGT      *ZEROS
019800960521IF  2C     TLTDDE        IFNE      PARDDE
019900960521     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
020000960521E   2C                   ENDIF
020100960521E   1C                   ENDIF
020200960521     C*
020300960521     C* SCADENZA <> DA QUELLA RICHIESTA, ESCLUDE
020400960521IF  1C     PARDSC        IFGT      *ZEROS
020500960521IF  2C     TLTDSC        IFNE      PARDSC
020600960521     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
020700960521E   2C                   ENDIF
020800960521E   1C                   ENDIF
020900960521     C*
021000960521     C                   ENDSR
021100960521     C*------------------------------------------------------------------------*
021200960521     C* MODSTM - MODIFICA I TRAINI E STAMPA L'ELENCO
021300960521     C*------------------------------------------------------------------------*
021400960521     C     MODSTM        BEGSR
021500960521     C*
021600960521     C* CONTROLLA SE IL TRAINO PUO' ESSERE MODIFICATO, NEL CASO AGGIORNA
021700960521     C                   EXSR      CHKTRN
021800960521IF  1C     WTRNOK        IFEQ      'S'                                          *TRAINO VALIDO
021900960521     C                   EXSR      AGGFIL
022000960521E   1C                   ENDIF
022100960521     C*
022200960521     C* STAMPA IL RISULTATO DELL'ELABORAZIONE
022300960521     C                   EXSR      STMELA
022400960521     C*
022500960521     C                   ENDSR
022600960521     C*------------------------------------------------------------------------*
022700960521     C* CHKTRN - CONTROLLA SE IL TRAINO PUO' ESSERE DUPLICATO
022800960521     C*------------------------------------------------------------------------*
022900960521     C     CHKTRN        BEGSR
023000960521     C*
023100960521     C* REIMPOSTA I FLAG DI ERRORE
023200960521     C                   MOVEL     'S'           WTRNOK                         *TRAINO VALIDO
023300960521     C                   MOVEL     '0'           WERR                           *NO ERRORE
023400960523     C*
023500960523     C* LA NUOVA SCADENZA DEVE ESSERE DIVERSA DALLA VECCHIA
023600960523IF  1C     WTRNOK        IFEQ      'S'                                          *ANCORA VALIDO
023700960523IF  2C     PARDS2        IFEQ      TLTDSC
023800960523     C                   MOVEL     'N'           WTRNOK                         *TRAINO NON VALIDO
023900960523     C                   MOVEL     '2'           WERR                           *ERRORE DUE
024000960523E   2C                   ENDIF
024100960523E   1C                   ENDIF
024200960523     C*
024300960523     C* LA DECORRENZA NON PUO' ESSERE MAGGIORE DELLA NUOVA SCADENZA
024400960523IF  1C     WTRNOK        IFEQ      'S'                                          *ANCORA VALIDO
024500000822IF  2C     PARDS2        IFLT      TLTDDE
024600960523     C                   MOVEL     'N'           WTRNOK                         *TRAINO NON VALIDO
024700960523     C                   MOVEL     '3'           WERR                           *ERRORE TRE
024800960523E   2C                   ENDIF
024900960523E   1C                   ENDIF
025000960521     C*
025100960521     C* CONTROLLO SE IL TRAINO DA FAR SCADERE SI SOVRAPPONE AD ALTRI
025200960521IF  1C     WTRNOK        IFEQ      'S'                                          *ANCORA VALIDO
025300960521     C     TLTTRN        SETLL     TNTLT11L
025400960521     C     TLTTRN        READE(N)  TNTLT11L                               98
025500960521DO  2C     *IN98         DOWEQ     *OFF
025600960521IF  3C     DLTATB        IFEQ      *BLANKS                                      *NON ANNULLATI
025700960521IF  4C     DLTDDE        IFEQ      TLTDDE                                       *STESSO TRAINO
025800960521     C     DLTDSC        ANDEQ     TLTDSC                                       *NON CONSIDERA
025900960521X   4C                   ELSE
026000960521IF  5C     DLTDDE        IFLT      TLTDDE                                       *NON SOVRAPPOSTO
026100960521     C     DLTDSC        ANDLT     PARDS2
026200960521     C     DLTDDE        ORGT      PARDS2
026300960521X   5C                   ELSE                                                   *SOVRAPPOSTO
026400960521     C                   MOVEL     'N'           WTRNOK                         *TRAINO NON VALIDO
026500960521     C                   MOVEL     '1'           WERR                           *ERRORE UNO
026600960521E   5C                   ENDIF
026700960521E   4C                   ENDIF
026800960521E   3C                   ENDIF
026900960521     C     TLTTRN        READE(N)  TNTLT11L                               98
027000960521E   2C                   ENDDO
027100960521E   1C                   ENDIF
027200960521     C*
027300960521     C                   ENDSR
027400960521     C*------------------------------------------------------------------------*
027500960521     C* AGGFIL - AGGIORNA IL TRAINO CON LA NUOVA SCADENZA
027600960521     C*------------------------------------------------------------------------*
027700960521     C     AGGFIL        BEGSR
027800960521     C*
027900960731     C                   Z-ADD     TLTDSC        PDEDSC                         *IN STAMPA
028000960731     C*
028100970723     C* AGIORNA SOLO SE RICHIESTO IL LANCIO NON IN SIMULAIZONE
028200970723IF  1C     PARSIM        IFEQ      'N'                                          *NO SIMULAZIONE
028300960521     C                   Z-ADD     PARDS2        TLTDSC                         *NUOVA SCADENZA
028400960521     C                   MOVEL     *BLANKS       TLTATB                         *NON ANNULLATO
028500960521     C                   Z-ADD     DATCOR        TLTDUV                         *DATA VARIAZIONE
028600960521     C                   MOVEL     *BLANKS       TLTFTR                         *TRASMISSIONE
028700960521     C                   EXCEPT    AGGTLT
028800970723E   1C                   ENDIF
028900960521     C*
029000960521     C                   ENDSR
029100960521     C*-----------------------------------------------------------------------*
029200960521     C* STMELA - STAMPA IL RISULTATO DELL'ELABORAZIONE
029300960521     C*------------------------------------------------------------------------*
029400960521     C     STMELA        BEGSR
029500960521     C*
029600960521     C* SE FINE PAGINA STAMPA TESTATA
029700960521IF  1C     *IN01         IFEQ      *ON
029800960521     C                   MOVEL     '0'           *IN01
029900960521     C                   WRITE     TL54TE                                       *TESTATA
030000960521     C                   WRITE     TL54IN                                       *INTESTAZIONE
030100960521E   1C                   ENDIF
030200960521     C* TRAINO MODIFICATO
030300960521     C                   Z-ADD     TLTTRN        PDETRN                         *TRAINO
030400960521     C                   Z-ADD     TLTDDE        G08INV
030500960521     C                   Z-ADD     *ZEROS        G08DAT
030600960521     C                   MOVEL     '3'           G08ERR
030700960521     C                   CALL      'XSRDA8'
030800960521     C                   PARM                    WLBDA8
030900960521     C                   Z-ADD     G08DAT        PDEDDE                         *DATA DECORRENZA
031000960731     C                   Z-ADD     PDEDSC        G08INV
031100960521     C                   Z-ADD     *ZEROS        G08DAT
031200960521     C                   MOVEL     '3'           G08ERR
031300960521     C                   CALL      'XSRDA8'
031400960521     C                   PARM                    WLBDA8
031500960521     C                   Z-ADD     G08DAT        PDEDSC                         *DATA SCADENZA
031600960521     C* NUOVA SCADENZA
031700960521     C                   Z-ADD     PARDS2        G08INV
031800960521     C                   Z-ADD     *ZEROS        G08DAT
031900960521     C                   MOVEL     '3'           G08ERR
032000960521     C                   CALL      'XSRDA8'
032100960521     C                   PARM                    WLBDA8
032200960521     C                   Z-ADD     G08DAT        PDEDS2                         *DATA SCADENZA
032300960521     C* RISULTATO MODIFICA
032400960521IF  1C     WERR          IFEQ      '0'
032500960521     C                   MOVEL     'SI'          PDEMOK                         *OK
032600960521     C                   MOVEL     *BLANKS       PDEANM
032700960521X   1C                   ELSE
032800960521     C                   MOVEL     'NO'          PDEMOK                         *NON OK
032900960521     C                   Z-ADD     *ZEROS        J                 2 0
033000960521     C                   MOVE      WERR          J
033100960521     C                   MOVEL     MSG(J)        PDEANM                         *MESSAGGIO DI ERRORE
033200960521E   1C                   ENDIF
033300960521     C*
033400960521     C                   WRITE     TL54DE                                       *DETTAGLIO
033500960521     C*
033600960521     C                   ENDSR
033700960521     C*------------------------------------------------------------------------*
033800960521     C* STAPAR - STAMPA PARAMETRI DI LANCIO
033900960521     C*------------------------------------------------------------------------*
034000960521     C     STAPAR        BEGSR
034100960521     C*
034200960521IF  1C     PARSIM        IFEQ      'S'                                          *SIMULAZIONE DUPLICA
034300960521     C                   MOVEL     DESSIM        TEDSIM
034400960521     C                   MOVEL     'SI'          PACSIM
034500960521X   1C                   ELSE
034600960521     C                   MOVEL     'NO'          PACSIM
034700960521E   1C                   ENDIF
034800960521     C                   Z-ADD     PARTR1        PACTR1                         *DA N°TRAINO
034900960521     C                   Z-ADD     PARTR2        PACTR2                         *A  N°TRAINO
035000980605     C                   Z-ADD     PARTE1        PACTE1                         *TRANNE TRAINI N°
035100980605     C                   Z-ADD     PARTE2        PACTE2
035200980605     C                   Z-ADD     PARTE3        PACTE3
035300980605     C                   Z-ADD     PARTE4        PACTE4
035400980605     C                   Z-ADD     PARTE5        PACTE5
035500980605     C                   Z-ADD     PARTE6        PACTE6
035600980605     C                   Z-ADD     PARTE7        PACTE7
035700980605     C                   Z-ADD     PARTE8        PACTE8
035800980605     C                   Z-ADD     PARTE9        PACTE9
035900980605     C                   Z-ADD     PARTE0        PACTE0
036000980605     C                   Z-ADD     PARTEA        PACTEA
036100980605     C                   Z-ADD     PARTEB        PACTEB
036200960521     C                   Z-ADD     PARDDE        G08INV
036300960521     C                   Z-ADD     *ZEROS        G08DAT
036400960521     C                   MOVEL     '3'           G08ERR
036500960521     C                   CALL      'XSRDA8'
036600960521     C                   PARM                    WLBDA8
036700960521     C                   Z-ADD     G08DAT        PACDDE                         *DATA DECORRENZA
036800960521     C                   Z-ADD     PARDSC        G08INV
036900960521     C                   Z-ADD     *ZEROS        G08DAT
037000960521     C                   MOVEL     '3'           G08ERR
037100960521     C                   CALL      'XSRDA8'
037200960521     C                   PARM                    WLBDA8
037300960521     C                   Z-ADD     G08DAT        PACDSC                         *DATA SCADENZA
037400960521     C                   Z-ADD     PARDS2        G08INV
037500960521     C                   Z-ADD     *ZEROS        G08DAT
037600960521     C                   MOVEL     '3'           G08ERR
037700960521     C                   CALL      'XSRDA8'
037800960521     C                   PARM                    WLBDA8
037900960521     C                   Z-ADD     G08DAT        PACDS2                         *NUOVA SCADENZA
038000960521     C*
038100960521     C                   WRITE     TL54TE                                       *TESTATA
038200960521     C                   WRITE     TL54PA                                       *PARAMETRI
038300960521     C*
038400960521     C                   ENDSR
038500960521     C*------------------------------------------------------------------------*
038600960521     C* *INZSR - ROUTINE INIZIALE
038700960521     C*------------------------------------------------------------------------*
038800960521     C     *INZSR        BEGSR
038900960521     C*--------------------
039000960521     C* RICEVIMENTO PARAMETRI
039100960521     C*--------------------
039200960521     C     *ENTRY        PLIST
039300960521     C                   PARM                    KPJBA
039400960521     C                   MOVEL     KPJBU         PARAM
039500960521     C*--------------------
039600960521     C* VARIABILI RIFERITE AL DATA BASE
039700960521     C*--------------------
039800960521     C*--------------------
039900960521     C* CHIAVI DI LETTURA
040000960521     C*--------------------
040100960521     C*--------------------
040200960521     C* VARIABILI DI CONTROLLO
040300960521     C*--------------------
040400960521     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA TRAINI
040500960521     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
040600960521     C                   MOVEL     'N'           WTRNOK            1            *VALIDITA' TRAINO
040700960521     C                   MOVEL     *BLANKS       $MSG             78            *MESSAGGI DI ERRORE
040800960521     C                   MOVEL     '1'           *IN01                          *OVERFLOW
040900960521     C                   MOVEL     '0'           WERR              1            *FLAG STAMPA ERRORE
041000960521     C*
041100960521     C* REPERISCE I DATI UTENTE
041200020716
041300020716     c     *dtaara       define    §azute        azuteds
041400020716     c     *dtaara       define    §datiute      ddatiute
041500020716
041600020716     c                   in(E)     *dtaara
041700020716     c                   if        %Error or rsut = *blanks
041800020716     c                   clear                   tibs34ds
041900020716     c                   call      'TIBS34R'
042000020716     c                   parm                    Tibs34Ds
042100020716     c                   in        *dtaara
042200020716     c                   endif
042300960521     C*
042400960521     C* CALCOLA LA DATA CORRENTE
042500960521     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
042600960521     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
042700960521     C                   Z-ADD     WNUM8         G08DAT
042800960521     C                   Z-ADD     *ZEROS        G08INV
042900960521     C                   MOVEL     '0'           G08ERR
043000960521     C                   CALL      'XSRDA8'
043100960521     C                   PARM                    WLBDA8
043200960521     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
043300960521     C*
043400960521     C                   ENDSR
043500960521     O*------------------------------------------------------------------------*
043600960521     O* AGGIORNAMENTO TNTLT00F
043700960521     O*------------------------------------------------------------------------*
043800960521     OTNTLT000  E            AGGTLT
043900960521     O                       TLTDSC
044000960521     O                       TLTDUV
044100960521     O                       TLTFTR
044200960521     O                       TLTATB
044300960521     O*------------------------------------------------------------------------*
044400960521** MSG - MESSAGGI DI ERRORE
044500960521TRAINO NON MODIFICATO: IN SOVRAPPOSIZIONE CON ALTRI               1
044600960523TRAINO NON MODIFICATO: LA SCADENZA E' UGUALE A QUELLA RICHIESTA   2
044700960523TRAINO NON MODIFICATO: NON PUO' SCADERE PRIMA DI DECORRERE        3
0448009605214
0449009605215
0450009605216
0451009605217
0452009605218
0453009605219
04540096052110
