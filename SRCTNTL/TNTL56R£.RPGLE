000100020716     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* AUTOMATISMO DUPLICA TRAINI - ESEGUE
000400000000     H*------------------------------------------------------------------------*
000500000000     H* NOTE PER COMPILARE IL PROGRAMMA:
000600000000     H* - ESEGUIRE LE OVRDBF DEL PROGRAMMA TNTL56C
000700000000     H* - COMPILARE IL PROGRAMMA IN INTERATTIVO
000800110610     h* - quando sovrascrive secondo me ci sono degli errori quindi ho protetto l'opzione
000900110610     h* -  non la ritengo molto utilizzabile. EDPES 10/06/11
001000000000     H*------------------------------------------------------------------------*
001100000000     F*------------------------------------------------------------------------*
001200000000     F* DATA BASE                                                              *
001300000000     F*------------------------------------------------------------------------*
001400000000     FTNTL56P   O    E             PRINTER OFLIND(*IN01)
001500000000     F*------------
001600000000     F* TRAINI DA CUI COPIARE
001700000000     F*------------
001800000000     FTNTLT01L  IF   E           K DISK
001900000000     FTNTLR01L  IF   E           K DISK
002000000000     FTNTLO01L  IF   E           K DISK
002100000000     FTNTLM01L  IF   E           K DISK
002200010822     FTITLL01L  IF   E           K DISK
002300000000     FTNTLN01L  IF   E           K DISK
002400110607     FTNTLS01L  IF   E           K DISK
002500000000     F*------------
002600000000     F* TRAINI SU CUI COPIARE
002700000000     F*------------
002800000000     FTNTLT11L  UF A E           K DISK
002900000000     F                                     RENAME(TNTLT000:TNTLTD)
003000000000     FTNTLR11L  UF A E           K DISK
003100000000     F                                     RENAME(TNTLR000:TNTLRD)
003200000000     FTNTLO11L  UF A E           K DISK
003300000000     F                                     RENAME(TNTLO000:TNTLOD)
003400000000     FTNTLM11L  UF A E           K DISK
003500000000     F                                     RENAME(TNTLM000:TNTLMD)
003600010822     FTITLL11L  UF A E           K DISK
003700010822     F                                     RENAME(TITLL000:TITLLD)
003800000000     FTNTLN11L  UF A E           K DISK
003900000000     F                                     RENAME(TNTLN000:TNTLND)
004000110607     FTNTLS11L  UF A E           K DISK
004100110607     F                                     RENAME(TNTLS000:TNTLSD) prefix(DLS:3)
004200000000     D*------------------------------------------------------------------------*
004300000000     D* SCHIERE                                                                *
004400000000     D*------------------------------------------------------------------------*
004500000000     D*-------------------
004600000000     D* MESSAGGI DI ERRORE
004700000000     D*-------------------
004800000000     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
004900000000     D*---------------------
005000000000     D* ARCHITETTURA
005100000000     D*---------------------
005200000000     D KPJBA         E DS
005300000000     D  PROFIL                 1     10                                         UTENTE
005400000000     D*---------------------
005500000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
005600000000     D*---------------------
005700000000     D DSMSG         E DS
005800020716
005900020716     d DdatiUte      e ds
006000020716     d AzuteDs       e ds                  ExtName(Azute00f)
006100020716     d Tibs34Ds      e ds                  Inz
006200020716
006300000000     D*---------------------
006400000000     D* DS PARAMETRI DI LANCIO
006500000000     D*---------------------
006600000000     D PARAM           DS
006700000000     D  PARTR1                 1      7  0
006800000000     D  PARTR2                 8     14  0
006900000000     D  PARDDE                15     22  0
007000000000     D  PARDSC                23     30  0
007100000000     D  PARNUM                31     37  0
007200000000     D  PARDD2                38     45  0
007300000000     D  PARDS2                46     53  0
007400000000     D  PARESI                54     54
007500000000     D  PARSIM                55     55
007600000000     D  PARMAN                56     56
007700980605     D  PARTE1                57     63  0
007800980605     D  PARTE2                64     70  0
007900980605     D  PARTE3                71     77  0
008000980605     D  PARTE4                78     84  0
008100980605     D  PARTE5                85     91  0
008200980605     D  PARTE6                92     98  0
008300980605     D  PARTE7                99    105  0
008400980605     D  PARTE8               106    112  0
008500980605     D  PARTE9               113    119  0
008600980605     D  PARTE0               120    126  0
008700980605     D  PARTEA               127    133  0
008800980605     D  PARTEB               134    140  0
008900000000     D*---------------------
009000000000     D* DS CONTROLLO DATA (8)
009100000000     D*------------------
009200000000     D WLBDA8          DS                  INZ
009300000000     D  G08DAT                 1      8  0
009400000000     D  G08INV                 9     16  0
009500000000     D  G08ERR                17     17
009600000000     D  G08TGI                18     22  0
009700000000     D*------------------
009800000000     D* DEFINIZIONE COSTANTI
009900000000     D*------------------
010000000000     D DESSIM          C                   CONST('*** SIMULAZIONE ***')
010100000000     I*------------------------------------------------------------------------*
010200000000     I* INPUT                                                                  *
010300000000     I*------------------------------------------------------------------------*
010400000000     I*-------------
010500000000     I* RIDENOMINAZIONE CAMPI FILES TRAINI SU CUI COPIARE
010600000000     I*-------------
010700000000     ITNTLTD
010800000000     I              TLTTRN                      DLTTRN
010900000000     I              TLTDDE                      DLTDDE
011000000000     I              TLTDSC                      DLTDSC
011100000000     I              TLTTFP                      DLTTFP
011200000000     I              TLTTFA                      DLTTFA
011300000000     I              TLTTTR                      DLTTTR
011400000000     I              TLTGSE                      DLTGSE
011500000000     I              TLTBIS                      DLTBIS
011600000000     I              TLTTMZ                      DLTTMZ
011700000000     I              TLTFES                      DLTFES
011800000000     I              TLTDIR                      DLTDIR
011900000000     I              TLTDUV                      DLTDUV
012000000000     I              TLTFTR                      DLTFTR
012100000000     I              TLTDTR                      DLTDTR
012200000000     I              TLTATB                      DLTATB
012300000000     ITNTLRD
012400000000     I              TLRTRN                      DLRTRN
012500000000     I              TLRDDE                      DLRDDE
012600000000     I              TLRNFM                      DLRNFM
012700000000     I              TLRTFM                      DLRTFM
012800000000     I              TLRFIL                      DLRFIL
012900000000     I              TLRSCA                      DLRSCA
013000000000     I              TLRCAR                      DLRCAR
013100000000     I              TLRKMT                      DLRKMT
013200000000     I              TLRTEM                      DLRTEM
013300000000     I              TLRGFV                      DLRGFV
013400000000     I              TLRDIR                      DLRDIR
013500000000     I              TLRDUV                      DLRDUV
013600000000     I              TLRFTR                      DLRFTR
013700000000     I              TLRDTR                      DLRDTR
013800000000     I              TLRATB                      DLRATB
013900000000     ITNTLOD
014000000000     I              TLOTRN                      DLOTRN
014100000000     I              TLODDE                      DLODDE
014200000000     I              TLONFM                      DLONFM
014300000000     I              TLOGSE                      DLOGSE
014400000000     I              TLOGGA                      DLOGGA
014500000000     I              TLOHHA                      DLOHHA
014600000000     I              TLOGGP                      DLOGGP
014700000000     I              TLOHHP                      DLOHHP
014800000000     I              TLODIR                      DLODIR
014900000000     I              TLODUV                      DLODUV
015000000000     I              TLOFTR                      DLOFTR
015100000000     I              TLODTR                      DLODTR
015200000000     I              TLOATB                      DLOATB
015300000000     ITNTLMD
015400000000     I              TLMTRN                      DLMTRN
015500000000     I              TLMDDE                      DLMDDE
015600000000     I              TLMTFP                      DLMTFP
015700000000     I              TLMTFA                      DLMTFA
015800000000     I              TLMTFF                      DLMTFF
015900000000     I              TLMMCA                      DLMMCA
016000000000     I              TLMMCT                      DLMMCT
016100000000     I              TLMDIR                      DLMDIR
016200000000     I              TLMDUV                      DLMDUV
016300000000     I              TLMFTR                      DLMFTR
016400000000     I              TLMDTR                      DLMDTR
016500000000     I              TLMATB                      DLMATB
016600000000     I              TLMMCB                      DLMMCB
016700010822     ITITLLD
016800000000     I              TLLTRN                      DLLTRN
016900000000     I              TLLDDE                      DLLDDE
017000000000     I              TLLTMZ                      DLLTMZ
017100000000     I              TLLPDR                      DLLPDR
017200000000     I              TLLILI                      DLLILI
017300000000     I              TLLIES                      DLLIES
017400000000     I              TLLCAU                      DLLCAU
017500000000     I              TLLDIR                      DLLDIR
017600000000     I              TLLDUV                      DLLDUV
017700000000     I              TLLFTR                      DLLFTR
017800000000     I              TLLDTR                      DLLDTR
017900000000     I              TLLATB                      DLLATB
018000010822     I              TLLDIV                      DLLDIV
018100000000     ITNTLND
018200000000     I              TLNTRN                      DLNTRN
018300000000     I              TLNDDE                      DLNDDE
018400000000     I              TLNNFM                      DLNNFM
018500000000     I              TLNPNO                      DLNPNO
018600000000     I              TLNTNO                      DLNTNO
018700000000     I              TLNDIR                      DLNDIR
018800000000     I              TLNDUV                      DLNDUV
018900000000     I              TLNFTR                      DLNFTR
019000000000     I              TLNDTR                      DLNDTR
019100000000     I              TLNATB                      DLNATB
019200000000     C*------------------------------------------------------------------------*
019300000000     C* MAIN LINE
019400000000     C*------------------------------------------------------------------------*
019500000000     C*
019600000000     C* STAMPA PARAMETRI DI LANCIO
019700000000     C                   EXSR      STAPAR
019800000000     C*
019900000000     C* POSIZIONAMENTO E PRIMA LETTURA TRAINI
020000000000     C                   EXSR      SETFIL
020100000000     C*
020200000000     C* FINO A FINE TRAINI
020300000000DO  1C     $FINE         DOWEQ     '0'
020400000000     C*
020500000000     C* DUPLICA I FILES E/O STAMPA ELENCO
020600000000     C                   EXSR      DUPSTM
020700000000     C*
020800000000     C* LETTURA SUCESSIVA TRAINI
020900000000     C                   EXSR      LETFIL
021000000000E   1C                   ENDDO
021100000000     C*
021200000000     C* STAMPA RIGA DI FINE PROSPETTO
021300000000     C                   WRITE     TL56FI
021400000000     C*
021500000000     C                   SETON                                        LR
021600000000     C*------------------------------------------------------------------------*
021700000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA TRAINI
021800000000     C*------------------------------------------------------------------------*
021900000000     C     SETFIL        BEGSR
022000000000     C*
022100000000     C* POSIZIONAMENTO
022200000000     C                   Z-ADD     PARTR1        KLTTRN
022300000000     C     KLTTRN        SETLL     TNTLT01L                           99        *FINE FILE
022400000000     C* LETTURA
022500000000IF  1C     *IN99         IFEQ      *OFF
022600000000     C                   EXSR      LETFIL
022700000000X   1C                   ELSE
022800000000     C                   MOVEL     '1'           $FINE
022900000000E   1C                   ENDIF
023000000000     C*
023100000000     C                   ENDSR
023200000000     C*------------------------------------------------------------------------*
023300000000     C* LETFIL - LETTURA SUCESSIVA TRAINI
023400000000     C*------------------------------------------------------------------------*
023500000000     C     LETFIL        BEGSR
023600000000     C*
023700000000     C                   MOVEL     'N'           WRECOK
023800000000     C*
023900000000     C* LEGGE FINO A FINE FILE O TROVATO RECORD VALIDO
024000000000DO  1C     $FINE         DOWEQ     '0'
024100000000     C     WRECOK        ANDEQ     'N'
024200000000     C                   READ      TNTLT01L                               99
024300000000     C*
024400000000IF  2C     *IN99         IFEQ      *ON
024500000000     C                   MOVEL     '1'           $FINE
024600000000X   2C                   ELSE
024700000000     C                   EXSR      CHKREC                                       *CONTROLLA RECROD
024800000000E   2C                   ENDIF
024900000000E   1C                   ENDDO
025000000000     C*
025100000000     C                   ENDSR
025200000000     C*------------------------------------------------------------------------*
025300000000     C* CHKREC - CONTROLLA VALIDITA' TRAINI
025400000000     C*------------------------------------------------------------------------*
025500000000     C     CHKREC        BEGSR
025600000000     C*
025700000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
025800000000     C*
025900000000     C* TRAINO > DELL'ULTIMO RICHIESTO, ESCE DAL CICLO
026000000000IF  1C     TLTTRN        IFGT      PARTR2                                       ULTIMO TRAINO CHIEST
026100000000     C                   MOVEL     '1'           $FINE                          *FINE LETTURA
026200000000E   1C                   ENDIF
026300000000     C*
026400000000     C* TRAINO ANNULLATO, ESCLUDE
026500000000IF  1C     TLTATB        IFNE      *BLANKS
026600000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
026700000000E   1C                   ENDIF
026800000000     C*
026900000000     C* TRAINO CON NUMERO > 9.000.000, ESCLUDE
027000000000IF  1C     TLTTRN        IFGT      9000000
027100000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027200000000E   1C                   ENDIF
027300000000     C*
027400000000     C* TRAINO <> DA UNO DI QUELLI RICHIESTI, ESCLUDE
027500000000IF  1C     TLTTRN        IFGE      PARTR1                                       PRIMO  TRAINO CHIEST
027600000000     C     TLTTRN        ANDLE     PARTR2                                       ULTIMO TRAINO CHIEST
027700000000X   1C                   ELSE
027800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
027900000000E   1C                   ENDIF
028000980605     C*
028100980605     C* TRAINO RICHIESTO DA ESCLUDERE, ESCLUDE
028200980605IF  1C     TLTTRN        IFEQ      PARTE1
028300980605     C     TLTTRN        OREQ      PARTE2
028400980605     C     TLTTRN        OREQ      PARTE3
028500980605     C     TLTTRN        OREQ      PARTE4
028600980605     C     TLTTRN        OREQ      PARTE5
028700980605     C     TLTTRN        OREQ      PARTE6
028800980605     C     TLTTRN        OREQ      PARTE7
028900980605     C     TLTTRN        OREQ      PARTE8
029000980605     C     TLTTRN        OREQ      PARTE9
029100980605     C     TLTTRN        OREQ      PARTE0
029200980605     C     TLTTRN        OREQ      PARTEA
029300980605     C     TLTTRN        OREQ      PARTEB
029400980605     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
029500980605E   1C                   ENDIF
029600000000     C*
029700000000     C* DECORRENZA <> DA QUELLA RICHIESTA, ESCLUDE
029800000000IF  1C     PARDDE        IFGT      *ZEROS
029900000000IF  2C     TLTDDE        IFNE      PARDDE
030000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
030100000000E   2C                   ENDIF
030200000000E   1C                   ENDIF
030300000000     C*
030400000000     C* SCADENZA <> DA QUELLA RICHIESTA, ESCLUDE
030500000000IF  1C     PARDSC        IFGT      *ZEROS
030600000000IF  2C     TLTDSC        IFNE      PARDSC
030700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
030800000000E   2C                   ENDIF
030900000000E   1C                   ENDIF
031000000000     C*
031100000000     C                   ENDSR
031200000000     C*------------------------------------------------------------------------*
031300000000     C* DUPSTM - DUPLICA I FILES E/O STAMPA ELENCO
031400000000     C*------------------------------------------------------------------------*
031500000000     C     DUPSTM        BEGSR
031600000000     C*
031700000000     C* CONTROLLA SE IL TRAINO PUO' ESSERE DUPLICATO, NEL CASO AGGIORNA I FILES
031800000000     C                   EXSR      CHKTRN
031900000000IF  1C     WTRNOK        IFEQ      'S'                                          *TRAINO VALIDO
032000000000IF  2C     PARSIM        IFEQ      'N'                                          *NO SIMULAZIONE
032100000000     C                   EXSR      AGGFIL
032200000000E   2C                   ENDIF
032300000000E   1C                   ENDIF
032400000000     C*
032500000000     C* STAMPA IL RISULTATO DELL'ELABORAZIONE
032600000000     C                   EXSR      STMELA
032700000000     C*
032800000000     C                   ENDSR
032900000000     C*------------------------------------------------------------------------*
033000000000     C* CHKTRN - CONTROLLA SE IL TRAINO PUO' ESSERE DUPLICATO
033100000000     C*------------------------------------------------------------------------*
033200000000     C     CHKTRN        BEGSR
033300000000     C*
033400000000     C* REIMPOSTA I FLAG DI ERRORE
033500000000     C                   MOVEL     'S'           WTRNOK                         *TRAINO VALIDO
033600000000     C                   MOVEL     '0'           WERR                           *NO ERRORE
033700000000     C*
033800000000     C* IMPOSTA IL NUMERO SUCCESSIVO SU CUI COPIARE
033900000000IF  1C     PARMAN        IFEQ      'N'                                          *NUOVA NUMERAZIONE
034000000000     C     NEWTRN        ADD       1             NEWTRN                         *1°NUM SU CUI COPIAR
034100000000X   1C                   ELSE
034200000000     C                   Z-ADD     TLTTRN        NEWTRN                         *1°NUM SU CUI COPIAR
034300000000E   1C                   ENDIF
034400000000     C*
034500000000     C* SE IL TRAINO SU CUI COPIARE HA UN NUMERO > 9.000.000 LO ESCLUDE PERCHE'
034600000000     C* QUESTI NUMERI SONO DI COMPETENZA DI UN ALTRO PGM E CONTENUTI IN DATAAREA
034700000000IF  1C     NEWTRN        IFGT      9000000
034800000000     C                   MOVEL     'N'           WTRNOK                         *TRAINO NON VALIDO
034900000000     C                   MOVEL     '4'           WERR                           *ERRORE QUATTRO
035000000000E   1C                   ENDIF
035100000000     C*
035200000000     C* NUOVA NUMERAZIONE: CONTROLLA SE IL NUOVO TRAINO ESISTE GIA' -->
035300000000     C* SE SI' DEVE ESSERE RICHIESTA LA SOVRAPPOSIZIONE
035400000000IF  1C     WTRNOK        IFEQ      'S'                                          *ANCORA VALIDO
035500000000IF  2C     PARMAN        IFEQ      'N'                                          *NUOVA NUMERAZIONE
035600000000     C                   Z-ADD     NEWTRN        KL2TRN                         *NUOVO NUMERO TRAINO
035700000000     C                   Z-ADD     PARDD2        KL2DDE                         *NUOVA DECORRENZA
035800000000     C     KE2TLT        CHAIN(N)  TNTLT11L                           98
035900000000IF  3C     *IN98         IFEQ      *OFF                                         *ESISTE
036000000000IF  4C     PARESI        IFEQ      'N'                                          *NON RICH.SOVRAPPOSI
036100000000     C                   MOVEL     'N'           WTRNOK                         *TRAINO NON VALIDO
036200000000     C                   MOVEL     '1'           WERR                           *ERRORE UNO
036300000000E   4C                   ENDIF
036400000000E   3C                   ENDIF
036500000000E   2C                   ENDIF
036600000000E   1C                   ENDIF
036700000000     C*
036800000000     C* CONTRALLA SE TRAINI SOVRAPPOSTI
036900000000IF  1C     WTRNOK        IFEQ      'S'                                          *ANCORA VALIDO
037000000000     C                   Z-ADD     NEWTRN        KL2TRN
037100000000     C     KL2TRN        SETLL     TNTLT11L
037200000000     C     KL2TRN        READE(N)  TNTLT11L                               98
037300000000DO  2C     *IN98         DOWEQ     *OFF
037400000000IF  3C     DLTATB        IFEQ      *BLANKS                                      *NON ANNULLATI
037500000000IF  4C     DLTDDE        IFLT      PARDD2                                       *NON SOVRAPPOSTO
037600110610     C     DLTDSC        ANDLT     PARDD2
037700000000     C     DLTDDE        ORGT      PARDS2
037800000000X   4C                   ELSE                                                   *SOVRAPPOSTO
037900000000IF  5C     DLTDDE        IFEQ      TLTDDE                                       *STESSO DECORRENZA
038000000000     C     DLTDSC        ANDEQ     TLTDSC                                       *STESSA SCADENZA
038100000000     C     PARESI        ANDEQ     'S'                                          *DA SOVRAPPORRE
038200000000X   5C                   ELSE                                                   *STESSO TRAINO
038300000000     C                   MOVEL     'N'           WTRNOK                         *TRAINO NON VALIDO
038400000000     C                   MOVEL     '2'           WERR                           *ERRORE UNO
038500000000E   5C                   ENDIF
038600000000E   4C                   ENDIF
038700000000E   3C                   ENDIF
038800000000     C     KL2TRN        READE(N)  TNTLT11L                               98
038900000000E   2C                   ENDDO
039000000000E   1C                   ENDIF
039100000000     C*
039200000000     C* SE IL TRAINO DA CUI COPIARE E' UGUALE AL TRAINO SU CUI COPIARE, ESCLUDE
039300000000IF  1C     TLTTRN        IFEQ      NEWTRN
039400000000     C     TLTDDE        ANDEQ     PARDD2
039500000000     C                   MOVEL     'N'           WTRNOK                         *TRAINO NON VALIDO
039600000000     C                   MOVEL     '3'           WERR                           *ERRORE UNO
039700000000E   1C                   ENDIF
039800000000     C*
039900000000     C                   ENDSR
040000000000     C*------------------------------------------------------------------------*
040100000000     C* AGGFIL - AGGIORNA I FILES
040200000000     C*------------------------------------------------------------------------*
040300000000     C     AGGFIL        BEGSR
040400000000     C*--------------------
040500000000     C* TRAINO
040600000000     C*--------------------
040700000000     C* LEGGE IL NUOVO TRAINO E IMPOSTA I CAMPI (QUELLO VECCHIO E' IN LINEA)
040800000000     C                   Z-ADD     NEWTRN        KL2TRN
040900000000     C                   Z-ADD     PARDD2        KL2DDE
041000000000     C     KE2TLT        CHAIN     TNTLT11L                           98
041100000000     C                   Z-ADD     NEWTRN        DLTTRN
041200000000     C                   Z-ADD     PARDD2        DLTDDE
041300000000     C                   Z-ADD     PARDS2        DLTDSC
041400000000     C                   Z-ADD     TLTTFP        DLTTFP
041500000000     C                   Z-ADD     TLTTFA        DLTTFA
041600000000     C                   MOVEL     TLTTTR        DLTTTR
041700000000     C                   MOVEL     TLTGSE        DLTGSE
041800000000     C                   MOVEL     TLTBIS        DLTBIS
041900000000     C                   MOVEL     TLTTMZ        DLTTMZ
042000000000     C                   Z-ADD     TLTFES        DLTFES
042100000000     C                   MOVEL     *BLANKS       DLTFTR
042200000000     C                   MOVEL     TLTATB        DLTATB
042300000000IF  1C     *IN98         IFEQ      *OFF                                         *VARIAZIONE
042400000000     C                   Z-ADD     TLTDIR        DLTDIR
042500000000     C                   Z-ADD     DATCOR        DLTDUV
042600000000     C                   Z-ADD     TLTDTR        DLTDTR
042700000000     C                   UPDATE    TNTLTD
042800000000X   1C                   ELSE                                                   *INSERIMENTO
042900000000     C                   Z-ADD     DATCOR        DLTDIR
043000000000     C                   Z-ADD     DATCOR        DLTDUV
043100000000     C                   Z-ADD     *ZEROS        DLTDTR
043200000000     C                   WRITE     TNTLTD
043300000000E   1C                   ENDIF
043400000000     C*--------------------
043500000000     C* FERMATE
043600000000     C*--------------------
043700000000     C* LEGGE LE VECCHIE FERMATE
043800000000     C                   Z-ADD     TLTTRN        KLRTRN
043900000000     C                   Z-ADD     TLTDDE        KLRDDE
044000000000     C     KEYTLR        SETLL     TNTLR01L
044100000000     C     KEYTLR        READE     TNTLR01L                               98
044200000000DO  1C     *IN98         DOWEQ     *OFF
044300000000     C*
044400000000     C* LEGGE LA NUOVA FERMATA
044500000000     C                   Z-ADD     NEWTRN        KL2TRN
044600000000     C                   Z-ADD     PARDD2        KL2DDE
044700000000     C                   Z-ADD     TLRNFM        KL2NFM
044800000000     C     KE2TLR        CHAIN     TNTLR11L                           97
044900000000     C                   Z-ADD     NEWTRN        DLRTRN
045000000000     C                   Z-ADD     PARDD2        DLRDDE
045100000000     C                   Z-ADD     TLRNFM        DLRNFM
045200000000     C                   MOVEL     TLRTFM        DLRTFM
045300000000     C                   Z-ADD     TLRFIL        DLRFIL
045400000000     C                   MOVEL     TLRSCA        DLRSCA
045500000000     C                   MOVEL     TLRCAR        DLRCAR
045600000000     C                   Z-ADD     TLRKMT        DLRKMT
045700000000     C                   Z-ADD     TLRTEM        DLRTEM
045800000000     C                   MOVEL     TLRGFV        DLRGFV
045900000000     C                   MOVEL     *BLANKS       DLRFTR
046000000000     C                   MOVEL     TLRATB        DLRATB
046100000000     C* E SCRIVE LE NUOVE
046200000000IF  2C     *IN97         IFEQ      *OFF                                         *AGGIORNAMENTO
046300000000     C                   Z-ADD     TLRDIR        DLRDIR
046400000000     C                   Z-ADD     DATCOR        DLRDUV
046500000000     C                   Z-ADD     TLRDTR        DLRDTR
046600000000     C                   UPDATE    TNTLRD
046700000000X   2C                   ELSE                                                   *INSERIMENTO
046800000000IF  3C     TLRATB        IFEQ      *BLANKS                                      *SE NON ANNULLATA
046900000000     C                   Z-ADD     DATCOR        DLRDIR
047000000000     C                   Z-ADD     DATCOR        DLRDUV
047100000000     C                   Z-ADD     *ZEROS        DLRDTR
047200000000     C                   WRITE     TNTLRD
047300000000E   3C                   ENDIF
047400000000E   2C                   ENDIF
047500000000     C*--------------------
047600000000     C* ORARI
047700000000     C*--------------------
047800000000     C* LEGGE I VECCHI ORARI DELLA FERMATA
047900000000     C                   Z-ADD     TLTTRN        KLOTRN
048000000000     C                   Z-ADD     TLTDDE        KLODDE
048100000000     C                   Z-ADD     TLRNFM        KLONFM
048200000000     C     KEYTLO        SETLL     TNTLO01L
048300000000     C     KEYTLO        READE     TNTLO01L                               97
048400000000DO  2C     *IN97         DOWEQ     *OFF
048500000000     C*
048600000000     C* LEGGE I NUOVI ORARI DELLA FERMATA
048700000000     C                   Z-ADD     NEWTRN        KL2TRN
048800000000     C                   Z-ADD     PARDD2        KL2DDE
048900000000     C                   Z-ADD     TLONFM        KL2NFM
049000000000     C                   MOVEL     TLOGSE        KL2GSE
049100000000     C     KE2TLO        CHAIN     TNTLO11L                           96
049200000000     C                   Z-ADD     NEWTRN        DLOTRN
049300000000     C                   Z-ADD     PARDD2        DLODDE
049400000000     C                   Z-ADD     TLONFM        DLONFM
049500000000     C                   MOVEL     TLOGSE        DLOGSE
049600000000     C                   MOVEL     TLOGGA        DLOGGA
049700000000     C                   Z-ADD     TLOHHA        DLOHHA
049800000000     C                   MOVEL     TLOGGP        DLOGGP
049900000000     C                   Z-ADD     TLOHHP        DLOHHP
050000000000     C                   MOVEL     *BLANKS       DLOFTR
050100000000     C                   MOVEL     TLOATB        DLOATB
050200000000     C* E SCRIVE I NUOVI
050300000000IF  3C     *IN96         IFEQ      *OFF                                         *AGGIORNAMENTO
050400000000     C                   Z-ADD     TLODIR        DLODIR
050500000000     C                   Z-ADD     DATCOR        DLODUV
050600000000     C                   Z-ADD     TLODTR        DLODTR
050700000000     C                   UPDATE    TNTLOD
050800000000X   3C                   ELSE                                                   *INSERIMENTO
050900000000IF  4C     TLOATB        IFEQ      *BLANKS                                      *SE NON ANNULLATO
051000000000     C                   Z-ADD     DATCOR        DLODIR
051100000000     C                   Z-ADD     DATCOR        DLODUV
051200000000     C                   Z-ADD     *ZEROS        DLODTR
051300000000     C                   WRITE     TNTLOD
051400000000E   4C                   ENDIF
051500000000E   3C                   ENDIF
051600000000     C     KEYTLO        READE     TNTLO01L                               97
051700000000E   2C                   ENDDO
051800000000     C*
051900000000     C     KEYTLR        READE     TNTLR01L                               98
052000000000E   1C                   ENDDO
052100000000     C*
052200000000     C* LEGGE LE FERMATE IN PIU', SE ESISTONO, DEL NUOVO TRAINO
052300000000     C                   Z-ADD     NEWTRN        KL2TRN                         *NUOVA TRAINO
052400000000     C                   Z-ADD     PARDD2        KL2DDE
052500000000     C                   Z-ADD     TLRNFM        KL2NFM                         *ULTIMA FERMATA LETT
052600000000     C     KE2TLR        SETGT     TNTLR11L
052700000000     C     KE3TLR        READE     TNTLR11L                               98
052800000000DO  1C     *IN98         DOWEQ     *OFF
052900000000     C*
053000000000     C* CANCELLA O ANNULLA GLI ORARI DELLE FERMATE IN PIU'
053100000000     C                   Z-ADD     NEWTRN        KLOTRN
053200000000     C                   Z-ADD     PARDD2        KLODDE
053300000000     C                   Z-ADD     DLRNFM        KLONFM
053400000000     C     KEYTLO        SETLL     TNTLO11L
053500000000     C     KEYTLO        READE     TNTLO11L                               97
053600000000DO  2C     *IN97         DOWEQ     *OFF
053700000000IF  3C     DLODTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
053800000000     C                   MOVEL     'A'           DLOATB
053900000000     C                   Z-ADD     DATCOR        DLODUV
054000000000     C                   MOVEL     *BLANKS       DLOFTR
054100000000     C                   UPDATE    TNTLOD
054200000000X   3C                   ELSE                                                   *NON ANCORA TRASMESS
054300000000     C                   DELETE    TNTLOD
054400000000E   3C                   ENDIF
054500000000     C     KEYTLO        READE     TNTLO11L                               97
054600000000E   2C                   ENDDO
054700000000     C*
054800000000     C* CANCELLA O ANNULLA LE FERMATE IN PIU'
054900000000IF  2C     DLRDTR        IFGT      *ZEROS                                       *GIA' TRASMESSA
055000000000     C                   MOVEL     'A'           DLRATB
055100000000     C                   Z-ADD     DATCOR        DLRDUV
055200000000     C                   MOVEL     *BLANKS       DLRFTR
055300000000     C                   UPDATE    TNTLRD
055400000000X   2C                   ELSE                                                   *NON ANCORA TRASMESS
055500000000     C                   DELETE    TNTLRD
055600000000E   2C                   ENDIF
055700000000     C     KE3TLR        READE     TNTLR11L                               98
055800000000E   1C                   ENDDO
055900000000     C*--------------------
056000000000     C* METRICUBI
056100000000     C*--------------------
056200000000     C* LEGGE I METRICUBI DEL NUOVO TRAINO, SE CI SONO, E LI ANNULLA TUTTI
056300000000     C                   Z-ADD     NEWTRN        KL2TRN
056400000000     C                   Z-ADD     PARDD2        KL2DDE
056500000000     C     KE2TLM        SETLL     TNTLM11L
056600000000     C     KE2TLM        READE     TNTLM11L                               97
056700000000DO  1C     *IN97         DOWEQ     *OFF
056800000000IF  2C     DLMDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
056900000000     C                   MOVEL     'A'           DLMATB
057000000000     C                   Z-ADD     DATCOR        DLMDUV
057100000000     C                   MOVEL     *BLANKS       DLMFTR
057200000000     C                   UPDATE    TNTLMD
057300000000X   2C                   ELSE                                                   *NON ANCORA TRASMESS
057400000000     C                   DELETE    TNTLMD
057500000000E   2C                   ENDIF
057600000000     C     KE2TLM        READE     TNTLM11L                               97
057700000000E   1C                   ENDDO
057800000000     C*
057900000000     C* LEGGE I VECCHI METRICUBI
058000000000     C                   Z-ADD     TLTTRN        KLMTRN
058100000000     C                   Z-ADD     TLTDDE        KLMDDE
058200000000     C     KEYTLM        SETLL     TNTLM01L
058300000000     C     KEYTLM        READE     TNTLM01L                               98
058400000000DO  1C     *IN98         DOWEQ     *OFF
058500000000     C                   Z-ADD     NEWTRN        KL2TRN
058600000000     C                   Z-ADD     PARDD2        KL2DDE
058700000000     C                   Z-ADD     TLMTFP        KL2TFP
058800000000     C                   Z-ADD     TLMTFA        KL2TFA
058900000000     C                   Z-ADD     TLMTFF        KL2TFF
059000000000     C     KE2TLM        CHAIN     TNTLM11L                           97
059100000000     C                   Z-ADD     NEWTRN        DLMTRN
059200000000     C                   Z-ADD     PARDD2        DLMDDE
059300000000     C                   Z-ADD     TLMTFP        DLMTFP
059400000000     C                   Z-ADD     TLMTFA        DLMTFA
059500000000     C                   Z-ADD     TLMTFF        DLMTFF
059600000000     C                   Z-ADD     TLMMCA        DLMMCA
059700000000     C                   Z-ADD     TLMMCB        DLMMCB
059800000000     C                   Z-ADD     TLMMCT        DLMMCT
059900000000     C                   MOVEL     *BLANKS       DLMFTR
060000000000     C                   MOVEL     TLMATB        DLMATB
060100000000     C* E SCRIVE I NUOVI
060200000000IF  2C     *IN97         IFEQ      *OFF                                         *AGGIORNAMENTO
060300000000     C                   Z-ADD     TLMDIR        DLMDIR
060400000000     C                   Z-ADD     DATCOR        DLMDUV
060500000000     C                   Z-ADD     TLMDTR        DLMDTR
060600000000     C                   UPDATE    TNTLMD
060700000000X   2C                   ELSE                                                   *INSERIMENTO
060800000000IF  3C     TLMATB        IFEQ      *BLANKS                                      *SE NON ANNULLATO
060900000000     C                   Z-ADD     DATCOR        DLMDIR
061000000000     C                   Z-ADD     DATCOR        DLMDUV
061100000000     C                   Z-ADD     *ZEROS        DLMDTR
061200000000     C                   WRITE     TNTLMD
061300000000E   3C                   ENDIF
061400000000E   2C                   ENDIF
061500000000     C     KEYTLM        READE     TNTLM01L                               98
061600000000E   1C                   ENDDO
061700000000     C*--------------------
061800000000     C* LISTINI
061900000000     C*--------------------
062000000000     C* LEGGE I LISTINI DEL NUOVO TRAINO, SE CI SONO, E LI ANNULLA TUTTI
062100000000     C                   Z-ADD     NEWTRN        KL2TRN
062200000000     C                   Z-ADD     PARDD2        KL2DDE
062300010822     C     KE2TLL        SETLL     TITLL11L
062400010822     C     KE2TLL        READE     TITLL11L                               97
062500000000DO  1C     *IN97         DOWEQ     *OFF
062600000000IF  2C     DLLDTR        IFGT      *ZEROS                                       *GIA' TRASMESSO
062700000000     C                   MOVEL     'A'           DLLATB
062800000000     C                   Z-ADD     DATCOR        DLLDUV
062900000000     C                   MOVEL     *BLANKS       DLLFTR
063000010822     C                   UPDATE    TITLLD
063100000000X   2C                   ELSE                                                   *NON ANCORA TRASMESS
063200010822     C                   DELETE    TITLLD
063300000000E   2C                   ENDIF
063400010822     C     KE2TLL        READE     TITLL11L                               97
063500000000E   1C                   ENDDO
063600000000     C*
063700000000     C* LEGGE I VECCHI LISTINI
063800000000     C                   Z-ADD     TLTTRN        KLLTRN
063900000000     C                   Z-ADD     TLTDDE        KLLDDE
064000010822     C     KEYTLL        SETLL     TITLL01L
064100010822     C     KEYTLL        READE     TITLL01L                               98
064200000000DO  1C     *IN98         DOWEQ     *OFF
064300000000     C                   Z-ADD     NEWTRN        KL2TRN
064400000000     C                   Z-ADD     PARDD2        KL2DDE
064500000000     C                   MOVEL     TLLTMZ        KL2TMZ
064600000000     C                   Z-ADD     TLLPDR        KL2PDR
064700010822     C     KE2TLL        CHAIN     TITLL11L                           97
064800000000     C                   Z-ADD     NEWTRN        DLLTRN
064900000000     C                   Z-ADD     PARDD2        DLLDDE
065000000000     C                   MOVEL     TLLTMZ        DLLTMZ
065100000000     C                   Z-ADD     TLLPDR        DLLPDR
065200000000     C                   Z-ADD     TLLILI        DLLILI
065300000000     C                   Z-ADD     TLLIES        DLLIES
065400010822     C                   MOVEL     TLLDIV        DLLDIV
065500000000     C                   MOVEL     TLLCAU        DLLCAU
065600000000     C                   MOVEL     *BLANKS       DLLFTR
065700000000     C                   MOVEL     TLLATB        DLLATB
065800000000     C* E SCRIVE I NUOVI
065900000000IF  2C     *IN97         IFEQ      *OFF                                         *AGGIORNAMENTO
066000000000     C                   Z-ADD     TLLDIR        DLLDIR
066100000000     C                   Z-ADD     DATCOR        DLLDUV
066200000000     C                   Z-ADD     TLLDTR        DLLDTR
066300010822     C                   UPDATE    TITLLD
066400000000X   2C                   ELSE                                                   *INSERIMENTO
066500000000IF  3C     TLLATB        IFEQ      *BLANKS                                      *SE NON ANNULLATO
066600000000     C                   Z-ADD     DATCOR        DLLDIR
066700000000     C                   Z-ADD     DATCOR        DLLDUV
066800000000     C                   Z-ADD     *ZEROS        DLLDTR
066900010822     C                   WRITE     TITLLD
067000000000E   3C                   ENDIF
067100000000E   2C                   ENDIF
067200010822     C     KEYTLL        READE     TITLL01L                               98
067300000000E   1C                   ENDDO
067400000000     C*--------------------
067500000000     C* NOTE
067600000000     C*--------------------
067700000000     C* LEGGE LE NOTE DEL NUOVO TRAINO, SE CI SONO, E LE ANNULLA TUTTE
067800000000     C                   Z-ADD     NEWTRN        KL2TRN
067900000000     C                   Z-ADD     PARDD2        KL2DDE
068000000000     C     KE2TLN        SETLL     TNTLN11L
068100000000     C     KE2TLN        READE     TNTLN11L                               97
068200000000DO  1C     *IN97         DOWEQ     *OFF
068300000000IF  2C     DLNDTR        IFGT      *ZEROS                                       *GIA' TRASMESSA
068400000000     C                   MOVEL     'A'           DLNATB
068500000000     C                   Z-ADD     DATCOR        DLNDUV
068600000000     C                   MOVEL     *BLANKS       DLNFTR
068700000000     C                   UPDATE    TNTLND
068800000000X   2C                   ELSE                                                   *NON ANCORA TRASMESS
068900000000     C                   DELETE    TNTLND
069000000000E   2C                   ENDIF
069100000000     C     KE2TLN        READE     TNTLN11L                               97
069200000000E   1C                   ENDDO
069300000000     C*
069400000000     C* LEGGE LE VECCHIE NOTE
069500000000     C                   Z-ADD     TLTTRN        KLNTRN
069600000000     C                   Z-ADD     TLTDDE        KLNDDE
069700000000     C     KEYTLN        SETLL     TNTLN01L
069800000000     C     KEYTLN        READE     TNTLN01L                               98
069900000000DO  1C     *IN98         DOWEQ     *OFF
070000000000     C                   Z-ADD     NEWTRN        KL2TRN
070100000000     C                   Z-ADD     PARDD2        KL2DDE
070200000000     C                   Z-ADD     TLNNFM        KL2NFM
070300000000     C                   Z-ADD     TLNPNO        KL2PNO
070400000000     C     KE2TLN        CHAIN     TNTLN11L                           97
070500000000     C                   Z-ADD     NEWTRN        DLNTRN
070600000000     C                   Z-ADD     PARDD2        DLNDDE
070700000000     C                   Z-ADD     TLNNFM        DLNNFM
070800000000     C                   Z-ADD     TLNPNO        DLNPNO
070900000000     C                   MOVEL     TLNTNO        DLNTNO
071000000000     C                   MOVEL     *BLANKS       DLNFTR
071100000000     C                   MOVEL     TLLATB        DLNATB
071200000000     C* E SCRIVE LE NUOVE
071300000000IF  2C     *IN97         IFEQ      *OFF                                         *AGGIORNAMENTO
071400000000     C                   Z-ADD     TLNDIR        DLNDIR
071500000000     C                   Z-ADD     DATCOR        DLNDUV
071600000000     C                   Z-ADD     TLNDTR        DLNDTR
071700000000     C                   UPDATE    TNTLND
071800000000X   2C                   ELSE                                                   *INSERIMENTO
071900000000IF  3C     TLNATB        IFEQ      *BLANKS                                      *SE NON ANNULLATA
072000000000     C                   Z-ADD     DATCOR        DLNDIR
072100000000     C                   Z-ADD     DATCOR        DLNDUV
072200000000     C                   Z-ADD     *ZEROS        DLNDTR
072300000000     C                   WRITE     TNTLND
072400000000E   3C                   ENDIF
072500000000E   2C                   ENDIF
072600000000     C     KEYTLN        READE     TNTLN01L                               98
072700000000E   1C                   ENDDO
072800000000     C*
072900110607     C*--------------------
073000110607     C* Punti scambio
073100110607     C*--------------------
073200110607     C* LEGGE i punti Sc del NUOVO TRAINO, SE CI SONO, E Li ANNULLO TUTTE
073300110607     C                   Z-ADD     NEWTRN        KL2TRN
073400110607     C                   Z-ADD     PARDD2        KL2DDE
073500110610     C     KE2TLT        SETLL     TNTLS11L
073600110610     C     KE2TLT        READE     TNTLS11L                               97
073700110607DO  1C     *IN97         DOWEQ     *OFF
073800110610IF  2C     DLSDTR        IFGT      *ZEROS                                       *GIA' TRASMESSA
073900110607     C                   MOVEL     'A'           DLSATB
074000110607     C                   Z-ADD     DATCOR        DLSDUV
074100110607     C                   MOVEL     *BLANKS       DLSFTR
074200110607     C                   UPDATE    TNTLsd
074300110607X   2C                   ELSE                                                   *NON ANCORA TRASMESS
074400110607     C                   DELETE    TNTLsd
074500110607E   2C                   ENDIF
074600110610     C     KE2TLT        READE     TNTLS11L                               97
074700110607E   1C                   ENDDO
074800110607     C*
074900110607     C* LEGGE i vecchi punti scambio
075000110610     C                   Z-ADD     TLTTRN        KL2TRN
075100110610     C                   Z-ADD     TLTDDE        KL2DDE
075200110610     C     KE2TLT        SETLL     TNTLS01L
075300110610     C     KE2TLT        READE     TNTLS01L                               98
075400110607DO  1C     *IN98         DOWEQ     *OFF
075500110607     C                   Z-ADD     NEWTRN        KL2TRN
075600110607     C                   Z-ADD     PARDD2        KL2DDE
075700110610     C                   movel     TLSgse        KL2GSE
075800110607     C     KE2TLS        CHAIN     TNTLS11L                           97
075900110607     C                   Z-ADD     NEWTRN        DLsTRN
076000110607     C                   Z-ADD     PARDD2        DLsDDE
076100110610     C                   movel     TLsgse        DLsgse
076200110607     c                   Z-ADD     TLstrns       DLstrns
076300110607     C                   MOVEL     TLspsc        DLspsc
076400110607     C                   MOVEL     TLsggs        DLsggs
076500110607     C                   MOVEL     TLshhs        DLshhs
076600110607     C                   MOVEL     TLsnot        DLsnot
076700110607     c
076800110607     C                   MOVEL     *BLANKS       DLsFTR
076900110607     C                   MOVEL     TLsATB        DLsATB
077000110607     C* E SCRIVE LE NUOVE
077100110607IF  2C     *IN97         IFEQ      *OFF                                         *AGGIORNAMENTO
077200110607     C                   Z-ADD     TLsDIR        DLsDIR
077300110607     C                   Z-ADD     DATCOR        DLsDUV
077400110607     C                   Z-ADD     TLsDTR        DLsDTR
077500110607     C                   UPDATE    TNTLsd
077600110607X   2C                   ELSE                                                   *INSERIMENTO
077700110607IF  3C     TLsATB        IFEQ      *BLANKS                                      *SE NON ANNULLATA
077800110607     C                   Z-ADD     DATCOR        DLsDIR
077900110607     C                   Z-ADD     DATCOR        DLsDUV
078000110607     C                   Z-ADD     *ZEROS        DLsDTR
078100110607     C                   WRITE     TNTLsd
078200110607E   3C                   ENDIF
078300110607E   2C                   ENDIF
078400110610     C     KE2TLT        READE     TNTLS01L                               98
078500110607E   1C                   ENDDO
078600000000     C                   ENDSR
078700000000     C*-----------------------------------------------------------------------*
078800000000     C* STMELA - STAMPA IL RISULTATO DELL'ELABORAZIONE
078900000000     C*------------------------------------------------------------------------*
079000000000     C     STMELA        BEGSR
079100000000     C*
079200000000     C* SE FINE PAGINA STAMPA TESTATA
079300000000IF  1C     *IN01         IFEQ      *ON
079400000000     C                   MOVEL     '0'           *IN01
079500000000     C                   WRITE     TL56TE                                       *TESTATA
079600000000     C                   WRITE     TL56IN                                       *INTESTAZIONE
079700000000E   1C                   ENDIF
079800000000     C* TRAINO DA CUI COPIARE
079900000000     C                   Z-ADD     TLTTRN        PDETRN                         *TRAINO
080000000000     C                   Z-ADD     TLTDDE        G08INV
080100000000     C                   Z-ADD     *ZEROS        G08DAT
080200000000     C                   MOVEL     '3'           G08ERR
080300000000     C                   CALL      'XSRDA8'
080400000000     C                   PARM                    WLBDA8
080500000000     C                   Z-ADD     G08DAT        PDEDDE                         *DATA DECORRENZA
080600000000     C                   Z-ADD     TLTDSC        G08INV
080700000000     C                   Z-ADD     *ZEROS        G08DAT
080800000000     C                   MOVEL     '3'           G08ERR
080900000000     C                   CALL      'XSRDA8'
081000000000     C                   PARM                    WLBDA8
081100000000     C                   Z-ADD     G08DAT        PDEDSC                         *DATA SCADENZA
081200000000     C* TRAINO SU CUI COPIARE
081300000000     C                   Z-ADD     NEWTRN        PDETR2                         *TRAINO
081400000000     C                   Z-ADD     PARDD2        G08INV
081500000000     C                   Z-ADD     *ZEROS        G08DAT
081600000000     C                   MOVEL     '3'           G08ERR
081700000000     C                   CALL      'XSRDA8'
081800000000     C                   PARM                    WLBDA8
081900000000     C                   Z-ADD     G08DAT        PDEDD2                         *DATA DECORRENZA
082000000000     C                   Z-ADD     PARDS2        G08INV
082100000000     C                   Z-ADD     *ZEROS        G08DAT
082200000000     C                   MOVEL     '3'           G08ERR
082300000000     C                   CALL      'XSRDA8'
082400000000     C                   PARM                    WLBDA8
082500000000     C                   Z-ADD     G08DAT        PDEDS2                         *DATA SCADENZA
082600000000     C* RISULTATO DUPLICA
082700000000IF  1C     WERR          IFEQ      '0'
082800000000     C                   MOVEL     'SI'          PDEDOK                         *OK
082900000000     C                   MOVEL     *BLANKS       PDEANM
083000000000X   1C                   ELSE
083100000000     C                   MOVEL     'NO'          PDEDOK                         *NON OK
083200000000     C                   Z-ADD     *ZEROS        J                 2 0
083300000000     C                   MOVE      WERR          J
083400000000     C                   MOVEL     MSG(J)        PDEANM                         *MESSAGGIO DI ERRORE
083500000000E   1C                   ENDIF
083600000000     C*
083700000000     C                   WRITE     TL56DE                                       *DETTAGLIO
083800000000     C*
083900000000     C                   ENDSR
084000000000     C*------------------------------------------------------------------------*
084100000000     C* STAPAR - STAMPA PARAMETRI DI LANCIO
084200000000     C*------------------------------------------------------------------------*
084300000000     C     STAPAR        BEGSR
084400000000     C*
084500000000IF  1C     PARSIM        IFEQ      'S'                                          *SIMULAZIONE DUPLICA
084600000000     C                   MOVEL     DESSIM        TEDSIM
084700000000     C                   MOVEL     'SI'          PACSIM
084800000000X   1C                   ELSE
084900000000     C                   MOVEL     'NO'          PACSIM
085000000000E   1C                   ENDIF
085100000000IF  1C     PARESI        IFEQ      'S'                                          *SOVRASCRITTURA
085200000000     C                   MOVEL     'SI'          PACESI
085300000000X   1C                   ELSE
085400000000     C                   MOVEL     'NO'          PACESI
085500000000E   1C                   ENDIF
085600000000     C                   Z-ADD     PARTR1        PACTR1                         *DA N°TRAINO
085700000000     C                   Z-ADD     PARTR2        PACTR2                         *A  N°TRAINO
085800980605     C                   Z-ADD     PARTE1        PACTE1                         *TRANNE TRAINI N°
085900980605     C                   Z-ADD     PARTE2        PACTE2
086000980605     C                   Z-ADD     PARTE3        PACTE3
086100980605     C                   Z-ADD     PARTE4        PACTE4
086200980605     C                   Z-ADD     PARTE5        PACTE5
086300980605     C                   Z-ADD     PARTE6        PACTE6
086400980605     C                   Z-ADD     PARTE7        PACTE7
086500980605     C                   Z-ADD     PARTE8        PACTE8
086600980605     C                   Z-ADD     PARTE9        PACTE9
086700980605     C                   Z-ADD     PARTE0        PACTE0
086800980605     C                   Z-ADD     PARTEA        PACTEA
086900980605     C                   Z-ADD     PARTEB        PACTEB
087000000000     C                   Z-ADD     PARDDE        G08INV
087100000000     C                   Z-ADD     *ZEROS        G08DAT
087200000000     C                   MOVEL     '3'           G08ERR
087300000000     C                   CALL      'XSRDA8'
087400000000     C                   PARM                    WLBDA8
087500000000     C                   Z-ADD     G08DAT        PACDDE                         *DATA DECORRENZA
087600000000     C                   Z-ADD     PARDSC        G08INV
087700000000     C                   Z-ADD     *ZEROS        G08DAT
087800000000     C                   MOVEL     '3'           G08ERR
087900000000     C                   CALL      'XSRDA8'
088000000000     C                   PARM                    WLBDA8
088100000000     C                   Z-ADD     G08DAT        PACDSC                         *DATA SCADENZA
088200000000IF  1C     PARMAN        IFEQ      'S'
088300000000     C                   MOVEL     'SI'          PACMAN
088400000000X   1C                   ELSE
088500000000     C                   MOVEL     'NO'          PACMAN
088600000000E   1C                   ENDIF
088700000000     C                   Z-ADD     PARNUM        PACNUM                         *PRIMO N°TRAINO
088800000000     C                   Z-ADD     PARDD2        G08INV
088900000000     C                   Z-ADD     *ZEROS        G08DAT
089000000000     C                   MOVEL     '3'           G08ERR
089100000000     C                   CALL      'XSRDA8'
089200000000     C                   PARM                    WLBDA8
089300000000     C                   Z-ADD     G08DAT        PACDD2                         *NUOVA DECORRENZA
089400000000     C                   Z-ADD     PARDS2        G08INV
089500000000     C                   Z-ADD     *ZEROS        G08DAT
089600000000     C                   MOVEL     '3'           G08ERR
089700000000     C                   CALL      'XSRDA8'
089800000000     C                   PARM                    WLBDA8
089900000000     C                   Z-ADD     G08DAT        PACDS2                         *NUOVA SCADENZA
090000000000     C*
090100000000     C                   WRITE     TL56TE                                       *TESTATA
090200000000     C                   WRITE     TL56PA                                       *PARAMETRI
090300000000     C*
090400000000     C                   ENDSR
090500000000     C*------------------------------------------------------------------------*
090600000000     C* *INZSR - ROUTINE INIZIALE
090700000000     C*------------------------------------------------------------------------*
090800000000     C     *INZSR        BEGSR
090900000000     C*--------------------
091000000000     C* RICEVIMENTO PARAMETRI
091100000000     C*--------------------
091200000000     C     *ENTRY        PLIST
091300000000     C                   PARM                    KPJBA
091400000000     C                   MOVEL     KPJBU         PARAM
091500000000     C*--------------------
091600000000     C* VARIABILI RIFERITE AL DATA BASE
091700000000     C*--------------------
091800000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
091900000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
092000000000     C     *LIKE         DEFINE    TLTTRN        KL2TRN
092100000000     C     *LIKE         DEFINE    TLTDDE        KL2DDE
092200000000     C     *LIKE         DEFINE    TLRTRN        KLRTRN                         *TNTLR00F
092300000000     C     *LIKE         DEFINE    TLRDDE        KLRDDE
092400000000     C     *LIKE         DEFINE    TLRNFM        KL2NFM
092500000000     C     *LIKE         DEFINE    TLOTRN        KLOTRN                         *TNTLO00F
092600000000     C     *LIKE         DEFINE    TLODDE        KLODDE
092700000000     C     *LIKE         DEFINE    TLONFM        KLONFM
092800000000     C     *LIKE         DEFINE    TLOGSE        KL2GSE
092900000000     C     *LIKE         DEFINE    TLMTRN        KLMTRN                         *TNTLM00F
093000000000     C     *LIKE         DEFINE    TLMDDE        KLMDDE
093100000000     C     *LIKE         DEFINE    TLMTFP        KL2TFP
093200000000     C     *LIKE         DEFINE    TLMTFA        KL2TFA
093300000000     C     *LIKE         DEFINE    TLMTFF        KL2TFF
093400010822     C     *LIKE         DEFINE    TLLTRN        KLLTRN                         *TITLL00F
093500000000     C     *LIKE         DEFINE    TLLDDE        KLLDDE
093600000000     C     *LIKE         DEFINE    TLLTMZ        KL2TMZ
093700000000     C     *LIKE         DEFINE    TLLPDR        KL2PDR
093800000000     C     *LIKE         DEFINE    TLNTRN        KLNTRN                         *TNTLN00F
093900000000     C     *LIKE         DEFINE    TLNDDE        KLNDDE
094000000000     C     *LIKE         DEFINE    TLNPNO        KL2PNO
094100000000     C     *LIKE         DEFINE    TLTTRN        NEWTRN                         *DEPOSITI
094200000000     C*--------------------
094300000000     C* CHIAVI DI LETTURA
094400000000     C*--------------------
094500000000     C* LETTURA TNTLT01L - COMPLETA
094600000000     C     KEYTLT        KLIST
094700000000     C                   KFLD                    KLTTRN                         *TRAINO
094800000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
094900000000     C* LETTURA TNTLT11L - COMPLETA
095000000000     C     KE2TLT        KLIST
095100000000     C                   KFLD                    KL2TRN                         *TRAINO
095200000000     C                   KFLD                    KL2DDE                         *DATA DECORRENZA
095300000000     C* LETTURA TNTLR01L - PARZIALE
095400000000     C     KEYTLR        KLIST
095500000000     C                   KFLD                    KLRTRN                         *TRAINO
095600000000     C                   KFLD                    KLRDDE                         *DATA DECORRENZA
095700000000     C* LETTURA TNTLR11L - COMPLETA
095800000000     C     KE2TLR        KLIST
095900000000     C                   KFLD                    KL2TRN                         *TRAINO
096000000000     C                   KFLD                    KL2DDE                         *DATA DECORRENZA
096100000000     C                   KFLD                    KL2NFM                         *NUMERO FERMATA
096200000000     C* LETTURA TNTLR11L - PARZIALE
096300000000     C     KE3TLR        KLIST
096400000000     C                   KFLD                    KL2TRN                         *TRAINO
096500000000     C                   KFLD                    KL2DDE                         *DATA DECORRENZA
096600000000     C* LETTURA TNTLO01L - PARZIALE
096700000000     C     KEYTLO        KLIST
096800000000     C                   KFLD                    KLOTRN                         *TRAINO
096900000000     C                   KFLD                    KLODDE                         *DATA DECORRENZA
097000000000     C                   KFLD                    KLONFM                         *NUMERO FERMATA
097100000000     C* LETTURA TNTLO11L - COMPLETA
097200000000     C     KE2TLO        KLIST
097300000000     C                   KFLD                    KL2TRN                         *TRAINO
097400000000     C                   KFLD                    KL2DDE                         *DATA DECORRENZA
097500000000     C                   KFLD                    KL2NFM                         *NUMERO FERMATA
097600000000     C                   KFLD                    KL2GSE                         *GIORNO DI PARTENZA
097700000000     C* LETTURA TNTLM01L - PARZIALE
097800000000     C     KEYTLM        KLIST
097900000000     C                   KFLD                    KLMTRN                         *TRAINO
098000000000     C                   KFLD                    KLMDDE                         *DATA DECORRENZA
098100000000     C* LETTURA TNTLM11L - COMPLETA
098200000000     C     KE2TLM        KLIST
098300000000     C                   KFLD                    KL2TRN                         *TRAINO
098400000000     C                   KFLD                    KL2DDE                         *DATA DECORRENZA
098500000000     C                   KFLD                    KL2TFP                         *FILIALE PARTENZA
098600000000     C                   KFLD                    KL2TFA                         *FILIALE SCARICO
098700000000     C                   KFLD                    KL2TFF                         *FILIALE ARRIVO
098800010822     C* LETTURA TITLL01L - PARZIALE
098900000000     C     KEYTLL        KLIST
099000000000     C                   KFLD                    KLLTRN                         *TRAINO
099100000000     C                   KFLD                    KLLDDE                         *DATA DECORRENZA
099200010822     C* LETTURA TITLL01L - COMPLETA
099300000000     C     KE2TLL        KLIST
099400000000     C                   KFLD                    KL2TRN                         *TRAINO
099500000000     C                   KFLD                    KL2DDE                         *DATA DECORRENZA
099600000000     C                   KFLD                    KL2TMZ                         *TIPO AUTOMEZZO
099700000000     C                   KFLD                    KL2PDR                         *TRAZIONISTA
099800000000     C* LETTURA TNTLN01L - PARZIALE
099900000000     C     KEYTLN        KLIST
100000000000     C                   KFLD                    KLNTRN                         *TRAINO
100100000000     C                   KFLD                    KLNDDE                         *DATA DECORRENZA
100200000000     C* LETTURA TNTLN01L - COMPLETA
100300000000     C     KE2TLN        KLIST
100400000000     C                   KFLD                    KL2TRN                         *TRAINO
100500000000     C                   KFLD                    KL2DDE                         *DATA DECORRENZA
100600000000     C                   KFLD                    KL2NFM                         *NUMERO FERMATA
100700000000     C                   KFLD                    KL2PNO                         *PROGRESSIVO NOTA
100800110607     C* LETTURA TNTLS11L - COMPLETA
100900110607     C     KE2TLS        KLIST
101000110607     C                   KFLD                    KL2TRN                         *TRAINO
101100110607     C                   KFLD                    KL2DDE                         *DATA DECORRENZA
101200110607     C                   KFLD                    KL2GSE                         *GIORNO DI PARTENZA
101300000000     C*--------------------
101400000000     C* VARIABILI DI CONTROLLO
101500000000     C*--------------------
101600000000     C                   MOVEL     '0'           $FINE             1            *FINE LETTURA TRAINI
101700000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
101800000000     C                   MOVEL     'N'           WTRNOK            1            *VALIDITA' TRAINO
101900000000     C                   MOVEL     *BLANKS       $MSG             78            *MESSAGGI DI ERRORE
102000000000     C                   MOVEL     '1'           *IN01                          *OVERFLOW
102100000000     C                   MOVEL     '0'           WERR              1            *FLAG STAMPA ERRORE
102200000000     C*
102300000000     C* REPERISCE I DATI UTENTE
102400020716
102500020716     c     *dtaara       define    §azute        azuteds
102600020716     c     *dtaara       define    §datiute      ddatiute
102700020716
102800020716     c                   in(E)     *dtaara
102900020716     c                   if        %Error or rsut = *blanks
103000020716     c                   clear                   tibs34ds
103100020716     c                   call      'TIBS34R'
103200020716     c                   parm                    Tibs34Ds
103300020716     c                   in        *dtaara
103400020716     c                   endif
103500000000     C*
103600000000     C* CALCOLA LA DATA CORRENTE
103700000000     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
103800000000     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
103900000000     C                   Z-ADD     WNUM8         G08DAT
104000000000     C                   Z-ADD     *ZEROS        G08INV
104100000000     C                   MOVEL     '0'           G08ERR
104200000000     C                   CALL      'XSRDA8'
104300000000     C                   PARM                    WLBDA8
104400000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
104500000000     C*
104600000000     C* IMPOSTA IL PRIMO NUMERO TRAINO SU CUI COPIARE
104700000000IF  1C     PARMAN        IFEQ      'N'                                          *NUOVA NUMERAZIONE
104800000000     C     PARNUM        SUB       1             NEWTRN
104900000000X   1C                   ELSE                                                   *MANTENIMENTO VECCHI
105000000000     C                   Z-ADD     TLTTRN        NEWTRN
105100000000E   1C                   ENDIF
105200000000     C*
105300000000     C                   ENDSR
105400000000     O*------------------------------------------------------------------------*
105500000000** MSG - MESSAGGI DI ERRORE
105600000000TRAINO NON DUPLICATO: ESISTENTE MA NON CHIESTA LA SOVRAPPOSIZIONE 1
105700000000TRAINO NON DUPLICATO: IN SOVRAPPOSIZIONE CON ALTRI                2
105800000000TRAINO NON DUPLICATO: UGUALE A QUELLO DA CUI COPIARE              3
105900000000TRAINO NON DUPLICATO: NUMERO SU CUI COPIARE MAGGIORE 9.000.000    4
1060000000005
1061000000006
1062000000007
1063000000008
106400000000
106500000000***  F I N E   P R O S P E T T O  ***                             10
